unit ThreadManagement;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Thread, ComInterface, GeneticAlgorithm, Dos;

type
  TThreadManagement = class
  private
    fUSE_DEBUG_FILE: Boolean;
    fDebugFile: TextFile;
    fResultFile: TextFile;
    fParametersFile: TextFile;
    procedure ShowStatus(Status: string);
    procedure InitPopulation(var aPop: TGAPopulation);
    procedure RunThreads(const THREADS: Byte; var aMSetup: TManagerSetup; var aGASetup: TGASetup);
    procedure LogResults(aSimNumber: Integer; var aPop: TGAPopulation);
  public
    constructor Create();
    destructor Destroy(); override;

    procedure RunSimulation();
  end;

implementation


constructor TThreadManagement.Create();
const
  DEBUG_FILE_NAME = 'DEBUG.txt';
  RESULT_FILE_NAME = 'RESUTLS.txt';
  PARAMETERS_FILE_NAME = 'PARAMETERS.txt';
begin
  fUSE_DEBUG_FILE := True;
  AssignFile(fDebugFile, DEBUG_FILE_NAME);
  AssignFile(fResultFile, RESULT_FILE_NAME);
  AssignFile(fParametersFile, PARAMETERS_FILE_NAME);
  try
    rewrite(fDebugFile);
    rewrite(fResultFile);
    rewrite(fParametersFile);
  except
    on E: EInOutError do
      writeln('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
  end;
  inherited;
end;


destructor TThreadManagement.Destroy();
begin
  try
    CloseFile(fDebugFile);
    CloseFile(fResultFile);
    CloseFile(fParametersFile);
  except
    on E: EInOutError do
      writeln('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
  end;
  inherited;
end;


procedure TThreadManagement.ShowStatus(Status: string);
begin
  writeln(Status);
end;


procedure TThreadManagement.LogResults(aSimNumber: Integer; var aPop: TGAPopulation);
var
  I: Integer;
  Idv: TGAIndividual;
begin
  I := 0;
  try
    if fUSE_DEBUG_FILE then
    begin
      writeln(fDebugFile, IntToStr(aSimNumber) + '. run:');
      for I := 0 to aPop.Count - 1 do
        writeln(fDebugFile, '  ' + FloatToStr(aPop[I].Fitness));
    end;
    Idv := aPop.GetFittest(I);
    writeln(fResultFile, 'Simulation: ' + IntToStr(aSimNumber) + '. Best individual index: ' + IntToStr(I) + ', fitness: ' + FloatToStr(Idv.Fitness));
    writeln(fParametersFile, IntToStr(aSimNumber) + ':');
    for I := 0 to Idv.Count - 1 do
      writeln(fParametersFile, FloatToStr(Idv.Gene[I]));
  except
    on E: EInOutError do
      writeln('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
  end;
end;


procedure TThreadManagement.InitPopulation(var aPop: TGAPopulation);
var
  I,K: Integer;
begin
  for I := 0 to aPop.Count - 1 do
  begin
    aPop.Individual[I].Fitness := 0;
    for K := 0 to aPop.Individual[I].Count - 1 do
      aPop.Individual[I].Gene[K] := Random();
  end;
end;


procedure TThreadManagement.RunThreads(const THREADS: Byte; var aMSetup: TManagerSetup; var aGASetup: TGASetup);
  function SplitPopulation(aStartIdx, aCnt: Integer): TGASetup;
  var
    I,K: Integer;
    DefPop: TGAPopulation;
  begin
    DefPop := aGASetup.Population;
    with Result do
    begin
      MapCnt := aGASetup.MapCnt;
      Population := TGAPopulation.Create(aCnt, DefPop.Individual[0].Count, true);
      for I := 0 to Population.Count - 1 do
      begin
        Population.Individual[I].Fitness := 0;
        for K := 0 to Population.Individual[I].Count - 1 do
          Population.Individual[I].Gene[K] := DefPop.Individual[aStartIdx].Gene[K];
        aStartIdx := aStartIdx + 1;
      end;
    end;
  end;
  procedure MergePopulation(aStartIdx, aCnt: Integer; var aThreadGAS: TGASetup);
  var
    I: Integer;
    DefPop: TGAPopulation;
  begin
    DefPop := aGASetup.Population;
    with aThreadGAS do
    begin
      for I := 0 to aCnt - 1 do
      begin
        DefPop.Individual[aStartIdx].Fitness := Population.Individual[I].Fitness;
        aStartIdx := aStartIdx + 1;
      end;
    end;
  end;
var
  I, CntInThread, ActualIdx: Integer;
  ThreadArr: array of TMyThread;
begin
  if (aGASetup.Population = nil) OR (aGASetup.Population.Individual[0].Count = 0) then
    Exit;

  write('  Init Threads: ');
  SetLength(ThreadArr, THREADS);
  ActualIdx := 0;
  CntInThread := Round(aGASetup.Population.Count / (THREADS * 1.0));
  for I := 0 to THREADS - 1 do
  begin
    // Create thread
    ThreadArr[I] := TMyThread.Create(I,True);
    ThreadArr[I].OnShowStatus := @ShowStatus;
    // Init data
    ThreadArr[I].fMSetup := aMSetup;
    ThreadArr[I].fGASetup := SplitPopulation(ActualIdx, CntInThread);
    ActualIdx := ActualIdx + CntInThread;
    if (I = THREADS - 2) then // Next cycle will be the last -> secure that all individual will be part of some thread (round problems)
      CntInThread := aGASetup.Population.Count - ActualIdx;
    write(IntToStr(I)+ '; ');
  end;
  writeln('... done!');

  writeln('  Run Threads:');
  // Start thread
  for I := 0 to THREADS - 1 do
    ThreadArr[I].Start;
  // Wait till is every thread finished
  for I := 0 to THREADS - 1 do
    ThreadArr[I].WaitFor;

  write('  Collecting data: ');
  // Collect data
  ActualIdx := 0;
  CntInThread := Round(aGASetup.Population.Count / (THREADS * 1.0));
  for I := 0 to THREADS - 1 do
  begin
    MergePopulation(ActualIdx, CntInThread, ThreadArr[I].fGASetup);
    ActualIdx := ActualIdx + CntInThread;
    if (I = THREADS - 2) then // Next cycle will be the last -> secure that all individual will be part of some thread (round problems)
      CntInThread := aGASetup.Population.Count - ActualIdx;
    write(IntToStr(I)+ '; ');
  end;
  writeln('... done!');

  write('  Close threads: ');
  // Clear threads
  for I := 0 to THREADS - 1 do
  begin
    ThreadArr[I].Free;
    write(IntToStr(I)+ '; ');
  end;
  writeln('... done!');
end;


procedure TThreadManagement.RunSimulation();
const
  // Command line cannot transfer infinite count of parameters
  // Maximal count per a thread is ~20 individual with 25 genes = 500 numbers + coding
  // In case that your GA have more parameters or idividuals you have to indrease count of threads
  M_FILE = 'Runner.exe';
  M_DIRECTORY = 'G:\KaM_AInew\Utils\Runner';
  M_SIM_CLASS = 'TKMRunnerGA_CityBuilder';//'TKMRunnerGA_CityPredictor';//'TKMRunnerGA_CityPlanner'; TKMRunnerGA_TestManager
  M_SIM_TIME_MIN = 1;
  GA_THREADS_CNT = 3; // Count of threads
  GA_Map_CNT = 20;
  GA_Generations = 40;
  GA_Individuals = 39;
  GA_Genes = 13;
  GA_Start_Mutation = 0.2;
  GA_Final_Mutation = 0.1;
  GA_Crossover = 1;
  GA_IndividualsInTournament = 3;
var
  hours, minutes, seconds, milliseconds: word;
  I: Integer;
  SecCnt: Cardinal;
  GAMut: Single;
  MSetup: TManagerSetup;
  GASetup: TGASetup;
  NewPopulation: TGAPopulation;
  fAlgorithm: TGAAlgorithm;
begin
  writeln('Starting simulation');
  GetTime(hours, minutes, seconds, milliseconds);
  SecCnt := seconds + minutes * 60 + hours * 60 * 60;
  with MSetup do
  begin
    SimFile := M_FILE;
    WorkDir := M_DIRECTORY;
    RunningClass := M_SIM_CLASS;
    SimTimeInMin := M_SIM_TIME_MIN;
  end;

  with GASetup do
  begin
    MapCnt := GA_Map_CNT;
    Population := TGAPopulation.Create(GA_Individuals, GA_Genes, True);
    InitPopulation(Population);
  end;

  fAlgorithm := TGAAlgorithm.Create;
  NewPopulation := nil;
  try
    for I := 0 to GA_Generations - 1 do
    begin
      writeln(IntToStr(I+1) + '. run');
      MSetup.SimNumber := I + 1;
      RunThreads(GA_THREADS_CNT, MSetup, GASetup);
      LogResults(I+1, GASetup.Population);

      GAMut := Abs(GA_Final_Mutation + (GA_Start_Mutation - GA_Final_Mutation) * (1 - (I / (GA_Generations * 1.0))));
      fAlgorithm.Mutation := GAMut;
      fAlgorithm.CrossoverCoef := GA_Crossover;
      fAlgorithm.IndividualsInTournament := GA_IndividualsInTournament; // This may be also changed during simulation

      NewPopulation := TGAPopulation.Create( GASetup.Population.Count, GASetup.Population.Individual[0].Count, False);
      fAlgorithm.EvolvePopulation(GASetup.Population, NewPopulation);
      GASetup.Population.Free;
      GASetup.Population := NewPopulation;
    end;
  finally
    fAlgorithm.Free;
    GASetup.Population.Free;
  end;
  writeln('Simulation is done!');

  GetTime(hours, minutes, seconds, milliseconds);
  SecCnt := seconds + minutes * 60 + hours * 60 * 60 - SecCnt;
  writeln('Time: ' + IntToStr(SecCnt));

  readln();
end;


end.
