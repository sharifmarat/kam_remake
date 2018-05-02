unit ManagerInterface;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface
uses
  Classes, SysUtils, Unit_Runner, Runner_Game, ComInterface, GeneticAlgorithm, Unit1;


type
  TKMParallelRun = class
  private
    fDebugForm: TForm2;

    fSetup: TRunnerSetup;
    fGASetup: TGASetup;
    fCommunication: TKMComInterface;
  public
    constructor Create(aDebugForm: TForm2);
    destructor Destroy();

    procedure InitSimulation();
    procedure RunSimulation();
    procedure LogResults();
  end;

implementation


constructor TKMParallelRun.Create(aDebugForm: TForm2);
begin
  fDebugForm := aDebugForm;
  fGASetup.Population := nil;
  fSetup.RunningClass := '';
  fSetup.SimNumber := 0;
  fSetup.SimTimeInMin := 0;
  fCommunication := TKMComInterface.Create;

  //fDebugForm.Memo3.Lines.Append('TKMParallelRun was created');
end;


destructor TKMParallelRun.Destroy();
begin
  fCommunication.Free;
end;


procedure TKMParallelRun.InitSimulation();
begin
  //fDebugForm.Memo3.Lines.Append('Initialization');
  fCommunication.SetupSimulation(fSetup, fGASetup);
end;


procedure TKMParallelRun.RunSimulation();
var
  Check: Boolean;
  I: Integer;
  RunnerClass: TKMRunnerClass;
  Runner: TKMRunnerCommon;
  GARunner: TKMRunnerGA_Common;
  Output: TKMRunResults;
begin
  Check := False;
  for I := 0 to Length(RunnerList) - 1 do
    if (CompareText(RunnerList[I].ClassName, fSetup.RunningClass) = 0) then
    begin
      Check := True;
      //fDebugForm.Memo3.Lines.Append('Name was found');
      break;
    end;
  if not Check then
  begin
    //fDebugForm.Memo3.Lines.Append('Name was NOT found : ' + RunnerList[3].ClassName + ' vs ' + fSetup.RunningClass);
    Exit;
  end;


  RunnerClass := RunnerList[I]; // ID of running test - planner / builder / predictor etc.
  Runner := RunnerClass.Create(nil); // No render in parallel run
  try
    //fDebugForm.Memo3.Lines.Append('Start simulation');
    Runner.Duration := fSetup.SimTimeInMin; // Minutes of simulation
    if (Runner.ClassType.InheritsFrom(TKMRunnerGA_Common)) then
    begin
      GARunner := TKMRunnerGA_Common(Runner);
      GARunner.IOData := fGASetup;
      GARunner.SimSetup := fSetup;
      Output := Runner.Run(1); // Only 1 run
      fGASetup := GARunner.IOData;
    end
    else
      Output := Runner.Run(1); // Only 1 run

  finally
    Runner.Free;
  end;

  //fDebugForm.Memo3.Lines.Append('End simulation');
end;


procedure TKMParallelRun.LogResults();
begin
  //fDebugForm.Memo3.Lines.Append('Log Results');
  fCommunication.LogSimulationResults(fSetup, fGASetup);
end;

end.

