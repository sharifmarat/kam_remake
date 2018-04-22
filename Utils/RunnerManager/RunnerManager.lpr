program RunnerManager;

{$mode objfpc}{$H+}
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
  {$ENDIF}{$ENDIF}
  { you can add units here }
  Classes, SysUtils,  Crt,
  ComInterface in '..\Runner\ComInterface.pas',
  GeneticAlgorithm in '..\Runner\GeneticAlgorithm.pas',
  ThreadManagement in 'ThreadManagement.pas';

var
  TM: TThreadManagement;
begin
  // All parameters are stored in constants in TThreadManagement in procedure RunSimulation
  // Edit it carefully
  TM := TThreadManagement.Create();
  try
    TM.RunSimulation();
  finally
    TM.Free;
  end;
end.

