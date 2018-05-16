program Runner;
{$I KaM_Remake.inc}
uses
  Forms,
  {$IFDEF FPC} Interfaces, {$ENDIF}
  Unit1 in 'Unit1.pas' {Form2},
  Unit_Runner in 'Unit_Runner.pas',
  Runner_Game in 'Runner_Game.pas',
  KM_Defaults in '..\..\src\common\KM_Defaults.pas', 
  ComInterface in 'ComInterface.pas',
  ManagerInterface in 'ManagerInterface.pas';

{$R *.res}

var
  ParRun: TKMParallelRun;
begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  
  if (ParamCount > 0) then
  begin
    ParRun := TKMParallelRun.Create(Form2);
    try
      PARALLEL_RUN := True;
      ParRun.InitSimulation();
      ParRun.RunSimulation();
      ParRun.LogResults();
    finally
      ParRun.Free();
    end;
    Application.Terminate;
  end;
  
  Application.Run;
end.
