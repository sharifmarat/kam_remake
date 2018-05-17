unit Thread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, ComInterface;

Type
  TShowStatusEvent = procedure(Status: String) of Object;

  TMyThread = class(TThread)
  private
    fThreadNumber: Integer;
    fStatusText : string;
    fOnShowStatus: TShowStatusEvent;
    procedure UpdateStatus(aString: String);
    procedure ShowStatus();
    function SimulateGeneration(): Boolean;
  protected
    procedure Execute; override;
  public
    fMSetup: TManagerSetup;
    fGASetup: TGASetup;

    constructor Create(aNumber: Integer; aCreateSuspended: boolean); reintroduce;
    property OnShowStatus: TShowStatusEvent read fOnShowStatus write fOnShowStatus;
  end;


implementation

constructor TMyThread.Create(aNumber: Integer; aCreateSuspended: boolean);
begin
  inherited Create(aCreateSuspended);
  fThreadNumber := aNumber;
  FreeOnTerminate := False;
end;


procedure TMyThread.UpdateStatus(aString: String);
begin
  fStatusText := '    ' + IntToStr(fThreadNumber) +  '. thread: ' + aString;
end;


procedure TMyThread.ShowStatus();
begin
  if Assigned(fOnShowStatus) then
    fOnShowStatus(fStatusText);
end;


function TMyThread.SimulateGeneration(): Boolean;
var
  CI: TKMComInterface;
begin
  Result := False;
  UpdateStatus('Start simulation');
  Synchronize(@Showstatus);
  CI := TKMComInterface.Create();
  try
    CI.CreateNewSimulation(fMSetup, fGASetup);
  finally
    CI.Free;
  end;
  UpdateStatus('Finish simulation');
  Synchronize(@Showstatus);
  Result := True;
end;


procedure TMyThread.Execute();
var
  SimulationIsFinished: Boolean;
begin
  SimulationIsFinished := False;
  while (not Terminated) AND not SimulationIsFinished do
  begin
    SimulationIsFinished := SimulateGeneration();
  end;
end;



end.

