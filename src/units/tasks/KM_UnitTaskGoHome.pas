unit KM_UnitTaskGoHome;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_Defaults, KM_Units, KM_Points;


type
  TKMTaskGoHome = class(TKMUnitTask)
  public
    constructor Create(aUnit: TKMUnit);
    destructor Destroy; override;

    function Execute: TKMTaskResult; override;
    function CouldBeCancelled: Boolean; override;
  end;


implementation


{ TTaskGoHome }
constructor TKMTaskGoHome.Create(aUnit: TKMUnit);
begin
  inherited;

  fTaskName := utn_GoHome;
end;


destructor TKMTaskGoHome.Destroy;
begin
  inherited;
end;


function TKMTaskGoHome.CouldBeCancelled: Boolean;
begin
  Result := (fPhase - 1) //phase was increased at the end of execution
                   <= 0; //Allow cancel task only at walking phases
end;


function TKMTaskGoHome.Execute: TKMTaskResult;
begin
  Result := tr_TaskContinues;

  if fUnit.GetHome.IsDestroyed then
  begin
    Result := tr_TaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0:  begin
          Thought := th_Home;
          SetActionWalkToSpot(GetHome.PointBelowEntrance);
        end;
    1:  SetActionGoIn(ua_Walk, gd_GoInside, GetHome);
    2:  begin
          Thought := th_None; //Only stop thinking once we are right inside
          GetHome.SetState(hst_Idle);
          SetActionStay(5, ua_Walk);
        end;
    else Result := tr_TaskDone;
  end;

  Inc(fPhase);
end;


end.
