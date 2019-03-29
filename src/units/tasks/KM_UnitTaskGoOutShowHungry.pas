unit KM_UnitTaskGoOutShowHungry;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_Defaults, KM_Units, SysUtils, KM_Points;

type
  TKMTaskGoOutShowHungry = class(TKMUnitTask)
  public
    constructor Create(aUnit:TKMUnit);
    function Execute:TKMTaskResult; override;
  end;


implementation
uses
  KM_CommonUtils;


{ TTaskGoOutShowHungry }
constructor TKMTaskGoOutShowHungry.Create(aUnit:TKMUnit);
begin
  inherited Create(aUnit);
  fType := uttGoOutShowHungry;
end;


function TKMTaskGoOutShowHungry.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;
  if fUnit.Home.IsDestroyed then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
    0: begin
         Thought := th_Eat;
         SetActionStay(20,uaWalk);
       end;
    1: begin
         SetActionGoIn(uaWalk,gd_GoOutside,fUnit.Home);
         Home.SetState(hst_Empty);
       end;
    2: SetActionLockedStay(4,uaWalk);
    3: SetActionWalkToSpot(fUnit.Home.PointBelowEntrance);
    4: SetActionGoIn(uaWalk,gd_GoInside,fUnit.Home);
    5: begin
         SetActionStay(20+KaMRandom(10, 'TKMTaskGoOutShowHungry.Execute'), uaWalk);
         Home.SetState(hst_Idle);
       end;
    else begin
         Thought := th_None;
         Result := trTaskDone;
       end;
  end;
  inc(fPhase);
end;


end.
