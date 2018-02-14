unit KM_UnitTaskDismiss;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_Defaults, KM_Units, KM_Houses, KM_Points, KM_CommonClasses;


type
  TTaskDismiss = class(TUnitTask)
  private
    fSchool: TKMHouse;
  protected
    procedure InitDefaultAction; override;
  public
    constructor Create(aUnit: TKMUnit);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    procedure SyncLoad; override;
    procedure Save(SaveStream: TKMemoryStream); override;
    function ShouldBeCancelled: Boolean;
    function CouldBeCancelled: BOolean;

    property School: TKMHouse read fSchool;
    function FindNewSchool: TKMHouse;

    function Execute: TTaskResult; override;
  end;


implementation
uses
  KM_ResHouses, KM_HandsCollection, KM_UnitActionAbandonWalk, KM_Hand;


{ TTaskDismiss }
constructor TTaskDismiss.Create(aUnit: TKMUnit);
begin
  inherited;

  fTaskName := utn_Dismiss;
  FindNewSchool;
end;


constructor TTaskDismiss.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fSchool, 4);
end;


destructor TTaskDismiss.Destroy;
begin
  gHands.CleanUpHousePointer(fSchool);

  inherited;
end;


function TTaskDismiss.ShouldBeCancelled: Boolean;
begin
  Result := (fSchool = nil) or fSchool.IsDestroyed;
end;


function TTaskDismiss.CouldBeCancelled: Boolean;
begin
  Result := fPhase <= 1;
end;


procedure TTaskDismiss.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  if fSchool <> nil then
    SaveStream.Write(fSchool.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
end;


procedure TTaskDismiss.SyncLoad;
begin
  inherited;
  fSchool := gHands[fUnit.Owner].Houses.GetHouseByUID(Cardinal(fSchool));
end;


function TTaskDismiss.FindNewSchool: TKMHouse;
var
  S: TKMHouse;
begin
  fSchool := nil;

  S := gHands[fUnit.Owner].FindHouse(ht_School, fUnit.GetPosition);

  if (S <> nil) and fUnit.CanWalkTo(fUnit.GetPosition, S.PointBelowEntrance, tpWalk, 0) then
    fSchool := S.GetHousePointer;

  Result := fSchool;
end;


procedure TTaskDismiss.InitDefaultAction;
begin
  //Do nothing here, as we have to continue old action, until it could be interrupted
end;


function TTaskDismiss.Execute: TTaskResult;
begin
  Result := tr_TaskContinues;

  if (fSchool = nil) or fSchool.IsDestroyed then
  begin
    Result := tr_TaskDone;
    Exit;
  end;

  with fUnit do
    case fPhase of
      0:  SetActionWalkToSpot(fSchool.PointBelowEntrance);
      1:  SetActionGoIn(ua_Walk, gd_GoInside, fSchool);
      2:  fUnit.Kill(PLAYER_NONE, False, False); //Silently kill unit
      else Result := tr_TaskDone;
    end;

  Inc(fPhase);
end;


end.

