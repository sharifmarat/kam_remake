unit KM_UnitTaskDismiss;
{$I KaM_Remake.inc}
interface
uses
  Classes, SysUtils,
  KM_Defaults, KM_Units, KM_Houses, KM_Points, KM_CommonClasses;


type
  TKMTaskDismiss = class(TKMUnitTask)
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
    function CouldBeCancelled: Boolean; override;

    property School: TKMHouse read fSchool;
    function FindNewSchool: TKMHouse;

    function Execute: TKMTaskResult; override;
  end;


implementation
uses
  KM_ResHouses, KM_HandsCollection, KM_UnitActionAbandonWalk, KM_Hand;


{ TTaskDismiss }
constructor TKMTaskDismiss.Create(aUnit: TKMUnit);
begin
  inherited;

  fType := uttDismiss;
  FindNewSchool;
end;


constructor TKMTaskDismiss.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(fSchool, 4);
end;


destructor TKMTaskDismiss.Destroy;
begin
  gHands.CleanUpHousePointer(fSchool);

  inherited;
end;


function TKMTaskDismiss.ShouldBeCancelled: Boolean;
begin
  Result := (fSchool = nil) or fSchool.IsDestroyed;
end;


function TKMTaskDismiss.CouldBeCancelled: Boolean;
begin
  Result := fPhase <= 1; //Allow cancel dismiss only while walking to the school point below entrance
end;


procedure TKMTaskDismiss.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  if fSchool <> nil then
    SaveStream.Write(fSchool.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
end;


procedure TKMTaskDismiss.SyncLoad;
begin
  inherited;
  fSchool := gHands[fUnit.Owner].Houses.GetHouseByUID(Cardinal(fSchool));
end;


function TKMTaskDismiss.FindNewSchool: TKMHouse;
var
  S: TKMHouse;
begin
  fSchool := nil;

  S := gHands[fUnit.Owner].FindHouse(htSchool, fUnit.CurrPosition);

  if (S <> nil) and fUnit.CanWalkTo(fUnit.CurrPosition, S.PointBelowEntrance, tpWalk, 0) then
    fSchool := S.GetHousePointer;

  Result := fSchool;
end;


procedure TKMTaskDismiss.InitDefaultAction;
begin
  //Do nothing here, as we have to continue old action, until it could be interrupted
end;


function TKMTaskDismiss.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;

  if (fSchool = nil) or fSchool.IsDestroyed then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
    case fPhase of
      0:  SetActionWalkToSpot(fSchool.PointBelowEntrance);
      1:  SetActionGoIn(uaWalk, gdGoInside, fSchool);
      2:  fUnit.Kill(PLAYER_NONE, False, False); //Silently kill unit
      else Result := trTaskDone;
    end;

  Inc(fPhase);
end;


end.

