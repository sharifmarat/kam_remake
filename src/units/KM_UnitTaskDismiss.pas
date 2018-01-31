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
  public
    constructor Create(aUnit: TKMUnit);
    constructor Load(LoadStream: TKMemoryStream); override;
    destructor Destroy; override;
    procedure SyncLoad; override;
    procedure Save(SaveStream: TKMemoryStream); override;

    property School: TKMHouse read fSchool;

    function Execute: TTaskResult; override;
  end;


implementation
uses
  KM_ResHouses, KM_HandsCollection;


{ TTaskDismiss }
constructor TTaskDismiss.Create(aUnit: TKMUnit);
begin
  inherited;

  fTaskName := utn_Dismiss;
  fSchool := gHands[aUnit.Owner].FindHouse(ht_School, aUnit.GetPosition).GetHousePointer;
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
      0:  begin
            Thought := th_Death;
            SetActionWalkToSpot(fSchool.PointBelowEntrance);
          end;
      1:  SetActionGoIn(ua_Walk, gd_GoInside, fSchool);
      2:  begin
            fUnit.KillUnit(PLAYER_NONE, False, False);
          end;
      else Result := tr_TaskDone;
    end;

  Inc(fPhase);
end;


end.
