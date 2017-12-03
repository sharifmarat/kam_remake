unit KM_ArmyManagement;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Units, KM_UnitGroups, KM_AISetup,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_ArmyAttack;

type
  TKMArmyManagement = class
  private
    fOwner: TKMHandIndex;
    fLastEquippedTimeIron, fLastEquippedTimeLeather: Cardinal;

    fSetup: TKMHandAISetup;

    fAttack: TKMArmyAttack;

    //fDefencePositions: TAIDefencePositions;

    fBalanceText: UnicodeString;
    {
    procedure CheckArmy;
    procedure CheckArmyCount;
    procedure CheckAttacks;
    procedure CheckAutoAttack;
    procedure CheckAutoDefend;
    }
    //procedure OrderAttack(aGroup: TKMUnitGroup; aTarget: TAIAttackTarget; aCustomPos: TKMPoint);
  public
    constructor Create(aPlayer: TKMHandIndex; aSetup: TKMHandAISetup);
    destructor Destroy; override;

    procedure AfterMissionInit();
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    property Attack: TKMArmyAttack read fAttack write fAttack;
    //property DefencePositions: TAIDefencePositions read fDefencePositions;
    //procedure RetaliateAgainstThreat(aAttacker: TKMUnit);
    //procedure WarriorEquipped(aGroup: TKMUnitGroup);

    procedure UpdateState(aTick: Cardinal);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad();

    procedure LogStatus(var aBalanceText: UnicodeString);
    procedure Paint();
  end;


implementation
uses
  KM_Game, KM_Hand, KM_HandsCollection, KM_Terrain, KM_AIFields,
  KM_Houses, KM_HouseBarracks,
  KM_ResHouses, KM_NavMesh, KM_CommonUtils, KM_RenderAux;


{ TKMArmyManagement }
constructor TKMArmyManagement.Create(aPlayer: TKMHandIndex; aSetup: TKMHandAISetup);
begin
  inherited Create;

  fOwner := aPlayer;
  fSetup := aSetup;

  fAttack := TKMArmyAttack.Create(aPlayer);
  //fAttacks := TAIAttacks.Create;
  //fDefencePositions := TAIDefencePositions.Create;
end;


destructor TKMArmyManagement.Destroy;
begin
  //fDefencePositions.Free;
  //fAttacks.Free;

  fAttack.Free;
  inherited;
end;


procedure TKMArmyManagement.AfterMissionInit();
begin
  fAttack.AfterMissionInit();
  if (gGame.MissionMode = mm_Tactic) then  // DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG
    fAttack.OrderToAttack();
end;


procedure TKMArmyManagement.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
end;


procedure TKMArmyManagement.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);
  SaveStream.Write(fLastEquippedTimeIron);
  SaveStream.Write(fLastEquippedTimeLeather);
  //fAttacks.Save(SaveStream);
  //fDefencePositions.Save(SaveStream);
end;


procedure TKMArmyManagement.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);
  LoadStream.Read(fLastEquippedTimeIron);
  LoadStream.Read(fLastEquippedTimeLeather);
  //fAttacks.Load(LoadStream);
  //fDefencePositions.Load(LoadStream);
end;


procedure TKMArmyManagement.SyncLoad;
begin
  //fDefencePositions.SyncLoad;
end;






procedure TKMArmyManagement.UpdateState(aTick: Cardinal);
begin


  if (aTick + Byte(fOwner)) mod MAX_HANDS = 0 then
  begin
    if (gGame.MissionMode = mm_Tactic) then  // DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG
      fAttack.UpdateState(aTick);

    //CheckEnemyPresence; //Check enemy threat in close range and issue defensive attacks (or flee?)
    //CheckAndIssueAttack; //Attack enemy
    //Anything Else?
  end;
end;


procedure TKMArmyManagement.LogStatus(var aBalanceText: UnicodeString);
begin
  aBalanceText := '';
end;


procedure TKMArmyManagement.Paint();
const
  COLOR_WHITE = $80FFFFFF;
  COLOR_BLACK = $80000000;
  COLOR_GREEN = $6000FF00;
  COLOR_RED = $800000FF;
  COLOR_YELLOW = $8000FFFF;
  COLOR_GREEN_Field = $4400FF00;
  COLOR_GREEN_Wine = $3355FFFF;
  COLOR_BLUE = $60FF0000;
begin
  gRenderAux.LineOnTerrain(KMPoint(2,2), KMPoint(4,4), $CCFF2222);
  //gRenderAux.CircleOnTerrain(CenterPlatoon.X, CenterPlatoon.Y, 5, $09FFFFFF, $99FFFFFF);
  //gRenderAux.Quad(Loc.X, Loc.Y, Color);
end;


end.
