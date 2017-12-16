unit KM_ArmyAttack;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Units, KM_UnitGroups,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  KM_Houses, KM_ResHouses, KM_NavMeshPathFinding;

type
  TAISquad = class // Squad management (one group) -> move group / attack something with group
  private
    fGroup: TKMUnitGroup; //Commander of group currently occupying position
    fFinalPosition: TKMPointDir;
    fTargetUnit: TKMUnit;
    fTargetHouse: TKMHouse;
    fTimeLimit: Cardinal;
    fPolygonPath: TKMWordArray;
    fPointPath: TKMPointArray;

    function GetGroupPosition(): TKMPoint; inline;
  public
    constructor Create(aGroup: TKMUnitGroup);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    //property State: TAISquadState read fSquadState;
    //property Order: TAISquadOrder read fSquadOrder write fSquadOrder;
    property TargetHouse: TKMHouse read fTargetHouse write fTargetHouse;
    property TargetUnit: TKMUnit read fTargetUnit write fTargetUnit;
    property Position: TKMPoint read GetGroupPosition;
    property Group: TKMUnitGroup read fGroup;

    procedure UpdateState(aTick: Cardinal);
  end;

  TAICompany = class
  private
    fOwner: TKMHandIndex;
    fCenterPoint: TKMPoint;
    fSquads: array[TGroupType] of TKMList;
    fPathfinding: TNavMeshPathFinding;

    function GetPosition(): TKMPoint;
  public
    constructor Create(aPlayer: TKMHandIndex);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property Position: TKMPoint read GetPosition;

    procedure UpdateState(aTick: Cardinal);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    procedure AddSquad(aGroup: TKMUnitGroup);
    procedure DeleteSquad(aGT: TGroupType; aIdx: Integer);
    procedure LaunchAttack(aTarget: TKMPoint);
  end;

  TKMArmyAttack = class
  private
    fOwner: TKMHandIndex;
    fCompaniesCnt: Word;
    fCompanies: TKMList;

    function SelectTarget(aCompany: TAICompany; aPoint: TKMPoint): Boolean;
  public
    constructor Create(aPlayer: TKMHandIndex);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure AfterMissionInit();
    procedure UpdateState(aTick: Cardinal);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    procedure OrderToAttack();

    procedure LogStatus(var aBalanceText: UnicodeString);
    procedure Paint();
  end;


implementation
uses
  KM_Game, KM_Hand, KM_HandsCollection, KM_Terrain, KM_AIFields,
  KM_NavMesh, KM_CommonUtils, KM_AISetup, KM_AI;


{ TAISquad }
constructor TAISquad.Create(aGroup: TKMUnitGroup);
begin
  inherited Create;
  fGroup := aGroup.GetGroupPointer();
end;


destructor TAISquad.Destroy();
begin
  fGroup.ReleaseGroupPointer();
  inherited;
end;


procedure TAISquad.Save(SaveStream: TKMemoryStream);
begin
  //SaveStream.Write(fOwner);
end;


procedure TAISquad.Load(LoadStream: TKMemoryStream);
begin
  //LoadStream.Read(fOwner);
end;


function TAISquad.GetGroupPosition(): TKMPoint;
begin
  Result := fGroup.Position;
end;


procedure TAISquad.UpdateState(aTick: Cardinal);
begin

end;





{ TAICompany }
constructor TAICompany.Create(aPlayer: TKMHandIndex);
var
  GT: TGroupType;
begin
  inherited Create;
  fOwner := aPlayer;
  fPathfinding := TNavMeshPathFinding.Create();
  for GT := Low(TGroupType) to High(TGroupType) do
    fSquads[GT] := TKMList.Create();
end;


destructor TAICompany.Destroy;
var
  GT: TGroupType;
begin
  fPathfinding.Free;
  for GT := Low(TGroupType) to High(TGroupType) do
    fSquads[GT].Free;
  inherited;
end;


procedure TAICompany.Save(SaveStream: TKMemoryStream);
begin
  //SaveStream.Write(fOwner);
end;


procedure TAICompany.Load(LoadStream: TKMemoryStream);
begin
  //LoadStream.Read(fOwner);
end;


procedure TAICompany.UpdateState(aTick: Cardinal);
begin

end;


procedure TAICompany.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
end;


procedure TAICompany.AddSquad(aGroup: TKMUnitGroup);
begin
  fSquads[ aGroup.GroupType ].Add( TAISquad.Create(aGroup) );
end;


procedure TAICompany.DeleteSquad(aGT: TGroupType; aIdx: Integer);
var
  Squad: TAISquad;
begin
  Squad := fSquads[aGT].Items[aIdx];
  Squad.Free;
  fSquads[aGT].Delete(aIdx);
end;


procedure TAICompany.LaunchAttack(aTarget: TKMPoint);
begin

  //function Route_Make(aStart, aEnd: Word; out aRouteArray: TKMWordArray; out aRoutePointArray: TKMPointArray): Boolean;
  //StartPolygon := gAIFields.NavMesh.Point2NodeArr[ Platoon.Position[True].Y, Platoon.Position[True].X ];
  //EndPolygon := gAIFields.NavMesh.Point2NodeArr[ Platoon.TargetPoint.Y, Platoon.TargetPoint.X ];

end;


function TAICompany.GetPosition(): TKMPoint;
var
  I, Count: Integer;
  Output: TKMPoint;
  G: TGroupType;
  Squad: TAISquad;
begin
  Output := KMPOINT_ZERO;
  Count := 0;
  for G := Low(TGroupType) to High(TGroupType) do
    for I := 0 to fSquads[G].Count-1 do
    begin
      Squad := fSquads[G][I];
      Output.X := Output.X + Squad.Group.Position.X;
      Output.Y := Output.Y + Squad.Group.Position.Y;
      Count := Count + 1;
    end;

  if Count > 0 then
  begin
    Output.X := Round( Output.X / Count );
    Output.Y := Round( Output.Y / Count );
  end;

  // If we cannot walk there choose first group instead
  if not gTerrain.CheckPassability(Output, tpWalk) then
    for G := Low(TGroupType) to High(TGroupType) do
      if fSquads[G].Count > 0 then
      begin
        Squad := fSquads[G][0];
        Output := Squad.Group.Position;
        break;
      end;
  Result := Output;
end;





{ TKMArmyAttack }
constructor TKMArmyAttack.Create(aPlayer: TKMHandIndex);
begin
  inherited Create;
  fCompanies := TKMList.Create();
  fOwner := aPlayer;
  fCompaniesCnt := 0;
end;


destructor TKMArmyAttack.Destroy;
var
  I: Integer;
  Company: TAICompany;
begin
  for I := fCompanies.Count - 1 downto 0 do
  begin
    Company := fCompanies.Items[I];
    Company.Free;
  end;
  fCompanies.Free;
  inherited;
end;


procedure TKMArmyAttack.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);
  SaveStream.Write(fCompaniesCnt);
end;


procedure TKMArmyAttack.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);
  LoadStream.Read(fCompaniesCnt);
end;


procedure TKMArmyAttack.AfterMissionInit();
begin

end;


procedure TKMArmyAttack.UpdateState(aTick: Cardinal);
begin
  //Update defence positions locations


  if (aTick + Byte(fOwner)) mod MAX_HANDS = 0 then
  begin


    //CheckEnemyPresence; //Check enemy threat in close range and issue defensive attacks (or flee?)
    //CheckAndIssueAttack; //Attack enemy
    //Anything Else?
  end;
end;


procedure TKMArmyAttack.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
end;


function TKMArmyAttack.SelectTarget(aCompany: TAICompany; aPoint: TKMPoint): Boolean;
var
  I: Integer;
  TargetHouse: TKMHouse;
  TargetUnit: TKMUnit;
begin
  Result := False;
  // Compute common stuf first
  TargetHouse := gHands.GetClosestHouse(aCompany.Position, fOwner, at_Enemy, false);
  if (TargetHouse = nil) then
  begin
    TargetUnit := gHands.GetClosestUnit(aCompany.Position, fOwner, at_Enemy);
    if (TargetUnit = nil) then
      Exit;
  end;

  Result := True;
end;


procedure TKMArmyAttack.OrderToAttack();
var
  I, idx: Integer;
  Target: TKMPoint;
  Company: TAICompany;
begin
  Company := nil;
  fCompaniesCnt := fCompaniesCnt + 1;
  idx := fCompaniesCnt - 1;
  fCompanies.Add( TKMArmyAttack.Create(fOwner) );
  for I := 0 to gHands[fOwner].UnitGroups.Count - 1 do   // Edit (not all groups into 1 company)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  begin
    Company := fCompanies.Items[idx];
    Company.AddSquad( gHands[fOwner].UnitGroups[I] );
  end;

  if SelectTarget(Company,Target) AND (Company <> nil) then
    Company.LaunchAttack(Target);
end;



procedure TKMArmyAttack.LogStatus(var aBalanceText: UnicodeString);
begin
  aBalanceText := '';
end;


procedure TKMArmyAttack.Paint();
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
  //gRenderAux.LineOnTerrain(P1, P2, $CCFF2222);
  //gRenderAux.CircleOnTerrain(CenterPlatoon.X, CenterPlatoon.Y, 5, $09FFFFFF, $99FFFFFF);
  //gRenderAux.Quad(Loc.X, Loc.Y, Color);
end;

end.
