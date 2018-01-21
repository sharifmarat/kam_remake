unit KM_ArmyAttack;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Units, KM_UnitGroups,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  KM_Houses, KM_ResHouses, KM_NavMeshPathFinding,
  KM_Eye, KM_NavMeshInfluences;

type
  TKMSquadArray = array[TGroupType] of TKMList;

  TAISquad = class // Squad management (one group)
  private
    fOwner: TKMHandIndex;
    fGroup: TKMUnitGroup;
    fIsReady: Boolean;
    fFinalPosition: TKMPointDir;
    fTargetHouse: TKMHouse;
    fTargetUnit: TKMUnit;
    fTimeLimit: Cardinal;

    fDEBUGPointPath: TKMPointArray;

    function GetGroupPosition(): TKMPoint; inline;
    function PlanPath(var aActualPosition, aTargetPosition: TKMPoint): Boolean;

    procedure SetTargetHouse(aHouse: TKMHouse);
    procedure SetTargetUnit(aUnit: TKMUnit);
  public
    constructor Create(aGroup: TKMUnitGroup; aPlayer: TKMHandIndex);
    constructor Load(LoadStream: TKMemoryStream);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure SyncLoad();

    property Group: TKMUnitGroup read fGroup;
    property IsReady: Boolean read fIsReady;
    property FinalPosition: TKMPointDir read fFinalPosition write fFinalPosition;
    property Position: TKMPoint read GetGroupPosition;
    property TimeLimit: Cardinal read fTimeLimit write fTimeLimit;
    property TargetHouse: TKMHouse read fTargetHouse write SetTargetHouse;
    property TargetUnit: TKMUnit read fTargetUnit write SetTargetUnit;

    property PointPath: TKMPointArray read fDEBUGPointPath write fDEBUGPointPath;

    procedure UpdateState(aTick: Cardinal);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
  end;

  TAICompany = class
  private
    fOwner: TKMHandIndex;
    fPathPosition: TKMPoint;
    fScanPosition: TKMPoint;
    fTargetHouse: TKMHouse;
    fTargetUnit: TKMUnit;
    fSquads: TKMSquadArray;

    fDEBUGPointPath: TKMPointArray;

    function GetPosition(): TKMPoint;
    function GetTargetPosition(): TKMPoint;
    function GetPointerPosition(aHouse: TKMHouse; aUnit: TKMUnit): TKMPoint;
    procedure OrderToAttack(aActualPosition: TKMPoint; UGA: TKMUnitGroupArray; HA: TKMHouseArray; aTargetHouse: Boolean);
    procedure OrderMove(aActualPosition: TKMPoint);
  public
    constructor Create(aPlayer: TKMHandIndex);
    constructor Load(LoadStream: TKMemoryStream);
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure SyncLoad();

    property PathPosition: TKMPoint read fPathPosition;
    property ScanPosition: TKMPoint read fScanPosition;
    property TargetPosition: TKMPoint read GetTargetPosition;
    property Squads: TKMSquadArray read fSquads;

    property PointPath: TKMPointArray read fDEBUGPointPath write fDEBUGPointPath;

    procedure InitCompany();
    procedure UpdateState(aTick: Cardinal);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    procedure AddSquad(aGroup: TKMUnitGroup);
    procedure DeleteSquad(aGT: TGroupType; aIdx: Integer);
    function SquadCnt(aTypes: TGroupTypeSet = [Low(TGroupType)..High(TGroupType)]): Word;
    function IsGroupInCompany(aGroup: TKMUnitGroup): Boolean;
    function SetTarget(aHouse: TKMHouse; aUnit: TKMUnit = nil): Boolean;
    function ActualizeTarget(aInitPosition: TKMPoint; var aTargetHouse: TKMHouse; var aTargetUnit: TKMUnit; aAimCivilians: Boolean = True): Boolean;
  end;

  TKMArmyAttack = class
  private
    fOwner: TKMHandIndex;
    fCompanies: TKMList;

    //function SelectClosestTarget(aInitPoint: TKMPoint; var aTargetHouse: TKMHouse; var aTargetUnit: TKMUnit; var aTargetGroup: TKMUnitGroup): Boolean;
    function GetCount(): Integer;
    function GetCompany(aIdx: Integer): TAICompany;
  public
    constructor Create(aPlayer: TKMHandIndex);
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad();

    property Count: Integer read GetCount;
    property Company[aIdx: Integer]: TAICompany read GetCompany; default;

    procedure AfterMissionInit();
    procedure UpdateState(aTick: Cardinal);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    function FindBestTarget(var TargetOwner: TKMHandIndex; var aTargetPoint: TKMPoint; aForceToAttack: Boolean = False): Boolean;
    procedure OrderToAttack(aTargetPoint: TKMPoint; aGroups: TKMUnitGroupArray);
    function IsGroupInAction(aGroup: TKMUnitGroup): Boolean;

    procedure LogStatus(var aBalanceText: UnicodeString);
    procedure Paint();
  end;

const
  COMPANY_SCAN_RAD = 20;
  SQR_COMPANY_SCAN_RAD = COMPANY_SCAN_RAD * COMPANY_SCAN_RAD;
  COMPANY_ATTACK_RAD = 15;
  SQR_COMPANY_ATTACK_RAD = COMPANY_ATTACK_RAD * COMPANY_ATTACK_RAD;
  TARGET_HOUSES: THouseTypeSet = [ht_WatchTower, ht_Barracks, ht_Store];

implementation
uses
  KM_Game, KM_Hand, KM_HandsCollection, KM_Terrain, KM_AIFields,
  KM_NavMesh, KM_CommonUtils, KM_AISetup, KM_AI, KM_RenderAux;




{ TAISquad }
constructor TAISquad.Create(aGroup: TKMUnitGroup; aPlayer: TKMHandIndex);
begin
  inherited Create;
  fGroup := aGroup.GetGroupPointer();
  fOwner := aPlayer;
  fIsReady := True;
  fTimeLimit := 0;
  fTargetHouse := nil;
  fTargetUnit := nil;
  //fFinalPosition := KMPointDir(KMPOINT_ZERO, dir_NA);
end;


destructor TAISquad.Destroy();
begin
  gHands.CleanUpGroupPointer(fGroup);
  gHands.CleanUpUnitPointer(fTargetUnit);
  gHands.CleanUpHousePointer(fTargetHouse);
  inherited;
end;


constructor TAISquad.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;
  LoadStream.ReadAssert('Squad');
  LoadStream.Read(fOwner);
  LoadStream.Read(fIsReady);
  LoadStream.Read(fFinalPosition);
  LoadStream.Read(fTimeLimit, SizeOf(fTimeLimit));
  //Subst on syncload
  LoadStream.Read(fGroup, 4);
  LoadStream.Read(fTargetUnit, 4);
  LoadStream.Read(fTargetHouse, 4);
end;


procedure TAISquad.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('Squad');
  SaveStream.Write(fOwner);
  SaveStream.Write(fIsReady);
  SaveStream.Write(fFinalPosition);
  SaveStream.Write(fTimeLimit, SizeOf(fTimeLimit));
  if (fGroup <> nil) then
    SaveStream.Write(fGroup.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
  if (fTargetUnit <> nil) then
    SaveStream.Write(fTargetUnit.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
  if (fTargetHouse <> nil) then
    SaveStream.Write(fTargetHouse.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
end;


procedure TAISquad.SyncLoad();
begin
  fGroup := gHands.GetGroupByUID( Cardinal(fGroup) );
  fTargetUnit := gHands.GetUnitByUID( Cardinal(fTargetUnit) );
  fTargetHouse := gHands.GetHouseByUID( Cardinal(fTargetHouse) );
end;


procedure TAISquad.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
end;


procedure TAISquad.SetTargetUnit(aUnit: TKMUnit);
begin
  gHands.CleanUpUnitPointer(fTargetUnit);
  if (aUnit <> nil) then
    fTargetUnit := aUnit.GetUnitPointer;
end;
procedure TAISquad.SetTargetHouse(aHouse: TKMHouse);
begin
  gHands.CleanUpHousePointer(fTargetHouse);
  if (aHouse <> nil) then
    fTargetHouse := aHouse.GetHousePointer;
end;


function TAISquad.GetGroupPosition(): TKMPoint;
begin
  Result := fGroup.Position;
end;


procedure TAISquad.UpdateState(aTick: Cardinal);
var
  ActPos, FinPos: TKMPoint;
begin
  // Check group
  if (fGroup = nil) OR fGroup.IsDead then
    Exit;
  if not fGroup.IsIdleToAI(True) then
    Exit;

  // Check targets
  if (fTargetHouse <> nil) AND (fTargetHouse.IsDestroyed) then
    gHands.CleanUpHousePointer(fTargetHouse);
  if (fTargetUnit <> nil) AND (fTargetUnit.IsDead) then
    gHands.CleanUpUnitPointer(fTargetUnit);

  // Do order
  //fTimeLimit := aTick;
  ActPos := fGroup.Position;
  if (fTargetUnit <> nil) then
  begin
    FinPos := fTargetUnit.GetPosition;
    if PlanPath(ActPos, FinPos) then
      Group.OrderWalk(FinPos, True, FinalPosition.Dir)
    else
      Group.OrderAttackUnit(fTargetUnit, True);
    fIsReady := True;
  end
  else if (fTargetHouse <> nil) then
  begin
    FinPos := fTargetHouse.GetPosition;
    if PlanPath(ActPos, FinPos) then
      Group.OrderWalk(FinPos, True, FinalPosition.Dir)
    else
      Group.OrderAttackHouse(fTargetHouse, True);
    fIsReady := True;
  end
  else
  begin
    FinPos := FinalPosition.Loc;
    if PlanPath(ActPos, FinPos) then
      Group.OrderWalk(FinPos, True, FinalPosition.Dir)
    else
      Group.OrderWalk(FinalPosition.Loc, True, FinalPosition.Dir);
  end;
end;


function TAISquad.PlanPath(var aActualPosition, aTargetPosition: TKMPoint): Boolean;
const
  SQR_TARGET_REACHED_TOLERANCE = 5*5;
var
  InitPolygon, ClosestPolygon, Distance: Word;
  I: Integer;
  PointPath: TKMPointArray;
begin
  Result := False;
  fIsReady := False;
  if (fTimeLimit < gGame.GameTickCount) OR (KMDistanceSqr(aActualPosition, aTargetPosition) < SQR_TARGET_REACHED_TOLERANCE) then
  begin
    fIsReady := True;
    Exit;
  end;
  if gAIFields.NavMesh.Pathfinding.AvoidEnemyRoute(fOwner, Group.GroupType, aActualPosition, aTargetPosition, Distance, PointPath) then
    if (Distance < 6) then
      Exit
    else
    begin
      InitPolygon := gAIFields.NavMesh.Point2Polygon[aActualPosition.Y,aActualPosition.X];
      I := Length(PointPath)-2;
      repeat
        aTargetPosition := PointPath[ Max(0, I) ];
        ClosestPolygon := gAIFields.NavMesh.Point2Polygon[aTargetPosition.Y,aTargetPosition.X];
        I := I - 1;
      until (InitPolygon <> ClosestPolygon) OR (I < 0);

      fDEBUGPointPath := PointPath;//  DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG
    end;
  Result := True;
end;





{ TAICompany }
constructor TAICompany.Create(aPlayer: TKMHandIndex);
var
  GT: TGroupType;
begin
  inherited Create;
  fOwner := aPlayer;
  fTargetHouse := nil;
  fTargetUnit := nil;

  for GT := Low(TGroupType) to High(TGroupType) do
    fSquads[GT] := TKMList.Create();
end;


destructor TAICompany.Destroy();
var
  GT: TGroupType;
begin
  gHands.CleanUpUnitPointer(fTargetUnit);
  gHands.CleanUpHousePointer(fTargetHouse);
  for GT := Low(TGroupType) to High(TGroupType) do
    fSquads[GT].Free;
  inherited;
end;


constructor TAICompany.Load(LoadStream: TKMemoryStream);
var
  I,Cnt: Integer;
  GT: TGroupType;
begin
  inherited Create;
  LoadStream.ReadAssert('Company');
  LoadStream.Read(fOwner);
  LoadStream.Read(fPathPosition);
  LoadStream.Read(fScanPosition);
  LoadStream.Read(fTargetUnit, 4);
  LoadStream.Read(fTargetHouse, 4);

  for GT := Low(TGroupType) to High(TGroupType) do
  begin
    fSquads[GT] := TKMList.Create();
    LoadStream.Read(Cnt);
    for I := 0 to Cnt - 1 do
      fSquads[GT].Add( TAISquad.Load(LoadStream) );
  end;
end;


procedure TAICompany.Save(SaveStream: TKMemoryStream);
var
  I,Cnt: Integer;
  GT: TGroupType;
  Squad: TAISquad;
begin
  SaveStream.WriteA('Company');
  SaveStream.Write(fOwner);
  SaveStream.Write(fPathPosition);
  SaveStream.Write(fScanPosition);
  if (fTargetUnit <> nil) then
    SaveStream.Write(fTargetUnit.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
  if (fTargetHouse <> nil) then
    SaveStream.Write(fTargetHouse.UID) //Store ID
  else
    SaveStream.Write(Integer(0));

  for GT := Low(TGroupType) to High(TGroupType) do
  begin
    Cnt := fSquads[GT].Count;
    SaveStream.Write(Cnt);
    for I := 0 to Cnt - 1 do
    begin
      Squad := fSquads[GT].Items[I];
      Squad.Save(SaveStream);
    end;
  end;
end;


procedure TAICompany.SyncLoad();
var
  I: Integer;
  GT: TGroupType;
  Squad: TAISquad;
begin
  fTargetUnit := gHands.GetUnitByUID( Cardinal(fTargetUnit) );
  fTargetHouse := gHands.GetHouseByUID( Cardinal(fTargetHouse) );
  for GT := Low(TGroupType) to High(TGroupType) do
    for I := 0 to fSquads[GT].Count - 1 do
    begin
      Squad := fSquads[GT].Items[I];
      Squad.SyncLoad();
    end;
end;


function TAICompany.SetTarget(aHouse: TKMHouse; aUnit: TKMUnit = nil): Boolean;
begin
  Result := False;
  if (aHouse = nil) AND (aUnit <> nil) then
  begin
    gHands.CleanUpUnitPointer(fTargetUnit);
    fTargetUnit := aUnit.GetUnitPointer;
    Result := True;
  end
  else if (aHouse <> nil) AND (aUnit = nil) then
  begin
    gHands.CleanUpHousePointer(fTargetHouse);
    fTargetHouse := aHouse.GetHousePointer;
    Result := True;
  end;
end;


function TAICompany.GetPointerPosition(aHouse: TKMHouse; aUnit: TKMUnit): TKMPoint;
begin
  Result := KMPOINT_ZERO;
  if (aHouse <> nil) AND not aHouse.IsDestroyed then
    Result := aHouse.GetPosition
  else if (aUnit <> nil) AND not aUnit.IsDead then
    Result := aUnit.GetPosition;
end;

function TAICompany.GetTargetPosition(): TKMPoint;
begin
  Result := GetPointerPosition(fTargetHouse, fTargetUnit);
end;


procedure TAICompany.UpdateState(aTick: Cardinal);
  function CheckPrimaryTarget(): Boolean;
  begin
    Result := False;
    if (fTargetHouse <> nil) then
      if fTargetHouse.IsDestroyed then
        gHands.CleanUpHousePointer(fTargetHouse)
      else
        Result := True;
    if (fTargetUnit <> nil) then
      if fTargetUnit.IsDead then
        gHands.CleanUpUnitPointer(fTargetUnit)
      else
        Result := True;
  end;
var
  SquadsAreReady: Boolean;
  I: Integer;
  GT: TGroupType;
  HA: TKMHouseArray;
  UGA: TKMUnitGroupArray;
  ClosestHouse: TKMHouse;
  ClosestUnit: TKMUnit;
  Squad: TAISquad;
begin
  // Check Squads
  SquadsAreReady := True;
  for GT := Low(TGroupType) to High(TGroupType) do
    for I := fSquads[GT].Count - 1 downto 0 do
    begin
      Squad := fSquads[GT].Items[I];
      if (Squad = nil) OR (Squad.Group = nil) OR Squad.Group.IsDead then
        fSquads[GT].Delete(I)
      else
        SquadsAreReady := SquadsAreReady AND Squad.IsReady;
    end;

  if (SquadCnt = 0) then
    Exit;

  // Check target
  ClosestHouse := nil;
  ClosestUnit := nil;
  //ClosestHouse := gHands.GetClosestHouse(ScanPosition, fOwner, at_Enemy);
  //ClosestUnit := gHands.GetClosestUnit(ScanPosition, fOwner, at_Enemy);
  HA := gHands.GetHousesInRadius(ScanPosition, SQR_COMPANY_ATTACK_RAD, fOwner, at_Enemy, TARGET_HOUSES, True);
  UGA := gHands.GetGroupsInRadius(ScanPosition, SQR_COMPANY_ATTACK_RAD, fOwner, at_Enemy);
  if not CheckPrimaryTarget() then
  begin
    if (Length(UGA) > 0) then
      ClosestUnit := UGA[0].GetAliveMember;
    if (Length(HA) > 0) then
      ClosestHouse := HA[0];
    if not SetTarget(ClosestHouse, ClosestUnit) then
      Exit;
  end;

  // Give new orders
  if (Length(UGA) > 0) then
    OrderToAttack(ScanPosition, UGA, HA, False)
  else if (Length(HA) > 0) then
    OrderToAttack(ScanPosition, UGA, HA, True)
  else if SquadsAreReady then
    OrderMove(PathPosition);

  // Update state of Squads
  for GT := Low(TGroupType) to High(TGroupType) do
    for I := fSquads[GT].Count - 1 downto 0 do
    begin
      Squad := fSquads[GT].Items[I];
      Squad.UpdateState(aTick);
    end;
end;


procedure TAICompany.OrderToAttack(aActualPosition: TKMPoint; UGA: TKMUnitGroupArray; HA: TKMHouseArray; aTargetHouse: Boolean);
  //function HouseTypeCnt(aHT: THouseType): Word;
  //var
  //  I: Integer;
  //begin
  //  Result := 0;
  //  for I := Low(HA) to High(HA) do
  //    if (HA[I].HouseType = aHT) then
  //      Result := Result + 1;
  //end;
  //function TargetHouses();
  //const
  //  IGNORE_TOWER_CNT = 3;
  //  ANTI_TOWER_TYPE = gt_Ranged;
  //var
  //  I: Integer;
  //  BestDistSqr, DistSqr: Single;
  //  BestSquad,Squad: TAISquad;
  //  GT: TGroupType;
  //  aGroups: set of TGroupType;
  //begin
  //  for I := Low(HA) to High(HA) do
  //  begin
  //    if (HA[I].HouseType = aHT) then
  //      aGroups := [ANTI_TOWER_TYPE];
  //    for GT in aGroups do
  //    begin
  //      BestDistSqr := 1000000;
  //      for I := 0 to fSquads[GT].Count - 1 do
  //      begin
  //        Squad := fSquads[GT].Items[I];
  //        DistSqr := Squad.TargetUnit := ;
  //        Idx := Idx + 1;
  //      end;
  //    end;
  //  end;
  //end;
var
  TowersCnt, BarrackCnt: Word;
  I, Idx: Integer;
  GT: TGroupType;
  Squad: TAISquad;
begin
  // Attack houses in radius
  if aTargetHouse then
  begin
    //if (HouseTypeCnt(ht_Barracks) > 0) then
    //  TargetHouses([gt_Melee, gt_]);
    //
    //TowersCnt := HouseTypeCnt(ht_WatchTower);
    //// Ignore towers in case that
    //if (TowersCnt <= IGNORE_TOWER_CNT) OR (fSquads[ANTI_TOWER_TYPE].Count = 0) then
    //  OrderMove(ActualPosition);
    for GT := Low(TGroupType) to High(TGroupType) do
      for I := 0 to fSquads[GT].Count - 1 do
      begin
        Squad := fSquads[GT].Items[I];
        Squad.TargetHouse := HA[ KaMRandom(Length(HA)) ];
      end;
  end
  else
  begin
    Idx := 0;
    for GT := Low(TGroupType) to High(TGroupType) do
      for I := 0 to fSquads[GT].Count - 1 do
      begin
        Squad := fSquads[GT].Items[I];
        Squad.TargetUnit := UGA[Idx].GetAliveMember;
        Idx := Idx + 1;
        if (Idx = Length(UGA)) then
          Idx := 0;
      end;
  end;
end;


procedure TAICompany.OrderMove(aActualPosition: TKMPoint);

  function GetInitPolygons(aCnt: Integer; var aPointPath: TKMPointArray): TKMWordArray;
  const
    MINIMAL_MOVEMENT = 5;
    INIT_POLYGONS_COEF = 3;
  var
    InitPolygon: Word;
    I, Idx: Integer;
    InitPolygons: TKMWordArray;
  begin
    // Get initial point on the path (it must be in specific distance from actual position to secure smooth moving of the company)
    I := Length(PointPath)-1;
    while (I >= 0) AND (KMDistanceAbs(aActualPosition, PointPath[I]) < MINIMAL_MOVEMENT) do
      I := I - 1;
    // Make sure that platoon will not start in actual polygon but position will be moved forward
    InitPolygon := gAIFields.NavMesh.Point2Polygon[aActualPosition.Y,aActualPosition.X];
    repeat
      fPathPosition := PointPath[ Max(0, I) ];
      I := I - 1;
    until (InitPolygon <> gAIFields.NavMesh.Point2Polygon[fPathPosition.Y,fPathPosition.X]) OR (I < 0);

    I := Max(0,I + 1); // I = 0 we are in polygon of our target
    // Get several init polygons
    SetLength(InitPolygons, Min(Length(PointPath) - I, Max(1, aCnt div INIT_POLYGONS_COEF)) );
    Idx := 0;
    while (I >= 0) AND (Idx < Length(InitPolygons)) do
    begin
      InitPolygons[Idx] := gAIFields.NavMesh.Point2Polygon[PointPath[I].Y,PointPath[I].X];
      Idx := Idx + 1;
      I := I - 2;
    end;
    if (Idx <> Length(InitPolygons)) then
      SetLength(InitPolygons, Idx);
    Result := InitPolygons;
  end;

  procedure SetOrders(aCnt: Integer; var aPositions: TKMPointArray);
  const
    TIME_PER_A_TILE = 8; // Max ticks per a tile
    INIT_DIST = 1000;
  var
    I, K, Dist, ClosestDist, ClosestIdx: Integer;
    Dir: TKMDirection;
    Position: TKMPoint;
    GT: TGroupType;
    Squads: array of TAISquad;
    AvaiableSquads: TBooleanArray;
    TagPositions: TKMPointTagList;
  begin
    TagPositions := TKMPointTagList.Create;
    try
      for I := 0 to Min(aCnt, High(aPositions)) do
        TagPositions.Add(aPositions[I], KMDistanceAbs(aActualPosition, aPositions[I]));
      TagPositions.SortByTag();
      fScanPosition := TagPositions.Items[TagPositions.Count - 1];
      //fPathPosition := TagPositions.Items[(TagPositions.Count - 1) div 2];
      Dir := KMGetDirection( aActualPosition, fPathPosition );

      SetLength(Squads, aCnt);
      SetLength(AvaiableSquads, aCnt);
      K := 0;
      for GT := Low(TGroupType) to High(TGroupType) do
        for I := 0 to fSquads[GT].Count - 1 do
        begin
          Squads[K] := fSquads[GT].Items[I];
          AvaiableSquads[K] := True;
          K := K + 1;
        end;
      for I := TagPositions.Count - 1 downto 0 do
      begin
        Position := TagPositions.Items[I];
        ClosestDist := INIT_DIST;
        for K := 0 to Length(Squads) - 1 do
          if AvaiableSquads[K] then
          begin
            Dist := KMDistanceAbs(Position, Squads[K].Position);
            if (Dist < ClosestDist) then
            begin
              ClosestDist := Dist;
              ClosestIdx := K;
            end;
          end;
        if (ClosestIdx = INIT_DIST) then
          break;
        Squads[ClosestIdx].FinalPosition := KMPointDir(Position, Dir);
        Squads[ClosestIdx].TimeLimit := gGame.GameTickCount + KMDistanceAbs(Position, Squads[ClosestIdx].Position) * TIME_PER_A_TILE;
        AvaiableSquads[ClosestIdx] := False;
      end;
    finally
      TagPositions.Free;
    end;
  end;

var
  Distance: Word;
  Cnt: Integer;
  TargetPoint: TKMPoint;
  PointPath, Positions: TKMPointArray;
  InitPolygons: TKMWordArray;
begin
  TargetPoint := TargetPosition;
  // Try find path to the target point
  if gAIFields.NavMesh.Pathfinding.ShortestRoute(aActualPosition, TargetPoint, Distance, PointPath) then
  begin
    fDEBUGPointPath := PointPath;
    Cnt := SquadCnt(); // Count of groups in company

    // Get init polygons -> polygons on road which are base for army positioning (groups will get order to walk there)
    InitPolygons := GetInitPolygons(Cnt, PointPath);

    // Get positions around init polygons
    gAIFields.NavMesh.Positioning.FindPositions(Cnt, InitPolygons, Positions);

    // Compute distance of new positions and select the closest group to walk there
    SetOrders(Cnt, Positions);
  end;
end;


function TAICompany.SquadCnt(aTypes: TGroupTypeSet = [Low(TGroupType)..High(TGroupType)]): Word;
var
  GT: TGroupType;
begin
  Result := 0;
  for GT in aTypes do
    Result := Result + fSquads[GT].Count;
end;


procedure TAICompany.OwnerUpdate(aPlayer: TKMHandIndex);
var
  I: Integer;
  GT: TGroupType;
begin
  fOwner := aPlayer;
  for GT := Low(TGroupType) to High(TGroupType) do
    for I := 0 to fSquads[GT].Count-1 do
      TAISquad(fSquads[GT][I]).OwnerUpdate(aPlayer);
end;


procedure TAICompany.AddSquad(aGroup: TKMUnitGroup);
begin
  fSquads[ aGroup.GroupType ].Add( TAISquad.Create(aGroup, fOwner) );
end;


procedure TAICompany.DeleteSquad(aGT: TGroupType; aIdx: Integer);
var
  Squad: TAISquad;
begin
  Squad := fSquads[aGT].Items[aIdx];
  Squad.Free;
  fSquads[aGT].Delete(aIdx);
end;


procedure TAICompany.InitCompany();
begin
  fPathPosition := GetPosition();
end;


function TAICompany.GetPosition(): TKMPoint;
var
  I, Count: Integer;
  Output: TKMPoint;
  G: TGroupType;
begin
  Output := KMPOINT_ZERO;
  Count := 0;
  for G := Low(TGroupType) to High(TGroupType) do
    for I := 0 to fSquads[G].Count-1 do
    begin
      Output := KMPointAdd(Output, TAISquad(fSquads[G].Items[I]).Group.Position);
      Count := Count + 1;
    end;

  if (Count > 0) then
  begin
    Output.X := Round( Output.X / Count );
    Output.Y := Round( Output.Y / Count );
  end;

  // If we cannot walk there choose first group instead
  if not gTerrain.CheckPassability(Output, tpWalk) then
    for G := Low(TGroupType) to High(TGroupType) do
      if (fSquads[G].Count > 0) then
      begin
        Output := TAISquad(fSquads[G].Items[0]).Group.Position;
        break;
      end;
  Result := Output;
end;


function TAICompany.ActualizeTarget(aInitPosition: TKMPoint; var aTargetHouse: TKMHouse; var aTargetUnit: TKMUnit; aAimCivilians: Boolean = True): Boolean;
var
  TargetGroup: TKMUnitGroup;
begin
  Result := False;
  aTargetHouse := gHands.GetClosestHouse(aInitPosition, fOwner, at_Enemy, TARGET_HOUSES, false);
  if (aTargetHouse = nil) then
  begin
    TargetGroup := gHands.GetClosestGroup(aInitPosition, fOwner, at_Enemy);
    if (TargetGroup = nil) then
    begin
      if not aAimCivilians then
        Exit;
      aTargetUnit := gHands.GetClosestUnit(aInitPosition, fOwner, at_Enemy);
      if (aTargetUnit = nil) then
        Exit;
    end
    else
      aTargetUnit := TargetGroup.GetAliveMember;
  end;
  Result := True;
end;


function TAICompany.IsGroupInCompany(aGroup: TKMUnitGroup): Boolean;
var
  I: Integer;
  G: TGroupType;
begin
  Result := True;
  for G := Low(TGroupType) to High(TGroupType) do
    for I := 0 to fSquads[G].Count-1 do
      if (TAISquad(fSquads[G].Items[I]).Group = aGroup) then
        Exit;
  Result := False;
end;









{ TKMArmyAttack }
constructor TKMArmyAttack.Create(aPlayer: TKMHandIndex);
begin
  inherited Create;
  fCompanies := TKMList.Create();
  fOwner := aPlayer;
end;


destructor TKMArmyAttack.Destroy;
begin
  fCompanies.Free;
  inherited;
end;


procedure TKMArmyAttack.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  SaveStream.WriteA('ArmyAttack');
  SaveStream.Write(fOwner);
  SaveStream.Write( Integer(Count) );

  for I := 0 to Count - 1 do
    Company[I].Save(SaveStream);
end;


procedure TKMArmyAttack.Load(LoadStream: TKMemoryStream);
var
  I, NewCount: Integer;
begin
  LoadStream.ReadAssert('ArmyAttack');
  LoadStream.Read(fOwner);
  LoadStream.Read(NewCount);

  for I := 0 to NewCount - 1 do
    fCompanies.Add( TAICompany.Load(LoadStream) );
end;


procedure TKMArmyAttack.SyncLoad();
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Company[I].SyncLoad();
end;


procedure TKMArmyAttack.AfterMissionInit();
begin

end;


procedure TKMArmyAttack.UpdateState(aTick: Cardinal);
var
  I: Integer;
  Company: TAICompany;
begin
  for I := Count - 1 downto 0 do
  begin
    Company := fCompanies.Items[I];
    Company.UpdateState(aTick);
    if (Company.SquadCnt = 0) OR KMSamePoint(Company.TargetPosition, KMPOINT_ZERO)then
      fCompanies.Remove( Company );
  end;
end;


procedure TKMArmyAttack.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
  // Update companies
end;


function TKMArmyAttack.GetCount(): Integer;
begin
  Result := fCompanies.Count;
end;


function TKMArmyAttack.GetCompany(aIdx: Integer): TAICompany;
begin
  Result := fCompanies.Items[aIdx];
end;


function TKMArmyAttack.IsGroupInAction(aGroup: TKMUnitGroup): Boolean;
var
  I: Integer;
  Company: TAICompany;
begin
  Result := True;
  for I := 0 to Count - 1 do
  begin
    Company := fCompanies.Items[I];
    if Company.IsGroupInCompany(aGroup) then
      Exit;
  end;
  Result := False;
end;


// Find best target -> to secure that AI will be as universal as possible find only point in map and company will destroy everything around automatically
function TKMArmyAttack.FindBestTarget(var TargetOwner: TKMHandIndex; var aTargetPoint: TKMPoint; aForceToAttack: Boolean = False): Boolean;
const
  DISTANCE_COEF = 0.75; // If second enemy is twice as far away decrease chance by 3/8
  MIN_COMPARSION = 0.3; // 30% advantage for attacker
var
  I, MinDist: Integer;
  Comparison, BestComparison: Single;
  Group: TKMUnitGroup;
  CenterPoints: TKMPointArray;
  EnemyStats: TKMEnemyStatisticsArray;
begin
  Result := False;
  aTargetPoint := KMPOINT_ZERO;

  // Find center point of city / army (where we should start scan - init point / center screen is useless for this)
  CenterPoints := gAIFields.Eye.GetCityCenterPoints(True);
  if (Length(CenterPoints) = 0) then // No important houses were found -> try find soldier
  begin
    Group := nil;
    Group := gHands[fOwner].UnitGroups.Groups[ KaMRandom(gHands[fOwner].UnitGroups.Count) ];
    if (Group <> nil) then
    begin
      SetLength(CenterPoints, 1);
      CenterPoints[0] := Group.Position;
    end
    else
      Exit;
  end;

  // Try find enemies by influence area
  if not gAIFields.Influences.InfluenceSearch.FindClosestEnemies(fOwner, CenterPoints, True) then // Try find hostile house
    if not gAIFields.Influences.InfluenceSearch.FindClosestEnemies(fOwner, CenterPoints, False) then // Try find hostile unit
      Exit;
  EnemyStats := gAIFields.Influences.InfluenceSearch.EnemiesStats;

  // Calculate strength of alliance, find best comparison - value in interval <-1,1>, positive value = advantage, negative = disadvantage
  if (Length(EnemyStats) > 0) then
  begin
    // Find closest enemy
    MinDist := High(Integer);
    for I := 0 to Length(EnemyStats) - 1 do
      if (MinDist > EnemyStats[I].Distance) then
        MinDist := EnemyStats[I].Distance;

    BestComparison := -2;
    for I := 0 to Length(EnemyStats) - 1 do
    begin
      Comparison := gAIFields.Eye.ArmyEvaluation.CompareAllianceStrength(fOwner, EnemyStats[I].Player) - (EnemyStats[I].Distance / Max(1,MinDist) - 1) * DISTANCE_COEF;
      if (Comparison > BestComparison) then
      begin
        BestComparison := Comparison;
        TargetOwner := EnemyStats[I].Player;
        aTargetPoint := EnemyStats[I].ClosestPoint;
      end;
    end;
  end;

  Result := (Length(EnemyStats) > 0) AND (aForceToAttack OR (BestComparison > MIN_COMPARSION));
end;


//function TKMArmyAttack.SelectClosestTarget(aInitPoint: TKMPoint; var aTargetHouse: TKMHouse; var aTargetUnit: TKMUnit; var aTargetGroup: TKMUnitGroup): Boolean;
//begin
//  Result := False;
//  aTargetHouse := nil;
//  aTargetUnit := nil;
//  aTargetGroup := nil;
//  // Compute common stuf first
//  aTargetHouse := gHands.GetClosestHouse(aInitPoint, fOwner, at_Enemy, TARGET_HOUSES, false);
//  if (aTargetHouse = nil) then
//  begin
//    aTargetGroup := gHands.GetClosestGroup(aInitPoint, fOwner, at_Enemy);
//    if (aTargetGroup = nil) then
//    begin
//      aTargetUnit := gHands.GetClosestUnit(aInitPoint, fOwner, at_Enemy);
//      if (aTargetUnit = nil) then
//         Exit;
//    end;
//  end;
//
//  Result := True;
//end;


procedure TKMArmyAttack.OrderToAttack(aTargetPoint: TKMPoint; aGroups: TKMUnitGroupArray);
var
  I: Integer;
  Company: TAICompany;
  TargetHouse: TKMHouse;
  TargetUnit: TKMUnit;
begin
  TargetHouse := nil;
  TargetUnit := nil;
  Company := TAICompany.Create(fOwner);
  fCompanies.Add( Company );
  if Company.ActualizeTarget(aTargetPoint, TargetHouse, TargetUnit) then
  begin
    Company.SetTarget(TargetHouse, TargetUnit);
    for I := 0 to Length(aGroups) - 1 do
      Company.AddSquad( aGroups[I] );
    Company.InitCompany();
  end
  else
    fCompanies.Remove( Company );
end;



procedure TKMArmyAttack.LogStatus(var aBalanceText: UnicodeString);
begin
  //aBalanceText := '';
end;


procedure TKMArmyAttack.Paint();
const
  COLOR_WHITE = $80FFFFFF;
  COLOR_BLACK = $80000000;
  COLOR_GREEN = $6000FF00;
  COLOR_RED = $800000FF;
  COLOR_YELLOW = $8000FFFF;
  COLOR_GREEN_Field = $4400FF00;
  COLOR_GREEN_1 = $3355FFFF;
  COLOR_BLUE = $60FF0000;
var
  I,K,J: Integer;
  Position: TKMPoint;
  GT: TGroupType;
  Company: TAICompany;
  Squad: TAISquad;
begin
  if (fOwner <> gMySpectator.HandIndex) then // Show just 1 player (it prevents notification to be mess)
    Exit;
  for I := 0 to Count - 1 do
  begin
    Company := fCompanies.Items[I];
    Position := Company.ScanPosition;
    gRenderAux.CircleOnTerrain(Position.X, Position.Y, COMPANY_SCAN_RAD, $09FFFFFF, $99FFFFFF);
    if (Length(Company.PointPath) > 0) then
      for K := Length(Company.PointPath)-2 downto 0 do
        gRenderAux.LineOnTerrain(Company.PointPath[K+1], Company.PointPath[K], $9900FFFF);
    for GT := Low(TGroupType) to High(TGroupType) do
      for K := Company.Squads[GT].Count - 1 downto 0 do
      begin
        Squad := Company.Squads[GT].Items[K];
        Position := Squad.Position;
        gRenderAux.CircleOnTerrain(Position.X, Position.Y, 1, $0500FF00, $6600FF00);
        if (Squad.TargetHouse <> nil) then
          gRenderAux.LineOnTerrain(Position, Squad.TargetHouse.GetPosition, $AA6666FF)
        else if (Squad.TargetUnit <> nil) then
          gRenderAux.LineOnTerrain(Position, Squad.TargetUnit.GetPosition, $AA6666FF)
        else if not KMSamePoint(Squad.FinalPosition.Loc,KMPOINT_ZERO) then
        begin
          //gRenderAux.LineOnTerrain(Position, Squad.FinalPosition.Loc, $AAFF5555);
          gRenderAux.CircleOnTerrain(Squad.FinalPosition.Loc.X, Squad.FinalPosition.Loc.Y, 1, $05FF0000, $66FF0000);
          if (Length(Squad.PointPath) > 0) then
            for J := Length(Squad.PointPath)-2 downto 0 do
              gRenderAux.LineOnTerrain(Squad.PointPath[J+1], Squad.PointPath[J], COLOR_BLUE);
        end;
      end;
  end;
  //gRenderAux.LineOnTerrain(P1, P2, $CCFF2222);
  //gRenderAux.CircleOnTerrain(CenterPlatoon.X, CenterPlatoon.Y, 5, $09FFFFFF, $99FFFFFF);
  //gRenderAux.Quad(Loc.X, Loc.Y, Color);
end;

end.
