unit KM_CityPlanner;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, KromUtils, Math, SysUtils,
  KM_Defaults, KM_Points, KM_CommonClasses, KM_CommonTypes,
  KM_TerrainFinder, KM_PerfLog, KM_Houses, KM_ResHouses, KM_ResWares,
  KM_PathFindingRoad, KM_CityPredictor, KM_Eye;

const
  AVOID_BUILDING_UNLOCK = 0;
  AVOID_BUILDING_HOUSE_OUTSIDE_LOCK = 30;
  AVOID_BUILDING_HOUSE_INSIDE_LOCK = 40;
  AVOID_BUILDING_NODE_LOCK_ROAD = 45;
  AVOID_BUILDING_NODE_LOCK_FIELD = 50;

var

  ManTune_PLANNER_FindPlaceForHouse_Influence       : Single = 40;
  ManTune_PLANNER_FindPlaceForWoodcutter_Influence  : Single = 20;
  {
  GA_PLANNER_SnapCrit_SnapToHouse                   : Single = 14.97831154;
  GA_PLANNER_SnapCrit_SnapToRoad                    : Single = 22.356440544;
  GA_PLANNER_SnapCrit_SnapToField                   : Single = 22.54335022;
  GA_PLANNER_SnapCrit_SnapToWine                    : Single = 23.59727478;
  GA_PLANNER_FieldCrit_SnapToNoFieldAreas           : Single = 2.945933819;
  GA_PLANNER_FindPlaceForHouse_SnapCrit             : Single = 12.62865162;
  GA_PLANNER_FindPlaceForHouse_DistCrit             : Single = 11.08990955;
  GA_PLANNER_FindPlaceForHouse_TreeInPlan           : Single = 10.98747635;
  GA_PLANNER_FindPlaceForHouse_FarmCrit             : Single = 10.98747635;
  GA_PLANNER_FindPlaceForQuary_SnapCrit             : Single = 7.913424015;
  GA_PLANNER_FindPlaceForQuary_DistCrit             : Single = 25.68084908;
  GA_PLANNER_FindPlaceForWoodcutter_DistFromForest  : Single = 27.32363129;
  GA_PLANNER_FindPlaceForWoodcutter_DistCrit        : Single = 5.61446953;
  GA_PLANNER_FindPlaceForWoodcutter_TreeCnt         : Single = 16.00013351;
  GA_PLANNER_FindPlaceForWoodcutter_SnapToEdge      : Single = 30.00013351;
  GA_PLANNER_FindPlaceForWoodcutter_CanPlaceTreeCnt : Single = 26.43145752;
  GA_PLANNER_FindPlaceForWoodcutter_PlansAround     : Single = 21.26153564;
  GA_PLANNER_DistCrit_CenterStore                   : Single = 12.46767139;
  GA_PLANNER_DistCrit_Store                         : Single = 16.98966408;
  GA_PLANNER_DistCrit_School                        : Single = 22.56131363;
  GA_PLANNER_DistCrit_Inn_Store                     : Single = 0.1000000015;
  GA_PLANNER_DistCrit_Inn_Inn                       : Single = 3.115944862;
  GA_PLANNER_DistCrit_Marketplace                   : Single = 16.9609642;
  GA_PLANNER_DistCrit_IronSmithy_Self               : Single = 22.67551041;
  GA_PLANNER_DistCrit_IronSmithy_Res                : Single = 10.71246529;
  GA_PLANNER_DistCrit_ArmorSmithy_Set               : Single = 28.41605949;
  GA_PLANNER_DistCrit_ArmorSmithy_Res               : Single = 23.25997734;
  GA_PLANNER_DistCrit_WeaponSmithy_Set              : Single = 8.050050735;
  GA_PLANNER_DistCrit_WeaponSmithy_Res              : Single = 8.690607071;
  GA_PLANNER_DistCrit_Tannery_Set                   : Single = 26.4419632;
  GA_PLANNER_DistCrit_ArmorWorkshop_Set             : Single = 20.60627747;
  GA_PLANNER_DistCrit_WeaponWorkshop_Set            : Single = 26.82677269;
  GA_PLANNER_DistCrit_Barracks_Set                  : Single = 25.63218307;
  GA_PLANNER_DistCrit_Bakery_Set                    : Single = 8.2283535;
  GA_PLANNER_DistCrit_Bakery_Res                    : Single = 10.03507137;
  GA_PLANNER_DistCrit_Butchers_Set                  : Single = 6.236026764;
  GA_PLANNER_DistCrit_Butchers_Res                  : Single = 19.85456467;
  GA_PLANNER_DistCrit_Mill_Set                      : Single = 11.99028778;
  GA_PLANNER_DistCrit_Mill_Res                      : Single = 10.05409908;
  GA_PLANNER_DistCrit_Swine_Set                     : Single = 19.73905563;
  GA_PLANNER_DistCrit_Swine_Res                     : Single = 22.38061333;
  GA_PLANNER_DistCrit_Stables_Set                   : Single = 11.59313393;
  GA_PLANNER_DistCrit_Stables_Res                   : Single = 28.58532333;
  GA_PLANNER_DistCrit_Farm_Set                      : Single = 0.1000000015;
  GA_PLANNER_DistCrit_Farm_Res                      : Single = 9.136079788;
  GA_PLANNER_DistCrit_Wineyard_Set                  : Single = 13.15403557;
  GA_PLANNER_DistCrit_Wineyard_Res                  : Single = 20.49876976;
  GA_PLANNER_DistCrit_Metallurgists_Set             : Single = 13.86183834;
  GA_PLANNER_DistCrit_Metallurgists_Res             : Single = 15.66738892;
  GA_PLANNER_DistCrit_GoldMine_Set                  : Single = 13.25415707;
  GA_PLANNER_DistCrit_CoalMine_Set                  : Single = 26.43909264;
  GA_PLANNER_DistCrit_IronMine_Set                  : Single = 30;
  GA_PLANNER_DistCrit_Quary_Set                     : Single = 16.07818031;
  GA_PLANNER_DistCrit_Woodcutters_Set               : Single = 30;
  GA_PLANNER_DistCrit_Sawmill_Set                   : Single = 2.060489893;
  //}
  //{
  GA_PLANNER_SnapCrit_SnapToHouse                   : Single = 40.35658836;
  GA_PLANNER_SnapCrit_SnapToFields                  : Single = 40;
  GA_PLANNER_FindPlaceForHouse_SnapCrit             : Single = 17.10942268;
  GA_PLANNER_FindPlaceForHouse_DistCrit             : Single = 13.00957012;
  GA_PLANNER_FindPlaceForHouse_TreeInPlan           : Single = 13.50516129;
  GA_PLANNER_FindPlaceForHouse_FarmCrit             : Single = 21.58874512;
  GA_PLANNER_FindPlaceForQuary_SnapCrit             : Single = 26.14780426;
  GA_PLANNER_FindPlaceForQuary_DistCrit             : Single = 37.61137009;
  GA_PLANNER_FindPlaceForWoodcutter_DistFromForest  : Single = 28.76280212;
  GA_PLANNER_FindPlaceForWoodcutter_DistCrit        : Single = 44.20793533;
  GA_PLANNER_FindPlaceForWoodcutter_TreeCnt         : Single = 0.1000000015;
  GA_PLANNER_FindPlaceForWoodcutter_SnapToEdge      : Single = 5.223324299;
  GA_PLANNER_FindPlaceForWoodcutter_CanPlaceTreeCnt : Single = 30.30228996;
  GA_PLANNER_FindPlaceForWoodcutter_PlansAround     : Single = 4.992388725;
  GA_PLANNER_DistCrit_CenterStore                   : Single = 3.870659351;
  GA_PLANNER_DistCrit_Store                         : Single = 0.1000000015;
  GA_PLANNER_DistCrit_School                        : Single = 39.48722076;
  GA_PLANNER_DistCrit_Inn_Store                     : Single = 50;
  GA_PLANNER_DistCrit_Inn_Inn                       : Single = 32.82717896;
  GA_PLANNER_DistCrit_Marketplace                   : Single = 35.4796524;
  GA_PLANNER_DistCrit_IronSmithy_Self               : Single = 3.963907719;
  GA_PLANNER_DistCrit_IronSmithy_Res                : Single = 28.90864563;
  GA_PLANNER_DistCrit_ArmorSmithy_Set               : Single = 1.426353335;
  GA_PLANNER_DistCrit_ArmorSmithy_Res               : Single = 7.746329784;
  GA_PLANNER_DistCrit_WeaponSmithy_Set              : Single = 4.051534653;
  GA_PLANNER_DistCrit_WeaponSmithy_Res              : Single = 11.29830551;
  GA_PLANNER_DistCrit_Tannery_Set                   : Single = 49.24342728;
  GA_PLANNER_DistCrit_ArmorWorkshop_Set             : Single = 4.114400864;
  GA_PLANNER_DistCrit_WeaponWorkshop_Set            : Single = 4.108706474;
  GA_PLANNER_DistCrit_Barracks_Set                  : Single = 13.32427883;
  GA_PLANNER_DistCrit_Bakery_Set                    : Single = 0.1000000015;
  GA_PLANNER_DistCrit_Bakery_Res                    : Single = 47.20677948;
  GA_PLANNER_DistCrit_Butchers_Set                  : Single = 50;
  GA_PLANNER_DistCrit_Butchers_Res                  : Single = 31.10999107;
  GA_PLANNER_DistCrit_Mill_Set                      : Single = 18.05562401;
  GA_PLANNER_DistCrit_Mill_Res                      : Single = 50;
  GA_PLANNER_DistCrit_Swine_Set                     : Single = 22.83971596;
  GA_PLANNER_DistCrit_Swine_Res                     : Single = 11.5138216;
  GA_PLANNER_DistCrit_Stables_Set                   : Single = 30.27074623;
  GA_PLANNER_DistCrit_Stables_Res                   : Single = 31.47228622;
  GA_PLANNER_DistCrit_Farm_Set                      : Single = 24.62744522;
  GA_PLANNER_DistCrit_Farm_Res                      : Single = 33.97316742;
  GA_PLANNER_DistCrit_Wineyard_Set                  : Single = 45.15957642;
  GA_PLANNER_DistCrit_Wineyard_Res                  : Single = 50;
  GA_PLANNER_DistCrit_Metallurgists_Set             : Single = 50;
  GA_PLANNER_DistCrit_Metallurgists_Res             : Single = 7.292239189;
  GA_PLANNER_DistCrit_GoldMine_Set                  : Single = 35.09430313;
  GA_PLANNER_DistCrit_CoalMine_Set                  : Single = 29.58112717;
  GA_PLANNER_DistCrit_IronMine_Set                  : Single = 29.57572556;
  GA_PLANNER_DistCrit_Quary_Set                     : Single = 17.80814362;
  GA_PLANNER_DistCrit_Woodcutters_Set               : Single = 38.6776886;
  GA_PLANNER_DistCrit_Sawmill_Set                   : Single = 10.2895546;
  //}

type
  THousePlan = record
    Placed, Exhausted, ShortcutsCompleted, RemoveTreeInPlanProcedure, HouseReservation: Boolean;  // EDIT
    UID: Integer;
    Loc, SpecPoint: TKMPoint;
  end;
  TPlannedHousesArray = array [HOUSE_MIN..HOUSE_MAX] of array of THousePlan;


  TPathFindingCityPlanner = class(TPathFindingRoad)
  private
  protected
    function MovementCost(aFromX, aFromY, aToX, aToY: Word): Word; override;
  public
  end;

  TPathFindingShortcutsCityPlanner = class(TPathFindingCityPlanner)
  private
  protected
    function DestinationReached(aX, aY: Word): Boolean; override;
    function MovementCost(aFromX, aFromY, aToX, aToY: Word): Word; override;
  public
  end;

  // Create plan of city AfterMissionInit and update it during game (if it is required)
  TKMCityPlanner = class
  private
    fOwner: TKMHandIndex;
    fPlannedHouses: TPlannedHousesArray;

    fRoadPlanner: TPathFindingCityPlanner;
    fRoadShortcutPlanner: TPathFindingShortcutsCityPlanner;

    procedure AddPlan(aHT: THouseType; aLoc: TKMPoint); overload;
    procedure AddPlan(aHT: THouseType; aLoc: TKMPoint; aSpecPoint: TKMPoint); overload;
    function GetPlan(aHT: THouseType; out aLoc: TKMPoint; out aIdx: Integer): Boolean;
    function GetTreesInHousePlanCnt(aHT: THouseType; aLoc: TKMPoint): Byte;

    function SnapCrit(aHT: THouseType; aLoc: TKMPoint): Single;
    function DistCrit(aHT: THouseType; aLoc: TKMPoint): Single;
    function FieldCrit(aHT: THouseType; aLoc: TKMPoint): Single;

    procedure PlanWineFields(aLoc: TKMPoint; var aNodeList: TKMPointList);
    procedure PlanFarmFields(aLoc: TKMPoint; var aNodeList: TKMPointList);
    function FindPlaceForHouse(aUnlockProcedure: Boolean; aHT: THouseType; out aBestLocs: TKMPointArray): Byte;
    function FindPlaceForMines(aHT: THouseType): Boolean;
    function FindPlaceForWoodcutter(): Boolean;
  public
    constructor Create(aPlayer: TKMHandIndex);
    destructor Destroy(); override;

    procedure AfterMissionInit();
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    function GetBlockingTrees(aHT: THouseType; aLoc: TKMPoint; var aTrees: TKMPointArray): Boolean;
    function GetBlockingFields(aHT: THouseType; aLoc: TKMPoint; var aFields: TKMPointArray): Boolean;

    // Properties for GA (in Runner)
    property PlannedHouses: TPlannedHousesArray read fPlannedHouses write fPlannedHouses;

    procedure MarkAsExhausted(aHT: THouseType; aLoc: TKMPoint);

    procedure RemovePlan(aHT: THouseType; aLoc: TKMPoint); overload;
    procedure RemovePlan(aHT: THouseType; aIdx: Integer); overload;

    function GetHousePlan(aUnlockProcedure: Boolean; aHT: THouseType; var aLoc: TKMPoint; var aIdx: Integer): Boolean;
    function GetRoadBetweenPoints(aStart, aEnd: TKMPoint; var aField: TKMPointList; var aFieldType: TFieldType): Boolean;
    function GetRoadToHouse(aHT: THouseType; aIdx: Integer; var aField: TKMPointList; var aFieldType: TFieldType): Boolean;
    function GetFieldToHouse(aHT: THouseType; aIdx: Integer; var aField: TKMPointList; var aFieldType: TFieldType): Boolean;
    function GetTreesInHousePlan(aHT: THouseType; aIdx: Integer; var aField: TKMPointList): Byte;

    procedure UpdateState(aTick: Cardinal);
    procedure Paint();
  end;



implementation
uses
  KM_Game, KM_HouseCollection, KM_HouseSchool, KM_HandsCollection, KM_Hand, KM_Terrain, KM_Resource,
  KM_AIFields, KM_Units, KM_UnitTaskDelivery, KM_UnitActionWalkTo, KM_UnitTaskGoEat, KM_UnitsCollection,
  KM_NavMesh, KM_HouseMarket, KM_CommonUtils,
  KM_RenderAux;






{ TKMCityPlanner }
constructor TKMCityPlanner.Create(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
  fRoadPlanner := TPathFindingCityPlanner.Create(fOwner);
  fRoadShortcutPlanner := TPathFindingShortcutsCityPlanner.Create(fOwner);
end;


destructor TKMCityPlanner.Destroy();
begin
  fRoadPlanner.Free;
  fRoadShortcutPlanner.Free;
  inherited;
end;


procedure TKMCityPlanner.AfterMissionInit();
var
  I: Integer;
  Houses: TKMHousesCollection;
  HT: THouseType;
  IdxArr: array [HOUSE_MIN..HOUSE_MAX] of Word;
begin
  for HT := HOUSE_MIN to HOUSE_MAX do
  begin
    IdxArr[HT] := gHands[fOwner].Stats.GetHouseQty(HT);
    SetLength(fPlannedHouses[HT], IdxArr[HT]);
  end;

  Houses := gHands[fOwner].Houses;
  for I := 0 to Houses.Count - 1 do
  begin
    HT := Houses[I].HouseType;
    Dec(IdxArr[HT], 1);
    fPlannedHouses[HT,IdxArr[HT]].Placed := True;
    fPlannedHouses[HT,IdxArr[HT]].Loc := Houses[I].Entrance;
    fPlannedHouses[HT,IdxArr[HT]].UID := Houses[I].UID;
  end;
end;

procedure TKMCityPlanner.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
end;

procedure TKMCityPlanner.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);
  //fTagListGold.SaveToStream(SaveStream);

end;

procedure TKMCityPlanner.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);
  //fTagListGold.LoadFromStream(LoadStream);

end;




procedure TKMCityPlanner.UpdateState(aTick: Cardinal);

  procedure CheckWoodcutter(aHousePlan: THousePlan; aHouse: TKMHouse);
  var
    W: TKMHouseWoodcutters;
  begin
    // Make sure that this house is woodcutter
    if (aHouse.HouseType <> ht_Woodcutters) then
      Exit;
    W := TKMHouseWoodcutters(aHouse);
    // Check if is point already set (compare with default cutting point)
    if W.IsCuttingPointSet or KMSamePoint(aHousePlan.SpecPoint, KMPOINT_ZERO) then
      Exit;
    // Set the cutting point (it can be in lager distance so check is needed)
    W.CuttingPoint := aHousePlan.SpecPoint;
    //W.ValidateCuttingPoint;
    // Check chop-only mode woodcutter
    if (gAIFields.Influences.AvoidBuilding[aHousePlan.SpecPoint.Y, aHousePlan.SpecPoint.X] = 0) then // Center of forest is not in protected area => chop only mode
      W.WoodcutterMode := wcm_Chop;
  end;

var
  I,K: Integer;
  HT: THouseType;
  H: TKMHouse;
begin
  // ADD PRIORITY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  for HT := Low(fPlannedHouses) to High(fPlannedHouses) do
    for I := Low(fPlannedHouses[HT]) to High(fPlannedHouses[HT]) do
    begin
      if fPlannedHouses[HT,I].Placed then
      begin
        H := gHands[fOwner].Houses.GetHouseByUID(fPlannedHouses[HT,I].UID);
        fPlannedHouses[HT,I].Placed := (H <> nil) AND not H.IsDestroyed;
        if (HT = ht_Woodcutters) AND fPlannedHouses[HT,I].Placed AND (H.IsComplete) then
          CheckWoodcutter(fPlannedHouses[HT,I], H);
      end
      else
        for K := 0 to gHands[fOwner].Houses.Count - 1 do
          if KMSamePoint(fPlannedHouses[HT,I].Loc, gHands[fOwner].Houses[K].Entrance) then
          begin
            fPlannedHouses[HT,I].Placed := not gHands[fOwner].Houses[K].IsDestroyed;
            fPlannedHouses[HT,I].UID := gHands[fOwner].Houses[K].UID;
            break;
          end;
    end;
end;


procedure TKMCityPlanner.AddPlan(aHT: THouseType; aLoc: TKMPoint);
begin
  AddPlan(aHT, aLoc, KMPOINT_ZERO); // Cannot declare KMPOINT_ZERO default value so overload method is used instead
end;

procedure TKMCityPlanner.AddPlan(aHT: THouseType; aLoc: TKMPoint; aSpecPoint: TKMPoint);
begin
  SetLength(fPlannedHouses[aHT], Length(fPlannedHouses[aHT])+1); // EDIT IT
  with fPlannedHouses[aHT, High(fPlannedHouses[aHT])] do
  begin
    Exhausted := False;
    Placed := False;
    ShortcutsCompleted := False;
    RemoveTreeInPlanProcedure := False;
    HouseReservation := False;
    //NearOwner := (gAIFields.Influences.GetBestOwner(aLoc.X, aLoc.Y) = fOwner);
    Loc := aLoc;
    SpecPoint := aSpecPoint;
  end;
end;


function TKMCityPlanner.GetPlan(aHT: THouseType; out aLoc: TKMPoint; out aIdx: Integer): Boolean;
const
  MAX_BID = 1000000;
  CHOP_ONLY_ADVANTAGE = 10;
  TREE_PENALIZATION = 5;

  // Try find gold / iron in range of mine
  function IsExhaustedMine(aMineLoc: TKMPoint; aIsGold: Boolean): Boolean;
  var
    Y, X: Integer;
  begin
    Result := False;
    for X := Max(aMineLoc.X-4, 1) to Min(aMineLoc.X+3, gTerrain.MapX-1) do
    for Y := Max(aMineLoc.Y-8, 1) to aMineLoc.Y do
      if ( not aIsGold AND (gTerrain.TileIsIron(X, Y) > 0) )
          OR ( aIsGold AND (gTerrain.TileIsGold(X, Y) > 0) ) then
      Exit;
    Result := True;
  end;

  function IsExhaustedQuary(aQuaryLoc: TKMPoint): Boolean;
  var
    Y, X: Integer;
  const
    RADIUS = 6;
  begin
    Result := False;
    for Y := Max(aQuaryLoc.Y-RADIUS, 1) to Min(aQuaryLoc.Y+RADIUS, gTerrain.MapY-1) do
    for X := Max(aQuaryLoc.X-RADIUS, 1) to Min(aQuaryLoc.X+RADIUS, gTerrain.MapX-1) do
      if (gTerrain.TileIsStone(X, Y) > 1) then
        Exit;
    Result := True;
  end;

  function IsExhaustedCoalMine(aCoalLoc: TKMPoint): Boolean;
  var
    X,Y,I: Integer;
    HMA: THouseMappingArray;
  begin
    Result := False;
    HMA := gAIFields.Eye.HousesMapping;
    for I := Low(HMA[ht_CoalMine].Tiles) to High(HMA[ht_CoalMine].Tiles) do
    begin
      X := aCoalLoc.X + HMA[ht_CoalMine].Tiles[I].X;
      Y := aCoalLoc.Y + HMA[ht_CoalMine].Tiles[I].Y;
      if (gTerrain.TileIsCoal(X, Y) > 1) then
        Exit;
    end;
    Result := True;
  end;

  function CheckMine(aIdx: Integer): Boolean;
  begin
    case aHT of
      ht_GoldMine: fPlannedHouses[aHT,aIdx].Exhausted := fPlannedHouses[aHT,aIdx].Exhausted OR IsExhaustedMine(fPlannedHouses[aHT,aIdx].Loc, True);
      ht_IronMine: fPlannedHouses[aHT,aIdx].Exhausted := fPlannedHouses[aHT,aIdx].Exhausted OR IsExhaustedMine(fPlannedHouses[aHT,aIdx].Loc, False);
      ht_CoalMine: fPlannedHouses[aHT,aIdx].Exhausted := fPlannedHouses[aHT,aIdx].Exhausted OR IsExhaustedCoalMine(fPlannedHouses[aHT,aIdx].Loc);
      ht_Quary:    fPlannedHouses[aHT,aIdx].Exhausted := fPlannedHouses[aHT,aIdx].Exhausted OR IsExhaustedQuary(fPlannedHouses[aHT,aIdx].Loc);
      else // ht_Woodcutters - in chop only mode (exhausted will be switched in different position)
        begin
        end;
    end;
    Result := fPlannedHouses[aHT,aIdx].Exhausted;
  end;

  function DistFromStore(aLoc: TKMPoint): Single;
  var
    I: Integer;
    Output, Bid: Single;
  begin
    Output := MAX_BID;
    for I := Low(fPlannedHouses[ht_Store]) to High(fPlannedHouses[ht_Store]) do
    begin
      Bid := KMDistanceAbs(aLoc, fPlannedHouses[ht_Store,I].Loc);
      if (Bid < Output) then
        Output := Bid;
    end;
    if (Output = MAX_BID) then
      Output := 0;
    Result := Output;
  end;

var
  Output: Boolean;
  I, BestIdx: Integer;
  Bid, BestBid: Single;
  SpecPoint: TKMPoint;
begin
  Output := False;
  BestBid := MAX_BID;
  for I := High(fPlannedHouses[aHT]) downto Low(fPlannedHouses[aHT]) do
    if not fPlannedHouses[aHT,I].Placed then
      if fPlannedHouses[aHT,I].RemoveTreeInPlanProcedure OR gAIFields.Eye.CanAddHousePlan(fPlannedHouses[aHT,I].Loc, aHT, True, True) then
      begin
        if (aHT in [ht_GoldMine, ht_IronMine, ht_CoalMine, ht_Quary, ht_Woodcutters]) AND CheckMine(I) then // Filter mines / chop-only woodcutters
          continue;
        Bid := DistFromStore(fPlannedHouses[aHT,I].Loc) + GetTreesInHousePlanCnt(aHT, fPlannedHouses[aHT,I].Loc) * TREE_PENALIZATION;
        if  (aHT = ht_Woodcutters) then
        begin
          SpecPoint := fPlannedHouses[aHT,I].SpecPoint;
          Bid := Bid - Byte(gAIFields.Influences.AvoidBuilding[SpecPoint.Y, SpecPoint.X] = 0) * CHOP_ONLY_ADVANTAGE; // Chop only mode
        end;
        if (Bid < BestBid) then
        begin
          BestBid := Bid;
          BestIdx := I;
        end;
        break;
      end
      else
        RemovePlan(aHT, I);
  if (BestBid <> MAX_BID) then
  begin
    aLoc := fPlannedHouses[aHT,BestIdx].Loc;
    aIdx := BestIdx;
    Output := True;
  end;
  Result := Output;
end;


procedure TKMCityPlanner.MarkAsExhausted(aHT: THouseType; aLoc: TKMPoint);
var
  I: Integer;
begin
  for I := Low(fPlannedHouses[aHT]) to High(fPlannedHouses[aHT]) do
    if KMSamePoint(fPlannedHouses[aHT,I].Loc,aLoc) then
    begin
      fPlannedHouses[aHT,I].Exhausted := True;
      break;
    end;
end;


procedure TKMCityPlanner.RemovePlan(aHT: THouseType; aLoc: TKMPoint);
var
  Check: Boolean;
  I: Integer;
begin
  Check := False;
  for I := Low(fPlannedHouses[aHT]) to High(fPlannedHouses[aHT]) do
    if KMSamePoint(fPlannedHouses[aHT,I].Loc,aLoc) then
    begin
      Check := True;
      break;
    end;
  if not Check then
    Exit;
  RemovePlan(aHT, I);
end;

procedure TKMCityPlanner.RemovePlan(aHT: THouseType; aIdx: Integer);
begin
  if (aIdx > High(fPlannedHouses[aHT])) then
    Exit;
  fPlannedHouses[aHT,aIdx].Exhausted := True;
  fPlannedHouses[aHT,aIdx] := fPlannedHouses[aHT, High(fPlannedHouses[aHT])];
  SetLength(fPlannedHouses[aHT], Length(fPlannedHouses[aHT])-1 ); // EDIT IT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end;


function TKMCityPlanner.GetHousePlan(aUnlockProcedure: Boolean; aHT: THouseType; var aLoc: TKMPoint; var aIdx: Integer): Boolean;
var
  Output: Boolean;
  Cnt: Byte;
  BestLocs: TKMPointArray;
begin
  Output := False;
  if GetPlan(aHT, aLoc, aIdx) then
    Output := True
  else
  begin
    case aHT of
      ht_Woodcutters: FindPlaceForWoodcutter();
      ht_GoldMine, ht_CoalMine, ht_IronMine, ht_Quary: FindPlaceForMines(aHT);
      else
      begin
        Cnt := FindPlaceForHouse(aUnlockProcedure, aHT, BestLocs);
        if (Cnt > 0) then
        begin
          AddPlan(aHT, BestLocs[0]);
          aLoc := BestLocs[0];
          Output := True;
        end;
      end;
    end;
    if GetPlan(aHT, aLoc, aIdx) then
      Output := True;
  end;
  Result := Output;
end;


function TKMCityPlanner.GetRoadToHouse(aHT: THouseType; aIdx: Integer; var aField: TKMPointList; var aFieldType: TFieldType): Boolean;
// New house plan may overlap existing road -> new road must be done (new road will extend aField list)
  procedure ReplaceOverlappingRoad(aLoc: TKMPoint);
  const
    VECTOR_ARR: array[0..3] of TKMPoint = (  (X:0; Y:1), (X:1; Y:0), (X:0; Y:-1), (X:-1; Y:0)  ); // Move actual position to left, top, right and down
  var
    I,K,RoadsInsidePlanIdx: Integer;
    Point: TKMPoint;
    Road, Path: TKMPointList;
    HMA: THouseMappingArray;
  begin
    HMA := gAIFields.Eye.HousesMapping;
    Road := TKMPointList.Create();
    Path := TKMPointList.Create();
    try
      // Find all road inside of newly placed house plan
      for I := Low(HMA[aHT].Tiles) to High(HMA[aHT].Tiles) do
      begin
        Point := KMPointAdd(aLoc, HMA[aHT].Tiles[I]);
        if gTerrain.TileIsWalkableRoad(Point) then
          Road.Add(Point);
      end;
      // Get outside roads which are connected to road plans inside of the house
      RoadsInsidePlanIdx := Road.Count - 1; // Use Road list for those points
      if RoadsInsidePlanIdx >= 0 then
      begin
        for I := RoadsInsidePlanIdx downto 0 do
          for K := Low(VECTOR_ARR) to High(VECTOR_ARR) do
          begin
            Point := KMPointAdd(Road.Items[I], VECTOR_ARR[K]);
            if gTerrain.TileIsWalkableRoad(Point) AND not Road.Contains(Point) then
              Road.Add(Point);
          end;
        Point := Road.Items[RoadsInsidePlanIdx + 1];
        for I := RoadsInsidePlanIdx + 2 to Road.Count - 1 do
        begin
          Path.Clear;
          if fRoadShortcutPlanner.Route_Make(Road.Items[I], Point, Path) then
            for K := 0 to Path.Count - 1 do
              aField.Add(Path.Items[K]);
        end;
      end;
    finally
      Road.Free;
      Path.Free;
    end;
  end;

var
  Output: Boolean;
  Loc: TKMPoint;
  H: TKMHouse;
begin
  Output := False;
  aFieldType := ft_Road;
  Loc := KMPointBelow(fPlannedHouses[aHT,aIdx].Loc);
  H := gHands[fOwner].Houses.FindHouse(ht_Any, Loc.X, Loc.Y, 1, False); // True = complete house, False = house plan
  if (H <> nil) AND fRoadPlanner.Route_Make(Loc, H.PointBelowEntrance, aField) then
    Output := True;
  ReplaceOverlappingRoad(fPlannedHouses[aHT,aIdx].Loc);
  Result := Output;
end;


function TKMCityPlanner.GetRoadBetweenPoints(aStart, aEnd: TKMPoint; var aField: TKMPointList; var aFieldType: TFieldType): Boolean;
var
  Output: Boolean;
begin
  Output := False;
  aFieldType := ft_Road;
  if fRoadShortcutPlanner.Route_Make(aEnd, aStart, aField) then
    Output := True;
  Result := Output;
end;


function TKMCityPlanner.GetFieldToHouse(aHT: THouseType; aIdx: Integer; var aField: TKMPointList; var aFieldType: TFieldType): Boolean;
begin
  Result := True;
  aField.Clear;
  if (aHT = ht_Farm) then
  begin
    aFieldType := ft_Corn;
    PlanFarmFields(fPlannedHouses[aHT,aIdx].Loc, aField);
  end
  else if (aHT = ht_Wineyard) then
  begin
    aFieldType := ft_Wine;
    PlanWineFields(fPlannedHouses[aHT,aIdx].Loc, aField);
  end
  else
    Result := False;
end;


function TKMCityPlanner.GetTreesInHousePlanCnt(aHT: THouseType; aLoc: TKMPoint): Byte;
var
  Output: Byte;
  I,X,Y: Integer;
  HMA: THouseMappingArray;
begin
  Output := 0;
  HMA := gAIFields.Eye.HousesMapping;
  for I := Low(HMA[aHT].Tiles) to High(HMA[aHT].Tiles) do
  begin
    X := aLoc.X + HMA[aHT].Tiles[I].X;
    Y := aLoc.Y + HMA[aHT].Tiles[I].Y;
    if gTerrain.ObjectIsChopableTree(X, Y) then
      Output := Output + 1;
  end;
  Result := Output;
end;


function TKMCityPlanner.GetTreesInHousePlan(aHT: THouseType; aIdx: Integer; var aField: TKMPointList): Byte;
var
  I: Integer;
  Point: TKMPoint;
  HMA: THouseMappingArray;
begin
  aField.Clear;
  HMA := gAIFields.Eye.HousesMapping;
  for I := Low(HMA[aHT].Tiles) to High(HMA[aHT].Tiles) do
  begin
    Point := KMPointAdd(fPlannedHouses[aHT,aIdx].Loc, HMA[aHT].Tiles[I]);
    if gTerrain.ObjectIsChopableTree(Point.X, Point.Y) then
      aField.Add(Point);
  end;
  if (aField.Count > 0) then
  begin
    fPlannedHouses[aHT,aIdx].RemoveTreeInPlanProcedure := True;
    for I := Low(HMA[aHT].Tiles) to High(HMA[aHT].Tiles) do
    begin
      Point := KMPointAdd(fPlannedHouses[aHT,aIdx].Loc, HMA[aHT].Tiles[I]);
      if (gAIFields.Influences.AvoidBuilding[Point.Y, Point.X] = 0) then
        gAIFields.Influences.AvoidBuilding[Point.Y, Point.X] := 10;
    end;
  end;
  Result := aField.Count;
end;


function TKMCityPlanner.GetBlockingTrees(aHT: THouseType; aLoc: TKMPoint; var aTrees: TKMPointArray): Boolean;
var
  Output: Boolean;
  I,X,Y, TreeCnt: Integer;
  HMA: THouseMappingArray;
begin
  Result := True;
  if (aHT in [ht_IronMine, ht_GoldMine, ht_CoalMine]) then
    Exit;

  HMA := gAIFields.Eye.HousesMapping;
  SetLength(aTrees, Length(HMA[aHT].Tiles));
  for I := Low(aTrees) to High(aTrees) do
    aTrees[I] := KMPOINT_ZERO;

  Output := True;
  TreeCnt := 0;
  for I := Low(HMA[aHT].Tiles) to High(HMA[aHT].Tiles) do
  begin
    X := aLoc.X + HMA[aHT].Tiles[I].X;
    Y := aLoc.Y + HMA[aHT].Tiles[I].Y;
    Output := Output AND (tpBuild in gTerrain.Land[Y,X].Passability);
    if gTerrain.ObjectIsChopableTree(X, Y) then
    begin
      aTrees[TreeCnt] := KMPoint(X,Y);
      TreeCnt := TreeCnt + 1;
    end;
    if not Output then
      break;
  end;
  Result := Output;
end;


function TKMCityPlanner.GetBlockingFields(aHT: THouseType; aLoc: TKMPoint; var aFields: TKMPointArray): Boolean;
var
  I,X,Y, FieldCnt: Integer;
  HMA: THouseMappingArray;
begin
  Result := True;
  if (aHT in [ht_IronMine, ht_GoldMine, ht_CoalMine]) then
    Exit;

  HMA := gAIFields.Eye.HousesMapping;
  SetLength(aFields, Length(HMA[aHT].Tiles));
  for I := Low(aFields) to High(aFields) do
    aFields[I] := KMPOINT_ZERO;

  FieldCnt := 0;
  for I := Low(HMA[aHT].Tiles) to High(HMA[aHT].Tiles) do
  begin
    X := aLoc.X + HMA[aHT].Tiles[I].X;
    Y := aLoc.Y + HMA[aHT].Tiles[I].Y;
    aFields[FieldCnt] := KMPoint(X,Y);
    if     (gHands[fOwner].BuildList.FieldworksList.HasField(aFields[FieldCnt]) = ft_Wine)
        OR (gHands[fOwner].BuildList.FieldworksList.HasField(aFields[FieldCnt]) = ft_Corn) then
      FieldCnt := FieldCnt + 1;
  end;
  Result := (FieldCnt > 0);
end;


procedure TKMCityPlanner.PlanWineFields(aLoc: TKMPoint; var aNodeList: TKMPointList);
const
  MAX_VINE = 10;
var
  I,Dist: Integer;
  Dir: TDirection;
  HT: THouseType;
  FieldLoc: TKMPoint;
  HMA: THouseMappingArray;
begin
  HT := ht_Wineyard;
  HMA := gAIFields.Eye.HousesMapping;
  for Dist := 1 to 4 do
  begin
    for Dir := Low(HMA[HT].Surroundings[Dist]) to High(HMA[HT].Surroundings[Dist]) do
    for I := Low(HMA[HT].Surroundings[Dist,Dir]) to High(HMA[HT].Surroundings[Dist,Dir]) do
    begin
      FieldLoc := KMPointAdd(aLoc, HMA[HT].Surroundings[Dist,Dir,I]);
      if gTerrain.TileInMapCoords(FieldLoc.X, FieldLoc.Y)                         // Tile must be in map
        AND gHands[fOwner].CanAddFieldPlan(FieldLoc, ft_Wine)                     // Plan can be placed
        AND (gAIFields.Influences.AvoidBuilding[FieldLoc.Y, FieldLoc.X] = 0) then // Tile is not reserved
        aNodeList.Add(FieldLoc);
      if (aNodeList.Count >= MAX_VINE) then
        Exit;
    end;
  end;
end;



//{
procedure TKMCityPlanner.PlanFarmFields(aLoc: TKMPoint; var aNodeList: TKMPointList);
type
  TDirArrInt = array[TDirection] of Integer;
  TDirArrByte = array[TDirection] of Byte;
const
  MAX_FIELDS = 15;
  SNAP_TO_EDGE = 5;
  FIELD_PRICE = 1;
  DIR_PRICE: array[TDirection] of Byte = (5,15,20,15); //(dirN,dirE,dirS,dirW);
  AVOID_BUILDING_HOUSE_OUTSIDE_LOCK = 30;
  PRICE_ARR_CONST: TDirArrInt = (0,0,0,0);
  CNT_ARR_CONST: TDirArrByte = (0,0,0,0);
var
  I,Dist: Integer;
  Dir, BestDir: TDirection;
  HT: THouseType;
  FieldLoc: TKMPoint;
  HMA: THouseMappingArray;
  PriceArr: TDirArrInt;
  CntArr: TDirArrByte;
begin
  PriceArr := PRICE_ARR_CONST;
  CntArr := CNT_ARR_CONST;
  HT := ht_Farm;
  HMA := gAIFields.Eye.HousesMapping;
  // Get best edge of current loc (try build field in edges)
  Dist := 5;
  for Dir := Low(HMA[HT].Surroundings[Dist]) to High(HMA[HT].Surroundings[Dist]) do
    for I := Low(HMA[HT].Surroundings[Dist,Dir]) + Dist to High(HMA[HT].Surroundings[Dist,Dir]) - Dist + 1 do
    begin
      FieldLoc := KMPointAdd(aLoc, HMA[HT].Surroundings[Dist,Dir,I]);
      if not gTerrain.TileInMapCoords(FieldLoc.X, FieldLoc.Y)
        OR not gRes.Tileset.TileIsRoadable( gTerrain.Land[FieldLoc.Y,FieldLoc.X].Terrain ) then
        PriceArr[Dir] := PriceArr[Dir] + SNAP_TO_EDGE;
    end;
  // Get count of possible fields
  for Dist := 1 to 4 do
    for Dir := Low(HMA[HT].Surroundings[Dist]) to High(HMA[HT].Surroundings[Dist]) do
      for I := Low(HMA[HT].Surroundings[Dist,Dir]) + Dist to High(HMA[HT].Surroundings[Dist,Dir]) - Dist + 1 do
      begin
        FieldLoc := KMPointAdd(aLoc, HMA[HT].Surroundings[Dist,Dir,I]);
        if gTerrain.TileInMapCoords(FieldLoc.X, FieldLoc.Y)                          // Tile must be in map
          AND gHands[fOwner].CanAddFieldPlan(FieldLoc, ft_Corn)                      // Plan can be placed
          AND (gAIFields.Influences.AvoidBuilding[FieldLoc.Y, FieldLoc.X] <= AVOID_BUILDING_HOUSE_OUTSIDE_LOCK) then  // Tile is not reserved
          CntArr[Dir] := CntArr[Dir] + FIELD_PRICE;
      end;
  // Compute price of each direction
  for Dir := Low(PriceArr) to High(PriceArr) do
    PriceArr[Dir] := PriceArr[Dir] + CntArr[Dir] + DIR_PRICE[Dir];
  // Pic the best fields
  while (aNodeList.Count < MAX_FIELDS) do
  begin
    // Find best direction
    BestDir := Low(PriceArr);
    for Dir := Low(PriceArr) to High(PriceArr) do
      if (PriceArr[Dir] > PriceArr[BestDir]) then
        BestDir := Dir;
    // Anti-overload condition
    if (PriceArr[BestDir] = -1) then
      Break;
    PriceArr[BestDir] := -1;
    // Add best fields to aNodeList
    Dir := BestDir;
    for Dist := 1 to 4 do
    begin
      for I := Low(HMA[HT].Surroundings[Dist,Dir]) + Dist to High(HMA[HT].Surroundings[Dist,Dir]) - Dist + 1 do
      begin
        FieldLoc := KMPointAdd(aLoc, HMA[HT].Surroundings[Dist,Dir,I]);
        if gTerrain.TileInMapCoords(FieldLoc.X, FieldLoc.Y)
          AND gHands[fOwner].CanAddFieldPlan(FieldLoc, ft_Corn)
          AND (gAIFields.Influences.AvoidBuilding[FieldLoc.Y, FieldLoc.X] <= AVOID_BUILDING_HOUSE_OUTSIDE_LOCK) then
          aNodeList.Add(FieldLoc);
      end;
      if (aNodeList.Count > MAX_FIELDS) then
        Break;
    end;
  end;
end;
//}


{
procedure TKMCityPlanner.PlanFarmFields(aLoc: TKMPoint; var aNodeTagList: TKMPointTagList);
const
  NORTH_PENALIZATION = 10;
  MAX_FIELDS = 16;
  NOT_IN_AVOID_BUILDING = 5;
  NOT_IN_FIELD = 8;
  SUM_COEF = 1;
var
  I,K,X,Y,I2,K2,Dist: Integer;
  Price: Cardinal;
  MaxP, MinP, Point: TKMPoint;
  Weight: Cardinal;
  Dir: TDirection;
  HT: THouseType;
  FieldLoc: TKMPoint;
  HMA: THouseMappingArray;
  PriceArr: array of array of Word;
begin
  HT := ht_Farm;
  HMA := gAIFields.Eye.HousesMapping;

  MaxP := KMPoint(Min(aLoc.X + 2 + 4, gTerrain.MapX - 1), Min(aLoc.Y + 0 + 5, gTerrain.MapY - 1));
  MinP := KMPoint(Min(aLoc.X - 1 - 4, 1),                 Min(aLoc.Y - 2 - 4, 1));
  SetLength(PriceArr, MaxP.Y - MinP.Y, MaxP.X - MinP.X);
  for I := Low(PriceArr) to High(PriceArr) do
  begin
    Y := MinP.Y + I;
    for K := Low(PriceArr[I]) to High(PriceArr[I]) do
    begin
      X := MinP.X + K;
      Point := KMPoint(X,Y);
      if (gAIFields.Influences.AvoidBuilding[Y,X] > 0) OR not gHands[fOwner].CanAddFieldPlan(Point, ft_Corn) then
        PriceArr[I,K] := 0
      else
        PriceArr[I,K] := + KMDistanceAbs(Point, aLoc)
                         + NOT_IN_AVOID_BUILDING * Byte(not (tpBuild in gTerrain.Land[Y,X].Passability) )
                         + NOT_IN_FIELD * Byte(not gTerrain.TileIsCornField(Point))
                         + NORTH_PENALIZATION * Byte(Y < aLoc.Y - 2);
    end;
  end;
  for I := Low(PriceArr) to High(PriceArr) do
  begin
    Y := MinP.Y + I;
    for K := Low(PriceArr[I]) to High(PriceArr[I]) do
      if (PriceArr[I,K] > 0) then
      begin
        X := MinP.X + K;
        Price := 0;
        for I2 := Max(Low(PriceArr),I-SUM_COEF) to Min(High(PriceArr),I+SUM_COEF) do
        for K2 := Max(Low(PriceArr[I]),K-SUM_COEF) to Min(High(PriceArr[I]),K+SUM_COEF) do
          Price := Price + PriceArr[I2,K2];
        aNodeTagList.Add(KMPoint(X,Y), Price);
      end;
  end;
  aNodeTagList.SortByTag;
end;
//}

{
procedure TKMCityPlanner.PlanFarmFields(aLoc: TKMPoint; var aNodeTagList: TKMPointTagList);
const
  NORTH_PENALIZATION = 10;
  MAX_FIELDS = 16;
  NOT_IN_AVOID_BUILDING = 3;
  NOT_IN_FIELD = 5;
var
  I,K,X,Y,I2,K2,Dist: Integer;
  Price: Cardinal;
  MaxP, MinP, Point: TKMPoint;
  Weight: Cardinal;
  Dir: TDirection;
  HT: THouseType;
  FieldLoc: TKMPoint;
  HMA: THouseMappingArray;
  //PriceArr: array of array of Word;
  PriceArr: array[TDirection] of Integer;
begin
  HT := ht_Farm;
  HMA := gAIFields.Eye.HousesMapping;
  // Try find something to snap

  Dist := 5;
  for Dir := Low(HMA[HT].Surroundings[Dist]) to High(HMA[HT].Surroundings[Dist]) do
  begin
    PriceArr[Dir] := 500;
    for I := Low(HMA[HT].Surroundings[Dist,Dir]) to High(HMA[HT].Surroundings[Dist,Dir]) do
    begin
      FieldLoc := KMPointAdd(aLoc, HMA[HT].Surroundings[Dist,Dir,I]);
      if not (gTerrain.TileInMapCoords(FieldLoc.X, FieldLoc.Y) AND gHands[fOwner].CanAddFieldPlan(FieldLoc, ft_Corn)) then
        PriceArr[Dir] := PriceArr[Dir] - 5;
    end;
  end;
  PriceArr[dirN] := Max(0, PriceArr[dirN] + NORTH_PENALIZATION);
  // Find all possible places to build field
  for Dist := 1 to 4 do
  begin
    for Dir := Low(HMA[HT].Surroundings[Dist]) to High(HMA[HT].Surroundings[Dist]) do
    begin
      if (Dist = 1) AND (Dir = dirS) then // Don't plan fields 1 tile under farm plan
        continue;
      for I := Low(HMA[HT].Surroundings[Dist,Dir]) to High(HMA[HT].Surroundings[Dist,Dir]) do
      begin
        FieldLoc := KMPointAdd(aLoc, HMA[HT].Surroundings[Dist,Dir,I]);
        if gTerrain.TileInMapCoords(FieldLoc.X, FieldLoc.Y) AND gHands[fOwner].CanAddFieldPlan(FieldLoc, ft_Corn) then
        begin
          Weight := KMDistanceAbs(FieldLoc, aLoc) + PriceArr[Dir];
          aNodeTagList.Add(FieldLoc, Weight);
        end;
      end;
      //if (aNodeTagList.Count >= MAX_FIELDS) then
      //  break;
    end;
  end;

  aNodeTagList.SortByTag;
end;
//}



function TKMCityPlanner.FieldCrit(aHT: THouseType; aLoc: TKMPoint): Single;
const
  MIN_CORN_FIELDS = 15;
  MIN_WINE_FIELDS = 9;
  DECREASE_CRIT = - 10000;
var
  X,Y,I,Dist: Integer;
  Fields, Obstacles: Single;
  Dir: TDirection;
  HMA: THouseMappingArray;
begin
  HMA := gAIFields.Eye.HousesMapping;
  Fields := 0;
  Obstacles := 0;
  for Dist := 1 to (Byte(aHT = ht_Wineyard) shl 1) + (Byte(aHT = ht_Farm) * 5) do
  for Dir := Low(HMA[aHT].Surroundings[Dist]) to High(HMA[aHT].Surroundings[Dist]) do
  for I := Low(HMA[aHT].Surroundings[Dist,Dir]) + Dist to High(HMA[aHT].Surroundings[Dist,Dir]) - Dist + 1 do
  begin
    X := aLoc.X + HMA[aHT].Surroundings[Dist,Dir,I].X;
    Y := aLoc.Y + HMA[aHT].Surroundings[Dist,Dir,I].Y;
    if gTerrain.TileInMapCoords(X,Y) then
    begin
      if gHands[fOwner].CanAddFieldPlan(KMPoint(X,Y), ft_Corn) then
        Fields := Fields + 1;
      if not gRes.Tileset.TileIsRoadable( gTerrain.Land[Y,X].Terrain ) then
        Obstacles := Obstacles + 1;
    end;
  end;
  Result := Byte(Fields < (Byte(aHT = ht_Wineyard)*MIN_WINE_FIELDS + Byte(aHT = ht_Farm)*MIN_CORN_FIELDS )) * DECREASE_CRIT + Obstacles;
end;


function TKMCityPlanner.SnapCrit(aHT: THouseType; aLoc: TKMPoint): Single;
  function IsPlan(aPoint: TKMPoint; aLock: TTileLock; aField: TFieldType): Boolean;
  begin
    Result := (gHands[fOwner].BuildList.FieldworksList.HasField(aPoint) = aField)
              OR (gTerrain.Land[aPoint.Y, aPoint.X].TileLock = aLock);
  end;
  function IsRoad(aPoint: TKMPoint): Boolean;
  begin
    Result := gTerrain.TileIsWalkableRoad(aPoint) OR IsPlan(aPoint, tlRoadWork, ft_Road);
  end;
  function IsCornField(aPoint: TKMPoint): Boolean;
  begin
    Result := gTerrain.TileIsCornField(aPoint) OR IsPlan(aPoint, tlFieldWork, ft_Corn);
  end;
  function IsWineField(aPoint: TKMPoint): Boolean;
  begin
    Result := gTerrain.TileIsWineField(aPoint) OR IsPlan(aPoint, tlFieldWork, ft_Wine);
  end;
  function IsNearHouse(aPoint: TKMPoint): Boolean;
  begin
    Result := not (tpBuild in gTerrain.Land[aPoint.Y,aPoint.X].Passability);
  end;
  function IsAvoidBuilding(aPoint:TKMPoint): Boolean;
  begin
    Result := gAIFields.Influences.AvoidBuilding[aPoint.Y, aPoint.X] > 0;
  end;
var
  I,Dist: Integer;
  Output: Single;
  Point: TKMPoint;
  Dir: TDirection;
  HMA: THouseMappingArray;
begin
  Output := 0;
  Dist := 1;
  HMA := gAIFields.Eye.HousesMapping;
  for Dir := Low(HMA[aHT].Surroundings[Dist]) to High(HMA[aHT].Surroundings[Dist]) do
  for I := Low(HMA[aHT].Surroundings[Dist,Dir]) to High(HMA[aHT].Surroundings[Dist,Dir]) do
  begin
    Point := KMPoint(aLoc.X + HMA[aHT].Surroundings[Dist,Dir,I].X, aLoc.Y + HMA[aHT].Surroundings[Dist,Dir,I].Y);
    Output := Output
              + Byte(IsNearHouse(Point)) * GA_PLANNER_SnapCrit_SnapToHouse
              + Byte(IsAvoidBuilding(Point) OR IsRoad(Point)) * GA_PLANNER_SnapCrit_SnapToFields; // OR IsCornField(Point) OR IsWineField(Point)
  end;
  Result := Output;
end;


function TKMCityPlanner.DistCrit(aHT: THouseType; aLoc: TKMPoint): Single;
const
  MAX_BID = 1000000;

  function DistFromHouses(aHTs: array of THouseType): Single;
  var
    I,K: Integer;
    Output: Single;
  begin
    Output := 0;
    for I := Low(aHTs) to High(aHTs) do
    for K := Low(fPlannedHouses[ aHTs[I] ]) to Min(2, High(fPlannedHouses[ aHTs[I] ])) do
      Output := Output + KMDistanceAbs(aLoc, fPlannedHouses[ aHTs[I],K ].Loc);
    Result := Output;
  end;

  function DistFromHouse(aHTs: array of THouseType): Single;
  var
    I,K: Integer;
    Output, Bid: Single;
  begin
    Output := MAX_BID;
    for I := Low(aHTs) to High(aHTs) do
    for K := Low(fPlannedHouses[ aHTs[I] ]) to Min(2, High(fPlannedHouses[ aHTs[I] ])) do
    begin
      Bid := KMDistanceAbs(aLoc, fPlannedHouses[ aHTs[I],K ].Loc);
      if (Bid < Output) then
        Output := Bid;
    end;
    if (Output = MAX_BID) then
      Output := 0;
    Result := Output;
  end;

var
  Output: Single;
begin
  case aHT of
    ht_Store:          Output := + GA_PLANNER_DistCrit_Store * DistFromHouse([ht_Store]);
    ht_School:         Output := - GA_PLANNER_DistCrit_School * DistFromHouse([ht_Store,ht_Metallurgists]);
    ht_Inn:            Output := - GA_PLANNER_DistCrit_Inn_Store * DistFromHouse([ht_Store]) + GA_PLANNER_DistCrit_Inn_Inn * DistFromHouse([ht_Inn]);
    ht_Marketplace:    Output := - GA_PLANNER_DistCrit_Marketplace * DistFromHouse([ht_Store]);

    ht_IronSmithy:     Output := - GA_PLANNER_DistCrit_IronSmithy_Self * DistFromHouse([ht_IronSmithy]) - GA_PLANNER_DistCrit_IronSmithy_Res * DistFromHouse([ht_CoalMine, ht_IronMine]);
    ht_ArmorSmithy:    Output := - GA_PLANNER_DistCrit_ArmorSmithy_Set * DistFromHouse([ht_IronSmithy]) - GA_PLANNER_DistCrit_ArmorSmithy_Res * DistFromHouse([ht_CoalMine, ht_IronMine]);
    ht_WeaponSmithy:   Output := - GA_PLANNER_DistCrit_WeaponSmithy_Set * DistFromHouse([ht_IronSmithy]) - GA_PLANNER_DistCrit_WeaponSmithy_Res * DistFromHouse([ht_CoalMine, ht_IronMine]);
    ht_Tannery:        Output := - GA_PLANNER_DistCrit_Tannery_Set * DistFromHouse([ht_Swine, ht_WeaponWorkshop]);
    ht_ArmorWorkshop:  Output := - GA_PLANNER_DistCrit_ArmorWorkshop_Set * DistFromHouse([ht_Sawmill, ht_Barracks]);
    ht_WeaponWorkshop: Output := - GA_PLANNER_DistCrit_WeaponWorkshop_Set * DistFromHouse([ht_Tannery, ht_Barracks]);
    ht_Barracks:       Output := - GA_PLANNER_DistCrit_Barracks_Set * DistFromHouses([ht_ArmorSmithy, ht_ArmorWorkshop, ht_WeaponSmithy, ht_WeaponWorkshop]);

    ht_Bakery:         Output := - GA_PLANNER_DistCrit_Bakery_Set * DistFromHouses([ht_Store, ht_Inn, ht_Mill]) + GA_PLANNER_DistCrit_Bakery_Res * DistFromHouse([ht_IronMine, ht_GoldMine]);
    ht_Butchers:       Output := - GA_PLANNER_DistCrit_Butchers_Set * DistFromHouses([ht_Store, ht_Inn, ht_Swine]) + GA_PLANNER_DistCrit_Butchers_Res * DistFromHouse([ht_IronMine, ht_GoldMine]);
    ht_Mill:           Output := - GA_PLANNER_DistCrit_Mill_Set * DistFromHouses([ht_Farm, ht_Bakery]) + GA_PLANNER_DistCrit_Mill_Res * DistFromHouse([ht_IronMine, ht_GoldMine]);
    ht_Swine:          Output := - GA_PLANNER_DistCrit_Swine_Set * DistFromHouses([ht_Farm]) + GA_PLANNER_DistCrit_Swine_Res * DistFromHouse([ht_IronMine, ht_GoldMine]);
    ht_Stables:        Output := - GA_PLANNER_DistCrit_Stables_Set * DistFromHouses([ht_Farm]) + GA_PLANNER_DistCrit_Stables_Res * DistFromHouse([ht_IronMine, ht_GoldMine]);
    ht_Farm:           Output := - GA_PLANNER_DistCrit_Farm_Set * DistFromHouse([ht_Farm]) + GA_PLANNER_DistCrit_Farm_Res * DistFromHouse([ht_IronMine, ht_GoldMine]);
    ht_Wineyard:       Output := - GA_PLANNER_DistCrit_Wineyard_Set * DistFromHouse([ht_Inn]) + GA_PLANNER_DistCrit_Wineyard_Res * DistFromHouse([ht_IronMine, ht_GoldMine]);

    ht_Metallurgists:  Output := - GA_PLANNER_DistCrit_Metallurgists_Set * DistFromHouse([ht_School, ht_Store, ht_Metallurgists]) - GA_PLANNER_DistCrit_Metallurgists_Res * DistFromHouse([ht_GoldMine]);
    ht_GoldMine:       Output := - GA_PLANNER_DistCrit_GoldMine_Set * DistFromHouse([ht_Metallurgists]);
    ht_CoalMine:       Output := - GA_PLANNER_DistCrit_CoalMine_Set * DistFromHouse([ht_Metallurgists, ht_IronSmithy, ht_ArmorSmithy, ht_ArmorWorkshop]);
    ht_IronMine:       Output := - GA_PLANNER_DistCrit_IronMine_Set * DistFromHouse([ht_IronSmithy]);

    ht_Quary:          Output := - GA_PLANNER_DistCrit_Quary_Set * DistFromHouse([ht_Store]);
    ht_Woodcutters:    Output := - GA_PLANNER_DistCrit_Woodcutters_Set * DistFromHouse([ht_Store]); // maybe ownership for first woodcutters? gAIFields.Influences.Ownership[fOwner, Mines.Items[I].Y, Mines.Items[I].X]
    ht_Sawmill:        Output := - GA_PLANNER_DistCrit_Sawmill_Set * DistFromHouse([ht_Woodcutters, ht_WeaponWorkshop]);
    else
      Output := 0;
  end;
  Result := Output - GA_PLANNER_DistCrit_CenterStore * DistFromHouse([ht_Store]);
end;


function TKMCityPlanner.FindPlaceForHouse(aUnlockProcedure: Boolean; aHT: THouseType; out aBestLocs: TKMPointArray): Byte;
const
  BEST_PLANS_CNT = 5;
  INIT_BEST_BID = -1000000;
var
  I,K,L,Dist: Integer;
  Dir: TDirection;
  HType: THouseType;
  Loc: TKMPoint;
  Bid, POMBid: Single;
  HMA: THouseMappingArray;
  BestBidArr: array[0..BEST_PLANS_CNT-1] of Single;
begin
  SetLength(aBestLocs, BEST_PLANS_CNT);
  for I := Low(BestBidArr) to High(BestBidArr) do
    BestBidArr[I] := INIT_BEST_BID;

  HMA := gAIFields.Eye.HousesMapping;
  for HType := HOUSE_MIN to HOUSE_MAX do
  for I := Length(fPlannedHouses[HType])-1 downto 0 do
  begin
    for Dist := 2 to MAX_SCAN_DIST_FROM_HOUSE do
    begin
      for Dir := Low(HMA[HType].Surroundings[Dist]) to High(HMA[HType].Surroundings[Dist]) do
      for K := Low(HMA[HType].Surroundings[Dist,Dir]) to High(HMA[HType].Surroundings[Dist,Dir]) do
      begin
        Loc := KMPointAdd(fPlannedHouses[HType,I].Loc, HMA[HType].Surroundings[Dist,Dir,K], HMA[aHT].MoveToEntrance[Dir]);
        if gAIFields.Eye.CanAddHousePlan(Loc, aHT, False, not aUnlockProcedure) then
        begin
          Bid := + SnapCrit(aHT, Loc) * GA_PLANNER_FindPlaceForHouse_SnapCrit
                 + DistCrit(aHT, Loc) * GA_PLANNER_FindPlaceForHouse_DistCrit
                 - GetTreesInHousePlanCnt(aHT, Loc) * GA_PLANNER_FindPlaceForHouse_TreeInPlan
                 //+ Abs(fPlannedHouses[HType,I].Loc.Y - Loc.Y) * 3 // Prefer to build houses on left / right side
                 //+ Abs(fPlannedHouses[HType,I].Loc.X - Loc.X) * 2
                 - gAIFields.Influences.OtherOwnerships(fOwner,Loc.X,Loc.Y) * ManTune_PLANNER_FindPlaceForHouse_Influence;
          if (aHT = ht_Farm) OR (aHT = ht_Wineyard) then
            Bid := Bid + FieldCrit(aHT, Loc) * GA_PLANNER_FindPlaceForHouse_FarmCrit;
          for L := Low(BestBidArr) to High(BestBidArr) do
            if KMSamePoint(Loc, aBestLocs[L]) then
              break
            else if (Bid > BestBidArr[L]) then
            begin
              KMSwapPoints(Loc, aBestLocs[L]);
              POMBid := BestBidArr[L];
              BestBidArr[L] := Bid;
              Bid := POMBid;
            end;
        end;
      end;
      if (Dist > 2) AND (BestBidArr[0] <> INIT_BEST_BID) then
        break;
    end;
  end;
  for I := High(BestBidArr) downto Low(BestBidArr) do
    if (BestBidArr[I] <> INIT_BEST_BID) then
      break;
  Result := I;
end;


function TKMCityPlanner.FindPlaceForMines(aHT: THouseType): Boolean;
const
  MAX_LOCS = 5;

  // Get closest mine
  function FindPlaceForMine(aMine: THouseType): Boolean;
  var
    Output: Boolean;
    Loc: TKMPoint;
    Bid, BestBid: Single;
  begin
    Output := gAIFields.Eye.GetMineLoc(aMine, Loc);
    if Output then
      AddPlan(aHT, Loc);
    Result := Output
  end;

  // Determine whether are coal tiles under coal mine plan in aCoalLoc
  function IsCoalUnderPlan(aCoalLoc: TKMPoint): Boolean;
  var
    Output: Boolean;
    X,Y,I: Integer;
    HMA: THouseMappingArray;
  begin
    Output := True;
    HMA := gAIFields.Eye.HousesMapping;
    for I := Low(HMA[ht_CoalMine].Tiles) to High(HMA[ht_CoalMine].Tiles) do
    begin
      X := aCoalLoc.X + HMA[ht_CoalMine].Tiles[I].X;
      Y := aCoalLoc.Y + HMA[ht_CoalMine].Tiles[I].Y;
      if not (gTerrain.TileIsCoal(X, Y) > 1) then
      begin
        Output := False;
        break;
      end;
    end;
    Result := Output;
  end;

  // Coal mine planner
  function FindPlaceForCoalMine(): Boolean;
  var
    Output: Boolean;
    I: Integer;
    Locs: TKMPointTagList;
  begin
    Output := False;

    Locs := gAIFields.Eye.GetCoalLocs();
    try
      if (Locs.Count > 0) then
      begin
        Locs.SortByTag();
        for I := Locs.Count-1 downto 0 do
          if gAIFields.Eye.CanAddHousePlan(Locs.Items[I], ht_CoalMine, True, False) AND IsCoalUnderPlan(Locs.Items[I]) then
          begin
            AddPlan(aHT, Locs.Items[I]);
            Output := True;
            break;
          end;
      end;
    finally
      Locs.Free;
    end;
    Result := Output;
  end;

  function DistFromClosestQuarry(aLoc: TKMPoint): Integer;
  var
    I,Dist,Output: Integer;
  begin
    Output := High(Integer);
    for I := Low(fPlannedHouses[ht_Quary]) to High(fPlannedHouses[ht_Quary]) do
    begin
      Dist := KMDistanceAbs(aLoc, fPlannedHouses[ht_Quary,I].Loc);
      if (Dist < Output) then
        Output := Dist;
    end;
    if (Output = High(Integer)) then
      Output := 0;
    Result := Output;
  end;

  // Quarry planner
  function FindPlaceForQuary(): Boolean;
  const
    OVERFLOW = 10;
    OWNER_PENALIZATION = 50;
    SCAN_RAD = 5;
    USED_TILES_PER_A_MINE = 5;
    USED_TILE_PRICE = 5;
  var
    X,Y,I: Integer;
    Bid, BestBid: Single;
    Loc, BestLoc: TKMPoint;
    Output: Boolean;
    //BidArr: array [0..MAX_LOCS-1] of Integer;
    Locs: TKMPointTagList;
  begin
    Output := False;
    Locs := gAIFields.Eye.GetStoneLocs();
    try
      // Calculate criterium = ownership + actual Tag2 (= used by other miners) + owner criterium
      for I := 0 to Locs.Count - 1 do
        Locs.Tag[I] := Max(0, + gAIFields.Influences.OtherOwnerships(fOwner,Locs.Items[I].X,Locs.Items[I].Y)
                              - Round(DistCrit(ht_Quary, Locs.Items[I])) );
      Locs.SortByTag();
      // Find best loc for a Quary
      BestBid := -100000;
      for I := 0 to Locs.Count - 1 do
      begin
        for Y := Max(1,Locs.Items[I].Y-SCAN_RAD) to Min(Locs.Items[I].Y+SCAN_RAD,gTerrain.MapY-1) do
        for X := Max(1,Locs.Items[I].X-SCAN_RAD) to Min(Locs.Items[I].X+SCAN_RAD,gTerrain.MapX-1) do
        begin
          Loc := KMPoint(X,Y);
          if gAIFields.Eye.CanAddHousePlan(Loc, ht_Quary, False, False) then
          begin
            Bid := + DistCrit(ht_Quary, Loc) * GA_PLANNER_FindPlaceForQuary_DistCrit
                   + SnapCrit(ht_Quary, Loc) * GA_PLANNER_FindPlaceForQuary_SnapCrit;
            if (Bid > BestBid) then
            begin
              BestBid := Bid;
              BestLoc := Loc;
              Output := True;
            end;
          end;
        end;
        if Output AND (I > min(5,Locs.Count-1)) then // Do several points
        begin
          AddPlan(aHT, BestLoc);
          break;
        end;
      end;
    finally
      Locs.Free;
    end;
    Result := Output;
  end;

var
  Output: Boolean;
begin
  case aHT of
    ht_GoldMine:  Output := FindPlaceForMine(ht_GoldMine);
    ht_IronMine:  Output := FindPlaceForMine(ht_IronMine);
    ht_CoalMine:  Output := FindPlaceForCoalMine();
    ht_Quary:     Output := FindPlaceForQuary();
    else          Output := False;
  end;
  Result := Output;
end;


function TKMCityPlanner.FindPlaceForWoodcutter(): Boolean;

  // Find place for woodcutter
  function PlaceWoodcutter(aCenter: TKMPoint): Boolean;
  const
    RADIUS = 5;
    MIN_BID = -100000;
  var
    Output: Boolean;
    X,Y: Integer;
    Bid, BestBid: Single;
    Loc, BestLoc: TKMPoint;
  begin
    Output := False;
    BestBid := MIN_BID;
    for Y := Max(1, aCenter.Y - RADIUS) to Min(gTerrain.MapY, aCenter.Y + RADIUS) do
    for X := Max(1, aCenter.X - RADIUS) to Min(gTerrain.MapX, aCenter.X + RADIUS) do
    begin
      Loc := KMPoint(X,Y);
      if gAIFields.Eye.CanAddHousePlan(Loc, ht_Woodcutters, True, False) then
      begin
        Bid := - GA_PLANNER_FindPlaceForWoodcutter_DistFromForest * KMDistanceAbs(aCenter, Loc)
               + DistCrit(ht_Woodcutters, Loc)
               + SnapCrit(ht_Woodcutters, Loc);
        if (Bid > BestBid) then
        begin
          BestBid := Bid;
          BestLoc := Loc;
        end;
      end;
    end;
    if (BestBid <> MIN_BID) then
    begin
      Output := True;
      // Check whether is cutting point (center of forest) inside of house plan and in this case set it to zero point
      if ((aCenter.Y <= BestLoc.Y) AND (aCenter.Y >= BestLoc.Y+1)) AND ((aCenter.X <= BestLoc.X) AND (aCenter.X >= BestLoc.X-2)) then
        aCenter := KMPOINT_ZERO;
      AddPlan(ht_Woodcutters, BestLoc, aCenter);
    end;
    Result := Output;
  end;

  function GetPotentialTreeTiles(aLoc: TKMPoint): Integer;
  const
    RADIUS = 3;
  var
    X,Y,Output: Integer;
  begin
    Output := 0;
    for Y := Max(1,aLoc.Y-RADIUS) to Min(gTerrain.MapY-1, aLoc.Y+RADIUS) do
    for X := Max(1,aLoc.X-RADIUS) to Min(gTerrain.MapX-1, aLoc.X+RADIUS) do
      if gTerrain.TileGoodForTree(X, Y) then
        Output := Output + 1;
    Result := Output;
  end;

  function FindPlansAround(aLoc: TKMPoint): Single;
  const
    RADIUS = 4;
  var
    X,Y: Integer;
    Output: Single;
  begin
    Output := 0;
    for Y := Max(1,aLoc.Y-RADIUS) to Min(gTerrain.MapY-1,aLoc.Y+RADIUS) do
    for X := Max(1,aLoc.X-RADIUS) to Min(gTerrain.MapX-1,aLoc.X+RADIUS) do
      if not (tpBuild in gTerrain.Land[Y,X].Passability) then
        Output := Output + 1;
    Result := Output;
  end;

  function GetEdgeTiles(aLoc: TKMPoint): Single;
  const
    RADIUS = 5;
  var
    X,Y,maxL,minL: Integer;
    Output: Single;
  begin
    Output := 0;
    minL := aLoc.Y-RADIUS;
    maxL := aLoc.Y+RADIUS;
    for X := Max(1,aLoc.X-RADIUS) to Min(gTerrain.MapX-1,aLoc.X+RADIUS) do
    begin
      if (minL < 1) OR not gRes.Tileset.TileIsRoadable( gTerrain.Land[minL,X].Terrain ) then
        Output := Output + 1;
      if (maxL > gTerrain.MapY-1) OR not gRes.Tileset.TileIsRoadable( gTerrain.Land[maxL,X].Terrain ) then
        Output := Output + 1;
    end;
    minL := aLoc.X-RADIUS;
    maxL := aLoc.X+RADIUS;
    for Y := Max(1,aLoc.Y-RADIUS) to Min(gTerrain.MapY-1,aLoc.Y+RADIUS) do
    begin
      if (minL < 1) OR not gRes.Tileset.TileIsRoadable( gTerrain.Land[Y,minL].Terrain ) then
        Output := Output + 1;
      if (maxL > gTerrain.MapX-1) OR not gRes.Tileset.TileIsRoadable( gTerrain.Land[Y,maxL].Terrain ) then
        Output := Output + 1;
    end;
    Result := Output;
  end;

const
  INC_USAGE = 150;
  BLOCK_RAD = 7.5;
var
  Output: Boolean;
  I: Integer;
  Forests: TKMPointTagList;
begin
  Output := False;
  Forests := gAIFields.Eye.GetForests();
  try
    for I := Forests.Count-1 downto 0 do
    begin
      if (gAIFields.Influences.AvoidBuilding[  Forests.Items[I].Y, Forests.Items[I].X  ] < 250) then
        Forests.Tag[I] := Max(0, Round(
                            + Forests.Tag[I] * GA_PLANNER_FindPlaceForWoodcutter_TreeCnt
                            + Forests.Tag2[I] * GA_PLANNER_FindPlaceForWoodcutter_DistCrit
                            + GetPotentialTreeTiles(Forests.Items[I]) * GA_PLANNER_FindPlaceForWoodcutter_CanPlaceTreeCnt
                            + GetEdgeTiles(Forests.Items[I]) * GA_PLANNER_FindPlaceForWoodcutter_SnapToEdge
                            - FindPlansAround(Forests.Items[I]) * GA_PLANNER_FindPlaceForWoodcutter_PlansAround
                            - gAIFields.Influences.OtherOwnerships(fOwner, Forests.Items[I].X, Forests.Items[I].Y) * ManTune_PLANNER_FindPlaceForWoodcutter_Influence
                          ))
      else
        Forests.Delete(I);
    end;

    Forests.SortByTag();
    I := Forests.Count;
    while not Output AND (I > 0) do
    begin
      I := I - 1;
      Output := PlaceWoodcutter(Forests.Items[I]);; // Get the best forest
      if Output then
        gAIFields.Influences.AddAvoidBuilding(Forests.Items[I].X, Forests.Items[I].Y, BLOCK_RAD, INC_USAGE);
    end;
  finally
    Forests.Free;
  end;
  Result := Output;
end;


procedure TKMCityPlanner.Paint();
const
  COLOR_WHITE = $80FFFFFF;
  COLOR_BLACK = $80000000;
  COLOR_GREEN = $6000FF00;
  COLOR_RED = $800000FF;
  COLOR_YELLOW = $8000FFFF;
  COLOR_GREEN_Field = $4400FF00;
  COLOR_GREEN_Wine = $3355FFFF;
  COLOR_BLUE = $60FF0000;
var
  I,K: Integer;
  HT: THouseType;
  Loc: TKMPoint;
  Color: Cardinal;
  HMA: THouseMappingArray;
begin
  HMA := gAIFields.Eye.HousesMapping;
  for HT := HOUSE_MIN to HOUSE_MAX do
  begin
    case HT of
      ht_Store,ht_School,ht_Inn,ht_Marketplace: Color := COLOR_BLACK;
      ht_Quary,ht_Woodcutters,ht_Sawmill: Color := COLOR_BLUE;
      ht_GoldMine,ht_CoalMine,ht_IronMine,ht_Metallurgists: Color := COLOR_YELLOW;
      ht_IronSmithy,ht_ArmorSmithy,ht_WeaponSmithy,ht_Tannery,ht_ArmorWorkshop,ht_WeaponWorkshop,ht_Barracks: Color := COLOR_RED;
      ht_Bakery,ht_Butchers,ht_Mill,ht_Swine,ht_Stables,ht_Farm,ht_Wineyard: Color := COLOR_GREEN;
      else Color := COLOR_WHITE;
    end;
    for I := 0 to Length(fPlannedHouses[HT])-1 do
    begin
      for K := 0 to Length(HMA[HT].Tiles)-1 do
      begin
        Loc := KMPointAdd(fPlannedHouses[HT,I].Loc, HMA[HT].Tiles[K]);
        gRenderAux.Quad(Loc.X, Loc.Y, Color);
      end;
    end;
  end;
end;







{ TPathFindingCityPlanner }
function TPathFindingCityPlanner.MovementCost(aFromX, aFromY, aToX, aToY: Word): Word;
var
  IsRoad: Boolean;
begin
  Result := 0;
  IsRoad := (gAIFields.Influences.AvoidBuilding[aToY, aToX] = AVOID_BUILDING_NODE_LOCK_ROAD)     // Reserved road plan
            OR (tpWalkRoad in gTerrain.Land[aToY, aToX].Passability)                             // Completed road
            OR (gHands[fOwner].BuildList.FieldworksList.HasField(KMPoint(aToX, aToY)) = ft_Road) // Placed road plan
            OR (gTerrain.Land[aToY, aToX].TileLock = tlRoadWork);                                // Road under construction

  if not IsRoad then
  begin
    // Snap to no-build areas (1 tile from house)
    if (tpBuild in gTerrain.Land[aToY,aToX].Passability) then
      Inc(Result, 11);
    //Building roads over fields is discouraged unless unavoidable
    if (gAIFields.Influences.AvoidBuilding[aToY, aToX] > 0) then // Forest / wine field / corn field
      Inc(Result, 30);
  end;
end;


{ TPathFindingShortcutsCityPlanner }
function TPathFindingShortcutsCityPlanner.MovementCost(aFromX, aFromY, aToX, aToY: Word): Word;
begin
  Result := 10 + inherited MovementCost(aFromX, aFromY, aToX, aToY);
end;


function TPathFindingShortcutsCityPlanner.DestinationReached(aX, aY: Word): Boolean;
begin
  Result := ((aX = fLocB.X) and (aY = fLocB.Y)); //We reached destination point
end;



end.

