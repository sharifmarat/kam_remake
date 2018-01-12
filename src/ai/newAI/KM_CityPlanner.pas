unit KM_CityPlanner;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, KromUtils, Math, SysUtils,
  KM_Defaults, KM_Points, KM_CommonClasses, KM_CommonTypes,
  KM_TerrainFinder, KM_PerfLog, KM_Houses, KM_ResHouses, KM_ResWares,
  KM_PathFindingRoad, KM_CityPredictor,
  KM_AIInfluences, KM_NavMeshDefences;


var

  ManTune_PLANNER_FindPlaceForHouse_Influence       : Single = 40;
  ManTune_PLANNER_FindPlaceForWoodcutter_Influence  : Single = 20;
  {
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
  //{
  GA_PLANNER_SnapCrit_SnapToHouse                   : Single = 40.35658836;
  GA_PLANNER_SnapCrit_SnapToFields                  : Single = 20;
  GA_PLANNER_SnapCrit_SnapToRoads                   : Single = 30;
  GA_PLANNER_FindPlaceForHouse_SnapCrit             : Single = 17.10942268;
  GA_PLANNER_FindPlaceForHouse_DistCrit             : Single = 13.00957012;
  GA_PLANNER_FindPlaceForHouse_TreeInPlan           : Single = 100.50516129;
  GA_PLANNER_FindPlaceForHouse_FarmCrit             : Single = 21.58874512;
  GA_PLANNER_FindPlaceForWoodcutter_DistFromForest  : Single = 28.76280212;
  GA_PLANNER_FindPlaceForWoodcutter_DistCrit        : Single = 90.20793533;
  GA_PLANNER_FindPlaceForWoodcutter_TreeCnt         : Single = 5.1000000015;
  GA_PLANNER_FindPlaceForWoodcutter_SnapToEdge      : Single = 5.223324299;
  GA_PLANNER_FindPlaceForWoodcutter_CanPlaceTreeCnt : Single = 30.30228996;
  GA_PLANNER_FindPlaceForWoodcutter_PlansAround     : Single = 4.992388725;
  //}

type
  THousePlan = record
    Placed, ShortcutsCompleted, RemoveTreeInPlanProcedure, HouseReservation: Boolean;  // EDIT
    UID: Integer;
    Loc, SpecPoint: TKMPoint;
  end;
  THousePlanArray = record
    Count: Word;
    Plans: array of THousePlan;
  end;
  TPlannedHousesArray = array [HOUSE_MIN..HOUSE_MAX] of THousePlanArray;


  TPathFindingCityPlanner = class(TPathFindingRoad)
  private
  protected
    function IsWalkableTile(aX, aY: Word): Boolean; override;
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
    fDefenceTowersPlanned: Boolean;
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
    function PlanDefenceTowers(): Boolean;
  public
    constructor Create(aPlayer: TKMHandIndex);
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure AfterMissionInit();
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    procedure UpdateState(aTick: Cardinal);

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

    procedure Paint();
  end;


const
  HOUSE_DEPENDENCE: array[HOUSE_MIN..HOUSE_MAX] of set of THouseType = (  // This array is sorted by priority
    {ht_ArmorSmithy}    [ ht_IronSmithy,     ht_CoalMine,       ht_Barracks,       ht_ArmorSmithy    ],
    {ht_ArmorWorkshop}  [ ht_Tannery,        ht_Barracks,       ht_Sawmill,        ht_ArmorWorkshop  ],
    {ht_Bakery}         [ ht_Inn,            ht_Mill,           ht_Store,          ht_Bakery         ],
    {ht_Barracks}       [ ht_ArmorWorkshop,  ht_ArmorSmithy,    ht_WeaponSmithy,   ht_WeaponWorkshop ],
    {ht_Butchers}       [ ht_Inn,            ht_Swine,          ht_Store,          ht_Butchers       ],
    {ht_CoalMine}       [ ht_Store                                                                   ],
    {ht_Farm}           [ ht_Farm,           ht_Swine,          ht_Mill,           ht_Stables        ],
    {ht_FisherHut}      [ ht_Store                                                                   ],
    {ht_GoldMine}       [ ht_Store                                                                   ],
    {ht_Inn}            [ ht_Butchers,       ht_Bakery,         ht_Store,          ht_Wineyard       ],
    {ht_IronMine}       [ ht_Store                                                                   ],
    {ht_IronSmithy}     [ ht_CoalMine,       ht_IronMine,       ht_WeaponSmithy,   ht_IronSmithy     ],
    {ht_Marketplace}    [ ht_Store,          ht_Metallurgists,  ht_Barracks,       ht_Marketplace    ],
    {ht_Metallurgists}  [ ht_GoldMine,       ht_CoalMine,       ht_School,         ht_Store          ],
    {ht_Mill}           [ ht_Bakery,         ht_Inn,            ht_Mill,           ht_Farm           ],
    {ht_Quary}          [ ht_Store                                                                   ],
    {ht_Sawmill}        [ ht_ArmorWorkshop,  ht_Sawmill,        ht_WeaponWorkshop                    ],
    {ht_School}         [ ht_Metallurgists,  ht_Store,          ht_Barracks,       ht_School         ],
    {ht_SiegeWorkshop}  [ ht_IronSmithy,     ht_Sawmill,        ht_Store,          ht_SiegeWorkshop  ],
    {ht_Stables}        [ ht_Farm,           ht_Barracks,       ht_Stables                           ],
    {ht_Store}          [ ht_Inn,            ht_Barracks,       ht_School                            ],
    {ht_Swine}          [ ht_Farm,           ht_Butchers,       ht_Swine                             ],
    {ht_Tannery}        [ ht_ArmorWorkshop,  ht_Tannery,        ht_Barracks                          ],
    {ht_TownHall}       [ ht_Metallurgists,  ht_Store,          ht_TownHall                          ],
    {ht_WatchTower}     [ ht_Store                                                                   ],
    {ht_WeaponSmithy}   [ ht_IronSmithy,     ht_CoalMine,       ht_Barracks,       ht_WeaponSmithy   ],
    {ht_WeaponWorkshop} [ ht_Sawmill,        ht_Barracks,       ht_WeaponWorkshop                    ],
    {ht_Wineyard}       [ ht_Inn,            ht_Quary                                                ],
    {ht_Woodcutters}    [ ht_Store                                                                   ]
  );

implementation
uses
  KM_Game, KM_HouseCollection, KM_HouseSchool, KM_HandsCollection, KM_Hand, KM_Terrain, KM_Resource,
  KM_AIFields, KM_Units, KM_UnitTaskDelivery, KM_UnitActionWalkTo, KM_UnitTaskGoEat, KM_UnitsCollection,
  KM_NavMesh, KM_HouseMarket, KM_HouseWoodcutters, KM_CommonUtils, KM_Eye,
  KM_RenderAux;






{ TKMCityPlanner }
constructor TKMCityPlanner.Create(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
  fDefenceTowersPlanned := False;
  fRoadPlanner := TPathFindingCityPlanner.Create(fOwner);
  fRoadShortcutPlanner := TPathFindingShortcutsCityPlanner.Create(fOwner);
end;


destructor TKMCityPlanner.Destroy();
begin
  fRoadPlanner.Free;
  fRoadShortcutPlanner.Free;
  inherited;
end;


procedure TKMCityPlanner.Save(SaveStream: TKMemoryStream);
var
  HT: THouseType;
  I, Len: Integer;
begin
  SaveStream.WriteA('CityPlanner');
  SaveStream.Write(fOwner);
  SaveStream.Write(fDefenceTowersPlanned);

  for HT := HOUSE_MIN to HOUSE_MAX do
  begin
    SaveStream.Write(fPlannedHouses[HT].Count);
    Len := Length(fPlannedHouses[HT].Plans);
    SaveStream.Write( Len );
    for I := 0 to fPlannedHouses[HT].Count - 1 do
      SaveStream.Write(fPlannedHouses[HT].Plans[I], SizeOf(THousePlan));
  end;

  fRoadPlanner.Save(SaveStream);
  fRoadShortcutPlanner.Save(SaveStream);
end;


procedure TKMCityPlanner.Load(LoadStream: TKMemoryStream);
var
  HT: THouseType;
  I, Len: Integer;
begin
  LoadStream.ReadAssert('CityPlanner');
  LoadStream.Read(fOwner);
  LoadStream.Read(fDefenceTowersPlanned);

  for HT := HOUSE_MIN to HOUSE_MAX do
  begin
    LoadStream.Read(fPlannedHouses[HT].Count);
    LoadStream.Read(Len);
    SetLength(fPlannedHouses[HT].Plans, Len);
    for I := 0 to fPlannedHouses[HT].Count - 1 do
      LoadStream.Read(fPlannedHouses[HT].Plans[I], SizeOf(THousePlan));
  end;

  fRoadPlanner.Load(LoadStream);
  fRoadShortcutPlanner.Load(LoadStream);
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
    fPlannedHouses[HT].Count := IdxArr[HT];
    SetLength(fPlannedHouses[HT].Plans, IdxArr[HT]);
  end;

  Houses := gHands[fOwner].Houses;
  for I := 0 to Houses.Count - 1 do
  begin
    HT := Houses[I].HouseType;
    Dec(IdxArr[HT], 1);
    with fPlannedHouses[HT].Plans[ IdxArr[HT] ] do
    begin
      Placed := True;
      ShortcutsCompleted := False;
      RemoveTreeInPlanProcedure := False;
      HouseReservation := False;
      Loc := Houses[I].Entrance;
      SpecPoint := KMPOINT_ZERO;
      UID := Houses[I].UID;
    end;
  end;
end;

procedure TKMCityPlanner.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
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
    if W.IsFlagPointSet or KMSamePoint(aHousePlan.SpecPoint, KMPOINT_ZERO) then
      Exit;
    // Set the cutting point (it can be in lager distance so check is needed)
    W.FlagPoint := aHousePlan.SpecPoint;
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
  // Priority: function is called from CityBuilder only in right time
  for HT := Low(fPlannedHouses) to High(fPlannedHouses) do
    for I := 0 to fPlannedHouses[HT].Count - 1 do
      with fPlannedHouses[HT].Plans[I] do
      begin
        if Placed then
        begin
          H := gHands[fOwner].Houses.GetHouseByUID(UID);
          Placed := (H <> nil) AND not H.IsDestroyed;
          if (HT = ht_Woodcutters) AND Placed AND (H.IsComplete) then
            CheckWoodcutter(fPlannedHouses[HT].Plans[I], H);
        end
        else
          for K := 0 to gHands[fOwner].Houses.Count - 1 do
            if KMSamePoint(Loc, gHands[fOwner].Houses[K].Entrance) then
            begin
              Placed := not gHands[fOwner].Houses[K].IsDestroyed;
              if Placed then
                gHands[fOwner].AI.CityManagement.Builder.UnlockHouseLoc(HT, fPlannedHouses[HT].Plans[I].Loc);
              UID := gHands[fOwner].Houses[K].UID;
              break;
            end;
      end;
end;


procedure TKMCityPlanner.AddPlan(aHT: THouseType; aLoc: TKMPoint);
begin
  AddPlan(aHT, aLoc, KMPOINT_ZERO); // Cannot declare KMPOINT_ZERO as a default value so overload method is used instead
end;

procedure TKMCityPlanner.AddPlan(aHT: THouseType; aLoc: TKMPoint; aSpecPoint: TKMPoint);
const
  ADD_VALUE = 8;
begin
  // Check size of array
  if (fPlannedHouses[aHT].Count >= Length(fPlannedHouses[aHT].Plans)) then
    SetLength(fPlannedHouses[aHT].Plans, fPlannedHouses[aHT].Count + ADD_VALUE);
  // Add plan
  with fPlannedHouses[aHT].Plans[ fPlannedHouses[aHT].Count ] do
  begin
    Placed := False;
    ShortcutsCompleted := False;
    RemoveTreeInPlanProcedure := False;
    HouseReservation := False;
    Loc := aLoc;
    SpecPoint := aSpecPoint;
    //UID := ...; UID cannot be added when house does not exist in hands
  end;
  fPlannedHouses[aHT].Count := fPlannedHouses[aHT].Count + 1;
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
  var
    Exhausted: Boolean;
  begin
    with fPlannedHouses[aHT].Plans[aIdx] do
    begin
      case aHT of
        ht_GoldMine: Exhausted := IsExhaustedMine(Loc, True);
        ht_IronMine: Exhausted := IsExhaustedMine(Loc, False);
        ht_CoalMine: Exhausted := IsExhaustedCoalMine(Loc);
        ht_Quary:    Exhausted := IsExhaustedQuary(Loc);
        else
          begin
          end;
      end;
      if Exhausted then
        RemovePlan(aHT, aIdx);
      Result := Exhausted;
    end
  end;

  function DistFromStore(aLoc: TKMPoint): Single;
  var
    I: Integer;
    Output, Bid: Single;
  begin
    Output := MAX_BID;
    for I := 0 to fPlannedHouses[ht_Store].Count - 1 do
    begin
      Bid := KMDistanceAbs(aLoc, fPlannedHouses[ht_Store].Plans[I].Loc);
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
  for I := fPlannedHouses[aHT].Count - 1 downto 0 do
    if not fPlannedHouses[aHT].Plans[I].Placed then
    begin
      if fPlannedHouses[aHT].Plans[I].RemoveTreeInPlanProcedure OR gAIFields.Eye.CanAddHousePlan(fPlannedHouses[aHT].Plans[I].Loc, aHT, True, True) then
      begin
        if (aHT in [ht_GoldMine, ht_IronMine, ht_CoalMine, ht_Quary]) AND CheckMine(I) then // Filter mines / chop-only woodcutters
          continue;
        Bid := DistFromStore(fPlannedHouses[aHT].Plans[I].Loc) + GetTreesInHousePlanCnt(aHT, fPlannedHouses[aHT].Plans[I].Loc) * TREE_PENALIZATION;
        if  (aHT = ht_Woodcutters) then
        begin
          SpecPoint := fPlannedHouses[aHT].Plans[I].SpecPoint;
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
    end;
  if (BestBid <> MAX_BID) then
  begin
    aLoc := fPlannedHouses[aHT].Plans[BestIdx].Loc;
    aIdx := BestIdx;
    Output := True;
  end;
  Result := Output;
end;


procedure TKMCityPlanner.MarkAsExhausted(aHT: THouseType; aLoc: TKMPoint);
begin
  RemovePlan(aHT, aLoc);
end;


procedure TKMCityPlanner.RemovePlan(aHT: THouseType; aLoc: TKMPoint);
var
  I: Integer;
begin
  for I := 0 to fPlannedHouses[aHT].Count - 1 do
    if KMSamePoint(fPlannedHouses[aHT].Plans[I].Loc,aLoc) then
    begin
      RemovePlan(aHT, I);
      Exit;
    end;
end;

procedure TKMCityPlanner.RemovePlan(aHT: THouseType; aIdx: Integer);
begin
  with fPlannedHouses[aHT] do
  begin
    if (aIdx >= Count) then
      Exit;
    gHands[fOwner].AI.CityManagement.Builder.UnLockHouseLoc(aHT, Plans[aIdx].Loc);
    Count := Count - 1;
    Plans[aIdx] := Plans[Count];
  end;
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
      ht_WatchTower:
      begin
        if not fDefenceTowersPlanned then
        begin
          fDefenceTowersPlanned := True;
          PlanDefenceTowers();
        end;
      end;
      else
      begin
        Cnt := FindPlaceForHouse(aUnlockProcedure, aHT, BestLocs);
        if (Cnt > 0) then
        begin
          AddPlan(aHT, BestLocs[0]);
          gHands[fOwner].AI.CityManagement.Builder.LockHouseLoc(aHT, BestLocs[0]);
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
  Loc := KMPointBelow( fPlannedHouses[aHT].Plans[aIdx].Loc );
  H := gHands[fOwner].Houses.FindHouse(ht_Any, Loc.X, Loc.Y, 1, False); // True = complete house, False = house plan
  if (H <> nil) AND fRoadPlanner.Route_Make(Loc, H.PointBelowEntrance, aField) then
    Output := True;
  ReplaceOverlappingRoad( fPlannedHouses[aHT].Plans[aIdx].Loc );
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
    PlanFarmFields( fPlannedHouses[aHT].Plans[aIdx].Loc, aField );
  end
  else if (aHT = ht_Wineyard) then
  begin
    aFieldType := ft_Wine;
    PlanWineFields( fPlannedHouses[aHT].Plans[aIdx].Loc, aField );
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
    Point := KMPointAdd( fPlannedHouses[aHT].Plans[aIdx].Loc, HMA[aHT].Tiles[I] );
    if gTerrain.ObjectIsChopableTree(Point.X, Point.Y) then
      aField.Add(Point);
  end;
  if (aField.Count > 0) then
  begin
    fPlannedHouses[aHT].Plans[aIdx].RemoveTreeInPlanProcedure := True;
    for I := Low(HMA[aHT].Tiles) to High(HMA[aHT].Tiles) do
    begin
      Point := KMPointAdd( fPlannedHouses[aHT].Plans[aIdx].Loc, HMA[aHT].Tiles[I] );
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
        AND (gAIFields.Influences.AvoidBuilding[FieldLoc.Y, FieldLoc.X] <= AVOID_BUILDING_HOUSE_OUTSIDE_LOCK) then // Tile is not reserved
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
  function IsRoad(aPoint: TKMPoint): Boolean; inline;
  begin
    Result := gTerrain.TileIsWalkableRoad(aPoint) OR IsPlan(aPoint, tlRoadWork, ft_Road);
  end;
  function IsCornField(aPoint: TKMPoint): Boolean; inline;
  begin
    Result := gTerrain.TileIsCornField(aPoint) OR IsPlan(aPoint, tlFieldWork, ft_Corn);
  end;
  function IsWineField(aPoint: TKMPoint): Boolean; inline;
  begin
    Result := gTerrain.TileIsWineField(aPoint) OR IsPlan(aPoint, tlFieldWork, ft_Wine);
  end;
  function IsNearHouse(aPoint: TKMPoint): Boolean; inline;
  begin
    Result := not (tpBuild in gTerrain.Land[aPoint.Y,aPoint.X].Passability);
  end;
  function IsAvoidBuilding(aPoint: TKMPoint): Boolean; inline;
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
      Point := KMPointAdd(aLoc, HMA[aHT].Surroundings[Dist,Dir,I]);
      Output := Output
                + Byte(IsNearHouse(Point)) * GA_PLANNER_SnapCrit_SnapToHouse
                + Byte(IsAvoidBuilding(Point) ) * GA_PLANNER_SnapCrit_SnapToFields // OR IsCornField(Point) OR IsWineField(Point)
                + Byte(IsRoad(Point)) * GA_PLANNER_SnapCrit_SnapToRoads;
    end;
  Result := Output;
end;


//{
function TKMCityPlanner.DistCrit(aHT: THouseType; aLoc: TKMPoint): Single;
  function ClosestDistance(): Single;
  const
    MAX_DIST = 1000;
  var
    I: Integer;
    Output, Bid: Single;
    HT: THouseType;
  begin
    Output := MAX_DIST;
    for HT in HOUSE_DEPENDENCE[aHT] do
      for I := 0 to fPlannedHouses[HT].Count - 1 do
      begin
        Bid := KMDistanceAbs(aLoc, fPlannedHouses[HT].Plans[I].Loc);
        if (Bid < Output) then
          Output := Bid;
      end;
    if (Output = MAX_DIST) then
      Output := 0;
    Result := Output;
  end;
  function AllDistances(): Single;
  var
    I: Integer;
    HT: THouseType;
  begin
    Result := 0;
    for HT in HOUSE_DEPENDENCE[aHT] do
      for I := 0 to fPlannedHouses[HT].Count - 1 do
        Result := Result + KMDistanceAbs(aLoc, fPlannedHouses[HT].Plans[I].Loc);
  end;
begin
  if (aHT = ht_Barracks) then
    Result := - AllDistances()
  else
    Result := - ClosestDistance();
end;
//}


// Faster method for placing house
//{
function TKMCityPlanner.FindPlaceForHouse(aUnlockProcedure: Boolean; aHT: THouseType; out aBestLocs: TKMPointArray): Byte;
const
  BEST_PLANS_CNT = 5;
  INIT_BEST_BID = -1000000;
var
  BestBidArr: array[0..BEST_PLANS_CNT-1] of Single;

  procedure EvaluateLoc(aLoc: TKMPoint);
  var
    L: Integer;
    Bid, POMBid: Single;
  begin
    Bid := + SnapCrit(aHT, aLoc) * GA_PLANNER_FindPlaceForHouse_SnapCrit
           + DistCrit(aHT, aLoc) * GA_PLANNER_FindPlaceForHouse_DistCrit
           - GetTreesInHousePlanCnt(aHT, aLoc) * GA_PLANNER_FindPlaceForHouse_TreeInPlan;
           //+ Abs(fPlannedHouses[HType,I].Loc.Y - Loc.Y) * 3 // Prefer to build houses on left / right side
           //+ Abs(fPlannedHouses[HType,I].Loc.X - Loc.X) * 2
    if (aHT = ht_Farm) OR (aHT = ht_Wineyard) then
      Bid := Bid + FieldCrit(aHT, aLoc) * GA_PLANNER_FindPlaceForHouse_FarmCrit;
    for L := 0 to BEST_PLANS_CNT - 1 do
      if KMSamePoint(aLoc, aBestLocs[L]) then
        break
      else if (Bid > BestBidArr[L]) then // Buble sort for BEST_PLANS_CNT elements ...
      begin
        KMSwapPoints(aLoc, aBestLocs[L]);
        POMBid := BestBidArr[L];
        BestBidArr[L] := Bid;
        Bid := POMBid;
      end;
  end;

  procedure FindPlaceAroundHType(aHT_HMA: THouseType);
  const
    INFLUENCE_LIMIT = 100;
  var
    I,K, Dist: Integer;
    Dir: TDirection;
    Loc: TKMPoint;
    HMA: THouseMappingArray;
  begin
    HMA := gAIFields.Eye.HousesMapping;
    for Dist := 2 to MAX_SCAN_DIST_FROM_HOUSE do
    begin
      for I := fPlannedHouses[aHT_HMA].Count - 1 downto 0 do
        for Dir := Low(HMA[aHT_HMA].Surroundings[Dist]) to High(HMA[aHT_HMA].Surroundings[Dist]) do
          for K := Low(HMA[aHT_HMA].Surroundings[Dist,Dir]) to High(HMA[aHT_HMA].Surroundings[Dist,Dir]) do
          begin
            Loc := KMPointAdd(fPlannedHouses[aHT_HMA].Plans[I].Loc, HMA[aHT_HMA].Surroundings[Dist,Dir,K], HMA[aHT].MoveToEntrance[Dir]);
            if (Dist > 4) AND gTerrain.TileInMapCoords(Loc.X, Loc.Y, 1) AND (gAIFields.Influences.Ownership[fOwner, Loc.Y, Loc.X] < INFLUENCE_LIMIT) then
              continue;
            if gAIFields.Eye.CanAddHousePlan(Loc, aHT, False, not aUnlockProcedure) then
            //if gAIFields.Eye.CanAddHousePlan(Loc, aHT, False, False) then
              EvaluateLoc(Loc);
          end;
      if (Dist > 2) AND (BestBidArr[BEST_PLANS_CNT-1] <> INIT_BEST_BID) then // When we have full array of possible houses break searching
        break;
    end;
  end;

var
  I, K: Integer;
  POMBid: Single;
  HT: THouseType;
begin
  SetLength(aBestLocs, BEST_PLANS_CNT);
  for I := 0 to BEST_PLANS_CNT - 1 do
    BestBidArr[I] := INIT_BEST_BID;

  for HT in HOUSE_DEPENDENCE[aHT] do
    FindPlaceAroundHType(HT);

  if (BestBidArr[0] = INIT_BEST_BID) then
    for HT := HOUSE_MIN to HOUSE_MAX do
      if not (HT in HOUSE_DEPENDENCE[aHT]) then
        FindPlaceAroundHType(HT);

  for I := 0 to BEST_PLANS_CNT - 1 do
    BestBidArr[I] := BestBidArr[I] - gAIFields.Influences.GetOtherOwnerships(fOwner, aBestLocs[I].X, aBestLocs[I].Y) * ManTune_PLANNER_FindPlaceForHouse_Influence;

  for I := 0 to BEST_PLANS_CNT - 2 do
    for K := 0 to BEST_PLANS_CNT - I - 2 do
      if (BestBidArr[K] < BestBidArr[K+1]) then
      begin
        KMSwapPoints(aBestLocs[K], aBestLocs[K+1]);
        POMBid := BestBidArr[K];
        BestBidArr[K] := BestBidArr[K+1];
        BestBidArr[K+1] := POMBid;
      end;

  for I := High(BestBidArr) downto Low(BestBidArr) do
    if (BestBidArr[I] <> INIT_BEST_BID) then
      break;
  Result := I;
end;
//}


{
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
    for K := 0 to Min(2, fPlannedHouses[ aHTs[I] ].Count - 1) do
      Output := Output + KMDistanceAbs(aLoc, fPlannedHouses[ aHTs[I] ].Plans[K].Loc);
    Result := Output;
  end;

  function DistFromHouse(aHTs: array of THouseType): Single;
  var
    I,K: Integer;
    Output, Bid: Single;
  begin
    Output := MAX_BID;
    for I := Low(aHTs) to High(aHTs) do
    for K := 0 to Min(2, fPlannedHouses[ aHTs[I] ].Count - 1) do
    begin
      Bid := KMDistanceAbs(aLoc, fPlannedHouses[ aHTs[I] ].Plans[K].Loc);
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
//}


// Original method (no optimalization)
{
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
  for I := fPlannedHouses[HType].Count - 1 downto 0 do
  begin
    for Dist := 2 to MAX_SCAN_DIST_FROM_HOUSE do
    begin
      for Dir := Low(HMA[HType].Surroundings[Dist]) to High(HMA[HType].Surroundings[Dist]) do
      for K := Low(HMA[HType].Surroundings[Dist,Dir]) to High(HMA[HType].Surroundings[Dist,Dir]) do
      begin
        Loc := KMPointAdd(fPlannedHouses[HType].Plans[I].Loc, HMA[HType].Surroundings[Dist,Dir,K], HMA[aHT].MoveToEntrance[Dir]);
        if gAIFields.Eye.CanAddHousePlan(Loc, aHT, False, not aUnlockProcedure) then
        begin
          Bid := + SnapCrit(aHT, Loc) * GA_PLANNER_FindPlaceForHouse_SnapCrit
                 + DistCrit(aHT, Loc) * GA_PLANNER_FindPlaceForHouse_DistCrit
                 - GetTreesInHousePlanCnt(aHT, Loc) * GA_PLANNER_FindPlaceForHouse_TreeInPlan
                 //+ Abs(fPlannedHouses[HType,I].Loc.Y - Loc.Y) * 3 // Prefer to build houses on left / right side
                 //+ Abs(fPlannedHouses[HType,I].Loc.X - Loc.X) * 2
                 - gAIFields.Influences.GetOtherOwnerships(fOwner,Loc.X,Loc.Y) * ManTune_PLANNER_FindPlaceForHouse_Influence;
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
//}


function TKMCityPlanner.FindPlaceForMines(aHT: THouseType): Boolean;
const
  MAX_LOCS = 5;

  // Get closest mine
  function FindPlaceForMine(aMine: THouseType): Boolean;
  const
    BEST_BID = -10000;
  var
    Output: Boolean;
    I, BestIdx: Integer;
    Bid, BestBid: Single;
    Locs: TKMPointTagList;
  begin
    Output := False;
    Locs := gAIFields.Eye.GetMineLocs(aMine);
    try
      if (Locs.Count > 0) then
      begin
        Locs.SortByTag();
        BestBid := BEST_BID;
        for I := Locs.Count-1 downto 0 do
        begin
          Bid := Locs.Tag[I] + DistCrit(aMine, Locs.Items[I]) * 10;
          if (Bid > BestBid) then
          begin
            BestIdx := I;
            BestBid := Bid;
          end;
        end;
        if (BestBid <> BEST_BID) then
        begin
          AddPlan(aHT, Locs.Items[BestIdx]);
          Output := True;
        end;
      end;
    finally
      Locs.Free;
    end;
    Result := Output;
  end;

  // Determine whether are coal tiles under coal mine plan in aCoalLoc
  function CoalUnderPlan(aCoalLoc: TKMPoint): Byte;
  var
    Output: Byte;
    X,Y,I: Integer;
    HMA: THouseMappingArray;
  begin
    Output := 0;
    HMA := gAIFields.Eye.HousesMapping;
    for I := Low(HMA[ht_CoalMine].Tiles) to High(HMA[ht_CoalMine].Tiles) do
    begin
      X := aCoalLoc.X + HMA[ht_CoalMine].Tiles[I].X;
      Y := aCoalLoc.Y + HMA[ht_CoalMine].Tiles[I].Y;
      Output := Output + Byte(gTerrain.TileIsCoal(X, Y) > 1);
    end;
    Result := Output;
  end;

  // Coal mine planner
  function FindPlaceForCoalMine(): Boolean;
  const
    BEST_BID = -10000;
  var
    Output: Boolean;
    Coal: Byte;
    I, BestIdx: Integer;
    Bid, BestBid: Single;
    Locs: TKMPointTagList;
  begin
    Output := False;

    Locs := gAIFields.Eye.GetCoalLocs();
    try
      if (Locs.Count > 0) then
      begin
        Locs.SortByTag();
        BestBid := BEST_BID;
        for I := Locs.Count-1 downto 0 do
          if gAIFields.Eye.CanAddHousePlan(Locs.Items[I], ht_CoalMine, True, False) then
          begin
            Coal := CoalUnderPlan(Locs.Items[I]);
            if (Coal = 0) then
              continue;
            Bid := Locs.Tag[I] + DistCrit(ht_CoalMine, Locs.Items[I]) * 10 + CoalUnderPlan(Locs.Items[I]) * 10;
            if (Bid > BestBid) then
            begin
              BestIdx := I;
              BestBid := Bid;
            end;
            //if (I > 10) AND (BestBid <> BEST_BID) then
            //begin
            //  AddPlan(aHT, Locs.Items[BestIdx]);
            //  Output := True;
            //  break;
            //end;
          end;
        if (BestBid <> BEST_BID) then
        begin
          AddPlan(aHT, Locs.Items[BestIdx]);
          Output := True;
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
    for I := 0 to fPlannedHouses[ht_Quary].Count - 1 do
    begin
      Dist := KMDistanceAbs(aLoc, fPlannedHouses[ht_Quary].Plans[I].Loc);
      if (Dist < Output) then
        Output := Dist;
    end;
    if (Output = High(Integer)) then
      Output := 0;
    Result := Output;
  end;

  // Quarry planner (constants in this function are critical and will not be set by CA)
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
        Locs.Tag[I] := Max(0, 100000 - Locs.Tag[I]
                                     + gAIFields.Influences.GetOtherOwnerships(fOwner,Locs.Items[I].X,Locs.Items[I].Y)
                                     - Round(DistCrit(ht_Quary, Locs.Items[I])) * 20);
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
            Bid := + DistCrit(ht_Quary, Loc) * 20 + SnapCrit(ht_Quary, Loc);
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
               + SnapCrit(ht_Woodcutters, Loc)
               - Byte(gAIFields.Influences.AvoidBuilding[Loc.Y, Loc.X] >= AVOID_BUILDING_COAL_TILE) * 1000; // Try not to place woodcutter into full forest or coal tile
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
      if ((aCenter.Y <= BestLoc.Y) AND (aCenter.Y >= BestLoc.Y-1)) AND ((aCenter.X <= BestLoc.X) AND (aCenter.X >= BestLoc.X-2)) then
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
                            - gAIFields.Influences.GetOtherOwnerships(fOwner, Forests.Items[I].X, Forests.Items[I].Y) * ManTune_PLANNER_FindPlaceForWoodcutter_Influence
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
        gAIFields.Influences.AddAvoidBuilding(Forests.Items[I].X, Forests.Items[I].Y, BLOCK_RAD, AVOID_BUILDING_FOREST_ADD);
    end;
  finally
    Forests.Free;
  end;
  Result := Output;
end;


function TKMCityPlanner.PlanDefenceTowers(): Boolean;

  procedure FindPlaceForTowers(aCenter: TKMPoint);
  const
    RADIUS = 3;
    MAX_BID = 100000;
  var
    X,Y: Integer;
    Bid, BestBid: Single;
    Loc, BestLoc: TKMPoint;
  begin
    BestBid := MAX_BID;
    for Y := Max(1, aCenter.Y - RADIUS) to Min(gTerrain.MapY, aCenter.Y + RADIUS) do
    for X := Max(1, aCenter.X - RADIUS) to Min(gTerrain.MapX, aCenter.X + RADIUS) do
    begin
      Loc := KMPoint(X,Y);
      if gAIFields.Eye.CanAddHousePlan(Loc, ht_WatchTower, True, False) then
      begin
        Bid := KMDistanceAbs(aCenter, Loc);
        if (Bid < BestBid) then
        begin
          BestBid := Bid;
          BestLoc := Loc;
        end;
      end;
    end;
    if (BestBid <> MAX_BID) then
      AddPlan(ht_WatchTower, BestLoc);
  end;

const
  DISTANCE_BETWEEN_TOWERS = 10;
var
  I, K, DefCount: Integer;
  P1,P2: TKMPoint;
  Ratio: Single;
  DefLines: TKMDefenceLines;
begin
  Result := False;

  if not gHands[fOwner].Locks.HouseCanBuild(ht_WatchTower)
    OR not gAIFields.NavMesh.GetDefenceLines(fOwner, DefLines)
    OR (DefLines.Count < 1) then
    Exit;

  //Make list of defence positions
  for I := 0 to DefLines.Count-1 do
  begin
    P1 := gAIFields.NavMesh.Nodes[ DefLines.Lines[I].Nodes[0] ].Loc;
    P2 := gAIFields.NavMesh.Nodes[ DefLines.Lines[I].Nodes[1] ].Loc;
    DefCount := Ceil( KMLength(P1,P2) / DISTANCE_BETWEEN_TOWERS );
    for K := 0 to DefCount - 1 do
    begin
      Ratio := (K + 1) / (DefCount + 1);
      FindPlaceForTowers( KMPointRound(KMLerp(P1, P2, Ratio)) );
    end;
  end;
  Result := True;
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
    for I := 0 to fPlannedHouses[HT].Count - 1 do
    begin
      for K := 0 to Length(HMA[HT].Tiles)-1 do
      begin
        Loc := KMPointAdd(fPlannedHouses[HT].Plans[I].Loc, HMA[HT].Tiles[K]);
        gRenderAux.Quad(Loc.X, Loc.Y, Color);
      end;
    end;
  end;
end;







{ TPathFindingCityPlanner }
function TPathFindingCityPlanner.IsWalkableTile(aX, aY: Word): Boolean;
begin
  // Just in case that worker will die while digging house plan or when you plan road near ally
  Result := inherited AND (gAIFields.Influences.AvoidBuilding[aY, aX] <> AVOID_BUILDING_HOUSE_INSIDE_LOCK);
end;


function TPathFindingCityPlanner.MovementCost(aFromX, aFromY, aToX, aToY: Word): Word;
var
  IsRoad: Boolean;
  AvoidBuilding: Byte;
begin
  Result := 0;
  AvoidBuilding := gAIFields.Influences.AvoidBuilding[aToY, aToX];
  IsRoad := (AvoidBuilding = AVOID_BUILDING_NODE_LOCK_ROAD)                                      // Reserved road plan
            OR (tpWalkRoad in gTerrain.Land[aToY, aToX].Passability)                             // Completed road
            OR (gHands[fOwner].BuildList.FieldworksList.HasField(KMPoint(aToX, aToY)) = ft_Road) // Placed road plan
            OR (gTerrain.Land[aToY, aToX].TileLock = tlRoadWork);                                // Road under construction

  if not IsRoad then
  begin
    // Snap to no-build areas (1 tile from house)
    if (tpBuild in gTerrain.Land[aToY,aToX].Passability) then
      Inc(Result, 10);
    //Building roads over fields is discouraged unless unavoidable
    case AvoidBuilding of
      AVOID_BUILDING_HOUSE_OUTSIDE_LOCK: begin Result := 5; end; // 1 tile from future house
      AVOID_BUILDING_NODE_LOCK_FIELD: Inc(Result, 60); // Corn / wine field
      //AVOID_BUILDING_HOUSE_INSIDE_LOCK: begin end; // Tiles inside future house (forbiden)
      //AVOID_BUILDING_NODE_LOCK_ROAD: begin end; // This will not occur
      else
        Inc(Result, 20); // Forest or mines etc.
    end;
  end;
end;


{ TPathFindingShortcutsCityPlanner }
function TPathFindingShortcutsCityPlanner.MovementCost(aFromX, aFromY, aToX, aToY: Word): Word;
begin
  Result := 15 + inherited MovementCost(aFromX, aFromY, aToX, aToY);
end;


function TPathFindingShortcutsCityPlanner.DestinationReached(aX, aY: Word): Boolean;
begin
  Result := ((aX = fLocB.X) and (aY = fLocB.Y)); //We reached destination point
end;



end.



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
