unit KM_CityPlanner;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, KromUtils, Math, SysUtils,
  KM_Defaults, KM_Points, KM_CommonClasses, KM_CommonTypes, KM_CommonUtils,
  KM_TerrainFinder, KM_PerfLog, KM_Houses, KM_ResHouses, KM_ResWares,
  KM_PathFindingRoad, KM_CityPredictor,
  KM_AIInfluences, KM_NavMeshDefences;


var

  GA_PLANNER_FindPlaceForHouse_Influence            : Single = 200; // 0..XXX
  GA_PLANNER_FindPlaceForWoodcutter_Influence       : Single = 20; // 0..255

  GA_PLANNER_ObstaclesInHousePlan_Tree                : Single = 146.4402008;
  GA_PLANNER_ObstaclesInHousePlan_Road                : Single = 122.669342;
  GA_PLANNER_FieldCrit_MissingFields                  : Single = 133.574173;
  GA_PLANNER_FieldCrit_FarmPosition                   : Single = 50.25531006;
  GA_PLANNER_SnapCrit_SnapToHouse                     : Single = 9.67427063;
  GA_PLANNER_SnapCrit_SnapToFields                    : Single = 44.00505447;
  GA_PLANNER_SnapCrit_SnapToRoads                     : Single = 69.20513153;
  GA_PLANNER_SnapCrit_ClearEntrance                   : Single = 50.13886261;
  GA_PLANNER_FindPlaceForHouse_CloseWorker            : Single = 1.469583392;
  GA_PLANNER_FindPlaceForHouse_SnapCrit               : Single = 10.59704971;
  GA_PLANNER_FindPlaceForHouse_DistCrit               : Single = 40.41218185;
  GA_PLANNER_FindPlaceForHouse_CityCenter             : Single = 39.48143005;
  GA_PLANNER_FindPlaceForHouse_EvalArea               : Single = 99.74505615;
  GA_PLANNER_PlaceWoodcutter_DistFromForest           : Single = 7.817649841;
  GA_PLANNER_FindPlaceForWoodcutter_TreeCnt           : Single = 97.99571991;
  GA_PLANNER_FindPlaceForWoodcutter_PolyRoute         : Single = 5.305774212;
  GA_PLANNER_FindPlaceForWoodcutter_EvalArea          : Single = 21.0926075;
  GA_PLANNER_FindPlaceForWoodcutter_ExistForest       : Single = 155.5762634;
  GA_PLANNER_FindPlaceForWoodcutter_DistCrit          : Single = 10.45453072;

  GA_PATHFINDING_BasePrice    : Word = 0;
  GA_PATHFINDING_HouseOutside : Word = 1;
  GA_PATHFINDING_Field        : Word = 5;
  GA_PATHFINDING_noBuildArea  : Word = 1;
  GA_PATHFINDING_Coal         : Word = 2;
  GA_PATHFINDING_Forest       : Word = 3;
  GA_PATHFINDING_OtherCase    : Word = 3;

  GA_SHORTCUTS_BasePrice      : Word = 1;
  GA_SHORTCUTS_HouseOutside   : Word = 1;
  GA_SHORTCUTS_Field          : Word = 8;
  GA_SHORTCUTS_noBuildArea    : Word = 1;
  GA_SHORTCUTS_Coal           : Word = 2;
  GA_SHORTCUTS_Forest         : Word = 4;
  GA_SHORTCUTS_OtherCase      : Word = 2;

type
  THousePlan = record
    Placed, ShortcutsCompleted, RemoveTreeInPlanProcedure, HouseReservation, ChopOnly: Boolean;
    House: TKMHouse;
    Loc, SpecPoint: TKMPoint;
  end;
  THousePlanArray = record
    Count, Calculated: Word;
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
    fConstructedHouses: Word;
    fDefenceTowersPlanned: Boolean;
    fPlannedHouses: TPlannedHousesArray;
    fForestsNearby: TKMPointTagList;

    fRoadPlanner: TPathFindingCityPlanner;
    fRoadShortcutPlanner: TPathFindingShortcutsCityPlanner;

    fPerfIdx: Byte;
    fPerfArr: TKMByte2Array;
    procedure ClearPerfArr();

    procedure AddPlan(aHT: TKMHouseType; aLoc: TKMPoint); overload;
    procedure AddPlan(aHT: TKMHouseType; aLoc: TKMPoint; aSpecPoint: TKMPoint; aChopOnly: Boolean = False); overload;
    function GetPlan(aHT: TKMHouseType; out aLoc: TKMPoint; out aIdx: Integer): Boolean;

    function ObstaclesInHousePlan(aHT: TKMHouseType; aLoc: TKMPoint): Single;
    function FieldCrit(aHT: TKMHouseType; aLoc: TKMPoint): Single;
    function SnapCrit(aHT: TKMHouseType; aLoc: TKMPoint): Single;
    function DistCrit(aHT: TKMHouseType; aLoc: TKMPoint): Single;

    procedure PlanWineFields(aLoc: TKMPoint; var aNodeList: TKMPointList);
    procedure PlanFarmFields(aLoc: TKMPoint; var aNodeList: TKMPointList);
    function FindPlaceForHouse(aUnlockProcedure: Boolean; aHT: TKMHouseType; out aBestLocs: TKMPointArray): Byte;
    function FindPlaceForMines(aHT: TKMHouseType): Boolean;
    function FindPlaceForWoodcutter(aCenter: TKMPoint; aChopOnly: Boolean = False): Boolean;
    function FindForestAndWoodcutter(): Boolean;
    function PlanDefenceTowers(): Boolean;

  public
    constructor Create(aPlayer: TKMHandIndex);
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad();

    procedure AfterMissionInit();
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    procedure UpdateState(aTick: Cardinal);

    // Properties for GA (in Runner)
    property ConstructedHouses: Word read fConstructedHouses;
    property PlannedHouses: TPlannedHousesArray read fPlannedHouses write fPlannedHouses;
    property DefenceTowersPlanned: Boolean read fDefenceTowersPlanned;

    procedure MarkAsExhausted(aHT: TKMHouseType; aLoc: TKMPoint);

    procedure RemovePlan(aHT: TKMHouseType; aLoc: TKMPoint); overload;
    procedure RemovePlan(aHT: TKMHouseType; aIdx: Integer); overload;

    function GetHousePlan(aUnlockProcedure, aIgnoreExistingPlans: Boolean; aHT: TKMHouseType; var aLoc: TKMPoint; var aIdx: Integer): Boolean;
    function GetRoadBetweenPoints(aStart, aEnd: TKMPoint; var aField: TKMPointList; var aFieldType: TKMFieldType): Boolean;
    function GetRoadToHouse(aHT: TKMHouseType; aIdx: Integer; var aField: TKMPointList; var aFieldType: TKMFieldType): Boolean;
    function GetFieldToHouse(aHT: TKMHouseType; aIdx: Integer; var aField: TKMPointList; var aFieldType: TKMFieldType): Boolean;
    function GetTreesInHousePlan(aHT: TKMHouseType; aIdx: Integer; var aField: TKMPointList): Byte;
    function FindForestAround(const aPoint: TKMPoint; aCountByInfluence: Boolean = False): Boolean;

    procedure Paint();
  end;


const
  HOUSE_DEPENDENCE: array[HOUSE_MIN..HOUSE_MAX] of set of TKMHouseType = (  // This array is sorted by priority
    {htArmorSmithy}    [ htIronSmithy,     htCoalMine,       htBarracks,       htIronMine       ],
    {htArmorWorkshop}  [ htTannery,        htBarracks,       htSawmill,        htArmorWorkshop  ],
    {htBakery}         [ htInn,            htMill,           htStore,          htBakery         ],
    {htBarracks}       [ htArmorWorkshop,  htArmorSmithy,    htWeaponSmithy,   htWeaponWorkshop ],
    {htButchers}       [ htInn,            htSwine,          htStore,          htButchers       ],
    {htCoalMine}       [ htStore                                                                ],
    {htFarm}           [ htFarm,           htSwine,          htMill,           htStables        ],
    {htFisherHut}      [ htStore                                                                ],
    {htGoldMine}       [ htMetallurgists,  htStore                                              ],
    {htInn}            [ htButchers,       htBakery,         htStore,          htWineyard       ],
    {htIronMine}       [ htStore                                                                ],
    {htIronSmithy}     [ htCoalMine,       htIronMine,       htWeaponSmithy,   htIronSmithy     ],
    {htMarketplace}    [ htStore,          htMetallurgists,  htBarracks,       htMarketplace    ],
    {htMetallurgists}  [ htGoldMine,       htCoalMine,       htSchool,         htStore          ],
    {htMill}           [ htBakery,         htInn,            htMill                             ],
    {htQuary}          [ htStore                                                                ],
    {htSawmill}        [ htArmorWorkshop,  htSawmill,        htWeaponWorkshop                   ],
    {htSchool}         [ htMetallurgists,  htStore,          htSchool                           ],
    {htSiegeWorkshop}  [ htIronSmithy,     htSawmill,        htStore,          htSiegeWorkshop  ],
    {htStables}        [ htFarm,           htBarracks,       htStables                          ],
    {htStore}          [ htInn,            htBarracks,       htSchool                           ],
    {htSwine}          [ htFarm,           htButchers,       htSwine                            ],
    {htTannery}        [ htArmorWorkshop,  htTannery,        htBarracks                         ],
    {htTownHall}       [ htMetallurgists,  htStore,          htTownHall                         ],
    {htWatchTower}     [ htStore                                                                ],
    {htWeaponSmithy}   [ htIronSmithy,     htCoalMine,       htBarracks,       htIronMine       ],
    {htWeaponWorkshop} [ htSawmill,        htBarracks,       htWeaponWorkshop                   ],
    {htWineyard}       [ htInn,            htQuary                                              ],
    {htWoodcutters}    [ htStore                                                                ]
  );

implementation
uses
  KM_Game, KM_HouseCollection, KM_HouseSchool, KM_HandsCollection, KM_Hand, KM_Terrain, KM_Resource,
  KM_AIFields, KM_Units, KM_UnitTaskDelivery, KM_UnitActionWalkTo, KM_UnitTaskGoEat, KM_UnitsCollection,
  KM_NavMesh, KM_HouseMarket, KM_HouseWoodcutters, KM_Eye,
  KM_RenderAux, KM_ResMapElements;






{ TKMCityPlanner }
constructor TKMCityPlanner.Create(aPlayer: TKMHandIndex);
begin
  fConstructedHouses := 0;
  fOwner := aPlayer;
  fDefenceTowersPlanned := False;
  fForestsNearby := TKMPointTagList.Create();
  fRoadPlanner := TPathFindingCityPlanner.Create(fOwner);
  fRoadShortcutPlanner := TPathFindingShortcutsCityPlanner.Create(fOwner);
end;


destructor TKMCityPlanner.Destroy();
var
  HT: TKMHouseType;
  I: Integer;
begin
  for HT := HOUSE_MIN to HOUSE_MAX do
    for I := 0 to fPlannedHouses[HT].Count - 1 do
      if (fPlannedHouses[HT].Plans[I].House <> nil) then
        gHands.CleanUpHousePointer(fPlannedHouses[HT].Plans[I].House);
  fRoadPlanner.Free;
  fForestsNearby.Free;
  fRoadShortcutPlanner.Free;
  inherited;
end;


procedure TKMCityPlanner.Save(SaveStream: TKMemoryStream);
var
  HT: TKMHouseType;
  I, K, Len: Integer;
begin
  SaveStream.WriteA('CityPlanner');
  SaveStream.Write(fOwner);
  SaveStream.Write(fConstructedHouses);
  SaveStream.Write(fDefenceTowersPlanned);
  fForestsNearby.SaveToStream(SaveStream);

  for HT := HOUSE_MIN to HOUSE_MAX do
  begin
    SaveStream.Write(fPlannedHouses[HT].Count);
    SaveStream.Write(fPlannedHouses[HT].Calculated);
    Len := Length(fPlannedHouses[HT].Plans);
    SaveStream.Write( Len );
    for I := 0 to fPlannedHouses[HT].Count - 1 do
      with fPlannedHouses[HT].Plans[I] do
      begin
        SaveStream.Write(Placed);
        SaveStream.Write(ShortcutsCompleted);
        SaveStream.Write(RemoveTreeInPlanProcedure);
        SaveStream.Write(HouseReservation);
        SaveStream.Write(ChopOnly);
        if (House <> nil) then
          SaveStream.Write(House.UID) // Store ID
        else
          SaveStream.Write(Integer(0));
        SaveStream.Write(Loc, SizeOf(Loc));
        SaveStream.Write(SpecPoint, SizeOf(SpecPoint));
      end;
  end;

  SaveStream.Write(fPerfIdx);
  Len := Length(fPerfArr);
  SaveStream.Write(Len);
  if (Len > 0) then
    SaveStream.Write(  Integer( Length(fPerfArr[0]) )  );
  for I := 0 to Length(fPerfArr) - 1 do
    for K := 0 to Length(fPerfArr[I]) - 1 do
      SaveStream.Write(fPerfArr[I,K]);

  fRoadPlanner.Save(SaveStream);
  fRoadShortcutPlanner.Save(SaveStream);
end;


procedure TKMCityPlanner.Load(LoadStream: TKMemoryStream);
var
  HT: TKMHouseType;
  I, K, Len: Integer;
begin
  LoadStream.ReadAssert('CityPlanner');
  LoadStream.Read(fOwner);
  LoadStream.Read(fConstructedHouses);
  LoadStream.Read(fDefenceTowersPlanned);
  fForestsNearby.LoadFromStream(LoadStream);

  for HT := HOUSE_MIN to HOUSE_MAX do
  begin
    LoadStream.Read(fPlannedHouses[HT].Count);
    LoadStream.Read(fPlannedHouses[HT].Calculated);
    LoadStream.Read(Len);
    SetLength(fPlannedHouses[HT].Plans, Len);
    for I := 0 to fPlannedHouses[HT].Count - 1 do
      with fPlannedHouses[HT].Plans[I] do
      begin
        LoadStream.Read(Placed);
        LoadStream.Read(ShortcutsCompleted);
        LoadStream.Read(RemoveTreeInPlanProcedure);
        LoadStream.Read(HouseReservation);
        LoadStream.Read(ChopOnly);
        LoadStream.Read(House, 4); // Load ID
        LoadStream.Read(Loc, SizeOf(Loc));
        LoadStream.Read(SpecPoint, SizeOf(SpecPoint));
      end;
  end;

  LoadStream.Read(fPerfIdx);
  LoadStream.Read(Len);
  if (Len > 0) then
  begin
    LoadStream.Read(K);
    SetLength(fPerfArr, Len, K);
  end;
  for I := 0 to Length(fPerfArr) - 1 do
    for K := 0 to Length(fPerfArr[I]) - 1 do
      LoadStream.Read(fPerfArr[I,K]);

  fRoadPlanner.Load(LoadStream);
  fRoadShortcutPlanner.Load(LoadStream);
end;


procedure TKMCityPlanner.SyncLoad();
var
  HT: TKMHouseType;
  I: Integer;
begin
  for HT := HOUSE_MIN to HOUSE_MAX do
    for I := 0 to fPlannedHouses[HT].Count - 1 do
      with fPlannedHouses[HT].Plans[I] do
        House := gHands.GetHouseByUID( Cardinal(House) );
end;



procedure TKMCityPlanner.AfterMissionInit();
begin
  fPerfIdx := 255; // fPerfArr will be reset in next step
  SetLength(fPerfArr, gTerrain.MapY, gTerrain.MapX);
  // Actual houses will be added in UpdateState (script may remove / add something after mission init ...)
  UpdateState(0);
end;

procedure TKMCityPlanner.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
end;




procedure TKMCityPlanner.UpdateState(aTick: Cardinal);
  procedure ScanChopOnly(aW: TKMHouseWoodcutters);
  const
    SQR_RAD = 10*10;
  var
    I: Integer;
    W: TKMHouseWoodcutters;
  begin
    if (aW.CheckResOut(wt_All) <> 0) then
      Exit;
    for I := 0 to fPlannedHouses[htWoodcutters].Count - 1 do
      with fPlannedHouses[htWoodcutters].Plans[I] do
        if Placed
          AND (House <> nil)
          AND not House.IsDestroyed
          AND House.IsComplete then
        begin
          W := TKMHouseWoodcutters( House );
          if (W.WoodcutterMode <> wcm_Chop)
            AND (KMDistanceSQR(aW.FlagPoint, W.FlagPoint) < SQR_RAD) then
          begin
            MarkAsExhausted(aW.HouseType, aW.Entrance);
            aW.DemolishHouse(fOwner);
          end;
        end;
  end;

  procedure CheckWoodcutter(aHousePlan: THousePlan; aCheckChopOnly: Boolean);
  var
    Point: TKMPoint;
    W: TKMHouseWoodcutters;
  begin
    // Make sure that this house is woodcutter
    if (aHousePlan.House.HouseType <> htWoodcutters) then
      Exit;
    W := TKMHouseWoodcutters(aHousePlan.House);
    // Check if is cutting point required (compare with default cutting point)
    if not KMSamePoint(aHousePlan.SpecPoint, KMPOINT_ZERO) AND not W.IsFlagPointSet then
    // Set the cutting point - do it only once because it reset empty message (woodcutters in chop only mode will not be destroyed)
      W.FlagPoint := aHousePlan.SpecPoint;
    // Check chop-only mode
    Point := W.FlagPoint;
    if aHousePlan.ChopOnly AND (gAIFields.Influences.AvoidBuilding[Point.Y, Point.X] < AVOID_BUILDING_FOREST_MINIMUM) then
    begin
      if aCheckChopOnly then
        ScanChopOnly(W);
      if (W.WoodcutterMode <> wcm_Chop) then // Center of forest is not in protected area => chop only mode
        W.WoodcutterMode := wcm_Chop
    end
    else if (W.WoodcutterMode <> wcm_ChopAndPlant) then
      W.WoodcutterMode := wcm_ChopAndPlant;
  end;

const
  WOODCUT_CHOP_ONLY_CHECK = MAX_HANDS * 100;
var
  CheckChopOnly, CheckExistHouse: Boolean;
  SumCalculated: Word;
  I,K: Integer;
  HT: TKMHouseType;
  H: TKMHouse;
begin
  // Priority: function is called from CityBuilder only in right time
  CheckChopOnly := (aTick mod WOODCUT_CHOP_ONLY_CHECK = fOwner);
  // Find new houses which are added by player / script / at the start of mission etc. and connect them with city plan
  for I := 0 to gHands[fOwner].Houses.Count - 1 do
  begin
    H := gHands[fOwner].Houses[I];
    if (H <> nil) AND not H.IsDestroyed then
    begin
      HT := H.HouseType;
      CheckExistHouse := False;
      for K := 0 to fPlannedHouses[HT].Count - 1 do
        if KMSamePoint(fPlannedHouses[HT].Plans[K].Loc, H.Entrance) then
        begin
          if (fPlannedHouses[HT].Plans[K].House <> H) then
          begin
            // Make sure that reservation is no longer used
            gHands[fOwner].AI.CityManagement.Builder.UnlockHouseLoc(HT, H.Entrance);
            if (fPlannedHouses[HT].Plans[K].House <> nil) then
              gHands.CleanUpHousePointer(fPlannedHouses[HT].Plans[K].House);
            fPlannedHouses[HT].Plans[K].House := H.GetHousePointer;
          end;
          CheckExistHouse := True;
          break;
        end;
      if not CheckExistHouse then // House was added by script / spectator in debug mode
      begin
        if (HT = htWoodcutters) then
          AddPlan(HT, H.Entrance, TKMHouseWoodcutters(H).FlagPoint, TKMHouseWoodcutters(H).WoodcutterMode = wcm_Chop)
        else
          AddPlan(HT, H.Entrance);
      end;
    end;
  end;
  // Check if are existing houses completed / destroyed
  fConstructedHouses := 0;
  for HT := Low(fPlannedHouses) to High(fPlannedHouses) do
  begin
    SumCalculated := fPlannedHouses[HT].Count;
    for I := 0 to fPlannedHouses[HT].Count - 1 do
      with fPlannedHouses[HT].Plans[I] do
      begin
        Placed := ((House <> nil) AND not House.IsDestroyed) OR gHands[fOwner].BuildList.HousePlanList.ExistPlan(Loc, HT);
        if Placed then // House was placed
        begin
          fConstructedHouses := fConstructedHouses + Byte((House = nil) OR not House.IsComplete);
          if (HT = htWoodcutters) AND (House <> nil) AND (House.IsComplete) then
            CheckWoodcutter(fPlannedHouses[HT].Plans[I], CheckChopOnly);
        end
        else if (HouseReservation OR RemoveTreeInPlanProcedure) then // House was reserved
        begin
          // Do nothing
        end
        else // House was destroyed
        begin
          if (House <> nil) then
            gHands.CleanUpHousePointer(House);
          SumCalculated := SumCalculated - 1;
        end;
      end;
    fPlannedHouses[HT].Calculated := SumCalculated;
  end;
end;


procedure TKMCityPlanner.AddPlan(aHT: TKMHouseType; aLoc: TKMPoint);
begin
  AddPlan(aHT, aLoc, KMPOINT_ZERO); // Cannot declare KMPOINT_ZERO as a default value so overload method is used instead
end;

procedure TKMCityPlanner.AddPlan(aHT: TKMHouseType; aLoc: TKMPoint; aSpecPoint: TKMPoint; aChopOnly: Boolean = False);
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
    ChopOnly := aChopOnly;
    Loc := aLoc;
    SpecPoint := aSpecPoint;
    House := nil;
  end;
  fPlannedHouses[aHT].Count := fPlannedHouses[aHT].Count + 1;
end;


function TKMCityPlanner.GetPlan(aHT: TKMHouseType; out aLoc: TKMPoint; out aIdx: Integer): Boolean;
const
  MAX_BID = 1000000;
  CHOP_ONLY_ADVANTAGE = 10;

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
    for I := Low(HMA[htCoalMine].Tiles) to High(HMA[htCoalMine].Tiles) do
    begin
      X := aCoalLoc.X + HMA[htCoalMine].Tiles[I].X;
      Y := aCoalLoc.Y + HMA[htCoalMine].Tiles[I].Y;
      if (gTerrain.TileIsCoal(X, Y) > 1) then
        Exit;
    end;
    Result := True;
  end;

  function CheckMine(aIdx: Integer): Boolean;
  var
    Exhausted: Boolean;
  begin
    Exhausted := False;
    with fPlannedHouses[aHT].Plans[aIdx] do
    begin
      case aHT of
        htGoldMine: Exhausted := IsExhaustedMine(Loc, True);
        htIronMine: Exhausted := IsExhaustedMine(Loc, False);
        htCoalMine: Exhausted := IsExhaustedCoalMine(Loc);
        htQuary:    Exhausted := IsExhaustedQuary(Loc);
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
    for I := 0 to fPlannedHouses[htStore].Count - 1 do
    begin
      Bid := KMDistanceAbs(aLoc, fPlannedHouses[htStore].Plans[I].Loc);
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
begin
  Output := False;
  BestBid := MAX_BID;
  BestIdx := 0; // For compiler
  for I := fPlannedHouses[aHT].Count - 1 downto 0 do
    with fPlannedHouses[aHT].Plans[I] do
      if not Placed then
      begin
        if RemoveTreeInPlanProcedure OR gAIFields.Eye.CanAddHousePlan(Loc, aHT, True, True) then
        begin
          if (aHT in [htGoldMine, htIronMine, htCoalMine, htQuary]) AND CheckMine(I) then // Filter mines / chop-only woodcutters
            continue;
          Bid := + DistFromStore(Loc)
                 + ObstaclesInHousePlan(aHT, Loc)
                 - Byte((aHT = htWoodcutters) AND ChopOnly) * CHOP_ONLY_ADVANTAGE; // Chop only mode
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


procedure TKMCityPlanner.MarkAsExhausted(aHT: TKMHouseType; aLoc: TKMPoint);
begin
  RemovePlan(aHT, aLoc);
end;


procedure TKMCityPlanner.RemovePlan(aHT: TKMHouseType; aLoc: TKMPoint);
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

procedure TKMCityPlanner.RemovePlan(aHT: TKMHouseType; aIdx: Integer);
begin
  with fPlannedHouses[aHT] do
  begin
    if (aIdx >= Count) then
      Exit;
    if (Plans[aIdx].House = nil) then // Unlock house plan
      gHands[fOwner].AI.CityManagement.Builder.UnLockHouseLoc(aHT, Plans[aIdx].Loc);
    Count := Count - 1;
    Plans[aIdx] := Plans[Count];
  end;
end;


function TKMCityPlanner.GetHousePlan(aUnlockProcedure, aIgnoreExistingPlans: Boolean; aHT: TKMHouseType; var aLoc: TKMPoint; var aIdx: Integer): Boolean;
var
  Output: Boolean;
  Cnt: Byte;
  BestLocs: TKMPointArray;
begin
  Output := False;
  if not aIgnoreExistingPlans AND GetPlan(aHT, aLoc, aIdx) then
    Output := True
  else
  begin
    case aHT of
      htWoodcutters: FindForestAndWoodcutter();
      htGoldMine, htCoalMine, htIronMine, htQuary: FindPlaceForMines(aHT);
      htWatchTower:
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
          FindForestAround(aLoc);
        end;
      end;
    end;
    if GetPlan(aHT, aLoc, aIdx) then // Plans are taking from the latest so there is not need to edit this function in case that we want the latest added element
      Output := True;
  end;
  Result := Output;
end;


function TKMCityPlanner.GetRoadToHouse(aHT: TKMHouseType; aIdx: Integer; var aField: TKMPointList; var aFieldType: TKMFieldType): Boolean;
  function IsRoad(aP: TKMPoint): Boolean;
  begin
    Result := (gAIFields.Influences.AvoidBuilding[aP.Y, aP.X] = AVOID_BUILDING_NODE_LOCK_ROAD) // Reserved road plan
              OR (tpWalkRoad in gTerrain.Land[aP.Y, aP.X].Passability)                         // Completed road
              OR (gHands[fOwner].BuildList.FieldworksList.HasField(aP) = ftRoad)              // Placed road plan
              OR (gTerrain.Land[aP.Y, aP.X].TileLock = tlRoadWork);                            // Road under construction
  end;
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
        if IsRoad(Point) then
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
            if IsRoad(Point) AND not Road.Contains(Point) then
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
  function FindClosestHouseEntrance(aOnlyPlaced: Boolean; var aNewLoc, aExistLoc: TKMPoint): Boolean;
  const
    INIT_DIST = 1000000;
    MAX_WATCHTOWER_DIST = 10;
  var
    I: Integer;
    Dist, BestDist: Single;
    Loc: TKMPoint;
    HT: TKMHouseType;
  begin
    BestDist := INIT_DIST;
    for HT := HOUSE_MIN to HOUSE_MAX do
      for I := 0 to fPlannedHouses[HT].Count - 1 do
        if (not aOnlyPlaced OR (fPlannedHouses[HT].Plans[I].Placed))     // Only placed houses?
           AND not fPlannedHouses[HT].Plans[I].RemoveTreeInPlanProcedure // Ignore Remove tree in plan procedure because there is not builded road
           AND not KMSamePoint(fPlannedHouses[HT].Plans[I].Loc, aNewLoc) // Ignore itself
           AND (not (HT = htWoodcutters) OR not fPlannedHouses[HT].Plans[I].ChopOnly) then // Chop only woodcutters are planned without road connection so skip it
        begin
          Loc := fPlannedHouses[HT].Plans[I].Loc;
          Dist := abs(Loc.X - aNewLoc.X) + abs(Loc.Y - aNewLoc.Y) * 1.5; // Prefer to connect road in X axis
          // Watchtowers may be far from the city and path may leads out of protected area
          // so if is distance from closest watchtower too far away better select other house type
          // -> secure that path will lead directly to the city
          if (Dist < BestDist) AND ( (aHT <> htWatchTower) OR (HT <> htWatchTower) OR (Dist < MAX_WATCHTOWER_DIST) ) then
          begin
            BestDist := Dist;
            aExistLoc := Loc;
          end;
        end;
    Result := BestDist <> INIT_DIST;
  end;

  function CheckRoadToTowers(): Boolean;
  const
    MAX_ENEMY_INFLUENCE = 150;
  var
    PolygonIdx: Word;
    I: Integer;
  begin
    Result := False;
    // Check road and if it goes to enemy influence remove house plan
    I := 0;
    while (I < aField.Count) do
    begin
      PolygonIdx := gAIFields.NavMesh.KMPoint2Polygon[ aField[I] ];
      if (gAIFields.Influences.GetBestAllianceOwnership(fOwner, PolygonIdx, at_Enemy) > MAX_ENEMY_INFLUENCE) then
        Exit;
      I := I + 5;
    end;
    Result := True;
  end;

var
  Output: Boolean;
  NewLoc, ExistLoc: TKMPoint;
  //H: TKMHouse;
begin
  aFieldType := ftRoad;
  ExistLoc := KMPOINT_ZERO;
  NewLoc := fPlannedHouses[aHT].Plans[aIdx].Loc;
  Output := FindClosestHouseEntrance((aHT = htWatchTower), NewLoc, ExistLoc); // Only placed in case of ht_WatchTower (ht_WatchTower are planned at once)
  //H := gHands[fOwner].Houses.FindHouse(ht_Any, NewLoc.X, NewLoc.Y, 1, False); // True = complete house, False = house plan
  //if (H <> nil) then
  //begin
  //  Output := true;
  //  ExistLoc := H.PointBelowEntrance;
  //end;
  if Output AND fRoadPlanner.Route_Make(KMPointBelow(NewLoc), KMPointBelow(ExistLoc), aField) then
  begin
    Output := True;
    ReplaceOverlappingRoad( fPlannedHouses[aHT].Plans[aIdx].Loc );

    if (aHT = htWatchtower) AND not CheckRoadToTowers() then
    begin
      Output := False;
      RemovePlan(aHT,aIdx);
    end;
  end
  else
    RemovePlan(aHT,aIdx);
  Result := Output;
end;


function TKMCityPlanner.GetRoadBetweenPoints(aStart, aEnd: TKMPoint; var aField: TKMPointList; var aFieldType: TKMFieldType): Boolean;
var
  Output: Boolean;
begin
  Output := False;
  aFieldType := ftRoad;
  if fRoadShortcutPlanner.Route_Make(aEnd, aStart, aField) then
    Output := True;
  Result := Output;
end;


function TKMCityPlanner.GetFieldToHouse(aHT: TKMHouseType; aIdx: Integer; var aField: TKMPointList; var aFieldType: TKMFieldType): Boolean;
begin
  Result := True;
  aField.Clear;
  if (aHT = htFarm) then
  begin
    aFieldType := ftCorn;
    PlanFarmFields( fPlannedHouses[aHT].Plans[aIdx].Loc, aField );
  end
  else if (aHT = htWineyard) then
  begin
    aFieldType := ftWine;
    PlanWineFields( fPlannedHouses[aHT].Plans[aIdx].Loc, aField );
  end
  else
    Result := False;
end;


function TKMCityPlanner.GetTreesInHousePlan(aHT: TKMHouseType; aIdx: Integer; var aField: TKMPointList): Byte;
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
    if gTerrain.ObjectIsChopableTree(Point, [caAge1,caAge2,caAge3,caAgeFull]) then
      aField.Add(Point);
  end;
  if (aField.Count > 0) then
    for I := Low(HMA[aHT].Tiles) to High(HMA[aHT].Tiles) do
    begin
      Point := KMPointAdd( fPlannedHouses[aHT].Plans[aIdx].Loc, HMA[aHT].Tiles[I] );
      if (gAIFields.Influences.AvoidBuilding[Point.Y, Point.X] = 0) then
        gAIFields.Influences.AvoidBuilding[Point.Y, Point.X] := 10;
    end;
  Result := aField.Count;
end;


procedure TKMCityPlanner.PlanWineFields(aLoc: TKMPoint; var aNodeList: TKMPointList);
const
  MAX_VINE = 10;
var
  I,Dist: Integer;
  Dir: TDirection;
  HT: TKMHouseType;
  FieldLoc: TKMPoint;
  HMA: THouseMappingArray;
begin
  HT := htWineyard;
  HMA := gAIFields.Eye.HousesMapping;
  for Dist := 1 to 4 do
  begin
    for Dir := Low(HMA[HT].Surroundings[Dist]) to High(HMA[HT].Surroundings[Dist]) do
    for I := Low(HMA[HT].Surroundings[Dist,Dir]) to High(HMA[HT].Surroundings[Dist,Dir]) do
    begin
      FieldLoc := KMPointAdd(aLoc, HMA[HT].Surroundings[Dist,Dir,I]);
      if gTerrain.TileInMapCoords(FieldLoc.X, FieldLoc.Y)                         // Tile must be in map
        AND gHands[fOwner].CanAddFieldPlan(FieldLoc, ftWine)                     // Plan can be placed
        AND (gAIFields.Influences.AvoidBuilding[FieldLoc.Y, FieldLoc.X] <= AVOID_BUILDING_HOUSE_OUTSIDE_LOCK) then // Tile is not reserved
      begin
        aNodeList.Add(FieldLoc);
        gAIFields.Influences.AvoidBuilding[FieldLoc.Y, FieldLoc.X] := AVOID_BUILDING_NODE_LOCK_FIELD;
      end;
      if (aNodeList.Count >= MAX_VINE) then
        Exit;
    end;
  end;
end;


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
  HT: TKMHouseType;
  FieldLoc: TKMPoint;
  HMA: THouseMappingArray;
  PriceArr: TDirArrInt;
  CntArr: TDirArrByte;
begin
  PriceArr := PRICE_ARR_CONST;
  CntArr := CNT_ARR_CONST;
  HT := htFarm;
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
          AND gHands[fOwner].CanAddFieldPlan(FieldLoc, ftCorn)                      // Plan can be placed
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
          AND gHands[fOwner].CanAddFieldPlan(FieldLoc, ftCorn)
          AND (gAIFields.Influences.AvoidBuilding[FieldLoc.Y, FieldLoc.X] <= AVOID_BUILDING_HOUSE_OUTSIDE_LOCK) then
          aNodeList.Add(FieldLoc);
      end;
      if (aNodeList.Count > MAX_FIELDS) then
        Break;
    end;
  end;
end;


function TKMCityPlanner.ObstaclesInHousePlan(aHT: TKMHouseType; aLoc: TKMPoint): Single;
var
  I,X,Y,Road,Tree: Integer;
  HMA: THouseMappingArray;
begin
  Road := 0;
  Tree := 0;
  HMA := gAIFields.Eye.HousesMapping;
  for I := Low(HMA[aHT].Tiles) to High(HMA[aHT].Tiles) do
  begin
    X := aLoc.X + HMA[aHT].Tiles[I].X;
    Y := aLoc.Y + HMA[aHT].Tiles[I].Y;
    Tree := Tree + Byte(gTerrain.ObjectIsChopableTree(KMPoint(X,Y), [caAge1,caAge2,caAge3,caAgeFull]));
    Road := Road + Byte(tpWalkRoad in gTerrain.Land[Y, X].Passability);
  end;
  Result := Tree * GA_PLANNER_ObstaclesInHousePlan_Tree + Road * GA_PLANNER_ObstaclesInHousePlan_Road;
end;


function TKMCityPlanner.FieldCrit(aHT: TKMHouseType; aLoc: TKMPoint): Single;
const
  MIN_CORN_FIELDS = 15;
  MIN_WINE_FIELDS = 9;
  DECREASE_CRIT = 100;
var
  X,Y,I,Dist: Integer;
  Fields, Obstacles: Single;
  Dir: TDirection;
  HMA: THouseMappingArray;
begin
  HMA := gAIFields.Eye.HousesMapping;
  Fields := 0;
  Obstacles := 0;
  for Dist := 1 to (Byte(aHT = htWineyard) shl 1) + (Byte(aHT = htFarm) * 5) do
  for Dir := Low(HMA[aHT].Surroundings[Dist]) to High(HMA[aHT].Surroundings[Dist]) do
  for I := Low(HMA[aHT].Surroundings[Dist,Dir]) + Dist to High(HMA[aHT].Surroundings[Dist,Dir]) - Dist + 1 do
  begin
    X := aLoc.X + HMA[aHT].Surroundings[Dist,Dir,I].X;
    Y := aLoc.Y + HMA[aHT].Surroundings[Dist,Dir,I].Y;
    if gTerrain.TileInMapCoords(X,Y) then
    begin
      if gHands[fOwner].CanAddFieldPlan(KMPoint(X,Y), ftCorn) then
        Fields := Fields + 1;
      //if not gRes.Tileset.TileIsRoadable( gTerrain.Land[Y,X].Terrain ) then
      //  Obstacles := Obstacles + 1;
    end;
  end;
  Result := - GA_PLANNER_FieldCrit_MissingFields * (
              + Max(0, MIN_WINE_FIELDS - Fields) * Byte(aHT = htWineyard) * DECREASE_CRIT
              + Max(0, MIN_CORN_FIELDS - Fields) * Byte(aHT = htFarm) * DECREASE_CRIT + Obstacles
            )
            - gAIFields.Eye.PolygonRoutes[ gAIFields.NavMesh.KMPoint2Polygon[aLoc] ] * GA_PLANNER_FieldCrit_FarmPosition;
end;


function TKMCityPlanner.SnapCrit(aHT: TKMHouseType; aLoc: TKMPoint): Single;
  function IsPlan(aPoint: TKMPoint; aLock: TKMTileLock; aField: TKMFieldType): Boolean;
  begin
    Result := (gHands[fOwner].BuildList.FieldworksList.HasField(aPoint) = aField)
              OR (gTerrain.Land[aPoint.Y, aPoint.X].TileLock = aLock);
  end;
  function IsRoad(aAvoidBuilding: Byte; aPoint: TKMPoint): Boolean; inline;
  begin
    Result := (aAvoidBuilding = AVOID_BUILDING_NODE_LOCK_ROAD)
              OR gTerrain.TileIsWalkableRoad(aPoint)
              OR IsPlan(aPoint, tlRoadWork, ftRoad);
  end;
  function IsCornField(aPoint: TKMPoint): Boolean; inline;
  begin
    Result := gTerrain.TileIsCornField(aPoint) OR IsPlan(aPoint, tlFieldWork, ftCorn);
  end;
  function IsWineField(aPoint: TKMPoint): Boolean; inline;
  begin
    Result := gTerrain.TileIsWineField(aPoint) OR IsPlan(aPoint, tlFieldWork, ftWine);
  end;
  function IsNearHouse(aAvoidBuilding: Byte; aPoint: TKMPoint): Boolean; inline;
  begin
    Result := (aAvoidBuilding = AVOID_BUILDING_HOUSE_OUTSIDE_LOCK)
              OR not (tpBuild in gTerrain.Land[aPoint.Y,aPoint.X].Passability);
  end;
  function IsSomeReservedField(aAvoidBuilding: Byte): Boolean; inline;
  begin
    Result := (aAvoidBuilding = AVOID_BUILDING_NODE_LOCK_FIELD);
  end;
var
  AvoidBuilding: Byte;
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
      AvoidBuilding := gAIFields.Influences.AvoidBuilding[Point.Y, Point.X];
      Output := Output
                + Byte(IsNearHouse(AvoidBuilding,Point)) * GA_PLANNER_SnapCrit_SnapToHouse
                + Byte(IsSomeReservedField(AvoidBuilding)) * GA_PLANNER_SnapCrit_SnapToFields // OR IsCornField(Point) OR IsWineField(Point)
                + Byte(IsRoad(AvoidBuilding,Point)) * GA_PLANNER_SnapCrit_SnapToRoads
                - Byte((Dir = dirS) AND IsSomeReservedField(AvoidBuilding)) * GA_PLANNER_SnapCrit_ClearEntrance;
    end;
  Result := Output;
end;


function TKMCityPlanner.DistCrit(aHT: TKMHouseType; aLoc: TKMPoint): Single;
  function ClosestDistance(): Single;
  const
    MAX_DIST = 1000;
  var
    I: Integer;
    Output, Bid: Single;
    HT: TKMHouseType;
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
    HT: TKMHouseType;
  begin
    Result := 0;
    for HT in HOUSE_DEPENDENCE[aHT] do
      for I := 0 to fPlannedHouses[HT].Count - 1 do
        Result := Result + KMDistanceAbs(aLoc, fPlannedHouses[HT].Plans[I].Loc);
  end;
begin
  if (aHT = htBarracks) then
    Result := - AllDistances()
  else
    Result := - ClosestDistance();
end;


procedure TKMCityPlanner.ClearPerfArr();
var
  X,Y: Integer;
begin
  fPerfIdx := 0;
  for Y := 0 to Length(fPerfArr) - 1 do
    for X := 0 to Length(fPerfArr[Y]) - 1 do
      fPerfArr[Y,X] := 0;
end;


// Faster method for placing house (old in at the end of this file)
//{
function TKMCityPlanner.FindPlaceForHouse(aUnlockProcedure: Boolean; aHT: TKMHouseType; out aBestLocs: TKMPointArray): Byte;
const
  BEST_PLANS_CNT = 8;
  INIT_BEST_BID = -1E20;
var
  Count: Word;
  CityCenter: TKMPoint;
  BestBidArr: array[0..BEST_PLANS_CNT-1] of Double;

  function EvalFreeEntrance(aLoc: TKMPoint): Single;
  const
    OBSTACLE_COEF = 1000;
  var
    I: Integer;
  begin
    Result := 0;
    if (aLoc.Y+2 >= gTerrain.MapY) then
    begin
      Result := OBSTACLE_COEF * 3;
      Exit;
    end;
    for I := -1 to 1 do
      Result := Result + Byte(tpWalk in gTerrain.Land[aLoc.Y+2,aLoc.X+I].Passability) * OBSTACLE_COEF;
  end;

  procedure EvaluateLoc(aLoc: TKMPoint; InitBid: Single);
  var
    L: Integer;
    Bid: Double;
  begin
    Count := Count + 1;
    Bid := - InitBid * GA_PLANNER_FindPlaceForHouse_CloseWorker
           + SnapCrit(aHT, aLoc) * GA_PLANNER_FindPlaceForHouse_SnapCrit
           + DistCrit(aHT, aLoc) * GA_PLANNER_FindPlaceForHouse_DistCrit
           - KMDistanceSqr(CityCenter, aLoc) * GA_PLANNER_FindPlaceForHouse_CityCenter
           - ObstaclesInHousePlan(aHT, aLoc)
           - gAIFields.Influences.GetOtherOwnerships(fOwner, aLoc.X, aLoc.Y) * GA_PLANNER_FindPlaceForHouse_Influence
           + gAIFields.Influences.EvalArea[aLoc.Y, aLoc.X] * GA_PLANNER_FindPlaceForHouse_EvalArea;
    if (aHT = htFarm) OR (aHT = htWineyard) then
      Bid := Bid + FieldCrit(aHT, aLoc)
    else if (aHT = htStore) OR (aHT = htBarracks) then
      Bid := Bid + EvalFreeEntrance(aLoc);
    for L := 0 to BEST_PLANS_CNT - 1 do
      if KMSamePoint(aLoc, aBestLocs[L]) then
        break
      else if (Bid > BestBidArr[L]) then // Insert sort for BEST_PLANS_CNT elements ...
      begin
        KMSwapPoints(aLoc, aBestLocs[L]);
        KMSwapFloat(Bid, BestBidArr[L]);
      end;
  end;

  procedure FindPlaceAroundHType(aHT_HMA: TKMHouseType);
  const
    INFLUENCE_LIMIT = 100;
  var
    I,K, Dist: Integer;
    Bid: Single;
    Dir: TDirection;
    Loc: TKMPoint;
    WorkersPos: TKMPointArray;
    HMA: THouseMappingArray;
    InitBids: TSingleArray;
  begin
    WorkersPos := gHands[fOwner].AI.CityManagement.Builder.WorkersPos;
    HMA := gAIFields.Eye.HousesMapping;
    // Calculate distance of house from closest free workers
    SetLength(InitBids, fPlannedHouses[aHT_HMA].Count);
    for I := 0 to fPlannedHouses[aHT_HMA].Count - 1 do
    begin
      InitBids[I] := 1000000;
      for K := 0 to Length(WorkersPos) - 1 do
      begin
        Bid := KMDistanceSqr(WorkersPos[K], fPlannedHouses[aHT_HMA].Plans[I].Loc);
        if (Bid < InitBids[I]) then
          InitBids[I] := Bid;
      end;
    end;
    for Dist := 2 to MAX_SCAN_DIST_FROM_HOUSE do
    begin
      if (Dist > 2) AND (BestBidArr[BEST_PLANS_CNT-1] <> INIT_BEST_BID) then // When we have full array of possible houses break searching and save time
        break;
      for I := fPlannedHouses[aHT_HMA].Count - 1 downto 0 do // The newest houses have the highest chance that we can place something here so start from newest
        for Dir := Low(HMA[aHT_HMA].Surroundings[Dist]) to High(HMA[aHT_HMA].Surroundings[Dist]) do
          for K := Low(HMA[aHT_HMA].Surroundings[Dist,Dir]) to High(HMA[aHT_HMA].Surroundings[Dist,Dir]) do
          begin
            Loc := KMPointAdd(fPlannedHouses[aHT_HMA].Plans[I].Loc, HMA[aHT_HMA].Surroundings[Dist,Dir,K], HMA[aHT].MoveToEntrance[Dir]);

            if not (gTerrain.TileInMapCoords(Loc.X, Loc.Y, 1))
              OR (fPerfArr[Loc.Y,Loc.X] >= fPerfIdx)
              OR (Dist > 4) AND (gAIFields.Influences.Ownership[fOwner, Loc.Y, Loc.X] < INFLUENCE_LIMIT) then
              continue;

            fPerfArr[Loc.Y,Loc.X] := fPerfIdx;
            if gAIFields.Eye.CanAddHousePlan(Loc, aHT, False, not aUnlockProcedure) then
              EvaluateLoc(Loc, InitBids[I]);
          end;
    end;
  end;

var
  I: Integer;
  Probability: Single;
  HT: TKMHouseType;
  CCPArr: TKMPointArray;
begin
  Result := 0;
  CCPArr := gAIFields.Eye.GetCityCenterPoints(False);
  if (Length(CCPArr) <= 0) then
    Exit;
  CityCenter := CCPArr[0];

  if (fPerfIdx >= 255) then
    ClearPerfArr();
  fPerfIdx := fPerfIdx + 1;

  Count := 0;
  SetLength(aBestLocs, BEST_PLANS_CNT);
  for I := 0 to BEST_PLANS_CNT - 1 do
    BestBidArr[I] := INIT_BEST_BID;

  for HT in HOUSE_DEPENDENCE[aHT] do
    FindPlaceAroundHType(HT);

  // Probability will change in dependence on count of avaiable plans
  Probability := (BEST_PLANS_CNT - Min(Count, BEST_PLANS_CNT-1)) / (BEST_PLANS_CNT*1.0); // 1 <> 0.125
  for HT := HOUSE_MIN to HOUSE_MAX do
    if (HT <> htWatchTower)
      AND (fPlannedHouses[HT].Count > 0)
      AND (KaMRandom() < Probability)
      AND not (HT in HOUSE_DEPENDENCE[aHT]) then
      FindPlaceAroundHType(HT);

  for I := High(BestBidArr) downto Low(BestBidArr) do
    if (BestBidArr[I] <> INIT_BEST_BID) then
      break;
  Result := Min(Count, BEST_PLANS_CNT);
end;
//}

function TKMCityPlanner.FindPlaceForMines(aHT: TKMHouseType): Boolean;
const
  MAX_LOCS = 5;

  // Get closest mine
  function FindPlaceForMine(aMine: TKMHouseType): Boolean;
  const
    BEST_BID = -10000;
  var
    Output, Check: Boolean;
    I, K, BestIdx: Integer;
    Bid, BestBid: Single;
    Loc: TKMPoint;
    Locs: TKMPointTagList;
  begin
    Output := False;
    Locs := gAIFields.Eye.GetMineLocs(aMine);
    try
      if (Locs.Count > 0) then
      begin
        Locs.SortByTag();
        BestBid := BEST_BID;
        BestIdx := 0; // For compiler
        for I := Locs.Count-1 downto 0 do
        begin
          // Check reserved mines
          Check := True;
          for K := 0 to fPlannedHouses[aMine].Count - 1 do
          begin
            Loc := fPlannedHouses[aMine].Plans[K].Loc;
            if KMSamePoint(Loc, Locs.Items[I])
              AND not ( (Loc.Y <> Locs.Items[I].Y) OR (Abs(Loc.X - Locs.Items[I].X) > (3 + Byte(aMine = htIronMine))) ) then
            begin
              Check := False;
              continue;
            end;
          end;
          if not Check then
            continue;
          Bid := Locs.Tag[I] + DistCrit(aMine, Locs.Items[I]) * 4;
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
    for I := Low(HMA[htCoalMine].Tiles) to High(HMA[htCoalMine].Tiles) do
    begin
      X := aCoalLoc.X + HMA[htCoalMine].Tiles[I].X;
      Y := aCoalLoc.Y + HMA[htCoalMine].Tiles[I].Y;
      Output := Output + Byte(gTerrain.TileIsCoal(X, Y) > 1);
    end;
    Result := Output;
  end;

  // Coal mine planner
  function FindPlaceForCoalMine(): Boolean;
  const
    INIT_BID = -10000;
  var
    Output: Boolean;
    I, BestIdx: Integer;
    Bid, BestBid: Single;
    Locs: TKMPointTagList;
  begin
    Output := False;

    Locs := gAIFields.Eye.GetCoalMineLocs(False);
    try
      if (Locs.Count > 0) then
      begin
        BestBid := INIT_BID;
        BestIdx := 0; // For compiler
        for I := 0 to Locs.Count - 1 do
        begin
          Bid := + Locs.Tag[I]
                 + DistCrit(htCoalMine, Locs.Items[I])
                 + SnapCrit(htCoalMine, Locs.Items[I])
                 - ObstaclesInHousePlan(htCoalMine, Locs.Items[I])
                 - gAIFields.Influences.GetOtherOwnerships(fOwner, Locs.Items[I].X, Locs.Items[I].Y);
          if (Bid > BestBid) then
          begin
            BestIdx := I;
            BestBid := Bid;
          end;
        end;
        if (BestBid <> INIT_BID) then
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
    for I := 0 to fPlannedHouses[htQuary].Count - 1 do
    begin
      Dist := KMDistanceAbs(aLoc, fPlannedHouses[htQuary].Plans[I].Loc);
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
    if (fPerfIdx >= 255) then
      ClearPerfArr();
    fPerfIdx := fPerfIdx + 1;

    Locs := gAIFields.Eye.GetStoneLocs();
    try
      // Calculate criterium = ownership + actual Tag2 (= used by other miners) + owner criterium
      for I := 0 to Locs.Count - 1 do
        Locs.Tag[I] := Max(0, 100000 - Locs.Tag[I]
                                     + gAIFields.Influences.GetOtherOwnerships(fOwner,Locs.Items[I].X,Locs.Items[I].Y)
                                     - Round(DistCrit(htQuary, Locs.Items[I])) * 50);
      Locs.SortByTag();
      // Find best loc for a Quary
      BestBid := -100000;
      for I := 0 to Locs.Count - 1 do
      begin
        for Y := Max(1,Locs.Items[I].Y-SCAN_RAD) to Min(Locs.Items[I].Y+SCAN_RAD,gTerrain.MapY-1) do
          for X := Max(1,Locs.Items[I].X-SCAN_RAD) to Min(Locs.Items[I].X+SCAN_RAD,gTerrain.MapX-1) do
            if (fPerfArr[Y,X] <> fPerfIdx) then
            begin
              fPerfArr[Y,X] := fPerfIdx;
              Loc := KMPoint(X,Y);
              if gAIFields.Eye.CanAddHousePlan(Loc, htQuary, False, False) then
              begin
                Bid := + DistCrit(htQuary, Loc) * 40 + SnapCrit(htQuary, Loc);
                if (Bid > BestBid) then
                begin
                  BestBid := Bid;
                  BestLoc := Loc;
                  Output := True;
                end;
              end;
            end;
        if Output AND (I >= min(5,Locs.Count-1)) then // Do several points
        begin
          AddPlan(aHT, BestLoc);
          gHands[fOwner].AI.CityManagement.Builder.LockHouseLoc(aHT, BestLoc);
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
    htGoldMine:  Output := FindPlaceForMine(htGoldMine);
    htIronMine:  Output := FindPlaceForMine(htIronMine);
    htCoalMine:  Output := FindPlaceForCoalMine();
    htQuary:     Output := FindPlaceForQuary();
    else          Output := False;
  end;
  Result := Output;
end;


// Find place for woodcutter
function TKMCityPlanner.FindPlaceForWoodcutter(aCenter: TKMPoint; aChopOnly: Boolean = False): Boolean;
const
  RADIUS = 5;
  MIN_BID = -100000;
var
  Output, Check: Boolean;
  I,X,Y: Integer;
  Bid, BestBid: Single;
  Loc, BestLoc, Point: TKMPoint;
  HMA: THouseMappingArray;
begin
  Output := False;
  HMA := gAIFields.Eye.HousesMapping;
  BestBid := MIN_BID;
  for Y := Max(1, aCenter.Y - RADIUS) to Min(gTerrain.MapY, aCenter.Y + RADIUS) do
  for X := Max(1, aCenter.X - RADIUS) to Min(gTerrain.MapX, aCenter.X + RADIUS) do
  begin
    Loc := KMPoint(X,Y);
    if gAIFields.Eye.CanAddHousePlan(Loc, htWoodcutters, not aChopOnly, False, False) then
    //if gAIFields.Eye.CanAddHousePlan(Loc, ht_Woodcutters, True, False, False) then
    begin
      Bid := - GA_PLANNER_PlaceWoodcutter_DistFromForest * KMDistanceAbs(aCenter, Loc)
             + DistCrit(htWoodcutters, Loc)
             + SnapCrit(htWoodcutters, Loc);
             //- Byte(gAIFields.Influences.AvoidBuilding[Loc.Y, Loc.X] >= AVOID_BUILDING_COAL_TILE) * 100000; // Try not to place woodcutter into full forest or coal tile
      if (Bid > BestBid) then
      begin
        Check := True;
        for I := Low(HMA[htWoodcutters].Tiles) to High(HMA[htWoodcutters].Tiles) do
        begin
          Point := KMPointAdd( Loc, HMA[htWoodcutters].Tiles[I] );
          if (gTerrain.TileIsCoal(Point.X, Point.Y) <> 0) then
          begin
            Check := False;
            break;
          end;
        end;
        if Check then
        begin
          BestBid := Bid;
          BestLoc := Loc;
        end;
      end;
    end;
  end;
  if (BestBid <> MIN_BID) then
  begin
    Output := True;
    // Check whether is cutting point (center of forest) inside of house plan and in this case set it 1 point on left from house entrance
    if ((aCenter.Y <= BestLoc.Y) AND (aCenter.Y >= BestLoc.Y-1)) AND ((aCenter.X <= BestLoc.X) AND (aCenter.X >= BestLoc.X-2)) then
      aCenter := KMPoint(BestLoc.X-1, BestLoc.Y+1);
    AddPlan(htWoodcutters, BestLoc, aCenter, aChopOnly);
    if aChopOnly then // Lock chop only woodcutter (it can be added in next tick and it is placed out of avoid building areas)
      gHands[fOwner].AI.CityManagement.Builder.LockHouseLoc(htWoodcutters, BestLoc);
  end;
  Result := Output;
end;


function TKMCityPlanner.FindForestAndWoodcutter(): Boolean;

  function EnoughTreeSeedTiles(aLoc: TKMPoint): Boolean;
  const
    RADIUS = 3;
    MIN_TREES_TILES = 15;
  var
    X,Y,Cnt: Integer;
  begin
    Cnt := 0;
    for Y := Max(1,aLoc.Y-RADIUS) to Min(gTerrain.MapY-1, aLoc.Y+RADIUS) do
    for X := Max(1,aLoc.X-RADIUS) to Min(gTerrain.MapX-1, aLoc.X+RADIUS) do
      if gRes.Tileset.TileIsSoil( gTerrain.Land[Y, X].Terrain ) then
        Cnt := Cnt + 1;
    Result := Cnt >= MIN_TREES_TILES;
  end;

const
  BLOCK_RAD = 8.0;
  SQR_MIN_DIST_FROM_CHOP_ONLY = 12*12;
  SQR_DEC_SPEED = AVOID_BUILDING_FOREST_RANGE / (BLOCK_RAD * BLOCK_RAD);
var
  Output, PartOfForest: Boolean;
  I,K: Integer;
  Point: TKMPoint;
begin
  Output := False;

  gAIFields.Eye.GetForests(fForestsNearby);
  // Delete used forests (used by woodcutter - no matter if in chop only or chop and plant mode)
  for I := fForestsNearby.Count-1 downto 0 do
    if (gAIFields.Influences.AvoidBuilding[  fForestsNearby.Items[I].Y, fForestsNearby.Items[I].X  ] >= 250)
      OR not EnoughTreeSeedTiles( fForestsNearby.Items[I] ) then
      fForestsNearby.Delete(I)
    else
      for K := 0 to fPlannedHouses[htWoodcutters].Count - 1 do
      begin
        Point := fPlannedHouses[htWoodcutters].Plans[K].SpecPoint;
        if (gAIFields.Influences.AvoidBuilding[Point.Y, Point.X] < AVOID_BUILDING_FOREST_MINIMUM) // Chop-only mode
          AND (KMDistanceSqr(fForestsNearby.Items[I], Point) < SQR_MIN_DIST_FROM_CHOP_ONLY) then
        begin
          fForestsNearby.Delete(I);
          break;
        end;
      end;

  // Evaluate clusters of trees (tree cnt + relative position [influence])
  for I := fForestsNearby.Count-1 downto 0 do
  begin
    Point := fForestsNearby.Items[I];
    PartOfForest := Boolean(fForestsNearby.Tag2[I]);
    fForestsNearby.Tag2[I] := fForestsNearby.Tag[I];
    fForestsNearby.Tag[I] := Max(0, Round(
                                  + 1000000 // Base price
                                  + fForestsNearby.Tag[I] * GA_PLANNER_FindPlaceForWoodcutter_TreeCnt
                                  - gAIFields.Eye.PolygonRoutes[ gAIFields.NavMesh.KMPoint2Polygon[Point] ] * GA_PLANNER_FindPlaceForWoodcutter_PolyRoute
                                  + Byte(PartOfForest) * GA_PLANNER_FindPlaceForWoodcutter_ExistForest
                                  - gAIFields.Influences.EvalArea[Point.Y, Point.X] * GA_PLANNER_FindPlaceForWoodcutter_EvalArea
                                  + gAIFields.Influences.Ownership[fOwner, Point.Y, Point.X] * GA_PLANNER_FindPlaceForWoodcutter_DistCrit
                                  - gAIFields.Influences.GetOtherOwnerships(fOwner, Point.X, Point.Y) * GA_PLANNER_FindPlaceForWoodcutter_Influence
                                ));
  end;

  fForestsNearby.SortByTag();

  I := fForestsNearby.Count;
  while not Output AND (I > 0) do
  begin
    I := I - 1;
    Point := fForestsNearby.Items[I]; // Get the best forest
    Output := FindPlaceForWoodcutter(Point);
    if Output then
      gAIFields.Influences.AddAvoidBuilding(Point.X, Point.Y, BLOCK_RAD, 255, True, SQR_DEC_SPEED);
  end;
  Result := Output;
end;


function TKMCityPlanner.FindForestAround(const aPoint: TKMPoint; aCountByInfluence: Boolean = False): Boolean;
const
  MIN_TREES = 3;
  MAX_DIST = 10;
  SQR_MIN_DIST_FROM_ACTIVE_FORESTS = 10*10;
  SQR_MIN_DIST_FROM_CHOP_ONLY = 12*12;
var
  Output, Check: Boolean;
  I,K: Integer;
  Bid, BestBid: Byte;
  Loc, BestLoc: TKMPoint;
begin
  Output := False;

  if (fForestsNearby <> nil) then
  begin
    BestBid := 0;
    for I := fForestsNearby.Count-1 downto 0 do
    begin
      Loc := fForestsNearby.Items[I];
      if (aCountByInfluence OR (KMDistanceAbs(aPoint, Loc) < MAX_DIST))
        AND (fForestsNearby.Tag2[I] >= MIN_TREES)
        AND (gAIFields.Influences.AvoidBuilding[ Loc.Y, Loc.X ] < AVOID_BUILDING_FOREST_MINIMUM) then
      begin
        Bid := gAIFields.Influences.Ownership[ fOwner, Loc.Y, Loc.X ]; // This is equivalent of distance
        if (Bid > BestBid) then
        begin
          BestBid := Bid;
          Check := True;
          for K := 0 to fPlannedHouses[htWoodcutters].Count - 1 do
            if (KMDistanceSqr(Loc, fPlannedHouses[htWoodcutters].Plans[K].SpecPoint) < SQR_MIN_DIST_FROM_ACTIVE_FORESTS) then
            begin
              Check := False;
              break;
            end;
          if Check then
          begin
            BestLoc := fForestsNearby.Items[I];
            Output := True;
          end;
        end;
      end;
    end;
    if Output then
    begin
      Output := FindPlaceForWoodcutter(BestLoc, True);
      // fForests is not updated so remove points around new chop only forest
      if Output then
      begin
        for I := fForestsNearby.Count-1 downto 0 do
          if (KMDistanceSqr(fForestsNearby.Items[I], BestLoc) < SQR_MIN_DIST_FROM_CHOP_ONLY) then
            fForestsNearby.Delete(I);
      end;
    end;
  end;

  Result := Output;
end;


function TKMCityPlanner.PlanDefenceTowers(): Boolean;

  procedure FindPlaceForTowers(aCenter: TKMPoint);
  const
    RADIUS = 3;
    MAX_BID = 100000;
    MAX_ENEMY_INFLUENCE = 100;
  var
    PL: TKMHandIndex;
    X,Y: Integer;
    Bid, BestBid: Single;
    Loc, BestLoc: TKMPoint;
  begin
    // Filter defence positions, build towers only at the closest
    PL := gAIFields.Influences.GetBestAllianceOwner(fOwner, aCenter, at_Ally);
    if (PL <> fOwner) AND (PL <> PLAYER_NONE) then
      Exit;
    if (gAIFields.Influences.GetBestAllianceOwnership(fOwner, gAIFields.NavMesh.Point2Polygon[aCenter.Y,aCenter.X], at_Enemy) > MAX_ENEMY_INFLUENCE) then
      Exit;

    BestBid := MAX_BID;
    for Y := Max(1, aCenter.Y - RADIUS) to Min(gTerrain.MapY, aCenter.Y + RADIUS) do
    for X := Max(1, aCenter.X - RADIUS) to Min(gTerrain.MapX, aCenter.X + RADIUS) do
    begin
      Loc := KMPoint(X,Y);
      if gAIFields.Eye.CanAddHousePlan(Loc, htWatchTower, True, False, False) then
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
    begin
      AddPlan(htWatchTower, BestLoc);
      gHands[fOwner].AI.CityManagement.Builder.LockHouseLoc(htWatchTower, BestLoc);
    end;
  end;

const
  DISTANCE_BETWEEN_TOWERS = 5.5;
var
  I, K, DefCount: Integer;
  P1,P2: TKMPoint;
  Ratio: Single;
  DefLines: TKMDefenceLines;
begin
  Result := False;

  if not gHands[fOwner].Locks.HouseCanBuild(htWatchTower)
    OR not gAIFields.NavMesh.Defences.FindDefenceLines(fOwner, DefLines)
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
  COLOR_WHITE = $FFFFFF;
  COLOR_BLACK = $000000;
  COLOR_GREEN = $00FF00;
  COLOR_RED = $0000FF;
  COLOR_YELLOW = $00FFFF;
  COLOR_BLUE = $FF0000;
var
  I,K: Integer;
  Division: Single;
  HT: TKMHouseType;
  Loc: TKMPoint;
  Color: Cardinal;
  HMA: THouseMappingArray;
begin
  HMA := gAIFields.Eye.HousesMapping;

  {
  for I := 1 to Length(fPerfArr) - 1 do
    for K := 1 to Length(fPerfArr[I]) - 1 do
      if (fPerfArr[I,K] = fPerfIdx) then
        gRenderAux.Quad(K, I, $60000000 OR COLOR_BLUE);
  //}

  // Paint houses
  for HT := HOUSE_MIN to HOUSE_MAX do
  begin
    case HT of
      htStore,htSchool,htInn,htMarketplace: Color := COLOR_BLACK;
      htQuary,htWoodcutters,htSawmill: Color := COLOR_BLUE;
      htGoldMine,htCoalMine,htIronMine,htMetallurgists: Color := COLOR_YELLOW;
      htIronSmithy,htArmorSmithy,htWeaponSmithy,htTannery,htArmorWorkshop,htWeaponWorkshop,htBarracks: Color := COLOR_RED;
      htBakery,htButchers,htMill,htSwine,htStables,htFarm,htWineyard: Color := COLOR_GREEN;
      else Color := COLOR_WHITE;
    end;
    Color := $80000000 OR Color;
    for I := 0 to fPlannedHouses[HT].Count - 1 do
    begin
      for K := 0 to Length(HMA[HT].Tiles) - 1 do
      begin
        Loc := KMPointAdd(fPlannedHouses[HT].Plans[I].Loc, HMA[HT].Tiles[K]);
        gRenderAux.Quad(Loc.X, Loc.Y, Color);
      end;
    end;
  end;

  // Paint potential forests
  if (fForestsNearby <> nil) AND (fForestsNearby.Count > 0) then
  begin
    Division := 1 / (fForestsNearby.Tag[fForestsNearby.Count - 1] - fForestsNearby.Tag[0]) * 255.0;
    for I := 0 to fForestsNearby.Count - 1 do
    begin
      Loc := fForestsNearby.Items[I];
      Color := (Max(50,Round((fForestsNearby.Tag[I] - fForestsNearby.Tag[0]) * Division)) shl 24) OR $000000FF;
      gRenderAux.Quad(Loc.X, Loc.Y, Color);
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
  Result := GA_PATHFINDING_BasePrice;
  AvoidBuilding := gAIFields.Influences.AvoidBuilding[aToY, aToX];
  IsRoad := (AvoidBuilding = AVOID_BUILDING_NODE_LOCK_ROAD)                                      // Reserved road plan
            OR (tpWalkRoad in gTerrain.Land[aToY, aToX].Passability)                             // Completed road
            OR (gHands[fOwner].BuildList.FieldworksList.HasField(KMPoint(aToX, aToY)) = ftRoad) // Placed road plan
            OR (gTerrain.Land[aToY, aToX].TileLock = tlRoadWork);                                // Road under construction

  if not IsRoad then
    //Building roads over fields is discouraged unless unavoidable
    case AvoidBuilding of
      AVOID_BUILDING_HOUSE_OUTSIDE_LOCK: begin Inc(Result, GA_PATHFINDING_HouseOutside); end; // 1 tile from future house
      AVOID_BUILDING_NODE_LOCK_FIELD: Inc(Result, GA_PATHFINDING_Field); // Corn / wine field
      AVOID_BUILDING_COAL_TILE: Inc(Result, GA_PATHFINDING_Coal);
      //AVOID_BUILDING_HOUSE_INSIDE_LOCK: begin end; // Tiles inside future house (forbiden)
      //AVOID_BUILDING_NODE_LOCK_ROAD: begin end; // This will not occur
      //AVOID_BUILDING_FOREST_MINIMUM: begin end;
      else
      begin
        // Snap to no-build areas (1 tile from house)
        if not (tpBuild in gTerrain.Land[aToY,aToX].Passability) then
          Inc(Result, GA_PATHFINDING_noBuildArea)
        else if (AvoidBuilding > AVOID_BUILDING_FOREST_MINIMUM) then // Forest or coal etc.
          Inc(Result, GA_PATHFINDING_Forest)
        else
          Inc(Result, GA_PATHFINDING_OtherCase);
      end;
    end;
end;


{ TPathFindingShortcutsCityPlanner }
function TPathFindingShortcutsCityPlanner.MovementCost(aFromX, aFromY, aToX, aToY: Word): Word;
var
  IsRoad: Boolean;
  AvoidBuilding: Byte;
begin
  Result := GA_SHORTCUTS_BasePrice;
  AvoidBuilding := gAIFields.Influences.AvoidBuilding[aToY, aToX];
  IsRoad := (AvoidBuilding = AVOID_BUILDING_NODE_LOCK_ROAD)                                     // Reserved road plan
            OR (tpWalkRoad in gTerrain.Land[aToY, aToX].Passability)                            // Completed road
            OR (gHands[fOwner].BuildList.FieldworksList.HasField(KMPoint(aToX, aToY)) = ftRoad) // Placed road plan
            OR (gTerrain.Land[aToY, aToX].TileLock = tlRoadWork);                               // Road under construction

  if not IsRoad then
    //Building roads over fields is discouraged unless unavoidable
    case AvoidBuilding of
      AVOID_BUILDING_HOUSE_OUTSIDE_LOCK: begin Inc(Result, GA_SHORTCUTS_HouseOutside); end; // 1 tile from future house
      AVOID_BUILDING_NODE_LOCK_FIELD: Inc(Result, GA_SHORTCUTS_Field); // Corn / wine field
      AVOID_BUILDING_COAL_TILE: Inc(Result, GA_SHORTCUTS_Coal);
      //AVOID_BUILDING_HOUSE_INSIDE_LOCK: begin end; // Tiles inside future house (forbiden)
      //AVOID_BUILDING_NODE_LOCK_ROAD: begin end; // This will not occur
      //AVOID_BUILDING_FOREST_MINIMUM: begin end;
      else
      begin
        // Snap to no-build areas (1 tile from house)
        if not (tpBuild in gTerrain.Land[aToY,aToX].Passability) then
          Inc(Result, GA_SHORTCUTS_noBuildArea)
        else if (AvoidBuilding > AVOID_BUILDING_FOREST_MINIMUM) then // Forest or coal etc.
          Inc(Result, GA_SHORTCUTS_Forest)
        else
          Inc(Result, GA_PATHFINDING_OtherCase);
      end;
    end;
end;


function TPathFindingShortcutsCityPlanner.DestinationReached(aX, aY: Word): Boolean;
begin
  Result := ((aX = fLocB.X) and (aY = fLocB.Y)); //We reached destination point
end;



{ JUNK:


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

{
function TKMCityPlanner.DistCrit(aHT: TKMHouseType; aLoc: TKMPoint): Single;
const
  MAX_BID = 1000000;

  function DistFromHouses(aHTs: array of TKMHouseType): Single;
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

  function DistFromHouse(aHTs: array of TKMHouseType): Single;
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
function TKMCityPlanner.FindPlaceForHouse(aUnlockProcedure: Boolean; aHT: TKMHouseType; out aBestLocs: TKMPointArray): Byte;
const
  BEST_PLANS_CNT = 5;
  INIT_BEST_BID = -1000000;
var
  I,K,L,Dist: Integer;
  Dir: TDirection;
  HType: TKMHouseType;
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

{
function GetBlockingTrees(aHT: TKMHouseType; aLoc: TKMPoint; var aTrees: TKMPointArray): Boolean;
function GetBlockingFields(aHT: TKMHouseType; aLoc: TKMPoint; var aFields: TKMPointArray): Boolean;

function TKMCityPlanner.GetBlockingTrees(aHT: TKMHouseType; aLoc: TKMPoint; var aTrees: TKMPointArray): Boolean;
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
    if gTerrain.ObjectIsChopableTree(KMPoint(X,Y), [caAge1,caAge2,caAge3,caAgeFull]) then
    begin
      aTrees[TreeCnt] := KMPoint(X,Y);
      TreeCnt := TreeCnt + 1;
    end;
    if not Output then
      break;
  end;
  Result := Output;
end;


function TKMCityPlanner.GetBlockingFields(aHT: TKMHouseType; aLoc: TKMPoint; var aFields: TKMPointArray): Boolean;
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
  HT: TKMHouseType;
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
  HT: TKMHouseType;
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


end.
