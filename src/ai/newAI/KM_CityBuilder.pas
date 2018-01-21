unit KM_CityBuilder;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_CommonUtils, KM_Points, KM_CommonTypes,
  KM_PathfindingRoad, //KM_AISetup,
  KM_ResHouses, KM_BuildList,
  KM_AIInfluences, KM_CityPlanner, KM_CityPredictor, KM_Eye;

type

  TBuildNode = record
    Active, RemoveTreesMode, ShortcutMode: Boolean;
    FreeWorkers, RequiredWorkers, MaxReqWorkers: Integer;
    HouseType: THouseType;
    CenterPoint, HouseLoc: TKMPoint;
    FieldType: TFieldType; //ft_Corn, ft_Wine, ft_Road
    FieldList: TKMPointList;
  end;

  TConstructionState = (cs_NoNodeAvailable, cs_HousePlaced, cs_CannotPlaceHouse, cs_HouseReservation, cs_RemoveTreeProcedure);

  //
  TKMCityBuilder = class
  private
    fOwner: TKMHandIndex;
    fBuildNodes: array of TBuildNode;

    fPlanner: TKMCityPlanner;
    fPredictor: TKMCityPredictor;

    procedure UpdateBuildNode(var aNode: TBuildNode);

    function BuildHouse(aUnlockProcedure, aHouseReservation: Boolean; aHT: THouseType): TConstructionState;
    function BuildHouse_GA_MODE(aHT: THouseType): TConstructionState;
    procedure LockNode(var aNode: TBuildNode);
    procedure UnlockPointOfNode(aPoint: TKMPoint); inline;
    procedure UnlockNode(var aNode: TBuildNode);

    procedure CreateShortcuts();
  public
    constructor Create(aPlayer: TKMHandIndex; aPredictor: TKMCityPredictor);
    destructor Destroy(); override;

    property Planner: TKMCityPlanner read fPlanner;

    procedure AfterMissionInit(out aGoldMineCnt, aIronMineCnt, aFieldCnt, aBuildCnt: Integer);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);

    procedure UpdateState(aTick: Cardinal; out aFreeWorkersCnt: Integer);
    procedure UpdateBuildNodes(out aFreeWorkersCnt: Integer);
    function ChooseHousesToBuild(aMaxCnt: Integer): Boolean;

    procedure LockHouseLoc(aHT: THouseType; aLoc: TKMPoint);
    procedure UnlockHouseLoc(aHT: THouseType; aLoc: TKMPoint);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure LogStatus(var aBalanceText: UnicodeString);
    procedure Paint();
  end;


implementation
uses
  KM_Game, KM_Houses, KM_HouseCollection, KM_HouseSchool, KM_HandsCollection, KM_Hand, KM_Terrain, KM_Resource,
  KM_ResWares, KM_AIFields, KM_Units, KM_UnitTaskDelivery, KM_UnitActionWalkTo, KM_UnitTaskGoEat, KM_UnitsCollection,
  KM_NavMesh, KM_HouseMarket, KM_RenderAux, KM_ResMapElements;

{ TKMCityBuilder }
constructor TKMCityBuilder.Create(aPlayer: TKMHandIndex; aPredictor: TKMCityPredictor);
begin
  inherited Create;

  fOwner := aPlayer;
  fPredictor := aPredictor;
  fPlanner := TKMCityPlanner.Create(aPlayer);
end;


destructor TKMCityBuilder.Destroy;
var
  I: Integer;
begin
  fPlanner.Free;
  for I := Low(fBuildNodes) to High(fBuildNodes) do
    fBuildNodes[I].FieldList.Free;
  inherited;
end;


procedure TKMCityBuilder.Save(SaveStream: TKMemoryStream);
var
  I, Cnt: Integer;
begin
  SaveStream.WriteA('CityBuilder');
  SaveStream.Write(fOwner);

  Cnt := Length(fBuildNodes);
  SaveStream.Write(Cnt);
  for I := 0 to Cnt - 1 do
    with fBuildNodes[I] do
    begin
      SaveStream.Write(Active);
      SaveStream.Write(RemoveTreesMode);
      SaveStream.Write(ShortcutMode);
      SaveStream.Write(FreeWorkers);
      SaveStream.Write(RequiredWorkers);
      SaveStream.Write(MaxReqWorkers);
      SaveStream.Write(CenterPoint, SizeOf(CenterPoint));
      SaveStream.Write(HouseType, SizeOf(HouseType));
      SaveStream.Write(HouseLoc, SizeOf(HouseLoc));
      SaveStream.Write(FieldType, SizeOf(TFieldType));
      FieldList.SaveToStream(SaveStream);
    end;

  fPlanner.Save(SaveStream);
end;


procedure TKMCityBuilder.Load(LoadStream: TKMemoryStream);
var
  I, Cnt: Integer;
begin
  LoadStream.ReadAssert('CityBuilder');
  LoadStream.Read(fOwner);

  LoadStream.Read(Cnt);
  SetLength(fBuildNodes, Cnt);
  for I := 0 to Cnt - 1 do
  with fBuildNodes[I] do
  begin
    LoadStream.Read(Active);
    LoadStream.Read(RemoveTreesMode);
    LoadStream.Read(ShortcutMode);
    LoadStream.Read(FreeWorkers);
    LoadStream.Read(RequiredWorkers);
    LoadStream.Read(MaxReqWorkers);
    LoadStream.Read(CenterPoint, SizeOf(CenterPoint));
    LoadStream.Read(HouseType, SizeOf(HouseType));
    LoadStream.Read(HouseLoc, SizeOf(HouseLoc));
    LoadStream.Read(FieldType, SizeOf(TFieldType));
    FieldList := TKMPointList.Create();
    FieldList.LoadFromStream(LoadStream);
  end;

  fPlanner.Load(LoadStream);
end;


procedure TKMCityBuilder.AfterMissionInit(out aGoldMineCnt, aIronMineCnt, aFieldCnt, aBuildCnt: Integer);
var
  I: Integer;
  U: TKMUnit;
begin
  fPlanner.AfterMissionInit();
  //SetLength(fBuildNodes, gHands[fOwner].AI.Setup.WorkerCount shr 1);
  SetLength(fBuildNodes, gHands[fOwner].AI.Setup.WorkerCount);
  for I := Low(fBuildNodes) to High(fBuildNodes) do
  begin
    fBuildNodes[I].FieldList := TKMPointList.Create();
    fBuildNodes[I].Active := False;
  end;
  // Remove units when is game in GA mode (avoid to place houses at unit)
  if GA_PLANNER then
    for I := 0 to gHands[fOwner].Units.Count - 1 do
    begin
      U := gHands[fOwner].Units[I];
      if (U <> nil) then
        U.KillUnit(fOwner, False, False);
    end;
end;


procedure TKMCityBuilder.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
  fPlanner.OwnerUpdate(aPlayer);
end;


procedure TKMCityBuilder.UpdateState(aTick: Cardinal; out aFreeWorkersCnt: Integer);
var
  Outline1, Outline2: TKMWeightSegments;
begin
  if (aTick mod MAX_HANDS = fOwner) then
  begin
    fPlanner.UpdateState(aTick); // Planner must be updated as first to secure that completed houses are actualized
    UpdateBuildNodes(aFreeWorkersCnt);
  end;
  ////((gGame.GameOptions.Peacetime <> 0) and gGame.CheckTime(600 * Max(0, gGame.GameOptions.Peacetime - 15)))
  //if (aTick = 50) then
  //  gAIFields.NavMesh.GetDefenceOutline(fOwner, Outline1, Outline2);
end;


procedure TKMCityBuilder.LockHouseLoc(aHT: THouseType; aLoc: TKMPoint);
var
  I,Dist: Integer;
  Point: TKMPoint;
  Dir: TDirection;
  HMA: THouseMappingArray;
begin
  HMA := gAIFields.Eye.HousesMapping;
  // Reserve all tiles inside house plan
  for I := Low(HMA[aHT].Tiles) to High(HMA[aHT].Tiles) do
    gAIFields.Influences.AvoidBuilding[aLoc.Y + HMA[aHT].Tiles[I].Y, aLoc.X + HMA[aHT].Tiles[I].X] := AVOID_BUILDING_HOUSE_INSIDE_LOCK;
  // Reserve all tiles in distance 1 from house plan
  Dist := 1;
  for Dir := Low(HMA[aHT].Surroundings[Dist]) to High(HMA[aHT].Surroundings[Dist]) do
    for I := Low(HMA[aHT].Surroundings[Dist,Dir]) to High(HMA[aHT].Surroundings[Dist,Dir]) do
    begin
      Point := KMPointAdd(aLoc, HMA[aHT].Surroundings[Dist,Dir,I]);
      if (gAIFields.Influences.AvoidBuilding[Point.Y, Point.X] < AVOID_BUILDING_HOUSE_INSIDE_LOCK) then
        gAIFields.Influences.AvoidBuilding[Point.Y, Point.X] := AVOID_BUILDING_HOUSE_OUTSIDE_LOCK;
    end;
end;


procedure TKMCityBuilder.UnlockHouseLoc(aHT: THouseType; aLoc: TKMPoint);
var
  I,Dist: Integer;
  Point: TKMPoint;
  Dir: TDirection;
  HMA: THouseMappingArray;
begin
  HMA := gAIFields.Eye.HousesMapping;
  // Free all tiles inside house plan
  for I := Low(HMA[aHT].Tiles) to High(HMA[aHT].Tiles) do
    gAIFields.Influences.AvoidBuilding[aLoc.Y + HMA[aHT].Tiles[I].Y, aLoc.X + HMA[aHT].Tiles[I].X] := AVOID_BUILDING_UNLOCK;
  // Free all tiles in distance 1 from house plan
  Dist := 1;
  for Dir := Low(HMA[aHT].Surroundings[Dist]) to High(HMA[aHT].Surroundings[Dist]) do
    for I := Low(HMA[aHT].Surroundings[Dist,Dir]) to High(HMA[aHT].Surroundings[Dist,Dir]) do
    begin
      Point := KMPointAdd(aLoc, HMA[aHT].Surroundings[Dist,Dir,I]);
      if (gAIFields.Influences.AvoidBuilding[Point.Y, Point.X] = AVOID_BUILDING_HOUSE_OUTSIDE_LOCK) then
        gAIFields.Influences.AvoidBuilding[Point.Y, Point.X] := AVOID_BUILDING_UNLOCK;
    end;
end;


procedure TKMCityBuilder.LockNode(var aNode: TBuildNode);
var
  NODE_TYPE: Byte;
  I: Integer;
begin
  case aNode.FieldType of
    ft_Road: NODE_TYPE := AVOID_BUILDING_NODE_LOCK_ROAD;
    else     NODE_TYPE := AVOID_BUILDING_NODE_LOCK_FIELD;
  end;
  with aNode.FieldList do
    for I := 0 to Count-1 do
      if (gAIFields.Influences.AvoidBuilding[Items[I].Y, Items[I].X] < NODE_TYPE) then
        gAIFields.Influences.AvoidBuilding[Items[I].Y, Items[I].X] := NODE_TYPE;
end;


procedure TKMCityBuilder.UnlockPointOfNode(aPoint: TKMPoint);
begin
  if (gAIFields.Influences.AvoidBuilding[aPoint.Y, aPoint.X] = AVOID_BUILDING_NODE_LOCK_ROAD) then // Only roads are unlocked
    gAIFields.Influences.AvoidBuilding[aPoint.Y, aPoint.X] := AVOID_BUILDING_UNLOCK;
end;


procedure TKMCityBuilder.UnlockNode(var aNode: TBuildNode);
var
  I: Integer;
begin
  with aNode.FieldList do
    for I := 0 to Count-1 do
      UnlockPointOfNode(Items[I]);
end;


//procedure ActivateNode(var aNode: TBuildNode; aRemoveTreesMode,aShortcutMode: Boolean = False);
//begin
//  with aNode do
//  begin
//    Active := True;
//    RemoveTreesMode := aRemoveTreesMode;
//    ShortcutMode := aShortcutMode;
//    MaxReqWorkers := 5;
//    RequiredWorkers := Min(MaxReqWorkers, FieldList.Count); // Real count will be updated during building process
//    CenterPoint := Loc;
//  end;
//end;


procedure TKMCityBuilder.UpdateBuildNodes(out aFreeWorkersCnt: Integer);
//Worker tasks:
//  Common phase of tasks
//    0: None (Think about plans)
//    1: Go to plan
//    2,3: Dig
//  TTaskBuildRoad:
//    4: Wait for stone
//    5,6,7: Get stone, Build road, Build road + change tile
//    8: Road is completed
//  TTaskBuildField
//    4: Field is completed
//  TTaskBuildWine
//    4: Dig + change tile
//    5: Wait for a wood
//    6,7: receive wood, build wine
//    8: wine is finished
var
  I, ClosestIdx, ClosestDist, Dist: Integer;
  WorkersPos: TKMPointArray;
begin
  // Reset count of free workers in each node
  for I := Low(fBuildNodes) to High(fBuildNodes) do
    fBuildNodes[I].FreeWorkers := 0;

  // Get positions of workes with nil task (no task)
  aFreeWorkersCnt := 0;
  SetLength(WorkersPos, gHands[fOwner].Stats.GetUnitQty(ut_Worker));
  for I := 0 to gHands[fOwner].Units.Count - 1 do
    if not gHands[fOwner].Units[I].IsDeadOrDying
       AND (gHands[fOwner].Units[I] is TKMUnitWorker)
       AND (    (gHands[fOwner].Units[I].UnitTask = nil)
             //OR ( (gHands[fOwner].Units[I].UnitTask.TaskName = utn_BuildRoad) AND (gHands[fOwner].Units[I].UnitTask.Phase > 7) )
             //OR ( (gHands[fOwner].Units[I].UnitTask.TaskName = utn_BuildField) AND (gHands[fOwner].Units[I].UnitTask.Phase > 4) )
             //OR ( (gHands[fOwner].Units[I].UnitTask.TaskName = utn_BuildWine) AND (gHands[fOwner].Units[I].UnitTask.Phase > 7) )
             //OR (gHands[fOwner].Units[I].UnitTask.TaskName = utn_BuildRoad)
             //OR (gHands[fOwner].Units[I].UnitTask.TaskName = utn_BuildField)
             //OR (gHands[fOwner].Units[I].UnitTask.TaskName = utn_BuildWine)
           ) then
    begin
      WorkersPos[aFreeWorkersCnt] := gHands[fOwner].Units[I].GetPosition;
      aFreeWorkersCnt := aFreeWorkersCnt + 1;
    end;

  // Find closest build-node to each free worker and allow to expand it in next update
  while (aFreeWorkersCnt > 0) do
  begin
    ClosestDist := High(Integer);
    for I := Low(fBuildNodes) to High(fBuildNodes) do
      if (fBuildNodes[I].Active) AND (fBuildNodes[I].RequiredWorkers > 0) then
      begin
        Dist := KMDistanceAbs(WorkersPos[aFreeWorkersCnt - 1], fBuildNodes[I].CenterPoint);
        if (Dist < ClosestDist) then
        begin
          ClosestDist := Dist;
          ClosestIdx := I;
        end;
      end;
    if (ClosestDist <> High(Integer)) then
    begin
      aFreeWorkersCnt := aFreeWorkersCnt - 1;
      with fBuildNodes[ClosestIdx] do
      begin
        RequiredWorkers := RequiredWorkers - 1;
        FreeWorkers := FreeWorkers + 1;
      end;
    end
    else
      break;
  end;

  //if (aFreeWorkersCnt > 0) then // Delete if?
    CreateShortcuts();

  // Update nodes
  for I := Low(fBuildNodes) to High(fBuildNodes) do
    if fBuildNodes[I].Active then
      UpdateBuildNode(fBuildNodes[I]);
end;


//{
procedure TKMCityBuilder.UpdateBuildNode(var aNode: TBuildNode);
  function IsPlan(aPoint: TKMPoint; aLock: TTileLock; aField: TFieldType): Boolean;
  begin
    Result := (gHands[fOwner].BuildList.FieldworksList.HasField(aPoint) = aField)
              OR (gTerrain.Land[aPoint.Y, aPoint.X].TileLock = aLock);
  end;
  function IsCompletedRoad(aPoint: TKMPoint): Boolean;
  begin
    Result := gTerrain.TileIsWalkableRoad(aPoint);
  end;
  function IsCompletedField(aPoint: TKMPoint): Boolean;
  begin
    Result := gTerrain.TileIsCornField(aPoint);
  end;
  function IsCompletedWine(aPoint: TKMPoint): Boolean;
  begin
    Result := gTerrain.TileIsWineField(aPoint);
  end;
  function IsRoad(aPoint: TKMPoint): Boolean;
  begin
    Result := IsCompletedRoad(aPoint) OR IsPlan(aPoint, tlRoadWork, ft_Road);
  end;
  function IsCornField(aPoint: TKMPoint): Boolean;
  begin
    Result := IsCompletedField(aPoint) OR IsPlan(aPoint, tlFieldWork, ft_Corn);
  end;
  function IsWineField(aPoint: TKMPoint): Boolean;
  begin
    Result := IsCompletedWine(aPoint) OR IsPlan(aPoint, tlFieldWork, ft_Wine);
  end;

  function BuildField(aIdx: Integer; aFieldType: TFieldType): Boolean;
  var
    Output: Boolean;
  begin
    Output := False;
    if gHands[fOwner].CanAddFieldPlan(aNode.FieldList.Items[aIdx], aFieldType) then
    begin
      gHands[fOwner].BuildList.FieldworksList.AddField(aNode.FieldList.Items[aIdx], aFieldType);
      aNode.FreeWorkers := aNode.FreeWorkers - 1;
      aNode.RequiredWorkers := aNode.RequiredWorkers - 1;
      Output := True;
    end;
    Result := Output;
  end;

  // Build roads
  procedure BuildRoad();
  var
    I: Integer;
  begin
    with aNode do
    begin
      // Remove elements of exist road from list
      for I := FieldList.Count - 1 downto 0 do
        if IsCompletedRoad(FieldList.Items[I]) then
        begin
          UnlockPointOfNode(FieldList.Items[I]);
          FieldList.Delete(I);
        end
        else if not ShortcutMode then
          break;
      if (FieldList.Count = 0) then
      begin
        Active := False;
        Exit;
      end;
      // Build road / check road plans / replace missing parts / reconnect road when is no more possible to place plan
      RequiredWorkers := FieldList.Count;
      for I := FieldList.Count - 1 downto 0 do
      begin
        // Does we have free workers?
        if (FreeWorkers <= 0) then
          break;
        // Is there already road / plan / work in progress?
        if IsRoad(FieldList.Items[I]) then
        begin
          RequiredWorkers := RequiredWorkers - 1;
          CenterPoint := FieldList.Items[I]; // Actualize center point (distribution of workers by distance)
        end
        // When cannot place new plan try find another way by calling pathfinding
        else if not BuildField(I, ft_Road) then
        begin
          if ShortcutMode then
          begin
            UnlockPointOfNode(FieldList.Items[I]);
            FieldList.Delete(I);
          end
          else
          begin
            UnlockNode(aNode);
            // If is not possible to connect 2 points by road destroy node
            if not fPlanner.GetRoadBetweenPoints(CenterPoint, FieldList.Items[0], FieldList, FieldType) then
            begin
              FieldList.Clear;
              Active := False;
            end;
            LockNode(aNode);
            RequiredWorkers := FieldList.Count;
            Exit; // Node will be updated in next calling
          end;
        end;
      end;
    end;
  end;

  // Build Wine or Corn fields
  procedure BuildFields();
  var
    I,K: Integer;
  begin
    with aNode do
    begin
      RequiredWorkers := FieldList.Count;
      for I := 0 to FieldList.Count - 1 do
      begin
        if (FreeWorkers <= 0) then
          Exit;
        // Check if field already exists ...
        if   ((FieldType = ft_Wine) AND (IsCornField(FieldList.Items[I])))
          OR ((FieldType = ft_Corn) AND (IsWineField(FieldList.Items[I]))) then
        begin
          RequiredWorkers := RequiredWorkers - 1;
        end
        // ... else try build it
        else
          BuildField(I, FieldType);
        // When node reached all plans disable it
        if (RequiredWorkers <= 0) OR (I = FieldList.Count-1) then
        begin
          for K := 0 to FieldList.Count-1 do
            if   ((FieldType = ft_Wine) AND (IsCornField(FieldList.Items[I])))
              OR ((FieldType = ft_Corn) AND (IsWineField(FieldList.Items[I]))) then
              UnlockPointOfNode(FieldList.Items[K]);
          FieldList.Clear;
          Active := False;
        end;
      end;
    end;
  end;

  // Remove trees or Wine / Corn fields which block placing house plan
  procedure RemoveObstaclesInPlan();
  var
    I: Integer;
  begin
    with aNode do
    begin
      if (FieldList.Count = 0) then
      begin
        Active := False;
        with fPlanner.PlannedHouses[aNode.HouseType] do
          for I := 0 to Count - 1 do
            if KMSamePoint(Plans[I].Loc, aNode.HouseLoc) then
              Plans[I].RemoveTreeInPlanProcedure := False;
        Exit;
      end;
      RequiredWorkers := FieldList.Count;
      for I := FieldList.Count - 1 downto 0 do
      begin
        if (FreeWorkers <= 0) then
          Exit;
        // Detect obstacles in house plan
        if gTerrain.ObjectIsChopableTree(FieldList.Items[I].X, FieldList.Items[I].Y) then
        begin
          // Check if is wine plan already placed
          if IsPlan(FieldList.Items[I], tlFieldWork, ft_Wine) then
          begin
            RequiredWorkers := RequiredWorkers - 1;
          end
          // If we cannot remove tree by placing wineyard remove point from list
          else if not BuildField(I, ft_Wine) then
            FieldList.Delete(I);
        end
        // If is plan blocked by fields which could be compensated by road do it
        else if IsCompletedWine(FieldList.Items[I]) OR IsCompletedField(FieldList.Items[I]) OR IsRoad(FieldList.Items[I]) then
        begin
          if IsCompletedRoad(FieldList.Items[I]) then
            FieldList.Delete(I) // Now can be item [I] deleted
          // Else try place road plan or delete point
          else if not IsPlan(FieldList.Items[I], tlRoadWork, ft_Road) then
          begin
            // Delete item [I] only in case that we cannot place road plan (point must be removed only in moment when is road completed)
            if not BuildField(I, ft_Road) then
              FieldList.Delete(I);
          end;
        end
        // Tree could be cut down
        else
        begin
          FieldList.Delete(I);
          RequiredWorkers := RequiredWorkers - 1;
        end;
      end;
    end;
  end;

begin
  // Build procedures are split because of quite complex algorithm (or can be merged into mess)
  if aNode.RemoveTreesMode then
    RemoveObstaclesInPlan()
  else
  begin
    case aNode.FieldType of
      ft_Road: BuildRoad();
      ft_Wine, ft_Corn: BuildFields();
      else
        begin
        end;
    end;
  end;
  if aNode.Active then
    aNode.RequiredWorkers := Min(aNode.MaxReqWorkers, aNode.RequiredWorkers);
end;
//}


function TKMCityBuilder.BuildHouse_GA_MODE(aHT: THouseType): TConstructionState;
var
  FieldType: TFieldType;
  FieldList: TKMPointList;

  procedure AddField();
  var
    I: Integer;
  begin
    for I := FieldList.Count - 1 downto 0 do
      case FieldType of
        ft_Road:
          begin
            gTerrain.SetRoad(FieldList.Items[I], fOwner);
            gTerrain.FlattenTerrain(FieldList.Items[I]);
            if gMapElements[  gTerrain.Land[ FieldList.Items[I].Y,FieldList.Items[I].X ].Obj  ].WineOrCorn then
              gTerrain.RemoveObject(FieldList.Items[I]);
          end;
        ft_Corn,ft_Wine:
        begin
          gTerrain.SetField(FieldList.Items[I], fOwner, FieldType);
          gAIFields.Influences.AvoidBuilding[FieldList.Items[I].Y, FieldList.Items[I].X] := AVOID_BUILDING_NODE_LOCK_FIELD;
        end;
      end;
  end;

var
  Output: Boolean;
  HouseIdx: Integer;
  Loc: TKMPoint;
  HPlan: TKMHousePlan;
begin
  Result := cs_CannotPlaceHouse;
  Output := False;
  FieldList := TKMPointList.Create;
  try
    if fPlanner.GetHousePlan(False, aHT, Loc, HouseIdx) then
    begin
      Output := True;
      gHands[fOwner].AddHousePlan(aHT, Loc); // Place house plan
      if fPlanner.GetRoadToHouse(aHT, HouseIdx, FieldList, FieldType) then // Place roads
        AddField();
      if fPlanner.GetFieldToHouse(aHT, HouseIdx, FieldList, FieldType) then // Place fields
        AddField();
      if gHands[fOwner].BuildList.HousePlanList.TryGetPlan(Loc, HPlan) then // Remove house plan (it must be done because of road planning)
      begin
        gHands[fOwner].BuildList.HousePlanList.RemPlan(Loc);
        gHands[fOwner].Stats.HousePlanRemoved(aHT);
      end;
      gHands[fOwner].AddHouse(aHT, Loc.X, Loc.Y, True); // Place house
    end;
  finally
    FieldList.Free;
  end;
  if Output then
    Result := cs_HousePlaced;
end;


function TKMCityBuilder.BuildHouse(aUnlockProcedure, aHouseReservation: Boolean; aHT: THouseType): TConstructionState;
var
  Output: TConstructionState;
  I, Node1Idx, Node2Idx, HouseIdx: Integer;
  Loc: TKMPoint;
begin
  Output := cs_NoNodeAvailable;
  // Find at least 2 non active build nodes
  Node1Idx := -1;
  Node2Idx := -1;
  for I := Low(fBuildNodes) to High(fBuildNodes) do
    if not fBuildNodes[I].Active then
      if (Node1Idx = -1) then
        Node1Idx := I
      else
      begin
        Node2Idx := I;
        break;
      end;

  // We need min 2 free nodes (1 for road and second for field or 1 for wine and second for road to remove trees in plan)
  if (Node1Idx = -1) OR (Node2Idx = -1) then
    Exit;

  if fPlanner.GetHousePlan(aUnlockProcedure, aHT, Loc, HouseIdx) then
  begin
    // Check if we can place house by default KaM function
    if gHands[fOwner].CanAddHousePlan(Loc, aHT) then
    begin

      // Update reservation status
      if fPlanner.PlannedHouses[aHT].Plans[HouseIdx].HouseReservation then
      begin
        Output := cs_HouseReservation; // House is already reserved -> no nodes will be updated and workers still have nothing to do so we can build another house
        if not aHouseReservation then
          fPlanner.PlannedHouses[aHT].Plans[HouseIdx].HouseReservation := aHouseReservation
        else
      end;

      // if house is not reserved (or will be reserved in this tick)
      if not fPlanner.PlannedHouses[aHT].Plans[HouseIdx].HouseReservation then
      begin
        Output := cs_HousePlaced; // Nodes will be updated -> workers will have something to do
        gHands[fOwner].AddHousePlan(aHT, Loc); // Place house
        // Add road to node
        if fPlanner.GetRoadToHouse(aHT, HouseIdx, fBuildNodes[Node1Idx].FieldList, fBuildNodes[Node1Idx].FieldType) then
          //if aUnlockProcedure then
          //begin
          //  for I := 0 to fBuildNodes[Node1Idx].FieldList.Count - 1 do
          //    if gHands[fOwner].CanAddFieldPlan( fBuildNodes[Node1Idx].FieldList.Items[I], ft_Road ) then
          //      gHands[fOwner].BuildList.FieldworksList.AddField( fBuildNodes[Node1Idx].FieldList.Items[I], ft_Road );
          //end
          //else
          begin
            LockNode(fBuildNodes[Node1Idx]);
            with fBuildNodes[Node1Idx] do
            begin
              Active := True;
              RemoveTreesMode := False;
              ShortcutMode := False;
              MaxReqWorkers := 5 + Byte(aUnlockProcedure) * 20;
              RequiredWorkers := Min(MaxReqWorkers, FieldList.Count);
              CenterPoint := FieldList[0];
            end;
          end;
        // Add field to node (if is required [ht_Farm, ht_Wineyard])
        if fPlanner.GetFieldToHouse(aHT, HouseIdx, fBuildNodes[Node2Idx].FieldList, fBuildNodes[Node2Idx].FieldType) then
        begin
          LockNode(fBuildNodes[Node2Idx]);
          with fBuildNodes[Node2Idx] do
          begin
            Active := True;
            RemoveTreesMode := False;
            ShortcutMode := False;
            MaxReqWorkers := 5 + Byte(aUnlockProcedure) * 20;
            RequiredWorkers := Min(MaxReqWorkers, FieldList.Count);
            CenterPoint := Loc;
          end;
        end;
        // Reserve house place
        if aHouseReservation then
        begin
          fPlanner.PlannedHouses[aHT].Plans[HouseIdx].HouseReservation := True;
          gHands[fOwner].RemHousePlan(Loc);
        end;
      end;
    end
    else if gAIFields.Eye.CanPlaceHouse(Loc, aHT, True) then
    begin
      Output := cs_RemoveTreeProcedure; // Remove tree procedure does not require significant count of workers so there is not need for separate mark
      // Remove tree procedure is already active
      if (fPlanner.PlannedHouses[aHT].Plans[HouseIdx].RemoveTreeInPlanProcedure) then
      begin
        // Wait till is tree removed
      end
      // House plan cannot be placed because of existing tree -> remove it by placing wine and road at specific tiles
      else if (fPlanner.GetTreesInHousePlan(aHT, HouseIdx, fBuildNodes[Node1Idx].FieldList) > 0) then
      begin
        for I := Low(fBuildNodes) to High(fBuildNodes) do
          if fBuildNodes[I].Active
            AND fBuildNodes[I].RemoveTreesMode
            AND (fBuildNodes[I].FieldList.Count > 0)
            AND KMSamePoint(fBuildNodes[Node1Idx].FieldList.Items[0], fBuildNodes[I].FieldList.Items[0]) then
          begin
            fBuildNodes[Node1Idx].FieldList.Clear;
            Exit;
          end;
        with fBuildNodes[Node1Idx] do
        begin
          Active := True;
          RemoveTreesMode := True;
          ShortcutMode := False;
          MaxReqWorkers := 5;
          RequiredWorkers := Min(MaxReqWorkers, FieldList.Count); // Real count will be updated during building process
          CenterPoint := Loc;
          HouseLoc := Loc;
          HouseType := aHT;
        end;
      end;
      // There is another problem...
      //else
      //  Planner.RemovePlan(aHT, Loc);
    end;
    //else
    //  Planner.RemovePlan(aHT, Loc);
  end
  else
  begin
    // new house cannot be added
  end;
  Result := Output
end;


function TKMCityBuilder.ChooseHousesToBuild(aMaxCnt: Integer): Boolean;
type
  TSetOfWare = set of TWareType;
const
  BASIC_HOUSES: set of THouseType = [ht_School, ht_Barracks, ht_Inn, ht_MarketPlace, ht_Store];
  BUILD_WARE: TSetOfWare = [wt_GoldOre, wt_Coal, wt_Gold, wt_Stone, wt_Trunk, wt_Wood];
  FOOD_WARE: TSetOfWare = [wt_Corn, wt_Flour, wt_Bread, wt_Pig, wt_Sausages, wt_Wine, wt_Fish];
  WEAPON_WARE: TSetOfWare = [wt_Skin, wt_Leather, wt_Horse, wt_IronOre, wt_Coal, wt_Steel, wt_Axe, wt_Bow, wt_Pike, wt_Armor, wt_Shield, wt_Sword, wt_Arbalet, wt_Hallebard, wt_MetalShield, wt_MetalArmor];
var
  RequiredHouses: TRequiredHousesArray;
  WareBalance: TWareBalanceArray;


  function GetHouseToUnlock(var aUnlockProcedure: Boolean; var aHT, aFollowingHouse: THouseType): Boolean;
  var
    Output: Boolean;
    initHT: THouseType;
  begin
    Output := True;
    // Repeat until is avaiable house finded (to unlock target house)
    initHT := aHT;
    aFollowingHouse := ht_None;
    while Output AND not gHands[fOwner].Locks.HouseCanBuild(aHT) do
    begin
      aUnlockProcedure := True;
      if gHands[fOwner].Locks.HouseBlocked[aHT] then // House is blocked -> unlock impossible
        Output := False
      else
      begin
        aFollowingHouse := aHT;
        aHT := gRes.Houses[aHT].ReleasedBy; // House have to be unlocked by this house
      end;
    end;
    // Output = false only in case that house is already under construction OR is blocked by script / settings from the map editor
    Result := Output AND ( (initHT = aHT) OR (gHands[fOwner].Stats.GetHouseTotal(aHT) = 0) );
  end;


  function AddToConstruction(aHT: THouseType; aUnlockProcedureRequired: Boolean = False): TConstructionState;
  var
    UnlockProcedure: Boolean;
    FollowingHouse: THouseType;
    Output: TConstructionState;
  begin
    Output := cs_CannotPlaceHouse;
    UnlockProcedure := aUnlockProcedureRequired; // Unlock procedure = build house as soon as possible -> avoid to build house plan inside of tree (remove tree will take time)
    // Check if AI can build house (if is house blocked [by script] ignore it)
    if GetHouseToUnlock(UnlockProcedure, aHT, FollowingHouse) then
    begin
      aMaxCnt := aMaxCnt - 1;
      if GA_PLANNER then
        Output := BuildHouse_GA_MODE(aHT)
      else
        Output := BuildHouse(UnlockProcedure, False, aHT);
    end
    else if (FollowingHouse <> ht_none) AND (gHands[fOwner].Stats.GetHouseQty(ht_School) > 0) then // Activate house reservation
    begin
      Output := BuildHouse(True, True, FollowingHouse);
    end;
    Result := Output;
  end;

  function SelectHouse(const aSetOfWare: TSetOfWare): Boolean;
  const
    FRACTION_COEF = 3.0;
  var
    Output: Boolean;
    I: Integer;
    Priority, POM_Priority: Single;
    Ware, WT, POM_WT: TWareType;
    WareOrder: array[0..5] of TWareType;
    WarePriority: array[0..5] of Single;
  begin
    Output := False;
    // Basic producing houses (secure resources for building)
    for I := Low(WareOrder) to High(WareOrder) do
    begin
      WareOrder[I] := wt_None;
      WarePriority[I] := 0; // Doesn't have to be initialized but in this case compilation throws warning
    end;
    // Find the most required house to be build
    for Ware in aSetOfWare do
    begin
      WT := Ware;
      if (RequiredHouses[ PRODUCTION[WT] ] > 0) then
      begin
        Priority := WareBalance[WT].Exhaustion - WareBalance[WT].Fraction * FRACTION_COEF;
        for I := Low(WareOrder) to High(WareOrder) do
          if (WT = wt_None) then
            break
          else if (WareOrder[I] = wt_None) OR (Priority < WarePriority[I]) then // Buble sort is best for few elements
          begin
            POM_WT := WT;
            WT := WareOrder[I];
            WareOrder[I] := POM_WT;
            POM_Priority := Priority;
            Priority := WarePriority[I];
            WarePriority[I] := POM_Priority;
          end;
      end;
    end;
    // Try build required houses
    for I := Low(WareOrder) to High(WareOrder) do
    begin
      if (WareOrder[I] = wt_None) then
        break;
      case AddToConstruction(PRODUCTION[ WareOrder[I] ]) of
        cs_NoNodeAvailable: break;
        cs_HouseReservation, cs_RemoveTreeProcedure: Output := True;
        cs_HousePlaced:
        begin
          Output := True;
          aMaxCnt := aMaxCnt - 1;
          if (aMaxCnt <= 0) then
            break;
          RequiredHouses[  PRODUCTION[ WareOrder[I] ]  ] := 0; // Make sure that next node will not scan this house in this tick
        end;
        cs_CannotPlaceHouse:
        begin
          RequiredHouses[  PRODUCTION[ WareOrder[I] ]  ] := 0; // Make sure that next node will not scan this house in this tick
          //Dec(aRequiredHouses[  PRODUCTION[ WareOrder[I] ]  ]);
        end;
      end;
    end;
    Result := Output;
  end;

  procedure CheckHouseReservation();
  var
    I: Integer;
    HT: THouseType;
  begin
    for HT := Low(fPlanner.PlannedHouses) to High(fPlanner.PlannedHouses) do
      for I := 0 to fPlanner.PlannedHouses[HT].Count - 1 do
        with fPlanner.PlannedHouses[HT].Plans[I] do
          if not Placed AND (HouseReservation OR RemoveTreeInPlanProcedure)
             AND (cs_HousePlaced = AddToConstruction(HT)) then
          begin
            aMaxCnt := aMaxCnt - 1;
            RequiredHouses[HT] := 0;
          end;
  end;
var
  Output: Boolean;
  POMCoal: Integer;
  HT: THouseType;
begin
  Output := False;
  RequiredHouses := fPredictor.RequiredHouses;
  WareBalance := fPredictor.WareBalance;

  CheckHouseReservation();
  if (aMaxCnt <= 0) then
    Exit;

  // Basic houses (for city management)
  for HT in BASIC_HOUSES do
    if (RequiredHouses[HT] > 0) AND (AddToConstruction(HT, True) = cs_HousePlaced) then
    begin
      Output := True;
      if (aMaxCnt <= 0) then
        break;
    end;

  WareBalance[wt_Trunk].Fraction := Max(0, RequiredHouses[ht_Woodcutters] - gHands[fOwner].Stats.GetHouseQty(ht_Woodcutters) - 4);
  POMCoal := RequiredHouses[ht_CoalMine]; // Coal is used by resource (Gold) and by weapon division -> extract just Gold requirements
  RequiredHouses[ht_CoalMine] := Max(0,gHands[fOwner].Stats.GetHouseTotal(ht_GoldMine)-gHands[fOwner].Stats.GetHouseTotal(ht_CoalMine));
  RequiredHouses[ht_Woodcutters] := Max(0,RequiredHouses[ht_Woodcutters] - Round(Byte(WareBalance[wt_Gold].Exhaustion < 20) * RequiredHouses[ht_Woodcutters] * 0.5));
  if (aMaxCnt > 0) AND SelectHouse(BUILD_WARE) then
    Output := True;

  // Make sure that gold will be produced (stones and wood are fines because of initial influence and order of construction)
  //if (gHands[fOwner].Stats.GetWareBalance(wt_Wood) < 10) AND (WareBalance[wt_Gold].Exhaustion < 20) then
  if (gHands[fOwner].Stats.GetWareBalance(wt_Wood) < 10) then
    Exit;

  // Now return coal count back to original values
  WareBalance[wt_Coal].Fraction := (gHands[fOwner].Stats.GetHouseTotal(ht_CoalMine) - RequiredHouses[ht_CoalMine]) / POMCoal;
  RequiredHouses[ht_CoalMine] := POMCoal;
  if (aMaxCnt > 0) then
  begin
    Output := SelectHouse(FOOD_WARE);
    SelectHouse(WEAPON_WARE);
    SelectHouse(FOOD_WARE);
  end;

  Result := Output;
end;


procedure TKMCityBuilder.CreateShortcuts();
const
  MAX_SHORTCUTS_PER_HOUSE_TYPE = 2;
  MAX_DISTANCE_TO_ALL_HOUSES = 8;
  MAX_WORKERS_FOR_NODE = 20;      // CHANGE CHANGE CHANGE CHANGE CHANGE CHANGE CHANGE CHANGE CHANGE CHANGE
  HOUSE_CONNECTION: array[HOUSE_MIN..HOUSE_MAX] of set of THouseType = (
    {ht_ArmorSmithy}    [ ht_IronSmithy,    ht_CoalMine,     ht_Barracks    ],
    {ht_ArmorWorkshop}  [ ht_Tannery,       ht_Barracks                     ],
    {ht_Bakery}         [ ht_Inn,           ht_Store,        ht_Mill        ],
    {ht_Barracks}       [ ht_School                                         ],
    {ht_Butchers}       [ ht_Inn,           ht_Store,        ht_Swine       ],
    {ht_CoalMine}       [ ht_None                                           ],
    {ht_Farm}           [ ht_None                                           ],
    {ht_FisherHut}      [ ht_None                                           ],
    {ht_GoldMine}       [ ht_Metallurgists                                  ],
    {ht_Inn}            [ ht_Store,         ht_Inn                          ],
    {ht_IronMine}       [ ht_IronSmithy                                     ],
    {ht_IronSmithy}     [ ht_CoalMine,      ht_WeaponSmithy, ht_ArmorSmithy ],
    {ht_Marketplace}    [ ht_Store                                          ],
    {ht_Metallurgists}  [ ht_School,        ht_GoldMine,     ht_CoalMine    ],
    {ht_Mill}           [ ht_Farm,          ht_Bakery                       ],
    {ht_Quary}          [ ht_Store                                          ],
    {ht_Sawmill}        [ ht_ArmorWorkshop, ht_Store                        ],
    {ht_School}         [ ht_Metallurgists, ht_Store,        ht_Barracks    ],
    {ht_SiegeWorkshop}  [ ht_IronSmithy,    ht_Sawmill,      ht_Store       ],
    {ht_Stables}        [ ht_Farm,          ht_Barracks                     ],
    {ht_Store}          [ ht_Inn,           ht_Barracks,     ht_School      ],
    {ht_Swine}          [ ht_Farm,          ht_Butchers                     ],
    {ht_Tannery}        [ ht_ArmorWorkshop, ht_Swine                        ],
    {ht_TownHall}       [ ht_Metallurgists, ht_Store                        ],
    {ht_WatchTower}     [ ht_None                                           ],
    {ht_WeaponSmithy}   [ ht_IronSmithy,    ht_CoalMine,     ht_Barracks    ],
    {ht_WeaponWorkshop} [ ht_Sawmill,       ht_Barracks                     ],
    {ht_Wineyard}       [ ht_Inn                                            ],
    {ht_Woodcutters}    [ ht_None                                           ]
  );

  function FindAndMarkNewHouse(var aHT: THouseType; var aLoc: TKMPoint): Boolean;
  var
    I: Integer;
    HT: THouseType;
  begin
    Result := True;
    for HT := Low(fPlanner.PlannedHouses) to High(fPlanner.PlannedHouses) do
    begin
      if (HT = ht_Woodcutters) then
        continue;
      with fPlanner.PlannedHouses[HT] do
        for I := 0 to Count - 1 do
          if Plans[I].Placed AND not Plans[I].ShortcutsCompleted then
          begin
            Plans[I].ShortcutsCompleted := True;
            aLoc := KMPointBelow(Plans[I].Loc);
            aHT := HT;
            Exit;
          end;
    end;
    Result := False;
  end;

  // Plan road from aBaseLoc to points in aLocs
  procedure PlanRoad(var aNode: TBuildNode; aBaseLoc: TKMPoint; var aLocs: TKMPointTagList; aAllLocs: Boolean = False);
  var
    I, K, cnt: Integer;
    Road: TKMPointList;
  begin
    if (aLocs.Count > 0) then
    begin
      if not aAllLocs then // Sort in case that we want to pick just the closest points
        aLocs.SortByTag();
      Road := TKMPointList.Create();
      try
        cnt := 0;
        for I := 0 to aLocs.Count-1 do
        begin
          Road.Clear();
          // Plan road
          if fPlanner.GetRoadBetweenPoints(aLocs.Items[I], aBaseLoc, Road, aNode.FieldType) then
          begin
            // Copy new road to build node (1 node will have all roads -> shortcuts will not take
            for K := 0 to Road.Count - 1 do
              aNode.FieldList.Add(Road.Items[K]);
            cnt := cnt + 1;
            LockNode(aNode); // Lock must be here because next shortcut will see road reservation -> avoid to build 2 road next to each other
            if not aAllLocs AND (cnt = MAX_SHORTCUTS_PER_HOUSE_TYPE) then
              break;
          end;
        end;
      finally
        Road.Free;
      end;
      with aNode do
        if (FieldList.Count > 0) then
        begin
          Active := True;
          RemoveTreesMode := False;
          ShortcutMode := True;
          MaxReqWorkers := MAX_WORKERS_FOR_NODE;
          RequiredWorkers := Min(MaxReqWorkers, FieldList.Count);
          CenterPoint := FieldList.Items[0];
        end;
    end;
  end;

var
  I,K,NodeIdx, Dist: Integer;
  HT, BaseHT: THouseType;
  BaseLoc: TKMPoint;
  PlannedHouses: TPlannedHousesArray;
  Locs: TKMPointTagList;
begin
  // Don't build shortcuts when is negative stone derivation
  if   (fPredictor.WareBalance[wt_Stone].Exhaustion < 60)
    //OR (fPredictor.WareBalance[wt_Wood].Exhaustion < 60)
    OR (fPredictor.WareBalance[wt_Gold].Exhaustion < 60) then
    Exit;

  // Check if there is free build node
  for NodeIdx := Low(fBuildNodes) to High(fBuildNodes) do
    if not fBuildNodes[NodeIdx].Active then
      Break;
  if fBuildNodes[NodeIdx].Active then
    Exit;

  // Find house which was not checked for shortcuts
  if not FindAndMarkNewHouse(BaseHT, BaseLoc) then
    Exit;

  // Special case for entrance of Store and Barrack
  if (BaseHT = ht_Store) OR (BaseHT = ht_Barracks) then
    if (BaseLoc.Y < gTerrain.MapY - 1) then
      for I := BaseLoc.X-1 to BaseLoc.X+1 do
      begin
        gAIFields.Influences.AvoidBuilding[BaseLoc.Y+1, I] := 255;
        if gHands[fOwner].CanAddFieldPlan(KMPoint(I, BaseLoc.Y+1) , ft_Road) then
          gHands[fOwner].BuildList.FieldworksList.AddField(KMPoint(I, BaseLoc.Y+1) , ft_Road);
      end;

  // Find houses which should be connected
  PlannedHouses := fPlanner.PlannedHouses;
  Locs := TKMPointTagList.Create();
  try
    // Create basic connection to houses which are part of specific distribution network
    for HT in HOUSE_CONNECTION[BaseHT] do
    begin
      if (HT = ht_None) then
        break;

      Locs.Clear();
      for K := 0 to PlannedHouses[HT].Count - 1 do
        with PlannedHouses[HT].Plans[K] do
          if Placed then
            Locs.Add( KMPointBelow(Loc), KMDistanceAbs(Loc, BaseLoc) );
      PlanRoad(fBuildNodes[NodeIdx], BaseLoc, Locs, False);
    end;

    // Create additional shortcuts to closest houses
    Locs.Clear();
    if (HT <> ht_None) then
      for HT := Low(PlannedHouses) to High(PlannedHouses) do
      begin
        if (HT = ht_Woodcutters) then
          continue;
        for I := 0 to PlannedHouses[HT].Count - 1 do
          with PlannedHouses[HT].Plans[I] do
            if Placed then
            begin
              Dist := KMDistanceAbs(BaseLoc, KMPointBelow(Loc));
              if (Dist <> 0) AND (Dist < MAX_DISTANCE_TO_ALL_HOUSES) then
                Locs.Add( KMPointBelow(Loc), Dist );
            end;
      end;
    PlanRoad(fBuildNodes[NodeIdx], BaseLoc, Locs, True);
  finally
    Locs.Free;
  end;
end;


procedure TKMCityBuilder.LogStatus(var aBalanceText: UnicodeString);
const
  HOUSE_TO_STRING: array[HOUSE_MIN..HOUSE_MAX] of UnicodeString = (
    'ArmorSmithy',   'ArmorWorkshop',   'Bakery',        'Barracks',       'Butchers',
    'CoalMine',      'Farm',            'FisherHut',     'GoldMine',       'Inn',
    'IronMine',      'IronSmithy',      'Marketplace',   'Metallurgists',  'Mill',
    'Quary',         'Sawmill',         'School',        'SiegeWorkshop',  'Stables',
    'Store',         'Swine',           'Tannery',       'TownHall',       'WatchTower',
    'WeaponSmithy',  'WeaponWorkshop',  'Wineyard',      'Woodcutters'
  );
var
  I, cnt: Integer;
  HT: THouseType;
begin
  aBalanceText := aBalanceText + '|Construction: ';
  cnt := 0;
  for HT := Low(fPlanner.PlannedHouses) to High(fPlanner.PlannedHouses) do
    for I := 0 to fPlanner.PlannedHouses[HT].Count - 1 do
      with fPlanner.PlannedHouses[HT].Plans[I] do
        if not Placed then
        begin
          if HouseReservation then
          begin
            aBalanceText := aBalanceText + HOUSE_TO_STRING[HT] + ' (Reservation), ';
            cnt := cnt + 1;
          end
          else if RemoveTreeInPlanProcedure then
          begin
            aBalanceText := aBalanceText + HOUSE_TO_STRING[HT] + ' (Remove trees), ';
            cnt := cnt + 1;
          end
          else
          begin
            aBalanceText := aBalanceText + HOUSE_TO_STRING[HT] + ' (Plan placed), ';
            cnt := cnt + 1;
          end;

          if (cnt > 5) then
          begin
            cnt := 0;
            aBalanceText := aBalanceText + '|';
          end;
        end;

  cnt := 0;
  for I := Low(fBuildNodes) to High(fBuildNodes) do
    if fBuildNodes[I].Active then
      cnt := cnt + 1;
  aBalanceText := aBalanceText + '|Active nodes:' + IntToStr(cnt);
end;

procedure TKMCityBuilder.Paint();
const
  COLOR_WHITE = $80FFFFFF;
  COLOR_BLACK = $20000000;
  COLOR_GREEN = $6000FF00;
  COLOR_RED = $800000FF;
  COLOR_YELLOW = $8000FFFF;
  COLOR_GREEN_Field = $4400FF00;
  COLOR_GREEN_Wine = $3355FFFF;
  COLOR_BLUE = $60FF0000;
var
  I,K: Integer;
  Color: Cardinal;
  Point: TKMPoint;
begin
  for I := Low(fBuildNodes) to High(fBuildNodes) do
    with fBuildNodes[I] do
      if Active then
      begin
        case FieldType of
          ft_Corn: Color := COLOR_GREEN_Field;
          ft_Wine: Color := COLOR_GREEN_Wine;
          ft_Road: Color := COLOR_YELLOW;
        end;
        if RemoveTreesMode then
          Color := COLOR_BLUE;
        if ShortcutMode then
          Color := COLOR_BLACK;
        for K := 0 to FieldList.Count - 1 do
        begin
          Point := FieldList.Items[K];
          gRenderAux.Quad(Point.X, Point.Y, Color);
        end;
      end;
end;


end.


{
procedure TKMCityBuilder.UpdateBuildNode(var aNode: TBuildNode);
  function IsPlan(aPoint: TKMPoint; aLock: TTileLock; aField: TFieldType): Boolean;
  begin
    Result := (gHands[fOwner].BuildList.FieldworksList.HasField(aPoint) = aField)
              OR (gTerrain.Land[aPoint.Y, aPoint.X].TileLock = aLock);
  end;
  function IsCompletedRoad(aPoint: TKMPoint): Boolean;
  begin
    Result := gTerrain.TileIsWalkableRoad(aPoint);
  end;
  function IsCompletedField(aPoint: TKMPoint): Boolean;
  begin
    Result := gTerrain.TileIsCornField(aPoint);
  end;
  function IsCompletedWine(aPoint: TKMPoint): Boolean;
  begin
    Result := gTerrain.TileIsWineField(aPoint);
  end;
  function IsRoad(aPoint: TKMPoint): Boolean;
  begin
    Result := IsCompletedRoad(aPoint) OR IsPlan(aPoint, tlRoadWork, ft_Road);
  end;
  function IsCornField(aPoint: TKMPoint): Boolean;
  begin
    Result := IsCompletedField(aPoint) OR IsPlan(aPoint, tlFieldWork, ft_Corn);
  end;
  function IsWineField(aPoint: TKMPoint): Boolean;
  begin
    Result := IsCompletedWine(aPoint) OR IsPlan(aPoint, tlFieldWork, ft_Wine);
  end;

  function BuildField(aIdx: Integer; aFieldType: TFieldType): Boolean;
  var
    Output: Boolean;
  begin
    Output := False;
    if gHands[fOwner].CanAddFieldPlan(aNode.FieldList.Items[aIdx], aFieldType) then
    begin
      gHands[fOwner].BuildList.FieldworksList.AddField(aNode.FieldList.Items[aIdx], aFieldType);
      aNode.FreeWorkers := aNode.FreeWorkers - 1;
      Output := True;
    end;
    Result := Output;
  end;

  // Build roads
  procedure BuildRoad();
  var
    I: Integer;
  begin
    with aNode do
    begin
      // Remove elements of exist road from list
      for I := FieldList.Count - 1 downto 0 do
        if IsCompletedRoad(FieldList.Items[I]) then
        begin
          UnlockPointOfNode(FieldList.Items[I]);
          FieldList.Delete(I);
        end
        else
          break;
      if (FieldList.Count = 0) then
      begin
        Active := False;
        Exit;
      end;
      RequiredWorkers := FieldList.Count;
      // Build road / check road plans / replace missing parts / reconnect road when is no more possible to place plan
      for I := FieldList.Count - 1 downto 0 do
      begin
        // Does we have free workers?
        if (FreeWorkers <= 0) then
          break;
        // Is there already road / plan / work in progress?
        if IsRoad(FieldList.Items[I]) then
        begin
          FreeWorkers := FreeWorkers - 1;
          CenterPoint := FieldList.Items[I]; // Actualize center point (distribution of workers by distance)
        end
        // When cannot place new plan try find another way by calling pathfinding
        else if not BuildField(I, ft_Road) then
        begin
          // If is not possible to connect 2 points by road destroy node
          if not fPlanner.GetRoadBetweenPoints(CenterPoint, FieldList.Items[0], FieldList, FieldType) then
          begin
            FieldList.Clear;
            Active := False;
          end;
          LockNode(aNode);
          RequiredWorkers := FieldList.Count;
          Exit; // Node will be updated in next calling
        end;
      end;
    end;
  end;

  // Build Wine or Corn fields
  procedure BuildFields();
  var
    I,K: Integer;
  begin
    with aNode do
    begin
      RequiredWorkers := FieldList.Count;
      for I := 0 to FieldList.Count - 1 do
      begin
        if (FreeWorkers <= 0) then
          Exit;
        // Check if field already exists ...
        if   ((FieldType = ft_Wine) AND (IsCompletedField(FieldList.Items[I])))
          OR ((FieldType = ft_Corn) AND (IsCompletedWine(FieldList.Items[I]))) then
        begin
          RequiredWorkers := RequiredWorkers - 1;
        end
        // ... or if is plan placed
        else if (IsPlan(FieldList.Items[I], tlFieldWork, FieldType)) then
          FreeWorkers := FreeWorkers - 1
        // ... else try build it
        else
          BuildField(I, FieldType);
        // When node reached all plans disable it
        if (RequiredWorkers <= 0) OR (I = FieldList.Count-1) then
        begin
          for K := 0 to FieldList.Count-1 do
            if   ((FieldType = ft_Wine) AND (IsCornField(FieldList.Items[I])))
              OR ((FieldType = ft_Corn) AND (IsWineField(FieldList.Items[I]))) then
              UnlockPointOfNode(FieldList.Items[K]);
          FieldList.Clear;
          Active := False;
        end;
      end;
    end;
  end;

  // Remove trees or Wine / Corn fields which block placing house plan
  procedure RemoveObstaclesInPlan();
  var
    I: Integer;
  begin
    with aNode do
    begin
      // Finish node
      if (FieldList.Count = 0) then
      begin
        Active := False;
        fPlanner.PlannedHouses[ResponsibleHouseType,ResponsibleHouseIdx].RemoveTreeInPlanProcedure := False;
        UnlockHouseLoc(ResponsibleHouseType, fPlanner.PlannedHouses[ResponsibleHouseType,ResponsibleHouseIdx].Loc);
        Exit;
      end;
      RequiredWorkers := FieldList.Count;
      for I := FieldList.Count - 1 downto 0 do
      begin
        if (FreeWorkers <= 0) then
          Exit;
        // Detect obstacles in house plan
        if gTerrain.ObjectIsChopableTree(FieldList.Items[I].X, FieldList.Items[I].Y) then
        begin
          // Check if is wine plan already placed
          if IsPlan(FieldList.Items[I], tlFieldWork, ft_Wine) then
          begin
          end
          // If we cannot remove tree by placing wineyard remove point from list
          else if not BuildField(I, ft_Wine) then
            FieldList.Delete(I);
        end
        // If is plan blocked by fields which could be compensated by road do it
        else if IsCompletedWine(FieldList.Items[I]) OR IsCompletedField(FieldList.Items[I]) OR IsRoad(FieldList.Items[I]) then
        begin
          if IsCompletedRoad(FieldList.Items[I]) then
            FieldList.Delete(I) // Now can be item [I] deleted
          // Else try place road plan or delete point
          else if not IsPlan(FieldList.Items[I], tlRoadWork, ft_Road) then
          begin
            // Delete item [I] only in case that we cannot place road plan (point must be removed only in moment when is road completed)
            if not BuildField(I, ft_Road) then
              FieldList.Delete(I);
          end;
        end
        // Tree could not be cutted down
        else
        begin
          FieldList.Delete(I);
          RequiredWorkers := RequiredWorkers - 1;
        end;
      end;
    end;
  end;

begin
  // Build procedures are split because of quite complex algorithm (or can be merged into mess)
  if aNode.RemoveTreesMode then
    RemoveObstaclesInPlan()
  else
    case aNode.FieldType of
      ft_Road: BuildRoad();
      ft_Wine, ft_Corn: BuildFields();
      else
        begin
        end;
    end;
    if aNode.Active then
      aNode.RequiredWorkers := Min(aNode.MaxReqWorkers, aNode.RequiredWorkers);
end;
//}
