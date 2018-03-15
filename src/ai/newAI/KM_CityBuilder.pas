unit KM_CityBuilder;
{$I KaM_Remake.inc}
interface
uses
  KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_Points,
  KM_ResHouses, KM_ResWares, KM_BuildList,
  KM_AIInfluences, KM_CityPlanner, KM_CityPredictor, KM_Eye;


var
  GA_BUILDER_BuildHouse_RoadMaxWork                   : Single = 6;
  GA_BUILDER_BuildHouse_FieldMaxWork                  : Single = 1;
  GA_BUILDER_BuildHouse_RTPMaxWork                    : Single = 10;
  GA_BUILDER_CreateShortcuts_MaxWork                  : Single = 6;
  GA_BUILDER_ChooseHousesToBuild_FC                   : Single = 8.512814522;


type

  TBuildNode = record
    Active, RemoveTreesMode, ShortcutMode: Boolean;
    FreeWorkers, RequiredWorkers, MaxReqWorkers: Integer;
    CenterPoint: TKMPoint;
    FieldType: TFieldType; //ft_Corn, ft_Wine, ft_Road
    FieldList: TKMPointList;
  end;

  TConstructionState = (cs_NoNodeAvailable, cs_NoPlaceCanBeFound, cs_HousePlaced, cs_CannotPlaceHouse, cs_HouseReservation, cs_RemoveTreeProcedure);

  // City builder (build nodes, selection from required houses)
  TKMCityBuilder = class
  private
    fOwner: TKMHandIndex;
    fBuildNodes: array of TBuildNode;
    fWorkersPos: TKMPointArray;

    fPlanner: TKMCityPlanner;
    fPredictor: TKMCityPredictor;

    procedure UpdateBuildNode(var aNode: TBuildNode);

    function BuildHouse(aUnlockProcedure, aHouseReservation, aIgnoreExistingPlans: Boolean; aHT: THouseType): TConstructionState;
    function BuildHouse_GA_MODE(aHT: THouseType): TConstructionState;
    procedure LockNode(var aNode: TBuildNode);
    procedure UnlockPointOfNode(aPoint: TKMPoint; aCheckHousePlan: Boolean = False);
    procedure UnlockNode(var aNode: TBuildNode; aCheckHousePlan: Boolean = False);

    procedure CreateShortcuts();
  public
    constructor Create(aPlayer: TKMHandIndex; aPredictor: TKMCityPredictor);
    destructor Destroy(); override;

    property Planner: TKMCityPlanner read fPlanner;
    property WorkersPos: TKMPointArray read fWorkersPos;

    procedure AfterMissionInit(out aGoldMineCnt, aIronMineCnt, aFieldCnt, aBuildCnt: Integer);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);

    procedure UpdateState(aTick: Cardinal; out aFreeWorkersCnt: Integer);
    procedure UpdateBuildNodes(out aFreeWorkersCnt: Integer);
    procedure ChooseHousesToBuild(aFreeWorkersCnt: Integer; aTick: Cardinal);

    procedure LockHouseLoc(aHT: THouseType; aLoc: TKMPoint);
    procedure UnlockHouseLoc(aHT: THouseType; aLoc: TKMPoint);

    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure LogStatus(var aBalanceText: UnicodeString);
    procedure Paint();
  end;


const
  STONE_SHORTAGE = 10;
  TRUNK_SHORTAGE = 5;
  WOOD_SHORTAGE = 10;
  GOLD_SHORTAGE = 20;


implementation
uses
  KM_Game, KM_HandsCollection, KM_Terrain, KM_Resource,
  KM_AIFields, KM_Units, KM_UnitTaskDelivery, KM_UnitActionWalkTo,
  KM_NavMesh, KM_RenderAux, KM_ResMapElements;

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

  Cnt := Length(fWorkersPos);
  SaveStream.Write(Cnt);
  if (Cnt > 0) then
    SaveStream.Write(fWorkersPos[0], SizeOf(fWorkersPos[0]) * Cnt);

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
  SetLength(fWorkersPos, Cnt);
  if (Cnt > 0) then
    LoadStream.Read(fWorkersPos[0], SizeOf(fWorkersPos[0]) * Cnt);

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
begin
  if (aTick mod MAX_HANDS = fOwner) then
  begin
    fPlanner.UpdateState(aTick); // Planner must be updated as first to secure that completed houses are actualized
    UpdateBuildNodes(aFreeWorkersCnt);
  end;
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


procedure TKMCityBuilder.UnlockPointOfNode(aPoint: TKMPoint; aCheckHousePlan: Boolean = False);
var
  X,Y: Integer;
begin
  if aCheckHousePlan then
    for Y := Max(1,aPoint.Y-1) to Min(aPoint.Y+1, gTerrain.MapY-1) do
    for X := Max(1,aPoint.X-1) to Min(aPoint.X+1, gTerrain.MapX-1) do
      if (gAIFields.Influences.AvoidBuilding[Y,X] = AVOID_BUILDING_HOUSE_INSIDE_LOCK) then
      begin
        gAIFields.Influences.AvoidBuilding[aPoint.Y, aPoint.X] := AVOID_BUILDING_HOUSE_OUTSIDE_LOCK;
        Exit;
      end;

  if (gAIFields.Influences.AvoidBuilding[aPoint.Y, aPoint.X] = AVOID_BUILDING_NODE_LOCK_ROAD) then // Only roads are unlocked
    gAIFields.Influences.AvoidBuilding[aPoint.Y, aPoint.X] := AVOID_BUILDING_UNLOCK;
end;


procedure TKMCityBuilder.UnlockNode(var aNode: TBuildNode; aCheckHousePlan: Boolean = False);
var
  I: Integer;
begin
  with aNode.FieldList do
    for I := 0 to Count-1 do
      UnlockPointOfNode(Items[I], aCheckHousePlan);
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
  I, ClosestIdx, ClosestDist, Dist, WorkerCnt: Integer;
  WorkersPos: TKMPointArray;
begin
  // Reset count of free workers in each node
  for I := Low(fBuildNodes) to High(fBuildNodes) do
    fBuildNodes[I].FreeWorkers := 0;

  // Get positions of workes with nil task (no task)
  aFreeWorkersCnt := 0;
  WorkerCnt := gHands[fOwner].Stats.GetUnitQty(ut_Worker);
  if (Length(fWorkersPos) <> WorkerCnt) then
    SetLength(fWorkersPos, WorkerCnt);
  SetLength(WorkersPos, WorkerCnt);
  for I := 0 to gHands[fOwner].Units.Count - 1 do
    if not gHands[fOwner].Units[I].IsDeadOrDying
       AND (gHands[fOwner].Units[I] is TKMUnitWorker) then
       //AND ((gHands[fOwner].Units[I].UnitTask = nil) OR (gHands[fOwner].Units[I].UnitTask.TaskName <> utn_SelfTrain)) then
    begin
      //WorkerCnt := WorkerCnt - 1;
      //fWorkersPos[WorkerCnt] := gHands[fOwner].Units[I].GetPosition;
      if (    (gHands[fOwner].Units[I].UnitTask = nil)
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
    end;
  SetLength(WorkersPos, aFreeWorkersCnt);

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

  fWorkersPos := WorkersPos;
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
    I, ActiveWorkers: Integer;
  begin
    ActiveWorkers := 0;
    with aNode do
    begin
      // Remove elements of exist road from list
      for I := FieldList.Count - 1 downto 0 do
        if IsCompletedRoad(FieldList.Items[I]) then
        begin
          UnlockPointOfNode(FieldList.Items[I], True);
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
        // Is there road plan / work in progress?
        if IsPlan(FieldList.Items[I], tlRoadWork, ft_Road) then
          ActiveWorkers := ActiveWorkers + 1
        // Is there completed road?
        else if IsCompletedRoad(FieldList.Items[I]) then
        begin
          RequiredWorkers := RequiredWorkers - 1;
          CenterPoint := FieldList.Items[I]; // Actualize center point (distribution of workers by distance)
        end
        // When cannot place new plan try find another way by calling pathfinding
        else
        begin
          // Does we have free workers? (this condition cannot be earlier because we need detection of ActiveWorkers)
          if (FreeWorkers <= 0) then
            break;
          if not BuildField(I, ft_Road) then
          begin
            if ShortcutMode then
            begin
              UnlockPointOfNode(FieldList.Items[I], True);
              FieldList.Delete(I);
            end
            else
            begin
              UnlockNode(aNode, True);
              // If is not possible to connect 2 points by road destroy node
              if not fPlanner.GetRoadBetweenPoints(CenterPoint, FieldList.Items[0], FieldList, FieldType) then
              begin
                FieldList.Clear;
                Active := False;
              end;
              LockNode(aNode);
              RequiredWorkers := Min(MaxReqWorkers, FieldList.Count);
              Exit; // Node will be updated in next calling
            end;
          end;
        end;
      end;
      // Restrict max required workers
      RequiredWorkers := Min(Max(0, MaxReqWorkers - ActiveWorkers), RequiredWorkers);
    end;
  end;

  // Build Wine or Corn fields
  procedure BuildFields();
  var
    I, ActiveWorkers: Integer;
  begin
    ActiveWorkers := 0;
    with aNode do
    begin
      RequiredWorkers := FieldList.Count;
      for I := 0 to FieldList.Count - 1 do
      begin
        // Check if field already exists ...
        if   ((FieldType = ft_Wine) AND IsCompletedWine(FieldList.Items[I]))
          OR ((FieldType = ft_Corn) AND IsCompletedField(FieldList.Items[I])) then
        begin
          RequiredWorkers := RequiredWorkers - 1;
        end
        else if ((FieldType = ft_Wine) AND IsPlan(FieldList.Items[I], tlFieldWork, ft_Wine))
             OR ((FieldType = ft_Corn) AND IsPlan(FieldList.Items[I], tlFieldWork, ft_Corn)) then
        begin
          ActiveWorkers := ActiveWorkers + 1;
        end
        // ... else try build it
        else
        begin
          if (FreeWorkers <= 0) then
            break;
          BuildField(I, FieldType);
        end;
        // When node reached all plans disable it
        if (RequiredWorkers <= 0) OR (I = FieldList.Count-1) then
        begin
          // Only roads are unlocked
          //for K := 0 to FieldList.Count-1 do
          //  if   ((FieldType = ft_Wine) AND (IsCornField(FieldList.Items[I])))
          //    OR ((FieldType = ft_Corn) AND (IsWineField(FieldList.Items[I]))) then
          //    UnlockPointOfNode(FieldList.Items[K]);
          FieldList.Clear;
          Active := False;
        end;
      end;
      // Restrict max required workers
      RequiredWorkers := Min(Max(0, MaxReqWorkers - ActiveWorkers), RequiredWorkers);
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
      // Restrict max required workers
      RequiredWorkers := Min(MaxReqWorkers, RequiredWorkers);
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
end;
//}


// Build house na GA mode (for Runner)
// aHT: THouseType = type of house
// Result: TConstructionState = state of construction
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
    if fPlanner.GetHousePlan(False, False, aHT, Loc, HouseIdx) then
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


// Build house in standard game
// aUnlockprocedure: Boolean = build house as fast as possible (add max workers to construction, no remove trees in house plan mode)
// aHouseReservation: Boolean = plan house plan and create reservation but dont place it (roads and fields will be constructed)
// aIgnoreExistingPlans: Boolean = planner will ignore existing plans and find new place for house (-> allow to plan multiple houses of 1 type)
// aHT: THouseType = type of house
// Result: TConstructionState = state of construction
function TKMCityBuilder.BuildHouse(aUnlockProcedure, aHouseReservation, aIgnoreExistingPlans: Boolean; aHT: THouseType): TConstructionState;
var
  Output: TConstructionState;
  FieldsComplete: Boolean;
  I, Node1Idx, Node2Idx, HouseIdx: Integer;
  Loc: TKMPoint;
begin
  Output := cs_NoNodeAvailable;
  FieldsComplete := False;
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

  Output := cs_NoPlaceCanBeFound;
  if fPlanner.GetHousePlan(aUnlockProcedure, aIgnoreExistingPlans, aHT, Loc, HouseIdx) then
  begin
    // Check if we can place house by default KaM function
    if gHands[fOwner].CanAddHousePlan(Loc, aHT) then
    begin

      // Update reservation status
      if fPlanner.PlannedHouses[aHT].Plans[HouseIdx].HouseReservation then
      begin
        Output := cs_HouseReservation; // House is already reserved -> no nodes will be updated and workers still have nothing to do so we can build another house
        if not aHouseReservation then
        begin
          fPlanner.PlannedHouses[aHT].Plans[HouseIdx].HouseReservation := False;
          FieldsComplete := True; // Fields was completed in reservation (only for farm and wine)
        end;
      end;

      // if house is not reserved (or will be reserved in this tick)
      if not fPlanner.PlannedHouses[aHT].Plans[HouseIdx].HouseReservation then
      begin
        Output := cs_HousePlaced; // Nodes will be updated -> workers will have something to do
        gHands[fOwner].AddHousePlan(aHT, Loc); // Place house
        // Add avoid building for Barracks and Store (road will be build later in shortcut procedure)
        if ((aHT = ht_Store) OR (aHT = ht_Barracks)) AND (Loc.Y+2 < gTerrain.MapY) then
          for I := Loc.X-1 to Loc.X+1 do
            gAIFields.Influences.AvoidBuilding[Loc.Y+2, I] := 255;
        // Add road to node
        if fPlanner.GetRoadToHouse(aHT, HouseIdx, fBuildNodes[Node1Idx].FieldList, fBuildNodes[Node1Idx].FieldType)
           AND (fBuildNodes[Node1Idx].FieldList.Count > 0) then
        begin
          LockNode(fBuildNodes[Node1Idx]);
          with fBuildNodes[Node1Idx] do
          begin
            Active := True;
            RemoveTreesMode := False;
            ShortcutMode := False;
            MaxReqWorkers := Round(GA_BUILDER_BuildHouse_RoadMaxWork) + Byte(aUnlockProcedure) * 20;
            RequiredWorkers := Min(MaxReqWorkers, FieldList.Count);
            CenterPoint := FieldList[ FieldList.Count-1 ]; // Road node must start from exist house
          end;
        end;
        // Add field to node (if is required [ht_Farm, ht_Wineyard])
        if not FieldsComplete AND fPlanner.GetFieldToHouse(aHT, HouseIdx, fBuildNodes[Node2Idx].FieldList, fBuildNodes[Node2Idx].FieldType) then
        begin
          LockNode(fBuildNodes[Node2Idx]);
          with fBuildNodes[Node2Idx] do
          begin
            Active := True;
            RemoveTreesMode := False;
            ShortcutMode := False;
            MaxReqWorkers := Round(GA_BUILDER_BuildHouse_FieldMaxWork);
            RequiredWorkers := Min(MaxReqWorkers, FieldList.Count);
            CenterPoint := Loc;
          end;
        end;
        // Reserve house place
        if aHouseReservation then
        begin
          fPlanner.PlannedHouses[aHT].Plans[HouseIdx].HouseReservation := True;
          gHands[fOwner].RemHousePlan(Loc);
        end
        else
        begin
          fPlanner.PlannedHouses[aHT].Plans[HouseIdx].HouseReservation := False;
          fPlanner.PlannedHouses[aHT].Plans[HouseIdx].RemoveTreeInPlanProcedure := False;
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
          MaxReqWorkers := Round(GA_BUILDER_BuildHouse_RTPMaxWork);
          RequiredWorkers := Min(MaxReqWorkers, FieldList.Count); // Real count will be updated during building process
          CenterPoint := Loc;
        end;
      end
      // There is another problem...
      else
        Planner.RemovePlan(aHT, Loc);
    end
    else
      Planner.RemovePlan(aHT, Loc);
  end
  else
  begin
    Output := cs_NoPlaceCanBeFound;
  end;
  Result := Output;
end;


procedure TKMCityBuilder.ChooseHousesToBuild(aFreeWorkersCnt: Integer; aTick: Cardinal);
type
  TSetOfWare = set of TWareType;
  TSetOfHouseType = set of THouseType;
const

  //ht_Woodcutters,    ht_Quary,         ht_Sawmill,        ht_IronMine,      ht_GoldMine,
  //ht_CoalMine,       ht_IronSmithy,    ht_Metallurgists,  ht_Wineyard,      ht_Farm,
  //ht_Bakery,         ht_Mill,          ht_Tannery,        ht_Butchers,      ht_Swine,
  //ht_Swine,          ht_ArmorWorkshop, ht_ArmorSmithy,    ht_ArmorWorkshop, ht_ArmorSmithy,
  //ht_WeaponWorkshop, ht_WeaponSmithy,  ht_WeaponWorkshop, ht_WeaponSmithy,  ht_WeaponWorkshop,
  //ht_WeaponSmithy,   ht_Stables,       ht_FisherHut


  BASIC_HOUSES: TSetOfHouseType = [ht_School, ht_Barracks, ht_Inn, ht_MarketPlace, ht_Store];
  //BUILD_WARE: TSetOfWare = [wt_GoldOre, wt_Coal, wt_Gold, wt_Stone, wt_Trunk, wt_Wood];
  FOOD_WARE: TSetOfWare = [wt_Corn, wt_Flour, wt_Bread, wt_Pig, wt_Sausages, wt_Wine, wt_Fish];
  WEAPON_WARE: TSetOfWare = [wt_Skin, wt_Leather, wt_Horse, wt_IronOre, wt_Coal, wt_Steel, wt_Axe, wt_Bow, wt_Pike, wt_Armor, wt_Shield, wt_Sword, wt_Arbalet, wt_Hallebard, wt_MetalShield, wt_MetalArmor];
  BUILD_ORDER_WARE: array[0..5] of TWareType = (wt_Stone, wt_Gold, wt_GoldOre, wt_Coal, wt_Trunk, wt_Wood);
var
  StoneShortage, WoodShortage, TrunkShortage, GoldShortage: Boolean;
  MaxPlans: Integer;
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
    Result := Output AND ( (initHT = aHT) OR (fPlanner.PlannedHouses[aHT].Count = 0) );
  end;


  function AddToConstruction(aHT: THouseType; aUnlockProcedureRequired: Boolean = False; aIgnoreWareReserves: Boolean = False): TConstructionState;
  var
    UnlockProcedure, MaterialShortage: Boolean;
    FollowingHouse: THouseType;
    Output: TConstructionState;
  begin
    Output := cs_CannotPlaceHouse;
    UnlockProcedure := aUnlockProcedureRequired; // Unlock procedure = build house as soon as possible -> avoid to build house plan inside of tree (remove tree will take time)
    // Check if AI can build house (if is house blocked [by script] ignore it)
    if GetHouseToUnlock(UnlockProcedure, aHT, FollowingHouse) then
    begin
      MaterialShortage := not aIgnoreWareReserves AND (WoodShortage OR TrunkShortage OR StoneShortage);
      //MaterialShortage := False;
      if GA_PLANNER then
        Output := BuildHouse_GA_MODE(aHT)
      else
        Output := BuildHouse(UnlockProcedure OR (aHT = ht_Farm), MaterialShortage, MaterialShortage, aHT); // Farm must be placed outside of forest
    end
    else if (FollowingHouse <> ht_none) AND (gHands[fOwner].Stats.GetHouseQty(ht_School) > 0) then // Activate house reservation (only when is first school completed)
    begin
      Output := BuildHouse(True, True, False, FollowingHouse);
    end;
    Result := Output;
  end;


  function SelectHouse(const aSetOfWare: TSetOfWare): Boolean;
  const
    FRACTION_COEF = 20.0;
  var
    Output: Boolean;
    I: Integer;
    Priority: Single;
    Ware, WT, POM_WT: TWareType;
    WareOrder: array[0..5] of TWareType;
    WarePriority: array[0..5] of Single;
  begin
    Output := False;
    // Basic producing houses (secure resources for building)
    for I := Low(WareOrder) to High(WareOrder) do
    begin
      WareOrder[I] := wt_None;
      WarePriority[I] := 1000000; // Doesn't have to be initialized but in this case compilation throws warning
    end;
    // Find the most required house to be build
    for Ware in aSetOfWare do
    begin
      WT := Ware;
      if (RequiredHouses[ PRODUCTION[WT] ] > 0) then
      begin
        Priority := WareBalance[WT].Exhaustion - WareBalance[WT].Fraction * GA_BUILDER_ChooseHousesToBuild_FC;
        for I := Low(WareOrder) to High(WareOrder) do
          if (WT = wt_None) then
            break
          else if (WareOrder[I] = wt_None) OR (Priority < WarePriority[I]) then // Buble sort is best for few elements
          begin
            POM_WT := WT;
            WT := WareOrder[I];
            WareOrder[I] := POM_WT;
            SwapFloat(Priority, WarePriority[I]);
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
          MaxPlans := MaxPlans - 1;
          if (MaxPlans <= 0) then
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

  procedure SelectHouseBySetOrder();
  var
    I: Integer;
    WT: TWareType;
  begin
    // Find the most required house to be build
    for I := Low(BUILD_ORDER_WARE) to High(BUILD_ORDER_WARE) do
    begin
      WT := BUILD_ORDER_WARE[I];
      if (RequiredHouses[ PRODUCTION[WT] ] > 0) AND (WareBalance[WT].Exhaustion < 20) then
      begin
        // Try build required houses
        if (AddToConstruction(PRODUCTION[WT], False, True) = cs_HousePlaced) then
          MaxPlans := MaxPlans - 1;
        RequiredHouses[ PRODUCTION[WT] ] := 0; // Make sure that next cycle will not scan this house in this tick
      end;
      if (MaxPlans <= 0) then
        Break;
    end;
  end;

  procedure CheckHouseReservation();
  const
    RESERVATION_StoneShortage: TSetOfHouseType = [ht_Quary, ht_MarketPlace];
    RESERVATION_GoldShortage: TSetOfHouseType = [ht_School, ht_Barracks, ht_Inn, ht_MarketPlace, ht_Store, ht_Quary, ht_GoldMine, ht_CoalMine, ht_Metallurgists, ht_Woodcutters, ht_Sawmill];
    RESERVATION_WoodShortage: TSetOfHouseType = [ht_School, ht_Barracks, ht_Inn, ht_MarketPlace, ht_Store, ht_Quary, ht_GoldMine, ht_CoalMine, ht_Metallurgists, ht_Woodcutters, ht_Sawmill];
    RESERVATION_FullSet: TSetOfHouseType = [ht_ArmorSmithy, ht_ArmorWorkshop, ht_Bakery, ht_Barracks, ht_Butchers, ht_CoalMine, ht_Farm, ht_FisherHut, ht_GoldMine, ht_Inn, ht_IronMine, ht_IronSmithy, ht_Marketplace, ht_Metallurgists, ht_Mill, ht_Quary, ht_Sawmill, ht_School, ht_SiegeWorkshop, ht_Stables, ht_Store, ht_Swine, ht_Tannery, ht_TownHall, ht_WatchTower, ht_WeaponSmithy, ht_WeaponWorkshop, ht_Wineyard, ht_Woodcutters];
  var
    I: Integer;
    HT: THouseType;
    ActualHouseSet: TSetOfHouseType;
  begin
    if StoneShortage then
      ActualHouseSet := RESERVATION_StoneShortage
    else if GoldShortage then
      ActualHouseSet := RESERVATION_GoldShortage
    else if WoodShortage then
      ActualHouseSet := RESERVATION_WoodShortage
    else
      ActualHouseSet := RESERVATION_FullSet;
    for HT in ActualHouseSet do
      for I := 0 to fPlanner.PlannedHouses[HT].Count - 1 do
        with fPlanner.PlannedHouses[HT].Plans[I] do
          if not Placed AND (HouseReservation OR RemoveTreeInPlanProcedure)
             AND (cs_HousePlaced = AddToConstruction(HT,False,True)) then
          begin
            MaxPlans := MaxPlans - 1;
            RequiredHouses[HT] := 0;
          end;
  end;

const
  BUILD_TOWER_DELAY = 17 * 60 * 10; // 17 minutes before end of peace
var
  HT: THouseType;
begin
  StoneShortage := False;
  WoodShortage := False;
  GoldShortage := False;
  RequiredHouses := fPredictor.RequiredHouses;
  WareBalance := fPredictor.WareBalance;

  // Analyze basic force stats (max possible plans, construction ware, gold)
  MaxPlans := Ceil(aFreeWorkersCnt / 5);

  // Secure stone production (low delay -> take from Exhaustion)
  if (WareBalance[wt_Stone].Exhaustion < STONE_SHORTAGE) then
    StoneShortage := True;

  // Secure wood production (high delay -> take directly from ware balance)
  if (gHands[fOwner].Stats.GetWareBalance(wt_Trunk) < TRUNK_SHORTAGE) then
    TrunkShortage := True;

  // Secure wood production (high delay -> take directly from ware balance)
  if (gHands[fOwner].Stats.GetWareBalance(wt_Wood) < WOOD_SHORTAGE) then
    WoodShortage := True;

  // Make sure that gold will be produced ASAP (low delay -> take from Exhaustion)
  if (WareBalance[wt_Gold].Exhaustion < GOLD_SHORTAGE) then
    GoldShortage := True;

  CheckHouseReservation();
  if (MaxPlans <= 0) then
    Exit;

  // Basic houses (for city management)
  for HT in BASIC_HOUSES do
    if (RequiredHouses[HT] > 0) AND (AddToConstruction(HT, True, True) = cs_HousePlaced) then
    begin
      MaxPlans := MaxPlans - 1;
      if (MaxPlans <= 0) then
        Exit;
    end;

  HT := ht_WatchTower;
  if (not Planner.DefenceTowersPlanned OR (gHands[fOwner].Stats.GetHouseTotal(HT) < Planner.PlannedHouses[HT].Count))
    AND (aTick - gGame.GameOptions.Peacetime * 600 + BUILD_TOWER_DELAY > 0)
    AND (AddToConstruction(HT, True, True) = cs_HousePlaced) then
    begin
      MaxPlans := MaxPlans - 1;
      if (MaxPlans <= 0) then
        Exit;
    end;

  SelectHouseBySetOrder();

  if TrunkShortage AND WoodShortage then
    fPlanner.FindForestAround(KMPOINT_ZERO, True);

  // Build woodcutter when is forest near new house (or when is woodcutter destroyed but this is not primarly intended)
  HT := ht_Woodcutters;
  if (gHands[fOwner].Stats.GetHouseTotal(HT) < fPlanner.PlannedHouses[HT].Count)
    AND (AddToConstruction(HT, True, True) = cs_HousePlaced) then
    begin
      MaxPlans := MaxPlans - 1;
      RequiredHouses[ht_Woodcutters] := 0;
    end;

  if (MaxPlans > 0) then
  begin
    SelectHouse(FOOD_WARE);
    //if (MaxPlans > 0) then
      SelectHouse(WEAPON_WARE);
  end;
end;




procedure TKMCityBuilder.CreateShortcuts();
const
  MAX_SHORTCUTS_PER_HOUSE_TYPE = 2;
  MAX_DISTANCE_TO_ALL_HOUSES = 8;
  MAX_WORKERS_FOR_NODE = 4;
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
          MaxReqWorkers := Round(GA_BUILDER_CreateShortcuts_MaxWork);//MAX_WORKERS_FOR_NODE;
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
  // Don't build shortcuts with low Exhaustion
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
  if (gHands[fOwner].Stats.GetWareBalance(wt_Wood) < WOOD_SHORTAGE) then
  begin
    aBalanceText := aBalanceText + '|';
    if (gHands[fOwner].Stats.GetWareBalance(wt_Trunk) < 5) then // Maybe we have trunk but just not sawmill
      aBalanceText := aBalanceText + 'Chop-Only required ';
    aBalanceText := aBalanceText + 'Wood shortage';
  end;
  if (fPredictor.WareBalance[wt_Gold].Exhaustion < GOLD_SHORTAGE) then
    aBalanceText := aBalanceText + '|Gold shortage';
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


{
var
  GA_BUILDER_WORKER_COEF : Single = 8.516485214;
  GA_BUILDER_EXHAUSTION_ARR: array[WARE_MIN..WARE_MAX] of Single = (
    1.597312808, 2.605776548, 6.652187347, 4.734027863, 4.655752659, 2.305694342, 3.052431107, 2.866589546, 0.3297367692, 4.905310631, 4.637497902, 1.792099953, 1.758574486, 8.000164986, 4.496774197, 9.744778633, 8.297982216, 7.661552429, 1.597817659, 4.179779053, 1.837774515, 9.829838753, 5.807341576, 1.473491907, 5.093095303, 0.7045341134, 2.986746073, 1.279728413
  );
  GA_BUILDER_FRACTION_ARR: array[WARE_MIN..WARE_MAX] of Single = (
    52.44208145, 97.94073486, 81.27591705, 14.42814064, 39.30894089, 23.50471687, 3.592087984, 90.95751953, 5.030199528, 14.30461979, 15.02885342, 23.04277039, 38.70484161, 34.08721161, 46.43330002, 36.89776611, 36.55716705, 61.80127335, 52.2832222, 64.24739075, 1.925330043, 89.33174133, 29.77691841, 100, 38.04268646, 88.26511383, 46.45424652, 85.67434692
  );
  // Length 30
  //GA_BUILDER_HOUSE_ARR: array[HOUSE_MIN..HOUSE_MAX] of Single = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29);

  //function ChooseHousesToBuildGA(aWorkerCnt: Integer): Boolean;


function TKMCityBuilder.ChooseHousesToBuildGA(aWorkerCnt: Integer): Boolean;
const
  BASIC_HOUSES: set of THouseType = [ht_School, ht_Barracks, ht_Inn, ht_MarketPlace, ht_Store];
var
  MaxPlans: Integer;
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
      if GA_PLANNER then
        Output := BuildHouse_GA_MODE(aHT)
      else
        Output := BuildHouse(UnlockProcedure OR (aHT = ht_Farm), False, False, aHT); // Farms should be build as soon as possible
    end
    else if (FollowingHouse <> ht_none) AND (gHands[fOwner].Stats.GetHouseQty(ht_School) > 0) then // Activate house reservation (only when is first school completed)
    begin
      Output := BuildHouse(True, True, False, FollowingHouse);
    end;
    Result := Output;
  end;

  procedure SelectBestHouses();
  var
    I: Integer;
    WT, HighWT: TWareType;
    HighPriority: Single;
    WarePriorityArr: array[WARE_MIN..WARE_MAX] of Single;
  begin
    for WT := Low(WarePriorityArr) to High(WarePriorityArr) do
      WarePriorityArr[WT] := ( + WareBalance[WT].Fraction * GA_BUILDER_FRACTION_ARR[WT] *1000
                               - WareBalance[WT].Exhaustion * GA_BUILDER_EXHAUSTION_ARR[WT]
                             ) * RequiredHouses[ PRODUCTION[WT] ];

    for I := 0 to 4 do
    begin
      if (MaxPlans <= 0) then
        Exit;
      HighPriority := 0;
      for WT := Low(WarePriorityArr) to High(WarePriorityArr) do
        if (HighPriority < WarePriorityArr[WT]) then
        begin
          HighPriority := WarePriorityArr[WT];
          HighWT := WT;
        end;
      if (HighPriority = 0) then
        Exit;
      WarePriorityArr[HighWT] := 0;
      // Try build required houses
      case AddToConstruction( PRODUCTION[HighWT] ) of
        cs_NoNodeAvailable: Exit;
        cs_HouseReservation, cs_RemoveTreeProcedure: begin end;
        cs_HousePlaced:
        begin
          MaxPlans := MaxPlans - 1;
          RequiredHouses[  PRODUCTION[HighWT]  ] := 0; // Make sure that next node will not scan this house in this tick
        end;
        cs_CannotPlaceHouse:
        begin
          RequiredHouses[  PRODUCTION[HighWT]  ] := 0; // Make sure that next node will not scan this house in this tick
        end;
      end;

    end;
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
            MaxPlans := MaxPlans - 1;
            RequiredHouses[HT] := 0;
          end;
  end;
var
  HT: THouseType;
begin
  RequiredHouses := fPredictor.RequiredHouses;
  WareBalance := fPredictor.WareBalance;

  CheckHouseReservation();
  MaxPlans := Ceil(aWorkerCnt / GA_BUILDER_WORKER_COEF);
  if (MaxPlans <= 0) then
    Exit;

  // Basic houses (for city management)
  for HT in BASIC_HOUSES do
    if (RequiredHouses[HT] > 0) AND (AddToConstruction(HT, True) = cs_HousePlaced) then
      if (MaxPlans <= 0) then
        break;

  SelectBestHouses();

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


end.
