unit KM_CityBuilder;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_CommonUtils, KM_Points, KM_CommonTypes,
  KM_PathfindingRoad, //KM_AISetup,
  KM_ResHouses, KM_BuildList,
  KM_CityPlanner, KM_CityPredictor, KM_Eye;

type

  TBuildNode = record
    Active, RemoveTreesMode: Boolean;
    FreeWorkers, RequiredWorkers, MaxReqWorkers, ResponsibleHouseIdx: Integer;
    ResponsibleHouseType: THouseType;
    CenterPoint: TKMPoint;
    FieldType: TFieldType; //ft_Corn,ft_Wine  ft_Road
    FieldList: TKMPointList;
  end;

  //
  TKMCityBuilder = class
  private
    fOwner: TKMHandIndex;
    fBuildNodes: array of TBuildNode;

    fPlanner: TKMCityPlanner;

    procedure UpdateBuildNode(var aNode: TBuildNode);

    function BuildHouse(aUnlockProcedure, aHouseReservation: Boolean; aHT: THouseType): Boolean;
    function BuildHouse_GA_MODE(aHT: THouseType): Boolean;
    procedure LockHouseLoc(aHT: THouseType; aLoc: TKMPoint);
    procedure UnlockHouseLoc(aHT: THouseType; aLoc: TKMPoint);
    procedure LockNode(var aNode: TBuildNode);
    procedure UnlockPointOfNode(aPoint: TKMPoint);

    procedure CreateShortcuts();
  public
    constructor Create(aPlayer: TKMHandIndex);
    destructor Destroy(); override;

    property Planner: TKMCityPlanner read fPlanner;

    procedure AfterMissionInit(out aGoldMineCnt, aIronMineCnt, aFieldCnt, aBuildCnt: Integer);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);

    procedure UpdateState(aTick: Cardinal; out aFreeWorkersCnt: Integer);
    procedure UpdateBuildNodes(out aFreeWorkersCnt: Integer);
    function ChooseHousesToBuild(aMaxCnt: Integer; var aRequiredHouses: TRequiredHousesArray; aWareBalance: TWareBalanceArray): Boolean;
    procedure ChooseHousesToBuildPOM(aMaxCnt: Integer; var aRequiredHouses: TRequiredHousesArray; aWareBalance: TWareBalanceArray);

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
constructor TKMCityBuilder.Create(aPlayer: TKMHandIndex);
begin
  inherited Create;

  fOwner := aPlayer;
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
    fBuildNodes[I].RemoveTreesMode := False;
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


procedure TKMCityBuilder.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.Write(fOwner);
  fPlanner.Save(SaveStream);
end;


procedure TKMCityBuilder.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.Read(fOwner);
  fPlanner.Load(LoadStream);
end;


procedure TKMCityBuilder.UpdateState(aTick: Cardinal; out aFreeWorkersCnt: Integer);
begin
  if (aTick mod 12 = fOwner) then
  begin
    fPlanner.UpdateState(aTick);
    UpdateBuildNodes(aFreeWorkersCnt);
  end;
end;


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
  I, K, ClosestIdx, ClosestDist, Dist: Integer;
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
    for K := Low(fBuildNodes) to High(fBuildNodes) do
      if (fBuildNodes[K].RequiredWorkers > 0) then
      begin
        Dist := KMDistanceAbs(WorkersPos[aFreeWorkersCnt - 1], fBuildNodes[K].CenterPoint);
        if (Dist < ClosestDist) then
        begin
          ClosestDist := Dist;
          ClosestIdx := K;
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

  CreateShortcuts();

  // Update nodes
  for I := Low(fBuildNodes) to High(fBuildNodes) do
    if fBuildNodes[I].Active then
      UpdateBuildNode(fBuildNodes[I]);
end;


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
        else
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


function TKMCityBuilder.BuildHouse(aUnlockProcedure, aHouseReservation: Boolean; aHT: THouseType): Boolean;
var
  Output: Boolean;
  I, Node1Idx, Node2Idx, HouseIdx: Integer;
  Loc: TKMPoint;
begin
  Output := False;
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
    // Check if we can place house
    Output := gHands[fOwner].CanAddHousePlan(Loc, aHT);
    if Output then
    begin
      // Update reservation status
      if fPlanner.PlannedHouses[aHT,HouseIdx].HouseReservation AND not aHouseReservation then
      begin
        fPlanner.PlannedHouses[aHT,HouseIdx].HouseReservation := False;
        UnLockHouseLoc(aHT, Loc);
      end;
      gHands[fOwner].AddHousePlan(aHT, Loc); // Place house
      if not fPlanner.PlannedHouses[aHT,HouseIdx].HouseReservation then
      begin
        // Add road to node
        if fPlanner.GetRoadToHouse(aHT, HouseIdx, fBuildNodes[Node1Idx].FieldList, fBuildNodes[Node1Idx].FieldType) then
        begin
          LockNode(fBuildNodes[Node1Idx]);
          with fBuildNodes[Node1Idx] do
          begin
            Active := True;
            RemoveTreesMode := False;
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
            MaxReqWorkers := 5 + Byte(aUnlockProcedure) * 20;
            RequiredWorkers := Min(MaxReqWorkers, FieldList.Count);
            CenterPoint := Loc;
          end;
        end;
      end;
      // Reserve house place
      if aHouseReservation then
      begin
        fPlanner.PlannedHouses[aHT,HouseIdx].HouseReservation := True;
        gHands[fOwner].RemHousePlan(Loc);
        LockHouseLoc(aHT, Loc);
      end;
    end
    else
    begin
      // Remove tree procedure is already active
      if (fPlanner.PlannedHouses[aHT,HouseIdx].RemoveTreeInPlanProcedure) then
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
          MaxReqWorkers := 5;
          RequiredWorkers := Min(MaxReqWorkers, FieldList.Count); // Real count will be updated during building process
          CenterPoint := Loc;
          ResponsibleHouseIdx := HouseIdx;
          ResponsibleHouseType := aHT;
        end;
        LockHouseLoc(aHT, Loc);
      end
      // There is another problem...
      else
      begin
        //RemovePlan(aHT, Loc);
        // Remove house plan from city planner
      end;
    end;
  end
  else
  begin
    // new house cannot be added
  end;
  Result := Output
end;


function TKMCityBuilder.BuildHouse_GA_MODE(aHT: THouseType): Boolean;
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
  //fPlanner.UpdateState(0);// DELETE (ADD SOMEWHERE ELSE)
  Result := Output;
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


function TKMCityBuilder.ChooseHousesToBuild(aMaxCnt: Integer; var aRequiredHouses: TRequiredHousesArray; aWareBalance: TWareBalanceArray): Boolean;
type
  TSetOfWare = set of TWareType;
const
  BASIC_HOUSES: array[0..4] of THouseType = (ht_School, ht_Barracks, ht_Inn, ht_MarketPlace, ht_Store);
  BUILD_WARE: TSetOfWare = [wt_Gold, wt_GoldOre, wt_Coal, wt_Stone, wt_Trunk, wt_Wood];
  FOOD_WARE: TSetOfWare = [wt_Corn, wt_Flour, wt_Bread, wt_Pig, wt_Sausages, wt_Wine, wt_Fish];
  WEAPON_WARE: TSetOfWare = [wt_Skin, wt_Leather, wt_Horse, wt_IronOre, wt_Coal, wt_Steel, wt_Axe, wt_Bow, wt_Pike, wt_Armor, wt_Shield, wt_Sword, wt_Arbalet, wt_Hallebard, wt_MetalShield, wt_MetalArmor];

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
    //aHT := ht_Farm;// DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG
    //Result := True;// DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG
    Result := Output AND ( (initHT = aHT) OR ((aRequiredHouses[aHT] > 0) AND (gHands[fOwner].Stats.GetHouseTotal(aHT) = 0)) );
  end;

  function AddToConstruction(aHT: THouseType; aUnlockProcedureRequired: Boolean = False): Boolean;
  var
    Output, UnlockProcedure: Boolean;
    FollowingHouse: THouseType;
  begin
    Output := False;
    UnlockProcedure := aUnlockProcedureRequired; // Unlock procedure = build house as soon as possible -> avoid to build house plan inside of tree (remove tree will take time)
    // Check if AI can build house (if is house blocked [by script] ignore it)
    if GetHouseToUnlock(UnlockProcedure, aHT, FollowingHouse) then
    begin
      aMaxCnt := aMaxCnt - 1;
      if not GA_PLANNER then
        Output := BuildHouse(UnlockProcedure, False, aHT)
      else
        Output := BuildHouse_GA_MODE(aHT);
    end;
    //else if (FollowingHouse <> ht_none) then
    //  BuildHouse(True, True, FollowingHouse);
    aRequiredHouses[aHT] := 0; // Make sure that next node will not scan this house in this tick
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
    //for Ware := WARE_MIN to WARE_MAX do
    for Ware in aSetOfWare do
    begin
      WT := Ware;
      if (aRequiredHouses[ PRODUCTION[WT] ] > 0) then
      begin
        Priority := aWareBalance[WT].Exhaustion - aWareBalance[WT].Fraction * FRACTION_COEF;
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
    I := 0;
    for I := Low(WareOrder) to High(WareOrder) do
      if (WareOrder[I] = wt_None) then
        break
      else if AddToConstruction(PRODUCTION[ WareOrder[I] ]) then
      begin
        Output := True;
        aMaxCnt := aMaxCnt - 1;
        if (aMaxCnt <= 0) then
          break;
      end;
    Result := Output;
  end;

var
  Output: Boolean;
  I, POMCoal: Integer;
begin
  //aRequiredHouses[ht_Farm] := 30; //DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG DEBUG
  Output := False;
  // Basic houses (for city management)
  for I := Low(BASIC_HOUSES) to High(BASIC_HOUSES) do
    if (aRequiredHouses[ BASIC_HOUSES[I] ] > 0) AND AddToConstruction(BASIC_HOUSES[I], True) then
    begin
      Output := True;
      if (aMaxCnt <= 0) then
        break;
    end;

  //Result := SelectHouse(False);
  //{
  POMCoal := aRequiredHouses[ht_CoalMine];
  aRequiredHouses[ht_CoalMine] := Max(0,gHands[fOwner].Stats.GetHouseTotal(ht_GoldMine)-gHands[fOwner].Stats.GetHouseTotal(ht_CoalMine));
  if not Output AND SelectHouse(BUILD_WARE) then
    Output := True;

  aRequiredHouses[ht_CoalMine] := POMCoal;
  if not Output then
    Output := SelectHouse(FOOD_WARE) OR SelectHouse(WEAPON_WARE);

  Result := Output;
  //}
end;


// Old test version
procedure TKMCityBuilder.ChooseHousesToBuildPOM(aMaxCnt: Integer; var aRequiredHouses: TRequiredHousesArray; aWareBalance: TWareBalanceArray);
  function GetHouseToUnlock(var aUnlockProcedure: Boolean; var aHT: THouseType): Boolean;
  var
    Output: Boolean;
    POM_HT: THouseType;
  begin
    Output := True;
    aUnlockProcedure := False;
    // Repeat until is avaiable house finded (to unlock target house)
    POM_HT := aHT;
    while Output AND not gHands[fOwner].Locks.HouseCanBuild(aHT) do
    begin
      aUnlockProcedure := True;
      if gHands[fOwner].Locks.HouseBlocked[aHT] then // House is blocked -> unlock impossible
        Output := False
      else
        aHT := gRes.Houses[aHT].ReleasedBy; // House have to be unlocked by this house
    end;
    // Output = false only in case that house is already under construction OR is blocked by script / settings from the map editor
    Result := Output AND ( (POM_HT = aHT) OR (gHands[fOwner].Stats.GetHouseTotal(aHT) = 0));
  end;
  //gHands[fOwner].Locks.HouseCanBuild(aHouse)
  //gHands[fOwner].Locks.HouseBlocked[ht_Quary]
  function AddToConstruction(aHT: THouseType): Boolean;
  var
    UnlockProcedure: Boolean;
  begin
    // Check if AI can build house (if is house blocked [by script] ignore it)
    if GetHouseToUnlock(UnlockProcedure, aHT) then
    begin
      BuildHouse(UnlockProcedure, false, aHT);
      //BuildHouse_DEBUG_MODE(aHT);
      aRequiredHouses[aHT] := aRequiredHouses[aHT] - 1;
      aMaxCnt := aMaxCnt - 1;
    end;
    aRequiredHouses[aHT] := 0; // Make sure that next node will not scan this house in this tick
    Result := (aMaxCnt > 0);
  end;

var
  I: Integer;
  BestDerivation: Single;
  WT, BestWT: TWareType;
const
  BASIC_HOUSES: array[0..4] of THouseType = (ht_School, ht_Barracks, ht_Inn, ht_Store, ht_MarketPlace);
  BASIC_WARE: array[0..5] of TWareType = (wt_Gold, wt_GoldOre, wt_Coal, wt_Stone, wt_Trunk, wt_Wood); //,
  //IRON_WEAPONS: array[..] of TWareType = ();
  //FOOD:
  //WEAPONS
  {
    wt_Trunk,          wt_Stone,         wt_Wood,           wt_IronOre,      wt_GoldOre,
    wt_Coal,           wt_Steel,         wt_Gold,           wt_Wine,         wt_Corn,
    wt_Bread,          wt_Flour,         wt_Leather,        wt_Sausages,     wt_Pig,
    wt_Skin,
  }
begin
  for I := Low(BASIC_HOUSES) to High(BASIC_HOUSES) do
    if (aRequiredHouses[ BASIC_HOUSES[I] ] > 0) then
    begin
      AddToConstruction(BASIC_HOUSES[I]);
      if aMaxCnt <= 0 then
        Exit;
    end;

  BestDerivation := 0;
  for I := Low(BASIC_WARE) to High(BASIC_WARE) do
    if (aRequiredHouses[  PRODUCTION[ BASIC_WARE[I] ]  ] > 0) then
    begin
      AddToConstruction(PRODUCTION[ BASIC_WARE[I] ]);
      {
      if (BASIC_WARE[I] = wt_Gold) AND (aWareBalance[wt_GoldOre].Derivation > aWareBalance[wt_Coal].Derivation) then
      begin
        aRequiredHouses[ht_CoalMine] := Max(0, gHands[fOwner].Stats.GetHouseTotal(ht_GoldMine) - gHands[fOwner].Stats.GetHouseTotal(ht_CoalMine));
        if (aRequiredHouses[ht_CoalMine] > 0) then
          AddToConstruction(ht_CoalMine);
      end;
      //}
      if aMaxCnt <= 0 then
        Exit;
    end;
  {
      if (aWareBalance[BASIC_WARE[I]].Derivation < BestDerivation) then
      begin
        BestWT := BASIC_WARE[I];
        BestDerivation := aWareBalance[BASIC_WARE[I]].Derivation;
      end;
  if (BestDerivation < 0) then
    if not AddToConstruction(PRODUCTION[BestWT]) then
      Exit;
  //}
  {
  while (aMaxCnt > 0) do
  begin
    BestDerivation := 0;
    for WT := Low(aWareBalance) to High(aWareBalance) do
    begin
      if WT in [wt_Shield,  wt_MetalShield, wt_Armor,     wt_MetalArmor,
                wt_Axe,     wt_Sword,   wt_Pike,        wt_Hallebard, wt_Bow,
                wt_Arbalet, wt_Horse,   wt_Fish] then
        continue;
      if (aWareBalance[WT].Derivation < BestDerivation) then
      begin
        BestWT := WT;
        BestDerivation := aWareBalance[WT].Derivation;
      end;
    end;
    if (BestDerivation < 0) then
    begin
      aWareBalance[BestWT].Derivation := 0;
      if not AddToConstruction(PRODUCTION[BestWT]) then
        Exit;
    end
    else
      break;
  end;
  //}
end;


procedure TKMCityBuilder.CreateShortcuts();
const
  MAX_SHORTCUTS_PER_HOUSE_TYPE = 2;
  MAX_DISTANCE_TO_ALL_HOUSES = 8;
  HOUSE_CONNECTION: array[HOUSE_MIN..HOUSE_MAX] of array[0..2] of THouseType = (
    {ht_ArmorSmithy}    ( ht_IronSmithy,    ht_CoalMine,     ht_Barracks    ),
    {ht_ArmorWorkshop}  ( ht_Tannery,       ht_Barracks,     ht_None        ),
    {ht_Bakery}         ( ht_Inn,           ht_Store,        ht_Mill        ),
    {ht_Barracks}       ( ht_School,        ht_None,         ht_None        ),
    {ht_Butchers}       ( ht_Inn,           ht_Store,        ht_Swine       ),
    {ht_CoalMine}       ( ht_None,          ht_None,         ht_None        ),
    {ht_Farm}           ( ht_None,          ht_None,         ht_None        ),
    {ht_FisherHut}      ( ht_None,          ht_None,         ht_None        ),
    {ht_GoldMine}       ( ht_Metallurgists, ht_None,         ht_None        ),
    {ht_Inn}            ( ht_Store,         ht_Inn,          ht_None        ),
    {ht_IronMine}       ( ht_IronSmithy,    ht_None,         ht_None        ),
    {ht_IronSmithy}     ( ht_CoalMine,      ht_WeaponSmithy, ht_ArmorSmithy ),
    {ht_Marketplace}    ( ht_Store,         ht_None,         ht_None        ),
    {ht_Metallurgists}  ( ht_School,        ht_GoldMine,     ht_CoalMine    ),
    {ht_Mill}           ( ht_Farm,          ht_Bakery,       ht_None        ),
    {ht_Quary}          ( ht_Store,         ht_None,         ht_None        ),
    {ht_Sawmill}        ( ht_ArmorWorkshop, ht_Store,        ht_None        ),
    {ht_School}         ( ht_Metallurgists, ht_Store,        ht_Barracks    ),
    {ht_SiegeWorkshop}  ( ht_IronSmithy,    ht_Sawmill,      ht_Store       ),
    {ht_Stables}        ( ht_Farm,          ht_Barracks,     ht_None        ),
    {ht_Store}          ( ht_Inn,           ht_Barracks,     ht_School      ),
    {ht_Swine}          ( ht_Farm,          ht_Butchers,     ht_None        ),
    {ht_Tannery}        ( ht_ArmorWorkshop, ht_Swine,        ht_None        ),
    {ht_TownHall}       ( ht_Metallurgists, ht_Store,        ht_None        ),
    {ht_WatchTower}     ( ht_None,          ht_None,         ht_None        ),
    {ht_WeaponSmithy}   ( ht_IronSmithy,    ht_CoalMine,     ht_Barracks    ),
    {ht_WeaponWorkshop} ( ht_Sawmill,       ht_Barracks,     ht_None        ),
    {ht_Wineyard}       ( ht_Inn,           ht_None,         ht_None        ),
    {ht_Woodcutters}    ( ht_None,          ht_None,         ht_None        )
  );
var
  Locs: TKMPointTagList;

  procedure PlanRoad(aBaseLoc: TKMPoint; aAllLocs: Boolean = False);
  var
    I,K, cnt: Integer;
  begin
    if (Locs.Count > 0) then
    begin
      Locs.SortByTag();
      K := 0;
      cnt := 0;
      for I := 0 to Locs.Count-1 do
      begin
        while (K < High(fBuildNodes)) AND fBuildNodes[K].Active do
          K := K + 1;
        if fPlanner.GetRoadBetweenPoints(Locs.Items[I], aBaseLoc, fBuildNodes[K].FieldList, fBuildNodes[K].FieldType) then
        begin
          cnt := cnt + 1;
          with fBuildNodes[K] do
          begin
            Active := True;
            RemoveTreesMode := False;
            MaxReqWorkers := 5;
            RequiredWorkers := Min(RequiredWorkers, FieldList.Count);
            CenterPoint := FieldList.Items[0];
          end;
          if not aAllLocs AND (cnt = MAX_SHORTCUTS_PER_HOUSE_TYPE) then
            break;
        end;
      end;
    end;
  end;

var
  Check: boolean;
  I,K,Dist: Integer;
  HT, BaseHT: THouseType;
  BaseLoc: TKMPoint;
  PlannedHouses: TPlannedHousesArray;
begin
  // Don't build shortcuts when is negative stone derivation
  if   (gHands.Hands[fOwner].AI.CityManagement.Predictor.WareBalance[wt_Stone].Exhaustion < 60)
    //OR (gHands.Hands[fOwner].AI.CityManagement.Predictor.WareBalance[wt_Wood].Exhaustion < 60)
    OR (gHands.Hands[fOwner].AI.CityManagement.Predictor.WareBalance[wt_Gold].Exhaustion < 60) then
    Exit;

  // Check if there are free nodes
  K := 0;
  for I := Low(fBuildNodes) to High(fBuildNodes) do
    if not fBuildNodes[I].Active then
      K := K + 1;
  if (K < (Length(fBuildNodes) shr 1)) then
    Exit;

  // Find house which was not checked for shortcuts
  PlannedHouses := fPlanner.PlannedHouses;
  Check := False;
  for HT := Low(PlannedHouses) to High(PlannedHouses) do
  begin
    if (HT = ht_Woodcutters) then
      continue;
    for I := Low(PlannedHouses[HT]) to High(PlannedHouses[HT]) do
      if PlannedHouses[HT,I].Placed AND not PlannedHouses[HT,I].ShortcutsCompleted then
      begin
        Check := True;
        break;
      end;
    if Check then
      break;
  end;

  if not Check then
    Exit;

  PlannedHouses[HT,I].ShortcutsCompleted := True;
  BaseLoc := KMPointBelow(PlannedHouses[HT,I].Loc);
  BaseHT := HT;

  Locs := TKMPointTagList.Create();
  try
    if (BaseHT = ht_Store) OR (BaseHT = ht_Barracks) then
      for I := BaseLoc.X-1 to BaseLoc.X+1 do
      begin
        gAIFields.Influences.AvoidBuilding[BaseLoc.Y+1, I] := 255;
        if gHands[fOwner].CanAddFieldPlan(KMPoint(I, BaseLoc.Y+1) , ft_Road) then
          gHands[fOwner].BuildList.FieldworksList.AddField(KMPoint(I, BaseLoc.Y+1) , ft_Road);
      end;
    for I := Low(HOUSE_CONNECTION[BaseHT]) to High(HOUSE_CONNECTION[BaseHT]) do
    begin
      HT := HOUSE_CONNECTION[BaseHT,I];
      if (HT = ht_None) then
        break;

      Locs.Clear();
      for K := Low(PlannedHouses[HT]) to High(PlannedHouses[HT]) do
        if PlannedHouses[HT,K].Placed then
          Locs.Add( KMPointBelow(PlannedHouses[HT,K].Loc), KMDistanceAbs(PlannedHouses[HT,K].Loc, BaseLoc) );
      PlanRoad(BaseLoc, False);
    end;
    Locs.Clear();
    for HT := Low(PlannedHouses) to High(PlannedHouses) do
    begin
      if (HT = ht_Woodcutters) then
        continue;
      for I := Low(PlannedHouses[HT]) to High(PlannedHouses[HT]) do
        if PlannedHouses[HT,I].Placed then
        begin
          Dist := KMDistanceAbs(BaseLoc, KMPointBelow(PlannedHouses[HT,I].Loc));
          if (Dist <> 0) AND (Dist < MAX_DISTANCE_TO_ALL_HOUSES) then
            Locs.Add( KMPointBelow(PlannedHouses[HT,I].Loc), Dist );
        end;
    end;
    PlanRoad(BaseLoc, True);
  finally
    Locs.Free;
  end;
end;


procedure TKMCityBuilder.LogStatus(var aBalanceText: UnicodeString);
var
  I, cnt: Integer;
begin
  cnt := 0;
  for I := Low(fBuildNodes) to High(fBuildNodes) do
    if fBuildNodes[I].Active then
      cnt := cnt + 1;
  aBalanceText := aBalanceText + '|| Active nodes:' + IntToStr(cnt);
end;

procedure TKMCityBuilder.Paint();
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
  Color: Cardinal;
  Point: TKMPoint;
begin
  for I := Low(fBuildNodes) to High(fBuildNodes) do
    if fBuildNodes[I].Active then
    begin
      case fBuildNodes[I].FieldType of
        ft_Corn: Color := COLOR_GREEN_Field;
        ft_Wine: Color := COLOR_GREEN_Wine;
        ft_Road: Color := COLOR_YELLOW;
      end;
      if fBuildNodes[I].RemoveTreesMode then
        Color := COLOR_BLUE;
      for K := 0 to fBuildNodes[I].FieldList.Count - 1 do
      begin
        Point := fBuildNodes[I].FieldList.Items[K];
        gRenderAux.Quad(Point.X, Point.Y, Color);
      end;
    end;
end;

{
procedure TKMCityBuilder.PlanDefenceTowers;
const
  DISTANCE_BETWEEN_TOWERS = 10;
var
  P: TKMHand;
  Outline1, Outline2: TKMWeightSegments;
  I, K, DefCount: Integer;
  Loc: TKMPoint;
  SegLength, Ratio: Single;
begin
  if fDefenceTowersPlanned then Exit;
  fDefenceTowersPlanned := True;
  P := gHands[fOwner];
  if not P.Locks.HouseCanBuild(ht_WatchTower) then Exit;

  //Get defence Outline with weights representing how important each segment is
  gAIFields.NavMesh.GetDefenceOutline(fOwner, Outline1, Outline2);
  //Make list of defence positions
  for I := 0 to High(Outline2) do
  begin
    //Longer segments will get several towers
    SegLength := KMLength(Outline2[I].A, Outline2[I].B);
    DefCount := Max(Trunc(SegLength / DISTANCE_BETWEEN_TOWERS), 1);
    for K := 0 to DefCount - 1 do
    begin
      Ratio := (K + 1) / (DefCount + 1);
      Loc := KMPointRound(KMLerp(Outline2[I].A, Outline2[I].B, Ratio));
      fDefenceTowers.Add(Loc, Trunc(1000*Outline2[I].Weight));
    end;
  end;
  fDefenceTowers.SortByTag;
  fDefenceTowers.Inverse; //So highest weight is first
end;


procedure TKMCityBuilder.TryBuildDefenceTower;
const
  SEARCH_RAD = 6;
  MAX_ROAD_DISTANCE = 25;
var
  P: TKMHand;
  IY, IX: Integer;
  Loc: TKMPoint;
  DistSqr, BestDistSqr: Integer;
  BestLoc: TKMPoint;

  NodeList: TKMPointList;
  H: TKMHouse;
  LocTo: TKMPoint;
  RoadConnectID: Byte;
  RoadExists: Boolean;
begin
  P := gHands[fOwner];
  //Take the first tower from the list
  Loc := fDefenceTowers[0];
  fDefenceTowers.Delete(0);
  //Look for a place for the tower
  BestDistSqr := High(BestDistSqr);
  BestLoc := KMPOINT_ZERO;
  for IY := Max(1, Loc.Y-SEARCH_RAD) to Min(gTerrain.MapY, Loc.Y+SEARCH_RAD) do
    for IX := Max(1, Loc.X-SEARCH_RAD) to Min(gTerrain.MapX, Loc.X+SEARCH_RAD) do
    begin
      DistSqr := KMLengthSqr(Loc, KMPoint(IX, IY));
      if (DistSqr < BestDistSqr) and P.CanAddHousePlanAI(IX, IY, ht_WatchTower, False) then
      begin
        BestLoc := KMPoint(IX, IY);
        BestDistSqr := DistSqr;
      end;
    end;
  if (BestLoc.X > 0) then
  begin
    //See if the road required is too long (tower might be across unwalkable terrain)
    H := P.Houses.FindHouse(ht_Any, BestLoc.X, BestLoc.Y, 1, False);
    if H = nil then Exit; //We are screwed, no houses left
    LocTo := H.PointBelowEntrance;

    //Find nearest complete house to get the road connect ID
    H := P.Houses.FindHouse(ht_Any, BestLoc.X, BestLoc.Y, 1, True);
    if H = nil then Exit; //We are screwed, no houses left
    RoadConnectID := gTerrain.GetRoadConnectID(H.PointBelowEntrance);

    NodeList := TKMPointList.Create;
    RoadExists := fPathFindingRoad.Route_ReturnToWalkable(BestLoc, LocTo, RoadConnectID, NodeList);
    //If length of road is short enough, build the tower
    if RoadExists and (NodeList.Count <= MAX_ROAD_DISTANCE) then
    begin
      gHands[fOwner].AddHousePlan(ht_WatchTower, BestLoc);
      TryConnectToRoad(KMPointBelow(BestLoc));
    end;
    NodeList.Free;
  end;
end;
//}

end.
