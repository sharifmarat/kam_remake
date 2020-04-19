{
Artificial intelligence
@author: Martin Toupal
@e-mail: poznamenany@gmail.com
}
unit KM_AIParameters;
{$I KaM_Remake.inc}

interface
uses
  SysUtils, KM_CommonClasses;

{$IFDEF PARALLEL_RUNNER}
  procedure LoadGAParameters(LoadStream: TKMemoryStream);
  procedure SaveGAParameters(SaveStream: TKMemoryStream);
{$ENDIF}


// AI parameters in enumerations
type

  TAIPar = (
     ARMY_MaxGgroupsInCompany
    ,ARMY_PATHFINDING_AvoidEdges
    ,ARMY_PATHFINDING_AvoidSpecEnemy
    ,ARMY_PATHFINDING_AvoidTraffic

    ,ATTACK_COMPANY_AttackRadius
    ,ATTACK_COMPANY_AttackRangedGain
    ,ATTACK_COMPANY_DecreaseThreat_Prio1
    ,ATTACK_COMPANY_DecreaseThreat_Prio2
    ,ATTACK_COMPANY_DecreaseThreat_Prio3
    ,ATTACK_COMPANY_DecreaseThreat_Prio4
    ,ATTACK_COMPANY_GroupTypePenalization
    ,ATTACK_COMPANY_MinCombatSpacing
    ,ATTACK_COMPANY_MinWalkSpacing
    ,ATTACK_COMPANY_MinimumMovement
    ,ATTACK_COMPANY_Positioning_InitPolyCnt
    ,ATTACK_COMPANY_ProtectRangedAllInDist
    ,ATTACK_COMPANY_ProtectRangedGain
    ,ATTACK_COMPANY_ProtectRangedRadius
    ,ATTACK_COMPANY_TimePerATile_Fast
    ,ATTACK_COMPANY_TimePerATile_Slow

    ,ATTACK_NMAP_BackwardFlood_MaxAllyInfluence
    ,ATTACK_NMAP_BackwardFlood_MaxEnemyInfluence
    ,ATTACK_NMAP_EvaluateLine_MinDist
    ,ATTACK_NMAP_EvaluateLine_QueueCnt
    ,ATTACK_NMAP_PrefillDistances_Groups
    ,ATTACK_NMAP_PrefillDistances_Houses
    ,ATTACK_NMAP_TArmyBackwardFF_EnemyInfluence

    ,ATTACK_SQUAD_ChangeTarget_Delay
    ,ATTACK_SQUAD_ChangeTarget_DistTolerance
    ,ATTACK_SQUAD_MinWalkingDistance
    ,ATTACK_SQUAD_TargetReached_House
    ,ATTACK_SQUAD_TargetReached_Position
    ,ATTACK_SQUAD_TargetReached_RangedSquad
    ,ATTACK_SQUAD_TargetReached_Unit

    ,ATTACK_SUPERVISOR_EvalTarget_DistanceGroup
    ,ATTACK_SUPERVISOR_EvalTarget_OportunityDistGain
    ,ATTACK_SUPERVISOR_EvalTarget_OportunityGain
    ,ATTACK_SUPERVISOR_EvalTarget_ThreatGainAntiHorse
    ,ATTACK_SUPERVISOR_EvalTarget_ThreatGainDist
    ,ATTACK_SUPERVISOR_EvalTarget_ThreatGainMelee
    ,ATTACK_SUPERVISOR_EvalTarget_ThreatGainMounted
    ,ATTACK_SUPERVISOR_EvalTarget_ThreatGainRangDist
    ,ATTACK_SUPERVISOR_EvalTarget_ThreatGainRanged
    ,ATTACK_SUPERVISOR_UpdateAttacks_AttackThreshold

    ,BUILDER_BuildHouse_FieldMaxWork
    ,BUILDER_BuildHouse_RTPMaxWork
    ,BUILDER_BuildHouse_RoadMaxWork
    ,BUILDER_ChHTB_AllWorkerCoef
    ,BUILDER_ChHTB_FractionCoef
    ,BUILDER_ChHTB_FreeWorkerCoef
    ,BUILDER_ChHTB_TrunkBalance
    ,BUILDER_ChHTB_TrunkFactor
    ,BUILDER_CreateShortcuts_MaxWork
    ,BUILDER_Shortage_Gold
    ,BUILDER_Shortage_Stone
    ,BUILDER_Shortage_StoneReserve
    ,BUILDER_Shortage_Trunk
    ,BUILDER_Shortage_Wood

    ,EYE_GetForests_MaxAB
    ,EYE_GetForests_MinRndSoil
    ,EYE_GetForests_MinTrees
    ,EYE_GetForests_Radius
    ,EYE_GetForests_SPRndOwnLimMax
    ,EYE_GetForests_SPRndOwnLimMin

    ,MANAGEMENT_CheckUnitCount_SerfGoldCoef
    ,MANAGEMENT_CheckUnitCount_SerfLimit1
    ,MANAGEMENT_CheckUnitCount_SerfLimit2
    ,MANAGEMENT_CheckUnitCount_SerfLimit3
    ,MANAGEMENT_CheckUnitCount_WorkerGoldCoef
    ,MANAGEMENT_GoldShortage

    ,PLANNER_FARM_FieldCrit_FlatArea
    ,PLANNER_FARM_FieldCrit_PolyRoute
    ,PLANNER_FARM_FieldCrit_Soil
    ,PLANNER_FARM_FindPlaceForHouse_CityCenter
    ,PLANNER_FARM_FindPlaceForHouse_FlatArea
    ,PLANNER_FARM_FindPlaceForHouse_HouseDist
    ,PLANNER_FARM_FindPlaceForHouse_Route
    ,PLANNER_FARM_PlanFields_CanBuild
    ,PLANNER_FARM_PlanFields_Dist
    ,PLANNER_FARM_PlanFields_ExistField

    ,PLANNER_FindPlaceForHouse_CityCenter
    ,PLANNER_FindPlaceForHouse_FlatArea
    ,PLANNER_FindPlaceForHouse_HouseDist
    ,PLANNER_FindPlaceForHouse_Route
    ,PLANNER_FindPlaceForHouse_SeedDist
    ,PLANNER_FindPlaceForHouse_SnapCrit
    ,PLANNER_FindPlaceForQuary_DistCity
    ,PLANNER_FindPlaceForQuary_DistStone
    ,PLANNER_FindPlaceForQuary_DistTimer
    ,PLANNER_FindPlaceForQuary_Obstacle
    ,PLANNER_FindPlaceForQuary_SnapCrit

    ,PLANNER_FOREST_FindForestAround_MaxDist
    ,PLANNER_FOREST_FindPlaceForWoodcutter_ABRange
    ,PLANNER_FOREST_FindPlaceForWoodcutter_DistCrit
    ,PLANNER_FOREST_FindPlaceForWoodcutter_DistTimer
    ,PLANNER_FOREST_FindPlaceForWoodcutter_ExistForest
    ,PLANNER_FOREST_FindPlaceForWoodcutter_FlatArea
    ,PLANNER_FOREST_FindPlaceForWoodcutter_FreeTiles
    ,PLANNER_FOREST_FindPlaceForWoodcutter_Radius
    ,PLANNER_FOREST_FindPlaceForWoodcutter_Routes
    ,PLANNER_FOREST_FindPlaceForWoodcutter_Soil
    ,PLANNER_FOREST_FindPlaceForWoodcutter_TreeCnt
    ,PLANNER_FOREST_FindPlaceForWoodcutter_TreeCntTimer
    ,PLANNER_FOREST_PlaceWoodcutter_DistFromForest

    ,PLANNER_ObstaclesInHousePlan_Road
    ,PLANNER_ObstaclesInHousePlan_Tree
    ,PLANNER_SnapCrit_Field
    ,PLANNER_SnapCrit_HouseOrRoad
    ,PLANNER_SnapCrit_NoBuild
    ,PLANNER_SnapCrit_ObstacleInEntrance
    ,PLANNER_SnapCrit_Road
    ,PLANNER_SnapCrit_RoadInEntrance

    ,PREDICTOR_SecondSchool_MinRequiredUnits
    ,PREDICTOR_WareNeedPerAWorker_Stone
    ,PREDICTOR_WareNeedPerAWorker_StoneOffset
    ,PREDICTOR_WareNeedPerAWorker_Wood

    ,SHORTCUTS_BasePrice
    ,SHORTCUTS_Coal
    ,SHORTCUTS_Field
    ,SHORTCUTS_Forest
    ,SHORTCUTS_OtherCase
    ,SHORTCUTS_Road
    ,SHORTCUTS_TurnPenalization
    ,SHORTCUTS_noBuildArea
    ,ROADS_BasePrice
    ,ROADS_Coal
    ,ROADS_Field
    ,ROADS_Forest
    ,ROADS_OtherCase
    ,ROADS_Road
    ,ROADS_TurnPenalization
    ,ROADS_noBuildArea
  );


// Global constants for AI
//const

// Global variables for AI
{$IFDEF DEBUG_NewAI}
var
{$ELSE}
const
{$ENDIF}

  AI_Par: array[TAIPar] of Single = (
        7.0000000000, // ARMY_MaxGgroupsInCompany
       85.0505981445, // ARMY_PATHFINDING_AvoidEdges
        1.0583767891, // ARMY_PATHFINDING_AvoidSpecEnemy
        2.7351799011, // ARMY_PATHFINDING_AvoidTraffic
       14.0000000000, // ATTACK_COMPANY_AttackRadius
        3.8477339745, // ATTACK_COMPANY_AttackRangedGain
        0.6768462062, // ATTACK_COMPANY_DecreaseThreat_Prio1
        0.6052376032, // ATTACK_COMPANY_DecreaseThreat_Prio2
        0.9684157968, // ATTACK_COMPANY_DecreaseThreat_Prio3
        0.4872157574, // ATTACK_COMPANY_DecreaseThreat_Prio4
      157.0000000000, // ATTACK_COMPANY_GroupTypePenalization
        3.0000000000, // ATTACK_COMPANY_MinCombatSpacing
        3.0000000000, // ATTACK_COMPANY_MinWalkSpacing
        5.0000000000, // ATTACK_COMPANY_MinimumMovement
        1.0000000000, // ATTACK_COMPANY_Positioning_InitPolyCnt
        4.0000000000, // ATTACK_COMPANY_ProtectRangedAllInDist
        1.5641868114, // ATTACK_COMPANY_ProtectRangedGain
        9.0000000000, // ATTACK_COMPANY_ProtectRangedRadius
        3.0000000000, // ATTACK_COMPANY_TimePerATile_Fast
        3.0000000000, // ATTACK_COMPANY_TimePerATile_Slow
       15.0000000000, // ATTACK_NMAP_BackwardFlood_MaxAllyInfluence
       30.0000000000, // ATTACK_NMAP_BackwardFlood_MaxEnemyInfluence
        0.4369973540, // ATTACK_NMAP_EvaluateLine_MinDist
        0.1092337593, // ATTACK_NMAP_EvaluateLine_QueueCnt
       29.0000000000, // ATTACK_NMAP_PrefillDistances_Groups
        5.0000000000, // ATTACK_NMAP_PrefillDistances_Houses
        4.0000000000, // ATTACK_NMAP_TArmyBackwardFF_EnemyInfluence
      697.0000000000, // ATTACK_SQUAD_ChangeTarget_Delay
        6.1796259880, // ATTACK_SQUAD_ChangeTarget_DistTolerance
        4.0000000000, // ATTACK_SQUAD_MinWalkingDistance
        8.0000000000, // ATTACK_SQUAD_TargetReached_House
        2.0000000000, // ATTACK_SQUAD_TargetReached_Position
       14.0000000000, // ATTACK_SQUAD_TargetReached_RangedSquad
       12.0000000000, // ATTACK_SQUAD_TargetReached_Unit
        1.2410084009, // ATTACK_SUPERVISOR_EvalTarget_DistanceGroup
        4.5673518181, // ATTACK_SUPERVISOR_EvalTarget_OportunityDistGain
        2.6104888916, // ATTACK_SUPERVISOR_EvalTarget_OportunityGain
        0.8289436817, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainAntiHorse
        1.1296768188, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainDist
        0.8641214371, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMelee
        2.6811749935, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMounted
        3.9079263210, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRangDist
        2.0872707367, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRanged
        0.7455104709, // ATTACK_SUPERVISOR_UpdateAttacks_AttackThreshold
        1.0000000000, // BUILDER_BuildHouse_FieldMaxWork
        5.0000000000, // BUILDER_BuildHouse_RTPMaxWork
       16.0000000000, // BUILDER_BuildHouse_RoadMaxWork
        7.1540000000, // BUILDER_ChHTB_AllWorkerCoef
        9.3520000000, // BUILDER_ChHTB_FractionCoef
        4.2510000000, // BUILDER_ChHTB_FreeWorkerCoef
        2.5110000000, // BUILDER_ChHTB_TrunkBalance
       16.0450000000, // BUILDER_ChHTB_TrunkFactor
       10.0000000000, // BUILDER_CreateShortcuts_MaxWork
       27.6380000000, // BUILDER_Shortage_Gold
       12.0000000000, // BUILDER_Shortage_Stone
       19.0000000000, // BUILDER_Shortage_StoneReserve
        3.3600000000, // BUILDER_Shortage_Trunk
       12.8900000000, // BUILDER_Shortage_Wood
      155.7503232955, // EYE_GetForests_MaxAB
       81.3960151672, // EYE_GetForests_MinRndSoil
        3.1505226641, // EYE_GetForests_MinTrees
        8.6702444553, // EYE_GetForests_Radius
      201.6418445110, // EYE_GetForests_SPRndOwnLimMax
       99.7703361511, // EYE_GetForests_SPRndOwnLimMin
        1.5343497992, // MANAGEMENT_CheckUnitCount_SerfGoldCoef
        9.0000000000, // MANAGEMENT_CheckUnitCount_SerfLimit1
       35.0000000000, // MANAGEMENT_CheckUnitCount_SerfLimit2
       50.0000000000, // MANAGEMENT_CheckUnitCount_SerfLimit3
        2.9055855274, // MANAGEMENT_CheckUnitCount_WorkerGoldCoef
        8.0000000000, // MANAGEMENT_GoldShortage
        7.9779438972, // PLANNER_FARM_FieldCrit_FlatArea
        2.9083948135, // PLANNER_FARM_FieldCrit_PolyRoute
        0.7377260327, // PLANNER_FARM_FieldCrit_Soil
       30.7348155975, // PLANNER_FARM_FindPlaceForHouse_CityCenter
       -2.1046175957, // PLANNER_FARM_FindPlaceForHouse_FlatArea
        3.9945483208, // PLANNER_FARM_FindPlaceForHouse_HouseDist
       -1.3569523096, // PLANNER_FARM_FindPlaceForHouse_Route
       33.0000000000, // PLANNER_FARM_PlanFields_CanBuild
        3.0000000000, // PLANNER_FARM_PlanFields_Dist
       33.0000000000, // PLANNER_FARM_PlanFields_ExistField
       37.5235137939, // PLANNER_FindPlaceForHouse_CityCenter
        0.7113031149, // PLANNER_FindPlaceForHouse_FlatArea
       10.2654743195, // PLANNER_FindPlaceForHouse_HouseDist
        2.2165458202, // PLANNER_FindPlaceForHouse_Route
       45.7494544983, // PLANNER_FindPlaceForHouse_SeedDist
        2.5595338345, // PLANNER_FindPlaceForHouse_SnapCrit
       24.3620433807, // PLANNER_FindPlaceForQuary_DistCity
       14.4260616302, // PLANNER_FindPlaceForQuary_DistStone
    10644.8984375000, // PLANNER_FindPlaceForQuary_DistTimer
       14.2196416855, // PLANNER_FindPlaceForQuary_Obstacle
       24.6157531738, // PLANNER_FindPlaceForQuary_SnapCrit
        7.3631623387, // PLANNER_FOREST_FindForestAround_MaxDist
      138.0085825920, // PLANNER_FOREST_FindPlaceForWoodcutter_ABRange
        0.9424397349, // PLANNER_FOREST_FindPlaceForWoodcutter_DistCrit
    11655.6522846200, // PLANNER_FOREST_FindPlaceForWoodcutter_DistTimer
      406.3492530584, // PLANNER_FOREST_FindPlaceForWoodcutter_ExistForest
        3.3771808147, // PLANNER_FOREST_FindPlaceForWoodcutter_FlatArea
        3.3483198881, // PLANNER_FOREST_FindPlaceForWoodcutter_FreeTiles
        4.7234718799, // PLANNER_FOREST_FindPlaceForWoodcutter_Radius
        0.8166046142, // PLANNER_FOREST_FindPlaceForWoodcutter_Routes
        2.9186718463, // PLANNER_FOREST_FindPlaceForWoodcutter_Soil
       32.1456005573, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCnt
    13805.7332038900, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCntTimer
        5.6932945251, // PLANNER_FOREST_PlaceWoodcutter_DistFromForest
      678.5110473633, // PLANNER_ObstaclesInHousePlan_Road
      717.8121337891, // PLANNER_ObstaclesInHousePlan_Tree
       23.0000000000, // PLANNER_SnapCrit_Field
       18.0000000000, // PLANNER_SnapCrit_HouseOrRoad
       27.0000000000, // PLANNER_SnapCrit_NoBuild
      275.7873535156, // PLANNER_SnapCrit_ObstacleInEntrance
       23.0000000000, // PLANNER_SnapCrit_Road
      295.2063903809, // PLANNER_SnapCrit_RoadInEntrance
       20.0000000000, // PREDICTOR_SecondSchool_MinRequiredUnits
        0.7049801946, // PREDICTOR_WareNeedPerAWorker_Stone
        7.0000000000, // PREDICTOR_WareNeedPerAWorker_StoneOffset
        0.2908146679, // PREDICTOR_WareNeedPerAWorker_Wood
       53.0000000000, // SHORTCUTS_BasePrice
       41.0000000000, // SHORTCUTS_Coal
       18.0000000000, // SHORTCUTS_Field
       35.0000000000, // SHORTCUTS_Forest
        9.0000000000, // SHORTCUTS_OtherCase
       28.0000000000, // SHORTCUTS_Road
       64.0000000000, // SHORTCUTS_TurnPenalization
       26.0000000000, // SHORTCUTS_noBuildArea
       55.0000000000, // ROADS_BasePrice
       36.0000000000, // ROADS_Coal
        9.0000000000, // ROADS_Field
       68.0000000000, // ROADS_Forest
       24.0000000000, // ROADS_OtherCase
       43.0000000000, // ROADS_Road
       47.0000000000, // ROADS_TurnPenalization
        0.0000000000  // ROADS_noBuildArea
  );


{$IFDEF DEBUG_NewAI}
const
  AI_Par_Offset: array[TAIPar] of Single = (
        5.00, // ARMY_MaxGgroupsInCompany
       80.00, // ARMY_PATHFINDING_AvoidEdges
        0.00, // ARMY_PATHFINDING_AvoidSpecEnemy
        0.00, // ARMY_PATHFINDING_AvoidTraffic
        9.00, // ATTACK_COMPANY_AttackRadius
        0.00, // ATTACK_COMPANY_AttackRangedGain
        0.00, // ATTACK_COMPANY_DecreaseThreat_Prio1
        0.00, // ATTACK_COMPANY_DecreaseThreat_Prio2
        0.00, // ATTACK_COMPANY_DecreaseThreat_Prio3
        0.00, // ATTACK_COMPANY_DecreaseThreat_Prio4
      100.00, // ATTACK_COMPANY_GroupTypePenalization
        2.00, // ATTACK_COMPANY_MinCombatSpacing
        2.00, // ATTACK_COMPANY_MinWalkSpacing
        3.00, // ATTACK_COMPANY_MinimumMovement
        1.00, // ATTACK_COMPANY_Positioning_InitPolyCnt
        2.00, // ATTACK_COMPANY_ProtectRangedAllInDist
        2.00, // ATTACK_COMPANY_ProtectRangedGain
        6.00, // ATTACK_COMPANY_ProtectRangedRadius
        2.00, // ATTACK_COMPANY_TimePerATile_Fast
        2.00, // ATTACK_COMPANY_TimePerATile_Slow
        0.00, // ATTACK_NMAP_BackwardFlood_MaxAllyInfluence
        0.00, // ATTACK_NMAP_BackwardFlood_MaxEnemyInfluence
        0.00, // ATTACK_NMAP_EvaluateLine_MinDist
        0.00, // ATTACK_NMAP_EvaluateLine_QueueCnt
        0.00, // ATTACK_NMAP_PrefillDistances_Groups
        0.00, // ATTACK_NMAP_PrefillDistances_Houses
        0.00, // ATTACK_NMAP_TArmyBackwardFF_EnemyInfluence
      400.00, // ATTACK_SQUAD_ChangeTarget_Delay
        4.00, // ATTACK_SQUAD_ChangeTarget_DistTolerance
       10.00, // ATTACK_SQUAD_MinWalkingDistance
        0.00, // ATTACK_SQUAD_TargetReached_House
        1.00, // ATTACK_SQUAD_TargetReached_Position
       12.00, // ATTACK_SQUAD_TargetReached_RangedSquad
        8.00, // ATTACK_SQUAD_TargetReached_Unit
        0.00, // ATTACK_SUPERVISOR_EvalTarget_DistanceGroup
        0.00, // ATTACK_SUPERVISOR_EvalTarget_OportunityDistGain
        0.00, // ATTACK_SUPERVISOR_EvalTarget_OportunityGain
        0.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainAntiHorse
        0.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainDist
        0.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMelee
        0.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMounted
        0.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRangDist
        0.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRanged
        0.00, // ATTACK_SUPERVISOR_UpdateAttacks_AttackThreshold
        1.00, // BUILDER_BuildHouse_FieldMaxWork
        1.00, // BUILDER_BuildHouse_RTPMaxWork
        5.00, // BUILDER_BuildHouse_RoadMaxWork
        8.00, // BUILDER_ChHTB_AllWorkerCoef
        5.00, // BUILDER_ChHTB_FractionCoef
        8.00, // BUILDER_ChHTB_FreeWorkerCoef
        0.00, // BUILDER_ChHTB_TrunkBalance
        8.00, // BUILDER_ChHTB_TrunkFactor
        1.00, // BUILDER_CreateShortcuts_MaxWork
       20.00, // BUILDER_Shortage_Gold
        5.00, // BUILDER_Shortage_Stone
       10.00, // BUILDER_Shortage_StoneReserve
        1.00, // BUILDER_Shortage_Trunk
       13.00, // BUILDER_Shortage_Wood
        1.00, // EYE_GetForests_MaxAB
       60.00, // EYE_GetForests_MinRndSoil
        1.00, // EYE_GetForests_MinTrees
        3.00, // EYE_GetForests_Radius
      155.00, // EYE_GetForests_SPRndOwnLimMax
       55.00, // EYE_GetForests_SPRndOwnLimMin
        1.00, // MANAGEMENT_CheckUnitCount_SerfGoldCoef
        0.00, // MANAGEMENT_CheckUnitCount_SerfLimit1
       20.00, // MANAGEMENT_CheckUnitCount_SerfLimit2
       40.00, // MANAGEMENT_CheckUnitCount_SerfLimit3
        2.00, // MANAGEMENT_CheckUnitCount_WorkerGoldCoef
       10.00, // MANAGEMENT_GoldShortage
        0.00, // PLANNER_FARM_FieldCrit_FlatArea
        0.00, // PLANNER_FARM_FieldCrit_PolyRoute
        0.00, // PLANNER_FARM_FieldCrit_Soil
        0.00, // PLANNER_FARM_FindPlaceForHouse_CityCenter
        0.00, // PLANNER_FARM_FindPlaceForHouse_FlatArea
        5.00, // PLANNER_FARM_FindPlaceForHouse_HouseDist
       -2.00, // PLANNER_FARM_FindPlaceForHouse_Route
        0.00, // PLANNER_FARM_PlanFields_CanBuild
        0.00, // PLANNER_FARM_PlanFields_Dist
       25.00, // PLANNER_FARM_PlanFields_ExistField
        0.00, // PLANNER_FindPlaceForHouse_CityCenter
        0.00, // PLANNER_FindPlaceForHouse_FlatArea
       15.00, // PLANNER_FindPlaceForHouse_HouseDist
        0.00, // PLANNER_FindPlaceForHouse_Route
        0.00, // PLANNER_FindPlaceForHouse_SeedDist
        0.00, // PLANNER_FindPlaceForHouse_SnapCrit
        0.00, // PLANNER_FindPlaceForQuary_DistCity
        0.00, // PLANNER_FindPlaceForQuary_DistStone
        0.00, // PLANNER_FindPlaceForQuary_DistTimer
       40.00, // PLANNER_FindPlaceForQuary_Obstacle
       10.00, // PLANNER_FindPlaceForQuary_SnapCrit
        5.00, // PLANNER_FOREST_FindForestAround_MaxDist
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_ABRange
        8.00, // PLANNER_FOREST_FindPlaceForWoodcutter_DistCrit
     6000.00, // PLANNER_FOREST_FindPlaceForWoodcutter_DistTimer
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_ExistForest
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_FlatArea
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_FreeTiles
        3.00, // PLANNER_FOREST_FindPlaceForWoodcutter_Radius
       -1.00, // PLANNER_FOREST_FindPlaceForWoodcutter_Routes
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_Soil
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCnt
    12000.00, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCntTimer
        0.00, // PLANNER_FOREST_PlaceWoodcutter_DistFromForest
      200.00, // PLANNER_ObstaclesInHousePlan_Road
      500.00, // PLANNER_ObstaclesInHousePlan_Tree
        0.00, // PLANNER_SnapCrit_Field
        0.00, // PLANNER_SnapCrit_HouseOrRoad
        0.00, // PLANNER_SnapCrit_NoBuild
        0.00, // PLANNER_SnapCrit_ObstacleInEntrance
        0.00, // PLANNER_SnapCrit_Road
        0.00, // PLANNER_SnapCrit_RoadInEntrance
        5.00, // PREDICTOR_SecondSchool_MinRequiredUnits
        0.50, // PREDICTOR_WareNeedPerAWorker_Stone
        1.00, // PREDICTOR_WareNeedPerAWorker_StoneOffset
        0.25, // PREDICTOR_WareNeedPerAWorker_Wood
       50.00, // SHORTCUTS_BasePrice
       20.00, // SHORTCUTS_Coal
        0.00, // SHORTCUTS_Field
       10.00, // SHORTCUTS_Forest
        0.00, // SHORTCUTS_OtherCase
        0.00, // SHORTCUTS_Road
       35.00, // SHORTCUTS_TurnPenalization
       20.00, // SHORTCUTS_noBuildArea
       25.00, // ROADS_BasePrice
        0.00, // ROADS_Coal
       20.00, // ROADS_Field
       30.00, // ROADS_Forest
        0.00, // ROADS_OtherCase
        0.00, // ROADS_Road
        0.00, // ROADS_TurnPenalization
       15.00  // ROADS_noBuildArea
  );


  AI_Par_Gain: array[TAIPar] of Single = (
       15.00, // ARMY_MaxGgroupsInCompany
       80.00, // ARMY_PATHFINDING_AvoidEdges
       10.00, // ARMY_PATHFINDING_AvoidSpecEnemy
        3.00, // ARMY_PATHFINDING_AvoidTraffic
       11.00, // ATTACK_COMPANY_AttackRadius
       10.00, // ATTACK_COMPANY_AttackRangedGain
        1.00, // ATTACK_COMPANY_DecreaseThreat_Prio1
        1.00, // ATTACK_COMPANY_DecreaseThreat_Prio2
        1.00, // ATTACK_COMPANY_DecreaseThreat_Prio3
        1.00, // ATTACK_COMPANY_DecreaseThreat_Prio4
      100.00, // ATTACK_COMPANY_GroupTypePenalization
        3.00, // ATTACK_COMPANY_MinCombatSpacing
        3.00, // ATTACK_COMPANY_MinWalkSpacing
       15.00, // ATTACK_COMPANY_MinimumMovement
        3.00, // ATTACK_COMPANY_Positioning_InitPolyCnt
        5.00, // ATTACK_COMPANY_ProtectRangedAllInDist
        5.00, // ATTACK_COMPANY_ProtectRangedGain
        8.00, // ATTACK_COMPANY_ProtectRangedRadius
        3.00, // ATTACK_COMPANY_TimePerATile_Fast
        3.00, // ATTACK_COMPANY_TimePerATile_Slow
       50.00, // ATTACK_NMAP_BackwardFlood_MaxAllyInfluence
       50.00, // ATTACK_NMAP_BackwardFlood_MaxEnemyInfluence
        1.00, // ATTACK_NMAP_EvaluateLine_MinDist
        2.00, // ATTACK_NMAP_EvaluateLine_QueueCnt
       50.00, // ATTACK_NMAP_PrefillDistances_Groups
       50.00, // ATTACK_NMAP_PrefillDistances_Houses
       10.00, // ATTACK_NMAP_TArmyBackwardFF_EnemyInfluence
      500.00, // ATTACK_SQUAD_ChangeTarget_Delay
        6.00, // ATTACK_SQUAD_ChangeTarget_DistTolerance
       20.00, // ATTACK_SQUAD_MinWalkingDistance
        8.00, // ATTACK_SQUAD_TargetReached_House
        4.00, // ATTACK_SQUAD_TargetReached_Position
        5.00, // ATTACK_SQUAD_TargetReached_RangedSquad
        8.00, // ATTACK_SQUAD_TargetReached_Unit
       10.00, // ATTACK_SUPERVISOR_EvalTarget_DistanceGroup
       10.00, // ATTACK_SUPERVISOR_EvalTarget_OportunityDistGain
       10.00, // ATTACK_SUPERVISOR_EvalTarget_OportunityGain
        5.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainAntiHorse
      500.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainDist
        5.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMelee
        5.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMounted
        5.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRangDist
        5.00, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRanged
        1.00, // ATTACK_SUPERVISOR_UpdateAttacks_AttackThreshold
        1.00, // BUILDER_BuildHouse_FieldMaxWork
       15.00, // BUILDER_BuildHouse_RTPMaxWork
       20.00, // BUILDER_BuildHouse_RoadMaxWork
       10.00, // BUILDER_ChHTB_AllWorkerCoef
       25.00, // BUILDER_ChHTB_FractionCoef
       10.00, // BUILDER_ChHTB_FreeWorkerCoef
        5.00, // BUILDER_ChHTB_TrunkBalance
       12.00, // BUILDER_ChHTB_TrunkFactor
        9.00, // BUILDER_CreateShortcuts_MaxWork
       20.00, // BUILDER_Shortage_Gold
       20.00, // BUILDER_Shortage_Stone
       40.00, // BUILDER_Shortage_StoneReserve
        3.00, // BUILDER_Shortage_Trunk
       10.00, // BUILDER_Shortage_Wood
      200.00, // EYE_GetForests_MaxAB
       22.00, // EYE_GetForests_MinRndSoil
        4.00, // EYE_GetForests_MinTrees
        4.00, // EYE_GetForests_Radius
      100.00, // EYE_GetForests_SPRndOwnLimMax
      100.00, // EYE_GetForests_SPRndOwnLimMin
        2.00, // MANAGEMENT_CheckUnitCount_SerfGoldCoef
       20.00, // MANAGEMENT_CheckUnitCount_SerfLimit1
       20.00, // MANAGEMENT_CheckUnitCount_SerfLimit2
       30.00, // MANAGEMENT_CheckUnitCount_SerfLimit3
        3.00, // MANAGEMENT_CheckUnitCount_WorkerGoldCoef
       15.00, // MANAGEMENT_GoldShortage
       12.00, // PLANNER_FARM_FieldCrit_FlatArea
        5.00, // PLANNER_FARM_FieldCrit_PolyRoute
        3.00, // PLANNER_FARM_FieldCrit_Soil
       10.00, // PLANNER_FARM_FindPlaceForHouse_CityCenter
       12.00, // PLANNER_FARM_FindPlaceForHouse_FlatArea
       10.00, // PLANNER_FARM_FindPlaceForHouse_HouseDist
        4.00, // PLANNER_FARM_FindPlaceForHouse_Route
       75.00, // PLANNER_FARM_PlanFields_CanBuild
       75.00, // PLANNER_FARM_PlanFields_Dist
       75.00, // PLANNER_FARM_PlanFields_ExistField
       50.00, // PLANNER_FindPlaceForHouse_CityCenter
        6.00, // PLANNER_FindPlaceForHouse_FlatArea
       20.00, // PLANNER_FindPlaceForHouse_HouseDist
        4.00, // PLANNER_FindPlaceForHouse_Route
       50.00, // PLANNER_FindPlaceForHouse_SeedDist
        3.00, // PLANNER_FindPlaceForHouse_SnapCrit
       50.00, // PLANNER_FindPlaceForQuary_DistCity
       50.00, // PLANNER_FindPlaceForQuary_DistStone
    15000.00, // PLANNER_FindPlaceForQuary_DistTimer
       50.00, // PLANNER_FindPlaceForQuary_Obstacle
       50.00, // PLANNER_FindPlaceForQuary_SnapCrit
        5.00, // PLANNER_FOREST_FindForestAround_MaxDist
      200.00, // PLANNER_FOREST_FindPlaceForWoodcutter_ABRange
       10.00, // PLANNER_FOREST_FindPlaceForWoodcutter_DistCrit
     6000.00, // PLANNER_FOREST_FindPlaceForWoodcutter_DistTimer
      500.00, // PLANNER_FOREST_FindPlaceForWoodcutter_ExistForest
        6.00, // PLANNER_FOREST_FindPlaceForWoodcutter_FlatArea
        6.00, // PLANNER_FOREST_FindPlaceForWoodcutter_FreeTiles
        4.00, // PLANNER_FOREST_FindPlaceForWoodcutter_Radius
        2.00, // PLANNER_FOREST_FindPlaceForWoodcutter_Routes
        3.00, // PLANNER_FOREST_FindPlaceForWoodcutter_Soil
       21.00, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCnt
    12000.00, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCntTimer
        2.00, // PLANNER_FOREST_PlaceWoodcutter_DistFromForest
      500.00, // PLANNER_ObstaclesInHousePlan_Road
     1000.00, // PLANNER_ObstaclesInHousePlan_Tree
       50.00, // PLANNER_SnapCrit_Field
       50.00, // PLANNER_SnapCrit_HouseOrRoad
       50.00, // PLANNER_SnapCrit_NoBuild
     1000.00, // PLANNER_SnapCrit_ObstacleInEntrance
       50.00, // PLANNER_SnapCrit_Road
      300.00, // PLANNER_SnapCrit_RoadInEntrance
       30.00, // PREDICTOR_SecondSchool_MinRequiredUnits
        0.50, // PREDICTOR_WareNeedPerAWorker_Stone
       15.00, // PREDICTOR_WareNeedPerAWorker_StoneOffset
        0.50, // PREDICTOR_WareNeedPerAWorker_Wood
       40.00, // SHORTCUTS_BasePrice
       50.00, // SHORTCUTS_Coal
       50.00, // SHORTCUTS_Field
       40.00, // SHORTCUTS_Forest
       50.00, // SHORTCUTS_OtherCase
       50.00, // SHORTCUTS_Road
       50.00, // SHORTCUTS_TurnPenalization
       50.00, // SHORTCUTS_noBuildArea
       30.00, // ROADS_BasePrice
       50.00, // ROADS_Coal
       40.00, // ROADS_Field
       50.00, // ROADS_Forest
       50.00, // ROADS_OtherCase
       50.00, // ROADS_Road
       50.00, // ROADS_TurnPenalization
       35.00  // ROADS_noBuildArea
  );
{$ENDIF}

implementation

{$IFDEF PARALLEL_RUNNER}
procedure LoadGAParameters(LoadStream: TKMemoryStream);
begin
  LoadStream.CheckMarker('LoadGAParameters');
  LoadStream.Read(AI_Par,SizeOf(AI_Par));
end;

procedure SaveGAParameters(SaveStream: TKMemoryStream);
begin
  SaveStream.PlaceMarker('LoadGAParameters');
  SaveStream.Write(AI_Par,SizeOf(AI_Par));
end;
{$ENDIF}

end.
