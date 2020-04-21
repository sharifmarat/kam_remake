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
          7.0000000, // ARMY_MaxGgroupsInCompany
         10.0505981, // ARMY_PATHFINDING_AvoidEdges
          1.0583768, // ARMY_PATHFINDING_AvoidSpecEnemy
          2.7351799, // ARMY_PATHFINDING_AvoidTraffic
         14.0000000, // ATTACK_COMPANY_AttackRadius
          3.8477340, // ATTACK_COMPANY_AttackRangedGain
          0.6768462, // ATTACK_COMPANY_DecreaseThreat_Prio1
          0.6052376, // ATTACK_COMPANY_DecreaseThreat_Prio2
          0.9684158, // ATTACK_COMPANY_DecreaseThreat_Prio3
          0.4872158, // ATTACK_COMPANY_DecreaseThreat_Prio4
        157.0000000, // ATTACK_COMPANY_GroupTypePenalization
          3.0000000, // ATTACK_COMPANY_MinCombatSpacing
          3.0000000, // ATTACK_COMPANY_MinWalkSpacing
          5.0000000, // ATTACK_COMPANY_MinimumMovement
          1.0000000, // ATTACK_COMPANY_Positioning_InitPolyCnt
          4.0000000, // ATTACK_COMPANY_ProtectRangedAllInDist
          1.5641868, // ATTACK_COMPANY_ProtectRangedGain
          9.0000000, // ATTACK_COMPANY_ProtectRangedRadius
          3.0000000, // ATTACK_COMPANY_TimePerATile_Fast
          3.0000000, // ATTACK_COMPANY_TimePerATile_Slow
         15.0000000, // ATTACK_NMAP_BackwardFlood_MaxAllyInfluence
         30.0000000, // ATTACK_NMAP_BackwardFlood_MaxEnemyInfluence
          0.4369974, // ATTACK_NMAP_EvaluateLine_MinDist
          0.1092338, // ATTACK_NMAP_EvaluateLine_QueueCnt
         29.0000000, // ATTACK_NMAP_PrefillDistances_Groups
          5.0000000, // ATTACK_NMAP_PrefillDistances_Houses
          4.0000000, // ATTACK_NMAP_TArmyBackwardFF_EnemyInfluence
        697.0000000, // ATTACK_SQUAD_ChangeTarget_Delay
          6.1796260, // ATTACK_SQUAD_ChangeTarget_DistTolerance
          4.0000000, // ATTACK_SQUAD_MinWalkingDistance
          8.0000000, // ATTACK_SQUAD_TargetReached_House
          2.0000000, // ATTACK_SQUAD_TargetReached_Position
         14.0000000, // ATTACK_SQUAD_TargetReached_RangedSquad
         12.0000000, // ATTACK_SQUAD_TargetReached_Unit
         20.0000000, // ATTACK_SUPERVISOR_EvalTarget_DistanceGroup
          0.1000000, // ATTACK_SUPERVISOR_EvalTarget_OportunityDistGain
          1.0000000, // ATTACK_SUPERVISOR_EvalTarget_OportunityGain
          1.0000000, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainAntiHorse
          1.0000000, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainDist
          1.0000000, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMelee
          1.0000000, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainMounted
          1.0000000, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRangDist
          1.0000000, // ATTACK_SUPERVISOR_EvalTarget_ThreatGainRanged
          0.7455105, // ATTACK_SUPERVISOR_UpdateAttacks_AttackThreshold
          1.7547250, // BUILDER_BuildHouse_FieldMaxWork
          1.0996417, // BUILDER_BuildHouse_RTPMaxWork
          9.1976185, // BUILDER_BuildHouse_RoadMaxWork
          8.8504696, // BUILDER_ChHTB_AllWorkerCoef
          9.9743118, // BUILDER_ChHTB_FractionCoef
         11.4246054, // BUILDER_ChHTB_FreeWorkerCoef
          1.0058377, // BUILDER_ChHTB_TrunkBalance
         16.8083534, // BUILDER_ChHTB_TrunkFactor
          3.9546027, // BUILDER_CreateShortcuts_MaxWork
         20.6492119, // BUILDER_Shortage_Gold
         12.1181965, // BUILDER_Shortage_Stone
         27.9681778, // BUILDER_Shortage_StoneReserve
          2.0124621, // BUILDER_Shortage_Trunk
         13.0000000, // BUILDER_Shortage_Wood
         11.2736206, // EYE_GetForests_MaxAB
         68.5816422, // EYE_GetForests_MinRndSoil
          3.1848333, // EYE_GetForests_MinTrees
          6.2504420, // EYE_GetForests_Radius
        231.7467957, // EYE_GetForests_SPRndOwnLimMax
         72.2787399, // EYE_GetForests_SPRndOwnLimMin
          1.0000000, // MANAGEMENT_CheckUnitCount_SerfGoldCoef
         15.6595001, // MANAGEMENT_CheckUnitCount_SerfLimit1
         37.3727493, // MANAGEMENT_CheckUnitCount_SerfLimit2
         58.1549873, // MANAGEMENT_CheckUnitCount_SerfLimit3
          2.0000000, // MANAGEMENT_CheckUnitCount_WorkerGoldCoef
         10.0000000, // MANAGEMENT_GoldShortage
          2.1741371, // PLANNER_FARM_FieldCrit_FlatArea
          2.2117908, // PLANNER_FARM_FieldCrit_PolyRoute
          1.3475945, // PLANNER_FARM_FieldCrit_Soil
          4.4648204, // PLANNER_FARM_FindPlaceForHouse_CityCenter
          2.9779010, // PLANNER_FARM_FindPlaceForHouse_FlatArea
          7.8197188, // PLANNER_FARM_FindPlaceForHouse_HouseDist
          1.3052022, // PLANNER_FARM_FindPlaceForHouse_Route
         35.7995033, // PLANNER_FARM_PlanFields_CanBuild
         36.2002411, // PLANNER_FARM_PlanFields_Dist
         56.6039543, // PLANNER_FARM_PlanFields_ExistField
         24.2363358, // PLANNER_FindPlaceForHouse_CityCenter
          1.4848325, // PLANNER_FindPlaceForHouse_FlatArea
         30.3053074, // PLANNER_FindPlaceForHouse_HouseDist
          1.0928961, // PLANNER_FindPlaceForHouse_Route
         19.3727894, // PLANNER_FindPlaceForHouse_SeedDist
          0.8143388, // PLANNER_FindPlaceForHouse_SnapCrit
         37.6775322, // PLANNER_FindPlaceForQuary_DistCity
         19.3908730, // PLANNER_FindPlaceForQuary_DistStone
        916.3164063, // PLANNER_FindPlaceForQuary_DistTimer
         79.8209305, // PLANNER_FindPlaceForQuary_Obstacle
         41.8161469, // PLANNER_FindPlaceForQuary_SnapCrit
          6.0032129, // PLANNER_FOREST_FindForestAround_MaxDist
         13.4361115, // PLANNER_FOREST_FindPlaceForWoodcutter_ABRange
          9.6319351, // PLANNER_FOREST_FindPlaceForWoodcutter_DistCrit
       6314.7558594, // PLANNER_FOREST_FindPlaceForWoodcutter_DistTimer
        213.5915833, // PLANNER_FOREST_FindPlaceForWoodcutter_ExistForest
          2.5095639, // PLANNER_FOREST_FindPlaceForWoodcutter_FlatArea
          0.4984407, // PLANNER_FOREST_FindPlaceForWoodcutter_FreeTiles
          3.3014369, // PLANNER_FOREST_FindPlaceForWoodcutter_Radius
         -0.5921079, // PLANNER_FOREST_FindPlaceForWoodcutter_Routes
          1.8728309, // PLANNER_FOREST_FindPlaceForWoodcutter_Soil
          3.4405437, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCnt
      23018.5097656, // PLANNER_FOREST_FindPlaceForWoodcutter_TreeCntTimer
          0.9458039, // PLANNER_FOREST_PlaceWoodcutter_DistFromForest
        353.0060120, // PLANNER_ObstaclesInHousePlan_Road
       1020.0874023, // PLANNER_ObstaclesInHousePlan_Tree
          0.0000000, // PLANNER_SnapCrit_Field
          0.0000000, // PLANNER_SnapCrit_HouseOrRoad
          0.0000000, // PLANNER_SnapCrit_NoBuild
        561.3941650, // PLANNER_SnapCrit_ObstacleInEntrance
         38.7289505, // PLANNER_SnapCrit_Road
          0.0000000, // PLANNER_SnapCrit_RoadInEntrance
         35.0000000, // PREDICTOR_SecondSchool_MinRequiredUnits
          0.6166656, // PREDICTOR_WareNeedPerAWorker_Stone
          8.2666798, // PREDICTOR_WareNeedPerAWorker_StoneOffset
          0.3024155, // PREDICTOR_WareNeedPerAWorker_Wood
         86.0810776, // SHORTCUTS_BasePrice
         36.1513824, // SHORTCUTS_Coal
         35.7559090, // SHORTCUTS_Field
         36.9485817, // SHORTCUTS_Forest
         39.3399887, // SHORTCUTS_OtherCase
         31.0901375, // SHORTCUTS_Road
         59.9326286, // SHORTCUTS_TurnPenalization
         65.4560699, // SHORTCUTS_noBuildArea
         45.7135544, // ROADS_BasePrice
          8.1210327, // ROADS_Coal
         43.4000740, // ROADS_Field
         52.5317612, // ROADS_Forest
         34.7178040, // ROADS_OtherCase
         22.2137413, // ROADS_Road
         21.5668221, // ROADS_TurnPenalization
         37.7816658  // ROADS_noBuildArea
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
        0.00, // BUILDER_Shortage_Gold
       10.00, // BUILDER_Shortage_Stone
       10.00, // BUILDER_Shortage_StoneReserve
        1.00, // BUILDER_Shortage_Trunk
        3.00, // BUILDER_Shortage_Wood
        1.00, // EYE_GetForests_MaxAB
       40.00, // EYE_GetForests_MinRndSoil
        1.00, // EYE_GetForests_MinTrees
        5.00, // EYE_GetForests_Radius
      100.00, // EYE_GetForests_SPRndOwnLimMax
        0.00, // EYE_GetForests_SPRndOwnLimMin
        0.10, // MANAGEMENT_CheckUnitCount_SerfGoldCoef
        5.00, // MANAGEMENT_CheckUnitCount_SerfLimit1
       20.00, // MANAGEMENT_CheckUnitCount_SerfLimit2
       40.00, // MANAGEMENT_CheckUnitCount_SerfLimit3
        0.10, // MANAGEMENT_CheckUnitCount_WorkerGoldCoef
        1.00, // MANAGEMENT_GoldShortage
        0.00, // PLANNER_FARM_FieldCrit_FlatArea
        0.00, // PLANNER_FARM_FieldCrit_PolyRoute
        0.00, // PLANNER_FARM_FieldCrit_Soil
        0.00, // PLANNER_FARM_FindPlaceForHouse_CityCenter
        0.00, // PLANNER_FARM_FindPlaceForHouse_FlatArea
        5.00, // PLANNER_FARM_FindPlaceForHouse_HouseDist
       -2.00, // PLANNER_FARM_FindPlaceForHouse_Route
        0.00, // PLANNER_FARM_PlanFields_CanBuild
        0.00, // PLANNER_FARM_PlanFields_Dist
       30.00, // PLANNER_FARM_PlanFields_ExistField
        0.00, // PLANNER_FindPlaceForHouse_CityCenter
        0.00, // PLANNER_FindPlaceForHouse_FlatArea
       25.00, // PLANNER_FindPlaceForHouse_HouseDist
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
        0.00, // PLANNER_FOREST_FindPlaceForWoodcutter_DistTimer
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
       20.00, // PREDICTOR_SecondSchool_MinRequiredUnits
        0.50, // PREDICTOR_WareNeedPerAWorker_Stone
        5.00, // PREDICTOR_WareNeedPerAWorker_StoneOffset
        0.01, // PREDICTOR_WareNeedPerAWorker_Wood
       70.00, // SHORTCUTS_BasePrice
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
       40.00, // BUILDER_ChHTB_FractionCoef
       20.00, // BUILDER_ChHTB_FreeWorkerCoef
        3.00, // BUILDER_ChHTB_TrunkBalance
       12.00, // BUILDER_ChHTB_TrunkFactor
        9.00, // BUILDER_CreateShortcuts_MaxWork
       35.00, // BUILDER_Shortage_Gold
       15.00, // BUILDER_Shortage_Stone
       40.00, // BUILDER_Shortage_StoneReserve
        3.00, // BUILDER_Shortage_Trunk
       20.00, // BUILDER_Shortage_Wood
      200.00, // EYE_GetForests_MaxAB
       22.00, // EYE_GetForests_MinRndSoil
        4.00, // EYE_GetForests_MinTrees
        4.00, // EYE_GetForests_Radius
      155.00, // EYE_GetForests_SPRndOwnLimMax
      155.00, // EYE_GetForests_SPRndOwnLimMin
        3.00, // MANAGEMENT_CheckUnitCount_SerfGoldCoef
       20.00, // MANAGEMENT_CheckUnitCount_SerfLimit1
       20.00, // MANAGEMENT_CheckUnitCount_SerfLimit2
       30.00, // MANAGEMENT_CheckUnitCount_SerfLimit3
        3.00, // MANAGEMENT_CheckUnitCount_WorkerGoldCoef
       15.00, // MANAGEMENT_GoldShortage
       15.00, // PLANNER_FARM_FieldCrit_FlatArea
        5.00, // PLANNER_FARM_FieldCrit_PolyRoute
        3.00, // PLANNER_FARM_FieldCrit_Soil
       10.00, // PLANNER_FARM_FindPlaceForHouse_CityCenter
       15.00, // PLANNER_FARM_FindPlaceForHouse_FlatArea
       15.00, // PLANNER_FARM_FindPlaceForHouse_HouseDist
        4.00, // PLANNER_FARM_FindPlaceForHouse_Route
       75.00, // PLANNER_FARM_PlanFields_CanBuild
       75.00, // PLANNER_FARM_PlanFields_Dist
       75.00, // PLANNER_FARM_PlanFields_ExistField
       80.00, // PLANNER_FindPlaceForHouse_CityCenter
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
      150.00, // PLANNER_FOREST_FindPlaceForWoodcutter_ABRange
       20.00, // PLANNER_FOREST_FindPlaceForWoodcutter_DistCrit
    10000.00, // PLANNER_FOREST_FindPlaceForWoodcutter_DistTimer
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
        0.30, // PREDICTOR_WareNeedPerAWorker_Stone
       10.00, // PREDICTOR_WareNeedPerAWorker_StoneOffset
        0.30, // PREDICTOR_WareNeedPerAWorker_Wood
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
