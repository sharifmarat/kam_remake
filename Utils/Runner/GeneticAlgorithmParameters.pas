unit GeneticAlgorithmParameters;

interface
uses
  Classes, SysUtils, Math,
  GeneticAlgorithm,
  KM_Log, KM_AIParameters;

type
  TGAParameterization = class
  private
    fLogPar: TKMLog;
    fClass: String;
    function Incr(var Idx: Word): Word;
    // Get count of parameters
    function GetParCnt_TestParRun(): Word;
    function GetParCnt_HandLogistics(): Word;
    function GetParCnt_Manager(): Word;
    function GetParCnt_CityAllIn(): Word;
    function GetParCnt_CityBuilder(): Word;
    function GetParCnt_Farm(): Word;
    function GetParCnt_Quarry(): Word;
    function GetParCnt_RoadPlanner(): Word;
    function GetParCnt_Forest(): Word;
    function GetParCnt_CityPlanner(): Word;
    function GetParCnt_ArmyAttack(): Word;
    function GetParCnt_ArmyAttackNew(): Word;
    // Set global parameters
    procedure SetPar_HandLogistics(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
    procedure SetPar_Manager      (aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
    procedure SetPar_CityAllIn    (aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
    procedure SetPar_CityBuilder  (aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
    procedure SetPar_Farm         (aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
    procedure SetPar_Quarry       (aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
    procedure SetPar_RoadPlanner  (aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
    procedure SetPar_Forest       (aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
    procedure SetPar_CityPlanner  (aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
    procedure SetPar_ArmyAttack   (aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
    procedure SetPar_ArmyAttackNew(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
  public
    constructor Create();
    destructor Destroy(); override;

    property SetLogPar: TKMLog write fLogPar;
    property CurrentClass: String read fClass write fClass;

    function GetParCnt(aNewClass: String = ''): Word;
    procedure SetPar(aIdv: TGAIndividual; aLogIt: Boolean = False);
  end;

implementation


{ TGAParameterization }
constructor TGAParameterization.Create();
begin
  inherited;

  fLogPar := nil;
  fClass := '';
end;


destructor TGAParameterization.Destroy();
begin
  fLogPar := nil;
  fClass := '';

  inherited;
end;



// Small helping function
function TGAParameterization.Incr(var Idx: Word): Word;
begin
  Result := Idx;
  Inc(Idx);
end;


function TGAParameterization.GetParCnt(aNewClass: String = ''): Word;
begin
  if not (CompareStr(aNewClass, '') = 0) then
    fClass := aNewClass;
  if      (CompareStr(fClass, 'TKMRunnerGA_CityAllIn'    ) = 0) then Result := GetParCnt_CityAllIn
  else if (CompareStr(fClass, 'TKMRunnerGA_CityBuilder'  ) = 0) then Result := GetParCnt_CityBuilder
  else if (CompareStr(fClass, 'TKMRunnerGA_CityPlanner'  ) = 0) then Result := GetParCnt_CityPlanner
  else if (CompareStr(fClass, 'TKMRunnerGA_Farm'         ) = 0) then Result := GetParCnt_Farm
  else if (CompareStr(fClass, 'TKMRunnerGA_Forest'       ) = 0) then Result := GetParCnt_Forest
  else if (CompareStr(fClass, 'TKMRunnerGA_HandLogistics') = 0) then Result := GetParCnt_HandLogistics
  else if (CompareStr(fClass, 'TKMRunnerGA_Manager'      ) = 0) then Result := GetParCnt_Manager
  else if (CompareStr(fClass, 'TKMRunnerGA_Quarry'       ) = 0) then Result := GetParCnt_Quarry
  else if (CompareStr(fClass, 'TKMRunnerGA_RoadPlanner'  ) = 0) then Result := GetParCnt_RoadPlanner
  else if (CompareStr(fClass, 'TKMRunnerGA_TestParRun'   ) = 0) then Result := GetParCnt_TestParRun
  else if (CompareStr(fClass, 'TKMRunnerGA_ArmyAttack'   ) = 0) then Result := GetParCnt_ArmyAttack
  else if (CompareStr(fClass, 'TKMRunnerGA_ArmyAttackNew') = 0) then Result := GetParCnt_ArmyAttackNew
  else Result := 0;
end;

procedure TGAParameterization.SetPar(aIdv: TGAIndividual; aLogIt: Boolean = False);
begin
  if      (CompareStr(fClass, 'TKMRunnerGA_CityAllIn'    ) = 0) then SetPar_CityAllIn(aIdv, aLogIt)
  else if (CompareStr(fClass, 'TKMRunnerGA_CityBuilder'  ) = 0) then SetPar_CityBuilder(aIdv, aLogIt)
  else if (CompareStr(fClass, 'TKMRunnerGA_CityPlanner'  ) = 0) then SetPar_CityPlanner(aIdv, aLogIt)
  else if (CompareStr(fClass, 'TKMRunnerGA_Farm'         ) = 0) then SetPar_Farm(aIdv, aLogIt)
  else if (CompareStr(fClass, 'TKMRunnerGA_Forest'       ) = 0) then SetPar_Forest(aIdv, aLogIt)
  else if (CompareStr(fClass, 'TKMRunnerGA_HandLogistics') = 0) then SetPar_HandLogistics(aIdv, aLogIt)
  else if (CompareStr(fClass, 'TKMRunnerGA_Manager'      ) = 0) then SetPar_Manager(aIdv, aLogIt)
  else if (CompareStr(fClass, 'TKMRunnerGA_Quarry'       ) = 0) then SetPar_Quarry(aIdv, aLogIt)
  else if (CompareStr(fClass, 'TKMRunnerGA_RoadPlanner'  ) = 0) then SetPar_RoadPlanner(aIdv, aLogIt)
  else if (CompareStr(fClass, 'TKMRunnerGA_ArmyAttack'   ) = 0) then SetPar_ArmyAttack(aIdv, aLogIt)
  else if (CompareStr(fClass, 'TKMRunnerGA_ArmyAttackNew') = 0) then SetPar_ArmyAttackNew(aIdv, aLogIt)
  else begin end;
end;


function TGAParameterization.GetParCnt_TestParRun(): Word;
begin
  Result := 10;
end;




function TGAParameterization.GetParCnt_HandLogistics(): Word;
begin
  Result := 0;
end;

procedure TGAParameterization.SetPar_HandLogistics(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
//var
//  I: Integer;
begin
  //I := 0;

  //GA_TCBB_BasicInit   := Max(1, Round(aIdv.Gene[Incr(K)] * 20));
  //GA_TCBB_BasicRnd    := Max(1, Round(aIdv.Gene[Incr(K)] * 60)+40);
  //GA_TCBB_NormRnd     := Max(1, Round(aIdv.Gene[Incr(K)] * 32));
  //GA_TCBB_Rnd         := Max(1, Round(aIdv.Gene[Incr(K)] * 50));
  //GA_TCBB_BasicPwr    := Max(1, Round(GA_TCBB_BasicRnd / 5)); // GA has discovered that this is best strategy
  //GA_TCBB_NormPwr     := Max(1, Round(GA_TCBB_NormRnd / 5));
  //
  //if aLogIt then
  //begin
  //  fLogPar.AddTime('GA_TCBB_BasicInit   : Integer = ' + IntToStr( GA_TCBB_BasicInit ) + ';');
  //  fLogPar.AddTime('GA_TCBB_BasicRnd    : Integer = ' + IntToStr( GA_TCBB_BasicRnd  ) + ';');
  //  fLogPar.AddTime('GA_TCBB_BasicPwr    : Integer = ' + IntToStr( GA_TCBB_BasicPwr  ) + ';');
  //  fLogPar.AddTime('GA_TCBB_NormRnd     : Integer = ' + IntToStr( GA_TCBB_NormRnd   ) + ';');
  //  fLogPar.AddTime('GA_TCBB_NormPwr     : Integer = ' + IntToStr( GA_TCBB_NormPwr   ) + ';');
  //  fLogPar.AddTime('GA_TCBB_Rnd         : Integer = ' + IntToStr( GA_TCBB_Rnd       ) + ';');
  //end;
end;


function TGAParameterization.GetParCnt_Manager(): Word;
begin
  Result := 6+1;
end;

procedure TGAParameterization.SetPar_Manager(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
begin
  GA_MANAGEMENT_GoldShortage                         := 10 + Round( aIdv.Gene[Incr(K)] * 15);
  GA_MANAGEMENT_CheckUnitCount_SerfLimit1            :=  0 + Round( aIdv.Gene[Incr(K)] * 20);
  GA_MANAGEMENT_CheckUnitCount_SerfLimit2            := 20 + Round( aIdv.Gene[Incr(K)] * 20);
  GA_MANAGEMENT_CheckUnitCount_SerfLimit3            := 40 + Round( aIdv.Gene[Incr(K)] * 30);
  GA_MANAGEMENT_CheckUnitCount_WorkerGoldCoef        :=  2 + aIdv.Gene[Incr(K)] * 3;
  GA_MANAGEMENT_CheckUnitCount_SerfGoldCoef          :=  1 + aIdv.Gene[Incr(K)] * 2;

  GA_PREDICTOR_SecondSchool_MinRequiredUnits         :=  Round(5 + aIdv.Gene[Incr(K)] * 30);

  if aLogIt AND (fLogPar <> nil) then
  begin
    fLogPar.AddTime(Format('GA_MANAGEMENT_GoldShortage                         : Word = %4d;',[ GA_MANAGEMENT_GoldShortage                  ]));
    fLogPar.AddTime(Format('GA_MANAGEMENT_CheckUnitCount_SerfLimit1            : Word = %4d;',[ GA_MANAGEMENT_CheckUnitCount_SerfLimit1     ]));
    fLogPar.AddTime(Format('GA_MANAGEMENT_CheckUnitCount_SerfLimit2            : Word = %4d;',[ GA_MANAGEMENT_CheckUnitCount_SerfLimit2     ]));
    fLogPar.AddTime(Format('GA_MANAGEMENT_CheckUnitCount_SerfLimit3            : Word = %4d;',[ GA_MANAGEMENT_CheckUnitCount_SerfLimit3     ]));
    fLogPar.AddTime(Format('GA_MANAGEMENT_CheckUnitCount_WorkerGoldCoef        : Single = %16.10f;',[ GA_MANAGEMENT_CheckUnitCount_WorkerGoldCoef ]));
    fLogPar.AddTime(Format('GA_MANAGEMENT_CheckUnitCount_SerfGoldCoef          : Single = %16.10f;',[ GA_MANAGEMENT_CheckUnitCount_SerfGoldCoef   ]));
    fLogPar.AddTime(Format('GA_PREDICTOR_SecondSchool_MinRequiredUnits         : Word = %4d;',[ GA_PREDICTOR_SecondSchool_MinRequiredUnits  ]));
  end;
end;


function TGAParameterization.GetParCnt_CityAllIn(): Word;
begin
  Result :=
    +GetParCnt_Manager()
    +GetParCnt_CityBuilder()
    +GetParCnt_Farm()
    +GetParCnt_Quarry()
    +GetParCnt_RoadPlanner()
    +GetParCnt_Forest()
    +GetParCnt_CityPlanner()
  ;
end;

procedure TGAParameterization.SetPar_CityAllIn(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
begin
  // Set global parameters
  SetPar_Manager     (aIdv, aLogIt, K);   Inc(K,GetParCnt_Manager());
  SetPar_CityBuilder (aIdv, aLogIt, K);   Inc(K,GetParCnt_CityBuilder());
  SetPar_Farm        (aIdv, aLogIt, K);   Inc(K,GetParCnt_Farm());
  SetPar_Quarry      (aIdv, aLogIt, K);   Inc(K,GetParCnt_Quarry());
  SetPar_RoadPlanner (aIdv, aLogIt, K);   Inc(K,GetParCnt_RoadPlanner());
  SetPar_Forest      (aIdv, aLogIt, K);   Inc(K,GetParCnt_Forest());
  SetPar_CityPlanner (aIdv, aLogIt, K);   Inc(K,GetParCnt_CityPlanner());
end;


function TGAParameterization.GetParCnt_CityBuilder(): Word;
begin
  Result := 4+5+5+2;
end;

procedure TGAParameterization.SetPar_CityBuilder(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
begin
  GA_BUILDER_BuildHouse_FieldMaxWork    := Max(1, 0 + aIdv.Gene[Incr(K)] * 2);
  GA_BUILDER_BuildHouse_RTPMaxWork      := Max(1, 0 + aIdv.Gene[Incr(K)] * 15);
  GA_BUILDER_BuildHouse_RoadMaxWork     := Max(1, 5 + aIdv.Gene[Incr(K)] * 20);
  GA_BUILDER_CreateShortcuts_MaxWork    := Max(1, 0 + aIdv.Gene[Incr(K)] * 10);

  GA_BUILDER_ChHTB_FractionCoef         := Max(1, 5 + aIdv.Gene[Incr(K)] * 25);
  GA_BUILDER_ChHTB_TrunkFactor          := Max(1, 8 + aIdv.Gene[Incr(K)] * 12);
  GA_BUILDER_ChHTB_TrunkBalance         := Max(0, 0 + aIdv.Gene[Incr(K)] * 5);
  GA_BUILDER_ChHTB_AllWorkerCoef        := Max(1, 8 + aIdv.Gene[Incr(K)] * 10);
  GA_BUILDER_ChHTB_FreeWorkerCoef       := Max(1, 8 + aIdv.Gene[Incr(K)] * 10);

  GA_BUILDER_Shortage_Stone             := Max(1, 5 + aIdv.Gene[Incr(K)] * 20);
  GA_BUILDER_Shortage_StoneReserve      := Max(1, 10 + aIdv.Gene[Incr(K)] * 40);
  GA_BUILDER_Shortage_Gold              := Max(1, 0 + aIdv.Gene[Incr(K)] * 20 + 20);
  GA_BUILDER_Shortage_Trunk             := Max(0.1, 1 + aIdv.Gene[Incr(K)] * 3);
  GA_BUILDER_Shortage_Wood              := Max(1, 13 + aIdv.Gene[Incr(K)] * 10);

  GA_PREDICTOR_WareNeedPerAWorker_Stone       := 0.50 + aIdv.Gene[Incr(K)] * 0.5;
  GA_PREDICTOR_WareNeedPerAWorker_StoneOffset := Round(1    + aIdv.Gene[Incr(K)] * 15);
  GA_PREDICTOR_WareNeedPerAWorker_Wood        := 0.25 + aIdv.Gene[Incr(K)] * 0.5;

  if aLogIt AND (fLogPar <> nil) then
  begin
    fLogPar.AddTime(Format('GA_BUILDER_BuildHouse_FieldMaxWork                 : Single = %16.10f;',[ GA_BUILDER_BuildHouse_FieldMaxWork          ]));
    fLogPar.AddTime(Format('GA_BUILDER_BuildHouse_RTPMaxWork                   : Single = %16.10f;',[ GA_BUILDER_BuildHouse_RTPMaxWork            ]));
    fLogPar.AddTime(Format('GA_BUILDER_BuildHouse_RoadMaxWork                  : Single = %16.10f;',[ GA_BUILDER_BuildHouse_RoadMaxWork           ]));
    fLogPar.AddTime(Format('GA_BUILDER_CreateShortcuts_MaxWork                 : Single = %16.10f;',[ GA_BUILDER_CreateShortcuts_MaxWork          ]));
    fLogPar.AddTime(Format('GA_BUILDER_ChHTB_FractionCoef                      : Single = %16.10f;',[ GA_BUILDER_ChHTB_FractionCoef               ]));
    fLogPar.AddTime(Format('GA_BUILDER_ChHTB_TrunkFactor                       : Single = %16.10f;',[ GA_BUILDER_ChHTB_TrunkFactor                ]));
    fLogPar.AddTime(Format('GA_BUILDER_ChHTB_TrunkBalance                      : Single = %16.10f;',[ GA_BUILDER_ChHTB_TrunkBalance               ]));
    fLogPar.AddTime(Format('GA_BUILDER_ChHTB_AllWorkerCoef                     : Single = %16.10f;',[ GA_BUILDER_ChHTB_AllWorkerCoef              ]));
    fLogPar.AddTime(Format('GA_BUILDER_ChHTB_FreeWorkerCoef                    : Single = %16.10f;',[ GA_BUILDER_ChHTB_FreeWorkerCoef             ]));
    fLogPar.AddTime(Format('GA_BUILDER_Shortage_Stone                          : Single = %16.10f;',[ GA_BUILDER_Shortage_Stone                   ]));
    fLogPar.AddTime(Format('GA_BUILDER_Shortage_StoneReserve                   : Single = %16.10f;',[ GA_BUILDER_Shortage_StoneReserve            ]));
    fLogPar.AddTime(Format('GA_BUILDER_Shortage_Gold                           : Single = %16.10f;',[ GA_BUILDER_Shortage_Gold                    ]));
    fLogPar.AddTime(Format('GA_BUILDER_Shortage_Trunk                          : Single = %16.10f;',[ GA_BUILDER_Shortage_Trunk                   ]));
    fLogPar.AddTime(Format('GA_BUILDER_Shortage_Wood                           : Single = %16.10f;',[ GA_BUILDER_Shortage_Wood                    ]));
    fLogPar.AddTime(Format('GA_PREDICTOR_WareNeedPerAWorker_Stone              : Single = %16.10f;',[ GA_PREDICTOR_WareNeedPerAWorker_Stone       ]));
    fLogPar.AddTime(Format('GA_PREDICTOR_WareNeedPerAWorker_StoneOffset        : Word = %4d;',      [ GA_PREDICTOR_WareNeedPerAWorker_StoneOffset ]));
    fLogPar.AddTime(Format('GA_PREDICTOR_WareNeedPerAWorker_Wood               : Single = %16.10f;',[ GA_PREDICTOR_WareNeedPerAWorker_Wood        ]));
  end;

end;


function TGAParameterization.GetParCnt_Farm(): Word;
begin
  Result := 3+4;
end;

procedure TGAParameterization.SetPar_Farm(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
begin
  GA_PLANNER_PlanFields_CanBuild   :=  0 + Round(aIdv.Gene[Incr(K)] * 75);
  GA_PLANNER_PlanFields_Dist       :=  0 + Round(aIdv.Gene[Incr(K)] * 75);
  GA_PLANNER_PlanFields_ExistField := 25 + Round(aIdv.Gene[Incr(K)] * 75);

  GA_PLANNER_FindPlaceForHouse_RouteFarm      := -2 + aIdv.Gene[Incr(K)] *   1 * 4; // 0-255
  GA_PLANNER_FindPlaceForHouse_FlatAreaFarm   :=  0 + aIdv.Gene[Incr(K)] *   3 * 4; // 0-82
  GA_PLANNER_FindPlaceForHouse_HouseDistFarm  :=  5 + aIdv.Gene[Incr(K)] *  10 * 1; // 3-100+
  GA_PLANNER_FindPlaceForHouse_CityCenterFarm :=  0 + aIdv.Gene[Incr(K)] *  10 * 1; // 3-100+;

  if aLogIt AND (fLogPar <> nil) then
  begin
    fLogPar.AddTime(Format('GA_PLANNER_PlanFields_CanBuild                     : Word = %4d;',[ GA_PLANNER_PlanFields_CanBuild   ]));
    fLogPar.AddTime(Format('GA_PLANNER_PlanFields_Dist                         : Word = %4d;',[ GA_PLANNER_PlanFields_Dist       ]));
    fLogPar.AddTime(Format('GA_PLANNER_PlanFields_ExistField                   : Word = %4d;',[ GA_PLANNER_PlanFields_ExistField ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForHouse_RouteFarm             : Single = %16.10f;',[ GA_PLANNER_FindPlaceForHouse_RouteFarm      ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForHouse_FlatAreaFarm          : Single = %16.10f;',[ GA_PLANNER_FindPlaceForHouse_FlatAreaFarm   ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForHouse_HouseDistFarm         : Single = %16.10f;',[ GA_PLANNER_FindPlaceForHouse_HouseDistFarm  ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForHouse_CityCenterFarm        : Single = %16.10f;',[ GA_PLANNER_FindPlaceForHouse_CityCenterFarm ]));
  end;
end;


function TGAParameterization.GetParCnt_Quarry(): Word;
begin
  Result := 5;
end;

procedure TGAParameterization.SetPar_Quarry(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
begin
  GA_PLANNER_FindPlaceForQuary_Obstacle	 := 40 + aIdv.Gene[Incr(K)] * 50;
  GA_PLANNER_FindPlaceForQuary_DistCity	 :=  0 + aIdv.Gene[Incr(K)] * 50;
  GA_PLANNER_FindPlaceForQuary_DistTimer :=  0 + aIdv.Gene[Incr(K)] * 15000;
  GA_PLANNER_FindPlaceForQuary_DistStone :=  0 + aIdv.Gene[Incr(K)] * 50;
  GA_PLANNER_FindPlaceForQuary_SnapCrit	 := 10 + aIdv.Gene[Incr(K)] * 50;

  if aLogIt AND (fLogPar <> nil) then
  begin
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForQuary_Obstacle              : Single = %16.10f;',[ GA_PLANNER_FindPlaceForQuary_Obstacle  ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForQuary_DistCity              : Single = %16.10f;',[ GA_PLANNER_FindPlaceForQuary_DistCity  ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForQuary_DistTimer             : Single = %16.10f;',[ GA_PLANNER_FindPlaceForQuary_DistTimer ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForQuary_DistStone             : Single = %16.10f;',[ GA_PLANNER_FindPlaceForQuary_DistStone ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForQuary_SnapCrit              : Single = %16.10f;',[ GA_PLANNER_FindPlaceForQuary_SnapCrit  ]));
  end;
end;


function TGAParameterization.GetParCnt_RoadPlanner(): Word;
begin
  Result := 8+8;
end;

procedure TGAParameterization.SetPar_RoadPlanner(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
begin
  GA_PATHFINDING_BasePrice        := 25 + Round( aIdv.Gene[Incr(K)] * 30 );
  GA_PATHFINDING_TurnPenalization :=  0 + Round( aIdv.Gene[Incr(K)] * 50 );
  GA_PATHFINDING_Road             := Min(GA_PATHFINDING_BasePrice, Round( aIdv.Gene[Incr(K)] * 50 ));
  GA_PATHFINDING_noBuildArea      := 15 + Round( aIdv.Gene[Incr(K)] * 35 );
  GA_PATHFINDING_Field            := 20 + Round( aIdv.Gene[Incr(K)] * 40 );
  GA_PATHFINDING_Coal             :=  0 + Round( aIdv.Gene[Incr(K)] * 50 );
  GA_PATHFINDING_Forest           := 30 + Round( aIdv.Gene[Incr(K)] * 50 );
  GA_PATHFINDING_OtherCase        :=  0 + Round( aIdv.Gene[Incr(K)] * 50 );

  GA_SHORTCUTS_BasePrice          := 50 + Round( aIdv.Gene[Incr(K)] * 40 );
  GA_SHORTCUTS_TurnPenalization   := 35 + Round( aIdv.Gene[Incr(K)] * 50 );
  GA_SHORTCUTS_Road               := Min(GA_SHORTCUTS_BasePrice,Round( aIdv.Gene[Incr(K)] * 50 ));
  GA_SHORTCUTS_noBuildArea        := 20 + Round( aIdv.Gene[Incr(K)] * 50 );
  GA_SHORTCUTS_Field              :=  0 + Round( aIdv.Gene[Incr(K)] * 50 );
  GA_SHORTCUTS_Coal               := 20 + Round( aIdv.Gene[Incr(K)] * 50 );
  GA_SHORTCUTS_Forest             := 10 + Round( aIdv.Gene[Incr(K)] * 40 );
  GA_SHORTCUTS_OtherCase          :=  0 + Round( aIdv.Gene[Incr(K)] * 50 );

  if aLogIt AND (fLogPar <> nil) then
  begin
    fLogPar.AddTime(Format('GA_PATHFINDING_BasePrice                           : Word = %4d;',[ GA_PATHFINDING_BasePrice        ]));
    fLogPar.AddTime(Format('GA_PATHFINDING_TurnPenalization                    : Word = %4d;',[ GA_PATHFINDING_TurnPenalization ]));
    fLogPar.AddTime(Format('GA_PATHFINDING_Road                                : Word = %4d;',[ GA_PATHFINDING_Road             ]));
    fLogPar.AddTime(Format('GA_PATHFINDING_noBuildArea                         : Word = %4d;',[ GA_PATHFINDING_noBuildArea      ]));
    fLogPar.AddTime(Format('GA_PATHFINDING_Field                               : Word = %4d;',[ GA_PATHFINDING_Field            ]));
    fLogPar.AddTime(Format('GA_PATHFINDING_Coal                                : Word = %4d;',[ GA_PATHFINDING_Coal             ]));
    fLogPar.AddTime(Format('GA_PATHFINDING_Forest                              : Word = %4d;',[ GA_PATHFINDING_Forest           ]));
    fLogPar.AddTime(Format('GA_PATHFINDING_OtherCase                           : Word = %4d;',[ GA_PATHFINDING_OtherCase        ]));
    fLogPar.AddTime(Format('GA_SHORTCUTS_BasePrice                             : Word = %4d;',[ GA_SHORTCUTS_BasePrice          ]));
    fLogPar.AddTime(Format('GA_SHORTCUTS_TurnPenalization                      : Word = %4d;',[ GA_SHORTCUTS_TurnPenalization   ]));
    fLogPar.AddTime(Format('GA_SHORTCUTS_Road                                  : Word = %4d;',[ GA_SHORTCUTS_Road               ]));
    fLogPar.AddTime(Format('GA_SHORTCUTS_noBuildArea                           : Word = %4d;',[ GA_SHORTCUTS_noBuildArea        ]));
    fLogPar.AddTime(Format('GA_SHORTCUTS_Field                                 : Word = %4d;',[ GA_SHORTCUTS_Field              ]));
    fLogPar.AddTime(Format('GA_SHORTCUTS_Coal                                  : Word = %4d;',[ GA_SHORTCUTS_Coal               ]));
    fLogPar.AddTime(Format('GA_SHORTCUTS_Forest                                : Word = %4d;',[ GA_SHORTCUTS_Forest             ]));
    fLogPar.AddTime(Format('GA_SHORTCUTS_OtherCase                             : Word = %4d;',[ GA_SHORTCUTS_OtherCase          ]));
  end;
end;


function TGAParameterization.GetParCnt_Forest(): Word;
begin
  Result := 6+12;
end;

procedure TGAParameterization.SetPar_Forest(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
begin
  GA_EYE_GetForests_MaxAB            :=   1 + aIdv.Gene[Incr(K)] * 200; // <0,201> Ignore trees in existing forest <0,255-AVOID_BUILDING_FOREST_MINIMUM)
  GA_EYE_GetForests_Radius           :=   3 + aIdv.Gene[Incr(K)] *   4; // Forest radius
  GA_EYE_GetForests_MinTrees         :=   1 + aIdv.Gene[Incr(K)] *   4; // Min trees in forest
  GA_EYE_GetForests_SPRndOwnLimMin   :=  55 + aIdv.Gene[Incr(K)] * 100; // Minimum influence of potential forest
  GA_EYE_GetForests_SPRndOwnLimMax   := 255 - aIdv.Gene[Incr(K)] * 100; // Maximum influence of potential forest
  GA_EYE_GetForests_MinRndSoil       :=  60 + aIdv.Gene[Incr(K)] *  22; // 0-82

  GA_PLANNER_FindPlaceForWoodcutter_TreeCnt       := 0 + aIdv.Gene[Incr(K)] *  21 * 1 ; // 0-~12
  GA_PLANNER_FindPlaceForWoodcutter_TreeCntTimer  := 12000 + aIdv.Gene[Incr(K)] * 12000; // 6000-18000 10-30 min
  GA_PLANNER_FindPlaceForWoodcutter_ExistForest   := 0 + aIdv.Gene[Incr(K)] * 255 * 2 ; // 0-1
  GA_PLANNER_FindPlaceForWoodcutter_Routes        :=-1 + aIdv.Gene[Incr(K)] *   2 * 1 ; // -255<->255
  GA_PLANNER_FindPlaceForWoodcutter_FlatArea      := 0 + aIdv.Gene[Incr(K)] *   3 * 2 ; // 0-81
  GA_PLANNER_FindPlaceForWoodcutter_Soil          := 0 + aIdv.Gene[Incr(K)] *   3 * 1 ; // 0-81
  GA_PLANNER_FindPlaceForWoodcutter_DistCrit      := 8 + aIdv.Gene[Incr(K)] *  10 * 1 ; // 0-20
  GA_PLANNER_FindPlaceForWoodcutter_DistTimer     := 6000 + aIdv.Gene[Incr(K)] * 6000; // 6000-18000 10-30 min
  GA_PLANNER_FindPlaceForWoodcutter_FreeTiles     := 0 + aIdv.Gene[Incr(K)] *   3 * 2 ; // 0-81
  GA_PLANNER_FindPlaceForWoodcutter_ABRange       := 0 + aIdv.Gene[Incr(K)] * 200; // 0-200
  GA_PLANNER_FindPlaceForWoodcutter_Radius        := 3 + aIdv.Gene[Incr(K)] *   4;
  GA_PLANNER_FindForestAround_MaxDist             := 5 + aIdv.Gene[Incr(K)] *   5; // 4-10

  if aLogIt AND (fLogPar <> nil) then
  begin
    fLogPar.AddTime(Format('GA_EYE_GetForests_MaxAB                            : Single = %16.10f;',[ GA_EYE_GetForests_MaxAB                        ]));
    fLogPar.AddTime(Format('GA_EYE_GetForests_Radius                           : Single = %16.10f;',[ GA_EYE_GetForests_Radius                       ]));
    fLogPar.AddTime(Format('GA_EYE_GetForests_MinTrees                         : Single = %16.10f;',[ GA_EYE_GetForests_MinTrees                     ]));
    fLogPar.AddTime(Format('GA_EYE_GetForests_SPRndOwnLimMin                   : Single = %16.10f;',[ GA_EYE_GetForests_SPRndOwnLimMin               ]));
    fLogPar.AddTime(Format('GA_EYE_GetForests_SPRndOwnLimMax                   : Single = %16.10f;',[ GA_EYE_GetForests_SPRndOwnLimMax               ]));
    fLogPar.AddTime(Format('GA_EYE_GetForests_MinRndSoil                       : Single = %16.10f;',[ GA_EYE_GetForests_MinRndSoil                   ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForWoodcutter_TreeCnt          : Single = %16.10f;',[ GA_PLANNER_FindPlaceForWoodcutter_TreeCnt      ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForWoodcutter_TreeCntTimer     : Single = %16.10f;',[ GA_PLANNER_FindPlaceForWoodcutter_TreeCntTimer ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForWoodcutter_ExistForest      : Single = %16.10f;',[ GA_PLANNER_FindPlaceForWoodcutter_ExistForest  ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForWoodcutter_Routes           : Single = %16.10f;',[ GA_PLANNER_FindPlaceForWoodcutter_Routes       ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForWoodcutter_FlatArea         : Single = %16.10f;',[ GA_PLANNER_FindPlaceForWoodcutter_FlatArea     ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForWoodcutter_Soil             : Single = %16.10f;',[ GA_PLANNER_FindPlaceForWoodcutter_Soil         ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForWoodcutter_DistCrit         : Single = %16.10f;',[ GA_PLANNER_FindPlaceForWoodcutter_DistCrit     ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForWoodcutter_DistTimer        : Single = %16.10f;',[ GA_PLANNER_FindPlaceForWoodcutter_DistTimer    ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForWoodcutter_FreeTiles        : Single = %16.10f;',[ GA_PLANNER_FindPlaceForWoodcutter_FreeTiles    ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForWoodcutter_ABRange          : Single = %16.10f;',[ GA_PLANNER_FindPlaceForWoodcutter_ABRange      ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForWoodcutter_Radius           : Single = %16.10f;',[ GA_PLANNER_FindPlaceForWoodcutter_Radius       ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindForestAround_MaxDist                : Single = %16.10f;',[ GA_PLANNER_FindForestAround_MaxDist            ]));
  end;
end;


function TGAParameterization.GetParCnt_CityPlanner(): Word;
begin
  Result := 2+3+2+4+7;
end;

procedure TGAParameterization.SetPar_CityPlanner(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
begin
  GA_PLANNER_ObstaclesInHousePlan_Tree       := 500 + aIdv.Gene[Incr(K)] * 100 * 10; // 0-3+
  GA_PLANNER_ObstaclesInHousePlan_Road       := 200 + aIdv.Gene[Incr(K)] *  50 * 10; // 0-5+

  GA_PLANNER_FieldCrit_PolyRoute             :=   0 + aIdv.Gene[Incr(K)] *   1 * 5; // 0-255
  GA_PLANNER_FieldCrit_FlatArea              :=   0 + aIdv.Gene[Incr(K)] *   3 * 4; // 0-82
  GA_PLANNER_FieldCrit_Soil                  :=   0 + aIdv.Gene[Incr(K)] *   3 * 1; // 0-82

  //GA_PLANNER_SnapCrit_SnapToHouse            :=   0 + aIdv.Gene[Incr(K)] *  10 * 5; // 0-5+
  //GA_PLANNER_SnapCrit_SnapToFields           :=   0 + aIdv.Gene[Incr(K)] *  10 * 3; // 0-5+
  //GA_PLANNER_SnapCrit_SnapToRoads            :=   0 + aIdv.Gene[Incr(K)] *  10 * 5; // 0-5+
  GA_PLANNER_SnapCrit_ObstacleInEntrance     :=   0 + aIdv.Gene[Incr(K)] *  10 * 100; // 0-1
  GA_PLANNER_SnapCrit_RoadInEntrance         :=   0 + aIdv.Gene[Incr(K)] *  10 * 30; // 0-1

  GA_PLANNER_SnapCrit_HouseOrRoad            :=   0 + aIdv.Gene[Incr(K)] *  10 * 5;
  GA_PLANNER_SnapCrit_NoBuild                :=   0 + aIdv.Gene[Incr(K)] *  10 * 5;
  GA_PLANNER_SnapCrit_Road                   :=   0 + aIdv.Gene[Incr(K)] *  10 * 5;
  GA_PLANNER_SnapCrit_Field                  :=   0 + aIdv.Gene[Incr(K)] *  10 * 5;

  GA_PLANNER_FindPlaceForHouse_SnapCrit      :=   0 + aIdv.Gene[Incr(K)] *   3 * 1; // var
  GA_PLANNER_FindPlaceForHouse_HouseDist     :=  15 + aIdv.Gene[Incr(K)] *  10 * 2; // 3-100+
  GA_PLANNER_FindPlaceForHouse_SeedDist      :=   0 + aIdv.Gene[Incr(K)] *  10 * 5; // 3-30
  GA_PLANNER_FindPlaceForHouse_CityCenter    :=   0 + aIdv.Gene[Incr(K)] *  10 * 5; // 3-100+
  GA_PLANNER_FindPlaceForHouse_Route         :=   0 + aIdv.Gene[Incr(K)] *   1 * 4; // 0-255
  GA_PLANNER_FindPlaceForHouse_FlatArea      :=   0 + aIdv.Gene[Incr(K)] *   3 * 2; // 0-82
  GA_PLANNER_PlaceWoodcutter_DistFromForest  :=   0 + aIdv.Gene[Incr(K)] *   2 * 1; // 0-X


  if aLogIt AND (fLogPar <> nil) then
  begin
    fLogPar.AddTime(Format('GA_PLANNER_ObstaclesInHousePlan_Tree               : Single = %16.10f;',[ GA_PLANNER_ObstaclesInHousePlan_Tree        ]));
    fLogPar.AddTime(Format('GA_PLANNER_ObstaclesInHousePlan_Road               : Single = %16.10f;',[ GA_PLANNER_ObstaclesInHousePlan_Road        ]));
    fLogPar.AddTime(Format('GA_PLANNER_FieldCrit_PolyRoute                     : Single = %16.10f;',[ GA_PLANNER_FieldCrit_PolyRoute              ]));
    fLogPar.AddTime(Format('GA_PLANNER_FieldCrit_FlatArea                      : Single = %16.10f;',[ GA_PLANNER_FieldCrit_FlatArea               ]));
    fLogPar.AddTime(Format('GA_PLANNER_FieldCrit_Soil                          : Single = %16.10f;',[ GA_PLANNER_FieldCrit_Soil                   ]));
    //fLogPar.AddTime(Format('GA_PLANNER_SnapCrit_SnapToHouse                    : Single = %16.10f;',[ GA_PLANNER_SnapCrit_SnapToHouse             ]));
    //fLogPar.AddTime(Format('GA_PLANNER_SnapCrit_SnapToFields                   : Single = %16.10f;',[ GA_PLANNER_SnapCrit_SnapToFields            ]));
    //fLogPar.AddTime(Format('GA_PLANNER_SnapCrit_SnapToRoads                    : Single = %16.10f;',[ GA_PLANNER_SnapCrit_SnapToRoads             ]));
    fLogPar.AddTime(Format('GA_PLANNER_SnapCrit_ObstacleInEntrance             : Single = %16.10f;',[ GA_PLANNER_SnapCrit_ObstacleInEntrance      ]));
    fLogPar.AddTime(Format('GA_PLANNER_SnapCrit_RoadInEntrance                 : Single = %16.10f;',[ GA_PLANNER_SnapCrit_RoadInEntrance          ]));
    fLogPar.AddTime(Format('GA_PLANNER_SnapCrit_HouseOrRoad                    : Single = %16.10f;',[ GA_PLANNER_SnapCrit_HouseOrRoad             ]));
    fLogPar.AddTime(Format('GA_PLANNER_SnapCrit_NoBuild                        : Single = %16.10f;',[ GA_PLANNER_SnapCrit_NoBuild                 ]));
    fLogPar.AddTime(Format('GA_PLANNER_SnapCrit_Road                           : Single = %16.10f;',[ GA_PLANNER_SnapCrit_Road                    ]));
    fLogPar.AddTime(Format('GA_PLANNER_SnapCrit_Field                          : Single = %16.10f;',[ GA_PLANNER_SnapCrit_Field                   ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForHouse_SnapCrit              : Single = %16.10f;',[ GA_PLANNER_FindPlaceForHouse_SnapCrit       ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForHouse_HouseDist             : Single = %16.10f;',[ GA_PLANNER_FindPlaceForHouse_HouseDist      ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForHouse_SeedDist              : Single = %16.10f;',[ GA_PLANNER_FindPlaceForHouse_SeedDist       ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForHouse_CityCenter            : Single = %16.10f;',[ GA_PLANNER_FindPlaceForHouse_CityCenter     ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForHouse_Route                 : Single = %16.10f;',[ GA_PLANNER_FindPlaceForHouse_Route          ]));
    fLogPar.AddTime(Format('GA_PLANNER_FindPlaceForHouse_FlatArea              : Single = %16.10f;',[ GA_PLANNER_FindPlaceForHouse_FlatArea       ]));
    fLogPar.AddTime(Format('GA_PLANNER_PlaceWoodcutter_DistFromForest          : Single = %16.10f;',[ GA_PLANNER_PlaceWoodcutter_DistFromForest   ]));
  end;
end;




function TGAParameterization.GetParCnt_ArmyAttack(): Word;
begin
  Result := 3 +(7-1) +12 +4 +1;
end;

procedure TGAParameterization.SetPar_ArmyAttack(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
begin
  GA_PATHFINDING_AvoidTraffic                    :=   0 +       aIdv.Gene[Incr(K)] *    3; //    1.5
  GA_PATHFINDING_AvoidSpecEnemy                  :=   0 +       aIdv.Gene[Incr(K)] *    2; //    1
  GA_PATHFINDING_AvoidEdges                      :=  80 +       aIdv.Gene[Incr(K)] *   80; //    1

  GA_ATTACK_SQUAD_ChangeTarget_DistTolerance     :=   4 +       aIdv.Gene[Incr(K)] *    6;  //   6
  GA_ATTACK_SQUAD_ChangeTarget_Delay             := 400 + Round(aIdv.Gene[Incr(K)] *  500); // 200
  GA_ATTACK_SQUAD_TargetReached_Position         :=   1 + Round(aIdv.Gene[Incr(K)] *    4); //   4
  GA_ATTACK_SQUAD_TargetReached_Unit             :=   8 + Round(aIdv.Gene[Incr(K)] *    8); //   4
  //GA_ATTACK_SQUAD_TargetReached_House            :=   0 + Round(aIdv.Gene[Incr(K)] *    8); //   8
  GA_ATTACK_SQUAD_TargetReached_RangedSquad      :=  12 + Round(aIdv.Gene[Incr(K)] *    5); //  15
  GA_ATTACK_SQUAD_MinWalkingDistance             :=  10 + Round(aIdv.Gene[Incr(K)] *   20); //   4

  GA_ATTACK_COMPANY_AttackRadius                 :=   9 + Round(aIdv.Gene[Incr(K)] *   11); //  20
  GA_ATTACK_COMPANY_ProtectRangedRadius          :=   6 + Round(aIdv.Gene[Incr(K)] *    8); //  10
  GA_ATTACK_COMPANY_AttackRangedGain             :=   0 +       aIdv.Gene[Incr(K)] *   10;  //   5
  GA_ATTACK_COMPANY_ProtectRangedGain            :=   2 +       aIdv.Gene[Incr(K)] *    5;  //   1
  GA_ATTACK_COMPANY_ProtectRangedAllInDist       :=   2 + Round(aIdv.Gene[Incr(K)] *    5); //   7
  GA_ATTACK_COMPANY_GroupTypePenalization        := 100 + Round(aIdv.Gene[Incr(K)] *  100); //   7
  GA_ATTACK_COMPANY_DecreaseThreat_Prio1         :=   0 +       aIdv.Gene[Incr(K)] *    1;  //   1
  GA_ATTACK_COMPANY_DecreaseThreat_Prio2         :=   0 +       aIdv.Gene[Incr(K)] *    1;  //   0.7
  GA_ATTACK_COMPANY_DecreaseThreat_Prio3         :=   0 +       aIdv.Gene[Incr(K)] *    1;  //   0.5
  GA_ATTACK_COMPANY_DecreaseThreat_Prio4         :=   0 +       aIdv.Gene[Incr(K)] *    1;  //   0.2
  GA_ATTACK_COMPANY_TimePerATile_Slow            :=   2 + Round(aIdv.Gene[Incr(K)] *    3); //   7
  GA_ATTACK_COMPANY_TimePerATile_Fast            :=   2 + Round(aIdv.Gene[Incr(K)] *    3); //   4

  GA_ATTACK_COMPANY_MinCombatSpacing             :=   2 + Round(aIdv.Gene[Incr(K)] *    3); //   3
  GA_ATTACK_COMPANY_MinWalkSpacing               :=   2 + Round(aIdv.Gene[Incr(K)] *    3); //   3
  GA_ATTACK_COMPANY_MinimumMovement              :=   3 + Round(aIdv.Gene[Incr(K)] *   15); //   5
  GA_ATTACK_COMPANY_Positioning_InitPolyCnt      :=   1 + Round(aIdv.Gene[Incr(K)] *    3); //   3

  GA_ARMY_MaxGgroupsInCompany                    :=   5 + Round(aIdv.Gene[Incr(K)] *   15); //   6


  if aLogIt AND (fLogPar <> nil) then
  begin
    fLogPar.AddTime(Format('GA_PATHFINDING_AvoidTraffic                        : Single = %16.10f;',[ GA_PATHFINDING_AvoidTraffic                ]));
    fLogPar.AddTime(Format('GA_PATHFINDING_AvoidSpecEnemy                      : Single = %16.10f;',[ GA_PATHFINDING_AvoidSpecEnemy              ]));
    fLogPar.AddTime(Format('GA_PATHFINDING_AvoidEdges                          : Single = %16.10f;',[ GA_PATHFINDING_AvoidEdges                  ]));

    fLogPar.AddTime(Format('GA_ATTACK_SQUAD_ChangeTarget_DistTolerance         : Single = %16.10f;',[ GA_ATTACK_SQUAD_ChangeTarget_DistTolerance ]));
    fLogPar.AddTime(Format('GA_ATTACK_SQUAD_ChangeTarget_Delay                 : Word = %4d;',      [ GA_ATTACK_SQUAD_ChangeTarget_Delay         ]));
    fLogPar.AddTime(Format('GA_ATTACK_SQUAD_TargetReached_Position             : Word = %4d;',      [ GA_ATTACK_SQUAD_TargetReached_Position     ]));
    fLogPar.AddTime(Format('GA_ATTACK_SQUAD_TargetReached_Unit                 : Word = %4d;',      [ GA_ATTACK_SQUAD_TargetReached_Unit         ]));
    //fLogPar.AddTime(Format('GA_ATTACK_SQUAD_TargetReached_House                : Word = %4d;',      [ GA_ATTACK_SQUAD_TargetReached_House        ]));
    fLogPar.AddTime(Format('GA_ATTACK_SQUAD_TargetReached_RangedSquad          : Word = %4d;',      [ GA_ATTACK_SQUAD_TargetReached_RangedSquad  ]));
    fLogPar.AddTime(Format('GA_ATTACK_SQUAD_MinWalkingDistance                 : Word = %4d;',      [ GA_ATTACK_SQUAD_MinWalkingDistance         ]));

    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_AttackRadius                     : Word = %4d;',      [ GA_ATTACK_COMPANY_AttackRadius             ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_ProtectRangedRadius              : Word = %4d;',      [ GA_ATTACK_COMPANY_ProtectRangedRadius      ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_AttackRangedGain                 : Single = %16.10f;',[ GA_ATTACK_COMPANY_AttackRangedGain         ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_ProtectRangedGain                : Single = %16.10f;',[ GA_ATTACK_COMPANY_ProtectRangedGain        ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_ProtectRangedAllInDist           : Word = %4d;',      [ GA_ATTACK_COMPANY_ProtectRangedAllInDist   ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_GroupTypePenalization            : Word = %4d;',      [ GA_ATTACK_COMPANY_GroupTypePenalization    ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_DecreaseThreat_Prio1             : Single = %16.10f;',[ GA_ATTACK_COMPANY_DecreaseThreat_Prio1     ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_DecreaseThreat_Prio2             : Single = %16.10f;',[ GA_ATTACK_COMPANY_DecreaseThreat_Prio2     ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_DecreaseThreat_Prio3             : Single = %16.10f;',[ GA_ATTACK_COMPANY_DecreaseThreat_Prio3     ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_DecreaseThreat_Prio4             : Single = %16.10f;',[ GA_ATTACK_COMPANY_DecreaseThreat_Prio4     ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_TimePerATile_Slow                : Word = %4d;',      [ GA_ATTACK_COMPANY_TimePerATile_Slow        ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_TimePerATile_Fast                : Word = %4d;',      [ GA_ATTACK_COMPANY_TimePerATile_Fast        ]));

    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_MinCombatSpacing                 : Word = %4d;',      [ GA_ATTACK_COMPANY_MinCombatSpacing         ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_MinWalkSpacing                   : Word = %4d;',      [ GA_ATTACK_COMPANY_MinWalkSpacing           ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_MinimumMovement                  : Word = %4d;',      [ GA_ATTACK_COMPANY_MinimumMovement          ]));
    fLogPar.AddTime(Format('GA_ATTACK_COMPANY_Positioning_InitPolyCnt          : Word = %4d;',      [ GA_ATTACK_COMPANY_Positioning_InitPolyCnt  ]));

    fLogPar.AddTime(Format('GA_ARMY_MaxGgroupsInCompany                        : Word = %4d;',      [ GA_ARMY_MaxGgroupsInCompany                ]));
  end;
end;




function TGAParameterization.GetParCnt_ArmyAttackNew(): Word;
begin
  Result := 2 + 6 + 10;
end;

procedure TGAParameterization.SetPar_ArmyAttackNew(aIdv: TGAIndividual; aLogIt: Boolean = False; K: Word = 0);
begin
  GA_PATHFINDING_AvoidSpecEnemy                      :=   0 + aIdv.Gene[Incr(K)] * 10;
  GA_PATHFINDING_AvoidEdges                          :=  80 + aIdv.Gene[Incr(K)] * 80;

  GA_ATTACK_NMAP_PrefillDistances_Groups             := Round( 0 + 50 * aIdv.Gene[Incr(K)] );
  //GA_ATTACK_NMAP_PrefillDistances_Houses             := Round( 0 + 50 * aIdv.Gene[Incr(K)] );
  GA_ATTACK_NMAP_TArmyBackwardFF_EnemyInfluence      := Round( 0 + 10 * aIdv.Gene[Incr(K)] );
  GA_ATTACK_NMAP_BackwardFlood_MaxEnemyInfluence     := Round( 0 + 50 * aIdv.Gene[Incr(K)] );
  GA_ATTACK_NMAP_BackwardFlood_MaxAllyInfluence      := Round( 0 + 50 * aIdv.Gene[Incr(K)] );
  GA_ATTACK_NMAP_EvaluateLine_QueueCnt               :=  0 + 2 * aIdv.Gene[Incr(K)];
  GA_ATTACK_NMAP_EvaluateLine_MinDist                :=  0 + 1 * aIdv.Gene[Incr(K)];

  GA_ATTACK_SUPERVISOR_EvalTarget_DistanceGroup      :=  0 + 10 * aIdv.Gene[Incr(K)];
  GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainMelee    :=  0 +  5 * aIdv.Gene[Incr(K)];
  GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainAntiHorse:=  0 +  5 * aIdv.Gene[Incr(K)];
  GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainRanged   :=  0 +  5 * aIdv.Gene[Incr(K)];
  GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainMounted  :=  0 +  5 * aIdv.Gene[Incr(K)];
  GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainRangDist :=  0 +  5 * aIdv.Gene[Incr(K)];
  GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainDist     :=  0 +500 * aIdv.Gene[Incr(K)];
  GA_ATTACK_SUPERVISOR_EvalTarget_OportunityGain     :=  0 + 10 * aIdv.Gene[Incr(K)];
  GA_ATTACK_SUPERVISOR_EvalTarget_OportunityDistGain :=  0 + 10 * aIdv.Gene[Incr(K)];
  GA_ATTACK_SUPERVISOR_UpdateAttacks_AttackThreshold :=  0 +  1 * aIdv.Gene[Incr(K)];

  if aLogIt AND (fLogPar <> nil) then
  begin
    fLogPar.AddTime(Format('GA_PATHFINDING_AvoidSpecEnemy                       : Single = %16.10f;',[ GA_PATHFINDING_AvoidSpecEnemy                        ]));
    fLogPar.AddTime(Format('GA_PATHFINDING_AvoidEdges                           : Single = %16.10f;',[ GA_PATHFINDING_AvoidEdges                            ]));
    fLogPar.AddTime(Format('GA_ATTACK_NMAP_PrefillDistances_Groups              : Word = %4d;',      [ GA_ATTACK_NMAP_PrefillDistances_Groups               ]));
  //fLogPar.AddTime(Format('GA_ATTACK_NMAP_PrefillDistances_Houses              : Word = %4d;',      [ GA_ATTACK_NMAP_PrefillDistances_Houses               ]));
    fLogPar.AddTime(Format('GA_ATTACK_NMAP_TArmyBackwardFF_EnemyInfluence       : Word = %4d;',      [ GA_ATTACK_NMAP_TArmyBackwardFF_EnemyInfluence        ]));
    fLogPar.AddTime(Format('GA_ATTACK_NMAP_BackwardFlood_MaxEnemyInfluence      : Word = %4d;',      [ GA_ATTACK_NMAP_BackwardFlood_MaxEnemyInfluence       ]));
    fLogPar.AddTime(Format('GA_ATTACK_NMAP_BackwardFlood_MaxAllyInfluence       : Word = %4d;',      [ GA_ATTACK_NMAP_BackwardFlood_MaxAllyInfluence        ]));
    fLogPar.AddTime(Format('GA_ATTACK_NMAP_EvaluateLine_QueueCnt                : Single = %16.10f;',[ GA_ATTACK_NMAP_EvaluateLine_QueueCnt                 ]));
    fLogPar.AddTime(Format('GA_ATTACK_NMAP_EvaluateLine_MinDist                 : Single = %16.10f;',[ GA_ATTACK_NMAP_EvaluateLine_MinDist                  ]));
    fLogPar.AddTime(Format('GA_ATTACK_SUPERVISOR_EvalTarget_DistanceGroup       : Single = %16.10f;',[ GA_ATTACK_SUPERVISOR_EvalTarget_DistanceGroup        ]));
    fLogPar.AddTime(Format('GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainMelee     : Single = %16.10f;',[ GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainMelee      ]));
    fLogPar.AddTime(Format('GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainAntiHorse : Single = %16.10f;',[ GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainAntiHorse  ]));
    fLogPar.AddTime(Format('GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainRanged    : Single = %16.10f;',[ GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainRanged     ]));
    fLogPar.AddTime(Format('GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainMounted   : Single = %16.10f;',[ GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainMounted    ]));
    fLogPar.AddTime(Format('GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainRangDist  : Single = %16.10f;',[ GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainRangDist   ]));
    fLogPar.AddTime(Format('GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainDist      : Single = %16.10f;',[ GA_ATTACK_SUPERVISOR_EvalTarget_ThreatGainDist       ]));
    fLogPar.AddTime(Format('GA_ATTACK_SUPERVISOR_EvalTarget_OportunityGain      : Single = %16.10f;',[ GA_ATTACK_SUPERVISOR_EvalTarget_OportunityGain       ]));
    fLogPar.AddTime(Format('GA_ATTACK_SUPERVISOR_EvalTarget_OportunityDistGain  : Single = %16.10f;',[ GA_ATTACK_SUPERVISOR_EvalTarget_OportunityDistGain   ]));
    fLogPar.AddTime(Format('GA_ATTACK_SUPERVISOR_UpdateAttacks_AttackThreshold  : Single = %16.10f;',[ GA_ATTACK_SUPERVISOR_UpdateAttacks_AttackThreshold   ]));
  end;
end;

end.