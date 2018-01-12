unit Runner_Game;
{$I KaM_Remake.inc}
interface
uses
  Forms, Unit_Runner, Windows, SysUtils, Classes, KromUtils, Math,
  KM_CommonClasses, KM_Defaults, KM_Points, KM_CommonUtils,
  KM_GameApp, KM_ResLocales, KM_Log, KM_HandsCollection, KM_ResTexts, KM_Resource,
  KM_Terrain, KM_Units, KM_Units_Warrior, KM_Campaigns, KM_AIFields, KM_Houses,
  GeneticAlgorithm, KM_AICityPlanner, KM_CityBuilder, KM_CityPlanner, KM_Eye;


type
  //Typical usage:
  //SetUp, Execute(1), Execute(2) .. Execute(N), TearDown

  TKMRunnerGA_Common = class(TKMRunnerCommon)
  private
    fOldPopulation, fNewPopulation: TGAPopulation;
    fAlgorithm: TGAAlgorithm;
    fFitnessCalc: TGAFitnessCalc;
    gLog, gLogPar: TKMLog;

    procedure DefaultValues(); virtual;
  protected
    procedure SetUp(); override;
    procedure TearDown(); override;
    procedure SimulateMap(aRun, aIdx: Integer; aSinglePLMapName: String; aSaveGame: Boolean = False);
  end;

  TKMRunnerGA_CityPlanner = class(TKMRunnerGA_Common)
  private
    procedure SetParameters(aIdv: TGAIndividual; aLogIt: Boolean = False);
  protected
    procedure Execute(aRun: Integer); override;
    function CostFunction(): Single;
  end;

  TKMRunnerGA = class(TKMRunnerGA_Common)
  private
    procedure SetParameters(aIdv: TGAIndividual; aLogIt: Boolean = False);
  protected
    procedure Execute(aRun: Integer); override;
    function CostFunction(aIdv: TGAIndividual): Single;
  end;

  TKMRunnerStone = class(TKMRunnerCommon)
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
    procedure TearDown; override;
  end;

  TKMRunnerFight95 = class(TKMRunnerCommon)
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
    procedure TearDown; override;
  end;

  TKMRunnerAIBuild = class(TKMRunnerCommon)
  private
    HTotal, WTotal, WFTotal, GTotal: Cardinal;
    HAver, WAver, WFAver, GAver: Single;
    HandsCnt, Runs: Integer;
    Time: Cardinal;
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
    procedure TearDown; override;
  end;

  TKMVortamicPF = class(TKMRunnerCommon)
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
    procedure TearDown; override;
  end;

  TKMReplay = class(TKMRunnerCommon)
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
    procedure TearDown; override;
  end;

  TKMVas01 = class(TKMRunnerCommon)
  protected
    procedure SetUp; override;
    procedure Execute(aRun: Integer); override;
    procedure TearDown; override;
  end;


const
  GA_SIMULATION_TIME_IN_MIN = 1/60;
  GA_POPULATION_CNT = 50;
  GA_INDIVIDUALS_IN_TOURNAMENT = 4;
  GA_GENE_CNT = 13;
  GA_MAPS_CNT = 5;
  GA_START_MUTATION = 0.2;
  GA_FINAL_MUTATION = 0.005;
  GA_CROSSOVER_COEF = 0.8;

implementation
uses KM_HandSpectator, KM_ResWares, KM_ResHouses, KM_Hand, KM_UnitsCollection;



{ TKMRunnerGA_Common }
procedure TKMRunnerGA_Common.SetUp;
begin
  inherited;
  fResults.ValueCount := 1;
  fResults.TimesCount := Ceil(10*60 * GA_SIMULATION_TIME_IN_MIN);
  // Do something before simulation
  fOldPopulation := nil;
  fNewPopulation := TGAPopulation.Create(GA_POPULATION_CNT, GA_GENE_CNT, True);
  fAlgorithm := TGAAlgorithm.Create;
  fFitnessCalc := TGAFitnessCalc.Create;
  DefaultValues();
  gLog := TKMLog.Create(ExeDir + '\Utils\Runner\LOG_GA.log'); //First thing - create a log
  gLogPar := TKMLog.Create(ExeDir + '\Utils\Runner\LOG_GA_PAR.log');
end;

procedure TKMRunnerGA_Common.TearDown;
begin
  inherited;
  // Do something after simulation
  fOldPopulation.Free;
  fNewPopulation.Free;
  fAlgorithm.Free;
  fFitnessCalc.Free;
  gLog.Free;
  gLogPar.Free
end;


procedure TKMRunnerGA_Common.DefaultValues();
var
  I,K: Integer;
  Idv: TGAIndividual;
begin
  for I := 0 to fNewPopulation.Count - 1 do
  begin
    Idv := fNewPopulation[I];
    for K := 0 to Idv.Count - 1 do
      Idv.Gene[K] := Random;
  end;
end;


procedure TKMRunnerGA_Common.SimulateMap(aRun, aIdx: Integer; aSinglePLMapName: String; aSaveGame: Boolean = False);
begin
  gGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + '..\..\Maps\' + aSinglePLMapName + '\' + aSinglePLMapName + '.dat', 'GA');

  gMySpectator.Hand.FogOfWar.RevealEverything;
  gGameApp.Game.GamePlayInterface.Viewport.PanTo(KMPointF(0, 60), 0);
  gGameApp.Game.GamePlayInterface.Viewport.Zoom := 0.25;
  //SetKaMSeed(aRun + 1);

  SimulateGame;
  if aSaveGame then
    gGameApp.Game.Save('GA Test #' + IntToStr(aRun) + 'n' + IntToStr(aIdx), Now);
end;




{ TKMRunnerGA_CityPlanner }
procedure TKMRunnerGA_CityPlanner.SetParameters(aIdv: TGAIndividual; aLogIt: Boolean = False);
  function Incr(var Idx: Integer): Integer;
  begin
    Result := Idx;
    Inc(Idx);
  end;
var
  I: Integer;
begin
  I := 0;
  GA_PLANNER := True;
  // House build
  // Please do NOT break this order and style use alt + shift + click if you want select / edit multiple colums at once!!!
  GA_PLANNER_SnapCrit_SnapToHouse                   := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  GA_PLANNER_SnapCrit_SnapToFields                  := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  GA_PLANNER_SnapCrit_SnapToRoads                   := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  GA_PLANNER_FindPlaceForHouse_SnapCrit             := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  GA_PLANNER_FindPlaceForHouse_DistCrit             := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  GA_PLANNER_FindPlaceForHouse_TreeInPlan           := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  GA_PLANNER_FindPlaceForHouse_FarmCrit             := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  GA_PLANNER_FindPlaceForWoodcutter_DistFromForest  := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  GA_PLANNER_FindPlaceForWoodcutter_DistCrit        := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  GA_PLANNER_FindPlaceForWoodcutter_TreeCnt         := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  GA_PLANNER_FindPlaceForWoodcutter_SnapToEdge      := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  GA_PLANNER_FindPlaceForWoodcutter_CanPlaceTreeCnt := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  GA_PLANNER_FindPlaceForWoodcutter_PlansAround     := Max(0.1, aIdv.Gene[Incr(I)] * 50);
  if aLogIt then
  begin
    gLogPar.AddTime('GA_PLANNER_SnapCrit_SnapToHouse                   : Single = ' + FloatToStr( GA_PLANNER_SnapCrit_SnapToHouse                   ) + ';');
    gLogPar.AddTime('GA_PLANNER_SnapCrit_SnapToFields                  : Single = ' + FloatToStr( GA_PLANNER_SnapCrit_SnapToFields                  ) + ';');
    gLogPar.AddTime('GA_PLANNER_SnapCrit_SnapToRoads                   : Single = ' + FloatToStr( GA_PLANNER_SnapCrit_SnapToRoads                   ) + ';');
    gLogPar.AddTime('GA_PLANNER_FindPlaceForHouse_SnapCrit             : Single = ' + FloatToStr( GA_PLANNER_FindPlaceForHouse_SnapCrit             ) + ';');
    gLogPar.AddTime('GA_PLANNER_FindPlaceForHouse_DistCrit             : Single = ' + FloatToStr( GA_PLANNER_FindPlaceForHouse_DistCrit             ) + ';');
    gLogPar.AddTime('GA_PLANNER_FindPlaceForHouse_TreeInPlan           : Single = ' + FloatToStr( GA_PLANNER_FindPlaceForHouse_TreeInPlan           ) + ';');
    gLogPar.AddTime('GA_PLANNER_FindPlaceForHouse_FarmCrit             : Single = ' + FloatToStr( GA_PLANNER_FindPlaceForHouse_FarmCrit             ) + ';');
    gLogPar.AddTime('GA_PLANNER_FindPlaceForWoodcutter_DistFromForest  : Single = ' + FloatToStr( GA_PLANNER_FindPlaceForWoodcutter_DistFromForest  ) + ';');
    gLogPar.AddTime('GA_PLANNER_FindPlaceForWoodcutter_DistCrit        : Single = ' + FloatToStr( GA_PLANNER_FindPlaceForWoodcutter_DistCrit        ) + ';');
    gLogPar.AddTime('GA_PLANNER_FindPlaceForWoodcutter_TreeCnt         : Single = ' + FloatToStr( GA_PLANNER_FindPlaceForWoodcutter_TreeCnt         ) + ';');
    gLogPar.AddTime('GA_PLANNER_FindPlaceForWoodcutter_SnapToEdge      : Single = ' + FloatToStr( GA_PLANNER_FindPlaceForWoodcutter_SnapToEdge      ) + ';');
    gLogPar.AddTime('GA_PLANNER_FindPlaceForWoodcutter_CanPlaceTreeCnt : Single = ' + FloatToStr( GA_PLANNER_FindPlaceForWoodcutter_CanPlaceTreeCnt ) + ';');
    gLogPar.AddTime('GA_PLANNER_FindPlaceForWoodcutter_PlansAround     : Single = ' + FloatToStr( GA_PLANNER_FindPlaceForWoodcutter_PlansAround     ) + ';');
  end;
end;


function TKMRunnerGA_CityPlanner.CostFunction(): Single;
const
  MAX_BID = 1000000;
var
  HMA: THouseMappingArray;
  PlannedHouses: TPlannedHousesArray;
  Loc: TKMPoint;

  function IsCompletedRoad(aPoint: TKMPoint): Boolean; inline;
  begin
    Result := gTerrain.TileIsWalkableRoad(aPoint);
  end;
  function IsCompletedField(aPoint: TKMPoint): Boolean; inline;
  begin
    Result := gTerrain.TileIsCornField(aPoint);
  end;
  function IsCompletedWine(aPoint: TKMPoint): Boolean; inline;
  begin
    Result := gTerrain.TileIsWineField(aPoint);
  end;

  function ClosestDistance(aHT: THouseType): Single;
  const
    MAX_DIST = 1000;
  var
    I: Integer;
    Output, Bid: Single;
    HT: THouseType;
  begin
    Output := MAX_DIST;
    for HT in HOUSE_DEPENDENCE[aHT] do
      for I := 0 to PlannedHouses[HT].Count - 1 do
      begin
        Bid := KMDistanceAbs(Loc, PlannedHouses[HT].Plans[I].Loc);
        if (Bid < Output) then
          Output := Bid;
      end;
    if (Output = MAX_DIST) then
      Output := 0;
    Result := Output;
  end;

  function AllDistances(aHT: THouseType): Single;
  var
    I: Integer;
    HT: THouseType;
  begin
    Result := 0;
    for HT in HOUSE_DEPENDENCE[aHT] do
      for I := 0 to PlannedHouses[HT].Count - 1 do
        Result := Result + KMDistanceAbs(Loc, PlannedHouses[HT].Plans[I].Loc);
  end;

  function SnapCrit(aHT: THouseType): Single;
  const
    SNAP_TO_HOUSE = 5;
    SNAP_TO_ROAD = 4;
    SNAP_TO_FIELD = 2;
  var
    I,Dist: Integer;
    Point: TKMPoint;
    Output: Single;
    Dir: TDirection;
  begin
    Output := 0;
    // Snap to road / field / wine / house / edge of map / mountains
    Dist := 1;
    for Dir := Low(HMA[aHT].Surroundings[Dist]) to High(HMA[aHT].Surroundings[Dist]) do
      for I := Low(HMA[aHT].Surroundings[Dist,Dir]) to High(HMA[aHT].Surroundings[Dist,Dir]) do
      begin
        Point := KMPointAdd(Loc, HMA[aHT].Surroundings[Dist,Dir,I]);
        Output := Output
                  + Byte(not (tpBuild in gTerrain.Land[Point.Y,Point.X].Passability)) * SNAP_TO_HOUSE
                  + Byte(IsCompletedRoad(Point)) * SNAP_TO_ROAD
                  + Byte(IsCompletedField(Point) OR IsCompletedWine(Point)) * SNAP_TO_FIELD;
      end;
    Result := Output;
  end;

  function WoodcutCrit(aIdx: Integer): Single;
  const
    SCAN_RAD = 5;
    PLAN_PENALIZATION = 4;
    POTENTIAL_TREE_PRICE = 3;
    TREE_PRICE = 5;
    EDGE_PRICE = 2;
  var
    X,Y,minL,maxL: Integer;
    FLoc: TKMPoint;
    Output: Single;
  begin
    FLoc := PlannedHouses[ht_Woodcutters].Plans[aIdx].SpecPoint;
    Output := 4*Abs(KMDistanceAbs(FLoc, PlannedHouses[ht_Woodcutters].Plans[aIdx].Loc)-2); // Place woocutter close to cutting point
    Output := Output - ClosestDistance(ht_Store); // We want wodcutter near city
    for Y := Max(1,FLoc.Y-SCAN_RAD) to Min(gTerrain.MapY,FLoc.Y+SCAN_RAD) do
    for X := Max(1,FLoc.X-SCAN_RAD) to Min(gTerrain.MapX,FLoc.X+SCAN_RAD) do
    begin
      if not (tpBuild in gTerrain.Land[Y,X].Passability) then // No plans around forest
        Output := Output - PLAN_PENALIZATION;
      if gTerrain.ObjectIsChopableTree(X, Y) then // Trees in forest
        Output := Output + TREE_PRICE;
      if gTerrain.TileGoodForTree(X, Y) then
        Output := Output + POTENTIAL_TREE_PRICE;
    end;
    minL := FLoc.Y-SCAN_RAD;
    maxL := FLoc.Y+SCAN_RAD;
    for X := Max(1,FLoc.X-SCAN_RAD) to Min(gTerrain.MapX-1,FLoc.X+SCAN_RAD) do
    begin
      if (minL < 1) OR not gRes.Tileset.TileIsRoadable( gTerrain.Land[minL,X].Terrain ) then
        Output := Output + EDGE_PRICE;
      if (maxL > gTerrain.MapY-1) OR not gRes.Tileset.TileIsRoadable( gTerrain.Land[maxL,X].Terrain ) then
        Output := Output + EDGE_PRICE;
    end;
    // Snap forest to edge
    minL := FLoc.X-SCAN_RAD;
    maxL := FLoc.X+SCAN_RAD;
    for Y := Max(1,FLoc.Y-SCAN_RAD) to Min(gTerrain.MapY-1,FLoc.Y+SCAN_RAD) do
    begin
      if (minL < 1) OR not gRes.Tileset.TileIsRoadable( gTerrain.Land[Y,minL].Terrain ) then
        Output := Output + EDGE_PRICE;
      if (maxL > gTerrain.MapX-1) OR not gRes.Tileset.TileIsRoadable( gTerrain.Land[Y,maxL].Terrain ) then
        Output := Output + EDGE_PRICE;
    end;
    Result := Output;
  end;

const
  PL = 1;
var
  K: Integer;
  Output: Single;
  HT: THouseType;
begin
  Output := 0.0;
  HMA := gAIFields.Eye.HousesMapping;
  PlannedHouses := gHands.Hands[PL].AI.CityManagement.Builder.Planner.PlannedHouses;

  for HT := HOUSE_MIN to HOUSE_MAX do
    for K := 0 to PlannedHouses[HT].Count - 1 do
    begin
      Loc := PlannedHouses[HT].Plans[K].Loc;
      Output := Output + SnapCrit(HT) + AllDistances(HT);
    end;
  Result := Output;
end;


procedure TKMRunnerGA_CityPlanner.Execute(aRun: Integer);
const
  MIN_SCORE = - 1000000;
var
  I, MapNum: Integer;
  Score, BestScore, MUTATION: Single;
  Idv: TGAIndividual;
begin
  // Run 1 generation of GA
  MUTATION := aRun / (fResults.ChartsCount * 1.0);
  MUTATION := Abs(GA_FINAL_MUTATION + (GA_START_MUTATION - GA_FINAL_MUTATION) * (1 - MUTATION));
  fAlgorithm.Mutation := MUTATION;
  fAlgorithm.CrossoverCoef := GA_CROSSOVER_COEF;
  fAlgorithm.IndividualsInTournament := GA_INDIVIDUALS_IN_TOURNAMENT; // This may be changed during simulation

  fOldPopulation.Free;
  fOldPopulation := fNewPopulation;
  BestScore := MIN_SCORE;

  for MapNum := 1 to GA_MAPS_CNT do
  begin
    for I := 0 to GA_POPULATION_CNT - 1 do
    begin
      SetParameters(fOldPopulation[I]);
      SimulateMap(aRun, I, 'GA_' + IntToStr(MapNum));// Name for maps is GA_1, GA_2 ...
      Score := CostFunction();
      fOldPopulation[I].Fitness := fOldPopulation[I].Fitness + Score;
      if (Score > BestScore) then
      begin
        BestScore := Score;
        //gGameApp.Game.Save('GA Run no' + IntToStr(aRun), Now);
      end;
    end;
  end;
  fNewPopulation := TGAPopulation.Create(GA_POPULATION_CNT, GA_GENE_CNT, False);
  fAlgorithm.EvolvePopulation(fOldPopulation, fNewPopulation);

  Idv := fOldPopulation.Fittest();

  // Save best score + parameters of best individual
  fResults.Value[aRun, 0] := Round(Idv.Fitness);
  gLog.AddTime('GA:' + IntToStr(aRun) + '. run; best Score: ' + FloatToStr(Idv.Fitness));

  BestScore := MIN_SCORE;
  for I := 0 to aRun - 1 do
    if (fResults.Value[I, 0] > BestScore) then
      BestScore := fResults.Value[I, 0];
  if (BestScore < Idv.Fitness) then
  begin
    SetParameters(Idv, True);
    //gGameApp.Game.Save('GA Run no' + IntToStr(aRun), Now);
  end;

  gGameApp.Stop(gr_Silent);
end;


{
procedure TKMRunnerGA_CityPlanner.Execute(aRun: Integer);
const
  MIN_SCORE = - 1000000;
var
  I, PL, MapNum: Integer;
  Score, BestScore, MUTATION: Single;
  Idv: TGAIndividual;
begin
  // Run 1 generation of GA
  MUTATION := aRun / (fResults.ChartsCount * 1.0);
  MUTATION := Abs(GA_FINAL_MUTATION + (GA_START_MUTATION - GA_FINAL_MUTATION) * (1 - MUTATION));
  fAlgorithm.Mutation := MUTATION;
  fAlgorithm.CrossoverCoef := GA_CROSSOVER_COEF;
  fAlgorithm.IndividualsInTournament := GA_INDIVIDUALS_IN_TOURNAMENT; // This may be changed during simulation

  fOldPopulation.Free;
  fOldPopulation := fNewPopulation;
  BestScore := MIN_SCORE;

  PL := 1;
  for MapNum := 1 to GA_MAPS_CNT do
  begin
    SimulateMap(aRun, 'GA_' + IntToStr(MapNum)); // Name for maps is GA_1, GA_2 ...
    for I := 0 to GA_POPULATION_CNT - 1 do
    begin
      gHands.Hands[PL].AI.CityManagement.Builder.Destroy;
      gHands.Hands[PL].AI.CityManagement.Builder := TKMCityBuilder.Create(PL);
      SetParameters(fOldPopulation[I]);
      gHands.Hands[PL].AI.CityManagement.AfterMissionInit();
      Score := CostFunction();
      fOldPopulation[I].Fitness := fOldPopulation[I].Fitness + Score;
      if (Score > BestScore) then
      begin
        BestScore := Score;
        //gGameApp.Game.Save('GA Run no' + IntToStr(aRun), Now);
      end;
    end;
  end;
  fNewPopulation := TGAPopulation.Create(GA_POPULATION_CNT, GA_GENE_CNT, False);
  fAlgorithm.EvolvePopulation(fOldPopulation, fNewPopulation);

  Idv := fOldPopulation.Fittest();

  // Save best score + parameters of best individual
  fResults.Value[aRun, 0] := Round(Idv.Fitness);
  gLog.AddTime('GA:' + IntToStr(aRun) + '. run; best Score: ' + FloatToStr(Idv.Fitness));

  BestScore := MIN_SCORE;
  for I := 0 to aRun - 1 do
    if (fResults.Value[I, 0] > BestScore) then
      BestScore := fResults.Value[I, 0];
  if (BestScore < Idv.Fitness) then
  begin
    SetParameters(Idv, True);
    //gGameApp.Game.Save('GA Run no' + IntToStr(aRun), Now);
  end;

  gGameApp.Stop(gr_Silent);
end;
//}













{ TKMRunnerGA }
procedure TKMRunnerGA.SetParameters(aIdv: TGAIndividual; aLogIt: Boolean = False);
var
  I: Integer;
begin
  if aLogIt then
    for I := 0 to GA_GENE_CNT - 1 do
      gLog.AddTime('Par ' + FloatToStr(I) + ': ' + FloatToStr( aIdv.Gene[I] ));
end;


function TKMRunnerGA.CostFunction(aIdv: TGAIndividual): Single;
  var
    I: Integer;
    Output: Single;
begin
  Output := 0.0;
  for I := 0 to GA_GENE_CNT - 1 do
    Output := Output + aIdv.Gene[I] - 1;
  Result := Output;
end;


procedure TKMRunnerGA.Execute(aRun: Integer);
const
  MIN_SCORE = - 1000000;
var
  I, MapNum: Integer;
  Score, BestScore, MUTATION: Single;
  Idv: TGAIndividual;
begin
  // Run 1 generation of GA
  MUTATION := aRun / (fResults.ChartsCount * 1.0);
  MUTATION := Abs(GA_FINAL_MUTATION + (GA_START_MUTATION - GA_FINAL_MUTATION) * (1 - MUTATION));
  fAlgorithm.Mutation := MUTATION;
  fAlgorithm.IndividualsInTournament := GA_INDIVIDUALS_IN_TOURNAMENT; // This may be changed during simulation

  fOldPopulation.Free;
  fOldPopulation := fNewPopulation;
  for I := 0 to GA_POPULATION_CNT - 1 do
    fOldPopulation[I].Fitness := CostFunction(fOldPopulation[I]);

  fNewPopulation := TGAPopulation.Create(GA_POPULATION_CNT, GA_GENE_CNT, False);
  fAlgorithm.EvolvePopulation(fOldPopulation, fNewPopulation);

  Idv := fOldPopulation.Fittest();

  // Save best score + parameters of best individual
  fResults.Value[aRun, 0] := Round(Idv.Fitness);
  gLog.AddTime('GA:' + IntToStr(aRun) + '. run; best Score: ' + FloatToStr(Idv.Fitness));

  BestScore := MIN_SCORE;
  for I := 0 to aRun - 1 do
    if (fResults.Value[I, 0] > BestScore) then
      BestScore := fResults.Value[I, 0];
  if (BestScore < Idv.Fitness) OR (aRun = 0) then
  begin
    gLog.AddTime('OK|');
    //SetParameters(Idv, True);
    //gGameApp.Game.Save('GA Run no' + IntToStr(aRun), Now);
  end;

  gGameApp.Stop(gr_Silent);
end;








procedure TKMRunnerStone.SetUp;
begin
  inherited;
  fResults.ValueCount := 1;
//  fResults.TimesCount := 0;

  AI_GEN_INFLUENCE_MAPS := False;
  AI_GEN_NAVMESH := False;
  DYNAMIC_TERRAIN := False;
end;


procedure TKMRunnerStone.TearDown;
begin
  inherited;
  AI_GEN_INFLUENCE_MAPS := True;
  AI_GEN_NAVMESH := True;
  DYNAMIC_TERRAIN := True;
end;


procedure TKMRunnerStone.Execute(aRun: Integer);
var
  I,K: Integer;
  L: TKMPointList;
  P: TKMPoint;
begin
  //Total amount of stone = 4140
  gTerrain := TKMTerrain.Create;
  gTerrain.LoadFromFile(ExeDir + 'Maps\StoneMines\StoneMines.map', False);

  SetKaMSeed(aRun+1);

  //Stonemining is done programmatically, by iterating through all stone tiles
  //and mining them if conditions are right (like Stonemasons would do)

  L := TKMPointList.Create;
  for I := 1 to gTerrain.MapY - 2 do
  for K := 1 to gTerrain.MapX - 1 do
  if gTerrain.TileIsStone(K,I) > 0 then
    L.Add(KMPoint(K,I));

  I := 0;
  fResults.Value[aRun, 0] := 0;
  repeat
    L.GetRandom(P);

    if gTerrain.TileIsStone(P.X,P.Y) > 0 then
    begin
      if gTerrain.CheckPassability(KMPointBelow(P), tpWalk) then
      begin
        gTerrain.DecStoneDeposit(P);
        fResults.Value[aRun, 0] := fResults.Value[aRun, 0] + 3;
        I := 0;
      end;
    end
    else
      L.Remove(P);

    Inc(I);
    if I > 200 then
      Break;
  until (L.Count = 0);

  FreeAndNil(gTerrain);
end;


procedure TKMRunnerFight95.SetUp;
begin
  inherited;
  fResults.ValueCount := 2;
//  fResults.TimesCount := 2*60*10;

  DYNAMIC_TERRAIN := False;
end;


procedure TKMRunnerFight95.TearDown;
begin
  inherited;
  DYNAMIC_TERRAIN := True;
end;


procedure TKMRunnerFight95.Execute(aRun: Integer);
begin
  gGameApp.NewEmptyMap(128, 128);
  SetKaMSeed(aRun + 1);

  //fPlayers[0].AddUnitGroup(ut_Cavalry, KMPoint(63, 64), dir_E, 8, 24);
  //fPlayers[1].AddUnitGroup(ut_Swordsman, KMPoint(65, 64), dir_W, 8, 24);

  //fPlayers[0].AddUnitGroup(ut_Swordsman, KMPoint(63, 64), dir_E, 8, 24);
  //fPlayers[1].AddUnitGroup(ut_Hallebardman, KMPoint(65, 64), dir_W, 8, 24);

  //fPlayers[0].AddUnitGroup(ut_Hallebardman, KMPoint(63, 64), dir_E, 8, 24);
  //fPlayers[1].AddUnitGroup(ut_Cavalry, KMPoint(65, 64), dir_W, 8, 24);

  gHands[0].AddUnitGroup(ut_Swordsman, KMPoint(63, 64), dir_E, 8, 24);
  gHands[1].AddUnitGroup(ut_Swordsman, KMPoint(65, 64), dir_W, 8, 24);

  gHands[1].UnitGroups[0].OrderAttackUnit(gHands[0].Units[0], True);

  SimulateGame;

  fResults.Value[aRun, 0] := gHands[0].Stats.GetUnitQty(ut_Any);
  fResults.Value[aRun, 1] := gHands[1].Stats.GetUnitQty(ut_Any);

  gGameApp.Stop(gr_Silent);
end;


{ TKMRunnerAIBuild }
procedure TKMRunnerAIBuild.SetUp;
begin
  inherited;
  if gLog = nil then
    gLog := TKMLog.Create(ExeDir + 'Utils\Runner\Runner_Log.log');

  fResults.ValueCount := 6;
//  fResults.TimesCount := 60*60*10;
  fResults.TimesCount := 10;
  HTotal := 0;
  HAver := 0;
  WTotal := 0;
  WAver := 0;
  GTotal := 0;
  GAver := 0;
  Runs := 0;
  Time := TimeGet;

  //SKIP_LOADING_CURSOR := True;
end;


procedure TKMRunnerAIBuild.TearDown;
begin
  //
  HAver := HTotal / (Runs*HandsCnt);
  WAver := WTotal / (Runs*HandsCnt);
  WFAver := WFTotal / (Runs*HandsCnt);
  GAver := GTotal / (Runs*HandsCnt);

  gLog.AddTime('==================================================================');
  gLog.AddTime(Format('HAver: %3.2f  WAver: %3.2f  WFAver: %3.2f  GAver: %5.2f', [HAver, WAver, WFAver, GAver]));
  gLog.AddTime('TimeAver: ' + IntToStr(Round(GetTimeSince(Time)/Runs)));
  gLog.AddTime('Time: ' + IntToStr(GetTimeSince(Time)));
  inherited;
end;


procedure TKMRunnerAIBuild.Execute(aRun: Integer);
var Str: String;
    I: Integer;
    HRun, HRunT, WRun, WRunT, WFRun, WFRunT, GRun, GRunT: Cardinal;
    StartT: Cardinal;
begin
  //gGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + '..\..\MapsMP\Cursed Ravine\Cursed Ravine.dat', 'Cursed Ravine');
  gGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + '..\..\Maps\GA_'+IntToStr(aRun+1)+'\GA_'+IntToStr(aRun+1)+'.dat', 'GA');
  Inc(Runs);
  gMySpectator.Hand.FogOfWar.RevealEverything;
  gGameApp.Game.GamePlayInterface.Viewport.PanTo(KMPointF(136, 25), 0);
  gGameApp.Game.GamePlayInterface.Viewport.Zoom := 0.25;

  SetKaMSeed(aRun + 1);
  StartT := TimeGet;

  SimulateGame;

  gGameApp.Game.Save('AI Build #' + IntToStr(aRun), Now);

  {fResults.Value[aRun, 0] := gHands[0].Stats.GetWarriorsTrained;
  fResults.Value[aRun, 1] := gHands[1].Stats.GetWarriorsTrained;
  fResults.Value[aRun, 2] := gHands[2].Stats.GetWarriorsTrained;
  fResults.Value[aRun, 3] := gHands[3].Stats.GetWarriorsTrained;
  fResults.Value[aRun, 4] := gHands[4].Stats.GetWarriorsTrained;
  fResults.Value[aRun, 5] := gHands[5].Stats.GetWarriorsTrained;}

  {fResults.Value[aRun, 0] := gHands[0].Stats.GetGoodsProduced(rt_Stone);
  fResults.Value[aRun, 1] := gHands[1].Stats.GetGoodsProduced(rt_Stone);
  fResults.Value[aRun, 2] := gHands[2].Stats.GetGoodsProduced(rt_Stone);
  fResults.Value[aRun, 3] := gHands[3].Stats.GetGoodsProduced(rt_Stone);
  fResults.Value[aRun, 4] := gHands[4].Stats.GetGoodsProduced(rt_Stone);}

//  fResults.Value[aRun, 0] := gHands[0].Stats.GetHousesBuilt;
//  fResults.Value[aRun, 1] := gHands[1].Stats.GetHousesBuilt;
//  fResults.Value[aRun, 2] := gHands[2].Stats.GetHousesBuilt;
//  fResults.Value[aRun, 3] := gHands[3].Stats.GetHousesBuilt;
//  fResults.Value[aRun, 4] := gHands[4].Stats.GetHousesBuilt;
//  fResults.Value[aRun, 5] := gHands[5].Stats.GetHousesBuilt;

  gLog.AddTime('------- Run ' + IntToStr(Runs));
  HandsCnt := gHands.Count - 1;
  HRunT := 0;
  WRunT := 0;
  WFRunT := 0;
  GRunT := 0;
  for I := 1 to HandsCnt do
  begin
    Str := '';
    HRun := gHands[I].Stats.GetHousesBuilt;
    HRunT := HRunT + HRun;
    HTotal := HTotal + HRun;
    WRun := gHands[I].Stats.GetWarriorsTrained;
    WTotal := WTotal + WRun;
    WRunT := WRunT + WRun;
    WFRun := gHands[I].Stats.GetWaresProduced(wt_Warfare);
    WFTotal := WFTotal + WFRun;
    WFRunT := WFRunT + WFRun;
    GRun := gHands[I].Stats.GetWaresProduced(wt_All);
    GTotal := GTotal + GRun;
    GRunT := GRunT + GRun;
    Str := Str + Format('Hand%d: H: %d  W: %d  WF: %d  G: %d', [I, HRun, WRun, WFRun, GRun]);
    gLog.AddTime(Str);
  end;
  gLog.AddTime(Format('HRunAver: %3.2f  WRunAver: %3.2f  WFRunAver: %3.2f  GRunAver: %5.2f',
               [HRunT/HandsCnt, WRunT/HandsCnt, WFRunT/HandsCnt,  GRunT/HandsCnt]));
  gLog.AddTime('Time: ' + IntToStr(GetTimeSince(StartT)));

  gGameApp.Stop(gr_Silent);
end;


{ TKMVortamicPF }
procedure TKMVortamicPF.SetUp;
begin
  inherited;
  fResults.ValueCount := 1;
//  fResults.TimesCount := 5*60*10;
end;

procedure TKMVortamicPF.TearDown;
begin
  inherited;

end;

procedure TKMVortamicPF.Execute(aRun: Integer);
var
  T: Cardinal;
begin
  inherited;

  //Intended to be run multiple of 4 times to compare different PF algorithms
//  PathFinderToUse := (aRun mod 4) div 2; //01230123 > 00110011
//  CACHE_PATHFINDING := Boolean(aRun mod 2);  //0101

  gGameApp.NewSingleMap(ExtractFilePath(ParamStr(0)) + '..\..\Maps\Vortamic\Vortamic.dat', 'Across the Desert');

  SetKaMSeed(aRun div 4 + 1); //11112222

  T := TimeGet;
  SimulateGame;
  fResults.Value[aRun, 0] := TimeGet - T;

  gGameApp.Stop(gr_Silent);
end;


{ TKMReplay }
procedure TKMReplay.SetUp;
begin
  inherited;
  fResults.ValueCount := 1;
//  fResults.TimesCount := 2*60*60*10;
end;

procedure TKMReplay.TearDown;
begin
  inherited;

end;

procedure TKMReplay.Execute(aRun: Integer);
var
  T: Cardinal;
begin
  inherited;

  gGameApp.NewReplay(ExtractFilePath(ParamStr(0)) + '\runner_replay.bas');

  //Don't set random seed or the replay won't work

  T := TimeGet;
  SimulateGame;
  fResults.Value[aRun, 0] := TimeGet - T;

  gGameApp.Stop(gr_Silent);
end;


{ TKMVas01 }
procedure TKMVas01.SetUp;
begin
  inherited;
  fResults.ValueCount := 1;
//  fResults.TimesCount := 2*60*10;
end;

procedure TKMVas01.TearDown;
begin
  inherited;

end;

procedure TKMVas01.Execute(aRun: Integer);
const
  cmp: TKMCampaignId = (Byte('V'), Byte('A'), Byte('S'));
var
  C: TKMCampaign;
  T: Cardinal;
begin
  inherited;

  C := gGameApp.Campaigns.CampaignById(cmp);
  gGameApp.NewCampaignMap(C, 1);

  gMySpectator.FOWIndex := -1;
  gGameApp.Game.GamePlayInterface.Viewport.PanTo(KMPointF(162, 26), 0);
  gGameApp.Game.GamePlayInterface.Viewport.Zoom := 0.5;

  //Don't set random seed or the replay won't work

  T := TimeGet;
  SimulateGame;
  fResults.Value[aRun, 0] := TimeGet - T;

  gGameApp.Stop(gr_Silent);
end;

initialization
  RegisterRunner(TKMRunnerGA_CityPlanner);
  RegisterRunner(TKMRunnerGA);
  RegisterRunner(TKMRunnerStone);
  RegisterRunner(TKMRunnerFight95);
  RegisterRunner(TKMRunnerAIBuild);
  RegisterRunner(TKMVortamicPF);
  RegisterRunner(TKMReplay);
  RegisterRunner(TKMVas01);


end.
