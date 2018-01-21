unit KM_Eye;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, KromUtils, Math, SysUtils,
  KM_Defaults, KM_Points, KM_CommonClasses, KM_CommonTypes, KM_FloodFill,
  KM_ResHouses, KM_Houses, KM_ResWares,
  KM_AIArmyEvaluation, KM_AIInfluences;

const
  MAX_SCAN_DIST_FROM_HOUSE = 10;
  MIN_SCAN_DIST_FROM_HOUSE = 2; // Houses must have at least 1 tile of space between them

type
  TDirection = (dirN,dirE,dirS,dirW);
  THouseMapping = record // Record of vectors from loc of house to specific point
    Tiles: TKMPointArray; // Tiles inside house plan
    Surroundings: array[1..MAX_SCAN_DIST_FROM_HOUSE] of array[TDirection] of TKMPointArray; // Tiles around house plan in specific distance and direction
    MoveToEntrance: array[TDirection] of TKMPoint; // Move entrance of house in dependence of direction to be able to place house plan
    // Note: if we want place houses close to each other without "try and see" method we have to move from Loc of exist house into surrounding tiles and then move by entrance offset of new house)
  end;
  THouseMappingArray = array [HOUSE_MIN..HOUSE_MAX] of THouseMapping;

  TKMSearchResource = class(TKMQuickFlood)
  private
    fSearch: SmallInt;
    fVisitArr: TKMByte2Array;
    fListOfPoints: TKMPointList;
  protected
    function CanBeVisited(const aX,aY: SmallInt): Boolean; override;
    function IsVisited(const aX,aY: SmallInt): Boolean; override;
    procedure MarkAsVisited(const aX,aY: SmallInt); override;
  public
    constructor Create(aMinLimit, aMaxLimit: TKMPoint; var aVisitArr: TKMByte2Array; const aScanEightTiles: Boolean = False); reintroduce;
    procedure QuickFlood(aX,aY,aSearch: SmallInt; var aListOfPoints: TKMPointList);  reintroduce;
  end;

  // Transform game data into "AI view" ... this is how AI see the map
  TKMEye = class
  private
    fOwner: TKMHandIndex;
    fHousesMapping: THouseMappingArray;
    fGoldMines, fIronMines, fStoneMiningTiles, fCoalMiningTiles: TKMPointList;

    fArmyEvaluation: TKMArmyEvaluation;

    procedure InitHousesMapping();
    function CheckResourcesNearMine(aLoc: TKMPoint; aHT: THouseType): Boolean;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property HousesMapping: THouseMappingArray read fHousesMapping write fHousesMapping;
    property ArmyEvaluation: TKMArmyEvaluation read fArmyEvaluation;

    procedure AfterMissionInit();
    procedure UpdateState(aTick: Cardinal);
    procedure ScanLocResources(out aGoldMineCnt, aIronMineCnt, aFieldCnt, aBuildCnt: Integer);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);

    function CanPlaceHouse(aLoc: TKMPoint; aHT: THouseType; aIgnoreTrees: Boolean = False): Boolean;
    function CanAddHousePlan(aLoc: TKMPoint; aHT: THouseType; aIgnoreAvoidBuilding: Boolean = False; aIgnoreTrees: Boolean = False; aIgnoreLocks: Boolean = True): Boolean;

    function GetMineLocs(aHT: THouseType): TKMPointTagList;
    function GetStoneLocs(): TKMPointTagList;
    function GetCoalLocs(): TKMPointTagList;
    function GetForests(): TKMPointTagList;
    function GetClosestTrees(aLoc: TKMPoint): TKMPointTagList;
    function GetHouse(aHT: THouseType; var aLoc: TKMPoint): Boolean;
    function GetCityCenterPoints(aMultiplePoints: Boolean = False): TKMPointArray;

    procedure Paint(aRect: TKMRect);
  end;



implementation
uses
  KM_Terrain, KM_Hand, KM_Resource, KM_AIFields, KM_HandsCollection, KM_RenderAux,
  KM_NavMesh;


{ TKMEye }
constructor TKMEye.Create();
begin
  fGoldMines := TKMPointList.Create();
  fIronMines := TKMPointList.Create();
  fStoneMiningTiles := TKMPointList.Create();
  fCoalMiningTiles := TKMPointList.Create();

  fArmyEvaluation := TKMArmyEvaluation.Create();

  InitHousesMapping();
end;

destructor TKMEye.Destroy();
begin
  fGoldMines.Free;
  fIronMines.Free;
  fStoneMiningTiles.Free;
  fCoalMiningTiles.Free;

  fArmyEvaluation.Free;

  inherited;
end;


procedure TKMEye.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('Eye');
  SaveStream.Write(fOwner);

  fGoldMines.SaveToStream(SaveStream);
  fIronMines.SaveToStream(SaveStream);
  fStoneMiningTiles.SaveToStream(SaveStream);
  fCoalMiningTiles.SaveToStream(SaveStream);

  fArmyEvaluation.Save(SaveStream);

  // The following does not requires save
  // fHousesMapping
end;

procedure TKMEye.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('Eye');
  LoadStream.Read(fOwner);

  fGoldMines.LoadFromStream(LoadStream);
  fIronMines.LoadFromStream(LoadStream);
  fStoneMiningTiles.LoadFromStream(LoadStream);
  fCoalMiningTiles.LoadFromStream(LoadStream);

  fArmyEvaluation.Load(LoadStream);
end;


procedure TKMEye.AfterMissionInit();
const
  COAL_NUM = 1;
  STONE_NUM = 2;
  TREE_NUM = 3;
var
  X,Y: Integer;
  Loc: TKMPoint;
  VisitArr: TKMByte2Array;
  SearchResource: TKMSearchResource;
begin
  SetLength(VisitArr, gTerrain.MapY, gTerrain.MapX);
  for Y := Low(VisitArr) to High(VisitArr) do
  for X := Low(VisitArr[Y]) to High(VisitArr[Y]) do
    VisitArr[Y,X] := 0;

  SearchResource := TKMSearchResource.Create(KMPoint(1,1), KMPoint(gTerrain.MapX-1,gTerrain.MapY-1), VisitArr);
  try
    for Y := 1 to gTerrain.MapY - 1 do
    for X := 1 to gTerrain.MapX - 1 do
      if (VisitArr[Y,X] = 0) then
      begin
        Loc := KMPoint(X,Y);
        if CanAddHousePlan(Loc, ht_GoldMine, True, False) then
        begin
          if CheckResourcesNearMine(Loc, ht_GoldMine) then
            fGoldMines.Add(Loc);
        end
        else if CanAddHousePlan(Loc, ht_IronMine, True, False) then
        begin
          if CheckResourcesNearMine(Loc, ht_IronMine) then
            fIronMines.Add(Loc);
        end
        else if (gTerrain.TileIsCoal(X, Y) > 1) then
        begin
          if CanAddHousePlan(Loc, ht_CoalMine, True, False) then
            fCoalMiningTiles.Add(Loc);
        end
        else if (gTerrain.TileIsStone(X, Y) > 1) then
        begin
          if (Y < gTerrain.MapY - 1) AND (tpWalk in gTerrain.Land[Y+1,X].Passability) then
            fStoneMiningTiles.Add(KMPoint(X,Y));
        end;
        //else if (gTerrain.ObjectIsChopableTree(X, Y)) then
        //  VisitArr[Y,X] := TREE_NUM;
      end;
  finally
    SearchResource.Free();
  end;
end;


procedure TKMEye.UpdateState(aTick: Cardinal);
begin
  fArmyEvaluation.UpdateState(aTick);
end;


procedure TKMEye.ScanLocResources(out aGoldMineCnt, aIronMineCnt, aFieldCnt, aBuildCnt: Integer);
  function FindSeparateMines(aMineType: THouseType; var MineList: TKMPointList): Word;
  var
    I,K, Output: Integer;
    InfluenceArr: array of Boolean;
  begin
    Output := 0;
    SetLength(InfluenceArr, MineList.Count);
    for I := Low(InfluenceArr) to High(InfluenceArr) do
      InfluenceArr[I] := False;
    for I := 0 to MineList.Count-1 do
      if (gAIFields.Influences.GetBestOwner(MineList.Items[I].X, MineList.Items[I].Y) = fOwner) then
      begin
        InfluenceArr[I] := True;
        Output := Output + 1;
        for K := 0 to I-1 do
          if InfluenceArr[K] then
          begin
            if (MineList.Items[K].Y <> MineList.Items[I].Y)
              OR (  Abs(MineList.Items[K].X - MineList.Items[I].X) > (3 + Byte(aMineType = ht_IronMine)) ) then
              continue
            else
            begin
              Output := Output - 1;
              InfluenceArr[I] := False;
              break;
            end;
          end;
      end;
    Result := Output;
  end;
var
  X,Y: Integer;
begin
  aFieldCnt := 0;
  aBuildCnt := 0;
  for Y := 1 to gTerrain.MapY - 1 do
  for X := 1 to gTerrain.MapX - 1 do
    if (gAIFields.Influences.GetBestOwner(X, Y) = fOwner) then
    begin
      if (tpBuild in gTerrain.Land[Y,X].Passability) then
        aBuildCnt := aBuildCnt + 1;
      if gHands[fOwner].CanAddFieldPlan(KMPoint(X,Y), ft_Corn) then
        aFieldCnt := aFieldCnt + 1;
    end;
  aIronMineCnt := FindSeparateMines(ht_IronMine, fIronMines);
  aGoldMineCnt := FindSeparateMines(ht_GoldMine, fGoldMines);
end;


procedure TKMEye.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
  //fFinder.OwnerUpdate(fOwner);
end;


// Search for ore - gold and iron mines have similar requirements so there are booth
function TKMEye.CheckResourcesNearMine(aLoc: TKMPoint; aHT: THouseType): Boolean;
var
  X,Y: Integer;
begin
  Result := True;
  for X := Max(aLoc.X-4, 1) to Min(aLoc.X+3+Byte(aHT = ht_GoldMine), gTerrain.MapX-1) do
    for Y := Max(aLoc.Y-8, 1) to aLoc.Y do
      if   (aHT = ht_GoldMine) AND (gTerrain.TileIsGold(X, Y) > 0)
        OR (aHT = ht_IronMine) AND (gTerrain.TileIsIron(X, Y) > 0) then
        Exit;
  Result := False; //Didn't find any ore
end;


// Create mapping of surrounding tiles for each house
procedure TKMEye.InitHousesMapping();
var
  EnterOff: ShortInt;
  House: THouseType;
  POMArr: array[1-MAX_SCAN_DIST_FROM_HOUSE..4+MAX_SCAN_DIST_FROM_HOUSE,1-MAX_SCAN_DIST_FROM_HOUSE..4+MAX_SCAN_DIST_FROM_HOUSE] of Byte;
  CntArr, Index: array [TDirection] of Integer;

  procedure SearchAndFill(const aIdx: Integer; aFill: Boolean = False);
  var
    PointAdded: Boolean;
    X,Y: Integer;
    Dir: TDirection;
  begin
    //FillChar(CntArr, SizeOf(CntArr), #0); //Clear up
    for dir := Low(CntArr) to High(CntArr) do
      CntArr[dir] := 0;

    for Y := 1-aIdx to 4+aIdx do
    for X := 1-aIdx to 4+aIdx do
      if (POMArr[Y,X] = aIdx) then
      begin
        PointAdded := False;
        if (X = Index[dirW]-aIdx) then
        begin
          PointAdded := True;
          if aFill then
            fHousesMapping[House].Surroundings[aIdx,dirW,CntArr[dirW]] := KMPoint(X - 3 - EnterOff, Y - 4);
          Inc(CntArr[dirW],1);
        end
        else if (X = Index[dirE]+aIdx) then
        begin
          PointAdded := True;
          if aFill then
            fHousesMapping[House].Surroundings[aIdx,dirE,CntArr[dirE]] := KMPoint(X - 3 - EnterOff, Y - 4);
          Inc(CntArr[dirE],1);
        end;
        if (Y = Index[dirS]+aIdx) then
        begin
          if aFill then
            fHousesMapping[House].Surroundings[aIdx,dirS,CntArr[dirS]] := KMPoint(X - 3 - EnterOff, Y - 4);
          Inc(CntArr[dirS],1);
        end
        else if not PointAdded OR (Y = Index[dirN]-aIdx) then // Plans with cutted top corners
        begin
          if aFill then
            fHousesMapping[House].Surroundings[aIdx,dirN,CntArr[dirN]] := KMPoint(X - 3 - EnterOff, Y - 4);
          Inc(CntArr[dirN],1);
        end;
      end;
    if not aFill then
      for dir := Low(CntArr) to High(CntArr) do
        SetLength(fHousesMapping[House].Surroundings[aIdx,dir], CntArr[dir]);
  end;

var
  I, X,Y,aX,aY, ActualIdx, Cnt: Integer;
  HA: THouseArea;
begin
  for House := HOUSE_MIN to HOUSE_MAX do
  begin
    EnterOff := gRes.Houses[House].EntranceOffsetX;

    // Init POMArr with value 255;
    for Y := Low(POMArr) to High(POMArr) do
    for X := Low(POMArr[Y]) to High(POMArr[Y]) do
      POMArr[Y,X] := 255;

    // Find house plan and save its shape into POMArr
    HA := gRes.Houses[House].BuildArea;
    Cnt := 0;
    for Y := 1 to 4 do
    for X := 1 to 4 do
      if (HA[Y,X] <> 0) then
      begin
        POMArr[Y,X] := 0;
        Inc(Cnt,1);
      end;

    // Save vectors from entrance to each tile which is in house plan
    SetLength(fHousesMapping[House].Tiles, Cnt);
    Cnt := 0;
    for Y := 1 to 4 do
    for X := 1 to 4 do
      if (POMArr[Y,X] = 0) then
      begin
        fHousesMapping[House].Tiles[Cnt] := KMPoint(X - 3 - EnterOff, Y - 4);
        Inc(Cnt);
      end;

    // Create around the house plan layers of increasing values in dependence on distance from the plan
    for I := 1 to MAX_SCAN_DIST_FROM_HOUSE do
    begin
      ActualIdx := I-1;
      for Y := 1-ActualIdx to 4+ActualIdx do
      for X := 1-ActualIdx to 4+ActualIdx do
        if (POMArr[Y,X] = ActualIdx) then
          for aY := -1 to 1 do
          for aX := -1 to 1 do
            if (POMArr[Y+aY,X+aX] > I) then
              POMArr[Y+aY,X+aX] := I;
    end;

    // Calculate size of plan
    Index[dirN] := 3 - Byte(POMArr[2,2] = 0) - Byte(POMArr[1,2] = 0);
    Index[dirS] := 4;
    Index[dirW] := 2 - Byte(POMArr[4,1] = 0);
    Index[dirE] := 3 + Byte(POMArr[4,4] = 0);

    // Get entrance with respect to array HA
    for X := 1 to 4 do
      if (HA[4,X] = 2) then
        break;
    fHousesMapping[House].MoveToEntrance[dirN] := KMPoint(0, 0);
    fHousesMapping[House].MoveToEntrance[dirS] := KMPoint(0, 4 - Index[dirN]);
    fHousesMapping[House].MoveToEntrance[dirW] := KMPoint(X - Index[dirE], 0);
    fHousesMapping[House].MoveToEntrance[dirE] := KMPoint(X - Index[dirW], 0);

    // Fill fHousesSurroundings
    for I := 1 to MAX_SCAN_DIST_FROM_HOUSE do
    begin
      SearchAndFill(I, False);
      SearchAndFill(I, True);
    end;

  end;
end;


// This function is copied (and reworked) from TKMTerrain.CanPlaceHouse and edited to be able to ignore trees
function TKMEye.CanPlaceHouse(aLoc: TKMPoint; aHT: THouseType; aIgnoreTrees: Boolean = False): Boolean;
var
  Output: Boolean;
  I,X,Y: Integer;
begin
  Output := True;
  for I := Low(fHousesMapping[aHT].Tiles) to High(fHousesMapping[aHT].Tiles) do
  begin
    X := aLoc.X + fHousesMapping[aHT].Tiles[I].X;
    Y := aLoc.Y + fHousesMapping[aHT].Tiles[I].Y;
    // Inset one tile from map edges
    Output := Output AND gTerrain.TileInMapCoords(X, Y, 1);
    // Mines have specific requirements
    case aHT of
      ht_IronMine: Output := Output AND gTerrain.TileGoodForIron(X, Y);
      ht_GoldMine: Output := Output AND gTerrain.CanPlaceGoldmine(X, Y);
      else         Output := Output AND (  (tpBuild in gTerrain.Land[Y,X].Passability) OR (aIgnoreTrees AND gTerrain.ObjectIsChopableTree(X, Y) AND gHands[fOwner].CanAddFieldPlan(KMPoint(X,Y), ft_Wine))  );
    end;
    if not Output then
      break;
  end;
  Result := Output;
end;


// Modified version of TKMHand.CanAddHousePlan - added possibilities
// aIgnoreAvoidBuilding = ignore avoid building areas
// aIgnoreTrees = ignore trees inside of house plan
function TKMEye.CanAddHousePlan(aLoc: TKMPoint; aHT: THouseType; aIgnoreAvoidBuilding: Boolean = False; aIgnoreTrees: Boolean = False; aIgnoreLocks: Boolean = True): Boolean;
var
  X, Y, I, K, PL: Integer;
  Dir: TDirection;
begin
  Result := False;

  // Check if we can place house on terrain, this also makes sure the house is
  // at least 1 tile away from map border (skip that below)
  if not CanPlaceHouse(aLoc, aHT, aIgnoreTrees) then
    Exit;

  // Make sure that we dont put new house into another plan (just entrance is enought because houses have similar size)
  //if gHands[fOwner].BuildList.HousePlanList.HasPlan(KMPoint(aLoc.X,aLoc.Y)) then
  //  Exit;

  // Scan tiles inside house plan
  for I := Low(fHousesMapping[aHT].Tiles) to High(fHousesMapping[aHT].Tiles) do
  begin
    X := aLoc.X + fHousesMapping[aHT].Tiles[I].X;
    Y := aLoc.Y + fHousesMapping[aHT].Tiles[I].Y;

    // Check with AvoidBuilding array to secure that new house will not be build in forests / coal tiles
    if aIgnoreAvoidBuilding then
    begin
      if not aIgnoreLocks AND
        ((gAIFields.Influences.AvoidBuilding[Y, X] = AVOID_BUILDING_HOUSE_OUTSIDE_LOCK)
          OR (gAIFields.Influences.AvoidBuilding[Y, X] = AVOID_BUILDING_HOUSE_INSIDE_LOCK)) then
      Exit;
    end
    else if (gAIFields.Influences.AvoidBuilding[Y, X] > 0) then
      Exit;

    //This tile must not contain fields/houseplans of allied players
    for PL := 0 to gHands.Count - 1 do
      if (gHands[fOwner].Alliances[PL] = at_Ally) then// AND (PL <> fOwner) then
        if (gHands[PL].BuildList.FieldworksList.HasField(KMPoint(X,Y)) <> ft_None) then
          Exit;
  end;

  // Scan tiles in distance 1 from house plan
  I := 1;
  for Dir := Low(fHousesMapping[aHT].Surroundings[I]) to High(fHousesMapping[aHT].Surroundings[I]) do
  for K := Low(fHousesMapping[aHT].Surroundings[I,Dir]) to High(fHousesMapping[aHT].Surroundings[I,Dir]) do
  begin
    Y := aLoc.Y + fHousesMapping[aHT].Surroundings[I,Dir,K].Y;
    X := aLoc.X + fHousesMapping[aHT].Surroundings[I,Dir,K].X;
    // Surrounding tiles must not be a house
    for PL := 0 to gHands.Count - 1 do
      if (gHands[fOwner].Alliances[PL] = at_Ally) then
        if gHands[PL].BuildList.HousePlanList.HasPlan(KMPoint(X,Y)) then
          Exit;
    if (aHT in [ht_GoldMine, ht_IronMine]) then
      continue;
    // Make sure we can add road below house;
    // Woodcutters / CoalMine may take place for mine so its arena must be scaned completely
    if (
         (Dir = dirS) OR (aHT = ht_Woodcutters) OR (aHT = ht_CoalMine)
       ) AND not (
         (gHands[fOwner].BuildList.FieldworksList.HasField(KMPoint(X,Y)) = ft_Road)
         OR (gTerrain.Land[Y, X].TileLock = tlRoadWork)
         OR (gTerrain.Land[Y, X].Passability * [tpMakeRoads, tpWalkRoad] <> [])
       ) then
      Exit;
  end;

  Result := True;
end;


function TKMEye.GetMineLocs(aHT: THouseType): TKMPointTagList;
var
  I: Integer;
  Mines: TKMPointList;
  Output: TKMPointTagList;
begin
  Output := TKMPointTagList.Create();
  case aHT of
    ht_GoldMine: Mines := fGoldMines;
    ht_IronMine: Mines := fIronMines;
    else
      Exit;
  end;

  for I := Mines.Count - 1 downto 0 do
    if (gAIFields.Influences.Ownership[fOwner, Mines.Items[I].Y, Mines.Items[I].X] > 0) then
      if CanAddHousePlan(Mines.Items[I], aHT, True, False) AND CheckResourcesNearMine(Mines.Items[I], aHT) then
        Output.Add(Mines.Items[I], gAIFields.Influences.Ownership[fOwner, Mines.Items[I].Y, Mines.Items[I].X])
      else
        Mines.Delete(I);
  Result := Output;
end;


function TKMEye.GetStoneLocs(): TKMPointTagList;
const
  SCAN_LIMIT = 10;
var
  X,Y,I, MaxDist: Integer;
  Output: TKMPointTagList;
begin
  Output := TKMPointTagList.Create();
  for I := fStoneMiningTiles.Count-1 downto 0 do
  begin
    X := fStoneMiningTiles.Items[I].X;
    Y := fStoneMiningTiles.Items[I].Y;
    MaxDist := Max(1, Y-SCAN_LIMIT);
    // Find actual stone tile (if exist)
    while (gTerrain.TileIsStone(X, Y) = 0) AND (Y > MaxDist) do
      Y := Y - 1;
    // Check if is possible to mine it
    if (gTerrain.TileIsStone(X, Y) > 0)
       AND (tpWalk in gTerrain.Land[Y+1,X].Passability) then
    begin
      // Save tile as a potential point for quarry
      if (gAIFields.Influences.Ownership[fOwner, Y+1, X] > 0) then
      begin
        fStoneMiningTiles.Items[I] := KMPoint(X,Y);
        Output.Add(fStoneMiningTiles.Items[I], gAIFields.Influences.Ownership[fOwner, Y+1, X]);
      end;
    end
    else
      // Else remove point
      fStoneMiningTiles.Delete(I);
  end;
  Result := Output;
end;



// Cluster algorithm (inspired by DBSCAN but clusters may overlap)
function TKMEye.GetForests(): TKMPointTagList;
const
  OWNERSHIP_LIMIT = 100;
  RANDOM_POINTS_OWNERSHIP_LIMIT = 150;
  RADIUS = 5;
  MAX_DIST = RADIUS+1; // When is max radius = 5 and max distance = 6 and use KMDistanceAbs it will give area similar to circle (without need to calculate euclidean distance!)
  VISIT_MARK = RADIUS+1;
  MIN_POINTS_CNT = 3;
  POLYGON_SCAN_RAD = 4;
  UNVISITED_TILE = 0;
  VISITED_TILE = 1;
  UNVISITED_TREE = 2;
  VISITED_TREE = 3;
  AVOID_BUILDING_FOREST_LIMIT = 250;
var
  Ownership: Byte;
  I,X,X2,Y,Y2, Distance, SparePointsCnt: Integer;
  Cnt: Single;
  Point, sumPoint, newPoint: TKMPoint;
  VisitArr: TKMByte2Array;
  Forests: TKMPointTagList;
  SparePoints: array[0..10] of Integer;
  Polygons: TPolygonArray;
begin
  SetLength(VisitArr, gTerrain.MapY, gTerrain.MapX);
  for Y := 1 to gTerrain.MapY - 1 do
  for X := 1 to gTerrain.MapX - 1 do
    VisitArr[Y,X] := UNVISITED_TILE;

  // Scan influence on 255*255 tiles takes performance -> use polygons from NavMesh instead
  SparePointsCnt := 0;
  Polygons := gAIFields.NavMesh.Polygons;
  for I := 0 to Length(Polygons) - 1 do
  begin
    Ownership := gAIFields.Influences.OwnPoly[fOwner, I];
    if (Ownership > OWNERSHIP_LIMIT) AND (fOwner = gAIFields.Influences.GetBestOwner(I)) then
    begin
      Point := Polygons[I].CenterPoint;
      for Y := Max(1, Point.Y - POLYGON_SCAN_RAD) to Min(gTerrain.MapY - 1, Point.Y + POLYGON_SCAN_RAD) do
      for X := Max(1, Point.X - POLYGON_SCAN_RAD) to Min(gTerrain.MapX - 1, Point.X + POLYGON_SCAN_RAD) do
      begin
        if (VisitArr[Y,X] = UNVISITED_TILE) then
        begin
          if (gAIFields.Influences.AvoidBuilding[Y,X] < 250) AND (gTerrain.ObjectIsChopableTree(X, Y)) then
            VisitArr[Y,X] := UNVISITED_TREE
          else
            VisitArr[Y,X] := VISITED_TILE;
        end;
      end;
      // In case that there is no forest save those points which should be in "ideal" distance from city to start forest
      if (Ownership < RANDOM_POINTS_OWNERSHIP_LIMIT) AND (SparePointsCnt < High(SparePoints)) then
      begin
        SparePoints[SparePointsCnt] := I;
        SparePointsCnt := SparePointsCnt + 1;
      end;
    end;
  end;

  Forests := TKMPointTagList.Create();
  for Y := 1 to gTerrain.MapY - 1 do
  for X := 1 to gTerrain.MapX - 1 do
    if (VisitArr[Y,X] = UNVISITED_TREE) then
    begin
      Point := KMPoint(X,Y);
      sumPoint := KMPOINT_ZERO;
      Cnt := 0;
      // It is faster to try find points in required radius than find closest points from list of points (when is radius small)
      for Y2 := Max(1, Y-RADIUS) to Min(Y+RADIUS, gTerrain.MapY-1) do
      for X2 := Max(1, X-RADIUS) to Min(X+RADIUS, gTerrain.MapX-1) do
        if (VisitArr[Y2,X2] >= UNVISITED_TREE) then
        begin
          newPoint := KMPoint(X2,Y2);
          Distance := KMDistanceAbs(Point, newPoint);
          if (Distance < MAX_DIST) then
          begin
            Cnt := Cnt + 1;
            sumPoint := KMPointAdd(sumPoint, newPoint);
            if (Distance < VISIT_MARK) then
              VisitArr[Y2,X2] := VISITED_TREE;
          end;
        end;
      if (Cnt > MIN_POINTS_CNT) then
      begin
        Point := KMPoint( Round(sumPoint.X/Cnt), Round(sumPoint.Y/Cnt) );
        Forests.Add( Point, Round(Cnt) );
        Forests.Tag2[Forests.Count-1] := gAIFields.Influences.Ownership[fOwner, Point.Y, Point.X];
      end;
    end;

  if (Forests.Count < 10) then
    for I := 0 to SparePointsCnt - 1 do
    begin
      X := SparePoints[I];
      Forests.Add( Polygons[X].CenterPoint, 0 );
      Forests.Tag2[Forests.Count-1] := gAIFields.Influences.OwnPoly[fOwner, X];
    end;

  Result := Forests;
end;


function TKMEye.GetCoalLocs(): TKMPointTagList;
var
  I,Own: Integer;
  Loc: TKMPoint;
  Output: TKMPointTagList;
begin
  Output := TKMPointTagList.Create();
  for I := fCoalMiningTiles.Count - 1 downto 0 do
  begin
    Loc := fCoalMiningTiles.Items[I];
    Own := gAIFields.Influences.Ownership[fOwner, Loc.Y, Loc.X];
    if (Own = 0) then
      continue;
    if (tpBuild in gTerrain.Land[Loc.Y,Loc.X].Passability) then
      Output.Add(Loc, Own)
    else
      fCoalMiningTiles.Delete(I);
  end;
  Result := Output;
end;


function TKMEye.GetClosestTrees(aLoc: TKMPoint): TKMPointTagList;
begin
  Result := nil;
end;


function TKMEye.GetCityCenterPoints(aMultiplePoints: Boolean = False): TKMPointArray;
const
  SCANNED_HOUSES = [ht_Store, ht_School, ht_Barracks];
var
  I, Cnt: Integer;
  HT: THouseType;
  H: TKMHouse;
begin
  // Find required house cnt
  Cnt := 0;
  for HT in SCANNED_HOUSES do
    Cnt := Cnt + gHands[fOwner].Stats.GetHouseQty(HT);
  SetLength(Result, 1 + (Cnt-1) * Byte(aMultiplePoints));
  // Exit if we have 0 houses
  if (Cnt = 0) then
    Exit;

  Cnt := 0;
  for I := 0 to gHands[fOwner].Houses.Count - 1 do
  begin
    H := gHands[fOwner].Houses[I];
    if (H.HouseType in SCANNED_HOUSES) AND not H.IsDestroyed AND H.IsComplete then
    begin
      Result[Cnt] := KMPointBelow(H.Entrance);
      Cnt := Cnt + 1;
      if (Length(Result) <= Cnt) then // in case of not aMultiplePoints
        Exit;
    end;
  end;
  SetLength(Result, Cnt); // Just to be sure ...
end;


function TKMEye.GetHouse(aHT: THouseType; var aLoc: TKMPoint): Boolean;
begin
  Result := True;
end;



procedure TKMEye.Paint(aRect: TKMRect);
const
  COLOR_WHITE = $80FFFFFF;
  COLOR_BLACK = $80000000;
  COLOR_GREEN = $CC00FF00;
  COLOR_RED = $800000FF;
  COLOR_YELLOW = $8000FFFF;
  COLOR_GREEN_Field = $4400FF00;
  COLOR_GREEN_Wine = $3355FFFF;
  COLOR_BLUE = $80FF0000;
var
  I: Integer;
  Point: TKMPoint;
begin
  {
  for I := 0 to fCoalMiningTiles.Count - 1 do
  begin
    Point := fCoalMiningTiles.Items[I];
    gRenderAux.Quad(Point.X, Point.Y, COLOR_BLACK);
  end;
  for I := 0 to fStoneMiningTiles.Count - 1 do
  begin
    Point := fStoneMiningTiles.Items[I];
    gRenderAux.Quad(Point.X, Point.Y, COLOR_RED);
  end;
  //}
end;



{ TKMSearchResource }
constructor TKMSearchResource.Create(aMinLimit, aMaxLimit: TKMPoint; var aVisitArr: TKMByte2Array; const aScanEightTiles: Boolean = False);
begin
  inherited Create(aScanEightTiles);
  fVisitArr := aVisitArr;
  fMinLimit := aMinLimit;
  fMaxLimit := aMaxLimit;
end;

function TKMSearchResource.CanBeVisited(const aX,aY: SmallInt): Boolean;
begin
  Result := (fVisitArr[aY,aX] = 0);
  case fSearch of
    1: Result := Result AND (gTerrain.TileIsCoal(aX, aY) > 0); // Coal
    2: Result := Result AND (gTerrain.TileIsStone(aX, aY) > 0); // Stone
    else
      begin
      end;
  end;
end;

function TKMSearchResource.IsVisited(const aX,aY: SmallInt): Boolean;
begin
  Result := (fVisitArr[aY,aX] > 0);
end;

procedure TKMSearchResource.MarkAsVisited(const aX,aY: SmallInt);
begin
  fVisitArr[aY,aX] := fSearch;
  if ((fSearch = 1) AND gAIFields.Eye.CanAddHousePlan(KMPoint(aX,aY), ht_CoalMine, True, True))
    OR (  (fSearch = 2) AND (aY < fMaxLimit.Y) AND (tpWalk in gTerrain.Land[aY+1,aX].Passability)  ) then
    fListOfPoints.Add(KMPoint(aX,aY));
end;

procedure TKMSearchResource.QuickFlood(aX,aY,aSearch: SmallInt; var aListOfPoints: TKMPointList);
begin
  fSearch := aSearch;
  fListOfPoints := aListOfPoints;
  inherited QuickFlood(aX,aY);
end;

end.
