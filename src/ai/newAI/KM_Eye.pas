unit KM_Eye;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, KromUtils, Math, SysUtils,
  KM_Defaults, KM_Points, KM_CommonClasses, KM_CommonTypes, KM_CommonUtils,
  KM_ResHouses, KM_Houses, KM_ResWares, KM_Units,
  KM_AIArmyEvaluation, KM_AIInfluences, KM_FloodFill, KM_NavMeshFloodFill;

const
  MAX_SCAN_DIST_FROM_HOUSE = 10;
  MIN_SCAN_DIST_FROM_HOUSE = 2; // Houses must have at least 1 tile of space between them

var
  GA_EYE_GetForests_SPRndOwnLimMin : Single = 219.373;
  GA_EYE_GetForests_SPRndOwnLimMax : Single = 7.841;
  GA_EYE_GetForests_InflLimit      : Single = 202.809;
  GA_EYE_GetForests_MinTrees       : Single = 3.11;
  GA_EYE_GetForests_Radius         : Single = 5.0698;
  GA_EYE_GetForests_MinRndSoil     : Single = 15.969; //0<->25


type
  TDirection = (dirN,dirE,dirS,dirW);
  THouseMapping = record // Record of vectors from loc of house to specific point
    Tiles: TKMPointArray; // Tiles inside house plan
    Surroundings: array[1..MAX_SCAN_DIST_FROM_HOUSE] of array[TDirection] of TKMPointArray; // Tiles around house plan in specific distance and direction
    MoveToEntrance: array[TDirection] of TKMPoint; // Move entrance of house in dependence of direction to be able to place house plan
    // Note: if we want place houses close to each other without "try and see" method we have to move from Loc of exist house into surrounding tiles and then move by entrance offset of new house)
  end;
  THouseMappingArray = array [HOUSE_MIN..HOUSE_MAX] of THouseMapping;


  TKMBuildState = (bsNoBuild = 0, bsHousePlan = 1, bsFieldPlan = 2, bsRoadPlan = 3, bsRoad = 4, bsBuild = 9, bsTree = 10, bsForest = 11, bsCoal = 12, bsDebug = 200, bsReserved = 255);
  TKMBuildInfo = record
    Visited, VisitedHouse: Byte;
    State: TKMBuildState;
    Next,Distance,DistanceInitPoint,Y: Word;
  end;

  TKMHouseRequirements = record
    HouseType: TKMHouseType;
    IgnoreTrees,IgnoreAvoidBuilding: Boolean;
    MaxCnt, MaxDist: Word;
  end;


  TKMBuildInfoArray = array of TKMBuildInfo;

  // This class transforms all house placing requirements into 1 array to avoid overlapping conditions
  // Placing new house is then question of internal house tiles
  TKMBuildFF = class
  private
    fOwner: TKMHandIndex;
    fOwnerUpdateInfo: TKMByteArray;
    fVisitIdx, fVisitIdxHouse: Byte;
    fStartQueue, fEndQueue, fQueueCnt, fMapX, fMapY: Word;
    fUpdateTick: Cardinal;
    fHouseReq: TKMHouseRequirements;
    fHMA: THouseMappingArray;
    fLocs: TKMPointList;
    fInfoArr: TKMBuildInfoArray;

//    function GetInfo(const aY,aX: Word): TKMBuildInfo;
//    procedure SetInfo(const aY,aX,aNext,aDistance: Word; const aVisited: Byte; const aState: TKMBuildState);
    function GetVisited(const aY,aX: Word): Byte;
    procedure SetVisited(const aY,aX: Word; const aVisited: Byte);
    function GetOwnersIndex(const aOwner: TKMHandIndex): Byte;
    function GetState(const aY,aX: Word): TKMBuildState;
    procedure SetState(const aY,aX: Word; const aState: TKMBuildState);
    function GetStateFromIdx(const aIdx: Word): TKMBuildState;
    function GetDistance(const aPoint: TKMPoint): Word;
    function GetDistanceInitPoint(const aPoint: TKMPoint): Word;
    function GetNext(const aY,aX: Word): Word;
    procedure SetNext(const aY,aX,aNext: Word);

  protected
    procedure InitQueue(aHouseFF: Boolean);
    function InsertInQueue(const aIdx: Word): Word;
    function RemoveFromQueue(var aX,aY,aIdx: Word): Boolean;

    function CanBeVisited(const aX,aY,aIdx: Word; const aHouseQueue: Boolean = False): Boolean; overload;
    procedure MarkAsVisited(const aY,aIdx,aDistance: Word; const aState: TKMBuildState); overload;
    procedure MarkAsVisited(const aX,aY,aIdx,aDistance: Word); overload;

    function GetTerrainState(const aX,aY: Word): TKMBuildState;
    procedure MarkPlans();
    procedure TerrainFF();
    procedure HouseFF();
  public
    constructor Create();
    destructor Destroy(); override;

    //property Info[const aY,aX: Word]: TKMBuildInfo read GetInfo write SetInfo;
    property VisitIdx: Byte read fVisitIdx;
    property VisitIdxHouse: Byte read fVisitIdxHouse;
    property Visited[const aY,aX: Word]: Byte read GetVisited write SetVisited;
    property VisitIdxOwner[const aOwner: TKMHandIndex]: Byte read GetOwnersIndex;
    property State[const aY,aX: Word]: TKMBuildState read GetState write SetState;
    property StateIdx[const aIdx: Word]: TKMBuildState read GetStateFromIdx;
    property Distance[const aPoint: TKMPoint]: Word read GetDistance;
    property DistanceInitPoint[const aPoint: TKMPoint]: Word read GetDistanceInitPoint;
    property Next[const aY,aX: Word]: Word read GetNext write SetNext;
    property Locs: TKMPointList read fLocs write fLocs;
    property HouseRequirements: TKMHouseRequirements read fHouseReq write fHouseReq;

    procedure UpdateState();
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    procedure ActualizeTile(aX, aY: Word);
    function CanBePlacedHouse(const aLoc: TKMPoint): Boolean;
    procedure FindPlaceForHouse(aHouseReq: TKMHouseRequirements; InitPointsArr: TKMPointArray; aClearHouseList: Boolean = True);
  end;

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


  TKMFFInitPlace = class(TNavMeshFloodFill)
  private
  protected
    fPolygonArr: TKMByteArray;
    function CanBeExpanded(const aIdx: Word): Boolean; override;
    procedure MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint); override;
  public
    function FillPolygons(const aMaxIdx: Word; aInitIdxArray: TKMWordArray; var aPolygonArr: TKMByteArray): Boolean; reintroduce;
  end;


  // Transform game data into "AI view" ... this is how AI see the map (influences have its own class)
  TKMEye = class
  private
    fOwner: TKMHandIndex;
    fHousesMapping: THouseMappingArray;
    fGoldMines, fIronMines, fStoneMiningTiles: TKMPointList;
    fCoalPolygons, fPolygonRoutes: TKMByteArray;  // fFertility

    fBuildFF: TKMBuildFF;
    fArmyEvaluation: TKMArmyEvaluation;

    procedure InitHousesMapping();
    function CheckResourcesNearMine(aLoc: TKMPoint; aHT: TKMHouseType): Boolean;
//    function GetResourcesNearMine(aLoc: TKMPoint; aHT: TKMHouseType): Word;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property HousesMapping: THouseMappingArray read fHousesMapping write fHousesMapping;
    property BuildFF: TKMBuildFF read fBuildFF;
    property ArmyEvaluation: TKMArmyEvaluation read fArmyEvaluation;
    property PolygonRoutes: TKMByteArray read fPolygonRoutes;

    procedure AfterMissionInit();
    procedure UpdateState(aTick: Cardinal);
    procedure ScanLocResources(out aGoldMineCnt, aIronMineCnt, aFieldCnt, aBuildCnt: Integer);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);

    function CanPlaceHouse(aLoc: TKMPoint; aHT: TKMHouseType; aIgnoreTrees: Boolean = False): Boolean;
    function CanAddHousePlan(aLoc: TKMPoint; aHT: TKMHouseType; aIgnoreAvoidBuilding: Boolean = False; aIgnoreTrees: Boolean = False; aIgnoreLocks: Boolean = True): Boolean;

    function GetMineLocs(aHT: TKMHouseType): TKMPointTagList;
    function GetStoneLocs(aOnlyMainOwnership: Boolean = False): TKMPointTagList;
    function GetCoalMineLocs(aOnlyMainOwnership: Boolean = False): TKMPointTagList;
    procedure GetForests(var aForests: TKMPointTagList);
    function GetCityCenterPoints(aMultiplePoints: Boolean = False): TKMPointArray;

    function GetClosestUnitAroundHouse(aHT: TKMHouseType; aLoc: TKMPoint; aInitPoint: TKMPoint): TKMUnit;

    procedure Paint(aRect: TKMRect);
  end;



implementation
uses
  KM_Game, KM_Terrain, KM_Hand, KM_Resource, KM_AIFields, KM_HandsCollection, KM_RenderAux, KM_ResMapElements,
  KM_NavMesh, KM_CityPlanner;


{ TKMEye }
constructor TKMEye.Create();
begin
  fGoldMines := TKMPointList.Create();
  fIronMines := TKMPointList.Create();
  fStoneMiningTiles := TKMPointList.Create();

  fBuildFF := TKMBuildFF.Create();
  fArmyEvaluation := TKMArmyEvaluation.Create();

  InitHousesMapping();
end;

destructor TKMEye.Destroy();
begin
  fGoldMines.Free;
  fIronMines.Free;
  fStoneMiningTiles.Free;

  fBuildFF.Free;
  fArmyEvaluation.Free;

  inherited;
end;


procedure TKMEye.Save(SaveStream: TKMemoryStream);
  procedure SaveByteArr(var aArray: TKMByteArray);
  var
    Len: Integer;
  begin
    Len := Length(aArray);
    SaveStream.Write(Len);
    SaveStream.Write(aArray[0], SizeOf(aArray[0]) * Len);
  end;
begin
  SaveStream.WriteA('Eye');
  SaveStream.Write(fOwner);

  fGoldMines.SaveToStream(SaveStream);
  fIronMines.SaveToStream(SaveStream);
  fStoneMiningTiles.SaveToStream(SaveStream);

  SaveByteArr(fPolygonRoutes);
  SaveByteArr(fCoalPolygons);

  fArmyEvaluation.Save(SaveStream);

  // The following does not requires save
  // fHousesMapping
end;

procedure TKMEye.Load(LoadStream: TKMemoryStream);
  procedure LoadByteArr(var aArray: TKMByteArray);
  var
    Len: Integer;
  begin
    LoadStream.Read(Len);
    SetLength(aArray, Len);
    LoadStream.Read(aArray[0], SizeOf(aArray[0]) * Len);
  end;
begin
  LoadStream.ReadAssert('Eye');
  LoadStream.Read(fOwner);

  fGoldMines.LoadFromStream(LoadStream);
  fIronMines.LoadFromStream(LoadStream);
  fStoneMiningTiles.LoadFromStream(LoadStream);

  LoadByteArr(fPolygonRoutes);
  LoadByteArr(fCoalPolygons);

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
  SetLength(fPolygonRoutes, Length(gAIFields.NavMesh.Polygons));
  SetLength(fCoalPolygons, Length(gAIFields.NavMesh.Polygons));
  for X := 0 to Length(fPolygonRoutes) - 1 do
  begin
    fCoalPolygons[X] := 0;
    fPolygonRoutes[X] := 0;
  end;

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
        if CanAddHousePlan(Loc, htGoldMine, True, False) then
        begin
          if CheckResourcesNearMine(Loc, htGoldMine) then
            fGoldMines.Add(Loc);
        end
        else if CanAddHousePlan(Loc, htIronMine, True, False) then
        begin
          if CheckResourcesNearMine(Loc, htIronMine) then
            fIronMines.Add(Loc);
        end
        else if (gTerrain.TileIsCoal(X, Y) > 1) then
        begin
          Inc(fCoalPolygons[  gAIFields.NavMesh.KMPoint2Polygon[ Loc ]  ]);
        end
        else if gTerrain.TileHasStone(X, Y) then
        begin
          if (Y < gTerrain.MapY - 1) AND (tpWalk in gTerrain.Land[Y+1,X].Passability) then
            fStoneMiningTiles.Add(KMPoint(X,Y));
        end;
      end;
  finally
    SearchResource.Free();
  end;
end;


// Search for ore - gold and iron mines have similar requirements so there are booth in 1 method
function TKMEye.CheckResourcesNearMine(aLoc: TKMPoint; aHT: TKMHouseType): Boolean;
var
  X,Y: Integer;
begin
  Result := True;
  for X := Max(aLoc.X-4, 1) to Min(aLoc.X+3+Byte(aHT = htGoldMine), gTerrain.MapX-1) do
    for Y := Max(aLoc.Y-8, 1) to aLoc.Y do
      if   (aHT = htGoldMine) AND gTerrain.TileHasGold(X, Y)
        OR (aHT = htIronMine) AND gTerrain.TileHasIron(X, Y) then
        Exit;
  Result := False; //Didn't find any ore
end;


//function TKMEye.GetResourcesNearMine(aLoc: TKMPoint; aHT: TKMHouseType): Word;
//var
//  X,Y: Integer;
//begin
//  Result := 0;
//  for X := Max(aLoc.X-4, 1) to Min(aLoc.X+3+Byte(aHT = htGoldMine), gTerrain.MapX-1) do
//    for Y := Max(aLoc.Y-8, 1) to aLoc.Y do
//      if (aHT = htGoldMine) then
//        Result := Result + gTerrain.TileIsGold(X, Y)
//      else if (aHT = htIronMine) then
//        Result := Result + gTerrain.TileIsIron(X, Y);
//end;


procedure TKMEye.ScanLocResources(out aGoldMineCnt, aIronMineCnt, aFieldCnt, aBuildCnt: Integer);
var
  CenterPolygon, PolygonsCnt: Integer;
  InitPolygons: TKMWordArray;

  procedure ScanLocArea(aStartPoint: TKMPoint);
  var
    Distance, StartPolygon: Word;
    I: Integer;
    PolygonRoute: TKMWordArray;
  begin
    if (CenterPolygon = High(Word)) then
      Exit;
    Distance := 0;
    StartPolygon := gAIFields.NavMesh.KMPoint2Polygon[ aStartPoint ];
    SetLength(PolygonRoute, 0);
    if gAIFields.NavMesh.Pathfinding.ShortestPolygonRoute(StartPolygon, CenterPolygon, Distance, PolygonRoute) then
    begin
      if (Length(PolygonRoute) + PolygonsCnt >= Length(InitPolygons)) then
        SetLength(InitPolygons, Length(PolygonRoute) + PolygonsCnt + 64);
      for I := 0 to Length(PolygonRoute) - 1 do
      begin
        InitPolygons[PolygonsCnt] := PolygonRoute[I];
        //fPolygonRoutes[ PolygonRoute[I] ] := 255;
        //fPolygonRoutes[ PolygonRoute[I] ] := Min(250, fPolygonRoutes[ PolygonRoute[I] ] + 100);
        PolygonsCnt := PolygonsCnt + 1;
      end;
    end;
  end;

  function FindSeparateMines(aMineType: TKMHouseType; var MineList: TKMPointList): Word;
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
            // Check if this mine is overlapping with already counted
            if (MineList.Items[K].Y <> MineList.Items[I].Y)
              OR (  Abs(MineList.Items[K].X - MineList.Items[I].X) > (3 + Byte(aMineType = htIronMine)) ) then
              continue
            else
            begin
              Output := Output - 1;
              InfluenceArr[I] := False;
              break;
            end;
          end;
        if InfluenceArr[I] then
          ScanLocArea( MineList.Items[I] );
       end;
    Result := Output;
  end;
var
  I,K, Increment: Integer;
  Loc: TKMPoint;
  CenterPointArr: TKMPointArray;
  TagList: TKMPointTagList;
  FFInitPlace: TKMFFInitPlace;
begin
  PolygonsCnt := 0;
  // Init city center point (storehouse / school etc.)
  CenterPointArr := GetCityCenterPoints(False);
  CenterPolygon := High(Word);
  if (Length(CenterPointArr) > 0) then
    CenterPolygon := gAIFields.NavMesh.KMPoint2Polygon[ CenterPointArr[0] ];
  // Scan Resources - gold, iron
  aIronMineCnt := FindSeparateMines(htIronMine, fIronMines);
  aGoldMineCnt := FindSeparateMines(htGoldMine, fGoldMines);
  // Scan Resources - stones
  {
  TagList := GetStoneLocs(True);
  try
    I := 0;
    Increment := Ceil(TagList.Count / 5.0); // Max 5 paths
    while (I < TagList.Count) do
    begin
      ScanLocArea(TagList.Items[I]);
      I := I + Increment;
    end;
  finally
    TagList.Free;
  end;
  //}
  // Scan Resources - coal
  TagList := GetCoalMineLocs(True);
  try
    I := 0;
    Increment := Ceil(TagList.Count / 10.0); // Max 10 paths
    while (I < TagList.Count) do
    begin
      ScanLocArea(TagList.Items[I]);
      I := I + Increment;
    end;
  finally
    TagList.Free;
  end;

  aFieldCnt := 0;
  aBuildCnt := 0;
  for I := 0 to Length(gAIFields.NavMesh.Polygons) - 1 do
    if (gAIFields.Influences.GetBestOwner(I) = fOwner) then
      for K := gAIFields.NavMesh.Polygons[I].Poly2PointStart to gAIFields.NavMesh.Polygons[I].Poly2PointCnt - 1 do
      begin
        Loc := gAIFields.NavMesh.Polygon2Point[K];
        if gTerrain.CheckHeightPass(Loc, hpBuilding) then // The strictest condition first
        begin
          if gTerrain.TileIsRoadable( Loc ) then // Roadable tile + height pass + other conditions = tpbuild
            aBuildCnt := aBuildCnt + 1;
          if gTerrain.TileIsSoil( Loc ) then // Scan just tile + height (ignore object and everything else)
            aFieldCnt := aFieldCnt + 1;
        end
        else if gTerrain.CheckHeightPass(Loc, hpWalking)
          AND gTerrain.TileIsSoil( Loc ) then // Scan just tile + height (ignore object and everything else)
          aFieldCnt := aFieldCnt + 1;
      end;

  if (PolygonsCnt > 0) then
  begin
    FFInitPlace := TKMFFInitPlace.Create;
    try
      FFInitPlace.FillPolygons(PolygonsCnt-1, InitPolygons, fPolygonRoutes);
    finally
      FFInitPlace.Free;
    end;
  end;
end;


// Create mapping of surrounding tiles for each house
procedure TKMEye.InitHousesMapping();
var
  EnterOff: ShortInt;
  House: TKMHouseType;
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


procedure TKMEye.UpdateState(aTick: Cardinal);
begin
  fArmyEvaluation.UpdateState(aTick);
end;


procedure TKMEye.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
  fBuildFF.OwnerUpdate(aPlayer);
  //fFinder.OwnerUpdate(fOwner);
end;


// This function is copied (and reworked) from TKMTerrain.CanPlaceHouse and edited to be able to ignore trees
function TKMEye.CanPlaceHouse(aLoc: TKMPoint; aHT: TKMHouseType; aIgnoreTrees: Boolean = False): Boolean;
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
      htIronMine: Output := Output AND gTerrain.CanPlaceIronMine(X, Y);
      htGoldMine: Output := Output AND gTerrain.CanPlaceGoldMine(X, Y);
      else         Output := Output AND ( (tpBuild in gTerrain.Land[Y,X].Passability)
                                          OR (aIgnoreTrees
                                              AND gTerrain.ObjectIsChopableTree(KMPoint(X,Y), [caAge1,caAge2,caAge3,caAgeFull])
                                              AND gHands[fOwner].CanAddFieldPlan(KMPoint(X,Y), ftWine))
                                        );
    end;
    if not Output then
      break;
  end;
  Result := Output;
end;


// Modified version of TKMHand.CanAddHousePlan - added possibilities
// aIgnoreAvoidBuilding = ignore avoid building areas
// aIgnoreTrees = ignore trees inside of house plan
// aIgnoreLocks = ignore existing house plans inside of house plan tiles
function TKMEye.CanAddHousePlan(aLoc: TKMPoint; aHT: TKMHouseType; aIgnoreAvoidBuilding: Boolean = False; aIgnoreTrees: Boolean = False; aIgnoreLocks: Boolean = True): Boolean;
  function CanBeRoad(X,Y: Integer): Boolean;
  begin
    Result := (gTerrain.Land[Y, X].Passability * [tpMakeRoads, tpWalkRoad] <> [])
              OR (gHands[fOwner].BuildList.FieldworksList.HasField(KMPoint(X,Y)) <> ftNone) // We dont need strictly road just make sure that it is possible to place something here (and replace it by road later)
              OR (gTerrain.Land[Y, X].TileLock in [tlFieldWork, tlRoadWork]);
  end;
var
  LeftSideFree, RightSideFree: Boolean;
  X, Y, I, K, PL: Integer;
  Point: TKMPoint;
  Dir: TDirection;
begin
  Result := False;

  // The loc is out of map or in inaccessible area
  if not gTerrain.TileInMapCoords(aLoc.X, aLoc.Y, 1) then
    //OR (gAIFields.Influences.AvoidBuilding[aLoc.Y,aLoc.X] = AVOID_BUILDING_INACCESSIBLE_TILES) then // Use only with old placing algorithm without flood fill
    Exit;

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
    Point := KMPoint(X,Y);

    // Check with AvoidBuilding array to secure that new house will not be build in forests / coal tiles
    if aIgnoreAvoidBuilding then
    begin
      if not aIgnoreLocks AND
        (gAIFields.Influences.AvoidBuilding[Y, X] in [AVOID_BUILDING_HOUSE_OUTSIDE_LOCK, AVOID_BUILDING_HOUSE_INSIDE_LOCK]) then
      Exit;
    end
    else if (gAIFields.Influences.AvoidBuilding[Y, X] > 0) then
      Exit;

    //This tile must not contain fields/houseplans of allied players
    for PL := 0 to gHands.Count - 1 do
      if (gHands[fOwner].Alliances[PL] = at_Ally) then// AND (PL <> fOwner) then
        if (gHands[PL].BuildList.FieldworksList.HasField(Point) <> ftNone) then
          Exit;
  end;

  // Scan tiles in distance 1 from house plan
  LeftSideFree := True;
  RightSideFree := True;
  I := 1;
  for Dir := Low(fHousesMapping[aHT].Surroundings[I]) to High(fHousesMapping[aHT].Surroundings[I]) do
  for K := Low(fHousesMapping[aHT].Surroundings[I,Dir]) to High(fHousesMapping[aHT].Surroundings[I,Dir]) do
  begin
    X := aLoc.X + fHousesMapping[aHT].Surroundings[I,Dir,K].X;
    Y := aLoc.Y + fHousesMapping[aHT].Surroundings[I,Dir,K].Y;
    Point := KMPoint(X,Y);
    // Surrounding tiles must not be a house
    for PL := 0 to gHands.Count - 1 do
      if (gHands[fOwner].Alliances[PL] = at_Ally) then
        if gHands[PL].BuildList.HousePlanList.HasPlan(Point) then
          Exit;
    if (aHT in [htGoldMine, htIronMine]) then
      continue;
    // Make sure we can add road below house;
    if (Dir = dirS) AND not CanBeRoad(X,Y) then // Direction south
      Exit;
    // Quarry / Woodcutters / CoalMine / Towers may take place for mine so its arena must be scaned completely
    if aIgnoreAvoidBuilding then
    begin
      if (gAIFields.Influences.AvoidBuilding[Y, X] = AVOID_BUILDING_HOUSE_INSIDE_LOCK) then
        Exit;
    end
    // For "normal" houses there must be at least 1 side also free (on the left or right from house plan)
    else if (Dir = dirE) then // Direction east
      RightSideFree := RightSideFree AND CanBeRoad(X,Y)
    else if (Dir = dirW) then // Direction west
      LeftSideFree := LeftSideFree AND CanBeRoad(X,Y);
    if not (LeftSideFree AND RightSideFree) then
      Exit;
  end;

  Result := True;
end;


function TKMEye.GetMineLocs(aHT: TKMHouseType): TKMPointTagList;
var
  I: Integer;
  Mines: TKMPointList;
  Output: TKMPointTagList;
begin
  Output := TKMPointTagList.Create();
  Mines := nil;
  case aHT of
    htGoldMine: Mines := fGoldMines;
    htIronMine: Mines := fIronMines;
  end;

  if (Mines <> nil) then
    for I := Mines.Count - 1 downto 0 do
      if (gAIFields.Influences.Ownership[fOwner, Mines.Items[I].Y, Mines.Items[I].X] > 0) then
        if CanAddHousePlan(Mines.Items[I], aHT, True, False) AND CheckResourcesNearMine(Mines.Items[I], aHT) then
          Output.Add(Mines.Items[I], gAIFields.Influences.Ownership[fOwner, Mines.Items[I].Y, Mines.Items[I].X])
        else
          Mines.Delete(I);
  Result := Output;
end;


function TKMEye.GetStoneLocs(aOnlyMainOwnership: Boolean = False): TKMPointTagList;
const
  SCAN_LIMIT = 10;
var
  Ownership: Byte;
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
    while not gTerrain.TileHasStone(X, Y) AND (Y > MaxDist) do
      Y := Y - 1;
    // Check if is possible to mine it
    if gTerrain.TileHasStone(X, Y)
       AND (tpWalk in gTerrain.Land[Y+1,X].Passability) then
    begin
      // Save tile as a potential point for quarry
      Ownership := gAIFields.Influences.Ownership[fOwner, Y+1, X];
      if (aOnlyMainOwnership AND (gAIFields.Influences.GetBestOwner(X, Y+1) = fOwner))
         OR (not aOnlyMainOwnership AND (Ownership > 0)) then
      begin
        fStoneMiningTiles.Items[I] := KMPoint(X,Y);
        Output.Add(fStoneMiningTiles.Items[I], Ownership);
      end;
    end
    else
      // Else remove point
      fStoneMiningTiles.Delete(I);
  end;
  Result := Output;
end;


function TKMEye.GetCoalMineLocs(aOnlyMainOwnership: Boolean = False): TKMPointTagList;
var
  Count: Word;
  CoalPolygons, PolygonEvaluation: TKMWordArray;

  procedure InsertionSort(aIdx,aBid: Word);
  var
    I: Integer;
  begin
    if (Length(CoalPolygons) >= Count) then
    begin
      SetLength(CoalPolygons, Count + 64);
      SetLength(PolygonEvaluation, Count + 64);
      for I := Count to High(PolygonEvaluation) do
        PolygonEvaluation[I] := High(Word);
    end;
    Count := Count + 1;
    I := Count - 2;
    while (I >= 0) do
    begin
      if (PolygonEvaluation[I] > aBid) then
      begin
        PolygonEvaluation[I+1] := PolygonEvaluation[I];
        CoalPolygons[I+1] := CoalPolygons[I];
      end
      else
        break;
      I := I - 1;
    end;
    PolygonEvaluation[I+1] := aBid;
    CoalPolygons[I+1] := aIdx;
  end;
const
  MAX_LOCS = 30;
  MAX_ENEMY_OWNERSHIP = 150;
var
  Dist: Word;
  I, K, Cnt, Idx: Integer;
  Loc: TKMPoint;
  HouseReq: TKMHouseRequirements;
  Output: TKMPointTagList;
begin
  Output := TKMPointTagList.Create();
  fBuildFF.UpdateState(); // Mark walkable area in owner's city
  Count := 0;
  for Idx := 0 to Length(fCoalPolygons) - 1 do
    if (fCoalPolygons[Idx] > 0) then
    begin
      if (aOnlyMainOwnership AND (gAIFields.Influences.GetBestOwner(Idx) <> fOwner)) then
        continue;
      if (gAIFields.Influences.GetBestAllianceOwnership(fOwner, Idx, at_Enemy) > MAX_ENEMY_OWNERSHIP) then
        continue;
      Loc := gAIFields.NavMesh.Polygons[Idx].CenterPoint;
      if (fBuildFF.VisitIdx <> fBuildFF.Visited[ Loc.Y, Loc.X ]) then
        continue;
      Dist := BuildFF.Distance[Loc];
      InsertionSort(Idx,Dist);
    end;

  with HouseReq do
  begin
    HouseType := htCoalMine;
    IgnoreTrees := False;
    IgnoreAvoidBuilding := True;
    MaxCnt := 0;
    MaxDist := 0;
  end;
  BuildFF.HouseRequirements := HouseReq;

  for I := 0 to Count - 1 do
  begin
    Idx := CoalPolygons[I];
    Cnt := 0;
    for K := gAIFields.NavMesh.Polygons[Idx].Poly2PointStart to gAIFields.NavMesh.Polygons[Idx].Poly2PointCnt - 1 do
    begin
      Loc := gAIFields.NavMesh.Polygon2Point[K];
      if gTerrain.TileHasCoal(Loc.X, Loc.Y) then // There are stored only coal tiles > 1 so here > 0 is ok because we can expect that surrouding tiles are also coal
      begin
        Cnt := Cnt + 1;
        if fBuildFF.CanBePlacedHouse(Loc) then // BuildFF can see more than CanAddHousePlan
          Output.Add(Loc, BuildFF.Distance[Loc]);
      end;
    end;
    fCoalPolygons[Idx] := Cnt;
    if (Output.Count > MAX_LOCS) then
      break;
  end;

  Result := Output;
end;



// Cluster algorithm (inspired by DBSCAN but clusters may overlap)
// Create possible places for forests and return count of already existed forests
procedure TKMEye.GetForests(var aForests: TKMPointTagList);
const
  //RADIUS = 5;
  //MAX_DIST = RADIUS+1; // When is max radius = 5 and max distance = 6 and use KMDistanceAbs it will give area similar to circle (without need to calculate euclidean distance!)

  UNVISITED_TILE = 0;
  VISITED_TILE = 1;
  UNVISITED_TREE = 2;
  UNVISITED_TREE_IN_FOREST = 3;
  VISITED_TREE = 4;
  VISITED_TREE_IN_FOREST = 5;
  MAX_SPARE_POINTS = 20;
var
  PartOfForest: Boolean;
  Ownership, AvoidBulding: Byte;
  RADIUS, MAX_DIST: Word;
  I,X,Y,X2,Y2, Distance, SparePointsCnt: Integer;
  Cnt: Single;
  Point, sumPoint: TKMPoint;
  VisitArr: TKMByte2Array;
  Polygons: TPolygonArray;
begin
  fBuildFF.UpdateState(); // Mark walkable area in owner's city
  aForests.Clear;
  Polygons := gAIFields.NavMesh.Polygons;

  RADIUS := Round(GA_EYE_GetForests_Radius);
  MAX_DIST := RADIUS + 1;

  // Init visit array and fill trees
  SetLength(VisitArr, gTerrain.MapY, gTerrain.MapX);
  for Y := 1 to gTerrain.MapY - 1 do
    for X := 1 to gTerrain.MapX - 1 do
    begin
      VisitArr[Y,X] := VISITED_TILE;
      if (BuildFF.VisitIdx = BuildFF.Visited[Y,X])
         AND (BuildFF.State[Y,X] = bsTree) then
      begin
        AvoidBulding := gAIFields.Influences.AvoidBuilding[Y,X];
        if (AvoidBulding < AVOID_BUILDING_FOREST_MINIMUM) then
          VisitArr[Y,X] := UNVISITED_TREE
        else if (AvoidBulding < GA_EYE_GetForests_InflLimit) then
          VisitArr[Y,X] := UNVISITED_TREE_IN_FOREST;
      end;
    end;

  for Y := 1 to gTerrain.MapY - 1 do
    for X := 1 to gTerrain.MapX - 1 do
      if ((VisitArr[Y,X] = UNVISITED_TREE) OR (VisitArr[Y,X] = UNVISITED_TREE_IN_FOREST))
         AND (fOwner = gAIFields.Influences.GetBestOwner(X,Y)) then
      begin
        Point := KMPoint(X,Y);
        PartOfForest := False;
        sumPoint := KMPOINT_ZERO;
        Cnt := 0;
        // It is faster to try find points in required radius than find closest points from list of points (when is radius small)
        for Y2 := Max(1, Point.Y-RADIUS) to Min(Point.Y+RADIUS, gTerrain.MapY-1) do
        for X2 := Max(1, Point.X-RADIUS) to Min(Point.X+RADIUS, gTerrain.MapX-1) do
          if (VisitArr[Y2,X2] >= UNVISITED_TREE) then // Detect tree
          begin
            Distance := KMDistanceAbs(Point, KMPoint(X2,Y2));
            if (Distance < MAX_DIST + 1) then // Check distance
            begin
              Cnt := Cnt + 1;
              sumPoint := KMPointAdd(sumPoint, KMPoint(X2,Y2));
              if (VisitArr[Y2,X2] = UNVISITED_TREE) then
                VisitArr[Y2,X2] := VISITED_TREE
              else if (VisitArr[Y2,X2] = UNVISITED_TREE_IN_FOREST) then
              begin
                PartOfForest := True;
                VisitArr[Y2,X2] := VISITED_TREE_IN_FOREST;
              end;
            end;
          end;
        if (Cnt > GA_EYE_GetForests_MinTrees) then
        begin
          Point := KMPoint( Round(sumPoint.X/Cnt), Round(sumPoint.Y/Cnt) );
          aForests.Add( Point, Cardinal(PartOfForest) );
          aForests.Tag2[aForests.Count-1] := Round(Cnt);
        end;
      end;

  // Try to find potential forests only in owner's influence areas
  SparePointsCnt := 0;
  for I := 0 to Length(Polygons) - 1 do
  begin
    Ownership := gAIFields.Influences.OwnPoly[fOwner, I];
    if (Ownership > GA_EYE_GetForests_SPRndOwnLimMin)
       AND (Ownership < GA_EYE_GetForests_SPRndOwnLimMax)
       AND (SparePointsCnt < MAX_SPARE_POINTS)
       AND (KaMRandom() > 0.7) then
    begin
      Point := Polygons[I].CenterPoint;
      Cnt := 0;
      for Y := Max(1,Point.Y-2) to Min(Point.Y+2,gTerrain.MapY-1) do
        for X := Max(1,Point.X-2) to Min(Point.X+2,gTerrain.MapX-1) do
          if gTerrain.TileIsSoil(X,Y) AND (BuildFF.State[Y,X] in [bsBuild, bsTree, bsForest]) then
            Cnt := Cnt + 1;
      if (Cnt > GA_EYE_GetForests_MinRndSoil) then
      begin
        aForests.Add( Point, 0 );
        aForests.Tag2[ aForests.Count-1 ] := 0;
        SparePointsCnt := SparePointsCnt + 1;
      end;
    end;
  end;
end;


function TKMEye.GetCityCenterPoints(aMultiplePoints: Boolean = False): TKMPointArray;
const
  SCANNED_HOUSES = [htStore, htSchool, htBarracks];
var
  I, Cnt: Integer;
  HT: TKMHouseType;
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
    if (H <> nil) AND not H.IsDestroyed AND H.IsComplete AND (H.HouseType in SCANNED_HOUSES) then
    begin
      Result[Cnt] := KMPointBelow(H.Entrance);
      Cnt := Cnt + 1;
      if (Length(Result) <= Cnt) then // in case of not aMultiplePoints
        Exit;
    end;
  end;
  SetLength(Result, Cnt); // Just to be sure ...
end;



function TKMEye.GetClosestUnitAroundHouse(aHT: TKMHouseType; aLoc: TKMPoint; aInitPoint: TKMPoint): TKMUnit;
const
  INIT_DIST = 10000;
var
  X, Y, I, Dist, Closest, Distance: Integer;
  Dir: TDirection;
  U: TKMUnit;
begin
  Result := nil;
  Closest := INIT_DIST;
  Dist := 1;
  for Dir := Low(fHousesMapping[aHT].Surroundings[Dist]) to High(fHousesMapping[aHT].Surroundings[Dist]) do
    for I := Low(fHousesMapping[aHT].Surroundings[Dist,Dir]) to High(fHousesMapping[aHT].Surroundings[Dist,Dir]) do
    begin
      Y := aLoc.Y + fHousesMapping[aHT].Surroundings[Dist,Dir,I].Y;
      X := aLoc.X + fHousesMapping[aHT].Surroundings[Dist,Dir,I].X;
      U := gTerrain.UnitsHitTest(X,Y);
      if (U <> nil)
       AND not U.IsDeadOrDying
       AND (U.Owner >= 0) // Dont select animals!
       AND (U.Owner <> fOwner)
       AND (gHands[fOwner].Alliances[U.Owner] <> at_Ally) then
      begin
        Distance := KMDistanceAbs(KMPoint(X,Y), aInitPoint);
        if (Closest > Distance) then
        begin
          Closest := Distance;
          Result := U;
        end;
      end;
    end;
end;



procedure TKMEye.Paint(aRect: TKMRect);
  procedure DrawTriangle(aIdx: Integer; aColor: Cardinal);
  var
    PolyArr: TPolygonArray;
    NodeArr: TNodeArray;
  begin
    PolyArr := gAIFields.NavMesh.Polygons;
    NodeArr := gAIFields.NavMesh.Nodes;
    gRenderAux.TriangleOnTerrain(
      NodeArr[ PolyArr[aIdx].Indices[0] ].Loc.X,
      NodeArr[ PolyArr[aIdx].Indices[0] ].Loc.Y,
      NodeArr[ PolyArr[aIdx].Indices[1] ].Loc.X,
      NodeArr[ PolyArr[aIdx].Indices[1] ].Loc.Y,
      NodeArr[ PolyArr[aIdx].Indices[2] ].Loc.X,
      NodeArr[ PolyArr[aIdx].Indices[2] ].Loc.Y, aColor);
  end;

const
  COLOR_WHITE = $FFFFFF;
  COLOR_BLACK = $000000;
  COLOR_GREEN = $00FF00;
  COLOR_RED = $8000FF;
  COLOR_YELLOW = $00FFFF;
  COLOR_BLUE = $FF0000;

  NO_BUILD = 0;
  HOUSE_PLAN = 1;
  FIELD_PLAN = 2;
  BUILD = 9;
  TREE = 10;
  FOREST = 11;
  COAL = 12;
  RESERVE = 255;
var
  PL: TKMHandIndex;
  I,X,Y: Integer;
begin
  if not OVERLAY_AI_EYE then
    Exit;
  //{ Build flood fill
  for PL := 0 to gHands.Count - 1 do
  begin
    OwnerUpdate(PL);
    fBuildFF.UpdateState();
  end;
  for Y := 1 to gTerrain.MapY - 1 do
    for X := 1 to gTerrain.MapX - 1 do
      //if (fBuildFF.Visited[Y,X] = fBuildFF.VisitIdxOwner[fOwner]) then
      //if (fBuildFF.Visited[Y,X] = fBuildFF.VisitIdx) then
        case fBuildFF.State[Y,X] of
          bsNoBuild:   gRenderAux.Quad(X, Y, $99000000 OR COLOR_BLACK);
          bsHousePlan: gRenderAux.Quad(X, Y, $66000000 OR COLOR_BLACK);
          bsFieldPlan: gRenderAux.Quad(X, Y, $33000000 OR COLOR_BLACK);
          bsRoadPlan:  gRenderAux.Quad(X, Y, $33000000 OR COLOR_BLUE);
          bsRoad:      gRenderAux.Quad(X, Y, $33000000 OR COLOR_BLUE);
          bsBuild:     gRenderAux.Quad(X, Y, $33000000 OR COLOR_YELLOW);
          bsTree:      gRenderAux.Quad(X, Y, $99000000 OR COLOR_GREEN);
          bsForest:    gRenderAux.Quad(X, Y, $66000000 OR COLOR_GREEN);
          bsCoal:      gRenderAux.Quad(X, Y, $66000000 OR COLOR_BLACK);
          bsReserved:  gRenderAux.Quad(X, Y, $66000000 OR COLOR_RED);
          bsDebug:     gRenderAux.Quad(X, Y, $FF000000 OR COLOR_BLACK);
          else begin end;
        end;
  for I := 0 to fBuildFF.Locs.Count - 1 do
    gRenderAux.Quad(fBuildFF.Locs.Items[I].X, fBuildFF.Locs.Items[I].Y, $66000000 OR COLOR_BLACK);

  //}
  { Coal
  for I := 0 to Length(fCoalPolygons) - 1 do
    DrawTriangle(I, $FF00FF OR (Min($FF,Round(fCoalPolygons[I]*5)) shl 24));
  //}
  { Polygon routes
  for I := 0 to Length(fPolygonRoutes) - 1 do
    DrawTriangle(I, $FFFFFF OR (fPolygonRoutes[I] shl 24));
  //}
  { Stone mining tiles
  for I := 0 to fStoneMiningTiles.Count - 1 do
    gRenderAux.Quad(fStoneMiningTiles.Items[I].X, fStoneMiningTiles.Items[I].Y, COLOR_RED);
  //}
end;




{ TKMBuildFF }
constructor TKMBuildFF.Create();
begin
  inherited Create();
  fUpdateTick := 0;
  fLocs := TKMPointList.Create();
end;

destructor TKMBuildFF.Destroy();
begin
  inherited;
  fLocs.free;
end;


// Transform 1D array to 2D
//function TKMBuildFF.GetInfo(const aY,aX: Word): TKMBuildInfo;
//begin
//  Result := fInfoArr[aY*fMapX + aX];
//end;
//procedure TKMBuildFF.SetInfo(const aY,aX,aNext,aDistance: Word; const aVisited: Byte; const aState: TKMBuildState);
//begin
//  with fInfoArr[aY*fMapX + aX] do
//  begin
//    Visited := aVisited;
//    State := aState;
//    Next := aNext;
//    Distance := aDistance;
//  end;
//end;

function TKMBuildFF.GetVisited(const aY,aX: Word): Byte;
begin
  Result := fInfoArr[aY*fMapX + aX].Visited;
end;
procedure TKMBuildFF.SetVisited(const aY,aX: Word; const aVisited: Byte);
begin
  fInfoArr[aY*fMapX + aX].Visited := aVisited;
end;

function TKMBuildFF.GetOwnersIndex(const aOwner: TKMHandIndex): Byte;
begin
  Result := fOwnerUpdateInfo[aOwner];
end;

function TKMBuildFF.GetState(const aY,aX: Word): TKMBuildState;
begin
  Result := fInfoArr[aY*fMapX + aX].State;
end;
procedure TKMBuildFF.SetState(const aY,aX: Word; const aState: TKMBuildState);
begin
  fInfoArr[aY*fMapX + aX].State := aState;
end;

function TKMBuildFF.GetStateFromIdx(const aIdx: Word): TKMBuildState;
begin
  Result := fInfoArr[aIdx].State;
end;

function TKMBuildFF.GetDistance(const aPoint: TKMPoint): Word;
begin
  Result := fInfoArr[aPoint.Y*fMapX + aPoint.X].Distance;
end;
function TKMBuildFF.GetDistanceInitPoint(const aPoint: TKMPoint): Word;
begin
  Result := fInfoArr[aPoint.Y*fMapX + aPoint.X].DistanceInitPoint;
end;

function TKMBuildFF.GetNext(const aY,aX: Word): Word;
begin
  Result := fInfoArr[aY*fMapX + aX].Next;
end;
procedure TKMBuildFF.SetNext(const aY,aX,aNext: Word);
begin
  fInfoArr[aY*fMapX + aX].Next := aNext;
end;


// Init queue
procedure TKMBuildFF.InitQueue(aHouseFF: Boolean);
var
  I: Integer;
begin
  fHMA := gAIFields.Eye.HousesMapping; // Make sure that HMA is actual
  fQueueCnt := 0;
  if (Length(fInfoArr) <> gTerrain.MapY * gTerrain.MapX) then
  begin
    fMapX := gTerrain.MapX;
    fMapY := gTerrain.MapY;
    SetLength(fInfoArr, fMapX * fMapY);
    SetLength(fOwnerUpdateInfo, MAX_HANDS);
    fVisitIdx := 255; // Fill char will be required
    fVisitIdxHouse := 255;
  end;
  if (fVisitIdx >= 254) then
  begin
    fVisitIdx := 0;
    for I := 0 to Length(fInfoArr) - 1 do
      fInfoArr[I].Visited := 0;
    //FillChar(fInfoArr[0], SizeOf(TKMBuildInfo)*Length(fInfoArr), #0); //Clear up
  end;
  if (fVisitIdxHouse >= 254) then
  begin
    fVisitIdxHouse := 0;
    for I := 0 to Length(fInfoArr) - 1 do
      fInfoArr[I].VisitedHouse := 0;
  end;
  if aHouseFF then
    fVisitIdxHouse := fVisitIdxHouse + 1
  else
    fVisitIdx := fVisitIdx + 1;
end;


function TKMBuildFF.InsertInQueue(const aIdx: Word): Word;
begin
  if (fQueueCnt = 0) then
    fStartQueue := aIdx
  else
    fInfoArr[fEndQueue].Next := aIdx;
  fEndQueue := aIdx;
  fQueueCnt := fQueueCnt + 1;
  Result := aIdx;
end;


function TKMBuildFF.RemoveFromQueue(var aX,aY,aIdx: Word): Boolean;
begin
  Result := (fQueueCnt > 0);
  if Result then
  begin
    aIdx := fStartQueue;
    aY := fInfoArr[fStartQueue].Y;// aIdx div fMapX;
    aX := aIdx - aY * fMapX;
    fStartQueue := fInfoArr[fStartQueue].Next;
    fQueueCnt := fQueueCnt - 1;
  end;
end;


function TKMBuildFF.CanBeVisited(const aX,aY,aIdx: Word; const aHouseQueue: Boolean = False): Boolean;
begin
  if aHouseQueue then
    Result := (fInfoArr[aIdx].Visited = fVisitIdx) AND (fInfoArr[aIdx].VisitedHouse < fVisitIdxHouse) AND gTerrain.TileIsRoadable( KMPoint(aX,aY) )
  else
    Result := (fInfoArr[aIdx].Visited < fVisitIdx) AND gTerrain.TileIsRoadable( KMPoint(aX,aY) );
end;


procedure TKMBuildFF.MarkAsVisited(const aY,aIdx,aDistance: Word; const aState: TKMBuildState);
begin
  with fInfoArr[aIdx] do
  begin
    Y := aY;
    Visited := fVisitIdx;
    State := aState;
    Distance := aDistance;
  end;
end;
procedure TKMBuildFF.MarkAsVisited(const aX,aY,aIdx,aDistance: Word);
var
  Point: TKMPoint;
begin
  with fInfoArr[aIdx] do
  begin
    VisitedHouse := fVisitIdxHouse;
    DistanceInitPoint := aDistance;

    Point := KMPoint(aX,aY);
    if CanBePlacedHouse(Point) then
      fLocs.Add(Point);
  end;
end;


function TKMBuildFF.CanBePlacedHouse(const aLoc: TKMPoint): Boolean;
const
  DIST = 1;
var
  LeftSideFree, RightSideFree: Boolean;
  I: Integer;
  Point: TKMPoint;
  Dir: TDirection;
begin
  Result := False;
  with fHMA[fHouseReq.HouseType] do
  begin
    for I := Low(Tiles) to High(Tiles) do
    begin
      Point := KMPointAdd(aLoc, Tiles[I]);
      if (Point.Y < 2) OR (Point.X < 2) OR (Point.X > fMapX - 2) OR (Point.Y > fMapY - 2) then
        Exit;
      case State[Point.Y, Point.X] of
        bsDebug,bsBuild:
        begin

        end;
        bsTree:
        begin
          if not fHouseReq.IgnoreTrees then
            Exit;
        end;
        bsCoal, bsForest:
        begin
          if not fHouseReq.IgnoreAvoidBuilding then
            Exit;
        end;
        else
          Exit;
      end;
    end;

    // Scan tiles in distance 1 from house plan
    LeftSideFree := True;
    RightSideFree := True;
    for Dir := Low(Surroundings[DIST]) to High(Surroundings[DIST]) do
      for I:= Low(Surroundings[DIST,Dir]) to High(Surroundings[DIST,Dir]) do
      begin
        Point := KMPointAdd(aLoc, Surroundings[DIST,Dir,I]);
        if (Dir = dirS) AND (State[Point.Y, Point.X] in [bsNoBuild, bsHousePlan, bsFieldPlan]) then
            Exit;
        if fHouseReq.IgnoreAvoidBuilding AND (State[Point.Y, Point.X] = bsHousePlan) then
            Exit;
        if (Dir = dirE) then
        begin
          if (State[Point.Y, Point.X] in [bsNoBuild, bsHousePlan]) then
            RightSideFree := False;
        end
        else if (Dir = dirW) then
        begin
          if (State[Point.Y, Point.X] in [bsNoBuild, bsHousePlan]) then
            LeftSideFree := False;
        end;
        if not (LeftSideFree OR RightSideFree) then
          Exit;
      end;
  end;
  Result := True;
end;


function TKMBuildFF.GetTerrainState(const aX,aY: Word): TKMBuildState;
var
  AB: Byte;
  Output: TKMBuildState;
begin
  Result := bsNoBuild;

  // Passability
  if (tpBuild in gTerrain.Land[aY,aX].Passability) then
    Output := bsBuild
  else if (gTerrain.ObjectIsChopableTree(KMPoint(aX,aY), [caAge1,caAge2,caAge3,caAgeFull])
        AND gHands[fOwner].CanAddFieldPlan(KMPoint(aX,aY), ftWine)) then
    Output := bsTree
  else if (gTerrain.Land[aY,aX].Passability * [tpMakeRoads, tpWalkRoad] <> []) then
    Output := bsRoad
  else if (gTerrain.Land[aY,aX].TileLock = tlRoadWork) then
    Output := bsRoadPlan
  else
    Exit;
  // Avoid building
  AB := gAIFields.Influences.AvoidBuilding[aY, aX];
  case AB of
    AVOID_BUILDING_NODE_LOCK_FIELD:    Output := bsFieldPlan;
    AVOID_BUILDING_NODE_LOCK_ROAD:     Output := bsRoadPlan;
    AVOID_BUILDING_HOUSE_INSIDE_LOCK:  Output := bsReserved;
    AVOID_BUILDING_HOUSE_OUTSIDE_LOCK:
      begin
        if (Output = bsBuild) then
          Output := bsRoad;
      end;
    AVOID_BUILDING_MINE_TILE:          Output := bsReserved;
    AVOID_BUILDING_COAL_TILE:
      begin
        case Output of
          bsTree: Output := bsNoBuild;
          bsBuild: Output := bsCoal;
          bsRoad: Output := bsCoal;
          bsRoadPlan: Output := bsCoal;
        end;
      end;
    else
    begin
      if (AB > AVOID_BUILDING_FOREST_MINIMUM) AND (Output <> bsTree) then
        Output := bsForest;
    end;
  end;
  Result := Output;
end;


procedure TKMBuildFF.MarkPlans();
const
  DIST = 1;
var
  PL: TKMHandIndex;
  I,K: Integer;
  Dir: TDirection;
  P1,P2: TKMPoint;
  HT: TKMHouseType;
begin
// Mark plans (only allied houses)
  for PL := 0 to gHands.Count - 1 do
    if (gHands[fOwner].Alliances[PL] = at_Ally) then
    begin
      // House plans
      for I := 0 to gHands[PL].BuildList.HousePlanList.Count - 1 do
        with gHands[PL].BuildList.HousePlanList.Plans[I] do
        begin
          HT := HouseType;
          if (HT = htNone) then
            continue;
          P1 := KMPointAdd( Loc, KMPoint(gRes.Houses[HT].EntranceOffsetX,0) ); // Plans have moved offset so fix it (because there is never enought exceptions ;)
          // Internal house tiles
          for K := Low(fHMA[HT].Tiles) to High(fHMA[HT].Tiles) do
          begin
            P2 := KMPointAdd(P1, fHMA[HT].Tiles[K]);
            State[P2.Y, P2.X] := bsHousePlan;
          end;
          // External house tiles in distance 1 from house plan
          for Dir := Low(fHMA[HT].Surroundings[DIST]) to High(fHMA[HT].Surroundings[DIST]) do
            for K := Low(fHMA[HT].Surroundings[DIST,Dir]) to High(fHMA[HT].Surroundings[DIST,Dir]) do
            begin
              P2 := KMPointAdd(P1, fHMA[HT].Surroundings[DIST,Dir,K]);
              if (gTerrain.Land[P2.Y,P2.X].Passability * [tpMakeRoads, tpWalkRoad] <> []) then
                State[P2.Y, P2.X] := bsRoad
              else
                State[P2.Y, P2.X] := bsHousePlan;
            end;
        end;
      // Field plans
      for I := 0 to gHands[PL].BuildList.FieldworksList.Count - 1 do
        with gHands[PL].BuildList.FieldworksList.Fields[I] do
          case FieldType of
            ftNone: continue;
            ftRoad: State[Loc.Y, Loc.X] := bsRoadPlan;
            else State[Loc.Y, Loc.X] := bsFieldPlan;
          end;
    end;
end;


procedure TKMBuildFF.TerrainFF();
const
  MAX_DIST = 40;
var
  X,Y,Idx,Distance: Word;
begin
  while RemoveFromQueue(X,Y,Idx) do
  begin
    Distance := fInfoArr[Idx].Distance + 1;
    if (Distance > MAX_DIST) then
      break;
    if (Y-1 >= 1      ) AND CanBeVisited(X,Y-1,Idx-fMapX) then MarkAsVisited(Y-1, InsertInQueue(Idx-fMapX), Distance, GetTerrainState(X,Y-1));
    if (X-1 >= 1      ) AND CanBeVisited(X-1,Y,Idx-1    ) then MarkAsVisited(Y,   InsertInQueue(Idx-1)    , Distance, GetTerrainState(X-1,Y));
    if (X+1 <= fMapX-1) AND CanBeVisited(X+1,Y,Idx+1    ) then MarkAsVisited(Y,   InsertInQueue(Idx+1)    , Distance, GetTerrainState(X+1,Y));
    if (Y+1 <= fMapY-1) AND CanBeVisited(X,Y+1,Idx+fMapX) then MarkAsVisited(Y+1, InsertInQueue(Idx+fMapX), Distance, GetTerrainState(X,Y+1));
  end;
end;


procedure TKMBuildFF.HouseFF();
const
  MAX_DIST = 40-10;
var
  X,Y,Idx,Distance: Word;
begin
  while RemoveFromQueue(X,Y,Idx) do
  begin
    Distance := fInfoArr[Idx].DistanceInitPoint + 1;
    if (Distance > MAX_DIST)
      OR (fHouseReq.MaxCnt <= fLocs.Count)
      OR (Distance > fHouseReq.MaxDist) then
      break;
    if (Y-1 >= 1      ) AND CanBeVisited(X,Y-1,Idx-fMapX,True) then MarkAsVisited(X,Y-1, InsertInQueue(Idx-fMapX), Distance);
    if (X-1 >= 1      ) AND CanBeVisited(X-1,Y,Idx-1    ,True) then MarkAsVisited(X-1,Y, InsertInQueue(Idx-1)    , Distance);
    if (X+1 <= fMapX-1) AND CanBeVisited(X+1,Y,Idx+1    ,True) then MarkAsVisited(X+1,Y, InsertInQueue(Idx+1)    , Distance);
    if (Y+1 <= fMapY-1) AND CanBeVisited(X,Y+1,Idx+fMapX,True) then MarkAsVisited(X,Y+1, InsertInQueue(Idx+fMapX), Distance);
  end;
end;


procedure TKMBuildFF.UpdateState();
  procedure MarkHouse(Loc: TKMPoint);
  begin
    MarkAsVisited(Loc.Y, InsertInQueue(Loc.Y*fMapX + Loc.X), 0, GetTerrainState(Loc.X,Loc.Y));
  end;
var
  I: Integer;
  H: TKMHouse;
  HT: TKMHouseType;
  Planner: TKMCityPlanner;
begin
  Planner := gHands[fOwner].AI.CityManagement.Builder.Planner;

  if (fUpdateTick = 0) OR (fUpdateTick < gGame.GameTickCount) then // Dont scan multile times terrain in 1 tick
  begin
    InitQueue(False);
    fOwnerUpdateInfo[fOwner] := fVisitIdx; // Debug tool

    if (gGame.GameTickCount <= MAX_HANDS) then // Make sure that Planner is already updated otherwise take only available houses
    begin
      for I := 0 to gHands[fOwner].Houses.Count - 1 do
      begin
        H := gHands[fOwner].Houses[I];
        if (H <> nil) AND not H.IsDestroyed AND not (H.HouseType in [htWatchTower, htWoodcutters]) then
          MarkHouse(H.Entrance);
      end;
    end
    else
    begin
      for HT := HOUSE_MIN to HOUSE_MAX do
        if not (HT in [htWatchTower, htWoodcutters]) then
          with Planner.PlannedHouses[HT] do
            for I := 0 to Count - 1 do
              MarkHouse(Plans[I].Loc);
    end;
    TerrainFF();

    fUpdateTick := gGame.GameTickCount;
    MarkPlans(); // Plans may change durring placing houses but this event is caught CityBuilder
  end;
end;


procedure TKMBuildFF.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
  fUpdateTick := 0; // Make sure that area will be scanned in next update
end;


procedure TKMBuildFF.ActualizeTile(aX, aY: Word);
begin
  if (fUpdateTick = gGame.GameTickCount) then // Actualize tile only when we need scan in this tick
    State[aY, aX] := GetTerrainState(aX,aY);
end;


procedure TKMBuildFF.FindPlaceForHouse(aHouseReq: TKMHouseRequirements; InitPointsArr: TKMPointArray; aClearHouseList: Boolean = True);
var
  I: Integer;
begin
  if aClearHouseList then
    fLocs.Clear;
  if (Length(InitPointsArr) <= 0) then
    Exit;

  fHouseReq := aHouseReq;
  UpdateState();

  InitQueue(True);
  for I := 0 to Length(InitPointsArr) - 1 do
    with InitPointsArr[I] do
      if CanBeVisited(X,Y,Y*fMapX + X, True) then
        MarkAsVisited(X,Y, InsertInQueue(Y*fMapX + X), 0);

  HouseFF();
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
    1: Result := Result AND gTerrain.TileHasCoal(aX, aY); // Coal
    2: Result := Result AND gTerrain.TileHasStone(aX, aY); // Stone
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
  if ((fSearch = 1) AND gAIFields.Eye.CanAddHousePlan(KMPoint(aX,aY), htCoalMine, True, True))
    OR (  (fSearch = 2) AND (aY < fMaxLimit.Y) AND (tpWalk in gTerrain.Land[aY+1,aX].Passability)  ) then
    fListOfPoints.Add(KMPoint(aX,aY));
end;

procedure TKMSearchResource.QuickFlood(aX,aY,aSearch: SmallInt; var aListOfPoints: TKMPointList);
begin
  fSearch := aSearch;
  fListOfPoints := aListOfPoints;
  inherited QuickFlood(aX,aY);
end;



{ TKMFFInitPlace }
function TKMFFInitPlace.CanBeExpanded(const aIdx: Word): Boolean;
const
  MAX_DISTANCE = 200;
begin
  Result := (fQueueArray[aIdx].Distance < MAX_DISTANCE);
end;


procedure TKMFFInitPlace.MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint);
const
  COEF = 5;
begin
  fPolygonArr[aIdx] := Max(fPolygonArr[aIdx], 255 - (aDistance * 5));
  inherited MarkAsVisited(aIdx, aDistance, aPoint);
end;


function TKMFFInitPlace.FillPolygons(const aMaxIdx: Word; aInitIdxArray: TKMWordArray; var aPolygonArr: TKMByteArray): Boolean;
begin
  fPolygonArr := aPolygonArr;
  Result := inherited FillPolygons(aMaxIdx, aInitIdxArray);
end;

end.
