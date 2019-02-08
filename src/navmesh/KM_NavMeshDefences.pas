unit KM_NavMeshDefences;
{$I KaM_Remake.inc}
interface
uses
  Math, KM_Defaults, KM_CommonTypes, KM_Points,
  KM_NavMeshFloodFill;

type
  TDefenceInfo = record
    Influence, EnemyInfluence, AllyInfluence: Byte;
    Distance: Word;
    PointInDir: TKMPoint;
  end;
  TDefInfoArray = array of TDefenceInfo;

  // Defence lines for watchtowers
  TKMDefenceLine = record
    Polygon: Word;
    Nodes: array[0..1] of Word;
  end;
  TKMDefenceLines = record
    Count: Word;
    Lines: array of TKMDefenceLine;
  end;

  // Defence position for army defence
  TKMDefencePosition = record
    Polygon, Weight: Word;
    DirPoint: TKMPointDir;
  end;
  TKMDefencePosArr = array of TKMDefencePosition;

  TForwardFF = class;
  TBackwardFF = class;
  TFilterFF = class;

  TForwardFF = class(TNavMeshFloodFill)
  private
  protected
    fOwner: TKMHandIndex;
    fDefInfo: TDefInfoArray;
    fBackwardFF: TBackwardFF;

    procedure MakeNewQueue(); override;
    function IsVisited(const aIdx: Word): Boolean; override;
    function CanBeExpanded(const aIdx: Word): Boolean; override;
    procedure MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint); override;
    function ForwardFF(): Boolean;
  public
    //D_INIT_FLOOD, D_FF_INIT_ARR, D_FF_INIT_FLOOD: TKMWordArray;
    BestDefLines: TKMDefenceLines;
    FirstLine: Word;

    constructor Create(aSorted: Boolean = False); reintroduce;
    destructor Destroy(); override;

    function FindDefenceLines(aOwner: TKMHandIndex; var aDefLines: TKMDefenceLines): Boolean;
    function FindDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt: Word; var aDefPosArr: TKMDefencePosArr; aMinDefeces: Boolean = True): Boolean;

    //procedure DEBUG();
  end;


  TBackwardFF = class(TNavMeshFloodFill)
  private
  protected
    fDefLinesRequired: Boolean;
    fOwner: TKMHandIndex;
    fBestEvaluation: Single;
    fDefInfo: TDefInfoArray;
    fFilterFF: TFilterFF;

    function CanBeExpanded(const aIdx: Word): Boolean; override;
    procedure BackwardFlood();
    procedure EvaluateDefence(const aIdx: Word);
    function FindDefencePos(var aBaseCnt, aFirstLine: Word; aMinDefeces: Boolean): Boolean;
  public
    //D_INIT_ARR, D_INIT_FLOOD: TKMWordArray;
    BestDefLines: TKMDefenceLines;
    DefPosArr: TKMDefencePosArr;

    constructor Create(aSorted: Boolean = False); reintroduce;
    destructor Destroy(); override;

    procedure UpdatePointers(var aDefInfo: TDefInfoArray; var aQueueArray: TPolygonsQueueArr);
    procedure NewQueue(aVisitedIdx: Byte);
    procedure AddPolygon(aIdx: Word);
    function FindDefenceLines(aOwner: TKMHandIndex): TKMDefenceLines;
    function FindDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt, aFirstLine: Word; var aBestDefLines: TKMDefenceLines; aMinDefeces: Boolean; aDefLinesRequired: Boolean): TKMDefencePosArr;

    //procedure DEBUG();
  end;


  TFilterFF = class(TNavMeshFloodFill)
  private
  protected
    procedure InitQueue(const aMaxIdx: Word; aInitIdxArray: TKMWordArray); override;
    function IsVisited(const aIdx: Word): Boolean; override;
  public
    BestDefLines, AllDefLines: TKMDefenceLines;
    function IsPolyInsideDef(aIdx: Word): Boolean;
    procedure FilterDefenceLine(var aAllDefLines: TKMDefenceLines);
  end;


const
  OWNER_INFLUENCE_LIMIT = 220; // From this influence limit will be counted distance, it is also the closest line of possible defence
  MAX_ENEMY_INFLUENCE = 200; // Maximal enemy influence in FordwardFF (forward flood fill will not scan futher)
  ALLY_INFLUENCE_LIMIT = 230; // When ally influence will be greater than this constant there will be applied penalization ALLY_INFLUENCE_PENALIZATION in weight function of actual defensive line
  ENEMY_INFLUENCE_LIMIT = 10; // When enemy influence will be greter than this constant there will be applied penalization ENEMY_INFLUENCE_PENALIZATION in weight function of actual defensive line

  // Weights of defensive line calculation
  MIN_OPTIMAL_INFLUENCE = 150; // Minimal optimal influence (maximal is given by ALLY_INFLUENCE_LIMIT)
  POLYGON_CNT_PENALIZATION = 2; // Polygon count penalization (more polygons = worse defensive line)
  OPTIMAL_INFLUENCE_ADD = 1; // Improve criterium of actual defence line in case that influence is in <MIN_OPTIMAL_INFLUENCE, ALLY_INFLUENCE_LIMIT>
  ALLY_INFLUENCE_PENALIZATION = 4; // Ally penalization (dont place defences inside of ally city)
  ENEMY_INFLUENCE_PENALIZATION = 6; // Enemy penalization (dont place defences inside of enemy city)
  MINIMAL_DEFENCE_DISTANCE = 1; // Minimal distance of defensive lines (minimal distance is also affected by OWNER_INFLUENCE_LIMIT)
  MAXIMAL_DEFENCE_DISTANCE = 75; // Maximal defence distance (maximal distance is also affected by MAX_ENEMY_INFLUENCE)

implementation
uses
  KM_Hand, KM_HandsCollection, KM_AIFields, KM_AIInfluences, KM_NavMesh;


{ TForwardFF }
constructor TForwardFF.Create(aSorted: Boolean = False);
begin
  inherited Create(aSorted);
  fBackwardFF := TBackwardFF.Create(aSorted);
end;


destructor TForwardFF.Destroy();
begin
  fBackwardFF.Free;
  inherited Destroy;
end;


// Prepare new Queue
procedure TForwardFF.MakeNewQueue();
begin
  fVisitedIdx := fVisitedIdx + 3; // There is 1 FF forward, 1 FF backward and 1 FF for positioning in 1 cycle; filter have its own array
  // Check length
  if (Length(fQueueArray) < Length(gAIFields.NavMesh.Polygons)) then
  begin
    SetLength(fQueueArray, Length(gAIFields.NavMesh.Polygons));
    SetLength(fDefInfo, Length(gAIFields.NavMesh.Polygons));
    fBackwardFF.UpdatePointers(fDefInfo, fQueueArray);
    ClearVisitIdx();
  end;
  // Reset VisitIdx if needed
  if (fVisitedIdx >= High(Byte)-3) then
    ClearVisitIdx();
  // Init also backward FF
  fBackwardFF.NewQueue(fVisitedIdx+1);
end;


function TForwardFF.IsVisited(const aIdx: Word): Boolean;
begin
  Result := (fQueueArray[aIdx].Visited >= fVisitedIdx); // Arrays are common for TForwardFF and TBackwardFF so >= must be here
end;


function TForwardFF.CanBeExpanded(const aIdx: Word): Boolean;
begin
  // This is border condition for final line of forward flood fill (polygons with 3 nearby polygons can create defensive lines)
  Result := (gAIFields.NavMesh.Polygons[aIdx].NearbyCount <> 3) OR (fDefInfo[aIdx].EnemyInfluence < MAX_ENEMY_INFLUENCE);
  if not Result then
    fBackwardFF.AddPolygon(aIdx);
end;


procedure TForwardFF.MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint);
var
  Distance: Word;
begin
  // Get owner influence and distance from influence
  fDefInfo[aIdx].Influence := gAIFields.Influences.OwnPoly[fOwner, aIdx];
  fDefInfo[aIdx].AllyInfluence := gAIFields.Influences.GetBestAllianceOwnership(fOwner, aIdx, at_Ally);
  fDefInfo[aIdx].EnemyInfluence := gAIFields.Influences.GetBestAllianceOwnership(fOwner, aIdx, at_Enemy);
  Distance := aDistance + fDefInfo[aIdx].EnemyInfluence shr 2;
  if (fDefInfo[aIdx].Influence > OWNER_INFLUENCE_LIMIT) OR (fDefInfo[aIdx].AllyInfluence > OWNER_INFLUENCE_LIMIT) then
    Distance := 0;
  inherited MarkAsVisited(aIdx, Distance, aPoint);
  // For special polygons calculate also ene influence
  //if (gAIFields.NavMesh.Polygons[aIdx].NearbyCount = 3) then
end;


// Forward flood fill -> from polygons in the owner's city make flood fill to polygons in hostile influence area
// ForwardFF will find enemies, compute distance and detect empty areas which does not requires defences
function TForwardFF.ForwardFF(): Boolean;
var
  PL: TKMHandIndex;
  I, Cnt: Integer;
  CityCenterPoints: TKMPointArray;
  StartPolygons: TKMWordArray;
begin
  Result := False;
  // Get center points of all allied cities
  Cnt := 0;
  for PL := 0 to gHands.Count - 1 do
    if (gHands[fOwner].Alliances[PL] = at_Ally) then
    begin
      gAIFields.Eye.OwnerUpdate(PL);
      CityCenterPoints := gAIFields.Eye.GetCityCenterPoints(True);
      if (PL = fOwner) AND (Length(CityCenterPoints) < 1) then
        Exit;
      // Transform it to polygons
      SetLength(StartPolygons, Cnt + Length(CityCenterPoints));
      for I := 0 to Length(CityCenterPoints) - 1 do
      begin
        StartPolygons[Cnt] := gAIFields.NavMesh.KMPoint2Polygon[ CityCenterPoints[I] ];
        Cnt := Cnt + 1;
      end;
    end;
  // Update owner
  gAIFields.Eye.OwnerUpdate(fOwner);

  // Flood fill
  Result := FillPolygons(Length(StartPolygons), StartPolygons);
end;


// Find best defensive lines (for watchtowers)
function TForwardFF.FindDefenceLines(aOwner: TKMHandIndex; var aDefLines: TKMDefenceLines): Boolean;
begin
  fOwner := aOwner;

  gAIFields.Eye.OwnerUpdate(aOwner); // Make sure that Eye is set to the right Owner (Old AI does not shift it)

  Result := ForwardFF();
  if Result then
    aDefLines := fBackwardFF.FindDefenceLines(aOwner);
end;


// Find best defensive polygons (for defensive positions)
function TForwardFF.FindDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt: Word; var aDefPosArr: TKMDefencePosArr; aMinDefeces: Boolean = True): Boolean;
begin
  fOwner := aOwner;
  FirstLine := 0;

  gAIFields.Eye.OwnerUpdate(aOwner); // Make sure that Eye is set to the right Owner (Old AI does not shift it)

  Result := ForwardFF();
  if Result then
    aDefPosArr := fBackwardFF.FindDefensivePolygons(aOwner, aBaseCnt, FirstLine, BestDefLines, aMinDefeces, False);
end;




{ TBackwardFF }
constructor TBackwardFF.Create(aSorted: Boolean = False);
begin
  inherited Create(aSorted);
  fFilterFF := TFilterFF.Create(False);
end;


destructor TBackwardFF.Destroy();
begin
  fFilterFF.Free;
  inherited Destroy;
end;


procedure TBackwardFF.UpdatePointers(var aDefInfo: TDefInfoArray; var aQueueArray: TPolygonsQueueArr);
begin
  fDefInfo := aDefInfo;
  fQueueArray := aQueueArray;
end;


procedure TBackwardFF.NewQueue(aVisitedIdx: Byte);
begin
  fQueueCnt := 0;
  fVisitedIdx := aVisitedIdx;
  fBestEvaluation := 1000000000;
end;


procedure TBackwardFF.AddPolygon(aIdx: Word);
begin
  fQueueArray[aIdx].Visited := fVisitedIdx; // Polygon is marked from ForwardFF
  fDefInfo[aIdx].Distance := fQueueArray[aIdx].Distance; // Save distance for future calculation of defensive lines
  fQueueArray[aIdx].Distance := High(Word) - fQueueArray[aIdx].Distance; // NavMesh Flood fill class can sort only in increased order -> distances must be inverted when we want to start with the farthest
  InsertAndSort(aIdx);
end;


// Evaluation of actual defensive line
procedure TBackwardFF.EvaluateDefence(const aIdx: Word);
var
  PolyArr: TPolygonArray;
  // Get second defensive polygon (2 neighboring polygons in NavMesh have common 1 line)
  function GetSecondDefencePolygon(const aDefIdx: Word): Word;
  var
    I, NearbyIdx, VisitedCnt, VisitedIdx, UnVisitedIdx: Word;
  begin
    Result := aDefIdx;
    // Find un/visited surrounding polygons (in case that there are 2 visited polygons defece line will be with unvisited and vice versa)
    VisitedCnt := 0;
    VisitedIdx := 0; // For compiler
    UnVisitedIdx := 0; // For compiler
    for I := 0 to PolyArr[aDefIdx].NearbyCount-1 do
    begin
      NearbyIdx := PolyArr[aDefIdx].Nearby[I];
      if IsVisited(NearbyIdx) then
      begin
        VisitedCnt := VisitedCnt + 1;
        VisitedIdx := NearbyIdx;
      end
      else
        UnVisitedIdx := NearbyIdx;
    end;
    if (VisitedCnt = 1) then
      Result := VisitedIdx
    else if (VisitedCnt <> 3) then
      Result := UnVisitedIdx;
  end;
  // Add new defensive polygon and find defensive line
  procedure AddDefence(aDefIdx1: Word);
  var
    SecondIndice: Boolean;
    I,K, DefIdx2: Word;
  begin
    BestDefLines.Lines[ BestDefLines.Count ].Polygon := aDefIdx1;
    // For defensive position we dont need proper defensive lines only border polygon for position flood fill
    if fDefLinesRequired then
    begin
      DefIdx2 := GetSecondDefencePolygon(aDefIdx1);
      // Find common defensive line of 2 polygons
      SecondIndice := False;
      for I := 0 to 2 do
        for K := 0 to 2 do
          if (PolyArr[aDefIdx1].Indices[I] = PolyArr[DefIdx2].Indices[K]) then
          begin
            BestDefLines.Lines[ BestDefLines.Count ].Nodes[ Byte(SecondIndice) ] := PolyArr[aDefIdx1].Indices[I];
            SecondIndice := True; // It will also switch index from 0 to 1
            break;
          end;
      if SecondIndice then // Make sure that this node exist
        Inc(BestDefLines.Count);
    end
    else
      Inc(BestDefLines.Count);
  end;
var
  I, QueueIdx: Word;
  Evaluation: Single;
begin
  PolyArr := gAIFields.NavMesh.Polygons;

  // Calculate evaluation of actual defensive position
  Evaluation := fQueueCnt * POLYGON_CNT_PENALIZATION;
  QueueIdx := aIdx;
  for I := 0 to fQueueCnt do // aIdx is already taken from Queue so I must be from 0 to fQueueCnt!
  begin
    Evaluation := + Evaluation
                  //+ fDefInfo[aIdx].Distance // Consideration of distance does more damage than benefit
                  - Byte(    ((fDefInfo[aIdx].AllyInfluence > MIN_OPTIMAL_INFLUENCE) AND (fDefInfo[aIdx].AllyInfluence < ALLY_INFLUENCE_LIMIT))
                          OR ((fDefInfo[aIdx].Influence > MIN_OPTIMAL_INFLUENCE) AND (fDefInfo[aIdx].Influence < ALLY_INFLUENCE_LIMIT))
                         ) * OPTIMAL_INFLUENCE_ADD
                  //+ Byte(fDefInfo[aIdx].AllyInfluence > ALLY_INFLUENCE_LIMIT) * ALLY_INFLUENCE_PENALIZATION
                  + Byte(fDefInfo[aIdx].EnemyInfluence > ENEMY_INFLUENCE_LIMIT) * ENEMY_INFLUENCE_PENALIZATION;
    QueueIdx := fQueueArray[QueueIdx].Next;
  end;
  // If is evaluation better save polygons
  if (Evaluation < fBestEvaluation) then
  begin
    fBestEvaluation := Evaluation;
    BestDefLines.Count := 0; // Set defences count to 0 (it will be incremented later)
    if (fQueueCnt >= Length(BestDefLines.Lines)) then
      SetLength(BestDefLines.Lines, fQueueCnt + 32);
    QueueIdx := aIdx;
    // Copy defensive polygons
    for I := 0 to fQueueCnt do // aIdx is already taken from Queue so I must be from 0 to fQueueCnt!
    begin
      AddDefence(QueueIdx);
      QueueIdx := fQueueArray[QueueIdx].Next;
    end;
  end;
end;


function TBackwardFF.CanBeExpanded(const aIdx: Word): Boolean;
begin
  Result := (fDefInfo[aIdx].Distance > MINIMAL_DEFENCE_DISTANCE);

  if Result AND (fDefInfo[aIdx].Distance < MAXIMAL_DEFENCE_DISTANCE) then
    EvaluateDefence(aIdx);
end;


// Backward flood fill -> flood fill from enemy influence area to owner city
// BackwardFF will find the best possible defences
procedure TBackwardFF.BackwardFlood();
var
  PolyArr: TPolygonArray;
  // Prepare init polygons for backward flood fill (polygons near enemy)
  procedure CreateBorders();
  var
    I, K, QueueIdx, NearbyIdx: Word;
  begin
    QueueIdx := fStartQueue;
    for I := 1 to fQueueCnt do // Enemy polygons are prepared in Queue
    begin
      for K := 0 to PolyArr[QueueIdx].NearbyCount-1 do
      begin
        NearbyIdx := PolyArr[QueueIdx].Nearby[K];
        if (fQueueArray[NearbyIdx].Visited < fVisitedIdx-1) then // This polygon was not visited in forward cycle -> it will not be visited in backward
          fQueueArray[NearbyIdx].Visited := fVisitedIdx;
      end;
      QueueIdx := fQueueArray[QueueIdx].Next;
    end;
  end;
  // Find nearby unvisited polygons and expand polygon
  procedure ExpandPolygon(aIdx: Word);
  var
    I, NearbyIdx: Integer;
  begin
    for I := 0 to PolyArr[aIdx].NearbyCount-1 do
    begin
      NearbyIdx := PolyArr[aIdx].Nearby[I];
      if not IsVisited(NearbyIdx) then
      begin
        fQueueArray[NearbyIdx].Visited := fVisitedIdx; // Mark as visited
        fDefInfo[NearbyIdx].Distance := fQueueArray[NearbyIdx].Distance; // Save distance for future calculation
        if (PolyArr[NearbyIdx].NearbyCount = 3) then // Defences are created only from polygons with 3 surrounding polygons
        begin
          fQueueArray[NearbyIdx].Distance := High(Word) - fQueueArray[NearbyIdx].Distance; // Invert distance (only polygons which will be inserted)
          InsertAndSort(NearbyIdx); // Insert ONLY polygons with 3 nearby polygons because it is possible to create defence line from them and also it save performance
        end
        else
          ExpandPolygon(NearbyIdx);
      end;
    end;
  end;
var
  Idx: Word;
begin
  PolyArr := gAIFields.NavMesh.Polygons;
  CreateBorders();
  while RemoveFromQueue(Idx) do
    if CanBeExpanded(Idx) then
      ExpandPolygon(Idx);
end;


// Flood fill for detection of defensive positions
function TBackwardFF.FindDefencePos(var aBaseCnt, aFirstLine: Word; aMinDefeces: Boolean): Boolean;
const
  INIT_WEIGHT = 10000;
  SQR_MIN_DEF_POINT_DISTANCE = 3*3;
var
  Check: Boolean;
  PL: TKMHandIndex;
  Idx, NearbyIdx, Cnt, ScannedCnt, AllianceCnt: Word;
  I, K: Integer;
  Direction: TKMDirection;
  PolyArr: TPolygonArray;
begin
  Result := False;

  AllianceCnt := 1;
  for PL := 0 to gHands.Count - 1 do
    if gHands[PL].Enabled AND (fOwner <> PL) AND (gHands[fOwner].Alliances[PL] = at_Ally) then
      AllianceCnt := AllianceCnt + 1;

  // Check and determine counts
  aFirstLine := BestDefLines.Count;
  if (BestDefLines.Count = 0) then
    Exit
  else if aMinDefeces then
    aBaseCnt := Max(4,BestDefLines.Count)
  else
    aBaseCnt := Max(aBaseCnt * AllianceCnt,BestDefLines.Count * 2); // We need much more defeces because most of them will be used by allies

  PolyArr := gAIFields.NavMesh.Polygons;
  // Prepare defensive FF - add all polygon is BestDefLines to queue
  NewQueue(fVisitedIdx + 1);
  for I := 0 to BestDefLines.Count - 1 do
  begin
    Idx := BestDefLines.Lines[I].Polygon;
    MarkAsVisited(Idx, fDefInfo[ Idx ].Distance, PolyArr[Idx].CenterPoint);
    InsertInQueue( Idx );
    // Direction of groups in defence position may be tricky so it is computed for all children at once as a average point of surrounding polygons
    fDefInfo[Idx].PointInDir := PolyArr[Idx].CenterPoint;
    Cnt := 1;
    // Mark every polygon before def line as visited so FF will not make defences here
    for K := 0 to PolyArr[Idx].NearbyCount-1 do
    begin
      NearbyIdx := PolyArr[Idx].Nearby[K];
      //if (fDefInfo[ NearbyIdx ].Distance > fDefInfo[ Idx ].Distance) then // Sometimes distances does not match with polygons inside of def line
      if not fFilterFF.IsPolyInsideDef(NearbyIdx) then
      begin
        Cnt := Cnt + 1;
        fDefInfo[Idx].PointInDir := KMPointAdd(fDefInfo[Idx].PointInDir, PolyArr[NearbyIdx].CenterPoint);
        MarkAsVisited(NearbyIdx, fDefInfo[ NearbyIdx ].Distance, PolyArr[Idx].NearbyPoints[K]);
      end
    end;
    // Average sum of points for direction
    fDefInfo[Idx].PointInDir.X := Round( fDefInfo[Idx].PointInDir.X / Cnt );
    fDefInfo[Idx].PointInDir.Y := Round( fDefInfo[Idx].PointInDir.Y / Cnt );
  end;

  // Execute FF
  SetLength(DefPosArr, aBaseCnt);
  Cnt := 0;
  ScannedCnt := 0;
  while RemoveFromQueue(Idx) AND (Cnt < aBaseCnt) do
  begin
    ScannedCnt := ScannedCnt + 1;
    // Add only defence points with specicific distance
    Check := True;
    for I := 0 to Cnt - 1 do
      if (KMDistanceSqr(fQueueArray[Idx].DistPoint, DefPosArr[I].DirPoint.Loc) < SQR_MIN_DEF_POINT_DISTANCE) then
      begin
        Check := False;
        break;
      end;
    if Check then
    begin
      with DefPosArr[Cnt] do
      begin
        Polygon := Idx;
        Direction := KMGetDirection( fQueueArray[Idx].DistPoint, fDefInfo[Idx].PointInDir );
        DirPoint := KMPointDir( fQueueArray[Idx].DistPoint, Direction );
        Weight := Max( 0, (High(Word) shr 1) // Init value
                           + (fDefInfo[Idx].Influence shl 1) // Increase weight of owner's city
                           - ((ScannedCnt div aFirstLine) * 100) // Penalize back lines
                           - fDefInfo[Idx].AllyInfluence // Penalize allied influence
                           + fDefInfo[Idx].EnemyInfluence // Add more soldiers closer to enemy influence
                     );
      end;
      if (Direction <> dir_NA) then
        Cnt := Cnt + 1;
    end;
    // Expand polygon
    for I := 0 to PolyArr[Idx].NearbyCount-1 do
    begin
      NearbyIdx := PolyArr[Idx].Nearby[I];
      if not IsVisited(NearbyIdx) then
      begin
        MarkAsVisited(NearbyIdx, fDefInfo[ NearbyIdx ].Distance, PolyArr[Idx].NearbyPoints[I]);
        InsertInQueue(NearbyIdx);
        fDefInfo[NearbyIdx].PointInDir := fDefInfo[Idx].PointInDir;
      end;
    end;
  end;

  // Determine first line (divided by count of allies - it is rought because allies can have different size of defensive line but better than nothing)
  aFirstLine := Round(aFirstLine / AllianceCnt);

  SetLength(DefPosArr, Cnt);
  Result := True;
end;


function TBackwardFF.FindDefenceLines(aOwner: TKMHandIndex): TKMDefenceLines;
begin
  fOwner := aOwner;
  fDefLinesRequired := True;
  BackwardFlood();
  fFilterFF.FilterDefenceLine(BestDefLines);
  Result := BestDefLines;
end;


function TBackwardFF.FindDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt, aFirstLine: Word; var aBestDefLines: TKMDefenceLines; aMinDefeces: Boolean; aDefLinesRequired: Boolean): TKMDefencePosArr;
begin
  fOwner := aOwner;
  fDefLinesRequired := aDefLinesRequired;
  BackwardFlood();
  fFilterFF.FilterDefenceLine(BestDefLines);
  FindDefencePos(aBaseCnt, aFirstLine, aMinDefeces);
  aBestDefLines := BestDefLines;
  Result := DefPosArr;
end;




{ TFilterFF }
function TFilterFF.IsPolyInsideDef(aIdx: Word): Boolean;
begin
  Result := fQueueArray[aIdx].Visited = fVisitedIdx;
end;


function TFilterFF.IsVisited(const aIdx: Word): Boolean;
var
  I: Integer;
begin
  if (fQueueArray[aIdx].Visited = fVisitedIdx - 1) then
  begin
    for I := 0 to AllDefLines.Count - 1 do
      if (AllDefLines.Lines[I].Polygon = aIdx) then
      begin
        BestDefLines.Lines[ BestDefLines.Count ] := AllDefLines.Lines[I]; // Length was already checked
        BestDefLines.Count := BestDefLines.Count + 1;
        break;
      end;
    fQueueArray[aIdx].Visited := fVisitedIdx;
  end;
  Result := (fQueueArray[aIdx].Visited = fVisitedIdx);
end;


procedure TFilterFF.InitQueue(const aMaxIdx: Word; aInitIdxArray: TKMWordArray);
var
  I, Idx: Integer;
begin
  // Index will be increased by 2 so clear it now if it is needed
  if (fVisitedIdx >= High(Byte) - 3) OR (fVisitedIdx = 0) then
  begin
    fVisitedIdx := $FE;
    MakeNewQueue(); // Init array and clear index
  end
  else
    // Increase visited idx
    fVisitedIdx := fVisitedIdx + 1;
  // Mark exist defence lines as a visited in previous step (faster than check each node whether is part of defensive line)
  for I := 0 to AllDefLines.Count - 1 do
  begin
    Idx := AllDefLines.Lines[I].Polygon;
    fQueueArray[Idx].Visited := fVisitedIdx;
  end;
  // Init queue - fVisitedIdx will be increased inside of this function
  inherited InitQueue(aMaxIdx, aInitIdxArray); // InitQueue calls MakeNewQueue which also increase fVisitedIdx
end;


procedure TFilterFF.FilterDefenceLine(var aAllDefLines: TKMDefenceLines);
var
  I: Integer;
  CityCenterPoints: TKMPointArray;
  StartPolygons: TKMWordArray;
begin
  AllDefLines := aAllDefLines;
  BestDefLines.Count := 0;
  if (Length(BestDefLines.Lines) < AllDefLines.Count) then
    SetLength(BestDefLines.Lines, AllDefLines.Count);
  // Get center points of city
  CityCenterPoints := gAIFields.Eye.GetCityCenterPoints(True);
  if (Length(CityCenterPoints) < 1) then
    Exit;
  // Transform it to polygons
  SetLength(StartPolygons, Length(CityCenterPoints));
  for I := 0 to Length(CityCenterPoints) - 1 do
    StartPolygons[I] := gAIFields.NavMesh.KMPoint2Polygon[ CityCenterPoints[I] ];

  InitQueue(Length(StartPolygons)-1, StartPolygons);
  Flood();
  aAllDefLines.Count := 0;
  for I := 0 to BestDefLines.Count - 1 do
  begin
    aAllDefLines.Lines[ aAllDefLines.Count ] := BestDefLines.Lines[I];
    aAllDefLines.Count := aAllDefLines.Count + 1;
  end;
end;


{
procedure TForwardFF.DEBUG();
var
  I, cnt: Integer;
begin
  SetLength(D_INIT_FLOOD, Length(fQueueArray));
  cnt := 0;
  for I := 0 to Length(fQueueArray) - 1 do
    if (fQueueArray[I].Visited = fVisitedIdx) then
    begin
      D_INIT_FLOOD[cnt] := I;
      cnt := cnt + 1;
    end;
  SetLength(D_INIT_FLOOD, cnt);
  fBackwardFF.DEBUG();
  D_FF_INIT_ARR := fBackwardFF.D_INIT_ARR;
  D_FF_INIT_FLOOD := fBackwardFF.D_INIT_FLOOD;
  BestDefLines := fBackwardFF.BestDefLines;
end;


procedure TBackwardFF.DEBUG();
var
  I, cnt: Integer;
begin
  SetLength(D_INIT_ARR, fQueueCnt);
  I := fStartQueue;
  for cnt := 0 to fQueueCnt - 1 do
  begin
    D_INIT_ARR[cnt] := I;
    I := fQueueArray[I].Next;
  end;
  BackwardFlood();
  SetLength(D_INIT_FLOOD, Length(fQueueArray));
  cnt := 0;
  for I := 0 to Length(fQueueArray) - 1 do
    if (fQueueArray[I].Visited = fVisitedIdx) then
    begin
      D_INIT_FLOOD[cnt] := I;
      cnt := cnt + 1;
    end;
  SetLength(D_INIT_FLOOD, cnt);
end;
//}

end.
