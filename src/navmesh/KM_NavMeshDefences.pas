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


  TBackwardFF = class(TNavMeshFloodFill)
  private
  protected
    fDefLinesRequired: Boolean;
    fOwner: TKMHandIndex;
    fBestEvaluation: Single;
    fDefInfo: TDefInfoArray;

    function CanBeExpanded(const aIdx: Word): Boolean; override;
    procedure BackwardFlood();
    procedure EvaluateDefence(const aIdx: Word);
    function FindDefencePos(var aBaseCnt, aFirstLine: Word; aMinDefeces: Boolean): Boolean;
  public
    //D_INIT_ARR, D_INIT_FLOOD: TKMWordArray;
    BestDefLines: TKMDefenceLines;
    DefPosArr: TKMDefencePosArr;

    procedure UpdatePointers(var aDefInfo: TDefInfoArray; var aQueueArray: TPolygonsQueueArr);
    procedure NewQueue(aVisitedIdx: Byte);
    procedure AddPolygon(aIdx: Word);
    function FindDefenceLines(aOwner: TKMHandIndex): TKMDefenceLines;
    function FindDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt, aFirstLine: Word; var aBestDefLines: TKMDefenceLines; aMinDefeces: Boolean; aDefLinesRequired: Boolean): TKMDefencePosArr;

    //procedure DEBUG();
  end;

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


const
  OWNER_INFLUENCE_LIMIT = 220; // From this influence limit will be counted distance, it is also the closest line of possible defence
  MAX_ENEMY_INFLUENCE = 200; // Maximal enemy influence in FordwardFF (forward flood fill will not scan futher)
  ALLY_INFLUENCE_LIMIT = 200; // When ally influence will be greater than this constant there will be applied penalization ALLY_INFLUENCE_PENALIZATION in weight function of actual defensive line
  ENEMY_INFLUENCE_LIMIT = 10; // When enemy influence will be greter than this constant there will be applied penalization ENEMY_INFLUENCE_PENALIZATION in weight function of actual defensive line

  // Weights of defensive line calculation
  MIN_OPTIMAL_INFLUENCE = 100; // Minimal optimal influence (maximal is given by ALLY_INFLUENCE_LIMIT)
  POLYGON_CNT_PENALIZATION = 2; // Polygon count penalization (more polygons = worse defensive line)
  OPTIMAL_INFLUENCE_ADD = 1; // Improve criterium of actual defence line in case that influence is in <MIN_OPTIMAL_INFLUENCE, ALLY_INFLUENCE_LIMIT>
  ALLY_INFLUENCE_PENALIZATION = 4; // Ally penalization (dont place defences inside of ally city)
  ENEMY_INFLUENCE_PENALIZATION = 4; // Enemy penalization (dont place defences inside of enemy city)
  MINIMAL_DEFENCE_DISTANCE = 5; // Minimal distance of defensive lines (minimal distance is also affected by OWNER_INFLUENCE_LIMIT)
  MAXIMAL_DEFENCE_DISTANCE = 100; // Maximal defence distance (maximal distance is also affected by MAX_ENEMY_INFLUENCE)

implementation
uses
  KM_AIFields, KM_NavMesh;


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
  fVisitedIdx := fVisitedIdx + 3; // There is 1 FF forward, 1 FF backward and 1 FF for positioning in 1 cycle
  // Check length
  if (Length(fQueueArray) < Length(gAIFields.NavMesh.Polygons)) then
  begin
    SetLength(fQueueArray, Length(gAIFields.NavMesh.Polygons));
    SetLength(fDefInfo, Length(gAIFields.NavMesh.Polygons));
    fBackwardFF.UpdatePointers(fDefInfo, fQueueArray);
    ClearVisitIdx();
  end;
  // Reset VisitIdx if needed
  if (fVisitedIdx = High(Byte)-2) then
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
  Distance := aDistance; // Min(fDefInfo[aIdx].Influence shr 2, Distance);
  if (fDefInfo[aIdx].Influence > OWNER_INFLUENCE_LIMIT) then
    Distance := 0;
  inherited MarkAsVisited(aIdx, Distance, aPoint);
  // For special polygons calculate also alliance influence
  if (gAIFields.NavMesh.Polygons[aIdx].NearbyCount = 3) then
  begin
    fDefInfo[aIdx].EnemyInfluence := gAIFields.Influences.GetBestAllianceOwnership(fOwner, aIdx, at_Enemy);
    if (Distance < MAXIMAL_DEFENCE_DISTANCE) then // We dont need to know about ally influence far behind maximal defence distance so save some time and do nothing
      fDefInfo[aIdx].AllyInfluence := gAIFields.Influences.GetBestAllianceOwnership(fOwner, aIdx, at_Ally);
  end;
end;


// Forward flood fill -> from polygons in the owner's city make flood fill to polygons in hostile influence area
// ForwardFF will find enemies, compute distance and detect empty areas which does not requires defences
function TForwardFF.ForwardFF(): Boolean;
var
  I: Integer;
  CityCenterPoints: TKMPointArray;
  StartPolygons: TKMWordArray;
begin
  Result := False;
  // Get center city points
  CityCenterPoints := gAIFields.Eye.GetCityCenterPoints(True);
  if (Length(CityCenterPoints) < 1) then
    Exit;
  // Transform it to polygons
  SetLength(StartPolygons, Length(CityCenterPoints));
  for I := Low(StartPolygons) to High(StartPolygons) do
    StartPolygons[I] := gAIFields.NavMesh.KMPoint2Polygon[ CityCenterPoints[I] ];
  // Flood fill
  Result := FillPolygons(Length(StartPolygons), StartPolygons);
end;


// Find best defensive lines (for watchtowers)
function TForwardFF.FindDefenceLines(aOwner: TKMHandIndex; var aDefLines: TKMDefenceLines): Boolean;
begin
  Result := False;
  fOwner := aOwner;

  gAIFields.Eye.OwnerUpdate(aOwner); // Make sure that Eye is set to the right Owner (Old AI does not shift it)

  Result := ForwardFF();
  if Result then
    aDefLines := fBackwardFF.FindDefenceLines(aOwner);
end;


// Find best defensive polygons (for defensive positions)
function TForwardFF.FindDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt: Word; var aDefPosArr: TKMDefencePosArr; aMinDefeces: Boolean = True): Boolean;
begin
  Result := False;
  fOwner := aOwner;
  FirstLine := 0;

  gAIFields.Eye.OwnerUpdate(aOwner); // Make sure that Eye is set to the right Owner (Old AI does not shift it)

  Result := ForwardFF();
  if Result then
    aDefPosArr := fBackwardFF.FindDefensivePolygons(aOwner, aBaseCnt, FirstLine, BestDefLines, aMinDefeces, False);
end;




{ TBackwardFF }
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
                  + Byte(fDefInfo[aIdx].AllyInfluence > ALLY_INFLUENCE_LIMIT) * ALLY_INFLUENCE_PENALIZATION
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
        if (PolyArr[NearbyIdx].NearbyCount = 3) then
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
  Idx, NearbyIdx, Cnt: Word;
  I, K: Integer;
  Direction: TKMDirection;
  PolyArr: TPolygonArray;
begin
  Result := False;
  // Check and determine counts
  if (BestDefLines.Count = 0) then
    Exit
  else if aMinDefeces then
    aBaseCnt := Max(4,BestDefLines.Count)
  else
    aBaseCnt := Max(aBaseCnt,BestDefLines.Count);

  PolyArr := gAIFields.NavMesh.Polygons;
  // Prepare defensive FF - add all polygon is BestDefLines to queue
  NewQueue(fVisitedIdx + 1);
  aFirstLine := BestDefLines.Count;
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
      if (fDefInfo[ NearbyIdx ].Distance > fDefInfo[ Idx ].Distance) then
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
  while RemoveFromQueue(Idx) AND (Cnt < aBaseCnt) do
  begin
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
        if (Cnt < aFirstLine) then
          Weight := High(Word) // First line, max importance
        else
          Weight := (High(Word) shr 1) - fDefInfo[Idx].Distance + fDefInfo[Idx].EnemyInfluence;
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
  SetLength(DefPosArr, Cnt);
  Result := True;
end;


function TBackwardFF.FindDefenceLines(aOwner: TKMHandIndex): TKMDefenceLines;
begin
  fDefLinesRequired := True;
  BackwardFlood();
  Result := BestDefLines;
end;


function TBackwardFF.FindDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt, aFirstLine: Word; var aBestDefLines: TKMDefenceLines; aMinDefeces: Boolean; aDefLinesRequired: Boolean): TKMDefencePosArr;
begin
  fDefLinesRequired := aDefLinesRequired;
  BackwardFlood();
  FindDefencePos(aBaseCnt, aFirstLine, aMinDefeces);
  aBestDefLines := BestDefLines;
  Result := DefPosArr;
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
