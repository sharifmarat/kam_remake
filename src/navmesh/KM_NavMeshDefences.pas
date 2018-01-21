unit KM_NavMeshDefences;
{$I KaM_Remake.inc}
interface
uses
  Math, KM_Defaults, KM_CommonTypes, KM_CommonUtils,
  KM_ResHouses, KM_Houses, KM_Points, KM_PolySimplify,
  KM_NavMeshFloodFill;

const
  MAX_SCAN_DISTANCE_FROM_0_INFLUENCE = 200;
  MAX_LAYERS = 100;

  type
  TDefenceInfo = record
    Influence, EnemyInfluence, AllyInfluence: Byte;
    Distance: Word;
  end;
  TDefInfoArray = array of TDefenceInfo;

  TKMDefenceLine = record
    Polygon: Word;
    Nodes: array[0..1] of Word;
  end;
  TKMDefenceLines = record
    //EnemyInfluence, AlliedInfluence, OwnerInfluence: Byte;
    Count: Word;
    Lines: array of TKMDefenceLine;
  end;

  TKMDefencePosition = record
    Polygon, Weight: Word;
    DirPoint: TKMPointDir;
  end;
  TKMDefencePosArr = array of TKMDefencePosition;


  TBackwardFF = class(TNavMeshFloodFill)
  private
  protected
    fOwner: TKMHandIndex;
    fDefInfo: TDefInfoArray;

    fBestEvaluation: Single;

    function CanBeExpanded(const aIdx: Word): Boolean; override;
    procedure BackwardFlood();
    procedure EvaluateDefence(const aIdx: Word);
    function FindDefencePos(var aBaseCnt: Word; aMinDefeces: Boolean): Boolean;
  public
    D_INIT_ARR, D_INIT_FLOOD: TKMWordArray;
    BestDefLines: TKMDefenceLines;
    DefPosArr: TKMDefencePosArr;

    procedure UpdatePointers(var aDefInfo: TDefInfoArray; var aQueueArray: TPolygonsQueueArr);
    procedure NewQueue(aVisitedIdx: Byte);
    procedure AddPolygon(aIdx: Word);
    function FindDefenceLines(aOwner: TKMHandIndex): TKMDefenceLines;
    function FindDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt: Word; aMinDefeces: Boolean): TKMDefencePosArr;

    procedure DEBUG();
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
    D_INIT_FLOOD, D_FF_INIT_ARR, D_FF_INIT_FLOOD: TKMWordArray;
    BestDefLines: TKMDefenceLines;

    constructor Create(aSorted: Boolean = False); reintroduce;
    destructor Destroy(); override;

    function FindDefenceLines(aOwner: TKMHandIndex; var aDefLines: TKMDefenceLines): Boolean;
    function FindDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt: Word; var aDefPosArr: TKMDefencePosArr; aMinDefeces: Boolean = True): Boolean;

    procedure DEBUG();
  end;


const
  MAX_BACKWARD_DISTANCE = 50; // Maximal distance from influence where may be defences
  // Defences cannot be between 255 and this value (owner limit)
  ALLY_INFLUENCE_LIMIT = 100; // Defences cannot be between 255 and this value (ally limit)
   // Defences cannot be between 255 and this value (enemy limit)
  DISTANCE_INFLUENCE_LIMIT = 200;
  MINIMAL_DISTANCE = 10;
  POLYGON_CNT_PENALIZATION = 20;


  OWNER_INFLUENCE_LIMIT = 200;
  ENEMY_INFLUENCE_LIMIT = 100;

  MINIMAL_DEFENCE_DISTANCE = 20;
  MAXIMAL_DEFENCE_DISTANCE = 100;

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


procedure TForwardFF.MakeNewQueue();
begin
  fVisitedIdx := fVisitedIdx + 3; // There is 1 FF forward, 1 FF backward and 1 FF for positioning in 1 round

  if (Length(fQueueArray) < Length(gAIFields.NavMesh.Polygons)) then
  begin
    SetLength(fQueueArray, Length(gAIFields.NavMesh.Polygons));
    SetLength(fDefInfo, Length(gAIFields.NavMesh.Polygons));
    fBackwardFF.UpdatePointers(fDefInfo, fQueueArray);
    ClearVisitIdx();
  end;
  if (fVisitedIdx = High(Byte)-2) then
    ClearVisitIdx();

  fBackwardFF.NewQueue(fVisitedIdx+1);
end;


function TForwardFF.IsVisited(const aIdx: Word): Boolean;
begin
  Result := (fQueueArray[aIdx].Visited >= fVisitedIdx); // Arrays are common for TForwardFF and TBackwardFF so >= must be here
end;


function TForwardFF.CanBeExpanded(const aIdx: Word): Boolean;
begin
  Result := (gAIFields.NavMesh.Polygons[aIdx].NearbyCount <> 3) OR (fDefInfo[aIdx].EnemyInfluence < ENEMY_INFLUENCE_LIMIT);
  if not Result then
    fBackwardFF.AddPolygon(aIdx);
end;


procedure TForwardFF.MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint);
var
  Distance: Word;
begin

//EnemyInfluence := gAIFields.Influences.GetBestAllianceOwnership(fOwner, aIdx, at_Enemy);
//AlliedInfluence := gAIFields.Influences.GetBestAllianceOwnership(fOwner, aIdx, at_Ally);
//OwnerInfluence := gAIFields.Influences.OwnPoly[fOwner, aIdx];

  fDefInfo[aIdx].Influence := gAIFields.Influences.OwnPoly[fOwner, aIdx];
  Distance := aDistance; // Min(fDefInfo[aIdx].Influence shr 2, Distance);
  if (fDefInfo[aIdx].Influence > OWNER_INFLUENCE_LIMIT) then
    Distance := 0;
  inherited MarkAsVisited(aIdx, Distance, aPoint);

  if (gAIFields.NavMesh.Polygons[aIdx].NearbyCount = 3) then
  begin
    fDefInfo[aIdx].EnemyInfluence := gAIFields.Influences.GetBestAllianceOwnership(fOwner, aIdx, at_Enemy);
    //AllyInfluence := gAIFields.Influences.GetBestAllianceOwnership(fOwner, aIdx, at_Ally);
  end;
end;


function TForwardFF.ForwardFF(): Boolean;
var
  I: Integer;
  CityCenterPoints: TKMPointArray;
  StartPolygons: TKMWordArray;
begin
  Result := False;
  CityCenterPoints := gAIFields.Eye.GetCityCenterPoints(True);
  if (Length(CityCenterPoints) < 1) then
    Exit;
  SetLength(StartPolygons, Length(CityCenterPoints));
  for I := Low(StartPolygons) to High(StartPolygons) do
    StartPolygons[I] := gAIFields.NavMesh.Point2Polygon[ CityCenterPoints[I].Y,CityCenterPoints[I].X ];

  Result := FillPolygons(Length(StartPolygons), StartPolygons);
end;


function TForwardFF.FindDefenceLines(aOwner: TKMHandIndex; var aDefLines: TKMDefenceLines): Boolean;
begin
  Result := False;
  fOwner := aOwner;

  gAIFields.Eye.OwnerUpdate(aOwner); // DEBUG

  Result := ForwardFF();
  if Result then
    aDefLines := fBackwardFF.FindDefenceLines(aOwner);
end;


function TForwardFF.FindDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt: Word; var aDefPosArr: TKMDefencePosArr; aMinDefeces: Boolean = True): Boolean;
begin
  Result := False;
  fOwner := aOwner;

  gAIFields.Eye.OwnerUpdate(aOwner); // DEBUG

  Result := ForwardFF();
  if Result then
    aDefPosArr := fBackwardFF.FindDefensivePolygons(aOwner, aBaseCnt, aMinDefeces);
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
  fBestEvaluation := 1000000;
end;


procedure TBackwardFF.AddPolygon(aIdx: Word);
begin
  fQueueArray[aIdx].Visited := fVisitedIdx; // Polygon is marked from ForwardFF
  fDefInfo[aIdx].Distance := fQueueArray[aIdx].Distance; // Save distance for future calculation
  fQueueArray[aIdx].Distance := High(Word) - fQueueArray[aIdx].Distance; // NavMesh Flood fill class can sort only in increased order -> distances must be inverted when we want to start with the farthest
  InsertAndSort(aIdx);
end;


procedure TBackwardFF.EvaluateDefence(const aIdx: Word);
var
  PolyArr: TPolygonArray;

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

  procedure AddDefence(aDefIdx1: Word);
  var
    SecondIndice: Boolean;
    I,K, DefIdx2: Word;
  begin
    BestDefLines.Lines[ BestDefLines.Count ].Polygon := aDefIdx1;
    DefIdx2 := GetSecondDefencePolygon(aDefIdx1);
    SecondIndice := False;
    for I := 0 to 2 do
      for K := 0 to 2 do
        if (PolyArr[aDefIdx1].Indices[I] = PolyArr[DefIdx2].Indices[K]) then
        begin
          BestDefLines.Lines[ BestDefLines.Count ].Nodes[ Byte(SecondIndice) ] := PolyArr[aDefIdx1].Indices[I];
          SecondIndice := True; // It will switch index from 0 to 1
          break;
        end;
    if SecondIndice then // Make sure that this node exist
      Inc(BestDefLines.Count);
  end;

const
  POLYGON_CNT_PENALIZATION = 20;
var
  I, QueueIdx: Word;
  Evaluation: Single;
begin
  PolyArr := gAIFields.NavMesh.Polygons;

  Evaluation := fQueueCnt * POLYGON_CNT_PENALIZATION;
  QueueIdx := aIdx;
  for I := 0 to fQueueCnt do // aIdx is already taken from Queue so I must be from 0 to fQueueCnt!
  begin
    Evaluation := Evaluation + fDefInfo[aIdx].Distance;
    QueueIdx := fQueueArray[QueueIdx].Next;
  end;
  if (Evaluation < fBestEvaluation) then
  begin
    fBestEvaluation := Evaluation;
    BestDefLines.Count := 0; // Set defences to 0
    if (fQueueCnt >= Length(BestDefLines.Lines)) then
      SetLength(BestDefLines.Lines, fQueueCnt + 32);
    QueueIdx := aIdx;
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


procedure TBackwardFF.BackwardFlood();
var
  PolyArr: TPolygonArray;
  procedure CreateBorders();
  var
    I, K, QueueIdx, NearbyIdx: Word;
  begin
    QueueIdx := fStartQueue;
    for I := 1 to fQueueCnt do
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
          InsertAndSort(NearbyIdx)
          //InsertInQueue(NearbyIdx);
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


function TBackwardFF.FindDefencePos(var aBaseCnt: Word; aMinDefeces: Boolean): Boolean;
const
  INIT_WEIGHT = 10000;
var
  I, K, Idx, NearbyIdx, Cnt: Word;
  Direction: TKMDirection;
  PolyArr: TPolygonArray;
begin
  Result := False;
  if (BestDefLines.Count = 0) then
    Exit
  else if aMinDefeces then
    aBaseCnt := Max(4,BestDefLines.Count);

  PolyArr := gAIFields.NavMesh.Polygons;
  // Prepare defensive FF
  NewQueue(fVisitedIdx + 1);
  for I := 0 to BestDefLines.Count - 1 do
  begin
    Idx := BestDefLines.Lines[I].Polygon;
    MarkAsVisited(Idx, fDefInfo[ Idx ].Distance, PolyArr[Idx].CenterPoint);
    InsertInQueue( Idx );
    for K := 0 to PolyArr[Idx].NearbyCount-1 do // Mark every polygon before def line as visited so FF will not make defences here
    begin
      NearbyIdx := PolyArr[Idx].Nearby[K];
      if (fDefInfo[ NearbyIdx ].Influence < fDefInfo[ Idx ].Influence) then
        MarkAsVisited(NearbyIdx, fDefInfo[ NearbyIdx ].Distance, PolyArr[Idx].NearbyPoints[K]);
    end;
  end;

    //  Polygon, Weight: Word;
    //DirPoint: KMPointDir;
    //
    //TDefenceInfo = record
    //  Influence, EnemyInfluence, AllyInfluence: Byte;
    //  Distance: Word;      fDefInfo TKMPointDir
    //end;

  // Execute FF
  SetLength(DefPosArr, aBaseCnt);
  Cnt := 0;
  while RemoveFromQueue(Idx) AND (Cnt < aBaseCnt) do
  begin
    with DefPosArr[Cnt] do
    begin
      Polygon := Idx;
      Weight := fDefInfo[Idx].Distance + fDefInfo[Idx].EnemyInfluence;
      Direction := KMGetDirection( PolyArr[Idx].CenterPoint, fQueueArray[Idx].DistPoint );
      DirPoint := KMPointDir( fQueueArray[Idx].DistPoint, Direction );

     //KMGetDirection(KMPointF(fQueueArray[Idx].DistPoint), KMPerpendecular(fQueueArray[Idx].DistPoint, PolyArr[Idx].CenterPoint)
    end;
    if (Direction <> dir_NA) then
      Cnt := Cnt + 1;
    for I := 0 to PolyArr[Idx].NearbyCount-1 do
    begin
      NearbyIdx := PolyArr[Idx].Nearby[I];
      if not IsVisited(NearbyIdx) then
      begin
        MarkAsVisited(NearbyIdx, fDefInfo[ NearbyIdx ].Distance, PolyArr[Idx].NearbyPoints[I]);
        InsertInQueue(NearbyIdx);
      end;
    end;
  end;
  SetLength(DefPosArr, Cnt);
  Result := True;
end;


function TBackwardFF.FindDefenceLines(aOwner: TKMHandIndex): TKMDefenceLines;
begin
  BackwardFlood();
  Result := BestDefLines;
end;


function TBackwardFF.FindDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt: Word; aMinDefeces: Boolean): TKMDefencePosArr;
begin
  BackwardFlood();
  FindDefencePos(aBaseCnt, aMinDefeces);
  Result := DefPosArr;
end;




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




end.
