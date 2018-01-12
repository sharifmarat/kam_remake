unit KM_NavMeshDefences;
{$I KaM_Remake.inc}
interface
uses
  Math, KM_Defaults, KM_CommonTypes, KM_CommonUtils,
  KM_ResHouses, KM_Houses, KM_Points, KM_PolySimplify;

type
  TKMPolygonInfo = record
    FowardVisited, BackwardVisited: Boolean;
    EnemyInfluence, AlliedInfluence, OwnerInfluence: Byte;
    PreviousPolygon, NextPolygon, Distance, Layer: Word;
  end;
  TKMDefenceLine = record
    Polygon: Word;
    Nodes: array[0..1] of Word;
  end;
  TKMDefenceLines = record
    //EnemyInfluence, AlliedInfluence, OwnerInfluence: Byte;
    Count: Word;
    Lines: array of TKMDefenceLine;
  end;

  TNavMeshDefences = class
  private
    fStartQueue, fEndQueue, fQueueCnt: Word;
  protected
    fOwner: TKMHandIndex;
    fDefencePolygons: TKMWordArray;
    fPolygonInfoArray: array of TKMPolygonInfo;


    procedure MakeNewQueue(const aIdx: Word);
    procedure MakeNewSortedQueue(const aStartIdx, aCnt: Word);
    procedure ClearQueue();
    procedure InsertInQueue(const aIdx: Word);
    procedure InsertInSortedQueue(const aIdx: Word);
    procedure DeleteElementInQueue(const aIdx: Word);
    procedure SwapElements(const aIdx1, aIdx2: Word);
    function RemoveFromQueue(var aIdx: Word; aDescentOrder: Boolean = False): Boolean;
    function IsQueueEmpty(): Boolean;
    procedure FowardCycle(aStartPolygons: TKMWordArray);
    procedure BackwardCycle();
    function FindPolyBehindDefLine(var aBaseCnt: Word): Boolean;
  public
    fDEBUGDefences, fDEBUGBestEval, fDEBUGInitPolygons, fBorderPolygons: TKMWordArray;
    BestDefLines: TKMDefenceLines;
    DefPolygons: TKMWordArray;

    constructor Create();
    destructor Destroy(); override;

    function GetDefenceLines(aOwner: TKMHandIndex; var aDefLines: TKMDefenceLines): Boolean;
    function GetDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt: Word; var aDefPolygons: TKMWordArray): Boolean;
  end;

const
  MAX_BACKWARD_DISTANCE = 50; // Maximal distance from influence where may be defences
  OWNER_INFLUENCE_LIMIT = 255;// Defences cannot be between 255 and this value (owner limit)
  ALLY_INFLUENCE_LIMIT = 100; // Defences cannot be between 255 and this value (ally limit)
  ENEMY_INFLUENCE_LIMIT = 50; // Defences cannot be between 255 and this value (enemy limit)
  DISTANCE_INFLUENCE_LIMIT = 200;
  MINIMAL_DISTANCE = 10;
  POLYGON_CNT_PENALIZATION = 20;


implementation
uses
   KM_AIFields, KM_NavMesh, KM_HandsCollection;


{ TNavMeshPathFinding }
constructor TNavMeshDefences.Create();
begin
  ClearQueue();
  inherited;
end;


destructor TNavMeshDefences.Destroy();
begin
  inherited;
end;


procedure TNavMeshDefences.MakeNewQueue(const aIdx: Word);
begin
  fStartQueue := aIdx;
  fEndQueue := aIdx;
  fQueueCnt := 1;
end;


procedure TNavMeshDefences.MakeNewSortedQueue(const aStartIdx, aCnt: Word);
var
  I, Idx, POM: Word;
begin
  Idx := aStartIdx;
  for I := 1 to aCnt do
  begin
    POM := fPolygonInfoArray[Idx].NextPolygon;
    InsertInSortedQueue(Idx);
    Idx := POM;
  end;
end;


procedure TNavMeshDefences.ClearQueue();
begin
  fQueueCnt := 0;
end;


procedure TNavMeshDefences.InsertInQueue(const aIdx: Word);
begin
  if IsQueueEmpty() then
    MakeNewQueue(aIdx)
  else
  begin
    fPolygonInfoArray[fEndQueue].NextPolygon := aIdx;
    fPolygonInfoArray[aIdx].PreviousPolygon := fEndQueue;
    fEndQueue := aIdx;
    fQueueCnt := fQueueCnt + 1;
  end;
end;


procedure TNavMeshDefences.InsertInSortedQueue(const aIdx: Word);
var
  I, Idx: Word;
begin
  if IsQueueEmpty() then
    MakeNewQueue(aIdx)
  else
  begin
    Idx := fStartQueue;
    I := 0;
    while (I < fQueueCnt) do
    begin
      if (fPolygonInfoArray[aIdx].Distance > fPolygonInfoArray[Idx].Distance) then
        break;
      Idx := fPolygonInfoArray[Idx].NextPolygon;
      I := I + 1;
    end;

    if (I = fQueueCnt) then
    begin
      fPolygonInfoArray[aIdx].PreviousPolygon := fEndQueue;
      fPolygonInfoArray[fEndQueue].NextPolygon := aIdx;
      fEndQueue := aIdx;
    end
    else if (I = 0) then
    begin
      fPolygonInfoArray[aIdx].NextPolygon := fStartQueue;
      fPolygonInfoArray[fStartQueue].PreviousPolygon := aIdx;
      fStartQueue := aIdx;
    end
    else
    begin
      fPolygonInfoArray[aIdx].PreviousPolygon := fPolygonInfoArray[Idx].PreviousPolygon;
      fPolygonInfoArray[aIdx].NextPolygon := Idx;
      fPolygonInfoArray[ fPolygonInfoArray[Idx].PreviousPolygon ].NextPolygon := aIdx;
      fPolygonInfoArray[Idx].PreviousPolygon := aIdx;
    end;

    fQueueCnt := fQueueCnt + 1;
  end;
end;


procedure TNavMeshDefences.DeleteElementInQueue(const aIdx: Word);
begin
  if (aIdx = fStartQueue) then
    fStartQueue := fPolygonInfoArray[aIdx].NextPolygon
  else
    fPolygonInfoArray[ fPolygonInfoArray[aIdx].PreviousPolygon ].NextPolygon := fPolygonInfoArray[aIdx].NextPolygon;

  if (aIdx = fEndQueue) then
    fEndQueue := fPolygonInfoArray[aIdx].PreviousPolygon
  else
    fPolygonInfoArray[ fPolygonInfoArray[aIdx].NextPolygon ].PreviousPolygon := fPolygonInfoArray[aIdx].PreviousPolygon;

  fQueueCnt := fQueueCnt - 1;
end;


procedure TNavMeshDefences.SwapElements(const aIdx1, aIdx2: Word);
begin
  if      (aIdx1 = fStartQueue) then fStartQueue := aIdx2
  else if (aIdx2 = fStartQueue) then fStartQueue := aIdx1;

  if      (aIdx1 = fEndQueue) then fEndQueue := aIdx2
  else if (aIdx2 = fEndQueue) then fEndQueue := aIdx1;

  KMSwapInt(fPolygonInfoArray[ fPolygonInfoArray[aIdx1].NextPolygon ].PreviousPolygon,
           fPolygonInfoArray[ fPolygonInfoArray[aIdx2].NextPolygon ].PreviousPolygon);

  KMSwapInt(fPolygonInfoArray[ fPolygonInfoArray[aIdx1].PreviousPolygon ].NextPolygon,
           fPolygonInfoArray[ fPolygonInfoArray[aIdx2].PreviousPolygon ].NextPolygon);

  KMSwapInt(fPolygonInfoArray[ aIdx1 ].PreviousPolygon,
           fPolygonInfoArray[ aIdx2 ].PreviousPolygon);

  KMSwapInt(fPolygonInfoArray[ aIdx1 ].NextPolygon,
           fPolygonInfoArray[ aIdx2 ].NextPolygon);
end;


function TNavMeshDefences.RemoveFromQueue(var aIdx: Word; aDescentOrder: Boolean = False): Boolean;
begin
  Result := not IsQueueEmpty;
  if Result then
  begin
    if aDescentOrder then
    begin
      aIdx := fEndQueue;
      fEndQueue := fPolygonInfoArray[fEndQueue].PreviousPolygon;
    end
    else
    begin
      aIdx := fStartQueue;
      fStartQueue := fPolygonInfoArray[fStartQueue].NextPolygon;
    end;
    fQueueCnt := fQueueCnt - 1;
  end;
end;


function TNavMeshDefences.IsQueueEmpty(): Boolean;
begin
  Result := fQueueCnt = 0;
end;


procedure TNavMeshDefences.FowardCycle(aStartPolygons: TKMWordArray);
  // Create backward queue in foward cycle
  // (it save time, those elements are not used in foward cycle so variable Next/Previous Polygon can be already used)
  procedure InsertInBackwardQueue(const aIdx: Word; var aEndQueue, aBackwardCnt: Word);
  begin
    fPolygonInfoArray[aEndQueue].NextPolygon := aIdx;
    fPolygonInfoArray[aIdx].PreviousPolygon := aEndQueue;
    aEndQueue := aIdx;
    aBackwardCnt := aBackwardCnt + 1;
  end;

  function NewElement(aIdx: Word; aFather: Word): Word;
  var
    CenterPoint: TKMPoint;
  begin
    Result := aIdx;
    CenterPoint := gAIFields.NavMesh.Polygons[aIdx].CenterPoint;
    with fPolygonInfoArray[aIdx] do
    begin
      FowardVisited := True;
      Distance := 0;
      EnemyInfluence := gAIFields.Influences.GetBestAllianceOwnership(fOwner, aIdx, at_Enemy);
      AlliedInfluence := gAIFields.Influences.GetBestAllianceOwnership(fOwner, aIdx, at_Ally);
      OwnerInfluence := gAIFields.Influences.OwnPoly[fOwner, aIdx];
      if (OwnerInfluence < DISTANCE_INFLUENCE_LIMIT) AND (aFather <> High(Word)) then
        Distance := fPolygonInfoArray[aFather].Distance + KMDistanceAbs(CenterPoint, gAIFields.NavMesh.Polygons[aFather].CenterPoint);
    end;
  end;

var
  I, Idx, BackwardStart, BackwardEnd, BackwardCnt: Word;
  PolyArr: TPolygonArray;
begin
  PolyArr := gAIFields.NavMesh.Polygons;

  if (Length(fPolygonInfoArray) <> Length(PolyArr)) then
    SetLength(fPolygonInfoArray, Length(PolyArr));
  for I := Low(fPolygonInfoArray) to High(fPolygonInfoArray) do
  begin
    fPolygonInfoArray[I].FowardVisited := False;
    fPolygonInfoArray[I].BackwardVisited := False;
    fPolygonInfoArray[I].Layer := 0;
    fPolygonInfoArray[I].Distance := 0;
  end;

  for I := Low(aStartPolygons) to High(aStartPolygons) do
    InsertInQueue(  NewElement( aStartPolygons[I], High(Word) )  );

  BackwardCnt := 0;
  // Empty queue
  while RemoveFromQueue(Idx, True) do
  begin
    if (fPolygonInfoArray[Idx].EnemyInfluence > ENEMY_INFLUENCE_LIMIT) then
    begin
      if (BackwardCnt = 0) then
      begin
        BackwardStart := Idx;
        BackwardEnd := Idx;
      end;
      InsertInBackwardQueue(Idx, BackwardEnd, BackwardCnt);
      continue;
    end;

    for I := 0 to PolyArr[Idx].NearbyCount-1 do
      if not fPolygonInfoArray[ PolyArr[Idx].Nearby[I] ].FowardVisited then
        InsertInSortedQueue(  NewElement( PolyArr[Idx].Nearby[I], Idx )  );
  end;

  if (BackwardCnt > 0) then
    MakeNewSortedQueue(BackwardStart, BackwardCnt);
  BackwardCycle();
end;


procedure TNavMeshDefences.BackwardCycle();
var
  PolyArr: TPolygonArray;
  DefLines: TKMDefenceLines;

  function MoveBackward(aIdx, aLayer: Word; AllowBranching: Boolean = False; OnlyCheck: Boolean = False): Boolean;
  var
    I, CanBeVisitedCnt, SameLayersNearbyCnt,newIdx: Word;
  begin
    fPolygonInfoArray[aIdx].Layer := aLayer;
    CanBeVisitedCnt := 0;
    SameLayersNearbyCnt := 0;
    for I := 0 to PolyArr[aIdx].NearbyCount-1 do
    begin
      newIdx := PolyArr[aIdx].Nearby[I];
      CanBeVisitedCnt := CanBeVisitedCnt + Byte(not fPolygonInfoArray[newIdx].BackwardVisited);
      SameLayersNearbyCnt := SameLayersNearbyCnt + Byte(fPolygonInfoArray[newIdx].Layer = aLayer); // For case of parallel branches
    end;

    if (SameLayersNearbyCnt <= 1) then // Nearby layer from parent polygon or missing (dont allow to expand in case that there are 2 and more actual layers)
      if (CanBeVisitedCnt = 1) OR AllowBranching then // Another accessible polygon or polygons in case of parallel branches
      begin
        for I := 0 to PolyArr[aIdx].NearbyCount-1 do
        begin
          newIdx := PolyArr[aIdx].Nearby[I];
          if (not fPolygonInfoArray[newIdx].BackwardVisited) then
          begin
            fPolygonInfoArray[newIdx].BackwardVisited := True;
            fPolygonInfoArray[newIdx].Layer := aLayer;
            MoveBackward(newIdx, aLayer);
          end;
        end;
        CanBeVisitedCnt := 0;
      end;
    // Add new polygon in case that we can visit more than 1 polygon or
    if not OnlyCheck AND not AllowBranching
      AND (  (CanBeVisitedCnt > 1) OR ( (CanBeVisitedCnt = 1) AND (SameLayersNearbyCnt = 2) )  ) then
      InsertInSortedQueue(aIdx);
    Result := (CanBeVisitedCnt = 0);
  end;

  function BorderPolygon(var aIdx, aBorderIdx: Word): Boolean;
  var
    I, CanBeVisitedIdx, CannotBeVisitedIdx, CanBeVisitedCnt, CannotBeVisitedCnt: Word;
  begin
    CannotBeVisitedCnt := 0;
    CanBeVisitedCnt := 0;
    for I := 0 to PolyArr[aIdx].NearbyCount-1 do
      if fPolygonInfoArray[ PolyArr[aIdx].Nearby[I] ].BackwardVisited then
      begin
        Inc(CannotBeVisitedCnt);
        CannotBeVisitedIdx := PolyArr[aIdx].Nearby[I];
      end
      else
      begin
        Inc(CanBeVisitedCnt);
        CanBeVisitedIdx := PolyArr[aIdx].Nearby[I];
      end;
    if (CannotBeVisitedCnt > CanBeVisitedCnt) then
      aBorderIdx := CanBeVisitedIdx
    else
      aBorderIdx := CannotBeVisitedIdx;
    Result := (CanBeVisitedCnt > 0);
  end;

  procedure AddDefence(const aPolygon1, aPolygon2: Word);
  var
    SecondIndice: Boolean;
    I,K: Word;
  begin
    if (BestDefLines.Count >= Length(BestDefLines.Lines)) then
      SetLength(BestDefLines.Lines, BestDefLines.Count + 32);
    BestDefLines.Lines[ BestDefLines.Count ].Polygon := aPolygon1;
    SecondIndice := False;
    for I := 0 to 2 do
      for K := 0 to 2 do
        if (PolyArr[aPolygon1].Indices[I] = PolyArr[aPolygon2].Indices[K]) then
        begin
          BestDefLines.Lines[ BestDefLines.Count ].Nodes[ Byte(SecondIndice) ] := PolyArr[aPolygon1].Indices[I];
          SecondIndice := True; // It will switch index from 0 to 1
          break;
        end;
    if SecondIndice then
      Inc(BestDefLines.Count);
  end;

var
  I, K, Idx, newIdx, DebugIdx, DebugIdx2, DebugIdxInitPolygons, Layer, CurrentLabelCnt, BorderIdx, BorderPolygonCnt, Overflow: Word;
  TempResArr: TKMWordArray;
  TempEval, Evaluation: Single;
  TempArrIdx: Word;
begin
  if IsQueueEmpty then
    Exit;

  PolyArr := gAIFields.NavMesh.Polygons;
  //for I := 0 to PolyArr[Idx].NearbyCount-1 do
  //  if not IsFowardVisited(PolyArr[Idx].Nearby[I]) then
  SetLength(fDefencePolygons, 100000);
  SetLength(fDEBUGDefences, 100000);
  SetLength(fDEBUGBestEval, 100000);
  SetLength(fDEBUGInitPolygons, 100000);
  SetLength(TempResArr, 100000);

  DebugIdx := 0;
  DebugIdx2 := 0;
  DebugIdxInitPolygons := 0;




  Layer := 1;
  Idx := fStartQueue;
  for I := 1 to fQueueCnt do
  begin
    // Mark as visited actual polygon
    fPolygonInfoArray[Idx].BackwardVisited := True;
    fPolygonInfoArray[Idx].Layer := Layer;

    // Mark as visited polygons outside of the scaned area
    for newIdx := 0 to PolyArr[Idx].NearbyCount-1 do
      with fPolygonInfoArray[ PolyArr[Idx].Nearby[newIdx] ] do
        if not FowardVisited then
          BackwardVisited := True;


    fDEBUGInitPolygons[DebugIdxInitPolygons] := Idx;
    DebugIdxInitPolygons := DebugIdxInitPolygons + 1;


    Idx := fPolygonInfoArray[Idx].NextPolygon;
  end;



  //{
  BorderPolygonCnt := 0;
  Overflow := 0;
  Evaluation := 1000000;
  BestDefLines.Count := 0;
  while not IsQueueEmpty AND (Overflow < 500) do // In 255*255 max 200 interactions
  begin
    if   (fPolygonInfoArray[fStartQueue].Distance < MINIMAL_DISTANCE)
      OR (fPolygonInfoArray[fStartQueue].Distance < MINIMAL_DISTANCE) then
      Break;

    Overflow := Overflow + 1;
    // Try expand polygon in 1 direction (in case that there are 2 ways do nothing)
    Idx := fStartQueue;
    CurrentLabelCnt := fQueueCnt;
    for K := 1 to CurrentLabelCnt do
    begin
      if    (fPolygonInfoArray[Idx].AlliedInfluence < ALLY_INFLUENCE_LIMIT)
        AND (fPolygonInfoArray[Idx].OwnerInfluence < OWNER_INFLUENCE_LIMIT) then
        if MoveBackward(Idx,Layer,False,True) then
          DeleteElementInQueue(Idx);
      Idx := fPolygonInfoArray[Idx].NextPolygon;
    end;

    // Find border poylgons and evaluate them (in case that we are close to the city)
    if (fPolygonInfoArray[fStartQueue].Distance < MAX_BACKWARD_DISTANCE) then
    begin
      TempEval := 0;
      TempArrIdx := 0;
      Idx := fStartQueue;
      CurrentLabelCnt := fQueueCnt;
      for K := 1 to CurrentLabelCnt do
      begin
        if BorderPolygon(Idx, BorderIdx) then
        begin
          TempEval := TempEval + fPolygonInfoArray[Idx].Distance;
          TempResArr[TempArrIdx] := BorderIdx;
          Inc(TempArrIdx);
        end
        else
          DeleteElementInQueue(Idx);
        Idx := fPolygonInfoArray[Idx].NextPolygon;
      end;
      if (TempArrIdx < 1) then
        break;

      // Evaluation of actual layer
      TempEval := TempEval + fQueueCnt*POLYGON_CNT_PENALIZATION;
      if (TempEval <= Evaluation) then
      begin
        BestDefLines.Count := 0; // Set defences to 0
        Evaluation := TempEval;
        Idx := fStartQueue;
        for K := 0 to TempArrIdx-1 do
        begin
          AddDefence(Idx, TempResArr[K]);
          Idx := fPolygonInfoArray[Idx].NextPolygon;
        end;
      end;
    end;

    // Start parallel backward cycle (in case that we are away from our / ally city)
    while RemoveFromQueue(Idx) do
    begin
      if   (fPolygonInfoArray[Idx].AlliedInfluence > ALLY_INFLUENCE_LIMIT)
        OR (fPolygonInfoArray[Idx].OwnerInfluence > OWNER_INFLUENCE_LIMIT) then
      begin
        if (BorderPolygonCnt >= Length(fBorderPolygons)) then
          SetLength(fBorderPolygons, Length(fBorderPolygons) + 32);
        fBorderPolygons[BorderPolygonCnt] := Idx;
        Inc(BorderPolygonCnt);
      end
      else
      begin
        Inc(Layer);
        MoveBackward(Idx, Layer, True);
        break;
      end;
    end;

    Idx := fStartQueue;
    for K := 1 to fQueueCnt do
    begin
      fDEBUGDefences[DebugIdx] := Idx;
      Inc(DebugIdx);
      Idx := fPolygonInfoArray[Idx].NextPolygon;
    end;

  end;
  //}

  SetLength(fBorderPolygons, BorderPolygonCnt);
  SetLength(fDEBUGDefences, DebugIdx);
  SetLength(fDEBUGBestEval, DebugIdx2);
  SetLength(fDEBUGInitPolygons, DebugIdxInitPolygons);
  SetLength(fDefencePolygons, 1);
end;


function TNavMeshDefences.FindPolyBehindDefLine(var aBaseCnt: Word): Boolean;
const
  MIN_DEFENCES = 20;
var
  I, K, Idx, NewIdx, Cnt: Word;
  PolyArr: TPolygonArray;
begin
  Result := False;
  if (BestDefLines.Count = 0) then
    Exit;

  PolyArr := gAIFields.NavMesh.Polygons;
  ClearQueue(); // Make sure that we start new queue
  for I := 0 to BestDefLines.Count - 1 do
  begin
    Idx := BestDefLines.Lines[I].Polygon;
    InsertInQueue( Idx );
    // Mark with zero layer border polygons before defence line
    for K := 0 to PolyArr[Idx].NearbyCount-1 do
    begin
      NewIdx := PolyArr[Idx].Nearby[K];
      if (fPolygonInfoArray[ NewIdx ].Layer < fPolygonInfoArray[ Idx ].Layer) then
        fPolygonInfoArray[ NewIdx ].Layer := 0;
    end;
    fPolygonInfoArray[ Idx ].Layer := 0;
  end;
  aBaseCnt := fQueueCnt;

  SetLength(DefPolygons, MIN_DEFENCES);
  Cnt := 0;
  while RemoveFromQueue(Idx) AND (Cnt < MIN_DEFENCES) do
  begin
    DefPolygons[Cnt] := Idx;
    Cnt := Cnt + 1;
    for I := 0 to PolyArr[Idx].NearbyCount-1 do
    begin
      NewIdx := PolyArr[Idx].Nearby[I];
      if (fPolygonInfoArray[ NewIdx ].Layer > 0) then
      begin
        fPolygonInfoArray[ NewIdx ].Layer := 0;
        InsertInQueue( NewIdx );
      end;
    end;
  end;
  SetLength(DefPolygons, Cnt);
  Result := True;
end;


function TNavMeshDefences.GetDefenceLines(aOwner: TKMHandIndex; var aDefLines: TKMDefenceLines): Boolean;
var
  I: Integer;
  CityCenterPoints: TKMPointArray;
  StartPolygons: TKMWordArray;
begin
  Result := False;
  fOwner := aOwner;

  CityCenterPoints := gAIFields.Eye.GetCityCenterPoints(True);
  if (Length(CityCenterPoints) < 1) then
    Exit;
  SetLength(StartPolygons, Length(CityCenterPoints));
  for I := Low(StartPolygons) to High(StartPolygons) do
    StartPolygons[I] := gAIFields.NavMesh.FindClosestPolygon( CityCenterPoints[I] );

  ClearQueue();
  FowardCycle(StartPolygons);

  aDefLines := BestDefLines;
  Result := (BestDefLines.Count > 0);
end;


function TNavMeshDefences.GetDefensivePolygons(aOwner: TKMHandIndex; var aBaseCnt: Word; var aDefPolygons: TKMWordArray): Boolean;
begin
  Result := GetDefenceLines(aOwner, BestDefLines) AND FindPolyBehindDefLine(aBaseCnt);
  aDefPolygons := DefPolygons;
end;


end.

