unit KM_NavMeshPathFinding;
{$I KaM_Remake.inc}
interface
uses
  Math, KM_CommonTypes, KM_Points,
  BinaryHeap, KM_NavMesh;


type
  TNavMeshNode = class
    Idx,CostTo,Estim: Word; // Index of navMeshNode in navmesh, cost to this navMeshNode, estimated cost from navMeshNode to end
    Point: TKMPoint; // Point in navMeshNode of navmesh (each triangle [created by points in navMeshNode] have its point -> multiple poits will create path)
    Parent: TNavMeshNode; // Parent navMeshNode
  end;

  TNavMeshPathFinding = class
  private
    fStart, fEnd: Word;
    fHeap: TBinaryHeap;
    fMinN: TNavMeshNode;
    fUsedNodes: array of TNavMeshNode;

    function HeapCmp(A,B: Pointer): Boolean;
    procedure Flush(aDestroy: Boolean = False);
  protected
    function GetNodeAt(aIdx: Word): TNavMeshNode;
    function MovementCost(aFrom, aTo: Word; var aSPoint, aEPoint: TKMPoint): Word;
    function EstimateToFinish(aIdx: Word): Word;

    function MakeRoute(): Boolean;
    procedure ReturnRoute(out aRouteArray: TKMWordArray; out aRoutePointArray: TKMPointArray);
  public
    constructor Create();
    destructor Destroy(); override;

    function Route_Make(aStart, aEnd: TKMPoint; out aRouteArray: TKMWordArray; out aRoutePointArray: TKMPointArray): Boolean;
  end;


implementation
uses
   KM_AIFields;


{ TNavMeshPathFinding }
constructor TNavMeshPathFinding.Create;
begin
  inherited;
  fHeap := TBinaryHeap.Create(High(Word));
  fHeap.Cmp := HeapCmp;
end;


destructor TNavMeshPathFinding.Destroy;
begin
  Flush(true);
  fHeap.Free;
  inherited;
end;


function TNavMeshPathFinding.HeapCmp(A,B: Pointer): Boolean;
begin
  if (A = nil) then
    Result := True
  else
    Result := (B = nil) OR (TNavMeshNode(A).Estim + TNavMeshNode(A).CostTo < TNavMeshNode(B).Estim + TNavMeshNode(B).CostTo);
end;


procedure TNavMeshPathFinding.Flush(aDestroy: Boolean = False);
var
  I: Integer;
begin
  if not aDestroy AND (Length(fUsedNodes) <> Length(gAIFields.NavMesh.Polygons)) then
    SetLength(fUsedNodes, Length(gAIFields.NavMesh.Polygons));
  for I := Length(fUsedNodes)-1 downto 0 do
  begin
    fUsedNodes[I].Free;
    fUsedNodes[I] := nil;
  end;
end;


function TNavMeshPathFinding.GetNodeAt(aIdx: Word): TNavMeshNode;
begin
  if (fUsedNodes[aIdx] = nil) then
  begin
    fUsedNodes[aIdx] := TNavMeshNode.Create;
    fUsedNodes[aIdx].Idx := aIdx;
    fUsedNodes[aIdx].Estim := EstimateToFinish(aIdx);
    //fUsedNodes[aIdx].CostTo := 0;
    //fUsedNodes[aIdx].Parent := nil;
    //fUsedNodes[aIdx].Point := TKMPOINT_ZERO;
  end;
  Result := fUsedNodes[aIdx];
end;


function TNavMeshPathFinding.MovementCost(aFrom, aTo: Word; var aSPoint, aEPoint: TKMPoint): Word;
var
  DX,DY, Output: Word;
begin
  Output := 0;
  //Do not add extra cost if the tile is the target, as it can cause a longer route to be chosen
  if (aTo <> fEnd) then
  begin
    DX := Abs(aSPoint.X - aEPoint.X);
    DY := Abs(aSPoint.Y - aEPoint.Y);
    if (DX > DY) then
      Output := (DX shl 3) + (DY shl 2)
    else
      Output := (DY shl 3) + (DX shl 2);
  end;
  // Output := KMDistanceAbs(SPoint, EPoint); // in test map it throws +1 cycle (109 vs 110 interactions
  Result := Output;
end;


function TNavMeshPathFinding.EstimateToFinish(aIdx: Word): Word;
var
  DX, DY, Output: Word;
  SPoint, EPoint: TKMPoint;
begin
  SPoint := gAIFields.NavMesh.Polygons[aIdx].CenterPoint;
  EPoint := gAIFields.NavMesh.Polygons[fEnd].CenterPoint;
  // Output := KMDistanceAbs(SPoint, EPoint); // in test map it throws +1 cycle (109 vs 110 interactions)
  DX := Abs(SPoint.X - EPoint.X);
  DY := Abs(SPoint.Y - EPoint.Y);
  if (DX > DY) then
    Output := (DX shl 3) + (DY shl 2)
  else
    Output := (DY shl 3) + (DX shl 2);
  Result := Output;
end;


function TNavMeshPathFinding.MakeRoute(): Boolean;
const
  c_closed = 65535; // High(Word)
var
  N: TNavMeshNode;
  I, Idx, NewCost: Word;
  POMPoint: TKMPoint;
  PolyArr: TPolygonArray;
begin
  Flush();
  PolyArr := gAIFields.NavMesh.Polygons;

  //Initialize first element
  N := GetNodeAt(fStart);
  N.Parent := nil;

  //Seed
  fMinN := N;
  while (fMinN <> nil) and (fMinN.Idx <> fEnd) do
  begin
    fMinN.Estim := c_closed;
    //Check all surrounding polygons and issue costs to them
    for I := 0 to PolyArr[fMinN.Idx].NearbyCount-1 do
    begin
      Idx := PolyArr[ fMinN.Idx ].Nearby[I];
      // New polygon
      if (fUsedNodes[Idx] = nil) then
      begin
        N := GetNodeAt(Idx);
        N.Parent := fMinN;
        N.Point := PolyArr[ fMinN.Idx ].NearbyPoints[I];
        N.CostTo := fMinN.CostTo + MovementCost(fMinN.Idx, Idx, fMinN.Point, N.Point);
        fHeap.Push(N);
      end
      // Visited polygon (recalculate estimation and actualize better result)
      else if (fUsedNodes[Idx].Estim <> c_closed) then
      begin
        POMPoint := PolyArr[ fMinN.Idx ].NearbyPoints[I];
        NewCost := fMinN.CostTo + MovementCost(fMinN.Idx, Idx, fMinN.Point, POMPoint);
        if (NewCost < fUsedNodes[Idx].CostTo) then
        begin
          fUsedNodes[Idx].Parent := fMinN;
          fUsedNodes[Idx].CostTo := NewCost;
          fUsedNodes[Idx].Point := POMPoint;
        end;
      end;
    end;
    //Find next cell with least (Estim+CostTo)
    if fHeap.IsEmpty then
      Break;
    fMinN := fHeap.Pop;
  end;
  //Route found, no longer need the lookups
  fHeap.Clear;
  Result := (fMinN.Idx = fEnd);
end;


procedure TNavMeshPathFinding.ReturnRoute(out aRouteArray: TKMWordArray; out aRoutePointArray: TKMPointArray);
const
  USED_PENALIZATION = 40;
var
  Count: Word;
  N: TNavMeshNode;
  Output: TKMWordArray;
begin
  N := fMinN;
  Count := 0;
  while (N <> nil) do
  begin
    N := N.Parent;
    Inc(Count,1);
  end;

  SetLength(aRoutePointArray, Count+1);
  aRoutePointArray[0] := gAIFields.NavMesh.Polygons[fMinN.Idx].CenterPoint;

  SetLength(Output, Count);
  N := fMinN;
  Count := 0;
  while (N <> nil) do
  begin
    aRoutePointArray[Count+1] := N.Point;
    Output[Count] := N.Idx;
    N := N.Parent;
    Inc(Count,1);
  end;
  aRoutePointArray[Count] := gAIFields.NavMesh.Polygons[ Output[Count-1] ].CenterPoint;
end;


function TNavMeshPathFinding.Route_Make(aStart, aEnd: TKMPoint; out aRouteArray: TKMWordArray; out aRoutePointArray: TKMPointArray): Boolean;
var Output: Boolean;
begin
  Result := False;
  fStart := gAIFields.NavMesh.FindClosestPolygon(aStart);
  fEnd := gAIFields.NavMesh.FindClosestPolygon(aEnd);
  if (fStart = High(Word)) OR (fEnd = High(Word)) then  // Non-Existing polygon
    Exit;
  Output := MakeRoute();
  if Output then
  begin
    ReturnRoute(aRouteArray, aRoutePointArray);
    aRoutePointArray[0] := aEnd;
  end;
  Result := Output;
end;


end.
