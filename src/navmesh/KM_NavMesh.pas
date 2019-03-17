unit KM_NavMesh;
{$I KaM_Remake.inc}
interface
uses
  KM_PolySimplify,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points, Contnrs,
  KM_NavMeshDefences,
  KromUtils, KM_CommonUtils,
  KM_NavMeshGenerator, KM_NavMeshFloodPositioning, KM_NavMeshPathFinding;// TimeGet


type

  //NavMesh is used to acess the map on a higher level than tiles
  //terrain is represented as a mesh interconnected polygons
  TKMNavMesh = class
  private
    fMapX, fMapY: Word;               // Limits of arrays
    fNodeCount, fPolyCount: Integer;  // Thresholds
    fNodes: TKMPointArray;            // Nodes
    fPolygons: TPolygonArray;         // Polygons
    fPoint2PolygonArr: TKMWordArray;  // KMPoint -> Polygon index
    //fPolygon2PointArr: TKMPointArray; // Polygon index -> KMPoint

    fDefences: TForwardFF; //Defences class
    fPathfinding: TNavMeshPathFinding; // NavMesh Pathfinding
    fPositioning: TNavMeshFloodPositioning; // NavMesh Positioning

    fNavMeshGenerator: TKMNavMeshGenerator; // NavMesh generator

    //Building the navmesh from terrain
    procedure FindClosestPolygon();
    procedure TieUpTilesWithPolygons();
    procedure TieUpPolygonsWithTiles();

    function GetPolygonFromPoint(const aY,aX: Integer): Word;
    function GetPolygonFromKMPoint(const aPoint: TKMPoint): Word;

  public
    constructor Create();
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property Point2Polygon[const aY,aX: Integer]: Word read GetPolygonFromPoint;
    property KMPoint2Polygon[const aPoint: TKMPoint]: Word read GetPolygonFromKMPoint;
    //property Polygon2Point: TKMPointArray read fPolygon2PointArr;
    property Polygons: TPolygonArray read fPolygons;
    property Nodes: TKMPointArray read fNodes;
    property Defences: TForwardFF read fDefences write fDefences;
    property Pathfinding: TNavMeshPathFinding read fPathfinding write fPathfinding;
    property Positioning: TNavMeshFloodPositioning read fPositioning write fPositioning;

    procedure AfterMissionInit();

    procedure UpdateState(aTick: Cardinal);
    procedure Paint(const aRect: TKMRect);
  end;


implementation
uses
  SysUtils, Math, Delaunay,
  KM_Terrain, KM_HandsCollection, KM_RenderAux, KM_Outline,
  Dialogs;


{ TKMNavMesh }
constructor TKMNavMesh.Create();
begin
  inherited Create;

  fNavMeshGenerator := TKMNavMeshGenerator.Create();
  fDefences := TForwardFF.Create(True);
  fPathfinding := TNavMeshPathFinding.Create();
  fPositioning := TNavMeshFloodPositioning.Create();
end;


destructor TKMNavMesh.Destroy();
begin
  fDefences.Free;
  fPathfinding.Free;
  fPositioning.Free;
  fNavMeshGenerator.Free;
  inherited;
end;


procedure TKMNavMesh.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
begin
  fNavMeshGenerator.Save(SaveStream);
  SaveStream.WriteA('NavMesh');
  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);

  SaveStream.Write(fNodeCount);
  SaveStream.Write(fNodes[0], SizeOf(fNodes[0]) * fNodeCount);

  SaveStream.Write(fPolyCount);
  SaveStream.Write(fPolygons[0], SizeOf(fPolygons[0]) * fPolyCount);
  // Maybe this code will save work in future
  //for I := 0 to fPolyCount - 1 do
  //begin
  //  SaveStream.Write(fPolygons[I].CenterPoint);
  //  SaveStream.Write(fPolygons[I].Indices, SizeOf(fPolygons[I].Indices));
  //  SaveStream.Write(fPolygons[I].NearbyCount);
  //  SaveStream.Write(fPolygons[I].Poly2PointStart);
  //  SaveStream.Write(fPolygons[I].Poly2PointCnt);
  //  SaveStream.Write(fPolygons[I].Nearby, SizeOf(fPolygons[I].Nearby));
  //  SaveStream.Write(fPolygons[I].NearbyPoints, SizeOf(fPolygons[I].NearbyPoints));
  //end;

  I := Length(fPoint2PolygonArr);
  SaveStream.Write( I );
  SaveStream.Write(fPoint2PolygonArr[0], SizeOf(fPoint2PolygonArr[0]) * I );

  //I := Length(fPolygon2PointArr);
  //SaveStream.Write( I );
  //SaveStream.Write(fPolygon2PointArr[0], SizeOf(fPolygon2PointArr[0]) * I );

  // The following does not requires save
  // fDefences
  // fPathfinding
  // fPositioning
end;


procedure TKMNavMesh.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
begin
  fNavMeshGenerator.Load(LoadStream);
  LoadStream.ReadAssert('NavMesh');
  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);

  LoadStream.Read(fNodeCount);
  SetLength(fNodes, fNodeCount);
  LoadStream.Read(fNodes[0], SizeOf(fNodes[0]) * fNodeCount);

  LoadStream.Read(fPolyCount);
  SetLength(fPolygons, fPolyCount);
  LoadStream.Read(fPolygons[0], SizeOf(fPolygons[0]) * fPolyCount);
  //for I := 0 to fPolyCount - 1 do
  //begin
  //  LoadStream.Read(fPolygons[I].CenterPoint);
  //  LoadStream.Read(fPolygons[I].Indices, SizeOf(fPolygons[I].Indices));
  //  LoadStream.Read(fPolygons[I].NearbyCount);
  //  LoadStream.Read(fPolygons[I].Poly2PointStart);
  //  LoadStream.Read(fPolygons[I].Poly2PointCnt);
  //  LoadStream.Read(fPolygons[I].Nearby, SizeOf(fPolygons[I].Nearby));
  //  LoadStream.Read(fPolygons[I].NearbyPoints, SizeOf(fPolygons[I].NearbyPoints));
  //end;

  LoadStream.Read(I);
  SetLength(fPoint2PolygonArr,I);
  LoadStream.Read(fPoint2PolygonArr[0], SizeOf(fPoint2PolygonArr[0]) * I );

  //LoadStream.Read(I);
  //SetLength(fPolygon2PointArr,I);
  //LoadStream.Read(fPolygon2PointArr[0], SizeOf(fPolygon2PointArr[0]) * I );
end;


procedure TKMNavMesh.AfterMissionInit();
var
  Time: Cardinal;
begin
  fMapX := gTerrain.MapX;
  fMapY := gTerrain.MapY;

  Time := TimeGet();
  fNavMeshGenerator.GenerateNewNavMesh();

  fNodeCount := fNavMeshGenerator.NodeCount;
  fPolyCount := fNavMeshGenerator.PolygonCount;
  fNodes := fNavMeshGenerator.Nodes;
  fPolygons := fNavMeshGenerator.Polygons;

  //Mapp all map tiles to its polygons and vice versa
  TieUpTilesWithPolygons();
  TieUpPolygonsWithTiles();

  Time := TimeGet() - Time;
end;


procedure TKMNavMesh.UpdateState(aTick: Cardinal);
begin

end;

function TKMNavMesh.GetPolygonFromPoint(const aY,aX: Integer): Word;
var
  Idx: Integer;
begin
  Result := 0;
  Idx := aY*(fMapX+1) + aX;
  if (Length(fPoint2PolygonArr) > Idx) then
    Result := fPoint2PolygonArr[Idx];
end;

function TKMNavMesh.GetPolygonFromKMPoint(const aPoint: TKMPoint): Word;
var
  Idx: Integer;
begin
  Result := 0;
  Idx := aPoint.Y*(fMapX+1) + aPoint.X;
  if (Length(fPoint2PolygonArr) > Idx) then
    Result := fPoint2PolygonArr[Idx];
end;

procedure TKMNavMesh.FindClosestPolygon();
var
  Len,MaxIdx: Word;
  Queue: array[0..255*255] of Integer;
  procedure AddToQueue(aIdx, aPolygon: Integer);
  begin
    Queue[MaxIdx] := aIdx;
    Inc(Len);
    Inc(MaxIdx);
    if (MaxIdx > High(Queue)) then
      MaxIdx := 0;
    fPoint2PolygonArr[aIdx] := aPolygon;
  end;
var
  I,Y,ActIdx: Integer;
begin
  FillChar(Queue, SizeOf(Queue), #0);
  // Mark borders so they do not have to be checked (fPoint2PolygonArr have length fMapX+1 in X and fMapY+1 in Y)
  FillChar(fPoint2PolygonArr[0], SizeOf(Word)*(fMapX+1), #255);
  FillChar(fPoint2PolygonArr[(fMapX+1)*fMapY], SizeOf(Word)*(fMapX+1), #255);
  Y := 0;
  while Y < (fMapX+1) * (fMapY+1) do
  begin
    fPoint2PolygonArr[Y] := High(Word);
    fPoint2PolygonArr[Y + fMapX] := High(Word);
    Y := Y + fMapX+1;
  end;
  // Get all used points
  Len := 0;
  for I := Low(fPoint2PolygonArr) to High(fPoint2PolygonArr) do
    if (fPoint2PolygonArr[I] <> 0) AND (fPoint2PolygonArr[I] <> High(Word)) then
    begin
      Queue[Len] := I;
      Inc(Len);
    end;
  // Expand used points into empty surrounding area
  ActIdx := 0;
  MaxIdx := Len;
  while (Len > 0) do
  begin
    I := Queue[ActIdx];
    Dec(Len);
    Inc(ActIdx);
    if (ActIdx > High(Queue)) then
      ActIdx := 0;
    if (fPoint2PolygonArr[I - 1        ] = 0) then AddToQueue(I - 1        , fPoint2PolygonArr[I]);
    if (fPoint2PolygonArr[I + 1        ] = 0) then AddToQueue(I + 1        , fPoint2PolygonArr[I]);
    if (fPoint2PolygonArr[I - fMapX - 1] = 0) then AddToQueue(I - fMapX - 1, fPoint2PolygonArr[I]);
    if (fPoint2PolygonArr[I + fMapX + 1] = 0) then AddToQueue(I + fMapX + 1, fPoint2PolygonArr[I]);
  end;
end;

procedure TKMNavMesh.TieUpTilesWithPolygons();
  procedure GetNodesSortedByY(aIdx: Integer; var a,b,c: TKMPoint);
  begin
    a := fNodes[ fPolygons[aIdx].Indices[0] ];
    b := fNodes[ fPolygons[aIdx].Indices[1] ];
    c := fNodes[ fPolygons[aIdx].Indices[2] ];
    if (a.Y > b.Y) then KMSwapPoints(a,b);
    if (b.Y > c.Y) then KMSwapPoints(b,c);
    if (a.Y > b.Y) then KMSwapPoints(a,b);
  end;
  procedure NormalLineEquation(P1,P2: TKMPoint; var a,b,c: Single);
  begin
    a := - P1.Y + P2.Y;
    b := + P1.X - P2.X;
    c := - a * (P1.X + 0.5) - b * (P1.Y + 0.5); // + 0.5 will move triangle by 1/2 of tile in right down direction
  end;
  function IsRightSide(P1,P2,Point: TKMPoint): Boolean;
  begin
    Result := ((P2.X - P1.X) * (Point.Y - P1.Y) < (P2.Y - P1.Y) * (Point.X - P1.X));
  end;
  procedure FillTriangle(RightSide: Boolean; aIdx, StartY,EndY: Integer; a1,b1,c1, a2,b2,c2: Single);
  var
    X,Y: Integer;
    invA1,invA2, X1, X2: Single;
  begin
    if (a1 = 0) OR (a2 = 0) then
      Exit;
    invA1 := 1 / (a1*1.0); // a*X + b*Y + c = 0  ->  X = (- b*Y - c) * (1/a)  ->  X = C1*Y + C2
    invA2 := 1 / (a2*1.0);
    for Y := StartY to EndY do
    begin
      X1 := (- b1*Y - c1) * invA1 + 0.499;
      X2 := (- b2*Y - c2) * invA2;
      if not RightSide then
        KMSwapFloat(X1,X2);
      for X := Round( X1 ) to Trunc( X2 ) do
        fPoint2PolygonArr[ Y*(fMapX+1) + X ] := aIdx; // Property does not have write attribute
    end;
  end;
  procedure ComputeNearbyPoints(aIdx: Word);
  var
    SecondPoint: Boolean;
    I,K,L, ToIdx: Integer;
    P: TKMPoint;
    Indices: array[0..1] of Word;
  begin
    for I := 0 to fPolygons[aIdx].NearbyCount - 1 do
    begin
      SecondPoint := False;
      ToIdx := fPolygons[aIdx].Nearby[I];
      for K := 0 to 2 do
      for L := 0 to 2 do
        if (fPolygons[aIdx].Indices[K] = fPolygons[ToIdx].Indices[L]) then
        begin
          Indices[ Byte(SecondPoint) ] := fPolygons[aIdx].Indices[K];
          SecondPoint := True;
          break;
        end;
       P := KMPointAverage(fNodes[ Indices[0] ], fNodes[ Indices[1] ]);
       fPolygons[aIdx].NearbyPoints[I] := KMPoint(  Min( fMapX-1, Max(1,P.X) ), Min( fMapY-1, Max(1,P.Y) )  );
    end;
  end;
var
  RightSide: Boolean;
  I: Integer;
  a1,b1,c1, a2,b2,c2, a3,b3,c3: Single;
  N1,N2,N3: TKMPoint;
begin
  SetLength(fPoint2PolygonArr, (fMapY+1) * (fMapX+1));
  FillChar(fPoint2PolygonArr[0], SizeOf(fPoint2PolygonArr[0]) * Length(fPoint2PolygonArr), #0); // 0 Is unused polygon
  for I := 1 to fPolyCount - 1 do
  begin
    // Fill fPoint2PolygonArr
    GetNodesSortedByY(I, N1,N2,N3);
    RightSide := IsRightSide(N1,N3,N2);
    NormalLineEquation(N1,N3, a1,b1,c1);
    NormalLineEquation(N1,N2, a2,b2,c2);
    NormalLineEquation(N2,N3, a3,b3,c3);
    FillTriangle(RightSide, I, N1.Y,N2.Y,   a1,b1,c1, a2,b2,c2); // Skip last line if second part is active
    FillTriangle(RightSide, I, N2.Y+1,N3.Y, a1,b1,c1, a3,b3,c3);
    // Fill another polygon informations
    // Center point must be inside of map coords
    fPolygons[I].CenterPoint := KMPoint(
                                         Min(  fMapX-1, Max( 1, Round((N1.X+N2.X+N3.X)/3) )  ),
                                         Min(  fMapY-1, Max( 1, Round((N1.Y+N2.Y+N3.Y)/3) )  )
                                       );
    ComputeNearbyPoints(I);
  end;
  FindClosestPolygon();
end;

procedure TKMNavMesh.TieUpPolygonsWithTiles();
//var
//  X,Y,Polygon: Integer;
begin
  {
  // Get count of points in specific polygon
  for Y := 1 to fMapY-1 do
  for X := 1 to fMapX-1 do
    Inc(fPolygons[ Point2Polygon[Y,X] ].Poly2PointCnt); // Poly2PointCnt was set to 0 in initialization
  // Get starting index of each polygon
  for X := 1 to fPolyCount - 1 do // 0. polygon is reserved / unused
    fPolygons[X].Poly2PointStart := fPolygons[X-1].Poly2PointStart + fPolygons[X-1].Poly2PointCnt;
  // Fill points in 1D array which is common for all polygons
  SetLength(fPolygon2PointArr,(fMapX-1) * (fMapY-1));
  for Y := 1 to fMapY-1 do
  for X := 1 to fMapX-1 do
  begin
    Polygon := Point2Polygon[Y,X];
    fPolygon2PointArr[  fPolygons[Polygon].Poly2PointStart  ] := KMPoint(X,Y);
    Inc(fPolygons[Polygon].Poly2PointStart);
  end;
  // Get starting index of each polygon
  for X := 1 to fPolyCount - 1 do // 0. polygon is reserved / unused
    fPolygons[X].Poly2PointStart := fPolygons[X-1].Poly2PointStart + fPolygons[X-1].Poly2PointCnt;
  //}
end;

//Render debug symbols
procedure TKMNavMesh.Paint(const aRect: TKMRect);
const
  COLOR_WHITE = $FFFFFF;
  COLOR_BLACK = $000000;
  COLOR_GREEN = $00FF00;
  COLOR_RED = $7700FF;
  COLOR_YELLOW = $00FFFF;
  COLOR_BLUE = $FF0000;

  function GetCommonPoints(aIdx1, aIdx2: Word; var aPoint1, aPoint2: TKMPoint): Boolean;
  var
    FirstPoint: Boolean;
    I,K: Integer;
  begin
    Result := False;
    FirstPoint := True;
    for I := 0 to 2 do
    for K := 0 to 2 do
      if (fPolygons[aIdx1].Indices[I] = fPolygons[aIdx2].Indices[K]) then
      begin
        if FirstPoint then
          aPoint1 := fNodes[ fPolygons[aIdx2].Indices[K] ]
        else
        begin
          aPoint2 := fNodes[ fPolygons[aIdx2].Indices[K] ];
          Result := True;
          Exit;
        end;
        FirstPoint := False;
      end;
  end;

  procedure Pokus(A,B: TKMPoint);
  var
    X,Y,K: single;
    dx,dy,step: single;
  begin
    dx := B.X - A.X;
    dy := B.Y - A.Y;
    if (abs(dx) >= abs(dy)) then
      step := abs(dx)
    else
      step := abs(dy);
    dx := dx / step;
    dy := dy / step;
    X := A.X;
    Y := A.Y;
    K := 1;
    while (K <= step) do
    begin
      gRenderAux.Quad(ceil(X), ceil(Y), $AA000000);
      X := X + dx;
      Y := Y + dy;
      K := K + 1;
    end;
    gRenderAux.Quad(ceil(X), ceil(Y), $AA000000);
    gRenderAux.LineOnTerrain(A, B, $50000000 OR COLOR_RED);
  end;

  procedure Pokus2(A,B: TKMPoint);
  var
    X,Y,K: single;
    dx,dy,step: single;
  begin
    dx := B.X - A.X;
    dy := B.Y - A.Y;
    if (abs(dx) >= abs(dy)) then
      step := abs(dx)
    else
      step := abs(dy);
    dx := dx / step;
    dy := dy / step;
    X := A.X;
    Y := A.Y;
    K := 1;
    while (K <= step) do
    begin
      gRenderAux.Quad(floor(X), floor(Y), $AA000000);
      X := X + dx;
      Y := Y + dy;
      K := K + 1;
    end;
    gRenderAux.Quad(floor(X), floor(Y), $AA000000);
    gRenderAux.LineOnTerrain(A, B, $50000000 OR COLOR_RED);
  end;

var
  I, K: Integer;
  p1,p2,p3: TKMPoint;

  Owner: TKMHandIndex;
  DefLines: TKMDefenceLines;
  DefencePosArr: TKMDefencePosArr;
  FFF: TForwardFF;
begin
  //Pokus(KMPoint(9,4), KMPoint(2,3));
  //Pokus2(KMPoint(19,4), KMPoint(12,3));
  {
  Pokus(KMPoint(12,13), KMPoint(19,17));
  Pokus(KMPoint(21,3), KMPoint(29,17));
  Pokus(KMPoint(32,3), KMPoint(33,17));
  Pokus(KMPoint(35,11), KMPoint(47,13));
  Pokus(KMPoint(50,11), KMPoint(55,13));

  Pokus(KMPoint(9,14), KMPoint(2,13));
  Pokus(KMPoint(12,23), KMPoint(19,27));
  }

  //fNavMeshGenerator.Paint(aRect);

  if not AI_GEN_NAVMESH OR not OVERLAY_NAVMESH then
    Exit;

  // EXTRACT POLYGONS

  //{ Triangles and connection of NavMesh
  for I := 1 to fPolyCount - 1 do
    with fPolygons[I] do
    begin
      gRenderAux.TriangleOnTerrain(
        fNodes[ Indices[0] ].X,
        fNodes[ Indices[0] ].Y,
        fNodes[ Indices[1] ].X,
        fNodes[ Indices[1] ].Y,
        fNodes[ Indices[2] ].X,
        fNodes[ Indices[2] ].Y, $50000000 OR COLOR_BLACK);
      for K := 0 to NearbyCount - 1 do
        if GetCommonPoints(I, Nearby[K], p1, p2) then
          gRenderAux.LineOnTerrain(p1, p2, $80000000 OR COLOR_BLUE)
        else
        begin
          gRenderAux.TriangleOnTerrain(
            fNodes[ Indices[0] ].X,
            fNodes[ Indices[0] ].Y,
            fNodes[ Indices[1] ].X,
            fNodes[ Indices[1] ].Y,
            fNodes[ Indices[2] ].X,
            fNodes[ Indices[2] ].Y, $90000000 OR COLOR_WHITE);
        end;
      p1.X := Round( (fNodes[ Indices[0] ].X + fNodes[ Indices[1] ].X + fNodes[ Indices[2] ].X) / 3 );
      p1.Y := Round( (fNodes[ Indices[0] ].Y + fNodes[ Indices[1] ].Y + fNodes[ Indices[2] ].Y) / 3 );
      gRenderAux.Text(p1.X, p1.Y + 1, IntToStr(I), $FFFFFFFF);
    end;//}
  { Center points and transitions of polygons
  for I := 0 to fPolyCount - 1 do
    with fPolygons[I] do
    begin
      gRenderAux.Quad(CenterPoint.X, CenterPoint.Y, $AAFFFFFF);
      for K := 0 to NearbyCount - 1 do
        gRenderAux.Quad(NearbyPoints[K].X, NearbyPoints[K].Y, $AA000000);
    end;//}

  //{ DEFENCE SYSTEM
  // Show this defences only in case that show combat AI is not enabled;
  // when it is we need existing results not the actual (defences are updated each 1 min so it may be different)
  if OVERLAY_NAVMESH AND OVERLAY_DEFENCES AND not OVERLAY_AI_COMBAT then
  begin
    Owner := gMySpectator.HandIndex;
    FFF := TForwardFF.Create(true);
    try
      if FFF.FindDefenceLines(Owner, DefLines) then
        for K := 0 to DefLines.Count - 1 do
        begin
          I := DefLines.Lines[K].Polygon;
          gRenderAux.TriangleOnTerrain(
            fNodes[fPolygons[I].Indices[0]].X,
            fNodes[fPolygons[I].Indices[0]].Y,
            fNodes[fPolygons[I].Indices[1]].X,
            fNodes[fPolygons[I].Indices[1]].Y,
            fNodes[fPolygons[I].Indices[2]].X,
            fNodes[fPolygons[I].Indices[2]].Y, $300000FF);
        end;
      if FFF.FindDefensivePolygons(Owner, DefencePosArr) then
        for K := 0 to Length(DefencePosArr) - 1 do
        begin
          p1 := DefencePosArr[K].DirPoint.Loc;
          gRenderAux.CircleOnTerrain(p1.X, p1.Y, 2, $0900FFFF, $FFFFFFFF);
        end;
    finally
      FFF.Free;
    end;
  end;//}
end;


end.
