unit KM_NavMesh;
{$I KaM_Remake.inc}
interface
uses
  KM_PolySimplify,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  KM_NavMeshDefences,
  KromUtils, KM_CommonUtils,
  KM_NavMeshFloodPositioning, KM_NavMeshPathFinding;// TimeGet


type
  TKMWeightSegments = array of record
    A,B: TKMPoint;
    Weight: Single;
  end;

  TNode = record
      Loc: TKMPoint;
      Nearby: array of Word; //Indexes of connected nodes
    end;

  TPolygon = record
      CenterPoint: TKMPoint;
      NearbyCount: Byte; //could be 0 .. 3
      Indices: array [0..2] of Word; //Neighbour nodes
      Nearby: array [0..2] of Word; //Neighbour polygons
      NearbyPoints: array [0..2] of TKMPoint; // Center points
    end;

  TNodeArray = array of TNode;
  TPolygonArray = array of TPolygon;

  //NavMesh is used to acess the map on a higher level than tiles
  //terrain is represented as a mesh interconnected polygons
  TKMNavMesh = class
  private
    //Keep a copy of these temp arrays for debug rendering
    fRawOutlines: TKMShapesArray;
    fSimpleOutlines: TKMShapesArray;
    fRawMesh: TKMTriMesh;

    //Working data
    fNodeCount: Integer;
    fPolyCount: Integer;
    fNodes: TNodeArray;               // Nodes
    fPolygons: TPolygonArray;         // Polygons
    fPoint2PolygonArr: TKMWord2Array; // KMPoint -> Polygon index


    fDefences: TForwardFF; //Defences class
    fPathfinding: TNavMeshPathFinding; // NavMesh Pathfinding
    fPositioning: TNavMeshFloodPositioning; // NavMesh Positioning

    //Building the navmesh from terrain
    //Process involves many steps executed in a functional way
    procedure GenerateTileOutline(out aTileOutlines: TKMShapesArray);
    procedure TriangulateOutlines;
    procedure AssembleNavMesh;
    procedure InitConnectivity;
    function FindClosestPolygon(aP: TKMPoint): Word;
    procedure TieUpTilesWithPolygons();

    //function GetOwnerPolys(aOwner: TKMHandIndex): TKMWordArray;
    //function ConvertPolysToEdges(aPolys: TKMWordArray): TKMEdgesArray;
    //function RemoveInnerEdges(const aEdges: TKMEdgesArray): TKMEdgesArray;
    //function EdgesToWeightOutline(const aEdges: TKMEdgesArray; aOwner: TKMHandIndex): TKMWeightSegments;

    //function GetBestOwner(aIndex: Integer): TKMHandIndex;
    //function NodeEnemyPresence(aIndex: Integer; aOwner: TKMHandIndex): Word;
    //function PolyEnemyPresence(aIndex: Integer; aOwner: TKMHandIndex): Word;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property Point2Polygon: TKMWord2Array read fPoint2PolygonArr;
    property Polygons: TPolygonArray read fPolygons;
    property Nodes: TNodeArray read fNodes;
    property Defences: TForwardFF read fDefences write fDefences;
    property Pathfinding: TNavMeshPathFinding read fPathfinding write fPathfinding;
    property Positioning: TNavMeshFloodPositioning read fPositioning write fPositioning;

    procedure AfterMissionInit();
    //procedure GetDefenceOutline(aOwner: TKMHandIndex; out aOutline1, aOutline2: TKMWeightSegments);

    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;

const
  NAVMESH_DENSITY = 3; // Density of NavMesh (affect count and size of points)


implementation
uses
  SysUtils, Math, Delaunay,
  KM_Terrain, KM_HandsCollection, KM_RenderAux, KM_Outline,
  Dialogs;


{ TKMNavMesh }
constructor TKMNavMesh.Create();
begin
  inherited Create;

  fDefences := TForwardFF.Create(True);
  fPathfinding := TNavMeshPathFinding.Create();
  fPositioning := TNavMeshFloodPositioning.Create();
end;


destructor TKMNavMesh.Destroy();
begin
  fDefences.Free;
  fPathfinding.Free;
  fPositioning.Free;
  inherited;
end;


procedure TKMNavMesh.Save(SaveStream: TKMemoryStream);
var
  I,K: Integer;
begin
  SaveStream.WriteA('NavMesh');

  SaveStream.Write(fNodeCount);
  for I := 0 to fNodeCount - 1 do
  begin
    SaveStream.Write(fNodes[I].Loc);

    SaveStream.Write(Integer(Length(fNodes[I].Nearby)));
    for K := 0 to Length(fNodes[I].Nearby) - 1 do
      SaveStream.Write(fNodes[I].Nearby[K]);
  end;

  SaveStream.Write(fPolyCount);
  for I := 0 to fPolyCount - 1 do
  begin
    SaveStream.Write(fPolygons[I].CenterPoint);
    SaveStream.Write(fPolygons[I].Indices, SizeOf(fPolygons[I].Indices));
    SaveStream.Write(fPolygons[I].NearbyCount);
    SaveStream.Write(fPolygons[I].Nearby, SizeOf(fPolygons[I].Nearby));
    SaveStream.Write(fPolygons[I].NearbyPoints, SizeOf(fPolygons[I].NearbyPoints));
  end;

  I := Length(fPoint2PolygonArr);
  SaveStream.Write( I );
  K := 0;
  if (I > 0) then
    K := Length(fPoint2PolygonArr[0]);
  SaveStream.Write( K );
  for I := 0 to Length(fPoint2PolygonArr) - 1 do
    SaveStream.Write(fPoint2PolygonArr[I,0], SizeOf(fPoint2PolygonArr[0,0]) * K );

  // The following does not requires save
  // fPathfinding
  // fPositioning
end;


procedure TKMNavMesh.Load(LoadStream: TKMemoryStream);
var
  I,K: Integer;
  NewCount: Integer;
begin
  LoadStream.ReadAssert('NavMesh');

  LoadStream.Read(fNodeCount);
  SetLength(fNodes, fNodeCount);
  for I := 0 to fNodeCount - 1 do
  begin
    LoadStream.Read(fNodes[I].Loc);

    LoadStream.Read(NewCount);
    SetLength(fNodes[I].Nearby, NewCount);
    for K := 0 to NewCount - 1 do
      LoadStream.Read(fNodes[I].Nearby[K]);
  end;

  LoadStream.Read(fPolyCount);
  SetLength(fPolygons, fPolyCount);
  for I := 0 to fPolyCount - 1 do
  begin
    LoadStream.Read(fPolygons[I].CenterPoint);
    LoadStream.Read(fPolygons[I].Indices, SizeOf(fPolygons[I].Indices));
    LoadStream.Read(fPolygons[I].NearbyCount);
    LoadStream.Read(fPolygons[I].Nearby, SizeOf(fPolygons[I].Nearby));
    LoadStream.Read(fPolygons[I].NearbyPoints, SizeOf(fPolygons[I].NearbyPoints));
  end;

  LoadStream.Read(I);
  LoadStream.Read(K);
  SetLength(fPoint2PolygonArr,I,K);
  for I := 0 to Length(fPoint2PolygonArr) - 1 do
    LoadStream.Read(fPoint2PolygonArr[I,0], SizeOf(fPoint2PolygonArr[0,0]) * K );
end;


procedure TKMNavMesh.AfterMissionInit();
var
  TileOutlines: TKMShapesArray;
begin
  //Convert tilemap into vector outlines
  GenerateTileOutline(TileOutlines);

  //Remove extra points on straights
  SimplifyStraights(TileOutlines, KMRect(0, 0, gTerrain.MapX-1, gTerrain.MapY-1), fRawOutlines);

  //Perform outlines simplification
  with TKMSimplifyShapes.Create(2, KMRect(0, 0, gTerrain.MapX-1, gTerrain.MapY-1)) do
  begin
    Execute(fRawOutlines, fSimpleOutlines);
    Free;
  end;

  //Triangulate everything
  TriangulateOutlines;

  //Force mesh triangulation to be along outlines
  ForceOutlines(fRawMesh, KMRect(0, 0, gTerrain.MapX-1, gTerrain.MapY-1), fSimpleOutlines);

  //Remove polygons within obstacles
  RemoveObstaclePolies(fRawMesh, fSimpleOutlines);

  //Remove outside frame (required by Delaunay)
  RemoveFrame(fRawMesh);

  //Make sure we dont have degenerate polys left
  CheckForDegenerates(fRawMesh);

  Assert(Length(fRawMesh.Polygons) >= 6);

  //Fill in NavMesh structure
  AssembleNavMesh;

  //Mapp all map tiles to its polygons
  TieUpTilesWithPolygons();
end;


// Find closest NavMesh in case that we get point which is not part of fPoint2PolygonArr
// (stonemason may mine stone and create walkable tiles which are not part of NavMesh [NavMesh is not updated during game])
// Computed points will be copied into fPoint2PolygonArr in TieUpTilesWithPolygons function -> fPoint2PolygonArr will always refer to existing point
function TKMNavMesh.FindClosestPolygon(aP: TKMPoint): Word;
const
  MAX_SCAN_DIST = 255;
var
  I,X,Y, Output: Word;
  PMin,PMax: TKMPoint;
begin
  Result := High(Word);
  if not gTerrain.TileInMapCoords(aP.X, aP.Y) then
    aP := KMPoint(  Min( Max(1,aP.X), gTerrain.MapX-1), Min( Max(1,aP.Y), gTerrain.MapY-1)  );

  Result := fPoint2PolygonArr[aP.Y,aP.X];
  if (Result <> High(Word)) then
    Exit;

  Output := High(Word);
  for I := 0 to MAX_SCAN_DIST do
  begin
    PMin := KMPoint(Max(1,aP.X - I), Max(1,aP.Y - I));
    PMax := KMPoint(Min(gTerrain.MapX-1,aP.X + I), Min(gTerrain.MapY-1,aP.Y + I));
    for X := PMin.X to PMax.X do
      if (fPoint2PolygonArr[PMin.Y,X] <> High(Word)) then
      begin
        Output := fPoint2PolygonArr[PMin.Y,X];
        break;
      end
      else if (fPoint2PolygonArr[PMax.Y,X] <> High(Word)) then
      begin
        Output := fPoint2PolygonArr[PMax.Y,X];
        break;
      end;
    if (Output <> High(Word)) then
      break;
    for Y := PMin.Y to PMax.Y do
      if (fPoint2PolygonArr[Y,PMin.X] <> High(Word)) then
      begin
        Output := fPoint2PolygonArr[Y,PMin.X];
        break;
      end
      else if (fPoint2PolygonArr[Y,PMax.X] <> High(Word)) then
      begin
        Output := fPoint2PolygonArr[Y,PMax.X];
        break;
      end;
    if (Output <> High(Word)) then
      break;
  end;
  Result := Output;
end;


procedure TKMNavMesh.TieUpTilesWithPolygons();

// Is inside triangle (source: https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle)
  function Sign(const aP1, aP2, aP3: TKMPointF): Single;
  begin
      Result := (aP1.X - aP3.X) * (aP2.Y - aP3.Y) - (aP2.X - aP3.X) * (aP1.Y - aP3.Y);
  end;

  function PointInTriangle(const aPt, aV1, aV2, aV3: TKMPointF): Boolean;
  var
    b1, b2, b3: Boolean;
  begin
    b1 := Sign(aPt, aV1, aV2) <= 0.0;
    b2 := Sign(aPt, aV2, aV3) <= 0.0;
    b3 := Sign(aPt, aV3, aV1) <= 0.0;
    Result := ((b1 = b2) AND (b2 = b3));
  end;

  procedure ComputeNearbyPoints(aIdx: Word);
  var
    SecondPoint: Boolean;
    I,K,L, ToIdx: Integer;
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
      fPolygons[aIdx].NearbyPoints[I] := KMPointAverage(fNodes[ Indices[0] ].Loc, fNodes[ Indices[1] ].Loc);
    end;
  end;
var
  AlreadyInTriangle: Boolean;
  I, i1, i2, i3: Word;
  X, Y: Integer;
  MinPoint, MaxPoint: TKMPoint;
  v1, v2, v3: TKMPointF;
  WordArr: TKMWord2Array;
begin
  SetLength(fPoint2PolygonArr, gTerrain.MapY, gTerrain.MapX);
  for Y := Low(fPoint2PolygonArr) to High(fPoint2PolygonArr) do
    for X := Low(fPoint2PolygonArr[Y]) to High(fPoint2PolygonArr[Y]) do
      fPoint2PolygonArr[Y,X] := High(Word);

  for I := 0 to fPolyCount - 1 do
  begin
    i1 := fPolygons[I].Indices[0];
    i2 := fPolygons[I].Indices[1];
    i3 := fPolygons[I].Indices[2];

    v1 := KMPointF(fNodes[i1].Loc.X, fNodes[i1].Loc.Y);
    v2 := KMPointF(fNodes[i2].Loc.X, fNodes[i2].Loc.Y);
    v3 := KMPointF(fNodes[i3].Loc.X, fNodes[i3].Loc.Y);
    //CenterPoint := KMPoint( Round((v1.X+v2.X+v3.X)/3), Round((v1.Y+v2.Y+v3.Y)/3) );
    fPolygons[I].CenterPoint := KMPoint( Round((v1.X+v2.X+v3.X)/3), Round((v1.Y+v2.Y+v3.Y)/3) );

    MinPoint.X := Min(  Min( fNodes[i1].Loc.X, fNodes[i2].Loc.X ), fNodes[i3].Loc.X  );
    MinPoint.Y := Min(  Min( fNodes[i1].Loc.Y, fNodes[i2].Loc.Y ), fNodes[i3].Loc.Y  );
    MaxPoint.X := Max(  Max( fNodes[i1].Loc.X, fNodes[i2].Loc.X ), fNodes[i3].Loc.X  );
    MaxPoint.Y := Max(  Max( fNodes[i1].Loc.Y, fNodes[i2].Loc.Y ), fNodes[i3].Loc.Y  );

    for Y := MinPoint.Y+1 to MaxPoint.Y do
    begin
      AlreadyInTriangle := False;
      for X := MinPoint.X+1 to MaxPoint.X do
      begin
        if PointInTriangle(KMPointF(X-0.5,Y-0.5), v1, v2, v3) then
        begin
          fPoint2PolygonArr[Y,X] := I;
          AlreadyInTriangle := True;
        end
        else if AlreadyInTriangle then
          break;
      end;
    end;

    ComputeNearbyPoints(I);
  end;

  // Now add points
  SetLength(WordArr, gTerrain.MapY, gTerrain.MapX);
  for Y := Low(fPoint2PolygonArr) to High(fPoint2PolygonArr) do
    for X := Low(fPoint2PolygonArr[Y]) to High(fPoint2PolygonArr[Y]) do
      if (fPoint2PolygonArr[Y,X] = High(Word)) then
        WordArr[Y,X] := FindClosestPolygon(KMPoint(X,Y));

  for Y := Low(fPoint2PolygonArr) to High(fPoint2PolygonArr) do
    for X := Low(fPoint2PolygonArr[Y]) to High(fPoint2PolygonArr[Y]) do
      if (fPoint2PolygonArr[Y,X] = High(Word)) then
        fPoint2PolygonArr[Y,X] := WordArr[Y,X];
end;


procedure TKMNavMesh.GenerateTileOutline(out aTileOutlines: TKMShapesArray);
var
  I, K: Integer;
  Tmp: TKMByte2Array;
begin
  SetLength(Tmp, gTerrain.MapY-1, gTerrain.MapX-1);

  //Copy map to temp array as 0/1 (generator uses other byte values for its needs)
  //0 - no obstacle
  //1 - obstacle
  for I := 0 to gTerrain.MapY - 2 do
  for K := 0 to gTerrain.MapX - 2 do
    Tmp[I,K] := 1 - Byte(tpOwn in gTerrain.Land[I+1,K+1].Passability);

  GenerateOutline(Tmp, 12, aTileOutlines);

  //GenerateOutline is 0 based for versatility purposes, but Terrain in 1 based
  //because of legacy reasons. Do the conversion
  for I := 0 to aTileOutlines.Count - 1 do
  for K := 0 to aTileOutlines.Shape[I].Count - 1 do
  begin
    aTileOutlines.Shape[I].Nodes[K].X := aTileOutlines.Shape[I].Nodes[K].X + 1;
    aTileOutlines.Shape[I].Nodes[K].Y := aTileOutlines.Shape[I].Nodes[K].Y + 1;
  end;
end;


procedure TKMNavMesh.TriangulateOutlines;
var
  fDelaunay: TDelaunay;
  I, K, L, M: Integer;
  MeshDensityX, MeshDensityY: Byte;
  SizeX, SizeY: Word;
  PX,PY,TX,TY: Integer;
  Skip: Boolean;
begin
  //Fill area with Delaunay triangles
  fDelaunay := TDelaunay.Create(-1, -1, gTerrain.MapX, gTerrain.MapY);
  try
    //Points that are closer than that will be skipped
    fDelaunay.Tolerance := 1;
    for I := 0 to fSimpleOutlines.Count - 1 do
    with fSimpleOutlines.Shape[I] do
      for K := 0 to Count - 1 do
        fDelaunay.AddPoint(Nodes[K].X, Nodes[K].Y);

    //Add more points along edges to get even density
    SizeX := gTerrain.MapX-1;
    SizeY := gTerrain.MapY-1;
    MeshDensityX := SizeX div NAVMESH_DENSITY;
    MeshDensityY := SizeY div NAVMESH_DENSITY;
    for I := 0 to MeshDensityY do
    for K := 0 to MeshDensityX do
    if (I = 0) or (I = MeshDensityY) or (K = 0) or (K = MeshDensityX) then
    begin
      Skip := False;
      PX := Round(SizeX / MeshDensityX * K);
      PY := Round(SizeY / MeshDensityY * I);

      //Don't add point to obstacle outline if there's one below
      for L := 0 to fSimpleOutlines.Count - 1 do
      with fSimpleOutlines.Shape[L] do
        for M := 0 to Count - 1 do
        begin
          TX := Nodes[(M + 1) mod Count].X;
          TY := Nodes[(M + 1) mod Count].Y;
          if InRange(PX, Nodes[M].X, TX) and InRange(PY, Nodes[M].Y, TY)
          or InRange(PX, TX, Nodes[M].X) and InRange(PY, TY, Nodes[M].Y) then
            Skip := True;
        end;

      if not Skip then
        fDelaunay.AddPoint(PX, PY);
    end;

    //Add more supporting points into the middle to get more even mesh
    //Tolerance must be a little higher than longest span we expect from polysimplification
    //so that not a single node was placed on an outline segment (otherwise RemObstaclePolys will not be able to trace outlines)
    fDelaunay.Tolerance := 7;
    for I := 1 to MeshDensityY - 1 do
    for K := 1 to MeshDensityX - Byte(I mod 2 = 1) - 1 do
      fDelaunay.AddPoint(Round(SizeX / MeshDensityX * (K + Byte(I mod 2 = 1) / 2)), Round(SizeY / MeshDensityY * I));

    //Do the Delaunay magick
    fDelaunay.Mesh;

    //Get triangulated mesh back
    SetLength(fRawMesh.Vertices, fDelaunay.VerticeCount);
    for I := 0 to fDelaunay.VerticeCount - 1 do
    begin
      fRawMesh.Vertices[I].X := Round(fDelaunay.Vertex[I].X);
      fRawMesh.Vertices[I].Y := Round(fDelaunay.Vertex[I].Y);
    end;
    SetLength(fRawMesh.Polygons, fDelaunay.PolyCount);
    for I := 0 to fDelaunay.PolyCount - 1 do
    begin
      fRawMesh.Polygons[I,0] := fDelaunay.Triangle^[I].vv0;
      fRawMesh.Polygons[I,1] := fDelaunay.Triangle^[I].vv1;
      fRawMesh.Polygons[I,2] := fDelaunay.Triangle^[I].vv2;
    end;
  finally
    FreeAndNil(fDelaunay);
  end;
end;


procedure TKMNavMesh.AssembleNavMesh;
var
  I: Integer;
begin
  fNodeCount := Length(fRawMesh.Vertices);
  fPolyCount := Length(fRawMesh.Polygons);

  //Bring triangulated mesh back
  SetLength(fNodes, fNodeCount);
  for I := 0 to fNodeCount - 1 do
    fNodes[I].Loc := fRawMesh.Vertices[I];

  SetLength(fPolygons, fPolyCount);
  for I := 0 to fPolyCount - 1 do
  begin
    fPolygons[I].Indices[0] := fRawMesh.Polygons[I,0];
    fPolygons[I].Indices[1] := fRawMesh.Polygons[I,1];
    fPolygons[I].Indices[2] := fRawMesh.Polygons[I,2];
  end;

  InitConnectivity;
end;


procedure TKMNavMesh.InitConnectivity;
  procedure DoConnectNodes(I, N1, N2: Word);
  begin
    with fNodes[I] do
    begin
      SetLength(Nearby, Length(Nearby) + 2);
      Nearby[High(Nearby)-1] := N1;
      Nearby[High(Nearby)] := N2;
    end;
  end;
  procedure DoConnectPolys(aPoly, N1, N2, N3: Word);
  var
    I: Integer;
    K1,K2,K3: Word;
  begin
    for I := 0 to fPolyCount - 1 do
    if (I <> aPoly) then
    with fPolygons[I] do
    begin
      K1 := Indices[0];
      K2 := Indices[1];
      K3 := Indices[2];
      //Need to check all combinations
      if ((K1 = N2) and (K2 = N1)) or ((K1 = N1) and (K2 = N3)) or ((K1 = N3) and (K2 = N2))
      or ((K2 = N2) and (K3 = N1)) or ((K2 = N1) and (K3 = N3)) or ((K2 = N3) and (K3 = N2))
      or ((K3 = N2) and (K1 = N1)) or ((K3 = N1) and (K1 = N3)) or ((K3 = N3) and (K1 = N2))
      then
      begin
        Nearby[NearbyCount] := aPoly;
        Inc(NearbyCount);
      end;
    end;
  end;
var
  I: Integer;
begin
  //Set nodes nearbys to fill influence field
  for I := 0 to fPolyCount - 1 do
  with fPolygons[I] do
  begin
    DoConnectNodes(Indices[0], Indices[1], Indices[2]);
    DoConnectNodes(Indices[1], Indices[0], Indices[2]);
    DoConnectNodes(Indices[2], Indices[0], Indices[1]);
  end;

  //Erase
  for I := 0 to fPolyCount - 1 do
  begin
    fPolygons[I].NearbyCount := 0;
    fPolygons[I].Nearby[0] := 0;
    fPolygons[I].Nearby[1] := 0;
    fPolygons[I].Nearby[2] := 0;
  end;

  //Set polys connectivity to be able to expand
  for I := 0 to fPolyCount - 1 do
  with fPolygons[I] do
    DoConnectPolys(I, Indices[0], Indices[1], Indices[2]);
end;




{
function TKMNavMesh.GetBestOwner(aIndex: Integer): TKMHandIndex;
var
  I: Integer;
  Best: Byte;
begin
  Best := 0;
  Result := PLAYER_NONE;
  for I := 0 to gHands.Count - 1 do
  if fNodes[aIndex].Owner[I] > Best then
  begin
    Best := fNodes[aIndex].Owner[I];
    Result := I;
  end;
end;


function TKMNavMesh.NodeEnemyPresence(aIndex: Integer; aOwner: TKMHandIndex): Word;
var I: Integer;
begin
  Result := 0;
  for I := 0 to gHands.Count - 1 do
  if (I <> aOwner) and (gHands.CheckAlliance(aOwner, I) = at_Enemy) then
    Result := Result + fNodes[aIndex].Owner[I];
end;


function TKMNavMesh.PolyEnemyPresence(aIndex: Integer; aOwner: TKMHandIndex): Word;
var I: Integer;
begin
  Result := 0;
  for I := 0 to gHands.Count - 1 do
  if (I <> aOwner) and (gHands.CheckAlliance(aOwner, I) = at_Enemy) then
    Result := Result + (fNodes[fPolygons[aIndex].Indices[0]].Owner[I]
                      + fNodes[fPolygons[aIndex].Indices[1]].Owner[I]
                      + fNodes[fPolygons[aIndex].Indices[2]].Owner[I]) div 3;
end;


function TKMNavMesh.GetOwnerPolys(aOwner: TKMHandIndex): TKMWordArray;
var I,K: Integer;
begin
  //Collect polys that are well within our ownership area
  K := 0;
  SetLength(Result, fPolyCount);

  for I := 0 to fPolyCount - 1 do
  with fPolygons[I] do
  if ((fNodes[Indices[0]].Owner[aOwner] >= OWN_MARGIN)
   or (fNodes[Indices[1]].Owner[aOwner] >= OWN_MARGIN)
   or (fNodes[Indices[2]].Owner[aOwner] >= OWN_MARGIN))
  and (fNodes[Indices[0]].Owner[aOwner] >= OWN_THRESHOLD)
  and (fNodes[Indices[1]].Owner[aOwner] >= OWN_THRESHOLD)
  and (fNodes[Indices[2]].Owner[aOwner] >= OWN_THRESHOLD)
  and (GetBestOwner(Indices[0]) = aOwner)
  and (GetBestOwner(Indices[1]) = aOwner)
  and (GetBestOwner(Indices[2]) = aOwner) then
  begin
    Result[K] := I;
    Inc(K);
  end;
  SetLength(Result, K);
end;


function TKMNavMesh.ConvertPolysToEdges(aPolys: TKMWordArray): TKMEdgesArray;
var I: Integer;
begin
  Result.Count := Length(aPolys) * 3;
  SetLength(Result.Nodes, Length(aPolys) * 3);
  for I := 0 to High(aPolys) do
  begin
    Result.Nodes[I * 3 + 0, 0] := fPolygons[aPolys[I]].Indices[0];
    Result.Nodes[I * 3 + 0, 1] := fPolygons[aPolys[I]].Indices[1];
    Result.Nodes[I * 3 + 1, 0] := fPolygons[aPolys[I]].Indices[1];
    Result.Nodes[I * 3 + 1, 1] := fPolygons[aPolys[I]].Indices[2];
    Result.Nodes[I * 3 + 2, 0] := fPolygons[aPolys[I]].Indices[2];
    Result.Nodes[I * 3 + 2, 1] := fPolygons[aPolys[I]].Indices[0];
  end;
end;


function TKMNavMesh.RemoveInnerEdges(const aEdges: TKMEdgesArray): TKMEdgesArray;
var
  I,K: Integer;
  Edges: TKMEdgesArray;
begin
  //Duplicate to avoid spoiling (we need to copy arrays manually)
  Edges.Count := aEdges.Count;
  SetLength(Edges.Nodes, Edges.Count);
  for I := 0 to aEdges.Count - 1 do
  begin
    Edges.Nodes[I][0] := aEdges.Nodes[I][0];
    Edges.Nodes[I][1] := aEdges.Nodes[I][1];
  end;

  //Remove duplicate Edges, that will leave us with an outline
  for I := 0 to Edges.Count - 1 do
    if (Edges.Nodes[I, 0] > -1) and (Edges.Nodes[I, 1] > -1) then
      for K := I + 1 to Edges.Count - 1 do
        if (Edges.Nodes[K, 0] > -1) and (Edges.Nodes[K, 1] > -1) then
          if (Edges.Nodes[I, 0] = Edges.Nodes[K, 1]) and (Edges.Nodes[I, 1] = Edges.Nodes[K, 0]) then
          begin
            Edges.Nodes[I, 0] := -1;
            Edges.Nodes[I, 1] := -1;
            Edges.Nodes[K, 0] := -1;
            Edges.Nodes[K, 1] := -1;
          end;

  //3. Detect and dismiss inner Edges
  //Separate Edges into open (having 2 polys) and closed (only 1 poly)
  //Once again we take advantage of the fact that polys built in CW order
  for I := 0 to fPolyCount - 1 do
    with fPolygons[I] do
      for K := 0 to Edges.Count - 1 do
        if (Edges.Nodes[K, 0] <> -1) then
          if ((Edges.Nodes[K, 0] = Indices[1]) and (Edges.Nodes[K, 1] = Indices[0]))
          or ((Edges.Nodes[K, 0] = Indices[2]) and (Edges.Nodes[K, 1] = Indices[1]))
          or ((Edges.Nodes[K, 0] = Indices[0]) and (Edges.Nodes[K, 1] = Indices[2])) then
          begin
            //Mark outer Edges
            Edges.Nodes[K, 0] := -1000 - Edges.Nodes[K, 0];
            Edges.Nodes[K, 1] := -1000 - Edges.Nodes[K, 1];
          end;
  K := 0;
  for I := 0 to Edges.Count - 1 do
    if (Edges.Nodes[I, 0] >= 0) then
    begin //Dismiss inner Edges
      Edges.Nodes[I, 0] := -1;
      Edges.Nodes[I, 1] := -1;
    end
    else if (Edges.Nodes[I, 0] < -1) then
    begin //Promote marked outer Edges back
      Edges.Nodes[I, 0] := -Edges.Nodes[I, 0] - 1000;
      Edges.Nodes[I, 1] := -Edges.Nodes[I, 1] - 1000;
      Inc(K);
    end;

  //4. Now we can assemble suboptimal outline from kept Edges
  Result.Count := K;
  SetLength(Result.Nodes, K);
  K := 0;
  for I := 0 to Edges.Count - 1 do
    if (Edges.Nodes[I, 0] >= 0) then
    begin
      Result.Nodes[K, 0] := Edges.Nodes[I, 0];
      Result.Nodes[K, 1] := Edges.Nodes[I, 1];
      Inc(K);
    end;
end;


function TKMNavMesh.EdgesToWeightOutline(const aEdges: TKMEdgesArray; aOwner: TKMHandIndex): TKMWeightSegments;
var I: Integer;
begin
  SetLength(Result, aEdges.Count);
  for I := 0 to aEdges.Count - 1 do
  begin
    Result[I].A := fNodes[aEdges.Nodes[I,0]].Loc;
    Result[I].B := fNodes[aEdges.Nodes[I,1]].Loc;
    Result[I].Weight := NodeEnemyPresence(aEdges.Nodes[I,0], aOwner)
                      + NodeEnemyPresence(aEdges.Nodes[I,1], aOwner);
  end;
end;


procedure TKMNavMesh.GetDefenceOutline(aOwner: TKMHandIndex; out aOutline1, aOutline2: TKMWeightSegments);
const
  AP_CLEAR = 0;
  AP_SEED = 255;
var
  AreaID: Byte;
  AreaPolys: array of Byte;
  AreaEnemy: array [1..254] of Word;

  procedure FloodFillPolys(aIndex: Integer);
  var I: Integer;
  begin
    with fPolygons[aIndex] do
    for I := 0 to NearbyCount - 1 do
    if (AreaPolys[Nearby[I]] = AP_CLEAR) then
    begin
      AreaPolys[Nearby[I]] := AreaID; //Mark as explored
      AreaEnemy[AreaID] := Max(AreaEnemy[AreaID], PolyEnemyPresence(Nearby[I], aOwner));
      FloodFillPolys(Nearby[I]);
    end;
  end;

var
  I,K: Integer;
  Polys: TKMWordArray;
  Edges: TKMEdgesArray;

  diff: Cardinal;
  Sdiff: String;

begin

  diff := TimeGet;


  //1. Get ownership area
  Polys := GetOwnerPolys(aOwner);

  Edges := ConvertPolysToEdges(Polys);

  //Obtain suboptimal outline of owned polys
  Edges := RemoveInnerEdges(Edges);

  aOutline1 := EdgesToWeightOutline(Edges, aOwner);

  //5. Remove spans that face isolated areas
  SetLength(AreaPolys, fPolyCount);
  for I := Low(AreaEnemy) to High(AreaEnemy) do
    AreaEnemy[I] := 0;

  for I := 0 to High(Polys) do
    AreaPolys[Polys[I]] := AP_SEED;

  //Floodfill outer polys skipping inner ones, remember best enemy influence
  AreaID := 0;
  for I := 0 to High(Polys) do
    with fPolygons[Polys[I]] do
      for K := 0 to NearbyCount - 1 do
      if (AreaPolys[Nearby[K]] = AP_CLEAR) then
      begin
        Inc(AreaID);
        FloodFillPolys(Polys[I]);
      end;

  //if enemy influence < 128 then mark as isolated
  K := Length(Polys);
  SetLength(Polys, fPolyCount);
  for I := 0 to fPolyCount - 1 do
    if (AreaPolys[I] <> AP_CLEAR)
    and (AreaPolys[I] <> AP_SEED)
    and (AreaEnemy[AreaPolys[I]] < 128) then
    begin
      Polys[K] := I;
      Inc(K);
    end;
  SetLength(Polys, K);

  Edges := ConvertPolysToEdges(Polys);

  Edges := RemoveInnerEdges(Edges);

  aOutline2 := EdgesToWeightOutline(Edges, aOwner);

  diff := GetTimeSince(diff);
  Str(diff,Sdiff);
  //ShowMessage(Sdiff);

  //6. See if we can expand our area while reducing outline length

  //7. Deal with allies
  //   Two players could be on same island and share defence lines,
  //   also they dont need defence line between them
end;
//}


procedure TKMNavMesh.UpdateState(aTick: Cardinal);
begin
  //if aTick mod 600 = 0 then
  //  UpdateOwnership;
end;


//Render debug symbols
procedure TKMNavMesh.Paint(aRect: TKMRect);
var
  I, K, J: Integer;
  T1, T2: TKMPointF;
  Col, Col2: Cardinal;
  Sz: Single;
  Outline1, Outline2: TKMWeightSegments;


  W: Word;
  Owner: TKMHandIndex;
  Point: TKMPoint;
  DefLines: TKMDefenceLines;
  FFF: TForwardFF;
  DefencePosArr: TKMDefencePosArr;

begin
  if not AI_GEN_NAVMESH then Exit;

  //{ Test defences
  // Show this defences only in case that show combat AI is not enabled;
  // when it is we need existing results not the actual (defences are updated each 1 min so it may be different)
  if OVERLAY_NAVMESH AND OVERLAY_DEFENCES AND not OVERLAY_AI_COMBAT then
  begin
    FFF := TForwardFF.Create(true);
    Owner := gMySpectator.HandIndex;
    try

      FFF.FindDefenceLines(Owner, DefLines);
      for K := 0 to DefLines.Count - 1 do
      begin
        I := DefLines.Lines[K].Polygon;
        gRenderAux.TriangleOnTerrain(
          fNodes[fPolygons[I].Indices[0]].Loc.X,
          fNodes[fPolygons[I].Indices[0]].Loc.Y,
          fNodes[fPolygons[I].Indices[1]].Loc.X,
          fNodes[fPolygons[I].Indices[1]].Loc.Y,
          fNodes[fPolygons[I].Indices[2]].Loc.X,
          fNodes[fPolygons[I].Indices[2]].Loc.Y, $300000FF);
      end;

      W := 20;
      FFF.FindDefensivePolygons(Owner, W, DefencePosArr);
      for K := 0 to Length(DefencePosArr) - 1 do
      begin
        Point := DefencePosArr[K].DirPoint.Loc;
        gRenderAux.CircleOnTerrain(Point.X, Point.Y, 2, $0900FFFF, $FFFFFFFF);
      end;

      //PolygonArr := FFF.D_FF_INIT_ARR;
      //for K := 0 to Length(PolygonArr) - 1 do
      //begin
      //  I := PolygonArr[K];
      //  gRenderAux.TriangleOnTerrain(
      //    fNodes[fPolygons[I].Indices[0]].Loc.X,
      //    fNodes[fPolygons[I].Indices[0]].Loc.Y,
      //    fNodes[fPolygons[I].Indices[1]].Loc.X,
      //    fNodes[fPolygons[I].Indices[1]].Loc.Y,
      //    fNodes[fPolygons[I].Indices[2]].Loc.X,
      //    fNodes[fPolygons[I].Indices[2]].Loc.Y, $900000FF);
      //end;

      //PolygonArr := FFF.D_FF_INIT_FLOOD;
      //for K := 0 to Length(PolygonArr) - 1 do
      //begin
      //  I := PolygonArr[K];
      //  gRenderAux.TriangleOnTerrain(
      //    fNodes[fPolygons[I].Indices[0]].Loc.X,
      //    fNodes[fPolygons[I].Indices[0]].Loc.Y,
      //    fNodes[fPolygons[I].Indices[1]].Loc.X,
      //    fNodes[fPolygons[I].Indices[1]].Loc.Y,
      //    fNodes[fPolygons[I].Indices[2]].Loc.X,
      //    fNodes[fPolygons[I].Indices[2]].Loc.Y, $9000FF00);
      //end;
    finally
      FFF.Free;
    end;
  end;//}

  //{Raw obstacle outlines
  if OVERLAY_NAVMESH then
    for I := 0 to fRawOutlines.Count - 1 do
    for K := 0 to fRawOutlines.Shape[I].Count - 1 do
    with fRawOutlines.Shape[I] do
      gRenderAux.LineOnTerrain(Nodes[K], Nodes[(K + 1) mod Count], $FFFF00FF);//}

  {NavMesh polys coverage
  if OVERLAY_NAVMESH then
    for I := 0 to fPolyCount - 1 do
      //if fPolygons[I].NearbyCount = 3 then
      gRenderAux.TriangleOnTerrain(
        fNodes[fPolygons[I].Indices[0]].Loc.X,
        fNodes[fPolygons[I].Indices[0]].Loc.Y,
        fNodes[fPolygons[I].Indices[1]].Loc.X,
        fNodes[fPolygons[I].Indices[1]].Loc.Y,
        fNodes[fPolygons[I].Indices[2]].Loc.X,
        fNodes[fPolygons[I].Indices[2]].Loc.Y, $90FF0000);//}

  //NavMesh edges
  if OVERLAY_NAVMESH then
    for I := 0 to fPolyCount - 1 do
    with fPolygons[I] do
    for K := 0 to 2 do
    begin
      T1 := KMPointF(fNodes[Indices[K]].Loc);
      J := (K + 1) mod 2;
      T2 := KMPointF(fNodes[Indices[J]].Loc);
      gRenderAux.LineOnTerrain(T1, T2, $80FF8000, $F0F0);
    end;

  {//NavMesh vertice ids
  if OVERLAY_NAVMESH then
    for I := 0 to High(fVertices) do
      gRenderAux.Text(fVertices[I].X,fVertices[I].Y, IntToStr(I), $FF000000); //}

  //{//NavMesh polys ids
  if OVERLAY_NAVMESH then
    for I := 0 to fPolyCount - 1 do
    with fPolygons[I] do
    begin
      T1.X := (fNodes[Indices[0]].Loc.X + fNodes[Indices[1]].Loc.X + fNodes[Indices[2]].Loc.X) / 3;
      T1.Y := (fNodes[Indices[0]].Loc.Y + fNodes[Indices[1]].Loc.Y + fNodes[Indices[2]].Loc.Y) / 3;
      gRenderAux.Text(Round(T1.X), Round(T1.Y) + 1, IntToStr(I), $FF000000);
    end;//}

  {//Simplified obstacle outlines
  if OVERLAY_NAVMESH then
    for I := 0 to fSimpleOutlines.Count - 1 do
    for K := 0 to fSimpleOutlines.Shape[I].Count - 1 do
    with fSimpleOutlines.Shape[I] do
      gRenderAux.Line(Nodes[K], Nodes[(K + 1) mod Count], $FF00FF00, $FF00);//}

  {//NavMesh influences
  if OVERLAY_NAVMESH then
    for I := 0 to fNodeCount - 1 do
    begin
      K := GetBestOwner(I);
      if K <> PLAYER_NONE then
      begin
        //Col := fPlayers[K].FlagColor or $FF000000;
        Col := $FF000000;
        Col2 := IfThen(fNodes[I].Owner[K] = 255, $FFFFFFFF);
        Sz := Max(fNodes[I].Owner[K] - 128, 0) / 64;

        gRenderAux.CircleOnTerrain(
          fNodes[I].Loc.X,
          fNodes[I].Loc.Y, Sz, Col, Col2);
      end;
    end;//}

  {//Defence outlines
  if OVERLAY_DEFENCES then
    for I := 0 to gHands.Count - 1 do
    begin
      fAI.Setup.NewAI
      GetDefenceOutline(I, Outline1, Outline2);

      for K := 0 to High(Outline1) do
        gRenderAux.LineOnTerrain(Outline1[K].A, Outline1[K].B, $FF00FFFF, $FF00);

      for K := 0 to High(Outline2) do
      begin
        gRenderAux.LineOnTerrain(Outline2[K].A, Outline2[K].B, $A000FF00);
        T1 := KMPointF(Outline2[K].A);
        T2 := KMPerpendecular(Outline2[K].A, Outline2[K].B);
        gRenderAux.LineOnTerrain(T1.X, T1.Y, T2.X, T2.Y, $A000FF00);
      end;
    end;//}
end;


end.

{
// The following code was removed and replaced by NavMesh alternative
TNode = record
    Loc: TKMPoint;
    Nearby: array of Word; //Indexes of connected nodes
    Owner: array [0..MAX_HANDS-1] of Byte;
  end;
//Copy ownership values from influence map
//  for now those values are more accurate
procedure TKMNavMesh.UpdateOwnership;
var
  I, K: Integer;
begin
  if not AI_GEN_NAVMESH then Exit;

  for I := 0 to fNodeCount - 1 do
    for K := 0 to gHands.Count - 1 do
      fNodes[I].Owner[K] := Min(255, Max(
        Max(fInfluences.Ownership[K, Max(fNodes[I].Loc.Y, 1), Max(fNodes[I].Loc.X, 1)],
            fInfluences.Ownership[K, Max(fNodes[I].Loc.Y, 1), Min(fNodes[I].Loc.X+1, gTerrain.MapX - 1)]),
        Max(fInfluences.Ownership[K, Min(fNodes[I].Loc.Y+1, gTerrain.MapY - 1), Max(fNodes[I].Loc.X, 1)],
            fInfluences.Ownership[K, Min(fNodes[I].Loc.Y+1, gTerrain.MapY - 1), Min(fNodes[I].Loc.X+1, gTerrain.MapX - 1)])
            ));
end;
//}
