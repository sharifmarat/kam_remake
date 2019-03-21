unit KM_NavMeshGenerator;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
  System.Diagnostics, System.TimeSpan,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  KromUtils, KM_CommonUtils;

type

  TKMNavMeshByteArray = array[-1..257,-1..257] of Byte;

  // Borders
  TKMBord = record
    Node, Prev, Next, NextY: Word;
  end;
  TKMBordInfo = record
    Count: Word;
    Borders: array[0..6000] of TKMBord;
  end;

  // PolyLines
  PPolyLine = ^TKMPolyLine;
  TKMPolyLine = record
    Node,Polygon: Word;
    Next: PPolyLine;
  end;
  PPolyLines = ^TKMPolyLines;
  TKMPolyLines = record
  	LeftEdge, RightEdge, FutureLeftEdge, FutureRightEdge: Word;
    FirstLine, LastLine: PPolyLine;
  end;

  TDebugLines = record // Only for debug
    Count: Word;
    Lines: array[0..15000] of record
      P1,P2: TKMPoint;
    end;
  end;

  TPolygon = record
      CenterPoint: TKMPoint;
      NearbyCount: Byte; //could be 0 .. 3
      Poly2PointStart, Poly2PointCnt: Word; // Indexes of fPolygon2PointArr (points which are part of this polygon)
      Indices: array [0..2] of Word; //Neighbour nodes
      Nearby: array [0..2] of Word; //Neighbour polygons
      NearbyPoints: array [0..2] of TKMPoint; // Center points
    end;
  TPolygonArray = array of TPolygon;

  // NavMeshGenerator create Navigation mesh
  TKMNavMeshGenerator = class
  private
    fMapX, fMapY: Word; // Limits of arrays
    //Keep a copy of these temp arrays for debug rendering
    fInnerPointStartIdx, fInnerPointEndIdx: Word;
    fBordByY, fIdxArr: array[0..256] of Word;
    fBord: TKMBordInfo;
    fDL, fDL2: TDebugLines;

    //Working data
    fNodeCount, fBorderNodeCount, fPolyCount: Integer;
    fNodes,fBorderNodes: TKMPointArray;            // Nodes
    fPolygons: TPolygonArray;         // Polygons

    //Building the navmesh from terrain
    function Intersect(aX1, aY1, aX2, aY2: Word): Boolean; overload;
    function Intersect(aX1, aY1, aX2, aY2: TKMPoint): Boolean; overload;
    function ExtractNodes(): TKMNavMeshByteArray;
    procedure AddInnerNodes(aW: TKMNavMeshByteArray);
    procedure PolygonTriangulation();
    procedure PrettyPoly();

    // Measure performance
    {$IFDEF MSWINDOWS}
    function TimeGetUsec(): Int64;
    {$ENDIF}
  public
    // Use properties to access new NavMesh so the generation may be independent on the existing NavMesh
    property NodeCount: Integer read fNodeCount;
    property PolygonCount: Integer read fPolyCount;
    property Nodes: TKMPointArray read fNodes;
    property Polygons: TPolygonArray read fPolygons;

    constructor Create();
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    procedure GenerateNewNavMesh();
    procedure Paint(const aRect: TKMRect);
  end;


implementation
uses
  SysUtils, Math,
  KM_Terrain, KM_HandsCollection, KM_RenderAux,
  KM_NavMesh,
  Dialogs;


const
  UNVISITED_OBSTACLE = 255;
  VISITED_OBSTACLE = UNVISITED_OBSTACLE - 1;
  NODE_IN_WALKABLE_AREA = 1;
  NODE_IN_OBSTACLE = VISITED_OBSTACLE - 1;
  FILTER_EDGES_MAX_EDGE_DISTANCE = 6;
  FILTER_EDGES_MAX_TOLERANCE = 0.5;
  INNER_EDGE_BORDER_STEP = 4;
  INNER_EDGE_STEP = 5;


{ TKMNavMeshGenerator }
constructor TKMNavMeshGenerator.Create();
begin
  inherited Create;
end;


destructor TKMNavMeshGenerator.Destroy();
begin
  inherited;
end;


procedure TKMNavMeshGenerator.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('KMNavMeshGenerator');
  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
end;


procedure TKMNavMeshGenerator.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('KMNavMeshGenerator');
  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);
end;


procedure TKMNavMeshGenerator.GenerateNewNavMesh();
var
  W: TKMNavMeshByteArray;
{$IFDEF MSWINDOWS}
  TimeStart,TimeDiff: UInt64;
  a,b,c,d: UInt64;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  TimeStart := TimeGetUsec();
  {$ENDIF}

  W := ExtractNodes();
  a := TimeGetUsec() - TimeStart;

  AddInnerNodes(W);
  b := TimeGetUsec() - TimeStart - a;

  PolygonTriangulation();
  c := TimeGetUsec() - TimeStart - b;

  PrettyPoly();
  d := TimeGetUsec() - TimeStart - c;
  if (a > 0) then a := a * 1;
  if (b > 0) then b := b * 1;
  if (c > 0) then c := c * 1;
  if (d > 0) then d := d * 1;

  {$IFDEF MSWINDOWS}
  TimeDiff := TimeGetUsec() - TimeStart;
  if (TimeDiff > 0) then TimeDiff := TimeDiff * 1; // Make sure that compiler does not f--ck up the variable TimeDiff
  {$ENDIF}
end;



{$IFDEF MSWINDOWS}
function TKMNavMeshGenerator.TimeGetUsec(): Int64;
var
  freq: Int64;
  newTime: Int64;
  factor: Double;
begin
  QueryPerformanceFrequency(freq);
  QueryPerformanceCounter(newTime);
  factor := 1000000 / freq; // Separate calculation to avoid "big Int64 * 1 000 000" overflow
  Result := Round(newTime * factor);
end;
{$ENDIF}


function TKMNavMeshGenerator.Intersect(aX1, aY1, aX2, aY2: Word): Boolean;
begin
  Result := Intersect(fNodes[aX1], fNodes[aY1], fNodes[aX2], fNodes[aY2]);
end;

function TKMNavMeshGenerator.Intersect(aX1, aY1, aX2, aY2: TKMPoint): Boolean;
  // Check if points are on segment
  function OnSegment(aP1,aP2,aP3: TKMPoint): Boolean;
  begin
    Result := (aP2.x <= max(aP1.x, aP3.x)) AND
              (aP2.x >= min(aP1.x, aP3.x)) AND
              (aP2.y <= max(aP1.y, aP3.y)) AND
              (aP2.y >= min(aP1.y, aP3.y));
  end;
  // Check orientation of the points
  function Orientation(aP1,aP2,aP3: TKMPoint): Integer;
  begin
    Result := (aP2.y - aP1.y) * (aP3.x - aP2.x) - (aP2.x - aP1.x) * (aP3.y - aP2.y);
    if (Result > 0) then
      Result := 1 // Clockwise
    else if (Result < 0) then
      Result := 2; // Counterclockwise
  end;
var
  o1,o2,o3,o4: Integer;
begin
  // Find the four orientations needed for general and special cases
  o1 := Orientation(aX1, aY1, aX2);
  o2 := Orientation(aX1, aY1, aY2);
  o3 := Orientation(aX2, aY2, aX1);
  o4 := Orientation(aX2, aY2, aY1);
  // General case
  Result := (o1 <> o2) AND (o3 <> o4);
  // Special Cases
  if not Result then
  begin
    // P1, Q1 and P2 are colinear and P2 lies on segment p1q1
    if      (o1 = 0) AND OnSegment(aX1, aX2, aY1) then
      Result := True
    // P1, Q1 and Q2 are colinear and Q2 lies on segment p1q1
    else if (o2 = 0) AND OnSegment(aX1, aY2, aY1) then
      Result := True
    // P2, Q2 and P1 are colinear and P1 lies on segment p2q2
    else if (o3 = 0) AND OnSegment(aX2, aX1, aY2) then
      Result := True
    // P2, Q2 and Q1 are colinear and Q1 lies on segment p2q2
    else if (o4 = 0) AND OnSegment(aX2, aY1, aY2) then
      Result := True;
  end;
end;




function TKMNavMeshGenerator.ExtractNodes(): TKMNavMeshByteArray;
  type
    TStartEndIdxs = record
      SIdx: Word;
      EIdx: Word;
    end;
var
  ShapeCnt: Integer;
  ShapeIdxArr: array[0..256*64] of TStartEndIdxs;
  NodeMap: array[0..256,0..256] of Word;
  EndfBordByY: array[0..256] of Word;
  W: TKMNavMeshByteArray;

  // Use relative coordinate system: visit surounding tiles and rotate coordinate system around walkable area
  procedure ScanObstacle(aStartPoint, aEndPoint, aInitDir: TKMPoint);
  const
    // Correct position of the point according to direction of move in the obstacle
    // It is necessary to have just 1 border point for 1 tile (example: we move on obstacle with width 1 tile)
    DirCorrArr:          array[-2..2] of TKMPoint = ( (X:-1;Y:0),  (X:0;Y:0),  (X:0;Y:0), (X:-1;Y:-1), (X:0;Y:-1) );
    DirCorrInnerTileArr: array[-2..2] of TKMPoint = ( (X:-1;Y:-1), (X:-1;Y:0), (X:0;Y:0), (X:0;Y:-1),  (X:0;Y:0) );
  var
    InnerTile, FinPointReached: Boolean;
    X,Y,{Dir,} Overflow: Integer;
    v, CorrP, FinPoint: TKMPoint;
  begin
    X := aStartPoint.X;
    Y := aStartPoint.Y;
    v := aInitDir; // Init vector
    // Specify final point (for special case when W[Y,X+2] is walkable tile we have to visit also W[Y+1,X])
    if (W[Y+1,X] = UNVISITED_OBSTACLE) then
      FinPoint := KMPoint(X,Y+1)
    else
      FinPoint := aStartPoint;
    FinPointReached := False;
    Overflow := 0;
    repeat
      Overflow := Overflow + 1;
      InnerTile := False;
      // Find next CanBeVisited tile and rotate vector
      if (W[ Y-v.X, X+v.Y ] > NODE_IN_WALKABLE_AREA) then // Left
      begin
        InnerTile := True; // This tiles have neighbor tiles unwalkable
        v := KMPoint(+v.Y,-v.X);
      end
      else if (W[ Y+v.Y, X+v.X ] > NODE_IN_WALKABLE_AREA) then // Forward
      begin
        //v := KMPoint(+v.X,+v.Y);
      end
      else if (W[ Y+v.X, X-v.Y ] > NODE_IN_WALKABLE_AREA) then // Right
      begin
        v := KMPoint(-v.Y,+v.X);
      end
      else if (W[ Y-v.Y, X-v.X ] > NODE_IN_WALKABLE_AREA) then // Backward
      begin
        v := KMPoint(-v.X,-v.Y);
      end
      else
        break;
      // Add point in case that distance is greater than threshold
      if InnerTile then
        CorrP := KMPointAdd( KMPoint(X,Y), DirCorrInnerTileArr[v.X+v.Y*2] )
      else
        CorrP := KMPointAdd( KMPoint(X,Y), DirCorrArr[v.X+v.Y*2] );
      if ((W[CorrP.Y,CorrP.X] <> NODE_IN_WALKABLE_AREA) AND (W[CorrP.Y,CorrP.X] <> NODE_IN_OBSTACLE)) then
      begin
        if (W[CorrP.Y,CorrP.X] = 0) then
          W[CorrP.Y,CorrP.X] := NODE_IN_WALKABLE_AREA
        else
          W[CorrP.Y,CorrP.X] := NODE_IN_OBSTACLE;
        fBorderNodes[fBorderNodeCount] := CorrP;
        NodeMap[CorrP.Y,CorrP.X] := fBorderNodeCount;
        Inc(fBorderNodeCount);
      end;
      // Mark visited obstacle and move into next point
      if (W[Y,X] = UNVISITED_OBSTACLE) then
        W[Y,X] := VISITED_OBSTACLE;
      X := X + v.X;
      Y := Y + v.Y;
      FinPointReached := FinPointReached OR KMSamePoint(KMPoint(X,Y),FinPoint);
    until (FinPointReached AND KMSamePoint(KMPoint(X,Y),aStartPoint)) OR (Overflow > 65536);
  end;

  procedure AddBorderPoint(aPoint: TKMPoint);
  var
    X,Y,K,PrevK: Integer;
  begin
    fNodes[fNodeCount] := aPoint;
    with fBord.Borders[fBord.Count] do
    begin
      Node := fNodeCount;
      Prev := fBord.Count - 1;
      Next := fBord.Count + 1;
      // Sort and store new border line into array
      Y := aPoint.Y;
      X := aPoint.X;
      PrevK := 0;
      K := fBordByY[Y];
      while (K > 0) AND (X > fNodes[ fBord.Borders[K].Node ].X) do
      begin // Insertion sort - in real 10-15 points; in borders max 50 => no need for quick sort
        PrevK := K;
        K := fBord.Borders[K].NextY;
      end;
      if (PrevK = 0) then
        fBordByY[Y] := fBord.Count
      else
        fBord.Borders[PrevK].NextY := fBord.Count;
      NextY := K;
    end;
    Inc(fBord.Count);
    Inc(fNodeCount);
  end;

  // Filter number of lines in all shapes
  procedure FilterEdges();
  var
    ShapeCheck: Boolean;
    K,L,M,X,Y, StartIdx, EndIdx, StartBorderIdx, Overflow1, Overflow2: Integer;
    InvDenominator, a,b,c: Single;
    StrP,EndP: TKMPoint;
  begin
    //FillChar(EndfBordByY, SizeOf(EndfBordByY), #0);
    fNodeCount := 1;
    fBord.Count := 1;
    // For all shapes
    for K := 0 to ShapeCnt-1 do
    begin
      StartIdx := ShapeIdxArr[K].SIdx;
      StartBorderIdx := fBord.Count;
      AddBorderPoint( fBorderNodes[StartIdx] );
      // Check all nodes in every shape
      Overflow1 := 0;
      while (StartIdx < ShapeIdxArr[K].EIdx) AND (Overflow1 < 65536) do
      begin
        Overflow1 := Overflow1 + 1;
        Overflow2:= 0;
        EndIdx := Min(StartIdx + FILTER_EDGES_MAX_EDGE_DISTANCE, ShapeIdxArr[K].EIdx);
        repeat
          Overflow2 := Overflow2 + 1;
          ShapeCheck := True;
          // Check if removed points are not too far from the new line
          a := - fBorderNodes[StartIdx].Y     + fBorderNodes[EndIdx-1].Y;
          b := + fBorderNodes[StartIdx].X     - fBorderNodes[EndIdx-1].X;
          c := - fBorderNodes[StartIdx].X * a - fBorderNodes[StartIdx].Y * b;
          InvDenominator := 1 / (a*a + b*b);
          for L := EndIdx-1 downto StartIdx+1 do
            if (  ( sqr(a*fBorderNodes[L].X + b*fBorderNodes[L].Y + c) * InvDenominator ) > FILTER_EDGES_MAX_TOLERANCE  ) then
            begin
              ShapeCheck := False;
              Dec(EndIdx);
              break;
            end;
          // Check if new line does not intersect with existing line
          if ShapeCheck then
          begin
            StrP := fBorderNodes[StartIdx];
            EndP := fBorderNodes[EndIdx];
            for X := Min(StrP.X,EndP.X) to Max(StrP.X,EndP.X) do
            begin
              for Y := Min(StrP.Y,EndP.Y) to Max(StrP.Y,EndP.Y) do
                if (NodeMap[Y,X] > 0) AND ((NodeMap[Y,X] < StartIdx - 1) OR (NodeMap[Y,X] > EndIdx + 1)) then
                //if (NodeMap[Y,X] > 0) then
                begin
                  // Make sure that the selection does not colide with starting point
                  if   (not KMSamePoint(fBorderNodes[ NodeMap[Y,X]+1 ], StrP) AND Intersect(StrP, EndP, KMPoint(X,Y), fBorderNodes[ NodeMap[Y,X]+1 ]))
                    OR (not KMSamePoint(fBorderNodes[ NodeMap[Y,X]-1 ], EndP) AND Intersect(StrP, EndP, KMPoint(X,Y), fBorderNodes[ NodeMap[Y,X]-1 ])) then
                  begin
                    ShapeCheck := False;
                    Dec(EndIdx);
                    break;
                  end;
                end;
              if not ShapeCheck then
                break;
            end;
          end;
        until ShapeCheck OR (Overflow2 > 65536);

        // Emergency function (prevents crashes in case of bugs)
        if (StartIdx = EndIdx) then
        begin
          Inc(EndIdx);
        end;

        // Create clean borders
        AddBorderPoint( fBorderNodes[EndIdx] );
        StartIdx := EndIdx;
      end;
      // Mark end of shape
      //if (fBord.Count - StartBorderIdx - 1 < 3) then
      //  fBord.Count := StartBorderIdx
      //else
      begin
        fBord.Borders[StartBorderIdx].Prev := fBord.Count - 1;
        fBord.Borders[fBord.Count - 1].Next := StartBorderIdx;
      end;
    end;
  end;


  // Mark all edges
  procedure MarkEdges();
  const
    MIN_BORDERS_IN_NEW_OBSTACLE = 3;
  var
    Walkable: Boolean;
    X,Y,Cnt: Integer;
  begin
    // Mark edges in the map
    FillChar(ShapeIdxArr, SizeOf(ShapeIdxArr), #0);
    FillChar(NodeMap, SizeOf(NodeMap), #0);
    fBorderNodeCount := 2; // Start at index 2 -> 0. is reserved and 1. will be the end point so circle is complete
    ShapeCnt := 0;
    Walkable := False;
    for Y := 1 to fMapY - 1 do
    for X := 1 to fMapX do // -1 cannot be here
    begin
      if Walkable AND (W[Y,X] = UNVISITED_OBSTACLE) then
      begin
        Cnt := fBorderNodeCount;
        ScanObstacle(KMPoint(X,Y), KMPoint(X,Y), KMPoint(0,-1));
        // Check the number of borders in the new obstacle
        if (fBorderNodeCount - Cnt < MIN_BORDERS_IN_NEW_OBSTACLE) then
          fBorderNodeCount := Cnt // Remove borders / ignore them
        else
        begin
          // Mark start and end index of new obstacle
          ShapeIdxArr[ShapeCnt].SIdx := Cnt;
          ShapeIdxArr[ShapeCnt].EIdx := fBorderNodeCount-1;
          Inc(ShapeCnt);
          // Add end point before the start point and vice versa so points in ShapeIdxArr are interconnected by indexes
          fBorderNodes[Cnt-1] := fBorderNodes[fBorderNodeCount-1];
          fBorderNodes[fBorderNodeCount] := fBorderNodes[Cnt];
          Inc(fBorderNodeCount);
          Inc(fBorderNodeCount); // Reserve next node
        end;
      end;
      Walkable := (W[Y,X] = 0);

    end;
    // Filter edges
    FilterEdges();
  end;

var
  X,Y: Integer;
begin
  fMapX := gTerrain.MapX;
  fMapY := gTerrain.MapY;
  // Clear array
  fNodeCount := 1;
  fBorderNodeCount := 1;
  SetLength(fNodes, 5000);
  SetLength(fBorderNodes, 255*255);
  FillChar(fNodes[0], SizeOf(fNodes[0]) * Length(fNodes), #0);
  FillChar(fBorderNodes[0], SizeOf(fBorderNodes[0]) * Length(fBorderNodes), #0);
  // Fill borders
  FillChar(W, SizeOf(W), #0);
  X := fMapX;
  for Y := 0 to fMapY do
  begin
    W[Y,0] := UNVISITED_OBSTACLE;
    W[Y,X] := UNVISITED_OBSTACLE;
  end;
  Y := fMapY;
  for X := 0 to fMapX do
  begin
    W[0,X] := UNVISITED_OBSTACLE;
    W[Y,X] := UNVISITED_OBSTACLE;
  end;
  // Fill map
  for Y := 1 to fMapY - 1 do
  for X := 1 to fMapX - 1 do
    if not (tpOwn in gTerrain.Land[Y,X].Passability) then
      W[Y,X] := UNVISITED_OBSTACLE;
  // Find edges of polygons
  FillChar(fBordByY, SizeOf(fBordByY), #0);
  FillChar(EndfBordByY, SizeOf(EndfBordByY), #0);
  FillChar(fIdxArr, SizeOf(fIdxArr), #0);
  MarkEdges();

  {
  for X := 1 to fBorderNodeCount-1 do
  begin
    gRenderAux.Text(fBorderNodes[X].X+0.25, fBorderNodes[X].Y+0.4, IntToStr(X), $FF00FF00);
  end;

  for X := 0 to ShapeCnt-1 do
    for Y := ShapeIdxArr[X].SIdx to ShapeIdxArr[X].EIdx do
    begin
        gRenderAux.LineOnTerrain(fBorderNodes[Y-1], fBorderNodes[Y], $FF00FF00);
    end;
  //}

  Result := W;
end;


procedure TKMNavMeshGenerator.AddInnerNodes(aW: TKMNavMeshByteArray);
var
  E: TKMNavMeshByteArray;
  procedure FillArea(aStep: Word; aInitPoint: TKMPoint);
  var
    X,Y: Integer;
  begin
    for Y := Max(1, aInitPoint.Y - aStep) to Min(fMapY - 1, aInitPoint.Y + aStep) do
    for X := Max(1, aInitPoint.X - aStep) to Min(fMapX - 1, aInitPoint.X + aStep) do
      E[Y,X] := 1;//Max(  E[Y,X], Min( abs(aInitPoint.Y - Y), abs(aInitPoint.X - X) )  );
  end;
var
  K,X,Y: Integer;
begin
  FillChar(E, SizeOf(E), #0);
  fInnerPointStartIdx := fNodeCount;
  for K := 1 to fBord.Count - 1 do
    FillArea( INNER_EDGE_BORDER_STEP+1, fNodes[ fBord.Borders[K].Node ] );

  for Y := 1 to fMapY - 1 do
  for X := 1 to fMapX - 1 do
    if (E[Y,X] = 0) AND (aW[Y,X] < VISITED_OBSTACLE) then
    begin
      fNodes[fNodeCount] := KMPoint(X,Y);
      Inc(fNodeCount);
      FillArea( INNER_EDGE_STEP+1, KMPoint(X,Y) );
    end;
  fInnerPointEndIdx := fNodeCount;

  FillChar(fIdxArr, SizeOf(fIdxArr), #0);
  Y := 0;
  K := fInnerPointStartIdx;
  while (K < fInnerPointEndIdx) do
    if (fNodes[K].Y > Y) then
    begin
      fIdxArr[Y] := K;
      Y := Y + 1;
    end
    else
      K := K + 1;
  for Y := Y to High(fIdxArr) do
    fIdxArr[Y] := fInnerPointEndIdx;
end;



procedure TKMNavMeshGenerator.PolygonTriangulation();
var
  LineArrayCnt: Word;
  LineArray: array of PPolyLines;

  procedure ConfirmLine(aIdx1, aIdx2: Word);
  begin
    with fDL.Lines[fDL.Count] do
    begin
      p1 := fNodes[  fBord.Borders[ aIdx1 ].Node  ];
      p2 := fNodes[  fBord.Borders[ aIdx2 ].Node  ];
    end;
    Inc(fDL.Count);
  end;
  procedure AddLine(aP1,aP2: TKMPoint);
  begin
    with fDL.Lines[fDL.Count] do
    begin
      p1 := aP1;
      p2 := aP2;
    end;
    Inc(fDL.Count);
  end;
  procedure ConfirmLine2(aIdx1, aIdx2: Word);
  begin
    with fDL2.Lines[fDL2.Count] do
    begin
      p1 := fNodes[  fBord.Borders[ aIdx1 ].Node  ];
      p2 := fNodes[  fBord.Borders[ aIdx2 ].Node  ];
    end;
    Inc(fDL2.Count);
  end;
  procedure AddLine2(aP1,aP2: TKMPoint);
  begin
    with fDL2.Lines[fDL2.Count] do
    begin
      p1 := aP1;
      p2 := aP2;
    end;
    Inc(fDL2.Count);
  end;

  procedure ConnectPolygons(aNewPoly1, aPossiblePoly2: Word);
  begin
    if (aPossiblePoly2 = 0) then
      Exit;
    with fPolygons[aNewPoly1] do
      if (NearbyCount < 3) then
      begin
        Nearby[NearbyCount] := aPossiblePoly2;
        Inc(NearbyCount);
      end;
    with fPolygons[aPossiblePoly2] do
      if (NearbyCount < 3) then
      begin
        Nearby[NearbyCount] := aNewPoly1;
        Inc(NearbyCount);
      end;
  end;

  procedure AddPolygon(var aPPL1, aPPL2: PPolyLine); overload;
  begin
    with fPolygons[fPolyCount] do // Array was filled with 0 bytes in FillChar
    begin
      Indices[0] := aPPL1^.Node;
      Indices[1] := aPPL2^.Node;
      Indices[2] := aPPL2^.Next^.Node;
      ConnectPolygons(fPolyCount, aPPL1^.Polygon);
      ConnectPolygons(fPolyCount, aPPL2^.Polygon);
    end;
    aPPL1^.Polygon := fPolyCount;
    aPPL2^.Polygon := fPolyCount; // Maybe unused (by: NewLine)
    Inc(fPolyCount);
  end;

  function AddPolygon(aIdx: Word; var aPPL: PPolyLine): Word; overload;
  begin
    with fPolygons[fPolyCount] do // Array was filled with 0 bytes in FillChar
    begin
      Indices[0] := aPPL^.Node;
      Indices[1] := aPPL^.Next^.Node;
      Indices[2] := aIdx;
      ConnectPolygons(fPolyCount, aPPL^.Polygon);
    end;
    aPPL^.Polygon := fPolyCount;
    Result := fPolyCount;
    Inc(fPolyCount);
  end;


  function NewLine(aNode: Word; aNext: PPolyLine; aPolygon: Word = 0): PPolyLine;
  begin
    New(Result);
    Result^.Node := aNode;
    Result^.Polygon := aPolygon;
    Result^.Next := aNext;
  end;


  function IsRightSide(P1,P2,Point: TKMPoint): Boolean;
  begin
    Result := ((P2.X - P1.X) * (Point.Y - P1.Y) > (P2.Y - P1.Y) * (Point.X - P1.X));
  end;

  procedure TryConnect(aRemovedNode, aLineIdx: Word; aLeftDirection: Boolean; aBorderPoly: Boolean = False; aCloseArea: Boolean = False);
  const
    SQR_MAX_RADIUS = 20*20;
  var
    PPoly1, PPoly2, PPoly3: PPolyLine;
  begin
    with LineArray[aLineIdx]^ do
    begin
      if (FirstLine = LastLine) OR (FirstLine^.Next = LastLine) // Only 2 verticles
        OR (FirstLine^.Node = aRemovedNode) OR (LastLine^.Node = aRemovedNode) then // Border node
        Exit;

      PPoly3 := FirstLine;
      PPoly2 := PPoly3^.Next;
      PPoly1 := PPoly2^.Next;
      while (PPoly2.Node <> aRemovedNode) do
      begin
        PPoly3 := PPoly2;
        PPoly2 := PPoly1;
        PPoly1 := PPoly1^.Next;
      end;

      if not aCloseArea AND (KMDistanceSqr(fNodes[ PPoly1^.Node ],fNodes[ PPoly3^.Node ]) > SQR_MAX_RADIUS) then
        Exit;

      if (aBorderPoly AND ( // Force to create polygon in case that it is border point
           (aLeftDirection     AND (fNodes[ PPoly3^.Node ].X > fNodes[ PPoly1^.Node ].X)) OR
           (not aLeftDirection AND (fNodes[ PPoly1^.Node ].X < fNodes[ PPoly3^.Node ].X))
         )) OR IsRightSide(fNodes[ PPoly3^.Node ],fNodes[ PPoly2^.Node ],fNodes[ PPoly1^.Node ])
         OR aCloseArea then // Force to create polygon in case that area will be closed
      begin
AddLine(fNodes[ PPoly3^.Node ],fNodes[ PPoly1^.Node ]);
        AddPolygon(PPoly3, PPoly2);
        PPoly3^.Next := PPoly1;
        Dispose(PPoly2); // Remove verticle from memory
        TryConnect( ifthen(aLeftDirection, PPoly1^.Node, PPoly3^.Node), aLineIdx, aLeftDirection, aBorderPoly, aCloseArea)
      end;
    end;
  end;

  procedure AddNewBorder(aIdx, aFutureIdx, aLineIdx: Word; aLeftDirection: Boolean; aTryConnect: Boolean = True; aCloseArea: Boolean = False);
  begin
    ConfirmLine(aIdx, aFutureIdx);
    with LineArray[aLineIdx]^ do
    begin
      if aLeftDirection then
      begin
        FirstLine := NewLine( fBord.Borders[aIdx].Node, FirstLine );
        if aTryConnect then
          TryConnect(fBord.Borders[LeftEdge].Node, aLineIdx, aLeftDirection, True, aCloseArea);
        LeftEdge := aIdx;
        FutureLeftEdge := aFutureIdx;
      end
      else
      begin
        LastLine^.Next := NewLine( fBord.Borders[aIdx].Node, nil );
        LastLine := LastLine^.Next;
        if aTryConnect then
          TryConnect(fBord.Borders[RightEdge].Node, aLineIdx, aLeftDirection, True, aCloseArea);
        RightEdge := aIdx;
        FutureRightEdge := aFutureIdx;
      end;
    end;
  end;

  procedure DisposeArea(aIdx: Word; aSkipPolyLine: Boolean = False);
  var
    PolyLine, OldPL: PPolyLine;
  begin
    if not aSkipPolyLine then
    begin
      PolyLine := LineArray[aIdx]^.FirstLine;
      while (PolyLine <> LineArray[aIdx]^.LastLine) AND (PolyLine <> nil) do
      begin
        OldPL := PolyLine;
        PolyLine := PolyLine^.Next;
        Dispose(OldPL);
      end;
      Dispose(PolyLine);
    end;
    // Free memory of second lines and update LineArray
    Dispose(LineArray[aIdx]);
    Dec(LineArrayCnt);
    LineArray[aIdx] := LineArray[LineArrayCnt];
  end;

  procedure AddNewWalkableArea(aIdx: Word);
  begin
    // Add new area
    if (Length(LineArray) <= LineArrayCnt) then
      SetLength(LineArray, LineArrayCnt + 20);
    New(LineArray[LineArrayCnt]);
    with LineArray[LineArrayCnt]^ do
    begin
      FirstLine := NewLine(fBord.Borders[aIdx].Node, nil);
      LastLine := FirstLine;
      LeftEdge := aIdx;
      RightEdge := aIdx;
      FutureLeftEdge := fBord.Borders[aIdx].Next;
      FutureRightEdge := fBord.Borders[aIdx].Prev;
ConfirmLine(aIdx, FutureLeftEdge);
ConfirmLine(aIdx, FutureRightEdge);
    end;
    Inc(LineArrayCnt);
  end;

  procedure DivideWalkableArea(aIdx, aLineIdx: Word; aPPrevLine, aPActLine: PPolyLine);
  var
    NewPolyIdx: Word;
  begin
    if (Length(LineArray) <= LineArrayCnt) then
      SetLength(LineArray, LineArrayCnt + 20);
    New(LineArray[LineArrayCnt]);
    // Copy left border from old line
    LineArray[LineArrayCnt]^.FirstLine := LineArray[aLineIdx]^.FirstLine;
    LineArray[LineArrayCnt]^.LeftEdge := LineArray[aLineIdx]^.LeftEdge;
    LineArray[LineArrayCnt]^.FutureLeftEdge := LineArray[aLineIdx]^.FutureLeftEdge;
    if (aPPrevLine = nil) then // Create right border of NEW line if does not exist
    begin
      aPPrevLine := NewLine( aPActLine^.Node, nil );
      LineArray[LineArrayCnt]^.FirstLine := aPPrevLine;
    end
    else if (aPActLine = nil) then // Create left border of OLD line if does not exist
    begin
      aPActLine := NewLine( aPPrevLine^.Node, nil );
      LineArray[aLineIdx]^.LastLine := aPActLine;
    end
    else // Copy the line for the first area + secure connection
    begin
      aPPrevLine^.Next := NewLine( aPActLine^.Node, nil );
      aPPrevLine := aPPrevLine^.Next;
    end;

AddLine2(fNodes[  fBord.Borders[ aIdx ].Node  ], fNodes[ aPPrevLine^.Node ] );
    LineArray[LineArrayCnt]^.LastLine := aPPrevLine;
    AddNewBorder(aIdx, fBord.Borders[aIdx].Prev, LineArrayCnt, False, False);
    NewPolyIdx := fPolyCount; // Polygon with this index does not exist yet
    TryConnect(aPPrevLine.Node, LineArrayCnt, False); // Now multiple new polygons may be created (first one will be connected with second side)

AddLine2(fNodes[  fBord.Borders[ aIdx ].Node  ], fNodes[ aPActLine^.Node ] );
    LineArray[aLineIdx]^.FirstLine := aPActLine;
    AddNewBorder(aIdx, fBord.Borders[aIdx].Next, aLineIdx, True, False);
    // New polygon was created -> copy information to the second area and it will be connected automatically
    if (NewPolyIdx < fPolyCount) then
      LineArray[aLineIdx]^.FirstLine^.Polygon := NewPolyIdx
    else // The polygon was not created -> it must be created from this side -> secure transition
      //LineArray[LineArrayCnt]^.LastLine^.Polygon := NewPolyIdx;
      LineArray[LineArrayCnt]^.FirstLine^.Polygon := NewPolyIdx;
    TryConnect(aPActLine.Node, aLineIdx, True);

    Inc(LineArrayCnt);
  end;

  function InsertPoint(aIdx, aLineIdx: Word; aBorder: Boolean): Boolean;
  var
    Point: TKMPoint;
    PPrevLine, PActLine: PPolyLine;
  begin
    // Find relative position of point in stack
    Point := fNodes[ ifthen(aBorder, fBord.Borders[aIdx].Node, aIdx) ];
    PPrevLine := nil;
    PActLine := LineArray[aLineIdx]^.FirstLine;
    repeat
      if (fNodes[ PActLine^.Node ].X >= Point.X ) then
        break;
      PPrevLine := PActLine;
      PActLine := PActLine^.Next;
    until (PActLine = nil);
    // Select the right method -> in case of border point divide area
    if aBorder then
      DivideWalkableArea(aIdx, aLIneIdx, PPrevLine, PActLine)
    else if (PPrevLine <> nil) AND (PActLine <> nil) then // In case of ordinary point just add new element + ignore border points
    begin
AddLine2(fNodes[ aIdx ], fNodes[ PPrevLine^.Node ] );
AddLine2(fNodes[ aIdx ], fNodes[ PActLine^.Node ] );
      PPrevLine^.Next := NewLine(aIdx, PActLine, AddPolygon(aIdx, PPrevLine)); // Add line
      // Try to connect new triangles
      TryConnect(PActLine.Node, aLineIdx, False);
      TryConnect(PPrevLine.Node, aLineIdx, True);
    end;
    Result := True;
  end;

  procedure CheckArea(aIdx: Word; aBorder: Boolean);
  var
    Inserted: Boolean;
    I: Integer;
    Point, LeftP, FutureLeftP, RightP, FutureRightP: TKMPoint;
  begin
    Inserted := False;
    Point := fNodes[ ifthen(aBorder, fBord.Borders[aIdx].Node, aIdx) ];
    // Check point position in area
    for I := 0 to LineArrayCnt - 1 do
    begin
      LeftP :=  fNodes[  fBord.Borders[ LineArray[I]^.LeftEdge ].Node  ];
      RightP := fNodes[  fBord.Borders[ LineArray[I]^.RightEdge ].Node  ];
      FutureLeftP :=  fNodes[  fBord.Borders[ LineArray[I]^.FutureLeftEdge ].Node  ];
      FutureRightP := fNodes[  fBord.Borders[ LineArray[I]^.FutureRightEdge ].Node  ];
      if (  ( Min(RightP.X, FutureRightP.X) >= Point.X ) AND ( Max(LeftP.X, FutureLeftP.X) <= Point.X )  )
        OR (  IsRightSide(FutureLeftP,LeftP,Point) AND IsRightSide(RightP,FutureRightP,Point)  ) then
      begin
        Inserted := InsertPoint(aIdx, I, aBorder);
        break;
      end;
    end;
    if not Inserted AND aBorder then // This must be border point or the point will be ignored
      AddNewWalkableArea(aIdx);
  end;

  procedure RemoveWalkableArea(aIdx, ConIdx: Word);
  begin
ConfirmLine2(aIdx, LineArray[ConIdx]^.LeftEdge);
ConfirmLine2(aIdx, LineArray[ConIdx]^.RightEdge);
    //AddPolygon(fBord.Borders[aIdx].Node, LineArray[ConIdx]^.FirstLine);
    AddNewBorder(aIdx, fBord.Borders[aIdx].Prev, ConIdx, False, True, True);
    DisposeArea(ConIdx);
  end;

  procedure MergeWalkableAreas(aIdx, aLeftEdgeLineIdx, aRightEdgeLineIdx: Word);
  begin
ConfirmLine(aIdx, LineArray[aLeftEdgeLineIdx]^.LeftEdge);
ConfirmLine(aIdx, LineArray[aRightEdgeLineIdx]^.RightEdge);
    // Add border points and everything around it
    AddNewBorder(aIdx, aIdx, aLeftEdgeLineIdx, True); // aIdx in second parameter is OK
    AddNewBorder(aIdx, aIdx, aRightEdgeLineIdx, False);
    // Copy border properties
    LineArray[aRightEdgeLineIdx]^.LastLine^.Polygon := LineArray[aLeftEdgeLineIdx]^.FirstLine^.Polygon;
    LineArray[aRightEdgeLineIdx]^.LastLine^.Next :=    LineArray[aLeftEdgeLineIdx]^.FirstLine^.Next; // Next because first point is new border
    LineArray[aRightEdgeLineIdx]^.LastLine :=          LineArray[aLeftEdgeLineIdx]^.LastLine;
    LineArray[aRightEdgeLineIdx]^.RightEdge :=         LineArray[aLeftEdgeLineIdx]^.RightEdge;
    LineArray[aRightEdgeLineIdx]^.FutureRightEdge :=   LineArray[aLeftEdgeLineIdx]^.FutureRightEdge;
    // Free memory of second lines and update LineArray
    Dispose(LineArray[aLeftEdgeLineIdx]^.FirstLine); // Other elements will be copied
    DisposeArea(aLeftEdgeLineIdx, True);
  end;

var
  Y,Idx,K, LeftEdgeLineIdx, RightEdgeLineIdx: Integer;
  PrevY: Integer;
  BorderPoint: TKMPoint;
begin
  fPolyCount := 1;
  SetLength(fPolygons, 5000);
  FillChar(fPolygons[0], SizeOf(fPolygons[0]) * Length(fPolygons), #0);
  //{
  fDL.Count := 0;
  fDL2.Count := 0;
  LineArrayCnt := 0;
  PrevY := 0;
  for Y := Low(fBordByY) to High(fBordByY) do
  begin
    Idx := fBordByY[Y];
    while (Idx <> 0) do
      with fBord.Borders[Idx] do
      begin
        BorderPoint := fNodes[Node];

        if (PrevY < BorderPoint.Y-1) then // Add all middle points which are between new and old Y coord
        begin
          for K := fIdxArr[PrevY] to fIdxArr[BorderPoint.Y - 1] - 1 do
            CheckArea(K, False);
          PrevY := BorderPoint.Y-1;
        end;

        LeftEdgeLineIdx := -1;
        RightEdgeLineIdx := -1;
        // Check connection with existing lines
        for K := 0 to LineArrayCnt - 1 do
        begin
          if (LineArray[K]^.FutureLeftEdge = Idx) then
            LeftEdgeLineIdx := K;
          if (LineArray[K]^.FutureRightEdge = Idx) then
            RightEdgeLineIdx := K;
        end;
        // Select the right action and add the point
        if (LeftEdgeLineIdx > -1) AND (RightEdgeLineIdx > -1) then // Border point can merge 2 areas
        begin
          if (LeftEdgeLineIdx = RightEdgeLineIdx) then
            RemoveWalkableArea(Idx, LeftEdgeLineIdx)
          else
            MergeWalkableAreas(Idx, LeftEdgeLineIdx, RightEdgeLineIdx)
        end
        else if (LeftEdgeLineIdx > -1) then // Left border is expanded
          AddNewBorder(Idx, Next, LeftEdgeLineIdx, True)
        else if (RightEdgeLineIdx > -1) then // Right border is expanded
          AddNewBorder(Idx, Prev, RightEdgeLineIdx, False)
        else
          CheckArea(Idx, True);
        Idx := NextY; // Move to next Idx
      end;
  end;

  // Clean mess (only for debug, in normal case it should not be required)
  while (LineArrayCnt > 0) do
    DisposeArea(LineArrayCnt - 1);
  //}
end;


// Polygon optimalization
procedure TKMNavMeshGenerator.PrettyPoly();
var
  chck: array of boolean;
  cnt: Integer;

  function AnalyzePoints(aNode1, aNode2: Word; var CP1,CP2,DP1,DP2: Word): Boolean;
  var
    K,L: Integer;
  begin
    CP1 := 0; // 0. index is reserved and it is not used in triangulation
    CP2 := 0;
    DP1 := 0;
    DP2 := 0;
    for K := 0 to 2 do
    for L := 0 to 2 do
      with fPolygons[aNode2] do
        if (fPolygons[aNode1].Indices[K] = Indices[L]) then
        begin
          if (CP1 = 0) then
            CP1 := Indices[L]
          else
          begin
            CP2 := Indices[L];
            break;
          end;
        end;
    with fPolygons[aNode1] do
      if      (CP1 <> Indices[0]) AND (CP2 <> Indices[0]) then DP1 := Indices[0]
      else if (CP1 <> Indices[1]) AND (CP2 <> Indices[1]) then DP1 := Indices[1]
      else if (CP1 <> Indices[2]) AND (CP2 <> Indices[2]) then DP1 := Indices[2];
    with fPolygons[aNode2] do
      if      (CP1 <> Indices[0]) AND (CP2 <> Indices[0]) then DP2 := Indices[0]
      else if (CP1 <> Indices[1]) AND (CP2 <> Indices[1]) then DP2 := Indices[1]
      else if (CP1 <> Indices[2]) AND (CP2 <> Indices[2]) then DP2 := Indices[2];
    Result := (CP2 > 0) AND (DP2 > 0);
  end;

  function HaveIndices(aPolyIdx,aIndice1,aIndice2: Word): Boolean;
  begin
    with fPolygons[aPolyIdx] do
      Result := (+ Byte( (Indices[0] = aIndice1) OR (Indices[0] = aIndice2) )
                 + Byte( (Indices[1] = aIndice1) OR (Indices[1] = aIndice2) )
                 + Byte( (Indices[2] = aIndice1) OR (Indices[2] = aIndice2) )) = 2;
  end;

  function BeautifyPoly(ActIdx: Integer): Boolean;
  var
    CP1,CP2,DP1,DP2, BestCP1,BestCP2,BestDP1,BestDP2, SwapPoly1, SwapPoly2: Word;
    K, BestIdx, Idx1, Idx2, Neat: Integer;
    SqrBestDist, SqrDist: Single;
  begin
    Result := False;
    BestCP1 := 0;
    BestCP2 := 0;
    BestDP1 := 0;
    BestDP2 := 0;
    BestIdx := 0;
// Pick up the longest line of triangle which have nearby polygon
    SqrBestDist := 0;
    for K := 0 to fPolygons[ActIdx].NearbyCount - 1 do
      if AnalyzePoints(ActIdx,fPolygons[ActIdx].Nearby[K],CP1,CP2,DP1,DP2) then
      begin
        SqrDist := KMDistanceSqr(fNodes[ CP1 ],fNodes[ CP2 ]) - KMDistanceSqr(fNodes[ DP1 ],fNodes[ DP2 ]);
        if (SqrDist > SqrBestDist) AND Intersect(CP1,CP2,DP1,DP2) then
        begin
          SqrBestDist := SqrDist;
          BestCP1 := CP1;
          BestCP2 := CP2;
          BestDP1 := DP1;
          BestDP2 := DP2;
          BestIdx := fPolygons[ActIdx].Nearby[K];
        end;
      end;
    Result := (SqrBestDist > 0);
    if Result then
    begin
      // Find indexes of nearby polygons which must be swaped to secure connection
      with fPolygons[ ActIdx ] do
        if      (Nearby[0] = 0) OR ((Nearby[0] <> BestIdx) AND HaveIndices(Nearby[0], BestCP1, BestDP1)) then Idx1 := 0
        else if (Nearby[1] = 0) OR ((Nearby[1] <> BestIdx) AND HaveIndices(Nearby[1], BestCP1, BestDP1)) then Idx1 := 1
        else{if (Nearby[2] = 0) OR ((Nearby[2] <> BestIdx) AND HaveIndices(Nearby[2], BestCP1, BestDP1)) then}Idx1 := 2;
      with fPolygons[ BestIdx ] do
        if      (Nearby[0] = 0) OR ((Nearby[0] <> BestIdx) AND HaveIndices(Nearby[0], BestCP2, BestDP2)) then Idx2 := 0
        else if (Nearby[1] = 0) OR ((Nearby[1] <> BestIdx) AND HaveIndices(Nearby[1], BestCP2, BestDP2)) then Idx2 := 1
        else{if (Nearby[2] = 0) OR ((Nearby[2] <> BestIdx) AND HaveIndices(Nearby[2], BestCP2, BestDP2)) then}Idx2 := 2;
      SwapPoly1 := fPolygons[ActIdx].Nearby[Idx1];
      SwapPoly2 := fPolygons[BestIdx].Nearby[Idx2];

      // Actualize connection of nearby polygons
      if (SwapPoly1 > 0) then
        with fPolygons[ SwapPoly1 ] do
          if      (Nearby[0] = ActIdx) then Nearby[0] := BestIdx
          else if (Nearby[1] = ActIdx) then Nearby[1] := BestIdx
          else if (Nearby[2] = ActIdx) then Nearby[2] := BestIdx;
      if (SwapPoly2 > 0) then
        with fPolygons[ SwapPoly2 ] do
          if      (Nearby[0] = BestIdx) then Nearby[0] := ActIdx
          else if (Nearby[1] = BestIdx) then Nearby[1] := ActIdx
          else if (Nearby[2] = BestIdx) then Nearby[2] := ActIdx;

      // Actualize connection of changed polygons
      with fPolygons[BestIdx] do
        if (SwapPoly1 > 0) then
        begin
          Nearby[Idx2] := SwapPoly1;
          NearbyCount := NearbyCount + Byte(SwapPoly2 = 0);
        end
        else
        begin
          NearbyCount := NearbyCount - Byte(SwapPoly2 > 0);
          Nearby[Idx2] := Nearby[NearbyCount];
          Nearby[NearbyCount] := 0; // Zero must be here
        end;
      with fPolygons[ActIdx] do
        if (SwapPoly2 > 0) then
        begin
          Nearby[Idx1] := SwapPoly2;
          NearbyCount := NearbyCount + Byte(SwapPoly1 = 0);
        end
        else
        begin
          NearbyCount := NearbyCount - Byte(SwapPoly1 > 0);
          Nearby[Idx1] := Nearby[NearbyCount];
          Nearby[NearbyCount] := 0; // Zero must be here
        end;

      // Change indices
      with fPolygons[ActIdx] do
        if      (BestCP1 = Indices[0]) then Indices[0] := BestDP2
        else if (BestCP1 = Indices[1]) then Indices[1] := BestDP2
        else{if (BestCP1 = Indices[2]) then}Indices[2] := BestDP2;
      with fPolygons[BestIdx] do
        if      (BestCP2 = Indices[0]) then Indices[0] := BestDP1
        else if (BestCP2 = Indices[1]) then Indices[1] := BestDP1
        else{if (BestCP2 = Indices[2]) then}Indices[2] := BestDP1;

      Inc(Cnt);
      BeautifyPoly(ActIdx);
      BeautifyPoly(BestIdx);
    end;
  end;

var
  ActIdx, Neat: Integer;
begin
  SetLength(chck,fPolyCount);
  FillChar(fPolygons[0], SizeOf(fPolygons[0]) * Length(fPolygons), #0);
  FillChar(chck[0], SizeOf(chck[0])*Length(chck),#0);
  cnt := 0;
  //for Neat := 0 to 3 do
  for ActIdx := 1 to fPolyCount - 1 do
    if not chck[ActIdx] then
        BeautifyPoly(ActIdx);
  if (Cnt > 0) then Cnt := Cnt*1;

end;


procedure TKMNavMeshGenerator.Paint(const aRect: TKMRect);
const
  COLOR_WHITE = $FFFFFF;
  COLOR_BLACK = $000000;
  COLOR_GREEN = $00FF00;
  COLOR_RED = $7700FF;
  COLOR_YELLOW = $00FFFF;
  COLOR_BLUE = $FF0000;
var
  K: Integer;
  p1,p2,p3: TKMPoint;
begin
  if not AI_GEN_NAVMESH OR not OVERLAY_NAVMESH then
    Exit;

  //GenerateNewNavMesh();

  { Nodes
  for K := 1 to fNodeCount - 1 do
      gRenderAux.Text(fNodes[K].X+0.25, fNodes[K].Y+0.4, IntToStr(K), $FF000000 OR COLOR_BLUE);
  for K := fInnerPointStartIdx to fInnerPointEndIdx do
      gRenderAux.Text(fNodes[K].X+0.25, fNodes[K].Y+0.4, IntToStr(K), $FF000000 OR COLOR_GREEN);//}
  //{ Border Lines
  for K := 0 to fBord.Count - 1 do
    with fBord.Borders[K] do
    begin
      p1 := fNodes[ Node ];
      p2 := fNodes[  fBord.Borders[ Next ].Node  ];
      p3 := fNodes[  fBord.Borders[ Prev ].Node  ];
      gRenderAux.LineOnTerrain(p1, p2, $50000000 OR COLOR_RED);
      gRenderAux.LineOnTerrain(p1, p3, $50000000 OR COLOR_RED);
    end;//}

  // TRIANGULATION
  { Debug lines of triangulation
  for I := 0 to fDL.Count - 1 do
  begin
      p1 := fDL.Lines[I].p1;
      p2 := fDL.Lines[I].p2;
      gRenderAux.LineOnTerrain(p1, p2, $40000000 OR COLOR_RED);
      gRenderAux.Text((p1.X+p2.X)/2, (p1.Y+p2.Y)/2, IntToStr(I), $FF000000 OR COLOR_RED);
  end;
  for I := 0 to fDL2.Count - 1 do
  begin
      p1 := fDL2.Lines[I].p1;
      p2 := fDL2.Lines[I].p2;
      gRenderAux.LineOnTerrain(p1, p2, $40000000 OR COLOR_BLACK);
      gRenderAux.Text((p1.X+p2.X)/2, (p1.Y+p2.Y)/2, IntToStr(I), $FF000000 OR COLOR_RED);
  end;//}
  { Debug lines of triangulation
  for I := 0 to fDL2.Count - 1 do
      gRenderAux.Text(p1.X, p1.Y + 1, IntToStr(I), $FFFFFFFF);//}
end;

end.
