unit KM_ArmyPresence;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_Units, KM_UnitGroups, KM_AISetup,
  KM_CommonTypes, KM_CommonClasses, KM_Points;

type
  TNodePointer = ^TNodeIdx;
  TNodeIdx = record
    Idx, Price, Distance: Word;
    Point: TKMPoint;
    Next: TNodePointer; // Parent navMeshNode
  end;

  TNavMeshFloodFill = class
  private
    fStartQueue: TNodePointer;
  protected
    fVisitIdx, fOwner: Byte;
    fGroupType: TGroupType;
    fVisited: TKMByteArray;

    procedure MakeNewQueue();
    procedure InsertInQueue(aIdx, aPrice, aDistance: Word; aPoint: TKMPoint);
    function RemoveFromQueue(var aIdx, aPrice, aDistance: Word; var aPoint: TKMPoint): Boolean;
    function IsQueueEmpty: Boolean;
    procedure SearchAround(aFromIdx, aPrice, aDistance: Word; aPoint: TKMPoint);
    function IsVisited(aIdx: Word): Boolean;
    procedure MarkAsVisited(aIdx, aPrice, aDistance: Word; aPoint: TKMPoint);

  public
    constructor Create();
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure Init(aNodesCnt: Word);

    procedure FloodFill(aOwner: Byte; aGroupType: TGroupType; aPrice: Word; aIdxArr: TKMWordArray);
  end;
  //TFightType = (ft_Melee, ft_Ranged);
  //TGroupType = (gt_Melee, gt_AntiHorse, gt_Ranged, gt_Mounted);
  //TGroupPresence = array[TGroupType] of Word;

  // Transform game data into "AI view" ... this is how AI see the map
  TKMArmyPresence = class
  private
    fPolygons: Word;
    fPresence: TKMWordArray;
    fFloodFill: TNavMeshFloodFill;

    procedure UpdateMilitaryPresence(aPL: Byte);

    function GetPresence(aPL: Byte; aIdx: Word; aGT: TGroupType): Word; inline;
    procedure SetPresence(aPL: Byte; aIdx: Word; aGT: TGroupType; aPresence: Word); inline;
    procedure SetIncPresence(aPL: Byte; aIdx: Word; aGT: TGroupType; aPresence: Word); inline;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property Presence[aPL: Byte; aIdx: Word; aGT: TGroupType]: Word read GetPresence write SetPresence;
    property IncPresence[aPL: Byte; aIdx: Word; aGT: TGroupType]: Word write SetIncPresence;

    procedure AfterMissionInit();
    procedure UpdateState(aTick: Cardinal);


    procedure Paint(aRect: TKMRect);
  end;



implementation
uses
  KM_Terrain, KM_Hand, KM_Resource, KM_AIFields, KM_HandsCollection, KM_RenderAux,
  KM_NavMesh;


{ TKMArmyPresence }
constructor TKMArmyPresence.Create();
begin
  fFloodFill := TNavMeshFloodFill.Create();
end;

destructor TKMArmyPresence.Destroy();
begin
  fFloodFill.Free;
  inherited;
end;


procedure TKMArmyPresence.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('ArmyPresence');
  SaveStream.Write(fPolygons);
  fFloodFill.Save(SaveStream);
end;

procedure TKMArmyPresence.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('ArmyPresence');
  LoadStream.Read(fPolygons);
  fFloodFill.Load(LoadStream);
end;


function TKMArmyPresence.GetPresence(aPL: Byte; aIdx: Word; aGT: TGroupType): Word;
begin
  Result := fPresence[((aPL*fPolygons + aIdx) shl 2) + Byte(aGT)];
end;

procedure TKMArmyPresence.SetPresence(aPL: Byte; aIdx: Word; aGT: TGroupType; aPresence: Word);
begin
  fPresence[((aPL*fPolygons + aIdx) shl 2) + Byte(aGT)] := aPresence;
end;

procedure TKMArmyPresence.SetIncPresence(aPL: Byte; aIdx: Word; aGT: TGroupType; aPresence: Word);
var
  Idx: Word;
begin
  Idx := ((aPL*fPolygons + aIdx) shl 2) + Byte(aGT);
  fPresence[Idx] := fPresence[Idx] + aPresence;
end;


procedure TKMArmyPresence.AfterMissionInit();
begin
  if AI_GEN_NAVMESH then
  begin
    fPolygons := Length(gAIFields.NavMesh.Polygons);
    SetLength(fPresence, (gHands.Count * fPolygons) * 4); // = Types of groups
    fFloodFill.Init(fPolygons);
  end;
end;


procedure TKMArmyPresence.UpdateState(aTick: Cardinal);
begin
  if AI_GEN_NAVMESH then
  begin
    UpdateMilitaryPresence(0);
  end;
end;


procedure TKMArmyPresence.UpdateMilitaryPresence(aPL: Byte);
  // Find closest NavMesh in case that we get walkable point which is not part of Point2NodeArr
  // (stonemason may mine stone and create walkable tiles which are not part of NavMesh [NavMesh is not updated durring game])
  function FindClosestNavMesh(aP: TKMPoint): Word;
  const
    MAX_SCAN_DIST = 3;
  var
    I,X,Y, Output: Word;
    PMin,PMax: TKMPoint;
  begin
    Output := High(Word);
    for I := 1 to MAX_SCAN_DIST do
    begin
      PMin := KMPoint(Max(1,aP.X - I), Max(1,aP.Y - I));
      PMax := KMPoint(Min(gTerrain.MapX,aP.X + I), Min(gTerrain.MapY,aP.Y + I));
      for X := PMin.X to PMax.X do
        if (gAIFields.NavMesh.Point2NodeArr[PMin.Y,X] <> High(Word)) then
        begin
          Output := gAIFields.NavMesh.Point2NodeArr[PMin.Y,X];
          break;
        end
        else if (gAIFields.NavMesh.Point2NodeArr[PMax.Y,X] <> High(Word)) then
        begin
          Output := gAIFields.NavMesh.Point2NodeArr[PMax.Y,X];
          break;
        end;
      if (Output <> High(Word)) then
        break;
      for Y := PMin.Y to PMax.Y do
        if (gAIFields.NavMesh.Point2NodeArr[Y,PMin.X] <> High(Word)) then
        begin
          Output := gAIFields.NavMesh.Point2NodeArr[Y,PMin.X];
          break;
        end
        else if (gAIFields.NavMesh.Point2NodeArr[Y,PMax.X] <> High(Word)) then
        begin
          Output := gAIFields.NavMesh.Point2NodeArr[Y,PMax.X];
          break;
        end;
      if (Output <> High(Word)) then
        break;
    end;
    Result := Output;
  end;
var
  I, K: Integer;
  P: TKMPoint;
  GT: TGroupType;
  G: TKMUnitGroup;
  PointArr: TKMWordArray;
begin
  SetLength(PointArr,1); // CHANGE CHANGE CHANGE CHANGE CHANGE CHANGE
  for I := 0 to fPolygons-1 do
    for GT in TGroupType do
      Presence[aPL,I,GT] := 0;

  for I := 0 to gHands[aPL].UnitGroups.Count-1 do
  begin
    G := gHands[aPL].UnitGroups.Groups[I];
    P := G.Position;
    PointArr[0] := gAIFields.NavMesh.Point2NodeArr[P.Y,P.X];
    for K := Low(PointArr) to High(PointArr) do
      if (PointArr[K] = High(Word)) then
      begin
        PointArr[K] := FindClosestNavMesh(P);
        if (PointArr[K] = High(Word)) then
        begin
          PointArr[K] := PointArr[High(PointArr)];
          SetLength(PointArr, Length(PointArr) - 1);
        end;
      end;
    if (Length(PointArr) > 0) AND not (G.IsDead OR KMSamePoint(P, KMPOINT_ZERO)) then
      fFloodFill.FloodFill(aPL, G.GroupType, 80, PointArr);
  end;

  //gAIFields.Eye.ArmyPresence.Presence[0,1,GT] := 5;

  //gAIFields.Eye.ArmyPresence.IncPresence[0,1,GT,5] := 3;
  //Members[aIndex: Integer]: TKMUnitWarrior
  //Members[I].IsDeadOrDying
end;


procedure TKMArmyPresence.Paint(aRect: TKMRect);
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
  PL: Byte;
  I, Cnt: Word;
  GT: TGroupType;
  T1: TKMPointF;
  PolyArr: TPolygonArray;
  NodeArr: TNodeArray;
begin
  if AI_GEN_NAVMESH AND OVERLAY_AI_COMBAT then
  begin
    PolyArr := gAIFields.NavMesh.Polygons;
    NodeArr := gAIFields.NavMesh.Nodes;

    PL := 0;
    for I := 0 to Length(PolyArr) - 1 do
    begin
      Cnt := 0;
      for GT in TGroupType do
        Cnt := Cnt + Presence[PL,I,GT];
      if (Cnt > 0) then
      begin
        //NavMesh polys coverage
        gRenderAux.TriangleOnTerrain(
          NodeArr[PolyArr[I].Indices[0]].Loc.X,
          NodeArr[PolyArr[I].Indices[0]].Loc.Y,
          NodeArr[PolyArr[I].Indices[1]].Loc.X,
          NodeArr[PolyArr[I].Indices[1]].Loc.Y,
          NodeArr[PolyArr[I].Indices[2]].Loc.X,
          NodeArr[PolyArr[I].Indices[2]].Loc.Y, $50FF0000);


        //NavMesh polys ids
        with PolyArr[I] do
        begin
          T1.X := (NodeArr[Indices[0]].Loc.X + NodeArr[Indices[1]].Loc.X + NodeArr[Indices[2]].Loc.X) / 3;
          T1.Y := (NodeArr[Indices[0]].Loc.Y + NodeArr[Indices[1]].Loc.Y + NodeArr[Indices[2]].Loc.Y) / 3;
          gRenderAux.Text(Round(T1.X), Round(T1.Y) + 1, IntToStr(I), $FF000000);
        end;
      end;
    end;
  end;
end;




{ TNavMeshFloodFill }
constructor TNavMeshFloodFill.Create();
begin
  fVisitIdx := High(Byte); // fVisited array will be automatically cleared in first calling of FloodFill
  //SetLength(fVisited, aNodesCnt); // This must be declared AfterMissionInit!
end;


destructor TNavMeshFloodFill.Destroy();
begin
  inherited;
end;


procedure TNavMeshFloodFill.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('NavMeshFloodFill');
  SaveStream.Write(fVisitIdx);
end;


procedure TNavMeshFloodFill.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('NavMeshFloodFill');
  LoadStream.Read(fVisitIdx);
end;


procedure TNavMeshFloodFill.Init(aNodesCnt: Word);
begin
  SetLength(fVisited, aNodesCnt);
end;


procedure TNavMeshFloodFill.MakeNewQueue();
begin
  fStartQueue := nil;
end;

// Insert in queue (sorted version)
procedure TNavMeshFloodFill.InsertInQueue(aIdx, aPrice, aDistance: Word; aPoint: TKMPoint);
var
  ActualP, PreviousP, NewP: TNodePointer;
begin
  // Find element which have smaller price
  PreviousP := nil;
  ActualP := fStartQueue;
  while (ActualP <> nil) AND (ActualP^.Price >= aPrice) do
  begin
    PreviousP := ActualP;
    ActualP := ActualP^.Next;
  end;
  // Create new element
  new(NewP);
  NewP^.Idx := aIdx;
  NewP^.Price := aPrice;
  NewP^.Distance := aDistance;
  NewP^.Point := aPoint;
  NewP^.Next := ActualP;
  // Fix Start of Queue (or next element of previous pointer)
  if (PreviousP = nil) then
    fStartQueue := NewP
  else
    PreviousP^.Next := NewP;
end;


function TNavMeshFloodFill.RemoveFromQueue(var aIdx, aPrice, aDistance: Word; var aPoint: TKMPoint): Boolean;
var pom: TNodePointer;
begin
  Result := True;
  if IsQueueEmpty then
    Result := False
  else
  begin
    aIdx := fStartQueue^.Idx;
    aPrice := fStartQueue^.Price;
    aDistance := fStartQueue^.Distance;
    aPoint := fStartQueue^.Point;
    pom := fStartQueue;
    fStartQueue := fStartQueue^.Next;
    Dispose(pom);
  end;
end;


function TNavMeshFloodFill.IsQueueEmpty: Boolean;
begin
  Result := (fStartQueue = nil);
end;


procedure TNavMeshFloodFill.SearchAround(aFromIdx, aPrice, aDistance: Word; aPoint: TKMPoint);
  function KMPointAverage(aP1,aP2: TKMPoint): TKMPoint;
  begin
    Result.X := (aP1.X + aP2.X) shr 1;
    Result.Y := (aP1.Y + aP2.Y) shr 1;
  end;
const
  DISTANCE_PENALIZATION = 5;
  MAX_DISTANCE = 30;
var
  I,K,L, ToIdx, Price, Distance, P1, P2: Word;
  Point: TKMPoint;
  PolyArr: TPolygonArray;
begin
  MarkAsVisited(aFromIdx, aPrice, aDistance, aPoint);
  PolyArr := gAIFields.NavMesh.Polygons;
  for I := 0 to PolyArr[aFromIdx].NearbyCount-1 do
  begin
    ToIdx := PolyArr[aFromIdx].Nearby[I];
    if not IsVisited(ToIdx) then
    begin
      P1 := High(Word);
      for K := 0 to 2 do
        for L := 0 to 2 do
          if (PolyArr[ToIdx].Indices[K] = PolyArr[aFromIdx].Indices[L]) then
          begin
            if (P1 = High(Word)) then
              P1 := PolyArr[ToIdx].Indices[K]
            else
              P2 := PolyArr[ToIdx].Indices[K];
            break;
          end;
      Point := KMPointAverage(gAIFields.NavMesh.Nodes[P1].Loc,gAIFields.NavMesh.Nodes[P2].Loc);
      Distance := aDistance + KMDistanceAbs(Point, aPoint);
      if (Distance < MAX_DISTANCE) then
      begin
        Price := Max(0, aPrice-DISTANCE_PENALIZATION);
        InsertInQueue(ToIdx, Price, Distance, Point);
      end;
    end;
  end;
end;


function TNavMeshFloodFill.IsVisited(aIdx: Word): Boolean;
begin
  Result := (fVisited[aIdx] = fVisitIdx);
end;


procedure TNavMeshFloodFill.MarkAsVisited(aIdx, aPrice, aDistance: Word; aPoint: TKMPoint);
begin
  fVisited[aIdx] := fVisitIdx;
  gAIFields.Eye.ArmyPresence.IncPresence[fOwner,aIdx,fGroupType] := aPrice;
end;


procedure TNavMeshFloodFill.FloodFill(aOwner: Byte; aGroupType: TGroupType; aPrice: Word; aIdxArr: TKMWordArray);
var
  I, Idx, Price, Distance: Word;
  Point: TKMPoint;
begin
  // Initialization
  fOwner := aOwner;
  fGroupType := aGroupType;
  MakeNewQueue();
  if (fVisitIdx >= 254) then
  begin
    for I := Low(fVisited) to High(fVisited) do
      fVisited[I] := 0;
    //FillChar(fVisited, SizeOf(fVisited), #0);
    fVisitIdx := 0;
  end;
  fVisitIdx := fVisitIdx + 1;
  // Copy all init points to queue
  for I := Low(aIdxArr) to High(aIdxArr) do
    InsertInQueue(aIdxArr[I], aPrice, 0, gAIFields.NavMesh.Polygons[ aIdxArr[I] ].CenterPoint);

  // Start FloodFill
  while not IsQueueEmpty do
  begin
    RemoveFromQueue(Idx, Price, Distance, Point);
    if not IsVisited(Idx) then
      SearchAround(Idx, Price, Distance, Point);
  end;
end;

end.
