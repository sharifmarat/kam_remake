{
NavMesh - army positioning
@author: Martin Toupal
@e-mail: poznamenany@gmail.com
}
unit KM_NavMeshArmyPositioning;
{$I KaM_Remake.inc}
interface
uses
  Math, KM_Defaults, KM_CommonTypes,
  KM_Points, KM_UnitGroup, KM_Houses, KM_ResHouses,
  KM_NavMeshGenerator, KM_NavMeshFloodFill;

type

  TDefenceInfo = record
    AllyInfluence, EnemyInfluence: Byte;
    Distance, Mark: Word;
  end;
  TDefInfoArray = array of TDefenceInfo;

  TKMAllianceInfo = record
    // GroupsNumber = real number of groups, GroupsCount = count of groups in arrays
    GroupsCount, HousesCount, GroupsNumber: Word;
    Groups: TKMUnitGroupArray;
    Houses: TKMHouseArray;
    GroupsPoly, HousesPoly: TKMWordArray;
  end;

  TKMBattleLine = record
    GroupsCount, HousesCount, PolygonsCount: Word;
    Groups, Houses, Polygons: TKMWordArray;
  end;
  TKMBattleLines = record
    Count: Word;
    Price: Single;
    Lines: array of TKMBattleLine;
  end;
  {$IFDEF DEBUG_NavMeshDefences}
  TKMDebugBattleLines = record
    Count, LineStartIdx: Word;
    DBLs: array of TKMBattleLines;
  end;
  {$ENDIF}


  TDistancePenalization = class;
  TArmyForwardFF = class;
  TArmyBackwardFF = class;

  TDistancePenalization = class(TNavMeshFloodFill)
  private
  protected
    fMaxDistance: Word;
    fDefInfo: TDefInfoArray;

    function CanBeExpanded(const aIdx: Word): Boolean; override;
    procedure MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint); override;
  public
    function PrefillDistances(var aAlliance: TKMAllianceInfo; var aDefInfo: TDefInfoArray; var aQueueArray: TPolygonsQueueArr): Boolean;
  end;


  TArmyForwardFF = class(TNavMeshFloodFill)
  private
  protected
    fOwner: TKMHandID;
    fPolyArr: TPolygonArray;
    fDistancePenalization: TDistancePenalization;
    fBackwardFF: TArmyBackwardFF;
    fCntAllyPoly, fCntEnemyPoly: Word;

    procedure MakeNewQueue(); override;
    function IsVisited(const aIdx: Word): Boolean; override;
    procedure MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint); override;
    procedure InitQueue(const aMaxIdx: Integer; aInitIdxArray: TKMWordArray); override;
    function ForwardFF(): Boolean;

    procedure AssignDefencePositions();

    function GetInitPolygonsGroups(aAllianceType: TKMAllianceType; var aAlliance: TKMAllianceInfo): Boolean;
    function GetInitPolygonsHouses(aAllianceType: TKMAllianceType; var aAlliance: TKMAllianceInfo): Boolean;
  public
    fDefInfo: TDefInfoArray;
    Enemy: TKMAllianceInfo;
    Ally: TKMAllianceInfo;
    BattleLines: TKMBattleLines;
    TargetPositions: TKMPointDirArray;
    TargetLines: TKMWordArray;

    constructor Create(aSorted: Boolean = False); override;
    destructor Destroy; override;

    function FindArmyPosition(var aOwners: TKMHandIDArray): Boolean;
    procedure Paint();
  end;


  TArmyBackwardFF = class(TNavMeshFloodFill)
  private
  protected
    fOwner: TKMHandID;
    fExpandedPolygonsCnt: Word;
    fBestEvaluation: Double;
    fPolyArr: TPolygonArray;
    fDefInfo: TDefInfoArray;
    fBattleLines: TKMBattleLines;
    fBestBattleLines: TKMBattleLines;
    {$IFDEF DEBUG_NavMeshDefences}
    fDebugLines: TKMDebugBattleLines;
    procedure SaveDebugLine();
    {$ENDIF}

    function CanBeExpanded(const aIdx: Word): Boolean; override;
    procedure MarkAsVisited(const aIdx: Word); reintroduce;
    //procedure InitQueue(const aMaxIdx: Integer; aInitIdxArray: TKMWordArray; aInitPolyGroups: TKMUnitGroupArray); reintroduce;
    procedure InitQueue(var aEnemy: TKMAllianceInfo); reintroduce;
    procedure BackwardFlood(var aEnemy: TKMAllianceInfo);
    procedure EvaluateLine(const aIdx: Word);
    procedure ExpandPolygon(aIdx: Word; aLineIdx1: Integer = -1; aPolyIdx1: Integer = -1);
    procedure ComputeWeightedDistance(const aIdx: Word);
  public
    constructor Create(aSorted: Boolean = False); override;

    function FindTeamDefences(var aEnemy: TKMAllianceInfo; var aOwners: TKMHandIDArray; var aDefInfo: TDefInfoArray; var aQueueArray: TPolygonsQueueArr): TKMBattleLines;

    procedure Paint();
  end;


const
  PREFILL_MAX_DISTANCE_GROUPS = 30;
  PREFILL_MAX_DISTANCE_HOUSES = 10;
  PREFILL_SHIFT = 3;

  POLYGON_CNT_PENALIZATION = 10;
  SCAN_HOUSES: THouseTypeSet = [htWatchTower, htBarracks, htStore, htSchool, htTownhall];


implementation
uses
  SysUtils,
  KM_Hand, KM_HandsCollection,
  KM_AIFields, KM_AIInfluences, KM_NavMesh,
  KM_Game, KM_RenderAux;




{ TDistancePenalization }
function TDistancePenalization.CanBeExpanded(const aIdx: Word): Boolean;
begin
  //Result := fDefInfo[aIdx].Distance > 0;
  Result := fDefInfo[aIdx].EnemyInfluence > 0;
end;


procedure TDistancePenalization.MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint);
begin
  inherited MarkAsVisited(aIdx, aDistance, aPoint);
  //fDefInfo[aIdx].Distance := Max(0,PREFILL_MAX_DISTANCE - aDistance) shl PREFILL_SHIFT;
  //fDefInfo[aIdx].Distance := Max(0,PREFILL_MAX_DISTANCE - aDistance) * 4;
  fDefInfo[aIdx].EnemyInfluence := Min(255,Max(0,fMaxDistance - aDistance));
end;


function TDistancePenalization.PrefillDistances(var aAlliance: TKMAllianceInfo; var aDefInfo: TDefInfoArray; var aQueueArray: TPolygonsQueueArr): Boolean;
begin
  Result := (aAlliance.GroupsCount > 0) OR (aAlliance.HousesCount > 0);
  if not Result then
    Exit;
  fDefInfo := aDefInfo;
  fQueueArray := aQueueArray; // This is clear array with zeros
  fVisitedIdx := 0; // It will be updated to 1 after FillPolygons are called
  fMaxDistance := PREFILL_MAX_DISTANCE_HOUSES;
  FillPolygons(aAlliance.HousesCount, aAlliance.HousesPoly);
  fMaxDistance := PREFILL_MAX_DISTANCE_GROUPS;
  FillPolygons(aAlliance.GroupsCount, aAlliance.GroupsPoly);
end;




{ TArmyForwardFF }
constructor TArmyForwardFF.Create(aSorted: Boolean = False);
begin
  inherited Create(aSorted);

  fDistancePenalization := TDistancePenalization.Create(aSorted);
  fBackwardFF := TArmyBackwardFF.Create(aSorted);
end;


destructor TArmyForwardFF.Destroy();
begin
  fDistancePenalization.Free;
  fBackwardFF.Free;

  inherited;
end;


// Prepare new Queue
procedure TArmyForwardFF.MakeNewQueue();
begin
  // Check length
  if (Length(fQueueArray) < gAIFields.NavMesh.PolygonsCnt) then
  begin
    SetLength(fQueueArray, gAIFields.NavMesh.PolygonsCnt);
    SetLength(fDefInfo, gAIFields.NavMesh.PolygonsCnt);
  end;
  // Clear queue
  fQueueCnt := 0;
  ClearVisitIdx();
  // Distance will be changed so clear array
  FillChar(fDefInfo[0], SizeOf(fDefInfo[0]) * Length(fDefInfo), #0);
end;


function TArmyForwardFF.IsVisited(const aIdx: Word): Boolean;
begin
  Result := (fQueueArray[aIdx].Visited >= fVisitedIdx); // Arrays are common so >= must be here
end;


procedure TArmyForwardFF.MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint);
begin
  //fDefInfo[aIdx].EnemyInfluence := Min(250,fDefInfo[aIdx].Distance);
  //Inc(fDefInfo[aIdx].Distance,aDistance);
  fDefInfo[aIdx].Distance := aDistance;
  inherited MarkAsVisited(aIdx, aDistance, aPoint);

  //fDefInfo[aIdx].AllyInfluence := gAIFields.Influences.GetAlliancePresence(fOwner, aIdx, atAlly);
end;


procedure TArmyForwardFF.InitQueue(const aMaxIdx: Integer; aInitIdxArray: TKMWordArray);
const
  INIT_DISTANCE = 0;
var
  I, Idx: Word;
begin
  fPolyArr := gAIFields.NavMesh.Polygons;
  fVisitedIdx := 4;
  if (aMaxIdx >= 0) then
    for I := 0 to aMaxIdx do
    begin
      Idx := aInitIdxArray[I];
      if not IsVisited(Idx) then
      begin
        MarkAsVisited(Idx, INIT_DISTANCE, fPolyArr[ Idx ].CenterPoint);
        InsertInQueue(Idx);
      end;
    end;
end;


function TArmyForwardFF.GetInitPolygonsGroups(aAllianceType: TKMAllianceType; var aAlliance: TKMAllianceInfo): Boolean;
var
  PL: TKMHandID;
  K, L: Integer;
  G: TKMUnitGroup;
begin
  with aAlliance do
  begin
    GroupsCount := 0;
    GroupsNumber := 0;
    for PL := 0 to gHands.Count - 1 do
      if (gHands[fOwner].Alliances[PL] = aAllianceType) then
      begin
        GroupsNumber := GroupsNumber + gHands[PL].UnitGroups.Count;
        for K := 0 to gHands[PL].UnitGroups.Count - 1 do
        begin
          G := gHands[PL].UnitGroups.Groups[K];
          if (G <> nil) AND not G.IsDead AND not KMSamePoint(KMPOINT_ZERO,G.Position) then
          begin
            L := 0;
            while (L < G.Count) do
            begin
              if (G.Members[L] <> nil) AND not G.Members[L].IsDeadOrDying then
              begin
                if (Length(GroupsPoly) <= GroupsCount) then
                begin
                  SetLength(GroupsPoly, GroupsCount + 20);
                  SetLength(Groups, Length(GroupsPoly));
                end;
                GroupsPoly[GroupsCount] := gAIFields.NavMesh.KMPoint2Polygon[ G.Members[L].CurrPosition ];
                Groups[GroupsCount] := G;
                Inc(GroupsCount);
                L := L + 5;
              end
              else
                L := L + 1;
            end;
          end;
        end;
      end;
    SetLength(GroupsPoly, GroupsCount);
    SetLength(Groups, GroupsCount);
    Result := GroupsCount > 0;
  end;
end;


function TArmyForwardFF.GetInitPolygonsHouses(aAllianceType: TKMAllianceType; var aAlliance: TKMAllianceInfo): Boolean;
var
  PL: TKMHandID;
  K: Integer;
  HT: TKMHouseType;
  H: TKMHouse;
begin
  with aAlliance do
  begin
    HousesCount := 0;
    for PL := 0 to gHands.Count - 1 do
      if (gHands[fOwner].Alliances[PL] = aAllianceType) then
        for K := 0 to gHands[PL].Houses.Count - 1 do
        begin
          H := gHands[PL].Houses[K];
          if (H <> nil) AND not H.IsDestroyed then
          begin
            HT := H.HouseType;
            if HT in SCAN_HOUSES then
            begin
              if (Length(Houses) <= HousesCount) then
              begin
                SetLength(HousesPoly, HousesCount + 10);
                SetLength(Houses, Length(HousesPoly));
              end;
              HousesPoly[HousesCount] := gAIFields.NavMesh.KMPoint2Polygon[ H.Entrance ];
              Houses[HousesCount] := H;
              Inc(HousesCount);
            end;
          end;
        end;
    SetLength(HousesPoly, HousesCount);
    SetLength(Houses, HousesCount);
    Result := HousesCount > 0;
  end;
end;



function TArmyForwardFF.ForwardFF(): Boolean;
begin
  Result := False;
  // Check allied units
  GetInitPolygonsGroups(atAlly, Ally);
  if (Ally.GroupsCount = 0) then
    Exit;
  // Check enemy units
  GetInitPolygonsGroups(atEnemy,Enemy);
  GetInitPolygonsHouses(atEnemy,Enemy);
  if (Enemy.GroupsCount = 0) AND (Enemy.HousesCount = 0) then
    Exit;
  // Mark enemy units and houses
  MakeNewQueue();
  fDistancePenalization.PrefillDistances(Enemy, fDefInfo, fQueueArray);
  // Flood fill
  Result := FillPolygons(Ally.GroupsCount-1, Ally.GroupsPoly);
end;


function TArmyForwardFF.FindArmyPosition(var aOwners: TKMHandIDArray): Boolean;
begin
  Result := False;
  fOwner := aOwners[0];
  if ForwardFF() then
  begin
    BattleLines := fBackwardFF.FindTeamDefences(Enemy, aOwners, fDefInfo, fQueueArray);
    Result := BattleLines.Count > 0;
    if Result then
      AssignDefencePositions();
  end;
end;


procedure TArmyForwardFF.AssignDefencePositions();

  function FindPosition(aIdx: Word): TKMPointDir;
  var
    K, Idx, NearbyIdx: Word;
    Dir: TKMDirection;
  begin
    Result := KMPointDir(KMPOINT_ZERO,dirN);
    fQueueCnt := 0;
    fVisitedIdx := fVisitedIdx + 1;
    InsertInQueue(aIdx);
    while RemoveFromQueue(Idx) do
    begin
      for K := 0 to fPolyArr[Idx].NearbyCount - 1 do
      begin
        NearbyIdx := fPolyArr[Idx].Nearby[K];
        //if (fDefInfo[Idx].Distance > fDefInfo[NearbyIdx].Distance) then
        //begin
          if (fQueueArray[NearbyIdx].Visited < fVisitedIdx) then
            InsertInQueue(NearbyIdx);
          if (fQueueArray[NearbyIdx].Visited < 10) then
          begin
            fQueueArray[NearbyIdx].Visited := fVisitedIdx;
            if (fQueueArray[NearbyIdx].Distance < fQueueArray[Idx].Distance) then
              Dir := KMGetDirection(fPolyArr[ Idx ].CenterPoint,fPolyArr[ NearbyIdx ].CenterPoint)
            else
              Dir := KMGetDirection(fPolyArr[ NearbyIdx ].CenterPoint,fPolyArr[ Idx ].CenterPoint);
            Exit( KMPointDir(fPolyArr[ Idx ].NearbyPoints[K], Dir) );
          end;
          fQueueArray[NearbyIdx].Visited := fVisitedIdx;
        //end;
      end;
    end;
  end;

var
  K, L, M, Idx: Integer;
  Price, BestPrice: Single;
  GroupPoint, LinePoint: TKMPoint;
  G: TKMUnitGroup;
  LineEval: array of array[TKMGroupType] of Word;
  TargetPoly: array of Word;
begin
  fVisitedIdx := 10;
  // Clear array
  SetLength(LineEval, BattleLines.Count);
  for K := Low(LineEval) to High(LineEval) do
    FillChar(LineEval[0], SizeOf(LineEval[0]), #0);
  // Evaluate battle lines
  for K := 0 to BattleLines.Count - 1 do
    for L := 0 to BattleLines.Lines[K].GroupsCount - 1 do
    begin
      G := Enemy.Groups[ BattleLines.Lines[K].Groups[L] ];
      Inc(LineEval[K,G.GroupType], G.Count);
    end;
  // Compute weight function
  SetLength(TargetPoly, Ally.GroupsCount);
  SetLength(TargetLines, Ally.GroupsCount);
  for K := 0 to Ally.GroupsCount - 1 do
  begin
    BestPrice := 1E10;
    GroupPoint := Ally.Groups[K].Position;
    for L := 0 to BattleLines.Count - 1 do
    begin
      for M := 0 to BattleLines.Lines[L].PolygonsCount - 1 do
      begin
        Idx := BattleLines.Lines[L].Polygons[M];
        LinePoint := fPolyArr[ Idx ].CenterPoint;
        Price := KMDistanceSqr(LinePoint, GroupPoint) - fDefInfo[Idx].Mark * 50;
        if (Price < BestPrice) then
        begin
          BestPrice := Price;
          TargetPoly[K] := Idx;
          TargetLines[K] := L;
        end;
      end;
    end;
  end;
  // Distribute groups
  SetLength(TargetPositions, Length(TargetPoly));   // GroupsNumber
  for K := Low(TargetPoly) to High(TargetPoly) do
    TargetPositions[K] := FindPosition(TargetPoly[K]);
end;


procedure TArmyForwardFF.Paint();
  procedure DrawPolygon(aIdx: Integer; const Opacity: Byte; aFillColor: Cardinal; aText: String = '');
  var
    P0,P1,P2: TKMPoint;
  begin
    if (Opacity = 0) then
      Exit;
    aFillColor := aFillColor OR (Opacity shl 24);
    with gAIFields.NavMesh do
    begin
      P0 := Nodes[ Polygons[aIdx].Indices[0] ];
      P1 := Nodes[ Polygons[aIdx].Indices[1] ];
      P2 := Nodes[ Polygons[aIdx].Indices[2] ];
      gRenderAux.TriangleOnTerrain(P0.X,P0.Y, P1.X,P1.Y, P2.X,P2.Y, aFillColor);
      if (Length(aText) > 0) then
        gRenderAux.Text(Polygons[aIdx].CenterPoint.X, Polygons[aIdx].CenterPoint.Y, aText, $FFFFFFFF);
    end;
  end;
const
  COLOR_WHITE = $FFFFFF;
  COLOR_BLACK = $000000;
  COLOR_GREEN = $00FF00;
  COLOR_RED = $7700FF;
  COLOR_YELLOW = $00FFFF;
  COLOR_BLUE = $FF0000;
var
  K: Integer;
  PL, Spec: TKMHandID;
  Owners: TKMHandIDArray;
begin
  Spec := gMySpectator.HandID;
  //Spec := 1;
  // Get players in alliance
  for PL := 0 to gHands.Count - 1 do
    if gHands[PL].Enabled AND (gHands[Spec].Alliances[PL] = atAlly) then
    begin
      SetLength(Owners, Length(Owners) + 1);
      Owners[ High(Owners) ] := PL;
      // Make first index spec. handID so it have different color
      if (Spec = Owners[ High(Owners) ]) then
      begin
        Owners[ High(Owners) ] := Owners[0];
        Owners[0] := Spec;
      end;
    end;

  fOwner := Owners[0];
  //if ForwardFF() then
  //begin
  {
    for K := Low(fDefInfo) to High(fDefInfo) do
      with fDefInfo[K] do
      begin
        if (Distance       > 0) then DrawPolygon(K, Min(240,Distance)      , COLOR_BLACK, IntToStr(fDefInfo[K].Distance));
        if (EnemyInfluence > 0) then DrawPolygon(K, Min(240,EnemyInfluence), COLOR_RED  , IntToStr(fDefInfo[K].Distance));
      end;
  //}
    //BattleLines := fBackwardFF.FindTeamDefences(fCntEnemyPoly, fEnemyPolyArr, fEnemyGroups, Owners, fDefInfo, fQueueArray);
    fBackwardFF.Paint();
    for K := 0 to fCntEnemyPoly - 1 do
      DrawPolygon(Enemy.GroupsPoly[K], 20, COLOR_RED);
  //end;
end;







{ TArmyBackwardFF }
constructor TArmyBackwardFF.Create(aSorted: Boolean = False);
begin
  inherited Create(aSorted);
end;


function TArmyBackwardFF.CanBeExpanded(const aIdx: Word): Boolean;
begin
  Result := True;

  EvaluateLine(aIdx);
end;


procedure TArmyBackwardFF.MarkAsVisited(const aIdx: Word);
begin
  fQueueArray[aIdx].Visited := fVisitedIdx;
end;


procedure TArmyBackwardFF.ComputeWeightedDistance(const aIdx: Word);
begin
  fQueueArray[aIdx].Distance := High(Word) - fDefInfo[aIdx].Distance - fDefInfo[aIdx].EnemyInfluence * 8;
end;


//procedure TArmyBackwardFF.InitQueue(const aMaxIdx: Integer; aInitIdxArray: TKMWordArray; aInitPolyGroups: TKMUnitGroupArray);
procedure TArmyBackwardFF.InitQueue(var aEnemy: TKMAllianceInfo);
  function FindPolyIdx(aPoly: Integer): Integer;
  var
    K: Integer;
  begin
    Result := fBattleLines.Count;
    for K := 0 to Result - 1 do
      if (aPoly = fBattleLines.Lines[K].Polygons[0]) then
        Exit(K);
  end;
  procedure CheckNewBattleLine(aPolygon: Word; aBattleLineIdx, aPolyIdx: Integer);
  begin
    with fBattleLines.Lines[aBattleLineIdx] do
      if (fBattleLines.Count = aBattleLineIdx) then
      begin
        GroupsCount := 0;
        HousesCount := 0;
        PolygonsCount := 1;
        SetLength(Groups,4); // Create new array
        SetLength(Houses,4); // Create new array
        SetLength(Polygons,8); // Create new array
        Polygons[0] := aPolygon;
      end;
  end;
  procedure AddWordField(aFied: Integer; var aCount: Word; var aArr: TKMWordArray); inline;
  begin
    if (Length(aArr) <= aCount) then
      SetLength(aArr,Length(aArr) + 4);
    aArr[ aCount ] := aFied;
    aCount := aCount + 1;
  end;
var
  K, Idx: Integer;
begin
  {$IFDEF DEBUG_NavMeshDefences}
    fDebugLines.Count := 0;
  {$ENDIF}
  fPolyArr := gAIFields.NavMesh.Polygons;
  fQueueCnt := 0;
  fExpandedPolygonsCnt := 0;
  fBestEvaluation := +1E10;
  // Mark init points and create lines
  fBattleLines.Count := 0;
  fBattleLines.Price := 0;
  SetLength(fBattleLines.Lines, aEnemy.GroupsCount + aEnemy.HousesCount);
  // Copy groups
  for K := 0 to aEnemy.GroupsCount - 1 do
  begin
    Idx := FindPolyIdx(aEnemy.GroupsPoly[K]);
    CheckNewBattleLine(aEnemy.GroupsPoly[K], Idx, K);
    with fBattleLines.Lines[Idx] do
      AddWordField(K, GroupsCount, Groups);
    Inc(fBattleLines.Count,Byte(Idx = fBattleLines.Count));
  end;
  // Copy houses
  for K := 0 to aEnemy.HousesCount - 1 do
  begin
    Idx := FindPolyIdx(aEnemy.HousesPoly[K]);
    CheckNewBattleLine(aEnemy.HousesPoly[K], Idx, K);
    with fBattleLines.Lines[Idx] do
      AddWordField(K, HousesCount, Houses);
    Inc(fBattleLines.Count,Byte(Idx = fBattleLines.Count));
  end;
  // Add to queue
  fVisitedIdx := 5;
  for K := 0 to fBattleLines.Count - 1 do
  begin
    Idx := fBattleLines.Lines[K].Polygons[0];
    if not IsVisited(Idx) then
    begin
      MarkAsVisited(Idx);
      ComputeWeightedDistance(Idx);
      InsertAndSort(Idx);
    end;
  end;
end;


procedure TArmyBackwardFF.ExpandPolygon(aIdx: Word; aLineIdx1: Integer = -1; aPolyIdx1: Integer = -1);
  function FindPolygonInLines(aIdx: Integer; var aLineIdx, aPolyIdx: Integer): Boolean;
  var
    K, L: Integer;
  begin
    Result := False;
    for K := 0 to fBattleLines.Count - 1 do
      for L := 0 to fBattleLines.Lines[K].PolygonsCount - 1 do
        if (aIdx = fBattleLines.Lines[K].Polygons[L]) then
        begin
          aLineIdx := K;
          aPolyIdx := L;
          Exit(True);
        end;
  end;
var
  K, NearbyIdx, LineIdx1, LineIdx2, PolyIdx1, PolyIdx2, Cnt: Integer;
begin
  fExpandedPolygonsCnt := fExpandedPolygonsCnt + 1;
  if (aLineIdx1 <> -1) then
  begin
    LineIdx1 := aLineIdx1;
    PolyIdx1 := aPolyIdx1;
  end
  else if FindPolygonInLines(aIdx, LineIdx1, PolyIdx1) then
  begin
    Dec(fBattleLines.Lines[LineIdx1].PolygonsCount);
    fBattleLines.Lines[LineIdx1].Polygons[PolyIdx1] := fBattleLines.Lines[LineIdx1].Polygons[ fBattleLines.Lines[LineIdx1].PolygonsCount ];
  end
  else
    Exit;

  for K := 0 to fPolyArr[aIdx].NearbyCount-1 do
  begin
    NearbyIdx := fPolyArr[aIdx].Nearby[K];
    // Expand polygon
    if not IsVisited(NearbyIdx) then
    begin
      MarkAsVisited(NearbyIdx);
      if (fPolyArr[NearbyIdx].NearbyCount = 3) then // New combat lines are created only from polygons with 3 surrounding polygons
      begin
        ComputeWeightedDistance(NearbyIdx);
        //if (fDefInfo[aIdx].Distance + 3 > fDefInfo[NearbyIdx].Distance) then
        //begin
          fDefInfo[NearbyIdx].Mark := fDefInfo[aIdx].Mark + 1;
          InsertAndSort(NearbyIdx);
          Inc(fBattleLines.Lines[LineIdx1].PolygonsCount);
          if (fBattleLines.Lines[LineIdx1].PolygonsCount > Length(fBattleLines.Lines[LineIdx1].Polygons)) then
            SetLength(fBattleLines.Lines[LineIdx1].Polygons, fBattleLines.Lines[LineIdx1].PolygonsCount + 16);
          fBattleLines.Lines[LineIdx1].Polygons[ fBattleLines.Lines[LineIdx1].PolygonsCount-1 ] := NearbyIdx;
        //end;
      end
      else
        ExpandPolygon(NearbyIdx, LineIdx1, PolyIdx1);
    end
    // Merge lines
    else if (fBattleLines.Count > 1) AND FindPolygonInLines(NearbyIdx, LineIdx2, PolyIdx2) AND (LineIdx1 <> LineIdx2) then
    begin
      Cnt := fBattleLines.Lines[LineIdx1].GroupsCount;
      if (fBattleLines.Lines[LineIdx2].GroupsCount > 0) then
      begin
        Inc(fBattleLines.Lines[LineIdx1].GroupsCount, fBattleLines.Lines[LineIdx2].GroupsCount);
        if (Length(fBattleLines.Lines[LineIdx1].Groups) < fBattleLines.Lines[LineIdx1].GroupsCount) then
          SetLength(fBattleLines.Lines[LineIdx1].Groups, fBattleLines.Lines[LineIdx1].GroupsCount + 8);
        Move(fBattleLines.Lines[LineIdx2].Groups[0], fBattleLines.Lines[LineIdx1].Groups[Cnt], SizeOf(fBattleLines.Lines[LineIdx1].Groups[0]) * fBattleLines.Lines[LineIdx2].GroupsCount);
      end;

      Cnt := fBattleLines.Lines[LineIdx1].PolygonsCount;
      if (fBattleLines.Lines[LineIdx2].PolygonsCount > 0) then
      begin
        Inc(fBattleLines.Lines[LineIdx1].PolygonsCount, fBattleLines.Lines[LineIdx2].PolygonsCount);
        if (Length(fBattleLines.Lines[LineIdx1].Polygons) < fBattleLines.Lines[LineIdx1].PolygonsCount) then
          SetLength(fBattleLines.Lines[LineIdx1].Polygons, fBattleLines.Lines[LineIdx1].PolygonsCount + 16);
        Move(fBattleLines.Lines[LineIdx2].Polygons[0], fBattleLines.Lines[LineIdx1].Polygons[Cnt], SizeOf(fBattleLines.Lines[LineIdx1].Polygons[0]) * fBattleLines.Lines[LineIdx2].PolygonsCount);
      end;

      Cnt := fBattleLines.Lines[LineIdx1].HousesCount;
      if (fBattleLines.Lines[LineIdx2].HousesCount > 0) then
      begin
        Inc(fBattleLines.Lines[LineIdx1].HousesCount, fBattleLines.Lines[LineIdx2].HousesCount);
        if (Length(fBattleLines.Lines[LineIdx1].Houses) < fBattleLines.Lines[LineIdx1].HousesCount) then
          SetLength(fBattleLines.Lines[LineIdx1].Houses, fBattleLines.Lines[LineIdx1].HousesCount + 8);
        Move(fBattleLines.Lines[LineIdx2].Houses[0], fBattleLines.Lines[LineIdx1].Houses[Cnt], SizeOf(fBattleLines.Lines[LineIdx1].Houses[0]) * fBattleLines.Lines[LineIdx2].HousesCount);
      end;

      Dec(fBattleLines.Count);
      fBattleLines.Lines[LineIdx2] := fBattleLines.Lines[ fBattleLines.Count ];
      if (LineIdx1 = fBattleLines.Count) then
        LineIdx1 := LineIdx2;
    end;
  end;
end;


procedure TArmyBackwardFF.BackwardFlood(var aEnemy: TKMAllianceInfo);
var
  InEnemyInfluence: Boolean;
  Idx, OldIdx, Overflow, Overflow2: Word;
begin
  //InitQueue(aInitPolyCnt - 1, aInitPolyArr, aInitPolyGroups);
  InitQueue(aEnemy);
  if (fQueueCnt <= 0) then
    Exit;

  // Expand enemy influence
  InEnemyInfluence := True;
  Overflow := 0;
  while InEnemyInfluence AND (Overflow < 5000) AND (fQueueCnt > 0) do
  begin
    InEnemyInfluence := False;
    Inc(Overflow);

    OldIdx := fStartQueue;
    Idx := fStartQueue;
    Overflow2 := 0;
    repeat
      Inc(Overflow2);
      if (fDefInfo[Idx].EnemyInfluence > 10) then
      begin
        //if CanBeExpanded(Idx) then // Def line must be behind safe radius
        fQueueArray[OldIdx].Next := fQueueArray[Idx].Next;
        if (Idx = fStartQueue) then fStartQueue := fQueueArray[Idx].Next;
        if (Idx = fEndQueue)   then fEndQueue := OldIdx;
        Dec(fQueueCnt);
        ExpandPolygon(Idx);
        {$IFDEF DEBUG_NavMeshDefences}
        SaveDebugLine();
        {$ENDIF}
        InEnemyInfluence := True;
        break;
      end;
      OldIdx := Idx;
      Idx := fQueueArray[Idx].Next;
    until (OldIdx <> fEndQueue) OR (Overflow2 < 1000) OR (fQueueCnt <= 0);
  end;
  {$IFDEF DEBUG_NavMeshDefences}
  fDebugLines.LineStartIdx := fDebugLines.Count;
  {$ENDIF}

  // Start searching for defense line
  Idx := fStartQueue;
  //while RemoveFromQueue(Idx) AND (fDefInfo[Idx].Distance > 5) do
  while RemoveFromQueue(Idx) AND (fQueueArray[Idx].Distance < High(Word) - 10) do
  begin
    //if (Idx = 167) then
    //  debug(Idx);
    if CanBeExpanded(Idx) then
      ExpandPolygon(Idx);
    {$IFDEF DEBUG_NavMeshDefences}
    SaveDebugLine();
    {$ENDIF}
  end;
end;


procedure TArmyBackwardFF.EvaluateLine(const aIdx: Word);
var
  K, L, Idx, MinDist, MaxDist: Integer;
  Evaluation: Single;
begin
  // Calculate evaluation of actual defensive position
  MaxDist := 0;
  MinDist := High(Word);
  for K := 0 to fBattleLines.Count - 1 do
    for L := 0 to fBattleLines.Lines[K].PolygonsCount - 1 do
    begin
      Idx := fBattleLines.Lines[K].Polygons[L];
      MaxDist := Max(MaxDist,fQueueArray[Idx].Distance);
      MinDist := Min(MinDist,fQueueArray[Idx].Distance);
      //Evaluation := + Evaluation + fQueueArray[Idx].Distance * 10;
    end;
  Evaluation := fQueueCnt * 0 + abs(High(Word) - MaxDist - 15) * 2 + abs(High(Word) - MinDist - 15) * 2;

  // If is evaluation better save polygons
  if (Evaluation < fBestEvaluation) then
  begin
    fBestEvaluation := Evaluation;
    fBestBattleLines.Count := fBattleLines.Count;
    fBestBattleLines.Price := Evaluation;
    if (Length(fBestBattleLines.Lines) < fBattleLines.Count) then
    SetLength(fBestBattleLines.Lines, fBattleLines.Count + 16);
    for K := 0 to fBattleLines.Count - 1 do
    begin
      fBestBattleLines.Lines[K].GroupsCount   := fBattleLines.Lines[K].GroupsCount;
      fBestBattleLines.Lines[K].HousesCount   := fBattleLines.Lines[K].HousesCount;
      fBestBattleLines.Lines[K].PolygonsCount := fBattleLines.Lines[K].PolygonsCount;
      SetLength(fBestBattleLines.Lines[K].Groups,   fBattleLines.Lines[K].GroupsCount);
      SetLength(fBestBattleLines.Lines[K].Houses,   fBattleLines.Lines[K].HousesCount);
      SetLength(fBestBattleLines.Lines[K].Polygons, fBattleLines.Lines[K].PolygonsCount);
      if (fBattleLines.Lines[K].PolygonsCount > 0) then
        Move(fBattleLines.Lines[K].Polygons[0], fBestBattleLines.Lines[K].Polygons[0], SizeOf(fBattleLines.Lines[K].Polygons[0]) * fBattleLines.Lines[K].PolygonsCount);
      if (fBattleLines.Lines[K].GroupsCount > 0) then
        Move(fBattleLines.Lines[K].Groups[0], fBestBattleLines.Lines[K].Groups[0], SizeOf(fBattleLines.Lines[K].Groups[0]) * fBattleLines.Lines[K].GroupsCount);
      if (fBattleLines.Lines[K].HousesCount > 0) then
        Move(fBattleLines.Lines[K].Houses[0], fBestBattleLines.Lines[K].Houses[0], SizeOf(fBattleLines.Lines[K].Houses[0]) * fBattleLines.Lines[K].HousesCount);
    end;
  end;
end;


{$IFDEF DEBUG_NavMeshDefences}
procedure TArmyBackwardFF.SaveDebugLine();
var
  K,L: Integer;
begin
  if (fBattleLines.Count <= 0) OR (Length(fBattleLines.Lines) <= 0) then
    Exit;
  if (fDebugLines.Count <= Length(fDebugLines.DBLs)) then
    SetLength(fDebugLines.DBLs, Length(fDebugLines.DBLs) + 16);
  L := fDebugLines.Count;
  Inc(fDebugLines.Count);

  fDebugLines.DBLs[L].Count := fBattleLines.Count;
  fDebugLines.DBLs[L].Price := 0;
  if (Length(fDebugLines.DBLs[L].Lines) <= fBattleLines.Count) then
    SetLength(fDebugLines.DBLs[L].Lines, fBattleLines.Count + 16);
  for K := 0 to fBattleLines.Count - 1 do
    if (fBattleLines.Lines[K].PolygonsCount > 0) then
    begin
      fDebugLines.DBLs[L].Lines[K].GroupsCount   := fBattleLines.Lines[K].GroupsCount;
      fDebugLines.DBLs[L].Lines[K].PolygonsCount := fBattleLines.Lines[K].PolygonsCount;
      SetLength(fDebugLines.DBLs[L].Lines[K].Groups,   fBattleLines.Lines[K].GroupsCount);
      SetLength(fDebugLines.DBLs[L].Lines[K].Polygons, fBattleLines.Lines[K].PolygonsCount);
      if (fBattleLines.Lines[K].GroupsCount > 0) then
        Move(fBattleLines.Lines[K].Groups[0],   fDebugLines.DBLs[L].Lines[K].Groups[0],   SizeOf(fBattleLines.Lines[K].Groups[0])   * fBattleLines.Lines[K].GroupsCount);
      if (fBattleLines.Lines[K].PolygonsCount > 0) then
        Move(fBattleLines.Lines[K].Polygons[0], fDebugLines.DBLs[L].Lines[K].Polygons[0], SizeOf(fBattleLines.Lines[K].Polygons[0]) * fBattleLines.Lines[K].PolygonsCount);
    end;
end;
{$ENDIF}


function TArmyBackwardFF.FindTeamDefences(var aEnemy: TKMAllianceInfo; var aOwners: TKMHandIDArray; var aDefInfo: TDefInfoArray; var aQueueArray: TPolygonsQueueArr): TKMBattleLines;
begin
  fOwner := aOwners[0];
  fDefInfo := aDefInfo;
  fQueueArray := aQueueArray;
  if (aEnemy.GroupsCount > 0) OR (aEnemy.HousesCount > 0) then
    BackwardFlood(aEnemy);
  Result := fBestBattleLines;
end;


procedure TArmyBackwardFF.Paint();
  procedure DrawPolygon(aIdx: Integer; const Opacity: Byte; aFillColor: Cardinal; aText: String = '');
  var
    P0,P1,P2: TKMPoint;
  begin
    if (Opacity = 0) then
      Exit;
    aFillColor := aFillColor OR (Opacity shl 24);
    with gAIFields.NavMesh do
    begin
      P0 := Nodes[ Polygons[aIdx].Indices[0] ];
      P1 := Nodes[ Polygons[aIdx].Indices[1] ];
      P2 := Nodes[ Polygons[aIdx].Indices[2] ];
      gRenderAux.TriangleOnTerrain(P0.X,P0.Y, P1.X,P1.Y, P2.X,P2.Y, aFillColor);
      if (Length(aText) > 0) then
        gRenderAux.Text(Polygons[aIdx].CenterPoint.X, Polygons[aIdx].CenterPoint.Y, aText, $FFFFFFFF);
    end;
  end;
const
  COLOR_WHITE = $FFFFFF;
  COLOR_BLACK = $000000;
  COLOR_GREEN = $00FF00;
  COLOR_RED = $7700FF;
  COLOR_YELLOW = $00FFFF;
  COLOR_BLUE = $FF0000;
var
  K,L,Idx: Integer;
{$IFDEF DEBUG_NavMeshDefences}
  //Opacity: Byte;
  Color, TickIdx: Cardinal;
  //MinPrc, MaxPrc: Single;
  //P1,P2: TKMPoint;
{$ENDIF}
begin
  {
  for K := 0 to gAIFields.NavMesh.PolygonsCnt - 1 do
    if IsVisited(K) then
    begin
      if (gAIFields.NavMesh.Polygons[K].NearbyCount = 3) then
        DrawPolygon(K, 50, COLOR_BLACK, IntToStr(High(Word) - fQueueArray[K].Distance))
      else
        DrawPolygon(K, 50, COLOR_BLACK, IntToStr(fQueueArray[K].Distance));
    end
    else
    begin
      DrawPolygon(K, 50, COLOR_WHITE, IntToStr(fQueueArray[K].Distance));
    end;
  //}
  //{
  for K := 0 to fBestBattleLines.Count - 1 do
    for L := 0 to fBestBattleLines.Lines[K].PolygonsCount - 1 do
    begin
      Idx := fBestBattleLines.Lines[K].Polygons[L];
      DrawPolygon(Idx, 30, COLOR_GREEN, IntToStr(fQueueArray[Idx].Distance));
    end;
    //}

  {$IFDEF DEBUG_NavMeshDefences}
  //{
  if fDebugLines.Count > 0 then
  begin
    TickIdx := Round(gGame.GameTick / 1.0) mod fDebugLines.Count;
    Color := COLOR_WHITE;
    if (fDebugLines.LineStartIdx >= TickIdx) then
      Color := COLOR_RED;
    with fDebugLines.DBLs[TickIdx] do
      for K := 0 to Count - 1 do
        for L := 0 to Lines[K].PolygonsCount - 1 do
        begin
          Idx := Lines[K].Polygons[L];
          DrawPolygon(Idx, 50, Color, IntToStr(fQueueArray[Idx].Distance));
        end;
  end;
    //}

  {
  Opacity := 250;
  with fBestLines do
    for L := 0 to Count - 1 do
    begin
      P1 := gAIFields.NavMesh.Nodes[ Lines[L].Nodes[0] ];
      P2 := gAIFields.NavMesh.Nodes[ Lines[L].Nodes[1] ];
      gRenderAux.LineOnTerrain(P1, P2, (Opacity shl 24) OR COLOR_RED);
      //DrawPolygon(DefArr.Lines[L].Polygon, Opacity, COLOR_BLUE);
    end;
  //}

  {
    MinPrc := +1E10;
    MaxPrc := -1E10;
    for K := 0 to fDebugDefLines.Count - 1 do
      with fDebugDefLines.DefLines[K] do
      begin
        if (MinPrc > Price) then MinPrc := Price;
        if (MaxPrc < Price) then MaxPrc := Price;
      end;
    for K := 0 to fDebugDefLines.Count - 1 do
      with fDebugDefLines.DefLines[K] do
        for L := 0 to DefArr.Count - 1 do
        begin
          Opacity := Max(50,Min(255,Round(255 - abs((Price - MinPrc) / MaxPrc) * 254)));
          P1 := gAIFields.NavMesh.Nodes[ DefArr.Lines[L].Nodes[0] ];
          P2 := gAIFields.NavMesh.Nodes[ DefArr.Lines[L].Nodes[1] ];
          gRenderAux.LineOnTerrain(P1, P2, (Opacity shl 24) OR COLOR_RED);
          //DrawPolygon(DefArr.Lines[L].Polygon, Opacity, COLOR_BLUE);
        end;
  //}
  {$ENDIF}
end;



end.
