unit KM_NavMeshInfluences;
{$I KaM_Remake.inc}
interface
uses
  Math, KM_Defaults, KM_CommonTypes,
  KM_Points, KM_NavMeshFloodFill;

type
  TKMEnemyStatistics = record
    Player: TKMHandIndex;
    Distance: Word;
    ClosestPoint: TKMPoint;
  end;
  TKMEnemyStatisticsArray = array of TKMEnemyStatistics;

  // 1 universal class for city and army influence search
  TNavMeshInfluenceSearch = class(TNavMeshFloodFill)
  private
    fEnemies: array of TKMHandIndex;
  protected
    fOwner: TKMHandIndex;
    fHouseInfluence: Boolean;
    fHighEnemiesIdx, fHighStatsIdx, fMaxEnemiesCnt: Integer;

    function CanBeExpanded(const aIdx: Word): Boolean; override;
    procedure MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint); override;
  public
    EnemiesStats: TKMEnemyStatisticsArray;
    function FindClosestEnemies(const aOwner: TKMHandIndex; aCenterPoints: TKMPointArray; aHouseInfluence: Boolean = True): Boolean;
  end;

  // 1 universal class for city and army influence flood fill
  TKMInfluenceFloodFill = class(TNavMeshFloodFill)
  private
    fCityFlood: Boolean;
    fDecreaseCoef: Word;
    fUnitStrength, fMaxDistance, fHouseInfluence: Word;
    fGroupType: TGroupType;
  protected
    fOwner: TKMHandIndex;
    function CanBeExpanded(const aIdx: Word): Boolean; override;
    procedure MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint); override;
  public
    function MilitaryPresence(aPlayer: TKMHandIndex; aUnitStrength, aMaxDistance, aMaximalIdx: Word; aGroupType: TGroupType; aInitIdxArray: TKMWordArray): Boolean;
    function HouseInfluence(aPlayer: TKMHandIndex; aHouseInfluence, aMaxDistance, aMaximalIdx: Word; aInitIdxArray: TKMWordArray): Boolean;
  end;

implementation
uses
  KM_AIFields, KM_NavMesh, KM_HandsCollection;


{TNavMeshInfluenceSearch}
function TNavMeshInfluenceSearch.CanBeExpanded(const aIdx: Word): Boolean;
begin
  // Can be visited only in case that we have space in array (and we are not out of enemies)
  Result := (fHighEnemiesIdx >= 0) AND (fHighStatsIdx < fMaxEnemiesCnt);
end;


procedure TNavMeshInfluenceSearch.MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint);
const
  HOUSE_INFLUENCE_LIMIT = 200;
  ARMY_INFLUENCE_LIMIT = 1;
var
  I: Integer;
begin
  inherited MarkAsVisited(aIdx, aDistance, aPoint);

  // Scan current polygon and try find enemies presence
  for I := 0 to fHighEnemiesIdx do
    if    (      fHouseInfluence AND (gAIFields.Influences.Ownership[  fEnemies[I], aPoint.Y, aPoint.X  ] > HOUSE_INFLUENCE_LIMIT)  )
       OR (  not fHouseInfluence AND (gAIFields.Influences.PresenceAllGroups[  fEnemies[I], aIdx  ] > ARMY_INFLUENCE_LIMIT) ) then
    begin
      with EnemiesStats[fHighStatsIdx] do
      begin
        Player := fEnemies[I];
        Distance := fQueueArray[aIdx].Distance;
        ClosestPoint := aPoint;
      end;
      fEnemies[I] := fEnemies[ fHighEnemiesIdx ];
      fHighEnemiesIdx := fHighEnemiesIdx - 1;
      fHighStatsIdx := fHighStatsIdx + 1;
      if not CanBeExpanded(aIdx) then
        break;
    end;
end;


function TNavMeshInfluenceSearch.FindClosestEnemies(const aOwner: TKMHandIndex; aCenterPoints: TKMPointArray; aHouseInfluence: Boolean = True): Boolean;
const
  MAX_ENEMIES_AT_ONCE = 3;
var
  PL: TKMHandIndex;
  I, Cnt: Integer;
  InitIdxArray: TKMWordArray;
begin
  Result := False;
  fOwner := aOwner;
  fHouseInfluence := aHouseInfluence;

  // Find center points of polygons
  Cnt := Length(aCenterPoints);
  SetLength(InitIdxArray, Cnt);
  for I := 0 to Cnt - 1 do
    InitIdxArray[I] := gAIFields.NavMesh.Point2Polygon[ aCenterPoints[I].Y, aCenterPoints[I].X ];

  // Find enemies (indexes)
  SetLength(fEnemies, gHands.Count - 1);
  Cnt := 0;
  for PL := 0 to gHands.Count - 1 do
    if gHands[PL].Enabled AND (gHands[aOwner].Alliances[PL] = at_Enemy) then
    begin
      fEnemies[Cnt] := PL;
      Cnt := Cnt + 1;
    end;
  if (Cnt = 0) then // No enemy left
    Exit;

  // Init variables
  fMaxEnemiesCnt := Min(Cnt, MAX_ENEMIES_AT_ONCE);
  fHighEnemiesIdx := fMaxEnemiesCnt - 1;
  SetLength(EnemiesStats, fMaxEnemiesCnt);
  fHighStatsIdx := 0;

  // Flood fill
  FillPolygons(High(InitIdxArray), InitIdxArray);
  Result := (fHighStatsIdx > 0);

  // Set Result length
  SetLength(EnemiesStats, fHighStatsIdx);
end;




{ TKMInfluenceFloodFill }
function TKMInfluenceFloodFill.CanBeExpanded(const aIdx: Word): Boolean;
begin
  Result := (fQueueArray[aIdx].Distance < fMaxDistance);
end;


procedure TKMInfluenceFloodFill.MarkAsVisited(const aIdx, aDistance: Word; const aPoint: TKMPoint);
const
  HOUSE_COEF = 2;
begin
  if fCityFlood then
    gAIFields.Influences.OwnPoly[fOwner,aIdx] := Max(0, fHouseInfluence - (aDistance shl HOUSE_COEF))
  else
    gAIFields.Influences.IncPresence[fOwner,aIdx,fGroupType] := Max(0, fUnitStrength - aDistance * fDecreaseCoef );
  inherited MarkAsVisited(aIdx, aDistance, aPoint);
end;


function TKMInfluenceFloodFill.MilitaryPresence(aPlayer: TKMHandIndex; aUnitStrength, aMaxDistance, aMaximalIdx: Word; aGroupType: TGroupType; aInitIdxArray: TKMWordArray): Boolean;
begin
  fCityFlood := False;
  fOwner := aPlayer;
  fUnitStrength := aUnitStrength;
  fMaxDistance := aMaxDistance;
  fGroupType := aGroupType;
  fDecreaseCoef := Max( 1, Round(aUnitStrength / (aMaxDistance * 1.0)) );
  Result := inherited FillPolygons(aMaximalIdx, aInitIdxArray);
end;


function TKMInfluenceFloodFill.HouseInfluence(aPlayer: TKMHandIndex; aHouseInfluence, aMaxDistance, aMaximalIdx: Word; aInitIdxArray: TKMWordArray): Boolean;
begin
  fCityFlood := True;
  fOwner := aPlayer;
  fHouseInfluence := aHouseInfluence;
  fMaxDistance := aMaxDistance;
  Result := inherited FillPolygons(aMaximalIdx, aInitIdxArray);
end;


end.
