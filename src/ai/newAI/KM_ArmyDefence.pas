unit KM_ArmyDefence;
{$I KaM_Remake.inc}
interface
uses
  KM_UnitGroups,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  KM_NavMeshDefences;


type
  //For now IDs must match with KaM

  TKMFormation = record NumUnits, UnitsPerRow: Integer; end;
  TKMDefenceStatus = (ds_Empty = 0, ds_Half = 1, ds_Full = 2);

  TKMDefencePosition = class
  private
    fCurrentGroup: TKMUnitGroup; //Commander of group currently occupying position
    fPosition: TKMPointDir; //Position and direction the group defending will stand
    fGroupType: TGroupType; //Type of group to defend this position (e.g. melee)

    procedure SetCurrentGroup(aGroup: TKMUnitGroup);
    procedure SetPosition(const Value: TKMPointDir);
    procedure SetGroupType(const Value: TGroupType);
  public
    constructor Create(aPos: TKMPointDir; aGroupType: TGroupType);
    constructor Load(LoadStream: TKMemoryStream);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure SyncLoad();

    property CurrentGroup: TKMUnitGroup read fCurrentGroup write SetCurrentGroup;
    property Position: TKMPointDir read fPosition write SetPosition; //Position and direction the group defending will stand
    property GroupType: TGroupType read fGroupType write SetGroupType; //Type of group to defend this position (e.g. melee)

    function CanAccept(aGroup: TKMUnitGroup; aMaxUnits: Integer): Boolean;
    procedure UpdateState(aTick: Cardinal);
  end;


  TKMArmyDefence = class
  private
    fOwner: TKMHandIndex;
    fFirstLineCnt: Word;
    fPositions: TKMList;

    function GetCount: Integer; inline;
    function GetPosition(aIndex: Integer): TKMDefencePosition; inline;
    procedure RestockPositionWith(aDefenceGroup, aGroup: TKMUnitGroup);
  public
    TroopFormations: array [TGroupType] of TKMFormation; //Defines how defending troops will be formatted. 0 means leave unchanged.

    constructor Create(aPlayer: TKMHandIndex);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad();

    property Count: Integer read GetCount;
    property Positions[aIndex: Integer]: TKMDefencePosition read GetPosition; default;

    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    procedure UpdateDefences();
    function DefenceStatus(): TKMDefenceStatus;
    function FindPositionOf(aGroup: TKMUnitGroup): TKMDefencePosition;
    function FindPlaceForGroup(aGroup: TKMUnitGroup; aTakeClosest: Boolean): Boolean;
    procedure ReleaseGroup(aGroup: TKMUnitGroup);

    procedure UpdateState(aTick: Cardinal);
    procedure Paint();
  end;

const
  MAX_SOLDIERS_IN_GROUP = 9; //These are the defaults in KaM
  FORMATION_OF_GROUP = 3;


implementation
uses
  Math,
  KM_Game, KM_HandsCollection, KM_RenderAux,
  KM_AIFields, KM_CommonUtils;


{ TKMDefencePosition }
constructor TKMDefencePosition.Create(aPos: TKMPointDir; aGroupType: TGroupType);
begin
  inherited Create;
  fPosition := aPos;
  fGroupType := aGroupType;
  CurrentGroup := nil; //Unoccupied
end;


destructor TKMDefencePosition.Destroy;
begin
  CurrentGroup := nil; //Ensure pointer is removed (property calls CleanUpGroupPointer)
  inherited;
end;


procedure TKMDefencePosition.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('DefencePosition');
  SaveStream.Write(fPosition);
  SaveStream.Write(fGroupType, SizeOf(fGroupType));
  if (fCurrentGroup <> nil) then
    SaveStream.Write(fCurrentGroup.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
end;


constructor TKMDefencePosition.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;
  LoadStream.ReadAssert('DefencePosition');
  LoadStream.Read(fPosition);
  LoadStream.Read(fGroupType, SizeOf(fGroupType));
  LoadStream.Read(fCurrentGroup, 4); //Subst on syncload
end;


procedure TKMDefencePosition.SyncLoad();
begin
  fCurrentGroup := gHands.GetGroupByUID(Cardinal(fCurrentGroup));
end;


procedure TKMDefencePosition.SetCurrentGroup(aGroup: TKMUnitGroup);
begin
  gHands.CleanUpGroupPointer(fCurrentGroup);
  if (aGroup <> nil) then
    fCurrentGroup := aGroup.GetGroupPointer;
end;


procedure TKMDefencePosition.SetPosition(const Value: TKMPointDir);
begin
  Assert(gGame.IsMapEditor);
  fPosition := Value;
end;


procedure TKMDefencePosition.SetGroupType(const Value: TGroupType);
begin
  Assert(gGame.IsMapEditor);
  fGroupType := Value;
end;


function TKMDefencePosition.CanAccept(aGroup: TKMUnitGroup; aMaxUnits: Integer): Boolean;
begin
  Result := (fGroupType = UnitGroups[aGroup.UnitType]) // The right type of group
            AND (
              (CurrentGroup = nil) OR ( (aGroup.Count = 1) AND (CurrentGroup.Count < aMaxUnits) ) // Empty defence or small group
            );
end;


procedure TKMDefencePosition.UpdateState(aTick: Cardinal);
begin
  //If the group is Dead or too far away we should disassociate
  //them from the defence position so new warriors can take up the defence if needs be
  if (CurrentGroup = nil)
    OR CurrentGroup.IsDead
    OR ( CurrentGroup.InFight OR (CurrentGroup.Order in [goAttackHouse, goAttackUnit]) ) then
    gHands.CleanUpGroupPointer(fCurrentGroup);

  //Tell group to walk to its position
  //It's easier to repeat the order than check that all members are in place
  if (CurrentGroup <> nil)
    AND CurrentGroup.IsIdleToAI
    AND CurrentGroup.CanWalkTo(Position.Loc, 0) then
    CurrentGroup.OrderWalk(Position.Loc, True, Position.Dir);
end;





{ TKMArmyDefence }
constructor TKMArmyDefence.Create(aPlayer: TKMHandIndex);
var
  GT: TGroupType;
begin
  inherited Create;

  fOwner := aPlayer;
  fPositions := TKMList.Create;

  for GT := Low(TGroupType) to High(TGroupType) do
  begin
    TroopFormations[GT].NumUnits := MAX_SOLDIERS_IN_GROUP;
    TroopFormations[GT].UnitsPerRow := FORMATION_OF_GROUP;
  end;
end;


destructor TKMArmyDefence.Destroy;
begin
  fPositions.Free;

  inherited;
end;


procedure TKMArmyDefence.Save(SaveStream: TKMemoryStream);
var I: Integer;
begin
  SaveStream.WriteA('DefencePositions');
  SaveStream.Write(fOwner);
  SaveStream.Write(fFirstLineCnt);
  SaveStream.Write(TroopFormations, SizeOf(TroopFormations));
  SaveStream.Write(Count);

  for I := 0 to Count - 1 do
    Positions[I].Save(SaveStream);
end;


procedure TKMArmyDefence.Load(LoadStream: TKMemoryStream);
var I, NewCount: Integer;
begin
  LoadStream.ReadAssert('DefencePositions');
  LoadStream.Read(fOwner);
  LoadStream.Read(fFirstLineCnt);
  LoadStream.Read(TroopFormations, SizeOf(TroopFormations));
  LoadStream.Read(NewCount);

  for I := 0 to NewCount - 1 do
    fPositions.Add( TKMDefencePosition.Load(LoadStream) );
end;


procedure TKMArmyDefence.SyncLoad;
var I: Integer;
begin
  for I := 0 to Count - 1 do
    Positions[I].SyncLoad;
end;


procedure TKMArmyDefence.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
end;


function TKMArmyDefence.GetCount: Integer;
begin
  Result := fPositions.Count;
end;


function TKMArmyDefence.GetPosition(aIndex: Integer): TKMDefencePosition;
begin
  Result := fPositions[aIndex];
end;


function TKMArmyDefence.FindPlaceForGroup(aGroup: TKMUnitGroup; aTakeClosest: Boolean): Boolean;
var
  I, Matched: Integer;
  Distance, Best: Single;
begin
  Result := False;
  Matched := -1;
  Best := MaxSingle;

  //Try to link to existing group
  for I := 0 to Count - 1 do
  if Positions[I].CanAccept(aGroup, TroopFormations[aGroup.GroupType].NumUnits) then
  begin
    //Take closest position that is empty or requries restocking
    Distance := KMLengthSqr(aGroup.Position, Positions[I].Position.Loc);
    if (Distance < Best) then
    begin
      Matched := I;
      Best := Distance;
      if not aTakeClosest then
        Break;
    end;
  end;

  if (Matched <> -1) then
  begin
    Result := True;
    if (Positions[Matched].CurrentGroup = nil) then
    begin
      //New position
      Positions[Matched].CurrentGroup := aGroup;
      if (aGroup.UnitsPerRow < TroopFormations[aGroup.GroupType].UnitsPerRow) then
        aGroup.UnitsPerRow := TroopFormations[aGroup.GroupType].UnitsPerRow;
      aGroup.OrderWalk(Positions[Matched].Position.Loc, True);
    end
    else
      //Append to existing position
      RestockPositionWith(Positions[Matched].CurrentGroup, aGroup);
  end;
end;


procedure TKMArmyDefence.RestockPositionWith(aDefenceGroup, aGroup: TKMUnitGroup);
var
  Needed: integer;
begin
  Needed := TroopFormations[aDefenceGroup.GroupType].NumUnits - aDefenceGroup.Count;
  if (Needed <= 0) then
    Exit;
  if (aGroup.Count <= Needed) then
    aGroup.OrderLinkTo(aDefenceGroup, True) //Link entire group
  else
    aGroup.OrderSplitLinkTo(aDefenceGroup, Needed, True); //Link only as many units as are needed

  if (aDefenceGroup.UnitsPerRow < TroopFormations[aDefenceGroup.GroupType].UnitsPerRow) then
    aDefenceGroup.UnitsPerRow := TroopFormations[aDefenceGroup.GroupType].UnitsPerRow;
end;


//Find DefencePosition to which this Commander belongs
//(Result could be nil if CommanderCount > PositionsCount
function TKMArmyDefence.FindPositionOf(aGroup: TKMUnitGroup): TKMDefencePosition;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
  if (Positions[I].CurrentGroup = aGroup) then
  begin
    Result := Positions[I];
    Break;
  end;
end;


procedure TKMArmyDefence.ReleaseGroup(aGroup: TKMUnitGroup);
var
  DP: TKMDefencePosition;
begin
  DP := FindPositionOf(aGroup);
  if (DP <> nil) then
    DP.CurrentGroup := nil;
end;


function TKMArmyDefence.DefenceStatus(): TKMDefenceStatus;
const
  FIRST_LINE_COEF = 2; // We should have at least 2 lines of defence
var
  I, Cnt: Integer;
begin
  Cnt := 0;
  for I := 0 to Count - 1 do
    if (Positions[I].CurrentGroup <> nil) then
      Cnt := Cnt + 1;
  case Byte(Cnt >= Min(fFirstLineCnt * FIRST_LINE_COEF, Count shr 1)) + Byte(Cnt >= Count * 0.8) of // In case that defence is too long keep max cnt decreased
    0: Result := ds_Empty;
    1: Result := ds_Half;
    2: Result := ds_Full;
  end;
end;


procedure TKMArmyDefence.UpdateDefences();
const
  GROUPS: array[0..3] of TGroupType = (gt_Melee, gt_AntiHorse, gt_Ranged, gt_Mounted);
var
  FirstLineCnt: Word;
  I, K: Integer;
  Point: TKMPoint;
  FaceDir: TKMDirection;
  VisitedNewPos, VisitedExistPos: TBooleanArray;
  DefPolygons: TKMWordArray;
  DefPosArr: TKMDefencePosArr;
begin
  //Get defence Polygons
  FirstLineCnt := 0;
  //if not gAIFields.NavMesh.Defences.FindDefensivePolygons(fOwner, gHands[fOwner].UnitGroups.Count + 10, DefPolygons) then
  if not gAIFields.NavMesh.Defences.FindDefensivePolygons(fOwner, FirstLineCnt, DefPosArr) then
    Exit;

  fFirstLineCnt := FirstLineCnt;
  SetLength(VisitedNewPos, Length(DefPolygons));
  for I := 0 to Length(VisitedNewPos) - 1 do
    VisitedNewPos[I] := False;
  SetLength(VisitedExistPos, fPositions.Count);
  for I := 0 to Length(VisitedExistPos) - 1 do
    VisitedExistPos[I] := False;

  // Compare new defences with old version and add new defences / remove old
  for I := 0 to Length(DefPolygons) - 1 do
  begin
    Point := gAIFields.NavMesh.Polygons[ DefPolygons[I] ].CenterPoint;
    // Try find existing defence position
    K := 0;
    while (K < fPositions.Count) do
    begin
      if KMSamePoint(Positions[K].Position.Loc, Point) then
      begin
        VisitedNewPos[I] := True;
        VisitedExistPos[K] := True;
        break;
      end;
      K := K + 1;
    end;
  end;

  // Remove old defence positions
  for I := fPositions.Count - 1 downto 0 do
    if not VisitedExistPos[I] then
      fPositions.Delete(I);

  // Add new defence positions
  for I := 0 to Length(DefPolygons) - 1 do
    if not VisitedNewPos[I] then
    begin
      Point := gAIFields.NavMesh.Polygons[ DefPolygons[I] ].CenterPoint;
      FaceDir := dir_NA; //KMGetDirection(KMPointF(Point), KMPerpendecular(Point, Point));
      fPositions.Add( TKMDefencePosition.Create(KMPointDir(Point, FaceDir), GROUPS[KaMRandom(4)]) );
    end;
end;



procedure TKMArmyDefence.UpdateState(aTick: Cardinal);
var
  I,K: Integer;
begin
  for I := 0 to Count - 1 do
    Positions[I].UpdateState(aTick);

  for I := 0 to Count - 1 do
    if (Positions[I].CurrentGroup = nil) then
      for K := I + 1 to Count - 1 do
        if Positions[I].GroupType = Positions[K].GroupType then
        begin
          Positions[I].CurrentGroup := Positions[K].CurrentGroup; //Take new position
          Positions[K].CurrentGroup := nil; //Leave current position
          Break;
        end;
end;


procedure TKMArmyDefence.Paint();
var
  I: Integer;
begin
  if not OVERLAY_DEFENCES then
    Exit;
  for I := 0 to Count - 1 do
    gRenderAux.CircleOnTerrain(Positions[I].fPosition.Loc.X, Positions[I].fPosition.Loc.Y, 2, $0900FFFF, $FFFFFFFF);
end;


end.
