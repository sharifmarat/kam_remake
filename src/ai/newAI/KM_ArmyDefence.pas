unit KM_ArmyDefence;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_CommonClasses, KM_CommonTypes, KM_Defaults,
  KM_Points, KM_UnitGroups, KM_NavMeshDefences, KM_ArmyAttack;


type
  //For now IDs must match with KaM

  TKMFormation = record NumUnits, UnitsPerRow: Integer; end;
  TKMDefenceStatus = (ds_Empty = 0, ds_Half = 1, ds_Full = 2, ds_None = 3);

  TKMDefencePosition = class
  private
    fWeight: Word; // Higher number = higher enemy influence or closer to the first line
    fGroup: TKMUnitGroup; //Commander of group currently occupying position
    fPosition: TKMPointDir; //Position and direction the group defending will stand

    procedure SetGroup(aGroup: TKMUnitGroup);
    procedure SetPosition(const Value: TKMPointDir);
    function GetGroupType(): TKMGroupType;
  public
    constructor Create(aWeight: Word; aPos: TKMPointDir);
    constructor Load(LoadStream: TKMemoryStream);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure SyncLoad();

    property Weight: Word read fWeight write fWeight;
    property Position: TKMPointDir read fPosition write SetPosition; //Position and direction the group defending will stand
    property Group: TKMUnitGroup read fGroup write SetGroup;
    property GroupType: TKMGroupType read GetGroupType;

    function CanAccept(aGroup: TKMUnitGroup; aMaxUnits: Integer): Boolean;
    procedure UpdateState(aTick: Cardinal);
  end;


  TKMArmyDefence = class
  private
    fOwner: TKMHandIndex;
    fCityUnderAttack: Boolean;
    fFirstLineCnt: Word;
    fPositions: TKMList;
    fDefPolyFirstLine: TKMWordArray;

    fAttack: TKMArmyAttack;

    function GetCount: Integer; inline;
    function GetPosition(aIndex: Integer): TKMDefencePosition; inline;
    procedure UpdateDefences();
  public
    TroopFormations: array [TKMGroupType] of TKMFormation; //Defines how defending troops will be formatted. 0 means leave unchanged.

    constructor Create(aPlayer: TKMHandIndex; aAttack: TKMArmyAttack; aHostileGroups: TList);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad();

    property Count: Integer read GetCount;
    property Positions[aIndex: Integer]: TKMDefencePosition read GetPosition; default;
    property CityUnderAttack: Boolean read fCityUnderAttack;

    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    function DefenceStatus(): TKMDefenceStatus;
    function FindPositionOf(aGroup: TKMUnitGroup): TKMDefencePosition;
    function FindPlaceForGroup(aGroup: TKMUnitGroup): Boolean;
    procedure ReleaseGroup(aGroup: TKMUnitGroup); overload;
    procedure ReleaseGroup(aDefPosIdx: Integer); overload;
    procedure FindEnemyInDefLine(aEnemyGroups: TKMUnitGroupArray);

    procedure UpdateState(aTick: Cardinal);
    procedure Paint();
  end;

const
  MAX_SOLDIERS_IN_GROUP = 9; //These are the defaults in KaM
  FORMATION_OF_GROUP = 3;
  SQR_FIRST_LINE_RADIUS = 8*8;


implementation
uses
  Math,
  KM_Game, KM_HandsCollection, KM_Hand, KM_RenderAux,
  KM_AIFields, KM_NavMesh, KM_CommonUtils;


{ TKMDefencePosition }
constructor TKMDefencePosition.Create(aWeight: Word; aPos: TKMPointDir);
begin
  inherited Create;
  fWeight := aWeight;
  fPosition := aPos;
  Group := nil; //Unoccupied
end;


destructor TKMDefencePosition.Destroy;
begin
  Group := nil; //Ensure pointer is removed (property calls CleanUpGroupPointer)
  inherited;
end;


procedure TKMDefencePosition.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('DefencePosition');
  SaveStream.Write(fWeight);
  SaveStream.Write(fPosition);
  if (fGroup <> nil) then
    SaveStream.Write(fGroup.UID) //Store ID
  else
    SaveStream.Write(Integer(0));
end;


constructor TKMDefencePosition.Load(LoadStream: TKMemoryStream);
begin
  inherited Create;
  LoadStream.ReadAssert('DefencePosition');
  LoadStream.Read(fWeight);
  LoadStream.Read(fPosition);
  LoadStream.Read(fGroup, 4); //Subst on syncload
end;


procedure TKMDefencePosition.SyncLoad();
begin
  fGroup := gHands.GetGroupByUID(Cardinal(fGroup));
end;


procedure TKMDefencePosition.SetGroup(aGroup: TKMUnitGroup);
begin
  gHands.CleanUpGroupPointer(fGroup);
  if (aGroup <> nil) then
    fGroup := aGroup.GetGroupPointer;
end;


procedure TKMDefencePosition.SetPosition(const Value: TKMPointDir);
begin
  Assert(gGame.IsMapEditor);
  fPosition := Value;
end;


function TKMDefencePosition.GetGroupType(): TKMGroupType;
begin
  Result := gt_Melee;
  if (Group <> nil) then
    Result := Group.GroupType;
end;


function TKMDefencePosition.CanAccept(aGroup: TKMUnitGroup; aMaxUnits: Integer): Boolean;
begin
  Result := (Group = nil)
            OR ((GroupType = UnitGroups[aGroup.UnitType]) AND (Group.Count < aMaxUnits));
end;


procedure TKMDefencePosition.UpdateState(aTick: Cardinal);
begin
  //If the group is Dead or too far away we should disassociate
  //them from the defence position so new warriors can take up the defence if needs be
  if (Group = nil)
    OR Group.IsDead
    OR ( Group.InFight OR (Group.Order in [goAttackHouse, goAttackUnit]) ) then
    gHands.CleanUpGroupPointer(fGroup);

  //Tell group to walk to its position
  //It's easier to repeat the order than check that all members are in place
  if (Group <> nil)
    AND Group.IsIdleToAI([wtokFlagPoint, wtokHaltOrder])
    AND Group.CanWalkTo(Position.Loc, 0) then
    Group.OrderWalk(Position.Loc, True, wtokAIGotoDefencePos, Position.Dir);
end;





{ TKMArmyDefence }
constructor TKMArmyDefence.Create(aPlayer: TKMHandIndex; aAttack: TKMArmyAttack; aHostileGroups: TList);
var
  GT: TKMGroupType;
begin
  inherited Create;

  fOwner := aPlayer;
  fCityUnderAttack := False;
  fPositions := TKMList.Create;
  fAttack := aAttack;

  for GT := Low(TKMGroupType) to High(TKMGroupType) do
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
  SaveStream.Write(fCityUnderAttack);
  SaveStream.Write(fFirstLineCnt);
  SaveStream.Write(TroopFormations, SizeOf(TroopFormations));

  I := Length(fDefPolyFirstLine);
  SaveStream.Write(I);
  if (I > 0) then
    SaveStream.Write(fDefPolyFirstLine[0], SizeOf(fDefPolyFirstLine[0]) * I);

  SaveStream.Write(Count);
  for I := 0 to Count - 1 do
    Positions[I].Save(SaveStream);
end;


procedure TKMArmyDefence.Load(LoadStream: TKMemoryStream);
var I, NewCount: Integer;
begin
  LoadStream.ReadAssert('DefencePositions');
  LoadStream.Read(fOwner);
  LoadStream.Read(fCityUnderAttack);
  LoadStream.Read(fFirstLineCnt);
  LoadStream.Read(TroopFormations, SizeOf(TroopFormations));

  LoadStream.Read(NewCount);
  SetLength(fDefPolyFirstLine, NewCount);
  if (NewCount > 0) then
    LoadStream.Read(fDefPolyFirstLine[0], SizeOf(fDefPolyFirstLine[0]) * NewCount);

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


function TKMArmyDefence.FindPlaceForGroup(aGroup: TKMUnitGroup): Boolean;
var
  I, BestIdx, Needed: Integer;
  BestWeight: Word;
  UG: TKMUnitGroup;
begin
  Result := False;

  //Try to link to existing group
  BestWeight := 0;
  BestIdx := -1;
  for I := 0 to Count - 1 do
    if Positions[I].CanAccept(aGroup, TroopFormations[aGroup.GroupType].NumUnits)
      AND (BestWeight < Positions[I].Weight) then
    begin
      BestIdx := I;
      BestWeight := Positions[I].Weight;
    end;

  if (BestIdx <> -1) then
  begin
    Result := True;
    //New position
    if (Positions[BestIdx].Group = nil) then
    begin
      Needed := TroopFormations[ aGroup.GroupType ].NumUnits;
      // Add group to new defense position
      Positions[BestIdx].Group := aGroup;
      // Defence position requires less soldiers -> distribute other soldiers into different defence position
      if (aGroup.Count > Needed) then
      begin
        UG := aGroup.OrderSplitUnit( aGroup.GetAliveMember, True);
        if (UG <> nil) then
        begin
          if (aGroup.Count - Needed > 0) then
            aGroup.OrderSplitLinkTo(UG, aGroup.Count - Needed, True); //Link only as many units as are needed
          //FindPlaceForGroup(UG); // The rest of group will be added later (not in this tick)
        end;
      end;
      UG := Positions[BestIdx].Group;
      UG.OrderWalk( Positions[BestIdx].Position.Loc, True, wtokAIGotoDefencePos, Positions[BestIdx].Position.Dir );
    end
    // Restock
    else
    begin
      //Append to existing position
      UG := Positions[BestIdx].Group;
      Needed := TroopFormations[ UG.GroupType ].NumUnits - UG.Count;
      if (aGroup.Count <= Needed) then
        aGroup.OrderLinkTo(UG, True) //Link entire group
      else
      begin
        aGroup.OrderSplitLinkTo(UG, Needed, True); //Link only as many units as are needed
        //FindPlaceForGroup(aGroup); // The rest of group will be added later (not in this tick)
      end;
    end;
    if (UG.UnitsPerRow <> TroopFormations[UG.GroupType].UnitsPerRow) then
      UG.UnitsPerRow := TroopFormations[UG.GroupType].UnitsPerRow;
  end;
end;


//Find DefencePosition of a group
function TKMArmyDefence.FindPositionOf(aGroup: TKMUnitGroup): TKMDefencePosition;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Count - 1 do
  if (Positions[I].Group = aGroup) then
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
    DP.Group := nil;
end;


procedure TKMArmyDefence.ReleaseGroup(aDefPosIdx: Integer);
begin
  if (aDefPosIdx < Count) then
    Positions[aDefPosIdx].Group := nil;
end;


function TKMArmyDefence.DefenceStatus(): TKMDefenceStatus;
const
  FIRST_LINE_COEF = 2; // We should have at least 2 lines of defences
  FORCE_ATTACK_LIMIT = 4; // We must have [FORCE_ATTACK_LIMIT]x more soldiers in defences to lauch force attack
var
  I, Cnt: Integer;
begin
  Result := ds_None;
  if (Count > 0) then
  begin
    Cnt := 0;
    for I := 0 to Count - 1 do
      if (Positions[I].Group <> nil) then
        Cnt := Cnt + 1;
    case + Byte(Cnt >= Min(fFirstLineCnt * FIRST_LINE_COEF, Count * 0.5))
         + Byte(Cnt >= Count * 0.8)
         + Byte(Cnt >= fFirstLineCnt * FORCE_ATTACK_LIMIT) of // In case that defence is too long keep max cnt decreased
      0: Result := ds_Empty;
      1: Result := ds_Half;
      2: Result := ds_Full;
    end;
  end;
end;


procedure TKMArmyDefence.UpdateDefences();
var
  DefCnt: Word;
  I, K: Integer;
  G: TKMUnitGroup;
  VisitedNewPos, VisitedExistPos: TBooleanArray;
  DefPosArr: TKMDefencePosArr;
  BestDefLines: TKMDefenceLines;
begin
  //Get defence Polygons
  DefCnt := gHands[fOwner].UnitGroups.Count + 10;
  if not gAIFields.NavMesh.Defences.FindDefensivePolygons(fOwner, DefCnt, DefPosArr, False) then
    Exit;
  // Actualize first line
  fFirstLineCnt := gAIFields.NavMesh.Defences.FirstLine;
  BestDefLines := gAIFields.NavMesh.Defences.BestDefLines;
  SetLength(fDefPolyFirstLine, BestDefLines.Count);
  for I := 0 to BestDefLines.Count - 1 do
    fDefPolyFirstLine[I] := BestDefLines.Lines[I].Polygon;

  SetLength(VisitedNewPos, Length(DefPosArr));
  for I := 0 to Length(VisitedNewPos) - 1 do
    VisitedNewPos[I] := False;
  SetLength(VisitedExistPos, fPositions.Count);
  for I := 0 to Length(VisitedExistPos) - 1 do
    VisitedExistPos[I] := False;

  // Compare new defences with old version and add new defences / remove old
  for I := 0 to Length(DefPosArr) - 1 do
    // Try find existing defence position
    for K := 0 to fPositions.Count - 1 do
      //if (Positions[K].Polygon = DefPosArr[I].Polygon) then // This cannot be used because 1 polygon can have 3 point
      if KMSamePoint(Positions[K].Position.Loc, DefPosArr[I].DirPoint.Loc) then
      begin
        VisitedNewPos[I] := True;
        VisitedExistPos[K] := True;
        Positions[K].Weight := DefPosArr[I].Weight;
        break;
      end;

  // Remove old and unused defence positions
  for I := fPositions.Count - 1 downto 0 do
    if not VisitedExistPos[I] then
      fPositions.Delete(I);

  // Add new defence positions
  for I := 0 to Length(DefPosArr) - 1 do
    if not VisitedNewPos[I] then
      fPositions.Add(  TKMDefencePosition.Create( DefPosArr[I].Weight, DefPosArr[I].DirPoint )  );

  for I := fPositions.Count - 1 downto 0 do
  begin
    G := Positions[I].Group;
    if (G <> nil) AND (G.Count < TroopFormations[ G.GroupType ].NumUnits) then
    begin
      ReleaseGroup(I);
      FindPlaceForGroup(G);
    end;
  end;
end;


// Scan defence positions in first line and try find hostile groups in specific radius
procedure TKMArmyDefence.FindEnemyInDefLine(aEnemyGroups: TKMUnitGroupArray);

  function IsCompanyAround(aLoc: TKMPoint): Boolean;
  const
    SQR_MAX_DISTANCE = 10*10;
  var
    I: Integer;
    Company: TAICompany;
  begin
    Result := False;
    for I := 0 to fAttack.Count - 1 do
    begin
      Company := fAttack.Company[I];
      if ((Company.CompanyMode = cm_Attack) AND (KMDistanceSqr(aLoc, Company.ScanPosition) < SQR_MAX_DISTANCE))
        OR ((Company.CompanyMode = cm_Defence) AND (KMDistanceSqr(aLoc, Company.TargetPoint) < SQR_MAX_DISTANCE)) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  function FindDefenceGroups(aLoc: TKMPoint): TKMUnitGroupArray;
  const
    INIT_BID = 10000000;
    SQR_MAX_DISTANCE = 15*15;
    MAX_GROUPS_PER_COMPANY = 8;
  var
    I, K, Idx, Cnt: Integer;
    Bid: Single;
    Group: TKMUnitGroup;
    UGA: TKMUnitGroupArray;
    IdxArr: array[0..MAX_GROUPS_PER_COMPANY-1] of Integer;
    BidArr: array[0..MAX_GROUPS_PER_COMPANY-1] of Single;
  begin
    for I := 0 to Length(BidArr) - 1 do
      BidArr[I] := INIT_BID;

    Cnt := 0;
    for I := 0 to fPositions.Count - 1 do
    begin
      Group := Positions[I].Group;
      if (Group <> nil) AND not Group.IsDead then
      begin
        Cnt := Cnt + 1;
        Idx := I;
        Bid := KMDistanceSqr(aLoc, Positions[I].Position.Loc) - Group.Count * 10;
        for K := 0 to Length(BidArr) - 1 do
          if (Bid < BidArr[K]) then
          begin
            KMSwapFloat(Bid, BidArr[K]);
            KMSwapInt(Idx, IdxArr[K]);
          end
          else if (Bid = INIT_BID) then
            break;
      end;
    end;

    Cnt := Min( MAX_GROUPS_PER_COMPANY, Cnt );
    SetLength(UGA, Cnt);
    for I := 0 to Cnt - 1 do
    begin
      UGA[I] := Positions[ IdxArr[I] ].Group;
      ReleaseGroup( IdxArr[I] );
    end;
    Result := UGA;
  end;

var
  I, Idx, Threat: Integer;
  Loc: TKMPoint;
  Group: TKMUnitGroup;
  GT: TKMGroupType;
  UGA: TKMUnitGroupArray;
begin
  fCityUnderAttack := False;
  // Check defensive line
  for I := 0 to Length(fDefPolyFirstLine) - 1 do
  begin
    Threat := 0;
    Idx := fDefPolyFirstLine[I];
    for GT := Low(TKMGroupType) to High(TKMGroupType) do
      Threat := Threat + gAIFields.Influences.EnemyGroupPresence[ fOwner, Idx, GT ];
    if (Threat > 0) then
    begin
      fCityUnderAttack := True;
      Loc := gAIFields.NavMesh.Polygons[Idx].CenterPoint;
      if not IsCompanyAround(Loc) then
      begin
        UGA := gHands.GetGroupsInRadius(Loc, SQR_FIRST_LINE_RADIUS, fOwner, at_Enemy);
        if (Length(UGA) > 0) then
        begin
          UGA := FindDefenceGroups(Loc);
          if (Length(UGA) > 0) then
            fAttack.CreateCompany(Loc, UGA, cm_Defence);
        end;
      end;
    end;
  end;
  // Check every group
  for I := 0 to Length(aEnemyGroups) - 1 do
  begin
    Group := aEnemyGroups[I];
    if (Group <> nil) AND not Group.IsDead then
    begin
      Loc := Group.Position;
      fCityUnderAttack := True;
      UGA := FindDefenceGroups(Loc);
      if (Length(UGA) > 0) then
        fAttack.CreateCompany(Loc, UGA, cm_Defence);
    end;
  end;
end;



procedure TKMArmyDefence.UpdateState(aTick: Cardinal);
const
  PERF_TIME_LIMIT = MAX_HANDS * 10 * 10 + MAX_HANDS; // Every 10 sec * MAX_HANDS + MAX_HANDS find new defence line
var
  I,K: Integer;
begin
  if (aTick mod PERF_TIME_LIMIT = fOwner) then
    UpdateDefences();

  for I := 0 to Count - 1 do
    Positions[I].UpdateState(aTick);

  // Move troops closer to the edge of defence line
  for I := 0 to Count - 1 do
    if (Positions[I].Group = nil) then
      for K := I + 1 to Count - 1 do
        if Positions[I].GroupType = Positions[K].GroupType then
        begin
          Positions[I].Group := Positions[K].Group; //Take new position
          Positions[K].Group := nil; //Leave current position
          Break;
        end;
end;


procedure TKMArmyDefence.Paint();
const
  COLOR_WHITE = $FFFFFF;
  COLOR_BLACK = $000000;
  COLOR_GREEN = $00FF00;
  COLOR_RED = $0000FF;
  COLOR_YELLOW = $00FFFF;
  COLOR_BLUE = $FF0000;
var
  I, K, Idx, Threat: Integer;
  Col: Cardinal;
  Loc, Pos: TKMPoint;
  GT: TKMGroupType;
  PolyArr: TPolygonArray;
  NodeArr: TNodeArray;
  UGA: TKMUnitGroupArray;
begin
  if not OVERLAY_DEFENCES then
    Exit;

  if (fOwner <> gMySpectator.HandIndex) then // Show just 1 player (it prevents notification to be mess)
    Exit;

  // Draw defensive positions as a circles
  for I := 0 to Count - 1 do
  begin
    Loc := Positions[I].Position.Loc;
    Col := $22;
    if (Positions[I].Group = nil) then
      Col := 0;
    gRenderAux.CircleOnTerrain(Loc.X, Loc.Y, 1, (Col shl 24) OR COLOR_GREEN, $FFFFFFFF);
  end;

  // First line of defences
  PolyArr := gAIFields.NavMesh.Polygons;
  NodeArr := gAIFields.NavMesh.Nodes;
  for I := 0 to Length(fDefPolyFirstLine) - 1 do
  begin
    Threat := 0;
    Idx := fDefPolyFirstLine[I];
    for GT := Low(TKMGroupType) to High(TKMGroupType) do
      Threat := Threat + gAIFields.Influences.EnemyGroupPresence[ fOwner, Idx, GT ];

    // Draw defensive lines as a triangles
    Col := Max( $22, Min($FF, Threat) );
    gRenderAux.TriangleOnTerrain(
      NodeArr[PolyArr[Idx].Indices[0]].Loc.X,
      NodeArr[PolyArr[Idx].Indices[0]].Loc.Y,
      NodeArr[PolyArr[Idx].Indices[1]].Loc.X,
      NodeArr[PolyArr[Idx].Indices[1]].Loc.Y,
      NodeArr[PolyArr[Idx].Indices[2]].Loc.X,
      NodeArr[PolyArr[Idx].Indices[2]].Loc.Y, (Col shl 24) OR COLOR_RED);

    // Draw hostile units around defensive lines
    if (Threat > 0) then
    begin
      Loc := gAIFields.NavMesh.Polygons[Idx].CenterPoint;
      UGA := gHands.GetGroupsInRadius(Loc, SQR_FIRST_LINE_RADIUS, fOwner, at_Enemy);
      for K := 0 to Length(UGA) - 1 do
      begin
        Pos := UGA[K].Position;
        gRenderAux.CircleOnTerrain(Pos.X, Pos.Y, 1, $44000000 OR COLOR_RED, $FF000000 OR COLOR_RED);
        //gRenderAux.LineOnTerrain(Pos, Loc, $AA000000 OR COLOR_BLACK);
      end;
    end;
  end;
end;


end.
