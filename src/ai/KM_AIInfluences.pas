unit KM_AIInfluences;
{$I KaM_Remake.inc}
interface
uses
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  KM_Units, KM_UnitGroups,
  KM_NavMesh, KM_NavMeshInfluences;


const

  AVOID_BUILDING_UNLOCK = 0;
  AVOID_BUILDING_HOUSE_OUTSIDE_LOCK = 30;
  AVOID_BUILDING_HOUSE_INSIDE_LOCK = 40;
  AVOID_BUILDING_NODE_LOCK_ROAD = 45;
  AVOID_BUILDING_NODE_LOCK_FIELD = 50;
  AVOID_BUILDING_FOREST_ADD = 150;
  AVOID_BUILDING_COAL_TILE = 249;


type

  //Collection of influence maps
  TKMInfluences = class
  private
    fMapX, fMapY, fPolygons: Word; // Limits of arrays

    fUpdateCityIdx, fUpdateArmyIdx: TKMHandIndex; // Update index
    fPresence: TKMWordArray; // Military presence
    fOwnership: TKMByteArray; // City mark the space around itself

    fFloodFill: TKMInfluenceFloodFill;
    fInfluenceSearch: TNavMeshInfluenceSearch;
    fNavMesh: TKMNavMesh;

    // Avoid building
    procedure InitAvoidBuilding();
    // Army presence
    function GetAllPresences(const aPL: TKMHandIndex; aIdx: Word): Word; inline;
    function GetPresence(const aPL: TKMHandIndex; const aIdx: Word; const aGT: TGroupType): Word; inline;
    procedure SetPresence(const aPL: TKMHandIndex; const aIdx: Word; const aGT: TGroupType; const aPresence: Word); inline;
    procedure SetIncPresence(const aPL: TKMHandIndex; aIdx: Word; const aGT: TGroupType; const aPresence: Word); inline;
    procedure UpdateMilitaryPresence(const aPL: TKMHandIndex);
    // City influence
    function GetOwnership(const aPL: TKMHandIndex; const aIdx: Word): Byte; inline;
    procedure SetOwnership(const aPL: TKMHandIndex; const aIdx: Word; const aOwnership: Byte); inline;
    function GetOwnershipFromPoint(const aPL: TKMHandIndex; aY, aX: Word): Byte; inline; // For property -> aY, aX are switched!
    procedure SetOwnershipFromPoint(const aPL: TKMHandIndex; aY, aX: Word; const aOwnership: Byte); inline; // For property -> aY, aX are switched!
    procedure UpdateOwnership(const aPL: TKMHandIndex);
    // Common
    procedure InitArrays();
  public
    AvoidBuilding: TKMByte2Array; //Common map of areas where building is undesired (around Store, Mines, Woodcutters)

    constructor Create(aNavMesh: TKMNavMesh);
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    // Avoid building
    // Army presence
    property PresenceAllGroups[const aPL: TKMHandIndex; aIdx: Word]: Word read GetAllPresences;
    property Presence[const aPL: TKMHandIndex; const aIdx: Word; const aGT: TGroupType]: Word read GetPresence write SetPresence;
    property IncPresence[const aPL: TKMHandIndex; aIdx: Word; const aGT: TGroupType]: Word write SetIncPresence;
    // City influence
    property Ownership[const aPL: TKMHandIndex; aY,aX: Word]: Byte read GetOwnershipFromPoint write SetOwnershipFromPoint; // To secure compatibility with old AI
    property OwnPoly[const aPL: TKMHandIndex; const aIdx: Word]: Byte read GetOwnership write SetOwnership;
    // Common
    property InfluenceSearch: TNavMeshInfluenceSearch read fInfluenceSearch write fInfluenceSearch;

    // Avoid building
    procedure AddAvoidBuilding(aX,aY: Word; aRad: Single; aValue: Byte = 255);
    procedure RemAvoidBuilding(aArea: TKMRect);
    // Army presence
    // City influence
    function GetBestOwner(aX,aY: Word): TKMHandIndex; overload;
    function GetBestOwner(const aIdx: Word): TKMHandIndex; overload;
    function GetBestAllianceOwnership(const aPL: TKMHandIndex; const aIdx: Word; const aAllianceType: TAllianceType): Byte;
    function GetOtherOwnerships(const aPL: TKMHandIndex; const aX, aY: Word): Word;



    procedure AfterMissionInit();
    procedure UpdateState(aTick: Cardinal);
    procedure Paint(aRect: TKMRect);
  end;


implementation
uses
  Classes, Graphics, SysUtils, Math,
  KM_RenderAux,
  KM_Terrain, KM_Houses, KM_HouseCollection,
  KM_Hand, KM_HandsCollection, KM_ResHouses,
  KM_AIFields;


const
  GROUPS = 4;
  INIT_HOUSE_INFLUENCE = 255;
  MAX_INFLUENCE_DISTANCE = 150;

{ TKMInfluenceMaps }
constructor TKMInfluences.Create(aNavMesh: TKMNavMesh);
begin
  inherited Create();

  fNavMesh := aNavMesh;
  fUpdateCityIdx := 0;
  fUpdateArmyIdx := 0;
  fFloodFill := TKMInfluenceFloodFill.Create(False); // Check if True is better
  fInfluenceSearch := TNavMeshInfluenceSearch.Create(False);
end;


destructor TKMInfluences.Destroy();
begin
  fFloodFill.Free;
  fInfluenceSearch.Free;
  inherited;
end;


procedure TKMInfluences.Save(SaveStream: TKMemoryStream);
var
  PCount: Word;
  Y, Len: Integer;
begin
  PCount := gHands.Count;

  SaveStream.WriteA('Influences');
  SaveStream.Write(PCount);
  SaveStream.Write(fMapX);
  SaveStream.Write(fMapY);
  SaveStream.Write(fPolygons);
  SaveStream.Write(fUpdateCityIdx);
  SaveStream.Write(fUpdateArmyIdx);

  SaveStream.WriteA('AvoidBuilding');
  for Y := 0 to fMapY - 1 do
    SaveStream.Write(AvoidBuilding[Y,0], fMapX * SizeOf(AvoidBuilding[0,0]));

  SaveStream.WriteA('Ownership');
  Len := Length(fOwnership);
  SaveStream.Write(Len);
  SaveStream.Write(fOwnership[0], SizeOf(fOwnership[0]) * Len);

  SaveStream.WriteA('ArmyPresence');
  Len := Length(fPresence);
  SaveStream.Write(Len);
  SaveStream.Write(fPresence[0], SizeOf(fPresence[0]) * Len);
end;


procedure TKMInfluences.Load(LoadStream: TKMemoryStream);
var
  PCount: Word;
  Y, Len: Integer;
begin
  LoadStream.ReadAssert('Influences');
  LoadStream.Read(PCount);
  LoadStream.Read(fMapX);
  LoadStream.Read(fMapY);
  LoadStream.Read(fPolygons);
  LoadStream.Read(fUpdateCityIdx);
  LoadStream.Read(fUpdateArmyIdx);

  LoadStream.ReadAssert('AvoidBuilding');
  SetLength(AvoidBuilding, fMapY, fMapX);
  for Y := 0 to fMapY - 1 do
    LoadStream.Read(AvoidBuilding[Y,0], fMapX * SizeOf(AvoidBuilding[0,0]));

  LoadStream.ReadAssert('Ownership');
  LoadStream.Read(Len);
  SetLength(fOwnership, Len);
  LoadStream.Read(fOwnership[0], SizeOf(fOwnership[0]) * Len);

  LoadStream.ReadAssert('ArmyPresence');
  LoadStream.Read(Len);
  SetLength(fPresence, Len);
  LoadStream.Read(fPresence[0], SizeOf(fPresence[0]) * Len);
end;




//Make the area around to be avoided by common houses
procedure TKMInfluences.AddAvoidBuilding(aX,aY: Word; aRad: Single; aValue: Byte = 255);
var
  X,Y: Integer;
begin
  for Y := Max(aY - Ceil(aRad), 1) to Min(aY + Ceil(aRad), fMapY - 1) do
  for X := Max(aX - Ceil(aRad), 1) to Min(aX + Ceil(aRad), fMapX - 1) do
    if Sqr(aX-X) + Sqr(aY-Y) <= Sqr(aRad) then
      AvoidBuilding[Y,X] := Min(AvoidBuilding[Y,X] + aValue, 255);
end;


procedure TKMInfluences.RemAvoidBuilding(aArea: TKMRect);
var
  X,Y: Integer;
begin
  for Y := Max(aArea.Top , 1) to Min(aArea.Bottom, fMapY - 1) do
  for X := Max(aArea.Left, 1) to Min(aArea.Right , fMapX - 1) do
    AvoidBuilding[Y,X] := 0;
end;


//AI should avoid certain areas, keeping them for special houses
procedure TKMInfluences.InitAvoidBuilding();
  procedure CheckAndMarkMine(aX,aY: Integer; aHT: THouseType);
  var
    X,Y,X2,Y2: Integer;
  begin
    for Y := Max(1,aY-3) to Min(fMapY-1,aY-1) do
    for X := Max(1,aX-1) to Min(fMapX-1,aX+1) do
      if   ((aHT = ht_IronMine) AND (gTerrain.TileIsIron(X,Y) > 1))
        OR ((aHT = ht_GoldMine) AND (gTerrain.TileIsGold(X,Y) > 1)) then
      begin
        for Y2 := aY to Min(fMapY-1,aY+1) do
        for X2 := Max(1,aX-2) to Min(fMapX-1,aX+1+Byte(aHT = ht_IronMine)) do
          AvoidBuilding[Y2, X2] := AVOID_BUILDING_COAL_TILE;
        Exit;
      end;
  end;
var
  H: TKMHouse;
  I,X,Y: Integer;
begin
  for Y := 0 to fMapY - 1 do
  for X := 0 to fMapX - 1 do
    AvoidBuilding[Y,X] := 0;

  //Avoid areas where Gold/Iron mines should be
  for Y := 3 to fMapY - 2 do
  for X := 2 to fMapX - 2 do
    if gTerrain.CanPlaceHouse(KMPoint(X,Y), ht_IronMine) then
      CheckAndMarkMine(X,Y, ht_IronMine)
    else if gTerrain.CanPlaceHouse(KMPoint(X,Y), ht_GoldMine) then
      CheckAndMarkMine(X,Y, ht_GoldMine);

  //Avoid Coal fields
  for Y := 1 to fMapY - 1 do
  for X := 1 to fMapX - 1 do
   AvoidBuilding[Y,X] := AvoidBuilding[Y,X] or (Byte(gTerrain.TileIsCoal(X, Y) > 1) * $FF);

  //Leave free space BELOW all players Stores
  for I := 0 to gHands.Count - 1 do
  begin
    H := gHands[I].FindHouse(ht_Store);
    if (H <> nil) then
    for Y := Max(H.Entrance.Y + 1, 1) to Min(H.Entrance.Y + 2, fMapY - 1) do
    for X := Max(H.Entrance.X - 1, 1) to Min(H.Entrance.X + 1, fMapX - 1) do
      AvoidBuilding[Y,X] := AvoidBuilding[Y,X] or $FF;
  end;
end;




function TKMInfluences.GetAllPresences(const aPL: TKMHandIndex; aIdx: Word): Word;
var
  GT: TGroupType;
begin
  Result := 0;
  aIdx := (aPL*fPolygons + aIdx) shl 2;
  for GT := Low(TGroupType) to High(TGroupType) do
    Result := Result + fPresence[aIdx + Byte(GT)];
end;


function TKMInfluences.GetPresence(const aPL: TKMHandIndex; const aIdx: Word; const aGT: TGroupType): Word;
begin
  Result := fPresence[((aPL*fPolygons + aIdx) shl 2) + Byte(aGT)];
end;


procedure TKMInfluences.SetPresence(const aPL: TKMHandIndex; const aIdx: Word; const aGT: TGroupType; const aPresence: Word);
begin
  fPresence[((aPL*fPolygons + aIdx) shl 2) + Byte(aGT)] := aPresence;
end;


procedure TKMInfluences.SetIncPresence(const aPL: TKMHandIndex; aIdx: Word; const aGT: TGroupType; const aPresence: Word);
begin
  aIdx := ((aPL*fPolygons + aIdx) shl 2) + Byte(aGT);
  fPresence[aIdx] := fPresence[aIdx] + aPresence;
end;


procedure TKMInfluences.UpdateMilitaryPresence(const aPL: TKMHandIndex);
const
  EACH_X_MEMBER_COEF = 10;
var
  I, K, Cnt: Integer;
  GT: TGroupType;
  G: TKMUnitGroup;
  U: TKMUnit;
  PointArr: TKMWordArray;
begin
  InitArrays();

  SetLength(PointArr,16);
  for I := 0 to fPolygons-1 do
    for GT := Low(TGroupType) to High(TGroupType) do
      Presence[aPL,I,GT] := 0;

  for I := 0 to gHands[aPL].UnitGroups.Count-1 do
  begin
    G := gHands[aPL].UnitGroups.Groups[I];
    if (G = nil) OR G.IsDead then
      continue;
    K := 0;
    Cnt := 0;
    while (K < G.Count) do
    begin
      U := G.Members[K];
      if (U <> nil) AND not U.IsDeadOrDying then
      begin
        if (Length(PointArr) <= Cnt) then
          SetLength(PointArr, Cnt + 16);
        PointArr[Cnt] := gAIFields.NavMesh.FindClosestPolygon(U.GetPosition);
        Cnt := Cnt + 1;
      end;
      K := K + EACH_X_MEMBER_COEF; // Pick each X member (Huge groups cover large areas so be sure that influence will be accurate)
    end;

    if (Cnt > 0) then
      fFloodFill.MilitaryPresence(aPL, 80, 30, Cnt-1, G.GroupType, PointArr);
  end;
end;






function TKMInfluences.GetOwnership(const aPL: TKMHandIndex; const aIdx: Word): Byte;
begin
  Result := fOwnership[aPL * fPolygons + aIdx];
end;


procedure TKMInfluences.SetOwnership(const aPL: TKMHandIndex; const aIdx: Word; const aOwnership: Byte);
begin
  fOwnership[aPL * fPolygons + aIdx] := aOwnership;
end;


function TKMInfluences.GetOwnershipFromPoint(const aPL: TKMHandIndex; aY, aX: Word): Byte;
var
  POM: Word;
begin
  POM := fNavMesh.FindClosestPolygon(KMPoint(aX,aY));
  Result := GetOwnership(aPL, POM);
end;


procedure TKMInfluences.SetOwnershipFromPoint(const aPL: TKMHandIndex; aY, aX: Word; const aOwnership: Byte);
begin
  aX := fNavMesh.FindClosestPolygon(KMPoint(aX,aY));
  SetOwnership(aPL, aX, aOwnership);
end;


function TKMInfluences.GetBestOwner(aX,aY: Word): TKMHandIndex;
begin
  aX := fNavMesh.FindClosestPolygon(KMPoint(aX,aY));
  Result := GetBestOwner(aX);
end;


function TKMInfluences.GetBestOwner(const aIdx: Word): TKMHandIndex;
var
  PL: TKMHandIndex;
  Best: Integer;
begin
  Result := PLAYER_NONE;
  if not AI_GEN_INFLUENCE_MAPS OR (aIdx = High(Word)) then
    Exit;

  Best := 0;
  for PL := 0 to gHands.Count - 1 do
    if (OwnPoly[PL,aIdx] > Best) then
    begin
      Best := OwnPoly[PL,aIdx];
      Result := PL;
    end;
end;


function TKMInfluences.GetBestAllianceOwnership(const aPL: TKMHandIndex; const aIdx: Word; const aAllianceType: TAllianceType): Byte;
var
  PL: TKMHandIndex;
begin
  Result := 0;
  if not AI_GEN_INFLUENCE_MAPS OR (aIdx = High(Word)) then
    Exit;

  for PL := 0 to gHands.Count - 1 do
    if (aPL <> PL) AND (gHands[aPL].Alliances[PL] = aAllianceType) AND (OwnPoly[PL,aIdx] > Result) then
      Result := OwnPoly[PL,aIdx];
end;


function TKMInfluences.GetOtherOwnerships(const aPL: TKMHandIndex; const aX, aY: Word): Word;
var
  PL: TKMHandIndex;
  Idx: Word;
begin
  Result := 0;
  Idx := fNavMesh.FindClosestPolygon(KMPoint(aX,aY));
  if not AI_GEN_INFLUENCE_MAPS OR (Idx = High(Word)) then
    Exit;

  Result := 0;
  for PL := 0 to gHands.Count - 1 do
    if (PL <> aPL) then
      Result := Result + OwnPoly[PL,Idx];
end;


// Here is the main reason for reworking influences: only 1 flood fill for city per a update + ~20x less elements in array
procedure TKMInfluences.UpdateOwnership(const aPL: TKMHandIndex);
var
  I, Idx, Cnt: Integer;
  H: TKMHouse;
  IdxArray: TKMWordArray;
begin
  InitArrays();

  //Clear array
  for Idx := 0 to fPolygons - 1 do
    OwnPoly[aPL, Idx] := 0;

  // Create array of polygon indexes
  SetLength(IdxArray, gHands[aPL].Houses.Count);
  Cnt := 0;
  for I := 0 to gHands[aPL].Houses.Count - 1 do
  begin
    H := gHands[aPL].Houses[I];
    if not H.IsDestroyed AND (H.HouseType <> ht_WatchTower) then  // Ignore watchtower?????????????????????????????????????????????
    begin
      Idx := fNavMesh.FindClosestPolygon( H.GetPosition );
      if (Idx <> High(Word)) then
      begin
        IdxArray[Cnt] := Idx;
        Cnt := Cnt + 1;
      end;
    end;
  end;

  if (Cnt > 0) then
    fFloodFill.HouseInfluence(aPL, INIT_HOUSE_INFLUENCE, MAX_INFLUENCE_DISTANCE, Cnt - 1, IdxArray);
end;




procedure TKMInfluences.InitArrays();
var
  I: Integer;
begin
  if (fPolygons <> Length(gAIFields.NavMesh.Polygons)) then
  begin
    fPolygons := Length(gAIFields.NavMesh.Polygons);
    SetLength(fPresence, gHands.Count * fPolygons * GROUPS);
    SetLength(fOwnership, gHands.Count * fPolygons);
    for I := 0 to Length(fPresence) - 1 do
      fPresence[I] := 0;
    for I := 0 to Length(fOwnership) - 1 do
      fOwnership[I] := 0;
  end;
end;


procedure TKMInfluences.AfterMissionInit();
var
  PL: TKMHandIndex;
begin
  fMapX := gTerrain.MapX;
  fMapY := gTerrain.MapY;
  SetLength(AvoidBuilding, fMapY, fMapX);
  InitAvoidBuilding();
  InitArrays();

  if AI_GEN_INFLUENCE_MAPS then
    for PL := 0 to gHands.Count - 1 do
      UpdateOwnership(PL);
end;


procedure TKMInfluences.UpdateState(aTick: Cardinal);
begin
  // City:
  if aTick mod 150 = 15 then // Update every 15 sec 1 player
  begin
    fUpdateCityIdx := (fUpdateCityIdx + 1) mod gHands.Count;
    UpdateOwnership(fUpdateCityIdx);
  end;
  // Army:
  if (aTick mod 5 = 0) then // Update every 0.5 sec 1 player
  begin
    fUpdateArmyIdx := (fUpdateArmyIdx + 1) mod gHands.Count;
    UpdateMilitaryPresence(fUpdateArmyIdx);
  end;
end;


//Render debug symbols
procedure TKMInfluences.Paint(aRect: TKMRect);
var
  PL, WatchedPL: TKMHandIndex;
  I, Cnt: Word;
  X,Y: Integer;
  PolyArr: TPolygonArray;
  NodeArr: TNodeArray;
  Col: Cardinal;
begin
  if not AI_GEN_NAVMESH OR not AI_GEN_INFLUENCE_MAPS then
    Exit;

  if OVERLAY_AVOID then
    for Y := aRect.Top to aRect.Bottom do
    for X := aRect.Left to aRect.Right do
    begin
      Col := AvoidBuilding[Y,X] * 65793 OR $80000000;
      gRenderAux.Quad(X, Y, Col);
    end;

  if OVERLAY_INFLUENCE OR OVERLAY_OWNERSHIP then
  begin
    PolyArr := fNavMesh.Polygons;
    NodeArr := fNavMesh.Nodes;
    for I := 0 to fPolygons - 1 do
    begin
      PL := GetBestOwner(I);
      if (PL = PLAYER_NONE) then
        continue
      else
        Col := (gHands[PL].FlagColor AND $FFFFFF) OR (OwnPoly[PL,I] shl 24);

      //NavMesh polys coverage
      gRenderAux.TriangleOnTerrain(
        NodeArr[PolyArr[I].Indices[0]].Loc.X,
        NodeArr[PolyArr[I].Indices[0]].Loc.Y,
        NodeArr[PolyArr[I].Indices[1]].Loc.X,
        NodeArr[PolyArr[I].Indices[1]].Loc.Y,
        NodeArr[PolyArr[I].Indices[2]].Loc.X,
        NodeArr[PolyArr[I].Indices[2]].Loc.Y, Col);
    end;
  end;

  if OVERLAY_AI_COMBAT then
  begin
    WatchedPL := gMySpectator.HandIndex;
    if (WatchedPL = PLAYER_NONE) then
      Exit;

    PolyArr := fNavMesh.Polygons;
    NodeArr := fNavMesh.Nodes;

    for PL := 0 to gHands.Count - 1 do
    begin
      if (WatchedPL = PL) then
        Col := $0000FF00 // Green
      else if (gHands[WatchedPL].Alliances[PL] = at_Ally) then
        Col := $00FF0000 // Blue
      else
        Col := $000000FF; // Red

      for I := 0 to fPolygons - 1 do
      begin
        Cnt := PresenceAllGroups[PL,I];
        if (Cnt > 0) then
        begin
          Cnt := Min(Cnt,$5F);
          //NavMesh polys coverage
          gRenderAux.TriangleOnTerrain(
            NodeArr[PolyArr[I].Indices[0]].Loc.X,
            NodeArr[PolyArr[I].Indices[0]].Loc.Y,
            NodeArr[PolyArr[I].Indices[1]].Loc.X,
            NodeArr[PolyArr[I].Indices[1]].Loc.Y,
            NodeArr[PolyArr[I].Indices[2]].Loc.X,
            NodeArr[PolyArr[I].Indices[2]].Loc.Y, (Col OR (Cnt shl 24)) ); // (Col OR $50000000)
        end;
      end;
    end;
    //for I := 0 to fPolygons - 1 do
    //begin
    //  BestCnt := 0;
    //  for PL := 0 to gHands.Count - 1 do
    //  begin
    //    Cnt := PresenceAllGroups[PL,I];
    //    if (Cnt > BestCnt) then
    //    begin
    //      BestCnt := Cnt;
    //      if (WatchedPL = PL) then
    //        Col := $0000FF00 // Green
    //      else if (gHands[WatchedPL].Alliances[PL] = at_Ally) then
    //        Col := $00FF0000 // Blue
    //      else
    //        Col := $000000FF; // Red
    //    end;
    //  end;
    //  if (BestCnt > 0) then
    //  begin
    //    BestCnt := Min(BestCnt,$9F);
    //    //NavMesh polys coverage
    //    gRenderAux.TriangleOnTerrain(
    //      NodeArr[PolyArr[I].Indices[0]].Loc.X,
    //      NodeArr[PolyArr[I].Indices[0]].Loc.Y,
    //      NodeArr[PolyArr[I].Indices[1]].Loc.X,
    //      NodeArr[PolyArr[I].Indices[1]].Loc.Y,
    //      NodeArr[PolyArr[I].Indices[2]].Loc.X,
    //      NodeArr[PolyArr[I].Indices[2]].Loc.Y, (Col OR (BestCnt shl 24)) );
    //  end;
    //end;
  end;
end;

end.
