unit KM_ArmyManagement;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_CommonClasses, KM_Defaults, KM_Points,
  KM_Units, KM_UnitGroups, KM_AISetup,
  KM_HandStats, KM_ArmyAttack, KM_ArmyDefence,
  KM_NavMeshFloodPositioning;

type
  TKMArmyManagement = class
  private
    fOwner: TKMHandIndex;
    fSetup: TKMHandAISetup;
    fLastEquippedTimeIron, fLastEquippedTimeLeather: Cardinal;

    fAttack: TKMArmyAttack;
    fDefence: TKMArmyDefence;

    fBalanceText: UnicodeString;

    procedure RecruitSoldiers();
    procedure CheckGroupsState();
    procedure CheckAttack();
  public
    constructor Create(aPlayer: TKMHandIndex; aSetup: TKMHandAISetup);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad();

    property Attack: TKMArmyAttack read fAttack write fAttack;
    property Defence: TKMArmyDefence read fDefence write fDefence;

    procedure AfterMissionInit();
    procedure UpdateState(aTick: Cardinal);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    procedure WarriorEquipped(aGroup: TKMUnitGroup);
    //function GetArmyDemand(): ...;
    //procedure RetaliateAgainstThreat(aAttacker: TKMUnit);

    procedure LogStatus(var aBalanceText: UnicodeString);
    procedure Paint();
  end;


implementation
uses
  KM_Game, KM_Hand, KM_HandsCollection, KM_Terrain, KM_AIFields,
  KM_Houses, KM_HouseBarracks,
  KM_ResHouses, KM_NavMesh, KM_CommonUtils, KM_RenderAux;


{ TKMArmyManagement }
constructor TKMArmyManagement.Create(aPlayer: TKMHandIndex; aSetup: TKMHandAISetup);
begin
  inherited Create;

  fOwner := aPlayer;
  fSetup := aSetup;

  fAttack := TKMArmyAttack.Create(aPlayer);
  fDefence := TKMArmyDefence.Create(aPlayer);
end;


destructor TKMArmyManagement.Destroy();
begin
  fAttack.Free;
  fDefence.Free;

  inherited;
end;


procedure TKMArmyManagement.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('ArmyManagement');
  SaveStream.Write(fOwner);
  SaveStream.Write(fLastEquippedTimeIron);
  SaveStream.Write(fLastEquippedTimeLeather);

  fAttack.Save(SaveStream);
  fDefence.Save(SaveStream);
end;


procedure TKMArmyManagement.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('ArmyManagement');
  LoadStream.Read(fOwner);
  LoadStream.Read(fLastEquippedTimeIron);
  LoadStream.Read(fLastEquippedTimeLeather);

  fAttack.Load(LoadStream);
  fDefence.Load(LoadStream);
end;


procedure TKMArmyManagement.SyncLoad();
begin
  fAttack.SyncLoad();
  fDefence.SyncLoad();
end;


procedure TKMArmyManagement.AfterMissionInit();
begin
  fAttack.AfterMissionInit();
  //fDefence.AfterMissionInit();

  //if (gGame.MissionMode = mm_Tactic) then
  //  fAttack.OrderToAttack;
end;


procedure TKMArmyManagement.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
  fAttack.OwnerUpdate(aPlayer);
  fDefence.OwnerUpdate(aPlayer);
end;


procedure TKMArmyManagement.WarriorEquipped(aGroup: TKMUnitGroup);
begin
  fDefence.FindPlaceForGroup(aGroup, True);
end;


// It is not army management who will decide which unit should be recruited but rather consideration of enemy's units (Eye function)
//function TKMArmyManagement.GetArmyDemand(): ...;
//begin
//
//end;


procedure TKMArmyManagement.RecruitSoldiers();

  function CanEquipIron: Boolean;
  begin
    Result := fSetup.UnlimitedEquip or gGame.CheckTime(fLastEquippedTimeIron + fSetup.EquipRateIron);
  end;

  function CanEquipLeather: Boolean;
  begin
    Result := fSetup.UnlimitedEquip or gGame.CheckTime(fLastEquippedTimeLeather + fSetup.EquipRateLeather);
  end;

var
  Barracks: array of TKMHouseBarracks;
  H: TKMHouse;
  GT: TGroupType;
  I,K: Integer;
  UT: TUnitType;
  GroupReq: TGroupTypeArray;
begin
  // Peace time; Max soldiers limit reached; cannot equip; no Barracks
  if gGame.IsPeaceTime
    OR ((fSetup.MaxSoldiers <> -1) AND (gHands[fOwner].Stats.GetArmyCount >= fSetup.MaxSoldiers))
    OR (not CanEquipIron AND not CanEquipLeather)
    OR (gHands[fOwner].Stats.GetHouseQty(ht_Barracks) = 0) then
    Exit;

  //Create a list of troops that need to be trained based on defence position requirements
  FillChar(GroupReq, SizeOf(GroupReq), #0); //Clear up
  for I := 0 to fDefence.Count - 1 do
    with fDefence[I] do
      if (CurrentGroup = nil) then
        Inc(GroupReq[GroupType], fDefence.TroopFormations[GroupType].NumUnits)
      else
        Inc(GroupReq[GroupType], Max(fDefence.TroopFormations[GroupType].NumUnits - CurrentGroup.Count, 0));

  //If we don't need anyone - Exit
  I := 0;
  for GT := Low(GroupReq) to High(GroupReq) do
    Inc(I, GroupReq[GT]);
  if I = 0 then
    Exit;

  //Find barracks
  SetLength(Barracks, gHands[fOwner].Stats.GetHouseQty(ht_Barracks));
  for I := 0 to Length(Barracks) - 1 do
    Barracks[I] := nil; // Just to be sure
  K := 0;
  for I := 0 to gHands[fOwner].Houses.Count - 1 do
  begin
    H := gHands[fOwner].Houses[I];
    if (H.HouseType = ht_Barracks) AND not H.IsDestroyed AND H.IsComplete then
    begin
      Barracks[K] := TKMHouseBarracks(H);
      Inc(K);
    end;
  end;

  //Train troops where possible in each barracks
  for I := 0 to High(Barracks) do
    if (Barracks[I] <> nil) then
    begin
      //Chose a random group type that we are going to attempt to train (so we don't always train certain group types first)
      K := 0;
      repeat
        GT := TGroupType(KaMRandom(4)); //Pick random from overall count
        Inc(K);
      until (GroupReq[GT] > 0) or (K > 9); //Limit number of attempts to guarantee it doesn't loop forever

      if (GroupReq[GT] = 0) then
        Break; // Don't train

      for K := Low(AITroopTrainOrder[GT]) to High(AITroopTrainOrder[GT]) do
      begin
        UT := AITroopTrainOrder[GT, K];

        if (UT <> ut_None) then
          while (  ( CanEquipIron AND (UT in WARRIORS_IRON) ) OR ( CanEquipLeather AND not (UT in WARRIORS_IRON) )  )
            AND Barracks[I].CanEquip(UT)
            AND (GroupReq[GT] > 0)
            AND (  ( fSetup.MaxSoldiers = -1 ) OR ( gHands[fOwner].Stats.GetArmyCount < fSetup.MaxSoldiers )  ) do
          begin
            Barracks[I].Equip(UT, 1);
            Dec(GroupReq[GT]);
            //Only reset it when we actually trained something (in IronThenLeather mode we don't count them separately)
            if (UT in WARRIORS_IRON) OR (fSetup.ArmyType = atIronThenLeather) then
              fLastEquippedTimeIron := gGame.GameTickCount;
            if not (UT in WARRIORS_IRON) OR (fSetup.ArmyType = atIronThenLeather) then
              fLastEquippedTimeLeather := gGame.GameTickCount;
          end;
      end;
  end;
end;


//Check army food level and positioning
procedure TKMArmyManagement.CheckGroupsState();
var
  I: Integer;
  Group: TKMUnitGroup;
begin
  // Feed army and find unused groups
  for I := 0 to gHands[fOwner].UnitGroups.Count - 1 do
  begin
    Group := gHands[fOwner].UnitGroups[I];

    if not Group.IsDead AND Group.IsIdleToAI then
    begin
      // Check hunger and order food
      if (Group.Condition < UNIT_MIN_CONDITION) then
        // Cheat for autobuild AI: Only feed hungry group members (food consumption lower and more predictable)
        Group.OrderFood(True, fSetup.AutoBuild);

      // Do not process attack or defence during peacetime
      if gGame.IsPeaceTime then
        Continue;

      // Group is in combat classes
      if fAttack.IsGroupInAction(Group) then
        Continue;

      // Group is in defence position
      if (fDefence.FindPositionOf(Group) <> nil) then
        Continue;

      // Find a place
      fDefence.FindPlaceForGroup(Group, True);
    end;
  end;
end;


procedure TKMArmyManagement.CheckAttack();
const
  COMPANY_MIN_ATTACK_CHANCE = 0.5;
var
  ForceToAttack: Boolean;
  I, Cnt: Integer;
  TargetOwner: TKMHandIndex;
  TargetPoint: TKMPoint;
  Group: TKMUnitGroup;
  AvaiableGroups: TKMUnitGroupArray;
begin
  //Do not process attack or defence during peacetime
  if gGame.IsPeaceTime then Exit;

  // 1. There must be enought soldiers for defences
  case fDefence.DefenceStatus() of
    ds_Empty: Exit;
    ds_Half: ForceToAttack := False;
    ds_Full: ForceToAttack := True;
  end;
  // ForceToAttack := ForceToAttack OR (gGame.MissionMode = mm_Tactic);

  // Get array of pointers to avaiable groups
  Cnt := 0;
  for I := 0 to gHands[fOwner].UnitGroups.Count - 1 do
  begin
    Group := gHands[fOwner].UnitGroups[I];
    if Group.IsDead OR not Group.IsIdleToAI then
      continue;
    if ForceToAttack then
    begin
      // Take all groups out of attack class
      if not fAttack.IsGroupInAction(Group) then
      begin
        if (Length(AvaiableGroups) <= Cnt) then
          SetLength(AvaiableGroups, Length(AvaiableGroups) + 16);
        AvaiableGroups[Cnt] := Group;
        Cnt := Cnt + 1;
      end;
    end
    else
    begin
      // Take group in defence position
      if (fDefence.FindPositionOf(Group) <> nil) then
      begin
        if (Length(AvaiableGroups) <= Cnt) then
          SetLength(AvaiableGroups, Length(AvaiableGroups) + 16);
        AvaiableGroups[Cnt] := Group;
        Cnt := Cnt + 1;
      end;
    end;
  end;
  // If there are not free groups exit
  if (Cnt = 0) then
    Exit;


  if fAttack.FindBestTarget(TargetOwner, TargetPoint, ForceToAttack) then
  begin
    SetLength(AvaiableGroups, Cnt);
    for I := Low(AvaiableGroups) to High(AvaiableGroups) do
      fDefence.ReleaseGroup(AvaiableGroups[I]);
    //if ForceToAttack OR (gAIFields.Eye.ArmyEvaluation.CompareAllianceStrength(TargetOwner, AvaiableGroups) > COMPANY_MIN_ATTACK_CHANCE) then
    fAttack.OrderToAttack(TargetPoint, AvaiableGroups);

  end;

  //Comparison := gAIFields.Eye.ArmyEvaluation.CompareAllianceStrength(fOwner, EnemyStats[I].Player) - (EnemyStats[I].Distance / MinDist - 1) * DISTANCE_COEF;
  //fAttack.AttackChance(AvaiableGroups);
end;



{
procedure TKMGeneral.RetaliateAgainstThreat(aAttacker: TKMUnit);
var
  I: Integer;
  Group: TKMUnitGroup;
begin
  if gHands[fOwner].HandType = hndHuman then Exit;

  //Attacker may be already dying (e.g. killed by script)
  //We could retaliate against his whole group however
  if (aAttacker = nil) or aAttacker.IsDeadOrDying or (aAttacker is TKMUnitRecruit) then Exit;

  //todo: Right now "idle" troops (without an assigned defence position) will do nothing (no attacking, defending, etc.)
  //Any defence position that is within their defence radius of this threat will retaliate against it
  for I := 0 to fDefencePositions.Count - 1 do
  begin
    Group := fDefencePositions[I].CurrentGroup;
    if (Group <> nil)
    and not Group.IsDead
    and Group.IsIdleToAI(True) //Units walking to their defence position can retaliate (but not if pursuing an enemy)
    //@Lewin: Is it right that Group defends against attackers within the Rad
    //rather than defending property within the Rad?
    //Think of archer, he attacks property in AI defense radius, but stands utself outside of the radius
    //should AI retaliate against him or not?
    //@Krom: Yes it's right the way it is now. It should be the attacker not the victim.
    //Otherwise the AI sends much more groups when you shoot them with 1 bowmen in the campaigns.
    //Right now it seems to be working almost the same as in the original game.
    and (KMLengthDiag(Group.Position, aAttacker.GetPosition) <= fDefencePositions[I].Radius) then
      Group.OrderAttackUnit(aAttacker, True);
  end;
end;
//}






procedure TKMArmyManagement.UpdateState(aTick: Cardinal);
const
  PERF_TIME_LIMIT = MAX_HANDS * 10;
  PERF_TIME_LIMIT_2 = MAX_HANDS * 11;
begin
  if (aTick mod MAX_HANDS = fOwner) then
  begin
    if (aTick mod PERF_TIME_LIMIT = fOwner) then
      fDefence.UpdateDefences();
    if (aTick mod PERF_TIME_LIMIT_2 = fOwner) then
      CheckAttack();
    RecruitSoldiers();
    CheckGroupsState();
    fAttack.UpdateState(aTick);
    fDefence.UpdateState(aTick);
  end;
end;


procedure TKMArmyManagement.LogStatus(var aBalanceText: UnicodeString);
begin
  aBalanceText := '';
end;


procedure TKMArmyManagement.Paint();
begin
  //gRenderAux.CircleOnTerrain(X, Y, 5, $09FFFFFF, $99FFFFFF);
  //gRenderAux.Quad(X, Y, Color: Cardinal);
end;


end.
