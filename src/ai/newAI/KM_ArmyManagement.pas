unit KM_ArmyManagement;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_CommonClasses, KM_CommonTypes, KM_Defaults, KM_Points,
  KM_Houses, KM_Units, KM_Units_Warrior,
  KM_UnitGroups, KM_AISetup,
  KM_HandStats, KM_ArmyAttack, KM_ArmyDefence,
  KM_NavMeshFloodPositioning;

type
  TKMArmyManagement = class
  private
    fOwner: TKMHandIndex;
    fSetup: TKMHandAISetup;
    fLastEquippedTimeIron, fLastEquippedTimeLeather: Cardinal;

    fHostileGroups: TList;
    fAttack: TKMArmyAttack;
    fDefence: TKMArmyDefence;

    fBalanceText: UnicodeString;

    procedure RecruitSoldiers();
    procedure CheckGroupsState();
    procedure CheckAttack();
    procedure AddNewThreat(aAttacker: TKMUnitWarrior; aUnit: TKMUnit = nil);
    procedure CheckThreats();

    function CombineBalanceStrings(): UnicodeString;
  public
    constructor Create(aPlayer: TKMHandIndex; aSetup: TKMHandAISetup);
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);
    procedure SyncLoad();

    property Attack: TKMArmyAttack read fAttack write fAttack;
    property Defence: TKMArmyDefence read fDefence write fDefence;
    property BalanceText: UnicodeString read CombineBalanceStrings;

    procedure AfterMissionInit();
    procedure UpdateState(aTick: Cardinal);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);
    procedure WarriorEquipped(aGroup: TKMUnitGroup);
    //function GetArmyDemand(): ...;
    procedure CheckNewThreat(aHouse: TKMHouse; aAttacker: TKMUnitWarrior); overload;
    procedure CheckNewThreat(aUnit: TKMUnit; aAttacker: TKMUnit); overload;

    procedure Paint();
  end;


implementation
uses
  KM_Game, KM_Hand, KM_HandsCollection, KM_Terrain, KM_AIFields,
  KM_HouseBarracks,
  KM_ResHouses, KM_NavMesh, KM_CommonUtils, KM_RenderAux;


{ TKMArmyManagement }
constructor TKMArmyManagement.Create(aPlayer: TKMHandIndex; aSetup: TKMHandAISetup);
begin
  inherited Create;

  fOwner := aPlayer;
  fSetup := aSetup;

  fHostileGroups := TList.Create();
  fAttack := TKMArmyAttack.Create(aPlayer);
  fDefence := TKMArmyDefence.Create(aPlayer, fAttack, fHostileGroups);
end;


destructor TKMArmyManagement.Destroy();
var
  I: Integer;
  UG: TKMUnitGroup;
begin
  //We don't have to release unit pointers, because the group is only destroyed when the game is canceled
  for I := fHostileGroups.Count - 1 downto 0 do
  begin
    UG := fHostileGroups.Items[I];
    gHands.CleanUpGroupPointer( UG );
    //fHostileGroups.Delete(I);
  end;
  fHostileGroups.Free;
  fAttack.Free;
  fDefence.Free;

  inherited;
end;


procedure TKMArmyManagement.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
  UG: TKMUnitGroup;
begin
  SaveStream.WriteA('ArmyManagement');
  SaveStream.Write(fOwner);
  SaveStream.Write(fLastEquippedTimeIron);
  SaveStream.Write(fLastEquippedTimeLeather);

  SaveStream.Write( Integer(fHostileGroups.Count) );
  for I := 0 to fHostileGroups.Count - 1 do
  begin
    UG := fHostileGroups.Items[I];
    if (UG <> nil) then
      SaveStream.Write(UG.UID) //Store ID
    else
      SaveStream.Write(Integer(0));
  end;

  fAttack.Save(SaveStream);
  fDefence.Save(SaveStream);
end;


procedure TKMArmyManagement.Load(LoadStream: TKMemoryStream);
var
  I, Count: Integer;
  UG: TKMUnitGroup;
begin
  LoadStream.ReadAssert('ArmyManagement');
  LoadStream.Read(fOwner);
  LoadStream.Read(fLastEquippedTimeIron);
  LoadStream.Read(fLastEquippedTimeLeather);

  LoadStream.Read(Count);
  for I := 0 to Count - 1 do
  begin
    LoadStream.Read(UG, 4);
    fHostileGroups.Add(UG);
  end;

  fAttack.Load(LoadStream);
  fDefence.Load(LoadStream);
end;


procedure TKMArmyManagement.SyncLoad();
var
  I: Integer;
begin
  for I := 0 to fHostileGroups.Count - 1 do
    fHostileGroups.Items[I] := gHands.GetGroupByUID( Cardinal(fHostileGroups.Items[I]) );

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
  fDefence.FindPlaceForGroup(aGroup);
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
  GT: TKMGroupType;
  I,K: Integer;
  UT: TKMUnitType;
  GroupReq: TKMGroupTypeArray;
begin
  // Peace time; Max soldiers limit reached; cannot equip; no Barracks
  if gGame.IsPeaceTime
    OR ((fSetup.MaxSoldiers <> -1) AND (gHands[fOwner].Stats.GetArmyCount >= fSetup.MaxSoldiers))
    OR (not CanEquipIron AND not CanEquipLeather)
    OR (gHands[fOwner].Stats.GetHouseQty(htBarracks) = 0)
    OR (fDefence.Count = 0) then
    Exit;

  //Create a list of troops that need to be trained based on defence position requirements
  FillChar(GroupReq, SizeOf(GroupReq), #0); //Clear up
  //for I := 0 to fDefence.Count - 1 do
  //  with fDefence[I] do
  //    if (CurrentGroup = nil) then
  //      Inc(GroupReq[GroupType], fDefence.TroopFormations[GroupType].NumUnits)
  //    else
  //      Inc(GroupReq[GroupType], Max(fDefence.TroopFormations[GroupType].NumUnits - CurrentGroup.Count, 0));

  // Take required warriors from CityManagement (-> implemented consideration of required units + save time)
  for GT := Low(TKMGroupType) to High(TKMGroupType) do
    for I := Low(AITroopTrainOrder[GT]) to High(AITroopTrainOrder[GT]) do
      if (AITroopTrainOrder[GT,I] <> ut_None) then
        Inc(GroupReq[GT], gHands[fOwner].AI.CityManagement.WarriorsDemands[ AITroopTrainOrder[GT,I] ] + 1); // Always recruit something

  //If we don't need anyone - Exit
  //I := 0;
  //for GT := Low(GroupReq) to High(GroupReq) do
  //  Inc(I, GroupReq[GT]);
  //if (I = 0) then
  //  Exit;

  //Find barracks
  SetLength(Barracks, gHands[fOwner].Stats.GetHouseQty(htBarracks));
  for I := 0 to Length(Barracks) - 1 do
    Barracks[I] := nil; // Just to be sure
  K := 0;
  for I := 0 to gHands[fOwner].Houses.Count - 1 do
  begin
    H := gHands[fOwner].Houses[I];
    if (H <> nil) AND not H.IsDestroyed AND (H.HouseType = htBarracks) AND H.IsComplete then
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
        GT := TKMGroupType(KaMRandom(4)); //Pick random from overall count
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
      fDefence.FindPlaceForGroup(Group);
    end;
  end;
end;


procedure TKMArmyManagement.CheckAttack();
type TAvailableGroups = record
  Count: Word;
  GroupArr: TKMUnitGroupArray;
  Price: TKMWordArray;
end;
  // Order multiple companies with equally distributed group types
  procedure OrderAttack(aTargetPoint: TKMPoint; aAGCnt: Word; aAvailableGroups: TKMUnitGroupArray);
  const
    MAX_GROUPS_IN_COMPANY = 9;
  var
    I, K, CompaniesCnt, GTMaxCnt, GCnt, HighAG: Integer;
    GT: TKMGroupType;
    GTArr: array[TKMGroupType] of Integer;
    Groups: TKMUnitGroupArray;
  begin
    // Get count of available group types
    FillChar(GTArr, SizeOf(GTArr), #0);
    //for GT in TKMGroupType do
    //  GTArr[GT] := 0;

    for I := 0 to aAGCnt - 1 do
      Inc(  GTArr[ aAvailableGroups[I].GroupType ]  );

    CompaniesCnt := Max(1, Ceil(aAGCnt / MAX_GROUPS_IN_COMPANY));
    HighAG := aAGCnt - 1;
    for I := 0 to CompaniesCnt - 1 do
    begin
      GCnt := 0;
      for GT := Low(TKMGroupType) to High(TKMGroupType) do
      begin
        GTMaxCnt := Max(0, Round(GTArr[GT] / (CompaniesCnt - I)));
        GTArr[GT] := GTArr[GT] - GTMaxCnt;
        for K := HighAG downto 0 do
          if (GTMaxCnt <= 0) then
            break
          else if (aAvailableGroups[K].GroupType = GT) then
          begin
            if (Length(Groups) <= GCnt) then
              SetLength(Groups, GCnt + MAX_GROUPS_IN_COMPANY);
            Groups[GCnt] := aAvailableGroups[K];
            GCnt := GCnt + 1;
            aAvailableGroups[K] := aAvailableGroups[HighAG];
            HighAG := HighAG - 1;
            GTMaxCnt := GTMaxCnt - 1;
          end;
       end;

      SetLength(Groups, GCnt);
      fattack.CreateCompany(aTargetPoint, Groups);
    end;
  end;
const
  COMPANY_MIN_ATTACK_CHANCE = 0.5;
  MIN_TROOPS_IN_GROUP = 6;
  MIN_GROUPS_IN_ATTACK = 4;
var
  ForceToAttack, TakeAllIn: Boolean;
  BestPrice,BestIdx: Word;
  I,K: Integer;
  TargetOwner: TKMHandIndex;
  TargetPoint: TKMPoint;
  Group: TKMUnitGroup;
  DP: TKMDefencePosition;
  AG: TAvailableGroups;
begin
  //Do not process attack or defence during peacetime
  if gGame.IsPeaceTime then Exit;

  // 1. There must be enought soldiers for defences
  // In case that there are not defeces maps is in combat mode so we should launch everything
  TakeAllIn := False;
  ForceToAttack := False;
  case fDefence.DefenceStatus() of
    ds_Empty: Exit;
    ds_Half: ForceToAttack := False;
    ds_Full: ForceToAttack := True;
    ds_None: begin
      TakeAllIn := True;
      ForceToAttack := True;
    end;
  end;
  // ForceToAttack := ForceToAttack OR (gGame.MissionMode = mm_Tactic); // Maybe force attack in mm_Tactic map? But some of mm_Tactic map are also defensive...

  // Get array of pointers to available groups
  AG.Count := 0;
  SetLength(AG.GroupArr, 4);
  SetLength(AG.Price, 4);
  for I := 0 to gHands[fOwner].UnitGroups.Count - 1 do
  begin
    Group := gHands[fOwner].UnitGroups[I];
    if (Group = nil)
      OR Group.IsDead
      OR not Group.IsIdleToAI([wtokFlagPoint, wtokHaltOrder])
      OR (not TakeAllIn AND (Group.Count < MIN_TROOPS_IN_GROUP)) then
      Continue;
    // Add grop pointer to array (but dont increase count now so it will be ignored)
    if (Length(AG.GroupArr) <= AG.Count) then
    begin
      SetLength(AG.GroupArr, AG.Count + 16);
      SetLength(AG.Price, AG.Count + 16);
    end;
    AG.GroupArr[AG.Count] := Group;
    AG.Price[AG.Count] := 0;
    // Check if group can be in array
    if ForceToAttack then
    begin
      // Take all groups out of attack class
      if not fAttack.IsGroupInAction(Group) then
        Inc(AG.Count,1); // Confirm that the group should be in array GroupArr
    end
    else
    begin
      // Take group in defence position
      DP := fDefence.FindPositionOf(Group);
      if (DP <> nil) then
      begin
        AG.Price[AG.Count] := DP.Weight; // Change weight
        Inc(AG.Count,1); // Confirm that the group should be in array GroupArr
      end;
    end;
  end;
  // If we dont have enought groups then exit
  if (AG.Count = 0) OR (not ForceToAttack AND (AG.Count < MIN_GROUPS_IN_ATTACK + fDefence.FirstLineCnt)) then
    Exit
  // Else remove first line from available groups
  else if not ForceToAttack then
    for I := 0 to Min(fDefence.FirstLineCnt,AG.Count) - 1 do
    begin
      BestPrice := 0;
      BestIdx := 0;
      for K := 1 to AG.Count - 1 do
        if (BestPrice < AG.Price[K]) then
        begin
          BestPrice := AG.Price[K];
          BestIdx := K;
        end;
      Dec(AG.Count,1);
      AG.GroupArr[BestIdx] := AG.GroupArr[AG.Count];
      AG.Price[BestIdx] := AG.Price[AG.Count];
    end;

  if fAttack.FindBestTarget(TargetOwner, TargetPoint, ForceToAttack) then
  begin
    for I := 0 to AG.Count - 1 do
      fDefence.ReleaseGroup(AG.GroupArr[I]);
    OrderAttack(TargetPoint, AG.Count, AG.GroupArr);
    //if ForceToAttack OR (gAIFields.Eye.ArmyEvaluation.CompareAllianceStrength(TargetOwner, AvailableGroups) > COMPANY_MIN_ATTACK_CHANCE) then
  end;

  //Comparison := gAIFields.Eye.ArmyEvaluation.CompareAllianceStrength(fOwner, EnemyStats[I].Player) - (EnemyStats[I].Distance / MinDist - 1) * DISTANCE_COEF;
  //fAttack.AttackChance(AvailableGroups);
end;


procedure TKMArmyManagement.CheckNewThreat(aHouse: TKMHouse; aAttacker: TKMUnitWarrior);
begin
  //Attacker may be already dying (e.g. killed by script)
  if (aAttacker = nil) OR aAttacker.IsDeadOrDying then
    Exit;
  AddNewThreat(aAttacker);
end;


procedure TKMArmyManagement.CheckNewThreat(aUnit: TKMUnit; aAttacker: TKMUnit);
begin
  //Attacker may be already dying (e.g. killed by script)
  if (aAttacker = nil) OR aAttacker.IsDeadOrDying OR not (aAttacker is TKMUnitWarrior) OR aUnit.IsDeadOrDying  then
    Exit;
  AddNewThreat( TKMUnitWarrior(aAttacker), aUnit );
end;


procedure TKMArmyManagement.AddNewThreat(aAttacker: TKMUnitWarrior; aUnit: TKMUnit = nil);
var
  Group, DefenceGroup: TKMUnitGroup;
begin
  Group := gHands[ aAttacker.Owner ].UnitGroups.GetGroupByMember( aAttacker );
  if (Group = nil) OR Group.IsDead then
    Exit;
  if (fHostileGroups.IndexOf(Group) = -1) then // Is this attacking group already in list?
  begin
    if (aUnit = nil) // Does attacker attack house
      OR (not (aUnit is TKMUnitWarrior) AND (gAIFields.Influences.Ownership[fOwner,aUnit.GetPosition.Y,aUnit.GetPosition.X] > 50)) then // Or citizen which is inside of city?
      fHostileGroups.Add( Group.GetGroupPointer() )
    else // Does attacker attack at soldier in defence position?
    begin
      DefenceGroup := gHands[ aUnit.Owner ].UnitGroups.GetGroupByMember( TKMUnitWarrior(aUnit) );
      if (DefenceGroup <> nil) AND not DefenceGroup.IsDead AND (fDefence.FindPositionOf(DefenceGroup) <> nil) then
        fHostileGroups.Add( Group.GetGroupPointer() );
    end;
  end;
end;


procedure TKMArmyManagement.CheckThreats();
var
  I: Integer;
  Group: TKMUnitGroup;
  UGA: TKMUnitGroupArray;
begin
  SetLength(UGA,fHostileGroups.Count);
  for I := fHostileGroups.Count - 1 downto 0 do
  begin
    UGA[I] := nil;
    Group := fHostileGroups.Items[I];
    fHostileGroups.Delete(I);
    if (Group <> nil) then
    begin
      UGA[I] := Group;
      gHands.CleanUpGroupPointer(Group);
    end;
  end;
  fDefence.FindEnemyInDefLine(UGA);
end;



procedure TKMArmyManagement.UpdateState(aTick: Cardinal);
const
  PERF_TIME_LIMIT = MAX_HANDS * 10 * 10;
  PERF_TIME_LIMIT_Tactic = MAX_HANDS * 5;
begin
  if (aTick mod MAX_HANDS = fOwner) then
  begin
    CheckThreats();
    if (aTick > 2*MAX_HANDS) AND // Make sure that influences and defences are actualized
      ( ((gGame.MissionMode = mm_Tactic) AND (aTick mod PERF_TIME_LIMIT_Tactic = fOwner))
      OR (aTick mod PERF_TIME_LIMIT = fOwner) ) then
      CheckAttack();
    RecruitSoldiers();
    CheckGroupsState();
    fAttack.UpdateState(aTick);
    fDefence.UpdateState(aTick);
  end;
end;


function TKMArmyManagement.CombineBalanceStrings(): UnicodeString;
begin
  Result := fBalanceText;
  fAttack.LogStatus(Result);
  fDefence.LogStatus(Result);
end;


procedure TKMArmyManagement.Paint();
begin

end;


end.
