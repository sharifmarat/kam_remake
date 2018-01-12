unit KM_CityManagement;
{$I KaM_Remake.inc}
interface
uses
  Classes, KromUtils, Math, SysUtils,
  KM_Defaults, KM_CommonClasses, KM_CommonUtils, KM_Points,
  KM_AISetup, KM_ResHouses, KM_ResWares, KM_ResUnits, KM_HandStats,
  KM_CityPredictor, KM_CityBuilder, KM_CityPlanner, KM_AIArmyEvaluation;

type
  TWarfareArr = array[WARFARE_MIN..WARFARE_MAX] of record
    Avaiable, Required: Word;
    Fraction: Single;
  end;

  //Mayor is the one who manages the town
  TKMCityManagement = class
  private
    fOwner: TKMHandIndex;
    fSetup: TKMHandAISetup;

    fBuilder: TKMCityBuilder;
    fPredictor: TKMCityPredictor;

    POMARR: TWarfareArr;

    fBalanceText: UnicodeString;

    procedure CheckUnitCount();
    procedure CheckMarketplaces();
    procedure CheckStoreWares();
    procedure CheckExhaustedHouses();
    procedure CheckAutoRepair();

    function WeaponsBalance(): TWarfareArr;
    procedure OrderWeapons(aWarfare: TWarfareArr);

  public
    constructor Create(aPlayer: TKMHandIndex; aSetup: TKMHandAISetup);
    destructor Destroy(); override;

    procedure AfterMissionInit();
    procedure OwnerUpdate(aPlayer: TKMHandIndex);

    property Builder: TKMCityBuilder read fBuilder write fBuilder;
    property Predictor: TKMCityPredictor read fPredictor;
    property BalanceText: UnicodeString read fBalanceText;

    procedure UpdateState(aTick: Cardinal);
    procedure LogStatus(var aBalanceText: UnicodeString);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

  end;


implementation
uses
  KM_Game, KM_Houses, KM_HouseCollection, KM_HouseSchool, KM_HandsCollection, KM_Hand, KM_Terrain, KM_Resource,
  KM_AIFields, KM_Units, KM_UnitTaskDelivery, KM_UnitActionWalkTo, KM_UnitTaskGoEat, KM_UnitsCollection,
  KM_NavMesh, KM_HouseMarket;


const
  LACK_OF_GOLD = 10;


{ TKMCityManagement }
constructor TKMCityManagement.Create(aPlayer: TKMHandIndex; aSetup: TKMHandAISetup);
begin
  inherited Create;

  fOwner := aPlayer;
  fSetup := aSetup;

  fBuilder := TKMCityBuilder.Create(aPlayer);
  fPredictor := TKMCityPredictor.Create(aPlayer);
end;


destructor TKMCityManagement.Destroy();
begin
  fBuilder.Free;
  fPredictor.Free;

  inherited;
end;


procedure TKMCityManagement.Save(SaveStream: TKMemoryStream);
begin
  SaveStream.WriteA('CityManagement');
  SaveStream.Write(fOwner);

  fBuilder.Save(SaveStream);
  fPredictor.Save(SaveStream);
end;


procedure TKMCityManagement.Load(LoadStream: TKMemoryStream);
begin
  LoadStream.ReadAssert('CityManagement');
  LoadStream.Read(fOwner);

  fBuilder.Load(LoadStream);
  fPredictor.Load(LoadStream);
end;


procedure TKMCityManagement.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
  fBuilder.OwnerUpdate(aPlayer);
  fPredictor.OwnerUpdate(aPlayer);
end;


procedure TKMCityManagement.AfterMissionInit();
  procedure SetWareDistribution();
  begin
    // Top priority for gold mines
    gHands[fOwner].Stats.WareDistribution[wt_coal, ht_Metallurgists] := 5;
    gHands[fOwner].Stats.WareDistribution[wt_coal, ht_WeaponSmithy] := 2;
    gHands[fOwner].Stats.WareDistribution[wt_coal, ht_IronSmithy] := 3;
    gHands[fOwner].Stats.WareDistribution[wt_coal, ht_ArmorSmithy] := 2;

    gHands[fOwner].Houses.UpdateResRequest;
  end;
const
  WORKER_COEF = 80.0;
var
  GoldCnt, IronCnt, FieldCnt, BuildCnt, FreeWorkersCnt, I, Cnt: Integer;
  vA1, vA2, vA3, vA4: Integer;
  A1: array[0..3] of Integer;
  A2: array of integer;
begin
  SetWareDistribution();

  vA1 := SizeOf(A1);
  vA2 := SizeOf(A2);
  SetLength(A2, 5);
  vA3 := SizeOf(A2);
  vA4 := SizeOf(A2[0]) * Length(A2);

  fSetup.ApplyAgressiveBuilderSetup;

  gAIFields.Eye.OwnerUpdate(fOwner);

  fPredictor.AfterMissionInit();

  // Find resources around Loc and change building policy
  gAIFields.Eye.ScanLocResources(GoldCnt, IronCnt, FieldCnt, BuildCnt);


      BuildCnt := 2500; // DELETE DEBUG DELETE DEBUG DELETE DEBUG DELETE DEBUG DELETE DEBUG DELETE DEBUG DELETE DEBUG DELETE DEBUG

  // Decide count of workers + build nodes
  gHands[fOwner].AI.Setup.WorkerCount := Min(35, Round(BuildCnt / WORKER_COEF));

  fBuilder.AfterMissionInit(GoldCnt, IronCnt, FieldCnt, BuildCnt);


  fPredictor.CityInitialization(GoldCnt, IronCnt, FieldCnt, BuildCnt);

  // Place all houses AfterMissionInit (only for GA_PLANNER tuning parameters)
  Cnt := 0;
  GA_PLANNER := True;
  if GA_PLANNER then
    for I := 0 to 100 do
    begin
      fBuilder.UpdateState(0, FreeWorkersCnt);
      fPredictor.UpdateState(0);
      if not fBuilder.ChooseHousesToBuild(1, fPredictor.RequiredHouses, fPredictor.WareBalance) then
      begin
        Cnt := Cnt + 1;
        if (Cnt > 5) then
          Exit;
      end
      else
        Cnt := 0;
    end;

  //SetKaMSeed(1); // REMOVE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end;


procedure TKMCityManagement.UpdateState(aTick: Cardinal);
const
  WORKER_COEF = 3.0;
  LONG_UPDATE = MAX_HANDS * 10;
var
  FreeWorkersCnt: Integer;
begin
  if (aTick mod MAX_HANDS = fOwner) then
  begin
    fPredictor.UpdateState(aTick);
    FreeWorkersCnt := 0;
    fBuilder.UpdateState(aTick, FreeWorkersCnt);
    fBuilder.ChooseHousesToBuild(Max(0,Ceil(FreeWorkersCnt/WORKER_COEF)), fPredictor.RequiredHouses, fPredictor.WareBalance);
    if not SKIP_RENDER then
    begin
      fPredictor.LogStatus(fBalanceText);
      fBuilder.LogStatus(fBalanceText);
      //LogStatus(fBalanceText);
    end;
  end;

  if (aTick mod LONG_UPDATE = fOwner) then
  begin
    if not GA_PLANNER then
      CheckUnitCount();
    //CheckMarketplaces();
    CheckStoreWares();
    OrderWeapons( WeaponsBalance() );
    CheckExhaustedHouses();
  end;
end;


procedure TKMCityManagement.CheckUnitCount;
var
  P: TKMHand;
  UnitReq: array [CITIZEN_MIN..CITIZEN_MAX] of Integer;

  function TryToTrain(aSchool: TKMHouseSchool; aUnitType: TUnitType; aRequiredCount: Integer): Boolean;
  begin
    // We summ up requirements for e.g. Recruits required at Towers and Barracks
    if P.Stats.GetUnitQty(aUnitType) < (aRequiredCount + UnitReq[aUnitType]) then
    begin
      Dec(UnitReq[aUnitType]); //So other schools don't order same unit
      aSchool.AddUnitToQueue(aUnitType, 1);
      Result := True;
    end
    else
      Result := False;
  end;

  function RecruitsNeeded: Integer;
  var AxesLeft: Integer;
  begin
    if P.Stats.GetHouseQty(ht_Barracks) = 0 then
      Result := 0
    else
      if gGame.IsPeaceTime then
      begin
        //Keep enough recruits to equip using all weapons once PT ends
        //Iron soldiers
        Result := Min(P.Stats.GetWareBalance(wt_MetalArmor),
                      P.Stats.GetWareBalance(wt_Arbalet) + P.Stats.GetWareBalance(wt_Hallebard)
                      + Min(P.Stats.GetWareBalance(wt_Sword), P.Stats.GetWareBalance(wt_MetalShield)));
        //Leather soldiers we can make
        Inc(Result, Min(P.Stats.GetWareBalance(wt_Armor),
                        P.Stats.GetWareBalance(wt_Bow) + P.Stats.GetWareBalance(wt_Pike)
                        + Min(P.Stats.GetWareBalance(wt_Axe), P.Stats.GetWareBalance(wt_Shield))));
        //Militia with leftover axes
        AxesLeft := P.Stats.GetWareBalance(wt_Axe) - Min(P.Stats.GetWareBalance(wt_Armor), P.Stats.GetWareBalance(wt_Shield));
        if AxesLeft > 0 then
          Inc(Result, AxesLeft);
      end
      else
        Result := fSetup.RecruitCount * P.Stats.GetHouseQty(ht_Barracks);
  end;

  function RequiredServCount: Byte;
  var
    I: Integer;
  begin
    Result := Max(0, gHands[fOwner].Stats.GetUnitQty(ut_Worker) + Round(gHands[fOwner].Stats.GetHouseTotal(ht_Any) * 0.5) - gHands[fOwner].Stats.GetUnitQty(ut_Serf));
    //Result := Max(0, gHands[fOwner].Stats.GetUnitQty(ut_Worker) - gHands[fOwner].Stats.GetUnitQty(ut_Serf));
    for I := 0 to P.Units.Count - 1 do
      if not P.Units[I].IsDeadOrDying
         AND (P.Units[I] is TKMUnitSerf)
         AND (P.Units[I].UnitTask = nil) then
           Exit;
    // Increase count of serfs carefully
    Result := Max( + 1
                   + Byte(gHands[fOwner].Stats.GetUnitQty(ut_Serf) < 80)
                   + Byte(gHands[fOwner].Stats.GetUnitQty(ut_Serf) < 60)*2, Result);
  end;

var
  I,K: Integer;
  H: THouseType;
  UT: TUnitType;
  Schools: array of TKMHouseSchool;
  HS: TKMHouseSchool;
  //serfCount: Integer;
const
  PRIORITY_TRAINING: array[0..12] of TUnitType = (
    ut_Miner, ut_Metallurgist, ut_StoneCutter, ut_Woodcutter, ut_Lamberjack,
    ut_Farmer, ut_AnimalBreeder, ut_Baker, ut_Butcher, ut_Fisher, ut_Smith, ut_Serf, ut_Worker
  );
begin
  P := gHands[fOwner];
  FillChar(UnitReq, SizeOf(UnitReq), #0); //Clear up

  //Citizens
  // Make sure we have enough gold left for self-sufficient city
  if (P.Stats.GetWareBalance(wt_Gold) < LACK_OF_GOLD) AND (P.Stats.GetHouseQty(ht_Metallurgists) = 0) then
  begin
    Inc(UnitReq[ut_Serf], 3 * Byte(P.Stats.GetUnitQty(ut_Serf) < 3)); // 3x Serf
    Inc(UnitReq[ut_Worker], Byte((P.Stats.GetUnitQty(ut_Worker) = 0) AND (fSetup.WorkerCount > 0)));// 1x Worker
    Inc(UnitReq[ut_Miner], P.Stats.GetHouseTotal(ht_CoalMine) + P.Stats.GetHouseTotal(ht_IronMine) + P.Stats.GetHouseTotal(ht_GoldMine) - P.Stats.GetUnitQty(ut_Miner)); // Miner can go into iron / gold / coal mines (idealy we need 1 gold and 1 coal but it is hard to catch it)
    Inc(UnitReq[ut_Metallurgist], P.Stats.GetHouseTotal(ht_Metallurgists) + P.Stats.GetHouseTotal(ht_IronSmithy) - P.Stats.GetUnitQty(ut_Metallurgist)); // Metallurgist (same problem like in case of miner)
    Inc(UnitReq[ut_Woodcutter], Byte(P.Stats.GetUnitQty(ut_Woodcutter) = 0)); // 1x Woodcutter
    Inc(UnitReq[ut_StoneCutter], Byte(P.Stats.GetUnitQty(ut_StoneCutter) = 0)); // 1x StoneCutter
    Inc(UnitReq[ut_Lamberjack], Byte(P.Stats.GetUnitQty(ut_Lamberjack) = 0)); // 1x Lamberjack
  end
  //Count overall unit requirement (excluding Barracks and ownerless houses)
  else
  begin
    for H := HOUSE_MIN to HOUSE_MAX do
      if (gRes.Houses[H].OwnerType <> ut_None) and (H <> ht_Barracks) then
        //Inc(UnitReq[gRes.Houses[H].OwnerType], P.Stats.GetHouseTotal(H));
        Inc(UnitReq[gRes.Houses[H].OwnerType], P.Stats.GetHouseQty(H));
    for UT := Low(UnitReq) to High(UnitReq) do
      Dec(UnitReq[UT], P.Stats.GetUnitQty(UT));
    //UnitReq[ut_Serf] := Round(fSetup.SerfsPerHouse * (P.Stats.GetHouseQty(ht_Any) + P.Stats.GetUnitQty(ut_Worker)/2));
    if (P.Stats.GetWareBalance(wt_Gold) < 15) AND (P.Stats.GetHouseQty(ht_Metallurgists) = 0) then
    begin
      UnitReq[ut_Serf] := 0;
      UnitReq[ut_Worker] := 0;
    end
    else
    begin
      UnitReq[ut_Serf] := RequiredServCount;
      UnitReq[ut_Worker] := fSetup.WorkerCount - P.Stats.GetUnitQty(ut_Worker);
    end;
  end;

  //Schools
  //Count overall schools count and exclude already training units from UnitReq
  SetLength(Schools, P.Stats.GetHouseQty(ht_School));
  K := 1;
  HS := TKMHouseSchool(P.FindHouse(ht_School, K));
  while HS <> nil do
  begin
    Schools[K-1] := HS;
    for I := 0 to HS.QueueLength - 1 do //Decrease requirement for each unit in training
      if HS.Queue[I] <> ut_None then
      begin
        if I = 0 then // if Queue is already active
          Dec(UnitReq[HS.Queue[I]]) //Can be negative and compensated by e.g. ReqRecruits
        else // else remove from Queue (it doesn't have to be actual ... when is city under attack we have to save gold)
          HS.RemUnitFromQueue(I);
      end;
    Inc(K);
    HS := TKMHouseSchool(P.FindHouse(ht_School, K));
  end;

  //Order the training. Keep up to 2 units in the queue so the school doesn't have to wait
  for I := 0 to High(Schools) do
  begin
    HS := Schools[I];
    if (HS <> nil) then
    begin
      //Order citizen training
      for K := Low(PRIORITY_TRAINING) to High(PRIORITY_TRAINING) do
      begin
        UT := PRIORITY_TRAINING[K];
        if UnitReq[UT] > 0 then
        begin
          Dec(UnitReq[UT], HS.AddUnitToQueue(UT, Min(UnitReq[UT], 2))); //So other schools don't order same unit
          if HS.QueueCount > 1 then //Don't need more UnitTypes yet
            Break;
        end;
      end;

      // While still haven't found a match...
      while (HS.QueueCount < 2) do
      begin
        // If we are low on Gold don't hire more ppl (next school will fail this too, so we can exit)
        if P.Stats.GetWareBalance(wt_Gold) < LACK_OF_GOLD then
          break;

        if not gGame.CheckTime(fSetup.RecruitDelay) then //Recruits can only be trained after this time
          Break
        else
          if not TryToTrain(HS, ut_Recruit, RecruitsNeeded) then
            Break; //There's no unit demand at all
      end;
    end;
  end;
end;


//Check if specific woodcutters are in Fell only mode
procedure TKMCityManagement.CheckMarketplaces();
var
  MarketCnt, RequiedCnt, AvaiableCnt, L: Word;
  RequiredWares, AvaiableWares: array of TWareType;
const
  SOLD_ORDER: array[0..27] of TWareType = (
    wt_Sausages,     wt_Wine,     wt_Fish,       wt_Bread,
    wt_Skin,         wt_Leather,  wt_Pig,
    wt_Trunk,        wt_Stone,    wt_Wood,
    wt_Shield,       wt_Axe,      wt_Pike,       wt_Bow,      wt_Armor,
    wt_MetalShield,  wt_Sword,    wt_Hallebard,  wt_Arbalet,  wt_MetalArmor,
    wt_Horse,        wt_Corn,     wt_Flour,
    wt_Steel,        wt_Gold,     wt_IronOre,    wt_Coal,     wt_GoldOre
  );

  procedure AddWare(aWare: TWareType; IsRequired: Boolean = True);
  begin
    if IsRequired then
    begin
      RequiredWares[RequiedCnt] := aWare;
      Inc(RequiedCnt,1);
    end
    else
    begin
      AvaiableWares[AvaiableCnt] := aWare;
      Inc(AvaiableCnt,1);
    end;
  end;

  procedure TryBuyItem(aResFrom, aResTo: TWareType);
  var
    M: Integer;
    Houses: TKMHousesCollection;
    H: TKMHouseMarket;
  begin
    Houses := gHands[fOwner].Houses;
    for M := 0 to Houses.Count - 1 do
      if (Houses[M].HouseType = ht_Marketplace)
        AND Houses[M].IsComplete
        AND not Houses[M].IsDestroyed then
      begin
        H := TKMHouseMarket(Houses[M]);
        if (TKMHouseMarket(H).ResOrder[0] <> 0)
          AND (H.ResTo = aResTo) then
          Exit;
      end;
    for M := 0 to Houses.Count - 1 do
      if (Houses[M].HouseType = ht_Marketplace)
        AND Houses[M].IsComplete
        AND not Houses[M].IsDestroyed then
      begin
        H := TKMHouseMarket(Houses[M]);
        if H.AllowedToTrade(aResFrom)
          AND H.AllowedToTrade(aResTo)
          AND (TKMHouseMarket(H).ResOrder[0] = 0) then
        begin
          if (H.ResFrom <> aResFrom) or (H.ResTo <> aResTo) then
          begin
            //H.ResOrder[0] := 0; //First we must cancel the current trade
            H.ResFrom := aResFrom;
            H.ResTo := aResTo;
            H.ResOrder[0] := 20; //Set the new trade
            break;
          end;
        end;
      end;
  end;
const
  MIN_GOLD_AMOUNT = LACK_OF_GOLD * 3;
  LACK_OF_STONE = 50;
  SELL_LIMIT = 100;
begin


  MarketCnt := gHands[fOwner].Stats.GetHouseQty(ht_Marketplace);
  if MarketCnt = 0 then
    Exit;

  //TryBuyItem(wt_Coal, wt_GoldOre);
  //{
  RequiedCnt := 0;
  SetLength(RequiredWares,3);

  // Gold
  if (gHands[fOwner].Stats.GetHouseQty(ht_Metallurgists) = 0)
     AND (gHands[fOwner].Stats.GetWareBalance(wt_Gold) <= LACK_OF_GOLD) then
     AddWare(wt_Gold);
  // Gold ore
  if ( gHands[fOwner].Stats.GetHouseQty(ht_GoldMine) < gHands[fOwner].Stats.GetHouseQty(ht_Metallurgists) )
    AND ( gHands[fOwner].Stats.GetWareBalance(wt_Gold) < MIN_GOLD_AMOUNT )
    AND ( gHands[fOwner].Stats.GetWareBalance(wt_GoldOre) < MIN_GOLD_AMOUNT ) then
    AddWare(wt_GoldOre);
  // Coal
  if ( gHands[fOwner].Stats.GetHouseQty(ht_CoalMine) < (gHands[fOwner].Stats.GetHouseQty(ht_Metallurgists) + gHands[fOwner].Stats.GetHouseQty(ht_IronSmithy) + gHands[fOwner].Stats.GetHouseQty(ht_WeaponSmithy) + gHands[fOwner].Stats.GetHouseQty(ht_ArmorSmithy)) )
    AND ( gHands[fOwner].Stats.GetWareBalance(wt_Coal) < MIN_GOLD_AMOUNT ) then
    AddWare(wt_Coal);
  // Stone
  if (gHands[fOwner].Stats.GetWareBalance(wt_Gold) < LACK_OF_STONE)
    AND (gHands[fOwner].Stats.GetHouseQty(ht_Quary) = 0) then
    AddWare(wt_Stone);

  if RequiedCnt = 0 then
    Exit;

  AvaiableCnt := 0;
  SetLength(AvaiableWares, RequiedCnt);
  for L := Low(SOLD_ORDER) to High(SOLD_ORDER) do
    if gHands[fOwner].Stats.GetWareBalance( SOLD_ORDER[L] ) > SELL_LIMIT then
    begin
      if L >= RequiedCnt then
         break;
      AddWare(SOLD_ORDER[L], False);
    end;

  for L := 0 to RequiedCnt do
   if L < AvaiableCnt then
     TryBuyItem(AvaiableWares[L], RequiredWares[L])
   else
     break;
end;


procedure TKMCityManagement.CheckStoreWares();
var
  I: Integer;
  S: TKMHouseStore;
begin
  //Iterate through all Stores and block certain wares to reduce serf usage
  for I := 0 to gHands[fOwner].Houses.Count - 1 do
    if (gHands[fOwner].Houses[I].HouseType = ht_Store)
      AND gHands[fOwner].Houses[I].IsComplete
      AND not gHands[fOwner].Houses[I].IsDestroyed then
    begin
      S := TKMHouseStore(gHands[fOwner].Houses[I]);

      // Materials
      S.NotAcceptFlag[wt_Trunk] := gHands[fOwner].Stats.GetWareBalance(wt_Trunk) > 100; // Trunk should not be blocked because of forest cleaning
      S.NotAcceptFlag[wt_Wood] := S.CheckResIn(wt_Wood) > gHands[fOwner].Stats.GetUnitQty(ut_Worker);
      //S.NotAcceptFlag[wt_Stone] := S.CheckResIn(wt_Stone) > gHands[fOwner].Stats.GetUnitQty(ut_Worker);
      S.NotAcceptFlag[wt_Stone] := Predictor.WareBalance[wt_Stone].Exhaustion > 40;
      S.NotAcceptFlag[wt_Gold] := S.CheckResIn(wt_Gold) > 200; // Everyone needs as much gold as possible

      // Food - don't store food when we have enought (it will cause trafic before storehouse)
      S.NotAcceptFlag[wt_Wine] := gHands[fOwner].Stats.GetWareBalance(wt_Wine)  > 100;
      S.NotAcceptFlag[wt_Sausages] := gHands[fOwner].Stats.GetWareBalance(wt_Sausages) > 100;
      S.NotAcceptFlag[wt_Bread] := gHands[fOwner].Stats.GetWareBalance(wt_Bread) > 100;
      S.NotAcceptFlag[wt_Fish] := gHands[fOwner].Stats.GetWareBalance(wt_Fish) > 100;

      // Others
      S.NotAcceptFlag[wt_GoldOre] := gHands[fOwner].Stats.GetHouseQty(ht_Metallurgists) > 0;
      S.NotAcceptFlag[wt_IronOre] := gHands[fOwner].Stats.GetHouseQty(ht_IronSmithy) > 0;
      S.NotAcceptFlag[wt_Coal] := gHands[fOwner].Stats.GetHouseQty(ht_Metallurgists) +
                                  gHands[fOwner].Stats.GetHouseQty(ht_IronSmithy) > 0;
      S.NotAcceptFlag[wt_Steel] := gHands[fOwner].Stats.GetHouseQty(ht_WeaponSmithy) +
                                   gHands[fOwner].Stats.GetHouseQty(ht_ArmorSmithy) > 0;
      S.NotAcceptFlag[wt_Corn] := gHands[fOwner].Stats.GetHouseQty(ht_Mill) +
                                  gHands[fOwner].Stats.GetHouseQty(ht_Swine) +
                                  gHands[fOwner].Stats.GetHouseQty(ht_Stables) > 0;
      S.NotAcceptFlag[wt_Leather] := gHands[fOwner].Stats.GetHouseQty(ht_ArmorWorkshop) > 0;
      S.NotAcceptFlag[wt_Flour] := gHands[fOwner].Stats.GetHouseQty(ht_Bakery) > 0;
      //Pigs and skin cannot be blocked since if swinefarm is full of one it stops working (blocks other)
      //S.NotAcceptFlag[wt_Skin] := gHands[fOwner].Stats.GetHouseQty(ht_Tannery) > 0;
      //S.NotAcceptFlag[wt_Pig] := gHands[fOwner].Stats.GetHouseQty(ht_Butchers) > 0;
    end;
end;


//Demolish any exhausted mines, they will be rebuilt if needed
procedure TKMCityManagement.CheckExhaustedHouses();
var
  I: Integer;
  Loc: TKMPoint;
begin
  //Wait until resource is depleted and output is empty
  for I := 0 to gHands[fOwner].Houses.Count - 1 do
  if not gHands[fOwner].Houses[I].IsDestroyed
    AND gHands[fOwner].Houses[I].ResourceDepletedMsgIssued
    AND (gHands[fOwner].Houses[I].CheckResOut(wt_All) = 0) then
  begin
    Loc := gHands[fOwner].Houses[I].Entrance;
    // Remove avoid building around coal mine
    if (gHands[fOwner].Houses[I].HouseType = ht_CoalMine) then
      gAIFields.Influences.RemAvoidBuilding(KMRect(Loc.X-3, Loc.Y-3, Loc.X+4, Loc.Y+2));
    // Mark house plan as exhausted
    Builder.Planner.MarkAsExhausted(gHands[fOwner].Houses[I].HouseType, Loc);
    gHands[fOwner].Houses[I].DemolishHouse(fOwner);
  end;
end;


// Allow auto repair procedure
procedure TKMCityManagement.CheckAutoRepair();
var I: Integer;
begin
  with gHands[fOwner] do
    if (HandType = hndComputer) then
      for I := 0 to Houses.Count - 1 do
        Houses[I].BuildingRepair := fSetup.AutoRepair;
end;


// Calculate weapons demand from combat AI requirements
function TKMCityManagement.WeaponsBalance(): TWarfareArr;
var
  EnemyEval, AllyEval: TKMArmyEval;
  WarriorsDemands: array[WARRIOR_EQUIPABLE_MIN..WARRIOR_EQUIPABLE_MAX] of Integer;

  procedure ComputeGroupDemands(aGT: TGroupType; aIronRatio: Single);
  const
    // It doesnt depends on future "optimalization" of parameters: against cav should be always good pikes etc. so this array doesnt have to be computed
    BEST_VERSUS_OPTION: array[TGroupType] of TGroupType = (gt_Melee, gt_Melee, gt_Ranged, gt_AntiHorse);
  var
    Wood, Iron: Boolean;
    I: Integer;
    EnemyAttack, EnemyDefence, MyAttack, MyDefence, AttackReq, DefenceReq: Single;
    UT: TUnitType;
    antiGT: TGroupType;
    UnitEval: TKMGroupEval;
  begin
    // Get best type of oponent and compute iron ratio
    antiGT := BEST_VERSUS_OPTION[aGT];
    Wood := False;
    Iron := False;
    for I := 1 to 3 do
    begin
      UT := AITroopTrainOrder[antiGT,I];
      if (UT <> ut_None) AND not gHands[fOwner].Locks.GetUnitBlocked(UT) then
        if (I = 1) then
          Iron := True
        else
          Wood := True;
    end;
    if Wood AND not Iron then
      aIronRatio := 0
    else if not Wood AND Iron then
      aIronRatio := 1
    else if not Wood AND not Iron then
      Exit;

    // Compute strength of specific enemy group
    with EnemyEval[aGT] do
    begin
      EnemyAttack := Attack + AttackHorse * Byte(antiGT = gt_Mounted);
      EnemyDefence := ifthen( (antiGT = gt_Ranged), DefenceProjectiles, Defence) * HitPoints;
    end;
    // Decrease strength by owner's existing units
    with AllyEval[antiGT] do
    begin
      EnemyAttack := EnemyAttack - ifthen( (aGT = gt_Ranged), DefenceProjectiles, Defence) * HitPoints;
      EnemyDefence := EnemyDefence - Attack - AttackHorse * Byte(aGT = gt_Mounted);
    end;
    // Compute unit requirements
    for I := 1 to 3 do
    begin
      UT := AITroopTrainOrder[antiGT,I];
      // Skip unit type if case that it is blocked
      if (UT = ut_None) OR gHands[fOwner].Locks.GetUnitBlocked(UT) then
        continue;
      // Calculate required count of specific unit type
      UnitEval := gAIFields.Eye.ArmyEvaluation.UnitEvaluation[UT, True];
      with UnitEval do
      begin
        MyAttack := Attack + AttackHorse * Byte(aGT = gt_Mounted);
        MyDefence := ifthen( (aGT = gt_Ranged), DefenceProjectiles, Defence) * HitPoints;
      end;
      AttackReq := EnemyAttack / MyDefence;
      DefenceReq := EnemyDefence / MyAttack;
      WarriorsDemands[UT] := WarriorsDemands[UT] + Max(0, Round(  Max(AttackReq, DefenceReq) * ifthen( (I = 1), aIronRatio, 1-aIronRatio )  )   );
      if (I = 2) then // In case that ut_AxeFighter is not blocked skip militia
        break;
    end;
  end;

  procedure CheckMinArmyReq();
  const
    DEFAULT_ARMY_REQUIREMENTS: array[WARRIOR_EQUIPABLE_MIN..WARRIOR_EQUIPABLE_MAX] of Word = (
      10, 10, 10, 30,//ut_Militia,      ut_AxeFighter,   ut_Swordsman,     ut_Bowman,
      30, 10, 10, 10,//ut_Arbaletman,   ut_Pikeman,      ut_Hallebardman,  ut_HorseScout,
      10//ut_Cavalry
    );
  var
    SoldierCnt: Integer;
    UT: TUnitType;
  begin
    SoldierCnt := 0;
    for UT := Low(WarriorsDemands) to High(WarriorsDemands) do
      SoldierCnt := SoldierCnt + WarriorsDemands[UT];
    if (SoldierCnt > 30) then
      Exit;

    for UT := Low(WarriorsDemands) to High(WarriorsDemands) do
      if not gHands[fOwner].Locks.GetUnitBlocked(UT) then
        WarriorsDemands[UT] := WarriorsDemands[UT] + DEFAULT_ARMY_REQUIREMENTS[UT];
  end;
//AITroopTrainOrder: array [TGroupType, 1..3] of TUnitType = (
//  (ut_Swordsman,    ut_AxeFighter, ut_Militia),
//  (ut_Hallebardman, ut_Pikeman,    ut_None),
//  (ut_Arbaletman,   ut_Bowman,     ut_None),
//  (ut_Cavalry,      ut_HorseScout, ut_None)
//);
//UnitGroups: array [WARRIOR_MIN..WARRIOR_MAX] of TGroupType = (
//  gt_Melee,gt_Melee,gt_Melee, //ut_Militia, ut_AxeFighter, ut_Swordsman
//  gt_Ranged,gt_Ranged,        //ut_Bowman, ut_Arbaletman
//  gt_AntiHorse,gt_AntiHorse,  //ut_Pikeman, ut_Hallebardman,
//  gt_Mounted,gt_Mounted,      //ut_HorseScout, ut_Cavalry,
//  gt_Melee,                   //ut_Barbarian
//  //TPR Army
//  gt_AntiHorse,        //ut_Peasant
//  gt_Ranged,           //ut_Slingshot
//  gt_Melee,            //ut_MetalBarbarian
//  gt_Mounted           //ut_Horseman
//);
//TroopCost: array [ut_Militia..ut_Cavalry, 1..4] of TWareType = (
//  (wt_Axe,          wt_None,        wt_None,  wt_None ), //Militia
//  (wt_Shield,       wt_Armor,       wt_Axe,   wt_None ), //Axefighter
//  (wt_MetalShield,  wt_MetalArmor,  wt_Sword, wt_None ), //Swordfighter
//  (wt_Armor,        wt_Bow,         wt_None,  wt_None ), //Bowman
//  (wt_MetalArmor,   wt_Arbalet,     wt_None,  wt_None ), //Crossbowman
//  (wt_Armor,        wt_Pike,        wt_None,  wt_None ), //Lance Carrier
//  (wt_MetalArmor,   wt_Hallebard,   wt_None,  wt_None ), //Pikeman
//  (wt_Shield,       wt_Armor,       wt_Axe,   wt_Horse), //Scout
//  (wt_MetalShield,  wt_MetalArmor,  wt_Sword, wt_Horse)  //Knight
//);
//WARRIOR_EQUIPABLE_MIN = ut_Militia;
//WARRIOR_EQUIPABLE_MAX = ut_Cavalry;
//  ut_Militia,      ut_AxeFighter,   ut_Swordsman,     ut_Bowman,
//  ut_Arbaletman,   ut_Pikeman,      ut_Hallebardman,  ut_HorseScout,
//  ut_Cavalry,      ut_Barbarian,

var
  I, SmithyCnt, WorkshopCnt, ArmorCnt: Integer;
  IronRatio: Single;
  WT: TWareType;
  GT: TGroupType;
  UT: TUnitType;
  Warfare: TWarfareArr;
begin
  ArmorCnt := gHands[fOwner].Stats.GetHouseQty(ht_ArmorSmithy) + gHands[fOwner].Stats.GetHouseQty(ht_ArmorWorkshop);
  SmithyCnt := gHands[fOwner].Stats.GetHouseQty(ht_WeaponSmithy);
  WorkshopCnt := gHands[fOwner].Stats.GetHouseQty(ht_WeaponWorkshop);
  if (ArmorCnt + SmithyCnt + WorkshopCnt = 0) then // Save time when nothing can be produced
    Exit;
  IronRatio := 0.5;
  if (SmithyCnt + WorkshopCnt > 0) then // This will avoid to divide by zero
    IronRatio := SmithyCnt / ((SmithyCnt + WorkshopCnt)*1.0);

  // Humans may spam archers -> AI will not produce long-range support because of balancing units in team
  // So it is better to calculate AllyEval just for Owner
  //AllyEval := gAIFields.Eye.ArmyEvaluation.GetAllianceStrength(aPlayer, at_Ally);
  AllyEval := gAIFields.Eye.ArmyEvaluation.Evaluation[fOwner];
  EnemyEval := gAIFields.Eye.ArmyEvaluation.AllianceEvaluation[fOwner, at_Enemy];

  // Compute requirements of warriors
  for UT := Low(WarriorsDemands) to High(WarriorsDemands) do
    WarriorsDemands[UT] := 0;
  for GT := Low(TGroupType) to High(TGroupType) do
    ComputeGroupDemands(GT, IronRatio);
  // Make sure that
  CheckMinArmyReq();

  // Get weapons reserves
  for WT := Low(Warfare) to High(Warfare) do
  begin
    Warfare[WT].Avaiable := gHands[fOwner].Stats.GetWareBalance(WT);
    Warfare[WT].Required := 0;
  end;

  // Get count of needed weapons
  for UT := WARRIOR_EQUIPABLE_MIN to WARRIOR_EQUIPABLE_MAX do
    for I := Low(TroopCost[UT]) to High(TroopCost[UT]) do
      if (TroopCost[UT,I] <> wt_None) then
      begin
        WT := TroopCost[UT,I];
        Warfare[WT].Required := Warfare[WT].Required + WarriorsDemands[UT];
      end
      else
        break;

  // Calculate fraction of demands
  for WT := Low(Warfare) to High(Warfare) do
    Warfare[WT].Fraction := Warfare[WT].Avaiable / Max(1,Warfare[WT].Required);

  Result := Warfare;

  for WT := Low(POMARR) to High(POMARR) do
    POMARR[WT] := Warfare[WT];
end;


// Distribute required weapons into exist houses (first will be produced the larger amount of wares)
procedure TKMCityManagement.OrderWeapons(aWarfare: TWarfareArr);
const
  WEAPONS_PER_A_UPDATE = 3;
  PRODUCTION_HOUSES = [ht_ArmorSmithy, ht_ArmorWorkshop, ht_WeaponSmithy, ht_WeaponWorkshop];
var
  I, K, MaxIdx, HouseCnt: Integer;
  MostRequired: Single;
  HT: THouseType;
  WT, MaxWT: TWareType;
  H: TKMHouse;
begin
  for HT in PRODUCTION_HOUSES do
  begin
    // Check house cnt
    HouseCnt := gHands[fOwner].Stats.GetHouseQty(HT);
    if (HouseCnt = 0) then
      continue;
    // Find produced ware which is the most required
    MostRequired := 1.0;
    MaxWT := wt_None;
    for I := 1 to 4 do
    begin
      WT := gRes.Houses[HT].ResOutput[I];
      if (WT <> wt_None) AND (aWarfare[WT].Fraction < MostRequired) then
      begin
        MostRequired := aWarfare[WT].Fraction;
        MaxWT := WT;
        MaxIdx := I;
      end;
    end;
    // Set order
    if (MaxWT <> wt_None) then
      for I := 0 to gHands[fOwner].Houses.Count - 1 do
        if not gHands[fOwner].Houses[I].IsDestroyed then
        begin
          H := gHands[fOwner].Houses[I];
          if (H.HouseType <> HT) then
            continue;
          for K := 1 to 4 do
            H.ResOrder[K] := 0;
          H.ResOrder[MaxIdx] := WEAPONS_PER_A_UPDATE; // With update each 1-2 minutes there is not need to calculate something more
        end;
  end;
end;


procedure TKMCityManagement.LogStatus(var aBalanceText: UnicodeString);
const
  WARFARE: array[WARFARE_MIN..WARFARE_MAX] of UnicodeString = ('Shield', 'MetalShield', 'Armor', 'MetalArmor', 'Axe', 'Sword', 'Pike', 'Hallebard', 'Bow', 'Arbalet', 'wt_Horse');
var
  WT: TWareType;
begin
  //aBalanceText := '';
  for WT := Low(POMARR) to High(POMARR) do
    aBalanceText := aBalanceText + WARFARE[WT] + ': ' + IntToStr(POMARR[WT].Avaiable) + '; ' + IntToStr(POMARR[WT].Required) + '; ' + FloatToStr(POMARR[WT].Fraction) +';|';
end;

end.
