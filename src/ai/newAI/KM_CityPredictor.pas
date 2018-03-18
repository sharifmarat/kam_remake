unit KM_CityPredictor;
{$I KaM_Remake.inc}
interface
uses
  Classes, Graphics, KromUtils, Math, SysUtils,
  KM_Defaults, KM_Points, KM_CommonClasses, KM_CommonTypes,
  KM_AISetup, KM_ResHouses, KM_ResWares;

type
  TWareBalance = record
    Production, ActualConsumption, FinalConsumption, Exhaustion, Fraction: Single;
  end;
  // Information about city (it can be replaced by player's stats but it doesnt allow prediction with different values)
  TCityStats = record
    CitizensCnt, WarriorsCnt, HousesCnt: Word;
    Citizens: array[CITIZEN_MIN..CITIZEN_MAX] of Word;
    Warriors: array[WARRIOR_MIN..WARRIOR_MAX] of Word;
    Houses: array[HOUSE_MIN..HOUSE_MAX] of Word;
  end;
  TWareBalanceArray = array[WARE_MIN..WARE_MAX] of TWareBalance;
  //TWarfareDemands = array[WARFARE_MIN..WARFARE_MAX] of Single;
  TRequiredHousesArray = array[HOUSE_MIN..HOUSE_MAX] of Integer;

  // City predictor (calculation of required houses based on prediction of resource flow)
  TKMCityPredictor = class
  private
    fOwner: TKMHandIndex;
    fCornDelay: Cardinal;
    fMaxSoldiersInMin: Single;
    fCityStats: TCityStats;
    fWareBalance: TWareBalanceArray;
    fSetup: TKMHandAISetup;

    procedure UpdateWareProduction(aWT: TKMWareType);
    procedure UpdateWareConsumption(aWT: TKMWareType; aInitialization: Boolean = False);
    procedure UpdateFoodConsumption(aInitialization: Boolean = False);
    procedure UpdateWareDerivation(aWT: TKMWareType; aInitialization: Boolean = False);
    procedure UpdateWareBalance(aInitialization: Boolean = False);

    procedure UpdateBasicHouses(aInitialization: Boolean = False);
    procedure UpdateCityStats();
  public
    RequiredHouses: TRequiredHousesArray;

    constructor Create(aPlayer: TKMHandIndex; aSetup: TKMHandAISetup);
    destructor Destroy(); override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property CityStats: TCityStats read fCityStats;
    property WareBalance: TWareBalanceArray read fWareBalance;

    procedure AfterMissionInit();
    procedure CityInitialization(aGoldMineCnt, aIronMineCnt, aFieldCnt, aBuildCnt: Integer);
    procedure UpdateState(aTick: Cardinal);
    procedure LogStatus(var aBalanceText: UnicodeString);
    procedure OwnerUpdate(aPlayer: TKMHandIndex);

  end;

const

  {
  TKMWareType:
  wt_None,
  wt_Trunk,   wt_Stone,   wt_Wood,        wt_IronOre,   wt_GoldOre,
  wt_Coal,    wt_Steel,   wt_Gold,        wt_Wine,      wt_Corn,
  wt_Bread,   wt_Flour,   wt_Leather,     wt_Sausages,  wt_Pig,
  wt_Skin,    wt_Shield,  wt_MetalShield, wt_Armor,     wt_MetalArmor,
  wt_Axe,     wt_Sword,   wt_Pike,        wt_Hallebard, wt_Bow,
  wt_Arbalet, wt_Horse,   wt_Fish,
  wt_All,     wt_Warfare, wt_Food
  }
  // Array of wares which are produced by specific houses
  PRODUCTION: array[WARE_MIN..WARE_MAX] of TKMHouseType = (
    ht_Woodcutters,    ht_Quary,         ht_Sawmill,        ht_IronMine,      ht_GoldMine,
    ht_CoalMine,       ht_IronSmithy,    ht_Metallurgists,  ht_Wineyard,      ht_Farm,
    ht_Bakery,         ht_Mill,          ht_Tannery,        ht_Butchers,      ht_Swine,
    ht_Swine,          ht_ArmorWorkshop, ht_ArmorSmithy,    ht_ArmorWorkshop, ht_ArmorSmithy,
    ht_WeaponWorkshop, ht_WeaponSmithy,  ht_WeaponWorkshop, ht_WeaponSmithy,  ht_WeaponWorkshop,
    ht_WeaponSmithy,   ht_Stables,       ht_FisherHut
  );
  // Possible transformation of wares: resource -> product
  CONSUMPTION: array[WARE_MIN..WARE_MAX] of array[0..3] of TKMWareType = (         // wt_Shield are ignored
    (wt_Wood, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None), (wt_Axe, wt_None, wt_None, wt_None), (wt_Steel, wt_None, wt_None, wt_None), (wt_Gold, wt_None, wt_None, wt_None),
    (wt_Steel, wt_Gold, wt_MetalArmor, wt_Sword), (wt_MetalArmor, wt_Sword, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None), (wt_Flour, wt_Pig, wt_Horse, wt_None),
    (wt_None, wt_None, wt_None, wt_None), (wt_Bread, wt_None, wt_None, wt_None), (wt_Armor, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None), (wt_Sausages, wt_None, wt_None, wt_None),
    (wt_Leather, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None),
    (wt_None, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None),
    (wt_None, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None), (wt_None, wt_None, wt_None, wt_None)
  );
  // Array of wares which are consumed in specific amount to achieve specific production (because of exceptions in ht_WeaponWorkshop / ht_Swine / ht_Stables etc.)
  CONSUMPTION_RATIO: array[wt_Trunk..wt_Skin] of array[0..3] of Single = (
    // 1 trunk = 2 Wood                2 wood = 1 axe/bow/spear        1 GoldOre = 2 Gold (+ 1 Coal)
    (0.5, 1, 1, 1),   (1, 1, 1, 1),   (2, 1, 1, 1),   (1, 1, 1, 1),   (0.5, 1, 1, 1),
    // 1 coal = 2 gold (+ 1 gold ore)                                    BEAST_COST = 4 (per a pig / horse)
    (0.5, 1, 1, 1),   (1, 1, 1, 1),   (1, 1, 1, 1),   (1, 1, 1, 1),   (1, 4, 4, 1),
    //               1 flour = 2 bread                                 1 pig = 3 sausages
    (1, 1, 1, 1),   (0.5, 1, 1, 1),   (1, 1, 1, 1),   (1, 1, 1, 1),   (0.333, 1, 1, 1),
    // 1 skin = 2 leather
    (0.5, 1, 1, 1)
  );
  CONSUMPTION_ORDER: array[0..27] of TKMWareType = ( // Basicaly TKMWareType but sorted by order: resource -> product
    wt_Stone,   wt_Trunk,    wt_Wood,
    wt_Corn,    wt_Flour,    wt_Bread,     wt_Wine,        wt_Fish,
    wt_Pig,     wt_Sausages, wt_Skin,      wt_Leather,     wt_Horse,
    wt_GoldOre, wt_IronOre,  wt_Coal,      wt_Gold,        wt_Steel,
    wt_Axe,     wt_Bow,      wt_Pike,      wt_Armor,       wt_Shield,
    wt_Sword,   wt_Arbalet,  wt_Hallebard, wt_MetalShield, wt_MetalArmor
  );
  CO_WEAPONS_MIN = 18;
  CO_WEAPONS_MAX = 27;
    CO_IRON_WEAPONS_MIN = 23;
    CO_IRON_WEAPONS_MAX = 27;
    CO_WOOD_WEAPONS_MIN = 22;
    CO_WOOD_WEAPONS_MAX = 18;
  CO_WARE_MIN = 0;
  CO_WARE_MAX = 17;
  {
  // Array of wares which are consumed by specific houses
  CONSUMPTION: array[WARE_MIN..WARE_MAX] of array[0..3] of TKMHouseType = (
    (ht_Sawmill, ht_None, ht_None, ht_None),   (ht_None, ht_None, ht_None, ht_None),   (ht_WeaponWorkshop, ht_ArmorWorkshop, ht_None, ht_None),   (ht_IronSmithy, ht_None, ht_None, ht_None),   (ht_Metallurgists, ht_None, ht_None, ht_None),
    (ht_Metallurgists, ht_IronSmithy, ht_ArmorSmithy, ht_WeaponSmithy),   (ht_ArmorSmithy, ht_WeaponSmithy, ht_None, ht_None),   (ht_None, ht_None, ht_None, ht_None),   (ht_None, ht_None, ht_None, ht_None),   (ht_Mill, ht_Swine, ht_Stables, ht_None),
    (ht_None, ht_None, ht_None, ht_None),   (ht_Bakery, ht_None, ht_None, ht_None),   (ht_ArmorWorkshop, ht_None, ht_None, ht_None),   (ht_None, ht_None, ht_None, ht_None),   (ht_Butchers, ht_None, ht_None, ht_None),
    (ht_Tannery, ht_None, ht_None, ht_None), (ht_None, ht_None, ht_None, ht_None), (ht_None, ht_None, ht_None, ht_None), (ht_None, ht_None, ht_None, ht_None), (ht_None, ht_None, ht_None, ht_None),
    (ht_None, ht_None, ht_None, ht_None), (ht_None, ht_None, ht_None, ht_None), (ht_None, ht_None, ht_None, ht_None), (ht_None, ht_None, ht_None, ht_None), (ht_None, ht_None, ht_None, ht_None),
    (ht_None, ht_None, ht_None, ht_None), (ht_None, ht_None, ht_None, ht_None), (ht_None, ht_None, ht_None, ht_None)
  );
  }

implementation
uses
  KM_HandsCollection, KM_Hand, KM_Resource, KM_Game, KM_CityPlanner;


{ TKMCityPredictor }
constructor TKMCityPredictor.Create(aPlayer: TKMHandIndex; aSetup: TKMHandAISetup);
begin
  inherited Create;
  fOwner := aPlayer;
  fSetup := aSetup;
end;

destructor TKMCityPredictor.Destroy();
begin
  inherited;
end;


procedure TKMCityPredictor.Save(SaveStream: TKMemoryStream);
var
  WT: TKMWareType;
begin
  SaveStream.WriteA('CityPredictor');
  SaveStream.Write(fOwner);
  SaveStream.Write(fCornDelay);
  SaveStream.Write(fMaxSoldiersInMin);

  // Requred houses should be saved because of public variable
  SaveStream.Write(RequiredHouses, SizeOf(TRequiredHousesArray));
  // fWareBalance must be saved because of FinalConsumption
  for WT := WARE_MIN to WARE_MAX do
    SaveStream.Write(fWareBalance[WT], SizeOf(TWareBalance));
  // Stats are updated each cycle and doesn't have to be saved
  //fCityStats: TCityStats;
end;


procedure TKMCityPredictor.Load(LoadStream: TKMemoryStream);
var
  WT: TKMWareType;
begin
  LoadStream.ReadAssert('CityPredictor');
  LoadStream.Read(fOwner);
  LoadStream.Read(fCornDelay);
  LoadStream.Read(fMaxSoldiersInMin);

  // Requred houses should be saved because of public variable
  LoadStream.Read(RequiredHouses, SizeOf(TRequiredHousesArray));
  // fWareBalance must be saved because of FinalConsumption
  for WT := WARE_MIN to WARE_MAX do
    LoadStream.Read(fWareBalance[WT], SizeOf(TWareBalance));
  // Stats are updated each cycle and doesn't have to be saved
  //fCityStats: TCityStats;
end;


procedure TKMCityPredictor.OwnerUpdate(aPlayer: TKMHandIndex);
begin
  fOwner := aPlayer;
end;


procedure TKMCityPredictor.AfterMissionInit;
begin

end;


// Update ware production
procedure TKMCityPredictor.UpdateWareProduction(aWT: TKMWareType);
begin
  fWareBalance[aWT].Production := fCityStats.Houses[ PRODUCTION[aWT] ] * ProductionRate[aWT];
end;


// Update ware consumption
procedure TKMCityPredictor.UpdateWareConsumption(aWT: TKMWareType; aInitialization: Boolean = False);
var
  I: Integer;
begin
  fWareBalance[aWT].ActualConsumption := 0;
  if aInitialization then
    fWareBalance[aWT].FinalConsumption := 0;
  for I := Low(CONSUMPTION[aWT]) to High(CONSUMPTION[aWT]) do
    if (CONSUMPTION[aWT,I] <> wt_None) then
    begin
      fWareBalance[aWT].ActualConsumption := fWareBalance[aWT].ActualConsumption + fWareBalance[ CONSUMPTION[aWT,I] ].ActualConsumption * CONSUMPTION_RATIO[aWT,I];
      if aInitialization then
        fWareBalance[aWT].FinalConsumption := fWareBalance[aWT].FinalConsumption + Max(fWareBalance[ CONSUMPTION[aWT,I] ].FinalConsumption, fWareBalance[ CONSUMPTION[aWT,I] ].ActualConsumption) * CONSUMPTION_RATIO[aWT,I];
    end
    else
      break;
end;


// Update food consumption
procedure TKMCityPredictor.UpdateFoodConsumption(aInitialization: Boolean = False);
const
  CITIZEN_FOOD_COEF = 0.05; // On average citizen needs to eat each 40min but takes 2 food to full status = 1/20 = 0.05
  SOLDIER_FOOD_COEF = 0.025; // On average soldier needs to eat each 40min and takes 1 food to full status = 1/40 = 0.025
  CITIZEN_RESERVE = 3.5 * 15; // Production of food suffer by ~15 min delay -> add some citizens (1 school = 3.5 citizen / min * estimated delay)
var
  Consumption: Single;
begin
  // Get consumption of city + army
  Consumption := ((fCityStats.CitizensCnt + CITIZEN_RESERVE) * CITIZEN_FOOD_COEF) + (fCityStats.WarriorsCnt * SOLDIER_FOOD_COEF);
  // Calculate consumption of leather armor / minute and pigs which are produced with this cycle
  // 2x armor = 2x leather = 1x skin = 1x pig = 3x sausages ... sausages = 3 / 2 * armor = 1.5 * armor
  fWareBalance[wt_Sausages].ActualConsumption := Min(Consumption, fWareBalance[wt_Armor].FinalConsumption * 1.5);
  // Split rest of consumtion into other houses
  Consumption := Max(0, Consumption - fWareBalance[wt_Sausages].ActualConsumption);
  fWareBalance[wt_Bread].ActualConsumption := Consumption * 0.7;
  fWareBalance[wt_Wine].ActualConsumption := Consumption * 0.3;
  fWareBalance[wt_Fish].ActualConsumption := 0;
  // Expected food consumption of the final city size (it helps with build order to secure food and weapons production ASAP)
  if aInitialization then
  begin
    fWareBalance[wt_Sausages].FinalConsumption := fWareBalance[wt_Sausages].ActualConsumption;
    fWareBalance[wt_Bread].FinalConsumption := fWareBalance[wt_Bread].ActualConsumption;
    fWareBalance[wt_Wine].FinalConsumption := fWareBalance[wt_Wine].ActualConsumption;
    fWareBalance[wt_Fish].FinalConsumption := fWareBalance[wt_Fish].ActualConsumption;
  end;
end;


// Update ware derivation - 2 views:
// 1. Exhaustion = estimation of time when will be ware depleted (determine which house should be built at first)
// 2. Fraction = fraction of required and avaiable houses
procedure TKMCityPredictor.UpdateWareDerivation(aWT: TKMWareType; aInitialization: Boolean = False);
var
  HouseReqCnt: Integer;
  HT: TKMHouseType;
begin
  HT := PRODUCTION[aWT];
  with fWareBalance[aWT] do
  begin
    // Calculate when will be ware depleted
    Exhaustion := 99;
    if (ActualConsumption - Production > 0) then
      Exhaustion := gHands[fOwner].Stats.GetWareBalance(aWT) / (ActualConsumption - Production);
    HouseReqCnt := Ceil(( Max(ActualConsumption, FinalConsumption) - Production) / Max(0.0001, ProductionRate[aWT]*1.0));
    Fraction := HouseReqCnt / Max(1.0,((fCityStats.Houses[HT] + HouseReqCnt)*1.0));
  end;
  RequiredHouses[HT] := HouseReqCnt;
end;


// Update ware balance
procedure TKMCityPredictor.UpdateWareBalance(aInitialization: Boolean = False);
const
  STONE_NEED_PER_A_WORKER = 0.6;
  WOOD_NEED_PER_A_WORKER = 0.35;
  GOLD_NEED_PER_A_SCHOOL = 3.5; // Amount of gold which requires school (in 1 minute)
var
  I: Integer;
begin
  // Update weapons
  for I := CO_WEAPONS_MAX downto CO_WEAPONS_MIN do
  begin
    UpdateWareProduction(CONSUMPTION_ORDER[I]);
    UpdateWareDerivation(CONSUMPTION_ORDER[I],aInitialization);
    // Final consumption for weapons is constant and actual is computed only for resource demands (coal mines etc.)
    fWareBalance[ CONSUMPTION_ORDER[I] ].ActualConsumption :=  fWareBalance[ CONSUMPTION_ORDER[I] ].FinalConsumption * (1.0 - fWareBalance[ CONSUMPTION_ORDER[I] ].Fraction);
  end;

  // Update "Normal" ware flow
  UpdateFoodConsumption(aInitialization); // Update food at once
  for I := CO_WARE_MAX downto CO_WARE_MIN do
  begin
    UpdateWareProduction(CONSUMPTION_ORDER[I]);
    // Exeptions
    case CONSUMPTION_ORDER[I] of
      // Food was updated at once
      wt_Wine, wt_Bread, wt_Sausages, wt_Fish:
        begin
        end;
      // Update Materials / Gold
      wt_Gold: fWareBalance[wt_Gold].ActualConsumption := Min(fMaxSoldiersInMin, (fCityStats.Houses[ht_School] + RequiredHouses[ht_School]) * GOLD_NEED_PER_A_SCHOOL);
      wt_Stone:
        begin
          fWareBalance[wt_Stone].ActualConsumption := Min(fCityStats.Citizens[ut_Worker]+10, fSetup.WorkerCount) * STONE_NEED_PER_A_WORKER;
          fWareBalance[wt_Stone].FinalConsumption := fSetup.WorkerCount * STONE_NEED_PER_A_WORKER;
        end;
      wt_Wood:
        begin
          UpdateWareConsumption(CONSUMPTION_ORDER[I], aInitialization);
          fWareBalance[wt_Wood].ActualConsumption := Max(fWareBalance[wt_Wood].ActualConsumption, fCityStats.Citizens[ut_Worker] * WOOD_NEED_PER_A_WORKER);
          fWareBalance[wt_Wood].FinalConsumption := Max(fWareBalance[wt_Wood].FinalConsumption, fSetup.WorkerCount * WOOD_NEED_PER_A_WORKER);
        end;
      // Other cases
      else UpdateWareConsumption(CONSUMPTION_ORDER[I], aInitialization);
    end;
    UpdateWareDerivation(CONSUMPTION_ORDER[I],aInitialization);
  end;
end;


// Get players stats and store them into local variable (to be able to edit them later)
procedure TKMCityPredictor.UpdateCityStats();
var
  UT: TKMUnitType;
  HT: TKMHouseType;
  Planner: TKMCityPlanner;
begin
  Planner := gHands[fOwner].AI.CityManagement.Builder.Planner;
  with fCityStats do
  begin
    CitizensCnt := 0;
    for UT := Low(Citizens) to High(Citizens) do
    begin
      Citizens[UT] := gHands[fOwner].Stats.GetUnitQty(UT);
      CitizensCnt := CitizensCnt + Citizens[UT];
    end;
    WarriorsCnt := 0;
    for UT := Low(Warriors) to High(Warriors) do
    begin
      Warriors[UT] := gHands[fOwner].Stats.GetUnitQty(UT);
      WarriorsCnt := WarriorsCnt + Warriors[UT];
    end;
    HousesCnt := 0;
    for HT := Low(Houses) to High(Houses) do
    begin
      //Houses[HT] := gHands[fOwner].Stats.GetHouseTotal(HT); // Does not consider planned houses
      Houses[HT] := Planner.PlannedHouses[HT].Count;
      HousesCnt := HousesCnt + Houses[HT];
    end;
    if GA_PLANNER then  // Short info for Genetic algorithm planner
    begin
      Citizens[ut_Worker] := fSetup.WorkerCount;
      Citizens[ut_Serf] := HousesCnt*2;
      CitizensCnt := Citizens[ut_Serf] + Citizens[ut_Worker];
      WarriorsCnt := 100;
    end;
  end;
end;


// Basic house requirements
procedure TKMCityPredictor.UpdateBasicHouses(aInitialization: Boolean = False);
const
  INN_TIME_LIMIT = 9000; // ~ 15 minutes from start
  SCHOOL_PRODUCTION = 3; // Amount of gold which requires school (in 1 minute) - in ideal case it requires only 3.5 in real there is not sometimes gold so it must be lower
  FIRST_MARKETPLACE = 10 * 60 * 80;
  SECOND_MARKETPLACE = 10 * 60 * 100;
begin
  // 1 Storehouse
  RequiredHouses[ht_Store] := 1 - fCityStats.Houses[ht_Store];
  // 1 Barracks (build only when we have or produce axe / armors)
  RequiredHouses[ht_Barracks] := Byte(aInitialization OR (gHands[fOwner].Stats.GetWareBalance(wt_Warfare) > 0)) - fCityStats.Houses[ht_Barracks];
  // Schools (at least 1 + WarriorsPerMinute criterium)
  RequiredHouses[ht_School] := Max( 0,  Max(1, Byte(  (fCityStats.Houses[ht_Barracks] > 0) OR aInitialization ) * (Round(fMaxSoldiersInMin / SCHOOL_PRODUCTION))) - fCityStats.Houses[ht_School]  );
  // Inn (at least 1 after INN_TIME_LIMIT + CitizensCnt criterium)
  RequiredHouses[ht_Inn] := Max(0, Ceil(  Byte( (gGame.GameTickCount > INN_TIME_LIMIT) OR aInitialization ) * fCityStats.CitizensCnt / 80  ) - fCityStats.Houses[ht_Inn]);
  // Marketplace - 1. after FIRST_MARKETPLACE; 2. after SECOND_MARKETPLACE
  RequiredHouses[ht_MarketPlace] := Byte( aInitialization OR (gGame.GameTickCount > FIRST_MARKETPLACE) ) + Byte( aInitialization OR (gGame.GameTickCount > SECOND_MARKETPLACE) ) - fCityStats.Houses[ht_Marketplace];
end;


//
procedure TKMCityPredictor.CityInitialization(aGoldMineCnt, aIronMineCnt, aFieldCnt, aBuildCnt: Integer);

  procedure AddCitizens(aUT: TKMUnitType; aCnt: Word; aOverride: Boolean = False);
  begin
    fCityStats.CitizensCnt := fCityStats.CitizensCnt - fCityStats.Citizens[aUT] * Byte(aOverride) + aCnt;
    fCityStats.Citizens[aUT] := fCityStats.Citizens[aUT] * Byte(not aOverride) + aCnt;
  end;
  procedure AddWarriors(aUT: TKMUnitType; aCnt: Word; aOverride: Boolean = False);
  begin
    fCityStats.WarriorsCnt := fCityStats.WarriorsCnt - fCityStats.Warriors[aUT] * Byte(aOverride) + aCnt;
    fCityStats.Warriors[aUT] := fCityStats.Warriors[aUT] * Byte(not aOverride) + aCnt;
  end;
  procedure AddHouses(aHT: TKMHouseType; aCnt: Word; aOverride: Boolean = False);
  begin
    fCityStats.HousesCnt := fCityStats.HousesCnt - fCityStats.Houses[aHT] * Byte(aOverride) + aCnt;
    fCityStats.Houses[aHT] := fCityStats.Houses[aHT] * Byte(not aOverride) + aCnt;
  end;

const
  IRON_WARFARE: set of TKMWareType = [wt_MetalShield, wt_MetalArmor, wt_Sword, wt_Hallebard, wt_Arbalet];
  STANDARD_WARFARE: array[0..3] of TKMWareType = (wt_Axe, wt_Pike, wt_Bow, wt_Shield);
var
  I: Integer;
  MaxIronWeapProd, MaxWoodWeapProd: Single;
  WT: TKMWareType;
begin
  // Estimation of final weapons production (productions are independence - in builder will be higher priority given to iron weapons)
  // Iron weapons
  MaxIronWeapProd := aIronMineCnt * ProductionRate[wt_IronOre] / 2.0; // / 2.0 for iron weapon and armor
  for WT in IRON_WARFARE do
    fWareBalance[WT].FinalConsumption := MaxIronWeapProd;
  // Wood weapons (depends on avaiable space)
  MaxWoodWeapProd := Round((aBuildCnt + 1500) / 1500) * ProductionRate[wt_Axe];
  for I := Low(STANDARD_WARFARE) to High(STANDARD_WARFARE) do
    fWareBalance[ STANDARD_WARFARE[I] ].FinalConsumption := MaxWoodWeapProd;
  fWareBalance[wt_Armor].FinalConsumption := MaxWoodWeapProd;
  fWareBalance[wt_Shield].FinalConsumption := MaxWoodWeapProd / 5;
  // Soldiers / min (only expected not final value)
  fMaxSoldiersInMin := MaxWoodWeapProd + MaxIronWeapProd;
  // Maybe there is no need to keep variable fMaxSoldiersInMin but I am afraid what scripters may do with fSetup
  fSetup.EquipRateIron := Round(600 / Max(0.01, MaxIronWeapProd));
  fSetup.EquipRateLeather := Round(600 / Max(0.01, MaxWoodWeapProd));

  // Predict final city stats (by potential size of city)
  fCityStats.CitizensCnt := Round(  Max(0,Min(aBuildCnt,4000)-1500)*0.052+70  ); // Min cnt of citizens is 70 and max 200
  fCityStats.WarriorsCnt := Round(  Max(0,Min(aBuildCnt,4000)-1500)*0.042+50  ); // Min cnt of soldiers is 50 and max 150
  UpdateWareBalance(True);

  // Corn delay to stop build swine or mill before farm (another way is to check if we have corn but it does not work well)
  fCornDelay := Byte(gHands[fOwner].Stats.GetWareBalance(wt_Corn) > 0);
end;


procedure TKMCityPredictor.UpdateState(aTick: Cardinal);
const
  WEAP_WORKSHOP_DELAY = 40 * 60 * 10;
var
  HT: TKMHouseType;
begin
  for HT := Low(RequiredHouses) to High(RequiredHouses) do
    RequiredHouses[HT] := 0;

  UpdateCityStats();
  UpdateBasicHouses(GA_PLANNER);
  UpdateWareBalance();

  if (fCornDelay = 0) AND (fCityStats.Houses[ht_Farm] > 0) then
  begin
    fCornDelay := gGame.GameTickCount + 6000; // 10 ticks = 1 sec (farm requires 10 min to produce corn)
  end;
  if not GA_PLANNER then
  begin
    // Corn delay (should not wait till is first corn produced because it have huge impact)
    if (gGame.GameTickCount < fCornDelay) then // Consider corn delay (~10 minutes) -> time saved by construction
    begin
      RequiredHouses[ht_Mill] := 0;
      RequiredHouses[ht_Bakery] := 0;
      RequiredHouses[ht_Swine] := 0;
      RequiredHouses[ht_Tannery] := 0;
      RequiredHouses[ht_ArmorWorkshop] := 0;
    end;
    // Pig delay
    //RequiredHouses[ht_Butchers] := RequiredHouses[ht_Butchers] * Byte(gHands[fOwner].Stats.GetWareBalance(wt_Pig) > 0);
    RequiredHouses[ht_Butchers] := RequiredHouses[ht_Butchers] * Byte(fCityStats.Houses[ht_Swine] > 0);
    //if (gHands[fOwner].Stats.GetWareBalance(wt_Skin) = 0) then
    //begin
    //  RequiredHouses[ht_Tannery] := 0;
    //  RequiredHouses[ht_ArmorWorkshop] := 0;
    //end;

    if (gGame.GameTickCount < 22000) then
      RequiredHouses[ht_Wineyard] := 0;

    RequiredHouses[ht_WeaponWorkshop] := RequiredHouses[ht_WeaponWorkshop] * Byte( (RequiredHouses[ht_Tannery] > 0) OR (WEAP_WORKSHOP_DELAY < aTick) );

    // Loghical house requirements (delay takes too long so it is not used)
    {
    RequiredHouses[ht_Swine] := RequiredHouses[ht_Swine] * Byte(gHands[fOwner].Stats.GetWareBalance(wt_Corn) > 0);
    RequiredHouses[ht_Butchers] := RequiredHouses[ht_Butchers] * Byte(gHands[fOwner].Stats.GetWareBalance(wt_Pig) > 0);
    RequiredHouses[ht_Tannery] := RequiredHouses[ht_Tannery] * Byte(gHands[fOwner].Stats.GetWareBalance(wt_Leather) > 0);
    RequiredHouses[ht_ArmorWorkshop] := RequiredHouses[ht_ArmorWorkshop] * Byte(gHands[fOwner].Stats.GetWareBalance(wt_Skin) > 0);
    RequiredHouses[ht_Mill] := RequiredHouses[ht_Mill] * Byte(gHands[fOwner].Stats.GetWareBalance(wt_Flour) > 0);
    RequiredHouses[ht_Bakery] := RequiredHouses[ht_Bakery] * Byte(gHands[fOwner].Stats.GetWareBalance(wt_Corn) > 0);
    //}
    // Iron production (it will give time to build more mines)
    {
    RequiredHouses[ht_IronSmithy] := RequiredHouses[ht_IronSmithy] * Byte(gHands[fOwner].Stats.GetWareBalance(wt_IronOre) > 0);
    RequiredHouses[ht_WeaponSmithy] := RequiredHouses[ht_WeaponSmithy] * Byte(gHands[fOwner].Stats.GetWareBalance(wt_Steel) > 0);
    RequiredHouses[ht_ArmorSmithy] := RequiredHouses[ht_ArmorSmithy] * Byte(gHands[fOwner].Stats.GetWareBalance(wt_Steel) > 0);
    //}
  end;
end;


procedure TKMCityPredictor.LogStatus(var aBalanceText: UnicodeString);
const
  COLOR_WHITE = '[$FFFFFF]';
  COLOR_RED = '[$0000FF]';
  COLOR_YELLOW = '[$00FFFF]';
  COLOR_GREEN = '[$00FF00]';
  WARE_TO_STRING: array[WARE_MIN..WARE_MAX] of UnicodeString = (
    'Trunk',   'Stone',   'Wood',        'IronOre',   'GoldOre',
    'Coal',    'Steel',   'Gold',        'Wine',      'Corn',
    'Bread',   'Flour',   'Leather',     'Sausages',  'Pig',
    'Skin',    'Shield',  'MetalShield', 'Armor',     'MetalArmor',
    'Axe',     'Sword',   'Pike',        'Hallebard', 'Bow',
    'Arbalet', 'Horse',   'Fish'
  );
  //HOUSE_TO_STRING: array[HOUSE_MIN..HOUSE_MAX] of UnicodeString = (
  //  'ArmorSmithy',     'ArmorWorkshop',   'Bakery',        'Barracks',      'Butchers',
  //  'CoalMine',        'Farm',            'FisherHut',     'GoldMine',      'Inn',
  //  'IronMine',        'IronSmithy',      'Marketplace',   'Metallurgists', 'Mill',
  //  'Quary',           'Sawmill',         'School',        'SiegeWorkshop', 'Stables',
  //  'Store',           'Swine',           'Tannery',       'TownHall',      'WatchTower',
  //  'WeaponSmithy',    'WeaponWorkshop',  'Wineyard',      'Woodcutters'
  //);

  procedure AddWare(aWT: TKMWareType; aSpecificText: UnicodeString);
  var
    HouseCntColor, ProductionColor, ActualConsumptionColor, FinalConsumptionColor, FractionColor, ExhaustionColor: UnicodeString;
    Cnt: Integer;
  begin
    Cnt := RequiredHouses[ PRODUCTION[aWT] ];
    HouseCntColor := COLOR_WHITE;
    if (Cnt > 0) then
      HouseCntColor := COLOR_RED;
    with fWareBalance[aWT] do
    begin
      ProductionColor := COLOR_YELLOW;
      ActualConsumptionColor := COLOR_YELLOW;
      FinalConsumptionColor := COLOR_YELLOW;
      FractionColor := COLOR_YELLOW;
      ExhaustionColor := COLOR_RED;
      if (Production > 0) then         ProductionColor := COLOR_GREEN;
      if (ActualConsumption > 0) then  ActualConsumptionColor := COLOR_RED;
      if (FinalConsumption > 0) then   FinalConsumptionColor := COLOR_RED;
      if (Fraction <= 0.1) then        FractionColor := COLOR_GREEN
      else                             FractionColor := COLOR_RED;
      if (Exhaustion > 10) then        ExhaustionColor := COLOR_GREEN
      else if (Exhaustion > 1) then    ExhaustionColor := COLOR_YELLOW;
      aBalanceText := aBalanceText + Format(HouseCntColor+'%Dx '+COLOR_WHITE, [Cnt]) //
                        + aSpecificText + Format(':   ('
                        +ProductionColor+'%.2f'+COLOR_WHITE+'; '
                        +ActualConsumptionColor+'%.2f'+COLOR_WHITE+'; '
                        +FinalConsumptionColor+'%.2f'+COLOR_WHITE+'; '
                        +FractionColor+'%.2f '+COLOR_WHITE+'; '
                        +ExhaustionColor+'%.2f'+COLOR_WHITE+')|', [Production, ActualConsumption, FinalConsumption, Fraction, Exhaustion]);
    end;
  end;
var
  I: Integer;
begin
  aBalanceText := aBalanceText + 'Ware balance|Required houses (ware type ->   production;   actual consumption;   final consumption;   fraction;   exhaustion):|';
  for I := CO_WARE_MIN to CO_WARE_MAX do
    AddWare(CONSUMPTION_ORDER[I], WARE_TO_STRING[ CONSUMPTION_ORDER[I] ]);
  AddWare(wt_Armor, 'Armors');
  AddWare(wt_Axe, 'Weapons');
  AddWare(wt_MetalArmor, 'Iron Armors');
  AddWare(wt_Sword, 'Iron Weapons');
end;




end.
