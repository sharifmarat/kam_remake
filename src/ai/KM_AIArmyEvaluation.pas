unit KM_AIArmyEvaluation;
{$I KaM_Remake.inc}
interface
uses
  KM_Defaults, KM_CommonClasses;
//Classes, Graphics, KromUtils, Math, SysUtils,
//KM_Defaults, KM_Points, KM_CommonClasses, KM_CommonTypes, KM_FloodFill,

type
  TKMArmyDemandArray = array[TGroupType] of Integer;

  TKMGroupEval = record
    HitPoints, Attack, AttackHorse, Defence, DefenceProjectiles: Single;
  end;

  TKMArmyEval = array[TGroupType] of TKMGroupEval;


  //This class evaluate self army relatively enemy armies
  TKMArmyEvaluation = class
  private
    fEvals: array [0 .. MAX_HANDS - 1] of TKMArmyEval; //Results of evaluation

    procedure EvaluatePower(aPlayer: TKMHandIndex; aConsiderHitChance: Boolean = False);
    function GetUnitEvaluation(aUT: TUnitType; aConsiderHitChance: Boolean = False): TKMGroupEval;
    function GetEvaluation(aPlayer: TKMHandIndex): TKMArmyEval;
    function GetAllianceStrength(aPlayer: TKMHandIndex; aAlliance: TAllianceType): TKMArmyEval;
    function CompareAllianceStrength(aPlayer: TKMHandIndex): TKMArmyEval;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    property UnitEvaluation[aUT: TUnitType; aConsiderHitChance: Boolean]: TKMGroupEval read GetUnitEvaluation;
    property Evaluation[aPlayer: TKMHandIndex]: TKMArmyEval read GetEvaluation;
    property AllianceEvaluation[aPlayer: TKMHandIndex; aAlliance: TAllianceType]: TKMArmyEval read GetAllianceStrength;

    procedure UpdateState(aTick: Cardinal);
  end;

const
  HIT_CHANCE_MODIFIER = 0.5;

implementation
uses
  Math,
  KM_Hand, KM_HandsCollection, KM_HandStats,
  KM_Resource, KM_ResUnits;


{ TKMArmyEvaluation }
constructor TKMArmyEvaluation.Create();
begin
  inherited Create;
end;


destructor TKMArmyEvaluation.Destroy;
begin

  inherited;
end;


procedure TKMArmyEvaluation.Save(SaveStream: TKMemoryStream);
var
  I: Integer;
  GT: TGroupType;
begin
  SaveStream.WriteA('ArmyEvaluation');
  for I := 0 to MAX_HANDS - 1 do
    for GT := Low(TGroupType) to High(TGroupType) do
      SaveStream.Write(fEvals[I,GT], SizeOf(TKMGroupEval));
end;


procedure TKMArmyEvaluation.Load(LoadStream: TKMemoryStream);
var
  I: Integer;
  GT: TGroupType;
begin
  LoadStream.ReadAssert('ArmyEvaluation');
  for I := 0 to MAX_HANDS - 1 do
    for GT := Low(TGroupType) to High(TGroupType) do
      LoadStream.Read(fEvals[I,GT], SizeOf(TKMGroupEval));
end;


function TKMArmyEvaluation.GetUnitEvaluation(aUT: TUnitType; aConsiderHitChance: Boolean = False): TKMGroupEval;
var
  US: TKMUnitSpec;
begin
  // Fill array with reference values
  US := gRes.Units[aUT];
  with Result do
  begin
    Hitpoints :=          US.Hitpoints;
    Attack :=             US.Attack;
    AttackHorse :=        US.AttackHorse;
    Defence :=            US.Defence;
    DefenceProjectiles := US.GetDefenceVsProjectiles(False);
    if aConsiderHitChance AND (UnitGroups[aUT] = gt_Ranged) then
      Attack := Attack * HIT_CHANCE_MODIFIER;
  end;
end;


function TKMArmyEvaluation.GetEvaluation(aPlayer: TKMHandIndex): TKMArmyEval;
begin
  Result := fEvals[aPlayer];
end;


function TKMArmyEvaluation.GetAllianceStrength(aPlayer: TKMHandIndex; aAlliance: TAllianceType): TKMArmyEval;
var
  PL: Integer;
  GT: TGroupType;
begin
  for GT := Low(TGroupType) to High(TGroupType) do
    with Result[GT] do
    begin
      Hitpoints := 0;
      Attack := 0;
      AttackHorse := 0;
      Defence := 0;
      DefenceProjectiles := 0;
    end;
  for PL := 0 to gHands.Count - 1 do
    if gHands[PL].Enabled AND (gHands[aPlayer].Alliances[PL] = aAlliance) then
      for GT := Low(TGroupType) to High(TGroupType) do
        with Result[GT] do
        begin
          Hitpoints := Hitpoints                   + fEvals[PL,GT].Hitpoints;
          Attack := Attack                         + fEvals[PL,GT].Attack;
          AttackHorse := AttackHorse               + fEvals[PL,GT].AttackHorse;
          Defence := Defence                       + fEvals[PL,GT].Defence;
          DefenceProjectiles := DefenceProjectiles + fEvals[PL,GT].DefenceProjectiles;
        end;
end;


function TKMArmyEvaluation.CompareAllianceStrength(aPlayer: TKMHandIndex): TKMArmyEval;
var
  GT: TGroupType;
  AllyEval, EnemyEval: TKMArmyEval;
begin
  AllyEval := GetAllianceStrength(aPlayer, at_Ally);
  EnemyEval := GetAllianceStrength(aPlayer, at_Enemy);
  for GT := Low(TGroupType) to High(TGroupType) do
    with Result[GT] do
    begin
      Hitpoints := AllyEval[GT].Hitpoints - EnemyEval[GT].Hitpoints;
      Attack := AllyEval[GT].Attack - EnemyEval[GT].Attack;
      AttackHorse := AllyEval[GT].AttackHorse - EnemyEval[GT].AttackHorse;
      Defence := AllyEval[GT].Defence - EnemyEval[GT].Defence;
      DefenceProjectiles := AllyEval[GT].DefenceProjectiles - EnemyEval[GT].DefenceProjectiles;
    end;
end;


// Actualize power of specific player
//
// Equation of combat:
//
//   Close combat
//                    Attack (+ AttackHorse) (+ DirModifier)   -> DirModifier is not considered in ArmyEvaluation
//     Probability = ---------------------------------------
//                                 Defence
//
//   Ranged units
//     HitProbability = 1 - Distance;  Distance in <0,1>       -> Hit probability is considered as a decreasing of attack by HIT_CHANCE_MODIFIER
//                         Attack
//     Probability = --------------------
//                    DefenceProjectiles
//
// Probability > random number => decrease hitpoint; 0 hitpoints = unit is dead
procedure TKMArmyEvaluation.EvaluatePower(aPlayer: TKMHandIndex; aConsiderHitChance: Boolean = False);
var
  Stats: TKMHandStats;
  Qty: Integer;
  US: TKMUnitSpec;
  UT: TUnitType;
  GT: TGroupType;
begin
  Stats := gHands[aPlayer].Stats;

  // Clear array
  for GT := Low(TGroupType) to High(TGroupType)  do
    with fEvals[aPlayer,GT] do
    begin
      Hitpoints := 0;
      Attack := 0;
      AttackHorse := 0;
      Defence := 0;
      DefenceProjectiles := 0;
    end;
  // Fill array with reference values
  for UT := WARRIOR_MIN to WARRIOR_MAX do
  begin
    Qty := Stats.GetUnitQty(UT);
    US := gRes.Units[UT];
    GT := UnitGroups[UT];
    with fEvals[aPlayer,GT] do
    begin
      Hitpoints := Hitpoints + Qty * US.HitPoints;
      Attack := Attack + Qty * US.Attack;
      AttackHorse := AttackHorse + Qty * US.AttackHorse;
      Defence := Defence + Qty * US.Defence;
      DefenceProjectiles := DefenceProjectiles + Qty * US.GetDefenceVsProjectiles(False); // True = IsBolt -> calculation without bolts
    end;
  end;
  if aConsiderHitChance then
    with fEvals[aPlayer,gt_Ranged] do
      Attack := Attack * HIT_CHANCE_MODIFIER;
end;


procedure TKMArmyEvaluation.UpdateState(aTick: Cardinal);
var
  PL: TKMHandIndex;
begin
  PL := aTick mod gHands.Count;
  if gHands[PL].Enabled then
    EvaluatePower(PL, True);
end;


end.
