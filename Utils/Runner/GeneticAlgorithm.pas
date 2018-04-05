unit GeneticAlgorithm;

interface
uses
  Classes, SysUtils, Math;

type
  TIntegerArray = array of Integer;

  TGAIndividual = class
  private
    fGenes: array of Single;
    fFitness: Single;

    function GetCount(): Integer;
    procedure SetCount(aCount: Integer);
    function GetGene(Idx: Integer): Single;
    procedure SetGene(Idx: Integer; Value: Single);
  public
    constructor Create(aCount: Integer);
    destructor Destroy(); override;

    property Count: Integer read GetCount write SetCount;
    property Gene[Idx: Integer]: Single read GetGene write SetGene;
    property Fitness: Single read fFitness write fFitness;
  end;

  TGAPopulation = class
  private
    fIndividuals: array of TGAIndividual;
    function GetIndividual(Idx: Integer): TGAIndividual;
    procedure SetIndividual(Idx: Integer; aIdv: TGAIndividual);
    function GetCount(): Integer;
  public
    constructor Create(aPopulationCount, aGeneCount: Integer; aInitialise: Boolean);
    destructor Destroy(); override;

    property Individual[Idx: Integer]: TGAIndividual read GetIndividual write SetIndividual; default;
    property Count: Integer read GetCount;

    function GetFittest(var aIdx: Integer): TGAIndividual; overload; // For debug only
    function GetFittest(aOnlySomeIdx: TIntegerArray = nil): TGAIndividual; overload;
  end;

  TGAAlgorithm = class
  private
    fMutation, fCrossoverCoef: Single;
    fIndividualsInTournamentCnt: Word;
    function TournamentSelection(var aPopulation: TGAPopulation): TGAIndividual;
    function Crossover(aIdv1, aIdv2: TGAIndividual): TGAIndividual;
    procedure Mutate(aIdv: TGAIndividual);
    procedure SetMutation(aMutation: Single);
  public
    constructor Create();
    destructor Destroy(); override;

    property Mutation: Single read fMutation write SetMutation;
    property CrossoverCoef: Single read fCrossoverCoef write fCrossoverCoef;
    property IndividualsInTournament: Word read fIndividualsInTournamentCnt write fIndividualsInTournamentCnt;

    procedure EvolvePopulation(var aOldPopulation, aNewPopulation: TGAPopulation);
  end;

  {
  TGAFitnessCalc = class
  private
    fSolution: array of Single;

    procedure SetSolution(Idx: Integer; aSolution: Single);
  public
    constructor Create();
    destructor Destroy(); override;

    property Solution[Idx: Integer]: Single write SetSolution;

    function GetFitness(aIdv: TGAIndividual): Integer;
    function GetMaxFitness(): Integer;
  end;
  //}

implementation





{ TGAAlgorithm }
constructor TGAAlgorithm.Create();
begin
  fMutation := 1;
end;


destructor TGAAlgorithm.Destroy();
begin
  inherited;
end;


procedure TGAAlgorithm.EvolvePopulation(var aOldPopulation, aNewPopulation: TGAPopulation);
var
  I,K, CrossoverCnt, Genes, Populations: Integer;
  Idv1, Idv2: TGAIndividual;
begin
  Populations := aOldPopulation.Count;
  Genes := aOldPopulation[0].Count;

  // Save the best individual
  Idv1 := aOldPopulation.GetFittest(I);
  Idv2 := TGAIndividual.Create(Genes);
  for I := 0 to Genes - 1 do // Old generation will be destroyed so copy genes
    Idv2.Gene[I] := Idv1.Gene[I];
  aNewPopulation[0] := Idv2;

  // Evolve population
  CrossoverCnt := Min(  Populations - 1, Round( (Populations - 1) * Max(0.01, fCrossoverCoef) )  );
  for I := 1 to CrossoverCnt do // Start from 1, dont override the best individual
  begin
    Idv1 := TournamentSelection(aOldPopulation);
    Idv2 := Idv1;
    K := 0;
    while (Idv2 = Idv1) AND (K < 20) do
    begin
      K := K + 1;
      Idv2 := TournamentSelection(aOldPopulation);
    end;
    aNewPopulation[I] := Crossover(Idv1, Idv2);
    Mutate(aNewPopulation[I]);
  end;

  // New blood
  for I := CrossoverCnt + 1 to Populations - 1 do
  begin
    aNewPopulation[I] := TGAIndividual.Create(Genes);
    for K := 0 to Genes - 1 do
      aNewPopulation[I].Gene[K] := Random();
  end;
end;


function TGAAlgorithm.TournamentSelection(var aPopulation: TGAPopulation): TGAIndividual;
var
  I, TournamentCount: Integer;
  TournamentIdx: TIntegerArray;
begin
  TournamentCount := Min(fIndividualsInTournamentCnt, aPopulation.Count);
  SetLength(TournamentIdx, TournamentCount);
  for I := 0 to TournamentCount - 1 do
    TournamentIdx[I] := Random(aPopulation.Count);
  Result := aPopulation.GetFittest(TournamentIdx);
end;


function TGAAlgorithm.Crossover(aIdv1, aIdv2: TGAIndividual): TGAIndividual;
var
  I: Integer;
  Output: TGAIndividual;
begin
  Output := TGAIndividual.Create(aIdv1.Count);
  for I := 0 to aIdv1.Count - 1 do
    if (Random() < 0.5) then
      Output.Gene[I] := aIdv1.Gene[I]
    else
      Output.Gene[I] := aIdv2.Gene[I];
  Result := Output;
end;


procedure TGAAlgorithm.SetMutation(aMutation: Single);
begin
  fMutation := Min( 1, Max(0, aMutation) );
end;


procedure TGAAlgorithm.Mutate(aIdv: TGAIndividual);
var
  I: Integer;
const
  MutationRate = 0.2; // Mutation
begin
  for I := 0 to aIdv.Count - 1 do
    if (Random() <= MutationRate) then
      aIdv.Gene[I] := Min(  1, Max( 0, aIdv.Gene[I] + fMutation * (2 * Random() - 1) )  );
end;




{ TGAPopulation }
constructor TGAPopulation.Create(aPopulationCount, aGeneCount: Integer; aInitialise: Boolean);
var
  I: Integer;
begin
  SetLength(fIndividuals, aPopulationCount);
  for I := 0 to aPopulationCount - 1 do
  begin
    fIndividuals[I] := nil;
    if aInitialise then
      fIndividuals[I] := TGAIndividual.Create(aGeneCount);
  end;
end;


destructor TGAPopulation.Destroy();
var
  I: Integer;
begin
  for I := 0 to Length(fIndividuals) - 1 do
    fIndividuals[I].Free;
  inherited;
end;


function TGAPopulation.GetIndividual(Idx: Integer): TGAIndividual;
begin
  Result := fIndividuals[Idx];
end;


procedure TGAPopulation.SetIndividual(Idx: Integer; aIdv: TGAIndividual);
begin
  fIndividuals[Idx] := aIdv;
end;


function TGAPopulation.GetFittest(aOnlySomeIdx: TIntegerArray = nil): TGAIndividual;
var
  I: Integer;
  Fittest: TGAIndividual;
begin
  Fittest := nil;
  if (aOnlySomeIdx = nil) OR (Length(aOnlySomeIdx) = 0) then
  begin
    for I := 0 to Length(fIndividuals) - 1 do
      if (Fittest = nil) OR (fIndividuals[I].Fitness > Fittest.Fitness) then
        Fittest := fIndividuals[I];
  end
  else if (Length(aOnlySomeIdx) > 0) then
  begin
    for I := Low(aOnlySomeIdx) to High(aOnlySomeIdx) do
      if (Fittest = nil) OR (fIndividuals[ aOnlySomeIdx[I] ].Fitness > Fittest.Fitness) then
        Fittest := fIndividuals[ aOnlySomeIdx[I] ];
  end;
  Result := Fittest;
end;


function TGAPopulation.GetFittest(var aIdx: Integer): TGAIndividual;
var
  I: Integer;
  Fittest: TGAIndividual;
begin
  Fittest := nil;
  for I := 0 to Length(fIndividuals) - 1 do
    if (Fittest = nil) OR (fIndividuals[I].Fitness > Fittest.Fitness) then
    begin
      Fittest := fIndividuals[I];
      aIdx := I;
    end;
  Result := Fittest;
end;


function TGAPopulation.GetCount(): Integer;
begin
  Result := Length(fIndividuals);
end;





{ TGAIndividual }
constructor TGAIndividual.Create(aCount: Integer);
var
  I: Integer;
begin
  fFitness := 0;
  SetCount(aCount);
  for I := 0 to aCount - 1 do
    fGenes[I] := 0;
end;


destructor TGAIndividual.Destroy();
begin
  inherited;
end;


function TGAIndividual.GetCount(): Integer;
begin
  Result := Length(fGenes);
end;


procedure TGAIndividual.SetCount(aCount: Integer);
begin
  SetLength(fGenes,aCount);
end;


function TGAIndividual.GetGene(Idx: Integer): Single;
begin
  Result := fGenes[Idx];
end;


procedure TGAIndividual.SetGene(Idx: Integer; Value: Single);
begin
  fGenes[Idx] := Value;
end;




{ TGAFitnessCalc }  // This class was replaced with function in Runner for now (and maybe forever)
{
constructor TGAFitnessCalc.Create();
begin
  SetLength(fSolution, 64);// Change length!!!!!!!!!!!
end;


destructor TGAFitnessCalc.Destroy();
begin
end;


procedure TGAFitnessCalc.SetSolution(Idx: Integer; aSolution: Single);
begin
  fSolution[Idx] := aSolution;
end;


function  TGAFitnessCalc.GetFitness(aIdv: TGAIndividual): Integer;
var
  I, Output: Integer;
begin
  Output := 0;
  for I := 0 to aIdv.Count - 1 do
    if aIdv.Gene[I] = fSolution[i] then
      Inc(Output,1);
  Result := Output;
end;


function TGAFitnessCalc.GetMaxFitness(): Integer;
begin
  Result := Length(fSolution);
end;
//}

end.

