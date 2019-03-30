unit KM_HouseInn;
{$I KaM_Remake.inc}
interface
uses
  KM_Houses,
  KM_ResWares, KM_ResHouses,
  KM_CommonClasses, KM_Defaults;


type
  TKMHouseInn = class(TKMHouse)
  private
    Eater: array [0..5] of record //only 6 units are allowed in the inn
      UnitType: TKMUnitType;
      FoodKind: TKMWareType; //What kind of food eater eats
      EatStep: Cardinal;
    end;
  public
    constructor Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
    constructor Load(LoadStream: TKMemoryStream); override;
    function EaterGetsInside(aUnitType: TKMUnitType): ShortInt;
    procedure UpdateEater(aIndex: ShortInt; aFoodKind: TKMWareType);
    procedure EatersGoesOut(aIndex: ShortInt);
    function HasFood: Boolean;
    function HasSpace: Boolean;
    procedure Save(SaveStream: TKMemoryStream); override;
    procedure Paint; override; //Render all eaters
  end;


implementation
uses
  KM_RenderPool,
  KM_Hand, KM_HandsCollection,
  KM_Points;


{ TKMHouseInn }
constructor TKMHouseInn.Create(aUID: Integer; aHouseType: TKMHouseType; PosX, PosY: Integer; aOwner: TKMHandID; aBuildState: TKMHouseBuildState);
var
  I: Integer;
begin
  inherited;

  for I := Low(Eater) to High(Eater) do
    Eater[I].UnitType := utNone;
end;


constructor TKMHouseInn.Load(LoadStream: TKMemoryStream);
begin
  inherited;
  LoadStream.Read(Eater, SizeOf(Eater));
end;


//EatStep := FlagAnimStep, cos increases it each frame, we don't need to increase all 6 AnimSteps manually
function TKMHouseInn.EaterGetsInside(aUnitType: TKMUnitType): ShortInt;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(Eater) to High(Eater) do
  if Eater[I].UnitType = utNone then
  begin
    Eater[I].UnitType := aUnitType;
    Eater[I].FoodKind := wtNone;
    Eater[I].EatStep  := FlagAnimStep;
    Result := I;
    Exit;
  end;
end;


procedure TKMHouseInn.UpdateEater(aIndex: ShortInt; aFoodKind: TKMWareType);
begin
  if aIndex = -1 then Exit;
  Assert(aFoodKind in [wtWine, wtBread, wtSausages, wtFish], 'Wrong kind of food in Inn');

  Eater[aIndex].FoodKind := aFoodKind; //Order is Wine-Bread-Sausages-Fish
  Eater[aIndex].EatStep  := FlagAnimStep; //Eat animation step will be difference between FlagAnim and EatStep
end;


procedure TKMHouseInn.EatersGoesOut(aIndex: ShortInt);
begin
  if aIndex <> -1 then
    Eater[aIndex].UnitType := utNone;
end;


function TKMHouseInn.HasFood: Boolean;
begin
  Result := CheckResIn(wtSausages) + CheckResIn(wtBread) + CheckResIn(wtWine) + CheckResIn(wtFish) > 0;
end;


function TKMHouseInn.HasSpace: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Eater) to High(Eater) do
    Result := Result or (Eater[I].UnitType = utNone);
end;


procedure TKMHouseInn.Save(SaveStream: TKMemoryStream);
begin
  inherited;
  SaveStream.Write(Eater, SizeOf(Eater));
end;


procedure TKMHouseInn.Paint;
  //Chose eater animation direction (135 face south, 246 face north)
  function AnimDir(aIndex: Integer): TKMDirection;
  begin
    case Eater[aIndex].FoodKind of
      wtWine:      Result  := TKMDirection(1 * 2 - 1 + (aIndex div 3));
      wtBread:     Result  := TKMDirection(2 * 2 - 1 + (aIndex div 3));
      wtSausages:  Result  := TKMDirection(3 * 2 - 1 + (aIndex div 3));
      wtFish:      Result  := TKMDirection(4 * 2 - 1 + (aIndex div 3));
      else          Result  := dirNA;
    end;
  end;
const
  offX: array [0..2] of Single = ( -0.5, 0, 0.5);
  offY: array [0..2] of Single = (-0.05, 0, 0.05);
var
  I: Integer;
  AnimStep: Cardinal;
begin
  inherited;
  if fBuildState <> hbsDone then exit;

  for I := Low(Eater) to High(Eater) do
  begin
    if (Eater[I].UnitType = utNone) or (Eater[I].FoodKind = wtNone) then Continue;

    AnimStep := FlagAnimStep - Eater[I].EatStep; //Delta is our AnimStep

    gRenderPool.AddHouseEater(fPosition, Eater[I].UnitType, uaEat,
                              AnimDir(I), AnimStep,
                              offX[I mod 3], offY[I mod 3],
                              gHands[fOwner].GameFlagColor);
  end;
end;


end.
