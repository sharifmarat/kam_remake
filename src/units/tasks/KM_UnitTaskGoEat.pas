unit KM_UnitTaskGoEat;
{$I KaM_Remake.inc}
interface
uses
  Classes, KM_CommonClasses, KM_Defaults, KM_Units, KM_Houses, KM_HouseInn, SysUtils, KM_Points;


type
  //Go to eat
  TKMTaskGoEat = class(TKMUnitTask)
  private
    fInn: TKMHouseInn; //Inn in which we are going to eat
    fPlace: ShortInt; //Units place in Inn
  public
    constructor Create(aInn: TKMHouseInn; aUnit: TKMUnit);
    constructor Load(LoadStream: TKMemoryStream); override;
    procedure SyncLoad; override;
    destructor Destroy; override;
    function Eating: Boolean;
    function Execute: TKMTaskResult; override;

    function CouldBeCancelled: Boolean; override;

    procedure Save(SaveStream: TKMemoryStream); override;
  end;


implementation
uses
  KM_HandsCollection, KM_ResWares, KM_Hand;


{ TTaskGoEat }
constructor TKMTaskGoEat.Create(aInn: TKMHouseInn; aUnit: TKMUnit);
begin
  inherited Create(aUnit);

  fType := uttGoEat;
  fInn      := TKMHouseInn(aInn.GetHousePointer);
  fPlace    := -1;
end;


constructor TKMTaskGoEat.Load(LoadStream: TKMemoryStream);
begin
  inherited;

  LoadStream.Read(fInn, 4);
  LoadStream.Read(fPlace);
end;


procedure TKMTaskGoEat.SyncLoad;
begin
  inherited;

  fInn := TKMHouseInn(gHands.GetHouseByUID(Cardinal(fInn)));
end;


destructor TKMTaskGoEat.Destroy;
begin
  //May happen when we die while desperatley trying to get some food
  if Eating then
    fInn.EatersGoesOut(fPlace);

  gHands.CleanUpHousePointer(TKMHouse(fInn));

  inherited;
end;


function TKMTaskGoEat.Eating: Boolean;
begin
  Result := fPlace <> -1;
end;


function TKMTaskGoEat.CouldBeCancelled: Boolean;
begin
  Result := (fPhase - 1) //phase was increased at the end of execution
                    = 1; //Allow cancel task only at walking phases
end;


function TKMTaskGoEat.Execute: TKMTaskResult;
begin
  Result := trTaskContinues;

  if fInn.IsDestroyed then
  begin
    Result := trTaskDone;
    Exit;
  end;

  with fUnit do
  case fPhase of
   0: begin
        Thought := th_Eat;
        if (Home <> nil) and not Home.IsDestroyed then Home.SetState(hst_Empty);
        if not Visible and (InHouse <> nil) and not InHouse.IsDestroyed then
          SetActionGoIn(uaWalk, gd_GoOutside, InHouse) //Walk outside the house
        else
          SetActionLockedStay(0, uaWalk); //Skip this step
      end;
   1: SetActionWalkToSpot(fInn.PointBelowEntrance);
   2: SetActionGoIn(uaWalk, gd_GoInside, fInn); //Enter Inn
   3: begin
        SetActionLockedStay(0, uaWalk);
        fPlace := fInn.EaterGetsInside(UnitType);
        //If there's no free place in the Inn skip to the step where we go out hungry
        if fPlace = -1 then
        begin
          fPhase := 7;
          Exit;
        end;
      end;
   4: //Typically when unit comes to Inn he is at 13%
      //Order is Bread-Sausages-Wine-Fish
      //We allow unit to eat foods until he is over 90% condition
      if (Condition < UNIT_MAX_CONDITION * UNIT_STUFFED_CONDITION_LVL)
        and (fInn.CheckResIn(wt_Bread) > 0) then
      begin
        fInn.ResTakeFromIn(wt_Bread);
        gHands[fUnit.Owner].Stats.WareConsumed(wt_Bread);
        SetActionLockedStay(29*4, uaEat);
        Feed(UNIT_MAX_CONDITION * BREAD_RESTORE);
        fInn.UpdateEater(fPlace, wt_Bread);
      end else
        SetActionLockedStay(0, uaWalk);
   5: if (Condition < UNIT_MAX_CONDITION * UNIT_STUFFED_CONDITION_LVL)
        and (fInn.CheckResIn(wt_Sausages) > 0) then
      begin
        fInn.ResTakeFromIn(wt_Sausages);
        gHands[fUnit.Owner].Stats.WareConsumed(wt_Sausages);
        SetActionLockedStay(29*4, uaEat);
        Feed(UNIT_MAX_CONDITION * SAUSAGE_RESTORE);
        fInn.UpdateEater(fPlace, wt_Sausages);
      end else
        SetActionLockedStay(0, uaWalk);
   6: if (Condition < UNIT_MAX_CONDITION * UNIT_STUFFED_CONDITION_LVL)
        and (fInn.CheckResIn(wt_Wine) > 0) then
      begin
        fInn.ResTakeFromIn(wt_Wine);
        gHands[fUnit.Owner].Stats.WareConsumed(wt_Wine);
        SetActionLockedStay(29*4, uaEat);
        Feed(UNIT_MAX_CONDITION * WINE_RESTORE);
        fInn.UpdateEater(fPlace, wt_Wine);
      end else
        SetActionLockedStay(0, uaWalk);
   7: if (Condition < UNIT_MAX_CONDITION * UNIT_STUFFED_CONDITION_LVL)
        and (fInn.CheckResIn(wt_Fish) > 0) then
      begin
        fInn.ResTakeFromIn(wt_Fish);
        gHands[fUnit.Owner].Stats.WareConsumed(wt_Fish);
        SetActionLockedStay(29*4, uaEat);
        Feed(UNIT_MAX_CONDITION * FISH_RESTORE);
        fInn.UpdateEater(fPlace, wt_Fish);
      end else
        SetActionLockedStay(0, uaWalk);
   8: begin
        //Stop showing hungry if we no longer are,
        //but if we are then walk out of the inn thinking hungry
        //so that the player will know that we haven't been fed
        if Condition < UNIT_MIN_CONDITION then
          Thought := th_Eat
        else
          Thought := th_None;
        SetActionGoIn(uaWalk, gd_GoOutside, fInn); //Exit Inn
        fInn.EatersGoesOut(fPlace);
        fPlace := -1;
      end;
   else
      Result := trTaskDone;
  end;

  Inc(fPhase);
end;


procedure TKMTaskGoEat.Save(SaveStream: TKMemoryStream);
begin
  inherited;

  if fInn <> nil then
    SaveStream.Write(fInn.UID) //Store ID, then substitute it with reference on SyncLoad
  else
    SaveStream.Write(Integer(0));
  SaveStream.Write(fPlace);
end;


end.
