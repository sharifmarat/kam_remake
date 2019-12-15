unit KM_PointerContainer;
{$I KaM_Remake.inc}
interface
//uses
//  KM_Houses, KM_UnitGroup;

type
  TKMPointerContainerType = (pctUnit, pctHouse, pctGroup);

  TKMPointerContainer<T> = class
  private
    fPointerObject: T;
    procedure SetPointer(aPointerObject: T);
  protected
    fPointerType: TKMPointerContainerType;

    function GetPointer(aPointerObject: T): T; virtual; abstract;
    procedure ReleasePointer(aPointerObject: T); virtual; abstract;
  public
//    constructor Create(aObject: T);
    destructor Destroy; override;

    property Obj: T read fPointerObject write SetPointer;
//    procedure AssignPointer(aObject: TObject);
  end;

//  TKMUnitPointerContainer = class(TKMPointerContainer<TKMUnit>)
//  protected
//    procedure GetPointer(aPointerObject: TKMUnit); override;
//    procedure ReleasePointer(aPointerObject: TKMUnit); override;
//  end;
//
//  TKMHousePointerContainer = class(TKMPointerContainer<TKMHouse>)
//  protected
//    procedure GetPointer(aPointerObject: TKMHouse); override;
//    procedure ReleasePointer(aPointerObject: TKMHouse); override;
//  end;



  
implementation


{ TKMPointerContainer }
//constructor TKMPointerContainer<T>.Create(aObject: T);
//begin
//  inherited Create;
//
//  GetPointer(fPointerObject);
//end;


destructor TKMPointerContainer<T>.Destroy;
begin
  ReleasePointer(fPointerObject);

  inherited;
end;


procedure TKMPointerContainer<T>.SetPointer(aPointerObject: T);
begin
  ReleasePointer(fPointerObject);

  fPointerObject := GetPointer(aPointerObject);
end;





//{ TKMHousePointerContainer }
//procedure TKMHousePointerContainer.GetPointer(aPointerObject: TKMHouse);
//begin
//  aPointerObject.GetHousePointer;
//end;
//
//
//procedure TKMHousePointerContainer.ReleasePointer(aPointerObject: TKMHouse);
//begin
//  aPointerObject.ReleaseHousePointer;
//end;
//
//
//{ TKMUnitGroupPointerContainer }
//procedure TKMUnitGroupPointerContainer.GetPointer(aPointerObject: TKMUnitGroup);
//begin
//  aPointerObject.GetGroupPointer;
//end;
//
//
//procedure TKMUnitGroupPointerContainer.ReleasePointer(aPointerObject: TKMUnitGroup);
//begin
//  aPointerObject.ReleaseGroupPointer;
//end;


end.
