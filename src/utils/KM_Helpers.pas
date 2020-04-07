unit KM_Helpers;
interface
uses
  Vcl.StdCtrls;


type
  TCheckBoxHelper = class helper for TCheckBox
    procedure SetStateWithoutClick(aState: TCheckBoxState);
    procedure SetCheckedWithoutClick(aChecked: Boolean);
  end;

implementation
uses
  Classes;


{ TCheckBoxHelper }
procedure TCheckBoxHelper.SetStateWithoutClick(aState: TCheckBoxState);
var
    BckEvent: TNotifyEvent;
begin
    BckEvent := OnClick;
    OnClick := nil;
    try
      State := aState;
    finally
      OnClick := BckEvent;
    end;
end;


procedure TCheckBoxHelper.SetCheckedWithoutClick(aChecked: Boolean);
var
    BckEvent: TNotifyEvent;
begin
    BckEvent := OnClick;
    OnClick := nil;
    try
      Checked := aChecked;
    finally
      OnClick := BckEvent;
    end;
end;

end.
