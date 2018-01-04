unit KM_InterfaceDefaults;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Controls, Classes,
  KM_Controls;


type
  TKMMenuPageType =  (gpMainMenu,
                        gpSinglePlayer,
                          gpCampaign,
                          gpCampaignEditor,
                          gpCampSelect,
                          gpCampaignEditSelect,
                          gpSingleMap,
                          gpLoad,
                        gpMultiplayer,
                          gpLobby,
                        gpReplays,
                        gpMapEditor,
                        gpOptions,
                        gpCredits,
                      gpResultsMP,
                      gpResultsSP,
                      gpLoading,
                      gpError);
  TGUIEvent = procedure (Sender: TObject; Dest: TKMMenuPageType) of object;
  TGUIEventText = procedure (Dest: TKMMenuPageType; const aText: UnicodeString = '') of object;

  TKMUserInterfaceCommon = class;

  TKMMenuPageCommon = class
  private
    FParent: TKMUserInterfaceCommon;
  protected
    OnKeyDown: TNotifyEventKeyShift;
    OnEscKeyDown: TNotifyEvent;
  public
    procedure MenuKeyDown(Key: Word; Shift: TShiftState); virtual;
    procedure MenuKeyUp(Key: Word; Shift: TShiftState); virtual;
    procedure MenuMouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual;
    procedure MenuMouseMove(Shift: TShiftState; X,Y: Integer); virtual;
    procedure MenuMouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure Resize(X, Y: Word); virtual;
  end;

  TKMFileIdentInfo = record // File identification info (for maps/saves)
    CRC: Cardinal;
    Name: UnicodeString;
  end;


  TKMUserInterfaceCommon = class
  protected
    fMyControls: TKMMasterControl;
    Panel_Main: TKMPanel;
  public
    constructor Create(aScreenX, aScreenY: Word);
    destructor Destroy; override;

    property MyControls: TKMMasterControl read fMyControls;
    procedure ExportPages(const aPath: string); virtual; abstract;

    procedure KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean); virtual; abstract;
    procedure KeyPress(Key: Char);
    procedure KeyUp(Key: Word; Shift: TShiftState; var aHandled: Boolean); virtual; abstract;
    //Child classes don't pass these events to controls depending on their state
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual; abstract;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); overload;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer; var aHandled: Boolean); overload; virtual; abstract;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual; abstract;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer); virtual;
    procedure Resize(X,Y: Word); virtual;
    procedure UpdateState(aTickCount: Cardinal); virtual;
    procedure Paint; virtual;
  end;


const
  //Options sliders
  OPT_SLIDER_MIN = 0;
  OPT_SLIDER_MAX = 20;
  MAX_SAVENAME_LENGTH = 50;

  CHAT_MENU_ALL = -1;
  CHAT_MENU_TEAM = -2;
  CHAT_MENU_SPECTATORS = -3;


implementation


{ TKMUserInterface }
constructor TKMUserInterfaceCommon.Create(aScreenX, aScreenY: Word);
begin
  inherited Create;

  fMyControls := TKMMasterControl.Create;

  //Parent Panel for whole UI
  Panel_Main := TKMPanel.Create(fMyControls, 0, 0, aScreenX, aScreenY);
end;


destructor TKMUserInterfaceCommon.Destroy;
begin
  fMyControls.Free;
  inherited;
end;


procedure TKMUserInterfaceCommon.KeyPress(Key: Char);
begin
  fMyControls.KeyPress(Key);
end;


procedure TKMUserInterfaceCommon.MouseMove(Shift: TShiftState; X, Y: Integer);
var MouseMoveHandled: Boolean;
begin
  MouseMove(Shift, X, Y, MouseMoveHandled);
end;


procedure TKMUserInterfaceCommon.MouseWheel(Shift: TShiftState; WheelDelta, X, Y: Integer);
begin
  fMyControls.MouseWheel(X, Y, WheelDelta);
end;


procedure TKMUserInterfaceCommon.Resize(X, Y: Word);
begin
  Panel_Main.Width := X;
  Panel_Main.Height := Y;
end;


procedure TKMUserInterfaceCommon.UpdateState(aTickCount: Cardinal);
begin
  inherited;
  fMyControls.UpdateState(aTickCount);
end;


procedure TKMUserInterfaceCommon.Paint;
begin
  fMyControls.Paint;
end;

{ TKMMenuPageCommon }

procedure TKMMenuPageCommon.MenuKeyDown(Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:  if Assigned(OnEscKeyDown) then
                  OnEscKeyDown(Self);
    else        if Assigned(OnKeyDown) then
                  OnKeyDown(Key, Shift);
  end;
end;

procedure TKMMenuPageCommon.MenuKeyUp(Key: Word; Shift: TShiftState);
begin

end;

procedure TKMMenuPageCommon.MenuMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TKMMenuPageCommon.MenuMouseMove(Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TKMMenuPageCommon.MenuMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TKMMenuPageCommon.Resize(X, Y: Word);
begin

end;

procedure TKMMenuPageCommon.Show;
begin

end;

procedure TKMMenuPageCommon.Hide;
begin

end;


end.
