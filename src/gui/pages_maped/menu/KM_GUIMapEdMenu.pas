unit KM_GUIMapEdMenu;
{$I KaM_Remake.inc}
interface
uses
   Classes, Controls, SysUtils,  KM_Defaults,
   KM_Controls, KM_InterfaceGame, KM_InterfaceDefaults,
   KM_GUIMapEdMenuResize,
   KM_GUIMapEdMenuQuickPlay,
   KM_GUIMapEdMenuLoad,
   KM_GUIMapEdMenuSave,
   KM_GUIMapEdMenuQuit,
   KM_GUIMapEdMenuSettings,
   KM_CommonTypes;

type
  TKMMapEdMenu = class (TKMMapEdMenuPage)
  private
    fGuiMenuResize: TKMMapEdMenuResize;
    fGuiMenuQuickPlay: TKMMapEdMenuQuickPlay;
    fGuiMenuLoad: TKMMapEdMenuLoad;
    fGuiMenuSave: TKMMapEdMenuSave;
    fGuiMenuSettings: TKMMapEdMenuSettings;
    fGuiMenuQuit: TKMMapEdMenuQuit;
    procedure MenuClick(Sender: TObject);
    procedure MenuDone(Sender: TObject);
  protected
    Panel_Menu: TKMPanel;
      Button_Resize: TKMButton;
      Button_Menu_Save: TKMButton;
      Button_Menu_Load: TKMButton;
      Button_QuickPlay: TKMButton;
      Button_Menu_Settings: TKMButton;
      Button_Menu_Quit: TKMButton;
    procedure DoShowSubMenu(aIndex: Byte); override;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent; aOnMapFolderChanged: TMapFolderEvent);
    destructor Destroy; override;

    property GuiMenuResize: TKMMapEdMenuResize read fGuiMenuResize;
    property GuiMenuQuickPlay: TKMMapEdMenuQuickPlay read fGuiMenuQuickPlay write fGuiMenuQuickPlay;
    procedure SetLoadMode(aMapFolder: TKMapFolder);
    procedure Show;
    procedure Hide;
    function Visible: Boolean; override;
    procedure UpdateState;
  end;


implementation
uses
  KM_ResTexts, KM_RenderUI, KM_Utils;


{ TKMapEdInterface }
constructor TKMMapEdMenu.Create(aParent: TKMPanel; aOnPageChange: TNotifyEvent; aOnMapFolderChanged: TMapFolderEvent);
begin
  inherited Create;

  fGuiMenuResize := TKMMapEdMenuResize.Create(aParent, MenuDone, aOnPageChange);
  fGuiMenuLoad := TKMMapEdMenuLoad.Create(aParent, MenuDone);
  fGuiMenuSave := TKMMapEdMenuSave.Create(aParent, MenuDone, aOnMapFolderChanged);
  fGuiMenuQuit := TKMMapEdMenuQuit.Create(aParent, MenuDone);
  fGuiMenuSettings := TKMMapEdMenuSettings.Create(aParent, MenuDone);

  Panel_Menu := TKMPanel.Create(aParent, 0, 45, aParent.Width, aParent.Height - 45);
  Panel_Menu.Anchors := [anLeft, anTop, anBottom];

  Button_Resize := TKMButton.Create(Panel_Menu, 9, 10, Panel_Menu.Width - 9, 30, gResTexts[TX_MAPED_MAP_RESIZE], bsGame);
  Button_Resize.Anchors := [anLeft, anTop, anRight];
  Button_Resize.Hint := GetHintWHotKey(TX_MAPED_MAP_RESIZE, MAPED_SUBMENU_HOTKEYS[0]);
  Button_Resize.OnClick := MenuClick;

  Button_QuickPlay := TKMButton.Create(Panel_Menu, 9, 50, Panel_Menu.Width - 9, 30, gResTexts[TX_MAPED_MAP_QUICK_PLAY], bsGame);
  Button_QuickPlay.Anchors := [anLeft, anTop, anRight];
  Button_QuickPlay.Hint := GetHintWHotKey(TX_MAPED_MAP_QUICK_PLAY_HINT, MAPED_SUBMENU_HOTKEYS[1]);
  Button_QuickPlay.OnClick := MenuClick;

  Button_Menu_Load := TKMButton.Create(Panel_Menu, 9, 110, Panel_Menu.Width - 9, 30, gResTexts[TX_MAPED_LOAD_TITLE], bsGame);
  Button_Menu_Load.Anchors := [anLeft, anTop, anRight];
  Button_Menu_Load.OnClick := MenuClick;
  Button_Menu_Load.Hint := GetHintWHotKey(TX_MAPED_LOAD_TITLE, MAPED_SUBMENU_HOTKEYS[2]);
  Button_Menu_Save := TKMButton.Create(Panel_Menu, 9, 150, Panel_Menu.Width - 9, 30, gResTexts[TX_MAPED_SAVE_TITLE], bsGame);
  Button_Menu_Save.Anchors := [anLeft, anTop, anRight];
  Button_Menu_Save.OnClick := MenuClick;
  Button_Menu_Save.Hint := GetHintWHotKey(TX_MAPED_SAVE_TITLE, MAPED_SUBMENU_HOTKEYS[3]);
  Button_Menu_Settings := TKMButton.Create(Panel_Menu, 9, 190, TB_MAP_ED_WIDTH - 9, 30, gResTexts[TX_MENU_SETTINGS], bsGame);
  Button_Menu_Settings.Hint := GetHintWHotKey(TX_MENU_SETTINGS, MAPED_SUBMENU_HOTKEYS[4]);
  Button_Menu_Settings.OnClick := MenuClick;

  Button_Menu_Quit := TKMButton.Create(Panel_Menu, 9, 250, Panel_Menu.Width - 9, 30, gResTexts[TX_MENU_QUIT_MAPED], bsGame);
  Button_Menu_Quit.Anchors := [anLeft, anTop, anRight];
  Button_Menu_Quit.Hint := GetHintWHotKey(TX_MENU_QUIT_MAPED, MAPED_SUBMENU_HOTKEYS[5]);
  Button_Menu_Quit.OnClick := MenuClick;
end;


destructor TKMMapEdMenu.Destroy;
begin
  fGuiMenuResize.Free;
  fGuiMenuLoad.Free;
  fGuiMenuSave.Free;
  fGuiMenuQuit.Free;
  fGuiMenuSettings.Free;

  inherited;
end;


procedure TKMMapEdMenu.DoShowSubMenu(aIndex: Byte);
begin
  inherited;

  case aIndex of
    0: Button_Resize.Click;
    1: Button_QuickPlay.Click;
    2: Button_Menu_Load.Click;
    3: Button_Menu_Save.Click;
    4: Button_Menu_Settings.Click;
    5: Button_Menu_Quit.Click;
  end;
end;


procedure TKMMapEdMenu.MenuClick(Sender: TObject);
begin
  if Sender <> Button_QuickPlay  then
    Hide;

  if Sender = Button_Resize then
    fGuiMenuResize.Show
  else
  if Sender = Button_QuickPlay then
    fGuiMenuQuickPlay.Show
  else
  if Sender = Button_Menu_Quit then
    fGuiMenuQuit.Show
  else
  if Sender = Button_Menu_Save then
    fGuiMenuSave.Show
  else
  if Sender = Button_Menu_Load then
    fGuiMenuLoad.Show;
  if Sender = Button_Menu_Settings then
  begin
    fGuiMenuSettings.Menu_Settings_Fill;
    fGuiMenuSettings.Show;
  end;
end;


procedure TKMMapEdMenu.MenuDone(Sender: TObject);
begin
  fGuiMenuResize.Hide;
  fGuiMenuQuickPlay.Hide;
  fGuiMenuLoad.Hide;
  fGuiMenuSave.Hide;
  fGuiMenuQuit.Hide;
  fGuiMenuSettings.Hide;

  Show;
end;


procedure TKMMapEdMenu.Hide;
begin
  Panel_Menu.Hide;
end;


procedure TKMMapEdMenu.Show;
begin
  Panel_Menu.Show;
end;


function TKMMapEdMenu.Visible: Boolean;
begin
  Result := Panel_Menu.Visible;
end;


procedure TKMMapEdMenu.UpdateState;
begin
  fGuiMenuLoad.UpdateState;
end;


procedure TKMMapEdMenu.SetLoadMode(aMapFolder: TKMapFolder);
begin
  fGuiMenuResize.SetLoadMode(aMapFolder);
  fGuiMenuQuickPlay.SetLoadMode(aMapFolder);
  fGuiMenuLoad.SetLoadMode(aMapFolder);
  fGuiMenuSave.SetLoadMode(aMapFolder);
end;


end.
