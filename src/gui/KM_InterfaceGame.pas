unit KM_InterfaceGame;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  SysUtils, Controls, Classes, Math, KM_Defaults, KM_Controls, KM_Points,
  KM_InterfaceDefaults, KM_CommonUtils, KM_CommonTypes,
  KM_GameCursor, KM_Render, KM_Minimap, KM_Viewport, KM_ResHouses, KM_ResWares;


type
  // Common class for ingame interfaces (Gameplay, MapEd)
  TKMUserInterfaceGame = class(TKMUserInterfaceCommon)
  private
    fDragScrollingCursorPos: TPoint;
    fDragScrollingViewportPos: TKMPointF;
    fPrevHint: TObject;
    fPrevHintMessage: UnicodeString;
    fOnUserAction: TKMUserActionEvent;
    procedure ResetDragScrolling;
  protected
    fMinimap: TKMMinimap;
    fViewport: TKMViewport;
    fDragScrolling: Boolean;

    Panel_Hint: TKMPanel;
      Label_Hint: TKMLabel;
      Bevel_HintBG: TKMBevel;

    function IsDragScrollingAllowed: Boolean; virtual;
    procedure DisplayHint(Sender: TObject);
    procedure AfterCreateComplete;
  public
    constructor Create(aRender: TRender); reintroduce;
    destructor Destroy; override;

    property Minimap: TKMMinimap read fMinimap;
    property Viewport: TKMViewport read fViewport;
    property OnUserAction: TKMUserActionEvent read fOnUserAction write fOnUserAction;

    function CursorToMapCoord(X, Y: Integer): TKMPointF;

    procedure KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean); override;
    procedure KeyUp(Key: Word; Shift: TShiftState; var aHandled: Boolean); override;
    procedure KeyPress(Key: Char); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var aHandled: Boolean); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer; var aHandled: Boolean); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;

    procedure GameSpeedChanged(aFromSpeed, aToSpeed: Single);
    procedure SyncUI(aMoveViewport: Boolean = True); virtual;
    procedure SyncUIView(const aCenter: TKMPointF; aZoom: Single = 1);
    procedure UpdateGameCursor(X, Y: Integer; Shift: TShiftState);
    procedure UpdateStateIdle(aFrameTime: Cardinal); virtual; abstract;
  end;


const
  // Toolbar pads
  TB_PAD = 9; // Picked up empirically
  TB_WIDTH = 180; // Max width of sidebar elements
  PAGE_TITLE_Y = 5; // Page title offset
  STATS_LINES_CNT = 13; //Number of stats (F3) lines

  // Shortcuts
  // All shortcuts are in English and are the same for all languages to avoid
  // naming collisions and confusion in discussions

  GUI_HOUSE_COUNT = 28;   // Number of KaM houses to show in GUI
  GUIHouseOrder: array [1..GUI_HOUSE_COUNT] of TKMHouseType = (
    htSchool, htInn, htQuary, htWoodcutters, htSawmill,
    htFarm, htMill, htBakery, htSwine, htButchers,
    htWineyard, htGoldMine, htCoalMine, htMetallurgists, htWeaponWorkshop,
    htTannery, htArmorWorkshop, htStables, htIronMine, htIronSmithy,
    htWeaponSmithy, htArmorSmithy, htBarracks, htStore, htWatchTower,
    htFisherHut, htMarketplace, htTownHall);

  // Template for how resources are shown in Barracks
  BARRACKS_RES_COUNT = 11;
  BarracksResType: array [1..BARRACKS_RES_COUNT] of TKMWareType =
    (wt_Shield, wt_MetalShield, wt_Armor, wt_MetalArmor, wt_Axe, wt_Sword,
     wt_Pike, wt_Hallebard, wt_Bow, wt_Arbalet, wt_Horse);

  // Layout of resources in Store
  STORE_RES_COUNT = 28;
  StoreResType: array [1..STORE_RES_COUNT] of TKMWareType =
    (wt_Trunk,    wt_Stone,   wt_Wood,        wt_IronOre,   wt_GoldOre,
     wt_Coal,     wt_Steel,   wt_Gold,        wt_Wine,      wt_Corn,
     wt_Bread,    wt_Flour,   wt_Leather,     wt_Sausages,  wt_Pig,
     wt_Skin,     wt_Shield,  wt_MetalShield, wt_Armor,     wt_MetalArmor,
     wt_Axe,      wt_Sword,   wt_Pike,        wt_Hallebard, wt_Bow,
     wt_Arbalet,  wt_Horse,   wt_Fish);

  School_Order: array [0..13] of TKMUnitType = (
    ut_Serf, ut_Worker, ut_StoneCutter, ut_Woodcutter, ut_Lamberjack,
    ut_Fisher, ut_Farmer, ut_Baker, ut_AnimalBreeder, ut_Butcher,
    ut_Miner, ut_Metallurgist, ut_Smith, ut_Recruit);

  Barracks_Order: array [0..8] of TKMUnitType = (
    ut_Militia, ut_AxeFighter, ut_Swordsman, ut_Bowman, ut_Arbaletman,
    ut_Pikeman, ut_Hallebardman, ut_HorseScout, ut_Cavalry);

  TownHall_Order: array [0..5] of TKMUnitType = (
    ut_Peasant, ut_Militia, ut_Slingshot, ut_Horseman, ut_Barbarian, ut_MetalBarbarian);

  // Stats get stacked by UI logic (so that on taller screens they all were
  // in nice pairs, and would stack up only on short screens)
  StatPlan: array [0..STATS_LINES_CNT-1] of record
    HouseType: array [0..3] of TKMHouseType;
    UnitType: array [0..1] of TKMUnitType;
  end = (
    (HouseType: (htQuary, htNone, htNone, htNone);                      UnitType: (ut_StoneCutter, ut_None)),
    (HouseType: (htWoodcutters, htNone, htNone, htNone);                UnitType: (ut_Woodcutter, ut_None)),
    (HouseType: (htFisherHut, htNone, htNone, htNone);                  UnitType: (ut_Fisher, ut_None)),
    (HouseType: (htFarm, htWineyard, htNone, htNone);                   UnitType: (ut_Farmer, ut_None)),
    (HouseType: (htMill, htBakery, htNone, htNone);                     UnitType: (ut_Baker, ut_None)),
    (HouseType: (htSwine, htStables, htNone, htNone);                   UnitType: (ut_AnimalBreeder, ut_None)),
    (HouseType: (htButchers, htTannery, htNone, htNone);                UnitType: (ut_Butcher, ut_None)),
    (HouseType: (htMetallurgists, htIronSmithy, htNone, htNone);        UnitType: (ut_Metallurgist, ut_None)),
    (HouseType: (htArmorSmithy, htWeaponSmithy, htNone, htNone);        UnitType: (ut_Smith, ut_None)),
    (HouseType: (htCoalMine, htIronMine, htGoldMine, htNone);           UnitType: (ut_Miner, ut_None)),
    (HouseType: (htSawmill, htWeaponWorkshop, htArmorWorkshop, htNone); UnitType: (ut_Lamberjack, ut_None)),
    (HouseType: (htBarracks, htTownHall, htWatchTower, htNone);         UnitType: (ut_Recruit, ut_None)),
    (HouseType: (htStore, htSchool, htInn, htMarketplace);              UnitType: (ut_Serf, ut_Worker))
    );

  MapEd_Order: array [0..13] of TKMUnitType = (
    ut_Militia, ut_AxeFighter, ut_Swordsman, ut_Bowman, ut_Arbaletman,
    ut_Pikeman, ut_Hallebardman, ut_HorseScout, ut_Cavalry, ut_Barbarian,
    ut_Peasant, ut_Slingshot, ut_MetalBarbarian, ut_Horseman);

  MapEd_Icon: array [0..13] of Word = (
    61, 62, 63, 64, 65,
    66, 67, 68, 69, 70,
    79, 80, 81, 82);

  Animal_Order: array [0..7] of TKMUnitType = (
    ut_Wolf, ut_Fish,        ut_Watersnake, ut_Seastar,
    ut_Crab, ut_Waterflower, ut_Waterleaf,  ut_Duck);

  Animal_Icon: array [0..7] of word = (
    71, 72, 73, 74,
    75, 76, 77, 78);

  MARKET_RES_HEIGHT = 35;

  // Big tab buttons in MapEd
  BIG_TAB_W = 36;
  BIG_PAD_W = 36;
  BIG_TAB_H = 36;
  // Small sub-tab buttons in MapEd
  SMALL_TAB_W = 30;
  SMALL_PAD_W = 30;
  SMALL_TAB_H = 26;

  MESSAGE_AREA_HEIGHT = 173+17; // Image_ChatHead + Image_ChatBody
  MESSAGE_AREA_RESIZE_Y = 200; // How much can we resize it


implementation
uses
  KM_Main, KM_Game, KM_HandSpectator, KM_Terrain, KM_RenderPool, KM_Resource, KM_ResCursors, KM_ResKeys, KM_RenderUI, KM_ResFonts;


{ TKMUserInterfaceGame }
constructor TKMUserInterfaceGame.Create(aRender: TRender);
begin
  inherited Create(aRender.ScreenX, aRender.ScreenY);

  fMinimap := TKMMinimap.Create(False, False);
  fViewport := TKMViewport.Create(aRender.ScreenX, aRender.ScreenY);

  fDragScrolling := False;
  fDragScrollingCursorPos.X := 0;
  fDragScrollingCursorPos.Y := 0;
  fDragScrollingViewportPos := KMPOINTF_ZERO;

  gRenderPool := TRenderPool.Create(fViewport, aRender);
end;


procedure TKMUserInterfaceGame.AfterCreateComplete;
begin
  //Hints should be created last, as they should be above everything in UI, to be show on top of all other Controls
  Bevel_HintBG := TKMBevel.Create(Panel_Main,224+35,Panel_Main.Height-23,300,21);
  Bevel_HintBG.BackAlpha := 0.5;
  Bevel_HintBG.EdgeAlpha := 0.5;
  Bevel_HintBG.Hide;
  Bevel_HintBG.Anchors := [anLeft, anBottom];
  Label_Hint := TKMLabel.Create(Panel_Main,224+40,Panel_Main.Height-21,0,0,'',fnt_Outline,taLeft);
  Label_Hint.Anchors := [anLeft, anBottom];

  // Controls without a hint will reset the Hint to ''
  fMyControls.OnHint := DisplayHint;
end;


destructor TKMUserInterfaceGame.Destroy;
begin
  FreeAndNil(fMinimap);
  FreeAndNil(fViewport);
  FreeAndNil(gRenderPool);
  Inherited;
end;


procedure TKMUserInterfaceGame.KeyPress(Key: Char);
begin
  inherited;
  if Assigned(fOnUserAction) then
    fOnUserAction(uatKeyPress);
end;


procedure TKMUserInterfaceGame.KeyDown(Key: Word; Shift: TShiftState; var aHandled: Boolean);
  {$IFDEF MSWindows}
var
  WindowRect: TRect;
  {$ENDIF}
begin
  if Assigned(fOnUserAction) then
    fOnUserAction(uatKeyDown);

  aHandled := True;
  //Scrolling
  if Key = gResKeys[SC_SCROLL_LEFT].Key       then fViewport.ScrollKeyLeft  := True
  else if Key = gResKeys[SC_SCROLL_RIGHT].Key then fViewport.ScrollKeyRight := True
  else if Key = gResKeys[SC_SCROLL_UP].Key    then fViewport.ScrollKeyUp    := True
  else if Key =  gResKeys[SC_SCROLL_DOWN].Key then fViewport.ScrollKeyDown  := True
  else if Key = gResKeys[SC_ZOOM_IN].Key      then fViewport.ZoomKeyIn      := True
  else if Key = gResKeys[SC_ZOOM_OUT].Key     then fViewport.ZoomKeyOut     := True
  else if Key = gResKeys[SC_ZOOM_RESET].Key   then fViewport.ResetZoom
  else if (Key = gResKeys[SC_MAP_DRAG_SCROLL].Key)
      and IsDragScrollingAllowed then
  begin
    fDragScrolling := True;
   // Restrict the cursor to the window, for now.
   //TODO: Allow one to drag out of the window, and still capture.
   {$IFDEF MSWindows}
     WindowRect := gMain.ClientRect(1); //Reduce ClientRect by 1 pixel, to fix 'jump viewport' bug when dragscrolling over the window border
     ClipCursor(@WindowRect);
   {$ENDIF}
   fDragScrollingCursorPos.X := gGameCursor.Pixel.X;
   fDragScrollingCursorPos.Y := gGameCursor.Pixel.Y;
   fDragScrollingViewportPos.X := fViewport.Position.X;
   fDragScrollingViewportPos.Y := fViewport.Position.Y;
   gRes.Cursors.Cursor := kmc_Drag;
  end
  else
    aHandled := False;
end;


procedure TKMUserInterfaceGame.KeyUp(Key: Word; Shift: TShiftState; var aHandled: Boolean);
begin
  if Assigned(fOnUserAction) then
    fOnUserAction(uatKeyUp);

  aHandled := True;
  //Scrolling
  if Key = gResKeys[SC_SCROLL_LEFT].Key       then fViewport.ScrollKeyLeft  := False
  else if Key = gResKeys[SC_SCROLL_RIGHT].Key then fViewport.ScrollKeyRight := False
  else if Key = gResKeys[SC_SCROLL_UP].Key    then fViewport.ScrollKeyUp    := False
  else if Key =  gResKeys[SC_SCROLL_DOWN].Key then fViewport.ScrollKeyDown  := False
  else if Key = gResKeys[SC_ZOOM_IN].Key      then fViewport.ZoomKeyIn      := False
  else if Key = gResKeys[SC_ZOOM_OUT].Key     then fViewport.ZoomKeyOut     := False
  else if Key = gResKeys[SC_ZOOM_RESET].Key   then fViewport.ResetZoom
  else if Key = gResKeys[SC_MAP_DRAG_SCROLL].Key then
  begin
    if fDragScrolling then
      ResetDragScrolling;
  end
  else aHandled := False;
end;


procedure TKMUserInterfaceGame.ResetDragScrolling;
begin
  fDragScrolling := False;
  gRes.Cursors.Cursor := kmc_Default; //Reset cursor
  gMain.ApplyCursorRestriction;
end;


function TKMUserInterfaceGame.IsDragScrollingAllowed: Boolean;
begin
  Result := True; // Allow drag scrolling by default
end;


procedure TKMUserInterfaceGame.DisplayHint(Sender: TObject);
var
  TxtSize: TKMPoint;
begin
  if (fPrevHint = nil) and (Sender = nil) then Exit; //in this case there is nothing to do

  if (fPrevHint <> nil) and (Sender = fPrevHint)
    and (TKMControl(fPrevHint).Hint = fPrevHintMessage) then Exit; // Hint didn't change (not only Hint object, but also Hint message didn't change)

  if (Sender = Label_Hint) or (Sender = Bevel_HintBG) then Exit; // When previous Hint obj is covered by Label_Hint or Bevel_HintBG ignore it.

  if (Sender = nil) or (TKMControl(Sender).Hint = '') then
  begin
    Label_Hint.Caption := '';
    Bevel_HintBG.Hide;
    fPrevHintMessage := '';
  end
  else
  begin
    Label_Hint.Caption := TKMControl(Sender).Hint;
    TxtSize := gRes.Fonts[Label_Hint.Font].GetTextSize(Label_Hint.Caption);
    Bevel_HintBG.Width := 10 + TxtSize.X;
    Bevel_HintBG.Height := 2 + TxtSize.Y;
    Bevel_HintBG.Top := Panel_Main.Height - Bevel_HintBG.Height - 2;
    Bevel_HintBG.Show;
    Label_Hint.Top := Bevel_HintBG.Top + 2;
    fPrevHintMessage := TKMControl(Sender).Hint;
  end;
  fPrevHint := Sender;
end;


procedure TKMUserInterfaceGame.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  inherited;
  if Assigned(fOnUserAction) then
    fOnUserAction(uatMouseUp);
end;


procedure TKMUserInterfaceGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  inherited;
  if Assigned(fOnUserAction) then
    fOnUserAction(uatMouseDown);
end;


procedure TKMUserInterfaceGame.MouseMove(Shift: TShiftState; X,Y: Integer; var aHandled: Boolean);
var
  VP: TKMPointF;
begin
  inherited;

  if Assigned(fOnUserAction) then
    fOnUserAction(uatMouseMove);

  aHandled := False;
  if fDragScrolling then
  begin
    if GetKeyState(gResKeys[SC_MAP_DRAG_SCROLL].Key) < 0 then
    begin
      UpdateGameCursor(X, Y, Shift);
      VP.X := fDragScrollingViewportPos.X + (fDragScrollingCursorPos.X - X) / (CELL_SIZE_PX * fViewport.Zoom);
      VP.Y := fDragScrollingViewportPos.Y + (fDragScrollingCursorPos.Y - Y) / (CELL_SIZE_PX * fViewport.Zoom);
      fViewport.Position := VP;
      aHandled := True;
    end else
      ResetDragScrolling;
  end;
end;


procedure TKMUserInterfaceGame.MouseWheel(Shift: TShiftState; WheelDelta, X, Y: Integer; var aHandled: Boolean);
var
  PrevCursor: TKMPointF;
begin
  inherited;

  if Assigned(fOnUserAction) then
    fOnUserAction(uatMouseWheel);

  if (X < 0) or (Y < 0) then Exit; // This happens when you use the mouse wheel on the window frame

  // Allow to zoom only when cursor is over map. Controls handle zoom on their own
  if aHandled then Exit;
  
  UpdateGameCursor(X, Y, Shift); // Make sure we have the correct cursor position to begin with
  PrevCursor := gGameCursor.Float;
  fViewport.Zoom := fViewport.Zoom + WheelDelta / 2000;
  UpdateGameCursor(X, Y, Shift); // Zooming changes the cursor position
  // Move the center of the screen so the cursor stays on the same tile, thus pivoting the zoom around the cursor
  fViewport.Position := KMPointF(fViewport.Position.X + PrevCursor.X-gGameCursor.Float.X,
                                 fViewport.Position.Y + PrevCursor.Y-gGameCursor.Float.Y);
  UpdateGameCursor(X, Y, Shift); // Recentering the map changes the cursor position
  aHandled := True;
end;


procedure TKMUserInterfaceGame.GameSpeedChanged(aFromSpeed, aToSpeed: Single);
begin
  fViewport.GameSpeedChanged(aFromSpeed, aToSpeed);
end;


procedure TKMUserInterfaceGame.SyncUI(aMoveViewport: Boolean = True);
begin
  fMinimap.LoadFromTerrain;
  fMinimap.Update;

  if aMoveViewport then
  begin
    fViewport.ResizeMap(gTerrain.MapX, gTerrain.MapY, gTerrain.TopHill / CELL_SIZE_PX);
    fViewport.ResetZoom;
  end;
end;


procedure TKMUserInterfaceGame.SyncUIView(const aCenter: TKMPointF; aZoom: Single = 1);
begin
  fViewport.Position := aCenter;
  fViewport.Zoom := aZoom;
end;


function TKMUserInterfaceGame.CursorToMapCoord(X, Y: Integer): TKMPointF;
begin
  Result.X := fViewport.Position.X + (X-fViewport.ViewRect.Right/2-TOOLBAR_WIDTH/2)/CELL_SIZE_PX/fViewport.Zoom;
  Result.Y := fViewport.Position.Y + (Y-fViewport.ViewRect.Bottom/2)/CELL_SIZE_PX/fViewport.Zoom;
  Result.Y := gTerrain.ConvertCursorToMapCoord(Result.X, Result.Y);
end;


// Compute cursor position and store it in global variables
procedure TKMUserInterfaceGame.UpdateGameCursor(X, Y: Integer; Shift: TShiftState);
begin
  with gGameCursor do
  begin
    Pixel.X := X;
    Pixel.Y := Y;
    Float := CursorToMapCoord(X, Y);

    PrevCell := Cell; //Save previous cell

    // Cursor cannot reach row MapY or column MapX, they're not part of the map (only used for vertex height)
    Cell.X := EnsureRange(round(Float.X+0.5), 1, gTerrain.MapX-1); // Cell below cursor in map bounds
    Cell.Y := EnsureRange(round(Float.Y+0.5), 1, gTerrain.MapY-1);

    ObjectUID := gRenderPool.RenderList.GetSelectionUID(Float);
    SState := Shift;
  end;
end;


end.
