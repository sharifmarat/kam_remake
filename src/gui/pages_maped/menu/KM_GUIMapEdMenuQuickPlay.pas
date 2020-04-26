unit KM_GUIMapEdMenuQuickPlay;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes,
  KM_Controls, KM_Defaults, KM_GUIMapEdMenuSave, KM_CommonTypes;

type
  TKMMapEdMenuQuickPlay = class
  private
    fMenuSave: TKMMapEdMenuSave;
    fMapFolder: TKMapFolder;
    procedure Cancel_Click(Sender: TObject);
    procedure QuickPlay_Click(Sender: TObject);
    procedure StartQuickPlay(aMapSaved: Boolean);
    procedure Update_PlayerSelect;
    procedure PlayerSelectFirst;
    procedure UpdatePanel;
    procedure SaveDone(Sender: TObject);
    procedure SaveBtn_EnableStatusChanged(Sender: TObject; aValue: Boolean);
  protected
    PopUp_QuickPlay: TKMPopUpPanel;
      DropList_SelectHand: TKMDropList;
      Radio_AIOpponents: TKMRadioGroup;
      Panel_Save: TKMPanel;

      Label_Difficulty: TKMLabel;
      DropBox_Difficulty: TKMDropList;
      Button_QuickPlay, Button_Cancel: TKMButton;
  public
    constructor Create(aParent: TKMPanel; aOnMapFolderChanged: TMapFolderEvent);
    destructor Destroy; override;

    procedure SetLoadMode(aMapFolder: TKMapFolder);
    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    //Todo refactoring - do not use KeyDown in TKMMapEdMenuQuickPlay, but use PopUp_QuickPlay.OnKeyDown instead
    function KeyDown(Key: Word; Shift: TShiftState): Boolean;
  end;


implementation
uses
  SysUtils, KromUtils, KM_GameApp, KM_Game, KM_HandsCollection, KM_Maps, KM_MapTypes,
  KM_Hand, KM_InterfaceGamePlay,
  KM_RenderUI, KM_ResFonts, KM_ResTexts, KM_Resource, Math;

const
  PANEL_QUICKPLAY_HEIGHT = 505;


constructor TKMMapEdMenuQuickPlay.Create(aParent: TKMPanel; aOnMapFolderChanged: TMapFolderEvent);
const
  CTRLS_WIDTH = 220;
var
  Left, Top: Integer;
begin
  inherited Create;

  PopUp_QuickPlay := TKMPopUpPanel.Create(aParent, 240, PANEL_QUICKPLAY_HEIGHT, gResTexts[TX_MAPED_MAP_QUICK_PLAY], pubgitGray);

  PopUp_QuickPlay.Width := Math.Max(240, gRes.Fonts[PopUp_QuickPlay.Font].GetTextSize(PopUp_QuickPlay.Caption).X + 40);
  Left := (PopUp_QuickPlay.Width - CTRLS_WIDTH) div 2;
    Top := 15;
    TKMLabel.Create(PopUp_QuickPlay, PopUp_QuickPlay.Width div 2, Top, gResTexts[TX_MAPED_MAP_QUICK_PLAY_SEL_PLAYER], fntMetal, taCenter);
    Inc(Top, 25);

    DropList_SelectHand := TKMDropList.Create(PopUp_QuickPlay, Left, Top, CTRLS_WIDTH, 20, fntGame, '', bsGame);
    DropList_SelectHand.Hint := gResTexts[TX_MAPED_MAP_QUICK_PLAY_SEL_PLAYER_TO_START];
    Inc(Top, 30);

    TKMBevel.Create(PopUp_QuickPlay, Left, Top - 5, CTRLS_WIDTH, 70);
    TKMLabel.Create(PopUp_QuickPlay, PopUp_QuickPlay.Width div 2, Top, gResTexts[TX_AI_PLAYER_TYPE], fntOutline, taCenter);
    Inc(Top, 20);
    Radio_AIOpponents := TKMRadioGroup.Create(PopUp_QuickPlay, Left + 5, Top, CTRLS_WIDTH - 10, 40, fntMetal);
    Radio_AIOpponents.Add(gResTexts[TX_AI_PLAYER_CLASSIC]);
    Radio_AIOpponents.Add(gResTexts[TX_AI_PLAYER_ADVANCED]);
    Radio_AIOpponents.ItemIndex := 1;

    Inc(Top, Radio_AIOpponents.Height);
    Panel_Save := TKMPanel.Create(PopUp_QuickPlay, Left, Top, CTRLS_WIDTH, 230);

    Inc(Top, 215);
    Button_QuickPlay := TKMButton.Create(PopUp_QuickPlay, Left, Top, CTRLS_WIDTH, 30, gResTexts[TX_MAPED_MAP_QUICK_PLAY_START_NO_SAVE], bsGame);
    Button_QuickPlay.Hint := gResTexts[TX_MAPED_MAP_QUICK_PLAY_START_NO_SAVE_HINT];
    Button_QuickPlay.OnClick := QuickPlay_Click;

    Inc(Top, 45);
    Label_Difficulty := TKMLabel.Create(PopUp_QuickPlay, Left, Top, gResTexts[TX_MISSION_DIFFICULTY], fntMetal, taLeft);
    Label_Difficulty.Anchors := [anLeft, anBottom];
    Inc(Top, 20);
    DropBox_Difficulty := TKMDropList.Create(PopUp_QuickPlay, Left, Top, CTRLS_WIDTH, 20, fntMetal, gResTexts[TX_MISSION_DIFFICULTY], bsMenu);
    DropBox_Difficulty.Anchors := [anLeft, anBottom];

    Button_Cancel := TKMButton.Create(PopUp_QuickPlay, (PopUp_QuickPlay.Width - CTRLS_WIDTH) div 2, PopUp_QuickPlay.Height - 40,
                                      CTRLS_WIDTH, 30, gResTexts[TX_WORD_CANCEL], bsGame);
    Button_Cancel.Anchors := [anBottom];
    Button_Cancel.Hint := gResTexts[TX_WORD_CANCEL];
    Button_Cancel.OnClick := Cancel_Click;

  fMenuSave := TKMMapEdMenuSave.Create(Panel_Save, SaveDone, aOnMapFolderChanged, 0, 10, 220);

  fMenuSave.Button_SaveCancel.Hide;

  fMenuSave.Button_SaveSave.Top := fMenuSave.Button_SaveSave.Top + 10;
  fMenuSave.Button_SaveSave.Caption := gResTexts[TX_MAPED_MAP_QUICK_PLAY_SAVE_AND_START];
  fMenuSave.Button_SaveSave.Hint := gResTexts[TX_MAPED_MAP_QUICK_PLAY_SAVE_AND_START_HINT];
  fMenuSave.Button_SaveSave.OnChangeEnableStatus := SaveBtn_EnableStatusChanged;

end;


destructor TKMMapEdMenuQuickPlay.Destroy;
begin
  FreeAndNil(fMenuSave);
  inherited;
end;


procedure TKMMapEdMenuQuickPlay.QuickPlay_Click(Sender: TObject);
begin
  StartQuickPlay(False);
end;


procedure TKMMapEdMenuQuickPlay.StartQuickPlay(aMapSaved: Boolean);
var
  GameName, MissionFile: String;
  Color: Cardinal;
  HandID: Integer;
  MapFolder: TKMapFolder;
  Difficulty: TKMMissionDifficulty;
  AIType: TKMAIType;
begin
  MissionFile := gGame.MissionFile;
  GameName := gGame.GameName;
  HandId := DropList_SelectHand.GetSelectedTag;
  Color := gHands[HandId].FlagColor;
  MapFolder := fMapFolder; //Somehow fIsMultiplayer sometimes change its value... have no time to debug it. Just save to local value for now

  Difficulty := mdNone;
  if DropBox_Difficulty.IsClickable and DropBox_Difficulty.IsSelected then
    Difficulty := TKMMissionDifficulty(DropBox_Difficulty.GetSelectedTag);

  AIType := TKMAIType(Radio_AIOpponents.ItemIndex + 1);

  FreeThenNil(gGame);
  gGameApp.NewSingleMap(MissionFile, GameName, HandId, Color, Difficulty, AIType, not aMapSaved);
  gGame.StartedFromMapEditor := True;
  gGame.StartedFromMapEdMapFolder := MapFolder;
  TKMGamePlayInterface(gGame.ActiveInterface).SetMenuState(gGame.MissionMode = mmTactic);
end;


//Todo refactoring - do not use KeyDown in TKMMapEdMenuQuickPlay, but use PopUp_QuickPlay.OnKeyDown instead
function TKMMapEdMenuQuickPlay.KeyDown(Key: Word; Shift: TShiftState): Boolean;
begin
  Result := True; //We want to handle all keys here
  case Key of
    VK_ESCAPE:  if Button_Cancel.IsClickable then
                  Cancel_Click(Button_Cancel);
  end;
end;


procedure TKMMapEdMenuQuickPlay.UpdatePanel;
var
  MD: TKMMissionDIfficulty;
  I: Integer;
begin
  Update_PlayerSelect;
  if not DropList_SelectHand.List.Selected then
  begin
    if gMySpectator.Hand.HasAssets then
      DropList_SelectHand.SelectByTag(gMySpectator.HandID)
    else
      PlayerSelectFirst;
  end;
  Button_QuickPlay.Enabled := (not gGame.MapEditor.IsNewMap or gGame.MapEditor.WasSaved) and DropList_SelectHand.List.Selected;

  //Update Difficulty dropbox
  DropBox_Difficulty.Clear;
  if gGame.MapTxtInfo.HasDifficultyLevels then
  begin
    I := 0;
    for MD in gGame.MapTxtInfo.DifficultyLevels do
    begin
      DropBox_Difficulty.Add(gResTexts[DIFFICULTY_LEVELS_TX[MD]], Byte(MD));
      if MD = mdNormal then //Default difficulty is "Normal"
        DropBox_Difficulty.ItemIndex := I;
      Inc(I);
    end;
    if not DropBox_Difficulty.IsSelected then
      DropBox_Difficulty.ItemIndex := 0;
    DropBox_Difficulty.DoSetVisible;
    Label_Difficulty.DoSetVisible;
  end else begin
    Label_Difficulty.Hide;
    DropBox_Difficulty.Hide;
  end;

  PopUp_QuickPlay.Height := PANEL_QUICKPLAY_HEIGHT - 50*(Byte(not DropBox_Difficulty.IsSetVisible));
end;


procedure TKMMapEdMenuQuickPlay.Update_PlayerSelect;
var
  I: Integer;
begin
  DropList_SelectHand.Clear;
  for I := 0 to MAX_HANDS - 1 do
  begin
    if gHands[I].HasAssets then
      DropList_SelectHand.Add(Format(gResTexts[TX_PLAYER_X], [I + 1]), I);
  end;
end;


procedure TKMMapEdMenuQuickPlay.PlayerSelectFirst;
var
  I: Integer;
begin
  for I := 0 to MAX_HANDS - 1 do
  begin
    if gHands[I].HasAssets then
    begin
      DropList_SelectHand.SelectByTag(I);
      Break;
    end;
  end;
end;


procedure TKMMapEdMenuQuickPlay.SaveBtn_EnableStatusChanged(Sender: TObject; aValue: Boolean);
begin
  if aValue and not DropList_SelectHand.List.Selected then
    fMenuSave.Button_SaveSave.Disable;
end;


procedure TKMMapEdMenuQuickPlay.SaveDone(Sender: TObject);
begin
  StartQuickPlay(True);
end;


procedure TKMMapEdMenuQuickPlay.SetLoadMode(aMapFolder: TKMapFolder);
begin
  fMapFolder := aMapFolder;
  fMenuSave.SetLoadMode(aMapFolder);
end;


procedure TKMMapEdMenuQuickPlay.Cancel_Click(Sender: TObject);
begin
  Hide;
end;


procedure TKMMapEdMenuQuickPlay.Hide;
begin
  PopUp_QuickPlay.Hide;
end;


procedure TKMMapEdMenuQuickPlay.Show;
begin
  UpdatePanel;
  PopUp_QuickPlay.Show;
  fMenuSave.Show;
end;


function TKMMapEdMenuQuickPlay.Visible: Boolean;
begin
  Result := PopUp_QuickPlay.Visible;
end;


end.
