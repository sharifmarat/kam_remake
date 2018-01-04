unit KM_GUIMenuCampaignEditor;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, Controls, SysUtils, Math,
  KM_Controls, KM_Pics, KM_Defaults,
  KM_Campaigns, KM_InterfaceDefaults;

type
  TKMMenuCampaignEditor = class (TKMMenuPageCommon)
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class
    fCampaign: TKMCampaign;

    fMousePosition: TPoint;
    fMouseMove: Boolean;

    procedure SelectMap(AIndex: Integer);

    procedure ListChange(Sender: TObject);
    procedure Campaign_SelectMap(Sender: TObject);
    procedure BackClick(Sender: TObject);
  protected
    Panel_CampaignEditor, Panel_Map, Panel_Left: TKMPanel;
    Image_CampaignBG: TKMImage;
    Image_CampaignFlags: array[0..MAX_CAMP_MAPS - 1] of TKMImage;
    Label_CampaignFlags: array[0..MAX_CAMP_MAPS - 1] of TKMLabel;
    Image_CampaignSubNode: array[0..MAX_CAMP_NODES - 1] of TKMImage;
    ColumnBox_Missions: TKMColumnBox;
    Button_CampaignEditorBack: TKMButton;
    Label_MousePosition: TKMLabel;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    procedure Show(aCampaign: TKMCampaignId);
    procedure Resize(X, Y: Word); override;
    procedure MenuKeyDown(Key: Word; Shift: TShiftState); override;
    procedure Hide; override;
    procedure MenuMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MenuMouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MenuMouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
  end;

implementation

uses
  KM_ResTexts, KM_ResFonts, KM_GameApp, KM_RenderUI;

const
  FLAG_LABEL_OFFSET_X = 10;
  FLAG_LABEL_OFFSET_Y = 7;

{ TKMMainMenuInterface }

constructor TKMMenuCampaignEditor.Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
var
  i: Integer;
begin
  inherited Create;
  fOnPageChange := aOnPageChange;

  Panel_CampaignEditor := TKMPanel.Create(aParent, 0, 0, aParent.Width, aParent.Height);
  Panel_CampaignEditor.AnchorsCenter;

  Panel_Map := TKMPanel.Create(Panel_CampaignEditor, 224, 0, 1024, 768);

  TKMImage.Create(Panel_Map,-448,-216, 960, 600, 17, rxGuiMain).AnchorsCenter;
  TKMImage.Create(Panel_Map, 512,-216, 960, 600, 18, rxGuiMain).AnchorsCenter;
  TKMImage.Create(Panel_Map,-448, 384, 960, 600, 19, rxGuiMain).AnchorsCenter;
  TKMImage.Create(Panel_Map, 512, 384, 960, 600, 20, rxGuiMain).AnchorsCenter;

  Image_CampaignBG := TKMImage.Create(Panel_Map, 0, 0, aParent.Width, aParent.Height, 0, rxGuiMain);
  for i := 0 to High(Image_CampaignFlags) do
  begin
    Image_CampaignFlags[I] := TKMImage.Create(Panel_Map, aParent.Width, aParent.Height, 23, 29, 10, rxGuiMain);
    Image_CampaignFlags[I].OnClick := Campaign_SelectMap;
    Image_CampaignFlags[I].Tag := I;

    Label_CampaignFlags[I] := TKMLabel.Create(Panel_Map, aParent.Width, aParent.Height, IntToStr(i+1), fnt_Mini, taCenter);
    Label_CampaignFlags[I].FontColor := $FFD0D0D0;
    Label_CampaignFlags[I].Hitable := False;
  end;

  for I := 0 to High(Image_CampaignSubNode) do
  begin
    Image_CampaignSubNode[I] := TKMImage.Create(Panel_Map, aParent.Width, aParent.Height, 0, 0, 16, rxGuiMain);
    Image_CampaignSubNode[I].ImageCenter; //Pivot at the center of the dot (width/height = 0)
  end;

  Panel_Left := TKMPanel.Create(Panel_CampaignEditor, 0, 0, 224, Panel_CampaignEditor.Height);

  TKMImage.Create(Panel_Left, 0, 0, 224, 400, 404);
  TKMImage.Create(Panel_Left, 0, 400, 224, 400, 404);
  TKMImage.Create(Panel_Left, 0, 800, 224, 400, 404); //For 1600x1200 this is needed

  ColumnBox_Missions := TKMColumnBox.Create(Panel_Left, 10, 100, 180, 500, fnt_Grey, bsMenu);
  ColumnBox_Missions.SetColumns(fnt_Outline, ['Mission'], [0]);

  Button_CampaignEditorBack := TKMButton.Create(Panel_Left, 10, Panel_Left.Height - 40, 180, 30, gResTexts[TX_MENU_BACK], bsMenu);
  Button_CampaignEditorBack.Anchors := [anLeft, anBottom];
  Button_CampaignEditorBack.OnClick := BackClick;

  Label_MousePosition := TKMLabel.Create(Panel_CampaignEditor, 0, 0, 864, 40, '0x0', fnt_Grey, taLeft);

  Panel_Map.Left := Panel_Left.Width + (Panel_CampaignEditor.Width - Panel_Left.Width) div 2 - Panel_Map.Width div 2;
  Panel_Map.Top := Panel_CampaignEditor.Height div 2 - Panel_Map.Height div 2;
end;

procedure TKMMenuCampaignEditor.Hide;
begin
  inherited;

  Panel_CampaignEditor.Hide;
end;

procedure TKMMenuCampaignEditor.MenuKeyDown(Key: Word; Shift: TShiftState);
begin
  inherited;
  {
  case Key of
    VK_LEFT: Panel_Map.Left := Panel_Map.Left - IfThen(ssShift in Shift, 100, 10);
    VK_RIGHT: Panel_Map.Left := Panel_Map.Left + IfThen(ssShift in Shift, 100, 10);
    VK_UP: Panel_Map.Top := Panel_Map.Top - IfThen(ssShift in Shift, 100, 10);
    VK_DOWN: Panel_Map.Top := Panel_Map.Top + IfThen(ssShift in Shift, 100, 10);
  end;
  }
end;

procedure TKMMenuCampaignEditor.MenuMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  fMousePosition.X := X;
  fMousePosition.Y := Y;
  fMouseMove := Button = mbLeft;
end;

procedure TKMMenuCampaignEditor.MenuMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if fMouseMove then
  begin

  end;

  fMousePosition.X := X;
  fMousePosition.Y := Y;

  Label_MousePosition.Caption := Format('%dx%d   %d', [X, Y, Panel_Map.FocusedControlIndex]);
end;

procedure TKMMenuCampaignEditor.MenuMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  fMouseMove := False;
end;

procedure TKMMenuCampaignEditor.Resize(X, Y: Word);
begin
  inherited;
  Panel_CampaignEditor.Left := -Panel_CampaignEditor.Parent.Left;
  Panel_CampaignEditor.Top := -Panel_CampaignEditor.Parent.Top;
  Panel_CampaignEditor.Width := Panel_CampaignEditor.Parent.Width;
  Panel_CampaignEditor.Height := Panel_CampaignEditor.Parent.Height;

  Panel_Left.Height := Panel_CampaignEditor.Height;

  Panel_Map.Left := Panel_Left.Width + (Panel_CampaignEditor.Width - Panel_Left.Width) div 2 - Panel_Map.Width div 2;
  Panel_Map.Top := Panel_CampaignEditor.Height div 2 - Panel_Map.Height div 2;
end;

procedure TKMMenuCampaignEditor.Show(aCampaign: TKMCampaignId);
const MapPic: array [Boolean] of byte = (10, 11);
var
  i: Integer;
begin
  fCampaign := gGameApp.Campaigns.CampaignById(aCampaign);

  Image_CampaignBG.RX := fCampaign.BackGroundPic.RX;
  Image_CampaignBG.TexID := fCampaign.BackGroundPic.ID;

  ColumnBox_Missions.Clear;
  for i := 0 to fCampaign.MapCount - 1 do
    ColumnBox_Missions.AddItem(MakeListRow([Format(gResTexts[TX_GAME_MISSION], [i + 1])], i));

  //Setup sites
  for I := 0 to High(Image_CampaignFlags) do
  begin
    Image_CampaignFlags[I].Visible := I < fCampaign.MapCount;
    Image_CampaignFlags[I].TexID   := MapPic[I <= fCampaign.UnlockedMap];
    Image_CampaignFlags[I].HighlightOnMouseOver := True;
    Label_CampaignFlags[I].Visible := (I < fCampaign.MapCount);
  end;

  //Place sites
  for I := 0 to fCampaign.MapCount - 1 do
  begin
    //Pivot flags around Y=bottom X=middle, that's where the flag pole is
    Image_CampaignFlags[I].Left := fCampaign.Maps[I].Flag.X - Round((Image_CampaignFlags[I].Width/2){*(1 - Panel_Campaign_Flags.Scale)});
    Image_CampaignFlags[I].Top  := fCampaign.Maps[I].Flag.Y - Round(Image_CampaignFlags[I].Height   {*(1 - Panel_Campaign_Flags.Scale)});

    Label_CampaignFlags[I].AbsLeft := Image_CampaignFlags[I].AbsLeft + FLAG_LABEL_OFFSET_X;
    Label_CampaignFlags[I].AbsTop := Image_CampaignFlags[I].AbsTop + FLAG_LABEL_OFFSET_Y;
  end;


  Panel_CampaignEditor.Show;
end;

procedure TKMMenuCampaignEditor.SelectMap(AIndex: Integer);
var
  i: Integer;
begin
  for i := 0 to High(Image_CampaignFlags) do
    Image_CampaignFlags[I].Highlight := i = AIndex;

end;

procedure TKMMenuCampaignEditor.ListChange(Sender: TObject);
begin
  if ColumnBox_Missions.ItemIndex = -1 then
  begin

  end
  else
    SelectMap(ColumnBox_Missions.Rows[ColumnBox_Missions.ItemIndex].Tag);
end;

procedure TKMMenuCampaignEditor.Campaign_SelectMap(Sender: TObject);
var
  i: Integer;
begin
  SelectMap(TKMControl(Sender).Tag);
  for i := 0 to ColumnBox_Missions.RowCount - 1 do
    if ColumnBox_Missions.Item[i].Tag = TKMControl(Sender).Tag then
    begin
      ColumnBox_Missions.ItemIndex := i;
      Break;
    end;
end;

procedure TKMMenuCampaignEditor.BackClick(Sender: TObject);
begin
  fOnPageChange(gpCampSelect, 'EDIT');
end;

end.
