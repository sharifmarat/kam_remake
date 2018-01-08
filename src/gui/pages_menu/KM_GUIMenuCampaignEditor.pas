unit KM_GUIMenuCampaignEditor;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLType, {$ENDIF}
  Classes, Controls, SysUtils, Math, KM_Points,
  KM_Controls, KM_Pics, KM_Defaults,
  KM_Campaigns, KM_InterfaceDefaults;

type
  TKMMenuCampaignEditor = class (TKMMenuPageCommon)
  private
    fOnPageChange: TGUIEventText; //will be in ancestor class
    fCampaign: TKMCampaign;

    fSelectedItem: array[0..2] of Integer;

    procedure SelectMap;

    procedure ListChange(Sender: TObject);
    procedure Campaign_SelectMap(Sender: TObject);
    procedure BackClick(Sender: TObject);
    procedure UpdateList;
    procedure MoveObject(Sender: TObject);
    procedure MouseWheel(Sender: TObject; WheelDelta: Integer);

    function GetActiveChapter: TKMCampaignChapter;
    function GetActiveMission: TKMCampaignMap;

  protected
    Panel_CampaignEditor, Panel_Map, Panel_Left: TKMPanel;
    Image_CampaignBG: TKMImage;
    Image_CampaignFlags: array[0..MAX_CAMP_MAPS - 1] of TKMImage;
    Label_CampaignFlags: array[0..MAX_CAMP_MAPS - 1] of TKMLabel;
    Image_CampaignSubNode: array[0..MAX_CAMP_NODES - 1] of TKMImage;
    ColumnBox_Missions: TKMColumnBox;
    Button_CampaignEditorBack: TKMButton;

    Label_MousePosition, Label_Index: TKMLabel;
    property ActiveChapter: TKMCampaignChapter read GetActiveChapter;
    property ActiveMission: TKMCampaignMap read GetActiveMission;
  public
    constructor Create(aParent: TKMPanel; aOnPageChange: TGUIEventText);
    procedure Show(aCampaign: TKMCampaignId);
    procedure Resize(X, Y: Word); override;
    procedure MenuKeyDown(Key: Word; Shift: TShiftState); override;
    procedure Hide; override;
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

  TKMImage.Create(Panel_Map,-448,-216, 960, 600, 17, rxGuiMain).AnchorsStretch;
  TKMImage.Create(Panel_Map, 512,-216, 960, 600, 18, rxGuiMain).AnchorsStretch;
  TKMImage.Create(Panel_Map,-448, 384, 960, 600, 19, rxGuiMain).AnchorsStretch;
  TKMImage.Create(Panel_Map, 512, 384, 960, 600, 20, rxGuiMain).AnchorsStretch;

  Image_CampaignBG := TKMImage.Create(Panel_Map, 0, 0, aParent.Width, aParent.Height, 0, rxGuiMain);
  Image_CampaignBG.DragAndDrop := True;
  Image_CampaignBG.OnMoveDragAndDrop := MoveObject;
  Image_CampaignBG.OnMouseWheel := MouseWheel;
  Image_CampaignBG.AnchorsStretch;

  for i := 0 to High(Image_CampaignFlags) do
  begin
    Image_CampaignFlags[I] := TKMImage.Create(Panel_Map, aParent.Width, aParent.Height, 23, 29, 10, rxGuiMain);
    Image_CampaignFlags[I].DragAndDrop := True;
    Image_CampaignFlags[I].OnMoveDragAndDrop := MoveObject;
    Image_CampaignFlags[I].OnClick := Campaign_SelectMap;
    Image_CampaignFlags[I].Tag := I;

    Label_CampaignFlags[I] := TKMLabel.Create(Panel_Map, aParent.Width, aParent.Height, IntToStr(i+1), fnt_Outline, taCenter);
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
  ColumnBox_Missions.OnChange := ListChange;

  Button_CampaignEditorBack := TKMButton.Create(Panel_Left, 10, Panel_Left.Height - 40, 180, 30, gResTexts[TX_MENU_BACK], bsMenu);
  Button_CampaignEditorBack.Anchors := [anLeft, anBottom];
  Button_CampaignEditorBack.OnClick := BackClick;

  Label_MousePosition := TKMLabel.Create(Panel_CampaignEditor, 10, 10, 864, 40, '0x0', fnt_Grey, taLeft);
  Label_Index := TKMLabel.Create(Panel_CampaignEditor, 10, 30, 864, 40, '0x0', fnt_Grey, taLeft);

  Panel_Map.Left := Panel_Left.Width + (Panel_CampaignEditor.Width - Panel_Left.Width) div 2 - Panel_Map.Width div 2;
  Panel_Map.Top := Panel_CampaignEditor.Height div 2 - Panel_Map.Height div 2;

  fSelectedItem[0] := 0;
  fSelectedItem[1] := -1;
  fSelectedItem[2] := -1;
end;

function TKMMenuCampaignEditor.GetActiveChapter: TKMCampaignChapter;
begin
  if fSelectedItem[0] >= 0 then
    Result := fCampaign.Chapters[fSelectedItem[0]];
end;

function TKMMenuCampaignEditor.GetActiveMission: TKMCampaignMap;
begin
  if (fSelectedItem[0] >= 0) and (fSelectedItem[1] >= 0) then
    Result := GetActiveChapter.Maps[fSelectedItem[1]];
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
    Ord('A'): Panel_Map.Left := Panel_Map.Left - IfThen(ssShift in Shift, 100, 10);
    Ord('D'): Panel_Map.Left := Panel_Map.Left + IfThen(ssShift in Shift, 100, 10);
    Ord('W'): Panel_Map.Top := Panel_Map.Top - IfThen(ssShift in Shift, 100, 10);
    Ord('S'): Panel_Map.Top := Panel_Map.Top + IfThen(ssShift in Shift, 100, 10);
  end;
  }
end;

procedure TKMMenuCampaignEditor.MouseWheel(Sender: TObject; WheelDelta: Integer);
//var
//  i: Integer;
begin
  Panel_Map.Scale := Panel_Map.Scale * IfThen(WheelDelta > 0, 0.99, 1.01);
 { for i := 0 to Panel_Map.ChildCount - 1 do
    if Panel_Map.Childs[i] is TKMImage then

    Panel_Map.Childs[i].Scale := Panel_Map.Scale;

  Label_MousePosition.Caption := Format('%dx%d   %f', [Panel_Map.SelfWidth, Panel_Map.SelfHeight, Panel_Map.Scale]);     }
end;

procedure TKMMenuCampaignEditor.MoveObject(Sender: TObject);
var
  Vector: TKMPoint;
  i: Integer;
begin
  if Image_CampaignBG = Sender then
  begin
    Vector := KMPoint(Image_CampaignBG.Left, Image_CampaignBG.Top);
    Image_CampaignBG.Left := 0;
    Image_CampaignBG.Top := 0;
    Panel_Map.Left := Panel_Map.Left + Vector.X;
    Panel_Map.Top := Panel_Map.Top + Vector.Y;
  end
  else
    if fSelectedItem[0] >= 0 then
    begin
      for i := 0 to ActiveChapter.MapCount - 1 do
        if Image_CampaignFlags[i] = Sender then
        begin
         // fCampaign.Maps[Image_CampaignFlags[i].Tag].Flag := KMPointW(Image_CampaignFlags[I].Left, Image_CampaignFlags[I].Top);

          Label_CampaignFlags[I].AbsLeft := Image_CampaignFlags[I].AbsLeft + FLAG_LABEL_OFFSET_X;
          Label_CampaignFlags[I].AbsTop := Image_CampaignFlags[I].AbsTop + FLAG_LABEL_OFFSET_Y;

          Exit;
        end;
      {
      if fSelectedMission >= 0 then
        for i := 0 to ActiveMission.NodeCount - 1 do
          if Image_CampaignFlags[i] = Sender then
          begin

          end;
      }
    end;
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

procedure TKMMenuCampaignEditor.UpdateList;
var
  i, m, n: Integer;
  ListRowChapter, ListRowMission, ListRowNode: TKMListRow;
begin

  ColumnBox_Missions.Clear;

  for i := 0 to High(fCampaign.Chapters) do
  begin
    ListRowChapter := MakeListRow([Format('Chapter %d', [i + 1])], i);
    ColumnBox_Missions.AddItem(ListRowChapter);

    for m := 0 to fCampaign.Chapters[i].MapCount - 1 do
    begin
      ListRowMission := MakeListRow([Format(gResTexts[TX_GAME_MISSION], [m + 1])], m);
      ListRowMission.Level := 1;
      ColumnBox_Missions.AddItem(ListRowMission);

      for n := 0 to fCampaign.Chapters[i].Maps[m].NodeCount - 1 do
      begin
        ListRowNode := MakeListRow([Format('Node %d', [n + 1])], n);
        ListRowNode.Level := 2;
        ColumnBox_Missions.AddItem(ListRowNode);
      end;
    end;

  end;
end;

procedure TKMMenuCampaignEditor.Show(aCampaign: TKMCampaignId);
const
  MapPic: array [Boolean] of byte = (10, 11);
var
  i: Integer;
begin
  fCampaign := gGameApp.Campaigns.CampaignById(aCampaign);

  Image_CampaignBG.RX := fCampaign.BackGroundPic.RX;
  Image_CampaignBG.TexID := fCampaign.BackGroundPic.ID;

  UpdateList;

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
    Image_CampaignFlags[I].Left := fCampaign.Maps[I].Flag.X;
    Image_CampaignFlags[I].Top  := fCampaign.Maps[I].Flag.Y;

    Label_CampaignFlags[I].AbsLeft := Image_CampaignFlags[I].AbsLeft + FLAG_LABEL_OFFSET_X;
    Label_CampaignFlags[I].AbsTop := Image_CampaignFlags[I].AbsTop + FLAG_LABEL_OFFSET_Y;
  end;


  Panel_CampaignEditor.Show;
end;

procedure TKMMenuCampaignEditor.SelectMap;
var
  i: Integer;
begin
  for i := 0 to High(Image_CampaignFlags) do
    Image_CampaignFlags[i].Highlight := i = fSelectedItem[1];

end;

procedure TKMMenuCampaignEditor.ListChange(Sender: TObject);

  function FindPrevRowByLevel(ALevel: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := ColumnBox_Missions.ItemIndex - 1 downto 0 do
      if ColumnBox_Missions.Rows[i].Level = ALevel then
      begin
        Result := ColumnBox_Missions.Rows[i].Tag;
        Exit;
      end;
  end;

var
  i: Integer;
  ListRow: TKMListRow;
begin
  fSelectedItem[0] := -1;
  fSelectedItem[1] := -1;
  fSelectedItem[2] := -1;
  if ColumnBox_Missions.IsSelected then
  begin
    ListRow := ColumnBox_Missions.SelectedItem;
    fSelectedItem[0] := IfThen(ListRow.Level = 0, ListRow.Tag, FindPrevRowByLevel(0));
    if ListRow.Level > 0 then
      fSelectedItem[1] := IfThen(ListRow.Level = 1, ListRow.Tag, FindPrevRowByLevel(1));
    if ListRow.Level > 1 then
      fSelectedItem[2] := ListRow.Tag;
  end;
  Label_Index.Caption := Format('%d  %d  %d', [fSelectedItem[0], fSelectedItem[1], fSelectedItem[2]]);
  SelectMap;
end;

procedure TKMMenuCampaignEditor.Campaign_SelectMap(Sender: TObject);
var
  i: Integer;
  Level: Integer;
begin
  if fSelectedItem[0] >= 0 then
  begin
    for i := 0 to ActiveChapter.MapCount - 1 do
      if Image_CampaignFlags[i] = Sender then
      begin
        fSelectedItem[1] := i;
        Break;
      end;

    Level := 0;
    for i := 0 to ColumnBox_Missions.RowCount - 1 do
      if (ColumnBox_Missions.Rows[i].Level = Level) and (ColumnBox_Missions.Rows[i].Tag = fSelectedItem[Level]) then
      begin
        Inc(Level);
        if (Level = 2) or (fSelectedItem[Level] = -1) then
        begin
          ColumnBox_Missions.itemIndex := i;
          Break;
        end;
      end;
    SelectMap;
  end;
end;

procedure TKMMenuCampaignEditor.BackClick(Sender: TObject);
begin
  fOnPageChange(gpCampSelect, 'EDIT');
end;

end.
