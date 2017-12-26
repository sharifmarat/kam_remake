unit Main;
{$I ..\..\KaM_Remake.inc}
interface
uses
  Windows, Classes, ComCtrls, Controls, Dialogs, ExtDlgs, ExtCtrls, Forms,
  Graphics, Mask, Math, Spin, StdCtrls, SysUtils, KM_Points,
  KM_Defaults, KM_Campaigns, KM_Pics, KM_ResSpritesEdit, KromUtils, inifiles,
  Quad.ObjectInspector, Vcl.ToolWin, System.ImageList, Vcl.ImgList,
  System.Actions, Vcl.ActnList, Vcl.Menus, RenderPanel;

const
  crHandMove = TCursor(1);

type
  TTreeChapter = class(TTreeNode)
  public
    ImageIndex: Byte;
    constructor Create(AOwner: TTreeNodes); override;
  end;

  TTreeChapterItem = class(TTreeNode)
  public
    Position: TKMPointW;
   end;

  TTreeChapterMission = class(TTreeChapterItem)
  public
    TextPos: TBriefingCorner;
    Video: array[TMissionVideoTypes] of AnsiString;
    constructor Create(AOwner: TTreeNodes); override;
  end;

  TTreeChapterNode = class(TTreeChapterItem)
  public
    constructor Create(AOwner: TTreeNodes); override;
  end;

  TMainForm = class(TForm)
    dlgOpenPicture: TOpenDialog;
    dlgSaveCampaign: TSaveDialog;
    StatusBar1: TStatusBar;
    ScrollBox1: TScrollBox;
    imgBlackFlag: TImage;
    imgRedFlag: TImage;
    imgNode: TImage;
    shpBriefing: TShape;
    ActionList: TActionList;
    aOpen: TAction;
    aNew: TAction;
    aSave: TAction;
    ImageList: TImageList;
    aOpenPicture: TAction;
    dlgOpenCampaign: TOpenDialog;
    pLeft: TPanel;
    tvList: TTreeView;
    pmMap: TPopupMenu;
    Addlevel1: TMenuItem;
    AddNode1: TMenuItem;
    aAddMission: TAction;
    aAddNode: TAction;
    pTop: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    aSettings: TAction;
    ImageTree: TImageList;
    MissionBox: TGroupBox;
    Panel1: TPanel;
    cbShowNodeNumbers: TCheckBox;
    cbShowBriefingPosition: TCheckBox;
    BriefingPositionPanel: TPanel;
    cbBriefingPos: TComboBox;
    VideoBeforePanel: TPanel;
    VideoAfterPanel: TPanel;
    edtVideoAfter: TEdit;
    edtVideoBefore: TEdit;
    Splitter: TSplitter;
    aAddChapter: TAction;
    AddChapter1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure tvListChange(Sender: TObject; Node: TTreeNode);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure aNewExecute(Sender: TObject);
    procedure aOpenExecute(Sender: TObject);
    procedure aSaveExecute(Sender: TObject);
    procedure aOpenPictureExecute(Sender: TObject);
    procedure aAddMissionExecute(Sender: TObject);
    procedure aAddNodeExecute(Sender: TObject);
    procedure aSettingsExecute(Sender: TObject);
    procedure tvListCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
    procedure aAddChapterExecute(Sender: TObject);
    procedure tvListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pmMapPopup(Sender: TObject);
  private
    FRender: TRenderPanel;
    fExePath: string;
    fCampaignsPath: string;
    fSprites: TKMSpritePackEdit;

    fUpdating: Boolean;
    FTreeItemCreate: TTreeNodeClass;
    fSelectedChapter: TTreeChapter;
    fSelectedMission: TTreeChapterMission;
    fSelectedNode: TTreeChapterNode;

    procedure LoadCmp(aFileName : String);

    function DlgQuestionShow(aCaption, aMsg: string): boolean;

    function GetCharset(aLang: string): TFontCharset;
    procedure LoadCampaignName(aFileName, aLocale: string);
    procedure SaveCampaignName(aFileName: string);
    procedure CreateDefaultLocaleLibxTemplate(aFileName: string);
    procedure AddMission(X, Y: Integer);
    procedure CampaignToList;
    procedure ListToCampaign;
  public
    procedure FlagDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FlagMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FlagEnter(Sender: TObject);
    procedure NodeDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure NodeMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure NodeEnter(Sender: TObject);

    procedure FlagNodeLeave(Sender: TObject);

    procedure UpdateCaption;
    procedure DrawFlagNumber(aIndexMap: Integer);
    procedure DrawNodeNumber(aIndexNode: Integer);

    property Sprites: TKMSpritePackEdit read fSprites;
    property SelectedChapter: TTreeChapter read fSelectedChapter;
    property SelectedMission: TTreeChapterMission read fSelectedMission;
    property SelectedNode: TTreeChapterNode read fSelectedNode;
  end;

var
  MainForm: TMainForm;
  C: TKMCampaign;
  Locale: String;

implementation

{$R *.dfm}

uses
  uCampaignSettings;

{ TTreeMap }

constructor TTreeChapter.Create(AOwner: TTreeNodes);
begin
  inherited;
  ImageIndex := 0;
  SelectedIndex := 0;
end;

{ TTreeItemMission }

constructor TTreeChapterMission.Create(AOwner: TTreeNodes);
begin
  inherited;
  ImageIndex := 1;
  SelectedIndex := 1;
end;

{ TTreeItemNode }

constructor TTreeChapterNode.Create(AOwner: TTreeNodes);
begin
  inherited;
  ImageIndex := 2;
  SelectedIndex := 2;
end;

{ TForm1 }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True; //Makes images drag around smoothly
  ScrollBox1.DoubleBuffered := True;

  Screen.Cursors[crHandMove] := LoadCursor(hInstance, 'CR_HAND');

  fExePath := ExtractFilePath(ParamStr(0));
  fCampaignsPath := ExpandFileName(fExePath + '..\..\Campaigns\');

  Locale := 'eng';

  C := TKMCampaign.Create;

  fSelectedChapter := nil;
  fSelectedMission := nil;
  fSelectedNode := nil;

  imgNode.Canvas.Font.Name := 'Verdana';
  imgNode.Canvas.Font.Style := [fsBold];
  imgNode.Canvas.Font.Size := 5;
  imgNode.Canvas.Font.Color := clWhite;

  imgBlackFlag.Canvas.Font.Name := 'Verdana';
  imgBlackFlag.Canvas.Font.Style := [fsBold];
  imgBlackFlag.Canvas.Font.Size := 8;
  imgBlackFlag.Canvas.Font.Color := clWhite;

  imgRedFlag.Canvas.Font.Name := 'Verdana';
  imgRedFlag.Canvas.Font.Style := [fsBold];
  imgRedFlag.Canvas.Font.Size := 8;
  imgRedFlag.Canvas.Font.Color := clWhite;

  fSprites := TKMSpritePackEdit.Create(rxCustom, nil);

  if FileExists(ParamStr(1)) then
    LoadCmp(ParamStr(1));
  UpdateCaption;

  FRender := TRenderPanel.Create(ScrollBox1, tvList);
  FRender.PopupMenu := pmMap;
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fSprites.Free;
end;

procedure TMainForm.aAddChapterExecute(Sender: TObject);
var
  Node: TTreeNode;
begin
  FTreeItemCreate := TTreeChapter;
  Node := tvList.Items.AddChild(nil, Format('Chapter %d', [tvList.Items.Count + 1]));
  Node.Selected := True;
end;

procedure TMainForm.LoadCmp(aFileName : String);
var
  I: Integer;
begin
  C.LoadFromFile(aFileName);

  fSprites.Free;
  fSprites := TKMSpritePackEdit.Create(rxCustom, nil);
  if FileExists(ExtractFilePath(dlgOpenCampaign.FileName) + 'images.rxx') then
    fSprites.LoadFromRXXFile(ExtractFilePath(dlgOpenCampaign.FileName) + 'images.rxx')
  else
    ShowMessage('Campaign background image (images.rxx) could not be found');
end;


procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var Img: TImage;
begin
  {
  Img := nil;

  if (fSelectedMap <> -1) then
    if (fSelectedNode <> -1) then
      Img := imgNodes[fSelectedNode]
    else
      Img := imgFlags[fSelectedMap];

  if Img = nil then Exit;

  case Key of
    Ord('D'): Img.Left := Img.Left + 1;
    Ord('A'): Img.Left := Img.Left - 1;
    Ord('W'): Img.Top  := Img.Top  - 1;
    Ord('S'): Img.Top  := Img.Top  + 1;
  end;
  Img.Left := EnsureRange(Img.Left, iMap.Left, iMap.Left + 1024-Img.Width);
  Img.Top  := EnsureRange(Img.Top, iMap.Top, iMap.Top + 768-Img.Height);
  if (fSelectedNode <> -1) then
  begin
    //Position node centers, so that if someone changes the nodes they still look correct
    C.Maps[fSelectedMap].Nodes[fSelectedNode].X := Img.Left - iMap.Left + Img.Width div 2;
    C.Maps[fSelectedMap].Nodes[fSelectedNode].Y := Img.Top  - iMap.Top + Img.Height div 2;
  end
  else
  begin
    C.Maps[fSelectedMap].Flag.X := Img.Left - iMap.Left;
    C.Maps[fSelectedMap].Flag.Y := Img.Top  - iMap.Top;
  end;

  }
  StatusBar1.Panels[1].Text := 'Position ' + IntToStr(Img.Left) + 'x' + IntToStr(Img.Top);
end;


procedure TMainForm.FlagDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
{
  if Button = mbLeft then
  begin
    if ssShift in Shift then
      AddMission(TImage(Sender).Left, TImage(Sender).Top)
    else
    begin
      fSelectedMap := TImage(Sender).Tag;
      SelectMap;
    end;
    fSelectedNode := -1;
    PrevX := X;
    PrevY := Y;
  end;
}
end;

procedure TMainForm.FlagEnter(Sender: TObject);
begin
  StatusBar1.Panels[2].Text := 'Map #' + IntToStr(TImage(Sender).Tag + 1);
end;

procedure TMainForm.FlagMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Img: TImage;
begin
{
  if (ssLeft in Shift) and (TImage(Sender).Tag = fSelectedMap) then
  begin
    Img := TImage(Sender);
    Assert(Img <> nil);

    Img.Left := EnsureRange(Img.Left + (X - PrevX), iMap.Left, iMap.Left + 1024-Img.Width);
    Img.Top  := EnsureRange(Img.Top  + (Y - PrevY), iMap.Top, iMap.Top + 768-Img.Height);

    C.Maps[fSelectedMap].Flag.X := Img.Left - iMap.Left;
    C.Maps[fSelectedMap].Flag.Y := Img.Top  - iMap.Top;

    StatusBar1.Panels[1].Text := 'Position ' + IntToStr(Img.Left) + 'x' + IntToStr(Img.Top);
  end;
}
end;


procedure TMainForm.NodeDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
{
  if Button = mbLeft then
  begin
    fSelectedNode := TImage(Sender).Tag;
    SelectMap;

    PrevX := X;
    PrevY := Y;
  end;
}
end;


procedure TMainForm.NodeEnter(Sender: TObject);
begin
  StatusBar1.Panels[2].Text := 'Node #' + IntToStr(TImage(Sender).Tag + 1);
end;


procedure TMainForm.FlagNodeLeave(Sender: TObject);
begin
  StatusBar1.Panels[2].Text := '';
end;


procedure TMainForm.NodeMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Img: TImage;
begin
  {
  if (ssLeft in Shift) and (fSelectedMap <> -1) and (fSelectedNode <> -1) then
  begin
    Img := TImage(Sender);
    Assert(Img <> nil);

    Img.Left := EnsureRange(Img.Left + (X - PrevX), iMap.Left, iMap.Left + 1024-Img.Width);
    Img.Top  := EnsureRange(Img.Top  + (Y - PrevY), iMap.Top, iMap.Top + 768-Img.Height);

    C.Maps[fSelectedMap].Nodes[fSelectedNode].X := Img.Left + Img.Width div 2  - iMap.Left;
    C.Maps[fSelectedMap].Nodes[fSelectedNode].Y := Img.Top  + Img.Height div 2 - iMap.Top;

    StatusBar1.Panels[1].Text := 'Position ' + IntToStr(Img.Left) + 'x' + IntToStr(Img.Top);
  end;
  }
end;

procedure TMainForm.pmMapPopup(Sender: TObject);
begin
  aAddMission.Enabled := Assigned(fSelectedChapter);
  aAddNode.Enabled := Assigned(fSelectedMission);
end;

function TMainForm.GetCharset(aLang: string): TFontCharset;
begin
  if Pos(aLang, 'bel,rus,bul,ukr') <> 0 then
    Result := RUSSIAN_CHARSET
  else if Pos(aLang, 'pol,hun,cze,svk,rom') <> 0 then
    Result := EASTEUROPE_CHARSET
  else if Pos(aLang, 'tur') <> 0 then
    Result := TURKISH_CHARSET
  else if Pos(aLang, 'lit,lat') <> 0 then
    Result := BALTIC_CHARSET
  else if Pos(aLang, 'eng,spa,ita,nor,chn,dut,est,ptb,fre,ger,jpn,swe') <> 0 then
    Result := ANSI_CHARSET
  else
    Result := DEFAULT_CHARSET;
end;


procedure TMainForm.LoadCampaignName(aFileName, aLocale: string);
var
  LibxFile: TStringList;
  I : Integer;
  VarText : String;
  VarIndex: Integer;
begin
  if not FileExists(Format(aFileName, [aLocale])) then Exit;
  LibxFile := TStringList.Create;
  LibxFile.LoadFromFile(Format(aFileName, [aLocale]));

  for I := 0 to LibxFile.Count - 1 do
  begin
    VarText := LibxFile.Strings[I];
    VarIndex := Pos('0:', VarText);
    if VarIndex > 0 then
    begin
      C.FullName := Copy(VarText, VarIndex + 2, Length(VarText));
      Break;
    end;
  end;
  LibxFile.Free;
end;


procedure TMainForm.SaveCampaignName(aFileName: string);
var
  LibxFile: TStringList;
  I: Integer;
begin
  LibxFile := TStringList.Create;
  LibxFile.LoadFromFile(aFileName);
  for I := 0 to LibxFile.Count - 1 do
    if Pos('0:', LibxFile.Strings[I]) > 0 then
    begin
      LibxFile.Strings[I] := '0:' + C.FullName;//fCampaignSettings.edtName.Text;
      Break;
    end;
  LibxFile.SaveToFile(aFileName);
  LibxFile.Free;
end;


procedure TMainForm.CreateDefaultLocaleLibxTemplate(aFileName: string);
var
  LibxFile: TextFile;
  I: Integer;
begin
  AssignFile(LibxFile, aFileName);
  try
    ReWrite(LibxFile);

    Writeln(LibxFile, '');
    Writeln(LibxFile, 'MaxID:' + IntToStr(C.MapCount + 9) + EolW);
    Writeln(LibxFile, '0:' + C.FullName);
    Writeln(LibxFile, '1:Mission %d');
    Writeln(LibxFile, '2:Campaign description');
    for I := 0 to C.MapCount-1 do
      Writeln(LibxFile, IntToStr(10 + I) + ':Mission description ' + IntToStr(I + 1));
  finally
    CloseFile(LibxFile);
  end;
end;


procedure TMainForm.DrawNodeNumber(aIndexNode: Integer);
var
  txtWiMainFormtxtHeight, txtLeft, txtTop: Integer;
begin
  {
  if not cbShowNodeNumbers.Checked then Exit;

  txtWidth := imgNodes[aIndexNode].Canvas.TextWidth(IntToStr(aIndexNode +1));
  txtHeight := imgNodes[aIndexNode].Canvas.TextHeight(IntToStr(aIndexNode +1));
  txtLeft := (imgNodes[aIndexNode].Width - txtWidth) div 2;
  txtTop := (imgNodes[aIndexNode].Height - txtHeight) div 2;

  SetBkMode(imgNodes[aIndexNode].Canvas.Handle, TRANSPARENT);
  imgNodes[aIndexNode].Canvas.TextOut(txtLeft, txtTop, IntToStr(aIndexNode +1));
  }
end;

procedure TMainForm.DrawFlagNumber(aIndexMap: Integer);
const
  OFF: array [Boolean] of TPoint = ((X:-3; Y:-2), (X:-1; Y:-2));
var
  txtWidth, txtHeight, txtLeft, txtTop: Integer;
  isRedFlag: Boolean;
begin
  {
  if not cbShowNodeNumbers.Checked then Exit;

  isRedFlag := aIndexMap <= fSelectedMap;

  txtWidth := imgFlags[aIndexMap].Canvas.TextWidth(IntToStr(aIndexMap +1));
  txtHeight := imgFlags[aIndexMap].Canvas.TextHeight(IntToStr(aIndexMap +1));
  txtLeft := (imgFlags[aIndexMap].Width - txtWidth) div 2 + OFF[isRedFlag].X;
  txtTop := (imgFlags[aIndexMap].Height - txtHeight) div 2 + OFF[isRedFlag].Y;

  SetBkMode(imgFlags[aIndexMap].Canvas.Handle, TRANSPARENT);
  imgFlags[aIndexMap].Canvas.TextOut(txtLeft, txtTop, IntToStr(aIndexMap + 1));
  }
end;

function TMainForm.DlgQuestionShow(aCaption, aMsg: string): boolean;
var
  VarBool: boolean;
begin
  VarBool := false;
  {$IFDEF MSWindows}
  if MessageBox(Handle, PChar(aCaption), PChar(aMsg), MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON2) = ID_YES then
    VarBool := true
  else
    VarBool := false;
  {$ENDIF}
  {$IFDEF Unix}
  if MessageDlg(aCaption, aMsg, mtConfirmation, [mbYes, mbNo], 0, mbNo) = mrYes then
    VarBool := true
  else
    VarBool := false;
  {$ENDIF}
  Result := VarBool;
end;

procedure TMainForm.AddMission(X, Y: Integer);
begin
   {
  if fUpdatingMainForm
    Exit;
  if C.MapCount = MAX_CAMP_MAPS then
  begin
    ShowMessage('Mission limit exceeded!');
    Exit;
  end;

  C.MapCount := EnsureRange(C.MapCount + 1, 1, MAX_CAMP_MAPS);
  C.Maps[C.MapCount - 1].Flag.X := EnsureRange(X, 0, 1024 - imgRedFlag.Width);
  C.Maps[C.MapCount - 1].Flag.Y := EnsureRange(Y, 0, 768 - imgRedFlag.Height);
       }
  //fSelectedMap := C.MapCount - 1;

end;

procedure TMainForm.aAddMissionExecute(Sender: TObject);
var
  Node: TTreeNode;
begin

  FTreeItemCreate := TTreeChapterMission;
  Node := tvList.Items.AddChild(fSelectedChapter, Format('Mission %d', [C.MapCount + 1]));
  Node.Selected := True;

{
  AddMission(
    FMousePosition.X + ScrollBox1.HorzScrollBar.Position - imgRedFlag.Width div 2,
    FMousePosition.Y + ScrollBox1.VertScrollBar.Position - imgRedFlag.Height
  );
}
end;

procedure TMainForm.aAddNodeExecute(Sender: TObject);
var
  curItem: Integer;
begin
  if fUpdating then
    Exit;
{
  if C.MapCount = C.Maps[].NodeCount then
  begin
    ShowMessage('Node limit exceeded!');
    Exit;
  end;

  curItem := C.Maps[fSelectedMap].NodeCount;

  C.Maps[fSelectedMap].NodeCount := EnsureRange(curItem + 1, 0, MAX_CAMP_NODES);

  C.Maps[fSelectedMap].Nodes[curItem].X := EnsureRange(FMousePosition.X + ScrollBox1.HorzScrollBar.Position - imgNode.Width div 2, 0, 1024 - imgNode.Width);
  C.Maps[fSelectedMap].Nodes[curItem].Y := EnsureRange(FMousePosition.Y + ScrollBox1.VertScrollBar.Position - imgNode.Height div 2, 0, 768 - imgNode.Height);

  fSelectedNode := C.Maps[fSelectedMap].NodeCount - 1;
}
end;

procedure TMainForm.aNewExecute(Sender: TObject);
var
  I: Integer;
begin
  if DlgQuestionShow('Unsaved data will be lost. Are you sure?', Self.Caption) then
  begin
    C.Free;
    fSprites.Free;

    C := TKMCampaign.Create;
    fSprites := TKMSpritePackEdit.Create(rxCustom, nil);
    //fSelectedMap := -1;

    //for I := 0 to Length(imgNodes) - 1 do
    //  imgNodes[I].Visible := False;
    UpdateCaption;
  end;
end;

procedure TMainForm.aOpenExecute(Sender: TObject);
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(fExePath + '..\..\KaM_Remake_Settings.ini');
  Locale := Ini.ReadString('Game', 'Locale', 'eng');
  Ini.Free;

  if DirectoryExists(fCampaignsPath) then
    dlgOpenCampaign.InitialDir := fCampaignsPath
  else
    dlgOpenCampaign.InitialDir := fExePath;

  if not dlgOpenCampaign.Execute then
    Exit;

  LoadCmp(dlgOpenCampaign.FileName);

  LoadCampaignName(ExtractFilePath(dlgOpenCampaign.FileName) + TEMPLATE_LIBX_FILE_TEXT, Locale);
  if Length(C.FullName) = 0 then
    LoadCampaignName(ExtractFilePath(dlgOpenCampaign.FileName) + TEMPLATE_LIBX_FILE_TEXT , 'eng');
  UpdateCaption;
  CampaignToList;
  FRender.RefreshBackground;
  FRender.Repaint;
end;

procedure TMainForm.aOpenPictureExecute(Sender: TObject);
begin
  dlgOpenPicture.InitialDir := ExtractFilePath(dlgOpenCampaign.FileName);

  if not dlgOpenPicture.Execute then Exit;
  try
    fSprites.AddImage(ExtractFilePath(dlgOpenPicture.FileName),
                      ExtractFileName(dlgOpenPicture.FileName), 1);
    FRender.RefreshBackground;
    FRender.Repaint;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TMainForm.aSaveExecute(Sender: TObject);
begin
  ListToCampaign;
  if C.MapCount < 2 then
  begin
    ShowMessage('Campaign must have at least 2 missions');
    Exit;
  end;

  if Length(Trim(C.CampName)) <> 3 then
  begin
    ShowMessage('Campaign short title must be 3 characters');
    Exit;
  end;

  dlgSaveCampaign.InitialDir := ExtractFilePath(dlgOpenCampaign.FileName);

  dlgSaveCampaign.FileName := 'info';

  if not dlgSaveCampaign.Execute then
    Exit;

  C.SaveToFile(dlgSaveCampaign.FileName);
  fSprites.SaveToRXXFile(ExtractFilePath(dlgSaveCampaign.FileName) + 'images.rxx');

  if FileExists(ExtractFilePath(dlgSaveCampaign.FileName) +
  Format(TEMPLATE_LIBX_FILE_TEXT, [Locale])) then
    SaveCampaignName(ExtractFilePath(dlgSaveCampaign.FileName) +
      Format(TEMPLATE_LIBX_FILE_TEXT, [Locale]))
  else
    CreateDefaultLocaleLibxTemplate(ExtractFilePath(dlgSaveCampaign.FileName) +
      Format(TEMPLATE_LIBX_FILE_TEXT, [Locale]));
end;

procedure TMainForm.aSettingsExecute(Sender: TObject);
var
  formSettings: TfCampaignSettings;
  cmp: TKMCampaignId;
begin
  if fUpdating then
    Exit;
  formSettings := TfCampaignSettings.Create(Self);
  try
    formSettings.edtName.Font.Charset := GetCharset(Locale);
    formSettings.edtName.Text := C.FullName;
    formSettings.edtShortName.Text := StringReplace(C.CampName, #0, '', [rfReplaceAll, rfIgnoreCase]);
    formSettings.edtIntroVideo.Text := C.IntroVideo;

    if formSettings.ShowModal = mrOk then
    begin
      C.FullName := formSettings.edtName.Text;
      cmp[0] := Ord(formSettings.edtShortName.Text[1]);
      cmp[1] := Ord(formSettings.edtShortName.Text[2]);
      cmp[2] := Ord(formSettings.edtShortName.Text[3]);
      C.CampaignId := cmp;
      C.IntroVideo := formSettings.edtIntroVideo.Text;
      UpdateCaption;
    end;
  finally
    formSettings.Free;
  end;
end;


procedure TMainForm.UpdateCaption;
begin
  Caption := 'Campaign Build (' + GAME_REVISION + ') ' + StringReplace(C.CampName, #0, '_', [rfReplaceAll, rfIgnoreCase]) + ' - ' + C.FullName;
end;

procedure TMainForm.CampaignToList;
var
  I, K, J: Integer;
  N, S: TTreeNode;
  VideoType: TMissionVideoTypes;
begin
  fUpdating := True;
  tvList.Items.BeginUpdate;
  try
    tvList.Items.Clear;

    for J := 0 to High(C.Chapters) do
    begin
      FTreeItemCreate := TTreeChapter;
      fSelectedChapter := tvList.Items.AddChild(nil, Format('Chapter %d', [J + 1])) as TTreeChapter;
      fSelectedChapter.ImageIndex := C.Chapters[J].ImageIndex;
      for I := 0 to C.Chapters[J].MapCount - 1 do
      begin
        FTreeItemCreate := TTreeChapterMission;
        fSelectedMission := tvList.Items.AddChild(fSelectedChapter, Format('Mission %d', [I + 1])) as TTreeChapterMission;
        fSelectedMission.TextPos := C.Chapters[J].Maps[I].TextPos;
        fSelectedMission.Position := C.Chapters[J].Maps[I].Flag;
        for VideoType := Low(TMissionVideoTypes) to High(TMissionVideoTypes) do
          fSelectedMission.Video[VideoType] := C.Chapters[J].Maps[I].Video[VideoType];
        for K := 0 to C.Chapters[J].Maps[I].NodeCount - 1 do
        begin
          FTreeItemCreate := TTreeChapterNode;
          fSelectedNode := tvList.Items.AddChild(fSelectedMission, Format('node %d', [K + 1])) as TTreeChapterNode;
          fSelectedNode.Position := C.Chapters[J].Maps[I].Nodes[K];
        end;
      end;
    end;
  finally
    tvList.Items.EndUpdate;
    fUpdating := False;
  end;
end;

procedure TMainForm.ListToCampaign;
var
  i, k: Integer;
  ChapterCount: Byte;
  node: TTreeNode;
  VideoType: TMissionVideoTypes;
begin
  fUpdating := True;
  try
    ChapterCount := 0;
    node := tvList.Items.GetFirstNode;
    while node <> nil do
    begin
      Inc(ChapterCount);
      node := node.GetNextSibling;
    end;

    SetLength(C.Chapters, ChapterCount);
    node := tvList.Items.GetFirstNode;
    while node <> nil do
    begin
      C.Chapters[node.Index].ImageIndex := (node as TTreeChapter).ImageIndex;
      C.Chapters[node.Index].MapCount := node.Count;
      SetLength(C.Chapters[node.Index].Maps, node.Count);
      for i := 0 to node.Count - 1 do
      begin
        C.Chapters[node.Index].Maps[i].Flag := (node[i] as TTreeChapterMission).Position;
        C.Chapters[node.Index].Maps[i].TextPos := (node[i] as TTreeChapterMission).TextPos;
        for VideoType := Low(TMissionVideoTypes) to High(TMissionVideoTypes) do
          C.Chapters[node.Index].Maps[i].Video[VideoType] := (node[i] as TTreeChapterMission).Video[VideoType];

        C.Chapters[node.Index].Maps[i].NodeCount := node[i].Count;
        for k := 0 to node[i].Count - 1 do
          C.Chapters[node.Index].Maps[i].Nodes[k] := (node[i][k] as TTreeChapterNode).Position;
      end;
      node := node.GetNextSibling;
    end;

  finally
    fUpdating := False;
  end;
end;

procedure TMainForm.tvListChange(Sender: TObject; Node: TTreeNode);
begin
  if fUpdating then
    Exit;

  fSelectedChapter := nil;
  fSelectedMission := nil;
  fSelectedNode := nil;

  case Node.Level of
    0: fSelectedChapter := Node as TTreeChapter;
    1: begin
        fSelectedChapter := Node.Parent as TTreeChapter;
        fSelectedMission := Node as TTreeChapterMission;
      end;
    2: begin
        fSelectedChapter := Node.Parent.Parent as TTreeChapter;
        fSelectedMission := Node.Parent as TTreeChapterMission;
        fSelectedNode := Node as TTreeChapterNode;
      end;
  end;
end;

procedure TMainForm.tvListCreateNodeClass(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
begin
  NodeClass := FTreeItemCreate;
end;

procedure TMainForm.tvListDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  AnItem: TTreeNode;
  AttachMode: TNodeAttachMode;
  Rect: TRect;
begin
  if tvList.Selected = nil then
    Exit;

  AnItem := tvList.GetNodeAt(X, Y);
  if Assigned(AnItem) then
  begin
    Rect := AnItem.DisplayRect(False);
    if Y > Rect.Top + Rect.Height div 2 then
      AttachMode := naAdd
    else
      AttachMode := naInsert;
    tvList.Selected.MoveTo(AnItem, AttachMode);
  end;
  tvList.Repaint;
end;

procedure TMainForm.tvListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Node, SelNode : TTreeNode;
  Rect: TRect;
  Position: Integer;
begin
  Node := tvList.GetNodeAt(x, y);
  SelNode := tvList.Selected;
  Accept := (Sender = Source) and ((Node <> nil) and (Node.Level = SelNode.Level));
  if Accept then
  begin
    tvList.Repaint;
    Rect := Node.DisplayRect(False);

    if Y > Rect.Top + Rect.Height div 2 then
      Position := Rect.Bottom
    else
      Position := Rect.Top;

    tvList.Canvas.MoveTo(Rect.Left, Position);
    tvList.Canvas.LineTo(Rect.Right, Position);
  end;
end;

end.
