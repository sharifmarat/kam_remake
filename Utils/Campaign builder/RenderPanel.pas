unit RenderPanel;

interface

uses
  Winapi.Windows, Vcl.ExtCtrls, System.Classes, Vcl.Controls,
  System.SysUtils, System.Variants, Winapi.Messages, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Forms, Vcl.Graphics, KM_ResSpritesEdit, KM_Points,
  KM_Campaigns, Math;

type
  TRenderPanel = class(TPanel)
  private
    FRect: TRect;
    FTree: TTreeView;
    FMousePosition: TPoint;
    FMouseCameraPosition: TPoint;
    FMouseMove: Boolean;
    FMouseMoveNode: TTreeNode;
    FBackground: TBitmap;
    FBackgroundIndex: Integer;
    FCamera: TPoint;
    procedure CnCtlColorStatic (var Msg: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure WmEraseBkgnd (var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure DrawImage(const APosition: TKMPointW; AImage: TImage); overload;
    procedure DrawImage(const APosition: TKMPoint; AImage: TImage); overload;
    procedure DrawImage(const APosition: TKMPointW; ABitmap: TBitmap); overload;
    procedure DrawFocus(const APosition: TKMPointW; AImage: TImage);
    function PointInRect(const APoint: TPoint; const ARect: TRect): Boolean; overload;
    function PointInRect(const APoint: TPoint; const APosition: TKMPointW; AImage: TImage): Boolean; overload;
  protected
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; ATree: TTreeView);
    destructor Destroy; override;
    procedure RefreshBackground(AIndex: Integer = 0);

  end;

implementation

uses
  Main;

{ TRenderPanel }

constructor TRenderPanel.Create(AOwner: TComponent; ATree: TTreeView);
var
  Panel: TPanel;
begin
  inherited Create(AOwner);
  //FScrollBox := AScrollBox;
  FTree := ATree;

  FRect := Rect(0, 0, 1024, 768);

  DoubleBuffered := True;
  Parent := AOwner as TWinControl;
  Align := alClient;
  FBackground := TBitmap.Create;
  FBackground.Transparent := True;
  FBackgroundIndex := 1;
  FCamera := Point(0, 0);
  FMouseMoveNode := nil;
end;

destructor TRenderPanel.Destroy;
begin
  FBackground.Free;

  inherited;
end;

procedure TRenderPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseMove := Button = mbLeft;
  FMousePosition := Point(X, Y);

  if Assigned(FMouseMoveNode) then
    FMouseMoveNode.Selected := True;
end;

procedure TRenderPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  NodeMission: TTreeChapterMission;
  NodeMissionNode: TTreeChapterNode;
  MoveNode: TTreeNode;
begin
  inherited;
  if FMouseMove then
  begin
    if Assigned(FMouseMoveNode) then
    begin

    end
    else
    begin
      FCamera.X := EnsureRange(FCamera.X + (FMousePosition.X - X), -448, IfThen(Width <= 1920, 1920 - 448 - Width, MaxInt));
      FCamera.Y := EnsureRange(FCamera.Y + (FMousePosition.Y - Y), -216, IfThen(Height <= 1200, 1200 - 216 - Height, MaxInt));
      Repaint;
    end;
  end
  else
  begin
    MoveNode := nil;
    if Assigned(MainForm.SelectedMission) then
    begin
      for i := 0 to MainForm.SelectedMission.Count - 1 do
        if MainForm.SelectedMission[i] is TTreeChapterNode then
        begin
          NodeMissionNode := TTreeChapterNode(MainForm.SelectedMission[i]);
          if PointInRect(FMouseCameraPosition, NodeMissionNode.Position, MainForm.imgNode) then
          begin
            MoveNode := NodeMissionNode;
            Break;
          end;
        end;
    end;

    if not Assigned(FMouseMoveNode) and Assigned(MainForm.SelectedChapter) then
      for i := 0 to MainForm.SelectedChapter.Count - 1 do
        if MainForm.SelectedChapter[i] is TTreeChapterMission then
        begin
          NodeMission := TTreeChapterMission(MainForm.SelectedChapter[i]);
          if PointInRect(FMouseCameraPosition, NodeMission.Position, MainForm.imgRedFlag) then
          begin
            MoveNode := NodeMission;
            Break;
          end;
        end;

    if MoveNode <> FMouseMoveNode then
    begin
      FMouseMoveNode := MoveNode;
      Repaint;
    end;
  end;
  FMousePosition := Point(X, Y);
  FMouseCameraPosition := Point(X + FCamera.X, Y + FCamera.Y);

  MainForm.StatusBar1.Panels[1].Text := IntToStr(FMouseCameraPosition.X) + 'x' + IntToStr(FMouseCameraPosition.Y);

end;

procedure TRenderPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseMove := False;
end;

procedure TRenderPanel.Paint;
var
  i: Integer;
  NodeMission: TTreeChapterMission;
  NodeMissionNode: TTreeChapterNode;
begin
  SetBKMode (Handle, TRANSPARENT);
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clBlack;
  Canvas.Rectangle(0, 0, Width, Height);
  DrawImage(KMPoint(-448, -216), MainForm.ImgBackground);

  //imgBlackFlag       imgRedFlag      imgNode
  if Assigned(FBackground) then
    DrawImage(KMPointW(0, 0), FBackground);

  if Assigned(MainForm.SelectedChapter) then
  begin
    for i := 0 to MainForm.SelectedChapter.Count - 1 do
      if MainForm.SelectedChapter[i] is TTreeChapterMission then
      begin
        NodeMission := TTreeChapterMission(MainForm.SelectedChapter[i]);
        If Assigned(MainForm.SelectedMission) and (MainForm.SelectedMission.Index >= NodeMission.Index) then
          DrawImage(NodeMission.Position, MainForm.imgRedFlag)
        else
          DrawImage(NodeMission.Position, MainForm.imgBlackFlag);
      end;

    if Assigned(MainForm.SelectedMission) then
    begin
      for i := 0 to MainForm.SelectedMission.Count - 1 do
        if MainForm.SelectedMission[i] is TTreeChapterNode then
        begin
          NodeMissionNode := TTreeChapterNode(MainForm.SelectedMission[i]);
          DrawImage(NodeMissionNode.Position, MainForm.imgNode);
        end;

      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clGreen;
      //Rectangle(MainForm.SelectedMission.Position.X, MainForm.SelectedMission.Position.Y, MainForm.imgRedFlag.Width, MainForm.imgRedFlag.Height);
    end;
  end;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := clRed;
  if Assigned(FMouseMoveNode) then
  begin
    if FMouseMoveNode is TTreeChapterMission then
      DrawFocus(TTreeChapterMission(FMouseMoveNode).Position, MainForm.imgRedFlag);

    if FMouseMoveNode is TTreeChapterNode then
      DrawFocus(TTreeChapterNode(FMouseMoveNode).Position, MainForm.imgNode);
  end;

  //Canvas.Rectangle(FMouseCameraPosition.X, FMouseCameraPosition.Y, FMouseCameraPosition.X + 10, FMouseCameraPosition.Y + 10 );


  //Img.Left := EnsureRange(Img.Left + (X - PrevX), iMap.Left, iMap.Left + 1024-Img.Width);
  //Img.Top  := EnsureRange(Img.Top  + (Y - PrevY), iMap.Top, iMap.Top + 768-Img.Height);

  if Assigned(MainForm.SelectedMission) and MainForm.cbShowBriefingPage.Checked then
    case MainForm.SelectedMission.TextPos of
      bcBottomRight: DrawImage(KMPointW(FRect.Width - MainForm.ImgBriefing.Width, FRect.Height - MainForm.ImgBriefing.Height), MainForm.ImgBriefing);
      bcBottomLeft: DrawImage(KMPointW(0, FRect.Height - MainForm.ImgBriefing.Height), MainForm.ImgBriefing);
    end;

end;

procedure TRenderPanel.DrawFocus(const APosition: TKMPointW; AImage: TImage);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clLime;
  Canvas.Rectangle(APosition.X - FCamera.X, APosition.Y - FCamera.Y,
    APosition.X - FCamera.X + AImage.Width, APosition.Y - FCamera.Y + AImage.Height);
end;

procedure TRenderPanel.DrawImage(const APosition: TKMPointW; AImage: TImage);
begin
  Canvas.Draw(APosition.X - FCamera.X, APosition.Y - FCamera.Y, AImage.Picture.Graphic);
end;

procedure TRenderPanel.DrawImage(const APosition: TKMPoint; AImage: TImage);
begin
  Canvas.Draw(APosition.X - FCamera.X, APosition.Y - FCamera.Y, AImage.Picture.Graphic);
end;

procedure TRenderPanel.DrawImage(const APosition: TKMPointW; ABitmap: TBitmap);
begin
  Canvas.Draw(APosition.X - FCamera.X, APosition.Y - FCamera.Y, ABitmap);
end;

function TRenderPanel.PointInRect(const APoint: TPoint; const ARect: TRect): Boolean;
begin
  Result := (ARect.Left <= APoint.X) and (APoint.X < ARect.Right)
    and (ARect.Top <= APoint.Y) and (APoint.Y < ARect.Bottom);
end;

function TRenderPanel.PointInRect(const APoint: TPoint; const APosition: TKMPointW; AImage: TImage): Boolean;
begin
  Result := (APosition.X <= APoint.X) and (APoint.X < APosition.X + AImage.Width)
    and (APosition.Y <= APoint.Y) and (APoint.Y < APosition.Y + AImage.Height);
end;

procedure TRenderPanel.RefreshBackground(AIndex: Integer = 0);
begin
  if (AIndex = 0) or (FBackgroundIndex <> AIndex) then
  begin
    if AIndex > 0 then
      FBackgroundIndex := AIndex;

    MainForm.Sprites.GetImageToBitmap(FBackgroundIndex, FBackground, nil);
  end;
end;

procedure TRenderPanel.Resize;
begin
  inherited;

  FCamera.X := EnsureRange(FCamera.X, -448, IfThen(Width <= 1920, 1920 - 448 - Width, MaxInt));
  FCamera.Y := EnsureRange(FCamera.Y, -216, IfThen(Height <= 1200, 1200 - 216 - Height, MaxInt));
end;

procedure TRenderPanel.WmEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TRenderPanel.CnCtlColorStatic(var Msg: TWMCtlColorStatic);
begin
  SetBKMode (Msg.ChildDC, TRANSPARENT);
  Msg.Result := GetStockObject (NULL_BRUSH);
end;

end.
