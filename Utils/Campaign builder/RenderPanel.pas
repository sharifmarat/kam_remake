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
    FTimer: TTimer;
    FRepaintTimer: TTimer;
    FRect: TRect;
    FTree: TTreeView;
    FMousePosition: TPoint;
    FMouseMovePosition: TPoint;
    FMouseCameraPosition: TPoint;
    FMouseMove: Boolean;
    FMouseMoveNode: TTreeNode;
    FBackground: TBitmap;
    FBackgroundIndex: Integer;
    FCamera: TPoint;
    FZoom: Single;
    procedure CnCtlColorStatic (var Msg: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure WmEraseBkgnd (var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure DrawImage(const APosition: TKMPoint; AImage: TImage); overload;
    procedure DrawImage(const ARect: TRect; AImage: TImage); overload;
    procedure DrawImage(const APosition: TKMPointW; ABitmap: TBitmap); overload;
    procedure DrawFocus(const ARect: TRect);
    procedure DrawText(const APosition: TPoint; const AText: String); overload;
    procedure DrawText(const ARect: TRect; const AText: String); overload;
    function PointInRect(const APoint: TPoint; const ARect: TRect): Boolean;
    function RectMove(const ARect: TRect; const APosition: TPoint): TRect;
    procedure Timer(Sender: TObject);
    procedure RepaintTimer(Sender: TObject);
    function ToDrawRect(const APosition: TKMPoint; AGraphic: TGraphic): TRect; overload;
    function ToDrawRect(const ARect: TRect): TRect; overload;
    procedure RefreshCamera;
  protected
    procedure MouseWheel(var AMessage: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; ATree: TTreeView);
    destructor Destroy; override;
    procedure RefreshBackground(AIndex: Integer = 0);
    procedure KeyDown(var Key: Word; Shift: TShiftState);
    procedure Repaint; override;
    property MouseCameraPosition: TPoint read FMouseCameraPosition;
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
  FTree := ATree;

  FRect := Rect(0, 0, 1024, 768);

  DoubleBuffered := True;
  Parent := AOwner as TWinControl;
  Align := alClient;
  FBackground := TBitmap.Create;
  FBackground.Transparent := True;
  FBackgroundIndex := 1;
  FCamera := Point(0, 0);
  FZoom := 1;
  FMouseMoveNode := nil;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 230;
  FTimer.OnTimer := Timer;

  FRepaintTimer := TTimer.Create(Self);
  FRepaintTimer.Enabled := False;
  FRepaintTimer.Interval := 32;
  FRepaintTimer.OnTimer := RepaintTimer;
end;

destructor TRenderPanel.Destroy;
begin
  FBackground.Free;

  inherited;
end;

procedure TRenderPanel.Repaint;
begin
  FRepaintTimer.Enabled := True;
end;

procedure TRenderPanel.RepaintTimer(Sender: TObject);
begin
  FRepaintTimer.Enabled := False;
  inherited Repaint;
end;

procedure TRenderPanel.KeyDown(var Key: Word; Shift: TShiftState);
var
  Node: TTreeChapterItem;
begin
  Node := MainForm.SelectedMission;
  if Assigned(MainForm.SelectedNode) then
    Node := MainForm.SelectedNode;

  if Assigned(Node) then
  begin
    case Key of
      Ord('A'): Node.Rect := RectMove(Node.Rect, Point(EnsureRange(Node.Rect.Left - 1, 0, 1024 - Node.Rect.Width), Node.Rect.Top));
      Ord('D'): Node.Rect := RectMove(Node.Rect, Point(EnsureRange(Node.Rect.Left + 1, 0, 1024 - Node.Rect.Width), Node.Rect.Top));
      Ord('W'): Node.Rect := RectMove(Node.Rect, Point(Node.Rect.Left, EnsureRange(Node.Rect.Top - 1, 0, 768 - Node.Rect.Height)));
      Ord('S'): Node.Rect := RectMove(Node.Rect, Point(Node.Rect.Left, EnsureRange(Node.Rect.Top + 1, 0, 768 - Node.Rect.Height)));
    end;
    Repaint;
  end;
end;

procedure TRenderPanel.Timer(Sender: TObject);
begin
  FMouseMove := True;
  FTimer.Enabled := False;
  Repaint;
end;

procedure TRenderPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ssShift in Shift then
  begin
    FMouseMove := Button = mbLeft;
  end
  else
  begin
    FTimer.Enabled := False;
    FTimer.Enabled := Button = mbLeft;
  end;
  FMousePosition := Point(X, Y);

  if Assigned(FMouseMoveNode) then
  begin
    if FMouseMove and (ssShift in Shift) then
      FMouseMoveNode := MainForm.CopyNode(FMouseMoveNode as TTreeChapterItem);
    FMouseMoveNode.Selected := True;
  end;
end;

procedure TRenderPanel.MouseWheel(var AMessage: TWMMouseWheel);
begin
  if (AMessage.WheelDelta > 0) and (FZoom > 0.2) then
    FZoom := FZoom * 0.95
  else
    if (AMessage.WheelDelta < 0) and (FZoom < 10) then
      FZoom := FZoom * 1.05;
  Repaint;
end;

procedure TRenderPanel.MouseMove(Shift: TShiftState; X, Y: Integer);

  function InNode(ANode: TTreeNode; AImage: TImage): TTreeNode;
  var
    i: Integer;
    Rect: TRect;
    Node: TTreeChapterItem;
  begin
    for i := 0 to ANode.Count - 1 do
      if ANode[i] is TTreeChapterItem then
      begin
        Node := TTreeChapterNode(ANode[i]);
        if PointInRect(FMouseCameraPosition, Node.Rect) then
        begin
          FMouseMovePosition.X := FMouseCameraPosition.X - Node.Rect.Left;
          FMouseMovePosition.Y := FMouseCameraPosition.Y - Node.Rect.Top;
          Exit(Node);
        end;
      end;
  end;

var
  i: Integer;
  Node: TTreeChapterItem;
  MoveNode: TTreeNode;
begin
  inherited;
  FMouseCameraPosition := Point(Round(X / FZoom + FCamera.X), Round(Y / FZoom + FCamera.Y));
  if FMouseMove then
  begin
    if Assigned(FMouseMoveNode) then
    begin
      if FMouseMoveNode is TTreeChapterItem then
      begin
        Node := TTreeChapterItem(FMouseMoveNode);
        Node.Rect := RectMove(Node.Rect, Point(
          EnsureRange(FMouseCameraPosition.X - FMouseMovePosition.X, 0, 1024 - Node.Rect.Width),
          EnsureRange(FMouseCameraPosition.Y - FMouseMovePosition.Y, 0, 768 - Node.Rect.Height)
        ));
        Repaint;
      end;
    end
    else
    begin
      FCamera.X := FCamera.X + Round((FMousePosition.X - X) / FZoom);
      FCamera.Y := FCamera.Y + Round((FMousePosition.Y - Y) / FZoom);
      RefreshCamera;
      Repaint;
    end;
  end
  else
  begin
    MoveNode := nil;
    if Assigned(MainForm.SelectedMission) then
      MoveNode := InNode(MainForm.SelectedMission, MainForm.imgNode);

    if not Assigned(MoveNode) and Assigned(MainForm.SelectedChapter) then
      MoveNode := InNode(MainForm.SelectedChapter, MainForm.imgRedFlag);

    if MoveNode <> FMouseMoveNode then
    begin
      FMouseMoveNode := MoveNode;
      Repaint;
    end;
  end;
  FMousePosition := Point(X, Y);

  MainForm.StatusBar1.Panels[1].Text := IntToStr(FMouseCameraPosition.X) + 'x' + IntToStr(FMouseCameraPosition.Y);

end;

procedure TRenderPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FTimer.Enabled := False;
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
  Canvas.Rectangle(-10, -10, Width + 10, Height + 10);
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
        begin
          DrawImage(NodeMission.Rect, MainForm.imgRedFlag);
          if MainForm.cbShowNodeNumbers.Checked then
          begin
            Canvas.Font := MainForm.imgRedFlag.Canvas.Font;
            DrawText(Point(
                NodeMission.Rect.Left + MainForm.imgRedFlag.Width div 2 - 1,
                NodeMission.Rect.Top + MainForm.imgRedFlag.Height div 2 - 2
              ), IntToStr(NodeMission.Number));
          end;
        end
        else
        begin
          DrawImage(NodeMission.Rect, MainForm.imgBlackFlag);
          if MainForm.cbShowNodeNumbers.Checked then
          begin
            Canvas.Font := MainForm.imgBlackFlag.Canvas.Font;
            DrawText(Point(
                NodeMission.Rect.Left + MainForm.imgBlackFlag.Width div 2 - 3,
                NodeMission.Rect.Top + MainForm.imgBlackFlag.Height div 2
              ), IntToStr(NodeMission.Number));
          end;
        end;
      end;

    if Assigned(MainForm.SelectedMission) then
    begin
      for i := 0 to MainForm.SelectedMission.Count - 1 do
        if MainForm.SelectedMission[i] is TTreeChapterNode then
        begin
          NodeMissionNode := TTreeChapterNode(MainForm.SelectedMission[i]);
          DrawImage(NodeMissionNode.Rect, MainForm.imgNode);

          if MainForm.cbShowNodeNumbers.Checked then
          begin
            Canvas.Font := MainForm.imgNode.Canvas.Font;
            DrawText(NodeMissionNode.Rect, IntToStr(NodeMissionNode.Index + 1));
          end;
        end;

      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clGreen;
    end;
  end;

  if Assigned(FMouseMoveNode) and (FMouseMoveNode is TTreeChapterItem) and not FMouseMove then
      DrawFocus(TTreeChapterItem(FMouseMoveNode).Rect);

  if Assigned(MainForm.SelectedMission) and MainForm.cbShowBriefingPage.Checked then
    case MainForm.SelectedMission.TextPos of
      bcBottomRight: DrawImage(KMPoint(FRect.Width - MainForm.ImgBriefing.Width, FRect.Height - MainForm.ImgBriefing.Height), MainForm.ImgBriefing);
      bcBottomLeft: DrawImage(KMPoint(0, FRect.Height - MainForm.ImgBriefing.Height), MainForm.ImgBriefing);
    end;

end;

function TRenderPanel.ToDrawRect(const APosition: TKMPoint; AGraphic: TGraphic): TRect;
begin
  Result.Left := Round((APosition.X - FCamera.X) * FZoom);
  Result.Top := Round((APosition.Y - FCamera.Y) * FZoom);
  Result.Right := Round(Result.Left + AGraphic.Width * FZoom);
  Result.Bottom := Round(Result.Top + AGraphic.Height * FZoom);
end;

function TRenderPanel.ToDrawRect(const ARect: TRect): TRect;
begin
  Result.Left := Round((ARect.Left - FCamera.X) * FZoom);
  Result.Top := Round((ARect.Top - FCamera.Y) * FZoom);
  Result.Right := Round(Result.Left + ARect.Width * FZoom);
  Result.Bottom := Round(Result.Top + ARect.Height * FZoom);
end;

procedure TRenderPanel.DrawText(const APosition: TPoint; const AText: String);
var
  Size: Integer;
begin
  Size := Canvas.Font.Size;
  Canvas.Font.Size := Round(Size * FZoom);
  SetBkMode(Canvas.Handle, TRANSPARENT);
  Canvas.TextOut(
    Round((APosition.X - FCamera.X) * FZoom) - Canvas.TextWidth(AText) div 2,
    Round((APosition.Y - FCamera.Y) * FZoom) - Canvas.TextHeight(AText) div 2,
    AText
  );
  Canvas.Font.Size := Size;
end;

procedure TRenderPanel.DrawText(const ARect: TRect; const AText: String);
begin
  DrawText(Point(ARect.Left + ARect.Width div 2, ARect.Top + ARect.Height div 2), AText);
end;

procedure TRenderPanel.DrawFocus(const ARect: TRect);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clLime;
  Canvas.Rectangle(
    Round((ARect.Left - FCamera.X) * FZoom),
    Round((ARect.Top - FCamera.Y) * FZoom),
    Round((ARect.Right - FCamera.X) * FZoom),
    Round((ARect.Bottom - FCamera.Y) * FZoom)
  );
end;

procedure TRenderPanel.DrawImage(const APosition: TKMPoint; AImage: TImage);
begin
  Canvas.StretchDraw(ToDrawRect(APosition, AImage.Picture.Graphic), AImage.Picture.Graphic);
end;

procedure TRenderPanel.DrawImage(const ARect: TRect; AImage: TImage);
begin
  Canvas.StretchDraw(ToDrawRect(ARect), AImage.Picture.Graphic);
end;

procedure TRenderPanel.DrawImage(const APosition: TKMPointW; ABitmap: TBitmap);
begin
  Canvas.StretchDraw(ToDrawRect(KMPoint(APosition.X, APosition.Y), ABitmap), ABitmap);
  //Canvas.Draw(APosition.X - FCamera.X, APosition.Y - FCamera.Y, ABitmap);
end;

function TRenderPanel.PointInRect(const APoint: TPoint; const ARect: TRect): Boolean;
begin
  Result := (ARect.Left <= APoint.X) and (APoint.X < ARect.Right)
    and (ARect.Top <= APoint.Y) and (APoint.Y < ARect.Bottom);
end;

function TRenderPanel.RectMove(const ARect: TRect; const APosition: TPoint): TRect;
begin
  Result.Left := APosition.X;
  Result.Top := APosition.Y;
  Result.Right := APosition.X + ARect.Width;
  Result.Bottom := APosition.Y + ARect.Height;
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

procedure TRenderPanel.RefreshCamera;
begin
  FCamera.X := EnsureRange(FCamera.X, -448, 1920 - 448 * 2);
  FCamera.Y := EnsureRange(FCamera.Y, -216, 1200 - 216 * 2);
end;

procedure TRenderPanel.Resize;
begin
  inherited;

  RefreshCamera;
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
