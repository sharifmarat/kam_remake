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
    FMouseMovePosition: TPoint;
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
    procedure DrawImage(const ARect: TRect; AImage: TImage); overload;
    procedure DrawImage(const APosition: TKMPointW; ABitmap: TBitmap); overload;
    procedure DrawFocus(const ARect: TRect);
    function PointInRect(const APoint: TPoint; const ARect: TRect): Boolean;
    function RectMove(const ARect: TRect; const APosition: TPoint): TRect;
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
    procedure KeyDown(var Key: Word; Shift: TShiftState);
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
  FMouseMoveNode := nil;
end;

destructor TRenderPanel.Destroy;
begin
  FBackground.Free;

  inherited;
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

procedure TRenderPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseMove := Button = mbLeft;
  FMousePosition := Point(X, Y);

  if Assigned(FMouseMoveNode) then
  begin
    if FMouseMove and (ssShift in Shift) then
      FMouseMoveNode := MainForm.CopyNode(FMouseMoveNode as TTreeChapterItem);
    FMouseMoveNode.Selected := True;
  end;
end;

procedure TRenderPanel.MouseMove(Shift: TShiftState; X, Y: Integer);

  function InNode(ANode: TTreeNode; AImage: TImage): TTreeNode;
  var
    i: Integer;
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
      FCamera.X := EnsureRange(FCamera.X + (FMousePosition.X - X), -448, IfThen(Width <= 1920, 1920 - 448 - Width, MaxInt));
      FCamera.Y := EnsureRange(FCamera.Y + (FMousePosition.Y - Y), -216, IfThen(Height <= 1200, 1200 - 216 - Height, MaxInt));
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
          DrawImage(NodeMission.Rect, MainForm.imgRedFlag)
        else
          DrawImage(NodeMission.Rect, MainForm.imgBlackFlag);
      end;

    if Assigned(MainForm.SelectedMission) then
    begin
      for i := 0 to MainForm.SelectedMission.Count - 1 do
        if MainForm.SelectedMission[i] is TTreeChapterNode then
        begin
          NodeMissionNode := TTreeChapterNode(MainForm.SelectedMission[i]);
          DrawImage(NodeMissionNode.Rect, MainForm.imgNode);
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
      bcBottomRight: DrawImage(KMPointW(FRect.Width - MainForm.ImgBriefing.Width, FRect.Height - MainForm.ImgBriefing.Height), MainForm.ImgBriefing);
      bcBottomLeft: DrawImage(KMPointW(0, FRect.Height - MainForm.ImgBriefing.Height), MainForm.ImgBriefing);
    end;

end;

procedure TRenderPanel.DrawFocus(const ARect: TRect);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Color := clLime;
  Canvas.Rectangle(ARect.Left - FCamera.X, ARect.Top - FCamera.Y, ARect.Right - FCamera.X, ARect.Bottom - FCamera.Y);
end;

procedure TRenderPanel.DrawImage(const APosition: TKMPointW; AImage: TImage);
begin
  Canvas.Draw(APosition.X - FCamera.X, APosition.Y - FCamera.Y, AImage.Picture.Graphic);
end;

procedure TRenderPanel.DrawImage(const APosition: TKMPoint; AImage: TImage);
begin
  Canvas.Draw(APosition.X - FCamera.X, APosition.Y - FCamera.Y, AImage.Picture.Graphic);
end;

procedure TRenderPanel.DrawImage(const ARect: TRect; AImage: TImage);
begin
  Canvas.Draw(ARect.Left - FCamera.X, ARect.Top - FCamera.Y, AImage.Picture.Graphic);
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
