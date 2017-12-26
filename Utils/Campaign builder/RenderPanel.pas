unit RenderPanel;

interface

uses
  Winapi.Windows, Vcl.ExtCtrls, System.Classes, Vcl.Controls,
  System.SysUtils, System.Variants, Winapi.Messages, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Forms, Vcl.Graphics, KM_ResSpritesEdit;

type
  TRenderPanel = class(TPanel)
  private
    FTree: TTreeView;
    FScrollBox: TScrollBox;
    FMousePosition: TPoint;
    FMouseMove: Boolean;
    FBackground: TBitmap;
    FBackgroundIndex: Integer;
  protected
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AScrollBox: TScrollBox; ATree: TTreeView);
    destructor Destroy; override;
    procedure RefreshBackground(AIndex: Integer = 0);

  end;

implementation

uses
  Main;

{ TRenderPanel }

constructor TRenderPanel.Create(AScrollBox: TScrollBox; ATree: TTreeView);
var
  Panel: TPanel;
begin
  inherited Create(AScrollBox);
  FScrollBox := AScrollBox;
  FTree := ATree;

  Parent := AScrollBox;
  Left := 0;
  Top := 0;
  Width := 1024;
  Height := 768;
  FBackground := TBitmap.Create;
  FBackgroundIndex := 1;
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

  FMousePosition := Point(X - FScrollBox.HorzScrollBar.Position, Y - FScrollBox.VertScrollBar.Position);
end;

procedure TRenderPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Position: TPoint;
begin
  inherited;
  if FMouseMove then
  begin
    Position := Point(X - FScrollBox.HorzScrollBar.Position, Y - FScrollBox.VertScrollBar.Position);
    FScrollBox.HorzScrollBar.Position := FScrollBox.HorzScrollBar.Position + (FMousePosition.X - Position.X);
    FScrollBox.VertScrollBar.Position := FScrollBox.VertScrollBar.Position + (FMousePosition.Y - Position.Y);

    FMousePosition := Position;
  end;
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
begin
  //imgBlackFlag       imgRedFlag      imgNode
  if Assigned(FBackground) then
    Canvas.Draw(0, 0, FBackground);

  for i := 0 to FTree.Items.Count - 1 do
    if FTree.Items[i] is TTreeChapterMission then
    begin
      NodeMission := TTreeChapterMission(FTree.Items[i]);
      Canvas.Draw(NodeMission.Position.X, NodeMission.Position.Y, MainForm.imgRedFlag.Picture.Bitmap);

    end;

    //Img.Left := EnsureRange(Img.Left + (X - PrevX), iMap.Left, iMap.Left + 1024-Img.Width);
    //Img.Top  := EnsureRange(Img.Top  + (Y - PrevY), iMap.Top, iMap.Top + 768-Img.Height);

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

end.
