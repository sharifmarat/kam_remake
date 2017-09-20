unit KM_GUIMapEdTerrainBrushes;
{$I KaM_Remake.inc}
interface
uses
   Classes, Math, SysUtils,
   KM_Controls, KM_Defaults, KM_Pics;


type
  //Painting on terrain with terrain brushes
  TKMMapEdTerrainBrushes = class
  private
    fLastShape: TKMMapEdShape;
    procedure BrushChange(Sender: TObject);
    procedure BrushRefresh;
  protected
    Panel_Brushes: TKMPanel;
    BrushSize: TKMTrackBar;
    BrushCircle: TKMButtonFlat;
    BrushSquare: TKMButtonFlat;
    BrushTable: array [0..6, 0..4] of TKMButtonFlat;
    MagicBrush: TKMButtonFlat;
    BrushRandom: TKMCheckBox;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    function Visible: Boolean;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var aHandled: Boolean);
    procedure UpdateState;
  end;


implementation
uses
  TypInfo, KM_ResFonts, KM_ResTexts, KM_Game, KM_GameCursor, KM_RenderUI,
  KM_TerrainPainter, KM_InterfaceGame, KM_ResTileset;


{ TKMMapEdTerrainBrushes }
constructor TKMMapEdTerrainBrushes.Create(aParent: TKMPanel);
const
  Surfaces: array [0..6, 0..4] of TKMTerrainKind = (
    (tkGrass,       tkMoss,         tkPaleGrass,    tkGrassDirt,    tkDirt),
    (tkCobbleStone, tkGravel,       tkCustom,       tkCustom,       tkCustom),
    (tkCoastSand,   tkGrassSand1,   tkGrassSand2,   tkGrassSand3,   tkSand),
    (tkSwamp,       tkGrassyWater,  tkWater,        tkFastWater,    tkCustom),
    (tkShallowSnow, tkSnow,         tkDeepSnow,     tkIce,          tkCustom),
    (tkStone,  tkGoldMount,    tkIronMount,    tkAbyss,        tkCustom),
    (tkCoal,        tkGold,         tkIron,         tkLava,         tkCustom));
var
  I,K: Integer;
  TerKindName: String;
begin
  inherited Create;

  fLastShape := hsCircle;

  Panel_Brushes := TKMPanel.Create(aParent, 0, 28, TB_WIDTH, 400);

  TKMLabel.Create(Panel_Brushes, 0, PAGE_TITLE_Y, TB_WIDTH, 0, gResTexts[TX_MAPED_TERRAIN_BRUSH], fnt_Outline, taCenter);
  BrushSize   := TKMTrackBar.Create(Panel_Brushes, 0, 30, 100, 0, 32);
  BrushSize.Position := 4;
  BrushSize.OnChange := BrushChange;
  BrushCircle := TKMButtonFlat.Create(Panel_Brushes, 106, 28, 24, 24, 592);
  BrushCircle.Hint := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_CIRCLE];
  BrushCircle.OnClick := BrushChange;
  BrushSquare := TKMButtonFlat.Create(Panel_Brushes, 134, 28, 24, 24, 593);
  BrushSquare.Hint := gResTexts[TX_MAPED_TERRAIN_HEIGHTS_SQUARE];
  BrushSquare.OnClick := BrushChange;

  for I := Low(Surfaces) to High(Surfaces) do
  for K := Low(Surfaces[I]) to High(Surfaces[I]) do
  if Surfaces[I,K] <> tkCustom then
  begin
    BrushTable[I,K] := TKMButtonFlat.Create(Panel_Brushes, K * 36, 60 + I * 40, 34, 34, Combo[Surfaces[I,K], Surfaces[I,K], 1] + 1, rxTiles);  // grass
    BrushTable[I,K].Tag := Byte(Surfaces[I,K]);
    TerKindName := GetEnumName(TypeInfo(TKMTerrainKind), Integer(Surfaces[I,K]));
    BrushTable[I,K].Hint := Copy(TerKindName, 3, Length(TerKindName) - 2);
    BrushTable[I,K].OnClick := BrushChange;
  end;

  BrushRandom := TKMCheckBox.Create(Panel_Brushes, 0, 350, TB_WIDTH, 20, gResTexts[TX_MAPED_TERRAIN_BRUSH_RANDOM], fnt_Metal);
  BrushRandom.OnClick := BrushChange;

  MagicBrush := TKMButtonFlat.Create(Panel_Brushes, 0, 380, 34, 34, 4, rxTiles);
  MagicBrush.OnClick := BrushChange;
end;


procedure TKMMapEdTerrainBrushes.BrushChange(Sender: TObject);
begin
  if gGameCursor.Mode <> cmBrush then
    gGameCursor.Mode := cmBrush;    // This will reset Tag

  gGameCursor.MapEdSize := BrushSize.Position;
  gGame.MapEditor.TerrainPainter.RandomizeTiling := BrushRandom.Checked;

  if Sender = MagicBrush then
    gGameCursor.MapEdMagicBrush := True
  else
  begin
    gGameCursor.MapEdMagicBrush := False;
    if Sender = BrushCircle then
    begin
      gGameCursor.MapEdShape := hsCircle;
      fLastShape := hsCircle;
    end
    else
    if Sender = BrushSquare then
    begin
      gGameCursor.MapEdShape := hsSquare;
      fLastShape := hsSquare;
    end
    else
    if Sender is TKMButtonFlat then
      gGameCursor.Tag1 := TKMButtonFlat(Sender).Tag;
  end;

  BrushRefresh;
end;


procedure TKMMapEdTerrainBrushes.BrushRefresh;
var
  I,K: Integer;
begin
  BrushCircle.Down := (gGameCursor.MapEdShape = hsCircle);
  BrushSquare.Down := (gGameCursor.MapEdShape = hsSquare);

  for I := Low(BrushTable) to High(BrushTable) do
  for K := Low(BrushTable[I]) to High(BrushTable[I]) do
  if BrushTable[I,K] <> nil then
    BrushTable[I,K].Down := (BrushTable[I,K].Tag = gGameCursor.Tag1);
end;


procedure TKMMapEdTerrainBrushes.Hide;
begin
  Panel_Brushes.Hide;
end;


procedure TKMMapEdTerrainBrushes.Show;
begin
  gGameCursor.MapEdShape := fLastShape;

  BrushChange(BrushTable[0,0]);

  Panel_Brushes.Show;
end;


function TKMMapEdTerrainBrushes.Visible: Boolean;
begin
  Result := Panel_Brushes.Visible;
end;


procedure TKMMapEdTerrainBrushes.MouseWheel(Shift: TShiftState; WheelDelta, X, Y: Integer; var aHandled: Boolean);
begin
  aHandled := False;
  if ssCtrl in Shift then
  begin
    BrushSize.Position := Max(0, BrushSize.Position - (WheelDelta div 100)); //can't set negative number
    BrushChange(nil);
    aHandled := True;
  end;
end;


procedure TKMMapEdTerrainBrushes.UpdateState;
begin
  BrushRefresh;
end;


end.
