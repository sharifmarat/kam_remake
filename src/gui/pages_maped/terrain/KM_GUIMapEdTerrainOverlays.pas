unit KM_GUIMapEdTerrainOverlays;
{$I KaM_Remake.inc}
interface
uses
   Math, SysUtils,
   KM_InterfaceDefaults,
   KM_Controls, KM_Defaults, KM_Pics;


const
  ALLOWED_OVERLAYS = 5;
  MAPED_OVERLAYS_ID: array[0..ALLOWED_OVERLAYS - 1] of Integer = (0, 249, 251, 253, 255);   //toNone, toDig1, toDig2, toDig3, toDig4

type
  TKMMapEdTerrainOverlays = class (TKMMapEdSubMenuPage)
  private
    fLastOverlay: Word;

    procedure OverlayChange(Sender: TObject);
    procedure OverlaySet(aIndex: Integer);
    procedure OverlayRefresh(Sender: TObject);
  protected
    Panel_Overlays: TKMPanel;
    OverlaysTable: array [0..ALLOWED_OVERLAYS - 1] of TKMButtonFlat;
  public
    constructor Create(aParent: TKMPanel);

    procedure Show;
    procedure Hide;
    procedure UpdateState;
    function Visible: Boolean; override;
  end;


implementation
uses
  KM_ResFonts, KM_ResTexts,
  KM_GameCursor, KM_RenderUI, KM_InterfaceGame;


constructor TKMMapEdTerrainOverlays.Create(aParent: TKMPanel);
const
  BTN_SIZE = 36;
  BTNS_PER_ROW = 5;
var
  J: Integer;
begin
  inherited Create;

  Panel_Overlays := TKMPanel.Create(aParent, 0, 28, aParent.Width, 400);
  with TKMLabel.Create(Panel_Overlays, 0, PAGE_TITLE_Y, Panel_Overlays.Width, 0, gResTexts[TX_MAPED_TERRAIN_OVERLAYS], fntOutline, taCenter) do
    Anchors := [anLeft, anTop, anRight];
  for J := 0 to ALLOWED_OVERLAYS - 1 do
  begin
    OverlaysTable[J] := TKMButtonFlat.Create(Panel_Overlays, 9 + (J mod BTNS_PER_ROW) * BTN_SIZE,
                                                             BTN_SIZE + (J div BTNS_PER_ROW) * BTN_SIZE,
                                                             BTN_SIZE,
                                                             BTN_SIZE,
                                                             IfThen(MAPED_OVERLAYS_ID[J] > 0, MAPED_OVERLAYS_ID[J] + 1, 0),
                                                             rxTiles);
    OverlaysTable[J].Tag := MAPED_OVERLAYS_ID[J];
//    OverlaysTable[J].Caption := IntToStr(OverlaysTable[J].Tag);
//    OverlaysTable[J].CapOffsetY := -8;
//    OverlaysTable[J].TexOffsetY := 6;
//    OverlaysTable[J].CapColor := icYellow;
    OverlaysTable[J].Hint := IntToStr(OverlaysTable[J].Tag);
    OverlaysTable[J].OnClick := OverlayChange;
  end;
end;


procedure TKMMapEdTerrainOverlays.OverlayChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ALLOWED_OVERLAYS - 1 do
    if Sender = OverlaysTable[I] then
    begin
      gGameCursor.Mode := cmOverlays;
      gGameCursor.Tag1 := MAPED_OVERLAYS_ID[I];
    end;
end;


procedure TKMMapEdTerrainOverlays.OverlaySet(aIndex: Integer);
begin
  if aIndex > 0 then
  begin
    gGameCursor.Mode := cmOverlays;
    gGameCursor.Tag1 := aIndex;
    fLastOverlay := aIndex;
  end;

  OverlayRefresh(nil);
end;


procedure TKMMapEdTerrainOverlays.OverlayRefresh(Sender: TObject);
var
  I, OverlayTexID: Integer;
begin
  for I := 0 to ALLOWED_OVERLAYS - 1 do
  begin
    OverlayTexID := MAPED_OVERLAYS_ID[I];
    OverlaysTable[I].Down := (gGameCursor.Mode = cmOverlays) and (gGameCursor.Tag1 = OverlayTexID);
  end;
end;


procedure TKMMapEdTerrainOverlays.Show;
begin
  OverlaySet(fLastOverlay);
  gGameCursor.MapEdDir := 0;
  Panel_Overlays.Show;
end;


function TKMMapEdTerrainOverlays.Visible: Boolean;
begin
  Result := Panel_Overlays.Visible;
end;


procedure TKMMapEdTerrainOverlays.Hide;
begin
  Panel_Overlays.Hide;
end;


procedure TKMMapEdTerrainOverlays.UpdateState;
begin
  OverlayRefresh(nil);
end;


end.
