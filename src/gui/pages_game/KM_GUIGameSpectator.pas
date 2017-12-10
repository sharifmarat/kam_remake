unit KM_GUIGameSpectator;
{$I KaM_Remake.inc}
interface
{
Ничего
Ресурсы
Доходы
Расходы
Боевые единицы
Строения
Потери
Производство
Улучшения
Армия
Действ./мин.
}
uses
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Math, StrUtils, SysUtils,
  KM_Controls, KM_HandsCollection, KM_Defaults, KromOGLUtils, KM_Hand, KM_Units,
  KM_ResWares;


type
  TKMGUIGameSpectatorPageTypes = (
    gsptNothing = 0,
    gsptResources = 1,
    gsptBuildings = 2,
    gsptProduction = 3,
    gsptArmy = 4,
    gsptPopulation = 5,
    gsptLosses = 6
  );

  TKMGUIGameSpectatorItem = class
  private
    FImage: TKMImage;
    FLabel: TKMLabel;
    FTag: Integer;

    function GetValue: String;
    procedure SetValue(AValue: String);
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
  public
    constructor Create(aParent: TKMPanel; ATag: Integer; AImage: Word; AHint: String; AColor: TColor4);
    procedure SetPosition(ALeft, ATop: Integer);
    property Tag: Integer read FTag;
    property Value: String read GetValue write SetValue;
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  TKMGUIGameSpectatorPanel = class(TKMPanel)
  public
    constructor Create(aParent: TKMPanel); virtual;
    procedure Activation; virtual;

  end;

  TKMGUIGameSpectatorPanelArmy = class(TKMGUIGameSpectatorPanel)
  private
    fItems: array of array[WARRIOR_MIN..WARRIOR_MAX] of TKMGUIGameSpectatorItem;
  public
    constructor Create(aParent: TKMPanel); override;
    procedure Activation; override;

  end;

  TKMGUIGameSpectatorPanelResources = class(TKMGUIGameSpectatorPanel)
  private
    fItems: array of array[WARE_MIN..WARE_MAX] of TKMGUIGameSpectatorItem;
  public
    constructor Create(aParent: TKMPanel); override;
    procedure Activation; override;
    procedure UpdateState(aTickCount: Cardinal); override;
  end;

  TKMGUIGameSpectator = class
  private
    FPanel: TKMPanel;
    FDropBox: TKMDropList;
    FSelectedPanel: TKMGUIGameSpectatorPanel;
    FPanelList: TList;

    //TKMGUIGameSpectatorArmy;

    procedure ChangePage(Sender: TObject);
    procedure AddPage(AName: String; APanel: TKMGUIGameSpectatorPanel);
  public
    constructor Create(aParent: TKMPanel);
    destructor Destroy; override;

  end;
implementation

uses
  KM_RenderUI, KM_ResFonts, KM_Resource;

{ TKMGUIGameSpectatorItem }

constructor TKMGUIGameSpectatorItem.Create(aParent: TKMPanel; ATag: Integer; AImage: Word; AHint: String; AColor: TColor4);
begin
  FTag := ATag;

  FImage := TKMImage.Create(aParent, 0, 0, 30, 30, AImage);
  FImage.Hint := AHint;
  FImage.FlagColor := AColor;
  FImage.ImageCenter;

  FLabel := TKMLabel.Create(aParent, 0, 0, '0', fnt_Grey, taRight);
  FLabel.Hitable := False;
end;

procedure TKMGUIGameSpectatorItem.SetPosition(ALeft, ATop: Integer);
begin
  FImage.Left := ALeft;
  FImage.Top := ATop;
  FLabel.Left := ALeft + 30;
  FLabel.Top := ATop + 16;
end;

function TKMGUIGameSpectatorItem.GetValue: String;
begin
  Result := FLabel.Caption;
end;

procedure TKMGUIGameSpectatorItem.SetValue(AValue: String);
begin
  FLabel.Caption := AValue;
end;

function TKMGUIGameSpectatorItem.GetVisible: Boolean;
begin
  Result := FImage.Visible;
end;

procedure TKMGUIGameSpectatorItem.SetVisible(AValue: Boolean);
begin
  FImage.Visible := AValue;
  FLabel.Visible := AValue;
end;

{ TKMGUIGameSpectatorPanel }

constructor TKMGUIGameSpectatorPanel.Create(aParent: TKMPanel);
begin
  inherited Create(aParent, aParent.Width - 160, 32, 160, 30);
  Anchors := [anTop, anRight];
  Focusable := false;
  Hide;


end;

procedure TKMGUIGameSpectatorPanel.Activation;
begin

end;

{ TKMGUIGameSpectatorPanelResources }

constructor TKMGUIGameSpectatorPanelResources.Create(aParent: TKMPanel);
begin
  inherited;

end;

procedure TKMGUIGameSpectatorPanelResources.Activation;
var
  i, p, HandCount, WareCount: Integer;
  WT: TWareType;
begin
  inherited;
  HandCount := Min(MAX_LOBBY_PLAYERS, gHands.Count);
  if HandCount = Length(fItems) then
    Exit;

  SetLength(fItems, HandCount);
  Height := 32 * HandCount;
  Width := 448;
  Left := Parent.Width - 448;
  for i := 0 to HandCount - 1 do
  begin
    p := 0;
    for WT := WARE_MIN to WARE_MAX do
    begin
      WareCount := gHands[i].Stats.GetWareBalance(WT);
      if WareCount > 0 then
        p := p + 32;
      fItems[i, WT] := TKMGUIGameSpectatorItem.Create(Self, Integer(WT), gRes.Wares[WT].GUIIcon, gRes.Wares[WT].Title, gHands[i].FlagColor);
      fItems[i, WT].SetPosition(Width - p, i * 32);
      fItems[i, WT].Value := IntToStr(WareCount);
      fItems[i, WT].Visible := WareCount > 0;
    end;
  end;
end;

procedure TKMGUIGameSpectatorPanelResources.UpdateState(aTickCount: Cardinal);
var
  i, p, HandCount, WareCount: Integer;
  WT: TWareType;
begin
  inherited;

  if Length(fItems) = 0 then
    Exit;

  for i := 0 to HandCount - 1 do
  begin
    p := 0;
    for WT := WARE_MIN to WARE_MAX do
    begin
      WareCount := gHands[i].Stats.GetWareBalance(WT);

      if WareCount > 0 then
        p := p + 32;

      fItems[i, WT].SetPosition(Width - p, i * 32);
      fItems[i, WT].Value := IntToStr(WareCount);
      fItems[i, WT].Visible := WareCount > 0;
    end;
  end;
end;

{ TKMGUIGameSpectatorPanelArmy }

constructor TKMGUIGameSpectatorPanelArmy.Create(aParent: TKMPanel);
begin
  inherited;

end;

procedure TKMGUIGameSpectatorPanelArmy.Activation;

  function GetUnitCount(AHand: TKMHand; AUnitType: TUnitType): Integer;
  var
    i: Integer;
  begin
    Result := 0;
    for i := 0 to AHand.Units.Count - 1 do
      if AHand.Units[i].UnitType = AUnitType then
        Inc(Result);
  end;
var
  i, p, HandCount: Integer;
  WT: TUnitType;
  UnitCount: Integer;
begin
  inherited;

  HandCount := Min(MAX_LOBBY_PLAYERS, gHands.Count);

  if HandCount = Length(fItems) then
    Exit;

  SetLength(fItems, HandCount);
  Height := 32 * HandCount;
  Width := 448;
  Left := Parent.Width - 448;
  for i := 0 to HandCount - 1 do
  begin
    p := 0;
    for WT := WARRIOR_MIN to WARRIOR_MAX do
    begin
      UnitCount := GetUnitCount(gHands[i], WT);
      if UnitCount > 0 then
      begin
        p := p + 32;
        fItems[i, WT] := TKMGUIGameSpectatorItem.Create(Self, Integer(WT), gRes.Units[WT].GUIIcon, gRes.Units[WT].GUIName, gHands[i].FlagColor);
        fItems[i, WT].SetPosition(Width - p, i * 32);
        fItems[i, WT].Value := IntToStr(UnitCount);
      end;
    end;
  end;
  //gHands
end;

{ TKMGUIGameSpectator }

constructor TKMGUIGameSpectator.Create(aParent: TKMPanel);
begin
  inherited Create;

  FPanel := TKMPanel.Create(aParent, aParent.Width - 160, 0, 160, 30);
  FPanel.Anchors := [anTop, anRight];
  FPanel.Focusable := false;
  FPanel.Show;
  FPanelList := TList.Create;
  FSelectedPanel := nil;

  FDropBox := TKMDropList.Create(FPanel, 5, 5, 150, 20, fnt_Metal, '', bsMenu);
  FDropBox.OnChange := ChangePage;

  AddPage('Ничего', nil);
  AddPage('Ресурсы', TKMGUIGameSpectatorPanelResources.Create(FPanel));
  AddPage('Строения', nil);
  AddPage('Производство', nil);
  AddPage('Армия', TKMGUIGameSpectatorPanelArmy.Create(FPanel));
  AddPage('Население', nil);
  AddPage('Потери', nil);
end;

destructor TKMGUIGameSpectator.Destroy;
begin
  FPanelList.Free;

  inherited;
end;

procedure TKMGUIGameSpectator.AddPage(AName: String; APanel: TKMGUIGameSpectatorPanel);
begin
  FDropBox.Add(AName);
  FPanelList.Add(APanel);
end;

procedure TKMGUIGameSpectator.ChangePage(Sender: TObject);
begin
  if Assigned(FSelectedPanel) then
    FSelectedPanel.Hide;

  FSelectedPanel := TKMGUIGameSpectatorPanel(FPanelList[FDropBox.ItemIndex]);

  if Assigned(FSelectedPanel) then
  begin
    FSelectedPanel.Activation;
    FSelectedPanel.Show;
  end;
end;

end.
