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
  KM_Controls, KM_HandsCollection, KM_Defaults, KromOGLUtils, KM_Hand, KM_Units;


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
  public
    constructor Create(aParent: TKMPanel; ATag: Integer; AImage: Word; AHint: String; AColor: TColor4);
    procedure SetPosition(ALeft, ATop: Integer);
    property Tag: Integer read FTag;
    property Value: String read GetValue write SetValue;
  end;

  TKMGUIGameSpectatorCustomPage = class
  private
    FPanel: TKMPanel;
  public
    constructor Create(aParent: TKMPanel); virtual;
    procedure Activation; virtual;

    property Panel: TKMPanel read FPanel;
  end;

  TKMGUIGameSpectatorArmy = class(TKMGUIGameSpectatorCustomPage)
  private
    fItems: array of array[WARRIOR_MIN..WARRIOR_MAX] of TKMGUIGameSpectatorItem;
  public
    constructor Create(aParent: TKMPanel); override;
    procedure Activation; override;

  end;

  TKMGUIGameSpectator = class
  private
    FPanel: TKMPanel;
    FDropBox: TKMDropList;
    FSelectedPage: TKMGUIGameSpectatorCustomPage;
    FPageList: TList;

    //TKMGUIGameSpectatorArmy;

    procedure ChangePage(Sender: TObject);
    procedure AddPage(AName: String; APage: TKMGUIGameSpectatorCustomPage);
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

{ TKMGUIGameSpectatorCustomPage }

constructor TKMGUIGameSpectatorCustomPage.Create(aParent: TKMPanel);
begin
  FPanel := TKMPanel.Create(aParent, aParent.Width - 160, 32, 160, 30);
  FPanel.Anchors := [anTop, anRight];
  FPanel.Focusable := false;
  FPanel.Hide;
end;

procedure TKMGUIGameSpectatorCustomPage.Activation;
begin

end;

{ TKMGUIGameSpectatorArmy }

constructor TKMGUIGameSpectatorArmy.Create(aParent: TKMPanel);
begin
  inherited;

end;

procedure TKMGUIGameSpectatorArmy.Activation;

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
  FPanel.Height := 32 * HandCount;
  FPanel.Width := 448;
  FPanel.Left := FPanel.Parent.Width - 448;
  for i := 0 to HandCount - 1 do
  begin
    p := 0;
    for WT := WARRIOR_MIN to WARRIOR_MAX do
    begin
      UnitCount := GetUnitCount(gHands[i], WT);
      if UnitCount > 0 then
      begin
        p := p + 32;
        fItems[i, WT] := TKMGUIGameSpectatorItem.Create(FPanel, Integer(WT), gRes.Units[WT].GUIIcon, gRes.Units[WT].GUIName, gHands[i].FlagColor);
        fItems[i, WT].SetPosition(FPanel.Width - p, i * 32);
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
  FPageList := TList.Create;
  FSelectedPage := nil;

  FDropBox := TKMDropList.Create(FPanel, 5, 5, 150, 20, fnt_Metal, '', bsMenu);
  FDropBox.OnChange := ChangePage;

  AddPage('Ничего', nil);
  AddPage('Ресурсы', nil);
  AddPage('Строения', nil);
  AddPage('Производство', nil);
  AddPage('Армия', TKMGUIGameSpectatorArmy.Create(FPanel));
  AddPage('Население', nil);
  AddPage('Потери', nil);
end;

destructor TKMGUIGameSpectator.Destroy;
begin
  FPageList.Free;

  inherited;
end;

procedure TKMGUIGameSpectator.AddPage(AName: String; APage: TKMGUIGameSpectatorCustomPage);
begin
  FDropBox.Add(AName);
  FPageList.Add(APage);
end;

procedure TKMGUIGameSpectator.ChangePage(Sender: TObject);
begin
  if Assigned(FSelectedPage) then
    FSelectedPage.Panel.Hide;

  FSelectedPage := TKMGUIGameSpectatorCustomPage(FPageList[FDropBox.ItemIndex]);

  if Assigned(FSelectedPage) then
  begin
    FSelectedPage.Activation;
    FSelectedPage.Panel.Show;
  end;
end;

end.
