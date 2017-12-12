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
  KM_ResWares, KM_ResHouses, KM_Pics;

const
  GUI_SPECTATOR_ITEM_WIDTH = 28;
  GUI_SPECTATOR_ITEM_HEIGHT = 34;
  GUI_SPECTATOR_ITEM_SPLITE_H = 4;
  GUI_SPECTATOR_ITEM_SPLITE_V = 4;

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

  TKMGUIGameSpectatorItem = class(TKMControl)
  private
    FHandIndex: Integer;
    FImageID: Word;
    FValue: String;
    FTag: Integer;
  public
    constructor Create(aParent: TKMPanel; ATag: Integer; AImageID: Word; AHint: String; AHandIndex: Integer);
    property Tag: Integer read FTag;
    property Value: String read FValue write FValue;
    procedure Paint; override;
  end;

  TKMGUIGameSpectatorItemLineClass = class of TKMGUIGameSpectatorItemLine;

  TKMGUIGameSpectatorItemLine = class(TKMPanel)
  protected
    FHandIndex: Integer;
    FItems: array of TKMGUIGameSpectatorItem;
  protected
    function CreateItem(AHandIndex: Integer; ATag: Integer): TKMGUIGameSpectatorItem; virtual; abstract;
    function GetTagCount: Integer; virtual; abstract;
    function GetTag(AIndex: Integer): Integer; virtual; abstract;
    function GetValue(AHandIndex: Integer; ATag: Integer): String; virtual; abstract;
  public
    constructor Create(aParent: TKMPanel; AHandIndex: Integer);
    procedure UpdateState(aTickCount: Cardinal); override;
  end;

  ///

  TKMGUIGameSpectatorItemLineResources = class(TKMGUIGameSpectatorItemLine)
  protected
    function CreateItem(AHandIndex: Integer; ATag: Integer): TKMGUIGameSpectatorItem; override;
    function GetTagCount: Integer; override;
    function GetTag(AIndex: Integer): Integer; override;
    function GetValue(AHandIndex: Integer; ATag: Integer): String; override;
  end;

  TKMGUIGameSpectatorItemLineBuildings = class(TKMGUIGameSpectatorItemLine)
  protected
    function CreateItem(AHandIndex: Integer; ATag: Integer): TKMGUIGameSpectatorItem; override;
    function GetTagCount: Integer; override;
    function GetTag(AIndex: Integer): Integer; override;
    function GetValue(AHandIndex: Integer; ATag: Integer): String; override;
  end;

  TKMGUIGameSpectatorItemLineArmy = class(TKMGUIGameSpectatorItemLine)
  protected
    function CreateItem(AHandIndex: Integer; ATag: Integer): TKMGUIGameSpectatorItem; override;
    function GetTagCount: Integer; override;
    function GetTag(AIndex: Integer): Integer; override;
    function GetValue(AHandIndex: Integer; ATag: Integer): String; override;
  end;

  ///

  TKMGUIGameSpectator = class
  private
    FDropBoxPanel: TKMPanel;
    FDropBox: TKMDropList;
    FLastIndex: Integer;

    FLines: array of array[0..MAX_LOBBY_PLAYERS] of TKMGUIGameSpectatorItemLine;

    procedure AddLineType(AIndex: Integer; ALineClass: TKMGUIGameSpectatorItemLineClass);
    procedure ChangePage(Sender: TObject);
  public
    constructor Create(aParent: TKMPanel);
  end;

implementation

uses
  KM_RenderUI, KM_ResFonts, KM_Resource;

{ TKMGUIGameSpectatorItem }

constructor TKMGUIGameSpectatorItem.Create(aParent: TKMPanel; ATag: Integer; AImageID: Word; AHint: String; AHandIndex: Integer);
begin
  inherited Create(aParent, 0, 0, GUI_SPECTATOR_ITEM_WIDTH, GUI_SPECTATOR_ITEM_HEIGHT);
  FTag := ATag;
  Hint := AHint;
  FHandIndex := AHandIndex;
  FImageID := AImageID;
end;

procedure TKMGUIGameSpectatorItem.Paint;
begin
  inherited;

  TKMRenderUI.WriteBevel(AbsLeft, AbsTop, Width, Height);
  TKMRenderUI.WritePicture(AbsLeft + 2, AbsTop, Width - 4, Height - 4, [], rxGui, FImageID, True, gHands[FHandIndex].FlagColor);
  TKMRenderUI.WriteText(AbsLeft, AbsTop + Height - 14, Width, FValue, fnt_Grey, taCenter, $FFFFFFFF);
end;

{ TKMGUIGameSpectatorItemLine }

constructor TKMGUIGameSpectatorItemLine.Create(aParent: TKMPanel; AHandIndex: Integer);
var
  i: Integer;
begin
  inherited Create(aParent, aParent.Width, 32 + AHandIndex * (GUI_SPECTATOR_ITEM_HEIGHT + GUI_SPECTATOR_ITEM_SPLITE_V), 0, GUI_SPECTATOR_ITEM_HEIGHT);
  Anchors := [anTop, anRight];
  Focusable := false;  
  FHandIndex := AHandIndex;
  SetLength(fItems, GetTagCount);
  for i := 0 to GetTagCount - 1 do
    fItems[i] := CreateItem(AHandIndex, GetTag(i));
end;

procedure TKMGUIGameSpectatorItemLine.UpdateState(aTickCount: Cardinal);
var
  i, Position: Integer;
  Value: String;
begin
  inherited;
  if not Visible then
    Exit;

  Position := 0;
  for i := 0 to GetTagCount - 1 do
  begin
    Value := GetValue(FHandIndex, GetTag(i));
    fItems[i].Visible := Value <> '0';
    fItems[i].Top := 0;
    fItems[i].Left := Position;
    fItems[i].Value := Value;
    if fItems[i].Visible then
      Inc(Position, GUI_SPECTATOR_ITEM_WIDTH + GUI_SPECTATOR_ITEM_SPLITE_H);
  end;
  Left := Parent.Width - Position;
end;

{ TKMGUIGameSpectatorItemLineResources }

function TKMGUIGameSpectatorItemLineResources.CreateItem(AHandIndex: Integer; ATag: Integer): TKMGUIGameSpectatorItem;
begin
  Result := TKMGUIGameSpectatorItem.Create(Self, ATag, gRes.Wares[TWareType(ATag)].GUIIcon, gRes.Wares[TWareType(ATag)].Title, FHandIndex);
  Result.Visible := False;
end;

function TKMGUIGameSpectatorItemLineResources.GetTagCount: Integer;
begin
  Result := Integer(WARE_MAX) - Integer(WARE_MIN);
end;

function TKMGUIGameSpectatorItemLineResources.GetTag(AIndex: Integer): Integer;
begin
  Result := Integer(WARE_MIN) + AIndex;
end;

function TKMGUIGameSpectatorItemLineResources.GetValue(AHandIndex: Integer; ATag: Integer): String;
begin
  Result := IntToStr(gHands[AHandIndex].Stats.GetWareBalance(TWareType(ATag)));
end;

{ TKMGUIGameSpectatorItemLineBuildings }

function TKMGUIGameSpectatorItemLineBuildings.CreateItem(AHandIndex: Integer; ATag: Integer): TKMGUIGameSpectatorItem;
begin
  Result := TKMGUIGameSpectatorItem.Create(Self, ATag, gRes.Houses[THouseType(ATag)].GUIIcon, gRes.Houses[THouseType(ATag)].HouseName, FHandIndex);
  Result.Visible := False;
end;

function TKMGUIGameSpectatorItemLineBuildings.GetTagCount: Integer;
begin
  Result := Integer(HOUSE_MAX) - Integer(HOUSE_MIN);
end;

function TKMGUIGameSpectatorItemLineBuildings.GetTag(AIndex: Integer): Integer;
begin
  Result := Integer(HOUSE_MIN) + AIndex;
end;

function TKMGUIGameSpectatorItemLineBuildings.GetValue(AHandIndex: Integer; ATag: Integer): String;
begin
  Result := IntToStr(gHands[AHandIndex].Stats.GetHouseQty(THouseType(ATag)));
end;

{ TKMGUIGameSpectatorItemLineArmy }

function TKMGUIGameSpectatorItemLineArmy.CreateItem(AHandIndex: Integer; ATag: Integer): TKMGUIGameSpectatorItem;
begin
  Result := TKMGUIGameSpectatorItem.Create(Self, ATag, gRes.Units[TUnitType(ATag)].GUIIcon, gRes.Units[TUnitType(ATag)].GUIName, FHandIndex);
  Result.Visible := False;
end;

function TKMGUIGameSpectatorItemLineArmy.GetTagCount: Integer;
begin
  Result := Integer(WARRIOR_MAX) - Integer(WARRIOR_MIN);
end;

function TKMGUIGameSpectatorItemLineArmy.GetTag(AIndex: Integer): Integer;
begin
  Result := Integer(WARRIOR_MIN) + AIndex;
end;

function TKMGUIGameSpectatorItemLineArmy.GetValue(AHandIndex: Integer; ATag: Integer): String;
begin
  Result := IntToStr(gHands[AHandIndex].Stats.GetUnitQty(TUnitType(ATag)));
end;

{ TKMGUIGameSpectator }

constructor TKMGUIGameSpectator.Create(aParent: TKMPanel);
begin
  inherited Create;

  FDropBoxPanel := TKMPanel.Create(aParent, aParent.Width - 210, 0, 210, 30);
  FDropBoxPanel.Anchors := [anTop, anRight];
  //FDropBoxPanel.Focusable := false;
  FDropBoxPanel.Show;


  FLastIndex := 0; 
    
  SetLength(FLines, 7);       
     
  AddLineType(0, nil);
  AddLineType(1, TKMGUIGameSpectatorItemLineResources);
  AddLineType(2, TKMGUIGameSpectatorItemLineBuildings);
  AddLineType(3, nil);
  AddLineType(4, TKMGUIGameSpectatorItemLineArmy);
  AddLineType(5, nil);
  AddLineType(6, nil);
                 
  FDropBox := TKMDropList.Create(FDropBoxPanel, 5, 5, 200, 20, fnt_Metal, '', bsGame);
  FDropBox.OnChange := ChangePage;
  
  FDropBox.Add('Ничего');  
  FDropBox.Add('Ресурсы');
  FDropBox.Add('Строения');
  FDropBox.Add('Производство');
  FDropBox.Add('Армия');
  FDropBox.Add('Население');  
  FDropBox.Add('Потери');
  
  FDropBox.ItemIndex := 0;
end;

procedure TKMGUIGameSpectator.AddLineType(AIndex: Integer; ALineClass: TKMGUIGameSpectatorItemLineClass);
var
  i: Integer;
begin
  if ALineClass <> nil then  
    for i := 0 to MAX_LOBBY_PLAYERS - 1 do
    begin     
      FLines[AIndex, i] := ALineClass.Create(FDropBoxPanel.Parent, i);
      FLines[AIndex, i].Visible := False;
    end;
end;

procedure TKMGUIGameSpectator.ChangePage(Sender: TObject);
var
  LineClass: TKMGUIGameSpectatorItemLineClass;
  i: Integer;
begin    
  for i := 0 to MAX_LOBBY_PLAYERS - 1 do
    if Assigned(FLines[FLastIndex, i]) then    
      FLines[FLastIndex, i].Visible := False;
                                      
  FLastIndex := FDropBox.ItemIndex;
  
  for i := 0 to Min(MAX_LOBBY_PLAYERS, gHands.Count) - 1 do
    if Assigned(FLines[FLastIndex, i]) then 
      FLines[FLastIndex, i].Visible := True;
end;

end.
