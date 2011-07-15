unit KM_ResourceGFX;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF WDC} PNGImage, {$ENDIF}
  {$IFDEF MSWindows} Windows, {$ENDIF}
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Forms, Graphics, SysUtils, Math, dglOpenGL, KM_Defaults, KM_TextLibrary, Classes, KM_CommonTypes
  {$IFDEF WDC}, ZLibEx {$ENDIF}
  {$IFDEF FPC}, Zstream {$ENDIF};


type
  TCardinalArray2 = array of Cardinal;
  TexMode = (tm_TexID, tm_AltID, tm_AlphaTest); //Defines way to decode sprites using palette info
  TDataLoadingState = (dls_None, dls_Menu, dls_All); //Resources are loaded in 2 steps, for menu and the rest


  THouseAnim = array[1..19] of packed record
      Step:array[1..30]of smallint;
      Count:smallint;
      MoveX,MoveY:integer;
    end;

  THouseBuildSupply = array[1..12] of packed record MoveX,MoveY:integer; end;
  THouseSupply = array[1..4,1..5]of smallint;

  TKMHouseDat = packed record
    StonePic,WoodPic,WoodPal,StonePal:smallint;
    SupplyIn:THouseSupply;
    SupplyOut:THouseSupply;
    Anim:THouseAnim;
    WoodPicSteps,StonePicSteps:word;
    a1:smallint;
    EntranceOffsetX,EntranceOffsetY:shortint;
    EntranceOffsetXpx,EntranceOffsetYpx:shortint; //When entering house units go for the door, which is offset by these values
    BuildArea:array[1..10,1..10]of shortint;
    WoodCost,StoneCost:byte;
    BuildSupply:THouseBuildSupply;
    a5,SizeArea:smallint;
    SizeX,SizeY,sx2,sy2:shortint;
    WorkerWork,WorkerRest:smallint;
    ResInput,ResOutput:array[1..4]of shortint; //KaM_Remake will use it's own tables for this matter
    ResProductionX:shortint;
    MaxHealth,Sight:smallint;
    OwnerType:shortint;
    Foot1:array[1..12]of shortint; //Sound indices
    Foot2:array[1..12]of smallint; //vs sprite ID
  end;

  THouseArea = array[1..4,1..4]of byte;
  THouseRes = array[1..4]of TResourceType;

  //This class wraps KaM House info and hides unused fields
  TKMHouseDatClass = class
  private
    fHouseType:THouseType; //Our class
    fHouseDat:TKMHouseDat;
    function GetArea:THouseArea;
    function GetAcceptsGoods:boolean;
    function GetDoesOrders:boolean;
    function GetGUIIcon:word;
    function GetHouseName:string;
    function GetHouseUnlock:THouseTypeSet;
    function GetResInput:THouseRes;
    function GetResOutput:THouseRes;
    function GetOwnerType:TUnitType;
    function GetProducesGoods:boolean;
  public
    constructor Create(aHouseType:THouseType);
    function IsValid:boolean;
    procedure LoadFromStream(Stream:TMemoryStream);
    //Derived from KaM
    property StonePic:smallint read fHouseDat.StonePic;
    property WoodPic:smallint read fHouseDat.WoodPic;
    property WoodPal:smallint read fHouseDat.WoodPal;
    property StonePal:smallint read fHouseDat.StonePal;
    property SupplyIn:THouseSupply read fHouseDat.SupplyIn;
    property SupplyOut:THouseSupply read fHouseDat.SupplyOut;
    property Anim:THouseAnim read fHouseDat.Anim;
    property WoodPicSteps:word read fHouseDat.WoodPicSteps;
    property StonePicSteps:word read fHouseDat.StonePicSteps;
    property EntranceOffsetX:shortint read fHouseDat.EntranceOffsetX;
    property EntranceOffsetXpx:shortint read fHouseDat.EntranceOffsetXpx;
    property WoodCost:byte read fHouseDat.WoodCost;
    property StoneCost:byte read fHouseDat.StoneCost;
    property BuildSupply:THouseBuildSupply read fHouseDat.BuildSupply;
    property WorkerRest:smallint read fHouseDat.WorkerRest;
    property ResProductionX:shortint read fHouseDat.ResProductionX;
    property MaxHealth:smallint read fHouseDat.MaxHealth;
    property Sight:smallint read fHouseDat.Sight;
    property OwnerType:TUnitType read GetOwnerType;
    //Additional properties added by Remake
    property AcceptsGoods:boolean read GetAcceptsGoods;
    property BuildArea:THouseArea read GetArea;
    property DoesOrders:boolean read GetDoesOrders;
    property HouseName:string read GetHouseName;
    property HouseUnlock:THouseTypeSet read GetHouseUnlock;
    property GUIIcon:word read GetGUIIcon;
    property ProducesGoods:boolean read GetProducesGoods;
    property ResInput:THouseRes read GetResInput;
    property ResOutput:THouseRes read GetResOutput;
  end;

  //Swine&Horses, 5 beasts in each house, 3 ages for each beast
  TKMHouseBeastAnim = packed record
    Step:array[1..30]of smallint;
    Count:smallint;
    MoveX,MoveY:integer;
  end;

  TKMHouseDatCollection = class
  private
    fItems: array[THouseType] of TKMHouseDatClass;
    fBeastAnim: array[1..2,1..5,1..3] of TKMHouseBeastAnim;
    function GetHouseDat(aType:THouseType):TKMHouseDatClass;
    function GetBeastAnim(aType:THouseType; aBeast, aAge:integer):TKMHouseBeastAnim;
  public
    constructor Create;
    destructor Destroy; override;

    property HouseDat[aType:THouseType]:TKMHouseDatClass read GetHouseDat; default;
    property BeastAnim[aType:THouseType; aBeast, aAge:integer]:TKMHouseBeastAnim read GetBeastAnim;

    procedure LoadHouseDat(aPath:string);
    procedure ExportCSV(aPath: string);
  end;


  TResource = class
  private
    fDataState:TDataLoadingState;
    fHouseDat: TKMHouseDatCollection;

    procedure StepRefresh;
    procedure StepCaption(aCaption:string);

    function LoadPalettes:boolean;
    function LoadMapElemDAT(FileName:string):boolean;
    function LoadPatternDAT(FileName:string):boolean;
    function LoadUnitDAT(FileName:string):boolean;
    function LoadFont(FileName:string; aFont:TKMFont; WriteFontToBMP:boolean):boolean;

    //procedure AddHouseDAT;

    procedure AllocateRX(ID:integer; Count:integer=0);
    function  LoadRX(FileName:string; ID:integer):boolean;
    procedure LoadRX7(RX:integer);
    procedure ExpandRX(ID:integer);
    procedure MakeGFX(RXid:integer);
    procedure MakeGFX_AlphaTest(RXid:integer);

    procedure ClearUnusedGFX(RXid:integer);

    procedure MakeMiniMapColors(FileName:string);
    procedure MakeCursors(RXid:integer);

    function GenTexture(DestX, DestY:word; const Data:TCardinalArray2; Mode:TexMode):gluint; //This should belong to TRender?
  public
    OnLoadingStep:TNotifyEvent;
    OnLoadingText:TStringEvent;

    constructor Create(aLocale:string; aLS:TNotifyEvent; aLT:TStringEvent);
    destructor Destroy; override;
    procedure LoadMenuResources(aLocale:string);
    procedure LoadGameResources;
    procedure MakeTileGFXFromTexture(Texture:GLuint);

    function GetColor32(aIndex:byte; aPal:TKMPal=DEF_PAL):cardinal;

    property DataState: TDataLoadingState read fDataState;
    property HouseDat: TKMHouseDatCollection read fHouseDat;

    function GetUnitSequenceLength(aUnitType:TUnitType; aAction:TUnitActionType; aDir:TKMDirection):smallint;

    procedure LoadFonts(DoExport:boolean; aLocale:string);
    //procedure ExportRX2BMP(RXid:integer);
    //procedure ExportTreeAnim2BMP;
    //procedure ExportHouseAnim2BMP;
    //procedure ExportUnitAnim2BMP;
  end;

const
//1-building area //2-entrance
HousePlanYX:array[THouseType] of THouseArea = (
((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0)), //0
((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0)), //0
((0,0,0,0), (0,1,1,0), (1,1,1,1), (1,2,1,1)), //Armor smithy
((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,2,1,1)), //Armor workshop
((0,0,0,0), (0,1,1,1), (0,1,1,1), (0,1,1,2)), //Bakery
((1,1,1,1), (1,1,1,1), (1,1,1,1), (1,2,1,1)), //Barracks
((0,0,0,0), (0,1,1,0), (0,1,1,1), (0,1,1,2)), //Butchers
((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,2,1,0)), //Coal mine
((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1)), //Farm
((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,2,1,1)), //Fisher hut
((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,0)), //Gold mine
((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,2,1,1)), //Inn
((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,1,2,1)), //Iron mine
((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,1,2,1)), //Iron smithy
((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0)), //Metallurgist
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1)), //Mill
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1)), //Quarry
((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1)), //Sawmill
((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0)), //School
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,2,1,1)), //Siege workshop
((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,1,2,1)), //Stables
((0,0,0,0), (1,1,1,0), (1,1,1,0), (1,2,1,0)), //Store
((0,0,0,0), (0,1,1,1), (1,1,1,1), (1,1,1,2)), //Swine
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,2,1)), //Tannery
((0,0,0,0), (1,1,1,1), (1,1,1,1), (1,2,1,1)), //Town hall
((0,0,0,0), (0,0,0,0), (0,1,1,0), (0,1,2,0)), //Watch tower
((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1)), //Weapon smithy
((0,0,0,0), (0,0,0,0), (1,1,1,1), (1,2,1,1)), //Weapon workshop
((0,0,0,0), (0,0,0,0), (0,1,1,1), (0,1,1,2)),  //Wineyard
((0,0,0,0), (0,0,0,0), (1,1,1,0), (1,1,2,0)) //Woodcutter
);

  //Building of certain house allows player to build following houses,
  //unless they are blocked in mission script of course
  HouseRelease:array[THouseType]of THouseTypeSet = (
  [], [],
  [], [], [], [], [],
  [],
  [ht_Mill,ht_Swine,ht_Stables], //Farm
  [],
  [ht_Metallurgists], //GoldMine
  [ht_Quary], //Inn
  [ht_IronSmithy], //IronMine
  [ht_WeaponSmithy,ht_ArmorSmithy,ht_SiegeWorkshop], //IronSmithy
  [ht_TownHall], //Metallurgists
  [ht_Bakery], //Mill
  [ht_Woodcutters,ht_WatchTower], //Quary
  [ht_Farm, ht_Wineyard, ht_CoalMine, ht_IronMine, ht_GoldMine, ht_WeaponWorkshop, ht_Barracks, ht_FisherHut], //Sawmill
  [ht_Inn],  //School
  [],
  [],
  [ht_School], //Store
  [ht_Butchers,ht_Tannery], //Swine
  [ht_ArmorWorkshop],  //Tannery
  [],
  [],
  [],
  [],
  [],
  [ht_Sawmill] //Woodcutters
  );

  //Does house output needs to be ordered by Player or it keeps on producing by itself
  HouseDoesOrders_:array[THouseType] of boolean = (
  false, false,
  true, true, false,false,false,false,false,false,false,false,
  false,false,false,false,false,false,false,true ,false,false,
  false,false,false,false,true ,true ,false,false);

  GUIBuildIcons_:array[0..HouseDatCount]of word = (
  0, //ht_None
  301, 302, 303, 304, 305,
  306, 307, 308, 309, 310,
  311, 312, 313, 314, 315,
  316, 317, 318, 319, 320,
  321, 322, 323, 324, 325,
  326, 327, 328, 329{, 338});


//What does house produces
HouseOutput_:array[0..HouseDatCount] of THouseRes = (
(rt_None,       rt_None,       rt_None,       rt_None), //None
(rt_Wood,       rt_None,       rt_None,       rt_None), //Sawmill
(rt_Steel,      rt_None,       rt_None,       rt_None), //Iron smithy
(rt_Sword,      rt_Hallebard,  rt_Arbalet,    rt_None), //Weapon smithy
(rt_Coal,       rt_None,       rt_None,       rt_None), //Coal mine
(rt_IronOre,    rt_None,       rt_None,       rt_None), //Iron mine
(rt_GoldOre,    rt_None,       rt_None,       rt_None), //Gold mine
(rt_Fish,       rt_None,       rt_None,       rt_None), //Fisher hut
(rt_Bread,      rt_None,       rt_None,       rt_None), //Bakery
(rt_Corn,       rt_None,       rt_None,       rt_None), //Farm
(rt_Trunk,      rt_None,       rt_None,       rt_None), //Woodcutter
(rt_MetalArmor, rt_MetalShield,rt_None,       rt_None), //Armor smithy
(rt_All,        rt_None,       rt_None,       rt_None), //Store
(rt_Horse,      rt_None,       rt_None,       rt_None), //Stables
(rt_None,       rt_None,       rt_None,       rt_None), //School
(rt_Stone,      rt_None,       rt_None,       rt_None), //Quarry
(rt_Gold,       rt_None,       rt_None,       rt_None), //Metallurgist
(rt_Pig,        rt_Skin,       rt_None,       rt_None), //Swine
(rt_None,       rt_None,       rt_None,       rt_None), //Watch tower
(rt_None,       rt_None,       rt_None,       rt_None), //Town hall
(rt_Axe,        rt_Pike,       rt_Bow,        rt_None), //Weapon workshop
(rt_Shield,     rt_Armor,      rt_None,       rt_None), //Armor workshop
(rt_None,       rt_None,       rt_None,       rt_None), //Barracks
(rt_Flour,      rt_None,       rt_None,       rt_None), //Mill
(rt_None,       rt_None,       rt_None,       rt_None), //Siege workshop
(rt_Sausages,   rt_None,       rt_None,       rt_None), //Butcher
(rt_Leather,    rt_None,       rt_None,       rt_None), //Tannery
(rt_None,       rt_None,       rt_None,       rt_None), //N/A
(rt_None,       rt_None,       rt_None,       rt_None), //Inn
(rt_Wine,       rt_None,       rt_None,       rt_None){, //Wineyard
(rt_None,       rt_None,       rt_None,       rt_None)}  //Wall
);

//What house requires
HouseInput_:array[0..HouseDatCount] of THouseRes = (
(rt_None,       rt_None,       rt_None,       rt_None), //None
(rt_Trunk,      rt_None,       rt_None,       rt_None), //Sawmill
(rt_IronOre,    rt_Coal,       rt_None,       rt_None), //Iron smithy
(rt_Coal,       rt_Steel,      rt_None,       rt_None), //Weapon smithy
(rt_None,       rt_None,       rt_None,       rt_None), //Coal mine
(rt_None,       rt_None,       rt_None,       rt_None), //Iron mine
(rt_None,       rt_None,       rt_None,       rt_None), //Gold mine
(rt_None,       rt_None,       rt_None,       rt_None), //Fisher hut
(rt_Flour,      rt_None,       rt_None,       rt_None), //Bakery
(rt_None,       rt_None,       rt_None,       rt_None), //Farm
(rt_None,       rt_None,       rt_None,       rt_None), //Woodcutter
(rt_Steel,      rt_Coal,       rt_None,       rt_None), //Armor smithy
(rt_All,        rt_None,       rt_None,       rt_None), //Store
(rt_Corn,       rt_None,       rt_None,       rt_None), //Stables
(rt_Gold,       rt_None,       rt_None,       rt_None), //School
(rt_None,       rt_None,       rt_None,       rt_None), //Quarry
(rt_GoldOre,    rt_Coal,       rt_None,       rt_None), //Metallurgist
(rt_Corn,       rt_None,       rt_None,       rt_None), //Swine
(rt_Stone,      rt_None,       rt_None,       rt_None), //Watch tower
(rt_Gold,       rt_None,       rt_None,       rt_None), //Town hall
(rt_Wood,       rt_None,       rt_None,       rt_None), //Weapon workshop
(rt_Wood,       rt_Leather,    rt_None,       rt_None), //Armor workshop
(rt_Warfare,    rt_None,       rt_None,       rt_None), //Barracks
(rt_Corn,       rt_None,       rt_None,       rt_None), //Mill
(rt_Wood,       rt_Steel,      rt_None,       rt_None), //Siege workshop
(rt_Pig,        rt_None,       rt_None,       rt_None), //Butcher
(rt_Skin,       rt_None,       rt_None,       rt_None), //Tannery
(rt_None,       rt_None,       rt_None,       rt_None), //N/A
(rt_Bread,      rt_Sausages,   rt_Wine,       rt_Fish), //Inn
(rt_None,       rt_None,       rt_None,       rt_None){, //Wineyard
(rt_None,       rt_None,       rt_None,       rt_None)}  //Wall
);

  var
    fResource:TResource;

    procedure ExportRX2BMP(RXid:integer);
    procedure ExportTreeAnim2BMP;
    procedure ExportHouseAnim2BMP;
    procedure ExportUnitAnim2BMP;


implementation
uses KromUtils, KM_Render, KM_Utils, KM_TGATexture;


constructor TResource.Create(aLocale:string; aLS:TNotifyEvent; aLT:TStringEvent);
begin
  Inherited Create;
  fDataState := dls_None;
  fLog.AppendLog('Resource loading state - None');

  OnLoadingStep := aLS;
  OnLoadingText := aLT;

  RXData[1].Title:='Trees';       RXData[1].NeedTeamColors:=false;
  RXData[2].Title:='Houses';      RXData[2].NeedTeamColors:=true;
  RXData[3].Title:='Units';       RXData[3].NeedTeamColors:=true;
  RXData[4].Title:='GUI';         RXData[4].NeedTeamColors:=true; //Required for unit scrolls and icons
  RXData[5].Title:='GUIMain';     RXData[5].NeedTeamColors:=false;
  RXData[6].Title:='GUIMainH';    RXData[6].NeedTeamColors:=false;
  RXData[7].Title:='Remake';      RXData[7].NeedTeamColors:=true;

  LoadMenuResources(aLocale);
end;


destructor TResource.Destroy;
begin
  if fHouseDat <> nil then FreeAndNil(fHouseDat);
  Inherited;
end;


procedure TResource.StepRefresh;
begin
  if Assigned(OnLoadingStep) then OnLoadingStep(Self);
end;


procedure TResource.StepCaption(aCaption:string);
begin
  if Assigned(OnLoadingText) then OnLoadingText(aCaption);
end;


procedure TResource.LoadMenuResources(aLocale:string);
var i:integer;
begin
  Assert(fTextLibrary <> nil, 'fTextLibrary should be init before ReadGFX');
  Assert(fRender <> nil, 'fRender should be init before ReadGFX to be able access OpenGL');

  StepCaption('Reading palettes ...');
  LoadPalettes;
  fLog.AppendLog('Reading palettes',true);

  for i:=4 to 6 do
  begin
    LoadRX(ExeDir+'data\gfx\res\'+RXData[i].Title+'.rx',i);
    LoadRX7(i); //Load RX data overrides

    if i=4 then MakeCursors(4);
    MakeGFX(i);
    ClearUnusedGFX(i);

    StepRefresh;
  end;

  AllocateRX(7, RX7_SPRITE_COUNT);
  LoadRX7(7); //Load RX7 data (custom bitmaps)
  MakeGFX(7);
  ClearUnusedGFX(7);

  AllocateRX(8, 256); //Terrain tiles are loaded later as RX8

  StepCaption('Reading fonts ...');
  LoadFonts(false, aLocale);
  fLog.AppendLog('Read fonts is done');

  StepRefresh;
  fLog.AppendLog('ReadGFX is done');
  fDataState:=dls_Menu;
  fLog.AppendLog('Resource loading state - Menu');
end;


procedure TResource.LoadGameResources;
var i:integer;
begin
  if fDataState = dls_All then Exit;

  Assert(fTextLibrary<>nil,'fTextLibrary should be init before ReadGFX');
  Assert(fRender<>nil,'fRender should be init before ReadGFX to be able access OpenGL');

  StepCaption('Reading defines ...');
  LoadMapElemDAT(ExeDir+'data\defines\mapelem.dat'); StepRefresh;
  LoadPatternDAT(ExeDir+'data\defines\pattern.dat'); StepRefresh;

  fHouseDat := TKMHouseDatCollection.Create;
  fHouseDat.LoadHouseDat(ExeDir+'data\defines\houses.dat');
  StepRefresh;

  LoadUnitDAT(ExeDir+'data\defines\unit.dat');       StepRefresh;

  for i:=1 to 3 do
    if (i=1) or ((i=2) and MAKE_HOUSE_SPRITES) or ((i=3) and MAKE_UNIT_SPRITES) then
    begin
      StepCaption('Reading '+RXData[i].Title+' GFX ...');
      fLog.AppendLog('Reading '+RXData[i].Title+'.rx',LoadRX(ExeDir+'data\gfx\res\'+RXData[i].Title+'.rx',i));
      LoadRX7(i); //Updated sprites
      MakeGFX(i);
      //Alpha_tested sprites for houses. They come after MakeGFX cos they will
      //replace above data.
      if i=2 then MakeGFX_AlphaTest(i);
      ClearUnusedGFX(i);
      StepRefresh;
    end;

  StepCaption('Making minimap colors ...');
  MakeMiniMapColors(ExeDir+'Resource\Tiles1.tga');
  fLog.AppendLog('Prepared MiniMap colors...');
  StepRefresh;
  fDataState:=dls_All;
  fLog.AppendLog('Resource loading state - Game');
end;


procedure TResource.LoadFonts(DoExport:boolean; aLocale:string);
var i:TKMFont;
begin
  for i:=low(TKMFont) to high(TKMFont) do
  if FileExists(ExeDir+FONTS_FOLDER+FontFiles[i]+'.'+aLocale+'.fnt') then
    LoadFont(ExeDir+FONTS_FOLDER+FontFiles[i]+'.'+aLocale+'.fnt', i, DoExport)
  else
    LoadFont(ExeDir+FONTS_FOLDER+FontFiles[i]+'.fnt', i, DoExport);
end;


function TResource.GetColor32(aIndex:byte; aPal:TKMPal=DEF_PAL):cardinal;
begin
  Result := Pal[aPal,aIndex,1] + Pal[aPal,aIndex,2] shl 8 + Pal[aPal,aIndex,3] shl 16 OR $FF000000;
end;


function TResource.GetUnitSequenceLength(aUnitType:TUnitType; aAction:TUnitActionType; aDir:TKMDirection):smallint;
begin
  Result := UnitSprite[byte(aUnitType)].Act[byte(aAction)].Dir[byte(aDir)].Count;
end;



//=============================================
//Reading Palette for trees/objects
//=============================================
function TResource.LoadPalettes:boolean;
var f:file; i:TKMPal; k:integer; FileName:string;
begin
  Result := true;

  for i:=low(TKMPal) to high(TKMPal) do begin

    FileName := ExeDir+'data\gfx\'+PalFiles[i];
    if FileExists(FileName) then begin
      AssignFile(f,FileName);
      FileMode := 0;
      Reset(f,1);
      FileMode := 2;
      blockread(f,Pal[i],48); //Unknown and/or not important
      blockread(f,Pal[i],768); //256*3
      closefile(f);

      if i = pal_lin then //Make greyscale linear Pal
        for k:=0 to 255 do begin
          Pal[pal_lin,k,1] := k;
          Pal[pal_lin,k,2] := k;
          Pal[pal_lin,k,3] := k;
        end;
    end else
      Result := false;
  end;
end;


//=============================================
//Reading map elements (has animation data)
//=============================================
function TResource.LoadMapElemDAT(FileName:string):boolean;
var ii,kk:integer; ft:textfile; f:file;
begin
  Result:=false;
  if not CheckFileExists(FileName) then exit;
  assignfile(f,FileName); reset(f,1);
  blockread(f,MapElem[1],MapElemQty*99);
  closefile(f);

  ActualMapElemQty:=0;
  for ii:=1 to MapElemQty do
  if (MapElem[ii].Count>0)and(MapElem[ii].Step[1]>0) then
  begin
    inc(ActualMapElemQty);
    ActualMapElem[ActualMapElemQty] := ii; //pointer
    OriginalMapElem[ii] := ActualMapElemQty; //Reverse lookup
  end;

  if WriteResourceInfoToTXT then begin
    assignfile(ft,ExeDir+'Trees.txt'); rewrite(ft);
    for ii:=1 to MapElemQty do begin
    writeln(ft);
    writeln(ft,inttostr(ii)+' :'+inttostr(MapElem[ii].Count));
      for kk:=1 to 30 do if MapElem[ii].Step[kk]>0 then
      write(ft,MapElem[ii].Step[kk],'.') else write(ft,'_.');

      writeln(ft);
//      for kk:=1 to 16 do
//      write(ft,MapElem[ii].CuttableTree,''); //Those are 1/0 so we can ommit space between them

      write(ft,' =',MapElem[ii].CanBeRemoved);
      writeln(ft);
    end;
    closefile(ft);
  end;

  Result:=true;
end;


//=============================================
//Reading pattern data (tile info)
//=============================================
function TResource.LoadPatternDAT(FileName:string):boolean;
var ii,kk:integer; ft:textfile; f:file; s:byte;
begin
  Result:=false;
  if not CheckFileExists(FileName) then exit;
  assignfile(f,FileName); reset(f,1);
  blockread(f,PatternDAT[1],6*256);
  for ii:=1 to 30 do begin
    blockread(f,TileTable[ii,1],30*10);
    blockread(f,s,1);
    if s<>0 then
      s:=s;
  end;

  closefile(f);

  if WriteResourceInfoToTXT then begin
    assignfile(ft,ExeDir+'Pattern.csv');
    rewrite(ft);
    writeln(ft,'PatternDAT');
    for ii:=0 to 15 do begin
      for kk:=1 to 16 do
        write(ft,inttostr(ii*16+kk),' ',PatternDAT[ii*16+kk].TileType,';');
      writeln(ft);
    end;
    writeln(ft,'TileTable');
    for ii:=1 to 30 do begin
      for kk:=1 to 30 do begin
      write(ft,inttostr(TileTable[ii,kk].Tile1)+'_'+inttostr(TileTable[ii,kk].Tile2)+'_'+inttostr(TileTable[ii,kk].Tile3)+' ');
      write(ft,inttostr(byte(TileTable[ii,kk].b1)));
      write(ft,inttostr(byte(TileTable[ii,kk].b2)));
      write(ft,inttostr(byte(TileTable[ii,kk].b3)));
      write(ft,inttostr(byte(TileTable[ii,kk].b4)));
      write(ft,inttostr(byte(TileTable[ii,kk].b5)));
      write(ft,inttostr(byte(TileTable[ii,kk].b6)));
      write(ft,inttostr(byte(TileTable[ii,kk].b7)));
      write(ft,';');
      end;

      writeln(ft);
    end;
    closefile(ft);
  end;

  Result:=true;
end;


//=============================================
//Reading unit.dat data
//=============================================
function TResource.LoadUnitDAT(FileName:string):boolean;
var ii,kk,jj,hh:integer; ft:textfile; f:file;
begin
  Result := false;

  if not CheckFileExists(FileName) then exit;
  assignfile(f,FileName); reset(f,1);

  for ii:=1 to 28 do
    blockread(f,SerfCarry[ii],8*70);

  for ii:=1 to 41 do begin
    blockread(f,UnitStat[ii],22);
    blockread(f,UnitSprite[ii],112*70);
    blockread(f,UnitSprite2[ii],36);
  end;

  closefile(f);

  if WriteResourceInfoToTXT then begin
    assignfile(ft,ExeDir+'UnitDAT.csv'); rewrite(ft);
    writeln(ft,'Name;HitPoints;Attack;AttackHorseBonus;x4;Defence;Speed;x7;Sight;x9;x10;CanWalkOut;x11;');
    for ii:=1 to 40 do begin
      write(ft,fTextLibrary.GetTextString(siUnitNames+ii)+';');
      write(ft,inttostr(UnitStat[ii].HitPoints)+';');
      write(ft,inttostr(UnitStat[ii].Attack)+';');
      write(ft,inttostr(UnitStat[ii].AttackHorseBonus)+';');
      write(ft,inttostr(UnitStat[ii].x4)+';');
      write(ft,inttostr(UnitStat[ii].Defence)+';');
      write(ft,inttostr(UnitStat[ii].Speed)+';');
      write(ft,inttostr(UnitStat[ii].x7)+';');
      write(ft,inttostr(UnitStat[ii].Sight)+';');
      write(ft,inttostr(UnitStat[ii].x9)+';');
      write(ft,inttostr(UnitStat[ii].x10)+';');
      write(ft,inttostr(UnitStat[ii].CanWalkOut)+';');
      write(ft,inttostr(UnitStat[ii].x11)+';');
      for kk:=1 to 18 do
        write(ft,inttostr(UnitSprite2[ii,kk])+';');
      writeln(ft);
    end;
    closefile(ft);

    assignfile(ft,ExeDir+'Units.txt'); rewrite(ft);
    for ii:=1 to 40 do begin
    writeln(ft);
    writeln(ft);
    writeln(ft,'NewUnit'+inttostr(ii));
    for kk:=1 to 14 do
    for hh:=1 to 8 do
    //  if UnitSprite[ii].Act[kk].Dir[hh].Step[1]>0 then
        begin
          write(ft,inttostr(kk)+'.'+inttostr(hh)+#9);
          for jj:=1 to 30 do
          if UnitSprite[ii].Act[kk].Dir[hh].Step[jj]>0 then //write(ft,'#');
          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].Step[jj])+'. ');
          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].Count)+' ');
          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].MoveX)+' ');
          write(ft,inttostr(UnitSprite[ii].Act[kk].Dir[hh].MoveY)+' ');
          writeln(ft);
        end;
    end;
    closefile(ft);
  end;
Result:=true;
end;


function TResource.LoadFont(FileName:string; aFont:TKMFont; WriteFontToBMP:boolean):boolean;
const
  TexWidth=256; //Connected to TexData, don't change
var
  f:file;
  L:byte;
  i,k,ci,ck:integer;
  MaxHeight:integer;
  AdvX,AdvY:integer;
  TD:array of cardinal;
  MyBitMap:TBitMap;
begin
  Result:=false;
  MaxHeight:=0;
  if not CheckFileExists(FileName, true) then exit;

  assignfile(f,FileName); reset(f,1);
  blockread(f,FontData[aFont].Unk1,8);
  blockread(f,FontData[aFont].Pal[0],256);

  //Read font data
  for i:=0 to 255 do
    if FontData[aFont].Pal[i]<>0 then
      with FontData[aFont].Letters[i] do begin
        blockread(f,Width,4);
        blockread(f,Add1,8);
        MaxHeight:=Math.max(MaxHeight,Height);
        fLog.AssertToLog(Width*Height<>0,'Font data Width*Height <> 0'); //Font01.fnt seems to be damaged..
        setlength(Data, Width*Height);
        blockread(f,Data[0],Width*Height);
      end;
  closefile(f);

  //Special fixes: for monochrome fonts
  if FontPal[aFont]=pal_lin then
  for i:=0 to 255 do
    if FontData[aFont].Pal[i]<>0 then //see if letterspace is used
      for k:=0 to length(FontData[aFont].Letters[i].Data)-1 do
        if FontData[aFont].Letters[i].Data[k]<>0 then
          FontData[aFont].Letters[i].Data[k]:=255; //Full white


  //Compile texture
  AdvX:=0; AdvY:=0;
  setlength(TD,TexWidth*TexWidth);

  for i:=0 to 255 do
    if FontData[aFont].Pal[i]<>0 then
      with FontData[aFont].Letters[i] do begin

      fLog.AssertToLog(FontData[aFont].Pal[i]=1,'FontData palette <> 1');

        if AdvX+Width+2>TexWidth then begin
          AdvX:=0;
          inc(AdvY,MaxHeight);
        end;

        for ci:=0 to Height-1 do for ck:=0 to Width-1 do begin
          L := Data[ci*Width+ck]; //0..255
          if L<>0 then //Transparent
            TD[(AdvY+ci)*TexWidth+AdvX+1+ck] := GetColor32(L,FontPal[aFont]);
        end;

        u1 := (AdvX+1)/TexWidth;
        v1 := AdvY/TexWidth;
        u2 := (AdvX+1+Width)/TexWidth;
        v2 := (AdvY+Height)/TexWidth;

        inc(AdvX,1+Width+1);
      end;

    FontData[aFont].TexID := GenTexture(TexWidth,TexWidth,@TD[0],tm_TexID);

  //for i:=1 to 10 do
  if WriteFontToBMP then begin
    MyBitMap:=TBitMap.Create;
    MyBitMap.PixelFormat:=pf24bit;
    MyBitMap.Width:=TexWidth;
    MyBitMap.Height:=TexWidth;

    for ci:=0 to TexWidth-1 do for ck:=0 to TexWidth-1 do begin
      MyBitMap.Canvas.Pixels[ck,ci]:= TD[ci*TexWidth+ck] AND $FFFFFF;
    end;

    CreateDir(ExeDir+'Export\');
    CreateDir(ExeDir+'Export\Fonts\');
    MyBitMap.SaveToFile(ExeDir+'Export\Fonts\'+ExtractFileName(FileName)+PalFiles[FontPal[aFont]]+'.bmp');
    MyBitMap.Free;
  end;

  setlength(TD,0);
  Result := true;
end;


{ This function should parse all valid files in Sprites folder and load them
  additionaly to or replacing original sprites }
procedure TResource.LoadRX7(RX:integer);
{$IFDEF WDC}
var
  FileList:TStringList;
  SearchRec:TSearchRec;
  i:integer; x,y:integer;
  ID:integer; p:cardinal;
  po:TPNGObject;
  {$ENDIF}
begin
  {$IFDEF WDC}
  if not DirectoryExists(ExeDir + 'Sprites\') then exit;

  FileList := TStringList.Create;
  ChDir(ExeDir + 'Sprites\');
  FindFirst('*', faAnyFile, SearchRec);
  repeat
    if (SearchRec.Name<>'.')and(SearchRec.Name<>'..') then //Exclude parent folders
    if SearchRec.Attr and faDirectory <> faDirectory then
    if GetFileExt(SearchRec.Name) = 'PNG' then
    if StrToIntDef(SearchRec.Name[1],0) = RX then
      FileList.Add(SearchRec.Name);
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);

  //#-####.png - default texture
  //#-####a.png - alternative texture
  
  for i:=0 to FileList.Count-1 do begin

    ID := StrToIntDef(Copy(FileList.Strings[i], 3, 4),0); //wrong file will return 0
    if InRange(ID,1,RXData[RX].Qty) then begin //Replace only certain sprites
      if Copy(FileList.Strings[i], 7, 1) = 'a' then
        RXData[RX].HasMask[i] := true //todo: [Krom] Support alternative textures
      else
        RXData[RX].HasMask[i] := false;
      po := TPNGObject.Create;
      po.LoadFromFile(ExeDir + 'Sprites\' + FileList.Strings[i]);

      RXData[RX].Size[ID].X := po.Width;
      RXData[RX].Size[ID].Y := po.Height;

      setlength(RXData[RX].RGBA[ID], po.Width*po.Height);
      setlength(RXData[RX].Mask[ID], po.Width*po.Height); //Should allocate space for it's always comes along

      case po.TransparencyMode of //There are ways to process PNG transparency
        ptmNone:
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
            RXData[RX].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR $FF000000;
        ptmBit:
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do
            if po.Pixels[x,y] = po.TransparentColor then
              RXData[RX].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) AND $FFFFFF //avoid black edging
            else
              RXData[RX].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR $FF000000;
        ptmPartial:
          for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do begin
            p := (po.AlphaScanline[y]^[x]) shl 24; //semitransparency is killed by render later-on
            RXData[RX].RGBA[ID, y*po.Width+x] := cardinal(po.Pixels[x,y]) OR p;
          end;
        else Assert(false, 'Unknown PNG transparency mode')
      end;

      //todo: Apply team colour masks after loading
      //@Krom: I'm struggling a bit here... do you think you could implement alternative textures for
      //       custom PNG images? Delete my attempt if it's wrong, I tried copying and modifying it.
      //@Lewin: I'm not sure how to make it to work so that custom 24bit images were easy to edit and
      //        loaded without errors in color areas recognition.
      //todo: Think about using grey masks for flag colors (see above)

      {for y:=0 to po.Height-1 do for x:=0 to po.Width-1 do begin
          L := RXData[RX].RGBA[ID,y*po.Width+x];
          if RXData[RX].HasMask[i] and (L in[GetColor32(23),GetColor32(24),GetColor32(25),GetColor32(26),
                                             GetColor32(27),GetColor32(28),GetColor32(29)]) then
          begin
            RGBA[i,Pixel] := cardinal(((L-26)*42+128)*65793) OR $FF000000;
            case L of
              GetColor32(23),GetColor32(29):  RXData[RX].Mask[i,Pixel] := $60FFFFFF;   //7  //6
              GetColor32(24),GetColor32(28):  RXData[RX].Mask[i,Pixel] := $90FFFFFF;   //11 //9
              GetColor32(25),GetColor32(27):  RXData[RX].Mask[i,Pixel] := $C0FFFFFF;   //14 //12
              GetColor32(26):                 RXData[RX].Mask[i,Pixel] := $FFFFFFFF;   //16 //16
            end;
            HasMask[i] := true;
          end;
      end;}

      po.Free;
    end;
  end;

  FileList.Free;
  {$ENDIF}
end;


procedure TResource.MakeTileGFXFromTexture(Texture:GLuint);
var i: integer;
begin
  for i:=0 to 255 do
    with GFXData[8,i+1] do
    begin
      TexID := Texture;
      v1 := (i div 16  ) / 16; //There are 16 tiles across the line
      u1 := (i mod 16  ) / 16;
      v2 := (i div 16+1) / 16;
      u2 := (i mod 16+1) / 16;
      PxWidth := 32;
      PxHeight := 32;
    end;
end;


procedure TResource.AllocateRX(ID:integer; Count:integer=0);
begin
  if Count>0 then
    RXData[ID].Qty := Count;

  Count := RXData[ID].Qty+1;
  setlength(GFXData[ID],        Count);
  setlength(RXData[ID].Flag,    Count);
  setlength(RXData[ID].Size,    Count);
  setlength(RXData[ID].Pivot,   Count);
  setlength(RXData[ID].Data,    Count);
  setlength(RXData[ID].RGBA,    Count);
  setlength(RXData[ID].Mask,    Count);
  setlength(RXData[ID].HasMask, Count);
end;


//=============================================
//Reading RX Data
//=============================================
function TResource.LoadRX(FileName:string; ID:integer):boolean;
var i:integer; f:file;
begin
  Result:=false;
  if not CheckFileExists(FileName) then exit;

  assignfile(f,FileName); reset(f,1);
  blockread(f, RXData[ID].Qty, 4);
  AllocateRX(ID);
  blockread(f, RXData[ID].Flag[1], RXData[ID].Qty);

  if (not LOAD_UNIT_RX_FULL)and(RXData[ID].Title = 'Units') then RXData[ID].Qty:=7885;

  for i:=1 to RXData[ID].Qty do
  if RXData[ID].Flag[i] = 1 then
  begin
    blockread(f, RXData[ID].Size[i].X, 4);
    blockread(f, RXData[ID].Pivot[i].x, 8);
    setlength(RXData[ID].Data[i], RXData[ID].Size[i].X * RXData[ID].Size[i].Y);
    blockread(f, RXData[ID].Data[i,0], RXData[ID].Size[i].X * RXData[ID].Size[i].Y);
  end;
  closefile(f);
  fLog.AppendLog(RXData[ID].Title+' -',RXData[ID].Qty);

  ExpandRX(ID);   
  Result:=true;
end;


//=============================================
// Expand RX Data
//Convert paletted data into RGBA and select Team color layer from it
//=============================================
procedure TResource.ExpandRX(ID:integer);
var i:integer; x,y:integer; Palette:TKMPal; L:byte; Pixel:integer;
begin
  with RXData[ID] do
  for i:=1 to Qty do begin

    case ID of
      5: Palette := RX5Pal[i];
      6: Palette := RX6Pal[i];
      else Palette := DEF_PAL;
    end;

    if Flag[i] = 1 then begin
      setlength(RGBA[i], Size[i].X*Size[i].Y);
      setlength(Mask[i], Size[i].X*Size[i].Y);

      for y:=0 to Size[i].Y-1 do for x:=0 to Size[i].X-1 do
      begin
        Pixel := y*Size[i].X + x;
        L := Data[i, Pixel]; //0..255

        if L<>0 then
          if NeedTeamColors and (L in[23..29]) //Only unit icons and scrolls in RX=4
          and ((ID<>4) or InRange(i,141,154) or InRange(i,521,550)) then
          begin
            RGBA[i,Pixel] := cardinal(((L-26)*42+128)*65793) OR $FF000000;
            case L of //Maybe it makes sense to convert to 8bit?
              23,29:  Mask[i,Pixel] := $60FFFFFF;   //7  //6
              24,28:  Mask[i,Pixel] := $90FFFFFF;   //11 //9
              25,27:  Mask[i,Pixel] := $C0FFFFFF;   //14 //12
              26:     Mask[i,Pixel] := $FFFFFFFF;   //16 //16
            end;
            HasMask[i] := true;
          end else
            RGBA[i,Pixel] := GetColor32(L, Palette);
      end;
   end;
  end;
end;


//=============================================
//Make texture
//=============================================
function TResource.GenTexture(DestX, DestY:word; const Data:TCardinalArray2; Mode:TexMode):gluint;
var
  MyBitMap:TBitMap;
  i,k:word;
begin

  Result := 0;

  DestX := MakePOT(DestX);
  DestY := MakePOT(DestY);
  if DestX*DestY = 0 then exit; //Do not generate zeroed textures

  Result := GenerateTextureCommon; //Should be called prior to glTexImage2D or gluBuild2DMipmaps

  case Mode of
    //Houses under construction
    tm_AlphaTest: glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,    DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
    //Base layer
    tm_TexID:     glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB5_A1, DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
    //Team color layer
    tm_AltID:     glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA2,   DestX, DestY, 0, GL_RGBA, GL_UNSIGNED_BYTE, Data);
  end;

  if WriteAllTexturesToBMP then begin
    CreateDir(ExeDir+'Export\GenTextures\');
    MyBitMap:=TBitMap.Create;
    MyBitMap.PixelFormat:=pf24bit;
    MyBitMap.Width:=DestX;
    MyBitMap.Height:=DestY;

    for i:=0 to DestY-1 do for k:=0 to DestX-1 do
      MyBitMap.Canvas.Pixels[k,i] := ((PCardinal(Cardinal(@Data[0])+(i*DestX+k)*4))^) AND $FFFFFF; //Ignore alpha
    MyBitMap.SaveToFile(ExeDir+'Export\GenTextures\'+int2fix(Result,4)+'.bmp');

    if Mode=tm_AlphaTest then begin //these Alphas are worth looking at
      for i:=0 to DestY-1 do for k:=0 to DestX-1 do
        MyBitMap.Canvas.Pixels[k,i] := ((PCardinal(Cardinal(@Data[0])+(i*DestX+k)*4))^) SHR 24 *65793;
      MyBitMap.SaveToFile(ExeDir+'Export\GenTextures\'+int2fix(Result,4)+'a.bmp');
    end;

    MyBitMap.Free;
  end;

end;


//=============================================
//Making OpenGL textures
//=============================================
{Take RX data and make nice textures out of it.
Textures should be POT to improve performance and avoid drivers bugs
In result we have GFXData filled.}
procedure TResource.MakeGFX_AlphaTest(RXid:integer);
var
  ID:THouseType;
  ID1,ID2:integer; //RGB and A index
  i,k,h,StepCount:integer;
  t,tx,ty:integer;
  ColorID:byte;
  WidthPOT,HeightPOT:integer;
  TD:array of cardinal;
begin
  for ID:=Low(THouseType) to High(THouseType) do
    if HouseDat[ID].IsValid then

      for h:=1 to 2 do begin
        if h=1 then begin
          ID1 := HouseDAT[ID].WoodPic+1;
          ID2 := HouseDAT[ID].WoodPal+1;
          StepCount := HouseDAT[ID].WoodPicSteps;
        end else begin
          ID1 := HouseDAT[ID].StonePic+1;
          ID2 := HouseDAT[ID].StonePal+1;
          StepCount := HouseDAT[ID].StonePicSteps;
        end;

        WidthPOT  := MakePOT(RXData[RXid].Size[ID1].X);
        HeightPOT := MakePOT(RXData[RXid].Size[ID1].Y);
        setlength(TD, WidthPOT*HeightPOT);

        //Fill in colors data
        for i := 0 to RXData[RXid].Size[ID1].Y-1 do
        for k := 0 to RXData[RXid].Size[ID1].X-1 do begin
          ColorID := RXData[RXid].Data[ID1,i*RXData[RXid].Size[ID1].X+k]; //0..255
          if ColorID<>0 then
            TD[i*WidthPOT+k] := GetColor32(ColorID, DEF_PAL);
        end;

        //Apply mask to where colors are (yes, it needs to be done in 2 steps, since offsets mismatch)
        tx := RXData[RXid].Pivot[ID2].x-RXData[RXid].Pivot[ID1].x;
        ty := (RXData[RXid].Pivot[ID2].y-RXData[RXid].Pivot[ID1].y)*WidthPOT;
        for i := 0 to RXData[RXid].Size[ID2].Y-1 do
        for k := 0 to RXData[RXid].Size[ID2].X-1 do begin
          t := i*WidthPOT+k + tx + ty; //Shift by pivot, always positive
          ColorID := RXData[RXid].Data[ID2,i*RXData[RXid].Size[ID2].X+k];
          if (ColorID<>0) and (TD[t]<>0) then
            TD[t] := TD[t] AND ($FFFFFF OR (255-round(ColorID*(255/StepCount))) shl 24);
        end;

        GFXData[RXid,ID1].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_AlphaTest);
        setlength(TD,0);
        GFXData[RXid,ID1].AltID := 0;
        GFXData[RXid,ID1].u1    := 0;
        GFXData[RXid,ID1].v1    := 0;
        GFXData[RXid,ID1].u2    := RXData[RXid].Size[ID1].X/WidthPOT;
        GFXData[RXid,ID1].v2    := RXData[RXid].Size[ID1].Y/HeightPOT;
        GFXData[RXid,ID1].PxWidth:=RXData[RXid].Size[ID1].X;
        GFXData[RXid,ID1].PxHeight:=RXData[RXid].Size[ID1].Y;
      end;
end;


//=============================================
//Making OpenGL textures
//=============================================
{Take RX data and make nice textures out of it.
Textures should be POT to improve performance and avoid driver bugs
In result we have GFXData filled.}
procedure TResource.MakeGFX(RXid:integer);
var
  ci,j,i,k,LeftIndex,RightIndex,TexCount,SpanCount:integer;
  AllocatedRAM,RequiredRAM,ColorsRAM:cardinal;
  WidthPOT,HeightPOT:word;
  TD:array of cardinal;
  TA:array of cardinal;
  hm:boolean;
begin
  LeftIndex:=0; AllocatedRAM:=0; RequiredRAM:=0; ColorsRAM:=0; TexCount:=0;
  repeat
    inc(LeftIndex);

    WidthPOT  := RXData[RXid].Size[LeftIndex].X;
    HeightPOT := MakePOT(RXData[RXid].Size[LeftIndex].Y);
    SpanCount := 1;

    //Pack textures with same POT height into rows to save memory
    //This also means fewer textures for GPU RAM == better performance
    while((LeftIndex+SpanCount<RXData[RXid].Qty)and //Keep packing until end of sprites
          (
            (HeightPOT=MakePOT(RXData[RXid].Size[LeftIndex+SpanCount].Y)) //Pack if HeightPOT matches
        or((HeightPOT>=MakePOT(RXData[RXid].Size[LeftIndex+SpanCount].Y))AND(WidthPOT+RXData[RXid].Size[LeftIndex+SpanCount].X<MakePOT(WidthPOT)))
          )and
          (WidthPOT+RXData[RXid].Size[LeftIndex+SpanCount].X<=MAX_TEX_RESOLUTION)) //Pack until max Tex_Resolution approached
    do begin
      inc(WidthPOT,RXData[RXid].Size[LeftIndex+SpanCount].X);
      if (RXid=5)and(RX5Pal[LeftIndex]<>RX5Pal[LeftIndex+SpanCount]) then break; //Don't align RX5 images for they use all different palettes
      if (RXid=6)and(RX6Pal[LeftIndex]<>RX6Pal[LeftIndex+SpanCount]) then break; //Don't align RX6 images for they use all different palettes
      inc(SpanCount);
    end;

    RightIndex := LeftIndex+SpanCount-1;
    WidthPOT := MakePOT(WidthPOT);
    setlength(TD,WidthPOT*HeightPOT+1);
    setlength(TA,WidthPOT*HeightPOT+1);

    for i:=0 to HeightPOT-1 do begin
      ci:=0;
      for j:=LeftIndex to RightIndex do
        for k:=0 to RXData[RXid].Size[j].X-1 do begin
          if i<RXData[RXid].Size[j].Y then begin
            //CopyMemory(TD[(i-1)*WidthPOT+ci-1], RXData[RXid].RGBA[j,(i-1)*RXData[RXid].Size[j].X+k-1], )
            TD[i*WidthPOT+ci] := RXData[RXid].RGBA[j,i*RXData[RXid].Size[j].X+k];
            TA[i*WidthPOT+ci] := RXData[RXid].Mask[j,i*RXData[RXid].Size[j].X+k];
          end;
          inc(ci);
        end;
    end;

    hm:=false;
    for j:=LeftIndex to RightIndex do
      hm := hm or RXData[RXid].HasMask[j];

    //If we need to prepare textures for TeamColors          //special fix for iron mine logo
    if MAKE_TEAM_COLORS and RXData[RXid].NeedTeamColors and (not ((RXid=4)and InRange(49,LeftIndex,RightIndex))) then
    begin
      GFXData[RXid,LeftIndex].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_TexID);
      //TeamColors are done through alternative plain colored texture
      if hm then begin
        GFXData[RXid,LeftIndex].AltID := GenTexture(WidthPOT,HeightPOT,@TA[0],tm_AltID);
        inc(ColorsRAM,WidthPOT*HeightPOT*4);
      end;
    end
    else
      GFXData[RXid,LeftIndex].TexID := GenTexture(WidthPOT,HeightPOT,@TD[0],tm_TexID);

    setlength(TD,0);
    setlength(TA,0);

    k:=0;
    for j:=LeftIndex to RightIndex do begin //Hack to test AlphaTest
      GFXData[RXid,j].TexID:=GFXData[RXid,LeftIndex].TexID;
      GFXData[RXid,j].AltID:=GFXData[RXid,LeftIndex].AltID;
      GFXData[RXid,j].u1:=k/WidthPOT;
      GFXData[RXid,j].v1:=0;
      inc(k,RXData[RXid].Size[j].X);
      GFXData[RXid,j].u2:=k/WidthPOT;
      GFXData[RXid,j].v2:=RXData[RXid].Size[j].Y/HeightPOT;
      GFXData[RXid,j].PxWidth:=RXData[RXid].Size[j].X;
      GFXData[RXid,j].PxHeight:=RXData[RXid].Size[j].Y;

      inc(RequiredRAM,RXData[RXid].Size[j].X*RXData[RXid].Size[j].Y*4);
    end;

    inc(AllocatedRAM,WidthPOT*HeightPOT*4);
    inc(LeftIndex,SpanCount-1);
    inc(TexCount);

  until(LeftIndex>=RXData[RXid].Qty); // >= in case data wasn't loaded and Qty=0

  fLog.AppendLog(inttostr(TexCount)+' Textures created');
  fLog.AddToLog(inttostr(AllocatedRAM div 1024)+'/'+inttostr((AllocatedRAM-RequiredRAM) div 1024)+' Kbytes allocated/wasted for units GFX when using Packing');
  fLog.AddToLog(inttostr(ColorsRAM div 1024)+' KBytes for team colors');
end;


//Now we can safely dispose of RXData[RXid].Data to save us some more RAM
procedure TResource.ClearUnusedGFX(RXid:integer);
var i:integer;
begin
  for i:=1 to RXData[RXid].Qty do begin
    setlength(RXData[RXid].Data[i],0);
    setlength(RXData[RXid].RGBA[i],0);
    setlength(RXData[RXid].Mask[i],0);
  end;
end;


//=============================================
//Export RX to Bitmaps
//=============================================
{That is when we want to export RX to Bitmaps without need to have GraphicsEditor, also this way we preserve image indexes}
procedure ExportRX2BMP(RXid:integer);
var MyBitMap:TBitMap;
    id:integer; t:byte;
    sy,sx,y,x:integer;
    UsePal:TKMPal;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\'+RXData[RXid].Title+'.rx\');
  MyBitMap := TBitMap.Create;
  MyBitMap.PixelFormat := pf24bit;

  fResource.LoadRX(ExeDir+'data\gfx\res\'+RXData[RXid].Title+'.rx',RXid);

  for id:=1 to RXData[RXid].Qty do begin

    sx := RXData[RXid].Size[id].X;
    sy := RXData[RXid].Size[id].Y;
    MyBitMap.Width  := sx;
    MyBitMap.Height := sy;

    case RXid of
      5:   UsePal := RX5Pal[id];
      6:   UsePal := RX6Pal[id];
      else UsePal := DEF_PAL;
    end;

    for y:=0 to sy-1 do for x:=0 to sx-1 do begin
      t := RXData[RXid].Data[id,y*sx+x];
      MyBitMap.Canvas.Pixels[x,y] := fResource.GetColor32(t,UsePal) AND $FFFFFF;
    end;
    if sy>0 then MyBitMap.SaveToFile(ExeDir+'Export\'+RXData[RXid].Title+'.rx\'+RXData[RXid].Title+'_'+int2fix(id,4)+'.bmp');

    setlength(RXData[RXid].Data[id],0);
  end;

  MyBitMap.Free;
end;

{Export Units graphics categorized by Unit and Action}
procedure ExportUnitAnim2BMP;
var MyBitMap:TBitMap;
    iUnit,iAct,iDir,iFrame,ci:integer; t:byte;
    sy,sx,y,x:integer;
    Used:array of integer;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\UnitAnim\');
  MyBitMap:=TBitMap.Create;
  MyBitMap.PixelFormat:=pf24bit;

  fResource.LoadUnitDAT(ExeDir+'data\defines\unit.dat');
  fResource.LoadRX(ExeDir+'data\gfx\res\'+RXData[3].Title+'.rx',3);

  ci:=0;
  for iUnit:=byte(ut_Militia) to byte(ut_Militia) do begin
    for iAct:=1 to 14 do begin
      for iDir:=1 to 8 do if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[1]<>-1 then begin
        for iFrame:=1 to UnitSprite[iUnit].Act[iAct].Dir[iDir].Count do begin
          CreateDir(ExeDir+'Export\UnitAnim\'+TypeToString(TUnitType(iUnit))+'\');
          CreateDir(ExeDir+'Export\UnitAnim\'+TypeToString(TUnitType(iUnit))+'\'+UnitAct[iAct]+'\');
          if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1<>0 then
            ci:=UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1;

          sx:=RXData[3].Size[ci].X;
          sy:=RXData[3].Size[ci].Y;
          MyBitMap.Width:=sx;
          MyBitMap.Height:=sy;

          for y:=0 to sy-1 do for x:=0 to sx-1 do begin
            t := RXData[3].Data[ci,y*sx+x];
            MyBitMap.Canvas.Pixels[x,y]:=fResource.GetColor32(t,DEF_PAL) AND $FFFFFF;
          end;
          if sy>0 then MyBitMap.SaveToFile(
            ExeDir+'Export\UnitAnim\'+TypeToString(TUnitType(iUnit))+'\'+UnitAct[iAct]+'\'+inttostr(iDir)+'_'+int2fix(iFrame,2)+'.bmp');
        end;
      end;
    end;
  end;

  CreateDir(ExeDir+'Export\UnitAnim\_TheRest');
  setlength(Used,length(RXData[3].Size));
  for iUnit:=1 to 41 do
  for iAct:=1 to 14 do
  for iDir:=1 to 8 do if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[1]<>-1 then
  for iFrame:=1 to UnitSprite[iUnit].Act[iAct].Dir[iDir].Count do
  if UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1<>0 then
  Used[UnitSprite[iUnit].Act[iAct].Dir[iDir].Step[iFrame]+1]:=1;

  for iUnit:=1 to 28 do
  for iDir:=1 to 8 do if SerfCarry[iUnit].Dir[iDir].Step[1]<>-1 then
  for iFrame:=1 to SerfCarry[iUnit].Dir[iDir].Count do
  if SerfCarry[iUnit].Dir[iDir].Step[iFrame]+1<>0 then
  Used[SerfCarry[iUnit].Dir[iDir].Step[iFrame]+1]:=1;

  for ci:=1 to length(Used)-1 do
  if Used[ci]=0 then begin
    sx:=RXData[3].Size[ci].X;
    sy:=RXData[3].Size[ci].Y;
    MyBitMap.Width:=sx;
    MyBitMap.Height:=sy;

    for y:=0 to sy-1 do for x:=0 to sx-1 do begin
      t := RXData[3].Data[ci,y*sx+x];
      MyBitMap.Canvas.Pixels[x,y]:=fResource.GetColor32(t,DEF_PAL) AND $FFFFFF;
    end;
    if sy>0 then MyBitMap.SaveToFile(
      ExeDir+'Export\UnitAnim\_TheRest\'+'_'+int2fix(ci,4)+'.bmp');
  end;

  MyBitMap.Free;
end;


{Export Houses graphics categorized by House and Action}
procedure ExportHouseAnim2BMP;
var MyBitMap:TBitMap;
    ID:THouseType;
    Q,Beast,Ac,k,ci:integer; t:byte;
    sy,sx,y,x:integer;
    s:string;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\HouseAnim\');
  MyBitMap:=TBitMap.Create;
  MyBitMap.PixelFormat:=pf24bit;

  fResource.LoadGameResources;

  ci:=0;
  for ID:=ht_WatchTower to ht_WatchTower do begin
    for Ac:=1 to 5 do begin //Work1..Work5
      for k:=1 to fResource.HouseDat[ID].Anim[Ac].Count do begin
        CreateDir(ExeDir+'Export\HouseAnim\'+fResource.HouseDat[ID].HouseName+'\');
        CreateDir(ExeDir+'Export\HouseAnim\'+fResource.HouseDat[ID].HouseName+'\Work'+IntToStr(Ac)+'\');
        if fResource.HouseDat[ID].Anim[Ac].Step[k]+1<>0 then
        ci:=fResource.HouseDat[ID].Anim[Ac].Step[k]+1;

        sx:=RXData[2].Size[ci].X;
        sy:=RXData[2].Size[ci].Y;
        MyBitMap.Width:=sx;
        MyBitMap.Height:=sy;

        for y:=0 to sy-1 do for x:=0 to sx-1 do begin
          t:=RXData[2].Data[ci,y*sx+x];
          MyBitMap.Canvas.Pixels[x,y]:=fResource.GetColor32(t,DEF_PAL) AND $FFFFFF;
        end;
        if sy>0 then MyBitMap.SaveToFile(
        ExeDir+'Export\HouseAnim\'+fResource.HouseDat[ID].HouseName+'\Work'+IntToStr(Ac)+'\_'+int2fix(k,2)+'.bmp');
      end;
    end;
  end;

  ci:=0;
  for Q:=1 to 2 do begin
    if Q=1 then ID:=ht_Swine
           else ID:=ht_Stables;
    CreateDir(ExeDir+'Export\HouseAnim\_'+fResource.HouseDat[ID].HouseName+'\');
    for Beast:=1 to 5 do begin
      for Ac:=1 to 3 do begin //Age 1..3
        for k:=1 to fResource.HouseDat.BeastAnim[ID,Beast,Ac].Count do begin
          CreateDir(ExeDir+'Export\HouseAnim\'+s+'\'+int2fix(Beast,2)+'\');
          if fResource.HouseDat.BeastAnim[ID,Beast,Ac].Step[k]+1<>0 then
          ci:=fResource.HouseDat.BeastAnim[ID,Beast,Ac].Step[k]+1;

          sx:=RXData[2].Size[ci].X;
          sy:=RXData[2].Size[ci].Y;
          MyBitMap.Width:=sx;
          MyBitMap.Height:=sy;

          for y:=0 to sy-1 do for x:=0 to sx-1 do begin
            t:=RXData[2].Data[ci,y*sx+x];
            MyBitMap.Canvas.Pixels[x,y]:=fResource.GetColor32(t,DEF_PAL) AND $FFFFFF;
          end;
          if sy>0 then MyBitMap.SaveToFile(ExeDir+'Export\HouseAnim\_'+fResource.HouseDat[ID].HouseName+'\'+int2fix(Beast,2)+'\_'+int2fix(Ac,1)+'_'+int2fix(k,2)+'.bmp');
        end;
      end;
    end;
  end;

  MyBitMap.Free;
end;


{Export Trees graphics categorized by ID}
procedure ExportTreeAnim2BMP;
var MyBitMap:TBitMap;
    ID,k,ci:integer; t:byte;
    sy,sx,y,x:integer;
begin
  CreateDir(ExeDir+'Export\');
  CreateDir(ExeDir+'Export\TreeAnim\');
  MyBitMap:=TBitMap.Create;
  MyBitMap.PixelFormat:=pf24bit;

  fResource.LoadMapElemDAT(ExeDir+'data\defines\mapelem.dat');
  fResource.LoadRX(ExeDir+'data\gfx\res\'+RXData[1].Title+'.rx',1);

  ci:=0;
  for ID:=1 to MapElemQty do begin
    for k:=1 to MapElem[ID].Count do begin
      if MapElem[ID].Step[k]+1<>0 then
      ci:=MapElem[ID].Step[k]+1;

      sx:=RXData[1].Size[ci].X;
      sy:=RXData[1].Size[ci].Y;
      MyBitMap.Width:=sx;
      MyBitMap.Height:=sy;

      for y:=0 to sy-1 do for x:=0 to sx-1 do begin
        t:=RXData[1].Data[ci,y*sx+x];
        MyBitMap.Canvas.Pixels[x,y]:=fResource.GetColor32(t,DEF_PAL) AND $FFFFFF;
      end;
      if sy>0 then MyBitMap.SaveToFile(
      //@Lewin: insert field here and press Export>TreeAnim. Rename each folder after export to 'Cuttable',
      //'Quad' and etc.. there you'll have it. Note, we use 1..254 counting, JBSnorro uses 0..253 counting
      ExeDir+'Export\TreeAnim\'+{inttostr(word(MapElem[ID].DiagonalBlocked))+'_'+}int2fix(ID,3)+'_'+int2fix(k,2)+'.bmp');
    end;
  end;

  MyBitMap.Free;
end;


{Tile textures aren't always the same, e.g. if someone makes a mod they will be different,
thus it's better to spend few ms and generate minimap colors from actual data}
procedure TResource.MakeMiniMapColors(FileName:string);
var ii,kk,h,j,pX:integer; c:array of byte; R,G,B,SizeX,SizeY:integer; f:file; {ft:textfile;}
  InputStream: TFileStream;
  OutputStream: TMemoryStream;
  {$IFDEF WDC}
  DecompressionStream: TZDecompressionStream;
  {$ENDIF}
  {$IFDEF FPC}
  DecompressionStream: TDecompressionStream;
  i: Integer;
  Buf: array[0..1023]of Byte;
  {$ENDIF}
begin
  if not FileExists(FileName) then exit;
  assignfile(f,FileName);
  FileMode:=0; Reset(f,1); FileMode:=2; //Open ReadOnly

  setlength(c,18+1);
  blockread(f,c[1],18); //SizeOf(TGAHeader)
  SizeX := c[13]+c[14]*256;
  SizeY := c[15]+c[16]*256;

  if c[1]=120 then
  begin
    closefile(f);
    InputStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    OutputStream := TMemoryStream.Create;
    {$IFDEF WDC}
     DecompressionStream := TZDecompressionStream.Create(InputStream);
     OutputStream.CopyFrom(DecompressionStream, 0);
    {$ENDIF}
    {$IFDEF FPC}
     DecompressionStream := TDecompressionStream.Create(InputStream);
     repeat
       i:=DecompressionStream.Read(Buf, SizeOf(Buf));
       if i <> 0 then OutputStream.Write(Buf, i);
     until i <= 0;
    {$ENDIF}
    OutputStream.Position := 0; 
    OutputStream.ReadBuffer(c[1], 18); //SizeOf(TGAHeader)
    SizeX := c[13]+c[14]*256;
    SizeY := c[15]+c[16]*256;
    setlength(c,SizeX*SizeY*4+1);
    OutputStream.ReadBuffer(c[1], SizeX*SizeY*4);
    InputStream.Free;
    OutputStream.Free;
    DecompressionStream.Free;
  end
  else
  begin
    setlength(c,SizeX*SizeY*4+1);
    blockread(f,c[1],SizeX*SizeY*4);
    closefile(f);
  end;

  for ii:=0 to 15 do for kk:=0 to 15 do begin

    R:=0; G:=0; B:=0;

    for j:=0 to (SizeY div 16 - 1) do
    for h:=0 to (SizeX div 16 - 1) do
    begin
      pX := (((SizeX-1)-(ii*(SizeY div 16)+j))*SizeX+kk*(SizeX div 16)+h)*4; //TGA comes flipped upside down
      inc(B, c[pX+1]);
      inc(G, c[pX+2]);
      inc(R, c[pX+3]);
    end;

    pX := ii*16+kk+1;

    TileMMColor[pX].R := round(R / (SizeX*SizeY div 256)); //each tile is 32x32 px
    TileMMColor[pX].G := round(G / (SizeX*SizeY div 256));
    TileMMColor[pX].B := round(B / (SizeX*SizeY div 256));

  end;

  {assignfile(ft,ExeDir+'mm.txt'); rewrite(ft);
  for ii:=1 to 256 do begin
    write(ft, '$'+inttohex(TileMMColor[ii].B,2)+inttohex(TileMMColor[ii].G,2)+inttohex(TileMMColor[ii].R,2)+',');
    if ii mod 8 = 0 then writeln(ft);
  end;
  closefile(ft);}
end;


procedure TResource.MakeCursors(RXid:integer);
var
  i,sx,sy,x,y:integer;
  bm,bm2:TBitMap;
  IconInfo:TIconInfo;
  {$IFDEF Unix} IconInfoPointer:PIconInfo; {$ENDIF}
begin
  bm:=TBitMap.Create;  bm.PixelFormat:=pf24bit;
  bm2:=TBitMap.Create; bm2.PixelFormat:=pf24bit;

  for i:=1 to length(Cursors) do begin

    //Special case for invisible cursor
    if Cursors[i] = 999 then
    begin
      bm.Width  := 1; bm.Height  := 1;
      bm2.Width := 1; bm2.Height := 1;
      bm2.Canvas.Pixels[0,0] := clWhite; //Invisible mask, we don't care for Image color
      IconInfo.xHotspot := 0;
      IconInfo.yHotspot := 0;
    end
    else
    begin
      sx := RXData[RXid].Size[Cursors[i]].X;
      sy := RXData[RXid].Size[Cursors[i]].Y;
      bm.Width  := sx; bm.Height  := sy;
      bm2.Width := sx; bm2.Height := sy;

      for y:=0 to sy-1 do for x:=0 to sx-1 do
      begin
        //todo: Find a PC which doesn't shows transparency and try to change 4th byte in bm.Canvas.Pixels
        if RXData[RXid].RGBA[Cursors[i],y*sx+x] and $FF000000 = 0 then begin
          bm.Canvas.Pixels[x,y] := 0; //If not reset will invert background color
          bm2.Canvas.Pixels[x,y] := clWhite;
        end else begin
          bm.Canvas.Pixels[x,y] := (RXData[RXid].RGBA[Cursors[i],y*sx+x] AND $FFFFFF);
          bm2.Canvas.Pixels[x,y] := clBlack;
        end;
        //bm2.Canvas.Pixels[x,y] := byte((RXData[RXid].RGBA[Cursors[i],y*sx+x] shr 24) and $FF)*65793;
      end;
      //Load hotspot offsets from RX file, adding the manual offsets (normally 0)
      IconInfo.xHotspot := Math.max(-RXData[RXid].Pivot[Cursors[i]].x+CursorOffsetsX[i],0);
      IconInfo.yHotspot := Math.max(-RXData[RXid].Pivot[Cursors[i]].y+CursorOffsetsY[i],0);
    end;
    IconInfo.fIcon := false; //true=Icon, false=Cursor
    IconInfo.hbmColor:=bm.Handle;
    IconInfo.hbmMask:=bm2.Handle;

    //I have a suspicion that maybe Windows could create icon delayed, at a time when bitmap data is
    //no longer valid (replaced by other bitmap or freed). Hence issues with transparency.
    {$IFDEF MSWindows}
    Screen.Cursors[Cursors[i]]:=CreateIconIndirect(IconInfo);
    {$ENDIF}
    {$IFDEF Unix}
    IconInfoPointer := @IconInfo;
    Screen.Cursors[Cursors[i]]:=CreateIconIndirect(IconInfoPointer);
    {$ENDIF}
  end;

  bm.Free;
  bm2.Free;
  Screen.Cursor := c_Default;
end;


{ TKMHouseDatCollection }
constructor TKMHouseDatCollection.Create;
var H:THouseType;
begin
  Inherited;

  for H := Low(THouseType) to High(THouseType) do
    fItems[H] := TKMHouseDatClass.Create(H);
end;


destructor TKMHouseDatCollection.Destroy;
var H:THouseType;
begin
  for H := Low(THouseType) to High(THouseType) do
    FreeAndNil(fItems[H]);

  Inherited;
end;


function TKMHouseDatCollection.GetHouseDat(aType:THouseType): TKMHouseDatClass;
begin
  Result := fItems[aType];
end;


function TKMHouseDatCollection.GetBeastAnim(aType:THouseType; aBeast, aAge: integer): TKMHouseBeastAnim;
begin
  Assert(aType in [ht_Swine, ht_Stables]);
  Assert(InRange(aBeast, 1, 5));
  Assert(InRange(aAge, 1, 3));
  case aType of
    ht_Swine:   Result := fBeastAnim[1, aBeast, aAge];
    ht_Stables: Result := fBeastAnim[2, aBeast, aAge];
  end;
end;


procedure TKMHouseDatCollection.LoadHouseDat(aPath: string);
var
  S:TKMemoryStream;
  i:integer;
begin
  Assert(FileExists(aPath));

  S := TKMemoryStream.Create;
  S.LoadFromFile(aPath);

  S.Read(fBeastAnim, SizeOf(fBeastAnim){30*70}); //Swine&Horses animations

  //Read the records one by one because we need to reorder them and skip one in the middle
  for i:=0 to HouseDatCount-1 do
  if HouseKaMType[i] <> ht_None then
    fItems[HouseKaMType[i]].LoadFromStream(S)
  else
    S.Seek(SizeOf(TKMHouseDat), soFromCurrent);

  S.Free;

  //ExportCSV(ExeDir+'Houses.csv');
end;


procedure TKMHouseDatCollection.ExportCSV(aPath: string);
var i:THouseType; Ap:string; S:TStringList; j,k:integer;
  procedure AddField(aField:string); overload; begin Ap := Ap + aField + ';'; end;
  procedure AddField(aField:integer); overload; begin Ap := Ap + inttostr(aField) + ';'; end;
begin
  S := TStringList.Create;

  Ap := 'House name;WoodCost;StoneCost';
  S.Append(Ap);

  for i:=Low(THouseType) to High(THouseType) do begin
    Ap := '';
    AddField(fItems[i].HouseName);
    S.Append(Ap);
    for j := 1 to 10 do
    begin
      Ap := '';
      for k:=1 to 10 do
        AddField(fItems[i].BuildArea[j,k]);
      S.Append(Ap);
    end;
    Ap := '';
    AddField(fItems[i].StoneCost);
    S.Append(Ap);
  end;

  S.SaveToFile(aPath);
  S.Free;
end;


{ TKMHouseDatClass }
constructor TKMHouseDatClass.Create(aHouseType: THouseType);
begin
  Inherited Create;
  fHouseType := aHouseType;
end;


function TKMHouseDatClass.GetAcceptsGoods: boolean;
begin
  Result := not (ResInput[1] in [rt_None,rt_All,rt_Warfare]); //Exclude aggregate types
end;

function TKMHouseDatClass.GetArea: THouseArea;
begin
  Result := HousePlanYX[fHouseType];
end;


function TKMHouseDatClass.GetDoesOrders: boolean;
begin
  Result := HouseDoesOrders_[fHouseType];
end;


function TKMHouseDatClass.GetGUIIcon: word;
begin
  Result := GUIBuildIcons_[HouseKaMOrder[fHouseType]];
end;


function TKMHouseDatClass.GetHouseName: string;
begin
  if IsValid then
    Result := fTextLibrary.GetTextString(siHouseNames+HouseKaMOrder[fHouseType])
  else
    Result := 'N/A';
end;


function TKMHouseDatClass.GetHouseUnlock: THouseTypeSet;
begin
  Result := HouseRelease[fHouseType];
end;


function TKMHouseDatClass.GetOwnerType: TUnitType;
begin
  Result := TUnitType(fHouseDat.OwnerType+1);
end;


function TKMHouseDatClass.GetProducesGoods: boolean;
begin
  Result := not (ResOutput[1] in [rt_None,rt_All,rt_Warfare]); //Exclude aggregate types
end;

function TKMHouseDatClass.GetResInput: THouseRes;
begin
  Result := HouseInput_[HouseKaMOrder[fHouseType]];
end;

function TKMHouseDatClass.GetResOutput: THouseRes;
begin
  Result := HouseOutput_[HouseKaMOrder[fHouseType]];
end;

function TKMHouseDatClass.IsValid: boolean;
begin
  Result := (fHouseType in [Low(THouseType)..High(THouseType)]-[ht_None, ht_Any]);
end;


procedure TKMHouseDatClass.LoadFromStream(Stream: TMemoryStream);
begin
  Stream.Read(fHouseDat, SizeOf(TKMHouseDat));
end;


end.
