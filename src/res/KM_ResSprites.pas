unit KM_ResSprites;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF Unix} LCLIntf, LCLType, {$ENDIF}
  Classes, Graphics, Math, SysUtils,
  KM_CommonTypes, KM_Defaults, KM_Pics, KM_PNG, KM_Render, KM_ResTexts, KM_ResTileset, KM_ResHouses
  {$IFDEF FPC}, zstream {$ENDIF}
  {$IFDEF WDC}, ZLib {$ENDIF};


const
  //Colors to paint beneath player color areas (flags)
  //The blacker/whighter - the more contrast player color will be
  FLAG_COLOR_DARK = $FF101010;   //Dark-grey (Black)
  FLAG_COLOR_LITE = $FFFFFFFF;   //White

type
  TRXUsage = (ruMenu, ruGame, ruCustom); //Where sprites are used

  TRXInfo = record
    FileName: string; //Used for logging and filenames
    TeamColors: Boolean; //sprites should be generated with color masks
    Usage: TRXUsage; //Menu and Game sprites are loaded separately
    LoadingTextID: Word;
  end;

  TKMGenTerrainInfo = record
    TerKind: TKMTerrainKind;
    Mask: TKMMaskFullType;
  end;


var
  RXInfo: array [TRXType] of TRXInfo = (
    (FileName: 'Trees';      TeamColors: False; Usage: ruGame;   LoadingTextID: TX_MENU_LOADING_TREES;),
    (FileName: 'Houses';     TeamColors: True;  Usage: ruGame;   LoadingTextID: TX_MENU_LOADING_HOUSES;),
    (FileName: 'Units';      TeamColors: True;  Usage: ruGame;   LoadingTextID: TX_MENU_LOADING_UNITS;),
    (FileName: 'GUI';        TeamColors: True;  Usage: ruMenu;   LoadingTextID: 0;),
    (FileName: 'GUIMain';    TeamColors: False; Usage: ruMenu;   LoadingTextID: 0;),
    (FileName: 'Campaign';   TeamColors: False; Usage: ruCustom; LoadingTextID: 0;),
    (FileName: 'Custom';     TeamColors: False; Usage: ruCustom; LoadingTextID: 0;),
    (FileName: 'Tileset';    TeamColors: False; Usage: ruMenu;   LoadingTextID: TX_MENU_LOADING_TILESET;));

type
  TRXData = record
    Count: Integer;
    Flag: array of Byte; //Sprite is valid
    Size: array of record X,Y: Word; end;
    Pivot: array of record x,y: Integer; end;
    SizeNoShadow: array of record left,top,right,bottom: Integer; end; //Image object (without shadow) rect in the image sizes
    Data: array of array of Byte;
    RGBA: array of array of Cardinal; //Expanded image
    Mask: array of array of Byte; //Mask for team colors
    HasMask: array of Boolean; //Flag if Mask for team colors is used
  end;
  PRXData = ^TRXData;

  TSoftenShadowType = (sstNone, sstOnlyShadow, sstBoth);

  TTGameResourceLoader = class;

  //Base class for Sprite loading
  TKMSpritePack = class
  private
    fPad: Byte; //Force padding between sprites to avoid neighbour edge visibility
    procedure MakeGFX_BinPacking(aTexType: TTexFormat; aStartingIndex: Word; var BaseRAM, ColorRAM, TexCount: Cardinal;
                                 aFillGFXData: Boolean = True; aOnStopExecution: TBooleanFuncSimple = nil);
    procedure SaveTextureToPNG(aWidth, aHeight: Word; const aFilename: string; const Data: TKMCardinalArray);
  protected
    fRT: TRXType;
    fRXData: TRXData;
    procedure Allocate(aCount: Integer); virtual; //Allocate space for data that is being loaded
  public
    constructor Create(aRT: TRXType);

    procedure AddImage(const aFolder, aFilename: string; aIndex: Integer);
    property RXData: TRXData read fRXData;
    property Padding: Byte read fPad write fPad;

    procedure LoadFromRXXFile(const aFileName: string; aStartingIndex: Integer = 1);
    procedure OverloadFromFolder(const aFolder: string; aSoftenShadows: Boolean = True);
    procedure MakeGFX(aAlphaShadows: Boolean; aStartingIndex: Integer = 1; aFillGFXData: Boolean = True; aOnStopExecution: TBooleanFuncSimple = nil);
    procedure DeleteSpriteTexture(aIndex: Integer);

    function GetSoftenShadowType(aID: Integer): TSoftenShadowType;
    procedure SoftenShadows(aIdList: TStringList); overload;
    procedure SoftenShadows(aStart: Integer = 1; aEnd: Integer = -1; aOnlyShadows: Boolean = True); overload;
    procedure SoftenShadows(aID: Integer; aOnlyShadows: Boolean = True); overload;
    procedure DetermineImagesObjectSize(aStart: Integer = 1; aEnd: Integer = -1);
    procedure RemoveSnowHouseShadows(aResHouses: TKMResHouses);

    function GetSpriteColors(aCount: Word): TRGBArray;

    procedure ExportAll(const aFolder: string);
    procedure ExportFullImageData(const aFolder: string; aIndex: Integer; aTempList: TStringList = nil);
    procedure ExportImage(const aFile: string; aIndex: Integer);
    procedure ExportMask(const aFile: string; aIndex: Integer);

    procedure ClearTemp; virtual;//Release non-required data
  end;


  TKMResSprites = class
  private
    fAlphaShadows: Boolean; //Remember which state we loaded
    fSprites: array[TRXType] of TKMSpritePack;
    fStepProgress: TEvent;
    fStepCaption: TUnicodeStringEvent;
    fGenTexIdStartI: Integer;
    fGenTexIdStartILegacy: Integer;
    fGenTerrainToTerKind: array of TKMGenTerrainInfo;
    fGenTerrainToTerKindLegacy: array of TKMGenTerrainInfo;

    fGameRXTypes: TStringList; //list of TRXType for game resources
    {$IFDEF LOAD_GAME_RES_ASYNC}
    fGameResLoader: TTGameResourceLoader; // thread of game resource loader
    fGameResLoadCompleted: Boolean;
    {$ENDIF}

    function GetRXFileName(aRX: TRXType): string;
    function GetSprites(aRT: TRXType): TKMSpritePack;

    {$IFDEF LOAD_GAME_RES_ASYNC}
    procedure ManageResLoader;
    procedure StopResourceLoader;
    procedure GenerateTextureAtlasForGameRes(aRT: TRXType);
    function GetNextLoadRxTypeIndex(aRT: TRXType): Integer;
    {$ENDIF}
  public
    constructor Create(aStepProgress: TEvent; aStepCaption: TUnicodeStringEvent);
    destructor Destroy; override;

    procedure LoadMenuResources;
    procedure LoadGameResources(aAlphaShadows: Boolean; aForceReload: Boolean = False);
    procedure ClearTemp;
    procedure ClearGameResGenTemp;
    class procedure SetMaxAtlasSize(aMaxSupportedTxSize: Integer);
    class function AllTilesOnOneAtlas: Boolean;

    procedure GenerateTerrainTransitions(aSprites: TKMSpritePack; aLegacyGeneration: Boolean = False);

    function GetGenTerrainInfo(aTerrain: Integer): TKMGenTerrainInfo;
    function GetGenTerrainInfoLegacy(aTerrain: Integer): TKMGenTerrainInfo;

    property Sprites[aRT: TRXType]: TKMSpritePack read GetSprites; default;

    {$IFDEF LOAD_GAME_RES_ASYNC}
    property GameResLoadCompleted: Boolean read fGameResLoadCompleted;
    {$ENDIF}

    //Used externally to access raw RGBA data (e.g. by ExportAnim)
    function LoadSprites(aRT: TRXType; aAlphaShadows: Boolean): Boolean;
    procedure ExportToPNG(aRT: TRXType);

    property AlphaShadows: Boolean read fAlphaShadows;
    property FileName[aRX: TRXType]: string read GetRXFileName;

    procedure UpdateStateIdle;
  end;


  //Game resource loader thread
  TTGameResourceLoader = class(TThread)
  private
    fResSprites: TKMResSprites;
    fAlphaShadows: Boolean;
    function IsTerminated: Boolean;
  public
    RXType: TRXType;
    LoadStepDone: Boolean;    // flag to show, when another rxx load is completed
    constructor Create(aResSprites: TKMResSprites; aAlphaShadows: Boolean; aRxType: TRXType);
    procedure Execute; override;
  end;


  TKMTexCoords = record
                  ID: Cardinal;
                  u1,v1,u2,v2: Single; //Top-Left, Bottom-Right uv coords
                end;

var
  gGFXData: array [TRXType] of array of record
    Tex, Alt: TKMTexCoords; //AltID used for team colors and house building steps
    PxWidth, PxHeight: Word;
  end;

  gGenTerrainTransitions: array[TKMTerrainKind]
                            of array[TKMTileMaskKind]
                              of array[TKMTileMaskType]
                                of array[TKMTileMaskSubType] //mask components (subtypes)
//                                  of array[0..3] //Terrain Rotation
                                    of Word;

  gGenTerrainTransitionsLegacy: array[TKMTerrainKind]
                            of array[TKMTileMaskKind]
                              of array[TKMTileMaskType]
                                of array[TKMTileMaskSubType] //mask components (subtypes)
//                                  of array[0..3] //Terrain Rotation
                                    of Word;
implementation
uses
  KromUtils,
  {$IFDEF LOAD_GAME_RES_ASYNC}
  KM_GameApp,
  {$ENDIF}
  KM_SoftShadows, KM_Resource, KM_ResUnits,
  KM_Log, KM_BinPacking, KM_CommonUtils, KM_Points;

type
  TSpriteAtlasType = (saBase, saMask);

const
  MAX_GAME_ATLAS_SIZE = 2048; //Max atlas size for KaM. No need for bigger atlases
  SPRITE_TYPE_EXPORT_NAME: array [TSpriteAtlasType] of string = ('Base', 'Mask');
  LOG_EXTRA_GFX: Boolean = False;

var
  AllTilesInOneTexture: Boolean = False;
  MaxAtlasSize: Integer;

  gGFXPrepData: array[TSpriteAtlasType] of  // for each atlas type
                  array of                  // Atlases for each rxx
                    record                  // Atlas data, needed for Texture Atlas Generation
                      SpriteInfo: TBinItem;
                      TexType: TTexFormat;
                      Data: TKMCardinalArray;
                    end;


{ TKMSpritePack }
constructor TKMSpritePack.Create(aRT: TRXType);
begin
  inherited Create;

  fRT := aRT;

  //Terrain tiles need padding to avoid edge bleeding
  if fRT = rxTiles then
    fPad := 1;
end;


//This is a crude solution to allow Campaigns to delete sprites they add
procedure TKMSpritePack.DeleteSpriteTexture(aIndex: Integer);
begin
  if gGFXData[fRT, aIndex].Tex.ID <> 0 then
    TRender.DeleteTexture(gGFXData[fRT, aIndex].Tex.ID);
  if gGFXData[fRT, aIndex].Alt.ID <> 0 then
    TRender.DeleteTexture(gGFXData[fRT, aIndex].Alt.ID);

  gGFXData[fRT, aIndex].Tex.ID := 0;
  gGFXData[fRT, aIndex].Alt.ID := 0;
end;


function TKMSpritePack.GetSoftenShadowType(aID: Integer): TSoftenShadowType;
var
  Step, SpriteID: Integer;
  UT: TKMUnitType;
  Dir: TKMDirection;
begin
  Result := sstNone;

  case fRT of
    rxHouses: if InRange(aID, 889, 892)            //Smooth smoke
                or InRange(aID, 1615, 1638) then   //Smooth flame
                Result := sstBoth
              else
                Result := sstOnlyShadow;
    rxUnits:  begin
                if InRange(aID, 6251, 6322) then     //Smooth thought bubbles
                begin
                  Result := sstBoth;
                  Exit;
                end;
                //Smooth all death animations for all units
                for UT := HUMANS_MIN to HUMANS_MAX do
                  for Dir := dirN to dirNW do
                    for Step := 1 to 30 do
                    begin
                      SpriteID := gRes.Units[UT].UnitAnim[uaDie,Dir].Step[Step]+1; //Sprites in units.dat are 0 indexed
                      if (aID = SpriteID) and (SpriteID > 0) then
                      begin
                        Result := sstBoth;
                        Exit;
                      end;
                    end;
                if Result = sstNone then
                  Result := sstOnlyShadow;
              end;
    rxTrees:  Result := sstOnlyShadow;
    rxGui:    if InRange(aID, 105, 128)         //Field plans
                or InRange(aID, 249, 281)       //House tablets only (shadow softening messes up other rxGui sprites)
                or InRange(aID, 461, 468)       //Field fences
                or InRange(aID, 660, 660) then  //Woodcutter cutting point sign
                Result := sstOnlyShadow;
  end;
end;


procedure TKMSpritePack.SoftenShadows(aIdList: TStringList);
var
  I, ID: Integer;
  ShadowConverter: TKMSoftShadowConverter;
  SoftenShadowType: TSoftenShadowType;
begin
  if aIdList.Count = 0 then Exit;

  ShadowConverter := TKMSoftShadowConverter.Create(Self);
  try
    for I := 0 to aIdList.Count - 1 do
    begin
      ID := StrToInt(aIdList[I]);
      if (fRXData.Flag[ID] <> 0) then
      begin
        SoftenShadowType := GetSoftenShadowType(ID);
        case SoftenShadowType of
          sstNone: ;
          sstOnlyShadow:  ShadowConverter.ConvertShadows(ID, True);
          sstBoth:        begin
                            ShadowConverter.ConvertShadows(ID, False);
                            ShadowConverter.ConvertShadows(ID, True);
                          end;
        end;
      end;
    end;
  finally
    ShadowConverter.Free;
  end;
end;


//Make old style KaM checkerboard shadows smooth and transparent
procedure TKMSpritePack.SoftenShadows(aStart: Integer = 1; aEnd: Integer = -1; aOnlyShadows: Boolean = True);
var
  I: Integer;
  ShadowConverter: TKMSoftShadowConverter;
begin
  ShadowConverter := TKMSoftShadowConverter.Create(Self);
  try
    if aEnd = -1 then aEnd := fRXData.Count;
    for I := aStart to aEnd do
      if (fRXData.Flag[I] <> 0) then
        ShadowConverter.ConvertShadows(I, aOnlyShadows);
  finally
    ShadowConverter.Free;
  end;
end;


procedure TKMSpritePack.DetermineImagesObjectSize(aStart: Integer = 1; aEnd: Integer = -1);
var
  I: Integer;
  ShadowConverter: TKMSoftShadowConverter;
begin
  ShadowConverter := TKMSoftShadowConverter.Create(Self);
  try
    if aEnd = -1 then aEnd := fRXData.Count;
    for I := aStart to aEnd do
      if (fRXData.Flag[I] <> 0) then
        ShadowConverter.DetermineImageObjectSize(I);
  finally
    ShadowConverter.Free;
  end;
end;


procedure TKMSpritePack.RemoveSnowHouseShadows(aResHouses: TKMResHouses);
var
  SnowID: Integer;
  ShadowConverter: TKMSoftShadowConverter;
  HT: TKMHouseType;
begin
  Assert(fRT = rxHouses);

  ShadowConverter := TKMSoftShadowConverter.Create(Self);
  try
    for HT := HOUSE_MIN to HOUSE_MAX do
    begin
      SnowID := aResHouses[HT].SnowPic + 1;
      if (fRXData.Flag[SnowID] <> 0) then
        ShadowConverter.RemoveShadow(SnowID);
    end;
  finally
    ShadowConverter.Free;
  end;
end;


procedure TKMSpritePack.SoftenShadows(aID: Integer; aOnlyShadows: Boolean = True);
var
  ShadowConverter: TKMSoftShadowConverter;
begin
  ShadowConverter := TKMSoftShadowConverter.Create(Self);
  try
    if (fRXData.Flag[aID] <> 0) then
      ShadowConverter.ConvertShadows(aID, aOnlyShadows);
  finally
    ShadowConverter.Free;
  end;
end;


procedure TKMSpritePack.Allocate(aCount: Integer);
begin
  fRXData.Count := aCount;

  aCount := fRXData.Count + 1;
  SetLength(gGFXData[fRT],        aCount);
  SetLength(fRXData.Flag,         aCount);
  SetLength(fRXData.Size,         aCount);
  SetLength(fRXData.Pivot,        aCount);
  //SizeNoShadow is used only for Units
  if fRT = rxUnits then
    SetLength(fRXData.SizeNoShadow, aCount);
  SetLength(fRXData.RGBA,         aCount);
  SetLength(fRXData.Mask,         aCount);
  SetLength(fRXData.HasMask,      aCount);
end;


//Release RAM that is no longer needed
procedure TKMSpritePack.ClearTemp;
var I: Integer;
begin
  for I := 1 to fRXData.Count do
  begin
    SetLength(fRXData.RGBA[I], 0);
    SetLength(fRXData.Mask[I], 0);
  end;
end;


//Add PNG images to spritepack if user has any addons in Sprites folder
procedure TKMSpritePack.AddImage(const aFolder, aFilename: string; aIndex: Integer);
type TMaskType = (mtNone, mtPlain, mtSmart);
var
  I,K:integer;
  Tr, Tg, Tb, T: Byte;
  Thue, Tsat, Tbri: Single;
  ft: TextFile;
  MaskFile: array [TMaskType] of string;
  MaskTyp: TMaskType;
  pngWidth, pngHeight: Word;
  pngData: TKMCardinalArray;
begin
  Assert(SameText(ExtractFileExt(aFilename), '.png'));

  if aIndex > fRXData.Count then
    Allocate(aIndex);

  LoadFromPng(aFolder + aFilename, pngWidth, pngHeight, pngData);
  Assert((pngWidth <= 2048) and (pngHeight <= 2048), 'Image size should be less than 2048x2048 pixels');

  fRXData.Flag[aIndex] := Byte(pngWidth * pngHeight <> 0); //Mark as used (required for saving RXX)
  fRXData.Size[aIndex].X := pngWidth;
  fRXData.Size[aIndex].Y := pngHeight;

  SetLength(fRXData.RGBA[aIndex], pngWidth * pngHeight);
  SetLength(fRXData.Mask[aIndex], pngWidth * pngHeight); //Should allocate space for it's always comes along

  for K := 0 to pngHeight - 1 do
  for I := 0 to pngWidth - 1 do
    fRXData.RGBA[aIndex, K * pngWidth + I] := pngData[K * pngWidth + I];

  MaskFile[mtPlain] := aFolder + StringReplace(aFilename, '.png', 'm.png', [rfReplaceAll, rfIgnoreCase]);
  MaskFile[mtSmart] := aFolder + StringReplace(aFilename, '.png', 'a.png', [rfReplaceAll, rfIgnoreCase]);

  //Determine mask processing mode
  if FileExists(MaskFile[mtPlain]) then
    MaskTyp := mtPlain
  else
  if FileExists(MaskFile[mtSmart]) then
    MaskTyp := mtSmart
  else
    MaskTyp := mtNone;

  fRXData.HasMask[aIndex] := MaskTyp in [mtPlain, mtSmart];

  //Load and process the mask if it exists
  if fRXData.HasMask[aIndex] then
  begin
    //Plain masks are used 'as is'
    //Smart masks are designed for the artist, they convert color brightness into a mask

    LoadFromPng(MaskFile[MaskTyp], pngWidth, pngHeight, pngData);

    if (fRXData.Size[aIndex].X = pngWidth)
    and (fRXData.Size[aIndex].Y = pngHeight) then
    begin
      //We don't handle transparency in Masks
      for K := 0 to pngHeight - 1 do
      for I := 0 to pngWidth - 1 do
      case MaskTyp of
        mtPlain:  begin
                    //For now process just red (assume pic is greyscale)
                    fRXData.Mask[aIndex, K*pngWidth+I] := pngData[K*pngWidth+I] and $FF;
                  end;
        mtSmart:  begin
                    if Cardinal(pngData[K*pngWidth+I] and $FFFFFF) <> 0 then
                    begin
                      Tr := fRXData.RGBA[aIndex, K*pngWidth+I] and $FF;
                      Tg := fRXData.RGBA[aIndex, K*pngWidth+I] shr 8 and $FF;
                      Tb := fRXData.RGBA[aIndex, K*pngWidth+I] shr 16 and $FF;

                      //Determine color brightness
                      ConvertRGB2HSB(Tr, Tg, Tb, Thue, Tsat, Tbri);

                      //Make background RGBA black or white for more saturated colors
                      if Tbri < 0.5 then
                        fRXData.RGBA[aIndex, K*pngWidth+I] := FLAG_COLOR_DARK
                      else
                        fRXData.RGBA[aIndex, K*pngWidth+I] := FLAG_COLOR_LITE;

                      //Map brightness from 0..1 to 0..255..0
                      T := Trunc((0.5 - Abs(Tbri - 0.5)) * 510);
                      fRXData.Mask[aIndex, K*pngWidth+I] := T;
                    end
                    else
                      fRXData.Mask[aIndex, K*pngWidth+I] := 0;
                 end;
        end;
    end;
  end;

  //Read pivot info
  if FileExists(aFolder + Copy(aFilename, 1, 6) + '.txt') then
  begin
    AssignFile(ft, aFolder + Copy(aFilename, 1, 6) + '.txt');
    Reset(ft);
    ReadLn(ft, fRXData.Pivot[aIndex].X);
    ReadLn(ft, fRXData.Pivot[aIndex].Y);

    //SizeNoShadow is used only for Units
    if fRT = rxUnits then
    begin
      ReadLn(ft, fRXData.SizeNoShadow[aIndex].left);
      ReadLn(ft, fRXData.SizeNoShadow[aIndex].top);
      ReadLn(ft, fRXData.SizeNoShadow[aIndex].right);
      ReadLn(ft, fRXData.SizeNoShadow[aIndex].bottom);
    end;
    CloseFile(ft);
  end;
end;


procedure TKMSpritePack.LoadFromRXXFile(const aFileName: string; aStartingIndex: Integer = 1);
var
  I: Integer;
  RXXCount: Integer;
  InputStream: TFileStream;
  DecompressionStream: TDecompressionStream;
begin
  if SKIP_RENDER then Exit;
  if not FileExists(aFileName) then Exit;

  InputStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyNone);
  DecompressionStream := TDecompressionStream.Create(InputStream);

  try
    DecompressionStream.Read(RXXCount, 4);
    if gLog <> nil then
      gLog.AddTime(RXInfo[fRT].FileName + ' -', RXXCount);

    if RXXCount = 0 then
      Exit;

    Allocate(aStartingIndex + RXXCount - 1);

    DecompressionStream.Read(fRXData.Flag[aStartingIndex], RXXCount);

    for I := aStartingIndex to aStartingIndex + RXXCount - 1 do
      if fRXData.Flag[I] = 1 then
      begin
        DecompressionStream.Read(fRXData.Size[I].X, 4);
        DecompressionStream.Read(fRXData.Pivot[I].X, 8);
        //SizeNoShadow is used only for Units
        if fRT = rxUnits then
          DecompressionStream.Read(fRXData.SizeNoShadow[I].left, 16);
        //Data part of each sprite is 32BPP RGBA in Remake RXX files
        SetLength(fRXData.RGBA[I], fRXData.Size[I].X * fRXData.Size[I].Y);
        SetLength(fRXData.Mask[I], fRXData.Size[I].X * fRXData.Size[I].Y);
        DecompressionStream.Read(fRXData.RGBA[I, 0], 4 * fRXData.Size[I].X * fRXData.Size[I].Y);
        DecompressionStream.Read(fRXData.HasMask[I], 1);
        if fRXData.HasMask[I] then
          DecompressionStream.Read(fRXData.Mask[I, 0], fRXData.Size[I].X * fRXData.Size[I].Y);
      end;
  finally
    DecompressionStream.Free;
    InputStream.Free;
  end;
end;


//Parse all valid files in Sprites folder and load them additionaly to or replacing original sprites
procedure TKMSpritePack.OverloadFromFolder(const aFolder: string; aSoftenShadows: Boolean = True);
  procedure ProcessFolder(const aProcFolder: string);
  var
    FileList, IDList: TStringList;
    SearchRec: TSearchRec;
    I, ID: Integer;
  begin
    if not DirectoryExists(aFolder) then Exit;
    FileList := TStringList.Create;
    try
      IDList := TStringList.Create;
      try
        try
          //PNGs
          if FindFirst(aProcFolder + IntToStr(Byte(fRT) + 1) + '_????.png', faAnyFile - faDirectory, SearchRec) = 0 then
          repeat
            FileList.Add(SearchRec.Name);
          until (FindNext(SearchRec) <> 0);
        finally
          FindClose(SearchRec);
        end;

        //PNG may be accompanied by some more files
        //#_####.png - Base texture
        //#_####a.png - Flag color mask
        //#_####.txt - Pivot info (optional)
        for I := 0 to FileList.Count - 1 do
          if TryStrToInt(Copy(FileList.Strings[I], 3, 4), ID) then
          begin
            AddImage(aProcFolder, FileList.Strings[I], ID);
            IDList.Add(IntToStr(ID));
          end;

        if aSoftenShadows then
          SoftenShadows(IDList); // Soften shadows for overloaded sprites

        try
          //Delete following sprites
          if FindFirst(aProcFolder + IntToStr(Byte(fRT) + 1) + '_????', faAnyFile - faDirectory, SearchRec) = 0 then
          repeat
            if TryStrToInt(Copy(SearchRec.Name, 3, 4), ID) then
              fRXData.Flag[ID] := 0;
          until (FindNext(SearchRec) <> 0);
        finally
          FindClose(SearchRec);
        end;
      finally
        IDList.Free;
      end;
    finally
      FileList.Free;
    end;
  end;
begin
  if SKIP_RENDER then Exit;

  ProcessFolder(aFolder);
  ProcessFolder(aFolder + IntToStr(Byte(fRT) + 1) + PathDelim);
end;


//Export RX to Bitmaps without need to have GraphicsEditor, also this way we preserve image indexes
procedure TKMSpritePack.ExportAll(const aFolder: string);
var
  I: Integer;
  SL: TStringList;
begin
  ForceDirectories(aFolder);

  SL := TStringList.Create;

  for I := 1 to fRXData.Count do
    ExportFullImageData(aFolder, I, SL);

  SL.Free;
end;


procedure TKMSpritePack.ExportFullImageData(const aFolder: string; aIndex: Integer; aTempList: TStringList = nil);
var
  ListCreated: Boolean;
begin
  ListCreated := False;
  if aTempList = nil then
  begin
    aTempList := TStringList.Create;
    ListCreated := True;
  end;

  if fRXData.Flag[aIndex] = 1 then
  begin
    ExportImage(aFolder + Format('%d_%.4d.png', [Byte(fRT)+1, aIndex]), aIndex);

    if fRXData.HasMask[aIndex] then
      ExportMask(aFolder + Format('%d_%.4da.png', [Byte(fRT)+1, aIndex]), aIndex);

    //Export pivot
    aTempList.Clear;
    aTempList.Append(IntToStr(fRXData.Pivot[aIndex].x));
    aTempList.Append(IntToStr(fRXData.Pivot[aIndex].y));
    //SizeNoShadow is used only for Units
    if fRT = rxUnits then
    begin
      aTempList.Append(IntToStr(fRXData.SizeNoShadow[aIndex].left));
      aTempList.Append(IntToStr(fRXData.SizeNoShadow[aIndex].top));
      aTempList.Append(IntToStr(fRXData.SizeNoShadow[aIndex].right));
      aTempList.Append(IntToStr(fRXData.SizeNoShadow[aIndex].bottom));
    end;
    aTempList.SaveToFile(aFolder + Format('%d_%.4d.txt', [Byte(fRT)+1, aIndex]));
  end;

  if ListCreated then
    aTempList.Free;
end;


procedure TKMSpritePack.ExportImage(const aFile: string; aIndex: Integer);
var
  I, K: Integer;
  M: Byte;
  TreatMask: Boolean;
  pngWidth, pngHeight: Word;
  pngData: TKMCardinalArray;
begin
  pngWidth := fRXData.Size[aIndex].X;
  pngHeight := fRXData.Size[aIndex].Y;

  SetLength(pngData, pngWidth * pngHeight);

  //Export RGB values
  for I := 0 to pngHeight - 1 do
  for K := 0 to pngWidth - 1 do
  begin
    TreatMask := fRXData.HasMask[aIndex] and (fRXData.Mask[aIndex, I*pngWidth + K] > 0);
    if (fRT = rxHouses)
      and ((aIndex < 680)
        or (aIndex = 1657)
        or (aIndex = 1659)
        or (aIndex = 1681)
        or (aIndex = 1683)
        or (aIndex > 2050)) then
      TreatMask := False;

    if TreatMask then
    begin
      M := fRXData.Mask[aIndex, I*pngWidth + K];

      //Replace background with corresponding brightness of Red
      if fRXData.RGBA[aIndex, I*pngWidth + K] = FLAG_COLOR_DARK then
        //Brightness < 0.5, mix with black
        pngData[I*pngWidth + K] := M
      else
        //Brightness > 0.5, mix with white
        pngData[I*pngWidth + K] := $FF + (255 - M) * $010100;
    end
    else
      pngData[I*pngWidth + K] := fRXData.RGBA[aIndex, I*pngWidth + K] and $FFFFFF;

    pngData[I*pngWidth + K] := pngData[I*pngWidth + K] or (fRXData.RGBA[aIndex, I*pngWidth + K] and $FF000000);
  end;

  //Mark pivot location with a dot
//  K := pngWidth + fRXData.Pivot[aIndex].x;
//  I := pngHeight + fRXData.Pivot[aIndex].y;
//  if InRange(I, 0, pngHeight-1) and InRange(K, 0, pngWidth-1) then
//    pngData[I*pngWidth + K] := $FFFF00FF;

  SaveToPng(pngWidth, pngHeight, pngData, aFile);
end;


procedure TKMSpritePack.ExportMask(const aFile: string; aIndex: Integer);
var
  I, K: Integer;
  pngWidth, pngHeight: Word;
  pngData: TKMCardinalArray;
  MaskColor: Cardinal;
begin
  pngWidth := fRXData.Size[aIndex].X;
  pngHeight := fRXData.Size[aIndex].Y;

  SetLength(pngData, pngWidth * pngHeight);

  //Export Mask
  if fRXData.HasMask[aIndex] then
  begin
    for I := 0 to pngHeight - 1 do
      for K := 0 to pngWidth - 1 do
      begin
        if fRT = rxHouses then
        begin
          MaskColor := fRXData.Mask[aIndex, I * pngWidth + K];
          if MaskColor <> 0 then
            MaskColor := GetGreyColor(MaskColor) or $FF000000;
        end else
          MaskColor := (Byte(fRXData.Mask[aIndex, I * pngWidth + K] > 0) * $FFFFFF) or $FF000000;
        pngData[I * pngWidth + K] := MaskColor;
      end;

    SaveToPng(pngWidth, pngHeight, pngData, aFile);
  end;
end;


function TKMSpritePack.GetSpriteColors(aCount: Word): TRGBArray;
var
  I, L, M: Integer;
  PixelCount: Word;
  R,G,B: Integer;
begin
  SetLength(Result, Min(fRXData.Count, aCount));

  for I := 1 to Min(fRXData.Count, aCount) do
  begin
    R := 0;
    G := 0;
    B := 0;
    for L := 0 to fRXData.Size[I].Y - 1 do
    for M := 0 to fRXData.Size[I].X - 1 do
    begin
      Inc(R, fRXData.RGBA[I, L * fRXData.Size[I].X + M] and $FF);
      Inc(G, fRXData.RGBA[I, L * fRXData.Size[I].X + M] shr 8 and $FF);
      Inc(B, fRXData.RGBA[I, L * fRXData.Size[I].X + M] shr 16 and $FF);
    end;
    PixelCount := Max(1, fRXData.Size[I].X * fRXData.Size[I].Y);
    Result[I-1].R := Round(R / PixelCount);
    Result[I-1].G := Round(G / PixelCount);
    Result[I-1].B := Round(B / PixelCount);
  end;
end;


//Take RX data and make nice atlas texture out of it
//Atlases should be POT to improve performance and avoid driver bugs
//In result we have GFXData structure filled
procedure TKMSpritePack.MakeGFX(aAlphaShadows: Boolean; aStartingIndex: Integer = 1; aFillGFXData: Boolean = True; aOnStopExecution: TBooleanFuncSimple = nil);
var
  TexType: TTexFormat;
  BaseRAM, IdealRAM, ColorRAM, TexCount: Cardinal;
  I: Integer;
begin
  if SKIP_RENDER then Exit;
  if fRXData.Count = 0 then Exit;

  if aAlphaShadows and (fRT in [rxTrees,rxHouses,rxUnits,rxGui]) or not aAlphaShadows and (fRT = rxGuiMain) then
    TexType := tfRGBA8
  else
    TexType := tfRGB5A1;

  MakeGFX_BinPacking(TexType, aStartingIndex, BaseRAM, ColorRAM, TexCount, aFillGFXData, aOnStopExecution);

  if LOG_EXTRA_GFX then
  begin
    IdealRAM := 0;
    for I := aStartingIndex to fRXData.Count do
    if fRXData.Flag[I] <> 0 then
      Inc(IdealRAM, fRXData.Size[I].X * fRXData.Size[I].Y * TexFormatSize[TexType]);

    gLog.AddTime(IntToStr(TexCount) + ' Textures created');
    gLog.AddNoTime(Format('%d/%d', [BaseRAM div 1024, IdealRAM div 1024]) +
                  ' Kbytes allocated/ideal for ' + RXInfo[fRT].FileName + ' GFX when using Packing');
    gLog.AddNoTime(IntToStr(ColorRAM div 1024) + ' KBytes for team colors');
  end;
end;


//Set GFXData from SpriteInfo
procedure SetGFXData(aTx: Cardinal; aSpriteInfo: TBinItem; aAtlasType: TSpriteAtlasType; aSpritesPack: TKMSpritePack; aRT: TRXType);
var
  K: Integer;
  ID: Word;
  TxCoords: TKMTexCoords;
begin
  for K := 0 to High(aSpriteInfo.Sprites) do
  begin
    ID := aSpriteInfo.Sprites[K].SpriteID;

    TxCoords.ID := aTx;
    TxCoords.u1 := aSpriteInfo.Sprites[K].PosX / aSpriteInfo.Width;
    TxCoords.v1 := aSpriteInfo.Sprites[K].PosY / aSpriteInfo.Height;
    TxCoords.u2 := (aSpriteInfo.Sprites[K].PosX + aSpritesPack.RXData.Size[ID].X) / aSpriteInfo.Width;
    TxCoords.v2 := (aSpriteInfo.Sprites[K].PosY + aSpritesPack.RXData.Size[ID].Y) / aSpriteInfo.Height;

    if aAtlasType = saBase then
    begin
      gGFXData[aRT, ID].Tex := TxCoords;
      gGFXData[aRT, ID].PxWidth := aSpritesPack.RXData.Size[ID].X;
      gGFXData[aRT, ID].PxHeight := aSpritesPack.RXData.Size[ID].Y;
    end
    else
      gGFXData[aRT, ID].Alt := TxCoords;
  end;

  //Fake Render from Atlas, to force copy of it into video RAM, where it is supposed to be
  with gGFXData[aRT, aSpriteInfo.Sprites[0].SpriteID] do
    if aAtlasType = saBase then
      TRender.FakeRender(Tex.ID)
    else
      TRender.FakeRender(Alt.ID);
end;


//This algorithm is planned to take advantage of more efficient 2D bin packing
procedure TKMSpritePack.MakeGFX_BinPacking(aTexType: TTexFormat; aStartingIndex: Word; var BaseRAM, ColorRAM, TexCount: Cardinal;
                                           aFillGFXData: Boolean = True; aOnStopExecution: TBooleanFuncSimple = nil);

  procedure PrepareAtlases(SpriteInfo: TBinArray; aMode: TSpriteAtlasType; aTexType: TTexFormat);
  var
    I, K, L, M: Integer;
    CT, CL, Pixel: Cardinal;
    Tx: Cardinal;
    ID: Word;
    TD: TKMCardinalArray;
    TexFilter: TFilterType;
  begin
    //Prepare atlases
    for I := 0 to High(SpriteInfo) do
    begin
      Assert(MakePOT(SpriteInfo[I].Width) = SpriteInfo[I].Width);
      Assert(MakePOT(SpriteInfo[I].Height) = SpriteInfo[I].Height);
      SetLength(TD, 0);
      SetLength(TD, SpriteInfo[I].Width * SpriteInfo[I].Height);

      //Copy sprite to Atlas
      for K := 0 to High(SpriteInfo[I].Sprites) do
      begin
        ID := SpriteInfo[I].Sprites[K].SpriteID;
        for L := 0 to fRXData.Size[ID].Y - 1 do
        for M := 0 to fRXData.Size[ID].X - 1 do
        begin
          CT := SpriteInfo[I].Sprites[K].PosY;
          CL := SpriteInfo[I].Sprites[K].PosX;
          Pixel := (CT + L) * SpriteInfo[I].Width + CL + M;
          if aMode = saBase then
            TD[Pixel] := fRXData.RGBA[ID, L * fRXData.Size[ID].X + M]
          else
            TD[Pixel] := $FFFFFF or (fRXData.Mask[ID, L * fRXData.Size[ID].X + M] shl 24);

          //Fill padding with edge pixels
          if fPad > 0 then
          begin
            if (M = 0) then
            begin
              TD[Pixel - 1] := TD[Pixel];
              if (L = 0) then
                TD[Pixel - SpriteInfo[I].Width - 1] := TD[Pixel]
              else
              if (L = fRXData.Size[ID].Y - 1) then
                TD[Pixel + SpriteInfo[I].Width - 1] := TD[Pixel];
            end;

            if (M = fRXData.Size[ID].X - 1) then
            begin
              TD[Pixel + 1] := TD[Pixel];
              if (L = 0) then
                TD[Pixel - SpriteInfo[I].Width + 1] := TD[Pixel]
              else
              if (L = fRXData.Size[ID].Y - 1) then
                TD[Pixel + SpriteInfo[I].Width + 1] := TD[Pixel];
            end;

            if (L = 0) then                       TD[Pixel - SpriteInfo[I].Width] := TD[Pixel];
            if (L = fRXData.Size[ID].Y - 1) then  TD[Pixel + SpriteInfo[I].Width] := TD[Pixel];
          end;

          //Sprite outline
          if OUTLINE_ALL_SPRITES and (
            (L = 0) or (M = 0)
            or (L = fRXData.Size[ID].Y - 1)
            or (M = fRXData.Size[ID].X - 1)) then
            TD[Pixel] := $FF0000FF;
        end;
      end;

      if aFillGFXData then
      begin
        //Generate texture once
        TexFilter := ftNearest;
        if LINEAR_FILTER_SPRITES and (fRT in [rxTrees, rxHouses, rxUnits]) then
          TexFilter := ftLinear;

        Tx := TRender.GenTexture(SpriteInfo[I].Width, SpriteInfo[I].Height, @TD[0], aTexType, TexFilter, TexFilter);
        //Now that we know texture IDs we can fill GFXData structure
        SetGFXData(Tx, SpriteInfo[I], aMode, Self, fRT);
      end else begin
        Assert(InRange(I, Low(gGFXPrepData[aMode]), High(gGFXPrepData[aMode])),
               Format('Preloading sprite index out of range: %d, range [%d;%d]', [I, Low(gGFXPrepData[aMode]), High(gGFXPrepData[aMode])]));
        // Save prepared data for generating later (in main thread)
        gGFXPrepData[aMode, I].SpriteInfo := SpriteInfo[I];
        gGFXPrepData[aMode, I].TexType := aTexType;
        gGFXPrepData[aMode, I].Data := TD;
      end;

      if aMode = saBase then
        Inc(BaseRAM, SpriteInfo[I].Width * SpriteInfo[I].Height * TexFormatSize[aTexType])
      else
        Inc(ColorRAM, SpriteInfo[I].Width * SpriteInfo[I].Height * TexFormatSize[aTexType]);

      Inc(TexCount);

      if aFillGFXData then
        SaveTextureToPNG(SpriteInfo[I].Width, SpriteInfo[I].Height, RXInfo[fRT].FileName + '_' +
                         SPRITE_TYPE_EXPORT_NAME[aMode] + IntToStr(aStartingIndex+I), TD);
    end;
  end;

  function StopExec: Boolean;
  begin
    Result := Assigned(aOnStopExecution) and aOnStopExecution;
  end;

var
  I, K: Integer;
  SpriteSizes: TIndexSizeArray;
  SpriteInfo: TBinArray;
  AtlasSize, AllTilesAtlasSize: Integer;
begin
  BaseRAM := 0;
  ColorRAM := 0;
  //Prepare base atlases
  SetLength(SpriteSizes, fRXData.Count - aStartingIndex + 1);
  K := 0;
  for I := aStartingIndex to fRXData.Count do
  if (fRXData.Size[I].X * fRXData.Size[I].Y <> 0) then
  begin
    SpriteSizes[K].ID := I;
    SpriteSizes[K].X := fRXData.Size[I].X;
    SpriteSizes[K].Y := fRXData.Size[I].Y;
    Inc(K);
  end;
  SetLength(SpriteSizes, K);

  //For RX with only 1 texture we can set small size, as 512, it will be auto enlarged to POT(image size)
  if K = 1 then
    AtlasSize := 512
  else if fRT = rxTiles then
  begin
    AllTilesAtlasSize := MakePOT(Ceil(sqrt(K))*(32+2*fPad)); //Tiles are 32x32
    AtlasSize := Min(MaxAtlasSize, AllTilesAtlasSize);       //Use smallest possible atlas size for tiles (should be 1024, until many new tiles were added)
    if AtlasSize = AllTilesAtlasSize then
      AllTilesInOneTexture := True;
  end else
    AtlasSize := MaxAtlasSize;

  SetLength(SpriteInfo, 0);
  BinPack(SpriteSizes, AtlasSize, fPad, SpriteInfo);

  if StopExec then Exit; //Our thread could be terminated and asked to stop. Exit immidiately then

  SetLength(gGFXPrepData[saBase], Length(SpriteInfo));
  PrepareAtlases(SpriteInfo, saBase, aTexType);

  if StopExec then Exit;

  //Prepare masking atlases
  SetLength(SpriteSizes, fRXData.Count - aStartingIndex + 1);
  K := 0;
  for I := aStartingIndex to fRXData.Count do
  if (fRXData.Size[I].X * fRXData.Size[I].Y <> 0) and fRXData.HasMask[I] then
  begin
    SpriteSizes[K].ID := I;
    SpriteSizes[K].X := fRXData.Size[I].X;
    SpriteSizes[K].Y := fRXData.Size[I].Y;
    Inc(K);
  end;
  SetLength(SpriteSizes, K);

  SetLength(SpriteInfo, 0);
  BinPack(SpriteSizes, AtlasSize, fPad, SpriteInfo);
  if StopExec then Exit;
  SetLength(gGFXPrepData[saMask], Length(SpriteInfo));
  PrepareAtlases(SpriteInfo, saMask, tfAlpha8);
end;


procedure TKMSpritePack.SaveTextureToPNG(aWidth, aHeight: Word; const aFilename: string; const Data: TKMCardinalArray);
var
  I, K: Word;
  Folder: string;
  pngWidth, pngHeight: Word;
  pngData: TKMCardinalArray;
begin
  if not EXPORT_SPRITE_ATLASES then Exit;

  Folder := ExeDir + 'Export'+PathDelim+'GenTextures'+PathDelim;
  ForceDirectories(Folder);

  pngWidth := aWidth;
  pngHeight := aHeight;
  SetLength(pngData, pngWidth * pngHeight);

  for I := 0 to aHeight - 1 do
  for K := 0 to aWidth - 1 do
    pngData[I * aWidth + K] := (PCardinal(Cardinal(@Data[0]) + (I * aWidth + K) * 4))^;

  SaveToPng(pngWidth, pngHeight, pngData, Folder + aFilename + '.png');
end;


{ TKMResSprites }
constructor TKMResSprites.Create(aStepProgress: TEvent; aStepCaption: TUnicodeStringEvent);
var
  RT: TRXType;
begin
  inherited Create;
  fGameRXTypes := TStringList.Create;

  for RT := Low(TRXType) to High(TRXType) do
  begin
    fSprites[RT] := TKMSpritePack.Create(RT);
    if RXInfo[RT].Usage = ruGame then
      fGameRXTypes.Add(IntToStr(Integer(RT)));
  end;

  fStepProgress := aStepProgress;
  fStepCaption := aStepCaption;
end;


destructor TKMResSprites.Destroy;
var
  RT: TRXType;
begin
  fGameRXTypes.Free;
  {$IFDEF LOAD_GAME_RES_ASYNC}
  // Stop resource loader before Freeing SpritePack, as loader use fRXData and could get an exception there on game exit
  if fGameResLoader <> nil then
    StopResourceLoader;
  {$ENDIF}

  for RT := Low(TRXType) to High(TRXType) do
    fSprites[RT].Free;

  inherited;
end;


//Clear unused RAM
procedure TKMResSprites.ClearTemp;
var RT: TRXType;
begin
  for RT := Low(TRXType) to High(TRXType) do
    fSprites[RT].ClearTemp;
end;


{$IFDEF LOAD_GAME_RES_ASYNC}
//Get next game resource RXType to load. Returns -1 if its the last one
function TKMResSprites.GetNextLoadRxTypeIndex(aRT: TRXType): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to fGameRXTypes.Count - 1 do
    if StrToInt(fGameRXTypes[I]) = Integer(aRT) then
    begin
      if I = fGameRXTypes.Count - 1 then
        Exit
      else begin
        Result := I + 1;
        Exit;
      end;
    end;
end;
{$ENDIF}


//aLegacyGeneration - support for maps with rev <= r10745, where transitions were written as generated terrain Id
//instead of generation parameters (TekKind + FullMaskType)
//Also there was no mkSoft mask and tkSnowOnTerrain terrain kind
procedure TKMResSprites.GenerateTerrainTransitions(aSprites: TKMSpritePack; aLegacyGeneration: Boolean = False);
//  procedure Rotate(aAngle: Byte; aXFrom, aYFrom: Integer; var aXTo, aYTo: Integer; aBase: Integer);
//  begin
//    case aAngle of
//      0:  begin
//            aXTo := aXFrom;
//            aYTo := aYFrom;
//          end;
//      1:  begin
//            aYTo := aXFrom;
//            aXTo := aBase - aYFrom;
//          end;
//      2:  begin
//            aXTo := aBase - aXFrom;
//            aYTo := aBase - aYFrom;
//          end;
//      3:  begin
//            aXTo := aYFrom;
//            aYTo := aBase - aXFrom;
//          end
//      else raise Exception.Create('Wrong Rotate angle');
//    end;
//
//  end;

var
  TK: TKMTerrainKind;
  MK: TKMTileMaskKind;
  MT: TKMTileMaskType;
  MST: TKMTileMaskSubType;
  TerrainId, MaskId: Word;
  MaskFullType: PKMMaskFullType;
  GenTerrainInfo: TKMGenTerrainInfo;

  TexId,{ K,} L, M, {P, }Q, MId, Tmp: Integer;
  GenTilesCnt, GenTilesCntTemp: Integer;
  StraightPixel{, RotatePixel}: Cardinal;
  GeneratedMasks: TStringList;
begin
  Assert(not aLegacyGeneration or (aSprites = nil));
  
  if aLegacyGeneration then
  begin
    TexId := 4999;
    fGenTexIdStartILegacy := TexId;
    GenTilesCntTemp := (Integer(High(TKMTerrainKind)) - 1)*Integer(High(TKMTileMaskKind))
                   *Integer(High(TKMTileMaskType))*(Integer(High(TKMTileMaskSubType)) + 1);
    SetLength(fGenTerrainToTerKindLegacy, GenTilesCntTemp);
  end
  else
  begin
    TexId := Length(aSprites.fRXData.RGBA) + 1;
    fGenTexIdStartI := TexId;
    GenTilesCnt := Integer(High(TKMTerrainKind))*Integer(High(TKMTileMaskKind))
                   *Integer(High(TKMTileMaskType))*(Integer(High(TKMTileMaskSubType)) + 1);
    SetLength(fGenTerrainToTerKind, GenTilesCnt);
    aSprites.Allocate(TexId + GenTilesCnt);
  end;
  GeneratedMasks := TStringList.Create;
//  K := 0;
  try
    //for all Terrain Kinds
    for TK := Low(TKMTerrainKind) to High(TKMTerrainKind) do
    begin
      if TK = tkCustom then Continue;

      if aLegacyGeneration and (TK = tkSnowOnGrass) then Continue;

      TerrainId := BASE_TERRAIN[TK] + 1; // in fRXData Tiles are 1-based

      //for all Mask Kinds
      for MK := Low(TKMTileMaskKind) to High(TKMTileMaskKind) do
      begin
        if MK = mkNone then Continue;

        if aLegacyGeneration and (MK = mkSoft2) then Continue;

        //for all Mask Types
        for MT := Low(TKMTileMaskType) to High(TKMTileMaskType) do
        begin
          if MT = mtNone then Continue;

          // for all Mask subtypes
          for MST := Low(TKMTileMaskSubType) to High(TKMTileMaskSubType) do //Mask subtypes (actual masks for layers)
          begin
            MaskId := TILE_MASKS_FOR_LAYERS[MK, MT, MST] + 1;
            if MaskId = 0 then Continue;   //Ignore non existent masks

            // We could use same mask image for several masktypes/subtypes
            MId := GeneratedMasks.IndexOf(IntToStr(MaskId));
            if MId > -1 then
            begin
              MaskFullType := PKMMaskFullType(GeneratedMasks.Objects[MId]);

              if aLegacyGeneration then
                Tmp := gGenTerrainTransitionsLegacy[TK, MaskFullType.Kind, MaskFullType.MType, MaskFullType.SubType]
              else
                Tmp := gGenTerrainTransitions[TK, MaskFullType.Kind, MaskFullType.MType, MaskFullType.SubType];
                
              if Tmp <> 0 then
              begin
                if aLegacyGeneration then
                  gGenTerrainTransitionsLegacy[TK, MK, MT, MST] := Tmp
                else
                  gGenTerrainTransitions[TK, MK, MT, MST] := Tmp;
                Continue;
              end;
            end else begin
              System.New(MaskFullType);
              MaskFullType.Kind := MK;
              MaskFullType.MType := MT;
              MaskFullType.SubType := MST;
              GeneratedMasks.AddObject(IntToStr(MaskId), TObject(MaskFullType));
            end;

    //        for K := 0 to 3 do //Rotation
    //        begin
              GenTerrainInfo.TerKind := TK;
              GenTerrainInfo.Mask.Kind := MK;
              GenTerrainInfo.Mask.MType := MT;
              GenTerrainInfo.Mask.SubType := MST;
              
              if aLegacyGeneration then
              begin
                gGenTerrainTransitionsLegacy[TK, MK, MT, MST] := TexId - 1;
                fGenTerrainToTerKindLegacy[TexId - fGenTexIdStartILegacy] := GenTerrainInfo;
              end
              else
              begin
                SetLength(aSprites.fRXData.RGBA[TexId], aSprites.fRXData.Size[TerrainId].X*aSprites.fRXData.Size[TerrainId].Y); //32*32 actually...
                SetLength(aSprites.fRXData.Mask[TexId], aSprites.fRXData.Size[TerrainId].X*aSprites.fRXData.Size[TerrainId].Y);
                aSprites.fRXData.Flag[TexId] := aSprites.fRXData.Flag[TerrainId];
                aSprites.fRXData.Size[TexId].X := aSprites.fRXData.Size[TerrainId].X;
                aSprites.fRXData.Size[TexId].Y := aSprites.fRXData.Size[TerrainId].Y;
                aSprites.fRXData.Pivot[TexId] := aSprites.fRXData.Pivot[TerrainId];
                aSprites.fRXData.HasMask[TexId] := False;
                gGenTerrainTransitions[TK, MK, MT, MST] := TexId - 1; //TexId is 1-based, but textures we use - 0 based
                fGenTerrainToTerKind[TexId - fGenTexIdStartI] := GenTerrainInfo;
              
    //          gLog.AddTime(Format('TerKind: %10s Mask: %10s TexId: %d ', [GetEnumName(TypeInfo(TKMTerrainKind), Integer(I)),
    //                                                        GetEnumName(TypeInfo(TKMTileMaskType), Integer(J)), TexId]));

    //          fGenTerrainToTerKind.Add(IntToStr(TexId) + '=' + IntToStr(Integer(I)));
                for L := 0 to aSprites.fRXData.Size[TerrainId].Y - 1 do
                  for M := 0 to aSprites.fRXData.Size[TerrainId].X - 1 do
                  begin
      //              Rotate(K, L, M, P, Q, aSprites.fRXData.Size[TerrainId].X - 1);
                    StraightPixel := L * aSprites.fRXData.Size[TerrainId].X  + M;
      //              RotatePixel := StraightPixel; //P * aSprites.fRXData.Size[TerrainId].X  + Q;
                    aSprites.fRXData.RGBA[TexId, StraightPixel] := ($FFFFFF or (aSprites.fRXData.RGBA[MaskId, StraightPixel] shl 24))
                                                       and aSprites.fRXData.RGBA[TerrainId, StraightPixel{RotatePixel}];
                  end;
              end;
              Inc(TexId);
    //        end;
          end;
        end;
      end;
    end;
    if aLegacyGeneration then
      SetLength(fGenTerrainToTerKindLegacy, TexId - fGenTexIdStartILegacy)
    else
    begin
      // There could be usused place in arrays, as we could use same mask image for different purposes
      aSprites.Allocate(TexId);
      SetLength(fGenTerrainToTerKind, TexId - fGenTexIdStartI);
    end;
    
  finally
    for Q := 0 to GeneratedMasks.Count - 1 do
      System.Dispose(PKMMaskFullType(GeneratedMasks.Objects[Q]));
    FreeAndNil(GeneratedMasks);
  end;
end;


function TKMResSprites.GetGenTerrainInfo(aTerrain: Integer): TKMGenTerrainInfo;
begin
  Assert(aTerrain + 1 - fGenTexIdStartI < Length(fGenTerrainToTerKind),
         Format('Try to get terrain info out of range: TileID = %d, GenStart = %d, GenTerrain arr length = %d',
                [aTerrain, fGenTexIdStartI, Length(fGenTerrainToTerKind)]));

  Result := fGenTerrainToTerKind[aTerrain + 1 - fGenTexIdStartI]; //TexId is 1-based, but textures we use - 0 based
end;


function TKMResSprites.GetGenTerrainInfoLegacy(aTerrain: Integer): TKMGenTerrainInfo;
begin
  Assert(aTerrain + 1 - fGenTexIdStartILegacy < Length(fGenTerrainToTerKindLegacy),
         Format('Try to get terrain info LEGACY out of range: TileID = %d, GenStart = %d, GenTerrain arr length = %d',
                [aTerrain, fGenTexIdStartILegacy, Length(fGenTerrainToTerKindLegacy)]));

  Result := fGenTerrainToTerKindLegacy[aTerrain + 1 - fGenTexIdStartILegacy]; //TexId is 1-based, but textures we use - 0 based
end;


function TKMResSprites.GetRXFileName(aRX: TRXType): string;
begin
  Result := RXInfo[aRX].FileName;
end;


function TKMResSprites.GetSprites(aRT: TRXType): TKMSpritePack;
begin
  Result := fSprites[aRT];
end;


procedure TKMResSprites.LoadMenuResources;
var
  RT: TRXType;
begin
  for RT := Low(TRXType) to High(TRXType) do
    if RXInfo[RT].Usage = ruMenu then
    begin
      fStepCaption('Reading ' + RXInfo[RT].FileName + ' ...');
      LoadSprites(RT, RT = rxGUI); //Only GUI needs alpha shadows
      fSprites[RT].MakeGFX(RT = rxGUI);
      fStepProgress;
    end;
end;


{$IFDEF LOAD_GAME_RES_ASYNC}
procedure TKMResSprites.StopResourceLoader;
begin
//  fGameResLoader.DoTerminate := True;
  fGameResLoader.Terminate;
  fGameResLoader.WaitFor;
  FreeThenNil(fGameResLoader);
  if gLog <> nil then //could be nil in some Utils, f.e.
    gLog.MultithreadLogging := False;
end;
{$ENDIF}


procedure TKMResSprites.LoadGameResources(aAlphaShadows: Boolean; aForceReload: Boolean = False);

  procedure LoadAllResources;
  var
    RT: TRXType;
  begin
    for RT := Low(TRXType) to High(TRXType) do
      if RXInfo[RT].Usage = ruGame then
      begin
        fStepCaption(gResTexts[RXInfo[RT].LoadingTextID]);
        gLog.AddTime('Reading ' + RXInfo[RT].FileName + '.rx');
        LoadSprites(RT, fAlphaShadows);
        fSprites[RT].MakeGFX(fAlphaShadows);
      end;
  end;

begin
  //Remember which version we load, so if it changes inbetween games we reload it
  fAlphaShadows := aAlphaShadows;
  {$IFDEF LOAD_GAME_RES_ASYNC}
  if gGameApp.GameSettings.AsyncGameResLoad then
  begin
    if fGameResLoader <> nil then
    begin
      if aForceReload then
        StopResourceLoader
      else begin
        while not fGameResLoadCompleted do
        begin
          ManageResLoader; //check if we have some work to do in this thread
          Sleep(5); // wait till load will be completed by fGameResLoader thread
        end;
        Exit;
      end;
    end;

    if aForceReload then
    begin
      fGameResLoadCompleted := False;
      if gLog <> nil then //could be nil in some Utils, f.e.
        gLog.MultithreadLogging := True;
      fGameResLoader := TTGameResourceLoader.Create(Self, fAlphaShadows, TRXType(StrToInt(fGameRXTypes[0])));
    end;
  end
  else
    LoadAllResources;
    
  {$ELSE}
  LoadAllResources;
  {$ENDIF}
end;


procedure TKMResSprites.ClearGameResGenTemp;
var
  SAT: TSpriteAtlasType;
begin
  for SAT := Low(TSpriteAtlasType) to High(TSpriteAtlasType) do
    SetLength(gGFXPrepData[SAT], 0);
end;

{$IFDEF LOAD_GAME_RES_ASYNC}
// Generate texture atlases from previosly prepared SpriteInfo data (loaded from RXX, Bin Packed, copied to atlas)
// Preparation was done asynchroniously by TTGameResourceLoader thread
// Texture generating task can be done only by main thread, as OpenGL does not work with multiple threads
procedure TKMResSprites.GenerateTextureAtlasForGameRes(aRT: TRXType);
var
  I: Integer;
  SAT: TSpriteAtlasType;
  Tx: Cardinal;
  SpritesPack: TKMSpritePack;
  TexFilter: TFilterType;
begin
  SpritesPack := GetSprites(aRT);
  for SAT := Low(TSpriteAtlasType) to High(TSpriteAtlasType) do
    for I := Low(gGFXPrepData[SAT]) to High(gGFXPrepData[SAT]) do
    begin
      with gGFXPrepData[SAT,I] do
      begin
        TexFilter := ftNearest;
        if LINEAR_FILTER_SPRITES and (aRT in [rxTrees, rxHouses, rxUnits]) then
          TexFilter := ftLinear;

        Tx := TRender.GenTexture(SpriteInfo.Width, SpriteInfo.Height, @Data[0], TexType, TexFilter, TexFilter);
        //Now that we know texture IDs we can fill GFXData structure
        SetGFXData(Tx, SpriteInfo, SAT, SpritesPack, aRT);

        SpritesPack.SaveTextureToPNG(SpriteInfo.Width, SpriteInfo.Height, RXInfo[aRT].FileName + '_' +
                                     SPRITE_TYPE_EXPORT_NAME[SAT] + IntToStr(I+1), Data);
      end;
    end;
end;
{$ENDIF}


//Try to load RXX first, then RX, then use Folder
function TKMResSprites.LoadSprites(aRT: TRXType; aAlphaShadows: Boolean): Boolean;
begin
  Result := False;
  if aAlphaShadows and FileExists(ExeDir + 'data' + PathDelim + 'Sprites' + PathDelim + RXInfo[aRT].FileName + '_a.rxx') then
  begin
    fSprites[aRT].LoadFromRXXFile(ExeDir + 'data' + PathDelim + 'Sprites' + PathDelim + RXInfo[aRT].FileName + '_a.rxx');
    Result := True;
  end
  else
  if FileExists(ExeDir + 'data' + PathDelim + 'Sprites' + PathDelim + RXInfo[aRT].FileName + '.rxx') then
  begin
    fSprites[aRT].LoadFromRXXFile(ExeDir + 'data' + PathDelim + 'Sprites' + PathDelim + RXInfo[aRT].FileName + '.rxx');
    Result := True;
  end
  else
    Exit;

  fSprites[aRT].OverloadFromFolder(ExeDir + 'Sprites' + PathDelim);

  if aRT = rxTiles then
  begin
    GenerateTerrainTransitions(fSprites[aRT]);
    GenerateTerrainTransitions(nil, True); //To get support for maps rev <= 10745
  end;
end;


class function TKMResSprites.AllTilesOnOneAtlas: Boolean;
begin
  Result := AllTilesInOneTexture;
end;


class procedure TKMResSprites.SetMaxAtlasSize(aMaxSupportedTxSize: Integer);
begin
  MaxAtlasSize := Min(aMaxSupportedTxSize, MAX_GAME_ATLAS_SIZE);
end;


{$IFDEF LOAD_GAME_RES_ASYNC}
procedure TKMResSprites.ManageResLoader;
var
  NextRXTypeI: Integer;
begin
  if gGameApp.GameSettings.AsyncGameResLoad
    and (fGameResLoader <> nil)
    and fGameResLoader.LoadStepDone then
  begin
    // Generate texture atlas from prepared data for game resources
    // OpenGL work mainly with 1 thread only, so we have to call gl functions only from main thread
    // That is why we need call this method from main thread only
    GenerateTextureAtlasForGameRes(fGameResLoader.RXType);
    fStepCaption(gResTexts[RXInfo[fGameResLoader.RXType].LoadingTextID]);
    fSprites[fGameResLoader.RXType].ClearTemp;      //Clear fRXData sprites temp data, which is not needed anymore
    ClearGameResGenTemp;                                   //Clear all the temp data used for atlas texture generating
    NextRXTypeI := GetNextLoadRxTypeIndex(fGameResLoader.RXType); // get next RXType to load
    if NextRXTypeI = -1 then
    begin
      //Load is completed, we can stop loading thread
      StopResourceLoader;
      fGameResLoadCompleted := True; // mark loading game res as completed
    end else begin
      fGameResLoader.RXType := TRXType(StrToInt(fGameRXTypes[NextRXTypeI]));
      fGameResLoader.LoadStepDone := False;
    end;
  end;
end;
{$ENDIF}


procedure TKMResSprites.UpdateStateIdle;
begin
  {$IFDEF LOAD_GAME_RES_ASYNC}
  ManageResLoader;
  {$ENDIF}
end;


procedure TKMResSprites.ExportToPNG(aRT: TRXType);
begin
  if LoadSprites(aRT, False) then
  begin
    fSprites[aRT].ExportAll(ExeDir + 'Export' + PathDelim + RXInfo[aRT].FileName + '.rx' + PathDelim);
    ClearTemp;
  end;
end;


{ TTGameResourceLoader }
constructor TTGameResourceLoader.Create(aResSprites: TKMResSprites; aAlphaShadows: Boolean; aRxType: TRXType);
begin
  inherited Create(False);

  {$IFDEF DEBUG}
  TThread.NameThreadForDebugging('GameResourceLoader', ThreadID);
  {$ENDIF}

  fResSprites := aResSprites;
  fAlphaShadows := aAlphaShadows;
  RXType := aRxType;
  FreeOnTerminate := False; //object can be automatically removed after its termination
end;


procedure TTGameResourceLoader.Execute;
begin
  inherited;
  while not Terminated do
  begin
    if not LoadStepDone then
    begin
      fResSprites.LoadSprites(RXType, fAlphaShadows);
      if Terminated then Exit;
      fResSprites.fSprites[RXType].MakeGFX(fAlphaShadows, 1, False, IsTerminated);
      LoadStepDone := True;
    end;
    Sleep(1); // sleep a a bit
  end;
end;


function TTGameResourceLoader.IsTerminated: Boolean;
begin
  Result := Terminated;
end;


end.
