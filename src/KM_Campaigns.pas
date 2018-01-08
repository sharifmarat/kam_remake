unit KM_Campaigns;
{$I KaM_Remake.inc}
interface
uses
  Classes,
  KM_ResTexts, KM_Pics, JsonDataObjects,
  KM_CommonClasses, KM_Points;


const
  MAX_CAMP_CHAPTERS = 64;
  MAX_CAMP_MAPS = 64;
  MAX_CAMP_NODES = 64;

type
  TBriefingCorner = (bcBottomRight, bcBottomLeft);
  TMissionVideoTypes = (mvBefore, mvAfter);
  //Unique campaign identification, stored as 3 ANSI letters (TSK, TPR, etc)
  //3 bytes are used to avoid string types issues
  TKMCampaignId = array [0..2] of Byte;

  TKMCampaignMap = record
    Flag: TKMPointW;
    NodeCount: Byte;
    Nodes: array [0 .. MAX_CAMP_NODES - 1] of TKMPointW;
    TextPos: TBriefingCorner;
    Video: array[TMissionVideoTypes] of UnicodeString;
  end;

  TKMCampaignChapter = record
    Maps: array of TKMCampaignMap;
    MapCount: Byte;
    PictureMapIndex: Byte;
    Video: array[TMissionVideoTypes] of UnicodeString;
  end;

  TKMCampaign = class
  private
    //Runtime variables
    fPath: UnicodeString;
    fTextLib: TKMTextLibrarySingle;
    fUnlockedMap: Byte;
    fScriptData: TKMemoryStream;
    fFullName: UnicodeString;
    fNodeAnimation: Integer;

    //Saved in CMP
    fCampaignId: TKMCampaignId; //Used to identify the campaign
    fBackGroundPic: TKMPic;
    procedure SetUnlockedMap(aValue: Byte);
    function GetMapCount: Byte;

    function GetMap(Index: Byte): TKMCampaignMap;
  public
    Chapters: array of TKMCampaignChapter;
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromDir(const aDirName: UnicodeString);
    procedure SaveToDir(const aDirName: UnicodeString);
    procedure LoadFromPath(const aPath: UnicodeString);

    property Maps[Index: Byte]: TKMCampaignMap read GetMap;
    property BackGroundPic: TKMPic read fBackGroundPic write fBackGroundPic;
    property MapCount: Byte read GetMapCount;
    property CampaignId: TKMCampaignId read fCampaignId write fCampaignId;
    property FullName: UnicodeString read fFullName write fFullName;
    function CampName: UnicodeString;
    property UnlockedMap: Byte read fUnlockedMap write SetUnlockedMap;
    property ScriptData: TKMemoryStream read fScriptData;
    property NodeAnimation: Integer Read fNodeAnimation write fNodeAnimation;

    function CampaignTitle: UnicodeString;
    function CampaignDescription: UnicodeString;
    function CampaignMissionTitle(aIndex: Byte): UnicodeString;
    function MissionFile(aIndex: Byte): UnicodeString;
    function MissionTitle(aIndex: Byte): UnicodeString;
    function MissionBriefing(aIndex: Byte): UnicodeString;
    function BreifingAudioFile(aIndex: Byte): UnicodeString;
    function ScriptDataTypeFile: UnicodeString;
  end;


  TKMCampaignsCollection = class
  private
    fActiveCampaign: TKMCampaign; //Campaign we are playing
    fActiveCampaignMap: Byte; //Map of campaign we are playing, could be different than UnlockedMaps
    fList: TList;
    function GetCampaign(aIndex: Integer): TKMCampaign;
    procedure AddCampaign(const aPath: UnicodeString);
  public
    constructor Create;
    destructor Destroy; override;

    //Initialization
    procedure ScanFolder(const aPath: UnicodeString);
    procedure SortCampaigns;
    procedure LoadProgress(const aFileName: UnicodeString);
    procedure SaveProgress(const aFileName: UnicodeString);
    procedure Save(SaveStream: TKMemoryStream);
    procedure Load(LoadStream: TKMemoryStream);

    //Usage
    property ActiveCampaign: TKMCampaign read fActiveCampaign;// write fActiveCampaign;
    function Count: Integer;
    property Campaigns[aIndex: Integer]: TKMCampaign read GetCampaign; default;
    function CampaignById(const aCampaignId: TKMCampaignId): TKMCampaign;
    procedure SetActive(aCampaign: TKMCampaign; aMap: Byte);
    procedure UnlockNextMap;
  end;


const
  NO_CAMPAIGN: TKMCampaignId = (0, 0, 0);

implementation
uses
  SysUtils, Math, KromUtils,
  KM_Resource, KM_ResLocales, KM_ResSprites,
  KM_Log, KM_Defaults;


const
  CAMP_HEADER_V1 = $FEED; //Just some header to separate right progress files from wrong
  CAMP_HEADER_V2 = $BEEF;


{ TCampaignsCollection }
constructor TKMCampaignsCollection.Create;
begin
  inherited Create;

  fList := TList.Create;
end;


destructor TKMCampaignsCollection.Destroy;
var
  I: Integer;
begin
  //Free list objects
  for I := 0 to Count - 1 do
    Campaigns[I].Free;

  fList.Free;
  inherited;
end;


procedure TKMCampaignsCollection.AddCampaign(const aPath: UnicodeString);
var
  C: TKMCampaign;
begin
  C := TKMCampaign.Create;
  C.LoadFromPath(aPath);
  fList.Add(C);
end;


//Scan campaigns folder
procedure TKMCampaignsCollection.ScanFolder(const aPath: UnicodeString);
var
  SearchRec: TSearchRec;
begin
  if not DirectoryExists(aPath) then Exit;

  FindFirst(aPath + '*', faDirectory, SearchRec);
  repeat
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
    and (SearchRec.Attr and faDirectory = faDirectory)
    and (FileExists(aPath + SearchRec.Name + PathDelim + 'info.cmp')
      or FileExists(aPath + SearchRec.Name + PathDelim + 'info.json')
      ) then
      AddCampaign(aPath + SearchRec.Name + PathDelim);
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);

  SortCampaigns;
end;


procedure TKMCampaignsCollection.SortCampaigns;

  //Return True if items should be exchanged
  function Compare(A, B: TKMCampaign): Boolean;
  begin
    //TSK is first
    if      A.CampName = 'TSK' then Result := False
    else if B.CampName = 'TSK' then Result := True
    //TPR is second
    else if A.CampName = 'TPR' then Result := False
    else if B.CampName = 'TPR' then Result := True
    //Others are left in existing order (alphabetical)
    else                            Result := False;
  end;

var I, K: Integer;
begin
  for I := 0 to Count - 1 do
    for K := I to Count - 1 do
      if Compare(Campaigns[I], Campaigns[K]) then
        SwapInt(NativeUInt(fList.List[I]), NativeUInt(fList.List[K]));
end;


procedure TKMCampaignsCollection.SetActive(aCampaign: TKMCampaign; aMap: Byte);
begin
  fActiveCampaign := aCampaign;
  fActiveCampaignMap := aMap;
end;


function TKMCampaignsCollection.GetCampaign(aIndex: Integer): TKMCampaign;
begin
  Result := fList[aIndex];
end;


//Read progress from file trying to find matching campaigns
procedure TKMCampaignsCollection.LoadProgress(const aFileName: UnicodeString);
var
  M: TKMemoryStream;
  C: TKMCampaign;
  I, campCount: Integer;
  campName: TKMCampaignId;
  unlocked: Byte;
  HasScriptData: Boolean;
  ScriptDataSize: Cardinal;
begin
  if not FileExists(aFileName) then Exit;

  M := TKMemoryStream.Create;
  try
    M.LoadFromFile(aFileName);

    M.Read(I); //Check for wrong file format
    //All campaigns will be kept in initial state
    if (I <> CAMP_HEADER_V1) and (I <> CAMP_HEADER_V2) then Exit;
    HasScriptData := (I = CAMP_HEADER_V2);

    M.Read(campCount);
    for I := 0 to campCount - 1 do
    begin
      M.Read(campName, sizeOf(TKMCampaignId));
      M.Read(unlocked);
      C := CampaignById(campName);
      if C <> nil then
      begin
        C.UnlockedMap := unlocked;
        C.ScriptData.Clear;
        if HasScriptData then
        begin
          M.Read(ScriptDataSize);
          C.ScriptData.Write(Pointer(Cardinal(M.Memory) + M.Position)^, ScriptDataSize);
          M.Seek(ScriptDataSize, soCurrent); //Seek past script data
        end;
      end;
    end;
  finally
    M.Free;
  end;
end;


procedure TKMCampaignsCollection.SaveProgress(const aFileName: UnicodeString);
var
  M: TKMemoryStream;
  I: Integer;
begin
  //Makes the folder incase it is missing
  ForceDirectories(ExtractFilePath(aFileName));

  M := TKMemoryStream.Create;
  try
    M.Write(Integer(CAMP_HEADER_V2)); //Identify our format
    M.Write(Count);
    for I := 0 to Count - 1 do
    begin
      M.Write(Campaigns[I].CampaignId, SizeOf(TKMCampaignId));
      M.Write(Campaigns[I].UnlockedMap);
      M.Write(Cardinal(Campaigns[I].ScriptData.Size));
      M.Write(Campaigns[I].ScriptData.Memory^, Campaigns[I].ScriptData.Size);
    end;

    M.SaveToFile(aFileName);
  finally
    M.Free;
  end;

  gLog.AddTime('Campaigns.dat saved');
end;


function TKMCampaignsCollection.Count: Integer;
begin
  Result := fList.Count;
end;


function TKMCampaignsCollection.CampaignById(const aCampaignId: TKMCampaignId): TKMCampaign;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if (Campaigns[I].CampaignId[0] = aCampaignId[0])
    and (Campaigns[I].CampaignId[1] = aCampaignId[1])
    and (Campaigns[I].CampaignId[2] = aCampaignId[2]) then
      Result := Campaigns[I];
end;


procedure TKMCampaignsCollection.UnlockNextMap;
begin
  if ActiveCampaign <> nil then
    ActiveCampaign.UnlockedMap := fActiveCampaignMap + 1;
end;


procedure TKMCampaignsCollection.Load(LoadStream: TKMemoryStream);
var
  cmp: TKMCampaignId;
begin
  LoadStream.ReadAssert('CampaignInfo');
  LoadStream.Read(cmp, SizeOf(TKMCampaignId));
  fActiveCampaign := CampaignById(cmp);
  LoadStream.Read(fActiveCampaignMap);
  //If loaded savegame references to missing campaign it will be treated as single-map (fActiveCampaign = nil)
end;


procedure TKMCampaignsCollection.Save(SaveStream: TKMemoryStream);
var
  cmp: TKMCampaignId;
begin
  SaveStream.WriteA('CampaignInfo');

  if fActiveCampaign <> nil then
    cmp := fActiveCampaign.CampaignId;

  SaveStream.Write(cmp, SizeOf(TKMCampaignId));
  SaveStream.Write(fActiveCampaignMap);
end;


{ TKMCampaign }
constructor TKMCampaign.Create;
begin
  inherited;

  SetLength(Chapters, 1);;

  //1st map is always unlocked to allow to start campaign
  fUnlockedMap := 0;
  fScriptData := TKMemoryStream.Create;
end;


destructor TKMCampaign.Destroy;
begin
  FreeAndNil(fTextLib);
  fScriptData.Free;

  //Free background texture
  if fBackGroundPic.ID <> 0 then
    gRes.Sprites[rxCustom].DeleteSpriteTexture(fBackGroundPic.ID);

  inherited;
end;


//Load campaign info from *.cmp file
//It should be private, but it is used by CampaignBuilder
procedure TKMCampaign.LoadFromDir(const aDirName: UnicodeString);

  procedure JsonToMap(AJson: TJsonObject; var AMap: TKMCampaignMap);
  var
    i: Integer;
  begin
    AMap.Flag.X := AJson.O['Flag']['X'];
    AMap.Flag.Y := AJson.O['Flag']['Y'];
    AMap.TextPos := TBriefingCorner(AJson['TextPos']);

    AMap.Video[mvBefore] := AJson['VideoBefore'];
    AMap.Video[mvAfter] := AJson['VideoAfter'];

    AMap.NodeCount := AJson.A['Nodes'].Count;
    for i := 0 to AMap.NodeCount - 1 do
    begin
      AMap.Nodes[i].X := AJson.A['Nodes'].Items[i].ObjectValue['X'];
      AMap.Nodes[i].Y := AJson.A['Nodes'].Items[i].ObjectValue['Y'];
    end;
  end;

  procedure JsonToChapter(AJson: TJsonObject; var AChapter: TKMCampaignChapter);
  var
    i: Integer;
  begin
    AChapter.PictureMapIndex := AJson['PictureMapIndex'];

    AChapter.Video[mvBefore] := AJson['VideoBefore'];
    AChapter.Video[mvAfter] := AJson['VideoAfter'];

    AChapter.MapCount := AJson['Maps'].Count;
    SetLength(AChapter.Maps, AChapter.MapCount);
    for i := 0 to AChapter.MapCount - 1 do
      JsonToMap(AJson['Maps'].Items[i], AChapter.Maps[i]);
  end;

var
  CampaignMemory, M: TKMemoryStream;
  I, K: Integer;
  cmp: TBytes;
  Count: Byte;
  Size, Level, VideoType: Byte;
  Json: TJsonObject;
begin
  Assert(aDirName <> '');

  if FileExists(aDirName + '\info.json') then
  begin
    Json := TJsonObject.Create;
    Json.LoadFromFile(aDirName + '\info.json');
    try
     fCampaignId[0] := Ord(Json['ID'].Value[1]);
     fCampaignId[1] := Ord(Json['ID'].Value[2]);
     fCampaignId[2] := Ord(Json['ID'].Value[3]);
     NodeAnimation := Json['NodeAnimation'];

     SetLength(Chapters, Json['Chapters'].Count);
     for i := 0 to Json['Chapters'].Count - 1 do
       JsonToChapter(Json['Chapters'].Items[i], Chapters[i]);

    finally
      Json.Free;
    end;

  end
  else
    if FileExists(aDirName + '\info.cmp') then
    begin
      NodeAnimation := 0;
      CampaignMemory := TKMemoryStream.Create;
      CampaignMemory.LoadFromFile(aDirName + '\info.cmp');
      //Convert old AnsiString into new [0..2] Byte format
      CampaignMemory.ReadBytes(cmp);
      Assert(Length(cmp) = 3);
      fCampaignId[0] := cmp[0];
      fCampaignId[1] := cmp[1];
      fCampaignId[2] := cmp[2];

      CampaignMemory.Read(Count);
      SetLength(Chapters, 1);
      Chapters[0].MapCount := Count;
      Chapters[0].PictureMapIndex := 1;
      SetLength(Chapters[0].Maps, Count);

      for I := 0 to Count - 1 do
      begin
        CampaignMemory.Read(Chapters[0].Maps[I].Flag);
        CampaignMemory.Read(Chapters[0].Maps[I].NodeCount);
        for K := 0 to Chapters[0].Maps[I].NodeCount - 1 do
          CampaignMemory.Read(Chapters[0].Maps[I].Nodes[K]);
        CampaignMemory.Read(Chapters[0].Maps[I].TextPos, SizeOf(TBriefingCorner));
      end;

      CampaignMemory.Free;
    end;
end;


procedure TKMCampaign.SaveToDir(const aDirName: UnicodeString);

  procedure MapToJson(AJsonArray: TJsonArray; const AMap: TKMCampaignMap);
  var
    i: Integer;
    Json, JsonNode: TJsonObject;
  begin
    Json := AJsonArray.AddObject;

    Json.O['Flag']['X'] := AMap.Flag.X;
    Json.O['Flag']['Y'] := AMap.Flag.Y;

    Json['TextPos'] := Integer(AMap.TextPos);
    Json['VideoBefore'] := AMap.Video[mvBefore];
    Json['VideoAfter'] := AMap.Video[mvAfter];

    for i := 0 to AMap.NodeCount - 1 do
    begin
      JsonNode := Json.A['Nodes'].AddObject;
      JsonNode['X'] := AMap.Nodes[i].X;
      JsonNode['Y'] := AMap.Nodes[i].Y;
    end;
  end;

  procedure ChapterToJson(AJsonArray: TJsonArray; const AChapter: TKMCampaignChapter);
  var
    i: Integer;
    Json: TJsonObject;
  begin
    Json := AJsonArray.AddObject;
    Json['PictureMapIndex'] := AChapter.PictureMapIndex;
    if AChapter.Video[mvBefore] <> '' then
      Json['VideoBefore'] := AChapter.Video[mvBefore];
    Json['VideoAfter'] := AChapter.Video[mvAfter];

    for i := 0 to AChapter.MapCount - 1 do
      MapToJson(Json.A['Maps'], AChapter.Maps[i]);
  end;

var
  i: Integer;
  Json: TJsonObject;
begin
  Assert(aDirName <> '');

  Json := TJsonObject.Create;
  try
    Json['ID'] := CampName;
    Json['NodeAnimation'] := NodeAnimation;

    for i := 0 to High(Chapters) do
      ChapterToJson(Json.A['Chapters'], Chapters[i]);

    Json.SaveToFile(aDirName + '\info.json');
  finally
    Json.Free;
  end;
end;


function TKMCampaign.ScriptDataTypeFile: UnicodeString;
begin
  Result := fPath + 'campaigndata.script';
end;


procedure TKMCampaign.LoadFromPath(const aPath: UnicodeString);
var
  SP: TKMSpritePack;
  FirstSpriteIndex: Word;
begin
  fPath := aPath;

  LoadFromDir(fPath);

  FreeAndNil(fTextLib);
  fTextLib := TKMTextLibrarySingle.Create;
  fTextLib.LoadLocale(fPath + 'text.%s.libx');

  if gRes.Sprites <> nil then
  begin
    SP := gRes.Sprites[rxCustom];
    FirstSpriteIndex := SP.RXData.Count + 1;
    SP.LoadFromRXXFile(fPath + 'images.rxx', FirstSpriteIndex);

    if FirstSpriteIndex <= SP.RXData.Count then
    begin
      //Images were successfuly loaded
      SP.MakeGFX(False, FirstSpriteIndex);
      SP.ClearTemp;
      fBackGroundPic.RX := rxCustom;
      fBackGroundPic.ID := FirstSpriteIndex;
    end
    else
    begin
      //Images were not found - use blank
      fBackGroundPic.RX := rxCustom;
      fBackGroundPic.ID := 0;
    end;
  end;

  if UNLOCK_CAMPAIGN_MAPS then //Unlock more maps for debug
    fUnlockedMap := MapCount - 1;
end;

function TKMCampaign.GetMapCount: Byte;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(Chapters) do
    Inc(Result, Chapters[i].MapCount);
end;

function TKMCampaign.GetMap(Index: Byte): TKMCampaignMap;
var
  i, N: Integer;
begin
  N := 0;
  for i := 0 to High(Chapters) do
  begin
    if Index < N + Chapters[i].MapCount then
      Exit(Chapters[i].Maps[Index - N]);
    Inc(N, Chapters[i].MapCount);
  end;
end;

function TKMCampaign.CampaignTitle: UnicodeString;
begin
  Result := fTextLib[0];
end;


function TKMCampaign.CampName: UnicodeString;
begin
  Result := WideChar(fCampaignId[0]) + WideChar(fCampaignId[1]) + WideChar(fCampaignId[2]);
end;


function TKMCampaign.CampaignDescription: UnicodeString;
begin
  Result := fTextLib[2];
end;


function TKMCampaign.CampaignMissionTitle(aIndex: Byte): UnicodeString;
begin
  if fTextLib[3] <> '' then
  begin
    Assert(CountMatches(fTextLib[3], '%d') = 1, 'Custom campaign mission template must have a single "%d" in it.');
    Result := Format(fTextLib[3], [aIndex+1]);
  end
  else
    Result := Format(gResTexts[TX_GAME_MISSION], [aIndex+1]);
end;


function TKMCampaign.MissionFile(aIndex: Byte): UnicodeString;
begin
  Result := fPath + CampName + Format('%.2d', [aIndex + 1]) + PathDelim +
   CampName + Format('%.2d', [aIndex + 1]) + '.dat';
end;


function TKMCampaign.MissionTitle(aIndex: Byte): UnicodeString;
begin
  Result := Format(fTextLib[1], [aIndex+1]);
end;


//Mission texts of original campaigns are available in all languages,
//custom campaigns are unlikely to have more texts in more than 1-2 languages
function TKMCampaign.MissionBriefing(aIndex: Byte): UnicodeString;
begin
  Result := fTextLib[10 + aIndex];
end;


function TKMCampaign.BreifingAudioFile(aIndex: Byte): UnicodeString;
begin
  Result := fPath + CampName + Format('%.2d', [aIndex+1]) + PathDelim +
            CampName + Format('%.2d', [aIndex + 1]) + '.' + UnicodeString(gResLocales.UserLocale) + '.mp3';

  if not FileExists(Result) then
    Result := fPath + CampName + Format('%.2d', [aIndex+1]) + PathDelim +
              CampName + Format('%.2d', [aIndex + 1]) + '.' + UnicodeString(gResLocales.FallbackLocale) + '.mp3';

  if not FileExists(Result) then
    Result := fPath + CampName + Format('%.2d', [aIndex+1]) + PathDelim +
              CampName + Format('%.2d', [aIndex + 1]) + '.' + UnicodeString(gResLocales.DefaultLocale) + '.mp3';
end;


//When player completes one map we allow to reveal the next one, note that
//player may be replaying previous maps, in that case his progress remains the same
procedure TKMCampaign.SetUnlockedMap(aValue: Byte);
begin
  fUnlockedMap := EnsureRange(aValue, fUnlockedMap, MapCount - 1);
end;


end.
