unit KM_Video;

{$I KaM_Remake.inc}

interface

uses
  Vcl.Forms, SysUtils, SyncObjs, Types, Messages, Classes, Dialogs, KromOGLUtils
{$IFDEF VIDEOS}
  , PasLibVlcUnit
{$ENDIF}
  {$IFDEF WDC} , UITypes {$ENDIF}
  {$IFDEF FPC} , Controls {$ENDIF}
  ;

{$IFDEF VIDEOS}
const
  VLC_PATH = 'lib\vlc';
  VIDEOFILE_PATH = 'data\gfx\video\';

  WM_MEDIA_PLAYER_EVENTS = WM_USER + 1;

type
  TVlcPlayerState = (
    plvPlayer_NothingSpecial,
    plvPlayer_Opening,
    plvPlayer_Buffering,
    plvPlayer_Playing,
    plvPlayer_Paused,
    plvPlayer_Stopped,
    plvPlayer_Ended,
    plvPlayer_Error
  );

  TVlcMessage = record
    Msg: DWORD;
    Result: Integer;
    EventType: libvlc_event_type_t;
    Data: Int64;
  end;
{$ENDIF}

type
  TKMVideoPlayerEvent = reference to procedure;

  TKMVideoPlayer = class
  private
{$IFDEF VIDEOS}
    FCriticalSection: TCriticalSection;

    FBuffer: array of Byte;

    FWidth: LongWord;
    FHeight: LongWord;

    FScreenWidth: Integer;
    FScreenHeight: Integer;

    FNext: Boolean;
    FTexture: TTexture;

    FIndex: Integer;
    FLenght: Int64;
    FTime: Int64;

    FInstance: libvlc_instance_t_ptr;
    FMediaPlayer: libvlc_media_player_t_ptr;
    FEvents: libvlc_event_manager_t_ptr;

    FTrackList: TStringList;
    FVideoList: TStringList;
    FEndVideo: TKMVideoPlayerEvent;

    procedure InitInstance;
    procedure InitMediaPlayer;
    procedure DestroyMediaPlayer;
    procedure EventsEnable();
    procedure EventsDisable();
    procedure DoPlay(aVideoName: array of string; aEndVideo: TKMVideoPlayerEvent = nil);
    procedure DoStop;
    procedure PlayNext;
    procedure Finish;

    function TryGetPathFile(aPath: string; var aFileName: string): Boolean;
    procedure SetTrackByLocale;
    function GetState: TVlcPlayerState;
    procedure MediaPlayerEvents(var Msg: TVlcMessage); message WM_MEDIA_PLAYER_EVENTS;

    property State: TVlcPlayerState read GetState;
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCampaignVideo(aCampaignPath: string; aVideoName: String);
    procedure AddMissionVideo(aMissionFile: string; aVideoName: String);
    procedure AddVideo(AVideoName: String);
    procedure Play;
    procedure Stop;
    procedure Pause;
    procedure Resume;

    procedure Resize(aWidth, aHeight: Integer);
    procedure Paint;

    procedure KeyDown(Key: Word; Shift: TShiftState);
    procedure KeyPress(Key: Char);
    procedure KeyUp(Key: Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseWheel(Shift: TShiftState; WheelSteps: Integer; X,Y: Integer);

    function IsActive: Boolean;
    function IsPlay: Boolean;
    function IsPause: Boolean;
  end;

var
  gVideoPlayer: TKMVideoPlayer;

implementation

{$IFDEF VIDEOS}

uses
  KM_Render, KM_RenderUI, dglOpenGL, KM_Controls, KM_ResFonts, KM_ResLocales, KM_GameApp;

procedure lib_vlc_player_event_hdlr(p_event: libvlc_event_t_ptr; data: Pointer); cdecl; forward;

function libvlc_lock(opaque: Pointer; var planes: Pointer) : Pointer; cdecl;
begin
  gVideoPlayer.FCriticalSection.Enter;
  if Length(gVideoPlayer.FBuffer) > 0 then
    planes := @(gVideoPlayer.FBuffer[0]);
  Result := nil;
end;

function libvlc_unlock(opaque: Pointer; picture: Pointer; planes: Pointer) : Pointer; cdecl;
begin
  gVideoPlayer.FCriticalSection.Leave;
  Result := nil;
end;

function libvlc_display(opaque: Pointer; picture: Pointer) : Pointer; cdecl;
begin
  Result := nil;
end;

procedure lib_vlc_player_event_hdlr(p_event: libvlc_event_t_ptr; data: Pointer); cdecl;
var
  player: TKMVideoPlayer;
  msg: TVlcMessage;
begin
  if (data = nil) then
    Exit;

  player := TKMVideoPlayer(data);
  if not Assigned(player) then
    Exit;

  msg.Msg := WM_MEDIA_PLAYER_EVENTS;
  msg.EventType := p_event^.event_type;

  case p_event^.event_type of
    libvlc_MediaPlayerTimeChanged: msg.Data := p_event^.media_player_time_changed.new_time;
    libvlc_MediaPlayerLengthChanged: msg.Data := p_event^.media_player_length_changed.new_length;
  end;

  player.Dispatch(msg);
end;

{$ENDIF}

{ TKMVideoPlayer }

constructor TKMVideoPlayer.Create;
begin
{$IFDEF VIDEOS}
  FCriticalSection := TCriticalSection.Create;
  FVideoList := TStringList.Create;
  FTrackList :=  TStringList.Create;
{$ENDIF}
end;

destructor TKMVideoPlayer.Destroy;
begin
{$IFDEF VIDEOS}
  FNext := False;
  FVideoList.Free;
  FTrackList.Free;

  DestroyMediaPlayer;

  if Assigned(libvlc_release) and (FInstance <> nil) then
  begin
    libvlc_release(FInstance);
    FInstance := nil;
  end;
{$ENDIF}

  FCriticalSection.Free;

  inherited;
end;


procedure TKMVideoPlayer.AddCampaignVideo(aCampaignPath: string; aVideoName: String);
var
  MissionPath, FileName: string;
  Path: string;
begin
{$IFDEF VIDEOS}
  if not gGameApp.GameSettings.VideoOn then
    Exit;

  if TryGetPathFile(aCampaignPath + aVideoName, Path) or
    TryGetPathFile(VIDEOFILE_PATH + aVideoName, Path) then
    FVideoList.Add(Path);
{$ENDIF}
end;

procedure TKMVideoPlayer.AddMissionVideo(aMissionFile: string; aVideoName: String);
var
  MissionPath, FileName: string;
  Path: string;
begin
{$IFDEF VIDEOS}
  if not gGameApp.GameSettings.VideoOn then
    Exit;
  MissionPath := ExtractFilePath(aMissionFile);
  FileName := ExtractFileName(ChangeFileExt(aMissionFile, '')) + '.' + aVideoName;

  if TryGetPathFile(MissionPath + FileName, Path) or
    TryGetPathFile(MissionPath + aVideoName, Path) or
    TryGetPathFile(VIDEOFILE_PATH + aVideoName, Path) then
    FVideoList.Add(Path);
{$ENDIF}
end;

procedure TKMVideoPlayer.AddVideo(aVideoName: String);
var
  Path: string;
begin
{$IFDEF VIDEOS}
  if not gGameApp.GameSettings.VideoOn then
    Exit;
  if TryGetPathFile(aVideoName, Path) or
    TryGetPathFile(VIDEOFILE_PATH + aVideoName, Path) then
    FVideoList.Add(Path);
{$ENDIF}
end;

procedure TKMVideoPlayer.Play;
begin
{$IFDEF VIDEOS}
  FIndex := 0;
  DoStop;
  if gGameApp.GameSettings.VideoOn and (FVideoList.Count > 0) then
    PlayNext;
{$ENDIF}
end;

procedure TKMVideoPlayer.Stop;
begin
{$IFDEF VIDEOS}
  DoStop;
  FVideoList.Clear;
  FTrackList.Clear;
{$ENDIF}
end;

procedure TKMVideoPlayer.Pause;
begin
{$IFDEF VIDEOS}
  if (FMediaPlayer <> nil) and (State = plvPlayer_Playing) then
    libvlc_media_player_pause(FMediaPlayer);
{$ENDIF}
end;

procedure TKMVideoPlayer.Resume;
begin
{$IFDEF VIDEOS}
  if (FMediaPlayer <> nil) and (State = plvPlayer_Paused) then
    libvlc_media_player_play(FMediaPlayer);
{$ENDIF}
end;

procedure TKMVideoPlayer.Resize(aWidth, aHeight: Integer);
begin
{$IFDEF VIDEOS}
  FScreenWidth := aWidth;
  FScreenHeight := aHeight;
{$ENDIF}
end;

procedure TKMVideoPlayer.Paint;
var
  i: Integer;
  AspectRatio: Single;
  Width, Height: Integer;
begin
{$IFDEF VIDEOS}
  if FNext then
  begin
    FNext := False;
    PlayNext;
    Exit;
  end;

  if IsPlay and (Length(FBuffer) > 0) and (FTexture.Tex > 0)  then
  begin
    FCriticalSection.Enter;
    glBindTexture(GL_TEXTURE_2D, FTexture.Tex);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, FWidth, FHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, FBuffer);
    glBindTexture(GL_TEXTURE_2D, 0);

    if gGameApp.GameSettings.VideoStretch then
    begin
      AspectRatio := FWidth / FHeight;
      if AspectRatio > FScreenWidth / FScreenHeight then
      begin
        Width := FScreenWidth;
        Height := Round(FScreenWidth / AspectRatio);
      end
      else
      begin
        Width := Round(FScreenHeight * AspectRatio);
        Height := FScreenHeight;
      end;
    end
    else
    begin
      Width := FWidth;
      Height := FHeight;
    end;

    TKMRenderUI.WriteTexture((FScreenWidth - Width) div 2, (FScreenHeight - Height) div 2, Width, Height, FTexture, $FFFFFFFF);
    FCriticalSection.Leave;
  end;
  {
  if IsPlay then
    TKMRenderUI.WriteText(100, 50, 1000, 'Play', fntArial, taLeft)
  else
    TKMRenderUI.WriteText(100, 50, 1000, 'Pause', fntArial, taLeft);

  TKMRenderUI.WriteText(200, 50, 1000, 'Index = ' + IntToStr(FIndex), fntArial, taLeft);
  TKMRenderUI.WriteText(350, 50, 1000, 'Size = ' + IntToStr(FWidth) + 'x' + IntToStr(FHeight), fntArial, taLeft);

  for i := 0 to FVideoList.Count - 1 do
  begin
    if i < FIndex then
      TKMRenderUI.WriteText(100, 100 + i * 20 + 20, 1000, FVideoList[i] + ' - Ok', fntArial, taLeft)
    else if i = FIndex then
      TKMRenderUI.WriteText(100, 100 + i * 20 + 20, 1000, FVideoList[i] + ' - ' + IntToStr(FTime) + ' / ' + IntToStr(FLenght), fntArial, taLeft)
    else
      TKMRenderUI.WriteText(100, 100 + i * 20 + 20, 1000, FVideoList[i], fntArial, taLeft)
  end;
  }
{$ENDIF}
end;

procedure TKMVideoPlayer.KeyDown(Key: Word; Shift: TShiftState);
begin
{$IFDEF VIDEOS}
  if not IsPlay then
    Exit;

  //  Space         Esc           Enter
  if (Key = 27) or (Key = 32) or (Key = 13) then
    PlayNext;

  {
  if Key = 32 then // Space
  begin
    if IsPlay then
      Pause
    else
      Resume;
  end;
  }
{$ENDIF}
end;

procedure TKMVideoPlayer.KeyPress(Key: Char);
begin
{$IFDEF VIDEOS}

{$ENDIF}
end;
procedure TKMVideoPlayer.KeyUp(Key: Word; Shift: TShiftState);
begin
{$IFDEF VIDEOS}

{$ENDIF}
end;
procedure TKMVideoPlayer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
{$IFDEF VIDEOS}
  if not IsPlay then
    Exit;

  PlayNext;
{$ENDIF}
end;
procedure TKMVideoPlayer.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
{$IFDEF VIDEOS}

{$ENDIF}
end;
procedure TKMVideoPlayer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
{$IFDEF VIDEOS}

{$ENDIF}
end;
procedure TKMVideoPlayer.MouseWheel(Shift: TShiftState; WheelSteps: Integer; X,Y: Integer);
begin
{$IFDEF VIDEOS}

{$ENDIF}
end;

function TKMVideoPlayer.IsActive: Boolean;
begin
{$IFDEF VIDEOS}
  Result := IsPlay or (FVideoList.Count > 0);
{$else}
  Result := False;
{$ENDIF}
end;

function TKMVideoPlayer.IsPlay: Boolean;
begin
{$IFDEF VIDEOS}
  Result := State = plvPlayer_Playing;
{$else}
  Result := False;
{$ENDIF}
end;

function TKMVideoPlayer.IsPause: Boolean;
begin
{$IFDEF VIDEOS}
  Result := State = plvPlayer_Paused;
{$else}
  Result := False;
{$ENDIF}
end;

{$IFDEF VIDEOS}

procedure TKMVideoPlayer.DestroyMediaPlayer;
begin
  EventsDisable;

  if (FMediaPlayer = nil) then
    Exit;

  Stop;
  libvlc_media_player_release(FMediaPlayer);
  FMediaPlayer := nil;
  Sleep(50);
end;

procedure TKMVideoPlayer.MediaPlayerEvents(var Msg: TVlcMessage);
begin
  case Msg.EventType of
    libvlc_MediaPlayerEndReached: FNext := True;
    libvlc_MediaPlayerTimeChanged: FTime := Msg.Data;
    libvlc_MediaPlayerLengthChanged: FLenght := Msg.Data;
  end;
  Msg.Result := 0;
end;


procedure TKMVideoPlayer.InitInstance;
begin
  if FInstance <> nil then
    Exit;

  libvlc_dynamic_dll_init_with_path(extractfilepath(paramstr(0)) + VLC_PATH);
  if (libvlc_dynamic_dll_error <> '') then
    libvlc_dynamic_dll_init();

  if (libvlc_dynamic_dll_error <> '') then
    Exit;

  with TArgcArgs.Create([libvlc_dynamic_dll_path, '--no-xlib', '-q', '--no-video-title-show', '--quiet', '--ignore-config', '--vout', 'vmem', '-I', 'dumy']) do
  begin
    FInstance := libvlc_new(ARGC, ARGS);
    Free;
  end;
end;

procedure TKMVideoPlayer.InitMediaPlayer;
begin
  if FMediaPlayer = nil then
  begin
    InitInstance;
    if FInstance <> nil then
      FMediaPlayer := libvlc_media_player_new(FInstance);
    EventsEnable;
  end;
end;

procedure TKMVideoPlayer.DoPlay(AVideoName: array of string; AEndVideo: TKMVideoPlayerEvent = nil);
var
  fileName: string;
begin
  FIndex := 0;
  DoStop;
  FVideoList.Clear;
  if Length(AVideoName) = 0 then
    Exit;

  for fileName in AVideoName do
    if FileExists(fileName) then
      FVideoList.Add(fileName);

  if FVideoList.Count > 0 then
    PlayNext;
end;

procedure TKMVideoPlayer.DoStop;
const
  TIME_STEP = 50;
var
  timeElapsed : Cardinal;
begin
{$IFDEF VIDEOS}
  Pause;
  if IsPlay then
  begin
    libvlc_media_player_stop(FMediaPlayer);
    Sleep(TIME_STEP);
    timeElapsed := TIME_STEP;
    while IsPlay do
    begin
      if (timeElapsed > 1000) then
        Break;
      Sleep(TIME_STEP);
      Inc(timeElapsed, TIME_STEP);
    end;
  end;
  FCriticalSection.Enter;
  if FTexture.Tex > 0 then
  begin
    TRender.DeleteTexture(FTexture.Tex);
    FTexture.Tex := 0;
  end;
  SetLength(FBuffer, 0);
  FCriticalSection.Leave;
{$ENDIF}
end;

procedure TKMVideoPlayer.Finish;
begin
  FVideoList.Clear;
end;

procedure TKMVideoPlayer.PlayNext;
var
  i: Integer;
  path: string;
  media: libvlc_media_t_ptr;

  trackCount: Cardinal;
  tracks: libvlc_media_track_list_t;
  track: libvlc_media_track_t_ptr;
begin
  InitMediaPlayer;
  if (FInstance = nil) or (FMediaPlayer = nil) then
    Exit;

  if FIndex >= FVideoList.Count then
  begin
    Stop;
    Exit;
  end;

  DoStop;

  media := libvlc_media_new_path(FInstance, PAnsiChar(UTF8Encode(FVideoList[FIndex])));
  Inc(FIndex);
  libvlc_media_parse(media);

  FTrackList.Clear;
  trackCount := libvlc_media_tracks_get(media, Pointer(tracks));

  if trackCount > 0 then
  begin
    for i := 0 to trackCount - 1 do
    begin
      track := tracks[i];
      case track.i_type of
        libvlc_track_video:
          begin
            FWidth := track.u.video.i_width;
            FHeight := track.u.video.i_height;
          end;
        libvlc_track_audio:
          if track.psz_language <> nil then
            FTrackList.AddObject(UpperCase(track.psz_language), TObject(track.i_id));
      end;
    end;

    FCriticalSection.Enter;
    SetLength(FBuffer, FWidth * FHeight * 3);

    FTexture.Tex := TRender.GenerateTextureCommon;
    FTexture.U := 1;
    FTexture.V := 1;
    FCriticalSection.Leave;

    libvlc_media_tracks_release(tracks, trackCount);

    libvlc_video_set_format(FMediaPlayer, 'RV24', FWidth, FHeight, FWidth * 3);
    libvlc_media_player_set_media(FMediaPlayer, media);
    libvlc_media_player_play(FMediaPlayer);
    SetTrackByLocale;
    libvlc_audio_set_volume(FMediaPlayer, Round(gGameApp.GameSettings.VideoVolume * 100));
  end;

  libvlc_media_release(media);
end;

function TKMVideoPlayer.TryGetPathFile(aPath: string; var aFileName: string): Boolean;
var
  i: Integer;
  SearchRec: TSearchRec;
  FileName, Path, f: string;
  LocalePostfixes: TStringList;
begin
  Result := False;
  aFileName := '';

  Path := ExtractFilePath(aPath);
  if not DirectoryExists(ExtractFilePath(ParamStr(0)) + Path) then
    Exit;

  LocalePostfixes := TStringList.Create;
  LocalePostfixes.Add('.' + UnicodeString(gResLocales.UserLocale));
  LocalePostfixes.Add('.' + UnicodeString(gResLocales.FallbackLocale));
  LocalePostfixes.Add('.' + UnicodeString(gResLocales.DefaultLocale));
  LocalePostfixes.Add('');

  FileName := ExtractFileName(aPath);
  for i := 0 to LocalePostfixes.Count - 1 do
  begin
    try
      if FindFirst(Path + '*', faAnyFile, SearchRec) <> 0 then
        Continue;

      repeat
        if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
          Continue;

        f := FileName + LocalePostfixes[i] + ExtractFileExt(SearchRec.Name);
        if CompareStr(SearchRec.Name, f) = 0 then
        begin
          aFileName := ExtractFilePath(ParamStr(0)) + Path + SearchRec.Name;
          Exit(True);
        end;

      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
  LocalePostfixes.Free;
end;

procedure TKMVideoPlayer.SetTrackByLocale;
const
  TIME_STEP = 50;
var
  id, index: Integer;
  timeElapsed : Cardinal;
begin
  if FTrackList.Count = 0 then
    Exit;

  if not FTrackList.Find(UpperCase(gResLocales.UserLocale), index) and
    not FTrackList.Find(UpperCase(gResLocales.FallbackLocale), index) and
    not FTrackList.Find(UpperCase(gResLocales.DefaultLocale), index) then
    Exit;

  id := Integer(FTrackList.Objects[index]);

  Sleep(TIME_STEP);
  timeElapsed := TIME_STEP;
  while (FMediaPlayer <> nil) and (timeElapsed < 1000) and (libvlc_audio_set_track(FMediaPlayer, id) < 0) do
  begin
    Sleep(TIME_STEP);
    Inc(timeElapsed, TIME_STEP);
  end;
end;

function TKMVideoPlayer.GetState: TVlcPlayerState;
begin
  Result := plvPlayer_NothingSpecial;
  if (FInstance = nil) or (FMediaPlayer = nil) then
    Exit;

  case libvlc_media_player_get_state(FMediaPlayer) of
    libvlc_NothingSpecial: Result := plvPlayer_NothingSpecial;
    libvlc_Opening:        Result := plvPlayer_Opening;
    libvlc_Buffering:      Result := plvPlayer_Buffering;
    libvlc_Playing:        Result := plvPlayer_Playing;
    libvlc_Paused:         Result := plvPlayer_Paused;
    libvlc_Stopped:        Result := plvPlayer_Stopped;
    libvlc_Ended:          Result := plvPlayer_Ended;
    libvlc_Error:          Result := plvPlayer_Error;
  end;
end;

procedure TKMVideoPlayer.EventsEnable;
begin
  EventsDisable;

  if FMediaPlayer = nil then
    Exit;

  libvlc_video_set_callbacks(FMediaPlayer, @libvlc_lock, @libvlc_unlock, nil, nil);
  FEvents := libvlc_media_player_event_manager(FMediaPlayer);

  if not Assigned(FEvents) then
    Exit;

  libvlc_event_attach(FEvents, libvlc_MediaPlayerEndReached, lib_vlc_player_event_hdlr, Self);
  libvlc_event_attach(FEvents, libvlc_MediaPlayerTimeChanged, lib_vlc_player_event_hdlr, Self);
  libvlc_event_attach(FEvents, libvlc_MediaPlayerLengthChanged, lib_vlc_player_event_hdlr, Self);
end;

procedure TKMVideoPlayer.EventsDisable;
begin
  if not Assigned(FEvents) then
    Exit;

  libvlc_event_detach(FEvents, libvlc_MediaPlayerEndReached, lib_vlc_player_event_hdlr, Self);
  libvlc_event_detach(FEvents, libvlc_MediaPlayerTimeChanged, lib_vlc_player_event_hdlr, Self);
  libvlc_event_detach(FEvents, libvlc_MediaPlayerLengthChanged, lib_vlc_player_event_hdlr, Self);

  FEvents := nil;

  Sleep(50);
end;
{$ENDIF}

end.
