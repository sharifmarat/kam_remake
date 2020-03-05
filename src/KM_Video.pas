unit KM_Video;

{$I KaM_Remake.inc}

interface

uses
  Vcl.Forms, SysUtils, SyncObjs, Types, Messages, Classes, Dialogs, KromOGLUtils
{$IFDEF VIDEOS}
  , PasLibVlcUnit
{$ENDIF}
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

    FNext: Boolean;

    FIndex: Integer;
    FLenght: Int64;
    FTime: Int64;

    FInstance: libvlc_instance_t_ptr;
    FMediaPlayer: libvlc_media_player_t_ptr;
    FEvents: libvlc_event_manager_t_ptr;

    FVideoList: TStringList;
    FEndVideo: TKMVideoPlayerEvent;

    procedure InitInstance;
    procedure InitMediaPlayer;
    procedure DestroyMediaPlayer;
    procedure EventsEnable();
    procedure EventsDisable();
    procedure PlayNext;

    function GetState: TVlcPlayerState;
    procedure MediaPlayerEvents(var Msg: TVlcMessage); message WM_MEDIA_PLAYER_EVENTS;

    property State: TVlcPlayerState read GetState;
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Play(AVideoName: array of String; AEndVideo: TKMVideoPlayerEvent = nil); overload;
    procedure Play(AVideoName: String; AEndVideo: TKMVideoPlayerEvent = nil); overload;
    procedure Stop;
    procedure Pause;
    procedure Resume;

    procedure Resize;
    procedure Paint;

    procedure KeyDown(Key: Word; Shift: TShiftState);

    function IsActive: Boolean;
    function IsPlay: Boolean;
    function IsPause: Boolean;
  end;

var
  gVideoPlayer: TKMVideoPlayer;

implementation

{$IFDEF VIDEOS}

uses
  KM_Render, KM_RenderUI, dglOpenGL, KM_Controls, KM_ResFonts;

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
{$ENDIF}
end;

destructor TKMVideoPlayer.Destroy;
begin
{$IFDEF VIDEOS}
  FNext := False;
  FVideoList.Free;
  FCriticalSection.Free;

  DestroyMediaPlayer;

  if Assigned(libvlc_release) and (FInstance <> nil) then
  begin
    libvlc_release(FInstance);
    FInstance := nil;
  end;
{$ENDIF}

  inherited;
end;

procedure TKMVideoPlayer.Play(AVideoName: String; AEndVideo: TKMVideoPlayerEvent = nil);
begin
  Play([AVideoName], AEndVideo);
end;

procedure TKMVideoPlayer.Play(AVideoName: array of string; AEndVideo: TKMVideoPlayerEvent = nil);
var
  fileName: string;
begin
{$IFDEF VIDEOS}
  FIndex := -1;
  Stop;
  FVideoList.Clear;
  if Length(AVideoName) = 0 then
    Exit;

  for fileName in AVideoName do
    FVideoList.Add(ExtractFilePath(ParamStr(0)) + VIDEOFILE_PATH + fileName);

  PlayNext;
{$ENDIF}
end;

procedure TKMVideoPlayer.Stop;
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
  SetLength(FBuffer, 0);
  FCriticalSection.Leave;
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

procedure TKMVideoPlayer.Resize;
var
  Width, Height: Integer;
  AspectRatio: Single;
begin
{$IFDEF VIDEOS}
  if not Assigned(FMediaPlayer) then
    Exit;

  AspectRatio := Width / Height;
  {
  if AspectRatio > FParentForm.ClientWidth / FParentForm.ClientHeight then
  begin
    FPanel.Width := FParentForm.ClientWidth;
    FPanel.Height := Round(FParentForm.ClientWidth / AspectRatio);
  end
  else
  begin
    FPanel.Width := Round(FParentForm.ClientHeight * AspectRatio);
    FPanel.Height := FParentForm.ClientHeight;
  end;

  FPanel.Left := FParentForm.ClientWidth div 2 - FPanel.Width div 2;
  FPanel.Top := FParentForm.ClientHeight div 2 - FPanel.Height div 2;
  }
  //FMediaPlayer.DisplayRect := Rect(0, 0, FPanel.Width, FPanel.Height);
{$ENDIF}
end;

procedure TKMVideoPlayer.Paint;
var
  i: Integer;
begin
{$IFDEF VIDEOS}
  if FNext then
  begin
    FNext := False;
    PlayNext;
    Exit;
  end;

  if Length(FBuffer) > 0 then
    glDrawPixels(FWidth, FHeight, GL_RGB, GL_UNSIGNED_BYTE, FBuffer);

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
{$ENDIF}
end;

procedure TKMVideoPlayer.KeyDown(Key: Word; Shift: TShiftState);
begin
{$IFDEF VIDEOS}
  if Key = 27 then
    PlayNext;

  if Key = 32 then
  begin
    if IsPlay then
      Pause
    else
      Resume;
  end;
{$ENDIF}
end;

function TKMVideoPlayer.IsActive: Boolean;
begin
{$IFDEF VIDEOS}
  Result := IsPlay or (FIndex < FVideoList.Count);
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

  //with TArgcArgs.Create([libvlc_dynamic_dll_path, '--ignore-config', '--intf=dummy', '--quiet']) do
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

procedure TKMVideoPlayer.PlayNext;
var
  path: string;
  media: libvlc_media_t_ptr;
  info: libvlc_media_track_info_t_ptr;
begin
  InitMediaPlayer;
  if (FInstance = nil) or (FMediaPlayer = nil) then
    Exit;

  Stop;

  repeat
    Inc(FIndex);
    if (FVideoList.Count = 0) or (FIndex >= FVideoList.Count) then
      Exit;

    path := FVideoList[FIndex];
  until FileExists(path);

  media := libvlc_media_new_path(FInstance, PAnsiChar(UTF8Encode(path)));
  libvlc_media_parse(media);

  if libvlc_media_get_tracks_info(media, info) > 0 then
  begin
    FWidth := info.video.i_width;
    FHeight := info.video.i_height;
    FCriticalSection.Enter;
    SetLength(FBuffer, FWidth * FHeight * 3);
    FCriticalSection.Leave;
    libvlc_video_set_format(FMediaPlayer, 'RV24', FWidth, FHeight, FWidth * 3);
    libvlc_media_player_set_media(FMediaPlayer, media);
    libvlc_media_player_play(FMediaPlayer);
  end;

  libvlc_media_release(media);
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
