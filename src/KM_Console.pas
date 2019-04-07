unit KM_Console;
interface
uses
  Generics.Collections,
  KM_CommonTypes, KM_NetworkTypes;



type
  TKMChatMode = (cmAll, cmTeam, cmSpectators, cmWhisper);

  TKMConsole = class
    private
      fLastConsoleTime: Cardinal;
      fHistory: TList<String>;
      fCurrConsoleHistoryId: Integer;
      fOnPostMsg: TUnicodeStringEvent;
      fOnError: TUnicodeStringEvent;
      fMessages: String;

      procedure SetMessages(aMessages: String);

      function TryDoCallConsoleCommand: Boolean;
    public

      Text: UnicodeString;

      constructor Create;
      destructor Destroy; override;

      property Messages: UnicodeString read fMessages write SetMessages;

      property OnPostMsg: TUnicodeStringEvent read fOnPostMsg write fOnPostMsg;
      property OnError: TUnicodeStringEvent read fOnError write fOnError;
      function DoPost: Boolean;
      function IsPostAllowed: Boolean;
      function TryCallConsoleCommand: Boolean;

      function GetNextHistoryMsg: String;
      function GetPrevHistoryMsg: String;
      procedure Clear; virtual;
    end;


  TKMChat = class(TKMConsole)
  private
    fMode: TKMChatMode;
    procedure SetMode(aMode: TKMChatMode);
  public
    WhisperRecipient: TKMNetHandleIndex;

    constructor Create;

    property Mode: TKMChatMode read fMode write SetMode;

    procedure Clear; override;
  end;

const
  CHAT_COOLDOWN = 500;  //Minimum time in milliseconds between chat messages
  CHAT_TAG: array[TKMChatMode] of Integer = (
    -1,  //cmAll
    -2,  //cmTeam
    -3,  //cmSpectators
    -1); //cmWhisper

  
implementation
uses
  Math, SysUtils, StrUtils,
  KM_ResTexts,
  KM_ScriptingEvents, KM_ScriptingConsoleCommands,
  KM_Game, KM_GameInputProcess,
  KM_CommonUtils, KM_Defaults;

const
  DEF_CURR_CONSOLE_HISTORY_ID = -1;


{ TKMConsole }
constructor TKMConsole.Create;
begin
  inherited;

  fHistory := TList<String>.Create;

  Clear;
end;


destructor TKMConsole.Destroy;
begin
  FreeAndNil(fHistory);

  inherited;
end;


procedure TKMConsole.Clear;
begin
  fLastConsoleTime := 0;
  fHistory.Clear;
  fCurrConsoleHistoryId := DEF_CURR_CONSOLE_HISTORY_ID;
  Messages := '';
  Text := '';
end;


procedure TKMConsole.SetMessages(aMessages: String);
begin
  fMessages := aMessages;
end;


function TKMConsole.GetNextHistoryMsg: String;
begin
  if fHistory.Count = 0 then
  begin
    Result := '';
    Exit;
  end;

  fCurrConsoleHistoryId := Min(fCurrConsoleHistoryId + 1, fHistory.Count - 1);
  Result := fHistory[fCurrConsoleHistoryId];
end;


function TKMConsole.GetPrevHistoryMsg: String;
begin
  if fHistory.Count = 0 then
  begin
    Result := '';
    Exit;
  end;

  fCurrConsoleHistoryId := Max(0, fCurrConsoleHistoryId - 1);
  Result := fHistory[fCurrConsoleHistoryId];
end;


function TKMConsole.IsPostAllowed : Boolean;
begin
  Result := (Trim(Text) <> '') and (GetTimeSince(fLastConsoleTime) >= CHAT_COOLDOWN)
end;


function TKMConsole.TryCallConsoleCommand: Boolean;
begin
  Result := False;
  if gGame = nil then //Can't manage console commands while not in the game
    Exit;

  if not gGame.IsMultiplayer then
    Exit;

  if (Length(Text) > 0) and (Text[1] = '/') and (Text[2] <> '/') then
    Result := TryDoCallConsoleCommand
  else
  if (Length(Text) > 1) and (Text[1] = '/') and (Text[2] = '/') then
    Delete(Text, 1, 1); //Remove one of the /'s
end;


function TKMConsole.TryDoCallConsoleCommand: Boolean;
var
  I, ParamsI, SpacePos: Integer;
  CmdName: AnsiString;
  ParamsStr, Param: UnicodeString;
  Params: TKMScriptCommandParamsArray;
  QuoteStart: Boolean;

  procedure AddParam(aParam: UnicodeString);
  begin
    if aParam <> '' then
    begin
      Params[ParamsI] := aParam;
      Inc(ParamsI);
    end;
  end;

begin
  Result := False;
  SpacePos := Pos(' ', Text);
  if SpacePos = 0 then
    SpacePos := Length(Text) + 1;

  CmdName := AnsiString(Copy(Text, 2, SpacePos - 2));

  if not gScriptEvents.HasConsoleCommand(CmdName)
    and Assigned(fOnError) then
  begin
    fOnError(Format(gResTexts[TX_SCRIPT_CONSOLE_CMD_NOT_FOUND], [WrapColorA(CmdName, clScriptCmdName)]));
    Exit;
  end;

  ParamsStr := RightStr(Text, Length(Text) - (SpacePos - 1));

  ParamsI := 0;
  Param := '';
  QuoteStart := False;
  ParamsStr := StringReplace(ParamsStr, '\''', #1, [rfReplaceAll]);

  for I := 1 to Length(ParamsStr) do
  begin
    if (ParamsStr[I] = ' ') and not QuoteStart then
    begin
      AddParam(Param);
      Param := '';
    end
    else
    if ParamsStr[I] = '''' then
      QuoteStart := not QuoteStart
    else
      Param := Param + ParamsStr[I];
  end;

  AddParam(Param);

  for I := 0 to ParamsI - 1 do
    Params[I] := StringReplace(Params[I], #1, '''', [rfReplaceAll]);

  for I := ParamsI to MAX_SCRIPT_CONSOLE_COMMAND_PARAMS - 1 do
    Params[I] := '';

  if not gScriptEvents.ConsoleCommand[CmdName].ValidateParams(Params)
    and Assigned(fOnError) then
    fOnError(Format(gResTexts[TX_SCRIPT_CONSOLE_CMD_PARAMS_NOT_VALID],
                    [WrapColorA(CmdName, clScriptCmdName),
                     gScriptEvents.ConsoleCommand[CmdName].ParamsTypes2String(True)]))
  else
  begin
    gGame.GameInputProcess.CmdConsoleCommand(gicScriptConsoleCommand, CmdName, Params);
    Result := True;
  end;
end;


function TKMConsole.DoPost: Boolean;
begin
  fLastConsoleTime := TimeGet;

  fHistory.Insert(0, Text);
  fCurrConsoleHistoryId := -1;

  if Assigned(fOnPostMsg) then
    fOnPostMsg(Text);
end;


{ TKMChat }
constructor TKMChat.Create;
begin
  inherited;

  Clear;
end;


procedure TKMChat.Clear;
begin
  inherited;

  WhisperRecipient := -1000;
  fMode := cmAll;
end;


procedure TKMChat.SetMode(aMode: TKMChatMode);
begin
  fMode := aMode;
end;


end.
