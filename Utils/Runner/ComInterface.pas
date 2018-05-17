unit ComInterface;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface
uses
  Classes, Windows, Sysutils, GeneticAlgorithm;


var
  PARALLEL_RUN: Boolean = False;


type
  TGASetup = record
    MapCnt: Integer;
    Population: TGAPopulation;
  end;
  TRunnerSetup = record
    RunningClass: String;
    SimTimeInMin, SimNumber: Integer;   //TGAPopulation
  end;
  TManagerSetup = record
    SimFile, WorkDir, RunningClass: WideString;
    SimTimeInMin, SimNumber: Integer;
  end;

  TKMComInterface = class // Communication Interface
  private
    procedure DecryptSetup(aParams: WideString; var aRSetup: TRunnerSetup; var aGASetup: TGASetup);
    function EncryptSetup(var aMSetup: TRunnerSetup; var aGASetup: TGASetup): WideString;
    procedure LogInConsole(aString: String);
    function CaptureConsoleOutput(const aWorkDir, aCommand: WideString): WideString;
    procedure DebugLogString(aString: WideString; aFileName: String);
  public
    procedure SetupSimulation(var aRSetup: TRunnerSetup; var aGASetup: TGASetup);
    procedure LogSimulationResults(var aRSetup: TRunnerSetup; var aGASetup: TGASetup);
    procedure CreateNewSimulation(var aMSetup: TManagerSetup; var aGASetup: TGASetup);
  end;

const
  COMMAND_START = '{';
  COMMAND_END = '}';
  INT_START = '(';
  INT_END = ')';
  FLOAT_START = '[';
  FLOAT_END = ']';
  STRING_START = '<';
  STRING_END = '>';
  CMND_RunningClass = 0;
  CMND_SimTimeInMin = 1;
  CMND_SimNumber = 2;

  CMND_GA_MapCnt = 4;
  CMND_GA_IdvCnt = 5;
  CMND_GA_GenCnt = 3;
  CMND_GA_Fit = 7;
  CMND_GA_Genes = 8;
  CMND_GA_DONE = 9;

implementation


procedure TKMComInterface.DecryptSetup(aParams: WideString; var aRSetup: TRunnerSetup; var aGASetup: TGASetup);
var
  Str: WideString;
  Command: Byte;
  Int, GA_IdvCnt, GA_GenCnt, GA_IdvIdx, GA_GenIdx: Integer;
  Flt, GA_Fit: Single;

  function GetCommand(var aIdx: Integer): Byte;
  var
    initIdx: Integer;
  begin
    Result := 0;
    initIdx := aIdx+1;
    while (aIdx < Length(aParams)) AND (aParams[aIdx] <> COMMAND_END) do
      aIdx := aIdx + 1;
    Try
      Result := Byte(  StrToInt( copy(aParams, initIdx, aIdx-initIdx) )  );
    except
      //On E : EConvertError do
      //  Writeln ('Invalid number encountered');
    end;
  end;
  function GetString(var aIdx: Integer): WideString;
  var
    initIdx: Integer;
  begin
    initIdx := aIdx+1;
    while (aIdx < Length(aParams)) AND (aParams[aIdx] <> STRING_END) do
      aIdx := aIdx + 1;
    Result := copy(aParams, initIdx, aIdx-initIdx);
  end;
  function GetInt(var aIdx: Integer): Integer;
  var
    initIdx: Integer;
  begin
    Result := 0;
    initIdx := aIdx+1;
    while (aIdx < Length(aParams)) AND (aParams[aIdx] <> INT_END) do
      aIdx := aIdx + 1;
    Try
      Result := StrToInt( copy(aParams, initIdx, aIdx-initIdx) );
    except
      //On E : EConvertError do
      //  Writeln ('Invalid number encountered');
    end;
  end;
  function GetFloat(var aIdx: Integer): Single;
  var
    initIdx: Integer;
  begin
    Result := 0;
    initIdx := aIdx+1;
    while (aIdx < Length(aParams)) AND (aParams[aIdx] <> FLOAT_END) do
      aIdx := aIdx + 1;
    Try
      Result := StrToFloat( copy(aParams, initIdx, aIdx-initIdx) );
    except
      //On E : EConvertError do
      //  Writeln ('Invalid number encountered');
    end;
  end;

  procedure AddGene();
  begin
    if (aGASetup.Population = nil) then
    begin
      aGASetup.Population := TGAPopulation.Create(GA_IdvCnt, GA_GenCnt, True);
      GA_IdvIdx := -1;
      GA_GenIdx := GA_GenCnt;
    end;
    if (GA_GenIdx >= GA_GenCnt) then
    begin
      GA_IdvIdx := GA_IdvIdx + 1;
      GA_GenIdx := 0;
      aGASetup.Population.Individual[ GA_IdvIdx ].Fitness := GA_Fit;
    end;
    aGASetup.Population.Individual[ GA_IdvIdx ].Gene[ GA_GenIdx ] := Flt;
    GA_GenIdx := GA_GenIdx + 1;
  end;

  procedure SaveInformation(aIdx: Integer);
  begin
    case Command of
      CMND_RunningClass: aRSetup.RunningClass := Str;
      CMND_SimTimeInMin: aRSetup.SimTimeInMin := Int;
      CMND_SimNumber: aRSetup.SimNumber := Int;
      CMND_GA_MapCnt: aGASetup.MapCnt := Int;
      CMND_GA_IdvCnt: GA_IdvCnt := Int;
      CMND_GA_GenCnt: GA_GenCnt := Int;
      CMND_GA_Fit: GA_Fit := Flt;
      CMND_GA_Genes: AddGene();
    end;
  end;
var
  I: Integer;
begin
  GA_IdvCnt := 0;
  GA_GenCnt := 0;
  aGASetup.Population := nil;

  I := 1;
  while (I < Length(aParams)) do // Strings starts with 1
  begin
    case aParams[I] of
      COMMAND_START: Command := GetCommand(I);
      STRING_START:
      begin
        Str := GetString(I);
        SaveInformation(I);
      end;
      INT_START:
      begin
        Int := GetInt(I);
        SaveInformation(I);
      end;
      FLOAT_START:
      begin
        Flt := GetFloat(I);
        SaveInformation(I);
      end;
      else begin end;
    end;
    I := I + 1;
  end;
end;


function TKMComInterface.EncryptSetup(var aMSetup: TRunnerSetup; var aGASetup: TGASetup): WideString;
  function CreateCommand(aCommand: Integer): WideString; inline;
  begin
    Result := COMMAND_START + IntToStr(aCommand) + COMMAND_END;
  end;
  function CreateString(aString: WideString): WideString; inline;
  begin
    Result := STRING_START + aString + STRING_END;
  end;
  function CreateInt(aInt: Integer): WideString; inline;
  begin
    Result := INT_START + IntToStr(aInt) + INT_END;
  end;
  function CreateFloat(aInt: Single): WideString; inline;
  begin
    Result := FLOAT_START + FloatToStr(aInt) + FLOAT_END;
  end;
  function EncryptGA(): WideString;
  var
    I,K: Integer;
    Pop: TGAPopulation;
  begin
    Result := '';
    Pop := aGASetup.Population;
    Result := Result + CreateCommand(CMND_GA_MapCnt) + CreateInt(aGASetup.MapCnt);
    Result := Result + CreateCommand(CMND_GA_IdvCnt) + CreateInt(Pop.Count);
    Result := Result + CreateCommand(CMND_GA_GenCnt) + CreateInt(Pop.Individual[0].Count);
    for I := 0 to Pop.Count - 1 do
    begin
      Result := Result + CreateCommand(CMND_GA_Fit) + CreateFloat( Pop.Individual[I].Fitness );
      Result := Result + CreateCommand(CMND_GA_Genes);
      for K := 0 to Pop.Individual[I].Count - 1 do
        Result := Result + CreateFloat( Pop.Individual[I].Gene[K] );
    end;
  end;
var
  Str: WideString;
begin
  Str := CreateCommand(CMND_RunningClass) + CreateString(aMSetup.RunningClass);
  Str := Str + CreateCommand(CMND_SimTimeInMin) + CreateInt(aMSetup.SimTimeInMin);
  Str := Str + CreateCommand(CMND_SimNumber) + CreateInt(aMSetup.SimNumber);
  Str := Str + EncryptGA() + CreateCommand(CMND_GA_DONE);
  Result := Str;
end;


procedure TKMComInterface.LogInConsole(aString: String);
var
  MS: TMemoryStream;
  OutputStream: THandleStream;
begin
  MS := TMemoryStream.Create;
  try
    //MS.WriteAnsiString(AnsiString('TEST'));
    MS.WriteBuffer(Pointer(aString)^, Length(aString));
    MS.Position := 0;
    //SetLength(Test, MS.Size);
    //MS.ReadBuffer(Pointer(Test)^, MS.Size);
    //MS.Position := 0;

    OutputStream := THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE));
    try
      OutputStream.Write(MS.Memory^, MS.Size);
    finally
      OutputStream.Free;
    end;
  finally
    MS.Free;
  end;
end;


function TKMComInterface.CaptureConsoleOutput(const aWorkDir, aCommand: WideString): WideString;
const
  CReadBuffer = 2400;
var
  saSecurity: TSecurityAttributes;
  hRead, hWrite: THandle;
  suiStartup: TStartupInfoW;
  piProcess: TProcessInformation;
  pBuffer: array [0 .. CReadBuffer] of AnsiChar;
  dRead: DWord;
  dRunning: DWord;
  Handle: Boolean;
begin
  Result := '';

  saSecurity.nLength := SizeOf(TSecurityAttributes);
  saSecurity.bInheritHandle := True;
  saSecurity.lpSecurityDescriptor := nil;

  if CreatePipe(hRead, hWrite, @saSecurity, 0) then
  begin
    try
      FillChar(suiStartup, SizeOf(TStartupInfoW), #0);
      suiStartup.cb := SizeOf(TStartupInfoW);
      suiStartup.hStdInput := hRead;
      suiStartup.hStdOutput := hWrite;
      suiStartup.hStdError := hWrite;
      suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      suiStartup.wShowWindow := SW_HIDE;

      Handle := CreateProcessW(nil,
        PWideChar('cmd.exe /C ' + aCommand),
        @saSecurity,
        @saSecurity,
        True,
        CREATE_NEW_PROCESS_GROUP or NORMAL_PRIORITY_CLASS,
        nil,
	      PWideChar(aWorkDir), suiStartup, piProcess);
      CloseHandle(hWrite);
      if Handle then
      begin
        try
          repeat
            dRunning := WaitForSingleObject(piProcess.hProcess, 100);

            repeat
              dRead := 0;
              if ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil) then
              begin
                pBuffer[dRead] := #0;
                //OemToAnsi(pBuffer, pBuffer);
                Result := Result + String(pBuffer);
              end;
            until (dRead < CReadBuffer);
          until (dRunning <> WAIT_TIMEOUT);
        finally
          CloseHandle(piProcess.hProcess);
          CloseHandle(piProcess.hThread);
        end;
      end
      else
        raise Exception.Create('Can not CreateProcess ' + QuotedStr(aCommand));
    finally
      CloseHandle(hRead);
    end;
  end
  else
    raise Exception.Create('Can not CreatePipe ' + QuotedStr(aCommand));
end;



procedure TKMComInterface.DebugLogString(aString: WideString; aFileName: String);
var
  debugFile: TextFile;
begin
  AssignFile(debugFile, aFileName);
  try
    rewrite(debugFile);
    writeln(debugFile, aString);
    CloseFile(debugFile);
  except
    on E: EInOutError do
      writeln('File handling error occurred. Details: ', E.ClassName, '/', E.Message);
  end;
end;


procedure TKMComInterface.SetupSimulation(var aRSetup: TRunnerSetup; var aGASetup: TGASetup);
var
  I: Integer;
  Params: WideString;
begin
  Params := '';

  //DebugLogString(Params, 'DEBUG_ComInterface_Receive.txt');

  //Params := '{0}<TKMRunnerGA_CityPlanner>{1}(60){2}(1){4}(1){5}(19){6}[666]{7}[0,5488135219][0,5928446054][0,7151893377][0,844265759][0,6027633548][0,857945621][0,5448831916][0,8472517133][0,4236547947][0,6235637069][0,6458941102][0,3843817115][0,4375872016][0,2975346148][0,8917729855][0,05671297759][0,9636627436][0,2726562917][0,3834415078]{8}';
  //Params := '{0}<TKMRunnerGA_TestManager>{1}(60){2}(1){4}(2){5}(1){6}[0]{7}[0,7151893377]{6}[0]{7}[0,5448831916]{8}';

  //{
  for I := 1 to ParamCount do
    Params := Params + ParamStr(I);
  //}

  DecryptSetup(Params, aRSetup, aGASetup);
end;


procedure TKMComInterface.LogSimulationResults(var aRSetup: TRunnerSetup; var aGASetup: TGASetup);
var
  SetupString: WideString;
begin
  SetupString := EncryptSetup(aRSetup, aGASetup);
  //DebugLogString(SetupString, 'DEBUG_ComInterface_Send.txt');
  LogInConsole(SetupString);
end;


procedure TKMComInterface.CreateNewSimulation(var aMSetup: TManagerSetup; var aGASetup: TGASetup);
var
  SetupString, Params: WideString;
  RSetup: TRunnerSetup;
begin
  with RSetup do
  begin
    RunningClass := aMSetup.RunningClass;
    SimTimeInMin := aMSetup.SimTimeInMin;
    SimNumber := aMSetup.SimNumber;
  end;
  SetupString := EncryptSetup(RSetup, aGASetup);
  //DebugLogString(SetupString, 'DEBUG_ComInterface_Send.txt');
  Params := CaptureConsoleOutput(aMSetup.WorkDir, aMSetup.SimFile + ' "' + SetupString + '"');
  DecryptSetup(Params, RSetup, aGASetup);
  //DebugLogString(Params, 'DEBUG_ComInterface_Receive.txt');
end;



end.

