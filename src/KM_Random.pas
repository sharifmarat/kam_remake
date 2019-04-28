unit KM_Random;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF FPC}zstream, {$ENDIF}
  {$IFDEF WDC}ZLib, {$ENDIF}
  Generics.Collections,
  KM_CommonTypes, KM_CommonClasses;

type
  TKMLogRngType = (lrtInt, lrtSingle, lrtExt);

  TKMRngLogRecord = record
//    ValueType: TKMLogRngType;
//    ValueI: Integer;
//    ValueS: Single;
    Value: Extended;
    CallerId: Byte;
    Tick: Cardinal;
  end;

  TKMRandomCheckLogger = class
  private
    fGameTick: Cardinal;
    fRngLogStream: TKMemoryStream;
    fCallers: TDictionary<AnsiString, Byte>;
    fRngLog: TList<TKMRngLogRecord>;
    function GetCallerID(const aCaller: AnsiString; aValue: Extended; aValueType: TKMLogRngType): Byte;
  public
    constructor Create;
    destructor Destroy; override;

//    procedure AddToLog(const aCaller: AnsiString; aValue: Integer); overload;
//    procedure AddToLog(const aCaller: AnsiString; aValue: Single); overload;
    procedure AddToLog(const aCaller: AnsiString; aValue: Extended); //overload;

    property RngLogStream: TKMemoryStream read fRngLogStream;

    procedure SaveToPath(aPath: String);
    procedure Clear;

    procedure UpdateState(aGameTick: Cardinal);
  end;


var
  gRandomCheckLogger: TKMRandomCheckLogger;



implementation
uses
  SysUtils, KromUtils, KM_Defaults{, KM_Game};

const
  MAX_LOG_LENGTH = 200000;


{ TKMRandomLogger }
constructor TKMRandomCheckLogger.Create;
begin
  fRngLogStream := TKMemoryStream.Create;
  fCallers := TDictionary<AnsiString, Byte>.Create;
  fRngLog := TList<TKMRngLogRecord>.Create;
end;


destructor TKMRandomCheckLogger.Destroy;
begin
  fCallers.Clear;
  FreeAndNil(fRngLog);
  FreeAndNil(fCallers);
  FreeAndNil(fRngLogStream);
  inherited;
end;


function TKMRandomCheckLogger.GetCallerID(const aCaller: AnsiString; aValue: Extended; aValueType: TKMLogRngType): Byte;
var
  CallerID: Byte;
begin
  if not fCallers.TryGetValue(aCaller, CallerID) then
  begin
    CallerId := fCallers.Count;
    fCallers.Add(aCaller, CallerId);
  end;
  Result := CallerId;
end;


procedure TKMRandomCheckLogger.AddToLog(const aCaller: AnsiString; aValue: Extended);
var
  rec: TKMRngLogRecord;
begin
  rec.Tick := fGameTick;

  rec.Value := aValue;
  rec.CallerId := GetCallerID(aCaller, aValue, lrtInt);
  fRngLog.Add(rec);
//  GetCallerID(aCaller, aValue, lrtInt);
//  fRngLogStream.Write(aValue);
end;


procedure TKMRandomCheckLogger.UpdateState(aGameTick: Cardinal);
begin
  fGameTick := aGameTick;
  if fRngLog.Count > MAX_LOG_LENGTH then
    fRngLog.DeleteRange(0, fRngLog.Count - MAX_LOG_LENGTH);
end;


//procedure TKMRandomCheckLogger.AddToLog(const aCaller: AnsiString; aValue: Single);
//begin
//  GetCallerID(aCaller, aValue, lrtSingle);
//  fRngLogStream.Write(aValue);
//end;
//
//
//procedure TKMRandomCheckLogger.AddToLog(const aCaller: AnsiString; aValue: Integer);
//var
//  rec: TKMRngLogRecord;
//begin
//  rec.Value := aValue;
//  rec.CallerId := GetCallerID(aCaller, aValue, lrtInt);
//  fRngLog.Add(rec);
////  GetCallerID(aCaller, aValue, lrtInt);
////  fRngLogStream.Write(aValue);
//end;


procedure TKMRandomCheckLogger.SaveToPath(aPath: String);
var
  SaveStream: TKMemoryStream;
//  CompressionStream: TCompressionStream;
  CallerPair: TPair<AnsiString, Byte>;
  I: Integer;
begin
  if not SAVE_RANDOM_CHECKS then
    Exit;

  SaveStream := TKMemoryStream.Create;

  SaveStream.WriteA('CallersTable');
  SaveStream.Write(fCallers.Count);

  for CallerPair in fCallers do
  begin
    SaveStream.WriteA(CallerPair.Key);
    SaveStream.Write(CallerPair.Value);
  end;
  SaveStream.WriteA('KaMRandom_calls');
  SaveStream.Write(fRngLog.Count);
  for I := 0 to fRngLog.Count - 1 do
  begin
    SaveStream.Write(fRngLog[I].Tick);
    SaveStream.Write(fRngLog[I].CallerId);
    SaveStream.Write(fRngLog[I].Value);
  end;

//  CompressionStream := TCompressionStream.Create(clNone, SaveStream);
//  CompressionStream.CopyFrom(fRngLogStream, 0);
  //SaveStream now contains the compressed data from SourceStream
//  CompressionStream.Free;

  SaveStream.SaveToFile(aPath);
  SaveStream.Free;
end;


procedure TKMRandomCheckLogger.Clear;
begin
  fRngLogStream.Clear;
end;


end.
