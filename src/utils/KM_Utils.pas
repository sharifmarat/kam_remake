unit KM_Utils;
{$I KaM_Remake.inc}
interface
uses
  {$IFDEF MSWindows}
  Windows,
  {$ENDIF}
  {$IFDEF Unix}
  unix, baseunix, UnixUtil,
  {$ENDIF}
  {$IFDEF FPC} FileUtil, {$ENDIF}
  {$IFDEF WDC} IOUtils, {$ENDIF}
	SysUtils, StrUtils, Classes, Controls, KM_Defaults, KM_CommonTypes, KM_CommonClasses, KM_Points;

const
  DEFAULT_ATTEMPS_CNT_TO_TRY = 3;

  function KMPathLength(aNodeList: TKMPointList): Single;

  function GetHintWHotKey(aTextId, aHotkeyId: Integer): String;

	function GetShiftState(aButton: TMouseButton): TShiftState;
  function GetMultiplicator(aButton: TMouseButton): Word; overload;
  function GetMultiplicator(aShift: TShiftState): Word; overload;

  function GetGameObjectOwnerIndex(aObject: TObject): TKMHandIndex;

  function ApplyColorCoef(aColor: Cardinal; aRed, aGreen, aBlue: Single): Cardinal;

  function TryExecuteMethod(var aObjParam: TObject; aStrParam, aMethodName: UnicodeString;
                            aMethod: TUnicodeStringObjEvent; aAttemps: Byte = DEFAULT_ATTEMPS_CNT_TO_TRY): Boolean;


implementation
uses
  Math, KM_CommonUtils, KM_ResTexts, KM_ResKeys, KM_Houses, KM_Units, KM_UnitGroups, KM_Log;




function KMPathLength(aNodeList: TKMPointList): Single;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to aNodeList.Count - 1 do
    Result := Result + KMLengthDiag(aNodeList[I-1], aNodeList[I]);
end;


function GetGameObjectOwnerIndex(aObject: TObject): TKMHandIndex;
begin
  Result := -1;
  if aObject is TKMHouse then
  begin
    Result := TKMHouse(aObject).Owner;
    Exit;
  end;
  if aObject is TKMUnit then
  begin
    Result := TKMUnit(aObject).Owner;
    Exit;
  end;
  if aObject is TKMUnitGroup then
  begin
    Result := TKMUnitGroup(aObject).Owner;
    Exit;
  end;
end;


function GetShiftState(aButton: TMouseButton): TShiftState;
begin
  Result := [];
  case aButton of
    mbLeft:   Include(Result, ssLeft);
    mbRight:  Include(Result, ssRight);
  end;

  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);
end;


function GetMultiplicator(aButton: TMouseButton): Word;
begin
  Result := GetMultiplicator(GetShiftState(aButton));
end;


function GetMultiplicator(aShift: TShiftState): Word;
begin
  Result := Byte(aShift = [ssLeft]) + Byte(aShift = [ssRight]) * 10 + Byte(aShift = [ssShift, ssLeft]) * 100 + Byte(aShift = [ssShift, ssRight]) * 1000;
end;


function GetHintWHotKey(aTextId, aHotkeyId: Integer): String;
var
  HotKeyStr: String;
begin
  Result := gResTexts[aTextId];
  HotKeyStr := gResKeys.GetKeyNameById(aHotkeyId);
  if HotKeyStr <> '' then
    Result := Result + Format(' (''%s'')', [HotKeyStr]);

end;


//Multiply color by channels
function ApplyColorCoef(aColor: Cardinal; aRed, aGreen, aBlue: Single): Cardinal;
var
  R, G, B, R2, G2, B2: Byte;
begin
  //We split color to RGB values
  R := aColor and $FF;
  G := aColor shr 8 and $FF;
  B := aColor shr 16 and $FF;

  R2 := Min(Round(aRed * R), 255);
  G2 := Min(Round(aGreen * G), 255);
  B2 := Min(Round(aBlue * B), 255);

  Result := (R2 + G2 shl 8 + B2 shl 16) or $FF000000;
end;


function TryExecuteMethod(var aObjParam: TObject; aStrParam, aMethodName: UnicodeString;
                          aMethod: TUnicodeStringObjEvent; aAttemps: Byte = DEFAULT_ATTEMPS_CNT_TO_TRY): Boolean;
var
  Success: Boolean;
  TryCnt: Byte;
begin
  Success := False;
  TryCnt := 0;
  while not Success and (TryCnt < aAttemps) do
    try
      Inc(TryCnt);

      aMethod(aObjParam, aStrParam);

      Success := True;
    except
      on E: Exception do //Ignore IO exceptions here, try to save file up to 3 times
      begin
        gLog.AddTime(Format('Error at attemp #%d while executing method %s for parameter: %s', [TryCnt, aMethodName, aStrParam]));
        Sleep(10); // Wait a bit
      end;
    end;

  if not Success then
    gLog.AddTime(Format('Error executing method (%d tries) %s for parameter: %s', [aAttemps, aMethodName, aStrParam]));

  Result := Success;
end;


end.

