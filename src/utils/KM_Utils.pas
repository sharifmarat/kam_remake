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

  function KMPathLength(aNodeList: TKMPointList): Single;

  function GetHintWHotKey(aText: String; aHotkeyId: Integer): String; overload;
  function GetHintWHotKey(aTextId: Integer; aHotkeyStr: String): String; overload;
  function GetHintWHotKey(aTextId, aHotkeyId: Integer): String; overload;

	function GetShiftState(aButton: TMouseButton): TShiftState;
  function GetMultiplicator(aButton: TMouseButton): Word; overload;
  function GetMultiplicator(aShift: TShiftState): Word; overload;

  function GetGameObjectOwnerIndex(aObject: TObject): TKMHandIndex;

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


function GetHintWHotKey(aText: String; aHotkeyId: Integer): String; overload;
var
  HotKeyStr: String;
begin
  Result := aText;
  HotKeyStr := gResKeys.GetKeyNameById(aHotkeyId);
  if HotKeyStr <> '' then
    Result := Result + Format(' (''%s'')', [HotKeyStr]);
end;


function GetHintWHotKey(aTextId: Integer; aHotkeyStr: String): String;
begin
  Result := gResTexts[aTextId];
  aHotkeyStr := Trim(aHotkeyStr);
  if aHotkeyStr <> '' then
    Result := Result + Format(' (''%s'')', [aHotkeyStr]);
end;


function GetHintWHotKey(aTextId, aHotkeyId: Integer): String;
begin
  Result := GetHintWHotKey(aTextId, gResKeys.GetKeyNameById(aHotkeyId));
end;



end.

