unit TranslationManagerUtils;
{$I ..\..\KaM_Remake.inc}

interface

	function GetWorkDir(aShowWarningMess: Boolean = False): UnicodeString;

implementation
uses
	SysUtils, Dialogs;


function GetWorkDir(aShowWarningMess: Boolean = False): UnicodeString;
var
  ExeDir, ProjectDir, WorkDir: UnicodeString;
begin
  Result := '';
	ExeDir := ExtractFilePath(ParamStr(0)); //if starts from kam_remake folder
  WorkDir := ExeDir + '..\'; // If starts from kam_remake/Utils
  ProjectDir := ExeDir + '..\..\'; //If start TranslationManager from kam_remake/Utils/TranslationManager folder

  if not FileExists(WorkDir + 'data\locales.txt') then
  begin
    if FileExists(ExeDir + 'data\locales.txt') then
      WorkDir := ExeDir //If start TranslationManager from KMR folder
    else
    if FileExists(ProjectDir + 'data\locales.txt') then
      WorkDir := ProjectDir
    else
    begin
      if aShowWarningMess then
        ShowMessage('Can''t find locales.txt file at destinations:' + #13#10
          + ExeDir + 'data\locales.txt' + #13#10
          + WorkDir + 'data\locales.txt');
      Exit;
    end;
  end;
  Result := WorkDir;
end;


end.
