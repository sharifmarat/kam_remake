@echo off
call make_beta.bat

if errorlevel 2 (goto exit2)

REM Archive into 7z
call 7zip.bat

goto exit0

:exit2
exit /B 2

:exit0
@echo off
exit /B 0
