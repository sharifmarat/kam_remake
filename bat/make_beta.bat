@echo off
call get_kam_folder.bat

if errorlevel 2 (goto exit2)

@SET kam_folder=%build_full_kmr_dir%

call create.bat

goto exit0

:exit2
@echo off
exit /B 2

:exit0
@echo off
exit /B 0