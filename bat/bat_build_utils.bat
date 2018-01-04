call bat_rsvars.bat

@REM build utils, included into the build
REM msbuild ..\Utils\DedicatedServerGUI\KaM_DedicatedServerGUI.dproj /p:Configuration=Release /t:Build /clp:ErrorsOnly /fl /flp:LogFile="bat_build_dedicated_server_gui.log"
msbuild ..\Utils\DedicatedServer\KaM_DedicatedServer.dproj /p:Configuration=Release /t:Build /clp:ErrorsOnly /fl /flp:LogFile="bat_build_dedicated_server.log"
REM msbuild "..\Utils\Campaign builder\CampaignBuilder.dproj" /p:Configuration=Release /t:Build /clp:ErrorsOnly /fl /flp:LogFile="bat_build_campaign_builder.log"
 
REM msbuild ..\Utils\ScriptValidator\ScriptValidator.dproj /p:Configuration=Release /t:Build /clp:ErrorsOnly /fl /flp:LogFile="bat_build_script_validator.log"
REM msbuild ..\Utils\TranslationManager\TranslationManager.dproj /p:Configuration=Release /t:Build /clp:ErrorsOnly /fl /flp:LogFile="bat_build_translation_manager.log"