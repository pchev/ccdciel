@echo off

rem This batch file is started by specinti_process.script to process the spectra in background and save the result
  
set specintiprogdir=%~1
set specintiprog=%~2
set specinticonfig=%~3
set obj=%~4
set robj=%obj: =%
set workdir=%~5
set capturedir=%~6
set resultdir=%~7
set archivedir=%~8
set saveflat=%~9
shift
set logfile=%~9

if not defined logfile (
  echo missing parameters
  timeout 30
  exit /b 1
)

echo %date% %time:~0,8% : Start processing for %obj% >> "%logfile%"

cd /d "%specintiprogdir%"
"%specintiprog%" "%specinticonfig%"

if errorlevel 1 ( 
  echo %date% %time:~0,8% : Error %obj% >> "%logfile%"
) else (
  move "%workdir%\_%robj%"* "%resultdir%"
  if "%archivedir%" NEQ "none" (
    echo move "%capturedir%\%obj%"* "%archivedir%"
    move "%capturedir%\%obj%"* "%archivedir%"
  )
  if "%saveflat%" NEQ "none" (
    echo move "%workdir%\_flat"* "%saveflat%"
    move "%workdir%\_flat"* "%saveflat%"
  )
  echo %date% %time:~0,8% : Finish processing of %obj% >> "%logfile%"
)

timeout 30
