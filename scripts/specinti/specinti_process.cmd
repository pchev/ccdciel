@echo off

rem This batch file is started by specinti_process.script to process the spectra in background and save the result
  
set specintiprogdir=%1
set specintiprog=%2
set specinticonfig=%3
set obj=%4
set robj=%obj: =%
set workdir=%5
set capturedir=%6
set resultdir=%7
set archivedir=%8
set logfile=%9

if not defined specintiprogdir exit /b 1
if not defined specintiprog exit /b 1
if not defined specinticonfig exit /b 1
if not defined obj exit /b 1
if not defined workdir exit /b 1
if not defined capturedir exit /b 1
if not defined resultdir exit /b 1
if not defined archivedir exit /b 1
if not defined logfile exit /b 1

echo %date% %time:~0,8% : Start processing for %obj% >> %logfile%

cd %specintiprogdir%
%specintiprog% %specinticonfig%

if errorlevel 1 ( 
  echo %date% %time:~0,8% : Error %obj% >> %logfile%
) else (
  move %workdir%\_%robj%* %resultdir% 
  if %archivedir% NEQ "none" (
    move %capturedir%\%obj%* %archivedir%
  )
  echo %date% %time:~0,8% : Finish processing of %obj% >> %logfile%
)

timeout 30
