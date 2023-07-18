@echo off

rem Example script to run astrometry.net on a online api server.
rem Tested with nova.astrometry.net and ANSVR

rem script configuration :

rem the server running the api
set "apiserver=http://nova.astrometry.net/api/"

rem get api key from the server i.e. http://nova.astrometry.net/
set "myapikey=xxxx"

rem python command, default is from CCDciel setup
set "pythoncmd=%ccdcielpython%"

rem directory containing the client script
set "scriptdir=%ccdcielscriptdir%"

rem the api client script (from https://github.com/dstndstn/astrometry.net/tree/master/net/client )
set "clientscript=%scriptdir%\client.py"

rem end of configuration

if "%~3" neq "" (
  echo wrong number of parameters
  exit /b 1
)

set "infile=%~1"
set "param=%~2"
set "wcsfile=%infile:.fits=.wcs%"
set "solvfile=%infile:.fits=.solved%"

set "onlparam=--server=%apiserver% --apikey=%myapikey% --upload=%infile% --wait --private --wcs=%wcsfile%"

rem copy parameters to onlparam
call :parse %param%
goto :continue

:parse
:loop
if "%~1" == "" goto :end
if "%~1" == "--scale-low" set "onlparam=%onlparam% --scale-lower=%~2" & shift & goto :loop
if "%~1" == "--scale-high" set "onlparam=%onlparam% --scale-upper=%~2" & shift & goto :loop
if "%~1" == "--scale-units" set "onlparam=%onlparam% --scale-units=%~2" & shift & goto :loop
if "%~1" == "--ra" set "onlparam=%onlparam% --ra=%~2" & shift & goto :loop
if "%~1" == "--dec" set "onlparam=%onlparam% --dec=%~2" & shift & goto :loop
if "%~1" == "--radius" set "onlparam=%onlparam% --radius=%~2" & shift & goto :loop
shift
goto :loop
:end

:continue

echo %pythoncmd% %clientscript% %onlparam%
"%pythoncmd%" "%clientscript%" %onlparam%
set rc=%errorlevel%
if %rc% equ 0 (
  echo 1 > %solvfile%
)
exit /b %rc%
