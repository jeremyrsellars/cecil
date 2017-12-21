:: @ECHO OFF
SETLOCAL
set root=%~dp0..
pushd "%~dp0"
set doc=%root%\docs
if exist "%doc%" RD "%doc%" /q/s
if not exist "%doc%" MD "%doc%"
xcopy /s /y  /exclude:exclusions.txt "%root%\src\cecil\resources\public\*" "%doc%\"
popd
