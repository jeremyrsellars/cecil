@echo off
SET GIT_DESCRIPTION=%~1
IF "x%GIT_DESCRIPTION%"=="x" for /f "usebackq" %%x in (`git describe --long --tags --dirty`) do set GIT_DESCRIPTION=%%x
ECHO git add "%~dp0..\docs"
ECHO git commit -m "Publish doc at %GIT_DESCRIPTION%"
ECHO git push
