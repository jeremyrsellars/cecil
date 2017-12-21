@echo off
for /f "usebackq" %%x in (`git describe --long --tags --dirty`) do set GIT_DESCRIPTION=%%x
pushd "%~dp0..\src\cecil"
call lein clean
call lein cljsbuild once release
popd
call "%~dp0pub_doc.cmd"
echo.
echo -------------------------------------------------------
echo.
echo Next, check over the changes and commit and push.
echo.
call "%~dp0commit_hint.cmd" "%GIT_DESCRIPTION%"
