@echo off
:: Assumes the clojure 1.9.0 nuget (4.0) package is decompressed and merged into this directory
if exist cecil.*.dll del cecil.*.dll
Clojure.Compile.exe cecil.ccl-to-sql cecil.cki cecil.cli cecil.reflow cecil.util cecil.standardize cecil.cki-macros
ECHO Now, copy the DLLs to /lib for cecil.net to reference
xcopy /y ..\clr\*.dll ..\..\..\lib\
