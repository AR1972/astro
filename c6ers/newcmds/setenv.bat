@echo off
REM If %1 is not empty, set prompt to fun ANSI string
rem
rem Environment for compiling NEWCMDS (deltree, move, etc.)
rem
set root=c:\dos6
set PATH=%root%\C6ERS\TOOLS6\BIN;%root%\tools\bin;%PATH%
set INCLUDE=%root%\C6ERS\TOOLS6\INCLUDE
set LIB=%root%\C6ERS\TOOLS6\LIB
if "%1" == "" goto SkipPrompt
prompt $e[34;47m[$e[33mNewCmds$e[34m $p] $e[30m
:SkipPrompt
set root=
