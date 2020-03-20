@echo off

set DEBUGSAV=%DEBUG%

set CMPROOT=

set DEBUG=NO
set DECOMP=COMPLEX
set OS=DOS
set OSLET=R
set MODEL=S

nmake -f mkfile2 -nologo
copy sdecompr.lib ..\..\lib

set DEBUG=NO
set MODEL=M

nmake -f mkfile2 -nologo
copy ldecompr.lib ..\..\lib

set DEBUG=%DEBUGSAV%

set DEBUGSAV=
set DECOMP=
set OS=
set OSLET=
set MODEL=
set CMPROOT=
set RELEASE=
