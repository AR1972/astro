set ROOT=C:\astro
set PATH=%ROOT%\c6ers\tools6\bin
set LIB=%ROOT%\c6ers\tools6\lib;%ROOT%\c6ers\toolsvr\lib
set INCLUDE=.;%ROOT%\c6ers\tools6\include;%ROOT%\c6ers\toolsvr\inc

cl buildmsg.c -link /stack:8192