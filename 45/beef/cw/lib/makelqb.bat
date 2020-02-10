if exist fx_twin.obj del fx_twin.obj
if exist fx_syd3.obj del fx_syd3.obj
if exist fx_kbd3.obj del fx_kbd3.obj
if exist fx_kbdk.obj del fx_kbdk.obj

cd ..\..\drv\csd\src
%TL%\nmake DEFS=-DPROJECT_LQB fx_twin.obj
copy fx_twin.obj ..\..\..\cw\lib

cd ..\..\syd\src
%TL%\nmake fx_syd3.obj
copy fx_syd3.obj ..\..\..\cw\lib

cd ..\..\kbd\src
%TL%\nmake fx_kbd3.obj fx_kbdk.obj
copy fx_kbd3.obj ..\..\..\cw\lib
copy fx_kbdk.obj ..\..\..\cw\lib

%TL%\nmake -f cowtandy.mak
copy cowtandy.lib ..\..\..\cw

cd ..\..\..\cw\lib
