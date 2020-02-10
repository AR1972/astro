echo off
if "%1" == "" goto useage
..\tl\link %2 %3 %4 %5 %6/qui,%1,%1/map,..\test\bqlb40.lib/noi/nod;
goto done
:useage
echo useage: bldlib ulname obj1 [obj2 obj3 ...]
:done
