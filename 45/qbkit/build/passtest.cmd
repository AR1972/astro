@echo off
if "%1"=="" goto USAGE 
echo This build passed the test suites. >> c:\build\rel_%1\read.me
goto END
:USAGE
echo USAGE:  passtest xx.yy
echo         where Rel_xx.yy is the name of the directory in which
echo         the build that passed the test is archived
:END

