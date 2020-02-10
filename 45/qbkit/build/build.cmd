echo off

for %%a in (usa dut itn swe frn ger spa por) do if "%1"=="%%a" goto start
for %%a in (USA DUT ITN SWE FRN GER SPA POR) do if "%1"=="%%a" goto start
goto options


:start
SET LANG=%1

if exist results.txt del results.txt > nul
if exist *.out       del *.out       > nul
if exist *.ht        del *.ht        > nul
if exist *.mrg       del *.mrg       > nul
if exist *.qh        del *.qh        > nul
if exist makefile    del makefile    > nul
if exist *.ctx       del *.ctx       > nul

if exist replace.%LANG% goto gotrpl

       
echo You need to have built a REPLACE.LANG file.  This file is
echo built by running MAKEMSGS.EXE on QBASMSGS.TXT in the QBLANG 
echo directory.  You then take the output file QBIMSGS.H and convert
echo it into a REPLACE.LANG format using some simple editor macros.
echo Rename the file to REPLACE.LANG and put the file into this 
echo directory.

goto end

:gotrpl 

copy REPLACE.%LANG% qbimsgs.rpl

copy ..\..\qb%LANG%\edit.src ..\FILES
copy ..\..\qb%LANG%\qbasener ..\FILES
copy ..\..\qb%LANG%\qbasex   ..\FILES
copy ..\..\qb%LANG%\qbaskey  ..\FILES

mybldkit -o makefile -i . -f make.src -m libmerge
nmake

helptree qbasic.qh > qbasic.ht
helptree ..\files\edit.src > edit.ht
linktest
REM linktest uses the files info.dat, files.dat, and optionally noctx.dat

results bld.out

echo To see whether the build was good:
echo   *  look at BLD.OUT (created by mybldkit)
echo   *  look at RESULTS.TXT (created by linktest)
echo   *  in the DOS box, go to the bld directory, type:
echo        QBASIC
echo        QBASIC /EDIT
echo ÿ
echo If you want to check this build into the slm project, type: GOODBLD
echo If you don't want to check it in dont do anything
echo The files will be checked into the qbLANG\helpbld directory.
goto end

:options
echo Usage is BUILD LANG
echo Current languages supported are USA DUT ITN SWE FRN and GER
echo The files EDIT.HLP and QBASIC.HLP will be built and placed in the 
echo BLD directory for testing.

:end


