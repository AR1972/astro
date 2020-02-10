@echo off

for %%a in (usa dut itn swe frn ger) do if "%1"=="%%a" goto start
for %%a in (USA DUT ITN SWE FRN GER) do if "%1"=="%%a" goto start
goto options


:start
SET LANG=%1
cd ..\..\qb%LANG%\helpbld
out *.hlp
copy ..\..\qbkit\bld\*.hlp
in -f -c "latest build" *.hlp
cd ..\..\qbkit\build


goto end

:options
echo Usage is GOODBLD LANG
echo This will check the latest build helpfiles into the project.

:end

