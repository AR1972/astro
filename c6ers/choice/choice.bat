@echo off
rem  Eric Straub 5/31/92
rem  Test Batch file for testing CHOICE
rem  To generate CHOICE.OUT, execute COMMAND/C CHOICE.BAT > CHOICE.OUT
rem  CHOICE.OUT should compare to CHOICE.TST
rem
rem  This test file causes all CHOICE.COM messages to be displayed, except
rem  for incorrect MS-DOS version message (choice requires 4.0 or later)
REM
REM Verify defaults (YN, not case sensitive)
set test=Default1
echo should here 11 beeps.. then choice Y
echo zfGhm˙IokbdY | choice.com
if not errorlevel 1 goto error
if errorlevel 2 goto error
echo *** TEST "%test%" returned expected value*

set test=Default2
echo should see no choices here...
echo n | choice.com /n
if not errorlevel 2 goto error
if errorlevel 3 goto error
echo *** TEST "%test%" returned expected value*

set test=Choices1
echo see choices A,B,C,D,E
echo E | choice /c:ABCDE
if not errorlevel 5 goto error
if errorlevel 6 goto error
echo *** TEST "%test%" returned expected value*

set test=Timeout1
echo should wait about 2 seconds...
choice /tn,2
if not errorlevel 2 goto error
if errorlevel 3 goto error
echo *** TEST "%test%" returned expected value*

set test=Timeout2
echo should see message about invalid timeout syntax
choice /t
if not errorlevel 255 goto error
echo *** TEST "%test%" returned expected value*

set test=Timeout3
echo should see message about timeout char not in choices
choice /t:z,3
if not errorlevel 255 goto error
echo *** TEST "%test%" returned expected value*

set test=BadChoice
echo should see message about invalid choice syntax
choice /c
if not errorlevel 255 goto error
echo *** TEST "%test%" returned expected value*

set test=/? help
echo should see /? help
choice /?
if not errorlevel 255 goto error
echo *** TEST "%test%" returned expected value*

set test= Invalid Switch
echo should see invalid switch message
choice /z
if not errorlevel 255 goto error
echo *** TEST "%test%" returned expected value*

set test= Prompt1
echo should see prompt: "Testing this[Y,N]?"
echo N | choice Testing this
if not errorlevel 2 goto error
if errorlevel 3 goto error
echo *** TEST "%test%" returned expected value*

set test= Prompt2
echo should see prompt: "Testing this /C /Z /T"
echo n | choice /n "Testing this /S /Z /T"
if not errorlevel 2 goto error
if errorlevel 3 goto error
echo *** TEST "%test%" returned expected value*

set test= Prompt3
echo should see message about only have one prompt
echo n | choice "Testing this /S /Z /T" OK
if not errorlevel 255 goto error
echo *** TEST "%test%" returned expected value*

set test= UcaseMap1
echo choice should be z.
echo ÇÑÖÜÅÉàâäãåçéèêìîïñóòôöz | choice /s /c:EAIOYUz
if not errorlevel 7 goto error
if errorlevel 8 goto error
echo *** TEST "%test%" returned expected value*

set test= UcaseMap2
echo choices should be upper case of ÑÖÜÅÉÇàâäãåçéèêìîïñóòôöz
echo e | choice /c:ÑÖÜÅÉÇàâäãåçéèêìîïñóòôöz
if not errorlevel 6 goto error
if errorlevel 7 goto error
echo *** TEST "%test%" returned expected value*

echo  ALL TESTS returned expected value
goto done

:error
echo  Error occurred with test: "%test%"

:done
@echo on
