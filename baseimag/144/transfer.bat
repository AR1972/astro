@echo off
echo *************************************************************************
echo *                                                                       *
echo *                       OEMBASE ASTRO (MS-DOS 6.0)                      *
echo *                                                                       *
echo *************************************************************************
echo *                                                                       *
echo *  This version requires three 1.44Mb floppy disks                      *
echo *                                                                       *
echo *  USAGE:  TRANSFER drive-spec:                                         *
echo *  WHERE:  drive-spec: is your 1.44Mb floppy drive                      *
echo *     IE:  TRANSFER b:                                                  *
echo *                                                                       *
echo *************************************************************************
if NOT "%1"=="" goto itsok
..\tools\ync /c abx "Drive a or b"
if ERRORLEVEL==2 goto EXIT
if ERRORLEVEL==1 goto BDRIVE

call trans a:
goto EXIT

:BDRIVE
call trans b:
goto EXIT

:itsok
call trans %1

:EXIT
