@echo off
if NOT "%1"=="" goto itsok
echo *************************************************************************
echo *                                                                       *
echo *                        OEM ASTRO (MS-DOS 6.0)                         *
echo *                                                                       *
echo *************************************************************************
echo *                                                                       *
echo *  This version requires four 1.2Mb floppy disks and two 360K disks     *                                                                 *
echo *  USAGE:  TRANSFER drive-spec:                                         *
echo *  WHERE:  drive-spec: is your 1.2Mb floppy drive                       *
echo *     IE:  TRANSFER b:                                                  *
echo *                                                                       *
echo *************************************************************************
goto egress
:itsok
echo 
echo Transferring Astro (MS-DOS 6.0) to drive %1
..\tools\ync /c ca "Continue or Abort"
if ErrorLevel==1 goto EGRESS


:DISK1
cls
echo Disk #1: Insert a 1.2M disk in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto DISK2
..\tools\imdump 12oem1.img %1
@rem ..\tools\imdump 12oem1.img %1 /v

:DISK2
echo 
echo Disk #2: Insert a 1.2M disk in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto DISK3
..\tools\imdump 12oem2.img %1
@rem ..\tools\imdump 12oem2.img %1 /v

:DISK3
echo 
echo Disk #3: Insert a 1.2M disk in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto DISK4
..\tools\imdump 12oem3.img %1
@rem ..\tools\imdump 12oem3.img %1 /v

:DISK4
echo 
echo Disk #4: Insert a 1.2M disk in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto DISK5
..\tools\imdump 12oem4.img %1
@rem ..\tools\imdump 12oem4.img %1 /v

:DISK5
echo 
echo Supplemental Disk 1: Insert a 360K disk in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto DISK6
..\tools\imdump 12oem5.img %1
@rem ..\tools\imdump 12oem5.img %1 /v

:DISK6
echo 
echo Supplemental Disk 2: Insert a 360K disk in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto DISK7
..\tools\imdump 12oem6.img %1
@rem ..\tools\imdump 12oem6.img %1 /v

:DISK7
GOTO DONE
echo 
echo Disk #7: Insert a 360K disk in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto DISK8
..\tools\imdump 12oem7.img %1
@rem ..\tools\imdump 12oem7.img %1 /v

:DISK8
echo 
echo Insert disk #8 in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto DISK9
..\tools\imdump disk8.img %1
@rem ..\tools\imdump disk8.img %1 /v

:DISK9
echo 
echo Insert disk #9 in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto DISK10
..\tools\imdump disk9.img %1
@rem ..\tools\imdump disk9.img %1 /v

:DISK10
echo 
echo Insert disk #10 in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto EGRESS
..\tools\imdump diska.img %1
@rem ..\tools\imdump diska.img %1 /v
goto EGRESS
:DISK11
echo 
echo Insert disk #11 in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto EGRESS
..\tools\imdump diskb.img %1
@rem ..\tools\imdump diskb.img %1 /v
goto EGRESS
:DISK12
echo 
echo Insert disk #12 in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto DISK13
..\tools\imdump diskc.img %1
@rem ..\tools\imdump diskc.img %1 /v

:DISK13
echo 
echo Insert disk #13 in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto DISK14
..\tools\imdump diskd.img %1
@rem ..\tools\imdump diskd.img %1 /v

:DISK14
echo 
echo Insert disk #14 in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto DISK15
..\tools\imdump diske.img %1
@rem ..\tools\imdump diske.img %1 /v

:DISK15
echo 
echo Insert disk #15 in %1
..\tools\ync /c msx "Make, Skip, eXit"
if ERRORLEVEL 2 goto EGRESS
if ERRORLEVEL 1 goto EGRESS
..\tools\imdump diskf.img %1
@rem ..\tools\imdump diskf.img %1 /v

:DONE
echo All done!
pause
:EGRESS
echo on
