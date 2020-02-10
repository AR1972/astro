echo off

ATTRIB +R inc\cow.inc

rem -- set up standard COW environment (requires DOSENV)

if "%1" == "" goto usage

rem -- prepare master defines (assume debug)
set DEFS=????
set CORE=COW_TEXT
set AFLAGS=-Z -Mx
set CCFLAGS=-Gcsq -Zpe -Oas -W3 -DCC
set MAKE_LOADER=
set MAKE_CBOX=
set MAKE_WIN=
set OSTARG=DOS3
set SWAP=NONE
set KANJI=
set DEBUG=
set MAKE_QC=

rem -- 3 standard versions
if "%1" == "cow3" goto v_cow3
if "%1" == "dcow3" goto v_dcow3
if "%1" == "dcow3o" goto v_dcow3o
if "%1" == "cow5" goto v_cow5
if "%1" == "dcow5" goto v_dcow5
if "%1" == "cows" goto v_cows
if "%1" == "dcows" goto v_dcows
if "%1" == "cowsx" goto v_cowsx
if "%1" == "dcowsx" goto v_dcowsx
if "%1" == "dcowd" goto v_dcowd
rem -- COWT
if "%1" == "cowt" goto v_cowt
if "%1" == "dcowt" goto v_dcowt
rem -- QB/QC variants
if "%1" == "cow3e" goto v_cow3e
if "%1" == "dcow3e" goto v_dcow3e
if "%1" == "cowej" goto v_cowej
if "%1" == "dcowej" goto v_dcowej
if "%1" == "cowqb" goto v_cowqb
if "%1" == "dcowqb" goto v_dcowqb
if "%1" == "cowlqb" goto v_cowlqb
if "%1" == "dcowlqb" goto v_dcowlqb
if "%1" == "kowqb" goto v_kowqb
if "%1" == "dkowqb" goto v_dkowqb
if "%1" == "cowsqc" goto v_cowsqc
if "%1" == "dcowsqc" goto v_dcowsqc
rem -- CBOX
if "%1" == "dprep" goto v_dprep
if "%1" == "cboxs" goto v_cboxs
if "%1" == "cboxw" goto v_cboxw
if "%1" == "cbox5" goto v_cbox5
if "%1" == "cboxd" goto v_cboxd
if "%1" == "dcboxs" goto v_dcboxs
if "%1" == "dcboxw" goto v_dcboxw
if "%1" == "dcbox5" goto v_dcbox5
if "%1" == "dcboxd" goto v_dcboxd

rem -- WINdows versions
if "%1" == "dcwow" goto v_dcwow
if "%1" == "cwow" goto v_cwow

REM : KOW used for Kanji versions
set KANJI=1
if "%1" == "kow3" goto v_kow3
if "%1" == "dkow3" goto v_dkow3
if "%1" == "kows" goto v_kows
if "%1" == "dkows" goto v_dkows

rem -- unknown version, print usage

:usage
echo "	 ===== make for CW =====                                        "
echo "	dcow3  => non-swapped CW for DOS 3                              "
echo "	dcow3o => non-swapped CW for DOS 3  with overlapping windows    "
echo "	dcow5  => non-swapped CW for OS/2                               "
echo "	dcows  => fat swapped CW for DOS 3                              "
echo "	dcowsx => fat swapped CW for DOS 3  without EXTRAS              "
echo "	dcowd  => fat swapped CW for DUAL                               "
echo "	dcowt  => thin swapped CW for DOS 3     (MANAGER 2.0)           "
echo "	dcow3e => like "dcow3" with full edit   (QB)                    "
echo "	dcowqb => everything for qb version of CW                       "
echo "	dcowsqc => everything for qc version of CW                      "
echo "	                                                                "
echo "	dcwow  => Windows version                                       "
echo "	dkow3  => Kanji version                                         "
echo "	                                                                "
echo "	dcboxs => dcows + CBOX extensions                               "
echo "	dcboxw => dcows + CBOX extensions + Word support                "
echo "	dcboxd => dcows + CBOX extensions + Word support + DUAL         "
echo "	dcbox5 => dcow5 + CBOX extensions                               "
echo "	                                                                "
echo "	also:                                                           "
echo "	        remove the "d" prefix for non-debugging versions        "
echo "	                                                                "
goto end

rem ***************************************************************************

rem ****** Non-swapped DOS 3 ******
:v_kow3
set TITLE=DOS 3, NON SWAPPED, NON-DEBUGGING
set VERSION=kow35.ver
set DEFS=-DDOS3 -DNOPCODE
set KERNEL=kernel3.lib
set CCFLAGS=-Gcs -Zpe -Oas -W3 -DCC
set FINAL=kow3.lib
goto doit

:v_dkow3
set TITLE=DOS 3, NON SWAPPED, DEBUGGING
set VERSION=kow35.ver
set DEFS=-DDEBUG -DDOS3 -DNOPCODE
set KERNEL=kernel3.lib
set CCFLAGS=-Gcs -Zped -Od -W3 -DCC
set FINAL=dkow3.lib
goto ddoit

:v_cow3
set TITLE=DOS 3, NON SWAPPED, NON-DEBUGGING
set VERSION=cow35.ver
goto doit_n3

:v_dcow3
set TITLE=DOS 3, NON SWAPPED, DEBUGGING
set VERSION=cow35.ver
goto doit_d3

:v_cowej
set KANJI=1

:v_cow3e
set TITLE=DOS 3, NON SWAPPED, NON-DEBUGGING, FULL EDIT
set VERSION=ecow35.ver
goto doit_n3

:v_dcowej
set KANJI=1

:v_dcow3e
set TITLE=DOS 3, NON SWAPPED, DEBUGGING, FULL EDIT
set VERSION=ecow35.ver
goto doit_d3

:v_kowqb
set KANJI=1
set TITLE=Everything for QBJ
set VERSION=kowqb.ver
set DEFS=-DDOS3 -DNOPCODE
set KERNEL=kernel3.lib
set CCFLAGS=-Gcs -Zpe -Oas -W3 -DCC
set FINAL=kowqb.lib
goto doit

:v_cowqb
set TITLE=Everything for QB
set VERSION=cowqb.ver
set DEFS=-DDOS3 -DNOPCODE
set KERNEL=kernel3.lib
set CCFLAGS=-Gcs -Zpe -Oas -W3 -DCC
set FINAL=cowqb.lib
goto doit

:v_cowlqb
set TITLE=Everything for LQB
set VERSION=cowlqb.ver
set DEFS=-DDOS3 -DNOPCODE
set KERNEL=kernel3.lib
set CCFLAGS=-Gcs -Zpe -Oas -W3 -DCC
set FINAL=cowlqb.lib
goto doit

:v_dkowqb
set KANJI=1
set TITLE=Everything for QBJ, DEBUGGING
set VERSION=kowqb.ver
set DEFS=-DDEBUG -DDOS3 -DNOPCODE
set KERNEL=kernel3.lib
set CCFLAGS=-Gcs -Zpe -Oas -W3 -DCC
set FINAL=dkowqb.lib
goto doit

:v_dcowqb
set TITLE=Everything for QB, DEBUGGING
set VERSION=cowqb.ver
set DEFS=-DDEBUG -DDOS3 -DNOPCODE
set KERNEL=kernel3.lib
set CCFLAGS=-Gcs -Zpe -Oas -W3 -DCC
set FINAL=dcowqb.lib
goto doit

:v_dcowlqb
set TITLE=Everything for LQB, DEBUGGING
set VERSION=cowlqb.ver
set DEFS=-DDEBUG -DDOS3 -DNOPCODE
set KERNEL=kernel3.lib
set CCFLAGS=-Gcs -Zpe -Oas -W3 -DCC
set FINAL=dcowlqb.lib
goto doit

:v_dcow3o
set TITLE=DOS 3, NON SWAPPED, DEBUGGING
set VERSION=cow35o.ver
set DEFS=-DDEBUG -DDOS3 -DNOPCODE
set KERNEL=kernel3.lib
set CCFLAGS=-Gcs -Zpe -Oas -W3 -DCC
set FINAL=dcow3o.lib
goto ddoit

:doit_n3	# non-debug DOS 3
set DEFS=-DDOS3 -DNOPCODE
set KERNEL=kernel3.lib
set CCFLAGS=-Gcs -Zpe -Oas -W3 -DCC
set FINAL=cow3.lib
goto doit

:doit_d3	# debug DOS 3
set DEFS=-DDEBUG -DDOS3 -DNOPCODE
set KERNEL=kernel3.lib
set CCFLAGS=-Gcs -Zpe -Oas -W3 -DCC
set FINAL=dcow3.lib
goto ddoit

rem ***************************************************************************

rem ****** Non-swapped OS/2 ******

:v_cbox5
set TITLE=OS/2, NON SWAPPED, NON-DEBUGGING, CBOX
set VERSION=cbox5.ver
set DEFS=-DDOS5 -DNOPCODE
set KERNEL=kernel5.lib
set FINAL=cbox5.lib
set MAKE_CBOX=1
goto doit_os2

:v_dcbox5
set TITLE=OS/2, NON SWAPPED, DEBUGGING, CBOX
set VERSION=cbox5.ver
set DEFS=-DDEBUG -DDOS5 -DNOPCODE
set KERNEL=kernel5.lib
set FINAL=dcbox5.lib
set MAKE_CBOX=1
goto ddoit_os2

:v_dprep
set TITLE=DUAL OS/2 prep
set VERSION=cboxd.ver
set DEFS=-DDOS5 -DNOPCODE
set KERNEL=dprep
set FINAL=dcboxd.lib
goto doit_os2

:v_cow5
set TITLE=OS/2, NON SWAPPED, NON-DEBUGGING
set VERSION=cow35.ver
set DEFS=-DDOS5 -DNOPCODE
set KERNEL=kernel5.lib
set FINAL=cow5.lib
goto doit_os2

:v_dcow5
set TITLE=OS/2, NON SWAPPED, DEBUGGING
set VERSION=cow35.ver
set DEFS=-DDEBUG -DDOS5 -DNOPCODE
set KERNEL=kernel5.lib
set FINAL=dcow5.lib
goto ddoit_os2

:ddoit_os2	# OS 2 special debug
set DEBUG=DEBUG

:doit_os2	# OS 2 special
set CCFLAGS=%CCFLAGS% -G2
rem -- -Mu set for OS/2 calls
set AFLAGS=-Z -Mu
set OSTARG=OS2
goto doit


rem ***************************************************************************

rem ****** Swapped Dos3 ******

:v_kows
set TITLE=DOS 3, SWAPPED, NON-DEBUGGING
set VERSION=kows.ver
set SWAP=COWS
set FINAL=kows.lib
goto doit_ns

:v_dkows
set TITLE=DOS 3, SWAPPED, DEBUGGING
set VERSION=kows.ver
set SWAP=COWS
set FINAL=dkows.lib
goto doit_ds

:v_cows
set TITLE=DOS 3, SWAPPED, NON-DEBUGGING
set VERSION=cows.ver
set SWAP=COWS
set FINAL=cows.lib
goto doit_ns

:v_dcows
set TITLE=DOS 3, SWAPPED, DEBUGGING
set VERSION=cows.ver
set SWAP=COWS
set FINAL=dcows.lib
goto doit_ds

:v_cowsx
set TITLE=DOS 3, SWAPPED, NON-DEBUGGING
set VERSION=email.ver
set SWAP=COWS
set FINAL=cowsx.lib
goto doit_ns

:v_dcowsx
set TITLE=DOS 3, SWAPPED, DEBUGGING
set VERSION=email.ver
set SWAP=COWS
set FINAL=dcowsx.lib
goto doit_ds

:v_dcowd
set TITLE=DUAL, SWAPPED, DEBUGGING
set VERSION=cowd.ver
set SWAP=COWS
set FINAL=dcowd.lib
goto doit_dd

:v_cboxs
set TITLE=DOS 3, SWAPPED, NON-DEBUGGING, CBOX
set VERSION=cboxs.ver
set SWAP=COWS
set FINAL=cboxs.lib
set MAKE_CBOX=1
goto doit_ns

:v_dcboxs
set TITLE=DOS 3, SWAPPED, DEBUGGING, CBOX
set VERSION=cboxs.ver
set SWAP=COWS
set FINAL=dcboxs.lib
set MAKE_CBOX=1
goto doit_ds

:v_cboxw
set TITLE=DOS 3, SWAPPED, NON-DEBUGGING, CBOX, WORDTSR
set VERSION=cboxw.ver
set SWAP=COWS
set FINAL=cboxw.lib
set MAKE_CBOX=1
goto doit_ns

:v_dcboxw
set TITLE=DOS 3, SWAPPED, DEBUGGING, CBOX, WORDTSR
set VERSION=cboxw.ver
set SWAP=COWS
set FINAL=dcboxw.lib
set MAKE_CBOX=1
goto doit_ds

:v_cboxd
set TITLE=DUAL, NON-DEBUGGING, CBOX, WORDTSR
set VERSION=cboxd.ver
set SWAP=COWS
set FINAL=cboxd.lib
set MAKE_CBOX=1
goto doit_nd

:v_dcboxd
set TITLE=DUAL, DEBUGGING, CBOX, WORDTSR
set VERSION=cboxd.ver
set SWAP=COWS
set FINAL=dcboxd.lib
set MAKE_CBOX=1
goto doit_dd

:v_cowt
set TITLE=DOS 3, SWAPPED, NON-DEBUGGING (tiny resident)
set VERSION=cowt.ver
set SWAP=COWT
set FINAL=cowt.lib
goto doit_ns

:v_dcowt
set TITLE=DOS 3, SWAPPED, DEBUGGING (tiny resident)
set VERSION=cowt.ver
set SWAP=COWT
set FINAL=dcowt.lib
goto doit_ds

:doit_ns	# non-debug swapped
set MAKE_LOADER=1
set CORE=CORE
set DEFS=-DCOW_SWAPPED
set KERNEL=kernel.lib
goto doit

:doit_ds	# debug swapped
set MAKE_LOADER=1
set CORE=CORE
set DEFS=-DDEBUG -DPROFILE -DCOW_SWAPPED
set KERNEL=kernel.lib
goto ddoit

:doit_nd	# non-debug DUAL
set MAKE_LOADER=1
set CORE=CORE
set DEFS=-DCOW_SWAPPED
set DEFSOS2=-DDOS5 -DNOPCODE
set OSTARG=DUAL
set KERNEL=kerneld.lib
goto doit

:doit_dd	# debug DUAL
set MAKE_LOADER=1
set CORE=CORE
set DEFS=-DDEBUG -DPROFILE -DCOW_SWAPPED
set DEFSOS2=-DDEBUG -DDOS5 -DNOPCODE
set OSTARG=DUAL
set KERNEL=kerneld.lib
goto ddoit

rem ****** Swapped QC Dos3 ******

:v_cowsqc
set TITLE=DOS 3, QC, SWAPPED, NON-DEBUGGING
set VERSION=cowsqc.ver
set SWAP=COWS
set MAKE_QC=QC
set FINAL=cowsqc.lib
goto doit_ns

:v_dcowsqc
set TITLE=DOS 3, QC, SWAPPED, DEBUGGING
set VERSION=cowsqc.ver
set SWAP=COWS
set MAKE_QC=QC
set FINAL=dcowsqc.lib
goto doit_ds

rem ***************************************************************************
rem ***** WINDOWS *****

:v_dcwow
set TITLE=DOS 3, WINDOWS, DEBUGGING
set DEFS=-DWIN -DDEBUG
set FINAL=dcwow.lib
goto doit_win

:v_cwow
set TITLE=DOS 3, WINDOWS, NON-DEBUGGING
set DEFS=-DWIN
set FINAL=cwow.lib

:doit_win
rem : NOTE: -Gw for Windows !!!
set CCFLAGS=-Gcsw -Zpe -Oas -W3 -DCC
set VERSION=cwow.ver
set SWAP=COWS
set CORE=CORE
set KERNEL=win.lib
set MAKE_WIN=1
goto doit

rem ***************************************************************************

:ddoit
set DEBUG=DEBUG

:doit

if "%KANJI%" == "" goto not_kanji
set DEFS=%DEFS% -DKANJI
echo !KANJI VERSION!
:not_kanji

rem ********** Prepare make log **********
if NOT "%LOG%" == "" goto :skip
set LOG=log
if not exist log goto skip
copy log log.old
:skip
echo Making %TITLE% Version of CW
echo *** CW MAKE PROCESS FOR %TITLE% *** >%LOG%

set incsave=%INCLUDE%
set INCLUDE=inc;%INCLUDE%;$%FINAL%

if EXIST depfile goto have_dep
%TL%\nmake depfile
:have_dep

rem ********** ALL **********
%TL%\nmake -f makefile -f depfile  %2 %3 %4 all		>> %LOG%

:end
rem -- clean up any set variables
if NOT "%LOG%" == "log" goto skip2
set LOG=
:skip2

set AFLAGS=
set CCFLAGS=
set CORE=
set DEFS=
set DEFSOS2=
set FINAL=
set KANJI=
set KERNEL=
set MAKE_CBOX=
set MAKE_WIN=
set MAKE_LOADER=
set OSTARG=
set SWAP=
set TITLE=
set VERSION=
set DEBUG=

set INCLUDE=%incsave%
set incsave=
