From:	BenS
Subj:	Building QBASIC, QEDIT, and HELP for MS-DOS
Date:	16-Mar-1992 bens Initial changes to files from QB group.
	23-Apr-1992 bens Updated for first delta of online HELP.

ENLISTING IN THIS PROJECT
=========================
To compile QBASIC, enlist this project (QBASIC) into the root of
a drive, e.g.:

    cd \
    md \45
    cd \45
    enlist -vs \\guilo\slm -p qbasic

NOTE: I know "45" is not very intuitive.  However, it IS a very short
      path name, and, as such, allows this project to build under DOS
      with the 128 character command line length limit!

SEMI-ORIGINAL SOURCES, TECHNICAL CONTACT
========================================
\\tal\oaks\5a\45 [password==skao] was where ericar put the source tree that
came from the QB group.  MarkCha is a technical contact in the QB group.


QBASIC SOURCE DELIVERY
======================
This source delivery contains all sources, tools, and libraries needed to
build QBASIC.EXE and EDIT.COM.  Also included are the sources, libraries,
tools, and makefiles needed to build BQB50.LIB, the runtime routines used
by the interpreter.

The build processes are set up to build in DOS.  To do the build in OS/2,
some minor alterations to the build process may be required.


BUILDING SOURCES
================
To build QBASIC.EXE, EDIT.COM, HELP.COM, and associated HELP files:

	1) Change directories to \45\QB5\QBAS
        2) Type SAMPLE

The built files are placed in \45\QB6\QBAS, and these should be installed
by setup into the DOS directory:
    edit.com	- editor (calls QBASIC with /EDCOM switch)
    edit.hlp	- editor help file
    help.com	- on-line help (calls QBASIC with /QHELP switch)
    help.hlp	- on-line help file for MS-DOS commands
    qbasic.exe	- quick basic
    qbasic.hlp	- quick basic help

NOTE:  The first NMAKE process may fail making the parser tables from
bnf.prs.  This error can be ignored -- it happens because there is not
enough memory (usually) to do the operation under NMAKE.  The operation
will get completed in the second NMAKE process, which echoes its commands
to a batch file which is executed later.

With 561,296 bytes free (in a DOS Window under Win 3.1), the parser tables
*do* build the first time, and take about 1 min 45 sec on a Compaq 386/33.


BUILDING BQB50.LIB
==================
A built BQB50.LIB is already present in the \45\QB5\QBAS directory.  If
you want to rebuild the BQB50.LIB library:

	1) Change directories to \45\RUNTIME\QBASIC
	2) Make sure the TL environment variable is set to \45\TL\BIN
	3) Type \45\TL\BIN\NMAKE

The library build by this process should exactly match the existing one
in \45\QB5\QBAS if no changes have been made to the sources.

NOTE:  Due to an apparent memory leak in NMAKE.EXE, the build process may
not finish the first time.  If an out of memory error occurs, simply
restart NMAKE, and it will pick up where it left off.


BUILDING COW.LIB and COWTANDY.LIB
=================================
A build COW.LIB and COWTANDY.LIB are already present in the \45\QB5\QBAS
directory.  If you want to rebuild either of these libraries, instructions
are in \45\BEEF\CW\README.TXT.

---- the end ----
