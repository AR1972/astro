From: BenS
Subj: New commands for MS-DOS 6, lifted from SYSTOOLS!
Date: 07-Nov-1992 bens Initial version
      14-Nov-1992 bens Modified comments to reflect original source location,
                         and importance of SETARGV.OBJ.

HOW TO BUILD
============
1) Edit SETENV.BAT to set the correct value for ROOT.
2) Run SETENV.BAT
3) NMAKE

   NOTE: These tools build with C6!


LOCALIZATION NOTES
==================
1) The *.MSG files are meant to be translated.  I assume johnhic will
   move these to the appropriate LANG directory tree and fix the makefile.

2) These commands use some C run-time library error message strings,
   so you need to be sure to link with the translated SLIBCE.LIB!


HISTORY
=======
The files in the NEWCMDS directory were copied from:
    \\TOOLSVR\SOURCES\SLM\SRC\SYSTOOLS\CMDS
and then trimmed down to remove OS/2, bound, and NT build options.

The files in the TOOLIB directory tree were copied from:
    \\TOOLSVR\SOURCES\SLM\SRC\TLIBS\TOOLS
and then trimmed down to remove OS/2, bound, and NT build options.

Note that MarkZ is the creator of this, but as of 07-Nov-1992
ErichSt is responsible for maintenance.


General Notes
=============
Tools are linked with SETARGV.OBJ from the standard C run-time library
to get automatic expansion of wild cards in arguments.
This is IMPORTANT!


Files to Localize
=================
DELTREE.MSG
MOVE.MSG
TOOLIB\SRC\MESSAGES.MSG


DELTREE (formerly DELNODE) NOTES
================================
In the \\TOOLSVR project, it linked with LIBH.LIB and RE.MI
libraries.  These did not seemed to be used, so I stopped linking
with them.


MOVE (formerly MV) NOTES
========================
In the \\TOOLSVR project, it linked with LIBH.LIB and RE.MI
libraries.  These did not seemed to be used, so I stopped linking
with them.

<<< the end >>>
