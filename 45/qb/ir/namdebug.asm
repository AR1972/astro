	TITLE	NamDebug.asm - Name Table manager debug routines
;****************************************************************************
;NamDebug.asm - Name Table Manager debug routines
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	To provide routines that are useful during the debugging of NamMgr
;	or QBI routines that use the name manager.
;
;    DebChkoNam		DebChkoNam(oNamW)
;		Verify that oNamW is a valid tNam offset.  DebHalt is called
;		if oNamW isn't valid.
;
;    DebChkogNam	DebChkogNam(ogNamW)
;		Same as DebChkoNam, but checks an ogNam instead of an oNam.
;
;    DebChktNam		DebChktNam()
;		Perform a sanity check on tNam.  DebHalt is called if any
;		inconsistency is found in tNam.
;
;    DebChktgNam	DebChktgNam()
;		Perform a sanity check on global tNam.	DebHalt is called if
;		any inconsistency is found in the global tNam.
;
;
;    DebNamTabNoMem	DebNamTabNoMem()
;		This routine is called from within NamMgr.asm whenever an
;		out of memory situation occurs.  DebHalt is then called.
;
;    DebOoNam		DebOoNam(oNamW)
;		To output the ASCII chars of oNamW to stdout.
;
;    DebOogNam		DebOogNam(ogNamW)
;		To output the ASCII chars of ogNamW to stdout.
;
;    DebOtNam		DebOtNam(fOmitNames)
;		To dump tNam to stdout.
;
;    DebOtgNam		DebOtgNam(fOmitNames)
;		To dump global tNam to stdout.
;
;
;****************************************************************************
MODULE_NAME EQU 'NamDebug'

	.xlist
	.list
	.lall


	NAMDEBUG_ASM = 1
	include			version.inc
	END	
