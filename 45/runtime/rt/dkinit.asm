	TITLE	DKINIT.ASM - Disk I/O Initialization/Termination module
;***
;DKINIT.ASM - Disk I/O	initialization/termination module
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	This module contains Disk I/O initialization and termination
;	support for the BASIC 3.0 runtime.  Most of this initialization
;	and termination is also necessary for programs which use device
;	I/O without using disk I/O.  This module will be present in a
;	user's program when a program contains statements which need
;	disk OR device I/O.
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;
;	Code Segments
;
	USESEG	<INIT_CODE>	;Initialization
	USESEG	<DV_TEXT>	;Device I/O
	USESEG	<DK_TEXT>	;Disk I/O

;
;	Data Segments
;
	USESEG	<_BSS>		;runtime data (uninitialized)
	USESEG	<_DATA> 	;runtime data (initialized)
	USESEG	<XIB>		; initializer start segment
	USESEG	<XI>		;initializer segment
	USESEG	<XIE>		; initializer end segment

	INCLUDE seg.inc
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE nhutil.inc
	INCLUDE rtps.inc

	INCLUDE compvect.inc	;component vectors

	INITIALIZER	B$xDKINI	;put B$xDKINI in initializer list.

	SUBTTL	Code Externals
	PAGE

	externFP	B$PEOS

sBegin	DV_TEXT
	externNP	B$NearRet	;for dispatch vectors in compvect.inc
	PUBLIC	b$dkused		;This public is defined here and
b$dkused equ 0				;  referenced in dkutil so that
					;  dkinit is pulled in when appropriate
	externNP	B$CLOSF		;closes all open files
	externFP	B$IOINI		;installs int handlers
	externNP	B$IOCLOS	;resets int handlers
	externNP	B$TEST_CLOSE	;test if in CLOSE at INT24 time

sEnd	DV_TEXT

sBegin	DK_TEXT
	externNP	B$FLUSH
sEnd	DK_TEXT

	SUBTTL	Runtime data definitions for BASIC Disk I/O
	PAGE

sBegin	_DATA
;
;	external data
;
	externW b$run_disp	; RUN time initialization dispatch table
	externW b$clrt_disp	; CLEAR statement support dispatch table
	externW b$shli_disp	; Shell initialization dispatch table
	externW b$shlt_disp	; Shell termination dispatch table
	externW b$pTEST_CLOSE	; conditional vector to B$TEST_CLOSE
	externW b$end_disp	; END time termination dispatch table
	externW b$err_disp	; error dispatch table
	externW b$pFLUSH	; conditional vector to B$FLUSH
	externB	b$FInput	
	InpTTY	= 0		; possible values of b$FInput
	InpDsk	= 1		
	InpDefault = 0FFh	
sEnd	_DATA

	SUBTTL	Runtime Disk I/O  Initialization
	PAGE
assumes CS,INIT_CODE
sBegin	INIT_CODE

;***
;B$xDKINI - Disk I/O  initializer
;PLM B$xDKINI()
;
;Purpose:
;	Initializer for Disk I/O  component.  This routine is called
;	by the Crt0 startup before _main is called.  It will update the
;	indirect dispatch tables for the Disk I/O  routines.  This
;	insures that the only time that Disk I/O  is accessed is when
;	this module is linked into the user program.
;
;Entry:
;	None.
;
;Exit:
;	Appropriate dispatch vectors filled.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$xDKINI,<FAR>
cBegin
;
;	update "RUN" time initialization dispatch address to B$CLOSF	
;
	MOV	WORD PTR [b$run_disp].DK_RVEC,DV_TEXTOFFSET B$CLOSF	
;
;	update CLEAR statement dispatch address to B$CLOSF		
;
	MOV	WORD PTR [b$clrt_disp].DK_CLTVEC,DV_TEXTOFFSET B$CLOSF	
;
;	update END statement dispatch address to B$CLOSF		
;
	MOV	WORD PTR [b$end_disp].DK_EVEC,DK_TEXTOFFSET B$CLOSF	

;
;	update SHELL statement initialization dispatch address to B$IOINI   
;
	MOV	WORD PTR [b$shli_disp].DK_SIVEC,DK_TEXTOFFSET B$IOINI	
;
;	update SHELL statement termination dispatch address to B$IOCLOS 
;
	MOV	WORD PTR [b$shlt_disp].DK_STVEC,DK_TEXTOFFSET B$IOCLOS	

;
;	update error dispatch address to B$DKERR
;
	MOV	WORD PTR [b$err_disp].DK_ERVEC,DK_TEXTOFFSET B$DKERR	

;	initialize granularity dispatchers.

	MOV	b$pFLUSH, DK_TEXTOFFSET B$FLUSH ; Set vector to point to
						;	  real routine
	MOV	b$pTEST_CLOSE, DK_TEXTOFFSET B$TEST_CLOSE ; Set vector to
							; real routine
cEnd
sEnd	INIT_CODE

assumes CS,DK_TEXT
sBegin	DK_TEXT

;***
;B$DKERR
;Purpose:
;	Error processing for DK component.
;	Added with [3].
;
;Entry:
;
;Exit:
;
;Uses:
;
;Exceptions:
;
;****


cProc	B$DKERR,<NEAR>
cBegin
	or	b$FInput,InpDsk	; keep B$PEOS from blowing away stack by
				; mapping InpTTY to InpDsk.
	cCALL	B$PEOS		;process print end-of-stmt
cEnd

sEnd	DK_TEXT
	END
