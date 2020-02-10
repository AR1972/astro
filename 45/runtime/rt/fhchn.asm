	TITLE	FHCHN - Far Heap CHAIN & SHELL Support
	PAGE	56,132
;***
;FHCHN.ASM - Far Heap CHAIN & SHELL for the BASIC 3 Common Runtime
;
;	Copyright (C) Microsoft Corp. 1988.
;
;*****************************************************************************

	SUBTTL	INCLUDES AND DEFINITIONS FOLLOWS

INCLUDE switch.inc
INCLUDE rmacros.inc

	USESEG	<INIT_CODE>	;Initialization
	USESEG	<BR_DATA>
	USESEG	<_DATA>
	USESEG	<_BSS>
	USESEG	<NH_TEXT>
	USESEG	<RT_TEXT>
	USESEG	<FH_TEXT>
	USESEG	<DV_TEXT>
	USESEG	<XIB>
	USESEG	<XI>
	USESEG	<XIE>
	USESEG	<BC_SAB>
	USESEG	<BC_SA>

INCLUDE seg.inc
INCLUDE idmac.inc
INCLUDE compvect.inc		
INCLUDE messages.inc
INCLUDE array.inc
INCLUDE smchain.inc
INCLUDE oscalls.inc
INCLUDE addr.inc
INCLUDE baslibma.inc
INCLUDE string.inc
INCLUDE stack2.inc

	externNP	B$ERR_FHC	;bad memory block address


	INITIALIZER	B$xFHCHNINI	;put B$xFHCHNINI in initializer list


	SUBTTL	DATA DEFINITIONS

sBegin	BR_DATA


	externW __acmdseg	;C startup __psp

sEnd	BR_DATA

sBegin	_DATA

	externW b$shli_disp	;Shell initialization dispatch table
	externW b$shlt_disp	;Shell termination dispatch table

sEnd	_DATA

sBegin	_BSS

	externW b$FHDStart	;starting FHD for list

	externB b$FHDULNearData
	externB b$FHDULFarData



	externW b$FHDEnd	  ;ending FHD for list

	staticW ShellRestore,,1 ;FH growth on shell restoration

	externW b$NH_first	;NHINIT - starting offset of near heap
	externW b$NH_last	;NHINIT - ending offset of near heap



sEnd	_BSS

sBegin	DV_TEXT 		
	externNP B$NearRet	;for disp vectors in compvect.inc
sEnd	DV_TEXT 		


sBegin	NH_TEXT
	externNP B$NHMOV	;move near heap
sEnd	NH_TEXT


sBegin	FH_TEXT

	externNP B$FHSelect
	externNP B$FHMemDosCall
	externNP B$FHCompact
	externNP B$FHLowerTop
	externNP B$FHRaiseTop
	externNP B$FHLowerBottom
	externNP B$FHRaiseBottom

sEnd	FH_TEXT

	PAGE
	SUBTTL	Far Heap Initialization

assumes CS,INIT_CODE
sBegin	INIT_CODE

;***
;B$xFHCHNINI - Far Heap initializer for chain/shell module
;PLM B$xFHCHNINI()
;
;Purpose:
;	Added with revision [1].
;	Initializer for chain/shell module of the Far Heap component.
;	This routine is called by the Crt0 startup before _main is
;	called.  It will update the indirect dispatch tables for the
;	far heap routines needed by chain/shell.  This insures that
;	the only time these routines are accessed is when this module
;	is linked into the user program.
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
;****************************************************************************

cProc	B$xFHCHNINI,<FAR>
cBegin


;	update SHELL time initialization dispatch address to FHShlIni

	MOV	WORD PTR [b$shli_disp].FH_SIVEC,FH_TEXTOFFSET FHShlIni

;	update SHELL time termination dispatch address to B$FHShlTerm

	MOV	WORD PTR [b$shlt_disp].FH_STVEC,FH_TEXTOFFSET B$FHShlTerm

cEnd

sEnd	INIT_CODE

sBegin	FH_TEXT
ASSUMES CS, FH_TEXT

	PAGE
	SUBTTL	Far Heap CHAIN Support

;***
; B$FHShlTerm - Pre SHELL "termination" (DOS 3)
;
;Purpose:
;	In preparation for the SHELL statement, compress both the
;	far and near heaps to the lowest memory possible.  Note the
;	shrinkage amount for restoration after the SHELL executes.
;Entry:
;	None
;Exit:
;	ShellRestore - contains amount released.
;Uses:
;	None.
;Exceptions:
;	Fatal error due to far heap inconsistency.
;******************************************************************************

cProc	B$FHShlTerm,<PUBLIC,NEAR> ; PUBLIC used to drag in file /O
cBegin

	MOV	AX,0FFFFH	;attempt maximum compression of FH
	CALL	B$FHLowerTop	;compress - AX returns amount done
	MOV	ShellRestore,AX ;save amount for shell restoration

cEnd

;***
; FHShlIni - Post SHELL memory recovery (DOS 3)
;
;Purpose:
;	After the SHELL statement has executed, grow the far heap to
;	its previous size.  Grow the near heap to its maximum amount.
;Entry:
;	ShellRestore - contains amout of memory to try to recover
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	Fatal error due to far heap inconsistency.
;******************************************************************************

cProc	FHShlIni,<NEAR>
cBegin
	MOV	AX,ShellRestore ;recall amount of compression before SHELL
	CALL	B$FHRaiseTop	;raise the size of FH to preSHELL level
	CALL	B$FHRaiseBottom ;raise the near heap to its maximum level
cEnd

;***
; B$FHClean  - Far heap clean for CHAIN statement (DOS 3 & 5 RTM)
;
;Purpose:
;	Far heap setup for CHAIN statement.  Cleans FH of entries
;	whose descriptors are not in COMMON.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************


;***
; FHClean - Clean FH of Non-COMMON entries (DOS 3 & 5 RTM)
;
;Purpose:
;	Clean far heap of entries whose descriptors are not
;	within COMMON.
;	If QBI, don't deallocate b$FHDULNearData and b$FHDULFarData.  These
;	two contain clean images of the user-lib preinited data (if UL loaded)
;	that are used to reinitialize the UL Data after CHAIN or RUN.
;
;Entry:
;	SI - ptr to FH descriptor to check
;Exit:
;	AX - 0 means don't deallocate entry
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************


;***
; B$FHPostChain - Far heap recovery for CHAIN statement (DOS 3 RTM)
;
;Purpose:
;	Finish DGROUP reconstruction after new program has been
;	chained.  The far and near heap have been recleaned if
;	COMMON truncation occurred.
;Entry:
;	BX = new starting offset of near heap.
;	CX = DGROUP saved area segment.
;	DS = ES = new program DGROUP segment.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	Error if near heap or far heap inconsistency.
;******************************************************************************


;***
;B$FHAdjPreChain - Adjust FH selectors and Give to Child process (DOS 5 RTM)
;
;Purpose:
;	This routine is used for DOS 5 CHAIN.  It will give all far
;	heap segments to the child process and adjust the descriptor chain.
;Entry:
;	CX = Child process ID
;Exit:
;	AX = DGROUP pointer to first FH descriptor in descriptor chain.
;Uses:
;	None.
;Exceptions:
;	None.
;*****************************************************************************


;***
;FHGiveSeg - give FH segment to child process (DOS 5 RTM)
;
;Purpose:
;	This routine is called once for each descriptor in the far heap
;	descriptor chain.  It will give the selector contained in the
;	descriptor to the chained process and update the selector in
;	the FH descriptor.
;Entry:
;	CX = Child process ID
;	SI = DGROUP offset of FH descriptor
;Exit:
;	AX = flag specifing whether or not to deallocate this entry
;		0 - means don't deallocate
;Uses:
;	Per Convention.
;Preserves:
;	CX,
;Exceptions:
;	None.
;******************************************************************************


;***
;B$FHAdjPostChain - Adjust FH selectors after CHAIN (DOS 5 RTM)
;
;Purpose:
;	This routine is used for DOS 5 CHAIN.  It will adjust all far
;	heap descriptors in the descriptor chain, deallocing any entries
;	truncated by the new COMMON bounds.
;Entry:
;	AX = Shared memory handle
;Exit:
;	None.
;Uses:
;	Per Convention.
;Exceptions:
;	None.
;*****************************************************************************

assumes	ES,DGROUP		

;***
;FHClean5 - Clean truncated COMMON FH entries (DOS 5 RTM)
;
;Purpose:
;	This routine is used for DOS 5 CHAIN.  It deallocates any far
;	entries with descriptors that are truncated by the new COMMON
;	boundaries.  The descriptors for these entries are removed
;	from the descriptor chain.
;Entry:
;	BX - size of new COMMON
;	DS - shared memory segment
;	ES - DGROUP
;	SI - Descriptor to check for cleaning
;Exit:
;	AX - 0 means don't deallocate entry
;Uses:
;	Per Convention.
;Preserves:
;	BX,CX
;Exceptions:
;	None.
;******************************************************************************


;***
; B$FHPreChain - Prepare far heap (and DGROUP) for CHAIN (DOS 3 RTM)
;
;Purpose:
;	Prepare the DGROUP for chaining.  This routine is entered
;	with both near and far heaps cleaned (nonCOMMON entries
;	removed) and the new stack defined.  The data segment is
;	truncated to RTM data, COMMON statics, a 200H-byte stack,
;	and compressed near and far heaps.  This data is placed in
;	high memory and the DS is adjusted.
;Entry:
;	AX - new DGROUP offset of near heap.
;Exit:
;	DS - new high data segment location.
;Uses:
;	AX, BX, CX, DX, SI, DI.
;Exceptions:
;	Errors due to near or far heap inconsistency.
;******************************************************************************


sEnd	FH_TEXT

	END
