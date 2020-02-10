	TITLE	OSCMD - Operating System Command line access

;***
;OSCMD.ASM - Operating System  command line access
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	This module contains routines to access and manipulate the
;	command line.  This is used by statements such as OPEN "PIPE:",
;	RUN/CHAIN/SHELL/COMMAND$.
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;
;	Code Segments
;
	USESEG	<OS_TEXT>	;Operating System
	USESEG	<NH_TEXT>	;Near Heap
	USESEG	<RT_TEXT>	;Runtime Core

;
;	Data Segments
;
	USESEG	<_DATA>
	USESEG	<_BSS>		;runtime data (uninitialized)

	INCLUDE seg.inc
	INCLUDE ascii.inc
	INCLUDE idmac.inc

	SUBTTL	Code Externals
	PAGE

sBegin	NH_TEXT
	externNP B$STALCTMPCPY
sEnd	NH_TEXT



sBegin	RT_TEXT
sEnd	RT_TEXT

	SUBTTL	Runtime data definitions for BASIC Operating System
	PAGE

sBegin	_BSS
	externW __acmdln	;command line pointer
	externB	b$Buf1		;defined in GWINI.ASM
sEnd	_BSS

	SUBTTL	Runtime Operating System  Initialization
	PAGE
assumes CS,OS_TEXT
sBegin	OS_TEXT

;***
;B$Arg0Skip - Skip past the program name on command line.
;Purpose:
;	The DOS 5 command line now has the program name as the
;	first element of the command line string.  This element
;	needs to be skipped for compatability in things like
;	Command$, RUN and CHAIN.
;
;	NOTE: this routine works for KANJI characters because all of
;	the items that we are testing (" ", 0, ASCCR) are less than
;	any possible second byte of a Kanji Character.
;
;Entry:
;	DS:SI - points to command line
;Exit:
;	DS:AX - points to command line past program name
;Modifies:
;	None.
;Exceptions:
;	None.
;*****************************************************************************

;***
;B$CmdCopy - copy DOS command line from DS:SI to ES:DI
;Purpose:
;	Copies the MSDOS command line into a buffer.  This routine is
;	called by COMMAND$, CHAIN, and RUN.
;Entry:
;	DS:SI - points to command line
;	ES:DI - points to place to put command line
;		For DOS 3, this is limited to 128 bytes, but OS/2 could
;		could have up to 256 bytes.
;Exit:
;	BX - count of chars copied
;	Command line is copied into ES:DI
;Modifies:
;	None.
;Exceptions:
;	None.
;***************************************************************************
cProc	B$CmdCopy,<PUBLIC,NEAR>,<SI,DI>
cBegin
	XOR	BX,BX		; initialize character count
CMD_IGNORE:
	LODSB			; fetch next char from command line
	CMP	AL," "
	JE	CMD_IGNORE	; ignore leading blanks
CMD_LOOP:
	OR	AL,AL		; 0 terminates command string
	JZ	CMD_TERM	
	CMP	AL,ASCCR
	JE	CMD_TERM	; CR terminates command string
	CMP	AL,"a"
	JB	CMD_COPY	; just copy if not lower case
	CMP	AL,"z"
	JA	CMD_COPY	; just copy if not lower case
	XOR	AL,20H		; convert lower case to upper case
CMD_COPY:
	STOSB			; copy char into temporary buffer
	INC	BX		; increment char count
DbAssertRelB	BH,E,0,OS_TEXT,<Buffer overflow in B$CmdCopy>
	LODSB			; fetch next char from command line
	JMP	SHORT CMD_LOOP	; repeat until end of command string
CMD_TERM:
cEnd

;***
;B$FCMD - return command line
;Purpose:
;	Implement COMMAND$ function by returning a string containimg
;	the MS-DOS command line.
;
;Inputs:
;	None.
;Outputs:
;	AX = ptr to string desc for temp with command line.
;Modifies:
;	None.
;Exceptions:
;	None.
;******************************************************************************


	externNP B$FrameAFE		

cProc	B$FCMD,<PUBLIC,FAR>		
cBegin	<nogen> 			
	jmp	B$FrameAFE		
cEnd	<nogen> 			

sEnd	OS_TEXT

	END
