	TITLE	STINKEY - INKEY Function
	PAGE	56,132
;***
;STINKEY - INKEY Function
;
; Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;   INKEY$ Function:
;
;     v$ = INKEY$
;	     |
;	   B$INKY
;
; Includes the INKEY function. This is pulled out to isolate it from both the
; string and input packages, to avoid one or the other being called in if INKEY
; is NOT used, and they are not needed.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	;Runtime Macro Defintions

	useSeg	_BSS
	useSeg	_DATA
	useSeg	ST_TEXT

	INCLUDE seg.inc
	INCLUDE const.inc

sBegin	_DATA
externB b$IOFLAG		;Misc. IO flags.  Defined in GWINI.ASM

externW b$nuldes
sEnd	_DATA

sBegin	_BSS
externW b$PTRFIL		;defined in GOSTOP.ASM
sEnd	_BSS

externFP B$FCHR 		;CHR$ function

sBegin	ST_TEXT
assumes CS,ST_TEXT

externNP B$STDGET
externNP B$INKMAP
externNP B$STALCTMP
externNP B$TTYIN
externNP B$TTYST
externNP B$END			


	SUBTTL	B$INKY - INKEY$ function
	PAGE
;***
; B$INKY - INKEY$ function
; sd * pascal B$INKY(void)
;
;Function:
; If a character is waiting at the console, return it in a 1-char temp string.
; Otherwise, return null string descriptor.
;
;Inputs:
; None.
;
;Outputs:
; [AX]	= Address of string descriptor
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$INKY,<FAR,PUBLIC,FORCEFRAME>	; set up frame for error recovery
cBegin

INKGET: 			;If we have to get another character
				;check for redirection again.
	TEST	b$IOFLAG,RED_INP;Is input redirected ?
	JZ	NOT_REDIT	;Jump if not redirected
;
; Since redirected input should always have a character ready, INKEY$ will
; always return a character until the end of the redirected input file is
; reached. At this point, the program should gracefully exit.
;
	MOV	b$PTRFIL,0
	CALL	B$STDGET 	;[AL] = character from standard input
	jnz	contin		;got one, continue
	JMP	B$END		;End of redirected input seen, exit program
NOT_REDIT:

	CALL	B$TTYST 	;See if anything out there
	MOV	AX,OFFSET DGROUP:b$nuldes
	JZ	INKY_90 	;exit if not
	CALL	B$TTYIN		;read the char if there
contin:

	cCALL	B$INKMAP	;OEM's routine to map char
	JZ	INKGET		;OEM has no associated character
	JAE	inknrm		;branch if not 2-byte sequence

inkfun:
	CMP	AX,00FEH	;The only way [ax]=00feh for the
				;2-byte case is when one of the fn keys
				;is assigned to chr$(254)
	JE	inknrm		;If true, treat this as a single character
				;set equal to 254.
	XCHG	AL,AH		;Yes:Get in order for POP [BX] in chrsub
	PUSH	AX		;Save 2 characters
	MOV	BX,2		;Want a 2 character string
	CALL	B$STALCTMP	;Allocate a one-byte string
	XCHG	BX,DX		;Get pointer to data in BX
	POP	[BX]		;Put character in string
	XCHG	AX,DX		;[AX] = descriptor address
	JMP	SHORT INKY_90	;and exit

inknrm:

	XOR	AH,AH		;[AX] has character
	cCall	B$FCHR,AX	;[AX] has pointer to sd
INKY_90:

cEnd

sEnd	ST_TEXT

	END
