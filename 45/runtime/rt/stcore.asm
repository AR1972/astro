	TITLE	STCORE - Core string assignment, concatenate & compare
	PAGE	56,132
;***
;STCORE - Core string assignment, concatenate & compare
;
; Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; String Addition:	String Assignment:	String Comparision:
;
;   Example:		  Example:		  Example:
;
;     a$ = b$ + c$	    A$ = B$		    IF A$ = B$ THEN 100
;	      | 	       |			  |
;	  B$SCAT	    B$SASS		       B$SCMP
;
; CHR$ Function:
;
;   v$ = CHR$(numeric)
;	   |
;      B$FCHR
;
;
; This module contains the most commonly used "core" string routines.
;
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	;Runtime Macro Defintions

	useSeg	_DATA
	useSeg	ST_TEXT

	INCLUDE seg.inc
	INCLUDE idmac.inc

sBegin	ST_TEXT
	ASSUMES CS,ST_TEXT

externNP B$ERR_FC
externNP B$STALCTMP
externNP B$STALCTMPSUB
externNP B$STCHKTMP
externNP B$STDALC
externNP B$STDALCTMP
externNP B$STDALCTMPDSC


	SUBTTL	B$SASS - String assignment
	PAGE
;***
; B$SASS - String assignment
;DBCS-callback
;
;Function:
; 1. Copy the source string into a temp, if it wasn't already.
; 2. Free any string data space of the destination
; 3. Copy the source descriptor to the destination descriptor
; 4. Reset the backpointer (unless it's a null string)
; 5. Free the (temporary) source descriptor
;
;Inputs:
; psdSource = Address of Source string descriptor
; psdDest   = Address of Destination string descriptor
;
;Outputs:
; None.
;
;Registers:
; per convention
;******************************************************************************
cProc	B$SASS,<FAR,PUBLIC>
parmW	psdSource
parmW	psdDst
cBegin
	MOV	BX,psdSource
DbAssertTst	BX,Z,1,ST_TEXT,<Source SD has odd address in B$SASS>
	CALL	B$STCHKTMP	;See if source is a temp
	JNC	NOTEMP		;go if not
HAVTEMP:
	XCHG	BX,DX
	MOV	BX,psdDst	;Need destination string in BX
DbAssertTst	BX,Z,1,ST_TEXT,<Destination SD has odd address in B$SASS>
DbAssertRel	BX,NE,DX,ST_TEXT,<Source == Dest in B$SASS>
	CALL	B$STDALC	;Free destination string, if any
;Copy the descriptor
	XCHG	BX,DX		;BX=psdSource (or temp), DX=psdDest
	MOV	CX,[BX] 	;CX=length
	MOV	AX,[BX+2]	;AX=data pointer
	XCHG	BX,DX		;BX=psdDest, DX=psdSource (or temp)
	MOV	[BX],CX 	;save length
	MOV	[BX+2],AX	;save data pointer
	JCXZ	NOBACK		;if null str, don't fix backpointer
	XCHG	BX,AX		;BX=data pointer, AX=psdDest
	MOV	[BX-2],AX	;Set backpointer
	XCHG	BX,DX		;Need source (temp) string in BX
	CALL	B$STDALCTMPDSC ;Free temp header
NOBACK:

cEnd

NOTEMP: 			;Copy the source string into a temp
	XOR	CX,CX		;offset of 0
	MOV	DX,[BX] 	;DX = length of string to assign
	CALL	B$STALCTMPSUB	;Create temp string and copy in data
	JMP	SHORT HAVTEMP

	SUBTTL	B$SCAT - Concatenate strings
	PAGE
;***
; B$SCAT - Concatenate strings
;DBCS-callback
;
;Function:
; Create a string containing the concatenation of the two given strings.
;
;Inputs:
; psd1	= Address of left string descriptor
; psd2	= Address of right string descriptor
;
;Outputs:
; [AX]	= Address of result string descriptor
;
;Registers:
;
;******************************************************************************
cProc	B$SCAT,<FAR,PUBLIC>,<ES,DI,SI>
parmW	psd1
parmW	psd2
cBegin

	PUSH	DS		;string package assumes ES = DS
	POP	ES

	MOV	SI,psd1 	;First string to move
	MOV	DI,psd2 	;Save second string
	MOV	BX,[SI] 	;Get length of first string
	ADD	BX,[DI] 	;Sum is length of result string
	JO	ERRFC		;Make sure no overflow
	CALL	B$STALCTMP	;Allocate temp string
	XCHG	DI,DX		;Put data address in DI, save second string
	PUSH	BX		;Save result string descriptor address
	CALL	MOVSTR		;Move first string into temp
	MOV	SI,DX
	CALL	MOVSTR		;Move second string in
	POP	AX		;Restore result

cEnd

MOVSTR:
	MOV	BX,SI		;Need descriptor address here for B$STDALCTMP
	LODSW			;[AX] = length of string
	XCHG	AX,CX		;[CX] = length of string
	LODSW			;[AX] = address of data
	XCHG	AX,SI		;[SI] = address of data
	SHR	CX,1		;Convert to word count
	REP	MOVSW
	JNC	DITS		;Is is odd?
	MOVSB			;Move last byte if odd
DITS:
	JMP	B$STDALCTMP	;Delete source string if temp

ERRFC: JMP     B$ERR_FC

	SUBTTL	B$SCMP - String comparison
	PAGE
;***
;B$SCMP,B$ISCMPNoRel - String comparison
;
;Function:
; Compare strings and set flags. For each byte of the strings, the flags
; are set based on [left operand] - [right operand] until inequality is
; found or one the strings end. If the end a string is reached, the
; flags are set with [LEN(left operand)] - [LEN(right operand)].
; B$ISCMPNoRel will do the comparison without releasing the LEFT side
; string if it is a temp.
;
;Inputs:
; psdL	= Address of left operand string descriptor
; psdR	= Address of right operand string descriptor
;
;Outputs:
; Result is flags, as noted above.
;
;Registers:
; Per convention
;******************************************************************************
cProc	B$ISCMPNoRel,<FAR,PUBLIC>
cBegin	<nogen>
	MOV	DX,-1		;Don't dealloc flag
	JMP	SHORT SCMP
cEnd	<nogen>

cProc	B$SCMP,<FAR,PUBLIC>
cBegin	<nogen>
	XOR	DX,DX		;Dealloc flag
cEnd	<nogen>

cProc	SCMP,<FAR>,<ES,SI,DI>
parmW	psdL
parmW	psdR
cBegin

	PUSH	DS		;string package assumes ES = DS
	POP	ES

	MOV	BX,psdR
	MOV	AX,psdL
	MOV	DI,[BX+2]	;Pointer to right side data
	MOV	CX,[BX] 	;Length of right string
	XCHG	AX,BX
	MOV	SI,[BX+2]	;Pointer to left side data
	CMP	CX,[BX] 	;See which string is shorter
	JBE	HAVLEN
	MOV	CX,[BX] 	;Use whichever length is shortest
HAVLEN:
	CMP	AX,AX		;Set zero flag in case CX=0
	REPE	CMPSB		;String compare
	JNZ	NOTEQ		;Strings equal so far?
	MOV	CX,[BX] 	;String data equal, so get lengths again
	XCHG	AX,BX
	CMP	CX,[BX] 	;Longer string is larger
	XCHG	AX,BX		;Left side back in BX
NOTEQ:
	PUSHF			;Save comparison flags
	OR	DX,DX		;should we dealloc left side string temp?
	JNZ	NoDealc 	;brif not
	CALL	B$STDALCTMP	;Free up temp strings
NoDealc:
	XCHG	AX,BX
	CALL	B$STDALCTMP
	POPF			;Restore flags

cEnd

				; use the B$FCHR function which is
				; currently part of the KANJI sources.
	SUBTTL	B$FCHR - CHR$ function
	PAGE
;***
;B$FCHR - CHR$ function
;sd * pascal B$FCHR(I2 val)
;
;Function:
; Create a temp string 1 character (1 or 2 bytes) long with the given value.
;
;Inputs:
; val	= Value, which will be checked to be sure it's valid ASCII (<256)
;	  if Kanji supported, valid ASCII (<65536)
;
;Outputs:
; [AX]	= Address of string descriptor
;
;Registers:
; Per convention
;******************************************************************************
cProc	B$FCHR,<FAR,PUBLIC>
parmW	val
cBegin

	MOV	AX,val		;[AX] = value
	MOV	BX,1
	OR	AH,AH		;See if value OK (<256)

	JNZ	ERRFC		;If not, argument error

	PUSH	AX		;Save character on stack
	CALL	B$STALCTMP	;Allocate a one or two byte string
	XCHG	BX,DX		;Get pointer to data in BX
	POP	[BX]		;Put character in string
	XCHG	AX,DX		;[AX] = descriptor address
cEnd


sEnd	ST_TEXT

	END
