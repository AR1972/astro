	TITLE	MTFLOAT - Floating call helper functions
;***
; MTFLOAT - Float call helper functions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	The majority of floating point math support for the runtime is via
;	'Float Calls', i.e., calls to floating point routines at a level
;	that utilize an 80[2]87 if present, and the emulator mathpack if not.
;	Functions in this module augment those float calls to provide
;	additional functionality not provided by the current float call
;	interface.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	;Runtime Macro Defintions

	USESEG	<DV_TEXT>	
	USESEG	MT
	USESEG	_DATA
	USESEG	_BSS

	INCLUDE seg.inc
	INCLUDE compvect.inc	; component dispatch vectors
	INCLUDE idmac.inc
	INCLUDE	rtps.inc	

sBegin	DV_TEXT 			
	externNP	B$NearRet	;for disp vectors in compvect.inc
sEnd	DV_TEXT 			

sBegin	_DATA
	externW	B$AC
	externW	B$DAC			
sEnd	_DATA

sBegin	_BSS
	externB	b$VTYP		
sEnd	_BSS


	externFP B$FIST		; IEEE round to nearest integer in DX:AX

sBegin	MT_TEXT
	ASSUMES CS,MT_TEXT

	externNP B$ERR_TM	

;***
;B$fmldw - push integer in BX onto top of numeric stack (ST0)
;Purpose:
;	This routine is used as an interface to __fldw when the
;	2-byte integer is already in a register.
;Entry:
;	BX contains the 2-byte integer to be put on the numeric stack.
;Exit:
;	ST0 contains floating point equivalent of the given integer.
;Preserves:
;	SI, DI, BX
;******************************************************************************
cProc	B$fmldw,<NEAR,PUBLIC> 	
cBegin				

	push	bx		; put integer in memory
	mov	bx,sp		; bx = address of integer
	fild	word ptr [bx]	; load integer
	pop	bx		; restore BX

cEnd				

;***
;B$fmlds - push s.p. number in CXDX onto top of numeric stack (ST0)
;Purpose:
;	This routine is used as an interface to __flds when the
;	4-byte real is already in registers.
;Entry:
;	CXDX contains the 4-byte s.p. number; the high byte is in CH,
;	low byte is in DL.
;Exit:
;	ST0 contains floating point equivalent of the given s.p. number.
;Preserves:
;	SI, DI, CX, DX
;******************************************************************************
cProc	B$fmlds,<NEAR,PUBLIC> 	
cBegin				

	PUSH	CX		;push high word first
	PUSH	DX
	MOV	BX,SP		;now BX points to s.p. number
	fld	dword ptr [bx]	; load number into ST0
	add	sp,4		; reset SP

cEnd				

;***
;B$ftolrup - Round ST0 up and pop, result in DX:AX
;Purpose:
;	This routine acts the same as __ftol, except the number is rounded
;	up instead of being truncated toward zero.
;Entry:
;	ST0 contains number to pop and round up.
;Exit:
;	ST0 is popped, rounded 32-bit integer equivalent in DX:AX
;Preserves:
;	SI, DI, BX, CX
;******************************************************************************
cProc	B$ftolrup,<NEAR,PUBLIC>	
cBegin				
	call	B$FIST		; temp (incorrect) rounding hack
cEnd				

;***
;B$ftolrnd - Round ST0 to nearest integer & pop, result in DX:AX
;Purpose:
;	This routine acts the same as __ftol, except the number is rounded
;	to the nearest integer (rounded to even number if equidistant) instead
;	of being truncated toward zero.
;Entry:
;	ST0 contains number to pop and round.
;Exit:
;	ST0 is popped, rounded 32-bit integer equivalent in DX:AX
;Preserves:
;	SI, DI, BX, CX
;******************************************************************************
cProc	B$ftolrnd,<NEAR,PUBLIC>	
cBegin				
	call	B$FIST
cEnd				


;***
;B$FRCINT - Force to integer routine compiler runtime interface routine
; Purpose:
;	   Convert contents of floating point accumulator to integer as
;	   follows:  Determine the type of variable contained in the FAC by
;	   checking the contents of b$VTYP.  If the variable is a string
;	   (b$VTYP = 3), issue a type mismatch error.  If b$VTYP is greater
;	   than 3, assume single precision and return the integer equivalent
;	   in BX.  If the b$VTYP is less than 3, assume integer and simply
;	   move the leftmost two bytes of the FAC contents to BX.
;
; Entry:
;	 FAC contains a variable, b$VTYP contains its type (integer is type 2,
;	 string is type 3, single precision is type 4, double precision is type
;	  8). Most often the variable in the FAC is a single precision number.
; Exit:
;	BX = integer result
;	If no error occurred, b$VTYP = 2.
; Preserves:
;	ES,SI,DI
;****
cProc	B$FRCINT,<NEAR,PUBLIC>	
cBegin				

	MOV	BX,OFFSET DGROUP:B$AC
	MOV	DL,[b$VTYP]	
	CMP	DL,VT_SD	; Is value type less than or equal to 3?
	JNB	ISFLPT		;No: assume it is single precision
	JZ	STRERR		;String: type mismatch error
	MOV	BX,[BX] 	;Put integer in BX
	RET
STRERR: 			;String: issue type mismatch error
	JMP	B$ERR_TM	;Type mismatch error routine
ISFLPT: 			;Floating pt number in accumulator
	PUSH	ES		;save across float calls
	PUSH	BX		;save pointer to B$AC
	CMP	DL,VT_R4	
	JNZ	FRC_DUBL	;brif vtyp not s.p.

	fld	dword ptr [bx]	; load s.p. number to ST0
	JMP	SHORT FRC_CONT
FRC_DUBL:
	DbAssertRelB DL,z,8d,MT_TEXT,<MTFLOAT.ASM: B$FRCINT: invalid var type>
	fld	qword ptr [bx]	; load d.p. number to ST0
FRC_CONT:
	CALL	B$ftolrnd	;DX:AX = integer equiv. of number


	AND	DX,08000H	; mask off all but sign bit
	OR	AX,DX		;mask sign bit into 2-byte integer

	POP	BX		;BX points to B$AC
	POP	ES
	MOV	[BX],AX 	;save integer in B$AC
	XCHG	AX,BX		;BX is retval
	MOV	[b$VTYP],VT_I2	; Make value type integer

cEnd				



sEnd	MT_TEXT
	END
