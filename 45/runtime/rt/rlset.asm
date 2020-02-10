	TITLE	RLSET - RSET and LSET statements
	PAGE	56,132
;***
; RLSET - RSET and LSET statements
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - LSET Statement:
;
;      LSET stringvar = x$
;	 |
;      B$LSET
;
; - RSET Statement:
;
;      RSET stringvar = x$
;	 |
;      B$RSET
;
;******************************************************************************
	INCLUDE switch.inc	
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	DK_TEXT 	

	INCLUDE seg.inc 	

assumes CS,DK_TEXT		
sBegin	DK_TEXT 		

	externNP B$STDALCTMP	

	SUBTTL	B$RSET, B$LSET - RSET and LSET statements
	PAGE
;***
; B$RSET, B$LSET - RSET and LSET statements
;
;Function:
; Copy source to destination. If destination is larger than source,
; pad with blanks. RSET has leading blanks, LSET trailing blanks.
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement.
;
;Inputs:
; psdSource	= Address of source string descriptor
; pDest 	= far address of destination
; cbDest	= length of destination foixed length string, or 0 if sd
;
;Outputs:
; None.
;
;Registers:
; per convention
;
;******************************************************************************
cProc	B$RSET,<PUBLIC,FAR>,<ES,SI,DI> 
parmW	psdSource		
parmD	psdDest 		
parmW	cbDest			; length of destination string
cBegin				
	CALL	PREP		;Set up registers (source in SI, dest in DI)
	PUSH	AX		;Save count of characters to copy
	XCHG	AX,DX		;put blanks in AX
	REP	STOSW
	JNC	SKPSTO		;See if we need an odd number
	STOSB			;Put in that last blank
SKPSTO:
	POP	CX		;Count of characters to copy
	REP	MOVSB		;Must move bytes for case source=dest
POPSET:
	CALL	B$STDALCTMP	;Delete source if a temp
cEnd				

cProc	B$LSET,<PUBLIC,FAR>,<ES,SI,DI> 
parmW	psdSource		
parmD	psdDest 		
parmW	cbDest			; length of destination string
cBegin				
	CALL	PREP
	XCHG	AX,CX		;Get move count in CX - save fill in AX
	REP	MOVSB		;Must use bytes for case source = dest
	XCHG	CX,AX		;Get count of blanks to fill in CX
	XCHG	AX,DX		;put blanks in AX
	REP	STOSW
	JNC	POPSET		;See if an odd count
	STOSB			;Do that last byte
	JMP	SHORT POPSET	
cEnd	<nogen>


;***
;PREP - sets up registers for RSET and LSET
;
;Purpose:
; This routine sets up the registers for LSET and RSET.
;
;Entry:
; psdSource = Source string descriptor
; pDest     = Destination
; cbDest    = length of destination (0 if sd)
;
;Exit:
; [AX]	  = Number of byte to move from source to dest.
; [BX]	  = offset of source string descriptor
; [CX]	  = Number of blanks to pad (in words,zero if source size > dest size)
; [DX]	  = "  " 2 spaces for blank filling
; [ES:DI] = Destination data address
; [SI]	  = Source DS offset
;
;Uses:
; eveything it returns
;
;******************************************************************************
cProc	PREP,<NEAR>
cBegin

psdSource  equ word ptr [bp+12] 
pDest	   equ dword ptr [bp+8]  
cbDest	   equ word ptr [bp+6]	

	MOV	SI,psdSource	;get source sd
	PUSH	SI		; save it for later
	LODSW			;Get size of source string
	MOV	SI,[SI] 	;Get source data address (LODSW added 2 to SI)

	LES	DI,pDest	; [ES:DI] = address of dest data or sd
	MOV	CX,cbDest	; Get size of dest string
	OR	CX,CX		; if non-zer, we have what we need
	JNZ	PREP_5		; jump if ready
	MOV	CX,[DI] 	; else get size of variable length string
	MOV	DI,[DI+2]	; and ds offset (es assummed == ds)
PREP_5:
	SUB	CX,AX		;Number of blanks to pad with (dest - source)
	JAE	RETL		;If not negative, we're done
	ADD	AX,CX		;Set move count to size of dest string
	XOR	CX,CX		;Set blank fill to zero
RETL:				
	SHR	CX,1		;Make byte count a word count
	MOV	DX,"  " 	;2 blanks at a time
	POP	BX		; [BX] = source sd
cEnd				

sEnd	DK_TEXT 		
	END
