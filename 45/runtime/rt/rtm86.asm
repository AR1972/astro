	TITLE	RTM86 - 8086 Runtime Module Dispatcher

;***
; RTM86 - 8086 Runtime Module Dispatcher
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Contains the runtime module entry point for direct
;	execution which outputs the copyright messages.
;	It also contains the RTM software interrupt handler
;	and the runtime entry dispatch tables.
;
;******************************************************************************
INCLUDE switch.inc
INCLUDE rmacros.inc

	USESEG	<CONST>
	USESEG	<_DATA>
	USESEG	<_BSS>
	USESEG	<CDATA>
	USESEG	<_TEXT>
	USESEG	<RT_TEXT>


INCLUDE seg.inc

INCLUDE idmac.inc
INCLUDE intasg.inc
INCLUDE	intmac.inc
INCLUDE	files.inc
INCLUDE	addr.inc
INCLUDE	stack.inc
INCLUDE ulib.inc			

sBegin	_BSS

	externW 	b$pULVars	;ptr to shared data items
					;defined in User lib.

sEnd	_BSS

sBegin	_TEXT			
externW VecTbl				; Start of dispatch table
externW _TEXT_START			; Start of _TEXT entries
externW _TEXT_END			; Past end of _TEXT entries
sEnd	_TEXT			

assumes	CS,RT_TEXT
sBegin	RT_TEXT


; values saved in code segment:

staticW	SaveAX,,1		;saved AX register
staticW	SaveBX,,1		;saved BX register
staticW	SaveSI,,1		;saved SI register
staticW	SaveDS,,1		;saved DS register
staticD	RTMChainPtr,,1		; RTM interrupt chain pointer
staticB UserTableFlag,,1	;flag whether or not we're using user-table

;*** 
; B$RTMInstall - install the RTM interrupt vector
;
;Purpose:
;	To install the RTM interrupt vector by first saving the
;	current vector and then setting it to the new value.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	B$RTMInstall,<FAR,PUBLIC>,<AX,BX,DX,DS,ES>
cBegin
	PUSH	CS			; DS = code segment for SAVINT
	POP	DS			; storage and SETVEC
	SAVINT	RTMChainPtr,(W0__NM*4)	;save the vector
	SETVEC	W0__NM,RTMVector	;set vector to the new value
cEnd

;*** 
; B$RTMDeinstall - deinstall the RTM vector
;
;Purpose:
;	To deinstall the RTM interrupt vector by setting it
;	to its previous value.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	B$RTMDeinstall,<FAR,PUBLIC>,<AX,DX,DS>
cBegin
	RSTVEC	W0__NM,RTMChainPtr	;restore to the old vector
cEnd

;***
; RTMVector - Runtime module interrupt service routine.
;
;Purpose:
;	This routine is executed when a compiled (non /O) module
;	requests runtime support.  The routine address is looked
;	up in a table based upon the index postbyte (which may be
;	preceded by a flag postbyte indicating special handling).
;	If the requested routine is found, the far call to the
;	interrupt request is backpatched into a far call to the
;	requested routine and reexecuted.  If a valid request is
;	made that does not have a defined entry, an advanced
;	feature error is issued.
;
;	Before backpatch:
;	    <FAR CALL stubroutine> --> <INT + postbyte(s)> --> RTMVector
;
;	After backpatch:
;	    <FAR CALL realroutine>
;Entry:
;	[SS:SP] - far pointer to first post byte.
;Exit:
;	FAR CALL to the Interrupt is backpatched and execution is restarted
;	at the backpatched call.
;Uses:
;	None.
;Exceptions:
;	B$ERR_AFE.
;****

;	These two code fragments are out here so that the normal case (single
;	postbyte) can be a complete fall through for speed.  If either of
;	the double postbyte cases are encountered, the code will jump out
;	here to one of these fragments which will jump back when they are done.

Part2:	LODSB			; [AL] = second post byte
	INC	AH		; [AX] = index into table (adjusted)
	JMP	SHORT DoIt	; continue


DbPub	RTMVector
cProc	RTMVector,<FAR>		
cBegin	<nogen>			

	MOV	CS:[SaveAX],AX	;save AX, but not on stack
	MOV	CS:[SaveBX],BX	;...and BX
	MOV	CS:[SaveSI],SI	;...and SI
	MOV	CS:[SaveDS],DS	;...and DS
	POP	SI		;get return offset (third byte in call)
	POP	DS		;get return segment
	POPF			; enable ints

;	Some trickery for handling table indicies:
;	The RTM dispatch table is "missing" the entry corresponding to
;	255 because a postbyte of FF is a special flag, and IF NOT EI_QB
;	it is also "missing" the entry at 254 (another flag).  To allow
;	addressing the table as a contiguous block, indicies in part 1
;	of the table (single postbyte entries) are bumped up (as a side
;	effect of testing for flag values).  This makes the index set
;	contiguous but 2 bytes (or 4 IF NOT EI_QB) too high.  We adjust
;	for this by addressing 2 bytes (or 4) below the table base.

	MOV	BX,_TEXTOFFSET VecTbl - 2 ;base of table minus 2 bytes
	LODSB			; [AL] = dispatch table index or flag
	XOR	AH,AH		; [AX] = dispatch table index or flag
	INC	AL		; part 2 of table? (2 post bytes)
	JZ	Part2		; yes, go read second post byte


DoIt:	ADD	AX,AX		; [AX] = word offset+2 into dispatch table
	ADD	BX,AX		; [BX] = CS Pointer into dispatch table
	MOV	AX,SEG _TEXT	
	MOV	DS,AX		; [DS:BX] points to table
	MOV	AX,[BX] 	; [AX] = target offset
	POP	SI		
	POP	DS		; [DS:SI] = return address into USER code
	SUB	SI,5		; [DS:SI] = call instruction in USER code
	PUSH	DS		; restore return segment
	PUSH	SI		; stack far pointer will execute new far CALL
	MOV	[SI+1],AX	; backpatch OFFSET part of far call

;	Determine target segment for backpatch

	MOV	AX,RT_TEXTBASE	; [AX] = target segment
	CMP	BX,_TEXTOFFSET _TEXT_START ; past start of _TEXT entries?
	JB	RTMVEC_20	; Jump if not
	CMP	BX,_TEXTOFFSET _TEXT_END ; past end of _TEXT entries?
	JAE	RTMVEC_20	; brif so
	MOV	AX,_TEXTBASE	; _TEXTBASE = target segment

RTMVEC_20:			
	MOV	[SI+3],AX	

	MOV	DS,CS:[SaveDS]	;restore original DS
	MOV	SI,CS:[SaveSI]	;restore original SI
	MOV	BX,CS:[SaveBX]	;restore original BX
	MOV	AX,CS:[SaveAX]	;restore original AX
	RET			;far return to routine address just pushed

cEnd	<nogen>			



labelNP <PUBLIC,B$Undef1>	; This is a list of empty slots in
labelNP <PUBLIC,B$Undef2>	; the RTMINT table.  Remove them when
labelNP <PUBLIC,B$Undef3>	; the slot is re-used to eliminate
labelNP <PUBLIC,B$Undef4>	; confusion and extranious labels.

externNP  B$FrameFC		
	JMP	B$FrameFC	; Give "Illegal Function Call"


sEnd	RT_TEXT

	END
