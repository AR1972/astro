	TITLE	STFREE - FRE String functions
;***
; STFREE - Free String function package
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	BASIC intrinsic FRE string function support.
;
; BASIC Syntax mapping to included runtime entry points:
;
; - FRE Function
;
;      v = FRE(arg)	   where 'arg' can be a numeric or a string
;
;    Examples:
;
;      v = FRE(1)		   v = FRE(a$)
;	    |				|
;	 B$FRI2		     B$FRSD
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	;Runtime Macro Defintions

	USESEG	_DATA		
	USESEG	FH_TEXT 	
	USESEG	ST_TEXT

	INCLUDE seg.inc
	INCLUDE stack2.inc	
	INCLUDE string.inc	
	INCLUDE idmac.inc	

sBegin	_DATA			
	externW b$pend		
sEnd				

sBegin	FH_TEXT

	externNP B$FHCompact	;FHINIT - compact far heap
	externNP B$FHByteSize	;FHINIT - get size of far heap after compaction

sEnd	FH_TEXT


sBegin	ST_TEXT

	externNP B$STDALCTMP	; deallocate if temp string
	externNP B$NHCPCT	
	externNP B$STGETFRESIZ	

assumes CS,ST_TEXT

;***
;B$FRSD - FREE function with string parameter
;
;Purpose:
; Perform a compaction on the string sb containing the input parm
; and return amount of free space in that string sb.
;
;Inputs:
; dummy = string desc
;
;Outputs:
; [DX:AX] =  size of free space
;
;Modifies:
; per convention
;
;******************************************************************************
cProc	B$FRSD,<FAR,PUBLIC>	
parmSD	dummy			; dummy sd (never -1, for fall)
cBegin				

	GetpSD	BX,dummy	; BX = psdDummy
	cCall	B$STDALCTMP	; deallocate it if it was a temp
	XOR	BX,BX		;clear flag for just NH compaction
	JMP	SHORT FRE_ENTRY ;perform NH compaction and return its size

cEnd	nogen			


;***
;B$FRI2 - FREE function with numeric parameter [5]
;
;Purpose:
; If BX is -1, the space in the FH heap is returned.
; If BX is -2, the unused stack space is returned.
; Else, returns the size of the next free block in string space.
;
; IF FV_FARSTR, "string space" is defined as the module-level/static string
; sb.
;
;Inputs:
; flag	= integer parameter
;
;Outputs:
; [DX:AX] =  size of free space
;
;Modifies:
; per convention
;
;******************************************************************************
cProc	B$FRI2,<FAR,PUBLIC>	
parmW	flag			; integer arg
cBegin

	MOV	BX,flag 	; get the flag
FRE_ENTRY:			;Common entry point for B$FRSD
	PUSH	SI		
	XOR	AX,AX		;set DX:AX to zero in case of...
	CWD			;...a non-far heap size is requested
	INC	BX		; test if FH space is to be returned
	JZ	FRE_FH		; if so, then do the FH compression
	INC	BX		; test if stack space to be returned
	JNZ	FRE_NOT_FH	; jump if not returning stack space

;
; FRE(-2): Determine available stack space. We do this by scanning from the
; bottom of the stack area [b$pend], looking for known data placed there at
; init/clear time. Once we find something other than that data, we have
; determined how much stack was used. The data expected tells us how much space
; was left.
;
FRE_STACK:			
	.erre	ID_SSEQDS	; we assume DS points at stack
	MOV	SI,[b$pend]	; start search for new data from _end
	MOV	BX,-STACK_MIN-2 ; for loop entry
FRE_STACK_LOOP: 		
	INC	BX		; move up in the stack
	INC	BX		
	CMP	BX,[SI] 	; is it what we expected?
	LODSW			; inc si by two
	JZ	FRE_STACK_LOOP	; loop until it isn't
	MOV	DX,-1		
	XCHG	AX,BX		; [AX] = # of bytes left
	CMP	AX,-STACK_MIN	; See if real bytes, or dipped into reserve
	JMP	SHORT FRE_EXIT_2; inc DX to zero if valid byte count

FRE_FH: 			
	CALL	B$FHCompact	;compact the far heap. SI = next to last FHD
	CALL	B$FHByteSize	;DX:AX = FH entry size

FRE_NOT_FH:
	CALL	B$NHCPCT	;compact all strings and heap
	CALL	B$STGETFRESIZ	;  [BX] =  free string size
	ADD	AX,BX		;add string size to FH size...
FRE_EXIT_2:			
	ADC	DX,0		;...propagate any carry
FRE_EXIT:			
	POP	SI		;recover original SI
cEnd				


sEnd	ST_TEXT

	END
