	TITLE	DYNAMIC - Dynamic array support
	PAGE	56,132
;***
; DYNAMIC.ASM - Dynamic array support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - DIM/REDIM Statement - Generates runtime call if $DYNAMIC was specified:
;
;	B$DDIM(hi 1, lo 1,... hi n, lo n, element size, ndims+typ<<8, pAd)
;	B$RDIM(hi 1, lo 1,... hi n, lo n, element size, ndims+typ<<8, pAd)
;
; - Dynamic array access routine - one call:
;
;	B$HARY(index 1, ..., index n, nindex) with BX = pointer to AD
;
; - ERASE Statement - generates one call:
;
;      ERASE arrayname {,arrayname}
;
;	B$ERAS(array desc)
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	;Runtime Macro Defintions

	useSeg	_DATA		
	useSeg	_BSS		
	useSeg	FH_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE array.inc	;far heap and array descriptor structures
	INCLUDE pointers.inc	;pointer reference macros
	INCLUDE baslibma.inc	
	INCLUDE nhutil.inc
	INCLUDE idmac.inc	

sBegin	_BSS			
	externB	b$HugeShift	;OS Selector increment for HUGE access
	externW	b$Buf1		; temporary buffer
sEnd	_BSS




sBegin	FH_TEXT 		
assumes CS,FH_TEXT		
;
;	Dynamic array runtime support
;


externNP B$FHAlloc		;FHINIT - far heap allocation
externNP B$FHDealloc		;FHINIT - far heap deallocation
externNP B$LHALC_CPCT		; compact heap and allocate heap entry
externNP B$LHDALC		; deallocate heap entry and compact heap
externNP B$LH_CPCT		; compact heap
externNP B$STDALC
externNP B$FHTestRaiseBottom	; attempt to reclaim DGROUP from FH
externNP B$ADArraySize		; Compute array size

externNP B$ERR_BS		;bad subscript error
externNP B$ERR_DD		;double dimension error


	SUBTTL	B$DDIM & B$RDIM - dimension & redimension
	PAGE
;***
; B$ADIM - DIM a dynamic array
; I4 pascal B$ADIM(I2 lo1, I2 hi1, ..., I2 loN, I2 hiN, I2 cbelem,	
;		      U2 ndims+typ<<8, ad *pAd)
;
; B$DDIM - DIM a dynamic array
; void pascal B$DDIM(I2 lo1, I2 hi1, ..., I2 loN, I2 hiN, I2 cbelem,	
;		      U2 ndims+typ<<8, ad *pAd)
;
; B$RDIM - REDIM a dynamic array
; void pascal B$RDIM(I2 lo1, I2 hi1, ..., I2 loN, I2 hiN, I2 cbelem,	
;		      U2 ndims+typ<<8, ad *pAd)
;
;Purpose:
; Runtime Entry Point. DIM Statement for dynamic arrays. If the array is
; already defined, an error is returned. REDIM Statement for dynamic arrays. If
; the array is already defined, it is released and then reallocated.
;
; B$ADIM performs all the functions of B$DDIM, except it does not actually
; allocate space for the array. It is used to fill in an array descriptor, and
; return the size of the array. [14]
;
; NOTE: In the interpeter (QB), the pointer to the array descriptor is actually
; a pointer into the variable heap. This heap cannot move during this
; operation.
;
;Inputs:
; lb	= lower bound for dimension n (lo1 through loN, above)
; ub	= upper bound for dimension n (hi1 through hiN, above)
; cbelem= element size
; ndtyp = number of dimensions (byte) & flags
; pAd	= pointer to array descriptor
;		(FV_LONGPTR only: pAd is a long ptr to the Ad)
;
;Outputs:
; [DX:AX] = resulting size of the array (B$ADIM only)
; Input parameters are removed from the stack.
;
;Modifies:
; Per convention
;
;*****************************************************************************
cProc	B$ADIM,<FAR,PUBLIC>	
cBegin	nogen			
	MOV	AL,1		; non-zero to indicate dim
	SKIP	2		; skip next instruction
cEnd	nogen			; fall into B$RDIM

cProc	B$RDIM,<FAR,PUBLIC>	
cBegin	nogen			
	XOR	AL,AL		; zero to indicate re-dim
	SKIP	2		; skip next instruction
cEnd	nogen			; fall into B$DDIM

cProc	B$DDIM,<FAR,PUBLIC>	
cBegin	nogen			
	MOV	AL,0FFH 	; flag to indicate dim
cEnd	nogen			


cProc	DIM_COMMON,FAR		
parmW	lb			;[4] lower bound for dimension n
parmW	ub			;[4] upper bound for dimension n
parmW	cbelem			; element size
parmW	ndtyp			; number of dimensions (byte) & flags
parmW	pAd			; pointer to array descriptor
cBegin				


	PUSH	SI		
	PUSH	DI		

	OR	AL,AL		; see who we were called as
	CBW			; [AX] = entry type
	XCHG	AX,DI		; [DI] = entry type (NOTE: Used way below)
	JNZ	BDDIM_5 	; Jump if dim (don't erase first)

;
;	Erase the present array if allocated.
;
	cCall	<FAR PTR B$ERAS>,pAd ; call runtime routine to erase array
;
;	Test if array is already allocated.  If so, clean the stack and
;	process the error.
;
BDDIM_5:			
	mov	bx,pAD		
	CMP	[bx].AD_fhd.FHD_hData,0    ;test if AD segment is zero
	JZ	BDDimNotAlloc	;if so, then not allocated, continue
	JMP	B$ERR_DD	;jump to double-dimensioned array error

;	Array is not allocated.  Fill in the AD from the stack variables.

BDDimNotAlloc:
	MOV	CX,ndtyp	; get number of dimensions and flags
	MOV	WORD PTR [bx].AD_cDims,CX  ;put flags, number of dims in AD
	MOV	AX,cbelem	; get size of an element in bytes
	MOV	[bx].AD_cbElement,AX	;and also put into AD
	XOR	CH,CH		;leave number of dimensions in CX
	LEA	SI,ub		;[4] point at lb entry of last index def
;
;	For each dimension, move the lower bound and compute the count
;	from the information on the stack.
;
;	[SI]			 -> upper bound of dimension on stack	
;	[SI+2]			 -> lower bound of dimension on stack	
;	ds:[bx].AD_tDM.DM_cElements -> count of elements of dimension in AD
;	ds:[bx].AD_tDM.DM_iLbound   -> lower bound of dimension in AD
;
	PUSH	BX		;save registers during move
	XOR	DX,DX		; [DX] = offset adjustment
BDDimLoop:
	lods	word ptr DGROUP:[si]	;get upper bound of current dimension
	SUB	AX,DGROUP:[SI] 	;subtract lower bound to count less 1
	JS	BDDimBadSubscript ; if lower > upper, bad subscript
	INC	AX		;increment to get real count of dimension
	MOV	[bx].AD_tDM.DM_cElements,AX ;put count of dimension into AD
	MUL	DX		; [AX] = offset adjustment * cElements
	XCHG	AX,DX		; [DX] = offset adjustment
	lods	word ptr DGROUP:[si]	;get lower bound of current dimension
	MOV	[bx].AD_tDM.DM_iLbound,AX  ;put into lower bound in AD
	SUB	DX,AX		; update offset adjustment
	ADD	BX,SIZE DM	;move AD pointers to next dimension entry
	LOOP	BDDimLoop

	POP	BX		;restore registers...
	XCHG	AX,DX		; [AX] = offset adjustment
	MUL	[bx].AD_cbElement	; Account for element size
	MOV	[bx].AD_oAdjusted,AX	; Store offset adjustment
;
;	With the information now in the AD pointed by BX, compute the
;	size of the array to allocate.
;
	CALL	B$ADArraySize	; compute the size in DX:AX
DJMP	JC	BDDimBadSubscript ;if too large, then give bad subscript error
        MOV     [bx].AD_fhd.FHD_cPara,AX; save byte count
	DEC	DI		; [DI] = entry type-1
	JZ	BDDimExit	; B$ADIM? if so, then go exit.
;
;	Jump if array is huge.	For a near or far array, give a bad
;	subscript error if the size is 64K or more.
;

	MOV	[bx].AD_fhd.FHD_oData,size AHD	; default offset

	TEST	[bx].AD_fFeatures,FADF_HUGE	;test if array is huge
	JNZ	BDDimHuge	;if huge, then jump
	CMP	DX,1		; byte count < 64K?
	JB	Less64K		; brif so -- value ok
	JA	BDDimBadSubscript ; brif > 64K -- give bad-subscript error
	OR	AX,AX		; byte count = 64K?
	JNZ	BDDimBadSubscript ; brif not -- give bad-subscript error
Less64K:			
;
;	Jump if array is far.  For a near array, allocate through the
;	near heap manager and jump to exit.
;
	TEST	[bx].AD_fFeatures,FADF_FAR ;test if array is far
	JNZ	BDDimAlloc	;if far, then just allocate directly
	MOV	CX,BX		;get array descriptor offset
	MOV	DL,LH_ARRAY	;set near heap entry type
	XCHG	BX,AX		;get size in bytes of entry to allocation
	CALL	B$LHALC_CPCT	; compact heap and allocate heap entry
	MOV	BX,CX		;get array descriptor pointer back
	MOV	[bx].AD_fhd.FHD_hData,DGROUPSEG ;save DGROUP segment/SB
						;  in descriptor
	MOV	[bx].AD_fhd.FHD_oData,SI   ;save base offset in descriptor
	ADD	[bx].AD_oAdjusted,SI	   ;Save adjusted offset
	JMP	SHORT BDDimExit ;jump to exit routine
;
; Place in center for relative jumps
;
BDDimBadSubscript:
	JMP	B$ERR_BS	;jump to bad-subscript error



;
;	Array is huge.	Determine 64K MOD <element-size> to compute
;	the array offset. (Value is remainder of integer divide of
;	64K by the element size.)
;
BDDimHuge:
	OR	DX,DX		; skip offset calc for arrays < 64k
	JZ	BDDimAlloc	
	PUSH	AX		;save size of allocation now in...
	PUSH	DX		;DX:AX since they are used by DIV
	XOR	AX,AX		;load 64K into DX:AX - 0 in AX...
	CWD			
	INC	DX		;...and 1 in DX
	DIV	[bx].AD_cbElement  ;divide 64K in DX:AX by element size
	MOV	[bx].AD_fhd.FHD_oData,DX ;move remainder into the AD offset
	ADD	[bx].AD_oAdjusted,DX	;Save adjusted offset
	OR	DX,DX		;test if remainder (MOD) is zero
	POP	DX		;restore array byte size...
	POP	AX		;in DX:AX
	JZ	BDDimAlloc	;if remainder was zero, then just allocate
	ADD	AX,[bx].AD_fhd.FHD_oData   ;make room for alignment
	ADC	DX,0		
	CMP	DX,1		;test if array was less than 128K
	JA	BDDimBadSubscript ;if 128K or more and nonzero offset, then err

BDDimAlloc:
	CALL	B$FHAlloc	;allocate FH entry of size DX:AX at desc BX

BDDimExit:
	MOV	CX,ndtyp	; CL = number of dimensions on stack
	XOR	CH,CH		; Clear high byte
	SHL	CX,1		; number of parameter bytes <dim #>*4
	SHL	CX,1		
	ADD	CX,6		; space for rest of parms
	MOV	b$Buf1,CX	; save # of bytes of parms to clear

	POP	DI		; restore registers
	POP	SI		

cEnd	nogen			

	JMP	CleanStack	; clean up stack and return

HAryErrorPopBP: 		
	POP	BP		;get back frame pointer (must be pushed last)
HAryError:			
	JMP	B$ERR_BS	;process bad subscript error

	SUBTTL	B$HARY - compute huge array element pointer
	PAGE
;***
;B$HARY - compute huge array element pointer
;void pascal B$HARY(BX: ad* pAd, i1, ..., iN, ci)
;
;Purpose:
; Runtime entry point. With the array descriptor and indices given, compute the
; segmented pointer to the huge array element.
;
; NOTE: In the interpeter (QB), the pointer to the array descriptor is actually
; a pointer into the variable heap. This heap cannot move during this
; operation.
;
;Entry:
; [BX]	= offset of array descriptor
; iN	= element index
; ci	= count of element indecies
;
;Exit:
; ES:BX = far pointer to array element.
;
;Uses:
; None.
;
;Preserves:
; AX,CX,DX	(Compiler requirement)
;
;Exceptions:
;	Error for unallocated array, bad subscript, or index number
;	inconsistency.
;******************************************************************************
cProc	B$HARY,<FAR,PUBLIC>	
parmW	iNdecies		; indecies
parmW	ci			; count of indecies
cBegin				
	PUSH	AX
	PUSH	CX
	PUSH	DX
	PUSH	SI
	PUSH	DI

;
;	SI:DI are used to accumulate the 32-bit offset of the array element.
;	CX is the loop counter for each index of the array access.
;	BX points to within the array descriptor.
;	BP points to within the stack frame.
;	DX:AX are used for intermediate calculations (including MUL).
;
;	Clear the offset accumulation in SI:DI and get and check the
;	number of indices in the array.
;
	XOR	SI,SI		;clear the running...
	MOV	DI,SI		;...accumulator for the offset
	MOV	CX,ci		; get number of indices from stack
	CMP	CL,[BX].AD_cDims ;test against dimensions in the AD
	JNE	HAryError	;if not the same, then jump to error
;
;	Within the offset calculation loop for each index, BP points
;	to the current index in the stack frame and BX points to the
;	current dimension count in the array descriptor.  Indices are
;	processed from last to first.
;
	PUSH	BX		;save registers again...
	PUSH	BP		;(save BP last for error routines)

	ADD	BP,8		;point to last index in the frame
	ADD	BX,AD_tDM	; points to first DM, last dim count...
	JMP	SHORT HAryStart ;jump to start within the loop
;
;	Calculation loop.  Adjust BP and BX to point to the next
;	index in the array.
;
HAryLoop:

	INC	BP		;point BP to next word...
	INC	BP		;...up the frame on the stack
	ADD	BX,SIZE DM	; point BX to the next index in the AD

;	Multiply SI:DI by the new dimension count.  Since each
;	index is in legal range in the AD, overflow is not possible.

	MOV	AX,DI		;get low word of accumulator
	MUL	[BX].DM_cElements ;multiply by current dimension count
	MOV	DI,AX		;put low word of product in accumulator
	MOV	AX,SI		;get high word of accumulator
	MOV	SI,DX		;accumulator SI:DI has first product
	MUL	[BX].DM_cElements ;multiply high word by count
	ADD	SI,AX		;add low word to high word of accumulator

;	Start within loop.  Get the array index and check if it is
;	within the bounds specified in the AD.	If not, report an error.

HAryStart:
	MOV	AX,[BP] 	;get the array index
	SUB	AX,[BX].DM_iLbound ;subtract lower bound to get index offset
	JL	HAryErrorPopBP	;if negative, then error
	CMP	AX,[BX].DM_cElements ;test if equal or larger than count
	JGE	HAryErrorPopBP	;if so, then error

;	Add array index offset to the accumulator in SI:DI.
;	Loop if more dimensions to process.

	ADD	DI,AX		;add index offset to DI...
	ADC	SI,0		;...and propagate carry to SI
	LOOP	HAryLoop	;loop on CX if more to process

	POP	BP		;restore registers saved before loop
	POP	BX

;	Finish the offset calculation by multiplying by the element
;	size and adding the AD offset.

	MOV	AX,DI		;get low word of accumulator
	MUL	[BX].AD_cbElement ;multiply by element byte count
	MOV	DI,AX		;put low word of product in accumulator
	MOV	AX,SI		;get high word of accumulator
	MOV	SI,DX		;accumulator SI:DI has first product
	MUL	[BX].AD_cbElement ;multiply high word by count
	ADD	SI,AX		;add low word to high word of accumulator

	ADD	DI,[BX].AD_fhd.FHD_oData ;add offset to accumulator...
Finish: 			
	ADC	SI,CX		;...and propagate the carry (CX=0 from LOOP)

;	From the 32-bit offset in SI:DI, compute the far pointer in ES:BX.

	GETSEG	AX,[BX].AD_fhd.FHD_hData,BX	;get segment from the AD
	OR	AX,AX		;test if segment is zero
	JZ	BadSubError	;if so, then array is unallocated
	MOV	CL,b$HugeShift ;get OS dependent shift count...
;	MOV	CL,12		;conversion count from 64K to 16...
	SHL	SI,CL		;...to calculate segment
	ADD	AX,SI		;segment is calculated (far ptr in AX:DI)

;	Move far pointer in AX:DI to ES:BX.

	MOV	ES,AX		;move segment of pointer
NearArray:			
	MOV	BX,DI		;move offset of pointer

	MOV	AX,ci		; AX = number of dimensions on stack
	INC	AX		; 1 extra parameter
	SHL	AX,1		; number of parameter bytes <dim #>*2 + 2
	MOV	b$Buf1,AX	; save # of bytes of parms to clear

	POP	DI		;restore registers
	POP	SI
	POP	DX
	POP	CX
	POP	AX

cEnd	nogen			; fall into CleanStack

;*** 
; CleanStack -- Common return for B$HARY and DIM_COMMON.  Added with [18].
;
;Purpose:
;	Clears variable number of bytes off the stack that are below BP and
;	a return address. Restores BP and jumps to the desired return address.
;
;Entry:
;	b$Buf1 = number of bytes to clear
;Exit:
;	None
;Uses:
;	b$Buf1
;Preserves:
;	All
;Exceptions:
;	None
;
;******************************************************************************
CleanStack:
	POP	BP		; restore BP
	POP	[b$Buf1+2]	; [b$Buf1+2] = far return address
	POP	[b$Buf1+4]
	ADD	SP,b$Buf1	; clean parameters from stack
	JMP	DWORD PTR [b$Buf1+2] ; return far

BadSubError:			
	JMP	B$ERR_BS	;process bad subscript error

	SUBTTL	B$LBND and B$UBND - LBOUND and UBOUND Functions
	PAGE
;***
; B$LBND and B$UBND - LBOUND and UBOUND Functions
; Added rev [11]
;
; Function:
;	Return the lowest or highest legal index for a
;	specified array dimension.
;
; NOTE: In the interpeter (QB), the pointer to the array descriptor is actually
; a pointer into the variable heap. This heap cannot move during this
; operation.
;
; Inputs:
;	pAd	ds relative array descriptor offset
;		(FV_LONGPTR only: pAd is a long ptr to the Ad)
;	iDim	1 relative index to array dimension
;
; Ouputs:
;	ax	lower or upper bound of the indicated dimension
;
; Registers:
;	none
;
;******************************************************************************

cProc	B$LBND,<FAR,PUBLIC>
cBegin	nogen
	XOR	CX,CX				;cx = 0 == LBOUND
	SKIP	2
cEnd	nogen					;fall OVER MOV CX,SP

cProc	B$UBND,<FAR,PUBLIC>
cBegin	nogen
	MOV	CX,SP				;cx <> 0 == UBOUND
cEnd	nogen					;fall INTO common routine

cProc	ULbound,FAR				;common routine to fall into
parmW	pAd					;pointer to array descriptor
parmW	iDim					;Index to desired dimension
cBegin

	MOV	DX,iDim
	mov	bx,pAd				;get pAD into register
	XOR	AX,AX				;Assume iDim out of range case
	CMP	[bx].AD_fhd.FHD_hData,AX	;test if segment is zero
	JZ	BadSubError			; Branch if unallocated
	SUB	DL,[BX].AD_cDims		;not this many dimensions?
	JA	BadSubError			; out of range: bad sub
						; treats negs as out of range
.errnz	(SIZE DM)-4
	NEG	DL				;Index to DIM info
						; Zero relative, always +
	SHL	DX,1				;To word offset
	SHL	DX,1				;2 word fields in DM
	ADD	BX,DX				;Index to dim iDim information
	MOV	AX,[BX.AD_tDM.DM_iLbound]	;Lower bound
	JCXZ	BOUNDX				;Return LBOUND
	ADD	AX,[BX.AD_tDM.DM_cElements]	;Count of elements
	DEC	AX				;To highest index
BOUNDX:

cEnd

	SUBTTL	B$ERAS - erase an array
	PAGE
;***
; B$ERAS - erase an array
; B$IErase - erase an array, but do not try to reclaim dgroup.
;
;Purpose:
; With the array descriptor given, erase the array.  For dynamic arrays, the
; space is deallocated. For static arrays, the array is only cleared. For
; static string arrays, the strings are deallocated.
;
; NOTE: In the interpeter (QB), the pointer to the array descriptor is actually
; a pointer into the variable heap. This heap cannot move during this
; operation.
;
;Entry:
;	pAd	ds relative array descriptor offset
;		(FV_LONGPTR only: pAd is a long ptr to the Ad)
;Exit:
;	None.
;Uses:
;	AX, BX, CX, DX.
;Exceptions:
;	None.
;******************************************************************************
cProc	B$ERAS,<FAR,PUBLIC>	
cBegin	nogen			


	MOV	AX,1		; nz to allow heap movement
	SKIP	2		

cEnd	nogen			

cProc	B$IErase,<FAR,PUBLIC>	
cBegin	nogen			

	XOR	AX,AX		; z to disable heap movement


cEnd	nogen			

cProc	ERASE_COMMON,FAR,<SI,DI>
parmW	pAd			
cBegin				

;	Get the array descriptor and test if array is static or dynamic.

	mov	bx,pAd			;get pAD into register
	mov	cx,[bx].AD_fhd.FHD_hData 
	jcxz	ErasReturn		 ; nothing to do if unalloc'd

	TEST	[bx].AD_fFeatures,FADF_STATIC ;test if a static array
	JNZ	ErasStatic		;if static, then jump

;	Dynamic array.	If not allocated, then just return.  Then test
;	if it is a string array.

	xor	si,si			
	MOV	[bx].AD_fhd.FHD_cPara,si;clear size field
	xchg	si,[bx].AD_fhd.FHD_hData;Get segment/SB, mark deallocated
	TEST	[bx].AD_fFeatures,FADF_SD;test if a string array
	JZ	ErasDynNumAry		;if not, dealloc dynamic numeric array

;	Dynamic string array.  Get the array offset and deallocate it.

	MOV	SI,[bx].AD_fhd.FHD_oData   ;get offset of array from AD
	PUSH	AX		; Save compaction flag
	CALL	B$LHDALC	; deallocate heap entry and compact heap
	POP	CX		; [CX] = compaction flag
	JCXZ	ErasReturn	
	CALL	B$LH_CPCT	; Compact when we're allowed to
	JMP	SHORT ErasReturn ;done - jump to return

;	Dynamic numeric array.	Deallocate it through its descriptor.

ErasDynNumAry:
	PUSH	AX		; Save movement flag
	cCall	B$FHDealloc	; deallocate the FH entry
	POP	CX		
	JCXZ	ErasReturn	; if movement not allowed, just return
	cCall	B$FHTestRaiseBottom ; Else try to reclaim DGROUP
	JMP	SHORT ErasReturn ;done - jump to return

;	Static array.  First get the base offset and compute its size.
;	Test if string or numeric.

ErasStatic:
	CALL	B$ADArraySize	; get the array size in bytes in DX:AX
	MOV	CX,AX		;put the size into CX
	MOV	DI,[bx].AD_fhd.FHD_oData   ;get the base offset from the AD
	TEST	[bx].AD_fFeatures,FADF_SD  ;test if string array
	JZ	ErasStatNumAry	;if not, then numeric and jump

;	Static string array.  Deallocate all strings referenced in
;	loop with BX pointing to each descriptor in the entry.

	SHR	CX,1		;divide byte count to get word count

	SHR	CX,1		;divide word count to get number of strings
	PUSH	BX		; Save array pointer
	MOV	BX,DI		;point to start of entry
ErasStatLoop:
	CALL	B$STDALC	;deallocate the string pointed by desc at BX
	ADD	BX,4		;point to the next descriptor
	LOOP	ErasStatLoop	;loop until done with entry
        MOV     CX,AX           ;get the length of entry back
	POP	BX		; get AD pointer back

;       Static array.  Clear the array entry of CX bytes at offset DI.

ErasStatNumAry:
	INC	CX		; Round up length if odd.
	SHR	CX,1		;Number of words to be cleared.
        XOR     AX,AX           ;value to clear entry
	GETSEG	ES,[bx].AD_fhd.FHD_hData,,<SIZE,LOAD>	;get seg from the AD
        REP     STOSW           ;clear the entry

;	Done - restore and return.

ErasReturn:

cEnd				

sEnd				
	END
