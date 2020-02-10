	PAGE	56,132
	TITLE	GRSPACE - Compute space left in an array
;***
; GRSPACE - Compute space left in an array
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;
;	Graphics support routines for computing space left in an array.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	useSeg	_BSS
	useSeg	GR_TEXT
	useSeg	FH_TEXT 

	INCLUDE seg.inc
	INCLUDE array.inc

sBegin	_BSS

externB b$HugeShift		;OS dependent selector increment

sEnd	_BSS

sBegin	FH_TEXT
externNP B$ADArraySize
sEnd	FH_TEXT

sBegin	GR_TEXT
assumes CS,GR_TEXT

externNP B$ERR_BS


	SUBTTL	B$ComputeSpace - Compute space left in an array.
	PAGE
;*** 
; B$ComputeSpace - compute space remaining in array
;
;Purpose:
;	Given an array descriptor and an address into the array,
;	compute the space, in bytes, from the address to the
;	end of the array.
;
;Entry:
;	fpData	= array address (possibly after the array starting address)
;	pAd	= array descriptor
;Exit:
;	[DX:AX] = array space remaining in bytes
;	[ES:SI] = start address to use
;Uses:
;
;Exceptions:
;	to B$ERR_BS
;******************************************************************************

cProc	B$ComputeSpace,<PUBLIC,NEAR>
parmD	fpData			;far ptr to start of array data
parmW	pAd			;pointer to array descriptor of interest
cBegin
	MOV	BX,pAd		;[BX] = pointer to array descriptor
	MOV	AX,[BX].AD_fhd.FHD_hData ;get seg from array desc
	CWD			
	OR	AX,DX		;was seg 0 (array unallocated)?
	JZ	CompSpaceExit	;yes, exit with DX:AX = 0
	cCall	B$ADArraySize	;[DX:AX] = size of the array in bytes
	PUSH	DX		
	PUSH	AX		;save that
	LES	SI,DWORD PTR [BX].AD_fhd.FHD_oData ;[ES:SI] = base array address

	MOV	DX,Seg_fpData
	MOV	AX,Off_fpData	;[DX:AX] = place to put data in array

	OR	DX,DX		;Is there a specified place?
	JNZ	ComputeSpace	;brif so
	MOV	DX,ES		;else use array base address
	MOV	AX,SI

ComputeSpace:
	PUSH	DX		
	PUSH	AX		;save array address


;	Convert seg:offset pairs into 20 bit addresses and subtract
;	them to compute 20 bit offset from base of array.

	cCall	ConvToAddr	;convert seg:off in [DX:AX] to 20 bit addr
	PUSH	BX		;in [BX:AX]
	PUSH	AX		;save address of get data
	XCHG	AX,SI
	MOV	DX,ES		;[DX:AX] = array base seg:offset
	cCall	ConvToAddr	;[BX:AX] = converted 20 bit address
	XCHG	AX,CX		;[BX:CX] = array base address
	POP	AX
	POP	DX		;[DX:AX] = array get address
	SUB	AX,CX		;low offset
	SBB	DX,BX		;[DX:AX] = 20 bit offset from array base
	JC	OVERFL		;error if get addr < array base addr
	POP	SI		
	POP	ES		;[ES:SI] = address for get data
	POP	BX		
	POP	CX		;[CX:BX] = size of array
	SUB	BX,AX		;[BX] = low difference
	SBB	CX,DX		;[CX:BX] = room left for get data
	XCHG	CX,DX		;swap so that
	XCHG	BX,AX		;    [DX:AX] = space remaining
	JC	OVERFL		;Jump if indexed off of array end
CompSpaceExit:			
cEnd


OVERFL: JMP	B$ERR_BS

;***
; ConvToAddr - converts a segment offset pair to 20 bit address
;
;Purpose:
; Converts the segment offset pair in DX:AX to a 20 bit address in BX:AX.
; Added for DOS 3 indexed GETs.
;
;Entry:
; DX:AX  = segment offset pair to be converted.
;
;Exit:
; BX:AX  = 20 bit address
;
;Uses:
; Per convention
;
;Exceptions:
; None.
;
;******************************************************************************
cProc	ConvToAddr,<NEAR>
cBegin
	XOR	BX,BX
	MOV	CX,4
SegLoop:
	SHL	DX,1
	RCL	BX,1
	LOOP	SegLoop 	;[BX] = high 4 bits of seg
	ADD	AX,DX		;add low 4 bits of seg to high 4 bits of offset
	ADC	BX,CX		;[BX:AX] = 20 bit address (CX=0 from LOOP)
cEnd

sEnd	GR_TEXT
	END
