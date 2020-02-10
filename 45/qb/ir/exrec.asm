page	49,132
	TITLE	ExRec - Executors for records
;***
;exrec.asm - executors for records
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module contains all exOff other record-related executors.
;
;
;****************************************************************************

	.xlist
	include		version.inc
EXREC_ASM	= ON
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	extort
	IncludeOnce	pcode		
	IncludeOnce	scanner
	IncludeOnce	variable
	IncludeOnce	opid
	.list




GetElemOffset	MACRO
	xchg	ax,bx			;;BX = oElem
	mov	ax,[pVarBx].ELEM_oVar	;;AX = Offset of this element
	pop	bx			;;BX = Offset of record
	add	bx,ax			;;Add offset of this element
	ENDM





GetElemSeg  MACRO
	mov	dx,ds			;;Save DS in DX
	pop	ds			;;Segment of record
	ENDM






RestoreVarTable equ	<mov ds,dx>



assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA
sBegin	DATA				
sEnd	DATA				


sBegin	CODE

MakeExe exStEndType,opStEndType 	;Can't be executed
	DbHalt	CODE,<exStEndType>

MakeExe	exElemRef,opElemRef
	DbHalt	CODE,<exElemRef>

MakeExe	exStType,opStType
	LODSWTX				;Get offset of EndType
	inc	ax
	inc	ax			;Skip over EndType's link field
	xchg	si,ax			;Jump over type definition
	DispMac				;Dispatch to next executor

MakeExe exOffELdI2,opOffLd,ET_I2
	SkipExHeader
MakeExe exOffILdI2,opOffLd,ET_Imp
	LODSWTX 			;Get oElem
	GetElemOffset			;BX = Offset of this element
	GetElemSeg			;DX = psVarTable ife FV_SBSWAP
	push	[bx]			;Push integer onto stack
	RestoreVarTable 		;DS = psVarTable
	DispMac

MakeExe exOffELdI4,opOffLd,ET_I4
	SkipExHeader
MakeExe exOffILd4,opOffLd,ET_Imp
	LODSWTX				;Get oElem
	GetElemOffset			;BX = Offset of this element
	GetElemSeg			;DX = psVarTable ife FV_SBSWAP
	push	[bx+2]			;Push High Word/SB
	push	[bx]			;Push Low Word/Handle
	RestoreVarTable 		;DS = psVarTable
	DispMac




	;Added with [11]


MakeExe exOffELdR4,opOffLd,ET_R4
	SkipExHeader
MakeExe exOffILdR4,opOffLd,ET_Imp
	LODSWTX				;Get oElem
	GetElemOffset			;BX = Offset of this element
	GetElemSeg			;DX = psVarTable ife FV_SBSWAP
	fld	dword ptr [bx]
	RestoreVarTable 		;DS = psVarTable
	DispMac


MakeExe exOffELdR8,opOffLd,ET_R8
	SkipExHeader
MakeExe exOffILdR8,opOffLd,ET_Imp	
	LODSWTX				;Get oElem
	GetElemOffset			;BX = Offset of this element
	GetElemSeg			;DX = psVarTable ife FV_SBSWAP
	fld	qword ptr [bx]
	RestoreVarTable 		;DS = psVarTable
	DispMac

	;End of [11]

MakeExe exOffERfR8,opOffLd,ET_R8
	SkipExHeader
MakeExe exOffERfI4,opOffLd,ET_I4
	SkipExHeader
MakeExe exOffERfR4,opOffLd,ET_R4
	SkipExHeader
MakeExe exOffERfI2,opOffLd,ET_I2
	SkipExHeader
MakeExe exOffIRf,opOffLd,ET_Imp
	LODSWTX				;Get oElem
	xchg	bx,ax
	pop	ax			;Get current offset
	add	ax,[pVarBx].ELEM_oVar	; Get offset of element
	push	ax
	DispMac

MakeExe exOffEStI2,opOffSt,ET_I2
	SkipExHeader
MakeExe exOffIStI2,opOffSt,ET_Imp
	LODSWTX				;Get oElem
	GetElemOffset			;BX = Offset of this element
	GetElemSeg			;DX = psVarTable ife FV_SBSWAP
OffSt2: 				
	pop	[bx]			;Pop integer from stack
	RestoreVarTable 		;DS = psVarTable
	DispMac


MakeExe exOffEStI4,opOffSt,ET_I4
	SkipExHeader
MakeExe exOffISt4,opOffSt,ET_Imp
	LODSWTX				;Get oElem
	GetElemOffset			;BX = Offset of this element
	GetElemSeg			;DX = psVarTable ife FV_SBSWAP
OffSt4: 				
	pop	[bx]
	pop	[bx+2]			;Pop 4-byte number from stack
	RestoreVarTable 		;DS = psVarTable
	DispMac




	;Added with [11]


MakeExe exOffEStR4,opOffSt,ET_R4
	SkipExHeader
MakeExe exOffIStR4,opOffSt,ET_Imp
	LODSWTX				;Get oElem
	GetElemOffset			;BX = Offset of this element
	GetElemSeg			;DX = psVarTable ife FV_SBSWAP
OffStR4: 				
	fstp	dword ptr [bx]
	RestoreVarTable 		;DS = psVarTable
	fwait
	DispMac



MakeExe exOffEStR8,opOffSt,ET_R8
	SkipExHeader			;Fall through to next executor
MakeExe exOffIStR8,opOffSt,ET_Imp	
	LODSWTX				;Get oElem
	GetElemOffset			;BX = Offset of this element
	GetElemSeg			;DX = psVarTable ife FV_SBSWAP
OffStR8: 				
	fstp	qword ptr [bx]
	RestoreVarTable 		;DS = psVarTable
	fwait
	DispMac


	;End of [11]

MakeExe exOffIStTyp,opOffSt,ET_Imp
	LODSWTX				;Get oElem
	add	ax,di			
	xchg	bx,ax			;bx = pElem
	xchg	ax,di			;Save di
	pop	di			;Current offset
	add	di,[bx].ELEM_oVar	;Add offset of element
	mov	bx,[bx].ELEM_oTyp

	public	MoveRec
MoveRec:
	;ax = saved di
	;bx = oTyp of record
	;di = offset of destination
	;[sp] = segment of destination
	;[sp+4]:[sp+2] = pointer to source
	add	bx,ax			;oTyp --> pTyp
	mov	cx,[bx].TYP_cbData	;Get length of this element
	mov	dx,es			;Save ES
	mov	bx,si			;Save SI
	pop	es			;ES:DI points to destination
	pop	si
	pop	ds			;DS:SI points to source
	shr	cx,1			;Move by words
	rep	movsw
	adc	cx,cx			;CX = 1 iff CY set
	rep	movsb			;copy odd byte if struct size odd
	mov	si,bx			;Restore SI
	xchg	ax,di			;Restore DI
	push	ss
	pop	ds			;Restore DS
	mov	es,dx			;Restore ES
	DispMac

	;Strings


public	exOffIStSd,exOffEStSd
public	exOffIRfSd,exOffERfSd
public	exOffILdSd,exOffELdSd

;Never referenced if not FV_FARSTR, but must be defined
exOffIStSd	=	0
exOffEStSd	=	0
exOffIRfSd	=	0
exOffERfSd	=	0
exOffILdSd	=	0
exOffELdSd	=	0





MakeExe exOffERfFS,opOffLd,ET_SD
	SkipExHeader
MakeExe exOffIRfFS,opOffLd,ET_Imp
	LODSWTX				;Get oElem
	xchg	bx,ax			;pElem to bx
	pop	ax			
	add	ax,[pVarBx].ELEM_oVar	; [3] Add offset of element
	push	ax			; Far address to stack
	push	[pVarBx].ELEM_cbFixed	; Get length
	DispMac

MakeExe exOffELdFS,opOffLd,ET_SD
	SkipExHeader			;Fall through to next executor
MakeExe exOffILdFS,opOffLd,ET_Imp
	LODSWTX				;Get oElem
	xchg	bx,ax			;pElem to bx
	pop	ax			
	add	ax,[pVarBx].ELEM_oVar	; [3] Add offset of element
	push	ax			; Far address to stack
	push	[pVarBx].ELEM_cbFixed	; Get length
	CALLRT	B$LDFS,DispMovSd	

MakeExe exOffEStFS,opOffSt,ET_SD
	SkipExHeader
MakeExe exOffIStFS,opOffSt,ET_Imp
	LODSWTX 			;Get oElem
	xchg	bx,ax			;pElem to bx
	pop	ax			
	add	ax,[pVarBx].ELEM_oVar	; [3] Add offset of element
	push	ax			; Far address to stack
OffStFS:				
	push	[pVarBx].ELEM_cbFixed	; Get length
	CALLRT	B$LSET,Disp


sEnd	CODE
end
