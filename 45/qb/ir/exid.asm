page	49,132
	TITLE	ExId	-	SImple Id Executors
;***
;exid.asm - executors for simple id references.
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module contains all exId executors.
;
;   Simple variables may also be referenced by exVtRfxx executors.
;   These are always nops, and are maintained with the nonspeed-
;   critical nop executors.
;
;   In general, these executors are very speed critical.
;
;
;****************************************************************************

	.xlist
	include 	version.inc
	IncludeOnce	architec
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	extort
	IncludeOnce	opaftqb4
	IncludeOnce	opid
	IncludeOnce	opstmt
	IncludeOnce	pcode		
	IncludeOnce	pointers
	IncludeOnce	qbimsgs
	IncludeOnce	rtinterp
	IncludeOnce	scanner
	IncludeOnce	variable
	.list

assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA



RestoreVarTable equ	<mov ds,cx>


sBegin	CODE

subttl	Public Load

;Added with [15]
;End of [15]

subttl	Static Load
MakeExe exIdISLd2,opIdLd,ET_Imp
	LODSWTX 			;Pick up variable address operand
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx
	push	[pVarBx]
	DispMac 			;Dispatch to next executor

MakeExe exIdESLdI2,opIdLd,ET_I2
	LODSWTX 			;Pick up variable address operand
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx
	push	[pVarBx]		;Push the I2
	DispMac 			;Dispatch to next executor

MakeExe exIdESLdI4,opIdLd,ET_I4
	SkipExHeader
MakeExe exIdISLd4,opIdLd,ET_Imp
	LODSWTX
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx
	push	[pVarBx+2]
	push	[pVarBx]
	DispMac


;Added with [18]
MakeExe exIdESLdR4,opIdLd,ET_R4
	SkipExHeader
MakeExe exIdISLdR4,opIdLd,ET_Imp
	LODSWTX
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx
	fld	dword ptr [pVarBx]
	DispMac

MakeExe exIdESLdR8,opIdLd,ET_R8
	SkipExHeader
MakeExe exIdISLdR8,opIdLd,ET_Imp 	
	LODSWTX
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx
	fld	qword ptr [pVarBx]
	DispMac
;End of [18]

subttl Common Load

MakeExe exIdECLdI2,opIdLd,ET_I2
	SkipExHeader
MakeExe exIdICLd2,opIdLd,ET_Imp
	LODSWTX 				;Get oVar
	xchg	ax,bx
	DbChk	oVar,bx 			; Check for valid oVar
	mov	ax,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,ax				;Offset in block
	push	[bx]
	DispMac


MakeExe exIdECLdI4,opIdLd,ET_I4
	SkipExHeader
MakeExe exIdICLd4,opIdLd,ET_Imp
	LODSWTX					;Get oVar
	xchg	ax,bx
	DbChk	oVar,bx 			; Check for valid oVar
	mov	ax,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,ax				;Offset in block
	push	[bx+2]
	push	[bx]
	DispMac



;Added with [18]
MakeExe exIdECLdR4,opIdLd,ET_R4
	SkipExHeader
MakeExe exIdICLdR4,opIdLd,ET_Imp
	LODSWTX					;Get oVar
	xchg	ax,bx
	DbChk	oVar,bx 			; Check for valid oVar
	mov	ax,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,ax				;Offset in block
	fld	dword ptr [bx]
	DispMac

MakeExe exIdECLdR8,opIdLd,ET_R8
	SkipExHeader
MakeExe exIdICLdR8,opIdLd,ET_Imp 		
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	xchg	bx,ax
	mov	ax,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,ax				;Offset in block
	fld	qword ptr [bx]
	DispMac
;End of [18]

subttl Indirect Load
page

MakeExe exIdEILdI2,opIdLd,ET_I2
	SkipExHeader
MakeExe exIdIILd2,opIdLd,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame			;Get pointer into frame
	mov	bx,[pFrame]		;Load address of variable
	push	[bx]			;Push the I2
	DispMac

MakeExe exIdEILdI4,opIdLd,ET_I4
	SkipExHeader
MakeExe exIdIILd4,opIdLd,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame			;Get pointer into frame
	mov	bx,[pFrame]		;Load address of variable
	push	[bx+2]
	push	[bx]
	DispMac


;Start of [18]

MakeExe exIdEILdR4,opIdLd,ET_R4
	SkipExHeader
MakeExe exIdIILdR4,opIdLd,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame			;Get pointer into frame
	mov	bx,[pFrame]		;Load address of variable
	fld	dword ptr [bx]
	DispMac


MakeExe exIdEILdR8,opIdLd,ET_R8
	SkipExHeader
MakeExe exIdIILdR8,opIdLd,ET_Imp 		
	LODSWTX 				;Get oVar
	xchg	ax,bx				;Move to base register
	DbChk	oVar,bx 			; Check for valid oVar
	GetpFrame				;Get pointer into frame
	mov	bx,[pFrame]			;Load address of variable
	fld	qword ptr [bx]
	DispMac

;End of [18]

subttl Frame Load and Indirect Reference
page

;  The executors for loading two byte and four byte frame variables are
;  also used for references to indirect variables.  For a load of a frame
;  variable, the variable table contains the value which is pushed on the
;  stack.  For a reference to an indirect variable, the variable table
;  contains the address which is then pushed on the stack.  Since addresses
;  are either two or four bytes, only these entries serve two purposes.

MakeExe exIdIIRfTyp,opIdLd,ET_Imp
	LODSWTX					;Get oVar
	xchg	ax,bx				;Move to base register
	DbChk	oVar,bx 			; Check for valid oVar
	mov	bx,[pVarBx]			;Get oFrame
	add	bx,bp				;oFrame to pFrame entry
	push	ds
	push	[bx]
	DispMac

MakeExe exIdEIRfR8,opIdLd,ET_R8
	SkipExHeader
MakeExe exIdEIRfSD,opIdLd,ET_SD
	SkipExHeader
MakeExe exIdEIRfI4,opIdLd,ET_I4
	SkipExHeader
MakeExe exIdEIRfR4,opIdLd,ET_R4
	SkipExHeader
MakeExe exIdEFLdI2,opIdLd,ET_I2
	SkipExHeader
MakeExe exIdIFLd2,opIdLd,ET_Imp
	LODSWTX 				;Get oVar
	xchg	ax,bx				;Move to base register
	DbChk	oVar,bx 			; Check for valid oVar
	GetpFrame				;Get pointer into frame
	push	[pFrame]			;Push the I2
	DispMac


MakeExe exIdEFLdI4,opIdLd,ET_I4
	SkipExHeader
MakeExe exIdIFLd4,opIdLd,ET_Imp
	LODSWTX 				;Get oVar
	xchg	ax,bx				;Move to base register
	DbChk	oVar,bx 			; Check for valid oVar
	GetpFrame				;Get pointer into frame
	push	[pFrame+2]
	push	[pFrame]
	DispMac


;Added with [18]
MakeExe exIdEFLdR4,opIdLd,ET_R4
	SkipExHeader
MakeExe exIdIFLdR4,opIdLd,ET_Imp
	LODSWTX 				;Get oVar
	xchg	ax,bx				;Move to base register
	DbChk	oVar,bx 			; Check for valid oVar
	GetpFrame				;Get pointer into frame
	fld	dword ptr [pFrame]
	DispMac

MakeExe exIdEFLdR8,opIdLd,ET_R8
	SkipExHeader
MakeExe exIdIFLdR8,opIdLd,ET_Imp 		
	LODSWTX 				;Get oVar
	xchg	ax,bx				;Move to base register
	DbChk	oVar,bx 			; Check for valid oVar
	GetpFrame				;Get pointer into frame
	fld	qword ptr [pFrame]
	DispMac
;End of [18]

subttl	Public Store
page
;Added with [15]

	;End of [15]



	subttl	Static Store
	page

MakeExe exIdESStSD,opIdSt,ET_SD
	SkipExHeader
MakeExe exIdISStSD,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	DbChk	oVar,ax 		; Check for valid oVar
	    add     ax,di		;ax = pSdStore
	    push    ax
	CALLRT	B$SASS,DispMov


MakeExe exIdISSt2,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	pop	[pVarBx]		;Store stack in value
	DispMac

MakeExe exIdESStI2,opIdSt,ET_I2
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	pop	[pVarBx]		;Store stack in value
	DispMac

MakeExe exIdESStI4,opIdSt,ET_I4
	SkipExHeader
MakeExe exIdISSt4,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	pop	[pVarBx]		;Store stack in value
	pop	[pVarBx+2]
	DispMac


	;Added with [18]

MakeExe exIdESStR4,opIdSt,ET_R4
	SkipExHeader
MakeExe exIdISStR4,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	fstp	dword ptr [pVarBx]
	fwait
	DispMac

MakeExe exIdESStR8,opIdSt,ET_R8
	SkipExHeader
MakeExe exIdISStR8,opIdSt,ET_Imp 	
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	fstp	qword ptr [pVarBx]
	fwait
	DispMac

	;End of [18]

MakeExe exIdISStTyp,opIdSt,ET_Imp
	LODSWTX 			    ;Get oVar
	DbChk	oVar,ax 		    ; Check for valid oVar
	    add     ax,di		    ;oVar --> pVar
	xchg	di,ax			    ;Destination (points to value field)
	mov	bx,[di-VAR_value].VAR_oTyp  ;Get oTyp
	    push    ds
	jmp	MoveRec

subttl	Common Store
page


MakeExe exIdECStI2,opIdSt,ET_I2
	SkipExHeader
MakeExe exIdICSt2,opIdSt,ET_Imp
	LODSWTX 				;Get oVar
	xchg	ax,bx
	DbChk	oVar,bx 			; Check for valid oVar
	mov	ax,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,ax				;Offset in block
	pop	[bx]
	DispMac


MakeExe exIdECStI4,opIdSt,ET_I4
	SkipExHeader
MakeExe exIdICSt4,opIdSt,ET_Imp
	LODSWTX 				;Get oVar
	xchg	ax,bx
	DbChk	oVar,bx 			; Check for valid oVar
	mov	ax,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,ax				;Offset in block
	pop	[bx]
	pop	[bx+2]
	DispMac


;Added with [18]
MakeExe exIdECStR4,opIdSt,ET_R4
	SkipExHeader
MakeExe exIdICStR4,opIdSt,ET_Imp
	LODSWTX 				;Get oVar
	xchg	ax,bx
	DbChk	oVar,bx 			; Check for valid oVar
	mov	ax,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,ax				;Offset in block
	fstp	dword ptr [bx]
	fwait
	DispMac

MakeExe exIdECStR8,opIdSt,ET_R8
	SkipExHeader
MakeExe exIdICStR8,opIdSt,ET_Imp 		
	LODSWTX 				;Get oVar
	xchg	ax,bx
	DbChk	oVar,bx 			; Check for valid oVar
	mov	ax,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,ax				;Offset in block
	fstp	qword ptr [bx]
	fwait
	DispMac
;End of [18]

MakeExe exIdECStSD,opIdSt,ET_SD
	SkipExHeader
MakeExe exIdICStSD,opIdSt,ET_Imp
	LODSWTX					;Get oVar
	xchg	ax,bx
	DbChk	oVar,bx 			; Check for valid oVar
	mov	ax,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,ax				;Offset in block
	push	bx			;Push pSD
	CALLRT	B$SASS,DispMov


MakeExe exIdICStTyp,opIdSt,ET_Imp
	LODSWTX					;Get oVar
	xchg	ax,bx
	DbChk	oVar,bx 			; Check for valid oVar
	mov	cx,[pVarBx].COMREF_oValue	;Offset into common block
	mov	ax,[pVarBx].COMREF_oCommon	;oCommon
	mov	bx,[pVarBx-VAR_value].VAR_oTyp	;Get oTyp
	add	ax,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	xchg	ax,di
	mov	di,[di].COM_bdValue.BD_pb	;Common block
	add	di,cx				;Offset in block
	push	ds
;ax = saved di
;bx = oTyp of record
;di = offset of destination
;[sp] = segment of destination
;[sp+4]:[sp+2] = pointer to source
	jmp	MoveRec


subttl	Frame Store
page

MakeExe exIdEFStI2,opIdSt,ET_I2
	SkipExHeader
MakeExe exIdIFSt2,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame			;Get pointer into frame
	pop	[pFrame]
	DispMac

MakeExe exIdEFStI4,opIdSt,ET_I4
	SkipExHeader
MakeExe exIdIFSt4,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame			;Get pointer into frame
	pop	[pFrame]
	pop	[pFrame+2]
	DispMac


;Added with [18]
MakeExe exIdEFStR4,opIdSt,ET_R4
	SkipExHeader
MakeExe exIdIFStR4,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame			;Get pointer into frame
	fstp	dword ptr [pFrame]
	fwait
	DispMac

MakeExe exIdEFStR8,opIdSt,ET_R8
	SkipExHeader
MakeExe exIdIFStR8,opIdSt,ET_Imp 	
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame			;Get pointer into frame
	fstp	qword ptr [pFrame]
	fwait
	DispMac
;End of [18]

MakeExe exIdEFStSD,opIdSt,ET_SD
	SkipExHeader
MakeExe exIdIFStSD,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	mov	ax,[pVarBx]		;Get oBP
	add	ax,bp			;ax = pSD
	push	ax			;Push pSD
	CALLRT	B$SASS,DispMov

MakeExe exIdIFStTyp,opIdSt,ET_Imp
	LODSWTX 			    ;Get oVar
	DbChk	oVar,ax 		    ; Check for valid oVar
	    add     ax,di		    ;oVar --> pVar
	xchg	bx,ax			    ;pVar to bx
	xchg	ax,di			    ;Save di in ax
	mov	di,[bx] 		    ;Get oBP
	add	di,bp
	mov	bx,[bx-VAR_value].VAR_oTyp  ;Get oTyp
	    push    ss
	jmp	MoveRec

subttl	Indirect Store

MakeExe exIdEIStI2,opIdSt,ET_I2
	SkipExHeader
MakeExe exIdIISt2,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame			;Get pointer into frame
	mov	bx,[pFrame] 		;Near address to bx
	pop	[bx]
	DispMac 			;Dispatch to next executor

MakeExe exIdEIStI4,opIdSt,ET_I4
	SkipExHeader
MakeExe exIdIISt4,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame			;Get pointer into frame
	mov	bx,[pFrame]		;Near address to bx
	pop	[bx]
	pop	[bx+2]
	DispMac 			;Dispatch to next executor


;Added with [18]
MakeExe exIdEIStR4,opIdSt,ET_R4
	SkipExHeader
MakeExe exIdIIStR4,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame			;Get pointer into frame
	mov	bx,[pFrame]		;Near address to bx
	fstp	dword ptr [bx]
	fwait
	DispMac 			;Dispatch to next executor


MakeExe exIdEIStR8,opIdSt,ET_R8
	SkipExHeader
MakeExe exIdIIStR8,opIdSt,ET_Imp 	
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame			;Get pointer into frame
	mov	bx,[pFrame] 		;Near address to bx
	fstp	qword ptr [bx]
	fwait
	DispMac 			;Dispatch to next executor
;End of [18]

MakeExe exIdEIStSD,opIdSt,ET_SD
	SkipExHeader
MakeExe exIdIIStSD,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	xchg	ax,bx			;Move to base register
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame			;Get pointer into frame
	push	[pFrame]		; Push pSD/handle
	CALLRT	B$SASS,DispMov

MakeExe exIdIIStTyp,opIdSt,ET_Imp
	LODSWTX 			;Get oVar
	DbChk	oVar,ax 		; Check for valid oVar
	    add     ax,di		;oVar --> pVar
	xchg	bx,ax			;pVar to bx
	    xchg    ax,di		;Save di in ax
	mov	di,[bx] 		;Get oBP
	mov	bx,[bx-VAR_value].VAR_oTyp ;Get oTyp
	    push    ds
	mov	di,[bp+di]		;Near address to di
	jmp	MoveRec

subttl	Public IdRf Executors
page

;Added with [15]

	;End of [15]

	subttl	Static IdRf Executors
	page

MakeExe exIdISRfTyp,opIdLd,ET_Imp
	LODSWTX 			;Get oVar
	DbChk	oVar,ax 		; Check for valid oVar
	add	ax,di			;oVar --> pVar
	push	ds			;Push far pointer
	push	ax
	DispMac

MakeExe exIdESRfR8,opIdLd,ET_R8
	SkipExHeader
MakeExe exIdESRfI4,opIdLd,ET_I4
	SkipExHeader
MakeExe exIdESRfSD,opIdLd,ET_SD
	SkipExHeader
MakeExe exIdESRfR4,opIdLd,ET_R4
	SkipExHeader
MakeExe exIdESRfI2,opIdLd,ET_I2
	SkipExHeader
MakeExe exIdISRf,opIdLd,ET_Imp
	LODSWTX 			;Get oVar
	DbChk	oVar,ax 		; Check for valid oVar
	    add     ax,di		;oVar --> pVar
	push	ax
	DispMac


SameExe exIdISLdSD,exIdISRf		
SameExe exIdESLdSD,exIdESRfSD



	subttl	Frame IdRf Executors
	page

MakeExe exIdIFRfTyp,opIdLd,ET_Imp
	LODSWTX 			;Get oVar
	DbChk	oVar,ax 		; Check for valid oVar
	xchg	bx,ax			;Move to base register
	mov	ax,[pVarBx]		;Get oFrame
	add	ax,bp			;oFrame to pValue
	push	ds
	push	ax
	DispMac

MakeExe exIdEFRfR8,opIdLd,ET_R8
	SkipExHeader
MakeExe exIdEFRfSD,opIdLd,ET_SD
	SkipExHeader
MakeExe exIdEFRfI4,opIdLd,ET_I4
	SkipExHeader
MakeExe exIdEFRfR4,opIdLd,ET_R4
	SkipExHeader
MakeExe exIdEFRfI2,opIdLd,ET_I2
	SkipExHeader
MakeExe exIdIFRf,opIdLd,ET_Imp
	LODSWTX 			;Get oVar
	DbChk	oVar,ax 		; Check for valid oVar
	xchg	bx,ax			;Move to base register
	mov	ax,[pVarBx]		;Get oFrame
	add	ax,bp			;oFrame to pValue
	push	ax
	DispMac


SameExe exIdIFLdSD,exIdIFRf		


	subttl	Common IdRf Executors
	page


MakeExe exIdICRfTyp,opIdLd,ET_Imp
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	xchg	bx,ax
	mov	ax,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,ax				;Offset in block
	push	ds
	push	bx
	DispMac


MakeExe exIdECRfR8,opIdLd,ET_R8
	SkipExHeader
MakeExe exIdECRfSD,opIdLd,ET_SD
	SkipExHeader
MakeExe exIdECRfI4,opIdLd,ET_I4
	SkipExHeader
MakeExe exIdECRfR4,opIdLd,ET_R4
	SkipExHeader
MakeExe exIdECRfI2,opIdLd,ET_I2
	SkipExHeader
MakeExe exIdICRf,opIdLd,ET_Imp
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	xchg	bx,ax
	mov	ax,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,ax				;Offset in block
	push	bx
	DispMac



SameExe exIdICLdSD,exIdICRf		
SameExe exIdECLdSD,exIdECRfSD



	subttl	FS load/store/ref executors
	page


MakeExe exIdESRfFS,opIdLd,ET_SD
	SkipExHeader
MakeExe exIdISRfFS,opIdLd,ET_Imp
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	    add     ax,di			;oVar --> pVar
	xchg	bx,ax				;pVar to bx
	    push    ds
	push	bx				;Far address to stack
	push	[bx-VAR_value].VAR_cbFixed	; Push length of FS
	DispMac

MakeExe exIdEIRfFS,opIdLd,ET_SD
	SkipExHeader
MakeExe exIdIIRfFS,opIdLd,ET_Imp
	LODSWTX					;Get oVar
	xchg	ax,bx				;Move to base register
	DbChk	oVar,bx 			; Check for valid oVar
	mov	ax,[pVarBx-VAR_value].VAR_cbFixed   ; Get length
	GetpFrame				;Get pointer into frame
	push	ds
	push	[pFrame]		;Push offset
	push	ax			;Push cb in FS
	DispMac

MakeExe exIdEFRfFS,opIdLd,ET_SD
	SkipExHeader
MakeExe exIdIFRfFS,opIdLd,ET_Imp
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	xchg	bx,ax				;Move to base register
	mov	ax,[pVarBx-VAR_value].VAR_cbFixed   ; Get length
	mov	bx,[pVarBx]			;Get oFrame
	add	bx,bp				;oFrame to pFrame entry
	    push    ss
	push	bx
	push	ax				;Push cb in FS
	DispMac


MakeExe exIdESLdFS,opIdLd,ET_SD
	SkipExHeader
MakeExe exIdISLdFS,opIdLd,ET_Imp
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	    add     ax,di			;oVar --> pVar
	xchg	bx,ax				;pVar to bx
	    push    ds
	push	bx				;Far address to stack
	push	[bx-VAR_value].VAR_cbFixed	; Push length of FS
	CALLRT	B$LDFS,DispMovSd

MakeExe exIdEILdFS,opIdLd,ET_SD
	SkipExHeader
MakeExe exIdIILdFS,opIdLd,ET_Imp
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	xchg	bx,ax				;Move to base register
	mov	ax,[pVarBx-VAR_value].VAR_cbFixed   ; Get length
	GetpFrame			;Get pointer into frame
	push	ds
	push	[pFrame]			;Push offset
	push	ax				;Push cb in FS
	CALLRT	B$LDFS,DispMovSd

MakeExe exIdEFLdFS,opIdLd,ET_SD
	SkipExHeader
MakeExe exIdIFLdFS,opIdLd,ET_Imp
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	xchg	bx,ax				;Move to base register
	mov	ax,[pVarBx-VAR_value].VAR_cbFixed   ; Get length
	mov	bx,[pVarBx]			;Get oFrame
	add	bx,bp				;oFrame to pFrame entry
	push	ss
	push	bx
	push	ax				;Push cb in FS
	CALLRT	B$LDFS,DispMovSd



MakeExe exIdESStFS,opIdSt,ET_SD
	SkipExHeader
MakeExe exIdISStFS,opIdSt,ET_Imp
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	    add     ax,di			;oVar --> pVar
	xchg	bx,ax				;pVar to bx
	    push    ds
	push	bx				;Far address to stack
	push	[bx-VAR_value].VAR_cbFixed	; Push length of FS
	CALLRT	B$LSET,Disp

MakeExe exIdEIStFS,opIdSt,ET_SD
	SkipExHeader
MakeExe exIdIIStFS,opIdSt,ET_Imp
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	xchg	bx,ax				;Move to base register
	mov	ax,[pVarBx-VAR_value].VAR_cbFixed   ; Get length
	GetpFrame				;Get pointer into frame
	push	ds
	push	[pFrame]			;Push offset
	push	ax				;Push cb in FS
	CALLRT	B$LSET,Disp

MakeExe exIdEFStFS,opIdSt,ET_SD
	SkipExHeader
MakeExe exIdIFStFS,opIdSt,ET_Imp
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	xchg	bx,ax				;Move to base register
	mov	ax,[pVarBx-VAR_value].VAR_cbFixed   ; Get length
	mov	bx,[pVarBx]			;Get oFrame
	add	bx,bp				;oFrame to pFrame entry
	push	ss
	push	bx
	push	ax				;Push cb in FS
	CALLRT	B$LSET,Disp


MakeExe exIdICRfFS,opIdLd,ET_Imp
	SkipExHeader
MakeExe exIdECRfFS,opIdLd,ET_SD
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	xchg	bx,ax
	mov	ax,[pVarBx-VAR_value].VAR_cbFixed   ; Get length
	mov	cx,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,cx				;Offset in block
	push	ds
	push	bx
	push	ax			;Push cb in FS
	DispMac


MakeExe exIdICLdFS,opIdLd,ET_Imp
	SkipExHeader
MakeExe exIdECLdFS,opIdLd,ET_SD
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	xchg	bx,ax
	mov	ax,[pVarBx-VAR_value].VAR_cbFixed   ; Get length
	mov	cx,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,cx				;Offset in block
	push	ds
	push	bx
	push	ax			;Push cb in FS
	CALLRT	B$LDFS,DispMovSd


MakeExe exIdICStFS,opIdSt,ET_Imp
	SkipExHeader
MakeExe exIdECStFS,opIdSt,ET_SD
	LODSWTX					;Get oVar
	DbChk	oVar,ax 			; Check for valid oVar
	xchg	bx,ax
	mov	ax,[pVarBx-VAR_value].VAR_cbFixed   ; Get length
	mov	cx,[pVarBx].COMREF_oValue	;Offset into common block
	mov	bx,[pVarBx].COMREF_oCommon	;oCommon
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,cx				;Offset in block
	push	ds
	push	bx
	push	ax				;Push cb in FS
	CALLRT	B$LSET,Disp




SameExe	exIdIIRf,exIdIFLd2
SameExe	exIdEIRfI2,exIdEFLdI2

;*************************************************************************

MakeExe exStDim,opStDim
	inc	si
	inc	si			;Eat the operand
	DispMac

sEnd	CODE
end
