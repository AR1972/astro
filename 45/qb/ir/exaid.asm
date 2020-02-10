page	49,132
	TITLE	exAId	-	Array Id Executors
;***
;exaid.asm - executors for simple id references.
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains all exAId executors and all exAVtRf executors.
;
;	In general, these executors are very speed critical.
;
;
;****************************************************************************

	.xlist
	include 	version.inc
	IncludeOnce	architec
	IncludeOnce	array
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	extort
	IncludeOnce	opid
	IncludeOnce	pcode		
	IncludeOnce	qbimsgs
	IncludeOnce	rtinterp
	IncludeOnce	variable
	.list


assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA


sBegin	CODE

	subttl	I2 Load Executors
	page
	;Indirect

MakeExe exAIdIILd2,opAIdLd,ET_Imp
	SkipExHeader
MakeExe exAIdEILdI2,opAIdLd,ET_I2
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		;ds:bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	;Correct number of indices?
	jz	ALdI2
	jmp	IndexCountErr
	
;Common
MakeExe exAIdECLdI2,opAIdLd,ET_I2
	SkipExHeader
MakeExe exAIdICLd2,opAIdLd,ET_Imp
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	short ALdI2

;Frame
MakeExe exAIdIFLd2,opAIdLd,ET_Imp
	SkipExHeader
MakeExe exAIdEFLdI2,opAIdLd,ET_I2
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]
	add	bx,bp			;ss:bx = pointer to array descriptor
	jmp	short ALdI2

	;Public


	;Static

MakeExe exAIdESLdI2,opAIdLd,ET_I2
	SkipExHeader
MakeExe exAIdISLd2,opAIdLd,ET_Imp
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX 			;Load operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
ALdI2S: 				
	lea	bx,[pVarBx].ASTAT_ad	;ds:bx = array descriptor address
ALdI2:
	call	ResolveArray		;resolve to segment and offset in dx:bx
	GETSEG	ds,dx,di,<SPEED,LOAD>	; Move to array element segment
	push	[bx]			;Push the I2
	    mov     ax,ss
	    mov     ds,ax		;Restore the data segment
	DispMac

	;Optimized I2 load/store executors for 1 index

	;Frame load

MakeExe exA1IdIFLdI2,opAIdLd,ET_Imp
	SkipExHeader
MakeExe exA1IdEFLdI2,opAIdLd,ET_I2
	inc	si
	inc	si			;Skip cDims
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]
	add	bx,bp			;bx = pointer to array descriptor
	    mov     dx,ds		;Save psVariableTable
	jmp	short A1LdI2

	;Static load

MakeExe exA1IdESLdI2,opAIdLd,ET_I2
	SkipExHeader
MakeExe exA1IdISLdI2,opAIdLd,ET_Imp
	inc	si
	inc	si			;Skip count ot indices
	LODSWTX 			;Load operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	lea	bx,[pVarBx].ASTAT_ad	;ds:bx = array descriptor address
	    mov     dx,ds		;Save psVariableTable
A1LdI2:
	mov	cx,[bx].AD_fhd.FHD_hData
	jcxz	RangeErr		; Brif array not allocated
	pop	ax			;Get index
	sub	ax,[bx].AD_tDM.DM_iLbound	;Subtract lower bound
	jl	RangeErr
	cmp	ax,[bx].AD_tDM.DM_cElements	;Test for range
	jge	RangeErr
	shl	ax,1
	add	ax,[bx].AD_fhd.FHD_oData ;Add base offset
	GETSEG	ds,cx,bx,<SPEED,LOAD>	; Move to array element segment
	xchg	bx,ax
	push	[bx]			;Push the I2
	    mov     ds,dx		;Restore module var table
	DispMac

IndexCountErr:
RangeErr:
	mov	al,ER_SOR		;Subscript out of range
	call	RtErrorCODE		;generate error, don't return

	;Frame store

MakeExe exA1IdIFStI2,opAIdSt,ET_Imp
	SkipExHeader
MakeExe exA1IdEFStI2,opAIdSt,ET_I2
	inc	si
	inc	si			;Skip count ot indices
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]
	add	bx,bp			;bx = pointer to array descriptor
	    mov     dx,ds		;Save psVariableTable
	jmp	short A1StI2

	;Static store

MakeExe exA1IdESStI2,opAIdSt,ET_I2
	SkipExHeader
MakeExe exA1IdISStI2,opAIdSt,ET_Imp
	inc	si
	inc	si			;Skip count ot indices
	LODSWTX 			;Load operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	lea	bx,[pVarBx].ASTAT_ad	;ds:bx = array descriptor address
	    mov     dx,ds		;Save psVariableTable
A1StI2:
	mov	cx,[bx].AD_fhd.FHD_hData
	jcxz	RangeErr		; Brif array not allocated
	pop	ax			;Get index
	sub	ax,[bx].AD_tDM.DM_iLbound	;Subtract lower bound
	jl	RangeErr
	cmp	ax,[bx].AD_tDM.DM_cElements	;Test for range
	jge	RangeErr
	shl	ax,1
	add	ax,[bx].AD_fhd.FHD_oData ;Add base offset
	GETSEG	ds,cx,bx,<SPEED,LOAD>	; Move to array element segment
	xchg	bx,ax
	pop	[bx]			;Store the I2
	    mov     ds,dx		;Restore module var table
	DispMac 			; and dispatch next executor


	subttl	I2 Store Executors
	page
	;Common

MakeExe exAIdICSt2,opAIdSt,ET_Imp
	SkipExHeader
MakeExe exAIdECStI2,opAIdSt,ET_I2
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:

	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	short AStI2

;Indirect
MakeExe exAIdIISt2,opAIdSt,ET_Imp
	SkipExHeader
MakeExe exAIdEIStI2,opAIdSt,ET_I2
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		;bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	;Correct number of indices?
	jz	AStI2
	jmp	IndexCountErr

;Frame
MakeExe exAIdEFStI2,opAIdSt,ET_I2
	SkipExHeader
MakeExe exAIdIFSt2,opAIdSt,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]
	add	bx,bp			;bx = pointer to array descriptor
	jmp	short AStI2

	;Public


	;Static

MakeExe exAIdESStI2,opAIdSt,ET_I2
	SkipExHeader
MakeExe exAIdISSt2,opAIdSt,ET_Imp
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX 			;Load operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
AStI2S: 				
	lea	bx,[pVarBx].ASTAT_ad	; ds:bx = array descriptor address
AStI2:
	call	ResolveArray		;resolve to segment and offset in dx:bx
	GETSEG	ds,dx,di,<SPEED,LOAD>	; Move to array element segment
	pop	[bx]			;Pop the I2 to the variable value
	    mov     ax,ss
	    mov     ds,ax		;Restore the data segment
	DispMac

	subttl	I4 Load Executors
	page
	;Indirect

MakeExe exAIdEILdI4,opAIdLd,ET_I4
	SkipExHeader
MakeExe exAIdIILd4,opAIdLd,ET_Imp
	LODSWTX 			    ;Index argument count
	xchg	ax,cx
	LODSWTX 			    ;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		    ;bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	    ;Correct number of indices?
	jz	ALd4
	jmp	IndexCountErr

;Common
MakeExe exAIdECLdI4,opAIdLd,ET_I4
	SkipExHeader
MakeExe exAIdICLd4,opAIdLd,ET_Imp
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	short ALd4

;Frame
MakeExe exAIdEFLdI4,opAIdLd,ET_I4
	SkipExHeader
MakeExe exAIdIFLd4,opAIdLd,ET_Imp
	LODSWTX 			    ;Index argument count
	xchg	ax,cx
	LODSWTX 			    ;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]   
	add	bx,bp			    ;bx = pointer to array descriptor
	jmp	short ALd4

	;Public


;Static
MakeExe exAIdESLdI4,opAIdLd,ET_I4
	SkipExHeader
MakeExe exAIdISLd4,opAIdLd,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
ALd4S:
	lea	bx,[pVarBx].ASTAT_ad	; ds:bx = array descriptor address
ALd4:
	call	ResolveArray		;resolve to segment and offset in dx:bx
	GETSEG	ds,dx,di,<SPEED,LOAD>	; Move to array element segment
SLoad4X:
	push	[bx+2]			;Push second word
	push	[bx]			;Push first word
	    mov     ax,ss
	    mov     ds,ax		;Restore the data segment
	DispMac

;Added with [15]
subttl	R4 Load Executors
page

;Indirect
MakeExe exAIdEILdR4,opAIdLd,ET_R4
	SkipExHeader
MakeExe exAIdIILdR4,opAIdLd,ET_Imp
	LODSWTX 			    ;Index argument count
	xchg	ax,cx
	LODSWTX 			    ;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		    ;bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	    ;Correct number of indices?
	jz	ALdR4
	jmp	IndexCountErr

;Common
MakeExe exAIdECLdR4,opAIdLd,ET_R4
	SkipExHeader
MakeExe exAIdICLdR4,opAIdLd,ET_Imp
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	short ALdR4

;Frame
MakeExe exAIdEFLdR4,opAIdLd,ET_R4
	SkipExHeader
MakeExe exAIdIFLdR4,opAIdLd,ET_Imp
	LODSWTX 			    ;Index argument count
	xchg	ax,cx
	LODSWTX 			    ;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]   
	add	bx,bp			    ;bx = pointer to array descriptor
	jmp	short ALdR4

	;Public


;Static
MakeExe exAIdESLdR4,opAIdLd,ET_R4
	SkipExHeader
MakeExe exAIdISLdR4,opAIdLd,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
ALdR4S:
	lea	bx,[pVarBx].ASTAT_ad	; ds:bx = array descriptor address
ALdR4:
	call	ResolveArray		;resolve to segment and offset in dx:bx
	GETSEG	ds,dx,di,<SPEED,LOAD>	; Move to array element segment
	fld	dword ptr [bx]
	    mov     ax,ss
	    mov     ds,ax		;Restore the data segment
	DispMac

;End of [15]

subttl	I4 Store Executors
page
;Common
MakeExe exAIdECStI4,opAIdSt,ET_I4
	SkipExHeader
MakeExe exAIdICSt4,opAIdSt,ET_Imp
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	short ASt4

;Indirect
MakeExe exAIdEIStI4,opAIdSt,ET_I4
	SkipExHeader
MakeExe exAIdIISt4,opAIdSt,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		;bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	;Correct number of indices?
	jz	ASt4
	jmp	IndexCountErr

;Frame
MakeExe exAIdEFStI4,opAIdSt,ET_I4
	SkipExHeader
MakeExe exAIdIFSt4,opAIdSt,ET_Imp
	LODSWTX 			    ;Index argument count
	xchg	ax,cx
	LODSWTX 			    ;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]   
	add	bx,bp			    ;bx = pointer to array descriptor
	jmp	short ASt4

	;Public


;Static
MakeExe exAIdESStI4,opAIdSt,ET_I4
	SkipExHeader
MakeExe exAIdISSt4,opAIdSt,ET_Imp
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX 			;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
ASt4S:
	lea	bx,[pVarBx].ASTAT_ad	;ds:bx = array descriptor address
ASt4:
	call	ResolveArray		;resolve to segment and offset in dx:bx
	GETSEG	ds,dx,di,<SPEED,LOAD>	; Move to array element segment
SStoreR4X:
	pop	[bx]			;Pop first word
	pop	[bx+2]			;Pop second word
	    mov     ax,ss
	    mov     ds,ax		;Restore the data segment
	DispMac

;Added with [15]
subttl	R4 Store Executors
page
;Common
MakeExe exAIdECStR4,opAIdSt,ET_R4
	SkipExHeader
MakeExe exAIdICStR4,opAIdSt,ET_Imp
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	short AStR4

;Indirect
MakeExe exAIdEIStR4,opAIdSt,ET_R4
	SkipExHeader
MakeExe exAIdIIStR4,opAIdSt,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		;bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	;Correct number of indices?
	jz	AStR4
	jmp	IndexCountErr

;Frame
MakeExe exAIdEFStR4,opAIdSt,ET_R4
	SkipExHeader
MakeExe exAIdIFStR4,opAIdSt,ET_Imp
	LODSWTX 			    ;Index argument count
	xchg	ax,cx
	LODSWTX 			    ;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]   
	add	bx,bp			    ;bx = pointer to array descriptor
	jmp	short AStR4

	;Public


;Static
MakeExe exAIdESStR4,opAIdSt,ET_R4
	SkipExHeader
MakeExe exAIdISStR4,opAIdSt,ET_Imp
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX 			;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
AStR4S:
	lea	bx,[pVarBx].ASTAT_ad	;ds:bx = array descriptor address
AStR4:
	call	ResolveArray		;resolve to segment and offset in dx:bx
	GETSEG	ds,dx,di,<SPEED,LOAD>	; Move to array element segment
	fstp	dword ptr [bx]
	    mov     ax,ss
	    mov     ds,ax		;Restore the data segment
	fwait
	DispMac

;End of [15]


;Added with [15]
subttl	R8 Load Executors
page
;Common
MakeExe exAIdECLdR8,opAIdLd,ET_R8
	SkipExHeader
MakeExe exAIdICLdR8,opAIdLd,ET_Imp
	LODSWTX 				;Load argument count
	xchg	ax,cx
	LODSWTX 				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue	;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:
	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	short ALdR8

;Indirect
MakeExe exAIdEILdR8,opAIdLd,ET_R8
	SkipExHeader
MakeExe exAIdIILdR8,opAIdLd,ET_Imp		
	LODSWTX 				;Index argument count
	xchg	ax,cx
	LODSWTX 				;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]			;bx points to array descriptor
	cmp	cl,[bx].AD_cDims		;Correct number of indices?
	jz	ALdR8				
	jmp	IndexCountErr

;Frame
MakeExe exAIdEFLdR8,opAIdLd,ET_R8
	SkipExHeader
MakeExe exAIdIFLdR8,opAIdLd,ET_Imp		
	LODSWTX 		;Index argument count
	xchg	ax,cx
	LODSWTX 		;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]
	add	bx,bp		;bx = pointer to array descriptor
	jmp	short ALdR8			

;Public

;Static
MakeExe exAIdESLdR8,opAIdLd,ET_R8
	SkipExHeader
MakeExe exAIdISLdR8,opAIdLd,ET_Imp	
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX 			;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
ALdR8S:
	lea	bx,[pVarBx].ASTAT_ad	; ds:bx = array descriptor address
ALdR8:					
	call	ResolveArray		;resolve to segment and offset in dx:bx
	GETSEG	ds,dx,di,<SPEED,LOAD>	; Move to array element segment
	fld	qword ptr [bx]
	    mov     ax,ss
	    mov     ds,ax		;Restore the data segment
	DispMac

;End of [15]


;Start of [15]
subttl	R8 Store Executors
page
;Indirect
MakeExe exAIdEIStR8,opAIdSt,ET_R8
	SkipExHeader
MakeExe exAIdIIStR8,opAIdSt,ET_Imp		
	LODSWTX 				;Index argument count
	xchg	ax,cx
	LODSWTX 				;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]			;bx points to array descriptor
	cmp	cl,[bx].AD_cDims		;Correct number of indices?
	jz	AStR8				
	jmp	IndexCountErr

;Common
MakeExe exAIdECStR8,opAIdSt,ET_R8
	SkipExHeader
MakeExe exAIdICStR8,opAIdSt,ET_Imp		
	LODSWTX 				;Load argument count
	xchg	ax,cx
	LODSWTX 				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue	;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:

	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	short AStR8			

;Frame
MakeExe exAIdEFStR8,opAIdSt,ET_R8
	SkipExHeader
MakeExe exAIdIFStR8,opAIdSt,ET_Imp		
	LODSWTX 				;Index argument count
	xchg	ax,cx
	LODSWTX 				;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]	
	add	bx,bp		;bx = pointer to array descriptor
	jmp	short AStR8			;jmp is faster from here

;Public

;Static
MakeExe exAIdESStR8,opAIdSt,ET_R8
	SkipExHeader
MakeExe exAIdISStR8,opAIdSt,ET_Imp	
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX 			;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
AStR8S:
	lea	bx,[pVarBx].ASTAT_ad	; ds:bx = array descriptor address
AStR8:					
	call	ResolveArray		;resolve to segment and offset in dx:bx
	GETSEG	ds,dx,di,<SPEED,LOAD>	; Move to array element segment
	fstp	qword ptr [bx]
	    mov     ax,ss
	    mov     ds,ax		;Restore the data segment
	fwait
	DispMac

;End of [15]

subttl	Sd Load Executors
page
;Common
MakeExe exAIdECRfSD,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAIdICRfSD,opAIdLd,ET_Imp
	LODSWTX 				;Load argument count
	xchg	ax,cx
	LODSWTX 				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:

	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	short ALdSd

;Indirect
MakeExe exAIdEIRfSD,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAIdIIRfSD,opAIdLd,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		;bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	;Correct number of indices?
	jz	ALdSD
	jmp	IndexCountErr

;Frame
MakeExe exAIdEFRfSD,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAIdIFRfSD,opAIdLd,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]
	add	bx,bp			;bx = pointer to array descriptor
	jmp	short ALdSD

;public


;Static
MakeExe exAIdESRfSD,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAIdISRfSD,opAIdLd,ET_Imp
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX 			;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	lea	bx,[pVarBx].ASTAT_ad	; ds:bx = array descriptor address
ALdSd:
	call	ResolveArray		;resolve to segment and offset in dx:bx
	push	bx			;Load pointer to SD
	DispMac

subttl	Sd Store Executors
page
;Common
MakeExe exAIdECStSD,opAIdSt,ET_SD
	SkipExHeader
MakeExe exAIdICStSD,opAIdSt,ET_Imp
	LODSWTX 				;Load argument count
	xchg	ax,cx
	LODSWTX 				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:

	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	short AStSD

;Indirect
MakeExe exAIdEIStSD,opAIdSt,ET_SD
	SkipExHeader
MakeExe exAIdIIStSD,opAIdSt,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		;bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	;Correct number of indices?
	jz	AStSD
	jmp	IndexCountErr

;Frame
MakeExe exAIdEFStSD,opAIdSt,ET_SD
	SkipExHeader
MakeExe exAIdIFStSD,opAIdSt,ET_Imp
	LODSWTX 		;Index argument count
	xchg	ax,cx
	LODSWTX 		;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]
	add	bx,bp		;bx = pointer to array descriptor
	jmp	short AStSD

;public


;Static
MakeExe exAIdESStSD,opAIdSt,ET_SD
	SkipExHeader
MakeExe exAIdISStSD,opAIdSt,ET_Imp
	LODSWTX 		;Load argument count
	xchg	ax,cx
	LODSWTX 		;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	lea	bx,[pVarBx].ASTAT_ad ;ds:bx = array descriptor address
AStSd:
	call	ResolveArray	;resolve to segment and offset in dx:bx
	push	bx		;Push pSD
	CALLRT	B$SASS,DispMov

	subttl	FS load/store executors
	page


MakeExe exAIdESRfFS,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAIdISRfFS,opAIdLd,ET_Imp
	LODSWTX 		;Index argument count
	xchg	ax,cx
	LODSWTX 		;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	lea	bx,[pVarBx].ASTAT_ad ;ds:bx = array descriptor address
FsRf:
	call	ResolveArray	;resolve to segment and offset in dx:bx
	push	dx		;Push segment
	push	bx		; and offset
	mov	bx,PTRTX[si-2]	;Get oVar again
	push	[pVarBx-VAR_value].VAR_cbFixed	; Push length of FS
	DispMac

MakeExe exAIdEIRfFS,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAIdIIRfFS,opAIdLd,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		;bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	;Correct number of indices?
	jz	FsRf
	jmp	IndexCountErr

MakeExe exAIdEFRfFS,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAIdIFRfFS,opAIdLd,ET_Imp
	LODSWTX 		;Index argument count
	xchg	ax,cx
	LODSWTX 		;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]
	add	bx,bp		;bx = pointer to array descriptor
	jmp	FsRf

MakeExe exAIdECRfFS,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAIdICRfFS,opAIdLd,ET_Imp
	LODSWTX 				;Load argument count
	xchg	ax,cx
	LODSWTX 				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:

	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	FsRf



MakeExe exAIdESLdFS,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAIdISLdFS,opAIdLd,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	lea	bx,[pVarBx].ASTAT_ad	;ds:bx = array descriptor address
FsLd:
	call	ResolveArray		;resolve to segment and offset in dx:bx
	push	dx			;Push segment
	push	bx			; and offset
	mov	bx,PTRTX[si-2]		;Get oVar again
	push	[pVarBx-VAR_value].VAR_cbFixed	; Push length of FS
	CALLRT	B$LDFS,DispMovSd	

MakeExe exAIdEILdFS,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAIdIILdFS,opAIdLd,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		;bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	;Correct number of indices?
	jz	FsLd
	jmp	IndexCountErr

MakeExe exAIdEFLdFS,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAIdIFLdFS,opAIdLd,ET_Imp
	LODSWTX 		;Index argument count
	xchg	ax,cx
	LODSWTX 		;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]
	add	bx,bp		;bx = pointer to array descriptor
	jmp	FsLd

MakeExe exAIdECLdFS,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAIdICLdFS,opAIdLd,ET_Imp
	LODSWTX 				;Load argument count
	xchg	ax,cx
	LODSWTX 				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:

	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	FsLd



MakeExe exAIdESStFS,opAIdSt,ET_SD
	SkipExHeader
MakeExe exAIdISStFS,opAIdSt,ET_Imp
	LODSWTX 		;Index argument count
	xchg	ax,cx
	LODSWTX 		;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	lea	bx,[pVarBx].ASTAT_ad ;ds:bx = array descriptor address
FsSt:
	call	ResolveArray	;resolve to segment and offset in dx:bx
	push	dx		;Push segment
	push	bx		; and offset
	mov	bx,PTRTX[si-2]	;Get oVar again
	push	[pVarBx-VAR_value].VAR_cbFixed	; Push length of FS
	CALLRT	B$LSET,Disp

MakeExe exAIdEIStFS,opAIdSt,ET_SD
	SkipExHeader
MakeExe exAIdIIStFS,opAIdSt,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		;bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	;Correct number of indices?
	jz	FsSt
	jmp	IndexCountErr

MakeExe exAIdEFStFS,opAIdSt,ET_SD
	SkipExHeader
MakeExe exAIdIFStFS,opAIdSt,ET_Imp
	LODSWTX 		;Index argument count
	xchg	ax,cx
	LODSWTX 		;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]
	add	bx,bp		;bx = pointer to array descriptor
	jmp	FsSt

MakeExe exAIdECStFS,opAIdSt,ET_SD
	SkipExHeader
MakeExe exAIdICStFS,opAIdSt,ET_Imp
	LODSWTX 				;Load argument count
	xchg	ax,cx
	LODSWTX 				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:

	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	FsSt

subttl	Record store executors
page

MakeExe exAIdICStTyp,opAIdSt,ET_Imp
	LODSWTX 				;Load argument count
	xchg	ax,cx
	LODSWTX 				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:

	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	short AStTyp

MakeExe exAIdIIStTyp,opAIdSt,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		;bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	;Correct number of indices?
	jz	AStTyp
	jmp	IndexCountErr

MakeExe exAIdIFStTyp,opAIdSt,ET_Imp
	LODSWTX 		;Index argument count
	xchg	ax,cx
	LODSWTX 		;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]
	add	bx,bp		;bx = pointer to array descriptor
	jmp	short AStTyp



MakeExe exAIdISStTyp,opAIdSt,ET_Imp
	LODSWTX 		;Load argument count
	xchg	ax,cx
	LODSWTX 		;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	lea	bx,[pVarBx].ASTAT_ad	;ds:bx = array descriptor address
AStTyp:
	call	ResolveArray		;resolve to segment and offset in dx:bx
	push	dx			;Need segment on stack
	xchg	ax,bx			;Offset in ax
	mov	bx,PTRTX[si-2]		;Go back and get oVar
	mov	bx,[pVarBx].VAR_oTyp-VAR_value	;oTyp in bx
	xchg	di,ax			;Offset in di, di saved in ax
;ax = saved di	(SizeD = 0 only)
;bx = oTyp of record
;di = offset of destination
;[sp] = segment of destination
;[sp+4]:[sp+2] = pointer to source
	jmp	MoveRec			;Copy the record

subttl	Rf executors
page
;Frame
MakeExe exAIdEFRfR8,opAIdLd,ET_R8
	SkipExHeader
MakeExe exAIdEFRfI2,opAIdLd,ET_I2
	SkipExHeader
MakeExe exAIdEFRfI4,opAIdLd,ET_I4
	SkipExHeader
MakeExe exAIdEFRfR4,opAIdLd,ET_R4
	SkipExHeader
MakeExe exAIdIFRf,opAIdLd,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	bx,[pVarBx+AFRAME_oFrame]
	add	bx,bp			;bx = pointer to array descriptor
	jmp	short Ref

	;Public


;Static
MakeExe exAIdESRfR8,opAIdLd,ET_R8
	SkipExHeader
MakeExe exAIdESRfI4,opAIdLd,ET_I4
	SkipExHeader
MakeExe exAIdESRfR4,opAIdLd,ET_R4
	SkipExHeader
MakeExe exAIdESRfI2,opAIdLd,ET_I2
	SkipExHeader
MakeExe exAIdISRf,opAIdLd,ET_Imp
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX 			;Operand
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	lea	bx,[pVarBx].ASTAT_ad	;ds:bx = array descriptor address
Ref:
	call	ResolveArray		;resolve to segment and offset in dx:bx
	push	dx
	push	bx			;Put far address on stack
	DispMac 			; and dispatch next executor



MakeExe exAIdECRfI2,opAIdLd,ET_I2
	SkipExHeader
MakeExe exAIdECRfI4,opAIdLd,ET_I4
	SkipExHeader
MakeExe exAIdECRfR4,opAIdLd,ET_R4
	SkipExHeader
MakeExe exAIdECRfR8,opAIdLd,ET_R8
	SkipExHeader
MakeExe exAIdICRf,opAIdLd,ET_Imp
	LODSWTX 			;Load argument count
	xchg	ax,cx
	LODSWTX				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	mov	dx,[pVarBx].ACOM_oValue		;Offset into common block
	test	byte ptr [pVarBx-VAR_value].VAR_fStat,FV_STATIC 
						;Is the array $STATIC?
	mov	bx,[pVarBx].ACOM_oCommon	;oCommon
	jz	@F
	add	bx,COM_bdType - COM_bdValue	;Adjust to point to type table
@@:

	add	bx,[grs.GRS_bdtComBlk.BD_pb]	;pCommon
	mov	bx,[bx].COM_bdValue.BD_pb	;Common block
	add	bx,dx				;Offset in block
	jmp	Ref

MakeExe exAIdEIRfI2,opAIdLd,ET_I2
	SkipExHeader
MakeExe exAIdEIRfI4,opAIdLd,ET_I4
	SkipExHeader
MakeExe exAIdEIRfR4,opAIdLd,ET_R4
	SkipExHeader
MakeExe exAIdEIRfR8,opAIdLd,ET_R8
	SkipExHeader
MakeExe exAIdIIRf,opAIdLd,ET_Imp
	LODSWTX 			;Index argument count
	xchg	ax,cx
	LODSWTX 			;oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	GetpFrame
	mov	bx,[pFrame]		;bx = pointer to array descriptor
	cmp	cl,[bx].AD_cDims	;Correct number of indices?
	jz	Ref
	jmp	IndexCountErr


	;Array descriptor references

MakeExe exAdRfSD,opAIdLd,ET_SD
	SkipExHeader
MakeExe exAdRfR8,opAIdLd,ET_R8
	SkipExHeader
MakeExe exAdRfI4,opAIdLd,ET_I4
	SkipExHeader
MakeExe exAdRfR4,opAIdLd,ET_R4
	SkipExHeader
MakeExe exAdRfI2,opAIdLd,ET_I2
	SkipExHeader
MakeExe exAdRfImp,opAIdLd,ET_Imp
	;Push addr of array descriptor

	inc	si
	inc	si			;Skip over count of indices
	LODSWTX				;Get oVar
	xchg	ax,bx			; BX = oVar
	DbChk	oVar,bx 		; Check for valid oVar
	call	oVarToPAd		;BX = pAd
	push	bx			;Return pAd
	DispMac

sEnd	CODE
end
