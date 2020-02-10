page	49,132
	TITLE	EXSTRING - string function and statement executors
;***
;exstring.asm - string function and statement executors for QBI
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;
;****************************************************************************

	.xlist
	include 	version.inc
EXSTRING_ASM = ON
	IncludeOnce	architec
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	extort
	IncludeOnce	opaftqb4
	IncludeOnce	opintrsc
	IncludeOnce	opstmt
	IncludeOnce	pcode		
	IncludeOnce	ui
	.list


assumes es, NOTHING
assumes ss, DATA

sBegin	DATA



sEnd	DATA

sBegin	CODE
assumes cs, CODE





;==============================================================================
;		Intrinsic String Function Executors
;==============================================================================

MakeExe exFnCvi,opFnCvi
	CALLRT	B$FCVI,DispAx

MakeExe exFnCvl,opFnCvl
	CALLRT	B$FCVL,DispDxAx

MakeExe exFnCvs,opFnCvs
	TestM	[cmdSwitches],CMD_SW_MBF
	jnz	exFnCvsmbf		;Brif we want this to act like CVSMBF
	CALLRT	B$FCVS,DispR4

MakeExe exFnCvd,opFnCvd
	TestM	[cmdSwitches],CMD_SW_MBF
	jnz	exFnCvdmbf		;Brif we want this to act like CVDMBF
	CALLRT	B$FCVD,DispR8

MakeExe exFnCvsmbf,opFnCvsmbf
	CALLRT	B$MCVS,DispR4

MakeExe exFnCvdmbf,opFnCvdmbf
	CALLRT	B$MCVD,DispR8

MakeExe exFnMki_,opFnMki_
	CALLRT	B$FMKI,DispMovSd	

MakeExe exFnMkl_,opFnMkl_
	CALLRT	B$FMKL,DispMovSd	

MakeExe exFnMks_,opFnMks_
	TestM	[cmdSwitches],CMD_SW_MBF
	jnz	exFnMksmbf_		;Brif we want this to act like MKSMBF
	sub	sp,4			
	mov	bx,sp			
	fstp	dword ptr DGROUP:[bx]	; Move to local stack
	fwait				
	CALLRT	B$FMKS,DispMovSd	

MakeExe exFnMkd_,opFnMkd_
	TestM	[cmdSwitches],CMD_SW_MBF
	jnz	exFnMkdmbf_		;Brif we want this to act like MKDMBF
	sub	sp,8			
	mov	bx,sp			
	fstp	qword ptr DGROUP:[bx]	; Move to local stack
	fwait				
	CALLRT	B$FMKD,DispMovSd	

MakeExe exFnMksmbf_,opFnMksmbf_
	sub	sp,4			
	mov	bx,sp			
	fstp	dword ptr DGROUP:[bx]	; Move to local stack
	fwait				
	CALLRT	B$FMSF,DispMovSd	

MakeExe exFnMkdmbf_,opFnMkdmbf_
	sub	sp,8			
	mov	bx,sp			
	fstp	qword ptr DGROUP:[bx]	; Move to local stack
	fwait				
	CALLRT	B$FMDF,DispMovSd	


MakeExe exFnAsc,opFnAsc
	CALLRT	B$FASC,DispAx


MakeExe exFnChr_,opFnChr_

	CALLRT	B$FCHR,DispMovSd	


MakeExe exFnHex_I2,opFnHex_
	pop	bx
	xor	ax,ax
	push	ax			;high word
	push	bx			;push back the low word
	SkipExHeader
MakeExe exFnHex_I4,opFnHex_
	CALLRT	B$FHEX,DispMovSd	

MakeExe exFnInstr2,opFnInstr2
	CALLRT	B$INS2,DispAx

MakeExe exFnInstr3,opFnInstr3
	CALLRT	B$INS3,DispAx


MakeExe exFnLCase_,opFnLCase_
	CALLRT	B$LCAS,DispMovSd	

MakeExe exFnLeft_,opFnLeft_
	CALLRT	B$LEFT ,DispMovSd	


MakeExe exFnLenTyp,opFnLen
	add	sp,4			;Pop the far reference
	LODSWTX 			;fetch cbTyp
PushLength:
	xor	dx,dx			; High word of length is 0
	jmp	DispDxAx		; Push result and dispatch

MakeExe exFnLen,opFnLen
	inc	si		;skip unneeded argument (present only for
	inc	si		;  compatability between executors for opFnLen)
	CALLRT	B$FLEN,DispAx

MakeExe exFnLTrim_,opFnLTrim_
	CALLRT	B$LTRM ,DispMovSd	

MakeExe exFnMid_2,opFnMid_2

	PUSHI	ax,07FFFH		;Max legal string length
	SkipExHeader
MakeExe exFnMid_3,opFnMid_3
	CALLRT	B$FMID,DispMovSd	

MakeExe exFnOct_I2,opFnOct_
	pop	bx
	xor	ax,ax
	push	ax			;High word
	push	bx			;Push back the low word
	SkipExHeader
MakeExe exFnOct_I4,opFnOct_
	CALLRT	B$FOCT,DispMovSd	

MakeExe exFnRight_,opFnRight_
	CALLRT	B$RGHT,DispMovSd	

MakeExe exFnRTrim_,opFnRTrim_
	CALLRT	B$RTRM,DispMovSd	

MakeExe exFnSAdd,opFnSAdd
	CALLRT	B$SADD,DispMovSd	

MakeExe exFnSpace_,opFnSpace_
	CALLRT	B$SPAC,DispMovSd	

MakeExe exFnStr_I2,opFnStr_
	CALLRT	B$STI2,DispMovSd	

MakeExe exFnStr_I4,opFnStr_
	CALLRT	B$STI4,DispMovSd	


MakeExe exFnStr_R4,opFnStr_
;Added with [9]
	sub	sp,4
	mov	bx,sp
	fstp	dword ptr DGROUP:[bx]
	fwait
;End of [9]
	CALLRT	B$STR4,DispMovSd	


MakeExe exFnStr_R8,opFnStr_
;Added with [9]
	sub	sp,8
	mov	bx,sp
	fstp	qword ptr DGROUP:[bx]
	fwait
;End of [9]
	CALLRT	B$STR8,DispMovSd	



MakeExe exFnString_I2,opFnString_
	CALLRT	B$STRI,DispMovSd	

MakeExe exFnString_SD,opFnString_
	CALLRT	B$STRS,DispMovSd	

MakeExe exFnUCase_,opFnUCase_
	CALLRT	B$UCAS,DispMovSd	

MakeExe exFnVal,opFnVal
	CALLRT	B$FVAL,DispMovR8




;==============================================================================
;		String Statement Executors
;==============================================================================

MakeExe exPushOp,opNolist1		;Push word operand
	LODSWTX
	jmp	DispAx

MakeExe exStLSetRec,opStLSet		;Record version of LSet
	pop	cx			;cb to copy
	pop	di
	    pop     es
	xchg	ax,si			;Preserve SI in AX
	pop	si
	    pop     ds

	shr	cx,1			;Move by words
	rep	movsw
	adc	cx,cx			;CX = 1 iff CY set
	rep	movsb

	xchg	si,ax			;Restore SI
	    push    ss			;Restore DS
	    pop     ds
	jmp	DispMov 		;Reload ES & DI (DS if SizeD) Dispatch

MakeExe exStLSet,opStLSet
	xor	ax,ax			;last parm = 0 == "SD, not an FS"
	push	ax
	SkipExHeader
MakeExe exStLSetFS,opStLSet
	CALLRT	B$LSET,Disp

;NOTE: Yes, we could reduce the opStMid executors (below) to save a few bytes
;NOTE: if we changed the parser. At this point (at least), the few bytes
;NOTE: arn't worth the calender time to make the change. (Oct. 86)

MakeExe exStMid_2,opStMid_2
	xor	ax,ax
	push	ax		;cbFS = 0 ==> SD, not FS
	SkipExHeader
MakeExe exStMid_FS2,opStMid_2
;!!!! WARNING !!!! The KANJI version of these executors assume that no flags
;!!!! WARNING !!!! are changed from here down to the runtime dispatch.

	pop	es		;pull off cbFS
	pop	di		;pull off (far) target address offset
	pop	bx		;pull off (far) target address seg
	pop	ax		;psdExp must be pulled off an max string length
	mov	cx,07FFFH
StMid_Common:
	pop	dx		;start
	push	bx
	push	di
	push	es		;cxFS
	push	ax		;psdSource
	push	cx		;string length (given or defaulted)
	push	dx		;start
	CALLRT	B$SMID,DispMov	;need the Mov variant, so es can be used below

MakeExe exStMid_3,opStMid_3
	xor	ax,ax
	push	ax		;cbFS = 0 ==> SD, not FS
	SkipExHeader
MakeExe exStMid_FS3,opStMid_3
;!!!! WARNING !!!! The KANJI version of these executors assume that no flags
;!!!! WARNING !!!! are changed from here down to the runtime dispatch.

	pop	es		;pull off cbFS
	pop	di		;pull off (far) target address offset
	pop	bx		;pull off (far) target address seg
	pop	ax		;psdExp must be pulled off an max string length
	pop	cx		;pull off specified string length
	jmp	short StMid_Common

MakeExe exStRset,opStRset
	xor	ax,ax		;last parm = 0 == "SD, not an FS"
	push	ax
	SkipExHeader
MakeExe exStRsetFS,opStRset
	CALLRT	B$RSET,Disp


	;Added with [18]


	;End of [18]

sEnd	CODE
end
