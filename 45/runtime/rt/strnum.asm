	TITLE	STRNUM - String to number and number to string conversions
;***
; STRNUM - String to number and number to string conversions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; CVI Function: 		CVL Function:
;
;   v = CVI(2-byte string)	  v = CVL(4-byte string)
;	    |				  |
;	 B$FCVI		       B$FCVL
;
;
; CVS Function: 		CVD Function:
;
;   v = CVS(4-byte string)	  v = CVD(8-byte string)
;	    |				  |
;	 B$FCVS		       B$FCVD
;
;
; CVSMBF Function:		CVDMBF Function:
;
;   v = CVSMBF(4-byte string)	  v = CVDMBF(8-byte string)
;	    |				  |
;	 B$FCSF		       B$FCDF
;
;
; MKI$ Function:		MKL$ Function:
;
;   v$ = MKI$(integer exp)	  v$ = MKL$(long integer exp)
;	     |				   |
;	 B$FMKI		       B$FMKL
;
;
; MKS$ Function:		MKD$ Function:
;
;   v$ = MKS$(s.p. exp) 	  v$ = MKD$(d.p. exp)
;	     |				   |
;	 B$FMKS		       B$FMKD
;
;
; MKSMBF$ Function:		MKDMBF$ Function:
;
;   v$ = MKSMBF$(s.p. exp)	  v$ = MKDMBF$(d.p. exp)
;	     |				   |
;	  B$FMSF			B$FMDF
;
;******************************************************************************
	INCLUDE switch.inc	
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	useSeg	ST_TEXT
	useSeg	_TEXT
	useSeg	_DATA
	useSeg	_BSS

	INCLUDE seg.inc
	INCLUDE	baslibma.inc	
	INCLUDE string.inc	

sBegin	_DATA

externW B$AC
externW B$DAC

sEnd	_DATA

sBegin	_BSS

	externB	b$Buf3		; temporary space

sEnd	_BSS

	externFP B$ERR_OV	; FAR because it's JMP'ed to from _TEXT

sBegin	_TEXT			
	externNP $i4_m4		
	externNP $i8_m8		
	externNP $m4_i4		
	externNP $m8_i8		
sEnd	_TEXT

sBegin	ST_TEXT

	externNP B$STDALCTMP
	externNP B$STALCTMPCPY
	externNP B$STALCTMP
	externNP B$ERR_FC

	ASSUMES CS,ST_TEXT

	SUBTTL
	PAGE
;***
;STRCPY - copy CX bytes from string to given location & free string if temp.
;
;Purpose:
; This routine is shared code for the CVS/CVD/... routines. Must be far, since
; it is called from another segment in some routines defined below.
;
;Entry:
; [BX] = pointer to a string descriptor.
; [CX] = count of bytes to copy from the string
; [DI] = target address for the copy.
;
;Exit:
; The copy is made, and the string is deallocated it temp.
; [AX] = pointer to the start of the target address (input DI)
;
;Modifies:
; Per Convention
;
;******************************************************************************
cProc	STRCPY,FAR,<DI,SI,ES>  ; added this routine to save code
cBegin

	CMP	BYTE PTR[BX],CL ;See if string is long enough
	JB	ARGERR

	PUSH	DS
	POP	ES		;[ES] = [DS]
	MOV	SI,[BX+2]	;Get address of data area
	PUSH	DI		;save for retval

	REP	MOVSB		;copy the string data
	CALL	B$STDALCTMP	;Delete string if temp
	POP	AX		;pointer to target address
cEnd

ARGERR: JMP	B$ERR_FC	;Illegal function call

;***
; B$FCVI - Convert string to integer
;
;Function:
; Make sure string is at least 2 bytes long, then return the first 2 bytes
; as an integer.
;
;Inputs:
; sdNum = string descriptor
;
;Outputs:
; [AX]	= number
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$FCVI,<FAR,PUBLIC>	
cBegin	<nogen>			
	MOV	CL,2		;[CL] = length to do
	SKIP	2		; fall through - share code
cEnd	nogen

;***
; B$FCVL - Convert string to long integer
;
;Function:
; Make sure string is at least 4 bytes long, then return the first 4 bytes
; as a long integer.
;
;Inputs:
; sdNum = string descriptor
;
;Outputs:
; [DX:AX]  = number
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$FCVL,<FAR,PUBLIC>
cBegin	<nogen>			
	MOV	CL,4		;[CL] = length to be copied
	XOR	CH,CH		
cEnd	<nogen>			; fall through to common code

cProc	BFCV_Int,<FAR>,DI	
parmSD	sdNum			
cBegin				
	GetpSD	BX,sdNum	; [BX] = psdNum
	MOV	DI,OFFSET DGROUP:B$AC	;[DI] = pointer to dummy place to copy
	CALL	STRCPY
	XCHG	AX,BX		;[BX] = pointer to number
	MOV	AX,[BX] 	;[AX] = low word of number (both CVL and CVS)
	MOV	DX,[BX+2]	;[DX] = high word (CVL only, else garbage)
cEnd

;***
; B$FCVS, B$FCVD - Convert string to single or double precision number
;
;Function:
; Convert the string to single or double precision number. Make sure it's long
; enough first.
;
;Inputs:
; sdNum = string descriptor
;
;Outputs:
; [AX] = pointer to resulting number in FAC/DAC
;
;Registers:
; per convention
;******************************************************************************
cProc	B$FCVS,<FAR,PUBLIC>	
cBegin	<nogen>			
	MOV	CL,4		
	SKIP	2
cEnd	<nogen>			; fall through to share code

cProc	B$FCVD,<FAR,PUBLIC>	
cBegin	<nogen>			
	MOV	CL,8		
	XOR	CH,CH		
cEnd	<nogen>

cProc	BFCV_Real,<FAR>,DI	
parmSD	sdNum			
cBegin				
	MOV	DI,OFFSET DGROUP:B$DAC
	GetpSD	BX,sdNum	; [BX] = psdNum
	CALL	STRCPY		
cEnd

;***
; B$FMKI, B$FMKL, B$FMKS, B$FMKD - Convert number to string
;
;Function:
; Create a 2-, 4-, or 8-byte temp string and put the number in it.
;
;Inputs:
; ivar	= Integer to be converted (MKI)
; lvar	= Long integer to be converted (MKL)
; R4var = real (s.p.) number to be converted (MKS)
; R8var = real (d.p.) number to be converted (MKD)
;
;Outputs:
; [AX]	= Address of string descriptor
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$FMKI,<FAR,PUBLIC>
parmW	ivar
cBegin
	MOV	BX,2		;Create a string of length 2
	LEA	DX,ivar		; [DX] = pointer to input parm
	CALL	B$STALCTMPCPY	; Allocate string and copy data in
	XCHG	AX,BX		; [AX] = pointer to descriptor
cEnd

cProc	B$FMKL,<FAR,PUBLIC>
cBegin	<nogen>			
cEnd	<nogen>			; share code with B$FMKS

cProc	B$FMKS,<FAR,PUBLIC>
parmD	R4num			
cBegin
MK_4:				
	MOV	BX,4		;Create a string of length 4
	LEA	DX,R4num	; [DX] = pointer to input parm
	CALL	B$STALCTMPCPY	; Allocate string and copy data in
	XCHG	AX,BX		; [AX] = pointer to descriptor
cEnd

cProc	B$FMKD,<FAR,PUBLIC>
parmQ	R8num			
cBegin
	MOV	BX,8		;4 Bytes in single precision number
	LEA	DX,R8num 	;[DX] = pointer to data
	CALL	B$STALCTMPCPY	;Allocate string and copy data in
	XCHG	AX,BX		;[AX] = pointer to descriptor
cEnd

sEnd	ST_TEXT			

sBegin	_TEXT			
assumes CS,_TEXT		

;***
;Ix_Mx
;
;Purpose:
; Since the low-level conversion functions (MBF to/from IEEE) are near routines
; in _TEXT, B$MCVS & B$MCVD call this routine to do the actual conversion
; (so they can be in ST_TEXT).
;
;Entry:
; AX is 4 if $i4_m4 is to called, 8 if $i8_m8 is to be called.
;
;******************************************************************************
;***
;Mx_Ix
;
;Purpose:
; Since the low-level conversion functions (MBF to/from IEEE) are near routines
; in _TEXT, B$FMSF & B$FMDF call this routine to do the actual conversion
; (so they can be in ST_TEXT).
;
;Entry:
; CX is 4 if $m4_i4 is to called, 8 if $m8_i8 is to be called.
;
;******************************************************************************
IX_MX_DISP:
	DW	_TEXTOFFSET $i4_m4 
	DW	_TEXTOFFSET $i8_m8 
MX_IX_DISP:
	DW	_TEXTOFFSET $m4_i4 
	DW	_TEXTOFFSET $m8_i8 

cProc	Ix_Mx,<FAR>		
cBegin	<nogen>			
	MOV	BX,_TEXTOFFSET IX_MX_DISP 
	JMP	SHORT Convert_Common 
cEnd	<nogen>			

cProc	Mx_Ix,<FAR>		
cBegin	<nogen>			
	XCHG	AX,CX		; put input into AX for common code, below
	MOV	BX,_TEXTOFFSET MX_IX_DISP 
cEnd	<nogen>			

cProc	Convert_Common,<FAR>	
cBegin
	SHR	AL,1		; convert AL = 4 or 8 to AL = 0 or 2
	SHR	AL,1		
	AND	AL,0FEH		
	ADD	BL,AL		; add offset to dispatch table start
	CALL	CS:[BX]		; call appropriate mathpack routine
	OR	AX,AX		; Overflow?
	JZ	Convert_done	; Jump if not
	JMP	B$ERR_OV	; Else declare error
Convert_done:			
cEnd

sEnd	_TEXT			


sBegin	ST_TEXT			
assumes CS,ST_TEXT		

;***
;B$MCVS - CVSMBF function
;
;Purpose:
; Given a string containing a 4-byte s.p. number in Microsoft Binary Format,
; return a pointer to an s.p. number in IEEE format.
;
;Entry:
; sdNum - string descriptor for the 4-byte string
;
;Exit:
; [AX]	= pointer to resulting number in FAC.
;
;Uses:
; Per convention
;******************************************************************************
cProc	B$MCVS,<PUBLIC,FAR,NODATA>	
cBegin	<nogen>			
	MOV	CL,4		
	SKIP	2		
cEnd	<nogen>			

;***
;B$MCVD - CVDMBF function
;
;Purpose:
; Given a string containing a 8-byte d.p. number in Microsoft Binary Format,
; return a pointer to an d.p. number in IEEE format.
;
;Entry:
; sdNum - string descriptor for the 4-byte string
;
;Exit:
; [AX]	= pointer to result in DAC
;
;Uses:
; Per convention
;******************************************************************************
cProc	B$MCVD,<PUBLIC,FAR,NODATA>	
cBegin	<nogen>
	MOV	CL,8		
	XOR	CH,CH		
cEnd	<nogen>			

cProc	BMCV_Real,<FAR,NODATA>,<ES,SI,DI> 
parmSD	sdNum				

cBegin				
	MOV	SI,OFFSET DGROUP:B$DAC
	GetpSD	BX,sdNum	;BX = psdNum
	MOV	DI,OFFSET DGROUP:b$Buf3 ; temporary storage (>= 8 bytes)
	PUSH	CX		;Save the byte count
	CALL	STRCPY		;copy data to $FTMP, delete string if temp
	POP	AX		;[AX] = count of bytes
	XCHG	SI,DI		;SI points to B$FTMP1 (input number)
	PUSH	DS
	POP	ES		;ES = DS for math format mapping call

	PUSH	DI		;save pointer to result for retval
	CALL	Ix_Mx		; perform actual conversion
	POP	AX		;restore pointer to converted number in DAC
cEnd

;***
;B$FMDF - MKDMBF$ function
;
;Purpose:
; Given an 8-byte d.p. number in IEEE format, return a pointer to
; an sd containing the d.p. number in Microsoft Binary Format.
;
;Entry:
; R8parm - a d.p. number
;
;Exit:
; [AX]	= pointer to a string descriptor
;
;Uses:
; Per convention
;******************************************************************************
cProc	B$FMDF,<PUBLIC,FAR,NODATA> ;[6][3]
parmQ	R8parm			
cBegin				
	MOV	BX,8		; 8 Bytes in d.p. # (required string size)
	LEA	AX,R8parm	
	CALL	MK_MBF_SHARED	
cEnd				

;***
;B$FMSF - MKSMBF$ function
;
;Purpose:
; Given a 4-byte s.p. number in IEEE format, return a pointer to
; an sd containing the s.p. number in Microsoft Binary Format.
;
;Entry:
; R4parm - a s.p. number
;
;Exit:
; [AX]	= pointer to a string descriptor
;
;Uses:
; Per convention
;******************************************************************************
cProc	B$FMSF,<PUBLIC,FAR,NODATA> ;[6][3]
parmD	R4parm
cBegin				
	MOV	BX,4		; 4 Bytes in s.p. # (required string size)
	LEA	AX,R4parm	
	CALL	MK_MBF_SHARED	
cEnd				

cProc	MK_MBF_SHARED,<NEAR>,<ES,SI,DI>	;[6]
cBegin
	XCHG	AX,SI		; DS:SI = source address
	PUSH	BX		; save size of number
	CALL	B$STALCTMP	;allocate temp string; DX points to data area
	MOV	DI,DX		;DI = target address
	PUSH	DS
	POP	ES		;ES:DI = target address
	POP	CX		; restore size of number
	PUSH	BX		;save address of string descriptor (for retval)

	CALL	Mx_Ix		; do the actual conversion
	POP	AX		;restore pointer to string descriptor
cEnd

sEnd	ST_TEXT			

	END
