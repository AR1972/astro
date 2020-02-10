	TITLE	STRING - String assignment, concatenate, compare
	PAGE	56,132
;***
; STRING - String assignment, concatenate, compare
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; Fixed length string manipulations
;
;     a$ = a.foo	a.foo = a$	swap a.foo a.bar
;	  |		    |			|
;	B$ASSN		  B$ASSN 	      B$SWPN
;
; - STR$ Function - calls $STI for integer arg, $STR for s.p., or $STD for d.p.:
;
;      v$ = STR$(x)
;
;    Examples:
;
;      v$ = STR$(50)	v$ = STR$(b&)
;	     |		      |
;	   B$STI2	    B$STI4
;
;******************************************************************************
	INCLUDE switch.inc	
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	useSeg	_DATA
	USESEG	_BSS		
	useSeg	ST_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE	baslibma.inc	
	INCLUDE idmac.inc	

externFP B$LSET 		

sBegin	_BSS
externB b$VTYP			; defined in GWDATA.ASM
sEnd	_BSS

externFP B$SASS 		

sBegin	ST_TEXT 		

externNP B$STALCTMP		
externNP B$STALCTMPSUB		
externNP B$STDALC		
externNP B$STDALCTMP		
externNP B$STSWAPDESC		; swap string descriptors
externNP B$STALCTMPCPY		
externNP B$FOUTBX		

	ASSUMES CS,ST_TEXT	

	SUBTTL	STR$ - Create String from number
	PAGE
;***
;B$STI2, B$STI4 - STR$ function support
;
;Purpose:
;	Runtime Entry Points
;	Create a string representing the number in ASCII
;	Moved here with revision [21].
;Entry:
;	parameter value is on the stack (I2, or I4)
;Exit:
;	AX = Address of string descriptor
;Uses:
;	Per Convention
;Exceptions:
;	Out of memory
;****
cProc	B$STI2,<PUBLIC,FAR>
ParmW	I2Arg
cBegin
	MOV	AL,VT_I2	;AL = data type
	LEA	BX,I2Arg	;BX = ptr to data
	cCall	B$STR_COMMON	
cEnd

cProc	B$STI4,<PUBLIC,FAR>
parmD	arg4
cBegin
	MOV	AL,VT_I4	;AL = data type
	LEA	BX,arg4
	cCall	B$STR_COMMON	
cEnd

;***
;B$STR_COMMON - support routine for STR$ function
;
;Purpose:
;	Converts numeric data to a string.
;Entry:
;	AL - data type
;	BX - pointer to data
;Exit:
;	AX - pointer to sd containing converted string
;Uses:
;	Per convention
;Exceptions:
;	Out of memory
;****
cProc	B$STR_COMMON,<PUBLIC,NEAR> 
cBegin
	MOV	b$VTYP,AL	; store data type
	CALL	B$FOUTBX	;do conversion - [AX] = cbStr, [BX] = pszStr
	XCHG	AX,BX		;Get length in BX
	XCHG	AX,DX		;Get address in DX
	CALL	B$STALCTMPCPY	;Create temp string
	XCHG	AX,BX		;return psd in AX
cEnd

	SUBTTL	B$LDFS - load fixed length string
	PAGE
;***
; B$LDFS - load fixed length string
; ushort B$LDFS(char far *pString, ushort cbString)
;
;Purpose:
;	Given a far pointer to a string (not zero-terminated) and count
;	of bytes in the string, allocate a string temporary, copy the
;	string data into it, and return a pointer to the temp string sd.
;
;Entry:
;	pString -	a far pointer to string data to copy
;	cbString -	count of bytes of string data to copy
;
;Exit:
;	AX = ptr to the newly allocated string temp.
;Uses:
;	none.
;
;Exceptions:
;	Runtime error can occur (such as 'insufficient string space'), in
;	which case control is transfered to runtime error code.
;******************************************************************************
cProc	B$LDFS,<PUBLIC,FAR>,<ES,SI,DI>
parmD	pString
parmW	cbString
cBegin	B$LDFS 			
	PUSH	DS		; string package assumes ES = DS
	POP	ES		
	MOV	BX,[cbString]	;input to B$STALCTMP
	PUSH	BX		;save string length
	CALL	B$STALCTMP	;BX = psdTmp, DX = pData for temp string
	POP	CX		;CX = string length
	PUSH	BX		;save for retval
	MOV	DI,DX		;ES:DI = destination
	PUSH	DS		;save across block copy
	LDS	SI,[pString]	;DS:SI = given string pointer
	INC	CX		;round up if odd
	SHR	CX,1		;CX = string length in words
	REP	MOVSW		;copy string
	POP	DS		;restore
	POP	AX		;retval - psdTmp
cEnd	B$LDFS

	SUBTTL	B$ASSN - fixed length string assignment
	PAGE
;***
;B$ASSN - fixed length string assignment [7]
;void pascal B$ASSN(data far *pSrc, I2 cbSrc, data far *pDest, I2 cbDest)
;
;Purpose:
; Assign the contenst of a fixed or variable length string to a fixed or
; variable length string.
;
;Entry:
; pSrc	= far pointer to source data or sd
; cbSrc = length of source string, or 0 if sd
; pDest = far pointer to destination data or sd
; cbDest= length of destination string, or 0 if sd
;
;Uses:
; per convention
;
;******************************************************************************
labelFP <PUBLIC,B_ASSN> 	;Interpreter reachable entry
cProc	B$ASSN,<FAR,PUBLIC>,<DI,SI,ES>
parmD	pSrc			;far pointer to source data or sd
parmW	cbSrc			;length of source string, or 0 if sd
parmD	pDest			;far pointer to destination data or sd
parmW	cbDest			;length of destination string, or 0 if sd
cBegin
	LES	SI,pSrc 	;[ES:SI] = pointer to source data
	MOV	BX,ES		;[BX:SI] = pointer to source data
	MOV	CX,cbSrc	;[CX] = length of source

	LES	DI,pDest	;[ES:DI] = pointer to destination
	MOV	AX,cbDest	;[AX] = length of destination fs
	OR	AX,AX		; Set flags on that.
	JCXZ	ASSN_50 	;Jump if the source is an sd
;
; source is a fixed length string, [BX:SI] points to data, [CX] has length
;
	JZ	ASSN_10 	; jump if destination is a string descriptor
;
; destination is an fs, [ES:DI] points to data, [AX] has length
;
	SUB	AX,CX		;[AX] = dest length - source len
	JNC	ASSN_30 	;Jump if source less than destination
;
; src>dst, copy cbDest bytes with no padding
;
	MOV	CX,cbDest	;[CX] = len to copy (dst)
	XOR	AX,AX		;[AX] = length to pad 0 since dest len used
;
; fixed length string assignment
;  [BX:SI]	= address of source data
;  [ES:DI]	= address of destination data
;  [CX] 	= number of bytes to copy
;  [AX] 	= number of bytes to pad
;
ASSN_30:
	PUSH	DS		;Save this
	MOV	DS,BX		;[DS:SI] = address of source data
	REP	MOVSB		;copy copy-able data
	XCHG	AX,CX		;get pad count
	MOV	AL," "		;get pad data
	REP	STOSB		;pad data
	POP	DS		;restore
	JMP	SHORT ASSN_90	;go exit
;
; destination is a string descriptor. create a temp string out of the fs,
; and then do a normal string assignment.
;
ASSN_10:
	cCall	B$LDFS,<BX,SI,CX> ;[AX] = address of temp sd
ASSN_15:
	cCall	B$SASS,<AX,DI> ;do normal string assign
	JMP	SHORT ASSN_90	;and quit
;
; Source is an sd. Use LSET
;
ASSN_50:
	XCHG	AX,SI		; [AX] = source sd, [SI] = cbDest
	JZ	ASSN_15 	; jump if target is also an sd
	cCall	B$LSET,<AX,ES,DI,SI>

ASSN_90:

cEnd

	SUBTTL	B$SWPN = SWAP strings
	PAGE
;***
; B$SWPN - Swap strings
; void pascal B$SWPN(data far *pOp1, I2 cbOp1, data far *pOp2, I2 cbOp2)
;
;Purpose:
; swap fixed or variable length strings
;
;Entry:
; pOp1	= pointer to first string operand
; cbOp1 = length thereof
; pOp2	= pointer to second string operand
; cbOp2 = length thereof
;
;Exit:
; swapped
;
;Uses:
; per convention
;
;******************************************************************************
cProc	B$SWPN,<FAR,PUBLIC>,<DI,SI,ES,DS>
parmD	pOp1			;pointer to first string operand
parmW	cbOp1			;length thereof
parmD	pOp2			;pointer to second string operand
parmW	cbOp2			;length thereof
cBegin

	LES	DI,pOp2 	;[ES:DI] points to second one
	MOV	DX,cbOp2	;[DX] = length of #2
	LDS	SI,pOp1 	;[DS:SI] points at the first
	MOV	CX,cbOp1	;[CX] = length of #1

	MOV	BX,DX		;[BX] = length of #2
	SUB	BX,CX		;[BX] = len2 - len1 (Pad length)
	JNZ	SWPN_20 	;Jump if the lengths differ
	JCXZ	SWPN_50 	;Jump if both strings are sd's
;
; We have two fixed length strings, and their lengths are the same.
;
SWPN_10:
;
;  [ES:DI] points to one, [DS:SI] points to the other, [CX] has the length
;  to be swapped, and [BX] has the length to pad.
;
	MOV	AL,ES:[DI]
	XCHG	AL,DS:[SI]
	STOSB			;Bytes swapped
	INC	SI
	LOOP	SWPN_10 	;Loop for shorter of the strings

	MOV	CX,BX		;[CX] = length to pad
	MOV	AL," "		;[AL] = character to pad with
	REP	STOSB		;pad string
	JMP	SHORT SWPN_90	;Exit
;
; The lengths are different. At least one must be an FS.
;
SWPN_20:
	JCXZ	SWPN_40 	;Jump if [DS:SI] points to an sd
	XCHG	CX,DX		;[CX] = length of [ES:DI]
	JCXZ	SWPN_30 	;Jump if [ES:DI] points to an sd
;
; Both point to FS's, but their lengths differ
; NOTE: flags for the JNC were set WAY WAY up there at the SUB BX,CX
;
	XCHG	CX,DX
	JNC	SWPN_10 	;Jump if len2 > len1
	MOV	CX,DX		;[CX] = copy length (smaller of the lengths)
	NEG	BX		;[BX] = pad length (absolute difference)
	LES	DI,pOp1 	;[ES:DI] points to first one
	LDS	SI,pOp2 	;[DS:SI] points at the second
	JMP	SWPN_10 	;Go swap fixed length strings
;
; [ES:DI] points to an sd, [DS:SI] points to an FS, with length in [DX]
;
SWPN_30:
	LES	DI,pOp1 	;[ES:DI] points to first one
	LDS	SI,pOp2 	;[DS:SI] points at the second
;
; [DS:SI] points to an sd, [ES:DI] points to an FS, with length in [DX]
;
SWPN_40:
	PUSH	DX		;Save length of FS
	cCall	B$LDFS,<ES,DI,DX> ;[AX] = pointer to temp sd
	POP	DX		;[DX] = length of FS
	XCHG	AX,DI		;[DI] = pointer to temp sd
	XOR	BX,BX
	cCall	B$ASSN,<DS,SI,BX,ES,AX,DX>	;original sd to original fs
	cCall	B$SASS,<DI,SI> ;temp sd to original sd
	JMP	SHORT SWPN_90
;
; We have two string descriptors. Swap 'em
;
SWPN_50:
	cCall	B$STSWAPDESC	;else swap string descriptors

SWPN_90:

cEnd

	SUBTTL	B$SWSD = SWAP string descriptors
	PAGE
;***
; B$SWSD - Swap string descrtiptors
; void pascal B$SWSD(SI: *psd1,DI: *psd2)
; Added rev [17]
;
;Purpose:
; swap variable length strings
;
;Entry:
; SI	= pointer to first string operand
; DI	= pointer to second string operand
;
;Exit:
; swapped
;
;Uses:
; per convention
;
;******************************************************************************
cProc	B$SWSD,<FAR,PUBLIC>
cBegin
	cCall	B$STSWAPDESC	;swap string descriptors
cEnd


	SUBTTL	B$SCPY - copy string to temp string
	PAGE
;***
;B$SCPY - String copy to temporary string
;
;Entry:
;	parmW = SDESC of string to copy
;Exit:
;	AX = SDESC of temp string (copy)
;Uses:
;	Per convention
;Exceptions:
;	B$ERR_OM
;****
cProc	B$SCPY,<PUBLIC,FAR>
parmW	psdSource
cBegin
	XOR	CX,CX		;CX = offset = 0
	MOV	BX,psdSource	;get ptr to sd
	MOV	DX,[BX] 	;DX = length
	CALL	B$STALCTMPSUB	;create copy of string
	XCHG	AX,BX		;return psd in AX
cEnd

	SUBTTL	B$STDL - deallocate a string
	PAGE
;***
; B$STDL - deallocate a string
; void B$STDL(sd *)
;
;Purpose:
;	Given a pointer to an sd, deallocate it.
;Entry:
;	pSd -	a pointer to a string descriptor
;Exit:
;	none.
;Uses:
;	Per convention
;Exceptions:
;	none.
;******************************************************************************
cProc	B$STDL,<PUBLIC,FAR>	
parmW	pSd
cBegin	B$STDL 			
	MOV	BX,[pSd]
	PUSH	BX		
	CALL	B$STDALC
	POP	BX		
	MOV	WORD PTR [BX],0 ; Indicate deallcated with zero length
cEnd	B$STDL


	SUBTTL	B$SCPF - copy string to temp string for recursive functions
	PAGE
;***
;B$SCPF - String copy to temporary string for recursive string functions
;
;Entry:
;	parmW = SDESC of string to copy
;Exit:
;	AX = SDESC of temp string (copy)
;Uses:
;	Per convention
;Exceptions:
;	B$ERR_OM
;****
cProc	B$SCPF,<PUBLIC,FAR>	; new for recursive string functions
parmW	psdSource
cBegin
	push	psdSource
	call	B$SCPY 		;make a copy of string function result
	push	ax		;save it
	push	psdSource
	call	B$STDL 		;delete string function result
	pop	ax		;restore copy of string function result
cEnd

sEnd	ST_TEXT 		

	END
