	TITLE	READ - Read and Restore statements
	PAGE	56,132
;***
; READ - Read and Restore statements
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************
	INCLUDE switch.inc	;Rutime switch file
	INCLUDE rmacros.inc	;General runtime macros

;Code segments
	useSeg	<INIT_CODE>	;Initialization
	useSeg	DK_TEXT
	useSeg	MT_TEXT
	useSeg	ST_TEXT
	useSeg	NH_TEXT
	useSeg	ER_TEXT
	useSeg	<XIB>		; XIB and XIE must bracket XI!
	useSeg	<XI>		;initializer segment
	useSeg	<XIE>		

;Data segments
	useSeg	_DATA
	useSeg	_BSS

	INCLUDE seg.inc 	;Segment definitions
	INCLUDE addr.inc	;Frame and module specific data definitions
	INCLUDE baslibma.inc	; get SKIP macro
	INCLUDE idmac.inc	; debug macros
	INCLUDE rtps.inc	
	INCLUDE pointers.inc	

	INITIALIZER	B$xRDINI	;put B$xRDINI in initializer list.

	SUBTTL	location constant definitions
	page

	InpDefault	EQU	0FFH

	TTY		EQU	0H

	SUBTTL	data definitions
	page

sBegin	_DATA

	externW b$pGetValDefault ; conditional vector for default read
				 ; routine B$READVAL
	externW B$AC

	externW b$cCSubs	; flag indicating compiler in execution
sEnd	;_DATA

sBegin	_BSS
	externB b$VTYP 	;defined in GLOBAL.INC
	externW b$GetOneVal	;function to get next input element

sEnd	;_BSS

	SUBTTL	code segment externals
	page

externFP B$IRDPTR		; return pointer to DATA
externFP B$IRDPTRUPD		; update pointer to DATA

sBegin	MT_TEXT
	externNP	B$FIN
	externNP	B$GETCH	
sEnd	;MT_TEXT

sBegin	ST_TEXT
	externFP	B$ASSN 	
sEnd

sBegin	ER_TEXT
	externNP	B$ERR_OD
	externNP	B$ERR_SN
sEnd	;ER_TEXT

sBegin	NH_TEXT
	externNP	B$GETMODCODE	;Get module specific code
sEnd	;NH_TEXT



	assumes CS,INIT_CODE				
sBegin	INIT_CODE					
;***
;B$xRDINI - Initialize default input vector
;PLM B$xRDINI()
;
;Purpose:
;	Added with revision [18].
;	Initialize default input vector for READ statement.
;
;Entry:
;	None.
;
;Exit:
;	b$pGetValDefault - contains near pointer to B$READVAL
;	b$GetOneVal - contains near pointer to B$READVAL
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$xRDINI,<FAR>
cBegin
	MOV	AX,DK_TEXTOFFSET B$READVAL	;get address of default routine
	MOV	b$pGetValDefault,AX		;store it in default vector
	MOV	b$GetOneVal,AX			;and in actual use vector
cEnd
sEnd	INIT_CODE

	assumes CS,DK_TEXT
sBegin	DK_TEXT

	SUBTTL	B$RSTA -- restore pointer to the begining of the DATA list
	PAGE
;***
;B$RSTA -- restore pointer to the begining of the DATA list
;void B$RSTA (void)
;
;Purpose:
; Restore the pointer to the begining of the DATA list.
;
;Entry:
; none
;
;Exit:
; [AX]	= offset of first byte of DATA in the module
; [BX]	= offset of the module specific data area
; data pointer in module_data location (OFD_DATA) is set to the begining of the
; data list. restore flag in module_data location (OFD_RESTORE) is set.
;
;Uses:
; per convention
;
;*******************************************************************************
cProc	B$RSTA,<PUBLIC,FAR,FORCEFRAME> 
cBegin
	MOV	AL,OF_DAT	
	cCall	B$GETMODCODE	;[3] get module data base
	PUSH	AX		; save pointer to module data
	MOV	AL,OF_DS	
	cCall	B$GETMODCODE	; on return, AX=*data
	POP	BX		; get back pointer to module_data
	MOV	[BX].OFD_DATA,AX ;save current data statement pointer
	MOV	[BX].OFD_RESTORE,1 ;note the restore was done
cEnd				;exit to caller

	SUBTTL	RESTORE interface -- RESTORE <line #>
	page
;***
;B$RSTB -- restore DATA list to a specific line number
;void B$RSTB(I2 lineNum)
;
;Purpose:
;	Reset data pointer to a specific line #.
;Entry:
;	Parameter is in stack.
;	U2	LineNum
;Exit:
;	data pointer in module_data location (OFD_DATA) is set to the
;	specified line number.
;	restore flag in module_data location (OFD_RESTORE) is set.
;Uses:
;	none
;Exceptions:
;	none
;*******************************************************************************

cProc	B$RSTB,<PUBLIC,FAR>,<DI,ES>
	ParmW	LineNum 	;U2 line number
cBegin
	cCall	B$RSTA 	; [AX] = ptr to first data, [BX] = mod data
	XCHG	DI,AX		; [DI] = ptr to first data
	DEC	DI
	DEC	DI		;Point to first line number address
	PUSH	DS		
	POP	ES		; make sure ES=DS
	XOR	AX,AX		;AX=0
	MOV	DX,LineNum	;get line #
	JMP	SHORT RSTB_10	

RSTB_5: 			
	XCHG	AX,DX		;AX=0, DX=line #
	MOV	CX,-1		;Allow maximum search range
	REPNE	SCASB		;Scan for zero, which is EOL
RSTB_10:			
	XCHG	AX,DX		;AX=line #, DX=0
	SCASW			;compare requested address with this address
	JA	RSTB_5		; If this line is as big, use it

	MOV	[BX].OFD_DATA,DI ;save new pointer
cEnd				;exit to the caller

	SUBTTL	READ interfaces -- B$RD<type>
	page
;***
;B$RD<type> -- assign a value into the variable
;void B$RD<type> (<type> far *pDest, [I2 cbDest])
;
;	Where,
;		<type>	=	I2:	Two byte integer
;				I4:	Four byte integer
;				R4:	Single precision real
;				R8:	Double precision real
;				SD:	String descriptor
;				FS:	Fixed length string
;
;Purpose:
;	Get the input value and assign it to the variable.  These routines
;	are shared by READ and INPUT [#] statements.
;
;	The functionality of these assignment routines, B$RD<type>, may be
;	roughly split into two parts -- namely, getting one value and then
;	assigning it to the variable.  General speaking, the second part of
;	the assignment routines DOESN'T discriminate among the statements
;	which share its use.  However, the first part of the assignment
;	routines MUST discriminate among the statements which share its use.
;
;	In order to achieve this, the first part of the assignment routines
;	uses an indirect call, CALL [b$GetOneVal], to get one value.
;	[b$GetOneVal] is default to the address of B$ReadVal, which gets one
;	value for READ stmt, or may contain either the address of TTYInpVal
;	for TTY input or the address of DskInpVal for disk (device) input.
;
;	B$RD<type> only sets the type, [b$VTYP], and then jump to a common
;	routine, CommRead, to perform most works there.  The stack frame is
;	not set in B$RD<type>; instead, it is set in CommRead.
;
;	B$InpReset should be called to reset the flag and variables in
;	the routine B$LNIN and B$PEOS.
;
;Entry:
; pDest 	= far pointer to the destination for the data
; cbDest	= for SD and FS only, a byte count of the length of the fixed
;		  length string, or zero indicating an SD.
;
;	[b$GetOneVal] = OFFSET B$ReadVal if READ stmt
;	[b$GetOneVal] = OFFSET TTYInpVal if INPUT from TTY
;	[b$GetOneVal] = OFFSET DskInpVal if INPUT from disk (device)
;Exit:
;	Via CommRead.  When call CommRead, BL has the type
;Uses:
; per convention
;Exceptions:
;	if b$FInput = InpDefault (for READ stmt)
;		B$ERR_OD -- out of DATA
;		B$ERR_SN -- syntax error
;	if b$FInput = InpDsk (for disk (device) input)
;		B$ERR_RPE -- read pass end
;		B$ERR_OV -- overflow
;*******************************************************************************

cProc	B$RDI2,<PUBLIC,FAR>

cBegin	nogen			;no code generated
	MOV	BL,VT_I2	; set type
	SKIP	2		; skip next two bytes
cEnd	nogen

cProc	B$RDI4,<PUBLIC,FAR>

cBegin	nogen			;no code generated
	MOV	BL,VT_I4	; set type
	SKIP	2		; skip next two bytes
cEnd	nogen

cProc	B$RDR4,<PUBLIC,FAR>

cBegin	nogen			;no code generated
	MOV	BL,VT_R4	; set type
	SKIP	2		; skip next two bytes
cEnd	nogen

cProc	B$RDR8,<PUBLIC,FAR>

cBegin	nogen			;no code generated
	MOV	BL,VT_R8	; set type
	SKIP	2		; skip next two bytes
cEnd	nogen


cProc	B$RDSD,<PUBLIC,FAR>

cBegin	nogen			;no code generated
	MOV	BL,VT_SD	; set type
cEnd	nogen

	SUBTTL	common assign routine
	page
;***
;CommRead -- Common assign routine for input & read
;
;Purpose:
;	Assign the value into the variable.  This is the common assigning
;	routine for READ, and INPUT [#] statements.  This routine first
;	gets the value via indirect call to [b$GetOneVal], and then
;	assign it to the destination, which is passed in stack.
;Entry:
;	Parameter is in stack.
;	item	far *pDest; where item is I2, I4, R4, R8 or SD
;
;	[BL]		= type
;	[b$FInput]	= input flag
;	[b$GetOneVal]	= offset of get one value routine for READ, InpTTY,
;				or InpDsk
;Exit:
;	variable is assigned
;Uses:
;	none
;Exceptions:
;	if b$FInput = InpDefault (for READ stmt)
;		B$ERR_OD -- out of DATA
;		B$ERR_SN -- syntax error
;	if b$FInput = InpDsk (for disk (device) input)
;		B$ERR_RPE -- read pass end
;		B$ERR_OV -- overflow
;*******************************************************************************

cProc	CommRead,<PUBLIC,FAR>,<SI,DI,ES>
	ParmD	pDest		; far *item of destination
cBegin
	MOV	[b$VTYP],BL	;save type
	PUSH	BX		; save for later

	PUSH	DS		
	POP	ES		; make sure ES=DS
	CALL	[b$GetOneVal]	; on return SI = *item

	POP	CX		; [CL] = valtype again
	SHR	CL,1		;is string ? (NOTE: CL=1,2,4 or 9 now)
	JB	AssignStr	;Brif yes

	GETPTR	,ES,DI,pDest,,<SIZE,LOAD>   ; ES:DI has the destination
	AND	CX,7H		; [CX] = word count of move
	REP	MOVSW		;store in destination

cEnd				;pop registers and exit to caller

cProc	AssignStr,FAR,<SI,DI,ES>
parmD	pDest			; far pointer to destination
parmW	cbDest			; length thereof
cBegin	nogen			; arelread set up in commread

	XOR	AX,AX		
	cCall	B$ASSN,<DS,[SI],AX,seg_pDest,off_pDest,cbDest> 
cEnd

	SUBTTL	READ supporting routine -- B$ReadVal
	page
;***
;B$ReadVal -- read one value for READ statement
;
;Purpose:
;	Read one value from DATA list for READ statement.
;
;	This routine first gets the pointer to the module_data, which stores
;	the information for RESTORE flag, pointer to DATA list, etc.  It then
;	gets the pointer to the begining of the DATA list, and reset it to
;	the begining of the DATA list if RESTORE has been requested.
;
;	Next this routine gets one value by calling B$GETCH & B$FIN.	B$GETCH
;	purges the leading white spaces from the DATA list, and then B$FIN
;	converts it to the value desired.  B$FIN needs [b$VTYP] being set and
;	SI pointing to the source character stream.
;
;	Finally, this routine moves the pointer of the DATA list to the next
;	item.
;Entry:
;	[b$VTYP]	= type
;Exit:
;	[SI]		= *value
;	OFD_DATA in module_data location is updated
;Uses:
;	none
;Exceptions:
;	B$ERR_OD -- out of data
;	others will handled by B$RUNERR
;*******************************************************************************

cProc	B$ReadVal,<PUBLIC,NEAR>,<DI,ES>

localV	buf,100 		; local buffer for stuff
cBegin				
;
; WARNING: we set DS to a NON-DGROUP value for the operations below. This is
; so that we can use B$GETCH unmodified. NOTE however, that this breaks the
; KANJI version of GETCH, which we'll have to fix someday.
;
; In reality, DS is set to a non-DGROUP value, ONLY if we are in the
; interpreter, and we are trying to read a string. All other cases are forced
; DGROUP, either by virtue of the compilers data being there, or ther
; interpreters numeric data being placed in the local space allocated above.
;
DbAssertRel	SP,A,b$pendchk,DK_TEXT,<Insufficient stack space in B$ReadVal>
	PUSH	DS		; save DGROUP
	LEA	BX,buf		; give get address routine temp space


	cCall	GETADDR 	; [DS:SI] = [ES:SI] = DATA source pointer
				;NOTE:	DS != DGROUP !!
	POP	BX		; [BX] = DGROUP
	PUSH	BX		;	 ...on stack

	PUSH	SI		; Save starting pointer

	CALL	B$GETCH		;Skip over blanks
	MOV	DS,BX		; DS = DGROUP (before error checking)
	CMP	[b$cCSubs],0	; See if we were interpreting
	JZ	moreDATA	; Jump if interpreting
	DEC	AL		;end of data list?
	JZ	ERCOD		;out of DATA
moreDATA:			; not out of DATA

	DEC	SI		;Re-read first character
	PUSH	ES		; save segment of source
	CALL	B$FIN		;Get data item (DS:SI points, ES:SI if string)
				;AL = next byte of string
	POP	DS		; [DS] = source segment

	POP	BX		; [BX] = starting pointer
	SUB	BX,SI		; [BX] = negative difference
	NEG	BX		; [BX] = number of bytes eaten
	POP	DS		; restore DS
	CMP	AL,","		;More values on this line?
	JZ	ReadEnd
	OR	AL,AL		;Better be EOL (zero) if no more values
	JNZ	ERCSN		;Syntax error

ReadEnd:
	cCall	PUTADDR 	; store ending address
	MOV	SI,OFFSET DGROUP:B$AC
				;result in SI
	TEST	[b$VTYP],8	;is 8-byte value ?
	JZ	ReadValExit	;Brif not
	SUB	SI,4		;offset of B$DAC
ReadValExit:
cEnd				;pop di, exit to caller

ERCOD:	JMP	B$ERR_OD	;Out of Data
ERCSN:	JMP	B$ERR_SN	;Syntax error in DATA

;***
; GETADDR - get address of next available DATA item
;
;Purpose:
; Determines what environment we're in, and gets the address of the next
; available DATA item.
;
;Entry:
; [BX]	= offset of local storage to copy FAR numbers to. EI_QB only.
;
;Exit:
; [DS:SI] = [ES:SI] = far address of value.
; [DI]	= Compilers module data area offset. (If compiler)
;
;Uses:
;
;Preserves: (optional)
;
;Exceptions:
;
;******************************************************************************
cProc	GETADDR,NEAR		
cBegin

	CMP	[b$cCSubs],0	; See if we were interpreting
	JZ	GETADDR_50	; Jump if interpreting

	MOV	AL,OF_DAT	; offset of data
	cCall	B$GETMODCODE	; on return base of module_data is in AX
	XCHG	AX,DI		; module_data area pointer
	CMP	[DI].OFD_RESTORE,0 ;have we ever restored?
	JNZ	GETADDR_5	
	cCall	B$RSTA 	; set the pointer to the begining of data
GETADDR_5:			
	MOV	SI,[DI].OFD_DATA ;Get pointer to next data item

	JMP	SHORT GETADDR_90 
GETADDR_50:			
	PUSH	DS		; save dgroup
	PUSH	BX		; [BX] is address of temp buffer
	cCall	B$IRDPTR	; [DS:SI] = pointer to data
	POP	DI		
	POP	ES		; [ES:DI] = address of temp buffer
	.erre	ID_SSEQDS	; temp buffer is on stack
	CMP	SS:[b$VTYP],VT_SD ; is this a string?
	JZ	GETADDR_90	; Then skip the local copy
	PUSH	DI		
	MOV	CX,50		
	REP	MOVSW		; copy to buffer
	PUSH	ES		
	POP	DS		; [DS] = DGROUP
	POP	SI		; [DS:SI] = pointer to local buffer
GETADDR_90:			
	PUSH	DS		
	POP	ES		

cEnd				

;***
; PUTADDR - Save updated READ pointer
;
;Purpose:
; Updates the appropriate READ data pointer.
;
;Entry:
; [AL]	= 0 if EOL, else end of item.
; [BX]	= number of bytes eaten (Interpreter only)
; [DI]	= Compilers module data area offset
; [SI]	= DATA offset to be stored (Compiler only)
;
;Exit:
; none
;
;Uses:
; Might adjust SI.
;
;******************************************************************************
cProc	PUTADDR,NEAR
cBegin

	CMP	[b$cCSubs],0	; See if we were interpreting
	JZ	PUTADDR_50	; Jump if interpreting
	OR	AL,AL		; End of line?
	JNZ	PUTADDR_5	; jump if not
	INC	SI		;Skip over line number
	INC	SI
PUTADDR_5:
	MOV	[DI].OFD_DATA,SI ;Save pointer

	JMP	SHORT PUTADDR_90	

PUTADDR_50:			
	cCall	B$IRDPTRUPD	; update read pointer

PUTADDR_90:			

cEnd				

sEnd	;DK_TEXT
	END
