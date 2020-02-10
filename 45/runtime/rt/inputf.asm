	TITLE	INPUTF - INPUT$ function
	page	,132
;***
; INPUTF - INPUT$ function
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - INPUT$ Function:
;
;      INPUT$(x [,[#]y])
;	  |
;	B$FINP
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

;Code segments			
	useSeg	RT_TEXT 	
	useSeg	DK_TEXT 	
	useSeg	NH_TEXT 	
	useSeg	ER_TEXT 	
;Data segments			
	useSeg	_BSS		
	UseSeg	_DATA		

	INCLUDE seg.inc 	
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE idmac.inc	
	INCLUDE string.inc	

	SUBTTL	local contant definitions	
	page

	TTY	EQU	0	; [b$PTRFIL] is default to TTY

	SUBTTL	data definitions
	page			


sBegin	_BSS			
	externW b$PTRFIL	; defined in GOSTOP.ASM
	externW b$RECPTR	; pointer to random record
sEnd	;_BSS			

	SUBTTL	code externals	
	page			

sBegin	DK_TEXT 		
	externNP B$$RCH		
	externNP B$PtrDispatch	
sEnd	;DK_TEXT

sBegin	NH_TEXT 		
	externNP B$LHFDBLOC	
	externNP B$STALCTMPSUB	
	externNP B$STALCTMP	

sEnd	;NH_TEXT

sBegin	RT_TEXT 		
	externNP B$END		
sEnd	;RT_TEXT		

sBegin	ER_TEXT 		
	externNP B$ERR_IFN	
	externNP B$ERR_RPE	
	externNP B$ERR_FC	
sEnd	;ER_TEXT		

sBegin	_DATA			
	globalW  B$ENTRY_BUFCNT,0 
sEnd	;_DATA			

sBegin	DK_TEXT 		
	assumes CS,DK_TEXT	
	SUBTTL	INPUT$ interface -- B$FINP	
	page			

ERCIFC:	JMP	B$ERR_FC	; illegal function call
BADNUM: JMP	B$ERR_IFN	

;***[4]
;B$FINP -- INPUT$ interface
;sd *B$FINP(I2 cbInp,I2 channel)
;DBCS-callback
;
;Purpose:
;	Return a string of n bytes read from the keyboard or from a file.
;	The input device is specified by the file descriptor called
;	channel.  This routine does not handle double byte characters.
;	See the function B$INPY (INPUT<yen>) for a version that will
;	read n characters (handling double byte characters correctly).
;
;Entry:
;	Parameters are in stack.
;	int	cbInp
;	int	channel; (0 or 7FFFH for TTY, otherwise a file)
;
;Exit:
;	[AX]	= *sd (pointer to a string descriptor)
;
;Uses:
;	none
;
;Exceptions:
;	B$ERR_IFN -- illegal file number
;	B$ERR_RPE -- read pass end
;*******************************************************************************

cProc	B$FINP,<PUBLIC,FAR>,<SI,DI,ES> 
	ParmW	cbInp		; I2 number of characters
	ParmW	Channel 	; I2 channel number
cBegin				
	PUSH	DS		
	POP	ES		; Set ES = DS
	MOV	CX,cbInp	; number in CX
	MOV	BX,Channel	; file number in BX

	XOR	SI,SI		; assume TTY input
	OR	BX,BX		; is zero (TTY) ?
	JZ	GetTmpStr	; Brif yes

	CMP	BL,255		; Is it TTY ?
	JZ	GetTmpStr	; Brif yes

	cCall	B$LHFDBLOC	; on return, NZ & SI=*FDB
	JZ	BADNUM		;illegal file number?
	FDB_PTR ES,SI,SI	;(ES:)[SI] = *FDB
	MOV	AX,FileDB.FD_BUFCNT ; Save fdb position.
	MOV	B$ENTRY_BUFCNT,AX   ; Used to reset on error
	TEST	FileDB.FD_MODE,MD_SQO+MD_APP ; output or append?
   DJMP JNZ	ERCRPE		; Brif yes

GetTmpStr:			
	MOV	[b$PTRFIL],SI	; [b$PTRFIL] has 0 or *FDB
	or	cx,cx		; > 32K length requested?
	js	ERCIFC		; brif so -- illegal function call	
	MOV	BX,CX		; length in BX
	cCall	B$STALCTMP	; allocate temp string, on return
				;  BX=*sd, DX=*temp string
	JCXZ	InpExit 	; exit (return a *sd to a null string)
	PUSH	BX		; save BX
	OR	SI,SI		; TTY input?
	JZ	NotBinary	; brif so -- not binary mode
	FDB_PTR ES		;refresh ES after possible global movement
	TEST	FileDB.FD_MODE,MD_BIN ; binary mode?
	JZ	NotBinary	; brif not -- don't load directly
				; load directly into the string
	MOV	[b$RECPTR],DX	; b$RECPTR = buffer pointer (seg and off)
	MOV	[b$RECPTR+2],DS 
	MOV	BX,CX		; BX = requested count
	MOV	AX,DV_RANDIO SHL 8 + 100B ;  AH = dispatch function (RANDIO)
				; AL = flag [record var, no rec #, GET]
	PUSH	CX		; save count
	CALL	B$PtrDispatch	; do the load -- AX = actual count read
				; BX, CX, DX unknown
	POP	CX		; restore count
	POP	BX		; get back SD
	CMP	CX,AX		; did we get the full count we requested?
	JE	InpExit		; brif so -- exit
				; string we have is too long
	XCHG	DX,AX		; DX = actual count
	XOR	CX,CX		; CX = copy from start of string
	CALL	B$STALCTMPSUB	; allocate temporary substring, deleting old
	JMP	SHORT InpExit	; leave the loop

NotBinary:			
	MOV	DI,DX		; put data pointer in DI

FillStr:
	cCall	B$$RCH		; read one character

	JZ	EndRedir	; end of redirected input seen, exit program.
	JC	ERCRPE		; Brif end of file
	STOSB			; put in string, DI incremented
	LOOP	FillStr 	; repeat until CX=0

	POP	BX		; result (*sd) in BX
InpExit:			
	XCHG	AX,BX		; return new SD in AX
	MOV	[b$PTRFIL],TTY	; reset [b$PTRFIL] to default
cEnd				; exit

ERCRPE: JMP	B$ERR_RPE

EndRedir:
	JMP	B$END		;jump to exit abruptly


sEnd	;DK_TEXT		
	END
