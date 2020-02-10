	TITLE	LININP - LINE INPUT statement
	page	56,132
;***
; LININP - LINE INPUT statement
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - LINE INPUT # Statement:
;
;      LINE INPUT # filenumber, string-var
;      -----------------------	   |
;		 |		   |
;	       B$DSKI		  B$LNIN
;
;
; - LINE INPUT Statement:
;
;      LINE INPUT [;] ["prompt string";] string-var
;      --------------------------------------------
;			  |
;			  |
;			B$LNIN
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE string.inc	

;Code segments
	useSeg	DK_TEXT
	useSeg	NH_TEXT
	useSeg	ST_TEXT
	useSeg	CN_TEXT
	useSeg	RT_TEXT
;Data segments
	useSeg	_DATA
	useSeg	_BSS

	INCLUDE seg.inc
	INCLUDE idmac.inc	

	SUBTTL	location constant definitions
	page

	InpDsk		EQU	1H	;device (disk) I/O

	SUBTTL	data definitions
	page

sBegin	_DATA
	externB b$FInput	;input flag
	externW	b$PTRFIL	
	externW	b$SDBuf1	

sEnd	;_DATA

	SUBTTL	code externals
	page

sBegin	NH_TEXT
	externNP	B$STGETSTRLEN
	externNP	B$STALCTMPSUB	
sEnd	;NH_TEXT

sBegin	ST_TEXT
	externFP	B$ASSN		
	externNP	B$STDALCTMP	; deallocate if temp string
sEnd	;ST_TEXT

sBegin	CN_TEXT
	externNP	B$TYPSTR
	externNP	B$RDLIN
	externNP	B$INPCRLF	
sEnd	;CN_TEXT

sBegin	RT_TEXT
sEnd	;RT_TEXT

sBegin	DK_TEXT
	externNP	B$FillBuf	;get one input item from a disk file
	externNP	B$InpReset	;reset flag & variables
sEnd

	assumes CS,DK_TEXT
sBegin	DK_TEXT

	SUBTTL	LINE INPUT interface -- B$LNIN
	page
;***
;B$LNIN -- LINE INPUT [#] interface
;void B$LNIN (sd *psdPrompt, far *pDest, I2 cbDest, I2 fCRLF)
;
;Purpose:
; This is the interface for LINE INPUT stmt to assign the input value into the
; destination (a stringVar). This interface is shared by either console LINE
; INPUT or device (disk) LINE INPUT. Since the device (disk) LINE INPUT doesn't
; give the prompt, parameters, psdPrompt and fCRLF, are invalid for device
; (disk) input.
;
; The algorithm is as follows:
;
; if not device (disk) I/O
;	print prompt, if there is any
;	read one line into buffer (b$Buf1)
;	force CR if fCRLF says so
; else
;	 read one line into buffer (SI points to, could be temp)
; endif
; assign to the destination
; reset b$FInput to InpDefault (for READ stmt)
; reset b$GetOneVal to B$ReadVal (for READ stmt)
; reset b$PTRFIL to TTY
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement.
;
;Entry:
; sdPrompt	= prompt sd
; pDest 	= far pointer to either the destination sd, (cbDest == 0) or
;		  the actual address of the destination (cbDest <> 0)
;			Note: if EI_EB, then the real SD is on the stack
;			      instead of a far pointer to the SD
; cbDest	= Byte count of the fixed length string to receive the data. 0
;		  if a variable length string.
; fCRLF 	= CRLF flag. Set to indicate a ";" was specified prior to the
;		  prompt string.
;
; Also, if [b$FInput]=InpDsk [b$PTRFIL] is set
;
;Exit:
;	if device (disk) I/O then
;		b$FInput, b$GetOneItem and b$PTRFIL are reset to default
;
;Uses:
; per convention
;
;*******************************************************************************
cProc	B$LNIN,<PUBLIC,FAR>,<SI>
parmSD	sdPrompt	; sd of the prompt
parmD	pDest		; far *sd to the destination (or sd if EI_EB)
parmW	cbDest		; length of destination
parmW	fCRLF		; I2 of input flag
cBegin

	CMP	[b$FInput],InpDsk	;disk I/O ?
	JZ	DiskIO		;Brif yes

	GetpSD	BX,sdPrompt	;BX=*sd to the prompt
	PUSH	BX		; save that
	CALL	B$TYPSTR 	;print prompt if any
	POP	BX		
	cCall	B$STDALCTMP	; deallocate prompt if it was temp

	CALL	B$RDLIN 	;read one line into b$Buf1

	TEST	fCRLF,FINP_CrLf ;need a CR ?
	JNZ	LNIN_5		; brif not
	CALL	B$INPCRLF	; force a CR to all sources where keyboard
				; was written (screen and/or redir. file).
LNIN_5: 			
	MOV	SI,OFFSET DGROUP:b$SDBuf1 ; [SI] = pointer to source
	JMP	SHORT Assign	;go assign
DiskIO:
	XOR	DX,DX		;tell B$FillBuf this is for LINE INPUT
	cCall	B$FillBuf	;get one line into buffer pointed by SI
	cCall	B$InpReset	;reset flag and variables

Assign:
	MOV	BX,[SI+2]	; [BX] = pointer to string data
	CALL	B$STGETSTRLEN	; [AX] = string length
	MOV	BX,SI		
	CMP	AX,[SI] 	; already correct length?
	JZ	Assign_5	; then go assign
	XOR	CX,CX		; [CX] = start offset of string
	XCHG	AX,DX		; [DX] = correct length
	cCall	B$STALCTMPSUB	; [BX] = sd of shorter substring
Assign_5:			
	XOR	AX,AX		
	cCall	B$ASSN,<DS,BX,AX,Seg_pDest,Off_pDest,cbDest> ; Assign
	MOV	[b$PTRFIL],0	; Reset it to default

cEnd				;pop si, exit to caller

sEnd	;DK_TEXT

	END
