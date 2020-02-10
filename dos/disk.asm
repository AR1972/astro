	TITLE	DISK - Disk utility routines
	NAME	Disk

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	Low level Read and write routines for local SFT I/O on files and devs
;
;	SWAPCON
;	SWAPBACK
;	DOS_READ
;	DOS_WRITE
;	get_io_sft
;	DirRead
;	FIRSTCLUSTER
;	SET_BUF_AS_DIR
;	FATSecRd
;	DREAD
;	CHECK_WRITE_LOCK
;	CHECK_READ_LOCK
;
;	Revision history:
;
;		A000   version 4.00  Jan. 1988
;
;----------------------------------------------------------------------------
;
; M065 : B#5276. On raw read/write of a block of characters if a critical
;		error happens, DOS retries the entire block assuming that
;		zero characters were transferred. Modified the code to take
;		into account the number of characters transfered before
;		retrying the operation.
;
;----------------------------------------------------------------------------
;
	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	include fastxxxx.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	include sf.inc
	include dpb.inc
	include mult.inc
	include filemode.inc
	.cref
	.list

Installed = TRUE

	I_need	DirStart,WORD
	I_Need	CONSft,DWORD		; SFT for swapped console In/Out
	i_need	CONSWAP,BYTE
	i_need	IDLEINT,BYTE
	i_need	THISSFT,DWORD
	i_need	DMAADD,DWORD
	i_need	DEVCALL,BYTE
	i_need	CALLSCNT,WORD
	i_need	CALLXAD,DWORD
	i_need	CONTPOS,WORD
	i_need	NEXTADD,WORD
	i_need	CONBUF,BYTE
	i_need	ClusFac,BYTE
	i_need	SecClusPos,BYTE
	i_need	DirSec,DWORD
	i_need	ClusNum,WORD
	i_need	NxtClusNum,WORD
	i_need	ReadOp,BYTE
	i_need	CURBUF,DWORD
	i_need	ALLOWED,BYTE
	i_need	EXTERR_LOCUS,BYTE


	i_need	HIGH_SECTOR,WORD
	I_need	JShare,DWORD
	i_need	DOS34_FLAG,WORD
;
;Flag to indicate WIN386 presence
;
	I_need	IsWin386,BYTE


DOSCODE	Segment
	ASSUME	SS:DOSDATA,CS:DOSCODE

	EXTRN	CharHard:near
	EXTRN	DevIoCall:near
	EXTRN	DevIoCall2:near
	EXTRN	Outt:near
	EXTRN	Setup:near


Break	<SwapCon, Swap Back - Old-style I/O to files>
; * * * * Drivers for file input from devices * * * *
;----------------------------------------------------------------------------
;   Indicate that ther is no more I/O occurring through another SFT outside of
;   handles 0 and 1
;
;   Inputs:	DS is DOSDATA
;   Outputs:	CONSWAP is set to false.
;   Registers modified: none
;----------------------------------------------------------------------------

procedure   SWAPBACK,NEAR

	DOSAssume   <DS>,"SwapBack"
	MOV	BYTE PTR [CONSWAP],0	; signal no conswaps
	return

EndProc SWAPBACK

;----------------------------------------------------------------------------
;
; Procedure Name : SWAPCON
;
;   Copy ThisSFT to CONSFT for use by the 1-12 primitives.
;
;   Inputs:	ThisSFT as the sft of the desired file
;		DS is DOSDATA
;   Outputs:	CONSWAP is set.  CONSFT = ThisSFT.
;   Registers modified: none
;--------------------------------------------------------------------------

procedure   SWAPCON,NEAR
	DOSAssume   <DS>,"SwapCon"

	mov	byte ptr [ConSwap], 1		; ConSwap = TRUE

	push	ax
	mov	ax, word ptr ThisSFT
	mov	word ptr ConSFT, ax
	mov	ax, word ptr ThisSFT+2
	mov	word ptr ConSFT+2, ax
	pop	ax

	return

EndProc SWAPCON

Break	<DOS_READ -- MAIN READ ROUTINE AND DEVICE IN ROUTINES>
;-----------------------------------------------------------------------------
;
; Inputs:
;	ThisSFT set to the SFT for the file being used
;	[DMAADD] contains transfer address
;	CX = No. of bytes to read
;	DS = DOSDATA
; Function:
;	Perform read operation
; Outputs:
;    Carry clear
;	SFT Position and cluster pointers updated
;	CX = No. of bytes read
;	ES:DI point to SFT
;    Carry set
;	AX is error code
;	CX = 0
;	ES:DI point to SFT
; DS preserved, all other registers destroyed
;
;-----------------------------------------------------------------------------

;hkn; called from fcbio.asm, handle.asm and dev.asm. DS is be set up.

procedure   DOS_READ,NEAR
	DOSAssume   <DS>,"DOS_Read"

	LES	DI,ThisSFT
	Assert	ISSFT,<ES,DI>,"DOS_Read"

; Verify that the sft has been opened in a mode that allows reading.

	MOV	AL,BYTE PTR ES:[DI.sf_mode]
	AND	AL,access_mask
	CMP	AL,open_for_write
	JNE	READ_NO_MODE		;Is read or both
	transfer   SET_ACC_ERR

READ_NO_MODE:
	call	SETUP
	JCXZ	NoIORet 		; no bytes to read - fast return
	invoke	IsSFTNet
	JZ	LOCAL_READ

IF NOT Installed
	transfer NET_READ
ELSE
	MOV	AX,(multNET SHL 8) OR 8
	INT	2FH
	return
ENDIF


; The user ended up requesting 0 bytes of input.  We do nothing for this case
; except return immediately.

NoIORet:
	CLC
	return

LOCAL_READ:
	TESTB	ES:[DI].SF_FLAGS,devid_device  ; Check for named device I/O
	JNZ	READDEV
	MOV	ExtErr_Locus,errLOC_Disk
	EnterCrit   critDisk

	invoke	DISKREAD

critexit:
	LeaveCrit   critDisk
	return


; We are reading from a device.  Examine the status of the device to see if we
; can short-circuit the I/O.  If the device in the EOF state or if it is the
; null device, we can safely indicate no transfer.

READDEV:
	DOSAssume   <DS>,"ReadDev"
	ASSUME	ES:NOTHING
	MOV	ExtErr_Locus,errLOC_SerDev
	MOV	BL,BYTE PTR ES:[DI].SF_FLAGS
	LES	DI,[DMAADD]
	test	BL,devid_device_EOF	; End of file?
	JZ	ENDRDDEVJ3
	test	BL,devid_device_null	; NUL device?
	JZ	TESTRAW 		; NO
	XOR	AL,AL			; Indicate EOF by setting zero
ENDRDDEVJ3:
	JMP	ENDRDDEVJ2

;
; We need to hit the device.  Figure out if we do a raw read or we do the
; bizarre std_con_string_input.
;
TESTRAW:
	test	BL,devid_device_raw	; Raw mode?
	JNZ	DVRDRAW 		; Yes, let the device do all local editing
	test	BL,devid_device_con_in	; Is it console device?
	JZ	NOTRDCON
	JMP	READCON

DVRDRAW:
	DOSAssume   <DS>,"DvRdRaw"	; BUGBUG - wasted DOSASSUME?
	PUSH	ES
	POP	DS			; Xaddr to DS:DI
    ASSUME DS:NOTHING

;SR;
;Check for win386 presence -- if present, do polled read of characters
;
	test	[IsWIN386],1
	jz	ReadRawRetry		;not present
	test	bl,devid_device_con_in	;is it console device
	jz	ReadRawRetry		;no, do normal read
	jmp	do_polling		;yes, do win386 polling loop

ReadRawRetry:
	MOV	BX,DI			; DS:BX transfer addr
	XOR	AX,AX			; Media Byte, unit = 0
	MOV	DX,AX			; Start at 0
	invoke	SETREAD
	PUSH	DS			; Save Seg part of Xaddr

;hkn; SS override
	LDS	SI,ThisSFT
	Assert	ISSFT,<DS,SI>,"DvRdRawR"
	call	DEVIOCALL
	MOV	DX,DI			; DS:DX is preserved by INT 24
	MOV	AH,86H			; Read error

;hkn; SS override
	MOV	DI,[DEVCALL.REQSTAT]
	.errnz	STERR-8000h
	or	di,di
	jns	crdrok			; no errors
	call	CHARHARD
	MOV	DI,DX			; DS:DI is Xaddr

	add	di, callscnt		; update ptr and count to reflect the	M065
	sub	cx, callscnt		; number of chars xferred		M065

	OR	AL,AL
	JZ	CRDROK			; Ignore
	CMP	AL,3
	JZ	CRDFERR 		; fail.
	POP	DS			; Recover saved seg part of Xaddr
	JMP	ReadRawRetry		; Retry

;
; We have encountered a device-driver error.  We have informed the user of it
; and he has said for us to fail the system call.
;
CRDFERR:
	POP	DI			; Clean stack
DEVIOFERR:

;hkn; SS override
	LES	DI,ThisSFT
	Assert	ISSFT,<ES,DI>,"DEVIOFERR"
	transfer    SET_ACC_ERR_DS

CRDROK:
	POP	DI			; Chuck saved seg of Xaddr
	MOV	DI,DX

;hkn; SS override
	ADD	DI,[CALLSCNT]		; Amount transferred
IF	DEBUG
	JMP	ENDRDDEVJ3
ELSE
	JMP	SHORT ENDRDDEVJ3
ENDIF


; We are going to do a cooked read on some character device.  There is a
; problem here, what does the data look like?  Is it a terminal device, line
; CR line CR line CR, or is it file data, line CR LF line CR LF?  Does it have
; a ^Z at the end which is data, or is the ^Z not data?  In any event we're
; going to do this:  Read in pieces up to CR (CRs included in data) or ^z (^z
; included in data).  this "simulates" the way con works in cooked mode
; reading one line at a time.  With file data, however, the lines will look
; like, LF line CR.  This is a little weird.

NOTRDCON:
	MOV	AX,ES
	MOV	DS,AX
ASSUME	DS:NOTHING
	MOV	BX,DI
	XOR	DX,DX
	MOV	AX,DX
	PUSH	CX
	MOV	CX,1
	invoke	SETREAD
	POP	CX

;hkn; SS override
	LDS	SI,ThisSFT
	Assert	ISSFT,<DS,SI>,"/NotRdCon"
	LDS	SI,[SI.sf_devptr]
DVRDLP:
	invoke	DSKSTATCHK
	call	DEVIOCALL2
	PUSH	DI		; Save "count" done
	MOV	AH,86H

;hkn; SS override
	MOV	DI,[DEVCALL.REQSTAT]
	.errnz	STERR-8000h
	or	di,di
	jns	CRDOK
	call	CHARHARD
	POP	DI

;hkn; SS override
	MOV	[CALLSCNT],1
	CMP	AL,1
	JZ	DVRDLP			;Retry
	CMP	AL,3
	JZ	DEVIOFERR		; FAIL
	XOR	AL,AL			; Ignore, Pick some random character
	JMP	SHORT DVRDIGN

CRDOK:
	POP	DI

;hkn; SS override
	CMP	[CALLSCNT],1
	JNZ	ENDRDDEVJ2
	PUSH	DS

;hkn; SS override
	MOV	DS,WORD PTR [CALLXAD+2]
	MOV	AL,BYTE PTR [DI]	; Get the character we just read
	POP	DS
DVRDIGN:

;hkn; SS override
	INC	WORD PTR [CALLXAD]	; Next character
	MOV	[DEVCALL.REQSTAT],0
	INC	DI			; Next character
	CMP	AL,1AH			; ^Z?
	JZ	ENDRDDEVJ2		; Yes, done zero set (EOF)
	CMP	AL,c_CR 		; CR?
	LOOPNZ	DVRDLP			; Loop if no, else done
	INC	AX			; Resets zero flag so NOT EOF, unless
					;  AX=FFFF which is not likely
ENDRDDEVJ2:
	JMP	ENDRDDEV		;changed short to long for win386

;SR;
;Polling code for raw read on CON when WIN386 is present
;
;At this point -- ds:di is transfer address
;		  cx is count
;

do_polling:
	mov	bx,di			;ds:bx is Xfer address
	xor	ax,ax
	mov	dx,ax
	call	setread			;prepare device packet

do_io:
;
;Change read to a NON-DESTRUCTIVE READ, NO WAIT
;
	mov	byte ptr es:[bx+2],DEVRDND	;Change command code
	push	ds
	lds	si,[THISSFT]		;get device header
	call	deviocall		;call device driver
	pop	ds
	
	test	es:[bx.REQSTAT],STERR	;check if error
	jz	check_busy		;no

	push	ds
	mov	dx,di
	invoke 	charhard		;invoke int 24h handler
	mov	di,dx
	or	al,al
	jz	pop_done_read		;ignore by user, assume read done
	cmp	al,3
	jz	devrderr		;user asked to fail
	pop	ds
	jmp	do_io			;user asked to retry

check_busy:
	test	es:[bx.REQSTAT],0200h	;see if busy bit set
	jnz	no_char			;yes, no character available
;
;Character is available. Read in 1 character at a time until all characters
;are read in or no character is available
;
	mov	byte ptr es:[bx+2],DEVRD	;command code is READ now
	mov	word ptr es:[bx+18],1		;change count to 1 character
	push	ds
	lds	si,[THISSFT]
	call	deviocall

	mov	dx,di
	mov	ah,86h
	mov	di,es:[bx.REQSTAT]	;get returned status
	test	di,STERR		;was there an error during read?
	jz	next_char		;no,read next character

	invoke	charhard		;invoke int 24h handler
	mov	di,dx			;restore di
	or	al,al			;
	jz	pop_done_read		;ignore by user,assume read is done
	cmp	al,3
	jz	devrderr		;user issued a 'fail',indicate error
	pop	ds
	jmp	do_io			;user issued a retry

next_char:
	pop	ds
	mov	di,dx
	dec	cx			;decrement count
	jcxz	done_read		;all characters read in
	inc	word ptr es:[bx+14]	;update transfer address
	jmp	do_io			;read next character in

devrderr:
	pop	di			;discard segment address
	les	di,[THISSFT]
	transfer SET_ACC_ERR_DS		;indicate error

no_char:
;
;Since no character is available, we let win386 switch the VM out
;
	push	ax
	mov	ah,84h
	int	2ah			;indicate idle to WIN386
;
;When control returns  from WIN386, we continue the raw read
;
	pop	ax
	jmp	do_io

pop_done_read:
	pop	ds
done_read:
	add	di,[CALLSCNT]
	jmp	ENDRDDEVJ3	;jump back to normal DOS raw read exit



ASSUME	DS:NOTHING,ES:NOTHING

TRANBUF:
	LODSB
	STOSB
	CMP	AL,c_CR 	; Check for carriage return
	JNZ	NORMCH
	MOV	BYTE PTR [SI],c_LF
NORMCH:
	CMP	AL,c_LF
	LOOPNZ	TRANBUF
	JNZ	ENDRDCON
	XOR	SI,SI		; Cause a new buffer to be read
	call	OUTT		; Transmit linefeed
	OR	AL,1		; Clear zero flag--not end of file
ENDRDCON:

;hkn; SS is DOSDATA
	Context DS
	CALL	SWAPBACK
	MOV	[CONTPOS],SI
ENDRDDEV:

;hkn; SS is DOSDATA
	Context DS

	MOV	[NEXTADD],DI
	JNZ	SETSFTC 	; Zero set if Ctrl-Z found in input
	LES	DI,ThisSFT
	Assert	ISSFT,<ES,DI>,"EndRdDev"
	AND	BYTE PTR ES:[DI].SF_FLAGS,NOT devid_device_EOF ; Mark as no more data available
SETSFTC:
	invoke	SETSFT
	return

ASSUME	DS:NOTHING,ES:NOTHING

READCON:
	DOSAssume   <DS>,"ReadCon"
	CALL	SWAPCON
	MOV	SI,[CONTPOS]
	OR	SI,SI
	JNZ	TRANBUF
	CMP	BYTE PTR [CONBUF],128
	JZ	GETBUF
	MOV	WORD PTR [CONBUF],0FF80H	; Set up 128-byte buffer with no template
GETBUF:
	PUSH	CX
	PUSH	ES
	PUSH	DI

;hkn; CONBUF is in DOSDATA
	MOV	DX,OFFSET DOSDATA:CONBUF

	invoke	$STD_CON_STRING_INPUT		; Get input buffer
	POP	DI
	POP	ES
	POP	CX

;hkn; CONBUF is in DOSDATA
	MOV	SI,2 + OFFSET DOSDATA:CONBUF

	CMP	BYTE PTR [SI],1AH	; Check for Ctrl-Z in first character
	JNZ	TRANBUF
	MOV	AL,1AH
	STOSB
	DEC	DI
	MOV	AL,c_LF
	call	OUTT		; Send linefeed
	XOR	SI,SI
	JMP	ENDRDCON

EndProc DOS_READ

Break	<DOS_WRITE -- MAIN WRITE ROUTINE AND DEVICE OUT ROUTINES>
;---------------------------------------------------------------------------
;
; Procedure Name : DOS_WRITE
;
; Inputs:
;	ThisSFT set to the SFT for the file being used
;	[DMAADD] contains transfer address
;	CX = No. of bytes to write
; Function:
;	Perform write operation
;	NOTE: If CX = 0 on input, file is truncated or grown
;		to current sf_position
; Outputs:
;    Carry clear
;	SFT Position and cluster pointers updated
;	CX = No. of bytes written
;	ES:DI point to SFT
;    Carry set
;	AX is error code
;	CX = 0
;	ES:DI point to SFT
; DS preserved, all other registers destroyed
;---------------------------------------------------------------------------

;hkn; called from fcbio2.asm, handle.asm and dev.asm. DS is set up at this 
;hkn; point to DOSDATA.

procedure   DOS_WRITE,NEAR
	DOSAssume   <DS>,"DOS_Write"

	LES	DI,ThisSFT
	Assert	ISSFT,<ES,DI>,"DosWrite"
	MOV	AL,BYTE PTR ES:[DI.sf_mode]
	AND	AL,access_mask
	CMP	AL,open_for_read
	JNE	Check_FCB_RO		 ;Is write or both
BadMode:
	transfer    SET_ACC_ERR

;
; NOTE: The following check for writting to a Read Only File is performed
;	    ONLY on FCBs!!!!
;	We ALLOW writes to Read Only files via handles to allow a CREATE
;	    of a read only file which can then be written to.
;	This is OK because we are NOT ALLOWED to OPEN a RO file via handles
;	    for writting, or RE-CREATE an EXISTING RO file via handles. Thus,
;	    CREATing a NEW RO file, or RE-CREATing an existing file which
;	    is NOT RO to be RO, via handles are the only times we can write
;	    to a read-only file.
;
Check_FCB_RO:
	TESTB	ES:[DI.sf_mode],sf_isfcb
	JZ	WRITE_NO_MODE		; Not an FCB
	TESTB	ES:[DI].sf_attr,attr_read_only
	JNZ	BadMode 		; Can't write to Read_Only files via FCB
WRITE_NO_MODE:
	call	SETUP
	invoke	IsSFTNet
	JZ	LOCAL_WRITE

IF NOT Installed
	transfer NET_WRITE
ELSE
	MOV	AX,(multNET SHL 8) OR 9
	INT	2FH
	return
ENDIF


LOCAL_WRITE:
	TESTB	ES:[DI].SF_FLAGS,devid_device  ; Check for named device I/O
	DLJNZ	WRTDEV
	MOV	ExtErr_Locus,errLOC_Disk
	EnterCrit   critDisk

	invoke	DISKWRITE

;; Extended Open
	JC	nocommit
	LES	DI,ThisSFT
	TESTB	ES:[DI.sf_mode],auto_commit_write
	JZ	nocommit
	PUSH	CX
	invoke	DOS_COMMIT
	POP	CX
nocommit:
;; Extended Open
	LeaveCrit   critDisk
	return

DVWRTRAW:
ASSUME	DS:NOTHING
	XOR	AX,AX			; Media Byte, unit = 0
	invoke	SETWRITE
	PUSH	DS			; Save seg of transfer

;hkn; SS override
	LDS	SI,ThisSFT
	Assert	ISSFT,<DS,SI>,"DosWrite/DvWrtRaw"
	call	DEVIOCALL		; DS:SI -> DEVICE


	MOV	DX,DI			; Offset part of Xaddr saved in DX
	MOV	AH,87H

;hkn; SS override
	MOV	DI,[DEVCALL.REQSTAT]
	.errnz	STERR-8000h
	or	di,di
	jns	CWRTROK
	call	CHARHARD

	sub	cx, callscnt		; update ptr & count to reflect	M065
	mov	bx, dx			; number of chars xferred	M065
	add	bx, callscnt		;				M065
	mov	di, bx			;				M065

;	MOV	BX,DX			; Recall transfer addr		M065

	OR	AL,AL
	JZ	CWRTROK 		; Ignore
	CMP	AL,3
	JZ	CWRFERR
	POP	DS			; Recover saved seg of transfer
	JMP	DVWRTRAW		; Try again

CWRFERR:
	POP	AX			; Chuck saved seg of transfer
	JMP	CRDFERR 		; Will pop one more stack element

CWRTROK:
	POP	AX			; Chuck saved seg of transfer
	POP	DS
	DOSAssume   <DS>,"DISK/CWrtOK"
	MOV	AX,[CALLSCNT]		; Get actual number of bytes transferred
ENDWRDEV:
	LES	DI,ThisSFT
	Assert	ISSFT,<ES,DI>,"DosWrite/EndWrDev"
	MOV	CX,AX
	invoke	ADDREC
	return

WRTNUL:
	MOV	DX,CX			;Entire transfer done
WrtCookJ:
	JMP	WRTCOOKDONE

WRTDEV:
	DOSAssume   <DS>,"DISK/WrtDev"
	MOV	ExtErr_Locus,errLOC_SerDev
	OR	BYTE PTR ES:[DI].SF_FLAGS,devid_device_EOF  ; Reset EOF for input
	MOV	BL,BYTE PTR ES:[DI].SF_FLAGS
	XOR	AX,AX
	JCXZ	ENDWRDEV		; problem of creating on a device.
	PUSH	DS
	MOV	AL,BL
	LDS	BX,[DMAADD]		; Xaddr to DS:BX
ASSUME	DS:NOTHING
	MOV	DI,BX			; Xaddr to DS:DI
	XOR	DX,DX			; Set starting point
	test	AL,devid_device_raw	; Raw?
	JZ	TEST_DEV_CON
	JMP	DVWRTRAW

TEST_DEV_CON:
	test	AL,devid_device_con_out ; Console output device?
	DLJNZ	WRITECON
	test	AL,devid_device_null
	JNZ	WRTNUL
	MOV	AX,DX
	CMP	BYTE PTR [BX],1AH	; ^Z?
	JZ	WRTCOOKJ		; Yes, transfer nothing
	PUSH	CX
	MOV	CX,1
	invoke	SETWRITE
	POP	CX

;hkn; SS override
	LDS	SI,ThisSFT
;
;SR; Removed X25 support from here
;
	LDS	SI,[SI.sf_devptr]
DVWRTLP:
	invoke	DSKSTATCHK
	call	DEVIOCALL2
	PUSH	DI
	MOV	AH,87H

;hkn; SS override
	MOV	DI,[DEVCALL.REQSTAT]
	.errnz	STERR-8000h
	or	di,di
	jns	CWROK
	call	CHARHARD
	POP	DI

;hkn; SS override
	MOV	[CALLSCNT],1
	CMP	AL,1
	JZ	DVWRTLP 	; Retry
	OR	AL,AL
	JZ	DVWRTIGN	; Ignore
	JMP	CRDFERR 	; Fail, pops one stack element

CWROK:
	POP	DI

;hkn; SS override
	CMP	[CALLSCNT],0
	JZ	WRTCOOKDONE
DVWRTIGN:
	INC	DX

;hkn; SS override for CALLXAD
	INC	WORD PTR [CALLXAD]
	INC	DI
	PUSH	DS
	MOV	DS,WORD PTR [CALLXAD+2]
	CMP	BYTE PTR [DI],1AH	; ^Z?
	POP	DS
	JZ	WRTCOOKDONE

;hkn; SS override
	MOV	[DEVCALL.REQSTAT],0
	LOOP	DVWRTLP
WRTCOOKDONE:
	MOV	AX,DX
	POP	DS
	JMP	ENDWRDEV

WRITECON:
	PUSH	DS

;hkn; SS is DOSDATA
	Context DS
	CALL	SWAPCON
	POP	DS
ASSUME	DS:NOTHING
	MOV	SI,BX
	PUSH	CX
WRCONLP:
	LODSB
	CMP	AL,1AH		; ^Z?
	JZ	CONEOF
	call	OUTT
	LOOP	WRCONLP
CONEOF:
	POP	AX			; Count
	SUB	AX,CX			; Amount actually written
	POP	DS
	DOSAssume   <DS>,"DISK/ConEOF"
	CALL	SWAPBACK
	JMP	ENDWRDEV

EndProc DOS_WRITE

;---------------------------------------------------------------------------
;
; Procedure Name : get_io_sft
;
;   Convert JFN number in BX to sf_entry in DS:SI We get the normal SFT if
;   CONSWAP is FALSE or if the handle desired is 2 or more.  Otherwise, we
;   retrieve the sft from ConSFT which is set by SwapCon.
;
;---------------------------------------------------------------------------

procedure   get_io_sft,near
	cmp	ConSwap,0					;smr;SS Override
	JNZ	GetRedir
GetNormal:

;hkn; SS is DOSDATA
	Context DS

	PUSH	ES
	PUSH	DI
	invoke	SFFromHandle
	JC	RET44P
	MOV	SI,ES
	MOV	DS,SI
ASSUME	DS:NOTHING
	MOV	SI,DI
RET44P:
	POP	DI
	POP	ES
	return
GetRedir:
	CMP	BX,1
	JA	GetNormal

;hkn; SS override
	LDS	SI,ConSFT
	Assert	ISSFT,<DS,SI>,"GetIOSft"
	CLC
	return
EndProc get_io_sft

Break	<DIRREAD -- READ A DIRECTORY SECTOR>
;---------------------------------------------------------------------------
;
; Procedure Name : DIRREAD
;
; Inputs:
;	AX = Directory block number (relative to first block of directory)
;	ES:BP = Base of drive parameters
;	[DIRSEC] = First sector of first cluster of directory
;	[CLUSNUM] = Next cluster
;	[CLUSFAC] = Sectors/Cluster
; Function:
;	Read the directory block into [CURBUF].
; Outputs:
;	[NXTCLUSNUM] = Next cluster (after the one skipped to)
;	[SECCLUSPOS] Set
;	ES:BP unchanged
;	[CURBUF] Points to Buffer with dir sector
;	Carry set if error (user said FAIL to I 24)
; DS preserved, all other registers destroyed.
;---------------------------------------------------------------------------

;hkn; called from dir.asm. DS already set up to DOSDATA.

procedure   DirRead,NEAR

	DOSAssume   <DS>,"DirRead"
	Assert	    ISDPB,<ES,BP>,"DirRead"

; Note that ClusFac is is the sectors per cluster.  This is NOT necessarily
; the same as what is in the DPB!  In the case of the root directory, we have
; ClusFac = # sectors in the root directory.  The root directory is detected
; by DIRStart = 0.

	XOR	DX,DX
	CMP	DirStart,0
	jnz	SubDir
	XCHG	AX,DX
	JMP	short DoRead

; Convert the sector number in AX into cluster and sector-within-cluster pair

SubDir:
	MOV	DL,AL
	AND	DL,ES:[BP.dpb_cluster_mask]

;	(DX) = sector-in-cluster

	MOV	CL,ES:[BP.dpb_cluster_shift]
	SHR	AX,CL

;	(DX) = position in cluster
;	(AX) = number of clusters to skip

DoRead:
	MOV	[SECCLUSPOS],DL
	MOV	CX,AX
	MOV	AH,DL

;	(CX) = number of clusters to skip.
;	(AH) = remainder

	MOV	DX,WORD PTR [DIRSEC+2]	     ;>32mb
	MOV	[HIGH_SECTOR],DX	     ;>32mb
	MOV	DX,WORD PTR [DIRSEC]
	ADD	DL,AH
	ADC	DH,0
	ADC	[HIGH_SECTOR],0 	     ;>32mb

	MOV	BX,[CLUSNUM]
	MOV	[NXTCLUSNUM],BX
	JCXZ	FIRSTCLUSTER
SKPCLLP:
	invoke	UNPACK
	retc
	XCHG	BX,DI
	invoke	IsEOF			; test for eof based on fat size
	JAE	HAVESKIPPED
	LOOP	SKPCLLP
HAVESKIPPED:
	MOV	[NXTCLUSNUM],BX
	MOV	DX,DI
	MOV	BL,AH
	invoke	FIGREC

	entry	FIRSTCLUSTER

	MOV	[ALLOWED],allowed_RETRY + allowed_FAIL
	XOR	AL,AL		; Indicate pre-read
	invoke	GETBUFFR
	retc

entry	SET_BUF_AS_DIR
	DOSAssume   <DS>,"SET_BUF_AS_DIR"
	ASSUME	ES:NOTHING

;	Set the type of CURBUF to be a directory sector.
;	Only flags are modified.

	PUSH	DS
	PUSH	SI
	LDS	SI,[CURBUF]
	Assert	ISBUF,<DS,SI>,"SetBufAsDir"
	OR	[SI.buf_flags],buf_isDIR	; Clears carry
	POP	SI
	POP	DS
	return
EndProc DirRead

Break	<FATSECRD -- READ A FAT SECTOR>
;----------------------------------------------------------------------------
;
; Procedure Name : FATSECRD
; Inputs:
;	Same as DREAD
;	DS:BX = Transfer address
;	CX = Number of sectors
;	DX = Absolute record number
;	ES:BP = Base of drive parameters
; Function:
;	Calls BIOS to perform FAT read.
; Outputs:
;	Same as DREAD
;---------------------------------------------------------------------------

procedure   FATSecRd,NEAR

	Assert	    ISDPB,<ES,BP>,"FATSecRd"

;hkn; SS override
	MOV	[ALLOWED],allowed_RETRY + allowed_FAIL
	MOV	DI,CX
	MOV	CL,ES:[BP.dpb_FAT_count]
	MOV	AX,ES:[BP.dpb_FAT_size] 	;>32mb
;	XOR	AH,AH
	XOR	CH,CH				;>32mb
	PUSH	DX
NXTFAT:

;hkn; SS override
	MOV	[HIGH_SECTOR],0       ;>32mb FAT sectors cannot exceed
	PUSH	CX		      ;>32mb
	PUSH	AX
	MOV	CX,DI
	invoke	DSKREAD
	POP	AX
	POP	CX
	JZ	RET41P		; Carry clear
	ADD	DX,AX
	LOOP	NXTFAT
	POP	DX
	MOV	CX,DI

; NOTE FALL THROUGH

Break	<DREAD -- DO A DISK READ>
;---------------------------------------------------------------------------
;
; Procedure Name : DREAD
;
; Inputs:
;	DS:BX = Transfer address
;	CX = Number of sectors
;	DX = Absolute record number	      (LOW)
;	[HIGH_SECTOR]= Absolute record number (HIGH)
;	ES:BP = Base of drive parameters
;	[ALLOWED] must be set in case call to HARDERR needed
; Function:
;	Calls BIOS to perform disk read. If BIOS reports
;	errors, will call HARDERRRW for further action.
; Outputs:
;	Carry set if error (currently user FAILED to INT 24)
; DS,ES:BP preserved. All other registers destroyed.
;---------------------------------------------------------------------------

	entry	DREAD
ASSUME	DS:NOTHING,ES:NOTHING

	Assert	ISDPB,<ES,BP>,"DREAD"
	invoke	DSKREAD
	retz			; Carry clear

;hkn; SS override
	MOV	BYTE PTR [READOP],0
	invoke	HARDERRRW
	CMP	AL,1		; Check for retry
	JZ	DREAD
	CMP	AL,3		; Check for FAIL
	CLC
	JNZ	NO_CAR		; Ignore
	STC
NO_CAR:
	return

RET41P: POP	DX
	return
EndProc FATSecRd


Break	<CHECK_WRITE_LOCK>
;---------------------------------------------------------------------------
;
; Procedure Name : CHECK_WRITE_LOCK
;
; Inputs:
;	output of SETUP
;	ES:DI -> SFT
; Function:
;	check write lock
; Outputs:
;	Carry set if error
;	Carry clear if ok
;
;----------------------------------------------------------------------------

procedure   CHECK_WRITE_LOCK,NEAR

	TESTB	ES:[DI].sf_attr,attr_volume_id ;volume id
	JZ	write_cont		       ;no
	invoke	SET_ACC_ERR_DS		       ;
	return

write_cont:				;
	PUSH	CX			;save reg
	OR	CX,CX			;
	JNZ	Not_Truncate		;
	dec	cx			;(cx) = -1; check for lock on whole file
Not_Truncate:				;
	MOV	AL,80H			;check write access
	invoke	LOCK_CHECK		;check lock
	POP	CX			;restore reg
	JNC	WRITE_OK		;lock ok
	invoke	WRITE_LOCK_VIOLATION	;issue I24
	JNC	write_cont		;retry
WRITE_OK:				;
	return				;

EndProc CHECK_WRITE_LOCK		;


Break	<CHECK_READ_LOCK>
;---------------------------------------------------------------------------
;
; Procedure Name : CHECK_READ_LOC
;
; Inputs:
;	ES:DI -> SFT
;	output of SETUP
; Function:
;	check read lock
; Outputs:
;	Carry set if error
;	Carry clear if ok
;----------------------------------------------------------------------------

procedure   CHECK_READ_LOCK,NEAR

	TESTB	ES:[DI].sf_attr,attr_volume_id ; volume id
	JZ	do_retry		       ; no
	invoke	SET_ACC_ERR		       ;
	return				       ;
do_retry:				;
	xor	al,al			;check read access
	invoke	LOCK_CHECK		;check lock
	JNC	READ_OK 		;lock ok
	invoke	READ_LOCK_VIOLATION	;issue I24
	JNC	do_retry		;retry
READ_OK:				;
	return				;
EndProc CHECK_READ_LOCK 		;

DOSCODE	ENDS
    END


