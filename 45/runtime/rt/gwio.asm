	TITLE	GWIO - Basic I/O and initialization
	PAGE	56,132
;***
; GWIO - Basic I/O and initialization
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; This module contains code for doing initialization and termination
; and code for calling the lowest level OEM dependent screen I/O routines.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	_DATA		
	USESEG	_BSS		

	USESEG	DV_TEXT 	
	useSeg	RT_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE baslibma.inc
	INCLUDE files.inc
	INCLUDE intmac.inc
	INCLUDE devdef.inc
	INCLUDE addr.inc
	INCLUDE event.inc	; functions for $RDevt routines
	INCLUDE dc.inc
	INCLUDE messages.inc	
	INCLUDE stack.inc	
	INCLUDE idmac.inc	

	.LIST

sBegin	_DATA			

globalW  b$pTEST_CLOSE,B$NearRet 

	externW	b$ERDEVP
	externB	__osmajor	

; initial indirect jumps for no /v, no /d

globalW b$IPOLKEY,B$NearRet

globalW b$fInt24Err,-1		;On entry, non-zero means IGNORE int24
				;on exit from int24, low 15 bits contain
				;INT 24 error code.
externB	fInt24Error		; flag to COW than an INT 24 occurred.

	globalB b$EventFlags,,1	


sEnd	_DATA			

sBegin	_BSS			

	externW	b$SOFT_KEYS	;defined in GWDATA.ASM
	externW	B$SOFT_KEY_LEN	;defined in GWDATA.ASM
	externW	B$SOFT_KEY_INDEX	;defined in GWDATA.ASM

	externW	b$ERDEV	;defined in GWDATA.ASM

	staticW	DIV0_SAVE,,2
	staticW	OVRF_SAVE,,2
	staticW	DEVR_SAVE,,2	;Disk error interrupt save

	staticW	DERRCD,,1	; Disk error error code save area
	staticW	CHAR_SAVE,,1
	staticW	DCHAR_SAVE,,1

	staticB	DEVICE,,1
	staticB	WPRFLG,,1	;Write Protect Flag for INT 24 hndlr
	staticB	EXTFLG,,1	;set if sharing violation

sEnd	_BSS			


sBegin	RT_TEXT 		
	externNP B$GETDS 	
sEnd	RT_TEXT 		

sBegin	DV_TEXT 		
	assumes CS,DV_TEXT	

	externNP B$BREAK	
	externNP B$DIV0		
	externNP B$OVFR		
	externNP B$ERR_DME	
	externNP B$ERR_DNR	
	externNP B$ERR_FWP	
	externNP B$ERR_IOE	
	externNP B$ERR_DVF	
	externNP B$ERR_OTP	
	externNP B$ERR_DNA	
	externNP B$ERR_DTO	

	externNP B$KEYINP	; oem routine


;B$NearRet -- near return
;Called by something that thinks there is event trapping when there isn't any

labelNP	<PUBLIC,B$NearRet>	
	RET			;near RETURN

	SUBTTL	B$IOINI - MS-DOS special I/O initialization

;***
; B$IOINI - Run-time initialization
;
;Purpose: To install handlers for Divide by zero, Oveflow and I/O interrupts.
;
;Entry:
;	None
;Exit:
;	None
;Modifies:
;	Per Convention
;Exceptions:
;	None
;****

cProc	B$IOINI,<PUBLIC,NEAR>	
cBegin				


	PUSHF			;save interrupt status
	XOR	AX,AX		;Clear segment
	PUSH	DS		;Save DS
	PUSH	ES		; new savint trashes [es]
	CLI			; Turn off interrupts
	SAVINT	DGROUP:DIV0_SAVE,0
	SAVINT	DGROUP:OVRF_SAVE,4*4
	SAVINT	DGROUP:DEVR_SAVE,36D*4
	POP	ES
	MOV	AX,CS
	MOV	DS,AX		; All interrupt vectors point to code segment
ASSUME	DS:NOTHING
	SETVEC	0,B$DIV0	; Divide by zero
	SETVEC	4,B$OVFR	; Overflow
	SETVEC	24h,DEVR_INT	; Fatal Error Abort Address
	POP	DS		;Restore DS
ASSUME	DS:DGROUP
	POPF			;restore interrupts

cEnd				; End of B$IOINI

	PAGE
	SUBTTL	DOSIO utility subroutines

;B$KYBINI puts the keyboard device server in an initial state.
; It is called at initialization time and after CTL-C.
; On exit, all registers are preserved.


	PUBLIC	B$KYBINI
B$KYBINI:
	push	ax
	xor	ax,ax
	mov	[B$SOFT_KEY_LEN],ax ;soft key expansion terminated.
	mov	[char_save],ax
	mov	[dchar_save],ax
	inc	ax		; psw.z reset so keyinp wont wait
kywait: cCall	B$KEYINP 	; any chars present?
	jnz	kywait		; read them until none left
	pop	ax
	ret

;***
;B$BREAK_CHK - check for keyboard interrupts (CNTL-BREAK)
;OEM-callback routine
;
;Purpose:
;	This routine checks if the user has tried to interrupt
;	the program with a CNTL-C.  If this has occured, then
;	branch to the break handler, else return.
;	NOTE: A /D switch to the compiler is required for this
;	routine to have any effect.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	Per convention.
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	May not return.
;
;****************************************************************************

labelNP <PUBLIC,B$CHKKYB>	

cProc	B$BREAK_CHK,<PUBLIC,NEAR> 
cBegin				
	CALL	[B$IPOLKEY]
	TEST	b$EventFlags,CNTLC ; bit CNTLC = 0 if no ^Break
	JNZ	BREAK		; brif ^Break
cEnd				

BREAK:
	AND	b$EventFlags,NOT CNTLC ; Clear it to avoid recursion
				; (for COMM close if output still busy).
	JMP	B$BREAK		; print Break message and die..



;***
; B$TTYST - TTY status
;
; Inputs:
;	None.
; Function:
;	Check console for pending character.
; Outputs:
;	PSW.Z set if if no char waiting.
;	AL =  character
;	AH =  0 if single byte, NZ if double byte
;	char_save and dchar_save have the byte(s) receive by keyinp
; Registers:
;	Only AX and F affected.
;****

cProc	B$TTYST,<NEAR,PUBLIC>,<DX>	
cBegin				

	cmp	[B$SOFT_KEY_LEN],0 ; Processing soft key?
	jnz	tty_ret 	; If so, then have char...
	mov	ax,[char_save]	
	or	al,al		; Was the pending char already input ?
	jnz	tty_ret 	; Yes, note presence
	or	sp,sp		; Psw.z reset so keyinp wont wait
	cCall	B$KEYINP 	
	jz	tty_ret 	; No char available
	mov	[char_save],ax	; Save high byte(s)
	mov	[dchar_save],dx ; Save low bytes (if present)

tty_ret:

cEnd				

;	Added with revision [47]
;	OS/2 support totally rewritten with [49]
;
;***
;B$TTYGetChar - Wait for character from keyboard, using polling
;
;Purpose:
;	This routine will Poll the keyboard until a character is
;	ready. The character is returned as in B$TTYIN.  B$BREAK_CHK
;	can be called between the polls by setting DX non-zero.
;
;	If OM_DOS5, we can't poll waiting for a keystroke as it
;	hogs the CPU.  Giving up our time slice between polls does
;	not work as the forground process of the current screen
;	group has extra priority, so we frequently get the time slice
;	back before anyone else can run.
;
;	If we do not have a keyboard monitor installed, or we are not
;	suspose to do polling, then we go directly to B$TTYIN and ask
;	it to wait for a charcter.  This is safe, as no keyboard events
;	could happen without the keyboard monitor.
;
;	If we do have a keyboard monitor, then it will clear the sleep
;	semaphore whenever a keystroke is recognized.  Thus, we block
;	on the sleep semaphore and only check for events/keys when it
;	is clear.
;
;Entry:
;	DL = 0	: don't call B$BREAK_CHK between checks for a character
;	DL != 0 : call B$BREAK_CHK
;
;Exit:
;	[AL]	Has character if not Scan Code. Zero Flag not set.
;	[AH]	Has 128D, [AL] has Scan Code; or AX has Kanji char
;	[DX]	May have OEM's special code, AL=254d if so, exit with NZ,NC
;	Z flag set if Scan Code, C flag set if two byte code
;
;Uses:
;	AX,DX,Flags
;
;Exceptions:
;	None.
;****

cProc	B$TTYGetChar,<PUBLIC,NEAR>
cBegin
TryAgain:
	CALL	B$TTYST 	;Is a character available?
	JNZ	CharReady	;yes, get char and return (Use code in B$TTYIN)
	OR	DL,DL		;Should we call B$BREAK_CHK
	JZ	TryAgain	;no, just loop
	CALL	B$BREAK_CHK	;Check if ^BREAK has been hit
	JMP	SHORT TryAgain	;try to get the character again
cEnd	<nogen>


;***
; B$TTYIN - TTY input
;
; Inputs:
;	None.
; Function:
;	Get character from console keyboard
; Outputs:
;	[AL]	Has character if not Scan Code. Zero Flag not set.
;	[AH]	Has 128D, [AL] has Scan Code; or AX has Kanji char
;	[DX]	May have OEM's special code, AL=254d if so, exit with NZ,NC
;	Z flag set if Scan Code, C flag set if two byte code
;
; Registers:
;	Only AX and F affected.
;	DX affected too.
;
;Note:	this code assumes that the first byte of a
;	two byte code will be in the range [80h..0FFh].
;****

cProc	B$TTYIN,<NEAR,PUBLIC>	
cBegin				
	CALL	B$TTYST	; check for presence of char
	jz	ttwait		; none present so wait for one
CharReady:			; Common entry point: B$TTYIN/B$TTYGetChar
	cmp	[B$SOFT_KEY_LEN],0 ; processing soft key?
	jnz	skey_rd 	; if so, return its next char
	mov	ax,[char_save]	; fetch char read by B$TTYST
	mov	dx,[dchar_save]
	jmp	short ttyin2
				;Second Entry point for B$TTYGetChar
ttwait: xor	ax,ax		; tell keyinp to wait for char
	cCall	B$KEYINP 	
	jnc	ttyx		; PSW.C set if exactly 2 bytes

ttyin2: CMP	AH,LOW 128D	;start checking for function key
	JNE	TTYINX0 	;branch if not function key
	CMP	AL,LOW 32D	;[AL]=' ' for 1st function key
	JB	TTYINX
	CMP	AL,NUM_FKEYS+32D ;see if its a	function key
	JB	SKEY_SET	; go setup and return 1st char
TTYINX:
	CMP	AH,128D 	;Zero flag set if returning Scan Code..
TTYINX0:
	CMC			;Carry set if 2 byte.
ttyx:
	mov	[char_save],0	; char no longer waiting
cEnd				

	PAGE

;SKEY_SET -	Soft Key detected.  Store index into B$SOFT_KEYS
;		and return 1st char of Soft Key.  SKEY_RD will
;		return the rest...
;Entry:
;	[AX]	Has Scan Code.
;Exit:
;	[AL]	Has Soft Key char if assigned, else...
;	[AH]	Has Scan Code if not.

SKEY_SET:
	PUSH	SI
	PUSH	AX
	SUB	AL,32D		;make into index into soft key table
	MOV	SI,OFFSET DGROUP:B$SOFT_KEYS
	XOR	AH,AH
	SHL	AL,1		;Soft Key Table entry size is 4
	SHL	AL,1		;[AX] = Key no. * 4
	ADD	SI,AX		;Index into Soft Key Descriptor Table
	MOV	AX,[SI] 	;[AX] = Soft Key string Length
	MOV	B$SOFT_KEY_LEN,AX ;Store Length of Soft Key
	OR	AX,AX		;Null?
	POP	AX		;clean up stack
	JZ	SKEY_RET	;If null then return Scan Code
	MOV	SI,[SI+2]	;[SI] = Addr of Soft Key String
	MOV	B$SOFT_KEY_INDEX,SI ;Store index for SKEY_RD
	JMP	SHORT SKEY_RD_2 ; and return 1st character...

;SKEY_RD -	If Soft Key in Progress, return next character
;		from Soft Key String.

SKEY_RD:
	PUSH	SI
SKEY_RD_2:
	MOV	SI,[B$SOFT_KEY_INDEX]
	MOV	AL,[SI] 	;Get char from Soft Key string
	xor	ah,ah
	cmp	al,0feh 	; soft key assigned to chr$(254) ?
	jne	not_254 	; brif not
	mov	dx,0feh 	; else set [dx] to 00feh bcos $inkmap
				; assumes a three byte code if [al]=254
	mov	[dchar_save],dx ; store the same in [dchar_save] also
not_254:
	INC	[B$SOFT_KEY_INDEX]
	DEC	[B$SOFT_KEY_LEN]
SKEY_RET:
	POP	SI
	JMP	TTYINX

;***
;B$CNTRL - check for control characters during printing
;
;****

cProc	B$CNTRL,<NEAR,PUBLIC>	
cBegin				
	JMP	[B$IPOLKEY]	; wait on pause_key
cEnd	<nogen>			

	PAGE
;***
;B$DOS3CHECK - Check DOS version, return carry set if < DOS 3.00
;OEM-callback routine
;
;Purpose:
;	Common routine to check for DOS 3.  This check is made
;	several places in the runtime.	A near call and a relative
;	jump is smaller than a cmp imm,mem and relative jump.
;
;Entry:
;	__osmajor - byte containing DOS major version number - must be set.
;		     This is done before any OEM routines can be called.
;
;Exit:
;	PSW.C - Set if __osmajor < DOS 3.00
;	PSW.Z - Set if = DOS 3.00
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	None.
;*****************************************************************************
cProc	B$DOS3CHECK,<PUBLIC,NEAR> 
cBegin				
	CMP	__osmajor,3	; check for dos 3
cEnd				

	PAGE
	SUBTTL	DEVR_INT - fatal device error interrupt handler
;***
;DEVR_INT - fatal device error interrupt handler
;
;Purpose:
;
;Entry:
; [BP:SI] = points to the device header
; [AL]	  = has drive number if block device (a=0,b=1,...)
; [DI]	  = Error code as follows:
;		00 - Write Protected.
;		01 - Unknown Unit
;		02 - Not Ready
;		03 - Unknown command
;		04 - Data Error
;		05 - Bad Drive structure length
;		06 - Seek Error
;		07 - Unknown Media
;		08 - Sector not found
;		09 - Printer out of paper
;		10 - Write Fault (hard disks only)
;		11 - Read Fault
;		12 - Other error.
;
; Special handling must be done for "Disk Write Protect"  error  due  to  some
; MS-DOS booboo.   Control  must pass to the standard INT 24h handler in order
; to clean up some (state?)  variables.  But then BASIC cannot trap the error.
; The solution	is  to	modify BASIC's parent pointer to point to itself, then
; if a write protect error occurs, DEVR_INT can  abort	to  MS-DOS  which  will
; return via the parent pointer to BASIC's error handling routine DSKERC.
;
; Note: On entry DS, ES are not pointing to BASIC.
;
;Exit:
;
;Uses:
;
;Preserves: (optional)
; SS, SP, DS, ES, BX, CX, and DX must be preserved if an IRET is executed
;
;Exceptions:
;
;******************************************************************************
DbPub	DEVR_INT		
cProc	DEVR_INT,FAR		
cBegin				
	ASSUME	DS:NOTHING, ES:NOTHING, SS:NOTHING
	STI
	PUSH	DS
	PUSH	BX
	CALL	B$GETDS
	MOV	DS,BX
	ASSUME	DS:DGROUP
	MOV	fInt24Error,-1	; set flag for QB5/COW

	CMP	b$fInt24Err,0	;Should we IGNORE this error?
	JZ	TestWriteProtect ;brif not, test for a write protect sequence

	MOV	b$fInt24Err,DI ; save error in b$fInt24Err
	OR	b$fInt24Err,0FF00h; insure b$fInt24Err does not go to 0.
	JMP	SHORT IgnoreInt24 ;ignore this error

TestWriteProtect:		

	CMP	BYTE PTR [WPRFLG],0 ;Are we in a write protect INT 24 Seqenc
	JZ	DSKER1		;Brif not

IgnoreInt24:			
	XOR	AX,AX		;Tell DOS to ignore the error
	pop	bx
	pop	ds
	IRET			;Back to DOS - ignore dirty buffers

DSKER1: 			;Here if Write protect flag not set
	CLI			; Critical section below.  A second
				; interrupt would blow away the first one
	PUSH	BX		; save basic DGROUP
	MOV	BX,SP		; get psuedo frame pointer
	MOV	CX,[BX+24]	; [CX] = [BP] at INT 21H time (documented)
	POP	BX		; restore basic DGROUP
	mov	es,bx		; ES = DGROUP
	mov	ss,bx		; restore BASIC's stack 
	mov	sp,cx		; new stack pointer = previous BP

	; Stack now back to where it was just after the:
	;	PUSH	BP
	;	MOV	BP,SP
	; of the runtime entry point that caused the error.

	assume	ds:dgroup, es:dgroup, ss:dgroup
	STI			;Turn interrupts back on
	.erre	ID_SSEQDS	; assumes ss equals ds.
	push	cx		; save old BP for later

	mov	cx,di		;get device error into cl
	mov	DEVICE,AL	; device
	AND	AH,80H		;test for char dev or bad FAT
	JZ	CHRDEV		;not either, so not char dev
	PUSH	DS		
	MOV	DS,BP		; get device header segment
	TEST	BYTE PTR DS:[SI+4],80H ; test for character device
	POP	DS		
	JZ	CHRDEV		;jump if char dev, not block dev

	XOR	AH,AH		;clear flag for no char device
CHRDEV:
	mov	al,cl		;error code into al
	mov	DERRCD,AX	; Save error code and device type bit
	assume	es:nothing
	test	al,255		; Is this a write protect error?
	jnz	noprtct 	; Yes: return to MS-DOS with abort request
;	---------------------------	;Mark begining of Wr-Pro Sequence
	mov	byte ptr [wprflg],1 ;"Zibo" method of handling INT 24
	callos	REST		;from write protects.  Forces DOS
	mov	byte ptr [wprflg],0 ;to REALLY ignore error and discard
;	---------------------------	;pending dirty buffers.
noprtct:
	MOV	EXTFLG,0	;no violation yet
	CALL	B$GETEXTERR	; psw.c set if <dos 3
;	JB	NO_VIOLATION	;if DOS2, then branch
	JNZ	NO_VIOLATION	; ZF=1 if violation
	INC	EXTFLG		;set violation flag
NO_VIOLATION:
	callos	VERSN		;Do this for good luck. clears unstable dos
				;state

;Control is passed here after a hard disk error (INT 24H).
;	  Now we decide what the error is and JMP to its error handler.
; Entry DS, ES undefined

	mov	ax,DERRCD	; Restore error code
	MOV	BX,OFFSET DGROUP:b$ERDEVP ;[BX]= ptr to ERDEV$ sd
	TEST	AH,80H		;Character device ?
	jnz	devr_2		;Yes: go copy name
				;No: block oriented device (disk)
	mov	WORD PTR[BX],2	;device name length to 2
	MOV	BX,[BX+2]	;get ptr to erdev$ string
	mov	word ptr [BX],":A" ;damn hi/low
	mov	ch,DEVICE
	add	byte ptr [BX],ch ;set name
	jmp	short devr_3
devr_2:
	PUSH	DS		;save basic data segment
	MOV	DS,BP		;point to device header segment
	add	si,10		;get to character device name
	mov	cx,8
	mov	es:[BX],cx	;store name length
	mov	di,ES:[BX+2]	;target
	cld
	rep	movsb
	pop	ds		;get back ds
devr_3:
	POP	BP		; get frame at error time
	mov	b$ERDEV,ax	;save in error variable
	CALL	[b$pTEST_CLOSE] ; If closing, close it w/o error checks
	CMP	EXTFLG,0	;violation?
	JNZ	FWP		;if so, then PERMISSION DENIED
	cmp	al,0		;Write protected ?
	jz	dwp
	cmp	al,1		;Device available ?
	je	dna		; no
	cmp	al,9		;no paper ?
	je	nopaper
	cmp	al,10		;device fault ?
	je	dvf
	TEST	AH,80H		;Character device ?
	jnz	devr_4		; yes
	cmp	al,2		;disk not ready
	je	dnr
	cmp	al,7		;disk media error
	je	dme
	jmp	short ioe	;device i/o error
devr_4:
	cmp	al,2		;timeout ?
	je	dto
IOE:
	JMP	B$ERR_IOE	; device i/o error

cEnd	<nogen>			

;***
;B$GETEXTERR -- get extended error info.
;
;Purpose:
; Return extended error info, if available.
; Redone, edit [24]
;
;Entry:
; None
;
;Exit:
; CF (and NZ):	No extended info available (running under < DOS 3.x)
; NC:	[AX] = extended error code
;	ZF	if error was either sharing violation or locking violation
;	NZ	Some other error
;
;Uses:
; If C1: nothing, else only AX.
;
;Preserves: (optional)
; BX, CX, DX
;
;******************************************************************************
cProc	B$GETEXTERR,<NEAR,PUBLIC>,<BX,CX,DX,SI,DI,BP,DS,ES> ; extended
				; error call destroys all these
cBegin
	cCall	B$DOS3CHECK	;See if we're DOS 3 or not
	JC	GETEXTERR_90	;just exit if not
	XOR	BX,BX		;clear for call...
	MOV	AH,59H		;extended error code
	INT	21H		;do the call
	CMP	AX,20H		;test if sharing violation
	JE	GACERR_10	;if so, then return it
	CMP	AX,21H		;test if lock violation
GACERR_10:
	CLC			;C0 means okay
GETEXTERR_90:

cEnd


DTO:
	JMP	B$ERR_DTO	
DVF:
	JMP	B$ERR_DVF	
NOPAPER:
	JMP	B$ERR_OTP	
DNA:
	JMP	B$ERR_DNA	

DME:
	JMP	B$ERR_DME	; give disk media error

DWP:				;permission denied due to write protection
FWP:				;permission denied due to open violation
	JMP	B$ERR_FWP	; give permission denied error

DNR:
	JMP	B$ERR_DNR	; give disk not ready error

	PAGE

;***
; B$IOCLOS - Run-time initialization
;
;Purpose: To deinstall handlers for Divide by zero, Oveflow and I/O interrupts.
;
;Entry:
;	None
;Exit:
;	None
;Modifies:
;	Per convention
;Exceptions:
;	None
;
;****

cProc	B$IOCLOS,<PUBLIC,NEAR>	
cBegin				

ASSUME	ES:DGROUP		
ASSUME	DS:NOTHING

	XOR	AX,AX		;Clear segment
	PUSH	DS		;Save DS
	PUSH	DS		; Restore ES
	POP	ES
	MOV	DS,AX		;Set DS to 0:xxxx
	PUSHF			;Save interrupt status
	CLI			;Turn off interrupts
	RSTVEC	0,ES:DIV0_SAVE
	RSTVEC	4,ES:OVRF_SAVE
	RSTVEC	24h,ES:DEVR_SAVE
	POPF			;Restore interrupts
	POP	DS		;Restore DS

ASSUME	DS:DGROUP


cEnd				; End of B$IOCLOS

sEnd	DV_TEXT 		
	END
