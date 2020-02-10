;************************************************************************
;									*
;	Copyright (C) 1991 by Trace Center 				*
;									*
;	EQUIP.ASM							*
;									*
;************************************************************************

TITLE	Equip
	page 80,180

;        BUG     equ     true


	EXTRN	fslow_baud_mouse:byte

	EXTRN	_singleUserSetup:byte
	EXTRN   _comp_id:byte
	EXTRN   _combase:word
	EXTRN   _vector:byte
	EXTRN   _finject_keys:byte
	EXTRN   fcomp_dialog:byte
	EXTRN	fcomp_dialog_id:byte
	EXTRN	fcomputer_not_found:byte
	EXTRN   btn_1:byte
	EXTRN   btn_2:byte
	EXTRN   Current_Button:byte
	EXTRN   fmouse_driver:byte
	EXTRN   _fmouse_id:byte
	EXTRN   ExtendedSeg:word
	EXTRN   fvideo_type:byte
	EXTRN	comp_flag:byte
	EXTRN	fmousetrapping:byte			; in handicap.asm

	EXTRN	_end:abs				; variable buily by assembler
	EXTRN	_dataBlock:byte				; in param.asm
	EXTRN	_dataBlockSize:word			; in param.asm
	EXTRN	_serialKeysOn:byte
	EXTRN	_skWindowCompatible:byte

	EXTRN   fAccessAlreadyLoaded:byte
	EXTRN	fserial_keys_loaded:byte
	EXTRN	fDialog_Filter_off:byte
	EXTRN	fDialog_Stickeys_off:byte
	EXTRN	fDialog_Mouse_off:byte
	EXTRN   fDialog_Toggle_off:byte
	EXTRN	fDialog_TimeOut_off:byte
	EXTRN	fDialog_Action:byte

	EXTRN	last_address:word			; in TimeOut.asm

	EXTRN	Enable:NEAR				; in Handicap.asm

	EXTRN	_serialKeysEnableFar:FAR		; in SerialKeys
	EXTRN	_serialKeysDisableFar:FAR		

	EXTRN cmdLineLen:byte
	EXTRN intNumber:byte
	EXTRN functionNumber:byte
	EXTRN tsrLoaded:word
	
	EXTRN parameters:dword
	EXTRN paramsOffset:word
	EXTRN paramsSegment:word

	EXTRN originalInt:dword
	EXTRN orgIntOffset:word
	EXTRN orgIntSegment:word

	EXTRN accessIntHandler:near

	EXTRN programStamp:byte
	EXTRN pgm_stamp_len:word
	EXTRN pgm_stamp_ofs:word

DGROUP	GROUP _TEXT, CONST, _DATA, c_common, _BSS

_TEXT	SEGMENT  WORD PUBLIC 'CODE'
_TEXT	ENDS
CONST	SEGMENT  WORD PUBLIC 'CONST'
CONST	ENDS
_DATA	SEGMENT  WORD PUBLIC 'DATA'
_DATA	ENDS
c_common segment word public 'BSS'
c_common ends
_BSS	SEGMENT  WORD PUBLIC 'BSS'
_BSS	ENDS



_BSS	segment
	

include KEYBOARD.INC

	PUBLIC runFrontEnd
	PUBLIC	throwAwayAddr



throwAwayAddr		label	word			; everything below is expendable

display 	MACRO	string
;        	mov	dx,OFFSET string
		lea	dx,string
	        mov	ah,9
        	int	21h
	        ENDM

displaydebug	MACRO	string
		local	displaydebug10

		cmp	_debug,false
		je	displaydebug10
;        	mov	dx,OFFSET string
		lea	dx,string
	        mov	ah,9
        	int	21h
displaydebug10:
	        ENDM

error0Msg       db      'Error opening configuration file',  13, 10, '$'
error0aMsg      db      'Error reading configuration file',  13, 10, '$'
error0bMsg      db      'Error closing configuration file',  13, 10, '$'
error1Msg       db      'Error releasing memory block',      13, 10, '$'
error2Msg       db      'Error spawning child process',      13, 10, '$'

abortAccessMsg  db      'Aborting AccessDOS installation.',			 	   13, 10, '$'
exitMsg         db      'Exiting AccessDOS menu and AccessDOS is still loaded.',	   13, 10, '$'
removeHelperMsg db      'Removing AccessDOS and subroutines from memory.',     		   13, 10, '$'
residentMsg     db      'AccessDOS is now loaded and resident on your computer.',      	   13, 10, '$'
userRespMsg	db	'Press any key to continue. ',   				   13, 10, '$'

debugMsg1	db	'AccessDOS is searching for a mouse, please wait. ',               13, 10, '$'
debugMsg2	db	'AccessDOS is determining your computer type, please wait. ',      13, 10, '$'
debugMsg3	db	'AccessDOS has finished hardware checks.',       		   13, 10, '$'



;;mesg100		db	'SerialKeys installed, but turned off', 13, 10, '$'
;;mesg101		db	'SerialKeys installed, and turned on', 13, 10, '$'


_debug		dw	false				; debug flag set to true when /d on command line

saveStackPtr    dw      ?
saveStackSeg	dw	?


param_block	label   word
        	dw      0				; 0 = use parent environmet segment
cmd_ofs	        dw      ?				; offset of command line sent to child
cmd_seg		dw	?				; segment of command line sent to child
        	dw      5Ch				; FCB1 offset
fcb1_seg        dw      ?				; FCB1 segment
        	dw      6Ch				; FCB2 offset
fcb2_seg        dw      ?				; FCB2 segment


cmd_buf	        db      ?				; command line buffer (also length byte)
cmd_txt	        db      80 dup (?)		
                db      '$',0				; make sure end of string defined

		even
transferBuf 	db	127 dup (?)			; temp storage when reading configuration data
handle		dw	0				; handle to configuration file

programName     db 15 dup ('ADOS.OVL',0)
prgmNameLen	dw 5
defaultPgmName	db 'ADOS.OVL',0
defaultPgmNameLen equ 5


alreadyLoaded   db      false
inhibitmessage	db	false

tempIntNumber	db	0
firstUnusedVect db	0
segOfFirstInst	dw	0

serkeyRoutine	label	dword
serkeyRoutOfs	dw	0
serkeyRoutSeg	dw	0


status			dw	0

INSTALLACCESS          	equ     1			; any modifications made here must also be made in datablk.c
ABORTACCESS            	equ     2			; any modifications made here must also be made in datablk.c


cmdLineSwitch		dw	0

DEBUGON			equ	01h
AUTOLOAD		equ	02h
USECFGFILE		equ	04h
SERIALKEYSPARAM		equ	80h


skCmdLineSwitch		dw	0

SKSINGLEUSER		equ	01h
SKDISABLE		equ	02h
SKENABLE 		equ	04h
SKWINDOWS		equ	08h



		even
stack		db	64 dup ('stackspc')
topOfStack	dw	0

;--------------------------------------------------------------------------------

	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT


runFrontEnd	proc	near

	lea	sp,topOfStack
	call	checkIfLoaded			; are we already installed once
	jnc	runFrontEnd10			; jump if we ARE INSTALLED
	call	loadUs				; install our int vector
runFrontEnd10:
	call	getPath				; get path to ados.xxx
	call	cmdLineTransfer			; copy the command line to our buffer
	call	cmdLineRead			; read the command line switches

; added one variables to inhibit erro messages from 'ados /? ' inquires @ the command line
; this still doesn't protect us if user enter command line switch (good or bad) w/o the '/'

	mov	cl,cmd_buf
	cmp	cl,0				; did we have any command line parameters ?
	je	runFrontEnd20			; if not, carry on
	mov	inhibitmessage,true		; if yes, flag it, so if we later ABORT, do not append 
						; abort message
	
runFrontEnd20:
	test	cmdLineSwitch,DEBUGON		; was the debug switch found
	jz	runFrontEnd30			; jump if not
	mov	_debug,true			; set debug variable

runFrontEnd30:

	; if this is the first instance, we have to determine the equipment setup
	; (this done here because we wanted to get the debug status from the command line)

	cmp	alreadyLoaded,true
	je	runFrontEnd50			; jump if this is the second instance running now

	displaydebug	debugMsg1

	call	_FindMouse			; determine mouse device installed
	displaydebug	debugMsg2

	call	_FindComputer			; determine computer running on
	displaydebug	debugMsg3

	; wait for keystroke if in debug mode
	cmp	_debug,true			; wait for keystroke if in debug mode
	jne	runFrontEnd50			; jump around if not in debug

	displaydebug	userRespMsg
	mov	ah,0Ch				; 21h function C - Clear Keyboard buffer and execute function in al
	mov	al,08h				; 21h function 8 - Console input without echo
	int	21h

runFrontEnd50:
	test	cmdLineSwitch,AUTOLOAD		; if /a on command line, do overlay
	jz	runFrontEnd100
	call	runOverlay
	jmp	runFrontEnd1000

runFrontEnd100:
	test	cmdLineSwitch,USECFGFILE	; was /x on command line
	jz	runFrontEnd200			; jump in NO

	; Force the user to run the overlay if already loaded
	; because some equipment dependent settings in the .cfg file
	; may not apply now and cause problems

	cmp     alreadyLoaded, true
	je	runFrontEnd110			; jump if this is the second instance running now

	call	runWithCFGfile			; try to load the configuration file
	jc	runFrontEnd110			; jump if there was a problem
	mov	status,INSTALLACCESS		; configuration file loaded, now install
	jmp	runFrontEnd1000
runFrontEnd110:
	call	runOverlay			; there was a problem with the cfg file so let overlay handle it
	jmp	runFrontEnd1000

runFrontEnd200:
	test	cmdLineSwitch,SERIALKEYSPARAM	; were there any serial keys switches
	jz	runFrontEnd500			; jump if NO
	cmp	alreadyLoaded,true		; if already loaded, we don't have to run the overlay or the cfg
	jne	runFrontEnd500			; jump if not loaded yet
	mov	status,INSTALLACCESS		; deal with switches later, for now skip overlay and jump to install
	jmp	runFrontEnd1000

runFrontEnd500:
	call	runOverlay			; if no switches (or not loaded and sk switches) do the overlay (sounds like a dance)
	jmp	runFrontEnd1000

;---
runFrontEnd1000:
	; what is the status of ADOS now? Do we install it, quit, modifiy paramaters, er what!

	cmp	status,ABORTACCESS		; should we cancel/abort
	jne	runFrontEnd1020			; jump if NO
	call	cancelAccess			; do cleanup and quit
	jmp	runFrontEndDone

runFrontEnd1020:
	cmp	status,INSTALLACCESS		; should we install it
	jne	runFrontEnd1040			; jump if NO

	; OK let's install/run ADOS
	; If already loaded, install means update remaining parameters and exit,
	; otherwise install means update remaining parameters and tsr

	cmp	alreadyLoaded,true              ; are we already installed once
	je	runFrontEnd1025			; jump if YES we are already installed

	call	updateAndTSR			; initialize our routines and Terminate and Stay resident

	; we do not come back from here unless there is an error
	jmp	runFrontEndDone

runFrontEnd1025:
	call	updateAndExit			; just update the remaining parameters and quit
	jmp	runFrontEndDone

runFrontEnd1040:

runFrontEndDone:
	mov	ax,4c00h
	int	21h
	int	20h
runFrontEnd	endp


;--------------------------------------------------------------
;checkIfLoaded - returns carry flag: carry = NOT LOADED
;
	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT

	PUBLIC checkIfLoaded
checkIfLoaded	proc	near

	; initialize variable for CHECK-IF-LOADED routines below
	mov	firstUnusedVect,0		; signal no vector found yet
	sub	bx,bx				; BX will hold int number 
	mov	bl,START_INT_NUMBER-1
	mov	si,offset programStamp		; si initialized for string compare
	cld
	jmp	short checkIfLoaded20

	assume  es:nothing			; keep here because of looping below
checkIfLoaded10:
	cmp	firstUnusedVect,0		; have we found the first unused vector already?
	jne	checkIfLoaded20			; jump if yes
	mov	firstUnusedVect,bl		; if not, we have now
checkIfLoaded20:
	inc	bx				; get next vector to check
	cmp	bx,END_INT_NUMBER		; are we at end of INTs to check?
	ja	checkIfLoaded60			; if yes, we are not loaded
	mov	di,bx				; get offset 
	shl	di,1				; x 4 to get vector table addr
	shl	di,1				; x 4 to get vector table addr
	sub     ax, ax				; get segment
	mov     es, ax
	assume  es:nothing
	mov     ax, WORD PTR es:[di]		; is there a vector (other than to 0) here
	or      ax, WORD PTR es:[di+2]
	jz	checkIfLoaded10			; jump if we know it is not loaded
	les	di,es:[di]			; get vector routine
	add	di,pgm_stamp_ofs 		; add offset to our ID message
	; es:di from vector table;    ds:si from our string
	mov	cx,pgm_stamp_len
	repe	cmpsb
	jcxz	checkIfLoaded50			; jump if loaded
	jmp	checkIfLoaded20			; we are NOT found

;------------
checkIfLoaded50:
	; yes we are loaded.  Save interrupt number
	mov	segOfFirstInst,es
	mov     alreadyLoaded, true		; signal already loaded
	mov	tempIntNumber, bl
	clc
	jmp	checkIfLoadedDone

;------------
checkIfLoaded60:
	; we have not been installed previously, so save interrupt
	; of the first unused vector

	mov	segOfFirstInst,cs
	mov     alreadyLoaded, false		; signal not already loaded
	mov	al,firstUnusedVect
	or	al,al				; did we find a vector that was not used already
	jnz	checkIfLoaded80			; jump if we had

	; All the vectors apparently are used.  We'll just have
	; to pick one for us to use
	mov	al,DEFAULT_INTERRUPT
checkIfLoaded80:
	mov	tempIntNumber,al		; save it
	stc

;------------
checkIfLoadedDone:
	push	cs
	push    cs
	pop	ds
	pop     es
	assume  es:DGROUP
	ret
checkIfLoaded	endp



;----------------------------------------------------------------------
	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT

	PUBLIC loadUs
loadUs	proc near
	; initialize our ISR variables
	mov     functionNumber, ACCESS_FUNCTION ; function number to respond to
	mov     tsrLoaded, TSR_HERE             ; value to return if TSR is loaded
	mov     paramsOffset, offset _dataBlock ; address of the shared data block
	mov	paramsSegment, cs		; segment of shared data block
	mov	segOfFirstInst,cs
	mov	al,tempIntNumber
	mov     intNumber, al		        ; interrupt to tag onto
	mov	ah, 35h			        ; get the original interrupt handler
	int	21h			        ; old vector in ES:BX
	assume	es:nothing
	mov	orgIntOffset, bx
	mov	orgIntSegment, es

	mov	ah, 25h                         ; set new interrupt handler
	mov	al, intNumber
	mov	dx, offset accessIntHandler
	int	21h
	push	cs
	pop	es
	assume	es:DGROUP
	ret
loadUs	endp

;----------------------------------------------------------------------
	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT

	PUBLIC getPath
getPath	proc near
	; get path of .com file for overlay file
	; only works for DOS 3 and above

	mov	ah,30h				; function 30h: Get DOS Version Number
	int	21h				; major version # in al, minor version # in ah
	cmp	al,3				; if 3 or higher then path can be found
	jge	getPath50
	jmp	getPathDone			; skip looking for path
getPath50:
	mov	es,cs:2Ch			; es now points to environment segment
	assume  es:nothing
	sub	di,di				; di points to offset within environment
	sub	ax,ax				; indicate searching for null
	cld
	mov	cx,8000h			; set up count
getPath100:
	; search for end of environment es:di ( two null chars together )
	repne	scasb
	jcxz	getPathDone			; if count ended then use default name
	scasb					; look for second null character
	jne	getPath100			; jump if not a null character

	; complete path of file running found two bytes after
	inc	di				; bypass two bytes of junk(?)
	inc	di
	mov	si,di				; set si to beginning of path string for later
	mov	cx,-1				; get length
	repne	scasb				; es:di
	jcxz	getPathDone			; if count ended then use default name
	not	cx				; length to . 
	cmp	cx,80h				; make sure it is not too long
	jb	getPath200			; jump if ok
	jmp	getPathDone			; use default if too long
getPath200:
	mov	prgmNameLen,cx			; save length
	lea	di,programName			; store path here
	push	ds				; setup segments so source = ds:si  and destin = es:di
	push	es
	pop	ds
	assume  ds:nothing
	pop	es
	rep	movsb				; transfer string to our buffer; add extension later

	; now set length from beginning to period starting extension
	mov	al,'.'				; set char to look for
	mov	cx,prgmNameLen			; get stored length
	std					; search backward
	dec	di				; point to null
	dec	di				; point to last char
	repne	scasb				; continue until found
	jcxz	getPath300			; if cx = 0 then period not found, use default name
	mov	prgmNameLen,cx			; save length
	jmp	getPathDone

getPath300:
	; restore default program name
	push	cs
	pop	ds
	mov	cx,defaultPgmNameLen
	mov	prgmNameLen,cx
	add	cx,4
	cld
	lea	di,programName
	lea	si,defaultPgmName
	rep	movsb
getPathDone:
	cld
	push	cs
	push	cs
	pop	es				; restore segments
	pop	ds
	assume	ds:DGROUP
	assume  es:DGROUP
	ret
getPath	endp


;----------------------------------------------------------------------
	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT

	PUBLIC cmdLineTransfer
cmdLineTransfer proc near
	; Copy the command line for use by us and the child process
	sub     cx, cx
	mov     cl, cmdLineLen
	inc     cx				; once for length byte
	inc     cx				; again for '0D' at end of command tail
	cld
	mov     si, offset cmdLineLen
;	mov     di, offset cmd_buf
	lea	di,cmd_buf
	rep     movsb
	mov	al,0h				; just to make sure there is an end of string
	stosb
	ret
cmdLineTransfer endp

;----------------------------------------------------------------------
	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT

	PUBLIC cmdLineRead
cmdLineRead proc near
	mov	cmdLineSwitch,0
	mov	skCmdLineSwitch,0
	mov	cl, cmd_buf			; get length
;	mov	di, offset cmd_txt		; es:di
	lea	di,cmd_txt
	mov	al,'/'				; look for switch
cmdLineRead10:
	repne	scasb
	jcxz	cmdLineReadjmp			; finished with command line checking
	mov	bl,[di]				; get character after /

	cmp	bl,'s'				; is it serial keys flag
	jne	cmdLineRead20			; if no, jump
	call	cmdLineReadSK			; look at next characters to verify serial keys
	jmp	cmdLineRead10

cmdLineReadjmp:
	jmp	cmdLineReadDone			; finished with command line checking

cmdLineRead20:
	cmp	bl,'d'				; is it debug flag
	jne	cmdLineRead40			; if no, jump
	or	cmdLineSwitch,DEBUGON		; signal /d detected
	jmp	cmdLineRead10
cmdLineRead40:
	cmp	bl,'a'				; is auto load indicated
	jne	cmdLineRead60			; if no, jump
	or	cmdLineSwitch,AUTOLOAD		; signal /a detected
	jmp	cmdLineRead10
cmdLineRead60:
	cmp	bl,'A'				; is auto load indicated
	jne	cmdLineRead80			; if no, jump
	or	cmdLineSwitch,AUTOLOAD		; signal /a detected
	jmp	cmdLineRead10
cmdLineRead80:
	cmp	bl,'x'				; is it no-load overlay switch
	jne	cmdLineRead100			; if no, jump
	or	cmdLineSwitch,USECFGFILE	; signal /x detected
	jmp	cmdLineRead10			
cmdLineRead100:
	cmp	bl,'X'				; is it no-load overlay switch
	jne	cmdLineRead105			; if no, jump
	or	cmdLineSwitch,USECFGFILE	; signal /x detected
	jmp	cmdLineRead10

cmdLineRead105:
	cmp	bl,'i'				; is it mouse switch
	jne	cmdLineRead110			; if no, jump
	cmp	alreadyLoaded,true		; if already loaded, ignore this switch
	je	cmdLineRead120
	mov	fmousetrapping,true		; if not loaded, set flag and cont.
	jmp	cmdLineRead10
cmdLineRead110:
	cmp	bl,'I'				; is it mouse switch
	jne	cmdLineRead120			; if no, jump
	cmp	alreadyLoaded,true		; if already loaded, ignore this switch
	je	cmdLineRead120
	mov	fmousetrapping,true		; if not loaded, set flag and cont.
	jmp	cmdLineRead10

cmdLineRead120:
	jmp	cmdLineRead10

cmdLineReadDone:
	ret

cmdLineRead	endp

;----------------------------------------------------------------------
	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT

	PUBLIC cmdLineReadSK
cmdLineReadSK	proc near

	; see if serial keys switch
	mov	bl,[di+1]
	cmp	bl,'k'				; is it /sk
	je	cmdLineReadSK10			; jump if yes
	jmp	cmdLineReadSKdone		; it isn't so quit on this one
cmdLineReadSK10:
	; find out what switch
	mov	bl,[di+2]			; get next char
	cmp	bl,'s'				; is it single user flag
	jne	cmdLineReadSK20			; jump if NO
	or	skCmdLineSwitch,SKSINGLEUSER	; signal /sks found
	jmp	cmdLineReadSK100
cmdLineReadSK20:
	cmp	bl,'d'				; is it serial keys disable cmd
	jne	cmdLineReadSK30			; jump if NO
	or	skCmdLineSwitch,SKDISABLE	; signal /skd found
	jmp	cmdLineReadSK100
cmdLineReadSK30:
	cmp	bl,'e'				; is it serial keys enable cmd
	jne	cmdLineReadSK40			; jump if NO
	or	skCmdLineSwitch,SKENABLE	; signal /ske found
	jmp	cmdLineReadSK100
cmdLineReadSK40:
	cmp	bl,'w'				; is it serial keys windows compatible command
	jne	cmdLineReadSKdone		; jump if NO
	or	skCmdLineSwitch,SKWINDOWS	; signal /skw found
cmdLineReadSK100:
	or	cmdLineSwitch,SERIALKEYSPARAM	; signal serial keys command line switch found
cmdLineReadSKdone:
	ret

cmdLineReadSK endp


		
;----------------------------------------------------------------------
; runWithCFGfile returns carry flag: carry set if error
;
	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT


runWithCFGfile:
	; user indicated that we should load with previous configuration file and go
	; without loading the overlay.

	; add filename extension to path
;	mov	di,offset programName
	lea	di,programName
	add	di,prgmNameLen
	mov	al,'C'				; add CFG extension
	stosb
	mov	al,'F'
	stosb
	mov	al,'G'
	stosb
	mov	al,0h
	stosb

	; try to open the configuration file
;	mov	dx,offset programName
	lea	dx,programName
	mov	ah,3Dh				; int 21h function 3Dh - Open a file
	mov	al, 00000000b			; inherited, compatibility mode, read access
	int	21h
	mov	handle,ax			; save handle to file
	jnc	runWithCFGfile100		; jump if no error

	; error of some type detected.  Close file and run ovl
	mov	ah,3Eh				; int 21h function 3Eh - Close a File handle
	mov	bx,handle
	int	21h
	display error0Msg
	stc
	jmp	runWithCFGfileDone		; run overlay, probably no cfg file

runWithCFGfile100:
	; read file into buffer
	mov	cx,_dataBlockSize		; get size of buffer
;	mov	dx,offset transferBuf
	lea	dx,transferBuf
	mov	ah,3Fh				; int 21h function 3Fh - Read from a file or device
	mov	bx,handle			; get handle to file
	int	21h
	cmp	ax,_dataBlockSize		; see how many bytes read
	je	runWithCFGfile200		; jump if ok

	; something is wrong with the config file. Close it and run ovl
	mov	ah,3Eh				; int 21h function 3Eh - Close a File handle
	mov	bx,handle
	int	21h
	display	error0aMsg			; 
	stc
	jmp	runWithCFGfileDone		; run overlay, probably no cfg file

runWithCFGfile200:
	; Bytes have been read into buffer.  Close file

	mov	ah,3Eh				; int 21h function 3Eh - Close a File handle
	mov	bx,handle
	int	21h
	jnc	runWithCFGfile300		; jump if no error
	display error0bMsg			; display error and continue

runWithCFGfile300:
	; now store data in shared parameter block
	; set up so ds:si = source (ds = this instance)
	;           es:di = destin (es = first instance)

	lea	si,transferBuf
	mov	di,offset _dataBlock
	mov	cx,_dataBlockSize
	cld
	cli					; don't want to upset ADOS if interrupts come along
	rep	movsb
	mov	fDialog_Filter_off,true
	mov	fDialog_Stickeys_off,true
	mov	fDialog_Mouse_off,true
	mov	fDialog_Toggle_off,true
	mov	fDialog_TimeOut_off,true
	mov	fDialog_Action,true
	sti
	clc
	jmp	runWithCFGfileDone

runWithCFGfileDone:
	ret


;----------------------------------------------------------------------
	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT

	PUBLIC runOverlay
runOverlay	proc near

	call	freeMemory
	jnc	runOverlay20
	display error1Msg
	mov	status,ABORTACCESS
	jmp	runOverlayDone

runOverlay20:
	call	spawnChild
	jnc	runOverlay40
	display error2Msg
	mov	status,ABORTACCESS
	jmp	runOverlayDone

runOverlay40:
	mov     ah, 4dh				; get return code
	int     21h
	cmp     al, ABORTACCESS			; Does user want to abort ACCESS?
	jne	runOverlay60
	mov	status,ABORTACCESS
	jmp	runOverlayDone
runOverlay60:
	cmp     al, INSTALLACCESS		; load access?
	jne	runOverlay80
	mov	status,INSTALLACCESS
	jmp	runOverlayDone
runOverlay80:
	mov	status,ABORTACCESS
	jmp	runOverlayDone

runOverlayDone:
	ret
runOverlay endp

;----------------------------------------------------------------------
; freeMemory returns carry set if error

	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT

	PUBLIC freeMemory
freeMemory	proc near
	mov	bx, offset _end		        ; free memory for child process
	mov	cl, 4
	shr	bx, cl
	inc     bx                              ; add 1 paragraph for rounding
	mov     ah, 4Ah
	int     21h
	; returns code in carry bit
	ret
freeMemory endp
        

;----------------------------------------------------------------------
; spawnChild returns carry set if error

	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT

	PUBLIC spawnChild
spawnChild	proc near

	; add filename extension to path
;	mov	di,offset programName
	lea	di,programName
	add	di,prgmNameLen
	mov	al,'O'				; add OVL extension
	stosb
	mov	al,'V'
	stosb
	mov	al,'L'
	stosb
	mov	al,0h
	stosb

	mov	ax, cs                          ; set up parameter block for child process
	mov	cmd_seg, ax
	mov	fcb1_seg, ax
	mov	fcb2_seg, ax
	lea	ax,cmd_buf
	mov	cmd_ofs,ax

	lea	dx,programName			; ds:dx points to program name
	lea	bx,param_block			; es:bx points to parameter block
	mov     saveStackPtr,sp		        ; save the stack pointer
	mov	saveStackSeg,ss
	mov	ax, 4b00h                       ; execute child process
	int	21h
	cli
	assume  es:nothing
	assume  ds:nothing
	assume	ss:nothing

	mov	ss, cs:saveStackSeg		; seg reg done first so no interrupt
	mov	sp, cs:saveStackPtr		;   until after the next instruction
	mov	cx, cs			        ; restore segment regs and stack
	mov	ds, cx
	mov	es, cx
	assume  es:DGROUP
	assume  ds:DGROUP
	assume	ss:DGROUP
	sti

	; returns code in carry bit
	ret

spawnChild endp


;----------------------------------------------------------------------
	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT

	PUBLIC updateAndTSR
updateAndTSR	proc near
	;this is the first instance so we already have segment register
	mov     fAccessAlreadyLoaded, true      ; set fAccessAlreadyLoaded (in Shared Parameter Block) to true

	cmp	fcomputer_not_found, false	; if computer was not found, we must call Find_Computer again
	je	updateAndTSR20
	call	_FindComputer

updateAndTSR20:
	call	Enable			        ; initialize Handicap and SerialKeys features
	display cs:residentMsg

	cmp	_serialKeysOn,true  		; is SerialKeys code installed ?
	je	updateAndTSR100	    		; yes, do TSR at _end
				    		; if not, do TSR at
	mov	dx,offset last_address		; last_address which is in TimeOut
				      		; last file of keyboard/mouse linked
	mov	fserial_keys_loaded,false	; set to false if SerialKeys not initially loaded
	jmp	short updateAndTSR200
	
updateAndTSR100:
	mov	fserial_keys_loaded,true	; set to true if SerialKeys is initially loaded

	test	cmdLineSwitch,SERIALKEYSPARAM
	jz	updateAndTSR150
	test	skCmdLineSwitch,SKSINGLEUSER
	jz	updateAndTSR150
	mov	_singleUserSetup,true
	jmp	updateAndTSR155

updateAndTSR150:
	mov	_singleUserSetup,false
updateAndTSR155:
;	mov	dx, offset _end
;	mov	dx, offset throwAwayAddr
	lea	dx,throwAwayAddr
	
updateAndTSR200:

	mov	cl,4
	shr	dx,cl
	inc	dx				; dx # of paragraphs
	inc	dx
	mov	ax,3100H
	int	21H				;TSR

	; don't come back unless error of some sort?

	ret

updateAndTSR endp

;----------------------------------------------------------------------
	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT

	PUBLIC updateAndExit
updateAndExit	proc near
	display cs:exitMsg

	mov	es,segOfFirstInst		; get segment of dataBlock, 1st instance
	assume	es:nothing
	cmp	es:fserial_keys_loaded,true	; was serial keys loaded
	je	updateAndExit50			; jmp if YES
	jmp	updateAndExitDone		; nothing to update so just exit

updateAndExit50:

; if we get here, fserial_keys_loaded must be true, this means AccesDOS must have been loaded, and SerialKeys
; was installed.  Therefore, whenever we exit again, we must check if we need to disable SerialKeys or re-enable it
; in case the user choose to turn SerialKeys On/Off from the menu !!!

; We also have to check for command line switches and change globals accordingly

	; assume no switches so change settins back to default
	mov	es:_singleUserSetup,false
	mov	es:_skWindowCompatible,false

	test	cmdLineSwitch,SERIALKEYSPARAM	; any serial keys stuff
	jz	updateAndExit150		; jump if NO serial keys switches

	test	skCmdLineSwitch,SKSINGLEUSER	; single user request?
	jz	updateAndExit100		; jump if NOT single user
	mov	es:_singleUserSetup,true	; set single user flag

updateAndExit100:
	; for the rest of the flags more than one on the command line doesn't make sense
	; so we will accept only one in the following priority- enable, disable, windows compatible

	test	skCmdLineSwitch,SKENABLE	; enable serial keys with current settings
	jz	updateAndExit105		; jump if NO
	mov	es:_serialKeysOn,true		; set global flag ON
	jmp	updateAndExit150
updateAndExit105:
	test	skCmdLineSwitch,SKDISABLE	; disable serial keys 
	jz	updateAndExit110		; jump if NO
	mov	es:_serialKeysOn,false		; set global flag OFF
	jmp	updateAndExit150
updateAndExit110:
	test	skCmdLineSwitch,SKWINDOWS	; disable serial keys for windows batch file
	jz	updateAndExit150		; jump if NO
	mov	es:_serialKeysOn,false		; turn serial keys OFF
	mov	es:_skWindowCompatible,true	; set WINDOWS flag ON so DOS SHELL will run serial keys

updateAndExit150:

	mov	ax,es
	mov	cs:serkeyRoutSeg,ax		; set up segment of enable/disable routines
	cmp	es:_serialKeysOn,true		; should serial keys be ON
	jne	updateAndExit225		; jump if NO

	mov	cs:serkeyRoutOfs,offset _serialKeysEnableFar
	call	dword ptr cs:serkeyRoutine	; make sure serial keys is ON
	jmp	updateAndExitDone

updateAndExit225:
	mov	serkeyRoutOfs,offset _serialKeysDisableFar
	call	dword ptr cs:serkeyRoutine	; make sure serial keys is OFF
	jmp	updateAndExitDone

updateAndExitDone:
	push	cs
	push	cs
	pop	es
	pop	ds
	ret
updateAndExit endp

;----------------------------------------------------------------------
	assume	cs:DGROUP
	assume  es:_TEXT
	assume  ds:_TEXT
	assume	ss:_TEXT

	PUBLIC cancelAccess
cancelAccess	proc near
	; if already Loaded, then cancel simply means quit,
	; otherwise cancel means remove our routine
	cmp	alreadyLoaded,false
	je	cancelAccess10			; jump if not already loaded
	cmp	inhibitmessage,true
	je	cancelAccess5
	display cs:exitMsg
cancelAccess5:
	mov	inhibitmessage,false		; reset
	jmp	cancelAccessDone
cancelAccess10:
	cmp	inhibitmessage,true
	je	cancelAccess15
	display cs:removeHelperMsg
cancelAccess15:
	mov	inhibitmessage,false		; reset
	mov	ah, 25h                         ; Set interrupt Vector
	mov     al, tempIntNumber
	mov     dx, cs:orgIntOffset
	mov	bx, cs:orgIntSegment            ; remove our TSR and replace original
	push    ds
	mov     ds, bx
	assume	ds:nothing
	int	21h
	pop     ds
	assume ds:DGROUP
cancelAccessDone:
	ret
	;exit
cancelAccess endp


;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
; Equipment check starts here
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------


	assume cs:DGROUP
	assume ds:nothing
        assume es:nothing
	assume ss:nothing


;;mesg00		DB	 13, 10, 13, 10, "$"
mesg0		DB	"Mouse driver not found", 13, 10, "$"
mesg1		DB	"Mouse not found", 13, 10, "$"
mesg2		DB	"Mouse found and reset", 13, 10, "$"
mesg3		DB	"A bus mouse was found", 13, 10, "$"
mesg4		DB	"A serial mouse was found", 13, 10, "$"
mesg5		DB	"An InPort mouse was found", 13, 10, "$"
mesg6		DB	"A PS/2 mouse was found", 13, 10, "$"
mesg7		DB	"A HP mouse was found", 13, 10, "$"
mesg8		DB	"Mouse type found was not recognized", 13, 10, "$"
;;mesg9		DB	"Com Port 2 Int. enabled, serial mouse is here", 13, 10, "$"
;;mesg10		DB	"Com Port 1 Int. enabled, serial mouse is here", 13, 10, "$"
mesg11		DB	"Mouse type found is not currently supported", 13, 10, "$"
mesg14		DB	"Computer ID is an IBM PS/2 Model L40SX, 55SX, 65SX, 70, 80, 90 or 95 ", 13, 10, "$"
mesg15		DB	"Computer ID is an IBM PS/1 or PS/2 Model 25/30-80286, 50, 50Z, or 60", 13, 10, "$"
mesg15A		DB	"Computer ID is an IBM AT or equivalent compatible", 13, 10, "$"
mesg15B		DB	"Computer ID is an IBM New AT/XT or equivalent 286/386/... compatible", 13, 10, "$"
mesg16		DB	"Computer ID is an IBM PS/2 Model 25/30-8086", 13, 10, "$"
mesg17		DB	"Computer ID is an IBM Original AT with 84 key keyboard", 13, 10, "$"
mesg17A		DB	"Computer ID is an IBM Original AT with 101 key keyboard", 13, 10, "$"
mesg18		DB	"Computer ID is an IBM PC Convertible and is not supported", 13, 10, "$"
mesg19		DB	"Computer ID is an IBM PC Junior and is not supported", 13, 10, "$"
mesg20		DB	"Computer ID is an IBM New PC/XT", 13, 10, "$"
mesg21		DB	"Computer ID is an IBM PC or an Original IBM PC/XT", 13, 10, "$"
mesg30		DB	"The serial mouse was found to be attached to COMM port 2", 13, 10, "$"
mesg31		DB	"The serial mouse was found to be attached to COMM port 1", 13, 10, "$"
mesg32		DB	"Computer type found to support Inject Keys routine", 13, 10, "$"
mesg33		DB	"Extended BIOS data area not found, PS/2 style MouseKeys not supported, try Serial Mouse", 13, 10, "$"
mesg47		DB	"Your computer supports int 15h calls", 13, 10, "$"
mesg48		DB	"Your computer supports the Extended BIOS data area", 13, 10, "$"
mesg49		DB	"A CGA monitor was detected", 13, 10, "$"
mesg50		DB	"A EGA monitor was detected", 13, 10, "$"
mesg51		DB	"A VGA monitor was detected", 13, 10, "$"
mesg52		DB	"A Monochrome monitor was detected", 13, 10, "$"
mesg53		DB	"A MCGA monitor was detected", 13, 10, "$"

mesg55		db	"Your serial mouse is runnning at 300 BAUD", 13, 10, "$"

mesg22		DB	"Computer was not identifiable and will be treated as a PC/XT/AT with an 84 key keyboard.", 13, 10, "$"
mesg22A		DB	"Please restart AccessDOS menu, type ados,  to change this selection if your ", 13, 10, "$"
mesg22B		DB	"computer is NOT a PC/XT/AT with an 84 key keyboard.", 13, 10, "$"

mesg70		DB	"AccessDOS detected DOS keyb.com to be running.", 13, 10, "$"

;j1		DB	"step1", 13, 10, "$"
;j2		DB	"step2", 13, 10, "$"
;j3		DB	"step3", 13, 10, "$"
;j4		DB	"step4", 13, 10, "$"

bios_table	label	byte

db	"01/10/84"		; OldAT	
db	"06/10/85"		; AT239	
db	"11/15/85"		; AT339	
db	"04/21/86"		; XT286	


NOMOUSE		equ 0
BUSMOUSE	equ 1
SERIALMOUSE	equ 2
INPORTMOUSE	equ 3
PS2MOUSE	equ 4
HPMOUSE		equ 5



print	MACRO	string
;	mov	dx,OFFSET string
	lea	dx,string
	mov	ah,9
	int	21h
	ENDM

;----------------------------------------------------------------------------
; Find_Mouse
; 
; This routine determines if a valid mouse driver is loaded, and what kind of hardware
; mouse is attached to the computer.  It was intended to be called prior to loading
; AccessDos, and therefore if a mouse was found and identified, MouseKeys would be 
; allowed to work.  By blocking it out into seperate code, I can now call it any time, 
; and therefore, the mouse driver could be loaded after AccessDos, and we could still
; use MouseKeys.
;
; Determine presence of MOUSE driver (from Microsoft's mouse prog. ref.)
; check first for a valid mouse interrupt

	PUBLIC _FindMouse
_FindMouse	proc	near
	
        push    si                                      ; save registers
        push    di
        push    ax
        push    bx
        push    cx
        push    dx

        push    ds
        push    es

        mov     ax, cs					; set up data segment
        mov     ds, ax
	assume	ds:DGROUP


	mov	ax,3533h				; get int. 33h vector
	int	21h

	assume	es:nothing
	mov	ax,es
	or	ax,bx
	jz	no_mouse_driver
	cmp	byte ptr es:[bx],207			; iret?
	jne	mouse_reset

no_mouse_driver:
	cmp	_debug, false
	je	debug_0
        print   mesg0
debug_0:
	mov	fmouse_driver,false
	mov	_fmouse_id,false				; if mouse not found, keep fmouse_id=0
	jmp	mouse_driver_end

mouse_reset:
  	xor	ax,ax
	int	33h					; check to see if mouse found
	or	ax,ax					; if mouse not found, ax will = 0
	jnz	mouse_found

	cmp	_debug, false
	je	debug_1
        print   mesg1
debug_1:

	mov	fmouse_driver,false
	jmp	mouse_driver_end

mouse_found:

	cmp	_debug, false
	je	debug_2
        print   mesg2
debug_2:

; determine which type of mouse was found

	mov	ax,36					; get version, mouse type and IRQ #
	int	33h					; call int, mouse type is returned in "Ch" register
	cmp	ch,BUSMOUSE				; was it a bus mouse
	jne	mou_drv_5

	cmp	_debug, false
	je	debug_3
        print   mesg3
        print   mesg11
debug_3:

	jmp	mouse_driver_end

mou_drv_5:
	cmp	ch,SERIALMOUSE
	je	mou_drv_5B
	jmp	mou_drv_10

mou_drv_5B:

	cmp	_debug, false
	je	debug_4
        print   mesg4
debug_4:

	mov	_fmouse_id,SERIALMOUSE
	mov	fmouse_driver,true
	mov	btn_1,20h				; define mouse btn_1
	mov	btn_2,10h				; define mouse btn_2
	mov	Current_Button,20h			; default to left button

; find out which int. we are using so we can identify which COMM port the mouse is attached to ???
; check which int. number was returned so we can load the correct COMM port address for the serial mouse

	cmp	cl,3					; if cl=3, then the mouse was found on COMM 2 or 4
	jne	mou_drv_5A
	mov	_combase,02f8h				; set for COM 2

	cmp	_debug, false
	je	debug_5
        print   mesg30
debug_5:

	jmp	short mou_drv_6

mou_drv_5A:
	mov	_combase,03f8h				; set for COM 1

	cmp	_debug, false
	je	debug_6
        print   mesg31
debug_6:

mou_drv_6:

; One more check to look for a slow (300 baud) serial mouse

	cli						; prevent interrupts while changing registers in serial port

	mov	dx,_combase
	add	dx,3
	in	al,dx					; get line control register
	mov	ah,al					; save status in ah for later restore
	or	al,080h					; prepare to set bit 7 high
	out	dx,al					; write to line control register so we can read lsb/msb of divisor latch

	mov	dx,_combase	
	in	al,dx					; get LSB
	mov	bl,al					; save in bl
	inc	dx
	in	al,dx					; get MSB
	mov	bh,al					; save in bh
	mov	al,ah					; get saved status of line control register

	mov	dx,_combase
	add	dx,3
	out	dx,al					; restore line control register

	sti						; re-enable interrupts

; Now check divisor in bh,bl and see if 300 Baud (384d or 180h), 1200 Baud (96d or 60h) or 2400 Baud (48d or 30h)
; Really only concerned with 300 Baud, so we will look for bh=01h,bl=80h for 300 Baud and set a flag if we find it
	
	cmp	bh,01h					; is this 300 baud ?
	jne	mou_drv_7
	cmp	bl,80h					; ""
	jne	mou_drv_7

	mov	fslow_baud_mouse,true			; flag that we found a slow 300 buad rate mouse
	cmp	_debug,false
	je	mou_drv_7
	print	mesg55					; print out message that we found a slow mouse

mou_drv_7:

	jmp	mouse_driver_end
;-----------------------------------------------------------------------------

mou_drv_10:
	cmp	ch,INPORTMOUSE
	jne	mou_drv_15

	cmp	_debug, false
	je	debug_7
        print   mesg5
        print   mesg11
debug_7:

	jmp	mouse_driver_end

mou_drv_15:
	cmp	ch,PS2MOUSE
	jne	mou_drv_20

	cmp	_debug, false
	je	debug_8
        print   mesg6
debug_8:

	mov	_fmouse_id,PS2MOUSE
	mov	fmouse_driver,true

;initialize or get mouse driver location for use later when MouseKeys is called

	sub 	ax,ax					; zero ax
	mov	ah,0c1h					; preload ah with "c1"
	int 	15h			   		; call int15 to get segment of mouse driver
	jc	mou_drv_18				; if call returns and carry flag set, erro occurred, flag it and exit
	mov	ax,es					; load mouse driver segment address into variable extendseg
	mov 	ExtendedSeg,ax		       	
	mov	btn_1,1					; define mouse btn_1
	mov	btn_2,2					; define mouse btn_2
	mov	Current_Button,01h			; default to left button
	jmp	mouse_driver_end

mou_drv_18:

	cmp	_debug, false
	je	debug_9
        print   mesg33
debug_9:

	jmp	mou_drv_26

mou_drv_20:
	cmp	ch,HPMOUSE
	jne	mou_drv_25

	cmp	_debug, false
	je	debug_10
        print   mesg7
        print   mesg11
debug_10:

	jmp	mouse_driver_end

mou_drv_25:

	cmp	_debug, false
	je	debug_11
        print   mesg8
debug_11:

mou_drv_26:
	mov	fmouse_driver,false
	mov	_fmouse_id,false				; if mouse found but not recognized, keep fmouse_id=0

mouse_driver_end:
        pop     es                                      ; restore registers
        pop     ds

        pop     dx
        pop     cx
        pop     bx
        pop     ax
        pop     di
        pop     si
	ret
_FindMouse	endp





;----------------------------------------------------------------------------
; Find_Computer
;
; Determine computer type.  For a complete computer ID list, see Keyboard.Inc file
; This routine was designed to call the ROM to determine what kind of computer
; that AccessDos was being installed on.  This is necessary to set up various flags
; that are required through the routines.
;
; With the use of int15, ah=0C0h, we can poll the hardware and check the computer ID, submodel,
; etc. just as easily, so we start out this way.  If it is determined that the computer
; does not support int 15h, then the calls to the ROMBIOS are made directly.
;
;--------------------------------------------------
; Need to add the ability to set neither/either/ or both computer flags for various other routine 
; which follow the comp_id needs
;--------------------------------------------------

	PUBLIC _FindComputer
_FindComputer	proc near

;-----------------------------------------------------------------------------
; Find out if computer supports int 15h calls.  Upon return , if the Carry flag is set, 
; (i.e. CF=1), then it definitely does not support int 15h calls.  If the Carry Flag is clear
; (i.e. CF=0), then we must query deeper into a data block to determine if int 15h is supported.
; This is also a traditional call for computer ID bytes.......

	assume cs:DGROUP
	assume ds:nothing
        assume es:nothing
	assume ss:nothing

	
        push    si                                      ; save registers
        push    di
        push    ax
        push    bx
        push    cx
        push    dx

        push    ds
        push    es

        mov     ax, cs					; set up data segment
        mov     ds, ax
	assume	ds:DGROUP

	mov	ah,0c0h					; load Ah register with C0h
	int	15h					; try an int 15
	assume	es:nothing
	cmp	ah,0					; if ah = 0, then we can check further
	je	FC_3					; if zero, we can check for other flags
							; if NOT zero, then Int 15 NOT supported, set vector and jump ahead
FC_2:

; last resort, check if DOS 4.0+/5.0.. is running with keyb.com, as it doesn't change ROM table which int 15h, with 
; C0h would test, just replaces int 9h, with an int 9/int15h equivalent, and we could use the int 15h intercept
; if we can detect keyb.com running.  We can check for this by using int 2Fh, with "ax"= AD80h.  IF "ax" comes back unaltered,
; then keyb.com is not loaded, if "ax" comes back as ffffh, then keyb.com is loaded, and we should use int 15h intercept...

	mov	ax,0AD80h				; request for kryb.com info ??
	int	2Fh					; interrrupt 
	cmp	ax,0ffffh				; is keyb.com loaded
	jne	FC_2A					; if yes, use int. 15h intercept
	cmp	_debug,false
	je	FC_4
	print	mesg70					; inform us that DOS keyb.com is running
	jmp	short FC_4

FC_2A:
	mov	_vector,09h				; kybd must use int. vector 09
	jmp	FC_100					; no need to check data area and PS/2 computer, 

FC_3:
	mov	ah,es:[bx+5]				; get descriptor byte that will tell us if int15h supported
	test	ah,10h					; mask off bit 4
	jz	FC_2					; if zero, bit 4 not set and int 15h intercept not supported, go back to int 9h
FC_4:
	mov	_vector,15h				; kybd can use int. vector 15


	cmp	_debug, false
	je	debug_12
        print   mesg47
	mov	ah,es:[bx+5]				; get descriptor byte again since print wiped out Ah register
	test	ah,04h					; mask off bit 2
	jz	FC_10
        print   mesg48
debug_12:


FC_10:
; Is the operator overriding the computer type ?

	cmp	fcomp_dialog,true			; has the computer type been over ridden by user from dialog box ??
	jne	FC_20					; if not use int 15h to cont
	jmp	FC_200					; if yes, cont. here

FC_20:
;------------------------------------------------------------------------------------------------------------------------------------------
; IBM PS/1 and PS/2 determination done here
;------------------------------------------------------------------------------------------------------------------------------------------
	xor	ax,ax					; zero ax register
	mov	al,es:[bx+2]				; get model byte

	cmp	al,0f8h					; DO WE HAVE A MODEL 55sx,65sx,70,80,90,95,L40sx ???
	jne	FC_23
;;        jmp     FC_175                                ; debug code to force computer not found on model 55, 70, 80

	mov	_comp_id,8				; 
	mov	comp_flag,true				; flag to tell us that we have a computer which supports BIOS kb_flag_1/3

	cmp	_debug, false
	je	debug_13
        print   mesg14
debug_13:

	mov	_finject_keys,true			; flag to tell us the computer supports key injection at keyboard hardware buffer

	cmp	_debug, false
	je	debug_14
        print   mesg32
debug_14:

	jmp	FC_end

FC_23:
	cmp	al,0fch					; DO WE HAVE A MODEL PS/1, 25/286, 30/286, 50, 60 or IBM New XT/AT ???
	je	FC_24
	jmp	FC_30

FC_24:

; if we have a PS/1 or PS/2 Model 25/30-286, 50, 60, then we can inject.  If we have either the  New XT/286
; or New AT 339, we CANNOT inject at the hardware but may use int15h.  We need to look at the sub model byte
; to determine this.

	mov	al,es:[bx+3]				; get submmodel byte

	cmp	al,0Bh					; was it a PS/1 ?
	je	FC_25

	cmp	al,04h					; was it a PS/2 Model 50,50z ?
	je	FC_25

	cmp	al,05h					; was it a PS/2 Model 60 ?
	je	FC_25

	cmp	al,09h					; was it a PS/2 Model 30/286 or 25/286 ?
	jne	FC_27

FC_25:

	cmp	_debug, false
	je	debug_15
        print   mesg32
debug_15:

	mov	_comp_id,7				; PS1, 25/30-386,50,60,50Z,
	mov	_finject_keys,true			; flag to tell us the computer supports key injection at keyboard hardware buffer
	mov	comp_flag,true				; flag to tell us that we have a computer which supports BIOS kb_flag_1/3

	cmp	_debug, false
	je	debug_16
        print   mesg15
debug_16:

	jmp	FC_end

FC_27:
; turns out that only IBM's will allow the write/read ot the 8042 trick, therefore, I will check if we 
; have an IBM Old AT, New AT (2 models) or a New XT, if not I will distinguis with another _comp_id value of 5Bh
; Also turns out that CLONES compy the FC,submodel,and revision such that I cannot distinguish between them.  THe only other method I 
; know of, is to look at the actual BIOS dates, saved as strings above 90-7 characters)
;
;	OldAT,AT239,AT339,XT286

	mov	ax,ROMBIOS				; Load segment location into
	mov	es,ax					; ES register also

	xor	ax,ax
	xor	di,di
	xor	dx,dx
	xor	si,si

string_5:
	xor	bx,bx
	mov	cx,8
string_6:


	mov	al,es:computer_bios_date[bx]
;	mov	dl,offset bios_table[si]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	mov	dl,bios_table[si]

	cmp	al,dl
	jne	string_10
	dec	cx
	cmp	cx,0
	je	FC_27A
	inc	bl
	inc	si
	jmp	string_6

string_10:

	inc	di
	cmp	di,4
	je	string_20
	xor	si,si
	mov	si,di
	mov	cx,3
	shl	si,cl
	jmp	string_5

string_20:


	mov	_comp_id,5Bh				; we have a CLONE !!!stick with reading 8042 status method
	jmp	short FC_28

FC_27A:

	mov	_comp_id,5Ah				; we have an New XT/AT, Clone, or Old AT running DOS 4+/5 w/keyb.com
FC_28:

;Need to check if we have 84 or 101 style kybd before we allow next flag to be set
; since the AT-239 (i.e I think this is the 6/10/85 BIOS date) supportes int 15h but may use an 84
; key keyboard. Most clones of 286 or higher will probably be using 101 key keyboard, but this simple check
; should tell us

	mov	ax,RAMBIOS				; Load segment location into
	mov	es,ax					; ES register also
	mov	al,es:[kb_flag_3]			; get 40:96h contents
	test	al,010h					; mask off bit 4, 101/102 kybd id bit
	jnz	FC_29					; if not ZERO, bit is set and we have 101 kybd

	cmp	_debug, false
	je	debug_17
        print   mesg15A
debug_17:

	jmp	FC_end

FC_29:
	mov	comp_flag,true			 	; flag to tell us that we have a computer which supports BIOS kb_flag_1/3

	cmp	_debug, false
	je	debug_18
        print   mesg15B
debug_18:

	jmp	FC_end

FC_30:

	cmp	al,0fah					; DO WE HAVE A MODEL 25/86 or 30/86 ???
	jne	FC_40

FC_32:	
	mov	_comp_id,6				; 
	mov	comp_flag,true				; flag to tell us that we have a computer which supports BIOS kb_flag_1/3


	cmp	_debug, false
	je	debug_19
        print   mesg16
debug_19:

	jmp	FC_end

FC_40:

	cmp	al,0fbh					; DO WE HAVE A New IBM PC/XT ???
	jne	FC_50
	mov	_comp_id,2				; 

	mov	ax,RAMBIOS				; Load segment location into
	mov	es,ax					; ES register also
	mov	al,es:[kb_flag_3]			; get 40:96h contents
	test	al,010h					; mask off bit 4, 101/102 kybd id bit
	jz	FC_45					; if ZERO, bit is clear and we do not have 101 kybd
	mov	comp_flag,true				; flag to tell us that we have a computer which supports BIOS kb_flag_1/3


	cmp	_debug, false
	je	debug_20
        print   mesg20
debug_20:


FC_45:
	jmp	FC_end

FC_50:

;******************************************************************************************************************************************
; If we get here, we have exhausted all IBM combinations of int15h computers that can do the key injection either 
; first by the hardware or second by the writing/reading from the 8042.  Note above, the only way the Old AT (1/10/84)
; can do int15h is with DOS 4+/5 and keyb.com running.  Also Note, that the only reason the OldAT gets caught above
; is that it has the same default ID byte, "FC", as a new AT.  Unfortunately, any other computer running
; will not be ID'ed in ROM, but may have been decidedly made an int 15h computer via keyb.com.  There-
; fore, I will need to go direct to the hardware ID byte and check further for these int 15h computers.
;
; Any IBM computers found beyond this point supporting int15h, are doing so
; because they are running DOS 4+/5 w/keyb.com loaded.  Since we already caught the Old AT, all that is left that we will support
; is the original PC or PC/XT (do not support PCjr or PCconv.).  I treat the PC and PC/XT as a computer ID of 1, so I can catch them both
; if I call them something like 1A...Beyond this check, we should jump to FC_175 for un ID'ed computer running int 15h.


	mov	ax,ROMBIOS				; Load segment location into
	mov	es,ax					; ES register also

	xor	ax,ax
	mov	al,es:[computer_model_id]


	cmp	al,0feh					; Do we have an old XT
	jne	FC_52
	jmp	short FC_55
FC_52:
	cmp	al,0ffh					; Do we have an old PC
	jne	FC_60
FC_55:

	mov	_comp_id,1Ah				; 
	mov	ax,RAMBIOS				; Load segment location into
	mov	es,ax					; ES register also
	mov	al,es:[kb_flag_3]			; get 40:96h contents
	test	al,010h					; mask off bit 4, 101/102 kybd id bit
	jz	FC_58					; if ZERO, bit is clear and we do not have 101 kybd
	mov	comp_flag,true				; flag to tell us that we have a computer which supports BIOS kb_flag_1/3


	cmp	_debug, false
	je	debug_21
        print   mesg21
debug_21:


FC_58:
	jmp	FC_end					; go check video

FC_60:
	jmp	FC_175					; computer type not found above

;------------------------------------------------------------------------------------------------------------------------------------------
; NON IBM PS/1 and PS/2 determination done here
;------------------------------------------------------------------------------------------------------------------------------------------
; If we junp to here, int 15h not supported, vector = 09h, and we nust go directly to the
; hardware to try to ID the ocmputer

FC_100:

	mov	ax,ROMBIOS				; Load segment location into
	mov	es,ax					; ES register also

	xor	ax,ax
	mov	al,es:[computer_model_id]

	cmp	al,0fch					; do we have Orig AT ?
	jne	FC_110

	mov	_comp_id,5				; we have an Original IBM AT

	mov	ax,RAMBIOS				; Load segment location into
	mov	es,ax					; ES register also
	mov	al,es:[kb_flag_3]			; get 40:96h contents
	test	al,010h					; mask off bit 4, 101/102 kybd id bit
	jz	FC_105					; if ZERO, bit is clear and we do not have 101 kybd

	mov	comp_flag,true				; flag to tell us that we have a computer which supports BIOS kb_flag_1/3
	cmp	_debug, false
	je	debug_22
        print   mesg17A
	jmp	FC_end

FC_105:
	cmp	_debug, false
	je	debug_22
        print   mesg17
debug_22:

	jmp	FC_end

FC_110:
	cmp	al,0f9h					; DO WE HAVE AN IBM CONVERTIBLE ???
	jne	FC_120

	mov	_comp_id,4				;

	cmp	_debug, false
	je	debug_23
        print   mesg18
debug_23:

	jmp	FC_end

FC_120:
	cmp	al,0fdh					; DO WE HAVE AN IBM PC/JR ???
	jne	FC_130

	mov	_comp_id,3				; 

	cmp	_debug, false
	je	debug_24
        print   mesg19
debug_24:

	jmp	FC_end

FC_130:


FC_135:
	cmp	al,0feh					; DO WE HAVE AN IBM PC OR Original PC/XT ???
	jne	FC_140

	mov	_comp_id,1				; 

	mov	ax,RAMBIOS				; Load segment location into
	mov	es,ax					; ES register also
	mov	al,es:[kb_flag_3]			; get 40:96h contents
	test	al,010h					; mask off bit 4, 101/102 kybd id bit
	jz	FC_136					; if ZERO, bit is clear and we do not have 101 kybd

	mov	comp_flag,true				; flag to tell us that we have a computer which supports BIOS kb_flag_1/3

FC_136:
	cmp	_debug, false
	je	debug_25
        print   mesg21
debug_25:

	jmp	FC_end

FC_140:
	cmp	al,0ffh					; DO WE HAVE AN IBM PC OR Original PC/XT ???
	jne	FC_175


	cmp	_debug, false
	je	debug_26
        print   mesg21
debug_26:

	mov	_comp_id,1				; 

	mov	ax,RAMBIOS				; Load segment location into
	mov	es,ax					; ES register also
	mov	al,es:[kb_flag_3]			; get 40:96h contents
	test	al,010h					; mask off bit 4, 101/102 kybd id bit
	jz	FC_145					; if ZERO, bit is clear and we do not have 101 kybd

	mov	comp_flag,true				; flag to tell us that we have a computer which supports BIOS kb_flag_1/3

FC_145:
	jmp	FC_end
;--------------------------------------------------------
; Computer not ID'ed above
;--------------------------------------------------------
FC_175:
        print   mesg22
        print   mesg22A
        print   mesg22B

	mov	fcomputer_not_found,true		; flag back to menu that computer OPTIONS need to be displayed now
	mov	_comp_id,1				; default to a PC if not ID'able
        mov     fcomp_dialog_id, 1
	mov	comp_flag,false				; reset flag incase user chose or chooses again
	mov	_finject_keys,false
	jmp	FC_end

FC_200:
; The operator chose a computer from the menu, and we should setup our variables here
; vector will get set by the computer regardless of the operator setting

	mov	fcomp_dialog,false			; reset flag incase user chose or chooses again
	mov	fcomputer_not_found,false		; reset this flag since user choose a computer
	mov	al,fcomp_dialog_id			; get user choice
	cmp	al,1					; did user choose clone PC/PCXT/PCAT w/84 key keyboard
	jne	FC_210
	mov	_comp_id,1
	jmp	short FC_end

FC_210:
; user must have choose 2 or 3, 2=clone PC/AT w/101 key keyboard, 3=clone PC/386 w/101 key keyboard 	

	mov	_comp_id,5

	mov	ax,RAMBIOS				; Load segment location into
	mov	es,ax					; ES register also
	mov	al,es:[kb_flag_3]			; get 40:96h contents
	test	al,010h					; mask off bit 4, 101/102 kybd id bit
	jz	FC_end					; if ZERO, bit is clear and we do not have 101 kybd
	mov	comp_flag,true				; flag to tell us that we have a computer which supports BIOS kb_flag_1/3

FC_end:
;*****************************************************************************************************************
; quick check of video
;*****************************************************************************************************************
; Video type is a mess to sort out.  What I will do is check the PS/2 sytems first, and if they are not supported, I will
; make a different int 10h function call and try to decipher what other video combinations I have.
; We will determine eqVideoType as follows
;
;	1=monocrome display adapter
;	2= open
;	3= CGA
;	4= EGA mono
;	5= EGA color
;	6= MCGA	(multi color graphics adapter ) special PS/2, usually 25/30-8086
;	7= VGA

; For PS/2, we can call int 10h with function 1Ah in ah, if it comes back 1Ah, we know it was a PS/2 and that
; the function is supported.

	mov	al,0h					; read dispaly code
	mov	ah,1ah					; PS/2 read display code function
	int	10h

	cmp	al,1Ah					; was function call supported ?
	je	FC_end_ps2
	jmp	FC_end_12				; if not, not a PS/2 so try another int 10h function call

FC_end_ps2:
	cmp	bl,01h
	jne	FC_end_2
	mov	fvideo_type,1				; was monochrome (5151)


	cmp	_debug, false
	je	debug_27
        print   mesg52
debug_27:


	jmp	FC_end_50

FC_end_2:

	cmp	bl,02h
	jne	FC_end_4
	mov	fvideo_type,3				; was CGA

	cmp	_debug, false
	je	debug_28
        print   mesg49
debug_28:

	jmp	FC_end_50

FC_end_4:

	cmp	bl,04h
	jne	FC_end_6
	mov	fvideo_type,5				; was EGA color

	cmp	_debug, false
	je	debug_29
        print   mesg50
debug_29:

	jmp	FC_end_50

FC_end_6:

	cmp	bl,05h
	jne	FC_end_8
	mov	fvideo_type,4				; was EGA mono

	cmp	_debug, false
	je	debug_30
        print   mesg50
debug_30:

	jmp	FC_end_50


FC_end_8:

	cmp	bl,06h
	jne	FC_end_10

	mov	fvideo_type,6				; was MCGA """GUESSING"""

	cmp	_debug, false
	je	debug_31
        print   mesg53
debug_31:

	jmp	FC_end_50

FC_end_10:
; above bl=6, is unused or VGA

	mov	fvideo_type,7				; was VGA

	cmp	_debug, false
	je	debug_32
        print   mesg51
debug_32:

	jmp	FC_end_50


; non PS/2 jump to here to be determined ???
FC_end_12:
	
	mov	ah,12h					; next try int 10h function "ah"=12h
	mov	bl,10h					; and ask for EGA info if it exists
	int	10h

	cmp	bl,10h					; if upon return bl does not = 10h, then EGA is present
	je	FC_end_18
	cmp	bh,0
	jne	FC_end_15
	mov	fvideo_type,5				; was EGA color 
	cmp	_debug, false
	je	debug_34
        print   mesg50
debug_34:
	jmp	short FC_end_50

FC_end_15:
	mov	fvideo_type,4				; was EGA mono
	cmp	_debug, false
	je	debug_34A
        print   mesg50
debug_34A:
	jmp	short FC_end_50

FC_end_18:

; didn't find VGA or EGA, could still be CGA or MDA ?  Need to check presence 
; of video buffers, 0B8000h or 0B0000h to determine which one....
; Since I am currently unsure as to just what this test is, I think I will query
; using int 11h, and see if mono is set
;
;	int	11h					; get equioment list
;	test	al,00110000b				; check bits 5,4 
;	jz	FC_end_20
;	mov	fvideo_type,1				; was MDA
;	cmp	_debug, false
;	je	debug_40
;        print   mesg52
;debug_40:
;	jmp	short FC_end_50
;
;FC_end_20:
;
; the video mode with function "0fh', int 10h, and if it is in mode 7, I will 
; say MDA, otherwise the monitor is or can support CGA

	mov	ah,0fh
	int	10h

	cmp	al,7
	jne	FC_end_22

	mov	fvideo_type,1				; was MDA
	cmp	_debug, false
	je	debug_42
        print   mesg52
debug_42:
	jmp	short FC_end_50


FC_end_22:
; give up, if PS/2 it was found above, if EGA, it was found above, if MDA, one of the above
; tests should have found it, if not any of the above, make a CGA and quit

	mov	fvideo_type,3				; was CGA,or operating at CGA resolution

	cmp	_debug, false
	je	debug_33
        print   mesg49
debug_33:


FC_end_50:
        pop     es                                      ; restore registers
        pop     ds

        pop     dx 
        pop     cx
        pop     bx
        pop     ax
        pop     di
        pop     si

	ret
_FindComputer	endp

_BSS		ends

                end
