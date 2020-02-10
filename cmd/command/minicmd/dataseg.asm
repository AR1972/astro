; ************************************************************************
;
; This file contains the data segment used by MINICMD.COM. This has both
; the initialized and un-initialized data. Note that all the INITIALIZED
; DATA is placed at the very beginning of this segment and the un-initialized
; stuff follows it. This is because when we put it in ROM, we have the
; code put in first (CSEG) followed by the data (DSEG). We need to only
; store in ROM the code and initialized data. Thus if the un-initialized
; data follows these, there is no problem in throwing it out.
;
;
; Note that this segment also contains some CODE!!! This is because
; we don't have access to our data segment (DSEG) in a few cases and
; by putting code in the data segment, when we get control here, we
; know that our CS can be used as the DSEG! After getting the value
; of DSEG, we pass control to our code in CSEG!
;
; The two cases when this happens is in the Critical Error Handler and
; in EXEC. Obviously, in the former case, when we get control we can't
; assume anything about ES, DS, etc. In the latter case, it was noticed
; that DS, ES, SS, etc were getting trashed after the EXEC call
; int 21h, fn 4Bh in certain cases (like if a critical error happened
; during EXEC, etc).
;
; ************************************************************************
;
;			Revision History
;			-----------------
;
; M003	SHK	moved strings out to dataseg.inc to help international.
;
; M005	SHK	Added an STI instruction at the	beginning of Crit. Err. 
;		Handler.
;
;

DSEG GROUP INITIALIZED_DATASEG, UN_INITIALIZED_DATASEG

INITIALIZED_DATASEG SEGMENT PARA PUBLIC 'DATA'

begin_initialized_data	EQU	$

; Since we are a COM program, we are allocated all of the memory.
; One of the first things we do is re-size! When we do that
; in the RAM version, we free up everything after our code and allocate
; a new block of memory for DSEG and then copy the data after the CODE into
; that block. When we do this re-sizing, the first 16 bytes of the INITIALIZED
; data will be modified (ARENA header) and so we have some dummy data sitting
; there! Anyway, this is no nig deal as the RAM version is just used for
; debugging purposes! In the ROM version we don't have any such problem,
; we don't dynamically allocate memory for DATA, STACK, ENV. We just take
; up whatever memory we need after the PSP before the re-sizing!
;
IFDEF	RAM_VERSION
dummy_arena_head	db	16 DUP (?)
ENDIF

; M003 moved this block (these 2 strings) a little later and put it 
;      in file dataseg.inc to help internationalization.
;
IF 0

LOAD_MSG	db	'Cannot load program'
LOAD_MSG_LEN	EQU	$ - LOAD_MSG

NO_LABEL_MSG	db	'Cannot find label'
NO_LABEL_MSG_LEN EQU	$ - NO_LABEL_MSG

ENDIF

;****************************************************************************
;*
;* ROUTINE:	CritErrStub
;*
;* FUNCTION: 	This is the entry point for DOS to pass control to when a 
;*		critical error happens (INT 24h). The main handler is 
;*		CritErrHandler in CSEG! Look at that function.
;*
;* INPUT:	The way DOS sets up the registers for INT 24!
;*		These valuse are in AX, BP:SI, DI
;*
;* OUTPUT:	Action code in AL (to signify ABORT, RETRY, IGNORE, or FAIL)
;*
;* RESISTERS:	No registers destroyed.
;****************************************************************************

CritErrorStub PROC FAR

	assume	ds:nothing

	sti			; M005 enable interrupts.

	push	ds

	push	cs
	pop	ds
	assume	ds:DSEG

	call	dword ptr [CSCritErrHandlerAddr]

	pop	ds

	assume	ds:nothing
	iret
CritErrorStub ENDP


;****************************************************************************
;*
;* ROUTINE:	DSExecHandler
;*
;* FUNCTION: 	This is the routine that does the EXEC by calling DOS.
;*
;* INPUT:    DS:DX = Null terminated program name to be launched. This
;*		routine does not search the PATH. So, you need to specify
;*		a full path name or it should be in the CWD!
;*	     ES:BX = Pointer to the environment block	
;*
;* OUTPUT:	CY set if error in launch, else clear!
;*
;* RESISTERS:	Saves DS, Sets up ES to DSEG, SS, SP restored.
;*		The rest of the registers could be destroyed by the EXEC!
;****************************************************************************

DSExecHandler PROC FAR
	assume	es:DSEG, cs:DSEG

	; DS could be destroyed by EXEC in some cases -- see below!
	push	ds

	mov	es:stack_seg, ss
	mov	es:stack_off, sp

	; int 21, EXEC call below does not seem to save and
	; restore registers!! Especially when there is a critical error
	; during EXEC etc, SS, SP are screwed up!!! harishn & mohans have
	; seen this bizarre behaviour!

	mov	bx, offset DSEG:par_blk	; ES:BX pointer to parameter block

	; folowing EXEC call screws up DX and BX on exit!!!!!
	; DOC is incorrect!!!
	;
	mov	ax, 4b00h
	int	21h

	; ES, DS, etc seem to be destroyed if crit. error happens
	; on an EXEC!
	assume	es:nothing

	cli
	mov	ss, cs:stack_seg
	mov	sp, cs:stack_off
	sti

	pop	ds

	push	cs
	pop	es
	assume	es:DSEG

   	ret
DSExecHandler ENDP


	; M003 -- moved strings to dataseg.inc
	; Include all the strings/messages used in program.
	; They are in a separate file so that international finds it easier
	; to localize it that way. If they don't see more than the
	; bare minimum they are happy
	;
	INCLUDE dataseg.inc

end_initialized_data	EQU	$

INITIALIZED_DATASEG ENDS

;;;; **** END OF INITIALIZED DATA -- to be part of ROM Image ****


;;;; **** BEG OF UN_INITIALIZED DATA ****
UN_INITIALIZED_DATASEG SEGMENT BYTE PUBLIC 'DATA'

begin_un_initialized_data	EQU	$

pgm_blk		db	?		; MAX_INP_LENGTH will be filled in
					; here at run-time.
num_readin	db	?		; actual length of input
pgm_name	db	MAX_INP_LENGTH dup (?)	; actual input placed here

par_blk		label 	WORD
env_seg		dw	?		; will be filled in at init time

		dw	?		; cmd_tail's offset/segment filled in
cmd_seg		dw	?		; at init time!

		dw	?		; fcb1's offset/segment filled in
fcb1_seg	dw	?		; at init time!

		dw	?		; fcb2's offset/segment filled in
fcb2_seg	dw	?		; at init time!

cmd_tail	db	128 dup ( ? )	; 1st byte will have count!
fcb1		db	?
		db	11 dup ( ? )
		db	25 dup ( ? )
fcb2		db	?
		db	11 dup ( ? )
		db	25 dup ( ? )

DTA_Buffer	db	43 dup ( ? )

last_pgm_exit_code	dw	?
kw_NOT_present		db	?
is_echo_on?		db	?

in_bat_file		db	?
fhandle		dw	?
fsize		dw	?

stack_seg	dw	?
stack_off	dw	?

MyPSP		dw	?
CritErrAX	dw	?


CritErrInpKeys		db	4 DUP ( ? )
NUM_CRIT_KEYS		EQU	$ - CritErrInpKeys

; Note that the storage for CritErrKeyCodes is assumed to be right after 
; CritErrInpKeys in fn CritErrHandler!

CritErrKeyCodes	db	4 DUP ( ? )

CSCritErrHandlerAddr	dd	?
DSExecHandlerAddr	dd	?

end_un_initialized_data	EQU	$

UN_INITIALIZED_DATASEG ENDS


