;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1981-1991
; *                      All Rights Reserved.
; */
	page	,160
	title	mschar - character and clock devices
;
;----------------------------------------------------------------------------
;
; M013 : printer driver recognizes a special return value from MODE's
;	 INT 17 handler and does not do a retry operation. This is used
;	 to break out of tight loops in case mode was installed with the
;	 'R' option. This involves change in MODE also (Tag # M001)
;
; M026 : Putting back the buggy 4.01 code in auxin. (Time out bug)
;
; M044 : Isolating NOPRINTER bits from error before cmp it with NOPRINTER
;
; M059 : Bug #5002. Treat rollover byte as a count instead of a flag, if
;			t_switch is not set.
;
;----------------------------------------------------------------------------
;

	.xlist

	include version.inc	; set build flags
	include biosseg.inc	; establish bios segment structure

	include msequ.inc
	include	devsym.inc
	include	bpb.inc
	include	ioctl.inc

break	macro
	endm

	include error.inc
	.list

	include msgroup.inc	; define Bios_Data segment


	extrn	ptrsav:dword

	extrn	fhavek09:byte
	extrn	altah:byte
	extrn	keyrd_func:byte
	extrn	keysts_func:byte

	extrn	auxnum:word
	extrn	auxbuf:byte

	extrn	wait_count:word
	extrn	printdev:byte


; daycnt is the number of days since 1-1-80.
; each time the clock is read it is necessary to check if another day has
; passed.  the rom only returns the day rollover once so if it is missed
; the time will be off by a day.

	extrn	daycnt:word
	extrn	t_switch:byte
	extrn	havecmosclock:byte
	extrn	base_century:byte
	extrn	base_year:byte
	extrn	month_tab:byte

	extrn	ttticks:dword
	extrn	bintobcd:dword		; far indirect calls to routines
	extrn	daycnttoday:dword	;  installed by msinit for cmos clock


; close Bios_Data and open Bios_Code segment

	tocode

	extrn	bc_cmderr:near
	extrn	bc_err_cnt:near

MODE_CTRLBRK	equ	0ffh		; M013

;************************************************************************
;*									*
;*	device driver dispatch tables					*
;*									*
;*	each table starts with a byte which lists the number of		*
;*	legal functions, followed by that number of words.  Each	*
;*	word represents an offset of a routine in Bios_Code which	*
;*	handles the function.  The functions are terminated with	*
;*	a near return.  If carry is reset, a 'done' code is returned	*
;*	to the caller.  If carry is set, the ah/al registers are	*
;*	returned as abnormal completion status.  Notice that ds		*
;*	is assumed to point to the Bios_Data segment throughout.	*
;*									*
;************************************************************************

	public	con_table
con_table:
	db	(((offset con_table_end) - (offset con_table) - 1)/2)
	dw	bc_exvec	; 00 init
	dw	bc_exvec	; 01
	dw	bc_exvec	; 02
	dw	bc_cmderr	; 03
	dw	con_read	; 04
	dw	con_rdnd	; 05
	dw	bc_exvec	; 06
	dw	con_flush	; 07
	dw	con_writ	; 08
	dw	con_writ	; 09
	dw	bc_exvec	; 0a
con_table_end:

	public	prn_table
prn_table	label	byte
	db	(((offset prn_table_end) - (offset prn_table) -1)/2)
	dw	bc_exvec	; 00 init
	dw	bc_exvec	; 01
	dw	bc_exvec	; 02
	dw	bc_cmderr	; 03
	dw	prn_input	; 04 indicate zero chars read
	dw	z_bus_exit	; 05 read non-destructive
	dw	bc_exvec	; 06
	dw	bc_exvec	; 07
	dw	prn_writ	; 08
	dw	prn_writ	; 09
	dw	prn_stat	; 0a
	dw	bc_exvec	; 0b
	dw	bc_exvec	; 0c
	dw	bc_exvec	; 0d
	dw	bc_exvec	; 0e
	dw	bc_exvec	; 0f
	dw	prn_tilbusy	; 10
	dw	bc_exvec	; 11
	dw	bc_exvec	; 12
	dw	prn_genioctl	; 13
	dw	bc_exvec	; 14
	dw	bc_exvec	; 15
	dw	bc_exvec	; 16
	dw	bc_exvec	; 17
	dw	bc_exvec	; 18
	dw	prn_ioctl_query	; 19
prn_table_end:



	public	aux_table
aux_table	label	byte
	db	(((offset aux_table_end) - (offset aux_table) -1)/2)

	dw	bc_exvec	; 00 - init
	dw	bc_exvec	; 01
	dw	bc_exvec	; 02
	dw	bc_cmderr	; 03
	dw	aux_read	; 04 - read
	dw	aux_rdnd	; 05 - read non-destructive
	dw	bc_exvec	; 06
	dw	aux_flsh	; 07
	dw	aux_writ	; 08
	dw	aux_writ	; 09
	dw	aux_wrst	; 0a
aux_table_end:


	public	tim_table
tim_table	label	byte
	db	(((offset tim_table_end) - (offset tim_table) -1)/2)
	dw	bc_exvec	; 00
	dw	bc_exvec	; 01
	dw	bc_exvec	; 02
	dw	bc_cmderr	; 03
	dw	tim_read	; 04
	dw	z_bus_exit	; 05
	dw	bc_exvec	; 06
	dw	bc_exvec	; 07
	dw	tim_writ	; 08
	dw	tim_writ	; 09
tim_table_end:

;************************************************************************
;*									*
;*	con_read - read cx bytes from keyboard into buffer at es:di	*
;*									*
;************************************************************************

con_read proc	near
	assume	ds:Bios_Data,es:nothing

	jcxz	con_exit

con_loop:
	call	chrin		;get char in al
	stosb			;store char at es:di
	loop	con_loop

con_exit:
	clc
	ret
con_read endp

;************************************************************************
;*									*
;*	chrin - input single char from keyboard into al			*
;*									*
;*	  we are going to issue extended keyboard function, if		*
;*	  supported.  the returning value of the extended keystroke	*
;*	  of the extended keyboard function uses 0e0h in al		*
;*	  instead of 00 as in the conventional keyboard function.	*
;*	  this creates a conflict when the user entered real		*
;*	  greek alpha charater (= 0e0h) to  distinguish the extended	*
;*	  keystroke and the greek alpha.  this case will be handled	*
;*	  in the following manner:					*
;*									*
;*	      ah = 16h							*
;*	      int 16h							*
;*	      if al == 0, then extended code (in ah)			*
;*	      else if al == 0e0h, then					*
;*	      if ah <> 0, then extended code (in ah)			*
;*		else greek_alpha character.				*
;*									*
;*	also, for compatibility reason, if an extended code is		*
;*	  detected, then we are going to change the value in al		*
;*	  from 0e0h to 00h.						*
;*									*
;************************************************************************


chrin	proc	near
	assume	ds:Bios_Data,es:nothing

	mov	ah,keyrd_func		; set by msinit. 0 or 10h
	xor	al,al
	xchg	al,altah		;get character & zero altah

	or	al,al
	jnz	keyret

	int	16h			; do rom bios keyrd function

alt10:
	or	ax,ax			;check for non-key after break
	jz	chrin

	cmp	ax,7200h		;check for ctrl-prtsc
	jnz	alt_ext_chk

	mov	al,16
	jmp	short keyret

alt_ext_chk:

;**************************************************************
;  if operation was extended function (i.e. keyrd_func != 0) then
;    if character read was 0e0h then
;      if extended byte was zero (i.e. ah == 0) then
;	 goto keyret
;      else
;	 set al to zero
;	 goto alt_save
;      endif
;    endif
;  endif

	cmp	byte ptr keyrd_func,0
	jz	not_ext
	cmp	al,0e0h
	jnz	not_ext

	or	ah,ah
	jz	keyret
ifdef	DBCS
ifdef   KOREA                           ; Keyl  1990/11/5
        cmp     ah, 0f0h                ; If hangeul code range then
        jb      EngCodeRange1           ; do not modify any value.
        cmp     ah, 0f2h
        jbe     not_ext
EngCodeRange1:
endif	; KOREA
endif	; DBCS
	xor	al,al
	jmp	short alt_save

not_ext:

	or	al,al			;special case?
	jnz	keyret

alt_save:
	mov	altah,ah		;store special key
keyret:
	ret
chrin	endp

;************************************************************************
;*									*
;*	con_rdnd - keyboard non destructive read, no wait		*
;*									*
;*	pc-convertible-type machine: if bit 10 is set by the dos	*
;*	in the status word of the request packet, and there is no	*
;*	character in the input buffer, the driver issues a system	*
;*	wait request to the rom. on return from the rom, it returns	*
;*	a 'char-not-found' to the dos.					*
;*									*
;************************************************************************

con_rdnd proc	near
	assume	ds:Bios_Data,es:nothing

	mov	al,[altah]
	or	al,al
	jnz	rdexit

	mov	ah,keysts_func		; keyboard i/o interrupt - get
	int	16h			;  keystroke status (keysts_func)
	jnz	gotchr

	cmp	fhavek09,0
	jz	z_bus_exit		; return with busy status if not k09

	les	bx,[ptrsav]
	assume	es:nothing
	test	es:[bx].status,0400h	; system wait enabled?
	jz	z_bus_exit		;  return with busy status if not

;	need to wait for ibm response to request for code
;	on how to use the system wait call.

	mov	ax,4100h		; wait on an external event
	xor	bl,bl			; M055; wait for any event
	int	15h			; call rom for system wait

z_bus_exit:
	stc
	mov	ah,3			; indicate busy status
	ret

gotchr:
	or	ax,ax
	jnz	notbrk			;check for null after break

	mov	ah,keyrd_func		; issue keyboard read function
	int	16h
	jmp	con_rdnd		;and get a real status

notbrk:
	cmp	ax,7200h		;check for ctrl-prtsc
	jnz	rd_ext_chk

	mov	al,'P' and 1fh		; return control p
	jmp	short rdexit

rd_ext_chk:
	cmp	keyrd_func,0		; extended keyboard function?
	jz	rdexit			; no. normal exit.

	cmp	al,0e0h 		; extended key value or greek alpha?
	jne	rdexit

ifdef	DBCS
ifdef   KOREA
        cmp     ah, 0f0h                ; If hangeul code range then
        jb      EngCodeRange            ; do not modify any value.
        cmp     ah, 0f2h
        jbe     rdexit                  ; Keyl 90/11/5
EngCodeRange:
endif	; KOREA
endif	; DBCS

	cmp	ah,0			; scan code exist?
	jz	rdexit			; yes. greek alpha char.
	mov	al,0			; no. extended key stroke.
					;  change it for compatibility

rdexit:
	les	bx,[ptrsav]
	assume	es:nothing
	mov	es:[bx].media,al	; *** return keyboard character here

bc_exvec:
	clc				; indicate normal termination
	ret

con_rdnd endp

;************************************************************************
;*									*
;*	con_write - console write routine				*
;*									*
;*	entry:	es:di -> buffer						*
;*		cx    =  count						*
;*									*
;************************************************************************

con_writ proc	near
	assume	ds:Bios_Data,es:nothing

	jcxz	bc_exvec

con_lp:
	mov	al,es:[di]		;get char
	inc	di
	int	chrout			;output char
	loop	con_lp			;repeat until all through

cc_ret:
	clc
	ret
con_writ	endp

;************************************************************************
;*									*
;*	con_flush - flush out keyboard queue				*
;*									*
;************************************************************************

	public	con_flush	; called from msbio2.asm for floppy swapping
con_flush proc	near
	assume	ds:Bios_Data,es:nothing


	mov	[altah],0		;clear out holding buffer

;	while (charavail()) charread();

flloop:
	mov	ah,1			; command code for check status
	int	16h			; call rom-bios keyboard routine
	jz	cc_ret			; return carry clear if none

	xor	ah,ah			; if zf is nof set, get character
	int	16h			; call rom-bios to get character
	jmp	flloop

con_flush endp

;************************************************************************
;*									*
;*	some equates for rom bios printer i/o				*
;*									*
;************************************************************************

; ibm rom status bits (i don't trust them, neither should you)
; warning!!!  the ibm rom does not return just one bit.  it returns a
; whole slew of bits, only one of which is correct.


notbusystatus	=   10000000b		; not busy
nopaperstatus	=   00100000b		; no more paper
prnselected	=   00010000b		; printer selected
ioerrstatus	=   00001000b		; some kinda error
timeoutstatus	=   00000001b		; time out.

noprinter	=   00110000b		; no printer attached

;************************************************************************
;*									*
;*	prn_input - return with no error but zero chars read		*
;*									*
;*	enter with cx = number of characters requested			*
;*									*
;************************************************************************

prn_input proc	near
	assume	ds:Bios_Data,es:nothing

	call	bc_err_cnt	; reset count to zero (sub reqpkt.count,cx)
	clc			;  but return with carry reset for no error
	ret

prn_input endp

;************************************************************************
;*									*
;*	prn_writ - write cx bytes from es:di to printer device		*
;*									*
;*	auxnum has printer number					*
;*									*
;************************************************************************

prn_writ proc	near
	assume	ds:Bios_Data,es:nothing

	jcxz	prn_done		;no chars to output

prn_loop:
	mov	bx,2			;retry count

prn_out:
	call	prnstat 		; get status
	jnz	TestPrnError		; error

	mov	al,es:[di]		; get character to print
	xor	ah,ah
	call	prnop			; print to printer
	jz	prn_con			; no error - continue

	cmp	ah, MODE_CTRLBRK	; M013
	jne	@f			; M013
	mov	al, error_I24_gen_failure ; M013
	mov	altah, 0		; M013
	jmp	short pmessg		; M013
@@:
	test	ah,timeoutstatus
	jz	prn_con			; not time out - continue

TestPrnError:
	dec	bx			;retry until count is exhausted.
	jnz	prn_out

pmessg:
	jmp	bc_err_cnt		; return with error

; next character

prn_con:
	inc	di			;point to next char and continue
	loop	prn_loop

prn_done:
	clc
	ret
prn_writ	endp

;************************************************************************
;*									*
;*	prn_stat - device driver entry to return printer status		*
;*									*
;************************************************************************

prn_stat proc	near

	call	prnstat 		;device in dx
	jnz	pmessg			; other errors were found
	test	ah,notbusystatus
	jnz	prn_done		;no error. exit
	jmp	z_bus_exit		; return busy status
prn_stat endp

;************************************************************************
;*									*
;*	prnstat - utilty function to call ROM BIOS to check		*
;*		 printer status.  Return meaningful error code		*
;*									*
;************************************************************************

prnstat proc	near
	assume	ds:Bios_Data,es:nothing

	mov	ah, 2			; set command for get status
prnstat	endp				; fall into prnop

;************************************************************************
;*									*
;*	prnop - call ROM BIOS printer function in ah			*
;*		return zero true if no error				*
;*		return zero false if error, al = error code		*
;*									*
;************************************************************************

prnop	proc	near
	assume	ds:Bios_Data,es:nothing

	mov	dx,[auxnum]		; get printer number
	int	17h			; call rom-bios printer routine

		; This check was added to see if this is a case of no
		; printer being installed. This tests checks to be sure
		; the error is noprinter (30h)

	push	ax			; M044
	and	ah, noprinter		; M044
	cmp	AH,noprinter		; Chk for no printer
	pop	ax			; M044

	jne	NextTest 
	and	AH,NOT nopaperstatus
	or	AH,ioerrstatus

; examine the status bits to see if an error occurred.	unfortunately, several
; of the bits are set so we have to pick and choose.  we must be extremely
; careful about breaking basic.

NextTest:
	test	ah,(ioerrstatus+nopaperstatus) ; i/o error?
	jz	checknotready		; no, try not ready

; at this point, we know we have an error.  the converse is not true.

	mov	al,error_I24_out_of_paper
					; first, assume out of paper
	test	ah,nopaperstatus	; out of paper set?
	jnz	ret1			; yes, error is set
	inc	al			; return al=10 (i/o error)
ret1:
	ret				; return with error

checknotready:
	mov	al,2			; assume not-ready
	test	ah,timeoutstatus	; is time-out set?
	ret				; if nz then error, else ok
prnop endp

;************************************************************************
;*									*
;*	prn_tilbusy - output until busy.  Used by print spooler.	*
;*		     this entry point should never block waiting for	*
;*		     device to come ready.				*
;*									*
;*	inputs:	cx = count, es:di -> buffer				*
;*	outputs: set the number of bytes transferred in the		*
;*		 device driver request packet				*
;*									*
;************************************************************************

prn_tilbusy proc near

	mov	si,di			; everything is set for lodsb

prn_tilbloop:
	push	cx

	push	bx
	xor	bh,bh
	mov	bl,[printdev]
	shl	bx,1
	mov	cx,wait_count[bx]	; wait count times to come ready
	pop	bx

prn_getstat:
	call	prnstat 		; get status
	jnz	prn_bperr		; error
	test	ah,10000000b		; ready yet?
	loopz	prn_getstat		; no, go for more

	pop	cx			; get original count
	jz	prn_berr		; still not ready => done

	lods	es:byte ptr [si]
	xor	ah,ah
	call	prnop
	jnz	prn_berr		; error

	loop	prn_tilbloop		; go for more

	clc				; normal no-error return
	ret				;   from device driver

prn_bperr:
	pop	cx			; restore transfer count from stack

prn_berr:
	jmp	bc_err_cnt
prn_tilbusy endp

;************************************************************************
;*									*
;*	prn_genioctl - get/set printer retry count			*
;*									*
;************************************************************************

prn_genioctl proc near
	assume	ds:Bios_Data,es:nothing

	les	di,[ptrsav]
	cmp	es:[di].majorfunction,ioc_pc
	jz	prnfunc_ok

prnfuncerr:
	jmp	bc_cmderr

prnfunc_ok:
	mov	al,es:[di].minorfunction
	les	di,es:[di].genericioctl_packet
	xor	bh,bh
	mov	bl,[printdev]		; get index into retry counts
	shl	bx,1
	mov	cx,wait_count[bx]	; pull out retry count for device

	cmp	al,get_retry_count
	jz	prngetcount

	cmp	al,set_retry_count
	jnz	prnfuncerr

	mov	cx,es:[di].rc_count
prngetcount:
	mov	wait_count[bx],cx	; place "new" retry count
	mov	es:[di].rc_count,cx	; return current retry count
	clc
	ret
prn_genioctl endp

;************************************************************************
;*									*
;*  prn_ioctl_query							*
;*									*
;*  Added for 5.00							*
;************************************************************************

prn_ioctl_query PROC NEAR
	assume	ds:Bios_Data,es:nothing

	les	di,[ptrsav]
	cmp	es:[di].majorfunction,ioc_pc
	jne	prn_query_err

	mov	al,es:[di].minorfunction
	cmp	al,get_retry_count
	je	IOCtlSupported
	cmp	al,set_retry_count
	jne	prn_query_err

IOCtlSupported:
	clc
	ret

prn_query_err:
	stc
	jmp	BC_CmdErr

prn_ioctl_query ENDP

;************************************************************************
;*									*
;*	aux port driver code -- "aux" == "com1"				*
;*									*
;*	the device driver entry/dispatch code sets up auxnum to		*
;*	give the com port number to use (0=com1, 1=com2, 2=com3...)	*
;*									*
;************************************************************************

;	values in ah, requesting function of int 14h in rom bios

auxfunc_send	 equ	1	;transmit
auxfunc_receive  equ	2	;read
auxfunc_status	 equ	3	;request status

;	error flags, reported by int 14h, reported in ah:

flag_data_ready  equ	01h	;data ready
flag_overrun	 equ	02h	;overrun error
flag_parity	 equ	04h	;parity error
flag_frame	 equ	08h	;framing error
flag_break	 equ	10h	;break detect
flag_tranhol_emp equ	20h	;transmit holding register empty
flag_timeout	 equ	80h	;timeout

;	these flags reported in al:

flag_cts	 equ	10h	;clear to send
flag_dsr	 equ	20h	;data set ready
flag_rec_sig	 equ	80h	;receive line signal detect

;************************************************************************
;*									*
;*	aux_read - read cx bytes from [auxnum] aux port to buffer	*
;*		   at es:di						*
;*									*
;************************************************************************

aux_read proc near
	assume	ds:Bios_Data,es:nothing

	jcxz	exvec2		; if no characters, get out

	call	getbx		; put address of auxbuf in bx
	xor	al,al		; clear al register
	xchg	al,[bx] 	; get character , if any, from
				;   buffer and clear buffer
	or	al,al		; if al is nonzero there was a
				;   character in the buffer
	jnz	aux2		; if so skip first auxin call

aux1:
	call	auxin		; get character from port
;		^^^^^ 		  won't return if error
aux2:
	stosb			; store character
	loop	aux1		; if more characters, go around again

exvec2:
	clc			; all done, successful exit
	ret

aux_read endp

;************************************************************************
;*									*
;*	auxin - call rom bios to read character from aux port		*
;*		if error occurs, map the error and return one		*
;*		level up to device driver exit code, setting		*
;*		the number of bytes transferred appropriately		*
;*									*
;************************************************************************

;
; M026 - BEGIN
;
auxin	proc	near
	mov	ah,auxfunc_receive
	call	auxop		;check for frame, parity, or overrun errors
	 			;warning: these error bits are unpredictable 
				; if timeout (bit 7) is set
	test	ah, flag_frame or flag_parity or flag_overrun
	jnz	arbad		; skip if any error bits set
	ret			; normal completion, ah=stat, al=char

;	error getting character

arbad:
	pop	ax		; remove return address (near call)
	xor	al,al
	or	al,flag_rec_sig or flag_dsr or flag_cts
	jmp	bc_err_cnt

auxin	endp

IFDEF	COMMENTEDOUT
auxin	proc	near
	push	cx
	mov	cx, 20		; number of retries on time out errors
@@:
	mov	ah,auxfunc_receive
	call	auxop		;check for frame, parity, or overrun errors
	 			;warning: these error bits are unpredictable 
				; if timeout (bit 7) is set
	test	ah, flag_timeout
	jz	no_timeout
	loop	@b
no_timeout:
	pop	cx
	test	ah, flag_timeout or flag_frame or flag_parity or flag_overrun
	jnz	arbad		; skip if any error bits set
	ret			; normal completion, ah=stat, al=char

;	error getting character

arbad:
	pop	ax		; remove return address (near call)
	xor	al,al
	or	al,flag_rec_sig or flag_dsr or flag_cts
	jmp	bc_err_cnt

auxin	endp
ENDIF

;
; M026 - END
;
;************************************************************************
;*									*
;*	aux_rdnd - non-destructive aux port read			*
;*									*
;************************************************************************

aux_rdnd proc	near
	assume	ds:Bios_Data,es:nothing

	call	getbx		; have bx point to auxbuf
	mov	al,[bx] 	; copy contents of buffer to al
	or	al,al		; if al is non-zero (char in buffer)
	jnz	auxrdx		;   then return character

	call	auxstat 	;   if not, get status of aux device
	test	ah,flag_data_ready ; test data ready
	jz	auxbus		;   then device is busy (not ready)

	test	al,flag_dsr	;test data set ready
	jz	auxbus		;   then device is busy (not ready)

	call	auxin		;   else aux is ready, get character
	mov	[bx],al 	; save character in buffer

auxrdx:
	jmp	rdexit		; return al in [packet.media]

auxbus:
	jmp	z_bus_exit	; return busy status
aux_rdnd endp

;************************************************************************
;*									*
;*	aux_wrst - return aux port write status				*
;*									*
;************************************************************************

aux_wrst proc	near
	assume	ds:Bios_Data,es:nothing

	call	auxstat 	; get status of aux in ax
	test	al,flag_dsr	; test data set ready
	jz	auxbus		;   then device is busy (not ready)
	test	ah,flag_tranhol_emp ;test transmit hold reg empty
	jz	auxbus		;   then device is busy (not ready)
	clc
	ret
aux_wrst endp

;************************************************************************
;*									*
;*	auxstat - call rom bios to determine aux port status		*
;*									*
;*	exit:	ax = status						*
;*		dx = [auxnum]						*
;*									*
;************************************************************************

auxstat	proc near
	mov	ah,auxfunc_status
auxstat endp			; fall into auxop

;************************************************************************
;*									*
;*	auxop - perform rom-biox aux port interrupt			*
;*									*
;*	entry:	ah = int 14h function number				*
;*	exit:	ax = results						*
;*		dx = [auxnum]						*
;*									*
;************************************************************************

auxop	proc	near
				;ah=function code
				;0=init, 1=send, 2=receive, 3=status
	mov	dx,[auxnum]	; get port number
	int	14h		; call rom-bios for status
	ret
auxop	endp

;************************************************************************
;*									*
;*	aux_flsh - flush aux input buffer - set contents of		*
;*		   auxbuf [auxnum] to zero				*
;*									*
;*	cas - shouldn't this code call the rom bios input function	*
;*	      repeatedly until it isn't ready?  to flush out any	*
;*	      pending serial input queue if there's a tsr like MODE	*
;*	      which is providing interrupt-buffering of aux port?	*
;*									*
;************************************************************************

aux_flsh proc	near
	call	getbx		; get bx to point to auxbuf
	mov	byte ptr [bx],0 ; zero out buffer
	clc			; all done, successful return
	ret
aux_flsh endp

;************************************************************************
;*									*
;*	aux_writ - write to aux device					*
;*									*
;************************************************************************

aux_writ proc	near
	assume	ds:Bios_Data 	; set by aux device driver entry routine
	jcxz	exvec2		; if cx is zero, no characters
				;   to be written, jump to exit
aux_loop:
	mov	al,es:[di]	; get character to be written
	inc	di		; move di pointer to next character
	mov	ah,auxfunc_send ;value=1, indicates a write
	call	auxop		;send character over aux port

	test	ah,flag_timeout ;check for error
	jz	awok		;   then no error
	mov	al,10		;   else indicate write fault
	jmp	bc_err_cnt 	; call error routines

				; if cx is non-zero, still more
awok:
	loop	aux_loop	; more characrter to print
	clc			; all done, successful return
	ret
aux_writ endp

;************************************************************************
;*									*
;*	getbx - return bx -> single byte input buffer for		*
;*		selected aux port ([auxnum])				*
;*									*
;************************************************************************

getbx	proc	near
	assume	ds:Bios_Data,es:nothing

	mov	bx,[auxnum]
	add	bx,offset auxbuf
	ret
getbx	endp


;----------------------------------------------------------------
;								:
;		    clock device driver 			:
;								:
;								:
;   this file contains the clock device driver. 		:
;								:
;   the routines in this files are:				:
;								:
;	routine 		function			:
;	------- 		--------			:
;	tim_writ		set the current time		:
;	tim_read		read the current time		:
;	time_to_ticks		convert time to corresponding	:
;				  number of clock ticks 	:
;								:
; the clock ticks at the rate of:				:
;								:
;	1193180/65536 ticks/second (about 18.2 ticks per second):
; see each routine for information on the use.			:
;								:
;----------------------------------------------------------------


; convert time to ticks
; input : time in cx and dx
; ticks returned in cx:dx

	public	time_to_ticks
time_to_ticks proc far

; first convert from hour,min,sec,hund. to
; total number of 100th of seconds

	mov	al,60
	mul	ch		;hours to minutes
	mov	ch,0
	add	ax,cx		;total minutes
	mov	cx,6000 	;60*100
	mov	bx,dx		;get out of the way of the multiply
	mul	cx		;convert to 1/100 sec
	mov	cx,ax
	mov	al,100
	mul	bh		;convert seconds to 1/100 sec
	add	cx,ax		;combine seconds with hours and min.
	adc	dx,0		;ripple carry
	mov	bh,0
	add	cx,bx		;combine 1/100 sec
	adc	dx,0

;	dx:cx is time in 1/100 sec

	xchg	ax,dx
	xchg	ax,cx		;now time is in cx:ax
	mov	bx,59659
	mul	bx		;multiply low half
	xchg	dx,cx
	xchg	ax,dx		;cx->ax, ax->dx, dx->cx
	mul	bx		;multiply high half
	add	ax,cx		;combine overlapping products
	adc	dx,0
	xchg	ax,dx		;ax:dx=time*59659
	mov	bx,5
	div	bl		;divide high half by 5
	mov	cl,al
	mov	ch,0
	mov	al,ah		;remainder of divide-by-5
	cbw
	xchg	ax,dx		;use it to extend low half
	div	bx		;divde low half by 5
	mov	dx,ax
				; cx:dx is now number of ticks in time
	ret
time_to_ticks endp

;--------------------------------------------------------------------
;
; tim_writ sets the current time
;
; on entry es:[di] has the current time:
;
;	number of days since 1-1-80	(word)
;	minutes (0-59)			(byte)
;	hours (0-23)			(byte)
;	hundredths of seconds (0-99)	(byte)
;	seconds (0-59)			(byte)
;
; each number has been checked for the correct range.
;

IFDEF	POWER		; Power management driver has it's own version of
			; this routine. ;M074

	extrn	tim_writ:near

ELSE

;	NOTE:  Any changes in this routine probably require corresponding
;	changes in the version that is built with the power manager driver.
;	See ptime.asm.

tim_writ proc	near
	assume	ds:Bios_Data
	mov	ax,word ptr es:[di]
	push	ax		;daycnt. we need to set this at the very
				;  end to avoid tick windows.
	cmp	havecmosclock, 0
	je	no_cmos_1
	mov	al,es:[di+3]		;get binary hours
	call	bintobcd		;convert to bcd
	mov	ch,al			;ch = bcd hours
	mov	al,es:[di+2]		;get binary minutes
	call	bintobcd		;convert to bcd
	mov	cl,al			;cl = bcd minutes
	mov	al,es:[di+5]		;get binary seconds
	call	bintobcd		;convert to bcd
	mov	dh,al			;dh = bcd seconds
	mov	dl,0			;dl = 0 (st) or 1 (dst)
	cli				;turn off timer
	mov	ah,03h			;set rtc time
	int	1ah			;call rom bios clock routine
	sti

no_cmos_1:
	mov	cx,word ptr es:[di+2]
	mov	dx,word ptr es:[di+4]
	call	ttticks		; convert time to ticks
				;cx:dx now has time in ticks
	cli			; turn off timer
	mov	ah, 1		; command is set time in clock
	int	1ah		; call rom-bios clock routines
	pop	[daycnt]
	sti

	cmp	havecmosclock, 0
	je	no_cmos_2

	call	daycnttoday	; convert to bcd format
	cli			; turn off timer
	mov	ah,05h		; set rtc date
	int	1ah		; call rom-bios clock routines
	sti

no_cmos_2:
	clc
	ret
tim_writ endp

ENDIF	; POWER

;
; gettime reads date and time
; and returns the following information:

;	es:[di]  =count of days since 1-1-80
;	es:[di+2]=hours
;	es:[di+3]=minutes
;	es:[di+4]=seconds
;	es:[di+5]=hundredths of seconds

IFDEF	POWER		; Power management driver has it's own version of
			; this routine. ;M074

	extrn	tim_read:near

ELSE

;	NOTE:  Any changes in this routine probably require corresponding
;	changes in the version that is built with the power manager driver.
;	See ptime.asm.

tim_read proc	near

	call	GetTickCnt	; M059 does INT 1A with ah=0 & updates daycnt
	mov	si,[daycnt]

; we now need to convert the time in tick to the time in 100th of
; seconds.  the relation between tick and seconds is:
;
;		 65536 seconds
;	       ----------------
;		1,193,180 tick
;
; to get to 100th of second we need to multiply by 100. the equation is:
;
;	ticks from clock  * 65536 * 100
;      ---------------------------------  = time in 100th of seconds
;		1,193,180
;
; fortunately this fromula simplifies to:
;
;	ticks from clock * 5 * 65,536
;      --------------------------------- = time in 100th of seconds
;		59,659
;
; the calculation is done by first multipling tick by 5. next we divide by
; 59,659.  in this division we multiply by 65,536 by shifting the dividend
; my 16 bits to the left.
;
; start with ticks in cx:dx
; multiply by 5

	mov	ax,cx
	mov	bx,dx
	shl	dx,1
	rcl	cx,1		;times 2
	shl	dx,1
	rcl	cx,1		;times 4
	add	dx,bx
	adc	ax,cx		;times 5
	xchg	ax,dx		
	

; now have ticks * 5 in dx:ax
; we now need to multiply by 65,536 and divide by 59659 d.

	mov	cx,59659	; get divisor
	div	cx
				; dx now has remainder
				; ax has high word of final quotient
	mov	bx,ax		; put high work if safe place
	xor	ax,ax		; this is the multiply by 65536
	div	cx		; bx:ax now has time in 100th of seconds


;rounding based on the remainder may be added here
;the result in bx:ax is time in 1/100 second.

	mov	dx,bx
	mov	cx,200		;extract 1/100's

;division by 200 is necessary to ensure no overflow--max result
;is number of seconds in a day/2 = 43200.

	div	cx
	cmp	dl,100		;remainder over 100?
	jb	noadj
	sub	dl,100		;keep 1/100's less than 100
noadj:
	cmc			;if we subtracted 100, carry is now set
	mov	bl,dl		;save 1/100's

;to compensate for dividing by 200 instead of 100, we now multiply
;by two, shifting a one in if the remainder had exceeded 100.

	rcl	ax,1
	mov	dl,0
	rcl	dx,1
	mov	cx,60		;divide out seconds
	div	cx
	mov	bh,dl		;save the seconds
	div	cl		;break into hours and minutes
	xchg	al,ah

;time is now in ax:bx (hours, minutes, seconds, 1/100 sec)

	push	ax
	mov	ax,si		; daycnt
	stosw
	pop	ax
	stosw
	mov	ax,bx
	stosw
	clc
	ret

tim_read endp

ENDIF	; POWER

; M059 - BEGIN
;
;----------------------------------------------------------------------------
;
; procedure : GetTickCnt
;
;		Returns the tick count in CX:DX. Takes care of DayCnt in case
;		of rollover [except when power management driver is in use]. 
;		Uses the following logic for updating Daycnt
;
;		if ( rollover ) {
;			if ( t_switch )
;				daycnt++ ;
;			else
;				daycnt += rollover ;
;		}
;
; USES : AX
;
; RETURNS : CX:DX - tick count
; MODIFIES : daycnt
;
;----------------------------------------------------------------------------
;
	public	GetTickCnt

GetTickCnt	proc

	xor	ah, ah
	int	1ah

IFNDEF	POWER		; Power management driver keeps track of rollover.

	cmp	t_switch, 0		; use old method ?
	jne	inc_case		; yes

;
;------ new method assumes that INT 1a returns roll over count and not flag.
;
	xor	ah, ah			; new method
	add	daycnt, ax
	ret

inc_case:
;
;------ old method assumes that INT 1a returns rollover flag
;
	or	al, al
	jz	no_rollover
	inc	daycnt
no_rollover:

ENDIF	; POWER

	ret

GetTickCnt	endp

; M059 - END

Bios_Code	ends
	end


