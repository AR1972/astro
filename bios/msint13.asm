;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1981-1991
; *                      All Rights Reserved.
; */
	page	,160
	title	MS-DOS BIOS Int 13 & int 2f handler


	include version.inc	; set build flags
	include biosseg.inc	; establish bios segment structure

	include	msequ.inc
	include	biostruc.inc
	include	bpb.inc
	include	msbds.inc

	include	msgroup.inc	; establish Bios_Data segment

multMULT		equ	4ah
multMULTGETHMAPTR	equ	1
multMULTALLOCHMA	equ	2

;SR;
; Include file for WIN386 support
;
	include win386.inc


;	Most of the disk routines keep es:[di] set up pointing to the
;	  currently selected bds.  This is often assumed to be the standard
;	  operating environment and in some cases may not be mentioned in
;	  the subroutine headers where it will be assumed.

	extrn	call_orig13:far		; M030
	extrn	prevoper:word
	extrn	secondary_model_byte:byte
	extrn	orig13:dword
	extrn	start_bds:dword
	extrn	fhave96:byte
	extrn	ec35_flag:byte
	extrn	model_byte:byte
	extrn	number_of_sec:byte
	extrn	disksector:byte
	extrn	old13:dword
	extrn	media_set_for_format:byte

	extrn	i2f_dskentry:far

	extrn	ptrsav:dword
	extrn	SysinitPresent:byte
	extrn	FreeHMAPtr:word
	extrn	MoveDOSIntoHMA:dword

;SR; 
;New variables for Win386 support
;
	extrn	IsWin386:byte
	extrn	Win386_SI:byte
	extrn	SI_Next:dword


; close data, open Bios_code segment

	tocode

	extrn	Bios_Data_Word:word
	extrn	set_changed_dl:near
	extrn	install_bds:near

;	int 2f function 13h allows the user to change the orig13 int_13 vector
;	after booting.	this allows testing and implementation of custom int_13
;	handlers, without giving up ms-dos error recovery
;	entry:	ds:dx	== addr. of new int_13 handler
;		es:bx	== addr. of new int_13 vector used by warm boot (int19)
;	exit:	orig13	== address of new int_13 handler
;		ds:dx	== old orig13 value
;		es:bx	== old old13  value
;
; int 2f handler for external block drivers to communicate with the internal
; block driver in msdisk. the multiplex number chosen is 8. the handler
; sets up the pointer to the request packet in [ptrsav] and then jumps to
; dsk_entry, the entry point for all disk requests.
;
; on exit from this driver, we will return to the external driver
; that issued this int 2f, and can then remove the flags from the stack.
; this scheme allows us to have a small external device driver, and makes
; the maintainance of the various drivers (driver and msbio) much easier,
; since we only need to make changes in one place (most of the time).
;
;   ax=800h - check for installed handler - reserved
;   ax=801h - install the bds into the linked list
;   ax=802h - dos request
;   ax=803h - return bds table starting pointer in ds:di
;	   (ems device driver hooks int 13h to handle 16kb dma overrun
;	    problem.  bds table is going to be used to get head/sector
;	    informations without calling generic ioctl get device parm call.)

	public	i2f_handler
i2f_handler proc far
	assume	ds:nothing,es:nothing

	cmp	ah,13h
	jz	int2f_replace_int13
	cmp	ah,8
	jz	mine

;
;Check for WIN386 startup and return the BIOS instance data
;
	cmp	ah,MULTWIN386
	jz	win386call

	cmp	ah, multMULT
	jne	@f
	jmp	handle_multmult
@@:
	iret

int2f_replace_int13:
	push	ax			; free up a register for caller's ds
	mov	ax,ds			;  then we can use ds: -> Bios_Data
	mov	ds,Bios_Data_Word
	assume	ds:Bios_Data

	push	word ptr [orig13]	;   save old value of old13 and
	push	word ptr [orig13 + 2]	;   orig13 so that we can
	push	word ptr [old13]	;   return them to caller
	push	word ptr [old13 + 2]

	mov	word ptr [orig13],dx	;   orig13 := addr. of new int_13
	mov	word ptr [orig13+2],ax
	mov	word ptr [old13],bx	;    old13 := addr. of new boot_13
	mov	word ptr [old13+2],es

	pop	es			;    es:bx := old old13 vector
	pop	bx
	pop	ds			;    ds:dx := old orig13 vector
	assume	ds:nothing
	pop	dx
	pop	ax			; restore caller's ax

i2f_iret:
	iret


mine:
	assume	ds:nothing
	cmp	al,0f8h 		; iret on reserved functions
	jae	i2f_iret
	or	al,al			; a get installed state request?
	jnz	disp_func
	mov	al,0ffh
	jmp	i2f_iret

disp_func:
	cmp	al,1			; request for installing bds?
	jz	do_subfun_01
	cmp	al,3			; get bds vector?
	jz	do_get_bds_vector

; set up pointer to request packet

	push	ds
	mov	ds,Bios_Data_Word	; ds: -> Bios_Data
	assume	ds:Bios_Data

	mov	word ptr [ptrsav],bx	;otherwise dos function.
	mov	word ptr [ptrsav+2],es
	pop	ds
	assume	ds:nothing
	jmp	i2f_dskentry		; NOTE:  jump to a FAR function, not an
;					;  IRET type function.  Callers of
;					;  this int2f subfunction will have
;					;  to be careful to do a popf

do_subfun_01:
	assume	ds:nothing
	push	es			; save caller's ds, es
	push	ds

	push	ds			; put his argument into es
	pop	es

	mov	ds,Bios_Data_Word	; point ds: -> Bios_Data
	assume	ds:Bios_Data

	call	install_bds

	pop	ds			; restore caller's registers
	assume	ds:nothing
	pop	es
	jmp	i2f_iret


do_get_bds_vector:			; al=3
	assume	ds:nothing
	mov	ds,Bios_Data_Word
	assume	ds:Bios_Data

	lds	di,[start_bds]
	assume	ds:nothing
ii2f_iret:
	jmp	i2f_iret

;
;WIN386 startup stuff is done here. If starting up we set our WIN386 present
;flag and return instance data. If exiting, we reset the WIN386 present flag
;NOTE: We assume that the BIOS int 2fh is at the bottom of the chain.

win386call:
	push	ds
	mov	ds,cs:Bios_Data_Word
	assume	ds:Bios_Data

	cmp	al, Win386_Init		; is it win386 initializing?
	je	Win386Init
	cmp	al, Win386_Exit		; is it win386 exiting?
	jne	win_iret		; if not, continue int2f chain

Win386Exit:
	test	dx, 1			; is it win386 or win286 dos extender?
	jnz	win_iret		; if not win386, then continue
	and	[IsWin386], 0		; indicate that win386 is not present
	jmp	short win_iret

Win386Init:
	test	dx, 1			; is it win386 or win286 dos extender?
	jnz	win_iret		; if not win386, then continue

	or	[IsWin386], 1		; Indicate WIN386 present
	mov	word ptr [SI_Next], bx	; Hook our structure into chain
	mov	word ptr [SI_Next + 2], es
	mov	bx, offset Win386_SI	; point ES:BX to Win386_SI
	push	ds
	pop	es

win_iret:
	pop	ds
	assume 	ds:nothing
	jmp	short i2f_iret		;return back up the chain

handle_multmult:
	cmp	al, multMULTGETHMAPTR
	jne	try_2

	push	ds
	call	HMAPtr			; get offset of free HMA
	mov	bx, 0ffffh
	mov	es, bx			; seg of HMA
	mov	bx, di
	not	bx
	or	bx, bx
	jz	@f
	inc	bx
@@:
	pop	ds
	jmp	ii2f_iret
try_2:
	cmp	al, multMULTALLOCHMA
	jne	try_3

	push	ds
	mov	di, 0ffffh		; assume not enough space
	mov	es, di
	call	HMAPtr			; get offset of free HMA
	assume	ds:Bios_Data
	cmp	di, 0ffffh
	je	InsuffHMA		
	neg	di			; free space in HMA
	cmp	bx, di
	jbe	@f
	mov	di, 0ffffh
	jmp	short InsuffHMA
@@:
	mov	di, FreeHMAPtr
	add	bx, 15
	and	bx, 0fff0h
	add	FreeHMAPtr, bx		; update the free pointer
	jnz	InsuffHMA
	mov	FreeHMAPtr, 0ffffh	; no more HMA if we have wrapped
InsuffHMA:
	pop	ds
	assume	ds:nothing
	jmp	ii2f_iret
try_3:
	jmp	ii2f_iret
i2f_handler endp

;
;--------------------------------------------------------------------------
;
; procedure : HMAPtr
;
;		Gets the offset of the free HMA area ( with respect to
;							seg ffff )
;		If DOS has not moved high, tries to move DOS high.
;		In the course of doing this, it will allocate all the HMA
;		and set the FreeHMAPtr to past the end of the BIOS and 
;		DOS code.  The call to MoveDOSIntoHMA (which is a pointer)
;		enters the routine in sysinit1 called FTryToMoveDOSHi.
;
;	RETURNS : offset of free HMA in DI
;		  BIOS_DATA, seg in DS
;
;--------------------------------------------------------------------------
;
HMAPtr	proc	near
	mov	ds, Bios_Data_Word
	assume	ds:Bios_Data
	mov	di, FreeHMAPtr
	cmp	di, 0ffffh
	jne	@f
	cmp	SysinitPresent, 0
	je	@f
	call	MoveDOSIntoHMA
	mov	di, FreeHMAPtr
@@:
	ret
HMAPtr	endp


;	move a 512 byte sector from ds:si to es:di, do not trash cx
;	  but go ahead and update direction flag, si, & di

move_sector proc near
	assume	ds:nothing,es:nothing

;	M014 -- begin changes
;
;	The 80386 microprocessor considers an access to WORD 0FFFFH in
;	  any segment to be a fault.  Theoretically, this could be handled
;	  by the fault handler and the behavior of an 8086 could be emulated
;	  by wrapping the high byte to offset 0000h.  This would be a lot
;	  of work and was, indeed, blown off by the Win386 guys.  COMPAQ
;	  also handles the fault incorrectly in their ROM BIOS for real
;	  mode.  Their fault handler was only designed to deal with one
;	  special case which occurred in a magazine benchmark, but didn't
;	  handle the general case worth beans.
;
;	Simply changing this code to do a byte loop would work okay but
;	  would involve a general case performance hit.  Therefore, we'll
;	  check for either source or destination offsets being within one
;	  sector of the end of their segments and only in that case fall
;	  back to a byte move.

	cld
	push	cx
	mov	cx,512/2		; number of words in a sector

	cmp	si,0fe00h
	ja	movsec_bytes
	cmp	di,0fe00h
	ja	movsec_bytes

	rep	movsw
	pop	cx
	ret

movsec_bytes:
	shl	cx,1			; get number of bytes
	rep	movsb
	pop	cx
	ret

;	M014 -- end changes

move_sector endp



; check_wrap is a routine that adjusts the starting sector, starting head
; and starting cylinder for an int 13 request that requests i/o of a lot
; of sectors. it only does this for fixed disks. it is used in the sections
; of code that handle ecc errors and dma errors. it is necessary, because
; ordinarily the rom would take care of wraps around heads and cylinders,
; but we break down a request when we get an ecc or dma error into several
; i/o of one or more sectors. in this case, we may already be beyond the
; number of sectors on a track on the medium, and the request would fail.
;
; input conditions:
;	all registers set up for an int 13 request.
;
; output:
;	dh - contains starting head number for request
;	cx - contains starting sector and cylinder numbers
;	(the above may or may not have been changed, and are 0-based)
;	all other registers preserved.

check_wrap proc	near
	assume	ds:Bios_Data,es:nothing

	push	ax
	push	bx
	push	es
	push	di

	call	find_bds		; get pointer to bds for drive in dl
	jc	no_wrap 		;  finished if DOS doesn't use it

	test	es:[di].bds_flags,fnon_removable
	jz	no_wrap 		; no wrapping for removable media

	mov	bx,es:[di].BDS_BPB.BPB_SECTORSPERTRACK
	mov	ax,cx
	and	ax,3fh			; extract sector number
	cmp	ax,bx			; are we going to wrap?
	jbe	no_wrap
	div	bl			; ah=new sector #, al=# of head wraps

; we need to be careful here. if the new sector # is 0, then we are on the
; last sector on that track.

	or	ah,ah
	jnz	not_on_bound

	mov	ah,bl			; set sector=BDS_BPB.BPB_SECTORSPERTRACK if on boundary
	dec	al			; also decrement # of head wraps

not_on_bound:
	and	cl,0c0h 		; zero out sector #
	or	cl,ah			; or in new sector #
	xor	ah,ah			; ax = # of head wraps
	inc	ax
	add	al,dh			; add in starting head #
	adc	ah,0			; catch any carry
	cmp	ax,es:[di].BDS_BPB.BPB_HEADS		; are we going to wrap around a head?
	jbe	no_wrap_head		; do not lose new head number!!

	push	dx			; preserve drive number and head number
	xor	dx,dx
	mov	bx,es:[di].BDS_BPB.BPB_HEADS
	div	bx			; dx=new head #, ax=# of cylinder wraps

; careful here! if new head # is 0, then we are on the last head.

	or	dx,dx
	jnz	no_head_bound
	mov	dx,bx			; on boundary. set to BDS_BPB.BPB_HEADS

; if we had some cylinder wraps, we need to reduce them by one!!

	or	ax,ax
	jz	no_head_bound
	dec	ax			; reduce number of cylinder wraps
no_head_bound:
	mov	bh,dl			; bh has new head number
	pop	dx			; restore drive number and head number
	dec	bh			; get it 0-based
	mov	dh,bh			; set up new head number in dh
	mov	bh,cl
	and	bh,3fh			; preserve sector number
	mov	bl,6
	xchg	cl,bl
	shr	bl,cl			; get ms cylinder bits to ls end
	add	ch,al			; add in cylinder wrap
	adc	bl,ah			; add in high byte
	shl	bl,cl			; move up to ms end
	xchg	bl,cl			; restore cylinder bits into cl
	or	cl,bh			; or in sector number

no_wrap:
	clc
	pop	di
	pop	es
	pop	bx
	pop	ax
	ret

no_wrap_head:
	mov	dh,al			; do not lose new head number
	dec	dh			; get it 0-based
	jmp	short no_wrap
check_wrap	endp

;	this is a special version of the bds lookup code which is
;	  based on physical drives rather than the usual logical drives
;	  carry is set if the physical drive in dl is found, es:di -> its bds
;	  otherwise carry is clear
;
;	guaranteed to trash no registers except es:di


find_bds proc	near
	assume	ds:Bios_Data,es:nothing

	les	di,[start_bds]		; point es:di to first bds

fbds_1:
	cmp	es:[di].bds_drivenum,dl
	je	fbds_2

	les	di,es:[di].bds_link	; go to next bds
	cmp	di,-1
	jnz	fbds_1

	stc
fbds_2:
	ret

find_bds endp


doint	proc	near
	assume	ds:Bios_Data,es:nothing

	mov	dl,byte ptr [bp.olddx]		; get physical drive number
	xor	ah,ah
	or	al,al
	jz	dointdone			; if zero sectors, return ax=0

	mov	ah,byte ptr [bp.oldax+1]	; get request code
	push	[bp.oldf]
	popf					; M030
	call	call_orig13			; M030

; M030	call	orig13

	pushf
	pop	[bp.oldf]

dointdone:
	ret

doint	endp


; this is the true int 13 handler.  we parse the request to see if there is
; a dma violation.  if so, depending on the function, we:
;   read/write	break the request into three pieces and move the middle one
;	into our internal buffer.
;
;   format	copy the format table into the buffer
;   verify	point the transfer address into the buffer
;
; this is the biggest bogosity of all.	the ibm controller does not handle
; operations that cross physical 64k boundaries.  in these cases, we copy
; the offending sector into the buffer below and do the i/o from there.

int13frame  struc
oldbp	dw  ?
oldax	dw  ?
oldbx	dw  ?
oldcx	dw  ?
olddx	dw  ?
oldds	dw  ?	; now we save caller's ds, too
olddd	dd  ?
oldf	dw  ?
int13frame  ends


;   entry conditions:
;	ah = function
;	al = number of sectors
;	es:bx = dma address
;	cx = packed track and sector
;	dx = head and drive
;   output conditions:
;	no dma violation.

;	use extreme caution when working with this code.  In general,
;	  all registers are hot at all times.
;
;	question:  does this code handle cases where dma errors
;	  occur during ecc retries, and where ecc errors occur during
;	  dma breakdowns????  Hmmmmm.
;

dtype_array	dd	00400090h	; 40:90 is drive type array

;	stick some special stuff out of mainline


;	we know we're doing a format command.  if we have changeline
;	  support, then flag some special changed stuff and set changed
;	  by format bit for all logical drives using this physical drive

format_special_stuff:
	assume	ds:Bios_Data,es:nothing

	cmp	fhave96,0		; do we have changeline support?
	jz	format_special_stuff_done ; brif not

	push	bx
	mov	bx,fchanged_by_format+fchanged
	call	set_changed_dl		; indicate that media changed by format
	pop	bx
	jmp	short format_special_stuff_done

;	we know we've got ec35's on the system.  Now see if we're doing
;	  a floppy.  If so, create a mask and see if this particular
;	  drive is an ec35.  If so, set dtype_array[drive]=93h

ec35_special_stuff:
	assume	ds:Bios_Data,es:nothing
	test	dl,dl			; floppy or hard disk?
	js	ec35_special_stuff_done	; if hard drive, we're done

	push	ax			; see if this PARTICULAR drive is ec35
	push	cx
	mov	cl,dl			; turn drive number into bit map:
	mov	al,1			;   assume drive 0
	shl	al,cl			;   shift over correct number of times
	test	al,ec35_flag 		; electrically compatible 3.5 incher?
	pop	cx
	pop	ax
	jz	ec35_special_stuff_done	; done if this floppy is not an ec35

	push	bx			; free up a far pointer (es:bx)
	push	es

	les	bx,[dtype_array]
	add	bl,dl
	adc	bh,0			; find entry for this drive
	mov	byte ptr es:[bx],93h	; establish drive type as:
					;  (360k disk in 360k drive,
	pop	es			;  no double-stepping,
	pop	bx			;  250 kbs transfer rate)
	jmp	short ec35_special_stuff_done

; ps2_30 machine has some problem with ah=8h(read drive parm), int 13h.
;this function does not reset the common buses after the execution.
;to solve this problem, when we detect ah=8h, then we will save the result and
;will issue ah=1 (read status) call to reset the buses.


ps2_special_stuff:
	assume	ds:Bios_Data,es:nothing
	cmp	[prevoper],8		; read driver parm ?
	jz	ps2_30_problem
	cmp	prevoper,15h		; apparently function 15h fails, too
	jnz	ps2_special_stuff_done	; it isn't one of the problem functions

ps2_30_problem: 			; ps2_30 = ps2 model 30.
	push	ax			; preserve ax result from int13
	mov	ah,1			; read status call resets the bus
; M030	pushf				; simulate int 13h
; M030	call	orig13			; as a side effect
	call	call_orig13		; M030
	pop	ax			; restore results of initial int13
	jmp	short ps2_special_stuff_done

;	here is the actual int13 handler

	public i13z
i13z proc	far
	assume	ds:nothing,es:nothing

;	cas -- inefficient!  could push ds and load ds-> Bios_Data before
;	  vectoring up here from Bios_Data

	push	ds			; save caller's ds register first thing
	mov	ds,Bios_Data_Word	; and set up our own ds -> Bios_Data
	assume	ds:Bios_Data

; let the operation proceed.  if there is a dma violation, then we do things.

	mov	[prevoper],ax		; save request
	cmp	ah,romformat
	jz	format_special_stuff	; go do special stuff for format
format_special_stuff_done:

	cmp	ec35_flag,0		; any electrically compat 3.5 inchers?
	jnz	ec35_special_stuff	; go handle it out of line if so
ec35_special_stuff_done:

; M030	pushf				; simulate int 13h
; M030	call	orig13

	call	call_orig13		; M030

	pushf				; save result flags
	cmp	[model_byte],mdl_ps2_30 ; is this a ps2/30?
	jz	ps2_special_stuff	;  exit mainline to address special
ps2_special_stuff_done:			;   ps2/30 problem if so
	popf

	jc	goterr13		; error on original orig13 call-thru?
ret_from_i13:
	pop	ds
	ret	2			; restore ds & iret w/flags

;	most of our code exits through here.  If carry isn't set, then
;	  just do a simple exit.  Else doublecheck that we aren't getting
;	  a changeline error.

i13ret_ck_chglinerr:
	jnc	ret_from_i13		; done if not an error termination

i13_ret_error:
	cmp	ah,06h			; did i see a change event?
	jnz	int13b			; skip if wrong error

	or	dl,dl			; is this for the hard disk?
	js	int13b			; yes, ignore

	cmp	fhave96,0
	jz	int13b			; just in case ROM returned this
					;  error even though it told us it
					;  never would
	push	bx
	mov	bx,fchanged
	call	set_changed_dl
	pop	bx
int13b:
	stc				; now return the error
	jmp	ret_from_i13

; some kind of error occurred.	see if it is dma violation

goterr13:
	cmp	ah,09h			; dma error?
	jz	gotdmaerr		; M046
goterr13_xxxx:				; M046
	cmp	ah,11h			; ecc error?
	jnz	i13_ret_error		; other error.	just return back.

;M046 chk_validmedia_err13:

;M008 start --- removed check for bds for this physical drive.  Sometimes
;		(like when we're FDISK'ing a virgin partition and the buffer
;		is on a DMA boundary), we can just go ahead without having
;		a BDS for the drive, since we'll always read one sector at
;		a time in that case anyway.  Fixing bug 335.
;
;					;if find_bds fails, then just
;	push	es			;    return back to int 13h caller,
;	push	di			;      without performing ecc, dma
;	call	find_bds		;	error handling.
;	pop	di
;	pop	es
;	jc	i13_ret_error
;
;M008 end
;
; test of bit pattern 08h let other errors be passed as dma errors
;
;M046	cmp	ah,09h			; dma error code
;M046	jz	gotdmaerr
;
;	M046 -- the following comments no longer apply.  ECC correction
;	        is performed on all machines.
;
; soft ecc bug is only applied to pc1 and pc-xt. so, we will enforce
;this ecc error handler for them.   also, since cmc hardfiles in pc at also
;generate a lot of unnecessary ecc errors, we will still cover pc ats as
;it is done in the earlier version of msbio.
;during format/verify operation, we are going to consider any soft ecc as a
;hard error.
;
;	see if the machine we are operating on is a pc, xt or at by checking
;	the model byte.  the soft ecc bug is only on these machines and if
;	the machine we are operating on is not one of these three then we
;	needn't do anything special.  if we are operating one these however
;	we check to see if the error occured during format by checking
;	media_set_for_format.  if it did occur during format we cannot do
;	anything but if not during format then check to see if the error
;	returned in ah is the soft_ecc error and if so go to ok11 since
;	the error can be ignored.

	cmp	[media_set_for_format],1 ; formatting?
	je	i13_ret_error

;M046	cmp	[model_byte],0feh	; pc or xt?
;M046	jae	go_chk_ecc
;M046	cmp	[model_byte],0fbh	; xt?
;M046	je	go_chk_ecc
;M046	cmp	[model_byte],0fch
;M046	jne	i13_ret_error
;M046	cmp	[secondary_model_byte],2 ; at?
;M046	ja	i13_ret_error		; return to the caller
;
; we have an error status 11h.	this indicates an ecc-corrected error.	note
; that this indicates that the data is probably correct but not certainly
; correct. the roms on pc-1s and pc_xts have a 'bug' in that if an ecc error
; occurs for a multi-sector read, only the sectors up to the one where the
; error occurred are read in. we have no way of knowing how many were read in
; this case, so we redo the operation, reading one sector at a time. if we
; get an ecc error on reading one sector, we ignore the error because the
; sector has been read in.


;	M046 -- begin additions

	cmp	byte ptr [prevoper].1,romread	; ECC correction only applies to reads
	jnz	i13_ret_error

;	M046 -- end additions

;M046 go_chk_ecc:
	xor	ah,ah
; M030	pushf
; M030	call	orig13		;reset. don't care about the result
	call	call_orig13	; M030 reset.  don't care about the result

	mov	ax,[prevoper]
	xor	ah,ah		; return code = no error
	cmp	al,1			; if request for one sector, assume ok
	jz	ret_from_i13		; return with carry clear

	push	bx
	push	cx
	push	dx
	mov	[number_of_sec],al

loop_ecc:
;M046	mov	ah,byte ptr [prevoper].1
;M046	mov	al,1			; request for one sector only
	mov	ax,201h			; M046 read one sector

; we do reads one sector at a time. this ensures that we will eventually
; finish the request since ecc errors on one sector do read in that sector.
;
; we need to put in some "intelligence" into the ecc handler to handle reads
; that attempt to read more sectors than are available on a particular
; track.
;
; we call check_wrap to set up the sector #, head # and cylinder # for
; this request.
;
; at this point, all registers are set up for the call to orig13, except
; that there may be a starting sector number that is bigger than the number
; of sectors on a track.
;
	call	check_wrap		; get correct parameters for int 13
; M030	pushf				; simulate int instruction
; M030	call	orig13
	call	call_orig13		; M030
	jnc	ok11_op

	cmp	ah,9			; M046  DMA error during ECC read?
	jz	handle_dma_during_ecc	; M046

	cmp	ah,11h			; only allow ecc errors
	jnz	ok11_exit_err		; other error?

	mov	ah,0			; ecc error. reset the system again.
; M030	pushf				; simulate int 13h
; M030	call	orig13
;	call	call_orig13		; M030 simulate int 13h
	xor	ax,ax			; clear the error code so that if this
					; was the last sector, no error code 
					; will be returned for the corrected 
					; read. (clear carry too.)
ok11_op:
	dec	[number_of_sec]
	jz	ok11_exit		; all done?
	inc	cl			; advance sector number
	inc	bh			; add 200h to address
	inc	bh
	jmp	loop_ecc

;	locate error returns centrally


ok11_exit_err:
	stc				; set carry bit again.
ok11_exit:
	pop	dx
	pop	cx
	pop	bx
	jmp	i13ret_ck_chglinerr

;	M046 begin additions

handle_dma_during_ecc:

;	do the single sector read again, this time into our temporary
;	   buffer, which is guaranteed not to have a DMA error, then
;	   move the data to its proper location and proceed

	push	es
	push	bx
	mov	bx,offset disksector
	push	ds
	pop	es			; point es:bx to buffer
	mov	ax,201h			; read one sector
	call	call_orig13
	pop	bx
	pop	es
	jnc	handle_dma_during_ecc_noerr
	cmp	ah,11h
	jnz	ok11_exit_err		; if anything but ecc error, bomb out

;	now we're kosher.  Copy the data to where it belongs and resume
;	  the ECC looping code.

handle_dma_during_ecc_noerr:
	push	si
	push	di
	mov	di,bx
	mov	si,offset disksector
	call	move_sector
	pop	di
	pop	si
	jmp	ok11_op

;	M046 end additions

; we truly have a dma violation.  restore register ax and retry the
; operation as best we can.

gotdmaerr:
	mov	ax,[prevoper]
	sti
	cmp	ah,romread		; save user flags
	jb	i13_done_dmaerr		; just pass dma error thru for
					;   functions we don't handle

	cmp	ah,romverify
	jz	intverify

	cmp	ah,romformat
	jz	intformat
	ja	i13_done_dmaerr

; we are doing a read/write call.  check for dma problems

;	******** set up stack frame here!!! ********

	push	dx
	push	cx
	push	bx
	push	ax
	push	bp
	mov	bp,sp
	mov	dx,es			; check for 64k boundary error

	shl	dx,1
	shl	dx,1
	shl	dx,1
	shl	dx,1			; segment converted to absolute address

	add	dx,bx			; combine with offset
	add	dx,511			; simulate a transfer

; if carry is set, then we are within 512 bytes of the end of the segment.
; we skip the first transfer and perform the remaining buffering and transfer

	jnc	no_skip_first
	jmp	bufferx			; restore dh=head & do buffer

;	M046 -- also moved a few things around to make relative branches
;	   reach where possible.

; dx is the physical 16 bits of start of transfer (+511).  compute remaining
; sectors in segment.

no_skip_first:
	shr	dh,1			; dh = number of sectors before address
	mov	ah,128			; ah = max number of sectors in segment
	sub	ah,dh

; ah is now the number of sectors that we can successfully write in this
; segment.  if this number is above or equal to the requested number, then we
; continue the operation as normal.  otherwise, we break it into pieces.
;
;	wait a sec.  this is goofy.  the whole reason we got here in the
;	  first place is because we got a dma error.  so it's impossible
;	  for the whole block to fit, unless the dma error was returned
;	  in error.

	cmp	ah,al			; can we fit it in?
	jb	doblock 		; no, perform blocking.

;	yes, the request fits.  let it happen.

	mov	dh,byte ptr [bp.olddx+1]	; set up head number
	call	doint
	jmp	bad13			; and return from this place

i13_done_dmaerr:
	mov	ah,9			; pass dma error thru to caller
	stc
	jmp	ret_from_i13		; return with error, we know it's not
					;  a changeline error

; verify the given sectors.  place the buffer pointer into our space.

intverify:
	push	es			; save caller's dma address
	push	bx

	push	ds			; es:bx -> Bios_Data:disksector
	pop	es
dosimple:
	mov	bx,offset disksector	; do the i/o from Bios_Data:disksector
; M030	pushf				;  cas -- it doesn't matter anyway?
; M030	call	orig13

	call	call_orig13		; M030
	pop	bx
	pop	es

	jmp	i13ret_ck_chglinerr

; format operation.  copy the parameter table into Bios_Data:disksector

intformat:
	push	es
	push	bx

	push	si
	push	di
	push	ds

;	point ds to the caller's dma buffer, es to Bios_Data
;	  in other words, swap (ds, es)

	push	es
	push	ds
	pop	es
	pop	ds
	assume	ds:nothing
	mov	si,bx
	mov	di,offset disksector

	call	move_sector		; user's data into Bios_Data:disksector

	pop	ds
	assume	ds:Bios_Data
	pop	di
	pop	si			; do the i/o from
	jmp	dosimple		;   Bios_Data:disksector



; we can't fit the request into the entire block.  perform the operation on
; the first block.
;
; doblock is modified to correctly handle multi-sector disk i/o.
; old doblock had added the number of sectors i/oed (ah in old doblock) after
; the doint call to cl.  observing only the lower 6 bits of cl(=max. 64) can
; represent a starting sector, if ah was big, then cl would be clobbered.
; by the way, we still are going to use cl for this purpose since checkwrap
; routine will use it as an input.  to prevent cl from being clobbered, a
; safe number of sectors should be calculated like "63 - # of sectors/track".
; doblock will handle the first block of requested sectors within the
; boundary of this safe value.

doblock:

;try to get the # of sectors/track from bds via rom drive number.
;for any mini disks installed, here we have to pray that they have the
;same # of sector/track as the main dos partition disk drive.
;
	mov	dx,word ptr [bp.olddx]	;get head #, drive #

	push	cx			; prepare a temporary register

	push	es			; save user's transfer segment
	push	di			;ah - # of sectors before dma boundary
					;al - requested # of sectors for i/o.
	call	find_bds		; get bds pointer for this disk.
	mov	cx,es:[di].BDS_BPB.BPB_SECTORSPERTRACK		; fetch up # sectors/trk
	test	es:[di].bds_flags,fnon_removable ;don't have to worry about floppies.
	pop	di
	pop	es			; restore original es/di

	mov	al,ah			; set al=ah for floppies
	jz	doblockflop		;they are track by track operation.

	mov	ah,63			; ah = 63-secpt (# safe sectors??)
	sub	ah,cl			;al - # of sectors before dma boundary
doblockflop:
	pop	cx

doblockcontinue:
	cmp	ah,al			;if safe_# >= #_of_sectors_to_go_before dma,
	jae	doblocklast		;then #_of_sectors_to_go as it is for doint.

	push	ax			;save ah, al
	mov	al,ah			;otherwise, set al to ah to operate.
	jmp	short doblockdoint	;doint will set ah to a proper function
					; in [bp.oldax]

doblocklast:
	mov	ah,al
	push	ax			;save ah
doblockdoint:				;let ah = al = # of sectors for this shot
	call	doint
	jc	bad13			;something happened, bye!

	pop	ax
	sub	byte ptr [bp.oldax],ah	;decrement by the successful operation
	add	cl,ah			;advance sector #. safety gauranteed.
	add	bh,ah			;advance dma address
	add	bh,ah			;twice for 512 byte sectors.
	cmp	ah,al			;check the previous value
	je	buffer			;if #_of_sectors_to_go < safe_#,
					; then we are done already.

	sub	al,ah			;otherwise, #_sector_to_go = #_of_sector_to_go - safe_#
	call	check_wrap		;get new cx, dh for the next operation.
	jmp	short doblockcontinue	;handles next sectors left.


bufferx:
	mov	dh,byte ptr [bp.olddx+1] ; set up head number
buffer:
	push	bx
	mov	ah,byte ptr [bp.oldax+1]
	cmp	ah,romwrite
	jnz	doread

; copy the offending sector into local buffer

	push	es
	push	ds
	push	si
	push	di

	push	ds			; exchange segment registers
	push	es
	pop	ds
	assume	ds:nothing
	pop	es
	assume	es:Bios_Data

	mov	di,offset disksector	; where to move
	push	di			; save it
	mov	si,bx			; source
	call	move_sector		; move sector into local buffer
	pop	bx			; new transfer address (es:bx =
					;    Bios_Data:diskbuffer)
	pop	di			; restore caller's di & si
	pop	si
	pop	ds			; restore Bios_Data
	assume	ds:Bios_Data
	mov	al,1

; see if we are wrapping around a track or head

	mov	dl,byte ptr [bp.olddx]	; get drive number
	call	check_wrap		; sets up registers if wrap-around

;   ah is function
;   al is 1 for single sector transfer
;   es:bx is local transfer addres
;   cx is track/sector number
;   dx is head/drive number
;   si,di unchanged

	call	doint
	pop	es			; restore caller's dma segment
	assume	es:nothing
	jc	bad13			; go clean up
	jmp	short dotail

; reading a sector.  do int first, then move things around

doread:
	push	es
	push	bx
	push	ds			; es = Bios_Code
	pop	es

	mov	bx,offset disksector
	mov	al,1

; see if our request will wrap a track or head boundary

	mov	dl,byte ptr [bp.olddx]	; get drive number
	call	check_wrap		; sets up registers if wrap-around

;   ah = function
;   al = 1 for single sector
;   es:bx points to local buffer
;   cx, dx are track/sector, head/drive

	call	doint

	pop	bx			; restore caller's dma address
	pop	es
	assume	es:nothing

	jc	bad13			; error => clean up

	push	si
	push	di

	mov	di,bx
	mov	si,offset disksector
	call	move_sector

	pop	di
	pop	si

; note the fact that we've done 1 more sector

dotail:
	pop	bx			; retrieve new dma area
	add	bh,2			; advance over sector
	inc	cx
	mov	al,byte ptr [bp.oldax]
	clc
	dec	al
	jz	bad13			; no more i/o

; see if we wrap around a track or head boundary with starting sector
; we already have the correct head number to pass to check_wrap

	mov	dl,byte ptr [bp.olddx]	; get drive number
	call	check_wrap		; sets up registers if wrap-around
	call	doint

; we are done.	ax has the final code; we throw away what we got before

;	M046  -- okay gang.  Now we've either terminated our DMA loop,
;		   or we've finished.  If carry is set now, our only
;		   hope for salvation is that it was a read operation
;		   and the error code is ECC error.  In that case, we'll
;		   just pop the registers and go do the old ECC thing.
;		   When the DMA error that got us here in the first
;		   place occurs, it'll handle it.

bad13:
	mov	sp,bp
	pop	bp
	pop	bx			; throw away old ax result
	pop	bx
	pop	cx
	pop	dx

	jc	xgoterr13_xxxx		; M046 go handle ECC errors
	jmp	ret_from_i13		; M046 non-error exit

xgoterr13_xxxx:
	jmp	goterr13_xxxx		; M046

handle_ecc_during_dma:
i13z endp


Bios_Code	ends
	end
