	page	,160
	title	syspre.asm - pre-load and final placement of dblspace.bin
;/*
; *			 Microsoft Confidential
; *			 Copyright (C) Microsoft Corporation 1981-1992
; *			 All Rights Reserved.
; */

;	History:
;
;	    09-Jul-1992 chuckst    Initial version
;	    26-Jul-1992 chuckst    substantial changes for final placement
;				   of device driver
;
;	Exported Routines:
;
;	public	MagicPreload	; loads the MagicDrv driver and initializes it
;	public	MagicPostLoad	; does final default placement of Magicdrv.bin
;	public	MagicSetCdss	; let's us disable extra DblSpace drives
;
;	Design Notes:
;
;	    This code was basically stripped out of SYSCONF.ASM, in
;	    the function multi_pass, just after the label tryd:.
;	    The original implementation was to make a special
;	    call to multi_pass with a phoney 'organized' CONFIG.SYS,
;	    but the code is not in the greatest shape, so it was
;	    not really suitable.
;
;	    In this version of the code, I've added better
;	    comments on what the subroutines are expected
;	    to accomplish, and also optimized it a bit since
;	    we don't have to deal with as many special cases
;	    as the main DD loading code.


break	macro	; dummy empty macro for include files
	endm


	.xlist
	include biosseg.inc
	include version.inc
	include sysvar.inc
	include dpb.inc
	include syscall.inc
	include devsym.inc
	include devmark.inc
	include ds_vers.inc
	include curdir.inc
	include magicdrv.inc
	.list

	ifdef	dblspace_hooks

	public	MagicPreload	; loads the MagicDrv driver and initializes it
	public	MagicPostLoad	; does final default placement of Magicdrv.bin
	public	MagicSetCdss	; let's us disable extra DblSpace drives

SYSPRE_BADFILE_ERROR	=	40h	; problem loading dblspace.bin
SYSPRE_EXEC_FAIL_ERROR	=	41h
SYSPRE_MEMORY_ERROR	=	42h
SYSPRE_NO_UNITS_ERROR	=	43h
SYSPRE_TOO_MANY_UNITS_ERROR =	44h
SYSPRE_BAD_SECTOR_SIZE_ERROR =	45h
SYSPRE_NOT_MAGIC	=	46h
SYSPRE_DRIVER_FAILED	=	47h

	extrn	NullBackdoor:near
	extrn	baddblspace:byte
	extrn	packet:byte
	extrn	MagicBackdoor:dword

	if	ibmjapver
noexec	equ	   true
	else
noexec	equ	   false
	endif


	if	not ibmjapver
	extrn	 re_init:far
	endif

	ifdef	TAIWAN
	extrn	cdosinit:near
	endif

;---------------------------------------



sysinitseg segment
	assume	cs:sysinitseg,ds:nothing,es:nothing,ss:nothing
	extrn	memhi:word
	extrn	alloclim:word


	extrn	ExecDev:near
	extrn	DevBreak:near
	extrn	tempcds:near
	extrn	round:near
	extrn	SizeDevice:near
	extrn	RoundBreakAddr:near
	extrn	DevSetBreak:near
	extrn	SetDevMark:near
	extrn	InitDevLoad:near

	extrn	unitcount:byte
	extrn	ConvLoad:byte
	extrn	DeviceHi:byte
	extrn	setdevmarkflag:byte
	extrn	drivenumber:byte
	extrn	devdrivenum:byte

	extrn	break_addr:dword
	extrn	dosinfo:dword
	extrn	DevEntry:dword
	extrn	bpb_addr:dword
	extrn	DevBrkAddr:dword
	extrn	DevCmdLine:dword

	extrn	DevSize:word
	extrn	DevLoadAddr:word
	extrn	DevLoadEnd:word
	extrn	DevSizeOption:word

	ifdef	COPYCDS
	extrn	newnum_cdss:word
	endif


MagicDDName	db	'\DBLSPACE.BIN',0

;***	MagicPreload - pre-load dblspace.bin
;
;	EXIT	ax = error code, 00 means none.
;		ZF = true if ax == 0
;

MagicPreload	proc	near

	mov	DeviceHi, 0		; not to be loaded in UMB
	mov	DevSizeOption, 0

	push	cs
	pop	ds			; BUGBUG - 7/9/92 chuckst - needed?

	mov	si,offset MagicDDName

;	MagicDrv won't actually look at the <null> command tail.

	mov	word ptr [bpb_addr],si	; pass the command line to the device
	mov	word ptr [bpb_addr+2],cs

	mov	word ptr DevCmdLine, si ; save it for error reporting
	mov	word ptr DevCmdLine+2, cs

	call	round			; normalize memhi:memlo (first free
;					;			   memory)

	push	cs
	pop	es			; get filename pointer to es:si
	call	SizeDevice		; get size of device file into DevSize
	mov	ax,SYSPRE_BADFILE_ERROR
	jc	pre_exit1

	mov	ConvLoad, 1		; Doesn't matter if DeviceHi==0

	call	InitDevLoad		; set up sub-arena, DevLoadAddr,
;					;  DevLoadEnd, and DevEntry
;					;  gets arena name from bpb_addr

;	check to make sure device driver fits our available space.

	mov	ax, DevLoadAddr
	add	ax, DevSize		; calculate seg after DD load
	jc	pre_exit_memory_err1	; choke if overflows address space
	cmp	DevLoadEnd, ax		; does it overflow available space?
	jae	loaddev 		; we're golden if not

pre_exit_memory_err1:
	mov	ax,SYSPRE_MEMORY_ERROR
pre_exit1:
	jmp	pre_exit


loaddev:
	lds	dx,DevCmdLine		; get the load file name
	assume	ds:nothing

	if	noexec
	les	bx,dword ptr cs:[memlo]
	call	ldfil			;load in the device driver

	else

	call	ExecDev 		; load device driver using exec call

	endif

	mov	ax,SYSPRE_EXEC_FAIL_ERROR
	jc	pre_exit1

	mov	word ptr break_addr, 0	; pass the limit to the DD
	mov	ax, DevLoadEnd
	mov	word ptr break_addr+2, ax


	les	bx,DevEntry		; point to the Magic DD header
	cmp	es:[bx].12h,'.,'	; is it our stamp?
	mov	ax,SYSPRE_NOT_MAGIC
	jnz	pre_exit_3		; abort if not MagicDrv!!!

;	Now save the backdoor entry.

	mov	cs:word ptr MagicBackdoor,14h
	mov	cs:word ptr MagicBackdoor+2,es

	mov	al,cs:drivenumber	; pass drive number to DBLSPACE as if
	mov	cs:devdrivenum,al	;  it is a normal block device driver

	push	cs
	pop	es			; calldev needs packet segment in es
	mov	bx,offset packet
	mov	ax,DS_INTERNAL_REVISION ; tell it what revision we expect
	call	cs:MagicBackdoor	; first time call is init entry point
;					;  with a standard device driver
;					;  init packet at es:bx
	jnc	no_driver_version_fail	; skip if not a version failure

;	In this case, we're going to display a message

	push	cs
	pop	ds
	mov	dx,offset baddblspace
	mov	ah,9
	int	21h			; display the message
	jmp	short fail_driver_load

no_driver_version_fail:
	or	ax,ax			; error code returned?????
	jz	magic_is_resident	; skip if no error

;	point backdoor call back to safe far return

fail_driver_load:
	mov	cs:word ptr MagicBackdoor,offset NullBackdoor
	mov	cs:word ptr MagicBackdoor+2,cs
	mov	ax,SYSPRE_DRIVER_FAILED ; error code
pre_exit_3:
	jmp	short pre_exit_2

;	Normalize the break_addr into ENDSEG:0 form.

magic_is_resident:
	mov	ax,word ptr break_addr
	add	ax,15
	rcr	ax,1
	shr	ax,1
	shr	ax,1
	shr	ax,1			; convert to paragraphs
	add	ax,word ptr break_addr+2 ; add to terminate segment
	mov	word ptr DevBrkAddr+2,ax
	mov	word ptr DevBrkAddr,0	; store normalized end here

;	see if we can move the bulk of the driver up out of the
;	way for now.

	mov	bx,4			; inquire how many paragraphs it wants
	call	cs:MagicBackdoor

	mov	bx,[alloclim]		; get top of free memory
	sub	bx,ax			; see how much we'll lower it
	cmp	bx,word ptr [DevBrkAddr+2] ; is there that much room free?
	jb	cant_move_driver	; if not, just leave it low

;	BUGBUG -- 29 Jul 92 -- chuckst -- If we are in a very low memory
;					situation, and there isn't enough
;					space free to copy the driver up,
;					we'll just leave it sitting where
;					it is.	The big disadvantage of
;					this is that it can't be placed
;					in a UMB, and the init-resident
;					code will stay resident.

	sub	[alloclim],ax		; allocate space!
	mov	es,[alloclim]
	mov	bx,6			; tell the driver to move itself
	call	cs:MagicBackdoor

	mov	word ptr [DevBrkAddr+2],ax ; save end of low stub

cant_move_driver:
	mov	ax,word ptr [DevBrkAddr+2] ; get terminate segment
	cmp	ax,DevLoadEnd		; terminate size TOO big?
	mov	ax,SYSPRE_MEMORY_ERROR
	ja	pre_exit2		;  error out if so

	lds	si,DevEntry		;set ds:si to header
	les	di,cs:[dosinfo] 	;es:di point to dos info

	jmp	short isblock

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; central error exit

pre_exit_memory_err:
	mov	ax,SYSPRE_MEMORY_ERROR
pre_exit_2:
	jmp	pre_exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;------ deal with block device drivers
;
isblock:
	mov	al,cs:[unitcount]	;if no units found, erase the device
	or	al,al
	jz	pre_exit_no_units_err

	mov	[si.sdevname],al	; number of units in name field
;					;  device drivers are *supposed*
;					;  to do this for themselves.

	xor	ah,ah
	mov	cx,ax
	mov	dh,ah
	mov	dl,es:[di.sysi_numio]	;get number of devices into dx

	mov	ah,dl
	add	ah,al			; check for too many devices
	cmp	ah,26			; 'A' - 'Z' is 26 devices

	ja	pre_exit_too_many_units_err

	or	cs:[setdevmarkflag],for_devmark
	call	DevSetBreak		; alloc the device
	jc	pre_exit_memory_err

	add	es:[di.sysi_numio],al	; update the amount

ifdef	COPYCDS
	xor	ah, ah
	mov	newnum_cdss, ax 	; save number of new CDSs to be built
endif ; COPYCDS

;	Note:  We won't add in the DblSpace drive count to [drivenumber],
;	which is used to inform future loadable devices of their base address.
;	Instead, we'll leave this alone, and after they load, call
;	back to DBLSPACE to shuffle the drives around.
;
;	add	cs:drivenumber,al	; remember amount for next device

	lds	bx,cs:[bpb_addr]	; point to bpb array

perunit:
	les	bp,cs:[dosinfo]
	les	bp,dword ptr es:[bp.sysi_dpb]	;get first dpb

scandpb:
	cmp	word ptr es:[bp.dpb_next_dpb],-1
	jz	foundpb
	les	bp,es:[bp.dpb_next_dpb]
	jmp	scandpb

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	Here are some centrally located error exits

pre_exit_too_many_units_err:
	mov	ax,SYSPRE_TOO_MANY_UNITS_ERROR
	jmp	short pre_exit2

pre_exit_no_units_err:
	mov	ax,SYSPRE_NO_UNITS_ERROR
pre_exit2:
	jmp	pre_exit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;	We've found the end of the DPB chain.  Now extend it.

foundpb:
	mov	ax,word ptr cs:[DevBrkAddr]
	mov	word ptr es:[bp.dpb_next_dpb],ax
	mov	ax, word ptr cs:[DevBrkAddr+2]
	mov	word ptr es:[bp.dpb_next_dpb+2],ax

	les	bp,dword ptr cs:[DevBrkAddr]
	add	word ptr cs:[DevBrkAddr],dpbsiz

	call	RoundBreakAddr

	mov	word ptr es:[bp.dpb_next_dpb],-1
	mov	es:[bp.dpb_first_access],-1

	mov	si,[bx] 			;ds:si points to bpb
	inc	bx
	inc	bx				;point to next guy
	mov	word ptr es:[bp.dpb_drive],dx
	mov	ah,setdpb			;hidden system call
	int	21h

	mov	ax,es:[bp.dpb_sector_size]
	push	es
	les	di,cs:[dosinfo] 		;es:di point to dos info
	cmp	ax,es:[di.sysi_maxsec]
	pop	es
	mov	ax,SYSPRE_BAD_SECTOR_SIZE_ERROR
	ja	pre_exit

	push	ds
	lds	ax,cs:[DevEntry]
	mov	word ptr es:[bp.dpb_driver_addr],ax
	mov	word ptr es:[bp.dpb_driver_addr+2],ds
	pop	ds

	inc	dl			; increment drive number
	inc	dh			; increment unit number
	loop	perunit

	push	cs			; restore addressability
	pop	ds

ifdef	COPYCDS
	xor	al, al			; build CDSs incrementally
endif ; COPYCDS

	call	tempcds 			; set cds for new drives

linkit:
	les	di,cs:[dosinfo] 		;es:di = dos table
	mov	cx,word ptr es:[di.sysi_dev]	;dx:cx = head of list
	mov	dx,word ptr es:[di.sysi_dev+2]

	lds	si,cs:[DevEntry]		;ds:si = device location
	mov	word ptr es:[di.sysi_dev],si	;set head of list in dos
	mov	word ptr es:[di.sysi_dev+2],ds

	mov	word ptr ds:[si],cx		;link in the driver
	mov	word ptr ds:[si+2],dx

;	For the magicdrv load case, assume only one device driver
;	per file.

	call	DevBreak		; mark successful install

	mov	cx, word ptr cs:[DevBrkAddr+2]	; pass it a work buffer
	mov	dx, cs:[alloclim]		;   address in cx (segment)
	sub	dx, cx				;   for len dx (paragraphs)

	mov	ax,5500h		; we're shuffle aware, but don't move
;					;  any drives at this point.
	mov	bx,2			; switch what we can now
	call	cs:MagicBackdoor

	xor	ax,ax			; no errors!!!!

pre_exit:
	or	ax,ax			; reset zero flag if error
	ret

MagicPreload	endp


;***	MagicPostload -- called to clean up and make sure Magic is final placed
;

MagicPostload	proc	near

	mov	ax,multMagicdrv
	mov	bx,MD_VERSION
	int	2fh
	or	ax,ax		; is it there?
	jnz	no_magic	; done if not
	test	dx,8000h	; is it already permanent?
	jz	no_magic	; done if so

	mov	bx,-1		; how much space does it want?
	mov	ax,multMagicdrv
	int	2fh		; get paragraphs into ax

;	Now adjust that for the extra 2 paragraph stub we're going to add

	add	ax,((offset tiny_stub_end - offset tiny_stub_start)+15)/16
	mov	DevSize,ax	; store that

	mov	DeviceHi, 0		; not to be loaded in UMB
	mov	DevSizeOption, 0
	mov	ConvLoad,1		; conventional load

	push	cs
	pop	ds			; BUGBUG - 7/9/92 chuckst - needed?

	mov	word ptr [bpb_addr],offset MagicDDName	; pass name so that
	mov	word ptr [bpb_addr+2],cs	; arena header can be set

	call	round			; normalize memhi:memlo

	call	InitDevLoad		; set up sub-arena, DevLoadAddr,
;					;  DevLoadEnd, and DevEntry
;					;  getss arena name from bpb_addr

;	check to make sure device driver fits our available space.

	mov	ax, DevLoadAddr
	add	ax, DevSize		; calculate seg after DD load
	mov	word ptr DevBrkAddr+2,ax ; save as ending address!
	mov	word ptr DevBrkAddr,0

;	Note:  no need to check out of memory here!  If we overflow the
;	       top, we just extend into the space where MagicDrv
;	       was temporarily located.  The driver should be smart
;	       enough to move itself correctly.

	mov	es, DevLoadAddr

;	First, move a little header in place so that this looks
;	to the mem command like a legitimate driver load.  Otherwise,
;	it will display garbage for the device name.

	mov	si,offset tiny_stub_start
	xor	di,di
	mov	cx,(offset tiny_stub_end - offset tiny_stub_start)
	rep	movsb			; move it!
	mov	ax,es			; advance es appropriately
	add	ax,((offset tiny_stub_end - offset tiny_stub_start)+15)/16
	mov	es,ax

	mov	ax,multMagicdrv
	mov	bx,-2			; final placement!!!!!
	int	2fh

	or	cs:[setdevmarkflag],for_devmark
	call	DevSetBreak		; go ahead and alloc mem for device
	call	DevBreak

no_magic:
	ret

MagicPostload	endp

;***	MagicSetCdss -- disable CDSs for still unmounted DblSpace drives
;
;	entry:
;	   CDSs are now persistent and in their final place

MagicSetCdss	proc	near

	mov	ax,multMagicdrv
	mov	bx,MD_VERSION	; get version
	int	2fh
	or	ax,ax		; is it there?
	jnz	magic_set_exit	; done if not

;	version call returned cl=first DblSpace drive in ASCII
;			      ch=number of DblSpace drive letters

	mov	al,cl		; get first DblSpace drive letter
	sub	al,'A'		; make it zero based.
	mov	dl,al		; and save for drive testing loop

	les	si,[dosinfo]	; point to DOS data area
	les	si,es:[si].sysi_cds ; fetch CDSs
	mov	ah,curdirLen
	mul	ah		; find first DblSpace CDS
	add	si,ax

	mov	cl,ch		; get DblSpace drive count into cx
	xor	ch,ch

;	We know cx > 0, or else the driver wouldn't have stayed resident

magic_set_cdss_1:

	push	si		; save cds pointer
	push	es
	push	cx		; save loop count
	push	dx		; and drive letter

	mov	ax,multMagicdrv
	mov	bx,MD_DRIVE_MAP ; inquire drive map
	int	2fh		; see if this is an unused DblSpace drive

	pop	dx
	pop	cx
	pop	es
	pop	si

	cmp	bl,dl		; if mapped to itself, it is vacant
	jnz	magic_set_cdss_2 ; skip if used

	and	es:[si].CURDIR_FLAGS,not CURDIR_inuse ; reset the bit in flags

magic_set_cdss_2:
	add	si,curdirLen
	inc	dl		; next drive
	loop	magic_set_cdss_1

magic_set_exit:
	ret

MagicSetCdss	endp


tiny_stub_start:
	dw	-1,-1		; phony device driver link
	dw	8000h		; mark as character device for MEM display
	dw	0,0		; strat and irpt
	db	'DBLSBIN$'	; magic default load
tiny_stub_end:


sysinitseg ends
	endif			; ifdef dblspace_hooks

	end
