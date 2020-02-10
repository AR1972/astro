;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include bambi.inc

public	detect_stacker
public  detect_stacker_volume
public  stacker_dd_pointer

zseg    segment public 'CODE'

	assume  cs:zseg
	assume	ds:nothing

st_ptr	DD	0			;pointer to Stacker (0-->not there)
stacker_tested	db	0		;havent tested for stacker yet
stacker_version dw	0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
; Input: None
; Output:AX=0  --> Stacker not installed
;	 AX<>0 --> Stacker version*100  (e.g.  1.00 --> 0064H)
;	
;	Stacker is detected by making an INT 25H call with invalid
;	parameters.
;
detect_stacker proc
	cmp	cs:stacker_tested,0
	je	do_stacker_test
	mov	ax,cs:stacker_version
	ret
do_stacker_test:
	push	bp
	push	si
	push	di

	sub	sp,1024			;use the stack as a temp buffer

	mov	ax,0cdcdh		;invalid drive #
	mov	bx,sp			;DS:BX --> buffer for INT 25H
	mov	cx,1
	xor	dx,dx			;read boot sector of invalid drive
	mov	word ptr [bx],dx	;clear the "return" value
	push	ds			;(set ES:BP to fix DOS 3 INT 25H bug)
	pop	es			
	mov	bp,bx
	int	25H
	pop	cx			;get rid of flags

	xor	ax,ax			;default is No stacker
	mov	bx,sp			;point at result
	cmp	word ptr [bx],0CDCDH	;Stacker INT 25 fills in some fields.
	jnz	gotres			;Make sure they all match
	cmp	word ptr 2[bx],1
	jnz	gotres
	les	di,4[bx]		;pointer into Stacker
	cmp	word ptr es:[di],0A55AH	;must find signature
	jnz	gotres
	mov	word ptr st_ptr  ,di	;save pointer to show it's found
	mov	word ptr st_ptr+2,es
	mov	ax,es:[di+2]		;stacker version * 100
gotres:
	add	sp,1024
	pop	di			;restore regs
	pop	si
	pop	bp
	mov	stacker_tested,1
	mov	stacker_version,ax
	ret
detect_stacker	endp


;
; Input: dx->driveno: 0=A, 1=B, 2=C, etc.
;
; Output:AX=1 --> Is a Stacker volume
;	 AX=0 --> Is not a Stacker volume
;	 BL = Stacker unit number if AX=1
;	
; Notes: If a multitasking environment is present (such as Windows), you 
;	 must make this a critical section of code!
;
;	 This method uses the removeable media ioctl call to detect 
;	 Stacker volumes.  It does NOT work under DR DOS 5.0, since that
;	 version of DOS does NOT pass these calls through to the Stacker
;	 device driver.
;

UNIT_OFFS equ	3EH			;offset with Stacker of 

driveno dw ?
detect_stacker_volume proc 
	mov	cs:driveno,dx
	cmp	word ptr st_ptr+2,0	;already found Stacker?
	jnz	stacker_fnd
	call	detect_stacker		;is not, try again
	xor	ax,ax			;return 0 if not found
	cmp	ax,word ptr st_ptr+2	;is it ther
	jz	retOp
	; Here if Stacker IS installed.
stacker_fnd:
	mov	ah,30h			;treat DR DOS special
	int	21h			;(it doesn't pass 4408H through)
	cmp	ax,1F03H
	mov	ax,4408H		;do an ioctl call 
	jnz	use08
	mov	al,0EH			;(DR DOS, Compaq 3.31: 440EH)

use08:	les	di,st_ptr
	mov	byte ptr es:UNIT_OFFS[di],0FFH	;set Stacker unit #
	mov	bx,driveno
	inc	bx			;adjust for default

	int	21h			;if Stacker drive, will return
	mov	ax,0
	jc	retOp			;no ioctl support --> not Stacker

	les	di,st_ptr		;see if unit # changed
	mov	bl,byte ptr es:UNIT_OFFS[di]
	cmp	bl,0FFH
	jz	retOp

	inc	ax			;if so, we have a Stacker volume
retOp:	ret
detect_stacker_volume endp



;
; This data structure is part of the Stacker internal data structure 
; maintained for each Stacker drive.  A pointer to this structure can be
; obtained from the routine stacker_dd_pointer() below.  See ST.C for
; an example.  Assuming that nothing else has "stolen" the DPB for a drive,
; you can perform a sanity check by comparing the strategy and interrupt
; pointers found here with those found in another DPB (or in the device
; chain).  As a quick check, you could also make sure that the segment
; part of dv_strategy is the same as that for dv_interrupt.
;
; To force Stacker to call a different device driver, the following
; fields must be modified:  dv_strategy, dv_interrupt, and dv_unit.
;
;
STACKER_DD struc
dv_strategy	dd	1 dup(?)	;physical device driver strategy addr
dv_interrupt	dd	1 dup(?)	;physical device driver interrupt addr
dv_att		dw	1 dup(?)	;device driver header attributes
dv_cluster0	dw	1 dup(?)	;first file cluster of
dv_log2		db	1 dup(?)	;LOG base 2 of physical bytes/sector
dv_unit		db	1 dup(?)	;physical device driver unit number
STACKER_DD ends

STACKER_UNIT_DD_OFFSET	equ	015CH

;
; C declaration:  STACKER_DD far *stacker_dd_pointer(int driveno);
;
; Input: dx->driveno: 0=A, 1=B, 2=C, etc.
;
; Output:DX:AX = far pointer to the STACKER_DD struc (0:0 if error)
;	
; Notes: If a multitasking environment is present (such as Windows), you 
;	 must make this a critical section of code!

stacker_dd_pointer proc 
	mov	cs:driveno,dx
	push	driveno
	call	detect_stacker_volume
	pop	cx			;get rid of driveNo on stack
	cwd
	or	ax,ax
	jnz	isStacker
	ret
isStacker:	
	les	di,st_ptr
	mov	di,es:4[di]		;get unit table pointer
	xor	bh,bh
	add	bx,bx			;multiply unit # by two
	mov	ax,es:[di+bx]		;dx:ax = per unit pointer
	add	ax,STACKER_UNIT_DD_OFFSET
	mov	dx,es			;return pointer
	ret
stacker_dd_pointer endp

zseg ends

end



































