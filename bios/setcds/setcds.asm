	page	,132
;***	MagicSetCdss -- disable CDSs for still unmounted DblSpace drives
;	   this routine was lifted out of MS-DOS 6's SYSPRE.ASM module.
;
;	entry:
;	   CDSs are now persistent and in their final place

	.xlist
break	macro		; dummy macro for include files
	endm
	include	curdir.inc
	include	magicdrv.inc
	include	sysvar.inc
	.list

boofy	segment
	assume	cs:boofy

entryx:
	call	MagicSetCdss
	mov	ax,4c00h	; terminate
	int	21h

MagicSetCdss	proc	near

	mov	ah,52h		; get DOS DATA pointer
	int	21h
	mov	si,bx		; get pointer into es:si

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

boofy	ends
	end	entryx
