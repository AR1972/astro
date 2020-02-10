include cmacros.inc

sBegin	DATA
stacker_lbl db	    'STACKER '	    ;
oldDTAaddr  dd	    0
myDTA	    db	    21 dup (0)	    ; reserved
	    db	    0		    ; attrib
	    dw	    0		    ; time
	    dw	    0		    ; date
	    dd	    0		    ; file size
volname     db	    13 dup (0)	    ; file name
	    db	    5  dup (0)	    ; buffer
pattern     db	    'A:*.*', 0	    ; search pattern
sEnd	DATA

sBegin	CODE
assumes cs,CODE
assumes ds,DATA

;------------------------------------------------------------------------
;
; get_volume_label
;
;   gets volume label of a drive
;
; Input:
;
;   WORD  drive (0=A, 1=B, ...)
;   LPSTR lpsz	receiving buffer
;
; Output:
;
;   void
;
cProc get_volume_label,<NEAR,PUBLIC>,<ds,es,ax,bx,cx,dx,si,di>
	parmW	drive
	parmD	lpsz
cBegin
	mov	ax,_DATA
	mov	ds,ax

	; replace DTA address with ours

	mov	ah,2fh
	int	21h
	mov	word ptr oldDTAaddr,bx
	mov	word ptr oldDTAaddr+2,es
	mov	ah,1ah
	lea	dx,myDTA
	int	21h

	; get volume label for the given drive

	lea	bx,pattern
	mov	ax,drive
	add	ax,'A'
	mov	byte ptr [bx],al
	mov	dx,bx
	mov	cx,8
	mov	ah,4eh
	int	21h

	; set up destination buffer

	cld
	les	di,lpsz

	jc	no_volume_label

	; copy volume label, insert brackets front and back

	mov	al,'['
	stosb

	lea	si,volname

	mov	cx,8
copy_name:
	lodsb
	cmp	al,'.'
	je	copy_ext
	stosb
	dec	cx
	cmp	al,0
	jnz	copy_name
	dec	di
	jmp	short copy_done

copy_ext:
	mov	al,' '
	rep	stosb
	mov	cx,3
	repnz	movsb

strip_tail:
	dec	di
	mov	al,byte ptr es:[di]
	cmp	al,' '
	je	strip_tail
	cmp	al,9
	je	strip_tail
	cmp	al,0
	je	strip_tail
	inc	di

copy_done:
	mov	al,']'
	stosb

no_volume_label:

	; store null terminator

	xor	ax,ax
	stosb

	; restore DTA address

	mov	ah,1ah
	mov	dx,word ptr oldDTAaddr
	mov	ds,word ptr oldDTAaddr+2
	int	21h
cEnd


;------------------------------------------------------------------------
;
; is_CDROM_drive
;
;   Determines if a drive is a CDROM drive
;
; Input:
;
;   WORD drive (0=A, 1=B, ...)
;
; Output:
;
;   returns non-zero for CDROM drive, 0 otherwise
;

cProc is_CDROM_drive,<NEAR,PUBLIC>,<bx,cx>
	parmW	drive
cBegin
	mov	ax, 1500h	; first test for presence of MSCDEX
	xor	bx, bx
	int	2fh
	mov	ax, bx		; MSCDEX is not there if bx is still zero
	or	ax, ax		; ...so return FALSE from this function
	jz	no_mscdex

	mov	ax, 150bh	; MSCDEX driver check API
	mov	cx, drive	; ...cx is drive index
	int	2fh
no_mscdex:
cEnd


;------------------------------------------------------------------------
;
; is_valid_CD
;
;   Determines if a CDROM drive contains a valid FAT disk.
;   Assumes current drive is a CDROM drive.
;
; Input:
;
;   none
;
; Output:
;
;   returns non-zero for a valid CDROM disk, 0 otherwise
;

cProc is_valid_CD,<NEAR,PUBLIC>,<bx,cx,dx,ds,es>
cBegin
	; replace DTA address with ours

	mov	ah,2fh
	int	21h
	mov	word ptr oldDTAaddr,bx
	mov	word ptr oldDTAaddr+2,es
	mov	ah,1ah
	lea	dx,myDTA
	int	21h

	; get "*.*" search pattern

	lea	dx,pattern
	add	dx,2		; go past "A:"
	mov	cx,8		; look for directory
	mov	ah,4eh
	clc
	int	21h
	mov	cx,1
	jc	ivc_found
	xor	cx,cx
ivc_found:

	; restore DTA address

	mov	ah,1ah
	mov	dx,word ptr oldDTAaddr
	mov	ds,word ptr oldDTAaddr+2
	int	21h

	mov	ax,cx
cEnd


;------------------------------------------------------------------------
;
; is_RAM_drive
;
;   Determines if a drive is a RAM drive
;
; Input:
;
;   WORD drive (0=A, 1=B, ...)
;
; Output:
;
;   returns non-zero for RAM drive, 0 otherwise
;

cProc is_RAM_drive,<NEAR,PUBLIC>,<ds,bx,dx>
	parmW	drive
cBegin
	mov	dx,drive	; set drive id
	inc	dx		; this function expects 1-based drive
	mov	ah,32h
	clc
	int	21h
	jc	not_ram 	; if failed assume it's not a RAM drive
	mov	al,ds:[bx+8]
	cmp	al,1		; only 1 FAT table, assume it's a RAM drive
	je	r_done
not_ram:
	xor	al,al
r_done:
	xor	ah,ah
cEnd

;------------------------------------------------------------------------
;
; is_Stacker_drive
;
;   Determines if a drive is a Stacker drive
;
; Input:
;
;   WORD drive (0=A, 1=B, ...)
;   LPSTR tempDTA
;
; Output:
;
;   returns non-zero for Stacker drive, 0 otherwise
;

ParamBlockStruc   struc
  LO_sector	dw    ?		; Lo word of starting sector
  HI_sector	dw    ?		; Hi word of starting sector
  SecCount  	dw    ?		; Number of sectors to read
  BuffOff	dw    ?		; Offset of Buffer
  BuffSeg	dw    ?		; Segment of Buffer
ParamBlockStruc   ends

cProc is_Stacker_drive,<NEAR,PUBLIC>,<ds,es,bx,cx,dx,di,si>
	parmW	drive
	parmD	tempDTA
	LocalV	ParamBlock, %(size ParamBlockStruc)
cBegin
	mov	ax,drive	; set drive id
	lds	bx,tempDTA
	mov	cx,1
	mov	dx,0
	push	bp		; Save BP
	int	25h
	pop	bx		; Remove flags from stack
	pop	bp		; Restore BP
	jnc	check_label

	; If this has failed, it could be a partition > 32 Meg. So, try
	; again assuming Partition size is > 32 Meg. If this also fails,
	; this is really an error;

	mov	ax,drive
	mov	dx,0

	; Fill the parameter block with proper values
	mov	ParamBlock.LO_sector,dx 	; Starting sector

	; We get only 16 bit starting sector; So, hi word is made zero
	mov	ParamBlock.HI_sector,0

	; The number of sectors to be read	
	mov	dx,1
	mov	ParamBlock.SecCount,dx

	; The address of the buffer to read into
	mov	dx,OFF_tempDTA
	mov	ParamBlock.BuffOff,dx
	mov	dx,SEG_tempDTA
	mov	ParamBlock.BuffSeg,dx

	; Keep the address of ParamBlock in DS:BX
	lea	bx,ParamBlock
	push	ss
	pop	ds
	mov	cx,-1		; > 32Meg partition
	push	bp		; Save BP
	int	25h
	pop	bx		; Remove the flags on stack
	pop	bp		; Restore BP
	jc	not_stacker

check_label:
	mov	cx,4		; See if disk label is right
	mov	ax,_DATA
	mov	es,ax
	lea	di,stacker_lbl
	lds	si,tempDTA
	add	si,3
	repe	cmpsw
	jcxz	found_stacker
	jmp	short not_stacker

found_stacker:
	mov	ax,1
	jmp	short s_done
not_stacker:
	xor	ax,ax
s_done:
cEnd


;------------------------------------------------------------------------
;
; get_current_drive
;

cProc get_current_drive,<NEAR,PUBLIC>
cBegin
	mov	ah,19h
	int	21h
	add	al,'a'		; 0=>a, 1=>b, etc
	sub	ah,ah
cEnd


;------------------------------------------------------------------------
;
; set_current_drive
;

cProc set_current_drive,<NEAR,PUBLIC>
	parmW	drive
cBegin
	mov	dl,drive
	mov	ah,0eh
	int	21h
cEnd


;------------------------------------------------------------------------
;
; get_free_space
;
; Input:
;
;   WORD drive (0=A, 1=B, ...)
;
; Output:
;
;   Returns free disk space in DX:AX
;

cProc get_free_space,<NEAR,PUBLIC>,<bx,cx>
	parmW	drive
cBegin
	mov	dl,drive
	inc	dl	    ; 1 based
	mov	ah,36h
	int	21h
	mul	cx
	mul	bx
cEnd


;------------------------------------------------------------------------
;
; get_boot_drive
;

cProc get_boot_drive,<NEAR,PUBLIC>
cBegin
	mov	ax,3305h	; ax = 3305h --> Get boot drive A=1,B=2, ect.
	int	21h		; Call DOS.
	mov	al,dl		; Return result in AX.
	xor	ah,ah
cEnd


;------------------------------------------------------------------------
;
; get_dos_version
;

cProc get_dos_version,<NEAR,PUBLIC,NODATA>, <si>
cBegin
        mov     ax,3000h
        int     21h
cEnd


;------------------------------------------------------------------------
;
; count_valid_drives
;

cProc count_valid_drives,<NEAR,PUBLIC>,<bx,cx,dx,es,di>
	parmD	drivelist
cBegin
	les	di,drivelist
	xor	cx,cx		; # Found so far

	mov	ah,19h
	int	21h		; Get current drive in AL
	mov	bx,ax		; Save so we can restore it

	cld
	stosb			; Save it as 1st buff entry

	xor	dx,dx		; Start with drive A:
find_loop:
	mov	ah,0eh		; Select the drive
	int	21h		; (AL contains max drive)
	mov	dh,al

	mov	ah,19h
	int	21h
	cmp	dl,al		; Q: Did change work?
	jne	short try_next	;    N: Invalid
				;    Y: Found one more
	inc	cx
	cld
	stosb

try_next:
	inc	dl		; DL = Next drive
	cmp	dl,dh		; Q: Any more drives to check?
	jb	short find_loop ;    Y: Keep looking

	mov	dl,bl		; DL = Original default drive
	mov	ah,0eh		; Select drive
	int	21h

	mov	ax,cx		; Return count
cEnd

sEnd	CODE
end
