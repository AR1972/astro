;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1981-1991
; *                      All Rights Reserved.
; */
	page	,160
	title msbio2 - bios
;
;----------------------------------------------------------------------------
;
; M052 : Resolved the confusion of media byte F9. BIOS used to default to 1.2M
;	 drive if formfactor was != ffsmall & mediabyte = f9.
;
;----------------------------------------------------------------------------
;
	include version.inc	; set build flags
	include biosseg.inc	; define segment structure

	include msequ.inc
	include	bpb.inc
	include	msbds.inc
	include	bootform.inc

;	include	mult.inc
multMULT	equ	4ah
multMULTSWPDSK	equ	0
;


;***	define where the rom routines are actually located
;	   in the buggy old AT BIOS that we might need to
;	   install a special level of int13 handler for


rom	segment at 0f000h

	org	2e1eh
romcommand	label	far

	org	2e7fh
romwait		label	far

	org	2ee2h
romwait_drq	label	far

	org	2ef8h
romcheck_status	label	far

	org	2f69h
romcheck_dma	label	far

	org	2f8eh
romget_vec	label	far

	org	0ff65h	; far return in rom
romfret		label	far

rom	ends

	include msgroup.inc	; define Bios_Data segment

;	Most of the disk routines keep es:[di] set up pointing to the
;	  currently selected bds.  This is often assumed to be the standard
;	  operating environment and in some cases may not be mentioned in
;	  the subroutine headers where it will be assumed.



	extrn	eot:byte
	extrn	fhave96:byte
	extrn	old13:dword

	extrn	ptrsav:dword
	extrn	start_bds:dword
	extrn	fdrive1:word
	extrn	fdrive2:word
	extrn	fdrive3:word
	extrn	fdrive4:word
	extrn	tim_drv:byte
	extrn	medbyt:byte
	extrn	drvmax:byte
	extrn	disksector:byte

	extrn	dskdrvs:word


;   data structures for real-time date and time

	extrn	bin_date_time:byte
	extrn	month_table:word
	extrn	daycnt2:word
	extrn	feb29:byte

	extrn	nul_vid:byte
	extrn	tmp_vid:byte
;SR;
; WIN386 presence flag
;
	extrn	IsWin386:byte
;
; WIN386 routine to set focus to VM
;
	extrn	V86_Crit_SetFocus:far


; CODE segment declarations...

	tocode

	extrn	Bios_Data_Word:word
	extrn	con_flush:near
	extrn	setptrsav:near
	extrn	harderr:near
	extrn	harderr2:near
	extrn	maperror:near
	extrn	getbp:near
	extrn	checksingle:near
	extrn	check_time_of_access:near
	extrn	has1:near
	extrn	Has720K:near		; M052
	extrn	read_sector:near


	public	dsk_init
dsk_init proc	near
	assume	ds:Bios_Data,es:nothing

	mov	ah,byte ptr drvmax
	mov	di,offset dskdrvs
	push	ds			; pass result in es:di
	pop	es
	jmp	setptrsav
dsk_init endp


; install_bes installs a bds at location ds:di into the current linked list of
; bds maintained by this device driver. it places the bds at the end of the
; list.  Trashes (at least) ax, bx, di, si

	public	install_bds
install_bds	proc	near
	assume	ds:Bios_Data,es:nothing

	push	ds			; save Bios_Data segment
	mov	si,offset start_bds	; beginning of chain

;	ds:si now points to link to first bds
;	  assume bds list is non-empty

loop_next_bds:
	lds	si,[si].bds_link		; fetch next bds
	mov	al,es:[di].bds_drivenum	; does this one share a physical
	cmp	[si].bds_drivenum,al	;  drive with new one?
	jnz	next_bds

	mov	bl,fi_am_mult		; set both of them to i_am_mult if so
	or	byte ptr es:[di].bds_flags,bl
	or	byte ptr [si].bds_flags,bl
	and	byte ptr es:[di].bds_flags,not fi_own_physical ; we don't own it

	mov	bl,byte ptr [si].bds_flags	; determine if changeline available
	and	bl,fchangeline
	or	byte ptr es:[di].bds_flags,bl

next_bds:
	cmp	word ptr [si].bds_link,-1	; are we at end of list?
	jnz	loop_next_bds

	mov	word ptr [si].bds_link+2,es ; install bds
	mov	word ptr [si].bds_link,di
	mov	word ptr es:[di].bds_link,-1	; set next pointer to null
	pop	ds			; restore Bios_Data

;	**** If the new drive has a higher EOT value, we must alter the
;	     'eot' variable appropriately.

	mov	al,byte ptr es:[di].BDS_RBPB.BPB_SECTORSPERTRACK
	cmp	al,eot
	jbe	eot_ok
	mov	eot,al
eot_ok:
	ret

install_bds endp

;-------------------------------------------------
;
;  ask to swap the disk in drive a:
;	es:di -> bds
;	ds -> Bios_Data
;
	public	swpdsk
swpdsk	proc	near
	assume	ds:Bios_Data,es:nothing

;SR;
; Set focus to the VM within which this message is to be issued
;
	test	[IsWin386],1	;Is win386 present?
	jz	no_win386	;no, skip SetFocus

	call	V86_Crit_SetFocus	;set focus to the correct VM

no_win386:
	push	cx
	push	dx			; save what we use

	mov	dl, es:[di].bds_drivelet; get the drive letter
;
; WARNING : next two instructions assume that if the new disk is for drive B
;           then existing dsk is drive A & vice versa
;
	mov	dh, dl
	xor	dh, 1
	sub	cx, cx			; nobody has handled swap disk
	mov	ax, (multMULT shl 8) or multMULTSWPDSK
					; broad cast code for swap disk
	int	2fh			; Braodcast it
	inc	cx			; cx == -1 ?
	jz	swpdsk9			; somebody has handled it

;using a different drive in a one drive system so request the user change disks

	add	dl,"A"
IFNDEF ROMDOS				; M095 Can't write to ROM locations
	mov	cs:drvlet,dl
ENDIF					; M095
	mov	si,offset sngmsg	; cs:si -> message
	push	bx
	
	lods	cs:byte ptr [si]	;get the next character of the message
wrmsg_loop:
	int	29h

IFDEF ROMDOS				; M095 Begin
	cmp	SI, OFFSET drvlet	; Set the flags for latter
	lods	cs:byte ptr [si]	; Get next character
	jne	@f			; IF not drive letter keep this char
	mov	AL,DL			; ELSE use driver letter from DL
@@:
ELSE
	lods	cs:byte ptr [si]
ENDIF					; M095 End
	or	al,al
	jnz	wrmsg_loop

	call	con_flush		; flush out keyboard queue
	xor	ah,ah			; set command to read character
	int	16h			; call rom-bios
	pop	bx
swpdsk9:
	pop	dx
	pop	cx
	ret
swpdsk	endp

	include msbio.cl2

; input : es:di points to current bds for drive.
; return : zero set if no open files
;	   zero reset if open files

chkopcnt proc near
	assume	ds:Bios_Data,es:nothing

	cmp	es:[di].bds_opcnt,0
	ret

chkopcnt endp


; at media check time, we need to really get down and check what the change is.
; this is guaranteed to be expensive.
;
;	es:di -> bds, ds -> Bios_Data

	public mediacheck
mediacheck proc near
	assume	ds:Bios_Data,es:nothing

	call	checksingle		; make sure correct disk is in place
	xor	si,si
	call	haschange
	jz	mediaret

	call	checkromchange
	jnz	mediadovolid

	push	ax
	push	dx

	mov	dl,es:[di].bds_drivenum	; set logical drive number
	mov	ah,16h 			; get changeline status
	int	13h			; call rom diskette routine

	pop	dx
	pop	ax
	jc	mediadovolid
	mov	si,1			; signal no change

; there are some drives with changeline that "lose" the changeline indication
; if a different drive is accessed after the current one. in order to avoid
; missing a media change, we return an "i don't know" to dos if the changeline
; is not active and we are accessing a different drive from the last one.
; if we are accessing the same drive, then we can safely rely on the changeline
; status.

	mov	bl,[tim_drv] 		; get last drive accessed
	cmp	es:[di].bds_drivenum,bl
	jz	mediaret

; do the 2 second twiddle. if time >= 2 seconds, do a volid check.
; otherwise return "i don't know" (strictly speaking, we should return a
; "not changed" here since the 2 second test said no change.)

	push	ax
	push	cx
	push	dx

	call	check_time_of_access

	pop	dx
	pop	cx
	pop	ax

	or	si,si
	jz	mediadovolid		; check_time says ">= 2 secs passed"

	xor	si,si			; return "i don't know"

mediaret:
	ret

; somehow the media was changed.  look at vid to see. we do not look at fat
; because this may be different since we only set medbyt when doing a read
; or write.

mediadovolid:
	call	getbp		       ; build a new bpb in current bds
	jc	mediaret

	call	check_vid
	jnc	mediaret

	jmp	maperror		; fix up al for return to dos
mediacheck endp

; simple, quick check of latched change.  if no indication, then return
; otherwise do expensive check.  if the expensive test fails, pop off the
; return and set al = 15 (for invalid media change) which will be returned to
; dos.
;
; for dos 3.3, this will work only for the drive that has changeline.


;	call with es:di -> bds, ds -> Bios_Data
;	***** warning:  this routine will return one level up on the stack
;			if an error occurs!

	public checklatchio
checklatchio proc near
	assume	ds:Bios_Data,es:nothing

; if returning fake bpb then assume the disk has not changed

	call	chkopcnt
	jz	checkret	; done if zero

; check for past rom indications.  if no rom change indicated, then return ok.

	call	checkromchange
	jz	checkret		; no change

; we now see that a change line has been seen in the past.  let's do the
; expensive verification.

	call	getbp			; build bpb in current bds
	jc	ret_no_error_map	; getbp has already called maperror

	call	check_vid
	jc	checklatchret		; disk error trying to read in.

	or	si,si			; is changed for sure?
	jns	checkret

	call	returnvid
checklatchret:
	call	maperror		; fix up al for return to dos
ret_no_error_map:
	stc
	pop	si			; pop off return address
checkret:
	ret

checklatchio endp

; check the fat and the vid.  return in di -1 or 0.  return with carry set
; only if there was a disk error.  return that error code in ax.
;
;	called with es:di -> bds, ds -> Bios_Data

checkfatvid proc near
	assume	ds:Bios_Data,es:nothing

	call	fat_check
	or	si,si
	js	changed_drv

;	the fat was the same.  fall into check_vid and check volume id.

checkfatvid endp			; fall into check_vid

;now with the extended boot record, the logic should be enhanced.
;
;if it is the extended boot record, then we check the volume serial
;number instead of volume id. if it is different, then set si to -1.
;
;if it is same, then si= 1 (no change).
;
;if it is not the extended boot record, then just follows the old
;logic. dos 4.00 will check if the # of fat in the boot record bpb
;is not 0.  if it is 0 then it must be non_fat based system and
;should have already covered by extended boot structure checking.
;so, we will return "i don't know" by setting si to 0.
;
;this routine assume the newest valid boot record is in cs:[disksector].
;(this will be gauranteed by a successful getbp call right before this
;routine.)
;
;	called with es:di -> bds, ds -> bds

check_vid proc	near
	assume	ds:Bios_Data,es:nothing

;	check the disksector.EXT_BOOT_SIG variable for the extended
;	   boot signature.  if it is set then go to do the extended
;	   id check otherwise continue with code below

	cmp	disksector.EXT_BOOT_SIG,ext_boot_signature
	jz	do_ext_check_id

	call	haschange
	jz	checkret

	xor	si,si				; assume i don't know.
	cmp	disksector.EXT_BOOT_BPB.BPB_NUMBEROFFATS,0 ; don't read vol id
	je	checkfatret			; from the directory if not
						; fat system
	call	read_volume_id
	jc	checkfatret

	call	check_volume_id
	mov	si,-1				; definitely changed
	jnz	changed_drv
	inc	si				; not changed

vid_no_changed:
	call	resetchanged
	clc
checkfatret:
	ret

changed_drv:
	clc				; cas -- return no error
	mov	[tim_drv],-1 		; ensure that we ask rom for media
	ret				; check next time round


; extended id check

do_ext_check_id:
	push	ax

;**************************************************************
;	the code to check extended id is basically a check to see if the
;	volume serial number is still the same.  the volume serial number
;	previously read is in cs:disksector.EXT_BOOT_SERIAL
;	ds:di points to the bds of the
;	drive under consideration.  the bds has fields containing the
;	high and low words of the volume serial number of the media in the
;	drive.	compare these fields to the fields mentioned above.  if these
;	fields do not match the media has changed and so we should jump
;	to the code starting at ext_changed else return "i don't know" status
;	in the register used for the changeline status and continue executing
;	the code given below.  for temporary storage use the register which
;	has been saved and restored around this block.
;
;	bds fields in inc\msbds.inc

	mov	ax,word ptr disksector.EXT_BOOT_SERIAL
	cmp	ax,word ptr es:[di].bds_vol_serial
	jnz	ext_changed
	mov	ax,word ptr disksector.EXT_BOOT_SERIAL+2
	cmp	ax,word ptr es:[di].bds_vol_serial+2
	jnz	ext_changed
	xor	si,si			; don't know

	pop	ax
	jmp	vid_no_changed		;reset the flag

ext_changed:				; serial number is different!
	pop	ax
	mov	si,-1			; disk changed!
	clc				; clear carry. only si is meaningful here.
	jmp	changed_drv

check_vid endp

; at i/o time, we detected the error.  now we need to determine whether the
; media was truly changed or not.  we return normally if media change unknown.
; and we pop off the call and jmp to harderr if we see an error.
;
;	es:di -> bds

	public	checkio
checkio	proc	near
	assume	ds:Bios_Data,es:nothing

	cmp	ah,06
	jnz	checkfatret
	call	chkopcnt
	jz	checkfatret		; no open files

	call	getbp			; build up a new bpb in current bds

	jc	no_error_map		; getbp has already called maperror

	call	checkfatvid
	jc	checkioret		; disk error trying to read in.
	or	si,si			; is changed for sure?
	js	checkioerr		; yes changed

ignorechange:
	inc	bp			; allow a retry
	ret

checkioerr:
	call	returnvid
checkioret:
	stc				; make sure carry gets passed through
	jmp	harderr

no_error_map:
	jmp	harderr2
checkio	endp

; return vid sets up the vid for a return to dos.
;	es:di -> bds, returns pointer in packet to bds_volid
;	  **** trashes si! ****

returnvid proc	near
	assume	ds:Bios_Data,es:nothing

	mov	si,extra		; offset into pointer to return value
	call	vid_into_packet
	mov	ah,6			; invalid media change
	stc
	ret

returnvid endp

; moves the pointer to the volid for the drive into the original request packet
; no attempt is made to preserve registers.
;
;	assumes es:di -> bds
;	  **trashes si**

	public media_set_vid
media_set_vid proc near
	assume	ds:Bios_Data,es:nothing

	mov	si,trans+1		; return the value here in packet

media_set_vid	endp			; fall into vid_into_packet


;	return pointer to vid in bds at es:di in packet[si]

vid_into_packet proc near
	assume	ds:Bios_Data,es:nothing

	push	ds			; save Bios_Data
	lds	bx,[ptrsav]
	assume	ds:nothing
	add	di,bds_volid
	mov	[bx.si],di
	sub	di,bds_volid		; restore bds
	mov	[bx.si.2],es
	pop	ds			; restore Bios_Data
	assume	ds:Bios_Data
	ret

vid_into_packet endp


;   hidensity - examine a drive/media descriptor to set the media type.  if
;   the media descriptor is not f9 (not 96tpi or 3 1/2), we return and let the
;   caller do the rest.  otherwise, we pop off the return and jump to the tail
;   of getbp. for 3.5" media, we just return.
;
;   inputs:	es:di point to correct bds for this drive
;		ah has media byte
;
;   outputs:	carry clear
;		    no registers modified
;		carry set
;		    al = sectors/fat
;		    bh = number of root directory entries
;		    bl = sectors per track
;		    cx = number of sectors
;		    dh = sectors per allocation unit
;		    dl = number of heads
;
	public hidensity
hidensity proc near
	assume	ds:Bios_Data,es:nothing

; check for correct drive

	test	es:[di].bds_flags,fchangeline	; is it special?
	jz	dofloppy		; no, do normal floppy test

; we have a media byte that is pretty complex.	examine drive information
; table to see what kind it is.

	cmp	es:[di].bds_formfactor,ffsmall ;  is it single-media?
	jz	dofloppy		; yes, use fatid...

; 96 tpi drive

	cmp	ah,0f9h
	jnz	dofloppy

;
;------ If formfactor of drive = ffother or ff288 it has to be ; M052
;------ a 720K diskette					       ; M052
;
	cmp	es:[di].bds_formfactor,ffother	; M052
	je	Is720k				; M052

	cmp	es:[di].bds_formfactor,ff288	; M052
	je	Is720k				; M052

	mov	al,7			; seven sectors / fat
	mov	bx,224*256+0fh		; 224 root dir entries & 0f sector max
	mov	cx,80*15*2		; 80 tracks, 15 sectors/track, 2 sides
	mov	dx,01*256+2		; sectors/allocation unit & head max
	add	sp,2			; pop off return address
	jmp	has1			; return to tail of getbp
Is720K:					;			 M052
	add	sp,2			; pop off return address M052
	jmp	Has720K			; return to 720K code	 M052
dofloppy:
	ret

hidensity endp

; set_changed_dl - sets flag bits according to bits set in bx.
;		   essentially used to indicate changeline, or format.
;
;   inputs:	dl contains physical drive number
;		bx contains bits to set in the flag field in the bdss
;   outputs:	none
;   registers modified: flags
;
;	called from int13 hooker.  Must preserve ALL registers!!!
;
; in the virtual drive system we *must* flag the other drives as being changed

	public	set_changed_dl
set_changed_dl proc near
	assume	ds:Bios_Data,es:nothing

	push	es		    ; save current bds
	push	di

	les	di,[start_bds]

;	note:  we assume that the list is non-empty

scan_bds:
	cmp	es:[di].bds_drivenum,dl
	jnz	get_next_bds

; someone may complain, but this *always* must be done when a disk change is
; noted.  there are *no* other compromising circumstances.

	or	es:[di].bds_flags,bx		; signal change on other drive

get_next_bds:
	les	di,es:[di].bds_link		; go to next bds
	cmp	di,-1
	jnz	scan_bds		; loop unless we hit end of chain

skipset:
	pop	di			; restore registers
	pop	es
	ret

set_changed_dl endp


; checkromchange - see if external program has diddled rom change line.
;
;   inputs:	es:di points to current bds.
;   outputs:	zero set - no change
;		zero reset - change
;   registers modified: none

checkromchange proc near
	assume	ds:Bios_Data,es:nothing

	test	es:[di].bds_flags,fchanged
	ret

checkromchange	endp

; resetchanged - restore value of change line
;
;   inputs:	es:di points to current bds
;   outputs:	none
;   registers modified: none

	public	resetchanged
resetchanged proc near
	assume	ds:Bios_Data,es:nothing

	and	es:[di].bds_flags,not fchanged
	ret

resetchanged endp

; haschange - see if drive can supply change line
;
;   inputs:	es:di points to current bds
;   outputs:	zero set - no change line available
;		zero reset - change line available
;   registers modified: none

	public	haschange
haschange proc near
	assume	ds:Bios_Data,es:nothing

	test	es:[di].bds_flags,fchangeline
	ret

haschange	endp


;-------------------------------------------------------------------------
;
;	   set_volume_id       -	main routine, calls other routines.
;	   read_volume_id      -	read the volume id and tells if it has
;					   been changed.
;	   transfer_volume_id  -	copy the volume id from tmp to special
;					   drive.
;	   check_volume_id     -	compare volume id in tmp area with one
;					   expected for drive.
;	   fat_check	       -	see of the fatid has changed in the
;					   specified drive.

;-------------------------------------------------------------------------

; set_volume_id
;   if drive has changeline support, read in and set the volume_id
; and the last fat_id byte.  if no change line support then do nothing.
;
;   on entry:
;	es:di points to the bds for this disk.
;	ah contains media byte
;
;   on exit:
;	carry clear:
;	   successful call
;	carry set
;	   error and ax has error code
;

	public set_volume_id
set_volume_id	proc	near
	assume	ds:Bios_Data,es:nothing

	push	dx			; save registers
	push	ax
	call	haschange		; does drive have changeline support?
	jz	setvret 		; no, get out

	call	read_volume_id		; read the volume id
	jc	seterr			; if error go to error routine

	call	transfer_volume_id	; copy the volume id to special drive
	call	resetchanged		; restore value of change line

setvret:				; set volume return
	clc				; no error, clear carry flag
	pop	ax			; restore registers
	pop	dx
	ret

seterr:
	pop	dx			; pop stack but don't overwrite ax
	pop	dx			; restore dx
	ret

set_volume_id	endp

root_sec	dw	?	;root sector #


; read_volume_id read the volume id and tells if it has been changed.
;
;   on entry:
;	es:di points to current bds for drive.
;
;   on exit:
;	carry clear
;	    si = 1  no change
;	    si = 0  ?
;	    si = -1 change
;
;	carry set:
;	    error and ax has error code.

read_volume_id proc near
	assume	ds:Bios_Data,es:nothing

	push	dx			; preserve registers
	push	cx
	push	bx
	push	ax

	push	es			; stack the bds last
	push	di

	push	ds			; point es to Bios_Data
	pop	es

	mov	di,offset tmp_vid
	mov	si,offset nul_vid
	mov	cx,VOLID_SIZ
	rep	movsb			; initialize tmp_vid to null vi_id

	pop	di			; restore current bds
	pop	es

	mov	al,es:[di].BDS_BPB.BPB_NUMBEROFFATS    	; # of fats
	mov	cx,es:[di].BDS_BPB.BPB_SECTORSPERFAT 	; sectors / fat
	mul	cl			; size taken by fats
	add	ax,es:[di].BDS_BPB.BPB_RESERVEDSECTORS  ; add on reserved sectors

					; ax is now sector # (0 based)
	mov	[root_sec],ax		; set initial value
	mov	ax,es:[di].BDS_BPB.BPB_ROOTENTRIES ; # root dir entries
	mov	cl,4			; 16 entries/sector
	shr	ax,cl			; divide by 16
	mov	cx,ax			; cx is # of sectors to scan

next_sec:
	push	cx			; save outer loop counter
	mov	ax,[root_sec]		; get sector #
	mov	cx,es:[di].BDS_BPB.BPB_SECTORSPERTRACK	; sectors / track
	xor	dx,dx
	div	cx

;	set up registers for call to read_sector

	inc	dx		; dx= sectors into track, ax= track count from 0
	mov	cl,dl		; sector to read
	xor	dx,dx
	div	es:[di].BDS_BPB.BPB_HEADS ; # heads on this disc
	mov	dh,dl		; head number
	mov	ch,al		; track #

	call	read_sector	; get first sector of the root directory,

				; ds:bx -> directory sector
	jc	readviderr	; error on read

	mov	cx,16		; # of dir entries in a block of root
	mov	al,08h		; volume label bit

fvid_loop:
	cmp	byte ptr [bx],0		; end of dir?
	jz	no_vid		; yes, no vol id
	cmp	byte ptr [bx],0e5h	; empty entry?
	jz	ent_loop	; yes, skip
	test	[bx+11],al	; is volume label bit set in fcb?
	jnz	found_vid	; jmp yes
ent_loop:
	add	bx,32		;add length of directory entry
	loop	fvid_loop

	pop	cx		; outer loop
	inc	[root_sec]	; next sector
	loop	next_sec	; continue
notfound:
	xor	si,si
	jmp	short fvid_ret

found_vid:
	pop	cx		; clean stack of outer loop counter
	mov	si,bx		; point to volume_id
	push	es		; preserve currnet bds
	push	di
	push	ds
	pop	es		; point es to Bios_Data
	mov	di,offset tmp_vid ; dest of volume_id
	mov	cx,VOLID_SIZ -1 ; length of string minus nul
	rep	movsb		; mov volume label to tmp_vid
	xor	al,al
	stosb			; null terminate
	xor	si,si
	pop	di		; restore current bds
	pop	es
fvid_ret:
	pop	ax
	clc
rvidret:
	pop	bx		; restore register
	pop	cx
	pop	dx
	ret

no_vid:
	pop	cx		; clean stack of outer loop counter
	jmp	notfound	; not found

readviderr:
	pop	si		; trash the outer loop counter
	pop	si		; caller's ax, return our error code instead
	jmp	rvidret

read_volume_id endp


;   transfer_volume_id - copy the volume id from tmp to special drive
;
;   inputs:	es:di has current bds
;   outputs:	bds for drive has volume id from tmp

transfer_volume_id proc near
	assume	ds:Bios_Data,es:nothing

	push	di		; preserve bds
	push	si
	push	cx

	mov	si,offset tmp_vid   ; source
	add	di,bds_volid
	mov	cx,VOLID_SIZ

	cld
	rep	movsb		; transfer

	pop	cx
	pop	si
	pop	di		; restore current bds
	ret

transfer_volume_id endp

;   check_volume_id - compare volume id in tmp area with one expected for
;	drive
;
;   inputs:	es:di has current bds for drive
;   outputs:	zero true means it matched

check_volume_id proc near
	assume	ds:Bios_Data,es:nothing

	push	di
	push	cx

	mov	si,offset tmp_vid
	add	di,bds_volid
	mov	cx,VOLID_SIZ

	cld
	repz	cmpsb			; are the 2 volume_ids the same?

	pop	cx
	pop	di			; restore current bds
	ret

check_volume_id	endp

;   fat_check - see of the fatid has changed in the specified drive.
;	      - uses the fat id obtained from the boot sector.
;
;   inputs:	medbyt is expected fat id
;		es:di points to current bds
;
;   output:	si = -1 if fat id different,
;		si = 0 otherwise
;
;   no other registers changed.

fat_check proc	near
	assume	ds:Bios_Data,es:nothing

	push	ax
	xor	si,si			 ; say fat id's are same.
	mov	al,medbyt
	cmp	al,es:[di].BDS_BPB.BPB_MEDIADESCRIPTOR 	; compare it with the bds medbyte
	jz	okret1			; carry clear
	dec	si
okret1:
	pop	ax
	ret

fat_check endp


	public end96tpi_code
end96tpi_code label byte

Bios_Code	ends

;	title:	disk

;		there is a bug in some versions of ibm's at rom bios.
;		interrupts are not disabled during read operations.
;
;	use:	this program should be chained in line with the disk
;		interupt 13h, it intercepts read calls to the hard disk
;		and handles them appropriately.  for other functions it
;		passes controll to old13, which should contain the
;		address of the at rom disk routine. the entry point for
;		this program is ibm_disk_io.
;

	.286c		;use 80286 non-protected mode

bioseg		= 040h		;segment for rom bios data

bad_disk 	= 01

hf_port 	= 01f0h
hf_reg_port 	= 03f6h

;*	offsets into fixed disk parameter table

fdp_precomp	= 5
fdp_control 	= 8

data	segment at bioseg	;rom bios data segment

	org 42h
cmd_block	db 6 dup (?)

;*	offsets into cmd_block for registers

pre_comp = 0	;write pre-compensation
sec_cnt	 = 1	;sector count
sec_num	 = 2	;sector number
cyl_low	 = 3	;cylinder number, low part
cyl_high = 4	;cylinder number, high part
drv_head = 5	;drive/head (bit 7 = ecc mode, bit 5 = 512 byte sectors, 
		;            bit 4 = drive number, bits 3-0 have head number)
cmd_reg  = 6	;command register


	org 074h

disk_status1 	db ?
hf_num		db ?
control_byte	db ?

data	ends

;
; Possibly disposable data, goes at end of data group
;


Bios_Data_Init	segment

	extrn	old13:dword		;link to at bios int 13h

public	ibm_disk_io	

	assume cs:datagrp
	assume ds:data


;***	ibm_disk_io - main routine, fixes at rom bug
;
;	entry:	(ah) = function, 02 or 0a for read.
;		(dl) = drive number (80h or 81h).
;		(dh) = head number.
;		(ch) = cylinder number.
;		(cl) = sector number (high 2 bits has cylinder number).
;		(al) = number of sectors.
;		(es:bx) = address of read buffer.
;		for more on register contents see rom bios listing.
;		stack set up for return by an iret.
;
;	exit:	(ah) = status of current operation.
;		(cy) = 1 if failed, 0 if successful.
;		for other register contents see rom bios listing.
;
;	uses:	
;
;
;	warning: uses old13 vector for non-read calls.
;		does direct calls to the at rom.
;		does segment arithmatic.
;
;	effects: performs disk i/o operation.

ibm_disk_io proc far
	assume	ds:data,es:nothing

	cmp	dl,80h
	jb	atd1		;pass through floppy disk calls.
	cmp	ah,02
	je	atd2		;intercept call 02 (read sectors).
	cmp	ah,0ah
	je	atd2		;and call 0ah (read long).

atd1:
	jmp	old13		;use rom int 13h handler.

atd2:
	push	bx
	push	cx
	push	dx
	push	di
	push	ds
	push	es
	push	ax

	mov	ax,bioseg	;establish bios segment addressing.
	mov	ds,ax

	mov	disk_status1,0	;initially no error code.
	and	dl,7fh		;mask to hard disk number
	cmp	dl,hf_num
	jb	atd3		;disk number in range

	mov	disk_status1,bad_disk
	jmp	short atd4	;disk number out of range error, return

atd3:
	push	bx
	mov	ax,es		;make es:bx to seg:000x form.
	shr	bx,4
	add	ax,bx
	mov	es,ax
	pop	bx
	and	bx,000fh
	push	cs
	call	check_dma
	jc	atd4		;abort if dma across segment boundary

	pop	ax		;restore ax register for setcmd
	push	ax
	call	setcmd		;set up command block for disk op
	mov	dx,hf_reg_port
	out	dx,al		;write out command modifier
	call	docmd		;carry out command
atd4:

;---------------------------------------------------
;  new code - let logical or clear carry and then set carry if ah!=0
;	       and save a couple bytes while were at it.

	pop	ax
	mov	ah,disk_status1	;on return ah has error code
	or	ah,ah
	jz	atd5		;carry set if error

	stc

atd5:
	pop	es
	pop	ds
	pop	di
	pop	dx
	pop	cx
	pop	bx
	ret	2		;far return, dropping flags

ibm_disk_io endp

;***	setcmd - set up cmd_block for the disk operation
;
;	entry:	(ds) = bios data segment.
;		(es:bx) in seg:000x form.
;		other registers as in int 13h call
;	
;	exit:	cmd_block set up for disk read call.
;		control_byte set up for disk operation.
;		(al) = control byte modifier
;
;
;	sets the fields of cmd_block using the register contents
;	and the contents of the disk parameter block for the given drive.
;
;	warning: (ax) destroyed.
;		does direct calls to the at rom.

setcmd	proc near
	assume	ds:data,es:nothing

	mov	cmd_block[sec_cnt],al
	mov	cmd_block[cmd_reg],020h ;assume function 02
	cmp	ah,2
	je	setc1			;cmd_reg = 20h if function 02 (read)

	mov  cmd_block[cmd_reg],022h	;cmd_reg = 22h if function 0a (" long)

setc1:					;no longer need value in ax
	mov	al,cl
	and	al,03fh			;mask to sector number
	mov	cmd_block[sec_num],al
	mov	cmd_block[cyl_low],ch
	mov	al,cl
	shr	al,6			;get two high bits of cylender number
	mov	cmd_block[cyl_high],al
	mov	ax,dx
	shl	al,4			;drive number
	and	ah,0fh
	or	al,ah			;head number
	or	al,0a0h			;set ecc and 512 bytes per sector
	mov	cmd_block[drv_head],al

	push	es			;get_vec destroys es:bx
	push	bx
	push	cs
	call	get_vec
	mov	ax,es:fdp_precomp[bx]	;write pre-comp from disk parameters
	shr	ax,2
	mov	cmd_block[pre_comp],al	;only use low part
	mov	al,es:fdp_control[bx]	;control byte modifier
	pop	bx
	pop	es
	mov	ah,control_byte
	and	ah,0c0h			;keep disable retry bits
	or	ah,al
	mov	control_byte,ah
	ret
setcmd	endp	


;***	docmd - carry out read operation to at hard disk
;
;	entry:	(es:bx) = address for read in data.
;		cmd_block set up for disk read.
;
;	exit:	buffer at (es:bx) contains data read.
;		disk_status1 set to error code (0 if success).
;
;	
;
;	warning: (ax), (bl), (cx), (dx), (di) destroyed.
;		no check is made for dma boundary overrun.
;
;	effects: programs disk controller.
;		performs disk input.

docmd	proc	near
	assume	ds:data,es:nothing

	mov	di,bx		;(es:di) = data buffer addr.
	push	cs
	call	command
	jnz	doc3
doc1:
	push	cs
	call	waitt		;wait for controller to complete read
	jnz	doc3
	mov	cx,100h		;256 words per sector
	mov	dx,hf_port
	cld			;string op goes up
	cli			;disable interrupts (bug was forgetting this)

;	M062 -- some of these old machines have intermittent failures
;		when the read is done at full speed.  Instead of using
;		a string rep instruction, we'll use a loop.  There is
;		a slight performance hit, but it only affects these
;		very old machines with an exact date code match, and
;		it makes said machines more reliable
;
;M062	repz	insw		;read in sector

rsct_loop:			; M062
	insw			; M062 read in sector
	loop	rsct_loop	; M062

  	sti
	test	cmd_block[cmd_reg],02
	jz	doc2		;no ecc bytes to read.
	push	cs
	call	wait_drq
	jc	doc3
	mov	cx,4		;4 bytes of ecc
	mov	dx,hf_port
	cli
	repz	insb		;read in ecc
  	sti
doc2:
	push	cs
	call	check_status
	jnz	doc3		;operation failed
	dec	cmd_block[sec_cnt]
	jnz	doc1		;loop while more sectors to read
doc3:
	ret
docmd	endp


;***	get_vec - get pointer to hard disk parameters.
;
;	entry:	(dl) = low bit has hard disk number (0 or 1).
;
;	exit:	(es:bx) = address of disk parameters table.
;
;	uses:	ax for segment computation.
;
;	loads es:bx from interrupt table in low memory, vector 46h (disk 0)
;	or 70h (disk 1).
;	
;	warning: (ax) destroyed.
;		this does a direct call to the at rom.

get_vec	proc	near
	assume	ds:data,es:nothing

	push	offset romfret
	jmp	romget_vec
get_vec endp


;***	command - send contents of cmd_block to disk controller.
;
;	entry:	control_byte 
;		cmd_block - set up with values for hard disk controller.
;
;	exit:	disk_status1 = error code.
;		nz if error, zr for no error.
;
;
;	warning: (ax), (cx), (dx) destroyed.
;		does a direct call to the at rom.
;
;	effects: programs disk controller.
;
command	proc	near
	assume	ds:data,es:nothing

	push	offset romfret
	jmp	romcommand
command endp


;***	waitt - wait for disk interrupt
;
;	entry:	nothing.
;
;	exit:	disk_status1 = error code.
;		nz if error, zr if no error.
;
;
;	warning: (ax), (bl), (cx) destroyed.
;		does a direct call to the at rom.
;		
;	effects: calls int 15h, function 9000h.

waitt	proc	near
	assume	ds:data,es:nothing

	push   offset romfret
	jmp	romwait
waitt	endp


;***	wait_drq - wait for data request.
;
;	entry:	nothing.
;
;	exit:	disk_status1 = error code.
;		cy if error, nc if no error.
;
;	warning: (al), (cx), (dx) destroyed.
;		does a direct call to the at rom.

wait_drq proc	near
	assume	ds:data,es:nothing

	push	offset romfret
	jmp	romwait_drq
wait_drq endp


;***	check_status - check hard disk status.
;
;	entry:	nothing.
;
;	exit:	disk_status1 = error code.
;		nz if error, zr if no error.
;
;
;	warning: (ax), (cx), (dx) destroyed.
;		does a direct call to the at rom.

check_status proc near
	assume	ds:data,es:nothing

	push	offset romfret
	jmp	romcheck_status
check_status endp


;***	check_dma - check for dma overrun 64k segment.
;
;	entry:	(es:bx) = addr. of memory buffer in seg:000x form.
;		cmd_block set up for operation.
;
;	exit:	disk_status1 - error code.
;		cy if error, nc if no error.
;
;	warning: does a direct call to the at rom.

check_dma proc near
	assume	ds:data,es:nothing

	push	offset romfret
	jmp	romcheck_dma
check_dma endp

; the following label defines the end of the at rom patch.  this is used at
; configuration time.
;
; warning!!! this code will be dynamically relocated by msinit.

	public	endatrom
endatrom label	byte

;	M015 -- begin changes
;
;	Certain old COMPAQ '286 machines have a bug in their ROM BIOS.
;	  When Int13 is done with AH > 15h and DL >= 80h, they trash
;	  the byte at DS:74h, assuming that DS points to ROM_DATA.
;	  If our init code detects this error, it will install this
;	  special Int13 hook through the same mechanism that was set
;	  up for the IBM patch above.  This code is also dynamically
;	  relocated by MSINIT.

	public	compaq_disk_io

compaq_disk_io	proc	far
	assume	cs:datagrp
	assume	ds:nothing,es:nothing

	cmp	ah,15h
	ja	mebbe_hookit	; only deal with functions > 15h
no_hookit:
	jmp	old13		; branch to ROM routine

mebbe_hookit:
	cmp	dl,80h
	jb	no_hookit	; bug is only on hardfile references

	push	ds
	push	ax
	mov	ax,40h
	mov	ds,ax
	pop	ax

	pushf			; simulate int13
	call	old13
	pop	ds
	retf	2

	public	end_compaq_i13hook
end_compaq_i13hook:

compaq_disk_io	endp


;	M015 -- end changes

Bios_Data_Init	ends
	end
