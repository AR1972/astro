; Copyright 1990 Central Point Software, Inc. 
; All rights reserved.
;------------------------------------------------------------
; This is the module of REBUILD/UnFormat which restores
; hard disk partition records (etc) from the file
; saved by the the new partn-saving module of MIRROR.
; Invoked by /PARTN
;
; Written by GWD.   10-31-88.	v5.00
;
; * New feature (12-6-88): "/PARTN /L" can now display
;   the partition tables currently on the hard disk.
;   Also improved the format of listing.
;
; * Bug fix 2-1-89.  During Restore, leading zero partn table
;   entries would prevent the table from being displayed.
;   Unused partn table entries are now skipped & used ones are shown.
;
; * 5-1-89.  Change the meaning of minor versions.  Now really only
;   a single digit.  The field in the save-file will be 'minor_digit,0'.
;
; * 10-10-89.  v5.6  Change meaning of that extra 0 byte after minor_ver.
;   Now it is the version number of the PARTNSAV.FIL structure (still 0).
;   This avoids the problems between different ver# of MIRROR & REBUILD.
;
; * 02-16-90.	v6.0 beta.  Now handles cylinder numbers > 1023.
;		/L checks parms via INT 41h/46h & shows them if valid.
;
; * 02-22-90.	v6.  When hard drive parms don't match the PARTNSAV.FIL,
;		we display the saved info anyway, before aborting.
;
; * 12-05-90	v6 for MSoft.  /PARTN now supports up to 8 hard drives.
;		New file structure: version = 1.
;
;-------------------------------------------------------
;
prog	SEGMENT public
	ASSUME	cs:prog, DS:prog
;
IFDEF	publics
%	INCLUDE	@FileName&.pub
ENDIF
	PUBLIC	restore_partitions, list_partitions
;
	PUBLIC	list_a_partition	;(Just for debug)
;
; In the _ORG module.
	EXTRN	top_of_mem:word, major_ver:abs, minor_ver:abs
;
; In the _MAIN module.
	EXTRN	stack_end_ptr:word, options:word
;
; The following are in the _IO module.
	EXTRN	getline:near, flushkey:near, printc:near, crout:near
	EXTRN	display:near, dis_word:word
	EXTRN	pr_text:near, ask_for_yes:near
	EXTRN	show_date:near, show_time:near
;
; The following are in the _MSG module.
	EXTRN	msg_small_mem:byte, msg_abort:byte
	EXTRN	msg_no_hard_drives:byte
	EXTRN	msg_rp_prompt:byte, msg_rp_drv_query:byte
	EXTRN	msg_file_err:byte, msg_newer_ver:byte

;	M005 -- made msg_diff_drives into separate singular & plural messages

	EXTRN	msg_diff_drives_sing:byte, msg_diff_parms:byte
	EXTRN	msg_diff_drives_plural:byte

	EXTRN	msg_partn_date:byte
	EXTRN	msg_old_partn:byte
	EXTRN	msg_menu:byte, msg_menu1:byte, msg_menu2:byte
	EXTRN	msg_partn_query:byte, msg_bios_err:byte, msg_reboot:byte
	EXTRN	msg_no_action:byte
	EXTRN	msg_disk_parms:byte
	EXTRN	msg_partn_from_sec:byte, msg_bad_AA55:byte
	EXTRN	msg_partn_header:byte
	EXTRN	msg_dos12:byte, msg_dos16:byte, msg_dos_extend:byte
	EXTRN	msg_dos_huge:byte, msg_strange_partn:byte
	EXTRN	msg_pboot:byte, msg_pnoboot:byte
	EXTRN	msg_partn_size:byte, msg_partn_edge:byte, msg_partn_rel:byte
	EXTRN	msg_via_bios:byte, msg_via_table:byte
	EXTRN	quit_char:abs, all_char:abs
;
	INCLUDE UF_INCL.INC
;
max_drvs EQU	8		;The number of hard drives we can support.
;
;---------------------------------------------------------------------
; The partition table is located at offset 1BEh in the sector.
; The signature is located at offset 1FEh (= 55h, AAh or word AA55h).
;
; This is the structure of a single element of a partition table.
;
partn_struc STRUC
partn_boot	DB	?	;80h means it's bootable.
partn_head	DB	?	;Starting head.
partn_cx	DW	?	;Starting cylinder & sector.
partn_sys	DB	?	;0=none, 1=FAT12, 4=FAT16, 5=Extended, 6=Huge.
partn_end_head	DB	?
partn_end_cx	DW	?	;Cylinder & sector # combined.
partn_rel	DW	?,?	;Relative sector.
partn_size	DW	?,?	;Count of sectors.
partn_struc ENDS
;
;--------------------------------------------------------------------
; This is the format in which we store the hard-drive parameters
; (in this program's variables and also in the saved file).
;
drive_parm_str STRUC
drive_number	DB	0
drive_max_head	DB	0
drive_max_cyl	DW	0	;Pure cylinder number.
drive_max_sector DB	0
		DB	?	;Not used.
drive_parm_str ENDS
;
;--------------------------------------------------------------------
; Version number of the structure in the PARTNSAV.FIL file.
; This MUST be updated if the file structure is changed!
; This number must match the one defined inside REBUILD (UF_PARTN.ASM).
;
file_struc_ver_old EQU	0	;0 is original version.
file_struc_ver	EQU	1	;1 is the first revision.  12-4-90 GWD.
;
;--------------------------------------------------------------------
; This header is placed at the beginning of the saved file.
;
header_structure STRUC
h_id		DB	0,0		;Should be CDh,20h (INT 20h).
h_name		DB	"MIRROR "
h_major		DB	?		;Major version number of MIRROR.
		DB	"."
		DB	?
h_struc_ver	DB	file_struc_ver
h_name2		DB	" saved partitions",1Ah
h_control	DB	0		;See bit flag definitions.
h_file_size	DW	0		;Size of PARTNSAV.FIL, in bytes.
h_drives	DW	0		;Number of physical hard drives.
h_parm_ptr	DW	0		;File offset (always in 1st sector).
;
; The following two pointers are valid only in the rev 0 structure.
; In newer revisions, use the H_LIST_POINTERS instead.  12-04-90 GWD.
h_list80_ptr_old DW	0	;File offset of start of List_structures.
h_list81_ptr_old DW	0	;File offset of list_structures for drv 81h.
;
h_min		DB	0		;Date & time of the save.
h_hour		DB	0
h_day		DB	0
h_month 	DB	0
h_year		DW	0
h_dos_ver	DW	0	;DOS ver# used during save (hi byte = major).
;
h_list_pointers	DW  max_drvs DUP (0)	;Ptrs to lists for drives 80h - 87h.
;
header_structure ENDS
;
IF	(512 - SIZE header_structure) LT (max_drvs * SIZE drive_parm_str)
	.ERR	; Header structure too big!  No room for list of drive parms.
ENDIF
;
;--------------------------------------------------------------------
; The general structure of the PARTNSAV.FIL
;
offset_of_header =	0
offset_of_drv_parms =	(SIZE header_structure)	;List of drive_parm_str's.
offset_of_lists =	1*200h	;Lists are in the second sector of the file.
offset_of_data	=	2*200h	;Data (sector copies) start at third sector.
;
;--------------------------------------------------------------------
; One of these will be present for each sector of data.  Each non-zero
; H_LIST_POINTER in the header points to a sequence of these items.
;
list_structure STRUC
l_dx		DW	0	;Bios location - drive & head.	0= list end.
l_cx		DW	0	;Bios location - cylinder & sector.
l_offset	DW	0	;File offset of the saved data.
l_ext_l 	DW	0	;File offset of list item for extended partn.
list_structure ENDS
;
;
;-------------------------------------------------------------------
; Bit flags inside H_CONTROL.
;
bit_has_boot	EQU	1	;DOS 4 logical boot record follows each partn.
bit_ps2_machine EQU	2	;Probably.  Look for drive parms in 2nd sect.
;
;-------------------------------------------------------------------
partn_buf_addr LABEL dword
partn_buf_offset EQU this word
		DW	0
partn_buf_seg	DW	?
;
list_offset	DW	?	;Offset in partn_buf_seg of current list item.
handle		DW	0	;DOS handle # for open file.
;
;-------------------------------------------------------------------
drive_parms LABEL word
	drive_parm_str (max_drvs) DUP ( <> )
;
;-------------------------------------------------------------------
header	LABEL word
	header_structure <,,major_ver+"0",,"M">
;
;-------------------------------------------------------------------
packed_date	DW	?
packed_time	DW	?
;
next_cx 	DW	1	;Bios-style mixed Cyl# & sector#.
next_dx 	DW	80h
;
phys_drive	DB	?	;A physical BIOS drive number (e.g., 80h).
;
phys_drive_count DB	0	;Number of BIOS hard drives we will process.
real_drive_count DB	0	;Number of hard drives actually installed.
;
p_filename	DB	"A:\PARTNSAV.FIL",0
;
;=================================================================
;
; Upon entry here, we know that the /PARTN option was selected.
;
restore_partitions PROC NEAR
	nop
	call	calc_dma_addr
	jnc	short enough_mem
	jmp	restp_exit
enough_mem:
	call	get_phys_parms
	jnc	restp_got_parms
	jmp	restp_exit
restp_got_parms:
	lea	dx,msg_rp_prompt
	call	pr_text
ask_for_drive:
	mov	p_filename,"A"
	lea	dx,msg_rp_drv_query
	call	pr_text
	call	getline
	jc	restp_esc
	jz	open_file	;Use default of A.
	cmp	al,"A"
	jb	ask_for_drive
	cmp	al,"Z"
	ja	ask_for_drive
	mov	p_filename,al
	sub	al,"A"		;Now 0=A, 1=B, etc.
	mov	bl,al		;Save requested drive.
	mov	ah,19h		;Get current drive.
	int	21h
	mov	bh,al		;Save current drive.
	mov	dl,bl
	mov	ah,0Eh		;Set default drive to requested one.
	int	21h
	mov	ah,19h		;Fetch it again, to check.
	int	21h
	push	ax
	mov	dl,bh
	mov	ah,0Eh		;Restore original default drive.
	int	21h
	pop	ax
	cmp	al,bl		;Did the Set succeed?
	jne	ask_for_drive	;No.  Ask again.
	jmp	short open_file
restp_esc:
	lea	dx,msg_abort
	call	pr_text
	jmp	restp_exit
file_error_close:
	call	close_file
file_error:
	lea	dx,msg_file_err
	call	pr_text
	jmp	restp_exit
close_exit:
	call	close_file
	jmp	restp_exit
open_file:
	lea	dx,p_filename
	mov	ax,3D00h	;OPEN for reading.
	int	21h
	mov	handle,ax
	jc	file_error
	mov	bx,ax
	push	ds
	mov	ds,partn_buf_seg
	xor	dx,dx
	mov	cx,512		;512 bytes.
	mov	ah,3Fh		;READ from file.
	int	21h
	pop	ds
	cld
	jc	file_error_close
	cmp	ax,cx
	jne	file_error_close
	mov	es,partn_buf_seg
	cld
	xor	bx,bx
	mov	word ptr header.h_id, 20CDh
	lea	si, header.h_id
	lea	di, [bx].h_id
	mov	cx,( h_major - h_id )
	repe cmpsb			;Compare strings CDh,20h,"MIRROR "
	jne	file_error_close
	lea	si, header.h_name2
	lea	di, [bx].h_name2
	mov	cx,( h_control - h_name2 )
	repe cmpsb			;Compare string " saved partitions",1A
	jne	file_error_close
	mov	al, es:[bx].h_major
	cmp	al,"5"			;/PARTN feature began in MIRROR v5.
	jl	file_error_close
	cmp	byte ptr es:[bx].h_struc_ver, file_struc_ver
	jbe	ver_ok
ver_bad:
	lea	dx,msg_newer_ver
	call	pr_text
	jmp	close_exit
ver_ok: lea	di,header
	push	ds
	push	es		;Swap DS,ES.
	pop	ds
	pop	es		;DS=partn_buf_seg, ES=our data seg.
	xor	si,si
	mov	cx,SIZE header_structure
	rep movsb		;Copy header into our local data structure.
	push	es
	pop	ds		;Restore DS.  Now DS=ES= our data.
	nop
	lea	bx,header
	mov	ax, [bx].h_drives
	mov	phys_drive_count,al
;
; Handle the old file format.
;
	mov	al, [bx].h_struc_ver
	cmp	al,file_struc_ver
	je	know_file_version	;It's not the old format.
	cmp	al,file_struc_ver_old
	jne	ver_bad
	mov	ax, [bx].h_list80_ptr_old
	mov	[bx].h_list_pointers,ax	;Copy old list pointers to new table.
	mov	ax, [bx].h_list81_ptr_old
	mov	[bx].h_list_pointers+2,ax
know_file_version:
;
; Re-arrange text of version # from header, for display.  Make it asciiz.
;
	mov	al, [bx].h_major+2
	mov	[bx].h_major+1, al
	mov	byte ptr [bx].h_major+2, 0
	lea	ax, [bx].h_major
	mov	dis_word+(2*1),ax
;
; The main module of this program already has Procs to
; display packed-format date & time, so we'll use them.
;
	mov	ax, [bx].h_year
	sub	ax,1980
	mov	cl,9
	shl	ax,cl
	mov	dx,ax
	mov	al, [bx].h_month
	mov	ah,0
	mov	cl,5
	shl	ax,cl
	or	dx,ax
	or	dl, [bx].h_day
	mov	packed_date,dx
;
	mov	al, [bx].h_hour
	mov	ah,0
	mov	cl,11
	shl	ax,cl
	mov	dx,ax
	mov	al, [bx].h_min
	mov	ah,0
	mov	cl,5
	shl	ax,cl
	or	dx,ax
	mov	packed_time,dx
;
; "Partn info was saved by Mirror @1t "   @1t is for the version # we've read.
;
	lea	dx,msg_partn_date
	call	display
	mov	ax,packed_date
	call	show_date
	mov	al," "
	call	printc
	mov	ax,packed_time
	call	show_time
	call	crout
;
	mov	bx,handle
	mov	cx,(2 + 26 + 26) * 512	;The most we'll need to read.
	push	ds
	mov	ds,partn_buf_seg
	mov	dx,512		;Offset just after first sector, already read.
	mov	ah,3Fh		;READ from file.
	int	21h
	pop	ds
	nop
	jnc	read2_ok
	jmp	file_error_close
read2_ok:
	call	close_file
;
	mov	bx, header.h_file_size
	add	ax,512		;Adjust, since we already read the 1st sector.
	cmp	ax,bx
	je	fsize_ok
	jmp	file_error
fsize_ok:
;
	mov	phys_drive,80h
	mov	cl,phys_drive_count
	mov	ch,0
	mov	es,partn_buf_seg
	lea	si, header.h_list_pointers
display_file_loop1:
	mov	bx,[si]			; ES:BX --> start of a list.
display_file_loop2:
	call	show_old_partitions
	jnc	disp_file_next
	jmp	restp_esc
disp_file_next:
	inc	phys_drive
	lea	si,[si+2]	; SI --> list pointer for next drive.
	loop	display_file_loop1
shown_all_file:
	mov	al,phys_drive_count	;From the file.
	mov	bl,real_drive_count	;As currently installed.
	cmp	al,bl
	je	show_menu
	jb	warn_drive_count
	mov	phys_drive_count,bl	;Use the smaller of the two counts.
warn_drive_count:
	mov	ah,0
	mov	bh,0
	mov	dis_word+(1*2),ax
	mov	dis_word+(2*2),bx

;	M005 -- made the following into separate singular/plural messages

; "WARNING! Saved info for @1d drive(s), but now found @2d."
	lea	dx,msg_diff_drives_sing
	cmp	al,1				; singular drive?
	jz	msg_diff_dr_singular
	lea	dx,msg_diff_drives_plural	; use plural form, then
msg_diff_dr_singular:

;	M005 -- end changes

	call	display
;
show_menu:
	lea	dx,msg_menu	;"Options:  Q  = ..."
	call	pr_text
	lea	dx,msg_menu1	;"1 = Restore fixed disk 1"
	mov	al,phys_drive_count
	cmp	al,1
	je	show_menu2
	mov	ah,0
	lea	dx,msg_menu2	;"1 - @0d = Restore selected disk"
show_menu2:
	call	display
;
;
; Note - this input query can handle responses only for drives 80 - 87h.
; This must be rewritten for more than 8 drives.
;
IF	max_drvs GT 8
	.ERR	; Too many drives for /PARTN option user prompt.
ENDIF
;
ask_for_option:
	lea	dx,msg_partn_query	;"Which option? Q"
	call	pr_text
	call	getline
	jc	restp_esc_stp
	jz	restp_quit
	cmp	al,quit_char
	je	restp_quit
	cmp	phys_drive_count,1
	je	ask_single
	cmp	al,all_char
	jne	ask_single
	mov	phys_drive,80h
	jmp	short restp_final_ask	;Leave phys_drive_count unchanged.
ask_single:
	sub	al,"0"
	jbe	ask_for_option
	cmp	al,phys_drive_count
	ja	ask_for_option
	dec	ax
	or	al,80h			;Convert to BIOS drive number.
	mov	phys_drive,al
	mov	phys_drive_count,1	;Only do one drive.
	jmp	short restp_final_ask
restp_quit:
	lea	dx,msg_no_action
	call	pr_text
	jmp	restp_exit
restp_esc_stp:
	jmp	restp_esc
restp_final_ask:
	call	ask_for_yes
	jc	restp_esc_stp
	jnz	restp_quit
;
restp_write:
	nop
;
; Arrive here once for each hard drive.  Before
; we attempt partition restoration, verify that
; this drive is really compatible with the saved data.
; If it's not, then we abort the operation.
;
restp_wr_drv_lp:
	mov	ah,(SIZE drive_parm_str)
	mov	al,phys_drive
	and	al,7Fh
	mul	ah
	lea	si,drive_parms		;DS:SI --> current drive parms.
	mov	es,partn_buf_seg
	cld
	mov	di, header.h_parm_ptr	;ES:DI --> saved drive parms.
	add	si,ax
	add	di,ax
	mov	cx,(SIZE drive_parm_str)
	repe cmpsb			;Drive parms must be the same.
	je	parms_match
	mov	al,phys_drive
	and	ax,007Fh
	inc	ax
	lea	dx,msg_diff_parms	;"Parms wrong for drive @0d; skipping"
	call	display
	jmp	short restp_wr_next_drv
parms_match:
	mov	bl,phys_drive
	and	bx,007Fh
	shl	bx,1
	mov	di, header.h_list_pointers [bx]	; ES:DI --> list_structure's.
restp_wr_sector_lp:
	call	flushkey
	jc	restp_esc_stp
	mov	cx, es:[di].l_cx
	mov	dx, es:[di].l_dx
	or	dx,dx
	jz	restp_wr_next_drv
	mov	bx, es:[di].l_offset	;Note ES = partn_buf_seg already.
	test	options, opt_wrfake	; /TEST option was used?
	jnz	restp_wr_next_sector	;Yes.  Just pretend we've written.
	mov	ax,0301h		;Write one sector to disk.
	int	13h
	jc	restp_wr_err
restp_wr_next_sector:
	add	di,SIZE list_structure
	jmp	restp_wr_sector_lp
restp_wr_next_drv:
	inc	phys_drive
	dec	phys_drive_count
	jz	restp_done
	jmp	restp_wr_drv_lp
restp_wr_err:
	mov	al,ah
	mov	dis_word+(1*2),ax	;Error #.
	call	get_cyl_from_cx_dh
	mov	dis_word+(2*2),ax	;Cylinder.
	mov	dis_word+(3*2),dx	;Drive.
; "BIOS error @1bh on cylinder @2wh, drive @3bh."
	lea	dx,msg_bios_err
	call	display
	jmp	short restp_exit
restp_done:
	lea	dx,msg_reboot	;Success.  Press Enter to reboot.
	call	pr_text
	call	flushkey
	call	getline
	jc	restp_exit
reboot: cli
	mov	ax,40h
	mov	es,ax
	nop
	mov	es:72h,1234h	;Flag to skip memory test.
	DB	0EAh		;JMP FFFF:0 (reset).
	DW	0
	DW	0FFFFh
restp_exit:
	ret
restore_partitions ENDP
;
;---------------------------------------------------------------------
; Display current partition tables from the hard disks.
;
 COMMENT @ Output will look like this:

Drive #80h has dddd cylinders, dd heads, dd sectors per track (via Bios).

The following table is from drive dd, cylinder dddd, head dd, sector dd:

		Total_size	 Start_partition   End_partition
   Type       Bytes   Sectors	 Cyl Head Sector   Cyl Head Sector     Rel#
-----------  -----------------	----------------  ----------------  ----------
DOS16  Boot  .ddddM ddddddddd  .dddd..ddd ...dd  .dddd..ddd ...dd   ddddddddd
EXTEND	     .ddddM ddddddddd  .dddd..ddd ...dd  .dddd..ddd ...dd   ddddddddd

 @ End of comment block.
;
;
; On entry here, we know that "/PARTN /L" was selected.
;
list_partitions PROC NEAR
	call	calc_dma_addr
	jnc	lparts_enough
lparts_x_stp:
	jmp	lparts_exit
lparts_enough:
	call	get_phys_parms
	jc	lparts_x_stp
;
	mov	phys_drive,80h
lparts_drive:
	call	crout
	mov	ah,SIZE drive_parm_str
	mov	al,phys_drive
	and	al,7Fh
	mul	ah
	mov	si,ax
	lea	ax,msg_via_bios
	call	show_hard_parms
	call	get_alternate_parms	;Maybe use parms from vectors 41h/46h.
	or	ax,ax
	jz	lparts_parms_shown
	call	show_hard_parms
lparts_parms_shown:
;
	mov	dl,phys_drive
	mov	dh,0
	mov	next_dx,dx
	mov	next_cx,1
lparts_sector:
	mov	dx,next_dx
	mov	cx,next_cx
	call	get_cyl_from_cx_dh
	mov	dis_word+(1*2),ax
	mov	al,dh
	and	ax,003Fh
	mov	dis_word+(2*2),ax
	mov	al,cl
	and	al,3Fh
	mov	dis_word+(3*2),ax
	mov	dis_word+(4*2),dx
;
;"The following table is from drive @4bh, cylinder @1d, head @2d, sector @3d:"
;
	lea	dx,msg_partn_from_sec
	call	display
	call	flushkey
	jc	lparts_esc
	les	bx, partn_buf_addr
	mov	cx,next_cx
	mov	dx,next_dx
	mov	ax,0201h
	int	13h
	jnc	lparts_rd_ok
	mov	al,ah
	mov	dis_word+(1*2),ax	;Error #.
	call	get_cyl_from_cx_dh
	mov	dis_word+(2*2),ax	;Cylinder.
	mov	dis_word+(3*2),dx	;Drive.
; "BIOS error @1bh on cylinder @2wh, drive @3bh."
	lea	dx,msg_bios_err
	call	display
	jmp	short lparts_exit
lparts_rd_ok:
	xor	ax,ax
	mov	next_cx,ax		;Assume there is no extended partn.
	mov	next_dx,ax
	cmp	word ptr es:1FEh, 0AA55h	;Correct signature?
	je	lparts_sig_ok
	lea	dx,msg_bad_AA55
	call	pr_text
	jmp	short lparts_exit
lparts_esc:
	lea	dx,msg_abort
	call	pr_text
	jmp	short lparts_exit
lparts_sig_ok:
	lea	dx,msg_partn_header
	call	pr_text
;
	mov	di,1BEh 		;ES:DI = partn table in the sector.
	mov	cx,4			;There are four table entries.
lparts_table:
	call	flushkey		;This detects Ctrl-S (pause) & ESC.
	jc	lparts_esc		;ESC key pressed.
	mov	al,es:[di].partn_sys
	cmp	al,0			;Unused partn?
	jz	lparts_tbl3		;Yes.
	cmp	al,5			;Extended partn?
	jne	lparts_tbl2		;No.
	mov	ax, es:[di].partn_cx	;Yes.
	mov	dh, es:[di].partn_head
	mov	dl,phys_drive
	mov	next_cx,ax			;Remember location for next.
	mov	next_dx,dx
lparts_tbl2:
	call	list_a_partition	;ES:DI points at the entry.
lparts_tbl3:
	add	di, SIZE partn_struc
	loop	lparts_table
	cmp	next_dx,0
	jz	lparts_next_drv
	jmp	lparts_sector
lparts_next_drv:
	mov	al,phys_drive
	inc	ax
	mov	phys_drive,al
	and	al,7Fh
	cmp	al,phys_drive_count
	jae	lparts_exit
	jmp	lparts_drive
lparts_exit:
	ret
list_partitions ENDP
;
;=========================  Sub-Procedures  ============================
;
; Calculate the buffer address which is safe for Bios DMA.
;
; On entry: uses STACK_END_PTR.
;
; On exit: if successful, then CF=false & PARTN_BUF_SEG is valid.
;	   If not enough memory, then a message is shown & CF=true.
;
; Destroys AX,BX,DX.
;
calc_dma_addr PROC NEAR
	mov	bx,stack_end_ptr
	mov	cl,4
	shr	bx,cl
	mov	ax,cs		;Use CS, since we are a .COM file.
	add	bx,ax
calc_dma_lp:
	mov	ax,bx
	mov	dx,bx
	add	ax,((3 + 26 + 26) * 512) / 16	;Room for max partnsav file.
	and	ax,0F000h
	and	dx,0F000h
	cmp	ax,dx			;In the same 64k physical bank?
	je	got_dma_addr		;Yes.
	add	bx,(512 / 16)		;Bump it up & try again.
	jmp	calc_dma_lp
got_dma_addr:
	add	bx,((3 + 26 + 26) * 512) / 16
	cmp	bx,top_of_mem
	jb	dma_addr_ok
	lea	dx,msg_small_mem
	call	pr_text
	stc
	jmp	short calc_dma_exit
dma_addr_ok:
	mov	partn_buf_seg,bx
	xor	ax,ax
	mov	partn_buf_offset,ax	;CF=false.
calc_dma_exit:
	ret
calc_dma_addr ENDP
;
;---------------------------------------------------------
; Get the hard drive parms from the Bios.
;
; On exit: if OK, then CF=false.
;	   If error, then message is shown and CF=true.
;
; Destroys AX,BX,CX,DX.
;
get_phys_parms PROC NEAR
	mov	phys_drive,80h
	stc
	mov	dl,phys_drive
	mov	dh,0
	mov	ah,8		;Get drive parameters.
	int	13h
	jc	get_pp_none
	or	ah,ah
	jnz	get_pp_none
	cmp	dl,0
	jle	get_pp_none
	cmp	dl,max_drvs
	jbe	get_pp_1
	mov	dl,max_drvs	;Ignore drives beyond max.
get_pp_1:
	mov	phys_drive_count,dl
	mov	real_drive_count,dl
	lea	bx,drive_parms
get_pp_loop:
	push	bx
	mov	dl,phys_drive
	mov	[bx].drive_number,dl
	stc
	mov	ah,8		;Get parms.
	int	13h
	pop	bx
	jc	get_pp_err
	call	store_parms
	lea	bx,[bx] + SIZE drive_parm_str
	mov	al,phys_drive
	inc	ax
	mov	phys_drive,al
	and	al,7Fh
	cmp	al,phys_drive_count
	jb	get_pp_loop
get_pp_ok:
	clc
	jmp	short get_pp_end
get_pp_err:
	mov	al,ah
	mov	dis_word+(1*2),ax	;Error #.
	mov	dis_word+(2*2),-1	;No cylinder.
	mov	dis_word+(3*2),dx	;Drive.
; "BIOS error @1bh on cylinder @2wh, drive @3bh."
	lea	dx,msg_bios_err
	call	display
	mov	al,phys_drive
	and	al,7Fh
	mov	phys_drive_count,al	;Truncate the drive scanning.
	mov	real_drive_count,al
	jnz	get_pp_end
get_pp_none:
	lea	dx,msg_no_hard_drives
	call	pr_text
	stc
get_pp_end:
	ret
get_phys_parms ENDP
;
;-------------------------------------------------------------------
; See if we should use the alternate drive parms via INT 41h/46h.
; But cannot ever use them for XT-type controllers.
;
; On entry: DRIVE_PARMS [SI] is the structure for the
;	    current drive (PHYS_DRIVE).
;
; On exit: if the vectored table parms are valid and they describe
;	   a larger drive, then we copy them into our parms and
;	   return [AX] --> asciiz message indicating vectored parms.
;
;	   Otherwise, we return AX=0.
;
; Destroys AX,BX,CX,DX,DI.  SI & ES are preserved.
;
get_alternate_parms PROC NEAR
	push	si
	push	es
	mov	dl,phys_drive
	cmp	dl,81h
	ja	get_alt_no
	mov	cx,-1		;Preload CX with illegal return value.
	stc
	mov	ax,15FFh	;Read DASD type.
	int	13h
	jc	get_alt_no	;Not supported.  Must be old XT-type card.
	cmp	ch,2		;CX legal?
	ja	get_alt_no	;Sector count >32 million (16 Gbyte) is silly.
	cmp	ah,3		;Type = hard disk?
	je	get_alt3
	cmp	ax,0003h	;Bug in Speedstore returns 3 in AX, not AH.
	je	get_alt3	;Yes, it's probably Speedstore.
	jmp	short get_alt_no	;Strange Bios.
get_alt3:
	mov	ax,0F000h
	mov	es,ax
	mov	al,es:0FFFEh	;Machine ID byte.
	cmp	al,0FEh 	;Old PC or XT?
	jae	get_alt_no
	cmp	al,0FBh 	;Newer XT?
	je	get_alt_no
	jmp	short get_alt_maybe
get_alt_no:
	xor	ax,ax
	jmp	short get_alt_ret
get_alt_maybe:
	xor	ax,ax
	mov	es,ax
	mov	di,41h
	cmp	phys_drive,80h
	je	get_alt_v
	mov	di,46h
get_alt_v:
	shl	di,1
	shl	di,1
	les	bx,es:[di]
	mov	ax,es
	or	ax,bx
	jz	get_alt_no		;Vector = 0:0.
	cmp	word ptr es:[bx+12],0	;Landing zone is valid?
	jle	get_alt_no
	mov	ax,es:[bx]		;Cylinder count (well, sort-of).
	cmp	ax,4096
	ja	get_alt_no
	sub	ax, drive_parms [si].drive_max_cyl
	cmp	ax,5			;Ignore tiny differences.
	jle	get_alt_no
	mov	al,es:[bx+2]		;Head count.
	cmp	al,64
	ja	get_alt_no
	cmp	al, drive_parms [si].drive_max_head
	jbe	get_alt_no
	mov	al,es:[bx+14]		;Sectors per track.
	cmp	al,0
	jle	get_alt_no
	cmp	al,63
	ja	get_alt_no
get_alt_yes:
	mov	ax,es:[bx]
	sub	ax,2
	mov	drive_parms [si].drive_max_cyl, ax
	mov	al,es:[bx+2]
	dec	ax
	mov	drive_parms [si].drive_max_head, al
	mov	al,es:[bx+14]
	mov	drive_parms [si].drive_max_sector, al
	lea	ax,msg_via_table
get_alt_ret:
	pop	es
	pop	si
	ret
get_alternate_parms ENDP
;
;------------------------------------------------------------------
; On entry: [AX] = asciiz message for source of drive parms.
;
; Drive # xxh has nnnn cylinders, nn heads, nn sectors per track (from Bios).
;
; SI is preserved.  Destroys AX,BX,CX,DX,DI.
;
show_hard_parms PROC NEAR
	mov	dis_word+(4*2),ax	;Use appropriate message.
	mov	ax, drive_parms [si].drive_max_cyl
	inc	ax
	mov	dis_word+(1*2),ax
	mov	ah,0
	mov	al, byte ptr drive_parms [si].drive_max_head
	inc	ax
	mov	dis_word+(2*2),ax
	mov	al, byte ptr drive_parms [si].drive_max_sector
	mov	dis_word+(3*2),ax
	mov	al,phys_drive
;
; "Drive #@0bh has @1d cylinders, @2d heads, @3d sectors per track"
; " (from @4t).",cr,lf
;
	lea	dx,msg_disk_parms
	call	display
	ret
show_hard_parms ENDP
;
;---------------------------------------------------------------
; Store drive parms from CX,DX values returned by INT 13h AH=8.
;
; AX is destroyed.
;
store_parms PROC NEAR
	mov	[bx].drive_max_head, dh
	call	get_cyl_from_cx_dh
	mov	[bx].drive_max_cyl, ax
	mov	ax,cx
	and	al,3Fh
	mov	[bx].drive_max_sector, al
	ret
store_parms ENDP
;
;----------------------------------------------------------
; On entry: DH=head, CX = cyl & sector in Bios packed format.
; On exit: AX = cylinder number (0 to 4095).
;
; Only AX is changed.
;
get_cyl_from_cx_dh PROC NEAR
	mov	al,cl
	mov	ah,dh
	and	ax,0C0C0h	;Two more cyl bits in DH (bits 6,7).
	rol	ah,1
	rol	ah,1
	shl	ax,1
	shl	ax,1
	mov	al,ch
	ret
get_cyl_from_cx_dh ENDP
;
;-----------------------------------------------------------
; Close the file handle.  All regs are preserved.
;
close_file PROC NEAR
	push	ax
	push	bx
	xor	bx,bx
	xchg	bx,handle
	or	bx,bx
	jz	closef2
	mov	ah,3Eh		;CLOSE.
	int	21h
closef2:
	pop	bx
	pop	ax
	ret
close_file ENDP
;
;------------------------------------------------------------------
; Display abbreviated partition info for one drive.
;
; On entry: PHYS_DRIVE is valid (e.g., 80h),
;	    ES:BX = address of our list_structure.
;
; On exit: If user pressed ESC then CF=true, else CF=false.
;
; Preserves BX,CX,SI,DS,ES.
;
show_old_partitions PROC NEAR
	push	bx
	push	cx
	mov	al,phys_drive
	mov	dis_word+(2*2),ax
	and	ax,7Fh
	inc	ax
	mov	dis_word+(1*2),ax
;
; "Old partition info for fixed disk # @1d (DL=@2bh):"
;
	lea	dx,msg_old_partn
	call	display
	lea	dx,msg_partn_header
	call	pr_text
showp_lp:
	call	flushkey
	jnc	showp_2
	lea	dx,msg_abort
	call	pr_text
	stc
	jmp	short showp_exit
showp_2:
	mov	ax, es:[bx].l_dx
	or	ax,ax
	jz	showp_end		;End of our table of LIST_STRUCTURES.
	cmp	al,phys_drive
	jne	showp_end		;No more for this drive.
	mov	di, es:[bx].l_offset	;Buffer offset of the sector.
	add	di,1BEh 		;Offset of partn table in the sector.
	mov	cx,4			;At most, 4 entries per table.
showp_find_nz:
	mov	al, es:[di].partn_sys
	cmp	al,0
	jz	showp_skip_0		;Skip empty entries.
	cmp	al,5			;Extended partn?
	je	showp_skip_0		;Skip that, too.
	call	list_a_partition
showp_skip_0:
	add	di,SIZE partn_struc
	loop	showp_find_nz
showp_3:
	mov	bx, es:[bx].l_ext_l
	or	bx,bx			;Any extended partn to look at?
	jnz	showp_lp		;Yes.
showp_end:
	clc
showp_exit:
	pop	cx
	pop	bx
	ret
show_old_partitions ENDP
;
;---------------------------------------------------------------------
; Display, on one line, all the info in a single partn table entry.
;
; On entry: ES:DI points at a partition table entry.
;
; Destroys AX.	Others preserved.
;
list_a_partition PROC NEAR
	push	cx
	push	dx
	mov	al, es:[di].partn_sys
	mov	ah, es:[di].partn_boot
	test	ah,NOT 80h		;Value should be 0 or 80h.
	jnz	lap_strange
	lea	dx,msg_dos12
	cmp	al,1
	je	lap_type
	lea	dx,msg_dos16
	cmp	al,4
	je	lap_type
	lea	dx,msg_dos_extend
	cmp	al,5
	je	lap_type
	lea	dx,msg_dos_huge
	cmp	al,6
	je	lap_type
lap_strange:
	mov	dis_word+(1*2),ax	;Sys ID byte.
	mov	al,ah
	mov	dis_word+(2*2),ax	;Boot ind. byte.
	lea	dx,msg_strange_partn	;" @1bh?  @2bh? ",0
	call	display
	jmp	short lap_size
lap_type:
	call	pr_text
	lea	dx,msg_pboot		;"Boot "
	cmp	byte ptr es:[di].partn_boot, 80h
	je	lap_boot
	lea	dx,msg_pnoboot		;Blanks.
lap_boot:
	call	pr_text
lap_size:
	mov	ax, es:[di].partn_size
	mov	dx, es:[di].partn_size+2
	mov	dis_word+(3*2),ax
	mov	dis_word+(4*2),dx
	shr	dx,1		;DX,AX = sector count.
	rcr	ax,1		;Convert to kbytes (assume 2 sectors/kbyte).
	or	dx,dx
	jnz	lap_size_m	;More than 64M.
	cmp	ax,1024 	;1 Megabyte or greater?
	jae	lap_size_m	;Yes.
	mov	cl,"K"
	jmp	short lap_size_store
lap_size_m:
	add	ax,200h 	;Add one-half of the divisor, for round up.
	adc	dx,0
	mov	cx,10
lap_size_shr:
	shr	dx,1		;Divide by 1024 to yield Megabytes.
	rcr	ax,1
	loop	lap_size_shr
	mov	cl,"M"
lap_size_store:
	mov	dis_word+(1*2),ax
	mov	byte ptr dis_word+(2*2),cl
	lea	dx,msg_partn_size
	call	display 		;" @1r@2a @3l  ",0
;
	mov	cx, es:[di].partn_cx
	mov	dh, es:[di].partn_head
	call	get_cyl_from_cx_dh
	mov	dis_word+(1*2),ax
	mov	al,cl
	and	ax,3Fh
	mov	dis_word+(3*2),ax
	mov	al,dh
	and	al,3Fh
	mov	dis_word+(2*2),ax	;AH is already 0.
	lea	dx,msg_partn_edge	;"@1r@2r @3r  ",0
	call	display
;
	mov	cx, es:[di].partn_end_cx
	mov	dh, es:[di].partn_end_head
	call	get_cyl_from_cx_dh
	mov	dis_word+(1*2),ax
	mov	al,cl
	and	ax,3Fh
	mov	dis_word+(3*2),ax
	mov	al,dh
	and	al,3Fh
	mov	dis_word+(2*2),ax
	lea	dx,msg_partn_edge	;"@1r@2r @3r  ",0
	call	display
;
	mov	ax, es:[di].partn_rel
	mov	dis_word+(1*2),ax
	mov	ax, es:[di].partn_rel+2
	mov	dis_word+(2*2),ax
	lea	dx,msg_partn_rel
	call	display
	pop	dx
	pop	cx
	ret
list_a_partition ENDP
;
;----------------------------------------------
prog	ENDS
	END
