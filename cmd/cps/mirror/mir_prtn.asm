; Copyright 1990-1991 Central Point Software, Inc. 
; All rights reserved.
;----------------------------------------------------------
; This module is part of the CPS 5.0 MIRROR.COM file.
; If option /PARTN is used, this saves the partition
; tables from the BIOS hard drives to a floppy file and
; terminates (does not return to caller).
;
; Created & written by GWD 10-27-88.
;
;
; Modifications:
;
; 11-29-88 GWD
; For ESDI drives ( #heads>16 ), don't bother checking if the
; ending head # of partn is exactly in range.  FDISK bug (?).
;
; 02-01-89 GWD
; Delete previous 'end_head # kluge' (11-29-88), replace with:
; For a partn table entry of type=Extended, don't check partn_end_head.
; IBM 3.30 FDISK writes silly values there.  Since Disk Manager (et al)
; won't have an extended partn, it's OK to ignore that end_head #.
;
; 02-15-90 GWD	6.0 BETA
; Adding handling for non-standard sys_id types in master partn sector.
; Added support for cylinders > 1023 (bits 7,6 of head byte).
;
; 02-16-90 GWD	6.0 Beta
; /U - new option to unload resident Delete-Tracking module.
; (Located here because it's much easier to modify my code than Jim's.)
;
; 02-19-90 GWD	6.0 Beta
; During 'unload' operation, Ctrl-C is disabled by taking INT 23h.
;
; 02-21-90 GWD	6.0 Beta
; Now hook INT 19h, too.  Helps prevent Unloading when maybe unsafe.
;
; 02-23-90 GWD 6.0 beta
; Now put dummy hook on INT 2Fh for same reasons as for INT 19h.
;
; 04-27-90 GWD
; Relocated global proc 'uppercase' into _DTRK module.
;
; ??-??-90 Who knows?
; Various non-documented hacked mods for MSoft.
;
; 12-04-90 GWD (after hack mods by both Jim S. & MSoft)
; Support for hard drives 80h - 87h (new in DOS 5.0).
; This requires the file PARTNSAV.FIL to have a different structure.
;
;----------------------------------------------------------
;
;
CODE	SEGMENT para public 'CODE'	;(Just like Jim's modules)
	ASSUME	CS:CODE, DS:CODE
;
IFDEF	publics
%	INCLUDE	@FileName&.pub
ENDIF
;
	PUBLIC	save_partition
;
	EXTRN	major_digit:abs, minor_digit:abs	;In ORG module.
;
	EXTRN	chk_dtrk_resident:near			;In DTRK module.
	EXTRN	dos_intercept:far
	EXTRN	int25_patch:far, int26_patch:far
	EXTRN	saved25:word, saved26:word
	EXTRN	saved19:word, int19_service:far
	EXTRN	saved2F:word, int2F_service:far
	EXTRN	dos_vector:dword
	EXTRN	uppercase:near
;
	EXTRN	end_prog:byte				;In LAST module.
;
	EXTRN	msg_ps_null:byte, msg_ps_banner:byte, msg_ps_readerr:byte
	EXTRN	msg_ps_bad_partn:byte, msg_ps_too_many:byte
	EXTRN	msg_ps_success:byte, msg_ps_file_err:byte
	EXTRN	msg_ps_small_mem:byte, msg_ps_floppy:byte
	EXTRN	msg_ps_drv_query:byte, msg_ps_bad_parm:byte
	EXTRN	msg_mir_help:byte, msg_crlf:byte
	EXTRN	msg_unloaded:byte, msg_cannot_unload:byte
	EXTRN	msg_not_resident:byte
;
cr	EQU	13
lf	EQU	10
;
max_drvs EQU	8
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
		DW	?,?	;Relative sector.
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
;; file_struc_ver EQU	0	;0 is original version.
file_struc_ver	EQU	1	;1 is the first revision.  12-4-90 GWD.
;
;--------------------------------------------------------------------
; This header is placed at the beginning of the saved file.
;
header_structure STRUC
h_id		DB	0,0		;Should be CDh,20h (INT 20h).
h_name		DB	"MIRROR "
		DB	?		;Major version number.
		DB	"."
		DB	?		;Minor version #.
h_struc_ver	DB	file_struc_ver
		DB	" saved partitions",1Ah
h_control	DB	0		;See bit flag definitions.
h_file_size	DW	0		;Size of PARTNSAV.FIL, in bytes.
h_drives	DW	0		;Number of physical hard drives.
h_parm_ptr	DW	0		;File offset.
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
h_list_pointers	DW  max_drvs DUP (0)	;Ptrs to lists for drives 80h - 87h.
;
header_structure ENDS
;
IF	(SIZE header_structure) GT 400
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
;---------------------------------------------------------
; Bit flags inside H_CONTROL.
;
bit_has_boot	EQU	1	;DOS 4 logical boot record follows each partn.
bit_ps2_machine EQU	2	;Probably.  Look for drive parms in 2nd sect.
;
;---------------------------------------------------------
; Bit flags for command-line options we recognize in this module.
;
opt_partn	EQU	1	;Partition save, not regular Mirror operation.
opt_unload	EQU	2	;Try to remove resident Delete-Tracking module.
opt_help	EQU	4	;Display help text & terminate.
;
	EVEN
dma_addr EQU this dword 	;Points to our DTA (buffer).
dma_offset	DW	?	;Offset to next item to be written.
dma_seg 	DW	?	;Segment of free memory beyond end of program.
;
list_pointer	DW	?	;Offset in DTA of current processed list item.
handle		DW	?	;Handle of open target file we created.
partn_count	DW	0
parm_ptr	DW	drive_parms	;Offset of parms to use.
last_p_rec_ptr	DW	-1	;File offset of recent partn list item.
;
;----------------------------------------------------------
; Offsets into our buffer of the table entries for the
; normal DOS partition & the extended partition (if any).
;
dos_entry_ptr	DW	?	;0 means no such partn in the table.
ext_entry_ptr	DW	?	;0 means none.
;
;----------------------------------------------------------
; Logical drive to save file on (0=A, 1=B).  Usually a floppy drive.
;
floppy		DB	0
;
;----------------------------------------------------------
; These are used to remember the location of the next partition record.
;
drive	DB	80h
head	DB	0
cx0	DW	0
;
phys_drives DB	0	;Actual number of physical drives to be processed.
master_flag DB	0	;NZ= last sector read was trk0, h0, s1 (master partn).
;
;---------------------------------------------------------
	EVEN
header	LABEL word
	header_structure <,,major_digit,,"M",,,,,,(offset_of_drv_parms)>
;
;---------------------------------------------------------
	EVEN
drive_parms LABEL byte
	drive_parm_str	max_drvs DUP ( <> )
;
;---------------------------------------------------------
ibm_parm_id DB	"IBM HARDFILE TYP"
ibm_parm_length EQU $-ibm_parm_id
;
line_input DB	8 DUP (0)	;Input buffer used for INT 21h, AH=0Ah.
;
filename DB	"x:\PARTNSAV.FIL",0
;
;---------------------------------------------------------
;
save_partition PROC NEAR
	push	ds
	push	es
;
	push	cs
	pop	ds			;DS = CS for this .COM file.
	cld
	sti
	nop
	call	look_for_option 	;Returns result in flags.
	jz	ret_to_mirror		;Nothing for us to do.
	test	al,(opt_help)
	jnz	show_help
	test	al,(opt_unload)
	jnz	unload_req
	lea	dx,msg_ps_banner
	call	pr_text
	call	calculate_dma_addr
	jc	exit
	mov	dma_seg,ax
	call	do_partn_save
exit:	int	20h		;Do not return.  Terminate to DOS.
show_help:
	lea	dx,msg_mir_help
	call	pr_text
	jmp	exit
unload_req:
	call	unload_tracker
	jmp	exit
ret_to_mirror:
	pop	es
	pop	ds
	cld
	sti
	ret
save_partition ENDP
;
dummy_iret:
	iret
;
;------------------------------------------------------------------
unload_tracker PROC NEAR
	lea	dx,msg_crlf
	call	pr_text
	lea	dx,dummy_iret
	mov	ax,2523h		;Take Ctrl-C vector, to disable it.
	int	21h
	call	chk_dtrk_resident
	or	ax,ax
	jnz	unload2 		;It is resident.
	lea	dx,msg_not_resident
	jmp	unload_done
unload_cannot:
	lea	dx,msg_cannot_unload
	jmp	unload_done
unload2:
	mov	bx,ax			;Save resident segment.
;
	mov	es,bx
	cld
	lea	di,saved25
	call	fetch_saved_vector
	lea	di,saved26
	call	fetch_saved_vector
	lea	di,dos_vector
	call	fetch_saved_vector
	lea	di,saved19
	call	fetch_saved_vector
	lea	di,saved2F
	call	fetch_saved_vector
;
	lea	si,int19_service
	mov	di,19h
	call	compare_vector
	jnz	unload_cannot
	lea	si,int2F_service
	mov	di,2Fh
	call	compare_vector
	jnz	unload_cannot
	lea	si,dos_intercept
	mov	di,21h
	call	compare_vector
	jnz	unload_cannot
	cmp	saved25+2,-1
	je	unload_21	;We didn't hook 25h & 26h, so don't check.
	lea	si,int25_patch
	mov	di,25h
	call	compare_vector
	jnz	unload_cannot
	lea	si,int26_patch
	mov	di,26h
	call	compare_vector
	jnz	unload_cannot
	lea	si,saved25
	mov	di,25h
	call	restore_vector
	lea	si,saved26
	mov	di,26h
	call	restore_vector
unload_21:
	lea	si,dos_vector
	mov	di,21h
	call	restore_vector
	lea	si,saved19
	mov	di,19h
	call	restore_vector
	lea	si,saved2F
	mov	di,2Fh
	call	restore_vector
	mov	es,bx
	mov	ax,es:2Ch		;Segment of resident environment.
	or	ax,ax
	jz	unload_free_prog
	dec	ax
	mov	es,ax			;MCB of that environment seg.
	inc	ax
	cmp	bx,es:1 		;Owner of env = our resident segment?
	jne	unload_free_prog
	mov	es,ax
	mov	ah,49h			;Free the resident environ block.
	int	21h
unload_free_prog:
	mov	es,bx
	mov	ah,49h			;Free the resident program block.
	int	21h
	jc	unload_cant_step
	lea	dx,msg_unloaded
unload_done:
	call	pr_text
	ret
unload_cant_step:
	jmp	unload_cannot
unload_tracker ENDP
;
;-----------------------------------------------------------
; Copy a saved-vector from our resident module.
;
; On entry: ES:DI points to a resident dword saved vector.
;
; Destroys AX.
;
fetch_saved_vector PROC NEAR
	mov	ax,es:[di]
	mov	[di],ax 	;Same offset in transient program.
	mov	ax,es:[di+2]
	mov	[di+2],ax
	ret
fetch_saved_vector ENDP
;
;------------------------------------------------------------------
; On entry: BX:SI=correct address of our resident handler,
;	    DI=vector number.
;
; On exit: if vector is correct then ZF=true, else ZF=false.
;
; Destroys AX,DI.
;
compare_vector PROC NEAR
	push	es
	xor	ax,ax
	mov	es,ax
	shl	di,1
	shl	di,1
	cmp	si,es:[di]
	jne	compv2
	cmp	bx,es:[di+2]
compv2: pop	es
	ret
compare_vector ENDP
;
;------------------------------------------------------------------
; On entry: DS:SI points to our dword saved vector, DI = vector number.
;
; Destroys AX,SI,DI.
;
restore_vector PROC NEAR
	cli		;Disable IRQ.
	push	es
	xor	ax,ax
	mov	es,ax
	cld
	shl	di,1
	shl	di,1
	cmp	word ptr [si+2],-1
	je	restv_end		;Not hooked.  Leave vector alone.
	movsw
	movsw
restv_end:
	pop	es
	ret
restore_vector ENDP
;
;------------------------------------------------------------------
; Fetch & save the partition tables.
;
do_partn_save PROC NEAR
	stc
	mov	dx,80h
	mov	ah,8		;Get drive parms.
	int	13h
	jc	fail_stp
	or	ah,ah
	jnz	fail_stp
	cmp	dl,0		;Do any hard drives exist?
	jg	drives_exist
null:	lea	dx,msg_ps_null	;No hard drives - no partitions.
	call	pr_text
fail_stp:
	jmp	fail
read_error:
	lea	dx,msg_ps_readerr
	call	pr_text
	jmp	fail_stp
drives_exist:
	cmp	dl,(max_drvs)
	ja	read_error
	mov	phys_drives,dl
	mov	drive,80h
	lea	bx,drive_parms
get_hard_parms:
	push	bx
	mov	dl,drive
	mov	ah,8
	int	13h
	pop	bx
	jc	read_error
	or	ah,ah
	jnz	read_error
	mov	dl,drive
	call	store_parms
	lea	bx,[bx] + SIZE drive_parm_str
	mov	al,drive
	inc	al
	mov	drive,al
	and	al,7Fh
	cmp	al,phys_drives
	jb	get_hard_parms	;Check the next drive.
got_parms:
;
	mov	al,phys_drives
	mov	ah,0
	mov	header.h_drives,ax
	mov	ah,2Ah		;Get current date (we include it in the file).
	int	21h
	mov	header.h_year, cx
	mov	header.h_month, dh
	mov	header.h_day, dl
	mov	ah,2Ch		;Get time.
	int	21h
	mov	header.h_hour, ch
	mov	header.h_min, cl
;
	xor	bx,bx
	mov	ax,3306h	;Ask DOS 5 for true DOS version
	int	21h
	mov	ax,bx
	or	ax,ax
	jnz	store_ver
	mov	ah,30h		;Get DOS version.
	int	21h
store_ver:
	xchg	al,ah
	mov	header.h_dos_ver, ax
	mov	byte ptr header.h_control, 0
	cmp	ax,400h
	jb	boot_or_not
	or	byte ptr header.h_control, bit_has_boot
boot_or_not:
	stc
	mov	bx,-1
	mov	ah,0C0h
	int	15h
	jc	setup
	or	ah,ah
	jnz	setup
	cmp	bx,-1
	je	setup
	mov	al,es:[bx+2]	;Machine ID byte.
	cmp	al,0FCh
	je	yes_ps2 	;Well, it's possibly a PS/2.  Good enough.
	cmp	al,0F9h 	;Convertible, PCjr, model 30, XT or PC?
	jae	setup		;Definitely not a PS/2.
yes_ps2:
	or	header.h_control, bit_ps2_machine
setup:	mov	dma_offset,0
	les	di,dma_addr
	cld
	mov	cx,(2* 200h) / 2
	xor	ax,ax
	rep stosw		;Clear 1st two sectors of buffer.
;
	mov	list_pointer,(offset_of_lists)
	mov	ax,(offset_of_data)
	mov	dma_offset,ax
	mov	header.h_file_size, ax
	lea	ax,drive_parms
	mov	parm_ptr,ax
	mov	partn_count,0
	mov	drive,80h		;Always start with drive 80h.
;
; Arrive here once for each physical drive.
;
fetch_lp0:
	mov	al,drive
	and	ax,7Fh
	mov	bx,ax
	shl	bx,1			;Index h_list_pointers table of words.
	mov	ax,list_pointer
	mov	header.h_list_pointers [bx], ax
	mov	ax,-1
	mov	master_flag,al
	mov	last_p_rec_ptr,ax	;No 'last partn record' for master.
	mov	head,0
	mov	cx0,1		;Begin with the master partition record.
	jmp	short fetch_lpe
fetch_lp1:
	mov	master_flag,0		;After 1st, it's not a master record.
fetch_lpe:
	mov	dl,drive
	mov	dh,head
	mov	cx,cx0
	les	bx,dma_addr
	mov	ax,0201h	;Read one sector.
	int	13h
	jnc	read_ok
read_err:
	lea	dx,msg_ps_readerr
	call	pr_text
	jmp	fail
read_ok:
	call	check_partn	;Returns CF & vars BOOT_ENTRY_PTR etc.
	jnc	seems_ok
	lea	dx,msg_ps_bad_partn
	call	pr_text
	jmp	fail
seems_ok:
	mov	ax,list_pointer 	;List item we are about to update.
	mov	di,ax
	xchg	di,last_p_rec_ptr	;Last previous partition record.
	cmp	di,-1			;'Last' = -1 (none)?
	je	seems_ok2		;Leave link=0, in previous item.
;
; Update the previous list_structure with a pointer to this
; current one, providing a forward link for REBUILD to follow.
;
	mov	es:[di].l_ext_l,ax	;Save forward link to ext partn item.
;
seems_ok2:
	call	update_list_vars	;Uses LIST_POINTER, not DI.
	test	master_flag,0FFh	;Just fetched the master partn record?
	jz	maybe_get_boot		;No.
	test	header.h_control, bit_ps2_machine
	jz	maybe_get_boot
	mov	dl,drive
	mov	dh,0
	mov	cx,2
	mov	bx,dma_offset
	mov	ax,0201h	;Read the PS/2 drive parameter sector.
	int	13h
	jc	read_err
	lea	di,[bx+2]
	lea	si,ibm_parm_id	;"IBM HARDFILE TYP"
	push	cx
	mov	cx,ibm_parm_length
	cld
	repe cmpsb
	pop	cx
	jne	maybe_get_boot
	call	update_list_vars	;Keep the sector data.
maybe_get_boot:
	test	byte ptr header.h_control, bit_has_boot
	jz	maybe_another_ext
	mov	di,dos_entry_ptr
	or	di,di
	jz	maybe_another_ext
	mov	cx, es:[di].partn_cx
	mov	dh, es:[di].partn_head
	mov	dl,drive
	mov	bx,dma_offset
	mov	ax,0201h		;Read one sector.
	int	13h
	jnc	read_boot_ok
	jmp	read_err
read_boot_ok:
	call	update_list_vars
maybe_another_ext:
	mov	di,ext_entry_ptr
	or	di,di
	jz	next_drive		;No more ext's on this phys drive.
	mov	al, es:[di].partn_head	;Yes, there is extended partn.
	mov	head,al
	mov	ax, es:[di].partn_cx
	mov	cx0,ax
next_partn:
	inc	partn_count
	cmp	partn_count,26
	jae	too_many_partns
	jmp	fetch_lp1	;Examine the extended partition table.
too_many_partns:
	lea	dx,msg_ps_too_many
	call	pr_text
	jmp	fail
;
next_drive:
	mov	al,drive
	inc	ax
	mov	drive,al
	and	al,7Fh
	cmp	al,phys_drives
	jae	done_reading
	add	list_pointer, SIZE list_structure ;One null entry after drive.
	add	parm_ptr, SIZE drive_parm_str		;Parms for next drive.
	jmp	fetch_lp0
;
; Copy header into buffer, then copy drive_parm structures.
;
done_reading:
	lea	si,header
	xor	di,di			;Header goes to offset 0 in the file.
	cld
	mov	cx,SIZE header_structure
	rep movsb
	mov	di, header.h_parm_ptr
	lea	si,drive_parms
	mov	cx,(max_drvs * (SIZE drive_parm_str))
	rep movsb
	mov	word ptr es:[0].h_id, 20CDh
;
; We have built the save_file image in memory.	Now write it to diskette.
;
prompt_for_drive:
	call	ask_for_floppy	;Updates variable FLOPPY.
	mov	dl,floppy
	inc	dl
	mov	ax,36FFh	;Get disk free space.
	int	21h
	cmp	al,0FFh
	je	prompt_for_drive
	mul	cx
	mul	bx
	or	dx,dx
	jnz	create_psav_file	;At least 64k bytes are available.
	cmp	ax, header.h_file_size	;(3+26+26)*512
	jb	prompt_for_drive	;Not enough room for our file.
;
create_psav_file:
	mov	al,floppy
	add	al,"A"
	mov	filename,al
	lea	dx,filename
	mov	cx,0
	mov	ah,3Ch		;CREATE file.
	int	21h
	mov	handle,ax
	jnc	created_ok
	jmp	short file_error
created_ok:
	mov	bx,handle
	mov	cx, header.h_file_size
	push	ds
	mov	ds,dma_seg
	xor	dx,dx
	mov	ah,40h		;WRITE to file.
	int	21h
	pop	ds
	cld
	jc	file_err_close
	cmp	ax,cx
	jne	file_err_close
success:
	mov	bx,handle
	mov	ah,3Eh		;CLOSE file.
	int	21h
	jc	file_error
	lea	dx,msg_ps_success
	call	pr_text
done:	ret
fail:	nop
	jmp	done
file_err_close:
	mov	bx,handle
	mov	ah,3Eh		;CLOSE.
	int	21h
file_error:
	lea	dx,msg_ps_file_err
	call	pr_text
	jmp	fail
do_partn_save ENDP
;
;=======================  Procedures  ============================
;
; On entry: ES:BX points to a partn sector.
;
; On exit: if any of the four entries are invalid then CF=true.  Otherwise,
;	   CF=false and DOS_ENTRY_PTR and EXT_ENTRY_PTR are updated.
;
; AX is destroyed.
;
check_partn PROC NEAR
	push	cx
	push	dx
	push	di
	cmp	word ptr es:[bx+1FEh], 0AA55h	;Proper signature?
	jne	chk_partn_err			;No.
	mov	di,1BEh ;Offset of table in the sector.
	xor	dx,dx	;Partition counters: DH=bootable DOS, DL=extended.
	mov	dos_entry_ptr,dx
	mov	ext_entry_ptr,dx
chk_partn_lp:
	mov	ah, es:[bx+di].partn_sys
	or	ah,ah				;System ID = unused (0)?
	jz	chk_partn2
	mov	al,es:[bx+di].partn_boot
	cmp	al,0
	jz	chk_partn1
	cmp	al,80h		;Boot indicator must be 0 or 80h.
	jne	chk_partn_err
	test	master_flag,0FFh	;Currently examining master partn?
	jz	chk_partn_err	;Only master record can have bootable partn.
	inc	dh		;Increment counter of bootable DOS partns.
chk_partn1:
	mov	al, es:[bx+di].partn_sys
	cmp	al,1		;DOS 12-bit FAT?
	je	chk_partn_dos
	cmp	al,4		;DOS 16-bit FAT?
	je	chk_partn_dos
	cmp	al,6		;DOS huge partition?
	je	chk_partn_dos
	cmp	al,5		;DOS extended partition?
	je	chk_partn_extd	;Yes.
	test	master_flag,0FFh	;Currently examining master partn?
	jz	chk_partn_err		;No.  Strange ID is illegal elsewhere.
	jmp	short chk_partn_next	;Ignore strange System ID in master.
chk_partn_extd:
	inc	dl
	lea	ax,[bx+di]		;It's an 'extended' partn entry.
	mov	ext_entry_ptr,ax
chk_partn2:
	cmp	byte ptr es:[bx+di].partn_boot, 0	;Marked bootable?
	jz	chk_partn_next				;No, so it's OK.
chk_partn_err:
	stc				;CF=true for error.
	jmp	short chk_partn_end
chk_partn_dos:
	call	check_partn_end
	jc	chk_partn_err
	lea	ax,[bx+di]
	mov	dos_entry_ptr,ax
chk_partn_next:
	add	di,SIZE partn_struc
	cmp	di,1FEh
	jae	chk_partn_finish
	jmp	chk_partn_lp
chk_partn_finish:
	cmp	dh,1		;More than one bootable DOS partition?
	ja	chk_partn_err	;Yes.  Error.
	cmp	dl,1		;More than one extended partition?
	ja	chk_partn_err	;Error.
	mov	al,dl
	clc
chk_partn_end:
	pop	di
	pop	dx
	pop	cx
	ret
check_partn ENDP
;
;--------------------------------------------------------------------
; On entry: ES:[BX+SI] points to one normal (#1,4,5,6) partn-table entry.
;	    PARM_PTR is assumed valid.
;
; On exit: if parition-end seems in legal range then CF=false, else CF=true.
;
; Only AX is changed.
;
check_partn_end PROC NEAR
	push	si
	mov	si,parm_ptr		;Parms for this physical drive.
	mov	al, byte ptr es:[bx+di].partn_end_cx
	mov	ah, es:[bx+di].partn_end_head
	and	ax,0C0C0h
	rol	ah,1
	rol	ah,1
	shl	ax,1
	shl	ax,1
	mov	al, byte ptr es:[bx+di].partn_end_cx + 1
	cmp	ax, [si].drive_max_cyl
	ja	chk_pend_err
	cmp	byte ptr es:[bx+di].partn_sys, 5	;Type = 'extended'?
	je	chk_pend_ok				;Skip (FDISK bug).
	mov	ah, es:[bx+di].partn_end_head
	and	ah,3Fh
	cmp	ah, [si].drive_max_head 	;Is end head # in range?
	ja	chk_pend_err
	mov	al, byte ptr es:[bx+di].partn_end_cx
	and	al,3Fh
	cmp	al, [si].drive_max_sector
	jbe	chk_pend_ok
chk_pend_err:
	stc
	jmp	short chk_pend_ret
chk_pend_ok:
	clc
chk_pend_ret:
	pop	si
	ret
check_partn_end ENDP
;
;----------------------------------------------------------------
; On entry: CX & DH are intact from INT 13h function 8.
;	    DL = drive number.
;	    DS:BX --> our drive parm structure to be filled in.
;
; Destroys AX.
;
store_parms PROC NEAR
	mov	[bx].drive_number, dl
	mov	al,dh
	and	al,3Fh
	mov	[bx].drive_max_head, al
	mov	ax,cx
	and	al,3Fh
	mov	[bx].drive_max_sector, al
	mov	al,cl
	mov	ah,dh
	and	ax,0C0C0h	;Support more cyl bits in d7,d6 of DH.
	rol	ah,1		;Max cylinder cound be up to 4095.
	rol	ah,1
	shl	ax,1
	shl	ax,1
	mov	al,ch
	mov	[bx].drive_max_cyl, ax
	ret
store_parms ENDP
;
;-----------------------------------------------------------------
; Update the sector-list table & our pointers after
; a sector has been added to the memory image.
;
; On entry: CX & DX set same as for Bios INT 13h.  ES= buffer segment.
;	    BX= file offset of the sector.
;
; Destroys AX.
;
update_list_vars PROC NEAR
	push	di
	mov	di,list_pointer
	mov	es:[di].l_cx, cx
	mov	es:[di].l_dx, dx
	mov	es:[di].l_offset, bx
	mov	ax,512				;Advance the pointers.
	add	header.h_file_size, ax
	add	dma_offset,ax
	add	list_pointer, SIZE list_structure	;Post-increment.
	pop	di
	ret
update_list_vars ENDP
;
;------------------------------------------------------------------
; Locate a segment where DMA transfers are 'safe'
; (the must not cross 64k physical boundaries).
;
; On exit: if CF=false then AX = segment for Bios DTA.
;	   If CF=true, then there isn't enough memory.
;
; Destroys AX,BX,DX.
;
calculate_dma_addr PROC NEAR
	lea	bx,end_prog
	lea	bx,[bx+16]
	mov	cl,4
	shr	bx,cl
	mov	dx,cs			;Use CS, since we are .COM file.
	add	dx,bx			;DX:0000 points after END_PROG.
calc_dma_lp:
	mov	ax,dx
	mov	bx,dx
;
; Room for:
;  header + index + parm sector + 26 partn records + 26 logical boot records.
;
	add	ax, ((3+26+26) * 512) / 16	;Paragraphs needed.
;
	cmp	ax,cs:2 		;CS=PSP.  PSP:2 is 'top_of_memory'.
	jae	calc_dma_small		;Beyond.  Not enough memory.
	and	ax,0F000h		;Isolate the 64k physical parts.
	and	bx,0F000h
	cmp	ax,bx			;Start & End are in the same bank?
	je	calc_dma_ok		;Yes.
	add	dx,512/16		;No.  Bump up addr by one sector size.
	jmp	calc_dma_lp		;Try again, with new starting_seg.
calc_dma_small:
	lea	dx,msg_ps_small_mem
	call	pr_text
	stc
	jmp	short calc_dma_exit
calc_dma_ok:
	mov	ax,dx
	clc
calc_dma_exit:
	ret
calculate_dma_addr ENDP
;
;------------------------------------------------------------
; Ask the user for the name of the floppy drive to
; write the partition info to.	Returns variable FLOPPY.
;
; Destroys AX,DX.
;
ask_for_floppy PROC NEAR
	mov	floppy,0	;Drive A is always the default.
	lea	dx,msg_ps_floppy
	call	pr_text
ask_flop0:
	lea	dx,msg_ps_drv_query
	call	pr_text
	mov	ax,0C00h	;Flush DOS keyboard.
	int	21h
	mov	line_input,4
	mov	line_input+1,0
	lea	dx,line_input
	mov	ah,0Ah		;DOS console line input.
	int	21h
	mov	dl,cr
	mov	ah,2
	int	21h
	mov	dl,lf
	mov	ah,2
	int	21h
	cmp	line_input+1,0
	jz	ask_flop_done	;Null input.  Use drive A.
	mov	al,line_input+2
	call	uppercase
	sub	al,"A"
	jb	ask_flop0
	mov	floppy,al	;Save it.  Now check if it's valid.
	mov	ah,19h		;Get current drive.
	int	21h
	push	ax		;Save current drive.
	mov	dl,floppy
	mov	ah,0Eh		;Set default drive to specified one.
	int	21h
	mov	ah,19h		;Fetch it again, to check.
	int	21h
	pop	dx		;Recover original default drive.
	push	ax		;Save resulting drive from last 19h.
	mov	ah,0Eh		;Restore original default drive.
	int	21h
	pop	ax
	cmp	al,floppy	;Did the Set succeed?
	jne	ask_flop0	;No.  Ask again.
ask_flop_done:
	ret
ask_for_floppy ENDP
;
;-----------------------------------------------------------
; Display text message at DS:DX, ending in a $.
;
pr_text PROC NEAR
	push	ax		;All regs unchanged.
	mov	ah,9
	int	21h
	pop	ax
	ret
pr_text ENDP
;
;-----------------------------------------------------------
skipb	PROC NEAR		;Skip over blanks at DS:SI.
	mov	al,[si]
	cmp	al," "
	jne	skipb_ret
	inc	si
	jmp	skipb
skipb_ret:
	ret
skipb	ENDP
;
;-----------------------------------------------------------
; Examine parms at DS:80h.
;
; On return: ZF=true means nothing for this module to do (AL=0), or
;	     ZF=false means something to do and AL = partn_opt_xxx bits.
;
; Destroys: AX,BX,CX,SI,DI,ES.
;
look_for_option PROC NEAR
	push	ds
	pop	es
	cld
	xor	bx,bx
	mov	cl,ds:80h	;Command line length, from DOS.
	and	cx,007Fh
	mov	si,81h
	cmp	cl,2
	jb	lfo_none
lfo_caps:
	mov	al,[si]
	call	uppercase
	mov	[si],al
	inc	si
	cmp	al,"?"
	je	lfo_help
	loop	lfo_caps
	mov	byte ptr [si],0Dh
	jmp	short lfo_1
lfo_help:
	mov	bl,opt_help
	jmp	short lfo_end
;
lfo_1:	mov	si,81h
	call	skipb
	jb	lfo_none
	cmp	al,"/"
	jne	lfo_none
	lea	di,option_text1
	mov	cx,option_length1
	lea	ax,[si]
	repe cmpsb
	je	lfo_partn
	mov	si,ax
	lea	di,option_text2
	mov	cx,option_length2
	repe cmpsb
	je	lfo_unload
	jmp	short lfo_none
lfo_unload:
	mov	bl,opt_unload
	jmp	short lfo_term
lfo_partn:
	mov	bl,opt_partn
lfo_term:
	cmp	byte ptr [si]," "	;Option is properly terminated?
	ja	lfo_err
	jmp	short lfo_end
lfo_none:
	mov	bl,0
	jmp	short lfo_end
lfo_err:
	lea	dx,msg_ps_bad_parm
	call	pr_text
	jmp	lfo_help
lfo_end:
	mov	al,bl
	and	ax,00FFh
	ret
look_for_option ENDP
;
option_text1	DB	"/PARTN"
option_length1	EQU	$ - option_text1
;
option_text2	DB	"/U"
option_length2	EQU	$ - option_text2

;
;-----------------------------------------------------------
;
CODE	ENDS
	END

