; Copyright 1990 Central Point Software, Inc. 
; All rights reserved.
;---------------------------------------------------------
;  MAIN MODULE of "Unformat".
;  (see _ORG module source for linking info)
;  A utility to attempt recovery from
;  an accidental hard disk "Format".
;
;  Written by GWD.  June - August 1987.
;  Revised 9-29-87.
;  Upgraded for DOS 3.31 huge partitions, etc. 12-23-87.
;  Now uses 'include' file.
;  Shortened a little.	3-31-88.
;  Deals with changed DOS 4.xx DPB structure.  7-29-88.
;  Uses new, powerful 'display' procedure.  8-5-88.
;  Amended for new Restore_partition function.	v5.00  10-31-88.
;  Improved INT25h protocol determination.  v5.00  11-18-88.
;  New partition display function (/PARTN /L) added.  5.00 12-6-88.
;  Refuses to run under 5.x DOS.  5.10	1-4-89.
;  Cosmetic.  In walk, bad sub is now subtracted from path shown.  2-3-89.
;  Cosmetic.  Show_dir_info now shows time=0 as 12:00am, not 0:00am.  3-21-89.
;  Check_cluster proc now accepts 'de_zero' OR 'de_zapped' entries.  3-21-89.
;  Now checks for network drives (IBM & Novell).  5-1-89.
;  Added international support (date & time displays).	5-23-89.
;  Added display of names of INT 25h errors.  02-14-90	6.0 BETA.
;  READ_DISK, WRITE_DISK now return true error code.  02-15-90	6.0 BETA.
;
;       M000 MD 9/23/90         Removed display of copyright message
;       M001 MD 10/14/90        Removed display of no action message with help
;       M002 MD 10/29/90        Exit correctly when Mirror file not found
;
;----------------------------------------------------------
;
	.LALL			;List all macro def's.
prog	SEGMENT public para
	ASSUME	CS:prog, DS:prog
cr	EQU	13
lf	EQU	10
;
asm_message MACRO dy1,dy2,dy3,dy4,dy5,dy6
	%OUT	*MSG: dy1&dy2&dy3&dy4&dy5&dy6
	ENDM

	 PUBLIC PROGRESS
	 PUBLIC START
	 PUBLIC CLUSTER_INDEX
	 PUBLIC STACK_END_PTR
	 PUBLIC CLUSTER_CNT_PLUS_1
	 PUBLIC CURR_FNAME
	 PUBLIC OPTION_TABLE
	 PUBLIC HSUB_COUNT
	 PUBLIC OPTIONS

;
; The following are in the ORG module.
;
	EXTRN	top_of_mem:word, fcb1:byte, parms:byte
;;	EXTRN	banner:byte
;
; The following are in the I/O module.
;
	EXTRN	display:near, dis_word:word
	EXTRN	get_country_info:near
	EXTRN	printc:near, pr_text:near
	EXTRN	getline:near, flushkey:near, uppercase:near
	EXTRN	skipb:near, ask_trunc:near, tab:near
	EXTRN	pr_dec:near, pr_decl:near
	EXTRN	crout:near, pr_hex_word:near
	EXTRN	pr_hex_byte:near, ask_for_yes:near
	EXTRN	show_progress:near, show_dir_info:near
	EXTRN	copy_fname:near, display_options:near
	EXTRN	look_for_parms:near
;
	EXTRN	print_flags:byte, pf_allowed:abs
	EXTRN	pf_con_pe:abs, pf_ptime:abs, red_pointer:word
;
; The following are in the message (MSG) module.
;
	EXTRN	MSG_INSERT_DISK:BYTE
	EXTRN	yes_char:abs, no_char:abs, quit_char:abs
	EXTRN	msg_bad_parms:byte, msg_network:byte
	EXTRN	msg_write_fake:byte
	EXTRN	msg_warning:byte, msg_bad_drive:byte, msg_print:byte
	EXTRN	msg_dos:byte, msg_strange_disk:byte, msg_small_mem:byte
	EXTRN	msg_abort:byte, msg_no_action:byte
	EXTRN	msg_sys_read_err:byte, msg_read:byte, msg_write:byte
	EXTRN	msg_error:byte
	EXTRN	msg_examined_ent:byte
	EXTRN	msg_root_files:byte, msg_subdirs_found:byte
	EXTRN	msg_searching:byte, msg_stop_hunt:byte
	EXTRN	msg_nothing_found:byte
	EXTRN	msg_only:byte, msg_crosslink:byte
	EXTRN	msg_delete:byte, msg_trunc:byte, msg_ignoring:byte
	EXTRN	msg_walk1:byte, msg_walk2:byte, msg_write_warn:byte
	EXTRN	msg_files:byte, msg_files_rec:byte, msg_done:byte
	EXTRN	msg_help:byte, msg_using_drv:byte, msg_path:byte
	EXTRN	msg_rp_title:byte, msg_listp_title:byte
	EXTRN	msg_i25_wrprot:byte, msg_i25_unit:byte
	EXTRN	msg_i25_not_ready:byte, msg_i25_bad_cmd:byte
	EXTRN	msg_i25_crc:byte, msg_i25_req:byte
	EXTRN	msg_i25_seek:byte, msg_i25_media:byte
	EXTRN	msg_i25_rnf:byte, msg_i25_paper:byte
	EXTRN	msg_i25_writef:byte, msg_i25_readf:byte
	EXTRN	msg_i25_general:byte
;
	EXTRN	restore_partitions:near ;In 'partn' module.
	EXTRN	list_partitions:near
;
	EXTRN	j_rebuild:near		;In Jim's 'Rebuild' module.
;
	EXTRN	last_byte:byte		;In 'last' module.
;
	INCLUDE UF_INCL.INC
;
;--------------------------------------------------------------------------
; This is used to translate the INT 25h error codes into text.
;
i25_error_struc STRUC
i25_err_code	DB	-1
i25_err_ptr	DW	0	;Offset of asciiz message text.
i25_error_struc ENDS
;
;--------------------------------------------------------------------------
; The following are copied from the Disk Parm Block.
; They must remain contiguous & in this order!
;
dpb_start EQU $
;
drive		DB	-1	;0=A, 1=B, 2=C, etc.
		DB	?
sector_size	DW	?	; Bytes.
cluster_mask	DB	?	; [sectors/cluster] -1.
		DB	?	; Log2 [sectors/cluster].
fat_1st_sector	DW ?
fat_count	DB	?
root_entries	DW	?
first_data_sector DW ?
cluster_cnt_plus_1 DW	?	;This is also the max allowed cluster number.
fat_size	DW	0		;In sectors.  A byte, before DOS 4.xx.
dpb_length1 EQU ($-1) - dpb_start	;Include only the lo byte of FAT_SIZE.
dpb_mid EQU $
dir_1st_sector	DW ?
		DW	?,?	;Driver address.
media		DB	0	;F8h for a hard disk.
		DB	?	;-1 mefore media chk.
		DW	?,?	;Ptr to next DPB.
		DW	?	;Last allocated cluster.
		DW	?	;Free clusters.
;
dpb_length2 EQU $ - dpb_mid	;Excludes FAT_SIZE.
;
;------------------------------------------------------------------
	EVEN
root_sectors DW ?	;Derived from the DPB above.
cluster_size DW ?	;In sectors.
clusters DW	?	;Total clusters on disk, not count+1.
dirs_per_cluster DW ?	;# of dir entries that will fit in 1 subdir cluster.
fat16bit DB	0	;NZ means FAT is 16-bit type, 0 means 12-bit type.
;
;---------------------------------------------------------------------
; User-selected disposition of fragmented files.
;
frag_opt DB	"?"
;
;	"D"	Delete this one file, ask again for next.
;	"d"	Delete all such files, don't ask again.
;	"T"	Truncate (shorten) this file, ask again.
;	"t"	Truncate all such files, don't ask again.
;
; Caution - these are not global equates.  They are defined
; in this module and again, identically, in the I/O module.
;
;----------------------------------------------------------------------
; Flags derived from the command-line parameters.
;
options DW	0	;See include-file OPT_xxxxx equates.
;
;----------------------------------------------------------------------
; Miscellanious variables.
;
dos_ver DW	0	;E.g., for DOS 3.20, =0314h.
progress DW	0	;Percentage of disk 'hunt' completed.
;
free_root_entries DW 0	;Count.
free_root_entry DW 0	;Offset of first free (unused) root entry.
file_counter DW 0
rootsub_count DW 0	;Root-level subdirs.
hsub_count DW	0	;Subdirs found during HUNT (any level).
repeat	DW	0	;Used as loop counter in various places.
root_offset DW	0	;Offset of the root entry currently being processed.
cluster_index DW 0	;Selects a disk cluster.
;
;-----------------------------------------------------------------
; Variables used by directory tree walker.
;
tree_struc STRUC
tcluster0 DW	?	;First cluster of current directory (special: 0=root).
tcluster DW	?	;Current cluster of the current directory.
toffset DW	?	;Offset of an entry in the current cluster.
tree_struc ENDS
;
;
tree_level DW	-1	;Offset into TREE.
;
tree_size EQU	33	;Max allowed depth of subdirectory tree.
tree	LABEL word
	tree_struc tree_size DUP( <0,0,0> )
;
pathdrv DB	"Q:"
;
path	DB	66 DUP(0)	;ASCIIZ path name (e.g. "\SUBDIR.1\PROGS").
path_guard DB	-1
;
curr_fname DB	"current_.fil",0	;Temp storage for ASCIIZ file name.
;
clu_in_buffer DW -1	;# of cluster now in the cluster-segment (-1=none).
clu_dirty DB	0	;0=data unchanged.  NZ=data must be rewritten to disk.
;
;------------------------------------------------------------------------
;  This controls the protocol used for INT 25h/26h.
;
i25_protocol	DB	i25p_unknown
i25p_unknown	EQU	"?"		;Use old method.
i25p_old	EQU	"o"		;Use old method.
i25p_new	EQU	"n"		;Use NEW method.
;
; Parm block for DISK_READ and DISK_WRITE.  Keep together & in order!
	EVEN
sector_lo	DW	0
sector_hi	DW	0
sector_count	DW	1
dta		DW	0
dta_seg 	DW	0F000h
;
;-------------------------------------------------------------------------
fat_para DW	0	;Paragraphs required for FAT when in 16-bit format.
;
fat_seg DW	?	;Segments are determined at run-time, of course.
dir_seg DW	?
cluster_seg DW	?	;Room for 1 cluster only.  Used when hunting/walking.
stack_end_ptr DW ?	;Offset in CS (& SS) of free space beyond our stack.
;
prog_size DW	OFFSET last_byte	;Space used by the load module.
stack_size EQU	300
;
;
;=====================	Program Code begins  =======================
;
start:	nop
	cld
	sti
	mov	bx,prog_size
	lea	bx,[bx+stack_size]
	lea	sp,[bx]
	add	bx,31
	and	bl,0F0h
	mov	stack_end_ptr,bx
	xor	bx,bx
	push	bx
	mov	bp,sp
	xor	al,0FFh ;If drive is valid then AL becomes FF, else 0.
	and	fcb1,al ;If no drive was specified, FCB1 was already 0.
;
	lea	di,parms
	mov	bl,[di]
	inc	di
	mov	bh,0
	mov	[bx+di],bh	;Make sure parmline ends with a 0.
	mov	cx,bx
	cmp	cl,2
	jb	start2

	mov	al,"?"
	cld
	repne scasb		;Search parmline for "?".
	jne	start2		;For now, ignore other parms.

;	call	show_banner             ;M000
	jmp	short show_help

start2:
	MOV	AX,3306H		; get true DOS version
	MOV	BX,0			;   ala DOS 5.0
	INT	21H
	CMP	BL,5			; function supported?
	JB	START3
	MOV	AX,BX
	JMP	SHORT START4

START3:
	mov	ah,30h		;Get DOS version.
	int	21h

start4:
	xchg	al,ah
	mov	dos_ver,ax
;	cmp	ax,600h         RI-Don't need to check upper dos version
;	jae	wrong_dos
	cmp	ax,200h
	jae	dos_ok

wrong_dos:
	lea	dx,msg_dos
	call	pr_text
	jmp	exit

dos_ok: xor	ax,ax
	mov	options,ax	;Init all parm switches OFF.
	mov	print_flags,al
;
	call	get_country_info
;
	lea	si,parms+1	;Examine the command line.
	call	look_for_parms
         jc    bad_parms

         test  options,opt_j            ; if /J specified
	jz 	parms_ok                 ; that better be all 
         cmp   options,opt_j            ; that is specified
         je    parms_ok

bad_parms:
	lea	dx,msg_bad_parms
	call	pr_text
         jmp   exit                     ; don't show help message on bad cmd line

show_help:
	lea	dx,msg_help
	call	pr_text
	jmp	exit                    ;M001

bad_drive:
;	call	show_banner             ;M000

bad_drive2:
	lea	dx,msg_bad_drive
	call	pr_text
	jmp	exit

network_drive:
;	call	show_banner             ;M000
	lea	dx,msg_network
	call	pr_text
	jmp	exit

parms_ok:
	mov	ax,options
	test	ax, opt_partn
	jz	not_partn
;
; User has selected the /PARTN option, & possibly the /L (list) option.
;
;	call	show_banner             ;M000
	test	ax, NOT (opt_wrfake OR opt_partn OR opt_list)
	jnz	bad_parms
	lea	dx,msg_rp_title 	;"Partition Table restoration"
	test	ax, opt_list
	jz	show_partn_title
	lea	dx,msg_listp_title	;"Partition Table display"

show_partn_title:
	call	pr_text
	call	display_print_opt		;(If no /P, does nothing.)
	test	print_flags, pf_allowed 	;Was /P option selected?
	jz	do_partn			;No.
	mov	al,pf_con_pe
	or	al,pf_ptime			;Combine two extrn symbols.
	or	print_flags,al			;Activate printing now.

do_partn:
	call	display_options
	test	options, opt_list
	jnz	do_list_partn
	call	restore_partitions	;In _PARTN module.
	jmp	exit

do_list_partn:
	call	list_partitions 	;In _PARTN module.
	jmp	exit
;
; For cases other than /PARTN, we require a valid drive specifier.
;
not_partn:
	mov	al,fcb1
	cmp	al,0
	jz	bad_drive
	dec	al
	mov	drive,al
	add	al,"A"
	mov	pathdrv,al
;;	 MOV   INSERT_DRV,AL

	call	check_network_drive
	jc	network_drive

	 CALL  HAVE_DISKETTE_INSERTED

	mov	bx,options
	test	bx, NOT opt_j	;Any parms other than /J ?
	jnz	do_unformat	;Yes, must be meant for us.  Skip Jim's stuff.
	test	print_flags, pf_allowed
	jnz	do_unformat	;Print options means it's for us.

do_j:	mov	ah,0		;Assume no option.
	test	bx, opt_j
	jz	do_j2
	mov	ah,1		;Inform j_rebuild of the /J option.

do_j2:
	mov	bx,stack_end_ptr
	mov	al,drive
	nop
	call	j_rebuild	;Try Jim's Rebuild first.
	nop
	push	cs
	pop	ds
	push	cs
	pop	es
	cld
	sti
	mov	bp,sp
	cmp	al,1		;Jim's side completed successfully?
	je	j_exit		;Yes.  Nothing more to do.
        cmp     al,2            ;User terminated?       M002
        je      j_exit          ;                       M002
	cmp	al,6		;Panic exit?
	je	j_exit
	test	options, opt_j	;Jim's option?
	jnz	j_exit		;Yes.  Not for us.

;       mov	ah,0Fh		;Get video mode into AL.
;	int	10h
;	and	al,7Fh		;Clear the EGA keep_screen bit.
;	mov	ah,0		;Re-init, to same mode (erases screen).
;	int	10h

;  gotta first find how many lines to clear

        MOV   CX,184FH
        XOR   DX,DX
        MOV   AX,1130H
        XOR   BH,BH
        PUSH  CX
        INT   10H
        POP   CX

        CMP   DX,18H
        JBE   CLEAR_SCREEN
        MOV   CH,DL

CLEAR_SCREEN:
        MOV   AX,0600H
        MOV   BH,7
        MOV   DX,CX
        MOV   CX,0
        INT   10H                      ; clear screen
        MOV   AH,2
        MOV   DX,0
        MOV   BH,0
        INT   10H                      ; move cursor to 0,0

	jmp	short do_unformat
j_exit: jmp	exit
;
;-------------------------------------------------------------------
do_unformat:
;	call	show_banner     ;M000
	lea	dx,msg_warning
	call	pr_text
	mov	ah,2Ah		;Get date from DOS.
	int	21h		;Returns DH=month, DL=day, CX=year.
	mov	bx,cx		;Save year into BX.
	mov	al,dl
	mov	ah,0		;Need correct date for our subdir template.
	mov	si,ax		;Build it in SI.
	mov	al,dh
	mov	cl,5
	shl	ax,cl
	or	si,ax
	sub	bx,1980
	mov	cl,9
	shl	bx,cl
	or	si,bx
	mov	subdir.date, si
	call	display_print_opt
	or	print_flags, pf_con_pe
	mov	al,drive
	add	al,"A"
	lea	dx,msg_using_drv	;"Using drive @0a:",cr,lf
	call	display
	call	ask_for_yes	;"Are you SURE?  If so, type in YES."
	jnz	no_action
	call	look_for_parms	;Slash parms are allowed after the YES.
	jnc	parms2_ok
badp_stp:
	jmp	bad_parms
no_action:
	lea	dx,msg_no_action	;"No action taken".
	call	pr_text
	jmp	exit
parms2_ok:
	mov	ax,options
	test	ax, opt_j OR opt_partn	;These are not allowed here.
	jnz	badp_stp
get_drv_parms:
	cmp	dos_ver, 314h	;3.20?
	jb	get_dpb
	mov	bl,drive
	mov	bh,0
	inc	bx
	mov	ax,4409h
	int	21h
	jc	get_dpb
	test	dx,9200h	;Network, remote or SUBST?
	jnz	strange_disk
get_dpb:
	mov	dl,drive
	inc	dl
	mov	ah,32h		;Get Drive Parm Block (DPB) for drive DL.
	int	21h		;Returns addr in DS:BX.
	cmp	al,0FFh 	;Should never happen, but check anyway.
	jne	copy_drv_parms
	push	cs
	pop	ds
	jmp	bad_drive2
;
strange_disk:
	lea	dx,msg_strange_disk
	call	pr_text
	jmp	no_action
;
copy_drv_parms:
	push	cs
	pop	es
	cld
	lea	si,[bx+1]
	mov	di,OFFSET dpb_start+1	;Don't copy the drive byte.
	mov	cx,dpb_length1-1
	rep movsb
	cmp	cs:dos_ver,400h
	jb	copy_drvp2
	movsb				;Copy the high byte of FAT_SIZE.
	jmp	short copy_drvp3
copy_drvp2:
	inc	di			;Leave high byte of FAT_SIZE = 0.
copy_drvp3:
	mov	cx,dpb_length2
	rep movsb
	push	cs
	pop	ds
	mov	al,0
	cmp	cluster_cnt_plus_1, 4085
	jbe	know_fat_type
	mov	al,16h			;Non-zero means 16-bit FAT.
know_fat_type:
	mov	fat16bit,al
;
	cmp	root_entries,4096	;Root must fit inside a 64k segment.
	jb	rt_sz_ok
strange_stp:
	jmp	strange_disk
;
rt_sz_ok:
	mov	ax,cluster_cnt_plus_1
	cmp	ax,1
	jbe	strange_stp
	mov	bx,ax
	dec	bx		;BX = cluster count.
	mov	clusters,bx
	inc	ax		;One more, for number of FAT entries (C+2).
	cmp	ax,0FFF6h
	jae	strange_stp
	add	ax,7		;Provide for rounding up.
	mov	cl,3		;8 words (FAT entries) per paragraph.
	shr	ax,cl		;AX = paragraphs for the 16-bit FAT.
	mov	bx,sector_size
	test	bx,0F1FFh	;Allow 512, 1024 and 2048 byte sectors.
	jnz	strange_stp
	mov	cl,4
	shr	bx,cl		;BX = paragraphs per sector.
	xor	dx,dx
	div	bx		;Compute for whole-sector requirement.
	call	roundup
	mul	bx		;AX = paragraphs for 16-bit FAT.
	mov	fat_para,ax
;
	mov	ax,32
	mul	root_entries	;(32 bytes/entry) * #entries.
	jc	strange_stp	;Root dir exceeds 64k bytes.
	div	sector_size
	call	roundup
	mov	root_sectors,ax ;Number of sectors in root dir.
;
	mov	al,cluster_mask
	mov	ah,0
	inc	ax
	mov	cluster_size,ax
	mul	sector_size
	mov	cx,32
	div	cx
	mov	dirs_per_cluster,ax
;
; Check available memory and setup pointers to our FAT, Dir & cluster buffers.
; Bytes needed =
;	100h + program + stack + FAT16 (maybe 128k) + root + 1 cluster.
;
	mov	bx,stack_end_ptr	;The offset base of free space.
	mov	cl,4
	shr	bx,cl		;Convert to paragraphs.
	mov	ax,cs
	add	bx,ax		;BX:0000 points at 1st free byte.
	mov	fat_seg,bx
	add	bx,fat_para
	jc	small_mem
	mov	dir_seg,bx
	mov	ax,sector_size
	mul	root_sectors
	shr	ax,cl		;CL=4.
	add	bx,ax
	jc	small_mem
	mov	cluster_seg,bx
	mov	ax,cluster_size
	mul	sector_size
	shr	ax,cl
	add	bx,ax		;BX = para just above what we need.
	jc	small_mem
	cmp	bx,top_of_mem
	jb	enough_mem
small_mem:
	lea	dx,msg_small_mem
	call	pr_text
	jmp	exit
;
enough_mem:			;Now we clear all our memory buffers.
	mov	dx,fat_seg	;First (and lowest) buffer.
	sub	bx,dx		;Compute total paragraphs for our buffers.
	xor	ax,ax
	cld
clr_all_bufs:
	mov	es,dx
	xor	di,di
	mov	cx,8		;Eight words per paragraph.
	rep stosw
	inc	dx		;Next para.
	dec	bx
	jnz	clr_all_bufs
	push	cs
	pop	es
	nop
	test	print_flags, pf_allowed
	jz	show_opts
	or	print_flags, pf_ptime		;Turn on printing, now.
show_opts:
	call	display_options
;
; Determine which INT 25h/26h protocol is required.
;
	mov	ax,dos_ver
	mov	bl,i25p_old
	cmp	ax,(3*256)+30	;Older than 3.30 DOS?
	jb	init_25p	;Always use old method.
	mov	bl,i25p_unknown
	cmp	ax,(3*256)+40	;Older than 3.40 DOS?
	jb	init_25p
	mov	bl,i25p_new	;Always use new method.
init_25p:
	mov	i25_protocol,bl
	xor	ax,ax
	mov	sector_lo,ax
	mov	sector_hi,ax
	mov	dta,ax
	inc	ax
	mov	sector_count,ax
	mov	ax,fat_seg
	mov	dta_seg,ax
try_protocol:
	call	read_disk	;See if the chosen protocol really works.
	jnc	try_ok
	cmp	al,7		;Error = unknown media ?
	jne	try_strange
	mov	al,i25p_new
	xchg	al,i25_protocol
	cmp	al,i25p_unknown
	je	try_protocol
try_strange:
	jmp	strange_disk
try_ok: cmp	i25_protocol,i25p_unknown
	jne	read_fat
	mov	i25_protocol,i25p_old
	jmp	short read_fat
;
sys_read_err:
	lea	dx,msg_sys_read_err	;"Can't read system area of disk."
	call	pr_text
	jmp	no_action
;
; Read in the entire first FAT into our buffer.
;
read_fat:
	mov	ax,fat_1st_sector
	mov	sector_lo,ax
	xor	ax,ax
	mov	sector_hi,ax
	mov	dta,ax		;Might be more than 128 sectors, which
	mov	cx,fat_size	;complicates reading & FAT addressing.
	mov	sector_count,1	;So, we will read 1 at a time.
	mov	ax,fat_seg
	mov	dta_seg,ax
readf_lp:
	call	read_disk
	jc	sys_read_err
	mov	ax,sector_size
	add	dta,ax
	jnc	readf_next
	add	dta_seg,1000h	;Next 64k segment.
	mov	dta,0
readf_next:
	inc	sector_lo
	loop	readf_lp
;
	test	fat16bit,0FFh	;Disk FAT is already in 16-bit format?
	jnz	fat_is_16	;Yes.
	call	fat12_expand	;No, convert 12-bit format into 16-bit.
fat_is_16:
	mov	ax,options
	test	ax, opt_keep_fat
	jnz	read_root	;Leave the FAT intact.
	mov	dx,fm_bad
	test	ax, opt_erase_fat
	jz	orig_fat0
	mov	dx,fm_free
orig_fat0:
	mov	cx,clusters
	mov	di,2*2		;Begin with cluster 2 (doubled for offset).
	mov	bl,media
	mov	bh,0FFh
	mov	ds,fat_seg
	ASSUME	DS:nothing
	xor	si,si
	xor	ax,ax		;AX = FM_FREE.
	xchg	bx,[si] 	;Should be unchanged.
	cmp	bx,[si] 	;FAT media byte was intact?
	jne	orig_fat_lp	;No - clear all FAT entries (AX=0).
	mov	ax,dx		;Write all entries, unless they match AX.
orig_fat_lp:
	cmp	ax,[di] 	;Matches FM_BAD (or FM_FREE)?
	je	orig_fat_next	;Yes, leave this one unchanged.
	mov	[di],si 	;Clear all other FAT entries.
orig_fat_next:
	inc	di
	inc	di
	loopnz	orig_fat_lp	;Until CX=0 (done) or DI=0 (segment limit).
	jcxz	orig_fat_end
	mov	di,ds
	add	di,1000h	;2nd segment.
	mov	ds,di
	xor	di,di
	jmp	orig_fat_lp
orig_fat_end:
	ASSUME	DS:prog
	push	cs
	pop	ds
	nop
;
; Read the entire root directory into our buffer.
;
read_root:
	mov	ax,root_sectors
	mov	sector_count,ax
	xor	ax,ax
	mov	dta,ax
	mov	sector_hi,ax
	mov	ax,dir_1st_sector
	mov	sector_lo,ax
	mov	ax,dir_seg
	mov	dta_seg,ax
	call	read_disk
	jnc	root_rdok
	jmp	sys_read_err
;
root_rdok:
	mov	ax,options
	test	ax, opt_keep_root
	jnz	alrf_stp		;Leave root intact.
	test	options, opt_erase_root
	jz	examine_root
	mov	es,dir_seg
	cld
	xor	di,di
	mov	ax,sector_size
	shr	ax,1		;Words per sector.
	mul	root_sectors
	mov	cx,ax
	xor	ax,ax		;Clear the entire root dir to zeros.
	rep stosw
alrf_stp:
	jmp	alloc_root_files
;
; Examine the root directory in the buffer.
; 1 - Existing files and level_1 subdirs are not harmed.
; 2 - Deleted root files & dirs are discarded.
; 3 - Format-zapped (1st byte=0) level_1 subdirectories will live again.
; 4 - Format-zapped root files will live again.
; 5 - Entries zapped by CPS Formatter will live again (with correct 1st char).
; 6 - The root directory is packed.
;
examine_root:
	mov	ax,root_entries
	mov	repeat,ax		;This is the big loop counter.
	mov	free_root_entries,ax
	mov	es,dir_seg
	cld
;
examine_root_lp:
	mov	ax,root_offset
	mov	di,ax
	call	check_dir_entry 	;Returns AH=status, AL=attribute.
	test	ah, de_invalid OR de_sublink
	jnz	root_corrupted
	test	ah,de_zeros
	jz	exr_not_z		;Not all zeros.
	jmp	ex_root_done		;Root has been zeroed.
root_corrupted:
	mov	byte ptr es:[di],0	;End-of-dir mark.
	mov	ax,di
	mov	cl,5
	shr	ax,cl			;Figure how many.
	lea	dx,msg_examined_ent	;"Examined @0d root entries.",cr,lf
	call	display
	jmp	ex_root_done		;Ignore remainder of root.
exr_not_z:
	test	ah,de_zapsaved	;Zapped by CPS Formatter (recoverable)?
	jz	exr_not_zs	;No.
	mov	dl,es:[di].zapsav_loc
	mov	es:[di].filename, dl	;Restore the saved 1st character.
	push	ax
	push	di
	lea	di,[di].zapsav_loc
	mov	cx,zapsav_length
	mov	al,0
	cld
	rep stosb		;Clear the zap-save area (don't need it now).
	pop	di
	pop	ax
exr_not_zs:
	test	al,vol_attr
	jnz	resurrect_vol
	test	ah,de_deleted
	jnz	ignore_dir_entry
	mov	bx,es:[di].start_cluster

         TEST  AL,DIR_ATTR              ; if this is a sub-dir
         JNZ   EXR_CLU                  ; size is gonna be zero, branch
         MOV   CX,ES:[DI].FILE_SIZE
         OR    CX,ES:[DI].FILE_SIZE+2   ; is file size zero?
         JNZ   EXR_CLU                  ; no, branch
         OR    BX,BX                    ; yes and if starting clu iz not zero
         JNZ   IGNORE_DIR_ENTRY         ; jmp
         JMP   SHORT RESURRECT_FILE     ; otherwise, process the zero len file
           
EXR_CLU:
	cmp	bx,2			;Valid starting cluster?
	jb	ignore_dir_entry
	cmp	bx,cluster_cnt_plus_1
	ja	ignore_dir_entry
	test	al,dir_attr
	jnz	resurrect_dir
resurrect_file:
	inc	file_counter
	test	ah, de_live OR de_zapsaved
	jnz	decide_if_pack			;Accept name as-is.
	mov	byte ptr es:[di].filename, "F"	;Fix 1st char of name.
	jmp	short decide_if_pack
resurrect_vol:
	test	ah,de_zapsaved
	jnz	decide_if_pack			;Accept restored name.
ignore_dir_entry:
	jmp	short next_root_entry
resurrect_dir:
	inc	rootsub_count
	test	ah, de_live OR de_zapsaved
	jnz	decide_if_pack			;Don't alter the name.
	mov	byte ptr es:[di].filename, "D"	;Fix the lost 1st char.
	call	make_subdir_entry
	push	di
	lea	di,[di].extension
	lea	si,subdir.extension
	mov	cx,3
	rep movsb			;Change the extension only.
	pop	di
decide_if_pack: 			;Valid root entry accepted.
	test	options, opt_list	;First, should we display it?
	jz	decide_ip2		;No.
	call	show_dir_info
	call	crout
decide_ip2:
	dec	free_root_entries
	mov	si,free_root_entry
	add	free_root_entry,32
	cmp	di,si			;Need to relocate this dir entry?
	je	next_root_entry 	;No.
pack:	push	es		;We want all entries to be packed near
	pop	ds		;the beginning of the root directory.
	nop
	push	di
	xchg	di,si
	mov	cx,32/2
	rep movsw		;Relocate this directory entry.
	pop	di		;Recover the pointer to old location.
	push	di		;Resave it.
	mov	cl,32/2
	xor	ax,ax
	rep stosw		;Erase the old occurrance of this entry.
	pop	di
	push	cs
	pop	ds		;Restore DS=CS.
	nop
next_root_entry:
	add	root_offset,32
	dec	repeat
	jz	ex_root_done
	call	flushkey
	jc	ex_rt_abt
	jmp	examine_root_lp
ex_rt_abt:
	jmp	no_action
ex_root_done:
	xor	ax,ax
	cmp	ax,free_root_entries
	jz	alloc_root_files	;Root is full.
	mov	di,free_root_entry
	mov	es:[di],al		;End-of-dir mark.
;
; Count the files & subdirs in the root.  Also, maybe mark
; up the FAT, according to the root entry start_clusters.
;
alloc_root_files:
	xor	ax,ax
	mov	rootsub_count,ax	;We're going to count them again.
	mov	file_counter,ax
	mov	es,dir_seg
	xor	di,di
	mov	cx,root_entries
	mov	free_root_entries,cx
alloc_rtf_lp:
	mov	ax,di
	call	check_dir_entry
	test	ah, de_zapped OR de_zeros OR de_invalid
	jnz	alloc_rtf_done
	test	ah,de_live
	jz	alloc_rtf_next
	test	al,vol_attr
	jnz	alloc_rtf_next	;Don't count vol label as a file.
	test	al,dir_attr
	jnz	alloc_rtf_d
	mov	dx,fm_file_start
	inc	file_counter
	mov	ax,es:[di].file_size
	or	ax,es:[di].file_size+2
	jz	alloc_rtf_next		;Zero length file has no clusters.
	jmp	short alloc_rtf_st
alloc_rtf_d:
	mov	dx,fm_sub_start
	inc	rootsub_count
alloc_rtf_st:
	test	options, opt_keep_fat
	jnz	alloc_rtf_next		;We're only counting them.
	mov	ax,dx
	mov	bx,es:[di].start_cluster
	call	store_link
alloc_rtf_next:
	dec	free_root_entries
	add	di,32
	loop	alloc_rtf_lp
;
alloc_rtf_done:
	mov	ax,file_counter
	or	ax,rootsub_count
	jz	hunt1
	call	crout
	mov	ax,file_counter
	lea	dx,msg_root_files	;"Files found in root: @0d",cr,lf
	call	display
	mov	ax,rootsub_count
	lea	dx,msg_subdirs_found ;"Subdirectories found in root: @0d",crlf
	call	display
	or	ax,ax
	jz	hunt1		;No subs in root, positively MUST hunt.
	test	options, opt_keep_fat
	jz	hunt1		;No, must search the disk to locate subsubs.
	jmp	nowalk		;Yes, hunt and tree walk are not needed.
;
;----------------------------------------------------------
; Hunt across the disk, reading every data cluster,
; looking for ones which look like subdirectories.
;
hunt1:	lea	dx,msg_searching	;"Searching disk...",cr,lf
	call	pr_text
	mov	cluster_index,2
	mov	hsub_count,0
	mov	progress,0
	mov	ax,clusters
	mov	repeat,ax
;
h1_lp:	call	show_progress
	call	flushkey
	jnc	h1_no_esc
h1_esc: lea	dx,msg_stop_hunt ;"Complete remainder of search (Y/N/Q)? "
	call	pr_text
	call	getline
	jc	h1_esc		;Reject ESC here, since Q is allowed.
	jz	h1_esc		;No default - must enter something.
	cmp	al,yes_char
	je	h1_no_esc	;Continue search.
	cmp	al,no_char
	je	h1_answ_no
	cmp	al,quit_char
	jne	h1_esc		;Ask again.
	lea	dx,msg_abort	;"Cancelled"
	call	pr_text
	jmp	no_action
h1_answ_no:
	jmp	h1_done
h1_no_esc:
	mov	bx,cluster_index
	call	get_link	;Fetch FAT entry # BX.
	cmp	ax,fm_bad
	je	h1_next_clu_stp ;Skip cluster already marked bad.
	mov	ax,bx
	call	calc_sector	;Updates SECTOR_HI & SECTOR_LO.
h1_read_it:
	mov	ax,cluster_seg
	mov	dta_seg,ax
	mov	es,ax
	xor	di,di
	mov	dta,di
	mov	ax,cluster_size
	mov	sector_count,ax
	call	read_disk		;Read the entire cluster.
	jnc	h1_chk
	mov	bx,cluster_index
	mov	ax,fm_bad		;Mark it bad in the FAT.
	jmp	short h1_mod_fat
h1_chk: call	check_cluster		;Is this a subdirectory cluster?
	or	ax,ax
	jnz	h1_found_sub		;Yes, it is.
h1_next_clu_stp:
	jmp	short h1_next_clu
h1_found_sub:
	mov	dx,ax			;Save return code from CHECK_CLUSTER.
	call	get_link		;Get FAT entry currently there (#BX).
	test	options, opt_keep_fat	;FAT is assumed already valid?
	jz	h1_fsub 		;No, must build it.
	cmp	ax,fm_free		;Cluster is (should be) allocated?
	je	h1_next_clu_stp 	;No.  Believe valid FAT, ignore find.
	jmp	short h1_fsubc		;Maybe need a new root entry.
h1_fsub:
	or	ax,ax			;Free cluster (as expected)?
	jnz	h1_next_clu_stp 	;Ignore it, cluster already allocated.
h1_fsubc:
	mov	ax,dx
	cmp	ax,fm_sub_start 	;Is it the start of a subdir?
	je	h1_fsubd		;Yes.
	cmp	ax,fm_end		;Complete subdir in the cluster?
	je	h1_fsubd		;Yes.
	cmp	ax,fm_sub_nul		;Empty, but a complete subdir?
	jne	h1_mod_fat		;No.  Make no root entry.
h1_fsubd:
	cmp	bx, es:[di].start_cluster	;Self-link is correct?
	jne	h1_next_clu_stp 		;Wrong.  Ignore it.
	inc	hsub_count
	cmp	word ptr es:[di+32].start_cluster, 0	;Root level subdir?
	jz	h1_make_sub				;Yes.
h1_mod_fat:
	call	store_link		;BX= cluster #, AX= new FAT entry
	jmp	short h1_next_clu	;(if /KF, STORE_LINK did nothing).
h1_make_sub:
	test	options, opt_keep_root	;Root is already valid & protected?
	jnz	h1_next_clu		;Yes.
	call	store_link
	cmp	free_root_entries,0	;We need to create a root entry.
	jz	h1_done 		;Abnormal exit, root dir is full.
	inc	rootsub_count		;Another subdir in root.
	call	make_root_sub		;Create root subdir entry.
h1_next_clu:
	inc	cluster_index
	dec	repeat
	jz	h1_done
	jmp	h1_lp			;Keep looking.
;
h1_done:
	call	crout
	mov	ax,rootsub_count
	or	ax,file_counter
	jnz	show_hunt_results
	lea	dx,msg_nothing_found
	call	pr_text
	jmp	no_action
;
show_hunt_results:
	mov	ax,file_counter
	lea	dx,msg_root_files	;"Files found in root: @0d",cr,lf
	call	display
	mov	ax,rootsub_count
	lea	dx,msg_subdirs_found ;"Subdirectories found in root: @0d",crlf
	call	display
;
	test	options, opt_keep_fat
	jz	cleanup_fat
nowalk: lea	dx,msg_write_fake	;" changes not written to disk."
	test	options,opt_wrfake
	jnz	fp_ww
	lea	dx,msg_write_warn	;"Next phase writes to hard disk."
fp_ww:	call	pr_text 		;(This appears also at WALK_DONE.)
	call	ask_for_yes
	jnz	cancel_stp
	jmp	write_system_area
cancel_stp:
	jmp	no_action
;
cleanup_fat:
	call	link_subs	;Deal with all those FM_SUB_XXXs in the FAT.
;
;-----------------------------------------------------------------------
; Now, we walk the directory tree structure, twice.
;
; First walk:  Mark 'live'-file starting_clusters, verify tree structure.
;	       Display paths.  In verbose /L mode only, list each filename.
;
; Second walk:	1. Display paths.
;		2. Check file lengths.
;		3. If fragmented, prompt user.
;		4. Delete/truncate as selected, or complete the FAT chains.
;
walk_begin:
	lea	dx,msg_walk1
	call	pr_text
	mov	repeat,2
walk_tree:
	lea	di,tree
	mov	cx,((SIZE tree_struc) * tree_size)/2
	xor	ax,ax
	mov	file_counter,ax
	push	cs
	pop	es
	cld
	rep stosw		;First, set things up.
	mov	tree_level,ax
	mov	word ptr path,ax	;AX=0.
	mov	clu_dirty,al	;Buffer never modified in 1st walk.
	dec	ax		;AX=-1.
	mov	clu_in_buffer,ax
	mov	tree.toffset,ax ;Special value for beginning of dir.
	jmp	walk_show_path	;Show it, then jump to WALK_LP.
;
walk_abort:
	lea	dx,msg_abort
	call	pr_text
	jmp	exit

walk_lp:
	call	flushkey
	jc	walk_abort
;
	call	dir_walk	;Returns ES:DI=dir entry, & AX=result.
	jc	walk_dir_end
;
	mov	dl,es:[di]	;Just to see in debugger.
	test	ah, de_zeros OR de_zapped OR de_invalid
	jnz	walk_dir_end
	test	ah,de_deleted
	jnz	walk_lp 	;Ignore deleted entries.  Get another.
	test	ah,de_sublink	;Entries "." or ".."?
	jnz	walk_lp 	;Ignore them (already checked by DIR_WALK).
	test	al,vol_attr
	jnz	walk_lp 	;Ignore volume label (uses no disk space).
	test	al,dir_attr
	jnz	walk_subdir	;It's a subdirectory entry.
	mov	dx,es:[di].file_size
	or	dx,es:[di].file_size+2
;        jz	walk_lp 	;Ignore zero length file (no disk space).
         JNZ   WALK_NON0_FILE           ; non-zero file, branch
         CMP   REPEAT,2                 ; is it the first pass
         JE    WALK1C_STP               ; yes, jmp
         INC   FILE_COUNTER             ; no, increment the file counter
         JMP   WALK_LP                  ; and then jump
          
WALK1C_STP:
         JMP   WALK1C

WALK_NON0_FILE:
	cmp	repeat,2	;Must be a live file.  First walk or 2nd?
	je	walk1
	jmp	walk2
;
walk_subdir:
	mov	bx,es:[di].start_cluster	;For subdir_nul handling.
	call	add_to_path	;(also returns CHECK_CLU code)
	jc	walk_suberr
	cmp	ax,fm_sub_nul	;Empty-subdir FAT mark?
	jne	walk_show_path	;No.
	mov	ax,fm_end	;Since this subdir IS part of the
	call	store_link	;tree, change mark #BX to FM_END.
	jmp	short walk_show_path
walk_suberr:
	test	options, opt_list
	jz	walk_lp
	call	show_dir_info
	lea	dx,msg_ignoring ;"Ignoring this subdirectory."
	call	pr_text
	lea	dx,path 	;To see in debug.
	call	sub_from_path
	cmp	repeat,2		;Which walk?
	je	walk_lp 		;Walk1 - just ignore it.
	mov	byte ptr es:[di],0E5h	;Mark deleted in walk2.
	jmp	walk2_dirt		;Maybe dirtied the buffer.
walk_dir_end:
	cmp	tree_level,0	;Root level?
	jnz	walk_backup
	jmp	walk_done
walk_backup:
	lea	dx,path 	;To see in debug.
	call	sub_from_path
walk_show_path: 		;Arrive from WALK_TREE, _SUBDIR or _DIR_END.
	lea	ax,pathdrv
	lea	dx,msg_path	;"Path=@0t\",cr,lf
	call	display
	jmp	walk_lp
;
; This is done only during the first walk.
;
walk1:	mov	bx,es:[di].start_cluster
	call	get_link
	cmp	ax,fm_free		;Starting cluster is free?
	je	walk1b			;Yes.  Allocate it for this file.
	cmp	ax,fm_file_start	;Already start-of-file there?
	jne	walk_lp_stp		;No.  Ignore cross-link.
	cmp	tree_level,0		;This is a root level file?
	jz	walk1c			;Must've been marked by EXAMINE_ROOT.
walk_lp_stp:
	jmp	walk_lp 		;Error!  Ignore cross-link.
walk1b: mov	ax,fm_file_start
	call	store_link		;Allocate the starting cluster.
walk1c: inc	file_counter
	test	options, opt_list
	jz	walk_nv
	call	show_dir_info
	call	crout
walk_nv:
	jmp	walk_lp
;
; This is done only during the second walk.
;
walk2:	mov	bx,es:[di].start_cluster
	call	get_link
	cmp	ax,fm_file_start	;The expected FAT entry.
	jne	walk2_cross		;Must've detected crosslink in walk1.
	call	check_contig_free	;Returns CF, AX=#contiguous clusters.
	mov	cx,ax
	jc	walk2_frag		;Not enough.  File must be fragmented.
	inc	file_counter
	call	complete_chain		;File seems contiguous, so it's easy.
	jmp	walk_lp
walk2_cross:
	call	show_dir_info
	lea	dx,msg_crosslink	;"Deleting crosslinked file.",CRLF.
	call	pr_text
	mov	byte ptr es:[di],0E5h	;Delete only the directory entry.
	jmp	short walk2_dirt
walk2_frag:				;CX holds max contig clusters.
	call	show_dir_info		;File name, size, date & time.
	mov	ax,sector_size
	mul	cluster_size
	mul	cx
	mov	dis_word+(2*1),ax
	mov	dis_word+(2*2),dx
	lea	dx,msg_only		;"Only @1l bytes are recoverable",crlf
	call	display
	mov	al,frag_opt
	cmp	al,"d"			;Was 'ALL' previously specified?
	jae	walk2_td		;Yes, so don't ask again.
	call	ask_trunc		;"Truncate or delete this file?"
	jnc	walk2_savop
	jmp	walk_abort
walk2_savop:
	mov	frag_opt,al
walk2_td:
	call	uppercase
	cmp	al,"T"			;Delete or Truncate the file?
	je	walk_truncate
	lea	dx,msg_delete
	call	pr_text 		;"Deleting this file."
	mov	byte ptr es:[di],0E5h	;Mark deleted, in the directory.
	mov	bx,es:[di].start_cluster
	mov	ax,fm_file_del
	call	store_link		;Mark cluster to be freed, later.
	jmp	short walk2_dirt
walk_truncate:
	lea	dx,msg_trunc		;"Truncating this file."
	call	pr_text
	mov	ax,cluster_size
	mul	cx
	mul	sector_size		;Clusters * sect/clu * bytes/sect.
	mov	es:[di].file_size,ax	;Change size in directory entry.
	mov	es:[di].file_size+2,dx
	call	complete_chain		;CX = # of clusters.
	inc	file_counter
walk2_dirt:
	cmp	tree_level,0
	jz	walk2_dirt2	;Root directory is not in cluster buffer.
	mov	clu_dirty,-1	;We have modified data in the cluster buffer.
walk2_dirt2:
	call	crout
	jmp	walk_lp
;
walk_done:
	dec	repeat
	jz	walks_both_done
	mov	ax,file_counter
	lea	dx,msg_files	;lf,"Files found: @0d",cr,lf,lf
	call	display
	lea	dx,msg_write_warn	;"Next phase writes to hard disk."
	test	options, opt_wrfake
	jz	walk_warn
	lea	dx,msg_write_fake	;"/W not specified.  Writes faked"
walk_warn:
	call	pr_text
	call	ask_for_yes
	jz	walk_wr_yes
	jmp	no_action
walk_wr_yes:
	lea	dx,msg_walk2
	call	pr_text
	jmp	walk_tree
walks_both_done:
	xor	ax,ax
	call	read_sub_cluster	;Write last changes (if any) to disk.
	call	fix_fm_dels		;Deal with any FM_FILE_DELs in FAT.
	mov	ax,file_counter
	lea	dx,msg_files_rec	;cr,lf,"@0d files recovered."
	call	display
;
;
; Lastly, we write the new FAT and root directory to the hard disk.
;
write_system_area:
	nop			;A place to put a breakpoint.
	test	options, opt_keep_fat
	jnz	write_dir
	cmp	fat16bit,0
	jnz	write_fats	;Disk FAT is 16-bit, so leave it that way.
	call	fat16_compress	;Back to 12-bit format.
write_fats:
	mov	ax,fat_1st_sector
	mov	sector_lo,ax
	mov	al,fat_count
	mov	ah,0
	mov	repeat,ax
write_fat:
	xor	ax,ax
	mov	dta,ax
	mov	sector_hi,ax
	mov	ax,fat_seg
	mov	dta_seg,ax
	mov	cx,fat_size
	mov	sector_count,1
write_fat_sec:
	call	write_disk
	jc	exit
	mov	ax,sector_size
	add	dta,ax
	jnc	write_fat_next
	add	dta_seg,1000h
	mov	dta,0
write_fat_next:
	inc	sector_lo
	loop	write_fat_sec
	sub	repeat,1	;Is there a 2nd FAT?
	ja	write_fat	;Yes.
;
; Even if root is 'protected', it might be slightly modified.  Rewrite it.
;
write_dir:
	mov	ax,dir_seg
	mov	dta_seg,ax
	xor	ax,ax
	mov	dta,ax
	mov	sector_hi,ax
	mov	ax,dir_1st_sector
	mov	sector_lo,ax
	mov	ax,root_sectors
	mov	sector_count,ax
	call	write_disk
	jc	exit
;
all_done:
	lea	dx,msg_done
	call	pr_text
;
exit:	mov	ah,0Dh		;Flush DOS disk buffers.
	int	21h
	nop
	int	20h		;Terminate.  Return to DOS.
;
;==========================  Procedures  =============================
;
; Read (write) logical sectors into (from) memory.
;
; On entry: rw_parm block must be setup with sector_lo & hi,
;	    sector_count and dta (disk transfer address).
;
; On exit: If error then CF=true and AX=error code, else CF=false.
;	   Error messages and sector # are printed in here.
;
; Only AX is changed.
;
read_disk PROC NEAR
	mov	ah,"R"
	jmp	short rw_disk
write_disk PROC NEAR
	mov	ah,"W"
rw_disk:
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	bp
	cmp	ah,"R"
	je	rw_2
	test	options, opt_wrfake	;Faking writes?
	jz	rw_2
	jmp	rw_done 		;Faking.  Leave with CF=false.
rw_2:	push	ds
	mov	al,drive
	lea	bx,sector_lo
	mov	cx,0FFFFh
	mov	dx,cx
	cmp	i25_protocol,i25p_new
	je	rw_3
	mov	dx,sector_lo
	mov	cx,sector_count
	lds	bx,dword ptr dta
rw_3:	cmp	ah,"W"
	jne	rw_25
	lea	si,msg_write	;"writing"
	push	si
	int	26h
	jmp	short rw_4
rw_25:	lea	si,msg_read	;"reading"
	push	si
	int	25h
rw_4:	pop	cx		;Discard extra flags from stupid DOS.
	pop	si		;Recover ptr to error message part.
	pop	ds		;Recover DS.
	cld
	sti
	jnc	rw_done 	;No error.
	cmp	al,7		;Error = unknown media (wrong protocol)?
	jne	rw_show_err	;No, display it.
	cmp	i25_protocol, i25p_unknown
	je	rw_error	;Suppress err msg during protocol testing.
rw_show_err:
	mov	cx,ax		;Preserve error code into CX.
	mov	dis_word+(2*4),ax
	mov	dis_word+(2*1),si
	mov	ax,sector_hi
	mov	dis_word+(2*2),ax
	mov	ax,sector_lo
	mov	dis_word+(2*3),ax
	lea	si,int25_error_list
rw_find_err_lp:
	xor	bx,bx
	mov	al, [si].i25_err_code
	cmp	al,-1
	je	rw_found_err		;End of list, without match.  BX=0.
	mov	bx, [si].i25_err_ptr
	cmp	cl,al			;Matching error code?
	je	rw_found_err
	lea	si,[si] + SIZE i25_error_struc
	jmp	rw_find_err_lp
rw_found_err:
	mov	dis_word+(2*5),bx
	lea	dx,msg_error
;
; "Error @1t sector# @2w@3wh, code @4wh@5t.",cr,lf
;
; Error writing sector# 00000000h, code 0000h sector not found.
;
	call	display
	mov	ax,cx		;Recover error code.
rw_error:
	stc
rw_done:
	pop	bp
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	ret
write_disk ENDP
read_disk ENDP
;
;-------------------------------------------------------------------
; Calculate logical sector from cluster number.
;
; On entry: AX= cluster number (range 2 to nnnn).
; On exit: If in range then CF=false and SECTOR_HI & _LO are updated.
;	   If invalid cluster number then CF=true.
;
; All regs are unchanged.
;
calc_sector PROC NEAR
	push	ax
	push	dx
	mov	dx,cluster_cnt_plus_1
	cmp	dx,ax		;Carry true if Max < requested #.
	jb	calcsec_done
	sub	ax,2
	jb	calcsec_done	;Return with CF=true.
	mul	cluster_size
	add	ax,first_data_sector
	adc	dx,0		;Should always yield CF=false.
	mov	sector_lo,ax
	mov	sector_hi,dx
calcsec_done:
	pop	dx
	pop	ax
	ret
calc_sector ENDP
;
;------------------------------------------------------------------
; Read one entire subdirectory cluster into our cluster buffer.
; If current contents of our cluster buffer have been modified
; (CLU_DIRTY non zero), that data will be rewritten to disk before
; the new cluster is read from disk.
;
; On entry: AX=cluster number (special value 0 means
;	    just flush the buffer, don't read anything).
;
; On exit: If successful then CF=false.
;	   If error writing (fatal), CF=true and AX=FFFF (-1).
;	   If error reading, then CF=true and AX=DOS code.
;	   Only AX is changed.
;
read_sub_cluster PROC NEAR
	push	bx
	cmp	ax,clu_in_buffer	;Buffer already holds desired data?
	je	read_sclu_exit		;Yes.  Do nothing, return CF=false.
	mov	bx,ax			;Save cluster # into BX.
	mov	ax,cluster_seg
	mov	dta_seg,ax
	mov	dta,0
	mov	ax,cluster_size
	mov	sector_count,ax
;
	mov	ax,clu_in_buffer
	cmp	ax,-1			;Buffer contains any data?
	je	read_sclu		;None.
	cmp	clu_dirty,0		;Buffer holds modified data?
	jz	read_sclu		;No.
	call	calc_sector		;For the cluster NOW in buffer.
	call	write_disk		;Write modified data back to disk.
	mov	clu_dirty,0
	mov	ax,-1			;Assume write error.
	jc	read_sclu_exit		;Error while writing.
read_sclu:
	mov	clu_dirty,0
	mov	clu_in_buffer,-1
	mov	ax,bx
	or	ax,ax			;Special value for flush-only?
	jz	read_sclu_exit		;Yes, don't read anything.  CF=0.
	call	calc_sector		;For desired cluster.
	call	read_disk
	jc	read_sclu_exit
	mov	clu_in_buffer,bx
read_sclu_exit:
	pop	bx
	ret
read_sub_cluster ENDP
;
;----------------------------------------------------------------
; On entry: BX=cluster number, AX=new link value to be written.
; On exit: all regs preserved.
;
store_link PROC NEAR
	cmp	bx,cluster_cnt_plus_1
	ja	stlnk2		;Out of range.
	test	options, opt_keep_fat
	jnz	stlnk3		;Do nothing.
	push	bx
	push	ds
	push	ax
	mov	ax,fat_seg
	shl	bx,1		;FAT entries are words.
	jc	stlnk4		;Offset beyond the first 64k.
stlnk1: mov	ds,ax
	pop	ax
	mov	[bx],ax
	pop	ds
	pop	bx
	ret
stlnk2: nop
stlnk3: ret
stlnk4: add	ah,10h		;Next 64k segment.
	jmp	stlnk1
store_link ENDP
;
;-----------------------------------------------------------------
; On entry: BX = cluster number (2 to nnn).
; On exit: AX = FAT entry for that cluster.  Only AX is changed.
;
get_link PROC NEAR
	push	bx
	push	ds
	mov	ax,fat_seg
	shl	bx,1		;Scale by 2 for array of words.
	jc	gtlnk2		;Beyond the first 64k.	Next segment.
gtlnk1: mov	ds,ax
	nop
	mov	ax,[bx]
	pop	ds
	pop	bx
	ret
gtlnk2: add	ah,10h
	jmp	gtlnk1
get_link ENDP
;
;----------------------------------------------------------------
; On entry: BX = cluster number (2 to nnn).
; On exit: AX = FAT entry for that cluster.
;
; AX is changed.  SI and ES are destroyed.
;
get_link_fast PROC NEAR
	mov	ax,fat_seg
	mov	si,bx
	shl	si,1		;Scale by 2 for array of words.
	jc	gtlf2		;Beyond the first 64k.
gtlf1:	mov	es,ax
	mov	ax,es:[si]
	ret
gtlf2:	add	ah,10h		;Next seg.
	jmp	gtlf1
get_link_fast ENDP
;
;----------------------------------------------------------------
; Make a new subdir entry in the root directory.
; On entry: BX = starting cluster #.
; On exit: If successful then CF=false, else CF=true (too many).
;	   All regs preserved.
;
make_root_sub PROC NEAR
	push	ax
	push	cx
	push	si
	push	di
	push	es
	call	make_subdir_entry
	jc	make_rs_exit		;Give up - more than 999 subdirs.
	mov	subdir.start_cluster, bx
	mov	es,dir_seg
	nop
	mov	di,free_root_entry	;ES:DI = free root dir entry.
	lea	si,subdir
	mov	cx,32/2
	cld
	rep movsw			;Write the new root entry.
	sub	free_root_entries,1
	jbe	make_rs_done		;Full root, no end-of-dir mark needed.
	mov	free_root_entry,di	;Update ptr.
	mov	al,0
	stosb				;New end-of-dir mark in root.
make_rs_done:
	clc
make_rs_exit:
	pop	es
	pop	di
	pop	si
	pop	cx
	pop	ax
	ret
make_root_sub ENDP
;
;-----------------------------------------------------------------------
; Build a new subdirectory entry at SUBDIR (new ext #).
; Subdirectory name will be "SUBDIR.n".
; It's up to somebody else to copy it somewhere useful.
;
; On entry: nothing
; On exit: If number of subdirs exceeds 999, CF=true.  Else CF=false.
;
; No registers changed.
;
make_subdir_entry PROC NEAR
	push	ax
	push	di
	mov	ax,rootsub_count
	mov	di,999
	cmp	di,ax			;More than 3 decimal digits?
	jb	mksub_exit
	lea	di,subdir.extension
	xchg	di,red_pointer
	call	pr_dec			;Write decimal string.
	mov	red_pointer,di		;Shut off redirection of PRINTC.
	clc
mksub_exit:
	pop	di
	pop	ax
	ret
make_subdir_entry ENDP
;---------------------------------------------------------------------
; CHECK VALIDITY OF A DIRECTORY ENTRY.
;
; On entry: ES:AX points at a possible directory entry.
;
; On exit: AH returns the status of the directory entry
;
; de_zeros	All zeros (may be an unused dir entry).
; de_live	A valid live entry (file or subdir).
; de_deleted	Deleted entry (first byte of name = E5h).
; de_zapped	Zapped valid entry (first byte = 00).
; de_sublink	Entry "." or ".." (allowed only if input AX=0 or 32).
; de_zapsaved	Zapped by CPS Format, 1st char saved in 'reserved' area.
; de_invalid	Not valid (can't be a directory entry).
;
; Only combination of above bit flags used is DE_ZAPPED+DE_ZAPSAVED.
;
;    If AH= 'de_invalid' then AL=??,
;    else AH=status and AL= the file attribute byte.
;
; Only AX is changed.
;
check_dir_entry PROC NEAR
	push	bx
	push	cx
	push	si
	push	di
	push	ds
	push	es
;
	push	es
	pop	ds		;DS=ES=yonder.
	cld
	mov	bx,ax		;Offset of the entry to be examined.
	lea	di,[bx]
	xor	ax,ax		;AX = 0.  This also inits AH= DE_XXX unknown.
	mov	cx,32/2
	repz scasw		;Entire entry is zeros?
	jnz	chkd_res
	mov	ah,de_zeros	;And AL=0.
	jmp	chkd_exit
chkd_res:
	lea	di,[bx].dir_reserved
	mov	cx,10
	mov	al,0		;Reserved bytes should normally be zeros.
	repz scasb
	jz	chkd1		;They're all 0.  Do normal processing (AH=0).
	push	cs
	pop	ds		;DS=CS for CMPSB.
	nop
	lea	di,[bx].zapsav_loc+1
	mov	si,OFFSET zapsav_text
	mov	cx,zapsav_length
	repe cmpsb		;Is it the special mark from CPS Formatter?
	jne	chkd_invalid
	push	es
	pop	ds		;DS=ES=yonder, again.
	nop
	mov	ah,de_zapsaved	;Change AH from 0 to DE_ZAPSAVED.
;
chkd1:	test	[bx].file_attr, dir_attr
	jz	chkd_file
	cmp	byte ptr [bx], "."	;Special subdir linking entry?
	je	chkd_dots
chkd_file:				;Either file or normal subdir entry.
	lea	si,[bx].filename+1
	mov	cx,8+3-1	;Name + ext - 1.
chkd_lp:

ifdef DBCS		; ### if DBCS ###
	call	dbcs_chk_file
	jnc	@f			; if valid file name
	lea	si,[bx].filename+2	; try for 1st char is Double Byte
	mov	cx,8+3-2
	call	dbcs_chk_file
	jc	chkd_invalid		; if invalid
@@:

else			; ### if Not DBCS ###

	lodsb			;Fetch one char from filename.
	cmp	al," "
	jb	chkd_invalid	;Reject control chars in filename.
	call	chk_fnchar
	loopne	chkd_lp
	je	chkd_invalid
endif			; ### end if Not DBCS ###

;
; Seems OK, so far.  Now we check the first char of the filename.
;
	mov	al,[bx].filename
	cmp	al,0			;1st char could have special things.
	je	chkd_zapped
ifdef DBCS
	cmp	al,05
	jz	chkd_lead		; if this is converted lead byte E5h
endif
	cmp	al," "
	jb	chkd_invalid
	cmp	al,0E5h
	je	chkd_deleted
ifdef DBCS
	call	IsDBCSLeadByte
	jz	chkd_lead		; if this is lead byte
endif
	call	chk_fnchar
	je	chkd_invalid		;First char of filename is invalid.
ifdef DBCS
chkd_lead:
endif
	or	ah,de_live
	jmp	short chkd_get_attr
;
chkd_dots:
	test	byte ptr [bx].file_attr, vol_attr OR sys_attr OR hide_attr
	jnz	chkd_invalid
	mov	al,[bx].filename+1
	xor	cx,cx		;". " allowed only at offset 0 (1st entry).
	cmp	al," "
	je	chkd_blanks	;Entry is ". "
	mov	cl,32		;Allowed offset of ".." in a directory.
	cmp	al,"."
	jne	chkd_invalid	;2nd char is neither "." nor " ".
chkd_blanks:
	cmp	bx,cx		;Sublink is in the allowed position?
	jne	chkd_invalid
	or	ah,de_sublink
	lea	di,[bx].filename+2
	mov	al," "
	mov	cx,8+3-2
	repz scasb		;Remainder of subdir name is blanks?
	je	chkd_get_attr	;Yes - correct.
;
chkd_invalid:
	mov	ah,de_invalid
	jmp	short chkd_exit
chkd_zapped:
	or	ah,de_zapped
	jmp	short chkd_get_attr
chkd_deleted:
	or	ah,de_deleted
chkd_get_attr:
	test	ah,de_zapsaved
	jz	chkd_attr2
	test	ah,de_zapped	;Only allowed combo is DE_ZAPPED+DE_ZAPSAVED.
	jz	chkd_invalid
chkd_attr2:
	mov	al,[bx].file_attr
	test	al,vol_attr
	jnz	chkd_exit	;Start_cluster of volume label is ignored.
	test	ah, de_live OR de_sublink
	jz	chkd_exit
	mov	cx,[bx].start_cluster	;For some types, check this too.
	cmp	cx,cs:cluster_cnt_plus_1
	ja	chkd_invalid
;
chkd_exit:
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	cx
	pop	bx
	ret
check_dir_entry ENDP

ifdef DBCS
;---------------------------------------------------------------------
;
;	*** Check if file name is valid (DBCS supported) ***
;
;	input:	DS:SI = string address
;		CX = string length
;	output:	CF = 1 if invalid
;
dbcs_chk_file	proc	near
chkf_loop:
	lodsb
	call	IsDBCSLeadByte
	jnz	@f			; if not lead byte
	dec	cx
	jz	chkf_invalid		; if no tail byte
	lodsb				; get tail byte
	call	IsDBCSTailByte
	jnz	chkf_invalid		; if not tail byte
	jmp	short chkf_next
@@:
	cmp	al," "
	jb	chkf_invalid		; if control char
	call	chk_fnchar
	jz	chkf_invalid
chkf_next:
	loop	chkf_loop
	clc				; valid file name
	jmp	short chkf_ret
chkf_invalid:
	stc				; invalid file name
chkf_ret:
	ret
dbcs_chk_file	endp
endif

;-------------------------------------------------------------------
; Check if a character is legal for a filename (or extension).
; It is checked for lowercase, ascii>126 and against BAD_CHAR_LIST.
;
; On entry: AL= character under test.
; On exit: If char is allowed then ZF=false.
;	   If illegal then ZF=true.
;
; DI is destroyed.
;
chk_fnchar PROC NEAR
	push	cx
	push	es
	push	cs
	pop	es
	cld
	cmp	al,7Eh
	ja	chkfnc_err
	cmp	al,"a"
	jb	chkfnc1
	cmp	al,"z"		;Lowercase letters are illegal.
	jb	chkfnc_err
chkfnc1:
	mov	di,OFFSET bad_char_list
	mov	cl,bad_char_length
        xor     ch,ch
	repne scasb
chkfnc_done:
	pop	es
	pop	cx
	ret
chkfnc_err:
	cmp	al,al		;Set ZF=true for a bad character.
	jmp	chkfnc_done
;
there	=$
bad_char_list DB '."/\[]:|<>+=;,'
bad_char_length =$-there
;
chk_fnchar ENDP
;---------------------------------------------------------------------
; On entry: CLUSTER_SEGment is assumed to contain a
;	    disk cluster for examination.
;
; On exit: AX returns a code (usually the suggested FAT entry)
;	0	     =	Not a valid subdirectory.
;	FM_END	     =	complete subdir in this cluster.
;	FM_SUB_START =	with "." & "..", but no 00 end-of-dir mark.
;	FM_SUB_MID   =	partial: missing both ".." and end-mark.
;	FM_SUB_TAIL  =	partial: no "..", but with end-mark.
;	FM_SUB_NUL   =	complete, but without any live files.
;
; If fm_sub_start, _end or _nul is returned, the caller should examine
; the start_cluster values in the 1st and 2nd entries ("." & "..").
;
; Only AX is changed.
;
check_cluster PROC NEAR
	push	bx
	push	cx
	push	dx
	push	es
	mov	es,cluster_seg
	xor	bx,bx		;Begin at offset 0 in the cluster segment.
	xor	dx,dx		;State flags.
	mov	cx,dirs_per_cluster
chcl_lp:
	mov	ax,bx		;AX = offset into cluster_seg.
	call	check_dir_entry
	test	ah, de_invalid OR de_zapsaved
	jnz	chcl_not_sub
	test	al,vol_attr
	jnz	chcl_not_sub	;Volume label not allowed in subdir.
	test	dh, de_zeros OR de_zapped	;Already found one zero entry?
	jz	chcl_next			;Not yet.
	test	ah, de_zeros OR de_zapped	;Remainder should be 0, too.
	jz	chcl_not_sub
chcl_next:
	or	dh,ah		;Accumulate status bits.
	add	bx,32
	loop	chcl_lp
	test	dh, de_sublink
	jz	chcl_not_start	;No "." and ".." entries.
	mov	ax,fm_sub_start
	test	dh, de_zeros OR de_zapped ;Was end-of-dir mark (0) found?
	jz	chcl_done		;No.  May be only part of a subdir.
	mov	ax,fm_sub_nul	;Assume null.
	test	dh,de_live	;Any live entries?
	jz	chcl_done	;None.	Deleted or empty complete subdir.
	mov	ax,fm_end	;Complete subdir in this cluster!
	jmp	short chcl_done
chcl_not_start:
	test	dh,de_live	;Any live entries?
	jz	chcl_not_sub	;None.	We'll ignore it.
	mov	ax,fm_sub_mid
	test	dh, de_zeros OR de_zapped ;Was the end-of-dir mark (0) found?
	jz	chcl_done		;No.
	mov	ax,fm_sub_tail
	jmp	short chcl_done
chcl_not_sub:
	xor	ax,ax
chcl_done:
	pop	es
	pop	dx
	pop	cx
	pop	bx
	ret
check_cluster ENDP
;
;--------------------------------------------------------------------
; Convert a 12-bit FAT in memory to 16-bit FAT format.
;
; The FAT_segment must be large enough to hold the 16-bit version.
;
fat12_expand PROC NEAR
	push	ax
	push	cx
	push	si
	push	di
	push	es
	mov	ax,cluster_cnt_plus_1	;Also = the max cluster number.
	mov	di,ax
	shl	di,1			;DI = 2 * max.
	mov	si,ax
	shr	si,1
	add	si,ax			;SI = 1.5 * max.
	mov	es,fat_seg
	nop
	mov	cl,4
	test	al,1			;Even or odd?
	jz	f12e_get2
f12e_lp:
	mov	ax,es:[si]		;Loop begins with the odd # entry.
	dec	si
	shr	ax,cl			;CL=4.
	cmp	ax,0FF6h		;4086.
	jb	f12e_store1
	mov	ah,0FFh
f12e_store1:
	mov	es:[di],ax
	dec	di
	dec	di
f12e_get2:
	mov	ax,es:[si]		;Fetch the even entry.
	dec	si
	dec	si
	and	ax,0FFFh
	cmp	ax,0FF7h
	jb	f12e_store2
	mov	ah,0FFh
f12e_store2:
	mov	es:[di],ax
	dec	di
	dec	di
	cmp	si,di		;Done yet?
	jne	f12e_lp
	pop	es
	pop	di
	pop	si
	pop	cx
	pop	ax
	ret
fat12_expand ENDP
;---------------------------------------------------------------
; Convert the 16-bit FAT in memory to 12-bit format.
;
;
fat16_compress PROC NEAR
	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	es
	mov	bx,cluster_cnt_plus_1	; = max cluster number.
	inc	bx			;Count = max +1.
	mov	es,fat_seg
	cld
	xor	si,si
	xor	di,di
	mov	cl,4
f16c_lp:
	mov	ax,es:[si]
	and	ax,0FFFh
	inc	si
	inc	si
	mov	dx,es:[si]
	inc	si
	inc	si
	shl	dx,cl		;CL=4.
	or	ah,dl
	mov	es:[di],ax
	inc	di
	inc	di
	mov	es:[di],dh
	inc	di
	sub	bx,2
	ja	f16c_lp
;
	mov	ax,fat_size
	mov	bx,sector_size
	shr	bx,1
	mul	bx		;# words in 12-bit FAT.
	mov	cx,ax
	mov	ax,di
	inc	ax		;Round up.
	shr	ax,1		;Convert offset to word count.
	sub	cx,ax		;Words beyond 12-bit format of FAT.
	jbe	f16c_done
	xor	ax,ax
	rep stosw		;Clear out the now unused part.
f16c_done:
	pop	es
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
fat16_compress ENDP
;
;-----------------------------------------------------------------
; Scan the FAT (in forward direction) for a value.
;
; On entry: AX = FAT entry value to search for,
;	    DI = 1st cluster number to examine.
;
; On exit: If found then ZF=true and AX=cluster number.
;	   If not found then ZF=false and AX=?
;
; AX, SI & ES are changed.
;
scan_fat PROC NEAR
	push	bx
	push	cx
	push	dx
	mov	bx,di		;Beginning cluster #.
	mov	cx,cluster_cnt_plus_1	;Also = max cluster #.
	sub	cx,bx
	jb	scanf_done	;Leave with ZF=false, AX unchanged.
	inc	cx		;Count = 1 + max - beginning.
	mov	dx,ax		;Save value to hunt for.
	dec	bx		;Setup for pre-increment.
scanf_lp:
	inc	bx
	call	get_link_fast
	cmp	ax,dx
	loopne	scanf_lp
	mov	ax,bx		;Return cluster number (=? if ZF=false).
scanf_done:
	pop	dx
	pop	cx
	pop	bx
	ret
scan_fat ENDP
;
;---------------------------------------------------------------
; Scan the FAT (in backward direction) for a value.
;
; On entry: AX = FAT entry value to search for,
;	    DI = 1st cluster # to examine.
;
; On exit: If found then ZF=true and AX=cluster number.
;	   If not found then ZF=false and AX=?
;
; AX, SI & ES are always changed.
;
scan_fat_r PROC NEAR
	push	bx
	push	cx
	push	dx
	mov	bx,di
	mov	cx,di
	sub	cx,2
	jb	scanfr_done
	inc	cx
	inc	bx		;Setup for pre-decrement.
	mov	dx,ax		;Save desired value.
scanfr_lp:
	dec	bx
	call	get_link_fast
	cmp	ax,dx
	loopne	scanfr_lp
	mov	ax,bx		;Cluster #.
scanfr_done:
	pop	dx
	pop	cx
	pop	bx
	ret
scan_fat_r ENDP
;
;-----------------------------------------------------------------
; Link up (in the FAT) the pieces of subdirs we've found.
;
; On entry: DS=CS.
; On exit: All FM_SUB_XXXs are gone from the FAT.
;	   Destroys AX,BX,CX,DX,SI,DI,ES.
;
link_subs PROC NEAR
;
; Start by linking up the FAT sub_mids to the sub_starts as best we can.
;
	xor	dx,dx		;Beginning of chain is undefined, at first.
ls1_top:
	mov	es,fat_seg
	cld
	mov	bx,1		;Init to 1 (pre-incremented to 2).
	mov	cx,clusters
ls1_lp: jcxz	ls1_done
ls1_scan:
	inc	bx
	call	get_link_fast
	cmp	ax,fm_free
	loope	ls1_scan
	je	ls1_done	;Nothing more of interest.
	cmp	ax,fm_sub_start
	je	ls1_found_start
	cmp	ax,fm_sub_mid
	jne	ls1_lp
	or	dx,dx		;Any chain-building in progress?
	jz	ls1_lp		;No, don't yet know where a chain starts.
	mov	ax,bx		;Cluster # of the sub_mid we've just found.
	mov	bx,dx		;End of previous chain.
	call	store_link	;Link previous chain to this sub_mid.
	mov	dx,ax		;New end of the growing chain.
	mov	bx,ax
	jmp	ls1_lp
ls1_found_start:
	mov	ax,dx
	mov	dx,bx
	or	ax,ax		;Any previous chain?
	jz	ls1_top 	;No.  Start over, with BX= start of 1st chain.
	jmp	ls1_lp		;Yes,
ls1_done:
	or	dx,dx		;Did we find any 'sub_start's?
	jnz	linksub2	;Yes.
	mov	dx,fm_free	;None, so replace all
	jmp	short ls3_start	;FM_SUB_XX's with FM_FREE.
;
; Search for sub_tails and then link them to sub_mids or sub_starts.
;
linksub2:
	mov	bx,1		;Init 2-1.
	mov	cx,clusters
ls2_lp:
	jcxz	linksub3
ls2_scan:
	inc	bx
	call	get_link_fast
	cmp	ax,fm_sub_tail
	loopne	ls2_scan
	jne	linksub3	;Not found.  Nothing more of interest.
	mov	di,bx
	dec	di		;The entry just before the sub_tail.
	mov	ax,fm_sub_mid
	call	scan_fat_r	;Hunt backwards for a sub_mid entry.
	je	ls2_found	;Returns AX=cluster # where it was found.
	mov	ax,fm_sub_start ;Try for a sub_start, too.
	call	scan_fat_r
	je	ls2_found
	mov	di,bx
	inc	di		;The entry just after the sub_tail.
	mov	ax,fm_sub_mid
	call	scan_fat	;Hunt forwards.
	je	ls2_found
	mov	ax,fm_sub_start
	call	scan_fat
	je	ls2_found
	mov	ax,fm_free	;No place to link the sub_tail, so erase it.
	jmp	short ls2_store
ls2_found:			;AX= cluster of item found by SCAN_FAT.
	xchg	ax,bx		;So AX=clu of sub_tail, BX=_start or _mid.
	call	store_link	;Replace _start or _mid with ptr to sub_tail.
	mov	bx,ax		;BX selects the TAIL entry.
	mov	ax,fm_end	;End the chain.
ls2_store:
	call	store_link
	jmp	ls2_lp
;
; Change every remaining FM_SUB_START or FM_SUB_MID into FM_END.
; (Note: at this point, there should be no remaining FM_SUB_TAILs)
;
linksub3:
	mov	dx,fm_end
;
; Special entry for deleting (DX will be FM_FREE).
;
ls3_start:
	mov	cx,clusters
	mov	bx,1
ls3_lp: jcxz	linksub4
ls3_scan:
	inc	bx
	call	get_link_fast
	cmp	ax,fm_free
	loope	ls3_scan
	je	linksub4
	cmp	ax,fm_sub_start
	je	ls3_change
	cmp	ax,fm_sub_mid
	jne	ls3_lp
ls3_change:
	mov	ax,dx		;DX is either FM_END or FM_FREE.
	call	store_link
	jmp	ls3_lp
;
linksub4:
	push	cs
	pop	es
	ret
link_subs ENDP
;
;------------------------------------------------------------------
; Files we deleted in WALK2 were marked in the FAT as fm_file_del,
; instead of fm_free (this improves performance regarding
; fragmented files).  This proc is provided to search the FAT
; and convert any fm_file_dels into fm_free.  Also, during
; HUNT empty subdirs were marked fm_sub_nul to allow their
; possible recovery.  Remaining fm_sub_nuls are now freed.
;
; On entry: nothing.
; On exit: AX,BX,CX,DI destroyed.
;
fix_fm_dels PROC NEAR
	mov	dx,fm_file_del
fix_fd_top:
	mov	di,2
fix_fd_lp:
	mov	ax,dx
	call	scan_fat
	jne	fix_fd2
	mov	di,ax		;Update cluster # for next loop.
	mov	bx,ax
	mov	ax,fm_free
	call	store_link
	jmp	fix_fd_lp	;Look some more.
fix_fd2:
	mov	ax,fm_sub_nul
	xchg	ax,dx
	cmp	ax,dx		;1st or 2nd pass?
	jne	fix_fd_top	;Do it a 2nd time, for FM_SUB_NULs.
	ret
fix_fm_dels ENDP
;
;-----------------------------------------------------------------
; WALK THROUGH A DIRECTORY, GET THE NEXT ENTRY.
;
; On entry: TREE_LEVEL, TREE and PATH are assumed setup.
;
; On exit: If successful then CF=false and AX returns
;	   the same results as CHECK_DIR_ENTRY.
;	   Also, ES:DI points to the directory entry.
;
;	   But if a disk error occurs, then CF=true.
;
; Only AX,ES,DI are changed.
;
dir_walk PROC NEAR
	push	bx
	push	cx
	push	dx
	push	si
;
	mov	si,tree_level	;Inside this proc, SI= offset into TREE.
	mov	di,tree [si].toffset
	cmp	di,-1		;Special value for beginning of new path?
	jne	dw0		;No.
	mov	di,-32		;So upcoming 'ADD DI,32' will yield 0.
dw0:	or	si,si
	jnz	dw_nonroot
dw_root:
	mov	es,dir_seg
	nop
	add	di,32
	mov	ax,32
	mul	root_entries
	cmp	di,ax
	jae	dw_dir_end
	mov	tree [si].toffset, di	;Update.
	cmp	byte ptr es:[di],0	;End-of-dir mark?
	jz	dw_dir_end		;End of root.
	jmp	short dw_examine
dw_dir_end:
	mov	ah,de_zeros	;Slightly fake # for 'no more entries'.
	xor	al,al		;CF=0.
	jmp	short dw_done
dw_nonroot:
	mov	es,cluster_seg
	nop
	add	di,32			;Next entry.
	mov	ax,32
	mul	dirs_per_cluster	;# dir entries that fit in a cluster.
	cmp	di,ax			;Are we still inside current cluster?
	jb	dw_read 		;Yes.
	mov	bx,tree [si].tcluster	;Get current cluster number.
	call	get_link		;Get FAT entry to find next cluster.
	cmp	ax,fm_bad		;End of chain (any 'reserved' value)?
	jae	dw_dir_end		;No more clusters for this subdir.
	mov	tree [si].tcluster, ax	;Update.
	xor	di,di			;Begin new cluster of this subdir.
dw_read:
	mov	tree [si].toffset, di	;Update.
	or	si,si			;Level = root?
	jz	dw_examine		;Root is always in memory.
	mov	ax,tree [si].tcluster
	call	read_sub_cluster	;Buffer management done internally.
	jnc	dw_examine		;No error.
dw_disk_err:
	stc
	jmp	short dw_done
;
dw_examine:
	cmp	byte ptr es:[di],0	;End of directory?
	jz	dw_dir_end
	mov	ax,di
	call	check_dir_entry 	;Entry at ES:AX.
	test	ah,de_sublink
	jnz	dw_sublink
	jmp	short dw_done	;Return with ES:DI ptg at the file entry.
;
dw_sublink:			;Dir entry was "." or "..".
	or	si,si
	jz	dw_invalid		;Sublinks musn't be in the root!
	mov	dx, tree [si].tcluster
	cmp	dx, tree [si].tcluster0 ;Sublinks OK only in 1st cluster.
	jne	dw_invalid
	mov	dx,es:[di].start_cluster	;Do a few checks on it.
	cmp	byte ptr es:[di].filename+1, "."
	je	dw_sublink2
	cmp	dx, tree [si].tcluster	;Should point at itself.
	jne	dw_invalid
	jmp	short dw_done			;Note CF=false.
dw_invalid:
	mov	ah,de_invalid
	xor	al,al
	jmp	short dw_done
dw_sublink2:			;Dir entry was ".."
	mov	bx,si
	sub	bx,SIZE tree_struc
	cmp	dx,tree [bx].tcluster0	;Previous level.
	jne	dw_invalid		;Back-link doesn't point at 'parent'.
dw_done:
	pop	si
	pop	dx
	pop	cx
	pop	bx
	ret
dir_walk ENDP
;
;--------------------------------------------------------------
; ADD TO THE PATH (and update the level).
;
; On entry: ES:DI points at a subdirectory entry.
; On exit: If success then CF=false, TREE_LEVEL, TREE and PATH are
;	   updated and the new cluster is read into our cluster buffer.
;	   CHECK_CLUSTER is called, and AX returns it's result code.
;	   But if error then CF=true, AX=?.
;
; Only AX is changed.
;
add_to_path PROC NEAR
	mov	ax,di
	call	check_dir_entry
	push	bx
	push	cx
	push	dx
	push	si
	push	es	; * Note ES must be before DI (for seg:offset pair).
	push	di	; *
	push	bp
	mov	bp,sp	;Now [BP+2]=stacked DI, [BP+4]=stacked ES.
;
	cmp	ax,(256*de_live) + dir_attr
	jne	adp_err_stp
	mov	bx,tree_level
	add	bx,SIZE tree_struc		;Advance pointer to next rec.
	cmp	bx,(SIZE tree_struc) * tree_size
	jae	adp_err_stp
	mov	ax,es:[di].start_cluster
	mov	tree [bx].tcluster, ax
	mov	tree [bx].tcluster0, ax
	mov	tree [bx].toffset, -1	;Special value for new path.
;
	mov	si,bx
	cmp	bx,SIZE tree_struc	;First-level subdir?
	jbe	adp1			;Yes, looping tree can't occur yet.
adp_circ_lp:
	sub	si, SIZE tree_struc
	jbe	adp1			;Reached root level.  Done checking.
	cmp	ax, tree [si].tcluster0 ;This subdir occurs twice in the tree?
	je	adp_err 		;Yes!  Invalid tree (circular).
	cmp	ax, tree [si].tcluster
	jne	adp_circ_lp
adp_err_stp:
	jmp	short adp_err
;
adp1:	lea	di,path 	;Next, we add to the text of the path.
	push	cs
	pop	es
	cld
	xor	ax,ax
	mov	cx,SIZE path
	repnz scasb		;Find the 1st zero (the end).
	jnz	adp_err
	mov	dx,bx		;Save new tree_level value into DX.
	lea	bx,[di]
	mov	byte ptr [bx-1],"\"	;Replace 0 with \.
	les	di,dword ptr [bp+2]	;Recover ptr to dir entry.
	call	copy_fname	;From subdir name at ES:DI to CS:BX.
	lea	bx,[bx] 	;Just to see it in debug.
	cmp	path_guard,-1	;Path has grown too long?
	je	adp2		;OK.
	mov	byte ptr [bx-1],0	;Reject addition to path.
	jmp	short adp_err
;
adp2:	mov	si,dx		;Fetch new tree_level into SI.
	mov	bx,tree [si].tcluster
	call	get_link
	cmp	ax,fm_free	;Validate the FAT entry for this cluster.
	je	adp_err 	;Cluster is unallocated!
	cmp	ax,fm_bad
	je	adp_err 	;Cluster is marked bad!
	mov	ax,bx
	call	read_sub_cluster
	jc	adp_err
	call	check_cluster
	cmp	ax,fm_sub_start ;Fm_sub_start, _end or _sub_nul are expected.
	je	adp3
	cmp	ax,fm_end
	je	adp3
	cmp	ax,fm_sub_nul
	jne	adp_err
adp3:	mov	tree_level,si	;Update TREE_LEVEL (at last!).
	clc			;Success (CF=false).
	jmp	short adp_exit
adp_err:
	stc
adp_exit:
	pop	bp
	pop	di
	pop	es
	pop	si
	pop	dx
	pop	cx
	pop	bx
	ret
add_to_path ENDP
;
;-----------------------------------------------------------
; Subtract the last subdir from path name
; (shorten the path) and back up tree level by 1.
;
; On entry: nothing.
; On exit: If new level = 0 then ZF=true, else ZF=false.
;
; All regs saved.
;
sub_from_path PROC NEAR
	push	ax
	push	bx
	push	cx
	push	di
	push	es
;
	push	cs
	pop	es
	cld
	mov	bx,tree_level
	sub	bx,SIZE tree_struc
	ja	subp0		;Not root.
	xor	ax,ax
	xor	bx,bx
	mov	tree_level,ax	;0.  Root.
	mov	word ptr path,ax
	lea	di,path
	mov	cx,SIZE path
	rep stosb
	jmp	short subp3
subp0:	mov	tree_level,bx
	lea	di,path
	mov	cx,SIZE path
	xor	ax,ax
	repnz scasb		;Find final 0.
	mov	ax,cx
	mov	cx,SIZE path
	sub	cx,ax

ifdef DBCS		; ### if DBCS ###
	push	si
	lea	si,path
	xor	ah,ah
subp1:	lea	di,[di-1]
	mov	al,[di]
	mov	[di],ah
	call	CheckDBCSTailByte
	jz	subp1a			; if this is tail byte
	cmp	al,'\'
	jz	subp1b			; if '\' is found
subp1a:
	loop	subp1			; do next
subp1b:
	pop	si

else			; ### if Not DBCS ###

	mov	al,"\"
subp1:	lea	di,[di-1]
	cmp	al,[di] 	;"\" ?
	mov	[di],ah 	;Zero.
	loopne	subp1		;Continue until we replace a \.
endif			; ### end if Not DBCS ###

subp3:	or	bx,bx
	pop	es
	pop	di
	pop	cx
	pop	bx
	pop	ax
	ret
sub_from_path ENDP
;
;--------------------------------------------------------------------
; Check the length of a file against the number
; of contiguous free clusters in the FAT.
;
; On entry: ES:DI points at a directory entry.
;
; On exit: If enough contiguous space is free then CF=false and
;	   AX = number of clusters which will be needed for the file.
;	   But if not, then CF=true and AX= max contiguous file size.
;
; Only AX is changed.
;
check_contig_free PROC NEAR
	push	bx
	push	cx
	push	dx
	push	di
	mov	ax,sector_size
	mul	cluster_size
	mov	cx,ax
	mov	ax,es:[di].file_size
	mov	dx,es:[di].file_size+2
	mov	bx,es:[di].start_cluster
	div	cx		;File_size / (bytes/cluster) = # clusters.
	call	roundup
	mov	di,ax		;DI= desired # clusters.
	or	di,di
	jz	chfr_done		;Zero length file.
	mov	cx,cluster_cnt_plus_1	;Max cluster #.
	sub	cx,bx		;Max # of FAT entries to examine.
	mov	dx,1		;Already know 1st one is allocated.
	cmp	dx,di
	je	chfr_enuf	;1-cluster file.  Done.
chfr_lp:
	inc	bx
	call	get_link
	cmp	ax,fm_free	;Is this cluster free for use?
	jne	chfr_not_free
	inc	dx		;Found one more free cluster.
	cmp	dx,di		;Enough?
	je	chfr_enuf
chfr_2: loop	chfr_lp 	;Loop until enough, or end of FAT.
	jmp	short chfr_fail ;Never got enough.
chfr_enuf:
	clc			;Success.
	jmp	short chfr_done
chfr_not_free:
	cmp	ax,fm_bad	;Is the non-free cluster just a bad one?
	je	chfr_2		;Yes.  Skip bad clu and keep looking.
chfr_fail:
	stc
chfr_done:
	mov	ax,dx
	pop	di
	pop	dx
	pop	cx
	pop	bx
	ret
check_contig_free ENDP
;
;-----------------------------------------------------------------------
; Complete a file's contiguous FAT chain.
;
; On entry: ES:DI points to the directory entry and
;	    CX= number of clusters (total) for the file.
;	    Note: this routine does not read the FAT links before
;	    overwriting them, so the caller must check beforehand.
;
; On exit: nothing.  All regs preserved.
;
complete_chain PROC NEAR
	jcxz	compch_exit
	push	ax
	push	bx
	push	cx
	push	si
	push	di
	mov	si,es:[di].start_cluster
	mov	di,si		;DI=entry to be written, SI=entry under test.
	cmp	cx,1
	je	compch_end
	dec	cx		;Decr count.  Final one is special.
compch_lp:
	inc	si		;Next clu #.
	mov	bx,si
	call	get_link
	cmp	ax,fm_bad	;Bad cluster?
	je	compch_lp	;Skip it.  Look at next one.
	mov	bx,di
	mov	ax,si		;Value stored = number of next free cluster.
	call	store_link
	mov	di,si
	loop	compch_lp
compch_end:
	mov	bx,di
	mov	ax,fm_end
	call	store_link
	pop	di
	pop	si
	pop	cx
	pop	bx
	pop	ax
compch_exit:
	ret
complete_chain ENDP
;
;--------------------------------------------------------------
; Round up the 16-bit quotient after an unsigned division.
;
; On entry: AX,DX = result of DIV.
; On exit: if DX was non-zero, then AX is incremented.
;
roundup PROC NEAR
	push	cx
	xor	cx,cx
	cmp	cx,dx		;CF=true if 0 is below DX.
	adc	ax,0
	pop	cx
	ret
roundup ENDP
;
;-------------------------------------------------------------------
; Announce UnFormat name & version #.  DX is destroyed.
;
;show_banner PROC NEAR          ;M000 - function removed
;lea	dx,banner               
;call	pr_text                 
;	ret
;show_banner ENDP
;
;-------------------------------------------------------------------
; If the printing option was selected, announce that fact.
; DX is destroyed.
;
display_print_opt PROC NEAR
	test	print_flags, pf_allowed
	jz	disp_p_o_end
	lea	dx,msg_print		;"Output will be echoed to LPT1."
	call	pr_text
disp_p_o_end:
	ret
display_print_opt ENDP



HAVE_DISKETTE_INSERTED PROC NEAR
	 CMP   DOS_VER,0300H
	 JB    H_D_I_EXIT

	 MOV   BL,DRIVE
	 INC   BL
	 MOV   AX,4408H		;Check if removable.
	 INT   21H
	or	ax,ax
;	 CMP   AX,0
	 JNE   H_D_I_EXIT

	mov	al,drive
	add	al,"A"
	 LEA   DX,MSG_INSERT_DISK	;"Insert disk in drive @0a:"
	call	display
;	 MOV   AH,9
;	 INT   21H

H_D_I_RETRY:
	mov	ax,0C00h
	int	21h
	 MOV   ah,01h		;Get one key via DOS.
	 INT   21H

	 CMP   AL,13		;Return key?
	 JNE   H_D_I_RETRY

H_D_I_EXIT:
	 RET

HAVE_DISKETTE_INSERTED ENDP



;
;--------------------------------------------------------------------
; On entry: DRIVE and DOS_VERSION are assumed valid.
;
; On exit: if it's a network drive then CF=true.  Otherwise CF=false.
;
; Destroys AX,BX,CX,DX,SI,DI.
;
check_network_drive PROC NEAR		; Added 05-01-89, GWD.
	push	bp
	push	ds
	push	es
	cld
	cmp	dos_ver,300h
	jb	chknet_not_ibm
	xor	ax,ax
	int	2Ah		;Is network installed?
	or	ah,ah
	jz	chknet_not_ibm	;No.
	mov	bl,drive
	mov	bh,0
	inc	bx
	mov	ax,4409h	;Is this drive remote?
	int	21h
	jc	chknet_not_ibm
	test	dx,1000h
	jnz	chknet_error	;It's remote.  Can't touch it.
	mov	al,drive
	add	al,"A"
	mov	net_string,al
	lea	si,net_string
	clc
	mov	ax,0300h
	int	2Ah		;Returns CF=true when INT25h is illegal.
	jmp	short chknet_done
chknet_not_ibm: 		;Look for Novell Netware 286.
	mov	cx,-1
	clc
	mov	ax,0DC00h	;'Request Novell connection #'.
	int	21h
	jc	chknet_ok	;Novell is not there.
	cmp	cx,-1
	je	chknet_ok
	mov	si,-1
	clc
	mov	ax,0EF01h	;'Get Novell drive table'.
	int	21h		;Returns ES:SI = pointer to table.
	jc	chknet_ok
	cmp	si,-1
	je	chknet_ok
	mov	bl,drive
	mov	bh,0
	test	byte ptr es:[bx+si],80h ;Is it a local drive?
	jz	chknet_error		;No - cannot process it.
chknet_ok:
	clc
	jmp	short chknet_done
chknet_error:
	stc
chknet_done:
	pop	es
	pop	ds
	pop	bp
	cld
	sti
	ret
check_network_drive ENDP
;
net_string	DB	"x:\",0
;
;


ifdef DBCS		; ### if DBCS ###
;--------------------------------------------------------------------
;
;	Test if the character is DBCS Lead Byte
;
;	input:	AL = character to check
;	outpit:	ZF = 1 if DBCS Lead Byte
;

DBCSLeadByteTable	dd	0

IsDBCSLeadByte		proc	near
	push	ax
	push	si
	push	ds
	lds	si,cs:DBCSLeadByteTable
	cmp	word ptr cs:DBCSLeadByteTable+2,0
	jnz	idlb_check		; if table is already set
	push	ax
	mov	ax,6300h
	int	21h			; get DBCS lead byte table
	pop	ax
	mov	word ptr cs:DBCSLeadByteTable,si
	mov	word ptr cs:DBCSLeadByteTable+2,ds
idlb_check:
	cmp	word ptr [si],0
	jz	idlb_not		; if end of table
	cmp	al,[si]
	jb	idlb_next		; if below low value
	cmp	al,[si+1]
	jbe	idlb_yes		; if below high value
idlb_next:
	add	si,2			; do next
	jmp	short idlb_check
idlb_not:
	or	al,1			; reset ZF
	jmp	short idlb_end
idlb_yes:
	and	al,0			; set ZF
idlb_end:
	pop	ds
	pop	si
	pop	ax
	ret
IsDBCSLeadByte		endp

;
;	Test if the character is DBCS Tail Byte
;
;	input:	AL = character to check
;	outpit:	ZF = 1 if DBCS Tail Byte
;
IsDBCSTailByte		proc	near
	push	ax
	push	si
	lea	si,tail_byte_table
idtb_check:
	cmp	word ptr cs:[si],0
	jz	idtb_not		; if end of table
	cmp	al,cs:[si]
	jb	idtb_next		; if below low value
	cmp	al,cs:[si+1]
	jbe	idtb_yes		; if below high value
idtb_next:
	add	si,2			; do next
	jmp	short idtb_check
idtb_not:
	or	al,1			; reset ZF
	jmp	short idtb_end
idtb_yes:
	and	al,0			; set ZF
idtb_end:
	pop	si
	pop	ax
	ret
IsDBCSTailByte		endp

tail_byte_table		label	byte
ifdef JAPAN
	db	40h,7eh
	db	80h,0fch
	dw	0
endif
ifdef TAIWAN
	db	40h,7eh
	db	0a1h,0feh
	dw	0
endif
ifdef KOREA
	db	0a1h,0abh
	db	0b0h,0c8h
	db	0cah,0fdh
	dw	0
endif

;
;	Check if the character position is at Tail Byte of DBCS
;
;	input:	ds:si = start address of the string
;		ds:di = character position to check
;	output:	ZF = 1 if at Tail Byte
;
CheckDBCSTailByte	proc	near
	push	ax
	push	cx
	push	di
	mov	cx,di			; save character position
cdtb_check:
	cmp	di,si
	jz	cdtb_next		; if at the top
	dec	di			; go back
	mov	al,[di]			; get character
	call	IsDBCSLeadByte
	jz	cdtb_check		; if DBCS lead byte do next
	inc	di			; adjust
cdtb_next:
	sub	cx,di			; if the length is odd then
	xor	cl,1			; the character position is
	test	cl,1			; at the tail byte
	pop	di
	pop	cx
	pop	ax
	ret
CheckDBCSTailByte	endp
endif		; ### end if DBCS ###


;--------------------------------------------------------------------
; Table of possible command line parms/options.
; Note: for parsing to work properly with similar keywords
;	like "/LIST" and "/L", the longer one must be first
;	in the list.
;
option_table LABEL byte
 opt_def <options, opt_wrfake, action_switch, 5, "/TEST">
 opt_def <options, opt_partn, action_switch, 6, "/PARTN">
 opt_def <options, opt_j, action_switch, 2, "/J">
 opt_def <options, opt_u, action_switch, 2, "/U">
 opt_def <print_flags, pf_allowed, action_switch, 2, "/P">
 opt_def <options, opt_list, action_switch, 2, "/L">
;opt_def <options, opt_keep_fat+opt_k+opt_f, action_switch, 3, "/KF">
;opt_def <options, opt_keep_root+opt_k+opt_r, action_switch, 3, "/KR">
;opt_def <options, opt_erase_fat+opt_f, action_switch, 3, "/EF">
;opt_def <options, opt_erase_root+opt_r, action_switch, 3, "/ER">
 opt_def <0,0,0>
;
;------------------------------------------------------------------------
int25_error_list LABEL byte
	i25_error_struc <0, msg_i25_wrprot>
	i25_error_struc <1, msg_i25_unit>
	i25_error_struc <2, msg_i25_not_ready>
	i25_error_struc <3, msg_i25_bad_cmd>
	i25_error_struc <4, msg_i25_crc>
	i25_error_struc <5, msg_i25_req>
	i25_error_struc <6, msg_i25_seek>
	i25_error_struc <7, msg_i25_media>
	i25_error_struc <8, msg_i25_rnf>
	i25_error_struc <9, msg_i25_paper>
	i25_error_struc <0Ah, msg_i25_readf>
	i25_error_struc <0Bh, msg_i25_writef>
	i25_error_struc <0Ch, msg_i25_general>
	i25_error_struc <-1>			;End of list.
;
;------------------------------------------------------------------------
	EVEN	;Make sure it does NOT align, in the program file.
there	=$
zapsav_text DB	"zSav",0	;This is the special mark for dir entries
zapsav_length =$-there		;zapped by the CPS Formatter.
;
subdir	dir_str <"SUBDIR  ","   ",dir_attr>	;Template for dir entries.
;
prog	ENDS
	END


