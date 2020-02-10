; Copyright 1990 Central Point Software. Inc. 
; All rights reserved.
;----------------------------------------------------
;
;       M001    MD      10/14/90        New help message, some text cleanup
; 12-06-90 GWD	Cleaned up MSG_INSERT_DISK.
; 12-06-90 GWD	Modified messages to handle partition restore on >2 drives.
;       M007    MD      02/21/91        Corrected help text error
;
; UnFormat.  Message module.
;
;----------------------------------------------------
prog	SEGMENT public
	ASSUME	CS:prog
;
	PUBLIC	yes_char, no_char, quit_char
	PUBLIC	delete_char, truncate_char, all_char
	PUBLIC	text_yes_length
	 PUBLIC TEXT_YES
;;	 PUBLIC INSERT_DRV
	 PUBLIC MSG_INSERT_DISK
	 PUBLIC MSG_I25_WRITEF
	 PUBLIC MSG_PARTN_QUERY
	 PUBLIC MSG_DELETE
	 PUBLIC MSG_WALK1
	 PUBLIC MSG_CROSSLINK
	 PUBLIC MSG_WALK2
	 PUBLIC MSG_REBOOT
;M005	 PUBLIC MSG_DIFF_DRIVES
	public	MSG_DIFF_DRIVES_SING
	public	MSG_DIFF_DRIVES_PLURAL
	public	msg_diff_parms
	 PUBLIC MSG_SURE
	 PUBLIC MSG_PNOBOOT
	 PUBLIC MSG_IGNORING
	 PUBLIC MSG_DOS12
	 PUBLIC MSG_EXAMINED_ENT
	 PUBLIC MSG_RP_DRV_QUERY
	public	msg_menu
	 PUBLIC MSG_MENU1
	 PUBLIC MSG_MENU2
	 PUBLIC MSG_USING_DRV
	 PUBLIC MSG_DOS16
	 PUBLIC MSG_DOS_EXTEND
	 PUBLIC MSG_I25_REQ
	 PUBLIC MSG_I25_GENERAL
	 PUBLIC MSG_FILE_ERR
	 PUBLIC MSG_VIA_TABLE
	 PUBLIC MSG_PARTN_SIZE
	 PUBLIC MSG_I25_SEEK
	 PUBLIC MSG_FILES_REC
	 PUBLIC MSG_PARTN_HEADER
	 PUBLIC MSG_DOS_HUGE
	 PUBLIC MSG_SEARCHING
	 PUBLIC MSG_READ
	 PUBLIC MSG_BIOS_ERR
	 PUBLIC MSG_I25_PAPER
	 PUBLIC MSG_WRITE_WARN
	 PUBLIC MSG_ASK_TRUNC
	 PUBLIC MSG_DIR
	 PUBLIC MSG_BAD_DRIVE
	 PUBLIC MSG_VOL
	 PUBLIC MSG_FAT_PROT
;M005	 PUBLIC MSG_PROGRESS
	public	MSG_PROGRESS_SING,MSG_PROGRESS_PLURAL
	 PUBLIC MSG_BAD_AA55
	 PUBLIC MSG_NO_HARD_DRIVES
	 PUBLIC MSG_OLD_PARTN
	 PUBLIC MSG_PARTN_REL
	 PUBLIC MSG_STRANGE_PARTN
	 PUBLIC MSG_DOS
	 PUBLIC MSG_FILES
	 PUBLIC MSG_I25_WRPROT
	 PUBLIC MSG_ABORT
	 PUBLIC MSG_NO_ACTION
	 PUBLIC MSG_I25_UNIT
	 PUBLIC MSG_BAD_PARMS
	 PUBLIC MSG_PARTN_EDGE
	 PUBLIC MSG_LISTP_TITLE
	 PUBLIC MSG_HELP
	 PUBLIC MSG_WARNING
	 PUBLIC MSG_ROOT_PROT
	 PUBLIC MSG_PBOOT
	 PUBLIC MSG_WRITE_FAKE
	 PUBLIC MSG_WRITE
	 PUBLIC MSG_STRANGE_DISK
	 PUBLIC MSG_PARTN_FROM_SEC
	 PUBLIC MSG_STOP_HUNT
	 PUBLIC MSG_PARTN_DATE
	 PUBLIC MSG_PRINT
	 PUBLIC MSG_I25_BAD_CMD
	 PUBLIC MSG_SUBDIRS_FOUND
	 PUBLIC MSG_NOTHING_FOUND
	 PUBLIC MSG_ERASE_FAT
	 PUBLIC MSG_DISK_PARMS
	 PUBLIC MSG_I25_NOT_READY
	 PUBLIC MSG_DONE
	 PUBLIC MSG_SYS_READ_ERR
	 PUBLIC MSG_I25_CRC
	 PUBLIC MSG_NEWER_VER
	 PUBLIC MSG_PATH
	 PUBLIC MSG_VIA_BIOS
	 PUBLIC MSG_ONLY
	 PUBLIC MSG_RP_PROMPT
	 PUBLIC MSG_ROOT_FILES
	 PUBLIC MSG_I25_READF
	 PUBLIC MSG_ERROR
	 PUBLIC MSG_ARROW
	 PUBLIC MSG_RP_TITLE
	 PUBLIC MSG_I25_MEDIA
	 PUBLIC MSG_ERASE_ROOT
	 PUBLIC MSG_NETWORK
	 PUBLIC MSG_TRUNC
	 PUBLIC MSG_SMALL_MEM
	 PUBLIC MSG_I25_RNF
;
cr	EQU	13
lf	EQU	10
;
; These characters (initials of words) are defined here to
; make foreign versions easier.  These definitions will effect
; the whole program.  Be sure that they match the translated
; messages in this file.  They MUST be in upper case.
;
yes_char	EQU	"Y"
no_char 	EQU	"N"
quit_char	EQU	"Q"
delete_char	EQU	"D"
truncate_char	EQU	"T"
all_char	EQU	"A"
;
;----------------------------------------------------------------------
; This entire message can now be translated.
;
; Notes:
;  1. The string named text_yes must be EXACTLY the same as the
;     example given in the prompt message.
;  2. The two 8's after "No" are backspaces.  They reposition the
;     cursor so that whatever the user types, it will overwrite the "No"
;     on the screen.  If the translation of "No" requires a different
;     number of characters, then the number of backspaces must be
;     changed to match the length of the foreign negative word.
;
;msg_sure	 DB	 cr,lf,"Are you SURE you want to do this?",cr,lf
;		 DB	 "If so, type in YES; anything else cancels.",cr,lf
;		 DB	 "? No",8,8,0
;;
;text_yes	 DB	 "YES"
;;
;text_yes_length EQU	 $-text_yes	 ;Do not change this.

msg_sure	DB	cr,lf,"Are you sure you want to do this?",cr,lf
		DB	"If so, press Y; anything else cancels.",cr,lf
		DB	"? N",8,0

text_yes	DB	"Y"

text_yes_length EQU	$-text_yes	;Do not change this.

MSG_INSERT_DISK DB cr,lf,"Insert disk to rebuild in drive @0a:",cr,lf
	DB	"and press ENTER when ready.",cr,lf,cr,lf,0

;----------------------------------------------------------------------
msg_warning LABEL byte
 DB cr,lf
 DB "  CAUTION !!",cr,lf
 DB "This attempts to recover all the files lost after a",cr,lf
 DB "format, assuming you've not been using the MIRROR command.",cr,lf
 DB "This method cannot guarantee complete recovery of your files.",cr,lf
 DB cr,lf
 DB "The search-phase is safe: nothing is altered on the disk.",cr,lf
 DB "You will be prompted again before changes are written to the disk.",cr,lf
 DB cr,lf,0
;
msg_bad_drive	DB	"Invalid or unspecified drive.",cr,lf,0
msg_dos 	DB	"Incorrect DOS version.",cr,lf,0
msg_network	DB	"Cannot process network drive.",cr,lf,0
msg_bad_parms	DB	"Invalid option(s).",cr,lf,0
msg_fat_prot	DB	"FAT will be preserved.",cr,lf,0
msg_root_prot	DB	"Root directory will be preserved.",cr,lf,0
msg_erase_fat	DB	"FAT will be replaced.",cr,lf,0
msg_erase_root	DB	"Root directory will be replaced.",cr,lf,0
msg_write_fake	DB	"Simulation only.",cr,lf,0
msg_strange_disk DB	"Cannot process a network, SUBST, or unrecognized disk.",cr,lf,0        ;M007
msg_small_mem	DB	"Not enough memory.",cr,lf,0
msg_no_action	DB	"No action taken.",cr,lf,0
msg_sys_read_err DB	"Cannot read system area of disk.",cr,lf,0                              ;M007
;
;------------------------------------------------------------------
msg_error	DB	"Error @1t sector# @2w@3wh, code @4wh@5t.",cr,lf,0
;
; The @1t will be replaced by one of the following.
;
msg_read	DB	"reading",0
msg_write	DB	"writing",0
;
; The @5t will be replaced by one of the following.
; The msg_i25_xxx were all added in v6.
;
msg_i25_wrprot	DB	" write-protect",0
msg_i25_unit	DB	" bad unit#",0
msg_i25_not_ready DB	" not ready",0
msg_i25_bad_cmd DB	" bad command",0
msg_i25_crc	DB	" data error",0
msg_i25_req	DB	" bad request structure",0
msg_i25_seek	DB	" bad seek",0
msg_i25_media	DB	" unknown media",0
msg_i25_rnf	DB	" sector not found",0
msg_i25_paper	DB	" no paper",0
msg_i25_writef	DB	" write fault",0
msg_i25_readf	DB	" read fault",0
msg_i25_general DB	" general failure",0
;
;------------------------------------------------------------------
; The first two characters must not be changed.
;
msg_arrow	DB	"^ Error",cr,lf,0
;
;------------------------------------------------------------------
; Each of these messages must be exactly nine characters long!
; All eight characters may be translated as needed.  For example,
; "<abcdefg>" would be OK.
;
msg_dir 	DB	"  <DIR>  "		; For sub-directory item.
		DB	0
msg_vol 	DB	"  <VOL>  "		; For disk volume label.
		DB	0
;
;------------------------------------------------------------------
msg_examined_ent DB	"Examined @0d root entries",cr,lf,0
;
msg_root_files	DB	"Files found in the root: @0d",cr,lf,0
msg_subdirs_found DB	"Subdirectories found in the root: @0d",cr,lf,0
;
msg_searching	DB	cr,lf,"Searching disk...",cr,lf,0
;
;M005 -- made the following into two separate messages
;M005msg_progress	DB	"@0d% searched, @1d subdir@1s found.",cr,0
;
;M007 -- Note that msg_progress_sing overwrites msg_progress_plural
; when the first subdirectory is found.  This leaves an artifact bit
; of text displayed.  To fix this, _sing must be the same length as
; _plural.  Here it is padded with blanks.  Translators beware!

msg_progress_sing   DB	"@0d% searched, 1 subdirectory found.    ",cr,0         ;M007
msg_progress_plural DB	"@0d% searched, @1d subdirectories found.",cr,0
;
msg_nothing_found DB "No files or subdirectories found for the root.",cr,lf,0
msg_abort DB	cr,lf,"* Cancelled by user *",cr,lf,0
msg_walk1 DB cr,lf,"Walking the directory tree to locate all files...",cr,lf,0
msg_walk2 DB cr,lf,"Checking for file fragmentation...",cr,lf,0
msg_crosslink DB "Deleting crosslinked file.",cr,lf,0
;
msg_only	DB	"Only @1l bytes are recoverable",cr,lf,0
;
;----------------------------------------------------------------------
; This is a prompt for user input.  The allowed English answers are:
;
; "DELETE" or "TRUNCATE" or "DELETE ALL" or "TRUNCATE ALL"
;
; Actually, only the initials of the words are checked by this program.
; So, either "D A" or "Dxxx Azzz" will be taken as "DELETE ALL".
;
; If the foreign translated key-words begin with other letters, then
; the message must indicate what initials the user should type.
; Be sure to change the definitions (EQU's) of delete_char,
; truncate_char and all_char to match the chosen initials.
;
msg_ask_trunc	DB	"Truncate or Delete this file? ",0
;
;----------------------------------------------------------------
msg_delete	DB	"File deleted.",cr,lf,0
msg_trunc	DB	"File size truncated.",cr,lf,0
msg_write_warn	DB "Warning!  The next step writes changes to disk.",cr,lf,0
;
msg_files	DB	lf,"Files found: @0d",cr,lf,0
;
msg_files_rec	DB	"@0d files recovered.",cr,lf,0
;
msg_done	DB	cr,lf,"Operation completed.",cr,lf,0
msg_stop_hunt	DB	cr,lf,"Finish remainder of search (Yes/No/Quit)? ",0
msg_ignoring	DB	"Ignoring this subdirectory.",cr,lf,0
msg_print	DB	"Printout will be sent to LPT1.",cr,lf,cr,lf,0
;

;M001 - new help message
;M004 - newer help message

msg_help  db    "Restores a disk erased by the FORMAT command or restructured by the RECOVER",cr,lf
          db    "command.",cr,lf
          db    cr,lf
          db    "UNFORMAT drive: [/J]",cr,lf
          db    "UNFORMAT drive: [/U] [/L] [/TEST] [/P]",cr,lf
          db    "UNFORMAT /PARTN [/L]",cr,lf
          db    cr,lf
          db    "  drive:   Specifies the drive to unformat.",cr,lf
          db    "  /J       Verifies that the mirror files agree with the system information",cr,lf
          db    "           on the disk.",cr,lf                                                                 ;M007
          db    "  /U       Unformats without using MIRROR files.",CR,LF
          db    "  /L       Lists all file and directory names found, or, when used with the",cr,lf 
          db    "           /PARTN switch, displays current partition tables.",cr,lf
          db    "  /TEST    Displays information but does not write changes to disk.",cr,lf
          db    "  /P       Sends output messages to printer connected to LPT1.",cr,lf
          db    "  /PARTN   Restores disk partition tables.",cr,lf,lf
          db    "MIRROR, UNDELETE, and UNFORMAT Copyright (C) 1987-1993 Central Point Software,",cr,lf
          DB	"Inc.",cr,lf,0

;M004 end

;
msg_path	DB	"Path=@0t\",cr,lf,0
msg_using_drv	DB	"Using drive @0a:",cr,lf,0
;
;--------------------------------------------------------------------------
; The following are messages for the PARTITION RESTORATION module.
;
msg_rp_title	DB	"Hard Disk Partition Table restoration.",cr,lf,0
msg_no_hard_drives DB	"No hard drives found.",cr,lf,0
msg_rp_prompt	LABEL byte
	DB	cr,lf
	DB	"Insert the disk containing the file PARTNSAV.FIL",cr,lf
	DB	"and type the letter of that disk drive.",cr,lf,0
msg_rp_drv_query DB	"What drive? A",8,0
msg_file_err	DB	"Error reading the file.",cr,lf,0
msg_newer_ver	DB	"Incompatible file version #.",cr,lf,0
;
;M005 -- made the following into two separate cases for singular/plural

;M005msg_diff_drives LABEL byte
;M005	DB	cr,lf
;M005	DB	"Warning!  Partitions were saved from @1d fixed disk@1s, "
;M005	DB	"but this system has @2d.",cr,lf,0

msg_diff_drives_sing LABEL byte
	DB	cr,lf
	DB	"Warning!  Partitions were saved from 1 fixed disk, "
	DB	"but this system has @2d.",cr,lf,0

msg_diff_drives_plural LABEL byte
	DB	cr,lf
	DB	"Warning!  Partitions were saved from @1d fixed disks, "
	DB	"but this system has @2d.",cr,lf,0

msg_diff_parms LABEL byte
	DB	cr,lf
	DB	"The saved information is incompatible with fixed disk @0d.",cr,lf
	DB	"Restoration is not possible; drive @0d is skipped.",cr,lf
	DB	7,cr,lf,0
;
msg_partn_date LABEL byte
 DB	cr,lf
 DB "Partition information was saved by MIRROR @1t, ",0
;
msg_old_partn DB	cr,lf
 DB	"Old partition information for fixed disk # @1d (DL=@2bh):"
 DB	cr,lf,0
;
msg_menu LABEL byte
 DB	cr,lf
 DB	"Options:  Q  =  Quit, take no action.",cr,lf,0
msg_menu1 LABEL byte
 DB	"          1  =  Restore the partitions for fixed disk 1.",cr,lf
 DB	cr,lf,0
msg_menu2 LABEL byte
 DB   "      1 - @0d  =  Restore partitions for one selected fixed disk.",cr,lf
 DB	"          A  =  Restore partitions for all fixed disks.",cr,lf
 DB	cr,lf
 DB	0
;
msg_partn_query DB	"Which option? Q",8,0
;
msg_bios_err LABEL byte
;;	DB	"** Disk error, BIOS code @0bh.",cr,lf,0
 DB	"** Disk error (BIOS code @1bh) at cylinder @2wh, drive @3bh.",cr,lf,0
;
msg_reboot LABEL byte
 DB "Operation completed.",cr,lf
 DB cr,lf
 DB "Insert a DOS boot disk in drive A and press ENTER to reboot...."
 DB 0
;
;----------------------------------------------------------------
; The following texts are for the LIST PARTITIONS option.  v5.00  12-7-88.
;
msg_listp_title DB	"Hard Disk Partition Table display.",cr,lf,0
;
msg_disk_parms LABEL byte
 DB	"Drive # @0bh has @1d cylinders, @2d heads, @3d sectors (from @4t)."
 DB	cr,lf,0
;
msg_via_bios	DB	"BIOS",0
msg_via_table	DB	"Table",0
;
msg_partn_from_sec LABEL byte
  DB	cr,lf
  DB	"The following table is from "
  DB	"drive @4bh, cylinder @1d, head @2d, sector @3d:"
  DB	cr,lf,0
;
msg_partn_header LABEL byte
 DB cr,lf
 DB "                Total_size       Start_partition   End_partition",cr,lf
 DB "   Type       Bytes   Sectors    Cyl Head Sector   Cyl Head Sector  "
 DB "   Rel#",cr,lf	; * Refers to 'Relative sector number'.
 DB "-----------  -----------------  ----------------  ----------------  "
 DB "----------",cr,lf
 DB 0
;
msg_dos16	DB	"DOS16 ",0		;Length must not change.
msg_dos12	DB	"DOS12 ",0		;Length must not change.
msg_dos_huge	DB	"HUGE  ",0		;Length must not change.
msg_dos_extend	DB	"EXTEND",0		;Length must not change.
msg_pboot	DB	"Boot  ",0		;Length must not change.
msg_pnoboot	DB	"      ",0		;Length must not change.
;
msg_strange_partn DB	" @1bh? @2bh?  ",0	;Do not translate.
msg_partn_size	DB	"@1r@2a @3l  ",0	;Do not translate.
msg_partn_edge	DB	"@1r@2r @3r  ",0	;Do not translate.
msg_partn_rel	DB	" @1l",cr,lf,0		;Do not translate.
;
msg_bad_AA55	DB	"Invalid boot record signature.",cr,lf,0
;
prog	ENDS
	END

