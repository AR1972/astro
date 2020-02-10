; Copyright 1990 Central Point Software, Inc. All rights reserved.
;--------------  'Include file' for UNFORMAT.  ------------------------
;
dir_str	STRUC
filename DB	"        "
extension DB	"   "
file_attr DB	0		;Attribute bits are defined below.
dir_reserved DB	4 DUP (0)	;10 reserved bytes total.
zapsav_loc DB	0		;Used by CPS Formatter to keep 1st char.
	DB	5 DUP (0)	;Last 5 reserved bytes.
time	DW	0
date	DW	0
start_cluster DW 0FFFFh
file_size DW	0,0
dir_str	ENDS
;
;
ro_attr	EQU	1		;File attribute bits.
hide_attr EQU	2
sys_attr EQU	4
vol_attr EQU	8
dir_attr EQU	10h
arch_attr EQU	20h
;
; The following FAT 'reserved' cluster numbers have special meanings:
;
fm_free		EQU	0	;Free cluster (same as DOS).
fm_bad		EQU	0FFF7h	;FAT Mark for bad cluster (same as DOS).
fm_sub_start	EQU	0FFF8h	;Cluster begins a subdir ("." & "..").
fm_sub_mid	EQU	0FFF9h	;Clu contains dir entries, but not ".."
fm_sub_tail	EQU	0FFFAh	;Like sub_mid, but we also know it's the last.
fm_file_start	EQU	0FFFBh	;Clu is pointed at by some file's dir entry.
fm_file_del	EQU	0FFFCh	;Clu belonged to a file which we deleted.
fm_sub_nul	EQU	0FFFDh	;Clu is a complete subdir, but has no files.
; * unused	EQU	0FFFEh
fm_end		EQU	0FFFFh	;End of any FAT chain (same as DOS).
;
; Bit flags (stored in OPTIONS) from command line options:
;
opt_list	EQU	2	;Verbose list of files during Walk1.
opt_keep_fat	EQU	4	;Assume FAT is valid, keep it intact.
opt_keep_root	EQU	8	;Assume root is valid, make minimal changes.
opt_erase_fat	EQU	10h	;Assume FAT is crap, don't bother reading it.
opt_erase_root	EQU	20h	;Assume root is crap, ignore it.
opt_k		EQU	40h	;Keep something.  Detects conflicting options.
opt_f		EQU	80h	;Option references FAT.  Detects conflicts.
opt_r		EQU	100h	;Root option.  Used to detect conflicts.
opt_j		EQU	200h	;/J invokes Jim's 'just checking' function.
opt_u		EQU	400h	;/U jumps into UnFormat, skipping Jim's.
opt_partn	EQU	800h	;/PARTN restores partition tables from floppy.
opt_wrfake	EQU	8000h	;Don't really write to disk.  (For testing.)
;
; The following are status codes returned in AH by proc CHECK_DIR_ENTRY:
;
de_zeros EQU	1	;All zeros (may be an unused subdir entry).
de_live	EQU	2	;A valid live entry.
de_deleted EQU	4	;Deleted entry (first byte of name = E5h).
de_zapped EQU	8	;Zapped valid entry (first byte = 00).
de_sublink EQU	10h	;Entry "." or ".." (allowed only if input AX=0 or 32).
de_zapsaved EQU	20h	;Zapped by CPS Format - 1st char saved.
de_invalid EQU	80h	;Not valid (can't be a directory entry).
;
;
; Equates & structure for the format of the command line parms:
;
max_optdef_text EQU 8
;
opt_def STRUC		;This is for a single parm.  Repeat for each parm.
optdef_var_ptr DW 0	;If NZ, pointer to this parm's variable's word.
optdef_mask   DW 0	;Only used for switch-type parms.
optdef_action DB 0	;Action bits are defined below.
optdef_length DB 0	;Length of parm text (e.g. for "/SIZE=", length=6).
				;Uppercase text of the parm follows.
optdef_text DB	"        "	;max_optdef_text blanks.
opt_def ENDS
;
; Bit flags within PARM_ACTION byte of an OPT_DEF structure:
;
action_switch EQU 80h	;Switch only (OPT_MASK will be returned).
action_k EQU	   4	;Trailing K is allowed if action_dec is used.
action_dec EQU	   1	;Decimal integer number is expected.
;
; If optdef_action = 0, then this invalid struc is the end of the list.
;
; - End of include-file. -
