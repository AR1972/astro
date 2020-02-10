	Page 84,132 ;

TITLE	FASTINIT - initialization code for FASTOPEN  (May 13, 1988)
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

; IBM changes incoporated during 11/89 code merge:
;	      AN002 - Jan 10, 1989  EMS Dynamic allocation support  P.A.L
;	      AN003 - Apr 15, 1989  Fastopen Rename directory fix

;
;	Revision History
;	================
;
; Microsoft changes for DOS 5.0
;
;	      RMFS - Dec 15, 1989	Remove FAST_SEEK code - jh
;		     5/11/90		Used proper EMS installed check - MD
;
;	M000	SR	08/24/90	Remove code to reserve 64K above
;				itself for running other programs.
;				Not needed any more
;
;	M001	SR	08/31/90	Initialized sp to a proper value
;				instead of 0.
;
;       M002    MD      09/17/90        Properly initialize name cache size
;                                       if only extent size given
;
;	M003	MD	09/19/90	Make default name cache size consistent
;
;	M007	SR	1/22/91	Bug #5263. In Adjust_segids, also
;				update segment for FOPEN_Rename.
;

;ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
; The entire Fastopen component is divided into 4 modules.  They are:
; Fastopen initialization routine-1, Fastopen initialization routine-2,
; Fastopen which manages the directory/file cache buffers,
; and the cache buffer which holds directory information.
;
; These modules resides in different segments for the reason that they can
; be overlayed conditionally, depending on the user request.  For example
; initially all segments are loaded into the memory.
; Segmentation is also usefull when Fastopen
; need to copy into Expanded memory.	Following figure shows
; memory map of the FastOpen.
;
;		   Modules	      Segment
;
;	      ฺ-------------------ฟ
;	      ณ      MAIN	  ณ   CSEG_MAIN
;	      ร-------------------ด
;	      ณ   FASTINIT1	  ณ   CSEG_MAIN
;	      ร-------------------ด
;	      ณ 		  ณ
;	      ณ    FASTOPEN	  ณ   CSEG_OPEN
;	      ณ 		  ณ
;	      ร-------------------ด
;	      ณ   FASTINIT2	  ณ   CSEG_INIT
;	      ร-------------------ด
;	      ณ 		  ณ
;	      ณ   NAME		  ณ
;	      ณ   CACHE BUFFERS   ณ   CSEG_INIT
;	      ณ 		  ณ
;	      ภ-------------------ู
;
; MAIN:       This module provides DOS entry point into FASTOPEN. It also
;	      dispatch various Fastopen and Fastseek functions.  This module is
;	      in the file FASTOPEN.asm
;
; FASTINIT-1: This module is called INIT_TREE which is also a part of the
;	      Cseg_Main segment.  This basically initializes both
;	      Name and Extent drive headers, and sets up name and extent
;	      cache buffers.  This module can be found in the file
;	      FASTINIT.asm
;
; FASTINIT-1: This module is called INIT which is part of the  Cseg_Init
;	      segment.	This module parses the user commad, check memory
;	      requirements, overlay Fastopen and Fastseek code and finally
;	      installs the Fastopen to be stay resident.  This module is
;	      eventually overlayed by the cache buffers created during the
;	      buffer initialization by FASTINIT-1 ( See INIT_TREE)
;	      This module can be found in FASTINIT.asm
;
; FASTOPEN:   This module is a collection of four Fastopen functions which
;	      manage the File/Directory cache buffers. These functions are
;	      in the file FASTOPEN.asm
;
;
; Fastopen Code and Cache buffer Relocation
; -----------------------------------------
;    If user specifies both  n and m in the user command  and /x, then
; Cseg_Open, Cseg_Seek and Cseg_Init will be copied into a 16K page of the
; Expanded Memory.  If only n is specified, then Cseg_Open and Cseg_Init will
; be copied.  If only m is specified, then Cseg_Seek and Cseg_init will be
; copied.  After this the total size of the segments transferred will be
; deblocked from the low memory to save available user space.
;
; WARNING: After every move you have to recalculate the Seg ID of moved
;	   modules depending on how far it has been displaced and then
;	   replace the Seg ID in the jump vectors used for accessing
;	   functions in the moved modules.  A wrong Seg ID can cause
;	   instant System CRASH ...@%+(@!$#@@*&...
;
; Future Enhancements:
;
;   1.	Modify Fastopen so that it can be run on removable media (Diskette).
;	At present only fixed disk is supported.
;
;   2.	Allocate all Extent buffers during initialization. Now they are
;	done in run time.  This may avoid using flags (-2) for discontinuous
;	buffers. Using (-2) requires buffers be filled with '0's during PURGE.
;
;
;   4;	Currently Fastopen code and cache is kept in one 16K page of the
;	Extended Memory.  This puts a restriction on the size of the cache
;	buffer available in EMS usually about 8K.  This can be avoided by
;	keeping code and cache buffers in two seperated pages, so that maximum
;	of 16K is available for cache buffers.
;ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
;
IF1
    ;%OUT ASSEMBLING: FASTINIT - FASTOPEN initialization
ENDIF
NAME	FASTINIT

.XCREF
.XLIST


TRUE	   EQU	  0FFFFh
FALSE	   EQU	  0

;DBCS	    =	  FALSE
Installed   =	  TRUE

IFNDEF	DEBUG
    DEBUG	= FALSE
ENDIF

INCLUDE    dosmac.inc
INCLUDE    vector.inc
INCLUDE    filemode.inc
INCLUDE    mult.inc
include	   version.inc

.LIST
.CREF

INCLUDE    fastsegs.inc
INCLUDE    fastopen.inc
INCLUDE    SYSCALL.INC ;

BUFFERFLAG	EQU	TRUE


;-----------------------------------------------------------------------
;		       EQUATES
;-----------------------------------------------------------------------
Top_mem 	       EQU    02h	      ;Top of memory index in PSP
Min_entry_num	       EQU    10	      ;minimum name cache entries
Max_entry_num	       EQU    999	      ;maximum name cache entries
Default_names	       EQU    48	      ;default name cache entries M003
Debug		       EQU    0 	      ;for callinstall
Len_source_xname       EQU    4 	      ;used for xname translate
No_siblings	       EQU    -1	      ;indicate no siblings
No_child	       EQU    -1	      ;indicate no children
No_backward	       EQU    -1	      ;no backward pt yet
Max_drives	       EQU    24	      ;maximum number of drives allowed

STACKSIZE		EQU    200h
IF	NOT BUFFERFLAG
Static_Alloc	       EQU    1
Dyna_Alloc	       EQU    2
ENDIF

; ----------------- MESSAGE EQUATES -------------------------------------

Not_enough_mem	       EQU	2
Invalid_switch	       EQU	3
Install1	       EQU	4
Already_install        EQU	5
Incorrect_param        EQU	6
Too_many_entries       EQU	7
Dup_drive	       EQU	8


Invalid_name	       EQU	12
Ems_failed	       EQU	13
Ems_not_install        EQU	14
Invalid_drive	       EQU	15
No_page_space	       EQU	16
Bad_Use_Message        EQU	17


Many_Name_Entries      EQU	19

Under_DosShell	       EQU	20

MSG_OPTIONS_FIRST      EQU	300		; Options help messages
MSG_OPTIONS_LAST       EQU	305

;------------ E M S SUPPORT EQUATES -------------------------------

EMS_GET_STATUS	       EQU	40H
EMS_GET_NUM_PAGES      EQU	42H
EMS_ALLOC_PAGES        EQU	43H
EMS_MAP_HANDLE	       EQU	44H
EMS_GET_VERSION        EQU	46H
EMS_SAVE_STATE	       EQU	4FH
EMS_RESTORE_STATE      EQU	48H
EMS_PAGE_SIZE	       EQU	4FH
EMS_2F_HANDLER	       EQU	1BH
EMS_GET_COUNT	       EQU	5801H
EMS_GET_FRAME_ADDR     EQU	5800H
EMS_HANDLE_NAME        EQU	53H
EMS_INT 	       EQU	67H
SINGLE_SEGMENT	       EQU	 1


;-------------------- STRUCTURES ---------------------------------

PAGE_FRAME_STRUC    STRUC	    ; EMS page frame structure

  PAGE_SEG	DW	?	    ;EMS page segment
  PAGE_NUM	DW	?	    ;EMS page number (only one page is used)

PAGE_FRAME_STRUC    ENDS

BUFFER_ENTRY_SIZE      EQU    TYPE  PAGE_FRAME_STRUC


SUB_LIST      STRUC			; Message handler sublist structure
	DB	11			;
	DB	0			;
DATA_OFF DW	0			; offset of data to be inserted
DATA_SEG DW	0			; offset of data to be inserted
MSG_ID	DB	0			; n of %n
FLAGS	DB	0			; Flags
MAX_WIDTH DB	0			; Maximum field width
MIN_WIDTH DB	0			; Minimum field width
PAD_CHAR DB	0			; character for pad field
SUB_LIST      ENDS


;-------------------------------------------------------------------------------
; Following two segments are used to define external variable that
; are defined in two other segments.
;-------------------------------------------------------------------------------

CSEG_OPEN   SEGMENT   PARA   PUBLIC 'CODE'       ; Cseg_Open segment
  EXTRN   Open_name_cache_seg:word
  EXTRN   Open_Name_Drive_Buff:word
  EXTRN   End_Open:byte
  EXTRN   Chk_Flag:word
  EXTRN   VECTOR_LOOKUP:dword	  ; jump vector inside Cseg_Main to make
				; a FAR call to Fopen LookUp function within
				; the segment
ifdef DBCS
  EXTRN   DBCSLeadByteTable:dword
endif

CSEG_OPEN	ENDS



;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
CSEG_MAIN   SEGMENT   PARA   PUBLIC 'CODE'       ;  MAIN segment

; This segment is a continuation of the Cseg_Main segment in Fastopen.asm
; and contains code to initializes name and extent drive buffers
;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
ASSUME	 CS:cseg_main,DS:nothing,SS:stack,ES:nothing

EXTRN	MAIN:FAR


EXTRN			EMS_PAGE_MAP	:WORD
EXTRN			EMS_PAGE_ARRAY	:DWORD

extrn	restore_page_state:near		; HKN 8/25/88

extrn	ems_save_handle1:word		; HKN
extrn	ems_page_number:word		; HKN


EXTRN	Main_Total_Name_Count:word
EXTRN	Main_Name_Drive_Buff:word
EXTRN	Main_Name_Cache_Buff:word
EXTRN	Main_Name_Cache_Seg:word
EXTRN	Main_Parambuff:byte


EXTRN	Main_Num_Of_drives:word


	if bufferflag
EXTRN	Main_EMS_FLAG:word
	else
EXTRN	MAIN_EMS_MEM:word		;AN002; 0= No EMS  2=/X, 1=/XS, 2=/XD
					;AN002; /X default dynamic  /XS - Static, /XD=dynamic
	endif

EXTRN	Main_Res_Segs:word
EXTRN	Main_EMS_PAGE_SEG:word
EXTRN	Main_EMS_PAGE_SIZE:word

EXTRN	FOPEN_Insert:dword
EXTRN	FOPEN_Update:dword
EXTRN	FOPEN_Delete:dword
EXTRN	FOPEN_Lookup:dword
EXTRN	FOPEN_Purge:dword
EXTRN	FOPEN_Rename:dword	       ;AN003



;*************************************************************************
;
;SUBROUTINE: INIT_TREE	  (FASTINIT-1)
;
;FUNCTION:  This routine builds 'N' name directory buffers under each drive
;	    header. The second half of this routine makes the Fastopen code
;	    resident.
;
;INPUT:     Drive_cache_header, End_Caches
;
;OUTPUT:    Name_cache and Extent Cache entries installed for every
;	    drive requested.
;
;*************************************************************************
	IF  ($-Cseg_Main) MOD 16
	   ORG ($-Cseg_Main)+16-(($-Cseg_Main) MOD 16)
	ENDIF
End_Main1  label   word


INIT_TREE:
	mov	ax,cseg_Main		       ;get addressiblity to
	mov	ds,ax			       ;DS --> Cseg_Main
	ASSUME	ds:cseg_Main


;-----------------------------------------------------------------------------
; Following code adds 'n' directory entry buffers to each Name Drive headers,
; depending on the value of 'n' specified with each drive ID
;-----------------------------------------------------------------------------
	mov	si,Main_Name_Drive_Buff        ;SI-->first Name drive cache buff
	mov	bx,Main_Name_Cache_Buff        ;BX-->Name cache buffer
	xor	dx,dx
	xor	ax,ax

	mov	ax,Main_Name_Cache_Seg	       ;get addresability to CSeg_Init
	mov	ds,ax			       ;DS=addressablity to Cseg_Init
	ASSUME	ds:cseg_Init

Set_Up_Cache:
	mov	[si].DCH_LRU_ROOT,bx		;set to point to first name
	mov	[si].DCH_NAME_BUFF,bx		;set to point to first name
	mov	cx,[si].DCH_num_entries 	;get number of name records

;-----------------------------------------------------------------------------
;  set up MRU and LRU pointers
;  AX points to last name record
;  BX points to current name record
;  DX points to next name record
;-----------------------------------------------------------------------------
	mov	[bx].nMRU_ptr,-1		;make first MRU -1
	jmp	short set_start

Set_Up_Names:
	mov	[bx].nMRU_ptr,ax		;set up MRU
	add	ax,size name_record

Set_Start:
	mov	[bx].nChild_ptr,no_child	;no children or siblings
	mov	[bx].nsibling_ptr,no_siblings	;  right now
	mov	[bx].nBackward_ptr,no_backward
	push	es
	push	di
	push	ax

	push	ds
	pop	es				;ES-->name cache buffer
	ASSUME	es:Cseg_Init

	mov	ax, '  '
	mov	di, bx
	add	di, nCmpct_Dir_Info		;blank out the Dir name area
	stosb					;the directory buffer
	stosw
	stosw					; Same as rep stosb * 11
	stosw
	stosw
	stosw

	pop	ax
	pop	di
	pop	es

	mov	dx,bx				;get name offset
	add	dx,size name_record		;get start of next name
	dec	cx				;decrement num_entries
	jcxz	get_next_drive			;if zero - get next drive
	mov	[bx].nLRU_ptr,dx		;LRU pointer - next name
	add	bx,size name_record		;
	jmp	set_up_names

Get_Next_Drive:
	mov	[bx].nLRU_ptr,-1		;LRU pointer - next name

	mov	[si].DCH_MRU_ROOT,bx		;set to point to last name
	mov	bx,dx				;get pointer to next name
	cmp	[si].dch_sibling_ptr,no_siblings  ;is there any more to set up??
	jz	Init_exit			; RMFS
	add	ax,size name_record		; yes - get next name directory buffer
	add	si,size drive_cache_header	;point to next drive header
	jmp	set_up_cache

;----------------------------------------------------------------------------
; Close handles 0 - 4
;----------------------------------------------------------------------------

INIT_EXIT:
	mov	bx,0
Handle_Loop:
	mov	ah,03EH
	INT	21H
	inc	bx
	cmp	bx,5
	jne	Handle_Loop


;----------------------------------------------------------------------------
; Get PSP segment and find the program environment segment and deallocate
; the environment space.
;----------------------------------------------------------------------------

	 push  ds
	 mov   si,0081H
	 mov   ah,62H
	 INT   21H		   ; get program PSP segment

	 mov   ds,bx		   ; DS = PSP segment
	 mov   si,02CH		   ; SI-->address of enviroment segment
	 mov   ax,[si]		   ; AX = environment seg id
	 cmp   ax,0		   ; environment present ??
	 je    dont_dealloc	   ; no - dont deallocate
	 mov   es,ax
	 mov   ah,49H
	 INT   21H		   ; deallocate environment
Dont_Dealloc:
	 pop   ds		   ; restore DS

;----------------------------------------------------------------------------
; Keep resident the Fastopen code and cache buffers.  The size of the resident
; area is in (Main_Res_Segs). Size may vary depending on whether Fastopen or
; Fastseek or both or extent memory is specified.
;----------------------------------------------------------------------------

	  if	 not bufferflag
	  CMP	 MAIN_EMS_Mem,Dyna_Alloc      ;AN002;Dynamic allocation ??
	  JNE	 SKIP_RESTORE1		      ;AN002; if not dont restore
	  endif

	  call	 restore_page_state	      ; HKN 8/25/88

Skip_Restore1:				      ;AN002;
	  mov	 ah,KEEP_PROCESS	      ;remain resident
	  mov	 al,0			      ;return code
	  mov	 dx,Main_Res_Segs	      ;size of area in paragraph
	  INT	 21h			      ;keep resident and then return
					      ;control to DOS

;----------------------------------------------------------------------------
; Calculate the size of the MAIN module in bytes.  First potion of this
; segment can be found in the Fastopen.asm
;----------------------------------------------------------------------------
	IF  ($-Cseg_Main) MOD 16
	   ORG ($-Cseg_Main)+16-(($-Cseg_Main) MOD 16)
	ENDIF
End_Main   label   word


CSEG_MAIN	ENDS		     ; End of Cseg_Main segment
page


;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

CSEG_INIT	SEGMENT PUBLIC PARA 'CODE'

;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	 ASSUME    cs:cseg_init,ds:cseg_init,ss:stack,es:cseg_init

	 EXTRN	  SYSPARSE:NEAR
	 EXTRN	  SYSLOADMSG:NEAR
	 EXTRN	  SYSDISPMSG:NEAR

; Changed to eliminate having to do a far call to same_ems_page_state
; from within the resident code. Added a duplicate function to the
; init code.
;
;	extrn	save_ems_page_state:far		; HKN 8/25/88




;----------------------------------------------------------------------------
; The cache buffers start from the first location of Cseg_Init.
; First portion is the NAME DRIVE HEADERS, which is followed by
; NAME CACHE BUFFER, which is followed by EXTENT DRIVE HEADER. Under each
; extent drive header its cache buffer. 24 Name drive buffers are allocated
; during assembly time.  Remaining drive and cache buffers are allocated
; during run time.  Eventhough 24 name cache buffers are allocated during
; assembly time, this number may be reduced to the specified number of drive
; numbers during run time by overlaying other drive buffers over the unused ones.
; The initialization code will be overlayed by name and extent cache buffs
; during second half of the initialization which is in the MAIN module (see INit_Tree).
;-----------------------------------------------------------------------------

Drive_header_start	label	byte	    ;Name cache drive buffer
Drive_Cache		Drive_Cache_Header    max_drives DUP (<>)  ; header for 24 drives are reserved

;-----------------------------------------------------------------------------
; Anything below this point will be overlayed by the Cache Buffers
; MSG retriever is placed after Cache buffer, so that the area can be
;-----------------------------------------------------------------------------
;=============================================================================
;		   Non_Resident Data Area
;=============================================================================
INIT_VECTOR		DD     INIT_TREE	;jump vector to INIT_TREE
MAIN_VECTOR		DD     MAIN		;entry point to MAIN routine
source_xname		DB	" :\",0         ;used for xname translate
target_xname		DB	65 DUP (0)	;used for xname translate
user_drive		db	0		;current user drive
psp_seg 		dw	0		;segment of psp
stack_seg_start 	dw	0		;segment of temporary stack
stack_seg_end		dw	0
num_of_drives		dw	0		;number of user specified drives
Ext_Mem 		dw	0		;=1 if exteded memory is enabled
						;or, if not bufferflag (ie, IBM):
						;  0=No EMS, 2=/X, 1=/XS, 2=/XD
drive_id		db     " :",0
Parambuff		db	50  dup (0)
Parmbuff_Ptr		dw	0
FRAME_COUNT		dw	0		;EMS frame count
FRAME_BUFFER		DB	100h DUP(0)	; EMS frame buffer
FST_PAGE		DW	0,0		; holds the second highest page above 640k

Cmdline_buff		db	135  dup (0)	;command line buffer
name_cache_seg		dw	Cseg_Init	;default to Init1 seg


name_cache_Buff 	dw	0		;pointer to Name cache buffer
EMS_FLAG		dw	0		;EMI flag  1= if EMI is enabled
CHECK_QUEUE		dw	0		; = 1 if analyser is activated
RES_SEGS		dw	010H	;PSP SIZE ; M001 :add stacksize later
EMS_PAGE_SEG		DW	0		;EMS code page segment ID
EMS_PAGE_NUM		DW	0		;EMS physical page number
EMS_NAME		db	"EMMXXXX0"	;Name of EMS driver

Total_Name_Count	DW	0		;Total Name entry count

;; JUNK ;; Total_Cache_Size	DW	0		;Total cache buffer size (name+extent) buffer

Name_Cache_Size 	DW	0		;Total name cache size (header + entry buffs)
Name_Count		DW	0		;name entry count
Name_Drive_Buff 	DW	0		;name driver buffer address


Open_SegID		DW	0		;SegId of Cseg_Open after relocation


Init_SegID		DW	0		;SegId of Cseg_Init  "       "
MAIN_Size		DW	0		;size of Cseg_Main in Paragraph
OPEN_Size		DW	0		;size of Cseg_Open in paragraph


;-----------------------------------------------------------------------;
;	EMS Support							;
;-----------------------------------------------------------------------;
EXT_HANDLE     DW   ?		     ; EMS handle for reference
EMS_PAGESIZE   DW   ?		     ; EMS handle for reference
EMS_FRAME_ADDR DW   ?		     ; EMS handle for reference
CURR_EMS_PAGE  DB   ?		     ; Current EMS page number
HANDLE_NAME    DB   'FASTOPEN',0     ; EMS handle name

;;; johnhe 12/31/89; SAVE_MAP_ADDR	DD	?	; HKN 8/25/88


;; md debug publics
public drive_cache, parambuff, parmbuff_ptr, current_parm, Next_parm
public ordinal, ordinal1, prev_type, par_min, par_max, par_sw, pos1type
public pos2type, valuelo

;---------------------------------------------------------------------------
;	PARSER Support
;---------------------------------------------------------------------------
CURRENT_PARM   DW   81H 	     ;POINTER INTO COMMAND OF CUREENT OPERANT
NEXT_PARM      DW   0		     ;POINTER INTO COMMAND OF NEXT OPERAND
ORDINAL        DW   0		     ;ORDINAL NUMBER OF MAIN PARSER LOOP
ORDINAL1       DW   0		     ;ORDINAL NUMBER OF COMPLEX ITEM LOOP
PREV_TYPE      DB   0		     ;PREVIOUS POSITIONAL PARAMETER TYPE

;---------------------------------------------------------------------------
; PRINT_STDOUT input parameter save area
;----------------------------------------------------------------------------
SUBST_COUNT DW	  0		   ;message substitution count
MSG_CLASS   DB	  0		   ;message class
INPUT_FLAG  DB	  0		   ;Type of INT 21 used for KBD
MSG_NUM     DW	  0		   ;message number


;----------------------------------------------------------------------------
; Following three sublists are used by the  Message Retriever
;----------------------------------------------------------------------------
SUBLIST1 LABEL	DWORD		   ;SUBSTITUTE LIST 1
	DB	11		   ;sublist size
	DB	0		   ;reserved
	DD	0		   ;substition data Offset
	DB	1		   ;n of %n
	DB	0		   ;data type
	DB	0		   ;maximum field width
	DB	0		   ;minimum field width
	DB	0		   ;characters for Pad field


SUBLIST2 LABEL	DWORD		   ;SUBSTITUTE LIST 2
	DB	11		   ;sublist size
	DB	0		   ;reserved
	DD	0		   ;substition data Offset
	DB	2		   ;n of %n
	DB	0		   ;data type
	DB	0		   ;maximum field width
	DB	0		   ;minimum field width
	DB	0		   ;characters for Pad field



;--------------------------------------------------------------------------
;   PARSER  Control Blocks and Buffers
;--------------------------------------------------------------------------

NUM_SWITCHES	equ	2	  ; number of command switches supported

PARMS	   label   word
	    DW	    parmsx
	    DB	    1		  ; number of delemeters
	    DB	    1		  ; extra delimeters length
	    DB	    "=" 	  ; extra delimeter expected
	    DB	    0		  ; extra end of line length
	    DB	    0


PARMSX	   label   byte
par_min     DB	    1		  ; min, max positional operands allowed
par_max     DB	    2		  ; min, max positional operands allowed
	    DW	    Pos1	  ; offset into positonal-1 control block
	    DW	    Pos2	  ; offset into positonal-1 control block
par_sw	    DB	    NUM_SWITCHES  ; two switches
	    DW	    Switch_X	  ; offset of Switch_X control bloc
	    DW	    Switch_?	  ; offset of Switch_? control bloc
	    DB	    0		  ; no keywords
	    DB	    0		  ; 0



;------------------ POS2 CONTROL BLOCK --------------------------------------

POS1	  label  word	  ; positional-1 control definition
Pos1Type    DW	   0100H	; control type flag (drive only)
	    DW	   0		; function flags
	    DW	   Result	; offset into result buffer
	    DW	   value_pos1	; offset value list buffer
	    DB	   0		; number of keyword/switch synonyms


Value_Pos1    label   byte	; postional parameter value expected
	    DB	    0		; no values expected



;---------------- POS1 CONTROL BLOCK ----------------------------------------

POS2	 label	word	       ; positional-2 control definition
Pos2Type   DW	  08502H       ; Control type (complex/integer/drive/
			       ; repeat)
	   DW	  0	       ; function flags
	   DW	  Result       ; offset into result buffer
	   DW	  value_pos2   ; offset value list buffer
	   DB	  0	       ; number of keyword/switch synonyms

Value_Pos2    label   byte
	   DB	   0	       ; either (n) or (m) will be returned



;--------------- RESULT BUFFER ---------------------------------------------

RESULT	 label	byte	 ; postional2 parameter result buffer
PosType    DB	  ?	       ; type of operand returned
Postag	   DB	  ?	       ; type of item tage returned
synonym    DW	  ?	       ; offset into synonyms returned
valuelo    DW	  ?	       ; space for drive number/integer/strin
valuehi    DW	  ?


;---------------- SWITCH CONTROL BLOCK S -------------------------------------

Switch_X   label  word	       ; switch control definition
	   DW	  0	       ; no match flag
	   DW	  0	       ; no function flags
	   DW	  Result       ; offset into result buffer
	   DW	  Value_Nul    ; offset value list buffer
	   DB	  1	       ; number of keyword/switch synonyms
E_Switch   DB	  "/X"	       ; /X option for extended memory access
	   DB	  0

Switch_?   label  word	       ; switch control definition
	   DW	  0	       ; no match flag
	   DW	  0	       ; no function flags
	   DW	  Result       ; offset into result buffer
	   DW	  Value_Nul    ; offset value list buffer
	   DB	  1	       ; number of keyword/switch synonyms
Sw_?_Syn   DB	  "/?"	       ; /? option for display options
	   DB	  0


Value_Nul  label   byte        ; switch parameter value expected
	   DB	   0	       ; no values expected







;-----------------------------------------------------------------------------
;  INIT     (FASTINIT-2)
;-----------------------------------------------------------------------------
;
;SUBROUTINE: INIT
;
;FUNCTION:  Performs FASTOPEN initialization function
;
;
;NOTE: This routine is the starting routine of FASTOPEN
;
;-----------------------------------------------------------------------------

START:
					; on entry DS and ES -->PSP
	mov	AX,CS			; DS-->Cseg_Init
	mov	DS,AX
	ASSUME	ds:cseg_init
	mov	psp_seg,es		; save PSP segment for later use
	mov	ES,AX			; ES-->Cseg_Init
	ASSUME	es:cseg_init

	CALL	SYSLOADMSG		; Preload messages
	jnc	Parse_cmd_line		; If no error, parse command line

	mov	ax,1
	CALL	SYSDISPMSG		; display error

	mov	ah,04ch 		; Terminate
	mov	al,0			; Errorlevel 0 (Compatible)
	INT	021h			; exit to DOS

Parse_Cmd_Line:
	CALL	PARSE			;Parse command line
	lea	si,parambuff		;drive ID buff address

;  RMFS ;;;;;	mov	ax,Total_name_Count	;
;  RMFS ;;;;;	mov	ax,num_of_drives
;  RMFS ;;;;;	mov	ax,ext_mem

	jc	error_exit		;yes - exit

Check_Installed:
	CALL	CHECK_INSTALL		; Fastopen installed ??
	jc	error_exit

;-----------------------------------------------------------------------------
; Set seg IDs of three segments.
;-----------------------------------------------------------------------------
Save_SegIds:
	mov	Open_SegID, Cseg_Open
	mov	Init_SegID, Cseg_Init

;-----------------------------------------------------------------------------
; Compute the size of segments and cache buffers.  Setup a temporary stack
; to be used by the second half of initilization.
;-----------------------------------------------------------------------------
	CALL	CHECK_MEM		;See if we have enough memory
	jc	error_exit		;no  - display not enough mem msg			   ;RMFS;

;-----------------------------------------------------------------------------
; Check if Extended Memeory is specified. If true, check if Extended memory is
; available.  Get segid of one extended memory page.
;-----------------------------------------------------------------------------
Chk_Extended_Mem:
	cmp	Ext_Mem,0		; enable EMS ??
	je	Set_Data_Areas		; no, set data areas

	CALL	SET_EMS 		; set expanded memory
	jc	error_exit

;------------------------------------------------------------------------------
; Copy Data and segid of Init segments to Main, Open and Seek segments.
; If code is relocated, segids have to be adjusted later. (See Adjust_SegID)
;------------------------------------------------------------------------------
Set_Data_Areas:
	CALL	COPY_DATA		; copy data to other segments


ifdef DBCS
	call	SET_DBCS_TABLE		; set DBCS lead byte table address
endif

;-----------------------------------------------------------------------------
; Relocate code to extended memory if extended memory is specified or
; relocate in lower memory itself.
;-----------------------------------------------------------------------------
Relocate_Code:
	CALL	RELOCATE_SEGMENT	; Relocate the code cnd buffers

;-----------------------------------------------------------------------------
; Adjust the segids and jump vectors in other segments after code relocation
;-----------------------------------------------------------------------------
	CALL	ADJUST_SEGIDS		; adjust segment ids after relocation

;-----------------------------------------------------------------------------
; Display FASTOPEN INSTALLED message. This must be done prior to the actual
; installation.
;-----------------------------------------------------------------------------
Disp_Install_Msg:			; display FASTOPEN installed message
	MOV	AX,INSTALL1		; message number
	MOV	MSG_NUM,AX		; set message number
	MOV	SUBST_COUNT,0		; no message
	MOV	MSG_CLASS,-1		; message class
	MOV	INPUT_FLAG,0		; no input
	CALL	PRINT_STDOUT		; show message

;-----------------------------------------------------------------------------
; Install Fastopen
;-----------------------------------------------------------------------------
	CALL	INSTALL_FASTOPEN	; Install Fastopen
	jc	error_exit

;----------------------------------------------------------------------------
; Set Stack Values. This stack is used by the cache buffer initilization
; portion of the code. This stack area will be eventually overlayed and
; wont be used by either Fastopen or Fastseek functions in MAIN module.
;----------------------------------------------------------------------------
SETUP_STACK:
;  RMFS ;;;;;	nop
	CLI				;no interrupts allowed during stach change
	mov	SS,Stack_Seg_Start	;set up new stack
	mov	SP,STACKSIZE			;
	STI				;interrupts ok now
	jmp	INIT_VECTOR		;Jump to Cseg_Main to do second
					;phase of the initialization
ERROR_EXIT:
	mov	al,1			;set up return code
	mov	ah,exit 		;set function code
	INT	INT_COMMAND		;exit to DOS



;----------------------------------------------------------------------------
;  CHECK_INSTALL
;----------------------------------------------------------------------------
; Input:  None
;
; Output:
;      IF Carry = 0   -  Fastopen is not already installed
;
;      IF Carry = 1   -  Fastopen is already installed
;
;----------------------------------------------------------------------------
;  Use CALLINSTALL macro to see if FASTOPEN is already installed.
;  If carry flag set then FASTOPEN is installed. In this case display
;  Already Installed message.
; Also check for the presence of DOSSHELL switcher being enabled. FASTOPEN 
; should not be installed if the switcher is present.
;----------------------------------------------------------------------------

CHECK_INSTALL	PROC	 NEAR

	push	ax				;save every registers that may
	push	bx				;be destroyed by DOS
	push	cx
	push	dx
	push	si
	push	di
	push	bp

	push	ds
	mov	bx, 1				;Fastopen function code
	mov	si, -1				;special check install code
	CALLINSTALL fastopencom,multdos,42	;see if fastopen installed
	pop	ds

	jnc	Chk_DosShell			; AN_RMFS



Install_Msg:					;installed previously display message
	MOV	AX,ALREADY_INSTALL		;message number
Chk_disp_Msg:	
	MOV	MSG_NUM,AX			;set message number
	MOV	SUBST_COUNT,0			;no message substitution
	MOV	MSG_CLASS,-1			;message class
	MOV	INPUT_FLAG,0			;no input
	CALL	PRINT_STDOUT			;show message "Already Installed"
	stc

Chk_Install_Exit:
	pop	bp				;restore registers
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret					;return

; M0006 BEGIN
; Check for the presence of DOSSHELL Switcher; if present display a message and
; quit
Chk_DosShell:
	mov	ax,4b02h			; see if switcher enabled
	xor	bx,bx
	push	es
	mov	es,bx
	mov 	di,bx				; es:di <- 0 on entry
	int	2fh				
	mov	ax,es
	pop	es
	cmc
	jnc	Chk_Install_Exit		; assume no Switcher
	or	ax,di				; es:di == 0 =>no switcher
	jz	Chk_Install_Exit
	mov	ax,UNDER_DOSSHELL		; display appropriate err msg
	jmp	short Chk_disp_Msg

; M0006 END

CHECK_INSTALL	  ENDP





;----------------------------------------------------------------------------
;  INSTALL_FASTOPEN
;----------------------------------------------------------------------------
; Input:     Addrss of entry point to Fastopen resident code
;
; Output:
;      IF Carry = 0
;	     Entry point to FASTOPEN resident code set
;
;      IF Carry = 1  Error
;
; Calls:     none
;----------------------------------------------------------------------------
;  Use CALLINSTALL macro to see if FASTOPEN is already installed.
;  If FASTOPEN is not installed, install it.
;  If carry flag set then FASTOPEN is installed. In this case display
;  already installed message.
;----------------------------------------------------------------------------

INSTALL_FASTOPEN    PROC    NEAR

	push	ax				;Save every registers,point reg since
	push	bx				;DOS may destroy it.
	push	cx
	push	dx
	push	si
	push	di
	push	bp


	push	ds				;yes - install fastopen
	mov	bx, 1				;tell DOS that this is the
	lds	si,Main_Vector
	CALLINSTALL fastopencom,multdos,42	;see if fastopen installed
	pop	ds
	jnc	Install_Exit			;No error detected


Installx_Msg:					;installed previously display message
	MOV	AX,ALREADY_INSTALL		;message number
	MOV	MSG_NUM,AX			;set message number
	MOV	SUBST_COUNT,0			;no message substitution
	MOV	MSG_CLASS,-1			;message class
	MOV	INPUT_FLAG,0			;no input
	CALL	PRINT_STDOUT			;show message "Already Installed"
	stc

Install_Exit:
	pop	bp		    ;restore registers
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret			    ;return

INSTALL_FASTOPEN    ENDP






;----------------------------------------------------------------------------
;  CHECK_MEM
;----------------------------------------------------------------------------
;  Function:  Compute the total size of memory required by the Fasteopen.
;	      This includes both code and the cache buffers.
;
;  Input:     Name_Count, extent_count, Drive_cache,  num_of_drives
;
;  Output:    Memory is validated, Resident segment size is calculated
;	      Temporary stack segment is set
;----------------------------------------------------------------------------
CHECK_MEM	PROC	NEAR		     ; DS-->Cseg_init

;** Compute the total resident segment size and then add the cache buffer
;** size.  The Resident segment size should be adjusted again after relocation.

;; JUNK ;;	mov	Total_Cache_Size,0	     ;reset total cache size (Name +Ext)
	mov	Name_Cache_Size,0	     ;reset Name cache buffer size
	mov	ax, offset End_Main	     ;size of Main_Seg in bytes
	add	ax,15
	mov	cl,4			     ;convert size to paragraph
	shr	ax,cl			     ;by dividng by 16
	mov	MAIN_Size, ax		     ;save MAIN segment size in para
	add	Res_Segs,ax		     ;update resident seg count

	mov	ax, offset End_Open	     ;size of Open_Seg in bytes
	add	ax,15
	mov	cl,4			     ;convert it to paragraph
	shr	ax,cl
	mov	OPEN_Size, ax		     ;save OPEN segment size in para
	add	RES_SEGS,ax		     ;update resident seg count


;----------------------------------------------------------------------------
; Calculate the size of the NAME DRIVE HEADER BUFFERS
;----------------------------------------------------------------------------
	xor	ax,ax			    ;reset the cache size register


	mov	bx,offset DRIVE_CACHE	    ;get beginning of cache buff
	xor	ax,ax
	mov	al,size drive_cache_header  ;get size of one name entry
	mul	Num_Of_drives		    ;get total needed for drive cache
	add	ax,bx			    ;set up correct offset
	add	ax,15			    ;round up to paragraph boundary
	mov	cl,4
	shr	ax,cl			    ;convert to paragraphs
	add	RES_SEGS,ax		    ;update resident seg count
;; JUNK ;;	mov	Total_Cache_Size, ax	    ;update total cache buff size
	mov	Name_Cache_Size, ax	    ;size in paragraph

; Calculate the offset of the Name cache buffers
	shl	ax,cl			    ;AX = offset to Name cache buff
	mov	NAME_CACHE_BUFF,ax	    ;save Name cache address

;-----------------------------------------------------------------------------
; Compute the size of the NAME CACHE  buffer
;-----------------------------------------------------------------------------
	mov	ax,size Name_Record
	mul	Total_Name_Count
	add	ax,15			    ;round up to paragraph boundary
	mov	cl,4
	shr	ax,cl			    ;convert to paragraphs ( divide 16)
	add	RES_SEGS,ax		    ;AX = End of Name cache buffers
;; JUNK ;;	add	Total_Cache_Size,ax	    ;update total cache buff size
	add	Name_Cache_Size, ax	    ;



;----------------------------------------------------------------------------
; Setup stack segment followed by the extent cache buffers.  This is a
; temporary stack used by the  drive buffer initilization code in the
; Cseg_Main segment.  This stack will be overlayed by the cache buffers.
;----------------------------------------------------------------------------
Set_Stack:
	mov	ax,RES_SEGS		    ;AX=size of code and buffs in para
	add	ax,PSP_Seg		    ;AX=segID of stack
	mov	Stack_Seg_Start,ax	    ;start of the new STACK
	add	ax,STACKSIZE / 16			    ;add the size of the stack
	mov	Stack_Seg_End,ax	    ;get end of what we need
	add	Res_Segs,STACKSIZE / 16		;add stacksize paras ;M001

	push	ds			    ;
	mov	ds,PSP_Seg		    ;access PSP for memory size
	mov	si,Top_mem
	LODSW				    ;get total memory size
	pop	ds
	sub	ax,Stack_Seg_End		    ;see if there is enough for us
	jnc	Check_Reloc_Size		    ;yes, continue ;M000

;M000;	sub	ax,1000h		    ;will there still be 64K ??
;M000;	jnc	Check_Reloc_Size	    ;and return

Not_Enough_Memory:
	MOV	AX,NOT_ENOUGH_MEM	    ;message number
	MOV	MSG_NUM,AX		    ;set message number
	MOV	SUBST_COUNT,0		    ;no message substitution
	MOV	MSG_CLASS,-1		    ;message class
	MOV	INPUT_FLAG,0		    ;no input
	CALL	PRINT_STDOUT		    ;show message "Insufficient Memory"
	stc				    ;set error flag
	jmp	short Set_Mem_Ret	    ;return

;------------------------------------------------------------------------------
; If relocation is needed, then recalculate the size of resident segment
; If extended memory relocation, OPEN, SEEK and INIT segments will be
; eliminated from the current resident seg.
;-----------------------------------------------------------------------------
Check_Reloc_Size:
	cmp	Ext_Mem,0		; extended memory relocation ??
	je	Set_Mem_Exit		; no - exit

;-----------------------------------------------------------------------------
; Check to see that the both code and the cache buffers fit in the
; exteneded memory one 16K page. Since the entire code segment and the
; cache buffers are going to be moved to XMA, that amount should be
; reduced from the size that should reside in the low memory.
;-----------------------------------------------------------------------------

;; RMJUNK ;;	xor	ax,ax
;; RMJUNK ;;	xor	bx,bx

	mov	ax, OPEN_SIZE		;size of Open seg in para
	add	ax, Name_Cache_Size	;size of Init_Seg in para
	cmp	ax, 0400H		;Less than 16K ??
	jge	Not_Enough_Space	;no - display message
;; JUNK ;;	mov	ax, OPEN_SIZE		;size of Open seg in para
;; JUNK ;;	add	ax, Total_Cache_Size	;reduce resident seg size
	sub	RES_SEGS,ax		;update resident seg count

;-----------------------------------------------------------------------------
; If the code is to be moved to extended memory.  There is no reason to
; keep Init_Tree in main memory.  Remove that also to save space in base memory
;-----------------------------------------------------------------------------
	mov	ax, offset End_Main1	;size of Main_Seg until Init_Tree (bytes)
	add	ax,15
	mov	cl,4			;convert size to paragraph
	shr	ax,cl			;by dividng by 16
	mov	bx,Main_Size		;bx=total size of Main seg including Init_Tree
	sub	bx,ax			;bx=size after reducing Init_Tree
	sub	RES_SEGS,bx		;update base memory resident seg count
	jmp	short Set_Mem_Exit
					;
Not_Enough_Space:
	MOV	AX,NO_PAGE_SPACE	; not enough space in EMS page
	MOV	MSG_NUM,AX		; set message number
	MOV	SUBST_COUNT,0		; no message
	MOV	MSG_CLASS,-1		; message class
	MOV	INPUT_FLAG,0		; no input
	CALL	PRINT_STDOUT		; display message
	mov	Ext_Mem, 0		; RESET XMA FLAG
	stc
	jmp	SHORT set_mem_ret

Set_Mem_Exit:
	clc

Set_Mem_Ret:
	ret

CHECK_MEM	endp





;----------------------------------------------------------------------------
;  RELOCATE
;----------------------------------------------------------------------------
;  Function:  Relocate Fastopen code and buffer in base memory or in
;	      Extended Memory.	If base memory relocation, then
;	      relocate Cseg_Seek over Cseg_Open segment if the user
;	      didn't specify Fastopen (n).  Relocate Cseg_Init over Cseg_Seek
;	      if user didn't specify Fastseek feature(m). If extended memory
;	      relocation, copy Cseg_Open, Cseg_Seek and Cseg_Init to
;	      a single page in extented memory if both Fastopen and Fastseek
;	      (n and m) are specified.	Copy Cseg_open and Cseg_Init only if Fastseek
;	      feature (m) is not specified.  Copy Cseg_Seek and Cseg_Init if
;	      FastOpen feature (n) is not specified
;
;----------------------------------------------------------------------------

RELOCATE_SEGMENT    PROC    NEAR

	cmp	Ext_Mem,0			; Extended memory enabled ??

	je	Reloc_Low_Mem			; If not relocate to low memory

;; JUNK ;;	jne	Set_Seg_Ids		; yes - do extented memory relocation
;; JUNK ;;	jmp	SHORT Reloc_Low_Mem	; no - do low memory relocation

;----------------------------------------------------------------------------
; Move Fastopen, FastSeek or both to the Extended memory
;----------------------------------------------------------------------------
Set_Seg_Ids:
	cld				; clear direction flag (increment si and di)

;-----------------------------------------------------------------------------
;	 ----  Extended Memory Relocation -----
;  Setup Cseg_Open segment in Extended Memory
;------------------------------------------------------------------------------
Set_Open_Seg:
	mov	ax,Cseg_Init		;
	mov	ds,ax			; DS-->Cseg_Init
	ASSUME	ds:Cseg_Init
	mov	ax,EMS_Page_Seg 	; AX = seg id of Cseg_Open in ext mem
	mov	Open_SegID,ax		; save it

Copy_Open_Seg:
	mov	ax, offset End_Open	; size of Open seg in bytes
	mov	cl,1
	shr	ax,cl			; convert to words
	mov	cx,ax			; CX = number of WORDS to transfer
	xor	si,si			; offset of the source in low memory
	xor	di,di			; offset of the destination in XMA
	mov	ax,Cseg_Open		; set source segID
	mov	ds,ax			; DS-->Cseg_Open
	ASSUME	ds:Cseg_Open
	mov	ax,Open_SegID		; set destination XMA  seg id
	mov	es,ax			; ES-->Extended memory page
	ASSUME	es:nothing
	REP	MOVSW			; copy Open segment to extended memory
					; SI-->Cseg_Seek segment
	mov	ax,Cseg_Init		; no - only Fastseek specified
	mov	ds,ax			; DS-->Cseg_Init
	ASSUME	ds:Cseg_Init

;-----------------------------------------------------------------------------
;  Setup Cseg_Init segment in Extended Memory
;------------------------------------------------------------------------------
Set_Init_seg:

	sub	ax,Cseg_Open		; AX = size of Open_Cseg+Seek_Cseg
	add	ax,EMS_Page_Seg 	; new Cseg_Init id in XMA if both
	mov	Init_SegID,ax		; Fastopen and Fastseek are enabled

Copy_Init_Seg:				; comes here if no Cseg_Seek is required
	xor	si,si			; offset of the source in low memory
	xor	di,di			; offset of the destination in XMA
	mov	ax, Name_Cache_Size	; size of Init seg area to be copied
	mov	cl,4			; in paragraph
	shl	ax,cl			; convert to number of bytes
	mov	cl,1			;
	shr	ax,cl			; convert to number ofwords
	mov	cx,ax			; CX = number of WORDS to transfer
	mov	ax,Cseg_Init		; set source segID
	mov	ds,ax
	ASSUME	ds:Cseg_Init
	mov	ax,Init_SegID		; set destination XMA  seg id
	mov	es,ax
	ASSUME	es:nothing
	REP	MOVSW			; copy Init segment to extended memory
	jmp	SHORT reloc_exit	; then return


;NOTE:	No need to adjust the resident segment size (Res_Segs) since it is
;	done in the routine (Check_Mem).


;-----------------------------------------------------------------------
;	 ---- LOW MEMORY RELOCATION ----
; Reloctae FastOpen in the low memory and adjust the
; resident size of the code.
;-----------------------------------------------------------------------
Reloc_LOW_Mem:


Reloc_Exit:
; copy the latest RES_SEGS size to Cseg_Main
	mov	ax,Cseg_Init		;
	mov	ds,ax			; DS-->Cseg_Init
	ASSUME	ds:Cseg_Init
	mov	ax,Cseg_Main		; set destination seg id
	mov	es,ax			; ES--> Cseg_Main
	ASSUME	es:Cseg_Main
	mov	ax,Res_Segs
	mov	es:Main_Res_Segs,ax	; save it

	RET

RELOCATE_SEGMENT      ENDP






;-----------------------------------------------------------------------
; Procedure:   COPY_DATA
;-----------------------------------------------------------------------
; Copy data values from Cseg_Init to other segments.  I the code is relocated,
; seg IDs should be updated after relocation.  This is done in "Update_SegID"
;
; Input:     Variables inside Cseg_Open, CsegSeek and Cseg_Main segments
;
; Output:    Data values copied to the above segments
;
;
;-----------------------------------------------------------------------
	public	copy_data
COPY_DATA	PROC	NEAR

	mov	ax,cseg_init
	mov	ds,ax			      ;DS--> Cseg_Init
	ASSUME	ds:Cseg_init
	mov	ax,cseg_Main
	mov	es,ax			      ;ES--> CSEG_MAIN
	ASSUME	es:Cseg_Main

	mov	es:Main_Name_Cache_Seg, Cseg_Init
	mov	ax,Num_Of_Drives
	mov	es:Main_Num_Of_Drives,ax


	mov	ax,Name_Cache_Buff
	mov	es:Main_Name_Cache_Buff,ax
	mov	ax,Name_Drive_Buff
	mov	es:Main_Name_Drive_Buff,ax
	mov	ax,Ems_Flag
	mov	es:Main_EMS_FLAG,ax
	mov	ax,EMS_PAGE_Seg
	mov	es:Main_EMS_PAGE_Seg,ax

IF	BUFFERFLAG
	mov	ax, EMS_PAGE_NUM
	mov	es:ems_page_number, ax	       ;HKN
ELSE
	CMP	EMS_Mem,Static_Alloc	       ; Dynamic allocation ??
	JE	Skip_Save_Num		       ; no - dont save page num
	mov	ax, EMS_PAGE_NUM	       ; save page num for
	mov	es:ems_page_number, ax	       ; Dyna mode page map
Skip_Save_Num:				       ;
ENDIF

	mov	ax,EMS_PAGE_SIZE
	mov	es:Main_EMS_PAGE_SIZE,ax


	mov	ax,Total_Name_Count
	mov	es:Main_Total_Name_Count,ax

; Copy drive buffer to MAIN segment
	 lea   si,ParamBuff
	 lea   di,es:Main_ParamBuff
	 mov   cx,50

Paramloop:
	 mov   al,[si]
	 mov   es:[di],al
	 inc   si
	 inc   di
	 LOOP  paramloop

;-----------------------------------------------------------------------
; Copy data values to OPEN segment (Cseg_Open)
;-----------------------------------------------------------------------
	mov	ax,cseg_Open
	mov	es,ax			      ;ES--> CSEG_Open
	ASSUME	es:Cseg_Open
	mov	si,offset drive_cache
	mov	es:Open_Name_Drive_Buff,si
	mov	es:Open_Name_Cache_Seg,Cseg_Init
	mov	ax,check_Queue
	mov	es:chk_Flag,ax


	mov	ax,cseg_Init
	mov	es,ax			      ;ES addressability to CSEG_Init
	ASSUME	es:Cseg_Init

	ret

COPY_DATA	ENDP




;-----------------------------------------------------------------------
; Procedure:   ADJUST_SEGIDS
;-----------------------------------------------------------------------
; Function:  Adjust segment Ids of various segments after relocation
;
; Input:   SegID Vectors
;
; Output:  SegIDs vectors are adjusted
;
; Note: The following segid and vectors are set previously either during
;	link time or during initialization time.  These SegIDS needs to
;	be changed after the code and buffers are relocated.
;-----------------------------------------------------------------------

ADJUST_SEGIDS	PROC	NEAR

	mov	ax,Cseg_Init
	mov	ds,ax			      ;DS addressability to Cseg_Init
	ASSUME	ds:Cseg_init
	mov	ax,cseg_Main
	mov	es,ax			      ;ES addressability to CSEG_MAIN
	ASSUME	es:Cseg_Main

	mov	bx, Init_segID		      ; copy seg ID of Init_Seg to
	mov	es:Main_Name_Cache_Seg, bx    ; Main seg


	mov	ax,Open_SegID
	mov	es,ax			      ; ES addressability to CSEG_Open
	ASSUME	es:Cseg_Open		      ; copy segid of init_seg to
	mov	es:Open_Name_Cache_Seg, bx    ; Open segment



; Adjust seg ids of jump vectors to Fastopen and Fastseek functions
Adjust_Vectors:
	mov	ax,cseg_Main
	mov	es,ax			      ;ES addressability to CSEG_MAIN
	ASSUME	es:Cseg_Main
					      ;DS addressability to Cseg_Init
	mov	ax, Open_SegID
	mov	word ptr es:FOPEN_Insert + word, ax
	mov	word ptr es:FOPEN_Update + word, ax
	mov	word ptr es:FOPEN_Delete + word, ax
	mov	word ptr es:FOPEN_Lookup + word, ax
	mov	word ptr es:FOPEN_Purge + word, ax						   ;TEL 9/29
;M007
; Update the segment of the Rename routine also
;
	mov	word ptr es:FOPEN_Rename + word, ax ;M007


; Change the segID of single Jump Vector inside Cseg_Main
	mov	ax,cseg_Main
	mov	es,ax			      ;ES addressability to CSEG_MAIN
	ASSUME	es:Cseg_Main
	mov	ax,Open_SegID
	mov	word ptr es:Vector_LookUp + word, ax						  ;AN000;



Adjust_Exit:
	ret
					      ;return
ADJUST_SEGIDS	ENDP








;******************************************************************************
; *
; *	 MODULE: PARSE
; *
; *	 FUNCTION: Parse  command line
; *
; *	 INPUT: FASTOPEN  d: {=n | (n,m) } ... /x  เ
; *		   where เ activates queue analyser for debugging
; *
; *	 OUTPUT:   Command line is parsed
; *		   For IBM:
; *		   [Ext_MEM] = 0  if no EMS enabled
; *			     = 1  if EMS enabled with /XS static allocation
; *			     = 2  if EMS enabled with /X  default dynamic allocation
; *			     = 2  if EMS enabled with /XD dynamic allocation
; *
; *	 RETURN SEQUENCE:
; *
; *		   If CY = 0	No error
; *
; *		   If CY = 1	Error
; *
; *	 EXTERNAL REFERENCES:	SYSPARSE
; *
; *************************************************************************

EOL	       EQU   -1 	   ; Indicator for End-Of-Line
NOERROR        EQU    0 	   ; Return Indicator for No Errors

	 public   PARSE
PARSE	 PROC	  NEAR

	 mov   num_of_drives,0	   ; initialize drive count
	 mov   name_count,0


	 mov   Total_name_count,0


	 mov   Prev_Type,0

	 mov	Ext_Mem,0

	 mov   Check_Queue,0
	 lea   si,parambuff	    ; drive ID buff address
	 mov   parmbuff_Ptr,si	    ; save it

;----------------------------------------------------------------------------
; Get command string address from PSP
;----------------------------------------------------------------------------
	 mov   si,0081H
	 mov   ah,62H
	 INT   21H		   ; get program PSP segment
	 mov   PSP_Seg,bx	   ; save PSP segment

	 mov   ds,bx		   ; DS = PSP segment
	 mov   si,0081h 	   ; SI-->beginning of parameter string in PSP
	 lea   di,cmdline_buff	   ; DI-->command param buffer
	 mov   cx,127		   ; copy 127 bytes from PSP
	 rep movsb

	 push  cs
	 pop   ds

;----------------------------------------------------------------------------
; set parametrs for SysParse call
;----------------------------------------------------------------------------
	 xor   cx,cx		   ; no params processed so far
	 MOV  ORDINAL,CX	   ; SAVE initial ordinal value
	 lea   si,cmdline_buff	   ; ES:SI-->command line
	 lea   di,parms 	   ; ES:DI-->parameter
	 MOV  CURRENT_PARM,SI	   ; pointer to next positional

	 mov   ax,0100h 	   ; Drive only
	 mov   pos1type,ax	   ; set positional control block 1
	 mov   ax,08502h	   ; Numeric/Complex/Drive/Repeat
	 mov   pos2type,ax	   ; set positional control block 2
	 mov   al,1		   ; minimum 1 positional
	 mov   Par_Min,al	   ;
	 mov   al,2
	 mov   Par_Max,al	   ; maximum 1 positional
	 jmp   short set_param

;----------------------------------------------------------------------------
;   MAIN PARSE LOOP
;----------------------------------------------------------------------------
PARSE_LOOP:			   ; MAIN PARSE LOOP
	 mov   ax,08502h	   ; number/drive ID/comlex/repeat
	 mov   pos1type,ax	   ; set positional control block
	 mov   ax,08502h	   ;
	 mov   pos2type,ax	   ;
	 mov   al,1		   ; minimum 1 positional
	 mov   Par_Min,al	   ; set min
	 mov   al,2		   ; maximum 2 positionals
	 mov   Par_Max,al	   ; set max
	 mov   par_sw,NUM_SWITCHES ; set number of switches to check

Set_Param:
	 xor   dx,dx
	 push  cs
	 pop   es		   ; ES=DS=CS
	 LEA  DI,PARMS		   ; ES:DI = PARSE CONTROL DEFINITON
	 MOV  SI,CURRENT_PARM	   ; DS:SI = next positional
	 XOR  DX,DX		   ; RESERVED, INIT TO ZERO
	 MOV  CX,ORDINAL	   ; OPERAND ORDINAL, INITIALLY

	 CALL  SYSPARSE 	   ; Parse current positional

	 mov   Next_Parm,si	   ; save pointer to next positional
	 mov   ORDINAL,CX	   ; save current ordinal
	 cmp   ax,EOL		   ; END-OF-COMMAND string ??
	 jne   Parse_chk_Error	   ; no -  check error


;----------------------------------------------------------------------------
; If previous positional is a drive ID without Name or Extent count then assign
; default counts .
;----------------------------------------------------------------------------
	 cmp   Prev_Type,6	   ; previous param = drive ID
	 jne   Take_Exit	   ; no - exit

	 CALL  PROC_DEFAULT	   ; yes - setup default counts for previous drive
	 jnc   Take_Exit	   ; exit
	 jmp   parse_Error	   ; error exit

Take_Exit:
	 CALL  Verify_Counts	   ; verify the Total counts
	 jnc   Counts_OK	   ; exit if count ok
	 jmp   parse_error	   ; else error exit

Counts_Ok:
	 jmp   parse_exit	   ; normal - exit


;----------------------------------------------------------------------------
;	CHECK ERROR CONDITIONS
;----------------------------------------------------------------------------

	PUBLIC	Parse_Chk_Error
Parse_Chk_Error:		   ; check for error conditions
	 cmp   ax,NOERROR	   ; any parse error ??
	 jne   verify_missing_oper ; yes - check missing operand
	 jmp   Chk_Result	   ; no - check result buffer

Verify_Missing_Oper:
	 cmp   ax,2		   ; yes - missing operand error??
	 jne   disp_error	   ; no - jump

	 cmp   Prev_Type,0	   ; yes - any previous parameters ??
	 jne   Chk_Prev_Drive	   ; yes, previous drive id
	 mov	 MSG_CLASS,2
	 MOV	 MSG_NUM,AX	   ; set message number
	 MOV	 SUBST_COUNT,0	   ; no message substitution
	 MOV	 INPUT_FLAG,0	   ; no input
	 CALL	 PRINT_STDOUT	   ; show message
	 stc			   ; set error flag
	 jmp   Parse_Exit	   ; exit

;----------------------------------------------------------------------------
; If previous positional is drive ID without counts then assign default counts
;----------------------------------------------------------------------------
Chk_prev_drive:
	 cmp   Prev_Type,6	   ; previous param = drive ID ??
	 jne   Take_Exit1	   ; no - exit

	 CALL  PROC_DEFAULT	   ; yes - assign default ID
	 jnc   Take_Exit1	   ; no error, verify counts
	 jmp   parse_Error	   ; error exit

Take_Exit1:
	 CALL  Verify_Counts	   ; verify the Total counts
	 jnc   Counts_right	   ; count ok - check special case
	 jmp   parse_error	   ; error - exit

Counts_right:
	 cmp   Prev_Type,0	   ; no previous param ?  (Special case)
	 je    invalid_operand	   ; no, exit ( FASTOPEN >TEMP ) case
	 clc
	 jmp   parse_exit	   ; exit

Invalid_Operand:		   ; else error
	 jmp   SHORT bad_param

Disp_Error:
	 cmp   ax, 3		   ; invalid switch type ??
	 jne   bad_param	   ; no -
	 jmp   Bad_Switch

;----------------------------------------------------------------------------
; If user entered เ to activate the analyser, than verify the previous
; drive case. If true, assign default name extent entries, set activation
; flag and take normal exit.
;----------------------------------------------------------------------------
Bad_Param:
	mov   si,Current_Parm	   ; SI-->current parameter (analyser hook)
	mov   al,0e0h		   ; เ (hidden character to activate analyser)
	cmp   [si],al		   ; activate analyser ??
	jne   set_disp_param	   ; no - normal error
	mov   Check_Queue,1	   ; yes - set flag to activate analyser
	clc
	jmp   Chk_Prev_Drive	   ; exit

Set_Disp_Param:
	mov   di,Next_Parm	  ; ending address of bad param  (1/6/88)
	mov   al,0
	mov   ds:[di],al	   ; set termination character
	LEA   SI,SUBLIST1	  ; DS:SI-->Substitution list
	MOV   AX,CURRENT_PARM	  ; starting address of bad parameter
	MOV   [SI].DATA_OFF,AX	  ; SI-->File name
	MOV   [SI].DATA_SEG,DS	  ; DS-->Segment
	MOV   [SI].MSG_ID,0	  ; message ID
	MOV   [SI].FLAGS,010H	  ; ASCIIZ string, left align
	MOV   [SI].MAX_WIDTH,0	  ; MAXIMUM FIELD WITH
	MOV   [SI].MIN_WIDTH,0	  ; MINIMUM FIELD WITH
	mov   ax,incorrect_param  ; Error Code
	MOV   MSG_NUM,AX	  ; set message number
	MOV   SUBST_COUNT,1	  ; substitution count
	MOV   MSG_CLASS,-1	  ; message class
	MOV   INPUT_FLAG,0	  ; no input
	CALL  PRINT_STDOUT	  ; display message
	stc			  ; error flag
	jmp   Parse_Exit	  ; exit		      (1/6/88 P2670)


;----------------------------------------------------------------------------
;	CHECK POSITIONAL PARAMETER TYPE
;----------------------------------------------------------------------------
Chk_Result:
	 push  es		   ; get DS back to Program data segment
	 pop   ds
	 cmp   postype,1	   ; number  ??
	 jne   chk_switch
	 jmp   short Proc_Name	   ; yes, process name entry

chk_switch:
	 cmp   postype,3	   ; switch  ??
	 je    Proc_sw		   ; yes, process switch
	 cmp   postype,6	   ; drive id ??
	 je    Proc_driveid	   ; yes, Process Drive ID
	 cmp   postype,4	   ; complex item ??
	 jne   disp_msg
	 jmp   Proc_complex	   ; yes, process Complex item

disp_msg:
	 mov   ax,incorrect_param  ; no, check reult buffer
	 jmp   bad_param	   ; else error

Proc_Sw: jmp   Proc_Switch	   ; process switch



;----------------------------------------------------------------------------
;	    PROCESS  DRIVE ID
;----------------------------------------------------------------------------
PROC_DRIVEID:			   ; PROCESS DRIVE ID
	 cmp   Prev_Type,6	   ; previous param = drive ID
	 jne   check_drive_id	   ; no, jump

; if not set default name and extent entry count for previous drive
	 CALL  PROC_DEFAULT	  ;  setup default counts
	 jnc   Check_Drive_id	  ;
	 jmp   parse_Error

Check_Drive_Id: 		   ; process current drive ID
	 mov   ax,ValueLo	   ; get drive letter number from result buff
				   ; C:=3 D:=4 etc, Parser drive id convention
	 add   al,040H		   ; convert to drive letter

	 CALL  CHECK_DRIVE	   ; validate drive ID ??
	 jnc   set_drive_id	   ; yes, jump
	 jmp   Parse_Exit	   ; no, invalid drive id , exit

Set_Drive_Id:
	 inc   num_of_drives	   ; update the drive count
	 xor   ax,ax
	 mov   ax,valuelo	   ; get drive number
	 xor   ah,ah		   ; only low byte is valid
	 mov   di,ParmBuff_Ptr	   ; DS:DI-->driveID buffer
	 dec   ax		   ; C:=2  D:=3 E:=4 etc Fastopen drive id
	 mov   [di],ax		   ; save drive in Drive ID table
	 add   parmbuff_ptr,2	   ; points to next extent count area
	 mov   al,PosTYpe	   ; set previous type before look for next
	 mov   Prev_Type,al	   ; positional parameter
	 mov   si,Next_Parm	   ; get pointer to next param (switch)
	 mov   Current_Parm,si	   ;
	 jmp   Parse_Loop	   ; look for next posistional parameter


;----------------------------------------------------------------------------
;	PROCESS INTEGER ( C:=n )  followed by drive ID
;----------------------------------------------------------------------------
PROC_NAME:
	 cmp   Prev_Type, 6	   ; previous type = drive ID
	 je    Get_Name_Value	   ; yes - jump
	 mov   ax,incorrect_param  ; error code
	 jmp   bad_param

Get_Name_Value:
	 xor   ax,ax
	 mov   ax,valuelo	   ; get name value
	 cmp   ax,10		   ; check validity of the count
	 jl    Bad_Name_Count
	 cmp   ax,999
	 jle   save_name_count	   ; count OK, save it

Bad_Name_Count: 		   ; bad name count
	 mov   ax,Invalid_Name	   ; error code
	 jmp   parse_error	   ; error - exit

Save_Name_Count:
	 mov   name_count,ax	   ; save it (name count)
	 add   Total_Name_Count,ax ; update total name count
	 mov   di,ParmBuff_Ptr	   ; DS:DI-->driveID buffer
	 mov   ax,-1
	 mov   [di],ax		   ; MARK this drive has no extent entry
	 add   parmbuff_ptr,2	   ; points to extent count area

Set_Drive_Hdr:
	 mov   ax,Name_Count	   ; get name count entry
	 CALL  SET_DRIVE_CACHE_HEADER	; Set Name cache header
	 jnc   set_min_max	   ; no error set min and max
	 jmp   parse_Error	   ; display error

Set_Min_Max:
	 mov   al,1
	 mov   Par_Min,al	   ; change min-max
	 mov   al,2
	 mov   Par_Max,al
	 mov   al,PosTYpe	   ; set previous type before look for next
	 mov   Prev_Type,al
	 mov   si,Next_Parm	   ; get pointer to next param (switch)
	 mov   Current_Parm,si	   ;
	 mov   ordinal,0
	 Jmp   Parse_Loop	   ; parse nexy positional


;----------------------------------------------------------------------------
;	 PROCESS COMPLEX (n,m)	followed by a drive id
;----------------------------------------------------------------------------
	public	proc_complex	; for debugging
PROC_COMPLEX:
	 cmp   Prev_Type, 6	  ; previous type = drive ID ??
	 je    Get_Cmplx_Item	  ; yes - ok
	 mov   ax,incorrect_param ; no - error, previous must be drive id
	 jmp   bad_param	  ; display error

Get_Cmplx_Item:
	 mov   al, PosType	  ;
	 mov   Prev_Type,al	  ; save current type as previous
	 lea   di,valuelo	  ; DI-->result buffer
	 mov   si,[di]		  ; get next positional param address
	 mov   current_parm,si	  ; SI-->first complex item
	 mov   ax,08001h	  ; Control ( Numeric/Optional )
	 mov   Pos1Type,ax	  ; change pos-param control block flag
	 mov   Pos2Type,ax
	 mov   Par_Min,al	  ; set minimum = 1
	 inc   al
	 mov   Par_Max,al	  ; set maximum = 2
	 mov   ordinal1,0	  ; initialize ordinal for complex item loop
	 mov   par_sw,0 	  ; reset switch flag in PARMSX

COMPLX_LOOP:
	 LEA  DI,PARMS		   ;ES:DI = PARSE CONTROL DEFINITON
	 MOV  SI,CURRENT_PARM	   ;SI = COMMAND STRING, NEXT PARM
	 XOR  DX,DX		   ;RESERVED, INIT TO ZERO
	 MOV  CX,ORDINAL1	   ;OPERAND ORDINAL, INITIALLY ZERO

	 CALL  SYSPARSE 	   ; parse positional param in complex item

	 cmp   ax,NOERROR	   ; parse error ??
	 je    Chk_Complex_Result  ; no, check result buffer
	 cmp   ax,EOL		   ; END-OF-COMMAND string ??
	 jne   Complex_Error	   ; no, check error
	 mov   si,Next_Parm	   ; Set pointer to next param	       (4/3/88)
	 mov   Current_Parm,si	   ; set next param address before parsing
	 jmp   Parse_Loop	   ; go to main parse loop

Complex_Error:
	 mov   ax,Incorrect_Param  ; no, check reult buffer
	 jmp   bad_param	   ; display error

;-------------------------------------------------------------------------------
;    Ckeck The Result Buffer
;-------------------------------------------------------------------------------
Chk_Complex_Result:
	 mov   ordinal1,cx	   ; save current ordinal
	 cmp   postype,1	   ; positional type = number  ??
	 je    Proc_Complex_Name   ; yes, process name entry
	 cmp   postype,3	   ; positional type = String ??
	 je    Miss_param	   ; yes, process missing parameter
	 mov   ax,incorrect_param  ; no, check reult buffer
	 jmp   bad_param

Miss_Param:
	 mov   ax,Default_Names    ; default name cache size ;M002  M003
	 mov   current_parm,si	   ; save current chara pointer  ;M002
	 jmp   short Store_Name_Count	   ; get extent count    ;M002


;-------------------------------------------------------------------------------
;   PROCESS NAME  ENTRY  (n)
;-------------------------------------------------------------------------------
Proc_Complex_Name:		   ; PROCESS COMPLEX ITEM
	 mov   current_parm,si	   ; save current chara pointer
	 cmp   cx,1		   ; ignore past first parameter
	 ja    complx_loop
	 xor   ax,ax		   ; eles process Name Count
	 mov   ax,valuelo	   ; get name value from result buffer
	 cmp   ax,10		   ; validate the name value for higher
	 jl    Name_Error	   ; and lower boundries
	 cmp   ax,Max_Entry_Num    ; name entry count ok ??
	 jg    Name_Error	   ; no - error
	 jmp   short Store_Name_Count	 ; yes - store it

Name_Error:			   ; invalid name count
	 mov   ax,invalid_name	   ; error code
	 jmp   parse_error	   ; display error

Store_Name_Count:
	 mov   Name_Count,ax	   ; save it (name count)
	 add   Total_name_count,ax ; update total name count

	 mov   di,ParmBuff_Ptr	   ; advance pointer into parameter buffer
	 mov   word ptr [di],-1    ; indicate no extent cache
	 add   ParmBuff_Ptr,2

	 CALL  SET_DRIVE_CACHE_HEADER	; Set Name cache header
	 jc    Cant_Set_Header	   ; jump if error
	 jmp   Complx_loop	   ; look for extent count

Cant_Set_Header:
	 jmp   Parse_Error	   ; error exit


;----------------------------------------------------------------------------
;	    PROCESS SWITCHES (/ OPTIONS)
;----------------------------------------------------------------------------

	PUBLIC	Proc_Switch
Proc_Switch:

; Check for other (non /X) switches first.
; The bulk of the remaining code (below
; ProcSwitchX) seems to be for just /X.

	cmp	[synonym], offset Sw_?_Syn	; /? found?
	jne	ProcSwitch?Done			;  skip this if not
	call	DISPLAY_OPTIONS			; else print msg
	stc					; set error flag
	jmp	Parse_Exit			; EXIT
ProcSwitch?Done:

; Check for the /X (Expanded Memory) switch.

ProcSwitchX:
	 cmp   Prev_Type,0	   ; any previous type ??
	 je    Switch_Error	   ; no  - error
	 cmp   Ext_Mem,0	   ; switch previously specified ??
	 je    set_sw_flag	   ; no, set flag

Switch_Error:
	 mov   ax,incorrect_param  ; error code
	 jmp   bad_param	   ; error - /x could be specified only once

Set_Sw_flag:
	 cmp   Prev_Type,6	   ; previous param = drive ID	12/15 P2939
	 jne   sw_save_Ptr	   ; no - continue		12/15 p2939

	 CALL  PROC_DEFAULT	   ; yes setup default counts for previous drive
	 jnc   sw_save_ptr	   ; no error - continue	12/15 p2939
	 jmp   short parse_Error   ; error - exit		12/15 P2939

Sw_save_ptr:
	 mov   current_parm,si	   ; save current chara pointer
	 mov   bx,synonym	   ; get synonym (/x)
	 cmp   bx,offset e_switch  ; /X ??
	 je    set_extflag	   ; yes - check result buffer
	 jmp   SHORT Bad_Switch	   ; error exit

Set_ExtFlag:			   ; no, check reult buffer
	 mov   Ext_Mem,1	   ; yes, set Hi Memory flag
	 mov   si,Current_parm	   ; -->next parameter
	 mov   al,PosTYpe	   ; set prevvious type before look for next
	 mov   Prev_Type,al
	 jmp   parse_loop

Bad_Switch:
	mov   di,Next_Parm	  ; ending address of bad param  1/6/88
	mov   al,0
	mov   ds:[di],al	   ; set termination character
	LEA   SI,SUBLIST1	  ; DS:SI-->Substitution list
	MOV   AX,CURRENT_PARM	  ; starting address of bad parameter
	MOV   [SI].DATA_OFF,AX	  ; SI-->File name
	MOV   [SI].DATA_SEG,DS	  ; DS-->Segment
	MOV   [SI].MSG_ID,0	  ; message ID
	MOV   [SI].FLAGS,010H	  ; ASCIIZ string, left align
	MOV   [SI].MAX_WIDTH,0	  ; MAXIMUM FIELD WITH
	MOV   [SI].MIN_WIDTH,0	  ; MINIMUM FIELD WITH
	MOV   BX,Invalid_Switch   ; get message number
	MOV   MSG_NUM,BX	  ; set message number
	MOV   SUBST_COUNT,1	  ; substitution count
	MOV   MSG_CLASS,-1	  ; message class
	MOV   INPUT_FLAG,0	  ; no input
	CALL  PRINT_STDOUT	  ; display message
	stc			  ; error flag
	jmp   SHORT Parse_Exit	  ; exit		      (1/6/88 P2670)



;----------------------------------------------------------------------------
;	      PROCESS PARSE ERROR
;----------------------------------------------------------------------------
PARSE_ERROR:			   ; AX = meassage number
	MOV	MSG_CLASS,-1	   ; message class
	MOV	MSG_NUM,AX	   ; set message number
	MOV	SUBST_COUNT,0	   ; no message substitution
	MOV	INPUT_FLAG,0	   ; no input
	CALL	PRINT_STDOUT	   ; show message
	stc			   ; set error flag

Parse_Exit:			   ; EXIT
	push	cs
	pop	ds		   ; DS - Program data area seg
	ret
PARSE	ENDP			   ; end of parser


;----------------------------------------------------------------------------
;
; Procedure:  DISPLAY_OPTIONS
;
; Function:   Display the options help message lines on standard output.
;
;----------------------------------------------------------------------------

DISPLAY_OPTIONS	PROC NEAR

	mov	[MSG_NUM], MSG_OPTIONS_FIRST	; set first message number
	mov	[MSG_CLASS], -1	   		; message class
	mov	[SUBST_COUNT], 0	   	; no message substitution
	mov	[INPUT_FLAG], 0	   		; no input
DO_LOOP:
	call	PRINT_STDOUT	   		; show message
	cmp	[MSG_NUM], MSG_OPTIONS_LAST	; last message?
	je	DO_DONE				; done if so
	inc	[MSG_NUM]			; else bump msg number
	jmp	short DO_LOOP			;  and go do it
DO_DONE:
	ret					; return

DISPLAY_OPTIONS ENDP


;----------------------------------------------------------------------------
;
; Procedure:  PROC_DEFAULT
;
; Function:   Process default parameters if name and extend counts
;	      are not specified with the drive id.
;
;----------------------------------------------------------------------------

PROC_DEFAULT	PROC		   ; PROCESS DEFAULT
	 push  si		   ; makesure to save next chara pointer
	 mov   ax,Default_names    ; get default name count M003
	 mov   name_count,ax	   ; save it
	 add   Total_name_count,ax ; update total name count


	 mov   di,ParmBuff_Ptr	   ; DS:DI-->parameter buffer
	 mov   [di],ax		   ; save in buffer
	 add   Parmbuff_ptr,2	   ; points to next drive id position
	 mov   ax,Name_Count
	 CALL  Set_drive_Cache_Header	; Set Name cache header

Default_Exit:
	 pop   si
	 ret			   ; return

PROC_DEFAULT   ENDP


;----------------------------------------------------------------------------
; Procedure:   VERIFY_COUNTS
;
; Function:    Verify the validity of the name and extent counts

;----------------------------------------------------------------------------
VERIFY_COUNTS	PROC   NEAR

; Check the validity of NAME and EXTENT count entries


Chk_Name_Count:

	cmp	Total_name_count, Max_Entry_Num
	jg	invalid_name_entry
	clc				 ; Name count is OK
	jmp	short verify_exit	 ; exit


Invalid_name_entry:
	mov	ax,many_name_entries	 ; AX = error code
	stc

Verify_Exit:		     ;

	RET

VERIFY_COUNTS	ENDP











;=========================================================================
; CHECK_DRIVE
;-----------------------------------------------------------------------
;
;  INPUT:  AL - Drive letter
;
;  OUTPUT:
;	   If Carry = 0
;	     user_drive       set to current entered drive letter
;	     num_Of_drives  incremented
;	   If Carry = 1       error
;-----------------------------------------------------------------------
; 1) see if drive is valid and removable using int 21h IOCTL
;
; 2) use int 21h name translate to make sure that the drive is not
;    redirected, substed, on another machine, or in any other way shape
;    or form hosed.
;=========================================================================

CHECK_DRIVE    PROC    NEAR

	CALL	Convert_To_Caps 	; make sure it is a capital letter
	mov	byte ptr user_drive,al	; save it in user drive
	mov	byte ptr source_xname,al ; put in source string for call

	mov	bl,al			;put drive letter in bl
	sub	bl,"A"-1                ;convert to 1 based number

	mov	ah,ioctl		;set up for removable call
	mov	al,8			;function code
	INT	int_command

	cmp	ax,1			;is drive fixed?
	jz	okay_drive		;yes - see if it's subst
	cmp	ax,0fh			;is drive valid?
	jnz	hosed_drive		;yes - but hosed

	mov	ax,invalid_drive	; set bad drive message
	jmp	short drive_Error	; display error message

Okay_Drive:
        mov     ah,ioctl                ;set up for network call          ;*EGH
        mov     al,9                    ;function code                    ;*EGH
        INT     int_command             ;                                 ;*EGH
        test    dx,1000H                ;Q: is drive local?               ;*EGH
        jnz     hosed_drive             ; N: drive is hosed               ;*EGH
                                                                          ;*EGH
        lea     si,source_xname         ; set up for name translate
	lea	di,target_xname
	mov	ax,xNameTrans SHL 8
	INT	int_command		;do the translation

	lea	si,source_xname 	;compare source and target drive
	lea	di,target_xname

	mov	cx,Len_source_xname	;get count of invalid chars
	repz	cmpsb			;compare until mismatch found
	jz	check_drive_end 	;no mismatch - exit

Hosed_Drive:
	MOV   AX,BAD_USE_MESSAGE	 ; message number

Drive_Error:
	push  ax		  ; save message number
	mov   ax,Valuelo	  ; get drive letter number from result buff
				  ; C:=3 D:=4 etc, Parser drive id convention
	add   al,040H		  ; convert to drive letter
	lea   si,Drive_Id	  ; DS:SI-->drive letter save area
	mov   [si],al		  ; save drive letter in buffer

	LEA   SI,SUBLIST1	  ; DS:SI-->Substitution list
	MOV   AX,OFFSET DRIVE_ID
	MOV   [SI].DATA_OFF,AX	  ; SI-->File name
	MOV   [SI].DATA_SEG,DS	  ; DS-->Segment
	MOV   [SI].MSG_ID,1	  ; message ID
	MOV   [SI].FLAGS,010H	  ; ASCIIZ string, left align
	MOV   [SI].MAX_WIDTH,0	  ; MAXIMUM FIELD WITH
	MOV   [SI].MIN_WIDTH,0	  ; MINIMUM FIELD WITH
	POP   AX		  ; restore message number
	MOV   MSG_NUM,AX	  ; set message number
	MOV   SUBST_COUNT,1	  ; substitution count
	MOV   MSG_CLASS,-1	  ; message class
	MOV   INPUT_FLAG,0	  ; no input
	CALL  PRINT_STDOUT	  ; display message
	stc			  ; error flag

Check_Drive_End:
	ret			  ; return

CHECK_DRIVE  endp





;=========================================================================
;  Procedure:  SET_DRIVE_CACHE_HEADER
;
;  Function: Set name cache drive header
;
;  Input:  ax		    contains number of entries for num_entries
;	   user_drive	    contains user drive for drive_letter
;	   num_Of_drives  contains number of caches set up so far
;	   drive_cache	    offset of drive cache headers start
;  Output:
;	   If successful:
;	     drive cache header set up
;	     user_drive 	reset to blank
;	     num_Of_drives    incremented
;	   else
;	     bx 	      set to error flag
;	     dx 	      points to error message
;-----------------------------------------------------------------------
; 1) see if drive too many drives have been entered.
; 2) Walk through drive cache headers to make sure that the drive
;    letter was not previously entered.
; 3) Set up drive cache header
;=========================================================================

	public	set_drive_cache_header
SET_DRIVE_CACHE_HEADER	 PROC	 NEAR

	mov	cx,num_of_drives	  ;get current count of drives
	mov	bx,offset drive_cache	  ;get start of name drive cache
	mov	dl,user_drive		  ;get user entered drive
	dec	cx			  ;is this the 1st drive entered ?
	jcxz	set_it_up		  ;yes - don't check

	cmp	num_Of_drives,max_drives  ;no - check for maximum num of drives
	jng	we_have_room		  ;yes - go check for dup drives
	mov	ax,too_many_entries	  ;set up for error message
	stc				  ;set up error flag
	jmp	short set_dheader_exit	  ;and exit

;-----------------------------------------------------------------------
; Search through the drive headers to see the duplicate drive exist.
; If a new drive header at the bottom of the chain for the new drive.
; If no drives exist, then create the new header as the first drive header.
;-----------------------------------------------------------------------
We_Have_Room:				  ;BX-->current drive header
	cmp	dl,[bx].dch_drive_letter  ;drive header exist for this drive??
	jnz	not_dup_drive		  ;no - continue
	mov	ax,dup_drive		  ;yes - set up for error message
	stc
	jmp	short set_dheader_exit	  ;exit

Not_Dup_Drive:
	cmp	[bx].dch_sibling_ptr,no_siblings  ;any more header to search ??
	jz	set_drive_sibling		  ;no - go create the new drive header
	add	bx,size drive_cache_header	  ;yes - get pointer to next drive header
	jmp short we_have_room			  ;check it

Set_drive_sibling:
	mov	cx,bx				  ;save current header address
	add	cx,size drive_cache_header	  ;pointer to next header
	mov	[bx].dch_sibling_ptr,cx 	  ;set pointer to new header from current hdr
	mov	bx,cx				  ;BX-->new header

Set_it_up:
	mov	[bx].dch_drive_letter,dl	  ;save drive letter in new header
	mov	[bx].dch_sibling_ptr,no_siblings  ;mark new header as last header in chain
	mov	[bx].dch_num_entries,ax 	  ;save name count in new header

Set_dheader_Exit:				  ; Exit
	ret

SET_DRIVE_CACHE_HEADER	 ENDP





subttl	Convert to caps
page
;=========================================================================
; Procedure: Convert_to_caps
;
; CONVERT LOWER CASE CHARACTERS TO UPPER CASE
; Convert character in al to a capital letter.

;=========================================================================

CONVERT_TO_CAPS    PROC     NEAR

	cmp	al,"a"
	JNAE	no_convert
	cmp	al,"z"
	JNBE	no_convert
	sub	al,32

No_Convert:
	ret					;and return

CONVERT_TO_CAPS    ENDP







;=========================================================================
; SET_EMS		: THIS MODULE SETS EMS FOR FASTOPEN CODE AND DATA
;			  PAGE 0 IN HIGH MEMORY IS MAPPED FOR CODE USING
;			  THE PHYSICAL PAGE FRAME  AND PAGE 1 IS
;			  MAPPED FOR DATA USING PHYSICAL PAGE FRAME NUMBER
;			  TWO PHYSICAL PAGE FRAME SEG IDs ARE SAVED AND
;			  THEY WILL BE USED BY THE (MAIN) ROUTINE.
;
;			For IBM:
;			  Dynamic Page allocation buffers are running in the
;			  dynamic page allocation mode.
;
;			  Dynamic Page mode is also selected if buffers are
;			  running in the base memory.
;
;			  Static meode is selected if Buffers are running in
;			  the static mode.
;
;
;	INPUTS		: NONE
;
;	OUTPUTS 	: CY  - ERROR
;
;			  NC  - EMS_PAGE_SEG - SEG ID OF SINGLE PAGE FRAME
;=========================================================================

SET_EMS  PROC  NEAR
	CALL	EMS_CHECK1		;SEE IF EMS INSTALLED
	JNC	EMS_GET_PAGE		; yes, get page

	MOV	EMS_FLAG,0		;  Flag EMS not installed
	JMP	EMS_EXIT	  ;  Leave check routine

EMS_GET_PAGE:
	PUSH	ES			; save ES,DI they may destroy by 2F
	PUSH	DI

IF	NOT BUFFERFLAG

	MOV	AH,EMS_2F_HANDLER
	XOR	AL,AL
	INT	2FH			; see 2F is there
	CMP	AL,0FFH
	JNE	EMS_SET_DYNAMIC 	; INT 2F handler no there, run in Dynamic mode

	MOV	EXT_MEM, STATIC_ALLOC	;AN002; Set static allocation mode
	MOV	EMS_FLAG, STATIC_ALLOC
	MOV	AH,EMS_2F_HANDLER
	MOV	AL,0FFH
	MOV	DI,0FEH
	INT	2FH		       ; get EMS page
	OR	AH,AH
	JNZ	EMS_PAGE_ERR
	MOV	EMS_PAGE_SEG,ES        ; SAVE PAGE SEG ID
	MOV	EMS_PAGE_NUM,DI        ; SAVE PHYSICAL PAGE NUMBER
	JMP	SKIP_DYNA_PAGE		; CONTINUE

EMS_SET_DYNAMIC:
	MOV	EXT_MEM, DYNA_ALLOC    ;AN002; Set dynamic mode
	MOV	EMS_FLAG, DYNA_ALLOC

ENDIF

;---------------------------------------------------------------HKN 8/25/88
;	Fastopen must get an EMS page like a well behaved program and
;	should not grab a reserved page from the BIOS.
;
	mov	cx, FRAME_COUNT
	xor	ax, ax
	mov	bx, ax
	mov	dx, ax

get_page:
	cmp	es:[di], 0a000h		; is the page in ax above 640K
	jb	next_page		; if no get next_page

	mov	bx, di			; we have a valid page

	inc	dx			; count the # of pages above 640K

	cmp	dx, 1
	je	next_page
	sub	di, 4
	mov	ax, es:[di]
	mov	[FST_PAGE], ax
	mov	ax, es:[di+2]
	mov	[FST_PAGE+2], ax
	mov	di, bx			; restore di

next_page:
	add	di, 4
	loop	get_page
	jne	found_page
	jmp	SHORT ems_page_err

found_page:
;	int	3
	cmp	dx, 1
	jne	second_last_page
	mov	di, bx
	mov	ax, es:[di]
	mov	ems_page_seg, ax
	mov	ax, es:[di+2]
	mov	ems_page_num, ax
	jmp	SHORT save_state

second_last_page:
	mov	ax, [FST_PAGE]
	mov	ems_page_seg, ax
	mov	ax, [FST_PAGE+2]
	mov	ems_page_num, ax

save_state:
	push	es
	mov	ax, Cseg_Main
	mov	es, ax
	assume 	es:Cseg_Main

;;; johnhe 12/31/89;	mov	word ptr save_map_addr, offset es:save_ems_page_state
;;; johnhe 12/31/89;	mov	word ptr save_map_addr + 2, ax

	mov	ax, ems_page_seg
	mov	es:Main_EMS_PAGE_SEG, ax
	pop	es
	assume	es:Cseg_Init

;;; johnhe 12/31/89;	call	[save_map_addr]
	call	SaveEmsState				; johnhe 12/31/89

	jc	ems_page_err

;--------------------------------------------------------------------------
IF	NOT BUFFERFLAG
SKIP_DYNA_PAGE:
ENDIF

	POP	DI
	POP	ES
	JMP	SHORT EMS_ALLOCATE_PAGE

EMS_PAGE_ERR:
	POP	DI
	POP	ES
	STC				;yes, page not found
	JMP	SHORT EMS_ERROR 	;error exit

;-----------------------------------------------------------------------
; Allocate one page
;-----------------------------------------------------------------------
EMS_ALLOCATE_PAGE:
	MOV	BX,1		     ;one page
	MOV	AH,EMS_ALLOC_PAGES   ;set op code
	INT	EMS_INT 	     ;allocate page
	OR	AH,AH		     ;Was there an error allocating?
	JNZ	EMS_ERROR	     ;yes - display error
	MOV	EXT_HANDLE,DX	     ;no -Save EMS handle


;------------------------------------------------------HKN 8/25/88
;	Must save ems handle in Cseg_Main also.

IF	NOT BUFFERFLAG
; If dynamic allocation, save handle in CSEG_MAIN
	CMP	EMS_Mem,Dyna_Alloc   ;AN002; Dynamic Allocation ??
	JNE	Skip_Save_Handle	;AN002; no - skip saving handle
ENDIF

	push	es
	push	ax
	mov	ax, Cseg_Main
	mov	es, ax
	assume	es:Cseg_Main
	mov	es:ems_save_handle1, dx
	pop	ax
	pop	es
	assume	es:Cseg_Init

IF	NOT BUFFERFLAG
SKIP_SAVE_HANDLE:
ENDIF

;-----------------------------------------------------------------------
; SET HANDLE NAME TO THE PAGE HANDLE
;-----------------------------------------------------------------------
	PUSH	DS
	POP	ES
	ASSUME	ES:CSEG_INIT
	LEA	SI,HANDLE_NAME	     ; DS:SI-->Handle name string
	MOV	DX,EXT_HANDLE	     ; handle number
	MOV	AH,EMS_HANDLE_NAME
	MOV	AL,1		     ; set op code code
	INT	67H		     ; set handle
	OR	AH,AH
	JNZ	EMS_ERROR	     ; jump if error

;-----------------------------------------------------------------------
; Map logical page 0 in physical page frame FE (P254)
;-----------------------------------------------------------------------
	CALL	MAP_FRAME	    ;map two pages
	JNC	EMS_GET_SIZE	    ;no error, normal exit

;-----------------------------------------------------------------------
; Get partial page map size
;-----------------------------------------------------------------------
EMS_GET_SIZE:
	MOV	AH,EMS_PAGE_SIZE    ;Allocate requested pages
	MOV	AL,2
	INT	EMS_INT 	    ;
	OR	AH,AH
	JNZ	EMS_ERROR
	XOR	AH,AH
	MOV	EMS_PAGESIZE,AX     ;save EMS page size
	CLC
	JMP	SHORT EMS_EXIT

EMS_ERROR:
	MOV	AX,EMS_FAILED	    ;error message
	MOV	MSG_NUM,AX	    ;save message number
	MOV	SUBST_COUNT,0	    ;no message substitution
	MOV	MSG_CLASS,-1	    ;message class
	MOV	INPUT_FLAG,0	    ;no input
	CALL	PRINT_STDOUT	    ;show message "Incorrect Parameter"
	STC			    ; set error flag

EMS_EXIT:
	RET			    ;	 Return

SET_EMS   ENDP






;=========================================================================
; EMS_CHECK1		: THIS MODULE DETERMINES WHETHER OR NOT EMS IS
;			  INSTALLED FOR THIS SESSION.
;
;	INPUTS		: NONE
;
;	OUTPUTS 	: ES:BX - FRAME ARRAY
;			  CY	- EMS NOT AVAILABLE
;			  NC	- EMS AVAILABLE
;=========================================================================

	public	EMS_CHECK1
EMS_CHECK1 PROC NEAR			;EMS INSTALL CHECK

	mov	ax,3567h		; Get interrupt vector
	int	21h
	mov	di,0Ah			; fixed offset of EMS name in dev header
	mov	si, offset EMS_NAME	; standard EMS device name string
	mov	cx, length EMS_NAME
	cld
	repe cmpsb			; do they match?
	JNE	 EMS_NOT_INST1		 ;no, EMS not installed

	MOV	AH,EMS_GET_STATUS	;YES, GET STATUS
	INT	EMS_INT 		;INT 67H
	CMP	AH,0			;EMS MANAGER PRESENT ??
	JNE	EMS_NOT_INST1		;NO, EMS NOT INSTALLED

	MOV	AH,EMS_GET_VERSION	;YES, GET STATUS
	INT	EMS_INT 		;INT 67H
	CMP	AH,0			;EMS MANAGER PRESENT ??
	JNE	EMS_NOT_INST1		;NO, EMS NOT INSTALLED

	CMP	AL,40H			;VERSION 4.0 ??
	JNE	EMS_NOT_INST1		;NO, EMS NOT INSTALLED

	MOV	AX,EMS_GET_COUNT
	INT	EMS_INT 		;GET ARRAY COUNT
	CMP	AH,0
	JNE	EMS_NOT_INST1

	MOV	FRAME_COUNT,CX

	MOV	AX, BUFFER_ENTRY_SIZE
	MUL	CX			; CALCULATE THE ARRAY SIZE BE RESERVED
	CMP	AX, 100h
	JG	EMS_NOT_INST1

	MOV	AX,EMS_GET_FRAME_ADDR	;YES, GET FRAME ADDRESS
	PUSH	DS			;SWAP DS & ES
	POP	ES			;
	LEA	DI,FRAME_BUFFER 	;ES:DI--> RESULT BUFFER
	INT	EMS_INT 		;GET FRAME ADDRESSES
	CMP	AH,0			;IS EMS INSTALLED
	JNE	EMS_NOT_INST1		;NO,exit
	CMP	CX,FRAME_COUNT		;
	JNE	SHORT EMS_NOT_INST1

	CLC
	MOV	EMS_FLAG,1		; EMS IS ACTIVE, SET FLAG
	JMP	SHORT EMS_CHECK1_EXIT

EMS_NOT_INST1:				;EMS NOT INSTALLED
	MOV	AX,EMS_NOT_INSTALL	;error message
	MOV	MSG_NUM,AX		;set message number
	MOV	SUBST_COUNT,0		;no message substitution
	MOV	MSG_CLASS,-1		;message class
	MOV	INPUT_FLAG,0		;no input
	CALL	PRINT_STDOUT		;show message
	STC				;FLAG EMS NOT INSTALLED

EMS_CHECK1_EXIT:			;EXIT ROUTINE
	RET				;RETURN TO CALLER

EMS_CHECK1 ENDP




;=========================================================================
; MAP_FRAME		: THIS MODULE MAPS TWO LOGICAL PAGES IN THE HIGH
;			  MEMORY TO TWO PHYSICAL PAGE FEAMES IN THE LOW
;			  MEMORY.
;
;	INPUTS		: EXT_HANDLE  - HANDLE
;
;	OUTPUTD 	  CY	- ERROR
;			  NC	- PAGE IS MAPPED
;=========================================================================

MAP_FRAME    PROC  NEAR 		; MAP physical page frames
	PUSH	BX			; DMS;
	XOR	BX,BX			; Logical page 0
	MOV	AX,EMS_PAGE_NUM 	; AL=Physical Page frame number
	MOV	AH,EMS_MAP_HANDLE	; AH=EMS function to map page
	MOV	DX,EXT_HANDLE		; EMS handle
	INT	EMS_INT
	OR	AH,AH			; Was there an error allocating?
	JNZ	MAP_ERROR		; yes - set flag
	CLC
	JMP	SHORT MAP_EXIT		; no - exit

MAP_ERROR:
	STC				; set error flag

MAP_EXIT:
	POP	BX
	RET				; return


MAP_FRAME ENDP






;************************************************************
;*
;*   SUBROUTINE NAME:	   PRINT_STDOUT
;*
;*   SUBROUTINE FUNCTION:
;*	   Display the requested message to the specified handle
;*
;*   INPUT:
;*	     Paramters in parater storage area
;*	     DS:SI-->Substitution List
;*	     ES:DI-->PTR to input buffer if buffered keyboard
;*		     input is specified (DL = 0A)
;*   OUTPUT:
;*	     AX =   Single character entered if DL=01
;*		OR
;*	     ES:DI-->input buffer where string is returned if DL=0A
;*
;*	The message corresponding to the requested msg number will
;*	be written to Standard Out. Message substitution will
;*	be performed if specified
;*
;*   NORMAL EXIT:
;*	Message will be successfully written to requested handle.
;*
;*   ERROR EXIT:
;*	None.  Note that theoretically an error can be returned from
;*	SYSDISPMSG, but there is nothing that the application can do.
;*
;*   INTERNAL REFERENCES:    SysDispMsg
;*
;*   EXTERNAL REFERENCES:
;*	None
;*
;************************************************************
PRINT_STDOUT PROC NEAR
	PUSH	BX
	PUSH	CX
	PUSH	DX

	MOV	AX,MSG_NUM		; Message ID
	MOV	BX,STDOUT		; standard input message handle
	MOV	CX,SUBST_COUNT		; message substitution count
	MOV	DH,MSG_CLASS		; message class
	MOV	DL,INPUT_FLAG		; Type of INT 10 for KBD input

	CALL	SYSDISPMSG		;  AX=Extended key value if wait
					;for key
	JNC	DISP_DONE		; If CARRY SET then registers
					;will contain extended error info
					;	AX - Extended error Number
					;	BH - Error Class
					;	BL - Suggested action
DISP_DONE:				;	CH - Locus
	POP	DX
	POP	CX
	POP	BX

	RET
PRINT_STDOUT ENDP


;---------------------------------------------------------------------------
;	Procedure name	:	SaveEmsState
;
;	Description:
;		Saves the state of the page whose physical segment value is
;	specified in Main_EMS_PAGE_SEG.
;
;	johnhe 12/31/89 - Added to eliminate having to declare the same
;			  function in the resident code as FAR
;---------------------------------------------------------------------------

SaveEmsState	PROC	NEAR
	assume	DS:cseg_main

	PUSH	AX			  ; save registers
	PUSH	DS
	PUSH	ES
	PUSH	SI
	PUSH	DI

	mov	AX,SEG EMS_PAGE_ARRAY
	mov	DS,AX
	mov	ES,AX

	mov	DI,offset EMS_PAGE_ARRAY ; ES:DI-->Page ARRAY
	mov	SI,offset EMS_PAGE_MAP	 ; DS:SI-->page map struc

	MOV	AH,EMS_SAVE_STATE	  ;
	MOV	AL,0			  ; subfunction code
	INT	EMS_INT 		  ; save page state

	add	AH,0ffh 		  ; Set carry if AH != 0

	POP	DI
	POP	SI
	POP	ES
	POP	DS
	POP	AX
	RET

SaveEmsState ENDP


ifdef DBCS
;
;	Set DBCS lead byte table address
;

	assume	es:cseg_open

SET_DBCS_TABLE	proc	near
	push	ax
	push	si
	push	ds
	push	es
	mov	ax,cseg_open
	mov	es,ax
	mov	ax,6300h
	int	21h			; get DBCS lead byte table
	mov	word ptr es:DBCSLeadByteTable,si
	mov	word ptr es:DBCSLeadByteTable+2,ds
	pop	es
	pop	ds
	pop	si
	pop	ax
	ret
SET_DBCS_TABLE	endp
endif


CSEG_INIT	ENDS


;===========================================================================
;;;	    STACK    SEGMENT	   SIZE = 20 PARAGRAPHS
;===========================================================================

STACK		SEGMENT PARA STACK 'STACK'
		DB	64 dup("STACK   ")     ; 512  WORD STACK AREA
STACK		ENDS


END		START
