; Copyright 1990 Central Point Software, Inc. 
; All rights reserved.
;--------------------------------------------------------------------
; This is Jim's portion of the program code for REBUILD.COM 4.23+
; GWD is supporting this, now.
;
; Removed all the dumb stuff regarding 'stand_alone'.  GWD.
; Now 'INCLUDEs' the messages, instead of EXTRNing them.  4-19-88 GWD.
; Added the dumb stuff that handles large disks that req
; more than two controls records.  4-25-88 JIM.
; Created equates for YES_CHAR and NO_CHAR (helps foreign vers) 5-24-88 GWD.
; Substituted YES_CHAR etc, for all the Y's and N's.  6-7-88.
; To fix problem with UnFormat finding parts of MIRROR.FIL as
;  subdirectories (Ack!), MIRROR.COM now pokes an FF into each
;  root entry at offset 21.  Added code here to undo that 6-8-88 v4.24.
; Now handles different DOS 4.xx DPB structure.  4.30  7-27-88.
; Now recognizes Zenith 3.30 DOS (like 3.31, huge partns).  5.00  11-18-88.
; Fixed date/time formats for foreign countries.  5.5	5-25-89.
; During long search for control file, checks for Ctrl-C.  5.5	 5-30-89.
; Translation-equates LAST_CHAR, PRIOR_, ABORT_, RETRY_, etc.  v6  03-05-90.
;
;       M000    MD      9/23/90         Removed output of CPS copyright
;       M002    MD                      Exit properly on user request


	 PAGE  66,132

DOSEXEC  MACRO
	 INT   21H
	 ENDM


; LIST_COUNT EQU WORD PTR DS:[SI]+18
; XLAT_TABLE EQU WORD PTR DS:[SI]+20

str1	STRUC
	DB	18 DUP (?)
list_count DW	?
xlat_table DW	?
str1	ENDS
;
str2	STRUC
from_lsn_lo DW	?
from_lsn_hi DW	?
to_lsn_lo DW	?
to_lsn_hi DW	?
str2	ENDS

; used to build the sector table
; FROM_LSN_LO EQU   WORD PTR DS:[BX]
; FROM_LSN_HI EQU   WORD PTR DS:[BX]+2
; TO_LSN_LO   EQU   WORD PTR DS:[BX]+4
; TO_LSN_HI   EQU   WORD PTR DS:[BX]+6



prog	SEGMENT PARA PUBLIC
	ASSUME CS:prog
;
	 PUBLIC J_REBUILD		;Referenced from UF_MAIN module.
;
	EXTRN	date_control:word, date_separator:byte	;In UF_IO module.
;
	INCLUDE uf_jmsg.asm



HIGHEST_CLUSTER        DW 0
SECTOR_SIZE	       DW 0
NUM_SECTORS_TO_HANDLE  DW 0
NUM_BOOT_FAT_SECTORS   DW 0
NUM_DIR_SECTORS        DW 0
FIRST_DIRECTORY_SECTOR DW 0
HARD_WAY_SECTOR_NUM_LO DW 0
HARD_WAY_SECTOR_NUM_HI DW 0

ABS_25_LIST   LABEL BYTE
ABS_25_SEC_LO DW 0
ABS_25_SEC_HI DW 0
ABS_25_COUNT  DW 0
ABS_25_OFF    DW 0
ABS_25_SEG    DW 0

MIRROR_SWITCH DB 0
ABS_25_HARD_WAY EQU 80H

SECTORS_PER_CLU   DW 0
FIRST_DATA_SECTOR DW 0
LAST_SECTOR_LO	  DW 0
LAST_SECTOR_HI	  DW 0
IO_AREA_PTR	  DW 0
dont_fix_cnt	DW 0

R_INFO	 EQU   $

R_CTL1_LSN_LO DW 0 ;***
R_CTL1_LSN_HI DW 0 ;
		   ; Gotta keep this stuff for COMPAQ DOS 3.31 >32 Meg
R_CTL2_LSN_LO DW 0 ;
R_CTL2_LSN_HI DW 0 ;***

R_SECTOR_SIZE		 DW 0
R_NUM_SECTORS_TO_HANDLE  DW 0
R_NUM_BOOT_FAT_SECTORS	 DW 0
R_NUM_DIR_SECTORS	 DW 0
R_FIRST_DIRECTORY_SECTOR DW 0
R_HEADS 		 DW 0
R_NUM_FATS		 DB 0
R_NUM_RESERVED		 DW 0
R_SECTORS_PER_CLU	 DB 0
R_SECTORS_PER_TRACK	 DW 0
R_NUM_HIDDEN		 DW 0
R_NUM_CTL_SECTORS	 DW 0  ; 4/24/88
R_ALT_SECTOR_SIZE	 DW 0  ; 4/24/88

	 DB    16 DUP(0)
	 ORG   R_INFO+48
R_INFO_LEN EQU $-R_INFO

CS_SAVE  DW    0
AX_SAVE  DW    0
BX_SAVE  DW    0
PSP_SAVE DW    0

OK_DWORD   DD  0
	   ORG OK_DWORD
OK_OFFSET  DW  0
OK_SEGMENT DW  0

NAMES	 DB    'MIRROR  FIL'
	 DB    'MIRROR  BAK'
	 DB    'MIRORSAVFIL'

ASCIIZ	 DB    'x:\'
	 DB    'MIRROR.FIL',0

CHECKS_FOR_OUR_MIRROR_FILE  DW	 0
MAX_MIRROR_CLUSTERS	    DW	 0
MIRROR_HI_CLU		    DW	 0
OUR_SIGNATURE	   DB 'aMSESLIFVASRORIMESAEP'
OUR_SIGNATURE_LEN      EQU  $-OUR_SIGNATURE

MIRROR_YEAR_1	EQU OUR_SIGNATURE_LEN+4
MIRROR_MODAY_1	EQU OUR_SIGNATURE_LEN+4+2
MIRROR_TIME_1	EQU OUR_SIGNATURE_LEN+4+4
MIRROR_LSN_LO_2 EQU OUR_SIGNATURE_LEN+4+6
MIRROR_LSN_HI_2 EQU OUR_SIGNATURE_LEN+4+8
MIRROR_YEAR_2	EQU OUR_SIGNATURE_LEN+4+10
MIRROR_MODAY_2	EQU OUR_SIGNATURE_LEN+4+12
MIRROR_TIME_2	EQU OUR_SIGNATURE_LEN+4+14

SEC_LO_FROM_MIRROR_1  DW  0
SEC_HI_FROM_MIRROR_1  DW  0
TIME_FROM_MIRROR_1    DW  0FFFFH
YEAR_FROM_MIRROR_1    DW  0
MODAY_FROM_MIRROR_1   DW  0
SEC_LO_FROM_MIRROR_2  DW  0
SEC_HI_FROM_MIRROR_2  DW  0
TIME_FROM_MIRROR_2    DW  0
YEAR_FROM_MIRROR_2    DW  0
MODAY_FROM_MIRROR_2   DW  0

SWITCH	 DB    0
JUST_CHECK     EQU 80H
INIT_F_O_F     EQU 40H
MATCH_FOUND    EQU 20H
DRIVE_FIXED    EQU 10H
REPORT_ERRORS  EQU 08H
GOT_TWO_FILES  EQU 04H
HARD_WAY       EQU 02H
ESCAPE_PRESSED EQU 01H

CHECK_FAILED DW 0
EXIT_ERROR   DB 0
;	 1 = ALL OK
;	 2 = USER CANCELLED
;	 3 = DID NOT RUN SUCCESSFULLY. DID NOT MODIFY
;	 4 = DID NOT RUN SUCCESSFULLY. DID MODIFY
;	 5 = STRANGE ENVIRONMENT OR DISK
;	 6 = PANIC EXIT, ABORT MAIN PROGRAM

DOS_VERSION  DW 0

K_ESCAPE EQU 27

ABS_SECTOR_LO DW 0
ABS_SECTOR_HI DW 0

NO_COPYRIGHT    equ     00h            ;M000

COPYRIGHT DB   10,13

	DB	"REBUILD  "             ;M000
	DB	24h

H_E_SECTOR_NUM DB '0000000.$'



J_REBUILD PROC NEAR
	 MOV   IO_AREA_PTR,BX
	 ADD   AL,41H
	 MOV   ASCIIZ,AL
	 MOV   drive_msg_patch,AL	;5-24-88 GWD.
	 MOV   WANNA_DRIVE,AL
	 MOV   SUCC_DRIVE,AL
	 MOV   SUCC_ALT_D,AL
	 MOV   CS_SAVE,CS
	 MOV   OUR_SIGNATURE,'A'
	 CALL  ISSUE_COPYRIGHT

	 CMP   AH,0
	 JE    J_REBUILD_1
	 OR    SWITCH,JUST_CHECK
	 LEA   DX,JUST_CHECKING_MSG
	 MOV   DS,CS_SAVE
	 MOV   AH,9
	 DOSEXEC

J_REBUILD_1:
	 CALL  CHECK_OBVIOUS_STUFF
	 MOV   EXIT_ERROR,5
;	 5 = STRANGE ENVIRONMENT OR DISK
	 JNC   START_REBUILD
	 JMP   BAIL_OUT

START_REBUILD:
	 MOV   AH,0DH
	 DOSEXEC

LOOK_SOME_MORE:
         MOV   EXIT_ERROR,2             ;M002 - Set error for user cancel
	 TEST  SWITCH,ESCAPE_PRESSED
	 JNZ   BAIL_OUT
	 CALL  FIND_OUR_FILE
	 JC    NO_FILE_FOUND

	 OR    SWITCH,MATCH_FOUND
	 CALL  VERIDATE_FILE
	 JC    LOOK_SOME_MORE

	 OR    SWITCH,REPORT_ERRORS
	 CALL  RE_BUILD_SYSTEM_AREA
	 JC    BAIL_OUT

	 LEA   DX,FAILED_CHK
	 CMP   CHECK_FAILED,0		; works only if all files in the
	 JA    FINAL_MSG		; same sector
	 LEA   DX,SUCC
	 TEST  SWITCH,JUST_CHECK
	 JZ    FINAL_MSG
	 LEA   DX,SUCC_ALT

FINAL_MSG:
	 MOV   DS,CS_SAVE
	 MOV   AH,9
	 DOSEXEC

	 MOV   AH,0DH
	 DOSEXEC

	 MOV   EXIT_ERROR,1
;	 1 = ALL OK
	 MOV   AL,EXIT_ERROR
	 RET

NO_FILE_FOUND:
	 LEA   DX,USER_CANCELLED
;	 3 = DID NOT RUN SUCCESSFULLY. DID NOT MODIFY
	 CMP   EXIT_ERROR,3
	 JNE   STC_EXIT
	 LEA   DX,NO_FILE
	 TEST  SWITCH,HARD_WAY
	 JNZ   STC_EXIT
	 TEST  SWITCH,MATCH_FOUND
	 JZ    STC_EXIT
	 LEA   DX,BAD_FILE

STC_EXIT:
	 MOV   DS,CS_SAVE
	 MOV   AH,9
	 DOSEXEC

BAIL_OUT:
	 LEA   DX,PROBLEMS
	 MOV   AH,9
	 DOSEXEC

	 MOV   AH,0DH
	 DOSEXEC

	 MOV   AL,EXIT_ERROR
	 RET

J_REBUILD ENDP



ISSUE_COPYRIGHT PROC NEAR
	 PUSH  AX
	 MOV   DS,CS_SAVE

	 LEA   DX,REBUILD_MAIN_MSG	    ; This will print the other part
	 MOV   AH,9			    ; of the message
	 DOSEXEC

	 POP   AX
	 RET

ISSUE_COPYRIGHT ENDP


VERIDATE_FILE PROC NEAR
	 CALL  INSERT_DATE

	 TEST  SWITCH,GOT_TWO_FILES
	 JZ    VERIDATE_FIRST
	 CALL  ASK_WITCH_FILE
	 JNC   VERIDATE_FIRST
	 TEST  SWITCH,ESCAPE_PRESSED
	 JNZ   V_F_STC_JMP
	 MOV   AX,SEC_LO_FROM_MIRROR_2	; where its at man
	 MOV   ABS_25_SEC_LO,AX
	 MOV   AX,SEC_HI_FROM_MIRROR_2	; where its at man
	 MOV   ABS_25_SEC_HI,AX
	 JMP   short VERIDATE_SECOND

VERIDATE_FIRST:
	 MOV   AX,SEC_LO_FROM_MIRROR_1	; where its at man
	 MOV   ABS_25_SEC_LO,AX
	 MOV   AX,SEC_HI_FROM_MIRROR_1	; where its at man
	 MOV   ABS_25_SEC_HI,AX

VERIDATE_SECOND:
	 MOV   BX,IO_AREA_PTR
	 MOV   CX,1
	 CALL  ABS_25

	 MOV   DS,CS_SAVE
	 MOV   ES,CS_SAVE
	 LEA   DI,R_INFO
	 MOV   SI,IO_AREA_PTR
	 ADD   SI,R_IO_INFO_OFFSET
	 MOV   CX,R_INFO_LEN
	 REP   MOVSB
	 MOV   AX,R_SECTOR_SIZE
	 CMP   AX,SECTOR_SIZE
	 JE    V_F_1

; Note, if R_SECTOR_SIZE is zero and the new field, R_NUM_CTL_SECTORS is
; two or greater, then we'll assume we have the new style that handles
; large multi-megabyte hard disks.

	 CMP   R_SECTOR_SIZE,0
	 JNE   V_F_STC_JMP
	 CMP   R_NUM_CTL_SECTORS,2
	 JB    V_F_STC_JMP
	 MOV   AX,R_ALT_SECTOR_SIZE
	 MOV   R_SECTOR_SIZE,AX
	 CMP   AX,SECTOR_SIZE
	 JE    V_F_1

V_F_STC_JMP:
	 JMP   V_F_STC

V_F_1:
	 MOV   AX,R_NUM_SECTORS_TO_HANDLE
	 CMP   AX,NUM_SECTORS_TO_HANDLE
	 JNE   V_F_STC_JMP
	 MOV   AX,R_NUM_BOOT_FAT_SECTORS
	 CMP   AX,NUM_BOOT_FAT_SECTORS
	 JNE   V_F_STC_JMP
	 MOV   AX,R_NUM_DIR_SECTORS
	 CMP   AX,NUM_DIR_SECTORS
	 JNE   V_F_STC_JMP
	 MOV   AX,R_FIRST_DIRECTORY_SECTOR
	 CMP   AX,FIRST_DIRECTORY_SECTOR
	 JNE   V_F_STC_JMP

	 MOV   AX,R_CTL1_LSN_LO 	; does this record point to
	 CMP   ABS_25_SEC_LO,AX 	; itself?
	 JNE   V_F_STC_JMP
	 MOV   AX,R_CTL1_LSN_HI
	 CMP   ABS_25_SEC_HI,AX
	 JNE   V_F_STC_JMP

	 MOV   BX,IO_AREA_PTR
	 MOV   OK_OFFSET,BX
	 MOV   OK_SEGMENT,DS

V_F_READ_CTL_1:
	 MOV   AX,R_CTL1_LSN_LO
	 MOV   ABS_25_SEC_LO,AX
	 MOV   AX,R_CTL1_LSN_HI
	 MOV   ABS_25_SEC_HI,AX

	 CMP   R_NUM_CTL_SECTORS,2	; we have the special stuff?
	 JBE   V_F_READ_CTL_1_A 	; no, branch

	 MOV   CX,R_NUM_CTL_SECTORS	; yes, get # ctl sectors
	 MOV   AH,0
	 MOV   AL,R_SECTORS_PER_CLU
	 CMP   CX,AX			; greater than sectors per clu?
	 JBE   V_F_READ_CTL_1_B 	; no, branch
	 MOV   CX,AX			; yes, force it
	 JMP   short V_F_READ_CTL_1_B

V_F_READ_CTL_1_A:
	 MOV   CX,1			; old way

V_F_READ_CTL_1_B:
	 MOV   AX,SECTOR_SIZE
	 MUL   CX
	 ADD   OK_OFFSET,AX

	 CALL  ABS_25
	 JNC   V_F_CTL1_OK
	 JMP   short V_F_STC

V_F_CTL1_OK:
	 LDS   BX,OK_DWORD

	 MOV   AX,R_CTL2_LSN_LO
	 MOV   ABS_25_SEC_LO,AX
	 MOV   AX,R_CTL2_LSN_HI
	 MOV   ABS_25_SEC_HI,AX

	 CMP   R_NUM_CTL_SECTORS,2
	 JBE   V_F_READ_CTL_2_A

	 MOV   CX,R_NUM_CTL_SECTORS
	 MOV   AH,0
	 MOV   AL,R_SECTORS_PER_CLU
	 CMP   CX,AX
	 JBE   V_F_CTL2_OK
	 SUB   CX,AX
	 JMP   short V_F_READ_CTL_2_B

V_F_READ_CTL_2_A:
	 MOV   CX,1

V_F_READ_CTL_2_B:
	 MOV   AX,SECTOR_SIZE
	 MUL   CX
	 ADD   OK_OFFSET,AX

	 CALL  ABS_25

	 JC    V_F_STC

V_F_CTL2_OK:
	 MOV   DS,CS_SAVE
	 MOV   ES,CS_SAVE
	 CLC
	 RET

V_F_STC:
	 TEST  SWITCH,HARD_WAY
	 JZ    V_F_STC_1
	 MOV   DS,CS_SAVE
	 LEA   DX,HARD_WAY_BACKUP_BAD
	 MOV   AH,9
	 DOSEXEC

V_F_STC_1:
	 STC
	 RET

VERIDATE_FILE ENDP



ASK_WITCH_FILE PROC NEAR
	 MOV   DS,CS_SAVE
	 LEA   DX,WITCH_MSG
	 MOV   AH,9
	 DOSEXEC
	 MOV   AX,0C01H
	 DOSEXEC
	 CMP   AL,K_ESCAPE
	 JE    ASK_WITCH_ESC
	 AND   AL,NOT 20H
	 CMP   AL,last_char	;'L'
	 JE    ASK_WITCH_CLC
	 CMP   AL,prior_char	;'P'
	 JE    ASK_WITCH_STC
	 JMP   ASK_WITCH_FILE

ASK_WITCH_CLC:
	 CLC
	 JMP   short ASK_WITCH_EXIT

ASK_WITCH_ESC:
	 OR    SWITCH,ESCAPE_PRESSED

ASK_WITCH_STC:
	 STC

ASK_WITCH_EXIT:
	 PUSHF
	 LEA   DX,LINE_DOWN
	 MOV   AH,9
	 DOSEXEC
	 POPF
	 RET

ASK_WITCH_FILE ENDP



RE_BUILD_SYSTEM_AREA PROC NEAR

RE_BUILD_SYSTEM_AREA_ASK:
	 MOV   DS,CS_SAVE
	 LEA   DX,LOOKS_GOOD
	 MOV   AH,9
	 DOSEXEC

	 TEST  SWITCH,JUST_CHECK
	 JNZ   DONT_ASK

	 LEA   DX,WANNA_CONT
	 MOV   AH,9
	 DOSEXEC
	 MOV   AX,0C01H
	 DOSEXEC
	 CMP   AL,K_ESCAPE
	 JE    RE_BUILD_ESCAPE
	 AND   AL,NOT 20H
	 CMP   AL,no_char
	 JE    RE_BUILD_ESCAPE
	 CMP   AL,yes_char
	 JNE   RE_BUILD_SYSTEM_AREA_ASK

DONT_ASK:
	 LEA   DX,LINE_DOWN
	 MOV   AH,9
	 DOSEXEC

	 MOV   SI,IO_AREA_PTR
	 ADD   SI,R_CTL_OFFSET
	 LEA   BX,word ptr ds:[si].xlat_table
	 MOV   BP,word ptr ds:[si].list_count
	 MOV   BX_SAVE,BX
	 mov   ax,num_boot_fat_sectors
         mov   dont_fix_cnt,ax
	 JMP   short RE_BUILD_REPEAT

RE_BUILD_ESCAPE:
	 MOV   EXIT_ERROR,2
;	 2 = USER CANCELLED
	 LEA   DX,LINE_DOWN
	 MOV   AH,9
	 DOSEXEC

RE_BUILD_CANCELLED:
	 MOV   DS,CS_SAVE
	 LEA   DX,USER_CANCELLED
	 MOV   AH,9
	 DOSEXEC

	 STC
	 RET

RE_BUILD_REPEAT:
	 MOV   BX,BX_SAVE

	 MOV   AX,word ptr ds:[bx].to_lsn_lo
	 MOV   ABS_25_SEC_LO,AX
	 MOV   AX,word ptr ds:[bx].to_lsn_hi
	 MOV   ABS_25_SEC_HI,AX

	 MOV   CX,1
	 LDS   BX,OK_DWORD
	 CALL  ABS_25
	 JC    RE_BUILD_CANCELLED

	xor	ax,ax			;We'll load AX in a moment, anyway.
	cmp	ax,dont_fix_cnt 	;Zero?
	JNE	re_build_dont_fix	;Not yet.
	mov	ds:[bx+21], al	;Fix root entry.
re_build_dont_fix:

	 MOV   BX,BX_SAVE
	 MOV   AX,word ptr ds:[bx].from_lsn_lo
	 MOV   ABS_25_SEC_LO,AX
	 MOV   AX,word ptr ds:[bx].from_lsn_hi
	 MOV   ABS_25_SEC_HI,AX

	 MOV   CX,1
	 LDS   BX,OK_DWORD
	 TEST  SWITCH,JUST_CHECK
	 JZ    RE_BUILD_WRITE
	 ADD   BX,SECTOR_SIZE
	 CALL  ABS_25
	 JC    RE_BUILD_CANCELLED

	 JMP   short RE_BUILD_INC

RE_BUILD_WRITE:
	 CALL  ABS_26
	 JC    RE_BUILD_CANCELLED

RE_BUILD_INC:
	 TEST  SWITCH,JUST_CHECK
	 JZ    RE_BUILD_INC_1

	xor	ax,ax			;We'll load AX in a moment, anyway.
	cmp	ax,dont_fix_cnt 	;Zero?
	JNE	re_build_dont_fix_2      ;Not yet.
	mov	ds:[bx+21], al	;Fix root entry.
re_build_dont_fix_2:

	 CALL  COMPARE_AREAS

RE_BUILD_INC_1:
	sub	dont_fix_cnt,1
	adc	dont_fix_cnt,0	;Keep it from going negative.
	 MOV   BX,BX_SAVE
	 ADD   BX,8
	 MOV   BX_SAVE,BX
	 DEC   BP
	jz	re_build_inc_end
	 jmp   RE_BUILD_REPEAT
re_build_inc_end:
;	 Now we gotta write the rest of the fats if any

	 DEC   R_NUM_FATS
	 JZ    RE_BUILD_THRU
	 MOV   BP,NUM_BOOT_FAT_SECTORS
	 SUB   BP,R_NUM_RESERVED	 ; # sectors per FAT
	 MOV   AX,R_NUM_RESERVED
	 MOV   CL,3
	 SHL   AX,CL
	 LEA   BX,word ptr ds:[si].xlat_table
	 ADD   BX,AX		       ; point to 1st FAT entry
	 MOV   BX_SAVE,BX
	 MOV   AX,NUM_BOOT_FAT_SECTORS ; logical sector # next sector

RE_BUILD_OTHER_FATS:
	 CALL  WRITE_OTHER_FAT_SECTOR
	 JC    RE_BUILD_OTHER_BAD
	 ADD   BX,8
	 INC   AX
	 DEC   BP
	 JNZ   RE_BUILD_OTHER_FATS

	 DEC   R_NUM_FATS
	 JZ    RE_BUILD_THRU

	 MOV   BP,NUM_BOOT_FAT_SECTORS
	 SUB   BP,R_NUM_RESERVED	 ; # sectors per FAT
	 MOV   BX,BX_SAVE
	 JMP   RE_BUILD_OTHER_FATS

RE_BUILD_THRU:
	 CLC
	 RET

RE_BUILD_OTHER_BAD:
	 STC
	 RET

RE_BUILD_SYSTEM_AREA ENDP



INSERT_DATE PROC NEAR
	 CMP   TIME_FROM_MIRROR_1,0FFFFH
	 JNE   INSERT_DATE_CONT
	 RET

INSERT_DATE_CONT:
	 MOV   AX,TIME_FROM_MIRROR_1
	 MOV   AH,0
	 CALL  HEX_TO_ASCII
	 MOV   WORD PTR TIME_MSG+3,AX

	 MOV   AX,TIME_FROM_MIRROR_1
	 MOV   AL,AH
	 MOV   AH,0
	 CALL  HEX_TO_ASCII
	 MOV   WORD PTR TIME_MSG,AX

	 MOV   AX,YEAR_FROM_MIRROR_1
	 SUB   AX,1900

	cmp	ax,100		;M006	; anything over 2000 should map back
	jb	got_date_lt_100	;M006
	sub	ax,100		;M006	; 2000 -> 00
got_date_lt_100:		;M006

	 CALL  HEX_TO_ASCII
	 MOV   WORD PTR MODAY_MSG+6,AX

	 MOV   AX,MODAY_FROM_MIRROR_1
	 MOV   AH,0
	 CALL  HEX_TO_ASCII
	 MOV   WORD PTR MODAY_MSG+3,AX

	 MOV   AX,MODAY_FROM_MIRROR_1
	 MOV   AL,AH
	 MOV   AH,0
	 CALL  HEX_TO_ASCII
	 MOV   WORD PTR MODAY_MSG,AX

	call	fix_date_foreign	;5-25-89 GWD.

	 LEA   DX,LAST_MSG
	 MOV   AH,9
	 DOSEXEC
	 LEA   DX,DATE_MSG
	 MOV   AH,9
	 DOSEXEC

	 TEST  SWITCH,GOT_TWO_FILES
	 JNZ   INSERT_DATE_CONT_1
	 RET

INSERT_DATE_CONT_1:
	 MOV   AX,TIME_FROM_MIRROR_2
	 MOV   AH,0
	 CALL  HEX_TO_ASCII
	 MOV   WORD PTR TIME_MSG+3,AX

	 MOV   AX,TIME_FROM_MIRROR_2
	 MOV   AL,AH
	 MOV   AH,0
	 CALL  HEX_TO_ASCII
	 MOV   WORD PTR TIME_MSG,AX

	 MOV   AX,YEAR_FROM_MIRROR_2
	 SUB   AX,1900

	cmp	ax,100		;M006	; anything over 2000 should map back
	jb	got_date_lt_100a ;M006
	sub	ax,100		;M006	; 2000 -> 00
got_date_lt_100a:		;M006

	 CALL  HEX_TO_ASCII
	 MOV   WORD PTR MODAY_MSG+6,AX

	 MOV   AX,MODAY_FROM_MIRROR_2
	 MOV   AH,0
	 CALL  HEX_TO_ASCII
	 MOV   WORD PTR MODAY_MSG+3,AX

	 MOV   AX,MODAY_FROM_MIRROR_2
	 MOV   AL,AH
	 MOV   AH,0
	 CALL  HEX_TO_ASCII
	 MOV   WORD PTR MODAY_MSG,AX

	call	fix_date_foreign	;5-25-89 GWD.

	 LEA   DX,PRIOR_MSG
	 MOV   AH,9
	 DOSEXEC
	 LEA   DX,DATE_MSG
	 MOV   AH,9
	 DOSEXEC
	 RET

INSERT_DATE ENDP

;
;-----------------------------------------------------------------
; Added 5-25-89, GWD.
;
; On entry: MODAY_MSG string is setup like this "MM/DD/YY".
;
; On exit: the string is adjusted for the current country-code.
;	   (country info was previously obtained by Glen's
;	    UF_Main module and is stored in Glen's UF_IO module.)
;
; AX is destroyed.
;
date_field1 EQU word ptr moday_msg
date_sep1   EQU byte ptr moday_msg+2
date_field2 EQU word ptr moday_msg+3
date_sep2   EQU byte ptr moday_msg+5
date_field3 EQU word ptr moday_msg+6
;
fix_date_foreign PROC NEAR
	mov	al,date_separator
	mov	date_sep1,al
	mov	date_sep2,al
	mov	ax,date_control ;Date format code, from DOS country-info.
	or	ax,ax
	jz	fix_d_end	;Date format = USA, so nothing to do.
	cmp	ax,2
	ja	fix_d_end	;Unknown format code.
	push	date_field3	;Year of initial US-format date.
	push	date_field1	;Month.
	push	date_field2	;Day.
	cmp	al,2		;Japan?
	je	fix_d_japan
	pop	date_field1	;Europe.
	pop	date_field2
	pop	date_field3
	jmp	short fix_d_end
fix_d_japan:
	pop	date_field3
	pop	date_field2
	pop	date_field1
fix_d_end:
	ret
fix_date_foreign ENDP



WRITE_OTHER_FAT_SECTOR PROC NEAR

;	 Upon entry AX has the logical sector # to write and
;	 BX points to the control record entry for the
;	 source of the data. With BX, we'll read the data.
;	 With AX, we'll write it.

	 PUSH  AX
	 PUSH  BX
	 MOV   AX_SAVE,AX

	 MOV   AX,word ptr ds:[bx].to_lsn_lo
	 MOV   ABS_25_SEC_LO,AX
	 MOV   AX,word ptr ds:[bx].to_lsn_hi
	 MOV   ABS_25_SEC_HI,AX

	 MOV   CX,1
	 LDS   BX,OK_DWORD
	 CALL  ABS_25
	 JC    WRITE_OTHER_STC

	 MOV   AX,AX_SAVE
	 MOV   ABS_25_SEC_LO,AX
	 MOV   ABS_25_SEC_HI,0

	 MOV   CX,1
	 LDS   BX,OK_DWORD
	 TEST  SWITCH,JUST_CHECK
	 JZ    W_O_F_S_WRITE

	 ADD   BX,SECTOR_SIZE
	 CALL  ABS_25
	 JC    WRITE_OTHER_STC
	 JMP   short W_O_F_S_WRITE_OK

W_O_F_S_WRITE:
	 CALL  ABS_26
	 JC    WRITE_OTHER_STC

W_O_F_S_WRITE_OK:
	 TEST  SWITCH,JUST_CHECK
	 JZ    W_O_F_S_WRITE_OK_1
	 CALL  COMPARE_AREAS

W_O_F_S_WRITE_OK_1:
	 POP   BX
	 POP   AX
	 CLC
	 RET

WRITE_OTHER_STC:
	 POP   BX
	 POP   AX
	 STC
	 RET

WRITE_OTHER_FAT_SECTOR ENDP


;
; Note: Glen's main module (UF_MAIN) has already validate
; the DOS version and checked for a network drive.
; So that code has been deleted.  5-2-89 GWD.
;
CHECK_OBVIOUS_STUFF PROC NEAR
	MOV	AX,3306H		; get true DOS version
	MOV	BX,0			;   ala DOS 5.0
	INT	21H
	CMP	BL,5			; function supported?
	JB	VERSION_OLD_WAY
	MOV	AX,BX
	JMP	SHORT STORE_DOSVER

VERSION_OLD_WAY:
	 MOV   AH,30H
	 DOSEXEC

STORE_DOSVER:
	 XCHG  AH,AL
	cmp	ax,(3*256)+30	;Zenith 3.30 DOS?
	jne	store_version	;No way.
	cmp	bh,5		;Zenith ID?   (GWD 11-18-88)
	jne	store_version
	mov	al,31		;Pretend it's 3.31 DOS (supports huge partns).

store_version:
	 MOV   DOS_VERSION,AX
	 CLC
	 RET

CHECK_OBVIOUS_STUFF ENDP



FIND_OUR_FILE PROC NEAR
	 TEST  SWITCH,INIT_F_O_F
	 JZ    F_O_F_FIRST_TIME
	 TEST  SWITCH,HARD_WAY
	 JZ    F_O_M_F_NEXT
	 ADD   HARD_WAY_SECTOR_NUM_LO,1
	 ADC   HARD_WAY_SECTOR_NUM_HI,0
	 JMP   NO_MIRROR_ASK_EM

F_O_F_FIRST_TIME:
	 OR    SWITCH,INIT_F_O_F
	 CALL  GET_DRIVE_SPECS
	 MOV   DS,CS_SAVE
	 JNC   F_O_F_FIND_A_MIRROR_FILE
	 JMP   F_O_F_STC

F_O_F_FIND_A_MIRROR_FILE:

; We'll look thru the last ten clusters for our wittle file. If we
;  find it we'll use the logical sector in it to begin our lookin'.
;  If we don't find it, we'll ask 'em if they want us to start from
;  the front.

	 MOV   CHECKS_FOR_OUR_MIRROR_FILE,0
	 MOV   AX,HIGHEST_CLUSTER
	 MOV   MIRROR_HI_CLU,AX

CHECK_FOR_MIRROR_LOOP:
	 MOV   AX,MIRROR_HI_CLU
	 CALL  CONVERT_CLU_TO_LOG_SECTOR
	 MOV   BX,IO_AREA_PTR
	 MOV   CX,1
	 CALL  ABS_25
	 JC    F_O_M_F_BYPASS

	 CALL  MATCH_OUR_MIRROR_FILE	; izit?
	 JC    F_O_M_F_NEXT		; not that wun
	 JMP   short FOUND_OUR_MIRROR_FILE

F_O_M_F_NEXT:
	 INC   CHECKS_FOR_OUR_MIRROR_FILE
	 MOV   AX,MAX_MIRROR_CLUSTERS
	 CMP   CHECKS_FOR_OUR_MIRROR_FILE,AX
	 JAE   NO_MIRROR_ASK_EM

	 DEC   MIRROR_HI_CLU
	 CMP   MIRROR_HI_CLU,1
	 JBE   NO_MIRROR_ASK_EM

	mov	ah,0Bh			;Check for Ctrl-C (allow user abort).
	int	21h			;Added 5-30-89	GWD.
	cmp	al,0
	jz	f_o_m_f_nokey
	mov	ax,0C00h		;Flush keyboard buffer.
	int	21h
f_o_m_f_nokey:

	 JMP   CHECK_FOR_MIRROR_LOOP
FOUND_OUR_MIRROR_FILE:
	 MOV   AX,SEC_LO_FROM_MIRROR_1	; where its at man
	 MOV   ABS_25_SEC_LO,AX
	 MOV   AX,SEC_HI_FROM_MIRROR_1	; where its at man
	 MOV   ABS_25_SEC_HI,AX

	 MOV   CX,1
	 CALL  ABS_25
	 JC    F_O_M_F_BYPASS

	 CALL  MATCH_REC
	 JNC   FOUND_OUR_FILE_CHK_PRIOR
	 JMP   F_O_M_F_NEXT

F_O_M_F_BYPASS:
	 PUSH  DX
	 LEA   DX,BAD_SECTOR_MSG
	 MOV   AH,9
	 DOSEXEC
	 POP   DX
	 JMP   F_O_M_F_NEXT

FOUND_OUR_FILE_CHK_PRIOR:
	 TEST  SWITCH,GOT_TWO_FILES
	 JZ    FOUND_OUR_FILE_JMP
	 AND   SWITCH,NOT GOT_TWO_FILES
	 MOV   AX,SEC_LO_FROM_MIRROR_2
	 MOV   ABS_25_SEC_LO,AX
	 MOV   AX,SEC_HI_FROM_MIRROR_2
	 MOV   ABS_25_SEC_HI,AX

	 MOV   CX,1
	 CALL  ABS_25
	 JC    FOUND_OUR_FILE_JMP

	 CALL  MATCH_REC
	 JC    FOUND_OUR_FILE_JMP
	 OR    SWITCH,GOT_TWO_FILES

FOUND_OUR_FILE_JMP:
	 JMP   FOUND_OUR_FILE

NO_MIRROR_ASK_EM:
	 OR    SWITCH,HARD_WAY
	 AND   SWITCH,NOT GOT_TWO_FILES

	 CMP   HARD_WAY_SECTOR_NUM_HI,0
	 JNE   FIND_OUR_FILE_READ

	 MOV   DX,HARD_WAY_SECTOR_NUM_LO
	 CMP   DX,FIRST_DATA_SECTOR
	 JNE   FIND_OUR_FILE_READ

	 MOV   DS,CS_SAVE
	 LEA   DX,CANT_FIND_MIRROR_MSG
	 MOV   AH,9
	 DOSEXEC

         public no_mirror_ask_em_again
NO_MIRROR_ASK_EM_AGAIN:
	 MOV   AX,0C01H
	 DOSEXEC
	 CMP   AL,K_ESCAPE
	 JE    F_O_F_ESC
	 AND   AL,NOT 20H
	 CMP   AL,yes_char
	 JE    FIND_OUR_FILE_SEARCH
	 CMP   AL,no_char
	 JNE   NO_MIRROR_ASK_EM_AGAIN

F_O_F_ESC:
	 LEA   DX,LINE_DOWN
	 MOV   AH,9
	 DOSEXEC
	 MOV   EXIT_ERROR,2
;	 2 = USER CANCELLED
	 JMP   short F_O_F_STC

FIND_OUR_FILE_SEARCH:
	 LEA   DX,LINE_DOWN
	 MOV   AH,9
	 DOSEXEC

FIND_OUR_FILE_READ:
	 MOV   AX,HARD_WAY_SECTOR_NUM_LO
	 MOV   ABS_25_SEC_LO,AX
	 MOV   AX,HARD_WAY_SECTOR_NUM_HI
	 MOV   ABS_25_SEC_HI,AX
	 MOV   CX,1
	 CALL  ABS_25
	 JC    F_O_F_BYPASS

	 CALL  MATCH_REC
	 JNC   FOUND_OUR_FILE_HARD
	 JMP   short F_O_F_INC

F_O_F_BYPASS:
	 PUSH  DX
	 LEA   DX,BAD_SECTOR_MSG
	 MOV   AH,9
	 DOSEXEC
	 POP   DX

F_O_F_INC:
	 ADD   HARD_WAY_SECTOR_NUM_LO,1
	 ADC   HARD_WAY_SECTOR_NUM_HI,0
	 MOV   DX,HARD_WAY_SECTOR_NUM_HI
	 CMP   DX,LAST_SECTOR_HI
	 JB    FIND_OUR_FILE_READ
	 MOV   DX,HARD_WAY_SECTOR_NUM_LO
	 CMP   DX,LAST_SECTOR_LO
	 JB    FIND_OUR_FILE_READ
	 MOV   EXIT_ERROR,3
;	 3 = DID NOT RUN SUCCESSFULLY. DID NOT MODIFY

F_O_F_STC:
	 STC
	 RET

FOUND_OUR_FILE_HARD:
	 MOV   AX,HARD_WAY_SECTOR_NUM_LO
	 MOV   SEC_LO_FROM_MIRROR_1,AX
	 MOV   ABS_SECTOR_LO,AX

	 MOV   AX,HARD_WAY_SECTOR_NUM_HI
	 MOV   SEC_HI_FROM_MIRROR_1,AX
	 MOV   ABS_SECTOR_HI,AX

	 CALL  DISPLAY_SECTOR_NUM

	 MOV   AX,WORD PTR H_E_SECTOR_NUM
	 MOV   WORD PTR HARD_WAY_NUM,AX
	 MOV   AX,WORD PTR H_E_SECTOR_NUM+2
	 MOV   WORD PTR HARD_WAY_NUM+2,AX
	 MOV   AX,WORD PTR H_E_SECTOR_NUM+4
	 MOV   WORD PTR HARD_WAY_NUM+4,AX
	 MOV   AL,H_E_SECTOR_NUM+6
	 MOV   HARD_WAY_NUM+6,AL

FOUND_OUR_FILE_HARD_AGAIN:
	 LEA   DX,HARD_WAY_MSG
	 MOV   AH,9
	 DOSEXEC

	 MOV   DI,IO_AREA_PTR
	 ADD   DI,BACKUP_IND_OFFSET
	 CMP   BYTE PTR [DI],0FFH
	 JNE   FOUND_OUR_FILE_MAYBE
	 LEA   DX,HARD_WAY_BACKUP_MSG
	 MOV   AH,9
	 DOSEXEC

FOUND_OUR_FILE_MAYBE:
	 LEA   DX,HARD_WAY_ASK_MSG
	 MOV   AH,9
	 DOSEXEC
	 MOV   AX,0C01H
	 DOSEXEC
	 CMP   AL,K_ESCAPE
	 JE    FOUND_HARD_ESC
	 AND   AL,NOT 20H
	 CMP   AL,yes_char
	 JE    FOUND_OUR_FILE_CLC
	 CMP   AL,no_char
	 JNE   FOUND_OUR_FILE_HARD_AGAIN
	 LEA   DX,LINE_DOWN
	 MOV   AH,9
	 DOSEXEC
	 JMP   F_O_F_INC

FOUND_HARD_ESC:
	 OR    SWITCH,ESCAPE_PRESSED
	 STC
	 RET

FOUND_OUR_FILE_CLC:
	 LEA   DX,LINE_DOWN
	 MOV   AH,9
	 DOSEXEC

FOUND_OUR_FILE:
	 CLC
	 RET

FIND_OUR_FILE ENDP



CONVERT_CLU_TO_LOG_SECTOR PROC NEAR
	 PUSH  BX
	 SUB   AX,2			; minus tew
	 MOV   BX,SECTORS_PER_CLU
	 MUL   BX			; times sectors per clu
	 ADD   AX,FIRST_DATA_SECTOR	; that shud be about it
	 ADC   DX,0
	 MOV   ABS_25_SEC_LO,AX
	 MOV   ABS_25_SEC_HI,DX
	 POP   BX
	 RET

CONVERT_CLU_TO_LOG_SECTOR ENDP



MATCH_REC PROC NEAR
	 MOV   SI,IO_AREA_PTR
	 LEA   DI,ASCIIZ
	 MOV   CX,14
	 TEST  SWITCH,DRIVE_FIXED
	 JNZ   MATCH_NO_INC
	 INC   SI
	 INC   DI
	 DEC   CX

MATCH_NO_INC:
	 REPE  CMPSB
	 JNE   MATCH_REC_STC

MATCH_REC_CLC:
	 CLC
	 RET

MATCH_REC_STC:
	 STC
	 RET
MATCH_REC ENDP



MATCH_OUR_MIRROR_FILE PROC NEAR
	 AND   SWITCH,NOT GOT_TWO_FILES
	 MOV   TIME_FROM_MIRROR_1,0FFFFH
	 MOV   SI,IO_AREA_PTR
	 MOV   AX,WORD PTR [SI] 	; in case this is it,
	 MOV   SEC_LO_FROM_MIRROR_1,AX	;  then save it
	 MOV   AX,WORD PTR [SI+2]	; in case this is it,
	 MOV   SEC_HI_FROM_MIRROR_1,AX	;  then save it
	 ADD   SI,4			; where it oughta be
	 LEA   DI,OUR_SIGNATURE
	 MOV   CX,OUR_SIGNATURE_LEN
	 REPE  CMPSB
	 JNE   M_O_M_F_STC

	 MOV   SI,IO_AREA_PTR
	 MOV   AX,WORD PTR MIRROR_TIME_1[SI]
	 MOV   TIME_FROM_MIRROR_1,AX
	 MOV   AX,WORD PTR MIRROR_YEAR_1[SI]
	 MOV   YEAR_FROM_MIRROR_1,AX
	 MOV   AX,WORD PTR MIRROR_MODAY_1[SI]
	 MOV   MODAY_FROM_MIRROR_1,AX

	 CMP   WORD PTR MIRROR_LSN_HI_2[SI],0
	 JNE   M_O_M_F_CHK
	 CMP   WORD PTR MIRROR_LSN_LO_2[SI],2
	 JB    M_O_M_F_CLC

M_O_M_F_CHK:
	 MOV   AX,HIGHEST_CLUSTER
	 CALL  CONVERT_CLU_TO_LOG_SECTOR
	 MOV   AX,ABS_25_SEC_HI
	 CMP   WORD PTR MIRROR_LSN_HI_2[SI],AX
	 JB    M_O_M_F_GET_2ND
	 MOV   AX,ABS_25_SEC_LO
	 CMP   WORD PTR MIRROR_LSN_LO_2[SI],AX
	 JA    M_O_M_F_CLC

M_O_M_F_GET_2ND:
	 OR    SWITCH,GOT_TWO_FILES
	 MOV   AX,WORD PTR MIRROR_LSN_LO_2[SI]
	 MOV   SEC_LO_FROM_MIRROR_2,AX
	 MOV   AX,WORD PTR MIRROR_LSN_HI_2[SI]
	 MOV   SEC_HI_FROM_MIRROR_2,AX
	 MOV   AX,WORD PTR MIRROR_TIME_2[SI]
	 MOV   TIME_FROM_MIRROR_2,AX
	 MOV   AX,WORD PTR MIRROR_YEAR_2[SI]
	 MOV   YEAR_FROM_MIRROR_2,AX
	 MOV   AX,WORD PTR MIRROR_MODAY_2[SI]
	 MOV   MODAY_FROM_MIRROR_2,AX

M_O_M_F_CLC:
	 CLC
	 RET

M_O_M_F_STC:
	 STC
	 RET

MATCH_OUR_MIRROR_FILE ENDP



GET_DRIVE_SPECS PROC NEAR
	push	si
	 MOV   MIRROR_SWITCH,0
	 MOV   AH,32H
	 MOV   DL,ASCIIZ
	 SUB   DL,40H
	 DOSEXEC
	 CMP   AL,0FFH
	 JNE   GET_DRIVE_1
	 JMP   SPEC_ERROR

GET_DRIVE_1:
	mov	si,1			;If DOS=4.xx then SI=1, else SI=0.
	cmp	dos_version,400h	;CF=true if ver# is < 400h.
	sbb	si,0			;Decrements only if DOS < 4.00.
	 CMP   BYTE PTR [BX+si]+22,0F8H   ; is it a fixed disk?
	 JE    GET_DRIVE_FIXED
	 CMP   DOS_VERSION,0300H
	 JB    GET_DRIVE_2

	 PUSH  BX
	 MOV   AX,4408H
	 MOV   BL,ASCIIZ
	 SUB   BL,40H
	 DOSEXEC
	 POP   BX
	 JC    GET_DRIVE_2
	 CMP   AL,1
	 JNE   GET_DRIVE_2

GET_DRIVE_FIXED:
	 OR    SWITCH,DRIVE_FIXED

GET_DRIVE_2:
	 MOV   CX,[BX]+11	       ; get first usable sector
	 MOV   FIRST_DATA_SECTOR,CX
	 MOV   HARD_WAY_SECTOR_NUM_LO,CX
	 MOV   HARD_WAY_SECTOR_NUM_HI,0

	 SUB   CX,[BX+si]+16	       ; minus first directory sector
	 MOV   NUM_DIR_SECTORS,CX

	 MOV   ax,[BX]+15	       ; get sectors per FAT
	or	si,si
	jnz	get_drive_2x		;DOS 4.xx
	mov	ah,0			;For DOS < 4.00, it's only a byte.
get_drive_2x:

	 ADD   AX,[BX]+6	       ; dont forget reserved (BOOT)
	 MOV   NUM_BOOT_FAT_SECTORS,AX

	 ADD   CX,AX
	 MOV   NUM_SECTORS_TO_HANDLE,CX

	 MOV   AX,[BX]+13	       ; get cluster count + 1
	 MOV   HIGHEST_CLUSTER,AX      ; save it

 	 PUSH  AX
 	 SHR   AX,1
 	 SHR   AX,1
 	 MOV   MAX_MIRROR_CLUSTERS,AX
 	 POP   AX
;        MOV   MAX_MIRROR_CLUSTERS,AX
;	 DEC   MAX_MIRROR_CLUSTERS

	 DEC   AX
	 MOV   CH,0
	 MOV   CL,[BX]+4	       ; get sectors per cluster - 1
	 INC   CX
	 MUL   CX		       ; # of sectors in data area
	 ADD   AX,[BX]+11	       ; add system sectors
	 ADC   DX,0
	 MOV   LAST_SECTOR_LO,AX
	 MOV   LAST_SECTOR_HI,DX

	 MOV   AX,[BX]+2	       ; get sector size
	 MOV   SECTOR_SIZE,AX

	 MOV   AL,[BX]+4	       ; get sectors per cluster
	 INC   AL
	 MOV   AH,0
	 MOV   SECTORS_PER_CLU,AX

	 MOV   AX,[BX+si]+16	       ; get 1st directory sector
	 MOV   FIRST_DIRECTORY_SECTOR,AX

	 CMP   DOS_VERSION,031FH	;Version 3.31 DOS?
	 JB    GET_DRIVE_THRU

	 MOV   AX,SECTORS_PER_CLU
	 MUL   HIGHEST_CLUSTER
	 ADD   AX,FIRST_DATA_SECTOR
	 ADC   DX,0
	 OR    DX,DX
	 JE    GET_DRIVE_THRU
	 OR    MIRROR_SWITCH,ABS_25_HARD_WAY

GET_DRIVE_THRU:

	 CLC
	 jmp	short get_drive_exit

SPEC_ERROR:
	 LEA   DX,DRIVE_MSG
	 MOV   AH,9
	 DOSEXEC

	 LEA   DX,DRIVE_SPEC_ERROR
	 MOV   AH,9
	 DOSEXEC

	 MOV   EXIT_ERROR,5
;	 5 = STRANGE ENVIRONMENT OR DISK
	 STC
get_drive_exit:
	pop	si
	 RET

GET_DRIVE_SPECS ENDP



ABS_25	 PROC NEAR
	 PUSH  BX
	 PUSH  CX
	 PUSH  DX
	 PUSH  SI
	 PUSH  DI
	 PUSH  BP
	 PUSH  DS

	 MOV   ABS_25_OFF,BX
	 MOV   ABS_25_SEG,DS
	 MOV   ABS_25_COUNT,CX

	 MOV   DX,ABS_25_SEC_HI
	 MOV   ABS_SECTOR_HI,DX
	 MOV   DX,ABS_25_SEC_LO
	 MOV   ABS_SECTOR_LO,DX

	 MOV   AL,ASCIIZ
	 SUB   AL,41H

	 TEST  MIRROR_SWITCH,ABS_25_HARD_WAY
	 JZ    ABS_25_READ
	 MOV   CX,-1
	 MOV   DS,CS_SAVE
	 LEA   BX,ABS_25_LIST

ABS_25_READ:
	 INT   25H
	 JC    ABS_25_ERROR
	 POPF
	 CLC

ABS_25_EXIT:
	 POP   DS
	 POP   BP
	 POP   DI
	 POP   SI
	 POP   DX
	 POP   CX
	 POP   BX
	 RET

ABS_25_ERROR:
	 POPF
	 TEST  SWITCH,REPORT_ERRORS
	 JNZ   ABS_25_HANDLE
	 STC
	 JMP   ABS_25_EXIT

ABS_25_HANDLE:
	 MOV   AX,0
	 CALL  HANDLE_ERROR
	 JC    ABS_25_EXIT
	or	ax,ax
;	 CMP   AX,0
	 JE    ABS_25_RETRY
	 CLC
	 JMP   ABS_25_EXIT

ABS_25_RETRY:
	 POP   BP
	 POP   DI
	 POP   SI
	 POP   DX
	 POP   CX
	 POP   BX
	 JMP   ABS_25

ABS_25	 ENDP



ABS_26	 PROC NEAR
	 PUSH  BX
	 PUSH  CX
	 PUSH  DX
	 PUSH  SI
	 PUSH  DI
	 PUSH  BP
	 PUSH  DS

	 MOV   ABS_25_OFF,BX
	 MOV   ABS_25_SEG,DS
	 MOV   ABS_25_COUNT,CX

	 MOV   DX,ABS_25_SEC_HI
	 MOV   ABS_SECTOR_HI,DX
	 MOV   DX,ABS_25_SEC_LO
	 MOV   ABS_SECTOR_LO,DX

	 MOV   AL,ASCIIZ
	 SUB   AL,41H

	 TEST  MIRROR_SWITCH,ABS_25_HARD_WAY
	 JZ    ABS_26_WRITE
	 MOV   CX,-1
	 MOV   DS,CS_SAVE
	 LEA   BX,ABS_25_LIST

ABS_26_WRITE:
	 INT   26H
	 JC    ABS_26_ERROR
	 POPF
	 CLC

ABS_26_EXIT:
	 POP   DS
	 POP   BP
	 POP   DI
	 POP   SI
	 POP   DX
	 POP   CX
	 POP   BX
	 RET

ABS_26_ERROR:
	 POPF
	 TEST  SWITCH,REPORT_ERRORS
	 JNZ   ABS_26_HANDLE
	 STC
	 JMP   ABS_26_EXIT

ABS_26_HANDLE:
	 MOV   AX,1
	 CALL  HANDLE_ERROR
	 JC    ABS_26_EXIT
	or	ax,ax
;	 CMP   AX,0
	 JE    ABS_26_RETRY
	 CLC
	 JMP   ABS_26_EXIT

ABS_26_RETRY:
	 POP   BP
	 POP   DI
	 POP   SI
	 POP   DX
	 POP   CX
	 POP   BX
	 JMP   ABS_26
ABS_26	 ENDP



HANDLE_ERROR PROC NEAR
	 PUSH  DS
	 MOV   DS,CS_SAVE
	 LEA   DX,ERROR_READ
	 CMP   AX,0
	 JE    H_E_1
	 LEA   DX,ERROR_WRITE

H_E_1:
	 MOV   AH,9
	 DOSEXEC
	 CALL  DISPLAY_SECTOR_NUM
	 LEA   DX,H_E_SECTOR_NUM
	 MOV   AH,9
	 DOSEXEC

H_E_AGAIN:
	 LEA   DX,H_E_QUESTION
	 MOV   AH,9
	 DOSEXEC
	 MOV   AX,0C01H
	 DOSEXEC
	 CMP   AL,K_ESCAPE
	 JE    H_E_ABORT
	 AND   AL,NOT 20H
	 CMP   AL,abort_char	;'A'
	 JE    H_E_ABORT
	 CMP   AL,retry_char	;'R'
	 JE    H_E_RETRY
	 CMP   AL,ignore_char	;'I'
	 JNE   H_E_AGAIN
	 MOV   AX,1
	 CLC
	 JMP   short H_E_EXIT

H_E_RETRY:
	xor	ax,ax
;	 MOV   AX,0
;	 CLC
	 JMP   short H_E_EXIT

H_E_ABORT:
	 MOV   EXIT_ERROR,4
;	 4 = DID NOT RUN SUCCESSFULLY. DID MODIFY
	 STC

H_E_EXIT:
	 PUSHF
	 PUSH  AX
	 LEA   DX,LINE_DOWN
	 MOV   AH,9
	 DOSEXEC
	 POP   AX
	 POPF
	 POP   DS
	 RET

HANDLE_ERROR ENDP



COMPARE_AREAS PROC NEAR
	 PUSH  ES
	 PUSH  DI
	 PUSH  DS
	 PUSH  SI
	 PUSH  CX
	 PUSH  AX
	 LES   DI,OK_DWORD
	 LDS   SI,OK_DWORD
	 ADD   DI,SECTOR_SIZE
	 MOV   CX,SECTOR_SIZE

COMPARE_AREAS_AGAIN:
	 REPE  CMPSB
	 JE    COMPARE_AREAS_EXIT

	 MOV   AX,CX
	 AND   AX,1FH
	 CMP   AX,9
	 JA    COMPARE_AREAS_FAILED
	 CMP   AX,4
	 JB    COMPARE_AREAS_FAILED
	 ADD   SI,AX
	 ADD   DI,AX
	 SUB   CX,AX
	 CALL  CHECK_OF_OK_TO_FAIL
	 JNC   COMPARE_AREAS_AGAIN

COMPARE_AREAS_FAILED:
	 INC   CHECK_FAILED

COMPARE_AREAS_EXIT:
	 POP   AX
	 POP   CX
	 POP   SI
	 POP   DS
	 POP   DI
	 POP   ES
	 RET
COMPARE_AREAS ENDP



CHECK_OF_OK_TO_FAIL PROC NEAR
	 PUSH  SI
	 PUSH  DI
	 PUSH  ES
	 PUSH  CX

	 SUB   SI,32
	 LEA   AX,NAMES
	 MOV   ES,CS_SAVE

C_O_O_T_F_COMPARE:
	 PUSH  SI
	 MOV   DI,AX
	 MOV   CX,11
	 REPE  CMPSB
	 POP   SI
	 JE    C_O_O_T_F_CLC
	 ADD   AX,11
	 CMP   AX,OFFSET NAMES+22
	 JNA   C_O_O_T_F_COMPARE
	 STC
	 JMP   short C_O_O_T_F_EXIT

C_O_O_T_F_CLC:
	 CLC

C_O_O_T_F_EXIT:
	 POP   CX
	 POP   ES
	 POP   DI
	 POP   SI
	 RET

CHECK_OF_OK_TO_FAIL ENDP



HEX_TO_ASCII PROC NEAR
	 PUSH  DX
	 PUSH  BX
	 xor	dx,dx
	 MOV   BX,10
	 DIV   BX
	 MOV   AH,DL
	 OR    AX,3030H
	 POP   BX
	 POP   DX
	 RET

HEX_TO_ASCII ENDP



DISPLAY_SECTOR_NUM PROC NEAR
	 PUSH  AX
	 PUSH  BX
	 PUSH  CX
	 PUSH  DX
	 PUSH  DI
	 PUSH  BP

	 MOV   CX,7
	 MOV   AX,ABS_SECTOR_LO
	 MOV   DX,ABS_SECTOR_HI
	 MOV   WORD PTR H_E_SECTOR_NUM,'  '
	 MOV   WORD PTR H_E_SECTOR_NUM+2,'  '
	 MOV   WORD PTR H_E_SECTOR_NUM+4,'  '

	 LEA   DI,H_E_SECTOR_NUM+6
	 MOV   BX,10000 	  ;DIVIDE BY 10000
	 DIV   BX		  ; BREAK INTO 2 PARTS
	 MOV   BP,AX		  ; SAVE HI PART
	 MOV   AX,DX		  ; WORK ON LOW PART FIRST
	xor	dx,dx

	 MOV   SI,CX
	 SUB   SI,4
	 MOV   CX,4

	 MOV   BX,10

HEX_TO_ASCIIZ_DIV:
	 DIV   BX		  ;PERFORM DIVISION
;	 AX = QUOTIENT
;	 DX = REMAINDER
	 ADD   DL,30H		  ;MAKE RELATIVE TO ASCII ZERO
	 MOV   BYTE PTR [DI],DL   ;MOVE VALUE TO OUTPUT AREA
	 DEC   DI		  ;DECREMENT OUTPUT POINTER
	 xor   dx,dx		  ;ADJUST AX BACK TO 32 BIT NUMBER
	 or	ax,ax
	 jnz   HEX_TO_ASCIIZ_CONT
	or	bp,bp
	 jz    HEX_TO_ASCIIZ_EXIT

HEX_TO_ASCIIZ_CONT:
	 LOOP  HEX_TO_ASCIIZ_DIV

	 MOV   AX,BP
	xor	dx,dx
	 MOV   CX,SI
	xor	si,si
	xor	bp,bp
	 JMP   HEX_TO_ASCIIZ_DIV

HEX_TO_ASCIIZ_EXIT:
	 POP   BP
	 POP   DI
	 POP   DX
	 POP   CX
	 POP   BX
	 POP   AX
	 RET

DISPLAY_SECTOR_NUM ENDP
;
;
OFFSET_COMP EQU $

	 DB    'x:\'
	 DB    'MIRROR.FIL',0
	 DB    9 DUP(0)

;BACKUP_LSEEK EQU $-OFFSET_COMP
BACKUP_IND_OFFSET EQU $-OFFSET_COMP

	 DB    0
	 DW    0
	 ORG   OFFSET_COMP+26

R_IO_INFO_OFFSET EQU $-OFFSET_COMP

	 DB    16 DUP(0)
	 ORG   OFFSET_COMP+64

R_CTL_OFFSET EQU $-OFFSET_COMP
;
prog	ENDS
	END
