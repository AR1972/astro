; Copyright (c) 1987-1991 Central Point Software, Inc.
; All rights reserved

         PAGE  61,131

         TITLE  PC Tools 6 - Delete Tracking


;  ***  With new Unload-option support.  Feb 1990, GWD.
;  ***  INT 19h & 2Fh hooks.
;
; 03-20-90 GWD  Added 3 checks to ignore attempted delete of READ_ONLY files.
; 03-20-90 GWD  Fixed up all four INT 21h exit paths, better handling of flags.
; 04-23-90 GWD  Fix CHK_DTRK_RESIDENT.  Skip check of segment=CS for res.
; 04-26-90 GWD  Fix CHK_DTRK_RESIDENT again.  Correctly check for last MCB.
; 04-27-90 GWD  Global proc UPPERCASE relocated to resident part.
; ??-??-90 ?	Various non-documented changes for MSoft.
; 12-05-90 GWD	Removed "include datetime.inc", and some unused extrn's.
; 12-18-90 MD   Cleaned up some short jumps
; 1/30/91  SR	M001 -- Check for /1 switch, bug #4999
; 2/1/91   SR	M003 -- Fix lh mirror, bug #4828
; 2/11/91  DLB  M006 -- Added explicit tests for ASSIGN'd, Net, SUBST'd and
;			JOIN'd drives during initialization.
; 2/21/91  DLB  M008 -- Changed DOS_VERSION to be EXTRN.
; 08-12-91 JAH  m012 -- Fixed problem with exiting with interrupts disabled.

        INCLUDE dossym.inc              ;M006
        INCLUDE syscall.inc             ;M006
	INCLUDE sysvar.inc		;M006
        INCLUDE curdir.inc              ;M006


DOSEXEC  MACRO
         INT   21H
         ENDM

DOSFAKE  MACRO
         PUSHF
         CALL  DOS_VECTOR
         ENDM

IVE_TABLE STRUC
IVE_NUMBER            DB ?
IVE_SECTOR_SIZE       DW ?
IVE_SECTORS_PER_CLU   DB ?
IVE_FIRST_FAT_SECTOR  DW ?
IVE_FIRST_ROOT_SECTOR DW ?
IVE_FIRST_DATA_SECTOR DW ?
IVE_SWITCH            DW ?
IVE_CTL_FILE_ENTRIES  DW ?
IVE_TABLE ENDS

FAT_IZ_16_BITS        EQU 8000H

CODE     SEGMENT PARA PUBLIC 'CODE'

         ASSUME CS:CODE
        PUBLIC  start
        PUBLIC  maybe_patch, hook_int_25_26
        PUBLIC  int25_patch, int26_patch, dos_intercept
        PUBLIC  chk_dtrk_resident, saved25, saved26, dos_vector
        PUBLIC  saved19, int19_service
        PUBLIC  saved2F, int2F_service          ;02-23-90 GWD.
        PUBLIC  UPPERCASE

        EXTRN   SAVE_PARTITION:NEAR
        EXTRN   $entry:near             	;M006
        EXTRN   org_id_length:abs  		;In _ORG the module.
        EXTRN   org_id2:byte, org_id2_length:abs
        EXTRN   end_prog:near           	;In MIR_LAST module.
        EXTRN   check_network_drive:near        ;M006
        EXTRN   DOS_VERSION:word                ;M008



DOS_VECTOR LABEL DWORD          ;The saved original INT 21h vector.
DOS_OFF  DW    0
DOS_SEG  DW    0

IN_DOS_DWORD LABEL DWORD
IN_DOS_OFFSET  DW 0
IN_DOS_SEGMENT DW 0

DOS_VERSION_BH DB 0

OLD_DTA_SEG DW    0
OLD_DTA_OFF DW    0

SWITCH                 DW   0
ACTIVE                 EQU  8000H
DELETE_OLD_WAY         EQU  4000H
DELETE_NEW_WAY         EQU  2000H
GIVE_IT_TO_DOS         EQU  1000H
KOULDNT_PROTECT        EQU  0800H
FORCED_TO_ROOT         EQU  0400H
EXTENDED_FCB           EQU  0200H
NO_CLUSTERS_ALLOCATED  EQU  0100H
DELETE_SUB_DIR         EQU  0080H
WE_FOUND_US            EQU  0040H

SAVE_FCB_DRIVE   DB 0

DRIVE_INFO    DB  0
ABOVE_32_MEG  EQU 80H

ABS25_PARM_BLOCK LABEL BYTE
ABS25_SECTOR_LOW     DW  0
ABS25_SECTOR_HIGH    DW  0
ABS25_SECTOR_COUNT   DW  0
ABS25_XFER_ADDR_OFF  DW  0
ABS25_XFER_ADDR_SEG  DW  0

OUR_CTL_ASCIIZ  DB  'x:\PCTRACKR.DEL',0
OUR_CTL_FCBNAME DB  'PCTRACKRDEL'
OUR_CTL_FCBNAME_LEN EQU $-OUR_CTL_FCBNAME

ASCIIZ      DB   73 DUP(0)
ASCIIZ_LEN  EQU  $-ASCIIZ

FIND_FIRST_DTA  DB  50 DUP (0)
         ORG   FIND_FIRST_DTA

;----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R        ;
;                                                                     ;
_FIND_FIRST_30_DRIVE   DB ?             ; drive of search
_FIND_FIRST_30_NAME    DB  11 DUP (?)   ; formatted name
_FIND_FIRST_30_SATTR   DB ?             ; attribute of search
FIND_FIRST_30_LASTENT  DW ?             ; LastEnt
FIND_FIRST_30_DIRSTART DW ?             ; DirStart
_FIND_FIRST_30_NETID   DB 4 DUP (?)     ; Reserved for NET
;         ORG   FIND_FIRST_DTA+13
;FIND_FIRST_20_LASTENT  DW ?             ; LastEnt
         ORG   FIND_FIRST_DTA+19
FIND_FIRST_20_DIRSTART DW ?             ; DirStart
;                                                                     ;
;            C  A  V  E  A  T     P  R  O  G  R  A  M  M  E  R        ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----;

FIND_FIRST_ATTRIBUTE DB  0
FIND_FIRST_TIME      DW  0
FIND_FIRST_DATE      DW  0
FIND_FIRST_SIZE_LOW  DW  0
FIND_FIRST_SIZE_HIGH DW  0
FIND_FIRST_FILE_EXT  DB  13 DUP (0)
         ORG   FIND_FIRST_DTA+50


SAVE_AX     DW   0
SAVE_BX     DW   0
SAVE_CX     DW   0
SAVE_DX     DW   0
SAVE_BP     DW   0
SAVE_SI     DW   0
SAVE_DI     DW   0
SAVE_DS     DW   0
SAVE_ES     DW   0
SAVE_FLAGS  DW   0
CARRY_FLAG  DW   0		; m012


TEMP_FLAGS DW 0

CS_SAVE    DW 0
TWO        DW 2
THIRTY_TWO DW 32

; directory attributes

READ_ONLY EQU  01H
HIDDEN    EQU  02H
SYSTEM    EQU  04H
SUB_DIR   EQU  10H

CTL_HEADER LABEL BYTE
CTL_HEADER_NUM_ENTRIES  DW 0
CTL_HEADER_OLDEST_ENTRY DW 0
CTL_HEADER_LEN EQU $-CTL_HEADER

CTL_FILE_HANDLE  DW 0
CTL_FILE_ASCIIZ_OFFSET DW CTL_FILE_ASCIIZ+3
CLUSTERS_STORED_END    DW 0

CTL_FILE_ENTRY LABEL BYTE
CTL_FILE_IND   DB 0
CTL_FIRST      EQU 80H
CTL_LAST       EQU 40H
CTL_EMPTY      EQU 20H
CTL_CLEAR      EQU NOT CTL_FIRST+CTL_LAST+CTL_EMPTY
CTL_SUB_DIR    EQU 10H
CTL_ATTR       DB 0
CTL_TIME       DW 0
CTL_DATE       DW 0
CTL_SIZE_LO    DW 0
CTL_SIZE_HI    DW 0
CTL_TIME_DEL   DW 0
CTL_DATE_DEL   DW 0
CTL_INFO_CONT EQU $
CTL_FILE_ASCIIZ DB 128 DUP (0)
CTL_INFO       DW 20 DUP(0)
CTL_FILE_END   EQU $
CTL_FILE_ENTRY_LEN EQU $-CTL_FILE_ENTRY


FAT_SECTOR       DW 0
FAT_AREA_PTR     DW 0
IO_AREA_PTR      DW DOS_INTERCEPT_END
LARGEST_SECTOR   DW 512

INSTALL_SWITCH   DB 0
GOT_SUMTHIN_TO_TRACK EQU 80H
WINDOWS_PRESENT      EQU 40H
ASSIGN_RESIDENT      EQU 20H
GOT_SLASH_TEE        EQU 10H
DO_IT_THE_SLOW_WAY   EQU 08H

DRIVES_GIVEN_TABLE DW 26 DUP(0FFFFH)
;        One word for each drive letter. If FFFF, ignore the drive.
;        If zero, use Delete Tracker file if found as iz. Otherwise,
;        ignore. If a value is specified, use this as the number of
;        entries for the file when creating.
;

DR       IVE_TABLE <>

DIR_ENT_ATTR EQU 11
DIR_ENT_TIME EQU 22
DIR_ENT_DATE EQU 24
DIR_ENT_CLU  EQU 26
DIR_ENT_SZLO EQU 28
DIR_ENT_SZHI EQU 30



DOS_INTERCEPT PROC FAR
         CLI

         PUSHF                          ;3-20-90 GWD.
         POP   TEMP_FLAGS               ;Saved flags have IF=0 (disabled).

         TEST  SWITCH,ACTIVE
         JNZ   DOS_INTERCEPT_BAILOUT

         CMP   AH,41H                   ; new style delete request?
         JE    NEW_WAY_TO_DELETE        ; yep
         CMP   AH,13H                   ; old style?
         JE    OLD_WAY_TO_DELETE        ; yep
         CMP   AH,3AH                   ; remove a sub-directory?
         JE    REMOVE_SUB_DIR           ; yep

DOS_INTERCEPT_BAILOUT:
         PUSH  TEMP_FLAGS
         POPF
         JMP   DWORD PTR DOS_VECTOR     ; on to DOS (or other trappeurs)

REMOVE_SUB_DIR:
         CALL  ENTERING_CRITICAL_SECTION

         MOV   SWITCH,DELETE_SUB_DIR+ACTIVE ; show how to dew it
         JMP   SHORT DEW_IT

OLD_WAY_TO_DELETE:
         CALL  ENTERING_CRITICAL_SECTION

         MOV   SWITCH,DELETE_OLD_WAY+ACTIVE ; show how to dew it
         JMP   SHORT DEW_IT

NEW_WAY_TO_DELETE:
         CALL  ENTERING_CRITICAL_SECTION

         MOV   SWITCH,DELETE_NEW_WAY+ACTIVE ; show how to dew it

DEW_IT:
         TEST  INSTALL_SWITCH,ASSIGN_RESIDENT
         JNZ   ASSIGN_ALREADY_RESIDENT
         CALL  HAS_ASSIGN_BEEN_INSTALLED
         JNE   ASSIGN_ALREADY_RESIDENT
;         CALL  BEEP                    ;M006: Disable useless feedback.
         MOV   SWITCH,0

         CALL  LEAVING_CRITICAL_SECTION

         JMP   DOS_INTERCEPT_BAILOUT

ASSIGN_ALREADY_RESIDENT:
         MOV   CTL_FILE_HANDLE,0        ;
         MOV   SAVE_AX,AX
         MOV   SAVE_BX,BX
         MOV   SAVE_CX,CX
         MOV   SAVE_DX,DX
         MOV   SAVE_SI,SI
         MOV   SAVE_BP,BP
         MOV   SAVE_DI,DI
         MOV   SAVE_DS,DS
         MOV   SAVE_ES,ES
         MOV   AX,TEMP_FLAGS
         MOV   SAVE_FLAGS,AX

         STI
         CLD

; save current DTA

         MOV   AH,2FH                   ; get curr DTA
         DOSFAKE
         MOV   OLD_DTA_SEG,ES           ; save segment
         MOV   OLD_DTA_OFF,BX           ; save offset

         MOV   DS,CS_SAVE               ; prime
         MOV   ES,CS_SAVE               ;  'em

         TEST  SWITCH,DELETE_OLD_WAY    ; dewin' a 13H delete?
         JNZ   DEW_IT_OLD_WAY           ; yup
         JMP   DEW_IT_NEW_WAY           ; nope

DEW_IT_OLD_WAY:
         CALL  CHECK_IF_WE_SHOULD_HANDLE_OLD
         JNC   HANDLE_IT_OLD            ; keep goin'
         OR    SWITCH,GIVE_IT_TO_DOS    ; sew that we'll exit rite
         JMP   CLEAN_UP_AND_LEAVE

HANDLE_IT_OLD:
         CALL  SEE_IF_FILE_EXISTS_OLD   ; see ifin it's out there
         JNC   GET_US_OLD               ; keep goin'

         TEST  SWITCH,WE_FOUND_US       ; were we the only one?
         JNZ   GET_US_OLD_BYPASS        ; yup, forget it
         OR    SWITCH,GIVE_IT_TO_DOS    ; sew that we'll exit rite

GET_US_OLD_BYPASS:
         JMP   CLEAN_UP_AND_LEAVE

GET_US_OLD:
         CALL  MAKE_SURE_OUR_FILE_IZ_THERE ; verify/create our dir
         JNC   GET_CLU_OLD             ; all's well

         OR    SWITCH,GIVE_IT_TO_DOS    ; sew that we'll exit rite
         OR    SWITCH,KOULDNT_PROTECT   ; can't protect!
         JMP   CLEAN_UP_AND_LEAVE

GET_CLU_OLD:
         CALL  GET_CLUSTERS_OLD         ; get our ctl file set
         JNC   DELETE_OLD

         OR    SWITCH,GIVE_IT_TO_DOS    ; sew that we'll exit rite
         OR    SWITCH,KOULDNT_PROTECT   ; can't protect!
         JMP   SHORT CHECK_FOR_OTHERS

DELETE_OLD:
         TEST  INSTALL_SWITCH,DO_IT_THE_SLOW_WAY
         JNZ   DELETE_OLD_SLOW

         CALL  GET_TIME_STAMP
         JMP   SHORT UPDATE_CTL_OLD

DELETE_OLD_SLOW:
         CALL  DELETE_IT_OLD            ; dew sum shuckin' & jivin'
         JNC   UPDATE_CTL_OLD           ; all's well

         OR    SWITCH,GIVE_IT_TO_DOS    ; sew that we'll exit rite
         OR    SWITCH,KOULDNT_PROTECT   ; can't protect!
         JMP   SHORT CHECK_FOR_OTHERS

UPDATE_CTL_OLD:
         CALL  UPDATE_CTL_FILE          ; update fur next time
         JNC   CHECK_FOR_OTHERS
         OR    SWITCH,KOULDNT_PROTECT   ; can't protect!

CHECK_FOR_OTHERS:
         CALL  SEE_IF_THERE_ARE_OTHERS  ; go see if wildcard wuz used
         JNC   GET_CLU_OLD              ; there's more!

         TEST  INSTALL_SWITCH,DO_IT_THE_SLOW_WAY
         JNZ   DELETE_OLD_SLOW_THRU
         OR    SWITCH,GIVE_IT_TO_DOS    ; sew that we'll exit rite

DELETE_OLD_SLOW_THRU:
         JMP   SHORT CLEAN_UP_AND_LEAVE



DEW_IT_NEW_WAY:
         CALL  CHECK_IF_WE_SHOULD_HANDLE_NEW
         JNC   HANDLE_IT_NEW            ; keep goin'
         OR    SWITCH,GIVE_IT_TO_DOS    ; sew that we'll exit rite
         JMP   SHORT CLEAN_UP_AND_LEAVE

HANDLE_IT_NEW:
         CALL  SEE_IF_FILE_EXISTS_NEW   ; see ifin it's out there
         JNC   GET_US_NEW               ; keep goin'
         TEST  SWITCH,WE_FOUND_US       ; were we the only one?
         JNZ   GET_US_NEW_BYPASS        ; yup, forget it
         OR    SWITCH,GIVE_IT_TO_DOS    ; sew that we'll exit rite
         JMP   SHORT CLEAN_UP_AND_LEAVE

GET_US_NEW:
         CALL  MAKE_SURE_OUR_FILE_IZ_THERE ; verify/create our dir
         JNC   GET_CLU_NEW              ; all's well
         OR    SWITCH,GIVE_IT_TO_DOS    ; sew that we'll exit rite
         OR    SWITCH,KOULDNT_PROTECT   ; can't protect!

GET_US_NEW_BYPASS:
         JMP   SHORT CLEAN_UP_AND_LEAVE

GET_CLU_NEW:
         CALL  GET_CLUSTERS_NEW
         JNC   DEW_DELETE_NEW
         OR    SWITCH,KOULDNT_PROTECT   ; can't protect!
         OR    SWITCH,GIVE_IT_TO_DOS    ; sew that we'll exit rite
         JMP   SHORT CLEAN_UP_AND_LEAVE

DEW_DELETE_NEW:
         CALL  DELETE_IT_NEW
         JNC   UPDATE_CTL_NEW

         TEST  SWITCH,DELETE_SUB_DIR    ; is for a sub_dir?
         JNZ   DEL_DELETE_NEW_CONT      ; yes, forget honking
         OR    SWITCH,KOULDNT_PROTECT   ; no, gotta honk

DEL_DELETE_NEW_CONT:
         OR    SWITCH,GIVE_IT_TO_DOS    ; sew that we'll exit rite
         JMP   SHORT CLEAN_UP_AND_LEAVE ; didn't work

UPDATE_CTL_NEW:
         CALL  UPDATE_CTL_FILE
         JNC   CLEAN_UP_AND_LEAVE
         OR    SWITCH,KOULDNT_PROTECT   ; can't protect!



CLEAN_UP_AND_LEAVE:
         CMP   CTL_FILE_HANDLE,0        ; file closed?
         JE    SHOULD_WE_BEEP

         MOV   BX,CTL_FILE_HANDLE       ; grab handle
         MOV   AH,3EH                   ; close
         DOSFAKE
         MOV   CTL_FILE_HANDLE,0        ;

SHOULD_WE_BEEP:
         TEST  SWITCH,KOULDNT_PROTECT   ; couldn't protect?
         JZ    NO_BEEP                  ; nope, all wuz well
         TEST  SWITCH,NO_CLUSTERS_ALLOCATED ; couldn't protect but
         JNZ   NO_BEEP                  ; who cares

;         CALL  BEEP                    ;M006: Disable useless feedback.

NO_BEEP:

; restore DTA where it wuz when we started this mess

         MOV   DS,OLD_DTA_SEG           ; saved segment
         MOV   DX,OLD_DTA_OFF           ; saved offset
         MOV   AH,1AH                   ; set DTA
         DOSFAKE

         MOV   AH,0DH                   ; flush buffers
         DOSFAKE

         MOV   AX,SAVE_AX
         MOV   BX,SAVE_BX
         MOV   CX,SAVE_CX
         MOV   DX,SAVE_DX
         MOV   BP,SAVE_BP
         MOV   SI,SAVE_SI
         MOV   DI,SAVE_DI
         MOV   DS,SAVE_DS
         MOV   ES,SAVE_ES

; There are three possible exit paths -
; (1) Pass onward to DOS, (2) return with AL=0 or (3) return with CF=0.

         CLI
         TEST  SWITCH,GIVE_IT_TO_DOS    ; sew that we'll exit rite
;;* GWD  MOV   SWITCH,0                 ;Shut off re-entrancy protection.
         JNZ   GO_TO_DOS                ; let DOS handle missing file

         test  switch,delete_old_way   ;3-20-90 GWD.
         jnz   go_back_to_caller_old

; Function was new delete (41h) or RMDIR (3Ah).

         MOV   SWITCH,0         ;Shut off re-entrancy protection.

         CALL  LEAVING_CRITICAL_SECTION

         push  bp              ;3-20-90 GWD.
         mov   bp,sp           ;On stack - BP, IP, CS, Flags.
         and   word ptr [bp+6], NOT 1  ;Clear carry flag bit image.
         pop   bp
         iret

go_back_to_caller_old:          ;Function was old delete (13h).
         mov   al,0             ;Return AL = 0 (success).
         MOV   SWITCH,0         ;Shut off re-entrancy protection.

         CALL  LEAVING_CRITICAL_SECTION

         iret

; removed, 3-20-90 GWD
;        MOV   AX,0                     ; sew return code iz clean
;        PUSH  SAVE_FLAGS               ; saved frags onto stack
;        POPF                           ; pop 'em
;        CLC                            ; assure no carry set
;        STI
;        RET   2                        ; return to interuptee


GO_TO_DOS:
         PUSH  SAVE_FLAGS
         POPF
         MOV   SWITCH,0                 ;Shut off re-entrancy protection.

;	M007 -- note: The old code here left a nasty hole in the
;			critical section handling that would allow a file
;			to be lost if another process was undeleting it
;			while we're deleting it.

;M007         CALL  LEAVING_CRITICAL_SECTION
;M007
;M007         JMP   DWORD PTR DOS_VECTOR     ; on to DOS

	dosfake				; call through to DOS
	pushf				; save those flags
	call	LEAVING_CRITICAL_SECTION ; then restore crit section

		; m012 Fixed bug with leaving interrupts disabled
		; m012 when do "ret 02" by setting the carry bit in the
		; m012 int 21h caller's original flags on the stack
;m012	popf				; get DOS's return flags back
;m012	ret	2			; and return to caller

	pop	CARRY_FLAG		; m012 Get return flags from DOS call
					; m012
	push	BP			; m012 Address int 21h caller's flags
	mov	BP,SP			; m012

	push	AX			; m012 
	mov	AX,CARRY_FLAG		; m012 Get flags from int 21h call
	and	AX,1			; m012 Mask off the carry flag

	and	[BP+6], NOT 1		; m012 Clear carry in caller's flags
	or	[BP+6],AX		; m012 Now to carry from int 21h call
	pop	AX			; m012

	pop	BP			; m012
	iret				; m012 

;	M007 -- end modifications


DOS_INTERCEPT ENDP



ENTERING_CRITICAL_SECTION PROC NEAR
         PUSH  AX
         TEST  INSTALL_SWITCH,WINDOWS_PRESENT
         JZ    E_C_S_EXIT

         MOV   AX,1681H
         INT   2FH
         TEST  AL,AL
         JZ    E_C_S_EXIT

         PUSH  ES
         PUSH  BX
         LES   BX,IN_DOS_DWORD
         INC   BYTE PTR ES:[BX]
         POP   BX
         POP   ES

E_C_S_EXIT:

	mov	ah,0dh		; M007	; Force disk reset to synchronize!
	dosfake			; M007

         POP   AX
         RET

ENTERING_CRITICAL_SECTION ENDP



LEAVING_CRITICAL_SECTION PROC NEAR
         PUSH  AX

	mov	ah,0dh		; M007	; Force disk reset to syncrhonize!
	dosfake			; M007

         TEST  INSTALL_SWITCH,WINDOWS_PRESENT
         JZ    L_C_S_EXIT

         MOV   AX,1682H
         INT   2FH
         TEST  AL,AL
         JZ    L_C_S_EXIT

         PUSH  ES
         PUSH  BX
         LES   BX,IN_DOS_DWORD
         CMP   BYTE PTR ES:[BX],0
         JE    L_C_S_NO_DEC

         DEC   BYTE PTR ES:[BX]

L_C_S_NO_DEC:
         POP   BX
         POP   ES

L_C_S_EXIT:
         POP   AX
         RET

LEAVING_CRITICAL_SECTION ENDP



HAS_ASSIGN_BEEN_INSTALLED PROC NEAR
         PUSH  AX
         PUSH  DS
         MOV   AX,0
         CMP   DOS_VERSION,0300H
         JB    HAS_BEEN_FOUND
         MOV   DS,AX
         CMP   WORD PTR DS:[2FH*4],0
         JNE   HAS_BEEN_CHK
         CMP   WORD PTR DS:[2FH*4]+2,0
         JNE   HAS_BEEN_CHK
         JMP   SHORT HAS_BEEN_FOUND

HAS_BEEN_CHK:
         MOV   AX,0600H
         INT   2FH                      ; iz ASSIGN resident?

HAS_BEEN_FOUND:
         CMP   AL,0FFH
         POP   DS
         POP   AX
         RET

HAS_ASSIGN_BEEN_INSTALLED ENDP


IF 0 ;M006: Extraneous.
BEEP     PROC  NEAR
         PUSH  AX
         PUSH  BX
         PUSH  CX
         PUSH  DX
         PUSH  SI
         PUSH  DI
         PUSH  BP
         MOV   BX,0
         MOV   AX,0E07H
         INT   10H
         MOV   BX,0
         MOV   AX,0E07H
         INT   10H
         POP   BP
         POP   DI
         POP   SI
         POP   DX
         POP   CX
         POP   BX
         POP   AX
         RET

BEEP     ENDP
ENDIF ;M006



CHECK_IF_WE_SHOULD_HANDLE_NEW PROC NEAR
         PUSH  ES
         PUSH  DS
         PUSH  SI
         MOV   DS,SAVE_DS               ; get his DS
         MOV   SI,SAVE_DX               ; get his DX
         CMP   BYTE PTR [SI+1],':'      ; drive given?
         JNE   CHECK_IF_DEFAULT_DRIVE
         MOV   AL,[SI]                  ; get drive letter
         CALL  UPPERCASE
         CMP   AL,'A'
         JB    CHECK_IF_WE_SHOULD_HANDLE_STC_JMP
         CMP   AL,'Z'
         JA    CHECK_IF_WE_SHOULD_HANDLE_STC_JMP
         CMP   BYTE PTR [SI+2],'\'      ; root forced?
         JE    CHECK_SETUP_GOTTA_SLASH
         CMP   BYTE PTR [SI+2],'/'      ; root forced?
         JNE   CHECK_SETUP_DRIVE

CHECK_SETUP_GOTTA_SLASH:
         OR    SWITCH,FORCED_TO_ROOT
         JMP   SHORT CHECK_SETUP_DRIVE

CHECK_IF_WE_SHOULD_HANDLE_STC_JMP:
         JMP   SHORT CHECK_IF_WE_SHOULD_HANDLE_NEW_STC

CHECK_IF_DEFAULT_DRIVE:
         MOV   AH,19H
         DOSFAKE
         ADD   AL,41H
         CMP   BYTE PTR [SI],'\'        ; forced to root?
         JE    CHECK_SETUP_GOTTA_SLASH_1
         CMP   BYTE PTR [SI],'/'        ; forced to root?
         JNE   CHECK_SETUP_DRIVE

CHECK_SETUP_GOTTA_SLASH_1:
         OR    SWITCH,FORCED_TO_ROOT

CHECK_SETUP_DRIVE:
         CALL  SETUP_DRIVE
         JC    CHECK_IF_WE_SHOULD_HANDLE_NEW_STC

         TEST  SWITCH,FORCED_TO_ROOT
         JNZ   CHECK_IF_CLC
         MOV   DS,CS_SAVE
         MOV   SI,CTL_FILE_ASCIIZ_OFFSET
         MOV   DL,DR.IVE_NUMBER
         INC   DL
         MOV   AH,47H
         DOSFAKE
         MOV   AL,0
         MOV   DI,CTL_FILE_ASCIIZ_OFFSET
         MOV   ES,CS_SAVE

         CMP   AL,ES:[DI]               ; at root?
         JE    CHECK_IF_CLC

         MOV   CX,125
         REPNE SCASB
         JNE   CHECK_IF_WE_SHOULD_HANDLE_NEW_STC
         MOV   BYTE PTR ES:[DI-1],'\'
         MOV   CTL_FILE_ASCIIZ_OFFSET,DI

CHECK_IF_CLC:
         CLC
         JMP   SHORT CHECK_NEW_EXIT

CHECK_IF_WE_SHOULD_HANDLE_NEW_STC:
         STC

CHECK_NEW_EXIT:
         POP   SI
         POP   DS
         POP   ES
         RET

CHECK_IF_WE_SHOULD_HANDLE_NEW ENDP



CHECK_IF_WE_SHOULD_HANDLE_OLD PROC NEAR
         PUSH  ES
         PUSH  DS
         PUSH  SI
         MOV   DS,SAVE_DS               ; get his DS
         MOV   SI,SAVE_DX               ; get his DX
         CMP   BYTE PTR [SI],0FFH       ; extended FCB?
         JNE   CHECK_OLD_NON_EXTENDED
         OR    SWITCH,EXTENDED_FCB
         MOV   AL,[SI]+7                ; get drive
         JMP   SHORT CHECK_OLD_DRIVE

CHECK_OLD_NON_EXTENDED:
         MOV   AL,[SI]                  ; get drive

CHECK_OLD_DRIVE:
         CMP   AL,0
         JNE   CHECK_OLD_GOT_DRIVE
         MOV   AH,19H
         DOSFAKE
         INC   AL

CHECK_OLD_GOT_DRIVE:
         ADD   AL,40H

         CALL  SETUP_DRIVE
         JC    CHECK_IF_WE_SHOULD_HANDLE_OLD_STC

         MOV   DS,CS_SAVE
         MOV   SI,CTL_FILE_ASCIIZ_OFFSET
         MOV   DL,DR.IVE_NUMBER
         INC   DL
         MOV   AH,47H
         DOSFAKE
         MOV   AL,0
         MOV   DI,CTL_FILE_ASCIIZ_OFFSET
         MOV   ES,CS_SAVE

         CMP   AL,ES:[DI]               ; at root?
         JE    CHECK_OLD_IF_CLC

         MOV   CX,125
         REPNE SCASB
         JNE   CHECK_IF_WE_SHOULD_HANDLE_OLD_STC
         MOV   BYTE PTR ES:[DI-1],'\'
         MOV   CTL_FILE_ASCIIZ_OFFSET,DI

CHECK_OLD_IF_CLC:
         CLC
         JMP   SHORT CHECK_OLD_EXIT

CHECK_IF_WE_SHOULD_HANDLE_OLD_STC:
         STC

CHECK_OLD_EXIT:
         POP   SI
         POP   DS
         POP   ES
         RET

CHECK_IF_WE_SHOULD_HANDLE_OLD ENDP



CLEAR_CTL_FILE_ENTRY PROC NEAR
         PUSH  ES
         PUSH  DI
         PUSH  CX
         MOV   ES,CS_SAVE
         LEA   DI,CTL_FILE_ENTRY
         MOV   CX,CTL_FILE_ENTRY_LEN
         PUSH  AX
         MOV   AL,0
         REP   STOSB
         POP   AX

         MOV   OUR_CTL_ASCIIZ,AL
         MOV   CTL_FILE_ASCIIZ,AL
         MOV   WORD PTR CTL_FILE_ASCIIZ+1,'\:'
         MOV   CTL_FILE_ASCIIZ_OFFSET,OFFSET CTL_FILE_ASCIIZ+3

         POP   CX
         POP   DI
         POP   ES
         RET

CLEAR_CTL_FILE_ENTRY ENDP



SETUP_DRIVE PROC NEAR
         PUSH  DS
         PUSH  BX

         SUB   AL,40H
         MOV   DL,AL
         MOV   AH,32H
         DOSFAKE
         CMP   AL,0FFH
         JE    SETUP_DRIVE_STC_JMP

         MOV   AL,[BX]                  ; This will cure assigning
         MOV   DR.IVE_NUMBER,AL
         ADD   AL,41H

         CALL  DRIVE_SUPPORTED
         JNC   SETUP_DRIVE_SUPPORTED

SETUP_DRIVE_STC_JMP:
         JMP   SETUP_DRIVE_STC

SETUP_DRIVE_SUPPORTED:
         CALL  CLEAR_CTL_FILE_ENTRY

         CALL  CHECK_FOR_BIG_HARD_DRIVE

         CMP   DR.IVE_CTL_FILE_ENTRIES,0
         JNE   SETUP_DRIVE_CONT

         MOV   AX,[BX+13]               ; get total clu count +1
         DEC   AX
         MOV   CH,0
         MOV   CL,[BX+4]                ; get sectors/clu -1
         INC   CX
         MUL   CX                       ; get # sectors

         MOV   DR.IVE_CTL_FILE_ENTRIES,303
         CMP   DX,0                     ; already too big?
         JNE   SETUP_DRIVE_CONT

         MOV   DX,WORD PTR [BX+2]       ; get bytes per sector
         MOV   CL,9
         SHR   DX,CL
         MUL   DX

; for 360K or less       25             4K  LE   2D0H 512 byte sectors
;     720K               50             8K  GT   2D0H 512 byte sectors
;     1.2Meg             75            12K  GT   5A0H 512 byte sectors
;     1.44Meg            75            12K
;     less than 20Meg   101            16K  GT   B40H 512 byte sectors
;     up to 32M         202            32K  GT  A000H 512 byte sectors
;     Over 32M          303            48K  GT 10000H 512 byte sectors

         CMP   DX,0
         JNE   SETUP_DRIVE_CONT
         MOV   DR.IVE_CTL_FILE_ENTRIES,202
         CMP   AX,0A000H
         JA    SETUP_DRIVE_CONT
         MOV   DR.IVE_CTL_FILE_ENTRIES,101
         CMP   AX,0B40H
         JA    SETUP_DRIVE_CONT
         MOV   DR.IVE_CTL_FILE_ENTRIES,75
         CMP   AX,05A0H
         JA    SETUP_DRIVE_CONT
         MOV   DR.IVE_CTL_FILE_ENTRIES,50
         CMP   AX,02D0H
         JA    SETUP_DRIVE_CONT
         MOV   DR.IVE_CTL_FILE_ENTRIES,25

SETUP_DRIVE_CONT:
         MOV   AX,[BX+2]                ; get sector size
         CMP   LARGEST_SECTOR,AX        ; can't support
         JB    SETUP_DRIVE_STC          ; if too large
         MOV   DR.IVE_SECTOR_SIZE,AX

         MOV   AL,[BX+4]                ; get sectors per clu minus one
         INC   AL
         MOV   DR.IVE_SECTORS_PER_CLU,AL

         MOV   AX,[BX+6]                ; get number reserved (boot +)
         MOV   DR.IVE_FIRST_FAT_SECTOR,AX
         MOV   FAT_SECTOR,0FFFFH        ; invalidate in-memory sector #

         MOV   AX,[BX+11]               ; get first data sector
         MOV   DR.IVE_FIRST_DATA_SECTOR,AX

         MOV   DR.IVE_SWITCH,0
         MOV   AX,[BX+13]               ; get total clu count + 1
         CMP   AX,4086
         JNA   SETUP_DRIVE_12_BITS
         OR    DR.IVE_SWITCH,FAT_IZ_16_BITS

SETUP_DRIVE_12_BITS:
         CMP   DOS_VERSION,0400H        ; DOS 4.00 or greater?
         JAE   SETUP_DRIVE_DOS_40       ; NO
         MOV   AX,[BX+16]               ; get first root dir sector
         MOV   DR.IVE_FIRST_ROOT_SECTOR,AX
         JMP   SHORT SETUP_DRIVE_CLC

SETUP_DRIVE_DOS_40:
         MOV   AX,[BX+17]               ; get first root dir sector
         MOV   DR.IVE_FIRST_ROOT_SECTOR,AX

SETUP_DRIVE_CLC:
         CLC
         JMP   SHORT SETUP_DRIVE_EXIT

SETUP_DRIVE_STC:
         STC

SETUP_DRIVE_EXIT:
         POP   BX
         POP   ES
         RET

SETUP_DRIVE ENDP



DRIVE_SUPPORTED PROC NEAR
         PUSH  DS
         PUSH  SI
         PUSH  AX
         PUSH  BX
         PUSH  DX
         MOV   DS,CS_SAVE
         MOV   BX,AX
         SUB   AL,41H
         MOV   AH,0
         SHL   AL,1
         LEA   SI,DRIVES_GIVEN_TABLE
         ADD   SI,AX

         MOV   AX,[SI]
         MOV   DR.IVE_CTL_FILE_ENTRIES,AX
         CMP   AX,0FFFFH                ; forget it?
         JE    DRIVE_SUPP_STC

         CMP   DOS_VERSION,030AH        ; DOS 3.10 or higher?
         JB    DRIVE_SUPP_CLC           ; no, branch
         MOV   AX,4409H                 ; see if NETWORK drive
         SUB   BL,40H
         MOV   DX,0
         DOSFAKE
         TEST  DX,1200H                 ; iz drive remote or network?
         JNZ   DRIVE_SUPP_NO_SUPP       ; yes, don't support it
         TEST  DX,8000H                 ; iz drive SUBSTed?
         JZ    DRIVE_SUPP_CLC           ; yes, don't support it

DRIVE_SUPP_NO_SUPP:
         OR    SWITCH,KOULDNT_PROTECT   ; couldn't protect?
         JMP   SHORT DRIVE_SUPP_STC     ; yes, don't support it

DRIVE_SUPP_CLC:
         CLC
         JMP   SHORT DRIVE_SUPP_EXIT

DRIVE_SUPP_STC:
         STC

DRIVE_SUPP_EXIT:
         POP   DX
         POP   BX
         POP   AX
         POP   SI
         POP   DS
         RET
DRIVE_SUPPORTED ENDP



MAKE_SURE_OUR_FILE_IZ_THERE PROC NEAR
         LEA   DX,OUR_CTL_ASCIIZ        ; to ASCIIZ string for dir
         MOV   AX,4301H                 ; open read/write
         MOV   CX,SYSTEM
         DOSFAKE                        ;
         JC    MAKE_SURE_FILE_OPEN_NO_FILE
         JMP   MAKE_SURE_FILE_OPEN

MAKE_SURE_FILE_OPEN_NO_FILE:
         MOV   BP,DR.IVE_CTL_FILE_ENTRIES
         CALL  ROUND_COUNT_TO_CLU

         LEA   DX,OUR_CTL_ASCIIZ        ; to ASCIIZ string for dir
         MOV   AX,3C00H                 ; open read/write
         MOV   CX,SYSTEM
         DOSFAKE                        ;
         JC    MAKE_SURE_BUILD_CLOSE
         MOV   BX,AX

         MOV   AX,DR.IVE_CTL_FILE_ENTRIES
         MOV   CTL_HEADER_NUM_ENTRIES,AX
         MOV   CTL_HEADER_OLDEST_ENTRY,0
         MOV   AH,40H
         MOV   CX,CTL_HEADER_LEN
         LEA   DX,CTL_HEADER
         MOV   DS,CS_SAVE
         DOSFAKE
         JC    MAKE_SURE_BUILD_CLOSE
         CMP   AX,CX
         JNE   MAKE_SURE_BUILD_CLOSE
         OR    CTL_FILE_IND,CTL_EMPTY

MAKE_SURE_BUILD_LOOP:
         MOV   AH,40H
         MOV   CX,CTL_FILE_ENTRY_LEN
         LEA   DX,CTL_FILE_ENTRY
         DOSFAKE
         JC    MAKE_SURE_BUILD_CLOSE
         CMP   AX,CX
         JNE   MAKE_SURE_BUILD_CLOSE
         DEC   BP
         JNZ   MAKE_SURE_BUILD_LOOP

MAKE_SURE_BUILD_CLOSE:
         MOV   CTL_FILE_IND,0
         MOV   AH,3EH
         DOSFAKE
         CMP   BP,0
         JE    MAKE_SURE_FILE_OPEN

         MOV   AH,41H
         LEA   DX,OUR_CTL_ASCIIZ
         DOSFAKE                        ; delete the control file

MAKE_SURE_BUILD_STC:
         STC
         JMP   SHORT MAKE_SURE_EXIT


MAKE_SURE_FILE_OPEN:
         LEA   DX,OUR_CTL_ASCIIZ        ; to ASCIIZ string for dir
         MOV   AX,3D02H                 ; open read/write
         DOSFAKE                        ;
         JC    MAKE_SURE_EXIT
         MOV   CTL_FILE_HANDLE,AX

         MOV   BX,AX
         MOV   AH,3FH
         MOV   CX,CTL_HEADER_LEN
         LEA   DX,CTL_HEADER
         DOSFAKE
         JC    MAKE_SURE_BUILD_STC
         CMP   AX,CX
         JE    MAKE_SURE_CLC
         JMP   MAKE_SURE_FILE_OPEN_NO_FILE      ;per Jim, GWD 01-05-89.

MAKE_SURE_CLC:
         CLC

MAKE_SURE_EXIT:
         RET

MAKE_SURE_OUR_FILE_IZ_THERE ENDP



ROUND_COUNT_TO_CLU PROC NEAR
         PUSH  DS
         PUSH  AX
         PUSH  BX
         PUSH  CX
         PUSH  DX

         MOV   AX,CTL_FILE_ENTRY_LEN
         MUL   BP
         ADD   AX,4
         ADC   DX,0
         PUSH  AX
         PUSH  DX

         MOV   DL,OUR_CTL_ASCIIZ
         SUB   DL,40H
         MOV   AH,1CH
         DOSFAKE
         MOV   AH,0
         MUL   CX
         MOV   BX,AX
         CMP   BX,0
         POP   DX
         POP   AX
         JE    R_C_T_C_EXIT

         DIV   BX
         MOV   AX,DX                    ; get remainder
         MOV   DX,0
         MOV   BX,CTL_FILE_ENTRY_LEN
         DIV   BX

R_C_T_C_EXIT:
         POP   DX
         POP   CX
         POP   BX
         POP   AX
         POP   DS
         RET

ROUND_COUNT_TO_CLU ENDP



SEE_IF_FILE_EXISTS_NEW PROC NEAR
         LEA   DI,ASCIIZ                ; we'll copy his ASCIIZ there
         MOV   DS,SAVE_DS               ; get his DS
         MOV   SI,SAVE_DX               ; get his DX
         MOV   CX,ASCIIZ_LEN            ;
         REP   MOVSB                    ; copy to our area

         MOV   DS,CS_SAVE               ; over here
         LEA   DX,FIND_FIRST_DTA        ; area FIND FIRST will use
         MOV   AH,1AH                   ; set DTA
         DOSFAKE

         LEA   DX,ASCIIZ                ; to ASCIIZ string for file
         MOV   CX,HIDDEN+SYSTEM         ; wanna see 'em all
         TEST  SWITCH,DELETE_SUB_DIR
         JZ    SEE_IF_FILE_EXISTS_NOT_SUB_DIR
         OR    CX,SUB_DIR               ; wanna see 'em all

SEE_IF_FILE_EXISTS_NOT_SUB_DIR:
         MOV   AH,4EH                   ; find first
         DOSFAKE
         JNC   SEE_IF_EXISTS_NEW_AGAIN
         JMP   SEE_IF_EXISTS_EXIT

SEE_IF_EXISTS_NEW_AGAIN:
         TEST  SWITCH,DELETE_SUB_DIR
         JZ    SEE_IF_FILE_EXISTS_IZNT_SUB_DIR
         TEST  FIND_FIRST_ATTRIBUTE,SUB_DIR
         JZ    SEE_IF_EXISTS_NEXT
         JMP   SHORT SEE_IF_FILE_EXISTS_CONT

SEE_IF_FILE_EXISTS_IZNT_SUB_DIR:
         TEST  FIND_FIRST_ATTRIBUTE,SUB_DIR
         JZ    SEE_IF_FILE_EXISTS_CONT

SEE_IF_EXISTS_NEXT:
         MOV   AH,4FH                   ; find next
         DOSFAKE
         JC    SEE_IF_EXISTS_EXIT
         JMP   SEE_IF_EXISTS_NEW_AGAIN

SEE_IF_FILE_EXISTS_CONT:
         CALL  IZ_IT_US_NEW
         JC    SEE_IF_EXISTS_NEXT
         CALL  IZ_IT_IGNORED_NEW
         JC    SEE_IF_EXISTS_NEXT
         MOV   AL,FIND_FIRST_ATTRIBUTE
         TEST  AL,READ_ONLY             ;GWD 03-20-90.  Per Jim.
         JNZ   SEE_IF_EXISTS_NEXT

         MOV   CTL_ATTR,AL
         MOV   AX,FIND_FIRST_TIME
         MOV   CTL_TIME,AX
         MOV   AX,FIND_FIRST_DATE
         MOV   CTL_DATE,AX
         MOV   AX,FIND_FIRST_SIZE_LOW
         MOV   CTL_SIZE_LO,AX
         MOV   AX,FIND_FIRST_SIZE_HIGH
         MOV   CTL_SIZE_HI,AX

         MOV   ES,CS_SAVE
         LEA   DI,ASCIIZ
         MOV   AL,0
         CMP   ASCIIZ+1,':'
         JNE   SEE_IF_SKIP_DRIVE
         ADD   DI,2

SEE_IF_SKIP_DRIVE:
         PUSH  DI
         MOV   CX,ASCIIZ_LEN
         REPNE SCASB
         POP   CX
         JNE   SEE_IF_EXISTS_EXIT
         XCHG  DI,CX
         SUB   CX,DI
         MOV   SI,DI
         MOV   DI,CTL_FILE_ASCIIZ_OFFSET

         TEST  SWITCH,FORCED_TO_ROOT
         JZ    SEE_IF_NOT_FORCED
         LEA   DI,CTL_FILE_ASCIIZ+2

SEE_IF_NOT_FORCED:
         REP   MOVSB
         CLC

SEE_IF_EXISTS_EXIT:
         RET

SEE_IF_FILE_EXISTS_NEW ENDP



;--------------------------------------------------
; This is a kluge to overcome a bug in DOS 2.x
; Function call # 11h (& probably 12h) corrupt
; the caller's FCB.  So we prevent that.  GWD 02-05-89.
;
; On entry: AH=11h or 12h, DS:DX points to an FCB.
;
; On exit: AX = whatever DOS returned.
;
; Only AX is changed.
;
FIXED_FCB_SEARCH PROC NEAR
         cmp   ah,12h
         je    fix_fs2         ;Func 12h (search for next) re-uses the FCB.
         push  cx
         push  si
         push  di
         push  es
         mov   di,cs
         mov   es,di
         mov   si,dx
         lea   di,fcb_copy
         cld
         mov   cx,44/2         ;37 for normal FCB, + 7 for extended FCB.
         rep   movsw           ;Copy it.
         pop   es
         pop   di
         pop   si
         pop   cx

fix_fs2:
         push  dx
         push  ds
         push  cs
         pop   ds
         nop
         lea   dx,fcb_copy     ;Let DOS use our local copy of the FCB.
         DOSFAKE
         pop   ds
         pop   dx
         ret

FCB_COPY DB    44 DUP(0),0

FIXED_FCB_SEARCH ENDP



SEE_IF_FILE_EXISTS_OLD PROC NEAR
         MOV   DS,CS_SAVE               ; over here
         LEA   DX,FIND_FIRST_DTA        ; area FIND FIRST will use
         MOV   AH,1AH                   ; set DTA
         DOSFAKE

         PUSH  DS
         PUSH  DX
         MOV   DS,SAVE_DS
         MOV   DX,SAVE_DX
         MOV   AH,11H
         CALL  FIXED_FCB_SEARCH
         CMP   AL,0
         JE    SEE_IF_EXISTS_OLD_CLC
         STC
         JMP   SHORT SEE_IF_EXISTS_OLD_EXIT

SEE_IF_EXISTS_OLD_IZ_SUB_DIR:
         CALL  SEE_IF_THERE_ARE_OTHERS
         JMP   SHORT SEE_IF_EXISTS_OLD_EXIT

SEE_IF_EXISTS_OLD_CLC:
         MOV   DS,CS_SAVE               ; over here
         LEA   SI,FIND_FIRST_DTA+1      ; area FIND FIRST will use
         TEST  SWITCH,EXTENDED_FCB      ; extended FCB?
         JZ    SEE_IF_EXISTS_OLD_NOT_EXTENDED
         ADD   SI,7                     ; point to start of good part

SEE_IF_EXISTS_OLD_NOT_EXTENDED:
         MOV   AL,[SI+DIR_ENT_ATTR]
         TEST  AL,SUB_DIR + READ_ONLY   ;Added 'read-only' 3-20-90 GWD.
         JNZ   SEE_IF_EXISTS_OLD_IZ_SUB_DIR
         CALL  IZ_IT_US_OLD
         JC    SEE_IF_EXISTS_OLD_IZ_SUB_DIR
         CALL  IZ_IT_IGNORED_OLD
         JC    SEE_IF_EXISTS_OLD_IZ_SUB_DIR

         CALL  ADD_OLD_NAME_TO_CTL

SEE_IF_EXISTS_OLD_EXIT:
         POP   DX
         POP   DS
         RET

SEE_IF_FILE_EXISTS_OLD ENDP



SEE_IF_THERE_ARE_OTHERS PROC NEAR
         PUSH  DS
         PUSH  DX

SEE_IF_THERE_IZ_SUB_DIR:
         MOV   DS,SAVE_DS
         MOV   DX,SAVE_DX
         MOV   AH,12H                   ; find next
         CALL  FIXED_FCB_SEARCH
         CMP   AL,0
         JE    SEE_IF_THERE_CLC
         STC
         JMP   SHORT SEE_IF_THERE_EXIT

SEE_IF_THERE_CLC:
         MOV   DS,CS_SAVE               ; over here
         LEA   SI,FIND_FIRST_DTA+1      ; area FIND FIRST will use
         TEST  SWITCH,EXTENDED_FCB      ; extended FCB?
         JZ    SEE_IF_THERE_NOT_EXTENDED
         ADD   SI,7                     ; point to start of good part

SEE_IF_THERE_NOT_EXTENDED:
         MOV   AL,[SI+DIR_ENT_ATTR]
         TEST  AL,SUB_DIR + READ_ONLY   ;Added 'read-only' 3-20-90 GWD.
         JNZ   SEE_IF_THERE_IZ_SUB_DIR
         CALL  IZ_IT_US_OLD
         JC    SEE_IF_THERE_IZ_SUB_DIR
         CALL  IZ_IT_IGNORED_OLD
         JC    SEE_IF_THERE_IZ_SUB_DIR

         CALL  ADD_OLD_NAME_TO_CTL

SEE_IF_THERE_EXIT:
         POP   DX
         POP   DS
         RET

SEE_IF_THERE_ARE_OTHERS ENDP



IZ_IT_US_OLD PROC NEAR
         PUSH  SI
         PUSH  DI
         PUSH  CX
         PUSH  ES
         CMP   CTL_FILE_ASCIIZ_OFFSET,OFFSET CTL_FILE_ASCIIZ+3
         JNE   IZ_IT_US_OLD_CLC
         LEA   DI,OUR_CTL_FCBNAME
         MOV   CX,OUR_CTL_FCBNAME_LEN
         MOV   ES,CS_SAVE
         REPE  CMPSB
         JNE   IZ_IT_US_OLD_CLC
         OR    SWITCH,WE_FOUND_US
         STC
         JMP   SHORT IZ_IT_US_OLD_EXIT

IZ_IT_US_OLD_CLC:
         CLC

IZ_IT_US_OLD_EXIT:
         POP   ES
         POP   CX
         POP   DI
         POP   SI
         RET

IZ_IT_US_OLD ENDP



IZ_IT_IGNORED_OLD PROC NEAR     ;Returns CF=true=ignore the file.
         PUSH  DI
         LEA   DI,[SI+8]
         CALL  CHECK_EXT
         POP   DI
         RET

IZ_IT_IGNORED_OLD ENDP



IZ_IT_US_NEW PROC NEAR
         PUSH  SI
         PUSH  DI
         PUSH  CX
         PUSH  ES
         PUSH  DS
         TEST  FIND_FIRST_ATTRIBUTE,SUB_DIR
         JNZ   IZ_IT_US_NEW_CLC
         CMP   CTL_FILE_ASCIIZ_OFFSET,OFFSET CTL_FILE_ASCIIZ+3
         JA    IZ_IT_US_NEW_CLC

         LEA   DI,ASCIIZ
         LEA   SI,OUR_CTL_ASCIIZ+3

         CMP   ASCIIZ+1,':'
         JNE   IZ_IT_US_NEW_SKIP_DRIVE
         ADD   DI,2
         CMP   BYTE PTR [DI],'\'
         JE    IZ_IT_US_NEW_GOTTA_SLASH
         CMP   BYTE PTR [DI],'/'
         JNE   IZ_IT_US_NEW_SKIP_DRIVE

IZ_IT_US_NEW_GOTTA_SLASH:
         INC   DI

IZ_IT_US_NEW_SKIP_DRIVE:
         MOV   CX,13
         MOV   ES,CS_SAVE
         MOV   DS,CS_SAVE
         REPE  CMPSB
         JNE   IZ_IT_US_NEW_CLC
         OR    SWITCH,WE_FOUND_US
         STC
         JMP   SHORT IZ_IT_US_NEW_EXIT

IZ_IT_US_NEW_CLC:
         CLC

IZ_IT_US_NEW_EXIT:
         POP   DS
         POP   ES
         POP   CX
         POP   DI
         POP   SI
         RET

IZ_IT_US_NEW ENDP



IZ_IT_IGNORED_NEW PROC NEAR
         PUSH  DI
         PUSH  CX
         PUSH  ES
         TEST  FIND_FIRST_ATTRIBUTE,SUB_DIR
         JNZ   IZ_IT_IGNORED_NEW_CLC

         MOV   ES,CS_SAVE
         LEA   DI,FIND_FIRST_FILE_EXT
         MOV   CX,13
         MOV   AL,'.'
         REPNE SCASB
         JNE   IZ_IT_IGNORED_NEW_CLC

         CALL  CHECK_EXT
         JMP   SHORT IZ_IT_IGNORED_NEW_EXIT

IZ_IT_IGNORED_NEW_CLC:
         CLC

IZ_IT_IGNORED_NEW_EXIT:
         POP   ES
         POP   CX
         POP   DI
         RET

IZ_IT_IGNORED_NEW ENDP



;--------------------------------------------------------
; Check a file extension for special values.  GWD.
; On entry: CS:DI points at the three-byte extension.
; On exit: if it matches any, then CF=true.  Else CF=false.
; All regs are preserved.

CHECK_EXT PROC NEAR
         push  ax
         push  dx
         push  di
         mov   al,cs:[di+2]
         call  uppercase
         mov   dl,al
         mov   al,cs:[di+1]
         call  uppercase
         mov   ah,al
         mov   al,cs:[di]
         call  uppercase               ;Sequential bytes are in AL,AH,DL.
         mov   dh,0
         lea   di,ext_ignore_list

chext_lp:
         cmp   dh,cs:[di]
         jz    chext_no                ;End of list.
         cmp   ax,cs:[di]
         jne   chext_next
         cmp   dl,cs:[di+2]
         jne   chext_next
         stc                           ;Indicate a match.
         jmp   short chext_done

chext_next:
         lea   di,[di+3]
         jmp   chext_lp

chext_no:
         clc                           ;No match.

chext_done:
         pop   di
         pop   dx
         pop   ax
         ret

CHECK_EXT ENDP


EXT_IGNORE_LIST LABEL byte
        DB      "$$$"
        DB      "TMP"
        DB      "IMG"           ; PC Shell
        DB      "THM"           ;    files
        DB      "IMX"           ;      to ignore
        DB      0               ;Zero marks the end of the list.



ADD_OLD_NAME_TO_CTL PROC NEAR
         MOV   DS,CS_SAVE               ; over here
         LEA   SI,FIND_FIRST_DTA+1      ; area FIND FIRST will use
         TEST  SWITCH,EXTENDED_FCB      ; extended FCB?
         JZ    ADD_OLD_SHORT
         ADD   SI,7                     ; point to start of good part

ADD_OLD_SHORT:

; now, move in the file name and extenshun from FCB

         PUSH  SI
         MOV   DI,CTL_FILE_ASCIIZ_OFFSET
         MOV   CX,8                     ; length of name
         REP   MOVSB                    ; move filename

         MOV   BYTE PTR ES:[DI],'.'     ; separate name frum ext
         INC   DI                       ; to next spot
         MOV   CX,3                     ; length of EXT
         REP   MOVSB                    ; move extenshun

         MOV   BYTE PTR ES:[DI],0       ; ternimate wit a binzer
         POP   SI

         MOV   AL,[SI+DIR_ENT_ATTR]
         MOV   CTL_ATTR,AL
         MOV   AX,[SI+DIR_ENT_TIME]
         MOV   CTL_TIME,AX
         MOV   AX,[SI+DIR_ENT_DATE]
         MOV   CTL_DATE,AX
         MOV   AX,[SI+DIR_ENT_SZLO]
         MOV   CTL_SIZE_LO,AX
         MOV   AX,[SI+DIR_ENT_SZHI]
         MOV   CTL_SIZE_HI,AX

         MOV   AX,[SI+DIR_ENT_CLU]
         MOV   CTL_INFO,AX

         CLC
         RET

ADD_OLD_NAME_TO_CTL ENDP



GET_CLUSTERS_OLD PROC NEAR
         AND   SWITCH,NOT NO_CLUSTERS_ALLOCATED
         MOV   AX,CTL_INFO              ; get starting_cluster #
         JMP   SHORT GET_CLUSTERS_CHECK

GET_CLUSTERS_NEW:
         AND   SWITCH,NOT NO_CLUSTERS_ALLOCATED
         CALL  GET_STARTING_CLU
         JC    GET_CLUSTERS_STC

;        AX has the starting clu. Go track chain.

GET_CLUSTERS_CHECK:
         CMP   AX,0                     ; not a real file?
         JE    GET_CLUSTERS_NOT_ALLOC

         CALL  FOLLOW_CHAIN
         JC    GET_CLUSTERS_STC
         CLC
         RET

GET_CLUSTERS_NOT_ALLOC:
         OR    SWITCH,NO_CLUSTERS_ALLOCATED

GET_CLUSTERS_STC:
         STC
         RET

GET_CLUSTERS_OLD ENDP



GET_STARTING_CLU PROC NEAR
         MOV   AX,FIND_FIRST_30_LASTENT

         MUL   THIRTY_TWO               ; get directory entry offset
         DIV   DR.IVE_SECTOR_SIZE       ; get which rel sector
         MOV   DI,AX                    ; save rel sector
         MOV   BX,DX                    ; save offset
         MOV   AX,FIND_FIRST_20_DIRSTART
         CMP   DOS_VERSION,0300H
         JB    G_S_C_2
         MOV   AX,FIND_FIRST_30_DIRSTART

G_S_C_2:
         CMP   AX,0
         JE    GET_STARTING_OUTTA_ROOT

         CALL  FIND_LOG_SECTOR
         JC    GET_STARTING_CLU_STC
         JMP   SHORT GET_STARTING_DIR_ENTRY

GET_STARTING_OUTTA_ROOT:
         ADD   DI,DR.IVE_FIRST_ROOT_SECTOR
         MOV   AX,DI
         MOV   DX,0

GET_STARTING_DIR_ENTRY:
         PUSH  BX
         MOV   BX,IO_AREA_PTR
         MOV   ABS25_SECTOR_LOW,AX      ; set up
         MOV   ABS25_SECTOR_HIGH,DX     ;  sector num
         MOV   CX,1
         MOV   ABS25_SECTOR_COUNT,CX    ;  (read jus wun)
         CALL  ABS_25
         POP   BX
         JC    GET_STARTING_CLU_STC

         ADD   BX,IO_AREA_PTR
         MOV   AL,[BX+DIR_ENT_ATTR]
         CMP   CTL_ATTR,AL
         JNE   GET_STARTING_CLU_STC
         MOV   AX,WORD PTR [BX+DIR_ENT_TIME]
         CMP   CTL_TIME,AX
         JNE   GET_STARTING_CLU_STC
         MOV   AX,WORD PTR [BX+DIR_ENT_DATE]
         CMP   CTL_DATE,AX
         JNE   GET_STARTING_CLU_STC
         MOV   AX,WORD PTR [BX+DIR_ENT_SZLO]
         CMP   CTL_SIZE_LO,AX
         JNE   GET_STARTING_CLU_STC
         MOV   AX,WORD PTR [BX+DIR_ENT_SZHI]
         CMP   CTL_SIZE_HI,AX
         JNE   GET_STARTING_CLU_STC

         MOV   AX,WORD PTR [BX+DIR_ENT_CLU]
         CLC
         RET

GET_STARTING_CLU_STC:
         STC
         RET
GET_STARTING_CLU ENDP



FOLLOW_CHAIN PROC NEAR
         MOV   DI,IO_AREA_PTR
         MOV   ES,CS_SAVE
         CMP   AX,2
         JB    FOLLOW_CHAIN_STC
         CMP   AX,0FFF7H
         JNB   FOLLOW_CHAIN_STC
         STOSW                          ; starting clu #

FOLLOW_CHAIN_LOOP:
         MOV   SI,AX                    ; save current clu #
         CALL  GET_FAT
         CMP   AX,2
         JB    FOLLOW_CHAIN_STC
         CMP   AX,0FFF7H
         JE    FOLLOW_CHAIN_STC
         JA    FOLLOW_CHAIN_END
         CALL  STOSW_CLU                ; go see if wrap around
         JC    FOLLOW_CHAIN_STC

         INC   SI                       ; increment last clu
         CMP   SI,AX                    ; do we have a progression?
         JNE   FOLLOW_CHAIN_LOOP        ; no, branch

         CALL  GET_FAT                  ; yes, get next one
         CMP   AX,2
         JB    FOLLOW_CHAIN_STC
         CMP   AX,0FFF7H
         JE    FOLLOW_CHAIN_STC
         JA    FOLLOW_CHAIN_END
         CALL  STOSW_CLU                ; go see if wrap around
         JC    FOLLOW_CHAIN_STC

         INC   SI                       ; increment last one
         CMP   SI,AX                    ; do we have three in a row?
         JNE   FOLLOW_CHAIN_LOOP
         MOV   WORD PTR ES:[DI-4],0     ; indicate a range

FOLLOW_CHAIN_RANGE:
         CALL  GET_FAT                  ; yes, get next one
         CMP   AX,2
         JB    FOLLOW_CHAIN_STC
         CMP   AX,0FFF7H
         JE    FOLLOW_CHAIN_STC
         JA    FOLLOW_CHAIN_END
         INC   SI                       ; increment last one
         CMP   SI,AX                    ; do we have three in a row?
         JNE   FOLLOW_CHAIN_STOSW
         MOV   ES:[DI-2],AX             ; extend a range
         JMP   FOLLOW_CHAIN_RANGE


FOLLOW_CHAIN_STOSW:
         CALL  STOSW_CLU                ; go see if wrap around
         JC    FOLLOW_CHAIN_STC
         JMP   FOLLOW_CHAIN_LOOP

FOLLOW_CHAIN_END:
         STOSW
         MOV   CLUSTERS_STORED_END,DI
         CLC
         RET

FOLLOW_CHAIN_STC:
         STC
         RET

FOLLOW_CHAIN ENDP



STOSW_CLU PROC NEAR
         PUSH  SI
         MOV   SI,IO_AREA_PTR

STOSW_CLU_LOOP:
         CMP   SI,DI
         JNB   STOSW_CLU_CLC
         CMP   WORD PTR ES:[SI],0       ; a range?
         JNE   STOSW_CLU_NOT_RANGE      ; no, branch
         CMP   WORD PTR ES:[SI-2],AX    ; below low part of range?
         JB    STOSW_CLU_INC_RANGE      ; yes, branch
         CMP   WORD PTR ES:[SI+2],AX    ; above high part of range?
         JNA   STOSW_CLU_STC            ; no, error, branch

STOSW_CLU_INC_RANGE:
         ADD   SI,2
         JMP   SHORT STOSW_CLU_INC

STOSW_CLU_NOT_RANGE:
         CMP   WORD PTR ES:[SI],AX      ; a match?
         JE    STOSW_CLU_STC            ; yes, oops

STOSW_CLU_INC:
         ADD   SI,2
         JMP   STOSW_CLU_LOOP

STOSW_CLU_CLC:
         STOSW

         MOV   SI,IO_AREA_PTR
         ADD   SI,LARGEST_SECTOR
         CMP   SI,DI                    ; blown buffer space?
         JBE   STOSW_CLU_STC            ; yup, ferget it

         POP   SI
         CLC
         RET

STOSW_CLU_STC:
         POP   SI
         STC
         RET

STOSW_CLU ENDP



DELETE_IT_NEW PROC NEAR
         MOV   DS,SAVE_DS
         MOV   DX,SAVE_DX
         MOV   AH,41H
         TEST  SWITCH,DELETE_SUB_DIR
         JZ    DELETE_IT_NEW_CONT
         MOV   AH,3AH

DELETE_IT_NEW_CONT:
         DOSFAKE
         CALL  GET_TIME_STAMP
         RET

DELETE_IT_NEW ENDP



DELETE_IT_OLD PROC NEAR
         MOV   DS,CS_SAVE
         MOV   ES,CS_SAVE
         LEA   SI,FIND_FIRST_DTA        ; point to unopened FCB
         LEA   DI,ASCIIZ                ; unused during old way
         MOV   CX,50
         REP   MOVSB
         LEA   DX,ASCIIZ
         MOV   AH,13H
         DOSFAKE
         CMP   AL,0
         JNE   DELETE_IT_OLD_STC
         CALL  GET_TIME_STAMP
         CLC
         RET

DELETE_IT_OLD_STC:
         STC
         RET

DELETE_IT_OLD ENDP



GET_TIME_STAMP PROC NEAR
         PUSHF
         PUSH  AX
         PUSH  BX
         PUSH  CX
         PUSH  DX

         MOV   AH,2CH
         DOSFAKE

         MOV   AX,CX
         MOV   CL,2
         SHL   AL,CL
         MOV   CL,3
         SHL   AX,CL
         MOV   CTL_TIME_DEL,AX

         MOV   AH,2AH
         DOSFAKE

         SUB   CX,1980
         MOV   AX,CX
         MOV   CL,9
         SHL   AX,CL
         MOV   CL,3
         SHL   DL,CL
         SHR   DX,CL
         OR    AX,DX
         MOV   CTL_DATE_DEL,AX

         POP   DX
         POP   CX
         POP   BX
         POP   AX
         POPF
         RET

GET_TIME_STAMP ENDP



ABS_25   PROC NEAR

;        DX    = logical sector num to read

         PUSH  DS
         PUSH  BX
         PUSH  CX
         PUSH  DX
         PUSH  SI
         PUSH  DI
         PUSH  BP

         TEST  DRIVE_INFO,ABOVE_32_MEG  ; in this mode?
         JZ    ABS_25_OLD_WAY           ; nope

         MOV   ABS25_XFER_ADDR_SEG,DS
         MOV   ABS25_XFER_ADDR_OFF,BX
         LEA   BX,ABS25_PARM_BLOCK
         MOV   CX,0FFFFH
         JMP   SHORT ABS_25_DEWIT

ABS_25_OLD_WAY:
         MOV   DX,ABS25_SECTOR_LOW

ABS_25_DEWIT:
         MOV   DS,CS_SAVE
         MOV   AL,DR.IVE_NUMBER
         INT   25H
         JC    ABS_25_ERROR
         POPF
         CLC

ABS_25_EXIT:
         POP   BP
         POP   DI
         POP   SI
         POP   DX
         POP   CX
         POP   BX
         POP   DS
         RET

ABS_25_ERROR:
         POPF
         STC
         JMP   ABS_25_EXIT

ABS_25   ENDP



UPDATE_CTL_FILE PROC NEAR
         MOV   ES,CS_SAVE
         MOV   DS,CS_SAVE
         MOV   CTL_FILE_IND,CTL_FIRST
         TEST  SWITCH,DELETE_SUB_DIR
         JZ    UPDATE_CTL_FILE_CONT
         OR    CTL_FILE_IND,CTL_SUB_DIR

UPDATE_CTL_FILE_CONT:
         CALL  BUILD_CTL_INFO

UPDATE_NOT_LAST:
         MOV   AX,CTL_HEADER_OLDEST_ENTRY

         MOV   BX,CTL_FILE_ENTRY_LEN
         MUL   BX
         ADD   AX,CTL_HEADER_LEN
         ADC   DX,0
         MOV   CX,DX
         MOV   DX,AX

         MOV   BX,CTL_FILE_HANDLE
         MOV   AX,4200H
         DOSFAKE
         JNC   UPDATE_CTL_POSITIONED
         JMP   SHORT UPDATE_CTL_STC

UPDATE_CTL_POSITIONED:
         INC   CTL_HEADER_OLDEST_ENTRY
         MOV   AX,CTL_HEADER_OLDEST_ENTRY
         CMP   AX,CTL_HEADER_NUM_ENTRIES
         JB    UPDATE_CTL_WRITE
         MOV   CTL_HEADER_OLDEST_ENTRY,0

UPDATE_CTL_WRITE:
         CALL  FORCE_UPPER_CASE
         MOV   AH,40H
         LEA   DX,CTL_FILE_ENTRY
         MOV   CX,CTL_FILE_ENTRY_LEN
         DOSFAKE
         JC    UPDATE_CTL_STC
         CMP   AX,CX
         JNE   UPDATE_CTL_STC

         TEST  CTL_FILE_IND,CTL_LAST
         JNZ   UPDATE_CTL_CLC

         AND   CTL_FILE_IND,CTL_CLEAR
         CALL  BUILD_CTL_INFO
         JMP   UPDATE_NOT_LAST

UPDATE_CTL_CLC:
         MOV   CX,0
         MOV   DX,0
         MOV   BX,CTL_FILE_HANDLE
         MOV   AX,4200H
         DOSFAKE
         JC    UPDATE_CTL_STC
         MOV   AH,40H
         MOV   CX,CTL_HEADER_LEN
         LEA   DX,CTL_HEADER
         DOSFAKE
         JC    UPDATE_CTL_STC
         CMP   AX,CX
         JNE   UPDATE_CTL_STC
         CLC
         RET

UPDATE_CTL_STC:
         STC
         RET

UPDATE_CTL_FILE ENDP



FORCE_UPPER_CASE PROC NEAR
         TEST  CTL_FILE_IND,CTL_FIRST
         JZ    F_U_C_RET
         PUSH  DS
         PUSH  SI
         PUSH  CX
         PUSH  AX
         MOV   CX,128
         LEA   SI,CTL_FILE_ASCIIZ
         MOV   DS,CS_SAVE

F_U_C_LOOP:
         MOV   AL,[SI]
         CALL  UPPERCASE
         MOV   [SI],AL
         INC   SI
         LOOP  F_U_C_LOOP

         POP   AX
         POP   CX
         POP   SI
         POP   DS

F_U_C_RET:
         RET

FORCE_UPPER_CASE ENDP



BUILD_CTL_INFO PROC NEAR
         MOV   CX,CTL_FILE_END-CTL_INFO
         LEA   DI,CTL_INFO
         TEST  CTL_FILE_IND,CTL_FIRST
         JNZ   B_C_I_1
         MOV   CX,CTL_FILE_END-CTL_INFO_CONT
         LEA   DI,CTL_INFO_CONT

B_C_I_1:
         MOV   AX,CLUSTERS_STORED_END
         SUB   AX,IO_AREA_PTR
         CMP   AX,CX
         JA    B_C_I_2
         OR    CTL_FILE_IND,CTL_LAST
         MOV   CX,AX

B_C_I_2:
         MOV   SI,IO_AREA_PTR
         REP   MOVSB
         MOV   CX,CLUSTERS_STORED_END
         SUB   CX,SI
         JZ    B_C_I_3
         PUSH  DI
         MOV   DI,IO_AREA_PTR
         REP   MOVSB
         MOV   CLUSTERS_STORED_END,DI
         POP   DI

B_C_I_3:
         LEA   CX,CTL_FILE_END
         SUB   CX,DI
         JZ    B_C_I_EXIT
         MOV   AL,0
         REP   STOSB

B_C_I_EXIT:
         RET

BUILD_CTL_INFO ENDP



START_CLU        DW 0
START_CLU_CONT   DW 0
REL_SECTOR       DW 0
REL_CLUSTER      DW 0
START_CLU_LAST   DW 0
REL_CLUSTER_LAST DW 0

FIND_LOG_SECTOR PROC NEAR
         PUSH  DS
         PUSH  BX
         PUSH  CX
         PUSH  DI
         PUSH  SI
         MOV   DS,CS_SAVE
         MOV   START_CLU,AX

;        First, we will find out which relative Cluster in the file
;        is involved and we will also have the relative sector in the
;        cluster.

F_L_S_1:
         MOV   AX,DI                    ; get relative clu/sec in file
         MOV   DX,0
         MOV   BL,DR.IVE_SECTORS_PER_CLU
         MOV   BH,0
         DIV   BX

;        If the starting cluster # is the same we may not need to
;        scan thru the entire FAT, just take off from where we were
;        before.

         MOV   BX,START_CLU
         CMP   START_CLU_LAST,BX       ; might this be a continuation?
         JE    FIND_LOG_SECTOR_CONTINUE ; yes, branch
         MOV   START_CLU_LAST,BX       ; no, save starting clu#
         JMP   SHORT FIND_LOG_SECTOR_OLD_WAY

FIND_LOG_SECTOR_CONTINUE:

;        Now, if the relative cluster is equal or greater than the
;        last, we can start from the last.

         CMP   AX,REL_CLUSTER_LAST     ; are we in range?
         JB    FIND_LOG_SECTOR_OLD_WAY ; no, branch

         PUSH  AX
         SUB   AX,REL_CLUSTER_LAST     ; get amount to go
         POP   REL_CLUSTER_LAST
         MOV   REL_CLUSTER,AX          ; set it up
         MOV   AX,START_CLU_CONT       ; get associated cluster #
         MOV   START_CLU,AX            ; and save it
         MOV   REL_SECTOR,DX           ; save relative sector #
         MOV   BX,FAT_AREA_PTR
         JMP   SHORT CHECK_NEXT_CLUSTER

;        Now, AX has the relative cluster in the file and DX has the
;        relative sector in the cluster

FIND_LOG_SECTOR_OLD_WAY:
         MOV   REL_SECTOR,DX           ; save relative sector in clus
         MOV   REL_CLUSTER,AX          ; save relative cluster in fil
         MOV   REL_CLUSTER_LAST,AX     ; save relative cluster in fil
         MOV   BX,FAT_AREA_PTR         ; point to FAT

CHECK_NEXT_CLUSTER:
         CMP   REL_CLUSTER,0              ; are we at the right cluster
         JE    AT_RIGHT_CLUSTER

;        Now we will determine move to the next cluster in the chain.

         TEST  DR.IVE_SWITCH,FAT_IZ_16_BITS
         JNZ   PROCESS_16_BIT_FAT

;        This will follow the chain thru the 12 bit FAT.

         DEC   REL_CLUSTER
         MOV   DI,START_CLU       ; pick up relative sector number
         SHR   DI,1               ; divide by 2
         ADD   DI,START_CLU       ; equals times 1.5

         MOV   DX,0
         MOV   AX,DI

         CALL  GET_RIGHT_SECTOR
         MOV   AX,[BX+DI]         ; pick up next

         TEST  START_CLU,1        ; is the sector number odd?
         JZ    ADJUST_FAT         ; no, branch
         MOV   CL,4
         SHR   AX,CL

ADJUST_FAT:
         AND   AX,0FFFH           ; clear hi-order byte
         MOV   START_CLU,AX
         CMP   AX,0FF7H            ; at end or error?
         JE    FAT_ERROR_JMP
         JA    AT_RIGHT_CLUSTER
         JMP   CHECK_NEXT_CLUSTER

FAT_ERROR_JMP:
         JMP   SHORT FAT_ERROR

PROCESS_16_BIT_FAT:
         DEC   REL_CLUSTER
         MOV   AX,START_CLU
         MUL   TWO

         CALL  GET_RIGHT_SECTOR
         MOV   AX,[BX+DI]               ; get next cluster #
         MOV   START_CLU,AX             ; save it
         CMP   START_CLU,0FFF7H         ; at end or error?
         JE    FAT_ERROR
         JA    AT_RIGHT_CLUSTER         ; at EOF
         JMP   CHECK_NEXT_CLUSTER

;        Now, if all is cool, compute logical sector # of INT 25H.

AT_RIGHT_CLUSTER:
         MOV   AX,START_CLU
         MOV   START_CLU_CONT,AX
         MOV   AX,REL_CLUSTER
         SUB   REL_CLUSTER_LAST,AX
         CMP   AX,0
         JNE   BEYOND_EOF

         TEST  DR.IVE_SWITCH,FAT_IZ_16_BITS
         JNZ   CHECK_FOR_16_BIT_EOF

         CMP   START_CLU,0FF7H
         JE    FAT_ERROR
         JA    BEYOND_EOF
         JMP   SHORT NOT_EOF

CHECK_FOR_16_BIT_EOF:
         CMP   START_CLU,0FFF7H
         JE    FAT_ERROR
         JA    BEYOND_EOF

NOT_EOF:
         SUB   START_CLU,2                ; sub 2
         MOV   AX,START_CLU
         MOV   BH,0
         MOV   BL,DR.IVE_SECTORS_PER_CLU
         MUL   BX
         ADD   AX,REL_SECTOR              ; rel sect # in cluster
         ADD   AX,DR.IVE_FIRST_DATA_SECTOR ; bump past FAT and DIR

         CLC

FIND_LOG_EXIT:
         POP   SI
         POP   DI
         POP   CX
         POP   BX
         POP   DS
         RET

FAT_ERROR:
         MOV   START_CLU_LAST,0FFFFH
         MOV   AX,0FFF7H
         STC
         JMP   FIND_LOG_EXIT

BEYOND_EOF:
         MOV   AX,0FFFFH
         STC
         JMP   FIND_LOG_EXIT

FIND_LOG_SECTOR ENDP



GET_RIGHT_SECTOR PROC NEAR
         PUSH  AX
         PUSH  CX
         PUSH  DX

         DIV   DR.IVE_SECTOR_SIZE
         MOV   DI,DX

         CMP   AX,FAT_SECTOR           ; got it in memory?
         JE    GET_RIGHT_OUT           ; yes, branch
         JB    GET_NEW_SECTORS         ; no, gotta read more
         MOV   CX,AX                   ; may be in second half
         DEC   CX

         CMP   CX,FAT_SECTOR           ; do they need second half?
         JNE   GET_NEW_SECTORS         ; no, forget it

         MOV   CX,DI                   ; but do they need very last byte
         INC   CX

         CMP   CX,DR.IVE_SECTOR_SIZE   ; spanning across last byte
         JNB   GET_NEW_SECTORS         ; yes, go read anyway

         ADD   DI,DR.IVE_SECTOR_SIZE   ; no, just adjust offset
         JMP   SHORT GET_RIGHT_OUT     ; and get out

GET_NEW_SECTORS:
         MOV   DX,AX
         MOV   FAT_SECTOR,AX
         ADD   DX,DR.IVE_FIRST_FAT_SECTOR
         MOV   ABS25_SECTOR_LOW,DX      ;  sector
         MOV   ABS25_SECTOR_HIGH,0      ;
         MOV   CX,2
         MOV   ABS25_SECTOR_COUNT,CX    ;  (read jus wun)
         CALL  ABS_25

GET_RIGHT_OUT:
         POP   DX
         POP   CX
         POP   AX
         RET

GET_RIGHT_SECTOR ENDP



GET_FAT  PROC  NEAR
         PUSH  BX
         PUSH  CX
         PUSH  DI
         MOV   BX,FAT_AREA_PTR
         MOV   START_CLU,AX
         MOV   DI,AX              ; pick up cluster number
         TEST  DR.IVE_SWITCH,FAT_IZ_16_BITS
         JNZ   GET_FAT_16
         SHR   DI,1               ; divide by 2
         ADD   DI,AX              ; equals times 1.5

         MOV   DX,0
         MOV   AX,DI

         CALL  GET_RIGHT_SECTOR
         MOV   AX,[BX+DI]         ; pick up next
         TEST  START_CLU,1        ; is the sector number odd?
         JZ    GF_1               ; no, branch
         MOV   CL,4
         SHR   AX,CL
GF_1:
         AND   AX,0FFFH           ; clear hi-order byte
         CMP   AX,0FF7H
         JB    GF_EXIT
         OR    AX,0F000H

GF_EXIT:
         POP   DI
         POP   CX
         POP   BX
         RET

GET_FAT_16:
         MOV   AX,DI
         MUL   TWO

         CALL  GET_RIGHT_SECTOR
         MOV   AX,[BX+DI]
         JMP   GF_EXIT

GET_FAT  ENDP



CHECK_FOR_BIG_HARD_DRIVE PROC NEAR

; see if we have over 32 meg hard drive support (like COMPAQ DOS 3.31)

         PUSH  DS
         AND   DRIVE_INFO,NOT ABOVE_32_MEG
         CMP   DOS_VERSION,031FH        ; less than 3.31?
         JAE   CFBHD_CONT_1             ; nope, branch
         CMP   DOS_VERSION,031EH        ; equal to 3.30?
         JB    CFBHD_EXIT               ; no, branch
         CMP   DOS_VERSION_BH,5         ; ZENITH DOS 3.30?
         JE    CFBHD_CONT_1             ; yup, branch

         JMP   SHORT CFBHD_EXIT

CFBHD_CONT_1:
         MOV   AX,[BX]+13              ; get highest valid clu #
         DEC   AX                      ; -1 since 1st is clu 2
         MOV   CH,0
         MOV   CL,[BX]+4               ; get sectors/clu -1
         INC   CX
         MUL   CX
         ADD   AX,[BX]+11              ; add first data sector #
         ADC   DX,0

         CMP   DX,0
         JE    CFBHD_EXIT

         OR    DRIVE_INFO,ABOVE_32_MEG  ; so note

CFBHD_EXIT:
         POP   DS

         RET

CHECK_FOR_BIG_HARD_DRIVE ENDP



UPPERCASE PROC NEAR     ;Convert AL to upper case.
         CMP   AL,'a'
         JB    UC
         CMP   AL,'z'
         JA    UC
         SUB   AL,20H

UC:
         RET

UPPERCASE ENDP



;----------------------------------------------------------------
; This proc is here really only because we hook INT 19h to help
; prevent dangerous attempts to improperly Unload the tracker.
; Since many programs hook INT 19h, this helps enforce
; the 'First-intalled, Last-removed' pseudo-rule.
; Added 02-21-90 GWD, v6.   Restore old vector 02-23-90.

INT19_SERVICE PROC FAR          ;Bootstrap.
         cli
         mov   ax,cs
         mov   ds,ax
         xor   ax,ax
         mov   es,ax           ;ES=0.
         cld
         mov   di,4*19h
         lea   si,saved19
         movsw                 ; Restore 19h (the only Bios vector we hook).
         movsw
         mov   di,4*2Fh
         lea   si,saved2F
         cmp   word ptr ds:[si+2],-1   ;Vector 2F was hooked?
         je    int19_x                 ;No.
         movsw                           ;Restore old vector 2Fh.
         movsw

int19_x:
         DB    0EAh            ; JMP FAR xxxx:xxxx (to old handler).
saved19  DW    0,-1

INT19_SERVICE ENDP



;----------------------------------------------------------------
; This dummy hooks INT 2Fh for reasons similar to int19_service.
; 02-23-90 GWD.

INT2F_SERVICE PROC FAR
         PUSHF
         PUSH  AX

         CMP   AX,1605H                 ; Windows coming up?
         JNE   INT2F_CONT

         MOV   AX,0
         CMP   BX,AX
         JNE   INT2F_EXIT
         CMP   CX,AX
         JNE   INT2F_EXIT
         CMP   SI,AX
         JNE   INT2F_EXIT
         MOV   AX,ES
         CMP   AX,0
         JNE   INT2F_EXIT
         MOV   AX,DS
         CMP   AX,0
         JNE   INT2F_EXIT

         OR    INSTALL_SWITCH,WINDOWS_PRESENT
         JMP   SHORT INT2F_EXIT

INT2F_CONT:
         CMP   AX,1606H                 ; Windows going down?
         JNE   INT2F_EXIT

         AND   INSTALL_SWITCH,NOT WINDOWS_PRESENT

INT2F_EXIT:
         POP   AX
         POPF
         DB    0EAh            ; JMP FAR xxxx:xxxx
saved2F  DW    0,-1            ;Seg = -1 means not hooked yet.

INT2F_SERVICE ENDP



;---------------------------------------------------------
; Fix the age-old problem with DOS INT 25h/26h.
;
; DOS saves the user's stack pointer into an
; internal DWORD variable upon entry at INT 25h or 26h.
; However, DOS uses that same DWORD to save the user's
; stack pointer during INT 21h fct# 00 - 0Ch !
; This can cause bad problems for TSR's which
; wake-up during DOS console input.
;
; So, we hook INT 25h to save & restore that DWORD.
;
; Added by GWD, 12-2-88, to MIRROR v5.00
;
;----------------------------------------------------------
; Note that the special value of SAVED_25+2 = -1 prevents
; the hookup procedure from modifying the vectors.

        EVEN
saved25         DW      0,-1    ;Saved vectors.
saved26         DW      0,-1

;----------------------------------------------------------
; Addresses of the DWORDS where DOS saves the user's SS:SP.
; Segments will always be the same.

dos_var1_addr   DW      0,-1
dos_var2_addr   DW      0,-1

;----------------------------------------------------------
saved_dos_var   DW      ?,?

black_hole      DW      ?

;----------------------------------------------------------
; Our resident patch-fixes for INT 25h and INT 26h.

INT25_PATCH PROC FAR
         nop
         call  swap_dos_var
         pushf
         call  dword ptr cs:saved25 ;Note - returns with flags still stacked.
         jmp   short int_25_26_back

INT26_PATCH PROC FAR
         nop
         call  swap_dos_var
         pushf
         call  dword ptr cs:saved26

int_25_26_back:
         pop   cs:black_hole   ;Discard unpopped flags.
         call  swap_dos_var
         ret                   ;Leave flag word on stack, like DOS does.
INT26_PATCH ENDP
INT25_PATCH ENDP



;---------------------------------------------------------
; Written as Swap, rather than Save/Restore, so
; that only a single procedure is needed (smaller code).
;
; All regs & flags are preserved.

SWAP_DOS_VAR PROC NEAR
         pushf
         cli
         push  ax
         push  bx
         push  ds
         lds   bx,dword ptr cs:dos_var1_addr
         mov   ax,ds:[bx]
         xchg  ax,cs:saved_dos_var
         mov   ds:[bx],ax
         lds   bx,dword ptr cs:dos_var2_addr
         mov   ax,ds:[bx]
         xchg  ax,cs:saved_dos_var+2
         mov   ds:[bx],ax
         pop   ds
         pop   bx
         pop   ax
         popf
         ret

SWAP_DOS_VAR ENDP

        DB      0

; =======  This is the end of the resident code for Delete-Tracking.  ======

DOS_INTERCEPT_END LABEL BYTE


;------------------------------------------------------------
; These are the first few bytes of DOS INT 25h/26h
; service routines.  We will compare these.

pattern1 DB    0FAh                    ;CLI
pattern2 DB    2Eh, 8Ch, 16h, 0,0      ;MOV CS:xxxx,SS
pattern3 DB    2Eh, 89h, 26h, 0,0      ;MOV CS:yyyy,SP

; The following four bytes are at offset +1 of the INT 25h
; pre-handler for Compaq DOS 3.31 and Zenith 3.30+.

pattern4 DB    83h, 0F9h, 0FFh, 74h    ;CMP CX,-1 then a JZ xx (pattern5).
         DB    0

; The following three bytes are where the JZ xx above point to.
; These do not immediately follow pattern4 in DOS.

pattern5 DB    2Eh, 0FFh, 2Eh          ;JMP DWORD PTR CS:[xxxx]
         DB    0,0


dos_code STRUC
dos_1   DB      ?
dos_2   DB      3 DUP (?)
dos_offset1 DW  ?
dos_3   DB      3 DUP (?)
dos_offset2 DW  ?
dos_code ENDS

        ASSUME  DS:CODE         ;This is the way Glen's code works.

;-----------------------------------------------------------
; Input: ES:BX contain the original 25h or 26h vector.  DS=CS.
;
; If patch is possible then CF=false and
; ES:SI and ES:DI point at the two DOS words.
;
; Otherwise, CF=true.
;
; Destroys AX,BX,CX,SI,DI,ES.

CHECK_25_26 PROC NEAR
         cld
         lea   si,pattern4
         lea   di,[bx+1]
         mov   cx,4
         repe  cmpsb
         jne   chk_first       ;It's not a kluge Compaq DOS 3.31, et al.
         mov   al,es:[di]      ;The offset byte of the JZ opcode.
         inc   di
         cbw
         add   di,ax           ;Compute the target of the JZ opcode.
         lea   si,pattern5
         mov   cl,3
         repe  cmpsb
         jne   chk_cannot_patch
         mov   di,es:[di]      ;Fetch offset from JMP opcode.
         les   bx,es:[di]      ;Fetch DWORD.

chk_first:
         mov   di,bx
         lea   si,pattern1
         cmpsb
         jne   chk_cannot_patch
         push  di
         mov   cx,3            ;Pattern2 follows #1, so SI & DI are OK.
         repe  cmpsb
         pop   di              ;Recover DI, since compare may have failed.
         je    chk_third       ;Matched!  Now see if other is OK, too.
         lea   si,pattern3     ;No match, try alternate.
         mov   cl,3
         repe  cmpsb
         je    chk_third
         jmp   SHORT chk_cannot_patch

chk_third:
         lea   si,pattern2
         lea   di,[bx].dos_3
         push  di
         mov   cl,3
         repe  cmpsb
         pop   di
         je    chk_can_patch
         lea   si,pattern3
         mov   cl,3
         repe  cmpsb
         je    chk_can_patch

chk_cannot_patch:
         stc
         jmp   short chk_exit

chk_can_patch:
         mov   si,es:[bx].dos_offset1
         mov   di,es:[bx].dos_offset2
         clc

chk_exit:
         ret

CHECK_25_26 ENDP



;----------------------------------------------------------
; See if we can (or should) patch INT 25h & 26h.  GWD.
;
; On exit:
;       If yes, then the current vectors are saved and CF=false.
;       If they cannot be patched, then CF=true.
;
; All regs preserved, only flags are changed.

MAYBE_PATCH PROC NEAR
         push  ax
         push  bx
         push  cx
         push  dx
         push  si
         push  di
         push  ds
         push  es
         mov   ax,cs
         mov   ds,ax           ;Set DS=CS.
         nop
         mov   ax,3525h        ;Ask DOS for the current vector.
         int   21h             ;Returns ES:BX.
         mov   saved25,bx
         mov   saved25+2,es    ;Save it, even though we might not hook it.
         call  check_25_26
         jc    maybe_not
         mov   dos_var1_addr,si
         mov   dos_var1_addr+2,es
         mov   dos_var2_addr,di
         mov   dos_var2_addr+2,es
         mov   ax,3526h        ;Ask DOS for the current vector.
         int   21h             ;Returns ES:BX.
         mov   saved26,bx
         mov   saved26+2,es
         call  check_25_26
         jc    maybe_not
         mov   ax,es
         cmp   ax,dos_var1_addr+2
         jne   maybe_not
         clc
         jmp   short maybe_end

maybe_not:
         stc

maybe_end:
         pop   es
         pop   ds
         pop   di
         pop   si
         pop   dx
         pop   cx
         pop   bx
         pop   ax
         ret

MAYBE_PATCH ENDP



;-------------------------------------------------
; Substitute our handlers on INT 25h & 26h.
; Destroys AX,DX.

HOOK_INT_25_26 PROC NEAR
         push  ds
         push  cs
         pop   ds
         nop
         cmp   saved25+2,-1    ;Have the vectors been saved?
         je    hook_i25_end    ;No.  We're skipping the hook.
         lea   dx,int25_patch
         mov   ax,2525h        ;Ask DOS to change the vector.
         int   21h
         lea   dx,int26_patch
         mov   ax,2526h
         int   21h

hook_i25_end:
         pop   ds
         ret

HOOK_INT_25_26 ENDP

         ASSUME DS:nothing      ;Put it back the way Jim likes it.

INCL_DELTRACK EQU $
         INCLUDE MIR_MSG.INC

COMMAND_LINE_CHECK_SWITCH DB 0
GOTTA_DRIVE_LETTER        EQU 80H
GOTTA_UNLOAD_LETTER       EQU 40H
GOTTA_PARTN_LETTER        EQU 20H
GOTTA_TRACK_LETTER        EQU 10H

GOTTA_ONE_LETTER		  EQU 01H		;for /1 switch ;M001
TRACK_DIGIT_COUNT DB 0

PHYS_DRV DW    0

C_E_H_DWORD DD 0
         ORG   $-4
C_E_H_OFF   DW 0
C_E_H_SEG   DW 0

STACK_SEGMENT_SAVE DW 0
STACK_POINTER_SAVE DW 0
ALLOC_STRAT        DB 0
UMB_LINK_STAT      DB 0
UMB_SWITCH         DB 0
OK_TO_SEARCH_UMBS  EQU 80H

PARAMETER DW   0
          DW   80H
PARM_CS1  DW   0
          DW   PARM_FCB
PARM_CS2  DW   0
          DW   PARM_FCB
PARM_CS3  DW   0

PARM_FCB  DB   0FFH,35 DUP(0)



INIT_RES PROC  FAR
         EXTRN STAK_END:BYTE
         EXTRN MIRROR:NEAR

; *** This is the installation code for Delete Tracking. ***

        nop
        nop
        nop

START:
         LEA   SP,STAK_END
         PUSH  DS                       ; DS to stack
         SUB   AX,AX
         PUSH  AX                       ; zero return

         CALL  GET_DOS_VERSION

         MOV   CS_SAVE,CS
         CMP   CS_SAVE,0A000H
         JB    START_CONT
;M003
; If we are loaded in UMBs and this field is set, then just install the
;tracker, otherwise do everything. This field is set when Mirror tries to
;do an automatic load into UMBs. The way Mirror does an automatic load into
;UMBs is by execing mirror again and setting this field to 0ffh.
;
	cmp	byte ptr cs:[5ch],0ffh	;M003
	jne	start_cont_1		;M003

         JMP   SHORT JUST_INSTALL

START_CONT:
         CMP   BYTE PTR CS:[5CH],0FFH   ; trying to load hi?
         JE    INIT_EXIT                ; forget all about it!

start_cont_1:				;M003
         LEA   DX,MSG_MAIN_TITLE
         MOV   AH,9
         DOSEXEC

         CALL  VALIFY_COMMAND_LINE
         JNC   START_PARMS_OK
         JMP   BAD_PARMS

START_PARMS_OK:
         PUSH  DS
         CALL  SAVE_PARTITION           ;Returns here, or TERMINATES to DOS!
         POP   DS

         CALL  FIRE_UP_MIRROR

         CALL  FIND_SLASH_TEE
         TEST  INSTALL_SWITCH,GOT_SLASH_TEE
         JZ    INIT_EXIT

         LEA   DX,MSG_TRACKER_TITLE
         MOV   AH,9
         DOSEXEC

         CALL  CHK_DTRK_RESIDENT
         JNC   SETUP_DRIVES

         LEA   DX,ALREADY_MSG           ; give 'em
         MOV   AH,9                     ;  bad
         DOSEXEC                        ;   news

INIT_EXIT:
         RET

SETUP_DRIVES:

; New residency checking method, requested by Jim 12-8-88.  GWD.

         xor   bx,bx           ;Function = residency test.
         xor   cx,cx           ;Pre-clear CX.
         mov   ax,0FFA3h       ; check for our new program
         int   16h
         cmp   cx,5555h
         jne   test_for_shell
         cmp   dx,cx
         je    tools_is_resident

test_for_shell:
         xor   bx,bx           ;Function = residency test.
         xor   cx,cx           ;Pre-clear CX.
         mov   ax,0FFDDh       ;Tom's method of checking PCSHELL residency.
         int   16h
         cmp   cx,5555h
         jne   test_for_desktop
         cmp   dx,cx
         je    tools_is_resident

test_for_desktop:
         xor   cx,cx
         mov   ax,0FFEFh       ;Tom's method of checking DESKTOP residency.
         int   16h
         cld
         sti
         cmp   cx,0ABCDh
         jne   tools_not_resident

tools_is_resident:
         LEA   DX,TOOLS_ALREADY_MSG     ; give 'em
         MOV   AH,9                     ;  bad
         DOSEXEC                        ;   news
         RET                            ;Terminate.

TOOLS_NOT_RESIDENT:
JUST_INSTALL:
         CMP   DOS_VERSION,0300H
         JB    NOT_ASSIGNED
         MOV   AX,0
         MOV   DS,AX
         CMP   WORD PTR DS:[2FH*4],0
         JNE   ASSIGNED_CHK
         CMP   WORD PTR DS:[2FH*4]+2,0
         JNE   ASSIGNED_CHK
         JMP   SHORT NOT_ASSIGNED

ASSIGNED_CHK:
         MOV   AX,0600H
         INT   2FH                      ; iz ASSIGN resident?
         CMP   AL,0FFH
         JNE   NOT_ASSIGNED
         OR    INSTALL_SWITCH,ASSIGN_RESIDENT

NOT_ASSIGNED:
         MOV   DS,CS_SAVE               ; DS=CS.  Good!
         INT   11H
         AND   AX,00C0H                 ; turn off all but # drives
         MOV   CL,6
         SHR   AX,CL
         CMP   AX,0
         JNE   GOT_AT_LEAST_TWO_PHYSICAL
         MOV   AX,1

GOT_AT_LEAST_TWO_PHYSICAL:
         MOV   PHYS_DRV,AX

         CALL  HOOK_UP_CRITICAL_ERROR
         CALL  PARSE_PARMS

         PUSHF
         CALL  UNHOOK_UP_CRITICAL_ERROR
         POPF

         JNC   SET_HOOKS

BAD_PARMS:
         LEA   DX,INVALID_PARMS         ; give 'em
         MOV   AH,9                     ;  bad
         DOSEXEC                        ;   news

INIT_NOTHIN_TEW_DEW:
         JMP   INIT_EXIT

SET_HOOKS:
         MOV   AX,IO_AREA_PTR
         ADD   AX,LARGEST_SECTOR
         MOV   FAT_AREA_PTR,AX

         CALL  TRY_TO_LOAD_US_HIGH
         JNC   INIT_NOTHIN_TEW_DEW

         CMP   DOS_VERSION,0500H
         JNB   NO_HOOKING_ALLOWED

         call  maybe_patch     ;Checks if we can do the INT 25h/26h fix.
         jnc   no_hook25_err   ;Yes we can.

         lea   dx,msg_nohook_25 ;"WARNING - can't hook INT 25h.  TSR danger."
         mov   ah,9
         int   21h

NO_HOOKING_ALLOWED:
         mov   ax,-1
         mov   saved25+2,ax    ;Set segments to FFFF to prevent hooking.
         mov   saved26+2,ax

no_hook25_err:
         TEST  INSTALL_SWITCH,GOT_SUMTHIN_TO_TRACK
         JZ    INIT_NOTHIN_TEW_DEW

         CALL  DISPLAY_RESULTS

         lea   dx,iret_for_23           ; GWD 2-19-90.
         mov   ax,2523h                 ; Take Ctrl-C vector to ignore it.
         dosexec

         MOV   AX,3521H                 ;  get vector of DOS int handler
         DOSEXEC
         MOV   DOS_SEG,ES               ; save
         MOV   DOS_OFF,BX               ; save


         LEA   DX,DOS_INTERCEPT         ;
         MOV   AX,2521H                 ; set vector of DOS INT 21h handler
         DOSEXEC

         call  hook_int_25_26          ;Conditionally, this hooks them.

 ; Added INT 19h stuff 02-21-90, GWD.  v6.
         mov   ax,3519h                ;Fetch INT 19h (bootstrap) vector.
         dosexec
         mov   saved19,bx
         mov   saved19+2,es
         lea   dx,int19_service
         mov   ax,2519h                ;Hook it.
         dosexec

 ; Added INT 2Fh  02-23-90 GWD.  v6
         cmp   dos_version,300h
         jb    skip_2F_hook
         mov   ax,352Fh                ;Fetch INT 2Fh DOS multiplex vector.
         dosexec
         mov   saved2F,bx
         mov   saved2F+2,es
         lea   dx,int2F_service
         mov   ax,252Fh                ;Hook it.
         dosexec

         MOV   AH,34H
         DOSEXEC
         MOV   IN_DOS_OFFSET,BX
         MOV   IN_DOS_SEGMENT,ES

skip_2F_hook:
         LEA   DX,INSTALLED_MSG         ; show 'em we are installed.
         MOV   AH,9
         DOSEXEC

	cmp	dos_version,400h	;Old DOS?
	jb	skip_env_free		;Leave it, so mem map shows TSR name.
         MOV   AH,62H			;Get PSP.
         DOSEXEC
         MOV   ES,BX
         MOV   ES,ES:[2CH]
         MOV   AH,49H
         DOSEXEC                        ; free enviroment
skip_env_free:

         MOV   DX,FAT_AREA_PTR
         ADD   DX,LARGEST_SECTOR
         ADD   DX,LARGEST_SECTOR
         ADD   DX,15
         MOV   CL,4
         SHR   DX,CL
         MOV   AX,3100H                 ; to termimate and stay resident
         DOSEXEC

INIT_RES ENDP



GET_DOS_VERSION PROC NEAR
        MOV     AX,3306H                ; get true DOS version
        MOV     BX,0                    ;   ala DOS 5.0
        INT     21H
        CMP     BL,5                    ; function supported?
        JB      VERSION_OLD_WAY
        MOV     AX,BX
        JMP     SHORT STORE_DOSVER

VERSION_OLD_WAY:
         MOV   AH,30H
         DOSEXEC

STORE_DOSVER:
         XCHG  AH,AL
         MOV   DOS_VERSION,AX
         MOV   DOS_VERSION_BH,BH
         RET

GET_DOS_VERSION ENDP



TRY_TO_LOAD_US_HIGH PROC NEAR
         CMP   DOS_VERSION,500H
         JB    T_T_L_U_H_STC_JMP
         CMP   CS_SAVE,0A000H
         JB    T_T_L_U_H_CONT

T_T_L_U_H_STC_JMP:
         JMP   T_T_L_U_H_STC

         ; Save original values

T_T_L_U_H_CONT:
         MOV   AX,5800H
         DOSEXEC                        ; get allocation strategy
         MOV   ALLOC_STRAT,AL

         MOV   AX,5802H
         DOSEXEC                        ; get UMB status
         MOV   UMB_LINK_STAT,AL

         ; Install new values

         MOV   AX,5803H
         MOV   BX,1                     ; link UMBs
         DOSEXEC
         JC    NO_UMBS_JMP

         MOV   AX,5801H
         MOV   BL,80H                   ; request high first, first fit
         DOSEXEC
         JNC   GOT_UMBS_LINKED

NO_UMBS_JMP:
         JMP   NO_UMBS

         ; Get an area and free it

GOT_UMBS_LINKED:
         MOV   BX,FAT_AREA_PTR
         ADD   BX,LARGEST_SECTOR
         ADD   BX,LARGEST_SECTOR
         ADD   BX,15
         MOV   CL,4                     ; get # paras needed for
         SHR   BX,CL                    ; resident portion of del track
         INC   BX                       ; include arena header
         MOV   ES,CS_SAVE
         MOV   CX,ES:[2CH]              ;
         DEC   CX                       ; point to environment arena
         MOV   ES,CX
         ADD   BX,ES:[0].MCB_PARAS      ; add size of environment

         MOV   AH,48H                   ; get worse case amount
         DOSEXEC
         JC    NO_UMBS

         MOV   ES,AX
         MOV   AH,49H
         DOSEXEC

         MOV   AX,ES
         CMP   AX,0A000H
         JB    NO_UMBS

         ; Attempt to load it high

         MOV   BX,0
         MOV   DS,BX
         PUSH  DS:[21H*4]+2              ; save current INT 21 vector

         MOV   STACK_SEGMENT_SAVE,SS
         MOV   STACK_POINTER_SAVE,SP

         MOV   PARM_CS1,CS
         MOV   PARM_CS2,CS
         MOV   PARM_CS3,CS

         MOV   AX,4B00H
         MOV   DS,CS_SAVE
         CALL  POINT_DS_DX_TO_ASCIIZ
         JB    NO_UMBS

         MOV   ES,CS_SAVE
         LEA   BX,PARAMETER
         DOSEXEC

         CLI
         MOV   SS,STACK_SEGMENT_SAVE
         MOV   SP,STACK_POINTER_SAVE
         STI
         POP   AX                       ; recover saved vector

         JC    NO_UMBS

         MOV   BX,0
         MOV   DS,BX
         CMP   DS:[21H*4]+2,AX           ; if changed, we loaded
         JE    DIDNT_LOAD
         CLC
         JMP   SHORT NO_UMBS

DIDNT_LOAD:
         STC


         ; Restore original values

NO_UMBS:
         MOV   DS,CS_SAVE
         PUSHF
         MOV   AX,5803H
         MOV   BH,0
         MOV   BL,UMB_LINK_STAT
         DOSEXEC

         MOV   AX,5801H
         MOV   BL,ALLOC_STRAT
         DOSEXEC

         POPF
         RET

T_T_L_U_H_STC:
         STC
         RET

TRY_TO_LOAD_US_HIGH ENDP



POINT_DS_DX_TO_ASCIIZ PROC NEAR
         PUSH  AX
         PUSH  CX
         PUSH  DI
         PUSH  ES

         MOV   ES,CS_SAVE
         MOV   DS,ES:[2CH]
         MOV   ES,ES:[2CH]
         SUB   AX,AX
         SUB   DI,DI
         MOV   CX,32767

P_D_T_A_LOOP:
         JCXZ  P_D_T_A_STC
         REPNE SCASB                    ; SCAN FOR A BYTE OF ZEROS
         CMP   ES:[DI][-1],AX           ; WAS IT A WORD OF ZEROS
         JNE   P_D_T_A_LOOP             ; NO - SCAN MORE ENVIRONMENT

         ADD   DI,3                     ; bump past werd and binzer
         MOV   DX,DI
         CLC
         JMP   SHORT P_D_T_A_EXIT

P_D_T_A_STC:
         STC

P_D_T_A_EXIT:
         POP   ES
         POP   DI
         POP   CX
         POP   AX
         RET

POINT_DS_DX_TO_ASCIIZ ENDP



FIRE_UP_MIRROR PROC NEAR
         MOV   ES,CS_SAVE
         LEA   BX,END_PROG
         ADD   BX,15
         MOV   CL,4
         SHR   BX,CL                    ; shrink our allocated
         MOV   AH,4AH                   ; memory so MIRROR can
         INT   21H                      ; allocate ala DOS

         MOV   AX,0FFFFH                ; so MIRROR will know its
                                        ; coming from DTRK instead
                                        ; of MSDOS FORMAT which uses
                                        ; 5050H in AX and the zero based
                                        ; drive letter in BL
         CALL  MIRROR

         MOV   BX,0FFFFH
         MOV   AH,4AH
         INT   21H                      ; we expect this to fail and
         JNC   F_U_M_RET                ; BX will have the max size

         MOV   AH,4AH                   ; reclaim all our space
         INT   21H

F_U_M_RET:
         RET

FIRE_UP_MIRROR ENDP



IRET_FOR_23:
         IRET



;-------------------------------------
; New method in v6.0, 02-16-90 GWD.
; Improved 04-26-90 GWD.
;
; On exit: if Mirror Delete-tracking is already resident then
;          CF=true and AX = the resident code segment.
;          If it's not resident then CF=false and AX=0.
;          But if we can't determine which, then CF=true and AX=0.
;
;          Therefore, CF=false means we can install resident.
;          AX > 0 means we maybe can unload the resident module.
;
; Only AX is modified.

mcb_struc STRUC
mcb_signature   DB      ?       ;4Dh or 5Ah (last).
mcb_owner       DW      ?       ;Segment of owner block (1 + that MCB).
mcb_paras       DW      ?       ;Paragraphs in the block (excluding the MCB).
                DB      3 DUP(?)
MCB_NAME        DW      4 DUP(?)
mcb_struc ENDS

CHK_DTRK_RESIDENT PROC NEAR
         push  bx
         push  cx
         push  dx
         push  si
         push  di
         push  ds
         push  es

         MOV   UMB_SWITCH,0
         CMP   DOS_VERSION,500H
         JB    C_D_R_INIT

         MOV   UMB_LINK_STAT,0FFH
         MOV   AX,5802H
         DOSEXEC
         JC    C_D_R_INIT

         MOV   UMB_LINK_STAT,AL
         OR    UMB_SWITCH,OK_TO_SEARCH_UMBS

C_D_R_INIT:
         mov   bx,-1
         mov   ah,52h          ;'Undoc' call: Get ptr to DOS internal vars.
         int   21h
         cmp   bx,-1           ;This should never happen, but
         JE    CHK_DTRK_ERR_JMP ;if it does, we'll refuse to install.

         mov   dx,es:[bx-2]    ;Fetch segment of first Memory Control Block.
         push  cs
         pop   ds
         cld
         sti                   ;In case we hang, allow warm boot.
         xor   bx,bx
         jmp   SHORT chk_dtrk_next

CHK_DTRK_ERR_JMP:
         JMP   SHORT CHK_DTRK_ERR

chk_dtrk_lp:
         mov   es,dx           ;Seg of the MCB.
         cld
         mov   cx, es:[bx].mcb_owner
         mov   ax,dx
         inc   ax
         cmp   ax,cx           ;Owner = Self?  (Is this a PSP segment?)
         jne   chk_dtrk_next   ;No.
         mov   es,ax           ;Segment of block itself (MCB+1).
         cmp   word ptr es:[bx+0], 20CDh       ;1st word of a PSP = 'INT 20h'.
         jne   chk_dtrk_next
         mov   cx,cs
         cmp   ax,cx           ;Is the segment = our CS ?
         je    chk_dtrk_next   ;No point in comparing against ourself!

; Found a resident program.  Check if it is MIRROR.

         mov   cx,org_id_length
         lea   si,$entry       ;M006; Start of COM file.
         mov   di,si
         repe  cmpsb
         je    chk_dtrk_found  ;Yes, resident delete-tracker & same version!

; Not an exact match, but maybe an old version?

         mov   cx,org_id2_length
         lea   si,org_id2
         mov   di,si
         repe  cmpsb
         je    chk_dtrk_err    ;An old version!  Cannot load or unload.

chk_dtrk_next:
         mov   es,dx
         cld
         mov   al, es:[bx].mcb_signature
         cmp   al,5Ah
         je    chk_dtrk_no     ;Last block.
         cmp   al,4Dh
         jne   chk_dtrk_err    ;Bad sig.
         inc   dx
         add   dx, es:[bx].mcb_paras   ;Calculate the next MCB.
         jmp   chk_dtrk_lp

chk_dtrk_found:
         mov   ax,dx
         inc   ax              ;Return segment of resident module.
         stc                     ;Prevent installation.
         jmp   short chk_dtrk_99

chk_dtrk_no:
         TEST  UMB_SWITCH,OK_TO_SEARCH_UMBS
         JZ    CHK_DTRK_NONE

         AND   UMB_SWITCH,NOT OK_TO_SEARCH_UMBS
         INC   DX
         ADD   DX,ES:[BX].MCB_PARAS    ;Calculate the next MCB.
         MOV   ES,DX
         CMP   ES:[BX].MCB_OWNER,8      ; is it UMB start?
         JNE   CHK_DTRK_NONE
         CMP   ES:[BX].MCB_NAME,'CS'    ; is it UMB start?
         JE    CHK_DTRK_LP

chk_dtrk_NONE:
         xor   ax,ax           ;AX=0 (not resident), CF=false (OK to install).
         jmp   short chk_dtrk_99

chk_dtrk_err:
         xor   ax,ax           ;Unknown resident location (cannot unload).
         stc                     ;Prevent installation, too.

chk_dtrk_99:
         PUSH  AX
         PUSHF

         CMP   DOS_VERSION,500H
         JB    C_D_R_EXIT

         CMP   UMB_LINK_STAT,0FFH
         JB    C_D_R_EXIT

         MOV   AX,5803H
         MOV   BH,0
         MOV   BL,UMB_LINK_STAT
         DOSEXEC

C_D_R_EXIT:
         POPF
         POP   AX
         pop   es
         pop   ds
         pop   di
         pop   si
         pop   dx
         pop   cx
         pop   bx
         ret

CHK_DTRK_RESIDENT ENDP



VALIFY_COMMAND_LINE PROC NEAR
         MOV   DS,CS_SAVE
         MOV   SI,81H                   ; point to command line
         MOV   CL,[SI-1]                ; get character count
         MOV   CH,0
         JCXZ  V_C_L_CLC_JMP_1          ; if none, exit

         MOV   AL,'?'
         MOV   DI,SI
         CLD
         REPNE SCASB
         MOV   CL,[SI-1]                ; get character count
         JNE   V_C_L_LOOP
         JMP   V_C_L_CLC

V_C_L_LOOP:
         CALL  FIND_NEXT_NON_BLANK
         JC    V_C_L_CLC_JMP_1          ; nothing but blanks, etc

         CMP   AL,'/'                   ; slash parm?
         JE    V_C_L_CHK_SLASH

V_C_L_FIND_COLON:
         JCXZ  V_C_L_STC_JMP_1          ; if no colon, error
;M006
         CALL  CheckDrive               ; Valid drive?
	 JC    V_C_L_STC_JMP_1	        ;  -no, jump.
;M006
         LODSB                          ; get next character
         DEC   CX
         CMP   AL,':'                   ; is it a colon?
         JNE   V_C_L_STC_JMP_1
         OR    COMMAND_LINE_CHECK_SWITCH,GOTTA_DRIVE_LETTER

         JCXZ  V_C_L_CLC_JMP_1
         LODSB                          ; get next character
         DEC   CX
         CMP   AL,'/'                   ; if slash, branch
         JE    V_C_L_CHK_SLASH          ;
         CMP   AL,' '                   ;*
         JE    V_C_L_LOOP               ;
         CMP   AL,13                    ; bypass white space
         JE    V_C_L_LOOP               ;
         CMP   AL,9                     ;
         JE    V_C_L_LOOP               ;*

V_C_L_STC_JMP_1:
         JMP   V_C_L_STC

V_C_L_CLC_JMP_1:
         JMP   V_C_L_CLC

V_C_L_CHK_SLASH:
         JCXZ  V_C_L_STC_JMP_1

         LODSB                          ; get parameter itself
         DEC   CX
;M001
; We do not check for the /1 switch. Do the check before we change to
;uppercase
;
	cmp	al, '1'	; is it /1? ;M001
	je	v_c_l_one	; yes ;M001

         AND   AL,NOT 20H
         CMP   AL,'U'
         JE    V_C_L_UNLOAD
         CMP   AL,'T'
         JE    V_C_L_TRACK
         CMP   AL,'P'
         JE    V_C_L_PARTN
         JMP   V_C_L_STC

;M001 -- Begin changes
; We have found a /1. Do some validity checking on it. It is not valid if a
;/partn or /u switch has been detected
; 
v_c_l_one:
	test	command_line_check_switch,GOTTA_UNLOAD_LETTER+GOTTA_PARTN_LETTER
	jnz	v_c_l_stc_jmp_1

	or	command_line_check_switch,GOTTA_ONE_LETTER
	jmp	v_c_l_loop
;
;M001 -- End changes
;

V_C_L_UNLOAD:
         CMP   COMMAND_LINE_CHECK_SWITCH,0
         JNE   V_C_L_STC_JMP_1
         OR    COMMAND_LINE_CHECK_SWITCH,GOTTA_UNLOAD_LETTER

         CALL  FIND_NEXT_NON_BLANK
         JC    V_C_L_CLC_JMP_1          ; nothing but blanks, etc
         JMP   V_C_L_STC

V_C_L_PARTN:
         CMP   COMMAND_LINE_CHECK_SWITCH,0
         JNE   V_C_L_STC_JMP_1
         OR    COMMAND_LINE_CHECK_SWITCH,GOTTA_PARTN_LETTER

         CMP   CX,4                     ; gotta have 4 more for ARTN
         JB    V_C_L_STC_JMP_1

         LODSW
         AND   AX,NOT 2020H
         CMP   AX,'RA'
         JNE   V_C_L_STC_JMP_1
         LODSW
         AND   AX,NOT 2020H
         CMP   AX,'NT'
         JNE   V_C_L_STC_JMP_1

         SUB   CX,4

         CALL  FIND_NEXT_NON_BLANK
         JC    V_C_L_CLC_JMP_1          ; nothing but blanks, etc
         JMP   V_C_L_STC

V_C_L_TRACK:
         TEST  COMMAND_LINE_CHECK_SWITCH,GOTTA_UNLOAD_LETTER+GOTTA_PARTN_LETTER
         JNZ   V_C_L_STC_JMP_1

         OR    COMMAND_LINE_CHECK_SWITCH,GOTTA_TRACK_LETTER

         JCXZ  V_C_L_STC
         LODSB
         DEC   CX
;M006
         CALL  CheckDrive               ; Valid drive?
	 JC    V_C_L_STC	        ;  -no, jump.
;M006
         JCXZ  V_C_L_CLC
         LODSB
         DEC   CX

         MOV   TRACK_DIGIT_COUNT,0
         CMP   AL,'-'                   ; is it a continuation of the /Td
         JE    V_C_L_TRACK_CONT         ; yes, branch

         CMP   AL,'/'
         JE    V_C_L_CHK_SLASH_JMP      ; if another parm, branch
         CMP   AL,' '                   ;
         JE    V_C_L_TRACK_OUT          ; otherwise, we better find
         CMP   AL,13                    ; some white space
         JE    V_C_L_TRACK_OUT          ;
         CMP   AL,9                     ;
         JE    V_C_L_TRACK_OUT          ;
         JMP   SHORT V_C_L_STC

V_C_L_CHK_SLASH_JMP:
         JMP   V_C_L_CHK_SLASH

V_C_L_TRACK_OUT:
         CALL  FIND_NEXT_NON_BLANK
         JC    V_C_L_CLC                ; nothing but blanks, etc
         CMP   AL,'/'                   ; next thing we find better be

;         JE    V_C_L_CHK_SLASH_JMP      ; a slash
;         JMP   V_C_L_STC

         JNE   V_C_L_STC                ; invert the check to avoid jmp to jmp
         JMP   V_C_L_CHK_SLASH

V_C_L_TRACK_CONT:
         JCXZ  V_C_L_STC
         LODSB                          ; after a dash, we need at
         DEC   CX                       ; least one digit

V_C_L_CHK_NUM:
         CMP   AL,'0'
         JB    V_C_L_STC
         CMP   AL,'9'
         JA    V_C_L_STC
         INC   TRACK_DIGIT_COUNT
         CMP   TRACK_DIGIT_COUNT,3
         JA    V_C_L_STC

         JCXZ  V_C_L_CLC
         LODSB
         DEC   CX
         CMP   AL,'/'
         JE    V_C_L_CHK_SLASH_JMP
         CMP   AL,' '
         JE    V_C_L_TRACK_OUT
         CMP   AL,13
         JE    V_C_L_TRACK_OUT
         CMP   AL,9
         JE    V_C_L_TRACK_OUT
         JMP   V_C_L_CHK_NUM

V_C_L_CLC:
;M006: If didn't find Drive Letter or /U or /PARTN, check Default Drive.

	test	command_line_check_switch,GOTTA_DRIVE_LETTER OR GOTTA_UNLOAD_LETTER OR GOTTA_PARTN_LETTER
	jnz	vclx            ;TEST sets C=0.

        mov     ah,19h
        int     21h             ;AL = drive code (0-25).
        add     al,'A'
        call    CheckDrive      ;Default drive valid?
        jnc     vclx            ; -yes, jump.
;M006
V_C_L_STC:
         STC
vclx:    RET                    ;M006.

VALIFY_COMMAND_LINE ENDP



FIND_NEXT_NON_BLANK PROC NEAR
         JCXZ  F_N_N_B_STC              ; ain't got nomore

         LODSB
         DEC   CX
         CMP   AL,' '                   ; blank
         JE    FIND_NEXT_NON_BLANK
         CMP   AL,13                    ; carriage return
         JE    FIND_NEXT_NON_BLANK
         CMP   AL,09                    ; tab
         JE    FIND_NEXT_NON_BLANK
         CLC
         RET

F_N_N_B_STC:
         STC
         RET

FIND_NEXT_NON_BLANK ENDP



PARSE_PARMS PROC NEAR
         MOV   DS,CS_SAVE
         MOV   ES,CS_SAVE
         MOV   DI,81H
         MOV   CL,[DI-1]
         MOV   CH,0
         JCXZ  P_P_CLC

P_P_LOOP:
         CLD
         MOV   AL,'/'
         REPNE SCASB
         JNE   P_P_CLC
         JCXZ  P_P_CLC

; changed 04-26-90 to use Uppercase Proc.  GWD.
         push  ax
         mov   al,[di]
         call  uppercase
         cmp   al,"T"
         pop   ax
         JE    P_P_PROCESS

         push  ax
         mov   al,[di]
         call  uppercase
         cmp   al,'S'
         pop   ax
         JNE   P_P_LOOP

         OR    INSTALL_SWITCH,DO_IT_THE_SLOW_WAY
         JMP   P_P_LOOP

P_P_PROCESS:
         CALL  VALIFY_PARM
         JC    P_P_STC
         JMP   P_P_LOOP

P_P_CLC:
         CLC
         RET

P_P_STC:
         STC
         RET

PARSE_PARMS ENDP



FIND_SLASH_TEE PROC NEAR
         MOV   DI,81H
         MOV   CH,0
         MOV   CL,[DI-1]
         JCXZ  FIND_ST_EXIT

FIND_ST_REPEAT:
         MOV   AL,'/'
         REPNE SCASB
         JCXZ  FIND_ST_EXIT
         JNE   FIND_ST_EXIT
         MOV   AL,[DI]
         CALL  UPPERCASE
         CMP   AL,'T'
         JNE   FIND_ST_REPEAT
         OR    INSTALL_SWITCH,GOT_SLASH_TEE

FIND_ST_EXIT:
         RET

FIND_SLASH_TEE ENDP



VALIFY_PARM PROC NEAR
         OR    BYTE PTR [DI],20H        ; makr parm lower case
         MOV   AL,[DI]+1                ; get drive letter
         OR    AL,20H                   ; lower case
         CMP   AL,'a'
         JB    V_P_STC
         CMP   AL,'z'
         JA    V_P_STC
         CMP   BYTE PTR [DI+2],'-'
         JE    V_P_COUNT
         MOV   AL,[DI+2]
         CALL  CHECK_FOR_TERMINATOR
         JNC   V_P_STC

         MOV   AX,0

V_P_FORCE_AUTO:
	 CALL  CONVERT_TO_DISP
         MOV   DRIVES_GIVEN_TABLE[BX],AX ; stuff it in
         OR    INSTALL_SWITCH,GOT_SUMTHIN_TO_TRACK
         CALL  FIND_SECTOR_SIZE
         JMP   SHORT V_P_CLC

V_P_COUNT:
         LEA   SI,[DI+3]
         CALL  CHECK_COUNT
         JC    V_P_STC
         JMP   V_P_FORCE_AUTO

V_P_CLC:
         CLC
         RET

V_P_STC:
         STC
         RET

VALIFY_PARM ENDP



FIND_SECTOR_SIZE PROC NEAR
         PUSH  DS
         MOV   DH,0
         CMP   AX,0FFFFH
         JE    F_S_EXIT
         MOV   AH,0
         MOV   AL,[DI+1]
         OR    AL,20H
         SUB   AL,61H
         CMP   PHYS_DRV,AX
         JNB   F_S_EXIT

         MOV   DL,AL
         INC   DL
         MOV   BL,DL

         MOV   AX,440EH
         DOSEXEC
         JC    F_S_BYPASS
         MOV   DH,AL

         MOV   AX,440FH
         DOSEXEC

F_S_BYPASS:
         MOV   AH,32H
         DOSEXEC
         CMP   AL,0FFH
         JE    F_S_EXIT

         MOV   AX,[BX+2]                ; get sector size
         CMP   AX,LARGEST_SECTOR
         JB    F_S_EXIT
         MOV   LARGEST_SECTOR,AX

F_S_EXIT:
         CMP   DH,0
         JE    F_S_LEAVE
         MOV   BL,DH
         MOV   AX,440FH
         DOSEXEC

F_S_LEAVE:
         POP   DS
         RET

FIND_SECTOR_SIZE ENDP



CHECK_FOR_TERMINATOR PROC NEAR
         CMP   AL,'/'                   ; nuther parm
         JE    C_F_T_STC
         CMP   AL,' '                   ; blank
         JE    C_F_T_STC
         CMP   AL,13                    ; carriage return
         JE    C_F_T_STC
         CMP   AL,09                    ; tab
         JE    C_F_T_STC
         CLC
         RET

C_F_T_STC:
         STC
         RET

CHECK_FOR_TERMINATOR ENDP



CONVERT_TO_DISP PROC NEAR
         PUSH  AX
         MOV   AL,[DI+1]
         OR    AL,20H
         SUB   AL,61H
         MOV   BL,AL
         MOV   BH,0
         SHL   BX,1
         POP   AX
         RET

CONVERT_TO_DISP ENDP



CHECK_COUNT PROC NEAR
         PUSH  DX
         PUSH  CX
         PUSH  BX
         MOV   CX,3
         MOV   BX,0

C_C_LOOP:
         MOV   AL,[SI]
         CALL  CHECK_FOR_TERMINATOR
         JC    C_C_TERM
         CMP   AL,'0'
         JB    C_C_STC
         CMP   AL,'9'
         JA    C_C_STC
         MOV   AX,BX
         MOV   BX,10
         MUL   BX
         MOV   BX,AX
         MOV   AL,[SI]
         SUB   AL,30H
         MOV   AH,0
         ADD   BX,AX
         INC   SI
         LOOP  C_C_LOOP
         MOV   AL,[SI]
         CALL  CHECK_FOR_TERMINATOR     ; better be a terminator
         JNC   C_C_STC

C_C_TERM:
         CMP   BX,0
         JE    C_C_STC
         MOV   AX,BX
         CLC
         JMP   SHORT C_C_EXIT

C_C_STC:
         STC

C_C_EXIT:
         POP   BX
         POP   CX
         POP   DX
         RET

CHECK_COUNT ENDP



DISPLAY_RESULTS PROC NEAR
         LEA   DX,RESULTS_PREFIX
         MOV   AH,9
         DOSEXEC

         MOV   CX,26
         MOV   BX,0

D_R_LOOP:
         CMP   DRIVES_GIVEN_TABLE[BX],0FFFFH
         JE    D_R_INC
         MOV   AX,DRIVES_GIVEN_TABLE[BX]
         CMP   AX,0
         JE    D_R_AUTO
         CALL  HEX_TO_ASCII
         LEA   DX,RESULTS_NUMBER
         JMP   SHORT D_R_PRINT

D_R_AUTO:
         LEA   DX,RESULTS_AUTO

D_R_PRINT:
         PUSH  DX
         LEA   DX,RESULTS_DRIVE
         MOV   AH,9
         DOSEXEC
         POP   DX
         DOSEXEC

D_R_INC:
         INC   RESULTS_DRIVE_NUM
         ADD   BX,2
         LOOP  D_R_LOOP
         RET

DISPLAY_RESULTS ENDP



HEX_TO_ASCII PROC NEAR
         PUSH  AX
         PUSH  BX
         PUSH  CX
         PUSH  DX
         PUSH  DI

         MOV   CX,3
         MOV   DX,0
         MOV   WORD PTR RESULTS_NUMBER,'  '

         LEA   DI,RESULTS_NUMBER+2
         MOV   BX,10

HEX_TO_ASCIIZ_DIV:
         DIV   BX                 ;PERFORM DIVISION
;        AX = QUOTIENT
;        DX = REMAINDER
         ADD   DL,30H             ;MAKE RELATIVE TO ASCII ZERO
         MOV   BYTE PTR [DI],DL   ;MOVE VALUE TO OUTPUT AREA
         DEC   DI                 ;DECREMENT OUTPUT POINTER
         MOV   DX,0               ;ADJUST AX BACK TO 32 BIT NUMBER
         CMP   AX,0
         JE    HEX_TO_ASCIIZ_EXIT
         LOOP  HEX_TO_ASCIIZ_DIV

HEX_TO_ASCIIZ_EXIT:
         POP   DI
         POP   DX
         POP   CX
         POP   BX
         POP   AX
         RET

HEX_TO_ASCII ENDP



HOOK_UP_CRITICAL_ERROR PROC NEAR
         PUSH  ES
         MOV   AX,3524H
         DOSEXEC
         MOV   C_E_H_SEG,ES
         MOV   C_E_H_OFF,BX
         POP   ES

         LEA   DX,C_E_HANDLER
         MOV   AX,2524H
         DOSEXEC
         RET

HOOK_UP_CRITICAL_ERROR ENDP



UNHOOK_UP_CRITICAL_ERROR PROC NEAR
         PUSH  DS
         LDS   BX,C_E_H_DWORD
         MOV   AX,2524H
         DOSEXEC
         POP   DS
         RET

UNHOOK_UP_CRITICAL_ERROR ENDP



C_E_HANDLER PROC FAR
         MOV   AL,0                     ; ignore all errors
         IRET

C_E_HANDLER ENDP

	BREAK <CheckDrive>

;Routine name:  CheckDrive
;
;Description:   Verifies that input drive letter is valid, checks
;		if drive is ASSIGN'd, and checks the drive's Current Directory
;		Structure (CDS) flags to determine if it is In Use, and
;		whether it is a Net, SUBST'd, or JOIN'd drive.
;
;Called Procedures:
;               INT 21H, function 52H ($GET_IN_VARS)
;
;Input:         AL = drive letter (upper or lower case)
;
;Output:        C=1 if a drive letter invalid or unused, or if drive is a
;		ASSIGN'd, NET, SUBST'd, or JOIN'd drive; otherwise C=0.
;
;Uses:          BX,DX
;
;Change History: Created  02/11/91  M006

public CheckDrive

CheckDrive PROC NEAR

ASSUME  ES:NOTHING

        SaveReg <es,ax,cx,si,di>

        call	UPPERCASE		;Convert drive letter to upper case.
        cmp     al,'A'
        jb      cd5
        cmp     al,'Z'
        ja      cd5

        call    check_network_drive     ;Net drive?
        jc      cd5                     ; -yes, jump.

        sub     al,'A'-1
        mov	ah,0			;AX = drive # (1-26).
        push    ax                      ;Save it.

        mov     ax,0601h                ;Get ASSIGN state and Segment.
        int     2fh
        cmp     al,0ffh                 ;ASSIGN installed?
        jne     cd2                     ; -no, jump.
                                        ; -yes: ES:103h -> ASSIGN drive map.

        pop     bx                      ;BX = drive # (1-26).
        mov     al,es:[bx].102h         ;AL = mapped drive # (1-26).
        cmp     bl,al                   ;Drive ASSIGN'd?
        jne     cd5                     ; -yes, jump.
        push    bx

cd2:    mov     ah,Get_In_Vars
        int     21h                     ;ES:BX -> SysInitVars structure.

        pop     ax                      ;AL = drive # (1-26).

        cmp     al,es:[bx].SYSI_NCDS    ;Drive in CDS?
        ja      cd5	                ; -No, jump.

        les     bx,es:[bx].SYSI_CDS     ;ES:BX -> Head of CDS list.
        mov     dl,curdirLen            ;DL = size of CDS entry.
        dec     al                      ;AL = drive code (0-25).
        mul     dl                      ;AX = offset of drive's CDS entry.
        add     bx,ax                   ;ES:BX -> CDS entry for drive.

; Invalid if not in use.

	test    es:[bx].CURDIR_FLAGS,CURDIR_inuse
        jz      cd5

; Invalid if Net, SUBST'd, or JOIN'd drive.

        test    es:[bx].CURDIR_FLAGS,(CURDIR_isnet OR CURDIR_local OR CURDIR_splice)
        jz	cdx                   	;TEST sets C=0.

cd5:	stc                             ;Invalid drive.
cdx:    RestoreReg <di,si,cx,ax,es>
	ret

CheckDrive ENDP


CODE     ENDS
         END

