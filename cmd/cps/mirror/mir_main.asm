; Copyright (c) 1990-1991 Central Point Software, Inc. 
; All rights reserved.
;--------------------------------------------------------
; MIRROR   10-25-88.
;
; (this does not include the Delete Tracking module)
; (the messages are in an INCLUDE-file)
;
; Added CALL to save the hard disk partition info
; to a floppy disk (in MIR_PRTN module).  v5.0	GWD.
;
; Version # is now EQUated, banner messages automatically follow it.
;
; Added code to recognize Zenith 3.30 DOS & support huge partns. 11-18-88 GWD.
;
; Reworked network checking.  5-1-89 GWD.
;
; If FAT>64k, program screws up.  Too hard to fix Jim's kluged code, so
; only added a check for sector_count>128 and fake an error.  5-1-89 GWD.
;
; Added /N option to suppress mirror.fil, etc, writing.  04-26-90 GWD.
; Removed display of main title & version (done in _DTRK).  04-26-90 GWD.
; /N is assumed (& not required) when LOADHI (et al) is used.  05-25-90 GWD.
;
;--------------------------------------------------------
;
; 01/30/91 SR	M002 -- Bug #5298 -- made message length independent
; ??/??/?? ??   M005
; 02/12/91 DLB  M006 -- Made check_network PUBLIC; changed DOS_VERSION to be
;                       EXTRN.
; 02/21/90 DLB  M008 -- Changed DOS_VERSION to be PUBLIC.
; 02/24/90 DLB  M009 -- MIRORSAV.FIL was over-writing existing files under
;                       certain conditions.  Also fixed detection of pre-
;                       existing sectors which contain MIRORSAV.FIL signature.

	PAGE  66,131



DRIVE		EQU BYTE PTR DS:[SI]+1

NUM_HEADS	EQU	WORD PTR DS:[SI]+2

LIST_COUNT	EQU	WORD PTR DS:[SI]+18
XLAT_TABLE	EQU	WORD PTR DS:[SI]+20
ENTRY_LEN	EQU	22

; used to build the sector table
FROM_LSN	EQU	WORD PTR DS:[BX]
FROM_LSN_HI	EQU	WORD PTR DS:[BX]+2
TO_LSN		EQU   WORD PTR DS:[BX]+4
TO_LSN_HI	EQU	WORD PTR DS:[BX]+6


CODE	SEGMENT PARA PUBLIC 'CODE'

INCL_MIRROR EQU $
	INCLUDE MIR_MSG.INC


	ASSUME	CS:CODE, DS:nothing

	PUBLIC  MIRROR
        PUBLIC  check_network_drive             	;M006
        PUBLIC  DOS_VERSION				;M008

K_ESCAPE	EQU	27

DOS_VERSION DW 0       			;M008: High byte is major ver. #.

RETURN_CODE DB 0
PASSED_DRIVE DB 0
BYTES_PER_CLUSTER_SHIFT_COUNT DW 0

CS_SAVE DW	0
START_OF_ROOT_OFFSET  DW 0
START_OF_ROOT_SEGMENT DW 0

SWITCH		  DB  0
ONE_FILE_ONLY	  EQU 80H
QUIET_MODE	  EQU 40H
GOTTA_PRE_CONTROL EQU 20H
GOTTA_PRE_IMAGE   EQU 10H

OLD_1BH_DWORD LABEL DWORD
OLD_1BH_OFF DW 0
OLD_1BH_SEG DW 0

OLD_23H_DWORD LABEL DWORD
OLD_23H_OFF DW 0
OLD_23H_SEG DW 0

BREAK_SWITCH DB 0
BEEN_BROKE   EQU 80H

OUR_THREE_SECTOR_SEGMENT DW 0
USE_AREA_PARAS DW 0

USE_DWORD	DD	0
	ORG	USE_DWORD
USE_POINTER	DW	0
USE_SEGMENT	DW	0
FAILURE_TYPE	DB	0		; M005;

;------------------------------------------------------


MIRROR	PROC
	PUSH   DS
	PUSH   ES
	CLD
	MOV	CS:CS_SAVE,CS
	MOV	DS,CS_SAVE
	MOV	ES,CS_SAVE
	MOV	RETURN_CODE,0
	MOV	FAILURE_TYPE,0		; M005; 
	MOV	PASSED_DRIVE,0

	CMP	AX,5050H	; is it MS-DOS FORMAT?
	JNE	MIRROR_PREP	; no, branch

	OR	SWITCH,QUIET_MODE	; yes, remember it
	ADD	BL,41H	; and save
	MOV	PASSED_DRIVE,BL 	;   the passed drive

MIRROR_PREP:
	MOV	AL,DS:[80H]	; get parm length
	MOV	AH,0
	MOV	cs:PARM_COUNT,AX

	MOV	AX,3306H	; get true DOS version
	MOV	BX,0	;   ala DOS 5.0
	INT	21H
	CMP	BL,5	; function supported?
	JB	VERSION_OLD_WAY
	MOV	AX,BX
	XCHG	AH,AL
	JMP	SHORT STORE_DOSVER

VERSION_OLD_WAY:
	mov	ah,30h	;Get DOS version.
	INT	21H
	xchg	al,ah
	cmp	ax,(3*256)+30
	jne	store_dosver
	cmp	bh,5	;Zenith DOS?
	jne	store_dosver
	mov	al,31	;Treat Zenith 3.30 like Compaq 3.31.

STORE_DOSVER:
	mov	cs:dos_version,ax

	TEST	SWITCH,QUIET_MODE	; are we in MSDOS FORMAT?
	JNZ	NO_SLASH_ONE	; yes, branch

	push	ds
	pop	es
	cld
	mov	al,"/"
	mov	di,81h
	mov	cl,ds:[di-1]
	mov	ch,0

find_slash_n:
	jcxz	no_slash_n
	repne scasb
	jne	no_slash_n
	jcxz	no_slash_n
	mov	al,es:[di]
;	call	uppercase
	and	al,not 20h
	cmp	al,"N"
	jne	find_slash_n

got_slash_n:
	clc
	jmp	mirror_return	;Write no mirror.fil, etc.

no_slash_n:
	CALL	FIND_SLASH_ONE	;May set ONE_FILE_ONLY bit in SWITCH.

NO_SLASH_ONE:
	MOV	byte ptr cs:OUR_SIGNATURE,'A'

	TEST	SWITCH,QUIET_MODE	; are we in MSDOS FORMAT?
	JNZ	DONT_ISSUE_CRIGHT       ; yes, branch

	LEA	DX,COPYRIGHT_3	;lf,cr,"Creates image of system area.."
	mov	ah,9
	INT	21H

DONT_ISSUE_CRIGHT:
	cmp	cs:dos_version,200h
	jae	proceed
	LEA	DX,BAD_VERSION
	MOV	RETURN_CODE,5
	MOV	AH,9
	INT	21H
	JMP	SHORT cant_process

PROCEED:
;	MOV	byte ptr cs:OUR_SIGNATURE,'A'
;	CALL	FIND_SLASH_ONE	;May set ONE_FILE_ONLY bit in SWITCH.
         CALL  SETUP_CTRL_BRK_INTERCEPTS

GET_DRIVE_LOOP:
	MOV	AL,PASSED_DRIVE
	TEST	SWITCH,QUIET_MODE
	JNZ	ANOTHER_DRIVE  ; if from MS-DOS FORMAT, drive in AL

	CALL	GET_NEXT_DRIVE	;Returns ASCII drive letter.
	jnc	another_drive
	 JMP   SHORT PROCESS_CHECK	;No more drives were requested.

ANOTHER_DRIVE:
	CALL	VALIFY_EACH_DRIVE	;Does it all.
	jnc	made_it_ok
	 JMP   SHORT  FORGET_PROCESS	;Error.

made_it_ok:
	TEST	SWITCH,QUIET_MODE
	JNZ	PROCESS_CHECK  ; if from MS-DOS FORMAT, don't loop
	jmp	get_drive_loop

PROCESS_CHECK:
	TEST	SWITCH,QUIET_MODE
	JNZ	MIRROR_RETURN

	MOV	DS,cs:CS_SAVE
	LEA	DX,MSG	; "MIRROR successful"
	MOV	AH,9
	INT	21H
	CLC
	jmp	short mirror_return

FORGET_PROCESS:
	TEST	SWITCH,QUIET_MODE
	JNZ	CANT_PROCESS

	MOV	DS,cs:CS_SAVE
	LEA	DX,FAIL_MSG	; "MIRROR unsuccessful"
	MOV	AH,9
	INT	21H

CANT_PROCESS:
	STC

MIRROR_RETURN:
         CALL  RESTORE_CTRL_BRK_INTERCEPTS

	CMP   OUR_THREE_SECTOR_SEGMENT,0
	JE    M_R_1

	MOV   ES,OUR_THREE_SECTOR_SEGMENT
	MOV   AH,49H
	INT   21H
	MOV   OUR_THREE_SECTOR_SEGMENT,0

M_R_1:
	CMP   USE_SEGMENT,0
	JE    M_R_2

	MOV   ES,USE_SEGMENT
	MOV   AH,49H
	INT   21H
	MOV   USE_SEGMENT,0

M_R_2:
	MOV   AL,RETURN_CODE
	MOV   AH,FAILURE_TYPE			; M005;
	POP   ES
	POP   DS
	RET

MIRROR	ENDP



SETUP_CTRL_BRK_INTERCEPTS PROC NEAR
         MOV   AX,351BH
         INT   21H
         MOV   OLD_1BH_SEG,ES
         MOV   OLD_1BH_OFF,BX

         MOV   AX,3523H
         INT   21H
         MOV   OLD_23H_SEG,ES
         MOV   OLD_23H_OFF,BX

         PUSH  CS
         POP   DS
         LEA   DX,OUR_1B_ROUTINE
         MOV   AX,251BH
         INT   21H

         LEA   DX,OUR_23_ROUTINE
         MOV   AX,2523H
         INT   21H

         RET

SETUP_CTRL_BRK_INTERCEPTS ENDP



RESTORE_CTRL_BRK_INTERCEPTS PROC NEAR
         CMP   OLD_1BH_SEG,0
         JE    R_C_B_I_RET

         LDS   DX,OLD_1BH_DWORD  
         MOV   AX,251BH
         INT   21H

         LDS   DX,OLD_23H_DWORD  
         MOV   AX,2523H
         INT   21H

R_C_B_I_RET:
         RET

RESTORE_CTRL_BRK_INTERCEPTS ENDP



OUR_1B_ROUTINE PROC NEAR
         OR    BREAK_SWITCH,BEEN_BROKE
         IRET

OUR_1B_ROUTINE ENDP



OUR_23_ROUTINE PROC NEAR
         OR    BREAK_SWITCH,BEEN_BROKE 
         IRET

OUR_23_ROUTINE ENDP



HEX_TO_ASCII PROC NEAR
	COMMENT *
	THE	PURPOSE OF THIS ROUTINE IS TO TAKE A HEX VALUE AND
	CONVERT IT TO decimal ASCII SO IT CAN BE DISPLAYED.

	INPUT:
		 CX - CONTAINS THE NUMBER OF OUTPUT ASCII CHARACTERS
		 BX - CONTAINS THE OFFSET INTO DS OF THE OUTPUT FIELD
		 DX:AX - CONTAINS THE NUMBER TO BE CONVERTED
	*

	PUSH	AX
	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	DI
	PUSH	SI
	PUSH	BP

	MOV	DI,BX	;USE DI AS FLOATING POINTER
	ADD	DI,CX	;POINT TO BYTE BEYOND FIELD
	DEC	DI	;BACKUP TO LAST BYTE
	MOV	BX,10000	;DIVIDE BY 10000
	DIV	BX	; BREAK INTO 2 PARTS
	MOV	BP,AX	; SAVE HI PART
	MOV	AX,DX	; WORK ON LOW PART FIRST
	MOV	DX,0
	MOV	SI,0
	CMP	CX,4
	JNA	PRE_CONV2
	MOV	SI,CX
	SUB	SI,4
	MOV	CX,4
; THE PURPOSE OF THE ABOVE INSTRUCTIONS IS BECAUSE THE OUTPUT FIELD IS
; CREATED RIGHT TO LEFT.
PRE_CONV2:
	MOV	BX,10
CONV2:
	DIV	BX	;PERFORM DIVISION
;	 AX = QUOTIENT
;	 DX = REMAINDER
	ADD	DL,30H	;MAKE RELATIVE TO ASCII ZERO
	MOV	BYTE PTR [DI],DL   ;MOVE VALUE TO OUTPUT AREA
	DEC	DI	;DECREMENT OUTPUT POINTER
	MOV	DX,0	;ADJUST AX BACK TO 32 BIT NUMBER
	LOOP	CONV2
	MOV	AX,BP
	MOV	DX,0
	MOV	CX,SI
	MOV	SI,0
	MOV	BP,0
	JCXZ	CONV2_OUT
	JMP	CONV2

CONV2_OUT:
	POP	BP
	POP	SI
	POP	DI
	POP	DX
	POP	CX
	POP	BX
	POP	AX
	RET

HEX_TO_ASCII ENDP



;BOOT_FAT_OR_DIR DB 0
;DIRECTORY	  EQU 80H
;MEMORY_SET	  EQU 40H

ABS_25_LIST	LABEL BYTE
ABS_25_SEC_LO	DW    0
ABS_25_SEC_HI	DW    0
ABS_25_COUNT	DW    0
ABS_25_OFF	DW    0
ABS_25_SEG	DW    0

NUM_SECTORS_PER_64K   DW 0
TOTAL_SECTORS_TO_READ DW 0

HANDLE		DW  0
HANDLE_CTL	DW  0

PARM_OFFSET	DW  81H
PARM_COUNT	DW  0

IO_AREA_DWORD	DD  0
	ORG	IO_AREA_DWORD
IO_AREA_OFFSET	DW  0
IO_AREA_SEGMENT DW 0

NUM_BOOT_SECTORS DW 0
OFFSET_TO_FAT	 DW 0

XLAT_POINTER	DW  0
XLAT_END	DW	0

BACKUP_IND_BYTE DB 0FFH

ASCIIZ_BACKUP	DB  'x:\MIRROR.BAK',0
ASCIIZ_TEMP	DB  'x:\MIRROR.$$$',0

ASCIIZ		DB  'x:\MIRROR.FIL',0

		DB  8 DUP(0)

MIRROR_SWITCH	DB   0
ABS_25_HARD_WAY EQU 80H

BACKUP_LSEEK	EQU $-ASCIIZ
BACKUP_IND	DB  0
BACKUP_LSN	DW  0
	ORG	ASCIIZ+26


CTL1_LSN	DW	0
CTL1_LSN_HI	DW  0

CTL2_LSN	DW	0
CTL2_LSN_HI	DW  0

SECTOR_SIZE	DW 0
NUM_SECTORS_TO_HANDLE DW 0
NUM_BOOT_FAT_SECTORS  DW 0
NUM_DIR_SECTORS       DW 0
FIRST_DIRECTORY_SECTOR DW 0
HEADS		       DW 0
NUM_FATS		DB 0
NUM_RESERVED	 DW 0
SECTORS_PER_CLU DB 0
SECTORS_PER_TRACK DW 0
NUM_HIDDEN	 DW 0
NUM_CTL_SECTORS DW 0
ALT_SECTOR_SIZE DW 0

	DB	16 DUP(0)
	ORG	ASCIIZ+64
CTL_LEN EQU	$-ASCIIZ

DIR_ENTRY_LEN	        EQU 32
TRYIN_CLUSTERS	        DW  0
NUM_FAT_SECTORS        DW  0
HIGHEST_CLUSTER        DW  0
FIRST_DATA_SECTOR      DW  0
CLUSTERS_WEVE_TRIED    DW  0
OUR_SPOTS_FAT_SECTOR_DISP  DW 0
OUR_SPOTS_DIR_SECTORS_LEFT DW 0
OUR_SPOTS_FAT_SECTOR       DW 0
CONTROL_FILE_START_CLU     DW 0
FAT_NEXT_CLUSTER           DW 0

SECTORS_LEFT_TO_WRITE      DW 0
SECTORS_MAX_AT_A_TIME      DW 0
MAX_WRITE_UPDATE_DS        DW 0

OUR_SPOTS_DIR_ENTRY DB 'MIRORSAVFIL'
		     DB  27H   	; archive+hidden+sys+read-only
		     DB  10 DUP(0)
		     DW  0     	; time
		     DW  0     	; date
OUR_SPOTS_START_CLU DW  0
		     DW  OUR_SPOTS_NEW_LEN ; size (lo)
		     DW  0    	; size (hi)

FAT_INFO		DB   0
FAT_EQU_16_BITS EQU 80H
CLU_IS_A_BADDIE EQU 40H

START_CLU	     DW  0
START_CLU_LAST	     DW  0
START_CLU_CONT	     DW  0
REL_SECTOR	     DW  0
REL_CLUSTER	     DW  0
REL_CLUSTER_LAST    DW  0

OUR_SPOT_ASCIIZ DB  'x:\'
		 DB  'MIRORSAV.FIL',0

OUR_SPOTS_FILE_NAME DB 'MIRORSAVFIL'

OUR_SPOTS_CTL_FILE_NAME DB 'MIRROR  FIL'

;M009: ********** START CONTIGUOUS BLOCK ***********************************

OUR_SPOTS_IO_AREA DW 0,0
OUR_SIGNATURE_OFFSET EQU $-OUR_SPOTS_IO_AREA  ;M009: SIGNATURE offset in block.
OUR_SIGNATURE	   DB 'aMSESLIFVASRORIMESAEP'
OUR_SIGNATURE_LEN EQU $-OUR_SIGNATURE
OUR_SPOTS_YEAR	   DW 0
OUR_SPOTS_MODAY   DW 0
OUR_SPOTS_TIME	   DW 0
OUR_SPOTS_OLD	DW 5 DUP(0)
OUR_SPOTS_NEW_LEN EQU $-OUR_SPOTS_IO_AREA

;M009: ********** END CONTIGUOUS BLOCK *************************************

OUR_SPOTS_DIR_OFFSET DW 0

OUR_SPOTS_FAT_OFFSET DW 0



FIND_SLASH_ONE PROC NEAR
	MOV	DI,81H
	MOV	CH,0
	MOV	CL,[DI-1]
	JCXZ	FIND_S1_EXIT

FIND_S1_REPEAT:
	MOV	AL,'/'
	REPNE	SCASB
	JCXZ	FIND_S1_EXIT
	JNE	FIND_S1_EXIT

	CMP	BYTE PTR [DI],'1'
	JNE	FIND_S1_REPEAT
	OR	SWITCH,ONE_FILE_ONLY

FIND_S1_EXIT:
	RET

FIND_SLASH_ONE ENDP

;------------------------------------------------------
; If CF=false, then AL = ascii drive letter.
; If CF=true, then there are no more requested drives.

GET_NEXT_DRIVE PROC NEAR
	MOV	DS,CS_SAVE
	MOV	SI,PARM_OFFSET
	MOV	CX,PARM_COUNT
	MOV	MIRROR_SWITCH,0

GET_NEXT_DRIVE_LOOP:
	JCXZ	NO_MORE_DRIVES
	LODSB
	DEC	CX
	CMP	AL,'/'
	JE	NO_MORE_DRIVES
;	call	uppercase
; 04-26-90 GWD	AND	AL,NOT 20H
	AND	AL,NOT 20H
	CMP	AL,'A'
	JB	GET_NEXT_DRIVE_LOOP
	CMP	AL,'Z'
	JA	GET_NEXT_DRIVE_LOOP

GOT_NEXT_DRIVE:
	MOV	PARM_OFFSET,SI
	MOV	PARM_COUNT,CX
	CLC
	RET

NO_MORE_DRIVES:
	CMP	PARM_OFFSET,81H
	JNE	NO_MORE_DRIVES_REALLY
	MOV	AH,19H
	INT	21H
	ADD	AL,41H
	INC	SI
	MOV	CX,0
	JMP	GOT_NEXT_DRIVE

NO_MORE_DRIVES_REALLY:
	STC
	RET

GET_NEXT_DRIVE ENDP



VALIFY_EACH_DRIVE PROC NEAR
	MOV	ASCIIZ,AL
	MOV	ASCIIZ_BACKUP,AL
	MOV	ASCIIZ_TEMP,AL
	MOV	OUR_SPOT_ASCIIZ,AL

	MOV	DRIVE_MSG+DRV_NAME_LEN,AL ;Use length equate ;M002

	AND	SWITCH,NOT GOTTA_PRE_CONTROL+GOTTA_PRE_IMAGE
	MOV	DS,CS_SAVE

	TEST	SWITCH,QUIET_MODE
	JNZ	V_E_D_NO_MSG

	LEA	DX,DRIVE_MSG
	MOV	AH,9
	INT	21H
	LEA	DX,PROCESSING_MSG
	INT	21H

V_E_D_NO_MSG:
	mov	al,asciiz
	call	check_network_drive	;Added 5-1-89 GWD.
	jnc	not_network
	lea	dx,network_installed
	JMP	VALIFY_ERR_5

NOT_NETWORK:
	CALL	GET_DRIVE_SPECS
	JNC	VALIFY_E_D_CTL_FILE
	JMP	VALIFY_ERR_5

VALIFY_ERR_5_JMP:
	JMP	VALIFY_ERR_5

VALIFY_E_D_CTL_FILE:
         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   VALIFY_ERR_5_JMP

	CALL  CHECK_ON_PREEXISTING_FILES
	LEA   DX,FULL_MSG
	JC    VALIFY_ERR_5_JMP

	CALL	GET_OLD_INFO

         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   VALIFY_ERR_5_JMP

	CALL	CHECK_FOR_OUR_SPOT
	JNC	VALIFY_E_D_0
	CMP	RETURN_CODE,4
	JNE	VALIFY_ERR_5_JMP

VALIFY_ERR_4_JMP:
	JMP	VALIFY_ERR_4

VALIFY_E_D_0:
	CALL	HANDLE_TWO_FILES
	MOV	DS,CS_SAVE
	LEA	DX,ASCIIZ
	MOV	AX,4301H
	MOV	CX,21H	; archive,read-only
	INT	21H
	JNC	VALIFY_E_D_1
	JMP	CREATE_FILE_FIRST

VALIFY_E_D_1:
         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   VALIFY_ERR_5_JMP

	MOV	AX,3D00H
	INT	21H
	JNC	VALIFY_E_D_2

CREATE_FILE_FIRST_JMP:
	JMP	CREATE_FILE_FIRST

VALIFY_E_D_2:
	MOV	HANDLE,AX	       ; get handle

	MOV	BX,HANDLE
	MOV	CX,0
	MOV	DX,0
	MOV	AX,4202H	       ; point to EOF
	INT	21H
	JC	CREATE_FILE_FIRST_JMP	; just go create it again

	DIV	SECTOR_SIZE	       ; get # of sectors it'll take
	MOV	DX,NUM_SECTORS_TO_HANDLE ; get our need
	ADD	DX,2	; don't forget control recs
	CMP	AX,DX	; got enuf room
	JB	CREATE_FILE_FIRST_JMP	; no, just go recreate it

	MOV	BX,HANDLE
	MOV	AH,3EH
	INT	21H
	LEA	DX,CLOSE_MSG
	JC	VALIFY_ERR_5_JMP_1

	MOV	AH,0DH
	INT	21H

;	MOV	CL,4
;	MOV	AX,SECTOR_SIZE
;	SHR	AX,CL
;	MUL	NUM_SECTORS_TO_HANDLE
;	ADD	AX,USE_SEGMENT
;	JC	VALIFY_NO_MEM
;
;	MOV	BX,USE_POINTER
;	SHR	BX,CL
;	ADD	AX,BX
;	JC	VALIFY_NO_MEM
;	CMP	AX,CS:[2]
;	JNA	VALIFY_GOT_MEM
;
;VALIFY_NO_MEM:
;	LEA	DX,NO_MEM_MSG
;	JMP	VALIFY_ERR_5
;
;VALIFY_GOT_MEM:
	LDS	SI,USE_DWORD
	MOV	CX,NUM_BOOT_FAT_SECTORS
	MOV	ABS_25_SEC_LO,0
	MOV	ABS_25_SEC_HI,0
	LDS	BX,IO_AREA_DWORD
	CALL	ABS_25	; read boot and FAT
	LEA	DX,READ_ERROR_MSG
	JC	VALIFY_ERR_4_JMP_1

         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   VALIFY_ERR_5_JMP_1

	MOV	AX,NUM_BOOT_FAT_SECTORS  ;
	MUL	SECTOR_SIZE	;
	PUSH	AX	;   Adjust DS:BX
	MOV	CX,4	;
	SHL	DX,CL	;   to point to the next
	SHR	AX,CL	;
	OR	AH,DL	;   logical spot
	MOV	DX,DS	;
	ADD	DX,AX	;
	MOV	DS,DX	;
	POP	AX	;
	AND	AX,0FH	;
	ADD	BX,AX	;

	MOV	START_OF_ROOT_OFFSET,BX
	MOV	START_OF_ROOT_SEGMENT,DS

	MOV	AX,FIRST_DIRECTORY_SECTOR
	MOV	ABS_25_SEC_LO,AX
	MOV	CX,NUM_DIR_SECTORS
	CALL	ABS_25	; read root
	LEA	DX,READ_ERROR_MSG
	JNC	VALIFY_E_D_3

VALIFY_ERR_4_JMP_1:
	JMP	VALIFY_ERR_4

VALIFY_ERR_5_JMP_1:
	JMP	VALIFY_ERR_5

VALIFY_E_D_3:
         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   VALIFY_ERR_5_JMP_1

	CALL	CHECK_RE_DEFINITION
	JC	VALIFY_ERR_5_JMP_1

	; If in QUIET mode and the files wern't preexisting,
	; gotta delete them out of our root image.
	CALL	MIGHT_NEED_TO_DELETE_FILES

	MOV	DS,CS_SAVE
	LEA	DX,ASCIIZ
	MOV	AX,4301H
	MOV	CX,0
	INT	21H

	MOV	AX,3D02H
	INT	21H
	LEA	DX,OPEN_MSG
	JC	VALIFY_ERR_5_JMP_1
	MOV	HANDLE,AX

	LDS	DX,IO_AREA_DWORD
	MOV	BP,NUM_SECTORS_TO_HANDLE
	CALL	WRITE_FILE
	LEA	DX,WRITE_ERROR_MSG
	JC	VALIFY_ERR_4_JMP_1

; One last thing.  Find the starting cluster number of the control
;  file and stuff it into our little teenie file out at the end.

	MOV	AH,0DH	; no bufferin' DOS
	INT	21H

	MOV	AX,FIRST_DIRECTORY_SECTOR
	MOV	ABS_25_SEC_LO,AX
	MOV	ABS_25_SEC_HI,0
	MOV	AX,NUM_DIR_SECTORS
	MOV	OUR_SPOTS_DIR_SECTORS_LEFT,AX

V_E_D_READ_DIR_LOOP:
         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   VALIFY_ERR_5_JMP_1

;	MOV	DS,CS_SAVE
	MOV	DS,OUR_THREE_SECTOR_SEGMENT
	MOV	BX,OUR_SPOTS_DIR_OFFSET
	MOV	CX,1
	CALL	ABS_25
	LEA	DX,READ_ERROR_MSG
	JNC	V_E_D_CHECK_DIR
	JMP	VALIFY_ERR_4

V_E_D_CHECK_DIR:
	MOV	CL,5
	MOV	AX,SECTOR_SIZE
	SHR	AX,CL
	MOV	CX,AX

V_E_D_CHECK_DIR_LOOP:
	PUSH	CX
	MOV	ES,CS_SAVE
	LEA	DI,OUR_SPOTS_CTL_FILE_NAME
	MOV	CX,11
	MOV	SI,BX
	REPE	CMPSB
	POP	CX
	JE	V_E_D_GOT_OUR_DIR
	ADD	BX,DIR_ENTRY_LEN	; tew next wun
	LOOP	V_E_D_CHECK_DIR_LOOP

	DEC	OUR_SPOTS_DIR_SECTORS_LEFT   ; minus wun
	JZ	V_E_D_NO_DIRS	; oops, none left
	ADD	ABS_25_SEC_LO,1
	ADC	ABS_25_SEC_HI,0
	JMP	V_E_D_READ_DIR_LOOP

V_E_D_NO_DIRS:
	JMP	CREATE_FILE_FIRST

V_E_D_GOT_OUR_DIR:
	MOV	AX,WORD PTR DS:[BX+26]
	MOV	CONTROL_FILE_START_CLU,AX

	MOV	DS,CS_SAVE
	LEA	DX,OUR_SPOT_ASCIIZ
	MOV	AX,4301H
	MOV	CX,0
	INT	21H
	LEA	DX,UPDATE_OURS_ERROR_MSG
	JNC	V_E_D_OPEN_OURS
	JMP	VALIFY_ERR_5

V_E_D_OPEN_OURS:
	LEA	DX,OUR_SPOT_ASCIIZ
	MOV	AX,3D01H	; open for write
	INT	21H	; izit there?
	LEA	DX,UPDATE_OURS_ERROR_MSG
	JNC	V_E_D_GOT_MIRORSAV
	JMP	VALIFY_ERR_5

V_E_D_GOT_MIRORSAV:
	MOV	HANDLE_CTL,AX

	MOV	AX,CONTROL_FILE_START_CLU
	CALL	CONVERT_CLU_TO_LOG_SECTOR
	MOV	AX,ABS_25_SEC_LO
	MOV	OUR_SPOTS_IO_AREA,AX
	MOV	AX,ABS_25_SEC_HI
	MOV	OUR_SPOTS_IO_AREA+2,AX

	MOV	AH,2AH	; get date
	INT	21H
	MOV	OUR_SPOTS_YEAR,CX
	MOV	OUR_SPOTS_MODAY,DX

	MOV	AH,2CH	; get time
	INT	21H
	MOV	OUR_SPOTS_TIME,CX

	MOV	BX,HANDLE_CTL
	MOV	CX,OUR_SPOTS_NEW_LEN
	LEA	DX,OUR_SPOTS_IO_AREA
	MOV	AH,40H	; write
	INT	21H

	LEA	DX,UPDATE_OURS_ERROR_MSG
	CMP	AX,CX
	JE	V_E_D_WROTE_MIRORSAV
	JMP	SHORT VALIFY_ERR_5

V_E_D_WROTE_MIRORSAV:
	MOV	AH,3EH	; close mirorsav.fil
	INT	21H
	LEA	DX,UPDATE_OURS_ERROR_MSG
	JNC	V_E_D_CHANGE_OUR_ATTRS
	JMP	SHORT VALIFY_ERR_5

V_E_D_CHANGE_OUR_ATTRS:
	LEA	DX,OUR_SPOT_ASCIIZ
	MOV	AX,4301H
	MOV	CX,27H	; archive+hidden+sys+read-only
	INT	21H
	LEA	DX,UPDATE_OURS_ERROR_MSG
	JC	VALIFY_ERR_5

	CALL	UPDATE_BACKUP_FILE
	CALL	BUILD_CONTROL_RECORD
	JC	VALIFY_ERR_5
	RET

VALIFY_ERR_4:
	MOV	RETURN_CODE,4
         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   VALIFY_ERR_5  
	JMP	SHORT VALIFY_ERR_STC

VALIFY_ERR_5:
	MOV	RETURN_CODE,5
         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   VALIFY_ERR_BYP

VALIFY_ERR_STC:
	MOV	DS,CS_SAVE
	PUSH	DX
	LEA	DX,DRIVE_MSG
	MOV	AH,9
	INT	21H
	POP	DX
	MOV	AH,9
	INT	21H

; if any trubbles at all, delete the MIRORSAV.FIL

VALIFY_ERR_BYP:
	CALL	MIRORSAV_DELETE

VALIFY_STC_EXIT:
	STC
	RET

CREATE_FILE_FIRST:
	MOV	DS,CS_SAVE
	LEA	DX,ASCIIZ
	MOV	AX,4301H
	MOV	CX,0
	INT	21H	; if file exists, make it
			; deletable
	MOV	AH,41H
	INT	21H	; delete it

	MOV	AH,3CH
	MOV	CX,21H
	INT	21H
	LEA	DX,OPEN_MSG
	JC	VALIFY_ERR_5

	MOV	BX,AX
	MOV	AX,NUM_SECTORS_TO_HANDLE
	ADD	AX,NUM_CTL_SECTORS	; don't forget control records
	MOV	SECTORS_LEFT_TO_WRITE,AX
	MOV	DS,CS_SAVE

RE_CREATE_FILE:
	MOV	AX,SECTORS_MAX_AT_A_TIME
	CMP	AX,SECTORS_LEFT_TO_WRITE
	JNA	RE_CREATE_FILE_1
	MOV	AX,SECTORS_LEFT_TO_WRITE

RE_CREATE_FILE_1:
	SUB	SECTORS_LEFT_TO_WRITE,AX
	MUL	SECTOR_SIZE
	MOV	CX,AX
	MOV	AH,40H
	INT	21H
	LEA	DX,FULL_MSG
	JC	V_E_D_STC_JMP
	CMP	AX,CX
	JNE	V_E_D_STC_JMP
	CMP	SECTORS_LEFT_TO_WRITE,0
	JNE	RE_CREATE_FILE

	MOV	AH,3EH
	INT	21H
	LEA	DX,CLOSE_MSG
	JNC	V_E_D_OPEN

V_E_D_STC_JMP:
	JMP	VALIFY_ERR_5

V_E_D_OPEN:
	MOV	DS,CS_SAVE
	LEA	DX,ASCIIZ
	MOV	AX,3D00H
	INT	21H
	LEA	DX,OPEN_MSG
	JC	V_E_D_STC_JMP
	JMP	VALIFY_E_D_2

VALIFY_EACH_DRIVE ENDP



CHECK_ON_PREEXISTING_FILES PROC NEAR
	 PUSH  DS
	 PUSH  BP

; Calculate # bits to shift to fake a divide by bytes per cluster

	 MOV   AX,SECTOR_SIZE
	 XOR   CH,CH
	 MOV   CL,SECTORS_PER_CLU
	 MUL   CX	; gets bytes per cluster
	 MOV   CX,AX
	 XOR   AX,AX

C_O_P_R_COUNT_BITS:
	 SHR   CX,1
	 JC    C_O_P_R_END
	 INC   AX	;  count bits we shifted
	 JMP   C_O_P_R_COUNT_BITS

C_O_P_R_END:
	 MOV   BYTES_PER_CLUSTER_SHIFT_COUNT,AX ;  save result

; Calculate # clusters needed for our files

	 MOV   AX,NUM_SECTORS_TO_HANDLE
	 ADD   AX,NUM_CTL_SECTORS
	 MOV   DX,0
	 MOV   BL,SECTORS_PER_CLU	; for MIRORSAV.FIL
	 MOV   BH,0
	 DIV   BX
	 CMP   DX,0
	 JE    C_O_P_R_NO_ROUND
	 INC   AX	; # clusters for MIRROR.FIL

C_O_P_R_NO_ROUND:
	 INC   AX	; # clusters both MIRROR.FIL
			; and MIRORSAV.FIL
	 MOV   BP,AX

; Calculate space currently occupied by MIRROR.FIL

	 MOV   DS,CS_SAVE
	 LEA   DX,ASCIIZ
	 MOV   AX,4300H
	 INT   21H	; does it exist?
	 JC    C_O_P_F_NEXT	; no, branch

	 OR    SWITCH,GOTTA_PRE_IMAGE
	 MOV   AX,3D00H
	 INT   21H	; open file
	 JC    C_O_P_F_NEXT

	 MOV   BX,AX
	 MOV   AX,4202H
	 XOR   CX,CX
	 XOR   DX,DX
	 INT   21H	; get size
	 JC    C_O_P_F_CLOSE_1

	 CALL  FAKE_DIVIDE	; calc # clusters
	 CMP   BP,AX	; got enuf?
	 JBE   C_O_P_F_CLC	; more than enuf, exit

	 SUB   BP,AX	; decrement amt needed

C_O_P_F_CLOSE_1:
	 MOV   AH,3EH	; close file
	 INT   21H

C_O_P_F_NEXT:

; Calculate space currently occupied by MIRORSAV.FIL

	 LEA   DX,OUR_SPOT_ASCIIZ
	 MOV   AX,4300H
	 INT   21H	; does it exist?
	 JC    C_O_P_F_CALC

	 OR    SWITCH,GOTTA_PRE_CONTROL
	 MOV   AX,3D00H
	 INT   21H	; open file
	 JC    C_O_P_F_CALC

	 MOV   BX,AX
	 MOV   AX,4202H
	 XOR   CX,CX
	 XOR   DX,DX
	 INT   21H	; get size
	 JC    C_O_P_F_CLOSE_2

	 CALL  FAKE_DIVIDE	; calc # clusters
	 CMP   BP,AX	; got enuf
	 JBE   C_O_P_F_CLC

	 SUB   BP,AX	; decrement amt needed

C_O_P_F_CLOSE_2:
	 MOV   AH,3EH	; close file
	 INT   21H

C_O_P_F_CALC:
	 CMP   BP,0	; need anymore?
	 JE    C_O_P_F_CLC	; no, exit

	 MOV   DL,ASCIIZ
	 SUB   DL,40H
	 MOV   AH,36H	; get free space
	 INT   21H
	 JC    C_O_P_F_CLC	; don't know, so try anyway

	 CMP   BX,BP	; got enuf clus?
	 JB    C_O_P_F_STC	; no, branch

C_O_P_F_CLC:
	 CLC
	 JMP   SHORT C_O_P_F_EXIT

C_O_P_F_STC:
	 STC

C_O_P_F_EXIT:
	 POP   BP
	 POP   DS
	 RET

CHECK_ON_PREEXISTING_FILES ENDP



FAKE_DIVIDE PROC NEAR
	 PUSH  BX
	 MOV   BX,0
	 MOV   CX,BYTES_PER_CLUSTER_SHIFT_COUNT
	 JCXZ  FAKE_DIVIDE_RET

FAKE_DIVIDE_LOOP:
	 SHR   DX,1
	 RCR   AX,1
	 RCR   BX,1	; for remainder
	 LOOP  FAKE_DIVIDE_LOOP 	; fakes the divide

FAKE_DIVIDE_RET:
	 CMP   BX,0
	 JE    FAKE_DIVIDE_EXIT
	 INC   AX	; round to next cluster

FAKE_DIVIDE_EXIT:
	 POP   BX
	 RET

FAKE_DIVIDE ENDP



MIGHT_NEED_TO_DELETE_FILES PROC NEAR
	 TEST  SWITCH,QUIET_MODE	; are we in MSDOS FORMAT?
	 JNZ   M_N_T_D_F_CONT	; yes, branch
	 RET

M_N_T_D_F_CONT:
	 TEST  SWITCH,GOTTA_PRE_IMAGE
	 JNZ   M_N_T_D_F_NEXT

	 LEA   SI,OUR_SPOTS_CTL_FILE_NAME
	 CALL  TRY_TO_DELETE_IT

M_N_T_D_F_NEXT:
	 TEST  SWITCH,GOTTA_PRE_CONTROL
	 JNZ   M_N_T_D_F_EXIT

	 LEA   SI,OUR_SPOTS_FILE_NAME
	 CALL  TRY_TO_DELETE_IT

M_N_T_D_F_EXIT:
	 RET

MIGHT_NEED_TO_DELETE_FILES ENDP



TRY_TO_DELETE_IT PROC NEAR
	 PUSH  DS
	 PUSH  ES
	 PUSH  AX
	 PUSH  BX
	 PUSH  CX
	 PUSH  BP

	 MOV   AX,NUM_DIR_SECTORS
	 MUL   SECTOR_SIZE
	 MOV   BX,32
	 DIV   BX
	 MOV   CX,AX
	 MOV   BX,START_OF_ROOT_OFFSET
	 MOV   ES,START_OF_ROOT_SEGMENT
	 MOV   BP,SI
	 MOV   DS,CS_SAVE

T_T_D_I_LOOP:
	 MOV   SI,BP
	 MOV   DI,BX
	 PUSH  CX
	 MOV   CX,11
	 REPE  CMPSB
	 POP   CX
	 JE    T_T_D_I_HIT
	 ADD   BX,32
	 LOOP  T_T_D_I_LOOP
	 JMP   SHORT T_T_D_I_EXIT

T_T_D_I_HIT:
	 MOV   BYTE PTR ES:[BX],0E5H
	 MOV   AX,ES:[BX+26]	; get starting cluster #
	 CALL  CLEAR_FILE_FROM_FAT

T_T_D_I_EXIT:
	 POP   BP
	 POP   CX
	 POP   BX
	 POP   AX
	 POP   ES
	 POP   DS
	 RET

TRY_TO_DELETE_IT ENDP



CLEAR_FILE_FROM_FAT PROC NEAR
	PUSH	DS
	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	DI
	PUSH	SI
	PUSH	BP
	MOV	BP,AX
	CMP	AX,2	; invalid?
	JB	C_F_F_F_EXIT

	MOV	AX,NUM_BOOT_SECTORS
	MUL	SECTOR_SIZE
	MOV	OFFSET_TO_FAT,AX

C_F_F_F_NEXT_CLUSTER:
	TEST	FAT_INFO,FAT_EQU_16_BITS
	JNZ	C_F_F_F_PROCESS_16_BIT_FAT

;	 This will follow the chain thru the 12 bit FAT.

	MOV	DX,0
	MOV	AX,BP	; pick up next cluster number
	SHR	AX,1	; divide by 2
	ADD	AX,BP	; equals times 1.5

	CALL	POINT_TO_CLUSTER_CELL

	MOV	AX,[BX] 	; pick up next
	TEST	BP,1	; is the cluster number odd?
	JZ	C_F_F_F_1	; no, branch
	MOV	CL,4
	SHR	AX,CL	; shift to right 4 bits
	AND	WORD PTR [BX],0000FH	; clear cell
	JMP	SHORT C_F_F_F_2

C_F_F_F_1:
	AND	WORD PTR [BX],0F000H	; clear cell

C_F_F_F_2:
	AND	AX,0FFFH	; clear hi-order byte
	MOV	BP,AX
	CMP	AX,2	; invalid?
	JB	C_F_F_F_EXIT
	CMP	AX,0FF7H	; at end or error?
	JAE	C_F_F_F_EXIT
	JMP	C_F_F_F_NEXT_CLUSTER

C_F_F_F_PROCESS_16_BIT_FAT:
	MOV	DX,0
	MOV	AX,BP	; get cluster #
	ADD	AX,AX	; times 2
	ADC	DX,0

	CALL	POINT_TO_CLUSTER_CELL

	MOV	AX,[BX] 	; get next cluster #
	MOV	BP,AX	; save it
	MOV	WORD PTR [BX],0 	; clear cluster cell
	CMP	AX,2	; invalid?
	JB	C_F_F_F_EXIT
	CMP	BP,0FFF7H	; at end or error?
	JAE	C_F_F_F_EXIT
	JMP	C_F_F_F_NEXT_CLUSTER

C_F_F_F_EXIT:
	POP	BP
	POP	SI
	POP	DI
	POP	DX
	POP	CX
	POP	BX
	POP	DS
	RET

CLEAR_FILE_FROM_FAT ENDP



POINT_TO_CLUSTER_CELL PROC NEAR
	 ADD   AX,OFFSET_TO_FAT
	 ADC   DX,0
	 MOV   BX,AX
	 AND   BX,0FH
	 MOV   CX,4

P_T_C_C_LOOP:
	 SHR   DX,1
	 RCR   AX,1
	 LOOP  P_T_C_C_LOOP

	 ADD   AX,IO_AREA_SEGMENT
	 MOV   DS,AX
	 RET

POINT_TO_CLUSTER_CELL ENDP



BUILD_CONTROL_RECORD PROC NEAR
	MOV	AH,0DH
	INT	21H
	MOV	OUR_SPOTS_FAT_SECTOR,0FFFFH
	MOV	START_CLU,0
	MOV	START_CLU_LAST,0
	MOV	START_CLU_CONT,0
	MOV	REL_SECTOR,0
	MOV	REL_CLUSTER,0
	MOV	REL_CLUSTER_LAST,0

	LDS	SI,USE_DWORD
	MOV	DI,0
	CALL	GET_NEXT_LSN
	JC	B_C_R_ERR
	MOV	AX,ABS_25_SEC_LO
	MOV	CTL1_LSN,AX
	MOV	AX,ABS_25_SEC_HI
	MOV	CTL1_LSN_HI,AX

	MOV	DI,1
	MOV	AH,0
	MOV	AL,SECTORS_PER_CLU
	CMP	AX,NUM_CTL_SECTORS
	JA	B_C_R_OLD_STYLE
	MOV	DI,AX

B_C_R_OLD_STYLE:
	CALL	GET_NEXT_LSN
	JC	B_C_R_ERR
	MOV	AX,ABS_25_SEC_LO
	MOV	CTL2_LSN,AX
	MOV	AX,ABS_25_SEC_HI
	MOV	CTL2_LSN_HI,AX

	LEA	BX,XLAT_TABLE
	MOV	CX,NUM_SECTORS_TO_HANDLE
	MOV	LIST_COUNT,CX

	MOV	DI,NUM_CTL_SECTORS
	MOV	BP,0
	MOV	CX,NUM_BOOT_FAT_SECTORS

B_C_R_LOOP_1:
	CALL	GET_NEXT_LSN
	JC	B_C_R_ERR
	MOV	FROM_LSN,BP
	MOV	FROM_LSN_HI,0
	MOV	AX,ABS_25_SEC_LO
	MOV	TO_LSN,AX
	MOV	AX,ABS_25_SEC_HI
	MOV	TO_LSN_HI,AX
	ADD	BX,8
	INC	DI
	INC	BP
	LOOP	B_C_R_LOOP_1
	JMP	SHORT B_C_R_CHK_DIR

B_C_R_ERR:
	LEA	DX,TRACK_ERROR_MSG
	JMP	B_C_R_STC

B_C_R_CHK_DIR:
	MOV	BP,FIRST_DIRECTORY_SECTOR
	MOV	CX,NUM_DIR_SECTORS

B_C_R_LOOP_2:
	CALL	GET_NEXT_LSN
	JC	B_C_R_ERR
	MOV	FROM_LSN,BP
	MOV	FROM_LSN_HI,0
	MOV	AX,ABS_25_SEC_LO
	MOV	TO_LSN,AX
	MOV	AX,ABS_25_SEC_HI
	MOV	TO_LSN_HI,AX
	ADD	BX,8
	INC	DI
	INC	BP
	LOOP	B_C_R_LOOP_2

;	 Write control record

	CMP	NUM_CTL_SECTORS,2
	JNA	B_C_R_OLD_STYLE_A
	MOV	SECTOR_SIZE,0

	;     This will throw the old REBUILD if it encounters this
	;     new scheme. Normally, CTL_1 and CTL_2 are the LSNs of
	;     the first two control sectors. If SECTOR_SIZE is zero,
	;     the CTL_1 and CTL_2 are the LSNs of the first two
	;     clusters. This will handle multi-megabyte hard drives
	;     with more than 120 or so FAT and ROOT sectors.

B_C_R_OLD_STYLE_A:
	MOV	BX,HANDLE
	MOV	CX,0
	MOV	DX,0
	MOV	AX,4200H	; point to start
	INT	21H
	MOV	BX,DX
	LEA	DX,FULL_MSG
	JNC	B_C_R_CHECK_PTRS
	JMP	SHORT B_C_R_STC

B_C_R_CHECK_PTRS:
	CMP	AX,0
	JE	B_C_R_WRITE_CTL
	CMP	BX,0
	JE	B_C_R_WRITE_CTL
	JMP	SHORT B_C_R_STC

B_C_R_WRITE_CTL:
	MOV	BX,HANDLE
	MOV	AH,40H
	MOV	DS,CS_SAVE
	MOV	CX,CTL_LEN
	LEA	DX,ASCIIZ
	INT	21H

	LDS	DX,USE_DWORD
	MOV	SI,DX
	MOV	CX,LIST_COUNT
	SHL	CX,1
	SHL	CX,1
	SHL	CX,1
	ADD	CX,ENTRY_LEN
	ADD	CX,CTL_LEN
	MOV	AX,SECTOR_SIZE
	SHL	AX,1
	SUB	CX,CTL_LEN
	MOV	AH,40H
	INT	21H
	LEA	DX,FULL_MSG
	JNC	B_C_R_WRITE_OK
	JMP	SHORT B_C_R_STC

B_C_R_WRITE_OK:
;	 MOV   AX,5700H 	; get the files date and time
;	 INT	21H
;
;	 MOV   AX,5701H 	; set the files date and time
;	 INT	21H

	MOV	AH,3EH
	INT	21H
	LEA	DX,CLOSE_MSG
	JNC	B_C_R_SET_ATTRS
	JMP	SHORT B_C_R_STC

B_C_R_SET_ATTRS:
	MOV	DS,CS_SAVE
	LEA	DX,ASCIIZ
	MOV	AX,4301H
	MOV	CX,21H	; read only, arch
	INT	21H
	CLC
	RET

;B_C_R_CONTROL_PROB:
;	  LEA	DX,CONTROL_TOO_SMALL

B_C_R_STC:
	STC
	RET

BUILD_CONTROL_RECORD ENDP
;
;------------------------------------------------------



GET_NEXT_LSN PROC NEAR
	MOV	AX,CONTROL_FILE_START_CLU
	CALL	FIND_LOG_SECTOR
	RET

GET_NEXT_LSN ENDP
;
;------------------------------------------------------



MIRORSAV_DELETE PROC NEAR
	PUSH	CX
	PUSH	DS
	PUSH	DX
	MOV	DS,CS_SAVE
	LEA	DX,OUR_SPOT_ASCIIZ
	MOV	AX,4301H
	MOV	CX,0	; change attrs
	INT	21H
	JC	MIRORSAV_DELETE_EXIT

	LEA	DX,OUR_SPOT_ASCIIZ
	MOV	AH,41H	; delete
	INT	21H

MIRORSAV_DELETE_EXIT:
	POP	DX
	POP	DS
	POP	CX
	RET

MIRORSAV_DELETE ENDP
;
;------------------------------------------------------



ISSUE_MSG_TO_SCREEN PROC NEAR
	PUSH	DS
	PUSH	DX
	MOV	DS,CS_SAVE
	PUSH	DX
	LEA	DX,DRIVE_MSG
	MOV	AH,9
	INT	21H
	POP	DX
	MOV	AH,9
	INT	21H
	POP	DX
	POP	DS
	RET
ISSUE_MSG_TO_SCREEN ENDP
;
;------------------------------------------------------



CHECK_RE_DEFINITION PROC NEAR
	PUSH	DS
	PUSH	SI
	PUSH	AX
	PUSH	BX
	PUSH	CX
	PUSH	DX

	LDS	BX,IO_AREA_DWORD	; point to BOOT record
	MOV	AX,[BX]+11
	CMP	AX,SECTOR_SIZE	; same sector size?
	JNE	C_R_D_BAD_BOOT
	MOV	AL,[BX]+13
	CMP	AL,SECTORS_PER_CLU	; same allocation size?
	JNE	C_R_D_BAD_BOOT
	MOV	AX,[BX]+14
	CMP	AX,NUM_RESERVED        ; same # of BOOT records?
	JNE	C_R_D_BAD_BOOT
	MOV	AL,[BX]+16
	CMP	AL,NUM_FATS	; same # of FATs?
	JNE	C_R_D_BAD_BOOT
	MOV	AX,[BX]+22	; get # sectors per FAT
	ADD	AX,[BX]+14	; add # of BOOT records
	CMP	AX,NUM_BOOT_FAT_SECTORS ; same # of records?
	JNE	C_R_D_BAD_BOOT
	MOV	AX,[BX]+24
	MOV	SECTORS_PER_TRACK,AX	; get sectors per track
	MOV	AX,[BX]+28
	MOV	NUM_HIDDEN,AX	; get # of hidden sectors
	MOV	AX,[BX]+26
	MOV	HEADS,AX	; get # of heads
	LDS	SI,USE_DWORD	; point to current table
	MOV	NUM_HEADS,AX
	POP	DX
	CLC		; no redefinition
	JMP	SHORT C_R_D_EXIT	; exit

C_R_D_BAD_BOOT:
	POP	DX
	LEA	DX,BOOT_BAD	; oops, point to msg
	STC		; tellum bad

C_R_D_EXIT:
	POP	CX
	POP	BX
	POP	AX
	POP	SI
	POP	DS
	RET

CHECK_RE_DEFINITION ENDP
;
;------------------------------------------------------



;READ_FILE PROC NEAR
;	 MOV   CX,1
;	 CALL  ABS_25
;	 JC    READ_FILE_EXIT
;
;	 ADD   ABS_25_SEC_LO,1	; inc sector #
;	 ADC   ABS_25_SEC_HI,0
;	 ADD   BX,SECTOR_SIZE	; bump I/O area pointer
;	 JNC   READ_FILE_DEC	; if no carry, don't adjust DS
;	 MOV   AX,DS	; get DS
;	 ADD   AX,1000H 	; add 64K
;	 MOV   DS,AX	; reset DS
;
;READ_FILE_DEC:
;	 DEC   BP	; dec sectors to read
;	 JNZ   READ_FILE
;	 CLC
;
;READ_FILE_EXIT:
;	 RET
;
;READ_FILE ENDP
;
;------------------------------------------------------



WRITE_FILE PROC NEAR
	MOV	SECTORS_LEFT_TO_WRITE,BP

;	 Write the equivalent of the control record

         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   WRITE_FILE_STC       

	PUSH	DX
	MOV	AX,SECTOR_SIZE
	MUL	NUM_CTL_SECTORS
	POP	DX
	MOV	CX,AX
	MOV	BX,HANDLE
	MOV	AH,40H
	INT	21H	; write ctl recs
	JC	WRITE_FILE_STC
	CMP	AX,CX	; write it all?
	JNE	WRITE_FILE_STC

WRITE_FILE_AGAIN:
         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   WRITE_FILE_STC       

	PUSH	DX
	MOV	AX,SECTORS_MAX_AT_A_TIME
	CMP	AX,SECTORS_LEFT_TO_WRITE
	JNA	WRITE_FILE_AGAIN_1
	MOV	AX,SECTORS_LEFT_TO_WRITE

WRITE_FILE_AGAIN_1:
	SUB	SECTORS_LEFT_TO_WRITE,AX
	MUL	SECTOR_SIZE
	POP	DX

	MOV	CX,AX
	MOV	BX,HANDLE
	MOV	AH,40H
	INT	21H
	JC	WRITE_FILE_STC

	CMP	AX,CX	; write it all?
	JNE	WRITE_FILE_STC

	CMP	SECTORS_LEFT_TO_WRITE,0 ; point to next sector
	JE	WRITE_FILE_CLC	; if no carry, don't adjust DS

	MOV	AX,DS	; oops, get DS
	ADD	AX,MAX_WRITE_UPDATE_DS	; add 64K
	MOV	DS,AX	; reset DS
	JMP	WRITE_FILE_AGAIN

WRITE_FILE_CLC:
	CLC
	RET

WRITE_FILE_STC:
	MOV	DS,CS_SAVE
	LEA	DX,DRIVE_MSG
	MOV	AH,9
	INT	21H
	LEA	DX,FULL_MSG
	MOV	AH,9
	INT	21H
	STC
	RET

WRITE_FILE ENDP
;
;-------------------------------------------------------
; On entry: AL = drive letter ("A" thru "Z").
; On exit: if it's a network drive then CF=true.
;
; Added 05-01-89, GWD.
;
; All regs are unchanged.
;
check_network_drive PROC NEAR
	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	ds
	push	es
;
	push	cs
	pop	ds
	cld
	mov	cs:net_string,al
	cmp	cs:dos_version,300h
	jb	chknet_not_ibm
	xor	ax,ax
	INT	2Ah	;Is network installed?
	or	ah,ah
	jz	chknet_not_ibm	;No.
	mov	bl,cs:net_string
	sub	bl,"A"
	mov	bh,0
	inc	bx
	mov	ax,4409h	;Is this drive remote?
	INT	21H
	jc	chknet_not_ibm
	test	dx,1000h
	jnz	chknet_error	;It's remote.
	lea	si,net_string
	mov	ax,0300h
	INT	2Ah	;Returns CF=true when INT25h is illegal.
	jmp	short chknet_done
chknet_not_ibm: 	;Look for Novell network.
	mov	cx,-1
	mov	ax,0DC00h	;'Request Novell connection #'.
	INT	21H
	jc	chknet_ok	;Novell is not there.
	cmp	cx,-1
	je	chknet_ok
	mov	si,-1
	mov	ax,0EF01h	;'Get Novell drive table'.
	INT	21H
	jc	chknet_ok
	cmp	si,-1
	je	chknet_ok
	mov	bl,cs:net_string ;M006
	sub	al,"A"
	mov	bh,0
	test	byte ptr es:[bx+si],80h ;Is it a local drive?
	jz	chknet_error	;No - cannot process it.
chknet_ok:
	clc
	jmp	short chknet_done
chknet_error:
	stc
chknet_done:
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	cld
	sti
	ret
check_network_drive ENDP

net_string DB	"x:\",0



;------------------------------------------------------
GET_DRIVE_SPECS PROC NEAR
	CMP    OUR_THREE_SECTOR_SEGMENT,0
	JE     G_D_1

	MOV    ES,OUR_THREE_SECTOR_SEGMENT
	MOV    AH,49H
	INT    21H
	MOV    OUR_THREE_SECTOR_SEGMENT,0

G_D_1:
	CMP    USE_SEGMENT,0
	JE     G_D_2

	MOV    ES,USE_SEGMENT
	MOV    AH,49H
	INT    21H
	MOV    USE_SEGMENT,0

G_D_2:
	MOV	MIRROR_SWITCH,0
	MOV	AH,32H	; that funny undocumented one
	MOV	DL,ASCIIZ
	SUB	DL,40H
	INT	21H
	CMP	AL,0FFH
	JNE	GET_DRIVE_1
	JMP	SPEC_ERROR	; should not occur if drive ok

GET_DRIVE_1:
	MOV	AX,[BX]+2	; get sector size
	MOV	SECTOR_SIZE,AX
	MOV	ALT_SECTOR_SIZE,AX

	 PUSH  DX
	 MOV   DX,1
	 MOV   AX,0
	 DIV   SECTOR_SIZE
	 MOV   NUM_SECTORS_PER_64K,AX
	 POP   DX

	MOV	AL,[BX]+8	; get number of FATs
	MOV	NUM_FATS,AL

	MOV	AX,[BX]+6	; get number of reserved sectors
	MOV	NUM_RESERVED,AX

	MOV	AL,[BX]+4	; get number of sectors per clus
	INC	AL
	MOV	SECTORS_PER_CLU,AL

	MOV	AX,[BX]+13	       ; total cluster count + 1
	MOV	HIGHEST_CLUSTER,AX	; equals highest valid cluster #
 	SHR	AX,1	;******
 	SHR	AX,1	;   changed for MS DOS 5.0
 	MOV	TRYIN_CLUSTERS,AX	;   to allow placement of the
;       MOV	TRYIN_CLUSTERS,AX	;   control file anywhere on the
;       SUB	TRYIN_CLUSTERS,2	;   disk. 8/22/90 Jim Sesma
                                        ;
                                        ;   changed back for perf. reasons         
			;******

	AND	FAT_INFO,NOT FAT_EQU_16_BITS
	MOV	AX,HIGHEST_CLUSTER
	CMP	AX,4086
	JNA	GET_DRIVE_2
	OR	FAT_INFO,FAT_EQU_16_BITS

GET_DRIVE_2:
	CMP	cs:dos_version,400h
	JAE	GET_DRIVE_ALA_DOS_40

	MOV	CX,[BX]+11	; get first usable sector
	MOV	FIRST_DATA_SECTOR,CX

	SUB	CX,[BX]+16	; minus first directory sector
	MOV	NUM_DIR_SECTORS,CX

	MOV	AX,0
	MOV	AL,[BX]+15	; get sectors per FAT
	MOV	NUM_FAT_SECTORS,AX

	ADD	AX,[BX]+6	; dont forget reserved (BOOT)
	MOV	NUM_BOOT_FAT_SECTORS,AX

	ADD	CX,AX
	MOV	NUM_SECTORS_TO_HANDLE,CX
	MOV	AX,[BX]+16	; get 1st directory sector
	MOV	FIRST_DIRECTORY_SECTOR,AX
	JMP	SHORT GET_DRIVE_3

GET_DRIVE_ALA_DOS_40:
	MOV	CX,[BX]+11	; get first usable sector
	MOV	FIRST_DATA_SECTOR,CX

	SUB	CX,[BX]+17	; minus first directory sector
	MOV	NUM_DIR_SECTORS,CX

	MOV	AX,[BX]+15	; get sectors per FAT
	MOV	NUM_FAT_SECTORS,AX

	ADD	AX,[BX]+6	; dont forget reserved (BOOT)
	MOV	NUM_BOOT_FAT_SECTORS,AX

	ADD	CX,AX
	MOV	NUM_SECTORS_TO_HANDLE,CX
	MOV	AX,[BX]+17	; get 1st directory sector
	MOV	FIRST_DIRECTORY_SECTOR,AX

GET_DRIVE_3:
	MOV	AX,[BX]+6	; dont forget reserved (BOOT)
	MOV	NUM_BOOT_SECTORS,AX

	MOV	DX,0
	MOV	AX,0FFFFH
	DIV	SECTOR_SIZE	; get # sectors in 64K-1
	MOV	SECTORS_MAX_AT_A_TIME,AX
	MUL	SECTOR_SIZE
	MOV	CX,4
	SHR	AX,CL
	MOV	MAX_WRITE_UPDATE_DS,AX

;	LEA	SI,END_PROG
;	MOV	OUR_SPOTS_DIR_OFFSET,SI
;	ADD	SI,SECTOR_SIZE
;	MOV	OUR_SPOTS_FAT_OFFSET,SI
;	ADD	SI,SECTOR_SIZE
;	ADD	SI,SECTOR_SIZE
;	MOV	USE_POINTER,SI
;	MOV	USE_SEGMENT,CS
	MOV	OUR_SPOTS_DIR_OFFSET,0
	MOV	BX,SECTOR_SIZE
	MOV	OUR_SPOTS_FAT_OFFSET,BX
	ADD	BX,SECTOR_SIZE
	ADD	BX,SECTOR_SIZE
	MOV	CX,4
	SHR	BX,CL
	MOV	AH,48H
	INT	21H
	JC	GET_DRIVE_NO_MEM_JMP
	MOV	OUR_THREE_SECTOR_SEGMENT,AX

	MOV	BX,0FFFFH
	MOV	USE_AREA_PARAS,BX
	MOV	AH,48H
	INT	21H
	JNC	GOT_ALL_MEM
	MOV	USE_AREA_PARAS,BX

	MOV	AH,48H
	INT	21H
	JNC	GOT_ALL_MEM

GET_DRIVE_NO_MEM_JMP:
	JMP	GET_DRIVE_NO_MEM

GOT_ALL_MEM:
	MOV	USE_SEGMENT,AX
	MOV	USE_POINTER,0

	CMP	USE_AREA_PARAS,512*10/16 ; at least room for 10 sectors?
	JB	GET_DRIVE_NO_MEM_JMP	 ; no, fail

	MOV	ES,USE_SEGMENT
	MOV	DI,USE_POINTER
	MOV	CX,NUM_SECTORS_TO_HANDLE
	SHL	CX,1
	SHL	CX,1
	SHL	CX,1
	ADD	CX,ENTRY_LEN
	PUSH	CX
	MOV	AL,0
	REP	STOSB	; clear table

	POP	AX
	ADD	AX,CTL_LEN
	MOV	DX,0
	DIV	SECTOR_SIZE
	CMP	DX,0
	JE	GET_DRIVE_NUM_CTL_SECTORS
	INC	AX

GET_DRIVE_NUM_CTL_SECTORS:
	CMP	AX,2
	JA	GET_DRIVE_NUM_CTL_SECTORS_1
	MOV	AX,2

GET_DRIVE_NUM_CTL_SECTORS_1:
	MOV	NUM_CTL_SECTORS,AX

	LDS	SI,USE_DWORD
	LEA	AX,XLAT_TABLE
	MOV	XLAT_POINTER,AX
	MOV	CL,3
	MOV	AX,NUM_SECTORS_TO_HANDLE
	MOV	LIST_COUNT,AX
	SHL	AX,CL
	ADD	AX,XLAT_POINTER
	MOV	XLAT_END,AX

	MOV	AL,ASCIIZ
	MOV	DRIVE,AL

	MOV	AX,NUM_SECTORS_TO_HANDLE
	ADD	AX,NUM_CTL_SECTORS
	MUL	SECTOR_SIZE
	MOV	CX,16
	DIV	CX
	CMP	AX,USE_AREA_PARAS
	JA	GET_DRIVE_NO_MEM

	MOV	AX,ES
	ADD	DI,15
	MOV	CL,4
	SHR	DI,CL
	ADD	AX,DI
	MOV	IO_AREA_OFFSET,0
	MOV	IO_AREA_SEGMENT,AX

	cmp	cs:dos_version,031Fh	;Version 3.31 ?
	jb	get_drive_thru

get_drive_zenith:
	MOV	AL,SECTORS_PER_CLU
	MOV	AH,0
	MUL	HIGHEST_CLUSTER
	ADD	AX,FIRST_DATA_SECTOR
	ADC	DX,0
	CMP	DX,0
	JE	GET_DRIVE_THRU
	OR	MIRROR_SWITCH,ABS_25_HARD_WAY

GET_DRIVE_THRU:
	CLC
	RET

GET_DRIVE_NO_MEM:
	LEA	DX,NO_MEM_MSG
	STC
	RET

SPEC_ERROR:
	LEA	DX,DRIVE_SPEC_ERROR
	STC
	RET

GET_DRIVE_SPECS ENDP



CHECK_FOR_OUR_SPOT PROC NEAR

; The intent here is to see if there's a spot big enuf in the last
;  part of the hard drive for our control file.  If so,
;  allocate it the hard way, otherwise tell 'em we kant go on (meeting
;  like this).	If we're allocating in other than the last cluster,
;  we gotta check to make sure one or more of the file(s) that's in
;  the way ain't one of our old ones (with a new name of course).

; Afore we do this, check to see if there's one there. If so, we don't
;  need to go tew all the trubble, UNLESS, of course, that it ain't in
;  the last cluster. Ifin it is, that's OK, but if not, we gotta quit.

	MOV	AH,0DH	; no bufferin' DOS
	INT	21H

	MOV	AX,FIRST_DIRECTORY_SECTOR
	MOV	ABS_25_SEC_LO,AX
	MOV	ABS_25_SEC_HI,0
;	 MOV   OUR_SPOTS_CURR_DIR_SECTOR,DX
	MOV	AX,NUM_DIR_SECTORS
	MOV	OUR_SPOTS_DIR_SECTORS_LEFT,AX

C_F_O_S_READ_DIR_LOOP1:
         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   C_F_O_S_BROKE        

;	MOV	DS,CS_SAVE
	MOV	DS,OUR_THREE_SECTOR_SEGMENT
	MOV	BX,OUR_SPOTS_DIR_OFFSET
	MOV	CX,1
	CALL	ABS_25
	LEA	DX,READ_ERROR_MSG
	JNC	C_F_O_S_CHECK_DIR1

C_F_O_S_BROKE:
	MOV	RETURN_CODE,4
	JMP	C_F_O_S_STC

C_F_O_S_CHECK_DIR1:
	MOV	CL,5
	MOV	AX,SECTOR_SIZE
	SHR	AX,CL
	MOV	CX,AX

C_F_O_S_CHECK_DIR_LOOP1:
	MOV	ES,CS_SAVE
	LEA	DI,OUR_SPOTS_FILE_NAME
	MOV	SI,BX

	CMP	BYTE PTR [SI],0 	; anymore entries?
	JE	C_F_O_S_NO_DIRS1	; no, branch

	PUSH	CX
	MOV	CX,11
	REPE	CMPSB
	POP	CX
	JE	C_F_O_S_GOT_OUR_DIR1
	ADD	BX,DIR_ENTRY_LEN	; tew next wun
	LOOP	C_F_O_S_CHECK_DIR_LOOP1

	DEC	OUR_SPOTS_DIR_SECTORS_LEFT   ; minus wun
	JZ	C_F_O_S_NO_DIRS1	    ; oops, none left
	ADD	ABS_25_SEC_LO,1
	ADC	ABS_25_SEC_HI,0
	JMP	C_F_O_S_READ_DIR_LOOP1

C_F_O_S_NO_DIRS1:
	JMP	SHORT C_F_O_S_CREATE_WUN

C_F_O_S_GOT_OUR_DIR1:
	MOV	AX,WORD PTR DS:[BX+26]	; grab starting cluster
	MOV	BX,HIGHEST_CLUSTER	; grab highest possible cluster
	CMP	AX,BX	; in the highest?
	JE	C_F_O_S_RE_USE_HIGHEST	; yep, that's kool
	SUB	BX,TRYIN_CLUSTERS	; gets first clu it can't be
	CMP	AX,BX	; are we above that?
	JA	C_F_O_S_RE_USE_OK	; yep

        TEST    SWITCH,QUIET_MODE       ; are we in MSDOS FORMAT?
        JNZ     C_F_O_S_RE_USE_OK       ; yes, branch

        LEA     DX,CANT_REUSE_ERROR_MSG
        CALL    ISSUE_MSG_TO_SCREEN

C_F_O_S_RE_USE_OK:
	CALL	MIRORSAV_DELETE
	JMP	SHORT C_F_O_S_CREATE_WUN

C_F_O_S_RE_USE_HIGHEST:
	JMP	C_F_O_S_CLC

C_F_O_S_CREATE_WUN:

; Ain't wun thar, sew, let's check things out from the back

	MOV	AH,0DH	; no bufferin' DOS
	INT	21H
	MOV	OUR_SPOTS_FAT_SECTOR,0FFFFH
	MOV	START_CLU,0
	MOV	START_CLU_LAST,0
	MOV	START_CLU_CONT,0
	MOV	REL_SECTOR,0
	MOV	REL_CLUSTER,0
	MOV	REL_CLUSTER_LAST,0

	MOV	AX,HIGHEST_CLUSTER
	MOV	OUR_SPOTS_START_CLU,AX

C_F_O_S_CHECK_NEXT_CLUSTER:
	MOV	DX,0
	MOV	AX,OUR_SPOTS_START_CLU
	TEST	FAT_INFO,FAT_EQU_16_BITS
	JZ	C_F_O_S_FAT_EQU_12_BITS
	ADD	AX,AX	; gets rel byte disp in FAT
	ADC	DX,0
	JMP	SHORT C_F_O_S_GET_FAT

C_F_O_S_FAT_EQU_12_BITS:
	MOV	BX,AX	; highest cluster
	SHR	AX,1	; times 1.5 gets
	ADD	AX,BX	;  rel byte disp in FAT
	ADC	DX,0

C_F_O_S_GET_FAT:
	DIV	SECTOR_SIZE	; bytes in FATs except last two
	MOV	OUR_SPOTS_FAT_SECTOR_DISP,DX ; save it

	CMP	OUR_SPOTS_FAT_SECTOR,AX ; got the right sector already?
	JE	C_F_O_S_GOT_FAT 	; yes, don't get it again

	MOV	OUR_SPOTS_FAT_SECTOR,AX ; save
	CMP	AX,0
	JE	C_F_O_S_READ_FAT
	MOV	AX,SECTOR_SIZE
	DEC	AX
	CMP	AX,DX
	JA	C_F_O_S_READ_FAT
	DEC	OUR_SPOTS_FAT_SECTOR
	ADD	DX,SECTOR_SIZE
	MOV	OUR_SPOTS_FAT_SECTOR_DISP,DX ; save it

C_F_O_S_READ_FAT:
         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   C_F_O_S_BROKE_1

;	MOV	DS,CS_SAVE
	MOV	DS,OUR_THREE_SECTOR_SEGMENT
	MOV	BX,OUR_SPOTS_FAT_OFFSET
	MOV	CX,2
	MOV	AX,OUR_SPOTS_FAT_SECTOR
	ADD	AX,NUM_RESERVED
	MOV	ABS_25_SEC_LO,AX
	MOV	ABS_25_SEC_HI,0
	CALL	ABS_25	; read FAT sector(s)
	LEA	DX,READ_ERROR_MSG
	JNC	C_F_O_S_GOT_FAT

C_F_O_S_BROKE_1:
	MOV	RETURN_CODE,4
	JMP	C_F_O_S_STC

C_F_O_S_GOT_FAT:
	MOV	SI,OUR_SPOTS_FAT_SECTOR_DISP
	MOV	BX,OUR_SPOTS_FAT_OFFSET         ;M009

	MOV	AX,WORD PTR DS:[BX+SI]	; get value out of FAT
	TEST	FAT_INFO,FAT_EQU_16_BITS ; 12 bit FAT?
	JNZ	C_F_O_S_CHECK_IF_AVAILABLE


; following should tell whether to throw away upper or lower 4 nibbles

	MOV	CL,4

	TEST	OUR_SPOTS_START_CLU,1
	JNZ	C_F_O_S_ODD	; nope

	AND	AX,0FFFH	; clear out hi-order 4 nibbles
	JMP	SHORT C_F_O_S_CHECK_IF_AVAILABLE

C_F_O_S_ODD:
	SHR	AX,CL	;  4 bits

C_F_O_S_CHECK_IF_AVAILABLE:
	CMP	AX,0	; available?
	JE	C_F_O_S_AVAIL	; yep

C_F_O_S_TRY_NEXT:
	MOV	FAT_NEXT_CLUSTER,AX
	CALL	C_F_O_S_CHECK_IT_OUT	; be sure a phony doesn't exist
	JC	C_F_O_S_STC_JMP

	INC	CLUSTERS_WEVE_TRIED
	MOV	AX,TRYIN_CLUSTERS
	CMP	CLUSTERS_WEVE_TRIED,AX
	JAE	C_F_O_S_NOT_AVAIL
	DEC	OUR_SPOTS_START_CLU
	JMP	C_F_O_S_CHECK_NEXT_CLUSTER

C_F_O_S_NOT_AVAIL:
	LEA	DX,NO_SPOTS_ERROR_MSG

C_F_O_S_STC_JMP:
	JMP	C_F_O_S_STC

C_F_O_S_AVAIL:

; Now read the directory to find a spot to create a directory entry
; for our little quile.

	MOV	AH,0DH	; no bufferin' DOS
	INT	21H

	MOV	AX,FIRST_DIRECTORY_SECTOR
	MOV	ABS_25_SEC_LO,AX
	MOV	ABS_25_SEC_HI,0
	MOV	AX,NUM_DIR_SECTORS
	MOV	OUR_SPOTS_DIR_SECTORS_LEFT,AX

C_F_O_S_READ_DIR_LOOP:
         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   C_F_O_S_BROKE_2

;	MOV	DS,CS_SAVE
	MOV	DS,OUR_THREE_SECTOR_SEGMENT
	MOV	BX,OUR_SPOTS_DIR_OFFSET
	MOV	CX,1
	CALL	ABS_25
	LEA	DX,READ_ERROR_MSG
	JNC	C_F_O_S_CHECK_DIR

C_F_O_S_BROKE_2:
	MOV	RETURN_CODE,4
	JMP	C_F_O_S_STC

C_F_O_S_CHECK_DIR:
	MOV	CL,5
	MOV	AX,SECTOR_SIZE
	SHR	AX,CL
	MOV	CX,AX	; calc # dir entries per sector

C_F_O_S_CHECK_DIR_LOOP:
	CMP	BYTE PTR DS:[BX],0E5H	; erased?
	JE	C_F_O_S_GOT_A_DIR	; yup
	CMP	BYTE PTR DS:[BX],0	; new wun?
	JE	C_F_O_S_GOT_A_DIR	; yup
	ADD	BX,DIR_ENTRY_LEN	; tew next wun
	LOOP	C_F_O_S_CHECK_DIR_LOOP

	DEC	OUR_SPOTS_DIR_SECTORS_LEFT   ; minus wun
	JZ	C_F_O_S_NO_DIRS 	    ; oops, none left
	ADD	ABS_25_SEC_LO,1
	ADC	ABS_25_SEC_HI,0
	JMP	C_F_O_S_READ_DIR_LOOP

C_F_O_S_NO_DIRS:
	LEA	DX,NO_DIRS_ERROR_MSG
	JMP	C_F_O_S_STC

C_F_O_S_GOT_A_DIR:
;	MOV	ES,CS_SAVE
	MOV	ES,OUR_THREE_SECTOR_SEGMENT
	MOV	DI,BX
	MOV	DS,CS_SAVE
	LEA	SI,OUR_SPOTS_DIR_ENTRY
	MOV	CX,DIR_ENTRY_LEN
	REP	MOVSB

; Now, update directory and FAT (GULP!)

	MOV	AH,0DH	; no bufferin' DOS
	INT	21H

         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   C_F_O_S_BROKE_3

	MOV	DS,OUR_THREE_SECTOR_SEGMENT
	MOV	BX,OUR_SPOTS_DIR_OFFSET
	MOV	CX,1
	CALL	ABS_26
	LEA	DX,WRITE_SYS_ERROR_MSG
	JNC	C_F_O_S_UPDATE_FAT

C_F_O_S_BROKE_3:
	MOV	RETURN_CODE,4
	JMP	C_F_O_S_STC

C_F_O_S_UPDATE_FAT:
;	MOV	DS,CS_SAVE
	MOV	BX,OUR_SPOTS_FAT_OFFSET
	MOV	SI,OUR_SPOTS_FAT_SECTOR_DISP

	TEST	FAT_INFO,FAT_EQU_16_BITS ; 12 bit FAT?
	JNZ	C_F_O_S_U_F_16	; nope

	MOV	AX,0FFFH
	TEST	OUR_SPOTS_START_CLU,1	; clu # odd?
	JZ	C_F_O_S_U_F_12	; nope

	MOV	CL,4
	SHL	AX,CL	; shift left about fore

C_F_O_S_U_F_12:
	OR	WORD PTR DS:[BX+SI],AX	; new FAT value
	JMP	SHORT C_F_O_S_WRITE_NEW_FAT

C_F_O_S_U_F_16:
	MOV	AX,0FFFFH
	MOV	WORD PTR DS:[BX+SI],AX	; new FAT value

C_F_O_S_WRITE_NEW_FAT:
	MOV	CH,0
	MOV	CL,NUM_FATS
	MOV	AX,OUR_SPOTS_FAT_SECTOR
	ADD	AX,NUM_RESERVED
	MOV	ABS_25_SEC_LO,AX
	MOV	ABS_25_SEC_HI,0

C_F_O_S_WRITE_NEW_FAT_LOOP:
	PUSH	CX
	MOV	CX,2

	MOV	AX,SECTOR_SIZE
	DEC	AX
	CMP	OUR_SPOTS_FAT_SECTOR_DISP,AX
	JNB	C_F_O_S_WRITE_FAT_ABS

	MOV	CX,1

C_F_O_S_WRITE_FAT_ABS:
	CALL	ABS_26
	POP	CX
	JC	C_F_O_S_UPDATE_FAT_ERROR

	MOV	AX,NUM_FAT_SECTORS
	ADD	ABS_25_SEC_LO,AX	; for second copy of FAT
	ADC	ABS_25_SEC_HI,0 	; for second copy of FAT
	LOOP	C_F_O_S_WRITE_NEW_FAT_LOOP

	JMP	SHORT C_F_O_S_CLC

C_F_O_S_UPDATE_FAT_ERROR:
	LEA	DX,WRITE_SYS_ERROR_MSG
	MOV	RETURN_CODE,4
	JMP	SHORT C_F_O_S_STC

C_F_O_S_CLC:
	CLC

C_F_O_S_EXIT:
	RET

C_F_O_S_STC:
	STC
	JMP	C_F_O_S_EXIT

CHECK_FOR_OUR_SPOT ENDP
;
;------------------------------------------------------



C_F_O_S_CHECK_IT_OUT PROC NEAR

; This cluster is allocated above our little file. Make sure it isn't
;  an old one of ours.

	AND	FAT_INFO,NOT CLU_IS_A_BADDIE
	MOV	AX,0FF7H
	TEST	FAT_INFO,FAT_EQU_16_BITS
	JZ	C_F_O_S_CIO_IZIT_MARKED

	MOV	AX,0FFF7H

C_F_O_S_CIO_IZIT_MARKED:
	CMP	FAT_NEXT_CLUSTER,AX	; a baddie?
	JNE	C_F_O_S_CIO_READ_IT	; nope
	OR	FAT_INFO,CLU_IS_A_BADDIE

C_F_O_S_CIO_READ_IT:
         TEST  BREAK_SWITCH,BEEN_BROKE
         JNZ   C_F_O_S_CIO_STC

	MOV	AX,OUR_SPOTS_START_CLU	 ; get clu #
	CALL	CONVERT_CLU_TO_LOG_SECTOR ; get log sector
	MOV	CX,1
;	MOV	DS,CS_SAVE
	MOV	DS,OUR_THREE_SECTOR_SEGMENT
	MOV	BX,OUR_SPOTS_DIR_OFFSET
	CALL	ABS_25
	JNC	C_F_O_S_CIO_READ_OK
	JMP	SHORT C_F_O_S_CIO_CLC	; ifin it can't be read,
			;  no need to worry about it

C_F_O_S_CIO_READ_OK:
	PUSH	ES
	MOV	ES,CS_SAVE
	LEA	DI,OUR_SIGNATURE
	MOV	SI,BX	; point to sector
	ADD	SI,OUR_SIGNATURE_OFFSET ;M009: Sig. offset within Control file.
	MOV	CX,OUR_SIGNATURE_LEN
	REPE	CMPSB
	POP	ES
	JNE	C_F_O_S_CIO_CLC

	TEST	FAT_INFO,CLU_IS_A_BADDIE
	JZ	C_F_O_S_CIO_NORMAL

; We were able to read the furst sector of a bad cluster. And,
;  below and mahold, we matched. Sew, write sum binzers into it.

	PUSH	ES
;	MOV	ES,CS_SAVE
	MOV	ES,OUR_THREE_SECTOR_SEGMENT
	MOV	DI,OUR_SPOTS_DIR_OFFSET
	MOV	AL,0
	MOV	CX,SECTOR_SIZE
	REP	STOSB
	POP	ES

	MOV	CX,1
;	MOV	DS,CS_SAVE
	MOV	BX,OUR_SPOTS_DIR_OFFSET
	CALL	ABS_26
	JNC	C_F_O_S_CIO_CLC 	; OK, if write worked

C_F_O_S_CIO_NORMAL:
	LEA	DX,FOUND_ONE_LIKE_US_MSG
	JMP	SHORT C_F_O_S_CIO_STC

C_F_O_S_CIO_CLC:
	CLC
	JMP	SHORT C_F_O_S_CIO_EXIT

C_F_O_S_CIO_STC:
	STC

C_F_O_S_CIO_EXIT:
	RET

C_F_O_S_CHECK_IT_OUT ENDP
;
;------------------------------------------------------



CONVERT_CLU_TO_LOG_SECTOR PROC NEAR

; To convert clu # to a logical sector #:

;  (clu#-2) * sectors per cluster + first data sector

	PUSH	BX
	SUB	AX,2	; minus tew
	MOV	BH,0
	MOV	BL,SECTORS_PER_CLU
	MUL	BX	; times sectors per clu
	ADD	AX,FIRST_DATA_SECTOR	; that shud be about it
	ADC	DX,0
	MOV	ABS_25_SEC_LO,AX
	MOV	ABS_25_SEC_HI,DX
	POP	BX
	RET

CONVERT_CLU_TO_LOG_SECTOR ENDP
;
;------------------------------------------------------




;   Given the starting cluster # (out of the directory) and the relative
;   sector # in the file desired, return the logical sector # on the
;   media.

;	 AX = starting cluster #
;	 DI = relative sector #
;    Returned,
;	 With carry clear,
;	     ABS_25_SEC_LO and ABS_25_SEC_HI have logical sector number
;	 With carry set,
;	 AX = FFF7 for bad cluster or FFFF for EOF.

;	 Remember, if AX returns with FFFF, we're already at
;	 EOF. If AX contains a FFF7, something in wrong for
;	 this is a BAD sector.

FIND_LOG_SECTOR PROC NEAR
	PUSH	DS
	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	DI
	PUSH	SI
;	MOV	DS,CS_SAVE
	MOV	DS,OUR_THREE_SECTOR_SEGMENT
	MOV	START_CLU,AX


;	 First, we will find out which relative Cluster in the file
;	 is involved and we will also have the relative sector in the
;	 cluster.

F_L_S_1:
	MOV	AX,DI	; get relative clu/sec in file
	MOV	DX,0
	MOV	BL,SECTORS_PER_CLU
	MOV	BH,0
	DIV	BX

;	 If the starting cluster # is the same we may not need to
;	 scan thru the entire FAT, just take off from where we were
;	 before.

	MOV	BX,START_CLU
	CMP	START_CLU_LAST,BX	; might this be a continuation?
	JE	FIND_LOG_SECTOR_CONTINUE ; yes, branch
	MOV	START_CLU_LAST,BX	; no, save starting clu#
	JMP	SHORT FIND_LOG_SECTOR_OLD_WAY

FIND_LOG_SECTOR_CONTINUE:

;	 Now, if the relative cluster is equal or greater than the
;	 last, we can start from the last.

	CMP	AX,REL_CLUSTER_LAST	; are we in range?
	JB	FIND_LOG_SECTOR_OLD_WAY  ; no, branch
	PUSH	AX
	SUB	AX,REL_CLUSTER_LAST	; get amount to go
	POP	REL_CLUSTER_LAST
	MOV	REL_CLUSTER,AX	; set it up
	MOV	AX,START_CLU_CONT	; get associated cluster #
	MOV	START_CLU,AX	; and save it
	MOV	REL_SECTOR,DX	; save relative sector #
	MOV	BX,OUR_SPOTS_FAT_OFFSET  ; point to FAT
	JMP	SHORT CHECK_NEXT_CLUSTER

;	 Now, AX has the relative cluster in the file and DX has the
;	 relative sector in the cluster

FIND_LOG_SECTOR_OLD_WAY:
	MOV	REL_SECTOR,DX	; save relative sector in clus
	MOV	REL_CLUSTER,AX	; save relative cluster in fil
	MOV	REL_CLUSTER_LAST,AX	; save relative cluster in fil
	MOV	BX,OUR_SPOTS_FAT_OFFSET  ; point to FAT

CHECK_NEXT_CLUSTER:
	CMP	REL_CLUSTER,0	; are we at the right cluster
	JE	AT_RIGHT_CLUSTER

;	 Now we will move to the next cluster in the chain.

	TEST	FAT_INFO,FAT_EQU_16_BITS
	JNZ	PROCESS_16_BIT_FAT

;	 This will follow the chain thru the 12 bit FAT.

	DEC	REL_CLUSTER

	MOV	DX,0
	MOV	AX,START_CLU	; pick up relative sector number
	SHR	AX,1	; divide by 2
	ADD	AX,START_CLU	; equals times 1.5

	CALL	GET_RIGHT_SECTOR

	MOV	AX,[BX+DI]	; pick up next
	TEST	START_CLU,1	; is the sector number odd?
	JZ	GF_1	; no, branch
	SHR	AX,1
	SHR	AX,1
	SHR	AX,1
	SHR	AX,1	; shift to right 4 bits

GF_1:
	AND	AX,0FFFH	; clear hi-order byte
	MOV	START_CLU,AX
	CMP	AX,0FF7H	; at end or error?
	JE	FAT_ERROR_JMP
	JA	AT_RIGHT_CLUSTER
	JMP	CHECK_NEXT_CLUSTER

FAT_ERROR_JMP:
	JMP	FAT_ERROR

PROCESS_16_BIT_FAT:
	DEC	REL_CLUSTER

	MOV	DX,0
	MOV	AX,START_CLU	; get cluster #
	ADD	AX,AX	; times 2
	ADC	DX,0

	CALL	GET_RIGHT_SECTOR

	MOV	AX,[BX+DI]	; get next cluster #
	MOV	START_CLU,AX	; save it
	CMP	START_CLU,0FFF7H	; at end or error?
	JE	FAT_ERROR
	JA	AT_RIGHT_CLUSTER	; at EOF
	JMP	CHECK_NEXT_CLUSTER

;	 Now, if all is cool, compute logical sector # of INT 25H.

AT_RIGHT_CLUSTER:
	MOV	AX,START_CLU
	MOV	START_CLU_CONT,AX
	MOV	AX,REL_CLUSTER
	SUB	REL_CLUSTER_LAST,AX

	CMP	AX,0
	JNE	BEYOND_EOF

	TEST	FAT_INFO,FAT_EQU_16_BITS
	JNZ	CHECK_FOR_16_BIT_EOF

	CMP	START_CLU,0FF7H
	JE	FAT_ERROR
	JA	BEYOND_EOF
	JMP	SHORT NOT_EOF

CHECK_FOR_16_BIT_EOF:
	CMP	START_CLU,0FFF7H
	JE	FAT_ERROR
	JA	BEYOND_EOF

NOT_EOF:
	SUB	START_CLU,2	; sub 2
	MOV	AX,START_CLU
	MOV	BH,0
	MOV	BL,SECTORS_PER_CLU
	MUL	BX
	ADD	AX,REL_SECTOR	; rel sect # in cluster
	ADC	DX,0
	ADD	AX,FIRST_DATA_SECTOR	; bump past FAT and DIR
	ADC	DX,0
	MOV	ABS_25_SEC_LO,AX
	MOV	ABS_25_SEC_HI,DX
	CLC

FIND_LOG_EXIT:
	POP	SI
	POP	DI
	POP	DX
	POP	CX
	POP	BX
	POP	DS
	RET

FAT_ERROR:
	MOV	START_CLU_LAST,0FFFFH
	MOV	AX,0FFF7H
	STC
	JMP	FIND_LOG_EXIT

BEYOND_EOF:
	STC
	MOV	AX,0FFFFH
	JMP	FIND_LOG_EXIT

FIND_LOG_SECTOR ENDP
;
;------------------------------------------------------



GET_RIGHT_SECTOR PROC NEAR
	PUSH	AX
	PUSH	CX
	PUSH	DX

	DIV	SECTOR_SIZE
	MOV	DI,DX
	CMP	AX,OUR_SPOTS_FAT_SECTOR  ; got it in memory?
	JE	GET_RIGHT_OUT	; yes, branch
	JB	GET_NEW_SECTORS          ; no, gotta read more
	MOV	CX,AX	; may be in second half
	DEC	CX
	CMP	CX,OUR_SPOTS_FAT_SECTOR  ; do they need second half?
	JNE	GET_NEW_SECTORS          ; no, forget it
	MOV	CX,DI	; but do they need very last byte
	INC	CX
	CMP	CX,SECTOR_SIZE	; spanning across last byte
	JNB	GET_NEW_SECTORS          ; yes, go read anyway
	ADD	DI,SECTOR_SIZE	; no, just adjust offset
	JMP	SHORT GET_RIGHT_OUT	; and get out

GET_NEW_SECTORS:
	MOV	OUR_SPOTS_FAT_SECTOR,AX
	ADD	AX,NUM_RESERVED
	MOV	ABS_25_SEC_LO,AX
	MOV	ABS_25_SEC_HI,0
	MOV	CX,2
	CALL	ABS_25

GET_RIGHT_OUT:
	POP	DX
	POP	CX
	POP	AX
	RET
GET_RIGHT_SECTOR ENDP
;
;------------------------------------------------------



GET_OLD_INFO PROC NEAR
	PUSH	DS
	PUSH	ES
	MOV	ES,CS_SAVE
	MOV	DS,CS_SAVE
	LEA	DI,OUR_SPOTS_OLD
	MOV	AX,0
	MOV	CX,4
	REP	STOSW

	TEST	SWITCH,ONE_FILE_ONLY
	JNZ	G_O_I_EXIT

	MOV	AX,3D00H
	LEA	DX,OUR_SPOT_ASCIIZ
	INT	21H
	JC	G_O_I_EXIT

	MOV	BX,AX
	MOV	CX,OUR_SPOTS_NEW_LEN
	MOV	AH,3FH
	MOV	DS,OUR_THREE_SECTOR_SEGMENT
	MOV	DX,OUR_SPOTS_DIR_OFFSET
	INT	21H
	JC	G_O_I_EXIT_CLOSE

	CMP	AX,CX
	JB	G_O_I_EXIT_CLOSE

	LEA	DI,OUR_SPOTS_OLD
	MOV	SI,OUR_SPOTS_DIR_OFFSET
	MOVSW
	MOVSW
	MOV	SI,OUR_SPOTS_DIR_OFFSET
	ADD	SI,OUR_SPOTS_YEAR-OUR_SPOTS_IO_AREA
	MOV	CX,3
	REP	MOVSW

G_O_I_EXIT_CLOSE:
	MOV	AH,3EH
	INT	21H

G_O_I_EXIT:
	POP	ES
	POP	DS
	RET

GET_OLD_INFO ENDP
;
;------------------------------------------------------



HANDLE_TWO_FILES PROC NEAR
	PUSH	DS
	PUSH	ES
	MOV	DS,CS_SAVE
	MOV	ES,CS_SAVE

	TEST	SWITCH,ONE_FILE_ONLY	; one file only?
	JZ	H_T_F_1 	; no, branch

	MOV	AX,4301H
	MOV	CX,0
	LEA	DX,ASCIIZ_BACKUP	; unprotect old backup file
	INT	21H
	JC	H_T_F_EXIT	; no, branch

	MOV	AH,41H
	LEA	DX,ASCIIZ_BACKUP	; delete old backup file
	INT	21H

	JMP	SHORT H_T_F_EXIT	; no, branch

H_T_F_1:
	MOV	AX,4301H
	MOV	CX,0
	LEA	DX,ASCIIZ_BACKUP	; unprotect old backup file
	INT	21H
	JC	H_T_F_2

	LEA	DI,ASCIIZ_TEMP
	MOV	AH,56H	; make it temporary
	INT	21H

H_T_F_2:
	MOV	AX,4301H
	MOV	CX,0
	LEA	DX,ASCIIZ	; unprotect current file
	INT	21H

	LEA	DI,ASCIIZ_BACKUP
	MOV	AH,56H	; make it the backup file
	INT	21H

	MOV	AX,3D02H
	LEA	DX,ASCIIZ_BACKUP
	INT	21H	; open backup file
	JC	H_T_F_3 	; no, branch

	MOV	BX,AX
	MOV	AX,4200H
	MOV	CX,0
	MOV	DX,BACKUP_LSEEK 	; point to a spot
	INT	21H

	LEA	DX,BACKUP_IND_BYTE
	MOV	CX,1
	MOV	AH,40H	; remember it is a backup
	INT	21H

	MOV	AX,5700H
	INT	21H
	MOV	AX,5701H
	INT	21H

	MOV	AH,3EH	; close it
	INT	21H

H_T_F_3:
	MOV	AX,4301H
	MOV	CX,21H	; archive+read-only
	LEA	DX,ASCIIZ_BACKUP	; protect it
	INT	21H

	LEA	DI,ASCIIZ	; rename temporary
	LEA	DX,ASCIIZ_TEMP	; to current
	MOV	AH,56H
	INT	21H

H_T_F_EXIT:
	POP	ES
	POP	DS
	RET

HANDLE_TWO_FILES ENDP
;
;------------------------------------------------------



UPDATE_BACKUP_FILE PROC NEAR
	MOV	AX,4301H
	MOV	CX,0
	LEA	DX,ASCIIZ_BACKUP	; unprotect old backup file
	INT	21H

	MOV	AX,3D02H
	LEA	DX,ASCIIZ_BACKUP
	INT	21H	; open backup file
	JC	U_B_F_EXIT

	MOV	BX,AX
	MOV	AX,4200H
	MOV	CX,0
	MOV	DX,BACKUP_LSEEK+1	; point to a spot
	INT	21H

	LEA	DX,OUR_SPOTS_IO_AREA
	MOV	CX,2
	MOV	AH,40H	; store current LSN
	INT	21H

	MOV	AX,5700H	; get the files date and time
	INT	21H

	MOV	AX,5701H	; set the files date and time
	INT	21H


	MOV	AH,3EH	; close it
	INT	21H

	MOV	AX,4301H
	MOV	CX,21H	; archive+read-only
	LEA	DX,ASCIIZ_BACKUP	; protect it
	INT	21H

U_B_F_EXIT:
	RET

UPDATE_BACKUP_FILE ENDP




ABS_25	 PROC NEAR

;	 CX    = num of sectors to read
;	 DS:BX = transfer (read into) address
;	 ABS_25_LIST will contain the sector number

	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	SI
	PUSH	DI
	PUSH	BP
	PUSH	DS
	 PUSH  ABS_25_SEC_LO
	 PUSH  ABS_25_SEC_HI

	 CALL  NORMALIZE_DS_BX

	MOV	ABS_25_OFF,BX
	MOV	ABS_25_SEG,DS
	 MOV   TOTAL_SECTORS_TO_READ,CX

ABS_25_LOOP:
	 MOV   CX,TOTAL_SECTORS_TO_READ
	 JCXZ  ABS_25_THRU

	 CMP   CX,NUM_SECTORS_PER_64K
	 JBE   ABS_25_CONT
	 MOV   CX,NUM_SECTORS_PER_64K

ABS_25_CONT:
	MOV    ABS_25_COUNT,CX
	SUB    TOTAL_SECTORS_TO_READ,CX

; 5-1-89 GWD.  At least now we'll definitely return an error.
;	mov	ax,040Ch	;Sector-not-found + General error.
;	mov	dx,128
;	cmp	dx,cx	;Trying more than 128 sectors?
;	jb	abs_25_exit	;Yes.  Return the error.

	MOV	AL,ASCIIZ
	SUB	AL,41H
	MOV	DX,ABS_25_SEC_LO
	TEST	MIRROR_SWITCH,ABS_25_HARD_WAY
	JZ	ABS_25_READ
	MOV	CX,-1
	MOV	DS,CS_SAVE
	LEA	BX,ABS_25_LIST

ABS_25_READ:
	INT	25H
	JC	ABS_25_ERROR
	POPF
	 ADD   ABS_25_SEG,1000H
	 MOV   AX,NUM_SECTORS_PER_64K
	 ADD   ABS_25_SEC_LO,AX
	 ADC   ABS_25_SEC_HI,0
	 JMP   ABS_25_LOOP

ABS_25_THRU:
	CLC

ABS_25_EXIT:
	 POP   ABS_25_SEC_HI
	 POP   ABS_25_SEC_LO
	POP	DS
	POP	BP
	POP	DI
	POP	SI
	POP	DX
	POP	CX
	POP	BX
	RET

ABS_25_ERROR:
	POPF
	STC
	JMP	ABS_25_EXIT

ABS_25	 ENDP



NORMALIZE_DS_BX PROC NEAR
	 PUSH  AX
	 PUSH  CX
	 PUSH  BX
	 MOV   AX,DS
	 MOV   CL,4
	 SHR   BX,CL
	 ADD   AX,BX
	 MOV   DS,AX
	 POP   BX
	 AND   BX,0FH
	 POP   CX
	 POP   AX
	 RET

NORMALIZE_DS_BX ENDP



ABS_26	 PROC NEAR

;	 CX    = num of sectors to write
;	 DS:BX = transfer (write from) address
;	 ABS_25_LIST will contain the sector number

	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	SI
	PUSH	DI
	PUSH	BP
	PUSH	DS

	MOV	ABS_25_OFF,BX
	MOV	ABS_25_SEG,DS
	MOV	ABS_25_COUNT,CX

; 5-1-89 GWD.  At least now we'll definitely return an error.
	mov	ax,040Ch	;Sector-not-found + General error.
	mov	dx,128
	cmp	dx,cx	;Trying more than 128 sectors?
	jb	abs_26_exit	;Yes.  Return the fake error.

	MOV	AL,ASCIIZ
	SUB	AL,41H
	MOV	DX,ABS_25_SEC_LO
	TEST	MIRROR_SWITCH,ABS_25_HARD_WAY
	JZ	ABS_26_WRITE
	MOV	CX,-1
	MOV	DS,CS_SAVE
	LEA	BX,ABS_25_LIST

ABS_26_WRITE:
	INT	26H
	JC	ABS_26_ERROR
	POPF
	CLC

ABS_26_EXIT:
	POP	DS
	POP	BP
	POP	DI
	POP	SI
	POP	DX
	POP	CX
	POP	BX
	RET

ABS_26_ERROR:
	POPF
	STC
	MOV	FAILURE_TYPE,AH		; M005;
	JMP	ABS_26_EXIT

ABS_26	 ENDP
;
;------------------------------------
CODE	ENDS
	END
