;       Standard includes for POWER.EXE device driver and control program
;
;       Microsoft Confidential
;       Copyright (C) 1991 Microsoft Corporation 
;       All Rights Reserved.

;
;------------------------------------------------------------------------------
;
; M077 : B#2669. Registered POWER's 2f channels in mult.inc
;
;------------------------------------------------------------------------------
;
REQHDR	STRUC			; DOS REQUEST HEADER STRUCTURE
RH_B_LEN	db ?		; request header length
		db ?		; block device only
RH_B_CMD	db ?		; command to execute on this request
RH_W_STATUS	dw ?		; return status from driver
		db 8 dup(?)	; reserved for dos
REQHDR	ENDS

RHINIT	STRUC
		db SIZE REQHDR dup(?)
		db ?		; block device only
RH_D_BREAKPTR	dd ?		; return address to end driver
RH_D_IDLEPTR	dd ?		; return address of idle routine
				; for DOS special definition for DOS 5
		db ?		; block device only
RHINIT	ENDS

RHIO	STRUC
		db SIZE REQHDR dup(?)
		db ?		; block device only
RH_D_XFERPTR	dd ?		; address of read/write block
RH_W_XFERCNT	dw ?		; size of block
		dw ?		; block device only
		dd ?		; block device only
RHIO	ENDS

POW_IOCTL_BLOCK struc           ; structure returned by IOCTL READ call

PIB_ENTRY_OFFSET dw ?            ; callback entry point
PIB_ENTRY_SEG   dw ?
PIB_VERSION     db ?            ; version of POWER.EXE
                db 27 dup (?)   ; reserved

POW_IOCTL_BLOCK ends

; SAVEPWR version nos
POW_MAJ_VERSION	EQU 01H 	; M084
POW_MIN_VERSION	EQU 00H 	; M084


; functions supported by POWER direct callback
POW_GET_CONTROL_INFO    equ     0       ; return pointer to control structure
POW_GET_STATS           equ     1       ; return pointer to idle measurment structure
POW_DO_IDLE             equ     2       ; direct DO_IDLE call

;-----------------------
; Equates for the BIT Values in POWER_STATUS (defined in POWER.ASM)
; Also used by GET/SET POWER STATUS API
;
SW_IDLE_ACTIVE	EQU	1	; BIT 0 ON => SW idle detection enabled
FW_IDLE_ACTIVE	EQU	2	; BIT 1 ON => FW (APM) Pow.Mgmt. enabled.

;-----------------------
; IDLE  strategy flags

KYC_ACTIVE	EQU 01h
DOSYIELD_ACTIVE	EQU 02h
APP_ACTIVE	EQU 04h
DOSIDLE_ACTIVE	EQU 08h

AUTO_ACTIVE	EQU 10h		; Set IFF kyc & DOSYIELD are on
SOUND_ACTIVE	EQU 80h		; DEBUG ONLY

ALG_RESERVED_BITS equ NOT (KYC_ACTIVE+DOSYIELD_ACTIVE+APP_ACTIVE+DOSIDLE_ACTIVE)
;-----------------------

CONTROL_INFO struc
IDLE_FLG	dw ?		; startup AUTO, MEASURE, IDLE
SWITCH_DLY	dw ?		; delay to switch between idle algorithms
THRESHOLD	dw ?		; idle ignore threshold
ADAPT_DLY	dw ?		; idle adaptation delay
MAXERRSAMPLE    dw ?            ; max. allowed errors while adapting
SPEED_DLY	dw ?		; full speed delay
SPDUP_RAMP	dw ?		; adjustable ramp delay used in APP_SPDUP
SPDUP_MAX	dw ?		; maximum delay speedup delay
CV86FLG		dw ?		; bit 0 set if test active
				; bit 1 set if 386 or above
CONTROL_INFO ends

IDLE_INFO struc
CPU_ON_TIME	dd ?		; total time CPU is ON (TIMER TICS)
CPU_IDLE_TIME	dd ?		; total time CPU is IDLE (TIMER TICS)
IDLE_TOT	dd ?		; total count of idles executed
APP_TOT		dd ?		; total count of APPLICATION IDLES
DOS_TOT		dd ?		; total count of DOS IDLES
KEY_TOT		dd ?		; total count of KEY BOARD IDLES
SHELL_TOT	dd ?		; total count of shell idles
IDLE_INFO ends

PERIOD_INFO struc
ACC_COUNTER	dw ?			; M106 
ACC_HI		dw ?			; M106
SAMPLE_COUNT	dw ?			; no of samples counted in the avg
PERIOD	dw ?			; Current period in timer tics
PSPREAD	dw ?			; Spread over the current avg in adapt cycle
BASE	dw ?			; Current base period in timer tics
NOISE   dw ?                    ; noise allowed on the current base
ADAPT	dw ?			; (1) Adapting to increase in period
DELAY	dw ?			; WAIT Count prior to adapting
PCOUNT	dw ?			; Counts times this interrupt has been called
PERIOD_INFO ends

MAXCMOSPOLLCOUNT	equ	364	; M092 approx. once in 20 secs 

; M092  BEGIN
BIOSDATASEG	equ	40h
ROLLOVERFLG	equ	70h
BIOSTICKCOUNT	equ	6ch
; M092 END

; M001 APM SUPPORT 
; Comment the next line out, if you don't want APM related code to be
; present in POWER.
INCL_APM	equ	1
;
; additional equates used by APM support code

APM_STATS_STRUC	struc
	RESUME_COUNT	dw	?
APM_STATS_STRUC	ends

APM_STATS_STRUC_SIZE	equ	size APM_STATS_STRUC

.errnz (APM_STATS_STRUC_SIZE - 2)
; If this line generates error message look at power.asm I2f processing code
; for get_STATS API. This is currently the size the APM_RESUME_COUNT

I1A_GET_DATE	equ	0400h		; get date int 1a function code
ROLLOVER_COUNT	equ	70h		; offset to the byte counter of 
					; elapsed days  in bios data area
					; (i.e. 40:70h)

; **************************** ERROR CODES ***********************

ERROR_PM_ALREADY_CONNECTED	equ	02h		; same as APM
ERROR_PM_NOT_CONNECTED		equ	03h		; same as APM
ERROR_PM_FUNCTION_NOT_SUPPORTED	equ	70h
ERROR_PM_BUFFER_TOO_SMALL	equ	71h
ERROR_PM_INVALID_PARAMETER	equ	87h		; same as system EXTEND

; ***************************** Int 2f subfunctions supported by POWER *****
;
I2F_PW_INSTALL_CHK	equ	0
I2F_PW_GET_SET_PWSTATE	equ	1
I2F_PW_SELECT_ALG	equ	2
I2F_PW_GETSET_SAVINGS	equ	3
I2F_PW_TUNE_PARM	equ	80h
I2F_PW_GET_STATS	equ	81h
I2F_PW_APM_POLLCOUNT	equ	82h
I2F_PW_GETSET_BASELINE	equ	8fh
;
; ***************************** parameters/args for the above int 2f functions
;
; args for PW_GET_STATS call (BX reg)
PW_GET_IDLE_STATS	equ	0
PW_GET_APM_STATS	equ	1
;******************************* the broadcast int. used by POWER ***********
;
I2F_APM_BROADCAST	equ	((MultPWR_BRDCST * 256)+0bh)		; M077
;
;*******************************  misc. equates *****************************
;
PWSTATE_RESERVED_BITS	equ	11111100b	; only the least 2 bits are used

IDLE_SUPPORT_BYTE	EQU	01010101b	; JAH Idles support byte for PSP
CUR_PSP_OFFSET		EQU	330h

STACK_WORDS		EQU	128		; JAH2 Size of int 21 stack in words
IDLE_SUPP_FLG		EQU	00000001b	; JAH2 Int 2f idle support flag
EXT_PSP_SIG		EQU	2fh		; JAH2 Flags signature in PSP
;
WIN286STDMODE		equ	1	; NSM2 Bit 0 of dx in Win386 startup
					; int 2f broadcast call
;
MINI16COUNT		equ	25	; no of i16s happened in 1 tick
;
; savings mode equates
PW_ADV_MAX	equ	7
PW_ADV_REG	equ	6
PW_ADV_MIN	equ	4

MAXSAVINGSMODE	equ	PW_ADV_MAX
REGSAVINGSMODE	equ	PW_ADV_REG

MAX_SAVINGS_VALUE	equ	8	; only 8 levels of savings allowed

; M106 -  use 16 samples to find an avg while adapting
MAX_SAMPLES_PER_AVG	equ	16
;
;****************************************************************************
