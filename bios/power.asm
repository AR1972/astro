	PAGE    ,132
;**************************************************************
;*  POWER.SYS - MSDOS 5.0 IDLE DETECTION DEVICE DRIVER
;*
;*  Microsoft Confidential
;*  Copyright (C) Microsoft Corporation 1991
;*  All Rights Reserved.
;*
;*  Revision History
;*  90/7/19  [BR]       Completed beta DRIVER V0.30
;*  04/29/91 SR         Modified to make it ROMmable. Separated CODE,
;*                      DATA & INIT segments.
;*  06/03/91 MD         Merged standalone and resident versions,
;*                      support idle checking on INT 2F Idle
;*  07/01/91 JRCB       Changed STANDALONE to POWERALONE to remove confusion
;*                      with ROMDRIVE variable of same name.
;*  08/12/91  NSM       added APM support (M001)
;*
;*  08/21/91  NSM       M002: Removed the comment marks in Int14 hook
;                       (bug #    : printing slowed when POWER is present)
;                       Added a call to CPU_BUSY after resume
;
;   08/29/91  NSM       M003: removed rest of IOCTL support
;               
;   09/05/91  NSM       M004: Bug# 2587 
;                       Moved CheckV86 calls only for DOING HLT and not
;                       for OEM_IDLE or for APM_IDLE (2587)
;                       Also fixed the problem with multiple resumes.
;                       If we get a resume without a initial suspend notifi.
;                       then we ignore this resume command now.
;                       Bug#2574 Count CPU_IDLE time at Int 1c
;
;   09/08/91  DBO       M005: Straighten out far calls in BIOS-resident
;                       driver:
;                        Do_APM_Enable_Disable  -- make near
;                        Check_and_Init_APM     -- list in seg_reinit
;                        time_to_ticks          -- reference via ttticks 
;                                                  (already in seg_reinit)
;                        Do_APM_Connect         -- make near
;
;   09/11/91  NSM/DBO   M074: Replace clock driver functions, so we can manage
;                       manage system date & time updates after resume events.
;
;   09/11/91  SMR       M075: B#2670. Get Idle Algorithm was trashing IDLE_FLG
;
;   09/11/91  SMR       M076: B#2668. Statistics were not being copied if the
;                       user buffer is bigger than the required size.
;
;   09/11/91  SMR       M077: B#2669. Registered POWER's 2f channels in mult.inc
;
;   09/11/91  SMR       M078: Use HLT even if the processor is in V86 mode
;
;   09/11/91  SMR       M079: PWR_API returns 0 in AX instead of no carry flag
;                               in case of no error
;
;   09/11/91  DBO       M080: Power management clock hooks int 6C for
;                       resume events.
;
;   09/12/91  SMR       Call P_UpdFromCMOS directly if power is standalone
;
;   09/12/91  SMR       Load ES:DI before dispatching a CLOCK$ call.
;
;   09/17/91  NSM       M084: Don't zero out AX for version check int 2f call
;
;   09/19/91  SMR       M085: Return break address from clock driver init also
;
;   09/19/91  SMR       M086: Save/Restore SI in get/Set statistics calls
;
;   09/23/91  NSM       M087: Change signature to 504DH
;
;   09/25/91  NSM       M088: (B# 2730) Hang in GET_APM_STAT - missing "push si"
;
;   09/25/91  NSM       M089: I2F service routine changes related to UI change.
;                       (definition of POWER STD needed some extra code)
;
;   09/25/91  NSM       M090: CLOCK driver related changes to take care of
;                       WIN 3 ENH mode eating up I1Cs.  Soln. is to update our
;                       time from CMOS once in 1024 I1c ticks. Do this only 
;                       when win enh mode starts up and discontinue once win
;                       ends. (Bug# 2729)
;
;   09/25/91  NSM       M091: Also update our time from CMOS when POWER STD
;                       is selected on an APM machine. 
;
;   10/25/91  NSM       M093: CX reg is getting trashed while making suspend
;                       standby APM call in Int1C.
;                       While changing APM_MAX_POLLCOUNT, we should update
;                       the current APMPoll Counter also (APM_POLL_COUNT).
;
;   10/29/91  NSM       M094: AX and BL regs were getting trashed in some of
;                       the INT 2f service routines.
;
;   11/05/91  NSM       M096: Eat the rollover flag in Int 1C before command
;                       gets it.
;
;   11/19/91  SMR	M098: Load power into UMB automatically
;
;   11/26/91 SMR	M099: Remember PSP of 2f instead of maintaining a
;			flag in the app's PSP
;
;   11/26/91 SMR	M100: Instance the PSPOf2fApp variable
;
;   11/27/91 SMR	M102: Reverse the order of instancing
;			Do definite idle on INT 2a ah=84
;
;   12/03/91 NSM	M103: Check for POWER OFF before doing IDLEs.
;
;   12/03/91 SMR	M104: Do APM Command line settings only in the
;				second copy of POWER if moved into UMB
;
;   12/04/91 SMR	M105: Fake out arena header & sub-arena header
;				so that MEM.EXE will see POWER properly.
;
;   12/06/91 NSM	M106: Change the averaging algorithm while adapting
;			 Instead of using a running avg, collect 16 samples
;			 at a time, find avg on them, and use that new avg
; 			 for the next 16 samples  and so on till end of adapt.
;
;   12/06/91 SMR	M107: Issue int 1A to clear BIOS rollover flag
;
;   12/10/91 NSM	M108: Put APM related I2F service code within IFDEF
;			INCL_APM directives
;
;**************************************************************

	TITLE   POWER$ IDLE DETECTION V1.0

	.xlist

	include version.inc     ; M004 set build flags

break   macro                   ; satisfy mult.inc
	endm

	include mult.inc        ; get our int 2f function equates       ; M077
	include	win386.inc	; NSM2
	include	devmark.inc

IFDEF   POWER

IFNDEF  POWERALONE              ; segment declarations for resident version

	include biosseg.inc     ; establish bios segment structure

ELSE                            ; segment declarations for standalonde version

.SEQ

Bios_Code       segment word public 'Bios_Code'
Bios_Code       ends

Bios_Data       segment word public 'Bios_Data'
Bios_Data       ends

SysInitSeg      segment word public 'system_init'
SysInitSeg      ends

Trans_Code      segment word public 'CODE'
		extrn	ParseCmdLine:far
Trans_Code      ends

Trans_Data      segment word public 'DATA'
		extrn	BadDOSMsg:byte
		extrn	PW_Low:byte, PW_Mode:byte, PW_Savings_Value:word ; M104
IFDEF	DEBUG
		extrn	PW_Debug:byte					 ; M104
ENDIF

Trans_Data      ends

ENDIF


	include msequ.inc
	include devsym.inc
	include bpb.inc
	include ioctl.inc
	include power.inc

IFDEF	POWERALONE			; M098
	include	umb.inc			; M098
	include	syscall.inc		; M098
ENDIF ; POWERALONE			; M098


IFDEF INCL_APM
	include apmequ.inc              ; M001
ENDIF

break   macro
	endm

	include error.inc
	.list

	include msgroup.inc     ; define Bios_Data segment

IFDEF   POWERALONE                      ; standalone device driver version
Bios_Res        dw      Bios_Code       ; Our code segment address
ELSE                                    ; resident BIOS version
	extrn   Bios_Res:word           ; Code segment address supplied externally
	extrn   ttticks:dword           ; far ptr to time_to_ticks routine
	extrn   P_UpdFromCMOS_Ptr:dword ; ptr to CMOS clock read        ; M081
IFDEF   INCL_APM
	extrn   Check_and_Init_APM_Ptr:dword    ; ptr to APM init routine
ENDIF   ;INCL_APM
ENDIF   ;NOT POWERALONE
;
; PUBLIC declaration for DATA
;
public  RHPTR, KYC, I28, CONTROL, INFO, MSW
public  IDLTIC, TOTTIC, SPDUP, SPDUP_DLY, SPDUP_CNT    ; M004
public  I2F_VEC, I28_TMR0, I28_VEC, KYC_TMR0, IN_KYC 
public  I16_VEC, SWITCH_CNT, I9_COUNT, I9_VEC, I10_VEC, I13_VEC
public  I14_VEC, I17_VEC, I21_VEC,I25_VEC,I26_VEC,I2A_VEC, I08_VEC

; PUBLIC declarations for APM related Data
;
IFDEF INCL_APM                                  ; M001
public  fAPM_PRESENT,APM_FLAGS,APM_VER,APM_POLL_COUNT,fAPM_CONNECT
public  APM_MAX_POLLCOUNT,fAPM_STATE,APM_RESUME_COUNT
ENDIF

RHPTR   dd 0

; DEBUG Macro
; Prints the argument (dispchar) on screen
; 
dbg_printchar   macro dispchar
IFDEF   DEBUG
	push    ax
	mov     ah,0eh
	mov     al,dispchar
	int     10h
	pop     ax      
ENDIF
endm

POWER_DATA label byte
;
; Period Info structure for Keyboard (KYC) and Int 28 (I28)
; This holds the values for BASE,NOISE, AVG, SPREAD etc.
; (look at power.inc for PERIOD_INFO structure
;
; BUGBUG - Nagara  - PERIOD_INFO structure has changed and the following
; lines do not reflect that. These lines should have been:
; 	KYC	PERIOD_INFO	<0,0,0,0,0,20,20,0,0,0>
;	I28 PERIOD_INFO <0,0,0,0,0,100,20,0,0,0>
; Luckily the current def., though wrong, will not cause any problems

KYC     PERIOD_INFO     < 0, 0,20, 20, 0, 0, 0>   ; default values for base and
I28     PERIOD_INFO     < 0, 0,100, 20, 0, 0, 0>  ; noise are (random)conservative

; BUGBUG END

; CONTROL holds constant/tunable parameters for (mainly) int 16/int 28 idle
; detection strategies.
;
CONTROL CONTROL_INFO    < 00011111b, 8*2, 31*1024, 4,  40, 0,  1,   8,    0>  
                                                                ; M078
;                       idle_flg, sw_dly,thres,ad_dl,mx,sp_dl,ramp,sp_mx,386flg
;
; INFO holds the idle detection statistics such as no of i16 idles detected etc.
INFO    IDLE_INFO       < 0, 0, 0, 0, 0, 0, 0>

ErrSampleCount  dw      0	; no of errors encountered so far in adapt cycle

MSW		dw	0	; store machine status word
				; used by ChkV86 procedure
				; bit 0 = 1 indicates protected mode

IDLTIC		dw	0	; M004 used as a flag that says CPU was idle 
				; in the last (I08) tick period

TOTTIC		dw	0	; running total of halt time	; counted in Int8
				; in 1/1.19 MHZ increments 65536=55ms

;
; ACTIVITY MONITOR DATA
;
SPDUP		dw	0	; Activity indicator; 1=> active =>avoid halting
SPDUP_DLY	dw	0	; MAX DELAY set by activity monitor
SPDUP_CNT	dw	0	; incremented via INT 08

;
; M100 - begin
;

; Data to track Int2F aware apps.
; Instance data structure for instancing PSPOf2fApp
;
Pwr_InstData	label	byte
 Pwr_SIVersion	dw	3	; ignored 
 Pwr_SINextDev	dd	0	; ptr to next Instance packet
 Pwr_SIVirtDev	dd	0	; ignored
 Pwr_SIRefData	dd	0	; ignored
 Pwr_SIInstData	dw	offset Bios_Data:Pwr_SITable, Bios_Data
				; pointer to instance table

Pwr_SITable	label	byte
Inst_PSPOf2fApp	dw	offset Bios_Data:PSPOf2fApp, Bios_Data
		dw	2	; 2 bytes
Inst_End	dd	0

;
; M100 - end
;

PSPOf2fApp	dw	0	; PSP of the current 2f-1680 App	; M099
PSPsegment	dw	(?)	; JAH3 - Dos DATA seg  used to	
				; access Current PSP

; Data to store prev. vectors of interrupts we hook
; At startup these hold our ISRs for corresponding ints, and exchanged with
; the previous ISR addresses at Device init time
;
I2F_VEC dw I2F_IDLE, 0          ; holds next pointer in chain

; BUGBUG - Nagara - I2F_TMR0 is not used at all and can be deleted
I2F_TMR0        dw   0          ;+0, holds temporary PC timer 0
				;    values used to compute PERIOD
		dw   2          ;+2, Counts number of times PC
				;    timer has Overflowed
;BUGBUG END

I2A_VEC dw I2A_IDLE, 0          ; holds next pointer in chain

; I28_TMR0 is used to store TMR0 readings whenever Int 28 occurs.
I28_TMR0        dw 0         ;+0, holds temporary PC timer 0
				;    values used to compute PERIOD
		dw 2            ;+2, Counts number of times PC
				;    timer has Overflowed

I28_VEC dw I28_IDLE, 0

KYC_TMR0        dw 0            ;+0, holds temporary PC timer 0
				;    values used to compute PERIOD	for Int 16s
		dw 2            ;+2, Counts number of times PC
				;    timer has Overflowed


IN_KYC  dw 0                    ; flags re-entry of key board check

I16_VEC dw I16_IDLE, 0          ; stores next key routine in chain

; SWITCH_CNT keeps track of when to switch idle detect method (whether int16
; or int 28)
SWITCH_CNT      dw 0                    
					

I08_VEC dw Pwr_i08_isr,0	; our Int 8 ISR ; NSM5

I9_COUNT        dw 2            ; for every key press more
				; a minimum of 2 INT 9's occur

; Application activity monitor interrupts

I9_VEC          dw I9_APP,0

I10_VEC dw I10_APP,0

I13_VEC dw I13_APP,0

I14_VEC dw I14_APP,0

I17_VEC dw I17_APP,0

I21_VEC dw I21_APP,0

I25_VEC dw I25_APP,0

I26_VEC dw I26_APP,0

; Our hook for Int 6c- update system date and time
I6C_VEC dw I6C_RESUME, 0        ;M080

;------------------------------------------------------------------------
; APM SPECIFIC DATA
;
IFDEF   INCL_APM                ; M001

fAPM_PRESENT    db      0	; set to 1 if APM present
fAPM_CONNECT    db      0 	; set to 1 if we are connected
APM_MAX_POLLCOUNT       dw      50 ; interval bet PM event polls (in tmr tics)
APM_FLAGS       dw      0	; flags returned by APM detect call
APM_VER         dw      0	; APM BIOS ver returned by APM detect call
APM_POLL_COUNT  dw      50	; interval bet PM event polls (in tmr tics)
				; counter for APM polling ; counted in int 8 time

fAPM_STATE      db      0       ; 0 if APM disabled and 1 if APM enabled

APM_RESUME_COUNT        dw      0   ; counter for no of resumes from last
				; APM enable

ENDIF                   ;INCL_APM
;-------------------------------------------------------------------
; DATA for CLOCK$ driver

	PUBLIC  CMOSUpdFlg      ; M090
	PUBLIC  CMOSPollCount   ; M090

CMOSUpdFlg      db      0       ; M090 set to 1 if we need to update from CMOS
				; set when we see a rollover in BDA (40:70h) or periodically
				; in POWER STD mode in an APM machine
CMOSPollCount   dw      MAXCMOSPOLLCOUNT        ; M090
				; counted at int 8 time and used in POWER STD mode to update
				; system date and time periodically
CMOSFlg         db      0       ; M091 bit 0 set to 1 in POWER STD mode on APM 
				; machines

;-------------------------------------------------------------------
; BASE LINE calculation data for Int 16 idle detection

BaseLineRef   dw      0		; counters for counting no of i16s we can get
				; through in about 4 secs  (done at each
				; exec)
BaseLineOvf	dw	0	; also used to store previous calculated 
				;  BaseLineRef

; SMR7	- Baseline Max calculation
		public	BL_Samples
BL_NUMSAMPLES	equ	9		; no of samples collected at EXEC time
BL_Samples	dw	BL_NUMSAMPLES dup ('SM') ; data to hold i16 samples
BL_EndSamples	label	byte
; SMR7 END


;-------------------------------------------------------------------
; CLOCK$ driver DATA

IFDEF   POWERALONE              ; needed only for standalone version
	public  daycnt		; no of days from 1-1-80 (ABS)
daycnt  dw      0
	public  daycnt2
daycnt2 dw      0		; used as a temp while calculating daycnt
	public  base_century
base_century    db      19
	public  base_year
base_year       db      80
	public  month_tab
month_tab       db      31,28,31,30,31,30,31,31,30,31,30,31

	public  bin_date_time
bin_date_time:
	db      0               ; year in century (0...99) or minutes (0-59)
	db      0               ; century (19 or 20) or hours (0-23)
	db      0               ; day in month (1...31)
	db      0               ; month in year (1...12) or seconds (0-59)

	public  month_table

month_table:			; no of days in year upto this month
	dw      0               ; january
	dw      31              ; february
	dw      59              ; march
	dw      90              ; april
	dw      120             ; may
	dw      151             ; june
	dw      181             ; july
	dw      212             ; august
	dw      243             ; september
	dw      273             ; october
	dw      304             ; november
	dw      334             ; december

ELSE                            ; for resident version
	extrn   bin_date_time:byte
	extrn   month_table:word
	extrn   daycnt:word
	extrn   daycnt2:word

ENDIF                   ; POWERALONE

;-------------------------------------------------------------------
; Used by Int 21 application activity monitor to filter those sysem calls
; that should not inhibit idles and those calls that have to be special cased
;
; 1 byte for each int 21 api from functions 0 thro 5fh
; an entry of 0ffh means ignore this api for false idle monitoring
; 0 -> this api means the system is busy (NOT idle) 
; any other number -> special cases
i21_table	label	near
	db	0,   0,   0,   0,   0,   0,   5,   0	; fns 0-7
	db	0,   0,   0,0ffh,   0,   0,   0,   0	; fns 8-0f
	db	0,   0,   0,   0,   0,   0,   0,   0	; fns 10-17
	db	0,   0,   0,   0,   0,   0,   0,   0	; fns 18-1f
	db	0,   0,   0,   0,   0,0ffh,   0,   0	; fns 20-27
	db	0,   0,0ffh,   0,0ffh,   0,   0,   0	; fns 28-2f
	db	0,   0,   0,   0,   0,0ffh,   0,   0	; fns 30-37
	db	0,   0,   0,   0,   0,   0,   0,   0	; fns 38-3f
	db	0,   0,   0,   0,043h,   0,   0,   0	; fns 40-47
	db	0,   0,   0,04ah,   0,   0,   0,   0	; fns 48-4f
	db   0ffh,0ffh,   0,   0,   0,   0,   0,   0	; fns 50-57
	db	0,   0,   0,   0,   0,   0,   0,05eh	; fns 58-5f
MAX_I21_ENTRY	equ 5fh
;
;-------------------------------------------------------------------
;
; MISCELLANEOUS FLAGS and TABLES
;

POWER_STATUS    db      1       ; default ;bit 0 -  S/W pw.mgmt always enabled
				; bit 1 - Take control of APM/FIRMWARE  mgmt 
				; (or get connected to APM)
				; M001
				; M089 If bit1 is set, We are connected to APM
				; but not neccessarily enabled APM
				; (look at fAPM_State)
;NSM1 BEGIN
; NSM9 BEGIN	- 8 sets of savings modes with 6 being the default
Savings_Mode	dw	6	; 3 = MIN savings
				; 6 = Reg savings (default)
				; 7 = Max savings

; Savings_Table used for NOISE/PSPREAD ratio calculation for the 8 levels of
; savings mode
;
Savings_Table	label	byte
;	bit	7 6 5 4 3   2    1    0
;			       12.5  25  50%  for NOISE/SPREAD RATIO
	db	4		; 12.5 %  saving  mode 1
	db	2		; 25 %		- mode 2
	db	6		; 37.5 % 	- mode 3
	db	1		; 50% 		- mode 4
	db	5		; 62.5% 	- mode 5 - MIN
	db	3		; 75%		- mode 6 - REG
	db	7		; 87.5% 	- mode 7 - MAX
	db	0		; 100% 		- mode 8  - special case
;
; Table used to calculate BaseLineRef for the current level of savings mode
; from the BaseLineMax value calculated at EXEC time.
;
BaseLine_Table	label	byte
; 	Bit	7 6   5   4       3     2    1   0	
;		    .03  .063    .125  .25  .5  1
; for levels upto 6, the calculated value from this table is substracted from
; BaseLineMax, whereas from level 6, it is added
;
	db	0	; dummy ; to make this 1-based 
	db	1ch	; for mode 1 - 45% of baseline freq
	db	2	; mode 2 - 40% of baseline freq ( or 1/2 of current max)
	db	12h	; mode 3 - approx 35% of baseline
	db	1ah	; mode 4 - approx 30% of baseline    - MIN 
	db	16h	; mode 5 - approx of 25% of baseline 
	db	0	; mode 6 - 20% of baseline	     - REG (+from here)
	db	2	; mode 7 - 12% of baseline	     - MAX
	db	1	; mode 8 - 10% of baseline	; special case
;	
; NSM9 END
;NSM1 END
	
NoBaseLineCalc	db	0	; = 1-> no baseline calc at EXEC time ; NSM2
				; set by WIN enh.mode startup call
				; BaseLineMax is not calculated if this is set

;-------------------------------------------------------------------
; Ptrs to previous ISRs for each of the interrupts we hook.
; these have to be in Data seg for ROMDOS. 
;
Pwr_i2f_next    dw      Pwr_i2f_lab
		dw      Bios_Data
Pwr_i2f_lab:
	pop     ds
	jmp     dword ptr cs:I2f_Vec

;
; M102 - begin
;
Pwr_call_i2f    dw      Pwr_calli2f_lab
		dw      Bios_Data

Pwr_calli2f_ret dw      Calli2fret
		dw      0

Pwr_calli2f_lab:
	pop     ds

	pushf
	call    dword ptr I2f_Vec

	push    ds
	jmp     dword ptr Pwr_calli2f_ret
;
; M102 - end
;
Pwr_i2a_next    dw      Pwr_i2a_lab
		dw      Bios_Data

Pwr_i2a_lab:
	pop     ds
	jmp     dword ptr cs:I2a_Vec

IFDEF  POWERALONE
Pwr_i28_next    dw      Pwr_i28_lab
		dw      Bios_Data
Pwr_i28_lab:
	pop     ds
	jmp     dword ptr cs:i28_Vec
ENDIF

Pwr_i16_next    dw      Pwr_i16_lab
		dw      Bios_Data
Pwr_i16_lab:
	pop     ds
	jmp     dword ptr cs:I16_Vec

Pwr_call_i16    dw      Pwr_calli16_lab
		dw      Bios_Data

Pwr_calli16_ret dw      Calli16ret
		dw      0

Pwr_calli16_lab:
	pop     ds

	pushf
	call    dword ptr I16_Vec

	push    ds
	jmp     dword ptr Pwr_calli16_ret

kb_call_i16	dw	kb_calli16_lab
		dw	Bios_Data
kb_calli16_ret	dw	kbChkRet
		dw	0

kb_calli16_lab:
	pushf
	call	dword ptr I16_Vec
	jmp	dword ptr kb_calli16_ret

Pwr_i9_next     dw      Pwr_i9_lab
		dw      Bios_Data
Pwr_i9_lab:
	pop     ds
	jmp     dword ptr cs:I9_Vec

Pwr_i10_next    dw      Pwr_i10_lab
		dw      Bios_Data
Pwr_i10_lab:
	pop     ds
	jmp     dword ptr cs:I10_Vec

Pwr_i13_next    dw      Pwr_i13_lab
		dw      Bios_Data
Pwr_i13_lab:
	pop     ds
	jmp     dword ptr cs:I13_Vec

Pwr_i14_next    dw      Pwr_i14_lab
		dw      Bios_Data
Pwr_i14_lab:
	pop     ds
	jmp     dword ptr cs:I14_Vec

Pwr_i17_next    dw      Pwr_i17_lab
		dw      Bios_Data
Pwr_i17_lab:
	pop     ds
	jmp     dword ptr cs:I17_Vec

Pwr_i21_next    dw      Pwr_i21_lab
		dw      Bios_Data
Pwr_i21_lab:
	pop     ds
	jmp     dword ptr cs:I21_Vec

Pwr_i25_next    dw      Pwr_i25_lab
		dw      Bios_Data
Pwr_i25_lab:
	pop     ds
	jmp     dword ptr cs:I25_Vec

Pwr_i26_next    dw      Pwr_i26_lab
		dw      Bios_Data
Pwr_i26_lab:
	pop     ds
	jmp     dword ptr cs:I26_Vec

Pwr_i6c_next    dw      Pwr_i6c_lab     ;M080
		dw      Bios_Data       ;M080
Pwr_i6c_lab:                            ;M080
	pop     ds                      ;M080
	jmp     dword ptr cs:I6c_Vec    ;M080

; NSM5 BEGIN - i08 hook
; This is the main interrupt used by POWER to do the following:
;	used for time outs on idle interrupts
;	used for counting idle statistics
;	used for polling for / Broadcasting PM event
;	used for periodically update date&time in POWER STD mode
;	used for switching between I16 and I28
;
IN_I08	db	0			; flag  to avoid  I08 reentrancy
;
I08_IdleControl_ptr	label	word
	dw	I08_IdleControl
	dw	0			; needs to be filled in

Pwr_i08_isr	proc	far

	pushf	
	call	dword ptr cs:I08_Vec
	call	dword ptr cs:I08_IdleControl_ptr
	iret

Pwr_i08_isr	endp
;
; NSM5 END

; JAH2 ====================================================================
; JAH2	- Need to keep our own stack when calculating baseline on int 21 exec
;	and while polling APM for PM event and broadcasting it.

Old_SS		dw	0		; JAH2
Old_SP		dw	0		; JAH2

StackInUse	db	0		; JAH2

I21_Stack	dw	STACK_WORDS dup ('TS') ; JAH2
	EVEN				; JAH2
StackTop	LABEL	WORD		; JAH2

; JAH2	- End
; JAH2 ====================================================================
; NSM4 ===================================================================
; NSM4 address of INDOS flag
INDOS_Offset	dw	0		; INDOS flag set while we use 
INDOS_Seg	dw	0		; different stack  to avoid task 
					; swapping
; NSM4 END ===============================================================

End_Data        label   near

	tocode                  ;Bios_Code segment starts

IFDEF   POWERALONE              ; device header needed here only for
				; standalone version - in MSBIO1 for resident
	assume  ds:nothing

	org 0
DEV_HDR dd CDEV_HDR             ; address of next driver in chain
	dw 8000H                ; character device      ; M003
	dw JDEV_STRATEGY	; DOS entry points to driver
	dw JDEV_INTERRUPT
	db 'POWER$  '           ; driver name

CDEV_HDR        dd      -1      ; address of next dev hdr in chain
	dw      8008h           ; CLOCK device (& character)
	dw      JDEV_STRATEGY
	dw      JCDEV_INTERRUPT
	db      'CLOCK$  '      ; driver name

;
; M098 - begin
;
StratPtr	dw	offset DEV_STRATEGY
		dw	BIOS_CODE
PwrIntPtr	dw	offset DEV_INTERRUPT
		dw	BIOS_CODE
ClkIntPtr	dw	offset CDEV_INTERRUPT
		dw	BIOS_CODE

JDEV_STRATEGY:
		jmp	dword ptr StratPtr
JDEV_INTERRUPT:
		jmp	dword ptr PwrIntPtr
JCDEV_INTERRUPT:
		jmp	dword ptr ClkIntPtr

ENDOFSTUB	label	byte

BreakAddr	dw	offset bios_data:end_data
		dw	bios_data

		public	SecondCopy
SecondCopy	db	0		; 1 => second copy of POWER just loaded
				; in UMB, and so no need to display messages
;
; M098 - end
;
;*************** DOS DEVICE REQUEST DEFINITIONS **************

; void DEV_STRATEGY( ES:BX.DOS_REQUEST_HEADER)
; purpose:   Store DOS REQUEST HEADER (ES:BX) for
;            DEV_INTERRUPT routine

DEV_STRATEGY proc far
	push    ds
	mov     ds,cs:Bios_Data_Word
	assume  ds:Bios_Data
	mov     word ptr [RHPTR]+0,bx
	mov     word ptr [RHPTR]+2,es
	pop     ds
	assume  ds:nothing
	ret
DEV_STRATEGY endp

; void DEV_INTERRUPT( [REQHDR].DOS_REQUEST_HEADER)
; purpose:   Executes DOS request; only one fuction is  ; M003
;               supportted by this driver: INIT         ; M003
; for POWER$ driver

DEV_INTERRUPT proc far

	push    ax
	push    dx
	push    bx
	push	cx
	push    es
	push    ds

	mov     ds,cs:Bios_Data_Word
	assume  ds:Bios_Data
	les     bx,[RHPTR]
	cmp     es:[bx].RH_B_CMD,DEVINIT        ; DEVICE INIT?
	jne     dstxxx

;
; M00x
;
	call	far ptr Banner			; Put up our signon Banner
	call	far ptr ChkDOSVer_DispErr	; Check for DOS version

	
	jae	di_continue			; run on DOS 5.0 and above


	mov	word ptr es:[bx].RH_D_BREAKPTR+0, 0
	mov	word ptr es:[bx].RH_D_BREAKPTR+2, cs
	mov	BreakAddr+0, 0
	mov	BreakAddr+2, cs
di_continue:
	call	far ptr DoCmdLine	; process CONFIG switches
	jnz	StayHere		; /low not specified ?

;
; M098 - begin
;
	call	far ptr UMBInit		; try loading ourselves in UMB
	jc	StayHere		; no UMB space ?
	mov	StratPtr+2, ax
	mov	PwrIntPtr+2, ax
	mov	ClkIntPtr+2, ax

	mov	word ptr es:[bx].RH_D_BREAKPTR+0, offset ENDOFSTUB
	mov	word ptr es:[bx].RH_D_BREAKPTR+2, cs

	mov	ax, 0100h			; DONE!
	jmp	short dstex0
StayHere:
; cannot load in UMB or /LOW specified; init our driver and ret
; M098 - end
;
	call    far ptr Power_Init	
	jmp     short dstex0

dstxxx: mov     ax,STERR+03H            ; RETURN DEVICE ERROR,
					; ILLEGAL COMMAND
dstex0: les     bx,[RHPTR]
	mov     es:[bx].RH_W_STATUS,ax
	pop     ds
	assume  ds:nothing
	pop     es
	pop	cx
	pop     bx
	pop     dx
	pop     ax
	ret
DEV_INTERRUPT endp

Bios_Data_Word  dw      Bios_Data               ; Our data segment

;*************** CLOCK$ DEVICE DRIVER STRAT/INT **************
	extrn   tim_read:near
	extrn   tim_writ:near

	extrn   P_UpdFromCMOS:far               ; M081

	public  tim_table

; dispatch table for CLOCK$ driver
;
tim_table       label   byte
	db      (((offset tim_table_end) - (offset tim_table) -1)/2)
	dw      Normal_exit     ; 00    init
	dw      Normal_exit     ; 01
	dw      Normal_exit     ; 02
	dw      Cldev_cmderr    ; 03
	dw      tim_read        ; 04
	dw      Cldev_BusyErr   ; 05
	dw      Normal_exit     ; 06
	dw      Normal_exit     ; 07
	dw      tim_writ        ; 08
	dw      tim_writ        ; 09
tim_table_end:


CDEV_INTERRUPT  proc    far
	assume  cs:Bios_Code,ds:nothing
	push    si
	push    ax
	push    bx
	push    cx
	push    dx
	push    di
	push    ds
	push    es

	lea     si,tim_table

	mov     ds,cs:Bios_Data_Word
	assume  ds:Bios_Data
	
	les     bx,[RHPTR]
	mov     al,es:[bx].RH_B_CMD             ; al = cmd
	cmp     al,cs:[si]
	jae     Cldev_cmd_error
	or      al,al                   ; is it init call
	jz      Cldev_Init

	cbw                             ; note that al <= 15 means ok
	shl     ax,1

	add     si,ax

	les     di, es:[bx].trans       ; M082
	cld                             ; ***** always clear direction
	call    cs:word ptr [si+1]      ;go do command
	assume  ds:nothing
	jc      Cldev_set_stat          ; if function returned status, don't
Cldev_Success:
	mov     ax,100h                 ;  load with normal completion

Cldev_set_stat:
	mov     ds,Bios_Data_Word       ; cas///// note: shouldn't be needed!
	assume  ds:Bios_Data
	les     bx,[RHPTR]
	mov     word ptr es:[bx].status,ax ;mark operation complete

	pop     es
	pop     ds
	pop     di
	pop     dx
	pop     cx
	pop     bx
	pop     ax
	pop     si
	ret

Cldev_cmd_error:
	mov     ax,STERR+03H            ; RETURN DEVICE ERROR,
					; ILLEGAL COMMAND
	jmp     short Cldev_set_stat

Cldev_Init:

IFDEF	POWERALONE
	call	far ptr ChkDOSVer
	jb	Cldev_skipinit		; run on DOS 5.0 and above
ENDIF

	call    far ptr Clock_Init

IFDEF   POWERALONE                      ; M085
	; Return DEVICE DRIVER end address
Cldev_skipinit:
	les     bx,[RHPTR]
	mov     word ptr es:[bx].RH_D_BREAKPTR+0,offset Bios_Data:End_Data
	mov     word ptr es:[bx].RH_D_BREAKPTR+2,ds
;
; M098 - begin
;
	push	ds
	mov	ax, bios_code
	mov	ds, ax
	assume	ds:bios_code
	mov	ax, BreakAddr
	mov     word ptr es:[bx].RH_D_BREAKPTR+0,ax
	mov	ax, BreakAddr+2
	mov     word ptr es:[bx].RH_D_BREAKPTR+2,ax
	pop	ds
	assume	ds:bios_data
;
; M098 - end
;
ENDIF                                   ; M085
	jmp     short Cldev_Success

CDEV_INTERRUPT  endp

Cldev_cmderr    proc    near
	mov     al,3            ; unknown command error
	mov     ah,81h          ; error return
Cldev_Ret:
	stc                     ; indicate abnormal end
	ret

Cldev_BusyErr   label   near
	mov     ah,3            ; indicate busy status
	jmp     short Cldev_Ret

Cldev_cmderr    endp

ELSE                                            ; resident driver version

	extrn   Bios_Data_Word:word             ; supplied externally

	public  power_table
power_table:
	db      (((offset power_table_end) - (offset power_table) - 1) / 2)
	dw      Normal_exit
	dw      Normal_exit
	dw      Normal_exit
; M003 : BEGIN ; remove ioctl support
;       dw      Power_ioctl_read
	dw      Normal_exit
; M003 : END

power_table_end:


ENDIF                                           ; resident driver version

Normal_exit      proc    near

	clc
	ret

Normal_exit      endp

;******************** INT 2F ISR routine for POWER services *************
;  Entry: AL = function code
;       0       Install check
;       
;               EXIT:
;               BX = 504dH (PM) signature if installed  /* M087 */
;
;       1       GET/(Enable/Disable) Power management
;               BH = 0  -> get POWER enabled status
;                  = 1 -> ENABLE DISABLE POWER
;                  BL:bit 0 =T enable Idle detection
;                           =F disable Idle detection
;                  BL:bit 1 =T enable FW/APM 
;                           =F disable FW/APM
;               EXIT:
;                  BH = Prev. Power status (bit 0 & 1 as defined above)
;                  BL = Current power status
;                  CY set if function failed
;
;       2       Get/Set Idle Detection Alg
;               BH = 0 -> Get Current idle detection alg.       
;                  = 1 -> set Current alg.
;                    BL:bit 0 -> T - KeyBoard idle (Int 16 )
;                       bit 1 -> T - Dos Yield (int 28)
;                       bit 2 -> T - App Idle (int 2f 1680)
;                       bit 3 -> T - DOS idle  (int 2a)
;			bit 15 -> T - (DEBUG ONLY) toggle SOUND
;                    BL = 0 -> disable all strategies
;		     BL = 0ffh -> enable all strategies
;               EXIT:
;                       BL = current alg if Get_ALG
;
;	03	Get/Set POWER savings level
;		BL = 0 -> get power savings level
;		    1-8 -> Set power savings level
;		EXIT:
;			BL = current savings level
;
;       80      Get/Set Idle Detection Tuning parameters
;               BX = 0 -> get 
;               else Set
;               CX = size of buffer 
;               DS:SI -> ptr to buffer
;       81      Get Statistics
;               CX = size of buffer
;               DS:SI -> ptr to buffer
;
;       82      Get/Set APM polling count
;               BX = 0 -> get APM polling count
;               else new polling count to be set
;               EXIT:
;                       BL = new polling count
;
;	8F	Get/Set current BaseLineRef
;		ENTRY:
;			BX = 0 -> get BaseLineRef
;			    else Set BaseLineRef
;		EXIT:
;			BX = current BaseLineRef
;			CX = Previous calculated BaseLineRef
;
; M001: Created - NSM
;
Pwr_Services    proc    near

; use a jump table to switch to appropriate services depending on AL value
	push    ds
	push	di
	lea	di,PS_JmpTable
PS_ChkNxtCmd:
	cmp	byte ptr cs:[di],0ffh	; end of table ?
	je	PS_Inv_fn
	cmp	al,cs:[di]
	je	PS_CmdFound
	inc	di
	inc	di
	inc	di
	jmp	PS_ChkNxtCmd
PS_CmdFound:
	inc	di
	jmp	word ptr cs:[di]
	
PS_JmpTable	label	near
; Service jmp table 
; Format:	db	Function no
;		dw	offset of service entry point for this function
;	a value of 0ffh for function no denotes end of jump table
;
	db	0
	dw	PS_Install_Chk
	db	1
	dw	PS_GetSet_PWState
	db	2
	dw	PS_Select_Alg
	db	3
	dw	PS_GetSet_Savings
	db	80h
	dw	PS_Parm_Tune
	db	81h
	dw	PS_Get_Stats
IFDEF	INCL_APM			; M108 APM specific service
	db	82h
	dw	PS_APM_PollCount
ENDIF					; M108 END
	db	8fh
	dw	PS_GetSet_BaseLine
	db	0ffh			; end of table
;
;
PS_Inv_fn:
	mov     ax,ERROR_PM_FUNCTION_NOT_SUPPORTED
PS_FRet:
	stc
	jmp     PS_Ret


;
; FUNCTION 0 - POWER install check call
;
PS_Install_Chk:
	mov     bx,504dH                ; "PM"  ;M087
	mov     ah,POW_MAJ_VERSION      ; M084 major version no
	mov     al,POW_MIN_VERSION      ; M084
	clc                             ; M084
	jmp     PS_Ret                  ; M084

;
; FUNCTION 2 - Get/set Idle detection strategy
;
PS_Select_Alg:
	mov     ds,cs:Bios_Data_Word
	assume  ds:Bios_Data

IFDEF DEBUG
	cmp     bl,80h          ; SOUND toggle
	je      PS_Toggle_Sound
ENDIF
	or      bh,bh           ; get current algorithm ?
	je      PS_Get_Alg
				; no, set alg.
	cmp	bl,0ffh		; all bits set ?
	je	PS_Alg_OK
	test    bl,ALG_RESERVED_BITS	; invalid bits set ?
	jz	PS_Alg_OK
	jmp     PS_Inv_Parm_Err
;
PS_Alg_OK:
	and	bl,NOT ALG_RESERVED_BITS
	xor     bh,bh
	and	[CONTROL].IDLE_FLG,ALG_RESERVED_BITS ; clear current strategey
					; except SOUND bit
	or      [CONTROL].IDLE_FLG,bx
	and	bl,(DOSYIELD_ACTIVE + KYC_ACTIVE)
	and	[CONTROL].IDLE_FLG, NOT AUTO_ACTIVE	; turn off AUTO 
	cmp	bl,DOSYIELD_ACTIVE + KYC_ACTIVE
	jne	PS_Get_Alg		; set AUTO bit if both I16 & I28
	or	[CONTROL].IDLE_FLG,AUTO_ACTIVE	; selected

; M094 - Fall through to return the current algorithm in BL

PS_Get_Alg:
	mov     bx,[CONTROL].IDLE_FLG   
	and     bx,NOT ALG_RESERVED_BITS
	test	[CONTROL].IDLE_FLG,AUTO_ACTIVE	
	jz	PS_SRet			; if AUTO, then say both I16 & I28 are
	or	bx,(KYC_ACTIVE+DOSYIELD_ACTIVE)	; enabled
	jmp     short PS_SRet

IFDEF DEBUG                     ;Toggle sound only for DEBUG VERSION
PS_Toggle_Sound:
	xor     [CONTROL].IDLE_FLG,SOUND_ACTIVE ; toggle the sound flag
	jmp     short PS_SRet
ENDIF

; FUNCTION 82 - get set APM poll count
;
IFDEF	INCL_APM			; M108 APM specific service
PS_APM_PollCount:                               ; 7 change APM poll count
	call    Do_APM_GetSet_PollCount
	jmp     short PS_SRet 

ENDIF					; M108 END
	assume  ds:nothing
;
; FUNCTION 80 - GET/SET tuning parameters (CONTROL)
;
PS_Parm_Tune:                           ; 4 Tune various POWER.SYS parameters
	cmp     cx, size CONTROL_INFO - SWITCH_DLY
	jb      PS_Buff_Ovf_Err
	push    si                      ; M086
	push    di
	push    es
	mov     es,Bios_Data_Word
	assume  es:Bios_Data
	lea     di,[CONTROL].SWITCH_DLY
	assume  es:nothing
	or      bx,bx                   ; get parm or set parm ?
	jnz     PS_DoCopy               ; if set parm 
	mov     bx,-1
PS_Exch_ESDS_SIDI:
	push    es
	push    ds
	pop     es
	pop     ds
	xchg    si,di
	or              bx,bx
	jz      PS_EndCopy
PS_DoCopy:
	rep     movsb                   ; transfer data
	inc     bx
	jz      PS_Exch_ESDS_SIDI
PS_EndCopy:
	pop     es
	pop     di
	pop     si                      ; M086
	jmp     short PS_SRet

; FUNCTION 81 - Get IDLE Statistics
;
PS_Get_Stats:

IFDEF INCL_APM
	cmp     bx,PW_GET_APM_STATS     ;                               ; M076
	ja      PS_Inv_Parm_Err         ; invalid info level ?
	je      PS_Get_APM_Stats        ; APM statistics (resume count ) ?
ELSE
	or      bx,bx                   ; only Idle detection stats will be
	jnz     PS_Inv_Parm_Err         ; returned
ENDIF
	cmp     cx,size IDLE_INFO       ; No, Idle detection statistics
	jb      PS_Buff_Ovf_Err         ; Buffer not sufficient         ; M076
	push    si                      ; M086
	push    di
	lea             di,     INFO
PS_Transf_Stats:
	push    es
	mov     es,Bios_Data_Word
	assume  es:Bios_Data
	assume  es:nothing
	mov     bx,-1                   ; get stats ; there is no set stats
	jmp     short PS_Exch_ESDS_SIDI

IFDEF   INCL_APM

PS_Get_APM_Stats:
	cmp     cx, APM_STATS_STRUC_SIZE
	jb      PS_Buff_Ovf_Err         ; Buffer not sufficient         ; M076
	push    si                      ; M088
	push    di
	lea     di,APM_RESUME_COUNT
	jmp     short PS_Transf_Stats

ENDIF   ; of IFDEF INCL_APM

PS_Inv_Parm_Err:
	mov     ax,ERROR_PM_INVALID_PARAMETER
	stc
	jmp     short PS_Ret

PS_SRet: xor    ax,ax                   ; M084 : Clear CY and  put
					; 0 in AX to mean no error
PS_Ret:
	pop	di
	pop     ds
	ret

; FUNCTION 3 - Get/Set POWER Savings level
;NSM1 BEGIN
PS_GetSet_Savings:
	mov     ds,cs:Bios_Data_Word
	assume  ds:Bios_Data
	test    [POWER_STATUS],SW_IDLE_ACTIVE
	jz	PS_Inv_Parm_Err
	or	bx,bx
	jz	PS_Get_Savings
	cmp	bx,MAX_SAVINGS_VALUE
	ja	PS_Inv_Parm_Err
	mov	[Savings_Mode],bx
	call	SetSavingsValues
PS_Get_Savings:
	mov	bx,[Savings_Mode]
	jmp	short PS_SRet

PS_Buff_Ovf_Err:
	mov     ax,ERROR_PM_BUFFER_TOO_SMALL
	jmp     PS_FRet

; FUNCTION 1 - Get/Set POWER state
;
PS_GetSet_PWState:
	mov     ds,cs:Bios_Data_Word
	assume  ds:Bios_Data
	or      bh,bh           ; get power state ?
	call    GetPowStatus    ; M089 : old state in AL
	mov     bh,al           ; get old status
	mov     ax,0            ; M084 : Assume no error 
	jz      PS_APM_Ret      ; M084 
	test    bl,PWSTATE_RESERVED_BITS
	jnz     PS_Inv_Parm_Err
	push    bx              ; save old status on stack
; set power status
	test    bl,1            ; check for POWER.SYS/EXE enable
	jnz     PS_Enable_Power
; disable POWER.SYS
	test    POWER_STATUS,SW_IDLE_ACTIVE
	jz      PS_Test_APM_Bit         ; already disabled
;
	and     POWER_STATUS,NOT(SW_IDLE_ACTIVE)     ; turn off S/W PM
	jmp     short PS_Test_APM_Bit
PS_Enable_Power:
	test    POWER_STATUS,SW_IDLE_ACTIVE
	jnz     PS_Test_APM_Bit         ; already enabled
	or      POWER_STATUS,SW_IDLE_ACTIVE          ; turn on S/W PM
;
PS_Test_APM_Bit:
;
IFDEF   INCL_APM
	call    Do_APM_Control
ENDIF
;
	pop     bx                      ; get back old status
;
; M084: NSM
; At this point :
; CY set if error 
;       AX = error code
; NC -> AX = 0
;
PS_APM_Ret:
	push    ax                      ; M089  BEGIN
	call    GetPowStatus            ; status in al
	mov     bl,al                   ; return with current status
	pop     ax                      ; M089  END
	jmp     PS_Ret

; FUNCTION 8F - get/set BaseLineRef
PS_GetSet_BaseLine:
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data
	or	bx,bx			; get base line ?
	je	PS_GetBaseLine
	mov	[BaseLineRef],bx
	mov	[KYC].BASE,bx		; also change our current Kbd base
					; for immediate effect
;
PS_GetBaseLine:
	mov	bx,[BaseLineRef]
	mov	cx,[BaseLineOvf]
	jmp	PS_SRet

Pwr_Services    endp

; FAR call for PWR_SERVICES procedure - used for init time settings
;
FPwr_Services	proc	far
	call	Pwr_Services
	ret
FPwr_Services	endp

;****************************** GetPowStatus **********
; Get the current power status
; Entry: none
; Exit:  AL:Bit 0 - 1 if S/W PowMgmt enabled
;           Bit 1 - 1 if H/W PowMgmt enabled
; Regs: AX
; M089: Created
; NOTE: we have no way of knowing whether we are in STD mode or not
; The only way we can find that out is by looking at our flags to see 
; if APM was in enabled state last time when we were connected to it
; and IF we are not connected to APM at present. This is the reason
; for this procedure's existence. 
;******************************************************************
GetPowStatus    proc    near
	pushf
	mov     al,POWER_STATUS
	test    al,FW_IDLE_ACTIVE           ; test for H/W enabled state
	jnz     GPS_Ret			
; H/W not connected state; let us see if it is atleast enabled from what
; we know of before last disconnect.
IFDEF   INCL_APM
	test    fAPM_STATE,1            
	jz      @f
	or      al,2            ; H/W pow.mgmt enabled - POWER STD mode
@f:
ENDIF                   ; IFDEF INCL_APM
GPS_Ret:
	popf
	ret
GetPowStatus    endp

IFDEF   INCL_APM
;****************************** Do_APM_GetSet_PollCount **********
; INT 2F subfunction service procedure 
; To get/set the APM pollcount
; Entry: BX = 0 -> get APM poll count
;               otherwise, New poll count to be set
; Exit:  BX = new APM Poll count
; Regs: BX,DS,Flags
;******************************************************************
Do_APM_GetSet_PollCount Proc    NEAR

	mov     ds,cs:Bios_Data_Word
	assume  ds:Bios_Data
	or      bx,bx
	jz      DAPC_Get_PollCount
	mov     [APM_MAX_POLLCOUNT],bx		; ref.counter
	mov     [APM_POLL_COUNT],bx             ; M093 current counter
	jmp     short DAPC_Ret 
DAPC_Get_PollCount:
	mov     bx,APM_MAX_POLLCOUNT
DAPC_Ret:
	ret

Do_APM_GetSet_PollCount endp

;****************************** Do_APM_Control *********************
; INT 2F subfunction service procedure 
; To (enable-connect to)/(disable-disconnect from) APM
; Entry:  BL = 3 -> connect and enable APM              ; M089
;         BL = 2 -> stay disconnected but enable it if needed
;         BL = 1 or 0 -> disconnect and disable
;
; Exit:  CY if operation failed
;               ax = error code
;        ELSE AX = 0
;
;       (POWER_STATUS flags updated properly
;        fAPM_STATE also updated)
;       APM_RESUME_COUNT initialised to zero if enable successful
;
; Regs: ax,bx,cx,dx
;******************************************************************

Do_APM_Control  proc    near

	cmp     bl,2            ; check for F/w PM bit enable
	jae     DACt_Enable_APM

; M089 BEGIN
; With the new definition of POWER STD, we need to enable APM but not
; connected to it (and sometime disable APM but we would not have been
; connected to it for doing it). Enable/disable can only be done if we
; are connected to APM and so We first connect to APM to enable/disable
; APM and depending on the requested state, we disconnect from APM
;
; Disconnect and disable APM
	and     [CMOSFlg],NOT(1)        ; M091 reset STD mode+APM bit in CMOSFlg
	test    POWER_STATUS,FW_IDLE_ACTIVE     ; are we  connected already ?
	jnz     DACt_Dis_con_able       ; yes, just disable and disconnect
; we are not connected ; so go connect to it first, enable it
; and then disable it!

IFDEF POWERALONE
	call    far ptr Check_and_Init_APM ; M003: always check for APM presence
ELSE
	call    Check_and_Init_APM_Ptr
ENDIF
; M089 END
DACt_Dis_con_able:
	test    fAPM_PRESENT,1          ; M003:no need to do anything if APM not
	jz      DACt_End                ; M003:present
; disable APM
	mov     cx,APM_DISABLE_FUNC
	call    Do_APM_Enable_Disable   ; disable APM ;M005
	jc      DACt_End
	mov     fAPM_STATE,0            ; flag APM disabled state
	call    Do_APM_DisConnect
	and     POWER_STATUS,NOT (FW_IDLE_ACTIVE)            
	jmp     short DACt_End
DACt_Enable_APM:
	push    bx                      ; save the desired state (input)

;M089 BEGIN
	test    POWER_STATUS,FW_IDLE_ACTIVE
	jz      DACt_Connect            ; Go connect and enable
	test    fAPM_STATE,1            ; are we also enabled ?
	jnz     DACt_ChkForDiscon       ; yes, go see if we have to disconnect
	mov     cx,APM_ENABLE_FUNC
	call    Do_APM_Enable_Disable   ; enable APM 
	mov     fAPM_STATE,1            ; flag APM enabled state

DACt_Connect:
;M005 BEGIN - for resident driver call Check_and_Init_APM through pointer

IFDEF POWERALONE
	call    far ptr Check_and_Init_APM ; M003: always check for APM presence
ELSE
	call    Check_and_Init_APM_Ptr
ENDIF ; POWERALONE

;M005 END

	jnc     DACt_ChkForDiscon       ;  before connecting to it
	jz      DACt_APM_ConnErr        ; ignore if APM not present
	clc

DACt_ChkForDiscon:
	pop     bx                      ; get back requested APM state
	and     [CMOSFlg],NOT(1)        ; assume not stdmode+apm
	cmp     bl,3                    ; both S/W and F/W pwmgmt ON ?
	je      DACt_End                ; Yes, all done

; User requested APM be ON but we should stay disconnected (POWER STD)

	call    Do_APM_DisConnect
	and     POWER_STATUS,NOT (FW_IDLE_ACTIVE)            
	test    fAPM_PRESENT,1          ; M091
	jz      DACt_End                ; M091
	or      [CMOSFlg],1             ; M091 set POWER_STDMODE+APM
	mov     [CMOSPollCount],MAXCMOSPOLLCOUNT ; M091
; M089 END

DACt_End: xor   ax,ax                   ; M094  - ax=0 -> no error
DACt_Ret:
	ret

DACt_APM_ConnErr:
	pop     bx
	mov     ax,ERROR_PM_ALREADY_CONNECTED   ; put valid error code here
	stc
	jmp     short DACt_Ret

Do_APM_Control  endp

ENDIF
;********************** TIMER TIC READ ROUTINE *******************
;********************** OEM EXTENDABLE ***************************
; ax = READ_TMR0( void)
; purpose: to return tic stored in timer0
;
; M0FF:
; OEMs NOTE:
; This procedure should always return a count between 64k and 1
; This code assumes the existence of 8253 chip only and does not care for
; the double countdown in mode 3. The algorithm in Chk_delay will take care
; of that problem to quite some extent.
;
; There is an alternate code for 8254 chip included here (within IFDEFs).
; Using this code, if the machine has 8254, will result in accurate timings.
;
; Further CAVEAT: WIN 3.0 does not support the readback command used in the
; 8254 code here and so POWER will not work properly if 8254 code is used 
; with win 3.0 even if the machine has an 8254.
;
; DOS usually uses mode 3 in both 8253/54. ( mode 2 counts the MAXCOUNT
; just once whereas mode counts it twice in a tic period). But in 8254,
; we can get half cycle we are in, in mode 3 whereas we can't get that
; info in 8253.   

PUBLIC Read_tmr0
Read_tmr0 proc near
	assume  ds:nothing, es:nothing

IFNDEF	_8254
;
; 8253 code

	cli
	mov     al,0
	out     43h,al
	jmp     $+2
	in      al,40h
	jmp     $+2
	mov     ah,al
	in      al,40h
	sti
	xchg    ah,al
	ret
ELSE				; 8254 specific code
	push    bx
	mov     al,0c2h         ; read mode as well as count of tmr0
	cli
	out     43h,al
	jmp     $+2

	in      al,40h		; status byte
				; bit 7 -> first half cycle
				; bit 1-3 -> mode 
	mov     bl,al		; save status in bl&bh
	mov     bh,al
	jmp	$+2

	in      al,40h		; low byte of count
	jmp     $+2
	mov     ah,al

	in      al,40h		; high byte of count
	sti
	xchg    ah,al		; ax = current tmr count

	and     bl,0eh          ; mask everything but mode bits 
	cmp     bl,6            ; mode 3 ?
	jne     rt_ret		; if mode 2 no need for adjustments
	shr     ax,1            ; halve for mode 3 cycle 1
	test    bh,80h          ; is this cycle 2 ?
	jz      rt_ret		; yes, the count is already correct 
	add     ax,8000h        ; first cycle, so add 32k to the count
rt_ret:
	pop     bx
	ret
ENDIF

Read_tmr0 endp

; ******************************* SOUND routines **********************
IFDEF DEBUG                  ; only use this code on test version

START_SOUND proc

	mov     al,0
	out     42h,al
	jmp     $+2
	mov     al,50h
	out     42h,al

	mov     al,3
	in      al,61h
	jmp     $+2
	or      al,3
	out     61h,al
	ret

START_SOUND endp

END_SOUND proc
	in      al,61h
	jmp     $+2
	and     al,not 3
	out     61h,al
	ret
END_SOUND endp

ENDIF                                   ; IFDEF DEBUG

; *****************************************************

; Z.flag CheckV86()
; return - NZ, if 386 V86 or protected  mode
;	   ZR  if real mode
; OEMS: NOTE:
; the flag CV86FLG in CONTROL data controls checking 86 mode.
; This procedure is just there so that we can avoid executing a CPU HLT in
; protected mode, if the protected mode software does not support HLT properly.
; Most DOS extenders/EMS and protected mode software supports HLT instruction
; and so, the default is : WE DON'T CHECK for Protected mode for doing HLTs.
; 

PUBLIC CheckV86
CheckV86 proc near
	assume  ds:Bios_Data, es:nothing

	test    [CONTROL].CV86FLG,1     ; Avoid  test
	jz      cv8ret                  ; if bit cleared

	test    [CONTROL].CV86FLG,2             ; is it a 386?
	jz      cv8all                  ; don't know yet ...
cv8386:
	.386p
	smsw    [MSW]

	.8086
	test    [MSW],1                 ; check for protected mode
	ret                             ; NZ, yes

cv8all: push    ax                      ; run through all proccesors

	push    sp                      ; below 286?
	pop     ax
	cmp     sp,ax
	jne     cv8ex0                  ; yes, disable check

	pushf                           ; check TS
	pop     ax                      ; should be zero for 286
	or      ax,4000h                ; try setting TS high
	push    ax
	popf
	pushf
	pop     ax                      ; restore TS

	test    ax,4000h                ; did it remain 0?
	jz      cv8ex0                  ; yes, 286

	pop     ax
	or      [CONTROL].CV86FLG,2     ; Set 386 bit to save time
	jmp     short cv8386

cv8ex0: xor     ax,ax                   ; We don't have to test again
	mov     [CONTROL].CV86FLG,ax    ; return ZF
	pop     ax

cv8ret: ret
CheckV86 endp

;***************** DO_IDLE_DOS ****************
; BUGBUG - nagara - this procedure is never called and can safely be
; removed  for POWER version 1.0. (note that there is a reference to this
; procedure in power_init procedure at the end of this file)
;
PUBLIC Do_idle_dos
Do_idle_dos proc far
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data
	call    DO_IDLE
	pop     ds
	assume  ds:nothing
	ret

Do_idle_dos endp

; ***************************  DO_IDLE ***************************
; ************************ OEM EXTENDABLE ************************
;
; CY.flag DO_IDLE()
; purpose: To put CPU in a low power state via a halt or
;          OEM supplied OEM_IDLE routine.
; passed:  control.IDLE_FLG - enables/disables DO_IDLE
; return:  Carry Set, if IDLE was executed
;
; EntryPoints: I2fIdleEntry
; PseudoCode:
;	Do_idle:
;		if (I2f_aware app) ret;
;	I2fIdleEntry:
;		if (Idle_detection disabled) ret;
;		if (activity detected)
;			ret;
;		if (int flags disbled) ret;
;		update idle statistics counter;
;		if (SOUND-ACTIVE)
;			start sound;
;		set IDLTIC flag;
;		if (APM enabled)
;			call APM_CPU_IDLE
;		else
;			if (OEM_EXTENDED)
;				call OEM_EXTENSION
;			else
;				if (not protected mode)		\\ checkV86
;				do CPU_HLT
;		// end of idle
;		if (SOUND_ACTIVE)
;			end sound;
;		ret.

PUBLIC Do_idle
Do_idle proc near
	assume  ds:Bios_Data, es:nothing

; JAH3 Start - If app supports IDLE only idle on int 2f idle entry point below


	push	ax			; M099
	push	ds
	mov	ds,PSPsegment		; DS:BX --> Current PSP in Dosdata
	assume	ds:nothing
	mov	ax,WORD PTR ds:[CUR_PSP_OFFSET]	; ax -> current app's PSP M099
	pop	ds			; M099
	assume	ds:Bios_Data
	cmp	ax, PSPOf2fApp		; M099
	pop	ax			; M099

	je	didex1			; Idle aware app so don't idle

	PUBLIC I2fIdleEntry
I2fIdleEntry:				; Entry point from int 2fh handler

; M103 - BEGIN - (code moved from do_idle entry)
; Check for POWER OFF. If IDLE DETECTION is disabled, we should not do idles.

	test    POWER_STATUS,SW_IDLE_ACTIVE ; do this only if IDLE_DETECT enabled
	jnz     didbgn
didex1: clc                             ; idle failed  to execute
	ret

didbgn:
; M103 END
	push    ax

	; if activity has been detected DO_IDLE will be avoided
	; for a period measured in timer interrupts

	cmp     [SPDUP],0               ; is system speed up?
	je      did1ot

	mov     ax,[SPDUP_DLY]          ; check if enough time
did3ot: cmp     [SPDUP_CNT],ax          ; has passed to return
	jae     did2ot                  ; to normal idle

didxxx: pop     ax
	jmp     short didex1

did2ot: mov     [SPDUP],0
	mov     [SPDUP_DLY],0

did1ot: in      al,21h                  ; get interrupt mask
	test    al,3h                   ; make sure timer and
	jnz     didxxx                  ; keyboard aren't masked 

	add     word ptr [INFO].IDLE_TOT,1
	adc     word ptr [INFO].IDLE_TOT+2,0


IFDEF DEBUG

	test    [CONTROL].IDLE_FLG,SOUND_ACTIVE
	jz      did0ot

	call    START_SOUND

ENDIF                                   ; IFDEF DEBUG

did0ot: mov     [IDLTIC],1              ; set IDLE flag ; M004

didhlt:

IFDEF INCL_APM
; Note that we should also see if we are connected to APM or not. But it is not
; needed because fAPM_STATE will be 1 for only STD mode or ADV mode.
; And we will come here only for ADV mode, and SW_IDLE_DETECTION_ONLY cases
; in the second case, we should disabled APM and so fAPM_STATE will be 0
;
	cmp     fAPM_STATE,0            ; is APM present and enabled ?
	je      didNoAPM
	call    Do_APM_Idle
	jmp     short didResume
didNoAPM:
ENDIF   ; for IFDEF INCL_APM

IFDEF OEM_EXTENSION
	call    OEM_IDLE
ELSE
	call    CheckV86                ; M004 AVOID HLT IN V86
	jnz     didNoHlt
	sti                             ; interrupts must be enabled
	hlt                             ; or system will halt ...
didNoHlt:
ENDIF

didResume:

didot2: 
IFDEF DEBUG        
	test    [CONTROL].IDLE_FLG,SOUND_ACTIVE
	jz      didex0

	call    END_SOUND
ENDIF                                    ; IFDEF DEBUG

didex0: pop     ax
	stc                             ; CY.set halt executed
	ret
DO_IDLE endp

;********************* APM functions ***********************************

IFDEF INCL_APM

;********************* APM IDLE routine ********************************
; Called by Do_Idle procedure when APM BIOS is present
; 
; Psuedocode:
;       call APM_IDLE API
;       if (clock changes for CPU_IDLE API) {
;               call CPU_BUSY API
;       }
;
; Entry: nothing
; exit: nothing
;
;***********************************************************************
; Do_APM_CPUBUSY        Entry Point             (M002 addition)
;       Calls APM CPUBUSY API to return CPU to full speed after a resume
;               or a CPU_IDLE
; Entry: none
; Exit: none
;***********************************************************************
 
Do_APM_Idle     Proc    near

	push    bx
	mov     ax,APM_CPUIDLE_FUNC
	int     15h             ; make CPU_IDLE API call
	pop     bx
;
Do_APM_CPUBUSY  label   near            ; M002; added this entry point
;
; Check if we need to make a CPU_BUSY call 
	test    APM_FLAGS,APM_SLOW_CLOCK        ; does CPU_IDLE slows clock ?
	jz      dai_End

; need to make CPU_BUSY call
	push    bx
	mov     ax,APM_CPUBUSY_FUNC
	int     15h             ; make CPU_BUSY call to speedup CPU
	pop     bx
dai_End:
	ret

Do_APM_Idle     endp

;********************* Do_APM_Connect **********************************
; Purpose: to connect to APM BIOS as the coop.process (real mode only)
; Entry: none
; exit: CY      - connect failed
;               ax = error code
;       NC      - connection succeeded
;***********************************************************************

Do_APM_Connect  proc    near            ;M005

	test    fAPM_PRESENT,1          ; Do this only if APM is present
	jz      DAC_End
	mov     ax,APM_CONNECT_FUNC
	mov     bx,APM_SYSTEM_BIOS
	int     15h
	jc      DAC_End
	mov     fAPM_CONNECT,1
DAC_End:
	ret
Do_APM_Connect  endp

;********************* Do_APM_Disconnect **********************************
; Purpose: to disconnect from APM BIOS 
; Entry: none
; exit: CY      - Disconnect failed (can this ever happen ?)
;               ax = error code
;       NC      - disconnect succeeded
;***********************************************************************

Do_APM_Disconnect       proc    near

	test    fAPM_CONNECT,1          ; disconnect only if WE are 
	jz      DAD_End                 ; connected     
	mov     ax,APM_DISCONNECT_FUNC
	mov     bx,APM_SYSTEM_BIOS
	int     15h
	jc      DAD_End
	mov     fAPM_CONNECT,0
DAD_End:
	ret
Do_APM_Disconnect       endp
;********************* Do_APM_Enable_Disable **********************************
; Purpose: to enable/disable all power management 
;
; Entry: CX = APM_DO_DISABLE -> disable all power management
;           = APM_DO_ENABLE  -> enable all power management
;
; exit: CY      - function unsuccessful
;               ax = error code
;       NC      - function succeeded
;***********************************************************************


Do_APM_Enable_Disable   proc    near    ;M005

	mov     ax,APM_ENABLE_DISABLE_FUNC
	mov     bx,APM_ALL_DEVICES
	int     15h
	jnc     DAED_Ret
	mov     ax,ERROR_PM_NOT_CONNECTED
DAED_Ret:
	ret

Do_APM_Enable_Disable   endp


ENDIF           ; for IFDEF INCL_APM 

;********************* General Idle Check Routine **********************
; GUTS of INT16/INT28 IDLE DETECTION CODE
;
; purpose: called by INT 28 and 16 interrupt handlers
;       This is the common routine which checks elapsed time between
;       interrupts in order to adjust idle signaling.
;
;       Entry : SI = base address of PERIOD_INFO structure for this interrupt
;               DI = address of total delay accumulator for this interrupt
;               BX = address of time accumulator for this interrupt
;               DS = our data segment
;       Exit  : nothing
;
;	REGS USED: AX,CX,FLAGS
;
; 	(Look at the POWER.EXE documentatiion for a complete alg. description
;	and pseudocode).


public Chk_Delay
Chk_Delay       proc
	assume  ds:Bios_Data, es:nothing
;entry point for I28 idle checking
	xor	cx,cx

; entry point for i16 idle checking
; for i16: cx = 1

Chk_i16idle	label	near
;
	test    [POWER_STATUS],SW_IDLE_ACTIVE  ; is idle detection on ?
	jz      cd_Ret			;
;
; if we see a timer tic bet last int and this int, we probably waited too
; long (or we cannot accurately calculate the time between them)
	push    ax
	cmp     word ptr [bx]+2,1      ; waited too long?
	jae	cd_ClrTmr		;NSM3 yes, do a recount

cd_readtime:
	call    READ_TMR0               ; read return time (in AX)
	sub     [bx],ax			; NSM3
	jc	cd_ClrTmr		; Go recount on overflow

; check for a very high value of time interval
;
cd_1: 	mov     ax,[si].BASE            ; is TIC >= BASE+THRESHOLD?
	add     ax,[CONTROL].THRESHOLD
	jnc     cd_2
	mov     ax,0FFFFh

cd_2:
	cmp     [bx],ax
	jae     cd_transf_period        ; yes, ignore reading   
;
; branch off for adapt/idle cycles
;
	cmp     word ptr [si].ADAPT,0   ; are we in adapt cycle ?
	jne     cd_doAvg		; yes go do avg.

; IDLE CYCLE:
; we are in idle cycle. check to see if we are within allowed limites for
; idle
	mov     ax,[si].BASE            ; is TIC <= BASE+NOISE?
	add     ax,[si].NOISE
	cmp     [bx],ax
	ja      cd_adapt                 ; no, go for adapt
;
;BUGBUG: we are only checking for upper limit and do not check for lower limit
; Checking lower limits will help us avoid some aliasing problems with the 8253
; timer reading code. (nagara dec/91)

; idle cycle processing

cd_doidle:
	call    DO_IDLE
	adc     word ptr [di],0         ; accumulate idle time
	adc     word ptr [di]+2,0
	mov     word ptr [bx]+2,0       ; avoid speed up error

cd_transf_period:
	mov	ax,[bx]			; take current interval and store it
	mov     [si].PERIOD,ax		; as PERIOD
cd_ClrTmr:
	call    ClearTimer              ; do APP_speedup if needed, clear 
					; timer accumulator for this int.
	pop     ax

cd_Ret:
	ret



; ADAPT CYCLE PROCESSING:
;
; try calculating avg.
; [bx] = current period
; [si].PERIOD = avg so far
; if the current period is not within 50% of prev. avg we check to see if
; we can ignore this period for avg and go for next one. If we have seen 
; a no of such high(or low) values in the cycle so far, then start adapt
; cycle all over again

cd_doAvg:
	mov     ax,[CONTROL].ADAPT_DLY
	cmp     [si].DELAY,ax           ; is delay over?
	jae	cd_end_adapt		; yes, go cmp our avg with baselineref
	mov	ax,[si].PERIOD
	push	ax
	add	ax,[si].PSPREAD		; ax = Higher limit for spread
	cmp	ax,[bx]			; is current period more than spread 
        pop	ax			; above our prev. period/avg
	jb	cd_throw_sample
	sub	ax,[si].PSPREAD		; ax =  lower limit for spread
	cmp	ax,[bx]			; 
	ja	cd_throw_sample		; if current period < lower spread limit
					; then throw sample
;IFDEF OLDVERSION
;	mov	ax,[bx]			; get current period
;	add	ax,[si].PERIOD		; add previous period
;	jnc	cd_4
;
;current avg + period is >64k. 
; no point in trying to find an avg with these figures. go take new avg
;
;	mov	ax,[BaseLineRef]
;	jmp	short cd_5
;
;cd_4: 	shr	ax,1
;
;cd_5:	mov	[bx],ax
;ELSE
;
; M106 - BEGIN  - collect 16 sample timings and do avg on them ; use this avg
; for the next 16 samples. 
	mov	ax,[bx]			; get current period
	add	[si].ACC_COUNTER,ax	; add it to accumulated value
	adc	[si].ACC_HI,0
	inc	[si].SAMPLE_COUNT
	cmp	[si].SAMPLE_COUNT,MAX_SAMPLES_PER_AVG	
	jb	cd_ClrTmr
	push	dx
	mov	dx,[si].ACC_HI
	mov	ax,[si].ACC_COUNTER
	shr	dx,1
	rcr	ax,1			; /2
	shr	dx,1
	rcr	ax,1			; /4
	shr	ax,1			; /8
	shr	ax,1			; /16
	add	ax,[si].PERIOD		; add previous avg
	shr	ax,1			; / 2
	pop	dx
;ENDIF
; M106 END

cd_SetNewAvg:
	mov     [si].PERIOD,ax
	call	CalcNewSpread
	mov	[si].PSPREAD,ax
; M106 BEGIN - initialise the time accumulation counters for avg.
;IFDEF OLDVERSION
;ELSE
	mov	[si].ACC_COUNTER,0
	mov	[si].ACC_HI,0
	mov	[si].SAMPLE_COUNT,0
;ENDIF
; M106 END
	jmp	short cd_ClrTmr        	; go for next int


; START ADAPT_CYCLE:
; we are above the allowed idle limits; go for adapt cycles

cd_adapt:
	mov     word ptr [si].ADAPT,1   ; start measuring increase
	mov     word ptr [si].DELAY,0
	mov	[ErrSampleCount],0
	mov	ax,[bx]
	jmp     short cd_SetNewAvg

cd_end_adapt:
	mov     word ptr [si].ADAPT,0
	mov     ax,[si].PERIOD             ; for next compare ...
	jcxz	cd_setbase		; for i28 and i2f, no baseline ref.
	cmp	ax,[BaseLineRef]	; for i16, have a max limit for period
	jbe	cd_setbase		; 
	mov	ax,[BaseLineRef]
	mov	[si].BASE,ax		; go up to the max.allowed base
	call	CalcNewNoise
	mov	[si].NOISE,ax
	jmp	cd_transf_period

; Count the errors while in adapt cycle; if the error count exceeded the
; max allowed value, start the adap cycle all over again with the last error
; interval as the new AVG
;
cd_throw_sample:
	inc	[ErrSampleCount]	
	mov	ax,[ErrSampleCount]
	cmp	ax,[CONTROL].MAXERRSAMPLE
	ja	cd_adapt		; go try adapting again
	jmp	cd_ClrTmr

cd_setbase:
	mov     [si].BASE,ax
	call	CalcNewNoise
	mov	[si].NOISE,ax		; noise = some % of base depending on
					; current savings mode
	jmp	cd_doidle
	
Chk_Delay       endp


;*************** Timer clear and app speedup adjustment
;       Helper routine for Chk_Delay.  Called independently
;       by I16_IDLE when it detects a key waiting.  Calls APP_SPDUP
;       if required, and clears out timer overflow
;
;       Entry : BX = address of timer accumulator for this interrupt
;       Exit  : nothing
;       Uses AX -- caller must preserve

public  ClearTimer
ClearTimer         proc
	assume  ds:Bios_Data
	cmp     word ptr [bx]+2,3	; BUGBUG - nagara; this code is
				; strange; it looks for 3 timer tics ?
	jb      ct_0
	call    APP_SPDUP               ; speedup proportional
					; to delay...
ct_0:
	mov     word ptr [bx]+2,0       ; clear key timer
	call    READ_TMR0		; read current timer value and
	mov     [bx],ax			; store it 
	cmp     word ptr [bx]+2,0
	jne     ct_0

	ret

ClearTimer         endp


;********************* APPLICATION IDLE CHECK *******************
; purpose: check application idle (INT 2F Function 1680H)
;          if interrupt is detected than DO_IDLE should be called
;          the interrupt should be absorbed.
; PseudoCode:
;	if (win enh mode startup)
;		set flag to avoid BaseLineRef calculation at EXEC time
;	if (TaskSwapper startup or Win Enh mode startup)
;	( call previous int 2f guy
;	  store the prev. inst.data ptr
;	  pass back our inst data ptr
;	  iret
;	}
;	if (win enh mode end)
;		reset flag to start BaseLineRef calc at EXEC time
;	if (idle interrupt) {
;		if (idle detection/i2f strategy disabled) chain to next guy;
;		remember current PSP as I2f_Aware app
;		call do_idle;
;		iret
;	}
;	if (POWER API call)
;		service the API call;
;		iret
;	else
;		chain to next i2f guy
;	

PUBLIC I2f_idle
I2f_idle proc   far
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds, Bios_Data_Word
	assume  ds:Bios_Data
;
	cmp	ax, 4b05h		; TaskSwapper startup ?	M100
	je	Instance_pwr		; M100
;
	cmp	ax,(MultWin386 * 256 + Win386_Init)	; NSM2
	je	I2fWinEnhStart				;NSM2
	cmp	ax,(MultWin386 * 256 + Win386_Exit)	;NSM2
	je	I2fWinEnhEnd				;NSM2
;
	cmp     ax,1680h                ; Any app idles?
	jne     i2F_chk_ours            ; check for a service call to us

; JAH3 Set PSPOf2fApp to the current app's PSP

	test    [POWER_STATUS],SW_IDLE_ACTIVE	;Make sure idle_detect is on
	jz	i2Fnxt			; Power is off so just chain
	test    [CONTROL].IDLE_FLG,APP_ACTIVE ; Make sure power is on
	jz	i2Fnxt			; Power is off so just chain


	push	ax			; M099
	push	ds
	mov	ds,PSPsegment		; DS --> Current PSP in Dosdata
	assume	ds:nothing
	mov	ax,WORD PTR ds:[CUR_PSP_OFFSET] ; ax -> current app's PSP M099
	pop	ds
	assume	ds:Bios_Data
	mov	PSPOf2fApp, ax		; M099
	pop	ax

; JAH3 end 

; load up registers and call do_idle
	push    di
	lea     di,[INFO].APP_TOT

; JAH	call    DO_IDLE			
	call	I2fIdleEntry		; JAH Use new idle entry point

	adc     word ptr [di],0         ; accumulate idle time
	adc     word ptr [di]+2,0

I2FIdleCleanup:				; JAH New label
	pop	di
	pop	ds
	iret

i2Fnxt: 
	jmp     dword ptr pwr_i2f_next

i2f_chk_ours:
	cmp     ah, MultPWR_API         ; is this a call for one of our ; M077
	jne     i2fnxt                  ; services ?
	pop     ds                      ; restore original vector
	call    Pwr_Services            ; one of our services
	iret                            ; no propagation

; If Win Enh mode starts  we don't need to recalc our baseline anymore
; just set a flag so that we avoid baseline calc oper at exec time
i2fWinEnhStart:
	test	dx,WIN286STDMODE	; is this a standard mode win ?
	jnz	i2fnxt			; yes, don't worry about it
; enhanced mode: set win3Flg	
	inc	[NoBaselineCalc]
	jmp	short Instance_Pwr	; M100
i2fWinEnhEnd:
	dec	[NoBaselineCalc]
	jmp	i2fnxt
;
; M100 - begin
;
Instance_Pwr:
	jmp	dword ptr Pwr_call_i2f		; M102
Calli2fRet:					; M102
	mov     ds,Bios_Data_Word               ;reinit DS to Bios_Data M102
	assume  ds:Bios_Data

	mov	word ptr Pwr_SINextDev+0, bx
	mov	word ptr Pwr_SINextDev+2, es
	mov	bx, offset Bios_Data:Pwr_InstData
	push	ds
	pop	es

	pop	ds
	iret

;
; M100 - end
;
I2f_idle endp

;************************ I28_IDLE *********************

PUBLIC I28_idle
I28_idle proc
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

	inc     [I28].PCOUNT

	test    [CONTROL].IDLE_FLG,DOSYIELD_ACTIVE
	jz      i28ret

		; load up registers and call general delay checker
	push	cx
	push    bx
	push    si
	push    di
	lea     bx,I28_TMR0
	lea     si,I28
	lea     di,[INFO].DOS_TOT
	call    Chk_Delay
	pop     di
	pop     si
	pop     bx
	pop	cx

i28ret:

IFDEF   POWERALONE                      ; chain to next
	jmp     dword ptr pwr_i28_next
ELSE
	pop     ds
	assume  ds:nothing
	iret                            ; don't need to chain if BIOS resident
ENDIF

I28_idle endp

;********************* Shell idle check ******************************
; purpose: check idle 
;          if interrupt is detected than DO_IDLE should be called
;          the interrupt should be absorbed.
; CAVEAT: This interrupt should never be used, is version bound and subject
; to change in future versions.

PUBLIC I2A_idle
I2A_idle proc   far
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds, Bios_Data_Word
	assume  ds:Bios_Data

	cmp     ah,84h                  
	jne     i2Anxt            	; check for a service call to us

; load up registers and call do idle
	test    [CONTROL].IDLE_FLG,DOSIDLE_ACTIVE ; Make sure power is on
	jz	i2Anxt			; Power is off so just chain
	push    di
	lea     di,[INFO].SHELL_TOT
;;	call    DO_IDLE			; M102

	call	I2fIdleEntry		; M102

	adc     word ptr [di],0         ; accumulate idle time
	adc     word ptr [di]+2,0
	pop	di
	pop	ds
	iret

i2Anxt: 
	jmp     dword ptr pwr_i2a_next

I2A_idle endp
;************************ I16_IDLE *********************
; Our Int16 handler
; PseudoCode:
;	if (reentering) jmp to prev.handler;
;	if (fn NOT keyboard_poll calls or
;		NOT blocked get key calls)
;		jmp to prev.handler
;	if (fn == keyboard poll call) {
;		call the prev. handler
;		if key_waiting, ret to caller.
;		call chk_delay for possible idles;
;		ret to caller.
;	}
;	if (fn == blocked get key call) {
;		while (no key_hit)
;			call do_idle	// directly trigger idles
;	}
;		

PUBLIC I16_IDLE
I16_IDLE proc far
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data


	cmp     [IN_KYC],0		; reentering ?
	jne     kybxxx

; check for keyboard poll calls
	cmp     ah,01h
	je      kbpchk
	cmp     ah,11h
	je      kbpchk
;
; check for Blocked get_key calls
	or	ah,ah
	jz	kbChkIdle
	cmp	ah,10h
	jz	kbChkIdle
kybxxx: 
	jmp     dword ptr pwr_i16_next	; chain to prev handler

kybClrTmr:
        push    ax
        push    bx
        lea     bx,KYC_TMR0
        call    ClearTimer
        pop     bx
        pop     ax
        jmp     short kybxxx


kbpchk: inc     [KYC].PCOUNT
	test    [CONTROL].IDLE_FLG,KYC_ACTIVE
	jz      kybClrTmr

	mov     [IN_KYC],1              ; ONLY ALLOW ONE RE-ENTRY

	jmp     dword ptr pwr_call_i16  ;call old int 16h handler
;
;We return to the label below after calling the previous int 16h handler
;
calli16ret:
	mov     ds,Bios_Data_Word               ;reinit DS to Bios_Data
	assume  ds:Bios_Data

	pushf
	push    bx
	lea     bx,KYC_TMR0

		; if a key is ready, just clear out our timer and
		; return.  If no key is ready, go through the full
		; delay check.  Here we check the flags returned
		; by the INT 16 handler we called.
       
	jz      i16_ChkDelay

		; Just clear the timer
	push    ax
	call    ClearTimer
	pop     ax
	jmp     short kbpret

i16_ChkDelay:
		; load up registers and call delay check
	push	cx
	push    si
	push    di
	lea     si,KYC
	lea     di,[INFO].KEY_TOT
	mov	cx,1
	call    Chk_i16idle
	pop     di
	pop     si
	pop	cx

kbpret: 
	pop     bx
	popf

	mov     [IN_KYC],0              ; can start up again

	pop     ds
	assume  ds:nothing
	ret	2		

; Blocked get_key calls
; see if key is available ; if not, just do idle directly
;
kbChkIdle:
	assume  ds:Bios_Data
; see if there is a key available
	inc	ah			; make it to fn 1 or 11
kbChkKey:
	push	ax			; save function no (1 or 11h)
	jmp     dword ptr kb_call_i16  ;call old int 16h handler
kbChkRet:
	pop	ax			; get back orig. polling fn no
	jz	kbDoIdle		; no key available
	dec	ah			; get back original get key fn(0 or 10)
	jmp	kybxxx			; go get the key
; no key available do idle
kbDoIdle:
	push	ax
	push    bx
	push    di
	lea     bx,KYC_TMR0
	lea     di,[INFO].KEY_TOT
;	call    DO_IDLE
	call	I2fIdleEntry		; JAH4
	adc     word ptr [di],0         ; accumulate idle time
	adc     word ptr [di]+2,0
	mov     word ptr [bx]+2,0       ; avoid speed up error
	call    ClearTimer
	pop	di
	pop	bx
	pop	ax
	jmp	short kbChkKey		; go look for key

I16_IDLE endp


;************************ I08_IdleControl *********************
; POWER's main Timing/CONTROL interrupt
; PseudoCode:
;	check for rollover flag
;	if (rollover)
;		set our local date_update flag
;		do an int 1a to eat the rollover flag // in case?
;	if (time_update_counter is 0)
;		set our local date_update_flag 	// if in POWER STD mode
;	if (SW idle detection enabled)
;	{
;		update Timer counters for i16,i28
;		update idle statistics counters
;		if (AUTO active && time_to_switch strategies)
;			check i16 , i28 counts for the last period
;			enable the int that has the max count and
;			  disable the other one.
;
;	}
;	if (FW idle detection enabled)		// APM enabled
;	{
;		if (time to poll APM) {
;			increment INDOS flag
;			switch stacks to our local stack
;			call Do_APM_POLL  // poll APM for a pm event
;					// if there is, broadcast it
;					// and take necessary action 
;			switch back to old stack
;			decrement INDOS flag
;		}
;	}
;	RET;
;	

PUBLIC I08_IdleControl
I08_IdleControl proc	far
	assume  ds:nothing, es:nothing

	push    ds
	push    ax
	mov     ax,BIOSDATASEG          ; M092 check rollover flag in BIOS
	mov     ds,ax                   ; M092
	xor     al,al                   ; M096
	xchg    al,ds:[ROLLOVERFLG]     ; M096 get Bios rollover flg and 
					;   reset it
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

;
; M107 - begin
;
	or	al, al
	jz	i08Skip1a
	or      [CMOSUpdFlg],al         ; M092 set update flag if rollover
	push	cx
	push	dx
	xor	ah, ah
	int	1ah
	pop	dx
	pop	cx
i08Skip1a:
;
; M107 - end
;
	dec     [CMOSPollCount]         ; M090
	jz      i08SetCMOSFlg           ; M090

i08NoTmUpdate:
	inc     [SPDUP_CNT]             ; speedup delay

	test    [POWER_STATUS],SW_IDLE_ACTIVE        ; is idle detection on ?
	jz      i08_to_end

	inc     [KYC_TMR0]+2            ; timer over flow
	inc     [KYC].DELAY             ; adjust delay

	inc     [I28_TMR0]+2            ; timer over flow
	inc     [I28].DELAY             ; adjust delay


	add     word ptr [INFO].CPU_ON_TIME,1
	adc     word ptr [INFO].CPU_ON_TIME+2,0

	xor     ax,ax                   ; M004  BEGIN
	xchg    [IDLTIC],ax             ; If we were idle during the last
	add     word ptr [INFO].CPU_IDLE_TIME,ax        ; tick period, let's
	adc     word ptr [INFO].CPU_IDLE_TIME+2,0       ; count it
					; M004 END

	test    [CONTROL].IDLE_FLG,AUTO_ACTIVE  ; no AUTO adjust
i08_to_end:
	jz      i08xxx

i08ot1: inc     [SWITCH_CNT]            ; time to switch methods?
	mov     ax,[CONTROL].SWITCH_DLY
	cmp     [SWITCH_CNT],ax
	jb      i08xxx                  ; not yet ...

i08ot2: mov     [SWITCH_CNT],0
	mov     ax,[I28].PCOUNT         ; If( KYC.COUNT < I28.COUNT)
	cmp     ax,[KYC].PCOUNT         ; occurs in windows ...
	ja      i08dos

		; keyboard count is higher than INT 28 count
		; keyboard is highest count, it wins

	cmp     [KYC].PCOUNT,0          ; avoid thrashing
	je      i08xxx
	mov     ax,KYC_ACTIVE
	jmp     short i08set

i08SetCMOSFlg:
	mov     al,[CMOSFlg]            ; M090 ; need to update from CMOS
	or	[CMOSUpdFlg],al         ; M090 ; only if POWER STD mode in APM 
	mov	ax,MAXCMOSPOLLCOUNT	; machines
	mov	[CMOSPollCount],ax	; start counting again
	jmp     i08NoTmUpdate

i08dos:         ; INT 28 count higher than keyboard count
		; INT 28 count is highest, it wins
	or      ax,ax                   ; don't bother if count is 0
	jz      i08xxx
	mov     ax,DOSYIELD_ACTIVE

i08set: and     [CONTROL].IDLE_FLG,NOT ( DOSYIELD_ACTIVE+KYC_ACTIVE)
	or      [CONTROL].IDLE_FLG,ax

i08skp: xor     ax,ax                   ;reset for next round
	mov     [KYC].PCOUNT,ax
	mov     [I28].PCOUNT,ax

i08xxx: 
	test    [POWER_STATUS],FW_IDLE_ACTIVE        ; M089 is F/W mgmt on ?
	jz      i08ret                ; M089 No need to poll APM in POWER
					; STD mode
IFDEF INCL_APM
; now check to see if we need to call APM for a PMEVENT 
	cmp     fAPM_STATE,0            ; is APM enabled ?
	je      i08ret
	dec     APM_POLL_COUNT
	jnz     i08ret
;
; NSM5 - BEGIN
	mov     ax,[APM_MAX_POLLCOUNT]  ; first reset the counter
	mov     APM_POLL_COUNT,ax

; time to poll APM for a PM Event
; First check to see if we are reentering our code
; if so, just quit; 
; else, change stacks and poll APM
;
	cli
	cmp	StackInUse,0		; See if called recursively
	jne	i08ret
;
; change stacks
;
	inc	StackInUse		; Show stack is being used
	mov	Old_SS,SS		; Save old SS:SP in bios data area
	mov	Old_SP,SP

	mov	SS,Bios_Data_Word
	mov	SP,OFFSET StackTop
;
; NSM4 - set indos flag so that we won't be swapped while doing our
; baseline calculation
	push	ds
	push	si			; NSM4
	mov	si,[INDOS_Offset]
	mov	ds,[INDOS_Seg]
	assume ds:nothing
	inc	byte ptr ds:[si]
	pop	si
	pop	ds
	assume	ds:Bios_Data
; NSM4 -END
	sti
;
	call	DoAPMPoll		; poll APM for an event,broadcast it etc
;
	cli			
; NSM4 - reset indos flag ; we are safe now and can be swapped out
	push	ds
	push	si
	mov	si,[INDOS_Offset]
	mov	ds,[INDOS_Seg]
	assume ds:nothing
	dec	byte ptr ds:[si]
	pop	si
	pop	ds
	assume	ds:Bios_Data
; NSM4 -END
; restore stack
	mov	SS,Old_SS	; Get entry stack from bios data area
	mov	SP,Old_SP
	mov	StackInUse,0	; Clear value to show not being used.

; NSM5 -END
;
ENDIF					; ifdef INCL_APM
i08ret:
	pop     ax
	pop	ds
	ret

I08_IdleControl endp

; ******************************** DoAPMPoll ***********************
; Poll APM for a PM event
; If there is an event broadcast it
; if the event == request for suspend/stand-by
;	look for rejection by an app
;	if no rejection, call APM back to switch to req. mode.
; if event == resume
;	Update system date and time from CMOS
;
IFDEF INCL_APM

DoAPMPoll	proc	near

; time to poll APM for any PM Event

	push    bx
;
DAPPoll:
	mov     ax,APM_GETPMEVENT_FUNC
	int     15h
	jc      DAP_End                 ; no events -> no work to do 
;
; BX = PM Event
;       1 = stand-by request
;       2 = suspend request
;       3 = normal resume
;       4 = crit. resume
;       5 = battery low
	cmp     bx,APM_NORM_RESUME
	je      DAP_Set_time
	cmp     bx,APM_CRIT_RESUME
	jne     DAP_broadcast

DAP_Set_time:
	INC     APM_RESUME_COUNT
	call    Do_APM_CPUBUSY          ; M002 bring CPU to full speed
	mov     [CMOSUpdFlg],1          ; M090

DAP_broadcast:
	mov     ax,I2F_APM_BROADCAST
	int     2fh                     ; broadcast the pm events to other
					; apps
; BUGBUG: Warning: the function codes for APM BIOS GetPMEvent and 
; the multiplex API broadcast functions are assumed to be the same
	cmp     bl,APM_NORM_RESUME      ; was it a stand-by/suspend request
	jae     DAP_End                 ; if not, nothing else to do
;
; this was an APM request for system stand-by/suspend
; if no apps/tsrs objected to this request, let us call the APM to suspend/
; stand-by
	or      bh,bh
	jnz     DAP_End                 ; somebody objected ? if so quit
	push    cx                      ; M093
	mov     cx,bx                   ; required power mode in CX
	mov     bx,APM_SYSTEM_DEV       ; full system
	mov     ax,APM_SETPWSTATE_FUNC
	int     15h
	pop     cx                      ; M093
IFDEF DEBUG
	jnc     DAP_dbg_Success
	dbg_printchar   'f'             ; CY for a set power state call
DAP_dbg_Success:
ENDIF                                   ; IFDEF DEBUG
; Resumed from a suspend/stand-by; we should get a resume notification
; from APM so that we can go and update the date and time 
	jmp     DAPPoll                 ; go back and poll APM for
					; a resume notification event
;
DAP_End:pop     bx
	ret

DoAPMPoll	endp
ENDIF

;************* I6C_RESUME *****************************
;
; On interrupts 6C, update system date and time from the hardware
; clock.  Int 6C is one way a system may signal a resume event.
; Entire routine is part of modification M080.

I6C_Resume      proc

	assume  ds:nothing,es:nothing

	push    ds
	mov     ds,cs:Bios_Data_Word
	assume ds:Bios_Data

;       Bugbug -- need to sti?
;       Bugbug -- enough stack space?

	push    ax
	push    bx
	push    cx
	push    dx
	push    si
	push    di

IFDEF   POWERALONE                      ; M081
	call    far ptr P_UpdFromCMOS
ELSE
	call    P_UpdFromCMOS_ptr       ; M074 update our date and time
					; from CMOS RTC
ENDIF

	pop     di
	pop     si
	pop     dx
	pop     cx
	pop     bx
	pop     ax

	jmp     dword ptr pwr_i6c_next

I6C_Resume      endp

;************* APPLICATION ACTIVITY MONITORS ***********
; ISRs for the application activity monitor interrupts that we hook and
; related procedures
;
;************************** APP_SPDUP *******************************
; called by all our activity monitoring interrupt handlers
; SPEED UP by setting DOIdle delay count relitive to TIMER interrupts
; that have occurred between polling interrupt ...
; ** REQUIRES TESTING -- At this point APP_SPDUP tends to inhibit idle

PUBLIC APP_SPDUP
App_Spdup proc near
	assume  ds:Bios_Data, es:nothing

	push    ax
	mov     ax,[SPDUP_DLY]          ; machine speed dependent
	add     ax,[CONTROL].SPDUP_RAMP
	cmp     ax,[CONTROL].SPDUP_MAX  ; machine speed dependent
	jb      asdot3
	mov     ax,[CONTROL].SPDUP_MAX
asdot3: mov     [SPDUP],1               ; indicate activity
	mov     [SPDUP_DLY],ax
	mov     [SPDUP_CNT],0
	pop     ax
asdex0: ret

App_Spdup endp

;
; Hardware Keyboard interrupt - called for each key press twice
;
I9_App  proc    
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

	dec     [I9_COUNT]
	jnz     i9ex0
	mov     [I9_COUNT],2
	call    APP_SPDUP
i9ex0:  
	jmp     dword ptr Pwr_i9_next

I9_App  endp

; BIOS screen I/O interrupt
I10_App         proc
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

	cmp     ah,04h                          ; avoid cursor set
	jb      i10ex0                          ; calls
	call    APP_SPDUP
i10ex0: 
	jmp     dword ptr Pwr_i10_next

I10_App         endp

;
; DISK I/O BIOS interrupt
;
I13_App         proc
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

	call    APP_SPDUP

	jmp     dword ptr Pwr_i13_next

I13_App         endp

;
; Communication I/O BIOS interrupt
;
I14_App         proc
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

	cmp     ah,3                            ; avoid status calls
	je      i14ex0
	call    APP_SPDUP                       ; M002
i14ex0: 
	jmp     dword ptr Pwr_i14_next

I14_App         endp

; Printer I/O BIOS interrupt
;
I17_App         proc
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

	cmp     ah,2                            ; avoid status calls
	je      i17ex0
	call    APP_SPDUP
i17ex0: 
	jmp     dword ptr Pwr_i17_next

I17_App         endp

;
; DOS system calls 
;
I21_App         proc
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

	push	ax

; upper bound check
	cmp	ah,MAX_I21_ENTRY	; anything above speeds up
	ja	i21SpdUp

; get action from the lookup table 
	push	bx
	mov	bx,offset i21_table		; look up table for APIs
	xchg	ah,al			; get function code  in al
	xlat
	pop	bx
	or	al,al			; a busy call ?
	jz	i21SpdUp
	inc	al			; idle calls ?
	jz	i21ex0

; special cases
	cmp	ax,644h			; ioctl get input status call ?
	je	i21ex0

	cmp	al,4bh			; is it an exec call ?
	je	i21GetNewBL		; go get a new base line for int 16s

	cmp	ax,505fh		; is it network assign call ?
	je	i21ex0			; ignore it

; only one special case remains now - the direct console io (fn 6) now
; if dl = ff it is an input call and otherwise output call

	cmp	dl,0ffh			; Direct console input ?
	je	i21ex0

; fall through for other special cases - should be changed if we add
; other special cases
i21SpdUp:
	call    APP_SPDUP
i21ex0: 
	pop	ax
	jmp     dword ptr Pwr_i21_next
	
i21GetNewBL:
; Reset I2f Apps's PSP so that the child is not marked as an INT 2f aware App
	mov	PSPOf2fApp, 0		; M099
;
; IF we are within Win ENH mode, no need to adjust BaseLine again
;
	cmp	[NoBaseLineCalc],0	; NSM2 do we need to recalc baselinemax?
	jne	i21SpdUp		; NSM2 no, just do SpeedUp
;
	call	GetNewBaseLine
	jmp	short i21SpdUp
	
I21_App         endp

; ABSOLUTE DISK READ/WRITE interrupts
;
I25_App         proc
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

	call    APP_SPDUP

	jmp     dword ptr Pwr_i25_next

I25_App         endp

I26_App         proc
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

	call    APP_SPDUP

	jmp     dword ptr Pwr_i26_next

I26_App         endp

; ****************************** GetNewBaseLine ***************************
; Called at each exec time  (except within win enh mode)
; calculates BaseLineRef for Int 16 idles by:
;	pumping 9 int16s in a tight loop
;	throwing the first and last 2 int16 intervals
;	throwing the highest and lowest 2 int16 intervals
;	and adding up the remaining five int16 intervals
;	using this added value as the New BaseLineRef.
; 
; NOTE: stack is switched to our local stack and INDOS flag is set during
;	this operation so that task swapper may not swap us out and to
;	avoid stack overruns in case of certain apps.
;
	PUBLIC GetNewBaseLine		; JAH2 - made public for debugging
GetNewBaseLine	proc	near

		; JAH2 - Setup new stack to fix problems with small exec stack


	pushf
	push	si			; NSM4
	cli
	cmp	StackInUse,0		; See if called recursively
	je	SetupStack
	jmp	NewBaseLineExit		; Stack is being used so don't recalc
SetupStack:
	inc	StackInUse		; Show stack is being used
	mov	Old_SS,SS		; Save old SS:SP in bios data area
	mov	Old_SP,SP

	mov	SS,Bios_Data_Word
	mov	SP,OFFSET StackTop
; NSM4 - set indos flag so that we won't be swapped while doing our
; baseline calculation
	push	ds
	mov	si,[INDOS_Offset]
	mov	ds,[INDOS_Seg]
	assume ds:nothing
	inc	byte ptr ds:[si]
	pop	ds
	assume	ds:Bios_Data
; NSM4 -END
	sti
		; JAH2 - End change
; SMR7 BEGIN - BaseLine calculation based on a few samples
;
; Save all regs we use
;
	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di

	mov	si, offset BL_Samples
	push	si
	xor	di, di				; Cumulative sum=0
;
; Pump in INT 16s and till we get 9 samples
;  & accumulate the total period for 9 samples in DI
;

	mov	ah, 1
	int	16h
	mov	bx, [KYC_TMR0]			; Our INT 16 handler would
						;  have read TMR0
bl_loop1:
	mov	ah, 1
	int	16h
	sub	bx, [KYC_TMR0]			; Get period for this sample
	jc	bl_ThrowSample			; ignore it in case of overflow
	mov	[si], bx			; else stow it away
	add	di, bx				;  and it to the total
	inc	si				; bump to next sample
	inc	si
bl_ThrowSample:
	mov	bx, [KYC_TMR0]			; remember current time stamp
	cmp	si, offset BL_EndSamples	; have we got 9 samples ?
	jb	bl_loop1			; no, try again
;
;
; Throw off the 2 highest & lowest samples and sum up the rest 5
;
; always ax,bx contain the highest 2 values so far (such that ax < bx)
;  & cx,dx contain the lowest 2 values so far (such that cx > dx)
; si contains the ptr to the current element
;	   	
;
	pop	si

	xor	ax, ax				; 2 MAXs = 0
	mov	bx, ax
	mov	cx, ax
	dec	cx
	mov	dx, cx				; 2 MINs = ffffh
	push	di				; save the total
bl_loop2:
	mov	di, [si]			; get sample

	cmp	ax, di				; > lo Max ?
	jae	bl_1				; no, chk against hi max
	mov	ax, di				; yes, update least max
	jmp	short bl_2
bl_1:
	cmp	bx, di				; > hi max ?
	jae	bl_2				; no
	mov	bx, di				; update hi max
bl_2:
	cmp	ax, bx				; is hi max < lo max
	jb	bl_MaxOk			; no
	xchg	ax, bx				; make hi max > lo max

bl_MaxOk:
	cmp	cx, di
	jbe	bl_3
	mov	cx, di
	jmp	short bl_4
bl_3:
	cmp	dx, di
	jbe	bl_4
	mov	dx, di
bl_4:
	cmp	cx, dx
	ja	bl_MinOk
	xchg	cx, dx
bl_MinOk:
	inc	si
	inc	si
	cmp	si, offset BL_EndSamples
	jb	bl_loop2
;
	pop	di
	sub	di, ax
	sub	di, bx
	sub	di, cx
	sub	di, dx

	mov	[BaseLineOvf], di	; this is the new calculated 
					; BaseLine ref value for REG mode

; NSM9 - set baelineref depending on the current savings mode value
	call	SetSavingsValues	; set the correct BaseLineRef depending
					; on the current Savings mode
; NSM9 END

	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
;
; SMR7 END
;
		; JAH2 - Restore stack and AX pushed at entry to function
	cli			; Popf at end will reset interrupt enable
; NSM4 - reset indos flag ; we are safe now and can be swapped out
	push	ds
	mov	si,[INDOS_Offset]
	mov	ds,[INDOS_Seg]
	assume ds:nothing
	dec	byte ptr ds:[si]
	pop	ds
	assume	ds:Bios_Data
; NSM4 -END
	mov	SS,Old_SS	; Get entry stack from bios data area
	mov	SP,Old_SP
	mov	StackInUse,0	; Clear value to show not being used.
NewBaseLineExit:
	pop	si		; NSM4
	popf
		; JAH2 - End

	ret

GetNewBaseLine	endp

; NSM9 - BEGIN - 8 levels of savings values/noise spreads
; *******************************************************************
; SetSavingsValues
; Entry:	none	(BaseLineOvf and Savings_mode set)
; Exit:		Current BaseLineRef set
;		Current Noise set
;		Current AvgSpread Set
;
; Regs affected: AX
;
; Uses the shift-right count in BaseLine_Table to shift out the current
; calculated baselineref to arrive at the BaseLineRef for the currently
; selected Savings level.
; also sets NOISE/SPREAD values 
;
;	called by GetNewBaseLine at each EXEC time to set proper values
;		for BaseLineRef, NOISE, SPREAD etc for I16.
; 
SetSavingsValues	proc	near
	push	cx
	push	bx
	xor	cx,cx
	lea	bx,BaseLine_Table	
	mov	ax,[Savings_Mode]	
	cmp	ax,REGSAVINGSMODE	; is it REG ?
	xlat
	mov	cl,al
	mov	ax,[BaseLineOvf]
	mov	bx,[BaseLineOvf]
	jae	SSV_AddLoop
	xor	bx,bx			; start with 0% for all other modes
					; below REG
SSV_AddLoop:	
	jcxz	SSV_SetBLRef
	shr	cl,1			;	
	jnc	SSV_HalveBL
	add	bx,ax
SSV_HalveBL:
	shr	ax,1
	Jmp	SSV_AddLoop
; BX = has the new BaseLineRef
SSV_SetBLRef:
	mov	[BaseLineRef],bx	; set new baseline Ref
	push	si
	lea	si,KYC
	call	CalcNewNoise
	mov	[si].NOISE,ax
	call	CalcNewSpread
	mov	[si].PSPREAD,ax
	pop	si
	pop	cx
	pop	bx
	ret

SetSavingsValues	endp

; *************************** CalcNewNoise/Spread ********************
;
; Entry:	[Savings_Mode]	set
;		si = Period structure for i16 or i28
; Exit:		ax = New Noise (or Spread Value) depending on the current 
;		savings_mode
;
; Regs affected: AX
; 
; EntryPoint:	CalcNewSpread
;
; Uses the shift-right count in  Savings_Table to shift out current BASE/AVG
; to arrive at the correct NOISE/SPREAD for the currently selected savings
; value.

CalcNewNoise	proc	near
	mov	ax,[si].BASE
	jmp	short	CNN_CalcPC

CalcNewSpread	label	near
	mov	ax,[si].PERIOD		; take current avg
; ax = current Base /AVG 
CNN_CalcPC:
	push	bx
	push	cx
	mov	cx,ax
	mov	ax,[Savings_Mode]
	dec	al			; 0 based table
	lea	bx,Savings_Table
	xlat
	xchg	ax,cx		; cl = mode bits and ax = BASE/AVG
	xor	bx,bx
CNN_AddLoop:
	or	cl,cl
	jz	CNN_RetValue
	shr	ax,1
	shr	cl,1
	jnc	CNN_AddLoop
	add	bx,ax
	jmp	CNN_AddLoop
CNN_RetValue:
	or	bx,bx
	jz	CNN_Ret
	mov	ax,bx		; ax = new value for NOISE/SPREAD
CNN_Ret: 
	pop	cx
	pop	bx
	ret	

CalcNewNoise	endp

; NSM9 END

IFDEF INCL_APM

; *********************************** Check_and_Init_APM ***************
; This procedure is called at init time ; checks for the presence of
; APM BIOS , if present connects to it (real mode only).
; If the connection is successful, we set APM_PRESENT flag so that
; later we can make use of APM 
;
; Input: none
; output: CY +NZ if NO APM
;         CY +ZR if cannot connect to it
;               or if APM cannot be enabled
;         NC = success (APM found+connected to APM+APM enabled state)
;       (fAPM_Present set to TRUE if APM present)
;       (APM_RESUME_COUNT initialised to 0)
;       (fAPM_CONNECT set to TRUE if connected to APM)
;       (fAPM_STATE set to TRUE if APM connected+enabled)
;               -> this is referred to before polling APM, before calling APM
;                       for idle
;
; regs affected: AX,BX,CX,DX,flags
;
; M001  : Created; APM support code

	public  Check_and_Init_APM

Check_and_Init_APM      proc    far
	
	mov     ax,APM_INSTALLCHK_FUNC
	mov     bx,APM_SYSTEM_BIOS      
	int     15h                     ; Check for APM BIOS presence

; CY set if no APM present
	jc      CAIA_NoAPM

; NC; check for APM signature (BH = "P" and BL = "M")   ; M003
; AX = version no (in BCD)
; CX = APM flags        (bit 0 - 16bit prot.mode intrfc. supported
;                        bit 1 - 32bit "  "      "  "      "  "
;                        bit 2 - set if CPU_IDLE call slows proc.clock
;                        bit 3 - set if BIOS power management disabled.

	cmp     bx,APM_SIGNATURE        ; M003
	jne     CAIA_NoAPM              
	mov     APM_FLAGS,cx            ; store the APM flags
	mov     APM_VER,ax              ; store APM BIOS ver no

	mov     fAPM_PRESENT,1          ; set APM flag  ; M002

; connect to APM
	call    Do_APM_Connect          ;M005
	jc      CAIA_APM_Err            ; cannot connect ? assume APM
					; not present/functional
;
; check for APM BIOS disabled state
	test    APM_FLAGS,APM_PWMGMT_DISABLED
	jz      CAIA_APM_Success                ; APM in enabled state; all success
;
	mov     cx,APM_ENABLE_FUNC
	call    Do_APM_Enable_Disable   ;M005
	jnc     CAIA_APM_Success
	mov     fAPM_PRESENT,0          ; reset APM flag since we couldn't
						; connect to it
	mov     fAPM_STATE,0            ; disabled state        
	mov     APM_RESUME_COUNT,0      ; initialise RESUME_COUNT
	jmp     short CAIA_APM_Err

CAIA_APM_Success:
	mov     fAPM_STATE,1            ; APM in enabled state
	mov     APM_RESUME_COUNT,0      ; initialise RESUME_COUNT
	or      POWER_STATUS,FW_IDLE_ACTIVE     ; M003: indicate APM is enabled
CAIA_End:
	ret

CAIA_NoAPM:
	mov     al,1                    ; APM not there; CY + NZ
CAIA_FEnd:
	or      al,al                   ; set Zero flag if APM error
	stc
	jmp     short CAIA_End
CAIA_APM_Err:
	xor     al,al                   ; APM is there but connect or some
	jmp     short CAIA_FEnd         ; other error => CY + Z

Check_and_Init_APM      endp

ENDIF   ; for INCL_APM conditional
; M001 END


Bios_Code       ends



;********************** INITIALIZE CODE ******************

Sysinitseg      segment
	assume  cs:Sysinitseg, ds:nothing, es:nothing

; AX.ReturnStatus = DEV_INIT( ES:BX.DRIVER_INIT_REQUEST)

	public  Power_Init
IFDEF   POWERALONE
Power_Init proc far
ELSE
Power_init proc near
ENDIF

	assume  ds:nothing, es:nothing
	pushf	
	sti
	push    si
	push    ds
	push    dx
	push    ax

	mov     ax, Bios_Data
	mov     ds,ax
	assume  ds:Bios_Data

; JAH Start - Get address of DOS data area
	push	bx
	push	es
	mov	ah,52h			; Get the DOS data segment
	int	21h
	mov	WORD PTR PSPsegment,es
	pop	es
	pop	bx
; JAH End

IFDEF   POWERALONE
	; Return DEVICE DRIVER end address
	mov     word ptr es:[bx].RH_D_BREAKPTR+0,offset Bios_Data:End_Data
	mov     word ptr es:[bx].RH_D_BREAKPTR+2,ds

	; Return DO_IDLE address to DOS
	mov     ax,Bios_Res                     ; get resident code segment
	mov     word ptr es:[bx].RH_D_IDLEPTR+0,offset Bios_Code:DO_IDLE_DOS
	mov     word ptr es:[bx].RH_D_IDLEPTR+2,ax

; M092 BEGIN
; see if POWER is already loaded ; If so, disable the previous POWER
; and load ourselves.
	
	xor     bx,bx
	mov     ax,(MultPWR_API*256)+00h; POWER detect mult.call 
	int     2fh
	cmp     ax,(MultPWR_API*256)+00h; MultAPI code unchanged ? 
	jz      pi_NoPrevPower
	cmp     bx,504dh                ; M087 signature correct ?
	jne     pi_NoPrevPower

; There is a previously loaded POWER; disable it.
	mov     bx,100h                 ; disable all Power management
	mov     ax,(MultPWR_API*256)+I2F_PW_GET_SET_PWSTATE
	int     2fh                     ; we don't expect any error here
;M092 END

ENDIF

; Continue init.
pi_NoPrevPower:
	mov     dx,Bios_Res             ;code segment of the BIOS
	mov     Pwr_calli16_ret+2,dx            ;init CS for this pointer
	mov     Pwr_calli2f_ret+2,dx            ;init CS for this pointer
	mov     kb_calli16_ret+2,dx            ;init CS for this pointer
	mov	I08_IdleControl_ptr+2,dx

	mov     si,offset I28_VEC               ; chain 28h
	mov     ax,28h                          ; DOS IDLE interrupt
	call    XVECT

	mov     si,offset I2F_VEC               ; chain 2Fh
	mov     ax,2Fh                          ; APP IDLE interrupt
	call    XVECT

	mov     si,offset I2A_VEC               ; chain 2Fh
	mov     ax,2Ah                          ; APP IDLE interrupt
	call    XVECT

	mov     si,offset I16_VEC               ; chain 16h
	mov     ax,16h                          ; keyboard intrrupt
	call    XVECT

	mov     si,offset I9_VEC                ; chain 8h
	mov     ax,9h                           ; PC TIMER interrupt
	call    XVECT

	mov     si,offset I10_VEC               ; chain 10h
	mov     ax,10h                          ; application activity
	call    XVECT

	mov     si,offset I13_VEC               ; chain 13h
	mov     ax,13h                          ; application activity
	call    XVECT

	mov     si,offset I14_VEC               ; chain 14h
	mov     ax,14h                          ; application activity
	call    XVECT

	mov     si,offset I17_VEC               ; chain 17h
	mov     ax,17h                          ; application activity
	call    XVECT

	mov     si,offset I21_VEC               ; chain 21h
	mov     ax,21h                          ; application activity
	call    XVECT

	mov     si,offset I25_VEC               ; chain 25h
	mov     ax,25h                          ; application activity
	call    XVECT

	mov     si,offset I26_VEC               ; chain 26h
	mov     ax,26h                          ; application activity
	call    XVECT

	mov     si,offset I6C_VEC               ; chain 6Ch             ;M080
	mov     ax,6Ch                          ; resume event          ;M080
	call    XVECT                                                   ;M080

; NSM4 - BEGIN get address of INDOS flag and store it for future use
	push	es
	mov	ah,34h				; get address of indos flag
	int	21h				; es:bx = address of INDOS
	mov	[INDOS_Offset],bx
	mov	[INDOS_Seg],es			; store them away for later use
	pop	es
; NSM4 - END

	mov     dx,ds				; I08 vec is in our data seg
	mov     si,offset I08_VEC               ; chain Int 08
	mov     ax,08h                          ; 
	call    XVECT

IFNDEF	POWERALONE
	push    ds      
	mov     ax,cs                           ; DS=CS for the message
	mov     ds,ax
	assume  ds:nothing
	mov     dx,offset INIT_MSG
	mov     ah,9                            ; report init complete
	int     21h
	pop     ds                              ; ds = bios_data
ENDIF ; NOT POWERALONE

	assume  ds:Bios_Data

;M001 BEGIN
;
IFDEF  INCL_APM

; Check and initialize APM.

;M005 BEGIN

IFDEF   POWERALONE
	call    Check_and_Init_APM
	call	InitCmdLineParms		; M104
ELSE
	call    Check_and_Init_APM_Ptr
ENDIF   ;NOT POWERALONE

;M005 END

ENDIF           ; INCL_APM

; M001 END

	pop     ax
	pop     dx
	pop     ds
	assume  ds:nothing
	pop     si

	popf
	clc
	ret

Power_Init      endp

;INIT_MSG db "MS-DOS Power Monitor Extension Version 1.00",10,13
;         db "    Copyright (C) 1990-1991 Microsoft Corp.",10,13,"$"

INIT_MSG:
include msbio.cl9

; void XVECT( ds:[si].vecp, ax.vec#,dx.Bios_Code_Seg)
; purpose: xchange a vector with vector pointed to by ds:si

Xvect   proc    near
	assume  ds:Bios_Data, es:nothing

	push    es
	push    di

	mov     di,ax
	shl     di,1
	shl     di,1

	xor     ax,ax
	mov     es,ax

	pushf
	cli                             ; disable interrupts
	mov     ax,[si]
	xchg    es:[di],ax
	mov     [si],ax
	mov     ax,dx                   ;dx contains Bios_Code seg
	xchg    es:[di]+2,ax
	mov     [si]+2,ax
	popf

	pop     di
	pop     es
	ret

Xvect   endp

;M074   BEGIN

	public  Clock_Init

Clock_init      proc far

IFDEF   POWERALONE                      ; M081
	call    far ptr P_UpdFromCMOS
ELSE
	call    P_UpdFromCMOS_ptr       ; M074 update our date and time
					; from CMOS RTC
ENDIF

;       Ints 1c and 6c are hooked during main power management init, above.
	
	clc
	ret

Clock_Init      endp

;M074   END

 
;
; M098 - begin
;
IFDEF	POWERALONE

;
;------------------------------------------------------------------------------
;
; UMBInit : Try moving ourself into UMB by Allocating memory in UMB and
;		Loading ourselves into it using 4b03 call (We are in EXE
;		format)
;
;	Input : ES:BX is the pointer to the original Request packet
;		passed in by DOS
;
;	Output:	Carry Clear if a copy of POWER was succesfully initialized
;		in an UMB
;		Carry set if POWER was not initialized in an UMB
;		AX=segment of the UMB copy (if carry clear)		
;
;------------------------------------------------------------------------------
;
UMBInit	proc	far

	assume	ds:bios_data

	call	AreWeInUMB			; Are we already in a UMB ?
	jc	StayLow				; if so stay where we are
	call	GetProgName			; extract the device driver name
						;  with full path
	call	TryGettingIntoUMB		; and try loading the program
						;  into an UMB
	jc	StayLow				; try staying low if we failed
	call	SetupDDStub			; Initialize the device driver
						;  loaded in the UMB
StayLow:
	ret

UMBInit	endp

;
;------------------------------------------------------------------------------
;
; AreWeImUMB : Check whether we are already in a UMB
;		Check if we are devicehighed into an UMB
;		This will also help the second copy of POWER which will
;		be EXECed in the UMB not to re-exec itself again
;		
;	Input :	None
;	Output:	Carry clear if we are not in UMB
;		Carry set if we are already in a UMB
;
;------------------------------------------------------------------------------
;
AreWeInUMB	proc	near

	assume	ds:bios_data

	push	es
	push	bx

	mov	ah, GET_IN_VARS			; get DOS data
	int	21h
	mov	ax, cs

	mov	bx, word ptr es:[UMB_ARENA]	; get first UMB arena
						; Note : UMB_ARENA is initial-
						;	 zed to ffff if no UMBs
						;	 are available
	cmp	bx, ax				; Sets/clears carry flag

	pop	bx
	pop	es
	ret

AreWeInUMB	endp

ProgName	db	129 dup (0)		; Storage for devicer driver
						;  name.
;
;------------------------------------------------------------------------------
;
; GetProgName : Extracts the device driver file name (with full path) from
;		device driver command line
;
;	Input : ES:BX = pointer to the request packet
;	Output:	The device driver name is copied into ProgName
;
;------------------------------------------------------------------------------
;
GetProgName	proc	near

	assume	ds:bios_data

	push	ds
	push	es
	push	si
	push	di
	lds	si, es:[bx].RH_D_IDLEPTR	; command line
	cld
gpn_loop1:					; skip all leading spaces/delims
	lodsb
	call	IsDelim
	jz	gpn_loop1

	dec	si				; get back to the 1st char
	mov	di, offset ProgName		; destination of copy
	push	cs
	pop	es
gpn_loop2:					; copy till the first space/delim
	lodsb
	call	IsDelim
	jz	gpn_copied
	stosb
	jmp	gpn_loop2
gpn_copied:
	pop	di
	pop	si
	pop	es
	pop	ds
	ret

GetProgName	endp

;
;------------------------------------------------------------------------------
;
; IsDelim : Check whether a character is a delimiter or not
;
;	Input : AL = character to be checked
;	Output:	Zero flag set if the character is a delimiter
;		Zero flag clear if the character is not a delimiter
;
;	Note : Takes the same Delimiter character set as SYSINIT
;
;------------------------------------------------------------------------------
;
IsDelim		proc	near

	cmp	al, ' '
	jz	id9
	cmp	al, 0dh				; CR ?
	jz	id9
	cmp	al, 0ah				; LF ?
id9:
	ret

IsDelim		endp

;
; Packet for Load Overlay
;
LoadArea	dw	?			; UMB where POWER is going to be
RelocFactor	dw	?


CallthruPtr	dw	?			; Temporary space to call the
		dw	?			;  new copy of POWER


;
;------------------------------------------------------------------------------
;
; TryGettingImtoUMB : Try to allocate UMB and load ourself into UMB using
;			4b03 call
;
;	Input :	ProgName contains the file name with full path
;	Output:	Carry clear if the prog was loaded succesfully into UMB
;		Carry set if the operation failed
;
;------------------------------------------------------------------------------
;

TryGettingIntoUMB	proc	near

	assume	ds:bios_data

	push	es
	push	ds
	push	bx
	push	dx

	push	cs
	pop	ds

	push	cs
	pop	es

	assume	ds:sysinitseg

;
; Open the file
;
	mov	dx, offset ProgName
	mov	ah, OPEN
	int	21h
	jc	tgiu9
;
; Seek to end of file to get the size of file
;
	mov	bx, ax
	mov	ax, (LSEEK shl 8)+2		; lseek from end of file
	xor	cx, cx
	mov	dx, cx
	int	21h
;
; Close the file
;
	pushf
	push	dx
	push	ax
	mov	ah, close
	int	21h
	pop	ax
	pop	dx
	popf
	jc	tgiu9				; fail if seek failed

	or	dx, dx				; > 64K ?
	jnz	tgiu9f				; we cannot be > 64K
						; lets abort this process
	mov	cl, 4
	add	ax, 15
	shr	ax, cl
	inc	ax				; One for the Device sub-arena
						; M105
;
; Allocate UMB space : We know that during SYSINIT the only memory available
;			thru INT 21 ah=48 is in the UMb space
;
	mov	bx, ax
	mov	ah, ALLOC
	int	21h
	jc	tgiu9
;
; Update all segment values
;
	inc	ax				; skip past Device sub-arena
						; M105
	mov	LoadArea, ax
	mov	RelocFactor, ax
	mov	CallThruPtr+2, ax
;
; Exec the program
;
	mov	dx, offset ProgName
	mov	bx, offset LoadArea
	mov	ax, (EXEC SHL 8) + 03		; Load overlay
	int	21h
	jc	RelMemory			; if EXEC failed remember to
						; release the memory we alloced
;
; M105 - begin
;
	mov	ax, LoadArea
	push	ds
	assume	ds:nothing
	dec	ax
	mov	ds, ax
	mov	byte ptr ds:[devmark_id], devmark_device
IFNDEF	DEBUG
	mov	word ptr ds:[devmark_filename+0], 'OP'
	mov	word ptr ds:[devmark_filename+2], 'EW'
	mov	word ptr ds:[devmark_filename+4], 'R'
	mov	word ptr ds:[devmark_filename+6], 0
ELSE
	mov	word ptr ds:[devmark_filename+0], 'PD'
	mov	word ptr ds:[devmark_filename+2], 'WO'
	mov	word ptr ds:[devmark_filename+4], 'RE'
	mov	word ptr ds:[devmark_filename+6], 0
ENDIF	
	dec	ax
	mov	ds, ax
	mov	word ptr ds:[arena_name+0], 'DS'
	mov	byte ptr ds:[arena_name+2], 0
	inc	ax
	inc	ax
	mov	word ptr ds:[devmark_seg+10h], ax
	pop	ds
	clc
;
; M105 - end
;
tgiu9:
	pop	dx
	pop	bx
	pop	ds
	pop	es
	ret
RelMemory:
	mov	ax, LoadArea			; M105
	dec	ax				; We have a arena subheader M105
						; above us		    M105
	mov	es, ax				; M105
	mov	ah, 49h
	int	21h
tgiu9f:
	stc
	jmp	short tgiu9

TryGettingIntoUMB	endp

;
;------------------------------------------------------------------------------
;
; SetupDDStub : Initialize the driver loaded in UMB and resize the UMB to
;		whatever is needed. For initializing we call the copy of
;		the device driver in UMB thru its Strategy & Interrupt
;		entry points with the same init request packet which we
;		received from sysinit.
;		Also we set the variable BreakAddr in the UMB copy to the
;		break address of the low copy, because when the clock driver
;		initialized it should return the break address of the low
;		memory copy, else sysinit will think that our drivers
;		are trying to use memory from the Low memory to the UMB !
;
;	Input : ES:BX pointer to the request packet paased to us by DOS
;	Output:	Carry set if Device driver did not initialize
;		Carry clear if Device driver succesfully initialized
;		AX=segment of the UMB copy (if carry clear)		
;
;	Note : The Clock driver will get initialized when DOS calls the
;		stub which has been re-directed to the copy in UMB
;
;------------------------------------------------------------------------------
;
SetupDDStub	proc	near

	assume	ds:bios_data

	push	es
	mov	es, LoadArea
	assume	es:nothing
	mov	byte ptr es:[SecondCopy], 0ffh
	pop	es

	mov	ax, offset JDEV_STRATEGY
	mov	CallThruPtr, ax			; call the strategy entry
	call	dword ptr CallThruPtr

	mov	ax, offset JDEV_INTERRUPT
	mov	CallThruPtr, ax			; call the interrupt entry
	call	dword ptr CallThruPtr

	mov	ax, word ptr es:[bx].RH_D_BREAKPTR+0
	add	ax, 15
	mov	cl, 4
	shr	ax, cl
	add	ax, word ptr es:[bx].RH_D_BREAKPTR+2
	sub	ax, LoadArea			; get the new size of the mem block
	jz	sds9f				; if ZERO the device driver failed

	push	es
	push	bx
	mov	bx, ax
	mov	ax, LoadArea
	mov	es, ax
	mov	word ptr es:[BreakAddr], offset bios_code:endofstub
	mov	word ptr es:[BreakAddr+2], bios_code
	dec	ax				; device sub-arena	M105
	mov	es, ax				;			M105
	mov	word ptr es:[devmark_size], bx	; Put in the size of	M105
						; device driver		M105
	inc	bx				; device sub-arena	M105

	mov	ah, SETBLOCK			; resize the memory block
	int	21h
	mov	bx, es				; 			M105
	dec	bx				; point to sub-arena	M105
	mov	es, bx				;			M105
	mov	word ptr es:[arena_owner], 8	; SYSTEM owner		M105
	pop	bx
	pop	es

	mov	ax, LoadArea			; return the segment address
	clc
	ret
sds9f:
	stc
	ret

SetupDDStub	endp

;
; M098 - end
;
;
;------------------------------------------------------------------------------
;
; DoCmdLine : process the commadn line and set the power mode
;
;	Input : ES:BX=request packet
;	Output: Zero flag set if /LOW was not specified
;		Zero flag set if /LOW was specified
;
;------------------------------------------------------------------------------
;
DoCmdLine	proc	far

	push	es
	push	bx
	les	di, es:[bx].RH_D_IDLEPTR	; command line
	cld

dcl_loop1:
	mov	al, es:[di]
	inc	di
	call	IsDelim
	jz	dcl_loop1

dcl_loop2:
	mov	al, es:[di]
	inc	di
	call	IsDelim
	jnz	dcl_loop2
	dec	di
	call	far ptr ParseCmdLine
	or	dl, dl				; M104
	pop	bx				; M104
	pop	es				; M104
	ret

DoCmdLine	endp
;
; M104 - begin
;

InitCmdLineParms	proc	near

	push	ds
	push	dx
	push	bx
	push	ax

	mov	ax, Trans_Data
	mov	ds, ax
	assume	ds:Trans_Data

	cmp	PW_Mode, 0ffh
	je	SPM_SkipMode

	mov	bh,1			; set  power state
	mov	bl, PW_Mode
	mov	ax,(MultPWR_API*256)+I2F_PW_GET_SET_PWSTATE	; M007
	call	far ptr FPwr_Services
;	int	2fh

	mov	ax, PW_Savings_Value
	or	ax, ax
	jz	SPM_SkipMode

	mov	bx,ax
	mov	ax,(MultPWR_API * 256)+I2F_PW_GETSET_SAVINGS
	call	far ptr FPwr_Services
;	int	2fh

SPM_SkipMode:

IFDEF	DEBUG
	cmp	PW_Debug, 0
	jz	SPM_SkipDbg

	mov	ax,(MultPWR_API*256)+02h; change allocation strategy
	mov	bl,80h			; special value for SOUND toggle
	call	far ptr FPwr_Services
;	int	2fh
SPM_SkipDbg:
ENDIF
	pop	ax
	pop	bx
	pop	dx
	pop	ds
	assume	ds:nothing
	ret
	
InitCmdLineParms	endp

	public	Banner

Banner	proc	far

	push    ds
	mov	ax, Bios_Code
	mov	ds, ax
	assume	ds:nothing
	cmp	byte ptr ds:[SecondCopy], 0
	jne	b_exit
	mov     ax,cs                           ; DS=CS for the message
	mov     ds,ax
	assume  ds:nothing
	mov     dx,offset INIT_MSG
	mov     ah,STD_CON_STRING_OUTPUT        ; display power banner
	int     21h
b_exit:
	pop     ds                              ;
	ret
Banner	endp

ChkDOSVer_DispErr	proc	far
	call	far ptr ChkDOSVer
	jae	cdv_exit
	push	ds
	mov	ah, STD_CON_STRING_OUTPUT
	mov	dx, Trans_Data
	mov	ds, dx
	mov	dx, offset Trans_Data:BadDOSMsg
	int	21h
	pop	ds
	mov	al, 1
	or	al, al
cdv_exit:
	ret
ChkDOSVer_DispErr	endp

ChkDOSVer	proc	far
	push	bx
	mov	ah, GET_VERSION
	int	21h
	cmp	ax, 0005
	pop	bx
	ret
ChkDOSVer	endp

ENDIF ; POWERALONE
Sysinitseg      ends

ENDIF   ;For POWER conditional

	end

