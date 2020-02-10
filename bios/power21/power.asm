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
;**************************************************************

	TITLE   POWER$ IDLE DETECTION V1.0

	.xlist

	include version.inc     ; M004 set build flags

break   macro                   ; satisfy mult.inc
	endm

	include mult.inc        ; get our int 2f function equates       ; M077

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

Pdb_Data segment at 0h			; JAH Dummy segment for accessing the
	org 42h				; JAH active program's PSP
PDB_Idle	label	BYTE		; JAH
Pdb_Data ends				; JAH

; following segment definitions used by transient part of code

ENDIF

	include msequ.inc
	include devsym.inc
	include bpb.inc
	include ioctl.inc
	include power.inc

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

public  RHPTR, KYC, I28, CONTROL, INFO, MSW
public  IDLTIC, TOTTIC, SPDUP, SPDUP_DLY, SPDUP_CNT    ; M004
public  I2F_VEC, I28_TMR0, I28_VEC, KYC_TMR0, KYC_RET, IN_KYC 
public  I16_VEC, SWITCH_CNT, I1C_VEC, I9_COUNT, I9_VEC, I10_VEC, I13_VEC
public  I14_VEC, I17_VEC, I21_VEC,I25_VEC,I26_VEC,I2A_VEC

IFDEF INCL_APM                                  ; M001
public  fAPM_PRESENT,APM_FLAGS,APM_VER,APM_POLL_COUNT,fAPM_CONNECT
public  APM_MAX_POLLCOUNT,fAPM_STATE,APM_RESUME_COUNT
ENDIF

RHPTR   dd 0

; M003 : BEGIN ioctl support removed
; following is indirect entry point into power control module.
; Replacements for Power.exe can plug a different address into
; this space to redirect calls to the Power driver.
;
;EntryPoint      dw      pow_control, Bios_Code
; M003 END

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
KYC     PERIOD_INFO     < 0, 20, 20, 0, 0, 0>   ; default values for base and
I28     PERIOD_INFO     < 0, 100, 20, 0, 0, 0>  ; noise are (random)conservative

CONTROL CONTROL_INFO    < 00101111b, 8*2, 58982, 4,  40, 0,  1,   8,    0>  
                                                                ; M078
;                       idle_flg, sw_dly,thres,ad_dl,mx,sp_dl,ramp,sp_mx,386flg
INFO    IDLE_INFO       < 0, 0, 0, 0, 0, 0, 0>

ErrSampleCount  dw      0

MSW     dw 0            ; store machine status word
			; bit 0 = 1 indicates protected mode

IDLTIC          dw 0    ; M004 used as a flag that says CPU was idle 
			; in the last (I1c) tick period

TOTTIC          dw 0    ; running total of halt time
			; in 1/1.19 MHZ increments 65536=55ms

SPDUP           dw 0    ; Activity indicator 1 active avoid halting
SPDUP_DLY       dw 0    ; MAX DELAY set by activity monitor
SPDUP_CNT       dw 0    ; incremented via INT 1C

PSPsegment		dw	(?)	; JAH - Dos DATA seg  used to	
				; access Current PSP

I2F_VEC dw I2F_IDLE, 0                  ; holds next pointer in chain

I2F_TMR0        dw   0          ;+0, holds temporary PC timer 0
				;    values used to compute PERIOD
		dw   2          ;+2, Counts number of times PC
				;    timer has Overflowed

I2A_VEC dw I2A_IDLE, 0                  ; holds next pointer in chain

I28_TMR0        dw 0            ;+0, holds temporary PC timer 0
				;    values used to compute PERIOD
		dw 2            ;+2, Counts number of times PC
				;    timer has Overflowed

I28_VEC dw I28_IDLE, 0

KYC_TMR0        dw 0            ;+0, holds temporary PC timer 0
				;    values used to compute PERIOD
		dw 2            ;+2, Counts number of times PC
				;    timer has Overflowed

KYC_RET dw 0,0                  ; stores 1 return address

IN_KYC  dw 0                    ; flags re-entry of key board check

I16_VEC dw I16_IDLE, 0          ; stores next key routine in chain

SWITCH_CNT      dw 0                    ; keeps track of when to
					; switch idle detect methods

I1C_VEC dw I1C_TIMR,0

I9_COUNT        dw 2                    ; for every key press more
					; a minimum of 2 INT 9's occur

I9_VEC          dw I9_APP,0

I10_VEC dw I10_APP,0

I13_VEC dw I13_APP,0

I14_VEC dw I14_APP,0

I17_VEC dw I17_APP,0

I21_VEC dw I21_APP,0

I25_VEC dw I25_APP,0

I26_VEC dw I26_APP,0

I6C_VEC dw I6C_RESUME, 0        ;M080

IFDEF   INCL_APM                                ; M001

fAPM_PRESENT    db      0
fAPM_CONNECT    db      0               ; set to 1 if connected
APM_MAX_POLLCOUNT       dw      50
APM_FLAGS       dw      0
APM_VER         dw      0
APM_POLL_COUNT  dw      50
				; counter for APM polling ; counted in int 1c
				; int. time.
fAPM_STATE      db      0       ; 0 if APM disabled and 1 if APM enabled

APM_RESUME_COUNT        dw      0       ; counter for no of resumes from last
				; APM enable

ENDIF                   ;INCL_APM

	PUBLIC  CMOSUpdFlg      ; M090
	PUBLIC  CMOSPollCount   ; M090

CMOSUpdFlg      db      0       ; M090 set to 1 if we need to update from CMOS
CMOSPollCount   dw      MAXCMOSPOLLCOUNT        ; M090
CMOSFlg         db      0       ; M091 bit 0 set to 1 in POWER STD mode on APM 
				; machines

BaseLineRef   dw      0		; counters for counting no of i16s we can get
				; through in about 4 secs  (done at each
				; exec)
BaseLineOvf	dw	0	;
				

IFDEF   POWERALONE              ; needed only for standalone version
	public  daycnt
daycnt  dw      0
	public  daycnt2
daycnt2 dw      0
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

month_table:
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
; 1 byte for each int 21 api from functions 0 thro 57h
; an entry of 0ffh means ignore this api for false idle monitoring
; 0 -> this api means we are  busy
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
;-------------------------------------------------------------------

POWER_STATUS    db      1       ; default ;bit 0 -  S/W pw.mgmt always enabled
				; bit 1 - Take control of APM/FIRMWARE  mgmt 
				; (or get connected to APM)
				; M001
				; M089 If bit1 is set, We are connected to APM
				; but not neccessarily enabled APM
				; (look at fAPM_State)
	
Pwr_i2f_next    dw      Pwr_i2f_lab
		dw      Bios_Data
Pwr_i2f_lab:
	pop     ds
	jmp     dword ptr cs:I2f_Vec

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
	pop     Kyc_Ret
	pop     Kyc_Ret+2

	call    dword ptr I16_Vec
	pushf
	push    Kyc_Ret+2               ; don't clear before
	push    Kyc_Ret         ; return values on stack

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

Pwr_i1c_next    dw      Pwr_i1c_lab
		dw      Bios_Data
Pwr_i1c_lab:
	pop     ds
	jmp     dword ptr cs:I1c_Vec

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

End_Data        label   near

	tocode                  ;Bios_Code segment starts

IFDEF   POWERALONE              ; device header needed here only for
				; standalone version - in MSBIO1 for resident
	assume  ds:nothing

	org 0
DEV_HDR dd CDEV_HDR             ; address of next driver in chain
	dw 8000H                ; character device      ; M003
	dw DEV_STRATEGY         ; DOS entry points to driver
	dw DEV_INTERRUPT        ;
	db 'POWER$  '           ; driver name

CDEV_HDR        dd      -1      ; address of next dev hdr in chain
	dw      8008h           ; CLOCK device (& character)
	dw      DEV_STRATEGY
	dw      CDEV_INTERRUPT
	db      'CLOCK$  '      ; driver name

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

DEV_INTERRUPT proc far
	push    ax
	push    dx
	push    bx
	push    es
	push    ds

	mov     ds,cs:Bios_Data_Word
	assume  ds:Bios_Data
	les     bx,[RHPTR]
	cmp     es:[bx].RH_B_CMD,DEVINIT        ; DEVICE INIT?
	jne     dstot0
	call    far ptr Power_Init
	jmp     short dstex0

; M003 : remove ioctl support
dstot0: 
;       cmp     es:[bx].RH_B_CMD,DEVRDIOCTL     ; DEVICE IOCTL READ?
;       jne     dstxxx
;       call    Power_IOCTL_Read
;       jmp     short dstex0
;
; M003: END

dstxxx: mov     ax,STERR+03H            ; RETURN DEVICE ERROR,
					; ILLEGAL COMMAND
dstex0: les     bx,[RHPTR]
	mov     es:[bx].RH_W_STATUS,ax
	pop     ds
	assume  ds:nothing
	pop     es
	pop     bx
	pop     dx
	pop     ax
	ret
DEV_INTERRUPT endp

Bios_Data_Word  dw      Bios_Data               ; Our data segment

	extrn   tim_read:near
	extrn   tim_writ:near

	extrn   P_UpdFromCMOS:far               ; M081

	public  tim_table

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
	mov     ah,1                    ;  load with normal completion

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
	call    far ptr Clock_Init

IFDEF   POWERALONE                      ; M085
	; Return DEVICE DRIVER end address
	les     bx,[RHPTR]
	mov     word ptr es:[bx].RH_D_BREAKPTR+0,offset Bios_Data:End_Data
	mov     word ptr es:[bx].RH_D_BREAKPTR+2,ds
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
;
; M003: BEGIN ; remove ioctl support
;

IF 0

Power_ioctl_read proc near
	assume  ds:Bios_Data
	push    es
	push    di
	cmp     es:[bx].RH_W_XFERCNT,32 ; get transfer size
	je      dirot0

	mov     es:[bx].RH_W_XFERCNT,0  ; invalid size non-transfered
	jmp     short dirxxx

dirot0: les     di,es:[bx].RH_D_XFERPTR ; address of read block
	mov     word ptr es:[di].PIB_ENTRY_OFFSET,offset EntryPoint 
	mov     word ptr es:[di].PIB_ENTRY_SEG,ds
	mov     byte ptr es:[di].PIB_VERSION,POW_VERSION

dirxxx: pop     di
	pop     es

	clc
	ret

power_ioctl_read endp


PUBLIC  Pow_control
Pow_control proc far
	assume  ds:nothing, es:nothing
; M001 - NSM all these functions except IDLE_INFO have been replaced
; by equivalent INT 2f functions 
	cmp     ah,POW_GET_CONTROL_INFO        ; return CONTROL_INFO pointer
	jnz     pclot0
	mov     dx,cs:Bios_Data_Word
	mov     ax,offset CONTROL
	mov     cx,size CONTROL_INFO
	jmp     short pclex0

pclot0: cmp     ah,POW_GET_STATS               ; return IDLE_INFO pointer
	jne     pclot1
	mov     dx,cs:Bios_Data_Word
	mov     ax,offset INFO
	mov     cx,size IDLE_INFO
	jmp     short pclex0


pclot1: cmp     ah,POW_DO_IDLE                 ; return call DO_IDLE direct
	jne     pclot2

	push    ds
	mov     ds,cs:Bios_Data_Word
	assume  ds:Bios_Data

	call    CheckV86        ; AVOID IN V86
	pop     ds
	assume  ds:nothing
	jnz     pclex0
	push    ds
	mov     ds,cs:Bios_Data_Word
	assume  ds:Bios_Data
	call    DO_IDLE         ; return CY set if idle
	pop     ds
	assume  ds:nothing
	jmp     short pclxxx

; M001 -nsm - commented out
pclot2: cmp     ah,0feh         ; Return POWER_DATA
	jne     pclerr          ; address ES:BX
	mov     es,cs:Bios_Data_Word       ; special function for cstat
	mov     bx,offset POWER_DATA

pclex0: clc                     ; return no error
	ret

pclot2:                         ; falls through to error
pclerr: mov     ax,-1           ; INVALID OPERATION
	stc

pclxxx: ret


Pow_control endp

ENDIF   ; the code above is commented out - M003 END

;******************** INT 2F routine for POWER services *************
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
;                    BL:bit 0 -> T - AUTO SELECT
;                       bit 1 -> T - App Idle (int 2f 1680)
;                       bit 2 -> T - Dos Yield (int 28)
;                       bit 3 -> T - Keyboard idle (int 16)
;                    BL = 0 -> disable Idle detecion
;               EXIT:
;                       BL = current alg if Get_ALG
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
; M001: Created - NSM
;
Pwr_Services    proc    near

	push    ds

	cmp     al,I2F_PW_INSTALL_CHK           ; install check
	je      PS_Install_Chk
;
	cmp     al,I2F_PW_GET_SET_PWSTATE       ; enable/disable PW MGMT
	jne     PS_Chk_SelAlg
	jmp     PS_GetSet_PWState
;
PS_Chk_SelAlg:
	cmp     al,I2F_PW_SELECT_ALG            ; select Idle Detection Alg
	je      PS_Select_Alg
;
	cmp     al,I2f_PW_TUNE_PARM             ; Idle detect. parm. Get/Set
	je      PS_Parm_Tune
;
	cmp     al,I2F_PW_APM_POLLCOUNT         ; Get/Set APM poll count
	je      PS_APM_PollCount
;
	cmp	al,I2F_PW_GETSET_BASELINE
	jne	PS_ChkGetStat
	jmp	PS_GetSet_BaseLine
;
PS_ChkGetStat:
	cmp     al,I2F_PW_GET_STATS             ; Get Idle Detect stats
	je      PS_Get_Stats
;
	mov     ax,ERROR_PM_FUNCTION_NOT_SUPPORTED
PS_FRet:
	stc
	jmp     PS_Ret


PS_Install_Chk:
	mov     bx,504dH                ; "PM"  ;M087
	mov     ah,POW_MAJ_VERSION      ; M084 major version no
	mov     al,POW_MIN_VERSION      ; M084
	clc                             ; M084
	jmp     PS_Ret                  ; M084

;
PS_Select_Alg:
	mov     ds,cs:Bios_Data_Word
	assume  ds:Bios_Data

IFDEF DEBUG
	cmp     bl,80h          ; SOUND toggle
	je      PS_Toggle_Sound
ENDIF
	or      bh,bh           ; get current algorithm
	je      PS_Get_Alg
	test    bl,ALG_RESERVED_BITS
	jnz     PS_Inv_Parm_Err
;
PS_Alg_OK:
	xor     bh,bh
	shl     bx,1                    ; rotate the given alg.bits to the IDLE_FLG format
	shl     bx,1
	and     [CONTROL].IDLE_FLG,NOT ( AUTO_ACTIVE+APP_ACTIVE+DOS_ACTIVE+KYC_ACTIVE)
	or      [CONTROL].IDLE_FLG,bx

; M094 - Fall through to return the current algorithm in BL

PS_Get_Alg:
	mov     bx,[CONTROL].IDLE_FLG   
	and     bx,( AUTO_ACTIVE+APP_ACTIVE+DOS_ACTIVE+KYC_ACTIVE)      ; M075
	shr     bx,1                    ; put this info into the desired 
	shr     bx,1                    ; API output format
	jmp     short PS_SRet

IFDEF DEBUG                     ;Toggle sound only for DEBUG VERSION
PS_Toggle_Sound:
	xor     [CONTROL].IDLE_FLG,SOUND_ACTIVE ; toggle the sound flag
	jmp     short PS_SRet
ENDIF

PS_APM_PollCount:                               ; 7 change APM poll count
	call    Do_APM_GetSet_PollCount
	jmp     short PS_SRet 

	assume  ds:nothing
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
	pop     ds
	ret


PS_Buff_Ovf_Err:
	mov     ax,ERROR_PM_BUFFER_TOO_SMALL
	jmp     PS_FRet

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
	test    POWER_STATUS,1
	jz      PS_Test_APM_Bit         ; already disabled
;
	and     [CONTROL].IDLE_FLG,NOT(IDLE_ACTIVE)     ; turn of idle detection
	and     POWER_STATUS,NOT(1)     ; turn off S/W PM
	jmp     short PS_Test_APM_Bit
PS_Enable_Power:
	test    POWER_STATUS,1
	jnz     PS_Test_APM_Bit         ; already enabled
	or      [CONTROL].IDLE_FLG,IDLE_ACTIVE  ; turn on idle detection
	or      POWER_STATUS,1          ; turn on S/W PM
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

PS_GetSet_BaseLine:
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data
	or	bx,bx			; get base line ?
	je	PS_GetBaseLine
	mov	[BaseLineRef],bx
	mov	[KYC].BASE,bx
;
PS_GetBaseLine:
	mov	bx,[BaseLineRef]
	mov	cx,[BaseLineOvf]
	jmp	PS_SRet

Pwr_Services    endp

;****************************** GetPowStatus **********
; Get the current power status
; Entry: none
; Exit:  AL:Bit 0 - 1 if S/W PowMgmt enabled
;           Bit 1 - 1 if H/W PowMgmt enabled
; Regs: AX
; M089: Created
;******************************************************************
GetPowStatus    proc    near
	pushf
	mov     al,POWER_STATUS
	test    al,2            ; test for H/W enabled state
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
	mov     [APM_MAX_POLLCOUNT],bx
	mov     [APM_POLL_COUNT],bx             ; M093
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
	test    POWER_STATUS,2          ; are we  connected already ?
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
	and     POWER_STATUS,NOT (2)            
	jmp     short DACt_End
DACt_Enable_APM:
	push    bx                      ; save the desired state (input)

;M089 BEGIN
	test    POWER_STATUS,2
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

; User requested APM be ON but we should stay disconnected

	call    Do_APM_DisConnect
	and     POWER_STATUS,NOT (2)            
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

; ax = READ_TMR0( void)
; purpose: to return tic stored in timer0
;
; M0FF:
; OEMs NOTE:
; This procedure should always return a count between 64k and 1
; This code assumes an 8254 timer chip and also assumes that 64k is the
; max count and counted DOWN.
; DOS usually uses mode 3 in both 8253/54. ( mode 2 counts the MAXCOUNT
; just once whereas mode counts it twice in a tic period). But in 8254,
; we can get half cycle we are in, in mode 3 whereas we can't get that
; info in 8253.   
; OEMS, if they don't have an 8254, need to modify this code to always
; return a count as if we are operating in mode 2 (down counter).

PUBLIC Read_tmr0
Read_tmr0 proc near
	assume  ds:nothing, es:nothing

IFDEF 8253
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
ELSE				; 8254 assumed so that we will know the mode
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

IFDEF DEBUG                         ; only use this code on test version

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


; Z.flag CheckV86()
; return - NZ, if 386 V86 or protected  mode


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

;***************** DO_IDLE OEM EXTENDABLE ****************

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

; CY.flag DO_IDLE()
; purpose: To put CPU in a low power state via a halt or
;          OEM supplied OEM_IDLE routine.
; passed:  control.IDLE_FLG - enables/disables DO_IDLE
; return:  Carry Set, if IDLE was executed

PUBLIC Do_idle
Do_idle proc near
	assume  ds:Bios_Data, es:nothing

; JAH Start - If app supports IDLE only idle on int 2f idle entry point below
	push	ds
	mov	ds,PSPsegment		; DS:BX --> Current PSP in Dosdata
	mov	ds,WORD PTR ds:[CUR_PSP_OFFSET]	; DS -> current app's PSP
	assume	ds:Pdb_Data
	cmp	PDB_Idle,IDLE_SUPPORT_BYTE ; Check app's idle support byte
	pop	ds
	assume	ds:Bios_Data

	je	didex1			; Idle aware app so don't idle

	PUBLIC I2fIdleEntry
I2fIdleEntry:				; Entry point from int 2fh handler
; JAH End

	test    [CONTROL].IDLE_FLG,IDLE_ACTIVE
	jnz     didbgn
didex1: clc                             ; idle failed  to execute
	ret

didbgn: push    ax

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

did0ot: test    [CONTROL].IDLE_FLG,MEASURE_ACTIVE
	jz      didhlt
	mov     [IDLTIC],1              ; set IDLE flag ; M004

didhlt:

IFDEF INCL_APM
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
; purpose: called by INT 28 and 16 interrupt handlers
;       This is the common routine which checks elapsed time between
;       interrupts in order to adjust idle signaling.
;
;       Entry : SI = base address of PERIOD_INFO structure for this interrupt
;               DI = address of total delay accumulator for this interrupt
;               BX = address of time accumulator for this interrupt
;               DS = our data segment
;       Exit  : nothing
;       must preserve all registers


public Chk_Delay
Chk_Delay       proc
	assume  ds:Bios_Data, es:nothing
	xor	cx,cx

; entry point for i16 idle checking
; for i16: cx = 1

Chk_i16idle	label	near
;
	push    ax
	cmp     word ptr [bx]+2,1      ; waited too long?
	jb	cd_readtime
	jmp	cd_ClrTmr              ; yes, do a recount

cd_readtime:
	call    READ_TMR0               ; read return time
	neg     ax
	cmp     word ptr [bx]+2,1
	jb      cd_0

	add     ax,0ffffh               ; OVERFLOW=-AX + 65535
	add     [bx],ax                 ; TICS=TICS + OVERFLOW
	jnc	cd_1
	jmp     cd_ClrTmr        	; Go recount on overflow

cd_0:
	add     [bx],ax                 ; TICS=TICS + -ax

cd_1: 	mov     ax,[si].BASE            ; is TIC >= BASE+THRESHOLD?
	add     ax,[CONTROL].THRESHOLD
	jnc     cd_2
	mov     ax,0FFFFh

cd_2:
	cmp     [bx],ax
	jae     cd_go_recount           ; yes, ignore reading   

	cmp     word ptr [si].ADAPT,0   ; are we in adapt cycle ?
	jne     cd_3			; yes go do avg.

; we are in idle cycle. check to see if we are within allowed limites for
; idle
	mov     ax,[si].BASE            ; is TIC <= BASE+NOISE?
	add     ax,[si].NOISE
	cmp     [bx],ax
	jbe     cd_doidle                 ; yes, stay in halt state

; we are above the allowed idle limits; go for adapt cycles

cd_adapt:
	mov     word ptr [si].ADAPT,1   ; start measuring increase
	mov     word ptr [si].DELAY,0

cd_go_recount:
	jmp     short cd_transf_period

; adapt cycle processing
; try calculating avg.
; [bx] = current period
; [si].PERIOD = avg so far
; if the current period is not within 50% of prev. avg we check to see if
; we can ignore this period for avg and go for next one. If we have seen 
; a no of such high(or low) values in the cycle so far, then start adapt
; cycle all over again

cd_3:
	mov     ax,[CONTROL].ADAPT_DLY
	cmp     [si].DELAY,ax           ; is delay over?
	jae	cd_end_adapt		; yes, go cmp our avg with baselineref
	mov	ax,[si].PERIOD
	shr	ax,1			; ax = 1/2 of avg
	shr	ax,1			; ax = .25 of avg
	push	ax

	push	bx
	mov	bx,ax
	add	ax,ax
	add	ax,bx			; ax = 0.75 of avg
	pop	bx

	add	ax,[si].PERIOD		; ax = 1.75 times avg
	cmp	ax,[bx]			; is current period more than 50% 
        pop	ax			; above our prev. period/avg
	jb	cd_throw_sample
	cmp	ax,[bx]			; ax = 1/2 of avg	
	ja	cd_throw_sample		; if below 0.5 (avg) then ignore this
;

	mov	ax,[bx]			; get current period
	add	ax,[si].PERIOD		; add previous period

; BUGBUG - nagara This should never happen ; we should rather ignore 
; this CY so that we get a low avg. this should help us not go idle when
; the period bet. ints are 32k apart
IFDEF DEBUG
	jnc	cd_4
	dbg_printchar 'o'
ENDIF
;	shr	ax,1
;	add	ax,8000h
;	jmp	short cd_5
; END BUGBUG

cd_4: 	shr	ax,1
cd_5:	mov	[bx],ax
	jmp	short cd_transf_period        ; go for next int

cd_end_adapt:
	mov	[ErrSampleCount],0
	mov     word ptr [si].ADAPT,0
	mov     ax,[si].PERIOD             ; for next compare ...
	jcxz	cd_setbase		; for i28 and i2f, no baseline ref.
	cmp	ax,[BaseLineRef]	; for i16, have a max limit for period
	jbe	cd_setbase		; 
	mov	ax,[BaseLineRef]
	mov	[si].BASE,ax		; go up to the max.allowed base
	shr	ax,1			; 50% of base
	mov	[si].NOISE,ax
	shr	ax,1			; 25% of base
	add	[si].NOISE,ax		; noise = 75% of base
	jmp	short cd_transf_period

cd_setbase:
	mov     [si].BASE,ax
	shr	ax,1			; 50% of base
	mov	[si].NOISE,ax
	shr	ax,1			; 25% of base
	add	[si].NOISE,ax		; noise = 75% of base
	
cd_doidle:
	call    DO_IDLE
	adc     word ptr [di],0         ; accumulate idle time
	adc     word ptr [di]+2,0
	mov     word ptr [bx]+2,0       ; avoid speed up error

cd_transf_period:
	mov	ax,[bx]
	mov     [si].PERIOD,ax
cd_ClrTmr:
	call    ClearTimer              ; do speedup if needed, clear 
					; timer accumulator
	pop     ax
	ret

cd_throw_sample:
	inc	[ErrSampleCount]	
	mov	ax,[ErrSampleCount]
	cmp	ax,[CONTROL].MAXERRSAMPLE
	jbe	cd_ClrTmr		; go try adapting again
	mov	[ErrSampleCount],0
	jmp	cd_adapt

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
	cmp     word ptr [bx]+2,3
	jb      ct_0
	call    APP_SPDUP               ; speedup proportional
					; to delay...
ct_0:
	mov     word ptr [bx]+2,0       ; clear key timer
	call    READ_TMR0
	mov     [bx],ax
	cmp     word ptr [bx]+2,0
	jne     ct_0

	ret

ClearTimer         endp


;********************* APPLICATION IDLE CHECK *******************
; purpose: check application idle (INT 2F Function 1680H)
;          if interrupt is detected than DO_IDLE should be called
;          the interrupt should be absorbed.

PUBLIC I2f_idle
I2f_idle proc   far
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds, Bios_Data_Word
	assume  ds:Bios_Data

	cmp     ax,1680h                ; Any app idles?
	jne     i2F_chk_ours            ; check for a service call to us

; JAH Set the idle support byte in the current apps PSP
	push	ds
	mov	ds,PSPsegment		; DS:BX --> Current PSP in Dosdata
	mov	ds,WORD PTR ds:[CUR_PSP_OFFSET] ; DS -> current app's PSP
	assume	ds:Pdb_Data
	mov	PDB_Idle,IDLE_SUPPORT_BYTE ; Set this app's idle support byte
	pop	ds
	assume	ds:Bios_Data
; JAH end 

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

I2f_idle endp

;************************ I28_IDLE *********************

PUBLIC I28_idle
I28_idle proc
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

	inc     [I28].PCOUNT

	test    [CONTROL].IDLE_FLG,DOS_ACTIVE
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
; purpose: check idle (INT 2A Function 84h)
;          if interrupt is detected than DO_IDLE should be called
;          the interrupt should be absorbed.

PUBLIC I2A_idle
I2A_idle proc   far
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds, Bios_Data_Word
	assume  ds:Bios_Data

	cmp     ah,84h                  
	jne     i2Anxt            	; check for a service call to us

; load up registers and call do idle
	push    di
	lea     di,[INFO].SHELL_TOT
	call    DO_IDLE
	adc     word ptr [di],0         ; accumulate idle time
	adc     word ptr [di]+2,0
	pop	di
	pop	ds
	iret

i2Anxt: 
	jmp     dword ptr pwr_i2a_next

I2A_idle endp
;************************ I16_IDLE *********************

PUBLIC I16_IDLE
I16_IDLE proc near
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data


	cmp     [IN_KYC],0
	jne     kybxxx
	cmp     ah,01h
	je      kbpchk
	cmp     ah,11h
	je      kbpchk
	or	ah,ah
	jz	kbChkIdle
	cmp	ah,10h
	jz	kbChkIdle
kybxxx: 
	jmp     dword ptr pwr_i16_next


kbpchk: inc     [KYC].PCOUNT
	test    [CONTROL].IDLE_FLG,KYC_ACTIVE
	jz      kybxxx

	mov     [IN_KYC],1              ; ONLY ALLOW ONE RE-ENTRY

	jmp     dword ptr pwr_call_i16  ;call old int 16h handler
;
;We return to the label below after calling the previous int 16h handler
;
calli16ret:
	mov     ds,Bios_Data_Word               ;reinit DS to Bios_Data
	assume  ds:Bios_Data

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
	mov     [IN_KYC],0              ; can start up again
	pop     ds
	assume  ds:nothing
	iret

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
	call    DO_IDLE
	adc     word ptr [di],0         ; accumulate idle time
	adc     word ptr [di]+2,0
	mov     word ptr [bx]+2,0       ; avoid speed up error
	call    ClearTimer
	pop	di
	pop	bx
	pop	ax
	jmp	short kbChkKey		; go look for key

I16_IDLE endp


;************************ I1C_TIMR *********************

PUBLIC I1c_Timr
I1c_Timr proc
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

	or      [CMOSUpdFlg],al         ; M092 set update flag if rollover

	dec     [CMOSPollCount]         ; M090
	jz      itmSetCMOSFlg           ; M090

itmNoTmUpdate:
	inc     [SPDUP_CNT]             ; speedup delay

	test    [POWER_STATUS],1        ; is idle detection on ?
	jz      i1c_to_end

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
i1c_to_end:
	jz      itmxxx

itmot1: inc     [SWITCH_CNT]            ; time to switch methods?
	mov     ax,[CONTROL].SWITCH_DLY
	cmp     [SWITCH_CNT],ax
	jb      itmxxx                  ; not yet ...

itmot2: mov     [SWITCH_CNT],0
	mov     ax,[I28].PCOUNT         ; If( KYC.COUNT < I28.COUNT)
	cmp     ax,[KYC].PCOUNT         ; occurs in windows ...
	ja      itmdos

		; keyboard count is higher than INT 28 count
		; keyboard is highest count, it wins

	cmp     [KYC].PCOUNT,0          ; avoid thrashing
	je      itmxxx
	mov     ax,KYC_ACTIVE
	jmp     short itmset

itmSetCMOSFlg:
	mov     al,[CMOSFlg]            ; M090 ; need to update from CMOS
	or	[CMOSUpdFlg],al         ; M090 ; only if POWER STD mode in APM 
	mov	ax,MAXCMOSPOLLCOUNT	; machines
	mov	[CMOSPollCount],ax	; start counting again
	jmp     itmNoTmUpdate

itmdos:         ; INT 28 count higher than keyboard count
		; INT 28 count is highest, it wins
	or      ax,ax                   ; don't bother if count is 0
	jz      itmxxx
	mov     ax,DOS_ACTIVE

itmset: and     [CONTROL].IDLE_FLG,NOT ( APP_ACTIVE+DOS_ACTIVE+KYC_ACTIVE)
	or      [CONTROL].IDLE_FLG,ax

itmskp: xor     ax,ax                   ;reset for next round
	mov     [KYC].PCOUNT,ax
	mov     [I28].PCOUNT,ax

itmxxx: 
	test    [POWER_STATUS],2        ; M089 is F/W mgmt on ?
	jz      itmChain                ; M089 No need to poll APM in POWER
					; STD mode
IFDEF INCL_APM
; now check to see if we need to call APM for a PMEVENT 
	cmp     fAPM_STATE,0            ; is APM enabled ?
	je      itmChain
	dec     APM_POLL_COUNT
	jnz     itmChain

; time to poll APM for any PM Event

	push    bx
	mov     ax,[APM_MAX_POLLCOUNT]  ; first reset the counter
	mov     APM_POLL_COUNT,ax
;
itmPoll:
	mov     ax,APM_GETPMEVENT_FUNC
	int     15h
	jc      itm_End                 ; no events -> no work to do 
;
; BX = PM Event
;       1 = stand-by request
;       2 = suspend request
;       3 = normal resume
;       4 = crit. resume
;       5 = battery low
	cmp     bx,APM_NORM_RESUME
	je      itm_Set_time
	cmp     bx,APM_CRIT_RESUME
	jne     itm_broadcast

itm_Set_time:
	INC     APM_RESUME_COUNT
	call    Do_APM_CPUBUSY          ; M002 bring CPU to full speed
	mov     [CMOSUpdFlg],1          ; M090

itm_broadcast:
	mov     ax,I2F_APM_BROADCAST
	int     2fh                     ; broadcast the pm events to other
					; apps
; BUGBUG: Warning: the function codes for APM BIOS GetPMEvent and 
; the multiplex API broadcast functions are assumed to be the same
	cmp     bl,APM_NORM_RESUME      ; was it a stand-by/suspend request
	jae     itm_End                 ; if not, nothing else to do
;
; this was an APM request for system stand-by/suspend
; if no apps/tsrs objected to this request, let us call the APM to suspend/
; stand-by
	or      bh,bh
	jnz     itm_End                 ; somebody objected ? if so quit
	push    cx                      ; M093
	mov     cx,bx                   ; required power mode in CX
	mov     bx,APM_SYSTEM_DEV       ; full system
	mov     ax,APM_SETPWSTATE_FUNC
	int     15h
	pop     cx                      ; M093
IFDEF DEBUG
	jnc     itm_dbg_Success
	dbg_printchar   'f'             ; CY for a set power state call
itm_dbg_Success:
ENDIF                                   ; IFDEF DEBUG
; Resumed from a suspend/stand-by; we should get a resume notification
; from APM so that we can go and update the date and time 
	jmp     itmPoll                 ; go back and poll APM for
					; a resume notification event
;
itm_End:pop     bx
ENDIF
itmChain:
	pop     ax
	jmp     dword ptr pwr_i1c_next

I1C_TIMR endp

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

I13_App         proc
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

	call    APP_SPDUP

	jmp     dword ptr Pwr_i13_next

I13_App         endp

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

I21_App         proc
	assume  ds:nothing, es:nothing

	push    ds
	mov     ds,Bios_Data_Word
	assume  ds:Bios_Data

	push	ax

; upper bound check
	cmp	ah,MAX_I21_ENTRY
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

; right now, there are only two special cases - 4406 and direct console io (6,
; dl= ff) and so now we just check for direct console io call

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
	call	GetNewBaseLine
	jmp	short i21SpdUp
	
I21_App         endp

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

GetNewBaseLine	proc	near

	push	ax
	push	cx
	push	dx
	push	[BaseLineOvf]		; save old value

	mov	[BaseLineRef],0		; init the I16 counters
	mov	[BaseLineOvf],0

	mov     [SPDUP],1               ; indicate activity
					; and stop idles temporarily
	mov     [SPDUP_DLY],5		; measure for 4 ticks
	mov     [SPDUP_CNT],0
	sti
gnb_start:
	cmp	[SPDUP_CNT],1
	jb	gnb_start
gnb_kb_loop:
	mov	ah,1
	int	16h			; get kbd status	
	add	[BaseLineRef],1
	adc	[BaseLineOvf],0
	cmp	[SPDUP_CNT],5
	jb	gnb_kb_loop
; 
	mov	ax,[BaseLineRef]
	mov	dx,[BaseLineOvf]
	shr	dx,1	
	rcr	ax,1		; divide by 2
	shr	dx,1	
	rcr	ax,1		; divide by 4 ; got I16 count for  1 tick
	shr	ax,1		; 50% of BaseLine 
	shr	ax,1		; 25% of BaseLine
	shr	ax,1		; 12.5% of baseline
	mov	dx,ax
	shr	ax,1		; 6.25 % of baseline
	add	ax,dx		; 18.75% of baseline
;	
	mov	cx,ax		; cx = no of I16s per tick (I16_count)
	xor	dx,dx
	mov	ax,0ffffh	; 64k /I16_count = period bet I16s
	div	cx		; ax = period bet I16s; dx remainder (ignored)
	mov	[BaseLineRef],ax; this is the new BaseLine ref value
	pop	[BaseLineOvf]	; get back old baseline value
	pop	dx
	pop	cx
	pop	ax
	ret

GetNewBaseLine	endp
	
	
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
	or      POWER_STATUS,2          ; M003: indicate APM is enabled
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
	mov     kb_calli16_ret+2,dx            ;init CS for this pointer

	mov     si,offset I1C_VEC               ; chain 1Ch
	mov     ax,1Ch                          ; 18.2 second interrupt
	call    XVECT

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

	push    ds      
	mov     ax,cs                           ; DS=CS for the message
	mov     ds,ax
	assume  ds:nothing
	mov     dx,offset INIT_MSG
	mov     ah,9                            ; report init complete
	int     21h
	pop     ds                              ; ds = bios_data
	assume  ds:Bios_Data

;M001 BEGIN
;
IFDEF  INCL_APM

; Check and initialize APM.

;M005 BEGIN

IFDEF   POWERALONE
	call    Check_and_Init_APM
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

Sysinitseg      ends

ENDIF   ;For POWER conditional

	end
