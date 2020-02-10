PAGE 58,132
;******************************************************************************
TITLE WDRMINIT.ASM -- Real Mode Initialization for WDCTRL.386
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1990
;
;   Title:      WDRMINIT.ASM -- Real Mode Initialization for WDCTRL.386
;
;   Version:    1.00
;
;   Date:       17-Oct-1990
;
;   Author:     RAL
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE     REV                 DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   17-Oct-1990 RAL
;   22-Oct-1991 JGT Switched to alternate status register
;                   Allowed Zenith machines to run by allowing track count
;                       to be THREE off (instead of the 'normal' TWO)
;   24-Oct-1991 JGT Allowed one drive to be non-FastDisk compatible and
;                       load.
;   27-Oct-1991 RAL For for hang if MS-NET kernel server running -- Don't
;		    call DOS to get InDOS ptr once incrmented.
;
; N.b., this file is (nearly) duplicated in apps\setup\dos\wdrminit.asm.
; Changes made to this file should also be made to that file.
;
;==============================================================================

	.386p

;******************************************************************************
;                             I N C L U D E S
;******************************************************************************

	.XLIST
	INCLUDE VMM.Inc
	INCLUDE OptTest.Inc
	INCLUDE BlockDev.Inc
	INCLUDE Int13.Inc
	INCLUDE WDLocal.Inc
	INCLUDE Int2FAPI.Inc
	.LIST

;******************************************************************************

SETUP EQU 1

IFDEF SETUP
    EXTRN _WDCtrlHangError:FAR

BeginProc MACRO ProcName
PUBLIC ProcName
ProcName PROC NEAR
	ENDM

EndProc MACRO ProcName
ProcName ENDP
	ENDM


DGROUP GROUP _DATA

ERR_WDCTRL_FATAL        equ 41
ERR_WDCTRL_BAD_SOFTWARE equ 42


FLAT SEGMENT AT 0

FLAT ENDS


;------------------------------------------------------------------------------

FASTDISK SEGMENT WORD PUBLIC USE16 'CODE'

	 ASSUME CS:FASTDISK
	 ASSUME DS:FASTDISK
	 ASSUME ES:FASTDISK
	 ASSUME SS:FASTDISK

ELSE

VxD_REAL_INIT_SEG

EXTRN Invalid_Win386_Ver_String:BYTE
EXTRN Invalid_Int13_Chain:BYTE
EXTRN Invalid_IRQ_String:BYTE
EXTRN Invalid_Controller_String:BYTE
EXTRN Validation_Failed_String:BYTE
EXTRN Invalid_DOS_Ver_String:BYTE
EXTRN WD_Fatal_Error_Code:BYTE
EXTRN WD_Fatal_Error_Msg:BYTE
EXTRN WD_Incompatible_Sw_Msg:BYTE
EXTRN No_Fixed_Disk_String:BYTE
EXTRN Pause_String:BYTE
EXTRN WD_Env_String_Bail:BYTE
EXTRN ES_Debug:BYTE
EXTRN ES_Debug_Len:BYTE
EXTRN ES_Disable:BYTE
EXTRN ES_Disable_Len:BYTE
EXTRN PS_Enable_Wdctrl:BYTE
EXTRN PS_Force_Enable_80:BYTE
EXTRN PS_Force_Enable_81:BYTE
EXTRN PS_Force_Alt_Status0:BYTE
EXTRN PS_Force_Alt_Status1:BYTE

ENDIF

;******************************************************************************


ALIGN 4
InDOS_Ptr LABEL DWORD
InDOS_Off	    dw	    ?
InDOS_Seg	    dw	    ?
DOS_BIOS_Int13_Vec  dd	    0			    ; 0 means not hooked
ROM_BIOS_Int13_Vec  dd	    0
Service_Addr        dd      0
Int13_Buffer        db      512 dup (?)
My_Read_Buffer      db      512 dup (?)

Num_Int13_Drives    db      0
Signaled_Detection  db	    False

QEMMDeviceName	    db	    'QEMM386$',0
QPIEntryPoint	    dd	    ?
QEMM_Stealth_Enabled db     False


;******************************************************************************
;
;   WD_Real_Mode_Init
;
;   DESCRIPTION:
;       This procedure attempts to detect a standard AT type hard disk
;       controller being used for Int 13h.  It will check for two hard
;       disk drives by monitoring changes to the cylinder register after
;       various reads.  Note that this code will use the DOS Int 2Fh, AH=13h
;       to bypass any caching software that may be installed.
;
;       To detect the presence of a Western Digital controller this code
;       will do the following:
;           Get original Int 13h vector from DOS
;           For disks 80h and 81h (if either exists) do
;               Read port 1F7h (Status) and make sure it looks OK
;               Read sector 1, track 0, head 0
;               Temp=Value of port(1F4h)
;               Read sector 1, track 2, head 0
;               If Temp=Value of port(1F4h) then NOT OK
;               If Temp-Value of port(1F4h) > 4 then NOT OK
;
;   ENTRY:
;       CS=DS=ES
;       AX = Win386 version number (requires 3.10)
;	SI = Environment segment
;
;   EXIT:
;       EDX = Flags
;             Bit 0 = 1 if drive 80h is on standard AT controller
;             Bit 1 = 1 if drive 81h is on standard AT controller
;             Bit 2 = 1 if alternate status register should be used
;
;   USES:
;
;==============================================================================

BeginProc WDCtrl_Real_Mode_Init

IFNDEF SETUP
	cmp     ax, 30Ah
	jb      WD_RMI_Invalid_Win386

        ;** Save the loader service address
	mov	[Service_Addr], ecx

	call	Test_Disable_Switch		; Q: User set /D:F option?
	jc	WD_RMI_Dont_Load_Silent 	;    Y: Don't load

        ;** Here we check for two environment strings:
        ;**     WDCTRLDISABLE=Y         ;Disables WDCTRL (displays message)
        ;**     WDCTRLDEBUG=Y           ;Stops debugger at INT 3 in RM Init
	mov	es, si			; Set ES to our environment
        mov     si, OFFSET ES_Disable   ;'WDCTRLDISABLE='
        xor     ch,ch
        mov     cl, ES_Disable_Len
        call    WD_RMI_Get_Env_String   ;Get the value for the string
        or      ax, ax                  ;Bail out?
        jz      SHORT WD_RMI_NoEnvBailOut ;No
        mov     dx, OFFSET WD_Env_String_Bail ;Get message
        jmp     WD_RMI_Print_Error      ;Exit with error

WD_RMI_NoEnvBailOut:
        mov     si, OFFSET ES_Debug     ;'WDCTRLDEBUG='
        xor     ch,ch
        mov     cl, ES_Debug_Len
        call    WD_RMI_Get_Env_String
        or      ax, ax
        jz      SHORT WD_RMI_NoInt3
        int     3                       ;Break to debugger
WD_RMI_NoInt3:
	;** Check for SYSTEM.INI string that must be set to true for
	;** WDCTRL to load.
        mov     ax, 3                   ;Service:  Get_Profile_Boolean
        xor     ecx, ecx                ;Default is FALSE
        xor     si, si                  ;[386Enh] is default section
	mov	di, OFFSET PS_Enable_Wdctrl ;'32BITDISKACCESS'
        call    cs:[Service_Addr]       ;Call loader service
	or	cx, cx
	jz	WD_RMI_Dont_Load_Silent

        ;** We have a SYSTEM.INI switch here to force WDCTRL enabled
        ;**     without validation.  This is useful for drives that fail
        ;**     validation, but work fine with FastDisk.
        mov     ax, 3                   ;Service:  Get_Profile_Boolean
        xor     ecx, ecx                ;Default is FALSE
        xor     si, si                  ;[386Enh] is default section
        mov     di, OFFSET PS_Force_Enable_80 ;'WDCTRLDRIVE0', 0
        call    cs:[Service_Addr]       ;Call loader service
        or      cx, cx
        mov     bl,0                    ;Clear the flag bits
        jz      SHORT WD_RMI_No_Drive_80 ;Drive not forced enabled
        or      bl, RF_Drive_80h_Ours   ;Flag that drive 80h was forced on
WD_RMI_No_Drive_80:
        mov     ax, 3                   ;Service:  Get_Profile_Boolean
        xor     ecx, ecx                ;Default is FALSE
        xor     si, si                  ;[386Enh] is default section
        mov     di, OFFSET PS_Force_Enable_81 ;'WDCTRLDRIVE1', 0
        push    bx                      ;Save flag bits
        call    cs:[Service_Addr]       ;Call loader service
        pop     bx
        or      cx, cx
        jz      SHORT WD_RMI_No_Drive_81 ;Drive not forced enabled
        or      bl, RF_Drive_81h_Ours   ;Flag that drive 80h was forced on
WD_RMI_No_Drive_81:
        test    bl,bl                   ;Any bits set?
        jz      SHORT WD_RMI_No_Force_Enable ;No.  Do normal validation

        ;** Now that we have verified that the user wanted one or two
        ;**     drives forced on, they can also specify if the drive
        ;**     should use the alternate status register.  Default is NO.
        mov     ax, 3                   ;Service:  Get_Profile_Boolean
        xor     ecx, ecx                ;Default is FALSE
        xor     si, si                  ;[386Enh] is default section
	mov	di, OFFSET PS_Force_Alt_Status0 ;'WDCTRLALTSTATUS0', 0
        push    bx                      ;Save flag bits
        call    cs:[Service_Addr]       ;Call loader service
        pop     bx
        or      cx, cx
	jz	SHORT WD_RMI_No_Alt_Status0 ;Drive not forced enabled
	or	bl, RF_Use_Alt_Stat_80	;Flag that user wanted alt status reg
WD_RMI_No_Alt_Status0:

        mov     ax, 3                   ;Service:  Get_Profile_Boolean
        xor     ecx, ecx                ;Default is FALSE
        xor     si, si                  ;[386Enh] is default section
	mov	di, OFFSET PS_Force_Alt_Status1 ;'WDCTRLALTSTATUS1', 0
        push    bx                      ;Save flag bits
        call    cs:[Service_Addr]       ;Call loader service
        pop     bx
        or      cx, cx
	jz	SHORT WD_RMI_No_Alt_Status1 ;Drive not forced enabled
	or	bl, RF_Use_Alt_Stat_81	;Flag that user wanted alt status reg
WD_RMI_No_Alt_Status1:

        jmp     WD_RMI_Have_Ref_Data    ;Force it on as if validation
                                        ;  passed successfully

WD_RMI_No_Force_Enable:

        push    ds                      ;Restore ES
        pop     es

ENDIF

;------------------------------------------------------------------------------
;
;   Now make sure that we have some Int 13h drives by calling Get Drive
;   Parameters for drive 80h.
;
;------------------------------------------------------------------------------

	mov     ah, 08h                         ; Get drive parameters
	mov     dl, 80h                         ; For first drive
	int	13h				; Q: Is there one or more?
	sti					; STI TO WORK AROUND AD-DOS!
IFNDEF SETUP
	jc	WD_RMI_No_Disk_Drives		;    N: Pretty pointless
ELSE
	jc	WD_RMI_Error_Exit		;    N: Pretty pointless
ENDIF
						;    Y: Do some funky tests
	mov     [Num_Int13_Drives], dl          ; Save this for later
;------------------------------------------------------------------------------
;
;   Now do a check for the COMPAQ dual WDCTRL configuration. COMPAQ machines
;   have the ability to support two WDCTRLs at different I/O ports but which
;   share the same IRQ (12). This would work, WDCTRL would handle one, and
;   COMPAQ driver (EXTDISK.SYS) the other, except for the fact that the IRQ
;   is shared. We don't have code to handle the IRQ sharing so we'll just
;   punt this config and not install.
;
;------------------------------------------------------------------------------
    ;
    ; First see if this is a COMPAQ machine. This is the same detection
    ;	code that is in VFD
    ;
	push	es
	mov	ax, 0F000h
	mov	es, ax
	cmp	DWORD PTR es:[0FFE8h], 'OC30'
	clc
	jnz	SHORT CompDoneP 	; Not COMPAQ
	cmp	DWORD PTR es:[0FFECh], 'QAPM'
	clc
	jnz	SHORT CompDoneP 	; Not COMPAQ
    ;
    ; Is a COMPAQ machine, check for DUAL controler config by reading CMOS
    ;
	mov	ax, 1bh			; Secondary controller drive 1
	mov	dx, 70h			; CMOS Index I/O port 
	out	dx, al
	inc	dx			; CMOS data I/O port 
	IO_Delay
	IO_Delay
	in	al, dx			; Get drive type
	or	al, al			; Q: Do we have a drive (clears carry)?
	jz	short CompDoneP 	;  N: jump with carry clear
	dec	dx
	mov	al, 0eh			; Check if CMOS is OK
	IO_Delay
	IO_Delay
	out	dx, al
	inc	dx
	IO_Delay
	IO_Delay
	in	al, dx
	test	al, 60h 		; Q: Is CMOS Valid (clears carry)?
					;    Checking CMOS Bad Checksum and
					;	      CMOS Config Invalid
	jnz	short CompDoneP 	;  N: Error, jump with carry clear
	xor	ecx,ecx
	dec	ecx			; Report validation phase 0FF,0FF
	stc				; SET CARRY, dual controler config
CompDoneP:
	pop	es
IFNDEF SETUP
	jc	WD_RMI_Validation_Failed	; Don't instl COMPAQ dual CTRL
ELSE
	jc	WD_RMI_Error_Exit		; Don't instl COMPAQ dual CTRL
ENDIF

;------------------------------------------------------------------------------
;
;   Check for QEMM stealth.  WDCTRL will load if stealth is enabled even though
;   the Int 13h hook points to a "bad" location (QEMM hooks it).
;
;------------------------------------------------------------------------------

	call	Detect_QEMM_Stealth

;------------------------------------------------------------------------------
;
;   Now do lots of party stuff to make sure cache programs flush or at least
;   don't try to lazy-write any data while we do this test.  To make sure
;   of this we will do the following:
;	Broadcast the BlockDev hardware detection API Int 2Fh
;	Do an Int 13h read of sector 0 on both drives
;	Do a DOS disk reset on drives C-Z (may flush)
;	Set the InDOS flag
;
;------------------------------------------------------------------------------
;
;   Broadcast an Int 2Fh that tells cache programs that disk hardware detection
;   is about to begin.	This broadcast can be intercepted by new cache
;   programs to prevent unnecessary flushes and to prevent conflicts.
;
;------------------------------------------------------------------------------

	mov	ax, (W386_Int_Multiplex SHL 8) + W386_Device_Broadcast
	mov	bx, BlockDev_Device_ID
	mov	cx, BlockDev_API_Hw_Detect_Start
	int	2Fh

;------------------------------------------------------------------------------
;
;   Some cache programs punt the entire cache when someone that is not DOS
;   does an Int 13h.  Hopefully this will happen when we do this read.
;
;------------------------------------------------------------------------------

	mov	ax, 0201h			; Read 1 sector
	mov	bx, OFFSET Int13_Buffer 	; Into this buffer (ES:BX)
	mov	cx, 0001h			; Track 0, sector 1
	mov	dx, 0080h			; Head 0, drive 80h
	int	13h				; Do it!
	sti					; STI TO WORK AROUND AD-DOS!

	cmp	[Num_Int13_Drives], 1		; Q: More than one drive?
	je	SHORT WD_RMI_Reset_Drives	;    N: Done

	mov	ax, 0201h			; Read 1 sector
	mov	bx, OFFSET Int13_Buffer 	; Into this buffer (ES:BX)
	mov	cx, 0001h			; Track 0, sector 1
	mov	dx, 0081h			; Head 0, drive 81h
	int	13h				; Do it!
	sti					; STI TO WORK AROUND AD-DOS!


;------------------------------------------------------------------------------
;
;   Do a disk reset on every drive C-Z so that cache programs will flush.
;   PC Super Kwik will pay attention to this DOS call and flush lazy writes.
;
;------------------------------------------------------------------------------

WD_RMI_Reset_Drives:
	mov	ah, 19h
	int	21h
	push	ax

	mov	dl, 2				; Start with drive C:

WD_RMI_Flush_Loop:
	mov	ah, 0Eh 			; Select the drive
	int	21h				; (AL contains max drive)

	mov	ah, 0Dh 			; Flush the drive
	int	21h

	inc	dl				; DL = Next drive to flush
	cmp	dl, al				; Q: Any more drives to flush?
	jb	SHORT WD_RMI_Flush_Loop 	;    Y: Keep looking

	pop	dx				; DL = Original default drive
	mov	ah, 0Eh 			; Select drive
	int	21h

;------------------------------------------------------------------------------
;
;   TSRs should not pop up and do DOS calls when the InDOS flag is set.
;   We will leave the flag non-zero throughout the entire test.
;
;------------------------------------------------------------------------------

WD_RMI_Set_InDOS:
	push	es

	mov	ah, 34h
	int	21h				; ES:[BX] -> InDOS flag
	inc	BYTE PTR es:[bx]
	mov	[InDOS_Seg], es
	mov	[InDOS_Off], bx

	pop	es

	mov	[Signaled_Detection], True	; Set flag to indicate that
						; we need to dec InDOS and
						; do end detection Int 2Fh call

;------------------------------------------------------------------------------
;
;   Now wait for 2 clock ticks to make sure all disk activity that may
;   have slipped in before we bumped InDOS will complete.
;
;------------------------------------------------------------------------------

	sti					; Paranoia in case some random
						; software int turned off ints
	push	es
	mov	ax, 40h
	mov	es, ax
	mov	ah, 2
WD_RMI_Set_Init_Time_Loop:
	mov	al, es:[6Ch]
WD_RMI_Wait_For_Tick_Loop:
	cmp	al, es:[6Ch]
	je	WD_RMI_Wait_For_Tick_Loop
	dec	ah
	jnz	WD_RMI_Set_Init_Time_Loop
	pop	es

;------------------------------------------------------------------------------
;
;   Now make sure that the Int 2Fh that we need will actually work.  We'll
;   do this by pointing DOS at somthing bogus for the time being.
;
;------------------------------------------------------------------------------

	mov     ah, 13h
	mov     bx, OFFSET WD_RMI_Int_13h_Hook
	mov     dx, OFFSET WD_RMI_Int_13h_Hook
	int     2Fh

	mov     si, ds
	mov     di, cs
	cmp     si, di
	je	WD_RMI_Cant_Hook_Int13

	mov	WORD PTR cs:[DOS_BIOS_Int13_Vec+2], ds
	mov	WORD PTR cs:[DOS_BIOS_Int13_Vec], dx

	mov	WORD PTR cs:[ROM_BIOS_Int13_Vec+2], es
	mov	WORD PTR cs:[ROM_BIOS_Int13_Vec], bx

	mov     ax, cs
	mov     es, ax
	mov     ds, ax

	call	Check_Hook_Addr_OK
IFNDEF SETUP
	jc	SHORT WD_RMI_Bad_Int_Chain
ELSE
	jc	short WD_RMI_Error_Exit
ENDIF


;------------------------------------------------------------------------------
;
;   It is possible for Int 13h to be hooked, so there may be some point
;   in loading this device.
;
;------------------------------------------------------------------------------

;
;   Make sure the interrupt we plan to use is unmasked.
;
	in      al, 0A1h
	test    al, 01000000b
IFNDEF SETUP
	jnz	SHORT WD_RMI_Wrong_IRQ
ELSE
	jnz     SHORT WD_RMI_Error_Exit
ENDIF


;
;   Check the status I/O port to make sure it looks right.
;
	mov	dx, WDIO_Def_Base_Port+WDIO_Status_Off
	IO_Delay
	IO_Delay
	in      al, dx
	and	al, NOT (WDStat_ECC_Corrected OR WDStat_Index)
	cmp     al, WDStat_Ready OR WDStat_Seek_Complete
IFNDEF SETUP
	jne	SHORT WD_RMI_Status_Bad
ELSE
	jne	SHORT WD_RMI_Error_Exit
ENDIF


;------------------------------------------------------------------------------
;
;   Now read a sector from each hard disk first through Int 13h and
;   then by sending a command directly to the controller.
;
;   We test both drives if present and set the appropriate bit for the
;   drive.  WDCTRL can use ony the drive that validates, even if there
;   are two drives and one doesn't validate
;
;------------------------------------------------------------------------------

	mov     dl, 80h
	mov     di, 41h*4
	call	WDCtrl_Validate_Drive
	pushf
	and	bx, RF_Use_Alt_Stat_80	; Save only alt status bit
	popf				; Restore carry flag
        jc      SHORT WD_RMI_First_Drive_Bogus
        or      bl, RF_Drive_80h_Ours   ; Alt status bit + good drive bit

WD_RMI_First_Drive_Bogus:
	cmp     [Num_Int13_Drives], 1
	je      SHORT WD_RMI_Do_We_Bail_Out

	mov     dl, 81h
	mov     di, 46h*4
        push    bx                      ; Save reference data
	call	WDCtrl_Validate_Drive
	mov	si, bx			; Save alt stat return from call
	pop	bx			; Get previous bits
	jc	SHORT WD_RMI_Do_We_Bail_Out
	and	si, RF_Use_Alt_Stat_81
	or	bx, si
        or      bl, RF_Drive_81h_Ours   ; Alt status bit + good drive bit

WD_RMI_Do_We_Bail_Out:
        test    bl, RF_Drive_80h_Ours OR RF_Drive_81h_Ours ; Either drive good?

IFNDEF SETUP
        jz      SHORT WD_RMI_Validation_Failed ; No, both bad so exit
ELSE
        jz      SHORT WD_RMI_Error_Exit
ENDIF

WD_RMI_Have_Ref_Data:
        movzx   edx, bl                  ; Get the reference data
	xor     bx, bx
	xor     si, si
IFNDEF SETUP
	mov     ax, Device_Load_Ok
ENDIF

;------------------------------------------------------------------------------
;
;   Common exit point for WD real mode init.  At this point, all return values
;   are in AX, BX, and SI.  If Int 13h is hooked then this code will reset
;   the hook.
;
;------------------------------------------------------------------------------

WD_RMI_Common_Exit:
	call	Clean_Up_Hooks
	ret

IFDEF SETUP

WD_RMI_Error_Exit:
	 mov   edx, 0
	 jmp   short WD_RMI_Common_Exit

ENDIF

;------------------------------------------------------------------------------
;
;   Error handlers
;
;------------------------------------------------------------------------------


IFNDEF SETUP
;
;   No disk drives were installed
;
WD_RMI_No_Disk_Drives:
	mov     dx, OFFSET No_Fixed_Disk_String
	jmp     SHORT WD_RMI_Print_Error
ENDIF

IFNDEF SETUP
;
;   Not running on Win386 verstion 3.10 or later
;
WD_RMI_Invalid_Win386:
	mov     dx, OFFSET Invalid_Win386_Ver_String
	jmp     SHORT WD_RMI_Print_Error
ENDIF

;
;   The DOS Int 2Fh did not work.  Assume wrong DOS version.
;
WD_RMI_Cant_Hook_Int13:
	mov     dx, cs
	mov     ds, dx
	mov     es, dx
IFNDEF SETUP
	mov     dx, OFFSET Invalid_DOS_Ver_String
	jmp     SHORT WD_RMI_Print_Error
ELSE
	jmp   SHORT WD_RMI_Error_Exit
ENDIF

IFNDEF SETUP
;
;   The Int 13h chain was hooked, but it appears that someone else has
;   hooked it in front of us.
;
WD_RMI_Bad_Int_Chain:
	mov     dx, OFFSET Invalid_Int13_Chain
	jmp     SHORT WD_RMI_Print_Error

;
;   The controller is not using the correct IRQ or the IRQ is masked.
;
WD_RMI_Wrong_IRQ:
	mov	dx, OFFSET Invalid_IRQ_String
	jmp     SHORT WD_RMI_Print_Error


;
;   The initial read of the controller status port did not match our
;   expectation.
;
WD_RMI_Status_Bad:
	mov	cx, ax
	call	Clean_Up_Hooks

	mov     dx, OFFSET Invalid_Controller_String
	mov     ah, 9
	int	21h

	mov	al, cl
	call	Print_Hex_Byte
	jmp	SHORT WD_RMI_Dont_Load


;
;   The controller did not pass our tests.
;
WD_RMI_Validation_Failed:
	call	Clean_Up_Hooks

	mov	dx, OFFSET Validation_Failed_String
	mov     ah, 9
	int	21h

	mov	al, cl
	call	Print_Hex_Byte

	mov	ah, 02h
	mov	dl, ','
	int	21h
	mov	dl, ' '
	int	21h

	mov	al, ch
	call	Print_Hex_Byte

	jmp	SHORT WD_RMI_Dont_Load


WD_RMI_Print_Error:
	call	Clean_Up_Hooks
	mov     ah, 9
	int	21h

WD_RMI_Dont_Load:
	mov	dx, OFFSET Pause_String
	mov	ah, 9
	int	21h

	xor	ax, ax
	int	16h

WD_RMI_Dont_Load_Silent:
	xor     bx, bx
	xor     si, si
	mov     ax, Abort_Device_Load + No_Fail_Message
	jmp     WD_RMI_Common_Exit
ENDIF

EndProc WDCtrl_Real_Mode_Init


;******************************************************************************
;
;   Detect_QEMM_Stealth
;
;   DESCRIPTION:
;	Specific check for QEMM stealth.  We'll call QEMM if it is installed
;	to find out if stealth is on.  This is done before entering the main
;	part of the detection code since we need to make DOS calls to open
;	the device driver.
;
;   ENTRY:
;	DS=ES=CS
;
;   EXIT:
;	None
;
;   USES:
;	Flags
;
;==============================================================================

BeginProc Detect_QEMM_Stealth

	pusha
;
;   Open QEMM device driver
;
	mov	dx, OFFSET QEMMDeviceName
	mov	ax, 3D00h
	int	21h				; Try to open QEMM386$
	jc	SHORT DQS_No_QEMM		; If CY, QEMM-386 not present
	mov	bx, ax				; Save file handle in BX
	mov	dx, OFFSET QPIEntryPoint	; Store the entry point here
	mov	cx, 4				; We're reading 4 bytes
	mov	ax, 4402h			; IOCTL read control string
	int	21h
	pushf					; Save error code
	mov	ah, 3Eh 			; Close the file handle
	int	21h
	popf
	jc	SHORT DQS_No_QEMM

;
;   At this point QPIEntryPoint contains the address of the QEMM entry
;   point.  We KNOW that we're on version 6.0 or later (since the IOCTL worked)
;   so we'll make the stealth install call.
;
	xor	cx, cx				; Set CL to zero just in case...
	mov	ax, 30*100h+0			; QEMM get info call
	push	ds				; More paranoia...
	push	es
	call	[QPIEntryPoint] 		; Call Mr. QEMM
	pop	es
	pop	ds
	test	cl, cl				; Q: Is stealth enabled?
	jz	SHORT DQS_No_Stealth		;    N: Don't set falg
						;    Y: Set flag for use later
	mov	[QEMM_Stealth_Enabled], True

DQS_No_Stealth:
DQS_No_QEMM:
	popa
	ret

EndProc Detect_QEMM_Stealth


;******************************************************************************
;
;   Check_Hook_Addr_OK
;
;   DESCRIPTION:
;	This procedure checks the BIOS Int 13h interrupt vector value to
;	see if it is "acceptable".  The address is considered acceptable
;	iff the segment is >= A000h or QEMM 6.0 or greater is installed
;	and stealth is enabled or the hook code responds to our Int 2Fh.
;
;   ENTRY:
;	DS=ES=CS
;
;   EXIT:
;	If carry set then
;	    Hook is NOT acceptable
;	else (carry clear)
;	    Hook address looks good
;
;   USES:
;	Flags
;
;==============================================================================

BeginProc Check_Hook_Addr_OK

	cmp	WORD PTR [ROM_BIOS_Int13_Vec+2], 0A000h
	jae	SHORT CHAO_Quick_OK_Exit

	pusha


;------------------------------------------------------------------------------
;
;   Broadcast an Int 2Fh that asks DOS Int 13h hookers if they are "blockdev
;   aware".  If they are aware of BlockDev and want fastdisk drivers to load
;   then they will return 0 in CX.
;
;------------------------------------------------------------------------------

	mov	ax, (W386_Int_Multiplex SHL 8) + W386_Device_Broadcast
	mov	bx, BlockDev_Device_ID
	.ERRE	BlockDev_API_Int13_Chain_Check	 ; Should not be 0!
	mov	cx, BlockDev_API_Int13_Chain_Check
	int	2Fh
	jcxz	SHORT CHAO_Chain_Is_OK

;------------------------------------------------------------------------------
;
;   Specific check for QEMM stealth.  We'll call QEMM if it is installed
;   to find out if stealth is on.  If it is then we'll load anyway.
;
;------------------------------------------------------------------------------

;
;   Now make sure that both hooks point to the same code segment as a sanity
;   check.
;
	mov	ax, WORD PTR [ROM_BIOS_Int13_Vec+2]
	cmp	ax, WORD PTR [DOS_BIOS_Int13_Vec+2]
	jne	SHORT CHAO_QEMM_Seg_Check_Failed

	cmp	[QEMM_Stealth_Enabled], True	; Q: Stealth on?
	jne	SHORT CHAO_QEMM_Seg_Check_Failed;    N: Not gonna do it!

;
;   All success cases exit here
;
CHAO_Chain_Is_OK:
	popa
CHAO_Quick_OK_Exit:
	clc
	ret

;
;   All failure cases exit here
;
CHAO_QEMM_Seg_Check_Failed:
CHAO_No_Stealth:
CHAO_No_QEMM:
	popa
	stc
	ret

EndProc Check_Hook_Addr_OK


IFNDEF SETUP


;******************************************************************************
;
;   Test_Disable_Switch
;
;   DESCRIPTION:
;	This procedure scans the parameters passed to Win386 for /d:F which
;	is a debug switch to disable "FastDisk".  It if is found then
;	WDCTRL will not load.
;
;   ENTRY:
;	None
;
;   EXIT:
;	If carry flag set then /D:F selected.  WDCTRL should not load.
;
;   USES:
;	Flags
;
;==============================================================================

BeginProc Test_Disable_Switch

	push	ds
	pusha

	mov	ah, 62h 			; Get current PSP
	int	21h
	mov	ds, bx				; ES -> Current PSP

    ;
    ; Get command-line debug option
    ;
    ; There are several forms for this:
    ;
    ;	  /d:s foo /d:p
    ;
    ; In this case, the first /d: is ours, the second isn't, the second
    ; is an argument to the app "foo". We detect the boundary (in this case
    ; the "foo") by detecting a character which is not a tab, space or '/'
    ; which is not part of a '/' arg. Thus:
    ;
    ;	  foo	 is a boundary
    ;	  /goooo is not a boundary
    ;	  /d:&	 is not a boundary and will be skipped, valid debug options
    ;		   are either 'A'-'Z' or 'a'-'z'
    ;
    ;
    ;	  /d:svf
    ;	  /d:s /d:v /d:f
    ;
    ; These two are the same, both s and v debug options are specified.
    ; we allow both forms so that users don't get confused and can do multiple
    ; debug options either way.
    ;
    ; This loop exits immeidatly if /d:f is found.  Wdctrl will be disabled.
    ;

	movzx	cx, BYTE PTR ds:[80h]
	jcxz	WD_RMI_TSD_No_Debug_Opt

	mov	si, 81h
	cld
WD_RMI_TSD_Cont_Search:
	lodsb
WD_RMI_TSD_Cont_Search2:
	cmp	al,'/'
	jne	short WD_RMI_TSD_ChkBnd
WD_RMI_TSD_NextSw:
	dec	cx
	jz	SHORT WD_RMI_TSD_No_Debug_Opt
	lodsb
	or	al,20h			; force to lower case
	cmp	al,'d'			; /d ?
	jne	SHORT WD_RMI_TSD_UnkSwitch	; No, unknown switch
	dec	cx
	jz	SHORT WD_RMI_TSD_No_Debug_Opt
	lodsb
	cmp	al,':'			; /d: ?
	jne	SHORT WD_RMI_TSD_UnkSwitch	; No, unknown switch
	dec	cx
	jz	SHORT WD_RMI_TSD_No_Debug_Opt
	lodsb
WD_RMI_TSD_ContDebOpt:
	cmp	al,'A'			; /d: something that is a letter?
	jb	short WD_RMI_TSD_UnkSwitch	; No, unknown switch
	cmp	al,'Z'
	jbe	short WD_RMI_TSD_IsUpDebOpt	; Yes
	cmp	al,'a'
	jb	short WD_RMI_TSD_UnkSwitch	; No, unknown switch
	cmp	al,'z'
	ja	short WD_RMI_TSD_UnkSwitch	; No, unknown switch
WD_RMI_TSD_IsUpDebOpt:
	or	al,20h			; force to lower case
	cmp	al, "f"
	je	SHORT WD_RMI_TSD_Dont_Load
	dec	cx
	jz	SHORT WD_RMI_TSD_No_Debug_Opt
	lodsb
	cmp	al,20h			; End of this debug opt?
	je	short WD_RMI_TSD_LoopIt 	; Yes
	cmp	al,09h			; End of this debug opt?
	je	short WD_RMI_TSD_LoopIt 	; Yes
	cmp	al,'/'			; End of this debug opt?
	je	short WD_RMI_TSD_NextSw 	; Yes
	jmp	short WD_RMI_TSD_ContDebOpt

WD_RMI_TSD_UnkSwitch:
	dec	cx
	jz	SHORT WD_RMI_TSD_No_Debug_Opt
WD_RMI_TSD_UnkSwitchCnt:
	lodsb
	cmp	al,20h
	je	short WD_RMI_TSD_LoopIt
	cmp	al,09h
	je	short WD_RMI_TSD_LoopIt
	cmp	al,'/'
	je	short WD_RMI_TSD_NextSw
	loop	WD_RMI_TSD_UnkSwitchCnt
	jmp	SHORT WD_RMI_TSD_No_Debug_Opt

WD_RMI_TSD_ChkBnd:
	cmp	al,' '
	je	short WD_RMI_TSD_LoopIt
	cmp	al,09h
	jne	short WD_RMI_TSD_No_Debug_Opt
WD_RMI_TSD_LoopIt:
	loop	WD_RMI_TSD_Cont_Search

WD_RMI_TSD_No_Debug_Opt:
	clc
WD_RMI_TSD_Pop_Exit:
	popa
	pop	ds
	ret

WD_RMI_TSD_Dont_Load:
	stc
	jmp	WD_RMI_TSD_Pop_Exit

EndProc Test_Disable_Switch


;******************************************************************************
;
;   Print_Hex_Byte
;
;   DESCRIPTION:
;
;   ENTRY:
;	AL = Byte to print
;
;   EXIT:
;
;   USES:
;	Flags
;
;==============================================================================

Hex_Convert_Table db "0123456789ABCDEF"

BeginProc Print_Hex_Byte

	pusha

	mov	bx, ax
	mov	si, ax

	shr	bx, 4
	and	bx, 1111b
	and	si, 1111b

	mov	dl, Hex_Convert_Table[bx]
	mov	ah, 2
	int	21h

	mov	dl, Hex_Convert_Table[si]
	int	21h

	popa
	ret

EndProc Print_Hex_Byte

ENDIF

;******************************************************************************
;
;   Clean_Up_Hooks
;
;   DESCRIPTION:
;
;   ENTRY:
;
;   EXIT:
;
;   USES:
;
;==============================================================================

BeginProc Clean_Up_Hooks

	pusha

	xor     ecx, ecx
	xchg	ecx, [DOS_BIOS_Int13_Vec]
	jecxz	WD_UIC_Int13_Unhooked

	mov	eax, [ROM_BIOS_Int13_Vec]

	mov     dx, cx
	shr     ecx, 16
	mov     ds, cx

	mov     bx, ax
	shr     eax, 16
	mov     es, ax

	mov     ah, 13h
	int     2Fh

	mov     ax, cs
	mov     ds, ax
	mov     es, ax

;
;   Check to see if we have incremented the InDOS flag and broadcasted the
;   hardware detection Int 2Fh API.  If so, dec the InDOS flag and signal
;   then end of hardware detection.
;
WD_UIC_Int13_Unhooked:
	xor	cx, cx
	xchg	cl, [Signaled_Detection]
	jcxz	SHORT WD_UIC_Exit

;
;   WARNING:  Strange hanging bug with MS-NET server kernel loaded if you
;	      try to get the InDOS ptr while InDOS is non-zero.  Therefore,
;	      we don't want to call DOS here to get the InDOS flag.  That's
;	      why we saved it away earlier.
;
	push	es
	les	bx, [InDOS_Ptr] 		; ES:[BX] -> InDOS flag
	dec	BYTE PTR es:[bx]		; Dec the InDOS flag
	pop	es

	mov	ax, (W386_Int_Multiplex SHL 8) + W386_Device_Broadcast
	mov	bx, BlockDev_Device_ID
	mov	cx, BlockDev_API_Hw_Detect_End
	int	2Fh

WD_UIC_Exit:
	popa
	ret

EndProc Clean_Up_Hooks


;******************************************************************************
;
;   Fatal_Disk_Error
;
;   DESCRIPTION:
;       This procedure is jumped to by WDCtrl_Validate_Drive if a fatal
;       error is detected.  It displays a nasty warning message and hangs
;       the machine.
;
;   ENTRY:
;       DS = Our data segment
;       BL = Error code
;
;   EXIT:
;       Never!
;
;   USES:
;       Who cares!
;
;==============================================================================

BeginProc Fatal_Disk_Error

	call	Clean_Up_Hooks

IFNDEF SETUP
        add     bl,'0'                  ;Convert error code to ASCII 0-9
        mov     si, OFFSET WD_Fatal_Error_Code
        mov     ds:[si], bl             ;Poke the error number in

	mov     ah, 9
	mov     dx, OFFSET WD_Fatal_Error_Msg
	int     21h
ELSE
	mov   ax, DGROUP              ;Set up DS, ES to our data segment
	mov   ds, ax                  ;before calling error display routine.
	mov   es, ax

	mov   ax, ERR_WDCTRL_FATAL
	push  ax
	call  _WDCtrlHangError
	pop   ax
ENDIF

	jmp     $

EndProc Fatal_Disk_Error


;******************************************************************************
;
;   WDCtrl_Validate_Drive
;
;   DESCRIPTION:
;   The procedure must be called with the following parameters:
;	DL = Int 13h drive number to test (80h or 81h)
;	[0:DI] -> Fixed disk parameter table for this drive
;	DS=ES pointing to a data segment with the following data variables:
;
;	    Orig_Int13_Vector	dd	?
;	    Int13_Buffer	db	512 dup (?)
;	    My_Read_Buffer	db	512 dup (?)
;
;	The Orig_Int13_Vector must be filled in by the caller before invoking
;	this code.  This variable should contain the CS:IP value returned
;	by DOS from the Int 2Fh/AH=13h call to obtain the address of the
;	original Int 13h handler.  The other variables do not need to be
;	filled in.
;
;   The procedure will do one of three things:
;	Return with carry clear to indicate that the drive is WDCtrl compatible
;	Return with carry set to indicate that it can't use the drive
;	Jump to a label named "FATAL_DISK_ERROR".  The main program should
;	    display an error message and HANG THE MACHINE if this happens.
;
;	If the procedure returns with carry set then CX will contain a value
;	    that defines the phase of initialization that failed.
;
;       A flag is returned in BX indicating whether or not the ALTERNATE
;	Status register should be used.  BX is set to 0FFFFh iff the
;	alt status register should be used by WDCtrl for this drive.  BX
;	will be 0 if the normal status register should be used for this drive.
;
;   All segment registers will be preserved by the procedure.  All other
;   registers and flags will be modified.
;
;==============================================================================


Check_Invalid MACRO JmpIns
        LOCAL WD_CI_Invalid, WD_CI_Valid

IFNDEF DEBUG
	JmpIns	WD_VD_Invalid
	inc	[bp.Test_Phase]
ELSE
        JmpIns  SHORT WD_CI_Invalid
        inc     [bp.Test_Phase]
        jmp     SHORT WD_CI_Valid

WD_CI_Invalid:
        int     3
        jmp     WD_VD_Invalid

WD_CI_Valid:
ENDIF
	ENDM

Check_Fatal_Error MACRO JmpIns,ErrorCode        ;;Trashes bl
	mov	bl, ErrorCode
	JmpIns	Fatal_Disk_Error
	ENDM


BeginProc WDCtrl_Validate_Drive

Test_Stack_Frame STRUC
Test_Sector_Num      dw         ?
Test_Cyl_Num	     dw	        ?
Test_Head_Num	     dw	        ?
Test_Phase	     dw	        ?
Test_Loop_Phase      dw	        ?
Test_Drive_Number    db	        ?
		     db	        ?       ; Dword align
Test_Param_Off	     dw	        ?
Test_Param_Seg	     dw	        ?
Test_Alt_Status_Flag dw         ?
Test_Stack_Frame ENDS


	push	es
	push	bp
	sub	sp, SIZE Test_Stack_Frame
	mov	bp, sp

	mov	[bp.Test_Drive_Number], dl

	xor	ax, ax
	mov	[bp.Test_Phase], ax
	mov	[bp.Test_Loop_Phase], ax

	mov	es, ax
	les	di, es:[di]			; ES:DI -> Drive param table

	mov	[bp.Test_Param_Seg], es
	mov	[bp.Test_Param_Off], di

        ;** Default to the ALT status register.  We clear the bit
        ;**     anytime we fail with this register and use the normal one.
	mov	[bp.Test_Alt_Status_Flag], 0FFFFh

;------------------------------------------------------------------------------
;
;   First make sure that the fixed disk parameter table contains the same
;   information as Int 13h Get Drive Parameters returns.
;
;------------------------------------------------------------------------------

        ;** Phase 0
	mov	ah, 08h
	pushf
	cli
	call	[ROM_BIOS_Int13_Vec]		; Q: Did get drive params work?
	Check_Invalid jc			;    N: Error
						;    Y: Compare to FDPT

        ;** Phase 1
	inc	dh				; One greater returned
	cmp	dh, es:[di.FDPT_Max_Heads]	; Q: Heads equal?
	Check_Invalid jne			;    N: No good

        ;** Phase 2
	mov	ax, cx
	and	al, 00111111b
	cmp	al, es:[di.FDPT_Sec_Per_Track]	; Q: Sector count the same?
	Check_Invalid jne			;    N: That's strange!

        ;** Phase 3
	shr	cl, 6
	xchg	ch, cl
	add	cx, 2				; TWO GREATER RETURNED!
	cmp	cx, es:[di.FDPT_Max_Cyl]	; Q: Cylinder numbers match?
        jne     SHORT @F                        ;    N: Try Zenith check
        inc     [bp.Test_Phase]                 ;    Y: Yes.  Bump phase
        jmp     SHORT WD_VD_CylOK
@@:     inc     cx                              ; Check THREE for Zenith
	cmp	cx, es:[di.FDPT_Max_Cyl]
	Check_Invalid jne			;    N: Very, very weird
WD_VD_CylOK:


;------------------------------------------------------------------------------
;
;   The Test_Loop_Phase variable determines which test to perform.  We will
;   attempt to read 3 different sectors:
;	The 1st sector on the 1st head on the 1st Cyl
;	The 503rd sector
;	The next-to-last sector on the next-to-last head on the next-to-last cyl
;
;------------------------------------------------------------------------------

WD_VD_Do_Next_Test:
	mov	ax, [bp.Test_Loop_Phase]
	cmp	ax, 1
	je	SHORT WD_VD_Test_503
	ja	SHORT WD_VD_Test_Last

	inc	ax
	mov	[bp.Test_Sector_Num], ax
	mov	[bp.Test_Head_Num], ax
	mov	[bp.Test_Cyl_Num], ax
	jmp	SHORT WD_VD_Do_Read_Now

;------------------------------------------------------------------------------
;
;   Now read the next-to-the-last sector on the next-to-the-last head,
;   and on the cylinder 7/8 of the way to the end.  We don't test the
;   last sector because the BIOS might be lying to us about where the
;   last sector resides.
;
;------------------------------------------------------------------------------

WD_VD_Test_Last:
	movzx	ax, es:[di.FDPT_Sec_Per_Track]
	dec	ax
	mov	[bp.Test_Sector_Num], ax
	movzx	ax, es:[di.FDPT_Max_Heads]
	dec	ax
	mov	[bp.Test_Head_Num], ax
	mov	ax, es:[di.FDPT_Max_Cyl]
        mov     cx, ax                          ; Get cyl 7/8 of way to end
        shr     cx, 3
        sub     ax, cx
	mov	[bp.Test_Cyl_Num], ax
	jmp	SHORT WD_VD_Do_Read_Now

;------------------------------------------------------------------------------
;
;   Now figure out the head/cyl/sector number for sector 503 on the disk.
;   This number was chosen since 503 (decimal) is a prime number.  This will
;   verify that the BIOS is using standard translation of the BIOS parameter
;   table.
;
;------------------------------------------------------------------------------

WD_VD_Test_503:
	mov	eax, 503			; EAX = Sector to begin xfer at

	xor	edx, edx			; Zero high dword for idiv
	movzx	ecx, es:[di.FDPT_Sec_Per_Track]
	idiv	ecx				; Remainder+1 = Starting sector
	inc	dx				; Inc to make 1 based
	mov	[bp.Test_Sector_Num], dx	; Save it on the stack
	xor	edx, edx			; Zero high dword for idiv
	movzx	ecx, es:[di.FDPT_Max_Heads]
	idiv	ecx				; EAX = Cylinder number
						; EDX = Starting head
	mov	[bp.Test_Cyl_Num], ax		; Save 'em for later
	mov	[bp.Test_Head_Num], dx

;------------------------------------------------------------------------------
;
;   The drive parameters look OK.  Now verify read a sector from the drive to
;   attempt to force that drive to be selected.  To prevent problems with a
;   cache we will call the original BIOS handler directly.
;
;------------------------------------------------------------------------------

        ;** Phase 4/B/12  (Three passes through here)
WD_VD_Do_Read_Now:
	push	es
	mov	ax, ds
	mov	es, ax
	mov	bx, OFFSET Int13_Buffer
	mov	ax, 0201h			; Read one sector
	mov	cx, [bp.Test_Cyl_Num]
	xchg	ch, cl
	shl	cl, 6
	or	cl, BYTE PTR [bp.Test_Sector_Num]
	mov	dl, [bp.Test_Drive_Number]	; DL = Drive number
	mov	dh, BYTE PTR [bp.Test_Head_Num]
	pushf
	cli
	call	[ROM_BIOS_Int13_Vec]
	pop	es				; ES:DI points to drive params!
        mov     al, ah                          ; Put value in AL for macro
	Check_Invalid jc

;------------------------------------------------------------------------------
;
;   The thing worked!  Now check all the I/O ports to make sure they look
;   the way we want them to.
;
;------------------------------------------------------------------------------

        ;** Phase 5/C/13
	mov	dx, WDIO_Def_Base_Port+WDIO_Error_Off
	in	al, dx				; Read the error status
	test	al, al				; Non-zero is no good
	Check_Invalid jnz

        ;** Phase 6/D/14
	inc	dx
	IO_Delay
	IO_Delay
	in	al, dx				; Read the sector count
	test	al, al				; Non-zero is no good
	Check_Invalid jnz

        ;** Phase 7/E/15
	inc	dx
	IO_Delay
	IO_Delay
	in	al, dx				; Read the sector number
	cmp	al, BYTE PTR [bp.Test_Sector_Num];Q: Is sector number right?
	Check_Invalid jne			;    N: Die! Die! Die!

        ;** Phase 8/F/16
	inc	dx
	IO_Delay
	IO_Delay
	in	al, dx				; Read low cyl number
	mov	ah, al
	inc	dx
	IO_Delay
	IO_Delay
	in	al, dx				; Read high cyl number
	xchg	al, ah
	cmp	ax, [bp.Test_Cyl_Num]		; Q: Cyl number correct?
	Check_Invalid jnz			;    N: Wrong track

        ;** Phase 9/10/17
	inc	dx
	mov	al, [bp.Test_Drive_Number]	; Get drive #
	and	ax, 1
	shl	ax, 12				; Move bit to proper position
	IO_Delay
	IO_Delay
	in	al, dx
	xor	al, ah				; Drive bit should be 0.
	xor	al, BYTE PTR [bp.Test_Head_Num] ; Should make head # 0
	cmp	al, 10100000b			; Q: Head 0, 512 byte sectors?
	Check_Invalid jne			;    N: Error

        ;** Phase A/11/18
	inc	dx
	IO_Delay
	IO_Delay
	in	al, dx				; Read status
	mov	ah, al				; Preserve real status in case
	and	ah, NOT (WDStat_ECC_Corrected OR WDStat_Index) ; of error we
	cmp	ah, WDStat_Ready OR WDStat_Seek_Complete ; will get good info
	Check_Invalid jne

;------------------------------------------------------------------------------
;
;   Wowsa!  All of the port status looks correct.  Now for the big, grand-
;   pooba test -- Actually try to read some data by directly programming
;   the controller.  This is pretty silly polling code, but it dosen't really
;   matter.
;
;------------------------------------------------------------------------------

	IO_Delay
	IO_Delay
	in	al, 0A1h
	or	al, 01000000b
	IO_Delay
	IO_Delay
	out	0A1h, al

	mov	dx, WDIO_Def_Base_Port+WDIO_Drive_Control_Off
	mov	al, es:[di.FDPT_Drive_Control]
	IO_Delay
	IO_Delay
	out	dx, al

	mov	ax, es:[di.FDPT_Write_Precom_Cyl]
	cmp	ax, -1
	jne	SHORT WD_VD_Set_Precom
	xor	ax, ax
WD_VD_Set_Precom:
	shr	ax, 2

	mov	dx, WDIO_Def_Base_Port+WDIO_Precomp_Off
	IO_Delay
	IO_Delay
	out	dx, al

	inc	dx
	mov	al, 1
	IO_Delay
	IO_Delay
	out	dx, al				; Sector count = 1

	inc	dx
	mov	ax, [bp.Test_Sector_Num]
	IO_Delay
	IO_Delay
	out	dx, al				; Sector number

	inc	dx
	mov	ax, [bp.Test_Cyl_Num]
	IO_Delay
	IO_Delay
	out	dx, al				; Low byte of cylinder
	inc	dx
	mov	al, ah
	IO_Delay
	IO_Delay
	out	dx, al				; High byte

	mov	al, 10100000b			; Head 0, drive 0
	test	[bp.Test_Drive_Number], 1	; Q: Drive 1?
	jz	SHORT WD_VD_Prog_Head
	mov	al, 10110000b			; Head 0, drive 1
WD_VD_Prog_Head:
	or	al, BYTE PTR [bp.Test_Head_Num]
	inc	dx
	IO_Delay
	IO_Delay
	out	dx, al

	inc	dx
	mov	al, 20h
	IO_Delay
	IO_Delay
	out	dx, al				; Send a read to Mr. Ctrl

;------------------------------------------------------------------------------
;
;   The command has been sent.	Now wait for the command to complete.
;   Check to see whether or not we have already detected that we CANNOT
;   use the ALT status register.  If we detect this, we don't try again.
;
;------------------------------------------------------------------------------

	cmp	[bp.Test_Alt_Status_Flag], 0
	je	SHORT WD_VD_Try_Normal_Status	; Nope, must use NORMAL status
	mov	ax, 40h
	mov	es, ax
	mov	cx, 18*3/4			; 3/4 second time-out
        mov     dx, WDIO_Def_Base_Port + WDIO_Alt_Stat_Off

WD_VD_One_More_Tick:
	mov	bl, BYTE PTR es:[6Ch]		; BL = Low byte of tick count
WD_VD_Loop_Til_Done:

	IO_Delay
	IO_Delay
	in	al, dx				; AL = Status
	test	al, WDStat_Busy 		; Q: Controller busy?
	jz	SHORT WD_VD_No_Longer_Busy	;    N: Read the data!
						;    Y: Keep polling

WD_VD_Not_Really_Done:
	cmp	bl, BYTE PTR es:[6Ch]		; Q: Has time changed?
	je	SHORT WD_VD_Loop_Til_Done	;    N: Don't time-out
	loop	WD_VD_One_More_Tick		;	else dec CX count
        jmp     SHORT WD_VD_Try_Normal_Status   ; We timed out!!

;------------------------------------------------------------------------------
;
;   The controller is no longer busy.  Now make sure the status looks right.
;   (No error, data request set)
;   This test is kind of strange, but it is important.	Apparently on some
;   controllers the controller will go to the not busy state BEFORE setting
;   the DRQ bit.  We'll just keep looping until we see not busy AND DRQ set.
;
;   If the ALT status register doesn't look good, we'll try it again with
;   the normal status register.
;
;------------------------------------------------------------------------------

WD_VD_No_Longer_Busy:
	test	al, WDStat_Error		; Q: Error?
        jnz     SHORT WD_VD_Try_Normal_Status   ;  Y: Try the normal status reg
	test	al, WDStat_DRQ			; Q: Data request set?
	jz	SHORT WD_VD_Not_Really_Done  	;  N: Keep looking for it

        ;** Once we get here, we're OK using the ALT status register
        mov     dx, WDIO_Def_Base_Port + WDIO_Status_Off
	IO_Delay
	IO_Delay
	in	al, dx				; Clear IRQ by reading status
        jmp     SHORT WD_VD_Status_OK           ;  Y: Things look happy

;------------------------------------------------------------------------------
;
;   If we timed out or if we encountered a strange register configuration,
;   we need to check the normal status register because we've only been
;   looking at the alternate up to this point.  We will flag what the final
;   decision between normal and alternate register is.  If neither one is
;   right, we bail out.
;
;------------------------------------------------------------------------------

WD_VD_Try_Normal_Status:
	mov	[bp.Test_Alt_Status_Flag], 0
	mov	cx, 18*3/4			; Reset 3/4 second time-out
        mov     dx, WDIO_Def_Base_Port + WDIO_Status_Off

WD_VD_N_One_More_Tick:
	mov	bl, BYTE PTR es:[6Ch]		; BL = Low byte of tick count
WD_VD_N_Loop_Til_Done:

	IO_Delay
	IO_Delay
	in	al, dx				; AL = Status
	test	al, WDStat_Busy 		; Q: Controller busy?
	jz	SHORT WD_VD_N_No_Longer_Busy	;    N: Read the data!
						;    Y: Keep polling
WD_VD_N_Not_Really_Done:
	cmp	bl, BYTE PTR es:[6Ch]		; Q: Has time changed?
	je	SHORT WD_VD_N_Loop_Til_Done	;    N: Don't time-out
	loop	WD_VD_N_One_More_Tick		;	else dec CX count
        Check_Fatal_Error jmp, FATAL_TIME_OUT   ; Timeout!!!

;------------------------------------------------------------------------------
;
;   As with the ALT status register, we get here when the NORMAL status
;   register is no longer busy.  We have to check to see that there was
;   no reported error, and that the DREQ bit is set.  If we pass these
;   tests, we're still OK (even though the ALT reg failed) and we just
;   flag to always use the normal status register.
;
;------------------------------------------------------------------------------

WD_VD_N_No_Longer_Busy:
	test	al, WDStat_Error		; Q: Error?
    Check_Fatal_Error jnz, FATAL_BAD_STATUS ;    Y: Die!
	test	al, WDStat_DRQ			; Q: Data request set?
    jz	WD_VD_N_Not_Really_Done         ;    N: Keep looping!
                                            ;    Y: We look happy now

;------------------------------------------------------------------------------
;
;   Looks good.  Now read the data and compare it to what the BIOS gave us back
;
;------------------------------------------------------------------------------

WD_VD_Status_OK:
	mov	ax, ds
	mov	es, ax
	mov	di, OFFSET My_Read_Buffer
	mov	cx, 100h
	cld
	mov	dx, WDIO_Def_Base_Port+WDIO_Data_Off
	IO_Delay
	IO_Delay
	rep insw

	mov	si, OFFSET Int13_Buffer
	mov	di, OFFSET My_Read_Buffer
	mov	cx, 80h
	cld
	rep cmpsd
	Check_Fatal_Error jne, FATAL_DATA_BAD_COMPARE

;------------------------------------------------------------------------------
;
;   It worked!	Everything is OK.  No re-enable interrupts for the drive.
;
;------------------------------------------------------------------------------

	in	al, 0A1h
	and	al, NOT 01000000b
	IO_Delay
	IO_Delay
	out	0A1h, al

;------------------------------------------------------------------------------
;
;   Step to the next phase of the test.  First we will re-load ES:DI to point
;   to the drive parameter table.
;
;------------------------------------------------------------------------------

	les	di, DWORD PTR [bp.Test_Param_Off]

	inc	[bp.Test_Loop_Phase]
	cmp	[bp.Test_Loop_Phase], 3
	jb	WD_VD_Do_Next_Test

;------------------------------------------------------------------------------
;
;   This is a valid drive.  Return with carry clear.
;
;------------------------------------------------------------------------------

	mov     bx, [bp.Test_Alt_Status_Flag]
	add	sp, SIZE Test_Stack_Frame
	pop	bp
	pop	es
	clc
	ret

;------------------------------------------------------------------------------
;
;   This is NOT a valid drive.
;
;------------------------------------------------------------------------------

WD_VD_Invalid:
        xor     bx, bx                          ;Clear use bits
	mov	cx, [bp.Test_Phase]
	mov	ch, al
	add	sp, SIZE Test_Stack_Frame
	pop	bp
	pop	es
	stc
	ret

EndProc WDCtrl_Validate_Drive


;******************************************************************************
;
;   WD_RMI_Int_13h_Hook
;
;   DESCRIPTION:
;       This stub procedure is only hooked into the Int 13h chain for a
;       very short period of time.  However, if it is called, it will cause
;       a fatal error.  Disk cache programs that do "lazy writes" may wake
;       up on a timer interrupt and attempt to write out a sector.  In this
;       case, we will display an error message and hang the machine.
;
;   ENTRY:
;       Who cares!
;
;   EXIT:
;       Never
;
;   USES:
;       Anything it wants
;
;==============================================================================

BeginProc WD_RMI_Int_13h_Hook

IFDEF SETUP
	mov   ax, DGROUP	; Set up DS, ES to our data segment before
	mov   ds, ax		; calling the display error routine.
	mov   es, ax
	mov   ss, ax
	mov   sp, 0FFFEh

	mov   ax, ERR_WDCTRL_BAD_SOFTWARE
	push  ax
	call  _WDCtrlHangError
	pop   ax
ELSE
	mov     ah, 9
	mov     dx, OFFSET WD_Incompatible_Sw_Msg
	int     21h
ENDIF

	jmp     $

EndProc WD_RMI_Int_13h_Hook

IFNDEF SETUP
;******************************************************************************
;
;   WD_RMI_Get_Env_String
;
;   DESCRIPTION:
;       Tries to find a given string in the environment.  If the resulting
;       string starts with 'Y', 'y', 'T', 't', or '1', returns nonzero in
;       AX
;
;   ENTRY:
;       ES points to environment
;       SI is string to match
;       CX is length of string to match
;
;   EXIT:
;       AX is nonzero iff string is found and starts with chars above.
;
;   USES:
;       Does not trash ES or other seg regs.  Don't assume anything else!
;
;==============================================================================
BeginProc WD_RMI_Get_Env_String

	xor	di, di                  ;Environment always at zero offset
        mov     dx, si                  ;Save string start in DX
        mov     bx, cx                  ;Save string len in BX

WD_GES_Try_Another_String:
	mov	si, dx                  ;Point to start of compare string
        mov     cx, bx                  ;Restore string len match
	cld
        push    di                      ;Save current position in env seg
	repe cmpsb
	mov	al, es:[di]             ;Get the first char after '='
        pop     di
	jne	SHORT WD_GES_Try_Next_Env_Str

        cmp     al, '1'
        je	SHORT WD_GES_Exit       ;Return nonzero
	and     al, NOT ('a'-'A')       ;Convert to uppercase
	cmp     al, 'Y'
        je	SHORT WD_GES_Exit       ;Return nonzero
        cmp     al, 'T'
        je	SHORT WD_GES_Exit       ;Return nonzero
        xor     ax, ax                  ;Return zero, string found but bad char
	jmp	SHORT WD_GES_Exit

WD_GES_Try_Next_Env_Str:
	xor	cx, cx
	mov	ax, cx
	dec	cx
	repne scasb
	cmp	es:[di], al             ;End of environment?
	jne	WD_GES_Try_Another_String ;No, try next string
                                        ;Return zero, string not found

WD_GES_Exit:
        ret

EndProc WD_RMI_Get_Env_String

ENDIF

IFDEF SETUP
        FASTDISK ENDS
ELSE
        VxD_REAL_INIT_ENDS
ENDIF

IFDEF SETUP
        EndFile MACRO EntryPoint
                END
        ENDM
ELSE
        EndFile MACRO EntryPoint
                END EntryPoint
        ENDM
ENDIF

        EndFile WDCtrl_Real_Mode_Init
