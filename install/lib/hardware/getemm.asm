;========================================================
COMMENT #

	GETEMM.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================

	 Function returns the EMM386 version number if it's present else 0

	 unsigned GetEmmVersion( void )

	 ARGUMENTS:	NONE
	 RETURNS:	unsigned - EMM386 internal revision number or 0

	================================================

	Change Log:

    	Date      Who   NUMBER		Description
  	--------  ---  ----	---------------------------------------------
        08/20/90  JAH           Created
        01/23/90  DLB  M000     Test for $MMXXXX0 device (when "device=
                                EMM386 NOEMS") as well as EMMXXXX0 device.
                                Also deleted "POP ES" which trashed ES.
        01/23/90  DLB  M001     If EMMXXXX0 or $MMXXXX0 device exists, verify
                                it is EMM386.EXE by checking for "MICROSOFT"
                                signature.
        09/14/92  ECH  M002     Test for $EMMQXXX0 device (VCPI, but no EMS)
                                as well.

END COMMENT #
; =======================================================

INCLUDE model.inc

; =======================================================

EMM_INT_VECT		EQU 	67h
DEVICE_OFFSET		EQU	10

EMM_READ_FUNC		EQU	1
EMM_GET_VER		EQU	2
EMM_DEV_NAME    	EQU     "EMMXXXX0",0 ; Name for EMM driver IOCTL open.
EMM_DEV_NOEMS           EQU     '$'          ; M000: Replacement for first
                                             ;   char. of EMM_DEV_NAME when
                                             ;   "device=EMM386 NOEMS".

EMM_DEV_VCPI            EQU     "EMMQXXX0",0 ; M002: Name for EMM driver when
                                             ;   VCPI but not EMS support.

EMM386_SIG           	EQU     "MICROSOFT"  ; M001: EMM386 Signature.
                                             ; M001: Offset from device base.
EMM386_SIG_OFFSET       EQU     (0ah + EMM_DEV_NAME_LEN - 1 + 2)


; Structure of the data returned on the IOCTL call to the EMM driver

EMM_STRUC	struc

    EMM_Version_Maj	db	?   ; Internal revision number
    EMM_Version_Min	db	?

EMM_STRUC	ends

Version		EQU (-((SIZE EMM_STRUC) - EMM_Version_Maj))
Function	EQU (-(SIZE EMM_STRUC))

; =======================================================
;
; For the IOCTL to get the data and version number of importable EMM info
; the IOCTL read subfunction is encoded in the first byte of the IOCTL
; read buffer.
;

; =======================================================

.DATA

; =======================================================

DeviceName	db	EMM_DEV_NAME	; Name for EMM IOCTL opens.
EMM_DEV_NAME_LEN EQU    ($-DeviceName)  ; M001: Length of DeviceName string.

EMM386Sig    	db      EMM386_SIG	; M001: Signature for EMM386 device.
EMM386_SIG_LEN  EQU     ($-EMM386Sig) 	; M001: Signature length.

;M002
VCPIDevName	db	EMM_DEV_VCPI	; Name for EMM VCPI IOCTL opens.

; =======================================================

.CODE

; =======================================================

GetEmmVersion	PROC USES ES DI SI      ; M000

        pushf                           ; M001: Save flags due to CLD below.
	push	BP
	mov	BP,SP
	sub	SP,SIZE EMM_STRUC

;M000
	mov	DX,OFFSET DeviceName	; DS:DX pointer to emm name
	mov	AX,3d02h		; Try to open the device
	int	21h
        jnc     GEV40                   ; Jump if device opens.

	mov	DeviceName,EMM_DEV_NOEMS ; DS:DX -> NOEMS emm name
	mov	AX,3d02h		; Try to open the device
	int	21h
        jnc     GEV40                   ; Jump if device opens.

;M002
	mov	DX,OFFSET VCPIDevName	; DS:DX pointer to VCPI emm name
	mov	AX,3d02h		; Try to open the device
	int	21h
        jc      GEV60                   ; Jump if device not found.

; M001: Validate "MICROSOFT" signature.

GEV40:  push    AX                      ; Save EMM device handle.
        mov     AX,(35h SHL 8) OR EMM_INT_VECT
        int     21h                     ; ES:BX -> EMM device handler.
                                        ; ES:0000 -> EMM device base.
        mov     DI,EMM386_SIG_OFFSET    ; ES:DI -> EMM Signature.
        mov     SI,OFFSET EMM386Sig	; DS:SI -> EMM386 Signature.
        mov     CX,EMM386_SIG_LEN       ; CX = Signature length.
        cld
        repz    cmpsb                   ; Signature match?
        jz      GEV80                  	;   Yes, jump.
                                        ;   No, fall thru and exit.

GEV60:	xor	AX,AX			; return code = 0.
	jmp	SHORT FunctExit         ; Go fix stack and exit.

GEV80:	pop	BX			; BX = device handle.
;M000

	mov	AX,4400h		; IOCTL get device information.
	int	21h
	jc	SHORT ErrorClose	; Carry indicates call unsuccesful

	test	DX,0080h		; Test if clock device.
	jz	SHORT ErrorClose	; if not, we can't steal memory.

	test	DX,4000h		; Are IOCTL's 02h and 03h supported
	jz	SHORT ErrorClose	; if not we cannot steal memory.

	push	DS
	mov	AX,SS
	mov	DS,AX
	mov	DX,BP			; DS:DX == SS:BP
	mov	CX,SIZE EMM_STRUC	; CX == size of the ctrl string
	sub	DX,CX			; DS:DX --> Start of data struct

	mov	BYTE PTR [BP].Function,EMM_GET_VER

	mov	AX,4402h		; Read control device string function
	int	21h
	pop	DS
	jc	SHORT ErrorClose	; Carry indicates call unsuccesful

	cmp	AX,CX			; If ax != cx we did not get the
	jne	SHORT ErrorClose	; number of bytes we requested !
	mov	AX,WORD PTR [BP].Version ; Have good version #
	xchg	AH,AL			; Put major ver. in AH & minor in AL
	jmp	SHORT CloseHandle

ErrorClose:
	xor	AX,AX			; Invalid SmartDrv version #

CloseHandle:
	push	AX
	mov	AX,3E00h		; Close device, handle in BX.
	int	21h			; Call DOS
	pop	AX

FunctExit:
	mov	SP,BP
	pop	BP
        popf                            ; M001
	ret				; return to caller.

GetEmmVersion     ENDP

; =======================================================

	END
