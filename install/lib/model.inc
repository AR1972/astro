; ========================================================================
call_M	MACRO	Function

IF 	@codesize			; Call adjusted for memory model

	call	FAR PTR Function	; FAR call if LARGE, MED, or HUGE
ELSE
	call	NEAR PTR Function	; Else NEAR call
ENDIF
ENDM

; ========================================================================

DOSSEG

IFDEF  MODEL_SMALL
.MODEL	LARGE,C
ENDIF

IFDEF	MODEL_COMPACT
.MODEL  COMPACT,C
ENDIF

IFDEF  MODEL_MEDIUM
.MODEL	MEDIUM,C
ENDIF

IFDEF  MODEL_LARGE
.MODEL	LARGE,C
ENDIF

; ========================================================================
; LoadPtr
;
;	Macro to load a pointer based on the memory model that is in use.
;	Argument checking is included in the macro and the appropriate
;	error message will be displayed.
;	
; LoadPtr MACRO vSegReg, vReg, vPtr
;
;	vSegReg		- Segment register ( ES or DS )
;	vReg		- Any general purpose register (AX,BX,CX,DX,SI,DI)
;	vPtr		- Pointer to a memory location.
;			  The value will be treated as a dword ptr in
;			  LARGE/COMPACT model and word ptr in MEDIUM/SMALL.
;
; ========================================================================


LoadPtr MACRO vSegReg, vReg, vPtr

  SEG_OK = 0			;; Variables to specify different errors
  REG_OK = 0
  PTR_OK = 0

				;; Error checking on segment register
  IFNB <vSegReg>
    IRP x, <ES,DS>
      IFIDN <x>,<vSegReg>
        SEG_OK = 1
      ENDIF
    ENDM 
  ENDIF

				;; Error checking on index register
  IFNB <vReg>
    IRP y, <AX,BX,CX,DX,SI,DI>
      IFIDN <y>,<vReg>
        REG_OK = 1
      ENDIF
    ENDM 
  ENDIF

				;; Error checking on memory pointer
  IFNB <vPtr>
    PTR_OK = 1
  ENDIF

				;; If errs found display appropriate message
  IF (SEG_OK + REG_OK + PTR_OK) NE 3

    IF SEG_OK EQ 0
      %out <LoadPtr Macro --> Invalid segment register vSegReg was specified.>
    ENDIF

    IF REG_OK EQ 0
      %out <LoadPtr Macro --> Invalid register vReg was specified.>
    ENDIF

    IF PTR_OK EQ 0
      %out <LoadPtr Macro --> No memory pointer was specified.>
    ENDIF

    .ERR

  ELSE			;; Start of code in the macro since no errs found

    IF @DataSize	;; For LARGE & COMPACT models use LDS or LES  opcodes
			
      IFIDN <vSegReg>,<DS>
        lds	vReg, vPtr
      ELSEIFIDN <vSegReg>,<ES>
        les vReg, vPtr
      ENDIF

    ELSE
			;; If vSegReg == ES we have to set ES to DATA SEG
      IFIDN <vSegReg>,<ES>
        push	DS
        pop	ES
      ENDIF
			;; Load pointer into specified register
      mov  vReg, vPtr	

    ENDIF

  ENDIF

ENDM

