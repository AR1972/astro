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
.MODEL	SMALL,C
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


LoadPtr MACRO vSegReg, vReg, vPtr

  SEG_OK = 0
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

  ELSE

    IF @DataSize

      IFIDN <vSegReg>,<DS>
        lds	vReg, vPtr
      ELSEIFIDN <vSegReg>,<ES>
        les vReg, vPtr
      ENDIF
    ELSE
      mov  vReg, vPtr	

    ENDIF

  ENDIF

ENDM
