;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1990
;
;   Title:    EISA.INC EISA machine related stuff
;
;   Module:   EISA.INC - Equates and structs for EISA ROM RAM search exclusion
;
;   Version:  0.00
;
;   Date:     5/29/90
;
;   Author: ARR
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   07/27/88  0.00	Original
;
;******************************************************************************

;
; INT 15h AH = D8h AL = 00h or 80h
;
; Get slot information SHORT FORM
;
; CL = Slot number
;

;
; Return in AL is Bit field
;
GSI_DupIDMask	    equ     00001111b
GSI_DupIDShft	    equ     0
GSI_SlotTypMask     equ     00110000b
 GSI_SlotTyp_Exp    equ     00000000b
 GSI_SlotTyp_Emb    equ     00010000b
 GSI_SlotTyp_Virt   equ     00100000b
GSI_IDNotReadable   equ     01000000b
GSI_IDIsDupID	    equ     10000000b
;
; Return in DL is Bit field
;
GSI_HasPortInit     equ     00100000b
GSI_HasPortRange    equ     00010000b
GSI_HasDMA	    equ     00001000b
GSI_HasIRQ	    equ     00000100b
GSI_HasMemory	    equ     00000010b
GSI_HasFuncTyp	    equ     00000001b

;
; INT 15h AH = D8h AL = 01h or 81h
;
; Get slot information LONG FORM
;
; CH = Function # to read
; CL = Slot number
; DS:SI -> Buffer (320 bytes)
;

;
; Memory Info sub structure
;
GSI_Mem_Ent	struc

  GSI_Mem_Conf	  db	  ?
  GSI_Mem_DSize   db	  ?
  GSI_Mem_Addr256 db	  3 dup (?)
  GSI_Mem_SizeK   dw	  ?

GSI_Mem_Ent	ends

  ;
  ; Bits of GSI_Mem_Conf
  ;
  GSI_Mem_Cnf_RAM	  equ	  00000001b
  GSI_Mem_Cnf_Cached	  equ	  00000010b
  GSI_Mem_Cnf_TypMask	  equ	  00011000b
    GSI_Mem_Cnf_TypSYS	  equ	  00000000b
    GSI_Mem_Cnf_TypEXP	  equ	  00001000b
    GSI_Mem_Cnf_TypVIR	  equ	  00010000b
    GSI_Mem_Cnf_TypOTH	  equ	  00011000b
  GSI_Mem_Cnf_Shared	  equ	  00100000b
  GSI_Mem_Cnf_More	  equ	  10000000b

  ;
  ; Bits of GSI_Mem_DSize
  ;
  GSI_Mem_DS_DecodeMsk	  equ	  00001100b
    GSI_Mem_DS_Dc_20	  equ	  00000000b
    GSI_Mem_DS_Dc_24	  equ	  00000100b
    GSI_Mem_DS_Dc_32	  equ	  00001000b
  GSI_Mem_DS_SzMsk	  equ	  00000011b
    GSI_Mem_DS_Sz_1	  equ	  00000000b
    GSI_Mem_DS_Sz_2	  equ	  00000001b
    GSI_Mem_DS_Sz_4	  equ	  00000010b

;
; IRQ Info sub structure
;
GSI_IRQ_Ent	struc

  GSI_IRQ_Data		db	?
  GSI_IRQ_Reserved	db	?

GSI_IRQ_Ent	ends

  ;
  ; Bits of GSI_IRQ_Data
  ;
  GSI_IRQ_D_IRQMsk	  equ	  00001111b	; IRQ number
  GSI_IRQ_D_LevelTrig	  equ	  00100000b
  GSI_IRQ_D_Shared	  equ	  01000000b
  GSI_IRQ_D_More	  equ	  10000000b

;
; DMA Info sub structure
;
GSI_DMA_Ent	struc

  GSI_DMA_Data0 	db	?
  GSI_DMA_Data1 	db	?

GSI_DMA_Ent	ends

  ;
  ; Bits of GSI_DMA_Data0
  ;
  GSI_DMA_D0_ChanMsk	  equ	  00000111b	; Channel #
  GSI_DMA_D0_Shared	  equ	  01000000b
  GSI_DMA_D0_More	  equ	  10000000b

  ;
  ; Bits of GSI_DMA_Data1
  ;
  GSI_DMA_D1_TranSzMsk	  equ	  00001100b
    GSI_DMA_D1_Tran8	  equ	  00000000b
    GSI_DMA_D1_Tran16	  equ	  00000100b
    GSI_DMA_D1_Tran32	  equ	  00001000b
  GSI_DMA_D1_TimngMsk	  equ	  00110000b
    GSI_DMA_D1_TmgISA	  equ	  00000000b
    GSI_DMA_D1_TmgA	  equ	  00010000b
    GSI_DMA_D1_TmgB	  equ	  00100000b
    GSI_DMA_D1_TmgBRST	  equ	  00110000b

;
; I/O port Info sub structure(s)
;
GSI_Port_REnt	struc

  GSI_Port_R_Flgs	db	?
  GSI_Port_R_PortAddr	dw	?

GSI_Port_REnt	ends

  ;
  ; Bits of GSI_Port_R_Flgs
  ;
  GSI_Port_RF_NumPrtMsk   equ	  00011111b	; Number of sequential ports - 1
  GSI_Port_RF_Shared	  equ	  01000000b
  GSI_Port_RF_More	  equ	  10000000b

GSI_Port_IEnt	struc

  GSI_Port_I_Type	db	?
  GSI_Port_I_PortAddr	dw	?

GSI_Port_IEnt	ends

  ;
  ; Bits of GSI_Port_I_Type
  ;
  GSI_Port_IT_AccTypeMsk  equ	  00000011b
    GSI_Port_IT_AccType8  equ	  00000000b
    GSI_Port_IT_AccType16 equ	  00000001b
    GSI_Port_IT_AccType32 equ	  00000010b
  GSI_Port_IT_UseMskVal   equ	  00000100b
  GSI_Port_IT_More	  equ	  10000000b

;
; 320 byte Structure returned by call
;

GSI_MAXSELS	equ	26
GSI_TYPSTRLEN	equ	80
MAX_MEM_ENTS	equ	9
MAX_IRQ_ENTS	equ	7
MAX_DMA_ENTS	equ	4
MAX_PORT_RENTS	equ	20
MAX_PORT_IENTS	equ	20

GSI_Data	struc

  GSI_CompID	    db	    4 dup (?)
  GSI_Info0	    db	    ?
  GSI_Info1	    db	    ?
  GSI_CFG_Rev_Min   db	    ?
  GSI_CFG_Rev_Maj   db	    ?
  GSI_Select	    db	    GSI_MAXSELS dup (?)
  GSI_FuncInfo	    db	    ?
  GSI_TypeSubType   db	    GSI_TYPSTRLEN dup (?)
  GSI_Mem_Info	    db	    (MAX_MEM_ENTS * (SIZE GSI_Mem_Ent)) dup (?)
  GSI_IRQ_Info	    db	    (MAX_IRQ_ENTS * (SIZE GSI_IRQ_Ent)) dup (?)
  GSI_DMA_Info	    db	    (MAX_DMA_ENTS * (SIZE GSI_DMA_Ent)) dup (?)
  GSI_Port_RInfo    db	    (MAX_PORT_RENTS * (SIZE GSI_Port_REnt)) dup (?)
  GSI_Port_IInfo    db	    (MAX_PORT_IENTS * (SIZE GSI_Port_IEnt)) dup (?)

GSI_Data	ends

;
; Bits of GSI_FuncInfo
;
GSI_FI_IsTypStr     equ     00000001b
GSI_FI_IsMem	    equ     00000010b
GSI_FI_IsIRQ	    equ     00000100b
GSI_FI_IsDMA	    equ     00001000b
GSI_FI_IsPortRng    equ     00010000b
GSI_FI_IsPortInit   equ     00100000b
GSI_FI_IsCFGFreeFrm equ     01000000b
GSI_FI_FuncDisabled equ     10000000b


;
; Get Slot Info error codes
;
EISA_Err_InvalSlot	equ	80h
EISA_Err_InvFuncNum	equ	81h
EISA_Err_CMOSBad	equ	82h
EISA_Err_EmptySlot	equ	83h
EISA_Err_InvBIOSCall	equ	86h
EISA_Err_InvConfig	equ	87h


