;*** 
;Scanner.inc
;
;	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
;
;
;*******************************************************************************

SCANNER_INC	= ON

;=============================================================================
;Scanner Constants
;

;Scanner State definitions
SS_EXECUTE	equ LOW 0
SS_PARSE	equ LOW 1
SS_RUDE		equ LOW 2
SS_SUBRUDE	equ LOW 3	;same as rude except oPrs operands are
				;actually oNam's. 

;Offsets into frame, relative execute-time BP:
FR	struc	
FR_bpLink	dw	?	;Caller's bp
FR_otxRet	dw	?	;oTx of return address
FR_oRsRet	dw	?	;oRS of return address
FR_pGosubLast	dw	?	;Pointer to last gosub return otx
FR	ends	

FR_MinFrame	=	size FR	; Minimum frame size, not counting portion
				; of frame below the pushed bp
FR_basBpLink	=      -2d	; previous value of b$curframe
FR_FirstVar	=      -2d	; offset to first framevar
;=============================================================================
;Non-RELEASE flag, allows for scanning from SS_RUDE to SS_PARSE


sBegin	DATA

ife	SSDATA_ASM
	extrn	SsLineCount:word	;No. of lines scanned
endif	;SSDATA_ASM
sEnd	DATA

ife	SSSCAN_ASM
	EXTRN	SsScan:far		
	EXTRN	SsDescan:far		
	EXTRN	ScanAndExec:near
endif	;SSSCAN_ASM

ife	SSDECLARE_ASM
	extrn	ChainCommon:far
	extrn	SsAdjustCommon:near
	extrn	SsTrimCommon:near
endif	;SSDECLARE_ASM

ife	SSRUDE_ASM
sBegin	SCAN
	extrn	SsRudeScan:far		; Rude scanner/descanner
sEnd	SCAN
endif	;SSSCAN_ASM

