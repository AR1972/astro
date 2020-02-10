	TITLE	WIDTH - WIDTH "device" Statement
;***
; WIDTH - WIDTH "device" Statement
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - WIDTH Statement:
;
;    Four different Syntax possibilities map to four runtime entry points:
;
;      WIDTH size			 WIDTH LPRINT size
;	 |				   |
;      B$WIDT				 B$LWID
;
;
;      WIDTH filenumber, size		 WIDTH device, size
;	 |				   |
;      B$DWID				 B$DWID
;
;******************************************************************************
	include switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	DV_TEXT 	
	USESEG	_DATA

	INCLUDE seg.inc 	
	include baslibma.inc
	include devdef.inc
	include files.inc
	include string.inc	

assumes CS,DV_TEXT		
sBegin	DV_TEXT 		

	externNP B$GET_DEV_NUM	
	externNP B$STDALCTMP
	externNP B$DevDispatch	
	externNP B$ERR_FC	

	PAGE


;***
; B$DWID - WIDTH "device" Statement
;
; Purpose:
;
; Entry:
;	sdDevName  = device name string
;	newWidth   = width
; Exit:
;	None
; Modifies:
;	Per convention
; Exceptions:
;	B$ERR_BFN -- for bad device name
;**************************************************
cProc	B$DWID,<PUBLIC,FAR>	
parmSD	sdDevName		
parmW	newWidth		
cBegin				
	GetpSD	BX,sdDevName	
	CALL	B$GET_DEV_NUM	; (AL) = device number, NZ if valid device
				; DISK_DWID gives "illegal function call"
wdd1:
	CALL	B$STDALCTMP	;Delete if temporary string

	MOV	AH,DV_DWID	; device width function
	MOV	DX,newWidth	
	or	dh,dh		; if width > 255 then error
	jnz	ercfc		; and
	or	dl,dl		; if width = 0 then error
	jz	ercfc		

	CALL	B$DevDispatch	; dispatch to the device width set routine
cEnd				

ercfc:	JMP	B$ERR_FC

sEnd	DV_TEXT 		
	END
