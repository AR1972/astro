	TITLE	TABSPC - TAB and SPC functions
;***
; TABSPC - TAB and SPC functions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************
	INCLUDE switch.inc	
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	_BSS		
	USESEG	DV_TEXT 	
	USESEG	_DATA		

	INCLUDE seg.inc 	
	INCLUDE idmac.inc	

sBegin	_BSS			

	externW b$VECS		

VPOS	EQU	b$VECS+2*0	
VWID	EQU	b$VECS+2*1	
VWCLF	EQU	b$VECS+2*2	

sEnd	_BSS			

assumes CS,DV_TEXT		
sBegin	DV_TEXT 		

	externNP B$OutBlanks	
;***
;B$FSPC - SPC function
;
;Purpose:
;	Runtime Entry Point.
;	Convert negative numbers to zero, then modulo it with line width.
;	Then print that many spaces.
;Entry:
;	cSpaces = number of spaces
;Exit:
;	None.
;Uses:
;	Per convention.
;Exceptions:
;	None.
;****
cProc	B$FSPC,<PUBLIC,FAR>	

parmW	cSpaces 		
cBegin				
	MOV	AX,cSpaces	
	CALL	RNGCHK		;[CL]=cSpaces to print
OUTSPC:
	XOR	CH,CH		
	CALL	B$OutBlanks	; output CX blanks
cEnd				

;***
;B$FTAB - TAB function of PRINT statement
;
;Purpose:
;	Runtime Entry Point.
;	Convert arg<1 to 1, then modulo with line length.  Tab to position,
;	send CR/LF if already beyond position.
;Entry:
;	TabPos = position to tab to (1 is first)
;Exit:
;	None.
;Uses:
;	Per convention.
;Exceptions:
;	None.
;****
cProc	B$FTAB,<PUBLIC,FAR>	
parmW	TabPos			
cBegin				
	MOV	AX,TabPos	
	DEC	AX
	CALL	RNGCHK		;[CL]=new pos, [CH] = current pos
	CMP	CL,CH		;Check against current position
	JAE	FIGSPC
	CALL	[VWCLF] 	; Output CR/LF if too far
	XOR	CH,CH
FIGSPC:
	SUB	CL,CH		;Number of spaces needed
	JMP	SHORT OUTSPC	
cEnd	<nogen> 		


;***
;RNGCHK - computes new position MOD line width.
;
;Purpose:
;	Converts negative numbers to Zero and computes requested
;	position MOD current line width.
;Entry:
;	AX - requested position
;Exit:
;	CH - current position
;	CL - requested position MOD line width
;Uses:
;	Per convention
;Exceptions:
;	None.
;****
cProc	RNGCHK,<NEAR>		
cBegin				
	CWD			;sign extend AX into DX
	NOT	DX		;DX = 0 if AX was negative
	AND	AX,DX		;Make negative numbers zero
	XCHG	AX,BX
	CALL	[VWID]
	XCHG	AL,AH
	MOV	AH,0
	DbAssertRelB al,ne,0,DV_TEXT,<RNGCHK:WIDTH=0> 
	XCHG	AX,BX		;(BX) = width
	XOR	DX,DX
	DIV	BX
	CALL	[VPOS]		; (AH) = position
	MOV	AL,DL		;DIV remainder is <arg> MOD <width>
	XCHG	AX,CX		;(AX) = now has original (CX)
cEnd				

sEnd	DV_TEXT 		
	END
