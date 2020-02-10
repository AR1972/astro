	TITLE	PEEK - PEEK and POKE runtime functions
;***
; PEEK - PEEK and POKE runtime functions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - PEEK Function:	- POKE Statement:
;
;      v = PEEK(n)	     POKE n,m
;	     |		       |
;	  B$PEEK	     B$POKE
;
;
; - INP Function:	- OUT Statement:
;
;      v = INP(port)	     OUT port,v
;	     |			|
;	  B$PINP	     B$POUT
;
;
; - WAIT Statement:
;
;      WAIT port, AND_mask [,XOR_mask]
;	     |
;	  B$WAIT
;
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	useSeg	_BSS		
	useSeg	RT_TEXT 	

	INCLUDE seg.inc 	


sBegin	_BSS			
	externW b$seg		;current DEF SEG
sEnd	_BSS			

sBegin	RT_TEXT 		
assumes CS,RT_TEXT		


	SUBTTL	B$PEEK & B$POKE
	PAGE
;***
; B$PEEK - PEEK location
;
;Purpose:
; Get byte value at a far location.
;
;Input:
; addr	= far pointer to location
;
;Output:
; [AX]	= byte fetched from specified location
;
;Modifies:
; Per convention
;
;******************************************************************************
cProc	B$PEEK,<FAR,PUBLIC>,<ES> 
parmW	addr			; Offset from b$seg to fetch from
cBegin				


	MOV	ES,[b$seg]	; [ES] = cur DEF SEG value
	MOV	BX,addr 	; [ES:BX] = address to read from


	XOR	AX,AX		; init return value
	MOV	AL,ES:[BX]	; [AL] = byte to return

cEnd

;***
;B$POKE - POKE location
;
;Purpose:
; Put byte value at a far location.
;
;Input:
; addr	= address to be poked
; val	= value to be placed there
;
;Output:
; NONE
;
;Modifies:
; Per convention
;
;******************************************************************************
cProc	B$POKE,<FAR,PUBLIC>,<ES,BX> 
parmW	addr			; Offset from b$seg to put to
parmW	val			; value to put
cBegin				


	MOV	ES,[b$seg]	; [ES] = cur DEF SEG value
	MOV	BX,addr 	; [ES:BX] = address to write to


	MOV	AX,val		; get value to place
	MOV	ES:[BX],AL	; put it

cEnd

	SUBTTL	B$WVERIFY & B$RVERIFY - verify validity of address
	PAGE



;***
;B$PINP - Input byte from port into AL.  Added as part of [5]. 
;
;Purpose:
;
;Entry:
;	port_num = port number from which to input
;Exit:
;	AL = value input from port
;	AH = 0
;Modifies:
;	DX
;Preserves:
;	BX,CX
;Exceptions:
;	Permission denied error for DOS 5
;
;******************************************************************************
cProc	B$PINP,<FAR,PUBLIC>
parmW	port_num		; port number
cBegin
	MOV	DX,port_num	; get port number in DX
	IN	AL,DX		; input value from port into AL
	XOR	AH,AH		; clear high byte
cEnd


;***
;B$POUT - Output byte to port.  Added as part of [5]. 
;
;Purpose:
;
;Entry:
;	port_num = port number to which to output
;	value = byte to output to the port
;Exit:
;	None
;Modifies:
;	AL,DX
;Preserves:
;	AH,BX,CX
;Exceptions:
;	Permission denied error for DOS 5
;
;******************************************************************************
cProc	B$POUT,<FAR,PUBLIC>
parmW	port_num		; port number
parmB	value			; data to output
				; (allocated as a word on the stack)
cBegin
	MOV	DX,port_num	; get port number in DX
	MOV	AL,value	; get value to output into AL
	OUT	DX,AL		; output value to port
cEnd


;***
;B$WAIT - Wait for value to appear at port.  Added as part of [5]. 
;
;Purpose:
;	Wait for a non-zero value to appear at a port.  Continues to
;	wait until ((port_value XOR xor_mask) AND and_mask) is non-zero.
;
;Entry:
;	port = port number on which to wait
;	AND_mask = expression to AND against port value
;	XOR_mask = expression to XOR against port value
;Exit:
;	None
;Modifies:
;	None
;Preserves:
;	All
;Exceptions:
;	Permission denied error for DOS 5
;
;******************************************************************************
cProc	B$WAIT,<FAR,PUBLIC>
parmW	port_num		; port number
parmB	AND_mask		; expression to AND against port value
				; (allocated as a word on the stack)
parmB	XOR_mask		; expression to XOR against port value
				; (allocated as a word on the stack)
cBegin
	MOV	DX,port_num	; get port number in DX
WAIT_LOOP:
	IN	AL,DX		; input value from port into AL
	XOR	AL,XOR_mask	; XOR the value read from port with mask
	AND	AL,AND_mask	; AND the value read from port with mask
	JZ	WAIT_LOOP	; brif still zero -- continue to wait
cEnd



sEnd	RT_TEXT 		

	END
