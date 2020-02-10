	PAGE	56,132
	TITLE	GETPUT - iAPX 88/86 SUPPORT FOR GRAPHICS PUT/GET STATEMENTS
;***
; GETPUT - iAPX 88/86 SUPPORT FOR GRAPHICS PUT/GET STATEMENTS
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - GET Statement:
;
;      GET (x1,y1)-(x2,y2),arrayname [(indecies)]
;	|     |       |
;	|   Coord Routines	      B$GGET
;	|				 |
;	+--------------------------------+
;
; - PUT Statement:
;
;      PUT (x,y),array[(indecies)][,action]
;	|    |
;	|  Coord Routines	B$GPUT
;	|			   |
;	+--------------------------+
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions


	useSeg	_BSS		
	useSeg	GR_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE array.inc	


sBegin	_BSS			
;
;****************************************************************************
; External low-level function vectors
;****************************************************************************
;
externW b$MapXYC		
externW b$UpC			
externW b$DownC			
externW b$SetPixLastC		

externW B$GXPOS			; defined in GWDATA.ASM
externW B$GYPOS			; defined in GWDATA.ASM
externW B$MINDEL 		; defined in GWDATA.ASM
externW B$MAXDEL 		; defined in GWDATA.ASM

externB B$WNDWSW 		; defined in GWDATA.ASM
externB B$WNDWSC 		; defined in GWDATA.ASM

sEnd	_BSS			

sBegin	GR_TEXT 		
assumes CS,GR_TEXT		

externNP B$IDIVBX		
externNP B$ComputeSpace		


externNP B$COORD1		
externNP B$COORD2		

externNP B$INVIEW		

externNP B$SCINIT		; Performs screen initialization

externNP B$ERR_FC		
externNP B$ERR_BS		

;
;low-levels:
;
externNP B$ImageSize		
externNP B$PixSize		
externNP B$PutGetInit		
externNP B$NReadC		
externNP B$NWriteC		
externNP B$XDELT 		
externNP B$YDELT 		



	SUBTTL	B$GGET - read rectangle screen area into the specified array
	PAGE
;***
; B$GGET - read the rectangle screen area into the specified array
; void pascal B$GGET(far *fpData, ad *pAd)
;
;Purpose:
; Read all the pixels in the specified rectangle on the screen into the array.
; The first two bytes of the array will be set to the number of bits along the
; X axis, the second two bytes, the Y axis.
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement.
;
;Entry:
; fpData = Far pointer to location at which to place the data.
; pAd	 = Pointer to the array descriptor to be used...
;
;Exit:
; Array set
;
;Uses:
; Per convention
;
;Exceptions:
; Control could be transfered to an error routine, such as B$ERR_FC
;
; [3/4] Routine largely rewritten for array indecies and new array descriptor
;******************************************************************************
cProc	B$GGET,<FAR,PUBLIC>,<ES,SI>
parmD	fpData			;far ptr to start of array data
parmW	pAd			;pointer to array descripptor of interest
cBegin				

	CALL	B$SCINIT	; initialize screen if not already done.
	cCall	B$COORD1	;Process first coord pair
	PUSH	CX		;save X1
	PUSH	DX		;save Y1

	cCall	B$COORD2	;Process second coord pair
;
; First step is to calculate the upper right hand corner and the delta-x and
; delta-y of the area that the caller has selected through previous calls to
; set the first and second pairs of coordinates.
;
	POP	DX		; [DX] = Y1
	POP	CX		; [CX] = X1
	CALL	B$YDELT		;CALC DELTA Y
	JNB	GPUTG2
	XCHG	DX,[B$GYPOS]	;ensure DX = MIN(Y1,Y2)
GPUTG2:

	CMP	B$WNDWSW,0
	JZ	GPUTGW		;Brif no WINDOW
	CMP	B$WNDWSC,0
	JNZ	GPUTGW		;Use MinY if WINDOW SCREEN
	XCHG	DX,[B$GYPOS]	;ensure DX = MIN(Y1,Y2)
GPUTGW:

	INC	BX		;MAKE DELTA A COUNT
	MOV	B$MINDEL,BX	;SAVE DELTA Y IN MIDEL
	CALL	B$XDELT
	JNB	GPUTG3
	XCHG	CX,[B$GXPOS]	;ensure CX = MIN(X1,X2)
GPUTG3:

	INC	BX		;MAKE DELTA A COUNT
	MOV	B$MAXDEL,BX	;SAVE DX IN B$MAXDEL
	CALL	[b$MapXYC]	;OEM ROUTINE CALL TO UPDATE GRAPHICS ACCUM
;
; Next step is to calculate the starting location in the array, and the amount
; of room available for the data to GET.
;
	cCall	B$ComputeSpace,<fpData,pAd>	
	PUSH	DX		;place on stack for subsequent pops
	PUSH	AX		;[ES:SI] = address, [TOS] = room left

;
; From the delta-x and delta-y, calculate the amount of room required for the
; graphics area to GET, and then make sure that the amount of available space
; calculated above is sufficient.
;
	MOV	BX,B$MINDEL	;Get delta Y
	MOV	DX,B$MAXDEL	;Get delta X
	XOR	CX,CX		;Clear CX before call to ImageSize
	CALL	B$ImageSize	;[CX:BX]=number bytes req'd, [DX]=bits per row
	JC	OVERFL		;Jump if overflow occurred
	ADD	BX,4		;Add 4 bytes to space needed for deltax,deltay
	adc	cx,0		;carry to high-order word
;	JC	GP0_ERR 	;Issue error if space needed > 64K
;	INC	CX
;	LOOP	GP0_ERR 	;If CX <> 0 issue error
	POP	AX		; [AX] = low bytes available
	SUB	AX,BX		; [AX] = low available - low required
	POP	AX		; [AX] = high available
	SBB	AX,CX		; [AX] = high available - high required
	JNB	GET0		;Continue if there's enough room
GP0_ERR: JMP	 B$ERR_FC	   ;ERROR IF TOO BIG

GET0:
;
; Begin the GET. Store the delta-x and delta-y as the first two (word) elements
; in the array, and then perform a line read delta-y times.
;
	PUSH	BP		;low level GET trashes BP
	MOV	BX,SI		;[ES:BX] = address
	MOV	ES:[BX],DX	;Store x dimension at ES:BX
	MOV	CX,B$MINDEL	;get y line count
	MOV	ES:[BX+2],CX	;Store y dimension at ES:BX + 2
	ADD	BX,4		;Increment array ptr past raster
	XCHG	CX,DX		;CX:=bits per row for INIT ;DX:=delta y of raster
	CLC			;Clear carry for INIT
	PUSH	DX		;RESAVE DY
	CALL	B$PutGetInit	; [CX]=BIT COUNT,[BX]=ARRAY ADDR
	POP	CX		;get y count


	MOV	AX,WORD PTR [B$WNDWSW] ;get window switches


;
;      if wndwsw and ~wndwsc then use upc else use downc
;
	NOT	AH		;negate B$WNDWSC (low bit 1 for no SCREEN)
	AND	AL,AH
	RCR	AL,1		;only make assumption about 0/1
	JC	GLOOPU

GLOOPD:
	PUSH	CX		;preserve count register
	CALL	B$NReadC	;read a raster line from the screen
	CALL	[b$DownC]	;move the cursor down a line
	POP	CX		;restore count register
	LOOP	GLOOPD		;repeat cx times


	JMP	SHORT GGET_90
GLOOPU:
	PUSH	CX		;preserve count register
	CALL	B$NReadC	;read a raster line from the screen
	CALL	[b$UpC]	;move the cursor up a line
	POP	CX		;restore count register
	LOOP	GLOOPU		;repeat cx times

GGET_90:

	POP	BP		;recover real BP
	call	[b$SetPixLastC] 



cEnd

OVERFL: JMP	B$ERR_BS
	PAGE

	SUBTTL	B$GPUT - write specified array contents to screen with action
	PAGE
;***
; B$GPUT - write specified array contents to screen with action
; void pascal B$GPUT(far *fpData, ad *pAd, I2 action)
;
;Purpose:
; Write the specified array data to the screen in a rectangle whose upper left
; vertex is the specified coordinate. The action is performed on the pixel
; value in the array before it is written to the screen. The rectangle width
; and height are kept in the first word of the array.
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement.
;
;Entry:
; fpData = Far pointer to location from which to obtain the data.
; pAd	 = Pointer to the array descriptor to be used.
; action = PUT action to be performed
;		1 for PSET
;		2 for PRESET
;		3 for XOR
;		4 for OR
;		5 for AND
;
;Exit:
; None.
;
;Uses:
; Per convention.
;
;Exceptions:
; Control could be transfered to an error routine, such as B$ERR_FC
;
; [3/4] Routine largely rewritten for array indecies and new array descriptor
;******************************************************************************
cProc	B$GPUT,<FAR,PUBLIC>,<DI,SI,ES>
parmD	fpData			;far ptr to start of array data
parmW	pAd			;pointer to array descriptor of interest
parmW	action			;PUT action to be taken
cBegin

	CALL	B$SCINIT	; initialize screen if not already done.
	cCall	B$COORD1	;Process Coord pair
	cCall	[b$MapXYC]	;update graphics cursor
;
; Start by determining the base address within the array at which to place
; data, taking into account if any indecies have been passed.
;
	LES	BX,fpData	;[ES:BX] = address to obtain data
	MOV	CX,ES
	INC	CX		;Is there a specified place?
	LOOP	GPUT_10 	;brif so
	MOV	BX,pAd		;[AX] = ptr to array descriptor
	LES	BX,DWORD PTR [BX].AD_fhd.FHD_oData ;[ES:BX] = base array address

GPUT_10:			;[ES:BX] = address
;
; Compute the opposite corner of the rectangle to be PUT, and ensure that it
; lies within the available area.
;
	MOV	SI,ES:[BX]	;[SI] = x bit count
	PUSH	SI
	MOV	DI,ES:[BX+2]	;[DI] = y dimension
	PUSH	DI
	ADD	BX,4		;[BX] = Pointer to array data
	PUSH	BX
	CALL	B$PixSize	;[AL] = BITS/PIXEL
	XCHG	AX,BX		;[BL] = bits/pixel
	XCHG	AX,SI		;[AX] = bit count for IDIV instruction
	CWD			;[DX:AX] = bit count for B$IDIVBX
	XOR	BH,BH		;[BX] = bits/pixel
	CALL	B$IDIVBX 	;[AX] = No. of pixels in x
	DEC	AX		;DECREMENT SINCE IT'S A COUNTER
	MOV	DX,B$GXPOS	;[DX] = starting X
	ADD	AX,DX		;[AX] = ending X
	JB	PRNGER		;ERROR IF CARRY
	MOV	CX,AX		;[CX] = ending X

	MOV	BX,B$GYPOS	;[BX] = starting Y
	CMP	B$WNDWSW,0
	JZ	PRNGNW		;Brif no WINDOW
	CMP	B$WNDWSC,0
	JNZ	PRNGNW		;Use MinY if WINDOW SCREEN
	SUB	BX,DI		;Since y is reversed if window
	JMP	SHORT PRNGER
PRNGNW:
	DEC	DI		;DECREMENT DY SINCE IT'S A COUNTER
	ADD	BX,DI		;[BX] = Ending Y (start Y + DELTA Y)
PRNGER:
	JNB	PRNGOK

PUTERR: JMP	B$ERR_FC	  ;ERROR IF CARRY

PRNGOK:
	MOV	DX,BX		;[DX]=Y + DELTA Y
	CALL	B$INVIEW 	;If coords inside viewport boundary
	JNC	PUTERR		;ABORT IF CX,DX OFF SCREEN
	POP	BX		;[ES:BX] = ARRAY POINTER
	POP	DX		;POP DY
	POP	CX		;POP DX*BITS/PIX
	PUSH	BP		;low levels trash BP
	PUSH	DX		;RESAVE DY
;
;For a PUT statement, on entry to INIT AL=put action mode (0..4):
;
;	0		OR
;	1		AND
;	2		PRESET
;	3		PSET
;	4		XOR
;


	STC			;SET CARRY TO FLAG PUT INIT
	MOV	AX,action	;get the action
	CALL	B$PutGetInit	;[CX]=BIT COUNT,[BX]=ARRAY ADDR
	POP	CX		;get y count

	MOV	AX,WORD PTR [B$WNDWSW] ;get window switches


;
;      if wndwsw and ~wndwsc then use upc else use downc
;
	NOT	AH		;negate B$WNDWSC (low bit 1 for no SCREEN)
	AND	AL,AH
	RCR	AL,1		;only make assumption about 0/1
	JC	PLOOPU

PLOOPD: 			;do put loop with downc
	PUSH	CX		;preserve count register
	CALL	B$NWriteC	;write a raster line to the screen
	CALL	[b$DownC]	;move the cursor down a line
	POP	CX		;restore count register
	LOOP	PLOOPD		;repeat cx times


	JMP	SHORT GPUT_90

PLOOPU: 			;put with upc
	PUSH	CX		;preserve count register
	CALL	B$NWriteC	;write a raster line to the screen
	CALL	[b$UpC]	;move the cursor up a line
	POP	CX		;restore count register
	LOOP	PLOOPU		;repeat cx times

GPUT_90:


	call	[b$SetPixLastC] 


	POP	BP		;recover trashed BP

cEnd

sEnd	GR_TEXT 		
	END
