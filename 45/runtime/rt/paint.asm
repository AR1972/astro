	PAGE	56,132
	TITLE	PAINT - iAPX 88/86 PAINT STATEMENT SUPPORT
;***
; PAINT - iAPX 88/86 PAINT STATEMENT SUPPORT
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - PAINT Statement - Calls B$PAIN or B$PNTC, depending on 'paint' attribute
;
;     PAINT (x,y) [[,paint [,boundary] ,background]]
;	|     |
;	|  Coord routines	    B$PAIN (if "paint" is numeric attribute)
;	|			       or
;	|			    B$PNTC (if "paint" is tile string)
;	|			       |
;	+------------------------------+
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	_DATA		
	USESEG	_BSS		
	USESEG	GR_TEXT 	

	INCLUDE seg.inc 	;define segments
	INCLUDE baslibma.inc
	INCLUDE idmac.inc	
	INCLUDE string.inc	

sBegin	_DATA			

	externW B$AC


sEnd	_DATA			

sBegin	_BSS			
;
;****************************************************************************
; External low-level function vectors
;****************************************************************************
;
externW b$LeftC		
externW b$ChkDownC		
externW b$ChkUpC		
externW b$ScanL		
externW b$ScanR		
externW b$MapXYC		
externW b$SetTile		
externW b$PaintBound		

	externW B$C1SAVE 	;defined in GWDATA.ASM
	externW B$C2SAVE 	;defined in GWDATA.ASM
	externW B$C3SAVE 	;defined in GWDATA.ASM
	externW B$MOVCNT 	;defined in GWDATA.ASM
	externW B$SKPCNT 	;defined in GWDATA.ASM
	externB B$LFPROG 	;defined in GWDATA.ASM
	externB B$RTPROG 	;defined in GWDATA.ASM
	externB B$PDIREC 	;defined in GWDATA.ASM
	externW B$GXPOS		;defined in GWDATA.ASM
	externW B$GYPOS		;defined in GWDATA.ASM
	externW B$GX_OLD 	;defined in GWDATA.ASM
	externW B$GY_OLD 	;defined in GWDATA.ASM

	externW B$BGTLOC 	;defined in GWDATA.ASM
	externB	b$Buf3		; > 16-byte buffer, defined in GWINI.ASM
	$BGTNUL EQU b$Buf3	
	externB B$TILFLG 	;defined in GWDATA.ASM
	externB B$TIPROG 	;defined in GWDATA.ASM
	externB B$TILLEN 	;GWDATA - length of fground tile string (0-rel)
	externB B$TILNDX 	;GWDATA - index to pixel of fground tile string
	externB B$TILPTR 	;GWDATA - byte ptr to fground tile string
	externW B$TILLOC 	;GWDATA - pointer to start of fground tile str
	externB B$TILHGT 	;GWDATA - height of foreground tile
	externB B$GRPLAN 	;GWDATA - number of graphics planes
	externW B$PQLEN		;defined in GWDATA.ASM
	externW B$PQNUM		;defined in GWDATA.ASM
	externW B$PQGET		;defined in GWDATA.ASM
	externW B$PQPUT		;defined in GWDATA.ASM

	externB b$Buf1		; foreground tile string buffer
	externB b$VTYP 	;defined in GLOBAL.INC



sEnd	_BSS			

	.RADIX	10

assumes CS,GR_TEXT		
sBegin	GR_TEXT 		


	PUBLIC	B$PAINTBEG
	externNP B$STALCTMP	
	externNP B$STALCMAXTMPNC ; Allocate max string w/o compaction

	externNP B$INVIEW
	externNP B$COORD1	
	externNP B$CLRATR

;low-level routines:
	externNP B$StoreC	
	externNP B$FetchC	
	externNP B$PaintInit	

	externNP B$GETFBC
	externNP B$TileMod	
	externNP B$STALCMAXTMP
	externNP B$STDALCTMP

	externNP B$SCINIT	; Performs screen initialization

	externNP B$ERR_FC
	externNP B$ERR_OM



	PAGE
	SUBTTL	PAINT ALGORITHM


B$PAINTBEG:			;Begin painting


	CALL	[b$PaintBound] ;Set Left and Right "C" for this line
	MOV	SI,1		;ENTRY COUNT IS ONE (SKIP NO BORDER)
	CALL	SCANR1		;SCAN RIGHT FROM INITIAL POSITION
	JZ	PNTEXT		;STARTED ON BORDER - GET TXTPTR & QUIT
	PUSH	BX		;SAVE NO. OF POINTS PAINTED TO RIGHT
	CALL	SCANL1		;NOW SCAN LEFT FROM INITIAL POS.
	POP	SI		;GET RIGHT SCAN COUNT.
	ADD	SI,BX		;ADD TO LEFT SCAN COUNT
	MOV	DL,64		;MAKE ENTRY FOR GOING DOWN
	CALL	ENTST1
	MOV	DL,0C0H 	;CAUSE PAINTING UP
	OR	DL,B$TILNDX	;Add Tile index
	JMP	SHORT STPAIN	;START PAINTING.
PNTEXT: 			;Exit PAINT statement


	MOV	B$TILFLG,0	;Turn Tiling off
	RET

;Register usage: (approximately)
;	In main paint loop: SI=B$SKPCNT, BX=B$MOVCNT, DL=Direction/Tile Index
;	In SCANR1,SCANL1:   AX=Word 1 of graphics cursor,BX=Word 2,CX=Word 3

;	MAIN PAINT LOOP

PNTLOP:
PNTLP1:
	CALL	GETQ		;Get one entry from queue
	CALL	B$StoreC	
STPAIN:
	MOV	AL,DL		;GET DIRECTION

	AND	AL,63		;Isolate Tile index
	MOV	B$TILNDX,AL	;and store it
	MOV	AH,B$GRPLAN	;get number of graphics planes
	MUL	AH		;product is in AX
	MOV	B$TILPTR,AL	;get real offset to fground tile string
	MOV	AL,DL		;reget direction byte
	AND	AL,0C0H 	;Isolate direction bits
	MOV	B$PDIREC,AL
	ADD	AL,AL		;SEE WHETHER TO GO UP, DOWN, OR QUIT
	JZ	PNTEXT		;IF ZERO, ALL DONE.
	JNB	PDOWN		;IF POSITIVE, GO DOWN FIRST
	CALL	[b$ChkUpC]	;MOVE UP BEFORE SCANNING
	JB	PNTLP1		;Off viewport - get next entry
	CALL	TILNDD		;Decrement tile index
	JMP	SHORT PNTLP2
PDOWN:
	CALL	[b$ChkDownC]	;SEE IF AT BOTTOM & MOVE DOWN IF NOT
	JB	PNTLP1		;Off viewport - get next entry
	CALL	TILNDI		;Increment tile index
PNTLP2:
	CALL	[b$PaintBound] ;Set Left and Right "C" for this line
	CALL	SCANR1		;SCAN RIGHT & SKIP UP TO B$SKPCNT BORDER
	JZ	PNTLP1		;IF NO POINTS PAINTED, GET NEXT ENTRY
	CALL	SCANL1		;NOW SCAN LEFT FROM START POINT
	MOV	SI,BX		;[SI] = LEFT B$MOVCNT
	TEST	B$TIPROG,255	;Is this a background tile?
	JNZ	TPROG1		;Yes: skip already painted check
	OR	CL,CL		;SEE IF LINE WAS ALREADY PAINTED
	JZ	PNTLP3		;IT WAS - DON'T MAKE OVERHANG ENTRY
TPROG1:
	CMP	BX,1		;SEE IF LEFT B$MOVCNT WAS .GT. 1
	JLE	PNTLP3		;No: go make entry for left+right move count
				;Yes: make entry in opposite direction for
				;overhang
	MOV	AL,B$PDIREC
	XOR	AL,128
	AND	AL,0C0H 	;Isolate new direction
	OR	AL,B$TILNDX	;Reverse direction and/or tile index
;++added 1 line (faster than doing it all in DL!)
	MOV	DL,AL
	CALL	B$FetchC	;Get current point address
	CALL	PUTQ
PNTLP3:
	ADD	SI,WORD PTR B$MOVCNT ;ADD COUNT PAINTED DURING RIGHT SCAN TO LEFT B$MOVCNT
	CALL	ENTSLR		;GO MAKE ENTRY.
;added 1 line
	MOV	CX,WORD PTR B$C3SAVE
	MOV	BX,WORD PTR B$C2SAVE ;SET CURRENT LOCATION BACK TO END
	MOV	AX,WORD PTR B$C1SAVE ;OF RIGHT SCAN.
	CALL	B$StoreC	
PNTLP4:
	MOV	SI,WORD PTR B$SKPCNT ;CALC B$SKPCNT - B$MOVCNT TO SEE IF
	SUB	SI,WORD PTR B$MOVCNT ;ANY MORE BORDER TO SKIP
	JZ	GOPLOP		;NO MORE - END OF THIS SCAN
	JB	PNTLP6		;RIGHT OVERHANG - SEE IF ENTRY NEEDED
	CALL	SCANR1		;HERE IF NEED TO CONTINUE RIGHT SCAN
	JZ	GOPLOP		;NO MORE POINTS.
	TEST	B$TIPROG,255	;Is this a background tile?
	JNZ	TPROG0		;Yes: enter regardless if already painted or not
	OR	CL,CL		;SEE IF LINE ALREADY PAINTED
	JZ	PNTLP4		;YES, DON'T ENTER ANYTHING
TPROG0:
	MOV	SI,BX		;ENTRY COUNT TO [SI]
;added 1 line
	MOV	CX,WORD PTR B$C3SAVE
	MOV	BX,WORD PTR B$C2SAVE ;MAKE ENTRY AT LOCATION SAVED BY ScanR
	MOV	AX,WORD PTR B$C1SAVE ;SO WE CAN ENTER A POSITIVE B$SKPCNT
	MOV	DL,B$PDIREC
	CALL	ENTSTK		;MAKE ENTRY
	JMP	SHORT PNTLP4	;CONTINUE UNTIL B$SKPCNT .LE. 0
PNTLP6:
	CMP	SI,WORD PTR -1	;If B$SKPCNT-B$MOVCNT .LT. -1
	JGE	GOPLOP		;No: overhang too small for entry
	NEG	SI		;Yes: Make right overhang entry
	DEC	SI		;     of at least one point wide overhang
	MOV	CX,SI		; LeftC protects CX
RTOVH1:
	CALL	[b$LeftC]	;START IS -(B$SKPCNT-B$MOVCNT)-1 TO LEFT
	LOOP	RTOVH1		
	MOV	AL,B$PDIREC	;MAKE ENTRY IN OPPOSITE DIRECTION
	XOR	AL,128
;++added 1 line (faster than doing it all in DL!)
	MOV	DL,AL
	CALL	ENTST1		;MAKE ENTRY
GOPLOP:
	JMP	PNTLOP		;GO PROCESS NEXT ENTRY
ENTSLR:
	MOV	AL,B$LFPROG	;DON'T STACK IF LINE ALREADY PAINTED
	OR	AL,B$RTPROG
	OR	AL,B$TIPROG
	JNZ	ENTST0
	RET			;Z IF SCAN LINE ALREADY PAINTED

ENTST0:
	MOV	DL,B$PDIREC
ENTST1:
	CALL	B$FetchC	;LOAD REGS WITH CURRENT "C"
ENTSTK:
	OR	DL,B$TILNDX	;Add tile index to direction
ENTST9:
	JMP	PUTQ
SCANR1:
;added 1 line
	MOV	DX,SI		;Can't change entry conditions for ScanR now...
	CALL	[b$ScanR]	;PERFORM LOW LEVEL RIGHT SCAN
	MOV	WORD PTR B$SKPCNT,DX ;SAVE UPDATED B$SKPCNT
	MOV	WORD PTR B$MOVCNT,BX ;SAVE B$MOVCNT
;added 3 lines
	MOV	WORD PTR B$C1SAVE,AX ;Save the "first non-BRDATR pixel
	MOV	WORD PTR B$C2SAVE,SI ; found during scan" in temp
	MOV	WORD PTR B$C3SAVE,DI
	OR	BX,BX		;SET CC'S ON B$MOVCNT
	MOV	AL,CL		;GET ALREADY-PAINTED FLAG FROM [C]
	MOV	B$RTPROG,AL
	RET
SCANL1:
	CALL	B$FetchC	;GET CURRENT LOCATION
;added 1 line
	PUSH	CX
	PUSH	BX		;AND SWAP WITH CSV
	PUSH	AX
;added 1 line
	MOV	CX,WORD PTR B$C3SAVE
	MOV	BX,WORD PTR B$C2SAVE
	MOV	AX,WORD PTR B$C1SAVE
	CALL	B$StoreC	;REPOS AT BEGINNING OF SCAN
	POP	AX		;REGET PLACE WHERE RT SCN STOPPED
	POP	BX
;added 2 lines
	POP	CX
	MOV	WORD PTR B$C3SAVE,CX
	MOV	WORD PTR B$C2SAVE,BX ;AND SAVE IT IN TEMP LOCATION
	MOV	WORD PTR B$C1SAVE,AX
	CALL	[b$ScanL]	;NOW DO LOW LEVEL LEFT SCAN
	MOV	AL,CL		;GET ALREADY-PAINTED FLAG FROM [C]
	MOV	B$LFPROG,AL	;WHETHER IT WAS ALREADY PAINTED (faster than
				;  doing MOV B$LFPROG,CL!)
	RET

	PAGE
	SUBTTL	PAINT AND TILE SUPPORT ROUTINES
; Little helpers for PAINT and TILE

;
;FGSTRINI		initialize foreground string
;
;Purpose:
;	  Check string for validity:  its length must be less than 256 and
;	  greater than 0.  Fill string buffer b$Buf1 with paint tile padded on the
;	  right with zeros.  Initialize B$TILLOC to point at b$Buf1.  Initialize
;	  B$GRPLAN to the number of graphics planes.  Stores rounded up tile
;	  length in B$TILLEN and pixel height of tile in B$TILHGT (must be less
;	  than or equal to 64).
;
;Entry:
;		BX = ptr to string descriptor
;
;Exit:
;	variables updated as noted
;
;Modifies:
;		CX,SI,DI
;

FGSTRINI:			;initialize paint string
	MOV	CX,WORD PTR 0[BX] ;[CX]= len
	MOV	SI,WORD PTR 2[BX] ;[SI]= addr
	OR	CH,CH		;Is CH=0?
	JNE	MATERR		;Length must be .LE. 255
	MOV	DI,OFFSET DGROUP:b$Buf1 ;DI points to b$Buf1
	MOV	WORD PTR B$TILLOC,DI ;Store adr
	PUSH	CX		;Save string length
	CLD			;Clear direction flag
				;Repeat until CX=0:
	REP	MOVSB		;Put Fill String in b$Buf1
	POP	CX		;Restore  string length
	PUSH	CX		;Resave string length
	SUB	CX,256d 	;CX:=CX-256
	NEG	CX		;CX:=-CX=remainder of BUF
	XOR	AL,AL		;Clear AL
				;Repeat until CL=0:
	REP	STOSB		;Fill the rest of BUF with zeros
;tile string is now saved in the temp buffer so temp string can be deallocated
	CALL	B$STDALCTMP	; deallocate temp string pointed to by BX
;Check tile height
	CALL	B$TileMod	;Get number of graphics planes
	POP	CX		;Restore CL
	MOV	B$GRPLAN,AL	;in this screen mode
	JCXZ	MATERR		;See if at least one byte in tile string
	XCHG	AX,CX		
	DEC	AX
	DIV	CL		;AX=(B$TILLEN-1)/B$GRPLAN
	XOR	AH,AH
	INC	AX		;AX=INT((B$TILLEN-1)/B$GRPLAN)+1=height in pixels rounded up
	CMP	AL,65
	JNB	MATERR		;Tile can only be 64 pixels long (IBM compatibility)
	MOV	B$TILHGT,AL	;Save pixel height of tile (always.LE.64)
	MUL	CL		;AX=INT(((B$TILLEN-1)/B$GRPLAN)+1)*B$GRPLAN
	DEC	AX		;make 0 based, guaranteed 1 or more
	OR	AH,AH		;Rounded up length must be .le. 255
	JNZ	materr		;Yes: this must be a multiplane with four or more planes
	MOV	B$TILLEN,AL	;Store rounded up length
	RET			;return to caller

MATERR:
	MOV	B$TILFLG,0	;Turn Tiling off
	JMP	B$ERR_FC

;***
; BGSTRINI - initialize background tile string
; Purpose:
;	Insures that string length is between 1 and 255 inclusive.
;	Initializes background string to specified background string.
;
; Entry:
;	BX = ptr to string descriptor
;	$BGTNUL = ptr to background tile string area
; Exit:
;	None.
; Modifies:
;	SI,DI,CX
;****

BGSTRINI:
	MOV	CX,[BX] 	;[CX]= len
	OR	CH,CH
	JNE	materr		;Max len is 255
	JCXZ	materr		;See if at least one byte in background string
	CMP	CL,B$GRPLAN	;Ignore any extra bytes
	JBE	FILL_BGSTR
	MOV	CL,B$GRPLAN
FILL_BGSTR:			;Fill background string
	MOV	SI,WORD PTR 2[BX] ;[SI]= addr
	MOV	DI,OFFSET DGROUP:$BGTNUL ;Point DI at backgrd tile area
	CLD			;Clear direction flag
	REP	MOVSB		;Fill with as much as user gave,
				;rest are chr$(0)
	JMP	B$STDALCTMP	; deallocate temp string pointed to by BX

;***
; B$ClrBgStr - clear background string area
; Purpose:
;	Initialize background string area by filling with zeros.
;	Make sure there are no more than 16 graphics planes.
;
; Entry:
;	$BGTNUL points to background string area
; Exit:
;	None.
; Modifies:
;	CX,DI,AX.
;****

cProc	B$ClrBgStr,<NEAR>	;Clear background string
cBegin				
	MOV	CX,16
	CMP	CL,B$GRPLAN	;Only allocated space for at most 16 planes
	JB	MATERR		;function call error
	MOV	DI,OFFSET DGROUP:$BGTNUL
	XOR	AL,AL
	CLD
	REP	STOSB		;Fill 16 bytes with CHR$(0)
	MOV	B$BGTLOC,OFFSET DGROUP:$BGTNUL ;No BG tile, use null (00)
cEnd				

;	Set starting tile index to Starting Y axis and
;	store in B$TILNDX.
;	Set B$TILNDX to (Y MOD pixel height)*B$GRPLAN
;
;Entry - [DX] = Starting Y line
;
;	Uses:	[BX]

cProc	B$SetTileIndex,<NEAR>	
cBegin				
	CMP	B$TILFLG,0
	JZ	STIND2		;nothing if not tile
	PUSH	AX
	PUSH	DX		;Protect DX
	MOV	BX,DX		;[BX]=Y coord
	MOV	DL,B$TILHGT	;[DX]= height of tile
	XOR	DH,DH
STIND1:
	SUB	BX,DX
	JNB	STIND1		;Find Y MOD pixel height
	ADD	BX,DX		;+len since overflowed
	MOV	B$TILNDX,BL	;get index of tile pixel
	MOV	AL,B$GRPLAN
	XOR	AH,AH
	MUL	BL		;AX=(Y MOD pixel height)*B$GRPLAN
	MOV	B$TILPTR,AL	;store fground tile string offset
	POP	DX		;get  Y coord
	POP	AX
STIND2:
cEnd				

;	Scan Fill argument,  set Fill color attr if numeric
;	or set up tile attr if string.
;	If string, check that its length .LE. 64*number of graphics planes
;	and round up length to even multiple of number of graphics planes.

; MATCH3	Checks 3 consecutive lines in TILE$ for match
;		with the background color or tile.  If 3 lines
;		are there, then give Illegal Function Call error.
;		If this was not done, PAINT might infinite loop.

; Uses	AX,BX,CX,DX,SI,DI

MATCH3:
	CMP	B$TILFLG,0
	JZ	MATC3X		;Do nothing if Tile wasn't given
	XOR	DH,DH
	MOV	DL,B$GRPLAN	;DX=tile "width"
	MOV	SI,WORD PTR B$TILLOC ;SI=pointer to tile string
	XOR	AH,AH
	MOV	AL,B$TILLEN
	ADD	AX,SI		;AX=end of tile string
	CLD
MAT3LP:
	CMP	SI,AX		;Done entire tile string yet?
	JA	MATC3X		;Yes:
	MOV	DI,WORD PTR B$BGTLOC
	CALL	CMPST		;Matched 3 tiles in a row?
	JE	PNTERR		;Yes: barf
	ADD	SI,DX		;No: look at next set in tile string
	JMP	SHORT MAT3LP	;Loop
MATC3X:
	RET

CMPST:
	PUSH	SI		;Save tile string pointer
	MOV	BX,3
	PUSH	ES		
	PUSH	DS		
	POP	ES		
CMPST2:
	MOV	CL,DL		;Look for 3 in a row matches of B$GRPLAN bytes
	XOR	CH,CH
CMPST0: CMPSB			;Compare [SI],[DI]
	JNE	CMPST1		;Different already, exit with Z clear
	CALL	WRAPSI
	LOOP	CMPST0		;Matched B$GRPLAN in a row yet?
	MOV	DI,WORD PTR B$BGTLOC ;Yes: reset background pointer
	DEC	BX		;Matched 3 sets of B$GRPLAN in a row?
	JNZ	CMPST2		;No:
				;Yes: exit with Z set
CMPST1:
	POP	ES		
	POP	SI
	RET

WRAPSI:
	CMP	SI,AX		;Wrap around to beginning of tile string yet?
	JBE	NoWrap		;No:
	MOV	SI,WORD PTR B$TILLOC ;Yes: back to go
NoWrap: RET

;***
; TILATR
;	Set ATRBYT from tile and set B$TIPROG if current tile
;	attribute equals BG tile attribute.
; USES BX,DI
;****

TILATR:
	CMP	B$TILFLG,0
	MOV	BL,B$TILPTR
	JNZ	B$TILAT1	;Update ATRBYT
	RET			;do nothing if not tile

;***
; TILNDD
;	Decrement Tile index
;	and store tile in ATRBYT if B$TILFLG True...
; USES AX,BX
;****

TILNDD:
	CMP	B$TILFLG,0
	JNZ	TILNDD1 	;do nothing if not tile
	RET
TILNDD1:
	DEC	B$TILNDX 	;index is one less
	MOV	BL,B$TILPTR	;get offset to fground tile string
	OR	BL,BL		;At beginning of tile string?
	JNZ	TILDN1		;Brif not underflowing
	MOV	BL,B$TILHGT	;get tile height
	DEC	BL		;make 0-relative
	MOV	B$TILNDX,BL	;put into index
	MOV	BL,B$TILLEN	;else wrap to top end
	INC	BL		;make 1 based
TILDN1:
	SUB	BL,B$GRPLAN	;Decrement index to beginning of prev tile
	JMP	SHORT TILND2	;Update index, ATRBYT


PNTERR:
	MOV	B$TILFLG,0	;Turn Tiling off
	JMP	B$ERR_FC


;***
; TILNDI
;	Increment Tile index (called after DOWNC)
;	and store tile in ATRBYT if B$TILFLG True...
; USES AX,BX
;****
TILNDI:
	CMP	B$TILFLG,0
	JNZ	TILNDI1 	;do nothing if not tile
	RET
TILNDI1:
	INC	B$TILNDX 	;increment tile pixel index
	MOV	BL,B$TILPTR	;get fground tile byte offset
	ADD	BL,B$GRPLAN	;Increment index to beginning of next tile
	CMP	BL,B$TILLEN
	JBE	TILND2		;Brif not overflowing
	XOR	BL,BL		;else wrap to bottom end
	MOV	B$TILNDX,BL	;also clear tile pixel index
TILND2:
	MOV	B$TILPTR,BL	;update tile byte offset pointer

;***
; B$TILAT1
;
;****

cProc	B$TILAT1,<NEAR>,<SI>	
cBegin				
	MOV	BH,0		;[BL]=[BL]*GRPLAN, [BH]=0
	ADD	BX,WORD PTR B$TILLOC
	MOV	SI,BX		;SI points into tile string
	MOV	DI,WORD PTR B$BGTLOC ;DI points into background string
	MOV	B$TIPROG,0	;Clear progress flag
	XOR	BH,BH
TILAT2: INC	BH
	CMP	BH,B$GRPLAN
	JA	TILAT4		;For BH = 1 to gplanes
	MOV	BL,[SI] 	;Get curr tile attr
	PUSH	BX
	CALL	[b$SetTile]	;Set OEM's 'ATRBYT' to tile attr
	POP	BX
	CMP	BL,[DI] 	;Compare FG attr to BG attr
	JNZ	TILAT3		;Brif not equal
	INC	B$TIPROG 	;Count matches
TILAT3:
	INC	SI		;Check next planes bytes
	INC	DI
	JMP	SHORT TILAT2	;next BH
TILAT4:
	MOV	BL,B$TIPROG
	MOV	B$TIPROG,0
	CMP	BL,B$GRPLAN	;Did all FG match BG?
	JNE	TILAT6		;No:  not all matched so say none did.
	MOV	B$TIPROG,1	;Yes: flag all matched
TILAT6:
cEnd				


	PAGE
	SUBTTL	PAINT QUEUE ROUTINES

cProc	B$INTQNOCPCT,<PUBLIC,NEAR>,<BX,DX>	
cBegin						

;	PUSH	BX		;save registers...
;	PUSH	DX		
;	cCALL	B$STGETFRESIZ	;get size of free string
;	OR	BX,BX		;test if no free string
;	JZ	PQOMER		;if not, give out of memory error
;	cCALL	B$STALCTMP	; Allocate all string space as temporary

	cCALL	B$STALCMAXTMPNC	; Allocate max-string w/o compaction
	JMP	SHORT INTQUE_COMMON ;jump to common code point

cEnd	<nogen>			; End of B$INTQNOCPCT

	PUBLIC	B$INTQUE

B$INTQUE:
	PUSH	BX		;Save x coord
	PUSH	DX		;SAVE Y COORD
	CALL	B$STALCMAXTMP	;allocate maximum temporary string
INTQUE_COMMON:			
	MOV	AX,0[BX]	;AX:=size of free string space
	MOV	CX,2[BX]	;CX:=location of free string space
	CMP	AX,9		;is temp string too small?
	JB	TEMP_TOO_SMALL	;if so, do not subtract
	SUB	AX,9		;give string room for test
TEMP_TOO_SMALL:

;Note: B$GY_OLD is the previous x coord accumulator.  Since this variable is
;never used in the PAINT statement, it is used here to save the location
;of the temporary string being used for the paint queue.
	MOV	B$GY_OLD,CX	;save string location in B$GY_OLD
	ADD	CX,AX		;CX points just past end of free string
	DEC	CX		;CX=highest byte of in-use string space
				;omit check for overflow
;Note: B$GX_OLD is the previous y coord accumulator.  Since this variable is
;never used in the PAINT statement, it is used here to save the pointer to
;the end of the temporary string being used for the paint queue.
	MOV	B$GX_OLD,CX	;Save this value in B$GX_OLD
	CALL	B$STDALCTMP	;mark temp string as deallocated
	CMP	AX,18d
	JNB	INTQU3		;Ok if at least 18 bytes (2 entries)
PQOMER:
	JMP	B$ERR_OM	  ; else Out of Memory
INTQU3:
	MOV	WORD PTR B$PQLEN,AX ;len is free size
	MOV	WORD PTR B$PQNUM,0 ;present number is 0
	XCHG	AX,DX		;AX:=beginning of usable free space
	MOV	WORD PTR B$PQGET,AX ;Init head
	MOV	WORD PTR B$PQPUT,AX ; and tail
	POP	DX		;Restore y coord
	POP	BX		;Restore X coord
	RET

; PUTQ - Enqueue entry in paint queue
; Entry - AX,BX,CX = OEM's graphics cursor
;	  SI = B$SKPCNT
;	  DL = Direction (and tiling) flag
; Exit	- B$PQNUM,B$PQPUT updated
; AX,DI Used
DbPub	PUTQ			
PUTQ:
	PUSH	ES		
	PUSH	DS		
	POP	ES		
	PUSH	DX
;added 1 line
	PUSH	SI
	PUSH	CX
	PUSH	BX
;added 1 line
	PUSH	AX
	ADD	WORD PTR B$PQNUM,9d ;len= len+9
	MOV	AX,WORD PTR B$PQLEN
	CMP	WORD PTR B$PQNUM,AX
	JNB	PQOMER		;Out of Memory if at end or higher
	MOV	AX,WORD PTR B$PQPUT ;[AX]= queue ptr
	CALL	WRAP		;check wrap
	MOV	DI,AX		;[DI]= queue ptr
	POP	AX
	CLD
	STOS	WORD PTR [SI]	;Graphics cursor
	POP	AX
	STOS	WORD PTR [SI]	;Graphics cursor
	POP	AX
	STOS	WORD PTR [SI]	;Graphics cursor
	POP	AX
	STOS	WORD PTR [SI]	;B$SKPCNT
;added 2 lines
	POP	AX
	STOSB			;Direction flag
	MOV	WORD PTR B$PQPUT,DI
	POP	ES		
	RET

WRAP:
	PUSH	AX


	ADD	AX,9
;Note: B$GX_OLD is the previous x coord accumulator.  Since this variable is
;never used in the PAINT statement, it is used here to save the location
;of the temporary string being used for the paint queue.
	CMP	AX,WORD PTR B$GX_OLD ;Are we at end of free space?

	POP	AX		;Restore AX
	JB	WRAPX		;Brif not off end of queue


;Note: B$GY_OLD is the previous y coord accumulator.  Since this variable is
;never used in the PAINT statement, it is used here to save the pointer to
;the end of the temporary string being used for the paint queue.  This
;value is set at the beginning of B$INTQUE.
	MOV	AX,B$GY_OLD	;else set to beginning of queue

WRAPX:
	RET

; GETQ - Dequeue entry from paint queue
; Entry - none
; Exit	- AX,BX,CX = OEM's graphics cursor
;	  SI = B$SKPCNT
;	  DL = Direction (and tiling) flag
;	  B$PQNUM,B$PQGET updated
; No other registers used.
DbPub	GETQ			
GETQ:
	MOV	DL,0		;preset "Direction=0" in case no entries
	CMP	WORD PTR B$PQNUM,0
	JZ	GETQX		;Brif empty
	SUB	WORD PTR B$PQNUM,9d ;len = len-9
	MOV	AX,WORD PTR B$PQGET ;[AX]= dequeue ptr
	CALL	WRAP		;check wrap
	MOV	SI,AX		;[SI]= dequeue ptr
;added 3 lines
	CLD
	LODS	WORD PTR [SI]	;Graphics cursor
	PUSH	AX
	LODS	WORD PTR [SI]	;Graphics cursor
	MOV	BX,AX
	LODS	WORD PTR [SI]	;Graphics cursor
	MOV	CX,AX
	LODS	WORD PTR [SI]	;B$SKPCNT
	PUSH	AX
;added 2 lines
	LODSB			;Direction flag
	MOV	DL,AL
	MOV	WORD PTR B$PQGET,SI
;added 2 lines
	POP	SI		;SI = B$SKPCNT
	POP	AX
GETQX:
	RET

	PAGE
	SUBTTL	COMPILER ENTRY POINTS FOR PAINT

; PAINT - FILL AN AREA WITH COLOR
;
; SYNTAX: PAINT [STEP](X,Y)[,<iexp>[,iexp]]|[,<sexp>[,<iexp>[,<sexp>]]]
;	where <iexp>=integer expression, <sexp>=string expression
;

;***
;B$PAIN - Process PAINT statement without tiling
;
;Purpose:
;	This routine will be called by the parsing routine PAINT in PRSG86
;	in the interpreter version or by the compiler in the compiler version
;	if the PAINT statement scanned does not contain a paint tile
;	attribute but uses a integer paint color instead.  The call to
;	BNDS_CHK tests the start coordinates of the PAINT against the current
;	viewport boundaries.  The paint and border colors are checked for
;	validity before the jump to B$PAINTBEG begins the actual painting.
;
;Entry:
;	Color	= paint color spec or -1 if default
;	Border	= border color spec or -1 if default
;Exit:
;	None.
;Uses:
;	Per convention
;Exceptions:
;	Control may be transfered to B$ERR_FC
;****
cProc	B$PAIN,<PUBLIC,FAR>,<SI,DI> 
parmW	Color			
parmW	Border			
cBegin				

	CALL	B$SCINIT	; init screen if not already done
	cCall	B$COORD1	;do any necessary translations
	MOV	BX,Color	;[BX] = paint color
	MOV	DX,Border	;[DX] = border color
	CALL	OLD_PA1 	; Perform
cEnd				


OLD_PA1:			; Entry point for PAINT with no tiling
	MOV	B$TILFLG,0	;tiling is not active
	MOV	B$TIPROG,0	
	CALL	B$INTQUE 	;Init the Queue
	PUSH	DX		;Save border color
	CALL	BNDS_CHK	;Check if coordinates are inside
				;screen or viewport
	JC	MAPIT		
	JMP	DONT_PAINT	;Don't PAINT if coordinates out of bounds
MAPIT:				
	push	bx		;preserve bx across call to MapXYC
	CALL	[b$MapXYC]	;Graphics Cursor:=(CX,DX)
	pop	ax		;restore paint color
	POP	DX		;Restore border color
	CALL	B$CLRATR 	;Returns paint color attribute in AL
	CMP	DX,-1		;Is border color defaulted?
	JZ	BRDCHK		;Yes: AL contains valid paint color which is
				;border color default when no tiling
	MOV	AX,DX		;AX:=border color
BRDCHK: 			;check for valid border color
	CALL	B$PaintInit	;OEM check for legal border color
	JC	ERR_PAINT	;carry set if invalid border color
	JMP	B$PAINTBEG	;Go begin painting

;***
;B$PNTC - Process PAINT statement with tiling option
;
;Purpose:
;	This routine will be called if the PAINT statement scanned contains
;	a paint tile attribute instead of an integer paint color.  The
;	call to BNDS_CHK checks to see if the PAINT coordinates are within
;	bounds of the current viewport (the screen if there is no viewport).
;	The call to FGSTRINI initializes a buffer area (b$Buf1) to the tile
;	specified in the PAINT statement or to the default value.  B$ClrBgStr
;	initializes the default background string.  TILEINIT checks for a
;	valid border color, initializes B$TILNDX to index the appropriate byte
;	of the tile string, initializes ATRBYT (variable containing tile
;	mask) and checks for three consecutive matches between the foreground
;	and background tiles.  If everything is ok the jump to B$PAINTBEG is
;	taken to begin the actual painting procedure.
;Entry:
;	sdTile = sd for tiling string
;	Border = border spec or -1 if default
;	sdBkgd = sd for background string or -1 if none
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	B$ERR_FC
;****
cProc	B$PNTC,<PUBLIC,FAR>,<ES,SI,DI>	
parmSD	sdTile			
parmW	Border			
parmSD	sdBkgd			
cBegin				

	CALL	B$SCINIT	; init screen if not already done

	cCall	B$COORD1	;do any necessary translations
	GetpSD	BX,sdTile	;[BX] = psdTile
	GetpSD	CX,sdBkgd	;[CX] = psdBkgd
	MOV	DX,Border	;[DX] = border color
	PUSH	DS		; set es=ds
	POP	ES		
	CALL	OLD_PA2 	; Perform

cEnd				

ERR_PAINT:
	MOV	B$TILFLG,0	;Turn Tiling off
	JMP	B$ERR_FC


OLD_PA2:			; Entry point for PAINT with tiling
	MOV	B$TILFLG,-1	;Tile is active
	PUSH	DX		;Save border color
	PUSH	CX		;Save ptr to backgrd string descriptor
	CALL	B$INTQUE 	;Init the Queue
	CALL	BNDS_CHK	;Check if coordinates are inside screen or viewport
	JNC	POPNRET 	;coords out of bounds: don't do PAINT
	push	bx		;preserve bx across mapxyc call
	CALL	[b$MapXYC]	;Graphics Cursor:=(CX,DX)
	pop	bx		;restore ptr to fg string desc
	CALL	FGSTRINI	;Initialize foreground paint string
	POP	BX		;Restore ptr to background string descriptor
	CALL	B$ClrBgStr	;initialize null string for background
	CMP	BX,-1		;is background string defaulted?
	JZ	TILEINIT	;yes: branch to tile ATRBYT initialization
	CALL	BGSTRINI	;initialize background string
	CALL	MATCH3		;check for three consecutive matching
				;bytes in paint tile and background
				;tile and abort if found
	JC	ERR_PAINT	;Three matches found: issue error
TILEINIT:			;Tile initialization
	POP	AX		;Restore border color attribute
	CMP	AX,-1		;Is border color defaulted?
	JNZ	BORDER_CHK	;NO: Go check for valid border color
	STC			;Set carry
	CALL	B$GETFBC 	;Get foreground color in AL
BORDER_CHK:
	CALL	B$PaintInit	;OEM routine to check for valid border color
	MOV	DX,WORD PTR B$GYPOS ;DX:=current y coord
	CALL	B$SetTileIndex ;set B$TILNDX to proper byte of tile string
	CALL	TILATR		;set up tiling mask in ATRBYT
	JMP	B$PAINTBEG	;Valid border: go begin painting
POPNRET:			;Pop stack and abort PAINT statement
	POP	CX		;pop backgrd string ptr
DONT_PAINT:
	POP	DX		;pop border color
	MOV	B$TILFLG,0	;clear tiling flag
	RET			;Return

;***
; BNDS_CHK - routine checks for valid PAINT coordinates,
; Purpose:
;	To see if the coordinates specified in the PAINT statement
;	are within the current viewport.  May be called from B$PAIN
;	or B$PNTC.
;
;	If clipping is supported:
;		If the coordinates are inside the viewport, then continue
;		to process the PAINT statement, otherwise, return with
;		carry clear to indicate that PAINT statement should be
;		aborted without issuing an error message.
;	If clipping is not supported:
;		If the coordinates are inside the viewport, then continue
;		to process the PAINT statement, otherwise, issue a
;		function call error.
;
; Entry:
;	None.
; Exit:
;	Graphics cursor updated if coordinates inside current viewport.
; Modifies:
;	AX,CX,DX.
;****

BNDS_CHK:
	MOV	CX,WORD PTR B$GXPOS ;CX:=Graphics accumulator x position
	MOV	DX,WORD PTR B$GYPOS ;DX:=Graphics accumulator y position
	PUSH	BX		;Save paint parameter
	CALL	B$INVIEW 	;Returns carry set if coords within
				;current viewport
	POP	BX		;Restore paint parameter
	RET			;return with carry flag


sEnd	GR_TEXT 		
	END
