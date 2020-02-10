	TITLE	GWDATA - Global Data Declarations for GW BASCOM
;***
; GWDATA - Global Data Declarations for GW BASCOM
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	

	useSeg	DV_TEXT 	
	useSeg	_DATA		
	useSeg	_BSS		
	useSeg	CONST		
	useSeg	BR_SKYS 	

	INCLUDE seg.inc 	
	INCLUDE	comdcb.inc	

sBegin	_DATA			

;***
;b$CURSOR - screen cursor position (1,1-relative)
;OEM-callback routine (variable)
;
;Purpose:
;	b$CURSOR keeps track of the current cursor position.
;	The low byte is the X coordinate (1-relative) and
;	the high byte is the Y coordinate (1-relative).
;
;	This value has to be set by the OEM at startup time.
;
;Allocation:
;	b$CURSOR is a WORD value declared in the _DATA segment
;	by the OEM-independent code.
;
;Values:
;	1	  <= b$CURSOR low byte <= b$CRTWIDTH (+1)
;	b$WDOTOP <= b$CURSOR hi byte	<= b$WDOBOT
;
;	There is a case where the value of b$CURSOR will not reflect
;	the actual cursor position.  This occurs when we have printed
;	a character to the last column of the screen and have incremented
;	the cursor.  In this case b$CURSOR will indicate column
;	b$CRTWIDTH + 1, and the cursor will be positioned at column
;	b$CRTWIDTH.  This is to keep from scrolling the screen when
;	writing to the bottom right most corner.
;
;Initially Set:
;	This value should be set by the OEM during initialization of
;	the screen I/O variables (currently done in B$GWINI).
;
;Modified by:
;	The value of b$CURSOR is updated anytime a routine moves the
;	cursor on the screen.  It is not done when the cursor is actually
;	moved, as several of the routines need b$CURSOR to stay fixed (as
;	an initial location) while they move the cursor around.  The OEM
;	routines should therefore NOT modify this value except upon
;	initialization.
;
;Used by:
;	Any routine that wants to know the position of the cursor
;	according to the OEM independent code.	If the position of
;	cursor must be known exactly, then it is suggested to obtain
;	this value from the hardware.
;*****************************************************************************

;	b$CSRY and b$CSRX must remain in this order and of this size.

	labelW	<PUBLIC,b$CURSOR> ; screen cursor (1,1)-relative.
	globalB b$CSRY,1	; cursor row
	globalB b$CSRX,1	; cursor column

	globalB b$LINCNT,24	;max lines on screen (logical is WDOBOT)
				; Set by calls to B$SCNSWI.
	globalB b$SCRNWIDTH,80 ; logical width of last screen
				; width change. (physical is B$CRTWIDTH)

;***
;b$WDOTOP, b$WDOBOT - Logical top and bottom lines of the screen.
;OEM-callback routine (variables)
;
;Purpose:
;	These variables define the logical top and bottom of the text
;	window (1-relative).
;
;Allocation:
;	b$WDOTOP, b$WDOBOT are BYTE values declared in the _DATA segment
;	by the OEM-independent code.
;
;Values:
;	Initially, b$WDOTOP = 1 and b$WDOBOT = 24.  Updated as needed.
;
;Initially Set:
;	b$WDOTOP and b$WDOBOT are statically initialized.
;
;Modified by:
;	These variables are modified by various OEM-independent routines
;	associated with VIEW PRINT, SCREEN, and other functions that modify
;	the size of the screen.  They should not be modified or changed
;	by the OEM.
;
;Used by:
;	The only OEM routines that should need access to these variables
;	is the clear screen routine (B$CLRSCN).
;*************************************************************************

	globalB b$WDOTOP,1
	globalB b$WDOBOT,24	  ;Logical screen last line
				;(LINCNT is physical)
	labelW <PUBLIC,b$ERDEVP>		; sd for errors
	staticW ,,1				;string desc for errors
	staticW ,<OFFSET DGROUP:ERDEVP_STR>

;    graphics routine pointers only set when a graphics
;    statment is executed so non-graphics program are smaller.

	globalW b$VWINI_PTR, <DV_TEXTOFFSET B$NearRet>
	globalW B$GRPINI_PTR,<DV_TEXTOFFSET B$NearRet>
	globalW B$GRPRST_PTR,<DV_TEXTOFFSET B$NearRet>

	globalW b$VWCLR_PTR,<DV_TEXTOFFSET B$NearRet>

	globalW b$pTrapEvent,<DV_TEXTOFFSET B$NearRet>	

;***
; b$CRTWIDTH - Current Physical Screen Width
;OEM-callback routine (variable)
;
;Purpose:
;	This variable specifies the current physical width of the screen.
;	It will also give the rightmost column of the screen (1-relative).
;
;	This variable should only be changed by the OEM by calling B$SCNSWI.
;
;Allocation:
;	b$CRTWIDTH is a BYTE value declared in the _DATA segment by the
;	OEM-independent code.
;
;Values:
;	Either 40 or 80, depending on screen mode.
;
;Initially Set:
;	b$CRTWIDTH is statically initialized to 80.
;
;
;Modified By:
;	The value of b$CRTWIDTH should only be modified by calling B$SCNSWI.
;	Note that the WIDTH function will also modify this value directly.
;
;Used By:
;	Any routine (OEM-dependent or OEM-independent) that needs to know
;	the current size of the screen.
;*****************************************************************************

	globalB b$CRTWIDTH,80	; Characters per line (40 or 80 allowed)

	globalD b$RndVar,050000H,1 ; Random number generator seed.

	externB	b$Buf1		; 256+2 bytes of temporary storage

	EVEN			; SD's must be word-aligned
labelW <PUBLIC,b$SDBuf1>	; sd to b$Buf1
	staticW	,0			; size
	staticW	,<OFFSET DGROUP:b$Buf1> ; address of data


	globalB b$PRFG,0	; print control flag.  See PRNVAL.ASM
				; for the legal bit values.

sEnd	_DATA			

sBegin	_BSS			


;	MACLNG variables for DRAW

	globalB B$DRWSCL,,1	;DRAW: SCALE
	globalB B$DRWFLG,,1	;DRAW flag
	globalB B$DRWANG,,1	;DRAW "ANGLE" (0..3)

	globalD B$COSA,,1	;Cosine(Rad(ang))
	globalD B$MSINA,,1	;-Sin(Rad(ang))*Aspect
	globalD B$DSINA,,1	;Sin(Rad(ang))/Aspect


;
	globalW B$MCLPTR,,1	;MAC LANG PTR
	globalW B$MCLLEN,,1	;STRING LENGTH
	globalW B$MCLTAB,,1	;PTR TO COMMAND TABLE
	globalW B$MCLPSD,,1	; PTR TO STRING DESCRIPTOR

B$LENDRW EQU	B$MCLPTR-B$DRWSCL 
	PUBLIC	B$LENDRW 	



;	From GWUND.ASM in interpreter:

	EVEN

	globalW B$X_BASE,,1	;Integer x base value for rel coords
	globalW B$Y_BASE,,1	;Integer y base value for rel coords

; To help speed up the point function, the following variables must be
; contiguous (can be in any order) so a block copy onto the stack can
; be done instead of a zillion pushes.

	labelW	<PUBLIC,B$GRAFACC> ; Graphics accumulator block
	globalW B$GRPACX,,1	;previous X Coordinate
	globalW B$GRPACY,,1	;previous Y Coordinate

;	B$GX_OLD, B$GY_OLD must be contiguous in memory for circle code

	globalW B$GX_OLD,,1	;X Position of First Coordinate
	globalW B$GY_OLD,,1	;Y Position of First Coordinate
	globalW B$GXPOS,,1	;X Position of Second Coordinate
	globalW B$GYPOS,,1	;Y Position of Second Coordinate

	globalB B$DFRACX,,1	;8 bit fraction x after
				;angle transform (*)Order!
	globalB B$DFRACY,,1	;8 bit fraction y after
				;angle transform (*)


;	The following two variables must be consecutive in memory.

	globalD B$GRFACX,,1	;Logical Graphic acc x
	globalD B$GRFACY,,1	;Logical Graphic acc y



	labelW	<PUBLIC,B$LASTACC>
	PUBLIC	B$SIZEACC
B$SIZEACC EQU	(B$LASTACC-B$GRAFACC)/2 ;external constant

	globalW B$MAXUPD,,1	;Address of Major Axis Move Update
	globalW B$MINUPD,,1	;Address of Minor Axis Move Update
	globalW B$MAXDEL,,1	;Largest Delta for Line
	globalW B$MINDEL,,1	;Smaller of 2 Deltas for Line

	globalW B$LINSTL,,1	;Line style

				; speed optimization for circle
	globalB B$CLIPF,,1	; Clipping flag
	globalB B$CLINEF,,1	;LINE-TO-CENTER FLAG
	globalW b$ASPECTR,,1	;ASPECT RATIO
	globalW B$CENCNT,,1	;END CIRCLE POINT COUNT
	globalW B$CNPNTS,,1	;1/8 NO. OF PTS IN CIRCLE
	globalW B$CPCNT,,1	;1/8 NO. OF PTS IN CIRCLE
	globalW B$CPCNT8,,1	;NO. OF PTS IN CIRCLE
	globalW B$CRCSUM,,1	;CIRCLE SUM
	globalW B$CSTCNT,,1	;START COUNT
	globalB B$CPLOTF,,1	;PLOT FLAG
	globalB B$COPTFL,,1	;FLAG WHETHER ASPECT WAS .GT. 1
	globalD B$A_START,,1	;FLTING PT. START ANGLE FOR CIRCLE STMT
	globalD B$A_END,,1	;FLTING PT. END ANGLE FOR CIRCLE STMT


	globalW B$C1SAVE,,1	;ADVGRP C save area
	globalW B$C2SAVE,,1	;ADVGRP C save area
	globalW B$C3SAVE,,1	;ADVGRP C save area
	globalW B$CXOFF,,1	;X OFFSET FROM CENTER SAVE LOC
	globalW B$CYOFF,,1	;Y OFFSET SAVE LOCATION


	globalB B$LFPROG,,1	;PAINT: SCAN LINE ALREADY PAINTED FLAGS
	globalB B$RTPROG,,1
	globalW B$SKPCNT,,1	;SKIP COUNT
	globalW B$MOVCNT,,1	;MOVE COUNT
	globalB B$PDIREC,,1	;PAINT DIRECTION

	EVEN
	globalB B$TILFLG,,1	;Tiling on/off flag
	globalB B$TIPROG,,1	;Tile progress flag
	globalB B$TILNDX,,1	;TILE TABLE INDEX (Y COORD MOD 64)
	globalB B$TILLEN,,1	;TILE LENGTH
	globalW B$TILLOC,,1	;TILE STRING'S ADDRESS
	globalW B$BGTLOC,,1	;Background tile STRING'S ADDRESS
	globalB B$TILHGT,,1	;Pixel height of tile string
	globalB B$TILPTR,,1	;ptr to fground tile string
	globalB B$GRPLAN,,1	;# of planes,for tile interpretation


	EVEN

	globalW B$PQGET,,1	;Queue head
	globalW B$PQPUT,,1	;Queue tail
	globalW B$PQNUM,,1	;Present length
	globalW B$PQLEN,,1	;Maximum queue length

;	(*)the next two variables must be in this order:

	globalB B$WNDWSW,,1	;1 if WINDOW in effect, else zero
	globalB B$WNDWSC,,1	;1 if WINDOW SCREEN in effect, else zero (*)

;	(*)the next two variables must be in this order:

	globalB B$VIEWSW,,1	;Viewport active=1
	globalB B$VIEWSC,,1	;VIEW SCREEN active=1 (*)

	globalW B$GXHPOS,,1	;bx1	Temps for clipping box
	globalW B$GXLPOS,,1	;bx2	and box fill...
	globalW B$GYHPOS,,1	;by1
	globalW B$GYLPOS,,1	;by2

;	End of new graphics data


;***
;b$COM_DCB - COM Device Control Block (DCB)
;
;Purpose:
;	This variable points to the beginning of the first COM
;	Device Control Block (DCB).  There are NUM_RS232 contiguous
;	control blocks at this location, each one controlling a
;	separate COM port.  NUM_RS232 is a constant defined at the
;	time the OEM-independent code is compiled.
;
;Allocation:
;	b$COM_DCB is a BYTE value declared in the _BSS segment by the
;	OEM-independent code.  The COM DCBs exist in memory even if
;	the COM support code is not linked in.
;
;Values:
;	The definition and size of a DCB can be found in COMDCB.INC.
;	The runtime will make sure that all bytes in this block are
;	0 at initialization time.  Even if COM support code is not linked
;	into the program, all items that access the b$COM_DCB will
;	currently work as they check if the port has been opened, and
;	0 is a flag to indicate that it is closed.
;
;Initially Set:
;	The value of b$COM_DCB is statically initialized to be all 0.
;	The DCBs are reset to a closed state at RUN time.
;
;Modified By:
;	The fields in b$COM_DCB may be modified by any routine as long
;	as the Device Control Block still repesents the current status
;	of the COM port(s).
;
;Used By:
;	Any routine needing information about the COM port(s).
;
;****************************************************************************

;	CDCBSZ=24D

	labelB <PUBLIC,B$CM1DCB> 
	globalB b$COM_DCB,0,<(SIZE COMDCB)*NUM_RS232> ;space for COM DCB's



	globalB b$LOCKTYPE,,1	; LOCK type for OPEN statment
	globalB b$ACCESS,,1	; ACCESS type for OPEN statement
	globalB b$FILMOD,,1	; file MODE for OPEN statment

;***
;b$KEY_SW - Status of Function Key Display
;OEM-Callback routine (variable)
;
;Purpose:
;	This variable serves two different purposes.  First of all, it
;	indicates the current status of the function key display.  Once
;	it is initially set, it should always reflect the display status
;	properly.  Secondly, this variable is used to turn on or off the
;	function key display.  To do this, set b$KEY_SW to the desired
;	status and do an indirect call to [b$vKEYDSP].
;
;Allocation:
;	b$KEY_SW is a byte value declared in the _BSS segment by the
;	OEM-Independent code.
;
;Values:
;	0  - Function Key Display is turned off (not displayed)
;	FF - Function Key Display is turned on (displayed)
;
;Initially Set:
;	b$KEY_SW is statically initialized to 0 (off).
;
;Modified By:
;	This variable should only be modified by code that updates the
;	function keys.	Any change to b$KEY_SW must be followed by a
;	call to [b$vKEYDSP].
;
;Used By:
;	Any routine that needs to know if the function keys are displayed.
;****
	globalB b$KEY_SW,,1	;initial func key off=0 / on=-1 switch
	globalB B$FKCNUM,,1	;number of chars of softkey to display

	staticB ERDEVP_STR,,8
	globalW b$ERDEV,,1	; device error word

	globalW B$LNASEG,,1	;LNA segment value
	globalW B$LNAOFF,,1	;LNA offset value


	globalW B$SOFT_KEY_LEN,,1    ;Nonzero if eating Soft Key chars
	globalW B$SOFT_KEY_INDEX,,1  ;Base pointer to Soft Key String



sEnd	_BSS			


sBegin	BR_SKYS 		

	labelD	<PUBLIC,b$STRTAB>       ;SOFTKEY table (word aligned)
	globalD b$SOFT_KEYS,0,NUM_FKEYS ;Initial SOFTKEY values
					;4 bytes/soft key
	labelD	<PUBLIC,b$STRTAB_END>   ;offset to end of table
sEnd	BR_SKYS 		


sBegin	CONST			

.8087				;so floating point constants will be IEEE format

	globalD b$FP_1,<1.0>	  ;s.p. 1.0 constant
	globalD b$FP_256,<256.0> ;s.p. 256.0 constant

sEnd	CONST			

sBegin	DV_TEXT

	externNP B$NearRet

sEnd	DV_TEXT

	END
