	TITLE	GRPMAP - Graphics Window-to-Viewport transormation code
;***
; GRPMAP - Graphics PMAP and POINT functions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; This module contains Window-to-Viewport transformation code. This code lives
; in a module of its own to allow us the opportunity to make low-level mathpack
; dependancies for these speed-critical transformations.
;
; BASIC Syntax mapping to included runtime entry points:
;
; - PMAP Function:
;
;      v = PMAP(x,n)
;	     |
;	   B$PMAP
;
; - POINT Function - There are two forms of this, one which takes 2 arguments
;		     (x and y coordinates), and one which takes only 1.
;		     The second form is given here:
;
;      v = POINT(n)
;	     |
;	   B$PNT1
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	useSeg	_DATA
	useSeg	_BSS
	useSeg	GR_TEXT

	INCLUDE seg.inc

sBegin	_DATA
externW B$AC
sEnd	_DATA

sBegin	_BSS

externW B$VXMIN
externW B$VYMIN
externW B$VXOFF
externW B$VYOFF
externW B$GRPACX
externW B$GRPACY

externB B$VIEWSW		
externB B$VIEWSC		
externW B$VXOFF
externW B$VYOFF

externB B$WNDWSW		
externW B$GRFACX
externW B$GRFACY
externW B$GRFACX
externW B$GRFACY


sEnd	_BSS


sBegin	GR_TEXT
assumes CS,GR_TEXT


externNP B$ERR_FC
externNP B$fmldw		
externNP B$ftolrnd		
externNP B$MAPLX1		; AX = view x coord
externNP B$MAPLY1		; AX = view y coord
externNP B$GRMODE

externNP B$MAPPX2
externNP B$MAPPY2


	SUBTTL	B$PMAP - Map expression to Logical or Physical Coordinates
	PAGE

;***
; B$PMAP - Map expression to Logical or Physical Coordinates
; R4 pascal B$PMAP(R4 expr, I2 funct)
;
;Purpose:
; x = PMAP(expr,funct) - Function to map expression to logical or physical
; coordinates as follows:
;
;	funct = 0	Maps Logical  expr to Physical x.
;		1	Maps Logical  expr to Physical y.
;		2	Maps Physical expr to Logical  x.
;		3	Maps Physical expr to Logical  y.
;
;Input:
; expr	= S.P. expr value
; funct = function code 0-3
;
;Output:
; [AX]	= pointer to result
;
;Modifies:
; per convention
;
;Exceptions:
; Control may be transfered to B$ERR_FC
;
;******************************************************************************
cProc	B$PMAP,<FAR,PUBLIC>,<SI,ES>
parmD	expr			;R4 expression result
parmW	funct			;function to perform
cBegin


	PUSH	DS		
	POP	ES		;Set ES = DS
	CALL	B$GRMODE 	;Legal graphics mode?
	JZ	ERRFC		;Brif Illegal Graphics

	LEA	BX,expr 	;[BX] = pointer to R4
	MOV	SI,funct	;[SI] = function code
	CMP	SI,4		;Check funct range
	JB	MAPID2		;Must be in range 0-3
ERRFC:
	JMP	B$ERR_FC	  ;function call error
MAPID2:

	SHL	SI,1		;funct*2 for dispatch

	CALL	WORD PTR CS:MAPTBL[SI] ;Do the map

	MOV	BX,OFFSET DGROUP:B$AC ; semi-colons work well for comments!
	FSTP	DWORD PTR [BX]	; return result in FAC
	XCHG	AX,BX		; pointer in ax
	FWAIT			

cEnd

MAPTBL:
	DW	OFFSET MAPLXP	;Log X to Phy X
	DW	OFFSET MAPLYP	;Log Y to Phy Y
	DW	OFFSET MAPPXL	;Phy X to Log X
	DW	OFFSET MAPPYL	;Phy Y to Log Y
;
; Logical to physical transformation.
;
; These routines call transormation routines in grwindow.asm, which return
; an integer result in AX.
;
MAPLYP: 			;Map window y coord to VIEW y coord
	CALL	B$MAPLY1	; AX = integer value
	JMP	SHORT MAPLXP1	;go put it in ST0.
MAPLXP: 			;Map window x coord to VIEW x coord
	CALL	B$MAPLX1	; AX = integer value
MAPLXP1:
	XCHG	BX,AX		; BX = view coord
	JMP	B$fmldw	;push value on numeric stack

;
; Physical to Logical transformation.
;
; These routines all jump to code in grinit to complete there functions. The
; code resides there to reduce size of programs which do not use graphics.
; (At least that's what the comment over there says).
;
MAPPXL:
	FLD	DWORD PTR [BX]	
	CALL	B$ftolrnd	
	XCHG	AX,BX		;[BX]= INT(expr)
	JMP	B$MAPPX2 	;continue calculation of transformation
MAPPYL:
	FLD	DWORD PTR [BX]	
	CALL	B$ftolrnd	
	XCHG	AX,BX		;[BX]= INT(expr)
	JMP	B$MAPPY2 	;continue transformation


	SUBTTL	B$PNT1 - Single argument POINT function
	PAGE
;***
;B$PNT1 - Single argument POINT function
;R4 pascal B$PNT1(I2 funct)
;
;Purpose:
; x = POINT(funct) - Function to map expression to logical or physical
; coordinates as follows:
;
;	funct = 0	Returns the current Physical x coordinate
;		1	Returns the current Physical y coordinate
;		2	if WINDOW active
;			  Returns the current Logical  x coordinate
;			else
;			  Returns the current Physical x coordinate
;		3	if WINDOW active
;			  Returns the current Logical  y coordinate
;			else
;			  Returns the current Physical y coordinate
;
;Input:
; funct = function code 0-3
;
;Output:
; [AX]	= pointer to result.
;
;Modifies:
; Per convention
;
;Exceptions:
; Control may be transfered to B$ERR_FC
;
;******************************************************************************
cProc	B$PNT1,<FAR,PUBLIC>
parmW	funct			;function code
cBegin

	CALL	B$GRMODE 	;Legal graphics mode?
	JZ	ERRFC		;Brif Illegal Graphics

	MOV	CX,funct	;[CX] = function code
	CMP	CX,4		;Allow 0-3 if WINDOW Statement
	JNB	ERRFC		;Brif out of range


POINT0:
	MOV	BX,B$GRPACX
	MOV	DX,B$VXMIN
	JCXZ	POINTP		;if funct==0 then return Physical x

	LOOP	POINT_NOT1	;Jump if not function 1
	MOV	BX,B$GRPACY
	MOV	DX,B$VYMIN
POINTP:
	CMP	B$VIEWSW,0	;VIEW in effect?
	JZ	POINT4		;Brif not
	CMP	B$VIEWSC,0	;VIEW SCREEN option?
	JNZ	POINT4		;Brif so
	SUB	BX,DX		;else map into Viewport
POINT4:
	CALL	B$fmldw	; ST0 = integer in BX
POINT_EXIT:
	MOV	BX,OFFSET DGROUP:B$AC ; semi-colons work well for comments!
	FSTP	DWORD PTR [BX]	; return result in FAC
	XCHG	AX,BX		
	FWAIT			
cEnd

POINT_NOT1:

	DEC	CX		;In case we jump
	CMP	B$WNDWSW,0	;WINDOW active?
	JZ	POINT0		;Jump if not, CX = changed FCN (2->0, 3->1)

	MOV	BX,OFFSET DGROUP:B$GRFACX
	JCXZ	POINTF		;Jump if it was function 2
	MOV	BX,OFFSET DGROUP:B$GRFACY ;Function 3: return Logical y
POINTF:
	FLD	DWORD PTR [BX]	
	JMP	POINT_EXIT	

POINT_NOT3:

sEnd	GR_TEXT

	END
