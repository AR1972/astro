	TITLE	GRINIT - GRAPHICS COMPONENT INITIALIZATION
;***
; GRINIT - GRAPHICS COMPONENT INITIALIZATION
;
;	Copyright <C> 1986 - 1988, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

;-------------------------------------------------------------------------------
;
;-----	Guide to Graphics Package Organization			7/16/86
;
;
;      The graphics package has been reorganized into several small modules
;      in order to reduce the size of compiled programs which use little or
;      no graphics.  This guide shows which modules contain which graphics
;      statements or routines.
;
;
;      LOCATION (MODULE)	   STATEMENT(s)
;      -------- --------	   ------------
;
;      circle.asm . . . . . . . . .circle
;
;      draw.asm . . . . . . . . . .draw
;
;      getput.asm . . . . . . . . .get
;				   put
;
;      grinit.asm . . . . . . . . .code used at initialization:
;				   viewport init: (B$VWINI,B$VEWINI)
;				   graphics init: (B$GRPINI)
;				   move utility:
;				       (B$MOV$MR)
;				   transformation routines (part of pmap):
;				       (B$MAPBXL,B$MAPBYL)
;
;      grputil.asm . . . . . . . . graphics utilities:
;				   coordinate processing routines:
;				       ($gsv,B$XYSAVE,$save1)
;				   attribute parser: B$CLRATR
;				   pixel clipping routine: B$INVIEW
;
;      grpmap.asm . . . . . . . . .point (arg) function
;				   rest of pmap function
;				   transformation routines (part of pmap):
;				       ($maplpx,$maplpy,$maplx1,$maply1)
;
;      gw2grp . . . . . . . . . . .gw2.0 graphics features:
;				   VIEW
;				   window
;
;      linept.asm . . . . . . . . .line
;				   point (x,y) function
;				   pset
;				   preset
;
;      paint.asm . . . . . . . . . paint
;
;      vwclr.asm . . . . . . . . . clear screen support:
;				   viewport clear: (B$VWCLR,B$VWFILL)
;				   box fill (part of line):
;				       (B$DOBOXF,boxtst,boxclp,clipck,B$SCALXY)
;				   line utilities:
;				       ($xchgx,$xchgy,$xchgac,B$XDELT,B$YDELT,
;					clipck,B$SCALXY)
;
;------------------------------------------------------------------------------
	INCLUDE switch.inc
	INCLUDE rmacros.inc	

	useSeg	_DATA		
	useSeg	_BSS		
	useSeg	GR_TEXT 	
	useSeg	DV_TEXT 	
	useSeg	<XIB>		; XIB and XIE must bracket XI!
	useSeg	<XI>		; initializer segment
	useSeg	<XIE>		

	INCLUDE seg.inc 	
	INCLUDE idmac.inc	; Internal debugging macros
	INCLUDE compvect.inc	; Initialization vectors

	INITIALIZER B$xGRINI	;Put B$xGRINI in initializer list

sBegin	_DATA			

	globalW	b$pFPGRINI,B$NearRet,1 ; vector to indirectly call B$FPGRINI

	externW	b$VWCLR_PTR
	externW	b$VWINI_PTR

	externW b$run_disp	; RUN time initialization dispatch table
	externW b$clr_disp	; CLEAR statement support dispatch table
	externW b$err_disp	; error dispatch table


sEnd	_DATA			

sBegin	_BSS			
;
;#***************************************************************************
; External low-level function vectors
;#***************************************************************************
;
externW b$MapXYC		
externW b$SetAttr		
externW b$PalReset		
;#***************************************************************************

	externB B$COPTFL 	; Circle options flag

staticB ScreenLocked,,1 	; Screen is locked flag

	EXTRN	B$GRPACX:WORD	;defined in GWDATA.ASM
	EXTRN	B$GRPACY:WORD	;defined in GWDATA.ASM

	externB	B$WNDWSC	; defined in GWDATA.ASM
	externB	B$WNDWSW	; defined in GWDATA.ASM

	globalW B$VXSIZ,,1	;Viewport horiz size
	globalW B$VYSIZ,,1	;Viewport vert size
	globalW B$VXMIN,,1	;Viewport minimum X coord
	globalW B$VXMAX,,1	;Viewport maximum Y coord
	globalW B$VYMIN,,1	;Viewport minimum X coord
	globalW B$VYMAX,,1	;Viewport maximum Y coord
	globalW B$VXOFF,,1	;Viewport offset X
	globalW B$VYOFF,,1	;Viewport offset Y

	globalW B$VXDIF,,1	; View x displacement
	globalW B$VYDIF,,1	; View y displacement

	externB	B$VIEWSW	; defined in GWDATA.ASM
	externB	B$VIEWSC	; defined in GWDATA.ASM

	EXTRN	B$DFRACX:WORD	;defined in GWDATA.ASM

	EXTRN	B$LINSTL:WORD	;defined in GWDATA.ASM

	EXTRN	B$LENDRW:ABS	;defined in GWDATA.ASM
	EXTRN	B$DRWSCL:WORD	;defined in GWDATA.ASM
	EXTRN	B$DRWANG:WORD	;defined in GWDATA.ASM

sEnd	_BSS			


sBegin	DV_TEXT 			
	externNP	B$NearRet	;for disp vectors in compvect.inc
sEnd	DV_TEXT 			

sBegin	GR_TEXT 		
assumes CS,GR_TEXT		

externNP B$RESETSCN		
;OEM routines:
	externNP B$GRMODE	
	externNP B$GrScreenSize 
externNP B$MapVWC		
	EXTRN	B$VWCLR:NEAR


	PUBLIC	B$VWINI
	PUBLIC	B$GRPINI
	PUBLIC	B$VEWINI
	PUBLIC	B$VWMAPC

	PAGE
	SUBTTL	Graphics initialization routines
;***
;B$xGRINI - Initializer for the graphics component
;PLM B$xGRINI()
;
;Purpose:
;
;Entry:
; None.
;
;Exit:
;	Appropriate dispatch vectors filled.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$xGRINI,<FAR> 	
cBegin				
;
;	update "RUN" time initialization dispatch address to B$VWINI
;
	MOV	WORD PTR [b$run_disp].GR_RVEC,GR_TEXTOFFSET B$GRINI	
;
;	update CLEAR statement dispatch address to B$GRPRST
;
	MOV	WORD PTR [b$clr_disp].GR_CVEC,GR_TEXTOFFSET B$GRPRST	
;
;	update ERROR dispatch address to B$GRREST
;
	MOV	WORD PTR [b$err_disp].GR_ERVEC,GR_TEXTOFFSET B$GRREST 

	MOV	[b$VWINI_PTR],OFFSET B$VWINI
	MOV	[b$VWCLR_PTR],OFFSET B$VWCLR
cEnd				

cProc	B$GRINI,<NEAR>		
cBegin				
	call	B$RESETSCN	;restore the screen to the startup state
	call	[b$PalReset]	;reset the palette
cEnd				


;***
;B$GRPINI	Calculate center of viewport
;
;Purpose:
;		Calculate center of viewport (center of screen if no viewports)
;		saving in B$GRPACX, B$GRPACY. Find B$VXDIF and B$VYDIF if viewports
;		and windows.
;
;Entry:
;		none
;
;Exit:
;		variables updated as specified above
;
;Modifies:
;		none
;
;****

B$GRPINI:
	PUSH	AX
	PUSH	CX
	XOR	CX,CX		;Use Vx1= 0
	MOV	AX,WORD PTR B$VXSIZ ;and Vx2= max
	CMP	[B$VIEWSC],0	; if VIEW SCREEN
	JNZ	VWMAP2		;Brif so, else
	MOV	CX,WORD PTR B$VXMIN ;calculate x center
	MOV	AX,WORD PTR B$VXMAX ;using View port Vx1,Vx2
VWMAP2:
	SUB	AX,CX		;[AX]= Vx2-Vx1
	MOV	WORD PTR B$VXDIF,AX 
	INC	AX		;+1 since inclusive
	SHR	AX,1
	ADD	CX,AX		;[CX]= xmin+(xmax-xmin)/2
	MOV	B$GRPACX,CX

;	--------------
	XOR	CX,CX		;Use Vy1= 0
	MOV	AX,WORD PTR B$VYSIZ ;and Vy2= max
	CMP	[B$VIEWSC],0 ; if VIEW SCREEN
	JNZ	VWMAP3		;Brif so, else
	MOV	CX,WORD PTR B$VYMIN ;calculate y center
	MOV	AX,WORD PTR B$VYMAX ;using View port Vy1,Vy2
VWMAP3:
	SUB	AX,CX		;[AX]= Vy2-Vy1
	MOV	WORD PTR B$VYDIF,AX 
	INC	AX		;+1 since inclusive
	SHR	AX,1
	ADD	CX,AX		;[CX]= ymin+(ymax-ymin)/2
	MOV	B$GRPACY,CX
	POP	CX
	POP	AX
	RET


;***
;B$VWINI 	Initialize graphics viewport
;
;Purpose:
;		Initialize graphics viewport by setting viewport state
;		variables.  Center graphics cursor.  Calculate floating
;		point screen aspect ratio.
;
;Entry:
;		none
;
;Exit:
;		none
;
;Modifies:
;		none
;
;****
B$VWINI:
	PUSH	BX
	PUSH	AX
	PUSH	CX
	PUSH	DX
	PUSH	SI
	PUSH	DI
	MOV	WORD PTR B$LINSTL,0FFFFh ;Set line style to solid
	CALL	B$VEWINI 	;Init viewport, center graphics cursor
	POP	DI
	POP	SI
	POP	DX
	POP	CX
	POP	AX
	POP	BX
	RET

;***
;B$VEWINI	 initialize viewport coordinates
;
;Purpose:
;		Initialize screen and viewport variables.  Calculate
;		window variables to make window transformations proceed
;		faster.
;
;Entry:
;		none
;
;Exit:
;		VIEW and window variables updated
;
;Uses:
;		Per convention.
;
;****

B$VEWINI:
	MOV	WORD PTR B$VIEWSW,0 ;Initially no VIEW (Clears B$VIEWSC also)
	CALL	B$GRMODE	;Legal graphics mode?
	JNZ	VEWIN0		;Yes: go init viewport
	RET			; else quit
VEWIN0:
	CALL	B$GrScreenSize   ;[CX]= B$VXSIZ, [DX]= B$VYSIZ
	MOV	WORD PTR B$VXSIZ,CX
	MOV	WORD PTR B$VYSIZ,DX
	XOR	AX,AX
VEWIN1:
	MOV	WORD PTR B$VXMIN,AX ;vx1= 0
	MOV	WORD PTR B$VYMIN,AX ;vy1= 0
	MOV	WORD PTR B$VXOFF,AX ;vx offset= 0
	MOV	WORD PTR B$VYOFF,AX ;vy offset= 0
	MOV	WORD PTR B$VXMAX,CX ;vx2= B$VXMAX
	MOV	WORD PTR B$VYMAX,DX ;vx2= B$VYMAX

;B$VWMAPC Caculates center of Viewport and stores in:
;	B$GRFACX,B$GRFACY (world coords), and B$GRPACX,B$GRPACY (physical coords).
;	Also calls B$MapVWC to set View bounds at 'CLOC' level so PAINT
;	works rapidly. Clears DRAW vars, recalculates WINDOW vars...

B$VWMAPC:
	PUSH	ES		
	PUSH	DS		
	POP	ES		;Set ES = DS
	PUSH	DI		
	MOV	DI,OFFSET DGROUP:B$DRWSCL ;Init Draw dependent
	MOV	CX,B$LENDRW	;get external constant of vars
	XOR	AX,AX
	CLD
	REP	STOSB		
	POP	DI		
	POP	ES		
	MOV	WORD PTR B$DFRACX,100200O ;x,y fraction is 1/2
;	PUSH	BX			;Save possible text pointer
	MOV	AX,WORD PTR B$VXMIN
	MOV	BX,WORD PTR B$VXMAX
	MOV	CX,WORD PTR B$VYMIN
	MOV	DX,WORD PTR B$VYMAX
	push	bp		;save bp ($mapxyc will trash it)
	CALL	B$MapVWC	;Let OEM know viewport boundaries for PAINT.
;	--------------		now move "C" to center
	CALL	B$GRPINI 	;Center graphics cursor
	MOV	CX,B$GRPACX
	MOV	DX,B$GRPACY
;	--------------
	CALL	[b$MapXYC]	;Set CLOC/CMASK
	pop	bp		;restore bp after mapxyc calls

	JMP	[b$pFPGRINI]	; Initialize window variables

	PAGE
	SUBTTL	Graphics Initialization

	PUBLIC	B$GRPRST 	;Graphics Initialization routine
	EXTRN	B$GETFBC:NEAR

;***
;B$GRPRST	Reset graphics variables
;Purpose:
;	 B$GRPRST resets graphics.  It is called at initialization and whenever
;	 a CLEAR statement is executed.
;
;Entry:
;	none
;Exit:
;	graphics variables reset
;Modifies:
;	none
;****

B$GRPRST:
	PUSHF
	PUSH	AX
	PUSH	BX
	XOR	AX,AX
	PUSH	ES		
	PUSH	DS		
	POP	ES		;Set ES = DS
	PUSH	DI
	PUSH	CX
	MOV	DI,OFFSET DGROUP:B$DRWSCL ;Init Draw dependent
	MOV	CX,B$LENDRW	;get external constant no. of vars
	CLD
	REP	STOSB		
	POP	CX
	POP	DI		
	POP	ES		
	MOV	WORD PTR B$DFRACX,100200O ;x,y fraction is 1/2
	MOV	BYTE PTR B$DRWSCL,AL ;Draw scale init
	MOV	BYTE PTR B$DRWANG,AL ;Draw angle init
	MOV	WORD PTR B$WNDWSW,0 ;Turn off windows
	CALL	B$VWINI		;Reset viewport, turn off window
				; and center the graphics cursor
	STC
	CALL	B$GETFBC 	;Get foreground/background colors
	CALL	[b$SetAttr]	;Set the default DRAW color
	POP	BX
	POP	AX
	POPF
	RET

;***
; B$GRREST - Reset graphics state variables.
;
;Purpose:
; Use at the end of a graphics statement, or when an error ocurrs which
; abnormally terminates a graphics statement, to reset state variables.
; Added with revision [6].
;
;Entry:
; None.
;
;Exit:
; Variables reset.
;
;Uses:
; Per convention.
;
;******************************************************************************
cProc	B$GRREST,<NEAR> 	
cBegin

	XOR	AX,AX
	MOV	B$COPTFL,AL	;Reset circle options.

cEnd





sEnd	GR_TEXT

	END
