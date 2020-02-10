;	TITLE	FSTART - Entrypoint for INT 10 processing for MSHERC
;***
;FSTART - Entrypoint for INT 10 processing for MSHERC
;
;	Copyright <C> 1987, 1988, Microsoft Corporation
;
;Purpose:
;
;	  Hercules Graphics Mode support in Language products
;	  ---------------------------------------------------
;
;Characteristics:
;
;    - Requires Hercules Graphics Card.  The Graphics Card Plus also works,
;      as should the Hercules InColor Card and 100% clones of these.
;
;    - Requires a monochrome monitor.
;
;    - Hercules text mode is SCREEN 0 (BIOS 7) and behaves exactly like
;      the IBM MDPA (mono adapter).
;
;    - Hercules graphics mode is SCREEN 3.  BIOS mode used by the driver
;      is 8.
;
;    - Requires the MSHERC driver to be loaded.  The RUNTIME will report
;      an "illegal function call" if entering SCREEN 3 is attempted when
;      the driver is not loaded.
;
;    - 80x25 text dimensions with a 9x14 character box
;      (bottom 2 scan lines of 25th row are not visible)
;
;    - 720x348 pixel resolution, monochrome.
;
;    - Two pages.  Note:  only one page allowed if second adapter installed.
;
;    - No palette support.
;
;    - All other characteristics identical to CGA SCREEN 2.
;
;    - Mouse support requires user to follow special instructions regarding
;      the Hercules card in the "Microsoft Mouse Programmer's Reference Guide."
;
;    - SHELL'ed programs may not properly recognize the arbitrary BIOS screen
;      mode (8) chosen.  (There is no standard BIOS number for Hercules
;      graphics mode.)
;
;
;MSHERC Driver specification:
;
;-------------------------------------------------------------------------------
; 0  SET MODE
;	 AL => mode #8, bit 7, when set, prevents regen buffer clear.
;    Pass on all but request for Hercules graphics mode.
;    Trap ALL functions (except EGAINT10) while in graphics mode.
;    This function should be ignored if BIOS equipment variable not
;    set for B/W.
;-------------------------------------------------------------------------------
; 1  SET CURSOR TYPE
;	 CX => Cursor type (saved, but no action taken)
;-------------------------------------------------------------------------------
; 2  SET CURSOR POSITION
;	 BH => Page Number
;	 DH,DL => Row, Column, (0,0) is upper left
;-------------------------------------------------------------------------------
; 3  READ CURSOR POSITION
;	 BH => Page Number
;	 DH,DL <= Row, Column, (0,0) is upper left
;	 CX <= BIOS Cursor Type (N/A for graphics mode)
;-------------------------------------------------------------------------------
; 4  READ LIGHT PEN POSITION
;    Ignored.
;-------------------------------------------------------------------------------
; 5  SELECTACTIVEPAGE
;	 AL => Page Number
;-------------------------------------------------------------------------------
; 6  SCROLL ACTIVE PAGE UP
;	 AL => # lines (0 means blank window)
;	 CH,CL => Row, Column of upper left corner
;	 DH,DL => Row, Column of lower right corner
;	 BH => attribute for blanking lines
;-------------------------------------------------------------------------------
; 7  SCROLL ACTIVE PAGE DOWN
;	 AL => # lines (0 means blank window)
;	 CH,CL => Row, Column of upper left corner
;	 DH,DL => Row, Column of lower right corner
;	 BH => attribute for blanking lines
;-------------------------------------------------------------------------------
; 8  READ ATTRIBUTE/CHARACTER AT CURRENT CURSOR POSITION
;	 BH => Display Page
;	 AL <= Character
;-------------------------------------------------------------------------------
; 9  WRITE ATTRIBUTE/CHARACTER AT CURRENT CURSOR POSITION
;	 BH => Display Page
;	 CX => Count of chars to write
;	 AL => Character
;	 BL => Color, bit 7 causes use of XOR operation for write
;-------------------------------------------------------------------------------
; A  WRITE CHARACTER ONLY AT CURRENT CURSOR POSITION
;	 BH => Display Page
;	 CX => Count of chars to write
;	 AL => Character
;-------------------------------------------------------------------------------
; B  SET COLOR PALETTE
;    Ignored.
;-------------------------------------------------------------------------------
; C  WRITE DOT
;	 BH => Display Page
;	 DX => Row
;	 CX => Column
;	 AL => Color, bit 7 causes use of XOR operation for write
;-------------------------------------------------------------------------------
; D  READ DOT
;	 BH => Display Page
;	 DX => Row
;	 CX => Column
;	 AL <= Color
;-------------------------------------------------------------------------------
; E  WRITE TELETYPE TO ACTIVE PAGE
;	 AL => Character
;	 BL => Color
;-------------------------------------------------------------------------------
; F  CURRENT VIDEO STATE
;	 AL <= Current Mode (8)
;	 AH <= # Character Columns
;	 BH <= Active Display Page (currently always 0)
;-------------------------------------------------------------------------------
; EF INQUIRE HGC MSHERC DRIVER
;	 DL => -1
;	 DL <= -1 if driver not loaded
;	     otherwise
;	 DL <= HGC card type:
;		 0: HGC102   Graphics Card
;		 1: HGC112   Graphics Card Plus
;		 2: HGC122   InColor Card
;	 DH <= max. number of graphics pages:
;		 0 if HALF mode
;		 1 if FULL mode
;-------------------------------------------------------------------------------
;ALL OTHERS
;    Ignored.
;-------------------------------------------------------------------------------
;******************************************************************************

	include	hgcdefs.inc

code     segment para public 'code'
         assume  cs:code,ds:code

Extrn	Save_Old_Int10h:near		;TSR loader routine
Extrn	GrFuncTable:word		;jump table ptr for INT 10H functions
Extrn	Old_Int10h_Routine:dword	;Segment:Offset of previous INT 10H
Extrn	UnHookFlag:byte
Extrn	ReturnHGCStatus:near
Extrn	SetGraphicsMode:near
Extrn	ZERO:word

Public	Start_Of_New_Routine
Public	Null_Function

         org    100h

main     proc   far
         jmp    Save_Old_Int10h

Start_Of_New_Routine label word        ;this is the start of the routine

;------- Push registers onto the stack------
	push	bp
	push	es
	push	ds
	push	di
	push	si
	push	dx
	push	cx
	push	bx
	push	ax

;------- set the pointer to point to the parameters on the stack ------
	mov	bp,sp
	push	[bp+16h]		;[2] save flag (interrupt status)
	push	cs			;ds = ours
	pop	ds
	mov	es,ZERO 		;es = BIOS variable segment
	cld

;------- Check for an Inquiry Function Call-------
	cmp	ah,InquireFuncNo	;Is this a driver Inquiry?
	je	DoInquiry		;go if so

;------- Check for a SetMode Function Call-------
	or	ah,ah			;Is this a SetMode Call (0)?
	jz	DoSetMode		;Yes, do it

;------- Check the state of the UnHook Flag-----
LookAtHookFlag:
	cmp	UnHookFlag,0		;Use our routines?
	jnz	Goto_Original_Routine	;No, use IBM BIOS

;------- Look for a supported INT 10H request code ------
	cmp	ah,LowerFuncMax 	;Is the Function in the lower range?
	ja	CheckEGAINT10		;No, look for EGAINT10

Use_This_Routine:			;This jump code handles lookalikes
	mov	di,word ptr FunData.FunAX[1]	;di = offset into address table
	and	di,0FFH
	shl	di,1
	popf				;[2] restore interrupt status

;------- jump to the requested function and then return to calling program
	call	GrFuncTable[di] 	;goto the function handler

Restore_Registers:
	pop	ax
	pop	bx
	pop	cx
	pop	dx
	pop	si
	pop	di
	pop	ds
	pop	es
	pop	bp
	iret

CheckEGAINT10:
	cmp	ah,0F0H 		;EGAINT10 function?
	jnb	Goto_Original_Routine	;[2]
	pop	ax			;[2] pop flags
	jmp	short Restore_Registers ;[2] Return to calling program

DoInquiry:
	popf				;[2] restore interrupt status
	call	ReturnHGCStatus 	;status request
	jmp	short Restore_Registers ;Return to calling program

;------- Is the request for a Hercules Mode? ------
DoSetMode:
	cmp	al,HGCGrMode		;request for graphics mode wo/clear?
	je	Use_This_Routine	;go if so, set graphics mode
	cmp	al,HGCGrMode+80H	;request for graphics mode w/clear?
	je	Use_This_Routine	;go if so, set graphics mode
	mov	UnHookFlag,1		;Set the unhook flag

Goto_Original_Routine:
	pop	ax			;[2] pop flags
	pop	ax
	pop	bx
	pop	cx
	pop	dx
	pop	si
	pop	di
	pop	ds
	pop	es
	pop	bp
	jmp	cs:Old_Int10h_Routine	;goto bios routines

Null_Function	proc near
	ret
Null_Function	endp

main     endp
code     ends
         end main
