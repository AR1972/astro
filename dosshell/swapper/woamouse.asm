;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This module contains routines realated to saving and restoring of the mouse;
; context.      							     ;
;									     ;
; History:								     ;
;									     ;
;	 Fri June-15-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 Adapted for the Dos Task Switcher.				     ;
;									     ;
;        Wed Dec-20-1989.       -by-  Amit Chatterjee [amitc]		     ;
;        Put in support for Mouse System's Version 5+ Mouse drivers.	     ;
;									     ;
;        Tue June-20-1989.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created for Windows. (Added the History legend) 		     ;
;----------------------------------------------------------------------------;

	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include	woahpeqs.inc
	include	njmp.mac
	include macros.mac
	include	woahp.inc
	.list

	.286p

		public	MouseSeg

createSeg   _WOARLMSEG,StubSeg,word,public,code
sBegin	StubSeg

	assumes	cs,StubSeg
	assumes	ds,StubSeg


;----------------------------------------------------------------------------;
; define the external function calls.		          		     ;
;----------------------------------------------------------------------------;

	;-------------------------------------------------------;
	; external OLDAPP procedures.			        ;
	;-------------------------------------------------------;


		externNP	IsVectra		;(WOAHP.ASM)
		externNP	SaveData		;(WOAHP.ASM)
		externNP	RestoreData		;(WOAHP.ASM)
		externNP	SaveLinkMapping		;(WOAHP.ASM)
		externNP	RestoreLinkMapping	;(WOAHP.ASM)

;----------------------------------------------------------------------------;
; declare the variables which are defined elsewhere.			     ;
;----------------------------------------------------------------------------;

externB		fVectra			;tells us whether m/c is a vectra

;----------------------------------------------------------------------------;
; define constants related to various mices.				     ;
;----------------------------------------------------------------------------;

HPMOUSE_SIZE	= 0A0H
TRACK_SIZE	= 280H

; Equates for EX-BIOS interrupt number and vector address.
HPENTRY 		equ	6FH
F_HPENTRY		equ	HPENTRY
HP_ID			equ	4850H	;'HP'

V_STRACK		equ	005AH	; Common cursor control funcs.
V_LHPMOUSE		equ	00CCH	; Microsoft/Mouse System's
					;   Compatible Driver
F33_INQUIRE		equ	6F00H	; HP inquire function
F_IO_CONTROL		equ	02H*2	; Device/Driver Dependent
					;   Functions

;----------------------------------------------------------------------------;
; define the mouse state related variables.				     ;
;----------------------------------------------------------------------------;

MouseType		db	0	;type of mouse
					; 1 => Microsoft version 4
MouseSize		dw	?	;size of MsMouse
MouseSeg		dw	?	;segment where mouse state is saved
MouseStateSize		dw	?	;size in para of mouse state
AppMouseX		dw	?	;save switch out x value
AppMouseY		dw	?	;save switch out y value
MouseVector		dd	?	;INT 33 vector
pRestoreMouse		dw	iqRet	     
pSaveMouse		dw	iqRet	     
HPMouseSize		dw	HPMOUSE_SIZE 
TrackSize		dw	TRACK_SIZE   

;----------------------------------------------------------------------------;

;----------------------------------------------------------------------------;
; EnableMouse:								     ;
;									     ;
; This routine enables the mouse driver.  				     ;
;----------------------------------------------------------------------------;

cProc	EnableMouse,<NEAR,PUBLIC,PASCAL>

cBegin

	cmp	MouseType,1		;MS mouse ?
	jnz	@f			;no.
	mov	ax,20h			;enable mouse code
	int	33h
@@:

cEnd
;----------------------------------------------------------------------------;
; DisableMouse:								     ;
;									     ;
; This routine disables the mouse driver. 				     ;
;----------------------------------------------------------------------------;

cProc	DisableMouse,<NEAR,PUBLIC,PASCAL>

cBegin

	cmp	MouseType,1		;MS Mouse
	jnz	@f			;no
	mov	ax,1fh			;disable mouse code
	int	33h
@@:

cEnd
;----------------------------------------------------------------------------;
; GetMouseStateSize:							     ;
;								             ;
; Gets the size of the buffer needed to save the mouse state if there is a   ;
; mouse driver installed.						     ;
;									     ;
;	The mouse driver compatiblity is searched in the following order:    ;
;									     ;
;		New Microsoft Mouse (4.0+)				     ;
;		New PC Mouse (5.0+)					     ;
;		ATT (Logitech)						     ;
;		Rom HP Mouse (A.01.02)					     ;
;		Ram HP Mouse (Patch versions)				     ;
;		Old Microsoft Mouse					     ;
;----------------------------------------------------------------------------;

cProc	GetMouseStateSize,<NEAR,PUBLIC,PASCAL>,<bp,ds,si,di>

	localV	link,32			;for vectra link mapping saving

cBegin

	assumes	cs,StubSeg
	assumes	ds,StubSeg
	assumes	es,nothing


; Is a INT 33 mouse driver there?

	xor	ax,ax			;Look in INT table for a vector
	mov	es,ax			;es has IDT seg
	les	bx, es:[33h*4]	    	;load INT 33 vector
	mov	word ptr cs:[MouseVector], bx 
	mov	word ptr cs:[MouseVector+2], es
	mov	cx,es			;check for null vector
	or	cx,bx		    	;is it NULL ?
	jz	imLExit 		;int 33 vector not stuffed, so no mouse
	cmp	byte ptr es:[bx], 0CFh	;make sure it is not an IRET
	jne	imIsMouse		;there is a mouse
imLExit:
	xor	ax,ax			;no mouse
	mov	MouseStateSize,ax	;save null size
	jmp	imExit			;return back


; There is a mouse driver.

imIsMouse:

; Vectras need there link mapping saved and restored by this function

	push	ss			;temp save area on stack
	pop	es
	lea	di, link		;es:di points to save are
	call	SaveLinkMapping 	;This func has no effect on non vectra

; Is this a Vectra? If so, Is this the wonderfull ROM mouse driver?

imRomHPMouse:

	call	IsVectra		; Is this a Vectra?
	jz	imNewMouse		;No, Can't be a HP Mouse
	mov	bx, word ptr cs:[MouseVector+2]
	cmp	bx, 0F000H		;Does Int 33 point to ROM
	jne	imRamHPMouse		;No, it might be a RAM HP Mouse
	jmp	short imHPDef		;it is a ROM based HP mouse

; Is this patch version of the HP Mouse which is ram based ?
; Notice that I don't make this call with ROM mouse drivers
; because the first ROM mouse driver barfs on this call. Great Code!

imRamHPMouse:

	xor	bx, bx			;Does this ID as an HP mouse?
	mov	AX, F33_INQUIRE 	;do the inquire
	int	33H			
	cmp	BX, 'HP'                ;HP ID Code (new version) ?
	jne	imNewMouse		;check for new mouse

; HPMOUSE:  Inquire LHPMOUSE to get the HPmouse and TRACK sizes

imHPSize:

	mov	AX, F_IO_CONTROL SHL 8 + SF_MOUSE_DSIZE
	mov	bp, V_LHPMOUSE
	int	6fh

; Returns:
;  AH=  RS_UNSUPPORTED - Use defaults
;  AH=  RS_SUCCESSFUL
;       BH= HPMOUSE size in paras
;       BL= TRACK size in paras

	cmp	ah, 0			;valid return ?
	jne	imHPDef 		;No, use default size
	mov	al, bh			;convert size in bytes and save
	mov	cl, 4			
	shl	al, cl
	mov	HPmouseSize, ax		;save size
	xor	bh, bh			;deal with tracksize in same way
	shl	bx, cl
	mov	TrackSize, bx		;save track size in bytes

imHPDef:

	mov	ax, cs:[HPMouseSize]   	;size of the mouse state
	add	ax, cs:[TrackSize]	;add track size
	mov	MouseSize,ax		;total mouse size in bytes

; save the address of the routines to do the save and restore

	mov	pSaveMouse, StubSegOFFSET SaveHPMouse
	mov	pRestoreMouse, StubSegOFFSET RestoreHPMouse
	jmp	imRestore	        ;done with HP mouse

; Now we know that the mouse is not a microsoft mouse.

; Does it support Microsoft's function 21?
;
;	If so, its Microsoft Mouse version 4.0+ compatible.
;	This is prefered method of saving the mouse state.

imNewMouse:

	mov	ax, 21			;check to see if this is supported
	xor	bx, bx			;zero out return value first
	int	33h
	cmp	bx, 0			;if 0 then 21 is not supported
	je	imPCMouse		;must be PC mouse

; we have a version 4.0 int 33 interface mouse. Save the address of the save
; and restore state routines and the mouse size

	mov	MouseType,1		;new mouse
	mov	pSaveMouse, StubSegOFFSET SaveNewMouse
	mov	pRestoreMouse, StubSegOFFSET RestoreNewMouse
	mov	MouseSize,bx		;save size of save area
	jmp	short imRestore		;done with new mouse



; now we need to see whether this is mouse systems mouse or not.

; Does it support Mouse System's function B
;	If so, it's Mouse System version 5.0+ compatible

imPCMouse:
					
	mov	ax, 'B'			;MS Mouse supports this
	xor	bx, bx			;zero out return value before call
	int	33h
	cmp	bx, 0			;if 0 then not MS Mouse
	je	imATTMouse		;could be ATT keyboard mouse

; This is a mouse systems mouse. So save the addresses of the appropriate
; routines for saving and restoring state

	mov	pSaveMouse, StubSegOFFSET SavePCMouse
	mov	pRestoreMouse,StubSegOFFSET RestorePCMouse
	mov	MouseSize,bx		;save the size
	jmp	short imRestore		;done with Mouse Systems mouse

; Now check to see if this is an ATT mouse driver.

imATTMouse:

	les	bx,MouseVector		;get the segment for the mouse vector
	cmp	es:[bx+10h],'OL'        ;see if logitech there
	jne	imOldMouse		;no. Must be old mouse
	mov	ax, '.1'                ;Further check requested by
	cmp	es:[bx+27h], ax 	; Bill Hall
	je	imFoundATT		;yes logitech mouse
	cmp	es:[bx+28h], ax 	;last test
	jne	imOldMouse		;not logitech. Must be old mouse

imFoundATT:

	mov	ax,es:[101h]		;Yes, calculate the save area
	add	ax,103h 		;relative jmp + len of psp + 3 =
	                                ;dist to blowaway code
	mov	MouseSize,ax	;save it

; save the address of the save restore routines

	mov	pSaveMouse,StubSegOFFSET SaveATTMouse
	mov	cs:[pRestoreMouse],StubSegOFFSET RestoreATTMouse
	jmp	short imRestore     		;done with logitech mouse


; Is this an old Microsoft Mouse driver 2.0 or less

imOldMouse:

	mov	MouseSize,0		;initialize
	mov	ax,'M'	  		;test for version 2 mouse driver
	int	33h
	cmp	word ptr es:[di],'oC'   ;Does secret mouse call point at
					;copyright message?
	jne	imExit			;No, no mouse present.

; save address of save/restore routines for old mouse

	mov	pSaveMouse, StubSegOFFSET SaveOldMouse
	mov	pRestoreMouse, StubSegOFFSET RestoreOldMouse
	mov	ax, word ptr MouseVector
	mov	MouseSize, ax
	errn$	imRestore

; Restore Vectra's HP-HIL mapping

imRestore:

	push	ss
	pop	es		       	
	lea	di, link		;es:di points to the saved state	
	call	RestoreLinkMapping 	;This func has no effect on non vectra

imExit:

; return with the size of the state in paragraphs

	mov	ax,MouseSize		;get the size
	shiftr	ax,4			;get it in paragraphs
	inc	ax			;round it up.
	test	fVectra,1		;is it a vectra
	jz	@f			;no
	add	ax,2			;for saving link mapping
@@:


cEnd	GetMouseState
;----------------------------------------------------------------------------;
; SaveMouseState:							     ;
;	     Save the state of the INT 33 driver       			     ;
;----------------------------------------------------------------------------;

cProc	SaveMouseState, <NEAR,PUBLIC,PASCAL>, <ES,DS,SI,DI>				     

cBegin	     

	assumes	cs,StubSeg
	assumes	ds,StubSeg
	assumes	es,nothing

	mov	cx,MouseSeg			  
	jcxz	smsx			;noting to save
	mov	es, cx			;get the mouse save state segment


; If this is a Vectra the link mapping needs to be saved.		     

	test	cs:[fVectra], 1		;is it a Vectra ?				     
	jz	smNotVectra		;no.				     
	xor	di, di			;Save the link mapping		     
	call	SaveLinkMapping 	;This func has no effect on non vectra
	mov	bx, es			;the mouse save area is next	     
	add	bx,2			;add 2 for the link mapping area				     
	mov	es,bx			;mouse state will be saved here							     

smNotVectra:

	mov	cx, cs:[MouseSize] 	;get the size of the mouse
	call	cs:[pSaveMouse]		;save the state

; Put the mouse in the default state with cursor hidden.

; This done so that when we start up another app the it
; will not have the cursor displayed initially.

	xor	ax, ax		       	;initialize mouse
	int	33h

smsx:
cEnd
;----------------------------------------------------------------------------;
; RestoreMouseState:							     ;
;									     ;
; Restores the state of the mouse.					     ;
;----------------------------------------------------------------------------;

cProc	RestoreMouseState, <NEAR,PUBLIC,PASCAL>, <ES,DS,SI,DI>

cBegin

	assumes	cs,StubSeg
	assumes	ds,StubSeg
	assumes	es,nothing


	mov	cx, MouseSeg		;get the segment for the save area
	jcxz	rmx		      	;no save area

; If this is a Vectra the link mapping must be restored. However
; the mouse must restored first because this operation will
; destroy the link mapping.

	test	fVectra,1 		;Is this a Vectra?
	jz	rmCall			;no.
	push	cx			;Save
	add	cx,2			;get to the save area for mouse state

rmCall:

	mov	es,cx			;points to mouse save area
	mov	cx,MouseSize		;get the size in bytes
	push	ds			;save DS for RestoreLinkMapping
	call	pRestoreMouse		;restore the mouse state
	pop	ds

; Restore the link mapping

	test	fVectra,1		;is this a vectra
	jz	rmx		     	;no
	pop	es			;get back start of segment
	xor	di, di			;es:di points to save area
	call	RestoreLinkMapping 	;This func has no effect on non vectra

rmx:

cEnd
;----------------------------------------------------------------------------;
;									     ;
; SaveNewMouse - Save the new mouse state using the function 22 code	     ;
;									     ;
; Entry									     ;
;	ES:0 - mouse segment						     ;
;									     ;
; Exit									     ;
;	MouseSeg is updated						     ;
;									     ;
; Uses									     ;
;	AX,BX,CX,DX,SI,DI,ES,DS						     ;
;----------------------------------------------------------------------------;

SaveNewMouse:

	mov	ax, 22			;save code
	jmp	short nmsEntry		;Use the RestoreNewMouse code


;----------------------------------------------------------------------------;
; RestoreNewMouse - Restore the new mouse state using the function 23 code.  ;
;									     ;
; Entry									     ;
;	ES:0 - mouse segment						     ;
;	CX   - Mouse size						     ;
;									     ;
; Exit									     ;
;	The mouse driver state restored					     ;
;									     ;
; Uses									     ;
;	AX,BX,CX,DX,SI,DI,ES,DS						     ;
;----------------------------------------------------------------------------;	     

RestoreNewMouse:	  

	xor	ax, ax
	int	33h			;reset the mouse
	mov	ax, 23			;restore mouse code

nmsEntry:

	xor	dx,dx			;es:dx has the save/restore area
	int	33h			;save/restore mouse state

iqret:
	ret				;done

;----------------------------------------------------------------------------;
; SavePCMouse - Save the PC mouse state using the P function.		     ;
;									     ;
; Entry									     ;
;	ES:0 - mouse segment						     ;
;	CX   - Mouse size						     ;
;									     ;
; Exit									     ;
;	MouseSeg is updated						     ;
;									     ;
; Uses									     ;
;	AX,BX,CX,DX,SI,DI,ES,DS						     ;
;----------------------------------------------------------------------------;

SavePCMouse:

	mov	ax, 'P'
	jmp	short pmsEntry		; Use the RestorePCMouse code

;----------------------------------------------------------------------------;
; RestorePCMouse - Restore the PC Mouse state using the R function.	     ;
;									     ;
; Entry									     ;
;	ES:0 - mouse segment						     ;
;	CX   - Mouse size						     ;
;									     ;
; Exit									     ;
;	The mouse driver state restored					     ;
;									     ;
; Uses									     ;
;	AX,BX,CX,DX,SI,DI,ES,DS						     ;
;----------------------------------------------------------------------------;

RestorePCMouse:

	mov	ax, 'R'

pmsEntry:

	mov	bx, cx
	xor	dx, dx
	int	33h
	ret

;----------------------------------------------------------------------------;
; SaveATTMouse								     ;
;									     ;
;									     ;
; Entry									     ;
;	ES:0 - mouse segment						     ;
;	CX   - Mouse size						     ;
;									     ;
; Exit									     ;
;	MouseSeg is updated						     ;
;									     ;
; Uses									     ;
;	AX,BX,CX,DX,SI,DI,ES,DS						     ;
;----------------------------------------------------------------------------;

SaveATTMouse:

	mov	ax,2			;hide cursor
	int	33h			
	jmp	SaveOldMouse

;----------------------------------------------------------------------------;
; RestoreATTMouse							     ;
;									     ;
; Entry									     ;
;	ES:0 - mouse segment						     ;
;	CX   - Mouse size						     ;
;									     ;
; Exit									     ;
;	The mouse driver state restored					     ;
;									     ;
; Uses									     ;
;	AX,BX,CX,DX,SI,DI,ES,DS						     ;
;----------------------------------------------------------------------------;

RestoreATTMouse:

	mov	ax, es
	mov	ds, ax
	mov	es, word ptr cs:[MouseVector+2] ; Dest is mouse driver
	call	MoveMouse	; Blat it in
	xor	cx,cx
	xor	dx,dx
	xor	si,si
	xor	di,di
	mov	ax,16		; show mouse cursor if it was on
	int	33H
	mov	ax,32		;; get mode we left it in
	int	33h
	mov	ax,33		;; set the mode
	int	33h
	call	MoveMouse
	mov	ax,1		;; restore the cursor
	int	33h
	ret

;----------------------------------------------------------------------------;
; SaveHPMouse - Save the HPMouse state by copying its data area.	     ;
;									     ;
;	The HP Mouse has two data areas V_LHPMOUSE and V_STRACK.	     ;
;	See the "HP Vectra Techinical Reference Manual" for details.	     ;
;	A new HP-HIL Mouse Microsoft Mouse driver has been released	     ;
;	by HP which was written by Microsoft. The NewHPMouse code	     ;
;	is used for this driver.					     ;
;									     ;
; Entry									     ;
;	ES:0 - mouse segment						     ;
;	CX   - Mouse size						     ;
;									     ;
; Exit									     ;
;	The mouse driver state is saved					     ;
;									     ;
; Uses									     ;
;	AX,BX,CX,DX,SI,DI,ES,DS						     ;
;----------------------------------------------------------------------------;

SaveHPMouse:

	EnterCrit
	xor	di, di			; MOUSE must be first
	mov	ax, V_LHPMOUSE
	mov	si, cs:[HPMouseSize]
	cCall	SaveData, <ax, di, si, di> ; Vector, start, end, loc
	add	di, si			; Save TRACK
	mov	ax, V_STRACK
	mov	si, cs:[TrackSize]
	xor	dx,dx
	cCall	SaveData, <ax, dx, si, di>
	LeaveCrit

	push	ax		   	;save
	mov	ax, 2			;hide the cursor
	int	33h
	pop	ax			;restore
	ret

;----------------------------------------------------------------------------;
; RestoreHPMouse							     ;
;									     ;
;	The HP Mouse has two data areas V_LHPMOUSE and V_STRACK.	     ;
;	See the "HP Vectra Techinical Reference Manual" for details.	     ;
;	A new HP-HIL Mouse Microsoft Mouse driver has been released	     ;
;	by HP which was written by Microsoft. The NewHPMouse code	     ;
;	is used for this driver.					     ;
;									     ;
; Entry									     ;
;	ES:0 - mouse segment						     ;
;	CX   - Mouse size						     ;
;									     ;
; Exit									     ;
;	The mouse driver state is restored				     ;
;									     ;
; Uses									     ;
;	AX,BX,CX,DX,SI,DI,ES,DS						     ;
;----------------------------------------------------------------------------;

RestoreHPMouse:

	xor	ax, ax
	int	33h
	EnterCrit
	xor	di, di			; MOUSE is always first
	mov	ax, es
	mov	ds, ax
	mov	ax, V_LHPMOUSE
	mov	si, cs:[HPMouseSize]
	cCall	RestoreData, <ax, di, si, di>; Vector, start, end, loc
	add	di, si			; Restore TRACK
	mov	ax, V_STRACK
	mov	si, cs:[TrackSize]
	xor	dx,dx
	cCall	RestoreData, <ax, dx, si, di>
	LeaveCrit
	ret

;----------------------------------------------------------------------------; 
;									     ;
; SaveOldMouse - Save the mouse state by copying the data area		     ;
;									     ;
;	As a freak of nature, old Microsoft mouse drivers stored	     ;
;	there data from CS:0 to CS:Entry. This allows us		     ;
;	to save the data area even though there are now explicit	     ;
;	calls to save it. However, not all the state might be		     ;
;	there.								     ;
;									     ;
; Entry									     ;
;	ES:0 - mouse segment						     ;
;	CX   - Mouse size						     ;
;									     ;
; Exit									     ;
;	The mouse driver state is saved					     ;
;									     ;
; Uses									     ;
;	AX,BX,CX,DX,SI,DI,ES,DS						     ;
;----------------------------------------------------------------------------;

SaveOldMouse:

	mov	ds,word ptr cs:[MouseVector+2]	; Source is mouse driver seg
	call	MoveMouse		;
	ret

;----------------------------------------------------------------------------;
;									     ;
; RestoreOldMouse - Restore the old mouse data area.			     ;
;									     ;
;	As a freak of nature, old Microsoft mouse drivers stored	     ;
;	there data from CS:0 to CS:Entry. This allows us		     ;
;	to save the data area even though there are now explicit	     ;
;	calls to save it. However, not all the state might be		     ;
;	there.								     ;
;									     ;
; Entry									     ;
;	ES:0 - mouse segment						     ;
;	CX   - Mouse size						     ;
;									     ;
; Exit									     ;
;	The mouse driver state is saved					     ;
;									     ;
; Uses									     ;
;	AX,BX,CX,DX,SI,DI,ES,DS						     ;
;----------------------------------------------------------------------------;

RestoreOldMouse:

	mov	ax, es
	mov	ds, ax
	mov	es, word ptr cs:[MouseVector+2] ; Dest is mouse driver
	call	MoveMouse	; Blat it in
	xor	cx,cx
	xor	dx,dx
	xor	si,si
	xor	di,di
	mov	ax,16		; show mouse cursor if it was on
	int	33H
	errn$	MoveMouse	; RestoreMouse falls into MoveMouse

;----------------------------------------------------------------------------; 
;									     ;
; MoveMouse - Transfer MouseSize bytes of mouse area			     ;
;									     ;
; ENTRY:								     ;
;	DS:0 -> Source of mouse area					     ; 
;	ES:0 -> Dest of mouse area					     ;
;	MouseSize is set						     ;
;									     ;
; EXIT:									     ;
;	Mouse area moved						     ;
;									     ;
; USES:									     ;
;	DI,SI,CX,DS,FLAGS						     ;
;----------------------------------------------------------------------------;

MoveMouse:

	cld
	xor	di,di
	xor	si,si
	mov	cx, cs:[MouseSize]
	shr	cx,1
	EnterCrit			; No Mouse INTs while copying
	rep	movsw
	LeaveCrit
	ret
;----------------------------------------------------------------------------;
sEnd	StubSeg

end
	


