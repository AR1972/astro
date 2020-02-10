;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This file will hold all fuctions necessary for handling the grabber module ;
; and also code to implement certain aspects of pasting and context saveing  ;
;									     ;
; History:								     ;
;									     ;
;	 Fri June-15-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 Adapted for the Dos Task Switcher.				     ;
;									     ;
;        Tue June-20-1989.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created for Windows. (Added the History legend)      		     ;
;----------------------------------------------------------------------------;


	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include	woagrab.inc
	include grabber.inc
	include macros.mac
	include	njmp.mac
	include	woaerr.inc
	include woapif.inc
	.list

;----------------------------------------------------------------------------;
; declare any public function or varaiable names here.			     ;
;----------------------------------------------------------------------------;


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

	externNP	DosCall			;(WOARLM.ASM)
	externNP	GetMouseStateSize	;(WOAMOUSE.ASM)
	externNP	SaveMouseState		;(WOAMOUSE.ASM)
	externNP	RestoreMouseState	;(WOAMOUSE.ASM)
	externNP	EnableMouse		;(WOAMOUSE.ASM)
	externNP	DisableMouse		;(WOAMOUSE.ASM)
	externFP	Int16ISR		;(WOARLM.ASM)
	externNP	FindVectra		;(WOAHP.ASM)
	externNP	DisableInt15Mouse	;(WOARLM.ASM)
	externNP	EnableInt15Mouse	;(WOARLM.ASM)
	externNP	GetSizeOfInstanceBuffer	;(WOAINSTD.ASM)
	externNP	GetInstanceDataSnapShot	;(WOAINSTD.ASM)

;----------------------------------------------------------------------------;
; declare the variables which are defined elsewhere.			     ;
;----------------------------------------------------------------------------;

externW		WoaStubSize		;size of the real mode stub
externW		WoaSegResizeBlock	;segment of block to resize
externW		WoaSizeReservedArea	;reserved area at start of block
externW		GrabberParaSize		;size in para of grabber context
externB		WoaBehavior		;pif behavior flag
externB	        WoaHotKeys		;hot key disabling info
externB		WoaGrabberSwapPath	;grabber data swap path
externB		WoaGrabberName		;grabber load name
externB		WoaSwapDrive		;swap drive to be used
externW		MouseSeg		;segment where mouse context is saved
externW		InstDataSeg		;buffer for instance data management

;----------------------------------------------------------------------------;
; define the grabber related variables.				             ;
;----------------------------------------------------------------------------;

GrabberSeg		dw	?	;segment where grabber is loaded
GrabberScreenBufSeg	dw	?	;segment for saving the screen
GrabberScreenBufSize	dw	?	;size of the above segment
GrabberSize		dw	?	;size of the grabber.(<64k)
lpGrabberEntry		dd	?	;long pointer grabber call address
GrabberInfo		GrabInfo <>	;grabber info structure

;----------------------------------------------------------------------------;
; define some variables necessary for saving and restoring HP vectra state   ;
;----------------------------------------------------------------------------;

globalW HPsize,?			;size needed for saving HP state
globalB fVectra,0			;HP Vectra PC or not
globalW HPWindowsSeg,?			;HP state buffer for windows
globalW HPDosAppSeg,?			;HP state buffer for dos app

;----------------------------------------------------------------------------;

;----------------------------------------------------------------------------;
; LoadGrabberAndInit						  	     ;
;									     ;
; This is the first call related to the grabber that WOA makes and it does   ;
; the following:							     ;
;									     ;
;           .  If the machine is a HP Vectra, we will allocate two buffers   ;
;              right after the real mode stub code for saving the state of   ;
;              windows HP state and the old app HP state. If the PC is not a ;
;	       Vectra, these buffers will not be allocated.		     ;
;           .  Just after the place where the real mode stub is loaded (and  ;
;              the buffers for saving/restoring vectra state, if present, we ;
;              will be saving the mouse state, it saves the segment where    ;
;              the mouse state is to be saved and get the size of the state  ;
;              buffer.							     ;
;	    .  A buffer would be reserved for Instance data management after ;
;	       this.							     ;
;	    .  The grabber will be loaded into the space after the buffer    ;
;	       reserved for the Instance Data.				     ;
;	    .  It then finds out the size of the buffer needed for grabbing  ;
;              the screen and saves the address of the segment just after the;
;	       grabber code, where the buffer will be located.		     ;
;	    .  Invokes the grabber to tell it about the swapdrive and path.  ;
;----------------------------------------------------------------------------;


cProc	LoadGrabberAndInit,<NEAR,PUBLIC,PASCAL>

cBegin

; calculate the segment where the HP state buffers, mouse and grabber will be 
; loaded.

	call	FindVectra		;get the size of HP save area
	mov	ax,WoaSizeReservedArea	;get the size of the resrved area
	add	ax,WoaSegResizeBlock	;take past reserved area
	cmp	HPsize,0		;do we have a HP vectra PC ?
	jz	@f			;no.

; we have a vectra PC, allocate two buffers of AX paragraphs each and save the
; segment addresses.

	mov	HPWindowsSeg,ax		;save HP state for windows here
	add	ax,HPsize		;size of the buffer
	mov	HPDosAppSeg,ax		;segment for storing app HP state
	add	ax,HPsize		;size of the buffer

; add the size of the two buffers to GrabberParaSize

	push	ax			;save
	mov	ax,HPsize		;get the size in paras
	shl	ax,1			;two buffres so double it
	mov	GrabberParaSize,ax	;size of save area context till now
	pop	ax			;restore

@@:
	mov	MouseSeg,ax		;mouse state will be saved here
	push	ax			;save start of mouse save area
	cCall	GetMouseStateSize	;get the size of the buffer needed
	pop	bx			;get back the start segment
	add	GrabberParaSize,ax	;save size of gtabber related context
	add	ax,bx			;Instance data buffer will be here
	mov	InstDataSeg,ax		;for instance data segment

; find out the size of the instance data buffer.

	push	ax			;save start of instance data save area
	cCall   GetSizeOfInstanceBuffer	;size in AX (could be 0)
	pop	bx			;get back start segment

; Note: If AX is 0, the aove routine would have reset InstaDataSeg to 0.

	add	GrabberParaSize,ax	;save size of gtabber related context
	add	ax,bx			;here the grabber will be loaded
	mov	GrabberSeg,ax		;grabber will be loaded here

; create a snap shot of the current instance data state

	save	<ax>			;save
	cCall   GetInstanceDataSnapShot

; if switching is disabled, we need not load the grabber
; at all.

	push	ax			;save
	mov	al,WoaBehavior		;load the behaviour bits
	and	al,fNoGrabber		;mask switch and print screen bits
	cmp	al,fNoGrabber		;if it matches, we do not need grabber
	pop	ax			;restore
	jnz	@f			;grabber is required.

; grabber is not required

	clc				;no error
	jmp	short LoadGrabberAndInitRet;go back

@@:
	
; set up the call vector to start of grabber code

	mov	wptr [lpGrabberEntry+2],ax
	mov	wptr [lpGrabberEntry],0

; load in the grabber.

	cCall	LoadGrabber		;load in the grabber
	jc	LoadGrabberAndInitRet	;return back, cannot load grabber

; get the segment where we will have the screen save buffer

	mov	ax,GrabberSize		;get the size of the code 
	shiftr	ax,4			;get paragraphs
	inc	ax			;align to next paragraph
	add	ax,10h			;add 256 bytes for parameter area
	push	ax			;save the number of paragraphs
	add	ax,GrabberSeg		;ax now has buffer's segment
	mov	GrabberScreenBufSeg,ax	;save it
	pop	ax			;get back the no of paragraphs
	add	GrabberParaSize,ax	;save current size of grabber context

; now invoke the grabber to set the swapdrive and path. 

	mov	di,StubSegOFFSET WoaGrabberSwapPath
	smov	es,ds			;ES:DI points to the swap name
	mov	si,GR_INQUIREGRAB	;main function number
	mov	ax,GR_SETSWAPDRIVE	;subfunction
	mov	bl,WoaSwapDrive		;set up drive C for swap.
	cCall	InvokeGrabber		;invoke the grabber

;----------------------------------------------------------------------------;
; get the size of the buffer needed for saving the context of the screen     ;
; we will need a graphics sized buffer if fGraphics bit is set in the        ;
; WoaBehavior byte.				                             ;
;----------------------------------------------------------------------------;

	mov	ax,2			;assume graphics mode buffer size
	test	WoaBehavior,fGraphics	;marked for graphics screen exchange ?
	jnz	@f			;yes, we need graphics sized buffer
	mov	ax,1			;text sized buffer will suffice
@@:
	mov	si,GR_INQUIRESAVE	;function to get size of buffer
	cCall	InvokeGrabber		;get back size in DX:AX

; the size with the present grabber will never exceed 64k, so just use the
; low word

	mov	GrabberScreenBufSize,ax	;save buffer size

; now calculate the total size in paragraphs being taken up by the grabber
; and the screen save buffer.
		   
	shiftr	ax,4			;get the buffer size in paragraphs
	inc	ax			;update to the next para
	add	GrabberParaSize,ax	;save the total size

	clc				;no error detected

LoadGrabberAndInitRet:

cEnd
;----------------------------------------------------------------------------;
;GrabberSaveScreen:							     ;
;									     ;
; This routine invokes the grabber (which is resident as a part of the stub) ;
; to save the old app screen.		         			     ;
;----------------------------------------------------------------------------;

cProc	GrabberSaveScreen,<NEAR,PUBLIC,PASCAL>

cBegin

; save the mouse state first

	cCall	SaveMouseState		;mouse state variables saved

; now disable the mouse

	cCall	DisableMouse		;mouse disabled

; invoke the save screen function

	mov	es,GrabberScreenBufSeg	;get the segment for the save area
	mov	ax,GrabberScreenBufSize ;the size of save area allocated
	xor	di,di			;offset in buffer starts at 0
	mov	si,GR_SAVESCREEN	;save screen call
	cCall	InvokeGrabber		;call the grabber function
	pushf				;save the carry flag

; now enable and initialize the mouse and return

	cCall	EnableMouse		;reenable the mouse
	popf				;get back save screen status
	jc	SaveScreenFails		;save screen call failed

; Disable INT 15 Mouse if a local INT 15 mouse handler is active.

	call	DisableInt15Mouse	
	clc				;success.
	jmp	short GrabberSaveScreenRet;go back.

SaveScreenFails:

; the save screen call failed, must restore mouse state

	cCall	EnableMouse		;reenable the mouse
	cCall	RestoreMouseState	;restore mouse state
	stc				;save screen has failed.

GrabberSaveScreenRet:

cEnd
;----------------------------------------------------------------------------;
; GrabberRestoreScreen:							     ;
;								             ;
; This routine invokes the grabber (which is a part of the old app context)  ;
; and asks it to restore the old app screen from the saved image.	     ;
;----------------------------------------------------------------------------;

cProc	GrabberRestoreScreen,<NEAR,PUBLIC,PASCAL>

cBegin

; disable the mouse so that it does not interfere with restore screen

	cCall	DisableMouse		;disable the mouse

; call the RestoreScreen function

	mov	es,GrabberScreenBufSeg	;get the segment for the save area
	mov	ax,GrabberScreenBufSize ;size of the buffer area allocated
	xor	di,di			;offset in buffer starts at 0
	mov	si,GR_RESTORESCREEN	;save screen call
	cCall	InvokeGrabber		;call the grabber function

; enable the mouse befor retoring it's state

	cCall	EnableMouse		;reeneable the mouse

; enable the INT 15 mouse if the INT 15 mouse had been disabled.

	call	EnableInt15Mouse

; now restore the mouse state

	cCall	RestoreMouseState	;restore mouse state

cEnd
;----------------------------------------------------------------------------;
; GrabberEnableSave:							     ;
;									     ;
; Calls the EnableSave function in the grabber if it is loaded.		     ;
;----------------------------------------------------------------------------;

cProc	GrabberEnableSave,<NEAR,PUBLIC,PASCAL>

cBegin

; if the grabber is not loaded, we cannot execute this function

	mov	al,WoaBehavior		;load the state of the hotkeys
	and	al,fNoGrabber		;mask switch and print screen bits
	cmp	al,fNoGrabber		;if it matches, we do not need grabber
	jz	GrabberEnableSaveRet	;no grabber loaded
	mov	si,GR_INQUIREGRAB	;the main function code
	mov	ax,GR_ENABLESAVE	;the sub function code
	cCall	InvokeGrabber		;call the grabber function

GrabberEnableSaveRet:

cEnd
;----------------------------------------------------------------------------;
; GrabberDisableSave:							     ;
;									     ;
; Calls the grabber DisableSave function if the grabber is loaded            ;
;----------------------------------------------------------------------------;

cProc	GrabberDisableSave,<NEAR,PUBLIC,PASCAL>

cBegin

; if the grabber is not loaded, we cannot execute this function

	mov	al,WoaBehavior		;load the state of the hotkeys
	and	al,fNoGrabber		;mask switch and print screen bits
	cmp	al,fNoGrabber		;if it matches, we do not need grabber
	jz	GrabberDisableSaveRet	;no grabber loaded
	mov	si,GR_INQUIREGRAB	;main function code
	mov	ax,GR_DISABLESAVE	;sub function code
	cCall	InvokeGrabber		;call the grabber function

GrabberDisableSaveRet:

cEnd
;----------------------------------------------------------------------------;
; GrabberInitScreen:							     ;
;									     ;
; Initailizes and clears the screen by seting it to a known text state.      ;
;----------------------------------------------------------------------------;

cProc	GrabberInitScreen,<NEAR,PUBLIC,PASCAL>

cBegin

; if the grabber is not loaded, we cannot execute this function

	push	ax			;save no of lines to load
	mov	al,WoaBehavior		;load the state of the hotkeys
	and	al,fNoGrabber		;mask switch and print screen bits
	cmp	al,fNoGrabber		;if it matches, we do not need grabber
	pop	ax			;restore number of lines to load
	jz	GrabberInitScreenRet	;no grabber loaded
	mov	si,GR_INITSCREEN	;the function code
	cCall	InvokeGrabber		;invoke the grabber
	clc				;can be no error, grabbers don't do this

GrabberInitScreenRet:

cEnd
;----------------------------------------------------------------------------;
; This routine is responsible for loading the grabber in from the load time  ;
; grabber file.								     ;
;----------------------------------------------------------------------------;


cProc	LoadGrabber,<NEAR,PUBLIC,PASCAL>

cBegin

; open the grabber file

	mov	dx,StubSegOFFSET WoaGrabberName
	mov	ax,3d00h		;need to open a r/o file
	int	21h			;try to open file
	jc	LoadGrabberRet		;load failure
	mov	bx,ax			;have the handle in BX

; get the size of the file by doing an LSEEK of 0 bytes from the end and
; looking at the offset

	xor	cx,cx			;need zero for start offset
	xor	dx,dx			;so cx=dx=0
	mov	ax,4202h		;need to seek from end
	int	21h			;does the seek, DX:AX = size
	mov	GrabberSize,ax		;save low word of size, ignore DX

; now rewind and read the file.

	xor	cx,cx			;set to 0
	xor	dx,dx			;cx:dx = 0
	mov	ax,4200h		;lseek from the start
	int	21h			;rewound.

; now read in the file in one go. Code size will be less than 64k

	push	ds			;save
	xor	dx,dx			;start offset = 0
	mov	cx,GrabberSize		;size of read data
	mov	ds,GrabberSeg		;segment to load data into
	mov	ah,3fh			;read code
	int	21h			;read in the file
	pop	ds			;get back data segment

; close the file
	
	mov	ah,3eh			;close code
	int	21h			;file closed

LoadGrabberRet:

cEnd
;----------------------------------------------------------------------------;
; This routine invokes the grabber in real mode. All resgisters (incl ES)    ;
; set up the way the grabber expects it. Particularly, SI must have the      ;
; offset of the function being called.					     ;
;----------------------------------------------------------------------------;

cProc	InvokeGrabber,<NEAR,PUBLIC,PASCAL>

cBegin

	push	ds			;save
	mov	wptr [lpGrabberEntry],si;set the offset of entry
	mov	ds,GrabberSeg		;have ds same as cs
	assumes	ds,nothing

	call	lpGrabberEntry		;invole the grabber
	pop	ds
	ret

InvokeGrabber	endp
;----------------------------------------------------------------------------;

sEnd  StubSeg

end


	
