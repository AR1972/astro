;	TITLE	SETMODE - Mode oriented routines for MSHERC.
;***
;SETMODE
;
;	Copyright <C> 1987, 1988, Microsoft Corporation
;
;Purpose:
;	Set graphics mode, get video state and check hardware card
;	status.
;
;******************************************************************************

	include	hgcdefs.inc

code     segment para public 'code'
         assume  cs:code,ds:code

Public	GetVideoState
Public	SetGraphicsMode
Public	ReturnHGCStatus
Public	GetHGCStatus

Extrn	ConfigMode:byte		;[1] either FULL or HALF mode
Extrn	UnHookFlag:byte
Extrn	DMC_Save:byte		;temp variable for the DMC_Port value
Extrn	HardGraphVals:byte	;Start of Graphics Mode Hardware Reg data
Extrn	Pause:near		;Procedure that counts vsync pulses, and
				;in effect executes a pause...

;-------------------------------------------------------------------------------
; F  CURRENT VIDEO STATE
;	 AL <= Current Mode (8)
;	 AH <= # Character Columns
;	 BH <= Active Display Page (currently always 0)
;-------------------------------------------------------------------------------
GetVideoState	proc near
	mov	ah,es:BIOSCOLS
	mov	al,HGCGrMode
	mov	FunData.FunAX,ax
	mov	al,es:BIOSPAGE
	mov	byte ptr FunData.FunBX[1],al
	ret
GetVideoState	endp

;-------------------------------------------------------------------------------
; 0  SET MODE
;	 AL => mode #8, bit 7, when set, prevents regen buffer clear.
;    Pass on all but request for Hercules graphics mode.
;    Trap ALL functions (except EGAINT10) while in graphics mode.
;    This function should be ignored if BIOS equipment variable not
;    set for B/W.
;-------------------------------------------------------------------------------
SetGraphicsMode proc near
	mov	ah,es:BIOSEQUIP
	and	ah,00110000B		;is B/W mode
	cmp	ah,00110000B
	je	StrtGMode		;go if OK
	ret				;no action if not B/W (mono)
StrtGMode:
	mov	DMC_Save,GraphicsOn+GraphicsPage0+ScreenOff

	push	ax			;save bit 7 (regen clear flag)
	xor	ax,ax
	mov	byte ptr es:BIOSMODE,8		    ;set bios mode number
	mov	word ptr es:BIOSCOLS,80 	    ;set # text columns
	mov	byte ptr es:BIOSPAGE,al 	    ;default page 0
	mov	word ptr es:BIOSCURS,ax 	    ;clear page 0 cursor
	cmp	[ConfigMode],FULL		    ;[1] make sure FULL mode
	jne	SkipPage1		            ;[1] brif not
	mov	word ptr es:BIOSCURS+2,ax	    ;clear page 1 cursor
SkipPage1:					    ;[1]
	mov	word ptr es:BIOSSTART,ax	    ;set buffer start
	mov	word ptr es:BIOSLEN,GraphBufferSize ;    and length
	mov	UnHookFlag,al			    ;Clear the unhook flag

FinGraphicsEntry:

;------Program the 6845 for Graphics Mode-------
	mov	si,offset HardGraphVals	;6845 Graphics values
	mov	cx,CRTValsCnt		;# of data values to program
	mov	ah,Register0		;Start with 6845 register 0

	cli				;Disable interrupts
	mov	dx,DMC_Port		;Use Display Mode Control Port
	mov	al,DMC_Save		;Fetch the DMC value
	out	dx,al			;Disable video
	Call	ProgramRegisters	;Program the 6845 registers
	mov	dx,Config_Port		;Use the configuration port
	mov	al,[ConfigMode] 	;[1] Fetch the config. value
	out	dx,al			;Set Configuration Port

;--------Wait for screen to settle-------
	mov	cx,delay
	Call	Pause

;-------Clear the screen------
	pop	ax			;restore bit 7 (regen clear flag)
	test	al,80H			;bit 7 set to preserve regen buffer?
	jnz	DontClear
	mov	ax,Scr0_Buf_Seg		;Assume Page 0
	push	es
	mov	es,ax			;Pnt es to graphics page
	xor	di,di			;Point di to beg. of buffer offset
	xor	ax,ax
	mov	cx,GraphBufferSize	;Size of both graphics pages in words
	cmp	[ConfigMode],FULL	;[1] make sure in FULL mode
	je	clearit			;[1] brif so
	shr	cx,1			;[1] only clear one page if HALF mode
clearit:				;[1]
rep	stosw				;Store blanking value in graphics buf
	pop	es

DontClear:
;------Set the font character height and width --------
	mov	dx,DMC_Port		;Use the Display Mode Control Port
	mov	al,DMC_Save		;Fetch the video-off value
	or	al,ScreenOn		;Set the video-on bit
	mov	DMC_Save,al		;Save it
	out	dx,al			;Set DMC Port and turn on screen
        ret	;Finished Graphics Mode Set
SetGraphicsMode endp

ReturnHGCStatus proc near
;-------Adjust pointers--------
	call	GetHGCStatus
	mov	word ptr FunData.FunDX,dx   ;[1] Return to user
	ret
ReturnHGCStatus endp

GetHGCStatus	proc near
	mov	dx,DS_Port
	in	al,dx
	and	al,IDMask
	cmp	al,HGC222ID	;Is this a 222?
	jnz	Chk112		;No, check for a 112
	mov	dl,2				;Return to user
	jmp	FiniStatReq			;Finished
Chk112:
	cmp	al,HGC112ID	;Is this a 112?
	jnz	Chk102		;No, check for a 102
	mov	dl,1				;Return to user
	jmp	FiniStatReq			;Finished
Chk102:
	MOV	DX,3BAH        	; Bit 7 at port 3BA changes pretty often
	MOV	CX,0FFFFH      	; on a Hercules so check it to see if
	IN	AL,DX	       	; it flips.
	MOV	BL,AL
tst102:
	IN	AL,DX			; Current value of Port 3BA
	XOR	AL,BL			; XOR to see if it changes
	TEST	AL,80H			; Did it?
	JNZ	got102			; Yes, we must have a Hercules
	LOOP	tst102	 		; No, try again
	mov	dx,-1			;[2] Indicate non-Hercules
	JMP	SHORT FiniRetDef	;[2] If we didn't get it, give up
got102:
	xor	dl,dl				;Indicate 102

FiniStatReq:
	mov	dh,[ConfigMode]		;[1] return FULL = 1 or HALF = 0
	shr	dh,1			;[1]
FiniRetDef:				;[2]
	ret	;Finished HGC Status Request
GetHGCStatus	endp

;-----------------------------------------------------------------------------;
;				ProgramRegisters
;
;	Loads a consecutive set of registers with the values from the
;	specified table via the Index/Data Ports.
;
;               Entry:  ds:si = offset of data table
;			ah = 1st register number
;			cx = # registers to load
;
;		Uses:	ax,cx,dx,si
;
;	**Notes: This routine is practical for programming 3 or more data
;		values
;
;-----------------------------------------------------------------------------;
ProgramRegisters	proc	near
	cli				;Disable interrupts
ProgRegs:
	mov	dx,IndexReg		;Use Index Register
	mov	al,ah			;Get Desired Register Number
	out	dx,al			;Point to Register
	inc	dx			;Use Data Port
	lodsb				;Fetch data value from table
	out	dx,al			;Output data value
	inc	ah			;Increment to next register number
	loop	ProgRegs		;Loop to output next data value
	sti				;Enable interrupts

         ret				;Finished
ProgramRegisters	endp

code     ends
         end
