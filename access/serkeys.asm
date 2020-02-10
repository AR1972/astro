;
;
	TITLE   serkeys.a

ALL_LINK	equ 1


;********* GLOBAL FUNCTIONS
	
	EXTRN	_serialKeysOn:byte
	EXTRN	_initGIDEI:near
	EXTRN	ExtendedSeg:word
	EXTRN	_fmouse_id:byte
	EXTRN	_comp_id:byte
	EXTRN   _combase:word

	PUBLIC	_serialKeysEnableFar
	PUBLIC	_serialKeysDisableFar
	PUBLIC	_putInMouseBuffer
	PUBLIC	_sendMouseData

	PUBLIC _mouBufTailPtr
	PUBLIC _mouBufHeadPtr
	PUBLIC _mouBuffer

	PUBLIC	_doBeep
	PUBLIC	_doReboot
	PUBLIC	_handleBreak
	PUBLIC	_inPauseCondition
	PUBLIC	_clearInPause
	PUBLIC	_setInPause

	PUBLIC	_serialKeysCommInit
	PUBLIC	_serialKeysDisable
	PUBLIC	_initCommPort
	PUBLIC	_setBaudRate
	PUBLIC	_turnOffHandshake
	PUBLIC	_turnOnHandshake
	PUBLIC	_disableComm
	PUBLIC	_enableComm
	PUBLIC	_clearOutComm

	PUBLIC	_disableKeyEnhance

	PUBLIC	_waitingForIndicatorUpdate
	PUBLIC	_getBiosFlags
	PUBLIC	_putBiosFlags
	PUBLIC	_keyBufferFull
	PUBLIC	_keyBufferEmpty
	PUBLIC	_putInKbdBuffer
	PUBLIC	_convertStringToInt

;********* GLOBAL VARIABLES
	PUBLIC	_tryingToWriteKeyboardData

	PUBLIC	_altSuppressedTbl
	PUBLIC	_shiftSuppressedTbl
	PUBLIC	_ctrlSuppressedTbl
	
;********* FUNCTIONS
	EXTRN	_doSerial:NEAR
	EXTRN	_kickStartSerialKeys:NEAR
	EXTRN	_serialKeysInit:NEAR
	EXTRN	Put_Mouse_Data:NEAR

;********* VARIABLES
	EXTRN	beep_low:near
	EXTRN	fFilterKeysOn:BYTE
	EXTRN	fDialog_Filter_off:BYTE
	EXTRN	fDialog_Action:BYTE
	EXTRN	_skCommPortAddr:WORD
	EXTRN	_skCommIRQ:WORD
	EXTRN	_skBaudRate:WORD
	EXTRN	_vector:BYTE
	EXTRN	_kbFlag:BYTE
	EXTRN	_kbFlag1:BYTE
	EXTRN	_kbFlag2:BYTE
	EXTRN	_altKeypad:BYTE
	EXTRN	_aliasStr:BYTE
	EXTRN	_inSerialKeys:BYTE
	
	EXTRN	_end:ABS

;********* LOCAL FUNCTIONS
	
	; serialInt	PROC NEAR
	; timerInt	PROC NEAR
	; skInt15Routine	PROC NEAR

no_key		equ	0
NOKEY		equ	0
lquote_key	equ	1
one_key		equ	2
two_key		equ	3
three_key	equ	4
four_key	equ	5
five_key	equ	6
six_key		equ	7
seven_key	equ	8
eight_key	equ	9
nine_key	equ	10
zero_key	equ	11
hyphen_key	equ	12
equal_key	equ	13
backspace_key	equ	15

tab_key		equ	16
q_key		equ	17
w_key		equ	18
e_key		equ	19
r_key		equ	20
t_key		equ	21
y_key		equ	22
u_key		equ	23
i_key		equ	24
o_key		equ	25
p_key		equ	26
lbracket_key	equ	27
rbracket_key	equ	28
bslash_key	equ	29

caps_key	equ	30
a_key		equ	31
s_key		equ	32
d_key		equ	33
f_key		equ	34
g_key		equ	35
h_key		equ	36
j_key		equ	37
k_key		equ	38
l_key		equ	39
semicolon_key	equ	40
rquote_key	equ	41
return_key	equ	43

lshift_key	equ	44
z_key		equ	46
x_key		equ	47
c_key		equ	48
v_key		equ	49
b_key		equ	50
n_key		equ	51
m_key		equ	52
comma_key	equ	53
period_key	equ	54
fslash_key	equ	55
rshift_key	equ	57

lcontrol_key	equ	58
lcommand_key	equ	59
lalt_key	equ	60
space_key	equ	61
ralt_key	equ	62
rcommand_key	equ	63
rcontrol_key	equ	64

insert_key	equ	75
delete_key	equ	76
left_key	equ	79
home_key	equ	80
end_key		equ	81
up_key		equ	83
down_key	equ	84
pageup_key	equ	85
pagedown_key	equ	86
right_key	equ	89

numlock_key	equ	90
kp7_key		equ	91
kp4_key		equ	92
kp1_key		equ	93
kpfslash_key	equ	95
kp8_key		equ	96
kp5_key		equ	97
kp2_key		equ	98
kp0_key		equ	99
kpstar_key	equ	100
kp9_key		equ	101
kp6_key		equ	102
kp3_key		equ	103
kpperiod_key	equ	104
kpminus_key	equ	105
kpplus_key	equ	106
kpequal_key	equ	107
kpenter_key	equ	108

escape_key	equ	110

f1_key		equ	112
f2_key		equ	113
f3_key		equ	114
f4_key		equ	115

f5_key		equ	116
f6_key		equ	117
f7_key		equ	118
f8_key		equ	119

f9_key		equ	120
f10_key		equ	121
f11_key		equ	122
f12_key		equ	123

print_key	equ	124
scroll_key	equ	125
pause_key	equ	126
reset_key	equ	127

shift_key	equ	lshift_key
control_key	equ	lcontrol_key
alt_key		equ	lalt_key

TRUE		equ 1
FALSE		equ 0

;COM1		equ	3F8h
;COM2		equ	2F8h
;COM3		equ	3E8h
;COM4 		equ	2E8h

IRQ4		equ	4
IRQ3		equ	3

RBR		equ	0
THR 		equ	0
DLL 		equ	0
IER 		equ	1
DLM 		equ	1
FCR 		equ	2
IIR 		equ	2
LCR 		equ	3
MCR 		equ	4
LSR 		equ	5

XON		equ	17
XOFF		equ	19

RTS_BIT		equ	2
DTR_BIT		equ	1

DR_FLAG		equ	01h
OR_FLAG		equ	02h
PE_FLAG		equ	04h
FE_FLAG		equ	08h
BI_FLAG		equ	010h
THRE_FLAG	equ	020h
TSRE_FLAG	equ	040h

RDA_INT		equ	01h
THRE_INT	equ	02h
RLS_INT		equ	04h

NOMOUSE		equ 0
BUSMOUSE	equ 1
SERIALMOUSE	equ 2
INPORTMOUSE	equ 3
PS2MOUSE	equ 4
HPMOUSE		equ 5

PAUSE_MODE_MASK		equ 08h
KBDINDICATORUPDATE	equ 01000000b

bios_seg	segment at 40h
	org 017h
kbFlag		db ?
kbFlag1		db ?
altKeypad	db ?

	org 01Ah
kbBufHeadPtr	dw ?
kbBufTailPtr	dw ?

	org 071h
breakState	db ?
resetFlag	dw ?

	org 080h
kbBufStartPtr	dw ?
kbBufEndPtr	dw ?

	org 096h
kbFlag2		db ?
kbLEDFlag	db ?

bios_seg ends


rom_seg segment at 0f000h
	org 0fff0h
reset	label far
rom_seg ends

_TEXT	SEGMENT  WORD PUBLIC 'CODE'
_TEXT	ENDS
CONST	SEGMENT  WORD PUBLIC 'CONST'
CONST	ENDS
_DATA	SEGMENT  WORD PUBLIC 'DATA'
_DATA	ENDS
_BSS	SEGMENT  WORD PUBLIC 'BSS'
_BSS	ENDS
DGROUP	GROUP _TEXT, CONST, _DATA, _BSS




_TEXT      SEGMENT



	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing

;**********************************************************************

	PUBLIC serkeysStartOfCode
serkeysStartOfCode	label byte
	even

oldSerialVect	DD 0

		even
skSaveSP	dw 0
skSaveSS	dw 0
		db 'SKb'
		even
skStack		dw 128 dup(0)
skTopOStack	dw $-2
		db 'SKt'


_mouBuffer	db	90 dup (0)
_mouBufferEnd	dw	$
_mouBufTailPtr	dw	_mouBuffer
_mouBufHeadPtr	dw	_mouBuffer

_tryingToWriteKeyboardData db 0


_altSuppressedTbl	db g_key, h_key, rquote_key, b_key, n_key, fslash_key, space_key, left_key
			db up_key, down_key, right_key, kp0_key, kpperiod_key, kpminus_key
			db escape_key, f4_key, f5_key, f6_key, 0,0

_shiftSuppressedTbl	db backspace_key, tab_key, t_key, y_key, lbracket_key, rbracket_key, caps_key
			db return_key, z_key, x_key, c_key, v_key, m_key, comma_key, period_key
			db rcontrol_key, kp4_key, kpfslash_key, kp5_key, kpstar_key, kp6_key
			db f3_key, f7_key, pause_key, 0, 0

_ctrlSuppressedTbl	db lquote_key, five_key, six_key, hyphen_key, equal_key, return_key, z_key
			db x_key, c_key, v_key, m_key, comma_key, period_key, rshift_key
			db insert_key, delete_key, home_key, pageup_key, kpfslash_key, kpstar_key
			db f1_key, f2_key, f8_key, f9_key, pause_key, 0, 0

;**********************************************************************
	ASSUME	CS: _TEXT
	ASSUME	DS: NOTHING
	ASSUME  SS: NOTHING
	ASSUME  ES: NOTHING

	PUBLIC	serialInt
serialInt PROC FAR
	cli
	cmp	cs:_inSerialKeys,TRUE
	je	serialInt10
	mov	cs:skSaveSP,sp
	mov	cs:skSaveSS,ss
	push	cs
	pop	ss
	mov	sp,offset skTopOStack

serialInt10:

	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	ds
	push	es
	push	bp
	mov	ax,cs
	mov	ds,ax
	mov	es,ax
	ASSUME	DS: _TEXT
	ASSUME	ES: _TEXT

	cld
	call	_doSerial			; call our routine

	cli
	pop	bp
	pop	es
	pop	ds
	ASSUME	DS: NOTHING
	ASSUME	ES: NOTHING
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax

	cmp	cs:_inSerialKeys,TRUE
	je	serialIntDone
	mov	ss,cs:skSaveSS
	mov	sp,cs:skSaveSP
serialIntDone:

	iret	

serialInt ENDP



;**********************************************************************

	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
_doBeep	proc	near

IFNDEF ALL_LINK
	push	ax
	push	bx
	push	cx
	mov	bx,200h
	in	al,61h
	push	ax
k65:
	and	al,0fch
	out	61h,al
	mov	cx,40h
k66:
	loop	k66
	or	al,2
	out	61h,al
	mov	cx,40h
k67:
	loop	k67
	dec	bx
	jnz	k65
	pop	ax
	out	61h,al
	pop	cx
	pop	bx
	pop	ax
	ret
ELSE
	jmp beep_low
	ret
ENDIF

_doBeep	endp


;**********************************************************************

	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME  SS: NOTHING
	ASSUME  ES: NOTHING

;**********************************************************************
_doReboot PROC NEAR
	; don't bother to save regs since computer will be rebooting
	mov	bx,bios_seg
	mov	es,bx
	assume	es:bios_seg
	mov	resetFlag,1234h			;signal for soft reset
	jmp	reset
	assume	es:nothing
_doReboot ENDP


;**********************************************************************

_handleBreak PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing

	push	es
	mov	bx,bios_seg
	pushf
	cli
	mov	es,bx
	assume	es:bios_seg
	mov	bx,word ptr kbBufStartPtr	; clear out kb buffer
	mov	word ptr kbBufTailPtr,bx
 	mov	word ptr kbBufHeadPtr,bx
	mov	al,080h				; set break bit
	mov	byte ptr breakState,al
	int	1Bh				; do break interrupt
	sub	ax,ax
	push	ax				; send dummy character
	call	_putInKbdBuffer
	pop	bx
	popf
	pop	es
	assume	es:nothing
	ret	

_handleBreak ENDP


;**********************************************************************

_inPauseCondition	PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	es
	push	bx
	mov	bx,bios_seg
	mov	es,bx
	assume	es:bios_seg
	sub	al,al
	test	byte ptr kbFlag1,PAUSE_MODE_MASK
	jz	inPauseConditionDone
	mov	al,1
inPauseConditionDone:
	pop	bx
	pop	es
	assume	es:nothing
	ret	

_inPauseCondition	ENDP

;**********************************************************************

_setInPause PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	es
	mov	bx,bios_seg
	mov	es,bx
	assume	es:bios_seg
	or	byte ptr kbFlag1,PAUSE_MODE_MASK
	pop	es
	assume	es:nothing
	ret	

_setInPause ENDP

;**********************************************************************

_clearInPause PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	es
	mov	bx,bios_seg
	mov	es,bx
	assume	es:bios_seg
	and	byte ptr kbFlag1, NOT PAUSE_MODE_MASK
	pop	es
	assume	es:nothing
	ret	

_clearInPause ENDP


;**********************************************************************

_serialKeysCommInit	PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME  SS: NOTHING
	ASSUME  ES: NOTHING

	push	ax
	push	bx
	push	cx
	push	dx
	push	es

	; disable serial interrupt at 8259 controller
	in	al,21h				; get 8259 interrupt mask
	cmp	byte ptr _skCommIRQ,IRQ4	; which IRQ
	jne	commInit100
commInit50:
	or	al,10h				; mask IRQ 4
	out	21h,al	
	mov	ax,350Ch			; setup for getting old com1 vector
	jmp	short commInit200
commInit100:
	or	al,8h				; mask IRQ 3
	out	21h,al
	mov	ax,350Bh			; setup for getting old com2 vector
commInit200:
	int	21h				; get vector (es:bx)

	; see if our routine was installed already
	cmp	bx,OFFSET serialInt		; see if vector offset is ours
	jne	commInit400			; jump if not ours
	mov	dx,es				; get vector segment
	mov	cx,cs
	cmp	dx,cx				; see if vector segment is ours
	jne	commInit400			; jump if not ours
	jmp	short commInit1000		; ours already here so skip

	; our routine was not installed so save old vectors
	; and put ours in instead
commInit400:
	mov	word ptr cs:oldSerialVect,bx
	mov	word ptr cs:oldSerialVect+2,es

	push	ds
	mov	dx,cs
	mov	ds,dx
	mov	dx,OFFSET serialInt
	mov	ah,25h
	int	21h
	pop	ds

commInit1000:
	call	_initCommPort			; initialize the comm port
	pop	es
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
_serialKeysCommInit	ENDP

;**********************************************************************

_serialKeysEnableFar	proc far
	push	ds
	push	es
	push	cs
	push	cs
	pop	ds
	pop	es
	call	_serialKeysInit
	mov	_serialKeysOn,TRUE			; set SerialKeys on ?
	pop	es
	pop	ds
	retf
_serialKeysEnableFar  endp


_serialKeysDisableFar	proc far
	push	ds
	push	es
	push	cs
	push	cs
	pop	ds
	pop	es
	mov	_serialKeysOn,FALSE			; set SerialKeys on ?
	call	_serialKeysDisable
	pop	es
	pop	ds
	retf
_serialKeysDisableFar  endp

_serialKeysDisable	PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME  SS: NOTHING
	ASSUME  ES: NOTHING

	push	ax
	push	bx
	push	cx
	push	dx
	push	es

	; get serial interrupt vectors
	cmp	byte ptr _skCommIRQ,IRQ4
	jne	skDisable100
skDisable50:
	mov	ax,350Ch
	jmp	short skDisable200

skDisable100:
	mov	ax,350Bh
skDisable200:
	int	21h

	; see if our routine was installed already
	cmp	bx,OFFSET serialInt		; see if vector offset is ours
	jne	skDisable1000			; jump if not ours
	mov	bx,es				; get vector segment
	mov	cx,cs
	cmp	bx,cx				; see if vector segment is ours
	jne	skDisable1000			; jump if not ours

	cli
	; we are installed so now disable our routine
	mov	bx,word ptr _skCommPortAddr
	mov	dx,bx
	add	dx,MCR
	in	al,dx
	jmp	$+2
	and	al,NOT 0Fh
	out	dx,al
	mov	dx,bx
	add	dx,IER
	mov	al,0
	out	dx,al
	call	_clearOutComm

	in	al,21h				; get 8259 int masks
	cmp	byte ptr _skCommIRQ,IRQ4	; which IRQ
	jne	skDisable300
skDisable250:
	or	al,10h				; mask IRQ 4
	out	21h,al
	mov	ax,250Ch			; setup for com1 vectors
	jmp	short skDisable400

skDisable300:
	or	al,8h				; mask IRQ 3
	out	21h,al
	mov	ax,250Bh			; setup for com2 vector
skDisable400:
	; restore old serial vector
	push	ds
	mov	dx,word ptr cs:oldSerialVect+2
	mov	ds,dx
	mov	dx,word ptr cs:oldSerialVect
	int	21h
	cli
	pop	ds
	call	_initGIDEI
	call	_clearOutComm

skDisable1000:
	pop	es
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret	

_serialKeysDisable	ENDP

;**********************************************************************
_initCommPort	PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing

	push	ax
	push	bx
	push	dx
	call	_turnOffHandshake
	call	_disableComm
	call	_clearOutComm
	mov	bx,word ptr _skCommPortAddr
	mov	ax,0
	mov	dx,bx
	add	dx,FCR
	out	dx, al				; turn off FIFO
	call	_setBaudRate
	in	al,21h				; get 8259 int mask
	cmp	byte ptr _skCommIRQ,IRQ4	; which IRQ
	jne	initCommPort100
initCommPort50:
	and	ax,0EFh				; unmask IRQ 4
	jmp	SHORT initCommPort200
initCommPort100:
	and	ax,0F7h				; unmask IRQ 3
initCommPort200:
	out	21h,al				; write updated mask byte
	call	_enableComm
	mov	dx,bx
	add	dx,MCR
	mov	al,08h
	out	dx,al
	call	_turnOnHandshake
	pop	dx
	pop	bx
	pop	ax
	ret	

_initCommPort	ENDP


;**********************************************************************

_setBaudRate	PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing

	push	ax
	push	bx
	push	cx
	push	dx
	mov	ax,0083h			; set DLAB = 1
	mov	cx,word ptr _skCommPortAddr	; save for later
	mov	dx,cx
	add	dx,LCR
	out	dx,al
	jmp	short $+2

	mov	ax,word ptr _skBaudRate	; set baud rate, LSB then MSB
	mov	dx,cx
	add	dx,DLL
	out	dx, al
	jmp	short $+2

	mov	dx,cx
	add	dx,DLM
	xchg	al,ah
	out	dx,al
	jmp	short $+2
	mov	ax,3				; set DLAB=0 (and word protocol 8,n,1)
	mov	dx,cx
	add	dx,LCR
	out	dx,al
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
_setBaudRate	ENDP

;**********************************************************************

_turnOnHandshake PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	ax
	push	dx
	mov	dx,word ptr _skCommPortAddr
	add	dx,MCR
	in	al,dx
	jmp	$+2
	or	al,RTS_BIT+DTR_BIT
	out	dx,al				; turn on handshaking
	mov	dx,word ptr _skCommPortAddr
;	add	dx,THR
	mov	al,XON
	out	dx,al
	pop	dx
	pop	ax
	ret
_turnOnHandshake ENDP

;**********************************************************************

_turnOffHandshake PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	ax
	push	dx
	mov	dx,word ptr _skCommPortAddr
	add	dx,MCR
	in	al,dx
	jmp	$+2
	and	al,NOT (RTS_BIT+DTR_BIT)
	out	dx,al				; turn off handshaking
	jmp	$+2
	mov	dx,word ptr _skCommPortAddr
;	add	dx,THR
	mov	al,XOFF
	out	dx,al
	pop	dx
	pop	ax
	ret
_turnOffHandshake ENDP


;**********************************************************************

_disableComm PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	ax
	push	dx
	mov	dx,word ptr _skCommPortAddr
	add	dx,IER				; turn off int on anything
	sub	al,al
	out	dx,al
	pop	dx
	pop	ax
	ret
_disableComm ENDP

;**********************************************************************

_enableComm PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	ax
	push	dx
	mov	dx,word ptr _skCommPortAddr
	add	dx,IER
	mov	al,RLS_INT+RDA_INT
	out	dx,al
	pop	dx
	pop	ax
	ret
_enableComm ENDP


;**********************************************************************

_clearOutComm PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	ax
	push	bx
	push	dx
	mov	bx,word ptr _skCommPortAddr
	mov	dx,bx
	add	dx,RBR
	in	al,dx				; clear out any pending character
	jmp	short $+2
	mov	dx,bx
	add	dx,LSR
	in	al,dx				; reset status int
	jmp	short $+2
	mov	dx,bx
	add	dx,IER
	in	al,dx				; reset THRE int
	jmp	short $+2
	mov	dx,bx
	add	dx,RBR
	in	al,dx				; clear out any pending characters
	jmp	short $+2
	pop	dx
	pop	bx
	pop	ax
	ret
_clearOutComm ENDP

;**********************************************************************
_disableKeyEnhance PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing

	cmp	fFilterKeysOn,TRUE
	jne	disableKE_end
	mov	fDialog_Filter_off,TRUE
	mov	fDialog_Action,TRUE
disableKE_end:
	ret

_disableKeyEnhance ENDP


;**********************************************************************
_waitingForIndicatorUpdate PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	sub	ax,ax
	push	es
	push	bx
	mov	bx,bios_seg
	pushf
	cli
	mov	es,bx
	assume	es:bios_seg
	test	kbLEDFlag,KBDINDICATORUPDATE
	jz	waitingForIndDone
	mov	ax,1
waitingForIndDone:
	popf
	pop	bx
	pop	es
	assume	es:nothing
	ret
_waitingForIndicatorUpdate ENDP


;**********************************************************************

_getBiosFlags PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	es
	mov	bx,bios_seg
	pushf
	cli
	mov	es,bx
	assume	es:bios_seg
	mov	bl,byte ptr kbFlag
	mov	_kbFlag,bl
	mov	bl,byte ptr kbFlag1
	mov	_kbFlag1,bl
	mov	bl,byte ptr kbFlag2
	mov	_kbFlag2,bl
	mov	bl,byte ptr altKeypad
	mov	_altKeypad,bl
	popf
	pop	es
	assume	es:nothing
	ret
_getBiosFlags ENDP
	

;**********************************************************************

_putBiosFlags PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	es
	mov	bx,bios_seg
	pushf
	cli
	mov	es,bx
	assume	es:bios_seg
	mov	bl,_kbFlag
	mov	byte ptr kbFlag,bl
	mov	bl,_kbFlag1
	mov	byte ptr kbFlag1,bl
	mov	bl,_kbFlag2
	mov	byte ptr kbFlag2,bl
	mov	bl,_altKeypad
	mov	byte ptr altKeypad,bl
	popf
	pop	es
	assume	es:nothing
	ret
_putBiosFlags ENDP
	


;**********************************************************************

_keyBufferFull	PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	sub	ax,ax
	push	es
	push	bx
	mov	bx,bios_seg
	pushf
	cli
	mov	es,bx
	assume	es:bios_seg
	mov	bx,word ptr kbBufTailPtr	;get pointer to next available spot
	inc	bx				;increment to next
	inc	bx
	cmp	bx,word ptr kbBufEndPtr		; should pointer rap around
	jb	keyBufferFull100		; jump if not
	mov	bx,word ptr kbBufStartPtr	; rap pointer to beginning of buffer
keyBufferFull100:
	cmp	bx,word ptr kbBufHeadPtr	; is buffer full
	jne	keyBufferFullDone		; jump if not full
	mov	ax,1				; signal buffer full
keyBufferFullDone:
	popf
	pop	bx
	assume	es:nothing
	pop	es
	ret	

_keyBufferFull	ENDP

;**********************************************************************

_keyBufferEmpty	PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	sub	ax,ax
	push	es
	push	bx
	mov	bx,bios_seg
	pushf
	cli
	mov	es,bx
	assume	es:bios_seg
	mov	bx,word ptr kbBufHeadPtr	;does head=tail
	cmp	bx,word ptr kbBufTailPtr
	jne	keyBufferEmptyDone		;jump if not empty
	mov	ax,1				;signal that it is full
keyBufferEmptyDone:
	popf
	pop	bx
	pop	es
	assume	es:nothing
	ret
_keyBufferEmpty	ENDP


;**********************************************************************

_putInKbdBuffer	PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	bp
	mov	bp,sp
	push	es
	push	si
	mov	bx,bios_seg
	pushf
	cli
	mov	es,bx
	assume	es:bios_seg
	mov	bx,word ptr kbBufTailPtr	; get pointer to next available spot
	mov	si,bx				; save it for later
	inc	bx				; increment to next
	inc	bx
	cmp	bx,word ptr kbBufEndPtr		; should pointer rap around
	jb	putInKbdBuffer100		; jump if not
	mov	bx,word ptr kbBufStartPtr	; rap pointer to beginning of buffer
putInKbdBuffer100:
	cmp	bx,word ptr kbBufHeadPtr	; is buffer full
	je	putInKbdBufferDone		; jump if not full
	mov	ax,word ptr [bp+4]		; get code from stack
	mov	es:[si],ax			; put byte in buffer
	mov	word ptr kbBufTailPtr,bx	; update new tail pointer
putInKbdBufferDone:
	popf
	pop	si
	pop	es
	assume	es:nothing
	mov	sp,bp
	pop	bp
	ret	

_putInKbdBuffer	ENDP


;**********************************************************************

_sendMouseData	PROC NEAR
	pushf
	push	ax
	push	bx
	push	dx
	mov	bx,_mouBufHeadPtr
	cmp	bx,_mouBufTailPtr
	je	sendMouDataDone
	cmp	_fmouse_id,4			; do we have a PS/2 mouse ?
	jne	sendMouData50

	push	es
	sub	ax,ax
	call	getFromMouseBuffer		; ax has the data upon return
	push	ax				; push status
	call	getFromMouseBuffer		; ax has the data upon return
	push	ax				; push X
	call	getFromMouseBuffer		; ax has the data upon return
	push	ax				; push Y
	sub	ax,ax
	push	ax				; push Z
	mov	ax,ExtendedSeg
	mov	es,ax
	call	dword ptr es:[22h]
	cmp	_comp_id,6
	je	sendMouData10
	int	74h
	jmp	sendMouData30
sendMouData10:
	int	71h
sendMouData30:
	add	sp,+8
	pop	es
	jmp	sendMouDataDone

sendMouData50:
	cmp	_fmouse_id,2			; do we have a serial mouse ?
	jne	sendMouDataDone
	mov	dx,_combase			; put address of combase in dx

; first check to be sure the byte we may have transmitted previously has been sent out
	cli
	add	dx,LSR
	in	al,dx
	test	al,THRE_FLAG+TSRE_FLAG		; bit 5, transmit buffer should be clear if okay to write
	jz	sendMouDataDone
	call	getFromMouseBuffer		; ax has the data upon return
	mov	dx,_combase			; put address of combase in dx
	out	dx,al		
sendMouDataDone:
	pop	dx
	pop	bx
	pop	ax
	popf
	ret
_sendMouseData	ENDP

;**********************************************************************
_putInMouseBuffer PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	bp
	mov	bp,sp
	push	si
	push	bx				; temp store of bx register
	pushf
	cli
	mov	bx,_mouBufTailPtr		; get tail pointer of mouse_data buffer
	mov	si,bx				; save pointer value
	inc	bx
	inc	bx
	inc	bx
	cmp	bx,OFFSET _mouBufferEnd		; are we at the end of the buffer ?
	jb	putInMouBuf100			; no
	mov	bx,OFFSET _mouBuffer		; yes we are, so reset to the buffer beginning
putInMouBuf100:
	cmp	bx,_mouBufHeadPtr		; has the buffer wrapped around ?
	je	putInMouBufDone
	mov	ax,[bp+4]			; STATUS
	mov	[si],al				; move whats in ax into address pointed to by si
	inc	si
	mov	ax,[bp+6]			; X
	mov	[si],al				; move whats in ax into address pointed to by si
	inc	si
	mov	ax,[bp+8]			; Y
	mov	[si],al				; move whats in ax into address pointed to by si
	mov	_mouBufTailPtr,bx		; update tail pointer
putInMouBufDone:
	popf
	pop	bx
	pop	si
	mov	sp,bp
	pop	bp
	ret
_putInMouseBuffer ENDP

;**********************************************************************
getFromMouseBuffer PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	bx				; temp store of bx register
	pushf
	cli
	mov	bx,_mouBufHeadPtr		; get tail pointer of mouse_data buffer
	cmp	bx,_mouBufTailPtr
	je	getFromMouBufDone
	mov	al,[bx]
	inc	bx
	cmp	bx,OFFSET _mouBufferEnd		; are we at the end of the buffer ?
	jb	getFromMouBuf100			; no
	mov	bx,OFFSET _mouBuffer		; yes we are, so reset to the buffer beginning
getFromMouBuf100:
	mov	_mouBufHeadPtr,bx		; update tail pointer
getFromMouBufDone:
	popf
	pop	bx
	ret
getFromMouseBuffer ENDP

;**********************************************************************

_convertStringToInt PROC NEAR
	ASSUME	CS: _TEXT
	ASSUME	DS: _TEXT
	ASSUME	SS: nothing
	ASSUME	ES: nothing
	
	push	si
	mov	si,offset _aliasStr		;get address of string
	xor	ax,ax				;clear upper byte
	cwd
	xor	bx,bx				;clear storage
	lodsb					;get first character
	push	ax				;save it for later
	cmp	al,'+'				;if + or - then get next char
	je	StrToInt2
	cmp	al,'-'
	jne	StrToInt3			;not + or - so jump
StrToInt2:
	lodsb					;get numeric char
StrToInt3:
	cmp	al,0				;at end of string?
	je	StrToIntDone			;jump if done
	cmp	al,'9'				;make sure it is a number
	ja	StrToIntError			;
	sub	al,'0'
	jb	StrToIntError
	;valid number so now convert to integer
	;multiply running sum by 10 and add current
	shl	bx,1				;multiply by 2 and save for later
	mov	cx,bx				;
	shl	bx,1				; x4 now
	shl	bx,1				; x8 now
	add	bx,cx				; x8 + x2 = x10
	add	bx,ax				;add current value
	jmp	SHORT StrToInt2			;continue until all done
StrToIntError:
	pop	ax				;clean up stack
	mov	bx,offset _aliasStr
	xor	ax,ax				;signal bad string
	mov	byte ptr [bx],al
	xor	ax,0FFFFH			;make sure larger than 256
	jmp	SHORT StrToInt6
StrToIntDone:
	;ok we are done converting to integer
	;Now lets see if it is supposed to be a number or a command code
	pop	ax				;get back first character
	cmp	al,'-'				;if - then a number
	xchg	ax,bx				;switch for easier processing
	jne	StrToInt6			;not a - so jump
StrToInt5:
	neg	ax				;convert to a negative
StrToInt6:
	pop	si
	ret
_convertStringToInt ENDP


_TEXT	ENDS
	
;	END	serialKeysMain

	end
