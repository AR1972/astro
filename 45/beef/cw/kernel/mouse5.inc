;*
;*	COW : Character Oriented Windows
;*
;*	mouse5.inc : DOS 5 specific mouse info

;*****************************************************************************

; Mouse monitor record flags
;
fOpen	equ    00000001b
fClose	equ    00000010b
fFlush	equ    00000100b

; Mouse monitor record event masks
;
mouMotionOnly	equ 0000000000000001b	; motion only
mouButMotion	equ 0000000000101010b   ; motion with button
mouButton	equ 0000000001111110b   ; buttons, with or without motion
mouBut1Only	equ 0000000000000100b   ; button 1 only
mouBut1		equ 0000000000000110b   ; button 1, with or without motion
mouBut2Only	equ 0000000000010000b   ; button 2 only
mouBut2		equ 0000000000011000b   ; button 2, with or without motion
mouBut3Only	equ 0000000001000000b   ; button 3 only
mouBut3		equ 0000000001100000b   ; button 3, with or without motion
mouAny		equ 0000000001111111b	; any

; Mouse monitor record structure
;
MouseRcd	STRUC
	Flags		DW	?	; Open, Flush, Close or 0
	Eventmsk	DW	?	; Event mask (buttons)
	TimeStamp  	DD	?	; Time stamp of the mouse event
	ayMouCur   	DW	?	; new Y
	axMouCur   	DW	?	; new X
MouseRcd  	ENDS

;* Mouse Area
AreaMou	STRUC
	ayTopArea	DW	?	;* top line
	axLeftArea	DW	?	;* left column
	dayArea		DW	?	;* height
	daxArea		DW	?	;* width
AreaMou	ENDS

;* Mouse Pointer Shape
PtrShapeMou	STRUC
	cbPtrShapeMou		dw	?
	colPtrShapeMou		dw	?
	rowPtrShapeMou		dw	?
	colHotPtrShapeMou	dw	?
	rowHotPtrShapeMou	dw	?
PtrShapeMou	ENDS


;* DOS 5 calls

externFP	<DosOpen,DosClose>
externFP	<DosMonClose,DosMonOpen,DosMonReg,DosMonRead,DosMonWrite>
externFP	<DosCreateThread,DosSetPrty>
externFP	<MouOpen,MouClose,MouGetNumButtons>
externFP	<MouDrawPtr,MouRemovePtr,MouGetPtrPos>
externFP	<MouSetPtrPos,MouSetPtrShape>
;externFP	<MouGetPtrShape>

