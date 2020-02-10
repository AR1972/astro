;***
;HGCDEFS - Global definitions for MSHERC.
;***
	.XLIST
;***
;
;	Copyright <C> 1987, 1988, Microsoft Corporation
;
;Purpose:
;	Global definitions for MSHERC.
;
;******************************************************************************

;--------------------------------------------------------------------;
;			Equates
;--------------------------------------------------------------------;
LowerFuncMax	equ	0FH	;Max func # for implemented IBM lookalikes
InquireFuncNo	equ	0EFH		;Inquire Driver Function Number
HGCGrMode	equ	8		;BIOS mode number for HGC graphics

INT10HVECTOR_OFFSET	EQU	10h*4	;INT 10H Code Offset
INT10HVECTOR_SEGMENT	EQU	10h*4+2	;INT 10H Code Segment

;------Registers------
IndexReg		equ    	03B4H	;6845 CRT Controller Index Register
DataReg			equ	03B5H	;6845 CRT Controller Data Register
DMC_Port		equ	03B8H	;Display Mode Control Port
DS_Port			equ	03BAH	;Display Status Port
Config_Port		equ	03BFH	;Configuration Port
Register0		equ	0	;6845 Register 0

Scr0_Buf_Seg		equ	0B000H	;Display Buffer Segment
Scr1_Buf_Seg		equ	0B800H	;Graphics Page 1 Segment

;-----Configuration Port Values--------
DIAG		equ	0		;Graphics Mode and Page1 Disabled
HALF		equ	1		;Graphics Mode enabled,Page1 disabled
FULL		equ	3		;Graphics Mode and Page1 enabled

;------ Hercules ID Code Values ------
IDMask		equ	01110000b	;Mask for HGC ID Code bits
HGC112ID	equ	00010000b	;HGC GB112 ID Code
HGC222ID	equ	01010000b	;HGC GB222 ID Code

;------Display Mode Control Port Values--------
GraphicsOn	equ	00000010b	;Selects Graphics Mode
TextOn		equ	00000000b	;Selects Text Mode
ScreenOn	equ	00001000b	;Activates video
ScreenOff	equ	00000000b	;Disables video
BlinkOn		equ	00100000b	;Enable the Text Blink Attribute
BlinkOff	equ	00000000b	;Disable the Text Blink Attribute
GraphicsPage0	equ	00000000b	;Graphics Display Segment = 0B000H
GraphicsPage1	equ	10000000b	;Graphics Display Segment = 0B800H
GraphBufferSize	equ	8000h		;Graphics Buffer size = 32k bytes

Delay			equ	20	;Number of vsyncs to pause for a
					;mode change
CRTValsCnt		equ	14	;# bytes of 6845 Register Data

BIOSMODE	EQU	449H	;BIOS screen mode number
BIOSCURS	EQU	450H	;BIOS Offset of current cursor position
BIOSCOLS	EQU	44AH	;BIOS Offset of current screen columns
BIOSPAGE	EQU	462H	;BIOS Offset of current page number
BIOSSTART	EQU	44EH	;BIOS Offset of current page buffer start
BIOSLEN 	EQU	44CH	;BIOS page length
BIOSEQUIP	EQU	410H	;BIOS equipment information
BIOSCURSMODE	EQU	460H	;BIOS cursor type

;-------Parameter Offsets In Stack-------
FunData		equ	[bp]

RegStrc	struc		;Structure for saved registers

	FunAX		dw	?
	FunBX		dw	?
	FunCX		dw	?
	FunDX		dw	?

RegStrc	ends

;----- Graphics Mode Constants -------
GrNextScan	equ	2000H		;Point to next scan in graphics mode
NextScanAdjust	equ	8000H-90	;Adjusts next scan ptr when off-screen

;------ Write TTY equates ------
Bell		equ	7
BackSpace	equ	8
CarriageReturn	equ	0DH
LineFeed	equ	0AH
