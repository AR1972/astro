;	TITLE	FVARS - Global data for MSHERC.
;***
;FVARS
;
;	Copyright <C> 1987, 1988, Microsoft Corporation
;
;Purpose:
;	Global data for MSHERC.
;
;******************************************************************************

	include	hgcdefs.inc

code            segment para public 'code'
                assume  cs:code,ds:code

Extrn	GetVideoState:near
Extrn	SetGraphicsMode:near
Extrn	Null_Function:near
Extrn	SetCursor:near
Extrn	ReadCursor:near
Extrn	SetActivePage:near
Extrn	WriteDot:near
Extrn	ReadDot:near
Extrn	WriteTTY:near
Extrn	GSetCursorType:near,GScrollUp:near,GScrollDown:near
Extrn	GReadAttrChar:near,WriteGrChar:near,GWriteChar:near

Public	Old_Int10h_Routine
Public	GrFuncTable
Public	DMC_Save,HardGraphVals
Public	DeltaCols,DeltaRows
Public	YTable,UnHookFlag
Public	Bit0
Public	LEdge,REdge,BitPos,DstEdgeMasks
Public	ZERO
Public	CharHeight
Public	CharBuff
Public	ScrollLines
Public	ConfigMode		;[1]

;---------INT 10H - Functions Dispatch Table----------

GrFuncTable	dw	offset SetGraphicsMode
		dw	offset GSetCursorType
		dw	offset SetCursor
		dw	offset ReadCursor
		dw	offset Null_Function
		dw	offset SetActivePage
		dw	offset GScrollUp
		dw	offset GScrollDown
		dw	offset GReadAttrChar
		dw	offset WriteGrChar
		dw	offset GWriteChar
		dw	offset Null_Function
		dw	offset WriteDot
		dw	offset ReadDot
		dw	offset WriteTTY
		dw	offset GetVideoState

Old_Int10h_Routine       dd    ?	;Segment:Offset of previous INT 10H
UnHookFlag		db	1	;Set to disable HGC Video Routines
					;Clear to enable HGC Video Routines
ConfigMode		db	FULL	;[1] default to FULL mode (3)

ZERO		dw	0

DMC_Save	db	?	;temp variable for the DMC_Port value

;-------6845 register values for HGC graphics mode-----
HardGraphVals	db	35H,2DH,2EH,7,5BH,2,57H,57H,2,3,0,0,0,0

;-------Temporary variables for the Scrolling Procedures------
DeltaCols	dw	?	;Number of columns in scroll window
DeltaRows	dw	?	;Number of rows in scroll window
ScrollLines	dw	?	;number of scan lines to scroll

;------Table of bit position masks------
Bit0	db	00000001b
Bit1	db	00000010b
Bit2	db	00000100b
Bit3	db	00001000b
Bit4	db	00010000b
Bit5	db	00100000b
Bit6	db	01000000b
Bit7	db	10000000b

;------Table of Left Edge Masks--------
LEdge	db	11111110b
	db	11111100b
	db	11111000b
	db	11110000b
	db	11100000b
	db	11000000b
	db	10000000b
	db	00000000b

;------Table of Right Edge Masks
REdge	db	00000000b
	db	00000001b
	db	00000011b
	db	00000111b
	db	00001111b
	db	00011111b
	db	00111111b
	db	01111111b

;------Table y coordinate offsets for use in graphics mode calculations-----
YTable	label	word
	?ROW = 0	;row number
	REPT	350	;loop for each row (scan line)
	;compute regen buffer offset for this row
	DW	2000H * (?ROW MOD 4) + (?ROW / 4) * 90
	?ROW = ?ROW + 1 ;next row
	ENDM

;------- Graphics Mode Variables ----------
DstEdgeMasks	dw	?	;Dest Left(LSB) and Right(MSB) Edge Masks
BitPos		db	?	;Bit Position in Graphics Byte
CharHeight	db	14
CharBuff	db	14 dup (?)  ;hold buffer for reading graphics character
code	ends
	end
