;***
; STACK.INC - 3-Sep-87 - Frame definitions
;***
	.XLIST
;***
;
; Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************


RUN_RESERVE= 14h		;Paragraphs for EXE loader

;******************************************************************************
;
;Frame Contents
;==============
;
; The basic frame is set up on entry to the main program, by B$ENSA/B$ENRA SUB
; and FUNCTION entry routines, and on an event GOSUB. SZ_FRAME is the size in
; bytes of the standard basic frame.
;
;		+------+------+------+------+------+------+------+------+
;		|							|
;	+4/+8	+	Return address Segment				+
;		|							|
;		+------+------+------+------+------+------+------+------+
;		|							|
;	+2/+6	+	Return address offset				+
;		|							|
;IF EVENT GOSUB +------+------+------+------+------+------+------+------+
;		|							|
;	+4	+	Context flag					+
;		|							|
;		+------+------+------+------+------+------+------+------+
;		|							|
;	+2	+	Trap table index				+
;		|							|
;END EVENT GOSUB+------+------+------+------+------+------+------+------+
;	BP  --> |							|
;		+	Previous BP					+
;		|							|
;		+------+------+------+------+------+------+------+------+
;	-2  --> |							|
;		+	Previous BASIC frame pointer			|
;		|							|
;		+------+------+------+------+------+------+------+------+
;	-4  --> |							|
;		+	Saved SI					+
;		|							|
;		+------+------+------+------+------+------+------+------+
;	-6  --> |							|
;		+	Saved DI					+
;		|							|
;		+------+------+------+------+------+------+------+------+
;	-8  --> |							|
;		+	Count of local bytes on stack			|
;		|							|
;    		+------+------+------+------+------+------+------+------+
;       -10->   |							|
;		+	User program current GOSUB count		|
;		|							|
;		+------+------+------+------+------+------+------+------+
;		|							|
;
;
; Event frame info
;
FR_EVRETSEG	EQU	8	;[5] Return address segment for EVENT returns
FR_EVRETOFF	EQU	6	;[5] Return address offset for EVENT returns
;
; Return address to routine which FAR CALLed a BASIC SUB or FUNCTION. Placed by
; CALL instruction.
;
FR_RETSEG	EQU	4	;[3] return address segment
FR_RETOFF	EQU	2	;[3] return address offset
;
; BP on entry into BASIC SUB or FUNCTION for standard BP chain. PUSHed by
; Compiled code when the switches are not specified, by B$ENSA/B$ENRA in other
; cases.
;
FR_BP		EQU	0	;[10] Previous BP
;
; Previous BASIC frame. The value of [b$curframe] is saved here by B$ENSA /
; B$ENRA such that B$EXSA can restore it on exit. Should always be the first
; thing pushed after BP, for futre optimizations.
;
FR_BFRAME	EQU	-2	;[10] Previous BASIC frame
;
; Saved SI & DI. Preserved for C compatability. Saved by B$ENSA / B$ENRA /
; B$ENSD / B$ENRD restored by B$EXSA.
;
FR_SI		EQU	-4	;[10] SI for C compat, also QBI text pointer
FR_DI		EQU	-6	;[10] DI for C compat
;
; Count of local bytes of storage allocated on stack by compiler generated
; code. Placed by B$ENSA/B$ENRA. Used at error time to recover state prior to
; any ON ERROR.
;
FR_CLOCALS	EQU	-8	;[10] count of locals on stack
;
;
; Gosub count used only in /D/E/X compiled programs. Used to ensure that
; RETURNs to not exceed GOSUBs. Zeroed by B$ENSA / B$ENRA, otherwise maintained
; by GOSUB / RETURN and multi-line DEF code. Also used at error time to recover
; state prior to ON ERROR.
;
; Must be last on the frame to optimize recursive zero-fill.
;
FR_GOSUB	EQU	-10	;[10] GOSUB count
;
; Size in bytes of the items "pushed" onto the stack after BP.
;
FR_SIZE 	EQU	10	;[3] Frame size (without word temps)

	.LIST
