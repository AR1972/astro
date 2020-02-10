COMMENT	#

	UNPACK.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	Functions for uncompressing an LZ compressed buffer.

	Created 11-28-89 johnhe
	TABS = 7

END_COMMENT #

RING_BUF_LEN	EQU	4096
UNPB_BUF_LEN	EQU	(512 * 17)
INDEX_LEN	EQU	2

WIN_MAX_STR_LEN	EQU	16
LANG_MAX_STR_LEN	EQU	(16 + INDEX_LEN)
MAX_STR_LEN		EQU	(LANG_MAX_STR_LEN + 2)

LANGUAGE_COMPRESS	EQU	1
WINDOWS_COMPRESS	EQU	2

; =========================================================================

DOSSEG

.MODEL	LARGE,C

.DATA

; =========================================================================
; UnpackSet has to allocated before a call to either ClearRingBuffer for
; Unpack. It will contain a ptr to a segment allocated for use by the
; unpack functions. When it is allocated it must be normalize to the
; allocated buffer + 16 so that it can be used with a zero offset by
; the unpack functions.
; =========================================================================

	PUBLIC	UnpackSeg
UnpackSeg	dd	(?)

; =========================================================================
; BUFFER_SEG is not a real segment in the program but is used to get the
; offsets into a buffer pointed to by UnpackSeg. By using this dummy segment
; it allows setting DS to WORD PTR UnpackSeg[2] and then accessing all of
; the area without using segment over-rides.
; =========================================================================

BUFFER_SEG 	SEGMENT at 00

	ORG	0
RingBuf		db	RING_BUF_LEN DUP (?)	; Must org @ 0
RingBufEnd	LABEL	BYTE

UnpackBuffer	db	UNPB_BUF_LEN DUP (?)
UnpackBufEnd	LABEL	BYTE

lToDo		dd	(?)		; Total bytes remaining in unpack buf
lWritten	dd	(?)		; Keeps track of total unpacked bytes

iOutFile	dw	(?)		; Open file handle passed by caller
CtrlFlags	dw	(?)		; Currunt control byte from upack buf
RingIndex	dw	(?)		; Start offset in RingBuffer
Char		db	(?)		; Current char from packed buffer

SplitPoint	db	(?)		; Determines where to start
OldSplitPoint	db	(?)		; Saves last spit point

BUFFER_SEG	ENDS

; =========================================================================

.CODE

EXTRN		fUpdateByteCount:FAR		; Must be explicitly FAR

; =========================================================================
; Sets up the ring buffer for a new file by filling the buffer with
; spaces and setting the ctrl flags and buffer index to known starting
; values.
;
; void ClearRingBuffer( int CompressType )
;
; ARGUMENTS:	int	- Compression type (0 == Language group, 1 == Windows)
; RETURNS: 	void
;
;	RingIndex =  MAX_STR_LEN (depending on compression type)
;	CtrlFlags = 0;
;	SplitPoint = 0;
;	memset( RingBuffer, ' ', RING_BUF_LEN - MAX_STR_LEN );
;
; =========================================================================

ClearRingBuffer PROC USES DS ES DI, CompressType:BYTE

	mov	AX,WORD PTR UnpackSeg[2]	; Get the allocated segment
	mov	DS,AX
	mov	ES,AX

	ASSUME	DS:BUFFER_SEG, ES:BUFFER_SEG

	xor	AX,AX				; DS:AX -> Start of ring buff
	mov	CtrlFlags,AX
	mov	SplitPoint,AL
	mov	DI,AX				; DS:DI -> Start of ring buff

	mov	RingIndex, RING_BUF_LEN - LANG_MAX_STR_LEN
	cmp	CompressType,WINDOWS_COMPRESS
	jne	@F
	mov	RingIndex, RING_BUF_LEN - WIN_MAX_STR_LEN 
@@:
	mov	AX,2020h			; AX == '  '
	mov	CX,(RING_BUF_LEN / 2)		; Storing words so use half
	cld
	rep	stosw				; Fill buffer with ' ' chars

	ret

ClearRingBuffer ENDP

; =========================================================================
; Uncompresses a portion of a compressed file which was compressed using
; LZ compression algorithm. Before the function is called the ring buffer
; and unpacked buffer must be allocated. Before each new file is unpacked
; a call must be done to ClearRingBuffer() to initialize the buffers and
; indices. As the buffer is unpacked it is written to the destination
; file when ever the index in the unpacked buffer reaches a point
; MAX_STR_LEN from the end of the buffer. Each time the buffer is flushed
; the ptr to the packed buffer must be normalized to prevent a possible
; segment wrap of the ptr.
; 																							
; long Unpack( int iFile, char far *InBuf, long lPackedBytes )			
; 																							
; ARGUMENTS:	iFile 	- Open DOS file handle to write the unpacked bytes
; 		InBuf 	- Ptr to buffer to be unpacked
; 			  lPackedBytes  - Length of the buffer in bytes
; RETURNS: 	long	- Number of bytes written to the file.
;
;
; Register will be setup and used as follows within the function
;
;	ES:BX	-> Next byte in packed buffer
;	DS:SI	-> Ring buffer
;	DS:DI	-> Next byte in unpack buffer
;	BP     ==  CtrlFlags
;	DX	   General use
;	CX	   General use
; =========================================================================

Unpack PROC USES DS ES DI SI,iFile:WORD, PackedBuf:PTR, lPackedBytes:DWORD

	mov	AX,WORD PTR UnpackSeg[2]; Get the allocated unpack segment
	mov	DS,AX

	ASSUME	DS:BUFFER_SEG

	les	AX,lPackedBytes		; ES:AX == number of bytes to unpack
	mov	WORD PTR lToDo,AX	; Move number for faster access
	mov	WORD PTR lToDo[2],ES	; lToDo = Total byte to unpacked

	mov	AL,SplitPoint		; Save split point in case this is
	mov	OldSplitPoint,AL	; a continuation of a previous unpack
	mov	SI,RingIndex		; DS:SI -> current pos. in ring buffer

	mov	AX,iFile		; Put iFile into new segment
	mov	iOutFile,AX

	les	BX,PackedBuf		; Get input buffer and normalize
	call	NEAR PTR NormalizePackPtr ; ES:BX -> normalize packed buffer

	mov	DI,OFFSET UnpackBuffer	; DS:DI -> Unpack buffer

	xor	AX,AX			; Reset everything else to 0
	mov	SplitPoint,AL
	mov	WORD PTR lWritten,AX
	mov	WORD PTR lWritten[2],AX


	push	BP			; Save BP for cleanup later
	mov	BP,CtrlFlags		; BP will always hold the ctrl flags

	cmp	OldSplitPoint,1		; See if this is a continuation
	je	SplitPoint1		; of a previous file and if so
	mov	AL,Char			; may need last char from unpack buf
	cmp	OldSplitPoint,2		; to start at the place where it
	je	SplitPoint2		; was left off last time

MainLoop:	; Start of loop which will continue until lToDo == 0

	mov	AX,WORD PTR lToDo	; See if all bytes are unpacked
	or	AX,WORD PTR lToDo[2]
	jnz	CheckUnpackBuf		; Continue if both bytes != 0
	jmp	UnpackExit		; Jump to successfull exit point

CheckUnpackBuf:				; See if time to flush buffer
	and	SI,0fffh		; Make ptr to wrap back to 0 if > 4095
	cmp	DI,OFFSET UnpackBufEnd - MAX_STR_LEN
	jl	GetNextChar 		; Buffer not full yet

	call	NEAR PTR FlushUnpackBuffer ; Else flush the buffer

	or	AX,AX			; AX will be -1 if error else 0
	jz	GetNextChar
	jmp	ErrorExit

GetNextChar:
	mov	AL,ES:[BX]		; Get next char from in buf
	inc	BX			; Increment packed buffer ptr
	sub	WORD PTR lToDo,1	; Decrement remaining packed bytes
	sbb	WORD PTR lToDo[2],0

AnyFlagsLeft:
	shr	BP,1			; High byte has bit mask of flags
	test	BP,0ff00h 		; Any flags left in this byte?
	jnz	TestFlag		; If yes go forward and check the flag

		; Set bit mask to for next 8 characters

	or	AX,0ff00h		; Set all bits in high byte for count
	mov	BP,AX

	mov	AX,WORD PTR lToDo	; See if all bytes are unpacked
	or	AX,WORD PTR lToDo[2]
	jnz	SplitPoint1		; Continue if both bytes != 0

		; If we get here it means we ran out of characters in the
		; unpacked buffer but there's still more to do on the next
		; call so set up to continue where we left off and then
		; flush the buffer and return. We we return on the next
		; call we already have the new control word can continue
		; where we left off

	mov	SplitPoint,1		; Set starting point for next call
	jmp	SHORT UnpackExit

SplitPoint1:				; At this point we just used the char
	mov	AL,ES:[BX]		; as ctrlbyte and need another char
	inc	BX			; Increment packed buffer ptr
	sub	WORD PTR lToDo,1	; Decrement remaining packed count
	sbb	WORD PTR lToDo[2],0

TestFlag:
	test	BP,1			; If flag == 1 character is not
	jz	UnpackIt		; special so just copy it

	mov	[DI],AL			; Store char in unpacked buffer
	mov	[SI],AL			; Also store it in the ring buffer
	inc	DI			; Point to next char in unpacked
	inc	SI			; and ring buffer
	jmp	MainLoop

		; Flag bit was 0 so get a string from ring buf
UnpackIt:
	mov	DX,WORD PTR lToDo	; See if all bytes are unpacked
	or	DX,WORD PTR lToDo[2]
	jnz	SplitPoint2		; Continue if both bytes != 0

		; If we get here it means we ran out of characters in the
		; unpacked buffer but there's still more to do on the next
		; call so set up to continue where we left off and then
		; flush the buffer and return. We we return on the next
		; call we already have the first byte of the control
		; word which determines the ringbuf offset and string count

	mov	Char,AL			; Save the byte, we need it next call
	mov	SplitPoint,2		; Set starting point for next call
	jmp	SHORT UnpackExit

SplitPoint2:	; offset in ringbuf = ((next char & 0xf0) << 4) + AL
		; length to copy    = (nextchar & 0x0f) + 2)

					; AL has last char from packed buffer
	mov	AH,ES:[BX]		; Get the ctrl byte from packed buf
	inc	BX			; Increment packed buffer ptr
	sub	WORD PTR lToDo,1	; Decrement remaining packed bytes
	sbb	WORD PTR lToDo[2],0

	xor	CX,CX			; Determine the str length in CX
	mov	CL,AH			; CX = ctrl byte from the packed buf
	and	CL,0fh			; Mask off the high nibble of low byte
	add	CL,3
;	inc	CL			; Add 2
;	inc	CL			; CX == count of bytes to copy


	shr	AH,1			; Put hi nibble into low nibble	of AH
	shr	AH,1			; and now AX will be a 12 bit value
	shr	AH,1			; which represents the offset in the
	shr	AH,1			; ring buffer

CopyString:
		; The string length may force a wrap of the ringbuffer so
		; we have to anticipate this by	having 2 methods of
		; copying. If the ring buffer wraps we do a slow copy that
		; normalizes SI after each movs instruction.

		; Don't need a CX check because the value will always
		; always be >= 3 because of the above ADD CX,3

	push	ES			; Save the unpacked segment

	mov	DX,SI			; DS:DX --> Dest ringbuf location

	mov	SI,AX			; DS:SI --> Source ringbuf location
	mov	AX,DS			
	mov	ES,AX			; ES:DI --> Dest unpackbuf location
					
	cld

DoCopy:
	lodsb				; Get source byte from ring buffer
	stosb				; Copy byte to unpacked buffer
	xchg	DI,DX			; DS:DI --> Dest ringbuf location
	stosb				; Copy byte to new ringbuf location
	xchg	DI,DX			; DS:DI --> Next unpackbuf location
	and	DX,0fffh		; Prevent wraps in the ring buffer
	and	SI,0fffh		; Have to do both source ptrs
	loop	DoCopy

	mov	SI,DX			; DS:SI --> Next ringbuf offset
	pop	ES			; ES:BX --> unpacked segment again
	jmp	MainLoop

		; ==========================================================
		; Function exit points. If the unpacked buffer has anything
		; copied to it which needs to be written it will be flushed
		; if there were no errors.
		; ==========================================================
UnpackExit:
	cmp	DI,OFFSET UnpackBuffer 	; See if anything in unpack buffer
	je	SetReturnCount

	call	NEAR PTR FlushUnpackBuffer ; Need to flush one last time
	or	AX,AX			; Check for error
	jnz	ErrorExit

SetReturnCount:
	les	AX,lWritten
	mov	DX,ES			; DX:AX == Unpacked bytes written
	jmp	SHORT UnpackExit2

ErrorExit:
	mov	AX,-1			; Signal error
	cwd				; DX:AX == -1

UnpackExit2:
	mov	RingIndex,SI		; Save current ring buffer index
	mov	CtrlFlags,BP
	pop	BP
	ret

Unpack	ENDP

; =========================================================================
; Flushes the unpack buffer to file and then resets the output buffer
; indices to be ready to starting filling the buffer again. After writing
; to the file a call is done to update the gage on the screen and
; then the unpacked ptr is normalized.
;
; int FlushUnpackBuffer( void )
;
; ARGUMENTS:	NONE																			*/
; RETURN:	int	- OK if disk write successfully else ERROR
;																						*/
; =========================================================================

FlushUnpackBuffer:

	push	SI
	mov	SI,BX			; Save unpacked offset for normalizing

	mov	DX,OFFSET UnpackBuffer	; DS:DX --> Start of unpacked buffer
	sub	DI,DX			; DI == count of bytes to write
	mov	CX,DI			; CX == Count of byte to write

	mov	AH,40h			; DOS write handle function
	mov	BX,iOutFile		; BX = Open file handle

	int	21h
	jc	FlushError		; Error check
	cmp	AX,DI			; See if all bytes were written
	jne	FlushError

	add	WORD PTR lWritten,CX	; Update total bytes
	adc	WORD PTR lWritten[2],0	; written to disk

		; Bytes were successfully written so setup and call
		; the function to update the gage on the screen

	push	DS
	push	ES

	mov	AX,@DATA		; Setup ES & DS to C data segment
	mov	DS,AX
	mov	ES,AX

	xor	AX,AX			
	push	AX			; Put byte count on the stack
	push	CX			; as a long value (dword)
					
	call	fUpdateByteCount	; Update the status gage C function
	add	SP,4			; Adjust SP for 2 pushes

	pop	ES
	pop	DS

	mov	BX,SI			; ES:BX --> packed buffer
	call	NEAR PTR NormalizePackPtr ; ES:BX -> normalize packed buffer

FlushExit:
	mov	DI,OFFSET UnpackBuffer	; Reset DS:DI to start of unpack buf
	pop	SI
	xor	AX,AX			; Return OK
	ret

FlushError:
	pop	SI
	mov	AX,-1			; Signal error
	ret

; =========================================================================
; Normalizes ptr to packed buffer in ES:BX
; =========================================================================

NormalizePackPtr:

	push	AX
	push	BX

	mov   	AX,ES			; AX:BX -> current packed buf location
	xchg	AX,BX			; Now BX == Segment, AX = Offset

	shr	AX,1			; Divid offset in AX by 16
	shr	AX,1
	shr	AX,1
	shr	AX,1
	add	AX,BX			; Add segment to normalized offset
	mov	ES,AX			; ES == Normalized segment

	pop	BX
	pop	AX

	and	BX,0fh			; Mask all but 4 low bits of offset

	ret				; ES:BX --> normalized packed location
					; Offset will be less than 16

; =========================================================================

END
