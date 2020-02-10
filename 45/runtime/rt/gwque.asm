	TITLE	GWQUE -- General Queue support routines
;***
; GWQUE -- General Queue support routines
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   B$INITQ   -   Initialize queue header
;   B$PUTQ    -   Enqueue a byte
;   B$GETQ    -   dequeues a byte
;   B$NUMQ    -   returns # bytes queued
;   B$LFTQ    -   returns space available in queue
;               ( B$NUMQ + B$LFTQ + 1 = # bytes allocated in buffer )
;
;   These routines are all called with SI pointing to a 12-byte queue
;   descriptor, defined as Que_ctrl_block, below.
;
;******************************************************************************

	include switch.inc	
	INCLUDE rmacros.inc	;Runtime Macro Defintions

	useSeg	RT_TEXT 	;Runtime Core

	INCLUDE seg.inc 	

Que_ctrl_block struc
Queput	dw	?		; queue put pntr
Queget	dw	?		; queue get pntr
Quebot	dw	?		; bot of queue location
Quetop	dw	?		; top of queue location
Quelen	dw	?		; length of queue = maximum queable bytes + 1
Quenum	dw	?		; number of bytes in que
Que_ctrl_block ends

Que_header_size = size que_ctrl_block

sBegin	RT_TEXT 		
assumes CS,RT_TEXT		

	PUBLIC	B$INITQ
	PUBLIC	B$PUTQ
	PUBLIC	B$GETQ
	PUBLIC	B$NUMQ
	PUBLIC	B$LFTQ		;[$]

;B$GETQ   - General Dequeue routine
;entry  - SI -> queue descriptor
;exit   - ZR flag   1 if queue underflow
;         AL        byte queued (if ~underflow)
;         Others    Preserved
B$GETQ:				;[$]
	push	bx
	mov	bx,[si].queget
	cmp	bx,[si].queput	;is queue empty?
	jz	nochar		;it is
	mov	al,[bx]		;Get char
	inc	bx		;Bump get
	cmp	bx,[si].quetop	;wrap-around?
	jnz	nwrdqu		;No
	mov	bx,[si].quebot	;Get Que Bot
nwrdqu:
	mov	[si].queget,bx	;Store new get ptr
	dec	[si].quenum	;One less in Queue
	inc	bx		;make certain that Z-flag is not zero
nochar:	pop	bx
	ret

;B$PUTQ   - General queue routine
;entry  - SI     -> queue descriptor
;         AL        byte to be queued
;exit   - ZR flag   1 if queue overflow
;         AH        can be used
;         Others    Preserved
B$PUTQ:				;[$]
	push	bx
	mov	bx,[si].queput	;bottom of queue
	mov	[bx],al		;Put char in queue
	inc	bx		;Advance put
	cmp	bx,[si].quetop
	jnz	nwrque		;No
	mov	bx,[si].quebot	;Get Que Bot
nwrque:
	cmp	bx,[si].queget	;overflow?
	jz	povrflo		;yes - ignore this put
	mov	[si].queput,bx	;no  - store new put ptr
	inc	[si].quenum	;One more in Queue
povrflo:
	pop	bx
	ret

;B$INITQ  - Initialize queue descriptor for empty queue
; entry - [SI]  uninitialized queue descriptor
;         [BX]  first byte of queue buffer
;         AX    number of bytes in buffer
; exit  - AX    used
;         Rest  Preserved
B$INITQ:				;[$]
	mov	[si].queget,bx	;reset pointers to
	mov	[si].queput,bx	;beginning of queue buffer
	mov	[si].quebot,bx	;ditto
	mov	[si].quelen,ax	;length of queue
	add	ax,bx		;now points to top of queue
	mov	[si].quetop,ax	;length of queue
	mov	[si].quenum,0	;[RDK]clear queue counter.
	ret

;B$LFTQ   - How many times can B$PUTQ be successfully called for queue SI
; entry - [SI]  queue descriptor
; exit  - AX    number of free bytes in buffer

B$LFTQ:	MOV	AX,[SI].QUELEN	;[$]
	sub	ax,[si].quenum
	dec	ax
	ret

;B$NUMQ - How many times can B$GETQ be successfully called for queue SI
; Entry - SI points to 8 byte queue descriptor
; Exit  - [AX] = number of bytes of data in queue (0..QueueSize - 1)

B$NUMQ:	MOV	AX,[SI].QUENUM	;[$]
	ret

sEnd	RT_TEXT 		
	END
