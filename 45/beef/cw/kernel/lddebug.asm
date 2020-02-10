;*
;*	COW : Character Oriented Windows
;*
;*	lddebug.asm : debugger support

	TITLE	LDDEBUG - Debugger interface procedures

	.xlist
	?LDDEBUG = 1
	include kernel.inc
	include handle.inc
	.list

	;*	 ENTIRE FILE IS DEBUG SUPPORT !
	;*
	;*	It comes in two parts --
	;*	the code to communicate with symdeb and the code
	;*	which is for doing debug assert checking.
	;*

IFDEF KEEP_SYMDEB_CODE

DEBUGOFFSET  EQU  000FBH
INTOFFSET    EQU  4*3+2				;* int 3 vector : segment

DEBUGCALL    MACRO
call lpfnDebug
ENDM

IFDEF	DEBUG
externFPublic <ExitKernel>			;* from kerninit.asm
ENDIF	; DEBUG

sBegin	DATA
    assumes CS,DATA

externW     <pGlobalHeap, psLom>

;*	* Far pointer to debugger special entry point
	PUBLIC	psDebug
lpfnDebug LABEL   DWORD
	DW	DEBUGOFFSET
psDebug	DW	0

;*	* Special name to test for
szDebug	    DB	'SEGDEBUG',0
cchDebug   =	$-szDebug

IFNDEF	DEBUG
;*	* Special value to test for
globalW	fgDoDebug, 0
ENDIF	; DEBUG

sEnd	DATA

;*****************************************************************************

sBegin	INIT
    assumes CS,INIT
    assumes DS,DATA


;
;   DEBUGINIT - Returns a non zero value in AX if debugger is resident.
;   Inputs: None.
;   Outputs:  AX non zero if debugger resident.
;
cProc	DebugInit,<NEAR,PUBLIC>,<es,si,di>
cBegin
IFNDEF	DEBUG
	cmp	fgDoDebug,0dbdbh	;* Allow debugging?
	jne	debugdone		;* No -- skip over rest of test
ENDIF	; DEBUG

	xor	ax,ax			;
	mov	es,ax			;
	    ;
	    ; If  the debugger	is  present,  a   distinquished  string of
	    ; "SEGDEBUG",0 will be found at 100H off of the interrupt vector
	    ; segment (use breakpoint interupt vector)
	    ;
	mov	bx,word ptr ES:INTOFFSET    ; Get interrupt vector segment.
	mov	es,bx
	mov	di,100H
	mov	si,dataOFFSET szDebug
	mov	cx,cchDebug
	CLD
	repz	cmpsb			;* special name ?
	jnz	debugdone
ok:
	mov	psDebug,bx		;* non-zero segment
;*	* Tell debugger about global heap
	mov	ax,pGlobalHeap
	push	ax
	mov	ax,3
	push	ax
	DEBUGCALL		    ; Tell debugger where the master object is
	add	sp,4
;*	* Tell debugger about thunk segment
	mov	ax,psLom
	push	ax
	mov	ax,10H
	push	ax
	DEBUGCALL		    ; Tell debugger where the master object is
	add	sp,4
;*	* Since the loader loaded in many segments, inform the debugger
;*	   after the fact
	mov	es,psLom
	mov	cx,es:[neLom.ne_cseg]	;* # of segments
	mov	si,es:[neLom.ne_segtab]
	xor	dx,dx			;* zero based segment #
debi_lp:
	mov	bx,es:[si].ns_handle	;* handle or ps
	mov	ax,es:[si].ns_flags
	test	ax,NSMOVE
	jz	debi_inform
	push	es
	mov	es,pGlobalHeap
	test	es:[bx].he_flags,HE_DISCARDED
	mov	bx,es:[bx].he_address	;* dereference
	pop	es
	jnz	debi_nxt
debi_inform:
	and	ax,NSDATA
	Save	<es,cx,dx>
	cCall	<far ptr DebugDefineSegment>,<dx,bx,ax>
debi_nxt:
	add	si,SIZE NEW_SEG1
	inc	dx
	loop	debi_lp
debugdone:
cEnd

sEnd	INIT


;*****************************************************************************

sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,NOTHING		;* can be called from anywhere
    assumes SS,DATA


; DEBUGDEFINESEGMENT - Inform debugger of physical address and type
;		       of a segment.
;
; Inputs:
;	  SegNumber   - zero based segment index
;	  LoadedSeg   - Physical segment address assigned by user to index.
;	  DataOrCodeFlag  - Whether segment is code or data.
;
; Outputs: None.
; SideEffects: Debugger informed of segment index and corresponding
;	       name and physical segment.
;
cProc  DebugDefineSegment,<FAR,PUBLIC>
	Parmw	SegNumber
	Parmw	LoadedSeg
	Parmw	DataOrCodeFlag
	LocalV	szBuff,10		;* buffer for module name
cBegin
	cmp	psDebug,0
	je	setdone
	push	DataOrCodeFlag	    ; Flag for code or data segment. 0 Code, 1 Data.
	xor	ax,ax
	push	ax		    ; Instance Number (0)
	push	LoadedSeg	    ; Segment value in loader.
	push	SegNumber	    ; Segment number

	lea	bx,szBuff
	cCall	GetModuleName		;* returns in ss:bx
	push	ss		    ;
	push	bx		    ;
	xor	ax,ax		    ;
	push	ax		    ;* call 0
	DEBUGCALL		    ;
	add	sp,14
setdone:
cEnd


;********** GetModuleName **********
;*	entry : SS:BX = buffer
;*	* get module name in buffer
;*	exit : SS:BX = buffer
cProc	GetModuleName,<NEAR, ATOMIC>,<BX,ES,SI>
cBegin

	mov	si,bx
	mov	es,psLom	    ;* segment of module name
	mov	bx,es:[neLom.ne_restab]	;* start of table
	add	bx,es:[bx]		;* point to "sz" string
name_lp:
	mov	al,es:[bx]
	inc	bx
	mov	ss:[si],al
	inc	si
	cmp	al,'.'
	jnz	name_lp
	mov	byte ptr ss:[si-1],0	;* zero term
cEnd

; DEBUGMOVEDSEGMENT - Inform debugger that a segment has moved.
;
; Inputs: SourceSeg   - Original segment value.
;	  DestSeg     - New segment value.
;
; Outputs: None.
; SideEffects: Debugger informed of the old and new values for
;	       a physical segment.
;
cProc	DebugMovedSegment,<NEAR,PUBLIC>
	ParmW	SourceSeg
	ParmW	DestSeg
cBegin
	cmp	psDebug,0
	je	movdone
	push	DestSeg 	    ; Push destination segment of move.
	push	SourceSeg	    ; Push moved source segment.
	mov	ax,1		    ; Function 1.
	push	ax		    ;
	DEBUGCALL
	add	sp,6
movdone:
cEnd


; DEBUGFREESEGMENT - Inform debugger that a segment is being returned
;		     to the global memory pool and is no longer code or
;		     data.
;
; Inputs: SegAddr - segment being freed
;
; Outputs: None.
; SideEffects: Debugger informed that it must remove references
;	       to a physical segment.
;
cProc	DebugFreeSegment,<NEAR,PUBLIC>,<es>
	Parmw	SegAddr
cBegin
	cmp	psDebug,0
	je	killdone
	push	SegAddr 	    ; Push segment address
	mov	ax,2		    ; Function 2
	push	ax		    ;
	DEBUGCALL		    ;
	add	sp,4
killdone:
cEnd

sEnd	KERNEL
ENDIF	; KEEP_SYMDEB_CODE	-- Above code is for dealing with symdeb

IFDEF	DEBUG	;* REST OF FILE IS ONLY DEBUGGING ROUTINES
sBegin	KERNEL

;*	* print debug string to debugging console *

cProc	PrDebugRgch,<FAR,PUBLIC,ATOMIC>
   parmD lpch		;* far pointer to string
   parmW cch		;* character count
cBegin	PrDebugRgch

	cmp	psDebug,0
	je	prdeb_end
	push	cch
	push	SEG_lpch
	push	OFF_lpch
	mov	ax,4
	push	ax
	DEBUGCALL
	add	sp,8
prdeb_end:

cEnd	PrDebugRgch


;********** AssertBreak *********
;*	entry : n/a
;*	* if debugger present BREAK
;*	* if not EXIT

cProc	AssertBreak,<FAR,PUBLIC>
cBegin	AssertBreak

	cmp	psDebug,0
	jne	assert_break
	mov	ax,exAssertFailed
	cCall	ExitKernel,<ax>		;* never return
;*	*NOTREACHED*
assert_break:
	int	3			;* HARD CODED BREAKPOINT !!
					;* type G=ip+1 to resume in SYMDEB
cEnd	AssertBreak


;********** CowAssertFailed **********
;*	entry : after return address = optional sd string that describes error
;*	* print assert failed, followed by message
;*	* Then exit
;*	exit : never return to caller
;*	* NOTE : message must be <31 bytes long

cProc	CowAssertFailed,<FAR,PUBLIC,ATOMIC>
cBegin	nogen ;CowAssertFailed

	mov	dx,kernelOffset sdCowAssertFailed
	push	cs
	pop	ds
	cCall	PrDebugSd
	pop	dx
	mov	di,dx
	pop	ax				;* return address => string
	mov	es,ax
	mov	ds,ax
	mov	al,'$'
	mov	cx,50				;* must be a "$" within 50 bytes
	repne	scasb
	jne	assert_skip
	cCall	PrDebugSd
	mov	dx,kernelOffset sdCrLf
	push	cs
	pop	ds
assert_skip:
	cCall	AssertBreak
cEnd	nogen ;CowAssertFailed

sdCowAssertFailed	DB	"Kernel Assert Failed!"
sdCrLf		DB	13,10,"$"


;********** PrDebugSd **********
;*	entry : DS:DX => sd string to print
;*	* Print string to debugging console
;*	exit : n/a

cProc	PrDebugSd,<NEAR,ATOMIC>
cBegin	PrDebugSd

;*	* Find string length
	mov	di,dx
	push	ds
	pop	es
	mov	cx,-1
	mov	al,'$'				;* sd string
	cld
	repne	scasb
	dec	cx
	not	cx				;* cx = length
	cCall	PrDebugRgch,<es,dx,cx>

cEnd	PrDebugSd



;********** Abort **********
;*	entry : call from debugger ! !
;*	* resident label for exit/ExitKernel
;*	exit : never

cProc	Abort,<FAR, PUBLIC, ATOMIC>
cBegin	nogen ;Abort

	push	ss
	pop	ds				;* restore DS

	mov	al,255				;* exit(255);
	cCall	ExitKernel,<ax>

cEnd	nogen; Abort


;********** LpGlobalHeap **********
;*	entry : n/a
;*	exit : DX:AX = far pointer to global heap
;*	* NOTE : may be called regardless of what DS is

cProc	LpGlobalHeap,<FAR,PUBLIC,ATOMIC>
cBegin	LpGlobalHeap

    assumes DS,NOTHING		;* just to be sure !!!
	MOV	DX,pGlobalHeap
	XOR	AX,AX

cEnd	LpGlobalHeap



cProc	CwOutSz,<FAR,PUBLIC>,<DI,DS>
    parmW	sz
cBegin	CwOutSz

    assumes DS,nothing

	PUBLIC	_cwoutsz
_cwoutsz:
	cld
	push	ss
	pop	es
	mov	di,sz
	xor	ax,ax
	mov	cx,0ffffh
	repne	scasb
	dec	di
	mov	by es:[di],'$'
	mov	ax,9
	push	es
	pop	ds
	mov	dx,sz
	int	21h
	mov	by es:[di],0

cEnd	CwOutSz


sEnd	KERNEL

ENDIF ;DEBUG

	END
