page	49,132
	TITLE	EXTORT	- Runtime Dispatch Support
;***
;exToRt - Runtime Dispatch Support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;     The calling sequence required to get to the runtime is not small.
;     This module supports a size efficient method of getting to the
;     runtime.
;
;     This file must parallel RTM86.asm in the runtime project as a compiled
;     code dispatch will be implemented for the user library feature.  The
;     compiled code dispatch will appear the same as for the compiler runtime
;     module.
;
;     There are two differences between RTM86.ASM and EXTORT.asm at this point:
;	- the vector table is in segment CODE rather than in the runtime
;	  segment.  It can only be in one segment.  The CODE segment is
;	  chosen to prefer speed for interpreted code.
;	- there must be one public label for the start of the entire
;	  vector table.  At present, rtm86.asm treats the vector table as 4
;	  separate tables.  These methods are compatible since the tables
;	  may be adjacent in memory.
;
;     There are 237 entrypoints documented in the runtime specification.
;     These calls are divided as follows:
;
;     returns	     0 word   1 word   n words	 total
;     -------------------------------------------------
;     heap moves      10       37	2	  49
;     no movement    153       24      11	 188
;     -------------------------------------------------
;     total	     163       61      13	 237
;
;     Since there are more than 256 entrypoints in the runtime, some
;     runtime calls will have more than one post byte.	A first byte
;     of 255 indicates that a second post byte exists.	The second byte
;     is added to the first one to form the table offset.
;
;
;****************************************************************************

	.xlist
	include 	version.inc
EXTORT_ASM = ON
	IncludeOnce	architec
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	variable
	.list


assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA
assumes ds, DATA		

	.list

DbSetNoMovAssert MACRO
	ENDM

sBegin	DATA
	staticW	retAddr2,0

	labelD  <PUBLIC,RtDispVec>
	globalW	RtDispVecLow,0
	globalW	RtDispVecHigh,<SEG B$ASSN>

sEnd	DATA

	EXTRN	B$ASSN:FAR
	EXTRN	B$POW4:FAR

sBegin	RT
sEnd	RT

;
; Generate externs for runtime entries
;
RTMENT	MACRO entryname,dummy,entryseg,startlabel,callflag,switches

	IFNB	<switches>	;;[J1]
	IF	(switches)	;;[J1]
	EXTRN	entryname:FAR	;;[J1]
	ENDIF			;;[J1]
	ELSE			;;[J1]
	EXTRN	entryname:FAR
	ENDIF			;;[J1]
	ENDM
	include rtmint.inc	;generate table of runtime entry point addresses

sBegin	CODE
;
;Generate the table of runtime entry point addresses
;
	EVEN			;start table on an even-byte boundary

_TEXTOFFSET	EQU	OFFSET _TEXT:

VecTbl	LABEL	WORD
RTMENT	MACRO entryname,dummy,entryseg,startlabel,callflag,switches
	IFNB	<switches>		;;[J1]
	IF	NOT (switches)		;;[J1]
	FO_HOLES = TRUE 		;;[J1]
	DW	RTOFFSET B$FrameFC	;;[J1]
	EXITM				;;[J1]
	ENDIF				;;[J1]
	ENDIF				;;[J1]
	IFNB	<startlabel>
startlabel	LABEL	WORD
	ENDIF
	IFNB	<entryseg>
	DW	entryseg&OFFSET	entryname
	ELSE
	DW	RTOFFSET entryname
	ENDIF
	ENDM
	include rtmint.inc	;generate table of runtime entry point addresses

IFDEF	FO_HOLES			;;[J1]
externNP B$FrameFC			;;[J1]
ENDIF	;FO_HOLES			;;[J1]

;The following table indices are needed by the user library code
PUBLIC	VecTbl
PUBLIC	_TEXT_START
PUBLIC	_TEXT_END		

;***********************************************************************
;ExToRt - Size efficient gateway to the runtime.
;Purpose:
;	Size efficient gateway to runtime entrypoints that can not
;	cause heap movement in any heap.
;
;	There are four varients of this routine they differ as follows:
;	- if the called entrypoint can cause heap movement then the di
;	  register (text table address) must be reloaded.  In DOS 3, es
;	  must also be reloaded.
;	- a large number of executors simply dispatch after calling the
;	  runtime the dispatch is performed here for size efficiency.
;	- most BASIC functions map to a single runtime entrypoint that
;	  returns a result in ax.  For these functions, a varient that
;	  simply pushes ax and dispatches is provided.	This varient
;	  always expects heap movement, as more than half of the functions
;	  can cause movement.
;
;	Varient names are:
;	ExToRt		- expects no heap movement.  Returns to caller.
;	ExToRtMov	- expects heap movement, BUT DOES NOT UPDATE ES or DI.
;							Returns to caller.
;	ExToRtDisp	- expects no heap movement.  Dispatches.
;	ExToRtDispMov	- expects heap movement.  Dispatches.
;	ExToRtDispAx	- expects no heap movement.  Pushes ax, and dispatches.
;	ExToRtDispMovAx - expects heap movement.  Pushes ax, and dispatches.
;	ExToRtDispDxAx	- expects no heap movement. Pushes dx:ax and dispatches.
;	ExToRtDispMovDxAx - expects heap movement. Pushes dx:ax and dispatches.
;
;	See the header for this module for more information.
;
;Entry:
;	return address points to byte index of rt entrypoint.
;
;Exit:
;	Same as X for called RT entrypoint, except as follows:
;	     Heap movement varients reload di and es.
;	     Function return varients push ax on the stack.
;	     Dispatch varients don't return.
;
;***********************************************************************

PUBLIC	ExToRtDispR4
ExToRtDispR4:
	mov	cx,CODEOFFSET ExDispR4	;Return address for Shared Entry
	DbSetNoMovAssert		;remember we expect RT call not to move
					;  near or far heap
	jmp	short GoShared

PUBLIC	ExToRtDispR8
ExToRtDispR8:
	DbSetNoMovAssert		;remember we expect RT call not to move
					;  near or far heap
PUBLIC	ExToRtDispMovR8
ExToRtDispMovR8:
	mov	cx,CODEOFFSET ExDispR8	;Return address for Shared Entry
	jmp	short GoShared


PUBLIC	ExToRtDispDxAx
ExToRtDispDxAx:
PUBLIC	ExToRtDispMovDxAx
ExToRtDispMovDxAx:
	mov	cx,CODEOFFSET ExDispMovDxAx ;Return address for Shared Entry
					  ;  near or far heap
	jmp	short GoShared

PUBLIC	ExToRtDispAx
ExToRtDispAx:
PUBLIC	ExToRtDispMovAx
ExToRtDispMovAx:
	mov	cx,CODEOFFSET ExDispMovAx ;Return address for Shared Entry
GoShared:
	pop	bx			;Location of post byte
	mov	al,cs:[bx]		;Post byte
	inc	ax
	DbAssertRelB  al,nz,0,CODE,<Extort: 2-byte postbyte found (1)>
	jmp	short RtDispatchAl	; Call the runtime
					;Entry: al = index
					;	bx = exe ret
					;	cx = continuation addr

PUBLIC	ExToRtDisp
ExToRtDisp:
PUBLIC	ExToRtDispMov
ExToRtDispMov:
	mov	cx,CODEOFFSET ExDispMov	;Return address for shared entry
	jmp	short GoShared

PUBLIC	ExToRtByteDisp			;Special case: ax already has postbyte
ExToRtByteDisp:
	mov	cx,CODEOFFSET ExDispMov
	;ax = index, cx = continuation address
	jmp	short RtDispatch	;Call the runtime



ExDispR8:
	;common exit from intrinsic function executors which receive a pointer
	;  to an R8 in ax on exit
	xchg	ax,bx			;bx = ptr to R8
	fld	qword ptr DGROUP:[bx]	
	jmp	short ExDisp		

ExDispR4:
	;common exit from intrinsic function executors which receive a pointer
	;  to an R4 in ax on exit
	xchg	ax,bx			;bx = ptr to R4
	fld	dword ptr DGROUP:[bx]	
	jmp	short ExDisp



ExDispDxAx:
	public	ExDispMovDxAx
ExDispMovDxAx:
	push	dx
ExDispAx:
ExDispMovAx:
	push	ax
ExDisp:
ExDispMov:
	RestorePcodeVar 		
	mov	si,[grs.GRS_otxCur]
	DispMac


;NOTE: the below didn't used to be separate from the above, but must be
;NOTE: now that all variants call GetEsDi, they are, for speed
PUBLIC	DispMovDxAx
DispMovDxAx:
	push	dx
PUBLIC	DispMovAx
DispMovAx: 
	push	ax
PUBLIC	DispMov
DispMov:
	RestorePcodeVar 		
	DispMac

PUBLIC	DispDxAx
DispDxAx:
	push	dx
PUBLIC	DispAx
DispAx: 
	push	ax
PUBLIC	Disp
Disp:
	DispMac

;***
;ExToRtCall - Call the runtime
;Purpose:
;	This entrypoint calls the runtime routine indicated by
;	the postword in ax.
;
;Entry:
;	ax = postword + 1
;	si = oTxCur
;
;Exit:
;	as per runtime routine
;	es,si,di updated
;**************************************************************************
PUBLIC	ExToRtCall			;Only used for MATH!!!
ExToRtCall:
	pop	[retAddr2]		;return address
	mov	[RtDispVecHigh],SEG B$POW4 ;dispatch to correct address
	mov	cx,CODEOFFSET ExToRt_Return
	jmp	short RtDispatch

ExToRt_Return:
	mov	[RtDispVecHigh],SEG B$ASSN ;restore default
	RestorePcodeVar 		
	mov	si,[grs.GRS_otxCur]
	jmp	[retAddr2]		;return address

PUBLIC	ExToRt
ExToRt:
PUBLIC	ExToRtMov
ExToRtMov:
	pop	bx			;return offset
	mov	al,cs:[bx]		;post byte
	inc	bx			;Offset after the post byte
	inc	al
	DbAssertRelB  al,nz,0,CODE,<Extort: 2-byte postbyte found (3)>
	mov	[retAddr2],bx		;return address to executor
	mov	cx,CODEOFFSET ExToRt_Return

RtDispatchAl:
	xor	ah,ah

PUBLIC	RtDispatch			
RtDispatch	PROC	FAR
;ax = postbyte value + 1
;cx = RtDispatch return address
	shl	ax,1			;*2 for 1 relative word index
	mov	bx,ax
	mov	ax,cs:[VecTbl.bx-2]	;Offset of runtime routine
	mov	[RtDispVecLow],ax

	;In the event that a runtime entry point has been deemed to be never
	;called by QBI, it's entry in this table will be zero unless QB is
	;invoked with the /L switch. See dennisc for details (brianle 15 May 87)
	DbAssertRel ax,nz,0,CODE,<extort: someone tried to dispatch to a U.L.-specific RT entry point>

	push	cs			;Build first half of far return
	push	cx			;offset part of far return

	mov	[grs.GRS_otxCur],si	;Make text pointer known to error code
DispRt:                                 ;Dispatch point for runtime
	jmp	[RtDispVec]		;far jump to runtime entry point
RtDispatch	ENDP






subttl	Ex to RT Dispatch Helpers
page
;***
;GetEsDi - load di and es for execution
;Purpose:
;	Load execution context di (and es in DOS 3).
;Input:
;	none
;Output:
;	es	= current pcode segment
;	di	= variable table address + VAR_value
;Preserves:
;	ax,cx,dx
;Modifies:
;	none
;****
	PUBLIC GetEsDi
GetEsDi PROC	NEAR
	mov	bx,[grs.GRS_pMrsCur]
	mov	di,PTRRS[bx.MRS_bdVar.BD_pb] ;di = start of variable table
	add	di,VAR_value		;add offset to value part for exe speed
	cmp	[grs.GRS_fDirect],FALSE ;check direct mode - diff. text table
	jnz	GetDirectSeg		;brif executing out of direct mode buf.

	mov	bx,[grs.GRS_bdRs.BD_pb] ;bx points to base of the Rs table
	add	bx,[grs.GRS_offsetTxdSeg] ;bx points to seg adr of active txd
	GETSEG	es,PTRRS[bx],bx,<SPEED,LOAD> ;[6] es = segment adr of text table
	ret
GetDirectSeg:
	GETSEG	es,[grs.GRS_bdlDirect_seg],bx,<SPEED,LOAD> ;[2] Pcode segment of direct buffer
	ret
GetEsDi ENDP



sEnd	CODE

end
