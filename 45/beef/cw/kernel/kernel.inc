;*
;*	COW : Character Oriented Windows
;*
;*	kernel.inc : kernel include file

.xlist
	include cow.inc
	include lom.inc
.list

;*	* must import from version file (for swapped versions)
;*	cparaRunShell = # paragraphs saved for running shell


IFDEF COW_SWAPPED

createSeg   CORE,KERNEL,BYTE,PUBLIC,CODE;* FIXED code

createSeg   INIT,INIT,BYTE,PUBLIC,CODE	;* Init code (discardable)

IFDEF COW_TINY_SWAP
createSeg   USER,EXIT,BYTE,PUBLIC,CODE	;* Exit code (discardable)
ELSE
createSeg   CORE,LOCALMEM,BYTE,PUBLIC,CODE ;* Local memory management
createSeg   EXIT,EXIT,BYTE,PUBLIC,CODE	;* Exit code (discardable)
ENDIF ;!COW_TINY_SWAP



IFDEF MOUSE_EXTRAS
createSeg   MOUSE_TEXT,MOUSE,BYTE,PUBLIC,CODE	;* Mouse code (discardable)
ENDIF ;MOUSE_EXTRAS

ELSE
;* not swaped => all 1 segment
createSeg   COW_TEXT,KERNEL,BYTE,PUBLIC,CODE;* FIXED code
createSeg   COW_TEXT,INIT,BYTE,PUBLIC,CODE	;* Init code (discardable)
createSeg   COW_TEXT,EXIT,BYTE,PUBLIC,CODE	;* Exit code (discardable)
createSeg   COW_TEXT,MOUSE,BYTE,PUBLIC,CODE	;* Mouse code (discardable)
ENDIF ;!COW_SWAPPED


;*	* Driver Segment
createSeg   DRV_FIXED,DRV,BYTE,PUBLIC,CODE		;* drivers


IFDEF DEBUG
exAssertFailed	=	99		;* exit code for Assert failed
ENDIF ;DEBUG

;*
;*	this will allow the release (non-debug) version of cow to
;*	have the code which communicates with symdeb if so desired
;*
IFNDEF	KEEP_SYMDEB_CODE
IFDEF	DEBUG
KEEP_SYMDEB_CODE = 1
ENDIF	; DEBUG
ENDIF	; !defined KEEP_SYMDEB_CODE

IFDEF KERNEL_SWAP_STACK
;*	*********   Below is QC Only ***********

;*	* Constants for multiple stack implementations

SST_NO_VAP	= 1		; No CC stack is currently present
SST_ACT_VAP	= 2		; CC stack is currently the active stack
SST_INACT_VAP	= 3		; CC stack is currently the inactive stack
ENDIF 	; KERNEL_SWAP_STACK
