;***
; STACK.INC - 18-Feb-88 - Stack size definitions
;***
	.XLIST
;***
;
; Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	Stack constants
;
;	WARNING:  This file is shared between the runtime and interpreter.
;	WARNING:  The runtime is the official "owner", and all changes should
;	WARNING:  be made there first, and then propogated to the interpreter.
;
;
;******************************************************************************

STACK2_INC = -1			;[3] remember file has been included




FAT_COW 	= 300h		;[6] Extra slop just in case
STACK_SIZE	= 0800h	+ FAT_COW  ;[3] Default stack size (MUST BE EVEN)
STACK_MIN	= 0320h + FAT_COW  ;[3] Minimum stack size


STACK_CHECK	= STACK_MIN-20h+1h ;[3] Stack check marker offset (MUST BE ODD)
				;[3] This constant is used by runtime to ensure
				;[3]  we never make long-term stack commitments
				;[3]  (i.e. GOSUB, CALL, FUNCTION invocations)
				;[3]  that would result in less than this many
				;[3]  free bytes between SP and b$pend.
