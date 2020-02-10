	TITLE	SWAP - Swap values
	PAGE	56,132
;***
; SWAP - Swap values
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; SWAP Statement - calls one of 4 entry points, based on arg type:
;
;      SWAP var1, var2
;
;    Examples:
;
;     SWAP a!,b!	    SWAP c#,d#		 SWAP e%,f%	      SWAP g$,h$
;	|		      | 		   |			|
;     $SWPA		    $SWPB		 $SWPC		      $SWPD
;
;******************************************************************************
	INCLUDE switch.inc	
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	useSeg	RT_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE baslibma.inc	
	INCLUDE pointers.inc	


sBegin	RT_TEXT 		

assumes CS,RT_TEXT		

;***
; B$SWP2, B$SWP4, B$SWP8 - Swap values
;
;Purpose:
; Runtime Entry Points. Interchange values.
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement.
;
;Entry:
; pOp1	= Far address of one operand
; pOp2	= FAR address of other operand
;
;******************************************************************************

cProc	B$SWP8,<FAR,PUBLIC>
cBegin	nogen			
	MOV	AL,4		;[AL] = number of words to swap
	SKIP	2		;skip the next
cEnd	nogen

cProc	B$SWP4,<FAR,PUBLIC>
cBegin	nogen			
	MOV	AL,2		;[AL] = number of words to swap
	SKIP	2		;skip the next
cEnd	nogen

cProc	B$SWP2,<FAR,PUBLIC>
cBegin	nogen			
	MOV	AL,1		;[AL] = number of words to swap
cEnd	nogen

cProc	SWAP_COMMON,FAR,<DI,ES,DS>  
parmD	pOp1			
parmD	pOp2			
cBegin
	GETPTR	,ES,DI,pOp1,,<SIZE,LOAD>    ; [ES:DI] = pointer to Op1
	GETPTR	,DS,BX,pOp2,,<SIZE,LOAD>    ; [DS:BX] = pointer to Op2
	CBW			;[AX] = number of words
	XCHG	AX,CX		;[CX] = number of words
SWAPLOOP:
	MOV	AX,ES:[DI]
	XCHG	AX,[BX]
	STOSW			;swap!
	INC	BX
	INC	BX
	LOOP	SWAPLOOP
cEnd				

sEnd	RT_TEXT 		

	END
