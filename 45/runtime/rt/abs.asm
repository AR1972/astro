	TITLE	ABSOLUTE - helper for assembly routines
;***
; ABSOLUTE - Helper for calling BASIC interpreter assembly routines
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; Just used to clear information from the stack to various registers and
; memory locations.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	useSeg	_BSS
	useSeg	ER_TEXT 	
	useSeg	RT_TEXT

	INCLUDE seg.inc

sBegin	_BSS

	externW b$seg		;def seg segment


sEnd	_BSS


sBegin	RT_TEXT
assumes CS,RT_TEXT



;***
; ABSOLUTE - Call absolute address
;
;Purpose:
;	Routine which can be directly called from the basic level which
;	in turn calls an absolute address.
;
;	Under DOS, we push the address of the function on the stack
;	and RET to it.	This gives a protection violation under OS/2
;	Protect mode, so we set up a variable to hold the address
;	and do a JSR indirect to the address.  On return, we have
;	to release the Segment descriptor that we created.
;
;Entry:
;	The actual number of parameters is variable, and depends on the
;	routine that ABSOLUTE will in turn call. The LAST parameter pushed
;	MUST be the DSoffset of an integer variable containing the offset
;	of the routine to be called. The current DEF SEG is used as the
;	segment for the call.
;Exit:
;	IF OM_DOS3
;	   Whatever the called routine elects. We do NOT return to basic
;	   code from here.
;	IF OM_DOS5
;	   Whatever the called routine elects. We do not save any of
;	   the registers around the call. After the call we trash AX,CX
;	   so we would need to modify this to make a Function ABSOLUTE.
;
;Uses:
;      This routine follows convention, but does no saving or checking of
;      the code actually called.
;
;Notes:
; The called routine receives control with all parameters passed to ABSOLUTE,
; except the offset integer, on the stack in Pascal convention. The return
; address present is back to the BASIC level code which CALLed ABSOLUTE.
;
; Stack on call to ABSOLUTE:
;
;
;		\ 	Variable number of parameters		\
;		|	   to routine to be CALLed		|
;		+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;		|	Near pointer to I2 var containing	|
;		|	the offset of the routine to CALL	|
;		+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;		|CS						|
;		+    Far return address to caller of ABSOLUTE	+
;	[SP] -> |IP						|
;		+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;
; Stack on transfer to called routine:
;
;		\ 	Variable number of parameters		\
;		|	   to routine to be CALLed		|
;		+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;		|CS						|
;		+    Far return address to caller of ABSOLUTE	+
;	[SP] -> |IP						|
;		+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;
;
;
; Under OS/2 Protect mode things get complicated.  We have to return to
; this routine in order to deallocate the Segment Descriptor that we
; have created.  However, to insure that error handling works, we must
; have a BP Stack Frame on the stack for this procedure.  In order to
; do this, we have to move all the parameters to the absolute function
; down three words on the stack so that we can stuff our CS:IP BP values.
;
; To get the size of the parameters, we assume that there has been nothing
; pushed on the stack since the last BASIC frame.  We calculate the size
; of the basic frame, then move everything from that point to the end of
; the parameters down so that we can insert our stack frame.
;
;
;******************************************************************************
cProc	ABSOLUTE,<FAR,PUBLIC>
cBegin



	POP	AX		;return offset
	POP	DX		;return segment
	POP	BX		;get pointer to routine address
	PUSH	DX		;restore return address
	PUSH	AX
	PUSH	[b$seg]	; stack DEF SEG segment
	PUSH	[BX]		; stack routine offset

cEnd



sEnd	RT_TEXT

	END
