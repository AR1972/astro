	TITLE	GRPUTIL - iAPX 88/86 GRAPHICS UTILITY ROUTINES
;***
; GRPUTIL - iAPX 88/86 GRAPHICS UTILITY ROUTINES
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Graphics utility routines.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	_BSS		
	USESEG	GR_TEXT 	

	INCLUDE seg.inc 	;segment definitions

sBegin	_BSS			
;
;****************************************************************************
; External low-level function vectors
;****************************************************************************
;
externW b$SetAttr		

	externW B$VXMIN		;defined in GWDATA.ASM
	externW B$VXMAX		;defined in GWDATA.ASM
	externW B$VYMIN		;defined in GWDATA.ASM
	externW B$VYMAX		;defined in GWDATA.ASM

sEnd	_BSS			

assumes CS,GR_TEXT		
sBegin	GR_TEXT 		

;low-level routines:

	externNP B$GETFBC

	externNP B$ERR_FC

	PAGE

	SUBTTL	GRAPHICS SUPPORT ROUTINES

;***
;
;B$CLRATR		PROCESS COLOR ATTRIBUTE FOR GRAPHICS STATEMENTS
;
;Purpose:
;	   This routine processes a color attribute for several graphics
;	   statements.	If the color attribute is defaulted, the default color
;	   is put into AL by a call to the OEM dependent routine B$GETFBC.  Next
;	   OEM routine SetAttr is called to set the current graphics attribute
;	   to the specified color.  If the color was invalid, SetAttr returns
;	   carry set and this routine issues a function call error.
;
;Entry:
;	AL=color attribute or AX=-1 if default color
;
;Exit:
;	AL=valid color attribute
;
;Modifies:
;	AX,BX
;
;****
cProc	B$CLRATR,<PUBLIC,NEAR>
cBegin
				;PROCESS COLOR ATTRIBUTE
	CMP	AX,-1		;IS DEFAULT COLOR NEEDED?
	JNE	SET_COLOR	;NO: USE SPECIFIED VALUE
	STC			;SET CARRY
	CALL	B$GETFBC 	;GET FG/BG COLORS
SET_COLOR:
	CALL	[b$SetAttr]	;SET ATTRIBUTE
	JC	FC_ERROR	;ABORT IF INVALID COLOR
cEnd

FC_ERROR:
	JMP	B$ERR_FC	;ILLEGAL FUNCTION CALL

	SUBTTL	COMPILER INTERFACE ROUTINES
	PAGE

;***
;
;B$INVIEW     Determine whether (x,y) coordinates within viewport boundaries
;
;Purpose:
;	  Test the (x,y) coordinates in (CX,DX) against the current viewport
;	  boundaries. The carry flag is cleared if either
;	  coordinate was outside the viewport boundary.
;
;Entry:
;	  CX=x coordinate DX=y coordinate
;
;Exit:
;	carry set if no coords changed, clear if either coord changed
;
;Modifies: none
;
;****
cProc	B$INVIEW,<PUBLIC,NEAR>
cBegin
				;See if x is in VIEW
	CMP	CX,B$VXMIN	;Is it off the left of the viewport
	JL	OUTVIEW 	;Yes, out of VIEW
	CMP	CX,B$VXMAX	;Is it off the right of the viewport
	JG	OUTVIEW 	;Yes, out of VIEW
	CMP	DX,B$VYMIN	;Is it off the top of the viewport
	JL	OUTVIEW 	;Yes, out of VIEW
	CMP	DX,B$VYMAX	;Is it off the bottom of the viewport
	JG	OUTVIEW 	;Yes, out of VIEW
	STC			;no, is in VIEW
	RET
OUTVIEW:
	CLC
	RET
cEnd	nogen

sEnd	GR_TEXT 		
	END
