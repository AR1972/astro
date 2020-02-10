	TITLE	DIRECTRY - GW BASIC 2.0 DIRECTORY HANDLING ROUTINES
;***
; DIRECTRY - GW BASIC 2.0 Directory handling routines
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - CHDIR Statement:
;
;      CHDIR pathname
;	 |
;      B$CDIR
;
;
; - MKDIR Statement:
;
;      MKDIR pathname
;	 |
;      B$MDIR
;
;
; - RMDIR Statement:
;
;      RMDIR pathname
;	 |
;      B$RDIR
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

;
;	Code Segments
;
	USESEG	<DK_TEXT>	;Disk I/O component
	USESEG	<NH_TEXT>	; For string deallocation

;
;	Data Segments
;
	USESEG	<_DATA> 	;Pre-initialized data

	INCLUDE seg.inc 	;segment definitions
	INCLUDE files.inc	;get callos macro, fn defs.
	INCLUDE baslibma.inc	
	INCLUDE string.inc	



sBegin	NH_TEXT			
	externNP B$STDALCTMP	; Deallocate temp string
	externNP B$STPUTZ	; Null-terminate string
sEnd	NH_TEXT			


assumes CS,DK_TEXT
sBegin	DK_TEXT

	externNP B$ERR_PNF
	externNP B$ERR_ACD


PAGE
;***
; B$MDIR - make a new directory entry (via DOS call)
; pascal far B$MDIR(psdDirectory)
;
; Input:
;	psdDirectory == a pointer to an sd containing the input string
; Output:
;	NONE
; Modifies:
;	per convention
; Exceptions:
;	May call B$ERR_PNF or B$ERR_ACD
;****
cProc	B$MDIR,<PUBLIC,FAR>
cBegin	<nogen>
	MOV	AH,C_MKDIR	; DOS function code for make-directory
	SKIP	2		; Proceed with processing	
cEnd	<nogen>			; End of B$MDIR

	PAGE
;***
; B$RDIR - remove a directory entry (via DOS call)
; pascal far B$RDIR(psdDirectory)
;
; Input:
;	psdDirectory == a pointer to an sd containing the input string
;
; Output:
;	NONE
; Modifies:
;	per convention
; Exceptions:
;	May call B$ERR_PNF or B$ERR_ACD
;****

cProc	B$RDIR,<PUBLIC,FAR>
cBegin	<nogen>
	MOV	AH,C_RMDIR	; DOS function code for remove-directory
	SKIP	2		; Proceed with processing
cEnd	<nogen>			; End of B$RDIR

	PAGE

;***
; B$CDIR - change current working directory (via a DOS call)
; pascal far B$CDIR(psdDirectory)
;
; Input:
;	psdDirectory == a pointer to an sd containing the input string
; Output:
;	NONE
; Modifies:
;	per convention
; Exceptions:
;	May call B$ERR_PNF or B$ERR_ACD
;****
cProc	B$CDIR,<PUBLIC,FAR>
cBegin	<nogen>
	MOV	AH,C_CHDIR	; AH = DOS function code for change-dir
cEnd	<nogen>			; End of B$CDIR & fall through

	PAGE

;***
;DIRECTORY - Common routine for all the three routines
;Pascal far DIRECTORY(psdDIRECTORY)   /* AH or AX is also input */
;
;ENTRY:
;	psdDIRECTORY = pointer to an sd containing the input string
;	AH = DOS function code for the directory function (DOS 2)
;	AL = Entry index into the dispatch-table for directory function (DOS 5)
;
;EXIT:
;	NONE
;
;Modifies:
;	per convention
;
;Exceptions:
;	May call B$ERR_PNF or B$ERR_ACD
;
;****

cProc	DIRECTORY,<FAR>,<SI,DI> 	
ParmSD	sdDIRECTORY			
cBegin					

	MOV	BX,sdDIRECTORY		; Get the sd of string
	CALL	B$STPUTZ		; null-terminate the string
					; AH preserved from entry
	MOV	DX,[BX+2]		; DS:DX = pathname string address
	CALLOS				; Do the requested function
	JC	DIR_ERROR		; Brif error
	cCALL	B$STDALCTMP		; deallocate temporary string

cEnd					; End of DIRECTORY

DIR_ERROR:				; DOS returned an error
					; Analyze it
	CMP	AL,ERRACD		; Is it Access-Denied?
	JE	ACCESS_DENIED		; Brif so
	JMP	B$ERR_PNF		; default to Path-not-found error
ACCESS_DENIED:				
	JMP	B$ERR_ACD		; Access-Denied error


sEnd	DK_TEXT

	END
