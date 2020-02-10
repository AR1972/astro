	TITLE	FIELD  - FIELD Statement Processors
;***
; FIELD  - FIELD Statement Processors
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; - FIELD Statement:
;
;	FIELD [#]n, Width AS StringVar [, Width AS StringVar] ...
;
;	FIELD now makes two calls :
;
;	void B$FLDP(I2 channel)
;
;	This call is a preamble to the actual FIELD statement call and sets
;	up the channel and pointers to the field buffer. (FLDP stands for
;	FieLD Preamble)
;
;	void B$FIEL(I2 Width, *psdStringvar)
;
;	This call be made for each pair of width and stringvar in the FIELD
;	statement.
;
;	NOTE: There is NO NEED to set up PTRFIL.  The only thing required
;	is a pointer to the field buffer and this can be obtained once the
;	channel number is known. Hence there is no call to B$PEOS.
;
;******************************************************************************
	page	,132

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;Code segments
	useSeg	DK_TEXT
	useSeg	NH_TEXT
	useSeg	ER_TEXT
;Data segments
	useSeg	_BSS
	useSeg	_DATA

	INCLUDE seg.inc
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE idmac.inc	
	INCLUDE string.inc	


sBegin	_BSS

	staticW FIELD_LEFT,,1	;bytes left in field buffer
	staticW FIELD_POS,,1	;current field position
	staticW FIELD_FDB,,1	;location of backptr descr

sEnd	;_BSS

sBegin	NH_TEXT

	externNP	B$LHFDBLOC	
	externNP	B$LHFLDDESCADD	
	externNP	B$STDALC	

sEnd	;NH_TEXT

sBegin	ER_TEXT

	externNP	B$ERR_BFM	
	externNP	B$ERR_FOV	
	externNP	B$ERR_FC	

sEnd	;ER_TEXT

	assumes CS,DK_TEXT
sBegin	DK_TEXT

	SUBTTL	FIELD interfaces -- B$FLDP & B$FIEL
	page
;***
;B$FLDP -- preamble of FIELD statement
;void B$FLDP(I2 Channel)
;
;Purpose:
;	Sets up the channel and pointers to the field buffer.
;
;	NOTE: no need to set up b$PTRFIL.
;Entry:
;	Parameter in stack.
;	int	Channel
;Exit:
;	[FIELD_FDB]	= *FDB
;	[FIELD_LEFT]	= record length
;	[FIELD_POS]	= *field buffer
;Uses:
;	none
;Exceptions:
;	bad file mode -- B$ERR_BFM
;*******************************************************************************

cProc	B$FLDP,<PUBLIC,FAR>,<SI>
	ParmW	Channel 		;I2 file number
cBegin
	MOV	BX,Channel		;BX has the file number
	CALL	B$LHFDBLOC		;if NZ then SI=*FDB
	JZ	ERCBFM			;Brif not, give "bad file mode"
	CMP	[SI].FD_MODE,MD_RND
	JNE	ERCBFM			;Brif not, give "bad file mode"
	OR	[SI].FD_FLAGS,FL_FIELD	; Indicate FIELD encountered
	MOV	[FIELD_FDB],SI		;get address of FDB data pointer
	MOV	AX,[SI].FD_VRECL	; get record length
	MOV	[FIELD_LEFT],AX 	;Save record length
	ADD	SI,FD_BUFFER		; [SI] = field buffer address
	MOV	[FIELD_POS],SI		;Save address
cEnd					;pop si, and exit to caller

ERCBFM: JMP	B$ERR_BFM		
ERCFC:	JMP	B$ERR_FC		
ERCFOV: JMP	B$ERR_FOV		


	page
;***
;B$FIEL -- set up each field of a record
;void B$FIEL(I2 wid, SD psdStringVar)
;
;Purpose:
;	Sets up each field for a record.  (pointer to the actual buffer
;	position and the length)
;Entry:
;	Parameters in stack.
;	int	Wid
;	sd	*psdStringVar
;Exit:
;	sd of StringVar is set up
;Uses:
;	per conv.
;Exceptions:
;	field overflow -- B$ERR_FOV
;	illegal function call -- B$ERR_FC
;*******************************************************************************

cProc	B$FIEL,<PUBLIC,FAR>,<SI>
	ParmW	Wid		;I2 field width
	ParmW	psdStringVar	;*SD pointer of the sd of StringVar
cBegin
	MOV	DX,Wid		;DX has the field width
	OR	DX,DX		;can't be negative
	JS	ERCFC		;Brif yes, give "illegal function call"
	SUB	[FIELD_LEFT],DX ;count how many left in the field buffer
	JB	ERCFOV		;if field past buffer size, then error
	MOV	BX,psdStringVar ;BX is the pointer of sd of StringVar
	CALL	B$STDALC	;free string space
	MOV	SI,[FIELD_POS]	;get current field position
	MOV	[BX],DX 	;set new string length
	MOV	[BX+2],SI	;set new string address
	OR	DX,DX		; zero length string?
	JZ	FieldExit	; brif so -- skip call to B$LHFLDDESCADD
				; to avoid String Space Corrupt.
	ADD	[FIELD_POS],DX	;bump to next field postion
	MOV	SI,[FIELD_FDB]	;set up pointer to FDB for heap entry

;	Add fielded descriptor value to end of string of descriptor
;	addresses associated with the FDB.

	CALL	B$LHFLDDESCADD	;add the descriptor pointed by BX
FieldExit:			
cEnd				;pop si and exit to caller


sEnd	;DV_TEXT

	END
