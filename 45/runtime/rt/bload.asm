	TITLE	BLOAD - BLOAD and BSAVE Library Support
;***
; BLOAD - BLOAD and BSAVE Library Support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************
	INCLUDE rmacros.inc	; Runtime Macro Defintions
	INCLUDE switch.inc

;	code segments

	useSeg	DK_TEXT
	useSeg	DV_TEXT
	useSeg	NH_TEXT
	useSeg	ER_TEXT
	useSeg	RT_TEXT		

;	data segments

	useSeg	_DATA
	useSeg	_BSS

	INCLUDE	seg.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE baslibma.inc
	INCLUDE rtps.inc	; constants shared with QBI


	SUBTTL	local constant definitions

	BFileType=	0FDH	;253d if the file type
	TTY=		0	;tty is default

	SUBTTL	data externals

sBegin	_DATA
	externW	b$seg		;for DEF SEG
sEnd	_DATA

sBegin	_BSS
	externB	b$Buf3		
	externW	b$PTRFIL
	externB	b$ACCESS
	externB	b$LOCKTYPE
sEnd	_BSS

	SUBTTL	code externals
	page
sBegin	DV_TEXT
	externFP	B$CLOS
	externNP	B$OPENIT
	externNP	B$BIN
	externNP	B$BOT
	externNP	B$DOS3CHECK	
sEnd	DV_TEXT

sBegin	NH_TEXT
	externNP	B$LHFDBLOC	
sEnd	NH_TEXT

sBegin	RT_TEXT				
sEnd	RT_TEXT				

sBegin	ER_TEXT
	externNP	B$ERR_BFM	;bad file mode
	externNP	B$ERR_DFL	;Disk full
sEnd	ER_TEXT

	assumes	CS,DK_TEXT
sBegin	DK_TEXT
	SUBTTL	BSAVE interface -- B$BSAV
	Page

;***
;B$BSAV -- binary save
;void B$BSAV(sd *pFn, U2 offset, U2 cByte)
;
;Purpose:
;	BSAVE the memory image from the area defined by the last DEF SEG
;	value and user supplied Offset and Length.
;Entry:
;	Parameters are in stack.
;	sd	*pFileName
;	U2	Offs -- user supplied offset
;	U2	Len -- user supplied length
;Exit:
;	none
;Uses:
;	none
;Exceptions:
;	disk full -- can't write on the disk
;*******************************************************************************

cProc	B$BSAV,<PUBLIC,FAR>
	ParmW	pFileName	;*sd of filename
	ParmW	Offs		;U2 user specified offset
	ParmW	Len		;U2 user specified length
cBegin


;	Construct header block of seven bytes for start of BSAVE file.

	MOV	BYTE PTR b$Buf3,bFileType ;get file type in first hdr byte
	MOV	DX,[b$seg]	;get segment of memory to be saved
	MOV	WORD PTR b$Buf3+1,DX ;put into second and third bytes
	MOV	AX,Offs		;get offset of memory to be saved
	MOV	WORD PTR b$Buf3+3,AX ;put into fourth and fifth bytes
	MOV	CX,Len		;get length of memory to be saved
	MOV	WORD PTR b$Buf3+5,CX ;put into sixth and seventh bytes

;	Use registers set up to determine if access is illegal.


;	The BSAVE can proceed.  Now open the file for output.

	MOV	AX,MD_SQO	;BSave is output
	MOV	DX,pFileName	;DX = *sd of filename
	MOV	[b$ACCESS],ACCESS_WRITE ;write access for save
	MOV	[b$LOCKTYPE],LOCK_BOTH	;deny read/write
	cCall	OpenFile0	;open file 0, need AX & DX, on return
				; [b$PTRFIL]=*FDB

;	Output the seven-byte header in b$Buf3.

	MOV	DX,DS		;segment of b$Buf3 is DGROUP
	MOV	BX,OFFSET DGROUP:b$Buf3 ;get offset of b$Buf3
	MOV	CX,7		;size is the seven bytes
	cCall	B$BOT		;output the header to the file
	JC	ERCDFL		;if disk full, then file is closed

;	Output the memory to the file.

	MOV	DX,WORD PTR b$Buf3+1 ;get the segment of the BSAVE
	MOV	BX,WORD PTR b$Buf3+3 ;get the offset of the BSAVE
	MOV	CX,WORD PTR b$Buf3+5 ;get the length of the BSAVE
	cCall	B$BOT		;block output, need [b$PTRFIL]=*FDB
				; [DX|BX]=[segment|offset], [CX]=len 
	JC	ERCDFL		;if disk full, then file is closed
	cCall	CloseFile0	;close file 0


cEnd				;clear stack and exit to caller

ERCDFL:	JMP	B$ERR_DFL	;disk full

	SUBTTL	BLOAD interface -- B$BLOD
	page
;***
;B$BLOD -- binary load interface
;void B$BLOD(sd *pFn, U2 offset, I2 UsrFlag)
;
;Purpose:
;	File is loaded into the same Segment:Offset either from which it
;	was saved, if UsrFlag=0, or from the last DEF SEG value and user
;	supplied Offset.
;Entry:
;	Parameters are in stack.
;	sd	*pFileName
;	U2	Offs
;	I2	UsrFlag
;Exit:
;	none
;Uses:
;	none
;Exceptions:
;	Bad file mode -- file isn't a BLOAD file
;	input pass end -- can't read from the file
;*******************************************************************************

cProc	B$BLOD,<PUBLIC,FAR>
	ParmW	pFileName	;*sd of filename
	ParmW	Offs		;U2 user specified offset
	ParmW	UsrFlag		;I2 flag, 0 if user didn't specify offset
cBegin


	MOV	AX,MD_SQI	;BLoad is input
	MOV	DX,pFileName	;DX=*sd of file name
	MOV	[b$ACCESS],ACCESS_READ		;access read for load
	MOV	[b$LOCKTYPE],LOCK_WRITE	;deny write for load
	cCall	OpenFile0	;open file 0, need AX & DX, on return
				; [b$PTRFIL]=*FDB

;	Input the seven-byte header into b$Buf3.

	MOV	DX,DS		;segment of b$Buf3 is DGROUP
	MOV	BX,OFFSET DGROUP:b$Buf3 ;get offset of b$Buf3
	MOV	CX,7		;size is seven bytes
	cCall	B$BIN		;input the header to the file
	JC	ERCBFM		; brif not a BSAVE file -- Bad File Mode

;	Determine the far pointer of the memory to BLOAD.

	MOV	DX,WORD PTR b$Buf3+1 ;get the segment of the BSAVE
	MOV	BX,WORD PTR b$Buf3+3 ;get the offset of the BSAVE
	MOV	AX,WORD PTR b$Buf3+5 ;get the length of the BSAVE

	MOV	CX,UsrFlag	;get user flag
	JCXZ	LoadFile	;Brif yes, go load file

	MOV	BX,Offs		;get user specified offset
	MOV	DX,[b$seg]	;user's DEF SEG in DX

LoadFile:
	XCHG	AX,CX		;[CX] = Length

	cCall	B$BIN		;do block in, need [b$PTRFIL]=*FDB,
				; [DX|BX]=[segment|offset], [CX]=len

ERCBFM:				
	cCall	CloseFile0	;close file 0 (flag preserved)
	JNC	BLOAD_EXIT	; Brif no error
	JMP	B$ERR_BFM	; "Bad File Mode"
BLOAD_EXIT:			


cEnd


	SUBTTL	BLOAD/BSAVE supporting routines
	page
;***
;OpenFile0
;
;Purpose:
;	Open the given filename as the BASIC internal file (file 0).
;	This routine checks the file name (append '.BAS' to the file name,
;	if it doesn't have the extension), and then open it as the file 0.
;	If open succeed, get the addr. of the FDB and give the error
;	if the file is a special character device.
;Entry:
;	AX		= file mode
;	DX		= *sd of file name
;	[b$ACCESS]	= access right
;	[b$LOCKTYPE]	= deny right
;Exit:
;	[b$PTRFIL]	= *FDB of file 0
;Uses:
;	none
;Exceptions:
;	illegal function call -- if file name is a special char. device
;*******************************************************************************

cProc	OpenFile0,<NEAR>,<SI>

cBegin

	cCall	B$DOS3CHECK	;Are we running under DOS 2.1?
	JNC	IsDos3		;No, skip special code.
	MOV	[B$LockType],0	;Locking must be disabled for DOS 2.1
				; to prevent a DOS error when trying to open
IsDos3: 			; the file.

	MOV	BX,-1		;file #, indicate this is an internal file
	MOV	CX,1		;record length=1


	cCall	B$OPENIT	;open the file with AX=file mode, BX=file#
				; CX=VrecL, DX=*sd of file name, if file
				; name doesn't have extention, B$OPENIT
				; will append ".BAS" to it for file 0
	XOR	BX,BX		;get file 0's
	cCall	B$LHFDBLOC	;SI=*FDB
	MOV	[b$PTRFIL],SI	;for file dispatch routines (B$$RCH,...)
cEnd				;pop si and exit

	page
;***
;CloseFile0 -- close file 0
;
;Purpose:
;	This routine use existing interface routine to close file 0.
;Entry:
;	none
;Exit:
;	[b$PTRFIL] is reset
;Uses:
;	none
;Preserves:
;	flags
;Exceptions:
;
;*******************************************************************************

cProc	CloseFile0,<NEAR>

cBegin
	PUSHF			;save flags
	XOR	AX,AX		;file 0
	PUSH	AX		;I2 channel in stack
	INC	AX		;one parameter
	PUSH	AX		;cParams in stack
	cCall	B$CLOS		;close file 0
	MOV	[b$PTRFIL],TTY	;clear b$PTRFIL
	POPF			;get back flag
cEnd				;exit to caller

sEnd	DK_TEXT
	END
