	TITLE	IOLPT  - Line printer Device Drivers
;***
; IOLPT  - Line printer Device Drivers
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains the device independent printer routines for
;	the BASIC Compiler.  While this module is intended for use with
;	the IBM Personal Computer, there is only a small amount which
;	actually depends on this fact.
;
; BASIC Syntax mapping to included runtime entry points:
;
; - LPOS Function:
;
;      LPOS(x)
;	 |
;     B$LPOS
;
; - WIDTH Statement:
;
;    Four different Syntax possibilities map to four runtime entry points:
;
;      WIDTH size			 WIDTH LPRINT size
;	 |				   |
;      B$WIDT				 B$LWID
;
;
;      WIDTH filenumber, size		 WIDTH device, size
;	 |				   |
;      B$DWID				 B$DWID
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	CONST		
	USESEG	_DATA		
	USESEG	_BSS		
	USESEG	OI_TEXT 	
	USESEG	DV_TEXT 	
	USESEG	DK_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE ascii.inc
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE const.inc	

	TTY	EQU	0	;default for b$PTRFIL is TTY

	PRSTM	EQU	0	;print statement
	CHANL	EQU	1	;#
	USING	EQU	2	;using
	WRSTM	EQU	4	;write statement
	LPSTM	EQU	8	;lprint statement

iniwid= 80			;Initial printer width

sBegin	_DATA			

	externB b$PRFG		;flag for PRINT/LPRINT/WRITE [#][USING]

	EXTRN	b$ERDEVP:word

;error codes from int 24 handler
;		00 - Write Protected.
;	       *01 - Unknown Unit
;	       *02 - Not Ready
;		03 - Unknown command
;		04 - Data Error
;		05 - Bad Drive structure length
;		06 - Seek Error
;		07 - Unknown Media
;		08 - Sector not found
;	       *09 - Printer out of paper
;		10 - Write Fault (hard disks only)
;		11 - Read Fault
;		12 - Other error.


iniwid= 80			; Initial printer width

B$LPTWID DB	NUM_LPT DUP(INIWID,0) ;Width/initial position
B$LPTCRF DB	NUM_LPT DUP(0,0) ;CR flag/unused field
				;Must follow B$LPTWID
				;width/pos field
	LPDCBSZ = NUM_LPT * 2



;LPT Device Control Block field definitions:

_LPWID= 0			;device width (columns per line)
_LPPOS= 1			;current column device is in

;LPT Device Flag Block field definitions:
_LPFLG= lpdcbsz 		;Boolean attributes mask for this device
				;Attribute definitions
	_LPCRF= 1		;non-zero=last char sent was Carriage Return
_LPunused=lpdcbsz+1		;Unused field.

LPTPOS	EQU	B$LPTWID+_LPPOS

LPTWIDTH EQU	B$LPTWID[DI+2*DN_LPT1+_LPWID] ;Base for indexed by
POSITION EQU	B$LPTWID[DI+2*DN_LPT1+_LPPOS] ;	device number

sEnd	_DATA			

sBegin	CONST			

	externW b$FILVEC	
LPNAMES DB	"LPT1LPT2LPT3LPT4"
LPERRS	DW	1,2,9

sEnd	CONST			

sBegin	_BSS			
	externW b$PTRFIL	
	externW b$ERDEV 	;defined in GWDATA.ASM
	externW b$LPTFDB	; defined in GLOBALS.INC
	externB	b$PATHNAM	; defined in GWINI.ASM
	externB b$EOL,,1	; set when in B$$WCLF


sEnd	_BSS			


sBegin	DK_TEXT
	externNP B$WCHSET	; in prnval.asm, set dispatch vector
sEnd	DK_TEXT

sBegin	DV_TEXT 		
	externNP b$devused	; drag in dvinit for lprint support
	externNP B$CLSLPT	
sEnd	DV_TEXT 		

assumes CS,OI_TEXT		
sBegin	OI_TEXT 		

;	Run-time Entries

	PUBLIC	B$D_LPT1
	PUBLIC	B$D_LPT2
	PUBLIC	B$D_LPT3
	PUBLIC	B$D_LPT4

;	Run-time Externals

	externNP B$OPNLPT	
	externNP B$SNDLPT	
	externNP B$UPCASE
	externNP B$LHDALC_CPCT	; deallocate FDB and compact local heap
	externNP B$OPEN_DEV	
	externNP B$ERR_FC	
	externNP B$ERR_BFN	
	externNP B$ERR_IOE	
	externNP B$ERR_DNA	
	externNP B$ERR_DTO	
	externNP B$ERR_OTP	


;	Illegal Device Functions

LPT_EOF EQU	B$ERR_FC
LPT_LOC EQU	B$ERR_FC
LPT_LOF EQU	B$ERR_FC
LPT_RANDIO EQU	B$ERR_FC
LPT_BAKC EQU	B$ERR_FC
LPT_SINP EQU	B$ERR_FC
LPT_BLKIN EQU	B$ERR_FC	  ;5.41 PBS 28/jun/83
LPT_BLKOUT EQU	B$ERR_FC

;	Device Independent Printer Interface

DSPMAC	MACRO	func
	DW	LPT_&func
	ENDM





;-----	File Action Routines  -----------------------------------------------


;	These routines are all called from the file dispatcher.

;	The registers are set up as follows:

;	   (ES:)[SI] = file data block address (points to file mode field)
;		(DI) = device offset  (0 = disk , 2 = next , 4 = next , ...)
;		(AH) = function code of routine
;		(AL,CX,DX,BX) = parameters for each routine

;	These routines are free to use SI and DI as temporaries


fileio	PROC

;*** 
; LPT_OPEN - OPEN statement for LPTn:
;
;Purpose:
;	Allocates FDB for LPTn:, and opens it for output.
;
;Entry:
;	(BX) = file number
;	(AL) = device number for OPEN
;	(CX) = variable record length
;	b$PATHNAM = null-terminated options string (including the "LPTn:")
;Exit:
;	(ES:)[SI] = *FDB
;
;Uses:
;
;Preserves:
;	BX, CX
;
;Exceptions:
;
;******************************************************************************
LPT_OPEN:
	PUSH	BX
	PUSH	CX
	PUSH	AX		;save device number

	MOV	AH,MD_SQO+MD_RND ; valid file modes

	MOV	DL,LPTWIDTH	; DL = width
	CALL	B$OPEN_DEV	; allocate FDB with no buffer
	FDB_PTR ES,SI,SI	;(ES:)[SI] = *FDB

;Parse option string.
	PUSH	SI		; save FDB pointer
	MOV	SI,OFFSET DGROUP:b$PATHNAM+5 ; get the options string

	CALL	GETPR0		;Get 1st char in Filename, skipping blanks
	JZ	Default 	;If no "BIN" option, default to ASC
LPRBIN: CMP	AL,"B"		;BIN?
	JNZ	BadLptOpt	;no, bad LPT option.
	CALL	GETPRM
	CMP	AL,"I"		; I?
	JNZ	BadLptOpt	;no, error
	CALL	GETPRM
	CMP	AL,"N"		; N?
	JNZ	BadLptOpt	;no, error
	CALL	GETPR0		;Get next non-blank char
	JNZ	BadLptOpt	;If not EOS then bad LPT option
	POP	SI		
	OR	FileDB.FD_FLAGS,FL_BIN ;indicate binary mode
	JMP	SHORT LPRBIX
Default:
	POP	SI		
	AND	FileDB.FD_FLAGS,NOT FL_BIN ;indicate ascii mode
LPRBIX:
	POP	AX		;restore device number
	PUSH	AX		;and save it again
	NEG	AL		; convert to positive number
	CBW			; AH = 0
	SHL	AX,1		;get -2*device number
	MOV	DI,AX		;move to DI
	CALL	B$GLPUID 	;get printer number in AH
	CALL	B$OPNLPT	; open the device
	FDB_PTR ES		;restore FDB SEG in ES after movement
	OR	AH,AH		;test for error
	JZ	NLPERR		;if none, then jump around
	CALL	B$LHDALC_CPCT	; Deallocate the FDB and compact local heap
	JMP	SHORT LPERR	;if so, then branch
NLPERR:
	MOV	FileDB.FD_HANDLE,BX ;store handle away in FDB
	POP	AX		;restore register

	pop	cx
	pop	bx
	RET

BadLptOpt:
	POP	SI		; SI = *FDB
	CALL	B$LHDALC_CPCT	; Deallocate the FDB and compact local heap
	JMP	B$ERR_BFN	;bad file name
;Get Next Option Char forcing upper case
; Exit - Z = end-of-statement, else [AL]=byte
;	SI not advanced if End Of String

GETPRM:
	CMP	BYTE PTR [SI],0
	JNZ	GetNxt		;Brif EOS
	RET
GetNxt: LODSB
	JMP	SHORT GETPR1

;Get Next Option Char skipping blanks and forcing upper case
; Exit -Z = end-of-statement, else [AL]=byte
;	SI not advanced if End Of String

GETPR0:
	LODSB			;Get next char
	CMP	AL," "
	JZ	GETPR0		;Ignore Blanks
	CMP	AL,0		;Is it End Of String?
	JZ	GETPRX
;++Not needed unless a second option added.
;	 CMP	 AL,","
;	 JNZ	 GETPR1 	 ;Brif not ","
;	 OR	 AL,AL		 ;set NZ (not end-of-statement)
;	 STC			 ;set carry
;	 RET			 ;Comma returns: NZ and TC
GETPR1:
	call	B$UPCASE
	OR	AL,AL		;Chars return NZ and NC
GETPRX:
	RET			;EOS returns TZ and NC
;++

B$D_LPT1:			;moved here out of module offset 0
B$D_LPT2:
B$D_LPT3:
B$D_LPT4:
	DSPNAM

	page

;*** 
;LPT_DWID -- WIDTH device statement.  Added with [16]
;
;Purpose:
;
;Entry:
;	(AL) = device number
;	(DL) = device width
;	(DI) = device offset
;Exit:
;	None
;Uses:
;	None
;Exceptions:
;	None
;
;******************************************************************************
cProc	LPT_DWID,<NEAR>
cBegin
	MOV	LPTWIDTH,DL	; Set new printer width (indexes by DI)
	CMP	AL,DN_LPT1	; LPT1:?
	JNE	WidthRet	; brif not -- don't set LPRINT width
	MOV	b$LPTFDB.FD_WIDTH,DL ; set width for LPRINT
WidthRet:			
cEnd

;*** 
;LPT_WIDTH -- WIDTH file statement.
;
;Purpose:
;
;Entry:
;	(DL) = device width
;	(DI) = device offset
;Exit:
;	None
;Uses:
;	None
;Exceptions:
;	None
;
;******************************************************************************
LPT_WIDTH:
	MOV	FileDB.FD_WIDTH,DL  
	RET

	page


LPERRT: DW	OFFSET B$ERR_DNA ; 1 = device not available
	DW	OFFSET B$ERR_DTO ; 2 = time out
	DW	OFFSET B$ERR_OTP ; 3 = out of paper

LPERR:
	CMP	AH,3
	JBE	ERROR1		;branch if legal error code
	JMP	B$ERR_IOE	  ;map all other error codes to I/O error
Error1: MOV	AL,AH
	cbw			; [AX]=error code 1..n
	DEC	AX		;[AX]=error code 0..n
	shl	ax,1		;[AX]=offset into error dispatch table
	xchg	ax,bx
	mov	si,offset dgroup:lpnames
	pop	ax
	mov	al,ah
	cbw
	shl	ax,1
	shl	ax,1
	add	si,ax
	mov	di,[b$ERDEVP+2]	;get ptr to erdev$ string
	PUSH	ES		
	PUSH	DS		
	POP	ES		
	movsw
	movsw
	POP	ES		
	mov	[b$ERDEVP],4
	mov	ax,lperrs[bx]
	mov	[b$ERDEV],ax
	JMP	word ptr lperrt[BX] ; asmter this way



;Raw Line Printer Output routine
; Entry - [AL]=byte to be sent to current line printer
;	  [AH]=device id (0..n)
; Exit	- Flags used, All other registers preserved.

LPROUT:
	PUSH	AX
	PUSH	BX
	MOV	BX,FileDB.FD_HANDLE ; get handle for routine
	CALL	B$SNDLPT	; Call OEM routine to output to printer
	OR	AH,AH
	JNE	LPERR		;branch if OEM routine detected error
	POP	BX
	POP	AX
	RET



;*** 
; LPT_SOUT -- Sequential output.
;
;Purpose:
;	This routine keeps track of column position, expands tabs, and forces
;	a carriage return when line width is exceeded.
;
;Entry:
;	(ES:)[SI] = *FDB
;	DI = -2*device id
;	[AL] = byte to be output.
;
;Exit:
;	None
;
;Uses:
;	SI, DI can be changed.
;
;Preserves:
;	All other registers.
;
;Exceptions:
;
;******************************************************************************
cProc	LPT_SOUT,<NEAR>,<DX>	
cBegin				

;	First check if LPRINT is being performed.  If so, then check
;	if it is the first character since it was last closed and do
;	an open operation on LPT1.

	CMP	SI,OFFSET DGROUP:b$LPTFDB ; test if LPRINT
	JNE	NO_LPRINT_OPEN	;if not, then jump
	TEST	b$LPTFDB.FD_FLAGS,FL_LPT_OPN ; test if device is open
	JNE	NO_LPRINT_OPEN	;if so, then jump
	PUSH	AX		;save registers...
	PUSH	BX
	XOR	AH,AH		;will select LPT1
	CALL	B$OPNLPT	; open the device
	OR	AH,AH		;test for any errors
	JNZ	LPERR		;if so, then jump
	MOV	WORD PTR b$LPTFDB.FD_HANDLE,BX ; get handle in FDB
	OR	b$LPTFDB.FD_FLAGS,FL_LPT_OPN ; set the open flag
	POP	BX		;restore registers...
	POP	AX
NO_LPRINT_OPEN:

	PUSH	BX		;save caller's BX
	PUSH	AX		;save char to be output
	CALL	GLPDCB		;DI points to line printer DCB, unit in AH
				; Note interpreter returns unit in AX!
	MOV	BL,AH		;[BL] = device id
	POP	AX		;[AL] = byte to be output
	MOV	AH,BL		;[AH] = device id
	POP	BX		;restore caller's BX

	MOV	DX,WORD PTR _LPWID[DI] ;[DL]=device width, [DH]=current column
	CMP	SI,OFFSET DGROUP:b$LPTFDB	; Test if LPRINT FDB
	JZ	NOTBIN		;branch if so -- not binary mode
	MOV	DL,FileDB.FD_WIDTH	;Get width from FDB

	TEST	FileDB.FD_FLAGS,FL_BIN	; binary mode?
	JZ	NOTBIN		; brif not
	CALL	LPROUT		; Raw output of character
	CALL	$UPDPOS		; DH = new position
	JMP	SHORT SAVPOS	; save new position
	
NOTBIN:
	CALL	$CRIFEL 	;force CR if End-Of-Line
SAVPOS:
	MOV	BYTE PTR _LPPOS[DI],DH ;save new column position
cEnd				


;*** 
; LPOUT1 -- low-level line printer output
;
;Purpose:
;	For IBM Compatibility, the filter performs the following translations
; 		x x x CR x x x    === x x x CR LF x x x
;		x x x CR LF x x x === x x x CR LF x x x
;	If LPT was opened for RANDOM mode, and WIDTH=255, then suppress LF which
;	follow carriage returns for IBM compatibility.
;
;	Eat all LineFeeds which follow CarriageReturns with following algorithm:
;	if (Char <> LF) or (LastWasCR = 0) then output (Char)
;	if (Char = CR) then
;	    LastWasCR = 1
;	    if FDB.MODE<>RANDOM or FDB.WIDTH<>255 then
;		output(LF)
;	else
;	    LastWasCR = 0
;
;	The only case where this is not compatible with IBM is when the user
;	executes:
;		PRINT CHR$(13);CHR$(10);...
;
;	The best way this could have been done was by setting CRONLY=1 in the
;	switch files and letting the device drivers append Line-Feeds if
;	necessary.  It was considered too late to make a change this drastic.
;
;Entry:
;	AL = char to output
;	(ES:)[SI] = *FDB
;
;Exit:
;	DH = new column position
;
;Uses:
;	None
;
;Exceptions:
;
;******************************************************************************
cProc	LPOUT1,<NEAR>		
cBegin				

	CALL	$UPDPOS 	;[DH]=new column position(AL, DH)
	CMP	AL,ASCLF
	JNE	LPOUT2		;branch if not attempting to output LF
	CMP	b$EOL,1	; end-of-line processing ?
	JNE	LPOUT2		; brif not end-of-line processing, the
				; user might be doing something like
				; print chr$(13)+chr$(10)
	TEST	BYTE PTR _LPFLG[DI],_LPCRF
	JNE	LPOUT3		;brif last byte out was CR (eat LF)
LPOUT2:
	CALL	LPROUT		;output the character
LPOUT3:
	AND	BYTE PTR _LPFLG[DI],255-_LPCRF ;reset last byte out was CR flag
	CMP	AL,ASCCR
	JNE	LPOUTX		;return if wasn't carriage return
	OR	BYTE PTR _LPFLG[DI],_LPCRF ;set last byte out CR flag

	CMP	SI,OFFSET DGROUP:b$LPTFDB	; Test if LPRINT FDB
	JZ	OUTLF		;branch if Pseudo FDB (LPRINT)

	CMP	FileDB.FD_MODE,MD_RND	;MODE = RANDOM?
	JNE	OUTLF		; brif not -- print LF after a CR
	CMP	FileDB.FD_WIDTH,255	;WIDTH = 255?
	JE	LPOUTX		; brif so -- suppress LF following CRs
OUTLF:
	PUSH	AX
	MOV	AL,ASCLF
	CALL	LPROUT
	POP	AX
LPOUTX:
cEnd				


$CRIFEL:
	CALL	LPOUT1		; output character
	CMP	DH,DL		; compare column with width
	JB	NOCR		; Brif still room on current line
	CMP	AL,32		; printable character ?
	JB	NOCR		; brif non-printable
	CMP	DL,255		; infinite width ?
	JZ	NOCR		; Brif so (width = 255)
				; else output <CR><LF>

WRICR:				; output <CR>
	PUSH	AX		; save char
	MOV	AL,ASCCR
	CALL	LPOUT1		
	POP	AX		; restore char
NOCR:				
	RET

;$UPDPOS - update column position (called by device out routines)
; Entry - [DH] = current 0-relative column position
;	  [AL] = byte to be output
; Exit	- [DH] = new column position.  All other registers preserved

$UPDPOS: CMP	AL,32
	JB	NPRINT		;branch if not printable (CTL CHR)
	INC	DH		;bump column position
	RET
NPRINT: CMP	AL,ASCCR
	JNE	NOTCR		;branch if not carriage return
ZERPOS: MOV	DH,0		;reset to left margin
	RET
NOTCR:	CMP	AL,ASCBS
	JNE	UPPOSX		;branch if not backspace
	OR	DH,DH
	JE	UPPOSX		;don't decrement below 0
	DEC	DH		;decrement position
UPPOSX: RET



;	GPOS - Get position

;	EXIT	(AH) = device position

LPT_GPOS:
	MOV	AH,POSITION	;Get position (device)
	RET


;	GWID - Get width

;	EXIT	(AH) = file width

LPT_GWID:
	MOV	AH,FileDB.FD_WIDTH  ;Get printer (file) width
	RET


fileio	ENDP

;***
; B$LPOS - Line printer position
;
; Purpose:
;	Get line printer position
; Input:
;	lptID = printer # (map 1 into 0, etc.)
; Output:
;	(AX) = printer position
; Modifies:
;	per convention
; Exceptions:
;	Control may be transfered to B$ERR_FC
;****
cProc	B$LPOS,<PUBLIC,FAR>	
parmW	lptID			
cBegin				
	MOV	BX,lptID	
	CMP	BX,NUM_LPT	;Check for too high
	JA	ercfc
	OR	BX,BX		;Test for 0 - same as 1
	JZ	lpos1
	DEC	BX		;Map 1 to 0
lpos1:	SHL	BX,1		;BX=0,2,4... offset into B$LPTWID
	MOV	AL,LPTPOS[BX]	;Get position for printer #n
	XOR	AH,AH		
	INC	AX		
cEnd				

ercfc:	JMP	B$ERR_FC

;***
; B$LWID - WIDTH LPRINT Statement
;
; Purpose:
;	Sets the LPRINT file width and device width
; Input:
;	newWidth = desired LPRINT file width
; Output:
;	NONE
; Modifies:
;	Per convention
; Exceptions:
;	NONE
;****
cProc	B$LWID,<PUBLIC,FAR>	
parmW	newWidth		
cBegin				
	MOV	DX,newWidth		;  Get New Width for Printer
	or	dh,dh			;  If new widht is >255 Error
	jnz	ercfc			;  and
	or	dl,dl			;  If new width is zero Error
	jz	ercfc			

	MOV	B$LPTWID,DL		;Set printer 1 width
	MOV	b$LPTFDB.FD_WIDTH,DL	;Set LPRINT file width
cEnd				

;GLPDCB - get pointer to LPT Device Control Block
; Entry - [DI] = -2*device id (2,4,..n)
; Exit	- DI points to the device control block for device DI.
;	  [AH] = 0..n for LPT1, LPT2, ...
;		 Note: interpreter returns unit number in AX!

GLPDCB: CALL	B$GLPUID 	; [AH]=unit id (0..n)
	PUSH	AX		;save unit id
	xchg	ah,al
	xor	ah,ah
	SHL	AX,1		;2 fields in DCB
	ADD	AX,OFFSET DGROUP:B$LPTWID
	MOV	DI,AX		;DI points to LPTx device ctl block
	POP	AX		;[AX]=unit id
	RET

;***
;B$LPTECHO
;Purpose:
;	Echo to the line printer.		[13]
;Entry:
;	[AX] = character code
;Exit:
;	none
;Modifies:
;	none ([AX] is preserved)
;****
cProc	B$LPTECHO,<PUBLIC,NEAR>,<DI,SI,AX>
cBegin
	MOV	SI,OFFSET DGROUP:b$LPTFDB	
	MOV	DI,DN_LPT1*-2	; lpt1:
	OR	AH,AH		;  Test for two byte character
	JZ	ONEBYT
	PUSH	AX
	XCHG	AH,AL
	CALL	LPT_SOUT	; Write the high byte first
	POP	AX
ONEBYT:
	CALL	LPT_SOUT	; Then the low byte
cEnd

; Entry - [DI] = -2*device id (2,4,..n)
; Exit	- [AH] = 0..n for LPT1, LPT2, ...
; Uses AH

cProc	B$GLPUID,<NEAR>,<BX>	
cBegin

	MOV	BX,DI
	ADD	BX,2*DN_LPT1
	SHR	BX,1		;[BX]=0, 1. for LPT1, LPT2, ...
	MOV	AH,BL		; return (printer # - 1) * 2 in AH

cEnd


;	File close routines
;	Deallocate file block

cProc	LPT_CLOSE,<NEAR>	
cBegin

	PUSH	BX		;save register
	MOV	BX,FileDB.FD_HANDLE ; get handle of printer
	CALL	B$CLSLPT	; close the device
	POP	BX		;restore the register
	CMP	SI,OFFSET DGROUP:b$LPTFDB ; is this LPRINT?
	JE	LPRINT_CLOSE	;if so, then branch
	CALL	B$LHDALC_CPCT	; deallocate FDB and compact local heap
LPRINT_CLOSE:

cEnd				; return to caller

	SUBTTL	interface for LPRINT preamble
	page
;***
;B$LPRT -- LPRINT preamble
;void B$LPRT(void)
;
;Purpose:
;	This is the only interface for LPRINT preamble.  BASCOM 2.0 uses two
;	preambles, $PR0E for LPRINT and $PR0F for LPRINT USING.  This routine
;	sets up flag, b$PRFG, to 8 (LPSTM) to indicate LPRINT is on going.
;	If there is LPRINT USING, B$USNG will OR the flag, b$PRFG, with
;	USING (=2) to indicate USING is on going.
;Entry:
;	none
;Exit:
;	b$PTRFIL = LPRINT FDB pointer/handle
;	b$PRFG is set to LPSTM (=8)
;Uses:
;	none
;Exceptions:
;	none
;*******************************************************************************

cProc	B$LPRT,<PUBLIC,FAR>,<SI> ; don't set up frame (can't get an error)
cBegin

	OR	[b$PRFG],LPSTM ;set the flag to indicate a LPRINT is on going

	MOV	[b$PTRFIL],OFFSET DGROUP:b$LPTFDB ;set up b$PTRFIL

	MOV	SI,OFFSET DGROUP:b$FILVEC
	cCall	B$WCHSET	;set the dispatch vector for print items

cEnd				; exit to caller



sEnd	OI_TEXT 		

	END
