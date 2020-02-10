	TITLE	IOCONS - Machine Independent CONS: Device Support
;***
; IOCONS - Machine Independent CONS: Device Support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; Contains the code to drive the machine independant "CONS:" device. Under DOS
; 2/3, we use direct console I/O to output data, and under DOS 5, VIO calls.
; This is a redirectable output only device which DOES get run through device
; drivers for escape sequence detection.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	

	USESEG	CN_TEXT 	
	USESEG	_DATA		

	INCLUDE seg.inc 	
	INCLUDE idmac.inc	
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE rtps.inc	; constants shared with QBI


sBegin	CN_TEXT 		
	assumes CS,CN_TEXT	

	externNP	B$ERR_BFM	
	externNP	B$LHDALC_CPCT	
	externNP	B$OPEN_DEV 	
	externNP	B$CON_SOUT	

	PAGE
	SUBTTL	CONS (Raw-CRT output Dispatch Table and Routines)

;   equates to make dspnam generate tables that dispatch directly to
;   the error routines.

CON_EOF 	EQU	B$ERR_BFM
CON_LOC 	EQU	B$ERR_BFM
CON_LOF 	EQU	B$ERR_BFM
CON_RANDIO	EQU	B$ERR_BFM
CON_BAKC	EQU	B$ERR_BFM
CON_SINP	EQU	B$ERR_BFM
CON_BLKIN	EQU	B$ERR_BFM
CON_BLKOUT	EQU	B$ERR_BFM


;   Device Independent Interface

DSPMAC	MACRO	func
	DW	CON_&func	;;Define Dispatch Table for Specific Device
	ENDM

;Console Dispatch Table 	;must not be at CS:0
;

labelNP <PUBLIC,B$D_CONS>	
	DSPNAM			;make dispatch table (from devdef)


;*** 
;CON_OPEN - perform any device dependent open functions.
;
;Purpose:
;	Open the CONS: device for output.
;
;
;Entry:
;	[AL]=  device id
;		0 if default device,
;		1..n for Disk A:, B:, ...
;		-1..-n for non-disk devices
;	[BX] = file number (0..n)
;	[CX] = random record size if [FILMOD] = random (ignored)
;	[DI] = device offset (2=SCND, 4=SCRN, etc.)
;	[FILMOD] = file mode
;		 MD.SQI = 1 ;sequential input
;		 MD.SQO = 2 ;sequential output
;		 MD.RND = 3 ;random
;		 MD.APP = 4 ;append
;Exit:
;	[SI] points to new FDB
;	FDB is linked into FDB chain with all standard fields initialized.
;Uses:
;	AX,DX
;
;Exceptions:
;	None, though exits through B$OPEN_DEV which can generate errors
;******************************************************************************

cProc	CON_OPEN,<NEAR>		
cBegin
	MOV	AH,MD_SQO	; allow open for output only
	MOV	DL,255D 	; initial file width=255
	JMP	B$OPEN_DEV	; allocate FDB with no buffer
				; and return
cEnd	<nogen>

;***
;CON_CLOSE - Console file close      ;[6-30-83 5.40 pbs]
;
;Purpose:
;	Close the CONS: device and deallocate its file block.
;
;Entry/Exit/Uses/Exceptions:
;	Same as B$LHDALC_CPCT
;****
CON_CLOSE EQU	B$LHDALC_CPCT	; jump directly to deallocate file block
				; compacts heap after deallocation

;***
;CON_GPOS - return current file position.
;
;Purpose:
;	Returns the current column position of CONS:.  Since CONS: is a
;	stream device without a width, 0 is returned instead.
;
;Entry:
;	SI points to File-Data-Block.
;
;Exit:
;	[AH] = current file column. (0-relative)
;
;Uses:
;	SI is an entry value
;
;Preserves:
;	AL, BX, CX, DX
;
;Exceptions:
;	None.
;****
cProc	CON_GPOS,<NEAR>
cBegin
	MOV   AH,0		
cEnd	<nogen> 		;fall through to return

;***
;CON_DWID, CON_WIDTH - Set CONS: width
;
;Purpose:
;	This function fullfills the requirement that all devices
;	have a routine to set their width (so that the dispatch
;	table does not have holes.  Since CONS: has no concept
;	as width, these routines just return.
;****
labelNP	<CON_DWID>		; no action necessary for CONS:
cProc	CON_WIDTH,<NEAR>	;so just return
cBegin
cEnd

;***
;CON_GWID - get device width for CONS:
;
;Purpose:
;	Return the width of the CONS: device.  Since CONS: does not
;	have a width, this routine will return 255 (= infinite width).
;
;Entry:
;	SI - Pointer to File Data Block
;
;Exit:
;	[AH] = device width = 255
;
;Uses:
;	SI is an entry condition.
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	None.
;****
cProc	CON_GWID,<NEAR>
cBegin
	MOV	AH,255		; infinite width
cEnd

;***
;CON_SOUT - Write one byte to the console.
;
;Purpose:
;	This routine will send a single byte to the CON: device.  This
;	is a redirectable output device.
;
;Entry:
;	AL = Byte to output
;
;Exit:
;	None.
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX,BX,CX,DX
;
;Exceptions:
;	None.
;****
CON_SOUT	EQU	B$CON_SOUT	

sEnd	CN_TEXT 		
	END
