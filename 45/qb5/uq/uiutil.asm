	TITLE	uiutil.asm - low level utilities for SHELL
;*** 
;uiutil.asm
;
;	Copyright <C> 1985-1988, Microsoft Corporation
;
;Purpose:
;	Support routines needed by user-interface.
;
;
;*******************************************************************************

	.xlist
	include	version.inc
	.list

	include cw/version.inc	
	include cw/windows.inc	
	include cw/edityp.inc	

	IncludeOnce architec	
	IncludeOnce qbimsgs	
	IncludeOnce ui		


assumes DS,DATA
assumes ES,DATA
assumes SS,DATA

	subttl	DATA segment definitions.
	page

externFP CbSz			

DOS_LOGICAL_DRIVE EQU	504H

sBegin	DATA
staticB	DisketteDrives,-1	; Number of Diskette Drives on machine
				; -1 ==> not yet determined
externW	__doserrno
externW	iHelpId			
externB fMousePresent
sEnd	DATA


sBegin	UI
assumes CS,UI

externNP MsgBoxPsz		

	subttl	Low level MSDOS File I/O calls.
	page

;***
;
;  GetCurDrive2()
;
;	Returns the current drive letter.
;
;  Inputs:	none
;
;  Outputs:	AL is the current drive letter
;
;  Uses:	AX, DX.
;
;****

cProc	GetCurDrive2,<NEAR,PUBLIC>
cBegin
	mov	ah, 19h
	int	21h
	add	al, 'A'
	xor	ah, ah
cEnd


;***
;
;  GetCurDriveDir(szPath)
;
;	Gets the current working directory and drive into the supplied buffer.
;	Re-written in assembler and moved here with revision [1].
;
;  Inputs:
;	szPath = pointer to buffer area.  (<=64 bytes long).
;
;  Outputs:
;	None.
;  Uses:
;	Per convention.
;
;****

cProc	GetCurDriveDir,<NEAR,PUBLIC>,<SI>
	parmW	szPath
cBegin
	cCall	GetCurDrive2		; al = current drive letter

	mov	si,szPath		;Get pointer to target area.
	mov	Byte Ptr [si],al	;store drive letter
	mov	Word Ptr [si+1],'\:'	; store ":\"
	add	si,3			;advance past "drive:\"


	;We do not have to worry about KANJI characters here, as
	;drive designators can only be single byte characters

	and	al, not 20H		; convert lower case to upper case.
	sub	al, 'A' - 1		; 0 -> Current drive, 1 -> A, 2 -> B,...
	mov	dl,al
	mov	ah, 47h
	int	21h

	mov	bx, 0			
	mov	[__doserrno],bx		
	xchg	ax,bx
	jnc	GetCurDriveDir_Exit

	mov	[__doserrno],bx
	dec	ax

GetCurDriveDir_Exit:

cEnd

	page

;***
;
;  SetCurDrive2()
;
;	Sets the current drive.
;
;  Inputs:	letter - is the drive letter.
;
;  Outputs:	none.
;
;  Uses:	AX, DX.
;
;****

cProc	SetCurDrive2,<NEAR,PUBLIC>
	parmB	letter
cBegin
	mov	dl, letter
	push	dx
	cCall	CheckSwitchDiskettes,<dx>
	pop	dx
	and	dl, not 20H			; convert lower case to upper
	sub	dl, 'A'				; A->0, B->1, etc
	mov	ah, 0eH
	int	21h
cEnd


;***
;
;  SetCurDir2(pArea)
;
;  Set's the current working directory from the supplied pointer buffer.
;
;  Inputs:	pointer to buffer area.  <= 64 bytes.
;
;  Outputs:	AX == 0  for normal completion.
;		AX == -1 for error during operation.
;
;  Uses:	AX, DX.
;
;****

cProc	SetCurDir2,<NEAR,PUBLIC>
	parmDP	pArea
cBegin
	xor	ax,ax
	mov	[__doserrno],ax

	mov	bx, pArea
	push	bx			; save for later
	mov	dx,[bx]			; dx = possible drive letter + colon
	cmp	dh, ':'			; drive specified?
	jne	NoDriveSpec		; brif not

	cCall	SetCurDrive2,<dx>	; set current drive

noDriveSpec:
	pop	dx			; dx = *directory name
	mov	ah, 3Bh			; set current directory
	int	21h

	mov	bx,0			; assume success
	xchg	ax,bx			; ax = return code
	jnc	SetCurDir_Exit		; brif success, with ax = 0

	mov	[__doserrno],bx
	dec	ax			; ax = -1 ==> failure

SetCurDir_Exit:
cEnd

	page



;***
;
; CheckSwitchDiskettes( drive )
;
; Inputs:	drive - is the drive letter we are about to access
;
; Outputs:	none
;
; Uses:	all
;
;****
cProc CheckSwitchDiskettes,<NEAR,PUBLIC>
	parmB	drive
cBegin
	mov	al,DisketteDrives		; al = # disk drives
	cmp	al,-1				; have we initialized it?
	jne	InitDone			; brif so


	int	11H				; BIOS Equipment List
	shl	ax, 1				
	shl	ax, 1				
	and	ah, 00000011B			
	inc	ah				
	mov	al,ah				
	mov	DisketteDrives, al		; save value for next time
						; so we don't have to do the
						; INT every time

InitDone:					; al = # disk drives
	cmp	al, 1				
	jne	EndCheckSwitchDiskettes

	mov	dl, drive
	and	dl, not 20H			; convert lower case to upper
	sub	dl, 'A'				; A->0, B->1, etc
	cmp	dl, 1
	ja	EndCheckSwitchDiskettes		; only drives A and B matter

	xor	ax, ax				
	mov	es, ax				
	mov	al, es:[DOS_LOGICAL_DRIVE]	; Get current logical drive
	cmp	dl, al
	je	EndCheckSwitchDiskettes

; Well now we know... We have to switch diskettes.
	push	es				; save segment 0 for later
	push	dx				; save drive number

	mov	ax,MSG_SwitchDisks	; "Insert diskette for drive A:"
	mov	iHelpid,ax			
	cCall	ListStdMsg,<ax>			; get message into bufStdMsg


	pop	ax				; restore AX = drive number
	push	ax				; save drive # again
	lea	bx,[bufStdMsg+26]		; BX = ptr to "A:"
	add	[bx],al				; change to 'B:' if necessary

	mov	ax,MB_OK			; display the message
	push	ax				
	mov	ax,OFFSET DGROUP:bufStdMsg	
	push	ax				
	call	MsgBoxPsz			

	; actually switch the diskettes
	pop	dx				; DX = drive number
	pop	es				; ES = 0
	mov	es:[DOS_LOGICAL_DRIVE], dl	; Tell DOS we switched the
						; diskettes

	mov	ah, 0dh				; reset the disk
	int	21h				

EndCheckSwitchDiskettes:
cEnd

	page

;***
;
;  FlushFile()
;
;  Flushes all DOS buffers for the file to disk.
;
;  Inputs:	fd - file descriptor for the file to flush.
;
;  Outputs:	
;
;  Uses:	AX, DX.
;
;****
cProc	FlushFile,<NEAR,PUBLIC>
	parmW	fd

cBegin
	mov	bx, [fd]
	mov	ah, 45H
	int	21H			; Duplicate the handle
	jc	FlushFileExit

	mov	bx, ax
	mov	ah, 3eH
	int	21H			; Close the dup'd handle to flush.

FlushFileExit:
cEnd


;***
;
;  fstrcpy2( fpDst, fpSrc )
;
;  Move null terminated string from Src to Dst
;
;  Inputs:
;		far pointer destination.
;		far pointer source.
;
;  Outputs:	none.
;
;  Uses:	AX, CX.
;
;****

cProc	fstrcpy2,<NEAR,PUBLIC>,<SI, DI, DS>
	ParmD	fpDst
	ParmD	fpSrc
cBegin
	les	di, fpDst
	lds	si, fpSrc
fstrcpy1:
	lodsb
	stosb
	cmp	al, 0
	jnz	fstrcpy1

cEnd

;***
;
;  CbSzUi(psz) 
;
;  Added with revision [4] to save bytes.
;
;  Return length of null-terminated string, not including the null
;  The length is returned in bytes (not characters).
;
;  Inputs:
;		psz = near pointer to string
;
;  Outputs:	AX = length in bytes.
;
;  Uses:	Per convention
;
;****
cProc	CbSzUI,<NEAR,PUBLIC>
cBegin
	pop	ax		; ax = return address

				; parm(s) are on the stack
	push	cs		; push return segment
	push	ax		; push return offset
	jmp	CbSz		; "CALL" CbSz,<parm(s)>
cEnd	<nogen>


;***
;
;  fmemcpy(fpDst, fpSrc, cb )
;
;  Move cb bytes (not characters) from Src to Dst
;
;  Inputs:
;		far pointer destination.
;		far pointer source.
;		unsigned integer for number of bytes to move.
;
;  Outputs:	none.
;
;  Uses:	AX, CX.
;
;****

cProc	fmemcpy,<NEAR,PUBLIC>,<SI, DI, DS>
	ParmD	fpDst
	ParmD	fpSrc
	ParmW cb

cBegin
	mov	cx, cb
	jcxz	fmemcpyEnd		;Early exit.

	les	di, fpDst
	lds	si, fpSrc
	repz	movsb

fmemcpyEnd:
cEnd

	subttl	Character mapping routines
	page

; These tables now conform to IBM code page 850

labelB	<Translation_Table>
	DB	081h, 09ah, 'U'
	DB	082h, 090h, 'E'
	DB	083h, 0b6h, 'A'
	DB	084h, 08eh, 'A'
	DB	085h, 0b7h, 'A'
	DB	086h, 08fh, 'A'
	DB	087h, 080h, 'C'
	DB	088h, 0d2h, 'E'
	DB	089h, 0d3h, 'E'
	DB	08ah, 0d4h, 'E'
	DB	08bh, 0d8h, 'I'
	DB	08ch, 0d7h, 'I'
	DB	08dh, 0deh, 'I'
	DB	091h, 092h, 'A'
	DB	093h, 0e2h, 'O'
	DB	094h, 099h, 'O'
	DB	095h, 0e3h, 'O'
	DB	096h, 0eah, 'U'
	DB	097h, 0ebh, 'U'
	DB	098h,  'Y', 'Y'
	DB	09bh, 09dh, 'O'
	DB	0a0h, 0b5h, 'A'
	DB	0a1h, 0d6h, 'I'
	DB	0a2h, 0e0h, 'O'
	DB	0a3h, 0e9h, 'U'
	DB	0a4h, 0a5h, 'N'
	DB	0a6h,  'A', 'A'
	DB	0a7h,  'O', 'O'
	DB	0c6h, 0c7h, 'A'
	DB	0e4h, 0e5h, 'O'
	DB	0e7h, 0e8h, 'P'
	DB	0ech, 0edh, 'Y'
	DB	0, 0, 0			; table end marker


;re-written in assembler, and moved here with revision [5].
;re-wrote table lookup with revision [7].
;*** 
;tolower, toupper, toupperNoAccent -- character conversion routines
;
;Purpose:
;
;	Convert a character to either lower or upper case
;	If char > 127, does an international mapping of the character
;	If toupperNoAccent, converts it to the upper case version of the
;	'Normal' character.
;
;Entry:
;	chr	= character to convert
;
;Exit:
;	AL	= converted char
;
;Uses:
;	Per convention
;
;*******************************************************************************
labelNP	<PUBLIC, toupperNoAccent>
	mov	dx,UIOFFSET ReturnNoAccent	; return a upper case char
	jmp	short toupper2			; without any accent

labelNP	<PUBLIC, toupper>
	mov	dx,UIOFFSET ReturnUpper		; return a upper case char
toupper2:
	mov	cx,'Aa'				; CX = map lower to upper
	jmp	short MapChar

labelNP	<PUBLIC, tolower>
	mov	dx,UIOFFSET ReturnLower		; return a lower case char
	mov	cx,'aA'				; CX = map upper to lower

cProc	MapChar,<NEAR>
parmB	chr		; char to convert
cBegin

	; BX = translation table, CL = old A-Z start, CH = new A-Z start
	mov	al,chr				; AL = char to convert
	or	al,al				; above 127?
	js	DoTableLookup			; brif so -- map the sucker

	push	ax				; save char
	sub	al,cl				; make zero-relative
	cmp	al,26				; out of range?
	pop	ax				; restore char
	jae	MapCharExit			; brif out of range - no change
	sub	al,cl				; make zero-relative again
	add	al,ch				; translate
	jmp	short MapCharExit		; done


DoTableLookup:
	mov	bx,UIOFFSET Translation_Table

TryNextEntry:
	mov	cx,cs:[bx]			; cl = lower case char
						; ch = upper case char
	jcxz	MapCharExit			; brif end of table reached
	inc	bx				; prepare to do next one
	inc	bx
	inc	bx
	cmp	al,cl				; lower case char match?	
	je	TableMatch			; brif so
	cmp	al,ch				; upper case char match?	
	jne	TryNextEntry			; brif not -- try again

TableMatch:
	jmp	dx				; return right result
ReturnNoAccent:
	mov	al,cs:[bx-1]			; return no-accent character 
	SKIP2_PSW				; exit
ReturnUpper:
	mov	al,ch				; return upper case character
	SKIP2_PSW				; exit
ReturnLower:
	mov	al,cl				; return lower case character

MapCharExit:
cEnd

;re-written in assembler, and moved here with revision [5].
;*** 
;lower -- convert a character string to lower case
;
;Purpose:
;
;Entry:
;	psz	= near pointer to string to convert
;
;Exit:
;	string converted to lower case
;
;Uses:
;	Per convention
;
;*******************************************************************************
cProc	lower,<NEAR,PUBLIC>,<SI>
parmW	psz		; string to convert
cBegin
	mov	si,psz				; si = * string to convert
NextChar:
	lodsb					; al = next char in string
	cCall	tolower,<ax>			; convert the char
	mov	[si-1],al			; store converted char 
	or	al,al				; all done?
	jne	NextChar			; brif not
cEnd



;***
;
; CbSizeMouseState
;
; Returns the number of bytes required to save the mouse state.
;
; Inputs:	none
;
; Outputs:	AX = number of bytes
;
; Uses:		AX, BX, CX, DX.
;
;****
cProc cbSizeMouseState,<FAR,PUBLIC>
cBegin
	xor	ax,ax
	cmp	[fMousePresent], al
	je	@F

	mov	ax, 21
	xor	bx,bx			; Won't be changed if not supported
	int	33H
	mov	ax, bx
@@:
cEnd

;***
;
; SaveMouseState
;
; Saves the mouse state in global memory
;
; Inputs:	pointer to buffer for mouse save data
;
; Outputs:	none
;
; Uses:		AX, BX, CX, DX.
;
;****
cProc SaveMouseState,<FAR,PUBLIC>
	parmD	pBuffer
cBegin
	les	dx, [pBuffer]
	mov	ax, 22
	int	33H
cEnd

;***
;
; RestoreMouseState
;
; Restores the mouse state saved by SaveMouse.
;
; Inputs:	pBuffer - pointer to buffer containing saved mouse state.
;
; Outputs:	none.
;
; Uses:		AX, BX, CX, DX.
;
;****

cProc RestoreMouseState,<FAR,PUBLIC>
	parmD	pBuffer
cBegin
	les	dx, [pBuffer]
	mov	ax, 23
	int	33H
cEnd

sEnd	UI
	end
