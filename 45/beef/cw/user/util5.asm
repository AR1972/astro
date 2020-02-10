;*
;*	COW : Character Oriented Windows
;*
;*	util5.asm : user utilities : DOS 5 specifics : included by util.asm

.xlist
	include	user.inc
.list
	include insyd.inc
	include util5.inc


externFP	<DosEnterCritSec, DosExitCritSec>
externFP	<DosBeep>

;****************************************************************************

sBegin	DATA

staticW	hDirFde,0		; Handle used by FindFirst/FindNext/FindClose

sEnd	DATA

;****************************************************************************

sBegin USER_CORE
    assumes CS,USER_CORE
    assumes DS,DATA
    assumes SS,DATA

;********** DisableInterrupts **********
;*	* CLI for Cmerge code

cProcNorF	DisableInterrupts,<>
cBegin
	cCall	DosEnterCritSec
cEnd


;********** EnableInterrupts **********
;*	* STI for Cmerge code

cProcNorF	EnableInterrupts,<>
cBegin
	cCall	DosExitCritSec
cEnd

sEnd	USER_CORE

;*****************************************************************************


;*	* Directory listbox helpers

externFP  <DosFindFirst, DosFindNext, DosFindClose>
externFP  <DosQCurDisk, DosQCurDir>
externFP  <DosSelectDisk, DosChdir>
externFP  <DosQFileMode>


sBegin	USER_LISTBOX
    assumes CS,USER_LISTBOX
    assumes DS,DATA
    assumes SS,DATA
    assumes ES,nothing


;********** FFindFirst **********
;*	entry : pfde = address of FDE buffer for file find
;*		szPath = path to search for
;*		atr = file attribute to search for
;*	* Find first occurance of a file spec with given attributes for search.
;*	exit : AX = TRUE if file found, FALSE otherwise

cProcNorF	FFindFirst,<di,si>
    parmDP pfde
    parmDP szPath
    parmW  atr
    localW centryFirst			; Number of directory entries.
    localV FindFirstBuf,<size FDE_OS2>	; Xfer from here to passed-in buffer
cBegin	FFindFirst

	PushArg	<ss, szPath>		;* Path file name

	mov	[hDirFde],-1		;* request new directory handle
	PushArg	<ds>			;* place for
	PushArg	<offset hDirFde>	;*   directory handle

	PushArg	<atr>			;* attribute for match

	lea	ax,FindFirstBuf
	PushArg	<ss, ax>		;* result buffer
	PushArg	<size FDE_OS2>		;* size of result buffer

	lea	bx,centryFirst
	mov	wo [bx],1
	PushArg	<ss, bx>

	xor	ax,ax
	PushArg	<ax>
	PushArg	<ax>

	cCall	DosFindFirst
	or	ax,ax
	mov	ax,0			; Preserve flags!
	jnz	FF99			; Jump if none found.
	AssertEq	centryFirst,1	; Should have found exactly 1 file.

;   Transfer info from FindFirstBuf.FDE_OS2 structure to pfde.FDE struct.

	mov	ax,ss
	mov	es,ax
	assumes	es,data
	mov	bx,pfde
	lea	di,[bx.atrFde]		; Where we transfer to.

	mov	ax,FindFirstBuf.attrFileFdeOS2
	xor	ah,ah
	stosb

	mov	ax,FindFirstBuf.wTimeWriteFdeOS2
	stosw
	
	mov	ax,FindFirstBuf.wDateWriteFdeOS2
	stosw
	
	mov	ax,FindFirstBuf.OFF_cbFileAllocFdeOS2
	stosw

	mov	ax,FindFirstBuf.SEG_cbFileAllocFdeOS2
	stosw

	xor	cx,cx
	mov	cl,FindFirstBuf.cchNameFdeOS2
	lea	si,FindFirstBuf.szNameFdeOS2	; Where we transfer from.
	rep	movsb				; Transfer the filename.
	xor	ax,ax
	stosb					; Zero-terminate.

	dec	ax				; Success
FF99:

cEnd	FFindFirst


;********** FFindNext **********
;*	entry : pfde = pointer to FDE initialized by FFindFirst
;*	* Find next occurance / close directory handle if no more found
;*	exit : AX = TRUE if file found, FALSE otherwise

    assumes CS,USER_LISTBOX
    assumes DS,DATA
    assumes SS,DATA
    assumes ES,nothing

cProcNorF	FFindNext,<di,si>
    parmDP pfde
    localW centryNext			; Number of directory entries.
    localV FindNextBuf,<size FDE_OS2>	; Xfer from here to passed-in buffer
cBegin	FFindNext

	PushArg	<hDirFde>	;* directory handle
	
	lea	ax,FindNextBuf
	PushArg	<ss, ax>		;* result buffer
	PushArg	<size FDE_OS2>		;* size of result buffer

	lea	bx,centryNext
	mov	wo [bx],1
	PushArg	<ss, bx>

	cCall	DosFindNext
	or	ax,ax
	mov	ax,0			; Preserve flags!
	jnz	FN99			; Jump if none found.
	AssertEq	centryFirst,1	; Should have found exactly 1 file.

;   Transfer info from FindNextBuf.FDE_OS2 structure to pfde.FDE struct.

	mov	ax,ss
	mov	es,ax
	assumes	es,data
	mov	bx,pfde
	lea	di,[bx.atrFde]		; Where we transfer to.

	mov	ax,FindNextBuf.attrFileFdeOS2
	xor	ah,ah
	stosb

	mov	ax,FindNextBuf.wTimeWriteFdeOS2
	stosw
	
	mov	ax,FindNextBuf.wDateWriteFdeOS2
	stosw
	
	mov	ax,FindNextBuf.OFF_cbFileAllocFdeOS2
	stosw

	mov	ax,FindNextBuf.SEG_cbFileAllocFdeOS2
	stosw

	xor	cx,cx
	mov	cl,FindNextBuf.cchNameFdeOS2
	lea	si,FindNextBuf.szNameFdeOS2	; Where we transfer from.
	rep	movsb				; Transfer the filename.
	xor	ax,ax
	stosb					; Zero-terminate.

	dec	ax				; Success
FN99:

cEnd	FFindNext


;********** FindClose *********
;*	entry : none.
;*	* close directory handle
;*	exit : n/a
;*
;*  It seems that DosFindClose will return an error if you've already
;*  found all the files with DosFindNext.

    assumes CS,USER_LISTBOX
    assumes DS,DATA
    assumes SS,DATA
    assumes ES,nothing

cProcNorF	FindClose,<>
cBegin	FindClose

	push	hDirFde
	call	DosFindClose

cEnd	FindClose



;********** AtrOfPath **********
;*	entry : sz = path (file / directory or other)
;*	* get the status of the file/path
;*	exit : ax = ATR = attribute (-1 for error)

cProcNorF	AtrOfPath,<DS>
    parmDP szPath
    localW atrCur	;* where to put attrib.
cBegin	AtrOfPath

	mov	bx,szPath
	PushArg	<ss,bx>
	lea	bx,atrCur
	PushArg	<ss,bx>
	xor	ax,ax
	PushArg	<ax,ax>			;* reserved
	cCall	DosQFileMode
	or	ax,ax
	mov	ax,atrCur		;* ax = atr
	jz	atr_ok			;* return atr
	mov	ax,-1			;* return error
atr_ok:

cEnd	AtrOfPath




;********** GetCurDrive **********
;*	entry : n/a
;*	* get the current drive letter
;*	exit : AL = current drive letter 'A' ...

cProcNorF	GetCurDrive,<>
    localD rgfExists	;* not used
    localW dnCur	;* current drive # result
cBegin	GetCurDrive
	lea	bx,dnCur
	PushArg	<ss,bx>
	lea	bx,rgfExists
	PushArg	<ss,bx>
	cCall	DosQCurDisk
	AssertEq	ax,0		; Shouldn't err.
	mov	ax,dnCur
	add	al,'A'-1		; (1..26) -> (A..Z)
cEnd	GetCurDrive


;********** SetCurDrive **********
;*	entry : chDrive = drive letter
;*	* set the current drive letter
;*	exit : n/a
;*
;*   Note that it is NOT allowed to call this procedure with a bogus drive
;*   letter.  Use FValidDrive to determine legitimacy.

cProcNorF	SetCurDrive,<>
    parmB chDrive
cBegin	SetCurDrive
	mov	al,chDrive
	and	ax,LOW(NOT 20H)			; convert lower case to upper
	sub	al,'A'-1			; A->1, B->2, etc.
	AssertCmp	ax,be,26		; Legit range = 
	AssertNE	ax,0			;          (1..26).
	cCall	DosSelectDisk,<ax>
	AssertEq	ax,0			; Shouldn't err.
cEnd	SetCurDrive



;********** GetCurDir **********
;*	entry : chDrive = drive letter
;*		szBuff = string buffer for directory <=256 bytes
;*	* get the current default directory
;*	* asserts if you give it a bogus drive letter.
;*	exit : n/a

    assumes CS,USER_LISTBOX
    assumes DS,DATA
    assumes SS,DATA
    assumes ES,nothing

cProcNorF	GetCurDir,<si,di>
    parmB chDrive
    parmDP szBuff
    localW cchBuff		;* size of buffer
cBegin	GetCurDir

	AssertUP

	mov	si,szBuff		; Get pointer to target area.
	mov	al,chDrive
	and	ax,LOW (not 20H)	; convert lower case to upper case.

	AssertCmp	al,ae,'A'	; Bust out if we're not in the 
	AssertCmp	al,be,'Z'	;   (A..Z) range.

	mov	[si],al
	mov	word ptr [si+1], '\:'	; C:\
	lea	si,[si+3]

	mov	byte ptr [si],ah	; zero terminate in case of error.
	sub	al,'A' - 1		; 0:Current drive, 1:A, 2:B, ...
	lea	bx,cchBuff
	mov	word ptr ss:[bx],256	; Hard coded maximum

	cCall	DosQCurDir, <ax, ds, si, ss, bx>

cEnd	GetCurDir



;********** FSetCurDir **********
;*	entry : szDir : directory
;*	* set the default directory
;*	exit : AX = TRUE if ok, FALSE if bogus drive and/or directory

    assumes CS,USER_LISTBOX
    assumes DS,DATA
    assumes SS,DATA
    assumes ES,nothing

cProcNorF	FSetCurDir,<si>
parmDP	szDir
localW	OrigDrive
cBegin	FSetCurDir

; Get the current drive, and save it away
;
; If the second char is a colon, then
;   If !FValidDrive(szDir[0]) then return FALSE
;   SetCurDrive(szDir[0])
;   If szDir[2] = NULL, then it's just drive & no directory: ret TRUE
;
; Do the DOS call to Set Current Dir.
;   if no error, return TRUE
;   else call SetCurDrive(saved original current drive), and ret FALSE

	call	GetCurDrive
	mov	OrigDrive,ax

	mov	si,szDir
	cmp	by [si+1], ':'
	jne	noDriveSpec

	push	[si]			; If the requested drive is bogus,
	call	far ptr FValidDrive	;   then return False.
	or	ax,ax
	jz	end_set_dir

	push	[si]			; Set the drive to the request.
	call	SetCurDrive

	mov	ax,sp			; Guess the following is True.
	cmp	by [si+2],0		; If setting drive, but not setting
	je	end_set_dir		; directory, then quit now & ret True

noDriveSpec:

	cCall	DosChdir, <ds, si, 0, 0>
	or	ax,ax
	mov	ax,sp			; Guess no error
	jz	end_set_dir		; If no error, jump and return True.

	push	OrigDrive		; Error in setting directory!
	call	SetCurDrive		; Reset drive to original, and
	xor	ax,ax			;   return False.
end_set_dir:

cEnd	FSetCurDir



;********** FValidDrive **********
;*	entry : chDrive = drive to see if exists
;*	* check whether drive exists or not
;*	exit : AX = TRUE if ok, FALSE if bogus drive

cPublic	FValidDrive, <ATOMIC>
    parmB chDrive		;* 'A' -> 'Z' (case insensitive)
    localD rgfExists	;* array of bits (1 per drive)
    localW dnCur	;* current drive (not used).
cBegin	FValidDrive

	lea	bx,dnCur
	PushArg	<ss,bx>
	lea	bx,rgfExists
	PushArg	<ss,bx>
	cCall	DosQCurDisk
	or	ax,ax
	jnz	fval_drive_error

;*	* bit lookup
	mov	cl,chDrive
	and	cl,NOT 20H			;* toupper
	sub	cl,'A'				;* cl = bit index
	mov	ax,word ptr (rgfExists)		;* assume low word
	test	cl,16
	jz	use_low_word
	mov	ax,word ptr (rgfExists+2)	;* use high word
use_low_word:	;* ax = value, cx = index
	and	cl,15				;* lower 4 bits
	shr	ax,cl
	and	ax,1				;* 1 if exists
fval_drive_end:

cEnd	FValidDrive

fval_drive_error:
	xor	ax,ax
	jmp	short fval_drive_end


sEnd	USER_LISTBOX

;*****************************************************************************

	END
