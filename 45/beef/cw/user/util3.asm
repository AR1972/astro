;*
;*	COW : Character Oriented Windows
;*
;*	util3.asm : user utilities : DOS 3 specifics : included by util.asm

.xlist
	include	user.inc
.list
	include insyd.inc
	include util3.inc


sBegin	DATA

externB	drivePhantom			; set to 2 on single floppy systems.
externW	chDrivePhantom			; set to "b:" on one floppy systems.

staticD	lpdtaOldFde,0		; Hold the pointer to the old DTA.

;* from kernel init
externB fInt24Error				;* checked for all file i/o

sEnd	DATA


sBegin USER_CORE
    assumes CS,USER_CORE
    assumes DS,DATA
    assumes SS,DATA
    assumes ES,nothing


;********** DisableInterrupts **********
;*	* CLI for Cmerge code

cProcNorF	DisableInterrupts,<>
cBegin
	CLI
cEnd


;********** EnableInterrupts **********
;*	* STI for Cmerge code

cProcNorF	EnableInterrupts,<>
cBegin
	STI
cEnd

sEnd	USER_CORE

;*****************************************************************************

;*	* Directory listbox helpers

sBegin	USER_LISTBOX
    assumes CS,USER_LISTBOX
    assumes DS,DATA
    assumes SS,DATA
    assumes ES,nothing



;********** FDoFile **********
;*	entry : all registers set for INT 21H call
;*	* call INT 21, check for INT 24 errors
;*	exit : AX != 0 if ok, AX == 0 if error
;*		all other registers set from INT 21H call

cProcNorF	FDoFile,<>
cBegin	FDoFile
	mov	fInt24Error,0
	int	21h
	mov	ax,0			;* assume error
	jc	dofile_end
	cmp	fInt24Error,al
	jne	dofile_end		;* an INT 24 error
	inc	ax			;* success
dofile_end:

cEnd	FDoFile



;********** FFindFirst **********
;*	entry : pfde = pointer to FDE to fill
;*		szPath = path to search for
;*		atr = file attribute to search for
;*	* Find first occurance of a file spec with given attributes for search.
;*	exit : AX = TRUE if file found, FALSE otherwise

cProcNorF	FFindFirst,<>
    parmDP pfde
    parmDP szPath
    parmW  atr
cBegin	FFindFirst

	StartPublic

	mov	ah,2fh		;* get old Dta
	int	21h

	mov	wo lpdtaOldFde,bx
	mov	wo lpdtaOldFde+2,es	;* save old Dta
	
	mov	dx,pfde
	mov	ah,1ah		; set dta
	int	21h

	mov	dx,szPath

	mov	bx,dx
	mov	ax,[bx]			; Get first two letters
	or	al,20h			; If the file is "B:xxx" on a single
	cmp	ax,chDrivePhantom	;   floppy system, then no can do.
	mov	ax,0
	je	@F

	mov	ah,4eh		;Search for first occurance.
	mov	cx,atr

	cCall	FDoFile
@@:

	StopPublic

cEnd	FFindFirst



;********** FFindNext **********
;*	entry : pfde (not used) (uses info set by previous FFindFirst)
;*	* Find next occurance.
;*	exit : AX = TRUE if file found, FALSE otherwise
;*	* NOTE : assumes DTA already set

cProcNorF	FFindNext,<>
   parmDP pfde			;* not used (assuming DTA already set)
cBegin	FFindNext

	StartPublic

	mov	ah,4fh		;Find next occurance of file string.
	cCall	FDoFile

	StopPublic

cEnd	FFindNext


;********** FindClose **********
;*	entry : none.
;*	* Close find (restore old DTA)
;*	exit : n/a

cProcNorF	FindClose,<DS>
cBegin	FindClose

	StartPublic

	lds	dx,lpdtaOldFde		;* get old dta
	mov	ah,1ah			;* set dta
	int	21h

	StopPublic

cEnd	FindClose



;********** AtrOfPath **********
;*	entry : sz = path (file / directory or other)
;*	* get the status of the file/path
;*	exit : ax = ATR = attribute

cProcNorF	AtrOfPath,<DS>
   parmDP szPath
cBegin	AtrOfPath

	mov	dx,szPath

	mov	bx,dx
	mov	ax,[bx]			; Get first two letters
	or	al,20h			; If the file is "B:xxx" on a single
	cmp	ax,chDrivePhantom	;   floppy system, then no can do.
	mov	ax,-1
	je	atr_error

	mov	ax,4300h		;* CHMOD (get old status)
	cCall	FDoFile
	or	ax,ax
	mov	ax,-1
	jz	atr_error		;* error
	mov	ax,cx			;* return attribute
atr_error:

cEnd	AtrOfPath



IFNDEF REMOVE_LATER

;********** GetCurDrive **********
;*	entry : n/a
;*	* get the current drive letter
;*	exit : AL = current drive letter 'A' ...

cProcNorF	GetCurDrive,<>
cBegin	GetCurDrive

	StartPublic

	mov	ah,19h
	int	21h
	add	al,'A'			; (0..25) -> (A..Z)

	StopPublic

cEnd	GetCurDrive


;********** SetCurDrive **********
;*	entry : chDrive = drive letter
;*	* set the current drive letter
;*	exit : rets AL = number of logical drives (used for True = nonzero)
;*	       This is used by FSetCurDir,
;*	       Note that the OS/2 version of this doesn't return anything.

cProcNorF	SetCurDrive,<>
    parmB chDrive
cBegin	SetCurDrive

	StartPublic

	mov	dl,chDrive
	and	dl,not 20H			; convert lower case to upper
	sub	dl,'A'				; A->0, B->1, etc.
	AssertCmp	dl,be,25		; Legit range = (0..25).
	mov	ah,0eH

	mov	al,drivePhantom		; Note! This stupid Dos call has 
	dec	al			;   0=A, 1=B unlike all others.
	cmp	dl,al			; Don't set B to current drive with
	je	@F			;   single floppy (still ret AX!=0)

	int	21h
@@:
	StopPublic

cEnd	SetCurDrive



;********** GetCurDir **********
;*	entry : chDrive = drive letter
;*		szBuff = string buffer for directory <=64 bytes
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
cBegin	GetCurDir

	StartPublic

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

	cmp	al,drivePhantom		; CurDir of B w/single floppy system
	je	@F			;   is "B:\".

	mov	dl,al
	mov	ah,47h
	int	21h
@@:
	StopPublic

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

	StartPublic

	call	GetCurDrive
	mov	OrigDrive,ax

	mov	si,szDir
	cmp	byte ptr [si+1], ':'
	jne	noDriveSpec

	push	[si]			; If the requested drive is bogus,
	call	far ptr FValidDrive	;   then return False.
	or	ax,ax
	jz	end_set_dir

	push	[si]			; Set the drive to the request.
	call	SetCurDrive		; Rets al = # drives, so AX = True.
	AssertNE	ax,0		; Prove it.

	cmp	byte ptr [si+2],0	; If setting drive, but not setting
	je	end_set_dir		; directory, then quit now & ret True

noDriveSpec:
	mov	dx,szDir
	mov	ah,3Bh
	int	21h
	mov	ax,sp			; Guess no error
	jnc	end_set_dir		; If no error, jump and return True.

	push	OrigDrive		; Error in setting directory!
	call	SetCurDrive		; Reset drive to original, and
	xor	ax,ax			;   return False.
end_set_dir:

	StopPublic

cEnd	FSetCurDir


ENDIF ;!REMOVE_LATER


;----------------------------------------------------------------------------
;
; FValidDrive
;
;   Check for phantom drives and non-existant drives.  Note: for single
;   floppy systems, either 'A' or 'B' will not exist.
;
;   input  : chDrive = drive to see if exists
;   output : AX = TRUE if ok, FALSE if bogus drive
;

cPublic	FValidDrive, <ATOMIC>
    parmB chDrive				; 'A'...'Z' (case insensitive)
cBegin	FValidDrive

	StartPublic

	xor	ax,ax				; Guess invalid.
	mov	dl,chDrive
	or	dl,20h				; tolower
	cmp	dl,by chDrivePhantom
	je	phantom

; Test for existance by setting then checking if set.

	mov	ah,19h
	int	21h
	push	ax				; Save old drive

	sub	dl,'a'				; 'A' -> 0, etc.
	mov	ah,0eH
	int	21h				; Set query drive.
	mov	ah,19h
	int	21h				; And get current drive.
	xor	cx,cx
	cmp	al,dl				; Are they the same ?
	jnz	@F
	inc	cx				; They're diffo.
@@:
	pop	dx
	mov	ah,0eH				; Restore original
	int	21h				;   current drive.

	mov	ax,cx

phantom:

	StopPublic

cEnd	FValidDrive


;********** PrepareWild **********
;*	entry : pfcb => FCB structure to fill.
;*		szFile => file name to parse
;*	* parse path name, fill FCB with wild find info
;*	exit : *pfcb filled with FCB info for FMatchWild calls

cProcNorF	PrepareWild,<SI,DI>
    parmDP pfcb
    parmDP szFile
cBegin	PrepareWild

	mov	si,szFile
	mov	di,pfcb
	push	ds
	pop	es				;* es:di => FCB
	mov	ax,2900H			;* Parse filename
						;* (no special processing).
	int	21h
;*	* no error codes make sense

cEnd	PrepareWild



;********** TestWildPart **********
;*	entry : SI => source (ending in '.' or '\0').
;*		DI => fcb image
;*		CX = # of characters in fcb image
;*	* scan for part of wildcard match
;*	* NOTE : may pop return address !!!
;*	exit : if match so far, return AL = '.' or '\0' terminator
;*		SI => after terminator
;*	  if no match, pop return address, jump to "fmatch_fail"

cProcNorF	TestWildPart,<>
cBegin	nogen	;TestWildPart

test_filename_loop:
	lodsb
	or	al,al
	jz	finish_test_wild
	cmp	al,'.'
	je	finish_test_wild
	mov	ah,ds:[di]			;* get match character
	inc	di
	cmp	ah,'?'
	je	match_this_one
	cmp	ah,al
	je	match_this_one
;*	* comparison failed
match_one_failed:
	pop	ax				;* kill return address
	jmp	short fmatch_fail			;* no match

match_this_one:
	AssertNE cx,0				;* must never run out
	dec	cx
	jmp	short test_filename_loop

finish_test_wild:
;*	* if CX != 0, then remaining FCB characters must be ' ' or '?'
	jcxz	end_test_wild

test_end_loop:
	mov	ah,ds:[di]
	inc	di
	cmp	ah,'?'
	je	test_end_next
	cmp	ah,' '
	jne	match_one_failed
test_end_next:
	loop	test_end_loop
end_test_wild:	;* SI => after terminator, AL == terminator ('.' or ' ').
	ret

cEnd	nogen	;TestWildPart



;********** FMatchWild **********
;*	entry : pfcb => FCB structure (prepared with PrepareWild)
;*		szFile => file name to match (must be well formed)
;*	* check to see if file name will match wildcard
;*	exit : ax != 0 if match, ax == 0 if no match

cProcNorF	FMatchWild,<SI,DI>
    parmDP pfcb
    parmDP szFile
cBegin	FMatchWild

	mov	si,szFile
	mov	bx,pfcb
;*	* test for filename (first 0->8 characters will be filename)
	lea	di,[bx+1]			;* start with filename
	mov	cx,8
	cCall	TestWildPart

	or	al,al
	jz	no_extension

	lea	di,[bx+1+8]
	mov	cx,3
	cCall	TestWildPart

;*	* if we get here we have succeeded
fmatch_ok:
	mov	ax,sp
fmatch_end:

cEnd	FMatchWild

no_extension:	;* if there is no extension, we should match no or any extension
	mov	al,[bx+1+8]		;* 1st char of extension
	cmp	al,'?'
	je	fmatch_ok		;* match anything
	cmp	al,' '
	je	fmatch_ok		;* match no extension
fmatch_fail:
	xor	ax,ax
	jmp	short fmatch_end



;********** FValidDir **********
;*	entry :	szTestDir = path of a directory (maybe)
;*	* perform "stat" to see if a path is a directory
;*	exit : AX = TRUE if a real directory, FALSE otherwise

cProcNorF	FValidDir,<>
    parmDP szTestDir
cBegin	FValidDir

	mov	dx,szTestDir

	mov	bx,dx
	mov	ax,[bx]			; Get first two letters
	or	al,20h			; If the file is "B:xxx" on a single
	cmp	ax,chDrivePhantom	;   floppy system, then no can do.
	je	err_fvalid_dir

	mov	ax,4300H			;* read attrib
	int	21h
	jc	err_fvalid_dir
	and	ax,10H				;* AX != 0 if directory
	jnz	end_fvalid_dir
;*	* unfortunately sometimes directory stat returns non-directory flags
;*	* if it was a ".." then assume it is ok
	mov	bx,szTestDir
	cmp	word ptr [bx],'..'
	jne	err_fvalid_dir
	mov	ax,sp				;* assume success
	cmp	byte ptr [bx+2],0		;* "..\0" ?
	je	end_fvalid_dir

err_fvalid_dir:	;* an error occured
	xor	ax,ax				;* return 0
end_fvalid_dir:
cEnd	FValidDir


sEnd	USER_LISTBOX

;*****************************************************************************

	END
