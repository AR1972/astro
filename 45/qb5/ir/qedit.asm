	TITLE	QEDIT - Loader for the QEDIT editor
;***
;QEDIT - Loader for the QEDIT editor
;
;	Copyright (C) 1990, Microsoft Corporation
;
;Purpose:
;	This file implements QEDIT.COM.  The purpose of this com file
;	is to invoke QBASIC.EXE with the /editor switch, passing on all
;	other command line options
;
;*******************************************************************************


	Stack_Size  = 256	; size of stack in bytes
	Path_Size   = 128	; max size of path + filename of QBASIC
	Max_CmdTail = 73	; max size of program options after /EDCOM
	OldCmdLine  = 80h	; DS:OldCmdLine = ptr to QEDIT command line
	EnvSeg	    = 2ch	; DS:EnvSeg	= ptr to QEDIT environment


code	SEGMENT PARA PUBLIC 'CODE'

	ASSUME	cs:code, ds:code, es:code, ss:code

	ORG	0100h

qedit:
	mov	bx, offset TopOfStack	; BX = ptr to top of stack (end of mem)
	mov	sp,bx			; move stack to perminent memory
	add	bx,000fh		; round up to units of paragraphs
	shr	bx,1
	shr	bx,1
	shr	bx,1
	shr	bx,1			; BX = # paragraphs of memory
	mov	ah,4ah			; modify memory block
	int	21h
	mov	dx, offset ErrART	; assume an error
	jnc	@F
GiveError_Near:
	jmp	GiveError		; brif error
@@:

	; set up the parameter block
	mov	ax,cs
	mov	[PB_CmdLineSeg],ax	; set the segment of the command line

	; set up the command line
	mov	al, ds:[OldCmdLine]	; al = length of old command line
	cmp	al, Max_CmdTail 	; is the command line too long
	mov	dx, offset ErrCmd	; assume so, prepare error
	ja	GiveError_Near		; brif too long

	cbw				; zero extend ax (cant be > 80)
	mov	cx, ax			; save in cx
	inc	cx			; also copy over 0D terminator
	add	ax, CmdTail-CmdLine	; ax = length of new command line
	mov	[CmdLine],al		; and save it
	mov	si, offset OldCmdLine+1 ; DS:SI = ptr to old command line
	mov	di, offset CmdTail	; DS:DI = ptr to new command line
	rep	movsb			; and copy it over

	; set up the program name

	mov	ax, ds:[EnvSeg]		; ax = segment of environment
	mov	es, ax
	xor	di, di			; ES:DI = ptr to environment
	xor	ax, ax			; look for a zero
	mov	pszPATH,-1		; indicate that no PATH found

LocateFname:
	mov	cx, cbPATH		; length of 'PATH='
	mov	si, offset stPATH	; see if this is the PATH specification
	repe	cmpsb
	jne	NotPath 		; brif not PATH
	mov	pszPATH, di		; EnvSeg:pszPATH = ptr to PATH data

NotPath:
	dec	di			; back up a character
	mov	cx, 8000h		; scan a long way for a 0 if needed
	repne	scasb			; skip an environment string
	cmp	es:[di],al		; is next byte a null?
	jne	LocateFname		; not double null term, keep looking

	inc	di			; skip last null
	scasw				; skip word containing 1h

	mov	si, di			; ES:SI = ptr to QEDIT path & name
	mov	di, offset ProgName	; DS:DI = ptr to QBASIC path & name
	push	es
	pop	ds			; DS:SI = ptr to QEDIT path & name
	push	cs
	pop	es			; ES:DI = ptr to QBASIC path & name

	mov	ah,30h			; get version number
	int	21h			; are we in DOS 3 or above?

	mov	bx,di			; CS:BX = ptr to first char in QEDIT
					; name as it is copied over
	cmp	al,3
	jb	HavePath		; brif not DOS 3 or above, no path

CopyName:
	lodsb				; grab a byte
	stosb				; and store it
	cmp	al,'\'                  ; is it a slash
	je	PathChar		;  go remember it
	cmp	al,'/'			; or this slash
	je	PathChar		;  go remember it
	cmp	al,':'			; or a colon
	jne	TestTerminator		; brif not
PathChar:
	mov	bx,di			; set last path character found
TestTerminator:
	or	al,al			; 0 terminator?
	jnz	CopyName		; brif not, more characters to do

HavePath:
	push	cs
	pop	ds			; restore ES = DS = CS

	cmp	bx, offset ProgName	; does it have any path?
	jne	@F			; brif so
	mov	fCurDir,1		; else flag cur directory tried
@@:
	mov	di, bx			; ES:DI = ptr to end of QEDIT path
	mov	si, offset szQBASIC	; DS:SI = ptr to new file name
	mov	cx, cbQBASIC		; CX = # characters
	rep	movsb			; and copy it over

	; all set up, lets go exec the program

	mov	dx, offset ProgName
	mov	bx, offset ParmBlock
	mov	ax, 4b00h
	int	21h			; Exec program
	jnc	GetRetCode		; brif ok

	mov	dx,offset ErrOM
	cmp	ax,8			; is it out of memory?
	jne	TryPathSearch		; brif not, assume file not found

GiveError:
	push	cs			; set DS=code segment (for error)
	pop	ds

	mov	ah,9
	int	21h			; print error message
	mov	ax,4cffh		; terminate program, return code FF
	int	21h
	;Does not return

GetRetCode:
	mov	ah,4dh
	int	21h			; al = return code of previous prog
	mov	ah,4ch
	int	21h			; exit with return code in al
	;Does not return


; We could not load QBASIC from the directory that QEDIT was located in.
; We will now try the directories in the PATH.

TryPathSearch:
	cmp	fCurDir,0		; have we looked in current dir?
	mov	bx, offset ProgName	; assume not, setup to try with no path
	jz	HavePath		; brif not, go try it.
	mov	dx, offset ErrFNF	; prepare for error
	mov	ax, cs:[EnvSeg]		; ax = segment of environment
	mov	ds, ax
	mov	si, cs:pszPATH		; DS:SI = ptr to PATH data
	cmp	si,-1			; was there a PATH env variable?
	je	GiveError		; brif not, give File Not Found

	push	cs
	pop	es
	mov	di, offset ProgName	; ES:DI = buffer to construct filename

NextPathChar:
	lodsb				; get a character of a path
	stosb				; save character
	cmp	al,';'			; end of path?
	je	PathEnd 		; brif so
	or	al,al			; end of string
	jne	NextPathChar		; brif not
	mov	si,-1			; indicate no more path
PathEnd:
	mov	cs:pszPATH,si		; set up pointer for next iteration
	dec	di			; back up over the ';' or 0 terminator
	mov	al,'\'                  ; look for a back slash
	cmp	byte ptr es:[di-1],al	; does it end in a backslash?
	je	HaveFullPath		; brif so
	cmp	byte ptr es:[di-1],'/'	; or does it end in a forward slash?
	je	HaveFullPath		; brif so
	stosb				; else, add it
HaveFullPath:
	mov	bx, di			; CS:BX = ptr to where to fill in name
	jmp	HavePath


ErrOM	db	'Out of memory$'
ErrFNF	db	'Can not find file QBASIC.EXE$'
ErrART	db	'DOS memory-arena error$'
ErrCMD	db	'Command line too long$'

szQBASIC db	'QBASIC.EXE', 0
cbQBASIC = $ - szQBASIC

stPATH	db	'PATH='
cbPATH	= $ - stPATH

pszPATH dw	?			; near ptr to PATH data
fCurDir db	0			; have we looked in the current dir?

ParmBlock dw	0			; use QEDITs environment
	dw	offset CmdLine		; offset of command line
PB_CmdLineSeg dw	0		; segment of command line
	dd	-1			; use default for FCB 1
	dd	-1			; use default for FCB 2


; NOTE: The command tail, program name, and stack do not have any
; NOTE: representation in this file.  They are allocated after the
; NOTE: command line in the memory that DOS allocates for us.  This
; NOTE: allows the COM file to be only 1 sector in size, thus saving
; NOTE: disk space.

CmdLine db	0			; length of command line
	db	'/EDCOM '		; static part of command line
CmdTail db	0			; user specified part of command line

ProgName =	CmdTail + Max_CmdTail -1; program to exec
TopOfStack =	ProgName + Path_Size + Stack_Size ; Top of stack = end of mem


code	ENDS

	END	qedit
