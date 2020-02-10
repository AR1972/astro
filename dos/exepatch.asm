;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;** EXEPATCH.ASM 
;
;	Contains the foll:
;
;		- code to find and overlay buggy unpack code
;		- new code to be overlayed on buggy unpack code 
;		- old code sequence to identify buggy unpack code
;
;	Revision history:
;
;		Created: 5/14/90
;
;
;
;----------------------------------------------------------------------------
;
; M020 : Fix for rational bug - for details see routine header
; M028 : 4b04 implementation
; M030 : Fixing bug in EXEPACKPATCH (EXEC_CS is an un-relocated value)
; M032 : set turnoff bit only if DOS in HMA.
; M033 : if IP < 2 then not exepacked.
; M046 : support for a 4th version of exepacked files.
; M068 : support for copy protected apps.
; M071 : use A20OFF_COUNT of 10.
;
;----------------------------------------------------------------------------
;
.XLIST
.XCREF

INCLUDE version.inc
INCLUDE dosseg.inc
INCLUDE DOSSYM.INC
INCLUDE exe.inc
INCLUDE	PDB.INC

.CREF
.LIST


	public	exepatch
	public	RationalPatch				; M020
	public	Rational386Patch
	public	IsCopyProt

PATCH1_COM_OFFSET	EQU	06CH
PATCH1_OFFSET		EQU	028H
PATCH1_CHKSUM		EQU	0EF4EH
CHKSUM1_LEN		EQU	11CH/2

PATCH2_COM_OFFSET	EQU	076H
PATCH2_OFFSET		EQU	032H

	; The strings that start at offset 076h have two possible 
	; check sums that are defined as PATCH2_CHKSUM PATCH2A_CHKSUM

PATCH2_CHKSUM		EQU	78B2H
CHKSUM2_LEN		EQU	119H/2
PATCH2A_CHKSUM		EQU	1C47H		; M046
CHKSUM2A_LEN		EQU	103H/2		; M046

PATCH3_COM_OFFSET	EQU	074H
PATCH3_OFFSET		EQU	032H
PATCH3_CHKSUM		EQU	4EDEH
CHKSUM3_LEN		EQU	117H/2




DOSDATA	SEGMENT

;	EXTRN	exec_signature	:WORD	; Must contain 4D5A  (yay zibo!)

;	EXTRN	exec_init_IP 	:WORD	; IP of entry
;	EXTRN	exec_init_CS 	:WORD	; CS of entry

	EXTRN	UNPACK_OFFSET	:WORD
	EXTRN	RatBugCode	:FAR
	EXTRN	RationalPatchPtr:word
	EXTRN	fixexepatch	:word		; M028
	EXTRN	Rational386PatchPtr:word
	EXTRN	Special_Version	:word		; M028
;	EXTRN	A20OFF_FLAG	:byte
	extrn	DosHasHMA	:byte		
	extrn	A20OFF_COUNT	:byte		; M068
	extrn	A20OFF_PSP	:word		; M068
	extrn	DOS_FLAG	:byte		; M068

DOSDATA	ENDS


DOSCODE	SEGMENT

; M028 - BEGIN

	EXTRN	Scan_Execname1:near
	EXTRN	Scan_Special_Entries:near

	ASSUME	CS:DOSCODE, SS:DOSDATA

	public	ExecReady

;-------------------------------------------------------------------------
;
;	Procedure Name		: ExecReady
;
;	Input			: DS:DX -> ERStruc (see exe.inc)
;
;--------------------------------------------------------------------------

ExecReady	proc	near

	mov	si, dx			; move the pointer into a friendly one
	test	[si].ER_Flags, ER_EXE	; COM or EXE ?
	jz	er_setver		; only setver for .COM files

	mov	ax, [si].ER_PSP
	add	ax, 10h
	mov	es, ax

	mov	cx, word ptr ds:[si].ER_StartAddr	; M030
	mov	ax, word ptr ds:[si+2].ER_StartAddr	; M030

	call	[fixexepatch]
	call	[Rational386PatchPtr]

er_setver:
					; Q: is this an overlay
	test	[si].ER_Flags, ER_OVERLAY
	jnz	er_chkdoshi		; Y: set A20OFF_COUNT if DOS high
					; N: set up lie version first
	push	ds
	push	si
	lds	si, [si].ER_ProgName
	call	Scan_Execname1
	call	Scan_Special_Entries
	pop	si
	pop	ds
	mov	es, [si].ER_PSP
	mov	ax, Special_Version
	mov	es:[0].PDB_Version, ax
er_chkdoshi:
	cmp	[DosHasHMA], 0		; M032: Q: is dos in HMA (M021)
	je	er_done			; M032: N: done

					; M068 - Start
	mov	ax, [si].ER_PSP		; ax = PSP

	or	[DOS_FLAG], EXECA20OFF	; Set bit to signal int 21
					; ah = 25 & ah= 49. See dossym.inc 
					; under TAG M003 & M009 for 
					; explanation

	test	[si].ER_Flags, ER_EXE	; Q: COM file
	jnz	er_setA20		; N: inc a20off_count , set 
					;    a20off_psp and ret
   	push	ds
	mov	ds, ax			; DS = load segment of com file.
	call	IsCopyProt		; check if copy protected
	pop	ds

er_seta20:

	;
	; We need to inc the A20OFF_COUNT here. Note that if the count
	; is non-zero at this point it indicates that the A20 is to be 
	; turned off for that many int 21 calls made by the app. In 
	; addition the A20 has to be turned off when we exit from this 
	; call. Hence the inc.
	;

	inc	[A20OFF_COUNT]		
	mov	[A20OFF_PSP], ax	; set the PSP for which A20 is to be
					; turned OFF.
er_done:				; M068 - End
	xor	ax, ax
	ret
ExecReady	endp

; M028 - END

;----------------------------------------------------------------------------
;
; procedure : Rational386Patch
;
; Older versions of the Rational DOS Extender have several bugs which trash
; 386 registers (usually just the high word of 32 bit registers) during
; interrupt processing.  Lotus 123 3.1+ is a popular application that uses a
; version of the Rational extender with the 32 bit register trashing bugs.
;
; This routine applies patches to the Rational DOS Extender to work around
; most of the register trashing bugs.
;
; Note that there are additional register trashing bugs not fixed by these
; patches.  In particular, the high word of ESP and the FS and GS registers
; may be modified on interrupts.
;
; There are two different Rational DOS Extender patchs in this module.
; Rational386Patch is to correct 386 register trashing bugs on 386 or later
; processors.  This patch code is executed when MS-DOS is running on a 386
; or later processor, regardless of whether MS-DOS is running in the HMA
; or not.
;
; The other Rational patch routine (RationalPatch, below) fixes a register
; trashing problem on 286 processors, and is only executed if MS-DOS is
; running in the HMA.
;
; This patch detection and replacement is based on an example supplied by
; Ben Williams at Rational.
;
;----------------------------------------------------------------------------

;----------------------------------------------------------------------------
;
; INPUT : ES = segment where program got loaded
;
;----------------------------------------------------------------------------

rpFind1 	db	0FAh, 0E4h, 21h, 60h, 33h, 0C0h, 0E6h, 43h, 8Bh, 16h
rpFind1Len	equ	$ - offset rpFind1

;	cli
;	in	al, 21h
;	pusha
;	xor	ax, ax
;	out	43h, al
;	mov	dx, ...

rpFind1a	db	0B0h, 0Eh, 0E6h, 37h, 33h, 0C0h, 0E6h, 0F2h
rpFind1aLen	equ	$ - offset rpFind1a

;	mov	al, 0Eh
;	out	37h, al
;	xor	ax, ax
;	out	0F2h, al


; bug # 1 -- loss of high EAX on 386+ if not VCPI or DPMI

rpFind2 	db	0Fh, 20h, 0C0h
rpFind2Len	equ	$ - offset rpFind2

;	mov	eax, cr0	;may be preceeded by PUSH CX (51h)

rpFind3 	db	0Fh, 22h, 0C0h, 0EAh
rpFind3Len	equ	$ - offset rpFind3

;	mov	cr0, eax	;may be preceeded by POP CX (59h)
;	jmp	far ptr xxx	;change far ptr to go to replace3
;	mov	ss, bx		;8E D3 ... and come back at or after this

; note, there is no rpRepl1 string

rpRepl2 	db	66h, 50h, 51h, 0Fh, 20h, 0C0h
rpRepl2Len	equ	$ - offset rpRepl2

;	push	eax
;	push	cx
;	mov	eax, cr0

rpRepl3 	db	8Eh, 0D3h, 59h, 66h, 58h
rpRepl3Len	equ	$ - offset rpRepl3

;	mov	ss, bx
;	pop	cx
;	pop	eax

; bug # 2 -- loss of high EAX and ESI on 386+ only if VCPI

rpFind4 	db	93h, 58h, 8Bh, 0CCh
rpFind4Len	equ	$ - offset rpFind4

;	xchg	bx, ax
;	pop	ax
;	mov	cx, sp

rpFind5 	db	0B8h, 0Ch, 0DEh, 0CDh, 67h, 8Bh, 0E1h, 0FFh, 0E3h
rpFind5Len	equ	$ - offset rpFind5

;	mov	ax, DE0Ch
;	int	67h
;	mov	sp, cx
;	jmp	bx

rpRepl4 	db	93h, 58h, 8Bh, 0CCh
		db	02Eh, 066h, 0A3h
rpRepl4o1Len	equ	$ - offset rpRepl4
		db	00h, 00h
		db	02Eh, 066h, 89h, 36h
rpRepl4o2Len	equ	$ - offset rpRepl4
		db	00h, 00h
rpRepl4Len	equ	$ - offset rpRepl4

;	xchg	bx, ax
;	pop	ax
;	mov	cx, sp
;	mov	dword ptr cs:[xxxx], eax
;	mov	dword ptr cs:[xxxx], esi

rpRepl5 	db	8Bh, 0E1h
		db	2Eh, 66h, 0A1h
rpRepl5o1Len	equ	$ - offset rpRepl5
		db	00h, 00h
		db	2Eh, 66h, 8Bh, 36h
rpRepl5o2Len	equ	$ - offset rpRepl5
		db	00h, 00h
		db	0FFh, 0E3h
rpRepl5Len	equ	$ - offset rpRepl5

;	mov	sp, cx
;	mov	eax, dword ptr cs:[xxxx]
;	mov	esi, dword ptr cs:[xxxx]
;	jmp	bx

; bug # 3 -- loss of high EAX, EBX, ECX, EDX on 386+ only if VCPI

rpFind6 	db	0FAh, 52h, 51h
rpFind6Len	equ	$ - offset rpFind6

;	cli
;	push	dx
;	push	cx

rpFind7a	db	0B8h, 0Ch, 0DEh, 66h, 26h, 0FFh, 1Eh
rpFind7aLen	equ	$ - offset rpFind7a

;	mov	ax, 0DE0Ch
;	call	fword ptr es:[xxxx]

rpFind7b	db	59h, 5Ah, 5Bh
rpFind7bLen	equ	$ - offset rpFind7b

;	pop	cx
;	pop	dx
;	pop	bx

rpRepl6 	db	0FAh, 66h, 50h, 66h, 53h, 66h, 51h, 66h, 52h
rpRepl6Len	equ	$ - offset rpRepl6

;	cli
;	push	eax
;	push	ebx
;	push	ecx
;	push	edx

rpRepl7 	db	66h, 5Ah, 66h, 59h, 66h, 5Bh, 66h, 58h, 5Bh
rpRepl7Len	equ	$ - offset rpRepl7

;	pop	edx
;	pop	ecx
;	pop	ebx
;	pop	eax
;	pop	bx

; bug # 4 -- loss of high EAX and EBX on 386+ only if VCPI

rpFind8 	db	60h, 06h, 1Eh, 0B8h, 00h, 00h, 8Eh, 0D8h
rpFind8Len	equ	$ - offset rpFind8

;	pusha
;	push	es
;	push	ds
;	mov	ax, dgroup	;jump back to here from replace8
;	mov	ds, ax

rpFind9 	db	1Fh, 07h, 61h
rpFind9Len	equ	$ - rpFind9

;	pop	ds
;	pop	es
;	popa

rpRepl8 	db	66h, 60h, 06h, 1Eh
rpRepl8Len	equ	$ - offset rpRepl8

;	pushad
;	push	es
;	push	ds

rpRepl9 	db	1Fh, 07h, 66h, 61h, 0C3h
rpRepl9Len	equ	$ - offset rpRepl9

;	pop	ds
;	pop	es
;	popad
;	ret			;no need to jmp back to main-line

;----------------------------------------------------------------------------

SearchPair	STRUC
sp_off1 	dw	?		; offset of 1st search string
sp_len1 	dw	?		; length of 1st search string
sp_off2 	dw	?		; 2nd string
sp_len2 	dw	?		; 2nd string
sp_diff 	dw	?		; max difference between offsets
SearchPair	ENDS

rpBug1Strs	SearchPair	<offset rpFind2, rpFind2Len, offset rpFind3, rpFind3Len, 20h>
rpBug2Strs	SearchPair	<offset rpFind4, rpFind4Len, offset rpFind5, rpFind5Len, 80h>
rpBug3Strs	SearchPair	<offset rpFind6, rpFind6Len, offset rpFind7a, rpFind7aLen, 80h>
rpBug4Strs	SearchPair	<offset rpFind8, 4, offset rpFind9, rpFind9Len, 80h>

;----------------------------------------------------------------------------

StackVars	STRUC
sv_wVersion	dw	?		; Rational extender version #
sv_cbCodeSeg	dw	?		; code seg size to scan
sv_pPatch	dw	?		; offset of next avail patch byte
StackVars	ENDS

;----------------------------------------------------------------------------

Rational386Patch  proc	near

	; Do a few quick checks to see if this looks like a Rational
	; Extended application.  Hopefully this will quickly weed out
	; most non Rational apps.

	cmp	word ptr es:[0], 395	; version number goes here - versions
	jae	rp3QuickOut		;   3.95+ don't need patching

	cmp	word ptr es:[0Ch], 20h	; always has this value here
	jne	rp3QuickOut

	push	ax

	mov	ax, 18h 		; extender has 18h at
	cmp	es:[24], ax		;   offsets 24, 28, & 36
	jne	rp3QO_ax
	cmp	es:[28], ax
	jne	rp3QO_ax
	cmp	es:[36], ax
	je	rp3Maybe

rp3QO_ax:
	pop	ax

rp3QuickOut:
	ret

; It might be the rational extender, do more extensive checking

rp3Maybe:

	cld
	push	bx		; note ax pushed above
	push	cx
	push	dx
	push	si
	push	di
	push	es
	push	ds			; we use all of them
	push	bp
	sub	sp, SIZE StackVars	; make space for stack variables
	mov	bp, sp

	push	cs
	pop	ds
	assume	ds:nothing

	mov	ax, es:[0]		; save version #
	mov	[bp].sv_wVersion, ax
					; check that binary version # matches
	call	VerifyVersion		;   ascii string
	jne	rp3Exit_j


; Looks like this is it, find where to put the patch code.  The
; patch will be located on top of Rational code specific to 80286
; processors, so these patchs MUST NOT be applied if running on
; an 80286 system.

	; Rational says the code to patch will never be beyond offset 46xxh

	mov	cx, 4500h		; force search len to 4700h (searches
	mov	[bp].sv_cbCodeSeg, cx	;   start at offset 200h)

	mov	es, word ptr es:[20h]	; es=code segment

	mov	si, offset rpFind1	; string to find
	mov	dx, rpFind1Len		; length to match
	call	ScanCodeSeq		; look for code seq
	jz	rpGotPatch

; According to Rational, some very old versions of the extender may not
; have the find1 code sequence.  If the find1 code wasn't found above,
; try an alternative patch area which is on top of NEC 98xx switching code.

	mov	si, offset rpFind1a
	mov	dx, rpFind1aLen
	call	ScanCodeSeq
	jz	rpGotPatch

rp3Exit_j:
	jmp	rp3Exit

; Found the location to write patch code!  DI = offset in code seg.

rpGotPatch:
	mov	[bp].sv_pPatch, di	; save patch pointer

;----------------------------------------------------------------------------
; Bug # 1 -- loss of high EAX on 386+ if not VCPI or DPMI

	cmp	word ptr [bp].sv_wVersion, 381	; only need bug 1 if version
	jae	rpBug2				;   < 3.81

	mov	bx, offset rpBug1Strs		; locate find2 & find3 code
	call	FindBadCode
	jc	rpBug2

; si = rpFind2 offset, di = rpFind3 offset

	push	di
	mov	di, si				; rpFind2 offset
	mov	dx, rpFind2Len

	cmp	byte ptr es:[di-1], 51h 	; find2 preceeded by push cx?
	jne	rp_no_cx

	dec	di				;   yes, gobble up push cx too
	inc	dx
rp_no_cx:
	mov	si, offset rpRepl2		; patch out find2 sequence
	mov	cx, rpRepl2Len
	call	GenPatch

	pop	di				; rpFind3 offset
	cmp	byte ptr es:[di-1], 59h 	; find3 preceeded by pop cx?
	jne	rp_no_cx2

	mov	byte ptr es:[di-1], 90h 	;   yes, no-op it

rp_no_cx2:
	mov	ax, [bp].sv_pPatch		; change offset of far jmp
	mov	es:[di+rpFind3Len], ax		;   to go to patch code

	push	di				; save find3 offset
	mov	si, offset rpRepl3		; copy repl3 to patch area
	mov	cx, rpRepl3Len
	call	CopyPatch

	pop	bx				; find3 offset
	add	bx, rpFind3Len+4		; skip over find3 and far jmp
	call	GenJump 			; jmp back from patch area
	mov	[bp].sv_pPatch, di		;   to main-line, update patch
						;   area pointer

;----------------------------------------------------------------------------
; Bug # 2 -- loss of high regs on 386+ under VCPI only

rpBug2:
	mov	bx, offset rpBug2Strs		; locate find4 & find5 code
	call	FindBadCode
	jc	rpBug3

; si = rpFind4 offset, di = rpFind5 offset

	push	[bp].sv_pPatch			; save current patch pointer
						;   (where repl4 goes)
	push	di				; save find5 offset

	mov	di, si
	mov	dx, rpFind4Len
	mov	si, offset rpRepl4
	mov	cx, rpRepl4Len
	call	GenPatch			; patch out find4 code

	pop	di				; find5 offset
	add	di, 5				; keep 5 bytes of find5 code
	mov	bx, [bp].sv_pPatch		; jump to patch area
	push	bx				; save repl5 location
	call	GenJump

	mov	si, offset rpRepl5		; copy repl5 code to patch
	mov	cx, rpRepl5Len			;   area -- it has a jmp bx
	call	CopyPatch			;   so no need to jmp back to
						;   main-line code

; patches have been made, now update the patch code to store/load dwords just
; after the code in the patch area

	pop	di				; repl5 location
	pop	si				; repl4 location

	mov	ax, [bp].sv_pPatch		; (where dwords go)

	mov	word ptr es:[si+rpRepl4o1Len], ax	; offset for EAX
	mov	word ptr es:[di+rpRepl5o1Len], ax
	add	ax, 4
	mov	word ptr es:[si+rpRepl4o2Len], ax	; offset for ESI
	mov	word ptr es:[di+rpRepl5o2Len], ax

	add	[bp].sv_pPatch, 8		; reserve space for 2 dwords in
						; patch area

;----------------------------------------------------------------------------
; Bug # 3 -- loss of high regs on 386+ under VCPI only

rpBug3:
	mov	bx, offset rpBug3Strs		; locate find6 & find7a code
	call	FindBadCode
	jc	rpBug4

	add	di, rpFind7aLen + 2		; skip over offset in find7a
	push	si				;   code and locate find7b
	mov	si, offset rpFind7b		;   sequence
	mov	dx, rpFind7bLen
	call	ScanCodeSeq_di
	pop	si
	jnz	rpBug4

	push	di				; save find7b code offset

	mov	di, si
	mov	dx, rpFind6Len
	mov	si, offset rpRepl6
	mov	cx, rpRepl6Len
	call	GenPatch			; patch out find6 code

	pop	di
	mov	dx, rpFind7bLen
	mov	si, offset rpRepl7
	mov	cx, rpRepl7Len
	call	GenPatch			; patch out find7b code


;----------------------------------------------------------------------------
; Bug # 4 -- loss of high regs on 386+ under VCPI only

rpBug4:
	cmp	[bp].sv_wVersion, 360	; only applies if version > 3.60
	jbe	rp3Exit 		;   and < 3.95

	mov	bx, offset rpBug4Strs		; locate find8 & find9 code
	call	FindBadCode
	jc	rp3Exit

	push	di				; save find9 code offset

	mov	di, si
	mov	dx, 3
	mov	si, offset rpRepl8
	mov	cx, rpRepl8Len
	call	GenPatch			; patch out find8 code

	pop	di				; find9 offset
	mov	bx, [bp].sv_pPatch		; patch find9 to jmp to
	call	GenJump 			;   patch area

	mov	si, offset rpRepl9		; copy replacement code to
	mov	cx, rpRepl9Len			;   patch area--it does a RET
	call	CopyPatch			;   so no jmp back to main-line

rp3Exit:
	add	sp, SIZE StackVars
	pop	bp
	pop	ds
	pop	es
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret

Rational386Patch endp

;----------------------------------------------------------------------------
;
; FindBadCode
;
; Searches Rational code segment looking for a pair of find strings (all
; patches have at least two find strings).
;
; Entry:
;	ES    = code segment to search
;	DS:BX = search pair structure for this search
;	[bp].sv_cbCodeSeg = length of code seg to search
;
; Exit:
;	CY flag clear if both strings found, and
;	SI    = offset in ES of 1st string
;	DI    = offset in ES of 2nd string
;	CY set if either string not found, or strings too far apart
;
; Used:
;	CX
;
;----------------------------------------------------------------------------

FindBadCode proc near

	mov	cx, [bp].sv_cbCodeSeg	; search length

	mov	si, [bx].sp_off1	; ds:si -> search string
	mov	dx, [bx].sp_len1	; dx = search len
	call	ScanCodeSeq
	jnz	fbc_error		; done if 1st not found

	push	di			; save 1st string offset

	mov	si, [bx].sp_off2
	mov	dx, [bx].sp_len2
	call	ScanCodeSeq_di		; don't change flags after this!

	pop	si			; restore 1st string offset
	jnz	fbc_error

	mov	ax, di			; sanity check that
	sub	ax, si			;   si < di && di - si <= allowed diff
	jc	fbc_error
	cmp	ax, [bx].sp_diff
	ja	fbc_error

	clc
	ret

fbc_error:
	stc
	ret

FindBadCode endp

;----------------------------------------------------------------------------
;
; GenPatch
;
; Generate a patch sequence.  1) insert a jump at the buggy code location
; (jumps to the patch code area), 2) copy the selected patch code to the
; patch area, 3) insert a jump from the patch area back to the main-line
; code.
;
; Entry:
;	ES:DI = start of buggy code to be patched
;	DX    = length of buggy code to be patched
;	DS:SI = replacement patch code
;	CX    = length of replacement patch code
;	[bp].sv_pPatch = offset in ES of where to copy patch code
;
; Exit:
;	DI, [bp].sv_pPatch = byte after generated patch code
;
; Used:
;	AX, BX, SI, Flags
;
;----------------------------------------------------------------------------

GenPatch proc	near

	push	di			;save offset of buggy code

	mov	bx, [bp].sv_pPatch	;jump from buggy code to patch area
	call	GenJump

	call	CopyPatch		;copy replacement code to patch area

	pop	bx			;offset of buggy code + buggy code
	add	bx, dx			;  length = return from patch offset

	call	GenJump 		;jump from patch area back to main-
	mov	[bp].sv_pPatch, di	;  line code, update patch pointer

	ret

GenPatch endp


;----------------------------------------------------------------------------
;
; CopyPatch
;
; Copies patch code to patch location.
;
; Entry:
;	DS:SI = patch code to be copied
;	ES    = segment of code to patch
;	CX    = length of code to copy
;	[bp].sv_pPatch = offset in ES of where to copy patch code
;
; Exit:
;	DI, [bp].sv_pPatch = byte after copied patch code
;
; Used:
;	SI, Flags
;
;----------------------------------------------------------------------------

CopyPatch proc	near

	push	cx
	mov	di, [bp].sv_pPatch	;patch pointer is the dest offset
	cld
	rep movsb
	pop	cx

	mov	[bp].sv_pPatch, di	;update net pointer location

	ret

CopyPatch endp

;----------------------------------------------------------------------------
;
; GenJump
;
; Generates a rel16 JMP instruction at location 'from' to location 'to'.
;
; Entry:
;	ES:DI = from location (where to put jmp instruction)
;	BX    = to location (where to jump to)
;
; Exit:
;	DI = byte after generated jump
;
; Used:
;	AX
;
;----------------------------------------------------------------------------

GenJump proc	near

	mov	al, 0E9h		; jmp rel16 opcode
	stosb

	mov	ax, bx			; calc offset to 'to' location
	sub	ax, di
	sub	ax, 2

	stosw				; output offset

	ret

GenJump endp

;
;----------------------------------------------------------------------------
;
; ScanCodeSeq
;
; Looks for a pattern pointed to by DS:SI & len DX in ES:200 to ES:200+CX-1
;
; returns in ES:DI the start of the pattern if Zero flag is set
;
;----------------------------------------------------------------------------
;
ScanCodeSeq	proc	near
		mov	di, 200h
ScanCodeSeq_di	label	near
		push	cx
		sub	cx, dx
		inc	cx
scsagain:
		push	si
		push	di
		push	cx
		mov	cx, dx
		rep	cmpsb
		pop	cx
		pop	di
		pop	si
		je	scsfound
		inc	di
		loop	scsagain
scsfound:
		pop	cx
		ret
ScanCodeSeq	endp
	
;
;----------------------------------------------------------------------------
;
; VerifyVersion
;
; Checks whether the binary version from ES:0 matches the ASCII version
; from ES:2A.
;
;       Entry: AX = binary version number 
;       Exit : Z flag set if version numbers match
;
;----------------------------------------------------------------------------
;
VerifyVersion	proc	near
		mov	si, es:[2ah]		; offset of version number
						;  in ascii
		mov	bl, 10
		add	si, 3			; point to last digit

		call	VVDigit
		jne	vvexit
		call	VVDigit
		jne	vvexit
		cmp	byte ptr es:[si], '.'
		jne	vvexit
		dec	si
		call	VVDigit
vvexit:
		ret
VerifyVersion	endp

VVDigit		proc	near
		div	bl
		add	ah, '0'
		dec	si
		cmp	es:[si+1], ah
		mov	ah, 0			; do not xor or sub we need Z
		ret
VVDigit 	endp

;-----------------------------------------------------------------------
public		exepatch_start
exepatch_start	label 	byte

	;
	; The following is the code that'll be layed over the buggy unpack
	; code.
	;
str1	label	byte

	db  06h	  		;push	es		 
	db  8Ch, 0D8h		;mov	ax,ds 

first_stop	equ	$ - str1
			
	db  2Bh, 0C2h		;sub	ax, dx			

first		label	byte

	db  8Eh, 0D8h		;mov	ds,ax			
	db  8Eh, 0C0h		;mov	es,ax			
	db  0BFh, 0Fh, 00h	;mov	di,000FH
	db  57h	    		;push	di
	db  0B9h, 10h, 00h	;mov	cx,0010H
	db  0B0h, 0FFh 		;mov	al,0FFH 		
	db  0F3h, 0AEh 		;repz	scasb			
	db  47h	    		;inc	di			
	db  8Bh, 0F7h  		;mov	si,di			
	db  5Fh	    		;pop	di
	db  58h	    		;pop	ax

second_stop	equ	$ - first

	db  2Bh, 0C2h  		;sub	ax, dx			

second		label	byte

	db  8Eh, 0C0h  		;mov	es,ax			
		    		;NextRec:				
	db  0B9h, 04h, 02h	;mov	cx, 0204h
		    		;norm_agn:				
	db  8Bh, 0C6h		;mov	ax,si			
	db  0F7h, 0D0h		;not	ax		
	db  0D3h, 0E8h		;shr	ax,cl		
	db  74h, 13h		;jz	SI_ok			
	db  8Ch, 0DAh		;mov	dx,ds			
	db  83h, 0CEh, 0F0h	;or	si,0FFF0H
	db  2Bh, 0D0h		;sub	dx,ax			
	db  73h, 08h		;jnc	SItoDS			
	db  0F7h, 0DAh		;neg	dx			
	db  0D3h, 0E2h		;shl	dx,cl			
	db  2Bh, 0F2h		;sub	si,dx			
	db  33h, 0D2h		;xor	dx,dx			
				;SItoDS: 				
	db  8Eh, 0DAh		;mov	ds,dx		
				;SI_ok:					
	db  87h, 0F7h		;xchg	si,di			
	db  1Eh			;push	ds			
	db  06h			;push	es			
	db  1Fh			;pop	ds			
	db  07h			;pop	es			
	db  0FEh, 0CDh		;dec	ch			
	db  75h, 0DBh		;jnz	norm_agn		
	db  0ACh		;lodsb			
	db  92h			;xchg	dx,ax
	db  4Eh			;dec	si
	db  0ADh		;lodsw			
	db  8Bh, 0C8h		;mov	cx,ax		
	db  46h			;inc	si		
	db  8Ah, 0C2h		;mov	al,dl		
	db  24h, 0FEh		;and	al,0FEH		
	db  3Ch, 0B0h		;cmp	al,RPTREC
	db  75h, 05h		;jne	TryEnum
	db  0ACh		;lodsb				
	db  0F3h, 0AAh		;rep stosb			

;	db  0EBh, 07h, 90h	;jmp	TryNext
	db  0EBh, 06h		;jmp	TryNext

				;TryEnum:
	db  3Ch, 0B2h		;cmp	al,ENMREC
	db  75h, 6Ch		;jne	CorruptExe		
	db  0F3h, 0A4h		;rep movsb			
				;TryNext:

	db  92h			;xchg	dx,ax
;	db  8Ah, 0C2h		;mov	al,dl			

	db  0A8h, 01h		;test	al,1			
	db  74h, 0B9h		;jz	NextRec			
	db  90h, 90h		;nop, nop
	

last_stop	equ	$ - second
size_str1	equ	$-str1


	;
	; The following is the code that we need to look for in the exe
	; file.
	;
scan_patch1	label	byte

	db  8Ch, 0C3h		;mov	bx,es			
	db  8Ch, 0D8h		;mov	ax,ds
	db  2Bh, 0C2h		;sub	ax, dx
	db  8Eh, 0D8h		;mov	ds,ax			
	db  8Eh, 0C0h		;mov	es,ax			
	db  0BFh, 0Fh,00h	;mov	di,000FH
	db  0B9h, 10h, 00h	;mov	cx,0010H
	db  0B0h, 0FFh		;mov	al,0FFH
	db  0F3h, 0AEh		;repz	scasb			
	db  47h			;inc	di			
	db  8Bh, 0F7h		;mov	si,di
	db  8Bh, 0C3h		;mov	ax,bx			
	db  2Bh, 0C2h		;sub	ax, dx
	db  8Eh, 0C0h		;mov	es,ax
	db  0BFh, 0fh,00h	;mov	di,000FH
				;NextRec:
	db  0B1h, 04h		;mov	cl,4
	db  8Bh, 0C6h		;mov	ax,si
	db  0F7h, 0D0h		;not	ax		
	db  0D3h, 0E8h		;shr	ax,cl		
	db  74h, 09h		;jz	SI_ok
	db  8Ch, 0DAh		;mov	dx,ds
	db  2Bh, 0D0h		;sub	dx,ax
	db  8Eh, 0DAh		;mov	ds,dx		
	db  83h, 0CEh, 0F0h	;or	si,0FFF0H	       
	       			;SI_ok:
	db  8Bh, 0C7h		;mov	ax,di		
	db  0F7h, 0D0h		;not	ax
	db  0D3h, 0E8h		;shr	ax,cl
	db  74h, 09h		;jz	DI_ok
	db  8Ch, 0C2h		;mov	dx,es
	db  2Bh, 0D0h		;sub	dx,ax
	db  8Eh, 0C2h		;mov	es,dx
	db  83h, 0CFh, 0F0h	;or	di,0FFF0H
				;DI_ok:

size_scan_patch1	equ	$-scan_patch1


scan_patch2	label	byte

			
	db  8Ch, 0C3h		;mov	bx,es			
	db  8Ch, 0D8h		;mov	ax,ds
	db  48h			;dec	ax
	db  8Eh, 0D8h		;mov	ds,ax			
	db  8Eh, 0C0h		;mov	es,ax			
	db  0BFh, 0Fh, 00h	;mov	di,000FH
	db  0B9h, 10h, 00h	;mov	cx,0010H
	db  0B0h, 0FFh		;mov	al,0FFH
	db  0F3h, 0AEh		;repz	scasb			
	db  47h			;inc	di			
	db  8Bh, 0F7h		;mov	si,di
	db  8Bh, 0C3h		;mov	ax,bx			
	db  48h			;dec	ax
	db  8Eh, 0C0h		;mov	es,ax
	db  0BFh, 0Fh, 00h	;mov	di,000FH		
				;NextRec:
	db  0B1h, 04h		;mov	cl,4
	db  8Bh, 0C6h		;mov	ax,si
	db  0F7h, 0D0h		;not	ax		
	db  0D3h, 0E8h		;shr	ax,cl		
	db  74h, 0Ah		;jz	SI_ok
	db  8Ch, 0DAh		;mov	dx,ds
	db  2Bh, 0D0h		;sub	dx,ax
	db  8Eh, 0DAh		;mov	ds,dx		
	db  81h, 0CEh, 0F0h, 0FFh
				;or	si,0FFF0H
				;SI_ok:
	db  8Bh, 0C7h		;mov	ax,di		
	db  0F7h, 0D0h		;not	ax
	db  0D3h, 0E8h		;shr	ax,cl
	db  74h, 0Ah		;jz	DI_ok
	db  8Ch, 0C2h		;mov	dx,es
	db  2Bh, 0D0h		;sub	dx,ax
	db  8Eh, 0C2h		;mov	es,dx
	db  81h, 0CFh, 0F0h, 0FFh
				;or	di,0FFF0H
				;DI_ok:


size_scan_patch2	equ	$-scan_patch2


scan_patch3	label	byte

			
	db  8Ch, 0C3h		;mov	bx,es			
	db  8Ch, 0D8h		;mov	ax,ds
	db  48h			;dec	ax
	db  8Eh, 0D8h		;mov	ds,ax			
	db  8Eh, 0C0h		;mov	es,ax			
	db  0BFh, 0Fh, 00h	;mov	di,000FH
	db  0B9h, 10h, 00h	;mov	cx,0010H
	db  0B0h, 0FFh		;mov	al,0FFH
	db  0F3h, 0AEh		;repz	scasb			
	db  47h			;inc	di			
	db  8Bh, 0F7h		;mov	si,di
	db  8Bh, 0C3h		;mov	ax,bx			
	db  48h			;dec	ax
	db  8Eh, 0C0h		;mov	es,ax
	db  0BFh, 0Fh, 00h	;mov	di,000FH		
				;NextRec:
	db  0B1h, 04h		;mov	cl,4
	db  8Bh, 0C6h		;mov	ax,si
	db  0F7h, 0D0h		;not	ax		
	db  0D3h, 0E8h		;shr	ax,cl		
	db  74h, 09h		;jz	SI_ok
	db  8Ch, 0DAh		;mov	dx,ds
	db  2Bh, 0D0h		;sub	dx,ax
	db  8Eh, 0DAh		;mov	ds,dx	
	db  83h, 0CEh, 0F0h	;or	si,0FFF0H	
				;SI_ok:
	db  8Bh, 0C7h		;mov	ax,di		
	db  0F7h, 0D0h		;not	ax
	db  0D3h, 0E8h		;shr	ax,cl
	db  74h, 09h		;jz	DI_ok
	db  8Ch, 0C2h		;mov	dx,es
	db  2Bh, 0D0h		;sub	dx,ax
	db  8Eh, 0C2h		;mov	es,dx
	db  83h, 0CFh, 0F0h	;or	di,0FFF0H
				;DI_ok:


size_scan_patch3	equ	$-scan_patch3


scan_com	label	byte

	db  0ACh		;lodsb			
	db  8Ah, 0D0h		;mov	dl,al		
	db  4Eh			;dec	si
	db  0ADh		;lodsw			
	db  8Bh, 0C8h		;mov	cx,ax		
	db  46h			;inc	si		
	db  8Ah, 0C2h		;mov	al,dl		
	db  24h, 0FEh		;and	al,0FEH		
	db  3Ch, 0B0h		;cmp	al,RPTREC
	db  75h, 06h		;jne	TryEnum
	db  0ACh		;lodsb				
	db  0F3h, 0AAh		;rep stosb			
	db  0EBh, 07h, 90h	;jmp	TryNext
				;TryEnum:
	db  3Ch, 0B2h		;cmp	al,ENMREC
	db  75h, 6Bh		;jne	CorruptExe		
	db  0F3h, 0A4h		;rep movsb			
				;TryNext:
	db  8Ah, 0C2h		;mov	al,dl			
	db  0A8h, 01h		;test	al,1			
;	db  74h, 0BAh		;jz	NextRec			

size_scan_com	equ	$-scan_com

ExePatch	proc	near
		call	ExePackPatch
		call	word ptr RationalPatchPtr
		ret
ExePatch	endp



;---------------------------------------------------------------------------
;
; Procedure Name 	: ExePackPatch
;
; Inputs	 	: DS 			-> DOSDATA
;			  ES:0 			-> read in image
;			  ax:cx = start cs:ip of program
; Output		:		
;
;	1. If ES <= 0fffh
;	   2. if exepack signature ('RB') found
;	      3. if common code to patch compares (for 3 diff. versions)
;	       	 4. if rest of the code & checksum compares
;	  	    5. overlay buggy code with code in 
;		       doscode:str1.
;		 6. endif
;	      7. endif
;	   8. endif
;	9. endif
;
;
; Uses			: NONE
;
;---------------------------------------------------------------------------
		

ExePackPatch	proc	near

	push	bx
	mov	bx, es			; bx has load segment
	cmp	bx, 0fffh		; Q: is the load segment > 64K
	jbe	ep_cont			; N: 
	pop	bx			; Y: no need to patch
	ret

ep_cont:
	push	ds
	push	es
	push	ax
	push	cx
	push	si
	push	di
	
		; M033 - start
		; exepacked prgrams have an IP of 12h (>=2)

	sub	cx, 2			; Q: is IP >=2 
	ljc	ep_notpacked		; N: exit
					; ax:cx now points to location of
					; 'RB' if this is an exepacked file.

		; M033 - end

	mov	di, cx
	mov	es, ax
	mov	[unpack_offset], di	; save pointer to 'RB' in 
					; unpack_offset


	cmp	word ptr es:[di], 'BR'
	ljne	ep_notpacked

	push	cs
	pop	ds			; set ds to cs
	assume	ds:nothing

	add	di, PATCH1_COM_OFFSET	; es:di -> points to place in packed 
					;          file where we hope to find
					;	   scan string. 

	call	chk_common_str		; check for match

	jnz	ep_chkpatch2		; Q: does the patch match
					; N: check at patch2_offset
					; Y: check for rest of patch string

	mov	si, offset DOSCODE:scan_patch1
					; ds:si -> scan string 
	mov	di, [unpack_offset]	; restore di to point to 'RB'

	add	di, PATCH1_OFFSET	; es:di -> points to place in packed 
					;          file where we hope to find
					;	   scan string. 
	mov	cx, size_scan_patch1
	mov	bx, CHKSUM1_LEN
	mov	ax, PATCH1_CHKSUM
	call	chk_patchsum		; check if patch and chk sum compare
	jc	ep_done1		; Q: did we pass the test
					; N: exit
					; Y: overlay code with new 
	
	mov	si, offset DOSCODE:str1
	mov	cx, size_str1
	
rep	movsb


ep_done1:
	jmp	ep_done


ep_chkpatch2:
	mov	di, PATCH2_COM_OFFSET	; es:di -> possible loaction of patch
					; in another version of unpack
	call	chk_common_str		; check for match

	jnz	ep_chkpatch3		; Q: does the patch match
					; N: check for patch3_offset
					; Y: check for rest of patch string

	mov	si, offset DOSCODE:scan_patch2
					; ds:si -> scan string 

	mov	di, PATCH2_OFFSET	; es:di -> points to place in packed 
					;          file where we hope to find
					;	   scan string. 
	mov	cx, size_scan_patch2
	mov	bx, CHKSUM2_LEN
	mov	ax, PATCH2_CHKSUM
	call	chk_patchsum		; check if patch and chk sum compare

					; M046 - Start
					; Q: did we pass the test
	jnc	ep_patchcode2		; Y: overlay code with new 
					; N: try with a different chksum


	mov	si, offset DOSCODE:scan_patch2
					; ds:si -> scan string 
	mov	cx, size_scan_patch2
	mov	bx, CHKSUM2A_LEN
	mov	ax, PATCH2A_CHKSUM
	call	chk_patchsum		; check if patch and chk sum compare
					; Q: did we pass the test
	jc	ep_notpacked		; N: try with a different chksum
					; Y: overlay code with new 
						
ep_patchcode2:			       	; M046 - End
	mov	si, offset DOSCODE:str1
	mov	cx, first_stop
	
rep	movsb

	mov	ax, 4890h		; ax = opcodes for dec ax, nop
	stosw
	add	si, 2

	mov	cx, second_stop

rep	movsb

	stosw				; put in dec ax and nop
	add	si, 2

	mov	cx, last_stop

rep	movsb

	jmp	short ep_done

ep_chkpatch3:

	mov	di, PATCH3_COM_OFFSET	; es:di -> possible loaction of patch
					; in another version of unpack
	call	chk_common_str		; check for match

	jnz	ep_notpacked		; Q: does the patch match
					; N: exit
					; Y: check for rest of patch string


	mov	si, offset DOSCODE:scan_patch3
					; ds:si -> scan string 

	mov	di, PATCH3_OFFSET	; es:di -> points to place in packed 
					;          file where we hope to find
					;	   scan string. 
	mov	cx, size_scan_patch3
	mov	bx, CHKSUM3_LEN
	mov	ax, PATCH3_CHKSUM
	call	chk_patchsum		; check if patch and chk sum compare
	jc	ep_notpacked		; Q: did we pass the test
					; N: exit
					; Y: overlay code with new 

	mov	si, offset DOSCODE:str1
	mov	cx, first_stop
	
rep	movsb

	mov	al, 48h			; al = opcode for dec ax
	stosb
	add	si, 2

	mov	cx, second_stop

rep	movsb

	stosb				; put in dec ax
	add	si, 2

	mov	cx, last_stop

rep	movsb

	

ep_notpacked:

;	stc
;

ep_done:

	pop	di
	pop	si
	pop	cx
	pop	ax
	pop	es
	pop	ds
	pop	bx
	ret

;-------------------------------------------------------------------------
;
; 	Procedure Name	: chk_common_str
;
;	Input		: DS = DOSCODE
;			; ES:DI points to string in packed file
;
;	Output		; Z if match else NZ
;
;-----------------------------------------------------------------------

chk_common_str	proc	near

	mov	si, offset DOSCODE:scan_com
					; ds:si -> scan string 
	mov	cx, size_scan_com

repe	cmpsb	       

					; M046 - start
	; a fourth possible version of these exepacked programs have a 
	; 056h instead of 06bh. See scan_com above
	;
	; 	db  75h, 6Bh		;jne	CorruptExe		
	;
	; If the mismatch at this point is due to a 56h instead of 6bh 
	; we shall try to match the rest of the string
	;

	jz	ccs_done
	cmp	byte ptr es:[di-1], 56h
	jnz	ccs_done

repe	cmpsb	    
  
ccs_done:				; M046 - end
	ret

chk_common_str	endp		


;---------------------------------------------------------------------------
;
;	Procedure Name	: chk_patchsum
;
;	Input		: DS:SI -> string we're looking for
;			: ES:DI -> offset in packed file
;			: CX 	= scan length
;			: BX	= length of check sum
;			: AX 	= value of check sum
;
;	Output		: if patch & check sum compare
;				NC
;			  else
;				CY
;
;	Uses		: AX, BX, CX, SI
;----------------------------------------------------------------------------

chk_patchsum	proc	near

	push	di

repe	cmpsb			   

	jnz	cp_fail			; Q: does the patch match
					; N: exit
					; Y:	


		; we do a check sum starting from the location of the 
		; exepack signature 'RB' up to 11c/2 bytes, the end of the
		; unpacking code.


	mov	di, [unpack_offset]	; di -> start of unpack code
	mov	cx, bx			; cx = length of check sum

	mov	bx, ax			; save check sum passed to us in bx
	xor	ax, ax

ep_chksum:
	add	ax, word ptr es:[di]
	add	di, 2
	loop	ep_chksum

	pop	di			; restore di

	cmp	ax, bx		 	; Q: does the check sum match
	jnz	cp_fail			; N: exit
					; Y: 

	clc	
	ret

cp_fail:
	stc
	ret

chk_patchsum	endp


ExePackPatch	endp
;
; M020 : BEGIN
;
;----------------------------------------------------------------------------
;
; procedure : RationalPatch
;
; A routine (in Ration DOS extender) which is invoked at hardware interrupts
; clobbers CX registeron 286 machines. (123 release 3 uses Rational DOS
; extender). This routine identfies Buggy Rational EXEs and fixes the bug.
;
; THE BUG is in the following code sequence:
;
;8b 0e 10 00	mov	cx, ds:[10h]		; delay count
;90		even				; word align
;e2 fe		loop	$			; wait		CLOBBERS CX
;e8 xx xx	call	set_A20			; enable A20
;
; This patch routine replaces the mov & the loop with a far call into a
; routine in DOS data segment which is in low memory (because A20 line
; is off). The routine (RatBugCode) in DOS data saves & restores CX around
; a mov & loop.
;
; Identification of Buggy Rational EXE
; ====================================
;
; (ALL OFFSETS ARE IN THE PROGRAM SECTION - EXCLUDING THE EXE HEADER)
;
; OFFSET				Contains
; ------				--------
; 0000h			100 times Version number in binary
;			bug exists in version 3.48 thru 3.83 (both inclusive)
;
; 000ah			the WORDS : 0000h, 0020h, 0000h, 0040h, 0001h
;
; 002ah			offset where version number is stored in ASCII
;				e.g. '3.48A'
;
; 0030h			offset of copyright string. Copyright strings either
;			start with "DOS/16M Copyright...." or
;			"Copyright.....". The string contains
;			"Rational Systems, Inc."
;
; 0020h			word : Paragraph offset of the buggy code segment
;				from the program image
; 0016h			word : size of buggy code segment
;
;	Buggy code is definite to start after offset 200h in its segment
;
;----------------------------------------------------------------------------
;
RScanPattern1	db	0, 0, 20h, 0, 0, 0, 40h, 0, 1, 0
RLen1		equ	$ - offset RScanPattern1

RScanPattern2	db	8bh, 0eh, 10h, 00h, 90h, 0e2h, 0feh, 0e8h
RLen2		equ	$ - offset RScanPattern2

RScanPattern3	db	8bh, 0eh, 10h, 00h, 0e2h, 0feh, 0e8h
RLen3		equ	$ - offset RScanPattern2
;
;----------------------------------------------------------------------------
;
; INPUT : ES = segment where program got loaded
;
;----------------------------------------------------------------------------

RationalPatch	proc	near
		cld
		push	ax
		push	bx
		push	cx
		push	dx
		push	si
		push	di
		push	es
		push	ds			; we use all of them
		mov	di, 0ah			; look for pat1 at offset 0a
		push	cs
		pop	ds
		assume	ds:nothing
		mov	si, offset RScanPattern1
		mov	cx, RLen1
		rep	cmpsb			; do we have the pattern ?
		jne	rpexit
		mov	ax, es:[0]
		cmp	ax, 348			; is it a buggy version ?
		jb	rpexit
		cmp	ax, 383			; is it a buggy version
		ja	rpexit

		call	VerifyVersion
		jne	rpexit

		mov	cx, es:[16h]		; Length of buggy code seg
		sub	cx, 200h		; Length we search (we start
						;  at offset 200h)
		mov	es, word ptr es:[20h]	; es=buggy code segment
		mov	si, offset RScanPattern2
		mov	dx, RLen2
		call	ScanCodeSeq		; look for code seq with nop
		jz	rpfound

		mov	si, offset RScanPattern3
		mov	dx, RLen3
		call	ScanCodeSeq		; look for code seq w/o nop
		jnz	rpexit
rpfound:

;		we set up a far call into DOS data
;		dx has the length of the doce seq we were searching for

		mov	al, 9ah			; far call opcode
		stosb
		mov	ax, offset RatBugCode
		stosw
		mov	ax, ss
		stosw
		mov	cx, dx
		sub	cx, 6			; filler (with NOPs)
		mov	al, 90h
		rep	stosb
rpexit:
		pop	ds
		pop	es
		pop	di
		pop	si
		pop	dx
		pop	cx
		pop	bx
		pop	ax
		ret
RationalPatch	endp
;
; M020 END
;

;---------------------------------------------------------------------------
;
;	M068
;
; 	Procedure Name	: IsCopyProt
;
;	Inputs		: DS:100 -> start of com file just read in
;
;	Outputs		: sets the A20OFF_COUNT variable to 10 if 
;			  the program loaded in DS:100 uses a MICROSOFT
;			  copy protect scheme that relies on the A20 line
;			  being turned off for it's scheme to work.
;
;			  Note: The int 21 function dispatcher will turn 
;				a20 off, if the A20OFF_COUNT is non-zero 
;				and dec the A20OFF_COUNT before	iretting 
;				to the user. 
;
;	Uses		: ES, DI, SI, CX
;
;---------------------------------------------------------------------------

CPStartOffset	EQU	0175h
CPID1Offset	EQU	011bh
CPID2Offset	EQU	0173h
CPID3Offset	EQU	0146h
CPID4Offset	EQU	0124h
ID1		EQU	05343h
ID2		EQU	05044h
ID3		EQU	0F413h
ID4		EQU	08000h

CPScanPattern	db	089h, 026h, 048h, 01h		; mov [148], sp
		db	08ch, 0eh , 04ch, 01h		; mov [14c], cs
		db	0c7h, 06h , 04ah, 01h, 0h, 01h	; mov [14a], 100h 
		db 	08ch, 0eh , 013h, 01h		; mov [113], cs
		db	0b8h, 020h, 01h			; mov ax, 120h
		db	0beh, 00h , 01h			; mov si, 100h

CPSPlen		EQU	$ - CPScanPattern

IsCopyProt	proc	near

	cmp	ds:[CPID1Offset], ID1
	jne	CP_done

	cmp	ds:[CPID2Offset], ID2
	jne	CP_done

	cmp	ds:[CPID3Offset], ID3
	jne	CP_done

	cmp	ds:[CPID4Offset], ID4
	jne	CP_done

	push	cs
	pop	es
	mov	di, offset CPScanPattern	; es:di -> Pattern to find

	mov	si, CPStartOffset		; ds:si -> possible location 
						; of pattern

	mov	cx, CPSPlen			; cx = length of pattern

repe	cmpsb

	jnz	CP_done
	mov	ss:[A20OFF_COUNT], 0AH		; M071
CP_done:
	ret
	
IsCopyProt	endp

		
DOSCODE	ENDS

	END


