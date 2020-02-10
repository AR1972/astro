	page	,132
	title	Calculate Compression Ratio Routines
;/*
; *                      Microsoft Confidential
; *			 Copyright (C) Microsoft Corporation 1992
; *                      All Rights Reserved.
; */


;***	SYMBOLS & MACROS

	.xlist
	.xcref

	include version.inc

ifdef dblspace_hooks
	include dossym.inc	; get DOS basic symbol set
	include syscall.inc	; get DOS call names
	include	comseg.asm	; define segment order
	include comequ.asm	; get equates for COMMAND routines
	include magicdrv.inc	; for MagivDrv (compression ratio) support

	.list
	.cref

Get_Drive_Data	equ	28		; Int 21h/1Ch call

;***	DATA

TRANDATA segment public byte

	extrn	sCVFRoot:byte		; root name of CVF files
	extrn	cbCVFRoot:abs		; size of CVF root name string

TRANDATA ends


TRANSPACE segment public byte

	extrn	fhCVF:word		; Compressed Volume File file handle
	extrn	szCVF:byte		; Compressed Volume File file name
	extrn	MDBPB:byte		; Extended MagicDrv BPB
	extrn	csecUsed:dword		; count of compressed sectors used-file
	extrn	csecUsedDir:dword	; count of compressed sectors used-dir
	extrn	csecUsedTotal:dword	; count of compressed sectors used-total
	extrn	ccluUsed:word		; count of DOS clusters used - file
	extrn	ccluUsedDir:word	; count of DOS clusters used - dir
	extrn	ccluUsedTotal:word	; count of DOS clusters used - total

	extrn	Tpa:word		; TPA buffer seg addr
	extrn	CurDrv:byte		; current drive # (0-based)
	extrn	BytCnt:word		; # bytes in TPA
	extrn	savBytCnt:word		; copy of BytCnt when FAT buffers in TPA

	extrn	fUseHostSize:byte	; use host cluster size with /C
	extrn	csecPerCluster:byte	; sectors/cluster for ratio calculation

; The code to calculate compression ratios requires access to the drive's
; (DOS) FAT and MagicDrv FAT regions.  Two buffers are used (one for each
; FAT type).  pbufDOSFAT and pbufMDFAT contain the offset to the buffers,
; segFATBuf contains the segment (both buffers are in the same segment).
; The buffers are of variable size: cFATEntries contains the size of the
; buffers in terms of the number of FAT entries they can contain.

	extrn	cFATEntries:word	; size of FAT buffers in entries
	extrn	entInBuf:word		; 1st FAT entry in buffers
	extrn	segFATBuf:word		; DOS & MD FAT buffer segment
	extrn	pbufDOSFAT:word 	; pointer to DOS FAT buffer
	extrn	pbufMDFAT:word		; pointer to MagicDrv FAT buffer
	extrn	bufDOSFAT:byte		; small resident DOS FAT buffer
	extrn	bufMDFAT:byte		; small resident MD FAT buffer

TRANSPACE ends


TRANCODE segment public byte

	assume	cs:TRANGROUP,ds:TRANGROUP,es:nothing,ss:TRANGROUP


;***	OpenCVF - open Compressed Volume File for compression ratio report
;
;	ENTRY
;		FCB setup with drive for DIR
;
;	EXIT	If successful, CY clear, CVF file open, fhCVF has file handle,
;		szCVF has \0 terminated CVF file name, MDBPB loaded.
;
;		If unsuccessful, CY set
;
;	USED	AX, BX, CX, DX, SI, DI

	public	OpenCVF

OpenCVF proc

	mov	ax, -1
	mov	fhCVF, ax		;indicate CVF not open
	mov	entInBuf, ax		;  and no FAT entries in buffers

	mov	dl, ds:[FCB]		;target drive of DIR cmd
	or	dl, dl			;default drive?
	jz	ocvf_default
	dec	dl			;no, from 1=A to 0=A
	jmp	short ocvf_swap_info

ocvf_default:
	mov	dl, ds:[CurDrv] 	;0=A, 1=B, ...

ocvf_swap_info:

	mov	ax, multMagicDrv	;magicdrv Int 2Fh multiplex ID
	mov	bx, MD_DRIVE_MAP	;get drive swap info
	int	2fh

	or	ax, ax			;0 if okay
	jnz	ocvf_error

	test	bl, 80h 		;80h set if compressed volume
	jz	ocvf_error

	and	bl, 7Fh 		;bl = host drive, bh = seq #

	; The CVF may have been mounted from a swapped host drive, in which
	; case the host drive returned in BL is the original host (now
	; swapped with a CVF).	Make a second drive swap info call on the
	; returned host to see if it must be accessed by a different drive
	; letter.

	push	bx
	push	dx			;save dl, orig drive letter

	mov	ax, multMagicDrv
	mov	dl, bl
	mov	bx, MD_DRIVE_MAP
	int	2fh

	pop	dx
	pop	cx			;bx from 1st drive swap info call

	or	ax, ax			;0 if okay
	jnz	ocvf_error

	and	bl, 7Fh
	xchg	bx, cx			;bx = 1st, cx = 2nd swap results

	cmp	dl, cl			;2nd swap info call return orig drive?
	jz	ocvf_got_host		;yes, 1st swap info call returned host

	mov	bl, cl			;no, use swapped host, orig seq #

ocvf_got_host:

	; Build the filename of the Compressed Volume File

	mov	di, offset TRANGROUP:szCVF	;CVF name buffer

	mov	al, bl
	add	al, 'A'
	mov	ah, ':'
	cld
	stosw					; drive:

	mov	si, offset TRANGROUP:sCVFRoot	; \name.
	mov	cx, cbCVFRoot
	rep movsb

	add	di, 3				; point past extension
	xor	al, al
	std
	stosb					; null terminate

	mov	al, bh				; seq #
	mov	bl, 10
	mov	cx, 3				; 3 digit ext/seq #
@@:
	xor	ah, ah				; convert seq # to ascii
	div	bl				;   and store as CVF extension
	add	ah, '0'
	xchg	ah, al
	stosb
	mov	al, ah
	loop	@b

	cld

	; Now open the Compressed Volume File

	mov	ax, (OPEN shl 8) or 00h 	;compatibility mode/read access
	mov	dx, offset TRANGROUP:szCVF
	int	21h
	jc	ocvf_error

	mov	fhCVF, ax			; success, save CVF file handle

	; Read the extended MagicDrv BPB

	mov	bx, ax
	mov	ah, READ
	mov	cx, size MD_BPB
	mov	dx, offset TRANGROUP:MDBPB
	int	21h
	jc	ocvf_error1

	cmp	ax, cx			; get it all?
	je	ocvf_pick_cluster_size	; yes...

ocvf_error1:
	call	CloseCVF

ocvf_error:
	stc				;indicate failure
	jmp	short ocvf_ret


	; Determine the cluster size to use for ratio calculation

ocvf_pick_cluster_size:

	cmp	fUseHostSize, 0 	; user want Host drive cluster size?
	je	ovcf_use_CVF_size	; no, use CVF cluster size

	mov	ah, Get_Drive_Data	; get the host drive cluster size
	mov	dl, szCVF
	sub	dl, 'A' - 1		; 1 = A, 2 = B, ...
	push	ds
	int	21h
	pop	ds

	cmp	al, 0FFh		; host drive cluster size in AL if okay,
	jne	ovcf_set_size		;   failed = 0FFh

ovcf_use_CVF_size:
	mov	al, MDBPB.dos_bpb.csecPerClu	; using CVF cluster size

ovcf_set_size:
	mov	csecPerCluster, al

	; Lastly, setup the FAT buffers

ocvf_set_buf:
	mov	ax, [BytCnt]		; if >= 32k TPA space available,
	mov	[savBytCnt], ax 	;   setup larger FAT buffers
	cmp	ax, 32*1024
	jae	ocvf_big_buf

	; small TPA, use small resident buffers

	mov	[cFATEntries], cRES_FAT_ENTRIES
	mov	[segFATBuf], ds
	mov	word ptr [pbufDOSFAT], offset TRANGROUP:bufDOSFAT
	mov	word ptr [pbufMDFAT],  offset TRANGROUP:bufMDFAT
	jmp	short ocvf_success

ocvf_big_buf:

	mov	bx, cBIG_FAT_ENTRIES
	mov	[cFATEntries], bx

	shl	bx, 1		; 6 bytes per entry (2 for DOS FAT, 4 MD FAT)
	mov	cx, bx		; entries * 2
	shl	bx, 1
	add	bx, cx		; bx = # entries * 6

	sub	ax, bx		; reduce TPA size by size of FAT buffers
	and	ax, 0FE00h	; init code rounds BytCnt down to multiple of
	mov	[BytCnt], ax	;   512 bytes -- a no-op with some buf sizes.

	mov	bx, [Tpa]		; buffers in the TPA
	mov	[segFATBuf], bx
	mov	[pbufDOSFAT], ax	; DOS FAT buffer offset
	add	ax, cx			;   + DOS FAT buffer size
	mov	[pbufMDFAT], ax 	;   = MD FAT buffer offset

ocvf_success:
	clc			;indicate success

ocvf_ret:
	ret

OpenCVF endp



;***	CloseCVF - close Compressed Volume File
;
;	ENTRY	fhCVF has file handle
;
;	EXIT
;
;	USED	AX, BX, CX, DX

	public	CloseCVF

CloseCVF	proc

	mov	bx, fhCVF		; -1 unless file is open
	cmp	bx, -1
	je	ccvf_ret

	mov	ah, CLOSE
	int	21h

	mov	fhCVF, -1		; don't try to close again

	mov	ax, [savBytCnt] 	; 'deallocate' DOS & MD FAT buffers
	mov	[BytCnt], ax		;   by restoring old TPA byte count

ccvf_ret:
	ret

CloseCVF	endp




;***	CalcCompRatio - calculate file compression ratio
;
;	ENTRY	AX = starting cluster of file to get compression ratio of
;
;	EXIT	AX = compression ratio.  Example: a ratio of 2.7 to 1.0
;		     will return AH = 02h & AL = 07h
;		ccluUsed set to # DOS clusters used by file
;		csecUsed set to # compressed sectors used by file
;		ccluUsedDir, ccluUsedTotal, csecUsedDir, csecUsedTotal updated
;	USED	none

	public	CalcCompRatio

CalcCompRatio	proc

	push	bx
	push	cx
	push	dx
	push	es
	mov	es, [segFATBuf] 	; es is pointer to FAT buffers
	assume	es:nothing

	xor	bx, bx			; zero count of sectors & clusters used
	mov	[ccluUsed], bx
	mov	word ptr [csecUsed], bx
	mov	word ptr [csecUsed+2], bx

ccr_next:
	cmp	ax, 2			; sanity check the DOS FAT value
	jb	ccr_screwy

	cmp	ax, 0FFF0h		; end of file?
	jae	ccr_eof

	call	CheckFATBuffers 	; make sure buffers contain target
	jc	ccr_screwy		;   FAT entries

	call	GetMDFATEntry		; returns corresponding entry in BX:CX
	jc	ccr_screwy

	shl	bx, 1			; used bit to CY
	jnc	ccr_screwy		; better be used!

	mov	ch, bh			; save uncompressed count

	shl	bx, 1			; get count into position
	and	bx, 0F00h		; bh = count of compressed sectors used
	xchg	bh, bl			; bx = count
	inc	bx			; 0 - 15 means 1 - 16 used

	add	word ptr [csecUsed], bx
	adc	word ptr [csecUsed+2], 0

	mov	dx, ax			; save cluster # in dx

	mov	al, ch			; uncompressed count to al
	mov	cl, 3
	shr	al, cl			; get uncompressed count into position
	and	ax, 000Fh		; ax = uncompressed count (0 - 15)
	dec	bx			; bx = compressed count (0 - 15)
	cmp	ax, bx			; if the compressed cnt > uncompressed
	jae	@f			;   fudge a little and use the larger
	mov	ax, bx
@@:	mov	cl, csecPerCluster	; round up to the number of clusters
	xor	ch, ch			;   required for uncompressed
	add	ax, cx			;   sectors
	div	cl
	xor	ah, ah
	add	[ccluUsed], ax

	mov	ax, dx			; restore cluster #
	call	GetDOSFATEntry		; retuns next DOS FAT entry in AX
	jc	ccr_screwy
	jmp	short ccr_next

	; Reached the end-of-file, now calculate the ratio as the
	; number of DOS sectors used / number of compressed sectors used.

ccr_eof:
	mov	ax, [ccluUsed]
	add	[ccluUsedDir], ax	; update cluster used totals
	add	[ccluUsedTotal], ax

	mov	cx, word ptr [csecUsed+2]
	mov	bx, word ptr [csecUsed] ; cx:bx = # compressed sectors used

	add	word ptr [csecUsedDir], bx	; update sector used totals
	adc	word ptr [csecUsedDir+2], cx
	add	word ptr [csecUsedTotal], bx
	adc	word ptr [csecUsedTotal+2], cx

	call	ComputeRatio		; ax=clusters used, cx:bx=sectors used

	jmp	short ccr_ret

ccr_screwy:
	xor	ax, ax			; something screwy happened, set
					;   ratio to 0.0 and exit
ccr_ret:
	pop	es
	pop	dx
	pop	cx
	pop	bx
	ret

CalcCompRatio	endp



;***	ComputeRatio - calculate ratio of compressed sectors used to
;		       (would be) DOS sectors used
;
;	Entry
;		AX = DOS clusters used, cx:bx = compressed sectors used
;	Exit
;		ah = whole portion, al = tenths
;
;	Used	BX, CX, DX
;
	public	ComputeRatio

ComputeRatio	proc

	push	si
	push	di

	mov	si, bx
	mov	di, cx			; save cx:bx in di:si

	mov	bl, csecPerCluster
	xor	bh, bh
	mul	bx			; dx:ax = # DOS sectors used
	mov	bx, si			; restore bx

	call	Div32			; dx:ax = quotient, cx:bx = remainder

	push	ax			; save quotient

	mov	ax, bx			; if no remainder, tenths will be 0
	or	ax, cx			;   which is in AX so skip following
	jz	cr_got_tenths		;   (happens frequently)

	; Multiply the reminder by 10, add half the divisor so result is
	; rounded up, and divide again to get tenths digit

	mov	ax, cx
	xor	dx, dx
	mov	cx, bx
	mov	bx, 10
	mul	bx
	xchg	ax, cx
	mul	bx
	add	dx, cx			; dx:ax = remainder * 10

	mov	cx, di
	mov	bx, si
	shr	cx, 1
	rcr	bx, 1			; cx:bx = 1/2 divisor
	add	ax, bx
	adc	dx, cx			; dx:ax = remainder * 10 + 1/2 divisor

	mov	cx, di
	mov	bx, si

	call	Div32

cr_got_tenths:
	pop	bx			; original quotient
	mov	ah, bl

	cmp	al, 10			; if the tenths rounded up to the
	jb	cr_exit 		;   next whole number, adjust the
					;   whole number part and 0 the
	inc	ah			;   tenths  (i.e. round 1.97 to 2.0)
	xor	al, al

cr_exit:
	pop	di
	pop	si

	ret

ComputeRatio	endp



;***	Div32 - 32 bit divide for computing ratios
;
;	Entry	DX:AX = dividend, CX:BX = divisor
;
;	Exit	DX:AX = quotient, CX:BX = reminder

Div32	proc

	jcxz	d32_16bit		; differently if 16bit divisor

	push	si
	push	di

	; Brute force divide by subtraction.  This is okay because worse case
	; the dividend will only be 16 times greater, and typically about 2
	; times

	xor	si, si
	mov	di, si			; di:si is quotient
@@:
	sub	ax, bx			; subtract divisor
	sbb	dx, cx
	jc	d32_too_far

	add	si, 1			; accumulate quotient
	adc	di, 0
	jmp	short @b

d32_too_far:
	add	ax, bx			; fix the last subtraction
	adc	dx, cx

	mov	cx, di
	mov	bx, si			; dx:ax = remainder, cx:bx = quoient

	xchg	ax, bx
	xchg	dx, cx			; dx:ax = quoient, cx:bx = remainder

	pop	di
	pop	si

	ret

d32_16bit:
	div	bx			; divide dx:ax by bx

	mov	bx, dx			; remainder to cx:bx
	xor	dx, dx			; quotient to dx:ax
	mov	cx, dx
	ret

Div32	endp



;***	GetDOSFATEntry - returns next cluster in file's FAT chain
;
;	Entry	AX = current cluster number
;		ES = segment of FAT buffer
;		Entry should be in FAT buffer
;
;	Exit	AX = next cluster number
;		CY set if error
;
;	Uses	BX

GetDOSFATEntry	proc

	sub	ax, [entInBuf]		; calc entry # in buffer
	jc	gdf_ret 		; CY already set for error

	mov	bx, ax

	cmp	MDBPB.f12BitFAT, 0	; 12 or 16 bit FAT?
	jnz	gdf_12			; go do 12

	shl	bx, 1				; offset = entry * 2
	add	bx, [pbufDOSFAT]
	mov	ax, word ptr es:[bx]

gdf_success:
	clc				; success

gdf_ret:
	ret

gdf_12:
	shr	bx, 1
	add	bx, ax			; offset to entry = entry * 1.5
	add	bx, [pbufDOSFAT]

;	ES:BX points to the word containing the desired 12 bit FAT entry.
;	For odd entries, the upper 12 bits are valid, for even entries
;	the low 12 bits are valid.  odd: OOOx  even: xEEE

	test	al, 1			; is current entry odd?

	mov	ax, word ptr es:[bx]	; word with FAT entry
	jnz	gdf_odd

	and	ax, 0fffh		; keep low 12 bits for even
	jmp	short gdf_testEOF

gdf_odd:
	mov	bx, cx			; (save cx in bx)
	mov	cl, 4
	shr	ax, cl			; upper 12 bits for odd
	mov	cx, bx			; (restore cx)

gdf_testEOF:
	cmp	ax, 0FF0h		; valid entry?
	jb	gdf_success

	or	ah, 0F0h		; caller expects 16 bit special values
	jmp	short gdf_success

GetDOSFATEntry	endp



;***	GetMDFATEntry - returns requested MD FAT entry
;
;	Entry	AX = current DOS cluster number
;		ES = segment of FAT buffer
;		Entry should be in FAT buffer
;
;	Exit	BX:CX = corresponding MD FAT entry
;		CY set if error
;
;	Uses	None

GetMDFATEntry	proc

	mov	bx, ax
	sub	bx, [entInBuf]		; calc entry # in buffer
	jc	gmf_ret 		; CY already set for error return

	shl	bx, 1
	shl	bx, 1			; * 4 bytes per MDFAT entry

	add	bx, [pbufMDFAT]
	mov	cx, word ptr es:[bx]
	mov	bx, word ptr es:[bx+2]

	clc
gmf_ret:
	ret

GetMDFATEntry	endp



;***	CheckFATBuffers - check that target FAT entry is in FAT buffers.  If
;			  not, fill the buffers starting with the requested
;			  entry.
;
;	ENTRY	AX = FAT entry #
;		ES = segment of FAT buffers
;
;	EXIT	FAT buffers contain target entry, or CY set if error
;		entInBuf updated
;
;	USED	BX

CheckFATBuffers proc

	mov	bx, ax
	sub	bx, [entInBuf]
	jb	cfb_load_fat

	sub	bx, [cFATEntries]
	jae	cfb_load_fat

	clc

	ret

	; Desired entry isn't in the FAT buffers, reload the buffers to
	; include it

cfb_load_fat:

	push	ax
	push	cx
	push	dx

	; Start with the DOS FAT buffer

	xor	cx, cx			; zero high offset to FAT file position

	cmp	MDBPB.f12BitFAT, 0	; 12 or 16 bit FAT?
	jnz	cfb_12			; go do 12

	mov	[entInBuf], ax		; this entry is first

	shl	ax, 1			; 2 bytes per cluster #
	rcl	cx, 1			; cx:ax = offset to FAT entry

	jmp	short cfb_common

cfb_12:
	and	al, not 1		; start with even # entry
	mov	[entInBuf], ax

	mov	bx, ax
	shr	bx, 1
	add	ax, bx			; ax = offset to FAT entry
					;      (entry # * 1.5)
cfb_common:
	mov	bx, ax			; cx:bx = offset to FAT entry

	mov	ax, MDBPB.csecMDReserved ; # magicDrv reserved sectors
	add	ax, MDBPB.dos_bpb.csecReserved

	mul	MDBPB.dos_bpb.cbPerSec	; DX:AX = DOS FAT file origin
	add	ax, bx
	adc	dx, cx			; DX:AX = file offset to read from

	mov	cx, [cFATEntries]	; size to read
	shl	cx, 1
	mov	bx, [pbufDOSFAT]	; es:bx = location to read

	call	ReadCVFile
	jc	cfb_error

	; Now read the corresponding MagicDrv FAT entries

	mov	ax, MDBPB.secMDFATStart
	inc	ax
	mul	MDBPB.dos_bpb.cbPerSec	; DX:AX = MDFAT file offset

	mov	bx, [entInBuf]
	xor	cx, cx			; CX:BX = 32 bit cluster #
	add	bx, MDBPB.cluFirstData
	adc	cx, cx			; CX:BX = MDFAT entry #

	shl	bx, 1
	rcl	cx, 1
	shl	bx, 1
	rcl	cx, 1			; * 4 bytes per MDFAT entry

	add	ax, bx
	adc	dx, cx			; DX:AX = file offset of MDFAT entry

	mov	cx, [cFATEntries]
	shl	cx, 1
	shl	cx, 1			; size to read
	mov	bx, [pbufMDFAT] 	; es:bx = location to read into

	call	ReadCVFile
	jnc	cfb_ret

cfb_error:
	stc

cfb_ret:
	pop	dx
	pop	cx
	pop	ax
	ret

CheckFATBuffers endp



;***	ReadCVFile - read from the Compressed Volume File
;
;	Entry	DX:AX file offset, ES:BX buffer location, CX length in bytes
;
;	Exit	CY set if error, else data read
;
;	Uses	AX, BX, CX, DX

ReadCVFile	proc

	push	bx			; save buffer loc
	push	cx			; save read length

	mov	cx, dx
	mov	dx, ax			; cx:dx = file offset of fat entry
	mov	ax, (LSEEK shl 8) or 0
	mov	bx, fhCVF
	int	21h
	jc	rcf_ret 		; CY set for error return

	mov	ah, READ
	pop	cx			; read length
	pop	dx			; buffer loc offset
	push	ds
	push	es
	pop	ds			; buffer loc segment
	int	21h
	pop	ds
	jc	rcf_ret 		; CY set for error return

	cmp	ax, cx			; read it all?
	je	rcf_ret 		; yes, CY clear

	stc				; end-of-file?

rcf_ret:
	ret

ReadCVFile	endp



TRANCODE ends

endif           ;dblspace_hooks
	end
