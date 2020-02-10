;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;
;SR; 5/31/89; 
;The routines :
;
;$ROM_FIND_FIRST --- Finds first occurrence of a name in ROM
;
;$ROM_FIND_NEXT ---  Finds succeeding occurrences of the same name
;
;$ROM_EXCLUDE --- Get/Set ROM scan start address & Rom exclude regions ; M078
;

;
;------------------------------------------------------------------------------
;
; M078 : Added ROM exclude API. And modified ROM find first & find next to
;		to consult ROM exclusion list before looking at ROMs
;
;------------------------------------------------------------------------------
;

include version.inc                             ; YI 09/05/89
include dossym.inc
include dosseg.inc
include dosmac.inc
include mi.inc
include	error.inc				; M078

ifdef ROMEXEC

ifdef      JAPAN                      ; YI 09/05/89
ROMSTARTSEG     EQU     0008H
ROMENDSEG       EQU     000AH
else
BDATA	segment at 70h
	extrn	ROMSTARTADDR:word
BDATA	ends
endif

		I_NEED RomExclRegPtr,WORD	; M078
		I_NEED DMAADD,DWORD

DOSCODE	SEGMENT	BYTE   PUBLIC	'CODE'		
	ASSUME	CS:DOSCODE,SS:DOSDATA,DS:NOTHING,ES:NOTHING

	extrn	BioDataSeg:word

		PUBLIC	$ROM_FIND_FIRST
		PUBLIC	$ROM_FIND_NEXT
		PUBLIC	$ROM_EXCLUDE
		PUBLIC	Check_ROM
                PUBLIC  Check_name
ifdef      JAPAN                                          ; YI 09/05/89
                PUBLIC  ROMSTARTSEG, ROMENDSEG
endif

;
;Procedure : $ROM_FIND_FIRST
;
;		Inputs:
;			ds:dx points to user string (zero terminated)
;
;		Outputs: DTA filled as follows if match found ---
;
;		Offset 0: Name that matched (Zero terminated)
;		Offset 15: Segment address of ROM that matched
;		Offset 13: Offset for start of next search
;		Offset 17: User pattern
;
$ROM_FIND_FIRST	PROC	NEAR
	mov	si,dx				;ds:si points to user string
	call	check_name			;check if any path specifiers
	jc	bad_path			;yes, bad path

        push    ds
        mov     ds, cs:[BioDataSeg]

ifdef      JAPAN                                   ; YI 09/05/89
        mov     ax, ds:[ROMSTARTSEG]
else
	mov	ax,ds:[ROMSTARTADDR]
endif
        pop     ds
        mov     es,ax
	xor	ax,ax				;es:ax points to ROM scan addr	
	call	check_ROM			;check if in ROM
	jc	not_fnd				;not found in ROM

	call	fill_DTA			;fill the DTA		
;
;All data for FIND_NEXT has been stored
;

	invoke	get_user_stack
	and	[SI.USER_F], NOT f_Carry
	ret
bad_path:
	mov	ax,error_path_not_found		;Invalid path		M078
	jmp	short first_done
not_fnd:
	mov	ax,error_file_not_found		;File not found		M078

first_done:
	invoke	get_user_stack
	mov	[SI.USER_AX], ax
	or	[SI.USER_F], f_Carry
	ret
$ROM_FIND_FIRST	ENDP

;
;Procedure : $ROM_FIND_NEXT
;
;		Inputs:
;			DTA filled by FIND_FIRST
;		Outputs:
;			Same as FIND_FIRST 
;
$ROM_FIND_NEXT	PROC	NEAR
	lds	si,[DMAAdd]			;get DTA
	les	ax,[si+13]			;get scan addr. in es:ax
	add	si,17				;get pattern in ds:si
	call	check_ROM			;check if in ROM
	jc	no_next

	call	fill_DTA			;fill up the DTA

	invoke	get_user_stack
	and	[SI.USER_F], NOT f_Carry
	ret
no_next:
	mov	ax, error_no_more_files		; M078
	invoke	get_user_stack
	mov	[SI.USER_AX], ax
	or	[SI.USER_F], f_Carry
	ret
$ROM_FIND_NEXT	ENDP

;Procedure : Check_name
;	Inputs: ds:si points to string
;
;	Function:
;		Check if string has path specifiers i.e. ':','\'
;	Outputs:
;		Carry set if path specifiers present
;		Carry clear if no path specifiers
;
	 
Check_name	PROC	NEAR
	push	si
scan_loop:
	cmp	BYTE PTR [si],0
	jz	name_done
	cmp	BYTE PTR [si],':'		;check if drive specifier
	jz	path_spec
	cmp	BYTE PTR [si],'\'		;check if path specifier
	jz	path_spec
	inc	si
	jmp	scan_loop
name_done:
	pop	si
	clc		;no path specifier found
	ret
path_spec:
	pop	si
	stc		;path specifier found
	ret
Check_name	ENDP

;
;Procedure : Check_ROM
;	Inputs: ds:si points to string
;		ax=0 --- start scan from beginning
;		ax>0 --- start scan from given address
;		         es:ax points to address to start scan
;	Function:
;		Check if string matches ROM name
;	Outputs:
;		Carry set if no match found
;		Carry clear if match found and
;		   es:ax contains address of jmp to program in ROM
;		   dx contains offset address of matched string	
;		   cx contains length of matched string	
;

ROMSIG	EQU	0AA55H

Check_ROM	PROC	NEAR

	SaveReg	<bp, bx, si, di>	; M078

	xor	di,di			; DI is offset to ROM header
	or	ax,ax
	jz	check_sig

; restart search at location specified in AX
	add	di,ax
	jmp	short restart_search

;
;es:di contains start addr for ROM header.  check for valid ROM.
;	
check_sig:	
	cmp	WORD PTR es:[di],ROMSIG		
	jz	valid_sig
	mov	bx,es
	add	bx,80h
	jc	rom_done		; jump if above FFFFFH	; M078

	call	CheckExclRegion					; M078
	jc	rom_done					; M078

        mov     es,bx   ;next 2K block

ifdef      JAPAN
	push    ds
        mov     ds, cs:[BioDataSeg]
        cmp     bx, ds:[ROMENDSEG]
        pop     ds
        jc      check_sig
else
	jmp	check_sig					; M078
endif

rom_done:
	RestoreReg <di, si, bx, bp>				; M078
	stc		;No match found
	ret

valid_sig:
	add	di,6			; advance past header to cmd table
restart_search:

	xor	cx, cx			; set max number of searches
	call	GetMaxExclOff		; Get max offset allowed in BP ; M078

valid_lp:
	mov	bx, cx			; save max number of searches in BX
	mov	cl,es:[di]		; get length byte
	or	cl,cl			; is it zero?
	jz	next_ROM		; end of header
	inc	di			; start of name
	xor	ch,ch			; clear upper byte of length
	mov	dx, di			; save pointer to cmd name in dx
;M078-begin
	add	di, cx
	dec	di			; offset of last byte in the name
	cmp	bp, di			; does it overflow into Exclude region ?
	jb	next_ROM		; yes, skip to next ROM
	mov	di, dx			; restore pointer to name
;M078-end
	push	si			; preserve pointer to name
	call	scan
	pop	si			; restore pointer to name
	jnc	rom_match		; matched,get address
	mov	di,dx			; restore pointer to cmd name
cont_chk_nam:
	add	di,cx			; skip over name
	add	di,3			; skip over jmp
	mov	cx, bx			; get max number of searches from BX
	loop	valid_lp

next_ROM:
	xor	di, di			; get offset of ROM header
	mov	al,es:[di+2]		; get no. of pages
;
;Round off to 2K boundary
;
	xor	ah, ah			; clear upper byte
	add	ax, 011B		; to round up to fourth page
	and	ax, not 011B		; this rounds off to fourth
	mov	cl, 5			; to mult by 512 bytes per page and...
	shl	ax, cl			; ...divide by 16 bytes per paragraph
	mov	bx,es
	add	bx,ax
	jc	rom_done
	call	CheckExclRegion		; M078
	jc	rom_done		; M078

	mov	es,bx

ifdef      JAPAN                           ; YI 09/05/89
        push    ds
        mov     ds, cs:[BioDataSeg]
        cmp     bx, ds:[ROMENDSEG]
        pop     ds
        jnc     rom_done
        jmp     check_sig
else
        jmp     check_sig
endif

rom_match:
	add	di,cx			;point di to the jump instrn.
	mov	ax,di			;address of jmp in ROM
	RestoreReg <di, si, bx, bp>	; M078
	clc
	ret
Check_ROM	ENDP

;
;Procedure : Fill_DTA
;	
;		Inputs:
;			es:ax = start address for next name in ROM
;			es:dx = start address for matched name
;			cx = length of matched name
;			DMAAdd = address of current DTA
;
;		Outputs:
;			DTA filled up with the above values.
;
fill_DTA	PROC	NEAR

	mov	bx,es
	les	di,[DMAAdd]			;get DTA
	mov	es:[di+15],bx			;Store segment address of ROM
	add	ax,3				;Start of next name
	mov	es:[di+13],ax			;Store start of next name for
						;FIND_NEXT
	push	si
	push	ds
	mov	si,dx
	mov	ds,bx
	rep	movsb				;Store matched name in DTA
	xor	al,al
	stosb					;Make name zero-terminated
	pop	ds
	pop	si

	mov	di,word ptr [DMAAdd]
	add	di,17				;Start of pattern save area
cont_pat:
	lodsb
	stosb
	or	al,al				;check if end of string
	jz	end_pat
	jmp	cont_pat
end_pat:
	ret
fill_DTA	ENDP


;
;	input parameters: DS:SI -> user string
;			  ES:DI -> rom string
;			  CX	=  length of rom string
;
;	output:
;			Carry clear
;			DTA contains matching ROM string
;
;			if error Carry Set
;
;
scan 	proc	near

	push	cx
	push	ax
	push	es
	push	ds
	push	si
	push	di

	mov	ax, cx		; length of ROM string in AX

	cld

next:	cmp	byte ptr [si], '?'
	jz	skip
	cmp	byte ptr [si], '*'
	jz	found
	call	cmpi			;do case insensitive compare
	jne	no_match
	dec 	cx
	jcxz	is_match
	jmp	next

skip:	cmp	byte ptr es:[di], '.'
	je	no_match
	call 	cmpi			;do case insensitive compare
	jmp	next


found:
;
;	inc si till '.' or NULL
;	if NULL
;		matched
;	else
;		inc di till '.' or CX Z
;		if CX Z
;			no match
;		else
;			goto next
;		endif
;     	endif

	cmp	byte ptr [si], '.'
	jz	src_ext
	cmp	byte ptr [si], 0
	jz	match
	inc	si
	jmp	found

src_ext:inc	si			; point to extension of source string

do_dst:	cmp	byte ptr es:[di], '.'
	jz	chk_ext
	dec 	cx
	jcxz	no_match
	inc	di
	jmp	do_dst

chk_ext:inc 	di
	jmp	next

is_match:
	cmp	byte ptr [si],0
	jnz	no_match
match:
	clc
	jmp	short scan_done

no_match:
	stc

scan_done:	pop	di
	pop	si
	pop	ds
	pop	es
	pop	ax
	pop	cx
	ret

scan	endp

;
;Procedure : Cmpi
;
;		Input: ds:si and es:di point to the chars to be compared
;
;		Output: Zero flag set if match
;			Both di and si advanced
;
cmpi	PROC	NEAR
	push	ax
	lodsb	  		;get one char
	cmp	al,byte ptr es:[di]
	jz	cmp_match
	xor	al,20h		;get opposite case
	cmp	al,byte ptr es:[di]
cmp_match:
	lea	di,[di+1]		;Increment without changing flags
	pop	ax
	ret
cmpi	ENDP

; M078 - BEGIN

;
; Procedure : CheckExclRegion
;
;	Input BX = segment value to be validated and bumped to the next
;			non-exclude region
;	Output : Carry clear -> BX contains new segment after skipping
;				exclude regions if any
;		 Carry set -> After skipping exclude regions we crossed
;				1Meg boundary

CheckExclRegion	proc	near
		push	cx
		push	si
		push	ds

		cld
		lds	si, dword ptr RomExclRegPtr

		mov	ax, ds
		or	ax, si			; ds:si == 0:0 ?
		jz	cer6			; yes, quit

		lodsw
		mov	cx, ax
		jcxz	cer6			; quit if number of entries=0
cer1:
		cmp	bx, [si]
		jb	cer5

		cmp	bx, [si+2]
		ja	cer5

		mov	bx, [si+2]
		jmp	short cer7
cer5:
		add	si, 4
		loop	cer1
cer6:
		pop	ds
		pop	si
		pop	cx
		jmp	short cer9s
cer7:
		pop	ds
		pop	si
		pop	cx
		cmp	bx, 0ffffh
		je	cer9f
		inc	bx
cer9s:
		clc
		ret
cer9f:
		stc
		ret
CheckExclRegion	endp


;
; Procedure : GetMaxExclOff
;
;	Input ES = segment value in which the max offset is to be found
;	Output : BP contains the max offset

GetMaxExclOff	proc	near
		push	bx
		push	cx
		push	si
		push	ds

		mov	bp, 1000h
		cld
		lds	si, dword ptr RomExclRegPtr

		mov	ax, ds
		or	ax, si			; ds:si == 0:0 ?
		jz	gme9			; yes, quit

		lodsw
		mov	cx, ax
		jcxz	gme9			; quit if number of entries=0
		mov	bx, es
gme1:
		cmp	[si], bx
		jb	gme5

		mov	ax, [si]
		sub	ax, bx
		cmp	ax, bp
		jae	gme5
		mov	bp, ax
gme5:
		add	si, 4
		loop	gme1
gme9:
		mov	cl, 4
		shl	bp, cl
		dec	bp

		pop	ds
		pop	si
		pop	cx
		pop	bx
		ret
GetMaxExclOff	endp



;
; Procedure : $ROM_EXCLUDE
;		To get/Set Rom scan start address and ROM exclude regions
;
;	Input : AL=0 -- Get Romscan start segment
;	Output: BX=Rom scan start segment
;
;	Input : AL=1 -- Set Romscan start segment
;		BX=New romscan start address
;	Output:	None
;
;	Input :	AL=2 -- Get pointer to rom exclude region list
;	Output:	ES:BX=pointer to rom exclude region list
;
;	Input : AL=3 -- Set pointer to rom exclude region list
;		DS:DX=new rom exclude region list
;	Output: None
;
;
$ROM_EXCLUDE	proc	near
		assume	ds:nothing, ss:dosdata
		cmp	al, 3		; is the command out of range ?
		ja	re9f		; yes, quit!

		xor	ah, ah		; ax = command code

		dec	ax
		js	re10		; Get rom scan start segment

		dec	ax
		js	re20		; Set romscan start segment

		dec	ax
		js	re30		; get rom exclude region ptr

;
; Set Rom exclude region list pointer
;
		mov	RomExclRegPtr, dx
		mov	RomExclRegPtr+2, ds
		jmp	short re9s
;
; Get Romscan start segment
;
re10:
	        mov     ds, cs:[BioDataSeg]

ifdef      JAPAN
	        mov     bx, ds:[ROMSTARTSEG]
else
		mov	bx,ds:[ROMSTARTADDR]
endif
 		jmp	short re9s
;
; Set Romscan start segment
;
re20:
	        mov     ds, cs:[BioDataSeg]

ifdef      JAPAN
	        mov     ds:[ROMSTARTSEG], bx
else
		mov	ds:[ROMSTARTADDR], bx
endif
 		jmp	short re9s

;
; Get pointer to ROM exclude region list
;
re30:
		les	bx, dword ptr RomExclRegPtr

;;		Note : fall thru

;
; Get back to the user with success (no carry)
;
re9s:
		invoke	get_user_stack
		xor	al, al
		mov	[SI.USER_BX], bx
		mov	[SI.USER_ES], es
		and	[SI.USER_F], NOT f_Carry
		ret

re9f:
		mov	al, error_invalid_function
		invoke	get_user_stack
		or	[SI.USER_F], f_Carry
		ret
$ROM_EXCLUDE	endp

;
; M078 - end
;
DOSCODE	ENDS

endif ; ROMEXEC

	END


