	page	,132
	title	Command.com stub for ROM COMMAND

comment	%

This command.com lies around on disk in a ROM DOS system, in case
someone needs to find and/or EXEC command.com from disk.  We find ROM
COMMAND and jump to it's entry point, with all registers set as for
the original EXEC.  ROM COMMAND will use the same PSP, etc. as this
program.

Bugbug:  Need to make sure there's enough space for the full COMMAND.

%


	.xlist
	.xcref

	include version.inc		; build version (ROMDOS, etc.)
	include versiona.inc		; DOS version number
	include	syscall.inc		; DOS function names

	.cref
	.list


ifdef      JAPAN                      ; YI 09/05/89
ROMSTARTSEG     EQU     0008H
ROMENDSEG       EQU     000AH
else
	include	oemconf.inc
endif



_TEXT	segment

	assume	cs:_TEXT,ds:_TEXT,es:_TEXT,ss:_TEXT

	org	100h
Entry:

;*	Check that we're running ROM DOS, our version.

	mov	ax,GET_VERSION shl 8
	int	21h
	cmp	ax,EXPECTED_VERSION
	jne	NoCanDo			; incorrect DOS version

	mov	ax,3306h		; AX = "Get real version info"
	int	21h
	test	dh,8			; is DOS in ROM?
	jz	DoItOurself		; no, try searching in ROM space

;*	Set DTA to somewhere besides the command tail buffer, to
;	preserve the command tail.

	mov	dx,offset TempDTA	; DS:DX = ptr to temporary DTA
	mov	ah,SET_DMA		; AH = 'Set DTA Address'
	int	21h

;*	Find the ROM COMMAND file

	mov	dx,offset ROMName	; DS:DX = ptr to ROM COMMAND name
	mov	ah,ROM_FIND_FIRST	; AH = 'Find First ROM File'
	int	21h

;*	Restore original DTA.

	pushf				; preserve search results
	mov	dx,80h			; DS:DX = ptr to normal default DTA
	mov	ah,SET_DMA		; AH = 'Set DTA Address'
	int	21h

	popf				; restore search results
	jc	NoCanDo			; search failed - give up


;*	Transfer to ROM COMMAND with original EXEC segment registers, et al.
;
;	See ROM_FIND_FIRST documentation (in dos\romfind.asm).

	push	word ptr TempDTA.15	; put segment of ROM COMMAND on stack
	sub	word ptr TempDTA.13,3
	push	word ptr TempDTA.13	; put offset of COMMAND jump-off
					;   on stack
Xfer:
	xor	ax,ax			; zero general-purpose registers
	xor	bx,bx
	xor	cx,cx
	xor	dx,dx
	retf				; transfer to ROM COMMAND


DoItOurself:

	push	es

ifdef      JAPAN                                   ; YI 09/05/89
        push    ds
	mov	ax, 70h
	mov	ds, ax
        mov     ax, ds:[ROMSTARTSEG]
        pop     ds
else
	mov	ax,ROMEXEC_SEG
endif

        mov     es,ax
	xor	ax,ax				;es:ax points to ROM scan addr	

	push	si
	mov	si, offset ROMName
	call	Check_ROM
	jc	NoCanDo

	mov	Temp, ax
	mov	Temp+2, es
	pop	si

	pop	es

	push	Temp+2
	push	Temp

	jmp	xfer

Temp	dw	?
	dw	?

NoCanDo:
	mov	ax,4C00h
	int	21h


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

;	SaveReg	<bx, si, di>

	push	bx
	push	si
	push	di

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
        mov     es,bx   ;next 2K block
ifdef      JAPAN
        jc      rom_done        ; jump if avobe 100000H
        push    ds
        mov     ds, cs:[BioDataSeg]
        cmp     bx, ds:[ROMENDSEG]
        pop     ds
        jc      check_sig
else
	jnc	check_sig
endif

rom_done:
	pop	di
	pop	si
	pop	bx

;	RestoreReg <di, si, bx>
	stc		;No match found
	ret

valid_sig:
	add	di,6			; advance past header to cmd table
restart_search:
	xor	cx, cx			; set max number of searches
valid_lp:
	mov	bx, cx			; save max number of searches in BX
	mov	cl,es:[di]		; get length byte
	or	cl,cl			; is it zero?
	jz	next_ROM		; end of header
	inc	di			; start of name
	xor	ch,ch			; clear upper byte of length
	mov	dx, di			; save pointer to cmd name in dx
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
	mov	es,bx
	jc	rom_done		; All segments done

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

	pop	di
	pop	si
	pop	bx

;	RestoreReg <di, si, bx>
	clc
	ret
Check_ROM	ENDP

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



ROMName	db	"COMMAND",0		; ROM COMMAND name

TempDTA	db	128 dup (?)		; DTA for ROM COMMAND search


_TEXT	ends
	end	Entry
