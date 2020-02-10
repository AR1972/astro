;========================================================
COMMENT #

	FCB_OPEN.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Does and FCB open using DOS int 21h function
	0xf.

	int FcbOpen( struct DOS_FCB *Fcb );

	ARGUMENTS: Fcb	- Ptr to FCB with name and drive
			  already initialized.
	RETURNS:   int	- 0 if OK else -1

	================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

FcbOpen PROC USES DS, Fcb:PTR 

;IF @datasize
;	lds	DX,Fcb
;ELSE
;	mov	DX,Fcb
;ENDIF
	LoadPtr	DS, DX, Fcb

	mov	AH,0fh
	int	21h

	mov	AH,0ffh		; If AL == 0ffh then we
	cmp	AL,AH		; have -1 in AX else clear
	je	@f		; AX to signal OK
	xor	AX,AX
@@:
	ret

FcbOpen ENDP

; =======================================================

END
