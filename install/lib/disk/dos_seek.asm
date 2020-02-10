;========================================================
COMMENT #

	DOS_SEEK.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Seeks to the specified offset in an open disk
	disk file.

	long	_dos_seek( int Handle, long lOffset, int Mode )

	ARGUMENTS:	Handle	- Open DOS file handle
			lOffset - Offset to seek to in bytes
			Mode	- Seek mode as described below
				  0 = Beginning of file + offset
				  1 = Current file position + offset
				  2 = End of file + offset
	RETURNS:	long	- New offset in file is success
				  or -1L if error
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

_dos_seek PROC USES ES, Handle:WORD, lOffset:DWORD, Mode:BYTE

	mov	AH,42h
	mov	AL,Mode
	mov	BX,Handle

LoadOffset:
	les	DX,lOffset
	mov	CX,ES

Int21Call:
	int	21h
	jc	SeekError
	jmp	SHORT SeekReturn

SeekError:
	mov	AX,-1		; Error code
	cwd			; Extend sign to make a LONG (dword)

SeekReturn:
	ret

_dos_seek ENDP

; =======================================================

	END

; =======================================================
