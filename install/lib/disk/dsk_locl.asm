;========================================================
COMMENT #

	DSK_LOCL.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Checks a DOS drive to see if it is removeable

	int IsRemoveable( int Drive );

	ARGUMENTS: Drive - DOS drive number (0=default, 1=A, 2=B, ...)
	RETURNS:   int	- TRUE if disk is removeable else false

	================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

NET_MAP		EQU    1000000000000b ; Mask for net drive bits



DIRSTRLEN	EQU	64+3		; Max length in bytes of directory strings
TEMPLEN 	EQU	DIRSTRLEN*2

CURDIR_LIST	STRUC
CURDIR_TEXT	DB	DIRSTRLEN DUP (?)   ; text of assignment and curdir
CURDIR_FLAGS	DW	?		; various flags
CURDIR_DEVPTR	DD	?		; local pointer to DPB or net device
CURDIR_ID	DW	2 dup (?)	; cluster of current dir (net ID)
CURDIR_USER_WORD DW	?
CURDIR_END	DW	?		; index to ".." backup limit - see above
CURDIR_TYPE	DB	?		; IFS drive (2=ifs, 4=netuse)
CURDIR_IFS_HDR	DD	?		; Ptr to File System Header
CURDIR_FSDA	DB	2 DUP (?)	; File System Dependent Data Area
CURDIR_LIST	ENDS

curdirLen	EQU	Size CURDIR_list	; Needed for screwed up
						; ASM87 which doesn't allow
						; Size directive as a macro
						; argument
CURDIR_netID	EQU	DWORD PTR CURDIR_ID


;**	Flag values for CURDIR_FLAGS

CURDIR_isnet	EQU	1000000000000000B
CURDIR_isifs	EQU	1000000000000000B
CURDIR_inuse	EQU	0100000000000000B
CURDIR_splice	EQU	0010000000000000B
CURDIR_local	EQU	0001000000000000B



SysInitVars STRUC
SYSI_DPB	    DD	    ?		; DPB chain
SYSI_SFT	    DD	    ?		; SFT chain
SYSI_CLOCK	    DD	    ?		; CLOCK device
SYSI_CON	    DD	    ?		; CON device
SYSI_MAXSEC	    DW	    ?		; maximum sector size
SYSI_BUF	    DD	    ?		; points to Hashinitvar
SYSI_CDS	    DD	    ?		; CDS list
SYSI_FCB	    DD	    ?		; FCB chain
SYSI_Keep	    DW	    ?		; keep count
SYSI_NUMIO	    DB	    ?		; Number of block devices
SYSI_NCDS	    DB	    ?		; number of CDS's
SYSI_DEV	    DD	    ?		; device list
SYSI_ATTR	    DW	    ?		; null device attribute word
SYSI_STRAT	    DW	    ?		; null device strategy entry point
SYSI_INTER	    DW	    ?		; null device interrupt entry point
SYSI_NAME	    DB	 8 DUP(?)	; null device name
SYSI_SPLICE	    DB	    0		; TRUE -> splicees being done
SYSI_IBMDOS_SIZE    DW	    ?		; DOS size in paragraphs
SYSI_IFS_DOSCALL@   DD	    ?		; IFS DOS service rountine entry
SYSI_IFS	    DD	    ?		; IFS header chain
SYSI_BUFFERS	    DW	    ?,0 	; BUFFERS= values (m,n)
SYSI_BOOT_DRIVE     DB	    ?		; boot drive A=1 B=2,..
SYSI_DWMOVE	    DB	    0		; 1 if 386 machine
SYSI_EXT_MEM	    DW	    0		; Extended memory size in KB.
SysInitVars ENDS

; =======================================================

.CODE

; =======================================================

IsRemoveable PROC Drive:BYTE

	xor	AX,AX			; First check to be sure the
	mov	AL,Drive		; drive is really local because
	push	AX			; net drives don't support the


	call_M	IsLocalDrive		; isremoveable ioctl so always


	add	SP,2			; return  non-removeable for
	or	AX,AX			; for net drives.
	jz	NotSupported		; NOTE: DOS 4.x hangs if 4408 is
					; called for a bogus drive

	mov	AX,4408h		; IOCTL is changeable? function
	mov	BL,Drive		; BL = drive number
	int	21h

	jc	NotSupported		; Error check
	and	AX,1			; Mask off low bit
	xor	AX,1			; Flip the bit
	jmp	SHORT IsRemoveableExit

NotSupported:
	mov	AX,0			; Can't tell so assume not removable

IsRemoveableExit:
	ret

IsRemoveable ENDP


;========================================================
COMMENT #

	Copyright (c) 1989 - Microsoft Corp.
	All rights reserved.


	=================================================
	Returns whether a drive is local or remote.

	int IsLocalDrive( int Drive );

	ARGUMENTS: Drive - DOS drive number (0=default, 1=A, 2=B, ...)
	RETURNS:   int	- TRUE if disk is local else FALSE


	================================================

	johnhe - 06/06/89

END COMMENT #

;========================================================

IsLocalDrive PROC USES ES, Drive:BYTE

	mov	AX,3000h
	int 	21h
	cmp	AL,04
	je	LookInCDS

	mov	AX,4409h		; IOCTL is redirected? function 
	mov	BL,Drive		; BL = drive number
	int	21h

	mov	AX,1			; Assume it is a local drive
	jc	IsLocalExit		; If not supported then can't be net

	test	DX,NET_MAP		; See if any funny bits are set
	jz	IsLocalExit		; No funny bits so must be local

	xor	AX,AX			; Signal not a local drive
	jmp	short IsLocalExit

LookInCDS:
	mov	AH,52h			; Get internal DOS DATA Segment
	int	21h			; ES:BX --> DOS interal varibles

	mov	CL,Drive		; First make sure that this drive
	dec	CL			; is not greater than last drive
					; If CL is zero it will fail the
	cmp	CL,ES:[BX].SYSI_NCDS	; compare by doing  unsigned cmp
	mov	AX,1
	ja	IsLocalExit

	les	BX,ES:[BX].SYSI_CDS	; ES:BX --> First CDS entry

	mov	AX,SIZE CURDIR_LIST	; Find offset of the entry indexed
	mul	CL			; by drive in CL
	add 	BX,AX			; ES:BX --> CDS entry for drive
	test	ES:[BX].CURDIR_FLAGS,CURDIR_isnet ; Test is net bit

	mov	AX,0			; Assume drive is not local
	jnz	IsLocalExit		; If true then jmp to exit
	inc	AX			; Else signal local drive
	
IsLocalExit:
	ret


IsLocalDrive ENDP


; ========================================================

END
