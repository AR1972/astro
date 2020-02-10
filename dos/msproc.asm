; =========================================================================
;	SCCSID = @(#)MSproc.asm        1.1 85/04/10
;
; Pseudo EXEC system call for DOS
;
; =========================================================================

.cref
.list

	TITLE	MSPROC - process maintenance
	NAME	MSPROC

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;
	PAGE	,132

; =========================================================================
;**	Process related system calls and low level routines for DOS 2.X.
;	I/O specs are defined in DISPATCH.
;
;	$WAIT
;	$EXEC
;	$Keep_process
;	Stay_resident
;	$EXIT
;	$ABORT
;	abort_inner
;
;	Modification history:
;
;		Created: ARR 30 March 1983
;		AN000	version 4.0 jan. 1988
;		A007	PTM 3957 - fake vesrion for IBMCACHE.COM
;		A008	PTM 4070 - fake version for MS WINDOWS
;
;		M000	added support for loading programs into UMBs 7/9/90
;
;		M004 - MS PASCAL 3.2 support. Please see under tag M003 in 
;		       dossym.inc. 7/30/90
;		M005 - Support for EXE programs with out STACK segment and 
;		       with resident size < 64K - 256 bytes. A 256 byte 
;		       stack is provided at the end of the program. Note that
;		       only SP is changed.
;		M020 - Fix for Rational bug for details see exepatch.asm
;
;		M028 - 4b04 implementation
;
;		M029 - Support for EXEs without stack rewritten. If EXE is
;			in memory block >= 64K, sp = 0. If memory block
;			obtained is <64K, point sp at the end of the memory
;			block. For EXEs smaller than 64K, 256 bytes are still
;			added for a stack segment which may be needed if it
;			is loaded in low memory situations.
;
;		M030 - Fixing bug in EXEPACPATCH & changing 4b04 to 4b05
;
;		M040 - Bug #3052. The environment sizing code would flag a
;			a bad environment if it reached 32767 bytes. Changed
;			to allow 32768 bytes of environment.
;
;		M047 - Release the allocated UMB when we failed to load a 
;		       COM file high. Also ensure that if the biggest block
;		       into which we load the com file is less than 64K then
;		       we provide atleast 256 bytes of stack to the user.
;
;		M050 - Made Lie table search CASE insensitive
;
;		M060 - Removed special version table from the kernal and
;                      put it in a device drive which puts the address
;                      in the DOS DATA area location UU_IFS_DOS_CALL
;		       as a DWORD.
;
;		M063 - Modified UMB support. If the HIGH_ONLY bit is set on
;		       entry do not try to load low if there is no space in
;		       UMBs.
;
;		M068 - Support for copy protect apps. Call ChkCopyProt to 
;		       set a20off_count. Set bit EXECA20BIT in DOS_FLAG. Also
;		       change return address to LeaveDos if AL=5.
;
;               20-Jul-1992 bens    Added ifdef RESTRICTED_BUILD code that
;                      controls building a version of MSDOS.SYS that only
;                      runs programs from a fixed list (defined in the
;                      file RESTRICT.INC).  Search for "RESTRICTED_BUILD"
;                      for details.  This feature is used to build a
;                      "special" version of DOS that can be handed out to
;                      OEM/ISV customers as part of a "service" disk.
;
; =========================================================================

.XLIST
.XCREF

INCLUDE version.inc
INCLUDE dosseg.inc
INCLUDE DOSSYM.INC
INCLUDE DEVSYM.INC
INCLUDE exe.inc
INCLUDE sf.inc
INCLUDE curdir.inc
INCLUDE syscall.inc
INCLUDE arena.inc
INCLUDE pdb.inc
INCLUDE vector.inc

.CREF
.LIST

public	retexepatch

; =========================================================================

ifdef	ROMEXEC
ifndef	JAPAN

BDATA	segment at 70h
	extrn	RomStartAddr	:word
BDATA	ends

endif ; JAPAN
endif ; ROMEXEC

; =========================================================================

DosData SEGMENT WORD PUBLIC 'DATA'

	EXTRN	CreatePDB	:BYTE
	EXTRN	DidCtrlC	:BYTE
	EXTRN	Exit_type	:BYTE
	EXTRN	ExtErr_Locus	:BYTE	; Extended Error Locus
	EXTRN	InDos		:BYTE

	EXTRN	OpenBuf		:BYTE
;	EXTRN	OpenBuf		:128

	EXTRN	CurrentPDB	:WORD
	EXTRN	Exit_code	:WORD

	EXTRN	DmaAdd		:DWORD


		; the following includes & i_needs are for exec.asm
		; which is included in this source

		; **** Fake_count to commented out
		
	EXTRN	Fake_Count	:BYTE	; Fake version count

	EXTRN	Special_Entries :WORD	; Address of special entries
	EXTRN	Special_Version :WORD	; Special version number
	EXTRN	Temp_Var2	:WORD	; File type from $open

		; following i_needs are becuse of moving these vars from
		; exec.asm to ../inc/ms_data.asm

	EXTRN	exec_init_SP	:WORD
	EXTRN	exec_init_SS	:WORD
	EXTRN	exec_init_IP	:WORD
	EXTRN	exec_init_CS	:WORD


	EXTRN	exec_signature	:WORD	; Must contain 4D5A  (yay zibo!)
	EXTRN	exec_len_mod_512:WORD	; Low 9 bits of length
	EXTRN	exec_pages	:WORD	; Number of 512b pages in file
	EXTRN	exec_rle_count	:WORD	; Count of reloc entries
	EXTRN	exec_par_dir	:WORD	; Number of paragraphs before image
	EXTRN	exec_min_BSS	:WORD	; Minimum number of para of BSS
	EXTRN	exec_max_BSS	:WORD	; Max number of para of BSS
	EXTRN	exec_SS 	:WORD	; Stack of image
	EXTRN	exec_SP 	:WORD	; SP of image
	EXTRN	exec_chksum	:WORD	; Checksum  of file (ignored)
	EXTRN	exec_IP 	:WORD	; IP of entry
	EXTRN	exec_CS 	:WORD	; CS of entry
	EXTRN	exec_rle_table	:WORD	; Byte offset of reloc table

	EXTRN	DOS_FLAG	:BYTE	; flag to indicate to redir that open
					; came from exec. 


	EXTRN   AllocMethod	:BYTE   ; how to alloc first(best)last
	EXTRN	SAVE_AX		:WORD	; temp to save ax
	EXTRN	AllocMsave	:BYTE	; M063: temp to save AllocMethod

	EXTRN	UU_IFS_DOS_CALL	:DWORD	; M060 Ptr to version table

	EXTRN	A20OFF_PSP	:WORD	; M068
	EXTRN	A20OFF_COUNT	:BYTE	; M068


; =========================================================================

	EXTRN	Disa20_Xfer	:WORD

	allow_getdseg

	EXTRN	DriverLoad	:BYTE
	EXTRN	BiosDataPtr	:DWORD
	extrn	DosHasHMA	:byte		; M021
	extrn	fixexepatch	:word
	extrn	Rational386PatchPtr:word
	extrn	ChkCopyProt	:word 		; M068
	extrn	LeaveDos	:word		; M068


DosData ENDS

; =========================================================================

DOSCODE	SEGMENT
	ASSUME	SS:DOSDATA,CS:DOSCODE


	EXTRN	ExecReady:near
	EXTRN	UCase:near			; M050


SAVEXIT 	EQU	10

	BREAK	<$WAIT - return previous process error code>

; =========================================================================
;	$WAIT - Return previous process error code.
;
;	Assembler usage:
;
;	    MOV     AH, WaitProcess
;	    INT     int_command
;
;	ENTRY	none
;	EXIT	(ax) = exit code
;	USES	all
; =========================================================================

	ASSUME	DS:NOTHING,ES:NOTHING

PROCEDURE $Wait ,NEAR

	xor	AX,AX
	xchg	AX,exit_code
	transfer Sys_Ret_OK

ENDPROC $Wait

ifdef RESTRICTED_BUILD
;***    IsFileOnGuestList - Check that file name is allowed to execute
;
;       This function is called by $Exec just before it attempts to
;       $Open the program file.
;
;       Entry
;           ds:dx = asciiz "guest" file name.
;                   Must be upper case and have no meta characters.
;           CLD
;
;       Exit-Success
;           returns Carry CLEAR
;
;       Exit-Failure
;           returns Carry SET
;           ax = error_access_denied

IsFileOnGuestList  proc    near
        SaveReg <bx,cx,si,di,ds,es>

;*  Compute length of guest name

        push    ds
        pop     es
        mov     di,dx                   ; es:di -> guest name
        invoke  StrLen                  ; cx = length of name including 00
        mov     bx,cx                   ; Save length for repeated use

;*  Get list of allowed names
        push    cs
        pop     es
        mov     di,offset aszRestrict   ; es:di -> allowed name list

;*  Set null character and no error in one fell swoop!
        xor     ax,ax

;*  Scan list for matching file name
;
;   ax     = 0 (used for repne scasb and successful return code)
;   bx     = length of guest name, including 00 terminator
;   ds:dx -> guest name
;   es:di -> next name on list

fgl10:  cmp     byte ptr es:[di],00     ; At end of list?
        jz      fgle                    ;   YES, file not allowed, go fail

;*  Compare names
        mov     si,dx                   ; ds:si -> guest name
        mov     cx,bx                   ; Length of guest name
        repe    cmpsb                   ; Do names match?
        jz      fglx                    ;   YES, return success

;*  Now we either ran past the end of the string in the list, or we are not
;   at the end, but found a difference.  We back up one character, since
;   repe cmpsb advanced one character past the difference, and then scan
;   the string in the list to find its end.

        dec     di                      ; es:di -> character that did not match
        mov     cx,-1                   ; Make sure we look far enough ahead
        repne   scasb                   ; Find tail of name in list

;*  Now es:[di] is first character of the next name in the list (or 00)
;
        jmp     fgl10                   ; Go check next name

;*  Name not found on list
;
fgle:   mov     ax,error_access_denied  ; Set error code
        stc                             ; Indicate error

fglx:   RestoreReg <es,ds,di,si,cx,bx>
        ret
IsFileOnGuestList  endp


;**     rstfile - macro to add file name to list
;
rstfile MACRO   name
    db  name,0
endm

;**     aszRestrict - list of files allowed to execute
;
aszRestrict LABEL   byte

include restrict.inc    ; build table

    db  0               ; terminate table

endif ; RESTRICTED_BUILD


; =========================================================================
;BREAK <$exec - load/go a program>
;	EXEC.ASM - EXEC System Call
;
;
; Assembler usage:
;	    lds     DX, Name
;	    les     BX, Blk
;	    mov     AH, Exec
;	    mov     AL, FUNC
;	    int     INT_COMMAND
;
;	AL  Function
;	--  --------
;	 0  Load and execute the program.
;	 1  Load, create  the  program	header	but  do  not
;	    begin execution.
;	 3  Load overlay. No header created.
;
;	    AL = 0 -> load/execute program
;
;	    +---------------------------+
;	    | WORD segment address of	|
;	    | environment.		|
;	    +---------------------------+
;	    | DWORD pointer to ASCIZ	|
;	    | command line at 80h	|
;	    +---------------------------+
;	    | DWORD pointer to default	|
;	    | FCB to be passed at 5Ch	|
;	    +---------------------------+
;	    | DWORD pointer to default	|
;	    | FCB to be passed at 6Ch	|
;	    +---------------------------+
;
;	    AL = 1 -> load program
;
;	    +---------------------------+
;	    | WORD segment address of	|
;	    | environment.		|
;	    +---------------------------+
;	    | DWORD pointer to ASCIZ	|
;	    | command line at 80h	|
;	    +---------------------------+
;	    | DWORD pointer to default	|
;	    | FCB to be passed at 5Ch	|
;	    +---------------------------+
;	    | DWORD pointer to default	|
;	    | FCB to be passed at 6Ch	|
;	    +---------------------------+
;	    | DWORD returned value of	|
;	    | CS:IP			|
;	    +---------------------------+
;	    | DWORD returned value of	|
;	    | SS:IP			|
;	    +---------------------------+
;
;	    AL = 3 -> load overlay
;
;	    +---------------------------+
;	    | WORD segment address where|
;	    | file will be loaded.	|
;	    +---------------------------+
;	    | WORD relocation factor to |
;	    | be applied to the image.	|
;	    +---------------------------+
;
; Returns:
;	    AX = error_invalid_function
;	       = error_bad_format
;	       = error_bad_environment
;	       = error_not_enough_memory
;	       = error_file_not_found
; =========================================================================
;
;   Revision history:
;
;	 A000	version 4.00  Jan. 1988
;
; =========================================================================

	EXTRN	Exec_Header_Len :ABS

Exec_Internal_Buffer		EQU	OpenBuf
Exec_Internal_Buffer_Size	EQU	(128+128+53+curdirLEN)

; =========================================================================

;IF1		; warning message on buffers
;%out	Please make sure that the following are contiguous and of the
;%out	following sizes:
;%out
;%out	OpenBuf     128
;%out	RenBuf	    128
;%out	SearchBuf    53
;%out	DummyCDS    CurDirLen
;ENDIF

; =========================================================================

ifdef	ROMEXEC				;SR; Externals from romfind.asm

	EXTRN	Check_name	:NEAR
	EXTRN	Check_ROM	:NEAR

	IFDEF	JAPAN			; YI 09/05/89

		EXTRN	ROMSTARTSEG	:ABS
		EXTRN	ROMENDSEG	:ABS

	ENDIF

	EXTRN	BioDataSeg	:WORD
endif ; ROMEXEC

; =========================================================================
;
; =========================================================================

procedure	$Exec,NEAR

	PUBLIC EXEC001S
EXEC001S:

	LocalVar    Exec_Blk		,DWORD
	LocalVar    Exec_Func		,BYTE
	LocalVar    Exec_Load_High	,BYTE
	LocalVar    Exec_FH		,WORD
	LocalVar    Exec_Rel_Fac	,WORD
	LocalVar    Exec_Res_Len_Para	,WORD
	LocalVar    Exec_Environ	,WORD
	LocalVar    Exec_Size		,WORD
	LocalVar    Exec_Load_Block	,WORD
	LocalVar    Exec_DMA		,WORD
	LocalVar    ExecNameLen 	,WORD
	LocalVar    ExecName		,DWORD

	LocalVar    Exec_DMA_Save	,WORD
	LocalVar    Exec_NoStack	,BYTE

ifdef ROMEXEC
			; Added 2 local variables to support exec from ROM
	LocalVar    ExecRomFound	,BYTE
	LocalVar    ExecRomAddr 	,DWORD

endif ; ROMEXEC

	; ==================================================================
	; validate function
	; ==================================================================

	PUBLIC	EXEC001E
EXEC001E:
		      	
	;
	; M068 - Start
	;
	; Reset the A20OFF_COUNT to 0. This is done as there is a 
	; possibility that the count may not be decremented all the way to
	; 0. A typical case is if the program for which we intended to keep 
	; the A20  off for a sufficiently long time (A20OFF_COUNT int 21 
	; calls), exits pre-maturely due to error conditions.
	;

	mov	[A20OFF_COUNT], 0

	;
	; If al=5 (ExecReady) we'll change the return address on the stack	
	; to be LeaveDos in msdisp.asm. This ensures that the EXECA20OFF
	; bit set in DOS_FLAG by ExceReady is not cleared in msdisp.asm
	;
	cmp	al, 5			; Q: is this ExecReady call
	jne	@f			; N: continue
					; Y: change ret addr. to LeaveDos.
	pop	cx			; Note CX is not input to ExecReady
	mov	cx, offset DOSCODE:LeaveDos
	push	cx
@@:
	;
	; M068 - End
	;

	Enter

	cmp	AL,5			; only 0, 1, 3 or 5 are allowed ;M028
					; M030
	jna	exec_check_2

Exec_Bad_Fun:
	mov	ExtErr_Locus,ErrLoc_Unk ; Extended Error Locus	;smr;SS Override
	mov	al,Error_Invalid_Function

Exec_Ret_Err:
	Leave
	transfer    SYS_RET_ERR
ExecReadyJ:
	call	ExecReady		; M028
	jmp	norm_ovl		; do a Leave & xfer sysret_OK ; M028

Exec_Check_2:
	cmp	AL,2
	jz	Exec_Bad_Fun

	cmp	al, 4			; 2 & 4 are not allowed
	je	Exec_Bad_Fun

	cmp	al, 5			; M028 ; M030
	je	ExecReadyJ		; M028

	mov	Exec_BlkL,BX		; stash args
	mov	Exec_BlkH,ES
	mov	Exec_Func,AL
	mov	Exec_Load_high,0

	mov	execNameL,DX		; set up length of exec name
	mov	execNameH,DS
	mov	SI,DX			; move pointer to convenient place
	invoke	DStrLen
	mov	ExecNameLen,CX		; save length

ifdef ROMEXEC
					; Check if ROM program
	mov     execRomFound,0          ; assume not in ROM		
					; Do not check for ROM if load overlay
	test	BYTE PTR Exec_Func,EXEC_FUNC_OVERLAY
	jnz	Do_Normal

					; A ROM program exec should have no
					; path or drive specifiers
	push	DX
	push	CX
	push	DS
	push	SI
	push	ES
	dec	CX			; length should not include NUL char

		; Check if the program exists in  ROM - returns address in
		; dx,ax if success

	call	Check_name		; DS:SI points to string
	jc      not_rom_name

Exist_ROM:

	push	DS
	mov	DS, CS:[BioDataSeg]
IFDEF	JAPAN				; YI 09/05/89
	mov	AX,DS:[ROMSTARTSEG]
ELSE
	mov	AX,DS:RomStartAddr
ENDIF
	pop	DS

	mov	ES,AX
	xor	AX,AX			; setup start addr for ROM scan
	call    Check_ROM
	jc      not_rom_name

	mov	execRomFound,1		; flag that program is in ROM
	mov	execRomAddrL,AX
	mov	execRomAddrH,ES
	pop	ES
	pop	SI			; Skip file opening and checking -
	pop	DS			; check environment
	pop	CX
	pop	DX
	jmp	short exec_check_environ

not_rom_name:
	pop	ES
	pop	SI
	pop	DS
	pop	CX
	pop	DX			; restore dx

do_normal:

endif ; ROMEXEC

	mov	al, [AllocMethod]	; M063: save alloc method in 
	mov	[AllocMsave], al	; M063: AllocMsave

	xor	AL,AL			; open for reading
	push	BP

	or	[DOS_FLAG], EXECOPEN	; this flag is set to indicate to 
					; the redir that this open call is
					; due to an exec.
ifdef RESTRICTED_BUILD
        call    IsFileOnGuestList       ; Should we execute this?
        jc      @F                      ;   NO, skip open and fail
endif ; RESTRICTED_BUILD

	invoke	$OPEN			; is the file there?

ifdef RESTRICTED_BUILD
@@: ;* Come here if file name does not pass muster
endif ; RESTRICTED_BUILD

	pushf
	and	[DOS_FLAG], not EXECOPEN; reset flag
	popf

	pop	BP
ifdef	ROMEXEC
	jnc	@F
	jmp	Exec_Ret_Err
@@:
else
	jc	Exec_Ret_Err
endif

	mov	Exec_Fh,AX
	mov	BX,AX
	xor	AL,AL
	invoke	$Ioctl
	jc	Exec_BombJ

	test	DL,DEVID_ISDEV
	jz	Exec_Check_Environ

	mov	AL,ERROR_FILE_NOT_FOUND
Exec_bombJ:
	jmp	Exec_Bomb

BadEnv:
	mov	AL,ERROR_BAD_ENVIRONMENT
	jmp	Exec_Bomb

Exec_Check_Environ:
	mov	Exec_Load_Block,0
	mov	Exec_Environ,0
					; overlays... no environment
	test	BYTE PTR Exec_Func,EXEC_FUNC_OVERLAY
	jnz	Exec_Read_Header

	lds	SI,Exec_Blk		; get block
	mov	AX,[SI].Exec1_Environ	; address of environ
	or	AX,AX
	jnz	exec_scan_env

	mov	DS,CurrentPDB		;smr;SS Override
	mov	AX,DS:[PDB_environ]

;---------------------------------------------BUG 92 4/30/90-----------------
;
; Exec_environ is being correctly initialized after the environment has been
; allocated and copied form the parent's env. It must not be initialized here.
; Because if the call to $alloc below fails Exec_dealloc will deallocate the
; parent's environment.
;	mov	Exec_Environ,AX
;
;----------------------------------------------------------------------------

	or	AX,AX
	jz	Exec_Read_Header

Exec_Scan_Env:
	mov	ES,AX
	xor	DI,DI
	mov	CX,8000h		; at most 32k of environment ;M040
	xor	AL,AL

Exec_Get_Environ_Len:
	repnz	scasb			; find that nul byte
	jnz	BadEnv

	dec	CX			; Dec CX for the next nul byte test
	js	BadEnv			; gone beyond the end of the environment

	scasb				; is there another nul byte?
	jnz	Exec_Get_Environ_Len	; no, scan some more

	push	DI
	lea	BX,[DI+0Fh+2]
	add	BX,ExecNameLen		; BX <- length of environment
					; remember argv[0] length
					; round up and remember argc
	mov	CL,4
	shr	BX,CL			; number of paragraphs needed
	push	ES
	invoke	$Alloc			; can we get the space?
	pop	DS
	pop	CX
	jnc	Exec_Save_Environ

	jmp	SHORT Exec_No_Mem	; nope... cry and sob

Exec_Save_Environ:
	mov	ES,AX
	mov	Exec_Environ,AX 	; save him for a rainy day
	xor	SI,SI
	mov	DI,SI
	rep	movsb			; copy the environment
	mov	AX,1
	stosw
	lds	SI,ExecName
	mov	CX,ExecNameLen
	rep	movsb

Exec_Read_Header:

ifdef ROMEXEC
		; SR; For a ROM program, we skip reading the program header.
		; We assume it to be a .COM program and go ahead.

	cmp	ExecRomFound,1
	jne	NoRom
	jmp	Exec_Com_File		;program is in ROM
NoRom:

endif ; ROMEXEC

		; We read in the program header into the above data area and
		; determine where in this memory the image will be located.

	Context DS
	mov	CX,Exec_Header_Len	; header size
	mov	DX,OFFSET DosData:Exec_Signature
	push	ES
	push	DS
	call	ExecRead
	pop	DS
	pop	ES
	jc	Exec_Bad_File

	or	AX,AX
	jz	Exec_Bad_File
	cmp	AX,EXEC_HEADER_LEN	; did we read the right number?
	jnz	Exec_Com_Filej		; yep... continue

	test	Exec_Max_BSS,-1 	; indicate load high?
	jnz	Exec_Check_Sig

	mov	Exec_Load_High,-1

Exec_Check_Sig:
	mov	AX,Exec_Signature	; rms;NSS
	cmp	AX,Exe_Valid_Signature	; zibo arises!
	jz	Exec_Save_Start 	; assume com file if no signature

	cmp	AX,exe_valid_Old_Signature  ; zibo arises!
	jz	Exec_Save_Start 	; assume com file if no signature

Exec_Com_Filej:
	jmp	Exec_Com_file

		; We have the program header... determine memory requirements

Exec_Save_Start:
	mov	AX,Exec_Pages		; get 512-byte pages	;rms;NSS
	mov	CL,5			; convert to paragraphs
	shl	AX,CL
	sub	AX,Exec_Par_Dir 	; AX = size in paragraphs;rms;NSS
	mov	Exec_Res_Len_Para,AX

		; Do we need to allocate memory?
		; Yes if function is not load-overlay

	test	BYTE PTR exec_func,exec_func_overlay
	jz	exec_allocate		; allocation of space

		; get load address from block

	les	DI,Exec_Blk
	mov	AX,ES:[DI].Exec3_Load_Addr
	mov	exec_dma,AX
	mov	AX,ES:[DI].Exec3_Reloc_Fac
	mov	Exec_Rel_Fac,AX

	jmp	Exec_Find_Res		; M000

Exec_No_Mem:
	mov	AL,Error_Not_Enough_Memory
	jmp	SHORT Exec_Bomb

Exec_Bad_File:
	mov	AL,Error_Bad_Format

Exec_Bomb:
	ASSUME	DS:NOTHING,ES:NOTHING

	mov	BX,Exec_fh
	call	Exec_Dealloc
	LeaveCrit   CritMem
	save	<AX,BP>
	invoke	$CLOSE
	restore <BP,AX>
	jmp	Exec_Ret_Err


Exec_Chk_Mem: 
		     			; M063 - Start
	mov	al, [AllocMethod]	; save current alloc method in ax
	mov	bl, [AllocMsave]
	mov	[AllocMethod], bl	; restore original allocmethod
	test	bl, HIGH_ONLY 		; Q: was the HIGH_ONLY bit already set
	jnz	Exec_No_Mem		; Y: no space in UMBs. Quit
					; N: continue

	test	al, HIGH_ONLY		; Q: did we set the HIGH_ONLY bit
	jz	Exec_No_Mem		; N: no memory 
	mov	ax, [save_ax]		; Y: restore ax and
	jmp	short Exec_Norm_Alloc	;    Try again
					; M063 - End
	
Exec_Allocate:
	DOSAssume   <DS>,"exec_allocate"


		; M005 - START
		; If there is no STACK segment for this exe file and if this
		; not an overlay and the resident size is less than 64K - 
		; 256 bytes we shall add 256bytes bytes to the programs 
		; resident memory requirement and set Exec_SP to this value.

	mov	Exec_NoStack,0
	cmp	Exec_SS, 0		; Q: is there a stack seg
	jne	@f			; Y: continue normal processing
	cmp	Exec_SP, 0		; Q: is there a stack ptr
	jne	@f			; Y: continue normal processing

	inc	Exec_NoStack
	cmp	ax, 01000h-10h		; Q: is this >= 64K-256 bytes
	jae	@f			; Y: don't set Exec_SP

	add	ax, 010h		; add 10h paras to mem requirement
@@:

		; M005 - END

					; M000 - start
	test	byte ptr [AllocMethod], HIGH_FIRST
					; Q: is the alloc strat high_first
	jz	Exec_Norm_Alloc		; N: normal allocate
					; Y: set high_only bit
	or	byte ptr [AllocMethod], HIGH_ONLY
					; M000 - end

Exec_Norm_Alloc:

	mov	[save_ax], ax		; M000: save ax for possible 2nd  
					; M000: attempt at allocating memory
;	push	ax			; M000

	mov	BX,0ffffh		; see how much room in arena
	push	DS
	invoke	$Alloc			; should have carry set and BX has max
	pop	DS

	mov	ax, [save_ax]		; M000
;	pop	AX			; M000

	add	AX,10h			; room for header
	cmp	BX,11h			; enough room for a header

	jb	Exec_Chk_Mem		; M000
;	jb	Exec_No_Mem		; M000


	cmp	AX,BX			; is there enough for bare image?

	ja	Exec_Chk_Mem		; M000
;	ja	Exec_No_Mem		; M000

	test	Exec_Load_High,-1	; if load high, use max
	jnz	Exec_BX_Max		; use max

	add	AX,Exec_Min_BSS 	; go for min allocation;rms;NSS

	jc	Exec_Chk_Mem		; M000
;	jc	Exec_No_Mem		; M000: oops! carry

	cmp	AX,BX			; enough space?

	ja	Exec_Chk_Mem		; M000: nope...	
;	ja	Exec_No_Mem		; M000: nope...


	sub	AX,Exec_Min_BSS 	; rms;NSS
	add	AX,Exec_Max_BSS 	; go for the MAX
	jc	Exec_BX_Max

	cmp	AX,BX
	jbe	Exec_Got_Block

Exec_BX_Max:
	mov	AX,BX

Exec_Got_Block:
	push	DS
	mov	BX,AX
	mov	exec_size,BX
	invoke	$Alloc			; get the space
	pop	DS

	ljc	exec_chk_mem		; M000

	mov	cl, [AllocMsave]	; M063: 
	mov	[AllocMethod], cl	; M063: restore allocmethod


;M029; Begin changes
; This code does special handling for programs with no stack segment. If so,
;check if the current block is larger than 64K. If so, we do not modify
;Exec_SP. If smaller than 64K, we make Exec_SP = top of block. In either
;case Exec_SS is not changed.
;
	cmp	Exec_NoStack,0
	je	@f

	cmp	bx,1000h		; Q: >= 64K memory block
	jae	@f			; Y: Exec_SP = 0

;
;Make Exec_SP point at the top of the memory block
;
	mov	cl,4
	shl	bx,cl			; get byte offset
	sub	bx,100h		; take care of PSP
	mov	Exec_SP,bx		; Exec_SP = top of block
@@:
;
;M029; end changes
;

	mov	exec_load_block,AX
	add	AX,10h
	test	exec_load_high,-1
	jz	exec_use_ax		; use ax for load info

	add	AX,exec_size		; go to end
	sub	AX,exec_res_len_para	; drop off header
	sub	AX,10h			; drop off pdb

Exec_Use_AX:
	mov	Exec_Rel_Fac,AX 	; new segment
	mov	Exec_Dma,AX		; beginning of dma

		; Determine the location in the file of the beginning of
		; the resident

Exec_Find_Res:

	mov	DX, exec_dma
	mov	exec_dma_save, DX

	mov	DX,Exec_Par_Dir
	push	DX
	mov	CL,4
	shl	DX,CL			; low word of location
	pop	AX
	mov	CL,12
	shr	AX,CL			; high word of location
	mov	CX,AX			; CX <- high

		; Read in the resident image (first, seek to it)

	mov	BX,Exec_FH
	push	DS
	xor	AL,AL
	invoke	$Lseek			; Seek to resident
	pop	DS
	jnc	exec_big_read

	jmp	exec_bomb

Exec_Big_Read:				; Read resident into memory
	mov	BX,Exec_Res_Len_Para
	cmp	BX,1000h		; Too many bytes to read?
	jb	Exec_Read_OK

	mov	BX,0fe0h		; Max in one chunk FE00 bytes

Exec_Read_OK:
	sub	Exec_Res_Len_Para,BX	; We read (soon) this many
	push	BX
	mov	CL,4
	shl	BX,CL			; Get count in bytes from paras
	mov	CX,BX			; Count in correct register
	push	DS
	mov	DS,Exec_DMA		; Set up read buffer

	ASSUME	DS:NOTHING

	xor	DX,DX
	push	CX			; Save our count
	call	ExecRead
	pop	CX			; Get old count to verify
	pop	DS
	jc	Exec_Bad_FileJ

	DOSAssume   <DS>,"exec_read_ok"

	cmp	CX,AX			; Did we read enough?
	pop	BX			; Get paragraph count back
	jz	ExecCheckEnd		; and do reloc if no more to read

		; The read did not match the request. If we are off by 512
		; bytes or more then the header lied and we have an error.

	sub	CX,AX
	cmp	CX,512
	jae	Exec_Bad_FileJ

		; We've read in CX bytes... bump DTA location

ExecCheckEnd:
	add	Exec_DMA,BX		; Bump dma address
	test	Exec_Res_Len_Para,-1
	jnz	Exec_Big_Read

		; The image has now been read in. We must perform relocation
		; to the current location.


ifdef ROMDOS
	cmp	[DosHasHMA], 0		; DOS using HMA?  (M021)
	je	exec_do_reloc		; if not, then no patch needed.
endif

exec_do_reloc:
	mov	CX,Exec_Rel_Fac
	mov	AX,Exec_SS		; get initial SS ;rms;NSS
	add	AX,CX			; and relocate him
	mov	Exec_Init_SS,AX 	; rms;NSS

	mov	AX,Exec_SP		; initial SP ;rms;NSS
	mov	Exec_Init_SP,AX 	; rms;NSS



	les	AX,DWORD PTR exec_IP	; rms;NSS
	mov	Exec_Init_IP,AX 	; rms;NSS
	mov	AX,ES			; rms;NSS
	add	AX,CX			; relocated...
	mov	Exec_Init_CS,AX 	; rms;NSS

	xor	CX,CX
	mov	DX,Exec_RLE_Table	; rms;NSS
	mov	BX,Exec_FH
	push	DS
	xor	AX,AX
	invoke	$Lseek
	pop	DS

	jnc	Exec_Get_Entries

exec_bad_filej:
	jmp	Exec_Bad_File

exec_get_entries:
	mov	DX,Exec_RLE_Count	; Number of entries left ;rms;NSS

exec_read_reloc:
	ASSUME	DS:NOTHING

	push	DX
	mov	DX,OFFSET DOSDATA:Exec_Internal_Buffer
	mov	CX,((EXEC_INTERNAL_BUFFER_SIZE)/4)*4
	push	DS
	call	ExecRead
	pop	ES
	pop	DX
	jc	Exec_Bad_FileJ

	mov	CX,(EXEC_INTERNAL_BUFFER_SIZE)/4
					; Pointer to byte location in header
	mov	DI,OFFSET DOSDATA:exec_internal_buffer 
	mov	SI,Exec_Rel_Fac 	; Relocate a single address

exec_reloc_one:
	or	DX,DX			; Any more entries?
	je	Exec_Set_PDBJ

exec_get_addr:
	lds	BX,DWORD PTR ES:[DI]	; Get ra/sa of entry
	mov	AX,DS			; Relocate address of item

;;;;;;	add	AX,SI
	add	AX, exec_dma_save

	mov	DS,AX
	add	[BX],SI
	add	DI,4
	dec	DX
	loop	Exec_Reloc_One		; End of internal buffer?

		; We've exhausted a single buffer's worth. Read in the next
		; piece of the relocation table.

	push	ES
	pop	DS
	jmp	Exec_Read_Reloc

Exec_Set_PDBJ:

		;
		; We now determine if this is a buggy exe packed file and if 
		; so we patch in the right code. Note that fixexepatch will
		; point to a ret if dos loads low. The load segment as 
		; determined above will be in exec_dma_save
		;

	push	es
	push	ax			; M030
	push	cx			; M030
	mov	es, exec_dma_save
	mov	ax, exec_init_CS	; M030
	mov	cx, exec_init_IP	; M030
	call	word ptr [fixexepatch]
	call	word ptr [Rational386PatchPtr]
	pop	cx			; M030
	pop	ax			; M030
	pop	es

	jmp	Exec_Set_PDB

Exec_No_Memj:
	jmp	Exec_No_Mem

		; we have a .COM file.	First, determine if we are merely
		; loading an overlay.

Exec_Com_File:
	test	BYTE PTR Exec_Func,EXEC_FUNC_OVERLAY
	jz	Exec_Alloc_Com_File
	lds	SI,Exec_Blk		; get arg block
	lodsw				; get load address
	mov	Exec_DMA,AX
	mov	AX,0ffffh
	jmp	SHORT Exec_Read_Block	; read it all!


			
Exec_Chk_Com_Mem:			
		     			; M063 - Start
	mov	al, [AllocMethod]	; save current alloc method in ax
	mov	bl, [AllocMsave]
	mov	[AllocMethod], bl	; restore original allocmethod
	test	bl, HIGH_ONLY 		; Q: was the HIGH_ONLY bit already set
	jnz	Exec_No_Memj		; Y: no space in UMBs. Quit
					; N: continue

	test	al, HIGH_ONLY		; Q: did we set the HIGH_ONLY bit
	jz	Exec_No_Memj		; N: no memory 

	mov	ax, exec_load_block	; M047: ax = block we just allocated	
	xor	bx, bx			; M047: bx => free arena
	call	ChangeOwner		; M047: free this block

	jmp	short Exec_Norm_Com_Alloc
					; M063 - End
	
		; We must allocate the max possible size block (ick!)
		; and set up CS=DS=ES=SS=PDB pointer, IP=100, SP=max
		; size of block.

Exec_Alloc_Com_File:
					; M000 -start
	test	byte ptr [AllocMethod], HIGH_FIRST
					; Q: is the alloc strat high_first
	jz	Exec_Norm_Com_Alloc	; N: normal allocate
					; Y: set high_only bit
	or	byte ptr [AllocMethod], HIGH_ONLY
					; M000 - end


Exec_Norm_Com_Alloc:			; M000

	mov	BX,0FFFFh
	invoke	$Alloc			; largest piece available as error
	or	BX,BX

	jz	Exec_Chk_Com_Mem	; M000
;	jz	Exec_No_Memj		; M000

	mov	Exec_Size,BX		; save size of allocation block
	push	BX
	invoke	$ALLOC			; largest piece available as error
	pop	BX			; get size of block...
	mov	Exec_Load_Block,AX
	add	AX,10h			; increment for header
	mov	Exec_DMA,AX
	xor	AX,AX			; presume 64K read...
	cmp	BX,1000h		; 64k or more in block?
	jae	Exec_Read_Com		; yes, read only 64k

	mov	AX,BX			; convert size to bytes
	mov	CL,4
	shl	AX,CL
        cmp     AX,200h                 ; enough memory for PSP and stack?

	jbe	Exec_Chk_Com_Mem	; M000: jump if not
;	jbe	Exec_No_Memj		; M000: jump if not

					; M047: size of the block is < 64K
	sub	ax, 100h		; M047: reserve 256 bytes for stack

Exec_Read_Com:
	sub	AX,100h 		; remember size of psp

ifdef ROMEXEC
		; If ROM program, do not read in file - start building header
	cmp	execRomFound,1
	jne	Exec_Read_Block		; not found, read file
	jmp     exec_build_header	; skip reading of file
endif

Exec_Read_Block:
	push	AX			; save number to read
	mov	BX,Exec_FH		; of com file
	xor	CX,CX			; but seek to 0:0
	mov	DX,CX
	xor	AX,AX			; seek relative to beginning
	invoke	$Lseek			; back to beginning of file
	pop	CX			; number to read
	mov	DS,Exec_DMA
	xor	DX,DX
	push	CX
	call	ExecRead
	pop	SI			; get number of bytes to read
	jnc	OkRead

	jmp	Exec_Bad_File

OkRead:
	cmp	AX,SI			; did we read them all?

	ljz	Exec_Chk_Com_Mem	; M00: exactly the wrong number...no 
;	ljz	Exec_No_Memj		; M00: exactly the wrong number...

	mov	bl, [AllocMsave]	; M063
	mov	[AllocMethod], bl	; M063: restore allocmethod

	test	BYTE PTR Exec_Func,EXEC_FUNC_OVERLAY
	jnz	Exec_Set_PDB		; no starto, chumo!

	mov	AX,Exec_DMA
	sub	AX,10h
	mov	Exec_Init_CS,AX
	mov	Exec_Init_IP,100h	; initial IP is 100

		; SI is AT MOST FF00h.	Add FE to account for PSP - word
		; of 0 on stack.

	add	SI,0feh 		; make room for stack

	cmp	si, 0fffeh		; M047: Q: was there >= 64K available
	je	Exec_St_Ok		; M047: Y: stack is fine
	add	si, 100h		; M047: N: add the xtra 100h for stack

Exec_St_Ok:
	mov	Exec_Init_SP,SI 	; max value for read is also SP!;smr;SS Override
	mov	Exec_Init_SS,AX 				;smr;SS Override
	mov	DS,AX
	mov	WORD PTR [SI],0 	; 0 for return

	;
	; M068
	;
	; We now determine if this is a Copy Protected App. If so the 
	; A20OFF_COUNT is set to 6. Note that ChkCopyProt will point to a 
	; a ret if DOS is loaded low. Also DS contains the load segment.

	call	word ptr [ChkCopyProt]	

Exec_Set_PDB:
	mov	BX,Exec_FH		; we are finished with the file.
	call	Exec_Dealloc
	push	BP
	invoke	$Close			; release the jfn
	pop	BP
	call	Exec_Alloc
	test	BYTE PTR Exec_Func,EXEC_FUNC_OVERLAY
	jz	Exec_Build_Header

	call	Scan_Execname
	call	Scan_Special_Entries

;SR;
;The current lie strategy uses the PSP to store the lie version. However,
;device drivers are loaded as overlays and have no PSP. To handle them, we
;use the Sysinit flag provided by the BIOS as part of a structure pointed at
;by BiosDataPtr. If this flag is set, the overlay call has been issued from
;Sysinit and therefore must be a device driver load. We then get the lie 
;version for this driver and put it into the Sysinit PSP. When the driver
;issues the version check, it gets the lie version until the next overlay
;call is issued.
;
	cmp	DriverLoad,0		;was Sysinit processing done?
	je	norm_ovl		;yes, no special handling
	push	si
	push	es
	les	si,BiosDataPtr		;get ptr to BIOS data block
	cmp	byte ptr es:[si],0		;in Sysinit?
	je	sysinit_done		;no, Sysinit is finished

	mov	es,CurrentPDB		;es = current PSP (Sysinit PSP)
	push	Special_Version
	pop	es:PDB_Version		;store lie version in Sysinit PSP
	jmp	short setver_done
sysinit_done:
	mov	DriverLoad,0		;Sysinit done,special handling off
setver_done:
	pop	es
	pop	si
norm_ovl:

	leave
	transfer    Sys_Ret_OK		; overlay load -> done

Exec_Build_Header:

ifdef ROMEXEC
		; Set up cs:ip and ss:sp for the ROM program; ax contains
		; offset of stack 100h for PSP

	cmp	ExecRomFound,1
	jnz	Not_ROM_Exec

		; AX is at most FF00h.	Add FE to account for PSP - word of
		; 0 on stack.

	add	AX,0feh
	mov	SI,AX			;store sp value
	mov	Exec_Init_SP, AX
	mov	AX,Exec_load_block	;address of PSP
	mov	Exec_Init_SS, AX	;set up ss
					;Set up word of 0 on stack for return
	mov	DS,AX			;PSP segment
	mov	WORD PTR [SI],0
	mov	AX, ExecRomAddrL	;get IP
	mov	Exec_init_IP, AX
	mov	AX,ExecRomAddrH 	;get CS
	mov	Exec_Init_CS, AX

Not_ROM_Exec:

endif ; ROMEXEC

	mov	DX,Exec_Load_Block
					; assign the space to the process
	mov	SI,Arena_Owner		; pointer to owner field
	mov	AX,Exec_Environ 	; get environ pointer
	or	AX,AX
	jz	No_Owner		; no environment

	dec	AX			; point to header
	mov	DS,AX
	mov	[SI],DX 		; assign ownership

No_Owner:
	mov	AX,Exec_Load_Block	; get load block pointer
	dec	AX
	mov	DS,AX			; point to header
	mov	[SI],DX 		; assign ownership

	push	DS			;AN000;MS. make ES=DS
	pop	ES			;AN000;MS.
	mov	DI,Arena_Name		;AN000;MS. ES:DI points to destination
	call	Scan_Execname		;AN007;MS. parse execname
					;	   ds:si->name, cx=name length
	push	CX			;AN007;;MS. save for fake version
	push	SI			;AN007;;MS. save for fake version

MoveName:				;AN000;
	lodsb				;AN000;;MS. get char
	cmp	AL,'.'			;AN000;;MS. is '.' ,may be name.exe
	jz	Mem_Done		;AN000;;MS. no, move to header
					;AN000;
	stosb				;AN000;;MS. move char
					; MSKK bug fix - limit length copied
	cmp	di,16			; end of memory arena block?
	jae	mem_done		; jump if so

	loop	movename		;AN000;;MS. continue
Mem_Done:				;AN000;
	xor	AL,AL			;AN000;;MS. make ASCIIZ
	cmp	DI,SIZE ARENA		;AN000;MS. if not all filled
	jae	Fill8			;AN000;MS.

	stosb				;AN000;MS.

Fill8:					;AN000;
	pop	SI			;AN007;MS. ds:si -> file name
	pop	CX			;AN007;MS.

	call	Scan_Special_Entries	;AN007;MS.

	push	DX
	mov	SI,exec_size
	add	SI,DX
	Invoke	$Dup_PDB		; ES is now PDB
	pop	DX

	push	exec_environ
	pop	ES:[PDB_environ]
					; *** Added for DOS 5.00
					; version number in PSP
 	push	[Special_Version]	; Set the DOS version number to
	pop	ES:[PDB_Version]	; to be used for this application

					; set up proper command line stuff
	lds	SI,Exec_Blk		; get the block
	push	DS			; save its location
	push	SI
	lds	SI,[SI.EXEC0_5C_FCB]	; get the 5c fcb

		; DS points to user space 5C FCB

	mov	CX,12			; copy drive, name and ext
	push	CX
	mov	DI,5Ch
	mov	BL,[SI]
	rep	movsb

		; DI = 5Ch + 12 = 5Ch + 0Ch = 68h

	xor	AX,AX			; zero extent, etc for CPM
	stosw
	stosw

		; DI = 5Ch + 12 + 4 = 5Ch + 10h = 6Ch

	pop	CX
	pop	SI			; get block
	pop	DS
	push	DS			; save (again)
	push	SI
	lds	SI,[SI.exec0_6C_FCB]	; get 6C FCB

		; DS points to user space 6C FCB

	mov	BH,[SI] 		; do same as above
	rep	movsb
	stosw
	stosw
	pop	SI			; get block (last time)
	pop	DS
	lds	SI,[SI.exec0_com_line]	; command line

		; DS points to user space 80 command line

	or	CL,80h
	mov	DI,CX
	rep	movsb			; Wham!

		; Process BX into default AX (validity of drive specs on args).
		; We no longer care about DS:SI.

	dec	CL			; get 0FFh in CL
	mov	AL,BH
	xor	BH,BH
	invoke	GetVisDrv
	jnc	Exec_BL

	mov	BH,CL

Exec_BL:
	mov	AL,BL
	xor	BL,BL
	invoke	GetVisDrv
	jnc	exec_Set_Return

	mov	BL,CL

Exec_Set_Return:
	invoke	get_user_stack		; get his return address
	push	[SI.user_CS]		; suck out the CS and IP
	push	[SI.user_IP]
	push	[SI.user_CS]		; suck out the CS and IP
	push	[SI.user_IP]
	pop	WORD PTR ES:[PDB_Exit]
	pop	WORD PTR ES:[PDB_Exit+2]
	xor	AX,AX
	mov	DS,AX
					; save them where we can get them
					; later when the child exits.
	pop	DS:[ADDR_INT_TERMINATE]
	pop	DS:[ADDR_INT_TERMINATE+2]
	mov	WORD PTR DMAADD,80h	; SS Override
	mov	DS,CurrentPDB		; SS Override
	mov	WORD PTR DMAADD+2,DS	; SS Override
	test	BYTE PTR exec_func,exec_func_no_execute
	jz	exec_go

	lds	SI,DWORD PTR Exec_Init_SP ; get stack SS Override
	les	DI,Exec_Blk		; and block for return
	mov	ES:[DI].EXEC1_SS,DS	; return SS

	dec	SI			; 'push' default AX
	dec	SI
	mov	[SI],BX 		; save default AX reg
	mov	ES:[DI].Exec1_SP,SI	; return 'SP'

	lds	AX,DWORD PTR Exec_Init_IP ; SS Override
	mov	ES:[DI].Exec1_CS,DS	; initial entry stuff

	mov	ES:[DI].Exec1_IP,AX
	leave
	transfer Sys_Ret_OK

exec_go:
	lds	SI,DWORD PTR Exec_Init_IP   ; get entry point SS Override
	les	DI,DWORD PTR Exec_Init_SP   ; new stack SS Override
	mov	AX,ES


	cmp	[DosHasHMA], 0		; Q: is dos in HMA (M021)
	je	Xfer_To_User		; N: transfer control to user

	push	ds			; Y: control must go to low mem stub	
	getdseg	<ds>			;    where we disable a20 and Xfer 
					;    control to user 

	or	[DOS_FLAG], EXECA20OFF	; M068:
					; M004: Set bit to signal int 21
					; ah = 25 & ah= 49. See dossym.inc 
					; under TAG M003 & M009 for 
					; explanation
	mov	[A20OFF_PSP], dx	; M068: set the PSP for which A20 is
					; M068: going to be turned OFF.

	mov	ax, ds			; ax = segment of low mem stub
	pop	ds
	assume	ds:nothing

	push	ax			; ret far into the low mem stub
	mov	ax, OFFSET Disa20_Xfer
	push	ax
	mov	AX,ES			; restore ax
	retf

Xfer_To_User:


		; DS:SI points to entry point
		; AX:DI points to initial stack
		; DX has PDB pointer
		; BX has initial AX value

	cli
	mov	BYTE PTR InDos,0	; SS Override

	ASSUME	SS:NOTHING

	mov	SS,AX			; set up user's stack
	mov	SP,DI			; and SP
	sti

	push	DS			; fake long call to entry
	push	SI
	mov	ES,DX			; set up proper seg registers
	mov	DS,DX
	mov	AX,BX			; set up proper AX

	retf

EndProc $Exec

; =========================================================================
;
; =========================================================================

Procedure   ExecRead,NEAR
	CALL	exec_dealloc
	MOV	bx,exec_fh
	PUSH	BP
	invoke	$READ
	POP	BP
	CALL	exec_alloc
	return
EndProc ExecRead

; =========================================================================
;
; =========================================================================

procedure   exec_dealloc,near

	push	    BX
	.errnz	    arena_owner_system
	sub	    BX,BX		; (bx) = ARENA_OWNER_SYSTEM
	EnterCrit   CritMEM
	call	    ChangeOwners
	pop	    BX
	return

EndProc exec_dealloc

; =========================================================================
;
; =========================================================================

procedure   exec_alloc,near
	ASSUME	SS:DOSDATA

	push	    BX
	mov	    BX,CurrentPDB	; SS Override
	call	    ChangeOwners
	LeaveCrit   CritMEM
	pop	    BX
	return

EndProc exec_alloc

; =========================================================================
;
; =========================================================================

PROCEDURE   ChangeOwners,NEAR

	pushf
	push	AX
	mov	AX,exec_environ
	call	ChangeOwner
	mov	AX,exec_load_block
	call	ChangeOwner
	pop	AX
	popf
	return

ENDPROC ChangeOwners

; =========================================================================
;
; =========================================================================

PROCEDURE   ChangeOwner,NEAR

	or	AX,AX			; is area allocated?
	retz				; no, do nothing
	dec	AX
	push	DS
	mov	DS,AX
	mov	DS:[ARENA_OWNER],BX
	pop	DS
	return

EndProc ChangeOwner

; =========================================================================
;
; =========================================================================

Procedure	Scan_Execname,near
	ASSUME	SS:DosData

	lds	SI,ExecName		; DS:SI points to name
Entry	Scan_Execname1			; M028
Save_Begin:				;
	mov	CX,SI			; CX= starting addr
Scan0:					;
	lodsb				; get char

IFDEF	DBCS		 		; MSKK01 07/14/89
	invoke	TESTKANJ		; Is Character lead byte of DBCS?
	jz	@F			; jump if not
	lodsb				; skip over DBCS character
	jmp	short scan0		; do scan again
@@:
ENDIF

	cmp	AL,':'			; is ':' , may be A:name
	jz	save_begin		; yes, save si
	cmp	AL,'\'                  ; is '\', may be A:\name
	jz	save_begin		; yes, save si
	cmp	AL,0			; is end of name
	jnz	scan0			; no, continue scanning
	sub	SI,CX			; get name's length
	xchg	SI,CX			; cx= length, si= starting addr

	return

EndProc Scan_Execname

; =========================================================================
;
; =========================================================================

Procedure    Scan_Special_Entries,near
	assume	SS:DOSDATA

	dec	CX			; cx= name length
;M060	mov	DI,[Special_Entries]	; es:di -> addr of special entries
					;reset to current version
	mov    [Special_Version],(Minor_Version SHL 8) + Major_Version
;***	call	Reset_Version

;M060	push	SS
;M060	pop	ES

	les	DI,SS:UU_IFS_DOS_CALL	;M060; ES:DI --> Table in SETVER.SYS
	mov	AX,ES			;M060; First do a NULL ptr check to
	or	AX,DI			;M060; be sure the table exists
	jz	End_List		;M060; If ZR then no table

GetEntries:
	mov	AL,ES:[DI]		; end of list
	or	AL,AL
	jz	End_List		; yes

	mov	[Temp_Var2],DI		; save di
	cmp	AL,CL			; same length ?
	jnz	SkipOne 		; no

	inc	DI			; es:di -> special name
	push	CX			; save length and name addr
	push	SI

;
; M050 - BEGIN
;
	push	ax			; save len
sse_next_char:
	lodsb
	call	UCase
	scasb
	jne	Not_Matched
	loop	sse_next_char
	
;
;	repz	cmpsb			; same name ?
;
;	jnz	Not_Matched		; no
;
	pop	ax			; take len off the stack
;
; M050 - END
;
	mov	AX,ES:[DI]		; get special version
	mov	[Special_Version],AX	; save it

;***	mov	AL,ES:[DI+2]		; get fake count
;***	mov	[Fake_Count],AL 	; save it

	pop	SI
	pop	CX
	jmp	SHORT end_list

Not_Matched:
	pop	ax			; get len from stack ; M050
	pop	SI			; restore si,cx
	pop	CX

SkipOne:
	mov	DI,[Temp_Var2]		; restore old di use SS Override
	xor	AH,AH			; position to next entry
	add	DI,AX

	add	DI,3			; DI -> next entry length
;***	add	DI,4			; DI -> next entry length

	jmp	Getentries

End_List:
	return

EndProc Scan_Special_Entries

; =========================================================================
;
; =========================================================================
;
;Procedure    Reset_Version,near
;	assume	SS:DOSDATA
;
;	cmp    [Fake_Count],0ffh
;	jnz    @F
;	mov    [Special_Version],0	;reset to current version
;@@:
;	return
;
;EndProc Reset_Version,near

PAGE
; =========================================================================
;SUBTTL Terminate and stay resident handler
;
; Input:    DX is  an  offset  from  CurrentPDB  at which to
;	    truncate the current block.
;
; output:   The current block is truncated (expanded) to be [DX+15]/16
;	    paragraphs long.  An exit is simulated via resetting CurrentPDB
;	    and restoring the vectors.
;
; =========================================================================
PROCEDURE $Keep_process ,NEAR
	ASSUME DS:NOTHING,ES:NOTHING,SS:DosData

	push	AX			; keep exit code around
	mov	BYTE PTR Exit_type,EXIT_KEEP_PROCESS
	mov	ES,CurrentPDB
	cmp	DX,6h			; keep enough space around for system
	jae	Keep_shrink		; info

	mov	DX,6h

Keep_Shrink:
	mov	BX,DX
	push	BX
	push	ES
	invoke	$SETBLOCK		; ignore return codes.
	pop	DS
	pop	BX
	jc	keep_done		; failed on modification

	mov	AX,DS
	add	AX,BX
	mov	DS:PDB_block_len,AX	;PBUGBUG

Keep_Done:
	pop	AX
	jmp	SHORT exit_inner	; and let abort take care of the rest

EndProc $Keep_process

; =========================================================================
;
; =========================================================================

PROCEDURE Stay_Resident,NEAR
	ASSUME	DS:NOTHING,ES:NOTHING,SS:NOTHING

	mov	AX,(Keep_process SHL 8) + 0 ; Lower part is return code;PBUGBUG
	add	DX,15
	rcr	DX,1
	mov	CL,3
	shr	DX,CL

	transfer    COMMAND

ENDPROC Stay_resident


PAGE
; =========================================================================
;SUBTTL $EXIT - return to parent process
;   Assembler usage:
;	    MOV     AL, code
;	    MOV     AH, Exit
;	    INT     int_command
;   Error return:
;	    None.
;
; =========================================================================

PROCEDURE   $Exit ,NEAR
	ASSUME	DS:NOTHING,ES:NOTHING,SS:DosData

	xor	AH,AH
	xchg	AH,BYTE PTR DidCtrlC
	or	AH,AH
	mov	BYTE PTR Exit_Type,EXIT_TERMINATE
	jz	exit_inner
	mov	BYTE PTR Exit_type,exit_ctrl_c

	entry	Exit_inner

	invoke	get_user_stack		;PBUGBUG

	ASSUME DS:NOTHING

	push	CurrentPDB
	pop	[SI.User_CS]		;PBUGBUG
	jmp	SHORT Abort_Inner

EndProc $EXIT

BREAK <$ABORT -- Terminate a process>
; =========================================================================
; Inputs:
;	user_CS:00 must point to valid program header block
; Function:
;	Restore terminate and Cntrl-C addresses, flush buffers and transfer to
;	the terminate address
; Returns:
;	TO THE TERMINATE ADDRESS
; =========================================================================

PROCEDURE   $Abort ,NEAR
	ASSUME	DS:NOTHING,ES:NOTHING	;PBUGBUG

	xor	AL,AL
	mov	exit_type,exit_abort

		; abort_inner must have AL set as the exit code! The exit type
		; is retrieved from exit_type. Also, the PDB at user_CS needs
		; to be correct as the one that is terminating.

	PUBLIC	Abort_Inner
Abort_Inner:

	mov	AH,Exit_Type
	mov	Exit_Code,AX
	invoke	Get_User_Stack

	ASSUME DS:NOTHING

	mov	DS,[SI.User_CS]	; set up old interrupts ;PBUGBUG
	xor	AX,AX
	mov	ES,AX
	mov	SI,SavExit
	mov	DI,Addr_Int_Terminate
	movsw
	movsw
	movsw
	movsw
	movsw
	movsw
	transfer reset_environment

ENDPROC $ABORT

;==========================================================================
;
; fixexepatch will poin to this is DOS loads low. 
;
;=========================================================================
retexepatch	proc	near
	
	ret

retexepatch 	endp

; =========================================================================

DOSCODE	ENDS

; =========================================================================

	END




