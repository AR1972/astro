	page	,132
	title	crt0 - OS/2 C start up routine
;***
;crt0.asm - OS/2 C start up routine
;
;	Copyright (c) 1986-1988, Microsoft Corporation, All Rights Reserved
;
;Purpose:
;	How startup works in a few words -
;
;	The startup and termination is performed by a few modules
;
;		crt0.asm	OS/2 specific init/term
;		crt0msg.asm	OS/2 error messages
;		crt0dat.asm	remainder of shared OS/2 init/term
;
;	*************  IMPORTANT  *****************************************
;
;	The "DOSSEG" directive in this module must not be removed or else
;	the user will have to link with the "/DOSSEG" linker switch in
;	order to get proper segment ordering.
;
;	See the C documentation for more information about the /DOSSEG switch.
;
;	All assembler modules must be assembled with the /Mx switch, i.e.
;
;		masm -Mx crt0,;
;
;*******************************************************************************

;*******************************;*
	DOSSEG			;* specifies DOS SEGment ordering *
;*******************************;*

;*******************************;*
;*******************************;*

?DF=	1			; this is special for c startup

.xlist
include	version.inc
include	cmacros.inc
include	msdos.inc
include	brkctl.inc
EI_QB = 0			;[1] constants so include file works
OM_DOS5 = NOT 0 		;[1]
include stack2.inc		;[1] STACK_SIZE constant

assumesdata	macro	seg	;;[1] Newer versions of CMACROS reject
assumes seg,DGROUP		;;[1]
endm				;;[1]

.list

	page
;===========================================================================
;
;	Segment definitions
;
;	The segment order is essentially the same as in XENIX.
;	This module is edited after assembly to contain a dosseg comment
;	record for the linker.
;
;===========================================================================

createSeg _TEXT, code,	word,	public, CODE,	<>
createSeg C_ETEXT,etext, word,	public, ENDCODE,<>

createSeg _DATA, data,	word,	public, DATA,	DGROUP
createSeg STACK, stack,	para,	stack,	STACK,	DGROUP

defGrp	DGROUP			; define DGROUP

codeOFFSET equ	offset _TEXT:
dataOFFSET equ	offset DGROUP:

page

public	__acrtused 		; trick to force in startup
	__acrtused = 9876h	; funny value not easily matched in SYMDEB

extrn	__acrtmsg:abs 		; trick to pull in startup messages


sBegin	stack
assumesdata	ds		;[1]
	db	STACK_SIZE dup (?) ;[1] default stack size
sEnd

page

extrn	B$amsg_exit:FAR 	;[1]
extrn	B$terminate:FAR 	;[1]
extrn	B$Init:FAR		;[1]

sBegin	data

extrn	_edata:byte 		; end of data (start of bss)
extrn	_end:byte 		; end of bss (start of stack)

externB _osfile
externB _osmajor		; Major and Minor versions of OS/2
externB _osmode 		; real/protected mode flag

				;[1]
extrn	b$nmalloc_start:word	;[1] start of BASIC near malloc buffer
externW	_acmdln 		;[1]
externW	_aenvseg 		;[1]
externW	_asizds 		;[1]
externW	_atopsp			;[1]
externW _aexit_rtn		;[1] NEAR pointer
externW	_abrktb 		;[1]
externB _acfinfo		;[1] special C environment string
extrn	b$cfilex:byte		;[1] end char of _acfinfo
extrn	b$cfileln:abs		;[1] length
	cfile	EQU _acfinfo	;[1]
	cfilex	EQU b$cfilex	;[1]
	cfileln EQU b$cfileln	;[1]

;[1] externW	_abrktbe	;[1]
;[1] externW	_abrkp		;[1]
				;[1]

sEnd

	page


externP	_cinit			; run-time initializers


externP	main			; C main program
externP	exit			; exit ( code )

extrn	__exit:far 		; _exit ( code) (cmacros name conflict)

	extrn	DOSGETVERSION:far
	extrn	DOSGETMACHINEMODE:far
	extrn	DOSREALLOCSEG:far	;[1]

sBegin	code
assumes	cs,code

page
;***
;_astart - start of all C programs
;
;Purpose:
;	Startup routine to initialize C run-time environment.
;
;Entry:
;	OS/2 Start-Up Conditions:
;
;	DS	= Automatic Data Segment
;	SS:SP	= Stack Segment and Initial Stack Pointer
;	ES	= 0000
;	AX	= Selector of Environment Segment
;	BX	= Offset of Command Line in Environment Segment
;	CX	= Size of Automatic Data Segment (CX=0 means 65536 bytes)
;	BP	= 0000
;
;Exit:
;	Exits to OS/2 via exit().
;
;Uses:
;
;Exceptions:
;
;*******************************************************************************

labelNP	<PUBLIC,_astart> 	; start address of all "C" programs
;
;

	cld			; set direction flag (up)
	mov	[_atopsp],sp	; Top of Stack Region
	mov	[_aenvseg],ax	;[1] Handle of Environment Segment
	mov	[_acmdln],bx	;[1] Offset of Command Line String
	mov	[_aexit_rtn],codeoffset _exit ;[1] call high-level exit()
	jcxz	Got64kDS	;[1] brif got 64k DS
	xor	cx,cx		;[1] if not get 64K
	push	cx		;[1] push requested size
	push	ds		;[1] doit to DS
	call	DOSREALLOCSEG	;[1] realloc for 64K
	xchg	ax,cx		;[1] return code in cx
	jcxz	Got64kDS	;[1] brif successful

	mov	[_aexit_rtn],codeoffset B$terminate ;[1] die quickly
	mov	ax,9		;[1] C error message (not enough space)
	jmp	short __amsg_exit ;[1] print message and die

Got64kDS:
	dec	cx
	mov	[_asizds],cx	; Size of Global Data Segment
;	[1] Initialize C near heap to BASIC near malloc buffer

	mov	ax,[b$nmalloc_start]	;[1] get top of nmalloc buffer
	mov	[_abrktb].sz,ax ;[1] set near brk end of heap marker
	mov	[_abrktb].sg,ds ;[1] DGROUP segment
	call	B$Init		;[1] Do necessary BASIC init prior to XI
				;[1] processing
;
;	get OS/2 version
;
	push	ax
	mov	ax,sp

	push	ss
	push	ax		; address for version
	call	DOSGETVERSION
	pop	ax
	xchg	ah,al		; swap bytes
	mov	word ptr [_osmajor],ax
;
; Get real/protected mode flag
;
	mov	ax,dataOFFSET _osmode
	push	ds
	push	ax
	call	DOSGETMACHINEMODE

;****
;*	C_FILE_INFO must be processed before _cinit() is called
;*	because _cinit() checks handles 0-2 for device/pipe.
;****

;	fix up files inherited from child using _C_FILE_INFO

	call	inherit

;	do necessary initialization BEFORE command line processing!

	call	_cinit		; shared by OS/2 and Windows

;	process command line and environment


	call	main		; main ( argc , argv , envp )

; use whatever is in ax after returning here from the main program

	push	ax
	call	exit		; exit (AX)
				;   _exit will call terminators

page
;***
;_amsg_exit, _cintDIV - Fast exit fatal errors
;
;Purpose:
;	Exit the program with error code of 255 and appropriate error
;	message.  cintDIV is used for integer divide by zero, amsg_exit
;	is for other run time errors.
;
;Entry:
;	AX	= error message number (amsg_exit only).
;
;Exit:
;	calls exit() [cintDIV] or indirect through _aexit_rtn [amsg_exit].
;
;Uses:
;
;Exceptions:
;
;*******************************************************************************

labelNP	<PUBLIC,_cintDIV>

	assumes ds,nothing
assumesdata	ss		;[1]

;	_NMSG_WRITE will reestablish ds = DGROUP

	mov	ax,3		; Integer divide by zero interrupt
	mov	[_aexit_rtn],codeoffset _exit ; call high-level exit()
				; to cause file buffer flushing

labelNP <PUBLIC,_amsg_exit>


	JMP	B$amsg_exit	;[1] print error message and terminate


page
;***
;inherit - process C_FILE_INFO variable from the environment
;
;Purpose:
;	locates and interprets the "C_FILE_INFO" environment variable.
;	The value of this variable is written into the "_osfile" array.
;	This routine recognizes both DOS and OS/2 formats:
;
;	DOS:	";C_FILE_INFO" + count byte "N" + "N" data bytes + "\0"
;
;		where each data byte is written directly into _osfile
;		except that 0xFF represents 0
;
;	OS/2:	"_C_FILE_INFO=<AA><BB><CC><DD>" + "\0"
;
;		In this case the variable is a null-terminated string
;		(a well-formed environment variable) where each pair
;		of successive letters form one byte in _osfile.
;		The letters are in the range "A" through "P", representing
;		0 through 15.  The first letter of each pair is the more
;		significant 4 bits of the result.
;
;Entry:
;
;Exit:
;
;Uses:
;	AX, BX, CX, DX, SI, DI, ES
;
;Exceptions:
;
;*******************************************************************************

inherit proc	near
	mov	bx,cfileln
	cmp	[_osmode],0
	jne	not_fapi
;
;	Set up real-mode version of ;C_FILE_INFO
;
	mov	[cfile],';'	; change _C_FILE_INFO= to ;C_FILE_INFO
	mov	[cfilex],0
	dec	bx		; length is one less
not_fapi:
	xor	di,di
	mov	es,[_aenvseg]	; ES:DI points to environment strings
	mov	cx,07FFFh	; environment max = 32K
	cmp	byte ptr es:[di],0
	jne	cfilp
	inc	di		; first environment string is null
cfilp:
	cmp	byte ptr es:[di],0 ; check for end of environment
	je	nocfi		;   yes - not found
	mov	si,dataOFFSET cfile
	mov	dx,cx		; DX has count of bytes left in environment
	mov	cx,bx		; BX=cfileln
	repe	cmpsb		; compare for '_C_FILE_INFO='/';C_FILE_INFO'
	mov	cx,dx		; environment max = 32K
	je	gotcfi		;   yes - now do something with it
	xor	ax,ax
	repne	scasb		; search for end of current string
	je	cfilp		; keep searching
;
	jmp	short nocfi	;   no 00 !!! - assume end of env.
;
;	found _C_FILE_INFO, so transfer handle info into _osfile
;
gotcfi:
	push	es
	push	ds

	pop	es		; es = DGROUP
	mov	si,di		; si = startup of _osfile info
	pop	ds		; ds = env. segment
assumesdata	es		;[1]
	assumes	ds,nothing

	mov	di,dataOFFSET _osfile ; di = _osfile block

	cmp	bx,cfileln
	jne	real_cfi
;
; Prot-mode _C_FILE_INFO:
;	Read in pairs of characters, expected to be ['A'..'P'].
;	Each pair represents one byte in the _osfile array.
;	A null is the normal terminator for the string.
;
	mov	cl,4

osfile_lp:
	lodsb			; get next byte (more significant 4 bits)
	sub	al,'A'
	jb	nocfi		; string should terminate with a null
	shl	al,cl
	xchg	dx,ax		; save in DL

	lodsb			; get next byte (less significant 4 bits)
	sub	al,'A'
	jb	nocfi
	or	al,dl		; this assumes that AL is in range
	stosb
	jmp	short osfile_lp
;
; Real-mode C_FILE_INFO
;
real_cfi:
	lodsb			; must be less than 20
	cbw
	xchg	cx,ax		; cx = number of entries

osflp:
	lodsb			; get next byte
	inc	al
	jz	saveit		; was FF - save as 00
	dec	ax		; restore al
saveit:
	stosb
	loop	osflp		; transfer next character

;------

nocfi:
	push	ss
	pop	ds		; ds = DGROUP
assumesdata	ds		;[1]

	ret
inherit endp

sEnd
	end	_astart		; start address
