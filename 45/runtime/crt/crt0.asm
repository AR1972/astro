	page	,132
	title	crt0 - C start up routine
;***
;crt0.asm - C start up routine
;
;	Copyright (c) 1985-1988, Microsoft Corporation.  All rights reserved.
;
;Purpose:
;	How startup works in a few words -
;
;	The startup and termination is performed by a few modules
;
;		crt0.asm	DOS 2.x/3.x specific init/term
;		crt0msg.asm	DOS 2.x/3.x error messages
;		(winstart.asm)	Windows specific init/term (not included)
;
;		crt0dat.asm	remainder of shared DOS 3.x init/term
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

?DF=	1			; this is special for c startup

.xlist
include	version.inc
include	cmacros.inc
include	msdos.inc
include brkctl.inc

EI_QB = -1			;[12] interpreter version
EI_EB = 0			;[12] not EB
OM_DOS5 = 0			;[12] not an OS/2 version
include stack2.inc		;[12] STACK_SIZE constant

.list

CondEnd MACRO	entrypoint	;[12] generate END _astart
	END	entrypoint	;[12]
	ENDM			;[12]

assumesdata	macro	seg	;;[12] Newer versions of CMACROS reject
assumes seg,DGROUP		;;[12]
endm				;;[12]



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
assumesdata	ds		;[12]
	db	STACK_SIZE dup (?) ;[12] default stack size
sEnd

page

externP	main			; C main program

externP	exit			; exit ( code )

extrn	__exit:far 		; _exit ( code) (cmacros name conflict)

extrn	__amsg_exit:FAR 	;[12]
extrn	B$terminate:FAR 	;[12]
extrn	B$Init:FAR		;[12]



sBegin	data

extrn	_edata:byte 		; end of data (start of bss)
extrn	_end:byte 		; end of bss (start of stack)

externW	_psp			; psp:0 (paragraph #)


;	these are used by DOS C memory management (not used in Windows)


externW _asizds 		;[12] moved these to CDATA.INC
externW _atopsp 		;[12]
externW _aexit_rtn		;[12]
externW _abrktb 		;[12]
externW _abrktbe		;[12]
externW _abrkp			;[12]

extrn	b$nmalloc_start:word	;[12] start of BASIC near malloc buffer
externW _aenvseg		;[12]
externW _acmdseg		;[12]
externW _acmdln 		;[12]


sEnd

	page


externP	_cinit			; run-time initializers


sBegin	code
assumes	cs,code

assumes	ds,nothing

page
;***
;_astart - start of all C programs
;
;Purpose:
;	Startup routine to initialize C run-time environment
;
;Entry:
;
;Exit:
;	Exits to DOS via exit().
;
;Uses:
;
;Exceptions:
;
;*******************************************************************************

labelNP	<PUBLIC,_astart> 	; start address of all "C" programs


;	check MS-DOS version for 2.0 or later

	callos	VERSION		; AX must be preserved until later
	cmp	al,2		; check for version 2 or later
	jae	setup		;   yes - continue with setup
	int	20h		; DOS 1.0 exit program

setup:

	mov	di,DGROUP
	mov	si,ds:[DOS_MAXPARA] ; get max. paragraph

	sub	si,di		; si = # para in data area
	cmp	si,1000h	; if more than 64K
	jb	setSP

	mov	si,1000H	; use full 64K (-16)

setSP:

	cli			; turn off interrupts
	mov	ss,di		; SS = DGROUP
 	add	sp,dataoffset _end-2 ; 2 for _asizds limit
	sti			; turn interrupts back on
	jnc	SPok

	assumes ss,DGROUP	;[12] assume it points somewhere
StackOverflow:			;[12]
	xor	ax,ax		;[12] stack overflow error message (0)
	mov	SS:[_aexit_rtn],codeoffset B$terminate ;[12] jmp to INT 21, fn 4C
	jmp	short jmp_amsg_exit ;[12] print message and exit

SPok:
assumesdata	ss		;[12]

	and	sp,not 1	; make even (if not)
	mov	[_atopsp],sp	; save top of stack


	mov	ax,si		; si = # paragraphs
	mov	cl,4
	shl	ax,cl
	dec	ax
	mov	[_asizds],ax	; save DS size - 1 (in bytes)


;	release extra space to DOS

	add	si,di		; si = DGROUP + # para in DGROUP
	mov	ds:[DOS_MAXPARA],si ; fix psp:2
	mov	bx,es		; bx = PSP base
	sub	bx,si		; bx = - # para used
	neg	bx
	callos	setmem		; set memory block size
	mov	[_psp],ds	; save psp:0

	MOV	AX,DS:[2CH]		;[12] AX = Handle of Environ Segment
	MOV	SS:[_aenvseg],AX	;[12]
	mov	SS:[_acmdseg],DS	;[12] Handle of Command Line Segment
	mov	SS:[_acmdln],081H	;[12] Offset of Command Line String

;	zero data areas (_BSS and c_common)

	push	ss
	pop	es
assumesdata	es		;[12]

	cld			; set direction flag (up)
	mov	di,dataOFFSET _edata ; beginning of bss area
	mov	cx,dataOFFSET _end ; end of bss area
	sub	cx,di
	xor	ax,ax
	rep	stosb		; zero bss

;	C segmentation conventions set up here	(DS=SS and CLD)

	push	ss		; set up initial DS=ES=SS, CLD
	pop	ds
assumesdata	ds		;[12]

;[12]	 Initialize C near heap to BASIC near malloc buffer

	mov [_aexit_rtn],codeoffset _exit ;[12] call high-level exit()
	mov	ax,[b$nmalloc_start]	;[12] get top of nmalloc buffer
	mov	_abrktb.sz,ax		;[12] set near brk end of heap marker
	call	B$Init		;[12]Do necessary BASIC init Prior to XI
				;[12] processing.


;	do necessary initialization BEFORE command line processing!

	call	_cinit		; shared by DOS and Windows

	push	ss
	pop	ds		; ds = DGROUP

	push	ss		;[12]
	pop	es		;[12]set es=ds
assumesdata	ds		;[12]


;	call main and exit

	xor	bp,bp		; mark top stack frame for SYMDEB


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
;	calls exit() [cintDIV] or indirect through _aexit_rtn [amg_exit].
;
;Uses:
;
;Exceptions:
;
;*******************************************************************************

labelNP	<PUBLIC,_cintDIV>

	assumes ds,nothing
	assumesdata	ss		;[12]

;	_cintDIV establishes ds = DGROUP


	mov	ax,3		; Integer divide by zero interrupt
	mov	[_aexit_rtn],codeoffset _exit ; call high-level exit()
				; to cause file buffer flushing

jmp_amsg_exit:			;[12]
	jmp	__amsg_exit	;[12] branch to __amsg_exit in BASIC code


sEnd

	CondEnd _astart 		;[12] start address if NOT FO_RTM
