	page	,132
	title	LOADHIGH Internal Command
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
;************ LOADHIGH command -- loads programs into UMBs.
;
comment %==================================================================

This is a new module added to support loading programs into UMBs provided
by DOS 5.0. 

Usage:

LOADHIGH [/L:umb[,size][;umb[,size]]*] <filespec>

<filespec> has to be a filename that is not wildcarded.


==========================================================================%

;
;	Revision History
;	================
;
;	M009	SR	08/01/90	Set flags to indicate that we are
;				loading and high and also remember
;				current UMB state.
;
;	M016	SR	08/09/90	Give special error message on attempt
;				to loadhigh batch files and invalid
;				filename on Loadhigh command line.
;
;	M039	SR	11/19/90	Bug #4270. Copy all the whitespaces
;				after the program name also as part
;				of the command line being passed to
;				the program to be invoked.
;


;*** INCLUDE FILES


	.xlist
	.xcref

	include	dossym.inc
	include	comequ.asm
	include	syscall.inc
	include arena.inc
	include	comseg.asm

	.list
	.cref

;*** EQUATES AND STRUCTURES

NUM_LH_SWS	equ	5	;number of valid switches

ResultBuffer	struc		; structure of parse result buffer

ValueType	db	?
ValueTag	db	?
SynPtr		dw	?
ValuePtr	dd	?

ResultBuffer	ends

;
; -----------------------------------------------------------------------------
;

DATARES		segment

HV_Extern	equ	1
HV_LoadHigh	equ	1
	include highvar.inc	; Includes high-variables here, as external

DATARES		ends
;
; -----------------------------------------------------------------------------
;


TRANDATA		segment

	extrn	ComExt		:BYTE
	extrn	ExeExt		:BYTE
	extrn	Extend_Buf_Ptr	:WORD
	extrn	Msg_Disp_Class	:BYTE
	extrn	Parse_LoadHi	:BYTE
	extrn	NoExecBat_Ptr	:WORD	; M016
	extrn	LhInvFil_Ptr	:WORD	; M016
	extrn	LhInvArg_Ptr	:WORD	; Richid
	extrn	ReqParmMiss	:WORD	; Richid
	extrn	LhInvSwt_Ptr	:WORD	; Richid
	extrn	LhBadUMB_Ptr	:WORD	; Richid

TRANDATA		ends

TRANSPACE	segment

	extrn	ResSeg		:WORD
	extrn	ExecPath	:BYTE
	extrn	Comsw		:WORD
	extrn	Arg		:BYTE
	extrn	SwitChar	:BYTE	; M039

TRANSPACE	ends

TRANCODE		segment

	extrn	Cerror:near
	extrn	Parse_With_Msg:near
	extrn	Lh_Execute:near			;new execute label; M051
	extrn	Path_Search:near

	assume	cs:TRANGROUP,ds:TRANGROUP,es:nothing,ss:TRANGROUP

;
; -----------------------------------------------------------------------------
;
	include highload.inc		; Grab code for ParseVar and such

iCmdLine	equ	81h		; PSP:81h points to command-line

;
; -----------------------------------------------------------------------------
;

;****	LoadHigh -- Main routine for Loadhigh command
;
;	ENTRY	Command line tail is at PSP:iCmdLine terminated by 0dh
;		CS = DS = SS = TRANGROUP
;
;	EXIT	None
;
;	USED	ax, bx, cx, dx, si, di, es
;
;	ERROR EXITS
;		Message pointers are setup at the error locations and then
;	we jump back to CERROR which is the transient error recycle point.
;	Apart from parse errors, the other errors handled are too many
;	switches anf invalid filenames.
;
;	EFFECTS
;		The allocation strategy and the state of the arena chain are
;	put in the requested state according to the given options. If a 
;	filename is also given, it is executed as well.
;
;

	public	LoadHigh

LoadHigh		proc	near

	push	ds
	pop	es
	assume	es:TRANGROUP

;
;Get command tail to be passed to the program. This includes any whitespace
;chars between the program name and its parameters as well.
;On return, ds:si points at the start of the command tail.
;

	call	ParseLhCmd		;parse the command line
	jc	LhErr			;error parsing, abort

	call	SetupCmdLine		;setup pgm's command line

	call	SetupPath		;setup path for file
	jc	LhErr			;file not found

;
;Set allocation strategy to HighFirst and link in UMBs for exec. This will
;be reset after return from the Exec
;We will also set a resident flag to indicate that UMBs were activated for
;the Exec. On return from the Exec, this flag will be used to deactivate UMBs
;

	call	HideUMBs		;prepare upper-memory for load

	jmp	Lh_Execute		;go and exec file ;M051

LhErr:
;
;The error message has been setup at this stage
;
	jmp	Cerror			;print error message and recycle 
					
LoadHigh		endp


;*** 	ParseLhCmd - parses any command-line options
;
;	ENTRY	None
;
;	EXIT	Carry clear -- command line parsed successfully
;		Carry set -- appropriate error message setup
;
;	USED	ax, si
;
;	EFFECTS
;		Options set up (see highvar.inc)
;		Filename to be executed setup
;
;	ParseLhCmd calls InitVar to initialize data filled in by ParseVar,
;	then calls ParseVar itself to actually parse the commmand-line.  On
;	return from ParseVar, DS:SI will point to the beginning of the child
;	module's name on the command-line; thus it calls LhCopyFilename to
;	prepare the command-line for that program.
;

ParseLhCmd	proc	near
	assume	ds:TRANGROUP, es:TRANGROUP

	mov	si,iCmdLine	;ds:si points at command line

	push	es		;Store ES 'cause we're gonna change it:

	push	ds
	pop	es		;Make sure es:si points to cmd line as well

	call	InitVar		;Initialize data for ParseVar
	call	ParseVar	;And parse the command line

	pop	es		;Restore ES now; we're done with it.

	jnc	plcC		;If no error, continue on our way.

	cmp	ax, PV_BadUMB	;Bad UMB passed?
	jnz	plc10
	mov	dx,offset TRANGROUP:LhBadUMB_Ptr
	stc
	ret

plc10:	mov	dx,offset TRANGROUP:LhInvSwt_Ptr
	cmp	ax, PV_InvSwt		;Unrecognized switch passed?
	jz	plc20
	mov	dx,offset TRANGROUP:LhInvArg_Ptr
plc20:	stc
	ret

plcC:	call	LhCopyFilename		;copy filename into our buffer
	ret				;Return-- carry=status
ParseLhCmd	endp

;***	LhCopyFilename -- copy filename from command line to buffer
;
;	ENTRY	ds:si points at primary argument (filename)
;
;	EXIT	Carry set -- filename has wildcards.  In this event, DX will
;				already contain an appropriate error number.
;		Carry clear -- filename has been copied as needed; DS:SI
;				points to first character (most likely space)
;				after filename.
;
;	USED	ax, si
;
;	EFFECTS
;		ExecPath contains the filename
;
;If there are any wildcards in the filename, then we have an error
;

LhCopyFilename	proc	near
	assume	ds:TRANGROUP, es:TRANGROUP

	mov	di, offset TRANGROUP:ExecPath

	mov	cx, 0	; Copied zero characters
@@:
	lodsb
	cmp	al, '*'			;wildcard?
	je	lhfilerr		;yes, error
	cmp	al, '?'			;wildcard?
	je	lhfilerr		;yes, error

	cmp	al,0dh			;carriage return?
	jz	@f
	cmp	al, SwitChar		;'/'?
	jz	@f
	or	al, al			;EOS?
	jz	@f
	cmp	al, ' '			;Space?
	jz	@f
	or	al, al
	jz	@f

	stosb				;store char
	inc	cx			;And remember that we did one more
	jmp short	@b
@@:
	xor	al,al			;Indicate EOS reached
	stosb				;store char

	or	cx, cx		; If we didn't copy any characters,
	jz	lhmissing	; they didn't give a filename.

	dec	si	; Move back to the delimiting character
	clc		; And indicate no error occurred
	ret

lhfilerr:
	mov	dx,offset TRANGROUP:LhInvFil_Ptr ; "Invalid Filename" ; M016
	stc
	ret

lhmissing:
	mov	dx,offset TRANGROUP:ReqParmMiss	; "Required parm missing"
	stc
	ret

LhCopyFilename	endp

;***	SetupCmdLine -- prepare command line for the program
;
;	ENTRY	{es/ds}:si = points just after the end of the child program
;
;	EXIT	None
;
;	USED
;
;	EFFECTS		
;		The rest of the command line following the pgm name is 
;	moved to the top of the command line buffer (at TRANGROUP:81h)
;	and a new command line length is put in
;

SetupCmdLine	proc	near
	assume	ds:TRANGROUP, es:TRANGROUP

	mov	di,iCmdLine
	xor	cl,cl
	dec	cl			;just CR means count = 0
@@:
	lodsb
	stosb

	inc	cl			;update count

	or	al, al
	jz	@f
	cmp	al,0dh			;carriage return?
	jnz	@b			;no, continue storing
@@:
	mov	es:[80h],cl		;store new cmd line length

	ret
SetupCmdLine	endp


;***	LhSetupErrMsg -- Sets up error messages
;
;	ENTRY	ax = error message number
;
;	EXIT	None
;
;	USED	dx
;
;	EFFECTS
;		Everything setup to display error message
;

LhSetupErrMsg	proc	near
	assume	ds:TRANGROUP, es:TRANGROUP

	mov	msg_disp_class,EXT_MSG_CLASS
	mov	dx,offset TranGroup:Extend_Buf_ptr
	mov	Extend_Buf_ptr,ax

	ret

LhSetupErrMsg	endp

;***	SetupPath -- Do path search for the file to be executed
;
;	ENTRY	None
;
;	EXIT	Carry set if file not found or not executable file
;
;	EFFECTS
;		ExecPath contains the full path of the file to be executed
;

SetupPath	proc	near
	assume	ds:TRANGROUP, es:TRANGROUP

;
;Juggle around the argv pointers to make argv[1] into argv[0]. This is 
;because the path search routine that we are about to invoke expects the
;filename to search for to be argv[0].
;
;If our new argv[0] starts with a switcharacter, it's an option... skip right
;over it by doing the whole move again (smaller, of course, this time).
;

	mov	ax,arg.argvcnt		;total number of arguments
	dec	ax			;less one - skip "LoadHigh"
	mov	bx,SIZE Argv_ele
	mul	bx			;dx:ax = size of argument lists

	getdata	cl, fm_argc		;CL = number of arguments to skip
	inc	cl			;Skip one arg, to get over "lh"

;
;Move argv[1]..argv[n] to argv[0]..argv[n-1].  Here, AX == the overall size
;of the argument lists.
;

argloop:
	jcxz	argdone			;If we've finished copying args, leave.

	dec	cx			;One less time we'll go through this.

	push	ax			;Copy ( size of remaining list ) bytes
	push	cx			;And remember how many args there were

	mov	cx,ax			;size to move

	mov	di,offset TRANGROUP:Arg	;Copy TO argv[0]
	mov	si,di			;
	add	si,SIZE Argv_ele	;Copy FROM argv[1]

	cld
	rep	movsb			;Move the argument list
	dec	arg.argvcnt		;Fake one less argument, and
	sub	ax, SIZE Argv_ele	;there's one argument we don't copy.

	pop	cx
	pop	ax			;Restore the size of the arg list

	jmp short argloop

;
; Done moving... argv[0] is now the child program's name, and [1] its first arg
;

argdone:
	call	path_search		;look in the path
;
;ax = 0, no file found
;ax < 4, batch file found -- cant be executed
;ax = 4,8 => .com or .exe file found
;
	or	ax,ax			;any file found?
	jz	no_exec_file		;no, error

	cmp	ax,4			;executable file?
	jl	no_exec_bat		;no, indicate fail ; M016

	clc
	ret

no_exec_bat:				; M016
	mov	dx,offset TRANGROUP:NoExecBat_Ptr ;Setup message ptr ; M016
	jmp	short lhsp_errret		;return error; M016

no_exec_file:
	mov	ax,ERROR_FILE_NOT_FOUND
	call	LhSetupErrMsg		;setup error message

lhsp_errret:				; M016
	stc
	ret

SetupPath	endp


TRANCODE		ends
	end
