.386p
page	58,132
;******************************************************************************
	title	MEMMONF - ON/OFF module for EMM386
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386
;
;   Module:   MEMMONF - parse for on/off/auto and perform the function
;
;   Version:  2.00
;
;   Date:     June 4, 1986
;
;   Author:   Brad Tate
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION                  DESCRIPTION
;   --------  --------  -------------------------------------------------------
;   06/04/86  Original
;   06/28/86  0.02	Name change from CEMM386 to CEMM (SBP).
;   07/14/86  0.06	displays inaccessible message (SBP).
;   06/07/87  2.00	Added Weitek switches (SBP).
;   07/27/87  2.02	Changed Weitek switches to W=ON/W=OFF.
;			Also updated get_token to only convert chars to lower
;			case (SBP).
;
;******************************************************************************
;
;   Functional Description:
;	CEMMONF is used by CEMM.EXE UTILITY code and CEMM.COM to parse
;   the command line for ON, OFF, AUTO, W=ON, or W=OFF and perform the function
;   via a call to ELIM_Entry.  This utility can also enable or disable
;   the Weitek Coprocessor support for a system which has a Weitek installed.
;   It also displays the appropriate message depending on the results of
;   the parsing and call to ELIM_Entry.
;
;******************************************************************************
.lfcond					; list false conditionals
	page
;******************************************************************************
;			P U B L I C   D E C L A R A T I O N S
;******************************************************************************
;
	public	onf_func
	public	get_token
;
;******************************************************************************
;			E X T E R N A L   R E F E R E N C E S
;******************************************************************************
include vdmseg.inc
include	emm386.inc
include	desc.inc


R_CODE	segment

	extrn	UMBHMA:byte
	extrn	Current_State:word

R_CODE	ends

LAST	segment
 	extrn 	Inst_chk:near
	extrn	ELIM_link:near
;***************************************************************************
;	external messages
;***************************************************************************
 	extrn 	OF_not_there:byte
 	extrn 	OF_vmode:byte
 	extrn 	OF_rmode:byte
 	extrn 	OF_inaccess:byte
 	extrn 	OF_amode:byte
 	extrn 	OF_verror:byte
 	extrn 	OF_rerr:byte
 	extrn 	OF_aerr:byte
 	extrn 	OF_won_mode:byte
 	extrn 	OF_woff_mode:byte
 	extrn 	OF_won_err:byte
 	extrn 	OF_woff_err:byte
 	extrn 	OF_w_not_inst:byte
 	extrn 	OF_w_inaccess:byte
 	extrn 	OF_parmerr:byte
	extrn	OF_invparm:byte
	extrn	OF_proterr:byte

	extrn	FarLink:dword
	extrn	I_Message_Display:near
	extrn	E_XStatus_Display:near

LAST	ends

;
	page
;******************************************************************************
;			L O C A L   C O N S T A N T S
;******************************************************************************
	include	ascii_sm.equ
	include emmfunct.inc
;
MSDOS		equ	21h			; MS-DOS function call
;
;  AX values for CEMM functions
;
CEMMF_SETON	equ	0100h		; set mode to ON
CEMMF_SETOFF	equ	0101h		; set mode to OFF
CEMMF_SETAUTO	equ	0102h		; set mode to AUTO

CEMMF_SENSEW	equ	0200h
CEMMF_SETWON	equ	0201h		; set Weitek support ON
CEMMF_SETWOFF	equ	0202h		; set Weitek support OFF

;******************************************************************************
;			S E G M E N T   D E F I N I T I O N
;******************************************************************************
;
LAST	segment
	assume	cs:LAST, ds:LAST, es:NOTHING, ss:NOTHING
;
;******************************************************************************
;			L O C A L   D A T A   A R E A
;******************************************************************************
;
;
mode_msg_tbl		label	word	; table of mode status messages
		dw	offset OF_vmode	; on
		dw	offset OF_rmode	; off
		dw	offset OF_amode	; auto

;
; local storage for get_token tokens
;
arg_str		db	max_arg_len	dup(0)

;
;	the valid arguments
;
on_arg		db	"on"		; CEMM -> ON mode
on_len		equ	(this byte - on_arg)

off_arg		db	"off"		; CEMM -> OFF mode
off_len		equ	(this byte - off_arg)

auto_arg	db	"auto"		; CEMM -> AUTO Mode
auto_len	equ	(this byte - auto_arg)

won_arg		db	"w=on"		; CEMM Weitek support on
won_len		equ	(this byte - won_arg)

woff_arg	db	"w=off"		; CEMM Weitek support on
woff_len	equ	(this byte - woff_arg)

help_arg	db	"/?"		; help text
help_len	equ	(this byte - help_arg)


;
; Tables of valid arguments and their lengths
;
arg_tbl		label	word
INDEX_ON	equ	(this byte - arg_tbl)
		dw	offset	on_arg
INDEX_OFF	equ	(this byte - arg_tbl)
		dw	offset	off_arg
INDEX_AUTO	equ	(this byte - arg_tbl)
		dw	offset	auto_arg
INDEX_WON	equ	(this byte - arg_tbl)
		dw	offset	won_arg
INDEX_WOFF	equ	(this byte - arg_tbl)
		dw	offset	woff_arg
INDEX_HELP	equ	(this byte - arg_tbl)
		dw	offset	help_arg
max_args	equ	(this byte - arg_tbl)
;
arg_len		label	word		; table of argument lengths
		dw	on_len
		dw	off_len
		dw	auto_len
		dw	won_len
		dw	woff_len
		dw	help_len
;
; flags for arguments - set when arg found
;
arg_flag	label	word
		dw	FALSE		; on_arg
		dw	FALSE		; off_arg
		dw	FALSE		; auto_arg
		dw	FALSE		; won_arg
		dw	FALSE		; woff_arg
		dw	FALSE		; help_arg
;
; State of parsing
;
arg_state	db	0
;
; flag bits for parsing state
;
fArg_Found	equ	00000001b	; Argument found
fMode_Arg	equ	00000010b	; Mode argument encountered
fWeitek_Arg	equ	00000100b	; Weitek arg encountered
fParmError	equ	00001000b	; Bad parameter(s) found

;
OFF_mode	db	0		; non-zero => CEMM OFF

page
;******************************************************************************
;	onf_func - Check command line for ON OFF or AUTO and perform function
;
;	ENTRY: es:di points to command line terminated by CR or LF
;		DS = CS
;
;	EXIT: The appropriate message is displayed
;
;	USED: none
;
;******************************************************************************
onf_func	proc	near


.8086
	push	ax
	push	dx
	push	di
	push	ds

	cld

	call	parse_args		; parse command line
	cmp	[arg_flag+INDEX_HELP],TRUE
					;Q: /? arg specified ?
	je	of_usage1		;  Y: display help text

	test	[arg_state],fParmError	;Q: any invalid args ?
	JNZ	of_invmsg1		;  Y: display msg and leave

;
; check for driver installed
;
	call	Inst_chk		; al = 0/1 => isn't/is installed
	or	al,al			; q: is it installed?
	jnz	short drvr_installed
	mov	dx,offset OF_not_there	; Not installed message
 	jmp	of_err_msg		; display message and quit
;

of_usage1:
	jmp	of_usage

of_invmsg1:
	jmp	of_invmsg

.386p
drvr_installed:



	test	[arg_state],fArg_Found	;Q: any cmd line args found ?
	JNZ	chk_mode_cmds		;  Y: check them out


	mov	ax, word ptr cs:[FarLink+2]
	mov	ds, ax	   		; ds = R_CODE
	assume	ds: NOTHING

;;	mov	ah,4			; display extended status
;;	call	ELIM_link
	call	I_Message_Display

;;	mov	ah,5			; display extended status
;;	call	ELIM_link
	call	E_XStatus_Display

	mov	ax, cs
	mov	ds, ax			; restore ds = LAST
	assume	ds:LAST
	jmp	of_status

;
; check command line args
;
;	call	parse_args		; parse command line
;	test	[arg_state],fArg_Found	;Q: any cmd line args found ?
;	JZ	of_status		;  N: display current status
;					;  Y: check for invalid parms
;of_chkprm:				;
;	test	[arg_state],fParmError	;Q: any invalid args ?
;	JNZ	of_usage		;  Y: display usage msg and leave
;					;  N: execute user's commands
;
; check for Mode commands
;
chk_mode_cmds:
	test	[arg_state],fMode_Arg		;Q: mode arg given ?
	jz	SHORT of_chkWeitek		;  N: check Weitek args
						;  Y: check mode arg
	cmp	[arg_flag+INDEX_ON],TRUE	;Q: ON arg specified ?
	jne	SHORT of_chkOFF			;  N: check for OFF arg

	mov	ax, word ptr cs:[FarLink+2]
	mov	ds, ax
	assume	ds: R_CODE
	test	ds:[Current_State],fState_Active	; Q: are we active
	mov	ax, cs
	mov	ds, ax			; restore ds 
	assume	ds:LAST
	jnz	of_on				; Y: turn on
	
	smsw	ax
	test 	ax, MSW_PROTECT			; Q: is the mc in real mode
	jz	of_on				; Y: turn on
	mov	dx, offset OF_proterr		
	jmp	of_err_msg

of_on:	
	mov	ax,CEMMF_SETON			;  Y: turn CEMM ON
	call	ELIM_link			;Q: CEMM -> ON mode ?
	jnc	SHORT of_chkWeitek		;  Y: chk Weitek cmds
	mov	dx,offset OF_verror		;  N: report error and stop
	jmp	of_err_msg
of_chkOFF:
	cmp	[arg_flag+INDEX_OFF],TRUE	;Q: OFF arg specified ?
	jne	SHORT of_chkAUTO		;  N: check for AUTO arg
	mov	[OFF_mode],0FFh			;  Y: set OFF flag & do OFF cmd
	mov	ax,CEMMF_SETOFF			; turn CEMM OFF
	call	ELIM_link			;Q: CEMM -> OFF mode ?
	jnc	SHORT of_chkWeitek		;  Y: chk Weitek cmds
	mov	dx, offset OF_rerr		;  N: report error and stop
	jmp	of_err_msg
of_chkAUTO:
	cmp	[arg_flag+INDEX_AUTO],TRUE	;Q: AUTO arg ?
	jne	SHORT of_chkWeitek		;  N: chk Weitek args
	mov	ax,CEMMF_SETAUTO		;  Y: CEMM to AUTO mode
	call	ELIM_link			;Q: CEMM -> AUTO mode ?
	jnc	SHORT of_chkWeitek		;  Y: chk Weitek cmds
	mov	dx,offset OF_aerr		;  N: report error and stop
	jmp	of_err_msg

;
; check for Weitek commands
;
of_chkWeitek:
	test	[arg_state],fWeitek_Arg		;Q: any Weitek arg ?
	jz	SHORT of_status			;  N: display current status
	mov	ax,CEMMF_SENSEW			;  Y: chk for Weitek installed
	call	ELIM_link
	test	al,1				;Q: Weitek installed ?
	jnz	SHORT of_chkWON			;  Y: chk Weitek arg
	mov	dx,offset OF_w_not_inst		;  N: Weitek not installed msg
	jmp	of_err_msg			;     & leave
of_chkWON:
	cmp	[arg_flag+INDEX_WON],TRUE	;Q: Weitek support ON ?
	jne	SHORT of_WOFF			;  N: must be OFF flag
	mov	ax,CEMMF_SETWON			;  Y: turn in on
	call	ELIM_link			;Q: Weitek support ON ?
	jnc	SHORT of_status			;  Y: display status
	mov	dx,offset OF_won_err		;  N: report error and stop
	jmp	SHORT of_err_msg
of_WOFF:
	mov	ax,CEMMF_SETWOFF
	call	ELIM_link		;Q: Weitek support OFF ?
	jnc	SHORT of_status		;  Y: display status
	mov	dx,offset OF_woff_err	;  N: report error and stop
	jmp	SHORT of_err_msg

;
; return current status of CEMM
;
of_status:
	xor	ah,ah			; get status
	call	ELIM_link		; status in ah
	mov	al,ah
	xor	ah,ah			; status in ax
	cmp	ax,1			; q: OFF (no auto) ?
	jne	SHORT chk_auto		;   n: check for auto mode
	mov	[OFF_mode],0FFh		;   y: set flag
chk_auto:
	cmp	ax,2			; q: auto mode?
	jb	SHORT mode_msg_disp	;   n: display mode
	push	ax			; save current mode
	mov	dx,offset OF_amode
	mov	ah,9
	int	MSDOS			; print auto mode
	pop	ax			; restore mode
	sub	ax,2			; get on or off indicator
mode_msg_disp:
	shl	ax,1
	mov	di,ax
	mov	dx,mode_msg_tbl[di]		; DS:DX -> status message
	mov	ah,9
	int	MSDOS			; display message
;
	cmp	[OFF_mode],0		; q: OFF mode ?
	je	SHORT w_status		; n: display weitek status
	mov	dx,offset OF_inaccess	; y: display inaccessible message also
	mov	ah,9
	int	MSDOS
	;
	; now display Weitek state msg
	;
w_status:
	mov	ax,CEMMF_SENSEW		;check for Weitek installed
	call	ELIM_link
	test	al,1			;Q: Weitek installed ?
	jz	SHORT exit		;  N: no message
					;  Y: display current Weitek state
	mov	dx,offset OF_won_mode	; assume Weitek support on
	test	al,2			;Q: Weitek support on ?
	jnz	SHORT disp_wmode	;  Y: display on msg
	mov	dx,offset OF_woff_mode	;  N: display off msg
disp_wmode:
	mov	ah,9
	int	MSDOS			; display message

	cmp	[OFF_mode],0		;Q: OFF mode ?
	je	SHORT exit		; N: leave
	mov	dx,offset OF_w_inaccess	; Y: display inaccessible message also
	mov	ah,9
	int	MSDOS
;
; leave onf
;
exit:
	pop	ds
	pop	di
	pop	dx
	pop	ax
	ret
;
; here for invalid args.  display usage message and leave
;

.8086
of_usage:
	mov	dx,offset OF_parmerr	; USAGE message
of_err_msg:
	mov	ah,9
	int	MSDOS			; display message
	jmp	exit			; leave

of_invmsg:
	mov	dx, offset OF_invparm	
	mov	ah, 9
	int	MSDOS

	mov	byte ptr es:[di], 0dh
	mov	byte ptr es:[di+1], 0ah
	mov	byte ptr es:[di+2], '$'
	sub	di, cx
	mov	dx, di
	push	es
	pop	ds
	mov	ah, 9
	int	MSDOS
	jmp	exit

	
onf_func	endp

	page
;******************************************************************************
;	parse_args - Parse command line for args
;
;	ENTRY: ES:DI points to command line terminated by CR or LF
;
;	EXIT:   DS -> CS
;		CS:state = flags set here
;		CS:arg_flags = flags set here for args found
;
;	      ES:DI points to end of parsed string
;
;	USED: flags
;
;******************************************************************************
parse_args 	proc		near
;

.386p
	push	bx
	push	si

;
; get a token from command line
;
pa_loop:
	mov	si,offset arg_str	; DS:SI = storage for argument
	mov	cx,max_arg_len		; maximum argument length
	call	get_token		; get an argument
	or	cx,cx			;Q: token found?
	jz	SHORT pa_exit		; N: quit
	or	[arg_state],fArg_Found	; Y: ok, look for it in parm table
;
; look for it in table of valid args...
;
	call	chk_token		;Q: valid arg ?
	jc	SHORT pa_invprm		;  N: set error and leave
	mov	arg_flag[bx],TRUE	;  Y: set flag for this arg

;
; check for multiple/inconsistent args
;
	cmp	bx,INDEX_AUTO		;Q: mode argument ?
	ja	SHORT pa_chkwa		;  N: check for Weitek arg
	test	[arg_state],fMode_Arg	;  Y: Q: mode arg already given ?
	jnz	SHORT pa_invprm		;       Y: invalid parameter
	or	[arg_state],fMode_Arg	;       N: set flag
	jmp	SHORT pa_next
pa_chkwa:
	cmp	bx,INDEX_WOFF		;Q: Weitek argument ?
	ja	SHORT pa_next		;  N: this arg is OK ...
	test	[arg_state],fWeitek_Arg	;  Y: Q: Weitek arg already given ?
	jnz	SHORT pa_invprm		;       Y: invalid parameter
	or	[arg_state],fWeitek_Arg	;       N: set flag

pa_next:
	jmp	pa_loop		; look for next arg

pa_exit:
	pop	si
	pop	bx
	ret

pa_invprm:
	or	[arg_state],fParmError	; set error flag
	jmp	pa_exit			; and leave
parse_args	endp

	page
;******************************************************************************
;	chk_token - is this a valid token/argument ?
;
;	ENTRY:  DS:SI points to token from command line
;		CX = length of token
;
;	EXIT:  if token found,
;		BX = offset in argument table
;		CLC
;	       else,
;		STC
;
;	USED: BX,flags
;
;******************************************************************************
chk_token	proc	near
	push	di
	push	es
	mov	bx,cs
	mov	es,bx		;  ES -> local data
;
	xor	bx,bx		; index into parameter table
ct_loop:
	cmp	cx,arg_len[bx]	;Q: lengths equal?
	jne	SHORT ct_notyet	; N: keep looking
	mov	di,arg_tbl[bx]	; Y: get destination address
	push	si		; save source string addr
	cld			;   foward
	push	cx		;   save arg length
	repe	cmpsb		;Q: is this a valid argument?
	pop	cx		;   restore arg length
	pop	si		;     restore source string address (command line)
	je	SHORT ct_found 	; Y: matched one
ct_notyet:			; N: try next arg in table
	inc	bx
	inc	bx		; update table pointer
	cmp	bx,max_args	;Q: have we done them all yet?
	jne	ct_loop		;  N: keep looking
	stc			;  Y: return not found
	jmp	SHORT ct_exit
;
ct_found:
	clc
ct_exit:
	pop	es
	pop	di
	ret
chk_token	endp

	page
;******************************************************************************
;	get_token - Retrieve a non-white-space string from a source string
;
;	ENTRY: es:di points to command line terminated by CR or LF
;	       ds:si points to storage for token
;	       cx = maximum length to store
;
;	EXIT: cx = length of token (0 => end of source string)
;	      es:di points to first char after new token in source string
;	      string of length cx stored in ds:si (and converted to lower case)
;
;	USED: see above
;
;******************************************************************************
get_token 	proc		near
	push	si		; save storage area
	push	bx
	push	ax
;
	cld
;
	mov	bx,cx		; number to store
	xor	cx,cx		; no chars found so far
;
; go to first non-blank character
;
gloop1:
	mov	al,es:[di]	; get a character
	inc	di		; point to next
	cmp	al,' '		; Q: space ?
	je	gloop1		; y: skip it
	cmp	al,TAB		; Q: TAB ?
	je	gloop1		; y: skip it
	dec	di		; N: start parsing and reset di
gloop2:
	mov	al,es:[di]	; get next char
	cmp	al,CR		; q: carriage return?
	je	SHORT token_xit	; y: quit
	cmp	al,LF		; q: line feed?
	je	SHORT token_xit	; y: quit
	cmp	al,' '		; Q: space ?
	je	SHORT token_xit	; y: quit
	cmp	al,TAB		; Q: TAB ?
	je	SHORT token_xit	; y: quit
	inc	di		; n: point to next
	inc	cx		; increment number of chars found
	cmp	cx,bx		; q: have we stored our limit yet?
	ja	gloop2		; y: don't store any more
	cmp	al,'A'		;Q: upper case ?
	jb	SHORT token_storeit	;  N: store it now
	cmp	al,'Z'		;  Y: maybe Q: Upper case ?
	ja	SHORT token_storeit	;     N: no conversion
	or	al,20h		;             Y: make it lower case
token_storeit:
	mov	ds:[si],al	; store it
	inc	si		; and point to next
	jmp	short gloop2	; continue
token_xit:
;
	pop	ax
	pop	bx
	pop	si
	ret
get_token	endp


LAST	ends

	end
