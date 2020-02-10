	page	,160
;
;       Microsoft Confidential
;       Copyright (C) Microsoft Corporation 1981-1991
;       All Rights Reserved.
;

        title   bios system initialization

;
; Multiple configuration block support  Created 16-Mar-1992 by JeffPar
;
; Summary:
;
;   The procedure "organize" crunches the in-memory copy of config.sys
;   into lines delimited by CR/LF (sometimes no CR, but *always* an LF)
;   with the leading "keyword=" replaced by single character codes (eg, B
;   for BUFFERS, D for DEVICE, Z for any unrecognized keyword);  see comtab
;   and/or config.inc for the full list.
;
;   [blockname] and INCLUDE are the major syntactical additions for multi-
;   configuration support.  blockname is either MENU, which contains one
;   or more MENUITEM lines, an optional MENUDEFAULT (which includes optional
;   time-out), or any user-defined keyword, such as NETWORK, CD-ROM, etc.
;   INCLUDE allows the current block to name another block for inclusion
;   during the processing phase of CONFIG.SYS.  An INCLUDE is only honored
;   once, precluding nasty infinite-loop scenarios.  If blocks are present
;   without a MENU block, then only lines inside COMMON blocks are processed.
;
; Example:
;
;   [menu]
;   menuitem=misc,Miscellaneous
;   menuitem=network,Network Configuration
;   menudefault=network,15
;
;   [network]
;   include misc
;   device=foo
;
;   [misc]
;   device=bar
;   include alternate
;
;   [alternate]
;   device=tar
;
;
;   When the menu is displayed
;
;    1. Miscellaneous
;    2. Network Configuration
;
;   #2 is highlighted as the default option, and will be automatically
;   selected after 15 seconds.  It will invoke the following lines in the
;   following order:
;
;       DEVICE=BAR
;       DEVICE=TAR
;       DEVICE=FOO
;

MULTI_CONFIG equ 1

	.xlist
	include version.inc		; set version build flags
	include biosseg.inc		; establish bios segment structure
	.list

lf	equ	10
cr	equ	13
tab	equ	9

; the following depends on the positions of the various letters in switchlist

switchnum	equ 11111000b		; which switches require number

	.xlist
	include	bpb.inc
	include syscall.inc
	include doscntry.inc
	include devsym.inc
	include ioctl.inc
        include devmark.inc
        include config.inc
ifdef   MULTI_CONFIG
        include rombios.inc
endif
	.list

stacksw equ     true                    ; include switchable hardware stacks

	if	ibmjapver
noexec	equ	true
	else
noexec	equ	false
	endif


Bios_Data segment 
	extrn eot:byte
	extrn ec35_flag: byte
Bios_Data ends

sysinitseg segment public

assume	cs:sysinitseg,ds:nothing,es:nothing,ss:nothing

ifdef   MULTI_CONFIG

        MAX_MULTI_CONFIG equ 9          ; max # of multi-config menu items supported

        extrn   config_envlen:word
        extrn   config_wrkseg:word
        extrn   config_cmd:byte
        extrn   config_multi:byte
        extrn   OnOff:byte
        extrn   $PauseMsg:byte
        extrn   $CleanMsg:byte
        extrn   $InterMsg:byte
        extrn   $MenuHeader:byte
        extrn   $MenuPrmpt:byte
        extrn   $StatusLine:byte
        extrn   $InterPrmpt:byte
        extrn   $YES:byte
        extrn   $NO:byte
        extrn   $TimeOut:byte
        extrn   $AutoPrmpt:byte
        extrn   swit_n:byte, swit_f:byte

        public  newcmd,tmplate,commnd2,commnd3,commnd4
endif

IFDEF	CONFIGPROC

	extrn	badopm:byte,badcom:byte,badblock:byte
	extrn	badsiz_pre:byte,
	extrn	error_line:near

	extrn	dosinfo:dword
	extrn	memory_size:word,fcbs:byte,keep:byte
	extrn	default_drive:byte,confbot:word
	extrn	buffers:word,zero:byte,sepchr:byte
	extrn	files:byte
	extrn	count:word,chrptr:word
	extrn	bufptr:byte,prmblk:byte
	extrn	ldoff:word,area:word,packet:byte,unitcount:byte,
	extrn	break_addr:dword,bpb_addr:dword,drivenumber:byte
	extrn	com_level:byte, cmmt:byte, cmmt1:byte, cmmt2:byte
	extrn	cmd_indicator:byte
	extrn	multdeviceflag:byte
	extrn	org_count:word

	extrn	getchr:near
	extrn	DevEntry:dword

	insert_blank	db	0	; M051: indicates that blank has been
					; M051: inserted 
ENDIF ; CONFIGPROC

	extrn	memlo:word,memhi:word,alloclim:word
	extrn	devmark_addr:word,badmem:byte,crlfm:byte
	extrn	setdevmarkflag:byte,badld_pre:byte

	extrn	donotshownum:byte
	extrn	pararound:near
	extrn	stall:near


        public  int24,open_dev,mem_err,prndev,auxdev,condev
        public  commnd,command_line
	public	badfil,round,print

IFDEF	CONFIGPROC

	public	organize,newline,calldev,badload
	public	config,getnum,prnerr
	public	delim
	public	setparms, parseline, diddleback
	public	setdoscountryinfo,set_country_path,move_asciiz
	public	cntry_drv,cntry_root,cntry_path
	public	pathstring

;
;----------------------------------------------------------------------------
;
; procedure : setparms
;
; the following set of routines is used to parse the drivparm = command in
; the config.sys file to change the default drive parameters.
;
;----------------------------------------------------------------------------
;
setparms	proc	near

	push	ds
	push	ax
	push	bx
	push	cx
	push	dx

	push	cs
	pop	ds
	assume	ds:sysinitseg

	xor	bx,bx
	mov	bl,byte ptr drive
	inc	bl			; get it correct for ioctl
					;  call (1=a,2=b...)
	mov	dx,offset deviceparameters
	mov	ah,ioctl
	mov	al,generic_ioctl
	mov	ch,rawio
	mov	cl,set_device_parameters
	int	21h

	mov	ax,Bios_Data		; get Bios_Data segment
	mov	ds,ax			; set Bios_Data segment
	assume	ds:Bios_Data

	test	cs:word ptr switches, flagec35
	jz	not_ec35

	mov	cl,cs:byte ptr drive	; which drive was this for?

	mov	al,1			; assume drive 0
	shl	al,cl			; set proper bit depending on drive
	or	ds:ec35_flag,al 	; set the bit in the permanent flags

not_ec35:

;	Now adjust the BIOS's EOT variable if our new drive has more
;	sectors per track than any old ones.

	mov	al,cs:byte ptr deviceparameters.DP_BPB.BPB_SECTORSPERTRACK
	cmp	al,eot
	jbe	eot_ok
	mov	eot,al
eot_ok:

	pop	dx			; fix up all the registers
	pop	cx
	pop	bx
	pop	ax
	pop	ds
	assume	ds:nothing
	ret

setparms	endp

;
;----------------------------------------------------------------------------
;
; procedure : diddleback
;
; replace default values for further drivparm commands
;
;----------------------------------------------------------------------------
;

diddleback	proc	near

	push	ds
	push	cs
	pop	ds
	assume	ds:sysinitseg
	mov	word ptr deviceparameters.dp_cylinders,80
	mov	byte ptr deviceparameters.dp_devicetype, dev_3inch720kb
	mov	word ptr deviceparameters.dp_deviceattributes,0
	mov	word ptr switches,0	    ; zero all switches
	pop	ds
	assume	ds:nothing
	ret

diddleback	endp

;
;----------------------------------------------------------------------------
;
; procedure : parseline
;
; entry point is parseline. al contains the first character in command line.
;
;----------------------------------------------------------------------------
;

parseline	proc	near
					; don't get character first time
	push	ds

	push	cs
	pop	ds
	assume	ds:sysinitseg

nextswtch:
	cmp	al,cr			; carriage return?
	jz	done_line
	cmp	al,lf			; linefeed?
	jz	put_back		; put it back and done

; anything less or equal to a space is ignored.

	cmp	al,' '                  ; space?
	jbe	get_next		; skip over space
	cmp	al,'/'
	jz	getparm
	stc				; mark error invalid-character-in-input
	jmp	short exitpl

getparm:
	call	check_switch
	mov	word ptr switches,bx	; save switches read so far
	jc	swterr

get_next:
	call	getchr
	jc	done_line
	jmp	nextswtch

swterr:
	jmp	short exitpl		; exit if error

done_line:
	test	word ptr switches,flagdrive  ; see if drive specified
	jnz	okay
	stc				; mark error no-drive-specified
	jmp	short exitpl

okay:
	mov	ax,word ptr switches
	and	ax,0003h	    ; get flag bits for changeline and non-rem
	mov	word ptr deviceparameters.dp_deviceattributes,ax
	mov	word ptr deviceparameters.dp_tracktableentries, 0
	clc			    ; everything is fine
	call	setdeviceparameters
exitpl:
	pop	ds
	ret

put_back:
	inc	count			; one more char to scan
	dec	chrptr			; back up over linefeed
	jmp	short done_line

parseline	endp

;
;----------------------------------------------------------------------------
;
; procedure : check_switch
;
; processes a switch in the input. it ensures that the switch is valid, and
; gets the number, if any required, following the switch. the switch and the
; number *must* be separated by a colon. carry is set if there is any kind of
; error.
;
;----------------------------------------------------------------------------
;

check_switch	proc	near

	call	getchr
	jc	err_check
        and     al,0dfh                 ; convert it to upper case
	cmp	al,'A'
	jb	err_check
	cmp	al,'Z'
	ja	err_check

	push	es

	push	cs
	pop	es

	mov	cl,byte ptr switchlist	; get number of valid switches
	mov	ch,0
	mov	di,1+offset switchlist	; point to string of valid switches
	repne	scasb

	pop	es
	jnz	err_check

	mov	ax,1
	shl	ax,cl			; set bit to indicate switch
	mov	bx,word ptr switches	; get switches so far
	or	bx,ax			; save this with other switches
	mov	cx,ax
	test	ax, switchnum		; test against switches that require number to follow
	jz	done_swtch

	call	getchr
	jc	err_swtch

	cmp	al,':'
	jnz	err_swtch

	call	getchr
	push	bx			; preserve switches
	mov	byte ptr cs:sepchr,' '	; allow space separators
	call	getnum
	mov	byte ptr cs:sepchr,0
	pop	bx			; restore switches

; because getnum does not consider carriage-return or line-feed as ok, we do
; not check for carry set here. if there is an error, it will be detected
; further on (hopefully).

	call	process_num

done_swtch:
	clc
	ret

err_swtch:
	xor	bx,cx			; remove this switch from the records
err_check:
	stc
	ret

check_switch	endp

;
;----------------------------------------------------------------------------
;
; procedure : process_num
;
; this routine takes the switch just input, and the number following (if any),
; and sets the value in the appropriate variable. if the number input is zero
; then it does nothing - it assumes the default value that is present in the
; variable at the beginning. zero is ok for form factor and drive, however.
;
;----------------------------------------------------------------------------
;

process_num	proc	near
	test	word ptr switches,cx	; if this switch has been done before,
	jnz	done_ret		; ignore this one.
	test	cx,flagdrive
	jz	try_f
	mov	byte ptr drive,al
	jmp	short done_ret

try_f:
	test	cx,flagff
	jz	try_t

; ensure that we do not get bogus form factors that are not supported

	mov	byte ptr deviceparameters.dp_devicetype,al
	jmp	short done_ret

try_t:
	or	ax,ax
	jz	done_ret		; if number entered was 0, assume default value
	test	cx,flagcyln
	jz	try_s

	mov	word ptr deviceparameters.dp_cylinders,ax
	jmp	short done_ret

try_s:
	test	cx,flagseclim
	jz	try_h
	mov	word ptr slim,ax
	jmp	short done_ret

; must be for number of heads

try_h:
	mov	word ptr hlim,ax

done_ret:
	clc
	ret

process_num	endp

;	M047 -- Begin modifications (too numerous to mark specifically)
;
;----------------------------------------------------------------------------
;
; procedure : setdeviceparameters
;
; setdeviceparameters sets up the recommended bpb in each bds in the
; system based on the form factor. it is assumed that the bpbs for the
; various form factors are present in the bpbtable. for hard files,
; the recommended bpb is the same as the bpb on the drive.
; no attempt is made to preserve registers since we are going to jump to
; sysinit straight after this routine.
;
;	if we return carry, the DRIVPARM will be aborted, but presently
;	  we always return no carry
;
;
;	note:  there is a routine by the same name in msdioctl.asm
;
;----------------------------------------------------------------------------
;

setdeviceparameters	proc	near

	push	es

	push	cs
	pop	es
	assume	es:sysinitseg

	xor	bx,bx
	mov	bl,byte ptr deviceparameters.dp_devicetype
	cmp	bl,dev_5inch
	jnz	got_80

	mov	word ptr deviceparameters.dp_cylinders,40 	; 48 tpi=40 cyl

got_80:
	shl	bx,1			; get index into bpb table
	mov	si,bpbtable[bx]		; get address of bpb

	mov	di,offset deviceparameters.dp_bpb ; es:di -> bpb
	mov	cx,size A_BPB
	cld
	repe	movsb

	pop	es
	assume es:nothing

	test	word ptr switches,flagseclim
	jz	see_heads

	mov	ax,word ptr slim
	mov	word ptr deviceparameters.dp_bpb.bpb_sectorspertrack,ax

see_heads:
	test	word ptr switches,flagheads
	jz	heads_not_altered

	mov	ax,word ptr hlim
	mov	word ptr deviceparameters.dp_bpb.bpb_heads,ax

heads_not_altered:


; set up correct media descriptor byte and sectors/cluster
;   sectors/cluster is always 2 except for any one sided disk or 1.44M

	mov	byte ptr deviceparameters.dp_bpb.bpb_sectorspercluster,2
	mov	bl,0f0h			; get default mediabyte

;	preload the mediadescriptor from the bpb into bh for convenient access

	mov	bh,byte ptr deviceparameters.dp_bpb.bpb_mediadescriptor

	cmp	word ptr deviceparameters.dp_bpb.bpb_heads,2	; >2 heads?
	ja	got_correct_mediad	; just use default if heads>2

	jnz	only_one_head		; one head, do one head stuff

;	two head drives will use the mediadescriptor from the bpb

	mov	bl,bh			; get mediadescriptor from bpb

;	two sided drives have two special cases to look for.  One is
;	   a 320K diskette (40 tracks, 8 secs per track).  It uses
;	   a mediaid of 0fch.  The other is 1.44M, which uses only
;	   one sector/cluster.

;	any drive with 18secs/trk, 2 heads, 80 tracks, will be assumed
;	   to be a 1.44M and use only 1 sector per cluster.  Any other
;	   type of 2 headed drive is all set.

	cmp	deviceparameters.dp_bpb.bpb_sectorspertrack,18
	jnz	not_144m
	cmp	deviceparameters.dp_cylinders,80
	jnz	not_144m

;	We've got cyl=80, heads=2, secpertrack=18.  Set cluster size to 1.

	jmp	short got_one_secperclus_drive


;	check for 320K

not_144m:
	cmp	deviceparameters.dp_cylinders,40
	jnz	got_correct_mediad
	cmp	deviceparameters.dp_bpb.bpb_sectorspertrack,8
	jnz	got_correct_mediad

	mov	bl,0fch
	jmp	short got_correct_mediad


only_one_head:

;	if we don't have a 360K drive, then just go use 0f0h as media descr.

	cmp	deviceparameters.dp_devicetype,dev_5inch
	jnz	got_one_secperclus_drive

;	single sided 360K drive uses either 0fch or 0feh, depending on
;	  whether sectorspertrack is 8 or 9.  For our purposes, anything
;	  besides 8 will be considered 0fch

	mov	bl,0fch			; single sided 9 sector media id
	cmp	word ptr deviceparameters.dp_bpb.bpb_sectorspertrack,8
	jnz	got_one_secperclus_drive ; okay if anything besides 8

	mov	bl,0feh			; 160K mediaid

;	we've either got a one sided drive, or a 1.44M drive
;	  either case we'll use 1 sector per cluster instead of 2

got_one_secperclus_drive:
	mov	byte ptr deviceparameters.dp_bpb.bpb_sectorspercluster,1

got_correct_mediad:
	mov	byte ptr deviceparameters.dp_bpb.bpb_mediadescriptor,bl


;	 Calculate the correct number of Total Sectors on medium
;
	mov	ax,deviceparameters.dp_cylinders
	mul	word ptr deviceparameters.dp_bpb.bpb_heads
	mul	word ptr deviceparameters.dp_bpb.bpb_sectorspertrack
	mov	word ptr deviceparameters.dp_bpb.bpb_totalsectors,ax
	clc				; we currently return no errors

	ret

setdeviceparameters	endp

;	M047 -- end rewritten routine

;
;----------------------------------------------------------------------------
;
; procedure : organize
;
;----------------------------------------------------------------------------
;
		assume ds:nothing, es:nothing
organize	proc	near

	mov	cx,[count]
	jcxz	nochar1

ifndef  MULTI_CONFIG
;
;   In MULTI_CONFIG, we map to upper case on a line-by-line basis,
;   because we the case of values in SET commands preserved
;
	call	mapcase
endif
	xor	si,si
	mov	di,si
	xor	ax,ax
	mov	com_level, 0

org1:
	call	skip_comment
	jz	end_commd_line		; found a comment string and skipped.
	call	get2			; not a comment string. then get a char.
	cmp	al, lf
	je	end_commd_line		; starts with a blank line.
	cmp	al, ' '
	jbe	org1			; skip leading control characters
	jmp	short findit

end_commd_line:
	stosb				; store line feed char in buffer for the linecount.
	mov	com_level, 0		; reset the command level.
	jmp	org1

nochar1:
	stc
	ret

findit:
	push	cx
	push	si
	push	di
	mov	bp,si
	dec	bp
        mov     si,offset comtab        ; prepare to search command table
	mov	ch,0
findcom:
	mov	di,bp
	mov	cl,[si]
	inc	si
	jcxz	nocom

ifdef   MULTI_CONFIG
;
;   Simplify future parsing by collapsing ";" onto "REM", and at the same
;   time skip the upcoming delimiter test (since ";" need not be followed by
;   anything in particular)
;
        cmp     byte ptr es:[di],CONFIG_SEMICOLON
        je      semicolon
loopcom:
        mov     al,es:[di]
        inc     di
        and     al,not 20h              ; force upper case
        inc     si                      ; compare to byte @es:di
        cmp     al,ds:[si-1]
        loope   loopcom
else
	repe	cmpsb
endif
	lahf
        add     si,cx                   ; bump to next position without affecting flags
	sahf
        lodsb                           ; get indicator letter
	jnz	findcom
        cmp     byte ptr es:[di], cr    ; the next char might be cr,lf
	je	gotcom0 		; such as in "rem",cr,lf case.
	cmp	byte ptr es:[di], lf
	je	gotcom0

ifdef   MULTI_CONFIG
;
;   Skip the delimiter test for the BEGIN identifier (it doesn't have one).
;
        cmp     al,CONFIG_BEGIN
        je      gotcom0
endif
	push	ax
        mov     al, byte ptr es:[di]    ; now the next char. should be a delim.

ifdef   MULTI_CONFIG
;
;   If keyword is *immediately* followed by a question mark (?), then
;   set the high bit of the ASCII command code (CONFIG_OPTION_QUERY) that is
;   stored in the CONFIG.SYS memory image.
;
        cmp     al,'?'                  ; explicit interactive command?
        jne     no_query                ; no
        pop     ax                      ; yes, so retrieve the original code
        or      al,CONFIG_OPTION_QUERY  ; and set the QUERY bit
        jmp     short gotcom0           ;
semicolon:
        mov     al,CONFIG_REM
        jmp     short gotcom0
no_query:
endif  ;MULTI_CONFIG

	call	delim
no_delim:
	pop	ax
	jnz	findcom
gotcom0:
	pop	di
	pop	si
	pop	cx
	jmp	short gotcom

nocom:
	pop	di
	pop	si
	pop	cx
        mov     al,CONFIG_UNKNOWN
	stosb				; save indicator char.
skipline:
	call	get2
	cmp	al, lf			; skip this bad command line
        jne     skipline
	jmp	end_commd_line		; handle next command line

gotcom:
        stosb                           ; save indicator char in buffer

ifdef   MULTI_CONFIG
;
;   Don't pollute "cmd_indicator" with the CONFIG_OPTION_QUERY bit though;
;   it screws up the direct comparisons below.
;
        and     al,NOT CONFIG_OPTION_QUERY
endif
	mov	cmd_indicator, al	; save it for the future use.

ifdef   MULTI_CONFIG
;
;   There is no whitespace/delimiter between the "begin block" character
;   ([) and the name of block (eg, [menu]), therefore skip this delimiter
;   skipping code
;
        cmp     al,CONFIG_BEGIN
        je      org31
        cmp     al,CONFIG_SUBMENU
        je      no_mapcase
        cmp     al,CONFIG_MENUITEM
        je      no_mapcase
        cmp     al,CONFIG_MENUDEFAULT
        je      no_mapcase
        cmp     al,CONFIG_INCLUDE
        je      no_mapcase
        call    mapcase                 ; map case of rest of line to UPPER
no_mapcase:
endif

org2:	call    get2                    ; skip the command name until delimiter
        cmp     al, lf
	je	org21
	cmp	al, cr
	je	org21
	cmp	al, '/'			; T-RICHJ: Added to allow DEVHIGH/L:...
	je	org21			; T-RICHJ: to be parsed properly.

	call	delim
        jnz     org2
	jmp	short org3

org21:					;if cr or lf then
	dec	si			; undo si, cx register
	inc	cx			;  and continue

org3:	cmp     cmd_indicator, CONFIG_COMMENT
	je	get_cmt_token
        cmp     cmd_indicator, CONFIG_DEVICE
	je	org_file
        cmp     cmd_indicator, CONFIG_INSTALL
	je	org_file
        cmp     cmd_indicator, CONFIG_INSTALLHIGH
        je      org_file
        cmp     cmd_indicator, CONFIG_SHELL
	je	org_file
        cmp     cmd_indicator, CONFIG_SWITCHES
	je	org_switch
org31:
	jmp	org4

org_switch:
	call	skip_comment
	jz	end_commd_line_brdg

	call	get2
	call	org_delim
	jz	org_switch

	stosb
	jmp	org5

org_file:			; get the filename and put 0 at end,
	call	skip_comment
	jz	org_put_zero

	call	get2		; not a comment
	call	delim
	jz	org_file	; skip the possible delimeters

	stosb			; copy the first non delim char found in buffer

org_copy_file:
	call	skip_comment	; comment char in the filename?
	jz	org_put_zero	; then stop copying filename at that point

	call	get2
	cmp	al, '/' 	; a switch char? (device=filename/xxx)
	je	end_file_slash	; this will be the special case.

	stosb			; save the char. in buffer
	call	delim
	jz	end_copy_file

	cmp	al, ' '
	ja	org_copy_file	; keep copying
	jmp	short end_copy_file ; otherwise, assume end of the filename.

get_cmt_token:			; get the token. just max. 2 char.
	call	get2
	cmp	al, ' ' 	; skip white spaces or "=" char.
	je	get_cmt_token	; (we are allowing the other special
	cmp	al, tab 	;  charaters can used for comment id.
	je	get_cmt_token	;  character.)
	cmp	al, '=' 	; = is special in this case.
	je	get_cmt_token
	cmp	al, cr
	je	get_cmt_end	; cannot accept the carridge return
	cmp	al, lf
	je	get_cmt_end

	mov	cmmt1, al	; store it
	mov	cmmt, 1 	; 1 char. so far.
	call	get2
	cmp	al, ' '
	je	get_cmt_end
	cmp	al, tab
	je	get_cmt_end
	cmp	al, cr
	je	get_cmt_end
	cmp	al, lf
	je	end_commd_line_brdg

	mov	cmmt2, al
	inc	cmmt

get_cmt_end:
	call	get2
	cmp	al, lf
	jne	get_cmt_end		; skip it.

end_commd_line_brdg: jmp end_commd_line ; else jmp to end_commd_line

org_put_zero:				; make the filename in front of
	mov	byte ptr es:[di], 0	;  the comment string to be an asciiz.
	inc	di
	jmp	end_commd_line		;  (maybe null if device=/*)

end_file_slash: 			; al = "/" option char.
	mov	byte ptr es:[di],0	; make a filename an asciiz
	inc	di			; and
	stosb				; store "/" after that.
	jmp	short org5		; continue with the rest of the line

end_copy_file:
	mov	byte ptr es:[di-1], 0	; make it an asciiz and handle the next char.
	cmp	al, lf
	je	end_commd_line_brdg
	jmp	short org5

org4:					; org4 skips all delimiters after the command name except for '/'
	call	skip_comment
	jz	end_commd_line_brdg

	call	get2
	call	org_delim		; skip delimiters except '/' (mrw 4/88)
	jz	org4
	jmp	short org51

org5:					; rest of the line
	call	skip_comment		; comment?
	jz	end_commd_line_brdg
	call	get2			; not a comment.

org51:
	stosb				; copy the character
	cmp	al, '"' 		; a quote ?
	je	at_quote
	cmp	al, ' '
	ja	org5
					; M051 - Start

        cmp     cmd_indicator, CONFIG_DEVICEHIGH; Q: is this devicehigh
	jne	not_dh			; N: 
	cmp	al, lf			; Q: is this line feed
	je	org_dhlf		; Y: stuff a blank before the lf
	cmp	al, cr			; Q: is this a cr
	jne	org5			; N: 
	mov	byte ptr es:[di-1], ' '	; overwrite cr with blank
	stosb				; put cr after blank
	inc	[insert_blank]		; indicate that blank has been 
					; inserted
	jmp	org5
not_dh:					; M051 - End

	cmp	al, lf			; line feed?
	je	org1_brdg		; handles the next command line.
	jmp	org5			; handles next char in this line.

org_dhlf:				; M051 - Start
	cmp	[insert_blank], 1	; Q:has a blank already been inserted
	je	org1_brdg		; Y:
	mov	byte ptr es:[di-1], ' '	; overwrite lf with blank
	stosb				; put lf after blank
					; M051 - End

org1_brdg: 
	mov	[insert_blank], 0	; M051: clear blank indicator for 
					; M051: devicehigh
	jmp	org1

at_quote:
	cmp	com_level, 0
	je	up_level
	mov	com_level, 0		; reset it.
	jmp	org5

up_level:
	inc	com_level		; set it.
	jmp	org5

organize	endp

;
;----------------------------------------------------------------------------
;
; procedure : get2
;
;----------------------------------------------------------------------------
;
get2	proc	near
	jcxz	noget
        lods    byte ptr es:[si]
	dec	cx
	ret
noget:
	pop	cx
	mov	count,di
	mov	org_count, di
	xor	si,si
	mov	chrptr,si
ifndef  MULTI_CONFIG
	ret
else
;
;   This was the rather kludgy way out of procedure "organize", but instead
;   of returning to doconf, we now want to check config.sys BEGIN/END blocks
;   and the new boot menu stuff...
;
        mov     cx,di
        jmp     menu_check
endif
get2	endp


;
;----------------------------------------------------------------------------
;
; procedure : skip_comment
;
;skip the commented string until lf, if current es:si-> a comment string.
;in) es:si-> sting
;	 cx -> length.
;out) zero flag not set if not found a comment string.
;	  zero flag set if found a comment string and skipped it. al will contain
;	  the line feed charater at this moment when return.
;	  ax register destroyed.
;	  if found, si, cx register adjusted accordingly.
;
;----------------------------------------------------------------------------
;
skip_comment	proc	near

	jcxz	noget		; get out of the organize routine.
	cmp	com_level, 0	; only check it if parameter level is 0.
	jne	no_commt	;  (not inside quotations)

	cmp	cmmt, 1
	jb	no_commt

	mov	al, es:[si]
	cmp	cmmt1, al
	jne	no_commt

	cmp	cmmt, 2
	jne	skip_cmmt

	mov	al, es:[si+1]
	cmp	cmmt2, al
	jne	no_commt

skip_cmmt:
	jcxz	noget		; get out of organize routine.
	mov	al, es:[si]
	inc	si
	dec	cx
	cmp	al, lf		; line feed?
	jne	skip_cmmt

no_commt:
	ret

skip_comment	endp


ifdef   MULTI_CONFIG

        assume  ds:sysinitseg
;
;----------------------------------------------------------------------------
;
;   kbd_read: wait for keystroke
;
;   INPUT
;       DS == CS == sysinitseg
;
;   OUTPUT
;       Carry SET to clean boot, CLEAR otherwise
;
;   OTHER REGS USED
;       All
;
;   HISTORY
;       Created 16-Nov-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  kbd_read

kbd_read proc   near
        test    byte ptr [bDisableUI],2
        jnz     kbd_nodelay

        push    ds              ; the bios timer tick count is incremented
        sub     ax,ax           ; 18.2 times per second;
        mov     ds,ax           ; watch the timer tick count for 37 transitions
        mov     dx,ds:[046Ch]   ; get initial value
kbd_loop:
        mov     ah,1            ;
        int     16h             ; peek the keyboard
        jnz     kbd_loopdone    ; something's there, get out
        mov     ah,2            ; peek the shift states
        int     16h             ;
        test    al,03h          ; either right or left shift key bits set?
        jnz     kbd_loopdone    ; yes
        mov     ax,ds:[046Ch]   ;
        sub     ax,dx           ; get difference
        cmp     al,37           ; reached limit?
        jb      kbd_loop        ; not yet
kbd_loopdone:
        pop     ds              ; delay complete!

kbd_nodelay:
        sub     bx,bx           ; assume clean boot
        mov     ah,2            ; peek the shift states
        int     16h             ;
        test    al,03h          ; either right or left shift key bits set?
        jz      kbd_notshift    ; no
        inc     bx              ; yes
        inc     bx              ;
kbd_notshift:                   ;
        mov     ah,1            ; peek the keyboard
        int     16h             ;
        jz      kbd_test        ; no key present
        or      al,al           ; is it a function key?
        jnz     kbd_test        ; no
        cmp     ah,3Fh          ; F5 function key?
        jne     kbd_notf5       ; no
        mov     dx,offset $CleanMsg
        call    print           ;
        jmp     short kbd_eat   ; yes, clean boot selected
kbd_notf5:
        cmp     ah,42h          ; F8 function key?
        jne     kbd_exit        ; no
        mov     dx,offset $InterMsg
        call    print           ;
        mov     bl,1            ; yes, interactive-boot option enabled
        mov     [bQueryOpt],bl  ; change default setting
kbd_eat:                        ;
        mov     ah,0            ;
        int     16h             ; eat the key we assumed was a signal
        mov     byte ptr [secElapsed],-1
        or      bx,bx           ;
        jz      kbd_clean       ;
kbd_test:                       ;
        cmp     bl,2            ;
        jb      kbd_exit        ;
kbd_clean:                      ;
        call    disable_autoexec; yes, tell COMMAND to skip autoexec.bat
        stc                     ; set carry to indicate abort
        ret                     ;
kbd_exit:                       ;
        clc                     ; clear carry to indicate success
        ret                     ;
kbd_read endp

;
;----------------------------------------------------------------------------
;
;   set_numlock: set numlock LED
;
;   INPUT
;       ES:SI -> numlock setting (ie, "ON" or "OFF")
;
;   OUTPUT
;       None
;
;   OTHER REGS USED
;       None
;
;   HISTORY
;       Created 16-Nov-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  set_numlock

set_numlock proc near
        push    ax
        push    ds
        sub     ax,ax
        mov     ds,ax
        mov     ax,es:[si]      ; get 1st 2 bytes of value (ON or OF)
        cmp     ax,word ptr cs:[OnOff+2]; should we turn it off?
        jne     not_off         ; no
        and     byte ptr ds:[0417h],not 20h
        jmp     short set_done
not_off:
        cmp     ax,word ptr cs:[OnOff]  ; should we turn it on?
        stc
        jne     set_done        ; no
        or      byte ptr ds:[0417h],20h
set_done:
        pop     ds
        pop     ax
        ret
set_numlock endp

;
;----------------------------------------------------------------------------
;
;   menu_check:  check for presence of menu (and other) configuration blocks
;
;   INPUT
;       CX == "organized" config.sys memory image length
;    ES:SI -> "organized" config.sys memory image
;       DS == CS == sysinitseg
;
;   OUTPUT
;       Same as above;  the idea is that menu_check simply transforms
;       a block-structured config.sys image into a conventional image,
;       based on the user's block selection and any other boot-time options
;       the user may have employed...
;
;   OTHER REGS USED
;       All
;
;   NOTES
;       [count] and [org_count] are set to the new config.sys image length
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  menu_check

menu_check  proc near
;
;   Search for SWITCHES, determine if /N or /F are present;  if so, then
;   disable clean/interactive boot options
;
        push    cx              ;
        push    si              ;
        sub     bx,bx           ; remains ZERO until first block
swchk_loop:                     ;
        call    get_char        ; get first char of current line
        jc      swchk_end       ; hit eof
        cmp     al,CONFIG_BEGIN ;
        jne     swchk_next1     ;
        inc     bx              ; remember that we've seen a block
        jmp     short swchk_nextline
swchk_next1:                    ;
        cmp     al,CONFIG_NUMLOCK
        jne     swchk_next2     ;
        or      bx,bx           ; only do NUMLOCK commands that exist
        jnz     swchk_nextline  ; before the first block
        call    set_numlock     ; REM it out so we don't act on it later, too
        mov     byte ptr es:[si-1],CONFIG_REM
        jmp     short swchk_nextline
swchk_next2:                    ;
        cmp     al,CONFIG_SWITCHES
        jne     swchk_nextline  ; this line ain't it
swchk_scan:                     ;
        call    get_char        ; look for /N or /F
swchk_scan1:                    ;
        cmp     al,LF           ; end of line?
        je      swchk_nextline  ; yes
        cmp     al,'/'          ; switch-char?
        jne     swchk_scan      ; no
        call    get_char        ;
        and     al,not 20h      ; convert to upper case
        cmp     al,byte ptr swit_n+1
        jne     swchk_scan2     ; no
        or      byte ptr [bDisableUI],1
        jmp     swchk_scan      ; continue looking for switches of interest
swchk_scan2:                    ;
        cmp     al,byte ptr swit_f+1
        jne     swchk_scan1     ; no
        or      byte ptr [bDisableUI],2
        jmp     swchk_scan      ; continue looking for switches of interest
swchk_nextline:                 ;
        call    skip_opt_line   ;
        jmp     swchk_loop      ;
swchk_end:                      ;
        pop     si              ;
        pop     cx              ;
;
;   Do the keyboard tests for clean/interactive boot now, but only if
;   the DisableUI flag is still clear
;
        test    byte ptr [bDisableUI],1
        jnz     menu_search
;
;   Wait for 2 seconds first, UNLESS the /F bit was set in bDisableUI, or
;   there is anything at all in the keyboard buffer
;
        call    kbd_read
        jnc     menu_search
        jmp     menu_abort
;
;   Search for MENU block;  it is allowed to be anywhere in config.sys
;
menu_search:
        sub     bx,bx           ; if no MENU, default to zero for no_selection
        mov     di,offset szMenu;
        call    find_block      ; find the MENU block
        jnc     menu_found      ;
        mov     byte ptr [szBoot],0
        jmp     no_selection    ; not found
;
;   Process the requested menu color(s)
;
menu_color:
        push    cx              ;
        push    dx              ;
        mov     dx,0007h        ; default color setting
        call    get_number      ; get first number
        and     bl,00Fh         ; first # is foreground color (for low nibble)
        mov     ch,bl           ; save it in CH
        and     dl,0F0h         ;
        or      dl,bl           ;
        call    delim           ; did we hit a delimiter
        jne     check_color     ; no, all done
        call    get_number      ; get next number
        and     bl,00Fh         ; second # is background color (for high nibble)
        mov     dh,bl           ; save it in DH
        and     dl,00Fh         ;
        mov     cl,4            ;
        shl     bl,cl           ;
        or      dl,bl           ;
check_color:                    ;
        cmp     ch,dh           ; are foreground/background the same?
        jne     set_color       ; no
        xor     dl,08h          ; yes, so modify the fgnd intensity
set_color:
        mov     [bMenuColor],dl ;
        pop     dx              ;
        pop     cx              ;
        jmp     menu_nextitem   ;
;
;   Back to our regularly scheduled program (the COLOR and other goop
;   above is there simply to alleviate short jump problems)
;
menu_found:
        mov     byte ptr [bDefBlock],1
        mov     word ptr [offDefBlock],0
        mov     byte ptr [secTimeOut],-1
        and     byte ptr [bQueryOpt],not 2

        call    skip_opt_line   ; skip to next line
        sub     dx,dx           ; initialize total block count (0 => none yet)
;
;   Process the menu block now
;
menu_process:
        call    get_char        ; get first char of current line
        jc      to_menu_getdefault  ; could happen if menu block at end (rare)
        and     al,NOT CONFIG_OPTION_QUERY
        cmp     al,CONFIG_BEGIN ; BEGIN implies END
        je      to_menu_getdefault
        cmp     al,CONFIG_SUBMENU
        je      menu_item       ; go process sub-menu
        cmp     al,CONFIG_MENUITEM
        je      menu_item       ; go process menu item
        cmp     al,CONFIG_MENUDEFAULT
        je      menu_default    ; go process menu default
        cmp     al,CONFIG_MENUCOLOR
        je      menu_color      ; go process menu color
        cmp     al,CONFIG_NUMLOCK
        je      menu_numlock    ;
        cmp     al,CONFIG_REM   ; allow remarks in menu block
        je      menu_nextitem   ;
        call    any_delim       ; allow blank lines and such
        je      menu_nextitem   ;
        stc                     ;
        call    print_error     ; non-MENU command!
        jmp     short menu_nextitem
menu_numlock:
        call    set_numlock
        jmp     short menu_nextitem
to_menu_getdefault:
        jmp     short menu_getdefault
;
;   Save the offset of the default block name, we'll need it later
;
menu_default:
        mov     [offDefBlock],si; save address of default block name
        cmp     byte ptr [secElapsed],0
        jne     timeout_skip    ; secElapsed is only zero for the FIRST menu,
        call    skip_token      ; and for subsequent menus IF nothing was typed;
        jc      menu_nextitem   ; secElapsed becomes -1 forever as soon as
        call    skip_delim      ; something is typed
        jc      menu_nextitem   ;
        mov     si,bx           ;
        call    get_number      ; get number (of seconds for timeout)
        cmp     bl,90           ; limit it to a reasonable number
        jb      timeout_ok      ; (besides, 99 is the largest # my simple
        mov     bl,90           ;  display function can handle)
timeout_ok:                     ;
        mov     [secTimeOut],bl ;
timeout_skip:
        jmp     short menu_nextitem
;
;   Verify that this is a valid menu item by searching for the named block
;
menu_item:
        cmp     dl,MAX_MULTI_CONFIG ; have we reached the max # of items yet?
        jae     menu_nextitem   ;
        mov     di,si           ; DS:DI -> block name to search for
        call    srch_block      ;
        je      menu_itemfound  ;
        stc                     ;
        call    print_error     ; print error and pause
        jmp     short menu_nextitem ; if not found, ignore this menu item
;
;   srch_block, having succeeded, returns DI -> past the token that it
;   just matched, which in this case should be a descriptive string;  ES:SI
;   and CX are unmodified
;
menu_itemfound:
        inc     dx              ; otherwise, increment total block count
        mov     bx,dx           ; and use it to index the arrays of offsets
        mov     abBlockType[bx],al
        add     bx,bx           ; of recorded block names and descriptions
;
;   There should be a description immediately following the block name on
;   MENUITEM line;  failing that, we'll just use the block name as the
;   description...
;
        mov     aoffBlockName[bx],si
        mov     aoffBlockDesc[bx],si
        mov     di,bx           ; skip_delim modifies BX, so stash it in DI
        call    skip_token      ;
        jc      menu_nextitem   ; hit eol/eof
        call    skip_delim      ;
        jc      menu_nextitem   ; hit eol/eof
        xchg    bx,di           ;
        mov     aoffBlockDesc[bx],di

menu_nextitem:
        call    skip_opt_line   ;
        jmp     menu_process    ; go back for more lines
;
;   Display menu items now, after determining which one is default
;
menu_getdefault:
        or      dl,dl           ; where there any valid blocks at all?
        jnz     menu_valid      ; yes
        sub     bx,bx           ; no, so force autoselect of 0
        jmp     menu_autoselect ; (meaning: process common blocks only)
menu_valid:
        sub     bx,bx           ;
        mov     [bMaxBlock],dl  ; first, record how many blocks we found
        mov     di,[offDefBlock];
        or      di,di           ; does a default block exist?
        jz      menu_nodefault  ; no
        inc     bx              ; yes, walk name table, looking for default
menu_chkdefault:
        push    bx              ;
        add     bx,bx           ;
        mov     si,aoffBlockName[bx]
        mov     cx,128          ; arbitrary maximum length of a name
        push    ds              ;
        push    es              ;
        pop     ds              ;
        call    comp_names      ; is this block the same as the default?
        pop     ds              ;
        pop     bx              ;
        je      menu_setdefault ; yes
        inc     bx              ;
        cmp     bl,[bMaxBlock]  ; all done searching?
        jbe     menu_chkdefault ; not yet
menu_nodefault:
        mov     bl,1            ; if no default, force default to #1
menu_setdefault:
        mov     [bDefBlock],bl  ; yes, this will be the initial current block
;
;   If the timeout was explicitly set to 0 (or technically, anything that
;   failed to resolve to a number, like "NONE" or "EAT POTATOES"), then we're
;   supposed to skip menu display and run with the specified default block;
;   however, if the user hit Enter prior to boot, thereby requesting fully
;   INTERACTIVE boot, then we shall display the menu block anyway (though still
;   with no timeout)
;
        cmp     [secTimeOut],0  ; is timeout zero? (ie, assume default)
        jne     menu_display    ; no
        test    byte ptr [bQueryOpt],1  ; yes, but was INTERACTIVE requested?
        jnz     menu_display    ; yes, so *don't* assume default after all
        jmp     not_topmenu     ;
;
;   Reset the mode, so that we know screen is clean and cursor is home
;
menu_display:
        mov     ah,0Fh          ; get current video mode
        int     10h             ;
        mov     ah,00h          ; just re-select that mode
        int     10h             ;
        push    es              ;
        mov     ax,40h          ; reach down into the ROM BIOS data area
        mov     es,ax           ; and save the current (default) video page
        mov     ax,es:[004Eh]   ; start address and page #, in case the
        mov     [wCRTStart],ax  ; undocumented QUIET option was enabled
        mov     al,es:[0062h]   ;
        mov     [bCRTPage],al   ;
        mov     ax,word ptr [bMenuPage] ; select new page for menu
        int     10h             ;
        mov     ax,0600h        ; clear entire screen
        mov     bh,[bMenuColor] ; using this color
        sub     cx,cx           ; upper left row/col
        mov     dl,byte ptr es:[CRT_Cols]
        dec     dl              ;
        mov     dh,es:[CRT_Rows];
        or      dh,dh           ; # of rows valid?
        jnz     menu_clear      ; hopefully
        mov     dh,[bLastRow]   ; no, use a default
menu_clear:
        int     10h             ; clear the screen using the req. attribute
        pop     es              ;
        mov     [bLastRow],dh   ; save DH
        mov     dx,offset $MenuHeader
        call    print           ; cursor now on row 3 (numbered from 0)

        test    byte ptr [bDisableUI],1
        jnz     menu_nostatus   ;
        mov     bh,[bMenuPage]  ;
        mov     dh,[bLastRow]   ; restore DH
        mov     dl,0            ; print the status line on row DH, col 0,
        mov     ah,02h          ; now that we can trash the cursor position
        int     10h             ;
        mov     dx,offset $StatusLine
        call    print           ;
        mov     ah,3            ; get cursor position
        int     10h             ;
        sub     dl,2            ;
        mov     [bLastCol],dl   ; save column where status char will go

menu_nostatus:
        mov     bx,1            ; now prepare to display all the menu items
menu_disploop:
        call    print_item      ; print item #BL
        inc     bx              ; why "inc bx"?  because it's a 1-byte opcode
        cmp     bl,[bMaxBlock]  ; all done?
        jbe     menu_disploop   ; not yet
;
;   Set cursor position to just below the menu items
;
        mov     dl,0            ; select column
        mov     dh,bl           ;
        add     dh,4            ; select row below menu
        mov     bh,[bMenuPage]  ;
        mov     ah,02h          ; set cursor position beneath the block list
        int     10h             ;

        mov     dx,offset $MenuPrmpt
        call    print           ;
        call    select_item     ; make a selection, return # in BX
        mov     dx,offset crlfm ;
        call    print           ;
        push    word ptr [bDisableUI]
        or      byte ptr [bDisableUI],1
        call    show_status     ; clear the status line now
        pop     word ptr [bDisableUI]
;
;   Now begins the "re-organization" process...
;
menu_autoselect:
        cmp     bx,-1           ; clean boot requested?
        jne     normal_boot     ; no
        call    disable_autoexec; basically, add a /D to the command.com line
menu_abort:
        sub     cx,cx           ; then immediately exit with 0 config.sys image
        jmp     menu_exit       ;

normal_boot:
        cmp     bx,-2           ; back to top-level menu?
        jne     not_topmenu     ; no
        mov     cx,[count]      ; yes, start all over
        sub     si,si           ;
        jmp     menu_search

not_topmenu:
        cmp     abBlockType[bx],CONFIG_SUBMENU
        jne     not_submenu     ;
        add     bx,bx           ;
        mov     di,aoffBlockName[bx]
        call    srch_block      ; THIS CANNOT FAIL!
        mov     si,di           ;
        mov     cx,bx           ; ES:SI and CX are ready for another round
        jmp     menu_found

not_submenu:
        add     bx,bx           ; get BX -> name of selected block
        mov     bx,aoffBlockName[bx]
;
;   BX should now either be ZERO (meaning no block has been selected) or
;   the offset relative to ES of the block name to be processed (along with
;   all the "common" lines of course)
;
no_selection:
        mov     [offDefBlock],bx; save selection
        mov     cx,[count]      ; reset ES:SI and CX for reprocessing
        sub     si,si           ;
        push    ds              ;
        mov     ds,[config_wrkseg]; this is where we'll store new config.sys image
        assume  ds:nothing      ;
        sub     di,di           ;
;
;   ES:SI-> config.sys, DS:DI -> new config.sys workspace
;
;   Work our way through the config.sys image again, this time copying
;   all lines that are (A) "common" lines outside any block or (B) lines
;   within the requested block.  Lines inside INCLUDEd blocks are transparently
;   copied by copy_block in a recursive fashion;  the amount of recursion is
;   limited by the fact INCLUDE statements are REMed by copy_block as they are
;   processed and by the number of unique INCLUDE stmts in config.sys...
;
;   BUGBUG 20-Mar-1992 JeffPar:  If we can figure out the lower bound of the
;   stack we're running on, then we should check it inside copy_block
;
copyblock_loop:
        push    bx              ; save selected block name
        call    copy_block      ; process (named or common) block
        pop     bx              ;
        jc      move_config     ; hit eof
;
;   copy_block can only return for two reasons:  it hit eof or a new block
;
copyblock_begin:
        push    ax              ;
        push    cx              ;
        push    si              ;
        push    di              ; always do "common" blocks
        mov     di,offset szCommon
        push    ds              ;
        push    cs              ;
        pop     ds              ;
        call    comp_names      ;
        pop     ds              ;
        pop     di              ;
        pop     si              ;
        pop     cx              ;
        pop     ax              ;
        je      copyblock_check ;
        or      bx,bx           ; is there a block name to check?
        jz      copyblock_skip  ; no
        push    di              ;
        mov     di,bx           ; check block against given block name
        push    ds              ;
        push    es              ;
        pop     ds              ;
        call    comp_names      ; is this the block we really want to do?
        pop     ds              ;
        pop     di              ;
copyblock_check:
        jc      move_config     ; hit eof
        jne     copyblock_skip  ;
        call    skip_opt_line   ;
        jmp     copyblock_loop  ;

copyblock_skip:                 ;
        call    skip_opt_line   ; this ain't the block we wanted, so skip it
        call    get_char        ;
        jc      move_config     ; hit eof
        and     al,NOT CONFIG_OPTION_QUERY
        cmp     al,CONFIG_BEGIN ;
        je      copyblock_begin ;
        jmp     copyblock_skip  ; anything else is just skipped
;
;   To create as little risk to the rest of SysInit as little as possible,
;   and to free the workspace at "config_wrkseg" for creating an environment,
;   copy the new config.sys image to "confbot"
;
move_config:
        mov     cx,di           ; now copy workspace at DS:DI to "confbot"
        push    cx              ;
;
;   But first, copy the CONFIG=<configuration><0> string to the workspace,
;   since the configuration name only currently exists in the "confbot" area
;
        mov     cx,offset szMenu-offset szBoot-1
        mov     si,offset szBoot; first copy the CONFIG= part
        inc     di              ; skip a byte, in case absolutely nothing
                                ; was copied to the workspace, because we always
                                ; zero the first byte of the workspace (below)
copy_boot:                      ;
        lods    byte ptr cs:[si];
        mov     [di],al         ;
        inc     di              ;
        loop    copy_boot       ;

        push    es              ; then copy the configuration name
        mov     cx,128-7        ; put an upper limit on the name, to be safe
        mov     si,cs:[offDefBlock]; ES:SI -> default block name
        or      si,si           ; valid?
        jnz     l1              ; yes
        push    cs              ;
        pop     es              ;
        mov     si,offset szCommon
l1:     mov     al,es:[si]      ;
        call    any_delim       ;
        je      l2              ;
        mov     [di],al         ;
        inc     si              ;
        inc     di              ;
        loop    l1              ;
l2:     mov     byte ptr [di],LF; terminate the configuration string
        pop     es              ;
;
;   Now we can copy "config_wrkseg" (DS) to "confbot" (ES)
;
        sub     di,di           ;
        mov     [config_envlen],di
        sub     si,si           ;
        pop     cx              ; recover the size of "config_wrkseg"

        push    cx              ;
        rep     movsb           ; moved!
        pop     cx              ;
        mov     ax,ds           ;
        pop     ds              ;
        assume  ds:sysinitseg   ;
;
;   Now that the config_wrkseg is available once again, we shall
;   use it to create an environment.  The first thing to go in will be
;   the "CONFIG=configuration" thing.  It is also important to zero
;   the first byte of the workspace, so that copy_envvar knows the buffer
;   is empty.
;
        push    es              ;
        mov     es,ax           ;
        inc     si              ; ES:SI -> "CONFIG=configuration"
        mov     byte ptr es:[0],0;empty the environment block
        call    copy_envvar     ; copy envvar at ES:SI to "config_wrkseg"
        pop     es
;
;   Before returning, restore the default video page setting but do NOT
;   do it using INT 10h's Set Active Page function, because if the menu was
;   displayed on a different page, then it's because we don't want to see
;   all the device driver/TSR goop (which goes to the default page)
;
menu_done:
        cmp     byte ptr [bMenuPage],0
        je      menu_exit       ;
        push    es              ;
        mov     ax,40h          ;
        mov     es,ax           ;
        mov     ax,[wCRTStart]  ;
        mov     es:[004Eh],ax   ;
        mov     al,[bCRTPage]   ;
        mov     es:[0062h],al   ;
        pop     es              ;
menu_exit:
        mov     [count],cx      ; set new counts
        mov     [org_count],cx  ;
        sub     si,si           ; always return ES:SI pointing to config.sys
        ret
menu_check  endp

;
;----------------------------------------------------------------------------
;
;   copy_envvar:  copy the envvar at ES:SI to "config_wrkseg"
;
;   INPUT
;    ES:SI -> environment variable (in the form "var=string<cr/lf>")
;
;   OUTPUT
;       config_envlen (ie, where to put next envvar) updated appropriately
;       carry set if error (eg, missing =); clear otherwise
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       None
;
;   HISTORY
;       Created 29-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  copy_envvar
        assume  ds:sysinitseg,es:nothing

copy_envvar proc near
        push    cx              ;
        push    si              ;
        push    ds              ;
        push    es              ;
        push    es              ;
        mov     es,[config_wrkseg]; ES:DI to point to next available byte
        pop     ds                ; DS:SI to point to envvar
        assume  ds:nothing      ;
;
;   Have to calculate the length of the variable name (and if we hit
;   the end of the line before we hit '=', then it's curtains for this
;   config.sys line)
;
;   The check for NULL is important because copy_envvar is also used to copy
;   the initial CONFIG= setting, which will have been zapped by a NULL if no
;   menu block existed (in order to prevent the creation of an environment)
;
        sub     cx,cx           ;
copy_varlen:                    ;
        lodsb                   ;
        or      al,al           ; NULL?
        stc                     ;
        jz      copy_envexit    ; yes, abort
        cmp     al,CR           ;
        stc                     ;
        je      copy_envexit    ;
        cmp     al,LF           ;
        stc                     ;
        je      copy_envexit    ;
        inc     cx              ;
        cmp     al,'='          ;
        jne     copy_varlen     ;
        mov     al,0            ;
        mov     ah,[si]         ; save char after '='
        sub     si,cx           ; back up to given varname
        dec     cx              ; CX == # of bytes in varname
        sub     di,di           ; start looking for DS:SI at ES:0
copy_varsrch:
        cmp     byte ptr es:[di],al
        je      copy_envprep    ; search failed, just copy var
        mov     bx,di           ; ES:BX -> start of this varname
        push    cx              ;
        push    si              ;
        repe    cmpsb           ;
        pop     si              ;
        pop     cx              ;
        jne     copy_varnext    ; no match, skip to next varname
        cmp     byte ptr es:[di],'='
        jne     copy_varnext    ; no match, there's more characters
;
;   Previous occurrence of variable has been found;  determine the
;   entire length and then destroy it
;
        mov     cx,-1           ;
        repne   scasb           ; guaranteed to get null (since we put it there)
        push    si              ;
        mov     si,di           ;
        mov     di,bx           ;
        mov     cx,[config_envlen]
        sub     cx,si           ; destroy variable now
        rep movs byte ptr es:[di],byte ptr es:[si]
        pop     si              ;
copy_envprep:
        cmp     ah,CR           ; if there is nothing after the '='
        je      copy_envdel     ; then just exit with variable deleted
        cmp     ah,LF           ;
        je      copy_envdel     ;
        jmp     short copy_envloop

copy_varnext:                   ;
        push    cx              ;
        mov     cx,-1           ;
        repne   scasb           ;
        pop     cx              ;
        jmp     copy_varsrch    ;

copy_envloop:                   ;
        lodsb                   ;
        cmp     al,CR           ;
        je      copy_envdone    ;
        cmp     al,LF           ;
        je      copy_envdone    ;
        stosb                   ;
        jmp     copy_envloop    ;

copy_envdone:                   ;
        sub     al,al           ; do SUB to clear carry as well
        stosb                   ; always null-terminate these puppies
copy_envdel:                    ;
        mov     es:[di],al      ; and stick another null to terminate the env.
        mov     [config_envlen],di

copy_envexit:                   ;
        pop     es              ;
        pop     ds              ;
        assume  ds:sysinitseg   ;
        pop     si              ;
        pop     cx              ;
        ret                     ;
copy_envvar endp

;
;----------------------------------------------------------------------------
;
;   copy_block:  copy the current block to the new config.sys workspace
;
;   INPUT
;       CX == remaining bytes in "organized" config.sys memory image
;    ES:SI -> remaining bytes in "organized" config.sys memory image
;    DS:DI -> new config.sys workspace (equal in size to the original
;             config.sys image) where the current block is to be copied
;
;   OUTPUT
;       Same as above
;       AL also equals the last character read from the organized image
;
;   OTHER REGS USED
;       All
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  copy_block

copy_block  proc near
        call    get_char        ; check for include
        jc      copy_done       ;
        and     al,NOT CONFIG_OPTION_QUERY
        cmp     al,CONFIG_BEGIN ; another BEGIN implies END as well
        je      copy_done       ;
;
;   BOOTCON relies on these things being in-memory (sigh)
;
;       cmp     al,CONFIG_REM   ; don't bother copying this
;       je      copy_nextline   ;
;       cmp     al,CONFIG_COMMENT;don't bother copying this either
;       je      copy_nextline   ;
;
;   This is being left out for old reporting of bad command lines;
;   alternatively, we could report the error here and save the rest of
;   sysinit from ever having to deal with bogus CONFIG.SYS stuff...
;
;       cmp     al,CONFIG_UNKNOWN;don't bother copying this either too
;       je      copy_nextline   ;
                                ;
        cmp     al,CONFIG_INCLUDE
        mov     al,ah           ; AL == the original line code
        jne     copy_line       ; not an "include" line
;
;   We have hit an "INCLUDE" line;  first, REM out the line so that we
;   never try to include the block again (no infinite include loops please),
;   then search for the named block and call copy_block again.
;
        mov     byte ptr es:[si-1],CONFIG_REM
        push    di              ;

        mov     di,offset szMenu
        call    comp_names_safe ; don't allow INCLUDE MENU
        je      copy_skip       ;

        mov     di,offset szCommon
        call    comp_names_safe ; don't allow INCLUDE COMMON
        je      copy_skip       ;

        mov     di,si           ; try to find the block
        call    srch_block      ;
        mov     dx,di           ;
        pop     di              ;
        jne     copy_error      ; no such block
        push    cx              ;
        mov     cx,bx           ;
        push    si              ;
        dec     dx              ;
        mov     si,dx           ;
        call    skip_line       ; skip the rest of the "block name" line
        call    copy_block      ; and copy in the rest of that block
        pop     si              ;
        pop     cx              ;
        sub     al,al           ; force skip_opt_line to skip...
        jmp     short copy_nextline

copy_skip:
        pop     di
copy_error:
        clc                     ;
        call    print_error     ; note that carry is clear, no pause
        jmp     short copy_nextline
;
;   Copy the line at ES:SI to the current location at DS:DI
;
copy_line:
        mov     [di],al         ;
        inc     di              ;
        cmp     al,' '          ; is this is a "real" line with a "real" code?
        jb      copy_nextline   ; no
        cmp     cs:[config_multi],0
        je      copy_loop       ; not a multi-config config.sys, don't embed #s
        call    get_linenum     ; BX == line # of line @ES:SI
        mov     [di],bx         ; stash it immediately following the line code
        inc     di              ;
        inc     di              ;
        jmp     short copy_next ;
copy_loop:                      ;
        call    get_char        ;
        jc      copy_done       ; end of file
        mov     [di],al         ;
        inc     di              ;
copy_next:
        cmp     al,LF           ; done with line?
        jne     copy_loop       ; nope

copy_nextline:
        call    skip_opt_line   ;
        jmp     copy_block      ;

copy_done:
        ret
copy_block  endp

;
;----------------------------------------------------------------------------
;
;   get_linenum:  return line # (in BX) of current line (@ES:SI)
;
;   INPUT
;    ES:SI -> some line in the config.sys memory image
;
;   OUTPUT
;       BX == line # (relative to 1)
;
;   OTHER REGS USED
;       DX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  get_linenum

get_linenum proc near
        push    ax              ;
        sub     bx,bx           ; BX == line # (to be returned)
        push    cx              ;
        mov     dx,si           ; DX == the offset we're looking for
        push    si              ;
        mov     cx,cs:[count]   ;
        sub     si,si           ; prepare to scan entire file
get_linenum_loop:               ;
        call    skip_line       ;
        jc      get_linenum_done;
        inc     bx              ;
        cmp     si,dx           ; have we exceeded the desired offset yet?
        jb      get_linenum_loop; no
get_linenum_done:               ;
        pop     si              ;
        pop     cx              ;
        pop     ax              ;
        ret                     ;
get_linenum endp

;
;----------------------------------------------------------------------------
;
;   srch_block:  searches entire config.sys for block name @ES:DI
;
;   INPUT
;       ES -> config.sys image
;    ES:DI -> block name to find
;
;   OUTPUT
;       ZF flag set, if found
;    ES:DI -> just past the name in the block heading, if found
;       BX == # bytes remaining from that point, if found
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       This differs from "find_block" in that it searches the ENTIRE
;       config.sys image, not merely the remaining portion, and that it
;       takes a pointer to block name that is *elsewhere* in the image
;       (ie, ES) as opposed to some string constant in our own segment (DS).
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  srch_block

srch_block  proc near           ; returns BX -> named block in CONFIG.SYS
        push    ax              ;
        push    cx              ;
        mov     cx,cs:[count]   ;
        push    si              ;
        sub     si,si           ;
        push    ds              ;
        push    es              ;
        pop     ds              ;
        call    find_block      ;
        mov     di,si           ;
        mov     bx,cx           ;
        pop     ds              ;
        pop     si              ;
        pop     cx              ;
        pop     ax              ;
        ret                     ;
srch_block  endp

;
;----------------------------------------------------------------------------
;
;   find_block:  searches rest of config.sys for block name @DS:DI
;
;   INPUT
;    DS:DI -> block name to find
;    ES:SI -> remainder of config.sys image
;       CX == remaining size of config.sys image
;
;   OUTPUT
;       ZF flag set, if found (also, CF set if EOF)
;    ES:SI -> where the search stopped (at end of block name or EOF)
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       This differs from "srch_block" in that it searches only the
;       remaining portion of the config.sys image and leaves SI and CX
;       pointing to where the search left off, and that it takes a pointer
;       to search string in our own segment (DS:DI instead of ES:DI).
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  find_block

find_block  proc near
        call    get_char        ; get line code
        jc      find_exit       ; end of file
        and     al,NOT CONFIG_OPTION_QUERY
        cmp     al,CONFIG_BEGIN ; beginning of a block?
        je      check_line      ; no
        cmp     al,CONFIG_INCLUDE
        jne     next_line       ;
        or      cs:[config_multi],1
        jmp     short next_line ;
check_line:
        or      cs:[config_multi],1
        call    comp_names      ; compare block names
        jbe     find_exit       ; end of file, or names matched
next_line:
        call    skip_opt_line   ; no, so skip to next line
        jmp     find_block      ;
find_exit:
        ret
find_block  endp

;
;----------------------------------------------------------------------------
;
;   comp_names:  compares keyword @DS:DI to position in config.sys @ES:SI
;
;   INPUT
;    DS:DI -> keyword to compare
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       ZF flag set, if match (also, CF set if EOF)
;    ES:SI -> where the comparison stopped (at end of block name or EOF)
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  comp_names

comp_names  proc near
        push    di              ;
comp_loop:                      ;
        call    get_char        ;
        jc      comp_exit       ;
        call    any_delim       ; is next character a delimiter?
        mov     ah,[di]         ; (get next character we're supposed to match)
        je      comp_almost     ; yes, it *could* be a match
        inc     di              ;
        and     ax,not 2020h    ; BUGBUG -- assumes both names are alphanumeric -JTP
        cmp     al,ah           ; match?
        je      comp_loop       ; yes, keep looking at the characters
        clc                     ; prevent erroneous eof indication: clear carry
comp_exit:                      ;
        pop     di              ;
        ret                     ;
comp_almost:                    ;
        xchg    al,ah           ; we don't know for sure if it's a match
        call    any_delim       ; until we verify that the second string has
        xchg    al,ah           ; been exhausted also...
        jmp     comp_exit       ; if we are, this call to any_delim will tell...
comp_names  endp

        public  comp_names_safe

comp_names_safe proc near
        push    ax
        push    cx
        push    si
        push    ds
        push    cs
        pop     ds
        call    comp_names
        pop     ds
        pop     si
        pop     cx
        pop     ax
        ret
comp_names_safe endp


;
;----------------------------------------------------------------------------
;
;   print_item:  display menu item #BL
;
;   INPUT
;       BL == menu item # to display
;
;   OUTPUT
;       Menu item displayed, with appropriate highlighting if BL == bDefBlock
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       This function saves/restores the current cursor position, so you
;       needn't worry about it.
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  print_item

print_item  proc near           ; prints menu item #BL (1 to N)
        push    ax              ;
        push    bx              ;
        push    cx              ;
        push    dx              ;
        push    si              ;
        mov     ah,03h          ; get cursor position
        mov     bh,[bMenuPage]  ; always page zero
        int     10h             ; DH/DL = row/column
        push    dx              ; save it
        mov     ah,02h          ; set cursor position
        mov     dh,bl           ;
        add     dh,3            ;
        mov     dl,5            ;
        int     10h             ; set cursor position for correct row/col
        mov     al,bl           ;
        add     al,'0'          ; convert menu item # to ASCII digit
        mov     ah,[bMenuColor] ; normal attribute
        cmp     bl,[bDefBlock]  ; are we printing the current block?
        jne     print_other     ; no
        or      ah,70h          ; yes, set bgnd color to white
        mov     ch,ah           ;
        mov     cl,4            ;
        rol     ch,cl           ;
        cmp     ch,ah           ; are fgnd/bgnd the same?
        jne     print_other     ; no
        xor     ah,08h          ; yes, so modify the fgnd intensity
print_other:                    ;
        mov     bh,0            ;
        add     bx,bx           ;
        mov     di,aoffBlockDesc[bx]
        mov     bl,ah           ; put the attribute in the correct register now
        mov     bh,[bMenuPage]  ; get correct video page #
        mov     ah,09h          ; write char/attr
        mov     cx,1            ;
        int     10h             ;
        inc     dl              ; increment column
        mov     ah,02h          ;
        int     10h             ;
        mov     ax,0900h+'.'    ;
        int     10h             ; display '.'
        inc     dl              ; increment column
        mov     ah,02h          ;
        int     10h             ;
        mov     ax,0900h+' '    ;
        int     10h             ; display ' '
        inc     dl              ; increment column
        mov     ah,02h          ;
        int     10h             ;
        push    es              ;
print_loop:                     ;
        mov     al,es:[di]      ; get a character of the description
        inc     di              ;
        cmp     al,TAB          ; substitute spaces for tabs
        jne     print_nontab    ;
        mov     al,' '          ;
print_nontab:                   ;
        cmp     al,' '          ;
        jb      print_done      ; stop at the 1st character < space
        cmp     al,'$'          ;
        je      print_done      ; also stop on $
        mov     ah,09h          ; display function #
        int     10h             ;
        inc     dl              ; increment column
        cmp     dl,78           ; far enough?
        jae     print_done      ; yes
        mov     ah,02h          ;
        int     10h             ;
        jmp     print_loop      ;
print_done:                     ;
        pop     es              ;
        pop     dx              ;
        mov     ah,02h          ;
        int     10h             ; restore previous row/col
        pop     si              ;
        pop     dx              ;
        pop     cx              ;
        pop     bx              ;
        pop     ax              ;
        ret                     ;
print_item  endp

;
;----------------------------------------------------------------------------
;
;   select_item:  wait for user to select menu item, with time-out
;
;   INPUT
;       None
;
;   OUTPUT
;       BX == menu item # (1-N), or -1 for clean boot
;       Selected menu item highlighted
;       Cursor positioned beneath menu, ready for tty-style output now
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  select_item

select_item proc near           ; returns digit value in BX (trashes AX/CX/DX)
        mov     bl,[bDefBlock]  ; BL will be the default block #
        mov     al,bl           ;
        call    disp_num        ;
        call    show_status     ; display current interactive status
        cmp     byte ptr [secTimeOut],-1
        je      input_key       ; no time-out, just go to input
        mov     ah,Get_Time     ;
        int     21h             ;
        mov     bh,dh           ; BH = initial # of seconds
check_time:
        mov     al,[secTimeOut] ;
        sub     al,[secElapsed] ;
        jae     show_time       ;
        or      byte ptr [bQueryOpt],2  ; disable all further prompting
        mov     byte ptr [secElapsed],0
        jmp     select_done     ; time's up!
show_time:
        push    bx              ;
        mov     bl,al           ; save # in BL
        mov     bh,[bMenuPage]  ;
        mov     ah,03h          ; get cursor position
        int     10h             ;
        push    dx              ;
	add	dl,8		; move cursor to the right
        mov     ah,02h          ; set cursor position
        int     10h             ;
        mov     dx,offset $TimeOut
        call    print           ; print the "Time remaining: " prompt
        mov     al,bl           ; recover # from BL
        cbw                     ; this works because AL is always <= 90
        mov     cl,10           ;
        div     cl              ; AL = tens digit, AH = ones digit
        mov     cl,ah           ;
        add     al,'0'          ;
        mov     ah,0Eh          ;
        int     10h             ; write TTY tens digit
        mov     al,cl           ;
        add     al,'0'          ;
        mov     ah,0Eh          ;
        int     10h             ; write TTY ones digit
        pop     dx
        mov     ah,02h          ; set cursor position back to where it was
        int     10h             ;
        pop     bx              ;
input_key:
        mov     ah,Raw_Con_IO   ;
        mov     dl,0FFh         ; input request
        int     21h             ;
        jnz     got_key         ;
        cmp     byte ptr [secTimeOut],-1; is there a time-out?
        je      input_key       ; no, just go back to input
        mov     ah,Get_Time     ;
        int     21h             ; DH = seconds
        mov     ah,dh           ;
        sub     dh,bh           ; should generally be zero or one
        mov     bh,ah           ;
        jnc     got_time        ;
        mov     dh,1            ; it wrapped back to zero, so assume one
got_time:
        or      dh,dh           ; any change?
        jz      input_key       ; no
        add     [secElapsed],dh ;
        jmp     check_time      ;
got_key:
        push    ax              ;
        mov     ax,-1           ; zap both secTimeOut and secElapsed
        xchg    word ptr [secTimeOut],ax
        cmp     al,-1           ; was time-out already disabled?
        je      timeout_disabled; yes
        push    bx              ; let's disable # seconds display
        mov     ax,0A20h        ; write multiple spaces
        mov     bx,word ptr [bMenuColor]
        mov     cx,80           ; 80 of them, to be safe
        int     10h             ; to completely obliterate # seconds display
        pop     bx              ;
timeout_disabled:
        pop     ax              ;
        or      al,al           ; extended key pressed?
        jnz     normal_key      ; no
        int     21h             ; get the next part of the key then
        jz      input_key       ; hmmm, what happened to the second part?

        cmp     al,48h          ; up arrow?
        jne     not_up          ; no
        cmp     bl,1            ; are we as up as up can get?
        jbe     input_key       ; yes, ignore it
        dec     [bDefBlock]     ;
        call    print_item      ; re-print the current item
        dec     bl              ; and then print the new current item
        jmp     short print1
not_up:
        cmp     al,50h          ; down arrow?
        jne     not_down        ; no
        cmp     bl,[bMaxBlock]  ; are we as down as down can get?
        jae     to_input_key    ; yes, ignore it
        inc     [bDefBlock]     ;
        call    print_item      ; re-print the current item
        inc     bx              ; and then print the new current item
print1: mov     al,bl           ;
print2: call    print_item      ;
        call    disp_num        ;
to_input_key:
        jmp     input_key       ;
not_down:
        test    byte ptr [bDisableUI],1
        jnz     to_input_key    ; don't allow F8 or F5
        cmp     al,42h          ; F8 function key?
        jne     not_f8          ; no
        xor     byte ptr [bQueryOpt],1
        call    show_status     ;
        jmp     input_key       ;
not_f8:
        cmp     al,3Fh          ; F5 function key?
        jne     to_input_key    ; no
        mov     bx,-1           ; special return code (-1) indicating clean boot
        mov     al,' '          ; don't want to display anything really;
        jmp     short disp_input; just want to display the cr/lf sequence...

normal_key:
        cmp     al,0Dh          ; Enter?
        je      select_done     ; yes
        cmp     al,08h          ; backspace?
        jne     not_backspace   ; no
        mov     bx,-2           ; yes, special return code
        ret                     ;
not_backspace:
        sub     al,'0'          ; is greater than '0'?
        jbe     to_input_key    ; no
        cmp     al,[bMaxBlock]  ; is less than or equal to the maximum digit?
        ja      to_input_key    ; no
        mov     [bDefBlock],al  ;
        call    print_item      ; redisplay the current selection
        mov     bl,al           ; set new selection
        jmp     print2

select_done:
        mov     bh,0            ; return a full 16-bit value (for indexing)
        mov     al,bl           ;
        add     al,'0'          ; convert it into a digit, then display it
        if2
        .errnz  disp_input-$    ; fall into disp_input
        endif
select_item endp

;
;----------------------------------------------------------------------------
;
;   disp_input:  display a single character + cr/lf
;
;   INPUT
;       AL == character to display
;
;   OUTPUT
;       None
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       This function is used not only for the menu input selection but
;       also for the interactive line prompting (the y/n/a thing).
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  disp_input

disp_input  proc near
        push    ax
        cmp     al,' '
        jae     disp_ok
        mov     al,' '
disp_ok:
        mov     dl,al
        mov     ah,Std_Con_Output
        int     21h
        mov     dx,offset crlfm
        call    print
        pop     ax
        ret
disp_input  endp

disp_num    proc near
        push    bx
        add     al,'0'
        mov     ah,0Ah
        mov     bx,word ptr [bMenuColor]
        mov     cx,1
        int     10h
        pop     bx
        ret
disp_num    endp

;
;----------------------------------------------------------------------------
;
;   show_status:  display current interactive mode setting (on/off/none)
;
;   INPUT
;       None
;
;   OUTPUT
;       None
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  show_status

show_status proc near
        push    bx              ; BL = video page #
        mov     bx,word ptr [bMenuColor]
        mov     ah,03h          ; get cursor position
        int     10h             ;
        push    dx              ; save it
        mov     ah,02h          ; set cursor position
        mov     dx,word ptr [bLastCol]   ; set correct row/col
        test    byte ptr [bDisableUI],1
        jz      show_onoff      ; just show on/off
        mov     dl,0            ;
        int     10h             ;
        mov     ax,0A20h        ; write multiple spaces
        mov     cx,80           ; 80 of them, to be exact
        int     10h             ; to obliterate the status line
        jmp     short show_done ;
show_onoff:                     ;
        int     10h             ;
        mov     al,$NO          ; assume OFF
        cmp     byte ptr [bQueryOpt],1  ; is interactive mode on?
        jne     show_noton      ; no
        mov     al,$YES         ; yes
show_noton:                     ;
        mov     ah,0Eh          ; write TTY
        int     10h             ;
show_done:                      ;
        pop     dx              ;
        mov     ah,02h          ;
        int     10h             ; restore original cursor position
        pop     bx              ;
        ret                     ;
show_status endp

;
;----------------------------------------------------------------------------
;
;   skip_token:  advances ES:SI/CX past the current token
;
;   INPUT
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       CF set if EOL/EOF hit
;       AL == 1st char of delimiter
;    ES:SI -> just past the delimiter
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  skip_token

skip_token  proc near
        call    get_char
        jc      skip_token_done
        call    any_delim
        jne     skip_token
skip_check_eol:
        cmp     al,CR
        je      skip_token_eol
        cmp     al,LF
        je      skip_token_eol
        clc
        jmp     short skip_token_done
skip_token_eol:
        stc
skip_token_done:
        ret
skip_token  endp

;
;----------------------------------------------------------------------------
;
;   skip_delim:  advances ES:SI/CX past the current delimiter
;
;   INPUT
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       CF set if EOF hit
;       AL == 1st char of token
;    ES:SI -> just past the token
;       CX == # bytes remaining from that point
;    ES:BX -> new token (since ES:SI is already pointing 1 byte past token)
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  skip_delim

skip_delim  proc near           ; returns carry set if eol/eof
        call    get_char        ;
        lea     bx,[si-1]       ; also returns BX -> next token
        jc      skip_token_done ;
        call    delim           ;
        je      skip_delim      ;
        jmp     skip_check_eol  ;
skip_delim  endp

;
;----------------------------------------------------------------------------
;
;   skip_opt_line:  same as skip_line provided AL != LF
;
;   INPUT
;       AL == last character read
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       CF set if EOF hit
;       AL == 1st char of new line
;    ES:SI -> just past 1st char of new line
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       In other words, the purpose here is to skip to the next line,
;       unless ES:SI is already sitting at the front of the next line (which
;       it would be if the last character fetched -- AL -- was a linefeed)
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  skip_opt_line

skip_opt_line   proc near
        cmp     al,LF
        je      skip_line_done
        if2
        .errnz  skip_line-$     ; fall into skip_line
        endif
skip_opt_line   endp

;
;----------------------------------------------------------------------------
;
;   skip_line:  skip to the next line
;
;   INPUT
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       CF set if EOF hit
;    ES:SI -> just past 1st char of new line
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  skip_line

skip_line   proc near
        call    get_char
        jc      skip_line_done
        cmp     al,LF
        jne     skip_line
skip_line_done:
        ret
skip_line   endp

;
;----------------------------------------------------------------------------
;
;   get_number:  return binary equivalent of numeric string
;
;   INPUT
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       AL == non-digit encountered
;       BX == binary #
;    ES:SI -> just past 1st non-digit
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  get_number

get_number  proc near
        sub     bx,bx           ; BX = result
num_loop:
        call    get_char        ;
        jc      num_done        ;
        cmp     al,'0'          ; convert to value
        jb      num_done        ; no more number
        cmp     al,'9'          ;
        ja      num_done        ;
        push    ax              ;
        mov     ax,10           ;
        push    dx              ;
        mul     bx              ;
        pop     dx              ;
        mov     bx,ax           ;
        pop     ax              ;
        sub     al,'0'          ;
        cbw                     ;
        add     bx,ax           ;
        jmp     num_loop        ;
num_done:
        ret
get_number  endp

;
;----------------------------------------------------------------------------
;
;   get_char:  return next character, advance ES:SI, and decrement CX
;
;   INPUT
;    ES:SI -> position in config.sys
;       CX == remaining bytes in config.sys
;
;   OUTPUT
;       AL == next character
;    ES:SI -> just past next character
;       CX == # bytes remaining from that point
;
;   OTHER REGS USED
;       AX
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  get_char

get_char    proc near
        sub     cx,1            ; use SUB to set carry,zero
        jb      get_fail        ; out of data
        lods    byte ptr es:[si];
        mov     ah,al           ;
        ret                     ;
get_fail:                       ; restore CX to zero
        mov     cx,0            ; leave carry set, zero not set
nearby_ret:
        ret
get_char    endp

;
;----------------------------------------------------------------------------
;
;   query_user:  ask user whether to execute current config.sys command
;
;   INPUT
;       AL == current command code
;    ES:SI -> current command line in config.sys
;    config_cmd == current command code, but with QUERY bit intact
;                  (00h used to generate "Process AUTOEXEC.BAT" prompt)
;
;   OUTPUT
;       CF set if command should be ignored (it is also REM'ed out)
;
;   OTHER REGS USED
;       BX, CX, DX, DI
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  query_user

query_user  proc near
        test    byte ptr [bQueryOpt],4  ; answer no to everything?
        jz      @F                      ;
        jmp     skip_all                ;
@@:     test    byte ptr [bQueryOpt],2  ; answer yes to everything?
        jnz     nearby_ret              ; yes (and return carry clear!)
        push    ax                      ;
        mov     al,[config_cmd]         ;
        test    byte ptr [bQueryOpt],1  ; query every command?
        jnz     query_all               ; yes
        test    al,CONFIG_OPTION_QUERY  ;
        jnz     query_all               ;
        jmp     do_cmd                  ;
query_all:
;
;   Search for the command code (AL) in "comtab", and then print
;   out the corresponding keyword, followed by the rest of the actual
;   line pointed to by ES:SI
;
        push    si                      ; save pointer to rest of CONFIG.SYS line
        mov     dx,offset $AutoPrmpt    ;
        and     al,NOT CONFIG_OPTION_QUERY
        jz      generic_prompt          ; config_cmd must have been 0

        mov     dh,al                   ; save config_cmd in DH
        sub     bx,bx                   ;
        mov     di,offset comtab        ;
find_match:                             ;
        mov     bl,[di]                 ; get size of current keyword
        or      bl,bl                   ;
        jz      line_print              ; end of table
        inc     di                      ;
        cmp     al,[di+bx]              ; match?
        je      cmd_match               ; yes
        lea     di,[di+bx+1]            ; otherwise, skip this command code
        jmp     find_match              ; loop
cmd_match:                              ;
        mov     cl,[di-1]               ;
        mov     ch,0                    ;
        mov     ah,Std_Con_Output
cmd_print:                              ;
        mov     al,[di]                 ;
        inc     di                      ;
        mov     dl,al                   ;
        int     21h                     ;
        loop    cmd_print               ;
        mov     dl,'='                  ;
        cmp     dh,CONFIG_SET           ; for SET commands, don't display a '='
        jne     cmd_notset              ;
        mov     dl,' '                  ;
cmd_notset:
        int     21h                     ; '=' looks funny on SET commands
line_print:                             ;
        lods    byte ptr es:[si]        ;
        or      al,al                   ;
        jnz     non_null                ;
        mov     al,' '                  ;
non_null:                               ;
        cmp     al,' '                  ; control code?
        jb      prompt_user             ; yes, assume end of line
        jne     non_space               ;
        cmp     byte ptr es:[si],' '    ;
        jb      prompt_user             ;
non_space:                              ;
        mov     dl,al                   ;
        mov     ah,Std_Con_Output       ;
        int     21h                     ;
        jmp     line_print              ;
prompt_user:                            ;
        mov     dx,offset $InterPrmpt   ;

generic_prompt:
        call    print                   ;
input_loop:                             ;
        mov     ah,0                    ; read a key
        int     16h                     ;
        or      al,al                   ; is it a function key?
        jnz     not_func                ; no
        cmp     ah,3Fh                  ; F5 function key?
        jne     input_loop              ; no
        mov     al,$NO                  ;
        or      byte ptr [bQueryOpt],4  ; no more queries
        jmp     short legal_char        ;
not_func:
        and     al,not 20h              ; converting to upper case
        cmp     al,$NO                  ; verify character is legal
        je      legal_char              ;
        cmp     al,$YES                 ;
        je      legal_char              ;
        cmp     [config_cmd],0          ;
        je      input_loop              ; don't allow Esc on this query
        cmp     al,1Bh                  ; Esc?
        jne     input_loop              ;
        or      byte ptr [bQueryOpt],2  ; no more interactive boot prompts
        mov     al,$YES
legal_char:                             ;
        call    disp_input              ;
        pop     si                      ; restore pointer to rest of CONFIG.SYS line

        cmp     al,$NO                  ; process line?
        je      skip_cmd                ; no
do_cmd:
        pop     ax                      ;
        clc                             ; just do the command
        ret

skip_cmd:
        pop     ax                      ;
skip_all:
        mov     ah,CONFIG_REM           ; fake out the rest of sysinit's processing
        stc
        ret
query_user  endp

;
;----------------------------------------------------------------------------
;
;   print_error:  displays multi-config error conditions
;
;   INPUT
;    Carry set to pause, clear to not
;    ES:SI -> current command line in config.sys
;
;   OUTPUT
;       None
;
;   OTHER REGS USED
;       None
;
;   NOTES
;       None
;
;   HISTORY
;       Created 16-Mar-1992 by JeffPar
;
;----------------------------------------------------------------------------
;
        public  print_error
        extrn   error_line:near
        extrn   linecount:word

print_error proc near
        push    ax
        push    bx
        push    cx
        push    dx
        push    ds
        push    cs
        pop     ds
        pushf
        call    get_linenum
        mov     [linecount],bx
        call    error_line
        popf
        jnc     pe_ret
        mov     dx,offset $PauseMsg
        call    print
        mov     ax,0C07h                ; flush input buffer, then wait for key
        int     21h                     ; wait for a key
        or      al,al                   ; extended key?
        jnz     @F                      ; no
        mov     ah,07h                  ; yes
        int     21h                     ; eat it too
@@:     mov     dx,offset crlfm
        call    print
pe_ret: pop     ds
        pop     dx
        pop     cx
        pop     bx
        pop     ax
	ret
print_error endp


;
;   This function is very simple:  it merely prepends a "/D" to the
;   command-line for the shell;  this (undocumented) switch disables
;   AUTOEXEC.BAT processing and the date/time prompt that is usually
;   displayed when there's no AUTOEXEC.BAT.
;
        public  disable_autoexec

disable_autoexec proc near
        or      byte ptr [bQueryOpt],2
        mov     al,[command_line-1]     ; get default switchchar
        or      al,al                   ; anything there?
        jz      disable_exit            ; no, disable_autoexec already called
        mov     bl,[command_line]       ;
        mov     bh,0                    ; BX == command-line length
        mov     cx,bx                   ;
        add     bl,3                    ;
        cmp     bl,126                  ;
        ja      disable_exit            ;
        mov     [command_line],bl       ; update length
        add     bx,offset command_line+1; make sure we move the NULL too
        inc     cx                      ; (just for consistency sake)
disable_loop:                           ;
        mov     ah,[bx-3]               ;
        mov     [bx],ah                 ;
        dec     bx                      ;
        loop    disable_loop            ;
        mov     [bx-2],al               ;
        mov     word ptr [bx-1],' D'    ; /D is stuffed into place now
        mov     byte ptr [command_line-1],0
disable_exit:                           ;
        ret                             ;
disable_autoexec endp

        assume  ds:nothing

endif  ;MULTI_CONFIG

;
;----------------------------------------------------------------------------
;
; procedure : delim
;
;----------------------------------------------------------------------------
;
ifdef   MULTI_CONFIG
any_delim proc  near
        cmp     al,CR
        je      delim_ret
        cmp     al,LF
        je      delim_ret
        cmp     al,'['
        je      delim_ret
        cmp     al,']'
        je      delim_ret
        if2
        .errnz  delim-$
        endif
any_delim endp
endif  ;MULTI_CONFIG

delim	proc	near
	cmp	al,'/'		; ibm will assume "/" as an delimeter.
	jz	delim_ret

	cmp	al, 0		; special case for sysinit!!!
	jz	delim_ret

org_delim:			; used by organize routine except for getting
	cmp	al,' '          ;the filename.
	jz	delim_ret
        cmp     al,TAB
	jz	delim_ret
	cmp	al,'='
	jz	delim_ret
	cmp	al,','
	jz	delim_ret
	cmp	al,';'
ifdef   MULTI_CONFIG
;
;   Make sure there's no chance of a false EOF indication
;
        clc
endif
delim_ret:
	ret
delim	endp

;
;----------------------------------------------------------------------------
;
; procedure : newline
;
;  newline returns with first character of next line
;
;----------------------------------------------------------------------------
;

newline	proc	near

	call	getchr			;skip non-control characters
	jc	nl_ret
	cmp	al,lf			;look for line feed
	jnz	newline
	call	getchr
nl_ret:
	ret

newline	endp

;
;----------------------------------------------------------------------------
; 
; procedure : mapcase
;
;----------------------------------------------------------------------------
;
mapcase	proc	near
	push	cx
	push	si
	push	ds

	push	es
	pop	ds

ifdef   MULTI_CONFIG
        mov     bl,al                   ; same cmd code this line
else
	xor	si,si
endif

convloop:
	lodsb

	ifdef	DBCS
	call	testkanj
	jz	normconv		; if this is not lead byte

	mov	ah,al
	lodsb				; get tail byte
	cmp	ax,DB_SPACE
	jnz	@f			; if this is not dbcs space
	mov	word ptr [si-2],'  '	; set 2 single space
@@:

	dec	cx
        jcxz    convdone                ; just ignore 1/2 kanji error
	jmp	short noconv

;fall through, know al is not in 'a'-'z' range

normconv:
	endif

	cmp	al,'a'
	jb	noconv
	cmp	al,'z'
	ja	noconv
	sub	al,20h
	mov	[si-1],al
noconv:
ifdef   MULTI_CONFIG
;
;   When MULTI_CONFIG enabled, "mapcase" is used to map everything to
;   upper-case a line at a time, after we've been able to figure out whether
;   the line is a SET command or not (since we don't want to upper-case
;   anything after the "=" in a SET)
;
        cmp     bl,CONFIG_SET           ; preserve case for part of the line?
        jne     check_eol               ; no, just check for end-of-line
        cmp     al,'='                  ; separator between SET var and value?
        je      convdone                ; yes
check_eol:
        cmp     al,cr
        je      convdone
        cmp     al,lf
        je      convdone
endif
	loop	convloop

convdone:
	pop	ds
	pop	si
	pop	cx
	ret

	ifdef	DBCS

	public	testkanj
testkanj:
	push	si
	push	ds

	push	ax
	mov	ax,6300h		; get dos dbcs vector
	int	21h
	pop	ax

bdbcs_do:

	cmp	ds:word ptr [si],0	; end of lead byte info?
	jz	bdbcs_notfound		; jump if so
	cmp	al,ds:[si]		; less than first byte character?
	jb	bdbcs_next		; jump if not
	cmp	al,ds:[si+1]		; grater than first byte character?
	ja	bdbcs_next

bdbcs_found:

	push	ax
	xor	ax,ax
	inc	ax			; reset zero flag
	pop	ax

bdbcs_exit:

	pop	ds
	pop	si
	ret

bdbcs_notfound:

	push	ax
	xor	ax,ax			; set zero flag
	pop	ax
	jmp	short bdbcs_exit

bdbcs_next:

	add	si,2			; points next lead byte table
	jmp	short bdbcs_do

	endif  ; DBCS

mapcase	endp

ENDIF ; CONFIGPROC

;
;----------------------------------------------------------------------------
;
; procedure : round
;
; round the values in memlo and memhi to paragraph boundary.
; perform bounds check.
;
;----------------------------------------------------------------------------
;

round	proc	near

	push	ax
	mov	ax,[memlo]

	call	pararound		; para round up

	add	[memhi],ax
	mov	[memlo],0
	mov	ax,memhi		; ax = new memhi
	cmp	ax,[alloclim]		; if new memhi >= alloclim, error
	jae	mem_err
	test	cs:[setdevmarkflag], for_devmark
	jz	skip_set_devmarksize
	push	es
	push	si
	mov	si, cs:[devmark_addr]
	mov	es, si
	sub	ax, si
	dec	ax
	mov	es:[devmark_size], ax	; paragraph
	and	cs:[setdevmarkflag], not_for_devmark
	pop	si
	pop	es
skip_set_devmarksize:
	pop	ax
	clc				;clear carry
	ret

mem_err:
	mov	dx,offset badmem
	push	cs
	pop	ds
	call	print
	jmp	stall

round	endp

IFDEF	CONFIGPROC
;
;----------------------------------------------------------------------------
;
; procedure : calldev
;
;----------------------------------------------------------------------------
;
calldev	proc	near

	mov	ds,word ptr cs:[DevEntry+2]
	add	bx,word ptr cs:[DevEntry]	;do a little relocation
	mov	ax,ds:[bx]

	push	word ptr cs:[DevEntry]
	mov	word ptr cs:[DevEntry],ax
	mov	bx,offset packet
	call	[DevEntry]
	pop	word ptr cs:[DevEntry]
	ret

calldev	endp

;
;----------------------------------------------------------------------------
;
; procedure : todigit
;
;----------------------------------------------------------------------------
;
todigit	proc	near
	sub	al,'0'
	jb	notdig
	cmp	al,9
	ja	notdig
	clc
	ret
notdig:
	stc
	ret
todigit	endp

;
;----------------------------------------------------------------------------
;
; procedure : getnum
;
; getnum parses a decimal number.
; returns it in ax, sets zero flag if ax = 0 (may be considered an
; error), if number is bad carry is set, zero is set, ax=0.
;
;----------------------------------------------------------------------------
;

getnum	proc	near

	push	bx
	xor	bx,bx			; running count is zero

b2:
	call	todigit 		; do we have a digit
	jc	badnum			; no, bomb

	xchg	ax,bx			; put total in ax
	push	bx			; save digit
	mov	bx,10			; base of arithmetic
	mul	bx			; shift by one decimal di...
	pop	bx			; get back digit
	add	al,bl			; get total
	adc	ah,0			; make that 16 bits
	jc	badnum			; too big a number

	xchg	ax,bx			; stash total

	call	getchr			;get next digit
	jc	b1			; no more characters
	cmp	al, ' ' 		; space?
	jz	b15			; then end of digits
	cmp	al, ',' 		; ',' is a seperator!!!
	jz	b15			; then end of digits.
	cmp	al, tab 		; tab
	jz	b15
	cmp	al,sepchr		; allow 0 or special separators
	jz	b15
	cmp	al,'/'			; see if another switch follows
	nop				; cas - remnant of old bad code
	nop
	jz	b15
	cmp	al,lf			; line-feed?
	jz	b15
	cmp	al,cr			; carriage return?
	jz	b15
	or	al,al			; end of line separator?
	jnz	b2			; no, try as a valid char...

b15:
	inc	count			; one more character to s...
	dec	chrptr			; back up over separator
b1:
	mov	ax,bx			; get proper count
	or	ax,ax			; clears carry, sets zero accordingly
	pop	bx
	ret
badnum:
	mov	sepchr,0
	xor	ax,ax		; set zero flag, and ax = 0
	pop	bx
	stc			; and carry set
	ret

getnum	endp

;*****************************************************************

setdoscountryinfo	proc	near

;input: es:di -> pointer to dos_country_cdpg_info
;	ds:0  -> buffer.
;	si = 0
;	ax = country id
;	dx = code page id. (if 0, then use ccsyscodepage as a default.)
;	bx = file handle
;	this routine can handle maxium 438 country_data entries.
;
;output: dos_country_cdpg_info set.
;	 carry set if any file read failure or wrong information in the file.
;	 carry set and cx = -1 if cannot find the matching country_id, codepage
;	 _id in the file.

	push	di
	push	ax
	push	dx

	xor	cx,cx
	xor	dx,dx
	mov	ax,512			;read 512 bytes
	call	readincontrolbuffer	;read the file header
	jc	setdosdata_fail

	push	es
	push	si

	push	cs
	pop	es

	mov	di,offset country_file_signature
	mov	cx,8			;length of the signature
	repz	cmpsb

	pop	si
	pop	es
	jnz	setdosdata_fail 	;signature mismatch

	add	si,18			;si -> county info type
	cmp	byte ptr ds:[si],1	;only accept type 1 (currently only 1 header type)
	jne	setdosdata_fail 	;cannot proceed. error return

	inc	si			;si -> file offset
	mov	dx,word ptr ds:[si]	;get the info file offset.
	mov	cx,word ptr ds:[si+2]
	mov	ax,6144			;read 6144 bytes.
	call	readincontrolbuffer	;read info
	jc	setdosdata_fail

	mov	cx, word ptr ds:[si]	;get the # of country, codepage combination entries
	cmp	cx, 438			;cannot handle more than 438 entries.
					;	
	ja	setdosdata_fail

	inc	si
	inc	si			;si -> entry information packet
	pop	dx			;restore code page id
	pop	ax			;restore country id
	pop	di

setdoscntry_find:			;search for desired country_id,codepage_id.
	cmp	ax, word ptr ds:[si+2]	;compare country_id
	jne	setdoscntry_next

	cmp	dx, 0			;no user specified code page ?
	je	setdoscntry_any_codepage;then no need to match code page id.
	cmp	dx, word ptr ds:[si+4]	;compare code page id
	je	setdoscntry_got_it

setdoscntry_next:
	add	si, word ptr ds:[si]	;next entry
	inc	si
	inc	si			;take a word for size of entry itself
	loop	setdoscntry_find

	mov	cx, -1			;signals that bad country id entered.
setdoscntry_fail:
	stc
	ret

setdosdata_fail:
	pop	si
	pop	cx
	pop	di
	jmp	short setdoscntry_fail

setdoscntry_any_codepage:		;use the code_page_id of the country_id found.
	mov	dx, word ptr ds:[si+4]

setdoscntry_got_it:			;found the matching entry
	mov	cs:cntrycodepage_id, dx ;save code page id for this country.
	mov	dx, word ptr ds:[si+10] ;get the file offset of country data
	mov	cx, word ptr ds:[si+12]
	mov	ax, 512 		;read 512 bytes
	call	readincontrolbuffer
	jc	setdoscntry_fail

	mov	cx, word ptr ds:[si]	;get the number of entries to handle.
	inc	si
	inc	si			;si -> first entry

setdoscntry_data:
	push	di			;es:di -> dos_country_cdpg_info
	push	cx			;save # of entry left
	push	si			;si -> current entry in control buffer

	mov	al, byte ptr ds:[si+2]	;get data entry id
	call	getcountrydestination	;get the address of destination in es:di
	jc	setdoscntry_data_next	;no matching data entry id in dos

	mov	dx, word ptr ds:[si+4]	;get offset of data
	mov	cx, word ptr ds:[si+6]
	mov	ax,4200h
	stc
	int	21h			;move pointer
	jc	setdosdata_fail

	mov	dx,512			;start of data buffer
	mov	cx,20			;read 20 bytes only. we only need to
	mov	ah,3fh			;look at the length of the data in the file.
	stc
	int	21h			;read the country.sys data
	jc	setdosdata_fail 	;read failure

	cmp	ax,cx
	jne	setdosdata_fail

	mov	dx,word ptr ds:[si+4]	;get offset of data again.
	mov	cx,word ptr ds:[si+6]
	mov	ax,4200h
	stc
	int	21h			;move pointer back again
	jc	setdosdata_fail

	push	si
	mov	si,(512+8)		;get length of the data from the file
	mov	cx,word ptr ds:[si]
	pop	si
	mov	dx,512			;start of data buffer
	add	cx,10			;signature + a word for the length itself
	mov	ah,3fh			;read the data from the file.
	stc
	int	21h
	jc	setdosdata_fail

	cmp	ax, cx
	jne	setdosdata_fail

	mov	al,byte ptr ds:[si+2]	;save data id for future use.
	mov	si,(512+8)		;si-> data buffer + id tag field
	mov	cx,word ptr ds:[si]	;get the length of the file
	inc	cx			;take care of a word for lenght of tab
	inc	cx			;itself.
	cmp	cx,(2048 - 512 - 8)	;fit into the buffer?
	ja	setdosdata_fail

	if	bugfix
	call	setdbcs_before_copy
	endif

	cmp	al, setcountryinfo	;is the data for setcountryinfo table?
	jne	setdoscntry_mov 	;no, don't worry

	push	word ptr es:[di+ccmono_ptr-cccountryinfolen]	;cannot destroy ccmono_ptr address. save them.
	push	word ptr es:[di+ccmono_ptr-cccountryinfolen+2]	;at this time di -> cccountryinfolen
	push	di			;save di

	push	ax
	mov	ax,cs:cntrycodepage_id	;do not use the code page info in country_info
	mov	ds:[si+4], ax		;use the saved one for this !!!!
	pop	ax

setdoscntry_mov:
	rep	movsb			;copy the table into dos
	cmp	al, setcountryinfo	;was the ccmono_ptr saved?
	jne	setdoscntry_data_next

	pop	di			;restore di
	pop	word ptr es:[di+ccmono_ptr-cccountryinfolen+2]	 ;restore
	pop	word ptr es:[di+ccmono_ptr-cccountryinfolen]

setdoscntry_data_next:
	pop	si			;restore control buffer pointer
	pop	cx			;restore # of entries left
	pop	di			;restore pointer to dso_country_cdpg
	add	si, word ptr ds:[si]	;try to get the next entry
	inc	si
	inc	si			;take a word of entry length itself
	dec	cx
	cmp	cx,0
	je	setdoscntry_ok
	jmp	setdoscntry_data

setdoscntry_ok:
	ret
setdoscountryinfo	endp

	if	bugfix
setdbcs_before_copy	proc	near

	cmp	al,setdbcs		; dbcs vector set?
	jnz	@f			; jump if not
	cmp	word ptr es:[di], 0	; zero byte data block?
	jz	@f			; jump if so

	push	di
	push	ax
	push	cx
	mov	cx,es:[di]		; load block length
	add	di,2			; points actual data
	xor	al,al			; fill bytes
	rep	stosb			; clear data block
	pop	cx
	pop	ax
	pop	di
@@:
	ret
setdbcs_before_copy	endp
	endif

getcountrydestination	proc	near

;get the destination address in the dos country info table.
;input: al - data id
;	es:di -> dos_country_cdpg_info
;on return:
;	es:di -> destination address of the matching data id
;	carry set if no matching data id found in dos.

	push	cx
	add	di,ccnumber_of_entries	;skip the reserved area, syscodepage etc.
	mov	cx,word ptr es:[di]	;get the number of entries
	inc	di
	inc	di			;si -> the first start entry id

getcntrydest:
	cmp	byte ptr es:[di],al
	je	getcntrydest_ok
	cmp	byte ptr es:[di],setcountryinfo ;was it setcountryinfo entry?
	je	getcntrydest_1

	add	di,5			;next data id
	jmp	short getcntrydest_loop

getcntrydest_1:
	add	di,new_country_size + 3 ;next data id
getcntrydest_loop:
	loop	getcntrydest
	stc
	jmp	short getcntrydest_exit

getcntrydest_ok:
	cmp	al,setcountryinfo	;select country info?
	jne	getcntrydest_ok1

	inc	di			;now di -> cccountryinfolen
	jmp	short getcntrydest_exit

getcntrydest_ok1:
	les	di,dword ptr es:[di+1]	;get the destination in es:di

getcntrydest_exit:
	pop	cx
	ret
getcountrydestination	endp


readincontrolbuffer	proc	near

;move file pointer to cx:dx
;read ax bytes into the control buffer. (should be less than 2 kb)
;si will be set to 0 hence ds:si points to the control buffer.
;entry:  cx,dx offset from the start of the file where the read/write pointer
;	 be moved.
;	 ax - # of bytes to read
;	 bx - file handle
;	 ds - buffer seg.
;return: the control data information is read into ds:0 - ds:0200.
;	 cx,dx value destroyed.
;	 carry set if error in reading file.

	push	ax			;# of bytes to read
	mov	ax, 4200h
	stc
	int	21h			;move pointer
	pop	cx			;# of bytes to read
	jc	ricb_exit

	xor	dx,dx			;ds:dx -> control buffer
	xor	si,si
	mov	ah,3fh			;read into the buffer
	stc
	int	21h			;should be less than 1024 bytes.

ricb_exit:
	ret
readincontrolbuffer	endp


set_country_path	proc	near

;in:  ds - sysinitseg, es - confbot, si -> start of the asciiz path string
;     dosinfo_ext, cntry_drv, cntry_root, cntry_path
;     assumes current directory is the root directory.
;out: ds:di -> full path (cntry_drv).
;     set the cntry_drv string from the country=,,path command.
;     ds, es, si value saved.

	push	si

	push	ds			;switch ds, es
	push	es
	pop	ds
	pop	es			;now ds -> confbot, es -> sysinitseg

	call	chk_drive_letter	;current ds:[si] is a drive letter?
	jc	scp_default_drv 	;no, use current default drive.

	mov	al, byte ptr ds:[si]
	inc	si
	inc	si			;si -> next char after ":"
	jmp	short scp_setdrv

scp_default_drv:
	mov	ah, 19h
	int	21h
	add	al, "A"			;convert it to a character.

scp_setdrv:
	mov	cs:cntry_drv, al	;set the drive letter.
	mov	di, offset cntry_path
	mov	al, byte ptr ds:[si]
	cmp	al, "\"
	je	scp_root_dir

	cmp	al,"/"			;let's accept "/" as an directory delim
	je	scp_root_dir

	jmp	short scp_path

scp_root_dir:
	dec	di			;di -> cntry_root
scp_path:
	call	move_asciiz		;copy it

	mov	di, offset cntry_drv
scpath_exit:

	push	ds			;switch ds, es
	push	es
	pop	ds
	pop	es			;ds, es value restored

	pop	si
	ret
set_country_path	endp


chk_drive_letter	proc	near
;check if ds:[si] is a drive letter followed by ":".
;assume that every alpha charater is already converted to upper case.
;carry set if not.

	push	ax
	cmp	byte ptr ds:[si], "A"
	jb	cdletter_no
	cmp	byte ptr ds:[si], "Z"
	ja	cdletter_no
	cmp	byte ptr ds:[si+1], ":"
	jne	cdletter_no

	jmp	short cdletter_exit

cdletter_no:
	stc

cdletter_exit:
	pop	ax
	ret
chk_drive_letter	endp


move_asciiz	proc	near
;in: ds:si -> source es:di -> target
;out: copy the string until 0.
;assumes there exists a 0.

masciiz_loop:
	movsb
	cmp	byte ptr ds:[si-1],0	;was it 0?
	jne	masciiz_loop
	ret
move_asciiz	endp

ENDIF ; CONFIGPROC

;
;	ds:dx points to string to output (asciz)
;
;	prints <badld_pre> <string> <badld_post>

badfil:
	push	cs
	pop	es

	mov	si,dx
badload:
	mov	dx,offset badld_pre	;want to print config error
	mov	bx, offset crlfm

prnerr:
	push	cs
	pop	ds
	call	print

prn1:
	mov	dl,es:[si]
	or	dl,dl
	jz	prn2
	mov	ah,std_con_output
	int	21h
	inc	si
	jmp	prn1

prn2:
	mov	dx,bx
	call	print

IFDEF	CONFIGPROC
	cmp	donotshownum,1		; suppress line number when handling command.com
	je	prnexit
	call	error_line
ENDIF ; CONFIGPROC

prnexit:
	ret

print:
	mov	ah,std_con_string_output
	int	21h
	ret


	if	noexec

; load non exe file called [ds:dx] at memory location es:bx

ldfil:
	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	ds
	push	bx

	xor	ax,ax			;open the file
	mov	ah,open
	stc				;in case of int 24
	int	21h
	pop	dx			;clean stack in case jump
	jc	ldret

	push	dx
	mov	bx,ax			;handle in bx
	xor	cx,cx
	xor	dx,dx
	mov	ax,(lseek shl 8) or 2
	stc				;in case of int 24
	int	21h			; get file size in dx:ax
	jc	ldclsp

	or	dx,dx
	jnz	lderrp			; file >64k
	pop	dx

	push	dx
	mov	cx,es			; cx:dx is xaddr
	add	dx,ax			; add file size to xaddr
	jnc	dosize
	add	cx,1000h		; ripple carry
dosize:
	mov	ax,dx
	call	pararound
	mov	dx,ax

	add	cx,dx
	cmp	cx,[alloclim]
	jb	okld
	jmp	mem_err

okld:
	xor	cx,cx
	xor	dx,dx
	mov	ax,lseek shl 8		;reset pointer to beginning of file
	stc				;in case of int 24
	int	21h
	jc	ldclsp

	pop	dx

	push	es			;read the file in
	pop	ds			;trans addr is ds:dx

	mov	cx,0ff00h		; .com files arn't any bigger than
					; 64k-100h
	mov	ah,read
	stc				;in case of int 24
	int	21h
	jc	ldcls

	mov	si,dx			;check for exe file
	cmp	word ptr [si],"ZM"
	clc				; assume ok
	jnz	ldcls			; only know how to do .com files

	stc
	jmp	short ldcls

lderrp:
	stc
ldclsp:
	pop	dx			;clean stack
ldcls:
	pushf
	mov	ah,close		;close the file
	stc
	int	21h
	popf

ldret:	pop	ds
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
	endif

;
;  open device pointed to by dx, al has access code
;   if unable to open do a device open null device instead
;
open_dev:
	call	open_file
	jnc	open_dev3

open_dev1:
	mov	dx,offset nuldev
	call	open_file
of_ret:
	ret

open_dev3:
	mov	bx,ax			; handle from open to bx
	xor	ax,ax			; get device info
	mov	ah,ioctl
	int	21h
	test	dl,10000000b
	jnz	of_ret

	mov	ah,close
	int	21h
	jmp	open_dev1

open_file:
	mov	ah,open
	stc
	int	21h
	ret

; test int24. return back to dos with the fake user response of "fail"

int24:
	mov	al, 3			; fail the system call
	iret				; return back to dos.

include copyrigh.inc			; copyright statement


nuldev	db	"NUL",0
condev	db	"CON",0
auxdev	db	"AUX",0
prndev	db	"PRN",0

IFDEF	CONFIGPROC
config	db	"\CONFIG.SYS",0

cntry_drv   db	  "A:"
cntry_root  db	  "\"
cntry_path  db	  "COUNTRY.SYS",0
	    db	  52 dup (0)

country_file_signature db 0ffh,'COUNTRY'

cntrycodepage_id dw ?

ENDIF ; CONFIGPROC

ifdef   MULTI_CONFIG
newcmd  db      0                       ; non-zero if non-std shell specified
tmplate db      64                      ; must precede commnd
endif

ifdef	ROMEXEC
        db      7                       ; size of commnd line (excl. null)
commnd	db	"COMMAND",0
	db	56 dup (0)
else
        db      12                      ; size of commnd line (excl. null)
commnd	db	"\COMMAND.COM",0
	db	51 dup (0)
endif

ifdef   MULTI_CONFIG
commnd2 db      "\COMMAND.COM",0        ; alternate commands to exec,
        db      2,"/P",0                ; followed by their respective alternate
commnd3 db      "\MSDOS\COMMAND.COM",0  ; command lines
        db      11,"A:\MSDOS /P",0      ;(the drive letter are dynamically replaced)
commnd4 db      "\DOS\COMMAND.COM",0    ;
        db      9,"A:\DOS /P",0         ;

        db      0                       ; default switchchar (referenced as command_line-1)
endif

command_line    db  2,"/P"              ; default command.com args
        db      125 dup (0)

IFDEF	CONFIGPROC

pathstring db	64 dup (0)

ifdef   MULTI_CONFIG
;
;   Beware of byte pairs accessed as words (see all "KEEP AFTER" notes below)
;
bMenuColor      db      07h ;1Fh        ; default fgnd/bgnd color
bMenuPage       db      0               ; menu video page (KEEP AFTER bMenuColor)
                db      5               ; video page function # (KEEP AFTER bMenuPage)
bLastCol        db      ?               ; ending column on status line
bLastRow        db      24              ; row # of status line (KEEP AFTER bLastCol)
bDisableUI      db      0               ; 1=disable clean/interactive
                                        ; 2=disable default 2-second delay
bCRTPage        db      ?               ; value saved from BIOS data area
wCRTStart       dw      ?               ; value saved from BIOS data area
bQueryOpt       db      0               ; 0=off, 1=prompt all, 2=prompt none, 4=skip all
bDefBlock       db      1               ; default block #
bMaxBlock       db      ?               ; maxmimum block #
offDefBlock     dw      0               ; offset of name of default block (if any)
secTimeOut      db      -1              ; # of seconds for timeout (-1 == indefinite)
secElapsed      db      0               ; # of seconds elapsed so far (KEEP AFTER secTimeOut)
abBlockType     db  MAX_MULTI_CONFIG+1 dup(?)   ; array of block types
aoffBlockName   dw  MAX_MULTI_CONFIG+1 dup(?)   ; array of offsets of block names
aoffBlockDesc   dw  MAX_MULTI_CONFIG+1 dup(?)   ; array of offsets of block descriptions

szBoot          db      "CONFIG=",0
szMenu          db      "MENU",0
szCommon        db      "COMMON",0

endif  ;MULTI_CONFIG

comtab	label	byte

;            cmd len    command         cmd code
;            -------    -------         --------
;
ifdef MULTI_CONFIG
        db      1,      "[",            CONFIG_BEGIN
endif
        db      5,      "BREAK",        CONFIG_BREAK
        db      7,      "BUFFERS",      CONFIG_BUFFERS
        db      7,      "COMMENT",      CONFIG_COMMENT
        db      7,      "COUNTRY",      CONFIG_COUNTRY
        db      6,      "DEVICE",       CONFIG_DEVICE
        db      10,     "DEVICEHIGH",   CONFIG_DEVICEHIGH
        db      3,      "DOS",          CONFIG_DOS
        db      8,      "DRIVPARM",     CONFIG_DRIVPARM
        db      4,      "FCBS",         CONFIG_FCBS
        db      5,      "FILES",        CONFIG_FILES
ifdef MULTI_CONFIG
        db      7,      "INCLUDE",      CONFIG_INCLUDE
endif
        db      7,      "INSTALL",      CONFIG_INSTALL
        db      11,     "INSTALLHIGH",  CONFIG_INSTALLHIGH
        db      9,      "LASTDRIVE",    CONFIG_LASTDRIVE
ifdef MULTI_CONFIG
        db      7,      "SUBMENU",      CONFIG_SUBMENU
        db      9,      "MENUCOLOR",    CONFIG_MENUCOLOR
        db      11,     "MENUDEFAULT",  CONFIG_MENUDEFAULT
        db      8,      "MENUITEM",     CONFIG_MENUITEM
endif
        db      10,     "MULTITRACK",   CONFIG_MULTITRACK
ifdef MULTI_CONFIG
        db      7,      "NUMLOCK",      CONFIG_NUMLOCK
endif
        db      3,      "REM",          CONFIG_REM
ifdef MULTI_CONFIG
        db      3,      "SET",          CONFIG_SET
endif
        db      5,      "SHELL",        CONFIG_SHELL
if    STACKSW
        db      6,      "STACKS",       CONFIG_STACKS
endif
        db      8,      "SWITCHES",     CONFIG_SWITCHES
	db	0

ENDIF  ;CONFIGPROC

public deviceparameters
deviceparameters a_deviceparameters <0,dev_3inch720kb,0,80>

IFDEF	CONFIGPROC

hlim	    dw	    2
slim	    dw	    9

public drive
drive	db	?

public switches
switches    dw	0


; the following are the recommended bpbs for the media that we know of so
; far.

; 48 tpi diskettes

bpb48t	dw	512
	db	2
	dw	1
	db	2
	dw	112
	dw	2*9*40
	db	0fdh
	dw	2
	dw	9
	dw	2
	dd	0
        dd      0

; 96tpi diskettes

bpb96t	dw	512
	db	1
	dw	1
	db	2
	dw	224
	dw	2*15*80
	db	0f9h
	dw	7
	dw	15
	dw	2
	dd	0
        dd      0

; 3 1/2 inch diskette bpb

bpb35	dw	512
	db	2
	dw	1
	db	2
	dw	70h
	dw	2*9*80
	db	0f9h
	dw	3
	dw	9
	dw	2
	dd	0
        dd      0
      
bpb35h	dw	0200h
	db	01h
	dw	0001h
	db	02h
	dw	0e0h
	dw	0b40h
	db	0f0h
	dw	0009h
	dw	0012h
	dw	0002h
	dd	0
        dd      0

;
; m037 - BEGIN
;
bpb288	dw	0200h
	db	02h
	dw	0001h
	db	02h
	dw	240
	dw	2*36*80
	db	0f0h
	dw	0009h
	dw	36
	dw	0002h
	dd	0
        dd      0
;
; m037 - END
;
bpbtable    dw	    bpb48t		; 48tpi drives
	    dw	    bpb96t		; 96tpi drives
	    dw	    bpb35		; 3.5" drives
; the following are not supported, so default to 3.5" media layout
	    dw	    bpb35		; not used - 8" drives
	    dw	    bpb35		; not used - 8" drives
	    dw	    bpb35		; not used - hard files
	    dw	    bpb35		; not used - tape drives
	    dw	    bpb35h		; 3-1/2" 1.44mb drive
	    dw	    bpb35		; ERIMO				m037
	    dw	    bpb288		; 2.88 MB diskette drives	m037

switchlist  db	8,"FHSTDICN"	     ; preserve the positions of n and c.

; the following depend on the positions of the various letters in switchlist

;switchnum	equ 11111000b		; which switches require number

flagec35	equ 00000100b		; electrically compatible 3.5 inch disk drive
flagdrive	equ 00001000b
flagcyln	equ 00010000b
flagseclim	equ 00100000b
flagheads	equ 01000000b
flagff		equ 10000000b
ENDIF ; CONFIGPROC
sysinitseg	ends
	end

