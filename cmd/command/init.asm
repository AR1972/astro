 	page ,132
	title	COMMAND Initialization
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
;	Revision History
;	================
;	M002	SR	07/15/90	Resize right at the start because
;				Carousel depends on it.
;
;	M004	SR	07/17/90	Initialization reworked so that 
;				transient is now moved at EndInit.
;				The old approach assumed that the
;				biggest block is the one currently 
;				loaded in, an assumption not true
;				for UMBs.
;
;	M005	SR	07/20/90	Numerous hacks for Carousel
;				1. Set CurrentPDB to ours at start
;				2. Normalize cs:ip of int 2fh hook
;				so that cs is different.
;
;	M013	SR	08/06/90	Fixed Setup_res_end & Move_res_code
;				to use new GetVersion call that
;				returns info about whether DOS is in
;				HMA or not.
;
;	M015	SR	08/09/90	Increased default environment size to
;				256 bytes from 160 bytes
;
;	M026	SR	9/12/90	Fixed environment trashing on second
;				Command if new comspec is given.
;
;	M030	SR	10/3/90	Before calling int 2fh 4a02h, set di
;				to 0ffffh so that we are ok if no one
;				answers this int 2fh.
;
;	M042	SR	12/13/90	Bug #4660. Changed setup_res_end to
;				take care of the dummy segment that
;				adds a para to the resident size.
;

.xlist
.xcref
	include comsw.asm
	include dossym.inc
	include pdb.inc
	include mult.inc
	include syscall.inc
	include doscntry.inc
	include comseg.asm
	include comequ.asm
	include resmsg.equ

	include envdata.asm
	include xmm.inc
.list
.cref



ENVBIG			equ	32768
ENVSML			equ	256		;Increased to 256 ; M015
KOREA_COUNTRY_CODE	equ	82


CODERES segment public byte

	extrn	ContC		:near
	extrn	DskErr		:near
	extrn	Int_2e		:near
	extrn	LodCom		:near
	extrn	MsgInt2fHandler	:far
	extrn	SetVect	:near
	extrn	ChkSum		:near
	extrn	CrLf		:near
	extrn	LoadCom	:near
	extrn	RPrint		:near

	extrn	EndCode	:byte
	extrn	StartCode	:byte

	ifdef	DBCS
	extrn	ItestKanj	:near
	endif

	extrn	BadMemErr	:near

CODERES ends

DATARES segment public byte

	extrn	Abort_Char	:byte
	extrn	AccDen		:byte
	extrn	Append_State	:word	
	extrn	Batch		:word
	extrn	Com_Fcb1	:dword
	extrn	Com_Fcb2	:dword
	extrn	Com_Ptr		:dword
	extrn	ComDrv		:byte
	extrn	ComSpec		:byte
	extrn	ComSpec_End	:word
	extrn	Crit_Msg_Off	:word	
	extrn	Crit_Msg_Seg	:word	
	extrn	DataResEnd	:byte 	
	extrn	Dbcs_Vector_Addr:word	
	extrn	EchoFlag	:byte
	extrn	EnvirSeg	:word
	extrn	ExtMsgEnd	:byte	
	extrn	fFail		:byte
	extrn	FUCase_addr	:word	
	extrn	InitFlag	:byte
	extrn	Int2fHandler	:dword
	extrn	Io_Save		:word
	extrn	LTpa		:word	
	extrn	MemSiz		:word
	extrn	MySeg		:word	
	extrn	MySeg1		:word
	extrn	MySeg2		:word
	extrn	MySeg3		:word
	extrn	Nest		:word
	extrn	OldTerm	:dword
	extrn	Parent		:word
	extrn	ParseMes_Ptr	:word	
	extrn	ParsMsgPtrs	:word
        extrn   PermCom         :byte
        extrn   SemiPermCom     :word
	extrn	PutBackDrv	:byte
	extrn	PutBackComSpec	:byte
	extrn	RDirChar	:byte
	extrn	Res_Tpa	:word
	extrn	ResMsgEnd	:word	    
	extrn	RSwitChar	:byte
	extrn	SingleCom	:word
	extrn	Sum		:word
	extrn	TrnSeg		:word
	extrn	TrnMvFlg	:byte

	extrn	ResSize	:word
	extrn	RStack		:word

	extrn	ComInHMA	:byte	;flag set if in HMA
	extrn	XMMCallAddr	:dword	;far call address to XMM

;;ifdef	ROMDOS
;;;	If LoadFromROM is in DATARES..
;;	extrn	LoadFromROM_seg	:word
;;endif

;
;All far pointers to resident routines that are to be patched
;
	extrn	Int2f_Entry	:dword
	extrn	Int2e_Entry	:dword
	extrn	Ctrlc_Entry	:dword
	extrn	CritErr_Entry	:dword

	extrn	Int2f_Trap	:near
	extrn	Int2e_Trap	:near
	extrn	Ctrlc_Trap	:near
	extrn	CritErr_Trap	:near
	extrn	LodCom_Trap	:near

	extrn	EndInit	:near

	extrn	Carousel_i2f_Hook	:byte	; M005

ifdef	BETA3WARN
	%out	Take this out before we ship
	extrn	Beta3Warned:byte
endif


DATARES ends


TAIL	segment public para

	extrn	TranStart	:word

TAIL	ends


TRANCODE	segment public byte

	extrn	DatInit	:far

TRANCODE	ends

TRANDATA	segment

	extrn	TranDataEnd	:byte

TRANDATA	ends

TRANSPACE	segment public byte

	extrn	TranSpaceEnd	:byte

TRANSPACE	ends




ifdef ROMDOS

;----------------------------------------------------------------------------
; Image of part of the BIOS data segment, named Bdata.
; Defines the following label:
;
;	BootFlags =	Boot options from CMOS RAM

include	msbdata.inc

; BootFlags bit we're concerned with:

BF_NoConfig	=	00000001b	; No config.sys processing

endif ; ROMDOS




; *******************************************************************
; START OF INIT PORTION
; This code is deallocated after initialization.

INIT	SEGMENT PUBLIC PARA

	extrn	AutoBat	:byte
	extrn	BadComAccMsg	:byte
	extrn	BadComLkMsg	:byte
	extrn	Badcspfl	:byte
	extrn	BadVerMsg	:byte
        extrn   AllocedEnv      :byte
	extrn	Command_?_syn	:byte	
	extrn	Command_c_syn	:byte	
        extrn   Command_k_syn   :byte
	extrn	Command_d_syn	:byte	
	extrn	Command_e_syn	:byte	
	extrn	Command_f_syn	:byte	
	extrn	Command_m_syn	:byte	
	extrn	Command_p_syn	:byte	
	extrn	Comnd1_syn	:word 	
	extrn	Comnd1_addr	:dword	
	extrn	ComSpect	:byte
	extrn	CopyrightMsg	:byte
	extrn	Dswitch	:byte	
	extrn	EnvMax		:word
	extrn	EnvSiz		:word
	extrn	EqualSign	:byte
	extrn	Eswitch	:byte	
	extrn	Ext_msg	:byte	
	extrn	HelpMsgs:word
	extrn	InitAdd	:dword
	extrn	InitEnd	:word
	extrn	Init_Parse	:dword	
	extrn	Internat_Info	:byte	
	extrn	KautoBat	:byte	
	extrn	Lcasea		:byte
	extrn	Lcasez		:byte
	extrn	Num_positionals	:word	
	extrn	OldEnv		:word
	extrn	Old_parse_ptr	:word	
	extrn	OutEnvMsg	:byte
	extrn	Parse_command	:byte	
	extrn	PrdAttm	:byte
	extrn	ResetEnv	:word	
	extrn	Scswitch	:byte
        extrn   Skswitch        :byte
	extrn	Space		:byte
	extrn	Triage_Add	:dword	
	extrn	TrnSize	:word
	extrn	Ucasea		:byte
	extrn	UsedEnv	:word

	extrn	PathString	:byte
        extrn   PathStrLen      :abs
        extrn   DefPathString   :byte
        extrn   DefPathStrLen   :abs
        extrn   DefPath2String  :byte
        extrn   DefPath2StrLen  :abs
        extrn   PrmptString     :byte
        extrn   PrmptStrLen     :abs
        extrn   PrmptStrLen2    :abs
        extrn   ComspOffset     :word
	extrn	ComspString	:byte
        extrn   ComspStrLen     :abs
        extrn   ComspStrLen2    :abs
	extrn	Reloc_Table	:word
	extrn	FirstCom	:byte
	extrn	ResJmpTable	:dword

	extrn	TriageError	:near

	extrn	NUM_RELOC_ENTRIES	:abs

	extrn	DevFlag	:byte
	extrn	PathFlag	:byte

	PUBLIC	ConProc
	PUBLIC	Init_ContC_SpecialCase

	assume	cs:ResGroup,ds:ResGroup,es:ResGroup,ss:ResGroup

	org	0
ZERO	=	$


ConProc:
	mov	sp,offset ResGroup:RStack	; must be first instruction
;
; We need to set the PSP to us right at start because Carousel needs
; to be lied to and it does not set PSP when it transfers control to
; us after loading us as an overlay. By setting PSP, we ensure that
; command.com is also not lied to.
;
        mov     ah,SET_CURRENT_PDB
        mov     bx,es
        int     21h

        mov     ax,GET_VERSION SHL 8
	int	21h
	cmp	ax,EXPECTED_VERSION
	je	OkDos				; DOS version is ok

	mov	dx,offset ResGroup:BadVerMsg	; DX = ptr to msg
	call	RPrint
	mov	ax,es
	cmp	es:PDB_Parent_Pid,ax		; if COMMAND is own parent,
Here:	jz	Here				;  loop forever
	
	int	20h				; otherwise, exit
okdos:

;
;  Calculate and save the end of the INIT segment (which is also
;  the beginning of TRANGROUP).
;

        mov     dx,offset resgroup:TranStart+15 ; get end of init code
        mov     cl,4                            ; change to paragraphs
        shr     dx,cl                           ;
        mov     ax,cs                           ; get current segment
        add     ax,dx                           ; calculate segment of end of init
        mov     InitEnd,ax                      ; save this

;
;  Check for /? on the command line.  If found, display help text
;  and exit.
;
;  NOTE:  this routine may terminate the program, never returning.
;
	call	CheckHelp

;
; We have to patch the segment values for the various interrupt entry points.
; This is because we need to have the default addresses of the handlers in our
; stub before the relocation is done. These values will then be changed once
; the resident is relocated
;
	call	patch_segs

;
;  Turn APPEND off during initialization processing
;
        mov     ax,APPENDINSTALL                ; see if append installed
	int	2fh				;
	cmp	al,0				; append installed?
	je	set_msg_addr			; no - continue
	mov	ax,APPENDDOS			; see if append DOS version right
	int	2fh				;
	cmp	ax,-1				; append version correct?
	jne	set_msg_addr			; no - continue
        mov     ax,APPENDGETSTATE               ; Get the state of Append
	int	2fh				;
        mov     Append_State,bx                 ; save append state
        xor     bx,bx                           ; clear out state
        mov     ax,APPENDSETSTATE               ; Set the state of Append
	int	2fh				; set everything off

set_msg_addr:
	mov	di,offset resgroup:DataresEnd 	; get address of resident end
	mov	ResMsgEnd,di			; save it


        call    get_XMMAddr                     ; get XMM call address
;
; Check if this is the first instance of command.com. If not, we just exit
; this routine without moving any code.
; After the int 2fh, ds:si points at the resident jump table in the previous
; stub. We just have to copy this over
;
	
ifndef ROMDOS
	mov	ax,GET_COMMAND_STATE	
else
	mov	ax,GET_ROMCOMMAND_STATE	
endif ; ROMDOS

	int	2fh
	assume	ds:nothing

	or	ax,ax
	jnz	first_com			;this is the first instance

ifdef	BETA3WARN
	%out	Take this out before we ship
	mov	es:Beta3Warned, 0ffh
endif

	mov	word ptr es:ResJmpTable,si		;save old stub jump table
	mov	word ptr es:ResJmpTable+2,ds
	jmp	short init_cntry

first_com:
	mov	es:FirstCom,1			;indicate first command.com

init_cntry:
	push	es
	pop	ds
	assume	ds:RESGROUP


	mov	ah,GETEXTCNTRY			; get extended country info
	mov	al,4				; get file ucase table
	mov	dx,-1				;
	mov	bx,-1				;
	mov	cx,5				; number of bytes we want
	mov	di,offset resgroup:Fucase_addr	; buffer for address
	int	21h				;

;	Bugbug:	conditionalize dbcs_vector stuff?
	push	ds				;
	mov	ax, (ECS_CALL shl 8) or GETLEADBTBL ;
	int	21h				;
	mov	bx,ds				; get segment to bx
	pop	ds				;
	mov	Dbcs_vector_addr,si			; save address of
	mov	Dbcs_vector_addr+2,bx			; dbcs vector


	mov	ax,word ptr ds:PDB_Parent_Pid 	; Init PARENT so we can exit
	mov	Parent,ax			;  correctly.
	mov	ax,word ptr ds:Pdb_Exit
	mov	word ptr OldTerm,ax
	mov	ax,word ptr ds:Pdb_Exit+2
	mov	word ptr Oldterm+2,ax


	mov	ax,offset ResGroup:EndCode + 15
	mov	cl,4				; ax = size of resident part of
	shr	ax,cl				;  command in paragraphs.  Add
	mov	cx,cs				;  this to CS and you get the
	add	ax,cx				;  segment of the TPA.

	mov	Res_tpa, ax			; Temporarily save the TPA segment
	and	ax, 0f000h
	add	ax, 01000h			; Round up to next 64K boundary
	jnc	TpaSet				; Memory wrap if carry set
	mov	ax, Res_tpa
TpaSet:
	mov	Ltpa,ax			; Good enough for the moment
	mov	ax,word ptr ds:PDB_Block_Len	; ax = # of paras given to command

	mov	Myseg1,ds			; These 3 variables are used as part of
	mov	Myseg2,ds			;  3 long ptrs that the transient will
	mov	Myseg,ds			;  use to call resident routines.
	mov	Myseg3,ds			; segment of msg retriever routine

	mov	Memsiz,ax			; Needed for execing other programs

;
; First reallocate the COMMAND size to its memory image
;
        push    ax                              ;
        mov     bx,offset RESGROUP:TranStart    ;
        add     bx,offset TRANGROUP:TranSpaceEnd;
        add     bx,15                           ; round up the size

        mov     cl,4                            ;
        shr     bx,cl                           ; size of command.com

        mov     ah,SETBLOCK                     ; free all memory above pgm
        int     21h                             ;
        pop     ax                              ;


;
; Compute maximum size of environment
;
        mov     EnvMax,(SIZE Environment + 15) / 16 + (EnvMaximum-zero + 15)/16 - 1
;
; Compute minimum size of environment
;

	mov	EnvSiz, ENVSML / 16

	mov	dx,offset TranGroup:Transpaceend + 15 ; dx = size of transient
	mov	cl,4				;  in paragraphs.
	shr	dx,cl
        mov     Trnsize,dx                      ; save size of transient in paragraphs

	sub	ax,dx				; max seg addr - # para's needed for transient
	mov	Trnseg,ax			;  = seg addr to load the transient at.
	mov	ax,ds:PDB_Environ			; ax = environment segment
        mov     EnvirSeg,ax                     ;
        or      ax,ax                           ; if there is no environment segment,
        jz      buildenv                        ; make one

        cmp     FirstCom,0                      ; if this is the first command.com,
        je      environpassed                   ; do a merge job (make sure COMSPEC exists)
;
; We allocate a buffer here just large enough to hold the 'PATH=' and
; the COMSPEC. After parsing, we will allocate an environment of the right
; size and free this buffer. We need this buffer because we no longer have an
; ENVIRONMENT segment but need a place to store the COMSPEC which can be
; given on the command line before we know the environment size. This routine
; will not return in case of an allocation error. It will either exit or hang
; depending on whether or not this is the first COMMAND.COM or not.
;
buildenv:
        call    alloc_env                       ; try to allocate buffer

environpassed:
        mov     es,ax                           ; and it load into es.
	assume	es:nothing

gottheenvir:
;
; Initialize the command drive
;
	mov	ah,GET_DEFAULT_DRIVE
	int	21h
	inc	al
	mov	Comdrv,al

        mov     al,byte ptr ds:Fcb              ; al = default drive number for command
	or	al,al
	jz	nocomdrv			; no drive specified

	mov	ah,':'
	mov	Comdrv,al
	add	al,40h				; convert number to uppercase character

ifndef	ROMDOS

;	(Don't add drive specifier to ROM COMMAND COMSPEC.)

	std
        cmp     AllocedEnv,0                    ; if a new environment is being built,
        je      notwidenv                       ;  move the default comspec string in it

        mov     di,ComspOffset
        cmp     byte ptr es:[di+1],':'          ; drive specifier already exist?
        je      notwidenv                       ; yes, must have been inherited that way
	push	ds				;  2 bytes to make room for a drivespec.
	push	es				;  the drivespec is in ax and is copied
	pop	ds				;  on to the front of the string.

        lea     si,[di+MAX_COMSPEC-3]
        lea     di,[di+MAX_COMSPEC-1]
        mov     cx,MAX_COMSPEC - 2
	rep	movsb
	pop	ds
        mov     word ptr es:[di-1],ax

endif	;ROMDOS

notwidenv:
	cld					; add the drivespec to the string
        mov     word ptr AutoBat,ax             ; used to reference autoexec.bat
        mov     word ptr KautoBat,ax            ; used to reference kautoexe.bat 3/3/kk

nocomdrv:
	call	setvect 			; set interrupt vectors 22h, 23h, & 24h

;*********************************
; parsing starts here
;*********************************

	push	cs				; get local segment
	push	cs				; into ds,es
	pop	ds				;
	pop	es				;

assume  ds:ResGroup,es:ResGroup

	mov	si,80h				; get command line
	lodsb					; get length of line
	mov	di,si				; get line position in di
	xor	ah,ah				; ax = length of command line
;
; insure that the command line correctly ends with a cr
;
	add	di,ax				; go to end of command line
        mov     byte ptr [di],0dh               ; insert a carriage return
	xor	cx,cx				; clear cx
        mov     Num_positionals,cx              ; initialize positionals
;
; Scan the command line looking for the parameters
;

Parse_command_line:
        mov     di,offset ResGroup:Parse_Command; Get address of parse_command
        mov     cx,Num_positionals              ; Get number of positionals
	xor	dx,dx				; clear dx
        mov     Old_parse_ptr,si                ; save position before calling parser
	call	init_parse			; call parser
        mov     Num_positionals,cx              ; Save number of positionals
	cmp	ax,END_OF_LINE			; are we at end of line?
        jne     t1
        jmp     ArgsDone                        ; yes - exit
t1:     cmp     ax,RESULT_NO_ERROR              ; did an error occur
	jz	parse_cont			; no - continue

;
; Before issuing error message - make sure switch is not /C
;

parse_line_error:
	push	si				; save line position
	push	ax				; save error number
	cmp	ax,BADSWT_PTR			; Was error invalid switch?
        jnz     parse_line_error_disp           ; No - just issue message
	mov	di,si				; Get terminating pointer in DI
        mov     si,Old_parse_ptr                ; Get starting pointer in SI

init_chk_delim:
	cmp	si,di				; at end of parsed parameter?
        jz      parse_line_error_disp           ; Yes - just display message
	lodsb					;
	cmp	al,Space			; Skip blank spaces
	jz	init_chk_delim			;
	cmp	al,TAB_CHR			; Skip tab characters
	jz	init_chk_delim			;

	cmp	al,Rswitchar			; Switch?
        jnz     parse_line_error_disp           ; No - just issue message
	lodsb					; Get the char after the switch

	ifdef	DBCS
	call	ItestKanj			; Is it DBCS?
        jnz     parse_line_error_disp           ; Yes - can't be /C or /K
	endif

	call	iupconv 			; upper case it
	cmp	al,Scswitch			; it is /C?
        jnz     check_k_too                     ;
	pop	dx				; even up stack
	pop	dx				; even up stack
	jmp	setSSwitch			; Yes - go set COMMAND /C

check_k_too:
        cmp     al,Skswitch                     ; it is /K?
        jnz     parse_line_error_disp           ;
	pop	dx				; even up stack
	pop	dx				; even up stack
        jmp     setKSwitch                      ; Yes - go set COMMAND /K

parse_line_error_disp:
	pop	ax				; restore error number
	pop	si				; restore line position
	mov	dx,ax				; get message number
	call	RPrintParse
	call	CrLf
        jmp     short Parse_command_line        ; continue parsing

parse_cont:
;
; See if a switch was entered
;
; Bugbug: See if Comnd1_Syn can be moved into a reg. before the compare

	cmp	Comnd1_Syn,offset ResGroup:Command_f_syn ; was /F entered?
	jz	SetFSwitch				 ; yes go set fail switch
	cmp	Comnd1_Syn,offset resgroup:Command_p_syn ; was /P entered?
	Jz	SetPSwitch				 ; yes go set up PERMCOM
	cmp	Comnd1_Syn,offset resgroup:Command_d_syn ; was /D entered?
	jz	SetDSwitch				 ; yes go set date switch
	cmp	Comnd1_Syn,offset resgroup:Command_c_syn ; was /C entered?
	jz	SetSSwitch				 ; yes go set up SINGLECOM
        cmp     Comnd1_Syn,offset resgroup:Command_k_syn ; was /K entered?
        jz      SetKSwitch                               ; yes go set up SINGLECOM
	cmp	Comnd1_Syn,offset resgroup:Command_e_syn ; was /E entered?
	jz	SetESwitch				 ; yes go set up environment
	cmp	Comnd1_Syn,offset resgroup:Command_m_syn ; was /MSG entered?
	jz	SetMSwitchjmp				 ; yes go set up message flag
	jmp	ChkOtherArgs				 ; Must be something else

SetMSwitchjmp:					; long jump needed
	jmp	SetMswitch			;

SetFSwitch:
	cmp	fFail,-1			; has fail switch been set?
	jnz	failok				; no - set it
        mov     ax,Moreargs_ptr                 ; set up too many arguments
        jmp     parse_line_error                ; go issue error message

failok:
	mov	fFail,-1			; fail all INT 24s.
        jmp     Parse_command_line              ;

SetPSwitch:
;
; We have a permanent COMMAND switch /P.  Flag this and stash the
; termination address.
;
	cmp	PermCom,0			; has /p switch been set?
	jz	permcomok			; no - set it
        mov     ax,moreargs_ptr                 ; set up too many arguments
        jmp     parse_line_error                ; go issue error message

permcomok:
	inc	PermCom
	mov	word ptr OldTerm,offset DATARES:LodCom_Trap
	mov	word ptr OldTerm+2,ds
;
; make sure that we display the date and time.	if the flag was not
; initialized, set it to indicate yes, do prompt.
;
	cmp	byte ptr PrdAttm,-1
	jnz	Parse_command_line_jmp		; keep parsing
        mov     byte ptr PrdAttm,0              ; if not set explicit, set to prompt

Parse_command_line_jmp: 			;
        jmp     parse_command_line              ; keep parsing

SetDSwitch:
;
; Flag no date/time prompting.
;
	cmp	Dswitch,0			; has /D switch been set?
	jz	setdateok			; no - set it
        mov     ax,Moreargs_ptr                 ; set up too many arguments
        jmp     parse_line_error                ; go issue error message

setdateok:
	inc	Dswitch 			; indicate /D entered
        mov     byte ptr PrdAttm,1              ; user explicitly says no date time
        jmp     Parse_command_line              ; continue parsing

SetKSwitch:
        mov     SemiPermCom,0
        jmp     short SetSorKSwitch

SetSSwitch:
;
; Set up pointer to command line, flag no date/time and turn off singlecom.
;
	mov	Permcom,0			; a singlecom must not be a permcom
SetSorKSwitch:
	mov	SingleCom,si			; point to the rest of the command line
        mov     byte ptr PrdAttm,1              ; no date or time either, explicit
	jmp	ArgsDone
;
; Look for environment-size setting switch
;
; The environment size is represented in decimal bytes and is
; converted into pargraphs (rounded up to the next paragraph).
;

SetESwitch:
	cmp	Eswitch,0			; has fail switch been set?
	jz	eswitchok			; no - set it
        mov     ax,Moreargs_ptr                 ; set up too many arguments
        jmp     Parse_line_error                ; go issue error message

eswitchok:
	inc	Eswitch 			; indicate /E entered
	mov	di,offset ResGroup:Comnd1_Addr	; get number returned
        mov     bx,word ptr [di]                ; into bx

	add	bx, 0fh 			; Round up to next paragraph
	mov	cl,4				; convert to pargraphs
	shr	bx, cl				; by right 4

	mov	EnvSiz,BX			; EnvSiz is in paragraphs
        jmp     Parse_command_line              ; continue parsing command line

SetMSwitch:
        cmp     Ext_msg,SET_EXTENDED_MSG        ; has /MSG switch been set?
	jnz	setMswitchok			; no - set it
        mov     ax,Moreargs_ptr                 ; set up too many arguments
        jmp     Parse_line_error                ; go issue error message
setMswitchok:
        mov     Ext_msg,SET_EXTENDED_MSG        ; set /MSG switch
        jmp     Parse_command_line              ; keep parsing

ArgsDoneJ:
	jmp  ArgsDone

;
; We have a non-switch character here.
;
ChkOtherArgs:
	push	ds				;
	push	si				; save place in command line
	lds	si,Comnd1_Addr			; get address of filespec
	assume	ds:nothing			;

	mov	dx,si				; put in dx also
        mov     ax,(OPEN shl 8) or 2            ; Read and write
	int	21h
	jc	ChkSrchSpec			; Wasn't a file
	mov	bx,ax
	mov	ax,IOCTL shl 8
	int	21h
	test	dl,80h
	jnz	IsaDevice

BadSetCon:					;
	mov	ah,CLOSE			; Close initial handle, wasn't a device
	int	21h
	jmp	short ChkSrchSpec

IsaDevice:
	xor	dh,dh
	or	dl,3				; Make sure has CON attributes
	mov	ax,(IOCTL shl 8) or 1
	int	21h
	jc	BadSetCon			; Can't set attributes - quit
	mov	dx,bx				; Save new handle

	cmp	es:DevFlag,1
	jz	DevErr

	push	cx
	mov	cx,3
	xor	bx,bx

rcclloop:					; Close 0,1 and 2
	mov	ah,CLOSE
	int	21h
	inc	bx
	loop	rcclloop

	mov	bx,dx				; New device handle
	mov	ah,XDUP
	int	21h				; Dup to 0
	mov	ah,XDUP
	int	21h				; Dup to 1
	mov	ah,XDUP
	int	21h				; Dup to 2
	mov	ah,CLOSE
	int	21h				; Close initial handle
	pop	cx
	pop	si				; restore position of command line
	pop	ds				;
;
; Register the fact that we already have redirected the output and cannot do
; it again
;
	inc	es:DevFlag			;
        jmp     Parse_command_line              ; continue parsing

DevErr:
	pop	si
	pop	ds
	mov	dx,1
        call    RPrintParse                     ; "Too many parameters"
        call    CrLf
	jmp	Parse_command_line

ChkSrchSpec:					; Not a device, so must be directory spec

        cmp     es:PathFlag,1                   ; already set COMSPEC?
        jz      DevErr                          ; yes, error

        inc     es:PathFlag                     ; mark that we have a path
;
; We have to override the passed environment. Allocate a buffer for use now.
; This buffer will later be replaced by a proper environment
;
        call    alloc_env                       ; environment buffer
	mov	es,ax
	assume	es:nothing
	push	si				; remember location of file
	xor	cx,cx				; clear cx for counting

countloop:
	lodsb					; get a character
	inc	cx				; increment counter
        cmp     al,END_OF_LINE_OUT              ; are we at end of line?
	jnz	countloop			; no - keep counting

	mov	al,Space
	dec	si				; move back one
        mov     byte ptr [si],al                ; put a space at end of line
;
; We now know how long the new pathspec for command.com is.  Time to
; figure out how long the current COMSPEC setting is, and then to move
; all the environment data up, throwing that COMSPEC setting away, and
; preparing to append the new COMSPEC.  ComspOffset (the offset of
; where the filespec exists in the environment) is updated as well.
;
        push    cx                              ;
        mov     cx,ENVBIG                       ;
        mov     di,ComspOffset                  ; get location of COMSPEC
        mov     al,0                            ;
        repne   scasb                           ; find the end of COMSPEC
        mov     si,di                           ;
comp_endenv:                                    ;
        scasb                                   ; end of env?
        je      got_endenv                      ; yes
        repne   scasb                           ;
        jmp     comp_endenv                     ;
got_endenv:                                     ;
        mov     cx,di                           ;
        sub     cx,si                           ;
        mov     di,ComspOffset                  ;
        sub     di,ComspStrLen                  ;
        push    ds                              ;
        push    es                              ;
        pop     ds                              ;
        rep     movsb                           ;
        dec     di                              ; copy in new COMSPEC=
        push    cs                              ;
        pop     ds                              ;
        assume  ds:ResGroup                     ;
        mov     si,offset RESGROUP:ComspString  ;
        mov     cx,ComspStrLen                  ;
        rep     movsb                           ;
        mov     ComspOffset,di                  ;
        pop     ds                              ;
        assume  ds:nothing                      ;
        pop     cx                              ;

        pop     si                              ; get new comspec location back

ComtrLoop:
	lodsb
	dec	cx
	cmp	al,Space
	jz	SetComsr
	stosb

	ifdef	DBCS
	xor	ah,ah
	endif

	jcxz	setcomsr

	ifdef	DBCS

	push	ds				; Make sure we have
	push	cs				;  local DS for
	pop	ds				;  ItestKanj
	call	ItestKanj
	pop	ds				; restore parser ds
	jz	ComtrLoop
	dec	cx
	movsb
	inc	ah
	jcxz	setcomsr

	endif

	jmp	short comtrloop

setcomsr:
	push	cx

	push	cs				; Get local segment
	pop	ds				;
	assume	ds:ResGroup			;

	push	ds
	mov	si,offset ResGroup:ComSpect
	mov	cx,14

	mov	al,es:[di-1]

	ifdef	DBCS
	or	ah,ah
	jnz	iNotRoot			; Last char was KANJI second byte, might be '\'
	endif

	cmp	al,RDirChar
	jnz	iNotRoot
	inc	si				; Don't make a double /
	dec	cx

iNotRoot:
	rep	movsb

        mov     dx,ComspOffset                  ; Now lets make sure its good!
	push	es
	pop	ds

	mov	ax,OPEN shl 8
	int	21h				; Open COMMAND.COM
	pop	ds
	jc	SetComsrBad			; No COMMAND.COM here
	mov	bx,ax				; Handle
	mov	ah,CLOSE
	int	21h				; Close COMMAND.COM

SetComsrRet:
	pop	cx
	pop	si
	pop	ds				;
	assume	ds:ResGroup			;

	push	cs				; Make sure local ES is
	pop	es				;  restored
	jmp	Parse_command_line			; continue parsing command line

SetComsrBad:
	mov	dx,offset ResGroup:BadComlkMsg	; dx = ptr to msg

;	Note:  we're about to make a near call to TriageError, which
;	lives in a different segment and group.  Some linkers will
;	generate a warning like "Possible fix-up overflow".  We're
;	ok, though, because we all fit in 64 KB and, at init time,
;	we're still all together.

	call	triageError
	cmp	ax, 65
	jnz	doprt
	mov	dx,offset ResGroup:BadComaccMsg	; dx = ptr to msg
DoPrt:
	call	RPrint
	mov	si,offset ResGroup:ComSpect
        mov     di,ComspOffset
	mov	cx,14
	rep	movsb				; get my default back

	jmp	short SetComsrRet

;*********************************
; Parsing Ends Here
;*********************************

ArgsDone:

	mov	es,EnvirSeg			; get environment back
	assume	es:nothing			;

	cmp	PermCom,0
	jz	ComReturns

	push	es				; Save environment pointer
	mov	ah,SET_CURRENT_PDB
	mov	bx,ds
	mov	es,bx
	int	21h				; current process is me
	mov	di,PDB_EXIT			; Diddle the addresses in my header
	mov	ax,offset DATARES:LodCom_Trap
	stosw
	mov	ax,ds
	stosw
	mov	ax,offset DATARES:Ctrlc_Trap
	stosw
	mov	ax,ds
	stosw
	mov	ax,offset DATARES:CritErr_Trap
	stosw
	mov	ax,ds
	stosw
	mov	word ptr ds:PDB_Parent_Pid,ds 	; Parent is me forever

	mov	dx,offset DATARES:Int2e_Trap
	mov	ax,(SET_INTERRUPT_VECTOR shl 8) or 02eh
	int	21h				;set magic interrupt
	pop	es				;Remember environment

ComReturns:
	mov	ax,word ptr ds:PDB_Parent_Pid
	mov	Parent,ax			; Save parent
	mov	word ptr ds:PDB_Parent_Pid,ds 	; Parent is me
	mov	ax,word ptr ds:PDB_Jfn_Table
	mov	Io_save,ax			; Get the default stdin and out
	mov	word ptr Com_ptr+2,ds		 	; set all these to resident
	mov	word ptr Com_fcb1+2,ds
	mov	word ptr Com_fcb2+2,ds
	mov	di,offset ResGroup:ComSpec

        mov     si,ComspOffset
        cmp     AllocedEnv,0

	mov	ax,ds				; Xchg es,ds
	push	es
	pop	ds
	mov	es,ax

        jne     CopyComsp                       ; All set up for copy

	push	cs
	pop	ds

	mov	si,offset ResGroup:ComspString
	push	es
	push	di
	call	IfindE
	mov	si,di
	push	es
	pop	ds
	pop	di
	pop	es
	jnc	CopyComsp

ComSpecNofnd:
        mov     si,offset ResGroup:ComspString
        add     si,ComspStrLen
	push	cs
	pop	ds

	assume	es:ResGroup

CopyComsp:
	mov	es:PutBackComSpec.SubstPtr,di	; Save ptr to beginning of comspec path
	cmp	byte ptr [si+1],':'             ; Is there a drive specifier in comspec
	jnz	CopyComspLoop			; If not, do not skip over first 2 bytes
	add	es:PutBackComSpec.SubstPtr,2

CopyComspLoop:
	lodsb
	stosb
	or	al,al
	jnz	CopyComspLoop
	mov	es:Comspec_end,di			; Save ptr to end of comspec path
	dec	es:Comspec_end
	mov	ah,es:comdrv
	add	ah,'A'-1
	mov	es:PutBackDrv,ah			; save drive letter


	call	setup_for_messages			; set up parse and extended error messages
;
; The routine below sets up the exact resident size of COMMAND. If this is not
; the first COMMAND, then the resident code is not duplicated and the resident
; size is just the data. If we are the first COMMAND, it checks if we are to
; be loaded into HIMEM. If not, then the resident size includes the code and
; the data otherwise it is just the data.
; 
	call	Setup_res_end			;put resident size in ResSize

	push	cs
	pop	ds
	assume	ds:RESGROUP


Public EnvMaximum
EnvMaximum:

;
; Compute checksum right now before we can get corrupted and save it
;

	mov	si,offset RESGROUP:TranStart
	add	si,100h
	mov	cx,offset TRANGROUP:TranDataEnd - 100H

	cld
	shr	cx,1
	xor	dx,dx
Ichksum:
	lodsw
	add	dx,ax
	adc	dx,0
	loop	Ichksum

        mov     Sum,dx                          ; store checksum

        cmp     byte ptr PrdAttm,0              ;
        jnz     NoBatchSeg                      ; don't do autoexec or date time
;
; Allocate batch segment for d:/autoexec.bat + no arguments
;
	mov	bx,((SIZE BatchSegment) + 15 + 1 + 0fh)/16 ;eg
        mov     ah,ALLOC                        ;
        int     21h                             ;
        jc      NoBatchSeg                      ; didn't allocate - pretend no batch
        mov     Batch,ax                        ; save batch segment

nobatchseg:
        mov     bx,EnvirSeg                     ; get old environment segment
        mov     OldEnv,bx                       ; save it

        mov     UsedEnv,0                       ; initialize env size counter
	mov	ds,bx
	assume	ds:nothing

	xor	si,si
	mov	di,si
;
; This is the maximum allowed size for the environment
;
	mov	bx,4096 - 1 			; max. allowed env. size
	mov	EnvMax,bx
				 
	shl	bx,1
	shl	bx,1
	shl	bx,1
	shl	bx,1
	mov	EnvMax, bx			; convert envmax to bytes
	dec	bx				; dec by one to leave room for double 0
	xor	dx,dx				; use dx to indicate that there was
						; no environment size error.
public NxtStr
NxtStr:
	call	GetStrLen			; get the size of the current env string

;Bugbug: Can use ss here to address UsedEnv

        push    ds                              ; get addressability to environment
        push    cs                              ;                       counter
        pop     ds                              ;
        assume  ds:ResGroup
        add     UsedEnv,cx                      ; add the string length to env size
        pop     ds                              ;
	assume	ds:nothing
	cmp	cx,1				; end of environment was encountered.
	jz	EnvExit
	sub	bx,cx
	jae	OkCpyStr			; can't fit in all of enviroment.
	inc	dx				; out of env space msg must be displayed
	jmp	short EnvExit
OkCpyStr:
	jmp	NxtStr
EnvExit:

	push	cs
	pop	ds
	assume	ds:ResGroup
	or	dx,dx				; dx will be non-zero if error
	jz	EnvNoErr
	mov	dx,offset ResGroup:OutEnvMsg	; dx = ptr to msg
	call 	RPrint

EnvNoErr:
	mov	ax,EnvSiz			;env size previously set
	mov	cl,4
	shl	ax,cl				;get size in bytes
	cmp	ax,UsedEnv			;is it a new env?
	ja	st_envsize			;yes, store the size

	mov	ax,UsedEnv
	add	ax,15				;round up
st_envsize:	
	shr	ax,cl
	mov	EnvSiz,ax			;store env size needed(paras)

if MSVER
	cmp	SingleCom,0
	jnz	nophead 			; don't print header if singlecom
	mov	dx,offset ResGroup:CopyrightMsg	; dx = ptr to msg
	call	RPrint
nophead:
endif

	cmp	Batch,0			;eg did we set up a batch segment?
	jnz	DoDate				;eg yes - go initialize it
	jmp	NoDttm				; don't do autoexec or date time
;
; allocate batch segment for d:/autoexec.bat + no arguments
;
DoDate:
	mov	ax,Batch			;eg get batch segment
	mov	EchoFlag,3			; set batch echo
	mov	Nest,1				; g set nest flag to 1 batch
	mov	es,ax
;
; initialize the segment
;
	xor	di,di
	mov	al,BATCHTYPE
	stosb
	mov	al,1				; g initialize echo for batch exit
	stosb					; g
;
; Hosebag! This guy does not use the struct fields to init the BatchSegment
;
	xor	ax,ax				; initialize to zero
	stosb					; clear out BatchEOF

	stosw					; g batch segment of last job - batlast
	stosw					; g segment for for
	stosb					; g for flag
	stosw					; position in file - batseek
	stosw
;
; clean out the parameters
;
	mov	ax,-1				; initialize to no parameters
	mov	cx,10
	rep	stosw
;
; decide whether we should grab the default drive
;
	cmp	byte ptr AutoBat,0
	jnz	NoAutSet
	mov	ah,GET_DEFAULT_DRIVE
	int	21h
	add	al,Ucasea

	mov	AutoBat,al
	mov	KautoBat,al			; 3/3/kk

NoAutSet:
;
; copy in the batch file name (including nul)
;
	mov	si,offset ResGroup:AutoBat
	mov	cx,8
	rep	movsw
	movsb					; move in carriage return to terminate string

ifdef ROMDOS

; Check to see if the Boot Options indicate that startup processing
; should be omitted.  If so, skip the open, and pretend that there is
; no autoexec.bat.

	push	es			; save current ES
	mov	ax,BDATA
	mov	es,ax
	assume	es:BDATA
	mov	ax,es:BootFlags		; get boot options from BIOS
	pop	es			; restore previous ES
	assume	es:RESGROUP
	test	al,BF_NoConfig		; flag set to supress processing?
	jnz	NOABAT			; if so, skip autoexec processing

endif ; ROMDOS

	mov	dx,offset ResGroup:AutoBat
	mov	ax,OPEN shl 8
	int	21h				; see if autoexec.bat exists
	jc	NoAbat
	mov	bx,ax
	mov	ah,CLOSE
	int	21h
	jmp	Drv0				; go process autoexec

noabat:
	push	ax
	call	Setup_Seg
	mov	word ptr Triage_Add+2,ax
	pop	ax
	call	Triage_Add
	cmp	ax, 65
	jz	AccDenErr			; was network access denied


; If AUTOEXEC.BAT is not found, then check for KAUTOEXE.BAT.  Changed
; by Ellen to check only when in Korea.  The country information
; returned will overlay the old parse data area, but we don't care
; since we won't need the parse information or country information.
; We only care about the country code returned in BX.

	mov	dx,offset ResGroup:Internat_Info ; set up internat vars
	mov	ax,INTERNATIONAL shl 8		; get country dependent info
	int	21h				;
	jc	NoKabat 			; error - don't bother with it
	cmp	bx,KOREA_COUNTRY_CODE			; are we speaking korean?
	jnz	OpenErr 			; no, don't check for kautoexe

	mov	di,BatFile			; 3/3/kk
	mov	si,offset ResGroup:KautoBat	; another trial to do	3/3/kk
	mov	cx,8				; auto execution for the 3/3/kk
	rep	movsw				; non-english country	3/3/kk
	movsb					; move in carraige return to terminate string
	mov	dx,offset ResGroup:KautoBat	; 3/3/kk
	mov	ax,OPEN shl 8			; 3/3/kk
	int	21h				; see if kautoexe.bat exists    3/3/kk
	jc	NoKabat 			; 3/3/kk
	mov	bx,ax				; 3/3/kk
	mov	ah,CLOSE			; 3/3/kk
	int	21h				; 3/3/kk
	jmp	short Drv0			; 3/3/kk

NoKabat:						; 3/3/kk
	call	Triage_Add			; get extended error
	cmp	ax, 65				; network access denied?
	jnz	OpenErr 			; no - go deallocate batch

AccDenErr:					; yes - put out message
	mov	dx,offset ResGroup:AccDen		; dx = ptr to msg
	call	RPrint

OpenErr:
	mov	es,Batch			; not found--turn off batch job
	mov	ah,DEALLOC
	int	21h
	mov	Batch,0			; after dealloc in case of ^c
	mov	EchoFlag,1
	mov	Nest,0				;g indicate no batch in progress

DoDttm:
	mov	ax,offset TranGroup:Datinit
	mov	word ptr InitAdd,ax
;;;M004	mov	ax,TrnSeg	
;
; M004; We cant use TrnSeg now because it is not initialized. We now that
; M004; the transient starts on a para boundary at the label TranStart.
; M004; We use TranStart to get the start of the transient segment.
;
	mov	ax,offset RESGROUP:TranStart	; M004
	mov	cl,4				; M004
	shr	ax,cl				; get relative seg ; M004
	mov	cx,cs
	add	ax,cx				; ax = transient seg ; M004

	mov	word ptr InitAdd+2,ax
	call	dword ptr InitAdd

NoDttm:

Copyright:
	public	Copyright
;	Bugbug:	remove Copyright label.

if IBMVER
	cmp	SingleCom,0
	jnz	Drv0				; don't print header if singlecom
	mov	dx,offset ResGroup:CopyrightMsg	; dx = ptr to msg
	call	RPrint
endif

Drv0:						; Reset APPEND state
	push	ds				; save data segment
	push	cs				; Get local segment into DS
	pop	ds				;
	mov	ax,APPENDSETSTATE			; Set the state of Append
	mov	bx,Append_State 			;  back to the original state
	int	2fh				;
	pop	ds				; get data segment back
;
;Check FirstCom set previously to see if this is the first instance of
;command.com. If not, we do not move command.com. Instead, we copy over the
;jump table from the previous stub to the current stub.
;
	cmp	FirstCom,1			;first command.com?
	jz	move_code			;yes, move it

	push	es
	push	ds

	push	ds
	pop	es
	mov	di,offset DATARES:Int2f_Entry

	mov	ds,word ptr es:ResJmpTable+2	;get segment address
	mov	si,word ptr es:ResJmpTable		;get offset address

	mov 	cx,NUM_RELOC_ENTRIES 			;number of dword ptrs
	shl	cx,1
	shl	cx,1				;size of table in bytes

	cld
	rep	movsb				;copy the jump table
;
;Check if the resident code is in HMA. We assume that it is in HMA if its 
;code segment > 0f000h. If in HMA, we set the ComInHMA flag
;
	cmp	es:[di-2],0f000h			;is resident code in HMA?
	jb	res_low			;no, dont set flag

	mov	es:ComInHMA,1			;indicate code in HMA

res_low:
	pop	ds
	pop	es
	jmp	short finish_init
;
;Now, we can move the resident code to its final location, either to HIMEM
;or to overlay the messages in the data segment if the user has not used the
;/msg switch.
;
move_code:
	call	Move_res_code			;move the code

finish_init:
	jmp	RESGROUP:EndInit 			;g finish initializing

;
;	Get length of string pointed to by DS:SI.  Length includes NULL.
;	Length is returned in CX
;
GetStrLen:
	xor	cx,cx
NxtChar:
	lodsb
	inc	cx
	or	al,al
	jnz	NxtChar
	ret
;
; If the transient has been loaded in TranSeg, then we need to use that
; segment for calls to routines in the transient area. Otherwise, the current
; code segment is used
; Segment returned in AX.
;
Setup_Seg:
	mov	ax,TrnSeg
	cmp	TrnMvFlg, 1			; Has transient portion been moved
	jz	setup_end
	push	bx
	mov	bx,cs
	mov	ax,offset ResGroup:TranStart
	shr	ax,1
	shr	ax,1
	shr	ax,1
	shr	ax,1
	add	ax,bx
	pop	bx
setup_end:
	ret


;***	RPrintParse - display parse error message
;
;	ENTRY	DX = parse error #
;
;	EXIT	nothing
;
;	USED	flags
;
;	EFFECTS
;	  Message is displayed on stdout.

RPrintParse	proc

	assume	ds:ResGroup,ss:ResGroup

	push	dx				; preserve DX
	xchg	bx,dx				; bx = parse error #
						; dx = saved BX
	dec	bx				; bx = parse error index, from 0
	shl	bx,1				; bx = offset in word table
	mov	bx,ParsMsgPtrs[bx]			; bx = ptr to error msg
	xchg	bx,dx				; dx = ptr to error msg
						; bx = restored
	call	RPrint				; print the message
	pop	dx				; restore DX
	ret

RPrintParse	endp



IfindE:
	call	ifind				; find the name
	jc	ifind2				; carry means not found
	jmp	short Iscasb1 			; scan for = sign
;
; on return of find1, es:di points to beginning of name
;
ifind:
	cld
	call	Icount0 			; cx = length of name
	mov	es,EnvirSeg
	xor	di,di

ifind1:
	push	cx
	push	si
	push	di

Ifind11:
	lodsb

	ifdef	DBCS

	call	ItestKanj
	jz	NotKanj4
	dec	si
	lodsw
	inc	di
	inc	di
	cmp	ax,es:[di-2]
	jnz	Ifind12
	dec	cx
	loop	Ifind11
	jmp	short Ifind12
NotKanj4:

	endif

	call	IupConv
	inc	di
	cmp	al,es:[di-1]
	jnz	Ifind12
	loop	Ifind11

Ifind12:
	pop	di
	pop	si
	pop	cx
	jz	Ifind2
	push	cx
	call	Iscasb2 			; scan for a nul
	pop	cx
	cmp	byte ptr es:[di],0
	jnz	Ifind1
	stc					; indicate not found

Ifind2:
	ret

Icount0:
	push	ds
	pop	es
	mov	di,si

	push	di				; count number of chars until "="
	call	Iscasb1
	jmp	short Icountx
	push	di				; count number of chars until nul
	call	Iscasb2

Icountx:
	pop	cx
	sub	di,cx
	xchg	di,cx
	ret

Iscasb1:
	mov	al,Equalsign			; scan for an =
	jmp	short Iscasbx

Iscasb2:
	xor	al,al				; scan for a nul

Iscasbx:
	mov	cx,100h
	repnz	scasb
	ret


; ****************************************************************
; *
; * ROUTINE:	 IUPCONV    (ADDED BY EMG 4.00)
; *
; * FUNCTION:	 This routine returns the upper case equivalent of
; *		 the character in AL from the file upper case table
; *		 in DOS if character if above  ascii 128, else
; *		 subtracts 20H if between "a" and "z".
; *
; * INPUT:	 DS	      set to resident
; *		 AL	      char to be upper cased
; *		 FUCASE_ADDR  set to the file upper case table
; *
; * OUTPUT:	 AL	      upper cased character
; *
; ****************************************************************


IupConv proc	near				
	assume	ds:ResGroup			;

	cmp	al,80h				; see if char is > ascii 128
	jb	other_fucase			; no - upper case math
	sub	al,80h				; only upper 128 chars in table
	push	ds				;
	push	bx				;
	lds	bx,dword ptr fucase_addr+1		; get table address
	add	bx,2				; skip over first word
	xlat	ds:byte ptr [bx]			; convert to upper case
	pop	bx				;
	pop	ds				;
	jmp	short iupconv_end			; we finished - exit

other_fucase:					;
	cmp	al,Lcasea			; if between "a" and "z",
	jb	iupconv_end			;     subtract 20h to get
	cmp	al,Lcasez			; upper case equivalent.
	ja	iupconv_end			;
	sub	al,20h				; Change lower-case to upper

iupconv_end:					;
	ret

IupConv endp					;


init_contc_specialcase:
						; This routine is called if control-C
	add	sp,6				;  is type during the date/time prompt
	push	si				;  at initialization time.  The desired
	mov	si,dx				;  response is to make it look like the
	mov	word ptr [si+1],0d00h			;  user typed <CR> by "popping" the
	pop	si				;  INT 21h stuff off the stack, putting
	iret					;  a <CR> in the user's buffer, and
						;  returning directly to the user.
						; In this case the user is TCODE.


; ****************************************************************
; *
; * ROUTINE:	 Setup_for_messages
; *
; * FUNCTION:	 Sets up system for PARSE and EXTENDED ERROR
; *		 messages as follows:
; *
; *		 IF /P and /MSG are entered
; *		    keep PARSE and EXTENDED ERRORS in memory
; *		 ELSE IF /P is entered
; *		    use PARSE and EXTENDED ERRORS on disk
; *		    remove PARSE ERRORS from memory
; *		 ELSE
; *		    remove PARSE ERRORS from memory
; *		 ENDIF
; *
; * INPUT:	 PERMCOM	Set up with user input
; *		 EXT_MSG	Set up with user input
; *		 System set up to retain PARSE ERRORS
; *
; * OUTPUT:	 registers unchanged
; *
; ****************************************************************


setup_for_messages	proc	near		

	push	bx
	push	ds				; save data segment
	push	es				; save environment segment
	push	ax				;
	push	dx				;
	push	di				;
	mov	ax,cs				; get local segment to ES and DS
	mov	ds,ax				;
	mov	es,ax				;

	cmp	PermCom,0			; was permcom set?
	jz	no_permcom			; No - don't worry about messages

;*	We're permanent.  Install our message services int 2f handler.

	push	es
	mov	ax,(GET_INTERRUPT_VECTOR shl 8) or 2Fh
	int	21h
	mov	word ptr Int2fHandler,bx
	mov	word ptr Int2fHandler+2,es
	pop	es

;	DS = RESGROUP seg addr

;
; M005; We will not hook int 2fh on any command.com other than the first.
; M005; Carousel loads as a permanent command.com and when we exit Carousel,
; M005; it just wipes our arena out. So, int 2fh is still hooked and the
; M005; first int 2fh call after exit from Carousel (from the DOS terminate
; M005; call) goes off into space.
;
	cmp	FirstCom,0			; M005
	je	no_msg_hook			; M005
;
; M005; !!!SLIMIEST CAROUSEL HACK OFF ALL!!!
; M005; Carousel plays around with the interrupt vector tables. He saves it
; M005; before loading a new command.com. Then, it takes hold of the current
; M005; command.com's PSP and then looks at all interrupt vectors whose
; M005; segment matches the command.com PSP and then updates these segments
; M005; to the new command.com's PSP in his saved vector table. Whenever we
; M005; we pop into his menu, he puts this saved table into the vector table.
; M005; If we now quit, Carousel just wipes out command.com's arena and then
; M005; issues a terminate. Unfortunately, the int 2fh vector is pointing at
; M005; the command.com that was wiped out and so the next int 2fh call will
; M005; bomb. To prevent Carousel from doing this clever(1**$$#) patching, we
; M005; renormalize our int 2fh pointer so that its cs is not the same as the
; M005; command.com PSP. Now, he does no such patching and our int 2fh vector
; M005; remains nice and happy. The renormalized pointer points at a far 
; M005; jump to the actual int 2fh entry point.
;
	push	ds				; M005
	mov	dx,offset DATARES:Carousel_i2f_Hook ; M005
	sub	dx,10h				; renormalize offset; M005
	mov	ax,ds				; M005
	inc	ax				; Relocated cs ; M005
	mov	ds,ax				; M005
	mov	ax,(SET_INTERRUPT_VECTOR shl 8) or 2Fh
	int	21h
	pop	ds				; M005
	mov	word ptr Carousel_i2f_Hook+3,ds	; M005
						; patch in the cs for jump
no_msg_hook:					; M005

	cmp	Ext_Msg,SET_EXTENDED_MSG
	jne	short permcom_end			; no /msg - exit

permcom_slash_msg:				; Keep messages in memory
	mov	di,offset ResGroup:ExtMsgEnd 	; get address of resident end
	mov	ResMsgEnd,di			; save it
	jmp	short permcom_end			; exit

no_permcom:					
	cmp	Ext_msg,SET_EXTENDED_MSG		; was /msg specified?
	jnz	permcom_end			; no - no error
	mov	dx,LessArgs_Ptr 			; get message number for "Required parameter missing"
	call	RPrintParse

permcom_end:
	pop	di				;
	pop	dx				;
	pop	ax				;
	pop	es				; get environment back
	pop	ds				;
	pop	bx

	ret					;

setup_for_messages	endp




;***	CheckHelp - print help text and exit if /? is on command line
;
;	ENTRY	command-line tail at 81h
;
;	EXIT	return if /? not found
;		terminate if /? found
;
;	USED	AX,BX,CX,DX,SI,DI
;
;	EFFECTS	Help text displayed if /? found on command line

CheckHelp	proc

	assume	cs:RESGROUP,ds:RESGROUP,es:RESGROUP,ss:RESGROUP

	mov	si,81h			; DS:SI = ptr to command-line tail
	mov	di,offset RESGROUP:Parse_Command
					; ES:DI = ptr to primary parse block
	xor	cx,cx			; CX = # positional param's found
	xor	dx,dx			; DX will be ptr to result buffer

chParse:
	call	Init_Parse		; call system parser

	cmp	ax,END_OF_LINE
	je	chRet			; end of command line, no /? found
	cmp	ax,RESULT_NO_ERROR
	je	chWhich			; valid syntax element found
	jmp	chParse			; go parse more

chWhich:
	cmp	Comnd1_Syn,offset RESGROUP:Command_?_Syn
	je	chHelp			; /? found - display help & exit
	cmp	Comnd1_Syn,offset RESGROUP:Command_C_Syn
	je	chRet			; /c found - ignore rest of line
        cmp     Comnd1_Syn,offset RESGROUP:Command_K_Syn
        je      chRet                   ; /k found - ignore rest of line
	jmp	chParse			; anything else - ignore, keep looking

chHelp:
	mov	si,offset RESGROUP:HelpMsgs	; SI = ptr to msg ptr list
chHelpNext:
	lodsw					; AX = ptr to msg
	or	ax,ax
	jz	chHelpDone			; end of list - all done
	mov	dx,ax				; DX = ptr to msg
	call	RPrint				; display msg
	jmp	chHelpNext			; go do next msg
chHelpDone:
	int	20h				; terminate program

chRet:	ret

CheckHelp	endp




;***** Setup_res_end -- This routine determines the resident size of COMMAND.
; It determines based on 2 factors:
;	1. Is this is the first COMMAND?
;	2. Is COMMAND to be loaded into HIMEM?
;   The strategy works as follows:
;
;	if ( First COMMAND)
;	then if (COMMAND in HIMEM)
;		ResSize = resident_data;
;	     else
;		ResSize = resident_data + resident_code;
;	else
;	   ResSize = resident_data;
;
; Int 2fh calls have been added to determine whether or not we are the first
;COMMAND and whether DOS is in HIMEM.
;
;	ENTRY: ResMsgEnd = resident size of data in paras
;
;	EXIT:  ResSize = resident size in low memory
;
;	REGISTERS AFFECTED: ax,cx,dx
;

GET_HMA_ADDR		equ	4a02h

Setup_res_end	proc	near
	
	push	ds
	mov	ax,cs
	mov	ds,ax				;ds = RESGROUP
	assume	ds:RESGROUP

	mov	cx,ResMsgEnd			;set resident size = data

ifndef	ROMDOS

;M042 -- Begin changes
;If messages are to be kept behind, we need to round up the messages to
;the next para boundary. This is because we have a dummy segment between the
;data and the resident code segment so that the code segment starts on a
;para boundary
;
	cmp	cx,offset RESGROUP: ExtMsgEnd	;messages to be resident?
	jne	calc_res			;no, continue
	add	cx,15				;round up
	and	cx,0fff0h
calc_res:
;
;M042 -- End changes
;
	xor	ax,ax
       	cmp	FirstCom,1			;is it first command.com?
	jnz	not_first			;no, do not keep code
;
;We issue a version check call with al=01 to detect if DOS is in HMA. If so,
;bit 4 of dh is set
;
	push	bx
	push	cx
	mov	ax,(SET_CTRL_C_TRAPPING shl 8) or 06h ;is DOS in HIMEM? ;M013
	int	21h
	pop	cx
;bugbug: remove version check after testing
	cmp	bl,5				;bl has true version ; M013
	jb	oldver

	xor	ax,ax
	and	dh,10h				;is DOS in HMA ; M013
	pop	bx
	jnz	not_first			;DOS in HIMEM, code not
						;	resident

	mov	ax,offset CODERES: EndCode		;size of code in bytes
not_first:
;
;Note that ax = 0(side effect of int 2fh), if the code is not to be retained
;
	add	cx,ax

endif	;not ROMDOS

	add	cx,15				;round up to next para
	shr	cx,1
	shr	cx,1
	shr	cx,1
	shr	cx,1				;ax = para size of res code
	mov	ResSize,cx			;store resident size

	pop	ds
	assume	ds:nothing
	ret

ifndef	ROMDOS

;bugbug: remove this code (for version independent COMMAND) after testing
oldver:
	pop	bx
	mov	ax,offset CODERES: EndCode		;size of code in bytes
       	jmp	short not_first

endif	;not ROMDOS

setup_res_end	endp


ifndef	ROMDOS

;*** Move_res_code -- This routine moves the resident code to its final 
; location. We check if DOS is in HIMEM. If so, we try to load ourselves
; in HIMEM. If we fail, then we remain low and update ResSize to reflect
; the correct resident size. When remaining low, we have to check if we 
; need to overlay the messages part of the data segment which is determined
; by the /msg switch.
;
;	ENTRY: ResMsgEnd = end of resident data
;
;	EXIT:  The resident code is either up high or in its final location
;		down low.
;
;	REGISTERS AFFECTED: ax,bx,cx,dx,si,di
;
	
Move_res_code	proc	near

	push	ds
	push	es

	mov	ax,cs
	mov	ds,ax
	assume	ds:RESGROUP

	mov	ax,(SET_CTRL_C_TRAPPING shl 8) or 06h ; M013
	int	21h				;DOS in HIMEM?

	and	dh,10h				; M013
	jnz	move_high			;yes, move code high

;
;Check if messages have been discarded or not
;
load_low:
	push	ds
	pop	es				;es = RESGROUP
	mov	di,ResMsgEnd			;end offset in DATARES
	mov	bx,offset RESGROUP: ExtMsgEnd	;end offset of messages

	cmp	di,bx				;are messages to be kept?
	jz	no_move			;yes, dont move code

	jmp	short setup_move			;es:di points at dest.

move_high:

;
;We have to call DOS to get the load address in HIMEM for COMMAND
;We pass in bx the number of bytes we need
;
	mov	bx,offset CODERES: EndCode

;M030;
; Set di=0ffffh so that we load low in case no one answers this int 2fh
;
	mov	di,0ffffh			;DT - in case no-one handles
						;this ; M030
	mov	ax,GET_HMA_ADDR
	int	2fh

;
;If the offset = 0xffff, then no HMA available
;
	cmp	di,0ffffh			;HMA available?
	mov	ComInHMA,1			;assume command.com in HMA
	jnz	setup_move			;no error, es:di = memory

	mov	ComInHMA,0			;could not load in HMA
;
;Zero means that we do not have enough HIMEM. Remain low and update
;ResSize to reflect this
;
	mov	cx,ResMsgEnd			;size of data in bytes
	mov	ax,offset CODERES: EndCode		;size of code in bytes

	add	cx,ax
	add	cx,15				;round up to next para
	shr	cx,1
	shr	cx,1
	shr	cx,1
	shr	cx,1				;ax = para size of res code
	mov	ResSize,cx			;store resident size
	jmp	short load_low			;let code remain low

no_move:
	mov	cl,4
	add	di,0fh
	and	di,0fff0h			;round it to a para offset
	jmp	short patch_up

setup_move:
	mov	si,offset RESGROUP: StartCode
	mov	cx,offset CODERES: EndCode		;cx = bytes to move

	cld
	push	di				;need di for patching offset
	rep	movsb
	pop	di

patch_up:
	call	patch_stub
	pop	es
	pop	ds
	assume	ds:nothing
	ret

Move_res_code	endp

else	;ROMDOS

;***	Move_res_code - ROMDOS version - locate ROM resident

Move_res_code	proc

	push	es

	invoke	FindROMRes		; ES:DI = ptr to ROM resident code
	call	patch_stub

	pop	es
	ret

Move_res_code	endp

	assume	ds:NOTHING		; to match ending assume above

endif	;ROMDOS


;*** Alloc_env -- This routine allocates the temporary environment for the
; Init code to initialize the COMSPEC. This is not a complete environment. 
; Later on, at EndInit time, a proper sized environment is allocated and
; the contents of this temporary environment are copied to it. This routine
; will not be called in case a valid environment is passed to command.com
;
;       ENTRY:  FirstCom and initial EnvirSeg set
;
;       EXIT:   ax = EnvirSeg = segment of newly allocated environment segment
;
;       REGISTERS AFFECTED: ax,bx,cx,dx
;

Alloc_env	proc	near
        assume  ds:nothing
	
        push    ds
	push	es
	push	si
	push	di

        push    ss
        pop     ds
        assume  ds:RESGROUP

        mov     ax,EnvirSeg

        cmp     AllocedEnv,0
        je      alloc_cont
        jmp     alloc_done

alloc_cont:
        sub     di,di                           ; default start
        mov     bx,SIZE Environment             ; default size needed

        cmp     FirstCom,0                      ; first COMMAND.COM?
        je      alloc_seg                       ; no
;
;   Check EnvirSeg;  if non-zero, then scan it for PATH and COMSPEC;
;   Record their respective locations and do not add the default vars.
;
        or      ax,ax
        jz      alloc_new                       ; no previous environment

        mov     es,ax
        assume  es:nothing

find_path:
        mov     al,0
        sub     di,di
comp_path:
        scasb                                   ; end of env?
        je      find_prompt                     ; yes
        dec     di
        mov     cx,PathStrLen
        mov     si,offset RESGROUP:PathString
        repe    cmpsb
        je      got_path
        mov     cx,256
        repne   scasb                           ; find next NULL
        jmp     comp_path

got_path:
        mov     PathString,0                    ; don't add it

find_prompt:
        sub     di,di
comp_prompt:
        scasb                                   ; end of env?
        je      find_comspec                    ; yes
        dec     di
        mov     cx,PrmptStrLen2
        mov     si,offset RESGROUP:PrmptString
        repe    cmpsb
        je      got_prompt
        mov     cx,256
        repne   scasb                           ; find next NULL
        jmp     comp_prompt

got_prompt:
        mov     PrmptString,0                   ; don't add it

find_comspec:
        sub     di,di
comp_comspec:
        scasb                                   ; end of env?
        je      got_envend                      ; yes
        dec     di
        mov     cx,ComspStrLen
        mov     si,offset RESGROUP:ComspString
        repe    cmpsb
        je      got_comspec
        mov     cx,256
        repne   scasb                           ; find next NULL
        jmp     comp_comspec

got_comspec:
        mov     ComspOffset,di

find_envend:
        sub     di,di
        mov     cx,ENVBIG                       ; max env size
comp_envend:
        dec     cx                              ;
        scasb                                   ; end of env?
        je      got_envend                      ; yes
        repne   scasb
        jmp     comp_envend

got_envend:
        dec     di
        lea     bx,[di+SIZE Environment]        ; add room for the basics
;
;   We want to fall through to alloc_new and set up default
;   path and prompt ONLY IF this is the first process;  in all other
;   cases, we assume it is a bad idea to try editing the user's environment
;
        push    ds
        mov     ds,ds:[PDB_Parent_Pid]
        cmp     ds:[PDB_Parent_Pid],0           ; is parent's parent pid field 0?
        pop     ds
        jne     alloc_seg                       ; no, we're not the first process
                                                ; so don't muck with the env.
alloc_new:
        inc     AllocedEnv                      ; note we have virgin env.

alloc_seg:
;
; Allocate default environment size
;
        mov     cx,bx                           ; save byte-granular size in CX
        add     bx,15
        shr     bx,1
        shr     bx,1
        shr     bx,1
        shr     bx,1                            ; BX = # paras
	mov	ah,ALLOC
	int	21h
        jnc     init_ok
        jmp     init_nomem                      ; insufficient memory, error
;
; If a previous environment existed (ie, DI != 0), then copy it into
; the new buffer
;
init_ok:
	mov	es,ax
        assume  es:nothing                      ; es = temp env segment

        or      di,di
        jz      copy_path

        push    cx
        push    ds
        mov     ds,EnvirSeg
        assume  ds:nothing
        sub     si,si
        mov     cx,di
        sub     di,di
        rep     movsb
        pop     ds
        assume  ds:RESGROUP
        pop     cx
        sub     cx,di

copy_path:
;
; First clear out (the rest of) the buffer
;
        push    di
        sub     ax,ax
        rep     stosb
        pop     di
;
; Initialize the path string (PATH=) first
;
        mov     si,offset RESGROUP:PathString   ; DS:SI -> "PATH=\0"
        cmp     byte ptr [si],al                ; add it?
        je      init_prompt                     ; no
        mov     cx,PathStrLen+1                 ;
        rep     movsb                           ;
        cmp     AllocedEnv,al                   ; virgin env?
        je      init_prompt                     ; no
;
; Establish a more reasonable default for the PATH
;
	mov	ah,GET_DEFAULT_DRIVE
	int	21h
        add     al,'A'                          ; convert to letter
        mov     [DefPathString],al              ;
        mov     [DefPath2String],al             ; now our default paths are complete

        mov     dl,0                            ; get dir for default drive
        push    ds                              ;
        push    es                              ;
        pop     ds                              ;
        mov     byte ptr [di],'\'               ;
        lea     si,[di+1]                       ; set DS:SI -> available space
        mov     ah,Current_Dir                  ;
        int     21h                             ;
        pop     ds                              ;

        mov     cx,DefPathStrLen+1              ;
        mov     dx,offset RESGROUP:DefPathString
        mov     si,dx                           ;
        mov     ah,CHDir                        ;
        int     21h                             ;
        jnc     init_setpath                    ; DefPathString exists!

        mov     cx,DefPath2StrLen+1             ;
        mov     dx,offset RESGROUP:DefPath2String
        mov     si,dx                           ;
        mov     ah,CHDir                        ;
        int     21h                             ;
        jc      init_prompt                     ; DefPath2String doesn't exist

init_setpath:
        mov     dx,di                           ; success
        push    ds                              ; so restore prev dir
        push    es                              ;
        pop     ds                              ; DS:DX -> prev dir
        mov     ah,CHDir                        ;
        int     21h                             ;
        pop     ds                              ;

        dec     di                              ; then copy in DefPathString
        rep     movsb                           ; DS:SI -> "C:\\DOS\0"
;
; Initialize the default prompt
;
init_prompt:
        push    di                              ;
        sub     ax,ax                           ;
        mov     cx,64                           ; insure any data read in
        rep     stosb                           ; from Current_Dir is zapped
        pop     di                              ;

        cmp     AllocedEnv,al                   ; virgin env?
        je      init_comspec                    ; no
        mov     si,offset RESGROUP:PrmptString  ; DS:SI -> "PROMPT=$P$G\0"
        cmp     byte ptr [si],al                ; add it?
        je      init_comspec                    ; no
        mov     cx,PrmptStrLen+1                ;
        rep     movsb                           ;
;
; Initialize the Comspec string
;
init_comspec:
        cmp     ComspOffset,ax                  ; add it?
        jne     init_done                       ; no
        lea     ax,[di+ComspStrLen]             ;
        mov     ComspOffset,ax                  ;
        mov     si,offset RESGROUP:ComspString  ; DS:SI -> "COMSPEC=\\COMMAND.COM\0"
        mov     cx,ComspStrLen2+1               ;
        rep     movsb                           ;

init_done:
        mov     ax,es                           ; return env seg in ax
        mov     EnvirSeg,ax                     ; save env seg
        inc     AllocedEnv                      ; remember that *we* alloced it

alloc_done:
	pop	di
	pop	si
	pop	es
        pop     ds
        assume  ds:nothing
	ret

init_nomem:
;
;We call the error routine from here. This routine never returns. It either
;terminates COMMAND with error( if it is not the first invocation ) or hangs
;the system ( if it is the first COMMAND.COM ).
;
	call	alloc_error

Alloc_env	endp


;*** Alloc_error: This routine just jumps to the actual label where we 
; check if this is a permanent or secondary command.com and take the 
; appropriate action.
;
;	ENTRY:	ds = RESGROUP = DATARES
;
;	EXIT:	None - does not return
;
;	REGISTERS AFFECTED: Does not matter
;

public Alloc_error
Alloc_error	proc	near

	jmp	RESGROUP:BadMemErr

Alloc_error	endp

;*** Patch_stub -- This routine patches in the segment and offset values in
; the stub table of the various entry points in the resident code segment.
; Some of them are interrupt entry points and some of them are entries from
; the transient to the resident code segment.
;
;	ENTRY:	ds = RESGROUP
;		es:di = segment:offset of final location of resident code
;
;	EXIT:	All segments and offsets patched into the stub table
;
;	REGISTERS AFFECTED: ax, bx, cx, dx, si, di
; 
;
Patch_stub	proc	near

	assume	ds:RESGROUP
	
	push	es

	mov	bx,es			;bx = resident code segment
	mov	dx,di
	mov	di,offset DATARES:Int2f_Entry
	mov	si,offset RESGROUP:Reloc_Table
	push	ds
	pop	es			;es = RESGROUP = DATARES
;
;bx:dx = segment:offset of resident code segment
;es:di = entry point table in stub
;ds:si = offset table in INIT segment -- offsets of code entry points now
;
	mov	cx,NUM_RELOC_ENTRIES		;number of entry points
patchlp:
	lodsw				;get current offset
	add	ax,dx			;offset it by code seg location 
	stosw				;store offset
	mov	ax,bx			
	stosw				;store segment 
	loop	patchlp

	pop	es
	ret

Patch_stub	endp

;*** Patch_segs -- This routine patches the segment values in the dword 
; pointers that the stub uses to jump to the actual handler. These values 
; are temporarily needed to handle these interrupts if they occur before
; the resident is relocated to its final position and all the addresses of
; the handlers have been updated.
;
;	ENTRY:	es = PSP segment = code segment
;
;	EXIT:	Current segment values patched into the jump table in the
;		stub.
;
;	REGISTERS AFFECTED: ax, cx, di
;

Patch_segs	proc	near

	mov	di,offset RESGROUP:Int2f_Entry
	mov	cx,4			;we have to patch 4 handlers
	add	di,2
	mov	ax,es

pseglp:
	stosw				;store the segment value
	add	di,2			;skip the next offset value
	loop	pseglp

	ret

Patch_segs	endp


;*** get_XMMAddr -- This routine gets the call address for the XMM driver
; by issuing the appropriate int 2fh. This is stored in a stub variable 
; and is used by the stub when we have to jump to the resident in HMA
;
;	ENTRY:	ds = RESGROUP
;
;	EXIT:	XMMCallAddr = XMM driver far call address
;
;	REGISTERS AFFECTED:
;

get_XMMAddr	proc	near
	assume	ds:RESGROUP

	push	es

	mov	ax,XMM_MULTIPLEX SHL 8 + XMM_INSTALL_CHECK
	int	2Fh
	cmp	al,80h			; Q: installed
	jne	short cXMMexit	;   N: set error, quit
;
; get the XMM control functions entry point, save it, we
; need to call it later.
;
	mov	ax,XMM_MULTIPLEX SHL 8 + XMM_FUNCTION_ADDR
	int	2Fh

	mov	word ptr [XMMCallAddr], bx
	mov	word ptr [XMMCallAddr+2],es

cXMMexit:
	pop	es
	ret				; done

get_XMMAddr	endp


INIT	ENDS

	END
