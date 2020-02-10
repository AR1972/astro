	TITLE	OSSTMT - OS Unique features for BASCOM-86

;***
; OSSTMT - OS Unique features for BASCOM-86
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains Operating system-unique features for supporting
;	BASIC-callable DOS functions.
;
; BASIC Syntax mapping to the included runtime entry points:
;
; - ENVIRON Statement:
;
;      ENVIRON string
;	  |
;      B$SENV
;
; - ENVIRON$ Function:
;
;    Two types of arguments produce unique runtime calls:
;
;      stringvar = ENVIRON$(num)	 stringvar = ENVIRON$(string)
;		      | 				|
;		   B$FEVI			      B$FEVS
;
; - ERDEV Variable:
;
;      v = ERDEV
;	     |
;	  B$ERDV
;
; - ERDEV$ Variable:
;
;      v = B$ERDEV
;	      |
;	   B$ERDS
;
; - IOCTL$ Function:
;
;      IOCTL$ ([#]filenumber)
;	 |
;     B$FICT
;
; - IOCTL Statement:
;
;      IOCTL [#]filenumber, string
;	 |
;     B$SICT
;
; - SHELL Statement:
;
;      SHELL [commandstring]
;	 |
;     B$SSHL
;
; - SHELL Function:
;	x = SHELL(commandstring)
;	      |
;	    B$FSHL
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	<LOADRTM>	
	USESEG	<OS_TEXT>	
	USESEG	<ER_TEXT>	
	USESEG	<RT_TEXT>	
	USESEG	<ST_TEXT>	
	USESEG	<NH_TEXT>	
	USESEG	<FH_TEXT>	

	USESEG	<_DATA> 	
	USESEG	<_BSS>		

	INCLUDE seg.inc 	
	INCLUDE oscalls.inc	
	INCLUDE baslibma.inc
	INCLUDE files.inc
	INCLUDE devdef.inc
	INCLUDE addr.inc
	INCLUDE ascii.inc
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE string.inc	
	INCLUDE idmac.inc	
	INCLUDE const.inc	



ExecParamStruc	STRUC		

ExecEnvSeg	DW	?	;environment segment
CmdLineOff	DW	?	;offset of command line tail
CmdLineSeg	DW	?	;segment of command line tail
FileBlock1Off	DW	?	;offset of first FCB (offset 5CH in PSP)
FileBlock1Seg	DW	?	;segment of first FCB
FileBlock2Off	DW	?	;offset of second FCB (offset 6CH in PSP)
FileBlock2Seg	DW	?	;segment of second FCB

ExecParamStruc	ENDS		



	externFP UiReInit	;hooks interrupts for UI
	externFP UiPause 	;unhooks UI interrupts

;	Operating System Equates and Macros

sBegin	_DATA			

	externW b$pCommSave	;conditional vector to B$CommSave
	externW b$pCommRestore	;conditional vector to B$CommRestore
	externW b$nuldes	
	externW b$shli_disp	;SHELL initialization dispatch vectors
	externW b$shlt_disp	;SHELL termination dispatch vectors
	externW b$CURSOR	; current cursor position
	externB b$WDOBOT	; text window bottom

sEnd	_DATA			

sBegin	_BSS			

	externB b$PATHNAM	
	externW b$ERDEV
	externW b$ERDEVP
	externW b$env_len


	externB b$Buf2		; overlaid with EXEC parameter blocks

Fcb1Blk EQU	b$Buf2		;block for first FCB
Fcb2Blk EQU	b$Buf2+10H	;block for second FCB
ExecBlk EQU	b$Buf2+20H	;parameter block for EXEC call
IF	(20H + SIZE ExecParamStruc) GT FILNAML		
	ERROR -- b$Buf2 not big enough for EXEC parameter blocks
ENDIF							



sEnd	_BSS			

	externFP __ctermsub	; C math termination (returns)
	externFP B$cinitsub	; C math initialion (returns)

sBegin	ER_TEXT 		

	externNP B$FrameAFE	
	externNP B$ERR_FC
	externNP B$ERR_OM
	externNP B$ERR_IOE
	externNP B$ERR_IFN

sEnd	ER_TEXT 		

sBegin	NH_TEXT 		
	externNP B$STDALCTMP	
	externNP B$STALCTMP	
sEnd	NH_TEXT 		

sBegin	FH_TEXT			
	externNP B$FHShlTerm	; trick to drag in fhchn.asm!
sEnd	FH_TEXT			

	externFP B$COMP_DISP	;generalized dispatcher

	externFP B$RTMInstall	;routine to install RTM dispatch vector
	externFP B$RTMDeinstall ;routine to deinstall RTM vector

sBegin	RT_TEXT 		

	externNP B$$TCR		; do a CR/LF
	externNP B$GETCSRDATA	; get current cursor position and type.
	externNP B$SCNLOC	; update cursor position
	externNP B$OVWCSR	; turn on overwrite cursor
	externNP B$OFFCSR	; turn off cursor
	externNP B$GRMODE	; test for graphics mode


sEnd	RT_TEXT 		

	SUBTTL	SHELL support shared with QBI RTM loader
	PAGE
assumes CS,LOADRTM		
sBegin	LOADRTM 		

;***
; B$SHELL - SHELL the specified process.
;
;Purpose:
;	Added with revision [21].
;	SHELL the requested process and return an error code.
;Entry:
;	AX = environment seg to use					
;	CX:DX = ptr to program pathname to be execed.
;	BX = DGROUP ptr to argument list.
;	SI = DGROUP ptr to argrments to be parsed into FCB blocks.
;Exit:
;	AX = 0 if no error occurred.
;Uses:
;	Per Convention.
;Exceptions:
;	None.
;******************************************************************************

assumes	ES,DGROUP		
cProc	B$Shell,<PUBLIC,FAR>,<SI,DI,ES>
cBegin
;	Perform the DOS 3 shell operation.

	PUSH	DS		;set ES to DS
	POP	ES

;	Fill the EXEC parameter block.

	MOV	WORD PTR ExecBlk.ExecEnvSeg,AX	;use specified ENV block
	MOV	WORD PTR ExecBlk.CmdLineOff,BX	;set command line offset
	MOV	WORD PTR ExecBlk.CmdLineSeg,DS ;set cmdline far ptr
	MOV	WORD PTR ExecBlk.FileBlock1Off,OFFSET DGROUP:Fcb1Blk
	MOV	WORD PTR ExecBlk.FileBlock1Seg,DS ;set first FCB far ptr
	MOV	WORD PTR ExecBlk.FileBlock2Off,OFFSET DGROUP:Fcb2Blk
	MOV	WORD PTR ExecBlk.FileBlock2Seg,DS ;set second FCB far ptr

;	Parse the program name to point to first parameter.
;	Parse the first two parameters to the default FCB blocks.

	MOV	DI,OFFSET DGROUP:Fcb1Blk ;point to first FCB block
	MOV	AX,2901H	;function to parse - strip leading blanks
	INT	21H		;dummy FCB fill - SI points to first param
	INT	21H		;fill first FCB - SI points to second param
	MOV	DI,OFFSET DGROUP:Fcb2Blk ;point to second FCB block
	INT	21H		;fill second FCB - (unused params null)

;	Set up the registers for the EXEC, save SS:SP in CS, and
;	perform the EXEC call.

	PUSH	DS		;save DGROUP segment on stack
	PUSH	BP		;save frame pointer on stack
	MOV	DS,CX		;set COMSPEC segment in data segment

assumes DS,NOTHING

	MOV	BX,OFFSET DGROUP:ExecBlk ;ES:BX has pointer to param block
	MOV	AX,4B00H	;DOS function to EXEC program


	MOV	CS:SavedSSinCS,SS ;save the stack segment in CS
	MOV	CS:SavedSPinCS,SP ;save the stack pointer in CS

	INT	21H		;EXEC - all regs changed except CS:IP

;	After EXEC return, restore the stack and save any error code.

	CLI			;turn off interrupts
	MOV	SS,CS:SavedSSinCS ;restore the stack segment
	MOV	SP,CS:SavedSPinCS ;restore the stack pointer
	STI			;turn interrupts back on


	POP	BP		;restore frame pointer from stack
	POP	DS		;restore the data segment

assumes DS,DGROUP
	JC	ExecError	;jump if error, AX has error code
	XOR	AX,AX		;if no error, then no error code
ExecError:

cEnd

;	Stack segment and offset pointer must be saved in the
;	current code segment for restoration after the EXEC call.


SavedSPinCS	DW	?	;saved stack pointer before EXEC
SavedSSinCS	DW	?	;saved stack segment before EXEC

assumes	ES,NOTHING		

sEnd	LOADRTM 		

	PAGE
assumes CS,OS_TEXT		
sBegin	OS_TEXT 		
	externNP B$LHFDBLOC	
	externNP B$SHLSAV	
	externNP B$SHLRST	
	externNP B$SEGINI

	externNP B$BldExecCmdLine ;parse and build command line for exec
	externNP B$FIRST_ENV
	externNP B$NEXT_ENV

;***
; B$ShellSetup - Set up runtime for a SHELL.
;
;Purpose:
;	Added with revision [21].
;	Prepares runtime for a SHELL by getting off of interrupts,
;	and compressing near and far heaps as much as possible.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	Per Convention.
;Exceptions:
;	None.
;******************************************************************************
cProc	B$ShellSetup,<PUBLIC,FAR>,<SI>
cBegin

;	For QBI, let User Interface off of it's interrupts.

	PUSH	ES		
	CALL	UiPause		; Let UI Get it's BUTT out of the way.
	POP	ES		

;	Disable and preserve the runtime state.

	MOV	SI,OFFSET DGROUP:b$shlt_disp ;SHELL term disp vector
	cCall	B$COMP_DISP	;do SHELL termination

	CALL	B$SHLSAV	;perform low-level SHELL save routine
	CALL	[b$pCommSave]	
	CALL	__ctermsub	; terminate math

;	give the shelled program a cursor
	MOV	AX,OFFSET CS:B$OVWCSR	; assume to turn on cursor
	call	B$GRMODE	; graphics mode?
	JZ	DSPL_CURSOR	; brif not -- display overwrite cursor
	MOV	AX,OFFSET CS:B$OFFCSR	; turn off the cursor
DSPL_CURSOR:
	MOV	DX,b$CURSOR	; at current position
	CALL	AX		; display appropriate cursor
NoCursor:			


;	For DOS 3 RTM and QBI UL, deinstall the RTM dispatch interrupt

	CALL	B$RTMDeinstall ;remove the vector

cEnd
	PAGE
;***
; B$ShellRecover - Restore runtime after a SHELL.
;
;Purpose:
;	Added with revision [21].
;	Restores runtime to the pre-SHELL state by reinstalling interrupts,
;	and expanding near and far heaps as much as possible.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	Per Convention.
;Exceptions:
;	None.
;******************************************************************************
cProc	B$ShellRecover,<PUBLIC,FAR>,<SI>
cBegin
;	For DOS 3 RTM, reinstall the RTM dispatch interrupt

	CALL	B$RTMInstall	;relink the interrupt vector

;	Restore the runtime state.

	CALL	B$cinitsub	; re-init math
	CALL	B$SEGINI 	;establish the new DGROUP value
	CALL	B$SHLRST	;perform low-level initialize after SHELL

	CALL	B$GETCSRDATA	; DX,CX = current cursor position,type
	CALL	B$SCNLOC	; update cursor position variables, since
				; the SHELL might have modified it, and
				; display user cursor.
	CMP	DL,b$WDOBOT	; outside of current text window now?
	JBE	NoCursorRestore	; brif not -- don't scroll
	CALL	B$$TCR		; do a CR/LF to scroll the screen and
				; (possibly) bring cursor position back
				; into current text window range
NoCursorRestore:		

	MOV	SI,OFFSET DGROUP:b$shli_disp	;SHELL init disp vector
	cCall	B$COMP_DISP	;do SHELL reinitialization



	CALL	[b$pCommRestore] ;restore the communication device state

;	For QBI, let User Interface off of it's interrupts.

	CALL	UiReInit	;Let UI Get it's BUTT back in the way.

cEnd
	PAGE
;***
; B$FSHL - asynchronous SHELL function runtime routine (DOS 5)
; Purpose:
;	B$FSHL performs an asynchronous SHELL function and
;	returns the process ID of the executing task.
;
; Inputs:
;	BX - string descriptor of EXEC command string
; Outputs:
;	AX - Process ID of spawned process.
; Modfies:
;	None.
; Uses:
;	None.
; Exceptions:
;	Advanced feature error if not OS/2
;******************************************************************************

cProc	B$FSHL,<PUBLIC,FAR>,<SI,DI,ES>

cBegin				
	JMP	B$FrameAFE	; advanced feature error + frame setup
cEnd	<nogen>


;***
; B$SSHL - SHELL command runtime routine
; Purpose:
;	B$SSHL saves its current environment and
;	performs an EXEC call with the string argument given.  The
;	saved environment is then restored.
;
; Inputs:
;	sdExec/psdExec - sd or psd of EXEC command string
; Outputs:
;	None.
; Modifies:
;	None.
; Uses:
;	None.
;*****************************************************************************

cProc	B$SSHL,<PUBLIC,FAR>,<SI,DI,ES>
parmSD	sdExec			
cBegin


	PUSH	DS		;get DGROUP on stack...
	POP	ES		;...and set ES to DGROUP
assumes	ES,DGROUP		

	GetpSD	BX,sdExec	; [BX] = psdExec
	cCall	B$BldExecCmdLine ;parse command line into b$PATHNAM

	PUSH	AX		;save COMSPEC far ptr offset
	PUSH	DX		;save COMSPEC far ptr segment

	cCall	B$ShellSetup	;prepare for SHELL operation

;	Perform the DOS 5 shell operation.


;	Perform the DOS 3 shell operation.

	POP	CX		;get COMSPEC segment (saved as DX)
	POP	DX		;get COMSPEC offset (saved as AX)

	MOV	SI,OFFSET DGROUP:b$PATHNAM ;get pointer to cmdline byte count
	MOV	BX,SI		;get ptr to command tail.
	CMP	BYTE PTR [SI],0	;test if null string given
	JZ	ExecNullCmd	;if so, process to get null FCB's
	MOV	SI,OFFSET DGROUP:b$PATHNAM+4 ;point to program name (past /C)

ExecNullCmd:			
	XOR	AX,AX		;tell B$SHELL to use current ENV block
	cCall	B$Shell	;call common code to do the SHELL.

	PUSH	AX		;Save return code for later

	cCall	B$ShellRecover ;reset runtime to PreSHELL state.

;	Now that runtime is restored, check return code

	POP	AX		;recover error code
	OR	AX,AX		;error
	JZ	SHL_NOT_FAILED	;if not, then jump
	CMP	AX,8		;test if out of memory
	JE	ShellMemErr	;if so, then jump
	JMP	B$ERR_FC	;else illegal function call
ShellMemErr:			
	JMP	B$ERR_OM	;give out-of-memory error
SHL_NOT_FAILED:


cEnd


;***
; B$SENV - Environ statement
; Purpose:
;	Argument is a string of the syntax <env_name>=<env_value>.
;	If <env_value> is nonnull and <env_name> exists, replace it.
;	If <env_value> is nonnull and <env_name> does not exist, add it.
;	If <env_value> is null, delete <env_name> if it exists.
;
; Inputs:
;	string descriptor of argument string
; Outputs:
;	None.
; Modifies:
;	None.
; Exceptions:
;	B$ERR_FC if function argument error.
;******************************************************************************

cProc	B$SENV,<PUBLIC,FAR>,<SI,DI,ES> 
parmSD	sdEnviron		
cBegin
	PUSH	DS		; Set es=DS
	POP	ES
	MOV	BX,sdEnviron	;get environment descriptor in BX
	mov	cx,[bx] 	;length of string
	mov	si,[bx+2]	;count in string
	push	bx
	mov	dx,cx
	push	dx
	push	si
	jcxz	errfc
envsf1: 			;look for blank or equals:  address of
	lodsb			;string to move into environment
	cmp	al,' '
	jz	envsf2
	cmp	al,'='
	jz	envsf2
	loop	envsf1
	JMP	SHORT ERRFC	;error if no delimiter

envsf2:
	sub	dx,cx		;size of parm
	jz	errfc		;zero is bad
envsf3:
	dec	cx		;adjust length
	jz	envsf4		;no text follows - delete
	lodsb
	CMP	AL,'='		;test for equals sign
	JNZ	ENVS3A		;if not, then jump
	DEC	CX		;adjust length
	JZ	ENVSF4		;no text follows - delete
	LODSB			;get character after '='
	JMP	SHORT ENVS3B	;jump to test for semicolon, etc.
ENVS3A:
	cmp	al,' '
	jz	envsf3		;strip leading blanks
ENVS3B:
	cmp	cx,1		;if 1 left
	jne	envsf4
	cmp	al,';'		; and it is semi-colon
	jz	envsf3		;then assume delete
envsf4:
	dec	si		;adjust to beginning of text
	pop	di		;start of string
	push	cx		;save length of arg text
	CALL	B$FIRST_ENV	;get start of env in bx
	mov	cx,dx		;cx = length of parm
envsf5:
	mov	dx,si		;dx = start of next
	mov	si,bx		;si = start of this
	push	di		;save string start
	jz	envsf9		;if end, add new parm and text
	push	cx		;save parm length
	cld
	repz	cmpsb		;compare parm with env parm
	je	envsf7		;matched
envsf6:
	mov	si,dx		;don't want tz unless real end
	CALL	B$NEXT_ENV	;go to next env parm
	pop	cx		;length and
	pop	di		; parm start back
	jmp	envsf5

errfc:	jmp	B$ERR_fc

envsf7:
	lodsb			;get next byte from environment
	cmp	al,'='		;must have equals to be a match
	jne	envsf6		;try next
	pop	ax		;trash parm length
	mov	si,dx		;si = address of next parm
	mov	di,bx		;di = address of this parm
	mov	cx,es:b$env_len ;get total length of environment
	sub	cx,dx		;cx=endnext (length to compress)
	inc	cx		;copy includes table terminator
	push	di
	push	es		;save seg of string
	push	ds		;move env. seg into es
	pop	es
	rep	movsb		;compress
	pop	es		;restore seg of string
	pop	si		;start of this parm or end
envsf9:
	CALL	B$NEXT_ENV	;find end of table
	jnz	envsf9
	mov	di,si		;di = end of env table
	push	ds
	push	es
	pop	ds
	pop	es		;ds = string seg, es = env seg
	pop	si		;string start
	pop	ax		;ax = length of arg text
	pop	cx		;string length

	or	ax,ax
	jz	envsfx		;if arg text length then done
	mov	ax,di		;ax = current length of environ
	add	ax,cx		;+ new size
;INC	AX		;include terminator byte in length
assumes	ES,NOTHING		
	cmp	ax,b$env_len	;check if overflow
	jb	envs10		; no overflow
	push	ds
	pop	es
	jmp	B$ERR_om	;give out of memory error
envs10:
	lodsb
	cmp	al,'='		;blanks to right of '=' are included
	jz	envs11
	cmp	al,' '		;skip blanks
	jz	envs11
	stosb
	loop	envs10
	jmp	SHORT envsft	;terminate
envs11:
	DEC	CX		;one less character
	CMP	AL,' '		;test for blank character
	JNE	ENVS12		;if not blank, then jump
	JCXZ	ENVSFT		;if end of string, jump
	LODSB			;get the next character
	JMP	SHORT ENVS11	;process the character
ENVS12: 			;character after leading blanks
	CMP	AL,'='		;test for equals sign
	JE	ENVS13		;process rest of string as is
	PUSH	AX		;save character after blanks
	MOV	AL,'='		;put in equals sign
	STOSB			;and place it in the table
	POP	AX		;restore old character
ENVS13: 			;start of text string
	STOSB			;store first char (or explicit =)
	rep	movsb
envsft: 			;terminate label
	xor	ax,ax
	stosw			;00 ends env table
envsfx: 			;exit label
	push	ds
	pop	es
	pop	bx
	CALL	B$STDALCTMP	; delete if temp string desciptor
cEnd

;***
; B$SICT - IOCTL statement
; Purpose:
;	Send the specified string argument as an I/O-control
;	function to the file.
;
; Inputs:
;	file number
;	string to send
; Outputs:
;	None.
; Modifies:
;	None.
; Exceptions:
;	B$ERR_IFN if illegal file number.
;	B$ERR_FC if function error.
;	B$ERR_IOC if IOCTL error.
;******************************************************************************


cProc	B$SICT,<PUBLIC,FAR>,<SI> 
parmW	fileNum
parmSD	sdCtl			
cBegin
	MOV	BX,fileNum	;put file number in BX
	CALL	B$LHFDBLOC	; find FDB
	JZ	BADIFN2
	MOV	BX,sdCtl	; [BX] = string desc
	MOV	DX,[BX+2]	
	MOV	CX,[BX] 	
	PUSH	BX		; save
	MOV	AL,3
	FDB_PTR ES,SI,SI	;get FDB pointer
	MOV	BX,FileDB.FD_HANDLE 
	OR	BX,BX		;if zero, one of basic's devices
	JZ	ERRCTL2		
	CALLOS	IOCTL,ERRCTL,BX,CX,DX
	POP	BX		
	cCall	B$STDALCTMP	; deallocate it if it was temp
cEnd

ERRCTL2:			
	JMP	SHORT ERRCTL	
BADIFN2:			
	JMP	SHORT BADIFN	



;***
; B$FEVS - ENVIRON$ function with string argument
; Purpose:
;	With <env_name> as an input argument, return <env_value> if
;	the entry "<env_name>=<env_value>" is in the environment table.
;
; Inputs:
;	string descriptor of <env_name>
; Outputs:
;	AX = ptr to string descriptor of <env_value>
; Modifies:
;	None.
; Exceptions:
;	B$ERR_FC if function error.
;******************************************************************************

cProc	B$FEVS,<PUBLIC,FAR>,<SI,DI,DS,ES>	
parmSD	sdEnv			
cBegin
	PUSH	DS		
	POP	ES		
assumes	ES,DGROUP		
	MOV	BX,sdEnv	;get string desc. in BX
	MOV	SI,[BX+2]	;address of string
	MOV	CX,[BX] 	;length
	MOV	DX,CX		;save
	PUSH	SI
	JCXZ	BADFC
ENVRS1:
	LODSB
	CMP	AL,' '		;error if embedded blanks
	JZ	BADFC
	CMP	AL,'='		;or '='
	JZ	BADFC
	LOOP	ENVRS1
	POP	DI		;address of string
	PUSH	DX		;save length
	CALL	B$FIRST_ENV	;get first env parm
	MOV	AX,CX		;save length of env table entry
	POP	CX		;length back
ENVRS5:
	MOV	DX,SI		;start of next
	MOV	SI,BX		;start of this
	JNZ	ENVR5A		;not end of list (yet)
	XOR	CX,CX		;end of list returns null string
	JMP	SHORT GOGOTENV	;and jump to process it
ENVR5A:
	PUSH	DI		;save
	PUSH	CX
	REPZ	CMPSB		;compare parms
	JZ	ENVRS7		;matched
ENVRS6:
	MOV	SI,DX
	CALL	B$NEXT_ENV	;get next
	MOV	AX,CX		;save length of env table entry
	POP	CX
	POP	DI
	JMP	ENVRS5
ENVRS7:
	CMP	BYTE PTR [SI],'=' ;must have equals
	JNE	ENVRS6
	INC	SI		;SI points after '=' now
	POP	CX		;restore string length
	POP	DI
	XCHG	CX,AX		;CX now length of table entry...
				;AX now length of string parameter
	SUB	CX,AX		;CX now length of '=' and value
	DEC	CX		;CX now length of just value
GOGOTENV:			
	CALL	GOTENV		; do the common code (returns DX:AX = sd)
	PUSH	AX		; save psd returned by GOTENV
	MOV	BX,sdEnv	
	PUSH	ES		
	POP	DS		; [DS] = dgroup again
assumes	ES,NOTHING		
	cCall	B$STDALCTMP	; deallocate it if it was temp
	POP	AX		; ax = pointer to descriptor
cEnd

ERRCTL:
	CMP	AX,ERRIDT
	JNE	BADFC		
	JMP	B$ERR_IOE

BADIFN:				
	JMP	B$ERR_IFN	
BADFC:
	JMP	B$ERR_FC


;***
; B$FEVI - ENVIRON$ function with integer argument
; Purpose:
;	Returns the ordinal i environment table entry, where
;	i is the input integer argument.
;
; Inputs:
;	integer value in range 1 to 255.
; Outputs:
;	AX = ptr to string descriptor to a copy of the table entry.
; Modifies:
;	None.
; Exceptions:
;	B$ERR_FC if function error.
;******************************************************************************

cProc	B$FEVI,<PUBLIC,FAR>,<SI,DI,DS,ES> 
parmW	EnvInt
cBegin
	PUSH	DS		
	POP	ES		;set es=ds
	MOV	BX,EnvInt	;get integer in BX
	MOV	DX,BX
	OR	BX,BX		;test for zero argument
	JZ	BADFC		;and error if so...
	CMP	BX,255
	JA	BADFC
	CALL	B$FIRST_ENV	;get first parm in env
ENVNF1:
	JZ	ENVNF2		;end, return null
	DEC	DX
	JZ	ENVNF2		;return pointer in BX
	CALL	B$NEXT_ENV
	JMP	SHORT ENVNF1
ENVNF2:
	MOV	SI,BX		;GOTENV needs pointer in SI
	CALL	GOTENV		;call common code (returns DX:AX = sd)
cEnd				

GOTENV:
	PUSH	DS		;save ENV segment
	PUSH	ES		;put DGROUP seg on stack
	POP	DS		;restore DS to DGROUP
	PUSH	CX		;save length
	MOV	BX,CX
	CALL	B$STALCTMP	; allocate string temp
	POP	CX		;count
	POP	DS		;get ENV segment back in DS
	XCHG	AX,BX		; [AX] = psd
	MOV	DI,DX		;target
	REP	MOVSB		;move into target
	RET			; return with [DX:AX] = sd or [AX] = psd

;***
; B$FICT - IOCTL$ function
; Purpose:
;	Returns a status string from the specified file using
;	an IOCTL call.
;
; Inputs:
;	 file number to get status
; Outputs:
;	AX = pointer to string descriptor of status string
; Modifies:
;	None.
; Exceptions:
;	B$ERR_FC if function call error.
;	B$ERR_IOC if IOCTL error.
;******************************************************************************

cProc	B$FICT,<PUBLIC,FAR>,<SI,DI,ES> 
parmW	fileNum
cBegin
	MOV	BX,fileNum	;get file number in BX
	CALL	B$LHFDBLOC	
	JZ	BADIFN		
	FDB_PTR ES,SI,SI	;get FDB pointer
	MOV	BX,FileDB.FD_HANDLE 
	OR	BX,BX		;if zero, one of basic's devices
	JZ	ERRCTL
	MOV	DX,OFFSET DGROUP:b$PATHNAM ; DX = buffer address
	MOV	AL,2		;read IOCTL info
	CALLOS	IOCTL,ERRCTL,BX,255,DX
	MOV	CX,AX		;[CX] = number of bytes read
	XCHG	AX,BX
	MOV	SI,DX		;address of ioctl data
	CALL	B$STALCTMP	; allocate string temp
	MOV	DI,DX		;target work area
	PUSH	DS		
	POP	ES		
	REP	MOVSB
	XCHG	AX,BX		;return sd in AX
cEnd


;***
; B$ERDV - return device error word
; Purpose:
;	Return the value of the device error word set by the
;	last error.
;
; Inputs:
;	None.
; Outputs:
;	AX = device error word.
; Modifies:
;	None.
; Exceptions:
;	None.
;******************************************************************************

cProc	B$ERDV,<PUBLIC,FAR>
cBegin
	MOV	AX,b$ERDEV
cEnd

;***
; B$ERDS - get device error string
; Purpose:
;	Returns the descriptor of the device error string.
;
; Inputs:
;	None.
; Outputs:
;	AX = address of device error string if set, otherwise null string.
; Modifies:
;	None.
; Exceptions:
;	None.
;******************************************************************************

cProc	B$ERDS,<PUBLIC,FAR>
cBegin
	MOV	BX,OFFSET DGROUP:b$ERDEVP ;address of string
	CMP	WORD PTR [BX],0 ;set yet ?
	MOV	AX,BX		;put sd in AX
	JNE	DVRET
	MOV	AX,OFFSET DGROUP:b$nuldes ;no - use null
DVRET:
cEnd

sEnd	OS_TEXT 		
	END
