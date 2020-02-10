; ************************************************************************
;
; This file is contains the guts of "minicmd.com." It has all the functions
; to handle the intrinsic commands like SET, ECHO, IF, GOTO, LABEL.
; It also allows extrinsic commands (.COM, .EXE) to be launched but
; a full path must be specified or the file should be on the current
; working directory. Note that it also has a Control-C and critical error
; handler. The main motivation for "minicmd.com" is to have a program
; launcher for dedicated ROM based systems where the full functionality
; of our normal "command.com" is not needed. Note that we only handle
; subsets of the internal commands too! For example, we do not handle
; replaceable arguments like "%ABC", etc in our "SET" command!
; Since it is very small in size, we can have a bootable and use-able
; system that fits in a single 64K ROM!
;
; ************************************************************************
;
; Scope for future modifications/enhancements:
;
; Make it so that there is a generic GET_LINE proceudre that will get
;   a line from the BAT file or STDIN to a local buffer in our DATASEG
;   We can then manipulate the contents of this buffer. This will save 
;   us the head-ache of DS not being DSEG when processing the BAT file
;   at INIT time.
;
; It would be nice to handle the '@' char/cmd in the BAT file.
;
;
;			Revision History
;			-----------------
;
; M002	SHK	B#2518. Make sure that there is a WORD 0 at the end of
;		the ENV (to be compatible with COMMAND.COM).
;
; M004	SHK	Removed almost all BUG BUGs from this file and added
;		better comments.
;
; M006	SHK	Changed a JZ to a JBE in function fn process_initfile.
;


CSEG	SEGMENT	PARA PUBLIC 'CODE'
CSEG	ENDS

INITIALIZED_DATASEG	SEGMENT PARA PUBLIC 'DATA'
INITIALIZED_DATASEG 	ENDS

UN_INITIALIZED_DATASEG	SEGMENT BYTE PUBLIC 'DATA'
UN_INITIALIZED_DATASEG	ENDS

	; include all the constants used in this file.
	INCLUDE minicmd.inc

	; include the data used in the program (initialized and un-initialized).
	INCLUDE dataseg.asm

IFNDEF RAM_VERSION

; BootFlag bit we're concerned with:
BF_NoConfig	EQU	00000001b	; No INIT file processing.

	INCLUDE msbdata.inc
ENDIF


CSEG	SEGMENT	PARA PUBLIC 'CODE'

	assume cs:CSEG,es:nothing,ds:nothing,ss:nothing

start:
IFDEF RAM_VERSION
start_ram PROC FAR
	mov	ax, cs
	add	ax, 10h
	mov	ds, ax		; make DS so that offsets are 0 based!
	mov	ax, offset main_shell

	push	cs		; this will be made ES in main_shell

	push	ds		; push SEG:OFF of
	push	ax		; routine main_shell and
	ret			; transfer control to it.
start_ram ENDP
ENDIF

; number of PARAs for the code in CSEG alone!
;
CODE_NUM_PARAS	EQU ((((offset last_code) - (offset start)) + 0fh)/16)

; number of PARAs for the code in CSEG + that for the PSP! PSP needs
; 10h PARAs = (CODE_NUM_PARAS + 10h)!
;
SEG_TRUNC_SIZE 	EQU ((((offset last_code) - (offset start)) + 10fh)/16)

; number of PARAs needed for the initialized data.
;
INITIALIZED_DATA_NUM_PARAS EQU ((((offset end_initialized_data) - \
			         (offset begin_initialized_data))+ 0fh)/16)

; number of PARAs needed for the un-initialized data.
;
UN_INITIALIZED_DATA_NUM_PARAS EQU ((((offset end_un_initialized_data) - \
				 (offset begin_un_initialized_data))+ 0fh)/16)

; number of PARAs for DATA in DSEG -- this includes some code sitting there
; the INITIALIZED data and the un-initialized data!
;
DATA_NUM_PARAS	EQU (INITIALIZED_DATA_NUM_PARAS + \
						UN_INITIALIZED_DATA_NUM_PARAS)

; number of bytes needed for the STACK. This may be reduced appropriately
; as such a big stack may not be needed.
;
MYSTACK_SIZE	EQU 1024

; number of PARAs needed for the stack
;
MYSTACK_PARAS	EQU ((MYSTACK_SIZE + 0fh)/16)

; number of bytes needed for the environment. This may be reduced appropriately
; as such a big senvironment is usually not needed by an user/application.
ENV_SIZE	EQU 4096	; ENV size in bytes.

; number of PARAs needed for the environment
;
ENV_PARAS	EQU ((ENV_SIZE + 0fh)/16)

; Note that the 10h below is the num. of paras for the PSP!
; number of PARAs needed in RAM to run the program. Memory is dynamically
; allocated for processing the INIT file.
;
ROM_NUM_PARAS	EQU (10h + MYSTACK_PARAS + DATA_NUM_PARAS + ENV_PARAS)



;****************************************************************************
;*
;* ROUTINE:	main_shell
;*		
;*
;* FUNCTION: 	This is the main routine that processes the INITFILE and
;*		reads lines from stdin and processes user commands!
;*		
;*
;* INPUT:	In RAM_VERSION stack has the PSP segment, else NONE.
;*		INIT file if present is read in at start up. Reads commands
;*		from stdin after processing INIT file.
;*
;* OUTPUT:	Processes the user commands from the INIT file or stdin.
;*		Sends output to stdout.
;*
;* REGISTERS:	ALL destroyed.
;****************************************************************************

main_shell PROC NEAR

IFDEF RAM_VERSION
	pop	es	; ES is the PSP segment!
ENDIF

	; now re-size and free up un-used memory!
	;
IFDEF RAM_VERSION
	mov	bx, SEG_TRUNC_SIZE
	mov	ah, 4ah
	int	21h
ELSE
	mov	bx, ROM_NUM_PARAS
	mov	ah, 4ah
	int	21h
ENDIF	

IFNDEF RAM_VERSION
	mov	bp, es	; BP is the TEMP which at all times points to
			; the next free area in the resized block!
	add	bp, 10h ; Initially point right past the PSP!
ENDIF

IFDEF RAM_VERSION
	; allocate a decent size STACK now!
	;
	mov	ah, 48h
	mov	bx, (MYSTACK_SIZE + 0fh)/16
	int	21h

	; Can be more graceful by putting up an out of mem message! But since
	; we are the command shell, we can't exit to anyone else. So, it
	; I guess it is OK to hang! BTW we don't expect to run out of mem!
	;
inf_loop:jc	inf_loop

ELSE

	; Note that we can save a few bytes for the ROM version if we used BP 
	; directly wherever we wanted instead of putting it in AX and 
	; then using it.
	;
	mov	ax, bp
	add	bp, MYSTACK_PARAS ; BP now points right past the STACK!
ENDIF

	cli
	mov	ss, ax
	mov	sp, MYSTACK_SIZE
	sti

IFDEF RAM_VERSION
	; allocate the memory needed for the DATA now!
	;
	mov	ah, 48h
	mov	bx, DATA_NUM_PARAS
	int	21h

	; Can be more graceful by putting up an out of mem message! But since
	; we are the command shell, we can't exit to anyone else. So, it
	; I guess it is OK to hang! BTW we don't expect to run out of mem!
	;
	jc	inf_loop
ELSE
	mov	ax, bp
	add	bp, DATA_NUM_PARAS ; BP now points right past the DATA!
ENDIF

	cld		; clear direction flag for all block move, etc.
			; Note that I assume that this will remain clear
			; during subsequent move/cmp, etc.

	; now block copy the DATA from ROM/Current area in RAM for the
	; RAM_VERSION into our new DSEG!
	;
	mov	es, ax		; ES now is the segment of allocated block!
	xor	di, di		; ES:DI points to beginning of this block.
				; This is our dynamically set up DSEG!
	mov	ax, cs
	add	ax, CODE_NUM_PARAS
	mov	ds, ax		; DS now points to end of code. This is the
				; same as the beginning of data!

	mov	si, di		; note that DI is still 0!

	mov	cx, (INITIALIZED_DATA_NUM_PARAS * 16)
	rep	movsb

	push	es
	pop	ds		; DS = ES now!

	assume es:DSEG,ds:DSEG

	; get and store our PSP. We use this later to decide during critical
	; errors whether we can "Abort" the process or not!
	;
	mov	ah, 51h
	int	21h
	mov	MyPSP, bx
	
IFDEF RAM_VERSION
	; allocate the environment space now!
	;
	mov	ah, 48h
	mov	bx, (ENV_SIZE + 0fh)/16
	int	21h

	; Can be more graceful by putting up an out of mem message! But since
	; we are the command shell, we can't exit to anyone else. So, it
	; I guess it is OK to hang! BTW we don't expect to run out of mem!
	;
	jc	inf_loop
ELSE
	mov	ax, bp		; BP is already pointing to ENV seg!
ENDIF

	mov	env_seg, ax	; Store the segment addr of this ENV block
				; for future use and for the EXEC call!

	; M002 Initialize the environment as being NULL! A 0 byte at the start
	; M002 of the ENV signifies this! However, to be compatible with
	; M002 command.com, put in a WORD 0!
	;
	push	es
	mov	es, ax
	assume	es:nothing
	mov	word ptr es:[0], 0	; M002
	pop	es
	assume	es:DSEG

	; now initialize some of the fields of the environment block's
	; that will be used for the EXEC call later. ES:BX will be pointing
	; to this block at that time.
	;
	mov	cmd_seg-2, offset DSEG:cmd_tail
	mov	cmd_seg, ds

	mov	fcb1_seg-2, offset DSEG:fcb1
	mov	fcb1_seg, ds

	mov	fcb2_seg-2, offset DSEG:fcb2
	mov	fcb2_seg, ds


	; Install Ctrl+C/Ctrl+Break handler.
	;
	push	ds
	push	cs
	pop	ds				; make DS = CS
	assume	ds:nothing
	mov	dx, offset CSEG:CtrlCHandler	; DS:DX = Int. handler addr.
	mov	ax, 2523h
	int	21h
	pop	ds
	assume	ds:DSEG

	; The critical error stub handler which is in DSEG will use this
	; FAR pointer to call this real critical error handler!
	;
	mov	word ptr CSCritErrHandlerAddr, offset CSEG:CritErrHandler
	mov	word ptr CSCritErrHandlerAddr+2, cs

	mov	word ptr DSExecHandlerAddr, offset DSEG:DSExechandler
	mov	word ptr DSExecHandlerAddr+2, es

	; Install Critical Error handler.
	; Note that the critical error handler stub is in the
	; data segment! We need this as that way we can get our DSEG
	; value and access variables like MyPSP, etc!
	;
	mov	dx, offset DSEG:CritErrorStub
	mov	ax, 2524h
	int	21h

	; Initialize the max. number of characters we read from stdin
	; when we are in interactive mode.
	;
	mov	pgm_blk, MAX_INP_LENGTH

	mov	last_pgm_exit_code, INIT_ERRLVL	; Init. last pgm exit code.
	mov	is_echo_on?, 1			; ECHO initially ON!

IFNDEF RAM_VERSION
	;
	; Check to see if the Boot Options indicate that startup processing
	; should be omitted.  If so, skip the open, and pretend that there
	; no INIT file.
	;
	push	es			; save current ES
	mov	ax,BDATA
	mov	es,ax
	assume	es:BDATA
	mov	ax,es:BootFlags		; get boot options from BIOS
	pop	es			; restore previous ES
	assume	es:DSEG
	test	al,BF_NoConfig		; flag set to supress processing?
	jnz	main_loop		; if so, skip autoexec processing
ENDIF

	call	process_initfile

main_loop:

	call	print_prompt		; that fn will take care of echo ON stuff!

	mov	dx, offset DSEG:pgm_blk	; DS:DX = buffer for user input
	mov	ah, 0ah
	int	21h			; get buffered input

	; We just read user input, so put out a CR-LF!
	call	print_CR_LF

	mov	si, offset DSEG:pgm_name

input_in_buffer:

	; DS:SI is the input from user - passed on to process_cmd!
	call	process_cmd

	jc	error_in_cmd

	; Actually we don't have to do the following check as we process
	; the full line of an IF cmd inside the if_proc itself using
	; recursion. It is there as a safety precaution.
	cmp	byte ptr [si-1], CR
	jne	input_in_buffer

	jmp	short	main_loop

error_in_cmd:

	; If we are here it means that it was not an intrinsic command
	; and also a program could not be launched succesfully!
	;
	mov	dx, offset DSEG:LOAD_MSG
	mov	cx, LOAD_MSG_LEN
	call	print_str_stderr

IFNDEF RAM_VERSION
	jmp	short main_loop
ELSE
	; This is just a method to get out of minicmd when using the RAM
	; version -- useful for debugging. A line with just q\r will
	; cause us to exit minicmd.
	;
	mov	si, offset DSEG:pgm_name
	lodsb
	cmp	al, 'q'
	jne	main_loop
	
	mov	ax, 4c01h
	int	21h
ENDIF

main_shell ENDP


;****************************************************************************
;*
;* ROUTINE:	process_initfile
;*
;* FUNCTION: 	Reads in commands from the INIT BAT file and processes them
;*		If the file is not present does nothing.
;*		
;*
;* INPUT:	DS, ES pointing to DSEG and a INIT file (optional).
;*		
;*
;* OUTPUT:	process INIT file and sends output to stdout.
;*
;* REGISTERS:	ALL except the segment registers destroyed (caller beware).
;****************************************************************************

process_initfile PROC NEAR

	assume	ds:DSEG
	push	ds

	mov	in_bat_file, 1	; mark that we are processing the BAT file!

	mov	ax, 3d00h	; OPEN file for read access!
	mov	dx, offset DSEG:INIT_BAT_NAME
	int	21h
	jnc	PIInitFileFound	; Short jump out of range!!!
	jmp	PINoInitFile

PIInitFileFound:
	mov	fhandle, ax	; store file handle for future use.

	; Seek to END of file to find out file size!
	mov	bx, ax
	mov	ax, 4202h
	xor	cx, cx
	mov	dx, cx
	int	21h

	jc	PIJumpErrInit

	or	dx, dx			; Is file size > 64K?
	jz	PIFileSizeUnder64K	; No! Too bad short jmp out of range!

PIJumpErrInit:
	jmp	PIErrorInitFIle	; file size >= 64K -- can't handle it :-(

PIFileSizeUnder64K:

	; store file size for future use.
	mov	fsize, ax

	; Seek to BEG of file - to start reading!
	mov	ax, 4200h	; BX still has file handle.
	mov	cx, dx		; Note that DX = 0 now as file size < 64K!
	int	21h		

	jc	PIJumpErrInit

	; If we are here, it means that we know the size of the INIT file
	; (It has been opened succesfully) and that it is < 64Kbytes!

	; Convert file size into num. of PARAs rounded up!
	mov	ax, fsize
	or	ax, ax
	jz	PIZeroSizeFile

	; assertion: DX = 0 now!
	mov	cx, 16
	div	cx
	or	dx, dx		; Is remainder 0?
	jz	PIExactNumParas ; Yes.
	
	inc	ax		; move number of PARAs up by 1 (round UP!)

PIExactNumParas:
	push	es
	push	ax		; save num paras reqd. for BAT file!


	; Now, try to allocate the necessary number of paragraphs to read in
	; the file (value in AX). Do this allocation at the top end of
	; memory, so that when we free this memory, no fragmentation of
	; occurs even if any TSRs have been launched from the INIT file.
	;
	; This is done in 3 steps:
	;   1) find out biggest free block available and allocate it.
	;   2) resize it so that we leave free a block needed for the INIT
	;	at the top end of memory.
	;   3) now allocate that free block we just created. I suppose it is
	;	possible to grab a different block if it matches this request!
	;   4) free the block we had re-sized!

	; Do step 1.
	;
	mov	bx, 0ffffh
	mov	ah, 48h
	int	21h

	; The above call should generate an error and BX will have the avail
	; amount of memory in PARAS!
	;
	mov	ah, 48h
	int	21h


	; Do step 2.
	;
	pop	cx		; get num paras reqd. for BAT file into CX
	sub	bx, cx

PIInfLoop:
	jbe	PIInfLoop	; M006 Make sure we have enough memory for
				; BAT file.

	dec	bx		; Subtract 1 for the arena header.
	mov	es, ax
	assume	es:nothing
	mov	ah, 4ah
	int	21h

	; Do step 3.
	;
	mov	bx, cx		; CX has number of PARAs to be allocated!
	mov	ah, 48h
	int	21h		; Note that this alloc cannot fail!!

	; Do step 4.
	;
	push	ax
	mov	ah, 49h
	int	21h		; ES stil has the re-sized block's segment!
	pop	ax
	pop	es
	assume	es:DSEG
	
	mov	cx, fsize
	mov	bx, fhandle

	; READ in whole file into the block we just allocated!
	;
	mov	ds, ax
	assume	ds:nothing
	mov	ah, 3fh
	xor	dx, dx
	int	21h		
	jc	PIErrorReading

	mov	si, es:fsize
	mov	al, [si-1]
	call	is_CR_LF?		; see if file has a CR/LF at EOF!
	je	PIProperlyTerminatedFile

	mov	byte ptr [si-1], CR	; replace last char with CR. This is
					; done to prevent us from overflowing
					; buffer when processing!

PIProperlyTerminatedFile:
	xor	si, si

	; DS:SI now points to beginning of BAT file in buffer!

PIBatFileLoop:
	cmp	si, es:fsize	; has whole file been processed?
	jge	PIBatFileDone	; Yes!

	call	echo_cmd	; echo cmd seen in INIT file if option ON!

	; process the command in buffer pointed to by DS:SI. Note that DS:SI
	; after this processing points to the next cmd in buffer.
	;
	call	process_cmd	

	jnc	PIBatFileLoop	; If no error, continue processing next cmd.

	; error in executing command in INIT file,print msg and continue!
	;
	mov	dx, offset DSEG:LOAD_MSG
	mov	cx, LOAD_MSG_LEN
	call	print_str_stderr

	jmp	short	PIBatFileLoop

PIBatFileDone:
PIErrorreading:
	; Free the buffer that was used for the BAT file
	mov	ah, 49h
	push	es
	push	ds
	pop	es
	assume	es:nothing
	int	21h		; Free block specified by ES (our buffer)

	pop	es
	assume	es:DSEG	

PIErrorInitFile:
PIZeroSizeFile:
	; CLOSE file
	;
	mov	ah, 3eh
	mov	bx, es:fhandle
	int	21h		

	; Note that we are ignoring errors on this call. It shouldn't matter.

PINoInitFile:
	pop	ds
	assume	ds:DSEG

	mov	in_bat_file, 0	; mark that we are done processing the BAT file!
	ret
process_initfile ENDP



	; Note that the following functions will be called
	; when inside process_initfile and at that time DS is not DSEG!
	; Hence the following 'assume'. However, if we implemented that
	; modular GET_LINE proc as suggested earlier, this will not
	; be needed.
	;
    	assume	ds:nothing

;****************************************************************************
;*
;* ROUTINE:	CtrlChandler
;*
;* FUNCTION: 	Does nothing but listens to Ctrl+C and Ctrl+Break and
;*		lets the current process continue un-interrupted.
;*
;* INPUT:	CARRY  clear.
;*
;* OUTPUT:	Nothing.
;*
;* REGISTERS:	None affected.
;****************************************************************************

CtrlCHandler PROC NEAR
	; We always let the program continue!
	; By default, at entry to this routine, CARRY is clear!
	;
	iret
CtrlCHandler ENDP



;****************************************************************************
;*
;* ROUTINE:	print_str_stderr
;*
;* FUNCTION: 	Prints a string to stderr.
;*
;* INPUT:	ES:DX = pointer points to string to be printed
;*		CX    = length of string to be printed
;*
;* OUTPUT:	Prints CR, LF, string, CR, LF to stderr
;*
;* REGISTERS:	AX, BX destroyed.
;****************************************************************************

print_str_stderr PROC NEAR

	call	print_CR_LF

	push	ds		; save DS

	push	es
	pop	ds
	assume	ds:DSEG		; make DS:DX point to string

	mov	ah, 40h		; AH = write to file
	mov	bx, STDERR	; BX = file handle
	int	21h

	pop	ds
	assume	ds:nothing	; for rest of the functions in file!

	call	print_CR_LF
	ret
print_str_stderr ENDP



;****************************************************************************
;*
;* ROUTINE:	print_char
;*
;* FUNCTION: 	Prints character to stdout. Can be called even from within
;*		the critical error handler.
;*		
;* INPUT:	DL = char to be printed.
;*
;* OUTPUT:	character printed to stdout.
;*
;* REGISTERS:	AX destroyed.
;****************************************************************************

print_char PROC NEAR
	mov	ah, 02h		; AH = print char to stdout
	int	21h
	ret
print_char ENDP



;****************************************************************************
;*
;* ROUTINE:	print_$str
;*
;* FUNCTION: 	Prints a string that is terminated by a '$'. Can be called
;*		even from within the critical error handler.
;*
;* INPUT:	DS:DX = pointer to '$' terminated string 
;*
;* OUTPUT:	string printed out to stdout. '$' is not printed!
;*
;* REGISTERS:	AX destroyed.
;****************************************************************************
	
print_$str PROC NEAR
	mov	ah, 09h
	int	21h
	ret
print_$str ENDP


;****************************************************************************
;*
;* ROUTINE:	print_crit_sep
;*
;* FUNCTION: 	Prints the critical error seperator string i.e., ", "
;*
;* INPUT:	CX = 0 implies don't print seperator, else print the string!
;*		DS = DSEG.
;*
;* OUTPUT:	string printed to stdout if CX not 0.
;*
;* REGISTERS:	AX,DX, destroyed.
;****************************************************************************

; Note that there could be some internationalization problems to the routine
; calling this function!
;
print_crit_sep PROC NEAR

	or	cx, cx		; Has a critical error option been printed?
	jz	PCSRet		; NO! Then don't print seperator!

	; Note that the crit error message should go to stderr
	; and not stdout as done here. But for minicmd.com there is not
	; redirection ('>' operator) hence not an issue!?
	;
	mov	dx, offset DSEG:CRIT_SEP_STR	 ; print string terminated by a '$'
	call	print_$str

PCSRet:
	ret
print_crit_sep ENDP



;****************************************************************************
;*
;* ROUTINE:	print_crit_err_msg
;*
;* FUNCTION: 	Prints out the appropriate critical error message based
;*		on whether it is a block device, character device, etc.
;*		The message is of the following form:
;*		"<CrMsg-N> [READ|WRIT]ING [DRIVE <drive-letter>:|DEVICE <device-name>"
;*		See dataseg.asm for CrMsg0 thru CrMag15!
;*
;* INPUT:	The same input that is passed by DOS on a critical error
;*		except that DS and ES = DSEG, Also in AH bit
;*		6 (ABORT_BIT_MASK) is zero-ed out!
;*		'CritErrAX' has the value of AX (with bit 6 zeroed as
;*		mentioned above) as passed to the Crit Err Handler.
;*
;* OUTPUT:	The appropriate crit error is printed out.
;*
;* REGISTERS:	ALL destroyed, except ES, DS, BP
;****************************************************************************

; Note that error 15 (Invalid disk change is not handled
;   thoroughly -- we don't specify the Vol #, Ser # of disk to put back!
;
; There is potential for sizzling this function.
;
; Note that the crit error message should go to stderr
;   and not stdout as done here! But in our case we assume
;   that both are the same as we don't support re-direction!!
;   Anyway, the necessary code to do this is in command2.asm in COMMAND dir!
;

print_crit_err_msg PROC NEAR

	assume	ds:DSEG, es:DSEG

	test	ah, 80h		; Is it a block device error?
	jz	PCEMBlkDevErr	; Yes!

	push	ds
	mov	ds, bp		; DS:SI now points to device header!
	assume	ds:nothing

	mov	ax, ds:[si+OFFSET_DEV_ATTR] ; get dev attr word in AX
	pop	ds			    ; Bit 15 in this word (Bit 7)	
	assume	ds:DSEG			    ; in the high word if set to 1
	test	ah, 80h			    ; it is a character device
	jz	PCEMFatErr		    ; else it is a FAT image in
					    ; memory corrupted error.

PCEMBlkDevErr:
	mov	bx, di		; BL now has the Driver Error code.

	cmp	bl, MAX_DRIVER_ERR_CODE	; Is error code < max # we can handle?
	jle	PCEMValidCode		; Yes, don't have to ask IFS!

	; The error code is greater than the numbers we can handle 
	; without calling IFS, extended error information, etc. 
	; Change it into a code for the general critical error message.
	; 
	mov	bl, UNKNOWN_MSG_ERR_CODE
					

	; All the CritMsgs are '$" terminated and BL is now a value from
	; 0 thru MAX_DRIVER_ERR_CODE (15 now). Get to message N (in BL)
	; by looking for '$' to find the beginning of the next message!
	;
PCEMValidCode:
	mov	di, offset DSEG:FIRST_CRIT_ERR_MSG

PCEMGetErrMsg:
	or	bl, bl		; Have we got start of message N?
	jz	PCEMGotMsg	; Yes!

	mov	al, '$'		; No, scan for '$', dec BL and check BL again!
	mov	cx, 0FFFFh
	repne	scasb

	dec	bl
	jmp	PCEMGetErrMsg

PCEMGotMsg:
	
	call	print_CR_LF

	mov	dx, di		
	;
	; DS:DX now points to the crit err msg - print it!
	;
	call	print_$str

	mov	dl, SPACE
	call	print_char

	; print the string READING or WRITING now. Bit 8 (or Bit 0 in the
	; high byte) of CritErrAX specifies this.
	;
	mov	dx, offset DSEG:READING_STR		; set DS:DX to READING str!

	test	byte ptr CritErrAX+1, 01h	; Is it a reading op?
	jz	PCEMReadingOp			; Yes!

	mov	dx, offset DSEG:WRITING_STR		; No, set DS:DX to WRITING str!

PCEMReadingOp:
	call	print_$str		; print word READING or WRITING.

	mov	dl, SPACE
	call	print_char

	; So far we have printed "<error msg> <READ|WRIT>ING "
	; Now print the phrase "DRIVE <drive-letter>:" for block device
	; or the phrase	       "DEVICE <device-name> for character device
	; Bit 15 (or bit 7 in high byte) of CritErrAX specifies this.
	;
	mov	dx, offset DSEG:DRIVE_STR	  ; point DS:DX to DRIVE str

	test	byte ptr CritErrAX+1, 80h ; Is it a BLOCK device?
	jz	PCEMDriveStr		  ; Yes.

	mov	dx, offset DSEG:DEVICE_STR	  ; No, point DS:DX to DEVICE str

PCEMDriveStr:
	call	print_$str		; print word DRIVE or DEVICE

	mov	dl, SPACE
	call	print_char

	; So far we have printed "<error msg> <READ|WRIT>ING <DRIVE|DEVICE> "


	; Now print <drive-letter>: for BLOCK devices
	; or	    <device-name> for character devices.
	;
	test	byte ptr CritErrAX+1, 80h ; This is tested too many times!
					  ; scope for optimization.
	jnz	PCEMPrintDeviceName

	mov	dl, byte ptr CritErrAX	; Get the drive letter index. 0 = A,
	add	dl, 'A'			; 1 = B, etc. convert to character
	call	print_char		; and print.

	mov	dl, COLON
	call	print_char
	jmp	short PCEMDone

PCEMPrintDeviceName:
	push	ds
	mov	ds, bp		; DS:SI points to device header
	assume	ds:nothing

	; We know it is a character device. Print device-name by looking at
	; the device header.
	;
	mov	cx, DEV_NAME_LEN
	add	si, OFFSET_DEV_NAME

PCEMDevNameLoop:
	lodsb
	mov	dl, al
	call	print_char

	loop	PCEMDevNameLoop

	pop	ds
	assume	ds:DSEG

	jmp	short PCEMDone

PCEMFatErr:
	; FAT error is a special critical error message -- a very rare
	; occurence!
	;
	mov	dx, offset DSEG:FAT_ERR_MSG
	call	print_$str

PCEMDone:
	call	print_CR_LF
	
	ret

print_crit_err_msg ENDP


;****************************************************************************
;*
;* ROUTINE:	CritErrHandler
;*
;* FUNCTION: 	Prints the appr. critical error message and also prompts
;*		the user for appropriate action (Abort, Retry, Ignore,
;*		Fail options).
;*
;* INPUT:	DS = DSEG and rest of registers as passed in by DOS
;*		when critical error happens. BP:SI, AX, DI have input!
;*
;* OUTPUT:	Handles critical error: Prints out critical error message,
;*		gives user choices, reads in user input, validates it
;*		and passes back appropriate code in AL.
;*
;* REGISTERS:	AX affected! AL has return value to DOS.
;****************************************************************************

CritErrHandler PROC FAR
	assume	ds:DSEG

	push	bx
	push	cx
	push	dx
	push	di
	push	si
	push	ds
	push	es

	push	ds
	pop	es
	assume	es:DSEG

	and	ah, NOT ABORT_BIT_MASK	; set the ABORT_BIT_MASK to zero!
	mov	CritErrAX, AX	; store the AX register, has bit fields
				; that indicate valid options, etc!

	call	print_crit_err_msg

CEHPrintCritErr:
	mov	ah, 51h		; AH = Get PSP.
	int	21h

	; ABORT is not allowed if MyPSP is same as the person who caused
	; the critical error. 
	; Note that we need to do this only if it is the top level process.
	; For simplicity sake, it was not done.

	cmp	bx, MyPSP
	je	@F

	or	byte ptr CritErrAX+1, ABORT_BIT_MASK
@@:

	; Note that there is scope to sizzle following code.

	xor	cx, cx		; CX used as temp var indicating whether
				; we need to print CRIT_SEP_STR or not!

	mov	di, offset DSEG:CritErrInpKeys
	mov	si, offset DSEG:CritErrKeyCodes
	push	di		; save for future scasb!

	; depending on which options are available (Abort, Retry,
	; Ignore and Fail) print the choices to the user in the
	; form of a question.
	;
	test	byte ptr CritErrAX+1, ABORT_BIT_MASK
	jz	CEHNoAbort

	mov	dx, offset DSEG:ABORT_STR
	call	print_$str

	mov	al, ABORT_KEY
	stosb
	mov	al, ABORT_KEY_CODE
	mov	[si], al
	inc	si
	inc	cx

CEHNoABort:
	test	byte ptr CritErrAX+1, RETRY_BIT_MASK
	jz	CEHNoRetry

	call	print_crit_sep

	mov	dx, offset DSEG:RETRY_STR
	call	print_$str

	mov	al, RETRY_KEY
	stosb
	mov	al, RETRY_KEY_CODE
	mov	[si], al
	inc	si
	inc	cx

CEHNoRetry:
	test	byte ptr CritErrAX+1, IGNORE_BIT_MASK
	jz	CEHNoIgnore

	call	print_crit_sep

	mov	dx, offset DSEG:IGNORE_STR
	call	print_$str

	mov	al, IGNORE_KEY
	stosb
	mov	al, IGNORE_KEY_CODE
	mov	[si], al
	inc	si
	inc	cx

CEHNoIgnore:
	test	byte ptr CritErrAX+1, FAIL_BIT_MASK
	jz	CEHNoFail

	call	print_crit_sep

	mov	dx, offset DSEG:FAIL_STR
	call	print_$str

	mov	al, FAIL_KEY
	stosb
	mov	al, FAIL_KEY_CODE
	mov	[si], al
	inc	si
	inc	cx

CEHNoFail:

	; Now print the question marker, as the message has been printed!
	mov	dx, offset DSEG:CRIT_Q_STR
	call	print_$str

	; The critical error message has been completely printed out.
	; Now flush stdin and then read 1 character!
	mov	ax, 0c01h
	int	21h		

	push	ax		; save AL the char user typed in!
	call	print_CR_LF
	pop	ax		; get back AL (user typed in character)

	call	upper_case

	pop	di	; ES:DI now points to first valid crit err char.
			; CX has number of valid user input keys!

	repne	scasb		; check if user entered a valid key

	je	CEHValidCharGot	; Too bad short jmp out of range!
	jmp	CEHPrintCritErr	; prompt again for bad response!

CEHValidCharGot:
	; Set AL = action code for MS-DOS according to key entered:
	; 0 = Ignore, 1 = Retry, 2 = Abort, 3 = Fail
	mov	al, [di+NUM_CRIT_KEYS-1]

CEHPopAndRet:
	pop	es
	pop	ds
	pop	si
	pop	di
	pop	dx
	pop	cx
	pop	bx

	assume	ds:nothing	; For rest of the routines in this file!
	ret			; return to STUB in data seg!
CritErrHandler ENDP



;****************************************************************************
;*
;* ROUTINE:	process_cmd
;*
;* FUNCTION: 	processes commands - both intrinsic and extrinsic
;*
;* INPUT:	DS:SI points to command. There could be leading white
;*		space (TABS, SPACE). The command should be terminated
;*		either by CR or LF.
;*
;* OUTPUT:	processes the command and on completion DS:SI points to
;*		the next command - after the CR or LF!
;*
;* REGISTERS:	ALL destroyed.
;****************************************************************************

process_cmd PROC NEAR

	mov	cx, si			; store for future reference

	call	intrinsic		; check for intrinsic command
	jnc	PCRet			; Yes, handled intrinsic cmd!

	; num_readin is not initialized if we are processing
	; BAT file. Initialize it now as it is referenced later.
	; BTW, if we had read in stdin, we un-necessarily re-compute it
	; here. No big deal as speed is not an issue. It is fast anyway!
	push	si
	mov	bl, CR
	call	scan_for_char
	mov	dx, si
	sub	dx, cx
	dec	dx			; don't include CR in count!
	mov	es:num_readin, dl	; Note that we assume line len < 256!
	pop	si

	; If we are here it was not an intrinsic cmd -- Launch it!!

	call	skip_white_space	; skip leading blanks before command.

	call	is_CR_LF?		; Empty command?
	jne	PCNotEmptyCmd		; No!

	inc	si			; DS:SI points to next cmd (after this CR)!
	clc				; Empty cmd succesfully executed :-)\

PCRet:
	ret

PCNotEmptyCmd:
	mov	dx, si			; DS:DX now points to command name.
					; This is requied by EXEC call below!
	call	get_to_white_space

	push	ax			; save character (SPACE, TAB, CR or LF)
					; for future use.

	mov	bx, si			; store SI for future reference. This
					; is the end of the command string!

	mov	ax, 2901h		; ParseFileName into FCB function
					; skipping leading separators.
	mov	di, offset DSEG:fcb1		; ES:DI = FCB to parse into
					; DS:SI already points to source.
	int	21h

	; Now DS:SI is ready for the next file name to be parsed!
	mov	al, 01			; AH still has 29H!?, AL = 1 implies
					; skip leading seperators.
	mov	di, offset DSEG:fcb2		; ES:DI = FCB to parse into
	int	21h

	sub	cx, bx			; Note that CL is as good as CX now
					; as the value in cx >= -128
	add	cl, es:num_readin
	mov	es:cmd_tail, cl		; init cmd_tail length.
	
	xor	ch, ch
	inc	cx			; to include CR also in count to copy

	mov	si, bx
	mov	di, offset DSEG:cmd_tail + 1
	rep	movsb			; copy cmd_tail from buffer.

	mov	BYTE PTR [bx], 0	; Make cmd string NULL terminated!
					; the character at this pos. is on
					; the stack (when we did the PUSH AX)
	push	bx			; save offset for future!
	push	dx

	call	dword ptr es:[DSExecHandlerAddr] ; launch program in DS:SI

	pop	si			; Get DX into SI, so that DS:SI
					; now has start of cmd!
	pop	bx
	pop	ax	
	mov	BYTE PTR [bx], al	; Put back original character!

	pushf				; Save CARRY flag from EXEC call!

	jc	PCDone

	; Note that our code is quite general and we can now check if it
	; is a BAT file and if so process it here! The only problem is
	; is that we can't process NESTED BAT files!
	;

	; program was succesfully executed and it terminated!
	mov	ah, 4dh
	int	21h			; Get return code of child process.
	mov	byte ptr es:last_pgm_exit_code, al ; store for future use in IF stmts!

PCDone:
	mov	bl, CR
	call	scan_for_char		; make DS:SI just past end of Cmd!

	popf				; Restore CARRY flag for caller!

	; DS:SI set up for next command in buffer!
	ret
process_cmd ENDP



;****************************************************************************
;*
;* ROUTINE:	is_CR_LF?
;*
;* FUNCTION: 	checks to see if the character in AL is a CR or LF
;*
;* INPUT:	AL has character to be tested
;*
;* OUTPUT:	ZERO flag set if char is CR or LF else it is cleared.
;*
;* REGISTERS:	NONE destroyed, FLAGS modified.
;****************************************************************************

is_CR_LF? PROC	NEAR
	cmp	al, CR
	je	ICLEnd
	
	cmp	al, LF
ICLEnd:
	ret	
is_CR_LF? ENDP



;****************************************************************************
;*
;* ROUTINE:	is_SP_TAB?
;*
;* FUNCTION: 	checks to see if the character is a SPACE/TAB (white space)
;*
;* INPUT:	AL has character to be tested
;*
;* OUTPUT:	ZERO flag set if char is SPACE or TAb, else it is cleared.
;*
;* REGISTERS:	NONE destroyed, FLAGS modified.
;****************************************************************************

is_SP_TAB? PROC	NEAR
	cmp	al, SPACE
	je	ISTEnd
	
	cmp	al, TAB
ISTEnd:
	ret	
is_SP_TAB? ENDP



;****************************************************************************
;*
;* ROUTINE:	get_to_white_space
;*
;* FUNCTION: 	Seeks to the first SPACE, TAB, CR or LF in the buffer. If the
;*		first character in the buffer is such a character, the pointer
;*		is not even advanced.
;*
;* INPUT:	DS:SI = buffer that has a line terminated by CR or LF.
;*
;* OUTPUT:	DS:SI points to character in buffer that is 1 of the 4 above.
;*		AL = 1 of the 4 characters mentioned above.
;*
;* REGISTERS:	NONE affected but the result registers.
;****************************************************************************

get_to_white_space PROC NEAR
	lodsb

	call	is_SP_TAB?
	je	gtws_ws_found

	call	is_CR_LF?
	jne	get_to_white_space

gtws_cr_found:
gtws_ws_found:
	dec	si
	ret
get_to_white_space ENDP


;****************************************************************************
;*
;* ROUTINE:	skip_white_space
;*
;* FUNCTION: 	Seeks past SPACE, TAB. If the first character in the 
;*		buffer is such a character, the pointer is not even advanced.
;*
;* INPUT:	DS:SI = buffer that has a line terminated by CR or LF.
;*
;* OUTPUT:	DS:SI points to character in buffer that is not SPACE or TAB.
;*		AL = 1st non-white space character (could be CR or LF also).
;*
;* REGISTERS:	NONE affected but the result registers.
;****************************************************************************

skip_white_space PROC NEAR
	lodsb

	call	is_SP_TAB?
	je	skip_white_space

	dec	si
	ret
skip_white_space	ENDP


;****************************************************************************
;*
;* ROUTINE:	upper_case
;*
;* FUNCTION: 	converts characters in range 'a' thru 'z' to 'A' thru 'Z'
;*
;* INPUT:	AL has character to be upper-cased!
;*
;* OUTPUT:	AL has the upper-cased character if it was 'a' thru 'z' else
;*		it is left un-changed!
;*
;* REGISTERS:	NONE but the result register (AL).
;****************************************************************************

upper_case PROC NEAR
	cmp	al, 'a'
	jb	UCDone
	cmp	al, 'z'
	ja	UCDone
	sub	al, 'a' - 'A'
UCDone:
	ret
upper_case ENDP


	; Note that it might be possible to write a general function
	; to combine the functionality of functions like:
	; print_str_stderr, print_$str, echo_line, etc and save space!

;****************************************************************************
;*
;* ROUTINE:	echo_line
;*
;* FUNCTION: 	prints a string terminated by CR or LF to stdout.
;*
;* INPUT:	DS:SI points to string to be printed.
;*
;* OUTPUT:	string is printed to stdout.
;*		DS:SI points to character right after the first CR, LF in
;*		buffer.
;*
;* REGISTERS:	AX destroyed. SI changed.
;****************************************************************************

echo_line PROC NEAR
	push	dx

echo_loop:
	mov	dl, [si]
	call	print_char
	
	inc	si

	call	is_CR_LF?
	jne	echo_loop

	; Note that CR is echoed as CR-LF, but LF is also echoed as LF-LF!
	; AH is still 02!
	mov	dl, LF
	int	21h

	pop	dx
	ret
echo_line ENDP


;****************************************************************************
;*
;* ROUTINE:	echo_cmd
;*
;* FUNCTION: 	If echo turned ON:
;*		Echoes to stdout the prompt followed by
;*		   command pointed to by DS:SI
;*		It doesn't echo labels or empty commands (commands
;*		   with CR or LF at the beginning).
;*		The commands could have leading white space.
;*		
;*
;* INPUT:	DS:SI points to command to be echo-ed.
;*		ES = DSEG.
;*		is_echo_on? in DSEG specifies whether to echo or not.
;*
;* OUTPUT:	string echo-ed to stdout.
;*
;* REGISTERS:	AX destroyed.
;****************************************************************************

echo_cmd PROC NEAR
	cmp	es:is_echo_on?, 1
	jne	ECRet

	push	si

	call	skip_white_space

	call	is_CR_LF?
	je	ECPopAndRet	; Don't echo empty commands!

	cmp	al, COLON
	je	ECPopAndRet	; Don't echo LABELs

	pop	si
	push	si

	call	print_prompt
	call	echo_line

ECPopAndRet:
	pop	si

ECRet:
	ret
echo_cmd ENDP


;****************************************************************************
;*
;* ROUTINE:	print_prompt
;*
;* FUNCTION: 	If echo turned ON:
;*		Prints the prompt string to stdout (right now, the COLON!)
;*
;* INPUT:	ES = DSEG
;*		is_echo_on? in DSEG specifies whether to echo or not.
;*
;* OUTPUT:	prompt printed to stdout.
;*
;* REGISTERS:	AX destroyed.
;****************************************************************************

print_prompt PROC NEAR
	cmp	es:is_echo_on?, 1
	jne	PPRet
	
	mov	dl, PROMPT_CHAR
	call	print_char

PPRet:
	ret
print_prompt ENDP


;****************************************************************************
;*
;* ROUTINE:	print_CR_LF
;*
;* FUNCTION: 	Prints to stdout CR followed by LF.
;*
;* INPUT:	NONE.
;*
;* OUTPUT:	see functionality.
;*
;* REGISTERS:	AX destroyed.
;****************************************************************************

print_CR_LF PROC NEAR

	push	dx		; save DX

	mov	dl, CR
	call	print_char

	mov	dl, LF
	call	print_char

	pop	dx		; restore DX

	ret

print_CR_LF ENDP


;****************************************************************************
;*
;* ROUTINE:	print_str
;*
;* FUNCTION: 	prints a string to stdout, also echoes a CR-LF after it.
;*
;* INPUT:	ES = DSEG.
;*		ES:DX points to string to be printed.
;*
;* OUTPUT:	see functionality.
;*
;* REGISTERS:	AX, BX destroyed.
;****************************************************************************

print_str PROC NEAR
	push	ds

	push	es
	pop	ds
	assume	ds:DSEG

	mov	bx, STDOUT
	mov	ah, 40h
	int	21h		; print str

	call	print_CR_LF

	pop	ds
	assume	ds:nothing

	ret
print_str ENDP



;****************************************************************************
;*
;* ROUTINE:	scan_for_char
;*
;* FUNCTION: 	scans for a character in a buffer that is terminated by
;*		a CR or LF.
;*
;* INPUT:	DS:SI = pointer to buffer terminated by CR or LF.
;*		BL = character to be scanned for in buffer.
;*
;* OUTPUT:	DS:SI points to the character after the found character.
;*		AL = character that caused termination of the func.
;*		   If (AL != BL) character was not found and DS:SI points
;*		      immediately after the CR or LF at the end of the line.
;*		      In this case AL = CR or LF.
;*
;* REGISTERS:	NONE except the output registers.
;****************************************************************************

scan_for_char PROC NEAR
	lodsb

	call	is_CR_LF?
	je	SFCNotFound

	cmp	al, bl
	jne	scan_for_char

SFCNotFound:
	ret	
scan_for_char ENDP


;****************************************************************************
;*
;* ROUTINE:	strcpy_till
;*
;* FUNCTION: 	copies from source to destination the characters in a buffer
;*		until a certain character is found.
;*
;* INPUT:	DS:SI = source buffer that has the character specified in
;*		        it or is terminated by CR or LF.
;*		BL    = char to copy till (including it too!).
;*		ES:DI = destination buffer.
;*
;* OUTPUT:	DS:SI points to character after the last one copied in source.
;*		ES:DI points to character after the last one copied in dest.
;*
;* REGISTERS:	None but DI and SI.
;****************************************************************************

strcpy_till PROC NEAR
	lodsb
	stosb
	cmp	al, bl
	je	strcpy_done

	call	is_CR_LF?
	jne	strcpy_till

strcpy_done:
	ret
strcpy_till ENDP


;****************************************************************************
;*
;* ROUTINE:	strcpyupper_till
;*
;* FUNCTION: 	Does the same as strcpy_till except that the alphabet ('a'
;*		thru 'z') are upper cased in destination.
;*
;* INPUT:	see fn strcpy_till -- same as that.
;*		
;* OUTPUT:	see functionality.
;*
;* REGISTERS:	None but DI and SI.
;****************************************************************************

strcpyupper_till PROC NEAR
	lodsb
	call	upper_case	
	stosb
	cmp	al, bl
	je	strcpyupper_done

	call	is_CR_LF?
	jne	strcpyupper_till

strcpyupper_done:
	ret
strcpyupper_till ENDP


;****************************************************************************
;*
;* ROUTINE:	atoi
;*
;* FUNCTION: 	converts an un-signed numerical string to a number
;*
;* INPUT:	DS:SI = points to 1st character in un-signed numerical str.
;*		Note that any character other than '0' thru '9' stops the
;*		   processing and value till that point is used.
;*
;* OUTPUT:	DS:SI points to first non numeric character from start.
;*		AX has numerical value of string.
;*
;* REGISTERS:	BX destroyed.
;****************************************************************************

atoi PROC NEAR
	xor	ax, ax
	mov	bh, ah		; same as mov bh, 0 but faster!
	mov	cl, 10

atoiLoop:
	mov	bl, [si]
	sub	bl, '0'

	jb	atoiDone

	cmp	bl, 9
	ja	atoiDone

	mul	cl
	add	ax, bx
	inc	si

	jmp	short atoiLoop

atoiDone:
	ret
atoi ENDP


;****************************************************************************
;*
;* ROUTINE:	check_for_kw
;*
;* FUNCTION: 	checks to see if a certain string is present in a buffer.
;*		It basically does a strncmpi() but if match succesfull the
;*		the next character in buffer should be a SPACE, TAB, CR
;*		or LF as these are the characters that are known to be
;*		delimiters!
;*
;* INPUT:	DS:SI points to location in buffer where the string is
;*		      expected to be present. buffer is expected to
;*		      to be terminated by CR or LF.
;*		ES:DI points to the keyword/string to be searched for.
;*		CX has length of keyword/string.
;*
;* OUTPUT:	If match found, DS:SI points to the next word after this
;*		string, or to CR/LF if no word present after this string.
;*		If match not found, DS:SI is left un-changed.
;*
;*		ZERO flag set is string/kw found, else clear.
;*
;* REGISTERS:	AX, CX, DI destroyed. SI appropriately changed.
;****************************************************************************

check_for_kw PROC NEAR
	push	dx
	push	si

CFKLoop:
	; The following few instr. until jnz CFKLoop is the REPE CMPSB
	; instruction but we need to do a case insensitive compare!
	; Note that both source and dest can be in upper/lower case!
	; especially when called from the goto_proc!
	; Note that we could have a fn that first converts input
	; to upper case and then uses the REPE instr! We can't do an
	; upper case translation in-place in the buffer because the F3
	; key etc won't echo back the same chars to user!

	lodsb
	call	upper_case		; get char from buffer
	mov	dl, al

	mov	al, es:[di]		; get char from keyword/string
	call	upper_case

	cmp	al, dl			; compare the two (case in-sensitive)
	jne	CFKNoMatch		; No match, sorry!

	inc	di
	dec	cx
	or	cx, cx			; have we compared upto strlen?
	jnz	CFKLoop			; No, check next chars

	; The string was found in the buffer, see if the next char is a
	; delimiter -- SPACE, TAB, CR, or LF.
	; This way we are sure that we don't mistake a string like
	; SETTLE in buffer to be the SET keyword!
	;
	lodsb
	call	is_SP_TAB?
	je	CFKMatched

	call	is_CR_LF?
	jne	CFKNoMatch

	dec	si		; bring DS:SI back to point to the CR or LF!

CFKMatched:
	pop	ax		; dummy pop SP <- SP + 2
	call	skip_white_space;point to the next keyword if present or EOL!
	push	si		; for the POP which will be done later anyway!
	xor	ax, ax		; set ZERO flag set to return to caller.

CFKNoMatch:
	pop	si
	pop	dx
	ret			; Note that ZERO flag is passed back correctly.

check_for_kw ENDP



;****************************************************************************
;*
;* ROUTINE:	intrinsic
;*
;* FUNCTION: 	Checks to see if the (DS:SI) buffer contains an intrinsic
;*		command like SET, ECHO, IF, GOTO or label (':') and if so
;*		processes it. Note that there could be leading white space
;*		and that the line is expected to be terminated by a CR or LF.
;*
;* INPUT:	DS:SI = buffer that has a command (possibly intrinsic)
;*
;* OUTPUT:	If it is an intrinsic command it is processed/executed
;*		and DS:SI points after the CR or LF that ends this cmd
;*		and CARRY is clear.
;*
;*		If not intrinsic, DS:SI is left un-changed and CARRY set.
;*
;* REGISTERS:	All except CX, segment registers destroyed.
;****************************************************************************

	; Note that one can use the generalized form of this matcher but that is
	; takes a few bytes more including the data it needs!

intrinsic PROC NEAR
	push	si
	push	cx

	call	skip_white_space	; skip leading white space

	; Is it a LABEL command?
	;
	cmp	byte ptr [si], COLON
	jne	IntrNotLabel

	call	label_proc
	jmp	short IntrSuccess

IntrNotLabel:

	; Is it a SET command?
	;
	mov	di, offset DSEG:SET_STR
	mov	cx, SET_STR_LEN
	call	check_for_kw
	jne	IntrNotSETCmd

	call	set_proc
	jmp	short IntrSuccess

IntrnotSETCmd:
	; Is it an ECHO command?
	;
	mov	di, offset DSEG:ECHO_STR
	mov	cx, ECHO_STR_LEN
	call	check_for_kw
	jne	IntrNotECHOCmd
	
	call	echo_proc
	jmp	short IntrSuccess

IntrNotECHOCmd:
	; Is it an IF command?
	;
	mov	di, offset DSEG:IF_STR
	mov	cx, IF_STR_LEN
	call	check_for_kw
	jne	IntrNotIFCmd
	
	call	if_proc
	jmp	short IntrSuccess

IntrNotIFCmd:
	; Is it a GOTO command?
	;
	mov	di, offset DSEG:GOTO_STR
	mov	cx, GOTO_STR_LEN
	call	check_for_kw
	jne	IntrFailure
	
	call	goto_proc

IntrSuccess:
	clc		; intrinsic cmd processed, clear CARRY for caller.
	pop	cx	
	pop	ax	; clean up stack (get SI into AX), leave SI unchanged!

	; DS:SI should now be pointing correctly past EOL for next cmd!
	ret

IntrFailure:
	stc		; not intrinsic cmd, set CARRY for caller.

IntrPopRegs:
	pop	cx
	pop	si
	ret			
intrinsic ENDP


; Note that in current command.com type ECHO.BAT and have a batch file
; by the name ECHO.BAT, it doen't launch the BAT file!!!

; Note that this fn does not print the white space between ECHO and the string
; as that is not passed in! It has already been stripped! Is this a problem?
;

;****************************************************************************
;*
;* ROUTINE:	echo_proc
;*
;* FUNCTION: 	This routine handles the "ECHO" intrinsic cmd. It handles
;*		ECHO [ON | OFF] and ECHO <string>. It checks if echo-ing
;*		is turned on or not before printing the string to stdout.
;*
;* INPUT:	DS:SI points to the string to be echo-ed or to the keywords
;*		    ON or OFF (there is no leading white space).
;*		The line is expected to be terminated by a CR or LF
;*
;* OUTPUT:	see functionality. DS:SI points after the CR or LF at end.
;*
;* REGISTERS:	ALL general purpose registers destroyed.
;****************************************************************************

echo_proc PROC NEAR

	; DS:SI points to the string to be echo-ed!
	push	si		; Save in case we need to backtrack
				; example: "ECHO ON schedule" should print
				; out "ON schedule" and not turn echo-ing ON!!

	mov	di, offset DSEG:ON_STR
	mov	cx, ON_STR_LEN
	call	check_for_kw
	je	EPONFound

	mov	di, offset DSEG:OFF_STR
	mov	cx, OFF_STR_LEN
	call	check_for_kw
	jne	EPCheckForPlainECHO

	xor	ah, ah			; ECHO OFF value!

	; Note that check_for_kw has already skipped the white space after
	; the OFF keyword!

EPCheckForBackTrack:
	mov	al, [si]
	call	is_CR_LF?
	jne	EPEchoStr

	mov	es:is_echo_on?, ah	; mark echo turned OFF!

EPPopAndRet:
	inc	si			; skip past the EOL char!
	pop	ax			; get rid of SI from stack!
	ret

EPONfound:
	; Note that check_for_kw has already skipped the white space after
	; the ON keyword!
	mov	ah, 1			; ECHO ON value!
	jmp	short	EPCheckForBackTrack

EPCheckForPlainECHO:
	mov	al, [si]
	call	is_CR_LF?
	jne	EPEchoStr

	cmp	es:is_echo_on?, 1
	jne	EPPrintEchoIsOff

	mov	dx, offset DSEG:ECHOISON_STR
	mov	cx, ECHOISON_STR_LEN

EPPrintStr:
	; print ECHO is on/off.
	;
	call	print_str
	jmp	short EPPopAndRet

EPPrintEchoIsOff:
	mov	dx, offset DSEG:ECHOISOFF_STR
	mov	cx, ECHOISOFF_STR_LEN

	jmp	short EPPrintStr
	
EPEchoStr:
	pop	si
	call	echo_line

	; NOTE: DS:SI now points just after the CR or LF in buffer!
	ret

echo_proc ENDP



;****************************************************************************
;*
;* ROUTINE:	get_env
;*
;* FUNCTION: 	Checks to see if a certain NAME has a value associated
;*		with it in the environment. It does a case insensitive
;*		check knowing that the ENV has NAMEs stored in upper case!
;*
;* INPUT:	ES:0  = pointer to environment we created.
;*		DS:SI = pointer to NULL terminated string NAME=
;*
;* OUTPUT:	CARRY clear, if variable found and ES:DI points to the 
;*		    start of it (NAME) in the ENV, i.e, to NAME=STRING
;*		CARRY set, if variable not found and ES:DI points to end of
;*		    ENV (the very last 0 byte).
;*
;* REGISTERS:	AX destroyed.
;****************************************************************************

get_env PROC NEAR
	assume	es:nothing

	push	cx
	push	bx

	xor	di, di			; initialize ENV offset.

GEMainLoop:
	mov	bx, si
	mov	cx, di			; store DI in CX for returning if
					; match found. ES:DI now points to
					; NAME=STRING in ENV!

	cmp	byte ptr es:[di], 0	; end of environment?
	je	GENotInENV

GEMatchName:
	mov	al, [bx]
	or	al, al			; end of NAME?
	jz	GEMatchFound

	call	upper_case		; it is stored in ENV in upper case!
	cmp	al, es:[di]		; compare to environment char
	jne	GEMatchFailed

	inc	bx
	inc	di
	jmp	short GEMatchName

GEMatchFound:
	mov	di, cx
	pop	bx
	pop	cx
	ret				; Note that CARRY is clear now!

GEMatchFailed:
	xor	al, al			; scan forward in ENV
	mov	cx, -1			; for 0 byte
	repnz	scasb

	jmp	short GEMainLoop

GENotInENV:
	pop	bx
	pop	cx
	stc				; NAME not in ENV -- set CARRY!
	ret

get_env ENDP



;****************************************************************************
;*
;* ROUTINE:	set_proc
;*
;* FUNCTION: 	This routine handles the "SET" intrinsic command. It handles
;*		the plain vanilla command "set NAME=STRING". It does not
;*		do any de-referencing like %NAME%, etc!
;*
;* INPUT:	DS:SI = pointer to the beginning of NAME in the line
;*						set NAME=STRING
;*			i.e., DS:SI now points to   ^
;*		The line is expected to be terminated by a CR or LF
;*
;* OUTPUT:	see functionality. DS:SI points after the CR or LF at end.
;*
;* REGISTERS:	AX, BX, DI destroyed.
;****************************************************************************

set_proc PROC NEAR

	assume	es:DSEG
	push	es	; ES is DSEG, save it so that we can restore on exit!
	mov	es, es:env_seg
	assume	es:nothing

	push	si		
	mov	bl, EQUALS
	call	scan_for_char	; look for '=' in this buffer (DS:SI)
				; user input:		set NAME=STRING
	mov	bx, si		; DS:BX now points to		 ^

	call	is_CR_LF?	; did we not find an EQUALS sign?
	je	SPNoEquals	; Yes, no '=' sign in user input!

	pop	si		; DS:SI is back where it was (beginning)!

	mov	cl, [bx]	; store the character after the '=' sign!
	mov	byte ptr [bx], 0; and replace it with a NULL character.

	call	get_env
	mov	byte ptr [bx], cl; put back the character in buffer!

	jnc	SPFoundEnvString ; the variable NAME has a value already set!

SPCopyThisStringToEnv:
	mov	al, cl
	call	is_CR_LF?	; Is the value for variable (STRING) empty?
				; i.e., is it CR? CL has char after the '='
	je	SPDontAddStr	; Yes, don't have to add it after all!

	; ES:DI is now at the end of ENV -- the 0 terminating byte!

	; Note that we currently do not check to see if we overflow
	; the statically allocated ENV buffer. It may be done here!
	mov	bl, EQUALS
	call	strcpyupper_till

	; copy till EOL (CR or LF)!
	mov	bl, CR
	call	strcpy_till
	dec	di		; make ES:DI point to the CR character
	xor	al, al		; Replace this CR with NULL!
	stosb

SPDone:
	mov	byte ptr es:[di], 0 ; put in ENV terminating NULL byte!

	cmp	byte ptr es:[0], 0  ; M002 Is it an empty environment?
	jne	SPPopAndRet	    ; M002 No, we already have a WORD 0 at end!

	mov	byte ptr es:[1], 0  ; M002 Add an extra 0 -- so that we have a
				    ; M002 WORD 0 -- This is done to be compatible
				    ; M002 with command.com!!! I don't like it as
				    ; M002 it is not elegant/appealing to me!

SPPopAndRet:
	pop	es
	assume es:DSEG

	; NOTE: DS:SI now points just after the CR or LF in buffer!
	ret

SPDontAddStr:
	; Need to leave SI just past the CR or LF at end of this line!
	mov	al, CR
	call	scan_for_char
	jmp	short SPDone

SPFoundEnvString:
	push	ds
	push	si	; DS:SI points to the beginning of NAME=STRING

	push	es
	pop	ds	; DS = ES = env_seg
	
	push	di
	pop	si	; DS:SI now points to the NAME=STRING in the ENV!
	
	mov	bl, 0
	call	scan_for_char	; look for the null terminator of this string

SPScootEnvLeft:
	cmp	byte ptr ds:[si], 0
	je	SPEnvEndFound

				; Note that BL = 0 for fn strcpy_till below to
	call	strcpy_till	; to do a "normal" strcpy!
	jmp	short SPScootEnvLeft

SPEnvEndFound:
	pop	si
	pop	ds
	jmp	short SPCopyThisStringToEnv

SPNoEquals:
	pop	ax	; Get rid of the SI on stack!
	pop	es
	assume	es:DSEG

	; NOTE: DS:SI now points just after the CR or LF in buffer!
	ret

set_proc ENDP


;****************************************************************************
;*
;* ROUTINE:	if_proc
;*
;* FUNCTION: 	This routine handles the "IF" intrinsic command. It handles
;*		the following commands:
;*              IF [NOT] [EXIST|ERRORLEVEL] ...
;*
;* INPUT:	DS:SI = pointer to the beginning of the word after the 'IF'
;*			keyword.
;*						if  ERRORLEVEL 3 ...
;*			i.e., DS:SI now points to   ^
;*		The line is expected to be terminated by a CR or LF.
;*
;*		Note that this function recurses -- we could have IFs
;*		within IFs!
;*
;* OUTPUT:	see functionality. DS:SI points after the CR or LF at end.
;*
;* REGISTERS:	ALL destroyed.
;****************************************************************************

if_proc PROC NEAR

	; Note that we can allow this command even in normal interactive mode!
	; The code is set up to handle it anyway!

	mov	es:kw_NOT_present, 0

	mov	di, offset DSEG:NOT_STR
	mov	cx, NOT_STR_LEN
	call	check_for_kw		; Do we have the NOT keyword?
	jne	IPNoNOTKeyword		; No!

	mov	es:kw_NOT_present, 1

IPNoNOTkeyword:
	mov	di,offset DSEG:ERRLVL_STR
	mov	cx, ERRLVL_STR_LEN
	call	check_for_kw		; Do we have ERRORLEVEL keyword?
	je	IPYupERRLVLFound	; Yes!

	mov	di, offset DSEG:EXIST_STR
	mov	cx, EXIST_STR_LEN
	call	check_for_kw		; Do we have EXIST keyword?
	jne	IPErrorIncorrectSyntax	; No, implies incorrect syntax

	;; If we are here it means we have syntax: IF [NOT] EXIST ...

	; Set up the DTA.
	;
	mov	ah, 1ah
	push	ds
	push	es
	pop	ds		; DS=ES=DSEG for the DTA!
	mov	dx, offset DSEG:DTA_Buffer
	int	21h		; Set DTA!
	pop	ds

	mov	dx, si		; DS:DX now points to first char of filename
	call	get_to_white_space
	push	ax		; AL has the character terminating filename!
				; save it for future use!
	mov	byte ptr [si], 0; NULL terminate filename!

	; Do the Find First.
	mov	ah, 4eh
	xor	cx, cx		; CX = attribs - find just normal files!
				; DS:DX already set up to point to filename.
	int	21h
	pop	ax		; get back AL -- char that was replaced by NULL!
	mov	[si], al	; and put it back in buffer!
	jc	IPExprIsFALSE
	jmp	short IPExprIsTRUE

IPYupERRLVLFound:
	;; If we are here it means we have syntax: IF [NOT] ERRORLEVEL ...
	call	atoi

	push	ax			; store the ATOI value!
	call	get_to_white_space
	call	skip_white_space	; DS:SI points to THEN command now!
	pop	ax			; get the ATOI value.

	cmp	ax, es:last_pgm_exit_code
	jbe	IPExprIsTRUE

IPExprIsFALSE:
	cmp	es:kw_NOT_present, 0
	je	IPDontExecThenCommand

IPExecThenCommand:
	
	; Note that even if I did not make this indirect recursive
	; call, everything will work fine -- the looping pump in batch
	; file processing or the main_loop would have handled it. But, then
	; echo-ing the command would be a problem as we would see the
	; THEN part of the command as another command to be echo-ed!
	call	process_cmd

	ret

IPExprIsTRUE:
	cmp	es:kw_NOT_present, 0
	je	IPExecThenCommand

IPDontExecThenCommand:
IPErrorIncorrectSyntax:
	mov	bl, CR
	call	scan_for_char

	; DS:SI now points past the cmd that forms the THEN part!
	ret

if_proc ENDP


; Note that no error is given out if user does this in non-BAT file
; processing mode! This is the way he can type stuff equivalent to the
; REM cmd!

;****************************************************************************
;*
;* ROUTINE:	label_proc
;*
;* FUNCTION: 	This routine handles the ":" intrinsic command. All it does
;*		is ignore this line. Note that the GOTO command is when we
;*		re-scan the buffer for this label!
;*
;* INPUT:	DS:SI = pointer to the ':' in the line that is expected
;*			to end in a CR or LF.
;*
;* OUTPUT:	see functionality. DS:SI points after the CR or LF at end.
;*
;* REGISTERS:	AX, BX destroyed.
;****************************************************************************

label_proc PROC NEAR

	; Note that we could just jump to appr label in prev function and
	; save a few bytes!
	mov	bl, CR
	call	scan_for_char
	
	; DS:SI points just after the CR or LF ready for next command!
	ret
label_proc ENDP



;****************************************************************************
;*
;* ROUTINE:	goto_proc
;*
;* FUNCTION: 	This routine handles the "GOTO" intrinsic command.
;*
;* INPUT:	DS:SI = pointer to the label in the GOTO cmd.
;*						goto label-name
;*			i.e., DS:SI now points to    ^
;*
;*		The variable 'in_bat_file' in DSEG specifies whether we
;*		   we are processing the INIT BAT file. If we are processing
;*		   the BAT file, DS:0 is where the BAT file has been loaded
;*                 in memory.
;*
;*		The line is expected to be terminated by a CR or LF.
;*
;* OUTPUT:	see functionality. DS:SI points after the CR or LF in the
;*		   line that has the label-name, so that execution can proceed
;*		   from there. If we are not processing the INIT BAT file and
;*		   say the user typed it in from the command line, DS:SI
;*		   points after the CR or LF in this line. If the label is
;*                 not found in the buffer DS:SI points past the end of the
;*		   buffer that stores the INIT file.
;*
;* REGISTERS:	ALL destroyed, including BP.
;****************************************************************************

goto_proc PROC NEAR

	cmp	es:in_bat_file, 1	; Are we processing BAT file?
	jne	GPNotInBatFile		; No, in that case do nothing!

	; DS:SI now points to the label-name in the GOTO cmd!

	mov	bp, es:fsize	; store file size, later on we screw up
				; ES also and so we won't have DSEG in it!
	mov	cx, si		; store for future use

	call	get_to_white_space

	xchg	cx, si	; Si is now back to start of LABEL!
	sub	cx, si	; CX has length of the label in the GOTO cmd!
	or	cx, cx
	jz	GPNoLabel

	push	es	; save ES = DSEG, will be restored at RET time!

	push	ds
	pop	es
	assume es:nothing
	mov	di, si	; ES:DI (same as DS:SI) points to label in GOTO cmd!

	xor	si, si	; DS:0 is where the BAT file has been loaded in mem.

GPLoop:
	call	skip_white_space
	cmp	al, COLON
	je	GPColonFound

GPSkipPastEOL:
	mov	bl, CR
	call	scan_for_char
	cmp	si, bp		; compare with file size!
	jb	GPLoop

	; If we are here, we have traversed thru whole file but not seen the
	; label!
	pop	es
	assume es:DSEG

GPNoLabel:

	mov	dx, offset DSEG:NO_LABEL_MSG
	mov	cx, NO_LABEL_MSG_LEN
	call	print_str_stderr

	mov	si, bp	; Set SI to the size of the Init file! This would
			; mean that the processing of the file is over!
	ret

GPColonFound:
	inc	si	; get past the COLON
	call	skip_white_space; There could be white space between 
				;the COLON and the label
	; ES:DI has the label after the GOTO cmd and CX has the label length!
	call	check_for_kw
	jne	GPSkipPastEOL
	
	pop	es
	assume	es:DSEG

GPNotInBatFile:
	mov	bl, CR
	call	scan_for_char
	; DS:SI points to command after the label -- we have done the GOTO!
	
	ret

goto_proc ENDP


last_code	EQU	$

CSEG	ENDS


	end	start

;;; ******************* Cool generalized function...
IF 0
;  INTRINSIC : FUNCTION
;	INPUT: 	DS:SI buffer that has input from user terminated by CR or LF
;	OUTPUT:	CARRY flag set if it is not an intrinsic command.
;		else, command is carried out and CARRY clear.
;	Destroys: AX, BX, DI
intrinsic PROC NEAR
	push	si
	push	cx		; used as temp space to save and restore SI

	call	skip_white_space

	cmp	byte ptr [si], COLON

	;; BUG BUG Actually label_proc needs to be called!
	je	IntrSuccess

	mov	cx, si		; save for future use
	mov	di, offset DSEG:Intr_Cmds	; DS:DI points to intrinsic cmd table!

IntrMainLoop:
	mov	si, cx		; resore DS:SI to point to user cmd.

	cmp	byte ptr[di], 0	; Are we at end of Intrinsic Cmd table?
	je	IntrNotIntrCmd

IntrNextTableCharLoop:	
	mov	bl, [di]	; Load next char from table.	
	or	bl, bl
	jz	IntrFullMatchFound

	mov	al, [si]
	call	upper_case

	cmp	al, bl
	jnz	IntrMisMatch

	inc	si
	inc	di
	jmp	short IntrNextTableCharLoop

IntrFullMatchFound:
	; Make sure user's entry has same length of characters in cmd!
	mov	al, [si]

	call	is_SP_TAB?
	je	IntrExecuteCmd

	call	is_CR_LF?
	jne	IntrMisMatch

IntrExecuteCmd:
	call	skip_white_space
	call	word ptr ds:[di+1]

IntrSuccess:
	clc
	jmp	short IntrPopRegs

IntrMisMatch:
	mov	al, [di]
	inc	di
	or	al, al
	jnz	IntrMisMatch

	add	di, 2
	jmp	short IntrMainLoop

IntrNotIntrCmd:
	stc

IntrPopRegs:
	pop	cx
	pop	si
	ret
intrinsic ENDP

;;;; *************** Used to be part of INITIALIZED DATA!
Intr_Cmds	EQU	$

		db	'SET',0
set_proc_off	dw	offset CSEG:set_proc

		db	'ECHO',0
echo_proc_off	dw	offset CSEG:echo_proc

		db	'IF',0
if_proc_off	dw	offset CSEG:if_proc

		db	'GOTO',0
goto_proc_off	dw	offset CSEG:goto_proc
		
		db	0		; End of Intrinsic Command marker!

ENDIF
