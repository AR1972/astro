	TITLE	LLASCN - GW-BASIC Advanced Screen Interface
;***
; LLASCN - GW-BASIC Advanced Screen Interface
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This is one of the sample OEM dependent source code modules
;	required to build the GW-BASIC Interpreter or the GW-BASIC
;	Compiler for the IBM PC family of computers.
;
;	This module contains OEM dependent support for the following:
;
;		SHELL Statement 	(B$SHLSAV and B$SHLRST)
;		PCOPY Statement 	(B$PCOPYS)
;
;******************************************************************************
	INCLUDE switch.inc	; feature switches [new]
	INCLUDE	rmacros.inc	

	UseSeg	_DATA		
	UseSeg	_BSS		
	UseSeg	RT_TEXT		

	INCLUDE	seg.inc		
	INCLUDE ibmunv.inc

sBegin	_DATA			

        externW b$CurrPSize    ;Current page size in paragraphs
        externW b$VideoBase    ;Current video base
sEnd	_DATA			

sBegin	_BSS			
	externB b$MaxPage	
	externB b$BiosMode	

	externW b$KBDVEC_SAVE	;LLINI - save area for keyboard vector
	externW b$CLKVEC_SAVE	;LLINI - save area for clock vector

	staticW OLDCS,,1	;used to save CS when SHELL is executed
	staticW ISAV,,6 	;save area for interrupt vectors

sEnd	_BSS			

sBegin	RT_TEXT			

	ASSUMES	CS,RT_TEXT	

	externD  b$OldClkTic	;LLINI - old vector of INT 1CH

	externNP B$SCINIT	; initialize screen

	externNP B$OutWord	;LLAGRP - write out port of EGA

;***
;B$SHLSAV - Save machine state for a SHELL
;OEM-interface routine
;
;Purpose:
;	This routine will be called whenever a process is
;	shelled. It saves all the machine dependent configuration
;	parameters which may be changed by the shelled process.
;	As of now it saves all the screen mode parameters and the
;	interrupt vectors.
;
;	This is a DOS only routine.
;
;Entry:
;	None
;
;Exit:
;	PSW.C set will cause a function call error to be declared.
;	No attempt will be made to execute the user process.
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	None
;********************************************************************

cProc	B$SHLSAV,<PUBLIC,NEAR>,<AX,BX,DX,ES,DS> 
cBegin				;NOTE: exits through
				;SAVXT in B$SHLRST

	SAVINT	ISAV,CLKINT	; save timer interrupt
	SAVINT	ISAV+4,TIMADR	; save timer control interrupt
	SAVINT	ISAV+8,KBDINT	; save keyboard interrupt
	XOR	DX,DX
	MOV	ES,DX		;[ES] := 0
	MOV	OLDCS,CS	;save old CS
	CLI			;disable interrupts
	RSTVEC	TMRCTL,ES:TICSAV ;restore 'get ctl on timer INT'
	POP	DS		;restore DS
	PUSH	DS		;save DS
	XFRINT	KYBINT,KBDVEC/4 ;move INT EFH to INT 09H
	XFRINT	TIMINT,CLKVEC/4 ;move INT F0H to INT 08H
	RSTVEC	KBDVEC/4,b$KBDVEC_SAVE ;restore INT EF vector
	POP	DS		;restore seg register
	PUSH	DS		;save seg register...
	RSTVEC	CLKVEC/4,b$CLKVEC_SAVE ;restore INT F0 vector
	POP	DS		;restore seg register
	PUSH	DS		;save seg register...
	JMP	SAVXT		;common return for
				;B$SHLSAV and B$SHLRST
cEnd	<nogen> 		

PAGE				
;***
;B$SHLRST - Restore machine state after a SHELL
;OEM-interface routine
;
;Purpose:
;	This routine is called after a shelled process terminates.
;	It restores the machine dependent configuration parameters
;	saved by B$SHLSAV. If this restoration can not be done,
;	the carry flag is set on exit and the runtime will abort
;	with the error "Can't continue after SHELL".
;
;	This is a DOS only routine.
;
;Entry:
;	None
;
;Exit:
;	PSW.C set will cause the runtime to abort with a SHELL error
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	None
;********************************************************************

cProc	B$SHLRST,<PUBLIC,NEAR>,<AX,BX,DX,ES,DS> 
cBegin
	CLI			;disable interrupts
	SAVINT	CS:b$OldClkTic,TIMADR ; saving int vector 1ch in b$OldClkTic
	MOV	AX,OLDCS	;[AX] = old CS
	CMP	AX,[ISAV+2]	;is ISAV+1 same as old CS   ?
	JNE	SHLRS1		;Brif not
	MOV	[ISAV+2],CS	;else set it to new CS
SHLRS1:
	CMP	AX,[ISAV+6]	;is [ISAV+3] same as old CS ?
	JNE	SHLRS2		;Brif not
	MOV	[ISAV+6],CS	;else set it to new CS
SHLRS2:
	CMP	AX,[ISAV+10]	;is [ISAV+5] same as old CS ?
	JNE	SHLRS3		;Brif not
	MOV	[ISAV+10],CS	;else set it to new CS
SHLRS3:
	PUSH	DS
	POP	ES		;[ES] := [DS]
assumes	ES,DGROUP		
assumes	DS,NOTHING		
	XFRINT	KBDVEC/4,KBDINT/4 ;move INT 09H to INT EFH
	XFRINT	CLKVEC/4,CLKINT/4 ;move INT 04H to INT F0H
	RSTVEC	TIMINT,ES:ISAV	;restore timer interrupt
	RSTVEC	TMRCTL,ES:[ISAV+4] ;restore 'get timer ctl interrupt'
	RSTVEC	KYBINT,ES:[ISAV+8] ;restore keyboard interrupt
	PUSH	ES
	POP	DS		; [ds] := [es] i.e., old [ds]
assumes	DS,DGROUP		
assumes	ES,NOTHING		

SAVXT:				;exit point for B$SHLSAV
	STI
	CLC			;indicate no error
cEnd				

PAGE				
;***
;B$PCOPYS - Support for PCOPY statement
;OEM-interface routine
;
;Purpose:
;	This routine provides the support for the PCOPY statement.
;	It copies the contents of one screen page (either graphics
;	or text) to another.
;
;	If the source or destination page numbers are not in the range of
;	allowable pages for the current mode as determined by the formula:
;
;
;	       max_page_allowed = video_memory/page_size - 1
;
;
;	then the routine returns with carry set so that an Illegal Function
;	Call will be issued.
;
;	If the source and destination pages are the same, this routine
;	can just return without trying to copy anything.
;
;	NOTE: if you are supporting delayed screen initialization,
;	      B$SCNINT must be called before any changes to the
;	      screen pages are made.
;
;	This is a DOS only routine.
;
;Entry:
;	[AX] = source page #
;	[CX] = destination page #
;
;Exit:
;	PSW.C set indicates a function call error.
;
;Uses:
;	Per Convention
;
;Exceptions:
;	None
;********************************************************************



cProc	B$PCOPYS,<PUBLIC,NEAR>,<SI,DI,DS,ES>	
cBegin				

	CALL	B$SCINIT	; initialize screen if not already done
	OR	CH,AH		; Check if High-bytes zero
	STC			; Prepare for error condition
	JNZ	PCOPY_EXIT	; Brif either arg > 255
	MOV	CH,AL		; CH = Source page : CX = page values
        MOV     AL,b$MaxPage   ;permitted max-pages for this mode
	CMP	AL,CH		; Is source-page > maximum allowed?
	JB	PCOPY_EXIT	; Yes, exit with error : CF = 1
	CMP	AL,CL		; Is destination-page > max value?
	JB	PCOPY_EXIT	; Yes - issue error : CF = 1
	CMP	CL,CH		; Src = Dst?
	JE	PCOPY_EXIT	; Br - nothing to do

;	Get the base address of source & destination pages

        MOV     BX,b$VideoBase 

COPY_PAGES:

;	Now, get the page size for this mode

        MOV     DI,b$CurrPSize ;DI = 128-byte page units
	CLD			; Get ready for auto-increment

;	Now, compute the offset of src/dst page from the base
;	The following multiplications always result in DX = 0 because only
;	the paragraph size of each page is used.

	MOV	AL,CL		; AX = Destination page
	MUL	DI		; AX = dst-page * page-size
	ADD	AX,BX		; Add the base address also to get the final
	MOV	ES,AX		; Address in ES (destination base)
	MOV	AL,CH		; Get source page number
	MOV	AH,BL		; AH = BL = 0
	MUL	DI		; AX = src-page * page-size
	ADD	AX,BX		; Add the base address also to get the final
	MOV	BL,b$BiosMode	;get BIOS mode while we still have DS
	MOV	DS,AX		; Address in DS (source base)

;	DI still has the page size (in paras) & convert it to word-size

	MOV	CL,3		; Multiply by 8
	SHL	DI,CL		; DI = page size in words
	MOV	CX,DX		; CX = DX = 0
	XCHG	DI,CX		; CX = page size
				; DI = 0 (destination offset from page)

;	If current mode is a CGA mode then all is set for page copying

	CMP	BL,0DH		;Is it a CGA mode?
	MOV	SI,DI		; SI = 0 (Source offset from page)
	JB	CGA_COPY	; Brif to normal word copy

EGA_COPY:

;	We can use the EGA write mode 1 to do 4-plane memory copying via
;	the MOVSB instruction. This mode is documented to work so that the
;	planes are written from the latches, which are loaded by a previous
;	read. What in fact happens is that the MOVSB instruction handles the
;	reads in such a way that the latches are correctly loaded for the
;	4-planes from DS:SI and the write to ES:DI reproduces the contents of
;	the source area at the destination.

	MOV	DX,GRPADD	; EGA port address
	MOV	AX,1 SHL 8 + RWMReg ; Use write-mode 1
	OutWord 		;[17]Set up EGA
	SHL	CX,1		; Make it a byte count
REP	MOVSB			; ... and go
	MOV	AX,RWMReg	; Reset EGA to mode 0
	OutWord 		;[17]and DX is still intact
	JMP	SHORT PCOPY_RET	; & Exit...

CGA_COPY:			; Simple word move would do

REP	MOVSW			; Copy source to destination

PCOPY_RET:			; Clear CF to indicate no error
	CLC			

PCOPY_EXIT:			; Time to exit - do not change flags

cEnd				; End of B$PCOPYS



sEnd	RT_TEXT			

	END
