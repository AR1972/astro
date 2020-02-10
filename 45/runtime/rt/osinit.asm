	TITLE	OSINIT - Operating System Initialization/Termination module

;***
;OSINIT.ASM - Operating System	initialization/termination module
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains Operating System specific initialization
;	and termination support for the BASIC 3.0 runtime.  This module
;	will only be present in a user's program when a program contains
;	statements or features which need Operating system specific support.
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;
;	Code Segments
;
	USESEG	<INIT_CODE>	;Initialization
	USESEG	<OS_TEXT>	;Operating System
	USESEG	<NH_TEXT>	;Near Heap
	USESEG	<ER_TEXT>	; Runtime errors
	USESEG	<DV_TEXT>	

;
;	Data Segments
;
	USESEG	<_BSS>		;runtime data (uninitialized)
	USESEG	<_DATA> 	;runtime data (initialized)
	USESEG	<CONST> 	;runtime constants
	USESEG	<XIB>		; XIB and XIE must bracket XI!
	USESEG	<XI>		;initializer segment
	USESEG	<XIE>		

	INCLUDE seg.inc
	INCLUDE files.inc	; buffer length constant
	INCLUDE idmac.inc	
	INCLUDE rtps.inc	; constants shared with QBI

	INCLUDE compvect.inc	;component vectors

	INITIALIZER	B$xOSINI	;put B$xOSINI in initializer list.

	SUBTTL	Code Externals
	PAGE

sBegin	DV_TEXT 			
	externNP	B$NearRet	;for disp vectors in compvect.inc
sEnd	DV_TEXT 			

sBegin	NH_TEXT 			
	externNP	B$STDALCTMP	;deallocate sd if temp
sEnd	NH_TEXT 			

sBegin	ER_TEXT 			
	externNP	B$ERR_FC	
sEnd	ER_TEXT 			

	SUBTTL	Runtime data definitions for BASIC Operating System
	PAGE

sBegin	CONST

COMSPEC		DB	"COMSPEC="	;environment header string
ComSpecLength	EQU	$ - COMSPEC	;length of environment header

ComSpecFile	DB	"\COMMAND.COM",0 ;default COMSPEC for DOS3

sEnd	CONST

sBegin	_DATA

;
;	external data
;
	externW b$ini_disp	;One time initialization dispatch table

sEnd	_DATA

sBegin	_BSS

	globalW b$env_len,?	;length of environment table.
	externW __aenvseg	;environment segment
	externB b$PATHNAM 	;buffer for filname parsing


sEnd	_BSS

	SUBTTL	Runtime Operating System  Initialization
	PAGE
assumes CS,INIT_CODE
sBegin	INIT_CODE

;***
;B$xOSINI - Operating System	initializer
;PLM B$xOSINI()
;
;Purpose:
;	Initializer for Operating System  component.  This routine is called
;	by the Crt0 startup before _main is called.  It will update the
;	indirect dispatch tables for the Operating System  routines.  This
;	insures that the only time that Operating System support is accessed
;	is when this module is linked into the user program.
;
;Entry:
;	None.
;
;Exit:
;	b$ini_disp.OS_IVEC	- contains pointer to B$OSINI
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$xOSINI,<FAR>
cBegin
;
;	update "ONE" time initialization dispatch address to B$OSINI
;
	MOV	WORD PTR [b$ini_disp].OS_IVEC,OS_TEXTOFFSET B$OSINI
cEnd
sEnd	INIT_CODE		

	PAGE
assumes CS,OS_TEXT		
sBegin	OS_TEXT 		

;***
;B$OSINI	- One time initialization for Operating System
;void pascal B$OSINI()
;
;Purpose:
; BC3
; ---
;	Initializes Operating System component.
;	B$OSINI does the following:
;		determines the length of the environment table.
;
;Entry:
;	None.
;
;Exit:
;	b$env_len	- contains the length of the environment table.
;
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;
;****
cProc	B$OSINI,<NEAR>,<SI>	
cBegin
	PUSH	DS		;B$FIRST_ENV uses DS,SI
	CALL	B$FIRST_ENV	;DS:SI points to next env entry
	JZ	ENV_DONE	;if done, then jump

ENV_LOOP:
	CALL	B$NEXT_ENV	;keep getting the next env entry
				;SI points to beginning of next entry
	JNZ	ENV_LOOP	;if still not done, then jump

ENV_DONE:
	POP	DS		;restore data segment
	MOV	b$env_len,SI	;store offset of end (segment length)
cEnd

;***
; B$BldExecCmdLine - Parse command line for SHELL into acceptable form
; Purpose:
;	Parse command line into acceptable form for DOS exec call.
;	This routine is used by SHELL and OPEN "PIPE:".
;
;	Callers should have called B$HoldBuf1 if ID_DEBUG.	
;	(B$HoldBuf2 also if OS/2)				
;
; Inputs:
;	BX - psd of EXEC command string
; Outputs:
;	b$PATHNAM
;	If DOS5:
;		AX - contains CMD.EXE file spec to exec into. (= b$Buf2)
;		b$PATHNAM - contains parsed command line.
;	If DOS3:
;		DX:AX - far pointer to COMSPEC file spec.
;		b$PATHNAM - parsed command tail for EXEC.
; Modifies:
;	per conv.
; Uses:
;	per conv.
; Exceptions:
;	B$ERR_FC upon buffer overflow (> 128)
;******************************************************************************

cProc	B$BldExecCmdLine,<PUBLIC,NEAR>,<SI,DI,ES>
cBegin

;	Put string argument into b$PATHNAM zero-terminated.

	MOV	CX,[BX] 	;get length of string
	MOV	SI,[BX+2]	;get starting offset of string

	PUSH	DS		; set ES = DS before STOS
	POP	ES		

	MOV	DI,OFFSET DGROUP:b$PATHNAM+1 ;start with second byte

	JCXZ	NO_ARGS 	;branch if null argument

	MOV	AX,'C/' 	;want command /C
	STOSW
	MOV	AL,' '		;put a space after /C
	STOSB

	CMP	CX,FILNAML -1 -3 -1	; will we overflow buffer?
	JA	Overflow	; Brif so
	REP	MOVSB		;move the string to b$PATHNAM

NO_ARGS:			

;	For DOS5, just terminate with a null byte.
;	For DOS3, terminate with a carraige-return and set the first
;	byte of the buffer with the length of the string (not including
;	the appended carraige-return).

	MOV	BYTE PTR [DI],13 ;put a carriage-return at line end [10]
	SUB	DI,OFFSET DGROUP:b$PATHNAM+1 ;length of line without CR
	XCHG	AX,DI		;put length to a "byte-able" register
	MOV	b$PATHNAM,AL	;...and put into the first byte of buffer

	CALL	B$STDALCTMP	;deallocate the argument string if temporary

;	Determine far pointer to ENV table value for COMSPEC.

	MOV	ES,__aenvseg	; set ES to environment segment
	XOR	DI,DI		;start at beginning of segment
	MOV	AX,DI		;clear for scanning and tests

;	See if entry header in ENV table is "COMSPEC=".

COM_SEARCH:
	MOV	SI,OFFSET DGROUP:COMSPEC
	MOV	CX,ComSpecLength ;length of environment header
	REPZ	CMPSB
	JE	GOT_COMSPEC	;found it, proceed.

	MOV	CX,0FFFFH	;scan maximum amount until match
	REPNZ	SCASB		;scan so ES:DI points to byte after zero
	CMP	ES:[DI],AL	;test for double zero at ENV table end
	JNZ	COM_SEARCH	;if not table end, then try again

	MOV	DI,OFFSET DGROUP:ComSpecFile ;get the default file spec
	PUSH	DS
	POP	ES		;set ES = BASIC DS
GOT_COMSPEC:

;	For DOS5, copy the COMSPEC value into the buffer at b$Buf2.


;	For DOS3, put far pointer of ENV value in DX:AX.

	MOV	DX,ES		;for DOS 3, save the pathname segment...
	MOV	AX,DI		;...and offset

	PUSH	DS		;put DGROUP segment on stack...
	POP	ES		;...and set ES = DS = DGROUP

cEnd

Overflow:			
	JMP	B$ERR_FC	

;***
; B$FIRST_ENV - get first ENV entry
; B$NEXT_ENV - get next ENV entry
; Purpose:
;	Establish segment addressibility for environment segment and
;	return offset of the first entry. (B$FIRST_ENV)
;	Return offset the next next entry. (B$NEXT_ENV)
;
; Inputs:
;	None. (B$FIRST_ENV)
;	DS = environment segment. (B$NEXT_ENV)
;	BX = start of search for entry. (B$NEXT_ENV)
; Outputs:
;	DS = environment segment
;	SI = start of string or end of environment
;	BX = start of search for next entry
;	CX = length of string returned (zero if end of environment)
;	ZF = set if CX=0
; Modifies:
;	None.
; Exceptions:
;	None.
;****


cProc	B$FIRST_ENV,<NEAR,PUBLIC>	
cBegin				

	MOV	DS,__aenvseg	;get environment segment
	XOR	SI,SI		;DS:SI points to first environment entry

cEnd	<nogen>			; fall into B$FIRST_ENV


cProc	B$NEXT_ENV,<NEAR,PUBLIC>,<AX>	
cBegin				

	MOV	BX,SI		;return with bx = si at entry
	XOR	CX,CX		;length = 0
	CMP	DS:[SI],CL	;end or empty ?
	JZ	NXTENX		; yes
	INC	SI
NXTEN1:
	INC	CX
	LODSB
	OR	AL,AL		;eos ?
	JNZ	NXTEN1		;no, continue
NXTENX:
	OR	CX,CX		;set non-zero if not end of table

cEnd				


sEnd	OS_TEXT 		

	END
