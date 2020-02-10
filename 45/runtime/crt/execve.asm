	title	execve	-- stub to disable C execs and spawns

;--------------------------------------------------------------------------
;
;	Microsoft C Compiler Runtime for MS-DOS
;
;	(C)Copyright Microsoft Corporation, 1985, 1985, 1986
;
;--------------------------------------------------------------------------
;
;	Purpose
;
;	This routine is used to disable all forms of C execs and spawns
;	for BASIC.  BASIC will trap the attempt and issue an advanced
;	feature error.	All forms of the C exec function filter through
;	_execve.  All forms of spawn, and system() filter into _spawnve.
;
;----------------------------------------------------------------------------

?DF=		1			; this is special for c startup
include version.inc
.xlist
include cmacros.inc
.list

createSeg	_TEXT,	code,	byte,	public, CODE,	<>

?PLM = 1
externP B$ERR_AFE			;BASIC advance feature error
sBegin	code
assumes cs,code

;	_spawnve, _execve
;
;	The BASIC runtime cannot coexist peacefully with the
;	C exec, spawn, and system functions.  All of these
;	functions filter their way into either _spawnve or
;	_execve.  We will trap the attempt and issue an
;	ADVANCED feature error to the BASIC program.

labelP	<PUBLIC,_spawnve>
cProc	_execve,<PUBLIC,FORCEFRAME>,<>
cBegin
	PUSH	SS		;[2] set DS = SS = DGROUP
	POP	DS		;[2]
	JMP	B$ERR_AFE	; let BASIC print the error message
cEnd	nogen

sEnd	code
	end
