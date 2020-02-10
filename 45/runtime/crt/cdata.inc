;***
;cdata.inc - 30-Jan-87 - C Run-time Shared Global Data
;***
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	The data items defined here  must  appear  first  in  the  DATA
;	segment  of  RT_DATA  because they must exactly match up between
;	the linked program and the separately linked run-time module.
;	This is also true of user libraries and the QB4 runtime.  This
;	file should be included in global.inc only.
;
;	All non-zero initialized data should be using the DEFDATA macro,
;	so that QLIB's will have a large uninitialized (or zero) data hole
;	that can be compressed.
;
;******************************************************************************

labelB	<PUBLIC,__anullsize>	;[6] label to "end" of _NULL for null pointer
				;[6] checking.  Any addr less than this is
				;[6] considered to be invalid.  Safe to put it
				;[6] at the beginning of our data, since we
				;[6] don't want to check null pointers.

; **********	Definitions copied from BRKCTL.INC	********************

MAXSEG_DOS = 20			; maximum brkctl segments for REAL mode
MAXSEG_OS2 = 256		;[6] maximum brkctl segments for PROTECTED mode

segrec		struc			; defines brkctl segment offsets
  sz		dw	?
  sg		dw	?
segrec		ends



; **********	Definitions copied from MSDOS.INC	********************

STACKSLOP	equ	256	; stack slop for interrupt overhead



; **********	Data taken from CHKSTK.ASM

DefData W,STKHQQ,<OFFSET DGROUP:_end+STACKSLOP> ;initial C stack low-water mark
;[2]  STKHQQ	dw	OFFSET DGROUP:_end+STACKSLOP ; initial value

DefData	D,__aaltstkovr,-1	;[3] alternate stack overflow
;[3] globalCP __aaltstkovr,-1	; alternate stack overflow


; **********	Data taken from CRT0MSG.ASM


; **********	Data taken from CRT0/5CRT0.ASM	****************************

;	these are used by DOS C memory management (not used in Windows)

globalW	__asizds,0 		; DS size (in bytes)
globalW	__atopsp,0 		; top of stack (heap bottom)
globalW __aexit_rtn,0		; NEAR pointer
;[2] globalW	__aexit_rtn,<OFFSET _TEXT:__exit> ; NEAR pointer
;[2] added

labelW	<PUBLIC,__abrktb> 	; segment table for brkctl
	dw	?
DefData W,,DGROUP
;	dw	DGROUP
	db	(MAXSEG_DOS-1) * (size segrec) dup (?)

labelW	<PUBLIC,__abrktbe>
DefData W,__abrkp,<OFFSET DGROUP:__abrktb>
;[2] globalW	_abrkp,<dataoffset _abrktb>


; **********	Data taken from CRT0DAT.ASM	****************************

;	special C environment string

labelB	<PUBLIC,__acfinfo>	;[2]


	cfile	db	';C_FILE_INFO'
	cfilex	db	0
	b$cfileln EQU cfilex-cfile


PUBLIC	b$cfileln		;[2]
	b$cfilex EQU cfilex	;[2]
PUBLIC	b$cfilex		;[2]


globalD	__aintdiv,0 		; divide error interrupt vector save

globalQ	__fac,0			; floating accumulator
globalW	_errno,0		; initial error code
globalW	__umaskval,0 		; initial umask value

;=============== following must be in this order

globalW	__pspadr,0 		; psp:0 (far * to PSP segment)
globalW	__psp,0			; psp:0 (paragraph #)

;=============== above must be in this order

;=============== following must be in this order

labelW	<PUBLIC,__osversion>
labelB	<PUBLIC,__dosvermajor>
globalB	__osmajor,0
labelB	<PUBLIC,__dosverminor>
globalB	__osminor,0

;=============== above must be in this order

globalB	__osmode,0 		; 0 = real mode

labelW	<PUBLIC,__oserr>
globalW	__doserrno,0 		; initial DOS error code

_NFILE_ = 20			; Maximum number of file handles

DefData W,__nfile,<_NFILE_>	; maximum number of file handles
;globalW	__nfile,_NFILE_ 	; maximum number of file handles

;	__osfile flag values for DOS file handles

FOPEN		equ	01H	; file handle open
FEOFLAG 	equ	02H	; end of file has been encountered
FCRLF		equ	04H	; CR-LF across read buffer (in text mode)
FPIPE		equ	08H	; file handle refers to a pipe
FRDONLY 	equ	10H	; file handle associated with read only file
FAPPEND 	equ	20H	; file handle opened O_APPEND
FDEV		equ	40H	; file handle refers to device
FTEXT		equ	80H	; file handle is in text mode


labelB	<PUBLIC,__osfile>
defdata	B,,<FOPEN+FTEXT>	; stdin
defdata	B,,<FOPEN+FTEXT>	; stdout
defdata	B,,<FOPEN+FTEXT>	; stderr
;[2]	db	3 dup (FOPEN+FTEXT)	; stdin, stdout, stderr
defdata	B,,FOPEN			; stdaux
defdata	B,,FOPEN			; stdprn
;[2]	db	2 dup (FOPEN)		; stdaux, stdprn
	db	_NFILE_-5 dup (0)	; the other 15 handles

globalW	___argc,0
globalDP ___argv,0
globalDP _environ,0 		; environment pointer

globalW __child,0		; flag used to handle signals from child process

labelD	<PUBLIC,__pgmptr> 	; pointer to program name
DefData W,,<OFFSET DGROUP:dos2nam>
DefData W,,DGROUP
labelB	dos2nam
DefData B,,'C'			; dummy argv[0] for DOS 2.X
DefData B,,0
;[2]  	dw	dataOFFSET dos2nam,DGROUP
;[2] dos2nam	db	'C',0		; dummy argv[0] for DOS 2.X

;*
;*	The following (_acmdln and _acmdseg) must be in this order!
;*

globalW __acmdln,0		;[1] Offset of command line string in env
globalW __acmdseg,0		;[1] command line pointer
globalW __aenvseg,0		;[1] Selector of Environment segment


;Overlay related data

globalB __ovlflag,0		;[4] Overlay flag (0 = no overlays)
globalB	__intno,0		;[4] Overlay interrupt value (e.g., 3F)
globalD __ovlvec,0		;[4] Address of original overlay handler
