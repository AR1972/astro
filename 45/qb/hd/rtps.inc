;***
; rtps.inc - 04-Aug-87
;***
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	This include file contains runtime constants needed by both the
;	runtime and QBI.  This file, along with fdb.inc, array.inc, and
;	messages.inc is part of a runtime "release" to QBI.
;
; WARNING: WARNING: WARNING: WARNING: WARNING: WARNING: WARNING: WARNING:
; WARNING:
; WARNING:  When making changes to this file, be sure to make equivalent
; WARNING:  changes to file rtps.h
; WARNING:
; WARNING: WARNING: WARNING: WARNING: WARNING: WARNING: WARNING: WARNING:
;
;******************************************************************************

RTPS_INC = -1			;remember that this file has been included

;==============================================================================
;
;QBI version & format revision constants (used by U.L. code & Binary SAVE/LOAD)
;   The version word is now a family id, it indicates what "family" the saving
;   product belongs to.  The revision byte indicates the revision of the
;   product within its family.




BINSAV_BASICVersion	EQU	0001h	; BASIC version number for QB 4.5
BINSAV_REVISION_BYTE	EQU	00h	; format revision byte
QLB_BASICVersion	EQU	0000h	; BASIC version no. for QB 4.x QLB's
QLB_REVISION_BYTE	EQU	13h	; QuickLib format revision byte

;Log of when BINSAV_REVISION_BYTE was changed:
;Date		Changed To	Reason
;------------------------------------------------------------------------
;23-Feb-87	    01		First change. Unknown number of older
;				binary-saved files in existence.
;27-Feb-87          02		No longer putting BLOCK_MOD at start of each
;				module, since we only save one module per file.
;16-Mar-87          03		Code to support compiling directly from 
;				our Binary SAVE format needs module name table
;				to be saved before the module text table.
;10-Jun-87          04		Changed for todays internal QB release. Must
;				be changed whenever peropcod.txt is changed.
;10-Jun-87 (PM)     05		Changed for todays internal QB release. Must
;				be changed whenever peropcod.txt is changed.
;17-Jun-87	    06		Changed for Beta Release because of peropcode
;				changes (DMC).
;30-Jun-87          07		Changed based on actual change to binsav format
;				(now saving both mrs flag bytes)
;15-Jul-87	    08		Peropcod changes. (DMC)
;21-Jul-87	    09		Peropcod changes and second beta. (DMC)
;03-Aug-87	    0a		Peropcod changes. (DMC)
;04-Aug-87	    0b		Peropcod changes. (DMC)
;10-Aug-87	    0c		VarMgr changes. (DMC)
;26-Aug-87	    0d		QLB dgroup compression changes. (DMC)
;31-Aug-87	    0e		C startup changes. (DMC)
;09-Sep-87	    0f		rtmint changes. (DMC)
;14-Sep-87	    10		rtmint changes. (DMC)
;15-Sep-87	    11		First final (?) beta REL.19 (DMC)
;15-Sep-87	    12		First final (?) beta REL.20 (DMC)
;??-???-87	    13		Release version REL.35 (???)
;11-Nov-87	    14		First Beta for BC6.  Want to be backwards
;				compatable to release version of QB4.
;16-Nov-87	    13		We're stuck at 13 for QB4 release compatibility!
;25-May-88	    14 (EB)	Changed for EB only, to make binary
;				incompatabilities easier to detect (BLR)
;08-Jun-88	    15 (EB)	Changed for EB only, due to changes in nam,
;				var, rs, and pcode tables to leave 2nd word
;				of each far heap buffer unused (used by Omega)
;15-Jun-88	    16 (EB)	Changed for EB only, due to additional pcode
;				changes to leave 2nd word free.
;24-Jun-88	    17 (EB)	Changed for EB only, removed data_otxfirst
;30-Jun-88	    18		Changed many AVtRfs to AIdLds in pcode
;05-Jul-88	    19		Removed obsolete pcodes
;07-Jul-88	    1a		Changed coercion pcode
;11-Jul-88	    00		Rewound due to change in binary format.  BASIC
;				version number is now a family number.	0 = QB
;				(v4.0 a,b), 1 = QB (v4.5 and later), 2 = EB.
;05-Aug-88	    01 (EB)	SQL pcode change
;==============================================================================


; [LINE] INPUT prompt flags
FINP_QSupress	EQU 1	;set if "prompt" was followed by a comma,
			;not a semicolon, indicating "? " is not to be
			;output after prompt.
FINP_CrLf	EQU 2	;set if INPUT was followed by optional ";",
			;meaning CrLf is not to be output when user
			;presses return.
FINP_Prompt	EQU 4	;set if the optional SDPrompt argument is included.


; Communications constants
cbComBuf	EQU 512 ; default COM buffer size

; File LOCK & UNLOCK Constants
LOCK_UNLOCK		EQU 0001h	;set if operation is UNLOCK, not LOCK
LOCK_1stToLast		EQU 0002h	;set if only part of file locked
LOCK_RtMask		EQU 0003h	;bits which are meaningful to runtime
LOCK_Def1stArg		EQU 4000h	;set if 1st arg defaulted - used by list
LOCK_DefLastArg		EQU 8000h	;set if last arg defaulted- used by exec

;File Open mode, access, locking flags
MD_SQI			EQU 1	; for INPUT
MD_SQO			EQU 2	; for OUTPUT
MD_RND			EQU 4	; [for RANDOM]
MD_APP			EQU 8	; for APPEND
MD_BIN			EQU 20H	; for BINARY
MD_DEFAULT		EQU MD_RND
ACCESS_READ		EQU 1	; READ
ACCESS_WRITE		EQU 2	; WRITE
ACCESS_BOTH		EQU 3	; READ WRITE
LOCK_READ		EQU 30H	; LOCK READ
LOCK_WRITE		EQU 20H	; LOCK WRITE
LOCK_BOTH		EQU 10H	; LOCK READ WRITE
LOCK_SHARED		EQU 40H	; LOCK SHARED

;----------------------------------------------------------------------
;DOS does not allow you to do a CD (change directory) to a path > 64,
;but once you are in a directory whose path is 64 bytes long, DOS
;will let you open a 64 byte relative path, making the maximum length
;of a filename accessible by DOS = 128.  Since the runtime just has 1
;static copy of a filename buffer, they decided to let running programs
;access files whose full path > 64 (even though you can't get into the
;lowest directory from DOS.
;  The user interface chooses to limit the path to 64 for the directory,
;and 12 for the filename (and 1 for a 0-byte terminator).  This is for
;two reasons:
;  - The directory that source files are saved in should be accessible by
;    all programs (including command.com).  
;  - There are several layers of functions in the user interface code that
;    each need a copy of a filename.  The increased stack demands of going
;    to 128 are prohibitive.
;
FILNAML = 128d+1	;max. filename length for runtime
FILNAML64 = 64d+12d+1	;max. filename length for user interface

;Runtime Value Types
VT_I2 EQU 02h		;short integer
VT_I4 EQU 14h		;long integer
VT_R4 EQU 04h		;32 bit real
VT_R8 EQU 08h		;64 bit real
VT_SD EQU 03h		;string

;Runtime bits for b$CtrlFlags
NoSTACKINIT EQU 01h	;Doesn't call B$STACKINIT when set during B$RUNINI
			; or B$CHNINI calls.

;Extended Out of Memory error codes for b$errinfo.
OMErr_OM   EQU 00h	;generic OM error
OMErr_NH   EQU 01h	;out of near heap space ( DS > 64k )
OMErr_FH   EQU 02h	;out or far heap space	( out of system memory )
OMErr_STK  EQU 03h	;out of stack space
OMErr_Proc EQU 04h	;out of Procedure text table space ( > 64k )
OMErr_Mod  EQU 05h	;out of Module text table space ( > 64k )
OMErr_Inc  EQU 06h	;out of Include file text table space ( > 64k )
OMErr_Doc  EQU 07h	;out of Document file text table space ( > 64k )

;File type constants for user-option path searching
LIBFILE    EQU 0	
EXEFILE    EQU 6	
INCFILE    EQU 12	
HELPFILE   EQU 18	

; Minimum space for NMALLOC buffer.  Moved here from const.inc with rev [17].
;
; This is the minimum space which must be allocated in the NMALLOC segment
; for any attempted near malloc() call to fail gracefully.  Enough space
; must be allocated for a "C" heap header.
;

NMALLOC_MIN	= 06H		;Minimum NMALLOC space needed.
