;***
; DEVDEF.INC - 08-Aug-86 - Device Independent I/O Definitions
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

GENDEV	MACRO	dev,num
if	num
	nmdev=0
	REPT	num
	nmdev=nmdev+1
	DEVMAC	dev&%nmdev
	ENDM
endif
	ENDM

DEVNAM	MACRO
	DEVMAC	KYBD
	DEVMAC	SCRN
	DEVMAC	CONS

	GENDEV	COM,NUM_RS232
	GENDEV	LPT,NUM_LPT
	DEVMAC	PIPE
	ENDM


___DEV= -1

DEVMAC	MACRO	ARG
DN_&ARG= ___DEV
.xcref
___DEV= ___DEV-1
.cref
	ENDM

	DEVNAM


LAST_DEVICE_OFFSET= -2*___DEV	;All devices have lower offsets
	PAGE

DSPNAM	MACRO
	DSPMAC	EOF		;;EOF function
	DSPMAC	LOC		;;LOC function
	DSPMAC	LOF		;;LOF function
	DSPMAC	CLOSE		;;CLOSE statement
	DSPMAC	WIDTH		;;WIDTH statement
	DSPMAC	RANDIO		;;GET/PUT statements
	DSPMAC	OPEN		;;OPEN statement
	DSPMAC	BAKC		;;Backup character
	DSPMAC	SINP		;;Serial input
	DSPMAC	SOUT		;;Serial output
	DSPMAC	GPOS		;;Get current position
	DSPMAC	GWID		;;Get current width
	DSPMAC	DWID		;[8] Set device width
	DSPMAC	BLKIN		;;BLOCK INPUT
	DSPMAC	BLKOUT		;;BLOCK OUTPUT
	ENDM


;	Device Function Dispatch Table Offsets


DSPMAC	MACRO	func
ENT	DV_&func,2
	ENDM


ENTORG	0
	DSPNAM
ENT	DV_TABLEN,0

	PAGE

;	File Mode Definitions

	MD_RNDCH	EQU	10H	;[5]Random character device - PIPE.

;[b$ACCESS] - file ACCESS definitions

	ACCESS_NONE	EQU	0h	;[1]
	ACCESS_DEFAULT	EQU	0h	;[1]

;[b$LOCKTYPE] - file LOCK definitions	;[3]

	LOCK_COMPATIBLE EQU	00h	;[1]
	LOCK_DEFAULT	EQU	00h	;[1]

	.LIST
