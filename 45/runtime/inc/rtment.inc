;***
;rtment.inc - 11/23/87
;***
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	The RTMENT macro is redefined in many places, but
;	RTMINT{1|2|3|4|5|6}.ASM all need it defined to the same
;	value.	To make maintenance of this macro easier it is
;	defined here for those 6 modules.
;
;************************************************************************

	maxpb equ 255		; maximum post-byte value for QB case (255
				;   signals double post-byte)

RTMENT	MACRO	entryname,file,entryseg,startlabel,callflag

IFNDEF	UL&entryname
IF	file EQ FILENUM
	PUBLIC	entryname
entryname PROC	    FAR

IFNB	<&startlabel>		;; for duplicate labels
	PUBLIC	startlabel	;; to save some bytes
startlabel	LABEL FAR	;;
ENDIF				;;

IFNB	<&callflag>		;; generate a far call here for interrupt
				;; handler to backpatch
	CALL	FAR PTR t_&entryname	;;handler to backpatch
IF	callflag EQ 0		;; if return instruction specified
	RET			;; follow it with a FAR return
ENDIF				;;
t_&entryname	LABEL FAR	;;
ENDIF				;;

	INT	3FH		;;Transfer to the runtime module

	IF	entrypb NE 0	;;if there is to be a two-byte post byte
	DB	0FFH		;;the put in flag value
	ENDIF
	DB	entrynum	;;and the post byte value (0 through f9)

entryname ENDP
ENDIF				;;IF file EQ FILENUM
ENDIF				;;IFNDEF UL&entryname

entrynum	=	entrynum + 1
	IF	entrynum GE maxpb
entrynum	=	0
	IF	entrypb NE 0	;;if too many entries
	.err	Too Many runtime entries
	ENDIF
entrypb 	=	1
	ENDIF

	ENDM
