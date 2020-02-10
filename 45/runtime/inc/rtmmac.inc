;***
; RTMMAC.INC - 20-Mar-86 - Macros for Generating Entry Numbers
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

WORG	MACRO	num
IF1
VecTable&num LABEL WORD
	INDEX	= 0		;;[1]
	PBYTE   = W1__NM - 1 + num
ENDIF	;;IF1
	ENDM

WLAST	MACRO	num
IF1			;;[1]
VecIndex&num = INDEX	;;[1]define ending index of each table
ENDIF	;;IF1		;;[1]
	ENDM

;	Display software interrupt postbyte.

PRINTE	MACRO	msg,pb,n
	ENDM

;	Create entry in table and define as external.

WORDDW	MACRO	pre,nam
	EXTRN	pre&nam:NEAR
	DW	pre&nam
IF1
	INDEX=INDEX+1		;;[1]
	PRINTE	pre&nam,%PBYTE,%INDEX
ENDIF	;;IF1
	ENDM

;	First table defines item if conditional true.

W1	MACRO	nam,cond
IFNB	<cond>
IF	cond
	WORDDW	<B$>,nam
ENDIF	;;IF <cond>
ELSE	;;IFNB <cond>
	WORDDW	<B$>,nam
ENDIF	;;IFB <cond>
	ENDM

;	Second table defines item if conditional true.

W2	MACRO	nam,cond
IFNB	<cond>
IF	cond
	WORDDW	<B$>,nam
ENDIF	;;IF <cond>
ELSE	;;IFNB <cond>
	WORDDW	<B$>,nam
ENDIF	;;IFB <cond>
	ENDM

;	Third table defines items depending on the list parameter:
;		If blank, num items defined suffixed by A, B, C, ...
;		If nonblank, items defined through list

W3	MACRO	nam,num,list
IFNB	<list>

IRP	item,<list>	;;if list is nonempty,...
	WORDDW	<B$>,item	;;...make an entry for each list item
ENDM

ELSE	;;IFNB <list>

IF	num GE 1
	WORDDW	<B$>,nam&A
ENDIF
IF	num GE 2
	WORDDW	<B$>,nam&B
ENDIF
IF	num GE 3
	WORDDW	<B$>,nam&C
ENDIF
IF	num GE 4
	WORDDW	<B$>,nam&D
ENDIF
IF	num GE 5
	WORDDW	<B$>,nam&E
ENDIF
IF	num GE 6
	WORDDW	<B$>,nam&F
ENDIF
IF	num GE 7
	WORDDW	<B$>,nam&G
ENDIF
IF	num GE 8
	WORDDW	<B$>,nam&H
ENDIF

ENDIF	;;IFB <list>

IF	num GT 8
	ERROR	W3 entry index too large
ENDIf
	ENDM

;	Fourth table defines items through the list.

W4	MACRO	nam,num,list

IRP	item,<list>
	WORDDW	<__>,item	;;make an entry for each list item
ENDM
ENDM

	.LIST
