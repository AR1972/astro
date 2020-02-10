;*
;*	COW : Character Oriented Windows
;*
;*	user.inc : user assembly segments

.xlist
	include cow.inc
.list

IFDEF COW_SWAPPED

createSeg   INIT,INIT,BYTE,PUBLIC,CODE			;* init code
createSeg   EXIT,EXIT,BYTE,PUBLIC,CODE			;* exit code
createSeg   CORE,USER_CORE,BYTE,PUBLIC,CODE		;* user fixed code

createSeg   CORE,SCREEN,BYTE,PUBLIC,CODE		;* screen code

IFDEF COW_TINY_SWAPPED
;* mostly everything in USER
createSeg   USER,USER_SCROLL,BYTE,PUBLIC,CODE		;* scroll helpers
createSeg   USER,USER_DIALOG,BYTE,PUBLIC,CODE		;* dialog helpers
createSeg   USER,USER_LISTBOX,BYTE,PUBLIC,CODE		;* listbox helpers
createSeg   USER,USER_EDIT,BYTE,PUBLIC,CODE		;* edit helpers
createSeg   USER,SDM,BYTE,PUBLIC,CODE			;* sdm helpers
ELSE
;* mostly everything in CORE
createSeg   CORE,USER_SCROLL,BYTE,PUBLIC,CODE		;* scroll helpers
createSeg   CORE,USER_DIALOG,BYTE,PUBLIC,CODE		;* dialog helpers
createSeg   CORE,USER_LISTBOX,BYTE,PUBLIC,CODE		;* listbox helpers
createSeg   CORE,USER_EDIT,BYTE,PUBLIC,CODE		;* edit helpers
createSeg   CORE,SDM,BYTE,PUBLIC,CODE			;* sdm helpers
ENDIF ;COW_TINY_SWAPPED


ELSE
;* not swapped => all 1 segment

createSeg   COW_TEXT,INIT,BYTE,PUBLIC,CODE
createSeg   COW_TEXT,EXIT,BYTE,PUBLIC,CODE
createSeg   COW_TEXT,USER_CORE,BYTE,PUBLIC,CODE
createSeg   COW_TEXT,SCREEN,BYTE,PUBLIC,CODE
createSeg   COW_TEXT,USER_SCROLL,BYTE,PUBLIC,CODE
createSeg   COW_TEXT,USER_DIALOG,BYTE,PUBLIC,CODE
createSeg   COW_TEXT,USER_LISTBOX,BYTE,PUBLIC,CODE
createSeg   COW_TEXT,USER_EDIT,BYTE,PUBLIC,CODE
createSeg   COW_TEXT,SDM,BYTE,PUBLIC,CODE
IFDEF	PROJECT_QB
createSeg   FAR_PRESERVED,FAR_PRESERVED,BYTE,PUBLIC,CODE
ENDIF	;PROJECT_QB

ENDIF ;!COW_SWAPPED
