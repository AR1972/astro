	TITLE QBI0 - Segment Organization Declarations

;QBI assumes that the following segments are the only segments
;    within their respective groups.

LOADRTMGROUP	GROUP	LOADRTM ;Runtime Module's EXE Loader must be first!
LOADRTMGROUP	GROUP	FAR_PRESERVED ; also include far data that's preserved
				; across Make-Exe
LOADRTMENDGROUP GROUP	LOADRTMEND ;End of Resident EXE Loader - must be last
				;always resident segment.  RTM loader will
				;toss all code between LOADRTMEND and DGROUP
				;during a Compile Dialog.
CPGROUP		GROUP	CP	;parser, scanner, varmgr, txtmgr, nammgr
UIGROUP		GROUP	UI	;user interface
LISTGROUP	GROUP	LIST	;lister
DBGGROUP	GROUP	DBG	;debug code
ID_STRINGSGROUP	GROUP	ID_STRINGS
				;debug strings
RAREGROUP	GROUP	RARE	;seldom used (error, init, terminate)
CODEGROUP	GROUP	CODE	;runtime
_TEXTGROUP	GROUP	_TEXT	;executors, math

LOADRTM 	SEGMENT BYTE PUBLIC 'CODE'	;must be 1st segment
LOADRTM 	ENDS
FAR_PRESERVED 	SEGMENT BYTE PUBLIC 'CODE'	; must be between LOADRTM
FAR_PRESERVED 	ENDS				; and LOADRTMEND
LOADRTMEND 	SEGMENT PARA PUBLIC 'CODE'	;must be para aligned for loader
LOADRTMEND 	ENDS
CP		SEGMENT BYTE PUBLIC 'CODE'
CP		ENDS
DBG		SEGMENT	BYTE PUBLIC 'CODE'
DBG		ENDS
ID_STRINGS	SEGMENT	BYTE PUBLIC 'CODE'
ID_STRINGS	ENDS
UI		SEGMENT	BYTE PUBLIC 'CODE'
UI		ENDS
LIST		SEGMENT	BYTE PUBLIC 'LIST'
LIST		ENDS
RARE		SEGMENT	BYTE PUBLIC 'CODE'
RARE		ENDS
CODE		SEGMENT	BYTE PUBLIC 'CODE'
CODE		ENDS
_TEXT		SEGMENT	BYTE PUBLIC 'CODE'
_TEXT		ENDS
	end
