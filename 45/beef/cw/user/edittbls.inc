ifdef	EDIT_USE_TABLES
LabelB	DefaultEditMgrTbls
	db	4 , "Alt", 0		; Name of ACTION Key
	db	4 , "Esc", 0		; Name of CANCEL key
	dw	VK_MENU, VK_F11, VK_ESCAPE AND 0ffh, VK_F12
LabelB	DefaultDoCharDispatch
else	; !EDIT_USE_TABLES
LabelB	DoCharDispatch
endif	; EDIT_USE_TABLES
	MainEditTable	IgnoreChar		  ; Null
	MainEditTable	WordLeft		  ; ^A
	MainEditTable	NearBeep		  ; ^B
	MainEditTable	PageDown		  ; ^C
	MainEditTable	CharRight		  ; ^D
	MainEditTable	LineUp			  ; ^E
	MainEditTable	WordRight		  ; ^F
	MainEditTable	Del			  ; ^G
	MainEditTable	Backspace		  ; ^H
	MainEditTable	TabOrBackTab		  ; ^I
	MainEditTable	NextLine		  ; ^J
ifdef	EDIT_USE_TABLES
	MainEditTable	DoPrefixTable		  ; ^K
	MainEditTable	SearchNext		  ; ^L
else	; ! EDIT_USE_TABLES
	MainEditTable	CtrlKState		  ; ^K
	MainEditTable	NearBeep		  ; ^L
endif	; EDIT_USE_TABLES
	MainEditTable	NewLine 		  ; ^M
	MainEditTable	SplitLine		  ; ^N
	MainEditTable	NearBeep		  ; ^O
ifdef	EDIT_USE_TABLES
	MainEditTable	DoQuoteCharacter	  ; ^P
	MainEditTable	DoPrefixTable		  ; ^Q
else	; ! EDIT_USE_TABLES
	MainEditTable	CtrlPState		  ; ^P
	MainEditTable	CtrlQState		  ; ^Q
endif	; EDIT_USE_TABLES
	MainEditTable	PageUp			  ; ^R
	MainEditTable	CharLeft		  ; ^S
	MainEditTable	DelWord 		  ; ^T
ifdef	EDIT_USE_TABLES
	MainEditTable	ResetState		  ; ^U
else	; EDIT_USE_TABLES
	MainEditTable	IgnoreChar		  ; ^U
endif	; EDIT_USE_TABLES
	MainEditTable	ToggleInsertMode	  ; ^V
	MainEditTable	ScrollUp		  ; ^W
	MainEditTable	LineDown		  ; ^X
	MainEditTable	KillLine		  ; ^Y
	MainEditTable	ScrollDown		  ; ^Z
ifdef	EDIT_USE_TABLES
	MainEditTable	DoEsc			  ; ^[ (ESC)	27
	MainEditTable	IgnoreChar		  ; ^\		28
	MainEditTable	MatchBrace		  ; ^]		29
	MainEditTable	IgnoreChar		  ; ^^		30
	MainEditTable	IgnoreChar		  ; ^_		31
	MainEditTable	Del			  ; DEL		127 ****
endif	; EDIT_USE_TABLES

ifdef	EDIT_USE_TABLES
	dw	KeyDispatchEnd - KeyDispatch
endif	; EDIT_USE_TABLES
LabelB	KeyDispatch
	KeyTableEntry	VK_TAB, TabOrBackTab
	KeyTableEntry	VK_INSERT, MaybeToggleInsertMode
	KeyTableEntry	VK_PRIOR, PageUp
	KeyTableEntry	VK_NEXT, PageDown
	KeyTableEntry	VK_END, EndLine
	KeyTableEntry	VK_HOME, HomeLine
	KeyTableEntry	VK_LEFT, CharLeft
	KeyTableEntry	VK_RIGHT, CharRight
	KeyTableEntry	VK_UP, LineUp
	KeyTableEntry	VK_DOWN, LineDown
	KeyTableEntry	0, IgnoreKey
ifdef	EDIT_USE_TABLES
KeyDispatchEnd:

	dw	(CtrlKeyDispatchEnd - CtrlKeyDispatch)
endif	; EDIT_USE_TABLES
LabelB	CtrlKeyDispatch
	KeyTableEntry	VK_PRIOR, FullPageLeft
	KeyTableEntry	VK_NEXT, FullPageRight
	KeyTableEntry	VK_END, EndPgm
	KeyTableEntry	VK_HOME, BegPgm
	KeyTableEntry	VK_LEFT, WordLeft
	KeyTableEntry	VK_RIGHT, WordRight
	KeyTableEntry	VK_UP, ScrollUp
	KeyTableEntry	VK_DOWN, ScrollDown
	KeyTableEntry	0, IgnoreKey
ifdef	EDIT_USE_TABLES
CtrlKeyDispatchEnd:

	dw	2			;* Number of Prefix tables

	;* Ctrl-Q

	dw	(CtrlQDispatchEnd - CtrlQDispatch)
	dw	''*2			;* Offset in table

endif	; EDIT_USE_TABLES
LabelB CtrlQDispatch
ifdef	EDIT_USE_TABLES
	SecondEditTable	'0',GotoBookmark0
	SecondEditTable	'1', GotoBookmark1
	SecondEditTable	'2', GotoBookmark2
	SecondEditTable	'3', GotoBookmark3
	SecondEditTable '', ResetState
endif	; EDIT_USE_TABLES
	SecondEditTable	'', BegLine
	SecondEditTable	's', BegLine
	SecondEditTable	'S', BegLine
	SecondEditTable	'', EndLine
	SecondEditTable	'd', EndLine
	SecondEditTable	'D', EndLine
	SecondEditTable	'', HomeScn
	SecondEditTable	'e', HomeScn
	SecondEditTable	'E', HomeScn
	SecondEditTable	'', EndScn
	SecondEditTable	'x', EndScn
	SecondEditTable	'X', EndScn
	SecondEditTable	'', EraseEol
	SecondEditTable	'y', EraseEol
	SecondEditTable	'Y', EraseEol
	SecondEditTable	'', Undo
	SecondEditTable	'l', Undo
	SecondEditTable	'L', Undo
	SecondEditTable	'', BegPgm
	SecondEditTable	'r', BegPgm
	SecondEditTable	'R', BegPgm
	SecondEditTable	'', EndPgm
	SecondEditTable	'c', EndPgm
	SecondEditTable	'C', EndPgm
	SecondEditTable	'', DispatchFind
	SecondEditTable	'F', DispatchFind
	SecondEditTable	'f', DispatchFind
	SecondEditTable	'', DispatchChange
	SecondEditTable	'A', DispatchChange
	SecondEditTable	'a', DispatchChange
	SecondEditTable	0, IgnoreChar

ifdef	EDIT_USE_TABLES
CtrlQDispatchEnd:
	
	dw	(CtrlKDispatchEnd - CtrlKDispatch)
	dw	''*2

LabelB	CtrlKDispatch
	SecondEditTable '', ResetState
	SecondEditTable	'0', SetBookmark0
	SecondEditTable	'1', SetBookmark1
	SecondEditTable	'2', SetBookmark2
	SecondEditTable	'3', SetBookmark3
	SecondEditTable	0, IgnoreChar
CtrlKDispatchEnd:

	;*
	;*	VK dispatch tables
	;*
	
	dw	0			;* Number of prefix tables
endif	; EDIT_USE_TABLES
