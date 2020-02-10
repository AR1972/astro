;*****************************************************************************
;*	* Structures:
;*	* NOTE: not generated

_ARC	STRUC		;* Absolute Rectangle
	axLeftArc	DB	?
	ayTopArc	DB	?
	axRightArc	DB	?
	ayBottomArc	DB	?
_ARC	ENDS

STF	RECORD	fEnabledStf:1, styleStf:15		;* style & flag

;*	* WND : window structure *
WND	STRUC
	idWnd		DW	?			;* id
	stfWnd		DB	(WIDTH STF) / 8 DUP (?)
	arcWindow	DB	SIZE _ARC DUP (?)
	arcClipping	DB	SIZE _ARC DUP (?)
	pfnWndProc	DD	?
	pwndParent	DW	?
	pwndSibbling	DW	?
	pwndChild	DW	?
	axCursor	DB	?
	ayCursor	DB	?
	rgwExtra	DW	?			;* array of WORDS
WND	ENDS

BOX	STRUC	;* Single and Double BCS
	chTopLeftBox	DB	?
	chTopRightBox	DB	?
	chBottomLeftBox	DB	?
	chBottomRightBox DB	?
	chTopBox	DB	?
	chBottomBox	DB	?
	chLeftBox	DB	?
	chRightBox	DB	?
BOX	ENDS


;*****************************************************************************


IDDEFAULT	=	1
IDCANCEL	=	2
IDNO		=	3
IDOK		=	IDDEFAULT
IDYES		=	IDDEFAULT
IDRETRY 	=	IDDEFAULT
IDABORT 	=	IDDEFAULT

tmcNull 	=	0
tmcOK		=	1
tmcOk		=	tmcOK
tmcCancel	=	2
tmcSysMin	=	0010H
tmcSysMax	=	0100H
tmcGrouped	=	8000H

tmcUserMin	=	tmcSysMax
tmcUserMax	=	tmcGrouped


MB_OK		=	1
MB_YESNOCANCEL	=	2
MB_RETRYCANCEL	=	3
MB_OKCANCEL	=	4
MB_ABORT	=	5
MB_YESNO	=	6
MB_RETRY	=	7
MB_TYPE 	=	00fH		; message type
MB_BEEP 	=	010H
MB_CAPTION	=	020H		; 1st param is caption
MB_NOHELP	=	8000H		; No Help button
