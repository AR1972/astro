;***	EDIT.INC - Structures and constants for the Document Text Manager
;
; DESCRIPTION
;	Constants, structures and function prototypes required to deal
;	with the TEXTMGR.
;
;	NOTE: any changes should also be made to EDIT.H
;
;****


	includeOnce	context

MRS_pDocumentBuf EQU MRS_oPastLastVar


extrn	AppendLineBuf:far
extrn	NewBuf:far
extrn	FreeBuf:far
extrn	S_LinesInBuf:far
extrn	S_CbGetLineBuf:far


sBegin	UI
extrn	CbFindLine:near
extrn	BigMoveUp:near
extrn	BigMoveDown:near
extrn	LinearAddr:near
extrn	SegAddr:near

extrn	S_ReplaceLineBuf:near
extrn	S_InsertLineBuf:near
extrn	S_InsertBufInBuf:near
extrn	S_DeleteLinesBuf:near
sEnd	UI
