;*
;*	COW : Character Oriented Windows
;*
;*	edityp.inc : Multi-line edit manager main structures
;*	* REVIEW : these should be cleaned up.
;*	* REVIEW : this should be generated from "umedit.h"
;*	* NOTE: this file is tied to "umedit.h" -- which is currently
;*		a public file !!


; Single line descriptor. All editing is done on this structure and then
; transfered to buffer.
LD	STRUC
	LD_flags	dw	0h
	LD_cb		dw	00h	;number of bytes in line buffer
	LD_cbMax	dw	0100h	;max. of bytes (size) allowed in buffer
	LD_prgch   	dw      0h	;pointer to line buffer
LD	ENDS

; Insertion point. Currency structure for current edit field
IP	STRUC
	IP_ob	dw	0	;offset for left edge of buffer
	IP_oln	dw	0	;offset within file 
IP	ENDS

; Program descripter. Currency for program (current buffer). This is used
; with the text manager to control what part of the current buffer is in
; view and what buffer line is in the line descripter
PD	STRUC
	pd_olntop	dw	0	;offset within file for top of screen
	pd_obleft	dw	0	;offset to left edge of screen
	pd_oln		dw	0	;line offset that is in ldCur
PD	ENDS

; General edit field structure. This is passed into EditMgr. Between calls
; to the edit manager it contains all state information about a particular
; edit field.
EF	STRUC
	EF_hBuffer	dw	0
	EF_pdCur	db	size pd dup(?)
	EF_ipCur	db	size ip dup(?)
	EF_ipAnchor	db	size ip dup(?)
	EF_pldCur	dw	?
	EF_pwndScrollV	dw	0
	EF_pwndScrollH	dw	0
	EF_attrCur	dw	0
	EF_Style	db	0
	EF_fSelection	db	0
EF	ENDS

EF_pdCur_olnTop		equ	word ptr EF_pdCur.PD_olnTop
EF_pdCur_obleft		equ	word ptr EF_pdCur.PD_obleft
EF_pdCur_oln		equ	word ptr EF_pdCur.PD_oln
EF_ipCur_ob		equ	word ptr EF_ipCur.IP_ob
EF_ipCur_oln		equ	word ptr EF_ipCur.IP_oln
EF_ipAnchor_ob		equ	word ptr EF_ipAnchor.IP_ob
EF_ipAnchor_oln		equ	word ptr EF_ipAnchor.IP_oln

LINEATTR STRUC
LA_attr	dw	?
LA_cb	dw	?
LINEATTR ENDS

;styles:
ES_MULTILINE    equ	0001H
ES_NOREDRAW	equ	0002H
ES_NOSELECT	equ	0004H
EF_MOVECURSOR	equ	0008H
ES_KK_UNIT	equ	0080H

;control messages:
EM_SELCHARS	equ	WM_USER+0
EM_REPLACESEL	equ	WM_USER+1
EM_GETWORD	equ	WM_USER+2
EM_GETLINESEL	equ	WM_USER+3
WM_UNDO 	equ	WM_USER+4
WM_CLEAR	equ	WM_USER+5
WM_SETTEXT	equ	WM_USER+6
WM_GETTEXT	equ	WM_USER+7
WM_SEARCHFIND	equ	WM_USER+8
WM_SEARCHCHANGE equ	WM_USER+9
WM_SETBOOKMARK	equ	WM_USER+10
WM_GOTOBOOKMARK equ	WM_USER+11
WM_SETREDRAW	equ	WM_USER+12
WM_SEARCHNEXT	equ	WM_USER+13
WM_MATCHBRACE	equ	WM_USER+14
WM_FLUSHFOCUS	equ	WM_USER+15
EM_MOVECURSOR	equ	WM_USER+16

pefExtra	equ	(rgwExtra+2)

; ldCur.flags values
LD_fDirty			EQU	0001H

; emFlags values
EMF_IN_EDITMGR			EQU	01H
