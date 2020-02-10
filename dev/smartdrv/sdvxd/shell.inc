;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1988-1990
;
;   Title:	SHELL.Inc - Public services for SHELL
;
;   Version:	1.00
;
;   Date:	30-Nov-1988
;
;   Author:	ARR
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   19-Sep-1988 RAL Original
;   12-Oct-1988 RAL Converted to use VxD_Service macros
;   29-Nov-1988 ARR Redo split from VMCTRL
;
;==============================================================================

Begin_Service_Table SHELL

SHELL_Service  SHELL_Get_Version, LOCAL
SHELL_Service  SHELL_Resolve_Contention
SHELL_Service  SHELL_Event,LOCAL
SHELL_Service  SHELL_SYSMODAL_Message,LOCAL
SHELL_Service  SHELL_Message,LOCAL

End_Service_Table SHELL

;
;   Windows Message Box Flags used on SHELL_SYSMODAL_Message and
;	SHELL_Message services
;
MB_OK			 EQU	    00H
MB_OKCANCEL		 EQU	    01H
MB_ABORTRETRYIGNORE	 EQU	    02H
MB_YESNOCANCEL		 EQU	    03H
MB_YESNO		 EQU	    04H
MB_RETRYCANCEL		 EQU	    05H
MB_ICONHAND		 EQU	    010H
MB_ICONEXCLAMATION	 EQU	    030H
MB_ICONASTERISK 	 EQU	    040H
MB_DEFBUTTON1		 EQU	    00H
MB_DEFBUTTON2		 EQU	    0100H
MB_DEFBUTTON3		 EQU	    0200H
MB_APPLMODAL		 EQU	    00H
MB_SYSTEMMODAL		 EQU	    01000H
MB_NOFOCUS		 EQU	    08000H
;
; Following bits are WIN386 SHELL extensions
;
MB_ASAP 		 EQU	    080000000H	; Message is timely, put
						;   up as soon as possible
;
;   Windows Message Box Return codes used on SHELL_SYSMODAL_Message and
;	SHELL_Message services
;
IDOK                     EQU	1
IDCANCEL                 EQU	2
IDABORT                  EQU	3
IDRETRY                  EQU	4
IDIGNORE                 EQU	5
IDYES                    EQU	6
IDNO                     EQU	7

;
; On SHELL_Event, the wParam in EAX occupies the low 16 bits of EAX. The High 16
;	bits contain extra flags
;
SE_WP_SetFocusBoost	equ	00000000000000010000000000000000b
SE_WP_SetFocusBoostBit	equ	16		; Boost the SYS VM till a
						; Set_Focus call
SE_WP_SwitcherBoost	equ	00000000000000100000000000000000b
SE_WP_SwitcherBoostBit	equ	17		; Boost the SYS VM on Switcher
						;  screen
SE_WP_FilSysChgBoost	equ	00000000000001000000000000000000b
SE_WP_FilSysChgBoostBit equ	18		; Boost the SYS VM till response
						;  to VDA_FileSysChange
SE_WP_ClipAPIBoost	equ	00000000000010000000000000000000b
SE_WP_ClipAPIBoostBit	equ	19		; Boost the SYS VM during clipbrd
						;  API
SE_WP_PrtScBoost	equ	00000000000100000000000000000000b
SE_WP_PrtScBoostBit	equ	20		; Boost the SYS VM during Print screen

include shellfsc.inc
