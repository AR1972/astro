;***
;uinhelp.h - Defines and data for the New Help System
;
;   Copyright <C> 1985-1988 Microsoft Corporation
;
;Purpose:
; NOTE: When making changes to this file, be sure to make equivalent
;   changes to file UINHELP.H
;
;***************************************************************************


; These constants define the source of calls to Help().  Each type of caller
; passes parameters in a different way and expects the text to be displayed
; differently

hemWindowHid    = hemUserMin+1    ; Display help (ID = hid) in window */
hemAllWindow    = hemUserMin      ; F1 pressed in any window */

WM_FIRSTHELPMSG = WM_USER+20      ; WM_USER+15 is the last editmgr message.
                  ; use +20 in case of change.

WM_HELPBACK = WM_FIRSTHELPMSG
WM_HELPNEXT = WM_FIRSTHELPMSG + 1
WM_HELPDISP = WM_FIRSTHELPMSG + 2
WM_HELPCURTOPIC = WM_FIRSTHELPMSG + 3
WM_HELPNXTTOPIC = WM_FIRSTHELPMSG + 4
WM_HELPCHGTOPIC = WM_FIRSTHELPMSG + 5
WM_HELPRESTORETOPIC = WM_FIRSTHELPMSG + 6   
WM_HELPTITLE    = WM_FIRSTHELPMSG + 7
WM_HELPLINE = WM_FIRSTHELPMSG + 8
WM_HELPATTR = WM_FIRSTHELPMSG + 9
WM_HELPFILESIZE = WM_FIRSTHELPMSG + 10

NUM_HELPMSG = 11

;; a-emoryh - Max size of command-line topic, in Dos6 QHelp.
cbmaxTOPIC  EQU  40

