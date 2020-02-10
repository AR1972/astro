;*************************************************************************
;
;       WINDOWS.INC - Windows assembly language structures & constants
;
;*************************************************************************
;
; Conditional Block includes:   (True states)
;     NOTEXT - don't include TextMetric struc & text drawing modes & stock objs.
;     NORASTOPS - don't include binary and ternary raster ops.
;     NOVK      - don't include virtual key definitions
;     NOMB      - don't include message box definitions
;     NOWM      - don't include window messages
;
;
FALSE   =       0
TRUE    =       1
NULL    =       0

;*******************************************************************
;
;       Rectangle
;
;*******************************************************************

RECT    struc
        rcLeft          dw      ?
        rcTop           dw      ?
        rcRight         dw      ?
        rcBottom        dw      ?
RECT    ends

;*******************************************************************
;
;  Window Class structure
;
;*******************************************************************

WNDCLASS struc
        clsStyle        dw      ?       ; class style
        clsLpfnWndProc  dd      ?
        clsCbClsExtra   dw      ?
        clsCbWndExtra   dw      ?
        clsHInstance    dw      ?       ; instance handle
        clsHIcon        dw      ?       ; class icon handle
        clsHCursor      dw      ?       ; class cursor handle
        clsHbrBackground dw     ?       ; class background brush
        clsLpszMenuName dd      ?       ; menu name
        clsLpszClassName dd     ?       ; far ptr to class name
WNDCLASS ends

IFNDEF NOTEXT
TEXTMETRIC struc
tmHeight        dw      ?
tmAscent        dw      ?
tmDescent       dw      ?
tmIntLeading    dw      ?
tmExtLeading    dw      ?
tmAveCharWidth  dw      ?
tmMaxCharWidth  dw      ?
tmWeight        dw      ?
tmItalic        db      ?
tmUnderlined    db      ?
tmStruckOut     db      ?
tmFirstChar     db      ?
tmLastChar      db      ?
tmDefaultChar   db      ?
tmBreakChar     db      ?
tmPitch         db      ?
tmCharSet       db      ?
tmOverhang      dw      ?
tmAspectX       dw      ?
tmAspectY       dw      ?
TEXTMETRIC ends
;
;  Text Drawing modes
;
TRANSPARENT     = 1
OPAQUE          = 2
;
;  Stock Logical Objects
;
WHITE_BRUSH     =  0
LTGRAY_BRUSH    =  1
GRAY_BRUSH      =  2
DKGRAY_BRUSH    =  3
BLACK_BRUSH     =  4
HOLLOW_BRUSH    =  5
WHITE_PEN       =  6
BLACK_PEN       =  7
NULL_PEN        =  8
DOT_MARKER      =  9
OEM_FIXED_FONT  = 10
ANSI_FIXED_FONT = 11
ANSI_VAR_FONT   = 12
ENDIF

;
ANSI_CHARSET    = 0
OEM_CHARSET     = 255
;
;  styles for CombineRgn
;
RGN_AND  = 1
RGN_OR   = 2
RGN_XOR  = 3
RGN_DIFF = 4
RGN_COPY = 5
;
;  Predefined cursor & icon IDs
;
IDC_ARROW       = 32512
IDC_IBEAM       = 32513
IDC_WAIT        = 32514
IDC_CROSS       = 32515
IDC_UPARROW     = 32516
IDC_SIZE        = 32640
IDC_ICON        = 32641

IDI_APPLICATION = 32512
IDI_HAND        = 32513
IDI_QUESTION    = 32514
IDI_EXCLAMATION = 32515
IDI_ASTERISK    = 32516
;
;   Scroll bar constants
;
SB_HORZ = 0
SB_VERT = 1
;
;   Scroll Commands
;
SB_LINEUP        = 0
SB_LINEDOWN      = 1
SB_PAGEUP        = 2
SB_PAGEDOWN      = 3
SB_THUMBPOSITION = 4
SB_THUMBTRACK    = 5
;
;  MessageBox type flags
;
IFNDEF                  NOMB
MB_OK                   = 0000H
MB_OKCANCEL             = 0001H
MB_ABORTRETRYIGNORE     = 0002H
MB_YESNOCANCEL          = 0003H

MB_ICONHAND             = 0010H
MB_ICONQUESTION         = 0020H
MB_ICONEXCLAMATION      = 0030H
MB_ICONASTERISK         = 0040H

MB_DEFBUTTON1           = 0000H
MB_DEFBUTTON2           = 0100H
MB_DEFBUTTON3           = 0200H

MB_APPLMODAL            = 0000H
MB_SYSTEMMODAL          = 1000H
;
;  Conventional dialog box and message box command IDs
;
IDOK     =   1
IDCANCEL =   2
IDABORT  =   3
IDRETRY  =   4
IDIGNORE =   5
IDYES    =   6
IDNO     =   7
;
;  Flags for OpenFile
;
OF_REOPEN       = 8000H
OF_EXIST        = 4000H
OF_PROMPT       = 2000H
OF_CREATE       = 1000H
OF_CANCEL       = 0800H

TF_FORCEDRIVE   = 80H

OPENSTRUC       STRUC
opLen   db      ?
opDisk  db      ?
opXtra  dw      ?
opDate  dw      ?
opTime  dw      ?
opFile  db      120 dup (?)
OPENSTRUC       ENDS
;
;  DrawText format flags
;
DT_LEFT         = 00H
DT_CENTER       = 01H
DT_RIGHT        = 02H
DT_TOP          = 00H
DT_VCENTER      = 04H
DT_BOTTOM       = 08H
DT_WORDBREAK    = 10H
DT_SINGLELINE   = 20H
DT_EXPANDTABS   = 40H
DT_TABSTOP      = 80H
ENDIF
;
; Memory manager flags
;
LMEM_FIXED      =   00h
LMEM_MOVEABLE   =   02h
LMEM_ZEROINIT   =   40h
LMEM_DISCARDABLE=   0F00h
LHND    =    LMEM_MOVEABLE+LMEM_ZEROINIT
LPTR    =    LMEM_FIXED+LMEM_ZEROINIT

GMEM_FIXED      =   00h
GMEM_MOVEABLE   =   02h
GMEM_ZEROINIT   =   40h
GMEM_DISCARDABLE=   0F00h
GMEM_SHARE	=   2000h
GHND    =    GMEM_MOVEABLE+GMEM_ZEROINIT
GPTR    =    GMEM_FIXED+GMEM_ZEROINIT

;  Virtual Keys, Standard Set

IFNDEF          NOVK
VK_LBUTTON      = 01H
VK_RBUTTON      = 02H
VK_CANCEL       = 03H
VK_BACK         = 08H
VK_TAB          = 09H
VK_CLEAR        = 0cH
VK_RETURN       = 0dH
VK_SHIFT        = 10H
VK_CONTROL      = 11H
VK_MENU         = 12H
VK_PAUSE        = 13H
VK_CAPITAL      = 14H
VK_ESCAPE       = 1bH
VK_SPACE        = 20H

VK_PRIOR        = 21H
VK_NEXT         = 22H
VK_END          = 23H
VK_HOME         = 24H
VK_LEFT         = 25H
VK_UP           = 26H
VK_RIGHT        = 27H
VK_DOWN         = 28H

;  VK_A thru VK_Z are the same as their ASCII equivalents: 'A' thru 'Z'
;  VK_0 thru VK_9 are the same as their ASCII equivalents: '0' thru '0'

VK_PRINT        = 2aH
VK_EXECUTE      = 2bH
VK_INSERT       = 2dH
VK_DELETE       = 2eH
VK_HELP         = 2fH

VK_NUMPAD0      = 60H
VK_NUMPAD1      = 61H
VK_NUMPAD2      = 62H
VK_NUMPAD3      = 63H
VK_NUMPAD4      = 64H
VK_NUMPAD5      = 65H
VK_NUMPAD6      = 66H
VK_NUMPAD7      = 67H
VK_NUMPAD8      = 68H
VK_NUMPAD9      = 69H
VK_MULTIPLY     = 6AH
VK_ADD          = 6BH
VK_SEPARATER    = 6CH
VK_SUBTRACT     = 6DH
VK_DECIMAL      = 6EH
VK_DIVIDE       = 6FH

VK_F1           = 70H
VK_F2           = 71H
VK_F3           = 72H
VK_F4           = 73H
VK_F5           = 74H
VK_F6           = 75H
VK_F7           = 76H
VK_F8           = 77H
VK_F9           = 78H
VK_F10          = 79H
VK_F11          = 7aH
VK_F12          = 7bH
VK_F13          = 7cH
VK_F14          = 7dH
VK_F15          = 7eH
VK_F16          = 7fH
ENDIF

;*************************************************************************
;
;       Misc structures & constants
;
;*************************************************************************

IFNDEF  NOMST
POINT   struc
        ptX             dw      ?
        ptY             dw      ?
POINT   ends

BITMAP STRUC
        bmType         DW ?
        bmWidth        DW ?
        bmHeight       DW ?
        bmWidthBytes   DW ?
        bmPlanes       DB ?
        bmBitsPixel    DB ?
        bmBits         DQ ?
BITMAP ENDS

WNDSTRUC struc
        WSwndStyle        dd      ?
        WSwndID           dw      ?
        WSwndText         dw      ?
        WSwndParent       dw      ?
        WSwndInstance     dw      ?
        WSwndClassProc    dd      ?
WNDSTRUC ends
;
;  Message structure
;
MSGSTRUCT       struc
msHWND          dw      ?
msMESSAGE       dw      ?
msWPARAM        dw      ?
msLPARAM        dd      ?
msTIME          dd      ?
msPT            dd      ?
MSGSTRUCT       ends

NEWPARMS struc
        nprmHwnd        dw      ?
        nprmCmd         db      ?
NEWPARMS ends
ENDIF

PAINTSTRUCT STRUC
    PShdc         DW ?
    PSfErase      DW ?
    PSrcPaint     DB size RECT dup(?)
    PSfRestore    DW ?
    PSfIncUpdate  DW ?
    PSrgbReserved DB 16 dup(?)
PAINTSTRUCT ENDS
;
; ShowWindow commands
;
HIDE_WINDOW     = 0
SHOW_OPENWINDOW = 1
SHOW_ICONWINDOW = 2
;
;       PostError constants
;
WARNING     = 0           ; command codes
MINOR_ERROR = 1
FATAL_ERROR = 2

IGNORE      = 0           ; response codes
RETRY       = 1
ABORT       = 2
;
; GDI-related constants & commands
;
ERRORREGION     = 0
NULLREGION      = 1
SIMPLEREGION    = 2
COMPLEXREGION   = 3

IFNDEF NORASTOPS
;
; Binary raster ops
;
R2_BLACK        =  1
R2_NOTMERGEPEN  =  2
R2_MASKNOTPEN   =  3
R2_NOTCOPYPEN   =  4
R2_MASKPENNOT   =  5
R2_NOT          =  6
R2_XORPEN       =  7
R2_NOTMASKPEN   =  8
R2_MASKPEN      =  9
R2_NOTXORPEN    = 10
R2_NOP          = 11
R2_MERGENOTPEN  = 12
R2_COPYPEN      = 13
R2_MERGEPENNOT  = 14
R2_MERGEPEN     = 15
R2_WHITE        = 16
;
; Ternary raster ops
;
SRCCOPY_L     = 0020h   ;dest=source
SRCCOPY_H     = 00CCh
SRCPAINT_L    = 0086h   ;dest=source OR dest
SRCPAINT_H    = 00EEh
SRCAND_L      = 00C6h   ;dest=source AND   dest
SRCAND_H      = 0088h
SRCINVERT_L   = 0046h   ;dest= source XOR      dest
SRCINVERT_H   = 0066h
SRCERASE_L    = 0328h   ;dest= source AND (not dest )
SRCERASE_H    = 0044h
NOTSRCCOPY_L  = 0008h   ;dest= (not source)
NOTSRCCOPY_H  = 0033h
NOTSRCERASE_L = 00A6h   ;dest= (not source) AND (not dest)
NOTSRCERASE_H = 0011h
MERGECOPY_L   = 00CAh   ;dest= (source AND pattern)
MERGECOPY_H   = 00C0h
MERGEPAINT_L  = 0226h   ;dest= (source AND pattern) OR dest
MERGEPAINT_H  = 00BBh
PATCOPY_L     = 0021h   ;dest= pattern
PATCOPY_H     = 00F0h
PATPAINT_L    = 0A09h   ;DPSnoo
PATPAINT_H    = 00FBh
PATINVERT_L   = 0049h   ;dest= pattern XOR     dest
PATINVERT_H   = 005Ah
DSTINVERT_L   = 0009h   ;dest= (not dest)
DSTINVERT_H   = 0055h
BLACKNESS_L   = 0042h   ;dest= BLACK
BLACKNESS_H   = 0000h
WHITENESS_L   = 0062h   ;dest= WHITE
WHITENESS_H   = 00FFh
;
; StretchBlt modes
;
BLACKONWHITE    = 1
WHITEONBLACK    = 2
COLORONCOLOR    = 3
;
; PolyFill modes
;
ALTERNATE       = 1
WINDING         = 2
ENDIF
;
; Menu flags for Change/Check/Enable MenuItem
;
MF_CHANGE       = 0080h
MF_INSERT       = 0000h
MF_APPEND       = 0100h
MF_DELETE       = 0200h
MF_BYPOSITION   = 0400h
MF_BYCOMMAND    = 0000h
MF_GRAYED       = 0001h
MF_DISABLED     = 0002h
MF_ENABLED      = 0000h
MF_CHECKED      = 0008h
MF_BITMAP       = 0004h
MF_STRING       = 0000h
MF_POPUP        = 0010h
MF_DIVIDER      = 0020h
MF_BREAK        = 0040h
;
;  System Menu Command Values
;
SC_SIZE         = 0F000H
SC_MOVE         = 0F010H
SC_ICON         = 0F020H
SC_ZOOM         = 0F030H
SC_NEXTWINDOW   = 0F040H
SC_PREVWINDOW   = 0F050H
SC_CLOSE        = 0F060H
SC_VSCROLL      = 0F070H
SC_HSCROLL      = 0F080H
SC_DUPLICATE    = 0F090H
SC_MOUSEMENU    = 0F100H
SC_KEYMENU      = 0F110H
;
;  Window State Messages
;
IFNDEF  NOWM
WM_STATE            = 0000H

WM_NULL             = 0000H
WM_CREATE           = 0001H
WM_DESTROY          = 0002H
WM_QUERYDESTROY     = 0003H
WM_SYNCSIZE         = 0004H
WM_SIZE             = 0005H
WM_ACTIVATE         = 0006H
WM_SETFOCUS         = 0007H
WM_KILLFOCUS        = 0008H
WM_SETVISIBLE       = 0009H
WM_ENABLE           = 000aH
WM_SETREDRAW        = 000bH
WM_SETTEXT          = 000cH
WM_GETTEXT          = 000dH
WM_GETTEXTLENGTH    = 000eH
WM_PAINT            = 000fH
WM_CLOSE            = 0010H
WM_QUERYQUIT        = 0011H
WM_QUIT             = 0012H
WM_QUERYOPEN        = 0013H
WM_ERASEBKGND       = 0014H
WM_SYSCOLORCHANGE   = 0015H
WM_ENDSESSION       = 0016H
WM_SYSTEMERROR      = 0017H
WM_SHOWWINDOW       = 0018H

WM_QUERYDEFPROC     = 0080H
WM_NCCREATE         = 0081H
WM_NCDESTROY        = 0082H
WM_NCCALCSIZE       = 0083H
WM_NCHITTEST        = 0084H
WM_NCPAINT          = 0085H
WM_NCACTIVATE       = 0086H
WM_GETDLGCODE       = 0087H
WM_ENDDIALOG        = 0088H

WM_NCMOUSEMOVE      = 00a0H
WM_NCLBUTTONDOWN    = 00a1H
WM_NCLBUTTONUP      = 00a2H
WM_NCLBUTTONDBLCLK  = 00a3H
WM_NCRBUTTONDOWN    = 00a4H
WM_NCRBUTTONUP      = 00a5H
WM_NCRBUTTONDBLCLK  = 00a6H

WM_KEYFIRST         = 0100H
WM_KEYDOWN          = 0100H
WM_KEYUP            = 0101H
WM_CHAR             = 0102H
WM_DEADCHAR         = 0103H
WM_SYSKEYDOWN       = 0104H
WM_SYSKEYUP         = 0105H
WM_SYSCHAR          = 0106H
WM_SYSDEADCHAR      = 0107H
WM_KEYLAST          = 0107H

WM_INITDIALOG       = 0110H
WM_COMMAND          = 0111H
WM_SYSCOMMAND       = 0112H
WM_TIMER            = 0113H
WM_HSCROLL          = 0114H
WM_VSCROLL          = 0115H
WM_INITMENU         = 0116H

WM_MOUSEFIRST       = 0200H
WM_MOUSEMOVE        = 0200H     ;  mouse related stuff
WM_LBUTTONDOWN      = 0201H
WM_LBUTTONUP        = 0202H
WM_LBUTTONDBLCLK    = 0203H
WM_RBUTTONDOWN      = 0204H
WM_RBUTTONUP        = 0205H
WM_RBUTTONDBLCLK    = 0206H
WM_MOUSELAST        = 0206H

;  clipboard messages
WM_RENDERFORMAT     = 0300H
WM_RENDERALLFORMATS = 0301H
WM_DESTROYCLIPBOARD = 0302H
WM_DRAWCLIPBOARD    = 0303H

;  private window messages start here
WM_USER             = 0400H
ENDIF           ; NOWM

; Size message commands
SIZENORMAL       = 0
SIZEICONIC       = 1
SIZEFULLSCREEN   = 2
SIZEZOOMSHOW     = 3
SIZEZOOMHIDE     = 4

;  identifiers for the WM_SHOWWINDOW message
SW_PARENTCLOSING =  1
SW_OTHERZOOM     =  2
SW_PARENTOPENING =  3
SW_OTHERUNZOOM   =  4
;
; Key state masks for mouse messages
;
MK_LBUTTON       = 0001h
MK_RBUTTON       = 0002h
MK_SHIFT         = 0004h
MK_CONTROL       = 0010h
;
; Class styles
;
CS_VREDRAW      = 0001h
CS_HREDRAW      = 0002h
CS_KEYCVTWINDOW = 0004H
CS_DBLCLKS      = 0008h
CS_OEMCHARS     = 0010h
CS_OWNDC        = 0020h
CS_CLASSDC      = 0040h
CS_PARENTDC     = 0080h
CS_NOKEYCVT     = 0100h
CS_SAVEBITS     = 0800h
CS_NOCLOSE      = 0200h
CS_BYTEALIGNCLIENT = 1000h
CS_BYTEALIGNWINDOW = 2000h
;
; Windows styles (the high words)
;
WS_TILED        = 0000h
WS_POPUP        = 8000h
WS_CHILD        = 4000h
WS_ICONIC       = 2000h
WS_VISIBLE      = 1000h
WS_DISABLED     = 0800h
WS_CLIPSIBLINGS = 0400h
WS_CLIPCHILDREN = 0200h
WS_BORDER       = 0080h
WS_CAPTION      = 0040h
WS_VSCROLL      = 0020h
WS_HSCROLL      = 0010h
WS_SYSMENU      = 0008h
WS_SIZEBOX      = 0004H
WS_HREDRAW      = 0002h
WS_VREDRAW      = 0001h
WS_TILEDWINDOW  = WS_TILED + WS_BORDER + WS_CAPTION + WS_SYSMENU + WS_SIZEBOX
WS_POPUPWINDOW  = WS_POPUP + WS_BORDER
WS_CHILDWINDOW  = WS_CHILD
;
; predefined clipboard formats
;
CF_TEXT         = 1
CF_BITMAP       = 2
CF_METAFILE     = 3
CF_BINARY       = 4
CF_SYLK         = 5
CF_DIF          = 6
CF_OEMTEXT      = 7

CF_OWNERDISPLAY = 80h       ; owner display
CF_DSPTEXT      = 81h       ; display text
CF_DSPBITMAP    = 82h       ; display bitmap
CF_DSPMETAFILE  = 83h       ; display metafile
;
; Private clipboard format range
;
CF_PRIVATEFIRST       = 200h       ; Anything in this range doesn't
CF_PRIVATELAST        = 2ffh       ; get GlobalFree'd
CF_GDIOBJFIRST        = 300h       ; Anything in this range gets
CF_GDIOBJLAST         = 3ffh       ; DeleteObject'ed

MAKEINTRESOURCE MACRO a
        mov     ax,a
        xor     dx,dx
        ENDM
;
;  Predefined resource types
;
RT_CURSOR       = 1              ; must be passed through MAKEINTRESOURCE
RT_BITMAP       = 2
RT_ICON         = 3
RT_MENU         = 4
RT_DIALOG       = 5
RT_STRING       = 6
RT_FONTDIR      = 7
RT_FONT         = 8
