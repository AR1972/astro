;*
;*      COW : Character Oriented Windows
;*
;*      editmgr1.asm : Multi-line edit manager (part1)
;*      (included by editmgr.asm)


cProc   InternalEditWndProc,<FAR,PUBLIC>,<SI,DI>
        parmW   pwnd
        parmW   message
        parmW   wParam
        parmD   lParam

        localW  oln
        localW  olnT
        localW  ob
        localW  clnT
        localW  ptMouseX
        localW  ptMouseY
        localB  fGrabFocus
        localW  olnFirst
        localW  olnLast
        localW  obFirst
        localW  obLast
        localW  fOk
cBegin
;
; We are not re-entrant!!!
; If we are in the middle of a big edit we could be re-entered
; because the text manager may put up a dialog box.
; If this happens, just ignore the messages
;
        test    [emFlags],EMF_IN_EDITMGR
        jz      NotReEntering
        cmp     [message],WM_PAINT
        jne     @F
ifdef   PROJECT_LQB
        mov     [pdCur.pd_oln],-1       ;edit buffer may be new
endif   ;PROJECT_LQB
        cCall   RefreshScreen           ; We can redraw the screen
@@:
        mov     [fOk],FALSE
        jmp     QuickExitEditMgr
NotReEntering:
        or      [emFlags],EMF_IN_EDITMGR
        mov     [fGrabFocus],FALSE
        mov     [fOk],TRUE

NoUpdate:
        mov     bx,[pwnd]               ;pwnd
        mov     pwndEditCur, bx         ;bx = pwnd
        mov     ax,[bx].pefExtra        ; pwnd->pef = pwnd->pefExtra
        mov     [pefCur],ax
        xor     ax,ax
        mov     al,[bx].arcClipping.axRightArc
        sub     al,[bx].arcClipping.axLeftArc
        mov     [cColumnsCur],ax
        mov     al,[bx].arcClipping.ayBottomArc
        sub     al,[bx].arcClipping.ayTopArc
        mov     [cLinesCur],ax
        mov     bx,[pefCur]
        mov     ax,[bx].EF_attrCur
        mov     [CurattrCur],ax
        mov     al,[bx].EF_Style
        mov     [emState],al
        mov     fResetAnchor,FALSE

        ; !!! change later to a single big move !!!
        mov     ax,[bx.EF_ipCur.ip_ob]  ;transfer insertion to local var.
        mov     dx,[bx.EF_ipCur.ip_oln]
        mov     ipCur.ip_ob, ax
        mov     ipCur.ip_oln, dx
        mov     ipStart.ip_ob, ax       ;local start for text selection
        mov     ipStart.ip_oln, dx

        mov     ax, [bx.EF_ipAnchor.ip_ob];
        mov     dx, [bx.EF_ipAnchor.ip_oln];
        mov     ipAnchor.ip_ob, ax
        mov     ipAnchor.ip_oln, dx
        mov     ipAnchorStart.ip_ob, ax
        mov     ipAnchorStart.ip_oln, dx

        mov     di,OFFSET DGROUP:ldCur  ;get pefCur.ldCur
        mov     si,[bx.EF_pldCur]       ;restore current line descriptor
        push    ds
        pop     es
        movsw
        movsw
        movsw
        movsb

        mov     di,OFFSET DGROUP:pdCur  ;get pefCur.pdCur
        lea     si,[bx.EF_pdCur]
        push    ds
        pop     es
        movsw
        movsw
        movsw

        cCall   EMRefreshCache

;
; Make sure the window is not past the end of the buffer
;
        mov     ax, [clnCur]
        cmp     ax, [pdCur.pd_olntop]
        jae     @F
        mov     [pdCur.pd_olntop], ax
@@:
;
; Make sure the cursor is not past the end of the buffer
;
        cmp     ax, [ipCur.ip_oln]
        jae     @F
        mov     [ipCur.ip_oln], ax
@@:
        cmp     ax, [ipAnchor.ip_oln]
        jae     @F
        mov     [ipAnchor.ip_oln], ax
@@:

        push    ds
        push    [ldCur.ld_prgch]
        call    fstrlen
        mov     bx, [ldCur.ld_cbMax]
        cmp     ax, bx          ; brif ldCur.cb < ldCur.cbMax
        jb      @F
        mov     ax, bx
        dec     ax
@@:
        mov     [ldCur.LD_cb], ax

        mov     [fRefreshScreen],0

; Finished initializing editor state

; Start case statement for message
        mov     ax,[message]    ;message
        ;jump   for primary (message)

;case WM_SETFOCUS
$SetFocus:
        cmp     ax, WM_SETFOCUS
        jne     FlushFocus

        mov     [pdCur.pd_oln],-1       ;edit buffer may be new even
                                        ; though pwndEditCur is same
                                        ; which would cause EditMgr entry
                                        ; to not reset pdCur.pd_oln to -1
;; a-emoryh - Don't screw ES_NOSELECT if in QHelp mode
        test    [cmdSwitches],CMD_SW_QHELP
        jnz     QHelpSkip1
        and     [emState], NOT ES_NOSELECT
QHelpSkip1:

        cCall   RefreshSelection

        mov     ax,1
        cCall   EnableCursor,<[pwndEditCur],ax> ;make sure cursor in on now that we
        test    [emState], ES_MULTILINE
        jnz     NotSingleline

        mov     ax, ldCur.ld_cb         ;for single line make end of line
        mov     ipCur.ip_ob,ax          ;current ip position
        mov     ipAnchor.ip_ob,0        ;Anchor to start so whole line selected

NotSingleline:
        cCall   DisplayCurPos           ;force recal of current postion
        jmp     ExitMainCase

;case   WM_FLUSHFOCUS
FlushFocus:
        cmp     ax, WM_FLUSHFOCUS
        jne     WantFocus

        cCall   UpdateLine
        mov     [pdCur.pd_oln],-1
        jmp     ExitMainCase

;case   WM_WANTFOCUS
WantFocus:
        cmp     ax, WM_WANTFOCUS
        jne     KillFocus

        mov     bx,[pwnd]
        mov     ax, word ptr [bx].stfWnd
        and     ax,MASK fEnabledStf
        mov     [fOk],ax
        jmp     ExitMainCase

;case   WM_KILLFOCUS
KillFocus:
        cmp     ax, WM_KILLFOCUS
        jne     SetReDraw

;; a-emoryh - Don't screw ES_NOSELECT if in QHelp mode
        test    [cmdSwitches],CMD_SW_QHELP
        jnz     QHelpSkip2
        or      [emState], ES_NOSELECT
QHelpSkip2:

        cCall   UpdateLine              ;make sure text buffer is correct
        cCall   RefreshSelection
        test    [emState],EF_TRACKING_MOUSE
        je      NoTrack_1               ;brif not tracking the mouse

        cCall   ReleaseCapture          ;if so make sure others get mouse
        and     [emState],NOT EF_TRACKING_MOUSE

NoTrack_1:
        xor     ax, ax
        cCall   EnableCursor,<[pwndEditCur],ax> ;turn cursor off
        test    [emState], ES_MULTILINE
        jnz     Multiline_2             ;brif multiline edit

        mov     ipCur.ip_ob,0           ;reset position for single line case
        mov     pdCur.pd_obleft,0       ;and offset into line

Multiline_2:
        cCall   DisplayCurPos
        jmp     ExitMainCase

;case   WM_SETREDRAW
SetReDraw:
        cmp     ax, WM_SETREDRAW
        jne     Paint

        mov     cx,[wParam]
        jcxz    NoRedraw

        and     [emState], NOT ES_NOREDRAW

        inc     [fRefreshScreen]
        cCall   DisplayCurPos
        jmp     ExitMainCase
NoRedraw:
        or      [emState], ES_NOREDRAW
        jmp     ExitMainCase

;case   WM_PAINT
Paint:
        cmp     ax, WM_PAINT
        jne     SetText
        mov     ax,[cColumnsCur]
        add     ax,WORD PTR pdCur.pd_obleft
        cmp     ax,ldCur.ld_cbMax
        jb      NoRecalc

        mov     ax,ldCur.ld_cbMax       ;change in border forces recalc of
        sub     ax,[cColumnsCur]
        dec     ax
        mov     WORD PTR pdCur.pd_obleft,ax
        cCall   DisplayCurPos

NoRecalc:
        inc     [fRefreshScreen]
        jmp     ExitMainCase

; case WM_SETTEXT
DEBPUBLIC SetText
SetText:
        cmp     ax, WM_SETTEXT
        jne     GetText

        push    ds                      ; Destination for fstrcpy
        push    [ldCur.LD_prgch]
        push    word ptr [lParam+2]     ; Source for fstrcpy
        push    word ptr [lParam]
        call    fstrcpy

        push    ds
        push    [ldCur.LD_prgch]
        call    fstrlen
        mov     [ldCur.LD_cb],ax

        cCall   GetFocus                ;if Focus is this window
        cmp     ax,[pwndEditCur]        ; then select the line
        jne     @F                      ; otherwise don't
        mov     ax,[ldCur.LD_cb]
        mov     ipCur.ip_ob,ax          ;current ip position
        mov     ipAnchor.ip_ob,0        ;Anchor to start so whole line selected
@@:

        cmp     [wParam],0
        jz      SetNoRefresh
        cCall   DisplayCurPos
        cCall   RefreshLine,<ipCur.IP_oln>
SetNoRefresh:
        jmp     ExitMainCase2

;case   WM_GETTEXT
DEBPUBLIC GetText
GetText:
        cmp     ax,WM_GETTEXT
        je      DoGetText
        jmp     $Undo

DoGetText:
        test    [emState], ES_MULTILINE
        jnz     MultiLineGetText

        mov     ax, [ldCur.LD_cb]
        mov     bx,[wParam]
        dec     bx                              ; ensure enough room for null.
        cmp     ax,bx
        jbe     LenOk
        xchg    ax,bx                           ; ax := wParam
LenOk:
        mov     [fOk],ax                        ; Save for return value

        push    ds                              ; Source for bltbytex
        mov     si,[ldCur.LD_prgch]             ; Source for bltbytex
        mov     es,[seg_lParam]                 ; Destination for bltbytex
        mov     di,[off_lParam]                 ; Destination for bltbytex
        mov     bx,ax                           ; bx = count
        add     bx,di                           ; bx = offset of dest. end
        mov     byte ptr es:[bx],0              ; NULL terminate destination.
        cCall   bltbytex,<ds,si,es,di,ax>
        jmp     ExitMainCase1

MultiLineGetText:
        mov     bx,[pefCur]
        test    [bx.EF_fSelection],0ffH
        jz      NoTextToGet

        lea     ax,[olnFirst]
        lea     bx,[obFirst]
        lea     cx,[olnLast]
        lea     dx,[obLast]
        cCall   BuildSelection,<ax,bx,cx,dx>
; Line 206
        mov     ax,[olnLast]
        cmp     [olnFirst],ax
        jne     NoTextToGet


        push    [olnFirst]
        push    [obFirst]
        push    [obLast]
        push    word ptr [lParam+2]
        push    word ptr [lParam]
        push    [wParam]
        call    CopyChars
        mov     [fOk],ax

        jmp     short ExitGetText

NoTextToGet:
        xor     ax,ax
        les     bx, [lParam]
        mov     byte ptr [bx], 0
        mov     [fOk],ax
ExitGetText:
        jmp     ExitMainCase1

;case   WM_UNDO
$Undo:
        cmp     ax, WM_UNDO
        jne     $Clear
        cCall   Undo
        jmp     ExitMainCase

;case   WM_CLEAR
$Clear:
        cmp     ax, WM_CLEAR
        jne     $Cut

        xor     ax,ax
        jmp     short DoCut

;case   WM_CUT
$Cut:
        cmp     ax, WM_CUT
        jne     $Copy

        mov     ax,1
DoCut:
        mov     bx,[pefCur]
        cmp     [bx.EF_fSelection],0
        je      NoSel
        cCall   Cut,<ax>
        cCall   DisplayCurPos
        jmp     ExitMainCase
NoSel:                  ;with no selection Del key is a delete
        cCall   Del
        jmp     ExitMainCase

;case   WM_COPY
$Copy:
        cmp     ax, WM_COPY
        jne     $Paste
        cCall   Copy
        jmp     ExitMainCase

;case   WM_PASTE
$Paste:
        cmp     ax, WM_PASTE
        jne     ReplaceSel
DoPaste:
        cCall   Paste,<wParam>
        cCall   DisplayCurPos
        jmp     ExitMainCase

;case   EM_REPLACESEL
ReplaceSel:
        cmp     ax, EM_REPLACESEL
        jne     GetLineSel
        cCall   Paste,<wParam>
        cCall   UpdateLine
        jmp     ExitMainCase

;case   EM_GETLINESEL
GetLineSel:
        cmp     ax, EM_GETLINESEL
        jne     GetSel

; Get the current selection (lines)
; wParam points to an array of two words.
; The numbers of the first and the last lines in the selection are stored here.
; Return the number of lines selected.
        lea     ax, [olnFirst]
        lea     bx, [obFirst]
        lea     cx, [olnLast]
        lea     dx, [obLast]
        cCall   BuildSelection,<ax,bx,cx,dx>

        mov     bx, [olnFirst]
        mov     cx, [obLast]
        mov     dx, [olnLast]

        xor     ax,ax
        cmp     bx,dx
        je      yyy2

        dec     dx
        jcxz    yyy1
        inc     dx
yyy1:
        mov     ax,dx
        sub     ax, bx
        inc     ax
        mov     [fOk],ax
yyy2:
        mov     si, [wParam]
        mov     [si], bx
        mov     [si+2], dx
        jmp     ExitMainCase

; case EM_GETSEL
GetSel:
        cmp     ax, EM_GETSEL
        jne     SetSel
        mov     ax,[ipAnchor.ip_ob]
        mov     dx,[ipCur.ip_ob]

        jmp     ExitMainCase

; case EM_SETSEL
DEBPUBLIC SetSel
SetSel:
        cmp     ax, EM_SETSEL
        jne     SelChars
        mov     [wParam],0
        jmp     short StartSel

SelChars:
        cmp     ax, EM_SELCHARS
        jne     DoChar

StartSel:

;; a-emoryh - Don't screw ES_NOSELECT if in QHelp mode
        test    [cmdSwitches],CMD_SW_QHELP
        jnz     QHelpSkip3
        and     [emState], NOT ES_NOSELECT
QHelpSkip3:

        mov     ax,[wParam]
        cmp     ax,[ipCur.ip_oln]
        je      @F
        cCall   UpdateLine
        cCall   NoSelection
        cCall   RefreshLine,<[ipCur.ip_oln]>

; wParm == Line
; HIWORD(lParam) = start column
; LOWORD(lParam) = end column + 1
@@:
        mov     bx, [clnCur]
        mov     ax, [wParam]
        cmp     ax, bx
        jbe     SelChars1
        xchg    ax, bx
SelChars1:
        mov     [ipCur.ip_oln], ax
        mov     [ipAnchor.ip_oln], ax

;
; First make ipAnchor.ob visible.
; then make ipCur.ob visible.
; Note: DisplayCurPos makes ipCur.ob visible, so first set ipCur.ob
;       to what ipAnchor.ob will be, then swap ipCur.ob and ipAnchor.ob
;
        mov     ax, [off_lParam]
        mov     [ipAnchor.ip_ob], ax
        mov     ax, [seg_lParam]
        mov     [ipCur.ip_ob], ax
        cCall   DisplayCurPos
        mov     ax, [ipCur.ip_ob]
        xchg    ax, [ipAnchor.ip_ob]
        xchg    ax, [ipCur.ip_ob]
        cCall   DisplayCurPos

        inc     [fRefreshScreen]
        jmp     ExitMainCase

;case   WM_CHAR
DEBPUBLIC DoChar
DoChar:
        cmp     ax, WM_CHAR
        jne     VScroll

ifdef   KANJI
        cmp     [fCharIsDbcs],0
        jz      CheckCharIsDbcs
        inc     [fCharIsDbcs]
        jmp     SHORT EndCharIsDbcs
CheckCharIsDbcs:
        cCall   FIsDbcsChar,<[wParam]>
        mov     [fCharIsDbcs],al
EndCharIsDbcs:
endif   ; KANJI

        mov     ax,[seg_lParam]
        mov     bx,ax
ifndef  MULTIPLE_ACTION
        test    ax,KK_ALT
        jz      NotMenu
        mov     [fOk],FALSE
        jmp     ExitMainCase
endif   ; !MULTIPLE_ACTION
NotMenu:
        and     ax,KK_SHIFT
        and     bx,KK_CONTROL
        mov     dx,[wParam]

        cmp     dx,VK_MIN
        jae     CHAR_DoKey
        cCall   EditMgrDoChar,<dx,ax>
        jmp     ExitMainCase

CHAR_DoKey:
        cCall   EditMgrDoKey,<dx,ax,bx>
        jmp     ExitMainCase

;case   WM_VSCROLL
VScroll:
        cmp     ax, WM_VSCROLL
        jne     HScroll
;Vertical Scroll case
        ;new case statement on wParam
        mov     ax,[wParam]

;case   SB_LINEUP
VLineUp:
        cmp     ax, SB_LINEUP
        jne     VLineDown
        cCall   ScrollUp
        jmp     ExitVScrollCase

;case   SB_LINEDOWN
VLineDown:
        cmp     ax, SB_LINEDOWN
        jne     VPageUp
        cCall   ScrollDown
        jmp     ExitVScrollCase

;case   SB_PAGEUP
VPageUp:
        cmp     ax, SB_PAGEUP
        jne     VPageDown
        cCall   PageUp
        jmp     ExitVScrollCase


;case   SB_PAGEDOWN
VPageDown:
        cmp     ax, SB_PAGEDOWN
        jne     VThumbPosition
        cCall   PageDown
        jmp     ExitVScrollCase

;case   SB_THUMBPOSITION
VThumbPosition:
        cmp     ax, SB_THUMBPOSITION
        jne     DefaultVScrollCase
        cCall   UpdateLine
        mov     cx, [clnCur]
        mov     ax, word ptr [lParam]
        cmp     cx, ax          ;is new position past end of file?
        ja      @F
        mov     ax, cx
        jcxz    @F
        dec     ax              ;move line position within file range
@@:
        mov     pdCur.pd_olntop,ax
        mov     ipCur.ip_oln,ax
        inc     [fRefreshScreen]
        
ExitVScrollCase:
        mov     fResetAnchor,1
        jmp     ExitMainCase
DefaultVScrollCase:
        jmp     ExitEditMgr     ;Must do this, otherwise we mess up tracking

;End Vertical Scroll inner case

; Line 256
$SC237:
HScroll:
        ;case   WM_HSCROLL
        cmp     ax, WM_HSCROLL
        je      HScroll1
        jmp     LButtonDblClk
HScroll1:               ;out of range!
;New case for Horizonal Scroll
        ;new case statement for (wParam)
        mov     ax,[wParam]

;case   SB_LINEUP
HLineUp:
        cmp     ax, SB_LINEUP
        jne     HLineDown
        mov     ax,1
        cCall   PageLeft,<shiftWidth,ax>
; Line 260
        jmp     ExitHScrollCase

;case   SB_LINEDOWN
HLineDown:
; Line 263
        cmp     ax, SB_LINEDOWN
        jne     HPageUp
        mov     ax,1
        cCall   PageRight,<shiftWidth,ax>
        jmp     ExitHScrollCase

;case   SB_PAGEUP
HPageUp:
        cmp     ax, SB_PAGEUP
        jne     HPageDown
        mov     ax,1
        cCall   PageLeft,<[cColumnsCur],ax>
        jmp     ExitHScrollCase

;case   SB_PAGEDOWN
HPageDown:
        cmp     ax, SB_PAGEDOWN
        jne     HThumbPosition
        mov     ax,1
        cCall   PageRight,<[cColumnsCur],ax>
        jmp     ExitHScrollCase

;case   SB_THUMBPOSITION
HThumbPosition:
        cmp     ax, SB_THUMBPOSITION
        jne     DefaultHScrollCase

        mov     dx,ldCur.ld_cbMax       ;dx = get cbMax (max line size)
        sub     dx,[cColumnsCur]        ;dx = cbMax - cColumns
        mov     ax, word ptr [lParam]   ;lParam (low word) new position
        cmp     ax, dx
        jb      HInRange                ;new col. position out of range?
        mov     ax, dx
HInRange:
        mov     pdCur.pd_obleft,ax
        mov     ipCur.ip_ob,ax
        inc     [fRefreshScreen]

ExitHScrollCase:
        mov     fResetAnchor,1
        jmp     ExitMainCase
DefaultHScrollCase:
        jmp     ExitEditMgr     ;Must do this, otherwise we mess up tracking

;End Horizonal Scroll inner case

;case   WM_LBUTTONDBLCLK
LButtonDblClk:
        cmp     ax, WM_LBUTTONDBLCLK
        jne     LButtonDown
        cCall   GetCurLine
        cCall   obGetFirst      ;ax = ob offset to first non-space char
        
        mov     bx,ipCur.ip_ob  ;bx = column offset for insertion point
        cmp     bx, ax          ;clicking to left of start of line
        jbe     Dbl1            ;ipCur.ob < ob
        cCall   FOnWord         ;if at start of word, no need to move left
        jz      Dbl0
        cCall   FAfterWord
        jz      Dbl2
Dbl0:
        cCall   LeftToWordStart ;if not then move left one word
        jmp     Dbl2            ;   this will adjust ipCur
Dbl1:
        mov     ipCur.ip_ob,ax  ;assume ipCur.ob < ob
Dbl2:
        ;adjust anchor and ipcur to equal i.e. remove text selection
        cCall   ResetAnchor             ;set ipAnchor to ip
        cCall   RightPastWordEnd        ;select word
        mov     ax,ipAnchor.ip_oln      ;
        cmp     ipCur.ip_oln,ax ;was it a multi-line selection?
        je      Dbl3                    ;brif ipCur.oln == ipAnchor.oln
        mov     ipCur.ip_oln,ax
        cCall   GetCurLine
        mov     ax,ldCur.ld_cb          ;ldCur.cb
        mov     ipCur.ip_ob,ax          ;ipCur.ob set ip to end of line
Dbl3:
        cCall   DisplayCurPos
        or      [emState],EF_TRACKING_MOUSE OR EF_WORD_SELECTING OR EF_IN_WINDOW
        cCall   SetCapture,<[pwndEditCur]>
        jmp     ExitMainCase

;case   WM_LBUTTONDOWN
LButtonDown:
        cmp     ax, WM_LBUTTONDOWN
        je      LButtonDown0
        jmp     LButtonUp
LButtonDown0:
        mov     [fGrabFocus],TRUE
;ptMouse.x and ptMouse.y in low word (lParam)
        mov     cx,word ptr [lParam]
        mov     al,cl           ;ptMouse.x in low byte
        cbw
        or      ax,ax           ;Check if negative
        jns     @F              ;If it is
        xor     ax,ax           ; then clear value
@@:
        mov     dx,[cColumnsCur]
        dec     dx
        cmp     ax,dx           ; See if ptMouse.x is out of range
        jna     @F              ; If so
        xchg    ax,dx           ;  then ptMouse.x = cColumnsCur-1
@@:
        mov     [ptMouseX],ax   ;ptMouse.x = rx
        mov     al,ch           ;ptMouse.y in low byte
        cbw
        or      ax,ax           ;Check if negative
        jns     @F              ;If it is
        xor     ax,ax           ; then clear value
@@:
        mov     dx,[cLinesCur]
        dec     dx
        cmp     ax,dx           ; See if ptMouse.y is out of range
        jna     @F              ; If so
        xchg    ax,dx           ;  then ptMouse.y = cLinesCur-1
@@:
        mov     [ptMouseY],ax   ;ptMouse.y = ry

        add     ax,pdCur.pd_olntop      ;ptMouse.y + pdCur.olntop
        mov     bx, ax                  ;save as temp
        mov     ax,ipCur.ip_oln
        cmp     bx,ax                   ;olnT to ipCur.oln
        je      LButtonDown1            ;don't distroy undo unless need be
        push    bx
        cCall   UpdateLine
        pop     bx
        mov     ipCur.ip_oln,bx ;ipCur.oln = ptMouse.y + pdCur.olntop
LButtonDown1:
        mov     ax,[ptMouseX]           ;ptMouse
        add     ax,pdCur.pd_obleft      ;ptMouse.x + pdCur.obleft
        mov     bx, [ldCur.ld_cbMax]
        cmp     ax, bx
        jb      LButtonDown1a
        xchg    ax, bx                  ; really mov ax,bx
        dec     ax
LButtonDown1a:
        mov     ipCur.ip_ob,ax          ;ipCur.ob
        mov     cx,ax                   ; save for later
        mov     ax, [clnCur]
        or      ax,ax
        jz      LButtonDown1c
        cmp     ax,ipCur.ip_oln ;check if of end of file
        ja      LButtonDown2            ;brif (file size) >= ipCur.oln
        jne     LButtonDown1b
        jcxz    LButtonDown1c
LButtonDown1b:
        dec     ax
LButtonDown1c:
        mov     ipCur.ip_oln,ax ;set to bottom of file is so
LButtonDown2:
        mov     al,0                    ;prepare fResetAnchor=!(wParam&MK_SHIFT)
        test    byte ptr [wParam],MK_SHIFT;wParam test if shift key down
        jne     LButtonDown3
        mov     al,1
LButtonDown3:
        mov     fResetAnchor,al
        mov     al,[emState]
        or      al,EF_IN_WINDOW OR EF_TRACKING_MOUSE
        and     al,NOT EF_WORD_SELECTING
        mov     [emState],al
        cCall   SetCapture,<[pwndEditCur]>
        jmp     ExitMainCase

;case   WM_LBUTTONUP
LButtonUp:
        cmp     ax, WM_LBUTTONUP
        jne     MouseMove

DoLButtonUp:
        mov     al,[emState]
        test    al,EF_TRACKING_MOUSE
        jz      LButtonUp1
        and     al,NOT EF_TRACKING_MOUSE
        mov     [emState],al
        cCall   ReleaseCapture
LButtonUp1:
        jmp     ExitMainCase

;case   WM_ALARM
;case   WM_MOUSEMOVE
MouseMove:
        cmp     ax, WM_MOUSEMOVE
        je      MouseMove1
        cmp     ax, WM_ALARM
        je      MouseMove1      
        jmp     CheckGetWord
MouseMove1:
        xor     bx,bx
        xor     dx,dx
;ptMouse.x and ptMouse.y in low word (lParam)
        mov     cx,word ptr [lParam]
        mov     al,cl           ;ptMouse.x in low byte
        cbw
        xchg    dx,ax
        mov     [ptMouseX],dx   ;ptMouse.x = rx
        mov     al,ch           ;ptMouse.y in low byte
        cbw
        xchg    bx,ax
        mov     [ptMouseY],bx   ;ptMouse.y = ry

        ;fInWindow= (ptMouse.x>=0 && ptMouse.x<pwnd->cColumns
        ;&& ptMouse.y>=0 && ptMouse.y < pwnd->cLines)
        ;set flag if mouse motion within window
        ;dx = ptMouse.x, bx = ptMouse.y
        mov     al,[emState]
        mov     cx,[cColumnsCur];cx = pwnd->cColumns
        cmp     dx, 0           ;
        jl      OutOfWindow     ;brif ptMouse.x < 0
        cmp     dx, cx
        jge     OutOfWindow     ;brif ptMouse.x > pwnd->cColumns
        mov     cx, [cLinesCur] ;cx = pwnd->cLines
        cmp     bx, 0
        jl      OutOfWindow     ;brif ptMouse.y < 0
        cmp     bx, cx
        jge     OutOfWindow     ;brif ptMouse.y >= pwnd->cColumns
        or      al,EF_IN_WINDOW
        jmp     InWindow
OutOfWindow:
        and     al,NOT EF_IN_WINDOW
InWindow:
        mov     [emState],al
        test    al,EF_TRACKING_MOUSE
        jnz     InWindow1
        mov     [fOk],False
        jmp     ExitEditMgr     ;brif if not tracking
InWindow1:
        test    [wParam], MK_LBUTTON
        jz      DoLButtonUp
        test    al,EF_IN_WINDOW
        jz      MouseMove9      ;brif if not in window

        ;we are both tracking and within window boundaries
        ;update insertion line position
        ;bx = ptMouse.y from above

        add     bx,pdCur.pd_olntop
        mov     ipCur.ip_oln,bx ;ipCur.oln = pdCur.oln + ptMouse.y
        mov     ax,[ptMouseX]           ;ptMouse.x
        add     ax,pdCur.pd_obleft
        mov     ipCur.ip_ob,ax          ;ipCur.ob = pdCur.obleft + ptMouse.x
        mov     cx,ax

        mov     ax, [clnCur]
        or      ax,ax
        jz      MM_3b
        cmp     ax,ipCur.ip_oln ;is new insertion point off file?
        ja      MouseMove4
        jne     MM_3a
        jcxz    MM_3b
MM_3a:
        dec     ax
MM_3b:
        mov     ipCur.ip_oln,ax
MouseMove4:
        test    [emState],EF_WORD_SELECTING
        jz      MouseMove8              ;brif not selecting a word
        cCall   GetCurLine
        cCall   FOnWord
        jz      MouseMove8

        mov     ax, [ipCur.ip_ob]
        cmp     ax, [ipAnchor.ip_ob]
        je      MouseMove8
        jb      MouseMove5

        cCall   RightPastWordEnd
        jmp     short MouseMove8

MouseMove5:
        cCall   FAfterWord
        jz      MouseMove8

        cCall   LeftToWordStart

MouseMove8:
        cCall   DisplayCurPos
        jmp     ExitMainCase

MouseMove9:
        ;check for scrolling
        cmp     [ptMouseX],0
        jge     NoLeftScroll            ;brif ptMouse.x is not at left border
        mov     ax,1                    ;scroll to left
        cCall   PageLeft,<shiftWidth,ax>
        jmp     CheckAlarm
NoLeftScroll:
        ;check for right border scroll
        mov     ax,[ptMouseX]
        cmp     [cColumnsCur],ax
        ja      NoRightScroll   ;brif ptMouse.x is not to right border
        mov     ax,1
        cCall   PageRight,<shiftWidth,ax>
        jmp     CheckAlarm
NoRightScroll:
        cmp     [ptMouseY],0
        jge     NoUpScroll
        ;now check for top/bottom of window scroll
        cCall   LineUp
        jmp     CheckAlarm
NoUpScroll:
        mov     ax,[ptMouseY]
        cmp     [cLinesCur],ax
        ja      CheckAlarm
        cCall   LineDown
CheckAlarm:
        mov     ax,2
        cCall   SetAlarm,<[pwnd],ax>
; Line 385
        jmp     ExitMainCase

; case EM_GETWORD
CheckGetWord:
        cmp     ax, EM_GETWORD
        jne     NoMsgMatch

        push    word ptr [lParam+2]
        push    word ptr [lParam]
        push    [wParam]
        cCall   EMGetWord
        mov     [fOk],ax
        jmp     ExitEditMgr

NoMsgMatch:
        mov     [fOk],FALSE

ExitMainCase:

        ;end of EditMgr

ExitMainCase1:
        cmp     fResetAnchor,0
        je      $I284

        ;reset insertion point for no selection
        cCall   ResetAnchor             ;set Anchor to ip (remove sel.)
$I284:
        cCall   HiliteSelection
ExitMainCase2:
        cmp     fRefreshScreen,0
        je      $I286
        cCall   RefreshScreen
$I286:
        test    [emState], ES_NOREDRAW
        jz      @F
J_DoneRedraw:
        jmp     DoneRedraw
@@:
        cCall   GetFocus
        cmp     ax,[pwndEditCur]
        jne     J_DoneRedraw
        ;In case cursor has been moved reset to correct position
ifdef   KANJI
        cCall   DbcsAdjCursor
else    ; !KANJI
        mov     ax,[ipCur.ip_ob]
endif   ; KANJI
        sub     ax,[pdCur.pd_obleft]
        mov     cx,[ipCur.ip_oln]
        sub     cx,[pdCur.pd_olntop]
        cCall   MoveCursor,<[pwndEditCur],ax,cx>

        sub     di,di
        mov     si,pefCur
        cmp     [si].ef_pwndScrollV,di  ;Do scroll if neccessary
        je      AdjustHorizontalScroll

        mov     ax,[clnCur]
        or      ax,ax
        jnz     @F
        inc     ax
@@:
        cCall   SetScrollRange,<[si].ef_pwndScrollV, di, ax, di>
        mov     dx,[ipCur.ip_oln]
        cmp     dx,[clnCur]
        ja      AdjustHorizontalScroll
        mov     ax,1
        cCall   SetScrollPos,<[si].ef_pwndScrollV, dx, ax>

AdjustHorizontalScroll:
        cmp     [si].ef_pwndScrollH,di  ;Do scroll if necessary
        je      DoneRedraw

        mov     ax,[ldCur.ld_cbmax]
        sub     ax,[cColumnsCur]
        jbe     DoneRedraw
        cCall   SetScrollRange,<[si].ef_pwndScrollH, di, ax, di>
        mov     ax,1
        cCall   SetScrollPos,<[si].ef_pwndScrollH, [pdCur.pd_obleft], ax>

DoneRedraw:
        mov     bx,[pefCur]
        mov     al,[emState]
        mov     [bx].EF_Style,al

        mov     bx,ldCur.ld_cb
        mov     si,ldCur.ld_prgch
        mov     BYTE PTR [bx][si],0
        ; Reset values for edit field
        mov     bx,[pefCur]
        mov     ax,hBuffer
        mov     [bx].EF_hBuffer,ax
        mov     ax,ipCur.ip_ob
        mov     dx,ipCur.ip_oln
        mov     [bx].EF_ipCur.ip_ob,ax
        mov     [bx].EF_ipCur.ip_oln,dx
        mov     ax,ipAnchor.ip_ob
        mov     dx,ipAnchor.ip_oln
        mov     [bx].EF_ipAnchor.ip_ob,ax
        mov     [bx].EF_ipAnchor.ip_oln,dx
        mov     di,[bx].EF_pldCur
        mov     si,OFFSET DGROUP:ldCur
        push    ds
        pop     es
        movsw
        movsw
        movsw
        movsb
        lea     di,[bx].EF_pdCur
        mov     si,OFFSET DGROUP:pdCur.pd_olntop
        push    ds
        pop     es
        movsw
        movsw
        movsw
;
; See if the cursor has moved.
;
        mov     ax,[ipCur.IP_oln]
        cmp     ax,[ipStart.IP_oln]
        jne     NotifyCursorMoved
        mov     ax,[ipCur.IP_ob]
        cmp     ax,[ipStart.IP_ob]
        je      NoCursorMove
NotifyCursorMoved:
        mov     bx, [pwndEditCur]
        mov     ax, [bx].pwndParent
        or      ax,ax
        jz      NoCursorMove
        push    ax
        mov     ax, WM_COMMAND
        push    ax
        mov     ax, EN_CURSORMOVED
        push    ax
        mov     ax, [bx].idWnd
        push    ax
        push    bx
        cCall   SendMessage
NoCursorMove:

ExitEditMgr:
        and     [emFlags],NOT EMF_IN_EDITMGR
;
; See if we want the focus
;
        test    [fGrabFocus],0ffH
        jz      NoGrabFocus
        cCall   SetFocus,<pwnd>
NoGrabFocus:
QuickExitEditMgr:
        mov     ax,[fOk]
        xor     dx,dx
cEnd

cProc EMRefreshCache,<NEAR>,<SI>
cBegin
        mov     si, [pefCur]
        mov     ax, [si].EF_hBuffer
        mov     [hBuffer], ax
        cCall   LinesInBuf,<ax>
        mov     [clnCur], ax
        mov     bx, [si].ef_pldCur
        mov     ax, [bx].ld_cbMax
        mov     [ldCur.ld_cbMax], ax
        mov     ax, [bx].ld_prgch
        mov     [ldCur.ld_prgch], ax
cEnd

ifdef   EDIT_USE_TABLES

EDITFUNCTION    macro   a
iFn&a   dw      User_EditOFFSET a
        endm

MainEditTable   macro   name
        db      (iFn&name - EditIndexTable)
        endm

SecondEditTable macro   ch,name
        dw      ch
        db      (iFn&name - EditIndexTable)
        endm

KeyTableEntry   macro   key,name
        dw      key
        db      (iFn&name - EditIndexTable)
        endm
else    ; ! EDIT_USE_TABLES

EDITFUNCTION    macro   a
        endm

MainEditTable   macro   name
        dw      User_EditOFFSET name
        endm

SecondEditTable macro   ch,name
        db      ch
        dw      User_EditOFFSET name
        endm

KeyTableEntry   macro   key,name
        dw      key, User_EditOFFSET name
        endm
endif   ; EDIT_USE_TABLES

include editfuns.inc

ifdef   EDIT_USE_TABLES
sEnd    USER_EDIT

sBegin  DATA
LabelW  pEditTbls
pCharTable      dw      dataOFFSET DoCharDispatch
pPrefixTable    dw      dataOFFSET PrefixTable
pVkTable        dw      0
pVkCtrlTable    dw      0

StaticB DoCharDispatch,0,33             ;* Control characters + DEL
StaticW PrefixTable,0,32                ;* Control characters only
StaticW pDoVkDispatch,0                 ;* Pointer to VkDispatch Table

GlobalB SzActionPrim,0,20               ;* Name of action key
GlobalB SzCancelPrim,0,20               ;* Name of cancel key

externW vkActionPrim
externW vkActionSec
externW vkCancelPrim
externW vkCancelSec

sEnd    DATA

externFP PpvAllocCb                     ;* Local memory manager - (used incorrectly)

sBegin  INIT
        assumes CS,INIT
endif   ; EDIT_USE_TABLES

include edittbls.inc

ifdef   EDIT_USE_TABLES
LabelW  pDefaultEditTbls
        dw      initOFFSET KeyDispatch, (CtrlKeyDispatch - KeyDispatch)
        dw      initOFFSET CtrlKeyDispatch, (pDefaultEditTbls - CtrlKeyDispatch)
        dw      initOFFSET CtrlQDispatch, (CtrlKDispatch - CtrlQDispatch)
        dw      initOFFSET CtrlKDispatch, (KeyDispatch - CtrlKDispatch)
        dw      0,0

include editinit.asm

sEnd    INIT

sBegin  USER_EDIT
        assumes CS,User_Edit
endif   ; EDIT_USE_TABLES

cProc EditMgrDoChar,<NEAR,PUBLIC>
        parmW   wParam
        parmW   fShift
cBegin
        mov     ax,[wParam]             ;wParam, ax = character code
        mov     dx,[fShift]             ;* If shift is not down then we
        or      dx,dx                   ;*     need to reset the anchor
        jnz     EMDC_NoResetAnchor      ;*     when we finish this character
        mov     [fResetAnchor],TRUE
EMDC_NoResetAnchor:
ifdef   EDIT_USE_TABLES
else    ; !EDIT_USE_TABLES
        cmp     al, ''                 ;* Hardcoded edit command -- Reset
        je      EMDC_ResetState
endif   ; EDIT_USE_TABLES
        jmp     [EditMgrDoCharState]    ;* Jump to controller for this state

EMDC_InitialState:
ifdef   EDIT_USE_TABLES
        cmp     al, ''                 ;* Hard coded edit command --- DEL
        je      DoDel
        cmp     ax,' '                  ;* Is character >= space?
        jae     DefaultEditMgrDoChar    ;* Yes -- do the default action
        mov     bx,ax                   ;* No -- look up and do the
DoDel2:
        add     bx,pEditTbls[0]         ;* Add offset of Table
        mov     bl,[bx]                 ;* Load the function index
        xor     bh,bh                   ;* Clear high end
        call    EditIndexTable[bx]      ;* for this character
        jmp     ExitEditMgrDoChar

DoDel:
        mov     bx,32                   ;* Last entry in table
        jmp     short   DoDel2          ;* Normal dispatch now

else    ; ! EDIT_USE_TABLES
        cmp     al, ''                 ;* Hard coded edit command --- DEL
        je      Do_DelRight
        cmp     al, ''                 ;* Hard coded edit command --- ESC
        je      DoEsc
        cmp     ax,'Z' - 'A' + 1
        ja      DefaultEditMgrDoChar
        mov     bx,ax
        add     bx,bx
        call    WORD PTR cs:DoCharDispatch[bx]
        jmp     ExitEditMgrDoChar
endif   ; EDIT_USE_TABLES

ifdef   EDIT_USE_TABLES

EMDC_SecondState:
        mov     bx, SecondDispatch
        cCall   DispatchAX
        jmp     short EMDC_ResetState
        

QuoteCharacter:
else    ; !EDIT_USE_TABLES

EMDC_CtrlQState:
        cmp     al, '0'
        jb      EMDC_DispatchCtrlQ
        cmp     al, '3'
        ja      EMDC_DispatchCtrlQ

        cCall   GotoBookmark
        jmp     short EMDC_ResetState

EMDC_DispatchCtrlQ:
        mov     bx, User_EditOFFSET CtrlQDispatch
        cCall   DispatchAL
        jmp     short EMDC_ResetState

EMDC_CtrlKState:
        cmp     al, '0'
        jb      EMDC_ResetState
        cmp     al, '3'
        ja      EMDC_ResetState
        cCall   SetBookmark
        jmp     short EMDC_ResetState


EMDC_CtrlPState:
endif   ; EDIT_USE_TABLES
        cmp     al, 0dH                 ; Carriage return?
        je      EMDC_Beep
        cmp     al, 0aH                 ; Line Feed?
        je      EMDC_Beep
        or      al,al                   ; Null?
        je      EMDC_Beep

DefaultEditMgrDoChar:
        cCall   InsertKey,<wParam>
ifndef  EDIT_USE_TABLES
DoEsc:
endif   ; EDIT_USE_TABLES
        mov     fResetAnchor,1
        jmp     short EMDC_ResetState

EMDC_Beep:
        cCall   NearBeep
        jmp     short EMDC_ResetState

ifndef  EDIT_USE_TABLES
Do_DelRight:
        cCall   Del
endif   ; EDIT_USE_TABLES

EMDC_ResetState:
        cCall   SetEMDC_InitialState

ExitEditMgrDoChar:
cEnd

ifdef   EDIT_USE_TABLES

cProc   ResetState,<NEAR>
cBegin
        cCall   SetEMDC_InitialState
cEnd


cProc   DoEsc,<NEAR>
cBegin
        mov     fResetAnchor,1
        cCall   SetEMDC_InitialState
cEnd

cProc   DoQuoteCharacter,<NEAR>
cBegin
        mov     ax,User_EditOFFSET QuoteCharacter ;* Routine to use on next
        mov     [EditMgrDoCharState],ax         ;* character when recieved
cEnd

cProc   DoPrefixTable,<NEAR>
cBegin
        mov     [chEditMgrState], al            ;* Setup status line display
        mov     bx,ax                           ;* Move into an index register
        shl     bx,1                            ;* Convert to byte offset
        add     bx,pPrefixTable                 ;* .. From the beginning of table
        mov     ax,[bx]                         ;* Load next dispatch table
        mov     [SecondDispatch],ax             ;* And save it away
        mov     ax,User_EditOFFSET EMDC_SecondState ;* Secondary dispatch routine
        mov     [EditMgrDoCharState],ax         ;*
        cCall   DrawToggles                     ;* Update status line
cEnd

cProc   DoVkPrefixTable,<NEAR>
cBegin
        mov     bx,pDoVkDispatch                ;* Set of dispatch tables
@@:     
        cmp     word ptr [bx],0                 ;* End of table?
        je      DVPT_01                         ;* Yes - Fail

        cmp     ax,[bx]                         ;* Match?
        je      @F                              ;* Yes dispatch routine
        add     bx,4                            ;* Move to next table entry
        jmp     short @B

@@:
        mov     [chEditMgrState], al            ;* Setup status line display
        mov     ax,[bx]                         ;* Get Table Pointer
        mov     [SecondDispatch],ax             ;* Load next dispatch table
        mov     ax,User_EditOFFSET EMDC_SecondState ;* Secondary dispatch routine
        mov     [EditMgrDoCharState],ax         ;*
        cCall   DrawToggles                     ;* Update status line
DVPT_02:
cEnd

DVPT_01:
        cCall   NearBeep                        ;* Complain
        jmp     short DVPT_02                   ;*

else    ; !EDIT_USE_TABLES
cProc CtrlPState,<NEAR>
cBegin
ifndef  KANJI
        mov     [chEditMgrState], al            ;* Setup status line display
        mov     ax,User_EditOFFSET EMDC_CtrlPState  ;* Secondary dispatch routine
        mov     [EditMgrDoCharState],ax         ;*
        cCall   DrawToggles                     ;* Update status line
endif   ; !KANJI
cEnd

cProc CtrlKState,<NEAR>
cBegin
        mov     [chEditMgrState], al
        mov     ax, User_EditOFFSET EMDC_CtrlKState
        mov     [EditMgrDoCharState], ax
        cCall   DrawToggles
cEnd

cProc CtrlQState,<NEAR>
cBegin
        mov     [chEditMgrState], al
        mov     ax, User_EditOFFSET EMDC_CtrlQState
        mov     [EditMgrDoCharState], ax
        cCall   DrawToggles
        mov     [fResetAnchor], 0
cEnd

endif   ; EDIT_USE_TABLES

cProc GetEditMgrState,<FAR,PUBLIC>
cBegin
        mov     al, [chEditMgrState]
        xor     ah,ah
cEnd

cProc IgnoreChar,<NEAR>
cBegin
        mov     fResetAnchor,0
        cCall   SetEMDC_InitialState
cEnd

cProc SetEMDC_InitialState,<NEAR>
cBegin

        xor     al,al
        mov     [chEditMgrState], al
        mov     ax, User_EditOFFSET EMDC_InitialState
        mov     [EditMgrDoCharState], ax
        cCall   DrawToggles
cEnd

cProc DispatchFind,<NEAR>
cBegin
        mov     cx, WM_SEARCHFIND
        jmp     short SendParentMsg
cEnd <NOGEN>

cProc DispatchChange,<NEAR>
cBegin
        mov     cx, WM_SEARCHCHANGE

SendParentMsg:
        xor     ax,ax

SendParentMsgWithWParam:
        xor     dx,dx
        mov     bx, [pwndEditCur]
        cCall   PostMessage,<[bx.pwndParent], cx, ax, dx, dx>
        mov     [fResetAnchor], 0
cEnd


;*******************************************************************************
;SetBookmark - Set bookmark to current cursor position
;
;Purpose:
;Entry:
;       AL - Which bookmark to set
;            Ascii number '0' through '3'
;Exit:
;Uses:
;Exceptions:
;*******************************************************************************

ifdef   EDIT_USE_TABLES
SetBookmark0:
        mov     al,'0'
        jmp     short SetBookmark

SetBookmark1:
        mov     al,'1'
        jmp     short SetBookmark

SetBookmark2:
        mov     al,'2'
        jmp     short SetBookmark

SetBookmark3:
        mov     al,'3'
;       jmp     short SetBookmark
endif   ; EDIT_USE_TABLES

cProc SetBookmark,<NEAR>
cBegin
        mov     cx, WM_SETBOOKMARK
        xor     ah,ah
        jmp     short SendParentMsgWithWParam
cEnd <NOGEN>

;*******************************************************************************
;GotoBookmark - Moves cursor to previously set bookmark
;
;Purpose:
;Entry:
;       AL - Which bookmark to goto
;            Ascii number '0' through '3'
;Exit:
;Uses:
;Exceptions:
;*******************************************************************************

ifdef   EDIT_USE_TABLES
GotoBookmark0:
        mov     al,'0'
        jmp     short GotoBookmark

GotoBookmark1:
        mov     al,'1'
        jmp     short GotoBookmark

GotoBookmark2:
        mov     al,'2'
        jmp     short GotoBookmark

GotoBookmark3:
        mov     al,'3'
;       jmp     short GotoBookmark
endif   ; EDIT_USE_TABLES

cProc GotoBookmark,<NEAR>
cBegin
        mov     cx, WM_GOTOBOOKMARK
        xor     ah,ah
        jmp     short SendParentMsgWithWParam
cEnd <NOGEN>

ifdef   EDIT_USE_TABLES
;*******************************************************************************
;SearchNext - Send a WM_SEARCHNEXT message to the parent window to continue
;       a search previously started
;
;ENTRY
;Exit:
;Uses:
;Exceptions:
;*******************************************************************************

cProc   SearchNext,<NEAR>
cBegin
        mov     cx,WM_SEARCHNEXT
        xor     ax,ax
        jmp     short SendParentMsgWithWParam
cEnd

;*******************************************************************************
;MatchBrace - Send a WM_MATCHBRACE message to the parent window
;
;ENTRY
;Exit:
;Uses:
;Exceptions:
;*******************************************************************************

cProc   MatchBrace,<NEAR>
cBegin
        mov     cx,WM_MATCHBRACE
        xor     ax,ax
        jmp     short SendParentMsgWithWParam
cEnd
endif   ; EDIT_USE_TABLES

;*******************************************************************************
;DispatchAX - Dispatch to a routine based on AX and table.
;
;Purpose:
;Entry:
;       AX - Value to dispatch on
;       DS:BX - points to table
;            Table is word pairs (match, routine)
;       All other registers are passed to the routine unchanged.
;Exit:
;Uses:
;Exceptions:
;*******************************************************************************
cProc DispatchAX,<NEAR,PUBLIC>
cBegin
TD_1:
ifdef   EDIT_USE_TABLES
        cmp     word ptr [bx],0                 ; End of table?
        je      TD_2                            ; Yes - Dispatch default routine

        cmp     ax, [bx]                        ; Match?
        je      TD_2                            ; Yes - Dispatch routine.

        add     bx, 3                           ; Move to next table entry
        jmp     short TD_1
else    ; !EDIT_USE_TABLES
        cmp     word ptr cs:[bx],0              ; End of table?
        je      TD_2                            ; Yes - Dispatch default routine

        cmp     ax, cs:[bx]                     ; Match?
        je      TD_2                            ; Yes - Dispatch routine.
        add     bx, 4                           ; Move to next table entry
        jmp     short TD_1
endif   ; EDIT_USE_TABLES

TD_2:
ifdef   EDIT_USE_TABLES
        mov     bl,BYTE PTR [bx+2]              ;* Get Function Index
        xor     bh,bh                           ;* Clear hi byte
        call    EditIndexTable[bx]              ;* Call function
else    ; !EDIT_USE_TABLES
        call    cs:[bx+2]
endif   ; EDIT_USE_TABLES
cEnd

ifndef  EDIT_USE_TABLES
;*******************************************************************************
;DispatchAL - Dispatch to a routine based on AL and table.
;
;Purpose:
;Entry:
;       AL - Value to dispatch on
;       CS:BX - points to table
;            Table is word pairs (match, routine)
;       All other registers are passed to the routine unchanged.
;Exit:
;Uses:
;Exceptions:
;*******************************************************************************
cProc DispatchAL,<NEAR,PUBLIC>
cBegin
DAL_1:
        cmp     byte ptr cs:[bx],0              ; End of table?
        je      DAL_2                           ; Yes - Dispatch default routine

        cmp     al, cs:[bx]                     ; Match?
        je      DAL_2                           ; Yes - Dispatch routine.

        add     bx, 3                           ; Move to next table entry
        jmp     short DAL_1

DAL_2:
        call    cs:[bx+1]
cEnd
endif   ; !EDIT_USE_TABLES

cProc EditMgrDoKey,<NEAR,PUBLIC>
        parmW   wParam
        parmW   fShift
        parmW   fCtrlKey

cBegin
        mov     dx,[fShift]
        or      dx,dx
        jnz     DoKeyNoResetAnchor
        mov     [fResetAnchor],TRUE
DoKeyNoResetAnchor:

ifdef   EDIT_USE_TABLES
        mov     bx, 0                           ; Assume Ctrl not down
        cmp     [fCtrlKey],0
        je      DoKeyNoCtrl                     ;brif control key is not down
        add     bx, 2                           ;If Control Key down!

DoKeyNoCtrl:
        mov     bx,pVkTable[bx]
else    ; !EDIT_USE_TABLES
        mov     bx, User_EditOFFSET KeyDispatch ; Assume Ctrl not down
        cmp     [fCtrlKey],0
        je      DoKeyDispatch                   ;brif control key is not down

        mov     bx, User_EditOFFSET CtrlKeyDispatch ;If Control Key down!

DoKeyDispatch:
endif   ; EDIT_USE_TABLES

        push    bx                              ;?????
        mov     cx, [cColumnsCur]
        pop     bx                              ;?????
        mov     ax, [wParam]
        cCall   DispatchAX

ExitEditMgrDoKey:
cEnd

cProc IgnoreKey,<NEAR>
cBegin
        mov     [fResetAnchor], 0
        mov     [fOk],FALSE
cEnd

cProc FullPageLeft,<NEAR>
cBegin
        mov     ax, 1
        cCall   PageLeft,<cx,ax>
cEnd

cProc FullPageRight,<NEAR>
cBegin
        mov     ax, 1
        cCall   PageRight,<cx,ax>
cEnd


;*******************************************************************************
;_HiliteSelection
;
;Purpose:
;       The current selection region defined by the delta for ipCur and
;       ipAnchor is rewritten to reflect and changes since entering the
;       EditMgr. The initial setting is in ipCurStart and ipAnchorStart.
;Entry:
;       ipCur - Current insertion point
;       ipAnchor - End of selection region if any
;       pefCur.ef_fSelection - flag if selection is know to be active
;       ?? under what conditions can this be false a selection is present
;Exit:
;       pefCur.ef_fSelection set to selection state.
;Uses:
;       cl - fSelection - selection flag at start of call
;       ax, dx, cx
;Exceptions:
;       none
;       
;*******************************************************************************
cProc HiliteSelection,<NEAR,PUBLIC>
cBegin
        mov     bx,pefCur
        mov     cl,[bx.ef_fSelection]

        mov     ax, ipCur.ip_oln        ;check for any current selection
        mov     dx, ipCur.ip_ob
        xor     ax, ipAnchor.ip_oln
        xor     dx, ipAnchor.ip_ob
        or      ax, dx
        or      al, ah

;al = state of current selection (ef_fSelection)
;bx = pefCur
;cl = state of selection to start with
        mov     [bx.ef_fSelection],al   ;note state of selection
        or      al, cl
        jz      Hilite16                ; There was no selection, and there
                                        ; is no selection so do nothing

;Here if either fWasSelection or current have a selection
Hilite8:
        ;Check if our current position has moved from the starting position
        mov     ax, ipStart.ip_oln
        mov     cx, ipCur.ip_ob

        cmp     ipCur.ip_oln, ax
        jne     Hilite10                ;brif move off of starting line

        cmp     ipStart.ip_ob, cx
        je      Hilite12                ;brif have not moved off start offset

;Here if current position is not the same as the starting position
Hilite10:
        cCall   RefreshLines,<ax,ipCur.ip_oln>

;Here if have selection but the start and current points were equal
Hilite12:
        mov     ax, ipAnchorStart.ip_oln
        mov     cx, ipAnchor.ip_ob

        cmp     ipAnchor.ip_oln,ax
        jne     Hilite14                        ;brif Anchor has moved

        cmp     ipAnchorStart.ip_ob, cx
        je      Hilite16                        ;brif Anchor has not moved

;Here if Anchor has moved from it's starting position
Hilite14:
        cCall   RefreshSelection

Hilite16:
cEnd

cProc RefreshSelection,<NEAR,PUBLIC>
cBegin
        cCall   RefreshLines,<[ipAnchorStart.ip_oln],[ipStart.ip_oln]>
cEnd

;*******************************************************************************
;_NoSelection
;
;Purpose:
;    Removes any selection present on screen and resets ipAnchor to ipCu
;Entry:
;       ipCur
;       ipAnchor
;Exit:
;       ipAnchor set to ipCur
;Uses:
;       none.
;Exceptions:
;       none
;*******************************************************************************
cProc NoSelection,<NEAR,PUBLIC>
cBegin
        cCall   ResetAnchor
        cCall   HiliteSelection
cEnd

;*******************************************************************************
;_BuildSelection
;
;Purpose:
;       Takes the current selection region defined by ipAnchor and ipCur and
;       returns an ordered pair of positions (ob, oln) that defines the
;       current selected region.
;Entry:
;       Parameters
;       polnFirst - pointer to lowest oln for selected region
;       pobFirst  - pointer to left most ob for selected region
;       polnLast  - pointer to highest oln
;       pobLast   - pointer to right most ob
;
;       register di = polnLast
;       register si = polnFirst
;
;Exit:
;       
;Uses:
;      ax, bx, cx, dx, si, di
;Exceptions:
;       none
;*******************************************************************************
cProc BuildSelection,<NEAR,PUBLIC>,<SI,DI>
        parmDP  polnFirst
        parmDP  pobFirst
        parmDP  polnLast
        parmDP  pobLast
cBegin

        ;assume multi-line selection
        mov     ax,[ipCur.ip_oln]
        mov     bx,[ipCur.ip_ob]
        mov     cx,[ipAnchor.ip_oln]
        mov     dx,[ipAnchor.ip_ob]
        cmp     ax, cx                  ;check if multiline
        jne     BuildSel10

;Single line case
        cmp     bx, dx
        jb      BuildAssign             ;brif ip < anchor
        xchg    bx, dx                  ;reverse ob's
        jmp     SHORT BuildAssign

;Here for multi-line case (ip.oln <> anchor.oln)
BuildSel10:
        cmp     ax, cx
        jb      BuildAssign             ;brif ip < anchor
        xchg    ax, cx
        xchg    bx, dx

BuildAssign:
        mov     si,[pobFirst]
        mov     di,[pobLast]
        mov     [si],bx
        mov     [di],dx
        mov     si,[polnFirst]
        mov     di,[polnLast]
        mov     [si],ax
        mov     [di],cx

;Check that selection is not off end of file
        mov     ax, [clnCur]            ;ax = last line number in file
        mov     bx, [si]                ;bx = olnFirst
        mov     cx, [di]                ;cx = olnLast
        cmp     bx, ax
        jbe     BuildSel20              ;brif first <= end of file
        mov     [si], ax                ;set first to end of file

BuildSel20:
        cmp     cx, ax
        jbe     BuildSel22              ;brif last <= end of file
        mov     [di], ax

BuildSel22:
        xor     dx, dx
        cmp     cx, ax  
        jne     BuildSel24              ;brif last <> end of file
        mov     di, [pobFirst]
        mov     [di], dx                ;at bottom make start of sel. at 0

BuildSel24:
        cmp     bx, ax
        jne     BuildSel26              ;brif first <> end of file
        mov     si, [pobLast]
        mov     [si], dx

BuildSel26:

cEnd

;*******************************************************************************
;fCheckReadOnly
;
;Purpose:
; Check if the buffer is read only, and sound alarm if it is
;Entry:
;Exit:
; Returns TRUE if the buffer is read only.
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc fCheckReadOnly,<NEAR,PUBLIC>
cBegin
        xor     ax,ax
        test    [emState], ES_MULTILINE
        jz      ExitCheckRO                             ; return FALSE

        cCall   fReadOnlyBuf,<[hBuffer]>
        or      ax,ax
        jz      ExitCheckRO                             ; return FALSE
        push    ax
        cCall   NearBeep
        pop     ax
ExitCheckRO:
        or      ax,ax
cEnd

;*******************************************************************************
;_ModifyLine
;
;Purpose:
;       Fetch the current line at ipCur.oln into the line descripter ldCur.
;       Mark the line in ldCur as dirty. This will cause UpdateLine
;       to replace the line in the buffer when called.
;Entry:
;       ldCur
;Exit:
;       ldCur.ld_fDirty - set to TRUE
;Uses:
;       none.
;Exceptions:
;
;*******************************************************************************

cProc ModifyLine,<NEAR,PUBLIC>
cBegin
        cCall   GetCurLine
        cCall   fCheckReadOnly
        jz      DoModifyLine
        xor     ax,ax
        jmp     SHORT ExitModifyLine
DoModifyLine:
        or      [ldCur.ld_flags],LD_fDirty
        inc     ax
ExitModifyLine:
cEnd

;*******************************************************************************
;cbGetLine
;
;Purpose:
;  Same as cbGetLineBuf followed by EMRefreshCache
;Entry:
;Exit:
;Uses:
;Exceptions:
;*******************************************************************************
cProc cbGetLine,<NEAR,PUBLIC>
        parmW   oln
        parmW   cbMax
        parmDP  prgch
cBegin
        cCall   cbGetLineBuf,<[hBuffer], [oln], [cbMax], [prgch]>
        push    ax                      ; save return value
        cCall   EMRefreshCache
        pop     ax                      ; restore return value
cEnd

;*******************************************************************************
;_GetLine
;
;Purpose:
;       Get the specified line offset (ipCur.oln) into ldCur. If it is already
;       in ldCur then do nothing. If in multi-line mode then don't get anything
;Entry:
;       fMultiline - flag if in multi-line or single-line mode
;       pdCur.oln - line offset currently in ldCur
;       pefCur.ef_fFocus - flag if edit field has focus
;       hBuffer - handle to source buffer
;Exit:
;       ldCur
;Uses:
;       oln - line offset to fetch
;       register si - oln
;       bx
;Exceptions:
;       none
;*******************************************************************************
cProc   GetLine,<NEAR,PUBLIC>,<SI>
        parmW   oln
cBegin
        mov     si,[oln]                ;oln
        test    [emState], ES_MULTILINE ;If not multi-line nothing to get
        jz      $I405                   ;brif not multi-line

        mov     ax,si                   ;si - oln (line to get)
        cmp     ax,pdCur.pd_oln         ;check line already in ldCur
        jne     $I406                   ;brif if line no current line

        cCall   GetFocus                ; Current Focus in ax
        cmp     ax,[pwndEditCur]
        je      $I405                   ;brif we do have focus

$I406:
        cCall   UpdateLine              ;make sure current line is back is src.
        mov     ax, [clnCur]
        cmp     ax,si                   ;si - current line
        jne     $I407                   ;brif not at end of file

        cCall   fCheckReadOnly          ;if read only
        jnz     $I405                   ; then don't insert line
        ;for get at end of file (1 past last line) we insert a blank line
        ;so that a new line is added to end of file
        sub     ax,ax
        cCall   InsertLineBuf,<hBuffer,si,ax,ldCur.ld_prgch>
        cCall   EMRefreshCache
        mov     ldCur.ld_cb,0           ;make line is zero width

;Here if line is not 1 past last line or eof
$I407:
        cCall   cbGetLine,<si,ldCur.ld_cbMax,ldCur.ld_prgch>
        mov     ldCur.ld_cb,ax

;Here after either inserting line or get line
$I408:
        mov     pdCur.pd_oln,si ;make it official line in ldCur
$I405:
cEnd

;*******************************************************************************
;_GetCurLine
;
;Purpose:
;       Fetch the current line offset (ipCur.oln) into ldCur.
;       Same as GetLine but line offset is not specified.
;Entry:
;       ipCur
;Exit:
;       ldCur
;Uses:
;       none.
;Exceptions:
;       none
;*******************************************************************************
cProc GetCurLine,<NEAR>
cBegin
        cCall   GetLine,<ipCur.ip_oln>
cEnd

;*******************************************************************************
;GetLineScratch
;
;Purpose:
;       Like GetLine, but into ldEMScratch
;Entry:
;       oln
;Exit:
;       ldEMScratch
;Uses:
;Exceptions:
;*******************************************************************************
cProc GetLineScratch,<NEAR>
        parmW   oln
cBegin
        xor     ax, ax
        mov     [ldEMScratch.LD_cb],ax
        mov     ax, [oln]
        cmp     ax, [clnCur]
        ja      GLS_Exit

        test    [emState], ES_MULTILINE
        jz      GLS1

        test    [ldCur.ld_flags], LD_fDirty
        jz      GLS2

        cmp     ax, [pdCur.pd_oln]
        jne     GLS2

GLS1:
        mov     ax, [ldCur.ld_cb]
        mov     [ldEMScratch.ld_cb], ax
        inc     ax
ifdef   PROJECT_QB
        push    [ldCur.ld_prgch]
        push    [ldEMScratch.ld_prgch]
        push    ax
        call    bltbyte                         ; pascal Calling convention
else    ;PROJECT_QB
        push    ax
        push    [ldCur.ld_prgch]
        push    [ldEMScratch.ld_prgch]
        call    _memmove                         ; C Calling convention
        add     sp, 6
endif   ;PROJECT_QB

        mov     ax, [ldCur.ld_flags]
        mov     [ldEMScratch.ld_flags], ax
        jmp     short GLS_Exit

GLS2:
        xor     bx, bx
        cmp     ax, [clnCur]
        xchg    ax,bx
        jae     GLS3

        cCall   cbGetLine,<bx,[ldEMScratch.ld_cbMax],[ldEMScratch.ld_prgch]>

GLS3:
        mov     [ldEMScratch.ld_cb], ax
        xor     ax,ax
        mov     [ldEMScratch.ld_flags],ax

GLS_Exit:
cEnd

cProc GetLineScratchPad,<NEAR>
        parmW   oln
cBegin
        cCall   GetLineScratch,<[oln]>

;
; Pad line with spaces
;
        mov     ax,[ldEMScratch.LD_cbMax]
        sub     ax,[ldEMScratch.LD_cb]
        push    ax
        mov     ax,' '
        push    ax
        mov     ax,[ldEMScratch.LD_cb]
        add     ax,[ldEMScratch.LD_prgch]
        push    ax
        call    _memset
        add     sp,6
cEnd

;*******************************************************************************
;_UpdateLine
;
;Purpose:
;       Use ldCur to replace the current buffer line. The line offset is
;       in pdCur.oln. Line is only replace if ldCur is marked as Dirty.
;       If out of memory to replace line old line is refetched and update
;       fails. Text manager will report error.
;Entry:
;       ldCur
;       pdCur
;       hBuffer
;Exit:
;       none
;Uses:
;       ax
;Exceptions:
;       none
;*******************************************************************************
cProc UpdateLine,<NEAR,PUBLIC>
cBegin

        mov     ax, 1
        push    ax                      ; Default Return value of TRUE

        test    ldCur.ld_flags,ld_fDirty
        je      $I413                   ;brif current line not modified

        mov     ax, [clnCur]
        cmp     ax,pdCur.pd_oln ;only happens for out of memory
        jbe     $I413                   ;brif if current ld past eof

        pop     ax                      ; Throw away default return value

        and     ldCur.ld_flags, not ld_fDirty   ;current line no longer dirty
        cCall   ReplaceLineBuf,<hBuffer,pdCur.pd_oln,ldCur.ld_cb,ldCur.ld_prgch>

        push    ax                      ; Save return value
        cCall   EMRefreshCache

        mov     ax,-1
        xchg    ax,[pdCur.PD_oln]
        cCall   RefreshLine,<ax>

        mov     [pdCur.pd_oln], -1
$I413:
        pop     ax                      ; restore return value.
        or      ax,ax
cEnd

cProc MaybeToggleInsertMode,<NEAR>
cBegin
        or      dl,dl
        jnz     MTIM_Exit

        cCall   ToggleInsertMode
MTIM_Exit:
cEnd

cProc ToggleInsertMode,<NEAR>
cBegin
        xor     ax,ax
        xor     cx,cx
        mov     [fResetAnchor],al
        inc     ax
        mov     cl,[fInsertMode]
        xor     ax,cx
        mov     [fInsertMode],al
        cCall   SetCursorBlock,<cx>
cEnd

;*******************************************************************************
; JoinLine - merge two consecative lines
; Description:
;       Merge lines at ip and previous line. The current line is copied into
;       a temperary buffer. The previous line is fetched. A check is made to
;       see if the current line ipCur.oln and the previous line will fit into
;       a single buffer (ldCur.cbMax). If so then the temperary buffer is copied
;       into ldCur.prgch after the previous line. The current line is then
;       deleted.
;
;       Checks are made to see if any special updating is needed such as
;       paging left or right.
;
;       Should be called when backspace at start of line.
;
; Input:
;       fBelow - TRUE  Join ipCur.oln with ipCur.oln + 1
;                               FALSE Join ipCur.oln with ipCur.oln - 1;
; Output:
;
;*******************************************************************************
cProc JoinLine,<NEAR,PUBLIC>,<SI,DI>
        parmB   fBelow

        localW  saveIpOln
        localW  saveIpOb
cBegin
        test    [emState], ES_MULTILINE
        jnz     JL2

JL1:
        cCall   NearBeep
J_JL_Exit:
        jmp     JL_Exit

JL2:
        cCall   fCheckReadOnly
        jnz     J_JL_Exit
;
; Save ipCur in case we have to restore it.
;
        mov     ax,[ipCur.IP_oln]
        mov     [saveIpOln],ax
        mov     ax,[ipCur.IP_ob]
        mov     [saveIpOb],ax

;
; Get the top line into ldCur
; Get the bottom line into ldEMScratch.
; Warning: Be very carefull here. ldCur.fDirty may be true
;          but we don't want to update the current line yet.
;
        cCall   GetCurLine

        cmp     [fBelow],0      ;fBelow
        je      JL3

        mov     ax,[ipCur.IP_oln]
        inc     ax
        cCall   GetLineScratch,<ax>

        mov     ax, [ldEMScratch.ld_cb]
        inc     ax                              ; error if ldEMScratch.cb == -1
        jnz     JL4
        jmp     SHORT JL1
JL3:

ifdef   PROJECT_QB
        push    [ldCur.LD_prgch]
        push    [ldEMScratch.LD_prgch]
        push    [ldCur.LD_cb]
        call    bltbyte                         ; pascal Calling convention
else    ;PROJECT_QB
        push    [ldCur.LD_cb]
        push    [ldCur.LD_prgch]
        push    [ldEMScratch.LD_prgch]
        call    _memmove
        add     sp,6
endif   ;PROJECT_QB

        mov     ax,[ldCur.LD_cb]
        mov     [ldEMScratch.LD_cb],ax

        dec     [ipCur.IP_oln]

        cCall   cbGetLine,<ipCur.ip_oln,ldCur.ld_cbMax,ldCur.ld_prgch>
        mov     [ldCur.LD_cb],ax

JL4:
;
; Pad the top line to the cursor position.
;
        mov     ax, [ipCur.IP_ob]
        sub     ax, [ldCur.LD_cb]
        jbe     JL4a

        push    ax                              ; cb - for memset

        mov     ax, ' '
        push    ax                              ; ch - for memset

        mov     ax, [ldCur.LD_prgch]
        add     ax, [ldCur.LD_cb]
        push    ax                              ; pb - for memset
        call    _memset                         ; C calling convention
        add     sp, 6

        mov     ax, [ipCur.IP_ob]
        mov     [ldCur.LD_cb], ax

JL4a:
;
; Move ip to end of the top line.
;
        mov     ax,[ldCur.LD_cb]
        mov     [ipCur.IP_ob],ax


;
; Get rid of leading spaces in the bottom line.
;
; UNDONE - This can be optimised with scasb
        mov     si,[ldEMScratch.LD_cb]
        sub     di,di
        mov     bx,[ldEMScratch.LD_prgch]

JL5:
        or      si,si
        jz      JL6

        cmp     BYTE PTR [bx][di],' '
        jne     JL6

        inc     di
        dec     si
        jmp     short JL5
JL6:

;
; Check if both lines will fit on the max line
;
        mov     ax,si
        add     ax,[ldCur.LD_cb]
        mov     cx,[ldCur.LD_cbMax]
        dec     cx
        cmp     ax,cx
        jbe     JL8

;
; If fBelow then restore ldCur from ldEMScratch
;
        cmp     [fBelow],0
        jne     JL7

ifdef   PROJECT_QB
        push    [ldEMScratch.LD_prgch]
        push    [ldCur.LD_prgch]
        push    [ldEMScratch.LD_cb]
        call    bltbyte                         ; pascal Calling convention
else    ;PROJECT_QB
        push    [ldEMScratch.LD_cb]
        push    [ldEMScratch.LD_prgch]
        push    [ldCur.LD_prgch]
        call    _memmove
        add     sp,6
endif   ;PROJECT_QB

        mov     ax,[ldEMScratch.LD_cb]
        mov     [ldCur.LD_cb],ax

JL7:
;
; Restore ipCur
;
        mov     ax,[saveIpOln]
        mov     [ipCur.IP_oln],ax
        mov     ax,[saveIpOb]
        mov     [ipCur.IP_ob],ax
        jmp     JL1

;
; Append the bottom line to the top line.
;
JL8:
        or      si,si
        je      JL9

ifdef   PROJECT_QB
        mov     ax,[ldEMScratch.LD_prgch]
        add     ax,di
        push    ax
        mov     ax,[ldCur.LD_cb]
        add     ax,[ldCur.LD_prgch]
        push    ax
        push    si
        call    bltbyte                         ; pascal Calling convention
else    ;PROJECT_QB
        push    si
        mov     ax,[ldEMScratch.LD_prgch]
        add     ax,di
        push    ax
        mov     ax,[ldCur.LD_cb]
        add     ax,[ldCur.LD_prgch]
        push    ax
        call    _memmove
        add     sp,6
endif   ;PROJECT_QB

JL9:
        add     [ldCur.LD_cb],si

        cCall   StartBigEdit

        mov     ax,2
        cCall   DeleteLinesBuf,<hBuffer,ipCur.ip_oln,ax>
        cCall   EMRefreshCache

        cCall   InsertLineBuf,<hBuffer,ipCur.ip_oln,ldCur.ld_cb,ldCur.ld_prgch>
        cCall   EMRefreshCache

        and     [ldCur.ld_flags], not ld_fDirty
        mov     [pdCur.PD_oln],-1

        cCall   EndBigEdit
        cCall   EMRefreshCache

;
; Determine what has to be redrawn
;
        cCall   NoSelection
JL10:
        cCall   DisplayCurPos
JL11:
        cmp     fRefreshScreen,0
        jne     JL_Exit

        mov     ax,-1
        cCall   RefreshLines,<ipCur.ip_oln,ax>
JL_Exit:
cEnd


;*******************************************************************************
; DoNewLine - Create a new line
; Description:
;       Create a new line of text at ip and display. The current line is
;       terminated at ipCur.ob.  If ipCur.ob is past the end of the line
;       the line is spaced filled out to ipCur.ob. If their was remaining
;       text on the line such as for a line split the truncated line is
;       inserted for the current line and a new line created consisting
;       of the remaining fragment.
;
;       Should be called on carriage return.
; Input:
;
;
; Output:
;
;*******************************************************************************
cProc DoNewLine,<NEAR,PUBLIC>,<SI>
        parmB   fMoveCursor

        localW  obFirst
        localW  obSave
        localW  olnSave
cBegin
        cCall   fCheckReadOnly
        jnz     J_NL_Exit

        test    [emState], ES_MULTILINE
        jnz     NL0

NL_Beep:
        cCall   NearBeep

J_NL_EXIT:
        jmp     NL_Exit
NL0:
        mov     ax, [ipCur.ip_ob]               ; Save current ip in case
        mov     [obSave], ax                    ;    it must be restored.
        mov     ax, [ipCur.ip_oln]
        mov     [olnSave], ax

        cCall   NoSelection

        cCall   ModifyLine
        jz      J_NL_Exit

        mov     ax,[ldCur.LD_cb]
        cmp     [ipCur.IP_ob],ax
        jb      NL1

        push    ax                              ; Save ldCur.ld_cb
        cCall   InsertLineBelow
        pop     cx                              ; Restore ldCur.ld_cb
        jz      NL_Beep

        test    [fMoveCursor], 0ffH
        jz      J_NL10

        jcxz    NL_0a                   ; Don't change ob for blank lines

        cCall   AutoIndent
NL_0a:
        inc     [ipCur.ip_oln]
if 1
        jmp     NL5a
else
        cCall   AdjustAfterInsertLine
        jmp     NL_Exit
endif

J_NL10:
        jmp     NL10

;
; Scan for first non-space
; UNDONE - optimise with scasb
;
NL1:
        mov     bx,[ipCur.IP_ob]
        mov     si,[ldCur.LD_prgch]
NL2:
        cmp     bx,[ldCur.LD_cb]
        jnb     NL3

        cmp     BYTE PTR [bx][si],' '
        jne     NL3

        inc     bx
        jmp     NL2

NL3:
        mov     [ipCur.IP_ob],bx

;
; Save start of line for autoindent
;
        cCall   obGetFirst
        mov     [obFirst],ax

        cCall   StartBigEdit

;
; Delete (logically - not really) to end of line.
;
; Warning: ReplaceLineBuf MUST not tamper with ldCur.prgch
;
        cCall   ReplaceLineBuf,<hBuffer,pdCur.pd_oln,ipCur.ip_ob,ldCur.ld_prgch>
        cCall   EMRefreshCache

;
; Delete (really) from begining of line to cursor
;
        cmp     [ipCur.IP_ob],0
        je      NL4

        mov     ax,[ipCur.IP_ob]
        cCall   DelCh,<ax,ax>

;
; Auto indent.
;
NL4:
        mov     [ipCur.IP_ob],0

        cmp     [obFirst],0
        je      NL5

        mov     ax, ' '
        cCall   InsCh,<obFirst,ax, ax>  ; InsCh( obFirst, ' ', TRUE );

NL5:
        mov     ax,[obFirst]
        mov     [ipCur.IP_ob],ax

        inc     [ipCur.IP_oln]
        cCall   InsertLineBuf,<hBuffer,ipCur.ip_oln,ldCur.ld_cb,ldCur.ld_prgch>
        cCall   EMRefreshCache

        and     [ldCur.ld_flags], not ld_fDirty

        mov     [pdCur.PD_oln],-1

        cCall   EndBigEdit
        cCall   EMRefreshCache

        test    [fMoveCursor], 0ffH
        jnz     NL5a

        mov     ax, [obSave]
        mov     [ipCur.ip_ob], ax
        mov     ax, [olnSave]
        mov     [ipCur.ip_oln], ax
        jmp     short NL10

NL5a:
;
; Ok - Now let's see what has to be redrawn
;
        mov     ax,[cLinesCur]
        add     ax,[pdCur.PD_olnTop]
        cmp     ax,[ipCur.IP_oln]
        ja      NL7

        inc     [pdCur.PD_olnTop]
        cCall   DisplayCurPos
NL6:
        inc     [fRefreshScreen]
        jmp     short NL_Exit

NL7:
        mov     ax,[pdCur.PD_obLeft]
        cmp     [ipCur.IP_ob],ax
        jge     NL10

        mov     ax,[ipCur.IP_ob]
        mov     [pdCur.PD_obLeft],ax

        mov     ax,[cColumnsCur]
        shr     ax,1
        cmp     ax,[pdCur.PD_obLeft]
        jbe     NL9

NL8:
        mov     [pdCur.PD_obLeft],0
        jmp     SHORT NL6

NL9:
        cmp     [pdCur.PD_obLeft],5
        jle     NL8

        sub     [pdCur.PD_obLeft],5

        jmp     SHORT NL6
NL10:
        mov     bx,-1
        mov     cx,[ipCur.IP_oln]
        jcxz    NL11
        dec     cx
NL11:
        cCall   RefreshLines,<cx,bx>
NL_Exit:
        mov     [fResetAnchor], 1
cEnd

cProc NewLine,<NEAR,PUBLIC>
cBegin
        mov     ax, 1
        cCall   DoNewLine,<ax>
cEnd

cProc SplitLine,<NEAR,PUBLIC>
cBegin
        xor     ax,ax
        cCall   DoNewLine,<ax>
cEnd

cProc NextLine,<NEAR,PUBLIC>
cBegin
        cCall   LineDown

        cCall   GetCurLine
        mov     ax, [ldCur.ld_cb]
        or      ax,ax
        je      NxL_1

        cCall   HomeLine
        jmp     short NxL_Exit

NxL_1:
        mov     [ipCur.ip_ob], -1
        cCall   obGetPrev
        mov     [ipCur.ip_ob], ax
        cCall   DisplayCurPos
NxL_Exit:
cEnd

cProc BegLine,<NEAR>
cBegin
        mov     [ipCur.ip_ob], 0
        cCall   DisplayCurPos
cEnd

;*******************************************************************************
;_InsertLineBelow
;
;Purpose:
;       Insert a blank line in the text buffer.
;Entry:
;       ldCur
;       ipCur
;       pdCur
;       hBuffer
;Exit:
;       none
;Uses:
;Exceptions:
;*******************************************************************************
cProc InsertLineBelow,<NEAR,PUBLIC>,<SI,DI>
cBegin
        cCall   fCheckReadOnly
        mov     al, 0                   ; mov does NOT affect flags
        jnz     ExitInsertLineBelow

        cCall   UpdateLine              ;make sure current line is in text buf.
        mov     al, 1                   ; Return TRUE even if above failed.
        jz      ExitInsertLineBelow

        cCall   NoSelection             ;turn off any selected region

        mov     [ldCur.ld_cb],0         ;make blank line
        mov     [pdCur.PD_oln],-1       ;

        mov     ax, [ipCur.ip_oln]
        inc     ax
        cCall   InsertLineBuf,<hBuffer,ax,ldCur.ld_cb,ldCur.ld_prgch>
        push    ax                      ; Save until after this call
        cCall   EMRefreshCache
        pop     ax                      ; restore result of InsertLineBuf

ExitInsertLineBelow:
        or      al,al                   ; Return Z or NZ.
cEnd

cProc AdjustAfterInsertLine,<NEAR>
cBegin
        mov     ax, [ipCur.ip_ob]
        mov     bx, [pdCur.pd_obleft]
        cmp     ax, bx
        jae     AAIL_1
        mov     [ipCur.ip_ob], bx
AAIL_1:
        mov     ax,[cLinesCur]          ;pwndEditCur.cLines (lines on screen)
        add     ax,[pdCur.pd_olntop]    ;set olnBottom 1 above last line on
        dec     ax                      ;screen  di - olnBottom

        cmp     [ipCur.ip_oln],ax       ;ax - olnBottom
        jbe     AAIL_2                  ;brif ip is above bottom

        inc     [pdCur.pd_olntop]       ;move top of screen up 1 line
        inc     [fRefreshScreen]
        jmp     SHORT AAIL_Exit 
AAIL_2:
        mov     bx,[ipCur.IP_oln]
        dec     bx
        cCall   RefreshLines,<bx,ax>
AAIL_Exit:
cEnd

cProc InsertLine,<NEAR,PUBLIC>
cBegin
        test    [emState], ES_MULTILINE
        jnz     IL_1

IL_Beep:
        cCall   NearBeep
        jmp     IL_Exit

IL_1:
        cCall   InsertLineBelow
        jz      IL_Beep

        cCall   GetCurLine
        mov     cx, [ldCur.ld_cb]
        jcxz    IL_2

        cCall   AutoIndent
        inc     [ipCur.ip_oln]
IL_2:
        cCall   AdjustAfterInsertLine
IL_Exit:
cEnd


;*******************************************************************************
;_KillLine
;
;Purpose:
;       Delete the current line from the text buffer. The current line is
;       first selected and the a distructive cut is done.
;Entry:
;       fMultiLine
;       hBuffer
;       pefCur.ef_fSelection
;       ipCur
;Exit:
;       ldCur
;Uses:
;       none.
;Exceptions:
;       none
;*******************************************************************************
cProc KillLine,<NEAR,PUBLIC>
cBegin
        test    [emState], ES_MULTILINE ;don't delete line if only single line
        jz      $I471                   ;brif not multiline
                                        ;also don't delete if last line in text
        mov     ax, [clnCur]            ;buffer
        cmp     ax,ipCur.ip_oln
        jne     $I470                   ;brif ip not last line in text buffer

;Here if not ok to delete line
$I471:
        cCall   NearBeep                ;warn user
        jmp     SHORT $EX469            ;return

;Here if ok to delete line
$I470:
        cCall   fCheckReadOnly
        jnz     $EX469

        cCall   StartBigEdit            ; Don't parse the dirty line
        cCall   UpdateLine
        cCall   EndBigEdit
        cCall   EMRefreshCache
                
        cCall   NoSelection             ;remove any selection
        inc     ipCur.ip_oln            ;Delete by forcing full selection
        mov     ipCur.ip_ob,0           ;of line. ipAnchor is still at old spot
        cCall   HiliteSelection         ;hilite whole line
        mov     bx,pefCur               ;set flag that we do have selection
        mov     [bx].ef_fSelection,TRUE
        mov     ax,TRUE
        cCall   Cut,<ax>                ;do cut. This will force old line
                                        ;into scrap for later recovery
        cCall   DisplayCurPos           ;recal. ip screen state
$EX469:
cEnd

;*******************************************************************************
;_EraseEol
;
;Purpose:
;       Delete characters from ip to end of line. Line is refreshed.
;       Deletion is done by selecting and then cutting.
;Entry:
;       ipCur
;       ldCur
;       pefCur.ef_fSelection
;Exit:
;       ldCur
;Uses:
;       none.
;Exceptions:
;
;*******************************************************************************
cProc EraseEol,<NEAR,PUBLIC>
cBegin
        cCall   fCheckReadOnly
        jnz     ExitEraseEol
        cCall   NoSelection                     ;remove any selection
        cCall   GetCurLine                      ;get the line in ld
        mov     ax,ldCur.ld_cb
        mov     ipAnchor.ip_ob,ax               ;move anchor to eol
        cCall   HiliteSelection                 ;hilite from ip to eol
        ;!!! This code can be shared with Kill Line
        mov     bx,pefCur                       ;set selection true
        mov     [bx].ef_fSelection,TRUE
        mov     ax,TRUE
        cCall   Cut,<ax>
ExitEraseEol:
cEnd

;*******************************************************************************
;DelWord
;
;Purpose:
; If the cursor is on a word then delete to the end of the word
; else do Del().
;
;Entry:
;Exit:
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc DelWord,<NEAR,PUBLIC>,<SI>
cBegin
        cCall   NoSelection

        cCall   ModifyLine
        jz      DW_Exit

        mov     si, [ipCur.ip_ob]

        cmp     si, [ldCur.ld_cb]
        jae     DW_0                            ; Cursor is past end of line
                                                ; do Del which will do JoinLine

        cCall   FOnSpace
        jz      DW_1                            ; Cursor is on a space

        cCall   FOnWord                         ; Is cursor on a word char?
        jnz     DW_2                            ; Yes - Delete word

DW_0:
;
; Cursor is not on a word and not on a space.
; So just delete one character (or JoinLine).
;
        cCall   Del
        jmp     short DW_Exit

DW_1:
;
; Cursor is on a space. So delete all white space after cursor
;
        inc     [ipCur.ip_ob]
        cCall   FOnSpace
        jz      DW_1
        jmp     short DW_3

DW_2:
;
; Cursor is on a word so delete to the end of the word.
;
        cCall   RightPastWordEnd                ; Find end of word
DW_3:
        mov     ax, [ipCur.ip_ob]
        mov     bx, ax
        mov     [ipCur.ip_ob], si
        sub     ax, si                          ; How many chars to delete
        cCall   DelCh,<ax,bx>
        cCall   RefreshLine,<ipCur.ip_oln>
DW_Exit:
cEnd

;*******************************************************************************
;FOnSpace
;
;Purpose:
; Checks if the cursor is on a space
;
;
;Entry:
;Exit:
; Return Z if cursor is on a space.
;
;Uses:
;Exceptions:
;
;*******************************************************************************
cProc FOnSpace,<NEAR>
cBegin
        mov     bx, [ldCur.ld_prgch]
        add     bx, [ipCur.ip_ob]
        mov     al,[bx]
        cmp     al, ' '
cEnd

;*******************************************************************************
;_InsertKey
;
;Purpose:
;       Insert the specfied character at the current ip
;Entry:
;       InsertCh - character to insert
;       ipCur
;       pdCur
;       ldCur
;       pwndEditCur.cColumns
;       fRedraw
;Exit:
;       ldCur
;       ipCur
;Uses:
;       si, di, cx, bx
;Exceptions:
;
;*******************************************************************************
cProc InsertKey,<NEAR,PUBLIC>,<SI,DI>
        parmB   InsertCh
cBegin
        cCall   fCheckReadOnly
        jnz     J_ExitInsertKey
        cmp     [InsertCh],0
        jne     @F                      ;if not null contine
J_ExitInsertKey:
        jmp     ExitInsertKey           ;if null don't insert key

;Here if key ok to insert
@@:
        sub     ax,ax                   ;first cut any selection
        cCall   Cut,<ax>                ;so that insertion replaces text
        cCall   ModifyLine              ;mark line as modified
        mov     di,ldCur.ld_cbMax
        dec     di
        cmp     [fInsertMode],0         ; Can always insert another char when
        jnz     @F                      ;   not in insert mode
ifdef   KANJI
        mov     si,[ldCur.LD_prgch]     ;If we are in overwrite, then
        cCall   DbcsAdjCursor
        add     si,ax
        cCall   FIsDbcsChar,<[si]>      ; See if current char is DBCS
        or      ax,ax
        jz      SkipInsertSpace
        inc     si                      ; if so,
        mov     byte ptr [si],' '       ; put a space at next byte
SkipInsertSpace:
endif   ; KANJI
        jmp     SHORT DoInsertKey
@@:
        cmp     di,ldCur.ld_cb          ;if line almost full don't insert
        je      NoInsertKey             ;alway leave 1 InsertCh space

;Here if room to insert key
DoInsertKey:
        cmp     di, [ipCur.ip_ob]       ;beep if cursor past cbMax
        jbe     NoInsertKey

        mov     al,[InsertCh]
        cbw     
        mov     bx,1                    ;insert only 1 copy of character
        mov     cl, [fInsertMode]
        cCall   InsCh,<bx,ax, cx>       ;insert into ldCur
        or      ax,ax                   ;test if insert successfull
        je      NoInsertKey             ;brif if not
        inc     ipCur.ip_ob             ;move to right on screen
        mov     si,[cColumnsCur]        ;pwndEditCur.cColumns
        add     si,pdCur.pd_obleft
        cmp     ipCur.ip_ob,si          ;compute max. right position
        jl      @F                      ;brif if new position to right of max
;Here if new position if off right end of screen
        sub     ax,ax
        cCall   PageRight,<shiftWidth,ax>               ;page 1 to right
        jmp     SHORT ExitInsertKey
;Here if new position is still on screen
@@:
ifdef   KANJI
        cmp     [fCharIsDbcs],0
        jnz     SHORT ExitInsertKey
endif   ; KANJI
        cCall   RefreshLine,<ipCur.ip_oln>
        jmp     SHORT ExitInsertKey

;Here if insert can't be done
NoInsertKey:
        cCall   NearBeep
ExitInsertKey:
cEnd

;*****************************************************************************
