;*      SCCSWHAT( "@(#)meditasm.asm     1.6 88/04/22 19:47:41   " ) */
;*
;*      COW : Character Oriented Windows
;*
;*      meditasm.inc : Multi-line edit manager main ASM file
;*      * REVIEW : these should be cleaned up.

        page    ,132
        TITLE   editmgr
        NAME    editmgr

SILENT = 1

        .xlist
        include user.inc

IFDEF EDIT_FULLMGR      ;(entire file)

        include cwproc.inc                      ;* CW procedures
        include uevent.inc                      ;* for messages
        include uisa.inc                        ;* for colors
        include uscroll.inc                     ;* for scroll messages
        include vkey.inc                        ;* for VK_ values
        include uscreen.inc                     ;* for draw modes
        include uedit.inc                       ;* for EN_ values
        include inmou.inc                       ;* for MK_ values
        include inkbd.inc                       ;* for KK_ values

        include util.inc

        include editmgr.inc             ;* REVIEW danger (this is public !!)
        include text.inc                ;* REVIEW (this entire interface)

        .list
        .sall

IFNDEF COW_SWAPPED
?win = 0
?cow = 0
ENDIF ;!COW_SWAPPED

TRUE            equ     01h
FALSE           equ     00h

sBegin DATA
    assumes ds,DGROUP

DEBPUBLIC       <clnCur, ldCur, fPasteOk, fInsertMode>
clnCur          dw      0

externB         ldEMScratch
ldCur           ld      <>

fPasteOk        DB      00H
fInsertMode     db      01H

ifdef KANJI
fCharIsDbcs     db      00H
externB         fReverse
endif

DEBPUBLIC       <pwndEditCur, cLinesCur, cColumnsCur, CurattrCur>
pwndEditCur     DW      00H
cLinesCur       DW      00H
cColumnsCur     DW      00H
CurattrCur      DW      isaBackground

DEBPUBLIC       <shiftWidth, tabStops, fResetAnchor>
shiftWidth      DW      01H
tabStops        DW      08H
fResetAnchor    DB      00H

DEBPUBLIC       <emFlags, emState>
;Global editmgr state flags
emFlags         db      00H     ;modified by EMF_xxx

;Local editmgr state flags
emState         db      00H     ;modified by EF_xxx
                                ;modified by ES_xxx
EF_TRACKING_MOUSE       equ     20H
EF_IN_WINDOW            equ     40H
EF_WORD_SELECTING       equ     80H

DEBPUBLIC       <pefCur, fRefreshScreen, hBuffer>
        EVEN
pefCur          dw      0

        EVEN
fRefreshScreen  db      0
hBuffer dw      0

DEBPUBLIC       <ipCur, ipStart, ipAnchor, ipAnchorStart, pdCur>
        EVEN
ipCur           ip      <>
ipStart         ip      <>
ipAnchor        ip      <>
ipAnchorStart   ip      <>
pdCur           pd      <>

EditMgrDoCharState      dw      User_EditOFFSET EMDC_InitialState
chEditMgrState  db      0

ifdef   EDIT_USE_TABLES
SecondDispatch  dw      0               ;* Current Secondary dispatch table
endif   ; EDIT_USE_TABLES

sb      struc
        sbfMultiLine    db      0
        sb_cb           dw      0
sb      ends

DEBPUBLIC       <_scrap>
_scrap          sb      <>
ifndef  PROJECT_QB
sb_buffer       db      MAXCHARS dup(?) ; NEAR massive buffer
endif   ;PROJECT_QB

DEBPUBLIC       <rgLineAttr0, rgLineAttr1, rgLineAttr2, rgLineAttr3>
rgLineAttr0     LINEATTR<0,0>
rgLineAttr1     LINEATTR<0,0>
rgLineAttr2     LINEATTR<0,0>
rgLineAttr3     LINEATTR<0,0>

pfnFilterSave   dw      0


;;
;; a-emoryh - Include cmwSwitches (from ..\..\..\qb5\ir\init.asm), so we can
;;      enable ES_NOSELECT when in QHelp mode.  Existing behavior is unaltered
;;      in any other modes.
;;
externW cmdSwitches

;; a-emoryh:
;; This must match up with the define in ..\..\..\qb\hd\ui.inc
;;
CMD_SW_QHELP equ 0800h          ;/QHELP switch found


sEnd DATA

ifdef   PROJECT_QB
sBegin  FAR_PRESERVED
DEBPUBLIC       <sb_buffer>
sb_buffer       db      MAXCHARS dup(?) ; FAR massive buffer
sEnd    FAR_PRESERVED
endif   ;PROJECT_QB

        externFP DrawToggles

IFDEF   KANJI
        externFP FIsDbcsChar
        externFP PchPrevDbcs
        externFP PchNextDbcs
ENDIF   ; KANJI

IFDEF   KK_UNIT
        externFP DisplayKKBuf
ENDIF   ; KK_UNIT

sBegin USER_EDIT
Assumes cs,USER_EDIT
Assumes ds,DGROUP
Assumes ss,DGROUP

;*      * REVIEW : these should be broken up
IFDEF   KANJI
        include qbj\editmgr1.asm
        include qbj\editmgr2.asm
ELSE    ; !KANJI
        include editmgr1.asm
        include editmgr2.asm
ENDIF   ; KANJI

sEnd USER_EDIT

ENDIF; EDIT_FULLMGR (entire file)

        END
