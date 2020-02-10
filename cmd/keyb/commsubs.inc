;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1987 - 1991
; *                      All Rights Reserved.
; */
.XLIST



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                           
;; MS-DOS 5.00 - NLS Support - KEYB Command
;;
;;
;; File Name:  CONVERT.INC
;; ----------
;;
;; Root File Name:  KEYBCMD.ASM
;; ---------------
;;
;;
;; Description:
;; ------------
;;       External declarations for procedures in file KEYBCMD.ASM.
;;
;; Change History:
;; ---------------
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
        EXTRN    FIND_SYS_TYPE   :NEAR ;;
        EXTRN    FIND_KEYB_TYPE  :NEAR ;;
                                       ;;
        EXTRN    HW_TYPE         :WORD ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.LIST
