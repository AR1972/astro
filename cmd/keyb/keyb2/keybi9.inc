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
;; File Name:  KEYBI9.INC
;; ----------
;;
;; Root File Name:  KEYB.ASM
;; ---------------
;;
;; Description:
;; ------------
;;       External declarations and equates for procedures in file
;;       KEYBI9.ASM
;;
;; Change History:
;; ---------------
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
        EXTRN    KEYB_STATE_PROCESSOR:NEAR

        EXTRN    FLAGS_TO_TEST:BYTE    ;;  (YST)
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.LIST

