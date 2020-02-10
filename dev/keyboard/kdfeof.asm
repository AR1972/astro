;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */





	PAGE	,132
	TITLE	MS-DOS TUGBOAT Keyboard Definition File

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MS-DOS TUGBOAT - NLS Support - Keyboard Definition File
;;
;; This file contains the eof marker for the entire table
;; and the keyboard.sys copyright information
;;
;; Linkage Instructions:
;;	Refer to KDF.ASM.
;;
;;
;; Author:     BILL DEVLIN  - IBM Canada Laboratory - May 1986
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
				       ;;
CODE	SEGMENT PUBLIC 'CODE'          ;;
	ASSUME CS:CODE,DS:CODE	       ;;
				       ;;
				       ;;
				       ;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Copyright statement
;;	DB 'Authors : Bill Devlin, Nick Savage, Mike Saunders, et al..',13,10
;;	DB 'Development: Toronto,Boca Raton,Basingstoke',13,10

include copyrigh.inc
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	DB  1AH 		       ;; EOF
				       ;;
CODE	ENDS			       ;;
	END			       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
