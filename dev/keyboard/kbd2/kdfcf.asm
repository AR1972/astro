;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

        PAGE    118,132
        TITLE   MS-DOS - Keyboard Definition File

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MS-DOS - NLS Support - Keyboard Definition File
;; (c) Copyright 1988 Microsoft
;;
;; This file contains the keyboard tables for Dual mode Canadian, French
;;
;; Linkage Instructions:
;;      Refer to KDF.ASM.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
        INCLUDE KEYBSHAR.INC           ;;
        INCLUDE POSTEQU.INC            ;;
        INCLUDE KEYBMAC.INC            ;;
                                       ;;
        PUBLIC CF_LOGIC 	       ;;
	PUBLIC CF_863_XLAT	       ;;
	PUBLIC CF_850_XLAT	       ;;
                                       ;;
CODE    SEGMENT PUBLIC 'CODE'          ;;
        ASSUME CS:CODE,DS:CODE         ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard translate table options are a linear search table
;; (TYPE_2_TAB) and ASCII entries ONLY (ASCII_ONLY)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
STANDARD_TABLE      EQU   TYPE_2_TAB+ASCII_ONLY
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;;
;; CF State Logic
;;
;;***************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
CF_LOGIC:                              ;;
                                       ;;
   DW  LOGIC_END-$                     ;; length
                                       ;;
   DW  SHIFTS_TO_LOGIC+SWITCHABLE      ;; special features
                                       ;;
                                       ;; COMMANDS START HERE
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPTIONS:  If we find a scan match in
;; an XLATT or SET_FLAG operation then
;; exit from INT 9.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
                                       ;;
   OPTION EXIT_IF_FOUND                ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mode change CHECK
;;
;; MODE CHANGE BY <CTRL + Left SHIFT> and
;; <CTRL+Right SHIFT> PRESS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
                                       ;;
 IFF SHIFTS_PRESSED                    ;;
    IFF EITHER_ALT,NOT                 ;;
    ANDF EITHER_CTL                    ;;
      IFF LEFT_SHIFT                   ;;
          BEEP                         ;;
          SET_FLAG RUS_MODE_SET        ;;
       ENDIFF                          ;;
      IFF RIGHT_SHIFT                  ;;
          BEEP                         ;;
          RESET_NLS                    ;;
       ENDIFF                          ;;
    ENDIFF                             ;;
    EXIT_STATE_LOGIC                   ;;
 ENDIFF                                ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 IFF RUS_MODE,NOT                      ;;  Primary Mode dead keys
  IFF  EITHER_ALT,NOT                  ;;
   ANDF EITHER_CTL,NOT                 ;;
    IFF LC_E0,NOT                      ;;
      IFF EITHER_SHIFT                 ;;
          SET_FLAG DEAD_UPPER          ;;
      ELSEF                            ;;
          SET_FLAG DEAD_LOWER          ;;
      ENDIFF                           ;;
    ENDIFF                             ;;
   ELSEF                               ;;
      IFF EITHER_SHIFT,NOT             ;;
        IFKBD XT_KB+AT_KB              ;;
          IFF EITHER_CTL               ;;
          ANDF ALT_SHIFT               ;;
            SET_FLAG DEAD_THIRD        ;;
          ENDIFF                       ;;
        ELSEF                          ;;
         IFF R_ALT_SHIFT               ;;
         ANDF EITHER_CTL,NOT           ;;
         ANDF LC_E0,NOT                ;;
            SET_FLAG DEAD_THIRD        ;;
         ENDIFF                        ;;
        ENDIFF                         ;;
       ENDIFF                          ;;
   ENDIFF                              ;;
 ELSEF                                 ;;    Secondary Mode Dead Keys
  IFF  EITHER_ALT,NOT                  ;;
   ANDF EITHER_CTL,NOT                 ;;
      IFF EITHER_SHIFT,NOT             ;;
          SET_FLAG DEAD_LOWER_SEC      ;;
      ENDIFF                           ;;
    ENDIFF                             ;;
   ENDIFF                              ;;
 ENDIFF                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CIRCUMFLEX ACCENT TRANSLATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   IFF CIRCUMFLEX,NOT                  ;;
      GOTO DIARESIS_PROC               ;;
      ENDIFF                           ;;
                                       ;;
      RESET_NLS                        ;;
      IFF R_ALT_SHIFT,NOT              ;;
         XLATT CIRCUMFLEX_SPACE        ;;
      ENDIFF                           ;;
      IFF EITHER_CTL,NOT               ;;
      ANDF EITHER_ALT,NOT              ;;
         IFF EITHER_SHIFT              ;;
            IFF CAPS_STATE             ;;
               XLATT CIRCUMFLEX_LOWER  ;;
            ELSEF                      ;;
               XLATT CIRCUMFLEX_UPPER  ;;
            ENDIFF                     ;;
         ELSEF                         ;;
            IFF CAPS_STATE             ;;
               XLATT CIRCUMFLEX_UPPER  ;;
            ELSEF                      ;;
               XLATT CIRCUMFLEX_LOWER  ;;
            ENDIFF                     ;;
         ENDIFF                        ;;
      ENDIFF                           ;;
                                       ;;
INVALID_CIRCUMFLEX:                         ;;
      PUT_ERROR_CHAR CIRCUMFLEX_SPACE       ;; If we get here then either the XLATT
      BEEP                             ;; failed or we are ina bad shift state.
      GOTO NON_DEAD                    ;; Either is invalid so BEEP and fall
                                       ;; through to generate the second char.
                                       ;; Note that the dead key flag will be
                                       ;; reset before we get here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DIARESIS ACCENT TRANSLATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
DIARESIS_PROC:                         ;;
				       ;;
   IFF DIARESIS,NOT                    ;;
      GOTO GRAVE_PROC                  ;;
      ENDIFF                           ;;
				       ;;
      RESET_NLS                        ;;
      IFF R_ALT_SHIFT,NOT              ;;
	 XLATT DIARESIS_SPACE          ;;  exist for 850 so beep for
      ENDIFF                           ;;
      IFF EITHER_CTL,NOT               ;;
      ANDF EITHER_ALT,NOT              ;;
	 IFF EITHER_SHIFT              ;;
	    IFF CAPS_STATE             ;;
	       XLATT DIARESIS_LOWER    ;;
	    ELSEF                      ;;
	       XLATT DIARESIS_UPPER    ;;
	    ENDIFF                     ;;
	 ELSEF                         ;;
	    IFF CAPS_STATE             ;;
	       XLATT DIARESIS_UPPER    ;;
	    ELSEF                      ;;
	       XLATT DIARESIS_LOWER    ;;
	    ENDIFF                     ;;
	 ENDIFF                        ;;
      ENDIFF                           ;;
				       ;;
INVALID_DIARESIS:                      ;;
      PUT_ERROR_CHAR DIARESIS_LOWER    ;; standalone accent
      BEEP                             ;; Invalid dead key combo.
      GOTO NON_DEAD                    ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRAVE ACCENT TRANSLATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
GRAVE_PROC:                            ;;
				       ;;
   IFF GRAVE,NOT		       ;;
      GOTO TILDE_PROC 	               ;;
      ENDIFF			       ;;
				       ;;
      RESET_NLS 		       ;;
      IFF R_ALT_SHIFT,NOT	       ;;
	 XLATT GRAVE_SPACE	       ;;
      ENDIFF			       ;;
      IFF EITHER_CTL,NOT	       ;;
      ANDF EITHER_ALT,NOT	       ;;
	IFF EITHER_SHIFT	       ;;
	   IFF CAPS_STATE	       ;;
	      XLATT GRAVE_LOWER        ;;
	   ELSEF		       ;;
	      XLATT GRAVE_UPPER        ;;
	   ENDIFF		       ;;
	ELSEF			       ;;
	   IFF CAPS_STATE,NOT	       ;;
	      XLATT GRAVE_LOWER        ;;
	   ELSEF		       ;;
	      XLATT GRAVE_UPPER        ;;
	   ENDIFF		       ;;
	ENDIFF			       ;;
      ENDIFF			       ;;
				       ;;
INVALID_GRAVE:			       ;;
      PUT_ERROR_CHAR GRAVE_LOWER       ;; standalone accent
      BEEP			       ;; Invalid dead key combo.
      GOTO NON_DEAD		       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACUTE ACCENT TRANSLATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
ACUTE_PROC:			       ;;
				       ;;
   IFF ACUTE,NOT		       ;;
      GOTO NON_DEAD     	       ;;
      ENDIFF			       ;;
				       ;;
      RESET_NLS1 		       ;;
ACUTE_ON:			       ;;
      IFF R_ALT_SHIFT,NOT	       ;;
	 XLATT ACUTE_SPACE	       ;;
      ENDIFF			       ;;
      IFF EITHER_CTL,NOT	       ;;
      ANDF EITHER_ALT,NOT	       ;;
	IFF EITHER_SHIFT	       ;;
	   IFF CAPS_STATE	       ;;
	      XLATT ACUTE_LOWER        ;;
	   ELSEF		       ;;
	      XLATT ACUTE_UPPER        ;;
	   ENDIFF		       ;;
	ELSEF			       ;;
	   IFF CAPS_STATE	       ;;
	      XLATT ACUTE_UPPER        ;;
	   ELSEF		       ;;
	      XLATT ACUTE_LOWER        ;;
	   ENDIFF		       ;;
	ENDIFF			       ;;
      ENDIFF			       ;;
				       ;;
INVALID_ACUTE:			       ;;
      PUT_ERROR_CHAR ACUTE_LOWER       ;; standalone accent
      BEEP			       ;; Invalid dead key combo.
      GOTO NON_DEAD		       ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDILLA ACCENT TRANSLATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
CEDILLA_PROC:			       ;;
				       ;;
   IFF CEDILLA,NOT		       ;;
      GOTO TILDE_PROC                  ;;
      ENDIFF			       ;;
				       ;;
      RESET_NLS1 		       ;;
CEDILLA_ON:			       ;;
      IFF R_ALT_SHIFT,NOT	       ;;
	 XLATT CEDILLA_SPACE	       ;;
      ENDIFF			       ;;
      IFF EITHER_CTL,NOT	       ;;
      ANDF EITHER_ALT,NOT	       ;;
	IFF EITHER_SHIFT	       ;;
	   IFF CAPS_STATE	       ;;
	      XLATT CEDILLA_LOWER      ;;
	   ELSEF		       ;;
	      XLATT CEDILLA_UPPER      ;;
	   ENDIFF		       ;;
	ELSEF			       ;;
	   IFF CAPS_STATE	       ;;
	      XLATT CEDILLA_UPPER      ;;
	   ELSEF		       ;;
	      XLATT CEDILLA_LOWER      ;;
	   ENDIFF		       ;;
	ENDIFF			       ;;
      ENDIFF			       ;;
				       ;;
INVALID_CEDILLA:		       ;;
      PUT_ERROR_CHAR CEDILLA_LOWER     ;; standalone accent
      BEEP			       ;; Invalid dead key combo.
      GOTO NON_DEAD		       ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TILDE ACCENT TRANSLATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
TILDE_PROC:			       ;;
				       ;;
   IFF TILDE,NOT		       ;;
      GOTO NON_DEAD     	       ;;
      ENDIFF			       ;;
				       ;;
      RESET_NLS1 		       ;;
TILDE_ON:			       ;;
      IFF R_ALT_SHIFT,NOT	       ;;
	 XLATT TILDE_SPACE	       ;;
      ENDIFF			       ;;
      IFF EITHER_CTL,NOT	       ;;
      ANDF EITHER_ALT,NOT	       ;;
	IFF EITHER_SHIFT	       ;;
	   IFF CAPS_STATE	       ;;
	      XLATT TILDE_LOWER        ;;
	   ELSEF		       ;;
	      XLATT TILDE_UPPER        ;;
	   ENDIFF		       ;;
	ELSEF			       ;;
	   IFF CAPS_STATE	       ;;
	      XLATT TILDE_UPPER        ;;
	   ELSEF		       ;;
	      XLATT TILDE_LOWER        ;;
	   ENDIFF		       ;;
	ENDIFF			       ;;
      ENDIFF			       ;;
				       ;;
INVALID_TILDE:			       ;;
      PUT_ERROR_CHAR TILDE_LOWER       ;; standalone accent
      BEEP			       ;; Invalid dead key combo.
      GOTO NON_DEAD		       ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                       ;;
NON_DEAD:                              ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Upper, lower and third shifts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
                                       ;;
                                       ;;
 IFF  EITHER_CTL,NOT                   ;; Lower and upper case.  Alphabetic
    IFF EITHER_ALT,NOT                 ;; keys are affected by CAPS LOCK.
      IFF RUS_MODE,NOT                      ;;
      ANDF LC_E0,NOT                    ;; Enhanced keys are not
        IFF EITHER_SHIFT                 ;; Numeric keys are not.
          XLATT NON_ALPHA_UPPER        ;;
          IFF CAPS_STATE               ;;
              XLATT ALPHA_LOWER        ;;
          ELSEF                        ;;
              XLATT ALPHA_UPPER        ;;
          ENDIFF                       ;;
        ELSEF                          ;;
          XLATT NON_ALPHA_LOWER        ;;
          IFF CAPS_STATE               ;;
             XLATT ALPHA_UPPER         ;;
          ELSEF                        ;;
             XLATT ALPHA_LOWER         ;;
          ENDIFF                       ;;
        ENDIFF                           ;; Third and Fourth shifts
      ELSEF
       IFF LC_E0, NOT
         IFF EITHER_SHIFT               ;;
           XLATT NON_ALPHA_UPPER_SEC    ;;
           IFF CAPS_STATE               ;;
              XLATT ALPHA_LOWER_SEC     ;;
           ELSEF                        ;;
              XLATT ALPHA_UPPER_SEC     ;;
           ENDIFF                       ;;
         ELSEF                          ;;
           XLATT NON_ALPHA_LOWER_SEC    ;;
           IFF CAPS_STATE               ;;
             XLATT ALPHA_UPPER_SEC      ;;
           ELSEF                        ;;
             XLATT ALPHA_LOWER_SEC      ;;
           ENDIFF                       ;;
         ENDIFF
       ENDIFF
      ENDIFF                           ;;
    ELSEF                              ;; ctl off, alt on at this point
         IFF R_ALT_SHIFT               ;; ALTGr
         ANDF EITHER_SHIFT,NOT         ;;
            XLATT THIRD_SHIFT          ;;
         ENDIFF                        ;;
    ENDIFF                             ;;
 ELSEF
    IFF EITHER_ALT,NOT                 ;;
        XLATT CTRL_CASE
    ELSEF                              ;;
      IFKBD XT_KB+AT_KB                ;; XT, AT,  keyboards.
         IFF EITHER_SHIFT,NOT          ;; only.
            XLATT THIRD_SHIFT          ;; ALT + Ctrl
         ENDIFF                        ;;
      ENDIFF                           ;;
    ENDIFF
 ENDIFF                                ;;
                                       ;;
;**************************************;;
                                       ;;
 EXIT_STATE_LOGIC                      ;;
                                       ;;
LOGIC_END:                             ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;**********************************************************************
;; CF Common Translate Section
;; This section contains translations for the lower 128 characters
;; only since these will never change from code page to code page.
;; Some common Characters are included from 128 - 165 where appropriate.
;; In addition the dead key "Set Flag" tables are here since the
;; dead keys are on the same keytops for all code pages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
 PUBLIC CF_COMMON_XLAT                ;;
CF_COMMON_XLAT:                       ;;
                                       ;;
   DW    COMMON_XLAT_END-$             ;; length of section
   DW    -1                            ;; code page
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: COMMON
;; STATE: low shift Dead_lower_SEC
;; KEYBOARD TYPES: G                              SECONDARY MODE
;; TABLE TYPE: Flag Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW	 COM_SC_LO_END-$	      ;; length of state section
   DB    DEAD_LOWER_SEC                ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;; Set Flag Table
   DW    3                             ;; number of entries
   DB    27                            ;;
   FLAG  TILDE                         ;;
   DB    13                            ;;
   FLAG  CEDILLA 		       ;;
   DB    39                            ;;
   FLAG  ACUTE                         ;;
                                       ;;
COM_SC_LO_END:			       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: COMMON
;; STATE: low shift Dead_lower
;; KEYBOARD TYPES: G
;; TABLE TYPE: Flag Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_PL_LO_END-$               ;; length of state section
   DB    DEAD_LOWER                    ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;; Set Flag Table
   DW    1                             ;; number of entries
   DB    26                            ;;
   FLAG  CIRCUMFLEX                    ;;
                                       ;;
COM_PL_LO_END:                         ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: COMMON
;; STATE: low shift Dead_UPPER
;; KEYBOARD TYPES: G
;; TABLE TYPE: Flag Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_PL_UP_END-$               ;; length of state section
   DB    DEAD_UPPER                    ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;; Set Flag Table
   DW    1                             ;; number of entries
   DB    26                            ;;
   FLAG  DIARESIS                      ;;
                                       ;;
COM_PL_UP_END:                         ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: COMMON
;; STATE: Third Shift Dead Key
;; KEYBOARD TYPES: G
;; TABLE TYPE: Flag Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_CZ_TH_END-$               ;; length of state section
   DB    DEAD_THIRD                    ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;; Set Flag Table
   DW    1                             ;; number of entries
   DB    26                            ;;
   FLAG  GRAVE                         ;;
                                       ;;
COM_CZ_TH_END:                         ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Non-alpha Upper Case
;; KEYBOARD: G_KB, P_KB, P12_KB
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 COM_NA_UP_1_END-$	       ;; Length of state section
   DB	 NON_ALPHA_UPPER	       ;;
   DW	 G_KB+P_KB+P12_KB	       ;;
   DB	 -1,-1			       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_005300-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE 	       ;; xlat options:
   DB	  4             	       ;; number of scans
   DB	  7,'?'                        ;;
   DB	 41,'\'                        ;;
   DB	 51,27H 		       ;;    single quote
   DB	 52,'"'                        ;;    double quote
CF_005300:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
COM_NA_UP_1_END:		       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Non-alpha Upper Case              SECONDARY KEYBOARD MODE
;; KEYBOARD: all
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 COM_NA_UP_2_END-$	       ;; Length of state section
   DB	 NON_ALPHA_UPPER_SEC	       ;;
   DW	 ANY_KB            	       ;;
   DB	 -1,-1			       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_005301-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE 	       ;; xlat options:
   DB	  6			       ;; number of scans
   DB     2,161                        ;;  This character may be wrong!!!
   DB     4,156                        ;;  ú
   DB     5,152                        ;;  œ  207 in 850
   DB    10,241                        ;;  Ò
   DB	 41,'ƒ'                        ;;  ƒ  196  This  may not be the right char.
   DB	 52,'ˆ'                        ;;
                                       ;;
CF_005301:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
COM_NA_UP_2_END:		       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: alpha lower Case
;; KEYBOARD: all
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 COM_AL_LC_1_END-$	       ;; Length of state section
   DB	 ALPHA_LOWER	               ;;
   DW	 ANY_KB	                       ;;
   DB	 -1,-1			       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_005305-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE 	       ;; xlat options:
   DB	 5			       ;; number of scans
   DB	 27,135			       ;;
   DB	 40,138			       ;;
   DB	 43,133		               ;;
   DB	 53,130	        	       ;;
   DB	 86,151 		       ;;
                                       ;;
CF_005305:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
COM_AL_LC_1_END:		       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: alpha lower Case                 SECONDARY MODE keyboard
;; KEYBOARD: all                           BLOT out Characters not in
;; TABLE TYPE: Translate                   secondary keyboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 COM_AL_LC_3_END-$	       ;; Length of state section
   DB	 ALPHA_LOWER_SEC	               ;;
   DW	 ANY_KB	                       ;;
   DB	 -1,-1			       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_005306-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE 	       ;; xlat options:
   DB	29			       ;; number of scans
   DB	 8,-1			       ;; BLOT OUT CHAR all with -1
   DB	 9,-1			       ;;
   DB	10,-1		               ;;
   DB	11,-1	        	       ;;
   DB	12,-1 	        	       ;;
   DB	16,-1			       ;; BLOT OUT CHAR
   DB	17,-1			       ;;
   DB	18,-1		               ;;
   DB	20,-1	        	       ;;
   DB	21,-1 	        	       ;;
   DB	22,-1		               ;;
   DB	23,-1	        	       ;;
   DB	26,-1 	        	       ;;
   DB	33,-1			       ;; BLOT OUT CHAR
   DB	34,-1			       ;;
   DB	35,-1		               ;;
   DB	36,-1	        	       ;;
   DB	37,-1 	        	       ;;
   DB	38,-1		               ;;
   DB	40,-1	        	       ;;
   DB   41,-1                          ;;
   DB	43,-1 	        	       ;;
   DB	42,-1			       ;; BLOT OUT CHAR
   DB	44,-1			       ;;
   DB	45,-1		               ;;
   DB	47,-1	        	       ;;
   DB	48,-1 	        	       ;;
   DB	49,-1			       ;; BLOT OUT CHAR
   DB	51,-1			       ;;
   DB	53,-1		               ;;

CF_005306:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
COM_AL_LC_3_END:		       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: alpha UPPER Case                 SECONDARY MODE keyboard
;; KEYBOARD: all                           BLOT out Characters not in
;; TABLE TYPE: Translate                   secondary keyboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 COM_AL_UC_3_END-$	       ;; Length of state section
   DB	 ALPHA_UPPER_SEC	               ;;
   DW	 ANY_KB	                       ;;
   DB	 -1,-1			       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_005307-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE 	       ;; xlat options:
   DB	29			       ;; number of scans
   DB	 3,-1			       ;; BLOT OUT CHAR all with -1
   DB	 6,-1			       ;;
   DB	 7,-1		               ;;
   DB	 8,-1	        	       ;;
   DB	 9,-1 	        	       ;;
   DB	11,-1			       ;; BLOT OUT CHAR
   DB	13,-1			       ;;
   DB	16,-1		               ;;
   DB	17,-1	        	       ;;
   DB	18,-1 	        	       ;;
   DB   20,-1                          ;;
   DB	22,-1		               ;;
   DB	23,-1	        	       ;;
   DB	34,-1			       ;;
   DB	35,-1		               ;;
   DB	36,-1	        	       ;;
   DB	37,-1 	        	       ;;
   DB	38,-1		               ;;
   DB	39,-1			       ;; BLOT OUT CHAR
   DB	40,-1	        	       ;;
   DB	43,-1 	        	       ;;
   DB	44,-1			       ;;
   DB	45,-1		               ;;
   DB	47,-1	        	       ;;
   DB	48,-1 	        	       ;;
   DB	49,-1			       ;; BLOT OUT CHAR
   DB	53,-1		               ;;

CF_005307:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
COM_AL_UC_3_END:		       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Non alpha lower Case
;; KEYBOARD: all
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 COM_NA_LC_1_END-$	       ;; Length of state section
   DB	 NON_ALPHA_LOWER	       ;;
   DW	 ANY_KB	                       ;;
   DB	 -1,-1			       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_005303-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE 	       ;; xlat options:
   DB	  1             	       ;; number of scans
   DB	 41,'/'                        ;;
                                       ;;
CF_005303:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
COM_NA_LC_1_END:		       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: COMMON
;; STATE: Third Shift
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_TS_END-$                ;; length of state section
   DB    THIRD_SHIFT                   ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    COM_TS_T1_END-$             ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    12                            ;; number of entries
   DB     8,123                        ;;   {
   DB     9,125                        ;;   }
   DB    10,91                         ;;   [
   DB    11,93                         ;;   ]
   DB    13,170                        ;;   ™
   DB    27,126                        ;;   ~
   DB    39,248                        ;;   ¯
   DB    41,124                        ;;   |
   DB    44,174                        ;;   Æ
   DB    45,175                        ;;   Ø
   DB    51,60                         ;;   <
   DB    52,62                         ;;   >
                                       ;;
COM_TS_T1_END:                         ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_TS_END:                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: CIRCUMFLEX Lower Case
;; KEYBOARD TYPES: ALL
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_CI_LO_END-$               ;; length of state section
   DB    CIRCUMFLEX_LOWER                   ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    94,0                          ;; error character = standalone accent
                                       ;;
   DW    COM_CI_LO_T1_END-$            ;; Size of xlat table
   DB    STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB    5                             ;; number of scans
   DB	 30,'É' 		       ;; scan code,ASCII - a
   DB	 18,'à'                        ;; scan code,ASCII - e
   DB	 24,'ì'                        ;; scan code,ASCII - o
   DB	 22,'ñ'                        ;; scan code,ASCII - u
   DB	 23,'å'                        ;; scan code,ASCII - i   DB    2                             ;; number of entries
COM_CI_LO_T1_END:                      ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_CI_LO_END:                         ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: COMMON
;; STATE: Circumflex Space Bar
;; KEYBOARD TYPES: P12_KB+G_KB
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_CI_SP_END-$               ;; length of state section
   DB    CIRCUMFLEX_SPACE                   ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    94,0                          ;; error character = standalone accent
                                       ;;
   DW    COM_CI_SP_T1_END-$            ;; Size of xlat table
   DB    STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB    1                             ;; number of scans
   DB    57,94                         ;; error character = standalone accent
COM_CI_SP_T1_END:                      ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_CI_SP_END:                         ;; length of state section
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Diaresis Lower Case
;; KEYBOARD: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 COM_DI_LO_END-$	       ;; Length of state section
   DB	 DIARESIS_LOWER 	       ;;
   DW	 ANY_KB 		       ;;
   DB	 -1,-1			       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_001400-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 3			       ;; number of scans
   DB	 18,'â'                        ;;
   DB	 23,'ã'                        ;;
   DB	 22,'Å'                        ;;
CF_001400:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
COM_DI_LO_END:			       ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Grave Lower Case
;; KEYBOARD: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 COM_GR_LO_END-$	       ;; Length of state section
   DB	 GRAVE_LOWER		       ;;
   DW	 ANY_KB 		       ;;
   DB	 '`',0                        ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_001200-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 3			       ;; number of scans
   DB	 30,'Ö'                        ;;
   DB	 18,'ä'                        ;;
   DB	 22,'ó'                        ;;
CF_001200:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
COM_GR_LO_END:			       ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Tilde Lower
;; KEYBOARD TYPES: Any
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;;
   DW	 COM_TI_LO_K1_END-$		  ;; length of state section
   DB	 TILDE_LOWER		       ;; State ID
   DW	 ANY_KB 	    ;; Keyboard Type
   DB	 07EH,0 		       ;; error character = standalone accent
				       ;;
   DW	 COM_TI_LO_K1_T1_END-$		  ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 0			       ;; number of scans
   				       ;; 863 has no tilde characters
COM_TI_LO_K1_T1_END:			  ;;
				       ;;
   DW	 0			       ;;
				       ;;
COM_TI_LO_K1_END:			  ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Tilde Upper Case
;; KEYBOARD TYPES: Any
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 COM_TI_UP_K1_END-$		  ;; length of state section
   DB	 TILDE_UPPER		      ;; State ID
   DW	 ANY_KB 	     ;; Keyboard Type
   DB	 07EH,0 			;; error character = standalone accent
				       ;;
   DW	 COM_TI_UP_K1_T1_END-$		  ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 0			       ;; number of scans
   				       ;; 863 has no tilde chars.
COM_TI_UP_K1_T1_END:			  ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
COM_TI_UP_K1_END:			  ;; length of state section
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Tilde Space Bar
;; KEYBOARD TYPES: Any,
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 COM_TI_SP_END-$	       ;; length of state section
   DB	 TILDE_SPACE		       ;; State ID
   DW	 ANY_KB 	    ;; Keyboard Type
   DB	 07EH,0 			 ;; error character = standalone accent
				       ;;
   DW	 COM_TI_SP_T1_END-$	       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 1			       ;; number of scans
   DB	 57,07EH		      ;; STANDALONE TILDE
				       ;;
COM_TI_SP_T1_END:		       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
COM_TI_SP_END:			       ;; length of state section
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Acute Lower Case
;; KEYBOARD: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 COM_AC_LO_END-$	       ;; Length of state section
   DB	 ACUTE_LOWER		       ;;
   DW	 ANY_KB 		       ;;
   DB	 -1,-1			       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_001100-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 3			       ;; number of scans
   DB	 18,'Ç'                        ;;
   DB	 24,'¢'                        ;;
   DB	 22,'£'                        ;;
CF_001100:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
COM_AC_LO_END:			       ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Acute Upper Case
;; KEYBOARD: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 COM_AC_UP_END-$	       ;; Length of state section
   DB	 ACUTE_UPPER		       ;;
   DW	 ANY_KB 		       ;;
   DB	 -1,-1			       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_003100-$			;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 1			       ;; number of scans
   DB	 18,'ê'                        ;;
CF_003100:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
COM_AC_UP_END:			       ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: COMMON
;; STATE: Cedilla Lower Case
;; KEYBOARD TYPES: G_KB
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW    COM_CE_LO_END-$               ;; length of state section
   DB    CEDILLA_LOWER                ;; State ID
   DW    G_KB                          ;; Keyboard Type
   DB    0F7H,0                        ;; error character = standalone accent
				       ;;
   DW    COM_CE_LO_T1_END-$            ;; Size of xlat table
   DB    STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB    1                             ;; number of scans
   DB    46,'á'                        ;; scan code,ASCII - á
COM_CE_LO_T1_END:                      ;;
				       ;;
   DW    0                             ;; Size of xlat table - null table
				       ;;
COM_CE_LO_END:                         ;; length of state section
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: COMMON
;; STATE: Cedilla Upper Case
;; KEYBOARD TYPES: G_KB
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW    COM_CE_UP_END-$               ;; length of state section
   DB    CEDILLA_UPPER                ;; State ID
   DW    G_KB                   ;; Keyboard Type
   DB    0F7H,0                        ;; error character = standalone accent
				       ;;
   DW    COM_CE_UP_T1_END-$            ;; Size of xlat table
   DB    STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB    1                             ;; number of scans
   DB    46,'Ä'                        ;;    Ä CEDILLA
COM_CE_UP_T1_END:                      ;;
				       ;;
   DW    0                             ;; Size of xlat table - null table
				       ;;
COM_CE_UP_END:                         ;; length of state section
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: COMMON
;; STATE: Cedilla Space
;; KEYBOARD TYPES: G_KB
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW    COM_CE_SP_END-$             ;; length of state section
   DB    CEDILLA_SPACE                   ;; State ID
   DW    G_KB                          ;; Keyboard Type
   DB    0F7H,0                        ;; error character = standalone accent
				       ;;
   DW    COM_CE_SP_T1_END-$        ;; Size of xlat table
   DB    STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB    1                             ;; number of entries
   DB    57,0F7H                       ;; CEDILLA SPACE
COM_CE_SP_T1_END:                    ;;
				       ;;
   DW    0                             ;; Size of xlat table - null table
				       ;;
COM_CE_SP_END:                       ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CODE PAGE: Any
;; STATE: RUS_MODE
;; KEYBOARD TYPES: All
;; TABLE TYPE: Flag Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_F1_END-$                  ;; length of state section
   DB    RUS_MODE_SET                  ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
                                       ;; Set Flag Table
   DW    1                             ;; number of entries
   DB	 42     		       ;; scan code (left Shift)
   FLAG  RUS_MODE                       ;; flag bit to set
;   DB	 54				;; scan code (right Shift)
;   FLAG  LAT_MODE                      ;; flag bit to set
;   DB    29                            ;; scan code (Ctrl)
;   FLAG  RUS_MODE                      ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_F1_END:                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CODE PAGE: Any
;; STATE: LAT_MODE
;; KEYBOARD TYPES: All
;; TABLE TYPE: Flag Table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW	 COM_F2_END-$		      ;; length of state section
   DB	 LAT_MODE_SET		      ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
                                       ;; Set Flag Table
   DW    1                             ;; number of entries
;   DB	 42     			;; scan code (left Shift)
;   FLAG  LAT_MODE                      ;; flag bit to set
    DB	 54				;; scan code (right Shift)
    FLAG  LAT_MODE                      ;; flag bit to set
;   DB    29                            ;; scan code (Ctrl)
;   FLAG  RUS_MODE                      ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_F2_END:			      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 0			       ;; Last State
COMMON_XLAT_END:                       ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;; CF Specific Translate Section for 863
;;***************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 863
;; STATE: Non Alpha Lower                     SECONDARY KEYBOARD
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    CP863_NA_LO_END-$          ;; length of state section
   DB	 NON_ALPHA_LOWER_SEC           ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    CP863_NA_LO_T1_END-$       ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    10                            ;; number of entries
   DB     3,253                        ;;
   DB     4,166                        ;;
   DB     5,172                        ;;
   DB     6,171                        ;;
   DB     7,173                        ;;
   DB    19,134                        ;;
   DB    31,225                        ;;
   DB    32,235                        ;;
   DB    46,155                        ;;
   DB    52,250                        ;;
                                       ;;
CP863_NA_LO_T1_END:                    ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
CP863_NA_LO_END:                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
 PUBLIC CF_863_XLAT                    ;;
CF_863_XLAT:                           ;;
                                       ;;
   DW     CP863_XLAT_END-$             ;; length of section
   DW     863                          ;;
                                       ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 863
;; STATE: Alpha Upper Case
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP863_AL_UP_END-$	       ;; length of state section
   DB	 ALPHA_UPPER		       ;; State ID
   DW	 ANY_KB 		       ;; Keyboard Type
   DB	 -1,-1			       ;; error character = standalone accent
				       ;;
   DW	 CP863_AL_UP_T1_END-$	       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 5			       ;; number of scans
   DB	 27,128			       ;;
   DB	 40,145			       ;;
   DB	 43,142		               ;;
   DB	 53,144	        	       ;;
   DB	 86,157 		       ;;
				       ;;
CP863_AL_UP_T1_END:		       ;;
 			               ;;
   DW    0			       ;; Size of xlat table - null table
				       ;;
CP863_AL_UP_END:		       ;; length of state section
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 863
;; STATE: Grave Upper Case
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP863_GR_UP_END-$	       ;; length of state section
   DB	 GRAVE_UPPER		       ;; State ID
   DW	 ANY_KB 		       ;; Keyboard Type
   DB	 -1,-1			       ;; error character = standalone accent
				       ;;
   DW	 CP863_GR_UP_T1_END-$	       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 3			       ;; number of scans
   DB	 30,142                        ;; Caps grave A
   DB	 18,145         	       ;; Caps grave E
   DB	 22,157			       ;; Caps grave U
				       ;;
CP863_GR_UP_T1_END:		       ;;
 			               ;;
   DW    0			       ;; Size of xlat table - null table
				       ;;
CP863_GR_UP_END:		       ;; length of state section
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 863
;; STATE: Diaresis Upper
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP863_DI_UP_END-$	       ;; length of state section
   DB	 DIARESIS_UPPER 	       ;; State ID
   DW	 ANY_KB 		       ;; Keyboard Type
   DB	 249,0			       ;; error character = standalone accent
				       ;;
   DW	 CP863_DI_UP_T1_END-$	       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 3			       ;; number of scans
   DB	 18,0D3H		       ;;    E diaeresis
   DB	 23,0D8H		       ;;    I diaeresis
   DB    22,154                        ;;    U diaeresis
                                       ;;    A and O not in 863
CP863_DI_UP_T1_END:		       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
CP863_DI_UP_END:		       ;; length of state section
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 863
;; STATE: Diaresis Space Bar
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW  CP863_DI_SP_END-$	       ;; length of state section
   DB	 DIARESIS_SPACE 	       ;; State ID
   DW	 ANY_KB 		       ;; Keyboard Type
   DB	 164,0			       ;; error character = standalone accent
				       ;;
   DW  CP863_DI_SP_T1_END-$	       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 1			       ;; number of scans
   DB	 57,164 		       ;; error character = standalone accent
CP863_DI_SP_T1_END:		       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
CP863_DI_SP_END:		       ;; length of state section
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 863
;; STATE: Circumflex Upper
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP863_CI_UP_END-$	       ;; length of state section
   DB	 CIRCUMFLEX_UPPER	       ;; State ID
   DW	 ANY_KB 		       ;; Keyboard Type
   DB	 94,0			       ;; error character = standalone accent
				       ;;
   DW	 CP863_CI_UP_T1_END-$	       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 5			       ;; number of scans
   DB	 30,132 		       ;;    A circumflex
   DB	 18,146 		       ;;    E circumflex
   DB	 23,168 		       ;;    I circumflex
   DB	 24,153	        	       ;;    O circumflex
   DB	 22,158 		       ;;    U circumflex
CP863_CI_UP_T1_END:		       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
CP863_CI_UP_END:		       ;; length of state section
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 863
;; STATE: Acute  INPUT: Space Bar
;; KEYBOARD: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP863_AC_SP_END-$	       ;; Length of state section
   DB	 ACUTE_SPACE		       ;;
   DW	 ANY_KB 		       ;;
   DB	 0A1H,0 		       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_104500-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 1			       ;; number of scans
   DB	 57,0A1H		       ;;   acute
CF_104500:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
CP863_AC_SP_END:		       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
                                       ;;
   DW     0                            ;; LAST STATE
                                       ;;
CP863_XLAT_END:                        ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;; CF Specific Translate Section for 850
;;***************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
PUBLIC CF_850_XLAT                     ;;
CF_850_XLAT:                           ;;
                                       ;;
   DW     CP850_XLAT_END-$             ;; length of section
   DW     850                          ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Third Shift
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    CP850_TS_END-$                ;; length of state section
   DB    THIRD_SHIFT                   ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    CP850_TS_T1_END-$             ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    14                            ;; number of entries
   DB    03H,040H                      ;;   @
   DB    04H,023H                      ;;   #
   DB    05H,0CFH                      ;; RUBLES sign ˝
   DB    07H,05EH                      ;;   ^
   DB    08H,026H                      ;;   &
   DB    09H,024H                      ;;   $
   DB    0AH,03CH                      ;;   <
   DB    0BH,03EH                      ;;   >
   DB    1AH,05BH                      ;;   [
   DB    1BH,05DH                      ;;   ]
   DB    2BH,07CH                      ;;   |
   DB    33H,03CH                      ;;   <
   DB    34H,03EH                      ;;   >
   DB    35H,02FH                      ;;   /
CP850_TS_T1_END:                       ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
CP850_TS_END:                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Non Alpha Lower    SECONDARY KEYBOARD
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    CP850_NA_LO_END-$          ;; length of state section
   DB	 NON_ALPHA_LOWER_SEC           ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    CP850_NA_LO_T1_END-$       ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    14                            ;; number of entries
   DB     2,251                        ;;
   DB     3,253                        ;;
   DB     4,252                        ;;
   DB     5,172                        ;;
   DB     6,171                        ;;
   DB     7,243                        ;;
   DB    19,244                        ;;
   DB    24,155                        ;;
   DB    25,231                        ;;
   DB    30,145                        ;;
   DB    31,225                        ;;
   DB    32,208                        ;;
   DB    46,189                        ;;
   DB    52,250                        ;;
                                       ;;
CP850_NA_LO_T1_END:                    ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
CP850_NA_LO_END:                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Non-Alpha Upper Case             SECONDARY KEYBOARD
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    CP850_NY_UP_END-$             ;; length of state section
   DB    NON_ALPHA_UPPER_SEC           ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    CP850_NY_UP_T1_END-$          ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    14                            ;; number of entrie
   DB     5,207                        ;;  œ  152 in 863
   DB    12,168                        ;;  ®  168 does not exist in 863
   DB    19,169                        ;;  ©  169 does not exist in 863
   DB    21,190                        ;;  æ  190 does not exist in 863
   DB    24,157                        ;;  ù  157 does not exist in 863
   DB    25,232                        ;;  Ë  232 does not exist in 863
   DB    30,146                        ;;  í  146 does not exist in 863
   DB    31,245                        ;;  ı  245 does not exist in 863
   DB    32,209                        ;;  —  209 does not exist in 863
   DB    33,166                        ;;  ¶  166 does not exist in 863
   DB    42,221                        ;;  ›  221 does not exist in 863
   DB    46,184                        ;;  ∏  184 does not exist in 863
   DB    50,167                        ;;  ß  167 does not exist in 863
   DB    51,158                        ;;  û  158 does not exist in 863

CP850_NY_UP_T1_END:                    ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
CP850_NY_UP_END:                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Alpha Upper Case
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP850_AL_UP_END-$	       ;; length of state section
   DB	 ALPHA_UPPER		       ;; State ID
   DW	 ANY_KB 		       ;; Keyboard Type
   DB	 -1,-1			       ;; error character = standalone accent
				       ;;
   DW	 CP850_AL_UP_T1_END-$	       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 5			       ;; number of scans
   DB	 27,128			       ;;
   DB	 40,212			       ;;
   DB	 43,183		               ;;
   DB	 53,144	        	       ;;
   DB	 86,235 		       ;;
				       ;;
CP850_AL_UP_T1_END:		       ;;
 			               ;;
   DW    0			       ;; Size of xlat table - null table
				       ;;
CP850_AL_UP_END:		       ;; length of state section
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Grave Lower Case
;; KEYBOARD: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP850_GR_LO_END-$	       ;; Length of state section
   DB	 GRAVE_LOWER		       ;;
   DW	 ANY_KB 		       ;;
   DB	 -1,-1			       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_201200-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 2			       ;; number of scans
   DB	 23,'ç'                        ;;
   DB	 24,'ï'                        ;;
CF_201200:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
CP850_GR_LO_END:		       ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Grave Upper Case
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP850_GR_UP_END-$	       ;; length of state section
   DB	 GRAVE_UPPER		       ;; State ID
   DW	 ANY_KB 		       ;; Keyboard Type
   DB	 -1,-1			       ;; error character = standalone accent
				       ;;
   DW	 CP850_GR_UP_T1_END-$	       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 5			       ;; number of scans
   DB	 30,0B7H			;; Caps grave A
   DB	 18,0D4H			;; Caps grave E
   DB	 23,0DEH			;; Caps grave I
   DB	 24,0E3H			;; Caps grave O
   DB	 22,0EBH			;; Caps grave U
				       ;;
CP850_GR_UP_T1_END:		       ;;
 			               ;;
   DW    0			       ;; Size of xlat table - null table
				       ;;
CP850_GR_UP_END:		       ;; length of state section
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Diaresis Lower Case
;; KEYBOARD: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP850_DI_LO_END-$	       ;; Length of state section
   DB	 DIARESIS_LOWER 	       ;;
   DW	 ANY_KB 		       ;;
   DB	 0F9H,0 		       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_201400-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 3			       ;; number of scans
   DB	 30,'Ñ'                        ;;
   DB	 24,'î'                        ;;
   DB	 21,'ò'                        ;;
CF_201400:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
CP850_DI_LO_END:		       ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Diaresis Upper
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP850_DI_UP_END-$	       ;; length of state section
   DB	 DIARESIS_UPPER 	       ;; State ID
   DW	 ANY_KB 		       ;; Keyboard Type
   DB	 249,0			       ;; error character = standalone accent
				       ;;
   DW	 CP850_DI_UP_T1_END-$	       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 5			       ;; number of scans
   DB    30,142                        ;;    A diaeresis
   DB	 18,0D3H		       ;;    E diaeresis
   DB	 23,0D8H		       ;;    I diaeresis
   DB    24,153                        ;;    O diaeresis
   DB    22,154                        ;;    U diaeresis
                                       ;;
CP850_DI_UP_T1_END:		       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
CP850_DI_UP_END:		       ;; length of state section
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Diaresis Space Bar
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW  CP850_DI_SP_END-$	       ;; length of state section
   DB	 DIARESIS_SPACE 	       ;; State ID
   DW	 ANY_KB 		       ;; Keyboard Type
   DB	 249,0			       ;; error character = standalone accent
				       ;;
   DW  CP850_DI_SP_T1_END-$	       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 1			       ;; number of scans
   DB	 57,249 		       ;; error character = standalone accent
CP850_DI_SP_T1_END:		       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
CP850_DI_SP_END:		       ;; length of state section
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Circumflex Upper
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP850_CI_UP_END-$	       ;; length of state section
   DB	 CIRCUMFLEX_UPPER	       ;; State ID
   DW	 ANY_KB 		       ;; Keyboard Type
   DB	 94,0			       ;; error character = standalone accent
				       ;;
   DW	 CP850_CI_UP_T1_END-$	       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 5			       ;; number of scans
   DB	 30,0B6H		       ;;    A circumflex
   DB	 18,0D2H		       ;;    E circumflex
   DB	 23,0D7H		       ;;    I circumflex
   DB	 24,0E2H		       ;;    O circumflex
   DB	 22,0EAH		       ;;    U circumflex
CP850_CI_UP_T1_END:		       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
CP850_CI_UP_END:		       ;; length of state section
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Tilde Lower
;; KEYBOARD TYPES: Any
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;;
    DW    CP850_TI_LO_END-$               ;; length of state section
    DB    TILDE_LOWER                   ;; State ID
    DW    ANY_KB             ;; Keyboard Type
    DB    07EH,0                        ;; error character = standalone accent
					;;
    DW    CP850_TI_LO_T1_END-$            ;; Size of xlat table
    DB    STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
    DB    3                             ;; number of scans
    DB    30,0C6H                       ;; scan code,ASCII - a tilde
    DB    24,0E4H                       ;; scan code,ASCII - o tilde
    DB	  49,0A4h                       ;; scan code - n tilde
                                        ;;
 CP850_TI_LO_T1_END:                    ;;
					;;
    DW    0                             ;;
					;;
 CP850_TI_LO_END:                         ;;
					;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Tilde Upper Case
;; KEYBOARD TYPES: Any
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;;
    DW    CP850_TI_UP_END-$               ;; length of state section
    DB    TILDE_UPPER                  ;; State ID
    DW    ANY_KB            ;; Keyboard Type
    DB    07eH,0                       ;; error character = standalone accent
					;;
    DW    CP850_TI_UP_T1_END-$            ;; Size of xlat table
    DB    STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
    DB    3                             ;; number of scans
    DB    30,0C7H                       ;; scan code,ASCII - A tilde
    DB    24,0E5H                       ;; scan code,ASCII - O tilde
    DB    49,0A4H                       ;;
                                        ;;
 CP850_TI_UP_T1_END:                    ;;
					;;
    DW    0                             ;; Size of xlat table - null table
					;;
 CP850_TI_UP_END:                       ;; length of state section
					;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Acute Lower Case
;; KEYBOARD: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP850_AC_LO_END-$	       ;; Length of state section
   DB	 ACUTE_LOWER		       ;;
   DW	 ANY_KB 		       ;;
   DB	 0EFH,0 		       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_201100-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 3			       ;; number of scans
   DB	 30,'†'                        ;;
   DB	 23,'°'                        ;;
   DB	 21,0ECH		       ;; y acute
CF_201100:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
CP850_AC_LO_END:		       ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Acute Upper Case
;; KEYBOARD: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP850_AC_UP_END-$	       ;; Length of state section
   DB	 ACUTE_UPPER		       ;;
   DW	 ANY_KB 		       ;;
   DB	 0EFH,0 		       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_203100-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 5			       ;; number of scans
   DB	 30,0B5H		       ;;    A acute
   DB	 23,0D6H		       ;;    I acute
   DB	 24,0E0H		       ;;    O acute
   DB	 22,0E9H		       ;;    U acute
   DB	 21,0EDH		       ;;    Y acute
CF_203100:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
CP850_AC_UP_END:		       ;;
				       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Acute  INPUT: Space Bar
;; KEYBOARD: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				       ;;
   DW	 CP850_AC_SP_END-$	       ;; Length of state section
   DB	 ACUTE_SPACE		       ;;
   DW	 ANY_KB 		       ;;
   DB	 0EFH,0 		       ;; Buffer entry for error character
				       ;; Set Flag Table
   DW	 CF_204500-$		       ;; Size of xlat table
   DB	 STANDARD_TABLE+ZERO_SCAN      ;; xlat options:
   DB	 1			       ;; number of scans
   DB	 57,0EFH		       ;;   acute
CF_204500:			       ;;
				       ;;
   DW	 0			       ;; Size of xlat table - null table
				       ;;
CP850_AC_SP_END:		       ;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	 Last state for 850		;;
                                       ;;
                                       ;;
   DW    0                             ;; LAST STATE
                                       ;;
CP850_XLAT_END:                        ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
				       ;;
				       ;;
CODE	 ENDS			       ;;
	 END			       ;;
