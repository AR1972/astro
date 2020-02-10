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
          RESET_NLS                    ;;
      ENDIFF                           ;;
      IFF RIGHT_SHIFT                   ;;
          BEEP                         ;;
          SET_FLAG RUS_MODE_SET        ;;
       ENDIFF                          ;;
    ENDIFF                             ;;
    EXIT_STATE_LOGIC                   ;;
 ENDIFF                                ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   IFF  EITHER_ALT,NOT                 ;;
   ANDF EITHER_CTL,NOT                 ;;
    IFF RUS_MODE                       ;;
    ANDF LC_E0,NOT                     ;;
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
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CIRCUMFLEX ACCENT TRANSLATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   IFF CIRCUMFLEX,NOT                  ;;
      GOTO DIARESIS_PROC               ;;
      ENDIFF                           ;;
                                       ;;
      RESET_NLS1                       ;;
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
      RESET_NLS1                       ;;
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
      GOTO NON_DEAD 	               ;;
      ENDIFF			       ;;
				       ;;
      RESET_NLS1 		       ;;
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
      IFF RUS_MODE                      ;;
      ANDF LC_E0,NOT                    ;; Enhanced keys are not
        IFF EITHER_SHIFT                 ;; Numeric keys are not.
          XLATT NON_ALPHA_UPPER        ;;
          IFF CAPS_STATE               ;;
              XLATT ALPHA_LOWER        ;;
          ELSEF                        ;;
              XLATT ALPHA_UPPER        ;;
          ENDIFF                       ;;
        ELSEF                            ;;
          XLATT NON_ALPHA_LOWER        ;;
          IFF CAPS_STATE               ;;
             XLATT ALPHA_UPPER         ;;
          ELSEF                        ;;
             XLATT ALPHA_LOWER         ;;
          ENDIFF                       ;;
        ENDIFF                           ;; Third and Fourth shifts
      ELSEF
       IFF LC_E0, NOT
         IFF EITHER_SHIFT                 ;;
           XLATT NON_ALPHA_UPPER_LAT    ;;
           IFF CAPS_STATE               ;;
              XLATT ALPHA_LOWER_LAT    ;;
           ELSEF                        ;;
              XLATT ALPHA_UPPER_LAT    ;;
           ENDIFF                       ;;
         ELSEF                            ;;
           XLATT NON_ALPHA_LOWER_LAT    ;;
           IFF CAPS_STATE               ;;
             XLATT ALPHA_UPPER_LAT     ;;
           ELSEF                        ;;
             XLATT ALPHA_LOWER_LAT     ;;
           ENDIFF                           ;;
         ENDIFF
       ENDIFF
      ENDIFF                            ;;
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
   DW    3                             ;; number of entries
   DB    54                            ;; scan code (Right Shift)
   FLAG  LAT_MODE                      ;; flag bit to set
   DB    42                            ;; scan code (Left Shift)
   FLAG  RUS_MODE                      ;; flag bit to set
   DB    29                            ;; scan code (Ctrl)
   FLAG  RUS_MODE                      ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_F1_END:                            ;;
                                       ;;
                                       ;;
                                       ;;
                                       ;;
   DW    0                             ;; Last State
COMMON_XLAT_END:                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;; CF Specific Translate Section for 863
;;***************************************
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
                                       ;;
   DW    CP850_NA_Y1_LO_END-$          ;; length of state section
   DB    NON_ALPHA_LOWER_LAT           ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    CP850_NA_LO_Y1_T1_END-$       ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    11                            ;; number of entries
   DB    51, 02CH                      ;;   033H
   DB    52, 02EH                      ;;   034H
   DB    53, 02DH                      ;;   035H
   DB    12, 027H                      ;;   0CH
   DB    13, 02BH                      ;;   0DH
   DB    86, 03Ch                      ;;   056H
   DB    26, 05Bh                      ;;   01AH
   DB    27, 05Ch                      ;;   01BH
   DB    39, 07Ch                      ;;   027H
   DB    40, 05Dh                      ;;   028H
   DB    43, 040h                      ;;   02BH
CP850_NA_LO_Y1_T1_END:                 ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
CP850_NA_Y1_LO_END:                    ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: 850
;; STATE: Non-Alpha Upper Case
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    CP850_NY_UP_END-$             ;; length of state section
   DB    NON_ALPHA_UPPER_LAT           ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    CP850_NY_UP_T1_END-$          ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    17                            ;; number of entrie
   DB    51, 03BH                      ;;   033H
   DB    52, 03AH                      ;;   034H
   DB    53, 05FH                      ;;   035H
   DB    12, 03FH                      ;;   0CH
   DB    13, 02AH                      ;;   0DH
   DB    86, 03EH                      ;;   056H
   DB    3,  022H                      ;;   03h
   DB    7,  026H                      ;;   07h
   DB    8,  02FH                      ;;   08h
   DB    9,  028H                      ;;   09h
   DB    10, 029H                      ;;   0ah
   DB    11, 03dH                      ;;   0bh
   DB    26, 07Bh                      ;;   01AH
   DB    27, 05Ch                      ;;   01BH
   DB    39, 05Eh                      ;;   027H
   DB    40, 07Dh                      ;;   028H
   DB    43, 040h                      ;;   02BH
CP850_NY_UP_T1_END:                    ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
CP850_NY_UP_END:                       ;;
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
   DB	 4			       ;; number of scans
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	 Last state for 850		;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
