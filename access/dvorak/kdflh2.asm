        PAGE    ,132
        TITLE   PC DOS 3.3 Keyboard Definition File

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PC DOS 5.0 - NLS Support - Keyboard Defintion File
;; (c) Copyright Microsoft Corp 1992...
;;
;; This file contains the keyboard tables for Left single-handed
;; key layout
;; Linkage Instructions:
;;      Refer to KDF.ASM.
;;
;;
;; Author:     YSt - MS WPG Ireland September 1992
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
        INCLUDE KEYBSHAR.INC           ;;
        INCLUDE POSTEQU.INC            ;;
        INCLUDE KEYBMAC.INC            ;;
                                       ;;
        PUBLIC LH2_LOGIC                ;;
        PUBLIC LH2_437_XLAT             ;;
        PUBLIC LH2_850_XLAT             ;;
                                       ;;
CODE    SEGMENT PUBLIC 'CODE'          ;;
        ASSUME CS:CODE,DS:CODE         ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard translate table options are a liner search table
;; (TYPE_2_TAB) and ASCII entries ONLY (ASCII_ONLY)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
STANDARD_TABLE      EQU   TYPE_2_TAB+ASCII_ONLY
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;; LH State Logic
;;***************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
                                       ;;
LH2_LOGIC:

   DW  LOGIC_END-$                     ;; length
                                       ;;
   DW  0                               ;; special features
                                       ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; COMMANDS START HERE
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPTIONS:  If we find a scan match in
;; an XLATT or SET_FLAG operation then
;; exit from INT 9.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   OPTION EXIT_IF_FOUND                ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Dead key definitions must come before
;;  dead key translations to handle
;;  dead key + dead key.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   IFF  EITHER_ALT,NOT                 ;;
   ANDF EITHER_CTL,NOT                 ;;
      IFF EITHER_SHIFT                 ;;
          SET_FLAG DEAD_UPPER          ;;
      ELSEF                            ;;
          SET_FLAG DEAD_LOWER          ;;
      ENDIFF                           ;;
   ENDIFF                              ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACUTE ACCENT TRANSLATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
ACUTE_PROC:                            ;;
                                       ;;
   IFF ACUTE,NOT                       ;;
      GOTO DIARESIS_PROC               ;;
      ENDIFF                           ;;
                                       ;;
      RESET_NLS                        ;;
      IFF R_ALT_SHIFT,NOT              ;;
         XLATT ACUTE_SPACE             ;;
      ENDIFF                           ;;
      IFF EITHER_CTL,NOT               ;;
      ANDF EITHER_ALT,NOT              ;;
         IFF EITHER_SHIFT              ;;
            IFF CAPS_STATE             ;;
               XLATT ACUTE_LOWER       ;;
            ELSEF                      ;;
               XLATT ACUTE_UPPER       ;;
            ENDIFF                     ;;
         ELSEF                         ;;
            IFF CAPS_STATE             ;;
               XLATT ACUTE_UPPER       ;;
            ELSEF                      ;;
               XLATT ACUTE_LOWER       ;;
            ENDIFF                     ;;
         ENDIFF                        ;;
      ENDIFF                           ;;
                                       ;;
INVALID_ACUTE:                         ;;
      PUT_ERROR_CHAR ACUTE_LOWER       ;; If we get here then either the XLATT
      BEEP                             ;; failed or we are ina bad shift state.
      GOTO NON_DEAD                    ;; Either is invalid so BEEP and fall
                                       ;; through to generate the second char.
                                       ;; Note that the dead key flag will be
                                       ;; reset before we get here.
                                       ;;
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
         XLATT DIARESIS_SPACE          ;;  exist for 437 so beep for
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
      PUT_ERROR_CHAR DIARESIS_SPACE    ;; standalone accent
      BEEP                             ;; Invalid dead key combo.
      GOTO NON_DEAD                    ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GRAVE ACCENT TRANSLATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
GRAVE_PROC:                            ;;
                                       ;;
   IFF GRAVE,NOT                       ;;
      GOTO CIRCUMFLEX_PROC             ;;
      ENDIFF                           ;;
                                       ;;
      RESET_NLS                        ;;
      IFF R_ALT_SHIFT,NOT              ;;
         XLATT GRAVE_SPACE             ;;
      ENDIFF                           ;;
      IFF EITHER_CTL,NOT               ;;
      ANDF EITHER_ALT,NOT              ;;
        IFF EITHER_SHIFT               ;;
           IFF CAPS_STATE              ;;
              XLATT GRAVE_LOWER        ;;
           ELSEF                       ;;
              XLATT GRAVE_UPPER        ;;
           ENDIFF                      ;;
        ELSEF                          ;;
           IFF CAPS_STATE,NOT          ;;
              XLATT GRAVE_LOWER        ;;
           ELSEF                       ;;
              XLATT GRAVE_UPPER        ;;
           ENDIFF                      ;;
        ENDIFF                         ;;
      ENDIFF                           ;;
                                       ;;
INVALID_GRAVE:                         ;;
      PUT_ERROR_CHAR GRAVE_LOWER       ;; standalone accent
      BEEP                             ;; Invalid dead key combo.
      GOTO NON_DEAD                    ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CIRCUMFLEX ACCENT TRANSLATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
CIRCUMFLEX_PROC:                       ;;
                                       ;;
   IFF CIRCUMFLEX,NOT                  ;;
      GOTO NON_DEAD                    ;;
      ENDIFF                           ;;
                                       ;;
      RESET_NLS                        ;;
      IFF R_ALT_SHIFT,NOT              ;;
         XLATT CIRCUMFLEX_SPACE        ;;
      ENDIFF                           ;;
      IFF EITHER_CTL,NOT               ;;
      ANDF EITHER_ALT,NOT              ;;
        IFF EITHER_SHIFT               ;;
           IFF CAPS_STATE              ;;
              XLATT CIRCUMFLEX_LOWER   ;;
           ELSEF                       ;;
              XLATT CIRCUMFLEX_UPPER   ;;
           ENDIFF                      ;;
        ELSEF                          ;;
           IFF CAPS_STATE,NOT          ;;
              XLATT CIRCUMFLEX_LOWER   ;;
           ELSEF                       ;;
              XLATT CIRCUMFLEX_UPPER   ;;
           ENDIFF                      ;;
        ENDIFF                         ;;
      ENDIFF                           ;;
                                       ;;
INVALID_CIRCUMFLEX:                    ;;
      PUT_ERROR_CHAR CIRCUMFLEX_LOWER  ;; standalone accent
      BEEP                             ;; Invalid dead key combo.
      GOTO NON_DEAD                    ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Upper, lower and third shifts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
NON_DEAD:                              ;;
                                       ;;
   IFKBD G_KB+P12_KB                   ;; Avoid accidentally translating
   ANDF LC_E0                          ;;  the "/" on the numeric pad of the
      EXIT_STATE_LOGIC                 ;;   G keyboard
   ENDIFF                              ;;
;;***BD ADDED FOR ALT, CTRL CASES      ;;
      IFF EITHER_CTL,NOT               ;;
         IFF  ALT_SHIFT                ;; ALT - case
         ANDF R_ALT_SHIFT,NOT          ;;
            XLATT ALT_CASE             ;;
         ENDIFF                        ;;
      ELSEF                            ;;
         IFF EITHER_ALT,NOT            ;; CTRL - case
            XLATT CTRL_CASE            ;;
         ENDIFF                        ;;
      ENDIFF                           ;;
;;***BD END OF ADDITION
                                       ;;
   IFF  EITHER_ALT,NOT                 ;; Lower and upper case.  Alphabetic
   ANDF EITHER_CTL,NOT                 ;; keys are affected by CAPS LOCK.
      IFF EITHER_SHIFT                 ;; Numeric keys are not.
;;***BD ADDED FOR NUMERIC PAD
          IFF NUM_STATE,NOT            ;;
              XLATT NUMERIC_PAD        ;;
          ENDIFF                       ;;
;;***BD END OF ADDITION
          XLATT NON_ALPHA_UPPER        ;;
          IFF CAPS_STATE               ;;
              XLATT ALPHA_LOWER        ;;
          ELSEF                        ;;
              XLATT ALPHA_UPPER        ;;
          ENDIFF                       ;;
      ELSEF                            ;;
;;***BD ADDED FOR NUMERIC PAD
          IFF NUM_STATE                ;;
              XLATT NUMERIC_PAD        ;;
          ENDIFF                       ;;
;;***BD END OF ADDITION
          XLATT NON_ALPHA_LOWER        ;;
          IFF CAPS_STATE               ;;
             XLATT ALPHA_UPPER         ;;
          ELSEF                        ;;
             XLATT ALPHA_LOWER         ;;
          ENDIFF                       ;;
      ENDIFF                           ;;
   ELSEF                               ;;
      IFF EITHER_SHIFT,NOT             ;;
          IFKBD XT_KB+AT_KB      ;;
              IFF  EITHER_CTL          ;;
              ANDF ALT_SHIFT           ;;
                  XLATT THIRD_SHIFT    ;;
              ENDIFF                   ;;
          ELSEF                        ;;
              IFF EITHER_CTL,NOT       ;;
              ANDF R_ALT_SHIFT         ;;
                  XLATT THIRD_SHIFT    ;;
              ENDIFF                   ;;
           ENDIFF                      ;;
      ENDIFF                           ;;
   ENDIFF                              ;;
                                       ;;
   EXIT_STATE_LOGIC                    ;;
                                       ;;
LOGIC_END:                             ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;; LH Common Translate Section
;; This section contains translations for the lower 128 characters
;; only since these will never change from code page to code page.
;; In addition the dead key "Set Flag" tables are here since the
;; dead keys are on the same keytops for all code pages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
 PUBLIC LH2_COMMON_XLAT                 ;;
LH2_COMMON_XLAT:                        ;;
                                       ;;
   DW    COMMON_XLAT_END-$             ;; length of section
   DW    -1                            ;; code page
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;;***BD - ADDED FOR ALT CASE
;;******************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Alt Case
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_ALT_K2_END-$              ;; length of state section
   DB    ALT_CASE                      ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    COM_ALT_K2_T1_END-$           ;; Size of xlat table
   DB    TYPE_2_TAB                    ;; xlat options:
   DB    0                             ;; number of entries
;;***BD THIS ENTRY IS A TEST ENTRY
;; DB    53,225,0                      ;; TEST ENTRY
COM_ALT_K2_T1_END:                     ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_ALT_K2_END:                        ;;
                                       ;;
;;******************************
;;***BD - ADDED FOR CTRL CASE
;;******************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Ctrl Case
;; KEYBOARD TYPES: G_KB+P12_KB+AT
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_CTRL_K2_END-$             ;; length of state section
   DB    CTRL_CASE                     ;; State ID
   DW    G_KB+P12_KB+AT_KB             ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    COM_CTRL_K2_T1_END-$          ;; Size of xlat table
   DB    TYPE_2_TAB                    ;; xlat options:
   DB    0                             ;; number of entries
;;***BD THIS ENTRY IS A TEST ENTRY
;; DB    53,226,0                      ;; TEST ENTRY
;   DB    43,-1,-1                      ;; invalid slash
;   DB    41,28,41                      ;; valid slash
COM_CTRL_K2_T1_END:                    ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_CTRL_K2_END:                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Non-Alpha Lower Case
;; KEYBOARD TYPES: ANY_KB
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_NA_LO_K1_END-$               ;; length of state section
   DB    NON_ALPHA_LOWER               ;; State ID
   DW    G_KB+P12_KB                   ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    COM_NA_LO_K1_T1_END-$            ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    20                            ;; number of entries
   DB    002H, '['                     ;;
   DB    003H, ']'                     ;;
   DB    004H, '/'                     ;;
   DB    00AH, '4'                     ;;
   DB    00BH, '3'                     ;;
   DB    00CH, '2'                     ;;
   DB    00DH, '1'                     ;;
   DB    010H, ';'                     ;;
   DB    018H, '.'                     ;;
   DB    019H, '6'                     ;;
   DB    01AH, '5'                     ;;
   DB    01BH, '='                     ;;
   DB    01EH, '-'                     ;;
   DB    027H, '8'                     ;;
   DB    028H, '7'                     ;;
   DB    02BH, 05CH
   DB    02CH, 027H                    ;;
   DB    033H, ','                     ;;
   DB    034H, '0'                     ;;
   DB    035H, '9'                     ;;
COM_NA_LO_K1_T1_END:                      ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_NA_LO_K1_END:                         ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Non-Alpha Lower Case
;; KEYBOARD TYPES: AT
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_NA_LO_K2_END-$               ;; length of state section
   DB    NON_ALPHA_LOWER               ;; State ID
   DW    AT_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    COM_NA_LO_K2_T1_END-$            ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    20                            ;; number of entries
   DB    002H, '['                     ;;
   DB    003H, ']'                     ;;
   DB    004H, '/'                     ;;
   DB    00AH, '4'                     ;;
   DB    00BH, '3'                     ;;
   DB    00CH, '2'                     ;;
   DB    00DH, '1'                     ;;
   DB    010H, ';'                     ;;
   DB    018H, '.'                     ;;
   DB    019H, '6'                     ;;
   DB    01AH, '5'                     ;;
   DB    01BH, '='                     ;;
   DB    01EH, '-'                     ;;
   DB    027H, '8'                     ;;
   DB    028H, '7'                     ;;
   DB    02BH, 05CH
   DB    02CH, 027H                    ;;
   DB    033H, ','                     ;;
   DB    034H, '0'                     ;;
   DB    035H, '9'                     ;;
COM_NA_LO_K2_T1_END:                      ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_NA_LO_K2_END:                         ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Non-Alpha Lower Case
;; KEYBOARD TYPES: XT_KB
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_NA_LO_K3_END-$               ;; length of state section
   DB    NON_ALPHA_LOWER               ;; State ID
   DW    XT_KB                   ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    COM_NA_LO_K3_T1_END-$            ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    20                            ;; number of entries
   DB    002H, '['                     ;;
   DB    003H, ']'                     ;;
   DB    004H, '/'                     ;;
   DB    00AH, '4'                     ;;
   DB    00BH, '3'                     ;;
   DB    00CH, '2'                     ;;
   DB    00DH, '1'                     ;;
   DB    010H, ';'                     ;;
   DB    018H, '.'                     ;;
   DB    019H, '6'                     ;;
   DB    01AH, '5'                     ;;
   DB    01BH, '='                     ;;
   DB    01EH, '-'                     ;;
   DB    027H, '8'                     ;;
   DB    028H, '7'                     ;;
   DB    02BH, 05CH
   DB    02CH, 027H                    ;;
   DB    033H, ','                     ;;
   DB    034H, '0'                     ;;
   DB    035H, '9'                     ;;
COM_NA_LO_K3_T1_END:                      ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_NA_LO_K3_END:                         ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Non-Alpha Upper Case
;; KEYBOARD TYPES: G_KB+P12_KB+
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_NA_UP_K1_END-$               ;; length of state section
   DB    NON_ALPHA_UPPER               ;; State ID
   DW    G_KB+P12_KB                   ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    COM_NA_UP_T1_K1_END-$            ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    20                            ;; number of entries
   DB    002H, '{'                     ;;
   DB    003H, '}'                     ;;
   DB    004H, '?'                     ;;
   DB    00AH, '$'                     ;;
   DB    00BH, '#'                     ;;
   DB    00CH, '@'                     ;;
   DB    00DH, '!'                     ;;
   DB    010H, ':'                     ;;
   DB    018H, '>'                     ;;
   DB    019H, '^'                     ;;
   DB    01AH, '%'                     ;;
   DB    01BH, '+'                     ;;
   DB    01EH, '_'                     ;;
   DB    027H, '*'                     ;;
   DB    02CH, '"'                     ;;
   DB    02BH, '|'                    ;;
   DB    033H, '<'                     ;;
   DB    034H, ')'                     ;;
   DB    035H, '('                     ;;
COM_NA_UP_T1_K1_END:                      ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_NA_UP_K1_END:                         ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Non-Alpha Upper Case
;; KEYBOARD TYPES: XT_KB
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_NA_UP_K2_END-$               ;; length of state section
   DB    NON_ALPHA_UPPER               ;; State ID
   DW    XT_KB                   ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    COM_NA_UP_T1_K2_END-$            ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    20                            ;; number of entries
   DB    002H, '{'                     ;;
   DB    003H, '}'                     ;;
   DB    004H, '?'                     ;;
   DB    00AH, '$'                     ;;
   DB    00BH, '#'                     ;;
   DB    00CH, '@'                     ;;
   DB    00DH, '!'                     ;;
   DB    010H, ':'                     ;;
   DB    018H, '>'                     ;;
   DB    019H, '^'                     ;;
   DB    01AH, '%'                     ;;
   DB    01BH, '+'                     ;;
   DB    01EH, '_'                     ;;
   DB    027H, '*'                     ;;
   DB    02CH, '"'                     ;;
   DB    02BH, '|'                    ;;
   DB    033H, '<'                     ;;
   DB    034H, ')'                     ;;
   DB    035H, '('                     ;;
COM_NA_UP_T1_K2_END:                      ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_NA_UP_K2_END:                         ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: Common
;; STATE: Non-Alpha Upper Case
;; KEYBOARD TYPES: AT_KB
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    COM_NA_UP_K3_END-$               ;; length of state section
   DB    NON_ALPHA_UPPER               ;; State ID
   DW    AT_KB                         ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    COM_NA_UP_T1_K3_END-$            ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    20                            ;; number of entries
   DB    002H, '{'                     ;;
   DB    003H, '}'                     ;;
   DB    004H, '?'                     ;;
   DB    00AH, '$'                     ;;
   DB    00BH, '#'                     ;;
   DB    00CH, '@'                     ;;
   DB    00DH, '!'                     ;;
   DB    010H, ':'                     ;;
   DB    018H, '>'                     ;;
   DB    019H, '^'                     ;;
   DB    01AH, '%'                     ;;
   DB    01BH, '+'                     ;;
   DB    01EH, '_'                     ;;
   DB    027H, '*'                     ;;
   DB    02CH, '"'                     ;;
   DB    02BH, '|'                    ;;
   DB    033H, '<'                     ;;
   DB    034H, ')'                     ;;
   DB    035H, '('                     ;;
COM_NA_UP_T1_K3_END:                      ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
COM_NA_UP_K3_END:                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: COMMON
;; STATE: Alpha Lower Case
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    CPCOM_A_K1_LO_END-$            ;; length of state section
   DB    ALPHA_LOWER                   ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
                                       ;;
   DW    CPCOM_A_LO_K1_T01_END-$       ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    25                            ;; Number of entries
   DB    5, 'p'                        ;;   j
   DB    6, 'f'                        ;;   l
   DB    7, 'm'                        ;;   m
   DB    8, 'l'                        ;;   f
   DB    9 , 'j'                       ;;   p
   DB    11H, 'q'                       ;;   q
   DB    12H, 'b'                           ;;   o
   DB    13H, 'y'                           ;;   r
   DB    14H, 'u'                           ;;   s
   DB    15H, 'r'                           ;;   u
   DB    16H, 's'                           ;;   y
   DB    17H, 'o'                           ;;   b
   DB    1FH, 'k'                           ;;   z
   DB    20H, 'c'                           ;;   a
   DB    21H, 'd'                           ;;   e
   DB    22H, 't'                           ;;   h
   DB    23H, 'h'                           ;;   t
   DB    24H, 'e'                           ;;   d
   DB    25H, 'a'                           ;;   c
   DB    27H, 'z'                           ;;   k
   DB    2EH, 'g'                           ;;   x
   DB    2FH, 'v'                           ;;   i
   DB    30H, 'w'                           ;;   n
   DB    31H, 'n'                           ;;   w
   DB    32H, 'i'                           ;;   v
CPCOM_A_LO_K1_T01_END:           ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
CPCOM_A_K1_LO_END:                      ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: COMMON
;; STATE: Alpha Upper Case
;; KEYBOARD TYPES: All
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    CPCOM_A_K1_UP_END-$            ;; length of state section
   DB    ALPHA_UPPER                   ;; State ID
   DW    ANY_KB                        ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    CPCOM_A_UP_K1_T01_END-$         ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    25                            ;; Number of entries
   DB    5, 'P'                        ;;   j
   DB    6, 'F'                        ;;   l
   DB    7, 'M'                        ;;   m
   DB    8, 'L'                        ;;   f
   DB    9 , 'J'                       ;;   p
   DB    11H, 'Q'                       ;;   q
   DB    12H, 'B'                           ;;   o
   DB    13H, 'Y'                           ;;   r
   DB    14H, 'U'                           ;;   s
   DB    15H, 'R'                           ;;   u
   DB    16H, 'S'                           ;;   y
   DB    17H, 'O'                           ;;   b
   DB    1FH, 'K'                           ;;   z
   DB    20H, 'C'                           ;;   a
   DB    21H, 'D'                           ;;   e
   DB    22H, 'T'                           ;;   h
   DB    23H, 'H'                           ;;   t
   DB    24H, 'E'                           ;;   d
   DB    25H, 'A'                           ;;   c
   DB    27H, 'Z'                           ;;   k
   DB    2EH, 'G'                           ;;   x
   DB    2FH, 'V'                           ;;   i
   DB    30H, 'W'                           ;;   n
   DB    31H, 'N'                           ;;   w
   DB    32H, 'I'                           ;;   v
CPCOM_A_UP_K1_T01_END:           ;;

                                       ;;
                                       ;;
   DW    0                             ;; Size of xlat table - null table
                                       ;;
CPCOM_A_K1_UP_END:                      ;;
                                       ;;
   DW    0                             ;; Last State
COMMON_XLAT_END:                       ;;
                                       ;;
                                       ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;; LH Specific Translate Section for 437
;; 437 IS COMPLETELY COVERED BY THE COMMON TABLE.
;;***************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
 PUBLIC LH2_437_XLAT                    ;;
LH2_437_XLAT:                           ;;
                                       ;;
   DW     CP437_XLAT_END-$             ;; length of section
   DW     437                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: CP437
;; STATE: Third Shift
;; KEYBOARD TYPES: G_KB+P12_KB
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    CP437_THIRD_K1_END-$            ;; length of state section
   DB    THIRD_SHIFT                   ;; State ID
   DW    G_KB+P12_KB                   ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    CP437_THIRD_K1_T1_END-$            ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    1                             ;; number of entries
   DB    41,0DDH                       ;; Solid vertical bar - graphics block
CP437_THIRD_K1_T1_END:                      ;;
                                       ;;
   DW    0                             ;; Last xlat table
CP437_THIRD_K1_END:                         ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   DW     0                            ;; LAST STATE
                                       ;;
CP437_XLAT_END:                        ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;; LH Specific Translate Section for 850
;;***************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
 PUBLIC LH2_850_XLAT                    ;;
LH2_850_XLAT:                           ;;
                                       ;;
   DW     CP850_XLAT_END-$             ;; length of section
   DW     850                          ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE PAGE: CP850
;; STATE: Third Shift
;; KEYBOARD TYPES: G_KB+P12_KB
;; TABLE TYPE: Translate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
   DW    CP850_THIRD_K1_END-$            ;; length of state section
   DB    THIRD_SHIFT                   ;; State ID
   DW    G_KB+P12_KB                   ;; Keyboard Type
   DB    -1,-1                         ;; Buffer entry for error character
                                       ;;
   DW    CP850_THIRD_K1_T1_END-$            ;; Size of xlat table
   DB    STANDARD_TABLE                ;; xlat options:
   DB    1                             ;; number of entries
   DB    41,07CH                       ;; Solid vertical bar - |
CP850_THIRD_K1_T1_END:                      ;;
                                       ;;
   DW    0                             ;; Last xlat table
CP850_THIRD_K1_END:                         ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   DW    0                             ;; LAST STATE
                                       ;;
CP850_XLAT_END:                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CODE     ENDS                          ;;
         END                           ;;

