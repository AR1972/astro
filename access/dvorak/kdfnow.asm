        PAGE    ,132
        TITLE   MS-DOS 5.0 Keyboard Definition File

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MS-DOS 5.0 - NLS Support - Keyboard Definition File
;; (c) Copyright Microsoft Corp 1992
;;
;;           KDFDV.ASM  - US Dvorak
;;           KDFLH2.ASM  - US left single-handed key layout
;;           KDFRH2.ASM  - US right single-handed key layout
;;           Dummy US   - US
;;
;;             Yuri Starikov -Microsoft September 1992
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
                                       ;;
CODE    SEGMENT PUBLIC 'CODE'          ;;
        ASSUME CS:CODE,DS:CODE         ;;
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;; File Header
;;***************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                     ;;
DB   0FFh,'KEYB   '                  ;; signature
DB   8 DUP(0)                        ;; reserved
DW   0460H                           ;; maximum size of Common Xlat Sect (650)
DW   01F0H                           ;; max size of Specific Xlat Sect (350)
DW   0280H                           ;; max size of State Logic (400)
DW   0                               ;;AC000;reserved
DW   4                               ;;AC000 number of IDs
DW   4                               ;M000     ;;AC000 number of languages
DB   'RH'                            ;;(YST);
DW   OFFSET RH2_LANG_ENT,0           ;;(YST); Right single-handed
DB   'LH'                            ;;(YST);
DW   OFFSET LH2_LANG_ENT,0           ;;(YST); Left single-handed
DB   'DV'                            ;;(YST);
DW   OFFSET DV_LANG_ENT,0           ;;(YST); Left single-handed
DB   'US'                            ;;
DW   OFFSET DUMMY_ENT,0              ;;
DW    987                            ;;(YST)
DW   OFFSET RH2_LANG_ENT,0           ;;(YST)
DW    986                            ;;(YST)
DW   OFFSET LH2_LANG_ENT,0           ;;(YST)
DW    985                            ;;(YST)
DW   OFFSET DV_LANG_ENT,0            ;;(YST)
DW    103                            ;;AN000;
DW   OFFSET DUMMY_ENT,0              ;;AN000;
;                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;; Language Entries
;*****************************************************************************
                                         ;;
     EXTRN RH2_LOGIC:FAR                 ;;
     EXTRN RH2_850_XLAT:FAR              ;;
     EXTRN RH2_437_XLAT:FAR              ;;
                                         ;;
  RH2_LANG_ENT:                           ;; language entry for Yugo (Cyrillic)
    DB   'RH'                            ;;
    DW   987                             ;; ID entry
    DW   OFFSET RH2_LOGIC,0               ;; pointer to LANG kb table
    DB   1                               ;; number of ids
    DB   2                               ;; number of code pages
    DW   437                             ;; code page  ; default to 437 -same as country.sys
    DW   OFFSET RH2_437_XLAT,0            ;; table pointer
    DW   850                             ;; code page
    DW   OFFSET RH2_850_XLAT,0            ;; table pointer
                                         ;;
;*****************************************************************************
     EXTRN LH2_LOGIC:FAR                  ;;
     EXTRN LH2_850_XLAT:FAR               ;;
     EXTRN LH2_437_XLAT:FAR              ;;
                                         ;;
  LH2_LANG_ENT:                           ;; language entry for Yugo (Cyrillic)
    DB   'LH'                            ;;
    DW   986                             ;; ID entry
    DW   OFFSET LH2_LOGIC,0               ;; pointer to LANG kb table
    DB   1                               ;; number of ids
    DB   2                               ;; number of code pages
    DW   437                             ;; code page  ; default to 437 -same as country.sys
    DW   OFFSET LH2_437_XLAT,0            ;; table pointer
    DW   850                             ;; code page
    DW   OFFSET LH2_850_XLAT,0            ;; table pointer
                                         ;;
;*****************************************************************************
                                         ;;
     EXTRN DV_LOGIC:FAR                  ;;
     EXTRN DV_COMMON_XLAT:FAR            ;;
                                         ;;
  DV_LANG_ENT:                           ;; language entry for Yugo (Cyrillic)
    DB   'DV'                            ;;
    DW   985                             ;; ID entry
    DW   OFFSET DV_LOGIC,0               ;; pointer to LANG kb table
    DB   1                               ;; number of ids
    DB   2                               ;; number of code pages
    DW   437                             ;; code page  ; default to 437 -same as country.sys
    DW   OFFSET DV_COMMON_XLAT,0            ;; table pointer
    DW   850                             ;; code page
    DW   OFFSET DV_COMMON_XLAT,0            ;; table pointer
                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DUMMY_ENT:                             ;; language entry
  DB   'XX'                            ;;
  DW   103                             ;;AC000; ID entry
  DW   OFFSET DUMMY_LOGIC,0            ;; pointer to LANG kb table
  DB   1                               ;;AC000; number of ids
  DB   8                               ;;AC000; number of code pages
  DW   437                             ;; code page
  DW   OFFSET DUMMY_XLAT_437,0         ;; table pointer
  DW   850                             ;; code page
  DW   OFFSET DUMMY_XLAT_850,0         ;; table pointer
  DW   852                             ;; code page     [Mihindu 11/30/90]
  DW   OFFSET DUMMY_XLAT_852,0         ;; table pointer
  DW   860                             ;; code page
  DW   OFFSET DUMMY_XLAT_860,0         ;; table pointer
  DW   863                             ;; code page
  DW   OFFSET DUMMY_XLAT_863,0         ;; table pointer
  DW   865                             ;; code page
  DW   OFFSET DUMMY_XLAT_865,0         ;; table pointer
  DW   866                             ;; code page  [YST 3/19/91]
  DW   OFFSET DUMMY_XLAT_866,0         ;; table pointer
  DW   855                             ;; code page  [YST 3/19/91]
  DW   OFFSET DUMMY_XLAT_855,0         ;; table pointer
                                       ;;
DUMMY_LOGIC:                           ;;
   DW  LOGIC_END-$                     ;; length
   DW  0                               ;; special features
   DB  92H,0,0                         ;; EXIT_STATE_LOGIC_COMMAND
LOGIC_END:                             ;;
                                       ;;
DUMMY_XLAT_437:                        ;;
   DW     6                            ;; length of section
   DW     437                          ;; code page
   DW     0                            ;; LAST STATE
                                       ;;
DUMMY_XLAT_850:                        ;;
   DW     6                            ;; length of section
   DW     850                          ;; code page
   DW     0                            ;; LAST STATE
                                       ;;
DUMMY_XLAT_852:                        ;; [Mihindu 11/30/90]
   DW     6                            ;; length of section
   DW     852                          ;; code page
   DW     0                            ;; LAST STATE
                                       ;;
DUMMY_XLAT_860:                        ;;
   DW     6                            ;; length of section
   DW     860                          ;; code page
   DW     0                            ;; LAST STATE
                                       ;;
DUMMY_XLAT_865:                        ;;
   DW     6                            ;; length of section
   DW     865                          ;; code page
   DW     0                            ;; LAST STATE
                                       ;;
DUMMY_XLAT_863:                        ;;
   DW     6                            ;; length of section
   DW     863                          ;; code page
   DW     0                            ;; LAST STATE
                                       ;;
DUMMY_XLAT_866:                        ;;   (YST 3/19/91)
   DW     6                            ;; length of section
   DW     866                          ;; code page
   DW     0                            ;; LAST STATE
                                       ;;
DUMMY_XLAT_855:                        ;;   (YST 3/19/91)
   DW     6                            ;; length of section
   DW     855                          ;; code page
   DW     0                            ;; LAST STATE
                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*****************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                       ;;
CODE     ENDS                          ;;
         END                           ;;
