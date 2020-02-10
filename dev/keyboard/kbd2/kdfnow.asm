;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
	PAGE  ,132
	TITLE MS-DOS 5.0 Keyboard Definition File

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MS-DOS 5.0 - NLS Support - Keyboard Definition File
;;
;; This the file header and table pointers ONLY.
;; The actual tables are contained in seperate source files.
;; These are:
;;       KDFSP.ASM  - Spanish
;;       KDFPO.ASM  - Portuguese
;;       KDFGR.ASM  - German
;;       KDFIT142.ASM  - Italian
;;       KDFIT.ASM  - Italian
;;       KDFFR189.ASM  - French
;;       KDFFR.ASM  - French
;;       KDFSG.ASM  - Swiss German
;;       KDFSF.ASM  - Swiss French
;;       KDFDK.ASM  - Danish
;;       KDFUK168.ASM  - English
;;       KDFUK.ASM  - English
;;       KDFBE.ASM  - Belgium
;;       KDFNL.ASM  - Netherlands
;;       KDFNO.ASM  - Norway
;;       KDFLA.ASM  - Latin American
;;       KDFSV.ASM  - SWEDEN   -----> This moddule is used for both Sweden
;;            and Finland - exact same template
;;       KDFSU.ASM  - Finland  -----> Same module as Sweden eliminated
;;       KDFBR.ASM  - Brazil      [Mihindu 10/30/90 : added Brazil]
;;       KDFPL.ASM  - Poland      [Mihindu 11/30/90 : added Poland]
;;       KDFCZ.ASM  - Czech       [Mihindu 11/30/90 : added Czek]
;;       KDFSL.ASM  - Slovak      [Mihindu 11/30/90 : added Slovak]
;;       KDFHU.ASM  - Hungary     [Mihindu 11/30/90 : added Hungary]
;;       KDFYU.ASM  - Yugoslavia      [Mihindu 11/30/90 : added Yugoslavia]
;;           KDFRO.ASM  - Romanian          [ 02/24/93 : added Romanian ]
;;           KDFIC.ASM  - Iceland           [ 02/24/93 : added Iceland ]
;;           KDFJP.ASM  - JAPAN                    ;M000     ;JP9002, JP9009
;;       Dummy US - US
;; Linkage Instructions:
;;  The following instructions are contained in KDFLINK.BAT:
;;
;;  LINK KDF+KDFSP+KDFGE+KDFFR+KDFIT+KDFPO+KDFUK+KDFSG+KDFDK+KDFEOF;
;;  EXE2BIN KDF.EXE KEYBOARD.SYS
;;
;; Author:     BILL DEVLIN  - IBM Canada Laboratory - May 1986
;; Author:     NICK SAVAGE  - IBM Corp. - May 1986,87
;;
;; DCLove, March 8, 1988 - swapped 437/850 to 850/437 for SG & SF
;; DCLove, March 8, 1988 - uncommented SU(finland)& swapped 437/850 to 850/437
;;         as the Finnish want 850/437 vs. 437/850 for Sweden
;;         did not alter the pointer to kbid 153 to Sweden
;; CNS     April 14 1988 - swapped 437/850 to 850/437 for SP & LA
;; Kusuda  Feb. 1990     - Support Japanese keyboard.     ;M000         ;JP9002
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
							 ;;
							 ;;
CODE  SEGMENT PUBLIC 'CODE'          ;;
	ASSUME CS:CODE,DS:CODE         ;;
							 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;; File Header
;;***************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						 ;;
DB   0FFh,'KEYB   '                  ;; signature
DB   8 DUP(0)          ;; reserved
DW   0460H           ;; maximum size of Common Xlat Sect (650)
DW   01F0H           ;; max size of Specific Xlat Sect (350)
DW   0280H           ;; max size of State Logic (400)
DW   0             ;;AC000;reserved
;;  Number of IDs and languages incremented [Mihindu 10/30/90 : added Brazil]
;;  [Mihindu, 11/30/90 : added 5 more for Eastern Europe]
DW   22          ;;AC000 number of IDs
DW   23            ;M000    ;;AC000 number of languages
DB   'GR'                            ;; LANGUAGE CODE TABLE
DW   OFFSET GE_LANG_ENT,0      ;;
DB   'SP'                            ;;
DW   OFFSET SP_LANG_ENT,0      ;;
DB   'PO'                            ;;
DW   OFFSET PO_LANG_ENT,0      ;;
DB   'FR'                            ;;
DW   OFFSET FR2_LANG_ENT,0       ;;
DB   'DK'                            ;;
DW   OFFSET DK_LANG_ENT,0      ;;
;;DB   'SG'                            ;;
;;DW   OFFSET SG_LANG_ENT,0      ;;
DB   'IT'                            ;;
DW   OFFSET IT2_LANG_ENT,0       ;;
DB   'UK'                            ;;
DW   OFFSET UK2_LANG_ENT,0       ;;
;;DB   'SF'                            ;;
;;DW   OFFSET SF_LANG_ENT,0      ;;
DB   'BE'                            ;;
DW   OFFSET BE_LANG_ENT,0      ;;
DB   'NL'                            ;;
DW   OFFSET NL_LANG_ENT,0      ;;
DB   'NO'                            ;;
DW   OFFSET NO_LANG_ENT,0      ;;
DB   'CF'                            ;;
DW   OFFSET CF_LANG_ENT,0      ;;
DB   'SV'                            ;;
DW   OFFSET SV_LANG_ENT,0      ;;
DB   'SU'                            ;;
DW   OFFSET Su_LANG_ENT,0      ;;
DB   'LA'                            ;;
DW   OFFSET LA_LANG_ENT,0      ;;
DB   'BR'
DW   OFFSET BR_LANG_ENT,0      ;; [Mihindu 10/30/90 : added Brazil]
DB   'PL'
DW   OFFSET PL_LANG_ENT,0      ;; [Mihindu 11/30/90]
DB   'YU'
DW   OFFSET YU_LANG_ENT,0      ;; [Mihindu 11/30/90]
DB   'HU'
DW   OFFSET HU_LANG_ENT,0      ;; [Mihindu 11/30/90]
DB   'RO'
DW   OFFSET RO_LANG_ENT,0      ;;(YST)
DB   'IS'          ;;   �sland !
DW   OFFSET IC_LANG_ENT,0      ;;
DB   'US'          ;;
DW   OFFSET DUMMY_ENT,0        ;;
;DB   'JP'            ;;M000        ;JP9002
;DW   OFFSET JP_LANG_ENT, 0       ;;M000        ;JP9002
DW    172          ;;AN000;ID CODE TABLE ***************************
DW   OFFSET SP_LANG_ENT,0      ;;AN000;
DW    163          ;;AN000;
DW   OFFSET PO_LANG_ENT,0      ;;AN000;
DW    120          ;;AN000;
DW   OFFSET FR1_LANG_ENT,0       ;;AN000;
DW    189          ;;AN000;
DW   OFFSET FR2_LANG_ENT,0       ;;AN000;
DW    159          ;;AN000;
DW   OFFSET DK_LANG_ENT,0      ;;AN000;
;;DW    000          ;;AN000;
;;DW   OFFSET SG_LANG_ENT,0      ;;AN000;
DW    129          ;;AN000;
DW   OFFSET GE_LANG_ENT,0      ;;AN000;
DW    142          ;;AN000;
DW   OFFSET IT1_LANG_ENT,0       ;;AN000;
DW    141          ;;AN000;
DW   OFFSET IT2_LANG_ENT,0       ;;AN000;
DW    168          ;;AN000;
DW   OFFSET UK1_LANG_ENT,0       ;;AN000;
DW    166          ;;AN000;
DW   OFFSET UK2_LANG_ENT,0       ;;AN000;
;;DW    150          ;;AN000;
;;DW   OFFSET SF_LANG_ENT,0      ;;AN000;
DW    120          ;;AN000;
DW   OFFSET BE_LANG_ENT,0      ;;AN000;
DW    143          ;;AN000;
DW   OFFSET NL_LANG_ENT,0      ;;AN000;
DW    155          ;;AN000;
DW   OFFSET NO_LANG_ENT,0      ;;AN000;
DW    058          ;;AN000;
DW   OFFSET CF_LANG_ENT,0      ;;AN000;
DW    153          ;;AN000;
DW   OFFSET SV_LANG_ENT,0      ;;AN000;
DW    171          ;;AN000;
DW   OFFSET LA_LANG_ENT,0      ;;AN000;
DW    161          ;;AN000;[ Added 03/09/93 for msdos6 supplemental disk]
DW   OFFSET IC_LANG_ENT,0      ;;AN000;
DW    275          ;;
DW   OFFSET BR2_LANG_ENT,0       ;; [ Verav : added Brazil IBM layout - Feb 92]
DW    274          ;;
DW   OFFSET BR_LANG_ENT,0      ;; [Mihindu 10/30/90 : added Brazil]
;DW    214
;DW   OFFSET PL_LANG_ENT,0       ;; [Mihindu 11/30/90]
;DW    243
;DW   OFFSET CZ_LANG_ENT,0       ;; [Mihindu 11/30/90]
;DW    245
;DW   OFFSET SL_LANG_ENT,0       ;; [Mihindu 11/30/90]
;DW    234
;DW   OFFSET YU_LANG_ENT,0       ;; [Mihindu 11/30/90]
;DW    208
;DW   OFFSET HU_LANG_ENT,0       ;; [Mihindu 11/30/90]
DW    103          ;;AN000;
DW   OFFSET DUMMY_ENT,0        ;;AN000;
;            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;***************************************
;; Language Entries
;;***************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
							 ;;
	 EXTRN SP_LOGIC:NEAR           ;;
	 EXTRN SP_437_XLAT:NEAR        ;;
	 EXTRN SP_850_XLAT:NEAR        ;;
							 ;;
SP_LANG_ENT:             ;; language entry for SPANISH
	DB   'SP'                            ;;
	DW   172             ;; AN000;ID entry  (ID CODE)
	DW   OFFSET SP_LOGIC,0         ;; pointer to LANG kb table
	DB   1             ;; AN000;number of IDs
	DB   2             ;; number of code pages
	DW   850             ;; code page
	DW   OFFSET SP_850_XLAT,0        ;; table pointer
	DW   437             ;; code page
	DW   OFFSET SP_437_XLAT,0        ;; table pointer
							 ;;
;*****************************************************************************
		EXTRN FR1_LOGIC:NEAR     ;;AN000;
		EXTRN FR1_437_XLAT:NEAR    ;;AN000;
		EXTRN FR1_850_XLAT:NEAR    ;;AN000;
					 ;;
 FR1_LANG_ENT:         ;;AN000; language entry for FRANCE
	 DB 'FR'                             ;;AN000; SECONDARY KEYBOARD ID VALUE
	 DW 120        ;;AN000; ID entry
	 DW OFFSET FR1_LOGIC,0     ;;AN000; pointer to LANG kb table
	 DB 2        ;;AN000; number of code pages
	 DB 2        ;;AN000; number of ids
	 DW 437        ;;AN000; code page
	 DW OFFSET FR1_437_XLAT,0    ;;AN000; table pointer
	 DW 850        ;;AN000; code page
	 DW OFFSET FR1_850_XLAT,0    ;;AN000; table pointer
					 ;;AN000;
;*****************************************************************************
		EXTRN FR2_LOGIC:NEAR     ;;AC000;
		EXTRN FR2_437_XLAT:NEAR    ;;AC000;
		EXTRN FR2_850_XLAT:NEAR    ;;AC000;
					 ;;
 FR2_LANG_ENT:         ;; language entry for FRANCE
	 DB 'FR'                             ;; PRIMARY  KEYBOARD ID VALUE
	 DW 189        ;;AC000; ID entry
	 DW OFFSET FR2_LOGIC,0     ;;AC000; pointer to LANG kb table
	 DB 1        ;;AC000; number of ids
	 DB 2        ;;AC000; number of code pages
	 DW 437        ;;AC000; code page
	 DW OFFSET FR2_437_XLAT,0    ;;AC000; table pointer
	 DW 850        ;;AC000; code page
	 DW OFFSET FR2_850_XLAT,0    ;;AC000; table pointer
					 ;;
;****************************************************************************
	 EXTRN PO_LOGIC:NEAR       ;;AC000;
	 EXTRN PO_850_XLAT:NEAR    ;;AC000;
	 EXTRN PO_860_XLAT:NEAR    ;;AC000;
					 ;;
PO_LANG_ENT:         ;; language entry for PORTUGAL
	DB   'PO'                              ;;
	DW   163         ;;AN000; ID entry
	DW   OFFSET PO_LOGIC,0     ;; pointer to LANG kb table
	DB   1         ;;AC000; number of ids
	DB   2         ;;AC000; number of code pages
	DW   850         ;;AC000; code page
	DW   OFFSET PO_850_XLAT,0    ;;AC000; table pointer
	DW   860         ;;AC000; code page
	DW   OFFSET PO_860_XLAT,0    ;;AC000; table pointer
					 ;;
;*****************************************************************************
	 EXTRN DK_LOGIC:NEAR       ;;
	 EXTRN DK_850_XLAT:NEAR    ;;AC000;
	 EXTRN DK_865_XLAT:NEAR    ;;AC000;
					 ;;
 DK_LANG_ENT:        ;; language entry for DENMARK
	 DB 'DK'                             ;;
	 DW 159        ;;AN000; ID entry
	 DW OFFSET DK_LOGIC,0    ;; pointer to LANG kb table
	 DB 1        ;;AN000;number of ids
	 DB 2        ;;AC000; number of code pages
	 DW 850        ;;AC000; code page
	 DW OFFSET DK_850_XLAT,0     ;;AC000; table pointer
	 DW 865        ;;AC000; code page
	 DW OFFSET DK_865_XLAT,0     ;;AC000; table pointer
					 ;;
;*****************************************************************************
;;	 EXTRN SG_LOGIC:NEAR       ;;
;;	 EXTRN SG_850_XLAT:NEAR    ;;
;;	 EXTRN SG_437_XLAT:NEAR    ;;
					 ;;
;;SG_LANG_ENT:         ;; language entry for SWISS GERMAN
;;	DB   'SG'                              ;;
;;	DW   000         ;;AN001; ID entry
;;	DW   OFFSET SG_LOGIC,0     ;; pointer to LANG kb table
;;	DB   1         ;;AN000; number of ids
;;	DB   2         ;;AC000; number of code pages
;;	DW   850         ;; code page ;;;dcl 850 now default March 8, 1988
;;	DW   OFFSET SG_850_XLAT,0    ;; table pointer
;;	DW   437         ;; code page
;;	DW   OFFSET SG_437_XLAT,0    ;; table pointer
;;					 ;;
;*****************************************************************************
;;	 EXTRN SF_LOGIC:NEAR       ;;
;;	 EXTRN SF_850_XLAT:NEAR    ;;
;;	 EXTRN SF_437_XLAT:NEAR    ;;
;;					 ;;
;;SF_LANG_ENT:         ;; language entry for SWISS FRENCH
;;	DB   'SF'                              ;;
;;	DW   150         ;;AN000; ID entry
;;	DW   OFFSET SF_LOGIC,0     ;; pointer to LANG kb table
;;	DB   1             ;;AN000; number of ids
;;	DB   2         ;;AC000; number of code pages
;;	DW   850         ;; code page ;;;dcl 850 now default March 8, 1988
;;	DW   OFFSET SF_850_XLAT,0    ;; table pointer
;;	DW   437         ;; code page
;;	DW   OFFSET SF_437_XLAT,0    ;; table pointer
					 ;;
;*****************************************************************************
	 EXTRN GE_LOGIC:NEAR       ;;
	 EXTRN GE_437_XLAT:NEAR    ;;
	 EXTRN GE_850_XLAT:NEAR    ;;
					 ;;
GE_LANG_ENT:         ;; language entry for GERMANY
	DB   'GR'                              ;;
	DW   129         ;;AN000; ID entry
	DW   OFFSET GE_LOGIC,0     ;; pointer to LANG kb table
	DB   1             ;;AN000; number of ids
	DB   2         ;;AC000; number of code pages
	DW   437         ;; code page
	DW   OFFSET GE_437_XLAT,0    ;; table pointer
	DW   850         ;; code page
	DW   OFFSET GE_850_XLAT,0    ;; table pointer
					 ;;
;*****************************************************************************
		EXTRN IT1_LOGIC:NEAR     ;;AN000;
		EXTRN IT1_437_XLAT:NEAR    ;;AN000;
		EXTRN IT1_850_XLAT:NEAR    ;;AN000;
					 ;;
 IT1_LANG_ENT:         ;;AN000; language entry for ITALY
	 DB 'IT'                             ;;AN000; SECONDARY KEYBOARD ID VALUE
	 DW 142        ;;AN000; ID entry
	 DW OFFSET IT1_LOGIC,0     ;;AN000; pointer to LANG kb table
	 DB 2        ;;AN000;number of ids
	 DB 2        ;;AN000; number of code pages
	 DW 437        ;;AN000; code page
	 DW OFFSET IT1_437_XLAT,0    ;;AN000; table pointer
	 DW 850        ;;AN000; code page
	 DW OFFSET IT1_850_XLAT,0    ;;AN000; table pointer
					 ;;
;*****************************************************************************
		EXTRN IT2_LOGIC:NEAR     ;;
		EXTRN IT2_437_XLAT:NEAR    ;;
		EXTRN IT2_850_XLAT:NEAR    ;;
					 ;;
 IT2_LANG_ENT:         ;;AC000; language entry for ITALY
	 DB 'IT'                             ;;AC000;  PRIMARY KEYBOARD ID VALUE
	 DW 141        ;;AN000; ID entry
	 DW OFFSET IT2_LOGIC,0     ;;AN000; pointer to LANG kb table
	 DB 1       ;;AC000; number of ids
	 DB 2        ;;AC000; number of code pages
	 DW 437        ;;AC000; code page
	 DW OFFSET IT2_437_XLAT,0    ;;AC000; table pointer
	 DW 850        ;;AC000; code page
	 DW OFFSET IT2_850_XLAT,0    ;;AC000; table pointer
					 ;;
;*****************************************************************************
		EXTRN UK1_LOGIC:FAR      ;;AN000;
		EXTRN UK1_437_XLAT:FAR     ;;AN000;
		EXTRN UK1_850_XLAT:FAR     ;;AN000;
					 ;;
 UK1_LANG_ENT:         ;;AN000; language entry for UNITED KINGDOM
	 DB 'UK'                             ;;AN000; SECONDARY KEYBOARD ID VALUE
	 DW 168        ;;AN000; ID entry
	 DW OFFSET UK1_LOGIC,0     ;;AN000; pointer to LANG kb table
	 DB 2       ;;AN000; number of ids
	 DB 2        ;;AN000; number of code pages
	 DW 437        ;;AN000; code page
	 DW OFFSET UK1_437_XLAT,0    ;;AN000; table pointer
	 DW 850        ;;AN000; code page
	 DW OFFSET UK1_850_XLAT,0    ;;AN000; table pointer
					 ;;
;*****************************************************************************
		EXTRN UK2_LOGIC:FAR      ;;AC000;
		EXTRN UK2_437_XLAT:FAR     ;;AC000;
		EXTRN UK2_850_XLAT:FAR     ;;AC000;
					 ;;
 UK2_LANG_ENT:         ;;AN000; language entry for UNITED KINGDOM
	 DB 'UK'                             ;;AC000; PRIMARY KEYBOARD ID VALUE
	 DW 166        ;;AC000; ID entry
	 DW OFFSET UK2_LOGIC,0     ;;AC000; pointer to LANG kb table
	 DB 1       ;; AN000;number of ids
	 DB 2        ;;AN000; number of code pages
	 DW 437        ;;AC000; code page
	 DW OFFSET UK2_437_XLAT,0    ;;AC000; table pointer
	 DW 850        ;;AC000; code page
	 DW OFFSET UK2_850_XLAT,0    ;;AC000; table pointer
					 ;;
;*****************************************************************************
	 EXTRN BE_LOGIC:NEAR       ;;
	 EXTRN BE_437_XLAT:NEAR    ;;
	 EXTRN BE_850_XLAT:NEAR    ;;
					 ;;
BE_LANG_ENT:         ;; language entry for BELGIUM
	DB   'BE'                              ;;
	DW   120         ;;AN000; ID entry
	DW   OFFSET BE_LOGIC,0     ;; pointer to LANG kb table
	DB   1         ;;AN000; number of ids
	DB   2         ;;AN000; number of code pages
	DW   850         ;; code page ;; default to 850 - same as country.sys
	DW   OFFSET BE_850_XLAT,0    ;; table pointer
	DW   437         ;; code page
	DW   OFFSET BE_437_XLAT,0    ;; table pointer
					 ;;
;*****************************************************************************
;*****************************************************************************
		 EXTRN NL_LOGIC:NEAR     ;;
		 EXTRN NL_437_XLAT:NEAR    ;;
		 EXTRN NL_850_XLAT:NEAR    ;;
					 ;;
	NL_LANG_ENT:         ;; language entry for NETHERLANDS
		DB   'NL'                            ;;
		DW   143         ;;AN000; ID entry
		DW   OFFSET NL_LOGIC,0     ;; pointer to LANG kb table
		DB   1         ;;AN000; number of ids
		DB   2         ;;AN000; number of code pages
		DW   43        ;; code page
		DW   OFFSET NL_437_XLAT,0    ;; table pointer
		DW   850         ;; code page
		DW   OFFSET NL_850_XLAT,0    ;; table pointer
					 ;;
;*****************************************************************************
;*****************************************************************************
		 EXTRN NO_LOGIC:NEAR     ;;
		 EXTRN NO_850_XLAT:NEAR    ;;AC000;
		 EXTRN NO_865_XLAT:NEAR    ;;AC000;
					 ;;
	NO_LANG_ENT:         ;; language entry for NORWAY
		DB   'NO'                            ;;
		DW   155         ;;AN000; ID entry
		DW   OFFSET NO_LOGIC,0     ;; pointer to LANG kb table
		DB   1         ;;AN000; number of ids
		DB   2         ;;AN000; number of code pages
		DW   850         ;;AC000; code page
		DW   OFFSET NO_850_XLAT,0    ;;AC000; table pointer
		DW   865         ;;AC000; code page
		DW   OFFSET NO_865_XLAT,0    ;;AC000; table pointer
					 ;;
;*****************************************************************************
;*****************************************************************************
		 EXTRN SV_LOGIC:NEAR     ;;
		 EXTRN SV_437_XLAT:NEAR    ;;
		 EXTRN SV_850_XLAT:NEAR    ;;
					 ;;
	SV_LANG_ENT:         ;; language entry for SWEDEN
		DB   'SV'                            ;;
		DW   153         ;;AN000; ID entry
		DW   OFFSET SV_LOGIC,0     ;; pointer to LANG kb table
		DB   1         ;;AN000; number of ids
		DB   2         ;;AN000; number of code pages
		DW   437         ;; code page
		DW   OFFSET SV_437_XLAT,0    ;; table pointer
		DW   850         ;; code page
		DW   OFFSET SV_850_XLAT,0    ;; table pointer
					 ;;
;*****************************************************************************
;*****************************************************************************
;;  Already declared external above
;;  EXTRN Sv_LOGIC:NEAR     ;; Finland & Sweden have same layout,
;;  EXTRN Sv_437_XLAT:NEAR    ;; but different code page defaults,
;;  EXTRN Sv_850_XLAT:NEAR    ;; use Sweden data for Finland
					;;
 SU_LANG_ENT:       ;; language entry for FINLAND
	 DB 'SU'                            ;;
	 DW 153       ;; ID entry
	 DW OFFSET Sv_LOGIC,0   ;; pointer to LANG kb table
	 DB 1       ;; number of ids
	 DB 2       ;; number of code pages
	 DW 850       ;; code page  ;;;dcl 850 now default, March 8, 1988
	 DW OFFSET Sv_850_XLAT,0    ;; table pointer
	 DW 437       ;; code page
	 DW OFFSET Sv_437_XLAT,0    ;; table pointer
					;;
;*****************************************************************************
;*****************************************************************************
		 EXTRN CF_LOGIC:NEAR     ;;
		 EXTRN CF_863_XLAT:NEAR    ;;
		 EXTRN CF_850_XLAT:NEAR    ;;
					 ;;
	CF_LANG_ENT:         ;; language entry for Canadian-French
		DB   'CF'                            ;;
		DW   058         ;; ID entry
		DW   OFFSET CF_LOGIC,0     ;; pointer to LANG kb table
		DB   1         ;; number of ids
		DB   2         ;; number of code pages
		DW   863         ;; code page
		DW   OFFSET CF_863_XLAT,0    ;; table pointer
		DW   850         ;; code page
		DW   OFFSET CF_850_XLAT,0    ;; table pointer
					 ;;
;*****************************************************************************
		 EXTRN LA_LOGIC:NEAR     ;;
		 EXTRN LA_850_XLAT:NEAR    ;;
		 EXTRN LA_437_XLAT:NEAR    ;;
					 ;;
	LA_LANG_ENT:         ;; language entry for LATIN AMERICAN
		DB   'LA'                            ;;
		DW   171         ;;AN000; ID entry
		DW   OFFSET LA_LOGIC,0     ;; pointer to LANG kb table
		DB   1         ;;AN000; number of ids
		DB   2         ;;AN000; number of code pages
		DW   850         ;; code page
		DW   OFFSET LA_850_XLAT,0    ;; table pointer
		DW   437         ;; code page  ; default to 437 -same as country.sys
		DW   OFFSET LA_437_XLAT,0    ;; table pointer
					 ;;
;*****************************************************************************
;*****************************************************************************
		 EXTRN IC_LOGIC:NEAR     ;;
		 EXTRN IC_861_XLAT:NEAR    ;;
		 EXTRN IC_850_XLAT:NEAR    ;;
					 ;;
	IC_LANG_ENT:         ;; language entry for �sland
		DB   'IS'                            ;;
		DW   161         ;; ID entry
		DW   OFFSET IC_LOGIC,0     ;; pointer to LANG kb table
		DB   1         ;; number of ids
		DB   2         ;; number of code pages
		DW   861         ;; code page
		DW   OFFSET IC_861_XLAT,0    ;; table pointer
		DW   850         ;; code page
		DW   OFFSET IC_850_XLAT,0    ;; table pointer

;*****************************************************************************
; [Mihindu 10/30/90 : added Brazil]

	 EXTRN BR_LOGIC:NEAR       ;;AC000;
	 EXTRN BR_850_XLAT:NEAR    ;;AC000;
	 EXTRN BR_437_XLAT:NEAR    ;;AC000;
					 ;;
BR_LANG_ENT:         ;; language entry for BRAZIL
	DB   'BR'        ;;
	DW   274         ;;AN000; ID entry
	DW   OFFSET BR_LOGIC,0     ;; pointer to LANG kb table
	DB   2         ;;AC000; number of ids
	DB   2         ;;AC000; number of code pages
	DW   850         ;;AC000; code page
	DW   OFFSET BR_850_XLAT,0    ;;AC000; table pointer
	DW   437         ;;AC000; code page
	DW   OFFSET BR_437_XLAT,0    ;;AC000; table pointer
					 ;;
																				 ;;
;*****************************************************************************
;       [Verav : added Brazil IBM layout - Feb 92]

	 EXTRN BR2_LOGIC:NEAR                   ;;AC000;
	 EXTRN BR2_850_XLAT:NEAR                ;;AC000;
	 EXTRN BR2_437_XLAT:NEAR                ;;AC000;
					 ;;
BR2_LANG_ENT:                             ;; language entry for BRAZIL
	DB   'BR'                              ;;
	DW   275                               ;;AN000; ID entry
	DW   OFFSET BR2_LOGIC,0                 ;; pointer to LANG kb table
	DB   1                                 ;;AC000; number of ids
	DB   2                                 ;;AC000; number of code pages
	DW   850                               ;;AC000; code page
	DW   OFFSET BR2_850_XLAT,0              ;;AC000; table pointer
	DW   437                               ;;AC000; code page
	DW   OFFSET BR2_437_XLAT,0              ;;AC000; table pointer
					 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 EXTRN PL_LOGIC:NEAR           ;;
	 EXTRN PL_850_XLAT:NEAR        ;;
	 EXTRN PL_852_XLAT:NEAR        ;;
							 ;;
PL_LANG_ENT:             ;; language entry for POLAND
	DB   'PL'                            ;;
	DW   214             ;; Keyboard ID entry |
	DW   OFFSET PL_LOGIC,0         ;; pointer to LANG kb table
	DB   1             ;; number of ids        |
	DB   2             ;; number of code pages
	DW   850             ;; code page
	DW   OFFSET PL_850_XLAT,0        ;; table pointer
	DW   852             ;; code page
	DW   OFFSET PL_852_XLAT,0        ;; table pointer
							 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 EXTRN YU_LOGIC:NEAR           ;;
	 EXTRN YU_850_XLAT:NEAR        ;;
	 EXTRN YU_852_XLAT:NEAR        ;;
							 ;;
YU_LANG_ENT:             ;; language entry for YUGOSLAVIA
	DB   'YU'                            ;;
	DW   234             ;; Keyboard ID entry |
	DW   OFFSET YU_LOGIC,0         ;; pointer to LANG kb table
	DB   1             ;; number of ids        |
	DB   2             ;; number of code pages |
	DW   850             ;; code page
	DW   OFFSET YU_850_XLAT,0        ;; table pointer
	DW   852             ;; code page
	DW   OFFSET YU_852_XLAT,0        ;; table pointer
							 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 EXTRN HU_LOGIC:NEAR           ;;
	 EXTRN HU_850_XLAT:NEAR        ;;
	 EXTRN HU_852_XLAT:NEAR        ;;
							 ;;
HU_LANG_ENT:             ;; language entry for HUNGARY
	DB   'HU'                            ;;
	DW   208             ;; Keyboard ID entry |
	DW   OFFSET HU_LOGIC,0         ;; pointer to LANG kb table
	DB   1             ;; number of ids        |
	DB   2             ;; number of code pages
	DW   850             ;; code page
	DW   OFFSET HU_850_XLAT,0        ;; table pointer
	DW   852             ;; code page
	DW   OFFSET HU_852_XLAT,0        ;; table pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 EXTRN RO_LOGIC:NEAR                 ;;
	 EXTRN RO_850_XLAT:NEAR              ;;
	 EXTRN RO_852_XLAT:NEAR              ;;
																			 ;;
RO_LANG_ENT:                           ;; language entry for ROMANIA
	DB   'RO'                            ;;
	DW   333                             ;; Keyboard ID entry |
	DW   OFFSET RO_LOGIC,0               ;; pointer to LANG kb table
	DB   1                               ;; number of ids        |
	DB   2                               ;; number of code pages
	DW   850                             ;; code page
	DW   OFFSET RO_850_XLAT,0            ;; table pointer
	DW   852                             ;; code page
	DW   OFFSET RO_852_XLAT,0            ;; table pointer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
							 ;;
; M000 --- ADDDED SECTION for JAPANESE support

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				EXTRN   JP_LOGIC:NEAR            ;;                             ;JP9002
				EXTRN   JP_932_XLAT:NEAR         ;;                             ;JP9002
				EXTRN   JP_437_XLAT:NEAR         ;;                             ;JP9002
JP_LANG_ENT:                             ;;                             ;JP9002
		DB   'JP'                            ;;                             ;JP9002
		DW   194                             ;; keyboard ID for Japan       ;JP9009
		DW   OFFSET JP_LOGIC, 0              ;; pointer to LANG kb table    ;JP9002
		DB   1                               ;; number of ids               ;JP9002
		DB   2                               ;; number of code pages        ;JP9002
		DW   932                             ;; code page                   ;JP9002
		DW   OFFSET JP_932_XLAT, 0           ;; table pointer               ;JP9002
		DW   437                             ;; code page                   ;JP9002
		DW   OFFSET JP_437_XLAT, 0           ;; table pointer               ;JP9002
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; M000 -- end added section


DUMMY_ENT:             ;; language entry
	DB   'XX'                            ;;
	DW   103             ;;AC000; ID entry
	DW   OFFSET DUMMY_LOGIC,0        ;; pointer to LANG kb table
	DB   1             ;;AC000; number of ids
	DB   6             ;;AC000; number of code pages
	DW   437             ;; code page
	DW   OFFSET DUMMY_XLAT_437,0         ;; table pointer
	DW   850             ;; code page
	DW   OFFSET DUMMY_XLAT_850,0         ;; table pointer
	DW   852             ;; code page [Mihindu 11/30/90]
	DW   OFFSET DUMMY_XLAT_852,0         ;; table pointer
	DW   860             ;; code page
	DW   OFFSET DUMMY_XLAT_860,0         ;; table pointer
	DW   863             ;; code page
	DW   OFFSET DUMMY_XLAT_863,0         ;; table pointer
	DW   865             ;; code page
	DW   OFFSET DUMMY_XLAT_865,0         ;; table pointer
	DW   861             ;; code page   �sland !
	DW   OFFSET DUMMY_XLAT_861,0         ;; table pointer
								 ;;
DUMMY_LOGIC:             ;;
	 DW  LOGIC_END-$           ;; length
	 DW  0             ;; special features
	 DB  92H,0,0             ;; EXIT_STATE_LOGIC_COMMAND
LOGIC_END:             ;;
							 ;;
DUMMY_XLAT_437:            ;;
	 DW   6            ;; length of section
	 DW   437            ;; code page
	 DW   0            ;; LAST STATE
							 ;;
DUMMY_XLAT_850:            ;;
	 DW   6            ;; length of section
	 DW   850            ;; code page
	 DW   0            ;; LAST STATE
							 ;;
DUMMY_XLAT_852:            ;; [Mihindu 11/30/90]
	 DW   6            ;; length of section
	 DW   852            ;; code page
	 DW   0            ;; LAST STATE
							 ;;
DUMMY_XLAT_860:            ;;
	 DW   6            ;; length of section
	 DW   860            ;; code page
	 DW   0            ;; LAST STATE
							 ;;
DUMMY_XLAT_865:            ;;
	 DW   6            ;; length of section
	 DW   865            ;; code page
	 DW   0            ;; LAST STATE
							 ;;
DUMMY_XLAT_863:            ;;
	 DW   6            ;; length of section
	 DW   863            ;; code page
	 DW   0            ;; LAST STATE
							 ;;
DUMMY_XLAT_861:            ;;
	 DW     6            ;; length of section
	 DW     861            ;; code page
	 DW     0            ;; LAST STATE      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;*****************************************************************************
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
							 ;;
CODE   ENDS            ;;
	 END             ;;
