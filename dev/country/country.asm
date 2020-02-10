	page	84,132
; July 8, 1986 - updated Finland for 437 dcl.
; July 8, 1986 - updated Netherland for 437 dcl.
; July 8, 1986 - updated Italy for 437 dcl.
; July 8, 1986 - updated Canada for 863 dcl.
; July 8, 1986 - updated Belgium for 437 dcl. (except Collate)
; July 8, 1986 - updated AreaSouth for 437 dcl.
; July 8, 1986 - updated Switzerland for 437 dcl.
; July 8, 1986 - updated Norway for 865 dcl.
; July 8, 1986 - updated Denmark for 865 dcl.
; July 14, 1986 - updated Sweden for 437 dcl.
; July 8, 1986 - updated AFE for 850 dcl.
; July 8, 1986 - updated Finland for 850 dcl.
; July 8, 1986 - updated Netherland for 850 dcl.
; July 8, 1986 - updated Canada for 850 dcl.
; July 8, 1986 - updated Italy for 850 dcl.
; July 8, 1986 - updated UK for 850 dcl.
; July 8, 1986 - updated Norway for 850 dcl.
; July 8, 1986 - updated Denmark for 850 dcl.
; July 8, 1986 - updated US for 850 dcl.
; July 8, 1986 - updated Switzerland for 850 dcl.
; July 14, 1986 - updated Sweden for 850 dcl.
; July 14, 1986 - updated Belgium for 850 dcl. (except Collate)
; August 26,1986 - updated Belgium for 437 collate EMG
; August 27,1986 - updated Belgium for 850 collate EMG
; August 27,1986 - updated Area South for 437  EMG
; August 27,1986 - updated Area South for 864  EMG
; August 27,1986 - make all collates 256 bytes	EMG
; August 27,1986 - updated Germany for 850  EMG
; August 27,1986 - updated Spain for 850  EMG
; August 28,1986 - updated Latin America for 850  EMG
; September 12, 1986 - updated SP,IS,CF,AS,PO,LA  EMG
; October 2, 1986 - updated AS and IS  EMG
; July 23,1987	  - DCR to alter Germany collate table to match US 850 DCR037
;		    CNS
; July 23,1987	  - DCR to alter SWISS collate info and ucase table 850 & 437
;		    DCR059--- CNS ***
; July 23,1987	  - DCR to alter Denmark's primary codepage to 850 instead of
;		    865 --- CNS ***
; NOvember 12,1987 - PTM2390 DBCS length of terminating value included for
;		     for DBCS countries
;		   - FILEUPCASE & UCASE swapped for 437 & 850 mapping incorrect
;		   - -ly for DOS 3.3
;		   - Make the FUCASE & UCASE tables equivalent for 850 & 437
; March 9, p3811, Finland to default to 850 now, w/437 as alternate code page
;
; March 30, p4072, LA wants to be same as Spain
; ***CNS
; April 14,1988    Re-adjust LA to match SPAIN which should have 850 as a
;PTM 4389	   default instead of 437 for both countries
;
;	create country.sys file
;
;
	include mkcntry.inc
;
; -----------------------------------------------------------
;
;	Data for COUNTRY.SYS file
;
; -----------------------------------------------------------
dseg	segment para
cdinfo	label	word
	db	0ffh,'COUNTRY'          ; signature
	db	8 dup (0)		; reserved
	dw	PTRCNT			; number of pointers in header
	db	CIPTYPE 		; type = country info pointer
	dd	offset cntryinfo	; pointer to country information
;
cntryinfo label word
cntrycnt=0
	dw	finalCNT		; number of countries
	ctryent <CENTRYSIZE,CID_US,437,,,us437_data>	    ; United States
	ctryent <CENTRYSIZE,CID_US,850,,,us850_data>	    ;
	ctryent <CENTRYSIZE,CID_UK,437,,,uk437_data>	    ; United Kingdom
	ctryent <CENTRYSIZE,CID_UK,850,,,uk850_data>	    ;
	ctryent <CENTRYSIZE,CID_FR,437,,,fr437_data>	    ; France
	ctryent <CENTRYSIZE,CID_FR,850,,,fr850_data>	    ;
	ctryent <CENTRYSIZE,CID_GR,437,,,gr437_data>	    ; Germany
	ctryent <CENTRYSIZE,CID_GR,850,,,gr850_data>	    ;
	ctryent <CENTRYSIZE,CID_SP,850,,,sp850_data>	    ;
	ctryent <CENTRYSIZE,CID_SP,437,,,sp437_data>	    ; Spain
	ctryent <CENTRYSIZE,CID_IT,437,,,it437_data>	    ; Italy
	ctryent <CENTRYSIZE,CID_IT,850,,,it850_data>	    ;
	ctryent <CENTRYSIZE,CID_SV,437,,,sv437_data>	    ; Sweden
	ctryent <CENTRYSIZE,CID_SV,850,,,sv850_data>	    ;
	ctryent <CENTRYSIZE,CID_DK,850,,,dk850_data>	    ; Denmark
	ctryent <CENTRYSIZE,CID_DK,865,,,dk865_data>	    ; DCR060 CNS ***** ;AN000;
	ctryent <CENTRYSIZE,CID_SW,850,,,sw850_data>	    ; Switzerland
	ctryent <CENTRYSIZE,CID_SW,437,,,sw437_data>	    ;
	ctryent <CENTRYSIZE,CID_NO,850,,,no850_data>	    ; Norway
	ctryent <CENTRYSIZE,CID_NO,865,,,no865_data>	    ;
	ctryent <CENTRYSIZE,CID_NL,437,,,nl437_data>	    ; Netherlands
	ctryent <CENTRYSIZE,CID_NL,850,,,nl850_data>	    ;
	ctryent <CENTRYSIZE,CID_BE,850,,,be850_data>	    ; Belgium
	ctryent <CENTRYSIZE,CID_BE,437,,,be437_data>	    ;
	ctryent <CENTRYSIZE,CID_FI,850,,,fi850_data>	    ; Finland p3811, requested 850 default
	ctryent <CENTRYSIZE,CID_FI,437,,,fi437_data>	    ; Finland
	ctryent <CENTRYSIZE,CID_IS,862,,,is862_data>	    ; Israel
	ctryent <CENTRYSIZE,CID_IS,850,,,is850_data>	    ;
	ctryent <CENTRYSIZE,CID_CA,863,,,ca863_data>	    ; Canada French
	ctryent <CENTRYSIZE,CID_CA,850,,,ca850_data>	    ;
	ctryent <CENTRYSIZE,CID_AS,864,,,as864_data>	    ; Area South (Arab States)
	ctryent <CENTRYSIZE,CID_AS,850,,,as850_data>	    ;
	ctryent <CENTRYSIZE,CID_PO,850,,,po850_data>	    ; Portugal
	ctryent <CENTRYSIZE,CID_PO,860,,,po860_data>	    ;
	ctryent <CENTRYSIZE,CID_LA,850,,,la850_data>	    ;
	ctryent <CENTRYSIZE,CID_LA,437,,,la437_data>	    ; Latin America
	ctryent <CENTRYSIZE,CID_AFE,437,,,afe437_data>	    ; International English
	ctryent <CENTRYSIZE,CID_AFE,850,,,afe850_data>	    ;
	ctryent <CENTRYSIZE,CID_JP,932,,,jp932_data>	    ; Japan
	ctryent <CENTRYSIZE,CID_JP,437,,,jp437_data>	    ;
	ctryent <CENTRYSIZE,CID_KO,934,,,ko934_data>	    ; Korea
	ctryent <CENTRYSIZE,CID_KO,437,,,ko437_data>	    ;
	ctryent <CENTRYSIZE,CID_PR,936,,,pr936_data>	    ; PRC
	ctryent <CENTRYSIZE,CID_PR,437,,,pr437_data>	    ;
	ctryent <CENTRYSIZE,CID_TA,938,,,ta938_data>	    ; Taiwan
	ctryent <CENTRYSIZE,CID_TA,437,,,ta437_data>	    ;

;	cs3 -- begin additions of new country data

	 ctryent <CENTRYSIZE,CID_BR,850,,,br850_data>        ; Brazil
	 ctryent <CENTRYSIZE,CID_BR,437,,,br437_data>        ;
	 ctryent <CENTRYSIZE,CID_IC,850,,,ic850_data>       ; Iceland
	 ctryent <CENTRYSIZE,CID_IC,861,,,ic861_data>       ;
	 ctryent <CENTRYSIZE,CID_TR,850,,,tr850_data>       ; Turkey
	 ctryent <CENTRYSIZE,CID_TR,857,,,tr857_data>       ;
	 ctryent <CENTRYSIZE,CID_YU,852,,,yu852_data>       ; yugoslavia
	 ctryent <CENTRYSIZE,CID_YU,850,,,yu850_data>       ;
	 ctryent <CENTRYSIZE,CID_CS,852,,,cs852_data>       ; czech
	 ctryent <CENTRYSIZE,CID_CS,850,,,cs850_data>       ;
	 ctryent <CENTRYSIZE,CID_PL,852,,,pl852_data>       ; poland
	 ctryent <CENTRYSIZE,CID_PL,850,,,pl850_data>       ;
	 ctryent <CENTRYSIZE,CID_HU,852,,,hu852_data>       ; hungary
	 ctryent <CENTRYSIZE,CID_HU,850,,,hu850_data>       ;

;	cs3 -- end additions


dummy	macro	p
finalCNT	equ	p
	endm
	dummy	%cntrycnt
;
uk437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,uk437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,uk_collate>
	ctrydat <CDATASIZE,SETUCASE,,uk_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,uk_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,uk_flist>
	ctrydat <CDATASIZE,SETDBCS,,uk_dbcs>
;
uk850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,uk437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,uk850_collate>
	ctrydat <CDATASIZE,SETUCASE,,uk850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,uk850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,uk_flist>
	ctrydat <CDATASIZE,SETDBCS,,uk_dbcs>
;
fr437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,fr437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,fr_collate>
	ctrydat <CDATASIZE,SETUCASE,,fr_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,fr_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,fr_flist>
	ctrydat <CDATASIZE,SETDBCS,,fr_dbcs>
;
fr850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,fr437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,fr850_collate>
	ctrydat <CDATASIZE,SETUCASE,,fr850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,fr850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,fr_flist>
	ctrydat <CDATASIZE,SETDBCS,,fr_dbcs>
;
gr437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,gr437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,gr_collate>
	ctrydat <CDATASIZE,SETUCASE,,gr_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,gr_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,gr_flist>
	ctrydat <CDATASIZE,SETDBCS,,gr_dbcs>
;
gr850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,gr437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,gr850_collate>
	ctrydat <CDATASIZE,SETUCASE,,gr850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,gr850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,gr_flist>
	ctrydat <CDATASIZE,SETDBCS,,gr_dbcs>
;
sp437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,sp437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,sp_collate>
	ctrydat <CDATASIZE,SETUCASE,,sp_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,sp_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,sp_flist>
	ctrydat <CDATASIZE,SETDBCS,,sp_dbcs>
;
sp850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,sp850_info>
	ctrydat <CDATASIZE,SETCOLLATE,,sp850_collate>
	ctrydat <CDATASIZE,SETUCASE,,sp850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,sp850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,sp_flist>
	ctrydat <CDATASIZE,SETDBCS,,sp_dbcs>
;
it437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,it437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,it_collate>
	ctrydat <CDATASIZE,SETUCASE,,it_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,it_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,it_flist>
	ctrydat <CDATASIZE,SETDBCS,,it_dbcs>
;
it850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,it437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,it850_collate>
	ctrydat <CDATASIZE,SETUCASE,,it850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,it850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,it_flist>
	ctrydat <CDATASIZE,SETDBCS,,it_dbcs>
;
sv437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,sv437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,sv_collate>
	ctrydat <CDATASIZE,SETUCASE,,sv_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,sv_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,sv_flist>
	ctrydat <CDATASIZE,SETDBCS,,sv_dbcs>
;
sv850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,sv437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,sv850_collate>
	ctrydat <CDATASIZE,SETUCASE,,sv850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,sv850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,sv_flist>
	ctrydat <CDATASIZE,SETDBCS,,sv_dbcs>
;
dk850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,dk865_info>
	ctrydat <CDATASIZE,SETCOLLATE,,dk850_collate>
	ctrydat <CDATASIZE,SETUCASE,,dk850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,dk850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,dk_flist>
	ctrydat <CDATASIZE,SETDBCS,,dk_dbcs>
;
dk865_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,dk865_info>
	ctrydat <CDATASIZE,SETCOLLATE,,dk_collate>
	ctrydat <CDATASIZE,SETUCASE,,dk_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,dk_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,dk_flist>
	ctrydat <CDATASIZE,SETDBCS,,dk_dbcs>
;
sw437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,sw437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,sw_collate>
	ctrydat <CDATASIZE,SETUCASE,,sw_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,sw_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,sw_flist>
	ctrydat <CDATASIZE,SETDBCS,,sw_dbcs>
;
sw850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,sw437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,sw850_collate>
	ctrydat <CDATASIZE,SETUCASE,,sw850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,sw850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,sw_flist>
	ctrydat <CDATASIZE,SETDBCS,,sw_dbcs>
;
no865_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,no865_info>
	ctrydat <CDATASIZE,SETCOLLATE,,no_collate>
	ctrydat <CDATASIZE,SETUCASE,,no_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,no_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,no_flist>
	ctrydat <CDATASIZE,SETDBCS,,no_dbcs>
;
no850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,no865_info>
	ctrydat <CDATASIZE,SETCOLLATE,,no850_collate>
	ctrydat <CDATASIZE,SETUCASE,,no850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,no850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,no_flist>
	ctrydat <CDATASIZE,SETDBCS,,no_dbcs>
;
nl437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,nl437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,nl_collate>
	ctrydat <CDATASIZE,SETUCASE,,nl_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,nl_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,nl_flist>
	ctrydat <CDATASIZE,SETDBCS,,nl_dbcs>
;
nl850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,nl437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,nl850_collate>
	ctrydat <CDATASIZE,SETUCASE,,nl850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,nl850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,nl_flist>
	ctrydat <CDATASIZE,SETDBCS,,nl_dbcs>
;
be437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,be437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,be_collate>
	ctrydat <CDATASIZE,SETUCASE,,be_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,be_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,be_flist>
	ctrydat <CDATASIZE,SETDBCS,,be_dbcs>
;
be850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,be437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,be850_collate>
	ctrydat <CDATASIZE,SETUCASE,,be850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,be850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,be_flist>
	ctrydat <CDATASIZE,SETDBCS,,be_dbcs>
;
fi437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,fi437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,fi_collate>
	ctrydat <CDATASIZE,SETUCASE,,fi_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,fi_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,fi_flist>
	ctrydat <CDATASIZE,SETDBCS,,fi_dbcs>
;
fi850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,fi437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,fi850_collate>
	ctrydat <CDATASIZE,SETUCASE,,fi850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,fi850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,fi_flist>
	ctrydat <CDATASIZE,SETDBCS,,fi_dbcs>
;
us437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,us437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,us_collate>
	ctrydat <CDATASIZE,SETUCASE,,us_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,us_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,us_flist>
	ctrydat <CDATASIZE,SETDBCS,,us_dbcs>
;
us850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,us437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,us850_collate>
	ctrydat <CDATASIZE,SETUCASE,,us850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,us850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,us_flist>
	ctrydat <CDATASIZE,SETDBCS,,us_dbcs>
;
is862_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,is862_info>
	ctrydat <CDATASIZE,SETCOLLATE,,is_collate>
	ctrydat <CDATASIZE,SETUCASE,,is_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,is_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,is_flist>
	ctrydat <CDATASIZE,SETDBCS,,is_dbcs>
;
is850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,is862_info>
	ctrydat <CDATASIZE,SETCOLLATE,,is850_collate>
	ctrydat <CDATASIZE,SETUCASE,,is850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,is850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,is_flist>
	ctrydat <CDATASIZE,SETDBCS,,is_dbcs>
;
ca863_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,ca863_info>
	ctrydat <CDATASIZE,SETCOLLATE,,ca_collate>
	ctrydat <CDATASIZE,SETUCASE,,ca_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,ca_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,ca_flist>
	ctrydat <CDATASIZE,SETDBCS,,ca_dbcs>
;
ca850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,ca863_info>
	ctrydat <CDATASIZE,SETCOLLATE,,ca850_collate>
	ctrydat <CDATASIZE,SETUCASE,,ca850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,ca850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,ca_flist>
	ctrydat <CDATASIZE,SETDBCS,,ca_dbcs>
;
as864_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,as864_info>
	ctrydat <CDATASIZE,SETCOLLATE,,as_collate>
	ctrydat <CDATASIZE,SETUCASE,,as_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,as_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,as_flist>
	ctrydat <CDATASIZE,SETDBCS,,as_dbcs>
;
as850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,as850_info>
	ctrydat <CDATASIZE,SETCOLLATE,,as850_collate>
	ctrydat <CDATASIZE,SETUCASE,,as850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,as850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,as_flist>
	ctrydat <CDATASIZE,SETDBCS,,as_dbcs>
;
po860_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,po860_info>
	ctrydat <CDATASIZE,SETCOLLATE,,po_collate>
	ctrydat <CDATASIZE,SETUCASE,,po_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,po_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,po_flist>
	ctrydat <CDATASIZE,SETDBCS,,po_dbcs>
;
po850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,po860_info>
	ctrydat <CDATASIZE,SETCOLLATE,,po850_collate>
	ctrydat <CDATASIZE,SETUCASE,,po850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,po850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,po_flist>
	ctrydat <CDATASIZE,SETDBCS,,po_dbcs>
;
la437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,la437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,la_collate>
	ctrydat <CDATASIZE,SETUCASE,,la_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,la_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,la_flist>
	ctrydat <CDATASIZE,SETDBCS,,la_dbcs>
;
la850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,la437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,la850_collate>
	ctrydat <CDATASIZE,SETUCASE,,la850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,la850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,la_flist>
	ctrydat <CDATASIZE,SETDBCS,,la_dbcs>
;
jp932_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,jp932_info>
	ctrydat <CDATASIZE,SETCOLLATE,,jp932_collate>
	ctrydat <CDATASIZE,SETUCASE,,jp932_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,jp932_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,jp932_flist>
	ctrydat <CDATASIZE,SETDBCS,,jp932_dbcs>
;
jp437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,jp437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,jp437_collate>
	ctrydat <CDATASIZE,SETUCASE,,jp437_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,jp437_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,jp437_flist>
	ctrydat <CDATASIZE,SETDBCS,,jp437_dbcs>
;
ko934_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,ko934_info>
	ctrydat <CDATASIZE,SETCOLLATE,,ko934_collate>
	ctrydat <CDATASIZE,SETUCASE,,ko934_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,ko934_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,ko934_flist>
	ctrydat <CDATASIZE,SETDBCS,,ko934_dbcs>
;
ko437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,ko437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,ko437_collate>
	ctrydat <CDATASIZE,SETUCASE,,ko437_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,ko437_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,ko437_flist>
	ctrydat <CDATASIZE,SETDBCS,,ko437_dbcs>
;
pr936_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,pr936_info>
	ctrydat <CDATASIZE,SETCOLLATE,,pr936_collate>
	ctrydat <CDATASIZE,SETUCASE,,pr936_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,pr936_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,pr936_flist>
	ctrydat <CDATASIZE,SETDBCS,,pr936_dbcs>
;
pr437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,pr437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,pr437_collate>
	ctrydat <CDATASIZE,SETUCASE,,pr437_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,pr437_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,pr437_flist>
	ctrydat <CDATASIZE,SETDBCS,,pr437_dbcs>
;
ta938_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,ta938_info>
	ctrydat <CDATASIZE,SETCOLLATE,,ta938_collate>
	ctrydat <CDATASIZE,SETUCASE,,ta938_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,ta938_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,ta938_flist>
	ctrydat <CDATASIZE,SETDBCS,,ta938_dbcs>
;
ta437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,ta437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,ta437_collate>
	ctrydat <CDATASIZE,SETUCASE,,ta437_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,ta437_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,ta437_flist>
	ctrydat <CDATASIZE,SETDBCS,,ta437_dbcs>
;
afe437_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,afe437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,afe_collate>
	ctrydat <CDATASIZE,SETUCASE,,afe_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,afe_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,afe_flist>
	ctrydat <CDATASIZE,SETDBCS,,afe_dbcs>
;
afe850_data label   word
	dw	CDATAITEMS				; number of entries
	ctrydat <CDATASIZE,SETCOUNTRYINFO,,afe437_info>
	ctrydat <CDATASIZE,SETCOLLATE,,afe850_collate>
	ctrydat <CDATASIZE,SETUCASE,,afe850_ucase>
	ctrydat <CDATASIZE,SETUCASEFILE,,afe850_ucase>
	ctrydat <CDATASIZE,SETFILELIST,,afe_flist>
	ctrydat <CDATASIZE,SETDBCS,,afe_dbcs>
	page

;	cs3 -- begin additions for new countries

br850_data label   word
	 dw    CDATAITEMS                    ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,br850_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,br850_collate>
	 ctrydat <CDATASIZE,SETUCASE,,br850_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,br850_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,br_flist>
	 ctrydat <CDATASIZE,SETDBCS,,br_dbcs>
;
br437_data label   word
	 dw    CDATAITEMS                    ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,br850_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,br437_collate>
	 ctrydat <CDATASIZE,SETUCASE,,br_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,br_ucase>
	 ctrydat <CDATASIZE,SETFILELIST,,br_flist>
	 ctrydat <CDATASIZE,SETDBCS,,br_dbcs>
;
ic861_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,ic861_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,ic_collate>
	 ctrydat <CDATASIZE,SETUCASE,,ic_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,ic_ucase>
	 ctrydat <CDATASIZE,SETFILELIST,,ic_flist>
	 ctrydat <CDATASIZE,SETDBCS,,ic_dbcs>
;
ic850_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,ic850_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,ic850_collate>
	 ctrydat <CDATASIZE,SETUCASE,,ic850_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,ic850_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,ic_flist>
	 ctrydat <CDATASIZE,SETDBCS,,ic_dbcs>
;
tr850_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,tr850_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,tr850_collate>
	 ctrydat <CDATASIZE,SETUCASE,,tr850_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,tr850_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,tr_flist>
	 ctrydat <CDATASIZE,SETDBCS,,tr_dbcs>
;
tr857_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,tr857_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,tr857_collate>
	 ctrydat <CDATASIZE,SETUCASE,,tr857_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,tr857_ucase>
	 ctrydat <CDATASIZE,SETFILELIST,,tr_flist>
	 ctrydat <CDATASIZE,SETDBCS,,tr_dbcs>
;
yu850_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,yu852_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,yu850_collate>
	 ctrydat <CDATASIZE,SETUCASE,,yu850_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,yu850_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,yu_flist>
	 ctrydat <CDATASIZE,SETDBCS,,yu_dbcs>   ;  This is DOUBLE BYTE CHAR SET
;                                               ;  char set (ahead of its time)
;                                               ; and doesn't need to be
;
yu852_data label   word
	 dw     CDATAITEMS                      ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,yu852_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,yu852_collate>
	 ctrydat <CDATASIZE,SETUCASE,,yu852_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,yu_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,yu_flist>
	 ctrydat <CDATASIZE,SETDBCS,,yu_dbcs>
;
cs850_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,cs852_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,cs850_collate>
	 ctrydat <CDATASIZE,SETUCASE,,cs850_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,cs850_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,cs_flist>
	 ctrydat <CDATASIZE,SETDBCS,,cs_dbcs>
;
cs852_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,cs852_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,cs852_collate>
	 ctrydat <CDATASIZE,SETUCASE,,cs852_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,cs_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,cs_flist>
	 ctrydat <CDATASIZE,SETDBCS,,cs_dbcs>
;
sl850_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,cs852_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,sl850_collate>
	 ctrydat <CDATASIZE,SETUCASE,,sl850_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,sl850_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,sl_flist>
	 ctrydat <CDATASIZE,SETDBCS,,sl_dbcs>
;
sl852_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,cs852_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,sl852_collate>
	 ctrydat <CDATASIZE,SETUCASE,,sl852_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,sl_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,sl_flist>
	 ctrydat <CDATASIZE,SETDBCS,,sl_dbcs>
;
pl850_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,pl852_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,pl850_collate>
	 ctrydat <CDATASIZE,SETUCASE,,pl850_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,pl850_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,pl_flist>
	 ctrydat <CDATASIZE,SETDBCS,,pl_dbcs>
;
pl852_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,pl852_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,pl852_collate>
	 ctrydat <CDATASIZE,SETUCASE,,pl852_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,pl_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,pl_flist>
	 ctrydat <CDATASIZE,SETDBCS,,pl_dbcs>
;
hu850_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,hu852_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,hu850_collate>
	 ctrydat <CDATASIZE,SETUCASE,,hu850_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,hu850_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,hu_flist>
	 ctrydat <CDATASIZE,SETDBCS,,hu_dbcs>
;
hu852_data label   word
	 dw     CDATAITEMS                              ; number of entries
	 ctrydat <CDATASIZE,SETCOUNTRYINFO,,hu852_info>
	 ctrydat <CDATASIZE,SETCOLLATE,,hu852_collate>
	 ctrydat <CDATASIZE,SETUCASE,,hu852_ucase>
	 ctrydat <CDATASIZE,SETUCASEFILE,,hu_ucfile>
	 ctrydat <CDATASIZE,SETFILELIST,,hu_flist>
	 ctrydat <CDATASIZE,SETDBCS,,hu_dbcs>
;
;	cs3 -- end additions for new countries
	page
; ----------------------------------------
;
;	World Trade Country Info Tables
;
; ----------------------------------------
uk437_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_UK,437,DATE_DMY,156,,,,,',',,'.',,'-',,':',,,2,HR12,,','>
		cinfo	<CID_UK,437,DATE_DMY,156,,,,,',',,'.',,'/',,':',,,2,HR24,,','>	; cs3
;
fr437_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_FR,437,DATE_DMY,'F',,,,,' ',,',',,'/',,':',,3,2,HR24,,';'>
		cinfo	<CID_FR,437,DATE_DMY,'F',,,,,' ',,',',,'.',,':',,3,2,HR24,,';'>	; cs3
;
gr437_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_GR,437,DATE_DMY,'D','M',,,,'.',,',',,'.',,'.',,2,2,HR24,,';'>
		cinfo	<CID_GR,437,DATE_DMY,'D','M',,,,'.',,',',,'.',,':',,0,2,HR24,,';'> ; cs3

;
sp437_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_SP,437,DATE_DMY,158,,,,,'.',,',',,'/',,':',,3,2,HR24,,';'>
		cinfo	<CID_SP,437,DATE_DMY,158,,,,,'.',,',',,'/',,':',,2,0,HR24,,';'>	; cs3

;
sp850_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_SP,437,DATE_DMY,'P','t','s',,,'.',,',',,'/',,':',,3,2,HR24,,';'>
		cinfo	<CID_SP,437,DATE_DMY,'P','t','s',,,'.',,',',,'/',,':',,2,0,HR24,,';'>	; cs3

;
it437_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_IT,437,DATE_DMY,'L','.',,,,'.',,',',,'/',,':',,,,HR24,,';'>
		cinfo	<CID_IT,437,DATE_DMY,'L','.',,,,'.',,',',,'/',,'.',,2,,HR24,,';'>	; cs3
;
sv437_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_SV,437,DATE_YMD,'S','E','K',,,'.',,',',,'-',,'.',,2,2,HR24,,';'>
		cinfo	<CID_SV,437,DATE_YMD,'K','r',,,,' ',,',',,'-',,'.',,3,2,HR24,,';'>	; cs3
;
dk865_info	   label   word
		ctable	<>
		cinfo	<CID_DK,865,DATE_DMY,'k','r',,,,'.',,',',,'-',,'.',,2,2,HR24,,';'>
;
sw437_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_SW,437,DATE_DMY,'F','r',,,,"'",,'.',,'.',,'.',,2,2,HR24,,','>
		cinfo	<CID_SW,437,DATE_DMY,'F','r','.',,,"'",,'.',,'.',,',',,2,2,HR24,,';'>	;cs3
;
;*** CNS Change date separator from slash to period - DCR 357
no865_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_NO,865,DATE_DMY,'K','r',,,,'.',,',',,'.',,'.',,2,2,HR24,,';'> ;AN000;
		cinfo	<CID_NO,865,DATE_DMY,'K','r',,,,'.',,',',,'.',,':',,2,2,HR24,,';'> ;AN000;cs3
;*** CNS Change date separator from slash to period - DCR 357
;*** Field #14
;
nl437_info	   label   word
		ctable	<>
		cinfo	<CID_NL,437,DATE_DMY,159,,,,,'.',,',',,'-',,':',,2,2,HR24,,';'>
;
be437_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_BE,437,DATE_DMY,'B','E','F',,,'.',,',',,'/',,':',,2,2,HR24,,';'>
		cinfo	<CID_BE,437,DATE_DMY,'B','F',,,,'.',,',',,'/',,':',,2,2,HR24,,';'>	; cs3
;
fi437_info	   label   word
		ctable	<>
		cinfo	<CID_FI,437,DATE_DMY,'m','k',,,,' ',,',',,'.',,'.',,3,2,HR24,,';'>
;
jp437_info	   label   word
;cs3 ko437_info	   label   word
pr437_info	   label   word
;cs3 ta437_info	   label   word
us437_info	   label   word
		ctable	<>
		cinfo	<CID_US,437,DATE_MDY,'$',,,,,',',,'.',,'-',,':',,,2,HR12,,','>
;
is862_info	   label   word
		ctable	<>
		cinfo	<CID_IS,862,DATE_DMY,153,,,,,',',,'.',,' ',,':',,2,2,HR24,,','>
;
ca863_info	   label   word
		ctable	<>
		cinfo	<CID_CA,863,DATE_YMD,'$',,,,,' ',,',',,'-',,':',,3,2,HR24,,';'>
;
as864_info	   label   word
		ctable	<>
		cinfo	<CID_AS,864,DATE_DMY,164,,,,,'.',,',',,'/',,':',,3,3,HR12,,';'>
;
as850_info	   label   word
		ctable	<>
		cinfo	<CID_AS,864,DATE_DMY,207,,,,,'.',,',',,'/',,':',,3,3,HR12,,';'>
;
po860_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_PO,860,DATE_DMY,'$',,,,,'.',,',',,'/',,':',,4,2,HR24,,';'>
		cinfo	<CID_PO,860,DATE_DMY,'E','s','c','.',,'.',,',',,'-',,':',,3,2,HR24,,';'>	; cs3
;
la437_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_LA,437,DATE_DMY,'$',,,,,',',,'.',,'/',,':',,3,2,HR24,,';'>
		cinfo	<CID_LA,437,DATE_DMY,'$',,,,,',',,'.',,'/',,':',,0,2,HR12,,','>	; cs3
;
afe437_info	   label   word
		ctable	<>
		cinfo	<CID_AFE,437,DATE_DMY,'$',,,,,',',,'.',,'-',,':',,,2,HR12,,','>
;
jp932_info	   label   word
		ctable	<>
		cinfo	<CID_JP,932,DATE_YMD,'\',,,,,',',,'.',,'-',,':',,,0,HR24,,','>
;
ko934_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_KO,934,DATE_YMD,'\',,,,,',',,'.',,'-',,':',,,0,HR24,,','>
		cinfo	<CID_KO,934,DATE_YMD,'\',,,,,',',,'.',,'.',,':',,,0,HR24,,','>	; cs3
;
;	cs3 -- added separate entries for ko437 and ta437

ta437_info	label	word
		ctable	<>
		cinfo	<CID_US,437,DATE_YMD,'$',,,,,',',,'.',,'/',,':',,,2,HR12,,','>
ko437_info	label	word
		ctable	<>
		cinfo	<CID_US,437,DATE_MDY,'$',,,,,',',,'.',,'.',,':',,,2,HR12,,','>
;
;	cs3 -- end additions

pr936_info	   label   word
		ctable	<>
		cinfo	<CID_PR,936,DATE_YMD,'\',,,,,',',,'.',,'-',,':',,,2,HR24,,','>
;
ta938_info	   label   word
		ctable	<>
;cs3		cinfo	<CID_TA,938,DATE_MDY,'N','T','$',,,',',,'.',,'-',,':',,,2,HR24,,','>
		cinfo	<CID_TA,938,DATE_YMD,'N','T','$',,,',',,'.',,'/',,':',,,2,HR24,,','>	; cs3
;

;	cs3 -- begin additions for new countries

br850_info        label   word
	       ctable      <>
	       cinfo <CID_BR,850,DATE_DMY,'C','r','$',,,'.',,',',,'/',,':',,,2,HR24,,';'>
;
ic861_info        label   word
	       ctable   <>
	       cinfo    <CID_IC,861,DATE_DMY,'k','r',,,,'.',,',',,'-',,':',,3,2,HR24,,','>
;
ic850_info        label   word
	       ctable   <>
	       cinfo    <CID_IC,850,DATE_DMY,'k','r',,,,'.',,',',,'-',,':',,3,2,HR24,,','>
;
tr850_info        label   word
	       ctable   <>
	       cinfo    <CID_TR,850,DATE_DMY,'T','L',,,,'.',,',',,'/',,':',,4,2,HR24,,','>
;
tr857_info        label   word
	       ctable   <>
	       cinfo    <CID_TR,857,DATE_DMY,'T','L',,,,'.',,',',,'/',,':',,4,2,HR24,,','>
;
yu852_info        label   word   ; Country info for yugoslavia 852
	       ctable   <>
	       cinfo    <CID_YU,852,DATE_YMD,'D','i','n',,,'.',,',',,'-',,':',,2,2,HR24,,','>
;
cs852_info        label   word   ; Country info for CSSR   852
	       ctable   <>
	       cinfo    <CID_CS,852,DATE_YMD,'K',9FH,'s',,,'.',,',',,'-',,':',,2,2,HR24,,','>
;
pl852_info        label   word   ; Country info for poland 852
	       ctable   <>
	       cinfo    <CID_PL,852,DATE_YMD,'Z',88H,,,,'.',,',',,'-',,':',,,2,HR24,,','>
;
hu852_info        label   word   ; Country info for hungary 852
	       ctable   <>
	       cinfo	<CID_HU,852,DATE_YMD,'F','t',,,,' ',,',',,'-',,':',,,2,HR24,,','>
;
;	cs3 -- end additions for new countries

	page
; ------------------------------------------------
;
;	World Trade Collating sequence tables
;
; ------------------------------------------------
;
; --------------------------------------------------------------------------
;
;	Collating Table Same for United States, United Kingdom, France, Germany,
;			Italy, and International English
; --------------------------------------------------------------------------
;
us_collate	label	word
;
uk_collate	label	word
;
fr_collate	label	word
;
gr_collate	label	word
;
it_collate	label	word
;
jp437_collate	label	word
;
ko437_collate	label	word
;
pr437_collate	label	word
;
ta437_collate	label	word
;
afe_collate	label	word
;
;	cs3 -- begin added country

br437_collate	label	word

;	cs3 -- end additions

		ctable	<,'COLLATE',256>
		db	0,1,2,3,4,5,6,7
		db	8,9,10,11,12,13,14,15
		db	16,17,18,19,20,21,22,23
		db	24,25,26,27,28,29,30,31
		db	" ","!",'"',"#","$","%","&","'"
		db	"(",")","*","+",",","-",".","/"
		db	"0","1","2","3","4","5","6","7"
		db	"8","9",":",";","<","=",">","?"
		db	"@","A","B","C","D","E","F","G"
		db	"H","I","J","K","L","M","N","O"
		db	"P","Q","R","S","T","U","V","W"
		db	"X","Y","Z","[","\","]","^","_"
		db	"`","A","B","C","D","E","F","G"
		db	"H","I","J","K","L","M","N","O"
		db	"P","Q","R","S","T","U","V","W"
		db	"X","Y","Z","{","|","}","~",127
		db	"C","U","E","A","A","A","A","C"
		db	"E","E","E","I","I","I","A","A"
		db	"E","A","A","O","O","O","U","U"
		db	"Y","O","U","$","$","$","$","$"
		db	"A","I","O","U","N","N",166,167
		db	"?",169,170,171,172,"!",'"','"'
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,"S"
		db	226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
	;
; Netherland 437 Collate
;
nl_collate	label	word
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	128,154,144,065,142,065,143,128
		db	069,069,069,073,073,073,142,143
		db	144,146,146,079,079,079,085,085
		db	152,079,085,155,156,157,158,159
		db	065,073,079,085,165,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
;la_collate	 label	 word
;		 ctable  <,'COLLATE',256>
;		 db	 000,001,002,003,004,005,006,007
;		 db	 008,009,010,011,012,013,014,015
;		 db	 016,017,018,019,020,021,022,023
;		 db	 024,025,026,027,028,029,030,031
;		 db	 032,033,034,035,036,037,038,039
;		 db	 040,041,042,043,044,045,046,047
;		 db	 048,049,050,051,052,053,054,055
;		 db	 056,057,058,059,060,061,062,063
;		 db	 064,065,066,067,068,069,070,071
;		 db	 072,073,074,075,076,077,078,079
;		 db	 080,081,082,083,084,085,086,087
;		 db	 088,089,090,091,092,093,094,095
;		 db	 096,065,066,067,068,069,070,071
;		 db	 072,073,074,075,076,077,078,079
;		 db	 080,081,082,083,084,085,086,087
;		 db	 088,089,090,123,124,125,126,127
;		 db	 128,"U","E","A","A","A","A",128
;		 db	 "E","E","E","I","I","I","A","A"
;		 db	 "E","A","A","O","O","O","U","U"
;		 db	 "Y","O","U","$","$","$","$","$"
;		 db	 "A","I","O","U",165,165,166,167
;		 db	 "?",169,170,171,172,"!",'"','"'
;		 db	 176,177,178,179,180,181,182,183
;		 db	 184,185,186,187,188,189,190,191
;		 db	 192,193,194,195,196,197,198,199
;		 db	 200,201,202,203,204,205,206,207
;		 db	 208,209,210,211,212,213,214,215
;		 db	 216,217,218,219,220,221,222,223
;		 db	 224,"S"
;		 db	 226,227,228,229,230,231
;		 db	 232,233,234,235,236,237,238,239
;		 db	 240,241,242,243,244,245,246,247
;		 db	 248,249,250,251,252,253,254,255
	page
la_collate	label	word			  ; new for DOS 4.0, wants same as spain
;
sp_collate	label	word			   ; new for DOS 4.0
		ctable	<,'COLLATE',256>
		db 000,150,151,152,153,154,155,156 ;7
		db 157,158,159,160,161,162,163,164 ;15
		db 165,166,167,168,169,170,171,172 ;23
		db 173,174,175,176,177,178,179,180 ;31
		db 000,060,061,062,063,064,065,000 ;39
		db 066,067,068,069,070,000,071,072 ;47
		db 034,035,036,037,038,039,040,041 ;55
		db 042,043,073,074,075,076,077,078 ;63
		db 079,001,002,003,006,008,009,010 ;71
		db 011,012,013,014,015,017,018,020 ;79
		db 021,022,023,024,026,028,029,030 ;87
		db 031,032,033,080,081,082,083,084 ;95
		db 085,001,002,003,006,008,009,010 ;103
		db 011,012,013,014,015,017,018,020 ;111
		db 021,022,023,024,026,028,029,030 ;119
		db 031,032,033,086,087,088,089,090 ;127
		db 004,028,008,001,001,001,001,004 ;135
		db 008,008,008,012,012,012,001,001 ;143
		db 008,001,001,020,020,020,028,028 ;151
		db 032,020,028,091,092,093,094,095 ;159
		db 001,012,020,028,019,019,001,020 ;167
		db 096,097,098,099,100,101,102,103 ;175
		db 104,105,106,107,108,181,182,183 ;183
		db 184,109,110,111,112,185,186,113 ;191
		db 114,115,116,117,118,119,187,188 ;199
		db 120,121,122,123,124,125,126,189 ;207
		db 190,191,192,193,194,195,196,197 ;215
		db 198,127,128,129,130,199,200,131 ;223
		db 201,025,202,203,204,205,132,206 ;231
		db 207,208,209,210,211,212,213,214 ;239
		db 215,133,216,217,218,219,134,220 ;247
		db 221,222,000,223,224,135,136,225 ;255
	page
;
dk_collate	label	word		; Denmark (dk) 865
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,040,047,041,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,040,047,041,126,127
		db	067,089,069,065,091,065,093,067
		db	069,069,069,073,073,073,091,093
		db	069,091,091,079,092,079,085,085
		db	089,092,089,092,036,092,036,036
		db	065,073,079,085,078,078,065,079
		db	063,169,170,171,172,033,034,036
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,083,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
no_collate	label	word		; Norway (no) 865
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	067,089,069,065,091,065,093,067
		db	069,069,069,073,073,073,091,093
		db	069,091,091,079,092,079,085,085
		db	089,092,089,092,036,092,036,036
		db	065,073,079,085,078,078,065,079
		db	063,169,170,171,172,033,034,036
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,083,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
; Finland 437 Collate (same as Sweden)
;
fi_collate	label	word
;
sv_collate	label	word
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	067,089,069,065,092,065,091,067
		db	069,069,069,073,073,073,092,091
		db	069,092,092,079,093,079,085,085
		db	089,093,089,036,036,036,036,036
		db	065,073,079,085,078,078,166,167
		db	063,169,170,171,172,033,034,034
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,083,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
;  Israel 862 collate
;
is_collate	label	word
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	128,129,130,131,132,133,134,135
		db	136,137,138,139,140,141,142,143
		db	144,145,146,147,148,149,150,151
		db	152,153,154,036,036,036,036,036
		db	065,073,079,085,078,078,166,167
		db	063,169,170,171,172,033,034,034
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,083,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
; Canada 863 Collate
;
ca_collate	label	word
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	067,085,069,065,065,065,134,067
		db	069,069,069,073,073,141,065,143
		db	069,069,069,079,069,073,085,085
		db	152,079,085,155,156,085,085,159
		db	160,161,079,085,164,165,166,167
		db	073,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
; Portugal 860 collating
;
po_collate	label	word
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	067,085,069,065,065,065,065,067
		db	069,069,069,073,079,073,065,065
		db	069,065,069,079,079,079,085,085
		db	073,079,085,036,036,085,036,079
		db	065,073,079,085,078,078,166,167
		db	063,079,170,171,172,033,034,034
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,083,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
sw_collate	label	word
		ctable	<,'COLLATE',256>
		db	001,200,201,202,203,204,205,206
		db	207,208,209,210,211,212,213,214
		db	215,216,217,218,140,141,219,220
		db	221,222,223,224,225,226,227,228
		db	001,060,061,062,063,064,065,066  ;DCR059 CNS ******  ;AN000;
		db	067,068,069,070,071,072,073,074
		db	032,033,034,035,036,037,038,039
		db	040,041,075,076,077,078,079,080
		db	081,002,003,004,005,007,008,009
		db	010,011,012,013,014,015,016,018
		db	019,020,021,022,024,026,027,028
		db	029,030,031,082,083,084,052,085
		db	051,002,003,004,005,007,008,009
		db	010,011,012,013,014,015,016,018
		db	019,020,021,022,024,026,027,028
		db	029,030,031,086,087,088,054,089
		db	004,026,007,002,002,002,002,004
		db	007,007,007,011,011,011,002,002
		db	007,002,002,018,018,018,026,026
		db	030,018,026,111,090,112,150,092
		db	002,011,018,026,017,017,002,018
		db	093,151,095,096,097,098,099,100
		db	101,102,103,104,105,152,153,154
		db	155,107,108,109,110,156,157,113
		db	114,115,116,117,118,119,158,159
		db	120,121,122,123,124,125,126,160
		db	161,162,163,164,165,166,167,168
		db	169,128,129,130,131,170,171,133
		db	172,023,173,174,175,176,134,177
		db	178,179,180,181,182,183,184,185
		db	186,137,187,188,189,190,142,191
		db	143,192,144,193,194,147,148,001
	page
;
be_collate	label	word
		ctable	<,'COLLATE',256>
		db	255,255,255,255,255,255,255,255
		db	255,255,255,255,255,255,255,255
		db	255,255,255,255,255,255,255,255
		db	255,255,255,255,255,255,255,255
		db	255,033,034,035,036,037,038,255
		db	040,041,042,043,044,255,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	067,085,069,065,065,065,065,067
		db	069,069,069,073,073,073,065,065
		db	069,065,065,079,079,079,085,085
		db	089,079,085,155,156,157,158,159
		db	065,073,079,085,078,078,166,167
		db	168,169,170,171,172,173,174,175
		db	255,255,255,255,255,255,255,255
		db	255,255,255,255,255,255,255,255
		db	255,255,255,255,255,255,255,255
		db	255,255,255,255,255,255,255,255
		db	255,255,255,255,255,255,255,255
		db	255,255,255,255,255,255,255,255
		db	224,083,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,255,255
	page
;
; Area South 864 Collate
;
as_collate	label	word
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	128,129,130,131,132,133,134,135
		db	136,137,138,139,140,141,142,143
		db	144,145,146,147,148,149,150,151
		db	152,233,234,251,255,235,236,179
		db	153,154,182,155,156,184,253,254
		db	188,189,192,194,163,196,198,200
		db	164,165,166,167,168,169,170,171
		db	172,173,224,174,206,208,210,175
		db	157,180,181,183,185,217,186,187
		db	190,191,193,195,197,199,201,202
		db	203,204,205,207,209,211,213,214
		db	215,218,222,158,159,160,161,216
		db	178,225,227,229,237,239,241,243
		db	245,246,250,212,219,221,220,238
		db	177,176,240,242,244,247,249,223
		db	226,231,232,230,228,248,162,255


	page
;
; -----------------------------------------
;
;     Collating Tables for Code Page 850
;
; -----------------------------------------
;
fr850_collate	   label   word
;
ca850_collate	   label   word
;
us850_collate	   label   word
;
uk850_collate	   label   word
;
it850_collate	   label   word
;
;;sp850_collate      label   word  ; spain now has there own
;
is850_collate	   label   word
;
po850_collate	   label   word
;
;;la850_collate      label   word   ; wants same as spain
;
as850_collate	   label   word
;
gr850_collate	   label   word 	 ;omitted tobe consistent with US DCR
;
afe850_collate	   label   word
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	067,085,069,065,065,065,065,067
		db	069,069,069,073,073,073,065,065
		db	069,065,065,079,079,079,085,085
		db	089,079,085,079,036,079,158,036
		db	065,073,079,085,078,078,166,167
		db	063,169,170,171,172,033,034,034
		db	176,177,178,179,180,065,065,065
		db	184,185,186,187,188,036,036,191
		db	192,193,194,195,196,197,065,065
		db	200,201,202,203,204,205,206,036
		db	068,068,069,069,069,073,073,073
		db	073,217,218,219,220,221,073,223
		db	079,083,079,079,079,079,230,232
		db	232,085,085,085,089,089,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page

;formerly GERMAN COLLATE TABLE DCR0037
;		ctable	<,'COLLATE',256>
;		db	000,001,002,003,004,005,006,007
;		db	008,009,010,011,012,013,014,015
;		db	016,017,018,019,020,021,022,023
;		db	024,025,026,027,028,029,030,031
;		db	032,033,034,035,036,037,038,039
;		db	040,041,042,043,044,045,046,047
;		db	048,049,050,051,052,053,054,055
;		db	056,057,058,059,060,061,062,063
;		db	064,065,066,067,068,069,070,071
;		db	072,073,074,075,076,077,078,079
;		db	080,081,082,083,084,085,086,087
;		db	088,089,090,091,092,093,094,095
;		db	096,065,066,067,068,069,070,071
;		db	072,073,074,075,076,077,078,079
;		db	080,081,082,083,084,085,086,087
;		db	088,089,090,123,124,125,126,127
;		db	067,117,069,065,097,065,065,067
;		db	069,069,069,073,073,073,065,065
;		db	069,065,065,079,111,079,085,085
;		db	089,079,085,079,036,079,158,036
;		db	065,073,079,085,078,078,166,167
;		db	063,169,170,171,172,033,034,034
;		db	176,177,178,179,180,065,065,065
;		db	184,185,186,187,188,036,036,191
;		db	192,193,194,195,196,197,065,065
;		db	200,201,202,203,204,205,206,036
;		db	068,068,069,069,069,073,073,073
;		db	073,217,218,219,220,221,073,223
;		db	079,115,079,079,079,079,230,232
;		db	232,085,085,085,089,089,238,239
;		db	240,241,242,243,244,245,246,247
;		db	248,249,250,251,252,253,254,255
	page
;
nl850_collate	  label   word
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	067,085,069,065,065,065,143,067
		db	069,069,069,073,073,073,065,143
		db	069,146,146,079,079,079,085,085
		db	152,079,085,079,156,079,158,159
		db	065,073,079,085,165,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,065,065,065
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,065,065
		db	200,201,202,203,204,205,206,207
		db	209,209,069,069,069,073,073,073
		db	073,217,218,219,220,221,073,223
		db	079,225,079,079,079,079,230,232
		db	232,085,085,085,089,089,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
dk850_collate	   label   word
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,040,047,041,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,040,047,041,126,127
		db	067,089,069,065,091,065,093,067
		db	069,069,069,073,073,073,091,093
		db	069,091,091,079,092,079,085,085
		db	089,092,089,092,036,092,158,036
		db	065,073,079,085,078,078,065,079
		db	063,169,170,171,172,033,034,034
		db	176,177,178,179,180,065,065,065
		db	184,185,186,187,188,036,036,191
		db	192,193,194,195,196,197,065,065
		db	200,201,202,203,204,205,206,036
		db	068,068,069,069,069,073,073,073
		db	073,217,218,219,220,221,073,223
		db	079,083,079,079,079,079,230,080
		db	080,085,085,085,089,089,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
no850_collate	   label   word
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	067,085,069,065,091,065,093,067
		db	069,069,069,073,073,073,091,093
		db	069,091,091,079,092,079,085,085
		db	089,092,089,092,036,092,158,036
		db	065,073,079,085,078,078,065,079
		db	063,169,170,171,172,033,034,034
		db	176,177,178,179,180,065,065,065
		db	184,185,186,187,188,036,036,191
		db	192,193,194,195,196,197,065,065
		db	200,201,202,203,204,205,206,036
		db	068,068,069,069,069,073,073,073
		db	073,217,218,219,220,221,073,223
		db	079,083,079,079,079,079,230,080
		db	080,085,085,085,089,089,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
sv850_collate	  label   word
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	067,089,069,065,092,065,091,067
		db	069,069,069,073,073,073,092,091
		db	069,092,092,079,093,079,085,085
		db	089,093,089,093,036,093,158,036
		db	065,073,079,085,078,078,166,167
		db	063,169,170,171,172,033,034,034
		db	176,177,178,179,180,065,065,065
		db	184,185,186,187,188,036,036,191
		db	192,193,194,195,196,197,065,065
		db	200,201,202,203,204,205,206,036
		db	068,068,069,069,069,073,073,073
		db	073,217,218,219,220,221,073,223
		db	079,083,079,079,093,093,230,232
		db	232,085,085,085,089,089,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
fi850_collate	   label   word
		ctable	<,'COLLATE',256>
		db	000,001,002,003,004,005,006,007
		db	008,009,010,011,012,013,014,015
		db	016,017,018,019,020,021,022,023
		db	024,025,026,027,028,029,030,031
		db	032,033,034,035,036,037,038,039
		db	040,041,042,043,044,045,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	067,089,069,065,092,065,091,067
		db	069,069,069,073,073,073,092,091
		db	069,092,092,079,093,079,085,085
		db	089,093,089,093,036,093,158,036
		db	065,073,079,085,078,078,166,167
		db	063,169,170,171,172,033,034,034
		db	176,177,178,179,180,065,065,065
		db	184,185,186,187,188,036,036,191
		db	192,193,194,195,196,197,065,065
		db	200,201,202,203,204,205,206,036
		db	068,068,069,069,069,073,073,073
		db	073,217,218,219,220,221,073,223
		db	079,083,079,079,079,079,230,232
		db	232,085,085,085,089,089,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
sw850_collate	   label   word
		ctable	<,'COLLATE',256>

		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
		db	000,165,168,133,185,134,135,169
		db	170,171,136,158,172,174,175,176
		db	117,118,120,122,124,125,126,127
		db	128,129,177,178,160,161,162,179
		db	137,002,018,020,024,028,038,040
		db	042,044,055,057,059,061,063,067
		db	081,083,085,087,090,094,104,106
		db	108,110,115,138,139,140,191,173
		db	190,003,019,021,025,029,039,041
		db	043,056,045,058,060,062,064,068
		db	082,084,086,088,091,095,105,107
		db	109,111,116,142,143,144,193,157
		db	022,103,031,009,011,007,015,023
		db	035,037,033,053,051,049,010,014
		db	030,017,016,074,076,072,101,099
		db	114,075,102,080,184,079,164,188
		db	005,047,070,097,066,065,149,148
		db	180,154,156,131,130,166,181,182
		db	207,208,209,206,204,004,008,006
		db	153,210,211,212,213,186,187,198
		db	197,202,201,203,205,200,013,012
		db	214,215,216,217,218,219,220,183
		db	027,026,034,036,032,054,046,050
		db	052,199,196,223,221,155,048,222
		db	069,089,073,071,078,077,146,092
		db	093,096,100,098,113,112,145,189
		db	167,159,141,132,151,150,163,194
		db	147,192,195,119,123,121,152,001

;		db	001,200,201,202,203,204,205,206
;		db	207,208,209,210,211,212,213,214
;		db	215,216,217,218,140,141,219,220
;		db	221,222,223,224,225,226,227,228
;		db	001,060,061,062,063,064,065,066
;		db	067,068,069,070,071,072,073,074
;		db	032,033,034,035,036,037,038,039
;		db	040,041,075,076,077,078,079,080
;		db	081,002,003,004,005,007,008,009
;		db	010,011,012,013,014,015,016,018
;		db	019,020,021,022,024,026,027,028
;		db	029,030,031,082,083,084,052,085
;		db	051,002,003,004,005,007,008,009
;		db	010,011,012,013,014,015,016,018
;		db	019,020,021,022,024,026,027,028
;		db	029,030,031,086,087,088,054,089
;		db	004,026,007,002,002,002,002,004
;		db	007,007,007,011,011,011,002,002
;		db	007,002,002,018,018,018,026,026
;		db	030,018,026,018,090,018,091,092
;		db	002,011,018,026,017,017,002,018
;		db	093,094,095,096,097,098,099,100
;		db	101,102,103,104,105,002,002,002
;		db	106,107,108,109,110,111,112,113
;		db	114,115,116,117,118,119,002,002
;		db	120,121,122,123,124,125,126,127
;		db	006,006,007,007,007,011,011,011
;		db	011,128,129,130,131,132,011,133
;		db	018,023,018,018,018,018,134,025
;		db	025,026,026,026,030,030,135,050
;		db	136,137,138,139,140,141,142,055
;		db	143,053,144,145,146,147,148,001
	page
;
be850_collate	label	word
		ctable	<,'COLLATE',256>
		db	255,255,255,255,255,255,255,255
		db	255,255,255,255,255,255,255,255
		db	255,255,255,255,255,255,255,255
		db	255,255,255,255,255,255,255,255
		db	255,033,034,035,036,037,038,255
		db	040,041,042,043,044,255,046,047
		db	048,049,050,051,052,053,054,055
		db	056,057,058,059,060,061,062,063
		db	064,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,091,092,093,094,095
		db	096,065,066,067,068,069,070,071
		db	072,073,074,075,076,077,078,079
		db	080,081,082,083,084,085,086,087
		db	088,089,090,123,124,125,126,127
		db	067,085,069,065,065,065,065,067
		db	069,069,069,073,073,073,065,065
		db	069,065,065,079,079,079,085,085
		db	089,079,085,079,156,079,158,159
		db	065,073,079,085,078,164,166,167
		db	168,169,170,171,172,173,174,175
		db	255,255,255,255,255,065,065,065
		db	184,255,255,255,255,189,190,255
		db	255,255,255,255,255,255,065,065
		db	255,255,255,255,255,255,255,207
		db	068,068,069,069,069,073,073,073
		db	073,255,255,255,255,221,073,255
		db	079,083,079,079,079,079,230,084
		db	084,085,085,085,089,089,238,239
		db	255,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,255,255
	page

la850_collate	   label   word 		   ; wants same as spain
;
sp850_collate	   label   word 		   ; new for DOS 4.0
		ctable	<,'COLLATE',256>
		db 000,150,151,152,153,154,155,156 ;7
		db 157,158,159,160,161,162,163,164 ;15
		db 165,166,167,168,169,170,171,172 ;23
		db 173,174,175,176,177,178,179,180 ;31
		db 000,060,061,062,063,064,065,000 ;39
		db 066,067,068,069,070,000,071,072 ;47
		db 034,035,036,037,038,039,040,041 ;55
		db 042,043,073,074,075,076,077,078 ;63
		db 079,001,002,003,006,008,009,010 ;71
		db 011,012,013,014,015,017,018,020 ;79
		db 021,022,023,024,026,028,029,030 ;87
		db 031,032,033,080,081,082,083,084 ;95
		db 085,001,002,003,006,008,009,010 ;103
		db 011,012,013,014,015,017,018,020 ;111
		db 021,022,023,024,026,028,029,030 ;119
		db 031,032,033,086,087,088,089,090 ;127
		db 004,028,008,001,001,001,001,004 ;135
		db 008,008,008,012,012,012,001,001 ;143
		db 008,001,001,020,020,020,028,028 ;151
		db 032,020,028,020,092,020,094,095 ;159
		db 001,012,020,028,019,019,001,020 ;167
		db 096,097,098,099,100,101,102,103 ;175
		db 104,105,106,107,108,001,001,001 ;183
		db 184,109,110,001,001,091,093,113 ;191
		db 114,115,116,117,118,119,187,188 ;199
		db 120,121,122,123,124,125,126,189 ;207
		db 007,007,008,008,008,012,012,012 ;215
		db 012,127,128,129,130,199,012,131 ;223
		db 020,025,020,020,020,020,132,027 ;231
		db 027,028,028,028,032,032,213,214 ;239
		db 000,133,216,217,218,219,134,220 ;247
		db 221,222,000,223,224,135,136,225 ;255

	page
;
jp932_collate	   label   word
		ctable	<,'COLLATE',256>
		db	0,1,2,3,4,5,6,7
		db	8,9,10,11,12,13,14,15
		db	16,17,18,19,20,21,22,23
		db	24,25,26,27,28,29,30,31
		db	" ","!",'"',"#","$","%","&","'"
		db	"(",")","*","+",",","-",".","/"
		db	"0","1","2","3","4","5","6","7"
		db	"8","9",":",";","<","=",">","?"
		db	"@","A","B","C","D","E","F","G"
		db	"H","I","J","K","L","M","N","O"
		db	"P","Q","R","S","T","U","V","W"
		db	"X","Y","Z","[","\","]","^","_"
		db	"`","A","B","C","D","E","F","G"
		db	"H","I","J","K","L","M","N","O"
		db	"P","Q","R","S","T","U","V","W"
		db	"X","Y","Z","{","|","}","~",127
		db	128,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	129,130,131,132,133,189,134,135
		db	136,137,138,139,140,141,142,143
		db	144,145,146,147,148,149,150,151
		db	152,153,154,155,156,157,158,159
		db	160,161,162,163,164,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,190,191,192
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page
;
ko934_collate	   label   word
		ctable	<,'COLLATE',256>
		db	0,1,2,3,4,5,6,7
		db	8,9,10,11,12,13,14,15
		db	16,17,18,19,20,21,22,23
		db	24,25,26,27,28,29,30,31
		db	" ","!",'"',"#","$","%","&","'"
		db	"(",")","*","+",",","-",".","/"
		db	"0","1","2","3","4","5","6","7"
		db	"8","9",":",";","<","=",">","?"
		db	"@","A","B","C","D","E","F","G"
		db	"H","I","J","K","L","M","N","O"
		db	"P","Q","R","S","T","U","V","W"
		db	"X","Y","Z","[","\","]","^","_"
		db	"`","A","B","C","D","E","F","G"
		db	"H","I","J","K","L","M","N","O"
		db	"P","Q","R","S","T","U","V","W"
		db	"X","Y","Z","{","|","}","~",127
		db	128,190,191,192,193,194,195,196
		db	197,198,199,200,201,202,203,204
		db	205,206,207,208,209,210,211,212
		db	213,214,215,216,217,218,219,220
		db	221,222,223,224,225,226,227,228
		db	229,230,231,232,233,234,235,236
		db	237,238,239,240,241,242,243,244
		db	245,246,247,248,249,250,251,252
		db	129,130,131,132,133,134,135,136
		db	137,138,139,140,141,142,143,144
		db	145,146,147,148,149,150,151,152
		db	153,154,155,156,157,158,159,160
		db	161,162,163,164,165,166,167,168
		db	169,170,171,172,173,174,175,176
		db	177,178,179,180,181,182,183,184
		db	185,186,187,188,189,253,254,255

	page
;
pr936_collate	   label   word
;
ta938_collate	   label   word
		ctable	<,'COLLATE',256>
		db	0,1,2,3,4,5,6,7
		db	8,9,10,11,12,13,14,15
		db	16,17,18,19,20,21,22,23
		db	24,25,26,27,28,29,30,31
		db	" ","!",'"',"#","$","%","&","'"
		db	"(",")","*","+",",","-",".","/"
		db	"0","1","2","3","4","5","6","7"
		db	"8","9",":",";","<","=",">","?"
		db	"@","A","B","C","D","E","F","G"
		db	"H","I","J","K","L","M","N","O"
		db	"P","Q","R","S","T","U","V","W"
		db	"X","Y","Z","[","\","]","^","_"
		db	"`","A","B","C","D","E","F","G"
		db	"H","I","J","K","L","M","N","O"
		db	"P","Q","R","S","T","U","V","W"
		db	"X","Y","Z","{","|","}","~",127
		db	128,129,130,131,132,133,134,135
		db	136,137,138,139,140,141,142,143
		db	144,145,146,147,148,149,150,151
		db	152,153,154,155,156,157,158,159
		db	160,161,162,163,164,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255

	 page
;
;	cs3 -- begin additions for new languages
;
ic_collate      label   word            ; Iceland (ic) 861
		ctable  <,'COLLATE',256>
		db      000,001,002,003,004,005,006,007
		db      008,009,010,011,012,013,014,015
		db      016,017,018,019,020,021,022,023
		db      024,025,026,027,028,029,030,031
		db      032,033,034,035,036,037,038,039
		db      040,041,042,043,044,045,046,047
		db      048,049,050,051,052,053,054,055
		db      056,057,058,059,060,061,062,063
		db      064,065,067,068,069,071,073,074 ;@,A
		db      075,076,078,079,080,081,082,083 ;H
		db      085,086,087,088,089,090,092,093 ;P
		db      094,095,097,040,047,041,094,095 ;X
		db      049,065,067,068,069,071,073,074 ;'
		db      075,076,078,079,080,081,082,083 ;h
		db      085,086,087,088,089,090,092,093 ;p
		db      094,095,097,040,047,041,126,127 ;x
		db      068,091,072,066,066,066,066,068 ;C cedilla
		db      072,072,072,070,070,098,066,066 ;e circ
		db      072,099,099,084,100,098,091,096 ;E acute
		db      096,100,091,084,036,084,036,036 ;y acute
		db      066,077,084,091,066,077,084,091 ;a acute
		db      063,169,170,171,172,033,034,036 ;inverted ?
		db      176,177,178,179,180,181,182,183 ; other stuff follows
		db      184,185,186,187,188,189,190,191
		db      192,193,194,195,196,197,198,199
		db      200,201,202,203,204,205,206,207
		db      208,209,210,211,212,213,214,215
		db      216,217,218,219,220,221,222,223
		db      224,083,226,227,228,229,230,231
		db      232,233,234,235,236,237,238,239
		db      240,241,242,243,244,245,246,247
		db      248,249,250,251,252,253,254,255
	 page

;
yu852_collate    label   word
cs852_collate         label   word
sl852_collate         label   word
hu852_collate         label   word
pl852_collate         label   word
;
		ctable  <,'COLLATE',256>
;
;                       000 001 002 003 004 005 006 007
;                       -------------------------------
		db      000,001,002,003,004,005,006,007


;                       008 009 010 011 012 013 014 015
;                       -------------------------------
		db      008,009,010,011,012,013,014,015
;
;                       016 017 018 019 020 021 022 023
;                       -------------------------------
		db      016,017,018,019,020,021,022,023
;
;                       024 025 026 027 028 029 030 031
;                       -------------------------------
		db      024,025,026,027,028,029,030,031
;
;                            !   "   #   $   %   &   '
;                       -------------------------------
		db      110,111,112,113,114,115,116,117
;
;                        (   )   *   +   ,   -   .   /
;                       -------------------------------
		db      118,119,120,121,122,123,124,125
;
;                        0   1   2   3   4   5   6   7
;                       -------------------------------
		db      100,101,102,103,104,105,106,107
;
;                        8   9   :   ;   <   =   >   ?
;                       -------------------------------
		db      108,109,126,127,128,129,130,131
;
;                        @   A   B   C   D   E   F   G
;                       -------------------------------
		db      132,033,039,040,044,047,052,053
;
;                        H   I   J   K   L   M   N   O
;                       -------------------------------
		db      054,055,058,059,060,064,065,068
;
;                        P   Q   R   S   T   U   V   W
;                       -------------------------------
		db      073,074,075,078,083,086,091,092
;
;                        X   Y   Z   [   \   ]   ^   _
;                       -------------------------------
		db      093,094,096,133,134,135,136,137
;
;                        `   a   b   c   d   e   f   g
;                       -------------------------------
		db      138,033,039,040,044,047,052,053
;
;                        h   i   j   k   l   m   n   o
;                       -------------------------------
		db      054,055,058,059,060,064,065,068
;
;                        p   q   r   s   t   u   v   w
;                       -------------------------------
		db      073,074,075,078,083,086,091,092
;
;                        x   y   z   {   |   }   ~   
;                       -------------------------------
		db      093,094,096,139,140,141,142,032
;
;                        Ä   Å   Ç   É   Ñ   Ö   Ü   á
;                       -------------------------------
		db      043,089,048,038,035,088,041,043
;
;                        à   â   ä   ã   å   ç   é   è
;                       -------------------------------
		db      063,051,072,072,057,098,035,041
;
;                        ê   ë   í   ì   î   ï   ñ   ó
;                       -------------------------------
		db      048,061,061,070,071,062,062,082
;
;                        ò   ô   ö   õ   ú   ù   û   ü
;                       -------------------------------
		db      082,071,089,084,084,063,143,042
;
;                        †   °   ¢   £   §   •   ¶   ß
;                       -------------------------------
		db      034,056,069,087,036,036,097,097
;
;                        ®   ©  SPC  ´   ¨   ≠   Æ   Ø
;                       -------------------------------
		db      050,050,144,098,042,081,145,146
;
;                        ∞   ±   ≤   ≥   ¥   µ   ∂   ∑
;                       -------------------------------
		db      151,152,153,154,155,034,038,049
;
;                        ∏   π   ∫   ª   º   Ω   æ   ø
;                       -------------------------------
		db      081,156,157,158,159,099,099,160
;
;                        ¿   ¡   ¬   √   ƒ   ≈   ∆   «
;                       -------------------------------
		db      161,162,163,164,165,166,037,037
;
;                        »   …       À   Ã   Õ   Œ   œ
;                       -------------------------------
		db      167,168,169,170,171,172,173,147
;
;                        –   —   “   ”   ‘   ’   ÷   ◊
;                       -------------------------------
		db      045,045,046,051,046,066,056,057
;
;                        ÿ   Ÿ   ⁄   €   ‹   ›   ﬁ   ﬂ
;                       -------------------------------
		db      049,174,175,176,177,085,088,178
;
;                        ‡   ·   ‚   „   ‰   Â   Ê   Á
;                       -------------------------------
		db      069,079,070,067,067,066,080,080
;
;                        Ë   È   Í   Î   Ï   Ì   Ó   Ô
;                       -------------------------------
		db      076,087,076,090,095,095,085,180
;
;                           Ò   Ú   Û   Ù   ı   ˆ   ˜
;                       -------------------------------
		db      148,181,182,183,184,149,150,185
;
;                        ¯   ˘   ˙   ˚   ¸   ˝   ˛  IGN
;                       -------------------------------
		db      186,187,188,090,077,077,179,255
;
	page
;
;  ***  MODIFIED FOR CP 857  ***  FI 16/03/90
tr857_collate      label   word
;
		ctable  <,'COLLATE',256>
;
;  0
		db      000,001,002,003,004,005,006,007
		db      008,009,010,011,012,013,014,015
;
;  1
		db      016,017,018,019,020,021,022,023
		db      024,025,026,027,028,029,030,031
;
;
;  2                    sp   !   "   #   $   %   &   '
		db      032,040,054,076,070,077,075,053
		db      057,058,073,078,037,036,045,044
;                        (   )   *   +   ,   -   .   /
;
;  3                     0   1   2   3   4   5   6   7
		db      091,092,093,094,095,096,097,098
		db      099,100,039,038,082,083,084,042
;                        8   9   :   ;   <   =   >   ?
;
;  4                     @   A   B   C   D   E   F   G
		db      067,101,102,103,105,106,107,108
		db      110,111,113,114,115,116,117,118
;                        H   I   J   K   L   M   N   O
;
;  5                     P   Q   R   S   T   U   V   W
		db      120,121,122,123,125,126,128,129
		db      130,131,132,059,074,060,048,033
;                        X   Y   Z   [   \   ]   ^   _
;
;  6                     `   a   b   c   d   e   f   g
		db      047,101,102,103,105,106,107,108
		db      110,112,113,114,115,116,117,118
;                        h   i   j   k   l   m   n   o
;
;  7                     p   q   r   s   t   u   v   w
		db      120,121,122,123,125,126,128,129
		db      130,131,132,061,086,062,050,150
;                        x   y   z   {   |   }   ~   
;
;  8                     Ä   Å   Ç   É   Ñ   Ö   Ü   á
		db      104,127,106,101,101,101,101,104
		db      106,106,106,112,112,111,101,101
;                        à   â   ä   ã   å   ç   é   è
;
;  9                     ê   ë   í   ì   î   ï   ñ   ó
		db      106,101,101,118,119,118,126,126
		db      112,119,127,118,071,118,124,124
;                        ò   ô   ö   õ   ú   ù   û   ü
;
;  A                     †   °   ¢   £   §   •   ¶   ß
		db      101,112,118,126,117,117,109,109
		db      043,066,085,091,091,041,055,056
;                        ®   ©   ™   ´   ¨   ≠   Æ   Ø
;
;  B               HEX  B0   1   2   3   4   5   6   7
		db      151,152,153,154,155,101,101,101
		db      065,156,157,158,159,069,072,160
;                  HEX  B8   9   A   B   C   D   E   F
;
;  C               HEX  C0   1   2   3   4   5   6   7
		db      161,162,163,164,165,166,101,101
		db      167,168,169,170,171,172,173,068
;                  HEX  C8   9   A   B   C   D   E   F
;
;  D               HEX  D0   1   2   3   4   5   6   7
		db      118,101,106,106,106,032,112,112
		db      112,174,175,176,177,087,112,178
;                  HEX  D8   9   A   B   C   D   E   F
;
;  E               HEX  E0   1   2   3   4   5   6   7
		db      118,123,118,118,118,118,089,032
		db      081,126,126,126,112,131,034,046
;                  HEX  E8   9   A   B   C   D   E   F
;
;  F               HEX  F0   1   2   3   4   5   6   7
		db      035,079,032,091,064,063,080,052
		db      088,049,051,092,094,093,179,090
;                  HEX  F8   9   A   B   C   D   E   F
;
	page
;
;
; Brazil 860 collating
;
br_collate     label word
	       ctable      <,'COLLATE',256>
	       db    000,001,002,003,004,005,006,007
	       db    008,009,010,011,012,013,014,015
	       db    016,017,018,019,020,021,022,023
	       db    024,025,026,027,028,029,030,031
	       db    032,033,034,035,036,037,038,039
	       db    040,041,042,043,044,045,046,047
	       db    048,049,050,051,052,053,054,055
	       db    056,057,058,059,060,061,062,063
	       db    064,065,066,067,068,069,070,071
	       db    072,073,074,075,076,077,078,079
	       db    080,081,082,083,084,085,086,087
	       db    088,089,090,091,092,093,094,095
	       db    096,065,066,067,068,069,070,071
	       db    072,073,074,075,076,077,078,079
	       db    080,081,082,083,084,085,086,087
	       db    088,089,090,123,124,125,126,127
	       db    067,085,069,065,065,065,065,067
	       db    069,069,069,073,079,073,065,065
	       db    069,065,069,079,079,079,085,085
	       db    073,079,085,036,036,085,036,079
	       db    065,073,079,085,078,078,166,167
	       db    063,079,170,171,172,033,034,034
	       db    176,177,178,179,180,181,182,183
	       db    184,185,186,187,188,189,190,191
	       db    192,193,194,195,196,197,198,199
	       db    200,201,202,203,204,205,206,207
	       db    208,209,210,211,212,213,214,215
	       db    216,217,218,219,220,221,222,223
	       db    224,083,226,227,228,229,230,231
	       db    232,233,234,235,236,237,238,239
	       db    240,241,242,243,244,245,246,247
	       db    248,249,250,251,252,253,254,255
	 page
;
br850_collate	label	word		; cs3 *** this may match another

	       ctable      <,'COLLATE',256>
	       db    000,001,002,003,004,005,006,007
	       db    008,009,010,011,012,013,014,015
	       db    016,017,018,019,020,021,022,023
	       db    024,025,026,027,028,029,030,031
	       db    032,033,034,035,036,037,038,039
	       db    040,041,042,043,044,045,046,047
	       db    048,049,050,051,052,053,054,055
	       db    056,057,058,059,060,061,062,063
	       db    064,065,066,067,068,069,070,071
	       db    072,073,074,075,076,077,078,079
	       db    080,081,082,083,084,085,086,087
	       db    088,089,090,091,092,093,094,095
	       db    096,065,066,067,068,069,070,071
	       db    072,073,074,075,076,077,078,079
	       db    080,081,082,083,084,085,086,087
	       db    088,089,090,123,124,125,126,127
	       db    067,085,069,065,065,065,065,067
	       db    069,069,069,073,073,073,065,065
	       db    069,065,065,079,079,079,085,085
	       db    089,079,085,079,036,079,158,036
	       db    065,073,079,085,078,078,166,167
	       db    063,169,170,171,172,033,034,034
	       db    176,177,178,179,180,065,065,065
	       db    184,185,186,187,188,036,036,191
	       db    192,193,194,195,196,197,065,065
	       db    200,201,202,203,204,205,206,036
	       db    068,068,069,069,069,073,073,073
	       db    073,217,218,219,220,221,073,223
	       db    079,083,079,079,079,079,230,232
	       db    232,085,085,085,089,089,238,239
	       db    240,241,242,243,244,245,246,247
	       db    248,249,250,251,252,253,254,255
	 page

ic850_collate         label   word
tr850_collate         label   word
yu850_collate         label   word
cs850_collate         label   word
sl850_collate         label   word
hu850_collate         label   word
pl850_collate         label   word
		ctable  <,'COLLATE',256>
		db      000,001,002,003,004,005,006,007
		db      008,009,010,011,012,013,014,015
		db      016,017,018,019,020,021,022,023
		db      024,025,026,027,028,029,030,031
		db      032,033,034,035,036,037,038,039
		db      040,041,042,043,044,045,046,047
		db      048,049,050,051,052,053,054,055
		db      056,057,058,059,060,061,062,063
		db      064,065,067,068,069,071,073,074 ;@
		db      075,076,078,079,080,081,082,083 ;H
		db      085,086,087,088,089,090,092,093 ;P
		db      094,095,097,040,047,041,094,095 ;X
		db      096,065,067,068,069,071,073,074 ;'
		db      075,076,078,079,080,081,082,083 ;h
		db      085,086,087,088,089,090,092,093 ;p
		db      094,095,097,040,047,041,126,127 ;x
		db      068,091,072,066,066,066,066,068 ;c cedilla
		db      072,072,072,077,077,077,066,066 ;e circ
		db      072,099,099,084,100,084,091,091 ;E acute
		db      096,100,091,084,036,084,158,036 ;y diar
		db      066,077,084,091,082,082,065,084 ;a acute
		db      063,169,170,171,172,033,034,034 ;a underscore
		db      176,177,178,179,180,066,066,066 ;sf1400
		db      184,185,186,187,188,036,036,191 ;copywrite symbol
		db      192,193,194,195,196,197,066,066 ;lower left single
		db      200,201,202,203,204,205,206,036 ;lower left double
		db      070,070,072,072,072,076,077,077 ;eth
		db      077,217,218,219,220,221,077,223 ;I diar
		db      084,067,084,084,084,084,230,098 ;O acute
		db      098,091,091,091,096,096,238,239 ;THORN
		db      240,241,242,243,244,245,246,247 ;sp32
		db      248,249,250,251,252,253,254,255 ;overcircle
	page
;
;	cs3 -- end additions for new languages
;
; ---------------------------------------------------------------
;
;	World Trade Case Mappings
;
; ---------------------------------------------------------------
;
us_ucase	label	word
;
uk_ucase	label	word
;
fr_ucase	label	word
;
gr_ucase	label	word
;
sp_ucase	label	word
;
la_ucase	label	word
;
it_ucase	label	word
;
afe_ucase	label	word
;
as_ucase	label	word
;
jp437_ucase	label	word
;
ko437_ucase	label	word
;
pr437_ucase	label	word
;
ta437_ucase	label	word
;
;	cs3 -- brazil added to this table

br_ucase label word

;	cs3 -- end additions

;
		ctable	<,'UCASE  ',128>
;***CNS PTM2390 table omitted	;AN000;
;***CNS restored for PTM 2575 to establish consistent and maintain 3.3 &
;under compatibility
	       db      128,154,069,065,142,065,143,128
	       db      069,069,069,073,073,073,142,143
	       db      144,146,146,079,153,079,085,085
	       db      089,153,154,155,156,157,158,159
	       db      065,073,079,085,165,165,166,167
	       db      168,169,170,171,172,173,174,175
	       db      176,177,178,179,180,181,182,183
	       db      184,185,186,187,188,189,190,191
	       db      192,193,194,195,196,197,198,199
	       db      200,201,202,203,204,205,206,207
	       db      208,209,210,211,212,213,214,215
	       db      216,217,218,219,220,221,222,223
	       db      224,225,226,227,228,229,230,231
	       db      232,233,234,235,236,237,238,239
	       db      240,241,242,243,244,245,246,247
	       db      248,249,250,251,252,253,254,255
;***CNS PTM2390 table omitted
;**CNS	replacement
;		 db	 067,085,069,065,065,065,065,067  ;AD000;
;		 db	 069,069,069,073,073,073,065,065  ;AD000;
;		 db	 069,146,146,079,079,079,085,085  ;AD000;
;		 db	 089,079,085,079,156,079,158,159  ;AD000;
;		 db	 065,073,079,085,165,165,166,167  ;AD000;
;		 db	 168,169,170,171,172,173,174,175  ;AD000;
;		 db	 176,177,178,179,180,065,065,065  ;AD000;
;		 db	 184,185,186,187,188,189,190,191  ;AD000;
;		 db	 192,193,194,195,196,197,065,065  ;AD000;
;		 db	 200,201,202,203,204,205,206,207  ;AD000;
;		 db	 209,209,069,069,069,073,073,073  ;AD000;
;		 db	 073,217,218,219,220,221,073,223  ;AD000;
;		 db	 079,225,079,079,079,079,230,232  ;AD000;
;		 db	 232,085,085,085,089,089,238,239  ;AD000;
;		 db	 240,241,242,243,244,245,246,247  ;AD000;
;		 db	 248,249,250,251,252,253,254,255  ;AD000;
;
page
;	Canadian French Code Page 863
;
ca_ucase	label	word
		ctable	<,'UCASE  ',128>
		db	067,085,069,065,065,065,134,067
		db	069,069,069,073,073,141,065,143
		db	069,069,069,079,069,073,085,085
		db	152,079,085,155,156,085,085,159
		db	160,161,079,085,164,165,166,167
		db	073,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
page
;	Denmark (Da) case mapping for 865
;
dk_ucase	label	word
;
;	Norway (No) case mapping for 865
;
no_ucase	label	word
		ctable	<,'UCASE  ',128>
		db	128,154,144,065,142,065,143,128
		db	069,069,069,073,073,073,142,143
		db	144,146,146,079,153,079,085,085
		db	089,153,154,157,156,157,158,159
		db	065,073,079,085,165,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
page
;	Belgium (Be) case mapping for 437
;
be_ucase	label	word
;
;	Finland (Fi) case mapping for 437
;
fi_ucase	label	word
;
;	Sweden (Sv) case mapping
;
sv_ucase	label	word
		ctable	<,'UCASE  ',128>
		db	128,154,144,065,142,065,143,128
		db	069,069,069,073,073,073,142,143
		db	144,146,146,079,153,079,085,085
		db	089,153,154,155,156,157,158,159
		db	065,073,079,085,165,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
page
;	Switzerland case mapping for 437
;
sw_ucase	label	word
		ctable	<,'UCASE  ',128>
		db	128,154,144,065,142,065,143,128
		db	069,069,069,073,073,073,142,143
		db	144,146,146,079,153,079,085,085
		db	089,153,154,155,156,157,158,159
		db	065,073,079,085,165,165,166,167    ;AN000;;DCR 059 CNS ***
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
page
;
;	Netherlands case mapping for 437
;
nl_ucase	label	word
		ctable	<,'UCASE  ',128>
		db	128,085,069,065,065,065,143,128
		db	069,069,069,073,073,073,065,143
		db	069,146,146,079,079,079,085,085
		db	152,079,085,155,156,157,158,159
		db	065,073,079,085,165,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
page
;
;	Portuguese (Po) case mapping
;
po_ucase	label	word
		ctable	<,'UCASE  ',128>
		db	128,154,144,143,142,145,134,128
		db	137,137,146,139,140,152,142,143
		db	144,145,146,140,153,169,150,157
		db	152,153,154,155,156,157,158,159
		db	134,139,159,150,165,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
page
;
;	Is case mapping
;
is_ucase	label	word
		ctable	<,'UCASE  ',128>
		db	128,129,130,131,132,133,134,135
		db	136,137,138,139,140,141,142,143
		db	144,145,146,147,148,149,150,151
		db	152,153,154,155,156,157,158,159
		db	065,073,079,085,165,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
page
;
us850_ucase	label	word
;
afe850_ucase	label	word
;
uk850_ucase	label	word
;
fr850_ucase	label	word
;
it850_ucase	label	word
;
is850_ucase	label	word
;
as850_ucase	label	word
;
po850_ucase	label	word
;
ca850_ucase	label	word
		ctable	<,'UCASE  ',128>
;**CNS replacement
	       db      067,085,069,065,065,065,065,067
	       db      069,069,069,073,073,073,065,065
	       db      069,146,146,079,079,079,085,085
	       db      089,079,085,079,156,079,158,159
	       db      065,073,079,085,165,165,166,167
	       db      168,169,170,171,172,173,174,175
	       db      176,177,178,179,180,065,065,065
	       db      184,185,186,187,188,189,190,191
	       db      192,193,194,195,196,197,065,065
	       db      200,201,202,203,204,205,206,207
	       db      209,209,069,069,069,073,073,073
	       db      073,217,218,219,220,221,073,223
	       db      079,225,079,079,079,079,230,232
	       db      232,085,085,085,089,089,238,239
	       db      240,241,242,243,244,245,246,247
	       db      248,249,250,251,252,253,254,255
;
;***CNS PTM2390 table omitted
;		 db	 128,154,069,065,142,065,143,128    ;AN000;
;		 db	 069,069,069,073,073,073,142,143    ;AN000;
;		 db	 144,146,146,079,153,079,085,085    ;AN000;
;		 db	 089,153,154,155,156,157,158,159    ;AN000;
;		 db	 065,073,079,085,165,165,166,167    ;AN000;
;		 db	 168,169,170,171,172,173,174,175    ;AN000;
;		 db	 176,177,178,179,180,181,182,183    ;AN000;
;		 db	 184,185,186,187,188,189,190,191    ;AN000;
;		 db	 192,193,194,195,196,197,198,199    ;AN000;
;		 db	 200,201,202,203,204,205,206,207    ;AN000;
;		 db	 208,209,210,211,212,213,214,215    ;AN000;
;		 db	 216,217,218,219,220,221,222,223    ;AN000;
;		 db	 224,225,226,227,228,229,230,231    ;AN000;
;		 db	 232,233,234,235,236,237,238,239    ;AN000;
;		 db	 240,241,242,243,244,245,246,247    ;AN000;
;		 db	 248,249,250,251,252,253,254,255    ;AN000;
;***CNS PTM2390 table omitted
;
;;la850_ucase	  label   word	- LA wants same as spain
;
;; sp850_ucase	   label   word - changed for 4.0
;		ctable	<,'UCASE  ',128>
;		db	128,085,069,065,065,065,065,128
;		db	069,069,069,073,073,073,065,065
;		db	069,146,146,079,079,079,085,085
;		db	089,079,085,079,156,079,158,159
;		db	065,073,079,085,165,165,166,167
;		db	168,169,170,171,172,173,174,175
;		db	176,177,178,179,180,065,065,065
;		db	184,185,186,187,188,189,190,191
;		db	192,193,194,195,196,197,065,065
;		db	200,201,202,203,204,205,206,207
;		db	209,209,069,069,069,073,073,073
;		db	073,217,218,219,220,221,073,223
;		db	079,225,079,079,079,079,230,232
;		db	232,085,085,085,089,089,238,239
;		db	240,241,242,243,244,245,246,247
;		db	248,249,250,251,252,253,254,255
;
gr850_ucase	   label   word
		ctable	<,'UCASE  ',128>
		db	067,154,069,065,142,065,065,067
		db	069,069,069,073,073,073,142,065
		db	069,146,146,079,153,079,085,085
		db	089,153,154,079,156,079,158,159
		db	065,073,079,085,165,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,065,065,065
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,065,065
		db	200,201,202,203,204,205,206,207
		db	209,209,069,069,069,073,073,073
		db	073,217,218,219,220,221,073,223
		db	079,225,079,079,079,079,230,232
		db	232,085,085,085,089,089,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
page
;
;	LA case mapping for 850, new for DOS 4.0 to be same as Spain
;
la850_ucase	label	word
;
;	Spain (sp) case mapping for 850, new for DOS 4.0
;
sp850_ucase	label	word
;
;	Belgium (Be) case mapping for 850
;
be850_ucase	   label   word
;
;	Finland (Fi) case mapping for 850
;
fi850_ucase	   label   word
;
;	Sweden (Sv) case mapping for 850
;
sv850_ucase	   label   word
;
;	Denmark (Da) case mapping for 850
;
dk850_ucase	   label   word
;
;	Norway (No) case mapping for 850
;
no850_ucase	   label   word

		ctable	<,'UCASE  ',128>
		db	128,154,144,182,142,183,143,128
		db	210,211,212,216,215,222,142,143
		db	144,146,146,226,153,227,234,235
		db	089,153,154,157,156,157,158,159
		db	181,214,224,233,165,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,199,199
		db	200,201,202,203,204,205,206,207
		db	209,209,210,211,212,073,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,229,229,230,232
		db	232,233,234,235,237,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
page
;	Switzerland (Sw) Case mapping (850)
;
sw850_ucase	  label   word
		ctable	<,'UCASE  ',128>
		db	128,154,144,182,142,183,143,128
		db	210,211,212,216,215,222,142,143
		db	144,146,146,226,153,227,234,235
		db	089,153,154,157,156,157,158,159
		db	181,214,224,233,165,165,166,167 ;DCR059 CNS ** ;AN000;
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,199,199
		db	200,201,202,203,204,205,206,207
		db	209,209,210,211,212,073,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,229,229,230,232
		db	232,233,234,235,237,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
;
;	Netherlands (Nl) case mapping 850
;
nl850_ucase	   label   word
		ctable	<,'UCASE  ',128>
		db	128,085,069,065,065,065,143,128
		db	069,069,069,073,073,073,065,143
		db	069,146,146,079,079,079,085,085
		db	152,079,085,079,156,079,158,159
		db	065,073,079,085,165,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,065,065,065
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,065,065
		db	200,201,202,203,204,205,206,207
		db	209,209,069,069,069,073,073,073
		db	073,217,218,219,220,221,073,223
		db	079,225,079,079,079,079,230,232
		db	232,085,085,085,089,089,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
page
;
;	Japan case mapping 932
;
jp932_ucase	   label   word
;
ko934_ucase	   label   word
;
pr936_ucase	   label   word
;
ta938_ucase	   label   word
		ctable	<,'UCASE  ',128>
		db	128,129,130,131,132,133,134,135
		db	136,137,138,139,140,141,142,143
		db	144,145,146,147,148,149,150,151
		db	152,153,154,155,156,157,158,159
		db	160,161,162,163,164,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
page
; ----------------------------------------------------------------------------

;	cs3 -- begin additions for new languages

page
;
;       Iceland (IC) case mapping for 861
;
ic_ucase        label   word
		ctable  <,'UCASE  ',128>
		db      128,154,144,065,142,065,143,128
		db      069,069,069,139,139,141,142,143
		db      144,146,146,079,153,141,085,151
		db      151,153,154,157,156,157,158,159
		db      164,165,166,167,164,165,166,167
		db      168,169,170,171,172,173,174,175
		db      176,177,178,179,180,181,182,183
		db      184,185,186,187,188,189,190,191
		db      192,193,194,195,196,197,198,199
		db      200,201,202,203,204,205,206,207
		db      208,209,210,211,212,213,214,215
		db      216,217,218,219,220,221,222,223
		db      224,225,226,227,228,229,230,231
		db      232,233,234,235,236,237,238,239
		db      240,241,242,243,244,245,246,247
		db      248,249,250,251,252,253,254,255
		page
;
;
tr850_ucase        label   word
ic850_ucase        label   word
yu850_ucase        label   word
cs850_ucase        label   word
sl850_ucase        label   word
hu850_ucase        label   word
pl850_ucase        label   word
		ctable  <,'UCASE  ',128>
		db      128,154,144,182,142,183,143,128
		db      210,211,212,216,215,222,142,143
		db      144,146,146,226,153,227,234,235
		db      152,153,154,157,156,157,158,159
		db      181,214,224,233,165,165,166,167
		db      168,169,170,171,172,173,174,175
		db      176,177,178,179,180,181,182,183
		db      184,185,186,187,188,189,190,191
		db      192,193,194,195,196,197,199,199
		db      200,201,202,203,204,205,206,207
		db      209,209,210,211,212,213,214,215	
		db      216,217,218,219,220,221,222,223
		db      224,225,226,227,229,229,230,232
		db      232,233,234,235,237,237,238,239
		db      240,241,242,243,244,245,246,247
		db      248,249,250,251,252,253,254,255
;
;tr850_ucase     label   word
		ctable  <,'UCASE  ',128>
;
		db      128,154,144,182,142,183,065,128
		db      210,211,212,216,215,073,142,143
		db      144,146,146,226,153,227,234,235
		db      152,153,154,155,156,157,158,158
		db      181,214,224,233,165,165,166,166
		db      168,169,170,171,172,173,174,175
		db      176,177,178,179,180,181,182,183
		db      184,185,186,187,188,189,190,191
		db      192,193,194,195,196,197,198,199
		db      200,201,202,203,204,205,206,207
		db      208,209,210,211,212,213,214,215
		db      216,217,218,219,220,221,222,223
		db      224,225,226,227,228,229,230,231
		db      232,233,234,235,236,237,238,239
		db      240,241,242,243,244,245,246,247
		db      248,249,250,251,252,253,254,255
	page
;
yu852_ucase     label   word
cs852_ucase     label   word
sl852_ucase     label   word
hu852_ucase     label   word
pl852_ucase     label   word
;
;
	ctable  <,'UCASE  ',128>
;
    db      128,154,144,182,142,222,143,128
    db      157,211,138,138,215,141,142,143
    db      144,145,145,226,153,149,149,151
    db      151,153,154,155,155,157,158,172
    db      181,214,224,233,164,164,166,166
    db      168,168,170,141,172,184,174,175
    db      176,177,178,179,180,181,182,183
    db      184,185,186,187,188,189,189,191
    db      192,193,194,195,196,197,198,198
    db      200,201,202,203,204,205,206,207
    db      209,209,210,211,210,213,214,215
    db      183,217,218,219,220,221,222,223
    db      224,225,226,227,227,213,230,230
    db      232,233,232,235,237,237,221,239
    db      240,241,242,243,244,245,246,247
    db      248,249,250,235,252,252,254,255


		page

;
tr857_ucase     label   word
		ctable  <,'UCASE  ',128>
		db      128,154,144,182,142,183,143,128
		db      210,211,212,216,215,073,142,143
		db      144,146,146,226,153,227,234,235
		db      152,153,154,157,156,157,158,158
		db      181,214,224,233,165,165,166,166
		db      168,169,170,171,172,173,174,175
		db      176,177,178,179,180,181,182,183
		db      184,185,186,187,188,189,190,191
		db      192,193,194,195,196,197,199,199
		db      200,201,202,203,204,205,206,207
		db      208,209,210,211,212,073,214,215
		db      216,217,218,219,220,221,222,223
		db      224,225,226,227,229,229,230,231
		db      232,233,234,235,222,237,238,239
		db      240,241,242,243,244,245,246,247
		db      248,249,250,251,252,253,254,255
page
;
br850_ucase    label word
;
	       ctable      <,'UCASE  ',128>
		db	128,154,144,182,142,183,143,128
		db	210,211,212,216,215,222,142,143
		db	144,146,146,226,153,227,234,235
		db	152,153,154,157,156,157,158,159
		db	181,214,224,233,165,165,166,167
		db      168,169,170,171,172,173,174,175
		db      176,177,178,179,180,181,182,183
		db      184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,199,199
		db      200,201,202,203,204,205,206,207
		db	209,209,210,211,212,213,214,215
		db      216,217,218,219,220,221,222,223
		db	224,225,226,227,229,229,230,232
		db	232,233,234,235,237,237,238,239
		db      240,241,242,243,244,245,246,247
		db      248,249,250,251,252,253,254,255
page

;	cs3 -- end additions

;
;	World Trade Mono Case Filename Character Tables
;
;		Currently all countries have same (4/14/86)
;
;
; ----------------------------------------------------------------------------
uk_ucfile	label	word
fr_ucfile	label	word
gr_ucfile	label	word
sp_ucfile	label	word
it_ucfile	label	word
sv_ucfile	label	word
dk_ucfile	label	word
sw_ucfile	label	word
no_ucfile	label	word
nl_ucfile	label	word
be_ucfile	label	word
fi_ucfile	label	word
is_ucfile	label	word
ca_ucfile	label	word
as_ucfile	label	word
po_ucfile	label	word
us_ucfile	label	word
la_ucfile	label	word
afe_ucfile	label	word

;	cs3 -- added countries

br_ucfile      label word
tr_ucfile      label word
ic_ucfile      label word

;	cs3 -- end added countries

		ctable	<,'FUCASE ',128>
		db	128,129,130,131,132,133,134,135
		db	136,137,138,139,140,141,142,143
		db	144,145,146,147,148,149,150,151
		db	152,153,154,155,156,157,158,159
		db	160,161,162,163,164,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,198,199
		db	200,201,202,203,204,205,206,207
		db	208,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,228,229,230,231
		db	232,233,234,235,236,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
	page

;	cs3 -- new ucfile tables for some new countries

br850_ucfile	label	word
ic850_ucfile	label	word
tr850_ucfile	label	word
yu850_ucfile	label	word
cs850_ucfile	label	word
pl850_ucfile	label	word
hu850_ucfile	label	word
sl850_ucfile    label   word

		ctable	<,'FUCASE ',128>
		db	128,154,144,182,142,183,143,128
		db	210,211,212,216,215,222,142,143
		db	144,146,146,226,153,227,234,235
		db	152,153,154,157,156,157,158,159
		db	181,214,224,233,165,165,166,167
		db	168,169,170,171,172,173,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,190,191
		db	192,193,194,195,196,197,199,199
		db	200,201,202,203,204,205,206,207
		db	209,209,210,211,212,213,214,215
		db	216,217,218,219,220,221,222,223
		db	224,225,226,227,229,229,230,232
		db	232,233,234,235,237,237,238,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,251,252,253,254,255
page
yu_ucfile	label	word
cs_ucfile       label   word
sl_ucfile       label   word
pl_ucfile       label   word
hu_ucfile       label   word

		ctable  <,'FUCASE ',128>
		db	128,154,144,182,142,222,143,128
		db	157,211,138,138,215,141,142,143
		db	144,145,145,226,153,149,149,151
		db	151,153,154,155,155,157,158,172
		db	181,214,224,233,164,164,166,166
		db	168,168,170,141,172,184,174,175
		db	176,177,178,179,180,181,182,183
		db	184,185,186,187,188,189,189,191
		db	192,193,194,195,196,197,198,198
		db	200,201,202,203,204,205,206,207
		db	209,209,210,211,210,213,214,215
		db	183,217,218,219,220,221,222,223
		db	224,225,226,227,227,213,230,230
		db	232,233,232,235,237,237,221,239
		db	240,241,242,243,244,245,246,247
		db	248,249,250,235,252,252,254,255
	page
;

;	cs3 -- end additions

; ----------------------------------------------------------------------------
;
;	World Trade Valid Filename Character Tables
;
;		Currently all countries have same (4/14/86)
;
;
; ----------------------------------------------------------------------------
uk_flist	label	word
fr_flist	label	word
gr_flist	label	word
sp_flist	label	word
it_flist	label	word
sv_flist	label	word
dk_flist	label	word
sw_flist	label	word
no_flist	label	word
nl_flist	label	word
be_flist	label	word
fi_flist	label	word
is_flist	label	word
ca_flist	label	word
as_flist	label	word
po_flist	label	word
us_flist	label	word
la_flist	label	word
afe_flist	label	word
jp437_flist	label	word
jp932_flist	label	word
ko437_flist	label	word
ko934_flist	label	word
pr437_flist	label	word
pr936_flist	label	word
ta437_flist	label	word
ta938_flist	label	word

;	cs3 -- begin additions

br_flist	 label word
ic_flist        label   word
tr_flist        label   word
yu_flist        label   word
cs_flist        label   word
sl_flist        label   word
pl_flist        label   word
hu_flist        label   word

;	cs3 -- end additions

		ctable	<,'FCHAR  ',fclend-fclbegin>
fclbegin	label	word
		db	1,0,255 			; include all
		db	0,0,20h 			; exclude 0 - 20h
		db	2,14,'."/\[]:|<>+=;,'           ; exclude 14 special chars
fclend		label	word
	page
; ----------------------------------------------------------------------------
;
;	World Trade DBCS Tables
;
;	    Currently all countries have same (4/14/86)
;
;
; ----------------------------------------------------------------------------
;					;***CNS
uk_dbcs 	label	word		;AN000;
fr_dbcs 	label	word		;AN000;
gr_dbcs 	label	word		;AN000;
sp_dbcs 	label	word		;AN000;
it_dbcs 	label	word		;AN000;
sv_dbcs 	label	word		;AN000;
dk_dbcs 	label	word		;AN000;
sw_dbcs 	label	word		;AN000;
no_dbcs 	label	word		;AN000;
nl_dbcs 	label	word		;AN000;
be_dbcs 	label	word		;AN000;
fi_dbcs 	label	word		;AN000;
is_dbcs 	label	word		;AN000;
ca_dbcs 	label	word		;AN000;
as_dbcs 	label	word		;AN000;
po_dbcs 	label	word		;AN000;
us_dbcs 	label	word		;AN000;
la_dbcs 	label	word		;AN000;
afe_dbcs	label	word		;AN000;
jp437_dbcs	label	word		;AN000;
ko437_dbcs	label	word		;AN000;
pr437_dbcs	label	word		;AN000;
ta437_dbcs	label	word		;AN000;

;	cs3 -- begin additions for new languages

br_dbcs		label	word
ic_dbcs         label   word
tr_dbcs         label   word
yu_dbcs         label   word
cs_dbcs         label   word
sl_dbcs         label   word
pl_dbcs         label   word
hu_dbcs         label   word

;	cs3 -- end additions

		ctable	<,'DBCS   ',dbcsterm-dbcsbegin>         ;AN000;
dbcsbegin	label	word					;AN000;
dbcsterm	db	0,0					;AN000;
dbcsend 	label	word					;AN000;
;
;    Japan DBCS lead byte table
;
jp932_dbcs	label	word					;AN000;
		ctable	<,'DBCS   ',db932end-db932bgn>          ;AN000;
db932bgn	label	word					;AN000;
		db	081h,09Fh				;AN000;
		db	0E0h,0FCh				;AN000;
db932term	db	000h,000h				;AN000;
db932end	equ	$					;AN000;
;
;    Korea DBCS lead byte table
;
ko934_dbcs	label	word					;AN000;
		ctable	<,'DBCS   ',db934end-db934bgn>          ;AN000;
db934bgn	label	word					;AN000;
		db	081h,0BFh				;AN000;
db934term	db	000h,000h				;AN000;
db934end	equ	$					;AN000;
;
;    PRC and Taiwan DBCS lead byte table
;
pr936_dbcs	label	word					;AN000;
;cs3 ta938_dbcs	label	word					;AN000;
		ctable	<,'DBCS   ',db936end-db936bgn>          ;AN000;
db936bgn	label	word					;AN000;
		db	081h,0FCh				;AN000;
db936term	db	000h,000h				;AN000;
db936end	equ	$					;AN000;

;	cs3 -- separate entry for taiwan 938 dbcs

ta938_dbcs	label	word					;AN000;
		ctable	<,'DBCS   ',ta938end-ta938bgn>          ;AN000;
ta938bgn	label	word					;AN000;
		db	081h,0FDh				;AN000;
ta938term	db	000h,000h				;AN000;
ta938end	equ	$					;AN000;

;	cs3 -- end added section

	include	copyrigh.inc

; ---------------------------------------------------------------
;
;		END OF CDI SEGMENT
;
; ---------------------------------------------------------------


dseg	ends
	end
