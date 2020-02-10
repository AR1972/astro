;
;	COW : Character Oriented Windows
;
;	util.inc : utilities, cow private interface
;
;

;*	* Find First/Next info (must jive with dirlist.h)

FDE	STRUC
	dtaFde		DB	21 DUP (?)	;* 21 bytes filler
	atrFde		DB	?
	wTimeFde	DW	?
	wDateFde	DW	?
	cbFileFde	DD	?
	szNameFde	DB	13 DUP (?)	;* file name
FDE	ENDS

; BLTS : both : source, destination, count_of_bytes
externFPrivate bltbyte
externFPrivate bltbytex

externFPrivate stringcpy
externFPrivate fstrlen
externFPrivate fstrcmp
externFPrivate fstrcpy

; NEAR helper procedures

; CORE segment
externNP DisableInterrupts
externNP EnableInterrupts

ifdef DIRLIST  ; in LISTBOX segment
externNP FFindNext
externNP FFindFirst
externNP FindClose
externNP AtrOfPath
externNP GetCurDrive
externNP SetCurDrive
externNP GetCurDir
externNP SetCurDir

ifndef DOS5    ; fast fill for DOS 3
externNP PrepareWild
externNP FMatchWild
endif ; !DOS5

endif ; DIRLIST

ifdef SCROLL
externNP TranslatePosition
endif

ifdef SDM
externNP FillBuf
endif
