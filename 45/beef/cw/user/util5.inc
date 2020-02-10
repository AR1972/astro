;*
;*	COW : Character Oriented Windows

FDE_OS2	STRUC
	wDateCreateFdeOS2	dw	?
	wTimeCreateFdeOS2	dw	?
	wDateAccessFdeOS2	dw	?
	wTimeAccessFdeOS2	dw	?
	wDateWriteFdeOS2	dw	?
	wTimeWriteFdeOS2	dw	?
	cbFileFdeOS2		dd	?
	OFF_cbFileAllocFdeOS2	dw	?
	SEG_cbFileAllocFdeOS2	dw	?
	attrFileFdeOS2		dw	?
	cchNameFdeOS2		db	?
	szNameFdeOS2		db	13 dup (?)
FDE_OS2	ENDS

FDE	STRUC
	dtaFde		DB	21 DUP (0)	;* 21 bytes filler
	atrFde		DB	0
	wTimeWriteFde	DW	0
	wDateWriteFde	DW	0
	cbFileFde	DD	0
	szNameFde	DB	13 DUP (0)	;* file name
FDE	ENDS


