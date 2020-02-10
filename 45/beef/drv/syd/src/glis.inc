;*
;*	CW : Character Windows Drivers
;*
;*	glis : Global Info Segment Contents
;*****************************************************************************

;*	* Global Info Segment
GLIS	STRUC		;* from DosGetInfoSeg
	timeGlis	DD	?
	msGlis		DD	?
	hourGlis	DB	?
	minuteGlis	DB	?
	secGlis		DB	?
	hundredthGlis	DB	?
	dminGmtGlis	DW	?
	intervalGlis	DW	?
;*	* more here
GLIS	ENDS

;*****************************************************************************
