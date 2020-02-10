;string.inc - far string constants and definitions


bd	STRUC
BD_cbLogical	dw	?	;logical size of content part of heap entry
BD_pb		dw	?	;points to 1st byte of heap entry
BD_cbPhysical	dw	?	;actual size of heap entry, including padding
bd	ENDS

parmSD	macro	ParmName
;;parm is near pointer to SD structure
parmW	ParmName
	endm

GetpSD	macro	reg,ParmName
;;set reg = pSD, depending on whether parm is SD or pSD
	mov	reg,ParmName	;;reg = parm (parm is pointer to SD)
	endm

ArgLast	equ	[bp+6]		;Location of last argument

NumTemps = 20			;number of string temps
LenTemp = 6			;descriptor length is six bytes
