	page	,132
	title	Find ROM Resident Code

comment %

FindROMRes locates COMMAND's ROM-resident code during initialization.

The supplied code handles the cases

  o  complete COMMAND image below 1 MB

  o  COMMAND resident code below 1 MB, rest of image somewhere else

The cases are identified by the symbols defined in romimg\command.loc.

%



;***	CONSTANTS

	include	command.loc		; location of ROM COMMAND image




;***	EXTERNALS

	.xlist
	include	comseg.asm		; COMMAND segment definitions
	.list

CODERES	segment

	extrn	StartCode:byte		; beginning of resident code

CODERES	ends




;***	CODE

INIT	segment public para	; initialization code

	public	FindROMRes

	assume	cs:RESGROUP




;***	FindROMRes - locate resident code
;
;	ENTRY	nothing
;
;	EXIT	ES:DI = ptr to resident code
;
;	USES	AX

FindROMRes	proc

ifdef COMMAND_SEG

;	ROM COMMAND image is below 1M, locate resident within image

	mov	ax,COMMAND_SEG
	mov	es,ax
	mov	di,offset RESGROUP:StartCode-100h
	ret

else

;	ROM COMMAND image isn't directly addressable.
;	Resident COMMAND code must be in its own granule.

	include	rescom.loc

	mov	ax,RESCOM_SEG
	mov	es,ax
	xor	di,di
	ret

endif

FindROMRes	endp


INIT	ends

	end
