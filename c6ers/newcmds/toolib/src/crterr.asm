;	Static Name Aliases
;
	TITLE   crterr.asm
	NAME    crterr

	.8087
CRTERR_TEXT	SEGMENT  WORD PUBLIC 'CODE'
CRTERR_TEXT	ENDS
_DATA	SEGMENT  WORD PUBLIC 'DATA'
_DATA	ENDS
CONST	SEGMENT  WORD PUBLIC 'CONST'
CONST	ENDS
_BSS	SEGMENT  WORD PUBLIC 'BSS'
_BSS	ENDS
DGROUP	GROUP	CONST, _BSS, _DATA
	ASSUME  CS: CRTERR_TEXT, DS: DGROUP, SS: DGROUP
EXTRN	__acrtused:ABS
EXTRN	__dosret:FAR
CRTERR_TEXT      SEGMENT
	ASSUME	CS: CRTERR_TEXT

;	Map an OS/2 error to an ANSI errno value
;	Use the undocumented OS/2 C runtime function __dosret
;	to perform the mapping.  __dosret sets the globals
;	_errno and __doserrno.

	PUBLIC	_DosErrToErrno
_DosErrToErrno	PROC FAR
	push	bp
	mov	bp,sp
	mov	ax,WORD PTR [bp+6]
	jmp	FAR PTR __dosret
_DosErrToErrno	ENDP
CRTERR_TEXT	ENDS
END
