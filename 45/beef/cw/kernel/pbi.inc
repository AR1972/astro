;*
;*	COW : Character Oriented Windows
;*
;*	pbi.inc : PBI structure

PBI	STRUC
	psEnviron	DW	?
	offCmdLine	DW	?
	psCmdLine	DW	?
	offFcb1		DW	?
	psFcb1		DW	?
	offFcb2		DW	?
	psFcb2		DW	?
PBI	ENDS

