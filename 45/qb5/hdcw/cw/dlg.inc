;WARNING: This file contains some of the same constants as DLG.H
;WARNING: When making a change to one, make it to both!

isNothing	EQU	0ffffh

tmcMake 	EQU	(tmcUserMin+0)
tmcMakeExit	EQU	(tmcUserMin+1)
tmcSzFileName	EQU	(tmcUserMin+2)
tmcFileType	EQU	(tmcUserMin+3)
			; NOTE: next one must start at +6
