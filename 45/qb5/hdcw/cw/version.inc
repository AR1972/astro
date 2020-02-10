
LIBC	macro name
	extrn	_&name:far
	endm

MEM_MGR	macro name
	endm

TEXT_MGR macro name
	endm

APPL	macro name
	endm

TWIN	macro name
	extrn	name:near
	endm
