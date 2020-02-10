	TITLE BdDebug.asm - Buffer Descriptor Debugging Routines

COMMENT	\

--------- --- ---- -- ---------- ----
COPYRIGHT (C) 1985 BY MICROSOFT, INC.
--------- --- ---- -- ---------- ----

\

;============================================================================
; Module: BdDebug.asm - Buffer Descriptor Debugging Routines
;
; System: Quick BASIC Interpreter
;
;============================================================================

	.xlist

	include version.inc
	BDDEBUG_ASM = ON
	includeOnce	architec	
	includeOnce	heap
	includeOnce	util
	includeOnce	rttemp
	includeOnce	context

	.list

;	.sall


	end
