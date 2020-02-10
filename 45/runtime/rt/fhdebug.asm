	TITLE	FHDEBUG - FAR HEAP DEBUG CODE

;***
;FHDEBUG.ASM - Far Heap Debugging Code for the BASIC 3 Common Runtime
;
;	Copyright (C) Microsoft Corp. 1986.
;
;****

	SUBTTL	INCLUDES AND DEFINITIONS FOLLOWS
	PAGE	56,132

INCLUDE switch.inc
INCLUDE rmacros.inc

	USESEG	<FH_TEXT>
	USESEG	<_BSS>
	USESEG	<_DATA>

INCLUDE seg.inc
INCLUDE idmac.inc
INCLUDE array.inc


	END
