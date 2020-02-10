	TITLE	binsav.asm - binary SAVE/LOAD for QBI
;*** 
;binsav.asm - binary SAVE/LOAD for QBI
;
;	Copyright (C) 1986-1989, Microsoft Corporation
;
;Purpose:
;   -  Performing Binary Load/Save of modules
;
;
;*******************************************************************************

	.xlist

	include version.inc
	BINSAV_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	conint
	includeOnce	heap
	includeOnce	names		
	includeOnce	optables	
	includeOnce	qblist
	includeOnce	qbimsgs
	includeOnce	rtinterp
	includeOnce	rtps
	includeOnce	rttemp
	includeOnce	scanner
	includeOnce	txtint
	includeOnce	txtmgr
	includeOnce	sb		
	includeOnce	ui
	includeOnce	util

	.list

	assumes CS,CP
	assumes DS,DATA
	assumes SS,DATA
	assumes ES,NOTHING


	end
