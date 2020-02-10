	page	,132
	TITLE	RtTemp - Temporary Runtime Routines


COMMENT \

--------- --- ---- -- ---------
COPYRIGHT (C) 1982 BY MICROSOFT
--------- --- ---- -- ---------

RtTemp is a file of routines that will be replaced with the common
runtime.

------------------
Revision History:
------------------


	\

	page

	.xlist
	RTTEMP_ASM = 1
	include 	version.inc
	IncludeOnce	executor
	IncludeOnce	msdos
	IncludeOnce	rttemp
	.list

assumes DS,DATA
assumes ES,DATA
assumes SS,DATA

sBegin	DATA
sEnd	DATA


sBegin	CODE
assumes CS,CODE

subttl	Internal Debug Function Timer
page

sEnd	CODE

	end
