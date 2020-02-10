	page	,132
	title	cmiscdat - miscellaneous C run-time data
;***
;cmiscdat.asm - miscellaneous C run-time data
;
;	Copyright (c) 1987-1988, Microsoft Corporation, All Rights Reserved
;
;Purpose:
;	Includes floating point conversion table (for C float output),
;	a data structure used by signal/exec to tell if a SIGINT handler 
;	has been installed, and the public variables _asizeC and _asizeD
;	for conveying memory-model information to the memory-model-independent
;	GRAPHICS.LIB.
;
;	When floating point i/o conversions are done, but no floating-point
;	variables or expressions are used in the C program, we use the
;	_cfltcvt_tab[] to map these cases to the _fptrap entry point,
;	which prints "floating point not loaded" and dies.
;
;	This table is initialized to five copies of _fptrap by default.
;	If floating-point is linked in (_fltused), these table entries
;	are reset (see input.c, output.c, fltused.asm, and fltuseda.asm).
;
;*******************************************************************************


include	version.inc
.xlist
include	cmacros.inc
.list

externNP _fptrap			;[2] not needed for QC version

sBegin	data

	assumes	ds,data
	assumes	cs,code

;
;	... table of (model-dependent) code pointers ...
;
;	Five entries, all point to _fptrap by default,
;	but are changed to point to the appropriate
;	routine if the _fltused initializer (_cfltcvt_init)
;	is linked in.
;
;	this table is used or modified in the following
;	source files:
;
;		\clib\stdio\input.c
;		\clib\stdio\output.c
;		\math\c\fltused.asm
;		\math\c\fltuseda.asm
;
;	if the _fltused modules are linked in, then the
;	_cfltcvt_init initializer sets the 5 entries of
;	_cfltcvt_tab to:
;
;		_cfltcvt
;		_cropzeros
;		_fassign
;		_forcdecpt
;		_positive
;


labelCP	<PUBLIC, _cfltcvt_tab>
	DD	5 dup (_fptrap)

;
;	miscellaneous data (used by signal and exec*)
;
;	(just by the DOS 3 versions, though)
;

globalW	_sigintseg,0 		;SIGINT default signal routine (segment)
globalW	_sigintoff,0 		;SIGINT default signal routine (offset)

;
;	memory model information for use in GRAPHICS.LIB
;	
;	_asizeC is zero    for Small   & Compact models, 
;	           nonzero for Medium  & Large   models;
;	_asizeD is zero    for Small   & Medium  models, 
;	           nonzero for Compact & Large   models.
;
;

globalB _asizeC,sizeC
globalB _asizeD,sizeD


sEnd	data

	end
