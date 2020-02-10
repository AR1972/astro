	TITLE	BINXLAT.ASM - Binaray file translation code
;*** 
;BinXlat.ASM - Provides binaray file translation
;
;	Copyright (C) 1988-1989, Microsoft Corporation
;
;Purpose:
;	BinXlat.ASM contains the product to product binary file translation
;	code.  in general, the translation is done by a routine called a
;	"specific translator" which is geared toward the translation of a
;	specific product's (or group of products) into our own.  currently
;	the following specific translators are defined:
;	    Xlat4045	    QB 4.0 -> QB 4.5
;	    XlatGeneral     general translation
;
;Specific Translators:
;	the specific translator is called for each text table in the program.
;	it should translate the text table and then return.
;
;	the following guidelines should be followed when adding specific
;	translators to the system:
;	    - the following registers and memory will be set up upon entry:
;		es:si		points to first opcode in text table
;	    - the offset of the translator data (in DS) set up by InitBinXlat
;	      is in oXlatData.
;	    - if an error which requires the load to be aborted occurs:
;		- return ax == 0
;		- set the word at [oXlatData] to a message number to display
;	    - if a warning error occurs:
;		- return ax != 0
;		- set the appropriate flag in the flag byte at [oXlatData]
;	    - if the translation succeeds:
;		- return ax != 0
;	    - the specific translator does not need to save di and si.
;
;Notes:
;	before calling any of the binary translation routines InitBinXlat
;	should be called.  InitBinXlat sets up internal data, and checks
;	to see if translation is required.
;
;	the main entrypoint in this module is BinaryXlat.  BinaryLoad should
;	call this routine if InitBinXlat says that binary translation is
;	necessary (in other words, the format and revision bytes in the binary
;	files header don't match).
;
;	all of the information required by XlatTxtTbl is placed on the stack
;	since the translator resides in both the SCAN and CP segments.
;
;	the translation is performed with ForEachTxtTblInMrs with the routine
;	XlatTxtTbl as the function to call.  XlatTxtTbl calls the specific xlatr.
;
;*******************************************************************************

	.xlist

	include 	version.inc
	include 	cw/cowdef.inc

	IncludeOnce	architec
	includeOnce	conint
	includeOnce	context
	includeOnce	opcodes
	includeOnce	optables
	includeOnce	pcode
	includeOnce	qbimsgs
	includeOnce	rtps
	includeOnce	txtint
	includeOnce	txtmgr

	includeOnce	ssint

	.list


;WARNING: much of this code ASSUMES that SS and DS are the SAME!  do not change
;WARNING: the assumes at the beginning of this file or BinarySav.ASM without
;WARNING: making appropriate changes to the source.

	assumes DS,DATA
	assumes SS,DATA
	assumes ES,NOTHING


	end
