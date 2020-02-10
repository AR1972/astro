	TITLE	PRNVALFP - Print floaing point values
	page	56,132
;***
; PRNVALFP - Print values
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;	This module contains B$P<term><type> for floating point
;
;	The PRINT, WRITE, and LPRINT statements have a large number of
;	runtime calls that can be generated, based on the list of
;	expressions given. There is a unique preamble call for each such
;	statement (except PRINT), to correctly set up flags, vectors, etc.
;	The actual printing call itself is one of 15 possibilities, based
;	on the argument type, and one of three possible ways of terminating
;	the print: ',', ';', or EOL. Each of these 15 calls is in the
;	form B$P<term><type> where <term> specifies the print termination
;	method, and <type> specifies the type of the argument.
;
;	<term>:
;		C == ',' (i.e., something like 'PRINT X,' was specified)
;		S == ';'
;		E == EOL (neither ',' nor ';' was specified)
;
;	<type>:
;		R4 == single precision real
;		R8 == double precision real
;		CY == currency
;
;	Thus, for example, a call to B$PER4 would be used to print a s.p.
;	value and terminate with a EOL.
;
;	The list of expressions can be of any size; for each additional
;	expression, another one of the fifteen possible runtime calls is
;	generated. After the last such expression, a call to B$PEOS is
;	generated (if not terminated with EOL). If no expression is
;	specified, a NULL string is printed, so 'PRINT' would generate a
;	call to B$PESD with a null string as the parameter (print a string,
;	terminate with EOL).
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	useSeg	DK_TEXT

	INCLUDE seg.inc
	INCLUDE rtps.inc

	SUBTTL	local constant definitions
	page


	COMA	EQU	0	;comma
	SEMI	EQU	1	;semicolumn
	EOL	EQU	2	;forced EOL


	externFP B$PRINT	;Common Print routine


	assumes CS,DK_TEXT
sBegin	DK_TEXT

	externNP B$FloatCONASC	;Pull in floating point conversion routines


	SUBTTL	print interfaces -- B$P<term><type>(<param>)
	page
;***
; B$P<term><type>(<param>) -- print an item for :
;	PRINT, PRINT #, PRINT USING, PRINT # USING,
;	WRITE, WRITE #,
;	LPRINT, LPRINT USING.
;
;Purpose:
;	These are interfaces to the compiler.  Each entry point sets up
;	(1) types of value & terminator, and
;	(2) a pointer to that item,
;	 and then fall through PRINT, which performs actual printing job.
;
;	<term>, <type> & <param> may vary as follows:
;	<term>:
;		C:	Comma used as terminator
;		S:	Semi used as terminator
;		E:	End of statement used as terminator
;	<type>:
;		R4:	Single precision real (sp)
;		R8:	Double precision real (dp)
;		CY:	Currency
;	<param>:
;		A parameter of type <type> to be printed.
;
;
;Entry:
;	Parameter was pushed in stack.
;	<type>	Val = Number if <type> is numerical type
;
;Exit:
;	through B$PRINT
;Uses:
;	none
;Exceptions:
;
;*******************************************************************************

;========================
;	Print R4,	|
;========================

cProc	B$PCR4,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,COMA SHL 8 + VT_R4 ;AX=[terminator type | value type]
	JMP	B$PRINT 	;print the item
cEnd	nogen			;no code generated

;========================
;	Print R4;	|
;========================

cProc	B$PSR4,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,SEMI SHL 8 + VT_R4 ;AX=[terminator type | value type]
	JMP	B$PRINT 	;print the item
cEnd	nogen			;no code generated

;========================
;	Print R4	|
;========================

cProc	B$PER4,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,EOL SHL 8 + VT_R4 ;AX=[terminator type | value type]
	JMP	B$PRINT 	;print the item
cEnd	nogen			;no code generated

;========================
;	Print R8,	|
;========================

cProc	B$PCR8,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,COMA SHL 8 + VT_R8 ;AX=[terminator type | value type]
	JMP	B$PRINT 	;print the item
cEnd	nogen			;no code generated

;========================
;	Print R8;	|
;========================

cProc	B$PSR8,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,SEMI SHL 8 + VT_R8 ;AX=[terminator type | value type]
	JMP	B$PRINT 	;print the item
cEnd	nogen			;no code generated

;========================
;	Print R8	|
;========================

cProc	B$PER8,<PUBLIC,FAR>

cBegin	nogen			;no stack frame generated
	MOV	AX,EOL SHL 8 + VT_R8 ;AX=[terminator type | value type]
	JMP	B$PRINT 	;print the item
cEnd	nogen			;no code generated



sEnd	DK_TEXT

	END
