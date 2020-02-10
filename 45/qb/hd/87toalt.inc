;***
;87toalt.inc - 11-Jul-88 - Math Instruction Macros
;***
	IFNDEF	LI_EXPAND
	.XLIST
	ENDIF
;***
;
;	Copyright <C> 1988, Microsoft Corporation
;
;Purpose:
;	This include file defines macros that redefines the 8087 emulator
;	instructions so that Altmath is used.  These macros are defined
;	whenever this file is included.  For emulator versions then this
;	file is not included.
;
;	The macro names are the actual 8087 instructions and the macros
;	use the exact same parameters as the 8087 instructions.  To use
;	Altmath all that has to be done is do an include of this file
;	in every source file that uses floating point instructions and
;	link it together with 87toalt.asm.  87toalt.asm contains the
;	Altmath interface calls and some routines that are not supplied in
;	altmath.
;
;	Not all 8087 instructions are available.  Basically the following
;	instructions are available:
;		fld
;		fild
;		fst
;		fstp
;		fistp
;		fxch
;		fld1
;		fldz
;		fcompp
;		fstsw
;		fadd
;		fsub
;		fmul
;		fdiv
;		fabs
;		fchs
;		fsqrt
;		fwait
;
;	All the possible forms may not be available for all of these
;	instructions.  Which forms that are available will depend of
;	which interface routines have been included in the 87toalt.asm
;	file.
;
;	If an 8087 instruction has been added to the source that was
;	currently supported and a link error claiming that AltM**** was
;	an unresolved external then the following things will have to
;	have to be done to include that form of the instruction.
;
;	1)	find the EXTRN in this for AltM**** and uncomment it.
;	2)	find the macro-call code for AltM**** in 87toalt.asm
;		and uncomment it.
;
;
;******************************************************************************

m87toalt_INC = -1   ;needed by QBI sources which also include this file.

;******************************************************************************
;
; The macros defined in this file along with the externs done in them should
; not be included by the 87toalt.asm file.
ifndef m87toalt_ASM
;
;******************************************************************************
;
;	Helper macros used only in this file
;
;********************
; Macro MSSame compares the sbs (string being searched) starting at col c
; with slf (string to look for).  TRUE is returend if the slf is present in
; the sbs starting at col c.  FALSE is returned otherwise.  TRUE or FALSE is
; retuened in the symbol same.  This comparison is CASE SENSITIVE.
;
; Uses local symbol prefix MM3.
;
MSSame	MACRO	sbs,slf,c,same
MM3ntmp	INSTR	c,<sbs>,<slf>
same	=	(MM3ntmp EQ c)
	ENDM ;; MSSame
;
;********************
; Macro MgetST takes as input in arg a string of the form "ST(x)" where x
; is from 0..7 and is the 8087 register number.  The macro returns in dest
; the register number from 0..7.  The macro will generate assembly errors if
; the arg is not in the correct form.  If the arg is simple "ST" then dest
; is 0.
;
; Uses local symbols prefix MM1
;
MgetST	MACRO	arg,dest
ifidni	<arg>,<ST>			;; Check for ST
  dest	= 0
else ;; if ST
  MM1narglen SIZESTR <arg>		;; Make sure that it is correct length
  if (MM1narglen NE 5)
    	.err	; Invalid numeric stack register form (len)
	dest = 0
  else ;; if len = 5 
	MSSame	<arg>,<ST(>,1,MM1nust	;; Check for ST(
	MSSame	<arg>,<st(>,1,MM1nlst   ;; st( is also acceptable

    MM1sRegchar SUBSTR	<arg>,4,1	;; get the register number
    MM1nRegNo INSTR	<01234567>,MM1sRegchar

	MSSame	<arg>,<)>,5,MM1nrp	;; Check for )
  
    if ((MM1nust OR MM1nlst) AND MM1nrp AND (MM1nRegNo GT 0))
	dest = MM1nRegNo-1		;; return valid reg number
    else ;; valid register
    	.err	; Invalid numeric stack register form (invalid chars)
	dest = 0
    endif ;; valid register
  endif ;; if len = 5
endif ;; if ST
	ENDM		;; MgetST
;
;********************
; This macro decodes a memory address and finds out what segment it is.  The
; following are examples of valid forms of arg:
;	[BX]
;	DS:[BX]
;	ES:[PFrame]
;	DGROUP:[SI-6]
;	CS:[bx]
; The macro returns	0 for DS
;		 	1 for SS
;			2 for ES
;			3 for CS
;
; The macro assumes that DGROUP is same as DS.
;
; Uses prefix MM4 for local symbols
;
MGetSeg	MACRO	arg,dest
MM4cc	INSTR	<arg>,<:>		;; check for location of the :
if (MM4cc EQ 0)
  dest	=	0			;; Assume DS if no segment override
elseif (MM4cc EQ 3)
  MM4subs	SUBSTR	<arg>,1,2		;; Take of the segment part
  MM4cd	INSTR	<DSdsSSssESesCScs>,MM4subs	;; Find out wich segment
  if ((MM4cd AND 1) EQ 0)		;; then not legal segment
  	.err	; Illegal segment
    dest =	0			;; DS, even if error
  else
    dest =	((MM4cd-1) / 4)		;; get segment number
  endif		;; if MM4cd even
elseif (MM4cc EQ 7)
  	MSSame	<arg>,<DGROUP>,1,MM4isudgroup
  	MSSame	<arg>,<dgroup>,1,MM4isldgroup
  if (MM4isudgroup OR MM4isldgroup)
    dest =	0		;; DGROUP same as DS
  else ;; if DGROUP
    .err	; illegal segment
    dest =	0
  endif ;; if DGROUP
else ;; none of the above
  .err		; illegal segment
  dest	=	0
endif ;; if MM4cc
	ENDM MGetSeg
;
;********************
;
; Get the segment letter from the Segment number.  This letter is intended
; to be used in the concatination of the altmath call name.  The letters are
; the letters used by altmath.
; 	SegNo	Segment	Letter		Altmath example
;	0	DS	f		flds
;	1	SS	s		slds
;	2	ES	e		elds
;	3	CS	e		elds	;; ES must be set to CS
;
GetSegLetter	MACRO	SegNo,SegLetter
SegLetter	SUBSTR	<fsee>,(SegNo+1),1
		ENDM		;; GetSegLetter
;
;********************
;
; Get the Floating point data size letter.  This letter is intended
; to be used in the concatination of the altmath call name.
;	Typeword	Type	TypeLetter	Altmath example
;	dword		single	s		flds
;	qword		double	d		fldd
;
GetTypeRLetter	MACRO	TypeWord,TypeLetter
ifidni	<TypeWord>,<dword>
  TypeLetter	EQU	<s>		;; type is single precision
elseifidni <TypeWord>,<qword>
  TypeLetter	EQU	<d>		;; type is double precision
elseifidni <TypeWord>,<tbyte>
  TypeLetter	EQU	<t>		;; type is 10 byte float
else	;; Not dword or qword
	.err		; Illegal pointer type for floating point instruction
endif	;; <TypeWord>
	ENDM	;; GetTypeRLetter
;
;********************
;
; Get the Integer data size letter.  This letter is intended
; to be used in the concatination of the altmath call name.
;	Typeword	Type	TypeLetter	Altmath example
;	word		I2	w		fldw
;	dword		I4	l		fldl
;
GetTypeILetter	MACRO	TypeWord,TypeLetter
ifidni	<TypeWord>,<word>
  TypeLetter	EQU	<w>		;; type is I2 precision
elseifidni <TypeWord>,<dword>
  TypeLetter	EQU	<l>		;; type is I4 precision
else	;; Not word or dword
	.err		; Illegal pointer type for Integer instruction
endif	;; <TypeWord>
	ENDM	;; GetTypeILetter
;
;********************
;
; Most altmath routines expect their input from [BX].  This macro is used
; to insure that the input will be found at [BX].  This is done by checking
; the argument for [BX], if it not there then expand to a lea bx,...  If [BX]
; is there then expand to nothing
;
; Uses MM5 as prefix to local symbols.
;
MSetupBX MACRO	arg
MM5cc	INSTR	<arg>,<:>
if (MM5cc)
  MM5loc	SUBSTR	<arg>,MM5cc+1	;; get the substring after the colon
else ; colon present
  MM5loc	EQU	arg		;; get the whole arg
endif ; colon present
MM5ubx	INSTR	MM5loc,<[BX]>		;; check for [BX]
MM5lbx	INSTR	MM5loc,<[bx]>		;; check for [bx]
if ((MM5ubx NE 1) AND (MM5lbx NE 1))
	push	bx			;; Save BX only if modified here
	lea	bx,MM5loc		;; if no [BX]
endif	;; if no [BX]
	ENDM	;; MSetupBX
;
;********************
; This routine will only restore BX if saved during MSetupBX
MRestoreBX	MACRO
if ((MM5ubx NE 1) AND (MM5lbx NE 1))	;; These symbols were set in MSetupBX
	pop	bx			;; if BX was saved
endif	;; if no [BX]
	ENDM	;; MRestoreBX
;
;********************
;
; This macro is used to generate the actual altmath call when a memory
; referenace is used.  The following things are done.
;	1)	Determine the data type (single(dword) or double(qword))
;	2)	Pushes BX and sets up the BX register if needed
;	3)	Determin the segemnt used and appropriate setup for segment
;		when needed.
;	4)	create the actual call to the interface routine
;	5)	Any cleanup that is needed
;
;**********
;
; Floating point version
GenerateRPtrCall MACRO	InstName,arg1,arg3,sufx
    	GetTypeRLetter <arg1>,MMTypeLetter
	MSetupBX <arg3>
	MGetSeg	<arg3>,MMSegNumber
	GetSegLetter MMSegNumber,MMSegLetter
if (MMsegNumber EQ 3)	;; Segment = CS
	push	es	;; make ES same as CS
	push	cs
	pop	es
endif ;; segment = CS

MMsinst	CATSTR	<AltM>,MMSegLetter,<InstName>,MMTypeLetter,<sufx>
	EXTRN	MMsinst:PROC
    	call	MMsinst
if (MMsegNumber EQ 3)
	pop	es	;; restore ES
endif ;; segment CS
	MRestoreBX	;
    	ENDM	;; GenerateRPtrCall
;
;**********
;
; Integer varsion
GenerateIPtrCall MACRO	InstName,arg1,arg3,sufx
    	GetTypeILetter <arg1>,MMTypeLetter
	MSetupBX <arg3>
	MGetSeg	<arg3>,MMSegNumber
	GetSegLetter MMSegNumber,MMSegLetter
if (MMsegNumber EQ 3)	;; Segment = CS
	push	es	;; make ES same as CS
	push	cs
	pop	es
endif ;; segment = CS

MMsinst	CATSTR	<AltM>,MMSegLetter,<InstName>,MMTypeLetter,<sufx>
    	EXTRN	MMsinst:PROC		;; the altmath interface call
    	call	MMsinst			;; the altmath interface call
if (MMsegNumber EQ 3)
	pop	es	;; restore ES
endif ;; segment CS
	MRestoreBX	;
    	ENDM	;; GenerateIPtrCall
;
;******************************************************************************
; fld
;
; Forms of this macro:
; 1	fld	ST
; 2	fld	ST(x)		;; where 0<=x<=7
; 3	fld	dword ptr [bx]	
; 4	fld	qword ptr [bx]
;
fld	MACRO	arg1,arg2,arg3,arg4
ifnb <arg4>			;; Check number of arguments
	.err			; Three arguments max for fld
endif	;; ifnb arg4
ifb <arg2>			;; Check if register form
	MgetST	<arg1>,MMstreg	;; Must be of form ST or ST(x)
  if (MMstreg EQ 0)
	EXTRN	AltMfdup:PROC
	call	AltMfdup	;; if ST 0 then duplicate TOS
  else	;; if ST(0)
	push	bx		;; fld ST(streg)
	mov	bl,MMstreg
	EXTRN	AltMfldst:PROC
	call	AltMfldst
	pop	bx
  endif	;; if ST(0)
else ;; arg2 not blank
  ifidni <arg2>,<ptr>
	GenerateRPtrCall <ld>,<arg1>,<arg3>
  else		;; if (isptr)
  	.err	Illegal form of fld instruction
  endif		;; if (isptr)
endif	;; arg2 not blank
	ENDM	;;   fld
;
;******************************************************************************
; fild
;
fild	MACRO	arg1,arg2,arg3,arg4
ifnb <arg4>			;; Check number of arguments
	.err			; Three arguments max for fild
endif	;; ifnb arg4
ifidni <arg2>,<ptr>
	GenerateIPtrCall <ld>,<arg1>,<arg3>
else		;; if arg2 is ptr
  	.err	Illegal form of fild instruction
endif		;; if arg2 is ptr
	ENDM	;; fild
;
;******************************************************************************
; fxch
;
fxch	MACRO	arg1,arg2
ifnb <arg2>			;; Check number of arguments
	.err			; Maximum one argument
endif	;; ifnb arg2
ifb <arg1>
	EXTRN	AltMfxch:PROC	;; moved inside macro
	call	AltMfxch
else
  	MgetST	<arg1>,MMstreg
  	push	bx
  	mov	bl,MMstreg
  	EXTRN	AltMfxchq:PROC
  	call	AltMfxchq
  	pop	bx
endif	;; ifb <arg1>
	ENDM		;; fxch
;
;******************************************************************************
; fst
fst	MACRO	arg1,arg2,arg3,arg4
ifnb <arg4>			;; Check number of arguments
	.err			; Three arguments max for fst
endif	;; ifnb arg4
ifidni <arg2>,<ptr>
	GenerateRPtrCall <st>,<arg1>,<arg3>
else		;; if (isptr)
  	.err	Illegal form of fst instruction
endif		;; if (isptr)
	ENDM	;; fst
;
;******************************************************************************
; fstp
;
fstp	MACRO	arg1,arg2,arg3,arg4
ifnb <arg4>			;; Check number of arguments
	.err			; Three arguments max for fstp
endif	;; ifnb arg4
ifb <arg2>
	MgetST arg1,MMstreg
  if (MMstreg EQ 0)
  	EXTRN	AltMfstpST0:PROC
  	call	AltMfstpST0
  else ;; if (MMstreg EQ 0)
  	.err	Illegal form of fstp instruction
  endif	;; if (MMstreg EQ 0)
else ;; ifb <arg2> 
  ifidni <arg2>,<ptr>
	GenerateRPtrCall <st>,<arg1>,<arg3>,<p>
  else		;; if (isptr)
  	.err	Illegal form of fstp instruction
  endif		;; if (isptr)
endif ;; ifb <arg2>
	ENDM	;; fstp
;
;******************************************************************************
; fistp
fistp	MACRO	arg1,arg2,arg3,arg4
ifnb <arg4>			;; Check number of arguments
	.err			; Three arguments max for fistp
endif	;; ifnb arg4
ifidni <arg2>,<ptr>
	GenerateIPtrCall <ist>,<arg1>,<arg3>,<p>
else		;; if (isptr)
  	.err	Illegal form of fistp instruction
endif		;; if (isptr)
	ENDM	;; fistp
;
;******************************************************************************
; fld1
;
fld1	MACRO
	EXTRN	AltMfld1:PROC
	call	AltMfld1
	ENDM	;; fld1
;
;******************************************************************************
; fldz
;
fldz	MACRO
	EXTRN	AltMfldz:PROC	;; moved inside macro
	call	AltMfldz
	ENDM	;; fldz
;
;******************************************************************************
; fcompp
fcompp	MACRO
	EXTRN	AltMfcompp:PROC
	call	AltMfcompp
	ENDM	;; fcompp
;
;******************************************************************************
; fstsw
fstsw	MACRO	arg1,arg2
ifnb	<arg2>
	.err	; Illegal form of fstsw, only one argument
else	;; if too many arguments
sEnd	CODE
sBegin	DATA
	EXTRN	AltMathStatus:WORD	;; declare AltMathStatus as extrn
sEnd	DATA
sBegin	CODE
	push	ax			;; save ax
	mov	ax,AltMathStatus	;; arg1 := AltMathStatus
	mov	arg1,ax
	pop	ax			;; restore ax
endif	;; if too many arguments
	ENDM	;; fstsw
;******************************************************************************
; fadd
fadd	MACRO	arg1,arg2,arg3,arg4
ifnb <arg4>			;; Check number of arguments
	.err			; Three arguments max for fadd
endif	;; ifnb arg4
ifb <arg1>
	EXTRN	AltMfadd:PROC
	call	AltMfadd
else
  ifidni <arg2>,<ptr>
	GenerateRPtrCall <add>,<arg1>,<arg3>
  else		;; if (isptr)
  	.err	Illegal form of fadd instruction
  endif		;; if (isptr)
endif	;; ifb arg1
	ENDM	;; fadd
;******************************************************************************
; fsub
fsub	MACRO	arg1,arg2,arg3,arg4
ifnb <arg4>			;; Check number of arguments
	.err			; Three arguments max for fsub
endif	;; ifnb arg4
ifb <arg1>
	EXTRN	AltMfsub:PROC
	call	AltMfsub
else
  ifidni <arg2>,<ptr>
	GenerateRPtrCall <sub>,<arg1>,<arg3>
  else		;; if (isptr)
  	.err	Illegal form of fsub instruction
  endif		;; if (isptr)
endif	;; ifb arg1
	ENDM	;; fsub
;******************************************************************************
; fmul
fmul	MACRO	arg1,arg2,arg3,arg4
ifnb <arg4>			;; Check number of arguments
	.err			; Three arguments max for fmul
endif	;; ifnb arg4
ifb <arg1>
	EXTRN	AltMfmul:PROC
	call	AltMfmul
else
  ifidni <arg2>,<ptr>
	GenerateRPtrCall <mul>,<arg1>,<arg3>
  else		;; if (isptr)
  	.err	Illegal form of fmul instruction
  endif		;; if (isptr)
endif	;; ifb arg1
	ENDM	;; fmul
;******************************************************************************
; fdiv
fdiv	MACRO	arg1,arg2,arg3,arg4
ifnb <arg4>			;; Check number of arguments
	.err			;; Three arguments max for fdiv
endif	;; ifnb arg4
ifb <arg1>
	EXTRN	AltMfdiv:PROC
	call	AltMfdiv
else
  ifidni <arg2>,<ptr>
	GenerateRPtrCall <div>,<arg1>,<arg3>
  else		;; if (isptr)
  	.err	Illegal form of fdiv instruction
  endif		;; if (isptr)
endif	;; ifb arg1
	ENDM	;; fdiv
;******************************************************************************
; fabs
fabs	MACRO
	EXTRN	AltMfabs:PROC	;; moved inside macro
	call	AltMfabs
	ENDM	;; fabs
;******************************************************************************
; fchs
fchs	MACRO
	EXTRN	AltMfchs:PROC	;; moved inside macro
	call	AltMfchs
	ENDM	;; fchs
;******************************************************************************
; fsqrt
fsqrt	MACRO
	EXTRN	AltMfsqrt:PROC
	call	AltMfsqrt
	ENDM	;; fsqrt
;******************************************************************************
; fwait
fwait	MACRO
	ENDM	;;fwait
;******************************************************************************
;
endif		; ife m87toalt_asm	
