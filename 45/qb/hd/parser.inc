; File: PARSER.INC
; NOTE: When making changes to this file, be sure to make equivalent
;	changes to file PARSER.H
; NOTE: For comments, see parser.h

PARSER_INC = -1			;remember that this file has been included
	IncludeOnce	heap

PERIOD_ID EQU TRUE		;temp switch - "." is a record separator

;**======================================================================**
;**==           External Interface to Parser Component                 ==**
;**======================================================================**

;CB_PCODE_MIN is used to ensure we never ps.bdpDst or grs.bdlDirect
;get so small so we can't execute a SYSTEM, SETMEM, or CLEAR stmt 
;in direct mode.
;
CB_PCODE_MIN EQU 20d

;**==================== parser interface descriptor ======================
;** The global  structure 'ps'  is used  to pass  inputs to  the
;** parser and receive the parser's results.
;**=======================================================================

PS_STRUCT		STRUC
;ParseLine INPUT source buffer for line to be parsed
PS_ldDummy	    DB 4 dup(0)
;ldDummy MUST immediately precede bdpSrc.  Used by TWIN
;The TWIN ld structure is layed over ldDummy and bdpSrc as follows:
;
;+-----------------------+----------+\
;| ldDummy[0]-ldDummy[1] | ld.flags | \
;+-----------------------+----------+  > Used only by TWIN (EditMgr)
;| ldDummy[2]-ldDummy[3] | ld.cb    | /
;+-----------------------+----------+<
;| bdpSrc.cbLogical	 | ld.cbMax | \
;+-----------------------+----------+  > Common data
;| bdpSrc.pb		 | ld.prgch | /
;+-----------------------+----------+/
;

PS_bdpSrc	DB SIZE BDP DUP(0) ;bdp for source buffer
PS_otxLine	DW 0		;text offset into current text table
				;where this line is being inserted.

;ParseLine INPUT/OUTPUT fields
PS_flags	DB 0		;see PSF_xxx below
PS_tEtCur	DB 26 DUP(0)
				;contains the default variable types
				;for where this statement is being
				;inserted in the text table. tEtCur[26]
				;is always ET_R4 for FN.xxx
PS_filler	DB 0		;round up to even byte boundary

;ParseLine OUTPUT fields

PS_bdpDst	DB SIZE BDP DUP(0) ;bdp for dest buffer
PS_bdErr	DB SIZE BD DUP(0)  ;bd for error buffer
PS_errCode	DW 0		;parser's error code
PS_oSrcErr	DW 0		;offset into src buffer where
				;  error occured
PS_STRUCT		ENDS

;PS_flags masks:
PSF_fParseExp		EQU 01h		;INPUT FLAG
   ;set if parser is to parse just an expression,
   ;zero if it is to parse a source line
PSF_fRetry		EQU 02h		;OUTPUT FLAG
   ;set if caller should call ParseLine again
   ;This is caused by parser non-terminals that require current
   ;text table to be in SS_RUDE state, like a COMMON statement.
   ;If current table isn't, we call AskCantCont, and if the user says
   ;ok to not continue, this flag is set.
PSF_UndoEdit		EQU 04h		;OUTPUT FLAG
   ;set if user wants to back out of this edit.
   ;Similar senerio to PSF_fRetry, but if this gets set if the user
   ;responds to AskCantCont with CANCEL.
PSF_fRef		EQU 08h		;OUTPUT FLAG
   ;If any labels or variables were referenced, on output, this is set,
   ;so the text manager knows to scan the whole program if the parsed
   ;statement was in direct mode.
PSF_fRudeIfErr		EQU 10h		;OUTPUT FLAG
   ;if non-zero, caller must call ModuleRudeEdit if line gets inserted
   ;as opReParse or doesn't get inserted at all (ie out-of-memory).
PSF_fLabelRef		EQU 20h		;OUTPUT FLAG
   ;If any labels were referenced this is set
   ;so the text manager can do an AskCantCont when the user tries to
   ;edit the program while there are label references within
   ;an active direct mode statement


;PS_errCode masks:
PSERR_fAsciiMsg		EQU 8000H
   ;Set if parser built an ASCII error message in ps.bdErr.
   ;Else, ps.errCode & PSERR_errCode = offset into QBI message table
   ;(i.e. ER_xxx or MSG_xxx)
   ;bits 1000, 0800, and 400 are unused
PSERR_fRude		EQU 4000H
   ;Set if varmgr has returned a RudeEdit error
PSERR_fAlert		EQU 2000H
   ;Set if error should be reported at entry time (FALSE if error should
   ;only be reported at ReParse time - just before RUN)
PSERR_errCode		EQU 03FFH

;Types of literal constants recognized by parser
;
OrdConstStart 0
OrdConst LIT_I2		; % suffix
OrdConst LIT_O2		; &O prefix
OrdConst LIT_H2		; &H prefix
OrdConst LIT_I4		; & suffix
OrdConst LIT_O4		; &&O prefix
OrdConst LIT_H4		; &&H prefix
OrdConst LIT_R4		; ! suffix
OrdConst LIT_R8		; # suffix
OrdConst LIT_STR	; "xxx"

;The parser compresses text strings in REMarks.  The lister expands this
;text. An encoded string record contains 3 bytes.
;	Compression STRUCT
;		fCompressed	DB 0DH
;		cRepeat 	DB ?
;		ch		DB ?
;	Compression ENDS
;The first byte is a flag identifying the char as a compression record.
;The second byte is a count of the number of characters which were compressed.
;The third byte is the character which was compressed.
;This encoding significantly cuts down the size of REMarks with sequential
;runs of characters.
;  For example, a string of 50 ' 's (spaces) is represented as:
;	0DH 32H 20H
;A single <CR> embedded in a line. Could be represented as with a compression
;record of:
;	0DH 01H 0DH
;0DH was chosen as the compresion flag since it is very difficult to embed
;a 0DH in a REM without having in interpreted as an end of line.

STR_EncodedText EQU 0DH



sBegin CP
ife	PRSUTIL_ASM
	EXTRN	ListStdMsgToBd:far
	EXTRN	CheckFreeDstBuf:near
	EXTRN	Emit16:near
	EXTRN	Emit16_AX:near
	EXTRN	ListIRW:far
endif	;PRSUTIL_ASM

ife	PRSNT_ASM
	EXTRN	SetDefBits:far
endif

ife	PRSMAIN_ASM
	EXTRN	ParseInit:far
	EXTRN	ParseNewInit:far
	EXTRN	ParseLine:near
	EXTRN	MakeOpReParse:near
	EXTRN	ParseUndo:near
	EXTRN	ResetDstPbCur:near
	EXTRN	SetDstPbCur:near
	EXTRN	SetPsBufSz:far
endif	;PRSMAIN_ASM

sEnd	CP

sBegin DATA
	EXTRN	ps:BYTE	

sEnd	DATA

