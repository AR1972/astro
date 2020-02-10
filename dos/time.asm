	TITLE	TIME - time and date functions
	NAME	TIME

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	TIME.ASM - System Calls and low level routines for DATE and TIME
;
;	$GET_DATE
;	$SET_DATE
;	$GET_TIME
;	$SET_TIME
;	DATE16
;	READTIME
;	DSLIDE
;	SETYEAR
;	DODATE
;	DSUM
;
;	Modification history:
;
;	    Created: ARR 30 March 1983
;
; 	M048 - set up DS using getdseg in date16 as we do not want to make 
;	       any assumptions regarding SS
;


	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	.cref
	.list


    i_need  DAY,BYTE
    i_need  MONTH,BYTE
    i_need  YEAR,WORD
    i_need  WEEKDAY,BYTE
    i_need  TIMEBUF,6
    i_need  BCLOCK,DWORD
    i_need  DAYCNT,WORD
    i_need  YRTAB,8
    i_need  MONTAB,12
    i_need  DATE_FLAG,WORD

    FOURYEARS = 3*365 + 366

DOSCODE	SEGMENT
	ASSUME	SS:DOSDATA,CS:DOSCODE

	allow_getdseg


	BREAK <DATE AND TIME - SYSTEM CALLS 42,43,44,45>

;**	$GET_DATE - Get Current Date
;
;	ENTRY	none
;	EXIT	(cx:dx) = current date
;	USES	all

procedure   $GET_DATE,NEAR

	Context DS
	CALL	READTIME		;Check for rollover to next day
	MOV	AX,[YEAR]

;	WARNING!!!! DAY and MONTH must be adjacently allocated!

	MOV	BX,WORD PTR [DAY]	; fetch both day and month
	invoke	get_user_stack		;Get pointer to user registers
    ASSUME DS:NOTHING
	MOV	[SI.user_DX],BX 	;DH=month, DL=day
	ADD	AX,1980 		;Put bias back
	MOV	[SI.user_CX],AX 	;CX=year

;hkn; SS override
	MOV	AL,BYTE PTR [WEEKDAY]
	return

EndProc $GET_DATE



;**	$SET_DATE - Set Current Date
;
;	ENTRY	(cx:dx) = current date
;	EXIT	(al) = -1 iff bad date
;		(al) = 0 if ok
;	USES	all

procedure   $SET_DATE,NEAR	;System call 43

	MOV	AL,-1			;Be ready to flag error
	SUB	CX,1980 		;Fix bias in year
	retc				;Error if not big enough
	CMP	CX,119			;Year must be less than 2100
	JA	RET24
	OR	DH,DH
	retz
	OR	DL,DL
	retz				;Error if either month or day is 0
	CMP	DH,12			;Check against max. month
	JA	RET24

;hkn; 	ss is DOSDATA
	Context DS
	invoke	DODATE
RET24:	return

EndProc $SET_DATE


;**	$GET_TIME - Get Current Time
;
;	ENTRY	none
;	EXIT	(cx:dx) = current time
;	USES	all

procedure   $GET_TIME,NEAR	; System call 44

;hkn; 	ss is DOSDATA
	Context DS
	CALL	READTIME
	invoke	get_user_stack		;Get pointer to user registers
	MOV	[SI.user_DX],DX
	MOV	[SI.user_CX],CX
	XOR	AL,AL
RET26:	return

EndProc $GET_TIME



;**	$SET_TIME - Set Current Time
;
;	ENTRY	(cx:dx) = time
;	EXIT	(al) = 0 if 0k
;		(al) = -1 if invalid
;	USES	ALL

procedure   $SET_TIME,NEAR	;System call 45

	MOV	AL,-1			;Flag in case of error
	CMP	CH,24			;Check hours
	JAE	RET26
	CMP	CL,60			;Check minutes
	JAE	RET26
	CMP	DH,60			;Check seconds
	JAE	RET26
	CMP	DL,100			;Check 1/100's
	JAE	RET26
	PUSH	CX
	PUSH	DX

;hkn; 	ss is DOSDATA
	Context DS

;hkn;	TIMEBUF	is now in DOSDATA
	MOV	BX,OFFSET DOSDATA:TIMEBUF

	MOV	CX,6
	XOR	DX,DX
	MOV	AX,DX
	PUSH	BX
	invoke	SETREAD

;hkn;	DOSAssume   CS,<ES>,"TIME/SetRead"
;hkn;	DS can still be used to access BCLOCK

	PUSH	DS
	LDS	SI,[BCLOCK]
ASSUME	DS:NOTHING
	invoke	DEVIOCALL2		;Get correct day count
	POP	DS
	DOSAssume   <DS>,"Set_Time"
	POP	BX
	invoke	SETWRITE
	POP	WORD PTR [TIMEBUF+4]
	POP	WORD PTR [TIMEBUF+2]
	LDS	SI,[BCLOCK]
ASSUME	DS:NOTHING
	invoke	DEVIOCALL2		;Set the time
	XOR	AL,AL
	return

EndProc $SET_TIME

SUBTTL DATE16, READTIME, DODATE -- GUTS OF TIME AND DATE
PAGE
;--------------------------------------------------------------------------
;
; Procedure Name : DATE16
;
; Date16 returns the current date in AX, current time in DX
;   AX - YYYYYYYMMMMDDDDD  years months days
;   DX - HHHHHMMMMMMSSSSS  hours minutes seconds/2
;
; DS = DOSGROUP on output
;
;--------------------------------------------------------------------------
procedure   DATE16,NEAR

;M048	Context DS		
;
; Since this function can be called thru int 2f we shall not assume that SS
; is DOSDATA

	getdseg	<ds>			; M048

	PUSH	CX
	PUSH	ES
	CALL	READTIME
	POP	ES
	SHL	CL,1			;Minutes to left part of byte
	SHL	CL,1
	SHL	CX,1			;Push hours and minutes to left end
	SHL	CX,1
	SHL	CX,1
	SHR	DH,1			;Count every two seconds
	OR	CL,DH			;Combine seconds with hours and minutes
	MOV	DX,CX

;	WARNING!  MONTH and YEAR must be adjacently allocated

	MOV	AX,WORD PTR [MONTH]	;Fetch month and year
	MOV	CL,4
	SHL	AL,CL			;Push month to left to make room for day
	SHL	AX,1
	POP	CX
	OR	AL,[DAY]
	return

EndProc DATE16

;----------------------------------------------------------------------------
;
; Procedure : READTIME
;
;Gets time in CX:DX. Figures new date if it has changed.
;Uses AX, CX, DX.
;
;----------------------------------------------------------------------------

procedure   READTIME,NEAR
	DOSAssume   <DS>,"ReadTime"

	MOV	[DATE_FLAG],0		; reset date flag for CPMIO
	PUSH	SI
	PUSH	BX

;hkn;	TIMEBUF is in DOSDATA
	MOV	BX,OFFSET DOSDATA:TIMEBUF

	MOV	CX,6
	XOR	DX,DX
	MOV	AX,DX
	invoke	SETREAD
	PUSH	DS
	LDS	SI,[BCLOCK]
ASSUME	DS:NOTHING
	invoke	DEVIOCALL2		;Get correct date and time
	POP	DS
	DOSAssume   <DS>,"ReadTime2"
	POP	BX
	POP	SI
	MOV	AX,WORD PTR [TIMEBUF]
	MOV	CX,WORD PTR [TIMEBUF+2]
	MOV	DX,WORD PTR [TIMEBUF+4]
	CMP	AX,[DAYCNT]		;See if day count is the same
	retz
	CMP	AX,FOURYEARS*30 	;Number of days in 120 years
	JAE	RET22			;Ignore if too large
	MOV	[DAYCNT],AX
	PUSH	SI
	PUSH	CX
	PUSH	DX			;Save time
	XOR	DX,DX
	MOV	CX,FOURYEARS		;Number of days in 4 years
	DIV	CX			;Compute number of 4-year units
	SHL	AX,1
	SHL	AX,1
	SHL	AX,1			;Multiply by 8 (no. of half-years)
	MOV	CX,AX			;<240 implies AH=0

;hkn;	YRTAB is in DOSDATA
	MOV     SI,OFFSET DOSDATA:YRTAB;Table of days in each year

	CALL	DSLIDE			;Find out which of four years we're in
	SHR	CX,1			;Convert half-years to whole years
	JNC	SK			;Extra half-year?
	ADD	DX,200
SK:
	CALL	SETYEAR
	MOV	CL,1			;At least at first month in year

;hkn;	MONTAB is in DOSDATA
	MOV     SI,OFFSET DOSDATA:MONTAB   ;Table of days in each month

	CALL	DSLIDE			;Find out which month we're in
	MOV	[MONTH],CL
	INC	DX			;Remainder is day of month (start with one)
	MOV	[DAY],DL
	CALL	WKDAY			;Set day of week
	POP	DX
	POP	CX
	POP	SI
RET22:	return
EndProc READTIME

;----------------------------------------------------------------------------
; Procedure : DSLIDE
;----------------------------------------------------------------------------

	procedure   DSLIDE,NEAR
	MOV	AH,0
DSLIDE1:
	LODSB				;Get count of days
	CMP	DX,AX			;See if it will fit
	retc				;If not, done
	SUB	DX,AX
	INC	CX			;Count one more month/year
	JMP	SHORT DSLIDE1
EndProc DSLIDE

;----------------------------------------------------------------------------
;
; Procedure : SETYEAR
;
; Set year with value in CX. Adjust length of February for this year.
;
; NOTE: This can also be called thru int 2f. If this is called then it will
;       set DS to DOSDATA. Since the only guy calling this should be the DOS
;	redir, DS will be DOSDATA anyway. It is going to be in-efficient to
;	preserve DS as CHKYR is also called as a routine.
;
;----------------------------------------------------------------------------

procedure   SETYEAR,NEAR

	GETDSEG DS

	MOV	BYTE PTR [YEAR],CL
CHKYR:
	TEST	CL,3			;Check for leap year
	MOV	AL,28
	JNZ	SAVFEB			;28 days if no leap year
	INC	AL			;Add leap day
SAVFEB:
	MOV	[MONTAB+1],AL		;Store for February
RET23:	return

EndProc SETYEAR

;---------------------------------------------------------------------------
;
; Procedure Name : DODATE
;
;---------------------------------------------------------------------------

procedure   DODATE,NEAR

	DOSAssume   <DS>,"DoDate"
	CALL	CHKYR			;Set Feb. up for new year
	MOV	AL,DH

;hkn;	MONTAB is in DOSDATA
	MOV     BX,OFFSET DOSDATA:MONTAB-1

	XLAT				;Look up days in month
	CMP	AL,DL
	MOV	AL,-1			;Restore error flag, just in case
	retc				;Error if too many days
	CALL	SETYEAR
;
; WARNING!  DAY and MONTH must be adjacently allocated
;
	MOV	WORD PTR [DAY],DX	;Set both day and month
	SHR	CX,1
	SHR	CX,1
	MOV	AX,FOURYEARS
	MOV	BX,DX
	MUL	CX
	MOV	CL,BYTE PTR [YEAR]
	AND	CL,3

;hkn;	YRTAB is in DOSDATA
	MOV	SI,OFFSET DOSDATA:YRTAB
	MOV	DX,AX
	SHL	CX,1			;Two entries per year, so double count
	CALL	DSUM			;Add up the days in each year
	MOV	CL,BH			;Month of year

;hkn;	MONTAB is in DOSDATA
	MOV	SI,OFFSET DOSDATA:MONTAB
	DEC	CX			;Account for months starting with one
	CALL	DSUM			;Add up days in each month
	MOV	CL,BL			;Day of month
	DEC	CX			;Account for days starting with one
	ADD	DX,CX			;Add in to day total
	XCHG	AX,DX			;Get day count in AX
	MOV	[DAYCNT],AX
	PUSH	SI
	PUSH	BX
	PUSH	AX

;hkn;	TIMEBUF is in DOSDATA
	MOV	BX,OFFSET DOSDATA:TIMEBUF

	MOV	CX,6
	XOR	DX,DX
	MOV	AX,DX
	PUSH	BX
	invoke	SETREAD

;hkn;	DOSAssume   CS,<ES>,"DoDate/SetRead"
;hkn;	DS is still valid and can be used to access BCLOCK

	PUSH	DS
	LDS	SI,[BCLOCK]
ASSUME	DS:NOTHING
	invoke	DEVIOCALL2		;Get correct date and time
	POP	DS
	POP	BX
	DOSAssume   <DS>,"DoDate2"
	invoke	SETWRITE
	POP	WORD PTR [TIMEBUF]
	PUSH	DS
	LDS	SI,[BCLOCK]
ASSUME	DS:NOTHING
	invoke	DEVIOCALL2		;Set the date
	POP	DS
	DOSAssume   <DS>,"DoDate3"
	POP	BX
	POP	SI
WKDAY:
	MOV	AX,[DAYCNT]
	XOR	DX,DX
	MOV	CX,7
	INC	AX
	INC	AX			;First day was Tuesday
	DIV	CX			;Compute day of week
	MOV	[WEEKDAY],DL
	XOR	AL,AL			;Flag OK
Ret25:	return

EndProc DODATE



;**	DSUM - Compute the sum of a string of bytes
;
;	ENTRY	(cx) = byte count
;		(ds:si) = byte address
;		(dx) = sum register, initialized by caller
;	EXIT	(dx) updated
;	USES	ax, cx, dx, si, flags

procedure   DSUM,NEAR

	MOV	AH,0
	JCXZ	dsum9

dsum1:	LODSB
	ADD	DX,AX
	LOOP	DSUM1
dsum9:	return

EndProc DSUM


DOSCODE	     ENDS
	END
