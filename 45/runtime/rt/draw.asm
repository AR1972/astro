	PAGE	56,132
	TITLE	DRAW - Draw Statement Support
;***
; DRAW - Draw Statement Support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - DRAW Statement:
;
;      DRAW string
;	 |
;      $DRA
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	_BSS		
	USESEG	_DATA		
	USESEG	CONST		
	USESEG	GR_TEXT 	

	INCLUDE seg.inc 	;segment definitions
	INCLUDE	rtps.inc	; variable types
	INCLUDE string.inc	


sBegin	_BSS			
;
;****************************************************************************
; External low-level function vectors
;****************************************************************************
;
externW b$SetAttr		
externW b$MapXYC		

	externW B$GRPACX 	;defined in GWDATA.ASM
	externW B$GRPACY 	;defined in GWDATA.ASM
	externD B$GRFACX 	;defined in GWDATA.ASM
	externD B$GRFACY 	;defined in GWDATA.ASM
	externB B$DRWFLG	;defined in GWDATA.ASM
	externW B$DRWSCL 	;defined in GWDATA.ASM
	externW B$DRWANG 	;defined in GWDATA.ASM
	externB b$ScreenMode	; defined in LLCGRP.ASM

	externW B$ASPRF		;defined in GWDATA.ASM
	externB B$DFRACX 	;defined in GWDATA.ASM
	externB B$DFRACY 	;defined in GWDATA.ASM
	externW B$COSA		;defined in GWDATA.ASM
	externW B$MSINA		;defined in GWDATA.ASM
	externW B$DSINA		;defined in GWDATA.ASM


	externB	b$Buf3		
	$BGTNUL EQU b$Buf3	
	externW B$TILFLG 	;defined in GWDATA.ASM
	externW B$TIPROG 	;defined in GWDATA.ASM
	externW B$TILNDX 	;defined in GWDATA.ASM

sEnd	_BSS			

sBegin	CONST			

	externD b$FP_256	; s.p. constant 256.0

	.8087			
	staticD FP_PI180,0.017453293	; s.p. PI/180

sEnd	CONST			

	externFP B$COS4	
	externFP B$SIN4	

assumes CS,GR_TEXT		
sBegin	GR_TEXT 		

	externNP B$GRMODE	




	externNP B$ftolrnd	; force to integer in AX

	externNP B$ERR_FC	
	externNP B$STCPCT	; Compact string space
	externNP B$STGETFRESIZ	; Get available size in string space

	externNP B$fmlds	
	externNP B$fmldw	

;low-level routines:
	externNP B$PaintInit	
	externNP B$SCINIT	; Performs screen initialization

	externNP B$INVIEW	


	externNP B$PAINTBEG	
	externNP B$INTQNOCPCT	; Init paint queue w/o compaction.


	SUBTTL	GML - Graphics Macro Language Support

	externNP B$GTABSC	
	externNP B$CLINE2	

MLCMD	MACRO	MC,ACTION
	DB	mc
	DW	OFFSET action
ENDM
MLARG	MACRO	MC,ACTION
	DB	mc+128
	DW	OFFSET action
ENDM

;EXT.CO  FETCHR,VARGET
	externNP B$VALSCN	
	externNP B$DECFET	
	externNP B$MACLNG	
	externNP B$MCLXEQ	
	externNP B$FETCHZ	


;***
;B$DRAW - process Draw statement
;
;Purpose:
;	Execute macro language substring containing DRAW commands.
;Entry:
;	sdDraw == string descriptor
;Exit:
;	None.
;Uses:
;	Per convention.
;Exceptions:
;	Control could be transfered to B$ERR_FC
;****
cProc	B$DRAW,<PUBLIC,FAR>,<SI,DI> 
parmSD	sdDraw			
cBegin				

	CALL	B$SCINIT	; initialize screen if not already done
	GetpSD	BX,sdDraw	; BX = psdDraw
	XOR	AL,AL		;CLEAR OUT DRAW FLAGS
	call	B$GRMODE	; are we in a graphics mode?
	JNZ	OKDRAW		;Brif in Graphics mode
	JMP	B$ERR_FC	  ; else error
OKDRAW: 			;In graphics mode: OK to draw

;	At this point, check if adequate room exists for a potential paint queue
;	If not, then compact the string heap now. The compaction cannot be done
;	later since it will invalidate the MCL string offsets.

	PUSH	BX		; Save psd
	CALL	B$STGETFRESIZ	; Determine the free size
	CMP	BX,900		; Is there space for 100 entries?
	JAE	DRAW_PROCEED	; Brif so - no need to compact
	CALL	B$STCPCT	; Else compact only the string space
DRAW_PROCEED:			
	POP	BX		; Get back BX (psd)

	MOV	DX,OFFSET DRWTAB ;DISPATCH TABLE FOR GML
	MOV	B$DRWFLG,AL	; AL must still be zero!
	CALL	B$MACLNG 	; GO DO IT.
cEnd				

DRWTAB:
	MLARG	"U",DRUP	;UP    ;GPS 29-JUN-83 ASM86 doesn't like DUP
	MLARG	"D",DDOWN	;DOWN
	MLARG	"L",DLEFT	;LEFT
	MLARG	"R",DRIGHT	;RIGHT
	MLCMD	"M",DMOVE	;MOVE
	MLARG	"E",DRWEEE	;-,-
	MLARG	"F",DRWFFF	;+,-
	MLARG	"G",DRWGGG	;+,+
	MLARG	"H",DRWHHH	;-,+
	MLARG	"A",DANGLE	;ANGLE COMMAND
	MLCMD	"T",TANGLE	;360 Degree Angle Command
	MLCMD	"B",DNOPLT	;MOVE WITHOUT PLOTTING
	MLCMD	"N",DNOMOV	;DON'T CHANGE CURRENT COORDS
	MLCMD	"X",B$MCLXEQ	;EXECUTE STRING
	MLARG	"C",DCOLR	;COLOR
	MLARG	"S",DSCALE	;SCALE
	MLCMD	"P",DPAINT	;PAINT fc,bc
	DB	00		;END OF TABLE

subttl	DRAW subcommands
page



;***
;	The following series of entry points handle integer arguments
;	to draw subcommands.  These entry points are the ones contained
;	in the draw subcommand dispatch table and are entered by a jump
;	from the macro language processor (mclprc.asm).  However, if a
;	"V1" draw subcommand is in effect (indicated by b$fTrans being
;	TRUE), then the draw subcommand arguments are interpreted as
;	floating point rather than integer arguments.  Floating point
;	arguments are handled by the corresponding routines with an "F"
;	prefix in the name (see above).  The two cases (integer or
;	floating point arguments) are distinguished before the dispatch
;	as follows:  mclprc.asm executes "cmp b$fTrans, TRUE" just prior
;	the the dispatch.  Thus psw.z will be set iff floating point 
;	arguments are being used.  Therefore each entry point for integer
;	args branches to the corresponding routine for floating point
;	args iff psw.z is set.  The psw is also stored in ah after the
;	"cmp b$fTrans,TRUE" and before the dispatch.
;****

;***
;DRUP
;Purpose:
;       Move up using integer args.
;Input:
;	ah = psw
;	psw.z set if args are f.p.
;       psw.z clear if args are integer
;Outputs:
;	move up specified amount
;****
DRUP:
        NEG     DX                      ;MOVE +0,-Y
;***
;DDOWN
;Purpose:
;       Move down using integer args.
;Input:
;	ah = psw
;	psw.z set if args are f.p.
;       psw.z clear if args are integer
;Outputs:
;	move down specified amount
;****
DDOWN:                                  ;MOVE +0,+Y
        MOV     CX,0                    ;DX=0
	JMP	SHORT DOMOVR		; TREAT AS RELATIVE MOVE
;***
;DLEFT
;Purpose:
;       Move left using integer args.
;Input:
;	ah = psw
;	psw.z set if args are f.p.
;       psw.z clear if args are integer
;Outputs:
;	move left specified amount
;****
DLEFT:
        NEG     DX                      ;MOVE -X,+0
;***
;DRIGHT
;Purpose:
;       Move right using integer args.
;Input:
;	ah = psw
;	psw.z set if args are f.p.
;       psw.z clear if args are integer
;Outputs:
;	move right specified amount
;****
DRIGHT:                                 ;MOVE +X,+0
        MOV     CX,DX                   ;[CX]=VALUE
        MOV     DX,0                    ;DY=0
	JMP	SHORT DOMOVR		; TREAT AS RELATIVE MOVE
;***
;DRWHHH
;Purpose:
;       Move up and left using integer args.
;Input:
;	ah = psw
;	psw.z set if args are f.p.
;       psw.z clear if args are integer
;Outputs:
;	move up and left specified amount
;****
DRWHHH:
        NEG     DX                      ;MOVE -X,-Y
;***
;DRWFFF
;Purpose:
;       Move down and right using integer args.
;Input:
;	ah = psw
;	psw.z set if args are f.p.
;       psw.z clear if args are integer
;Outputs:
;	move down and right specified amount
;****
DRWFFF:
        MOV     CX,DX                   ;MOVE +X,+Y
	JMP	SHORT DOMOVR		; TREAT AS RELATIVE MOVE
;***
;DRWEEE
;Purpose:
;       Move up and right using integer args.
;Input:
;	ah = psw
;	psw.z set if args are f.p.
;       psw.z clear if args are integer
;Outputs:
;	move up and right specified amount
;****
DRWEEE:
DRWHHC:					; continuation of DRWGGG
        MOV     CX,DX                   ;MOVE +X,-Y
        NEG     DX
	JMP	SHORT DOMOVR		; TREAT AS RELATIVE MOVE
;***
;DRWGGG
;Purpose:
;       Move down and left using integer args.
;Input:
;	ah = psw
;	psw.z set if args are f.p.
;       psw.z clear if args are integer
;Outputs:
;	move down and left specified amount
;****
DRWGGG:
        NEG     DX                      ;MOVE -X,+Y
        JMP     SHORT DRWHHC            ;MAKE DY POSITIVE & GO

;***
;DMOVE
;Purpose:
;       Move to the specified location
;Input:
;	b$fTrans TRUE if coords to be interpreted as floating point
;	b$fTrans FALSE if coords to be interpreted as integer
;Outputs:
;	graphics cursor position updated
;****
DMOVE:
	CALL	B$FETCHZ 	;GET NEXT CHAR AFTER COMMA
	CMP	AL,'+'		;IF '+' OR '-' THEN RELATIVE
	JZ	MOVREL
	CMP	AL,'-'
MOVREL:
	PUSHF			;SAVE ABS/REL FLAG ON STACK
	CALL	B$DECFET 	;BACK UP SO B$VALSCN WILL SEE "-"
	CALL	B$VALSCN 	;GET X VALUE
	PUSH	DX		;SAVE IT
GetComma:			
	CALL	B$FETCHZ 	;NOW CHECK FOR COMMA
	CMP	AL,','		;COMMA?
	JZ	MOVRE2
	JMP	B$ERR_FC
MOVRE2:
	CALL	B$VALSCN 	;[DX]= Y
	POP	CX		;[CX]= X
	MOV	BX,8080h	; x,y fraction is 1/2
	POPF			;GET ABS/REL FLAG

	JNZ	DRWABS		;NZ - ABSOLUTE


DOMOVR:
	CALL	DSCLDE		;ADJUST Y OFFSET BY SCALE
	PUSH	DX		;SAVE Y OFFSET
	MOV	DX,CX		;GET X INTO [DX]
	CALL	DSCLDE		;GO SCALE IT.
	MOV	CX,DX		;[CX]= Adjusted x
	POP	DX		;[DX]= Adjusted y
	XOR	AX,AX		;Assume no low order 8 bits
	CMP	BYTE PTR B$COSA+2,0 ;Check for cos(ang) = 0 (poss. no turnangle)
	JZ	CHKSINE 	;  brif cos(0) = 0; if sin(ang) = 0, no TA given
	CMP	BYTE PTR B$COSA+3,LOW 0 ;Check for 180 with B$COSA neg., B$DSINA= 0
	JNS	FRCANG		;brif cos(ang) was a positive number
CHKSINE:
	CMP	WORD PTR B$DSINA+2,0 ;If exp 0, then Angle is 0.
	JZ	DRWAFN		;Skip floating stuff if no angle..

; else calculate:
; Delta y [DXAH]:= (x*B$MSINA)+(y*B$COSA)
; Delta x [CXAL]:= (x*B$COSA) +(y*B$DSINA)

FRCANG:
	PUSH	DX		;Save y
	PUSH	CX		;Save x
	MOV	BX,OFFSET DGROUP:B$COSA
	CALL	FMULDX		;ST0 = (y*B$COSA)
	POP	DX		;Get x
	PUSH	DX
	MOV	BX,OFFSET DGROUP:B$MSINA
	CALL	FADARG		;[CXAL]= (x*B$MSINA)+(y*B$COSA)
	POP	DX		;Get x
	POP	BX		;Get y
	PUSH	CX
	PUSH	AX		;Save 24 bit y delta
	PUSH	BX		;Save y
	MOV	BX,OFFSET DGROUP:B$COSA
	CALL	FMULDX		;ST0 = (x*B$COSA)
	POP	DX		;Get y
	MOV	BX,OFFSET DGROUP:B$DSINA
	CALL	FADARG		;[CXAL]= (x*B$COSA)+(y*B$DSINA)
	POP	DX
	MOV	AH,DL		;[CXAL]= x Delta
	POP	DX		;[DXAH]= y Delta
DRWAFN:
	ADD	AL,[B$DFRACX]	;incr x low
	JNB	DRWXNC		;Brif no ovf
	INC	CX		;incr x high
DRWXNC:
	ADD	AH,[B$DFRACY]	;incr y low
	JNB	ANGPOS		;Brif no ovf
	INC	DX		;incr y high
ANGPOS:
	PUSH	AX		;Save fractional x,y
	CALL	B$GTABSC 	;[CX]= True x, [DX]= True y
	POP	BX		;Want fractional x,y in [BX]
DRWABS:
	MOV	AL,BYTE PTR B$DRWFLG ;SEE WHETHER WE PLOT OR NOT
	ADD	AL,AL		;CHECK HI BIT
	JB	DSTPOS		;JUST SET POSITION.
	PUSH	AX		;SAVE THIS FLAG
	PUSH	BX		;Save fractional x,y
	PUSH	CX		;SAVE X,Y COORDS
	PUSH	DX		;BEFORE SCALE SO REFLECT DISTANCE OFF
	CALL	B$CLINE2
	POP	DX
	POP	CX		;GET THEM BACK
	POP	BX
	POP	AX		;GET BACK FLAG
DSTPOS:
	ADD	AL,AL		;SEE WHETHER TO STORE COORDS
	JB	DNSTOR		;DON'T UPDATE IF B6=1
	MOV	B$GRPACY,DX	;UPDATE GRAPHICS AC
	MOV	B$GRPACX,CX
	MOV	WORD PTR [B$DFRACX],BX ;Store fractional x,y
DNSTOR:
	MOV	BYTE PTR B$DRWFLG,0 ; CLEAR SPECIAL FUNCTION FLAGS
	RET


DNOMOV:
	MOV	AL,64		;SET BIT SIX IN FLAG BYTE
	JMP	SHORT DSTFLG
DNOPLT:
	MOV	AL,128		;SET BIT 7
DSTFLG:
	OR	BYTE PTR B$DRWFLG,AL ;STORE UPDATED BYTE
	RET

DPAINT:

;	Initialize the paint queue without compacting it.

	cCALL	B$INTQNOCPCT	; Initialize it w/o compaction
	XOR	AL,AL
	MOV	BYTE PTR B$TILNDX,AL ;Clear Tile index
	MOV	BYTE PTR B$TIPROG,AL ;Clear Tile proceed flag
	MOV	BYTE PTR B$TILFLG,AL ;Initially no FG_TILE
	MOV	BYTE PTR $BGTNUL,AL ;Setup Null BG FG_TILE pattern
	CALL	B$VALSCN 	;GET Fill color
	PUSH	DX		;SAVE IT
	CALL	B$FETCHZ 	;NOW CHECK FOR COMMA
	CMP	AL,','		;COMMA?
	JZ	DPAIN2
	JMP	B$ERR_FC
DPAIN2:
	CALL	B$VALSCN 	;[DX]= Border Color
	POP	AX		;[AL]= fill color
	PUSH	DX		;In case SetAttr trashes DX
	CALL	[b$SetAttr]	;Set Fill attribute
	POP	DX
	JB	NCFCER		;Brif illegal
	MOV	AL,DL		;[AL]= Border color
	CALL	B$PaintInit	;Init Paint, check border attr
	JB	NCFCER		;Brif illegal
	MOV	CX,[B$GRPACX]	;[CX]= x
	MOV	DX,[B$GRPACY]	;[DX:= y
;The label PNTDRW no longer exists due to restructuring of PAINT statement
;12/7/83 JMB
;	JMP	PNTDRW		;Do the PAINT..
	CALL	B$INVIEW
	JNB	PAIRET		;Exit if out of FG_VIEW
	CALL	[b$MapXYC]	;Graphics cursor:=(CX,DX)
	JMP	B$PAINTBEG	;Go Paint it

DSCALE:
	JNB	NCFCER		;FC ERROR IF NO ARG
	CMP	DX,256		;MAKE SURE LESS THAN 256
	JNB	NCFCER
	OR	DX,DX
	JZ	NCFCER		;DONT ALLOW SCALE OF ZERO
	MOV	BYTE PTR B$DRWSCL,DL ;STORE SCALE FACTOR
PAIRET: 			;Early return from paint in draw
	RET

NCFCER:				; moved here to fix jump out of range
	JMP	B$ERR_FC

DSCLDE:
	MOV	AL,BYTE PTR B$DRWSCL ;GET SCALE FACTOR
	OR	AL,AL		;ZERO MEANS NO SCALING
	JNZ	DSCLD0
	RET
DSCLD0:
	MOV	BX,0
DSCLP:
	ADD	BX,DX		;ADD IN [DX] SCALE TIMES
	DEC	AL
	JNZ	DSCLP
	XCHG	DX,BX		;PUT IT BACK IN [DX]
	MOV	AL,DH		;SEE IF VALUE IS NEGATIVE
	ADD	AL,AL
	PUSHF			;SAVE RESULTS OF TEST
	JNB	DSCPOS
	DEC	DX		;MAKE IT TRUNCATE DOWN
DSCPOS:
	SHR	DX,1		;DIVIDE BY FOUR
	SHR	DX,1
	POPF			;SEE IF WAS NEGATIVE
	JNB	DSCPOX		;ALL DONE IF WAS POSITIVE
DSCPO1:
	OR	DH,192		;OR IN HIGH 2 BITS TO MAKE NEGATIVE
	INC	DX		;ADJUST SO TRUNCATING TO LOWER VALUE
DSCPOX:
	RET
DCOLR:
	JNB	NCFCER		;FC ERROR IF NO ARG
	MOV	AL,DL		;GO SET ATTRIBUTE
	CALL	[b$SetAttr]	
	JB	NCFCER		;ERROR IF ILLEGAL ATTRIBUTE
	RET


DANGLE:
	JNB	NCFCER		;ERROR IF NO ARG
	CMP	DL,4		;MAKE SURE LESS THAN 4
	JNB	NCFCER		;ERROR IF NOT
	MOV	AL,90		;Map B$DRWANG 0,1,2 or 3
	MUL	DL		;    to 0,90,180 or 270 degrees
	MOV	DX,AX
	JMP	SHORT TANGL1
TANGLE:
	CALL	B$FETCHZ 	;Get char after "T"
	CMP	AL,'A'		;Must be Angle
	JNZ	NCFCER		;else error
	CALL	B$VALSCN 	;[DX]= degrees
	MOV	AX,360
	OR	DX,DX		;See if negative
	JNS	TANGL0
	ADD	DX,AX		;neg so add 360 (let 'em go backwards)
TANGL0:
	CMP	AX,DX		;Must be .le. 360 degrees
	JB	NCFCER		; else error..
TANGL1:
;IFN	 SCNROT,<
;	 CALL	 SCNDIR
;	 JZ	 NoARot 	 ;Screen is horizontal
;	 SUBI	 DX,360d	 ;Swap X and Y
;	 NEG	 DX
;NoARot:
;>				 ;IFN FG_SCRNROT
	MOV	BYTE PTR B$DRWANG,DL
	MOV	BX,DX


	CALL	B$fmldw		; push [BX] to ST0
	FMUL	FP_PI180	; RAD(ang)= ang*(PI/180)
	FLD	ST(0)		; duplicate this on numeric stack
	CALL	B$COS4 		; ST0 = cos((Rad(ang))
	FSTP	DWORD PTR B$COSA ; B$COSA= COS((Rad(ang)), ST1 = Rad(ang)
	CALL	B$SIN4 		; Take Sin
	FLD	ST(0)		; duplicate SIN((Rad(ang)) on numeric stack
	CALL	GTASPF		; Get Aspect in [CXDX]
	CALL	B$fmlds		; load aspect to numeric stack
	FMUL		  ; ST0 = SIN((Rad(ang))*Aspect,ST1 = SIN(Rad(ang))
	FCHS		  ; ST0 = -SIN((Rad(ang))*Aspect, ST1 unchanged
	FSTP	DWORD PTR B$MSINA ; B$MSINA= -SIN((Rad(x))*Aspect
	CALL	B$fmlds		; ST0 = Aspect, ST1 = SIN(Rad(ang))
	FDIV			; ST0 = SIN((Rad(ang))/Aspect
	FSTP	DWORD PTR B$DSINA ; B$DSINA= SIN((Rad(ang))/Aspect
	FWAIT			
	RET			

;FMULDX
;Purpose:
;	Given an integer in DX and the address of a s.p. number in BX,
;	multiply the two numbers, leaving the result on top of numeric stack
;Entry:
;	DX contains an integer operand
;	BX is a pointer to an s.p. operand
;Exit:
;	ST0 contains the result of the multiplication
;Preserves:
;	SI, DI
FMULDX: 			
	PUSH	BX		
	MOV	BX,DX		
	CALL	B$fmldw		; push number on numeric stack
	POP	BX		
	FMUL	DWORD PTR [BX]	; multiply s.p. number time integer
	RET			

;FADARG
;Purpose:
;	Given an integer in DX and the address of a s.p. number in BX,
;	multiply the two numbers, with the result in ST0. Next,
;	add arguments at ST0 and ST1. Scale result by 2^8 (i.e., multiply
;	by 256), put 24-bit result in CX,AL (also in DX,AX)
;Entry:
;	DX contains an integer operand
;	BX is a pointer to an s.p. operand
;Exit:
;	CX,AL contains 24-bit result.
;Preserves:
;	SI, DI
FADARG:
	CALL	FMULDX		; multiply integer in DX by s.p. # at [BX]
	FADD			; ST0 = ST0 + ST1
	FMUL	b$FP_256	; scale result by 2^8 (i.e., mult. by 256)
	CALL	B$ftolrnd	; round, pop from ST0, get result in DX:AX
	MOV	CH,DL		
	MOV	CL,AH		; in [CX,AL]
	RET			


GTASPF:
	MOV	CX,WORD PTR [B$ASPRF+2]
	MOV	DX,WORD PTR [B$ASPRF]
	RET


sEnd	GR_TEXT 		
	END
