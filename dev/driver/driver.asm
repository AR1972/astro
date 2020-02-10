;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
	PAGE 64,132
;
; External block device driver
; Hooks into existing routines in IBMBIO block driver via Int 2F mpx # 8.
; This technique minimizes the size of the driver.
;

; Revised Try_h: to test for flagheads  as msg was being displayed on bds_formfactor
;  this caused the bds_formfactor to be set in the Head
; Revised the # of sectors/cluster for F0h to 1
;
;	Revision History
;	================
;
;	M000	SR	10/19/90	Changed F_Val to handle the formfactor
;				value 9 for 2.88M media.
;
;


code segment byte public
assume cs:code,ds:code,es:code

;
.xlist
include SYSMSG.INC			;equates and macros
.list
MSG_UTILNAME <DRIVER>

iTEST = 0
;---------------------------------------------------
;
;	Device entry point
;
DSKDEV	LABEL	WORD
	DW	-1,-1			; link to next device
	DW	0000100001000000B	; bit 6 indicates DOS 3.20 driver
	DW	STRATEGY
	DW	DSK$IN
DRVMAX	DB	1

;
; Various equates
;
CMDLEN	equ	0			;LENGTH OF THIS COMMAND
UNIT	equ	1			;SUB UNIT SPECIFIER
CMD	equ	2			;COMMAND CODE
STATUS	equ	3			;STATUS
MEDIA	equ	13			;MEDIA DESCRIPTOR
TRANS	equ	14			;TRANSFER ADDRESS
COUNT	equ	18			;COUNT OF BLOCKS OR CHARACTERS
START	equ	20			;FIRST BLOCK TO TRANSFER
EXTRA	equ	22			;Usually a pointer to Vol Id for error 15
CONFIG_ERRMSG  equ     23		; To set this field to Non-zero
					;	to display "Error in CONFIG.SYS..."

PTRSAV	DD	0


STRATP PROC FAR

STRATEGY:
	MOV	WORD PTR CS:[PTRSAV],BX
	MOV	WORD PTR CS:[PTRSAV+2],ES
	RET

STRATP ENDP

DSK$IN:
	push	es
	push	bx
	push	ax
	les	bx,cs:[ptrsav]
	cmp	byte ptr es:[bx].cmd,0
	jnz	Not_Init
	jmp	DSK$INIT

not_init:
; Because we are passing the call onto the block driver in IBMBIO, we need to
; ensure that the unit number corresponds to the logical (DOS) unit number, as
; opposed to the one that is relevant to this device driver.
	mov	al,byte ptr cs:[DOS_Drive_Letter]
	mov	byte ptr es:[bx].UNIT,al
	mov	ax,0802H
	int	2fH
;
; We need to preserve the flags that are returned by IBMBIO. YUK!!!!!
;
	pushf
	pop	bx
	add	sp,2
	push	bx
	popf

exitp	proc	far
DOS_Exit:
	pop	ax
	POP	BX
	POP	ES
	RET				;RESTORE REGS AND RETURN
EXITP	ENDP

TINY_BPB	=	1	; use short form of bpb.inc
TINY_BDS	=	1	; use short form of msbds.inc
include	bpb.inc				; include BPB structure
include msbds.inc			; include BDS structures

BDS	DW	-1			;Link to next structure
	DW	-1
	DB	1			;Int 13 Drive Number
	DB	3			;Logical Drive Letter
FDRIVE:
	DW	512			;Physical sector size in bytes
	DB	-1			;Sectors/allocation unit
	DW	1			;Reserved sectors for DOS
	DB	2			;No. allocation tables
	DW	64			;Number directory entries
	DW	9*40			;Number sectors (at 512 bytes ea.)
	DB	00000000B		;Media descriptor, initially 00H.
	DW	2			;Number of FAT sectors
	DW	9			;Sector limit
	DW	1			;Head limit
	DW	0			;Hidden sector count
	dw	0			; Hidden sector count (High)
	dw	0			; Number sectors (low)
	dw	0			; Number sectors (high)
	DB	0			; TRUE => Large fats
OPCNT1	DW	0			;Open Ref. Count
	DB	2			;Form factor
FLAGS1	DW	0020H			;Various flags
	DW	80			;Number of cylinders in device
RecBPB1 DW	512			; default is that of 3.5" disk
	DB	2
	DW	1
	DB	2
	DW	70h
	DW	2*9*80
	DB	0F9H
	DW	3
	DW	9
	DW	2
	DW	0
	dw	0	
	dw	0	
	dw	0	
	db	6 dup (0)		;AC000;
TRACK1	DB	-1			;Last track accessed on this drive
TIM_LO1 DW	-1			;Keep these two contiguous (?)
TIM_HI1 DW	-1
VOLID1	DB	"NO NAME    ",0         ;Volume ID for this disk
VOLSER	dd	0	
FILE_ID db	"FAT12   ",0            ;

DOS_Drive_Letter	db	?	; Logical drive associated with this unit

ENDCODE LABEL WORD			; Everything below this is thrown away
					; after initialisation.

DskDrv	    dw	    offset FDRIVE	; "array" of BPBs

; For system parser;

FarSW	equ	0	; Near call expected

DateSW	equ	0	; Check date format

TimeSW	equ	0	; Check time format

FileSW	equ	0	; Check file specification

CAPSW	equ	0	; Perform CAPS if specified

CmpxSW	equ	0	; Check complex list

NumSW	equ	1	; Check numeric value

KeySW	equ	0	; Support keywords

SwSW	equ	1	; Support switches

Val1SW	equ	1	; Support value definition 1

Val2SW	equ	1	; Support value definition 2

Val3SW	equ	0	; Support value definition 3

DrvSW	equ	0	; Support drive only format

QusSW	equ	0	; Support quoted string format
;---------------------------------------------------
;.xlist
assume ds:nothing				;!!!Parse.ASM sometimes assumes DS
                                                ;      to access its own variable!!!
        include version.inc
	include PARSE.ASM			;together with PSDATA.INC
assume ds:code			
;.list
;Control block definitions for PARSER.
;---------------------------------------------------
Parms	label	byte
	dw	Parmsx
	db	0		;No extras

Parmsx	label	byte
	db	0,0		;No positionals
	db	5		;5 switch control definitions
	dw	D_Control	;/D
	dw	T_Control	;/T
	dw	HS_Control	;/H, /S
	dw	CN_Control	;/C, /N
	dw	F_Control	;/F
	db	0		;no keywords

D_Control	label	word
	dw	8000h		;numeric value
	dw	0		;no functions
	dw	Result_Val	;result buffer
	dw	D_Val		;value defintions
	db	1		;# of switch in the following list
Switch_D	label	byte
	db	'/D',0          ;

D_Val	label	byte
	db	1		;# of value defintions
	db	1		;# of ranges
	db	1		;Tag value when match
;	 dd	 0,255		 ;
	dd	0,127		;Do not allow a Fixed disk.

Result_Val	label	byte
	db	?
Item_Tag	label	byte
	db	?
Synonym_ptr	label	word
	dw	?		;es:offset -> found Synonym
RV_Byte 	label	byte
RV_Word 	label	word
RV_Dword	label	dword
	dd	?		;value if number, or seg:off to string

T_Control	label	word
	dw	8000h		;numeric value
	dw	0		;no functions
	dw	Result_Val	;result buffer
	dw	T_Val		;value defintions
	db	1		;# of switch in the following list
Switch_T	label	byte
	db	'/T',0          ;

T_Val	label	byte
	db	1		;# of value defintions
	db	1		;# of ranges
	db	1		;Tag value when match
	dd	1,999

HS_Control	label	word
	dw	8000h		;numeric value
	dw	0		;no function flag
	dw	Result_Val	;Result_buffer
	dw	HS_VAL		;value definition
	db	2		;# of switch in following list
Switch_H	label	byte
	db	'/H',0          ;
Switch_S	label	byte
	db	'/S',0          ;

HS_Val	 label	 byte
	db	1		;# of value defintions
	db	1		;# of ranges
	db	1		;Tag value when match
	dd	1,99

CN_Control	 label	 word
	dw	0		;no match flags
	dw	0		;no function flag
	dw	Result_Val	;no values returned
	dw	NoVal		;no value definition
;	 db	 2		 ;# of switch in following list
	db	1
Switch_C	label	byte
	db	'/C',0          ;
;Switch_N	 label	 byte	 ;
;	 db	 '/N',0          ;

Noval	db	0

F_Control	label	word
	dw	8000h		;numeric value
	dw	0		;no function flag
	dw	Result_Val	;Result_buffer
	dw	F_VAL		;value definition
	db	1		;# of switch in following list
Switch_F	label	byte
	db	'/F',0          ;

F_Val		label	byte
	db	2		;# of value definitions (Order dependent)
	db	0		;no ranges
	db	5		;# of numeric choices ;M000; 5 now
F_Choices	label	byte
	db	1		;1st choice (item tag)
	dd	0		;0
	db	2		;2nd choice
	dd	1		;1
	db	3		;3rd choice
	dd	2		;2
	db	4		;4th choice
	dd	7		;7
	db	5		;5th choice ;M000
	dd	9		;9 ;M000


;System messages handler data
;Put the data here
.sall
MSG_SERVICES <MSGDATA>

;Place the messages here
MSG_SERVICES <DRIVER.CL1, DRIVER.CL2, DRIVER.CLA>

;Put messages handler code here.
MSG_SERVICES <LOADmsg,DISPLAYmsg,CHARmsg>
.xall

;
; Sets ds:di -> BDS for this drive
;
SetDrive:
	push	cs
	pop	ds
	mov	di,offset BDS
	ret

;
; Place for DSK$INIT to exit
;
ERR$EXIT:
	MOV	AH,10000001B			   ;MARK ERROR RETURN
	lds	bx, cs:[ptrsav]
	mov	byte ptr ds:[bx.MEDIA], 0	   ; # of units
	mov	word ptr ds:[bx.CONFIG_ERRMSG], -1 ;Show IBMBIO error message too.
	JMP	SHORT ERR1

Public EXIT
EXIT:	MOV	AH,00000001B
ERR1:	LDS	BX,CS:[PTRSAV]
	MOV	WORD PTR [BX].STATUS,AX ;MARK OPERATION COMPLETE

RestoreRegsAndReturn:
	POP	DS
	POP	BP
	POP	DI
	POP	DX
	POP	CX
	POP	AX
	POP	SI
	jmp	dos_exit


drivenumb   db	    5
cyln	    dw	    80
heads	    dw	    2
ffactor     db	    2
slim	    dw	    9

Switches    dw	0

Drive_Let_Sublist	label	dword
	db     11	;length of this table
	db	0	;reserved
	dw	D_Letter;
D_Seg	dw	?	;Segment value. Should be CS
	db	1	;DRIVER.SYS has only %1
	db	00000000b ;left align(in fact, Don't care), a character.
	db	1	;max field width 1
	db	1	;min field width 1
	db	' '     ;character for pad field (Don't care).

D_Letter	db	"A"

if iTEST
Message:
	push	ax
	push	ds
	push	cs
	pop	ds
	mov	ah,9
	int	21h
	pop	ds
	pop	ax
	ret
extrn	nodrive:byte,loadokmsg:byte,letter:byte, badvermsg:byte
endif


if iTEST
%OUT Testing On
initmsg     db	    "Initializing device driver",13,10,"$"
stratmsg    db	    "In strategy of driver",10,13,"$"
dskinmsg    db	    "In DSKIN part of driver",10,13,"$"
outinitmsg  db	    "Out of init code ",10,13,"$"
exitmsg     db	    "Exiting from driver",10,13,"$"
parsemsg    db	    "Parsing switches",10,13,"$"
errmsg	    db	    "Error occurred",10,13,"$"
linemsg     db	    "Parsed line",10,13,"$"
int2fokmsg  db	    "****************Int2f loaded**************",10,13,"$"
mediamsg    db	    "Media check ok",10,13,"$"
getbpbmsg   db	    "getbpb ok",10,13,"$"
iookmsg     db	    "Successful I/O",10,13,"$"
parseokmsg  db	    "Parsing done fine",10,13,"$"
nummsg	    db	    "Number read is "
number	    db	    "00  ",10,13,"$"
drvmsg	    db	    "Process drive "
driven	    db	    "0",10,13,"$"
cylnmsg     db	    "Process cylinder ",10,13,"$"
slimmsg     db	    "Process sec/trk ",10,13,"$"
hdmsg	    db	    "Process head "
hdnum	    db	    "0",10,13,"$"
ffmsg	    db	    "Process form factor "
ffnum	    db	    "0",10,13,"$"
nxtmsg	    db	    "Next switch ",10,13,"$"
msg48tpi    db	    "Got a 48 tpi drive",10,13,"$"

ENDIF

DSK$INIT:
	PUSH	SI
	PUSH	AX
	PUSH	CX
	PUSH	DX
	PUSH	DI
	PUSH	BP
	PUSH	DS

	LDS	BX,CS:[PTRSAV]		;GET POINTER TO I/O PACKET

	MOV	AL,BYTE PTR DS:[BX].UNIT    ;AL = UNIT CODE
	MOV	AH,BYTE PTR DS:[BX].MEDIA   ;AH = MEDIA DESCRIP
	MOV	CX,WORD PTR DS:[BX].COUNT   ;CX = COUNT
	MOV	DX,WORD PTR DS:[BX].START   ;DX = START SECTOR

	LES	DI,DWORD PTR DS:[BX].TRANS

	PUSH	CS
	POP	DS

	ASSUME	DS:CODE

	cld
	push	cs			; Initialize Segment of Sub list.
	pop	[D_Seg] 
	call	SYSLOADMSG		; linitialize message handler
	jnc	GoodVer 		; Error. Do not install driver.
	mov	cx, 0			; No substitution
	mov	dh, -1			; Utility message
	call	Show_Message		; Show message
	jmp	short err$exitj2	;  and exit

;; check for correct DOS version
;	 mov	 ah,30h
;	 int	 21H

;	 cmp	 ax,expected_version
;	 je	 GoodVer

;	cmp	al,DOSVER_HI
;	jnz	BadDOSVer
;	cmp	ah,DOSVER_LO
;	jz	GoodVer

;BadDOSVer:
;	 Mov	 dx,offset BadVerMsg
;	 call	 message
;	 jmp	 err$exitj2		 ; do not install driver

GoodVer:
	mov	ax,0800H
	int	2fH			    ; see if installed
	cmp	al,0FFH
	jnz	err$exitj2		    ; do not install driver if not present
	lds	bx,[ptrsav]
	mov	si,word ptr [bx].count	    ; get pointer to line to be parsed
	mov	ax,word ptr [bx].count+2
	mov	ds,ax
	call	Skip_Over_Name		    ; skip over file name of driver
	mov	di,offset BDS		    ; point to BDS for drive
	push	cs
	pop	es			    ; es:di -> BDS
	Call	ParseLine
	jc	err$exitj2
	LDS	BX,cs:[PTRSAV]
	mov	al,byte ptr [bx].extra	; get DOS drive letter
	mov	byte ptr es:[di].bds_drivelet,al
	mov	cs:[DOS_Drive_Letter],al
	add	al,"A"
;	 mov	 cs:[letter],al 	 ; set up for printing final message
	mov	cs:[D_Letter], al
	call	SetDrvParms		; Set up BDS according to switches
	jc	err$exitj2
	mov	ah,8			; Int 2f multiplex number
	mov	al,1			; install the BDS into the list
	push	cs
	pop	ds			; ds:di -> BDS for drive
	mov	di,offset BDS
	int	2FH
	lds	bx,dword ptr cs:[ptrsav]
	mov	ah,1
	mov	cs:[DRVMAX],ah
	mov	byte ptr [bx].media,ah
	mov	ax,offset ENDCODE
	mov	word ptr [bx].TRANS,AX	    ; set address of end of code
	mov	word ptr [bx].TRANS+2,CS
	mov	word ptr [bx].count,offset DskDrv
	mov	word ptr [bx].count+2,cs

	push	dx
	push	cs
	pop	ds
	mov	si, offset Drive_Let_SubList  ;AC000;
	mov	ax, LOADOK_MSG_NUM	;load ok message
	mov	cx, 1			; 1 substitution
	mov	dh, -1			; utility message
	call	Show_Message
;	 mov	 dx,offset loadokmsg
;	 call	 message
	pop	dx
	jmp	EXIT

err$exitj2:
	stc
	jmp	err$exit

;
; Skips over characters at ds:si until it hits a `/` which indicates a switch
; J.K. If it hits 0Ah or 0Dh, then will return with SI points to that character.
Skip_Over_Name:
	call	scanblanks
loop_name:
	lodsb
	cmp	al,CR		
	je	End_SkipName	
	cmp	al,LF		
	je	End_SkipName	
	cmp	al,'/'
	jnz	loop_name
End_SkipName:			
	dec	si			    ; go back one character
	RET

;ParseLine:
;	 push	 di
;	 push	 ds
;	 push	 si
;	 push	 es
;Next_Swt:
;IF iTEST
;	 mov	 dx,offset nxtmsg
;	 call	 message
;ENDIF
;	 call	 ScanBlanks
;	 lodsb
;	 cmp	 al,'/'
;	 jz	 getparm
;	 cmp	 al,13		     ; carriage return
;	 jz	 done_line
;	 CMP	 AL,10		     ; line feed
;	 jz	 done_line
;	 cmp	 al,0		     ; null string
;	 jz	 done_line
;	 mov	 ax,-2		     ; mark error invalid-character-in-input
;	 stc
;	 jmp	 short exitparse
;
;getparm:
;	 call	 Check_Switch
;	 mov	 cs:Switches,BX      ; save switches read so far
;	 jnc	 Next_Swt
;	 cmp	 ax,-1		     ; mark error number-too-large
;	 stc
;	 jz	 exitparse
;	 mov	 ax,-2		     ; mark invalid parameter
;	 stc
;	 jmp	 short exitparse
;
;done_line:
;	 test	 cs:Switches,flagdrive	   ; see if drive specified
;	 jnz	 okay
;	 push	 dx
;	 mov	 ax, 2
;	 call	 Show_Message
;	 mov	 dx,offset nodrive
;	 call	 message
;	 pop	 dx
;	 mov	 ax,-3		     ; mark error no-drive-specified
;	 stc
;	 jmp	 short exitparse
;
;okay:
;	 call	 SetDrive			; ds:di points to BDS now.
;	 mov	 ax,cs:Switches
;	 and	 ax,fChangeline+fNon_Removable	; get switches for Non_Removable and Changeline
;	 or	 ds:[di].flags,ax
;	 xor	 ax,ax		     ; everything is fine
;
;;
;; Can detect status of parsing by examining value in AX.
;;	 0  ==>  Successful
;;	 -1 ==>  Number too large
;;	 -2 ==>  Invalid character in input
;;	 -3 ==>  No drive specified
;
;	 clc
;exitparse:
;	 pop	 es
;	 pop	 si
;	 pop	 ds
;	 pop	 di
;	 ret



ParseLine	proc	near
;In) DS:SI -> Input string
;    ES = CS
;    ES:DI -> BDS table inside this program
;
;Out)
;	if successfule, then {	AX will be set according to the switch
;				flag value.  BDS.Flag, Drivenumb, cylin,
;				slim, heads ffactor are set }
;	else
;	   {
;	    If (no drive specified) then { display messages };
;	    Set carry;
;	   }
;
;Subroutine to be called:
;	SYSPARSE:NEAR, SHOW_MESSAGE:NEAR, GET_RESULT:NEAR
;
;Logic:
;{	While (Not end_of_Line)
;	 {
;	  SYSPARSE ();
;	  if (no error) then
;	      GET_RESULT ()
;	else
;	      Set carry;
;	  };
;
;	if (carry set) then Exit;	/* Initialization failed */
;	if (No drive number entered)	/* Drive number is a requirement */
;	 then { Show_Message ();
;		exit;
;	      };
;
	assume	ds:nothing		;make sure
	push	di			;save BDS pointer
	mov	di, offset PARMS	;now, es:di -> parse control definition
SysP_While:		
	xor	cx, cx			; I don't have positionals.
	xor	dx, dx	
	call	SYSPARSE
	cmp	ax, $P_RC_EOL		;end of line?
	je	SysP_End
	cmp	ax, $P_NO_ERROR 	;no error?
	jne	SysP_Fail
	call	Get_Result
	jmp	SysP_While
SysP_End:		
	test	Switches, FLAGDRIVE	;drive number specified?
	jnz	SysP_Ok 		;Drive number is a requirement
	push	ds	
	mov	ax, NODRIVE_MSG_NUM	;no drive specification
	mov	cx, 0			;no substitutions
	mov	dh, -1			;utility message
	call	Show_Message
	pop	ds	
	jmp short SysP_Err
SysP_Fail:		
	mov	dh, 2			; parse error
	mov	cx, 0	
	call	Show_Message		; Show parse error
SysP_Err:		
	stc		
	jmp short PL_Ret
SysP_Ok:		
	clc		
PL_Ret: 		
	pop	di			;restore BDS pointer
	ret		
ParseLine	endp

;
Get_Result	proc	near
;In) A successful result of SYSPARSE in Result_Val
;    es = cs, ds = command line segment
;Out)
;   Switches set according to the user option.
;   Drivenumb, Cyln, Heads, Slim, ffactor set if specified.
;Logic)
;   Switch (Synonym_Ptr)
;	{ case Switch_D: Switches = Switches | FLAGDRIVE; /* Set switches */
;			 Drivenumb = Reg_DX.Value_L;
;			 break;
;
;	  case Switch_T: Switches = Switches | Flagcyln;
;			 Cyln	= Reg_DX.Value_L;
;			 break;
;
;	  case Switch_H: Switches = Switches | Flagheads;
;			 Heads	= Reg_DX.Value_L;
;			 break;
;
;	  case Switch_S: Switches = Switches | FlagSecLim;
;			 Slim	= Reg_DX.Value_L;
;			 break;
;
;	  case Switch_C: Switches = Switches | fChangeline;
;			 break;
;
;;	   case Switch_N: Switches = Switches | fNon_Removable;
;;			  break;
;
;	  case Switch_F: Switches = Switches | Flagff;
;			 Reg_DX = (Reg_DX.ITEM_Tag - 1)*5;/*Get the offset of
;							  /*the choice.
;			 ffactor = byte ptr (F_Choices + DX + 1);
;					/*Get the value of it */
;			 break;
;
;	}
;


	mov	ax, Synonym_Ptr 
	push	ax			; save Synonym_ptr
	cmp	ax, offset Switch_D
	jne	Stch_T	
	or	Switches, FLAGDRIVE
	mov	al, RV_Byte
	mov	Drivenumb, al
	jmp	short GR_Ret
Stch_T: 		
	cmp	ax, offset Switch_T
	jne	Stch_H	
	or	Switches, FLAGCYLN
	mov	ax, RV_Word
	mov	Cyln, ax
	jmp	short GR_Ret
Stch_H: 		
	cmp	ax, offset Switch_H
	jne	Stch_S	
	or	Switches, FLAGHEADS
	mov	ax, RV_Word
	mov	Heads, ax
	jmp	short GR_Ret
Stch_S: 		
	cmp	ax, offset Switch_S
	jne	Stch_C	
	or	Switches, FLAGSECLIM
	mov	ax, RV_Word
	mov	Slim, ax
	jmp	short GR_Ret
Stch_C: 		
	cmp	ax, offset Switch_C
;	 jne	 Stch_N 		 ;
	jne	Stch_F	
	or	Switches, fCHANGELINE
	jmp	short GR_Ret
;Stch_N:				 ;
;	 cmp	 ax, offset Switch_N	 ;
;	 jne	 Stch_F 		 ;
;	 or	 Switches, fNON_REMOVABLE  ;
;	 jmp	 GR_Ret 		 ;
Stch_F: 		
	cmp	ax, offset Switch_F
	jne	GR_Not_Found_Ret	;error in SYSPARSE
	or	Switches, FLAGFF
	push	si	
	mov	si, offset F_Choices
	xor	ax, ax	
	mov	al, Item_Tag
	dec	al	
	mov	cl, 5	
	mul	cl	
	add	si, ax	
	mov	al, byte ptr es:[si+1]	;get the result of choices
	mov	ffactor, al		;set form factor
	pop	si	
GR_Ret: 		
	pop	ax			; Restore Synonym ptr
	push	di			; Save di
	push	ax	
	pop	di	
	mov	byte ptr es:[di], ' '   ;We don't have this switch any more.
	pop	di	
	jmp	short Gr_Done_Ret
GR_Not_Found_Ret:
	pop	ax			;adjust stack
GR_Done_Ret:
	ret		
Get_Result	endp


;
; Scans an input line for blank or tab characters. On return, the line pointer
; will be pointing to the next non-blank character.
;
ScanBlanks:
	lodsb
	cmp	al,' '
	jz	ScanBlanks
	cmp	al,9		    ; Tab character
	jz	ScanBlanks
	dec	si
	ret

;
; Gets a number from the input stream, reading it as a string of characters.
; It returns the number in AX. It assumes the end of the number in the input
; stream when the first non-numeric character is read. It is considered an error
; if the number is too large to be held in a 16 bit register. In this case, AX
; contains -1 on return.
;
;GetNum:
;	 push	 bx
;	 push	 dx
;	 xor	 ax,ax
;	 xor	 bx,bx
;	 xor	 dx,dx
;
;next_char:
;	 lodsb
;	 cmp	 al,'0'              ; check for valid numeric input
;	 jb	 num_ret
;	 cmp	 al,'9'
;	 ja	 num_ret
;	 sub	 al,'0'
;	 xchg	 ax,bx		     ; save intermediate value
;	 push	 bx
;	 mov	 bx,10
;	 mul	 bx
;	 pop	 bx
;	 add	 al,bl
;	 adc	 ah,0
;	 xchg	 ax,bx		     ; stash total
;	 jc	 got_large
;	 cmp	 dx,0
;	 jz	 next_char
;got_large:
;	 mov	 ax,-1
;	 jmp	 short get_ret
;
;num_ret:
;	 mov	 ax,bx
;	 dec	 si		     ; put last character back into buffer
;
;get_ret:
;	 pop	 dx
;	 pop	 bx
;	 ret


;
; Processes a switch in the input. It ensures that the switch is valid, and
; gets the number, if any required, following the switch. The switch and the
; number *must* be separated by a colon. Carry is set if there is any kind of
; error.
;
;Check_Switch:
;	 lodsb
;	 and	 al,0DFH	     ; convert it to upper case
;	 cmp	 al,'A'
;	 jb	 err_swtch
;	 cmp	 al,'Z'
;	 ja	 err_swtch
;	 mov	 cl,cs:switchlist    ; get number of valid switches
;	 mov	 ch,0
;	 push	 es
;	 push	 cs
;	 pop	 es			 ; set es:di -> switches
;	 push	 di
;	 mov	 di,1+offset switchlist  ; point to string of valid switches
;	 repne	 scasb
;	 pop	 di
;	 pop	 es
;	 jnz	 err_swtch
;	 mov	 ax,1
;	 shl	 ax,cl		 ; set bit to indicate switch
;	 mov	 bx,cs:switches
;	 or	 bx,ax		 ; save this with other switches
;	 mov	 cx,ax
;	 test	 ax,7cH 	 ; test against switches that require number to follow
;	 jz	 done_swtch
;	 lodsb
;	 cmp	 al,':'
;	 jnz	 reset_swtch
;	 call	 ScanBlanks
;	 call	 GetNum
;	 cmp	 ax,-1		 ; was number too large?
;	 jz	 reset_swtch
;IF iTEST
;	 push	 ax
;	 add	 al,'0'
;	 add	 ah,'0'
;	 mov	 cs:number,ah
;	 mov	 cs:number+1,al
;	 mov	 dx,offset nummsg
;	 call	 message
;	 pop	 ax
;ENDIF
;	 call	 Process_Num
;
;done_swtch:
;	 ret
;
;reset_swtch:
;	 xor	 bx,cx			 ; remove this switch from the records
;err_swtch:
;	 stc
;	 jmp	 short done_swtch

;
; This routine takes the switch just input, and the number following (if any),
; and sets the value in the appropriate variable. If the number input is zero
; then it does nothing - it assumes the default value that is present in the
; variable at the beginning.
;
;Process_Num:
;	 push	 ds
;	 push	 cs
;	 pop	 ds
;	 test	 Switches,cx	     ; if this switch has been done before,
;	 jnz	 done_ret	     ; ignore this one.
;	 test	 cx,flagdrive
;	 jz	 try_f
;	 mov	 drivenumb,al
;IF iTEST
;	 add	 al,"0"
;	 mov	 driven,al
;	 mov	 dx,offset drvmsg
;	 call	 message
;ENDIF
;	 jmp	 short done_ret
;
;try_f:
;	 test	 cx,flagff
;	 jz	 try_t
;	 mov	 ffactor,al
;IF iTEST
;	 add	 al,"0"
;	 mov	 ffnum,al
;	 mov	 dx,offset ffmsg
;	 call	 message
;ENDIF
;
;try_t:
;	 cmp	 ax,0
;	 jz	 done_ret	     ; if number entered was 0, assume default value
;	 test	 cx,flagcyln
;	 jz	 try_s
;	 mov	 cyln,ax
;IF iTEST
;	 mov	 dx,offset cylnmsg
;	 call	 message
;ENDIF
;	 jmp	 short done_ret
;
;try_s:
;	 test	 cx,flagseclim
;	 jz	 try_h
;	 mov	 slim,ax
;IF iTEST
;	 mov	 dx,offset slimmsg
;	 call	 message
;ENDIF
;	 jmp	 short done_ret
;
;; Switch must be one for number of Heads.
;try_h:
;	 test	 cx,flagheads
;	 jz	 done_ret
;	 mov	 heads,ax
;IF iTEST
;	 add	 al,"0"
;	 mov	 hdnum,al
;	 mov	 dx,offset hdmsg
;	 call	 message
;ENDIF
;
;done_ret:
;	 pop	 ds
;	 ret

;
; SetDrvParms sets up the recommended BPB in each BDS in the system based on
; the form factor. It is assumed that the BPBs for the various form factors
; are present in the BPBTable. For hard files, the Recommended BPB is the same
; as the BPB on the drive.
; No attempt is made to preserve registers since we are going to jump to
; SYSINIT straight after this routine.
;
SetDrvParms:
	push	cs
	pop	es
	xor	bx,bx
	call	SetDrive		; ds:di -> BDS
	mov	bl,cs:[ffactor]
	mov	byte ptr [di].bds_formfactor,bl   ; replace with new value
formfcont:
	mov	bl,[di].bds_formfactor
	cmp	bl,ff48tpi
	jnz	Got_80_cyln
IF iTEST
	mov	dx,offset msg48tpi
	call	message
ENDIF
	mov	cx,40
	mov	cs:[cyln],cx
Got_80_cyln:
	shl	bx,1			; bx is word index into table of BPBs
	mov	si,ds:word ptr BPBTable[bx]	; get address of BPB
Set_RecBPB:
	add	di,BDS_RBPB		; es:di -> Recommended BPB
	mov	cx,size A_BPB - 12	; don't move whole thing!!!!!
	cld
	repe	movsb

	call	Handle_Switches 	; replace with 'new' values as
					; specified in switches.
;
; We need to set the media byte and the total number of sectors to reflect the
; number of heads. We do this by multiplying the number of heads by the number
; of 'sectors per head'. This is not a fool-proof scheme!!
;
	mov	ax,[di].BDS_RBPB.BPB_TOTALSECTORS  ; this is OK for two heads
	sar	ax,1			; ax contains # of sectors/head
	mov	cx,[di].BDS_RBPB.BPB_HEADS
	dec	cl			; get it 0-based
	sal	ax,cl
	jc	Set_All_Done_BRG	; We have too many sectors - overflow!!

	mov	[di].BDS_RBPB.BPB_TOTALSECTORS,ax
	cmp	cl,1

; We use media descriptor byte F0H for any type of medium that is not currently
; defined i.e. one that does not fall into the categories defined by media
; bytes F8H, F9H, FCH-FFH.

	JE	HEAD_2_DRV

	MOV	AL, 1				;1 sector/cluster
	MOV	BL, [DI].BDS_RBPB.BPB_MEDIADESCRIPTOR
	CMP	BYTE PTR [DI].bds_formfactor, ffOther
	JE	GOT_CORRECT_MEDIAD
	MOV	CH, BYTE PTR [DI].bds_formfactor
	CMP	CH, ff48tpi
	JE	SINGLE_MEDIAD
	MOV	BL, 0F0h
	JMP	short GOT_CORRECT_MEDIAD

Set_All_Done_BRG:
	jmp	short Set_All_Done

SINGLE_MEDIAD:
	CMP	[DI].BDS_RBPB.BPB_SECTORSPERTRACK, 8	;8 SEC/TRACK?
	JNE	SINGLE_9_SEC
	MOV	BL, 0FEh
	JMP	short GOT_CORRECT_MEDIAD

SINGLE_9_SEC:
	MOV	BL, 0FCh
	JMP	short GOT_CORRECT_MEDIAD

HEAD_2_DRV:
	MOV	BL, 0F0h		;default 0F0h
	MOV	AL, 1			;1 sec/cluster
	CMP	BYTE PTR [DI].bds_formfactor, ffOther
	JE	GOT_CORRECT_MEDIAD
	CMP	BYTE PTR [DI].bds_formfactor, ff48tpi
	JNE	NOT_48TPI
	MOV	AL, 2
	CMP	[DI].BDS_RBPB.BPB_SECTORSPERTRACK, 8	;8 SEC/TRACK?
	JNE	DOUBLE_9_SEC
	MOV	BL, 0FFh
	JMP	short GOT_CORRECT_MEDIAD

DOUBLE_9_SEC:
	MOV	BL, 0FDh
	JMP	short GOT_CORRECT_MEDIAD

NOT_48TPI:
	CMP	BYTE PTR [DI].bds_formfactor, ff96tpi
	JNE	NOT_96TPI
	MOV	AL, 1			;1 sec/cluster
	MOV	BL, 0F9h
	JMP	short GOT_CORRECT_MEDIAD

NOT_96TPI:
	CMP	BYTE PTR [DI].bds_formfactor, ffSmall	;3-1/2, 720kb
	JNE	GOT_CORRECT_MEDIAD	;Not ffSmall. Strange Media device.
	MOV	AL, 2			;2 sec/cluster
	MOV	BL, 0F9h


Got_Correct_Mediad:
	mov	[di].BDS_RBPB.BPB_SECTORSPERCLUSTER,al
	mov	[di].BDS_RBPB.BPB_MEDIADESCRIPTOR,bl
; Calculate the correct number of Total Sectors on medium
	mov	ax,[di].bds_ccyln
	mov	bx,[di].BDS_RBPB.BPB_HEADS
	mul	bx
	mov	bx,[di].BDS_RBPB.BPB_SECTORSPERTRACK
	mul	bx
; AX contains the total number of sectors on the disk
	mov	[di].BDS_RBPB.BPB_TOTALSECTORS,ax
;J.K. For ffOther type of media, we should set Sec/FAT, and # of Root directory
;J.K. accordingly.
	cmp	byte ptr [di].bds_formfactor, ffOther  ;
	jne	Set_All_Ok	
	xor	dx, dx		
	dec	ax				; TOTALSECTORS - 1.
	mov	bx, 3				; Assume 12 bit fat.
	mul	bx				;  = 1.5 byte
	mov	bx, 2		
	div	bx		
	xor	dx, dx		
	mov	bx, 512 	
	div	bx		
	inc	ax		
	mov	[di].BDS_RBPB.BPB_SECTORSPERFAT, ax
	mov	[di].BDS_RBPB.BPB_ROOTENTRIES, 0E0h	; directory entry # = 224
Set_All_Ok:			
	clc
Set_All_Done:
	RET

;
; Handle_Switches replaces the values that were entered on the command line in
; config.sys into the recommended BPB area in the BDS.
; NOTE:
;	No checking is done for a valid BPB here.
;
Handle_Switches:
	call	setdrive		; ds:di -> BDS
	test	cs:switches,flagdrive
	jz	done_handle		    ; if drive not specified, exit
	mov	al,cs:[drivenumb]
	mov	byte ptr [di].bds_drivenum,al
;	 test	 cs:switches,flagcyln
;	 jz	 no_cyln
	mov	ax,cs:[cyln]
	mov	[di].bds_ccyln,ax
no_cyln:
	test	cs:switches,flagseclim
	jz	no_seclim
	mov	ax,cs:[slim]
	mov	[di].BDS_RBPB.BPB_SECTORSPERTRACK,ax
no_seclim:
	test	cs:switches,flagheads
	jz	done_handle
	mov	ax,cs:[heads]
	mov	[di].BDS_RBPB.BPB_HEADS,ax
done_handle:
	RET


Show_Message	proc	near
;In) AX = message number
;    DS:SI -> Substitution list if necessary.
;    CX = 0 or n depending on the substitution number
;    DH = -1 FOR UTILITY MSG CLASS, 2 FOR PARSE ERROR
;Out) Message displayed using DOS function 9 with no keyboard input.
	push	cs
	pop	ds
	mov	bx, -1
	mov	dl, 0		;no input
	call	SYSDISPMSG
	ret	
Show_Message	endp

;
; The following are the recommended BPBs for the media that we know of so
; far.

; 48 tpi diskettes

BPB48T	DW	512
	DB	2
	DW	1
	DB	2
	DW	112
	DW	2*9*40
	DB	0FDH
	DW	2
	DW	9
	DW	2
	DW	0

; 96tpi diskettes

BPB96T	DW	512
	DB	1
	DW	1
	DB	2
	DW	224
	DW	2*15*80
	DB	0F9H
	DW	7
	DW	15
	DW	2
	DW	0


; 3 1/2 inch diskette BPB

BPB35	DW	512
	DB	2
	DW	1			; Double sided with 9 sec/trk
	DB	2
	DW	70h
	DW	2*9*80
	DB	0F9H
	DW	3
	DW	9
	DW	2
	DW	0

bpb35h	dw	0200h
	db	01h
	dw	0001h
	db	02h
	dw	0e0h
	dw	0b40h
	db	0f0h
	dw	0009h
	dw	0012h
	dw	0002h
	dd	0
        dd      0


bpb288	dw	0200h
	db	02h
	dw	0001h
	db	02h
	dw	240
	dw	2*36*80
	db	0f0h
	dw	0009h
	dw	36
	dw	0002h
	dd	0
        dd      0


BPBTable    dw	    BPB48T		; 48tpi drives
	    dw	    BPB96T		; 96tpi drives
	    dw	    BPB35		; 3.5" drives
; The following are not supported, so we default to 3.5" layout
	    dw	    BPB35		; Not used - 8" drives
	    dw	    BPB35		; Not Used - 8" drives
	    dw	    BPB35		; Not Used - hard files
	    dw	    BPB35		; Not Used - tape drives
	    dw	    bpb35h		; 3.5" - 1.44 MB diskette
	    dw	    BPB35		; ERIMO
	    dw	    bpb288		; 2.88 mb diskette

switchlist  db	7,"FHSTDCN"         ; Preserve the positions of N and C.

; The following depend on the positions of the various letters in SwitchList

flagdrive   equ     0004H
flagcyln    equ     0008H
flagseclim  equ     0010H
flagheads   equ     0020H
flagff	    equ     0040H

;
;Equates for message number
NODRIVE_MSG_NUM   equ	2
LOADOK_MSG_NUM	  equ	3

code ends

end

