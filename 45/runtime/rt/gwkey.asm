	PAGE	56,132
	TITLE	GWKEY - SUPPORT FOR SOFT KEY ASSIGNMENT
;***
; GWKEY - SUPPORT FOR SOFT KEY ASSIGNMENT
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;      KEY n,x$
;      --------
;	   |
;	 $KY1
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	useSeg	EV_TEXT 	
	useSeg	_DATA		
	useSeg	_BSS		

	INCLUDE seg.inc 	; segment definitions
	INCLUDE baslibma.inc
	INCLUDE event.inc	; oem evt routines parameters
	INCLUDE string.inc	

	SUBTTL	local constant definitions	
	page

;Number of keys 		
	OrgFky		= 10	; normally, there is 10 function keys
	CsrMovKy	= 4	; four cursor movement keys

	NumF10		= 10	; # of F10
	NumF11		= 30	; # of F11
	NumF12		= 31	; # of F12

sBegin	_DATA			
	externW	b$vKEYDSP	; vector for indirect call of B$KEYDSP
sEnd	_DATA			

sBegin	_BSS			
	externW b$STRTAB	;defined in GWDATA.ASM
	externB b$KEY_SW
sEnd	_BSS			

sBegin	EV_TEXT 		

assumes CS,EV_TEXT		

	externNP B$ERR_FC	
	externFP B$FMID 	; MID$ function
	externFP B$SASS 	
	externNP B$STDALCTMP	; deallocate temp string
	externNP B$RDKYBD


;***
; B$KMAP - Soft Key Assignment
;
;Purpose:
;
;	Runtime Entry Point
;	Syntax:
;	      KEY<n>,<string>
;
;	Where:
;
;	<n>	is an integer expression returning a result in
;		the range 1 to 10 (or 30,31).  Other values will result
;		in an "Illegal Function Call" Error.
;
;	<string>is any string expression.  Only the 1st 15 characters
;		are assigned as the Soft Key value.  If a null string
;		is assigned, the Soft Key is effectively disabled.
;		In this case, INKEY$ returns a 2 byte string.
;		B$TTYIN returns 0, and the Line Editor complains.
;		if FK_KANJI then if 15th byte is the beginning of a Kanji char
;				then substitute a blank and ignore second half
;
;
;Entry:
;
;	keynum	= Key number (1 to 10)
;	string	= SD of assignment string
;
;Exceptions:
;	B$ERR_FC if bad key number.
;
;******************************************************************************
cProc	B$KMAP,<PUBLIC,FAR>	
parmW	keynum			; key number
parmSD	string			;string to assign
cBegin				
	MOV	AX,keynum	; get key number
	GetpSD	BX,string	;and string descriptor address
	CMP	AL,OrgFky	; if not one of F1-F10 then
	JA	check_user	;[7]  check for user-defined key or F11, F12
SoftKeys:			; if F11 & F12 continue from here
	DEC	AX		
	JS	ERRFC		; Jump if invalid
	ADD	AX,AX		
	ADD	AX,AX		; [AX] = (4 * Key number).
	ADD	AX,OFFSET DGROUP:b$STRTAB ;Index into Soft Key table
	PUSH	AX		;Save Soft Key Table addr
	MOV	DX,1
	MOV	CX,15		;String may be 0 to 15 chars.


	cCall	B$FMID,<BX,DX,CX>	; Get a String 0 to 15 chars long
	POP	DX		; [DX] = softkey table address
	cCall	B$SASS,<AX,DX>	; in the Soft Key Table
	TEST	b$KEY_SW,1	;test if key display is on
	JZ	KMAP_90 	;if not, just jump
	CALL	[b$vKEYDSP] 	; Display new Keys
	JMP	SHORT KMAP_90	
;
; BASICA32 doesn't check whether the ronco is present when assigning the key
; definition, so is here.
;
F11F12:
	SUB	AX,NumF11-NumF10-1 ; F11, F12 are next to F10 in b$STRTAB
	JMP	SHORT SoftKeys	; process as one of function keys
;
ERRFC:	JMP	B$ERR_FC	; complain..
;
; Check for user defined key definition
;
check_user:
	CMP	AL,NUM_TKEYS+1	; beyond the range ?
	JNB	ERRFC		; Brif yes
	cmp	AL,OrgFky+CsrMovKy+1 ; is it a soft key? (10 < key# < 14)
	jb	ERRFC		; yes - (can't define crsr movement keys)
	CMP	AL,NumF11	; is F11 ? (BX is 0-relative)
	JZ	F11F12		; Brif yes
	CMP	AL,NumF12	; is F12 ? (current last soft key is F12)
	JZ	F11F12		; Brif yes
	CMP	AL,NumF11-NUM_GAP+1 ; gap between the last user key and F11
	jnb	ERRFC		; no -	complain
	XCHG	DX,AX		; [DX] = 1-offset key num
	push	bx		; save psd
	mov	cx,[bx] 	; CX has length of string
	mov	bx,[bx+2]	; BX -> text
	mov	al,define_key
	call	B$RDKYBD 	; set the trap
	pop	bx		; retrieve psd
	jc	ERRFC		; oem didn't like that
	CALL	B$STDALCTMP	; Delete if temporary string

KMAP_90:

cEnd				

sEnd	EV_TEXT

	END
