	TITLE	LLCEVT - Core GW-BASIC Event Interface
	PAGE	56,132
;***
; LLCEVT - Core GW-BASIC Event Interface
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;	This module has the CORE EVENT support routines:
;
;	1. B$RDKYBD
;	2. B$KBDTRP....This is the keyboard interrupt handler. All keys
;			are filtered through this interrupt routine.
;			In case the key hit is not a trappable key
;			it is allowed to pass through the ROM keyboard
;			interrupt handler.
;
;	The following is the relationship of several tables used in this
;	module.  TRTBL2, USRTBL2 & TRTBL4 are the table of flags, and
;	TRTBL1 & USRTBL1 are the tables of definitions of keys (either
;	the scan code or user's definitions).  In order to support the
;	IBM advanced 101-key keyboard (ronco keyboard), there are four
;	entries paded after USRTBL2.  This is for the convenience of
;	table search for key number.
;
;	The definition of the flag, please refer the comments for
;	PPBEBL below.
;
;	TRTBL2 -->|--|	TRTBL1 -->|--| <-- Scan code of F1-F10
;		  |--|		  |--|		(10 keys)
;	(flags) ->|--|		  |--|
;		  \  \  	  \  \
;		  \  \  	  \  \
;		  |--|		  |--|
;		  |--|		  |--| <-- Scan code of cursor move key
;		  \  \  	  \  \  	(4 keys)
;		  \  \  	  \  \
;	USRTBL2 ->|--|	USRTBL1 ->|--|--| <-- user key definition
;		  |--|		  |--|--|	(11 keys)
;		  \  \  	  \  \  \
;		  \  \  	  \  \  \
;		  |--|		  |__|__|
;	(dummy) ->|--|
;	(0,4 keys)\  \  		(no corresponding table)
;		  \  \
;	TRTBL4 -->|--|			(no corresponding table)
;		  |--|
;		  |__|
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	

	useSeg	_BSS		
	useSeg	CONST		
	useSeg	_DATA		
	useSeg	EV_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE ibmunv.inc
	INCLUDE idmac.inc	
	INCLUDE intmac.inc
	INCLUDE oscalls.inc	
	INCLUDE baslibma.inc	
	INCLUDE event.inc	
	INCLUDE const.inc	

	SUBTTL	local constant definitions	
	page

;Number of keys 		
	OrgFky		= 10	; normally, there is 10 function keys
	CsrMovKy	= 4	; four cursor movement keys
	ExtFky		= 2	; extended function keys (F11/F12)

;Scan codes			
	ScanPrtsc	= 37H	; scan code for Prtsc
	ScanPause	= 45H	; scan code for Pause
	ScanBreak	= 46H	; scan code for Break
	ScanF11 	= 57H	; scan code for F11
	ScanF12 	= 58H	; scan code for F12
	ScanExt1	= 0E0H	; scan code for extended keys
	ScanExt2	= 0E1H	; scan code for extended keys

	CTRLDown	= 04H	; bit for CTRL been pressed
	ALTDown 	= 08H	; bit for ALT been pressed


	SUBTTL	data definitions
	page

sBegin	_DATA			

externB b$IOFLAG		
externW b$IPOLKEY		
externB b$CtrlFlags		
staticB PPBEBL,07H		;This byte is used in the following way:
				;Bit 0 used for CTRL BRK enable/disable
				;Bit 1 used for CTRL PAUSE enb/disb
				;Bit 2 used for CTRL PRTSC enb/disb
				;ENABLE=1 and DISABLE=0

staticB TRTBL2,4,OrgFky+CsrMovKy; validate the first 14 keys (*)
staticB USRTBL2,0,NUM_UKEYS	; invalidate NUM_UKEYS user defined keys (*)

; Constants used in TRTBL2 and USRTBL2
TRP_OCCURED	EQU	1	; Bit 0 indicates occurence of a trap
TRP_ENABLED	EQU	2	; Bit 1 indicates trapping enb/disb
TRP_VALID	EQU	4	; Bit 2 indicates def valid/invalid

staticB ,0,NUM_GAP		; there is a gap between the last user
				;  defined key and F11
staticB TRTBL4,4,ExtFky 	; flags for F11/F12, NOTE: TRTBL2, USRTBL2
				;  and TRTBL4 has to be contiguous and in
				;  this order
sEnd	;_DATA			

sBegin	CONST			

staticB TRTBL1,<59,60,61,62,63>
staticB ,<64,65,66,67,68>	; 10 function keys
staticB ,<72,75,77,80>		; 4 cursor control keys
				; currently, there is no need to use a
				;  table for F11/F12, since there are only
				;  two of them.  Within the code, it checks
				;  the scan code of F11/F12 directly.  But
				;  the trap flag table, TRTBL4, is needed.
staticB SHFTBL,<52H,3AH,45H,46H>; used to check for shift keys in SHKEYS
staticB ,<38H,1DH,2AH,36H>	; The last 3 entries must remain last!


sEnd	;CONST			

sBegin	_BSS			

staticB WasE0E1,0		; the value will be either 00H or 80H, to
				;  indicate the extended keys

	externB b$NetCard	; defined in LLINI.ASM
				; b$NetCard=1 --> network installed and
				; running on 3.xx
				; b$NetCard=0 --> network not installed

staticW USRTBL1,,NUM_UKEYS	; NUM_UKEYS user defined keys
staticW KEYHIT,,1		
staticB LPTFLG,,1		; line printer echo key flag
staticB POSFLG,,1		; pause key flag
staticB BRKFLG,,1		; break key flag
staticB KEYFLG,,1		; trapped key flag
staticB KEYTRP,,1		; key trapping enable/disable flag
staticB WASPOS,,1		; helps in handling the CTRL PAUSE key.
				; A value of 0 indicates that the CTRL
				; PAUSE is not active, whereas a value
				; of 0FFH indicates that the CTRL PAUSE
				; is active. (used in B$KBDTRP & B$RDKYBD)

externB	b$EventFlags		; misc event flags

sEnd	;_BSS			


	SUBTTL	code externals	
	page

externFP B$IBreak		;interp call back to notify of CTRL-BREAK


sBegin	EV_TEXT 		
assumes CS,EV_TEXT		

	externNP B$TrapEvent	
	externNP B$TestTrap	
	externNP B$ReqTrap	
	externNP B$Wakeup	
	externNP B$GETDS	

	SUBTTL	B$RDKYBD - Keyboard control routine
	PAGE
;***
;B$RDKYBD - Support for Keyboard Events
;OEM-interface routine
;
;Purpose:
;	This routine allows the caller to do one of the following:
;	(1) Check whether the key trap happened.
;	(2) Define the user definable trap keys. The key definition syntax
;		specifies a string.This string is passed directly to B$RDKYBD.
;	(3) Start trapping keys.
;	(4) Stop trapping keys.
;	(5) Enable trapping for a specific key#
;	(6) Disable trapping for specific key#
;
;	See the documentation for B$POLLEV for a description of the
;	event and trapping mechanisms.
;
;	The string that is passed in as a parameter in function number
;	1 is directly from the user.  It has to be checked for proper
;	length and contents before it can be used.  In this implementation,
;	the length of this string must be 2.  If the string is incorrect,
;	return with PSW.C set and an ILLEGAL FUNCTION CALL will be
;	generated.
;
;Entry:
;	[AL] = 0: Return in [BX]
;			0 if no keyboard trap occurred
;			key number if a trap key was detected
;			-1 if ctrl-break was encountered
;			-2 if pause was encountered
;			-3 if LPTECHO was encountered
;	       1: define a user definable key for trapping
;			[BX] = address of the key definition string
;			[CX] = length of the key definition string
;			[DX] = key number
;	       252: start trapping keys
;	       253: stop trapping keys
;	       254: enable trapping for key # [DX]
;			Key number range is [-3,(max key index)]
;			Key numbers are one relative, Key 0 not used.
;	       255: disable trapping for key # [DX]
;			Key number range is [-3,(max key index)]
;			Key numbers are one relative, Key 0 not used.
;	[BX] = value as specified by function [AL]
;	[CX] = value as specified by function [AL]
;	[DX] = value as specified by function [AL]
;
;Exit:
;	[BX] = value, if specified by function [AL]
;	[DX] = value, if specified by function [AL]
;	PSW.C set will cause a function call error
;
;Uses:
;	Per Convention
;
;Exceptions:
;	None.
;******************************************************************************
;
;ALGORITHM:
;
;	case [AL] of
;		0: begin
;			if LPTFLG then
;			    begin
;			       set [BX] to -3
;			       clear LPTFLG
;			    end
;			if (any trap key flag set ) then
;			      begin
;				set [BX] to trap key #
;				clear corr. trap key flag
;			      end
;			else
;			    if POSFLG then
;				begin
;				  set [BX] to -2
;				  clear POSFLG
;				end
;			    else
;				if BRKFLG then
;				  begin
;				    set [BX] to -1
;				    clear BRKFLG
;				  end
;				else
;				   set [BX] to 0
;		   end
;		1: begin
;		     if (( [CX] < 1 ) or ( [CX] > 2 )) then
;			set carry  to indicate function
;			call error
;		     else
;			redefine key # [DX] using
;			the address in [BX]
;		   end
;		252: enable
;		253: clear enable flag for all trappable keys
;		254: set enable flag for
;		      key # [DX]
;		255: clear enable flag for
;		      key # [DX]
;		otherwise : set carry to indicate error
;			    in function call
;	endcase
;
;DATA STRUCTURE:
;	The trap table has NUM_TKEYS rows, each row made up of
;	2 bytes. The two bytes are required for the definition
;	of a key string. There is another table having 20 rows
;	each row having a byte. This byte is used to store
;	three flags, namely
;	   1. Definition valid or not
;	   2. Key enabled or not
;	   3. Trap occurred or not
;	The mask is defined as follows:
;	   Bit 0 corresponds to the occurence of a key trap.
;	   Bit 1 corresponds to key Enable/Disable.
;	   Bit 2 corresponds to key definition validation.
;
;#****

cProc	B$RDKYBD,<NEAR,PUBLIC>,ES 
cBegin				


	PUSH	DS		
	POP	ES		
	CALL	B$SetKybdInt	; install keyboard handler if not already
				; installed
	OR	AL,AL		;test if key trapping status
	JNZ	RDK1A		;if not, then process normally
	XOR	BX,BX		;preset BX to zero
	XCHG	BL,KEYFLG	;swap zero and key trapping flag
	OR	BX,BX		;test if flag was set
RDK1A:
	PUSH	SI
	push	ax		; save [ax]
	JZ	RDKRET		; if flag was not set, then don't process it
	ADD	AL,4		;[7] check if [AL] is within limits
	JS	RDKERR
	CMP	AL,6		; no LOC/LOF support (one less entry)
	JNB	RDKERR
RDK2:
	CBW			; since all positive, save one byte
	SHL	AX,1
	MOV	SI,AX		;[SI] = word index into table
	PUSH	ES
	PUSH	CX
	PUSH	DI
	PUSH	DS		
	POP	ES		; make ES=DS
	CALL	CS:RDKOFST[SI]
	POP	DI
	POP	CX
	POP	ES
	JMP	SHORT	RDKRET
RDKERR:
	STC
RDKRET:
	pop	ax		; restore [ax]
	POP	SI

cEnd				


RDKOFST DW	TRPEBL
	DW	TRPDIS
	DW	ENABL
	DW	DISABL
	DW	KEYRTN
	DW	KEYDEF

	SUBTTL	B$SetKybdInt - Install keyboard handler
	PAGE
;***
;B$SetKybdInt, B$InstKybdInt - install keyboard interrupt handler
;OEM-Interface Routine
;
;PURPOSE:
;	These routines install the keyboard interrupt service
;	routine. B$SetKybdInt checks to see if the routine has already
;	been installed and returns without doing anything if it has
;	been.  B$InstKybdInt does not check if there is already a keyboard
;	servrice routine.
;
;Preserves:
;	AX
;
;********************************************************************
cProc	B$SetKybdInt,<NEAR,PUBLIC>
cBegin

	TEST	b$EventFlags,KybdInst ; keyboard service routine installed ?
	JZ	B$InstKybdInt 	; brif not -- install it
	RET			; otherwise, just return

labelNP	<B$InstKybdInt>		


	CLI			;clear interrupts
	PUSH	DS
	PUSH	DX
	PUSH	AX
	PUSH	CS
	POP	DS		;[DS] := [CS]
	SETVEC	KYBINT,B$KBDTRP ;install new keyboard service routine
	POP	AX
	POP	DX
	POP	DS
	OR	b$EventFlags,KybdInst		; set init flag
	STI			;restore interrupts


cEnd				

;**
;This routine returns the key that was most
;recently struck. Actually it returns a value
;in [BX] corresponding to the key struck.

KEYRTN:
	CMP	LPTFLG,1	;was it the line printer echo?
	JNE	TRPKYS		;no, check for the trap keys
	MOV	LPTFLG,0	;clear the printer echo flag
	MOV	BX,-3		;return a -3 in BX
	JMP	SHORT KEYRET
TRPKYS:
	MOV	DI,OFFSET DGROUP:TRTBL2 ;[DI] = offset of trap table 2
	MOV	CX,NUM_TKEYS	; [CX] = count of total function keys
				;  including the gap (which is always 0)
				; this number including the 4 padding
				; entries
	MOV	AL,07H		;[AL] = 00000111B
	CLD			;just to be safe
	REPNZ	SCASB		;search for a trapped key
	JNZ	POSBRK		; Brif none found, the original code used
				;  count=(total number of keys)+1 and JCXZ
				;  here, it was OK.	I use JNZ and count=
				;  (total number of keys) instead.  The
				;  effect is waste one byte, but save one
				;  comparison.
	SUB	CX,NUM_TKEYS	; getting the 1-relative key
	NEG	CX		;number in [CX]
	DEC	DI		;adjust [DI] to point to found key
	AND	BYTE PTR[DI],0FEH ;clear the trap flag for this key
	MOV	BX,CX		;return the key number in [BX]
	JMP	SHORT KEYRET
POSBRK:
	CMP	POSFLG,1	;was it the pause key?
	JNE	CHKBRK		;no, check for BRK
	MOV	POSFLG,0	;yes, clear the pause key flag
	MOV	BX,-2		;return -2 in BX
	NOT	WASPOS		;WASPOS indicates whether
				;CTRL PAUSE is active or not
	JMP	SHORT	KEYRET
CHKBRK:
	CMP	BRKFLG,1	;was it the break key
	JNE	NOKEYS		;no, jump to no keys trapped
	MOV	BRKFLG,0	;clear the break flag
	MOV	BX,-1		;return -1 in BX
	JMP	SHORT	KEYRET
NOKEYS:
	XOR	BX,BX		; return 0 to indicate no keys
				;  and XOR clear the carry flag
	RET			; return with NC & BX=0
KEYRET:
	MOV	KEYFLG,1	;set flag to process again
	CLC			;indicate no error
	RET

;**
;This routine is used to define a user defined
;trap key.
; DI is used. (caller, B$RDKYBD preserves AX,CX,SI,DI)
; BX is preserved.

KEYDEF:
	CMP	CX,2		;is CX = 2 as it should be
	STC			; assume error
	JNE	DEFRET		; Brif CX <> 2, return with CY (error)
	PUSH	BX		; save BX
	MOV	CX,[BX] 	;get the key definition in [CX]
	XCHG	CH,CL		;swap before storing
	TEST	CH,3		; left/right shift been pressed ?
	JZ	NO_SHIFT	; Brif not
	OR	CH,3		; make both the same
NO_SHIFT:
	MOV	DI,DX			; # in DI
	SUB	DI,OrgFky+CsrMovKy+1	; map 15,16,... to 0,1,...
	MOV	BX,OFFSET DGROUP:USRTBL2; [BX] = offset of trap table 2
	OR	BYTE PTR [BX+DI],TRP_VALID  ; validate the new definition
	MOV	BX,OFFSET DGROUP:USRTBL1; [BX] = offset of trap table 1
	SHL	DI,1			; get the word offset
	MOV	WORD PTR[BX+DI],CX	; store the new key definition
	POP	BX		; get back BX
	CLC
DEFRET:
	RET

;**
;enables key trapping

TRPEBL:
	MOV	KEYTRP,1
	CLC			; original code exit via TRPDIS, slower
	RET

;**
;disables key trapping

TRPDIS:
	MOV	KEYTRP,0
	CLC
	RET

;**
;enables trapping for key # [DX]

ENABL:
	PUSH	DX		;save [DX]
	OR	DX,DX		; is [DX]>0 ? NOTE: [DX]=0 isn't taken care
	JG	ENABL1		;Brif so
	NEG	DX		;else complement [DX]
	CMP	DL,3		;if [DL]=3 then [DL]:=4
	JB	ENABL0		;Brif not
	INC	DL		;else make [DL]=4
ENABL0:
	OR	PPBEBL,DL	;enable the corresponding key
				; carry is cleared
	JMP	SHORT ENABL2
ENABL1:
	MOV	DI,OFFSET DGROUP:TRTBL2-1 ; [DI] = (offset of trap table2)-1
	ADD	DI,DX		;get byte offset in table
	OR	BYTE PTR[DI],02H ;enable the key
				; carry is cleared
ENABL2:
	POP	DX		;restore [DX]
	RET			; with NC

;**
;disables trapping for key # [DX]

DISABL:
	PUSH	DX		;save [DX]
	OR	DX,DX		; is [DX]>0 ? NOTE: [DX]=0 isn't taken care
	JG	DISBL1		;Brif so
	DEC	DX		;disable the corresponding key
;Bug fixed April 12,1984
	CMP	DL,0FCH 	;is low nibble of [DL]=1100 ?
	JA	DISBL0		;Brif not
	DEC	DL		;else make it 1011
DISBL0:
	AND	PPBEBL,DL	;leaving the others intact
				; carry is cleared
	JMP	SHORT DISBL2
DISBL1:
	MOV	DI,OFFSET DGROUP:TRTBL2-1 ; [DI] = (offset of trap table2)-1
	ADD	DI,DX		;get byte offset in table
	AND	BYTE PTR[DI],0FDH ;disable the corr. key
				; carry is cleared
DISBL2:
	POP	DX		;restore [DX]
	RET			; with NC

	SUBTTL	B$KBDTRP - Keyboard interrupt / monitor loop
	PAGE
;***
;B$KBDTRP
;
;PURPOSE:
;	The keyboard interrupt handler first branches to
;	this routine to see if any of the following keys
;	were typed in that order:
;	1. The NUM_TKEYS trappable keys including the NUM_UKEYS user
;	   defined trap keys.
;	2. PRTSC key, LPT echo toggle key, Pause key
;	   or the Break key.
;	If any of the above keys were hit, B$KBDTRP sets
;	Event flag and the corresponding key flag and
;	returns.
;	If none of the above keys were hit then the
;	routine branches to the ROM keyboard interrupt
;	handler.
;
;ALGORITHM:
;	if (key typed is the LPTECHO key) then
;	   begin
;		set LPTFLG
;		set EVTFLG and KEYFLG
;	   end
;	If (key typed is a valid trap key for which
;	    trapping is enabled) then
;	   begin
;	      set the corr. TRAP key flag in the TRAPTABLE
;	      set the EVTFLG and KEYFLG
;	   end
;	else
;	     if (key typed is PAUSE key and CTRL PAUSE
;			trapping enabled) then
;		begin
;		  set PAUSE key flag
;		  set EVTFLG and KEYFLG
;		end
;	     else
;		if (key typed is BREAK key and CTRL BREAK
;			trapping enabled) then
;		      begin
;			set BREAK key flag
;			set EVTFLG and KEYFLG
;		      end
;	     else
;		continue with ROM keyboard interrupt
;		handler
;
;DATA STRUCTURE:
;	This routine also uses the TRAP TABLE data
;	structure described above in B$RDKYBD.
;
;ENTRY:
;	None
;
;EXIT:
;	Set the EVTFLG, KEYFLG and the corresponding key
;	flag if any of the enabled trap keys were typed.
;
;MODIFIED:
;	None
;
;***********************************************************************

dbpub	B$KBDTRP			
B$KBDTRP:					

	ENABLE			;enable further interrupts
	PUSH	AX
	PUSH	BX
	PUSH	CX
	PUSH	DS
	PUSH	DI
	PUSH	ES
	XOR	CX,CX
	MOV	DS,CX		;zero DS
	MOV	BX,KBDFLG	;set BX to special status flag's addr
	MOV	AH,BYTE PTR[BX] ;get the mask in AH
	CALL	B$GETDS
	MOV	DS,BX		;get the addr of basic data seg
	MOV	ES,BX		;make [ES] = [DS]
	IN	AL,KBDATA	;else get keyboard data in acc.
				; when enter here,
				;  [AX]=[shift status|scan code]
	MOV	KEYHIT,AX	; store the key hit
	CMP	AL,ScanExt1	; is extended scan code (E0H) ?
	JZ	SetExtFlg	; Brif yes
	CMP	AL,ScanExt2	; is extended scan code (E1H) ?
	JNZ	SetToggle	; Brif not
SetExtFlg:			
	MOV	WasE0E1,80H	; set extended scan code flag
	JMP	NONE		; let ROM handles it
SetToggle:			
	AND	AH,6FH		;throw away INS & SCROLL (80H & 10H)
				;and continue with the INT handler.
	OR	AH,WasE0E1	; or the flag for extended key
	MOV	WasE0E1,0	; reset extended scan code flag
	TEST	AH,3		; was either right or left shift ?
	JZ	NO_SHFT 	; Brif not
	OR	AH,3		; make them the same
NO_SHFT:

	TEST	b$EventFlags,InSLEEP ; in sleep statement?
	JZ	NoWakeup	; brif not -- nothing to wake up
	OR	AL,AL		; BREAK scan code?
	JS	NoWakeup	; brif so -- don't wake up SLEEP
	CALL	B$Wakeup	; key hit -- force SLEEP statement wakeup
NoWakeup:			

	CALL	CHKLPT		;check for CTRL PRTSC
	CMP	WASPOS,0	;Pause active ?
	JE	NOPAUS		;Brif not
	AND	AL,07FH 	;else strip bit 7
NOPAUS:
	CALL	B$CHKTRP	; check for trap keys
	JC	KBDRET		;Brif found
	CALL	OTHERS		;check for CTRL PAUSE and CTRL BRK
	JC	KBDRET		;Brif found
	JMP	NONE

KBDRET: 			

	MOV	KEYFLG,1	;set flag for key event
	CALL	B$TrapEvent	;set global event flag

	IN	AL,KBDATA+1	;get keyboard control port
	MOV	AH,AL		;save it in AH
	OR	AL,80H		;keyboard reset bit
	PAUSE			;make sure instruction fetch has occurred
	OUT	KBDATA+1,AL	;Ack so keyboard can Int again
	MOV	AL,AH		;get control bits back
	PAUSE			;make sure instruction fetch has occurred
	OUT	KBDATA+1,AL	;restore keyboard mode
	MOV	AL,EOI		;send End of Interrupt
	PAUSE			;make sure instruction fetch has occurred
	OUT	INTA0,AL	;to 8259
	STC			;carry since we processed char
ROMINT:
				;CF indicates whether we processed
				;character or not
	POP	ES
	POP	DI
	POP	DS
	POP	CX
	POP	BX
	POP	AX
	JC	REALRT
	INT	ROMKBD
REALRT:
	IRET

	SUBTTL
	PAGE
;**
;This routine checks for CTRL PRTSC
;and if CTRL PRTSC is down it sets LPTFLG
;and the event flags

CHKLPT:
	TEST	AH,CTRLDown	; CTRL down ?
	JZ	LPTRET		;Brif not
	CMP	AL,ScanPrtsc	; was it the PRTSC key ?
	JNE	LPTRET		;Brif not
	TEST	PPBEBL,04H	;PRTSC key trapping enabled ?
	JZ	LPTRET		;Brif not
	MOV	LPTFLG,1	;else set printer echo flag
	MOV	KEYFLG,1	;set flag for key event
	CALL	B$TrapEvent	;set the global event flag
LPTRET:
	RET


;**
;This routine checks for any of the
;NUM_TKEYS trappable keys and if found sets
;the trap flag for the corresponding key

dbpub	B$CHKTRP		
B$CHKTRP:			
	CMP	KEYTRP,1	;key trapping enabled ?
	JNE	NoTrap		;Brif not
				; check F11/F12, or F1-F10 & cursor movement
				;  keys, and then user defined keys
	XOR	CX,CX		; CX=0, in case it is F11
	CMP	AL,ScanF11	; is F11/F12
	JB	SetTbl1 	; Brif not
	MOV	BX,OFFSET DGROUP:TRTBL4 ; [BX]=offset of trap table 4
	JZ	IsTrapable	; Brif F11, TRAPPED need BX & CX
	DEC	CX		; CX=-1, TRAPPED need negative 0-relatived #
	CMP	AL,ScanF12	; is F12 ?
	JZ	IsTrapable	; Brif yes, TRAPPED need BX & CX
	JMP	SHORT UsrKeys	; search user defined keys
SetTbl1:			
	TEST	AH,80H		; is new set of cursor moving keys ?
	JNZ	UsrKeys 	; Brif yes
	MOV	DI,OFFSET DGROUP:TRTBL1 ; [DI]=offset of trap table 1
	MOV	CX,OrgFky+CsrMovKy	; [CX]=count of keys
	MOV	BX,OFFSET DGROUP:TRTBL2 ; [BX]=offset of trap table 2
	CLD			;just to be safe
	REPNZ	SCASB		;search for a trappable key
	JNZ	UsrKeys 	; Brif none found
	SUB	CX,OrgFky+CsrMovKy-1	; get negative 0-relative key
IsTrapable:			
	CALL	TRAPPED 	; trappable ?
	JC	SHORT TrpExit	; is a trapped key, return with CY
UsrKeys:			
	MOV	DI,OFFSET DGROUP:USRTBL1;[DI]=offset of user trap_table 1
	MOV	CX,NUM_UKEYS	; [CX]=count of keys
	MOV	BX,OFFSET DGROUP:USRTBL2;[BX]=offset of user trap_table 2
	CLD			; just to be safe
Loop2:
	REPNZ	SCASW		;search for a trappable key
	JNZ	NoTrap		; Brif none found
	SUB	CX,NUM_UKEYS-1	; get the negative 0-relative key #
	PUSH	BX		; save BX for next run, BX is needed by
				;  TRAPPED and is ruined in it
	CALL	TRAPPED 	; trappable ? on return CX=positive
				;  0-relative key #, CY indicate the key is
				;  trapable, ZR or NC indicate not trapable
	POP	BX		; get BX back
	JC	TrpExit 	; exit with carry set if found
	SUB	CX,NUM_UKEYS-1	; get back count in [CX]
	JCXZ	NoTrap		; if zero, then done, no trap key found
	NEG	CX
	JMP	SHORT Loop2	; look for other trappable keys
NoTrap:
	CLC			;CLC to indicate none found
TrpExit:			; exit
	RET

TRAPPED:			; this routine returns with
				; CF set if the key is trappable
				; else returns with Z flag set & NC
	NEG	CX		;number
	ADD	BX,CX		;get byte offset into table
	TEST	BYTE PTR[BX],TRP_VALID ; key valid?
	JZ	NOTFOUND	; Brif not -- return with carry clear
	TEST	BYTE PTR[BX],TRP_ENABLED ; key trapping enabled?
	JZ	NOTFOUND	; Brif not -- return with carry clear
	OR	BYTE PTR[BX],TRP_OCCURED ; else set the trap key flag
GotKeyBreak:			
	STC			;STC to indicate that a trappable
NOTFOUND:
	RET			;key was found
;**
;This routine checks for CTRL PAUSE and CTRL BRK
;in that order and if found returns with carry set
;else returns with carry clear.

OTHERS:
	TEST	AH,CTRLDown	; CTRL down ? (carry reset NC)
	JZ	NOTFND		;Brif not
	CMP	AL,ScanPause	; was it CTRL PAUSE ?
	JNE	BRKCHK		;Brif not
	TEST	AH,ALTDown	; was ALT also down ?
	JNZ	OTHRET		;EAT CTRL ALT PAUSE
	TEST	PPBEBL,02H	;PAUSE key trapping enabled ?
	JZ	BRKCHK		;Brif not
	CMP	WASPOS,0	;CTRL PAUSE active ?
	JZ	SETPOS		;Brif not (^S not active, so treat
				;this as ^S)
	JMP	SHORT OTHRET	;else just eat the character
SETPOS:
	MOV	POSFLG,1	;else set the PAUSE key flag
	JMP	SHORT OTHRET	;and return
BRKCHK:
	CMP	AL,ScanBreak	;was it CTRL BRK ?
	JNE	NOTFND		;Brif not
	CMP	b$NetCard,1	; network installed ?
	JNZ	NetNotFound	; brif not,
	TEST	AH,ALTDown	; alt key down ?
	JNZ	NOTFND		; brif so, without eating ctrl-alt-brk
NetNotFound:			
	TEST	AH,ALTDown	; was ALT also down ?
	JNZ	OTHRET		;EAT CTRL ALT BRK
	TEST	PPBEBL,01H	;BREAK key trapping enabled ?
	JZ	NOTFND		;Brif not
	CMP	WASPOS,0	;CTRL PAUSE active ?
	JZ	BRKCH1		;Brif not
	JMP	SHORT SETPOS	;else report this as ^S
BRKCH1:
	MOV	BRKFLG,1	;else set BREAK key flag
	CALL	B$IBreak	;notify interpreter of BREAK

OTHRET:
	STC			;indicates key found
	RET
NOTFND:
	CLC			;indicates key not found
	RET

;**
;Control branches here if no trappable key was
;hit. This routine lets all other keys to go
;through the ROM keyboard interrupt handler
;IF AND ONLY IF CTRL PAUSE is NOT active. If
;CTRL PAUSE is active then it reports that key as
;another CTRL PAUSE if it is NOT any of the SHIFT
;keys.

NONE:

	CMP	WASPOS,0	;CTRL PAUSE active ?
	JZ	NONE2		;Brif not
	CALL	SHKEYS		;else check for shift keys
	JC	NONE1		;Brif it is a shift key
	MOV	POSFLG,1	;else report it as CTRL PAUSE
	JMP	KBDRET		;set EVTFLG and return
NONE1:
	CMP	CX,3		; was it CTRL, LEFT-SHIFT or RIGHT-SHIFT?
	JBE	NONE2		; brif so -- let it go through
	JMP	KBDRET		;EAT ALL OTHER SHIFT KEYS
NONE2:				;comes here if CTRL PAUSE not
				;active and no trapping occured
	CLC			;clear carry
	MOV	AX,KEYHIT	; restore key hit
	JMP	ROMINT		;pass control to ROM INTERRUPT
				;i.e. pass the key to the input stream


;******************************************************
;SHKEYS is used to find out if the key hit
;is one of the following:
;	INS, CAPSLOCK, NUMLOCK, SCROLLLOCK, ALT,
;	CTRL, LEFTSHIFT, RIGHTSHIFT.
;	If the key hit is one of the above it returns
;	with carry set else returns with carry clear.
;	Also if it is the CTRL key it returns the
;	value 3 in [CX]. If PAUSE is active then typing
;	any of these keys will not release pause, and
;	except for CTRL all other shift keys are EATEN
;	by B$KBDTRP.
;******************************************************
SHKEYS:				; AL = scan code
	MOV	DI,OFFSET DGROUP:SHFTBL
	MOV	CX,9		;count of 8 keys
	CLD			;to be safe
	REPNZ	SCASB		;search for a shift key
	CLC			; clear carry
	JCXZ	SHKRET		; Brif none found
	STC
SHKRET:
	RET
	PAGE
;***
;B$HookKbd - Hook keyboard interrupt
;
;Purpose:
;	Added with revision [13].
;	The QB4 user interface needs to have control of the
;	keyboard interrupt when it is active to avoid conflicts
;	with TSR programs like SIDEKICK.  When the user interface
;	is exitting, it will deinstall it's handler and call
;	this routine so that the runtime can reinstall our keyboard
;	handler.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	Per Convention.
;Exceptions:
;	None.
;******************************************************************************
cProc	B$HookKbd,<PUBLIC,FAR>
cBegin
	AND 	b$EventFlags,NOT KybdInst	; force re-installation of
						; keyboard interrupt handler
	XOR	AL,AL				; get current key status,
						; flush keystrokes, and
	CALL	B$RDKYBD			; hook int 9
cEnd
	PAGE
;***
;B$UnHookKbd - UnHook runtime keyboard interrupt
;
;Purpose:
;	The QB4 user interface needs to have control of the
;	keyboard interrupt when it is active to avoid conflicts
;	with TSR programs like SIDEKICK.  When the user interface
;	is entering, it will call this routine and the runtime will
;	unhook itself from the keyboard interrupt chain.  The user
;	interface will then install its keyboard handler.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	Per Convention.
;Exceptions:
;	None.
;******************************************************************************
cProc	B$UnHookKbd,<PUBLIC,FAR>
cBegin
	XFRINT	KYBINT,KBDVEC/4 	;restore original INT 9 (saved in EF)
cEnd

;***
;B$InitKeys1 - Init some event stuff
;OEM-interface routine
;
;Purpose:
;	Init some event stuff.
;
;Entry:
;	AL = value to pass to B$RDKYBD when DX = Ctl_Creak
;
;Exit:
;	None.
;
;Uses:
;	Per Convention.
;
;Exceptions:
;	None.
;
;******************************************************************************
cProc	B$InitKeys1,<PUBLIC,NEAR>
cBegin
	MOV	[B$IPOLKEY],EV_TEXTOFFSET B$POLKEY 
	TEST	b$CtrlFlags,DSwitch ;has B$DBINI done this already?
	jnz	inidon		;brif so
	PUSH	AX		;save this value for later

	MOV	SI,EV_TEXTOFFSET B$RDKYBD ;stick in reg to make

	mov	al,enable_trap		  ;  code smaller & faster
	mov	dx,printer_echo ;enable the printer echo
	call	si

	mov	dx,pause_key	;enable pause key
	call	si

	POP	AX		;retrieve input value
	mov	dx,Ctl_Break	;en/disable ^Break trapping (depending on AL)
	call	si

	mov	al,start_key	;start key trapping
	call	si

inidon:
cEnd				

	SUBTTL	Keyboard Interrupt/Trap Checking in an Operating System Environment
	PAGE

;***
;B$POLKEY -
;OEM-interface Routine
;
;Purpose:
;	Process keys trapped by the OEM/machine dependent keytrapping
;	support.
;	Algorithm:
;
;	Set b$CntlC to 0
;	Call OEM routine B$RDKYBD to detect if a key event has occurred.
;	If trapped key then
;		report to KEYTRP
;	If ^C then
;		Set b$CntlC to 1
;	If ^S then
;		toggle the ^S flag
;	If <printer-echo>
;		toggle the printer echo flag
;	Loop to call B$RDKYBD until no more key events are reported.
;	If ^S flag is set then
;		loop to call B$RDKYBD until another ^S event is detected.
;Entry:
;	none
;
;Exit:
;	none
;
;Uses:
;	none
;
;Exceptions:
;	none
;
;****

cProc	B$POLKEY,<PUBLIC,NEAR>,<AX,BX>
cBegin				; - this routine was completely
				; rewritten - mostly taken from
				; the interpreter
Pollop:
	xor	ax,ax		;Read trapped keys function code
	call	B$RDKYBD	;OEM dependent key trap routine
	dec	bx
	jns	Waskey		;Function, arrow, or user def. key trapped
	inc	bx
	jz	Polkyx		;No key was trapped
	inc	bx
	jz	Wasctc		;^C / <break> function
	inc	bx
	jz	Wascts		;^S / <pause> function
	inc	bx
	jnz	Pollop		;Not <print-screen> func. - ignore, poll next

Wasctp: 			;<printer echo> function
	XOR	b$IOFLAG,LPR_ECHO ; Toggle the printer echo flag
	jmp	short Pollop	;Key found, so there may be more

Waskey:
	; Trap zero relative key if event is ON
	xchg	ax,bx		
	ADD	AL,KEYOFF	; [AL] = 0-relative event id
	CALL	B$TestTrap	
	JZ	Pollop		
	CALL	B$ReqTrap	    ; Trap enabled, Issue Request
	jmp	short Pollop	; Key found, so there may be more

Wascts: 			;^S / <pause> function
	XOR	b$EventFLags,PAUSEF ; Toggle the pause flag
	jmp	short Pollop	;Key found, so there may be more

Wasctc: 			;flag ^Break found
	OR	b$EventFlags,CNTLC ; ^Break is reported last so
	AND	b$EventFlags,NOT PAUSEF ; turn off pause and exit

Polkyx:
	TEST	b$EventFlags,PAUSEF ; Test for pause processing
	jnz	Pollop		;Pause in process, wait for "unpause"
cEnd
sEnd	EV_TEXT 		
	END
