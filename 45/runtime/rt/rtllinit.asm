	TITLE	RTLLINIT - Low Level Core initialization module
	PAGE	56,132
;***
;RTLLINIT.ASM - Low Level core initialization module
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains low level initialization support for the
;	BASIC 3.0 runtime.  This module will always be present in a user's
;	program.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc
;
;	Code Segments
;
	USESEG	<RT_TEXT>	;runtime core
;
;	Data Segments
;
	useSeg	_DATA		
	useSeg	_BSS		;runtime data (uninitialized)

	INCLUDE seg.inc
	INCLUDE idmac.inc	


	SUBTTL	Runtime data definitions for Low level BASIC Runtime Core
	PAGE

sBegin	_DATA			
sEnd	_DATA			

sBegin	_BSS

;
;	Global data
;
;***
;b$RcoFlg - Flag to indicate if Ronco keyboard is present
;OEM-interface routine (variable)
;
;Purpose:
;	This variable is a flag used to indicate if a Ronco (101 key or
;	extended) keyboard is attached to the system.  This keyboard has
;	to be handled specially in the runtime code because of the extra
;	function keys.
;
;Allocation:
;	b$RcoFlg is a BYTE value declared in the _BSS segment by
;	the OEM.
;
;Values:
;	0   - Normal keyboard present
;	10H - Extended (Ronco) keyboard present
;
;Initially Set:
;	b$RcoFlg will be initialized by the OEM-Dependent code during
;	the call to B$RTLLINI or B$GWINI.
;
;Modified By:
;	Once set, this value is never changed.
;
;Used By:
;	OEM-Dependent code and any code dealing with the function keys.
;****
	staticW b$llequipflags,?	; hardware equipment flags.
	staticB b$llmachineid,?		; machine id. PC,XT,AT,JR,etc.
	globalB b$RcoFlg,?		; NZ if ronco present

sEnd	_BSS


assumes CS,RT_TEXT			
sBegin	RT_TEXT				

	externNP	B$SEGINI	

	SUBTTL	Runtime Low Level Core Initialization
	PAGE
;***
;B$RTLLINI - OEM-Dependent runtime initialization.
;OEM-interface routine
;
;Purpose:
;	OEM-Dependent initialization for all of the BASIC runtime.
;
;	There are two different OEM-Dependent initialization routines
;	B$RTLLINI and B$GWINI. In terms of functionality, there is no
;	difference between the two routines.  B$GWINI is from an earlier
;	concept of the organization of the runtime while B$RTLLINI is from
;	a newer, more structured approach to the runtime.  Eventually,
;	we hope to be able to distribute the code currently in B$GWINI
;	but until that time there are two initialization routines.
;
;	The following differences exist between the two routines:
;
;		B$RTLLINI		B$GWINI
;		Called before B$GWINI	called after B$RTLLINI
;
;	Currently, this routine only sets b$RcoFlg and then calls
;	B$SEGINI.
;
;Entry:
;	None.
;
;Exit:
;	AX != 0 if initialization failed.
;
;Uses:
;	Per Convention
;
;Exceptions:
;	None.
;******************************************************************************
;
;     Get Machine ID.
;     Get keyboard type.
;     Save equipment flag.
;
;#**

cProc	B$RTLLINI,<PUBLIC,NEAR>,<ES>
cBegin

;
;	Get the machine ID and save it in b$llmachineid.
;	The machine ID is located at 0FFFF:0E and are mapped as
;	follows:
;		PC	0FFH
;		XT	0FEH
;		JR	0FDH
;		AT	0FCH
;
	MOV	BX,0FFFFH		;segment 0FFFFH
	MOV	ES,BX			;address with ES
	MOV	AL,ES:[000EH]		;get machine ID
	MOV	b$llmachineid,AL	;save machine ID

;
;	Check whether the Ronco is presented.  Ronco may be installed on
;	XT or AT only.	If presented, bit 4 at 0000:496H (actually 40:96H)
;	is on.

	XOR	AX,AX			;segment 0000H
	MOV	ES,AX			;address with ES
	CMP	b$llmachineid,0FCH	; AT or newer model ?
	JBE	ChkRco			; Brif yes
	CMP	b$llmachineid,0FEH	; XT is another possibility
	JNZ	KybChkEnd		; Brif not
ChkRco: 				
	MOV	AL,ES:[496H]		; get KB_FLAG_3 in BIOS data seg
	AND	AL,10H			; mask the other bits
	MOV	[b$RcoFlg],AL		; save the result (either 0 or 10H)

;	Save the equipment flags in b$llequipflags.
;	The equipment flags are located at 0000:410H.
;
KybChkEnd:				
	MOV	AX,ES:[410H]		;get equipment flags.
	MOV	b$llequipflags,AX	;save equipment flags.
	cCall	B$SEGINI		

	XOR	AX,AX			;return 0 => init OK, no errors
cEnd

	SUBTTL	D5OPEN - DOS 5 OPEN helper
	PAGE



sEnd	RT_TEXT 		
	END
