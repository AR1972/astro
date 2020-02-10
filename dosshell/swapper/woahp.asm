;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This module contains all the necessary functions needed for saving and res-;
; -toring the HP vectra state. The routines here were originally developed   ;
; for Real Mode Winoldap and has been adopted after minor modifications.     ;
;									     ;
; History:								     ;
;									     ;
;	 Fri June-15-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 Adapted for the Dos Task Switcher.				     ;
;									     ;
;        Tue June-20-1989.  	-by-  Amit Chatterjee [amitc]		     ;
;        Adapted for 286 protected mode Winoldap			     ;
;----------------------------------------------------------------------------;


	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include	woahpeqs.inc
	include	woahp.inc
	include	macros.mac
	.list

	public	CheckWithApp
	public	NotifyApp

createSeg   _WOARLMSEG,StubSeg,word,public,code
sBegin	StubSeg

	assumes	cs,StubSeg

;----------------------------------------------------------------------------;
;  define state specific tables that are of known size.			     ;
;----------------------------------------------------------------------------;

SAVE_TABLE_REC	struc	
SVector	dw	?
SFirst	dw	?			     
SLast	dw	?
SLoc	dw	?
SAVE_TABLE_REC	ends  

DefineTable 	label 	word 
SAVE_TABLE_REC 	<V_LTOUCH, LD_SOURCE, LD_ACCUM_Y+2, LTouch>
SAVE_TABLE_REC 	<V_LPOINTER, LD_SOURCE, LD_ACCUM_Y+2, LPointer>
SAVE_TABLE_REC 	<V_LTABLET, LD_SOURCE, LD_ACCUM_Y+2, LTablet>
SAVE_TABLE_REC	<V_SPCCP, DH_V_PARENT, DH_MAJOR, HPCCP>
SAVE_TABLE_REC	<V_PSOFTKEY, DH_V_PARENT, DH_MAJOR, HPSoftkeys>
	
NUM_DEFINE_TBL_ENTRIES	equ	($-DefineTable)/8

;----------------------------------------------------------------------------;
; declare variables that are defined elsewhere				     ;
;----------------------------------------------------------------------------;

externB		fVectra			;tells us whether m/c is a vectra
externW		HPsize			;size of HP state save area
externW		HPWindowsSeg		;save windows HP state here
externW		HPDosAppSeg		;save app HP state here
externB		Woa6fValue		;INT 6F to be done or not

;----------------------------------------------------------------------------;
; declare all functions which might be called from other files.		     ;
;----------------------------------------------------------------------------;

		public	IsVectra
		public	FindVectra
		public	EnableVectra
		public	DisableVectra
		public	SaveLinkMapping
		public	RestoreLinkMapping
		public  SaveKeyTrans
		public	RestoreKeyTrans

;----------------------------------------------------------------------------;
; define other variables related to the job of saving vectra state.	     ;
;----------------------------------------------------------------------------;


GlobalW	HPVectorTable, 0		;Address of the HP Vector table
CurHPEntry	db	0		;Current HPEntry vector
CurHILState	db	0		;Bit 6= current HIL OFF state

;----------------------------------------------------------------------------;

;----------------------------------------------------------------------------;
;									     ;
; SysCall	Driver, AXReg						     ;
;									     ;
; Purpose	General purpose HP system calling routine		     ;
;									     ;
; Parameters	Driver  which will be stored in BP			     ;
;		AX value						     ;
;									     ;
; Results	returns AH which is 0 for success			     ;
;				    2 for unsupported			     ;
;									     ;
;----------------------------------------------------------------------------;	

SysCall macro	device, AXReg
	mov	ax, device
	push	ax
	mov	ax, AXReg
	call	HPSysCall
	endm

cProc	HPSysCall,<NEAR>, <ds,bp>
	parmW	Device

cBegin	HPSysCall

	mov	bp, Device
	call	MakeHPIntCall		;make HP system INT call

cEnd	HPSysCall
	 
;----------------------------------------------------------------------------;
;									     ;
; SaveHPSystem( lpSystemState ):Size					     ;
;									     ;
; Purpose	To save the state of the HP EX-BIOS on the Vectra	     ;
;									     ;
; Parameters	lpSystemState - if NULL just return size. 		     ;
;		DS= SS=  Winoldap data segment				     ;
;									     ;
; Results	The size of SystemState in Paras			     ;
;									     ;
; Notes		The following items are saved:				     ;
;			HP Vector table					     ;
;			LTABLET	 data structure				     ;
;			LPOINTER data structure				     ;
;			LTOUCH	 data structure				     ;
;			all physical drivers mapping	       		     ;
;									     ;
;		Since the size of the system state can vary with different   ;
;		version of Vectra, SaveHPSystem should be called first with  ;
;		lpSystemState = NULL to get the size of the system state.    ;
;									     ;
;----------------------------------------------------------------------------;
cProc	SaveHPSystem, <NEAR,PUBLIC>, <DS,ES,SI,DI>

	parmD	HPState
	localW	StateSize  

cBegin	SaveHPSystem	

; set DS - Winoldap data segment, ES - Saved HP data segment, cld

	SysCall V_SYSTEM, <F_INS_BASEHPVT shl 8>
	mov	CS:[HPVectorTable], es
	cld
	
; Find the size of SAVE_STATE if HPState is NULL
; if load is true load them up
;
	les	di, HPState		;Inquire HPstate size if NULL
	mov	ax, es
	or	ax, di
	jz	SaveRet
	
; Save the vector table

	EnterCrit  
	xor	si, si
	mov	DS, CS:[HPVEctorTable]
	les	di, HPState
	lea 	di, ES:[di].VectorTable
 	mov	cx, (V_WINDOWS+6)/2
	cld
	rep	movsw

; Save the well defined data areas
	
	smov	ds,cs			;get stub data segment
	lea	si, DefineTable
	mov	cx, NUM_DEFINE_TBL_ENTRIES  

define_save_loop:

	mov	ax, DS:[si].SLoc
	add	ax, OFF_HPstate
	Save	cx
	cCall	SaveData, <DS:[si].SVector, DS:[si].SFirst, DS:[si].SLast, ax>
	add	si, size SAVE_TABLE_REC
	loop	define_save_loop

; Save the link mapping 

	les	di, HPState
	lea	di, es:[di].Devices
	call	SaveLinkMapping

; Save the keyboard translators

	les	di, HPState
	lea	di, es:[di].Translators
	call	SaveKeyTrans

; Save the speed and key click of the system. Note this will not
; work without EX-BIOS. Tuff Luck.

	les	di, HPState
	mov	bl, 1			;STATE_1
   	SysCall	V_SCANDOOR, <F_STATE_IOCTL shl 8 + SF_GET_STATE>
   	mov	es:[di].Click, bh
	mov	bl, 3			;STATE_3
   	SysCall	V_SCANDOOR, <F_STATE_IOCTL shl 8 + SF_GET_STATE>
   	mov	es:[di].Speed, bh
	
	LeaveCrit

SaveRet:

	mov	AX, size SAVE_STATE     ; AX= Size of HPstate in bytes using

cEnd	SaveHPSystem	
;----------------------------------------------------------------------------;
;									     ;
; RestoreHPSystem( lpSystemState )					     ;
;									     ;
; Purpose	To restore the state of the HP EX-BIOS on the Vectra	     ;
;									     ;
; Parameters	Long pointer lpSystemState				     ;
;		DS = SS = Winoldap data segment				     ;
;									     ;
; Notes		The following items are resotred:			     ;
;			HP Vector table					     ;
;			LTABLET	 data structure				     ;
;			LPOINTER data structure				     ;
;			LTOUCH	 data structure				     ;
;			all physical drivers mapping	       		     ;
;									     ;
;----------------------------------------------------------------------------;
cProc	RestoreHPSystem, <NEAR,PUBLIC>, <DS,ES,SI,DI>

	parmD	HPRState
	localV	CurrentDevices, 14

cBegin	RestoreHPSystem	
		
	EnterCrit

; Restore the speed and key click of the system

	les	di, HPState
	mov	bl, 1			;STATE_1
   	mov	bh, es:[di].Click
   	SysCall	V_SCANDOOR, <F_STATE_IOCTL shl 8 + SF_SET_STATE>
	mov	bl, 3			;STATE_3
   	mov	bh, es:[di].Speed
   	SysCall	V_SCANDOOR, <F_STATE_IOCTL shl 8 + SF_SET_STATE>
	
; Restore the keyboard translators

	les	di, HPRState
	lea	di, es:[di].Translators
	call	RestoreKeyTrans

; Restore the link

	les	di, HPRState
	lea	di, es:[di].Devices
	call	RestoreLinkMapping
					
; Restore the defined data areas

	smov	es,cs
	mov	ds, SEG_HPRState

; ES= Winoldap data - DS= Saved HP area

	lea	si, DefineTable
	mov	cx, NUM_DEFINE_TBL_ENTRIES		    

restore_loop:

	mov	ax, ES:[si].SLoc
	add	ax, OFF_HPRState
	Save	cx
	cCall	RestoreData, <ES:[si].SVector, ES:[si].SFirst, ES:[si].SLast, ax>
	add	si, size SAVE_TABLE_REC
	loop	restore_loop		

; Restore the vector table

	mov	es, CS:[HPVectorTable]	;DS= Saved HP area - ES= HP Vector table
	lds	si, HPRState
	lea 	si, [si].VectorTable
	xor	di, di
 	mov	cx, (V_WINDOWS+6)/2
	cld
	rep	movsw
		 
	LeaveCrit

cEnd	RestoreHPSystem	
;----------------------------------------------------------------------------;
;									     ;
; SaveKeyTrans - Save the state of the keyboard translators.		     ;
;									     ;
;	The state of the keyboard translators are saved in two places:	     ;
;	First, in the headers of V_SCCP and V_PSOFTKEY and in the	     ;
;	8042 chip.							     ;
;									     ;
;									     ;
; Entry									     ;
;	ES:DI	- Pointer to the save area. Must be 6 bytes.		     ;
;	DS, SS  - Winoldap data segment					     ;
;	HP ENTRY must be 6Fh						     ;
;									     ;
; Exit									     ;
;									     ;
; Uses									     ;
;	AX, BX								     ;
;									     ;
;----------------------------------------------------------------------------;

SaveKeyTrans:

	test	[fVectra], 1
	jz	skExit

; Save the cursor pad header

	mov	bx, V_SPCCP		;Cursor cursor pad	
	call	GetParent
	mov	es:[di], ax

; Save the SoftKeys header

	mov	bx, V_PSOFTKEY		
	call	GetParent
	mov	es:[di+2], ax
   
; Save the state of Scandor. If V_SCANDOOR is not present
; these calls will have no effect.

   	mov	bl, 0			;State 0
   	SysCall	V_SCANDOOR, <F_STATE_IOCTL shl 8 + SF_GET_STATE>
   	mov	es:[di+4], bh
   	mov	bl, 2			;State 2
   	SysCall	V_SCANDOOR, <F_STATE_IOCTL shl 8 + SF_GET_STATE>
   	mov	es:[di+5], bh
skExit:
	ret
	
; Save the parent
;    BX    =  Driver vector number
;    ES:DI -> Storage word	


GetParent:

	push	es
	
; get HP vector table

	mov	es, cs:[HPVectorTable]	
	
; get the Device header address

	mov	es, es:[bx+4]		;Get the DS of the driver
	mov	ax, es:[DH_V_PARENT]	;load the parent

; Store the parent

	pop	es
	ret
;----------------------------------------------------------------------------;
;									     ;
; RestoreKeyTrans - Restore the state of the keyboard translators.	     ;
;									     ;
;	The state of the keyboard translators are saved in two places:	     ;
;	First, in the headers of V_SCCP and V_PSOFTKEY and in the	     ;
;	8042 chip.							     ;
;									     ;
;									     ;
; Entry									     ;
;	ES:DI	- Pointer to the save area. Must be 6 bytes.		     ;
;	DS, SS  - Winoldap data segment					     ;
;	HP ENTRY must be 6Fh						     ;
;									     ;
; Exit									     ;
;									     ;
; Uses									     ;
;	AX, BX								     ;
;									     ;
;----------------------------------------------------------------------------;
RestoreKeyTrans:

	test	[fVectra], 1
	jz	rkExit

; Restore the cursor pad header

	mov	bx, V_SPCCP		;Cursor cursor pad	
	mov	ax, es:[di]
	call	SetParent

; Restore the SoftKeys header

	mov	bx, V_PSOFTKEY		
	mov	ax, es:[di+2]
	call	SetParent
   
; Restore the state of Scandor. If V_SCANDOOR is not present
; these calls will have no effect.

   	mov	bl, 0			;State 0
   	mov	bh, es:[di+4] 
   	SysCall	V_SCANDOOR, <F_STATE_IOCTL shl 8 + SF_SET_STATE>
   	mov	bl, 2			;State 2
   	mov	bh, es:[di+5]
   	SysCall	V_SCANDOOR, <F_STATE_IOCTL shl 8 + SF_SET_STATE>
rkExit:
	ret
	
; Set the parent
;    BX    =  Driver vector number
;    ES:DI -> Storage word	

SetParent:
	push	es
	
; get HP vector table

	mov	es, cs:[HPVectorTable]	
	
; get the Device header address

	mov	es, es:[bx+4]		;Get the DS of the driver
	mov	es:[DH_V_PARENT], ax	;set the parent

; Store the parent

	pop	es
	ret
;----------------------------------------------------------------------------;
;									     ;
; SaveLinkMapping							     ;
;									     ;
;	Save the device mapping of the HP-HIL link.			     ;
;									     ;
; Entry									     ;
;	ES:DI	- Pointer to the save area. Must be 28 bytes.		     ;
;	HP ENTRY must be 6Fh						     ;
;									     ;
; Exit									     ;
;									     ;
; Uses									     ;
;	C Convention							     ;
;			       						     ;
;----------------------------------------------------------------------------;
cProc 	SaveLinkMapping, <NEAR>, <ES,DS,SI,DI>

cBegin

	call	IsVectra
	jz	sdExit
	
	mov	si, di
	SysCall V_SINPUT, <F_INQUIRE_ALL shl 8>
	mov	si, di
	add	si, 14	
	SysCall V_SINPUT, <F_IO_CONTROL shl 8 + SF_GET_LINKS>

sdExit:

cEnd
;----------------------------------------------------------------------------;
;									     ;
; RestoreLinkMapping							     ;
;									     ;
;	Restore the device mapping of the HP-HIL link.			     ;
;									     ;
; Entry									     ;
;	ES:DI	- Pointer to the save area. Must be 28 bytes.		     ;
;	HP ENTRY must be 6Fh						     ;
;									     ;
; Exit									     ;
;									     ;
; Uses									     ;
;	C convention							     ;
;									     ;
;----------------------------------------------------------------------------;
cProc	RestoreLinkMapping, <NEAR>, <DS,ES,SI,DI>

	localV	CurrentDevices, 14

cBegin	

	call	IsVectra
	jz	rdExit
	
; Has the link changed ?

	push	es		   	;save map buffer
	smov	es,ss			;will read header data into stack
	lea	si, CurrentDevices
	SysCall V_SINPUT, <F_INQUIRE_ALL shl 8>
	pop	es			;get back buffer with saved links

	smov	ds,ss			;need to compare with data on stack

; DS:SI - current device header data, ES:DI - saved device header data.
; check to see if the device mapping is still intact or not.

	push	di
	mov	cx, 7
	cld
	repe	cmpsw
	pop	di
	je	restore_old
	

; device mappings have been changed (rare), saved link mappings are meaningless
; now. We will restore the default mapping.

	SysCall V_SINPUT, <F_IO_CONTROL shl 8 + SF_DEF_LINKS>
	jmp	short rdExit
	
; device mapping is still the same, restore the link mapping data.

restore_old:

	mov	si, di
	add	si, 14	
        SysCall V_SINPUT, <F_IO_CONTROL shl 8 + SF_SET_LINKS>

rdExit:

cEnd
;----------------------------------------------------------------------------;
;									     ;
; SaveData( Vector, Loc, Start, End ) 					     ;
;									     ;
; Purpose 	Save the data contained in a driver data space		     ;
;									     ;
; Parameters	Vector - HP System vector number			     ;
;		First - starting address				     ;
;		Last - ending address					     ;
;		Loc - save pointer					     ;
;									     ;
; On Entry	es - HP saved Data segment				     ;
;		ds - Winoldap data					     ;
;									     ;
; On Exit	es, ds, si, di preserved				     ;
;									     ;
;----------------------------------------------------------------------------;

cProc	SaveData, <NEAR,PUBLIC>, <es, ds, si, di>

	parmW	Vector
	parmW	First
	parmW	Last
	parmW	Loc

cBegin	SaveData	    

; move the data
	mov	bx, Vector    		;get describe record address
	mov	DS, CS:[HPVectorTable]
	mov	ds, ds:[bx+4]		
	mov	si, First
	mov	di, Loc			;load the destenation addres
	mov	cx, Last
	sub	cx, si	
	cld
	rep 	movsb

cEnd	SaveData
;----------------------------------------------------------------------------;
;									     ;
; RestoreData( Vector, Loc, Start, End ) 				     ;
;									     ;
; Purpose 	Restore the data contained in a driver data space	     ;
;									     ;
; Parameters	SS= Winoldap Data segment				     ;
;		DS= Saved HP data area					     ;
;		Vector - HP System vector number			     ;
;		First - starting address				     ;
;		Last - ending address					     ;
;		Loc - save pointer					     ;
;									     ;
;----------------------------------------------------------------------------;

cProc	RestoreData, <NEAR,PUBLIC>, <ES, SI, DI>

	parmW	Vector
	parmW	First
	parmW	Last
	parmW	Loc

cBegin	RestoreData	    

; find the HP vector table

	mov	es, CS:[HPVectorTable]

; move the data

	mov	bx, Vector    		;get describe record address
	mov	es, es:[bx+4]		;ES= device data segment
	mov	di, First
	mov	si, Loc
	mov	cx, Last
	sub	cx, di	
	cld
	rep 	movsb
	
cEnd	RestoreData
;----------------------------------------------------------------------------;
; IsVectra:							             ;
;									     ;
; Determine if this is a Vectra A, A+, A++ with EX-BIOS resident.	     ;
;									     ;
; Entry:								     ;
;  DS:		Winoldap data						     ;
;									     ;
; Exit:									     ;
;   If it is a Vectra ax != 0 and zero flag cleared			     ;
;									     ;
;----------------------------------------------------------------------------;

IsVectra	Proc	NEAR

;  Check if the PC is a Vectra. If Yes, then call HPSystem to get the
;  current size of the HP state

	push	ES			;Save it
	mov	AX, BIOS_SEG
	mov	ES, AX			;ES: Segment of ID code
	cmp	Word Ptr ES:[ID_OFFSET], 'PH'
	pop	ES			;Restore entry ES
	je	ivCheckBios
	jmp	short ivNo     		;Not a Vectra, so no extra HP processing

;  Check if EX-BIOS is present

ivCheckBios:	

	mov	AX, F16_INQUIRE
	mov	BX, AX			;set BX with out HP
	int	INT_KBD
	cmp	BX, 'HP'		;EX-BIOS present?
	je	ivYes			;No, finish

ivNo:	

	xor	ax, ax
	jmp	short ivRet

ivYes:	

; for a vactra the HP system vector is normaly 6fh, but there might be a
; substitute, so we will get the vector now.

	mov	ax,6F0Dh		;this magic Int 16h returns
	int	16h			;  the HP extended BIOS interrupt
	cmp	ah,2			;  number -- a return of 2 == 6Fh
	jne	@f			;  which is the default
	mov	ah,6Fh
@@:

; AH has the vector number, we will modify a byte in our code to prepare
; the INT xx instruction.

	mov	cs:[HPIntVector],ah	;modify code.

	xor	AX, AX
	inc	ax	

ivRet:	

	ret

IsVectra 	endp
;----------------------------------------------------------------------------;
; FindVectra:								     ;
;									     ;
; Set fVectra flag if we have a Vectra 	A, A+, or A++ with EX-BIOS resident. ;
; Initialize HPsize accordingly.					     ;
;									     ;
; This code is only accessed once when the first context-switchable BAD app  ;
; is executed. 								     ;
;									     ;
; Entry:								     ;
;  DS:		Winoldap data						     ;
;									     ;
; Exit:									     ;
;  fvectra, HPsize							     ;
;----------------------------------------------------------------------------;

FindVectra	Proc	NEAR

	assumes	ds,StubSeg
  
	mov	HPsize,0		;reset size incase not vectra
	call	IsVectra		;is it HP vectra ?
	jz	FindVRet		;No, finish
	mov	fVectra, 1	 	;Yes, flag we have a Vectra

; Under Windows, it is ASSUMED that HPentry is always 6Fh so we can issue
; EX-BIOS calls safely.

	xor	AX, AX
	cCall	SaveHPSystem, <AX, AX>

;  AX:	Size of the HP state in Bytes,convert it into paragraphs and save it

	shiftr	ax,4			;convert to paragraphs
	inc	ax			;round up to the next one
	mov	HPsize,ax		;save size of state in paragraph

FindVRet:

	ret

FindVectra	endp
;----------------------------------------------------------------------------;
; EnableVectra:								     ;
;									     ;
; If we have a Vectra A, A+, or A++ with EX-BIOS, save the current HPEntry   ;
; vector, HPHIL state and set HPentry=6Fh, Turn ON HPHIL.		     ;
;									     ;
; Entry:								     ;
;   DS:		Winoldap DATA						     ;
;									     ;
;									     ;
; Exit:									     ;
;   CurHPentry, CurHILState						     ;
;									     ;
; Regs:									     ;
;   AX, BX								     ;
;----------------------------------------------------------------------------;									     ;

cproc	EnableVectra, <NEAR>

cBegin

	cmp	CS:[fVectra], 1
	jne	EnVret			;No special processing if not a vectra

	push	BP
	push	DS

; Save current HP entry and set it to be 6Fh

	mov	AX, F16_GET_INT_NUMBER
	int	INT_KBD
	mov	CS:[CurHPEntry], HPENTRY ; Assume we have HPentry= 6Fh
	cmp	AH, RS_UNSUPPORTED
	je	EnVectraA		;We have a Vectra A, A+ if unsupported
	mov	CS:[CurHPEntry], AH	;Save it if valid

	mov	BL, HPENTRY		;~~tqn 061287
	mov	AX, F16_SET_INT_NUMBER
	int	INT_KBD

; Save current HPHIL state and set it ON

EnVectraA:

	mov	AH, F_SYSTEM
	mov	AL, SF_REPORT_STATE
	mov	BP, V_HPHIL
	call	MakeHPIntCall		;INT 6f or its substitute

; Bit 14 of BX (Status Word) = 1: HPHIL OFF
;			       0: 	ON

	mov	CS:[CurHILState], BH

; Turn HIL ON

	mov	AH, F_IO_CONTROL
	mov	AL, SF_HIL_ON
	mov	BP, V_HPHIL
	mov	ax,6F0Dh		;this magic Int 16h returns
	int	16h			;  the HP extended BIOS interrupt
	cmp	ah,2			;  number -- a return of 2 == 6Fh
	jne	@f			;  which is the default
	mov	ah,6Fh
@@:
	call	MakeHPIntCall		;INT 6f or its substitute
	pop	DS
	pop	BP

EnVret:	

cEnd	EnableVectra
;----------------------------------------------------------------------------;
; DisableVectra:      							     ;
;									     ;
; Restore the Vectra environment according to CurHPEntry and CurHILState     ;
; Assume that HPENTRY is always 6Fh					     ;
;									     ;
; Entry:								     ;
;   DS:		Winoldap DATA						     ;
;									     ;
; Exit:									     ;
;									     ;
;									     ;
; Regs:									     ;
;   AX, BX								     ;
;----------------------------------------------------------------------------;

cProc	DisableVectra, <NEAR>

cBegin

	cmp	CS:[fVectra], 1
	jne	DisVRet

	push	BP
	push	DS

; Restore the HIL state according to CurHILState

	mov	AH, F_IO_CONTROL
	mov	BP, V_HPHIL
	mov	AL, SF_HIL_ON		;~~tqn 061287 Assume HIL is ON
	test	CS:[CurHILState], B_HIL_STATE
	je	DisVHIL			;0= correct assumption
	mov	AL, SF_HIL_OFF		;~~tqn 061287

DisVHIL:

	call	MakeHPIntCall		;INT 6f or substitute

; Restore the Saved HPEntry

	mov	AX, F16_SET_INT_NUMBER
	mov	BL, CS:[CurHPEntry]
	int	INT_KBD

	pop	DS
	pop	BP

DisVRet:

cEnd	DisableVectra
;----------------------------------------------------------------------------;
; SaveWindowsHPState:							     ;
;									     ;
; This routine saves the state of the HP system when windows was around.     ;
;----------------------------------------------------------------------------;

cProc	SaveWindowsHPState,<NEAR,PUBLIC,PASCAL>

cBegin

	cmp	fVectra,0		;is this a Vectra PC
	jz	SaveWindowsHPStateRet	;nothing to save

; save the state in the pre allocated buffer.

	mov	es,HPWindowsSeg		;segment of save buffer
	xor	ax,ax			;offset is 0
	cCall	SaveHPSystem,<es,ax>	;save the state

; now enable the vectra before handing control to the old app

	cCall	EnableVectra		;see function defined above

SaveWindowsHPStateRet:

cEnd
;----------------------------------------------------------------------------;
; RestoreWindowsHPState:						     ;
;									     ;
; Restores the state of the HP system that was current while windows was act-;
; -ive.									     ;
;----------------------------------------------------------------------------;

cProc	RestoreWindowsHPState,<NEAR,PUBLIC,PASCAL>

cBegin

	cmp	fVectra,0		;is it a HP vectra
	jz	RestoreWindowsHPStateRet;no, nothing to restore

; disable the vectra settings that we had turned on for running the oldapp

	cCall	DisableVectra		;does some state restore

; now restore the state of the HP system from the saved state buffer

	mov	es,HPWindowsSeg		;segment of save buffer
	xor	ax,ax			;offset is 0
	cCall	RestoreHPSystem,<es,ax>	;restore the state

RestoreWindowsHPStateRet:

cEnd
;----------------------------------------------------------------------------;
; SaveDosAppHPState:							     ;
;									     ;
; Saves the state of the HP system before switching out from an Old App.     ;
;----------------------------------------------------------------------------;

cProc	SaveDosAppHPState,<NEAR,PUBLIC,PASCAL>

cBegin
	
	cmp	fVectra,0		;is it a HP Vectra system ?
	jz	SaveDosAppHPStateRet	;no, nothing to save

; save the state of the system

	mov	es,HPDosAppSeg		;segment of save buffer
	xor	ax,ax			;offset is 0
	cCall	SaveHPSystem,<es,ax>	;save the state

SaveDosAppHPStateRet:

cEnd
;----------------------------------------------------------------------------;
; RestoreDosAppHPState:							     ;
;									     ;
; Restores the HP state for an old app to what was current at the time of    ;
; switching the old app out.					   	     ;
;----------------------------------------------------------------------------;
cProc	RestoreDosAppHPState,<NEAR,PUBLIC,PASCAL>

cBegin
	
	cmp	fVectra,0		;is it a HP Vectra system ?
	jz	RestoreDosAppHPStateRet	;no, nothing to restore

; save the state of the system

	mov	es,HPDosAppSeg		;segment of save buffer
	xor	ax,ax			;offset is 0
	cCall	RestoreHPSystem,<es,ax>	;save the state

RestoreDosAppHPStateRet:

cEnd
;----------------------------------------------------------------------------;
; CheckWithApp:								     ;
;								             ;
; Tests to see if the app will allow us to switch out at this point.	     ;
; This routine will set zero if it is OK to switch out from the application. ;
;									     ;
; If INT 6F scheme has bisabled then simply return from this routine with    ;
; ZERO set. (It can be disabled by a switch setting in SYSTEM.INI)           ;
;----------------------------------------------------------------------------;

CheckWithApp	proc	near

	cmp	cs:[Woa6fValue],0	;int 6f to be bypassed ?
	jz	CheckWithAppEnd		;yes, bypass it.

	pushem	ax,si,di,bp,ds		;save the context

	call	CheckNotify		;is windows notification installed?
	jz	CheckWithAppRet		;no, so we can switch out.

; get the approval from the app to switch out.

	mov	bp, V_WINDOWS 		
	mov	ax, F_SYSTEM shl 8 + SF_REPORT_STATE
	call	MakeHPIntCall		;int 6f or its substitute
	or	ax,ax			;can switch out if zero.

CheckWithAppRet:
	
	popem	ax,si,di,bp,ds		;restore the saved registers

CheckWithAppEnd:

	ret

CheckWithApp	endp
;----------------------------------------------------------------------------;
;  CheckNotify:								     ;
;									     ;
;  Checks to see if the INT 6F notification hooks are installed or not.	     ;
;									     ;
;  Outputs:								     ;
;    ZF:	1: Hooks not installed					     ;
;		0: Hooks in						     ;
;----------------------------------------------------------------------------;									     ;

CheckNotify	proc near

	push	es			;save.
	cmp	cs:[fVectra], 1 	;do we have a Vectra ?
	jnz	CheckAsInIBM_PC_or_AT 	;check the IBM way
	mov	es, cs:[HPVectorTable]
	cmp	es:[V_WINDOWS +2], ROM_SEGMENT
	jmp	short CheckNotifyRet	;ZF set if V_WINDOWS CS = ROM_SEGMENT

CheckAsInIBM_PC_or_AT:

	xor	ax,ax			;need to access the IDT at segment 0:
	mov	es,ax			;es has IDT segment
	cmp	es:[4*HPENTRY+2],ROM_AREA;is INT 6F hook in the rom ?
	jb	CheckAsInIBM_PC		;no, but machine may be a IBM_PC
	xor	ax,ax			;vector in ROM for AT.
	jmp 	short CheckNotifyRet 	;app has not hooked INT 6F

CheckAsInIBM_PC:

	cmp word ptr es:[4*HPENTRY+2],0 ;Initial HPENTRY value in an IBM-PC

CheckNotifyRet:

	pop	es			;restore thrashed es
	ret

CheckNotify	endp
;----------------------------------------------------------------------------;
;  NotifyApp:								     ;
;									     ;
;  If the notification hooks are installed by the Oldapp, Notify that app    ;
;  that we have successfully complete a context switch and are back to resume;
;  the app.								     ;
;									     ;
; If INT 6F scheme is marked as disabled, simply return from the routine.    ;
; (it can be disabled by a switch setting in SYSTEM.INI)		     ;
;----------------------------------------------------------------------------;

NotifyApp	proc  near

	cmp	cs:[Woa6fValue],0	;int 6f tobe bypassed ?
	jz	NotifyAppEnd		;yes

	push	ax			;save
	call	CheckNotify		;is the notification hook in?
	jz	NotifyAppRet		;no,will not try to notify

; notification hook is in, notify the app that we are back.

	pushem	bp,ds			;save these registers
	mov	bp, V_WINDOWS		
	mov	ax, F_SYSTEM shl 8 + SF_START
	call	MakeHPIntCall		;int 6f or its substitute

	popem	bp,ds			;restore saved registers

NotifyAppRet:

	pop	ax			;restore

NotifyAppEnd:

	ret

NotifyApp	endp
;----------------------------------------------------------------------------;
; MakeHPIntCall:							     ;
; 									     ;
; This routine makes the HP system INT call. This is just an INT XX instruct-;
; -ion where XX is normally 6fh but may be different on some machines. The   ;
; XX part of the instruction will be modified at run time by the IsVectra    ;
; routine.								     ;
;----------------------------------------------------------------------------;

MakeHPIntCall  proc  near

		db	0cdh		 ;INT opcode
HPIntVector	db	6fh		 ;default is 6fh

		ret

MakeHPIntCall	endp
;----------------------------------------------------------------------------;
sEnd	StubSeg

end
