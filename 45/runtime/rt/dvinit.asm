	TITLE	DVINIT.ASM - Device I/O Initialization/Termination module
;***
;DVINIT.ASM - Device I/O  initialization/termination module
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains Device I/O initialization and termination
;	support for the BASIC 3.0 runtime.  This module will only be present
;	in a user's program when a program contains statements which need
;	generalized device I/O.
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;
;	Code Segments
;
	USESEG	<INIT_CODE>	;Initialization
	USESEG	<DV_TEXT>	;Device I/O
	USESEG	<OI_TEXT>	;comm/printer I/O
	USESEG	<RT_TEXT>	

;
;	Data Segments
;
	USESEG	<_BSS>		;runtime data (uninitialized)
	USESEG	<_DATA> 	;runtime data (initialized)
	USESEG	<XIB>		; XIB and XIE must bracket XI!
	USESEG	<XI>		;initializer segment
	USESEG	<XIE>		

	INCLUDE seg.inc
	INCLUDE baslibma.inc
	INCLUDE devdef.inc	
	INCLUDE files.inc	
	INCLUDE nhutil.inc	
	INCLUDE const.inc	
	INCLUDE rtps.inc	

	INCLUDE compvect.inc	;component vectors

	INITIALIZER	B$xDVINI	;put B$xDVINI in initializer list.

	SUBTTL	Code Externals
	PAGE

sBegin	OI_TEXT 			
	externNP	B$LPTECHO	
sEnd	OI_TEXT 			

sBegin	DV_TEXT 			
	externNP	B$NearRet	;for disp vectors in compvect.inc
	externNP	B$CommSave	
	externNP	B$CommRestore	
sEnd	DV_TEXT 			

	SUBTTL	Runtime data definitions for BASIC Device I/O
	PAGE
sBegin	_BSS			
	externW b$LPTFDB	
	externB b$FILMOD	;GLOBAL.INC -
	externB b$TTY_BAKCHR	;defined in iotty
sEnd	_BSS			

sBegin	_DATA
;
;	external data
;
	externW b$pDevTable	
	externW b$run_disp	;RUN time initialization dispatch table
	externW b$pLPTECHO	; conditional vector to B$LPTECHO
	externW b$pKYBD_OPEN	; conditional vector to KYBD_OPEN
	externW b$pSCRN_OPEN	; conditional vector to SCRN_OPEN
	externB b$SCRNWIDTH	; logical width of SCRN:
	externW b$pCommSave	; conditional vector to B$CommSave
	externW b$pCommRestore	; conditional vector to B$CommRestore
sEnd	_DATA

sBegin	RT_TEXT 		

	PUBLIC	b$devused	; The compiler will generate a reference
b$devused equ 0 		; to b$devused if it decides that a user
				; program needs device I/O support.  This
				; causes dvinit to get drug in.
	externNP B$OPEN_DEV	

assumes cs,RT_TEXT

;	These macros are used to define the dispatch table entry
;	equates for the communication and cassette devices.  The
;	macro is invoked with the device operation (I or C), the device
;	type (COM or CAS), and the number of devices (NUM_RS232 or
;	NUM_CASS).

DevDspEquates	MACRO	Dop,Dtype,Dnum
	Dindex	= 0		;;value to pass to item generation macro

	REPT	Dnum		;;one item for each device defined
	Dindex	= Dindex + 1	;;update the counter
	DevItem	Dop,Dtype,%Dindex ;;define item with op (I or C), type (COM
				;;or CAS), and counter
	ENDM

	ENDM	;;DevDspEquates

DevItem	MACRO	Dop,Dtype,Dindex
	externNP B$&Dop&_&Dtype&Dindex ;;define ext (e.g., externNP $I_COM3)
Dop&_&Dtype&Dindex EQU B$&Dop&_&Dtype&Dindex ;;define the equate...
				;;(e.g., I_COM3 EQU $I_COM3)
	ENDM	;;DevItem

;	Device initialization dispatch address generator

DEVMAC	MACRO	ARG
	DW	I_&ARG
	ENDM

;	Define the equates in the form "I_CASx EQU B$I_CASx".

	DevDspEquates	I,CAS,NUM_CASS

;	Define the equates in the form "I_COMx EQU B$I_COMx".

	DevDspEquates	I,COM,NUM_RS232

INIDEV_TABLE LABEL WORD

	DEVNAM			;Device initialization addresses

INIDEV_END LABEL WORD		;End of table

iniwid= 80			;Initial printer width

sEnd	RT_TEXT 		


	SUBTTL	Runtime Device I/O  Initialization
	PAGE
assumes CS,INIT_CODE
sBegin	INIT_CODE

;***
;B$xDVINI - Device I/O  initializer
;PLM B$xDVINI()
;
;Purpose:
;	Initializer for Device I/O  component.	This routine is called
;	by the Crt0 startup before _main is called.  It will update the
;	indirect dispatch tables for the Device I/O  routines.	This
;	insures that the only time that Device I/O  is accessed is when
;	this module is linked into the user program.
;
;Entry:
;	None.
;
;Exit:
;	b$run_disp.DV_RVEC	- contains pointer to B$INIDEV
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$xDVINI,<FAR>
cBegin
;
;	update "RUN" time initialization dispatch address to B$INIDEV	
;
	MOV	WORD PTR [b$run_disp].DV_RVEC,DV_TEXTOFFSET B$INIDEV	
;
;	initialize granularity dispatchers.

	MOV	b$pLPTECHO, OI_TEXTOFFSET B$LPTECHO ; Set vector to point
						    ;     to real routine
	MOV	b$pKYBD_OPEN, DV_TEXTOFFSET KYBD_OPEN ; Set vector to point
						      ;	to real routine
	MOV	b$pSCRN_OPEN, DV_TEXTOFFSET SCRN_OPEN ; Set vector to point
						      ;	to real routine
	MOV	b$pDevTable, DV_TEXTOFFSET DevTable   ; Set vector to point
						      ;	to full table
	MOV	b$pCommSave, DV_TEXTOFFSET B$CommSave ; Set vector to point
						      ;	to real routine
	MOV	b$pCommRestore,DV_TEXTOFFSET B$CommRestore ; Set vector to
							   ;   real routine

cEnd
sEnd	INIT_CODE

	SUBTTL RUN time initialization for device independent I/O
	PAGE
assumes CS,DV_TEXT
sBegin	DV_TEXT

	SUBTTL Device Dispatch Routines
	PAGE
;***
; B$INIDEV - Device initialization dispatcher
;
;	This routine calls initialization routines for each device
;	in the device table.
;****
cProc	B$INIDEV,<NEAR>,<BX,DI>	
cBegin

	MOV	BX,OFFSET INIDEV_TABLE-2 ;Adjust for no disk entry
	MOV	DI,LAST_DEVICE_OFFSET-2 ;Use actual offset of last device

devlp:	PUSH	BX
	PUSH	DI
	CALL	WORD PTR CS:[BX+DI] ;Dispatch to device routine
	POP	DI
	POP	BX
	DEC	DI
	DEC	DI
	JNZ	devlp		;Loop until device offset = 0

cEnd

	PAGE

;-----	Device Action Routines	---------------------------------------------

;	These routines are always called from special device dispatchers.
;	The registers are set up as follows:
;		(DI) = device offset
;	The remaining registers are used to pass parameters.

devio	PROC	NEAR

;	Device initialization routines
;		Device control blocks are initialized in data segment.
;	ENTRY	(DI) = Device offset
;	USES	any but DI

I_LPT1:
	MOV	b$LPTFDB.FD_MODE,MD_SQO ; Initialize LPRINT file data block
	MOV	b$LPTFDB.FD_DEVICE,DN_LPT1 
	MOV	b$LPTFDB.FD_WIDTH,iniwid   
I_LPT2: 			;no special initialization (at Basic Start-up)
I_LPT3:
I_LPT4:
I_KYBD:	
I_SCRN:
I_CONS:
I_PIPE:
	RET			; just return

;	Device close routines

;	ENTRY	(DI) = Device offset
;	USES	any but DI


KYBD_OPEN:
	XOR	DL,DL		; DL = width
	MOV	BYTE PTR [b$TTY_BAKCHR],DL ; Clear keyboard backup flag/char
	MOV	AH,MD_SQI	;Valid open modes

	JMP	B$OPEN_DEV	; allocate FDB with no buffer

;***
; SCRN_OPEN - open the SCRN: device as a file.  Re-written as part of [16].
;
; Purpose:
;	Create an Fdb associated with a given file number for the screen
;	Allows SCRN: to be opened as either OUTPUT or RANDOM.
; Input:
;	[AL] == file device (?)
;	[BX] == file #
; Output:
;	(ES:)[SI] = *FDB
; Modifies:
;	AX,DX
;****
SCRN_OPEN:
	MOV	DL,[b$SCRNWIDTH]	;(DL) = width
	MOV	AH,MD_SQO+MD_RND	;Set legal modes
	CALL	B$OPEN_DEV		; allocate FDB with no buffer
	FDB_PTR ES,SI,SI		;(ES:)[SI] = *FDB
	MOV	b$FILMOD,MD_SQO 	;Force sequential output
	MOV	FileDB.FD_MODE,MD_SQO	;Force sequential output
	RET

DEVIO	ENDP

	SUBTTL	device table & device dispatch routine
	page
;***
;DevTable -- contains address of all individual device dispatch tables
;
;Purpose:
;	It contains address of all individual device dispatch tables.  The
;	procedure of I/O dispatching is first to get the address of
;	individual device table from here (DevTable) by refering the
;	device number (major #, formed by DN_<device>), and second, to
;	dispatch to the address of desired I/O function from individual
;	device table by refering the function number (minor #, formed by
;	DV_<func>).
;
;	Each individual device dispatch table has the name formed by the
;	prefix $D_ and the device name, e.g. B$D_DISK.
;
;	Each entry in the DevTable is associated with one device number
;	(major #), which has the symbolic name formed by the prefix DN_ and
;	the device name, e.g. DN_KYBD is the device number for keyboard and
;	its value is -1.
;
;	The following table contains names of each individual device table,
;	the associated symbols of device numbers and values, and the module
;	they reside.  DevTable is acutally the first column of the
;	following table.
;	_________________________________________________
;	| B$D_DISK | DN_DISK |	0  | iodisk		|
;	|----------|---------|-----|--------------------|
;	| B$D_KYBD | DN_KYBD | - 1 | iotty		|
;	|----------|---------|-----|--------------------|
;	| B$D_SCRN | DN_SCRN | - 2 | iotty		|
;	|----------|---------|-----|--------------------|
;	| B$D_CONS | DN_CONS | - 3 | iocons		|
;	|----------|---------|-----|--------------------|
;	| $D_CAS1  | DN_CAS1 | - 4 | gwcass		|
;	\..........|.........|.....|....................\
;	| $D_CAS4  | DN_CAS4 | - 7 | gwcass		|
;	|----------|---------|-----|--------------------|
;	| B$D_COM1 | DN_COM1 | - 8 | gwcom		|
;	\..........|.........|.....|....................\
;	| $D_COM8  | DN_COM8 | -15 | gwcom		|
;	|----------|---------|-----|--------------------|
;	| B$D_LPT1 | DN_LPT1 | -16 | iolpt		|
;	\..........|.........|.....|....................\
;	| B$D_LPT4 | DN_LPT4 | -19 | iolpt		|
;	|__________|_________|_____|____________________|
;	* the number of CAS, COM & LPT may vary.
;
;-------
;Individual device dispatch table ($D_<device name>):
;-------
;	Each device dispatch table, which is similar to one column of the
;	following table, contains addresses of all routines (I/O functions)
;	performing special device functions.  Each entry in the device
;	dispatch table is associated with one function number (minor #,
;	actually the offset to that table), which has the symbolic name
;	formed by the prefix DV_ and the function name, e.g. DV_CLOSE (=6).
;
;	All dispatching routines have the name formed by
;	<DeviceName>_<FunctionName>, e.g. DISK_OPEN is the routine which opens
;	the disk file, and COM_CLOSE is the one which closes COM port.
;
;	The following table shows:
;	(1) the functions for each device (in the first column), and
;	(2) the actual working routines for I/O functions of KYBD, SCRN, CONS
;	    and LPT. (The symbolic name of each routine is still formed as
;	    usual, but there doesn't have actual code for that label.  Each
;	    label is defined by EQU to the actual working routine.  For
;	    example, label KYBD_EOF is defined by KYBD_EOF EQU B$ERR_FC.)
;	DISK & COM don't have equates.
;	______________________________________________________________
;	| func	 | KYBD       | SCRN	   | CONS	| LPT	     |
;	|________|____________|____________|____________|____________|
;	| EOF	 | B$ERR_FC   | B$ERR_FC   | B$ERR_BFM	| B$ERR_FC   |
;	|--------|------------|------------|------------|------------|
;	| LOC	 | B$ERR_FC   | B$ERR_FC   | B$ERR_BFM	| B$ERR_FC   |
;	|--------|------------|------------|------------|------------|
;	| LOF	 | B$ERR_FC   | B$ERR_FC   | B$ERR_BFM	| B$ERR_FC   |
;	|--------|------------|------------|------------|------------|
;	| CLOSE  | B$LHDALC   | B$LHDALC   | B$LHDALC	| B$LPTCLOSE |
;	|--------|------------|------------|------------|------------|
;	| WIDTH  | B$ERR_FC   | $SCRNWID   |		|	     |
;	|--------|------------|------------|------------|------------|
;	| RANDIO | B$ERR_FC   | B$ERR_FC   | B$ERR_BFM	| B$ERR_FC   |
;	|--------|------------|------------|------------|------------|
;	| OPEN	 |	      | 	   |		|	     |
;	|--------|------------|------------|------------|------------|
;	| BAKC	 | B$TTY_BAKC | B$ERR_FC   | B$ERR_BFM	| B$ERR_FC   |
;	|--------|------------|------------|------------|------------|
;	| SINP	 | B$TTY_SINP | B$ERR_FC   | B$ERR_BFM	| B$ERR_FC   |
;	|--------|------------|------------|------------|------------|
;	| SOUT	 | B$ERR_FC   | B$TTY_SOUT |		| B$LPTOUT   |
;	|--------|------------|------------|------------|------------|
;	| GPOS	 | B$ERR_FC   | B$TTY_GPOS |		|	     |
;	|--------|------------|------------|------------|------------|
;	| GWID	 | B$ERR_FC   | B$TTY_GWID |		|	     |
;	|--------|------------|------------|------------|------------|
;	| BLKIN  | B$ERR_FC   | B$ERR_FC   | B$ERR_BFM	| B$ERR_FC   |
;	|--------|------------|------------|------------|------------|
;	| BLKOUT | B$ERR_FC   | B$ERR_FC   | B$ERR_BFM	| B$ERR_FC   |
;	|________|____________|____________|____________|____________|
;
;	The following table shows where the routines reside.
;	_________________________________
;	| DISK_xxxx	| dkio.asm	|
;	|---------------|---------------|
;	| KYBD_xxxx	| iotty.asm	|
;	|---------------|---------------|
;	| SCRN_xxxx	| iotty.asm	|
;	|---------------|---------------|
;	| CON_xxxx	| iocons.asm	|
;	|---------------|---------------|
;	| CAS_xxxx	| gwcass.asm	|
;	|---------------|---------------|
;	| COM_xxxx	| gwcom.asm	|
;	|---------------|---------------|
;	| LPT_xxxx	| iolpt.asm	|
;	|_______________|_______________|
;
;*******************************************************************************

	page
;***
;DevTable -- device dispatch table
;
;Purpose:
;	Device dispatch table, which is generated by macros DEVMAC & DEVNAM,
;	is used to select the individual device dispatch table.
;	Note:	DEVMAC redefins DEVMAC in devdef.inc.
;		DEVNAM is defined in devdef.inc
;Entry:
;	none
;Exit:
;	DevTable is set up
;Uses:
;	none
;*******************************************************************************

DEVMAC	MACRO	DeviceName
	EXTRN	B$D_&DeviceName:NEAR	;;all entries are externals
	DW	B$D_&DeviceName 	;;each entry is the address of
					;; individual device table
	ENDM

labelW	DevTable		;device table

	DW	EndOfDevTable-DevTable-2 ; count of bytes in table (not
					 ;	 including count word)
	DEVMAC	DISK		;Disks are treated as device type 0
	DEVNAM			;Rest of devices
EndOfDevTable:			; label for count calculation

sEnd	DV_TEXT
	END
