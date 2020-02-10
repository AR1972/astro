	TITLE	SNINIT - Sound and Play Initialization/Termination module
;***
;SNINIT.ASM - Sound/Play initialization/termination module
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains Sound/Music specific initialization
;	and termination support for the BASIC 3.0 runtime.  This module
;	will only be present in a user's program when a program contains
;	statements or features which need Sound/Play support.
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;
;	Code Segments
;
	useSeg	<INIT_CODE>	;Initialization
	useSeg	<SN_TEXT>	;Operating System
	useSeg	<DV_TEXT>	

;
;	Data Segments
;
	useSeg	<_BSS>		;runtime data (uninitialized)
	useSeg	<_DATA> 	;runtime data (initialized)
	useSeg	<XIB>		; XIB and XIE must bracket XI!
	useSeg	<XI>		;initializer segment
	useSeg	<XIE>		

	INCLUDE seg.inc
	INCLUDE compvect.inc	; component vectors
	INCLUDE idmac.inc	
	INCLUDE	ibmunv.inc	

sBegin	DV_TEXT 			
	externNP	B$NearRet	;for disp vectors in compvect.inc
sEnd	DV_TEXT 			

	INITIALIZER	B$xSNINI	;put B$xSNINI in initializer list.

	SUBTTL	Runtime data definitions for BASIC Sound/Music
	PAGE
sBegin	_DATA

;
;	external data
;
	externW b$ini_disp	;One time initialization dispatch table
	externW b$run_disp	;Run time initialization dispatch table
	externW b$term_disp	;One time termination dispatch table
	externW b$clrt_disp	;Clear termination dispatch table
	externW b$shlt_disp	;Shell termination dispatch table
	externW b$shli_disp	;Shell initialization dispatch table

sEnd	_DATA

sBegin	_BSS

;
;	Global Uninitialized Data
;


;***
;b$SNQueSeg - Sound Queue Segment
;
;Purpose:
;	This variable gives the segment in which the sound queue exists.
;	The sound queue is the first 256 bytes of this segment.  It is also
;	used as a flag to prevent multiple deallocations.  If the value of
;	b$SNQueSeg is 0, then the segment is not currently allocated.
;
;	The sound queue is used to hold notes that have been specified by
;	the PLAY statement.  It holds unplayed notes for both the foreground
;	and background music.
;
;Allocation:
;	b$SNQueSeg is a WORD value declared in the _BSS segment by
;	the OEM-Independent code.
;
;Values:
;	0 - No sound queue allocated
;	not 0 - Segment in which sound queue exists.
;
;Initially Set:
;	This variable is statically initialized to be 0 (No Sound Queue).
;	At program initialization (after B$GWINI is called but before
;	any user code is executed) it is set to the location of the
;	sound queue.
;
;Modified By:
;	Once set, this variable should not be modified as long as the
;	sound queue exists.  The sound queue exists until the end of
;	the program or until a RUN command is executed.
;
;Used By:
;	The sound queue is only used by the OEM-Dependent code.  It is
;	not accessed or modified by any OEM-Independent code.
;************************************************************************
	globalW b$SNQueSeg,?,1	;music queue buffer segment
	globalB B$MMODE,?,1
	EVEN

	globalW B$VCEVOL,?,NUM_VOICES
	globalB B$BEATS, ?,NUM_VOICES
	globalB B$NOTE1L,?,NUM_VOICES
	globalB B$NOTELN,?,NUM_VOICES
	globalB B$NOTFLG,?,NUM_VOICES
	globalB B$MSCALE,?,NUM_VOICES
	globalB B$OCTAVE,?,NUM_VOICES


sEnd	_BSS


	externNP	B$BREAK_CHK
	externNP	B$ERR_FC
	externNP	B$DONOTE	
	externNP	B$SNDOFF	
	externNP	B$ERR_OM_FH	
	externNP	B$FHHighAlloc	
	externNP	B$FHHighDealloc 


	SUBTTL	Runtime Sound/Music Initialization
	PAGE
assumes CS,INIT_CODE
sBegin	INIT_CODE

;***
;B$xSNINI - Sound/Music initializer
;PLM B$xSNINI()
;
;Purpose:
;	Initializer for Sound/Music component.	This routine is called
;	by the Crt0 startup before _main is called.  It will update the
;	indirect dispatch tables for the Sound/Music routines.	This
;	insures that the only time that Sound/Music support is accessed
;	is when this module is linked into the user program.
;
;Entry:
;	None.
;
;Exit:
;	Appropriate dispatch vector entries filled.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$xSNINI,<FAR>
cBegin
;
;	update "ONE" time initialization dispatch address to B$SNINI
;
	MOV	WORD PTR [b$ini_disp].SN_IVEC,SN_TEXTOFFSET B$SNINI 
;
;	update "RUN" time initialization dispatch address to B$SNRUN
;
	MOV	WORD PTR [b$run_disp].SN_RVEC,SN_TEXTOFFSET B$SNRUN 
;
;	update "ONE" time termination dispatch address to B$SNTERM
;
	MOV	WORD PTR [b$term_disp].SN_TVEC,SN_TEXTOFFSET B$SNTERM
;
;	update "CLEAR" time termination dispatch address to B$SNCLEAR
;
	MOV	WORD PTR [b$clrt_disp].SN_CLTVEC,SN_TEXTOFFSET B$SNCLEAR
;
;	update "SHELL" time termination dispatch address to B$SNTEM
;
	MOV	WORD PTR [b$shlt_disp].SN_STVEC,SN_TEXTOFFSET B$SNTERM 

;
;	update "SHELL" time initializatiom dispatch address to B$SNINI
;
	MOV	WORD PTR [b$shli_disp].SN_SIVEC,SN_TEXTOFFSET B$SNINI 
cEnd
sEnd	INIT_CODE

assumes CS,SN_TEXT
sBegin	SN_TEXT


;***
;B$SNINI - One time initialization for Sound/Music
;void pascal B$SNINI()
;
;Purpose:
;	Allocate and initialize the music queue.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	AX, BX.
;Exceptions:
;	Out of memory error if allocation fails.
;****

cProc	B$SNINI,<NEAR>		
cBegin

;	First, allocate the memory for the music queue.

	MOV	AX,256		;queue size is 256 bytes
	CWD			;clear DX for doubleword size
	CALL	B$FHHighAlloc	;allocate the music queue buffer
	JCXZ	SNIniOMErr	;Out of memory if no room for buffer
	MOV	b$SNQueSeg,CX	;save segment of memory block
cEnd				

SNIniOMErr:			
	JMP	B$ERR_OM_FH	;out of memory - no room for sound buffer


;***
;B$SNRUN - RUN time initialization for Sound/Music
;void pascal B$SNRUN()
;
;Purpose:
;	Added as part of [5].
;	Reset and initialize the music queue.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	Per convention.
;Exceptions:
;	None.
;****

cProc	B$SNRUN,<NEAR>		
cBegin

;	Turn off any sound and initialize the queue.

	cCall	B$SNDOFF	
	JMP	SHORT INI_QUE
cEnd	<nogen> 		

;***
;B$SNCLEAR - CLEAR time termination for Sound/Music
;void pascal B$SNCLEAR()
;
;Purpose:
;	Reset and reinitialize music.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	None.
;
;****

cProc	B$SNCLEAR,<NEAR>	
cBegin
	cCall	SNDRST
INI_QUE:			
	cCall	B$SNDINI
cEnd

;***
;B$SNTERM - One time termination for Sound/Music
;void pascal B$SNTERM()
;
;Purpose:
;	Flush music queues and deallocate sound buffer if needed.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****

cProc	B$SNTERM,<NEAR> 	
cBegin
	cCall	B$SNDOFF	


;	Deallocate the music queue memory block.
;	Because of the way that RUN/CHAIN works, it is possible for
;	B$SNTERM to be called multiple times.	Make sure that the
;	Sound Queue is deallocated only once.  The value 0 is used
;	to indicate a non-init queue, as it can not occur as a legal
;	segment in either DOS3 or DOS5

	MOV	AX,b$SNQueSeg	;get the memory block segment
	OR	AX,AX		;If the value is zero..
	JZ	NoDealloc	;do not deallocate block
	CALL	B$FHHighDealloc ;deallocate the block
	XOR	AX,AX		;Clear AX
	MOV	b$SNQueSeg,AX	;And flag memory as unallocated
NoDealloc:


cEnd


;***
;B$STRTSD - Start Playing Background Sound
;Purpose:
;	Signal background task to start emptying voice queues and playing
;	music.
;Input:
;	None.
;Output:
;	None.
;Uses:
;	None.
;Exceptions:
;	B$ERR_FC
;****

cProc	B$STRTSD,<PUBLIC,NEAR>
cBegin
	MOV	AL,LOW STRSND	;Function code to start sound
	cCALL	B$DONOTE 	; Pass command to the OEM
	JC	FCERR
cEnd


;***
;B$SNDWAT - Wait until foreground sound has stopped
;Purpose:
;	If Music mode is foreground, wait until all sound
;	activity has stopped, and all voices are silent.  If
;	Music mode is background, return immediately.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	B$ERR_FC
;****

cProc	B$SNDWAT,<PUBLIC,NEAR>
cBegin
	CMP	[B$MMODE],0	;Test Music Mode (0=FG, 255=BG)
	JNE	SNDWAX		;Continue if Music Background

	PUSH	AX		;Save registers used
WAITFG:
	CALL	B$BREAK_CHK	;Check for user break
	MOV	AL,LOW TSTVOC	;Function to test for active voices
	cCALL	B$DONOTE 	; Ask the OEM about it
	JNC	SNDWAT_OK
FCERR:
	JMP	B$ERR_FC	;bad fn call if no data returned
SNDWAT_OK:
	OR	AL,AL		;See if any of them are active
	JNE	WAITFG		;and wait until they aren't
	POP	AX		;Restore the registers
SNDWAX:
cEnd

;***
;SNDRST - Reset Backgound Music
;
;Purpose:
;	SNDRST is called to reset background music.  It is called during
;	initialization from INIT and during the processing of CTL-C
;	from B$BREAK_CHK.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	B$ERR_FC
;****

cProc	SNDRST,<NEAR>
cBegin
	MOV	AX,STPSND	;Flush queue function
	cCALL	B$DONOTE 	; Disable background music, init music queue
	JC	FCERR
cEnd

	SUBTTL	Sound initialization

;***
;B$SNDINI - Initialize Sound Variables
;
;Purpose:
;	B$SNDINI is called to set B$OCTAVE, B$BEATS, B$NOTELN, B$NOTE1L, B$MSCALE,
;	and B$MMODE to appropriate initial settings.
;	B$SNDINI is called at CLEARC and during initialization.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	None.
;****
cProc	B$SNDINI,<PUBLIC,NEAR>,<SI>
cBegin
	MOV	[B$MMODE],LOW 0D

	XOR	SI,SI		;Start with voice 0
SDIN10: MOV	B$BEATS[SI], low 120d
	MOV	B$MSCALE[SI],low 3d
	MOV	B$NOTELN[SI],low 4d
	MOV	B$NOTE1L[SI],low 4d
	MOV	B$OCTAVE[SI],low 4d
	MOV	B$NOTFLG[SI],low 0d

; Initialize the default volume for each voice

	ADD	SI,SI
	MOV	B$VCEVOL[SI],DFLVOL


cEnd


sEnd	SN_TEXT
	END
