	TITLE	NHINIT.ASM - Near Heap Initialization/Termination module
;***
;NHINIT.ASM - Near Heap  initialization/termination module
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains near heap managment initialization and termination
;	support for the BASIC 3.0 runtime.  This module will only be present
;	in a user's program when a program contains statements which need
;	dynamic memory management.
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;
;	Code Segments
;
	USESEG	<INIT_CODE>	;Initialization
	USESEG	<NH_TEXT>	;Near Heap
	USESEG	<ER_TEXT>	;Error handler
	USESEG	<RT_TEXT>	;RunTime Core
	USESEG	<DV_TEXT>	

;
;	Data Segments
;
	USESEG	<_BSS>		;runtime data (uninitialized)
	USESEG	<_DATA> 	;runtime data (initialized)
	USESEG	<XIB>		; XIB and XIE must bracket XI!
	USESEG	<XI>		;initializer segment
	USESEG	<XIE>		

	INCLUDE seg.inc

	INCLUDE compvect.inc	;component vectors
	INCLUDE messages.inc	;error/message definitions.
	INCLUDE idmac.inc	; debugging macros

	INITIALIZER	B$xNHINI	;put B$xNHINI in initializer list.

	SUBTTL	Code Externals
	PAGE

sBegin	DV_TEXT 			
	externNP	B$NearRet	;for disp vectors in compvect.inc
sEnd	DV_TEXT 			

sBegin	ER_TEXT
	externNP	B$ERR_OM_NH	; Runtime Out of Memory error
	externNP	B$RUNERR	;runtime error handler
sEnd	ER_TEXT

sBegin	NH_TEXT
	externNP B$NHINIT	;routine to initialize and reset near heap.
	externNP B$NHCLEAN	; Clean local heap and string space.
	externNP B$VarHeap_CPCT ;crunches down var heap
	externNP B$LHDALC_CPCT	
	externNP B$STDALCTMP	;erprococ all string temps
	externNP B$STDALCALLTMP ;erprococ all string temps
sEnd	NH_TEXT

	PAGE
	SUBTTL	Runtime data definitions for BASIC Near Heap
sBegin	_DATA

;
;	Global Data
;
	globalW b$NH_first,0	;first word of near heap space
	globalW b$NH_last,0	;last word of near heap space

;
;	external data
;
	externW b$ini_disp	;One time initialization dispatch table
	externW b$run_disp	;RUN time initialization dispatch table
	externW b$clrt_disp	; CLEAR statement support dispatch table
	externW b$err_disp	; error dispatch table
	externW b$pSTDALCALLTMP ; indirect B$STDALCALLTMP vector
	externW b$pSTDALCTMP	; indirect B$STDALCTMP vector
	externW b$pLHDALC_CPCT	; indirect B$LHDALC_CPCT vector

	externW __atopsp	;top word allocated to stack
	externW __asizds	;top of allocated DGROUP (64K or max avail)
	externW b$HugeDelta	
	externB b$HugeShift	

sEnd	_DATA

sBegin	_BSS			
	externW b$curlevel	;defined in NHSTUTIL.ASM
sEnd	_BSS			


	SUBTTL	Runtime Near Heap  Initialization
	PAGE
assumes CS,INIT_CODE
sBegin	INIT_CODE

;***
;B$xNHINI - Near Heap	initializer
;PLM B$xNHINI()
;
;Purpose:
;	Initializer for Near Heap  component.  This routine is called
;	by the Crt0 startup before _main is called.  It will update the
;	indirect dispatch tables for the Near Heap  routines.  This
;	insures that the only time that Near Heap  is accessed is when
;	this module is linked into the user program.
;
;Entry:
;	None.
;
;Exit:
;	Appropriate dispatch vectors filled.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$xNHINI,<FAR>
cBegin
;
;	update "ONE" time initialization dispatch address to B$NHINI
;
	MOV	WORD PTR [b$ini_disp].NH_IVEC,NH_TEXTOFFSET B$NHINI 

;
;	update "RUN" time initialization dispatch address to B$NHCLR
;
	MOV	WORD PTR [b$run_disp].NH_RVEC,NH_TEXTOFFSET B$NHCLR

;
;	update CLEAR statement termination dispatch address to B$NHCLR
;
	MOV	WORD PTR [b$clrt_disp].NH_CLTVEC,NH_TEXTOFFSET B$NHCLR 

;
;	update error dispatch address to B$NHERR
;
	MOV	WORD PTR [b$err_disp].XH_ERVEC,NH_TEXTOFFSET B$NHERR	

;
;	initialize /O granularity vectors
;
	MOV	[b$pSTDALCALLTMP],NH_TEXTOFFSET B$STDALCALLTMP 
	MOV	[b$pSTDALCTMP],NH_TEXTOFFSET B$STDALCTMP       
	MOV	[b$pLHDALC_CPCT],NH_TEXTOFFSET B$LHDALC_CPCT   


cEnd
sEnd	INIT_CODE	

assumes CS,NH_TEXT	
sBegin	NH_TEXT 	

;***
;B$NHINI	- One time initialization for Near Heap
;void pascal B$NHINI()
;
;Purpose:
; BC3
; ---
;	Initializes near heap manager.
;	B$NHINI does the following:
;		allocates all free space in DGROUP.
;		initializes global heap shadow variables.
;
;Entry:
;	None.
;
;Exit:
;	b$NH_first - points to first unallocated word in DGROUP.
;	b$NH_last  - points to last unallocated word in DGROUP.
;
;	Note: these variables must lie on the first byte of an eight
;	byte boundary.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;
;****
cProc	B$NHINI,<NEAR,PUBLIC>	
cBegin

	MOV	b$HugeShift,12	;Huge shift is 12 for DOS 3
	MOV	b$HugeDelta,1000H ;save seg increment for DOS 3
	MOV	CX,__atopsp	;get top of stack
	MOV	AX,__asizds	;get top of allocated DGROUP (64K or max
	AND	AL,0FEH 	;available - round down to next word
	CMP	AX,CX		;check to see if any room for heap
	JB	FATAL		;out of memory if not

;	(CX) = first available byte
;	(AX) = last available byte
	MOV	b$NH_first,CX	;Set starting offset of near heap
	MOV	b$NH_last,AX	;Set ending offset of near heap

	cCall	B$NHRST 	;only reset NH once for interp
cEnd

FATAL:
	MOV	BX,FE_MEMORY	;OUT OF MEMORY error code
	JMP	FAR PTR B$RUNERR	;fatal error - get out of town

;***
;B$NHERR - Reset the Near Heap at error time
;void pascal B$NHERR()
;
;Purpose:
; Deallocates all temp strings
;
;Entry:
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
cProc	B$NHERR,<NEAR>		
cBegin				
	MOV	AX,b$curlevel	;at or above current level...
	CALL	B$STDALCALLTMP ;deallocate all string temps.
cEnd				

;***
;B$NHRST - Reset the Near Heap based upon the shadow values.
;void pascal B$NHRST()
;
;Purpose:
;
;Entry:
;	b$NH_first - points to first byte of the near heap
;	b$NH_last  - points to last word of the near heap
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
cProc	B$NHRST,<NEAR>		
cBegin
	MOV	AX,b$NH_first	;get first word of dynamic space
	MOV	CX,b$NH_last	;get last word of dynamic space
	cCall	B$NHINIT	;Initialize Near Heap
	JNC	RESET_OK	;Was memory set up ok?
	JMP	B$ERR_OM_NH	;Issue "out of memory" error
RESET_OK:
cEnd

;***
;B$NHCLR - Clean string space and local heap
;Purpose:
;	To clean the string space and local heap of entries whose
;	descriptors are not in interpreter entries or other special areas.
;	This routine is used during the RUN init, NEW and CLEAR process.
;
;Entry:
;	None.
;Exit:
;	None.
;Modifies:
;	Per Convention.
;Exceptions:
;	None.
;
;Added as part of [7]
;****
cProc	B$NHCLR,<PUBLIC,NEAR>,ES
cBegin
	XOR	AX,AX		
	MOV	b$curlevel,AX	;set current level to main (0)
	CALL	B$STDALCALLTMP ;deallocate all string temps.
	PUSH	DS		
	POP	ES		; requires ES set
	CALL	B$NHCLEAN	; clean local heap and string space.
	CALL	B$VarHeap_CPCT	
cEnd


sEnd	NH_TEXT
	END
