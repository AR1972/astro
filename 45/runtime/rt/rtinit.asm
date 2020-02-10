	TITLE	RTINIT - Initialization module for BASIC runtime
;***
;RTINIT.ASM - Initialization module for BASIC runtime
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains the main entry point and initialization
;	routines for the BASIC 3.0 runtime.  The runtime is designed
;	to achieve modularity and granularity between components of
;	the runtime.  Detailed documentation about the structure of the
;	runtime is presented below.
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - DEF SEG Statement - calls B$DSG0 if no parm, B$DSEG if segment addr given:
;
;      DEF SEG [ = seg]
;
;    Examples:
;
;      DEF SEG				       DEF SEG = 3000
;      -------				       --------------
;	  |					      |
;	B$DSG0					    B$DSEG
;
;******************************************************************************

	INCLUDE switch.inc	;assembly switch file
	INCLUDE rmacros.inc	;segment/C macros

;
;	Code Segments
;
	USESEG	<RT_TEXT>	;runtime core segment
	USESEG	<FH_TEXT>	;far heap segment
	USESEG	<NH_TEXT>	;near heap segment
	USESEG	<DV_TEXT>	;device segment
	USESEG	<INIT_CODE>	;initialization
	USESEG	_TEXT		; c
;
;	Data Segments
;
	USESEG	<NULL>		;BegData

	USESEG	<BR_DATA>	;Hole for runtime module
	USESEG	<_DATA> 	;runtime data (initialized)
	USESEG	<_BSS>		;runtime data (uninitialized)

	USESEG	<NMALLOC>
	USESEG	<ENMALLOC>
	USESEG	<XIB>		;beginning of C initializers
	USESEG	<XI>		;C initializers
	USESEG	<XIE>		;end of C initializers
	USESEG	<BC_DATA>	;users variables
	USESEG	<BC_FT> 	;end of users variables
	USESEG	<BC_SAB>	;beginning of user module start address table
	USESEG	<BC_SA> 	;user module start address table

	INCLUDE seg.inc 	;segment definitions
	INCLUDE compvect.inc	;component vector structures
	INCLUDE fdb.inc		
	INCLUDE const.inc	; bit flag constants
	INCLUDE stack2.inc	; stack related constants
	INCLUDE stack.inc	
	INCLUDE string.inc	

;bring in our versions of C runtime routines.

	externFP __chkstk	; drag in CHKSTK


	externFP __exit 	
	externFP __ctermsub	
	externB	_end		
	externFP _exit		; C _exit proc




	externFP B$ULtoRTFar	;UL to RT far call helper
	externFP B$ULtoRTNear	;UL to RT near call helper

	externFP fEditorActive	

sBegin	NULL			
	INCLUDE global.inc	;global data definitions
sEnd	NULL			




	INCLUDE addr.inc	;compiled module header structure
	INCLUDE idmac.inc	; internal debug macros
	INCLUDE ibmunv.inc	

	INITIALIZER	B$xRTINI	;Put B$xRTINI in initializer list

	SUBTTL	Runtime design
	PAGE
;*****************************************************************************
;*		BASCOM 3.0 and Quick BASIC Interpreter 3.0
;*
;*	The runtime system has been reworked considerably since BASCOM 2.0.
;*	The major goals which prompted the revision of the runtime are
;*	numerous.  A major deficiency in the BASIC language to this point
;*	has been the lack of a truly compatable compiler/interpreter pair.
;*	In order to resolve this deficiency, the BASIC 3.0 runtime
;*	will attempt to achieve this goal without compromising size and
;*	speed goals of either the interpreter or the compiler.
;*
;*	Another major reason for reworking the runtime is to provide support
;*	for the OS/2 environment.  The runtime has been enhanced to
;*	take advantage of new features provided by the DOS.  In addition to
;*	the support of new features. Some existing features have been
;*	reworked to execute under the constraints of OS/2, while still
;*	supporting previous versions of DOS.
;*
;*	There have been several new language enhancements made which
;*	the runtime has been modified to support.  Such items as 4 byte
;*	integers, EGA support, and Advanced 101-key Keyboard support have
;*	been added to the runtime.
;*
;*	The runtime has also been reworked to improve its granularity
;*	through a series of techniques to segment and conditionally
;*	initialize components of the runtime system.
;*****************************************************************************

	SUBTTL	Runtime components, segmentation, and granularity
	PAGE
;*****************************************************************************
;*		Segmentation and Granularity
;*
;*	The initialization and segmentation schemes used by the runtime
;*	attempt to view the runtime as a collection of individual components
;*	which are unique and independent of each other.  The intent is that
;*	a compiled basic module should have the minimal amount of runtime
;*	code present to correctly complete its assigned task.  There are
;*	two major ways to achieve this goal: 1) conditional initialization
;*	of components on an as needed basis and 2) segmentation of compon-
;*	ents, therby allowing the operating system to swap out unused segments
;*	through an LRU mechanism.  We have combined both of these techniques
;*	to produce what we hope is the best of both worlds.
;*
;*
;*	The common runtime is organized into several components, as follows:
;*
;*	Code	Contents
;*	------	---------------------------------------------------------------
;*	CN:	Screen and Keyboard text I/O.
;*	DB:	User debug code /D.
;*	DK:	Disk file related I/O.
;*	DV:	Device Independant IO support.
;*	ER:	Error handling & trapping.
;*	EV:	Event trapping.
;*	FH:	Far heap manager.
;*	GR:	Graphics.
;*	ID:	Internal (development) debugging aids.
;*	MT:	Floating point math.
;*	NH:	Near heap manager.
;*	OI:	Comm/Printer I/O.
;*	OS:	Operating System functions & features.
;*	RT:	Runtime core.
;*	SN:	Sound and music.
;*	ST:	String package.
;*
;*	Each component is further divided into 6 areas, which may or may not
;*	be present for each component:
;*
;*	Initializer	- Updates core dispatch tables for ordered
;*			  initialization, termination, and indirect dispatches.
;*	Initialization	- Component-specific initialization.
;*	Termination	- Component-specific termination.
;*	Dispatch	- Dispatch tables used for indirect access to routines
;*			  within the component which may not always be present.
;*	Core Code	- Code which must always be present for the component.
;*	Component Code	- Code which is present on an as needed basis.
;*
;*	Each component may be contained in its own physical segment, or
;*	combined with other components into common segments. Throughout the
;*	runtime source code, each component is placed in a segment with a
;*	unique logical name. The global segment definitions control the
;*	actual physical segment names used.
;*
;*	In order to minimize the size of user programs, portions of each
;*	component, or the entire component itself, may be optional and
;*	linked/loaded only if referenced by the user program. An "optional
;*	initialization" mechanism is used to initialize components which
;*	require initialization only if they are present. In some cases stubs
;*	or crippled versions of some component routines may be required, even
;*	when the complete component is not present. In these cases indirect
;*	calls are used. The target of the call defaults to the stub, or is
;*	updated as appropriate by the component initialization routine.
;*
;*	If a component needs specialized initialization then this is done
;*	in an ordered manner through indirect dispatches performed on an
;*	as needed basis. This is done by defining far pointers to initializers
;*	in a special segment (XI) which is known to the startup.  When a
;*	module containing the pointer to the initializer gets linked into
;*	the exe file, the pointer to the initializer also gets appended to
;*	the XI segment.  The startup will then indirectly call each routine
;*	which has a pointer in the XI segment before _main is called.  The
;*	initializers for the runtime do not actually initialize the individual
;*	components.  Instead they change a pointer in an initialization vector
;*	to the real initialization routine for the component.  This mechanism
;*	allows the runtime to initialize its components in a well defined
;*	order when _main gets called.  If a component needs specialized
;*	termination then its initializer should also place a pointer to
;*	the termination routine in the termination vector (analogous to
;*	the initialization vector).  When the runtime terminates it will
;*	indirectly call all routines in the termination vector.
;*
;*	Access between some components should be done through
;*	indirect calls.  An example of this would be the
;*	floating input processor FIN.  FIN can handle ints, longs, sp
;*	and dp numbers.  You shouldn't need to load the floating point
;*	math pack if you want to call FIN with an integer.  This can
;*	be accomplished by indirectly calling FIN through a pointer.
;*	If the floating math pack is not loaded then the routine
;*	pointed to by the fin ptr only understands ints and longs.
;*	If the math pack is loaded then the math pack initializer
;*	will change the fin ptr to point to a fin routine which
;*	understands all numeric types.	This type of indirection
;*	can greatly improve the granularity of the runtime.
;*
;*	In addition each component can be composed of other components
;*	(sub components).  With the definition of what a component is
;*	we can come up with a suggested module naming convention.  A
;*	module can be named as follows:
;*
;*		xx(yy)zzzz.ext
;*	where
;*		xx	- is abbreviation of major component (as listed above).
;*		yy	- is the optional abbreviation of a subcomponent such
;*			  as: HL for high level, LL for low level, etc.
;*		zzzz	- description of modules action such as: init, data,
;*			  term, disp, etc.
;*		ext	- is extension for file .asm .c .h .inc .doc etc.
;******************************************************************************

	SUBTTL	Runtime memory maps for BASIC 3.0
	PAGE
;==============================================================================
;
;					      HIGH MEMORY
; Top of MEMORY ----------------------> +-----------------------+
;					| QuickLIB symbols	|
;					| QuickLIB code 	|
;					+-----------------------+
;					|			|
;					| Far heap entries and	|
;					| free space for Huge	|
;					| Numeric arrays Comm	|
;					| buffers and interp	|
; 64k DGROUP boundary ----------+	| tables		|
; b$dynamic_end ----------------+-----> +-----------------------+ LH grows down
;					| Local Heap Space	| -------------
;					|			|	|
;					| contains FDBs 	|	|
;					| Dynamic String Array	|	|
;					| entries and interp	|	|
; flexible boundary between the 	| tables		|	v
; Local Heap and String Space	------> |/\/\/\/\/\/\/\/\/\/\/\/|	-
;					| String Heap Space	|	^
;					|			|	|
;					| contains string	|	|
;					| entries		|------------
;					|			| SS grows up
; b$dynamic_start --------------------> +-----------------------+
; __atopsp ---------------------------> | STACK (class STACK)	| Stack grows
;					|			| down
;					|			| -----------
;					|\/\/\/\/\/\/\/\/\/\/\/\|	|
;					|			|	|
;					|			|	v
; _end -------------------------------> +-----------------------+
;			+-------------> | BC_SA (class BC_SEGS) |
;			|		+-----------------------+
;			|		| BC_SAB (class BC_SEGS)|
;			|		+-----------------------+
;			|		| BC_DS (class BC_SEGS) |
;			|		+-----------------------+
; BASIC program data ---+		| BC_CN (class BC_SEGS) |
;			|		+-----------------------+
;			|		| BC_FT (class BC_SEGS) |
;			|		+-----------------------+
;			+-------------> | BC_DATA (BC_DATA)	|
;					+-----------------------+
;			+-------------> | XCE (class DATA)	|
;			|		+-----------------------+
;			|		| XC (class DATA)	|
;			|		+-----------------------+
;			|		| XCB (class DATA)	|
;			|		+-----------------------+
;			|		| XPE (class DATA)	|
;			|		+-----------------------+
; Initializers ---------+		| XP (class DATA)	|
; and terminators	|		+-----------------------+
;			|		| XPB (class DATA)	|
;			|		+-----------------------+
;			|		| XIE (class DATA)	|
;			|		+-----------------------+
;			|		| XI (class DATA)	|
;			|		+-----------------------+
;			+-------------> | XIB (class DATA)	|
;					+-----------------------+
;					| CDATA (class DATA)	|
; b$common_end -----------------------> +-----------------------+
;					| COMMON (class BLANK)	| <-+preserved
;					+-----------------------+   |
; Soft key string descriptors --------> | BR_SKYS (class BLANK) | --+
; b$common_start ---------------------> +-----------------------+ across chain
; Uninitialized runtime data ---------> | _BSS (class DATA)	|
;					+-----------------------+
; BASIC runtime data (initialized) ---> | _DATA (class DATA)	|
;					+-----------------------+
; BASIC constants --------------------> | CONST (class DATA)	|
;					+-----------------------+
;					| NULL (class BEGDATA)	|
; Beginning of DGROUP ----------------> +-----------------------+
;					| C_ETEXT(class ENDCODE)|
;					+-----------------------+
; BASIC runtime init code ------------> | INIT_CODE(INIT_CODE)	|
;					+-----------------------+
; FAR BASIC runtime code -------------> | *_TEXT (class CODE)	|
;					+-----------------------+
; BASIC runtime code	--------------> | CODE	(class CODE)	|
;					+-----------------------+
; Interpreter code	--------------> | _TEXT (class CODE)	|
;					+-----------------------+
;
;					       LOW MEMORY
;
; _end		- last word of useable stack space.
; __atopsp	- points to first useable word of stack space.
; The BP register is assumed to point to a valid stack frame upon
; entry to the runtime.
;
; b$dynamic_start - points to the first useable word of dynamic space.
; b$dynamic_end   - points to the last useable word of dynamic space.
; NOTE:
;    1) b$dynamic_start and b$dynamic_end are shadow values for dynamic space.
;	the real heap management variables are internal to nh*.asm.
;    2) For OS/2 the above segments may be in any physical order (except
;	for DGROUP).  No assumptions can be made about selectors and the
;	address correlation with physical memory.
;    3) The segments CONST, _DATA, _BSS, and BR_SKYS are aliased by BR_DATA
;	in user programs using the common runtime module.  BR_DATA is filled
;	with blank storage and overlayed by the runtime module during RTM
;	initialization.
;=============================================================================
	SUBTTL Component Initialization and Termination
	PAGE
;******************************************************************************
;	There are several classes of initialization and termination for
;	the runtime.  There is "ONE time" intialization/termination,
;	"RUN/END time" initialization/termination, and
;	initialization/termination actions that take place during
;	"SHELL", "CHAIN", "CLEAR", and "NEW" (QB4 only).
;
;	"ONE time" initialization occurs once when the system is initialized.
;	"RUN time" initialization takes place whenever a program
;	is restarted (RUN, LOAD, NEW).	The other functions occur
;	whenever a "SHELL", "CHAIN", or "CLEAR" statement is executed.
;
;	"One time" termination occurs once when the system is terminated.
;	For the interpreter this occurs when a SYSTEM statement is
;	executed.  For the compiler this happens whenever an END, SYSTEM,
;	Cntrl-Break, end of module, RUN "filename", or CHAIN "filename"
;	(non DOS 3 runtime module) is executed.  The compiler RUN/CHAIN
;	case is somewhat special since we aren't actually returning
;	to the OS.  However, the actions taken are identical to terminating
;	to the OS except that another program is started.  The new program
;	will reinitialize the whole runtime system as if it were started
;	from the DOS command line.
;
;	"END time" termination is specific to the interpreter.	The only
;	thing that happens here is that all files and devices are closed.
;	This will only occur when the interpreter executes an END statement.
;
;	"CHAIN time" and "SHELL time" termination occur whenever a
;	CHAIN (only applicable for interpreter and DOS 3 runtime module
;	for the compiler) or SHELL statement is executed.
;
;	Initialization/termination of each runtime component may need to
;	take specific actions based on the context of the execution
;	environment (compiled or interpreted), the product (QB4, BC3),
;	the DOS (3,5), and (for the compiler) the runtime environment (/O,
;	runtime module).
;******************************************************************************

	SUBTTL C runtime CRT0 support for BASIC runtime
	PAGE
;******************************************************************************
;	Assumptions the runtime makes about startup.
;
;	The runtime assumes that crt0 provides the following
;	initialization support for us:
;		all initializers in XI are called before _main
;		DS, ES, SS are set up to DGROUP
;		__osversion is a word containing DOS major and
;			minor version numbers.
;		__osfile[] is a static array of system file handles
;			which can be checked against 40H to determine
;			if redirection has occurred.
;		__psp contains the PSP segment value
;		__aenvseg contains selector to environment segment for OS/2
;		__psp:[2C] contains environment segment for non-OS/2.
;		_end points to last useable word of STACK
;		__atopsp points to first useable word of stack
;
;	__osversion is a word containing the DOS major and minor
;	version numbers.  The version is stored as MinMaj in
;	a decimal value.  For example DOS 5.00 is 0005H, DOS 2.10
;	is 0A02H etc.
;
;	To determine if input and/or output is redirected,
;	check the file handle in the __osfile[] array against
;	40H (bit 6).  If bit 6 is clear then there is redirection.
;	__osfile[] is a static array of file handles managed by C.
;	__osfile[0] is stdin, __osfile[1] is stdout, etc.
;	NOTE: this is valid for DOS 3, but C is uncertain if this
;	will stay this way for OS/2.
;
;******************************************************************************

	SUBTTL	Code Externals
	PAGE

sBegin	DV_TEXT 			
	externNP	B$IOCLOS	;to de-install interrupt vectors
	externNP	B$IOINI 	;to install interrupt vectors
	externNP	B$NearRet	
sEnd	DV_TEXT 			

	externFP	__fpreset	; floating point reset
	externFP	B$ERR_LLI	; low-level init error
	externFP	B$ERR_DOS	; bad DOS version error




sBegin	FH_TEXT 			
	externNP	B$FHClear	;cleans far heap
sEnd	FH_TEXT 			

sBegin	NH_TEXT 			
	externNP	B$NHCLR 	;cleans near heap
sEnd	NH_TEXT 			

	externFP	B$ULInit	;calls UL "C" initializers
	externFP	B$SETM		; SETMEM function
 	externFP	B$RTMInstall	;install the RTM interrupt vector


	externFP	B$?EVT 		; event initialization



sBegin	RT_TEXT
	externFP	B$GWINI		;Low level initialization
	externNP	B$CNINI 	;drag in console io
	externNP	B$RTLLINI	; Low Level one time core init


externNP B$LHClearRange			; Clear local heap
externNP B$SSClearRange			; Clear string space
externNP B$FHClearRange			; Clear far heap
externNP B$TglHeapSptNEAR		; Toggle to/from var heap support

	externNP	B$DONOTE	;turns off sound for QBI CHAIN


sEnd	RT_TEXT

	SUBTTL	Runtime data definitions for BASIC Runtime Core
	PAGE


sBegin	_BSS
	globalW b$UcodeOff,?		;offset to user code module header
	globalW b$UcodeSeg,?		;segment of user code module

LabelW	<PUBLIC,b_cbComBuffer>	;Interpreter reachable symbol
	globalW b$cbComBuffer,? ;combuffer size specified on QB command line

	externW b$STRTAB 		;Beginning of soft key table

	globalB b$fRTInit,?		;TRUE if RT initialization complete

sEnd	_BSS

sBegin	_DATA

;
;	Global data
;
	EVEN				;insure disp vectors are word aligned

	PUBLIC	b$ini_disp
	PUBLIC	b$term_disp
	PUBLIC	b$run_disp
	PUBLIC	b$end_disp
	PUBLIC	b$clrt_disp		
	PUBLIC	b$clr_disp
	PUBLIC	b$shli_disp
	PUBLIC	b$shlt_disp
	PUBLIC	b$err_disp		

b$ini_disp	INIT_VECTORS	<>	;One time initialization dispatch table
b$term_disp	TERM_VECTORS	<>	;One time termination dispatch table
b$run_disp	RUN_VECTORS	<>	;"RUN" initialization dispatch table
b$end_disp	END_VECTORS	<>	;"END" termination dispatch table
b$clrt_disp	CLRT_VECTORS	<>	; CLEAR "termination" dispatch table
b$clr_disp	CLR_VECTORS	<>	;CLEAR statement dispatch table
b$shli_disp	SHLI_VECTORS	<>	;SHELL re-initialization dispatch table
b$shlt_disp	SHLT_VECTORS	<>	;SHELL "termination" dispatch table
b$err_disp	ERROR_VECTORS	<>	; error vectors

	globalW b$commonfirst,0		;first word of COMMON block
	globalW b$commonlast,0 		;last word of COMMON block
					;output is also sent to lpt.

	PUBLIC	b$fCompErr		
b$fCompErr = OFFSET DGROUP:b$errmod+2	;seg of errmod 0 if interp error
	PUBLIC	b_fCompErr		;Interpeter reachable symbol
b_fCompErr = b$fCompErr 		;Interpeter reachable symbol

sEnd	_DATA


sBegin	NMALLOC 		
	DB	NMALLOC_MIN DUP(?) ; Default malloc buffer - room
sEnd	NMALLOC 		; for header.


	SUBTTL	Runtime Core Initializer
	PAGE
assumes CS,INIT_CODE
sBegin	INIT_CODE

;***
;B$xRTINI - Initializer for the runtime core.
;PLM B$xRTINI()
;
;Purpose:
;	This is the initializer for the BASIC runtime core.  It is placed
;	in the same module as _main so that it is guaranteed to always
;	be present at initialization time.  It's sole responsiblility
;	is to update the initialization and termination dispatch vectors
;	to include the true initialization and termination entry points
;	for the runtime core.
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
cProc	B$xRTINI,<FAR>
cBegin
;
;	update "ONE" time initialization dispatch address to B$RTINI
;
	MOV	WORD PTR [b$ini_disp].RT_IVEC,RT_TEXTOFFSET B$RTINI 
;
;	update "ONE" time termination dispatch address to B$IOCLOS	
;
	MOV	WORD PTR [b$term_disp].RT_TVEC,RT_TEXTOFFSET B$IOCLOS 
;
;	update "RUN" time initialization dispatch address to B$RTRUNINI
;
	MOV	WORD PTR [b$run_disp].RT_RVEC,RT_TEXTOFFSET B$RTRUNINI
;
;	update CLEAR statement dispatch address to B$RTCLR
;
	MOV	WORD PTR [b$clr_disp].RT_CVEC,RT_TEXTOFFSET B$RTCLR
cEnd
	SUBTTL	Runtime Initialization Code
	PAGE
;***
;_main,B$IINIT - Main entry point into the runtime
;main()
;void pascal B$IINIT()
;
;Purpose:
;	This is the main entry point into the runtime.	It is responsible for
;	initializing the components of the runtime which are present and
;	need initializing.  This is done by indirectly calling each routine
;	which has an address defined in b$ini_disp.  This table is built
;	up by conditional initializers which are dragged in at link time.
; BC3
; ---
;	A BASCOM compiled program enters in the C library startup(crt0).  Crt0
;	calls each routine which has its address defined in the segment XI.
;	These initializers will update the dispatch table b$ini_disp.
;	Crt0 will then call runtime at _main.  _main will call all routines in
;	b$ini_disp.  After all components of the runtime have been initialized,
;	_main needs to determine where to begin execution of the user program.
;	BASIC has no concept of a "main" module the compiler does not generate
;	an _main label (the first module in the link is considered to be the
;	entry point).
;	   NOTE: the compiler generates modulename_CODE as the segment name for
;		  the user code.  Therefore the runtime does not know what
;		  segment the users code resides in.
;	The compiler generates a segment called BC_SA of CLASS 'BC_SEGS'
;	which contains the segmented address of every module linked.  Therefore
;	the runtime can pick out the first dword of this segment to obtain the
;	address of the user code module that was linked FIRST.	This address
;	points to the module header of the module.  The actual beginning of
;	the users code is at a fixed offset (O_ENT) from this address.	We can
;	then call the users module to begin execution of the BASIC program.
; QB4
; ---
;	The routine B$IINIT is identical to the compilers _main except
;	that it does not need to find the users program.  All components
;	will be inited in the interpreter.  The initializers will be set
;	up identically for the compiler and interpreter.  Note that B$IINIT
;	is called by the interpreter, initializes and returns.
;
;Entry:
;	DS = ES = user DS for DOS3 RTM
;	SS:SI = ptr to command parameters (QB4 only)
;		[SI]   - UlibNameOffset
;		[SI+2] - UlibNameSegment
;		[SI+4] - cbLibName
;		[SI+6] - cbComBuffer
;Exit:
;	None
;
;Uses:
;	None
;
;Exceptions:
;	May not return if initialization error occurrs.
;****
cProc	B$IINIT,<PUBLIC,FAR>,<SI,ES>	
cBegin

?PLM = 1				

	PUSH	DS			
	POP	ES			
	MOV	AX,[SI+6]		;get requested com buffer size
	MOV	b$cbComBuffer,AX	;save combuffer size



;
;	Call "ONE" time and "RUN" time initialization routines for
;	each component which has an entry defined in b$ini_disp and
;	b$run_disp.  Components not needing initialization point
;	to B$FAR_RET, which just does a far return.
;
	MOV	SI,OFFSET DGROUP:b$ini_disp ;get dispatch table addr
	CALL	FAR PTR B$COMP_DISP	;routine to do table dispatches
	MOV	SI,OFFSET DGROUP:b$run_disp ;get "run" time dispatch table
	CALL	FAR PTR B$COMP_DISP	;do "RUN" time initialization
	call	fEditorActive		; did we start with /EDITOR
	jnz	InEditor		; brif so, don't restrict memory
	MOV	AH,7Fh			; request 7Fxx7Fxx bytes more in
	cCall	B$SETM,<AX,AX>		; order to REDUCE max available
					; memory to 128K (far) + 32K (near)
InEditor:				

	inc	b$fRTInit		; indicate RT is inited

cEnd
?PLM = 1

; common error entry points to save NEAR jumps
BADLL:					
	JMP	B$ERR_LLI		; Low level initialization error

	SUBTTL	Post-RTM-load initialization
	PAGE
;*** 
; B$RTMInit - initialization for RTM programs.
;
;Purpose:
;	Moved from RTMLOAD.ASM with revision [64]. (was called ChainInit)
;	Moved into segment INIT_CODE from RT_TEXT with revision [75].
;
;	To perform initialization specific to RTM programs.
;Entry:
;	SS = DGROUP segment.
;Exit:
;	None.
;Uses:
;	None
;Exceptions:
;
;******************************************************************************


sEnd	INIT_CODE

	SUBTTL	Top Level "RUN" time initialization for QB4
	PAGE
assumes CS,RT_TEXT
sBegin	RT_TEXT

;***
;B$INIT - Initialization prior to XI processing.
;PLM B$INIT()
;
;Purpose:
;	This is the FIRST entry point into the BASIC runtime.  It gets called
;	prior to any of the initialization vectors.  It's primary purpose in
;	life is to clear BC user variables prior to the XI dispatches.
;	The reason that we need to do this here is that near malloc() space
;	comes out of the NMALLOC BASIC common block, and we would wipe out
;	any initial mallocs done in the XI dispatches. Fortran and Pascal
;	both do this kind of allocation.
;	Added with revision [68].
;
;	This routine move from INIT_CODE to RT_TEXT with [88].
;
;Entry:
;	None.
;Exit:
;	The bit flag b$CtrlFlags.BCVarsCleard is set.
;Uses:
;	None.
;Exceptions:
;	None.
;****
cProc	B$Init,<PUBLIC,FAR>,<ES,SI>
cBegin
	MOV	SI,b$pBC_SAB		;get offset to beginning of table
	LES	SI,[SI] 		;ES:SI = module header ptr
	MOV	b$ucodeoff,SI
	MOV	b$ucodeseg,ES
	CALL	B$RTRUNINI		;clear user vars
	OR	b$CtrlFlags,NoInitBcVars ;We have already inited BcVars,
					;B$RtRunini doesn't need to.

cEnd

GenericError:

BADLLI: 				
	JMP	B$ERR_LLI		; Low level initialization error
BADDOS: 				
	JMP	B$ERR_DOS		; Invalid dos version error

	SUBTTL	Runtime Core "ONE" time initialization
	PAGE
;***
;B$RTINI - One time Initializion routine for the runtime core.
;PLM B$RTINI()
;
;Purpose:
;	This is the routine responsible for initializing the runtime
;	core.  This routine checks to make sure that the runtime is
;	running under a reasonable version of DOS.  It also checks
;	to make sure that each object module was compiled with the
;	correct version of the compiler, and that each module was
;	linked with the correct library.  For DOS 3 RTM module versions,
;	this routine causes the runtime module to get loaded.
;	We will also determine if IO has been redirected.
;	We will set up the signal/interrupt handlers for divide by 0,
;	overflow, and HARD disk errors.  For OS/2 versions,
;	if we have been chained to we will get the data that was
;	passed across shared memory.
;	This routine move from INIT_CODE to RT_TEXT with [88].
;
;
; BC3	- void pascal B$RTINI()
; ---
;	This entry point is called B$RTINI.  It has no parameters and
;	performs initialization that is only needed once.  This initialization
;	includes:
;		Checking for reasonable versions of DOS
;		Checking each user module for proper linkage
;			and compiler version
;	  DOS 3 specific:
;		Get/Save machine ID (low level)
;		Set /0, overflow, and int24 handlers
;		Have low level save runtime DS in LL CS (chain/shell/ints)
;	  RTM module specific:
;		Load and fixup RTM module
;		Init RTM module interrupt vectors
;	  OS/2 specific:
;		Set /0, and overflow signal handlers
;		set ignore int24 signal to return an error code
;
; QB4	- void pascal B$RTINI()
; ---
;	The interpreter entry point will also be B$RTINI for "ONE time" init.
;	The interpreter runtime will not need as much "ONE time" initialization
;	as the compiler.  It will still be sensitive to differences in DOS 3 vs
;	OS/2.
;		Checking for reasonable versions of DOS
;	  DOS 3 specific:
;		Get/Save machine ID (low level)
;		Set /0, overflow, and int24 handlers
;		Have low level save runtime DS in LL CS (ints)
;	  OS/2 specific:
;		Set /0, and overflow signal handlers
;		set ignore int24 signal and return an error code
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
;	Doesn't return in following conditions:
;		Wrong version of DOS.
;		Wrong runtime module.(BC3)
;		Linked with wrong library. (BC3)
;		Bad runtime module. (BC3)
;		Not enough memory to load runtime module. (BC3)
;****
cProc	B$RTINI,<NEAR>,<SI>	
cBegin
	PUSH	ES			;ensure ES is preserved.

;	Install the RTM interrupt vector.  This is placed first to
;	to ensure that the vector is always installed and deinstalled
;	once under all circumstances.

; RTM already installed for DOS3.  Couldn't have reached here if not.
	cCall	B$RTMInstall	;install the RTM interrupt vector

;
;	Check for the correct version of DOS.  BASIC .EXEs created for
;	DOS 3 can run in greater than or equal to DOS 2.10.  BASIC .EXEs
;	created for OS/2 can run in only OS/2.	The DOS or OS/2 program
;	loader will ensure that the user can't run a DOS 3 exe in OS/2 or
;	vice versa, so all we have to check here is that we aren't trying
;	to execute a DOS3 .EXE in a DOS less than 2.10.  The current dos
;	version is defined in the C startup in a variable called __osversion.
;	The high byte of this variable contains the decimal value for the
;	minor version number.  The low byte of this variable contains the
;	decimal value for the major version number of dos.  For example
;	2.10 is represented as 0A02H.
;

	MOV	AX,__osversion		; get dos version
	XCHG	AH,AL			; AH = major #, AL = minor #
	CMP	AX,020Ah		; all versions run on >= 2.10
	JB	BADDOS			; brif bad dos version

;
;	Each user module that gets linked into the executable file
;	contains a header which is ~30 bytes.  This header contains
;	information about the module.  The module code begins after
;	the header.  The header is described in addr.inc.  We need
;	to check each module to ensure that it was compiled with
;	the correct version of the compiler, and linked with the
;	appropriate runtime libraries.	A table of module start
;	addresses is emitted by the compiler into the BC_SA segment.
;
	MOV	BX,b$pBC_SAB		;offset from DS of BC_SAB seg
	MOV	AX,[BX+2]		;get user code segment
	MOV	b$ucodeseg,AX		;save user code segment
	MOV	SI,[BX] 		;get user code offset
	MOV	b$ucodeoff,SI		;save user code offset
CHECK_MODULE:
	PUSH	AX			;save segment
	OR	AX,SI			;at end of zero terminated list?
	POP	AX			;recover segment
	JZ	CHECK_MODULE_DONE	;finished if so
	ADD	BX,4			;point to next entry in module table
	MOV	ES,AX			;mov segment to seg reg (AFTER CHECK
					;so no segmentation fault will occur
	MOV	AX,ES:[SI].U_FLAG	;get compile switches
	OR	b$userflags,AX 		;update user flags

NEXT_MODULE:				; label used to save code for RTM
	MOV	SI,[BX] 		;get offset of users module
	MOV	AX,[BX+2]		;get segment of users module
	JMP	SHORT CHECK_MODULE	;check all modules in link

CHECK_MODULE_DONE:


	cCall	B$IOINI			; Init /0, OVF, 24H interrupts


	cCall	B$RTLLINI		;do low level init
	OR	AX,AX			;check for low level error
	JNZ	GenericError		;error out if LL init failed

	cCall	B$GWINI			;do low level init
	JC	BADLLI			;low level error


LLSkip: 				
	POP	ES			;recover ES
cEnd

	PAGE	
;***
;B$CHNINI - CHAIN initialization for QB interpreter.
;void pascal B$CHNINI(*sd)
;
;Purpose:
;	Added with revision [40].
;	This routine provides QBI specific CHAIN support.
;	It will call user library reinitialization, stop
;	any currently playing music, clear any arrays or
;	strings that are not in blank COMMON, reset rt
;	state variables, and reinitialize the EVENT queues.
;Entry:
;	psdCommon - non-zero indicates an sd describing the
;	QBI blank COMMON value table.
;Exit:
;	None.
;Uses:
;	Per convention.
;Exceptions:
;	None.
;****
cProc	B$CHNINI,<PUBLIC,FAR>
parmW	psdCommon
cBegin
	PUSH	b$commonfirst	;save start of compiled common block
	PUSH	b$commonlast	;save end of compiled common block
	INC	b$chaining	;we are chaining, not running
	MOV	BX,psdCommon	;get ptr to common block descriptor
	OR	BX,BX		;should we use interpreters COMMON?
	JZ	UseCompiledCommon ;brif not, use Compiled COMMON

	MOV	CX,[BX+2]	;get ptr to start of QBI common block
	MOV	b$commonfirst,CX ;save it
	ADD	CX,[BX] 	;get ptr to end of QBI common block
	MOV	b$commonlast,CX ;save it

UseCompiledCommon:
	MOV	AL,STPSND	;flag to turn off noise
	cCall	B$DONOTE	;turn off any pending music
	cCall	B$FHClear	;clear non-COMMON huge arrays
	cCall	B$NHCLR	;clear non-COMMON arrays and strings
	cCall	B$CLEAR	;clear rt state variables
	cCall	B$?EVT 	;reset event queue
	cCall	B$ULInit	;reinit user library
	MOV	b$Chaining,0	;clear chaining flag
	POP	b$commonlast	;recover end of compiled common
	POP	b$commonfirst	;recover start of compiled common
cEnd
	PAGE
;***
;B$RUNINI - "RUN time" initialization for QB4 Interpreter.
;void pascal B$RUNINI()
;
;Purpose:
;	This routine is a common runtime entry point for the interpreter.
;	It is responsible for providing the necessary initialization to
;	support the interpreter concept of restartability.  The interpreter
;	will call this routine whenever a RUN or NEW statement is executed.
;	This routine will cause all components needing initialization
;	to be re-initialized.
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
cProc	B$RUNINI,<PUBLIC,FAR,FORCEFRAME>
cBegin
	PUSH	SI
	MOV	SI,OFFSET DGROUP:b$run_disp ;get "run" time dispatch table
	CALL	FAR PTR B$COMP_DISP	     ;do "RUN" time initialization
	POP	SI		
	CALL	B$ULInit	;reinitialize user library.

	MOV	AH,7Fh		; request 7Fxx7Fxx bytes more in order
	cCall	B$SETM,<AX,AX>	; to get back the maximum space for BASIC
cEnd
	SUBTTL	Component generalized dispatcher
	PAGE
;***
;B$COMP_DISP - Indirect component dispatch routine.
;void pascal B$COMP_DISP()
;
;Purpose:
;	This routine will indirectly call each routine which
;	is in the dispatch table which is passed into this
;	routine.  The first word in the dispatch table should
;	contain the number of routines to be called in the table.
;
;Entry:
;	SI	- points to first word of dispatch table.
;
;Exit:
;	None.
;
;Uses:
;	SI
;
;Exceptions:
;	None.
;****
cProc	B$COMP_DISP,<PUBLIC,FAR>
cBegin
	MOV	CX,[SI] 		;number of entries in dispatch table
	JCXZ	DISP_EXIT		;no entries, don't go into loop
DISP_LOOP:				
	LODSW				;point to first routine in table.
	PUSH	CX			;preserve count of routines to check
	CALL	WORD PTR [SI]		;dispatch to init routine (ret if
					;component is not present).
	POP	CX			;restore routine counter
	LOOP	DISP_LOOP		;continue until all routines called
DISP_EXIT:				
cEnd
	SUBTTL	RT Component "RUN" time Initilization
	PAGE
;***
;B$RTRUNINI -	RT Core component "RUN" time initialization.
;void pascal B$RTRUNINI()
;
;Purpose:
;   BC3
;   ---
;	This routine performs the runtime core component initialization that is
;	necessary whenever the runtime system is initialized, a CHAIN statement
;	is executed in the DOS 3 /O environment, or a RUN "filename" statement
;	is executed in the DOS 3 environment.  B$RTRUNINI does the following:
;		Clear user variables (share with CLEAR by calling B$RTCLR)
;		Reset b$traceon and LPR_ECHO flag. [43]
;   QB4
;   ---
;	Same as BC3.
;
;Entry:
;	None.
;
;Exit:
;	b$CtrlFlags & traceon = 0 [61]
;	User variables and runtime state variables cleared.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$RTRUNINI,<NEAR>	
cBegin
	CALL	B$RTCLR 		;Clear all user variables
	AND	b$CtrlFlags,NOT(traceon OR NoInitBcVars) ;reset trace on
					; tracking, and init BC_VARS all
					; subsequent times called.
	cCall	__fpreset		;reset numeric stack
cEnd

	SUBTTL	Interpreter helper
	PAGE
;***
;B$IRTCLR -	B$RTCLR for the interpreter
;
;Purpose:
;	Added with revision [97] to clear QuickLib variables.
;	clear items in blank common, BC_DATA, named common (except NMALLOC).
;	Re-written with revision [98].
;
;	This should not be called if chaining.  B$CHNINI will do the
;	work in that case.
;
;	This should also not be called unless common is shared between
;	the interpreter and the QuickLib.
;
;Entry:
;	None.
;Exit:
;	Variables deallocated, and owners in common or BC_VARS cleared.
;Uses:
;	Per convention.
;
;Exceptions:
;	None.
;****
cProc	B$IRTCLR,<FAR,PUBLIC>,<ES,DI>
cBegin

	cmp	b$ucodeseg,0		; compiled code present?
	jz	NoQlbCode		; brif not -- nothing to do.
	test	b$CtrlFlags,ULInited	; has QuickLib been initialized yet?
	jz	NoQlbCode		; brif not -- nothing to do.
					; (b$nmalloc_start and b$nmalloc_end
					; haven't been properly initialized).

	les	BX,DWORD PTR b$ucodeoff ; ES:BX = ptr to users module

	mov	DI,ES:[BX].OF_COM	; DI = ptr to COMMON block
	mov	CX,b$nmalloc_start	; CX = start of NMALLOC

	cmp	cx,di			; before start of COMMON
	jbe	NoNMALLOC		; brif so -- no QLB nmalloc
	cmp	cx,ES:[BX].OF_FT	; past end of BC_FT? 
	jae	NoNMALLOC		; brif so -- no QLB nmalloc

	push	bx			; save module addr
	call	ClearHelper		; deallocate owners in range + zero
	pop	bx			; restore module addr

	mov	DI,b$nmalloc_end	; DI = end of NMALLOC
NoNMALLOC:
	mov	CX,ES:[BX].OF_FT	; CX = start of BC_FT
	call	ClearHelper		; deallocate owners in range + zero

NoQlbCode:
cEnd

;*** 
;ClearHelper -- helper routine for B$IRTCLR
;
;Purpose:
;	Deallocate all items are in a given range, and zero the range
;	Added with revision [98].
;
;Entry:
;	DI = start addr to zero
;	CX = end addr to zero
;
;Exit:
;	None
;
;Uses:
;	Per convention + DI
;
;Preserves:
;
;Exceptions:
;	None
;
;******************************************************************************
cProc	ClearHelper,<NEAR>,<ES>
cBegin

	push	ds			; set ES=DS
	pop	es

	push	cx			; save end addr
	cCall	<FAR PTR B$ClearRange>,<DI,CX>	; clean out entries whose
					; owners are in range DI ==> CX
	pop	cx			; restore end address

	sub	CX,DI			; CX = count of bytes to clear
	shr	CX,1			; move words not bytes
	xor	AX,AX			; fill with zeros
	REP	STOSW			; clear block at ES:DI
cEnd


	SUBTTL	RT Component CLEAR statement support
	PAGE
;***
;B$RTCLR -	RT Core CLEAR statement support
;void pascal B$RTCLR()
;
;Purpose:
;	Resets all user variables, runtime state variables,
;	and DEF SEG segment.
;
;Entry:
;	None.
;
;Exit:
;	b$seg = DS
;	BC3:
;		COMMON variables cleared
;		USER variables cleared
;	runtime state vars reset
;
;Uses:
;	Per convention.
;
;Exceptions:
;	None.
;****
cProc	B$RTCLR,<NEAR>,<ES,DI>		
cBegin

	CMP	b$ucodeseg,0		;compiled code present?
	JZ	NoCompiledCode		;brif not

	LES	BX,DWORD PTR b$ucodeoff ;load ES:BX with ptr to users module
	MOV	DI,ES:[BX].OF_COM	;get ptr to COMMON block
	MOV	CX,ES:[BX].OF_DAT	; get end of blank COMMON for
					; normal RTM's
	SUB	CX,DI			; get size of blank COMMON

;
;	Set up pointers into COMMON block.  b$commonfirst points to
;	first word of the soft key function table which immediately preceeds
;	the blank COMMON block.  b$commonlast to the end of COMMON block.
;
	PUSH	DS			;get dgroup
	POP	ES			;force ES = DS

	MOV	b$commonfirst,DI	;QB common and soft keys are
	SHR	CX,1			;move words not bytes
	XOR	AX,AX			;fill common with zeros
	REP	STOSW			;CLEAR COMMON block
	MOV	b$commonlast,DI 	;save end of common block

NoCompiledCode: 			
	MOV	b$seg,DS		;Initialize the default DEF SEG segment
	cCall	B$CLEAR 		;clear other user vars and RT data
cEnd
	SUBTTL	B$ClearRange - Clear heap entries in ranges
	PAGE
;***
;B$ClearRange - Clear heap entries in ranges
;void far pascal B$ClearRange(ushort start, ushort end)
;
;Purpose:
; Release all local heap, variable heap, far heap and string entries whose 
; owners fall into a particular range. Except for updating the affected owners,
; other data in range is untouched.
; Exception: Not to be called to clear a range in the variable heap. WILL
;		handle variable heap entries whose owners are in the given
;		range, but the range must not be in the variable heap (the
;		string code, at least, doesn't support this)
;
; The range is inclusive, and must/may include any valid DGROUP range.
; Assumption is made that the heaps will not move during this operation. If
; start>=end, no operation occurs. Debug code asserts that end is greater than
; start. Release code checks, but skips the operation on an invalid range.
;
;Entry:
; rstart = low address of range
; rend	 = ending address of range.
;
;
;Exit:
; none
;
;Uses:
; Per convention, plus preserves ES
;
;******************************************************************************
cProc	B$ClearRange,<FAR,PUBLIC>,ES 
parmW	rstart
parmW	rend
cBegin
	PUSH	DS		; Set ES = DS
	POP	ES		
	MOV	AX,rstart
	MOV	BX,rend


	CMP	AX,BX		; Anything to do?
	JAE	ClearRangeExit	; skip the work if not

	cCall	B$FHClearRange	;Clear far heap

	MOV	AX,rstart
	MOV	BX,rend
	MOV	CX,SP		; CX != 0: release ALL owners in range
	cCall	B$LHClearRange	;Clear local heap

	call	B$TglHeapSptNEAR 
	MOV	AX,rstart	
	MOV	BX,rend		
	MOV	CX,SP		; CX != 0: release ALL owners in range
	cCall	B$LHClearRange	; Clear variable heap
	call	B$TglHeapSptNEAR 
	MOV	AX,rstart
	MOV	BX,rend
	cCall	B$SSClearRange	;Clear string space

ClearRangeExit: 		

cEnd

	SUBTTL
	PAGE
;***
;B$CLEAR - common CLEAR statement support
;void pascal B$CLEAR()
;
;Purpose:
;	Resets user variables, and runtime state variables.
;Entry:
;	None.
;Exit:
;	BC3:
;		USER variables cleared
;	runtime state vars reset
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	B$CLEAR,<NEAR,PUBLIC>,<ES,SI,DI> 
cBegin
	PUSH	DS			;Get dgroup
	POP	ES			;force ES = DS
	XOR	AX,AX			;fill user data with zeros

	TEST	b$CtrlFlags, NoInitBcVars ; have BC_Vars already been
	JNZ	NoCompiledData		; cleared?  brif so -- don't
					; clear them again.  An NMALLOC
					; could have been done by an XI
					; initializer!

	CMP	b$ucodeseg,AX		;compiled code present?
	JZ	NoCompiledData		;brif not

; Clear the named common segments, including NMALLOC
	PUSH	ES			;Save ES
	LES	BX,DWORD PTR b$ucodeoff ;load ES:BX with ptr to users module
	MOV	DI,ES:[BX].OF_DAT	;get ptr to start of user data
	MOV	CX,ES:[BX].OF_FT	;get ptr to end of user data
	POP	ES			;recover ES
	SUB	CX,DI			;compute size of user data
	SHR	CX,1			;move words not bytes
	REP	STOSW			;CLEAR user data block

NoCompiledData: 			
	MOV	DI,OFFSET DGROUP:STARTZERO	;get ptr to start of RT data
	MOV	CX,STOPZERO-STARTZERO	;compute size of RT data
	REP	STOSB			;clear runtime data state vars


;
; Fill the stack from [b$pend] to [SP] for known data, for use by FRE(-2)
;
	MOV	BX,SP			; location to end at
	cCall	<FAR PTR B$STACKINIT>	
cEnd
	SUBTTL	B$STACKINIT - Init Stack Area
	PAGE
;*** 
;B$STACKINIT - Init Stack Area
;
;Purpose:
; Load stack area with known data for use by FRE(-2)
;
;Entry:
; [BX]	=  Stack Address at which to STOP filling
;
;Exit:
; none
;
;Uses:
; per convention + DI & ES
;
;******************************************************************************
cProc	B$STACKINIT,<FAR,PUBLIC>
cBegin				
	TEST	b$CtrlFlags,NoSTACKINIT ; B$STACKINIT disabled?
	JNZ	StackInitExit	; brif so -- just exit
	SUB	BX,4		; Adjust BX for return address on stack
	PUSH	SS		
	POP	ES		
	MOV	DI,[b$pend]	; [ES:DI] points to stack area
	MOV	AX,-STACK_MIN	; word to be written to stack area
InitStackLoop:			
	STOSW			; put into unused stack space
	INC	AX		; bump count of available bytes
	INC	AX		
	CMP	DI,BX		; have we reached our stack?
	JC	InitStackLoop	; loop until stack filled

StackInitExit:			
cEnd				

	SUBTTL	Def Seg setup routines.
	PAGE

;***
; B$DSG0 - Set BASIC Segment Address to Data Segment (DEF SEG w/no parameter)
;
; Purpose:
;  Runtime Entry Point. Save DS in b$seg
;
; Input:
;  NONE
;
; Output:
;  [b$seg] == DS
;
; Modifies:
;	NONE
;
;******************************************************************************
cProc	B$DSG0,<PUBLIC,FAR>	
cBegin				
	MOV	[b$seg],DS	;Save initial data seg
cEnd				

;***
; B$DSEG - Set BASIC Segment Address (DEF SEG w/parameter)
;
; Purpose:
;  Runtime Entry Point. Save given segment address in b$seg
;
; Input:
;  new segment address
;
; Output:
;  [b$seg] set.
;
; Modifies:
;	NONE
;
;******************************************************************************
cProc	B$DSEG,<PUBLIC,FAR>	
parmW	newseg			
cBegin				
	MOV	AX,newseg	; get...
	MOV	[b$seg],AX	; and save new b$seg value
cEnd				

;*** 
;B$GetCompSwitches
;
;Purpose:
;	Added with revision [85].  The interpreter calls this routine
;	to find out whether or not any QuickLIB modules were compiled
;	with /V or /W.	Since the interpreter uses both or neither of
;	these, we just return TRUE or FALSE with no differentiation
;	between V or W.
;Entry:
;	None
;Exit:
;	AX=0  if no QuickLIB modules were compiled with /V or /W
;	AX<>0 if at least one QuickLIB routine was compiled /V or /W
;Uses:
;	None
;Exceptions:
;	None
;******************************************************************************
cProc	B$GetCompSwitches,<PUBLIC,FAR>
cBegin
	MOV	AX,[b$userflags]	;get switches used
	AND	AX,u_sw_w + u_sw_v	;mask down to just /V and /W
cEnd					;AX = 0 if no /V or /W, else nonzero

	page

sEnd	RT_TEXT
	END
