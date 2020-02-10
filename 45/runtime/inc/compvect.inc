;***
;compvect.inc - 03-Sep-86  -  Component vector dispatch table structures
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;	There are several dispatch tables that the runtime uses during
;	initialization and termination.  These tables contain addresses
;	of routines that need to be called during initialization or
;	termination.  The tables are initialized to contain ptrs to
;	B$NearRet which only contains a return.  The
;	tables get updated whenever crt0 (the c startup) indirectly
;	calls a runtime routine with its address in the XI segment.
;	The initialization routine that crt0 calls will update the
;	appropriate dispatch tables.  crt0 will call all routines
;	with addresses in the XI segment before _main gets called.
;	This allows us to build up the dispatch tables for _main,
;	and lets us call each initialization routine in an ordered
;	fashion.
;
;
;******************************************************************************

;
;	One time initialization dispatch vector table structure
;
INIT_VECTORS	STRUC
	NM_IVECS	DW	(SIZE INIT_VECTORS)/2-1 ;[21]Number of vectors
	RT_IVEC 	DW	B$NearRet	;ptr to Basic Core routine
	NH_IVEC 	DW	B$NearRet	;ptr to near heap routine
	FH_IVEC 	DW	B$NearRet	;ptr to far heap routine
	CN_IVEC 	DW	B$NearRet	;ptr to console I/O routine
	OS_IVEC 	DW	B$NearRet	;ptr to OS specific routine
	SN_IVEC 	DW	B$NearRet	;[12]ptr to sound routine
	DB_IVEC 	DW	B$NearRet	;ptr to debug routine
INIT_VECTORS	ENDS

;
;	One time termination dispatch vector table structure
;
TERM_VECTORS	STRUC
	NM_TVECS	DW	(SIZE TERM_VECTORS)/2-1 ;[21]Number of vectors
	SN_TVEC 	DW	B$NearRet	;[2]ptr to sound and music routine
	CN_TVEC 	DW	B$NearRet	;ptr to console I/O routine
	RT_TVEC 	DW	B$NearRet	;[15] Terminate Interrupt vects

TERM_VECTORS	ENDS

;
;	"RUN" time initialization dispatch vector table structure.
;
RUN_VECTORS	STRUC
	NM_RVECS	DW	(SIZE RUN_VECTORS)/2-1 ;[21]Number of vectors
	NH_RVEC 	DW	B$NearRet	;ptr to near heap routine
	FH_RVEC 	DW	B$NearRet	;ptr to far heap routine
	RT_RVEC 	DW	B$NearRet	;[14]ptr to Basic Core routine
	CN_RVEC 	DW	B$NearRet	;ptr to console I/O routine
	SN_RVEC 	DW	B$NearRet	;ptr to sound and music routine
	MT_RVEC 	DW	B$NearRet	;ptr to math routine
	GR_RVEC 	DW	B$NearRet	;ptr to graphics routine
	DV_RVEC 	DW	B$NearRet	;ptr to device io routine
	EV_RVEC 	DW	B$NearRet	;ptr to event routine
	DK_RVEC 	DW	B$NearRet	;[18]ptr to disk I/O routine
RUN_VECTORS	ENDS

;
;	"END" time termination dispatch vector table structure.
;
END_VECTORS	STRUC
	NM_EVECS	DW	(SIZE END_VECTORS)/2-1 ;[21]Number of vectors
	DK_EVEC 	DW	B$NearRet	;[18]ptr to disk io routine
END_VECTORS	ENDS

;
;	CLEAR statement "termination" dispatch vector table structure.
;
;	For the clear statement, "termination" are those activities which must
;	be cleaned up prior to the actual reallocation of memory, and the
;	movement of the stack pointer that occurs during the CLEAR statement.
;
CLRT_VECTORS	STRUC				;[1]
	NM_CLTVECS	DW	(SIZE CLRT_VECTORS)/2-1 ;[21] Number of vectors
	SN_CLTVEC	DW	B$NearRet	;[1]ptr to music routine
	DK_CLTVEC	DW	B$NearRet	;[18]ptr to disk io routine
	FH_CLTVEC	DW	B$NearRet	;[16] ptr to far heap routine
	NH_CLTVEC	DW	B$NearRet	;[16] ptr to Near Heap routine
CLRT_VECTORS	ENDS				;[1]

;
;	CLEAR statement initialization dispatch vector table structure.
;
;	For the CLEAR statement, "initialization" are those activities which
;	must occur to reinitialize the system after the stack has been moved,
;	and after memory has been reallocated.
;
CLR_VECTORS	STRUC
	NM_CVECS	DW	(SIZE CLR_VECTORS)/2-1 ;[21] Number of vectors
	RT_CVEC 	DW	B$NearRet	;ptr to Basic Core routine
	GR_CVEC 	DW	B$NearRet	;[4] ptr to graphics routine
	EV_CVEC 	DW	B$NearRet	;[10]ptr to event routine
CLR_VECTORS	ENDS

;
;	SHELL "initialization" dispatch vector table structure.
;
SHLI_VECTORS	STRUC
	NM_SIVECS	DW	(SIZE SHLI_VECTORS)/2-1 ;[21]Number of vectors
	DK_SIVEC	DW	B$NearRet	;[18]ptr to disk I/O routine
	CN_SIVEC	DW	B$NearRet	;ptr to console I/O routine
	FH_SIVEC	DW	B$NearRet	;ptr to far heap routine
	SN_SIVEC	DW	B$NearRet	;[8][3]ptr to sound routine
SHLI_VECTORS	ENDS

;
;	SHELL "termination" dispatch vector table structure.
;
SHLT_VECTORS	STRUC
	NM_STVECS	DW	(SIZE SHLT_VECTORS)/2-1 ;[21]Number of vectors
	SN_STVEC	DW	B$NearRet	;ptr to sound and music routine
	FH_STVEC	DW	B$NearRet	;ptr to far heap routine
	CN_STVEC	DW	B$NearRet	;ptr to console I/O routine
	DK_STVEC	DW	B$NearRet	;[18]ptr to device I/O routine
SHLT_VECTORS	ENDS

;
;	ERROR reset dispatch vector table structure
;
ERROR_VECTORS	STRUC				;[13]
	NM_ERVECS	DW	(SIZE ERROR_VECTORS)/2-1 ;[21]Number of vectors
	DK_ERVEC	DW	B$NearRet	;[18] disk IO
	XH_ERVEC	DW	B$NearRet	;[22] near heap or far heap,
						;..depending on version built
	GR_ERVEC	DW	B$NearRet	;[13] graphics
ERROR_VECTORS	ENDS				;[13]

	.LIST
