;***
;global.inc - 23-Jun-87 - Run-time Global Data Areas
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	 The data items defined here  must  appear  first  in  the  DATA
;	 segment  of  RT_DATA  because they must exactly match up between
;	 the linked program and the separately linked run-time module.
;
;******************************************************************************
;
; The following data is included in two places. In the runtime module it is
; uninitialized, while the version in the runtime module lib is initialized.
; During RTM all data is copied to user data space except for the data in this
; file. This allows the RTM, compiler, and c runtime to share a limited set of
; data.
;
DefData MACRO	t,nam,val
IFDEF	NO_INITDATA		;;[29]
	global&t nam,?		;; define global data space (uninitialized)
ELSE				;;[29] IFDEF FO_NOINIT
	global&t nam,<val>	;; define global data (initialized)
ENDIF				;;[29] IFDEF FO_NOINIT
;[29] WARNING:  this macro does NOT work right for more than one item on a line!
	ENDM

;[29] An EVEN directive causes problems in this file, since EVEN assembles a
;[29] 90H into the data (NOP opcode).  Don't want initialized data in some
;[29] cases (QB user libs), so we have the EVENMAC macro that generates
;[29] a DB ? instead of a NOP in order to word-align the data.
EVENMAC	MACRO
IF	(($ - DataStart) AND 1)	;;[29] if odd address
	DB	?		;;[29]    make it even
ENDIF				;;[29]
ENDM
;******************************************************************************
;
; Data from here to the start of saved common areas below is local and unique
; to each RTM user program. When a CHAIN occurs, this data is reloaded from the
; copy contained in the chained-to program.
;
;******************************************************************************
	INCLUDE array.inc	;[26]
	INCLUDE rtps.inc	;[28]constants shared with QBI
DataStart label byte		;[29]
	INCLUDE cdata.inc	;[15]include shared C startup data.


	DefData W,b$pend,<OFFSET DGROUP:_end>	      ;[3]End of static data
	DefData W,b$pendchk,<OFFSET DGROUP:_end+STACK_CHECK> ;[3]stack boundary
labelW	<PUBLIC,__bpBC_SAB>			;[32]
	DefData W,b$pBC_SAB,<OFFSET DGROUP:BC_SAB>   ;[3]Ucode start adresses
	DefData W,b$nmalloc_start,<OFFSET DGROUP:NMALLOC>;[12]
	DefData W,b$nmalloc_end,<OFFSET DGROUP:ENMALLOC> ;[12]

	DefData D,b$pULtoRTFar,B$ULtoRTFar ;[10]ptr to UL to RT far call helper
	DefData D,b$pULtoRTNear,B$ULtoRTNear ;[10]ptr to UL to RT near call helper




;******************************************************************************
;
; Data from here to b$CommonLast (which points at the end of the common in the
; user program), preserved across a DOS 3 CHAIN.
;
;******************************************************************************

	EVENMAC 	;[29] EVEN operation that doesn't generate a NOP
	labelW	<PUBLIC,b$RTMOverlayStart> ;[7]start of overlay for CHAIN

	globalB b$Chaining,0	;[6] flag for CHAIN processing
	globalW b$chaindata_len,? ;[6]length of RTM static data

;******************************************************************************
;
; Data from here to b$CommonLast (which points at the end of the common in the
; user program), preserved across a DOS 5 CHAIN.
;
;******************************************************************************

;	Program level

	globalW b$seg,?		;DEF SEG segment value

;	Floating point accumulator
labelB	<PUBLIC,$DAC>
labelB	<PUBLIC,$AC>
	globalB B$DAC,?,4	;double precision accumulator
	globalB B$AC,?,4	;single precision accumulator


labelW	<PUBLIC,b_ULSymSeg>		;Interpreter Reachable Symbol
	globalW b$ULSymSeg,0		;User Library symbol table segment
	globalW b$ULDataLength,0	;length of UL static data
	globalW b$ULDataStart,0	;[10]offset of UL static data in DGROUP
	globalB b$FHDULNearData,0,<SIZE FHD> ;FHD for UL near-data image
	globalB b$FHDULFarData,0,<SIZE FHD>  ;FHD for UL far-data image



	globalB b$CtrlFlags,0		; Misc. control flags
					; (bits defined in CONST.INC)
	globalW b$userflags,0		;compile time flags
	globalW b$mainframe,0		;Main level program BP
	globalW b$curframe,0		;Current program level BP
	globalW b$curlevel,0		;current program level

	globalD b$EVTRETV,0		;Event Trapping Return Dispatch
	labelW	<PUBLIC, b$?TYP>	; word label for b$TTYP and b$VTYP
	labelB <PUBLIC,B$VALTYP>	;
	globalB b$VTYP,0		; value type for print items
	globalB b$TTYP,0		; current type of print item

	globalB	b$LPTFDB,0,FDB_SIZE	; LPRINT file data block

;******************************************************************************
;
; The following data between the STARTZERO and STOPZERO labels gets zeroed on
; initialization, CHAIN, RUN, and CLEAR
;
;******************************************************************************
	EVENMAC		;[29] EVEN operation that doesn't generate a NOP
labelB	STARTZERO			;[10]

	globalW b$PUDS,,2		;[10]Print Using Descriptor

labelD	<PUBLIC,b_erradr>		;Intepreter reachable symbol
	globalD b$erradr,0		;[10]Error offset
labelD	<PUBLIC,b_errmod>		;Intepreter reachable symbol
	globalD b$errmod,0		;[10]pointer to error module name
labelW	<PUBLIC,b_errnum>		;Intepreter reachable symbol
	globalW b$errnum,0		;[10]Error number
labelW	<PUBLIC,b_errlin>		;Intepreter reachable symbol
	globalW b$errlin,0		;[10]Error line
	globalB b$inonerr,0		;[10]flag indicating an ON ERROR GOTO is
					;[10]is currently active.
	globalW b$PTRFIL,,1		;[10]Current File Pointer
	staticW unused,,1		;[46]keeping byte count the same
	globalW b$cCSubs,0		;[10]Count of active subs for compiler in interp
	globalW b$cNonQBIFrames,0	;[21]Count of non QBI frames on stack.
					;[21](that we know about)
labelB	STOPZERO			;[10]

	INCLUDE cdata3.inc		;[47] More shared C startup data.

;******************************************************************************
;
; The following label must be the last data item in this file.
;
;******************************************************************************

	.LIST
