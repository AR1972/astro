	TITLE	RTSEG - Segment ordering for the BASIC runtime
;***
;RTSEG.ASM - Segment ordering for the BASIC runtime
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	This module contains the segment ordering for the BASIC
;	runtime module.
;
;******************************************************************************

	INCLUDE switch.inc	;assembly switch file
	INCLUDE rmacros.inc	;segment/C macros

;
;	Define segment order for runtime module
;
;
;	Data Segments
;
	USESEG	<NULL>		; BEGDATA (empty for RTMs)
	USESEG	<BR_DATA>	;Hole for user program data

	USESEG	<CONST> 	;runtime constants
	USESEG	<_BSS>		;runtime data (uninitialized)
	USESEG	<_DATA> 	;runtime data (initialized)


	USESEG	<BC_DATA>	;users variables
	USESEG	<BC_FT> 	
	USESEG	<BC_CN> 	
	USESEG	<BC_DS> 	
	USESEG	<BC_SAB>	;beginning of user module start address table
	USESEG	<BC_SA> 	;user module start address table
	USESEG	<BC_SAE>	;end of user module start address table
				; creation of BR_DATA, class BLANK before
				; these segments
	USESEG	<BR_SKYS>	;Soft key definitions
	USESEG	<COMMON>	;User blank COMMON data
	USESEG	<NMALLOC>	;start of near malloc buffer
	USESEG	<ENMALLOC>	;end of near malloc buffer space
	USESEG	<STACK> 	;runtime stack


;
;	Code Segments
;
	USESEG	<RT_TEXT>	;runtime core segment
	USESEG	<NH_TEXT>	;near heap manager
	USESEG	<FH_TEXT>	;far heap manager
	USESEG	<ST_TEXT>	;string functions
	USESEG	<GR_TEXT>	;graphics
	USESEG	<MT_TEXT>	;floating point math
	USESEG	<ER_TEXT>	;error trapping
	USESEG	<EV_TEXT>	;event trapping
	USESEG	<SN_TEXT>	;sound and music
	USESEG	<DV_TEXT>	;device I/O
	USESEG	<CN_TEXT>	;console I/O
	USESEG	<DK_TEXT>	;disk I/O
	USESEG	<OI_TEXT>	;comm/printer I/O
	USESEG	<OS_TEXT>	;operating system functions/features
	USESEG	<DB_TEXT>	;debug utilities /D
	USESEG	<ID_TEXT>	;internal runtime debugging
	USESEG	<INIT_CODE>	;initialization
	USESEG	_TEXT		;c


	INCLUDE seg.inc 	;segment definitions

	END
