;***
; smchain.inc - 26-Aug-86
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; This include file contains definitions for shared memory versions of
; the CHAIN statement.
;
;******************************************************************************


;============================================================================
;	Shared Memory header offsets
;
;	The shared memory header basically looks like a list of
;	string descriptors, with a descriptor for each major component
;	of shared memory.  These components include the runtime
;	state variables which are contained in the segment BR_DATA,
;	variables in the blank COMMON segment, string space, and
;	local heap (containing dynamic string arrays and fdb's).
;	Finally, a Far heap descriptor is included which points to
;	the first item in the far heap descriptor list.

SM_HEADER STRUC
	SM_RTD_LEN DW	?		;length of runtime data segment
	SM_RTD_PTR DW	?		;pointer to runtime data data
	SM_COM_LEN DW	?		;length of COMMON segment
	SM_COM_PTR DW	?		;ptr to COMMON data
	SM_SS_LEN DW	?		;length of string space data
	SM_SS_PTR DW	?		;ptr to string space data
	SM_LH_LEN DW	?		;length of local heap data
	SM_LH_PTR DW	?		;ptr to local heap data
	SM_FH_PTR DW SIZE FHD DUP(?)	;Far heap descriptor (start of FHD list)
SM_HEADER ENDS

SM_HEADER_LEN EQU	SIZE SM_HEADER	;length of shared memory header
;============================================================================

	.LIST
