;=============================================================================
;== (C) Copyright MICROSOFT Corp. 1989-1991
;=============================================================================
;=============================================================================
;==	E X T E R N A L  D E C L A R A T I O N S
;=============================================================================
LAST	segment
	extrn	MemInit:near
	extrn	MemGet:near
	extrn	MemExit:near
	extrn	TotalFreeMem:near
LAST	ends

;=============================================================================
;==	T Y P E   O F   M E M O R Y :   M e m G e t
;=============================================================================
	extrn	fRSRVD:abs
	extrn	fXMSMem:abs				; XMS memory
	extrn	fBIMMem:abs       ; BIM memory
	extrn	fExtMem:abs       ; Extended memory
	extrn	fSupExt:abs       ; Super extended memory

;=============================================================================
;==	B O U N D A R Y   C O N D I T I O N S :   M e m G e t
;=============================================================================
	extrn	PARA_BOUNDARY:abs	; paragraph boundary conditions
	extrn	PAGE_BOUNDARY:abs	; 4K boundary condition
	extrn	EMS_BOUNDARY:abs	; 16K boundary condition
	extrn	DMA_BOUNDARY:abs	; no 64K or 128K boundary crossings
