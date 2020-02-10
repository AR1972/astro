;	SCCSWHAT( "@(#)newexe.inc	1.1 87/12/19 16:42:42	" )
;*
;*	COW : Character Oriented Windows
;*
;*	newexe.inc : Munged version of NEWEXE
;*	* things that are not used for COW have been removed

EMAGIC	= 05A4Dh
ERESWDS =  0010h

EXE_HDR     STRUC
e_magic     DW	?		; magic in same location
e_cblp	    DW	?
e_cp	    DW	?
e_crlc	    DW	?
e_cparhdr   DW	?
e_minalloc  DW	?
e_maxalloc  DW	?
e_ss	    DW	?
e_sp	    DW	?
e_csum	    DW	?
e_cs	    DW	?
e_ip	    DW	?
e_lfarlc    DW	?
e_ovno	    DW	?
e_res	    DW	ERESWDS DUP (?)
e_lfanew    DD	?
EXE_HDR     ENDS


magicNe		= 336Eh		;* 'n3' for fast boot format


NERESBYTES	= 2

NEW_EXE 	STRUC
ne_magic	DW  ?		; Magic value 'ne'
ne_ver		DB  ?		; version number
ne_rev		DB  ?		; revision number
ne_enttab	DW  ?		; offset to entry table
ne_cbenttab	DW  ?		; number of bytes in entry table

ne_crc		DD  ?		; CRC of file

ne_flags	DW  ?		; flag word
ne_autodata	DW  ?		; segment number of auto data segment
ne_heap 	DW  ?		; initial size of local heap
ne_stack	DW  ?		; initial size of stack

ne_csip 	DD  ?		; CS:IP start address
ne_sssp 	DD  ?		; SS:SP initial stack pointer.	0 if
				; stack size word non-zero

ne_cseg 	DW  ?		; number of segment in segment table

IFDEF DUAL
ne_cmod 	DW  ?		; number of entries in module reference table
ne_cbnrestab	DW  ?		; number of bytes in non-resident name table
ELSE ;!DUAL
ne_sectorFix	DW  ?		;* sector where Fixed stuff starts
ne_cparaFix	DW  ?		;* # of fixed paragraphs to read
ENDIF ;!DUAL

ne_segtab	DW  ?		; NE relative offset to segment table
;;;ne_rsrctab	DW  ?		; NE relative offset to resource table
ne_cbRlbMax	DW  ?		;* Maximum size of rlb tables
ne_restab	DW  ?		; NE relative offset to resident name table
IFDEF DUAL
ne_rlbtab	DW  ?		;* offset of Rle entries
ne_imptab	DW  ?		; NE relative offset to imported name table
ne_nrestab	DD  ?		; file offset to non-resident name table
ELSE ;!DUAL
ne_cbRlbFix	DW  ?		;* size of rlbFix
ne_cbRlbMove	DW  ?		;* size of rlbMove
ne_lfaRlbMove	DD  ?		;* where rlbMove is located
ENDIF ;!DUAL

ne_cmovent	DW  ?		; Count of movable entries
ne_align	DW  ?		; Alignment shift count for segment data
ne_res		DB  NERESBYTES DUP (?)
ne_cparaReserve	DW  ?		;* # of reserved paragraphs for code
				;* set by fixstub
ne_rgentmove	DW  ?		;* offset to array of ENTMOVE
ne_psegcsum	DW  ?		; offset to segment chksums.
ne_pretthunks	DW  ?		; offset to return thunks
ne_psegrefbytes DW  ?		; offset to segment ref. bytes
NEW_EXE 	ENDS

;*	* Mini-header for swap files:

EXH	STRUC
magicExh	DW	?		;* magic value (magicNe)
chStampExh	DB	?		;* application version stamp
ifileExh	DB	?		;* ifile index check
lfaRlbExh	DD	?		;* address of RLB
cbRlbExh	DW	?		;* size of RLB
EXH	ENDS


;*	* Segment Table Info

NEW_SEG 	STRUC
ns_sector	DW  ?	; logical sector number in file of start of segment
ns_cbseg	DW  ?	; number bytes in file
ns_flags	DW  ?	; segment flags
ns_minalloc	DW  ?	; minimum number bytes to allocate for segment
NEW_SEG 	ENDS

NEW_SEG1        STRUC
                DB  SIZE NEW_SEG DUP (?)
ns_handle       DW  ?   ; Handle to segment (0 if not loaded)
NEW_SEG1        ENDS

NSTYPE		=  0007h    ; Segment type mask
NSCODE		=  0000h    ; Code segment
NSDATA		=  0001h    ; Data segment
NSMOVE		=  0010h    ; Moveable segment
NSPRELOAD	=  0040h    ; Preload this segment
NSRELOC 	=  0100h    ; Relocation information following segment data

NSDPL		=  0C00h	;;* IO priv level (for file index !!)
SHIFTDPL	=  10


;*	* Entry Table Info

ENT_UNUSED	= 000h
ENT_MOVEABLE	= 0FFh
ENT_PUBLIC	= 001h
ENT_DATA	= 002h


ENT		STRUC
ent_flags	DB	?
ENT		ENDS

ENTM		STRUC
		DB	SIZE ENT DUP (?)
entm_int3f	DW	?
entm_seg	DB	?
entm_offset	DW	?
ENTM		ENDS

ENTF		STRUC
		DB	SIZE ENT DUP (?)
entf_offset	DW	?
ENTF		ENDS


;*	* Moveable entry in RAM - already reloaded & re-lru'd
ENTMOVE		STRUC
opcEntmove	DB  ?			;* JMPF opcode
offEntmove	DW  ?			;* offset
segEntmove	DW  ?			;* segment
segnoEntmove	DB  ?			;* segment #
ENTMOVE		ENDS

;*	* Moveable entry in RAM - Near jump to LOM far jump
ENTMOVE1	STRUC
opcEntmove1	DB  ?			;* CALLN opcode
relEntmove1	DW  ?			;* relative jump - Reload or Relru
offDestEntmove1	DW  ?			;* destination offset
segnoEntmove1	DB  ?			;* segment #
ENTMOVE1	ENDS

;*	* Return thunk (in RAM)
ENTRET		STRUC			;* Return thunk
opcEntret	DB	?		;* CALLN opcode
relEntret	DW	?		;* relative jump - RetThunk
segnoEntret	DB	?		;* segment to return to
offEntret	DW	?		;* offset of top return thunk
ENTRET		ENDS


;*	* Opcodes *
opcCallf	= 09ah			;* CALLF opcode
opcCalln	= 0e8h			;* CALLN opcode
opcJmpn		= 0e9h			;* JMPN opcode
opcJmpf		= 0eah			;* JMPF opcode

;*	* SEGREF byte info (i.e. LRU)
segrefFixed	=	0F0h		;* fixed segment no LRU
segrefLoaded	=	000h		;* just loaded
segrefDiscarded	=	0D0h		;* not resident
segrefBound	=	0B0h		;* bound segment

