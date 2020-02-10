;*
;*	COW : Character Oriented Windows
;*
;*	galloc.inc : global memory allocation.

	include handle.inc

; Global arena objects are kept in a doubly linked list.
; No LRU is maintained (Code segment LRU maintained in segrefbyte table).
;
GlobalArena STRUC
ga_sig	    DB	?	    ; DOS 2.x 3.x signature byte
ga_owner    DW	?	    ; DOS 2.x 3.x owner field (current task)
				;* 0 => free
				;* 1 .. 254 = code segment # (Code blocks)
				;* -1 => special objects (SENTINEL / MOB)
				;* ownerDgroup = DATA GROUP
				;* ownerData = all other USER global data blocks
ga_size     DW	?	    ; DOS 2.x 3.x size, in paragraphs, not incl. header
ga_flags    DB	?	    ; 1 byte available for flags
ga_prev     DW	?	    ; previous arena entry (first points to self)
ga_next     DW	?	    ; next arena entry (last points to self)
ga_handle   DW	?	    ; back link to handle table entry
ga_newpara  DW  ?           ; Paragraph allocated (GA_HOLE only)
ga_reserved DW	?	    ;* RESERVED
				;* ga_lruprev & ga_lrunext not used
GlobalArena ENDS

GA_SIGNATURE	= 04Dh		;* Mark Zbikowski was here !
GA_ENDSIG	= 05Ah
GA_HOLE         = 04Ah          ;* A hole from TSR?

ownerDgroup	EQU	"DG"		;* DGroup
ownerData	EQU	"GD"		;* Global Data

; Global arena objects are aligned on 2 para. boundaries, leaving the
; low order bit always zero.
;
GA_ALIGN    = 2-1
GA_MASK     = NOT GA_ALIGN
GA_FIXED    = 1
Assert	<GA_FIXED EQ GA_ALIGN>

; Flags passed to GlobalAlloc (zero is the default case)
;
GA_ALLOCHIGH	EQU 01h 	; Flag to indicate allocate high
GA_MOVEABLE	EQU 02h
GA_DGROUP	EQU 04h
GA_DISCCODE	EQU 08h
GA_SEGTYPE	EQU 0Ch 	; Above 2 bits stored in he_flags field
GA_NOCOMPACT	EQU 10h
GA_NODISCARD	EQU 20h
GA_ZEROINIT	EQU 40h
; These flags for use by KERNEL only (caller's CS must match)
GA_INTFLAGS	= GA_ALLOCHIGH+GA_SEGTYPE

; High byte of flags remembered in handle table (he_flags field)
;
GA_DISCARDABLE	EQU 01h 	; Boolean flag for global object, not a level.
; User setable handle table flags

; Data structure that describes the global arena.  Allocated at the end
; of the local heap information structure.
;
GlobalInfo  STRUC
	    DB	SIZE HeapInfo DUP (?)
gi_disfence DW	?	; Fence for discardable code.
gi_lrulock  DW	?	; Lock out access to LRU chain from interrupt level
gi_reserve  DW	?	; #paragraphs to reserve for discardable code
			; Zero means not enabled
GlobalInfo  ENDS
gi_cmpflags = hi_dislevel   ; Flags to control gcompact

; Notify procedure message codes
;
GN_MOVE     = 1 	; Object moved - arg1 = handle arg2 = old location
GN_DISCARD  = 2 	; Object discard? - arg1 = handle, arg2 = discard flags
			; Returns new discard flags in AX

