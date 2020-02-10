;*
;*	COW : Character Oriented Windows
;*
;*	handle.inc : include file for handles

; Data structure that describes an allocation arena.  Both the local
; and global allocators use this structure at the beginning of their
; information structures.
;
HeapInfo    STRUC
hi_check    DW	?	; arena check word (non-zero enables heap checking)
hi_freeze   DW	?	; arena frozen word (non-zero prevents compaction)
hi_count    DW	?	; #entries in arena
hi_first    DW	?	; first arena entry (sentinel, always busy)
hi_last     DW	?	; last arena entry (sentinel, always busy)
hi_ncompact DB	?	; #compactions done so far (max of 3)
hi_dislevel DB	?	; current discard level
hi_distotal DW	?	; total amount discarded so far
hi_htable   DW	?	; head of handle table list
hi_hfree    DW	?	; head of free handle table list
hi_hdelta   DW	?	; #handles to allocate each time
hi_hexpand  DW	?	; address of near procedure to expand handles for
			; this arena
HeapInfo    ENDS

; Handle table entry.
;
HandleEntry STRUC
he_address  DW	?		    ; actual address of object
he_flags    DB	?		    ; flags and priority level
he_count    DB	?		    ; lock count
HandleEntry ENDS
he_link     EQU he_address	    ; Free handle table entries chained
				    ; through this word
he_owner    EQU he_address	    ; Discarded objects contain owner field
				    ; here so we know when to free handle
				    ; table entries of discarded objects.

HE_SWAPPED	EQU 080h	    ; Bit to mark objects that have been
				    ; handled by the swap module
HE_DISCARDED	EQU 040h	    ; Bit to mark objects that have been
				    ; discarded.

HE_BIT1 	EQU 020h	    ; Available bit
HE_BIT0 	EQU 010h	    ; Available bit
HE_DISCARDABLE	EQU 00Fh	    ; Discard level of this object
HE_USERFLAGS	EQU 03Fh	    ; Mask for user setable flags

HE_FREEHANDLE	EQU 0FFFFh	    ; Use -1 to mark free handle table entries

HE_ALIGN    = 4-1
HE_MASK     = NOT HE_ALIGN

; Handles are allocated in blocks of N, where N is the hi_hdelta field
; in the local heap information structure.  The last word of each block
; of handles is used to thread the blocks together, allowing all handles
; to be enumerated.  The first word of every block is the number of
; handle table entries in the block.  Not only does it save us code
; in henum, but it also has the convenient property of placing all
; handle entries on 2 byte boundaries (i.e. 2, 6, 10, 14), since the
; LA_MOVEABLE bit is 02h.  Thus the address of the he_address field of
; a handle table entry is also the address of the handle table entry
; itself.
;
HandleTable STRUC
ht_count    DW	?		    ; # handletable entries in this block
ht_entry    DB SIZE HandleEntry DUP (?)
HandleTable ENDS
