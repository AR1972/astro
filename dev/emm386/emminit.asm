.386p
page 58,132
;******************************************************************************
	title	EMMINIT - Expanded Memory Manager initialization
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986
;   (C) Copyright COMPAQ Computer Corp. 1986
;
;   Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:	EMMINIT - Expanded Memory Manager initialization routine
;
;   Version:	0.05
;
;   Date:	November 9, 1988
;
;   Author:	Daniel J. Mazina
;
;******************************************************************************
;
;	Change Log:
;
;	DATE	 REVISION	Description
;	-------- --------	--------------------------------------------
;
;******************************************************************************
;   Functional Description:
;	This module initializes the data structures for the EMM.
;******************************************************************************
.lfcond
page
;******************************************************************************
;			P U B L I C   D E C L A R A T I O N S
;******************************************************************************
public	EMM_Init

page
;******************************************************************************
;			I N C L U D E S
;******************************************************************************
include	vdmseg.inc
include page.inc
include emmfunct.inc
include	emmdata.inc

	page
LAST	segment
	assume	cs:LAST,ds:_DATA,es:_DATA,gs:R_CODE

;******************************************************************************
;	EMM_Init - initialization routine for EMM.
;
;	ENTRY:
;		DS - _DATA segment
;		GS - R_CODE segment
;
;	EXIT:	EMM vector initialized.
;		EMM data structures initialized
;******************************************************************************
EMM_Init	proc	near

		; The registers are saved and the index registers are cleared
		; so that they may be used for 32-bit scaling.
	pushad
	xor	esi,esi
	xor	edi,edi

ifdef QEMS
		; The amount of memory in KB is converted into 4k pages
		; and stored.
	mov	cx,[pool_size]
	shr	cx,2
	mov	gs:[total_4k_pages],cx
	mov	gs:[free_4k_pages],cx
	MOV	GS:[free_pages], CX	; Original free 4K pages	;@PIW
	mov	[first_free_handle_space],FIRST_HANDLE_PTE

		; The high memory area is check to see if there is any.
		; If some is available, then store the starting address
		; in EAX with the number of 4k pages in CX.
	mov	cx,[hi_size]
	shr	cx,2
	jcxz	short EI_check_extended
	mov	eax,[high_memory_address]
	shr	eax,12
	mov	gs:[starting_high_mem_PTE],ax
	mov	gs:[number_high_mem_PTEs],cx

		; The extended memory area is checked to see if there is any.
		; If some is available, then store the starting address
		; in EAX with the number of 4k pages in CX.
EI_check_extended:
	mov	cx,[ext_size]
	shr	cx,2
	jcxz	short EI_setup_handle_zero				;LEO
	mov	eax,[ext_memory_address]
	shr	eax,12
	mov	gs:[starting_ext_mem_PTE],ax
	mov	gs:[number_ext_mem_PTEs],cx
		; The conventional memory below 640k is now allocated to
		; handle 0.
EI_setup_handle_zero:
IFDEF SEP092889
	inc	gs:[handle_count]
	MOV	ESI, [hndl_tbl_ptr]
	MOV	DS:[ESI].base_PTE_index, FIRST_HANDLE_PTE
	mov	ax,[end_of_base_memory_PTE]
	MOV	DX, [strtng_bs_wndw_PTE] ; STARTING_BASE_WINDOW_PTE
	sub	ax, DX
	MOV	DS:[ESI].number_PTEs, AX
	add	gs:[total_4k_pages],ax

		; The conventional memory pool below 640k is now set up.
	mov	gs:[starting_conv_mem_PTE], DX ; STARTING_BASE_WINDOW_PTE
	mov	gs:[number_conv_mem_PTEs],ax

	cmp	ax,0
	je	SHORT EI_init_continue
		; The window mapping for handle zero is now done.  First
		; the beginning window for handle zero is found.
	xor	esi,esi
	mov	cx,TOTAL_EMS_WINDOWS
EI_find_low_window:
	cmp	EMS_window_location[esi*2], DX ; STARTING_BASE_WINDOW_PTE
	je	short EI_map_in_handle_zero
	inc	si
	loop	EI_find_low_window

		; Now the actual handle and logical 4k page pairs are copied
		; in.  BX is the logical page which is 4 for every window.
EI_map_in_handle_zero:
	mov	cx,ax
	shr	cx,2
	xor	bx,bx
EI_init_windows:
	mov	BYTE PTR EMS_window[esi*2+ESI].handle,0
	mov	EMS_window[esi*2+ESI].logical_4k_page,bx
	inc	si
	add	bx,4
	loop	EI_init_windows

		; The amount of memory in bytes needed for the window context
		; save area is calculated and stored for easy reference.
ENDIF
endif
EI_init_continue:
	mov	ax,[number_EMS_windows]
	CMP	[xma2ems], TRUE		; Q: Is it in XMA2EMS mode?	;@PIW
	JNE	SHORT EI_normal		; N:				;@PIW
	MOV	AX, 6			; Save all of them		;@PIW
EI_normal:								;@PIW
ifndef QEMS
	shl	ax,2			; context size
	.errnz  (size EMSmap_struc)-4
else
	MOV	CX, AX			; AX *= 3
	ADD	AX, AX			; Faster than shift left?	;@PIW
	ADD	AX, CX
endif
	add	ax,4			; add header
 	mov	[context_save_area_size],ax

		; The current register set is set to 0 and is activated.
	mov	[current_register_set],0
	mov	register_set[0].active,TRUE

		; The return code in AX is cleared along with the carry.
EIexit:
	popad
	ret
EMM_Init	endp

LAST	ends
	END
