;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1990-1991
;
;   Title:	COPYINST.ASM
;
;   Version:	1.00
;
;   Date:	24-Jul-1990
;
;   Author:	RAL
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   24-Jul-1990 RAL Original
;   25-Jul-1990 RAL Updated documentation
;   26-Jul-1990 RAL Fixed 2 bugs
;
;==============================================================================
;
;   This procedure will copy instance data fields from the Windows/386 3.00
;   initialization data structure that specify instance data in Upper Memory
;   Blocks into a table inside of a LIMulator.	When Copy_Inst_Data returns
;   it will have filled in the LIMulator's table and removed the entries from
;   the Win386 initialization data structure.
;
;------------------------------------------------------------------------------

	.386p

	INCLUDE Int2FAPI.Inc


%OUT PUT SEGMENT DECLARATIONS HERE!

;******************************************************************************
;
;   Copy_Inst_Data
;
;   DESCRIPTION:
;	(See file header)
;
;   ENTRY:
;	CX = Maximum number of entries to copy (6 bytes each)
;	ES:DI -> Area to copy instance info into
;		 Table should be of size CX*6+4 (add 4 for term dword 0)
;	ESI = Value passed by Windows 3.00 to LIMulator shut down procedure
;
;   EXIT:
;	If carry flag is clear then
;	    Success!
;	    CX = Number of entries NOT used in table
;	    ES:DI -> Byte past null termination dword in table
;	else
;	    ERROR:  Not enough space in table for all instance items
;
;   USES:
;	DS, EAX, EBX, ECX, EDX, ESI, EDI, EBP, Flags
;
;==============================================================================

Copy_Inst_Data PROC NEAR

	cld

	mov	bx, si
	and	bx, 1111b
	shr	esi, 4
	mov	ds, si

	cmp	BYTE PTR [bx+4Bh], 1
	jne	CID_Success

	mov	esi, DWORD PTR [bx+26h]
	mov	ebx, esi
	shr	ebx, 4
	mov	ds, bx
	and	si, 1111b

;
;   At this point DS:SI -> Win386_Startup_Info_Struc
;
	push	ds
	push	si

	lds	si, [si.SIS_Instance_Data_Ptr]

	mov	ax, ds				; Just to be paranoid...
	or	ax, si
	jz	CID_Success

;
;   Now move all instance fields from the current table into our private
;   table.  This code will shift all instance fields that are not in the
;   Upper Memory region down.  If there are no instance regions remaining
;   after the loop then it will zero out the SIS_Instance_Data_Ptr in the
;   init data area's fake Int 2Fh data structure.
;
	push	si

	mov	bx, si

CID_Move_Data_Loop:
	lodsd
	test	eax, eax			; Q: At the end?
	jz	CID_At_End_Of_List

	mov	ebp, eax
	shr	ebp, 16
	shl	ebp, 4
	movzx	edx, ax
	add	ebp, edx

	cmp	ebp, 0A0000h			; Q: Is this in a UMB?
	jae	CID_Found_One			;    Y: Copy data into us
						;    N: Shift into correct pos
;
;   This entry does NOT specify instance data in a UMB.  Leave it in the
;   init data table.
;
	mov	DWORD PTR [bx], eax
	lodsw
	mov	WORD PTR [bx+4], ax
	add	bx, 6
	jmp	CID_Move_Data_Loop

;
;   This entry specifies instance data in a UMB.  Copy it into our
;   internal table if enough room remains.
;
CID_Found_One:
	dec	cx				; Q: Enough room left?
	jl	CID_Out_Of_Copy_Space		;    N: ERROR!

	stosd
	movsw
	jmp	CID_Move_Data_Loop

;
;   We're at the end of the list of instance regions.  We may have copied
;   all of the instance regions into our internal table.  If so, then zero
;   the SIS_Instance_Data_Ptr in the fake Int 2Fh startup info data structure
;   to indicate that there is no instance data.  Otherwise, null terminate
;   the table and leave the SIS_Instance_Data_Ptr alone.
;
CID_At_End_Of_List:
	pop	si				; SI->Start of original table
	cmp	bx, si				; Q: Has copy pointer moved?
	je	CID_None_Left_In_List		;    N: The table is empty
						;    Y: Still some in table
	mov	DWORD PTR [bx], eax		; Terminate original table
	add	sp, 4				; Junk DS and SI on stack
	jmp	CID_Success			; Return success!

;
;   All of the instance fields were in UMB's -- Zap the entire table
;
CID_None_Left_In_List:
	pop	si
	pop	ds				; DS:SI -> Startup info struc
	mov	[si.SIS_Instance_Data_Ptr], eax ; Zero instance table ptr

;
;   In all cases where we return with success, we must null terminate the
;   table.
;
CID_Success:
	xor	eax, eax
	stosd					; Terminate internal list

	clc					; Indicate that it worked
	ret

;
;   ERROR:  Out of space in internal table.  Clear stack and return
;	    with carry flag set to indicate an error.
;
CID_Out_Of_Copy_Space:
	add	sp, 6

	stc					; Indicate an error
	ret

Copy_Inst_Data ENDP


%OUT PUT SEGMENT DECLARATIONS HERE!


	END
