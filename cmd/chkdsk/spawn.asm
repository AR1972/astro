TITLE	SPAWN  - procedures to spawn another program before exiting
;/*
; *                      Microsoft Confidential
; *			 Copyright (C) Microsoft Corporation 1993
; *                      All Rights Reserved.
; */
page	,132

	.xlist
	include chkseg.inc
	include chkmacro.inc
	INCLUDE SYSCALL.INC
	INCLUDE PATHMAC.INC
	.list

;**************************************************************************
; Structures
;**************************************************************************

Exec_Block_Parms struc
Segment_Env	dw	0
Offset_Command	dw	0
Segment_Command dw	0
Offset_FCB1	dw	0
Segment_FCB1	dw	0
Offset_FCB2	dw	0
Segment_FCB2	dw	0
Exec_Block_Parms ends

PSP_OFFSET_FCB1 EQU	05Ch
PSP_OFFSET_FCB2 EQU	06Ch

EXEC_STACK_SIZE EQU	1024

;**************************************************************************
;**************************************************************************
										
data	segment
ASSUME	cs:DG,ds:DG,es:nothing,ss:nothing

	extrn	fatmap:word
	extrn	fattbl_seg:word

	pathlabl spawn

;**************************************************************************
; Data Area
;**************************************************************************

lclExitStatus	db	?		;hold exit status to pass to MS-DOS

Exec_Block	Exec_Block_Parms <>	;EXEC parameter block

		public	EXEC_Path, EXEC_CmdTail

EXEC_Path	db	80 dup (0)	;EXEC pathname

EXEC_CmdTail	db	20 dup (0)	;EXEC command tail


;=========================================================================
; SpawnAndExit	-	This routine spawns another program before exiting
;			CHKDSK.  When the child process terminates, this
;			routines terminates CHKDSK.
;
;			This file is linked first in the CHKDSK image so
;			this this code and data leave a very small stub
;			when Exec'ing the child process.
;
;
;	Inputs	:	AL - Exit code to use when terminating CHKDSK
;			ES - CHKDSK PSP segment
;			EXEC_Path - contains full pathname of pgm to spawn.
;			EXEC_CmdTail - contains cmd tail to pass to child.
;
;	Outputs :	Exits to MS-DOS
;=========================================================================

	assume	CS:DG,DS:DG,ES:NOTHING

	; ShrinkExecExit is actually a part of SpawnAndExit, but located
	; before SpawnAndExit so some of the code can be discarded

ShrinkExecExit	proc	near

	; Switch to local EXEC stack

	mov	ax, ds
	cli
	mov	ss, ax
	mov	sp, cx
	sti

	DOS_Call Setblock		;shrink memory image, bx has size

	; Spawn the child process

	mov	ax, ds
	mov	es, ax
	mov	bx, offset DG:Exec_Block ;es:bx -> parameter block
	mov	dx, offset DG:Exec_Path  ;ds:dx -> program specification

	xor	al, al			 ;exec pgm subfunction
	DOS_Call Exec

	; The child has completed, now terminate CHKDSK.  If lclExitStatus
	; is not 0 return that, otherwise get and return the child's exit
	; status.

	mov	al, lclExitStatus
	jc	see_exit		;Use this status if Exec failed

	or	al, al
	jnz	see_exit		;Status != 0, return it

	DOS_Call WaitProcess		;Our status is 0, get child's status

see_exit:
	DOS_Call Exit

ShrinkExecExit	endp

End_Exec	label	near		;End of code/data kept for Exec'ing
					;  child process

	public	SpawnAndExit
	assume	cs:DG,DS:DG,ES:NOTHING

SpawnAndExit	proc	near

	mov	lclExitStatus, al	;save exit status locally

	; Free other CHKDSK memory blocks to make more mem available to child

	push	es			;save PSP segment
	mov	es, fattbl_seg		;these appear to be the only
	Dos_Call Dealloc		;  other two blocks allocated
	mov	es, fatmap
	Dos_Call Dealloc
        pop     es

	; Build the EXEC call parameter block

	xor	ax, ax
	mov	Exec_Block.Segment_Env, ax

	mov	Exec_Block.Offset_Command, offset DG:EXEC_CmdTail
	mov	Exec_Block.Segment_Command, ds

	mov	Exec_Block.Offset_FCB1, PSP_OFFSET_FCB1
	mov	Exec_Block.Segment_FCB1, es

	mov	Exec_Block.Offset_FCB2, PSP_OFFSET_FCB2
	mov	Exec_Block.Segment_FCB2, es

	; Setup to shrink CHKDSK memory size to make room for child process

	mov	ax, es			;ax = PSP segment
	mov	bx, cs			;bx = data/code segment
	sub	bx, ax			;bx = # paras from psp to data seg
	mov	ax, offset DG:End_Exec
	add	ax, EXEC_STACK_SIZE + 15
	mov	cl, 4
	shr	ax, cl			;ax = siz data seg to keep in paras
	add	bx, ax			;bx = # paras to keep

	mov	cx, offset DG:End_Exec
	add	cx, EXEC_STACK_SIZE + 1
	and	cl, not 1		;cx = word offset of temp exec stack

	jmp	ShrinkExecExit		;go shrink, exec child, and exit

SpawnAndExit	endp

	pathlabl spawn

data	ENDS
	END
