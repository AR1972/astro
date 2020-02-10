TITLE   DOS_ABORT - Internal SFT close all files for proc call for MSDOS
NAME    DOS_ABORT

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**
;
; Internal Abort call closes all handles and FCBs associated with a process.
;  If process has NET resources a close all is sent out over the net.
;
;   DOS_ABORT
;
;   Modification history:
;
;       Created: ARR 30 March 1983
;
;	M038	SR	10/16/90	Free SFT with the PSP of the process
;				being terminated only if it is busy.
;


;
; get the appropriate segment definitions
;


	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	include sf.inc
	include pdb.inc
	include mult.inc
	.cref
	.list

Installed = TRUE

	I_Need  PROC_ID,WORD            ; current process ID
	I_Need  USER_ID,WORD            ; current user ID
	i_need  CurrentPDB,WORD
	i_need  sft_addr,DWORD
	i_need  THISSFT,DWORD
	i_need  JSHARE,DWORD
	I_need  sftFCB,DWORD            ; pointer to SFTs for FCB cache

DOSCODE SEGMENT
	ASSUME  SS:DOSDATA,CS:DOSCODE


Break   <DOS_ABORT -- CLOSE all files for process>
;--------------------------------------------------------------------------
;
; Procedure Name : DOS_ABORT
;
; Inputs:
;       [CurrentPDB] set to PID of process aborting
; Function:
;       Close all files and free all SFTs for this PID
; Returns:
;       None
; All destroyed except stack
;---------------------------------------------------------------------------

	Procedure   DOS_ABORT,NEAR
	ASSUME  DS:NOTHING,ES:NOTHING

	MOV     ES,[CurrentPDB]		; SS override
	MOV     CX,ES:[PDB_JFN_Length]  ; Number of JFNs
reset_free_jfn:
	MOV     BX,CX
	PUSH    CX
	DEC     BX                      ; get jfn (start with last one)

	invoke  $close
	POP     CX
	LOOP    reset_free_jfn          ; and do 'em all
;
; Note:  We do need to explicitly close FCBs.  Reasons are as follows:  If we
; are running in the no-sharing no-network environment, we are simulating the
; 2.0 world and thus if the user doesn't close the file, that is his problem
; BUT...  the cache remains in a state with garbage that may be reused by the
; next process.  We scan the set and blast the ref counts of the FCBs we own.
;
; If sharing is loaded, then the following call to close process will
; correctly close all FCBs.  We will then need to walk the list AFTER here.
;
; Finally, the following call to NET_Abort will cause an EOP to be sent to all
; known network resources.  These resources are then responsible for cleaning
; up after this process.
;
; Sleazy, eh?
;

	context DS			; SS is DOSDATA
	CallInstall Net_Abort, multNet, 29
if installed
	call    JShare + 4 * 4
else
	call    mftCloseP
endif
	assume  ds:nothing
;
; Scan the FCB cache for guys that belong to this process and zap their ref
; counts.
;

					; SS override
	les     di,sftFCB               ; grab the pointer to the table
	mov     cx,es:[di].sfCount
	jcxz    FCBScanDone
	LEA     DI,[DI].sfTable         ; point at table
	mov     ax,proc_id		; SS override
FCBTest:
	cmp     es:[di].sf_PID,ax       ; is this one of ours
	jnz     FCBNext                 ; no, skip it
	mov     es:[di].sf_ref_count,0  ; yes, blast ref count
FCBNext:
	add     di,size sf_Entry
	loop    FCBTest
FCBScanDone:

;
; Walk the SFT to eliminate all busy SFT's for this process.
;
	XOR     BX,BX
Scan:
	push    bx
	invoke  SFFromSFN
	pop     bx
	retc
;M038
; Do what the comment above says, check for busy state
;
	cmp     es:[di].sf_ref_count,sf_busy	; Is Sft busy? ;M038
	jnz      next
;
; we have a SFT that is busy.  See if it is for the current process
;

	mov     ax,proc_id		; SS override
	cmp     es:[di].sf_pid,ax
	jnz     next
	mov     ax,user_id		; SS override
	cmp     es:[di].sf_uid,ax
	jnz     next
;
; This SFT is labelled as ours.
;
	mov     es:[di].sf_ref_count,0
next:
	inc     bx
	jmp     scan

EndProc DOS_Abort

DOSCODE    ENDS
    END


