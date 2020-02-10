;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This is the erroe manager module. Control is transferred to the error mana-;
; -ger when ever any non recoverable error is encountered. The error type is ;
; set in the 'ErrorType' global variable. This routine does the necessary    ;
; tidy up operations before exiting to win main with the error code. In case ;
; a 'window swap in error' happens, a dos exit is called.		     ;
;									     ;
; History:								     ;
;									     ;
;	 Fri June-15-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 Adapted for the Dos Task Switcher.				     ;
;									     ;
;        Tue June-20-1989.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created for Windows. (Added the History legend)      		     ;
;----------------------------------------------------------------------------;


	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include	woaerr.inc
	.list

	;-----------------------------------------------------;
	; declare any external WINDOWS procedures used here.  ;
	;-----------------------------------------------------;


	;-----------------------------------------------------;
	; declare any FAR external Winoldap procedures.       ;
	;-----------------------------------------------------;


	;-----------------------------------------------------;
	; define any public labels,names here.		      ;
	;-----------------------------------------------------;

	public	ErrorManager
	public	WoaAppSwapErr
	public	RecoverWindows
	public	DeleteFilesAndResources


sBegin	Data

	;----------------------------------------------------;
	; declare the global variables to be accessed        ;
	;----------------------------------------------------;

externB		ErrorType		;type of error to handle
externW		WinMainSP		;sp needed to return to WinMain
externW		LowMemArenaSel		;selector for low heap arena
externW		LowMemParaSize		;size of windows low heap
externB		LowMemBlockType		;type of the low heap block
externW 	hApp		   	;windows handle for this instance

	;----------------------------------------------------;
	; define any variables used internally		     ;
	;----------------------------------------------------;

	;----------------------------------------------------;
	; include a file with additional messages. This is in;
	; an incluse file for ease in internationalization.  ;
	;----------------------------------------------------;

		include	woamsg1.inc

sEnd	Data
;----------------------------------------------------------------------------;
sBegin	Code

 	assumes	cs,Code
	assumes	ds,Data
	assumes	es,nothing

	;-----------------------------------------------------;
	; declare any external WOA procedures used here.      ;
	;-----------------------------------------------------;

	externNP	SwapWindowsIn		;(WOAMGR.ASM)
	externNP	RestoreSwapBlock	;(WOAMGR.ASM)
	externNP	OldAppManagerEnd	;(WOAMGR.ASM)
	externNP	EmergencyExit		;(WOAPMRM.ASM)
	externNP	GetDosAppSwapFileName	;(WOAMGR.ASM)
	externNP 	RestoreCtrlCFlag	;(WOAUTILS.ASM)
	externNP	ResetClassInt15Word	;(WOAUTILS.ASM)

;----------------------------------------------------------------------------;

ErrorManager proc near

	mov	al,ErrorType		;load the error type

; isolate 'windows swap in error '

	cmp	al,ER_WND_SWAP_IN	;windows cant be swapped in
	jnz	not_critical_error	;no, we can still recover

; we have crtical error. Clean up as much as possible and exit.

; restore the type and size of the low heap windows block and mark it as
; free.

	mov	es,LowMemArenaSel	;sel for low heap dos block
	mov	al,LowMemBlockType	;get the type of the block
	mov	es:[0],al		;reset it
	mov	ax,LowMemParaSize	;get the original size
	mov	es:[3],ax		;set the size back
	mov	word ptr es:[1],0 	;mark it as free

; now exit back to dos.

	mov	sp,WinMainSP		;load the entry sp value
	jmp	OldAppManagerEnd	;go back to winmain

not_critical_error:

; isolate the routines which occur when window is active, OEM is active

	cmp	al,ER_WND_OK_SCR_OK	;compare against the barrier
	jb	proper_window_active	

; Windows must have been partially or totally been swapped out. We must restore
; it.

	call	RecoverWindows		;restore window after error

; now tidy up and exit to WinMain by reloding the entry stack pointer and doing
; a 'ret'. Also clean up all file that we might have created.

proper_window_active:

; if the instance that is being force-terminated was using INT 15 memory then
; reset the ID in the class word.

	cCall	ResetClassInt15Word,<hApp>

; now delete Files and Resources.

	call	DeleteFilesAndResources

	xor	ah,ah			;zero out ah
	mov	al,ErrorType		;load the error code before exit
	mov	sp,WinMainSP		;load the entry sp value
	jmp	OldAppManagerEnd	;go back to winmain

ErrorManager endp
;----------------------------------------------------------------------------;
; RecoverWindows:							     ;
;									     ;
; This routine retrieves the Windows context when an error occurs. The error ;
; might occur when we are swapping windows out, in which case the swap out   ;
; might be partial. The error might occur after Windows had been swapped out ;
; totally.								     ;
;----------------------------------------------------------------------------;

RecoverWindows proc near

; we have a windows swap out failure. 

	push	ax			;save the code

; now isolate errors where windows code is there, but OEM swapped out

	cmp	al,ER_WND_OK_SCR_GONE	;compare against the barrier
	jae	windows_code_thrashed	;windows code has been overwritten.

	jmp	short windows_code_ok

windows_code_thrashed:

	cmp	al,ER_WND_SWAP_OUT_BAD	;win swap fail after memory thrashed ?
	jnz	not_bad_win_swap_err	;no.

; we have a bad windows swap out error. The memory has been thrashed but 
; swap out fails. In this case windows must be read back. Change the error
; code though.

	pop	ax			;get back the error code
	mov	al,ER_WND_SWAP_OUT	;windows swap out failure
	push	ax			;push it back
	mov	ErrorType,al		;update the error type

not_bad_win_swap_err:

; first get back windows in.
	
	cCall	SwapWindowsIn		;get back windows

windows_code_ok:

	cCall	RestoreSwapBlock	;restore low memory IDT

; re-enable kernels dos traps, before this we must return the state of the
; CTRL+C flag. This had been saved after doing the DisableDos call.

	cCall	RestoreCtrlCFlag	;resturn original state

	pop	ax			;get back error type in al
	mov	ErrorType,al		;save the error type
	ret

RecoverWindows endp
;----------------------------------------------------------------------------;
; DeleteFilesAndResources:						     ;
;									     ;
; Delete all temp files and free up selectors:				     ;
;----------------------------------------------------------------------------;

DeleteFilesAndResources proc near

; free up all work selectors


; clean up files.

	call	GetDosAppSwapFileName	;get the dos swap file name
	call	ErrDeleteFile		;delete it

	ret

DeleteFilesAndResources endp
;----------------------------------------------------------------------------;
; ErrDeleteFile:							     ;
;									     ;
; This routine takes a pointer to a file name in ds:si and deletes the file. ;
; Some of the swap files may have read-only/hidden attributes set.           ;
;----------------------------------------------------------------------------;

ErrDeleteFile  proc  near

	mov	dx,si			;ds:dx has the pointer
	mov	ax,4301h		;set file attributes
	xor	cx,cx			;normal attributes
	int	21h			;normal attributes set
	mov	ah,41h			;delete file code
	int	21h
	ret

ErrDeleteFile  endp
;----------------------------------------------------------------------------;

sEnd	Code

end
