;****************************************************************************
;
; dosdir.asm - IsDir() and support functions for compress.c,
;              stolen from dos.asm
;
;              Copyright (c) Microsoft Corporation 1989-1991
;              Microsoft Confidential
;              All Rights Reserved.
;
;****************************************************************************

; cmacros.inc options:
memS equ 1                 ; small model
?PLM = 0                   ; C calling convention, not Pascal
?WIN = 0                   ; no Windows prolog\epilog code

include cmacros.inc

COLON         equ ':'
PATH_SEP      equ '\'
NOT_SUPPORTED equ 2h       ; Return code from IsDeviceRemote function.
REMOTE        equ 3h       ; Return code for remote drive found.
TRUE          equ 1h       ; TRUE Definition
FALSE         equ 0h       ; False Definition.

FARPOINTER struc
         off   dw ?
         sel   dw ?
FARPOINTER ends

;=============================================================================

sBegin DATA

include ioctl.inc

sEnd   DATA

;=============================================================================

sBegin CODE
assumes CS, CODE
assumes DS, DATA

;----------------------------------------------------------------------------
;
; GetCurDrive() - returns current drive in AX
;                 (0 --> A, 25 --> Z)
;
;----------------------------------------------------------------------------

cProc GetCurDrive, <FAR, PUBLIC>

cBegin
         mov   ah, 19h              ; Get Current Drive
         int   21h
         xor   ah, ah               ; Zero out AH
cEnd


;----------------------------------------------------------------------------
;
; GetCurDir() - puts current directory in space pointed to by lpszDest
;
;----------------------------------------------------------------------------

cProc GetCurDir, <FAR, PUBLIC>, <DS, SI, DI>

ParmD lpszBuf

cBegin
         cCall GetCurDrive
         mov   si, ax               ; SI = Current drive

         les   di, lpszBuf          ; ES:DI = lpDest
         push  es
         pop   ds                   ; DS:DI = lpDest
         cld
         mov   ax, si               ; AX = Current drive
         inc   al                   ; Convert to logical drive number
         mov   dl, al               ; DL = Logical Drive Number
         add   al, '@'              ; Convert to ASCII drive letter
         stosb
         mov   al, ':'
         stosb
         mov   al, '\'              ; Start string with a backslash
         stosb
         mov   byte ptr es:[di], 0  ; Null terminate in case of error
         mov   si, di               ; DS:SI = lpDest[1]
         mov   ah, 47h              ; Get Current Directory
         int   21h
         jc    GetCurDirExit        ; Skip if error
         xor   ax, ax               ; Return FALSE if no error

GetCurDirExit:
cEnd


;----------------------------------------------------------------------------
;
; SetDrive() - returns the number of drives in AX
;
;----------------------------------------------------------------------------

cProc SetDrive, <FAR, PUBLIC>

ParmW wDrive   ; A --> 0, Z --> 25

cBegin
         mov   dx, wDrive
         mov   ah, 0Eh            ; Set Current Drive
         int   21h
         sub   ah, ah             ; Zero out AH
cEnd


;----------------------------------------------------------------------------
;
; SetDir() - changes the current directory to that specified by lpszDirName,
;            returns 0 in AX if successful, non-0 if not successful
;
;----------------------------------------------------------------------------

cProc SetDir, <FAR, PUBLIC>, <DS, DI>

ParmD lpszDirName

LocalV pszCwd, 128

cBegin
			lds	dx, lpszDirName	 ; DS:DX -> lpszDirName

         mov   bx, dx
         mov   ax, ds:[bx]
         cmp   ah, ':'
         jnz   NoDrive

         ; Convert drive letter to drive index.
         or    al, 20h
         sub   al, 'a'
         xor   ah, ah

         push  dx
         cCall SetDrive, <ax>
         pop   dx

         ; Check for drive-only specification.
         mov   di, dx
         cmp   BYTE PTR [di + 2], 0
         jne   ChDir

         ; Only drive specified.  Get current directory on that drive.
         lea   di, pszCwd         ; ds:di = pszCwd
         cCall GetCurDir, <di, ds>
         lea   dx, pszCwd

NoDrive:
ChDir:
         mov   ah, 3Bh            ; Change Current Directory
         int   21h
         jc    SetDirExit         ; Skip on error
         xor   ax, ax             ; Return 0 if successful

SetDirExit:
cEnd


;----------------------------------------------------------------------------
;
; IsDir() - returns 1 if the directory specified by lpszDir is valid,
;				0 if not
;
; History: M001 - Change to lpszDir's Drive before saving current directory.
;
;----------------------------------------------------------------------------

cProc IsDir, <FAR, PUBLIC>, <DS,ES,SI,DI>		;M001.

ParmD lpszDir FARPOINTER

LocalV pszCwd, 128

cBegin
											; M001 begin.
			cCall GetCurDrive
			mov	di, ax				; DI = Current drive

			les	bx, lpszDir			; ES:BX ->	lpszDir
			push	bx
			mov	ax, es:[bx]
			cmp	ah, ':'				; 1st character of lpszDir = drive letter?
			jnz	id20					; No, jump.

         ; Convert drive letter to drive index.
			or 	al, 20h				; Convert to lowercase.
         sub   al, 'a'
         xor   ah, ah

			cCall SetDrive, <ax> 	; Change to lpszDir's drive.

id20: 	lea	si, pszCwd
			cCall GetCurDir, <si, ds> ; DS:SI -> current dir. on this drive.
			pop	bx 					; ES:BX -> lpszDir.
			cCall SetDir, <bx, es>	; M001 end: Attempt change to lpszDir.
         or    ax, ax
			pushf 						; Save result.
			cCall SetDir, <si, ds>	; M001: Return to original dir. on this drive.

			cCall SetDrive, <di> 	; M001: Restore original drive.

         ; return TRUE (1) if SetDir returns 0, FALSE (0) otherwise

         xor   ax, ax
         popf
         jnz   IsDirExit
         inc   ax

IsDirExit:

cEnd


;*--------------------------------------------------------------------------*
;*                                                                          *
;* BOOL DosIsRemote(int);                                                   *
;*                                                                          *
;* ENTRY:  Word, iPDrive: must be of the form ( logical volume A = 0 )      *
;*               Physical Drive Spec.                          B = 1        *
;*                                                             C = 2        *
;*                                                             ect.         *
;* EXIT: BOOL  returned in AX Remote = Remote                               *
;*                            False  = Local                                *
;*                            not_supported                                 *
;*                                                                          *
;* DESTROYS: AX. (preserves all registers except AX for return value)       *
;*                                                                          *
;* AUTHOR: MC                                                               *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc   DosIsRemote, <FAR, PUBLIC>, <BX,CX,DX,SI,DI,ES,DS>

ParmW   iPDrive                   ; Int Physical drive spec 0 - 25
localV  local_name,     16d       ; Buffer to hold redirected local name.
localV  net_name,      128d       ; Buffer to hold remote device name.
localV  remote_list,    42d       ; Buffer, will be filled with list of
                                  ; redirected local device names.
cBegin

        ; First, we need to make a list of remote devices (logical Volumes)
        ; I have to use DOS call int 21h/5f02h because DOS call int 21h/4409h
        ; is not reliable under DOS versions 4.00 and 4.01. (IBM fuck-up).

        xor     cx,cx
        xor     dx,dx

next_entry:
        mov     bx,cx                 ; CX = redirection list index.
        mov     ax,ss                 ; Load segs for stack vars.
        mov     es,ax
        mov     ds,ax
        mov     ax,5f02h              ; func 5f/02 Get redirection list.
        lea     si,local_name         ; ds:si = local_name
        lea     di,net_name           ; es:di = net_name
        push    cx                    ; save CX
        push    dx                    ; save DX
        int     21h
        pop     dx                    ; restore DX
        pop     cx                    ; restore CX
        jc      check_support         ; error, not supported or end of list.
        cmp     bl,04h                ; Is redirected device a drive ?
        jne     not_a_drive           ; If not, we don't care !

        mov     al,byte ptr ds:[si]   ; Grab volume name.
        sub     al,41h                ; Convert to volume number A=0 ect.
        lea     di,remote_list        ; get a pointer to our remote list.
        add     di,dx                 ; DX is index into list of devices.
        mov     byte ptr es:[di],al   ; save remote device into out list.
        inc     dx                    ; Increment list index pointer.

not_a_drive:
        inc     cx                    ; CX = redirection list index.
        jmp     short next_entry

check_support:
        cmp     ax,12h                ; AX = 12h means end of redir list !
        jne     Game_Over_Man         ; If not then soooo long.
        lea     di,remote_list
        add     di,dx                 ; DX is index into list of devices.
        mov     byte ptr es:[di],0ffh ; Terminate list.

        ;
        ; Is the drive remote ?
        ; See if it matches any entries in list of redirected devices.
        ;

        lea     di,remote_list        ; pointer to list of redirected drives.
        mov     bx,iPDrive            ; Grab the parameter.

next_in_list:
        cmp     byte ptr es:[di],0ffh ; Are we at the end of the redir list ?
        jne     keep_checking
        mov     ax,FALSE              ; Indicate Volume not remote !
        jmp     IsRemoteDone

keep_checking:
        xor     cx,cx
        mov     cl,byte ptr es:[di]
        cmp     bx,cx
        je      Found_Remote
        inc     di
        jmp     short next_in_list

Game_Over_Man:
        mov     ax,NOT_SUPPORTED      ; Indicate no support in return value.
        jmp     short IsRemoteDone

Found_Remote:
        mov     ax,REMOTE             ; Indicate Volume is remote !

IsRemoteDone:

cEnd


;*--------------------------------------------------------------------------*
;*                                                                          *
;* BOOL DosRemoveable(char);                                                *
;*                                                                          *
;* ENTRY: Word iLDrive - Logical drive spec. A - Z. (case does not matter)  *
;*                                                                          *
;* EXIT:  BOOL Returns TRUE if media is removable. False if Remote or fixed *
;*                                                                          *
;* DESTROYS: AX. (preserves all registers except AX for return value)       *
;*                                                                          *
;* AUTHOR: MC                                                               *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc DosRemoveable, <FAR, PUBLIC>, <BX>

ParmW   iLDrive                     ; Logical Drive spec A - Z.

cBegin

        mov     bx,iLDrive          ; Grab the parameter.
        or      bx,20h              ; case insensitive.
        sub     bx,'a'              ; map to A=0
		  cCall	 DosIsRemote, <bx>	; M001: Find out if logical volume is remote
        cmp     ax,NOT_SUPPORTED    ; Not supported usually means no net, so
        je      ok_for_4408         ; drive should be local.
        or      ax,ax               ; check IsRemote return ?
        jnz     Remote_Or_Removable ; False ( 0 ) means local.

        ; Now we have found the volume is local, we can check to
        ; see if it's removable or not.

ok_for_4408:
        mov     ax,4408h            ; IOCTL is removeable
        inc     bx                  ; Map from A=0 to A=1 ...
        int     21h
        jc      Remote_Or_Removable ; error, skip
        or      al,al
        jnz     Remote_Or_Removable ; NOT removeable
        inc     ax
        jnz     ir_exit

Remote_Or_Removable:
        xor     ax,ax               ; Return FALSE for remote or removable.

ir_exit:                            ; Finished.

cEnd

sEnd CODE

END


