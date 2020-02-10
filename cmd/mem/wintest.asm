;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1988 - 1993
; *                      All Rights Reserved.
; */
;
; Checks to see if Windows is running.  Returns 1 if Windows 3.0 or
;   above is running, 0 otherwise.


.MODEL SMALL


TRUE                EQU   1

NO_WINDOWS          EQU   0
WIN_286             EQU   1
WIN_386             EQU   2
WIN_REAL_MODE       EQU   3
WIN_STANDARD_MODE   EQU   4
WIN_ENHANCED_MODE   EQU   5
WIN_UNKNOWN_MODE    EQU   6

wWindowsType        EQU   Word Ptr [bp-02]
wWindowsMajor       EQU   Word Ptr [bp-04]
wWindowsMinor       EQU   Word Ptr [bp-06]
fDosShell           EQU   Word Ptr [bp-08]

.CODE

public _IsWindowsRunning


_IsWindowsRunning proc

; Make room for local variables

  push    bp
  mov     bp,sp
  sub     sp,8

; Set fDosShell to FALSE

  xor     ax,ax                   ; Zero out fDosShell
  mov     fDosShell,ax


;*************************************************************************
;* The following code was taken from MSD 2.01's OSINFO.C, WinVerDetect() *
;*************************************************************************

; Check for Windows 3.1

  mov     ax,160Ah                ; WIN31CHECK
  int     2Fh                     ; check if running under win 3.1
  or      ax,ax
  jnz     Win30EnhModeCheck

; Windows 3.1 detected

  mov     wWindowsMajor,3         ; Set the version number
  mov     wWindowsMinor,10

;   CX = 3 - Enhanced, CX = 2 - Standard, CX = 1 - Real.

  cmp     cx,1
  jne     Win31StdChk
  mov     wWindowsType, WIN_REAL_MODE
  jmp     WinDetectComplete

Win31StdChk:

  cmp     cx,2
  jne     Win31EnhChk
  mov     wWindowsType, WIN_STANDARD_MODE
  jmp     WinDetectComplete

Win31EnhChk:

  cmp     cx,3
  jne     Win31UnknownMode
  mov     wWindowsType, WIN_ENHANCED_MODE
  jmp     WinDetectComplete

Win31UnknownMode:

  mov     wWindowsType, WIN_UNKNOWN_MODE
  jmp     WinDetectComplete


; Check for 3.0 Enhanced mode

Win30EnhModeCheck:
  mov     ax,1600h                ; WIN386CHECK
  int     2Fh
  test    al,7Fh
  jz      Win286Check

; Windows 3.0 Enhanced Mode detected

  mov     wWindowsMajor,3         ; Set the version number
  mov     wWindowsMinor,0
                                  ; Set the mode
  mov     wWindowsType, WIN_ENHANCED_MODE
  jmp     WinDetectComplete


; Check for Windows/286

Win286Check:
  mov     ax,1700h                ; WIN286CHECK
  int     2Fh
  cmp     al,2h                   ; If /286 installed, ver = AL.AH
  jnz     WinOldApCheck           ; /286 is always 2.x

; Windows/286 detected

  xor     bh,bh
  mov     bl,al
  mov     wWindowsMajor,bx
  mov     bl,ah
  mov     wWindowsMinor,bx
  mov     wWindowsType, WIN_286
  jmp     WinDetectComplete


; Check for Windows 3.0 WINOLDAP

WinOldApCheck:
  mov     ax,4680h                ; IS_WINOLDAP_ACTIVE
  int     2Fh
  or      ax,ax                   ; running under 3.0 derivative ?
  jz      DosShellCheck

; Windows is not running on this computer

  jmp     NotRunningUnderWin


; Check for DOS 5.0 DOSSHELL Task Switcher

DosShellCheck:
  mov     ax,4b02h                ; detect switcher
  push    bx
  push    es
  push    di
  xor     bx,bx
  mov     di,bx
  mov     es,bx
  int     2Fh
  pop     di
  pop     es
  pop     bx
  or      ax,ax
  jnz     RunningUnderWinStdReal30

; Running under DOS 5.0 task switcher

  mov     wWindowsMajor,0         ; Windows is not running
  mov     wWindowsMinor,0
  mov     wWindowsType, NO_WINDOWS
  mov     fDosShell, TRUE         ; Set the flag for the DOSSHELL
  jmp     WinDetectComplete


RunningUnderWinStdReal30:

  mov     ax,1605h                ; PMODE_START
  int     2Fh
  cmp     cx,-1
  jnz     Running30RealOr386

; Windows 3.0 Standard Mode detected

  mov     ax,1606h                ; PMODE_STOP
  int     2Fh                     ; in case someone is accounting.

  mov     wWindowsMajor,3         ; Set the version number
  mov     wWindowsMinor,0
                                  ; Set the Windows mode
  mov     wWindowsType, WIN_STANDARD_MODE
  jmp     WinDetectComplete

Running30RealOr386:

  mov     ax,1606h                ; PMODE_STOP
  int     2Fh                     ; in case someone is accounting.

  cmp     al,1                    ; WIN386CHECK again
  jnz     RunningUnderRealMode
  cmp     al,0FFh
  jz      RunningUnderWin386

RunningUnderRealMode:

  mov     wWindowsMajor,3         ; Set the version number
  mov     wWindowsMinor,0
                                  ; Set the Windows mode
  mov     wWindowsType, WIN_REAL_MODE
  jmp     WinDetectComplete


RunningUnderWin386:

  mov     wWindowsMajor,2
  mov     wWindowsMinor,0FFh
  mov     wWindowsType, WIN_386
  jmp     WinDetectComplete

NotRunningUnderWin:

  mov     wWindowsMajor,0         ; Windows is not running
  mov     wWindowsMinor,0
  mov     wWindowsType, NO_WINDOWS

WinDetectComplete:

;************************************************************************
;* The previous code was taken from MSD 2.01's OSINFO.C, WinVerDetect() *
;************************************************************************


; Return 1 if wWindowsType != NO_WINDOWS

  mov     ax,wWindowsType       ; AX == Windows type
  or      ax,ax                 ; Is it zero
  jz      ReturnToCaller        ; True: Return

  mov     ax,1                  ; If Windows is active, return 1

ReturnToCaller:

  mov     sp,bp
  pop     bp
  ret

_IsWindowsRunning endp

end
