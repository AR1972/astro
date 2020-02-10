;****************************************************************************
;*                                                                          *
;*  WFDOSDIR.ASM -                                                          *
;*                                                                          *
;*      Directory searching primitives                                      *
;*                                                                          *
;****************************************************************************

?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=1      ; Windows calling convention
?386=0      ; Use 386 code?

include cmacros.inc

; The following structure should be used to access high and low
; words of a DWORD.  This means that "word ptr foo[2]" -> "foo.hi".

LONG    struc
lo      dw      ?
hi      dw      ?
LONG    ends

FARPOINTER      struc
off     dw      ?
sel     dw      ?
FARPOINTER      ends

ifndef SEGNAME
    SEGNAME equ <TEXT>
endif

if ?386
    createSeg _%SEGNAME, CodeSeg, word, use16, CODE
else
    createSeg _%SEGNAME, CodeSeg, word, public, CODE
endif

NOT_SUPPORTED     =  2h      ; Return code from IsDeviceRemote function.
REMOTE            =  3h      ; Return code for remote drive found.
TRUE              =  1h      ; TRUE Definition
FALSE             =  0h      ; False Definition.

;=============================================================================

sBegin DATA

include ioctl.inc

VolExtendedFCB  db  0FFh
                db  0, 0, 0, 0, 0
                db  1000b
                db  0
                db  11 dup('?')
                db  5 dup(0)
                db  11 dup('?')
                db  9 dup(0)

DTA             db  64 dup(0)

sEnd   DATA

;=============================================================================

sBegin CodeSeg

assumes CS,CodeSeg
assumes DS,DATA

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  DosFindFirst() -                                                        *
;*                                                                          *
;*--------------------------------------------------------------------------*

; Get the first directory entry.

cProc DosFindFirst, <FAR, PUBLIC>

ParmD lpDest
ParmD szFileSpec
ParmW attrib

cBegin
            push    ds
            lds     dx,lpDest
            mov     ah,1Ah          ; Set DTA
            int     21h

            mov     cx,attrib       ; Find First File
            lds     dx,szFileSpec   ; Path = szFileSpec
            mov     ah,4Eh
            int     21h
            jc      fferr
            mov     ax,1
            jmp     short ffdone

fferr:
            xor     ax,ax           ; Return zero on error
ffdone:
            pop     ds
cEnd


;*--------------------------------------------------------------------------*
;*                                                                          *
;*  DosFindNext() -                                                         *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc DosFindNext, <FAR, PUBLIC>

ParmD lpDest

cBegin
            push    ds
            lds     dx,lpDest
            mov     ah,1Ah          ; Set DTA
            int     21h
            pop     ds

            les     bx,lpDest       ; ES:BX = lpDest
            mov     ah,4Fh          ; Find Next File
            int     21h
            mov     ax,1
            jnc     FNExit          ; Exit if no error
FNErr:
            xor     ax,ax           ; Return FALSE
FNExit:
cEnd

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  DosMkDir()                                                              *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc DosMkDir, <FAR, PUBLIC>
ParmD   szDir
cBegin
        lds     dx,szDir
        mov     ah,39h
        int     21h
        jc      mderror
        xor     ax,ax
        jmp     short mdexit

mderror:
        mov     ah,59h
        xor     bx,bx
        int     21h

mdexit:

cEnd

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  GetCurrentDrive() -                                                     *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc GetCurrentDrive, <FAR, PUBLIC>

cBegin
        mov     ah,19h              ; Get Current Drive
        int     21h
        sub     ah,ah               ; Zero out AH
cEnd

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  SetCurrentDrive() -                                                     *
;*                                                                          *
;*--------------------------------------------------------------------------*

; Returns the number of drives in AX.

cProc SetCurrentDrive, <FAR, PUBLIC>

ParmW Drive

cBegin
            mov     dx,Drive
            mov     ah,0Eh              ; Set Current Drive
            int     21h
            sub     ah,ah               ; Zero out AH
cEnd

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  GetCurrentDirectory() -                                                 *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc DosCwd, <FAR, PUBLIC>, <SI, DI>

ParmD lpDest

cBegin
            push    ds                  ; Preserve DS

            call    GetCurrentDrive
            mov     si,ax               ; SI = Current drive

            les     di,lpDest           ; ES:DI = lpDest
            push    es
            pop     ds                  ; DS:DI = lpDest
            cld
            mov     ax,si               ; AX = Current drive
            inc     al                  ; Convert to logical drive number
            mov     dl,al               ; DL = Logical Drive Number
            add     al,'@'              ; Convert to ASCII drive letter
            stosb
            mov     al,':'
            stosb
            mov     al,'\'              ; Start string with a backslash
            stosb
            mov     byte ptr es:[di],0  ; Null terminate in case of error
            mov     si,di               ; DS:SI = lpDest[1]
            mov     ah,47h              ; Get Current Directory
            int     21h
            jc      CDExit              ; Skip if error
            xor     ax,ax               ; Return FALSE if no error
CDExit:
            pop     ds                  ; Restore DS
cEnd

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  SetCurrentDirectory() -                                                 *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc DosChDir, <FAR, PUBLIC>

ParmD lpDirName

cBegin
            push    ds                  ; Preserve DS
            lds     dx,lpDirName        ; DS:DX = lpDirName

            mov     bx,dx
            mov     ax,ds:[bx]
            cmp     ah,':'
            jnz     cdnodrive

            ;
            ;       Convert drive letter to drive index
            ;
            or      al,20h
            sub     al,'a'
            xor     ah,ah

            push    dx
            cCall   SetCurrentDrive,<ax>
            pop     dx
cdnodrive:
            mov     ah,3Bh              ; Change Current Directory
            int     21h
            jc      SCDExit             ; Skip on error
            xor     ax,ax               ; Return FALSE if successful
SCDExit:
            pop     ds                  ; Restore DS
cEnd

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  DosValidDir()                                                           *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc DosValidDir, <FAR, PUBLIC>, <SI, DI>
ParmD       szDir
LocalV      szCwd, 128
cBegin
    lea     si,szCwd
    cCall   DosCwd,<ss,si>
    push    szDir.sel
    push    szDir.off
    call    DosChDir
    or      ax,ax
    pushf
    cCall   DosChDir,<ss,si>
    ;
    ;   return TRUE if DosChdir returns 0, FALSE otherwise
    ;
    xor     ax,ax                ; don't care about this return val.
    popf
    jnz     vdexit
    inc     ax
vdexit:

cEnd

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  DosExit(ec);                                                            *
;*                                                                          *
;*  Terminate program                                                       *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc DosExit, <FAR, PUBLIC>
ParmW   ec
cBegin
        mov     al,byte ptr ec
        mov     ah,4Ch
        int     21h
cEnd

;*******************************************************************
;
; None of the code below this comment is presently used in DOS2 GUI.
;
;*******************************************************************

if 0

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  DosRename(lpszFrom, lpszTo);                                            *
;*                                                                          *
;*  Rename file From (far pointer to old filename)                          *
;*                To (far pointer to new filename)                          *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc DosRename, <FAR, PUBLIC>, <DI,DS>
ParmD   lpszFrom
ParmD   lpszTo
cBegin
        lds     dx,lpszFrom
        les     di,lpszTo
        mov     ah,56h
        int     21h
        jc      rnexit
        xor     ax,ax
rnexit:
cEnd

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  DosDelete(szFile);                                                      *
;*                                                                          *
;*  Delete file                                                             *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc DosDelete, <FAR, PUBLIC>, <DS>
ParmD   szFile
cBegin
        lds     dx,szFile
        mov     ah,41h
        int     21h
        jc      dexit
        xor     ax,ax
dexit:
cEnd

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  int GetCodePage(void)                                                   *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc GetCodePage, <FAR, PUBLIC>

cBegin
        mov     ax,6601h          ; Get Code page call (Dos Ver >= 3.30)
        int     21h               ; Call DOS.
        mov     ax,bx             ; Stick axtive code page in AX.
        jnc     short GCPdone     ; default CP is already in DX so were done.
        mov     ax,-1             ; return error condition if carry set.
                                  ; other wise, return ax,dx pair.
GCPdone:                          

cEnd

;*--------------------------------------------------------------------------
;*
;*  LONG DosDiskFreeSpace(Drive)
;*
;*  note:
;*      Drive == 0      default
;*      Drive == 1      A
;*      Drive == 2      B
;*--------------------------------------------------------------------------

; Returns the number of bytes free in DX:AX

cProc DosDiskFreeSpace, <FAR, PUBLIC>
ParmW Drive
cBegin
            mov     dx,Drive
            mov     ah,36h              ; Set Current Drive
            int     21h
            cmp     ax, 0ffffh          ; bogus drive?
            je      error
            mul     cx                  ;
            mul     bx
            jmp     done                ; DX:AX contains free space
error:
            mov     dx, ax              ; return dx:ax = -1L
done:
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
;* int GetFixedDisks(int foo[26]);                                          *
;*                                                                          *
;* Returns the number of active LOCAL FIXED disk drives in the system AND   *
;* puts their indexes into rgiDrive[].                                      *
;*                                                                          *
;* ENTRY: Pointer to array of 26 integers.                                  *
;*                                                                          *
;* EXIT:  Returns the number of active LOCAL FIXED disk drives in the       *
;*        system puts their indexes into rgiDrive[].                        * 
;*                                                                          *
;* DESTROYS: AX. (preserves all registers except AX for return value)       *
;*                                                                          *
;* AUTHOR: MC                                                               *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc GetFixedDisks, <FAR, PUBLIC>, <BX,SI,DI>

ParmW   rgiDrive
localW  cRealDrives

cBegin
        ; Preserve the current drive setting.
        call    GetCurrentDrive
        mov     di,ax                ; Save original drive.

        xor     si,si                ; SI = iLogDrives = zero
        mov     cRealDrives,si       ; cRealDrives = zero

GDCLoop:
        ; Attempt to set the current drive to SI.
        
        push    si
        call    SetCurrentDrive

        ; Did it actually work?
        ;
        call    GetCurrentDrive
        cmp     ax,si
        jnz     GDC300                ; Nope, skip

        ;
        ; Is the drive remote ?
        ;

        push    si
        call    DosIsRemote
        cmp     ax,REMOTE             ; Is the drive remote ?
        je      GDC300

        ; Here we have found the drive is not Remote or we cannot tell if
        ; the drive is remote so lets find out if
        ; its removable before we add it to the cRealDrives list.
        
        mov     ax,4408h             ; IOCTL is removeable            
        mov     bx,si            
        inc     bx                   ; map from 0=A to 1=A            
        int     21h            
        jc      check_call           ; error, skip            
        or      al,al            
        jz      GDC300               ; removeable, skip            
        
        ; Store the drive number in rgiDrive[]
        mov     ax,rgiDrive
        mov     bx,cRealDrives
        inc     cRealDrives
        shl     bx,1
        add     bx,ax
        mov     word ptr [bx],si
        jmp     short GDC300

; We have a problem if IOCTL int 21h/4408h is not supported because then
; we have know way of knowing weather or not the machine in question really
; does have a fixed disk connected. Therefore, in this case, we'll give the
; user the benifit of the doubt and let them continue to install windows.
            
check_call:
        cmp     ax,1                 ; Is this call supported ?
        jnz     GDC300               ; If yes, continue on normally.
        mov     cRealDrives,1        ; If not, we better indicate we have
        jmp     short no_chance      ; a fixed disk and return.

GDC300:
        inc     si
        cmp     si,26                ; Loop through all 26 drives
        jne     GDCLoop

no_chance:
        ; Restore the original current drive.

        push    di                   ; Put pram on stack for call.
        call    SetCurrentDrive      ; Restore original drive.
        mov     ax,cRealDrives       ; Return num local drives in AX.
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
        push    bx                  ; Put param on stack for call.
        call    DosIsRemote         ; Find out if logical volume is remote
        or      ax,ax               ; check IsRemote return ?
        jnz     Remote_Or_Removable ; False ( 0 ) means local.

        ; Now we have found the volume is local, we can check to
        ; see if it's removable or not.
        
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

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  MyReadWriteSector() -                                                   *
;*                                                                          *
;*--------------------------------------------------------------------------*

; Uses INT 13h to read/write an absolute sector.

cProc MyReadWriteSector, <PUBLIC, FAR>, <SI,DI>

ParmD   lpBuffer
ParmW   Function                       ; 02 for Read and 03 for Write
ParmW   Drive
ParmW   Cylinder
ParmW   Head
ParmW   Count

LocalW  wRetryCount 

cBegin
        ; Retry this operation three times.
        mov     wRetryCount,4

MRWS_TryAgain:
        mov     ax,Count                ; AL = Number of sectors
        mov     ah,byte ptr Function    ; AH = Function #
        mov     ch,byte ptr Cylinder    ; CH = Starting Cylinder
        mov     cl,1                    ; CL = Starting Sector

        mov     dx,Drive        ; DL = INT 13h drive designation

        mov     dh,byte ptr Head        ; DH = Head #
        les     bx,lpBuffer             ; ES:BX = Buffer
        int     13h
        
        mov     ax, 1                   ; success

        jnc     MRWS_End_success        ; Problems?
        dec     wRetryCount             ; Yup, retry
        jz      MRWS_End_fail           ; Are we out of retries?

        xor     ah,ah                   ; Nope, reset the disk
        mov     dx,Drive
        int     13h
        jmp     short MRWS_TryAgain
MRWS_End_fail:
        xor     al,al           ; AH contains the error code, if any.
MRWS_End_success:

cEnd

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  GetCurrentVolume() -                                                    *
;*                                                                          *
;*--------------------------------------------------------------------------*

; Returns ZERO if successful.

cProc GetCurrentVolume, <FAR, PUBLIC>, <SI, DI>

ParmD lpszVol

cBegin
            push    ds

            ; Set the DTA
            mov     dx,offset DGROUP:DTA
            mov     ah,1Ah              ; Set DTA
            int     21h

            ; Get the current volume name.
            mov     dx,offset DGROUP:VolExtendedFCB
            mov     ah,11h              ; Search for First Entry
            int     21h
            test    al,al
            jnz     GVNoVol

            ; Copy volume name into buffer.
            les     di,lpszVol
            mov     al,'['
            stosb
            cld
            mov     si,offset DGROUP:DTA + 8
            mov     cl,11
GVLoop:
            lodsb
            cmp     al,' '
            jz      GVCont
            stosb
            loop    GVLoop
GVCont:
            mov     al,']'
            stosb

            ; NULL terminate.
            xor     ax,ax
            stosb
GVNoVol:
            pop     ds
cEnd

if 0
;*--------------------------------------------------------------------------*
;*                                                                          *
;*  GetFileAttributes() -                                                   *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc GetFileAttributes, <FAR, PUBLIC>

ParmD lpszPath

cBegin
            push    ds                  ; Preserve DS
            lds     dx,lpszPath         ; DS:DI = lpszPath
            mov     ax,4300h            ; Get File Attributes
            int     21h
            jc      GFAErr
            mov     ax,cx               ; AX = attribute
            jmps    GFAExit             ; Return attribute
GFAErr:     mov     ah,80h              ; Return negative error code
GFAExit:
            pop     ds                  ; Restore DS
cEnd
endif

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  DosMemory()                                                             *
;*                                                                          *
;*  RETURNS     ax = Max DOS memory dx = Max XMS                            *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc DosMemory, <FAR, PUBLIC>, <si>
cBegin
        mov     dx,-1       ; just incase it fails
        mov     ah,08h      ; Get XMS Memory Free
        call    XmsCall     ; call XMS driver, Total memory in DX
        int     12h         ; Total base memory in AX
cEnd


;*--------------------------------------------------------------------------*
;*                                                                          *
;*  ExtendedMemory()                                                        *
;*                                                                          *
;*  find out how much mem is free above 1M                                  *
;*                                                                          *
;*  RETURNS     ax = Extended memory free                                   *
;*                                                                          *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc ExtendedMemory, <FAR, PUBLIC>, <si>
cBegin
        mov     ah, 88h         ; now ask int 15 how much he knows about
        int     15h
cEnd



;*--------------------------------------------------------------------------*
;*                                                                          *
;*  DosVersion()                                                            *
;*                                                                          *
;*  RETURNS     ax = DOS version                                            *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc DosVersion, <FAR, PUBLIC, NODATA>, <si>
cBegin
        mov     ax,3000h
        int     21h
cEnd

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  Reboot()                                                                *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc Reboot, <FAR, PUBLIC, NODATA>, <>
cBegin
        ;
        ;   disable memory test
        ;
        mov     ax,40h
        mov     ds,ax
        mov     word ptr ds:[72h],1234h
        ;
        ;   jmp FFFF:0000
        ;
        mov     ax,0FFFFh
        push    ax
        xor     ax,ax
        push    ax
        retf
cEnd nogen

;
;   DosGetEnv
;
;   returns a FAR pointer to the current environment block
;
cProc DosGetEnv,<FAR,PUBLIC>,<si,di>
cBegin
        mov ah,62H
        int 21h                 ; get PSP seg in BX
        mov es,bx
        mov dx,es:[002CH]       ; get environment seg
        mov ax,0                ; DS:AX --> env block
cEnd

;*--------------------------------------------------------------------------*
;*                                                                          *
;*  XmsVersion()                                                            *
;*                                                                          *
;*  RETURNS     AX = (LOWORD) XMS spec version				    *
;*              DX = (HIWORD) driver internal version			    *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc XmsVersion, <FAR, PUBLIC>
        LocalD  xms
cBegin
        mov     ah,0
        call    XmsCall
        mov     dx,bx
cEnd


; BOOL XmsInstalled(void);

cProc XmsInstalled, <FAR, PUBLIC>
cBegin
        mov     ax,4300h
	int	2Fh
	cmp	al,80h
	jne	no_xms
	mov	ax,1
	jmp	xms_done
no_xms:
        xor	ax,ax
xms_done:
cEnd


;*--------------------------------------------------------------------------*
;*                                                                          *
;*  XmsCall()                                                               *
;*                                                                          *
;*--------------------------------------------------------------------------*

cProc XmsCall, <NEAR>
        LocalD  xms
cBegin
        mov     cx,ax
        mov     ax,4300h    ; Hello XMS?
        int     2fh
        cmp     al,80h
        mov	ax,0	    ; don't set the flags
        jnz     xc_exit
        mov     ax,4310h    ; Get XMS function pointer
        int     2fh
        mov     ax,cx       ; Get XMS function number back
        mov     xms.sel,es
        mov     xms.off,bx
        call    [xms]
xc_exit:
cEnd

; constants used by _get_ext

CMOS_PORT       equ     70H
CMOS_DATA       equ     71H
CMOS_REG_D2     equ     1AH
CMOS_EXT_STF    equ     1718H



;***************************************************************************
; Get extended memory amount.
;---------------------------------------------------------------------------

cProc  get_ext, <FAR, PUBLIC>
cBegin

       MOV        AX, CMOS_EXT_STF
       CALL       CMOS_READ
       XCHG       AL, AH
       CALL       CMOS_READ
  
cEnd   get_ext



;***************************************************************************
; very similar to IBM AT Technical Reference BIOS listing
; determine total extended memory by looking at the CMOS RAM.
;---------------------------------------------------------------------------

CMOS_POPF       PROC NEAR
       IRET
CMOS_POPF       ENDP

CMOS_READ       PROC NEAR
       PUSHF
       ROL      AL, 1
       STC
       RCR      AL, 1
       CLI
       OUT      CMOS_PORT, AL
       NOP

       IN       AL, CMOS_DATA
       PUSH     AX
       MOV      AL, CMOS_REG_D2
       RCR      AL, 1
       OUT      CMOS_PORT, AL
       POP      AX
       PUSH     CS
       CALL     CMOS_POPF
       RET

CMOS_READ       ENDP

;* BOOL PASCAL fnGetRamDriveVersion(char *szVerString);
;*
;* Function will look for and return the presence of ramdrive. If a ramdrive
;* is found it's version string will be returned in the char buffer provided
;* as a formal argument.
;*
;* ENTRY: szVerString - pointer to buffer of sufficent size to hold the
;*                      ramdrive version string. (8 chars).
;*
;* EXIT: Bool as to the presence of a ramdrive.
;*
;*
cProc fnGetRamDriveVersion, <FAR, PUBLIC>, <SI, DI, BX, DX>
   ParmD       szVerString
   LocalV      FixedDisks,   52d
   LocalV      ReadBuf,      512d
   LocalW      Cnt
cBegin

        lea   ax,FixedDisks
        cCall GetFixedDisks,<ax>
        or    ax,ax
        jz    No_Ramdrive
        mov   Cnt,ax                ; Number of disks to check.
        mov   ax,ss                 ; DS = SS for this code.
        mov   ds,ax
        lea   si,FixedDisks

Check_Next_Drv:
        mov   ax,[si]
        mov   cx,1
        lea   bx,ReadBuf
        xor   dx,dx
        push  si
        push  bp
        int   25h                   ; This call will trash SI and BP @!#
        inc   sp                    ; Fix up stack, this call leaves flags.
        inc   sp
        pop   bp
        pop   si
        jc    Next_Drive
        cmp   [bx+2],5290h
        jne   Next_Drive
        cmp   [bx+4],5644h
        je    RamDrive_Found

Next_Drive:
        dec   Cnt
        inc   si
        inc   si
        or    Cnt,0
        jnz   Check_Next_Drv

No_Ramdrive:
        xor   ax,ax
        lds   di,szVerString
        mov   byte ptr ds:[si],0
        jmp   short RD_Done

RamDrive_Found:
        lea   si,ReadBuf
        add   si,3                  ; Increment pointer past near jump.
        les   di,szVerString
        mov   cx,8
        cld
        rep   movsb
        mov   byte ptr es:[di],0    ; Null terminate string.
        mov   ax,1

RD_Done:

cEnd

;****************************************************************************
;
; unsigned fnGetSmartDrvVersion(void);
;
; This function will check to see if smartdrv is installed. If it is found
; that SD is installed we will return it's version number as an unsigned
; int.
;
; ENTRY: None.
;
; EXIT : Returns carry set if SD is not present. If carry clear, AX will
;        contain the Smartdrv version number.
;
; Thanks AAR.
;
;****************************************************************************

cProc fnGetSmartDrvVersion, <FAR, PUBLIC>, <SI, DI, BX, DX>
cBegin

        mov  dx,offset DGROUP:SMRTDRVName ; DS:DX pointer to smartdrv name
        mov  ax,3D02h                     ; Q:func is smartdrv installed ?
        int  21h                          ;   Y: continue.
        jc   short NoSmartShrink          ;   N: No memory steal possible.
        mov  bx,ax                        ; move SD handle to BX for next call

        mov  ax,4400h                     ; IOCTL get device information.
        int  21h                      
        jc   short NoSmartShrinkCls       ; Carry indicates call unsuccesful
        test dx,0080h                     ; Test if clock device.
        jz   short NoSmartShrinkCls       ; if not, we can't steal memory.
        test dx,4000h                     ; Are IOCTL's 02h and 03h supported
        jz   short NoSmartShrinkCls       ; if not we cannot steal memory.
        mov  dx,offset DGROUP:SMRTDRVInfo ; Now, make call to get SD info.
        mov  cx,SIZE SD_IOCTL_Read
        mov  ax,4402h                     ; Read control data from SD.
        int  21h
        jc   short NoSmartShrinkCls       ; Carry indicates call unsuccesful
        cmp  ax,cx                        ; If ax != cx we did not get the
        jne  short NoSmartShrinkCls       ; number of bytes we requested !
        mov  dx,offset DGROUP:SMRTDRVInfo ; DX may be trashed after last call.
        mov  si,dx                        ; si = index into SD_IR struc.
        mov  al,[si.SD_IR_Minor_Ver]      ; Save SD version in ax.
        mov  ah,[si.SD_IR_Major_Ver]
        push ax                           ; Save AX.
        mov  ax,3E00h                     ; Close device, handle in BX.
        int  21h                          ; Call DOS
        pop  ax                           ; restore AX.
        clc
        jmp  short ok_to_steal            ; Ax contains Amt avail to steal.

NoSmartShrinkCls:
        mov  ax,3E00h                     ; Close device, handle in BX.
        int  21h                          ; Call DOS

NoSmartShrink:
        xor  ax,ax                        ; return AX = 0.
        stc                               ; Set carry to indicate no success.

ok_to_steal:

cEnd

endif

sEnd CodeSeg

end
