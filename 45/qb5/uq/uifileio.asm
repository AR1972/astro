        page    ,132

        TITLE UIFILEIO - low level file io routines.
;***
;UIFILEIO - low level file io routines.
;
;       Copyright <C> 1988, Microsoft Corporation
;
;Purpose:
;
;
;*******************************************************************************

        .xlist
        include version.inc

        UIFILEIO_ASM = ON

        .list

        include cw/version.inc
        include cw/windows.inc
        include cw/edityp.inc

        includeonce     architec
        includeonce     rtps
        includeonce     uiint
        includeonce     heap

assumes DS,DATA
assumes ES,DATA
assumes SS,DATA

    ; Following declarations/definitions added while converting c - Asm.


        externFP BdAlloc
        externFP SetTabs
        externFP SetIsaColor
        externFP GetIsaColor
        externFP GetTabs
        externNP ColorResolution

        externFP FindAndOpenFile

        subttl  DATA segment definitions.

sBegin DATA

;NOTE: ORDERING OF bdLibPath, bdExePath, bdInclPath, bdHelpPath
;      IN UIPATHS.ASM MUST REMAIN CONSTANT.

        externW bdLibPath
        externW fOptionsChanged
        externW fScrollBars
        externB fSyntaxCheck
        externB fDebugScr

        externB fRightMouseHelp

        externB b$Buf1
        externW __aenvseg


; some constants for ReadQbIni and WriteQbIni

QBSIGNATURE     equ    4251h        ; Signature for 2.0, 3.0, 4.5

QB45VERSION     equ    0450h

QB40SIGNATURE   equ    4215h
QB40VERSION     equ    0390h
QB20VERSION     equ    0200h

QHELP11VERSION  equ    0460h                    ; [QH1]


AtrData struc

    bkClr   dw      ?
    fgClr   dw      ?
    fHLgt   dw      ?
    fBlnk   dw      ?

AtrData ends

    ;*************************************************************
    ; WARNING ! WARNING ! WARNING ! WARNING ! WARNING ! WARNING !

    ; B$ULLoad in RTMLOAD.ASM also reads qb.ini but doesn't have
    ; access to this structure definition. If this structure changes
    ; B$ULLoad must also be changed.

    ; WARNING ! WARNING ! WARNING ! WARNING ! WARNING ! WARNING !
    ;*************************************************************

;; [QH1]
;;  a-emoryh:
;;  Well, i looked, but i didn't find any code to change over in rtmload.asm.
;;  My guess is that user-library stuff was removed from Dos5 QBasic, to keep
;;  it lower-end than the stand-alone Basic products.  Hope that's true!
;;
OPTIONS struc

    signature   dw  ?
    version     dw  ?
    normT       db  SIZE ATRDATA DUP(?)
    curLn       db  SIZE ATRDATA DUP(?)
    bpLn        db  SIZE ATRDATA DUP(?)
    tabStopsOpt dw  ?
    fScroBars   dw  ?
    fSyntCheck  dw  ?

    fEZM        db  ?
    RightM      db  ?

    iPrint      dw  ?                           ; [QH1]
    fFile       dw  ?                           ; [QH1]
    szPrint     db  MAX_PATH DUP(?)             ; [QH1]

OPTIONS ends

staticW fh,0
globalB qbIniFName,<"qbasic.ini",0>
        cbqbIniFName = $ - qbIniFName

sEnd    DATA

        page

sBegin  UI
assumes CS,UI

        externNP CheckSwitchDiskettes

        subttl  Low level MSDOS File I/O calls.
        page

; NOTE: can't assert that b$fInt24Err is NZ in all these routines, since
;       a few are called from TXTLOAD/TXTSAVE, and int24 errors are supposed
;       to be hooked there.

; Exit:
;       If error, ax = UNDEFINED
;       otherwise, ax = return code
;
cProc   DoInt21,<NEAR>
cBegin
        int     21h
        jnc     IntOk           ;brif no error
        mov     ax,-1           ;Return error condition.
IntOk:
cEnd

;***
; OpenFile(szFile), CreateFile(szFile)
; Purpose:
;       Open specified file using supplied string.
;       Opens file for Read-Only access.
;       Use CreateFile to open for write-access.
; Entry:
;       pointer to Zero terminated string.
; Exit:
;       AX == file fhandle.
;       AX == -1 for error condition, carry set
;
;****
labelNP <PUBLIC,OpenFile>
        mov     ax,3d00h                ; open for read access
        SKIP2_PSW                       ; eat the next MOV

labelNP <PUBLIC,CreateFile>
        mov     ah,3ch                  ; create

cProc   OpenCreateFile,<NEAR>
        ParmW   szFile
cBegin
        mov     bx,[szFile]             ; bx = *file name
        cCall   <FAR ptr OpenCheckDrive>,<ds:[bx]> ; make sure we don't
                                        ; have to switch diskettes
                                        ; PRESERVES ALL REGISTERS
        mov     dx,bx                   ; dx = * to fn
        xor     cx,cx                   ; for CreateFile
        call    DoInt21
cEnd



;***
;OpenCheckDrive(drive)
;
;Purpose:
;       Called before opening a file or getting the current directory,
;       to check if the logical drive might have to be switched.
;
;       Re-written with revision [6].
;
;Entry:
;       drive = possible "drive_letter" + ":"
;
;Exit:
;       None
;
;Uses:
;       None.  Callers depend on ALL REGISTERS being preserved
;****

cProc   OpenCheckDrive,<FAR,PUBLIC>,<ax,bx,cx,dx,es>
parmW   drive
cBegin
        cmp     fDebugScr,0             ; capable of displaying a msg?
        jz      NoNewDrive              ; brif not -- let DOS do it

        mov     ax,[drive]              ; bx = possible "drive_letter" + ":"
        cmp     ah,':'                  ; drive specified?
        jne     NoNewDrive              ; brif not
        DbAssertRelB    al,ne,0,UI,<OpenCheckDrive: Null filename>
        cCall   CheckSwitchDiskettes,<ax> ; if we are switching logical disk
                                        ; drives, do it, and display a msgbox
NoNewDrive:
cEnd

        page

;***
;  CloseFileNear(fhandle)       [3]
;
;  Near routine to close file specified by "fhandle"
;
;  Inputs:      integer specifying fhandle to be closed.
;
;  Outputs:     AX == -1 for error conditio, carry set
;
;****

cProc CloseFileNear,<PUBLIC,NEAR>
        ParmW   fhandle
cBegin
        mov     bx, [fhandle]
        mov     ah, 3eh
        call    DoInt21
cEnd


        page

;***
;  WriteFile(fhandle, offBuf, cb)
;
;  Write cb bytes to file fhandle.
;
;  Inputs:
;       fhandle - file handle to write to.
;       offBuf  - address of buffer.
;       cb      - length of buffer.
;
;  Outputs:     AX == -1 for error conditions, carry set
;
;****

cProc WriteFile,<PUBLIC,NEAR>,<SI,DI,DS>
        ParmW   fhandle
        ParmW   offBuf
        ParmW   cb

cBegin
        mov     bx, [fhandle]
        mov     cx, [cb]
        mov     dx, [offBuf]
        mov     ah, 40H
        call    DoInt21
cEnd

        page

;***
;  ReadFile(fhandle, offBuf, cb)
;
;  Read file fhandle of cb bytes into the segment:offset buffer area.
;
;  Inputs:      unsigned integer file fhandle.
;               unsigned integer offset address.
;               unsigned integer count of bytes to read.
;
;  Outputs:     AX == -1 for error conditions, carry set
;
;****

cProc ReadFile,<PUBLIC,NEAR>
        ParmW   fhandle
        ParmW   offBuf
        ParmW   cb

cBegin
        mov     bx, [fhandle]
        mov     cx, [cb]
        mov     dx, [offBuf]
        mov     ah, 3fh
        call    DoInt21
cEnd

        page


;***
; FileExists(szFile)
; Purpose:
;       Determine whether or not a given filename exists.
; Entry:
;       szFile = pointer to Zero terminated string.
; Exit:
;       AX == TRUE if file found, false if not.
;
;****
cProc   FileExists,<PUBLIC,FAR>
        ParmW   szFile
cBegin
        push    [szFile]
        call    OpenFile                ;ax = file handle
        inc     ax
        je      NotOpened               ;return FALSE if file not opened
        dec     ax                      ;restore ax = file handle
        cCall   CloseFileNear,<ax>
        mov     ax,sp                   ;return TRUE
NotOpened:
cEnd

;***
; DelFile(szFile)
; Purpose:
;       Delete a file
; Entry:
;       szFile = pointer to Zero terminated string.
; Exit:
;       ax = -1 if error
;
;****
cProc   DelFile,<PUBLIC,FAR>
        ParmW   szFile
cBegin
        mov     dx, [szFile]
        mov     ah, 41h
        call    DoInt21
cEnd


;***
; ReadQbIni () - Read QB.INI file into the system
;
; Purpose:
;       Called at startup to read in the qb.ini file off the path and
;       set up defaults as appropriate.
;
; Entry:
;       None.
;
; Exit:
;       Sets up fOptionsChanged, fEZMenus, fRightMouseHelp, fSyntaxCheck
;       variables to default setup. Also sets colors using SetIsaColor.
;
; Uses:
;       Per Convention
;
;************************************************************************


cProc   ReadQbIni,<PUBLIC,NEAR>,<SI,DI>
cBegin

DbAssertRel b$fInt24Err,ne,0,UI,<ReadQbIni: Int 24 errors not ignored>

        mov     iPrintPort, DEV_LPT1    ; Set up print-dest defaults   ; [QH1]
        mov     fPrintToFile, 0                                        ; [QH1]
        mov     szPrintDest, 0                                         ; [QH1]


        mov     ax,SIZE OPTIONS         ; Allocate space on the frame
        sub     sp,ax                   ; for local structure.
        mov     fOptionsChanged,FALSE
        push    ax                      ; AX = SIZE OPTIONS

        mov     di,DATAOFFSET bdLibPath ; Allocate runtime heap entries
        mov     si,UIOFFSET DoAllocs    ; for all 4 Paths in environment
        Call    FourTimes               ; Lib, Exe, Incl, Help

        mov     di,DATAOFFSET bdLibPath ;[9] Set cbLogical to 1 for all 4
        mov     si,UIOFFSET DoTrims     ; path bd's (Lib, Exe, Incl, Help)
        Call    FourTimes

        cmp     uierr,FALSE             ; if heap allocation failed then exit
        jne     jmp_OpenFailed          ; from ReadQbIni. This is Out_of_Memory
                                        ; error. User interface will check
                                        ; for this and exit gracefully.

                                        ; initialise for unchanged Ini file
        xor     bx,bx                   ; BX = FALSE for Read Only Mode.
        call    OpenQBIni               ; Open the QB.INI file for reading
        jnz     OpenOK                  ; AX = File Handle for QB.INI
jmp_OpenFailed:
        jmp     short OpenFailed

OpenOK:
        DbHeapMoveOff           ; can't have heap movement below, since BD's
                                ; could be trimed back to their cbLogical's,
                                ; causing us to read QB.INI paths over the
                                ; top of memory we don't own.
        pop     bx                      ; BX = Size of Options, Bytes to be read
        mov     si,sp                   ; set up si to Frame for Ini Options
        push    bx                      ; Save Size of Options for later usage
        cCall   ReadFile,<ax,si,bx>
        cmp     ax,UNDEFINED            ; AX = -1 if error in reading.
        jz      ReadDone                ; brif error -- close file & return

        mov     ax,[si].signature
        cmp     ax,QB40SIGNATURE        ; if it is qb40 change its signature
        jnz     notqb40                 ; to qbsignature
        mov     ax,QBSIGNATURE
        dec     fOptionsChanged         ; fOptionsChanged set to TRUE
notqb40:
        cmp     ax,QBSIGNATURE          ; check for qb signature
        jnz     ReadDone                ; if not qb4 then look for qb2

        mov     ax,[si].version         ; AX=VERSION

; [QH1]
        cmp     ax, QHELP11VERSION      ; If QHELP version, start reading
        je      qb4version
        mov     fOptionsChanged, TRUE   ; Else upgrade it to QHELP11VERSION


        cmp     ax,QB40VERSION          ; check for qb 40 or 45 versions
        je      qb4version
        cmp     ax,QB45VERSION
        je     qb4version

        cmp     ax,QB20VERSION          ; Check for qb20 version. If
        jnz     ReadDone                ; yes then OptionsChanged True.
        jmp     SHORT ReadDone


qb4version:
        push    si                      ; Save SI = pointer to Option
                                        ; Frame on stack, because it is
                                        ; modified by Chk3Windows/SetSColor

        mov     di,UIOFFSET SetSColor   ; Set up default colors for
        call    Chk3Windows             ; Normal Text, Break Points,
                                        ; and Current Statement by calling
                                        ; SetSColor in Chk3Windows.
        call    ColorResolution         ; Resolve color dependencies.

        pop     si                      ; Restore SI=ptr to Option Frame
        mov     di,si                   ; di = *options(for later)
        lea     si,[si.tabStopsOpt]     ; SI = *tabstopsOpt
        lodsw                           ; Get default TabStops
        cCall   SetTabs,<ax>            ; Set Tabs
        lodsw                           ; Get default ScrollBars
        mov     fScrollBars,ax          ; Set ScrollBars
        lodsw                           ; Get default SyntaxCheck Status
        mov     fSyntaxCheck,al         ; Set SyntaxCheck Status

        cmp     [di].version,QB45VERSION ; DI = Options
        jl      ReadDone

        lodsb                           ; Get fEZM
        lodsb                           ; Get RightMouseHelp Status


;; [QH1] - Read print-dest info, if ini-file was QHELP version
        cmp     [di].version, QHELP11VERSION
        je      ReadPrintInfo
ReadPaths:

        mov     ax,UIOFFSET readfileabc ; Read four Paths
        call    rd_wrt_path


ReadDone:
        cCall   CloseFileNear,<fh>      ; Close the opened file
        DbHeapMoveOn                    ; heap movement OK now

OpenFailed:
        pop     ax                      ; AX = SIZE OPTIONS
        add     sp,ax                   ; Release allocated frame space
cEnd

;;
;; [QH1] Start
;;
;; Read print-dest info
;;      SI now points to Options.iPrint
;;      DI points to beginning of Options
;;
ReadPrintInfo:
        lodsw                           ; Load printer port
        cmp     ax, cDEVICES            ; Make sure field is valid
        jge     SkipDevField            ; Bogus value!!, so keep default
        mov     iPrintPort, ax

        lodsw                           ; Read PrinterOrFile flag
        mov     fPrintToFile, ax

SkipDevField:
        ; Load szPrintDest
        push    di
        mov     ax, ds                  ; Give ES the DATA seg (I hope!)
        mov     es, ax
        lea     di, szPrintDest

        mov     cx, MAX_PATH
        rep     movsb
        dec     di
        mov     BYTE PTR [di], 0        ; Make it asciiz, if not already
        pop     di

        ; Now SI should point to first field following Option.szPrint[]
        jmp     short ReadPaths
;;
;; [QH1] End
;;


;***
; rd_wrt_path() - performs read/write of all 4 path names
;
; Purpose:
;       This is written to save code. ReadQbIni reads and WriteQbIni
;       writes the information.
;
; Entry:
;       AX should have address of read/write-fileabc to be invoked by
;       by rd_wrt_cb2.
;
; Exit:
;       None.
;
; Uses:
;       Both SI,DI are destroyed.
;***********************************************************************

cProc   rd_wrt_path,<NEAR>
cBegin
        mov     di,DATAOFFSET bdLibPath ; Write BD structure for all the
        mov     si,UIOFFSET rd_wrt_cb2  ; four paths
        jmp     SHORT Fourtimes         ; Call rd_wrt_cb2 4 times and return
cEnd    <nogen>


;***
; rd_wrt_cb2() - reads/writes size of paths, then reads/writes the path into
;             heap entry for it.
; Purpose:
;       It has been written to save on code. This reading/writing
;       is done for all the four paths. It is used in conjunction
;       with procedure FourTimes.
;
; Entry:
;       DI should point to bd structure for current path.
;       fh has file handle for qb.ini
;       AX has offset of readfileabc/writefileabc - routine to be called
;       bdLibPath, bdExePath, bdInclPath, bdHelpPath must be contiguous
;       and in correct order.
;
; Exit:
;       DI is updated to next bd structure
;
; Uses:
;       DI is destroyed.
;
;***************************************************************************

cProc   rd_wrt_cb2,<NEAR>
cBegin
        lea     bx,[di].bd_cbLogical    ; reads a word having size of
        mov     cx,02                   ; bd structure into bd_cbLogical
        push    ax                      ; save address read/write-fileabc
        call    ax                      ; call read/write-fileabc
        pop     ax                      ; restore address read/write-fileabc
        mov     bx,[di].bd_pb           ; reads the bd structure from
        mov     cx,[di].bd_cbLogical    ; file.
        push    ax                      ; save address read/write-fileabc
        call    ax                      ; Call read/write-fileabc
        pop     ax                      ; restore address read/write-fileabc
        add     di,SIZE bd              ; Updates DI to point to next bd.
cEnd

;***
; readfileabc() - makes the call to read file.
;
; Purpose:
;       Written to save code by pushing 3 parms, and calling ReadFile.
;
; Entry:
;       fh has the file handle to read from.
;       cx has the number of bytes to read
;       bx has the address of buffer to read into.
;
; Exit:
;       None.
;
; Uses:
;       Per Convention
;
;**************************************************************************

cProc   readfileabc,<NEAR>
cBegin
        cCall   ReadFile,<fh,bx,cx>
cEnd

;***
; writefileabc() - Makes the WriteFile call.
;
; Purpose:
;       Written to save code by pushing 3 params, and calling WriteFile
;
; Entry:
;       fh has the file handle.
;       bx has the address of buffer
;       cx has the count of bytes to be written into file.
;
; Exit:
;       None.
;
; Uses:
;       Per Convention
;
;**************************************************************************

cProc   writefileabc,<NEAR>
cBegin
        cCall   WriteFile,<fh,bx,cx>
cEnd



;***
; FourTimes() - Makes call to the given procedure four times.
;
; Purpose:
;       Read and Write QBINI procedures make all file operations
;       for four paths Exe, Lib, Help and Inc.
;
; Entry:
;       SI has the OFFSET of procedure to be called four times.
;
; Exit:
;       None.
;
; Uses:
;       Per Convention
;
;*********************************************************************

cProc   FourTimes,<NEAR>
cBegin
        mov     cx,4            ; Sets up a count of 4
FT_next:
        push    cx              ; loop until zero to make four call on given
        call    si              ; procedure. Since the given procedure could
        pop     cx              ; destroy count (CX) save it.
        loop    FT_next
cEnd



;***
; DoAllocs - allocates runtime heap entry
;
; Purpose:
;       Allocate an interpreter specific heap entry from runtime
;       and set respective flags. If Out_of_Memory then SetUiErrOm.
;
;       DoAllocs works in conjunction with Fourtimes. It is done to
;       save on code for param initialization and passing. It assumes
;       ORDER of bdLibPath,bdExePath,bdInclPath,bdHelpPath and adds
;       size of Bd structure to get to next. Saves code!
;
; Entry:
;       DI has address of a bd structure of a env path
;
; Exit:
;       bd_pb of bd structure points to heap entry.
;       bd_cbPhysical of bd structure is set to cbSize.
;       DI is incremented to next bd structure.
;       AX is set TRUE is heap entry was allocated else FALSE.
;
;****************************************************************

cProc  DoAllocs,<NEAR>
cBegin
        mov     ax,MAX_SEARCH_PATH      ; Number of bytes wanted on heap
        mov     bx,IT_NO_OWNERS         ; Type of interp table
        cCall   BdAlloc,<di,ax,bx>      ; Allocate the heap entry
        or      ax,ax                   ; NZ if allocation successful
        jnz     DoAllocsOK              ; if heap alloc failed
        cCall   SetUiErrOm              ; out of memory error. (sets uierr)
                                        ; OK to fall into DoTrims, since it
                                        ; will be done anyway later.

labelNP <DoTrims>                       ; ROUTINE to trim size of BD's
        mov     [di].bd_cbLogical,1     ; set initial size = 1 so we have
                                        ; a null path string.

DoAllocsOK:
        add     di,SIZE bd              ; increment DI to point to next
                                        ; bd structur i.e. next path.
cEnd



;***
; OpenQBIni() - Opens the QB.INI file
;
; Purpose:
;       Opens the QB.INI file for reading/writing the system
;       default parameters.
; Entry:
;       BX = TRUE for WriteOnly mode
;            FALSE for ReadOnly mode
; Exit:
;       fh has the file handle
;       NZ  if open successful
;       ZF  if open unsuccessful
; Uses:
;       Per Convention
;
;*********************************************************************

cProc   OpenQBIni,<NEAR>,<DI,SI>
cBegin
        push    bx                      ; save file open mode
        mov     ah,30h                  ; get version number
        int     21h                     ; AX = dos version number
        pop     bx                      ; restore file open mode
        cmp     al,3                    ; are we DOS 3.x or above?
        jb      UsePATH                 ; brif not, no path present

        mov     es,__aenvseg            ; ES:0 is location of env.
        xor     di,di                   ; ES:DI is location of env
        mov     cx,8000h                ; scan to end of env table
        xor     ax,ax                   ; for a 0
LocateFname:
        repne   scasb                   ;
        scasb                           ; skip null, see if double 0
        jne     LocateFname             ; brif not double 0, keep looking
        scasw                           ; skip following 1

ASSUMES DS,NOTHING
        mov     si, di                  ; ES:SI = ptr to QBASIC path & name
        mov     di, DATAOFFSET b$buf1   ; DS:DI = ptr to INI path & name
        push    es                      ;
        pop     ds                      ; DS:SI = ptr to QBASIC path & name
        push    ss                      ;
        pop     es                      ; ES:DI = ptr to INI path & name
        mov     cx,di                   ; DS:CX = ptr to last seen path char
                                        ; in name as it is copied over

CopyName:
        lodsb                           ; grab a byte
        stosb                           ; and store it
        cmp     al,'\'                  ; is it a slash
        je      PathChar                ;  go remember it
        cmp     al,'/'                  ; or this slash
        jne     TestTerminator          ;  go remember it
PathChar:
        mov     cx,di                   ; set last path character found
TestTerminator:
        or      al,al                   ; 0 terminator?
        jnz     CopyName                ; brif not, more characters to do

HavePath:
        push    ss                      ;
        pop     ds                      ; restore DS = DGROUP = ES

ASSUMES DS,DGROUP

        cmp     cx, DATAOFFSET b$buf1   ; did we have any path?
        je      UsePATH                 ; brif not, use $PATH:

        mov     di, cx                  ; ES:DI = ptr to end of INI path
        mov     si, DATAOFFSET qbIniFName; DS:SI = ptr to new file name
        mov     cx, cbqbIniFName        ; CX = # characters
        rep     movsb                   ; and copy it over

        ; all set up, lets go open the INI file

        mov     ax,3d00h                ; Open existing file, read only
        or      bx,bx                   ; BX = 0 (FALSE = Read Only)
        jz      ReadOnly                ; brif assumption true
        mov     ah,3ch                  ; Create file, write access
ReadOnly:
        push    bx                      ; save open flag (open could fail)
        PUSHI   cx,<DATAOFFSET b$buf1>  ;
        call    OpenCreateFile          ; open file for mode AX
        pop     bx                      ; restore open flag
        mov     cl,1                    ;
        inc     cl                      ; Clear ZF, don't touch Carry
        jnc     FileOpened              ; brif success

UsePATH:
        mov     ax,DATAOFFSET qbIniFName ; AX = "QB.INI"
        mov     cx,EXEFILE              ; return value for success/failure
        cCall   FindAndOpenFile,<ax,bx,cx>
        or      ax,ax                   ; NZ ==> success

FileOpened:
        mov     fh,ax                   ; save the file handle value.

cEnd

;***
;Chk3Windows() - It makes GetSColor/SetSColor to 3 Isa's.
;
;Purpose:
;       To set up / save color defaults for various things on QB
;       Get / Set is performed during write/read. This helps save
;       code by setting up parameter and calling Get/Set as set
;       in DI.
;Entry:
;       SI points to the OPTIONS frame on stack
;       DI points to procedure for GetSColor/SetSColor
;Exit:
;       SI points to ATTRDATA of BreakPoint.
;Uses:
;       Per Convention
;
;*********************************************************************

cProc   Chk3Windows,<NEAR>          ; Added with revision
cBegin
        lea     si,[si].normT       ; SI to point to ATTRDATA for Normal Text
        mov     ax,isaEditWindow    ; Isa - Edit Window
        call    di                  ; Call GetSColor/SetSColor
        lea     si,[si].(curLn-normT); SI to point to ATTRDATA for Cur Stmt
        mov     ax,isaCurStmt       ; Isa - Cur Stmt
        call    di                  ; Call GetSColor/SetSColor
        lea     si,[si].(bpLn-curLn); SI to point to ATTRDATA for Brk Point
        mov     ax,isaBreakpoint    ; Isa - Break Point
        jmp     di                  ; Call GetSColor/SetSColor and return
cEnd    <nogen>

;***
; SetSColor() - Set System Color Defaults
;
; Purpose:
;       Default colors for Hight Light, Back Ground, Fore Ground etc
;       set for Edit Window, Break Point etc.
;
;       Based on SetSysColor Macro
;
; Entry:
;       ax has the Isa identifier like Edit Window, etc.
;       SI points to ATTRDATA of window in Options Frame on stack.
; Exit:
;       None.
; Uses:
;       Per Convention
;
;************************************************************************

cProc   SetSColor,<NEAR>
cBegin
        push    ax              ; SetIsaColor Parm #1

        mov     ax,[si].fHlgt   ; Set up the High Light + Foreground
        or      ax,ax           ; Parameter
        jz      NoBrForeground
        mov     ax,8
NoBrForeground:
        add     ax,[si].fgClr
        push    ax              ; SetIsaColor Parm #2

        mov     ax,[si].fBlnk   ; Set up the Blink + Background parameter
        or      ax,ax
        jz      NoBrBackground
        mov     ax,8
NoBrBackground:
        add     ax,[si].bkClr
        push    ax              ; SetIsaColor Parm #3

        cCall   SetIsaColor     ; Set up the colors.
cEnd

;***
; GetSColor() - Gets current system colors
; Purpose:
;       Current colors for EditWindow, BreakPoint, Current Statement
;       etc are obtained and later written into QB.INI file. This
;       ensures that next time the user would get same set up as at
;       the end of last session with QB.
;       Based on GETSYSCOLOR in file "uioptns.c".
; Entry:
;       ax has the Isa identifier like Edit Window, Break Point.
;       SI points to ATTRDATA structure correspoinding Isa in Options
;       frame on stack.
; Exit:
;       None.
; Uses:
;       Per Convention
;
;**********************************************************************

cProc   GetSColor,<NEAR>
cBegin
        push    ax              ; GetIsaColor parm #1

        lea     ax,[si].fgClr   ; Set up the address of Foreground Color
        push    ax              ; GetIsaColor parm #2

        lea     ax,[si].bkClr   ; Set up the address of Background Color
        push    ax              ; GetIsaColor parm #3

        cCall   GetIsaColor     ; Read the current colors into frame.

        xor     ax,ax           ; Decode Background and Blink based on
        test    [si].bkClr,8    ; formula in SetSColor and setup.
        jz      dontsetbit
        inc     ax
dontsetbit:
        mov     [si].fBlnk,ax   ; store blink bit (0, 1)

        xor     ax,ax
        test    [si].fgClr,8    ; Decode Foreground and High Light based
        jz      donotsetbit     ; on formula in SetSColor and setup.
        inc     ax
donotsetbit:
        mov     [si].fHlgt,ax   ; store highlight bit (0, 1)

        and     [si].bkClr,0fff7h   ; Strip off Blink bit
        and     [si].fgClr,0fff7h   ; Strip off Hightlight bit
cEnd


;***
; WriteQbIni() - Writes QB.INI File.
;
; Purpose:
;       Called at exit to write the qb.ini file if current default
;       options of QB are changed.
;
; Entry:
;       Uses a lot of system's common status variables.
;
; Exit:
;       None.
;
; Uses:
;       Per Convention (After explicitly saving SI,DI)
;
;**********************************************************************

cProc   WriteQBIni,<PUBLIC,NEAR>,<SI,DI>
cBegin

        cCall   HookInt24               ; ignore int 24 errors for the
                                        ; duration of this routine

        cmp     fOptionsChanged,FALSE   ; were options changed?
        je      OptNotChanged           ; brif not -- just exit

        mov     bx,TRUE                 ; Open QB.INI file for write only mode
        call    OpenQBIni
        jnz     FileFound               ; brif file found -- don't create one.


        mov     ax,DATAOFFSET qbIniFName; AX = "QB.INI"
        cCall   CreateFile,<AX>         ; create file, AX = file handle
        mov     fh,ax                   ; fh = file handle
FileFound:
        cmp     ax,UNDEFINED            ; If file creation successful proceed
        jz      NoHandle                ; else set fOptionsChanged to FALSE
                                        ;   and exit gracefully.

        mov     ax,SIZE OPTIONS
        sub     sp,ax                   ;Set up frame for OPTION structure
        mov     di,sp                   ; DI points to OPTIONS frame.
        push    ax                      ;save SIZE OPTIONS

        mov     [di].signature,QBSIGNATURE  ; Set up QBSIGNATURE and
                                            ; VERSION on frame.

        mov     [di].version, QHELP11VERSION                    ; [QH1]

        push    ds  ; Set ES = DS for following STOS instructions.
        pop     es

        push    di                      ; save *options
        lea     di,[di.tabStopsOpt]
        cCall   GetTabs                 ; AX = current tab setting
        stosw                           ; and set in Option Frame
        mov     ax,fScrollBars          ; AX = ScrollBars
        stosw                           ; and set in Option Frame
        mov     al,fSyntaxCheck         ; AX = SyntaxCheck
        cbw                             ; convert to word
        stosw                           ; and set in Option Frame

        mov     al,1                    ; store TRUE
        stosb
        mov     al,fRightMouseHelp      ; store RightMouseHelp
        stosb
        pop     si                      ; si = *options

        mov     di,UIOFFSET GetSColor   ; Get color settings.
        push    si
        call    Chk3Windows             ; for each isa, perform GetSColor
        pop     si

        call    SavePrintInfo                           ; [QH1]

        pop     ax      ;AX = SIZE OPTIONS
        push    ax      ;FH = File Handle
                                        ; Write OPTION structure with current
        cCall   WriteFile,<fh,si,ax>    ; settings into QB.INI file.


        mov     ax,UIOFFSET writefileabc ; for each bd, call writefileabc
        call    rd_wrt_path


        cCall   CloseFileNear,<fh>      ; Close the file.
        pop     ax                      ; AX = SIZE OPTIONS
        add     sp,ax                   ; Release frame from stack.

NoHandle:
        mov     fOptionsChanged,FALSE   ; Now current settings conform to
                                        ; the ones in QB.INI
OptNotChanged:

        cCall   UnHookInt24             ; have runtime handle int 24 errors

cEnd


;;
;; [QH1] Start
;;
;; Save print-dest info
;;      SI points to beginning of Options
;;
SavePrintInfo:
        cld
        mov     ax, iPrintPort          ; Save printer port
        mov     [si].iPrint, ax

        mov     ax, fPrintToFile        ; Save PrinterOrFile flag
        mov     [si].fFile, ax

        mov     cx, MAX_PATH            ; Save print-filename, if any
        lea     di, [si].szPrint
        mov     ax, ds
        mov     es, ax
        push    si
        lea     si, szPrintDest
        rep     movsb
        pop     si

        ret
;;
;; [QH1] End
;;


sEnd    UI
        end



