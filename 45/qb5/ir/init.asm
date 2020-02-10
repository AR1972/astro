        TITLE   INIT - interpreter-specific initialization code
;***
;init.asm - interpreter-specific initialization code
;
;       Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;       This module contains the initial entry point to QBI.
;
;*******************************************************************************

        .xlist
        include         version.inc
        INIT_ASM = ON
        includeOnce     architec
        includeOnce     context
        includeOnce     executor
        includeOnce     heap
        includeOnce     parser
        includeOnce     qbimsgs
        includeOnce     rtps
        includeOnce     rtinterp
        includeOnce     ui
        .list

assumes DS, DATA
assumes ES, NOTHING
assumes SS, DATA

IFNDEF IBMBASIC
;; [QH1] - Max length of topic argument to Dos6 QHelp
ENDIF
cbmaxTOPIC equ 40

sBegin  DATA
        externW __acmdseg
        externW __acmdln
        externB b$CtrlFlags        ;global runtime flag bits
        externB fRecord            ;global flag, set TRUE is /RCD switch seen
        globalW cmdSwitches,0      ;global bit flags for command-line switches
        globalB fInitialized,0     ;set non-zero when initializing completed
        globalW __acmdln_orig,0    ;original value of __acmdln
        globalB fQedit,0
IFNDEF IBMBASIC
; [QH1]
ENDIF
        globalB szCmdLineTopic,0,cbmaxTOPIC     ; Buffer to save topic arg in


sEnd    DATA

        EXTRN   B$IINIT:FAR
        EXTRN   B$LDFS:FAR
        EXTRN   B$IScram:FAR
        EXTRN   B$IInitPrint:FAR
        EXTRN   exEot:FAR
        EXTRN   exStRunFile:FAR
        EXTRN   InitUIStack:FAR

sBegin	RT
externNP B$LH_FROM_SS
externNP B$NHINIT
sEnd	RT

sBegin  RARE
assumes CS, RARE
        subttl  Process MSDOS COMMAND line.
        page

switch_codes equ $
NUM_SWITCHES = 8
        db      'BEGHMNQR',0

; Stored in reverse order to switch_codes.
; NOTE: If you add a switch, update the Invalid Switch message in qbimsgs.txt

switch_jmps  dw offset Proc5_R  ;/RUN load file and run it
             dw offset Proc5_Q  ;/QHELP MS-DOS Help Viewer
             dw offset Proc5_N  ;/NOHI no Highlighted colors
             dw offset Proc5_M  ;/M translate MKS to MKSMBF, CVS to ... etc.
             dw offset Proc5_H  ;/H Highest resolution possible
             dw offset Proc5_G  ;/G Glitch flag for SHELL.
             dw offset Proc5_E  ;/Editor
             dw offset Proc5_B  ;/B Color On/Off.

;***
;_main - Entry point to the Quick Basic Interpreter
;Purpose:
;       Entry point from startup code to QBI.
;       Parses the command line, initializes the runtime, initializes the
;       global context, sets up the stack, and starts the interpreter.
;Input:
;       __acmdseg is a pointer to a pointer to the segment address of cmd line
;       __acmdln is the offset to the cmd line string (can be accessed as
;               a far ptr to command line).
;Output:
;       none; never returns. Program terminates via the runtime.
;*****************************************************************************
cProc   _main,<PUBLIC,FAR,NODATA>

;***************************** NOTE - NOTE - NOTE *****************************
CB_MAIN_LOCALS EQU 12d+2        ;We're resetting SP & BP at the end of _main,
                                ;based on the above number of Local's - - - if
                                ;you add or remove any local's, change this
                                ;constant (note that the constant is too large
                                ;by 2 bytes to account for the PUSH BP that
                                ;occurs at the start of _main).
;***************************** NOTE - NOTE - NOTE *****************************
        localW  cbComBuffer        ;For saving /C:nnn value.
        localW  cbLibName       ;for saving length of library name
        localW  cmdSeg          ;seg part of pointer for command line
        localW  pLibName        ;for saving ptr to library name

        ;NOTE: The order of these first local's is part of the interface
        ;NOTE: to B$IINIT - - we pass a pointer to pLibName, and B$IINIT
        ;NOTE: accesses these directly

        localW  cbFileName      ;for saving length of input program
        localW  pFileName       ;for saving ptr to program to load & run

cBegin

        xor     ax,ax
        mov     es,ax
        mov     al,es:[0417H]
        and     al,0fH
        cmp     al,0fH
        jne     @F
        call    InitUIStack
@@:
        ;
        ; Process the Command Line
        ; Perform the specialized processing of the command line for the QBI

        mov     [pLibName],0    ;default - no /L
        mov     [cbComBuffer],CbComBuf ;default Com Buffer size

        mov     si,[__acmdln]
        mov     [__acmdln_orig],si ;save in case user wants to change his
                                     ; command$ string later - - -
        mov     ds,[__acmdseg]  ;DS:SI == MSDOS Command line buffer.
        mov     [cmdSeg],ds     ;save in case we're passing LibName to runtime
assumes DS, NOTHING

        xor     dx,dx           ;initially, all flags false.
Proc2:
        lodsb
Proc2a:
        cmp     al,9
        je      Proc2           ;brif horizontal tab found

        cmp     al,' '
        je      Proc2
        jb      Proc2c          ;All done parsing command line

        cmp     al,'/'          ;Look for switch character.
        je      Proc3           ;Working on a switch.

Proc2b:
        ;user specified a filename to load [& run if CMD_SW_FLE is set]
        test    dx,CMD_SW_FLE   ;already seen a filename?
        jnz     SYS_Error1      ;brif so - - - error

        or      dx,CMD_SW_FLE   ;remember that we're to load this file
        dec     si
        mov     [pFileName],si  ;save pointer to start of filename
        lea     bx,[cbFileName] ;store size of filename here
Proc2d:
        ;parse and count bytes in a filename. Note that this is used
        ;  for the /L (user library) name as well
        inc     si              ;move back to point to 2nd byte of filename
        xor     di,di           ;initialize name length to zero
Proc2d1:
        inc     di              ;last char was part of filename - count it
        lodsb

        cmp     al,' '
        jb      Proc2d2         ;brif end of cmdline
IFNDEF IBMBASIC
;; [QH1]
ENDIF
        test    dx,CMD_SW_QHELP ; Are we running as QHELP?
        jnz     Proc2d1         ; If so, we allow multi-word topics, so ignore
                                ; whitespace, and any "/" switches after topic.
                                ; WARNING : /QHELP switch MUST be first!
    ; If not QHelp mode, enable normal param error checking
        cmp     al,'/'
        je      Proc2d2         ;brif switch found - end of filename

        cmp     al,9
        je      Proc2d2         ;brif tab found - end of filename

        cmp     al,' '
        ja      Proc2d1         ;brif not end of filename
Proc2d2:
        mov     ss:[bx],di      ;save size of filename
        jmp     short Proc2a    ;All done counting filename - proceed w/parsing

Proc2c:
        jmp     Proc7           ;Control Characters terminate differently.

;
; Process Switch character "/"
;

Proc3:
        lodsb                   ;Get switch type.
        and     al,5Fh          ;Brute force to upper case.
        mov     cx,NUM_SWITCHES ;Only check for active switches.
        mov     di,offset switch_codes
        push    cs
        pop     es              ;es:di set for scasb
        repnz   scasb
        jnz     SYS_Error1      ;Error condition - invalid switch

        shl     cx,1
        mov     bx,cx
        jmp     [switch_jmps+bx]

;
; The actual switch codes are processed here.
;

SYS_Error1:
SYS_Error:
        mov     ax,ER_ISWEDIT   ;begining editor message
        mov     di,ER_ISWEDITEND;ending editor message
        test    dx,CMD_SW_EDCOM ;were we started from EDIT.COM?
        jnz     PrintMsgs       ;brif so, we have proper message set
        mov     ax,ER_ISWQHELP  ;begining qhelp message
        mov     di,ER_ISWQHELPEND;ending qhelp message
        test    dx,CMD_SW_QHELP ;were we started from EDIT.COM?
        jnz     PrintMsgs       ;brif so, we have proper message set
        test    dx,CMD_SW_EDCOM ;were we started from EDIT.COM?
        mov     ax,ER_ISWINTERP ;Use message set for interpreter
        mov     di,ER_ISWINTERPEND
PrintMsgs:
        push    ax              ;save message number
        call    B$IInitPrint    ;print message AX to stdout
        mov     ah,2            ;print character to STDOUT
        mov     dl,13           ;Carriage Return
        int     21h
        mov     dl,10           ;Line Feed
        int     21h
        pop     ax              ;restore ax = message number
        inc     ax              ;move to next message
        cmp     ax,di           ;have we gone beyond last message?
        jbe     PrintMsgs       ;brif not, display another line
        call    B$IScram        ;scram, i.e., get out of town, never returns.
                                ; just sets appropriate error code and exits.
        DbHalt  RARE,<init.asm: B$IScram returned>
Proc5_B:
        or      dx,CMD_SW_MNO   ;remember that /B switch was seen
        jmp     short Proc2_Relay1

Proc2_Relay1:
        lodsb
        cmp     al,' '
        jbe     NextChar_OK

        cmp     al,'/'
        jnz     SYS_Error       ;only whitespace, another switch, or end of
                                ;  command line is valid as the immediately
                                ;  following char after a valid switch
NextChar_OK:
        jmp     Proc2a

Proc5_M:
        lodsw                   ;Get 2 chars
        and     ax,5F5Fh        ;Brute force both chars to upper case.
        cmp     ax,04642H       ;'BF' of '/MBF' switch
        jnz     SYS_Error

        or      dx,CMD_SW_MBF   ;remember that /MBF switch was seen
        jmp     short Proc2_Relay1


Proc5_H:
        or      dx,CMD_SW_HIR   ;remember that /H switch was seen
        jmp     short Proc2_Relay1

Proc5_G:
        or      dx,CMD_SW_GLT   ;remember that /G switch was seen
        jmp     short Proc2_Relay1

Proc5_N:
        lodsw                   ;Get 2 chars
        and     ax,5F5Fh        ;Brute force both chars to upper case.
        cmp     ax,0484FH       ;'OH' of '/NOHI' switch
        jnz     SYS_Error
        lodsb                   ;get third char
        and     al,5Fh          ;force to upper case
        cmp     al,49H          ;'I' of '/NOHI' switch
        jnz     SYS_Error

        or      dx,CMD_SW_NOH   ;remember that /NOHI switch was seen
        jmp     short Proc2_Relay1

Proc5_R:
        lodsw                   ;Get 2 chars
        and     ax,5F5Fh        ;Brute force both chars to upper case.
        cmp     ax,04443H       ;'CD' of '/RCD' switch
        jnz     Proc5_R_Try_RUN ;  brif not '/RCD' switch

        mov     [fRecord],TRUE  ;remember that /RCD switch was seen
Proc2_Relay2:
        jmp     short Proc2_Relay1

Proc5_R_Try_RUN:
        cmp     ax,04E55H       ;'UN' of '/RUN' switch
        jnz     J_SYS_Error
        or      dx,CMD_SW_RUN   ;remember that we're to execute the file
                                ;  that's found
        ;error if filename not found now
Proc5_R1:
        lodsb                   ;Get ascii character.
        cmp     al,9
        je      Proc5_R1        ;brif horizontal tab found

        cmp     al,' '
        je      Proc5_R1        ;brif space found
        jb      J_SYS_Error     ;brif end of command line

        cmp     al,'/'          ;Look for switch character.
        jne     Proc5_R_OK
J_SYS_Error:
        jmp     SYS_Error       ;  brif found - - filename not found after /R

Proc5_R_OK:
        jmp     Proc2b          ;process filename and continue

Proc5_E:
        or      dx,CMD_SW_ED    ;remember that /Editor switch was seen
        lodsb                   ; get next character (must be D)
        mov     bx,offset EditorSwitch  ; ptr to string to match
        mov     cx,si           ; save ptr to source string

Proc5_E1:
        and     al,5fH          ; convert to upper
        cmp     al, cs:[bx]     ; is it the next legal character
        jnz     Proc5_E3        ; no, not /EDITOR
        inc     bx              ; bump pointer
        lodsb                   ; grab next character of switch
        cmp     al,'/'          ; until we see the next switch
        je      Proc5_E2        ;  or
        cmp     al,' '          ; until we see a space or end of line
        ja      Proc5_E1

Proc5_E2:
        dec     si              ; back up one character
Proc2_Relay3:
        jmp     Proc2_Relay1

EditorSwitch:
        db      'DITOR',0       ; valid chars of switch, 0 to catch
                                ; a switch that is too long

Proc5_E3:
        ;/EDITOR not found, look for /EDCOM (all uppercase)
        mov     si,cx           ;restore pointer to source (after D)
        dec     si              ;backup in case second letter was not D
        lodsw                   ;grab two character (DC)
        cmp     ax,'CD'         ;is it correct?
        jne     J_SYS_Error     ;brif not
        lodsw                   ;grab last two chars (OM)
        cmp     ax,'MO'         ;is it correct?
        jne     J_SYS_Error     ;brif not
        or      dx,CMD_SW_EDCOM ;indicate switch seen
        jmp     SHORT Proc2_Relay3 ;go look for more switches


Proc5_Q:
        ; Look for /QHELP (all uppercase)
        ; If we find it, we turn on CMD_SW_ED, too, so that we can
        ; avoid QBASIC code paths by default!
        lodsw                   ;grab two character (HE)
        cmp     ax,'EH'         ;is it correct?
        jne     J_SYS_Error     ;brif not
        lodsw                   ;grab last two chars (LP)
        cmp     ax,'PL'         ;is it correct?
        jne     J_SYS_Error     ;brif not
        or      dx,CMD_SW_QHELP or CMD_SW_ED ;indicate switch seen
        jmp     SHORT Proc2_Relay3 ;go look for more switches

;
; Finished parsing line.
;


Proc7:
        dec     si              ;point to terminating character
Proc7a:
        push    ss
        pop     ds              ;restore ds=ss
assumes DS, DATA
        mov     [__acmdln],si   ;change __acmdln to point past filename
                                ;  to end of buffer, if no filename given

        test    dx,CMD_SW_QHELP ; Are we running as QHELP?
        jz      Proc7b          ;   NO, don't modify flags
        jmp     DoQHelpStuff

DoQHelpDone:
Proc7b:
        mov     [cmdSwitches],dx ;save for global access

        test    dx,CMD_SW_ED    ; are we in editor mode?
        jz      NotEditor       ; brif not
        test    dx,CMD_SW_RUN or CMD_SW_MBF ; conflicting switches?
        jnz     J_SYS_Error     ; brif so, error out
NotEditor:

        push    [cmdSeg]        ;push these 3 bp-relative parms to B$LDFS now,
        push    [pFileName]     ;  because B$IINIT trashes bp
        push    [cbFileName]

        lea     si,[pLibName]   ;pass pointer to parm block to B$IINIT
        or      [b$CtrlFlags],NoSTACKINIT
                                ;speed optimization - - -
                                ;  Don't bother to init. stack, since
                                ;  we will init. the stack when we actually
                                ;  start running a program.
IFDEF ROMBASIC

IFNDEF IBMBASIC

;	The IBM specific version of QBASIC will only run on machines with
;	IBM's BASIC ROM.  The ROM is at F600:0000 in old machines, but with
;	the new machines we need to do a BIOS call to find out where it is.
;	(If the BIOS indicates that call is not supported then we must by
;	on an old machine with the ROM at F600:0000.)
;	The near heap code from B$LH_FROM_SS to B$NHINIT has been
;	XOR'ed with that ROM image.  XOR it again here to restore the
;	near heap code to executable.  If we're not running on an IBM
;	machine we'll end up with garbage code and crash during the
;	first call to the near heap.
;	IBM's new spec for the ROM reduction uses a BIOS call to locate
;	ROM BASIC instead of assuming F600.  It is as follows:
;
;	Interrupt 15H
;		AH = 22H
;	EXIT:
;		ROM resident Basic present
;
;UNDONE: This should be just ES, not ES:BX, because the offset must be zero.
;		(ES:BX) = Pointer to Basic
;		(CY)	= 0
;		(AH)	= 0
;
;		ROM resident portion Basic not present (should never happen)
;
;		(ES:BX) = Undefined
;		(CY)	= 1
;		(AH)	= 1
;
;	Old BIOS versions return:
;
;		(CY) = 1
;		(AH) = 86H
ENDIF

assumes DS,nothing

labelNP <PUBLIC,IBMRom>

	push	ds
	push	si

	mov	ah,22h
	int	15h

	mov	ax,es		; assume new ROM with BASIC
	jnc	j111		; brif new ROM with BASIC
	mov	ax,0F600h	; else assume it's the old ROM
j111:	mov	ds,ax
	xor	si,si		; DS:SI points to ROM image
	mov	bx,RToffset B$LH_FROM_SS
	mov	ax,seg B$LH_FROM_SS
	mov	es,ax		; ES:BX points to code to manipulate
j112:	lodsw
	xor	es:[bx],ax	; do it.
	inc	bx
	inc	bx
	cmp	bx,RToffset B$NHINIT ; done?
	jb	j112		; brif not, keep looping
	pop	si
	pop	ds

assumes DS,data
ENDIF


        call    B$IINIT
        and     [b$CtrlFlags],NOT NoSTACKINIT
                                ;reset to default

        cCall   InitContext
        jnz     OM_Err_In_Init  ; brif error return

        cCall   ParseInit
        cCall   ParseNewInit
        or      ax,ax
        jnz     Main_Done       ;brif no error return
OM_Err_In_Init:
        call    RtErrorOM_INI   ;Out of Memory during init - abort
Main_Done:
        cCall   UiInit          ;read file qb.ini
        xor     cx,cx           ;assume no filename
        mov     dx,4+CB_MAIN_LOCALS+6 ;assume no filename
        test    [cmdSwitches],CMD_SW_FLE
        jz      Exit_main       ;brif no filename given

        ;parms to B$LDFS are already on the stack
        call    B$LDFS          ;ax = psdTemp for user-specified filename
        mov     cl,1            ;remember there was a filename given
        mov     dx,4+CB_MAIN_LOCALS ;B$LDFS took its 3 parms off stack already
Exit_main:
        add     sp,dx
                                ;set sp & bp up as high as we can - - - i.e.,
                                ;eliminate local's and return address from _main
        mov     bp,sp
        mov     [b$mainframe],bp ;frame to reset sp & bp to on RUN/CHAIN/NEW
        mov     [b$curframe],bp ;LoadFile wants this setup in case of error
        mov     [bp],word ptr 0 ;runtime assumes bp chain to be 0-terminated
        jcxz    No_File_Name

        push    ax              ;pass psdFileName
        jmp     exStRunFile     ;load [& start] specified program running

No_File_Name:
        inc     [fInitialized]  ;remember that we're done initializing now
        jmp     exEot           ;start execution - never returns


DoQHelpStuff:
IFNDEF IBMBASIC
;;
;; [QH1], Dos6 QHelp - If topic given on commandline, save it elsewhere and
;;              clear CMD_SW_FLE flag so qbasic doesn't try to load it as a
;;              file!  We'll process it later, in uihelp.c.
;;      (Had to move it down here as it was screwing up everyone's short jumps)
;;
ENDIF
        mov    szCmdLineTopic, 0        ;clear topic buffer

        test    dx,CMD_SW_FLE           ;do we have a topic argument?
        jnz     GetTopic                ; brif so
        jmp     DoQHelpDone

GetTopic:
IFNDEF IBMBASIC
;;
;; Copy topic name to szCmdLineTopic
;;
ENDIF
        and     dx, NOT CMD_SW_FLE      ; Don't want QBASIC to try to run it!
        push    ds
        push    es
        cld
        mov     di, DATAOFFSET szCmdLineTopic

        mov     ax, ds                  ; Load ES with DATA seg
        mov     es, ax
        mov     ds, cmdSeg              ; Load commandline seg
assumes DS, NOTHING
assumes ES, DATA
        mov     si, pFileName
        mov     bx, si
        add     bx, cbFileName

IFNDEF IBMBASIC
    ; BUGBUG - If user stuck tabs in the middle of the topic, the search will
    ;     fail later.  That's too bad.  If someone complains, i'll add in the
    ;     code to strip them out.
ENDIF
StripWhite:

IFNDEF IBMBASIC
    ; Strip off any terminating white-space
ENDIF
        dec     bx                      ; Start with last char, not terminator
        mov     al, BYTE PTR ds:[bx]
        cmp     al, ' '
        je      StripWhite
        cmp     al, 9
        je      StripWhite
IFNDEF IBMBASIC
    ; Now get real length
ENDIF
        inc     bx
        sub     bx, si                  ; length = end - start
        mov     cx, bx

        cmp     cx, cbmaxTOPIC          ; Make sure it's not too long
        jl      TopicNotTooLong
        mov     cx, cbmaxTOPIC-1        ; If so, truncate it
TopicNotTooLong:
        rep     movsb
        mov     BYTE PTR es:[di], 0     ; Make it asciiz

        pop     es                      ; Restore everything.  (Not needed?)
        pop     ds
assumes DS, DATA
assumes ES, NOTHING
        jmp     DoQHelpDone


cEnd    <nogen>

sEnd    RARE

        end

