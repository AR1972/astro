
.MODEL medium
;***************************************************************************
;*                                                                         *
;* MD.ASM Mouse detection code.                                            *
;*                                                                         *
;***************************************************************************

memM = 1       ; Medium Model
?WIN = 0       ; No windows prolog / epilog code (std epilog / prolog).
?PLM = 0       ; CPROC calling convention.  NOT Pascal

INCLUDE cmacros.inc     ;* must be version 2.09 or higher
INCLUDE md.inc
INCLUDE id.inc


cPublic MACRO   n,c,a
  cProc n,<PUBLIC, c>,<a>
ENDM


sBegin  DATA

LATLSBSave           db  ?
LATMSBSave           db  ?
LCRSave              db  ?
MCRSave              db  ?
IERSave              db  ?
fSingle8259          db  0
SerialBuf            db  10 dup (?)
SemaphoreWord        dw  ?

COM_3_4_ADDRS        LABEL     WORD
                 dw  03E8h
                 dw  02E8h

sEnd   DATA

sBegin  CODE

assumes CS, CODE
assumes DS, DATA

;*****************************************************************************
;
;       int fnHardMouseDetect(void);
;
;       This procedure what type of mouse is being used. It will determine
;       that that type of mouse is present in the system and is working
;       properly.
;
;       ENTRY   None.
;
;       EXIT    Mouse type in AX. or if AX = -1 the hardware will not support
;                                               mouse detection.
;
;       ALTERS  AX, BX, CX, DX, SI, DI
;
;       CALLS   TestForInport
;               IRQAvailable
;               TestForBus
;               TestForSerial
;               TestForPS2
;
;*****************************************************************************

cProc   fnHardMouseDetect <PUBLIC, FAR>, <ES, DS, SI, DI>
        localW  locvar1
cBegin  fnHardMouseDetect

; First of all we need to know if the two int 15h call we need to make are
; supported on the hardware were running on. So, the setup_for_detect call
; will do just that, setup for the detection. If the call returns carry set
; then it's "game over man" because we won't have the int 15h calls we need.

        call    setup_for_detect
        mov     locvar1,-1
        jc      finished_mouse

; Now that we know the hardware will support the code used here to detect
; mice we can go about our business of detecing mice !!
;

        mov     locvar1,NO_MOUSE         ; Initilize the return value.

;
; First try, we'll look for an inport mouse !
;

        mov     dx,INPORT_FIRST_PORT + 2 ; Get address of ID register.

inport_try_again:
        call    TestForInport           ; Does an InPort exist at this address?
        jnc     inport_found            ; No carry ! Inport found !

        sub     dx,4                    ; Nope, try the next possible port.
        cmp     dx,INPORT_LAST_PORT + 2
        jae     inport_try_again
        jmp     short look_for_bus

inport_found:
        mov     locvar1,INPORT_MOUSE
        jmp     finished_mouse

;
; We enter this point if we couldn't find an InPort mouse in the system.
;

look_for_bus:
        call    TestForBus            ; Does a Bus mouse exist in the system?
        jc      look_for_PS2          ; No, look for a PS/2 mouse.

        mov     locvar1,BUS_MOUSE     ; Indicate bus mouse found.
        jmp     finished_mouse
        
;
; We enter this point if we couldn't find either an InPort
; mouse or a Bus mouse in the system.
;

look_for_PS2:
        call    LookForPS2Mouse

        cmp     ax,NO_MOUSE
        je      look_for_serial

        mov     locvar1,AX              ; Yep! return mouse type !
        jmp     short finished_mouse

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;       call    TestForPS2              ; Does a PS/2 style mouse exist?
;       jc      look_for_serial         ; Nope, check for serial mouse.
;
;       mov     locvar1,LT_PS2MOUSE     ; Yep! return mouse type !
;       jc      finished_mouse          ; Nope, leave with error.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; We enter this point if we couldn't find either an InPort
; mouse, a Bus mouse, or a PS/2 mouse in the system.
;

look_for_serial:
        call    LookForSerialMouse
        mov     locvar1, ax

finished_mouse:
        mov     ax,locvar1

cEnd    fnHardMouseDetect

;*****************************************************************************
;
; setup_for_detect
;
; Procedure will assure that were ready to start the detection and that the
; hardware were running on will allow int 15h/86h (delay) and int 15h/C0h
; (Get system environment).
;
; ENTRY: None.
;
; EXIT:  NC - All is well to begin detection.
;        CY - Nope, int 15h calls needed are not available.
;
; ALTERS: AX ES BX CX DX And sets the fSingle8259 gloval var to true or false
;         depending on weather there are multiple 8259 chips in the system.
;

setup_for_detect   PROC   NEAR

        mov    ah,0c0h               ; int 15h - func get sys environment.
        int    15h
        jc     old_machine
        cmp    word ptr es:[bx],5    ; Did we return 5 bytes ?
        jb     old_machine           ; If not, no go.

        mov    ah,byte ptr es:[bx+5] ; Get the configuration flags.
        test   ah,01000000b          ; Is bit 6 set ? (slave 8259 present ?)
        mov    fSingle8259,TRUE
        jz     All_is_well
        mov    fSingle8259,FALSE
        jmp    short All_is_well

; I know that since the int 15h/0C0h call failed, we have an old machine upon
; which I can assume will not have a slave 8259. If call int 15h/8300h will
; work we can still detect the mouse.

old_machine:
        mov    fSingle8259,TRUE
        clc

All_is_well:
        ret

setup_for_detect   ENDP

;*****************************************************************************
;
;       TestForInport
;
;       This procedure will attempt to find an InPort mouse at the given base
;               I/O address. Note that if an InPort is found, it will be left
;               in a state where it will not be generating any interrupts.
;
;       ENTRY   DX              I/O address of InPort Identification Register
;
;       EXIT    NC              An Inport was found 
;               CY              No Inport was found
;
;       ALTERS  AX, BX, CX, DX, SI
;
;       CALLS   FindInportInterrupt
;

TestForInport   PROC    NEAR

;
; Since the identification register alternates between returning back
;       the Inport chip signature and the version/revision, if we have an
;       InPort chip, the chip signature will be read in one of the following
;       two reads. If it isn't, we do not have an InPort chip.
;

        mov     bl,INPORT_ID
        in      al,dx                   ; Read ID register.
        cmp     al,bl                   ; Is value the InPort chip signature?
        je      possible_inport         ; Yes, go make sure we have an InPort.
        in      al,dx                   ; Read ID register again.
        cmp     al,bl                   ; Is value the InPort chip signature?
        jne     inport_not_found        ; No, return error

;
; At this point, we managed to read the InPort chip signature, so we have
;       a possible InPort chip. The next read from the ID register will
;       return the version/revision. We then make sure that the ID register
;       alternates between the chip signature and this version/revision. If
;       it does, we have an InPort chip.
;

possible_inport:
        in      al,dx                   ; Read version/revision.
        mov     ah,al                   ; Save it.
        mov     cx,5                    ; Test ID register 5 times.

inport_check:
        in      al,dx                   ; Read ID register.
        cmp     al,bl                   ; Make sure it is the chip signature.
        jne     inport_not_found        ; If not, we don't have an InPort chip.
        in      al,dx                   ; Read ID register.
        cmp     al,ah                   ; Make sure version/revision is same.
        jne     inport_not_found        ; If not, we don't have an InPort chip.
        loop    inport_check            ; Test desired number of times.

        clc
        ret
;
; At this point, we know we have an InPort chip. We now try to determine
;       what interrupt level it is jumpered at.
;

inport_not_found:                       ; We don't have an InPort chip.
        stc                             ; Show failure.
        ret                             ; Return to caller.

TestForInport   ENDP

;*****************************************************************************
;
;       TestForBus
;
;       This procedure will attempt to find a bus mouse adaptor in the system
;               and will return the results of this search.
;
;       ENTRY   None.
;
;       EXIT    NC              Bus mouse adaptor was found
;
;               CY              Bus mouse not found.
;
;       ALTERS  AX, BX, CX, DX
;
;       CALLS   FindBusInterrupt
;


TestForBus      PROC    NEAR

;
; We determine if the bus mouse adaptor is present by attempting to
;       program the 8255A, and then seeing if we can write a value out to
;       Port B on the 8255A and get that value back. If we can, we assume
;       that we have a bus mouse adaptor.
;

        mov     dx,BUS_INIT             ; Get address of 8255A control port.
        mov     al,BUS_INIT_VALUE       ; Get proper value.
        out     dx,al                   ; Set up 8255A.
        IOdelay
        mov     ax,0A5A5h               ; Get a signature byte and a copy.
        address BUS_SIG BUS_INIT        ; Get address of Port B.
        out     dx,al                   ; Set Port B with signature.
        IOdelay
        in      al,dx                   ; Read back Port B.
        IOdelay
        cmp     al,ah                   ; Does it match signature byte?
        stc                             ; Set return for no bus mouse adaptor.
        jne     bus_not_found           ; Nope - no bus mouse adaptor
        clc

bus_not_found:
        ret

TestForBus      ENDP

;***************************************************************************
;*
;* ArcnetCardPresent - Detects the presence of an Arcnet Card
;*
;* ENTRY: None.
;*
;* EXIT: Boolean
;*       AX = 1 , Arcnet Card Probably present
;*       AX = 0 , Arcnet Card Probably NOT Present
;*
;***************************************************************************
cPublic ArcnetCardPresent <FAR,PUBLIC>,<SI,DX>
localV  InputBytes, 4d
localV  ACP_First, 1d
cBegin  ArcnetCardPresent

      ; Read a BYTE from four different 16-bit I/O addresses that have
      ; the same lower 10 bits (2E0h).  2E0h is the address of the Status
      ; register for the Arcnet Cards that tend to be reset easily
      ; (a Read or write to I/O addresses 2E8, 2E9, 2EA, OR 2EB).
      ; Store the BYTE in InputBytes
      ;
      mov   dx, 56E0h
      IOdelay
      in    al, dx
      mov   InputBytes, al

      mov   dx, 0AAE0h
      IOdelay
      in    al, dx
      mov   InputBytes+1, al

      mov   dx, 0E2E0h
      IOdelay
      in    al, dx
      mov   InputBytes+2, al

      mov   dx, 1EE0h
      IOdelay
      in    al, dx
      mov   InputBytes+3, al

      xor   si, si                  ; Clear the loop counter

ACP_Loop:
      mov   al, [InputBytes+si]     ; Read a byte from table

      cmp   al, 0FFh                ; If it is FF or E0 (low Byte of I/O
      je    NoArcNet                ;  address we will assume that we read
                                    ;  air.
      cmp   al, 0E0h
      je    NoArcNet

      and   al, 78h                 ; bits 6 through 3 (0XXX X000) should
                                    ;  be constant

      cmp   si, 0h                  ; Is it first byte read ?
      je    ACP_First_Pass

      cmp   ACP_First, al           ; is it equal to the first Byte read
      jne   NoArcNet

      inc   si                      ; point to next byte in table
      cmp   si, 4d                  ; have we done all four bytes
      je    ArcNetFound             ; Yes, we have found an Arcnet Card

      jmp   ACP_Loop

ACP_First_Pass:
      mov   ACP_First, al           
      inc   si                      ; point to next byte in table
      jmp   ACP_Loop
      

ArcNetFound:
      mov   ax, 1                   ; return TRUE
      jmp   short ACP_End

NoArcNet:
      xor   ax, ax                  ; return FALSE

ACP_End:
cEnd    ArcnetCardPresent

;*****************************************************************************
;
; LookForSerialMouse
;
;*****************************************************************************
LookForSerialMouse PROC NEAR

      mov       ax,CRT_Data_Seg         ; Get ROM BIOS Data segment.
      mov       es,ax                   ; Establish addressability to it.
      assume    es:nothing

      xor       di,di                   ; Assume starting at COM1

serial_try_again:
      mov       dx,es:[di]              ; Get base address of COM port to test.
      or        dx,dx                   ; Does this port exist?
      jnz       serial_test_port        ; Yes, go and test it.
      cmp       di,4                    ; If this is COM3 or COM4, use the
      jb        serial_next_port        ;  internal addresses for them.
      je        @F                      ; If we have COM3, DX=0.
      inc       dx                      ; If we have COM4, DX=2.
      inc       dx                      ; ...

@@:
      mov       bx,dx                   ; BX has index into our table.
      cmp       bx,2                    ; Are we about to look at COM4 ?
      jne       NotCom4

      cCall     ArcnetCardPresent
      or        ax,ax                   ; if Arcnet Present do not do
      jnz       No_Serial_Mouse         ;   COM4 test as the Arcnet Card will
                                        ;   be Reset.

NotCom4:
      mov       dx,COM_3_4_Addrs[bx]    ; Get base address of the COM port.

serial_test_port:
;
; First, do a quick test to make sure something out there responds like a
;       COM port.
;
      push      dx                      ; Save base address.
      address   IIR RXB                 ; Address Interrupt ID register.
      DelayIn   al,dx                   ; AL=contents of Interrupt ID reg.
      pop       dx                      ; Restore base address.
      test      al,0F8h                 ; Make sure bits 3-7 are all 0!
      jnz       serial_next_port        ; No COM port -> no mouse.

      call      TestForSerial           ; Is a serial mouse attached to port?
      cmp       ax,NO_MOUSE
      jne       Serial_Mouse_Found      ; Yes!

serial_next_port:                       ; No serial mouse on this COM port.
      add       di,2                    ; Otherwise, move to the next possible
      cmp       di,8                    ; port and try there.
      jb        serial_try_again        ; Look for COM1 through COM4.

No_Serial_Mouse:
      mov       ax,NO_MOUSE

Serial_Mouse_Found:
      ret

LookForSerialMouse ENDP

;*****************************************************************************
;
; TestForSerial
;
;*****************************************************************************
TestForSerial   PROC    NEAR

      call      SaveCOMSetting

      call      SetupCOMForMouse        ; Set up COM port to talk to mouse.

      mov       cx,SHORTDELAY           ; Use a short delay time.
      call      ResetSerialMouse        ; Reset mouse to see if it is there.
      cmp       ax,NO_MOUSE
      jne       TFS_Found

      mov       cx,LONGDELAY            ; Maybe the mouse is just slow.
      call      ResetSerialMouse        ; Reset mouse to see if it is there.
      cmp       ax,NO_MOUSE
      jne       TFS_Found

      call      TestForLogitechSerial   ; Maybe it's a Logitech Series C

TFS_Found:
      push      ax                      ; Save return value
      call      RestoreCOMSetting
      pop       ax
      ret

TestForSerial   ENDP

;*****************************************************************************
;
;       SaveCOMSetting
;
;       This procedure will save the current state of the COM port given.
;
;       ENTRY   DX              Base address of COM port.
;
;       EXIT    none
;
;       ALTERS  AX
;
;       CALLS   none
;


SaveCOMSetting  PROC    NEAR

        push    dx                      ; Save base I/O address.
        address LCR RXB                 ; Get address of Line Control Register.
        DelayIn al,dx                   ; Get current contents.
        mov     [LCRSave],al            ; Save them.
        or      al,LC_DLAB              ; Set up to access divisor latches.
        DelayOut dx,al
        address LATMSB LCR              ; Get address of high word of divisor
        DelayIn al,dx                   ; latch and save its current contents.
        mov     [LATMSBSave],al
        address LATLSB LATMSB           ; Get address of low word of divisor
        DelayIn al,dx                   ; latch and save its current contents.
        mov     [LATLSBSave],al
        address LCR LATLSB              ; Get address of Line Control Register
        mov     al,[LCRSave]            ; and disable access to divisor.
        and     al,NOT LC_DLAB
        DelayOut dx,al
        address MCR LCR                 ; Get address of Modem Control Register
        DelayIn al,dx                   ; and save its current contents.
        mov     [MCRSave],al
        address IER MCR                 ; Get address of Interrupt Enable Reg-
        DelayIn al,dx                   ; ister and save its current contents.
        mov     [IERSave],al
        pop     dx                      ; Restore base I/O address.
        ret

SaveCOMSetting  ENDP

;*****************************************************************************
;
;       RestoreCOMSetting
;
;       This procedure will restore the state of the COM port.
;
;       ENTRY   DX              Base address of COM port.
;
;       EXIT    none
;
;       ALTERS  AX
;
;       CALLS   none
;


RestoreCOMSetting       PROC    NEAR

        push    dx                      ; Save base I/O address.
        address LCR RXB                 ; Get address of Line Control Register.
        mov     al,LC_DLAB              ; Set up to access divisor latches.
        DelayOut dx,al
        address LATMSB LCR              ; Get address of high word of divisor
        mov     al,[LATMSBSave]         ; and restore it.
        DelayOut dx,al
        address LATLSB LATMSB           ; Get address of low word of divisor
        mov     al,[LATLSBSave]         ; and restore it.
        DelayOut dx,al
        address LCR LATLSB              ; Get address of Line Control Register
        mov     al,[LCRSave]            ; and restore it, disabling access to
        and     al,NOT LC_DLAB          ; the divisor latches.
        DelayOut dx,al
        address MCR LCR                 ; Get addres of Modem Control Register
        mov     al,[MCRSave]            ; and restore it.
        DelayOut dx,al
        address IER MCR                 ; Get address of Interrupt Enable Reg-
        mov     al,[IERSave]            ; ister and restore it.
        DelayOut dx,al
        pop     dx                      ; Restore base I/O address.
        ret

RestoreCOMSetting       ENDP

;*****************************************************************************
;
;       SetupCOMForMouse
;
;       This procedure will set up the given COM port so that it can talk to
;               a serial mouse.
;
;       ENTRY   DX              Base address of COM port to set up
;
;       EXIT    COM port set up, all interrupts disabled at COM port
;
;       ALTERS  AX
;
;       CALLS   none
;


SetupCOMForMouse        PROC    NEAR

        push    dx                      ; Save base I/O address.
        address LCR RXB                 ; Get address of Line Control Reg.
        mov     al,LC_DLAB              ; Set up to access divisor latches.
        DelayOut dx,al
        address LATMSB LCR              ; Get address of high word of divisor
        mov     al,HIGH DIV_1200        ; latch and set it with value for
        DelayOut dx,al                  ; 1200 baud.
        address LATLSB LATMSB           ; Get address of low word of divisor
        mov     al,LOW DIV_1200         ; latch and set it with value for
        DelayOut dx,al                  ; 1200 baud.
        address LCR LATLSB              ; Get address of Line Control Reg.
        mov     al,LC_BITS7 + LC_STOP1 + LC_PNONE
        DelayOut dx,al                  ; Set 7,n,1; disable access to divisor.
        address IER LCR                 ; Get address of Int. Enable Register
        xor     al,al                   ; Disable all interrupts at the COM
        DelayOut dx,al                  ; port level.
        address LSR IER                 ; Get address of Line Status Reg.
        DelayIn al,dx                   ; Read it to clear any errors.
        pop     dx                      ; Restore base I/O address
        ret

SetupCOMForMouse        ENDP

;*****************************************************************************
ResetSerialMouse2  PROC    NEAR

        push    dx                      ; Save environment.
        push    si
        push    di
        push    es

        address IER RXB                 ; Get address of Interrupt Enable Reg.
        DelayIn al,dx                   ; Get current contents of IER and
        push    ax                      ; save them.
        push    dx                      ; Save address of IER.
        xor     al,al                   ; Disable all interrupts at the
        DelayOut dx,al                  ; COM port level.

        address MCR IER                 ; Get address of Modem Control Reg.
        mov     al,MC_DTR               ; Set DTR active; RTS, OUT1, and OUT2
        DelayOut dx,al                  ; inactive. This powers down mouse.

        push    cx                      ; Save amount of time to delay.
        call    SetupForWait            ; Set up BX:CX and ES:DI properly for
        assume  es:nothing              ; upcoming delay loop.

        address RXB MCR                 ; Get address of Receive Buffer.

;
; Now, we wait the specified amount of time, throwing away any stray
; data that we receive. This gives the mouse time to properly reset
; itself.
;

rsm2_waitloop:
        DelayIn al,dx                   ; Read and ignore any stray data.
        call    IsWaitOver              ; Determine if we've delayed enough.
        jnc     rsm2_waitloop            ; If not, keep waiting.

rsm2_waitover:
        address LSR RXB                 ; Get address of Line Status Reg.
        DelayIn al,dx                   ; Read it to clear any errors.
        address MCR LSR                 ; Get address of Modem COntrol Reg.
        mov     al,MC_DTR + MC_RTS + MC_OUT2 ; Set DTR, RTS, and OUT2 active
                                        ; OUT1 inactive.
        DelayOut dx,al                  ; This powers up the mouse.

        pop     cx                      ; Get amount of time to delay.
        call    SetupForWait            ; Set up BX:CX and ES:DI properly for
        assume  es:nothing              ; the upcoming delay loop.

;
; We give the mouse the specified amount of time to respond by sending
;       us an M. If it doesn't, or we get more than 3 characters that aren't
;       an M, we return a failure indication.
;

        address LSR MCR                 ; Get address of Line Status Reg.
        mov     si,0                    ; Read up to 3 chars from port.

rsm2_lookforM:
        DelayIn al,dx                   ; Get current status.
        test    al,LS_DR                ; Is there a character in Receive Buff?
        jnz     rsm2_gotchar             ; Yes! Go and read it.
        call    IsWaitOver              ; No, determine if we've timed out.
        jnc     rsm2_lookforM            ; Haven't timed out; keep looking.

        jmp     short rsm2_leave         ; Timed out. Leave with carry set.

rsm2_gotchar:
        address RXB LSR                 ; Get address of Receive Buffer.
        DelayIn al,dx                   ; Get character that was sent to us.
        mov     [SerialBuf+si],al
        address LSR RXB                 ; Oh well. Get address of LSR again.
        inc     si                      ; Have we read 3 chars yet?
        cmp     si,10d
        jne     rsm2_lookforM            ; Nope, we'll give him another try.

rsm2_leave:
        pop     dx                      ; Get address of IER.
        pop     ax                      ; Get old value of IER.
        DelayOut dx,al                  ; Restore IER.

        pop     es                      ; Restore environment.
        assume  es:nothing
        pop     di
        pop     si
        pop     dx
        ret

ResetSerialMouse2       ENDP

;*****************************************************************************
;
; ResetSerialMouse
;
; This procedure will reset a serial mouse on the given COM port and will
; return an indication of whether a mouse responded or not.
;
; The function now also checks for the presence of a 'B' as well as an 
; 'M' to determine the presence of a pointing device.  Also, if the 'M' is 
; followed by a '3' the serial mouse is a Logitech.
;
; Mouse     returns M
; Ballpoint returns B
;
; ENTRY   DX    Base I/O address of COM port to use
;         CX    Number of msecs to use for delays
;
; EXIT    AX    Mouse Type
;
;*****************************************************************************
ResetSerialMouse PROC NEAR

      push      dx                  ; Save environment.
      push      si
      push      di
      push      es

      address   IER RXB             ; Get address of Interrupt Enable Reg.       
      DelayIn   al,dx               ; Get current contents of IER and       
      push      ax                  ; save them.       
      push      dx                  ; Save address of IER.       
      xor       al,al               ; Disable all interrupts at the       
      DelayOut  dx,al               ; COM port level.       

      address   MCR IER             ; Get address of Modem Control Reg.       
      mov       al,MC_DTR           ; Set DTR active; RTS, OUT1, and OUT2       
      DelayOut  dx,al               ; inactive. This powers down mouse.       

      push      cx                  ; Save amount of time to delay.       
      call      SetupForWait        ; Set up BX:CX and ES:DI properly for       
      assume    es:nothing          ; upcoming delay loop.       

      address   RXB MCR             ; Get address of Receive Buffer.       
;
; Now, we wait the specified amount of time, throwing away any stray
; data that we receive. This gives the mouse time to properly reset
; itself.
;
rsm_waitloop:
      DelayIn   al,dx               ; Read and ignore any stray data.
      call      IsWaitOver          ; Determine if we've delayed enough.
      jnc       rsm_waitloop        ; If not, keep waiting.

; Wait is over.

      address   LSR RXB             ; Get address of Line Status Reg.
      DelayIn   al,dx               ; Read it to clear any errors.
      address   MCR LSR             ; Get address of Modem COntrol Reg.
      mov       al,MC_DTR+MC_RTS+MC_OUT2 ; Set DTR, RTS, and OUT2 active
                                         ; OUT1 inactive.
      DelayOut  dx,al               ; This powers up the mouse.

      pop       cx                  ; Get amount of time to delay.
      call      SetupForWait        ; Set up BX:CX and ES:DI properly for
      assume    es:nothing          ; the upcoming delay loop.
;
; We give the mouse the specified amount of time to respond by sending
; us an M. If it doesn't, or we get more than 5 characters that aren't
; an M, we return a failure indication.
;

      address   LSR MCR             ; Get address of Line Status Reg.
      mov       si,5                ; Read up to 5 chars from port.
      mov       bl,'3'              ; '3' will follow 'M' on Logitech.
      mov       bh,'B'              ; 'B' for BALLPOINT
      mov       ah,'M'              ; Get an M. (We avoid doing a cmp al,M
                                    ; because the M could be left floating
                                    ; due to capacitance.)
rsm_getchar:
      DelayIn   al,dx               ; Get current status.
      test      al,LS_DR            ; Is there a character in Receive Buff?
      jnz       rsm_gotchar         ; Yes! Go and read it.
      call      IsWaitOver          ; No, determine if we've timed out.
      jnc       rsm_getchar         ; Haven't timed out; keep looking.

      mov       bx,NO_MOUSE
      jmp short rsm_leave           ; Timed out. Leave with NO_MOUSE.

rsm_gotchar:
      address   RXB LSR             ; Get address of Receive Buffer.
      DelayIn   al,dx               ; Get character that was sent to us.
      cmp       al,ah               ; Is it an M?
      jne       check_for_b
;
; We received an 'M', now wait for next character to see if it is a '3'.
;
      mov       cx,1                ; Wait between 55.5 and 111ms for
      call      SetupForWait        ;   next character.
      address   LSR RXB             

rsm_waitfor3:
      DelayIn   al,dx               ; Get current status.
      test      al,LS_DR            ; Is there a character in Receive Buff?
      jnz       rsm_gotchar3        ; Yes! Go and read it.
      call      IsWaitOver          ; No, determine if we've timed out.
      jnc       rsm_waitfor3        ; Haven't timed out; keep looking.

; Not a Logitech - must be a standard Microsoft compatible serial mouse.

      jmp short rsm_notLT

rsm_gotchar3:
      address   RXB LSR             ; Get address of Receive Buffer.
      DelayIn   al,dx               ; Get character that was sent to us.
      cmp       al,bl               ; Is it a 3?
      jne       rsm_notLT

      mov       bx,LT_MOUSE         ; Yes, we've found a Logitech M+ series,
      address   LSR RXB             ; Choose Line Status Register.
      jmp short rsm_leave           ;   3 button mouse

rsm_notLT:
      mov       bx,SERIAL_MOUSE     ; We didn't get the '3' after the 'M'
      address   LSR RXB             ; Choose Line Status Register.
      jmp short rsm_leave           ; We still have a standard serial mouse.

check_for_b:
      cmp       al,bh               ; Is it a B?
      jne       rsm_next_char

      mov       bx,MSBPOINT_MOUSE   ; We've found a BallPoint Mouse
      address   LSR RXB             ; Choose Line Status Register.
      jmp short rsm_leave

rsm_next_char:
      address   LSR RXB             ; Oh well. Get address of LSR again.
      dec       si                  ; Have we read 5 chars yet?
      jnz       rsm_getchar         ; Nope, we'll give him another try.

; We've read many characters - No a single 'M' or 'B' in the lot.

      mov       bx,NO_MOUSE

rsm_leave:
      pushf                         ; Save flags


; Clear out any extra incoming characters

      mov       cx,2                ; Get amount of time to delay.
      call      SetupForWait        ; Set up BX:CX and ES:DI properly for
      assume    es:nothing          ; the upcoming delay loop.

rsm_ClearAll:
      DelayIn   al,dx               ; Read the LSR to clear any errors.
      address   RXB LSR             ; Get address of Receive Buffer.
      DelayIn   al,dx               ; Read and ignore any stray data.
      address   LSR RXB             ; Get address of Line Status Register.

rsm_ClearPort:
      call      IsWaitOver          ; Determine if we've timed out.
      jnc       rsm_ClearAll        ; Haven't timed out; keep looking.


      popf                          ; Restore flags

      pop       dx                  ; Get address of IER.
      pop       ax                  ; Get old value of IER.
      DelayOut  dx,al               ; Restore IER.

      pop       es                  ; Restore environment.
      assume    es:nothing
      pop       di
      pop       si
      pop       dx
      mov       ax,bx               ; Set return value.
      ret

ResetSerialMouse        ENDP

;*****************************************************************************
;
;       SetupForWait
;
;       This procedure accepts the number of milliseconds that we will want
;       to delay for and will set things up for the wait.
;
;       ENTRY   CX              Number of Clock ticks to wait for.
;
;       EXIT    None.
;
;       ALTERS  CX
;
;       CALLS   none
;


SetupForWait    PROC    NEAR

        push    ax                        ; Do your saving !
        push    es

        xor     ax,ax
        mov     es,ax                     ; Point to 40:6C = 0:46C
        mov     ax,es:[LW_ClockTickCount] ; Get tick count in AX.
        add     ax,cx                     ; [Current + delay] = delay ends.
        mov     ds:[SemaphoreWord],ax     ; Save ending time in Semiphore

        pop     es                        ; Restore now !
        pop     ax
        
        ret

SetupForWait    ENDP

;*****************************************************************************
;
;       IsWaitOver
;
;       This procedure accepts the current time and the ending time and
;       return and indication of whether the current time is past
;       the ending time.
;
;       ENTRY  None
;
;       EXIT    carry clear     Current time is not past ending time
;               carry set       Current time is past ending time
;
;       ALTERS  none
;
;       CALLS   none
;

IsWaitOver      PROC    NEAR

        push    ax                         ; Preserve AX
        push    es                         ; Preserve ES
        push    bx
        sti                                ; Hosebag BIOS bugs can't get me.
        xor     ax,ax
        mov     es,ax                      ; Point to 40:6C = 0:46C
        mov     ax,es:[LW_ClockTickCount]  ; Get tick count in AX.
        sti
        mov     bx,ds:[SemaphoreWord]
        cmp     bx,ax                      ; This will set carry accordingly.

        pop     bx
        pop     es                         ; Restore ES
        pop     ax                         ; Restore AX
        ret

IsWaitOver      ENDP

;*****************************************************************************
;
; LookForPS2Mouse
;
;*****************************************************************************
LookForPS2Mouse  PROC  NEAR
;
; Lets make the Int 15h, C201h BIOS call to reset the pointing device.
; If the call is supported, and returns success then we know that we have
; a PS/2 mouse.
;
      stc
      mov       bx,0ffffh           ; Initialize, see later if it changes.
      mov       ax,0c201h
      int       15h                 ; Reset Pointing device.
      jc        No_PS2_Mouse        ; Function call unsuccesful.

      cmp       bh,0ffh             ; BH should be device ID, it should not
      je        No_PS2_Mouse        ;   be what we initialized it to (FF).
;
; The following sequence of Int 15h calls will determine if a Logitech
; PS/2 mouse is present.  This information was obtained from Logitech.
;
      mov       ax,0C203h           ; Set resolution to 1 cnt/mm
      mov       bh,0h
      int       15h
      jc        PS2_Mouse             

      mov       ax,0C206h           ; Set scaling to 1:1
      mov       bh,1h
      int       15h
      jc        PS2_Mouse             

      mov       ax,0C206h           ; Set scaling to 1:1
      mov       bh,1h
      int       15h
      jc        PS2_Mouse             

      mov       ax,0C206h           ; Set scaling to 1:1
      mov       bh,1h
      int       15h
      jc        PS2_Mouse             

      mov       ax,0C206h           ; Get status
      mov       bh,0h
      int       15h
      jc        PS2_Mouse             

      or        cl,cl               ; Is resolution 1 cnt/mm?
      jz        PS2_Mouse           ; Yes, then not a Logitech.
;
; If cl is not zero (i.e. 1 cnt/mm) then it is the number of buttons
; and we've found a Logitech 3-button PS/2 mouse
;
LT_PS2_Mouse:
      mov       ax,LT_PS2MOUSE
      jmp short PS2MouseFound

PS2_Mouse:
      mov       ax,MSPS2_MOUSE
      jmp short PS2MouseFound

No_PS2_Mouse:
      mov       ax,NO_MOUSE

PS2MouseFound:
      ret

LookForPS2Mouse  ENDP


;*****************************************************************************
;
;       TestForPS2
;
;       This procedure will attempt to find a PS/2 mouse in the system and
;               return the results of this search.
;
;       ENTRY   fSingle8259     Non-zero if we have a single 8259A
;
;       EXIT    Carry clear     A PS/2 style Microsoft mouse was found
;               Mouse interrupts disabled at 8042.
;               Mouse disabled at 8042.
;               Mouse is reset.
;
;               Carry set       A PS/2 style Microsoft mouse was not found
;
;       ALTERS  AX, CX
;
;       CALLS   CheckForAuxSupport, MouseIntOff, CheckForMSMouse
;


TestForPS2      PROC    NEAR

        call    CheckForLogitechPS2     ; Check for Logitech PS/2 Mouse.
;       jmp     tfp_leave
        jc      tfp_CheckForMS          ; Check for MS/IBM mouse if not.
        mov     ax,LT_PS2MOUSE          ; Set the mouse type in AL
        jmp     short tfp_leave

tfp_CheckForMS:
        test    [fSingle8259],0FFh      ; Does machine have only 1 8259A?
        jnz     tfp_no_mouse            ; Yes - impossible to have PS/2 mouse.
        call    CheckForAuxSupport      ; Does machine support an aux device?
        jc      tfp_leave               ; Nope - impossible to have PS/2 mouse.
        call    MouseIntOff             ; Disable mouse and mouse interrupts.
        call    CheckForMSMouse         ; Is Microsoft mouse attached?
                                        ; If so, will be reset.
        jmp     short tfp_leave

tfp_no_mouse:
        stc                             ; Show that we failed.

tfp_leave:
        ret                             ; Carry set/clear appropriately.

TestForPS2      ENDP

;*****************************************************************************
;
;       CheckForAuxSupport
;
;       This procedure will determine if the 8042 supports an auxiliary device.
;
;       Note that this procedure will always leave with the keyboard enabled.
;
;       ENTRY   Keyboard enabled.
;
;       EXIT    Carry clear     The 8042 supports an auxiliary device
;               Interrupts enabled.
;               Aux device disabled.
;
;               Carry set       The 8042 does not support an auxiliary device
;               Interrupts enabled.
;
;       ALTERS  AX, BX, CX, DX, DI, ES
;
;       CALLS   Read8042CommandByte, WriteControlRegister
;


CheckForAuxSupport      PROC    NEAR

        mov     al,CMD8042_DISABLE_AUX  ; Disable the auxiliary device.
        call    WriteControlRegister
        jc      cfas_leave              ; Leave if error.
                                        ; INTERRUPTS DISABLED!!
        mov     al,CMD8042_DISABLE_KBD  ; Disable the keyboard.
        call    WriteControlRegister
        jc      cfas_leave              ; Leave if error.
                                        ; INTERRUPTS DISABLED!!

; We have to disable the keyboard interrupt here. This is because when the
;       8042 returns the status from the Test Auxiliary Device command,
;       it asserts IRQ 1. We can't just disable interrupts entirely, otherwise
;       we cannot time ourselves off of the system clock, so we have to do it
;       this way.

        in      al,MASTER_MASK_ADDR     ; Read interrrupt mask.
        IOdelay
        push    ax                      ; Save mask read in.
        or      al,00000010b            ; Mask keyboard interrupt.
        out     MASTER_MASK_ADDR,al     ; Write out changed mask.
        IOdelay

        call    Read8042CommandByte     ; Read the 8042 command byte.
                                        ; INTERRUPTS ENABLED!!
        jc      cfas_no_support         ; Leave if error.
        test    al,AUX_DISABLED         ; If the auxiliary device isn't
        jz      cfas_no_support         ; disabled, we don't have aux support.

;
; It is still possible that the 8042 supports an auxiliary device. We
;       now issue the 8042 command to test the auxiliary interface. This
;       command only exists on 8042s that support an auxiliary device. If the
;       test succeeds, a 00h will be placed in the 8042's output buffer. If no
;       output appears in the 8042's output buffer, it is assumed that the
;       8042 does not support an auxiliary device.
;
; WARNING!!! The testing of the auxiliary interface will more than likely
;       screw up an auxiliary device attached to it. Therefore, the auxiliary
;       device should be initialized after calling this procedure.
;

        mov     al,CMD8042_TEST_AUX     ; Command to test the auxiliary
        call    WriteControlRegister    ; interface.
        jc      cfas_no_support         ; Leave if error.
                                        ; INTERRUPTS DISABLED!!

        mov     cx,0ah                  ; We will wait half a second.
        call    SetupForWait            ; BX:CX has ending time, ES:DI points
                                        ;  at current time.
        sti                             ; Make sure interrupts are enabled so
                                        ;  timer ticks can get in.
        xor     dx,dx                   ; Clear byte read flag.

cfas_read_test_results:
        in      al,STATUS_8042          ; Get 8042's current status.
        test    al,I_FULL               ; Wait for 8042 to remove the command
        jnz     cfas_is_wait_over       ; from its input buffer.
        test    al,O_FULL               ; Is there an output byte yet?
        jz      cfas_is_wait_over       ; Nope, check if wait is over.

        in      al,DATA_8042            ; Read byte from 8042.
        or      dx,dx                   ; Have we read a byte yet?
        jnz     cfas_is_wait_over       ; Yes, ignore this byte.
        mov     ah,al                   ; Save first byte read in.
        inc     dx                      ; Show that a byte was read.

cfas_is_wait_over:
        call    IsWaitOver              ; Has interval expired?
        jnc     cfas_read_test_results  ; Nope, keep looking.

        or      dx,dx                   ; Did we get a byte?
        jz      cfas_no_support         ; Nope, no auxiliary device.
        or      ah,ah                   ; Was the byte we got 0?
        jz      cfas_clean_up           ; Yes, return with carry clear.

cfas_no_support:
        stc                             ; Show no aux device is supported.

cfas_clean_up:                          ; Carry clear/set appropriately.
        pop     ax                      ; Restore saved interrupt mask.
        sti                             ; Interrupts are reenabled.
        pushf                           ; Save result in carry.
        cli                             ; Interrupts off for mask update.
        mov     ah,al                   ; Put it in AH.
        in      al,MASTER_MASK_ADDR     ; Read in current mask.
        IOdelay
        and     ax,0000001011111101b    ; Unmask keyboard interrupt.
        or      al,ah                   ; Move in original state of keyboard.
        out     MASTER_MASK_ADDR,al     ; Write out changed mask.

cfas_reenable_kbd:
        mov     al,CMD8042_ENABLE_KBD   ; Reenable the keyboard.
        call    WriteControlRegister
        jc      cfas_reenable_kbd       ; Make sure we reenable the keyboard!!
                                        ; INTERRUPTS DISABLED!!
        popf                            ; Restore result and interrupts.
                                        ; INTERRUPTS ENABLED!!

cfas_leave:
        ret

CheckForAuxSupport      ENDP

;*****************************************************************************
;
;       Read8042CommandByte
;
;       This procedure will try to read the 8042's command byte.
;
;       Note that this procedure should only be called if both the keyboard
;               and auxiliary devices are disabled.
;
;       ENTRY   none
;
;       EXIT    Carry clear     Command byte was read successfully
;               AL              Command byte
;               Interrupts enabled
;
;               Carry set       Command byte couldn't be read
;               Interrupts enabled
;
;       ALTERS  AL
;
;       CALLS   WriteControlRegister
;


Read8042CommandByte     PROC    NEAR

        mov     al,CMD8042_READ_CMD     ; Tell 8042 that we want to read the
        call    WriteControlRegister    ; command byte.
        jc      r8cb_leave              ; Leave if error.

                                        ; INTERRUPTS DISABLED!
        push    cx                      ; Save register.
        mov     cx,400h                 ; Short delay because ints are off.

r8cb_loop:
        in      al,STATUS_8042          ; Get current status of 8042.
        test    al,O_FULL               ; Is there data in the output buffer?
        jnz     r8cb_got_data           ; Yes! Go and read it.
        loop    r8cb_loop               ; Keep trying.

        stc                             ; Show failure.
        jmp     short r8cb_leave2

r8cb_got_data:                          ; Carry is clear!
        in      al,DATA_8042            ; Get 8042's command byte.

r8cb_leave2:
        pop     cx                      ; Restore register.
        IOdelay                         ; Make sure interrupt is cleared.
        sti                             ; Reenable interrupts.
                                        ; INTERRUPTS ENABLED!

r8cb_leave:                             ; Carry set/clear appropriately.
        ret

Read8042CommandByte     ENDP

;*****************************************************************************
;
;       Write8042CommandByte
;
;       This procedure sets the 8042's command byte to the value given.
;
;       ENTRY   AL              Value to set the command byte to
;
;       EXIT    Carry clear     Command byte was set
;               Interrupts enabled
;
;               Carry set       Command byte couldn't be set
;               Interrupts enabled
;
;       ALTERS  none
;
;       CALLS   WriteControlRegister, WriteInputBuffer
;


Write8042CommandByte    PROC    NEAR

        push    ax                      ; Save value passed in.
        mov     al,CMD8042_WRITE_CMD    ; Tell 8042 that we want to write the
        call    WriteControlRegister    ; command byte.
        jc      w8cb_leave              ; Leave if we have an error.

                                        ; INTERRUPTS DISABLED!
        pop     ax                      ; Restore value.
        call    WriteInputBuffer        ; Send it to 8042.
                                        ; INTERRUPTS ENABLED!
        jc      Write8042CommandByte    ; Retry if unsuccessful.

w8cb_leave:                             ; Carry set/clear appropriately.
        ret

Write8042CommandByte    ENDP

;*****************************************************************************
;
;       WriteInputBuffer
;
;       This procedure sends a byte of data to the 8042's input buffer. It
;               should not be called unless the 8042 has already been set up to
;               expect this byte of data.
;
;       Note that interrupts should be disabled from the time that the command
;               is sent to the 8042 until the time that the data byte is
;               actually written.
;
;       ENTRY   AL              Byte of data to be sent.
;               Interrupts disabled
;
;       EXIT    Carry clear     Byte of data has been sent to the 8042.
;               Interrupts enabled
;
;               Carry set       Byte of data could not be sent to the 8042.
;               Interrupts enabled
;
;       ALTERS  none
;
;       CALLS   none
;

WriteInputBuffer        PROC    NEAR

;
; Upon entry, it is assumed that a command has just been sent to the
;       8042. Therefore, we will wait until the 8042 has removed the command
;       byte from its input buffer before doing anything else.
;
; Note that because interrupts are disabled at this time, we will only
;       wait a short while before timing out.
;

        push    cx                      ; Save registers that we use
        push    ax

        mov     cx,400h                 ; Short delay

wib_wait:
        in      al,STATUS_8042          ; Get 8042's current status.
        test    al,I_FULL               ; Is the input buffer still full?
        jz      wib_check_output        ; Nope - can now test for output.
        loop    wib_wait                ; Keep waiting.

        jmp     short wib_failure       ; Timed out - return with failure.

wib_check_output:
        and     al,O_FULL + A_FULL      ; Keep output and aux buffer bits.
        cmp     al,O_FULL               ; Output buffer contain keybd data?
        jne     wib_check_aux_data      ; Nope - check for aux data.

wib_failure:
        pop     ax                      ; Restore saved data byte.
        stc                             ; Show failure.
        jmp     short wib_leave         ; Return to caller

wib_check_aux_data:
        cmp     al,O_FULL + A_FULL      ; Output buffer contain aux data?
        jne     wib_success             ; Nope, can just send data byte.
        in      al,DATA_8042            ; Yup - read and discard data.

wib_success:                            ; Input and output buffers are empty.
        pop     ax                      ; Get saved data byte.
        out     DATA_8042,al            ; Send data byte.
        clc                             ; Show success.

wib_leave:
        pop     cx                      ; Restore register.
        sti                             ; Reenable interrupts.
        ret

WriteInputBuffer        ENDP

;*****************************************************************************
;
;       WriteControlRegister
;
;       This procedure will send a command byte to the 8042's control register.
;       Note that nothing will be sent as long as the 8042's input or
;       output buffer is full.
;
;       Note that only 15*64K attempts will be made before timing out.
;
;       ENTRY   AL              Command byte to be sent
;
;       EXIT    Carry clear     Byte was written to 8042 successfully
;               Interrupts disabled
;
;               Carry set       Byte could not be written to 8042
;               Interrupts enabled
;
;       ALTERS  none
;
;       CALLS   none
;


WriteControlRegister    PROC    NEAR

        push    cx                      ; Save registers we use.
        push    ax

        mov     ah,15                   ; Set up for looping 15*64K times.
        xor     cx,cx

;
; At this point, we wait until the input and output buffers are empty.
;       Interrupts have to be disabled from the time we find the buffers
;       empty and write the command byte until the time that the data that
;       the command produces is read. Therefore, we will disable and enable
;       interrupts each time through the loop to ensure that interrupts will
;       not be disabled for too long a time.
;

wcr_loop:
        cli                             ; Disable ints before reading status.
        in      al,STATUS_8042          ; Get 8042's current status.
        test    al,I_FULL + O_FULL      ; Are input and output buffers empty?
        jz      wcr_success             ; Yes! Go write the command byte.
        sti                             ; Nope. Reenable ints and keep trying
        loop    wcr_loop                ; the desired number of times.
        dec     ah
        jnz     wcr_loop

;
; Interrupts are enabled.
;

        pop     ax                      ; Restore registers.
        pop     cx

        stc                             ; Show that we failed.
        ret

;
; Interrupts are disabled and carry is clear (from TEST instruction).
;

wcr_success:
        pop     ax                      ; Get saved command byte.
        out     CONTROL_8042,al         ; Write command byte out.
        pop     cx                      ; Restore register.
        ret                             ; Carry is already clear.

WriteControlRegister    ENDP

;*****************************************************************************
;
;       MouseIntOff
;
;       This procedure will disable the mouse and mouse interrupts at the 8042
;               level.
;
;       Note that it assumes that the keyboard is enabled upon entry to this
;               procedure.
;
;       ENTRY   none
;
;       EXIT    Carry clear     Mouse and mouse interrupts are disabled.
;
;               Carry set       Some error occurred.
;
;       ALTERS  AL
;
;       CALLS   WriteControlRegister, Read8042CommandByte, Write8042CommandByte
;


MouseIntOff     PROC    NEAR

        mov     al,CMD8042_DISABLE_AUX  ; Disable the aux device.
        call    WriteControlRegister
        jc      mioff_leave             ; Leave if error.
        mov     al,CMD8042_DISABLE_KBD  ; Disable the keyboard.
        call    WriteControlRegister
        jc      mioff_leave             ; Leave if error.
                                        ; INTERRUPTS DISABLED!!

        call    Read8042CommandByte     ; Get the current command byte.
                                        ; INTERRUPTS ENABLED!!
        jc      mioff_cleanup           ; Leave if error.
        and     al,not AUX_INT_ENABLE   ; Disable mouse interrupts.
        call    Write8042CommandByte    ; Write out altered command byte.

mioff_cleanup:
        pushf                           ; Save return status.

mioff_reenable_kbd:
        mov     al,CMD8042_ENABLE_KBD   ; Reenable the keyboard.
        call    WriteControlRegister    ; If an error occurs, keep trying to
        jc      mioff_reenable_kbd      ; reenable it.
                                        ; INTERRUPTS DISABLED!!
        popf                            ; Restore return status.
                                        ; INTERRUPTS ENABLED!!

mioff_leave:                            ; Carry set/clear appropriately.
        ret

MouseIntOff     ENDP

;*****************************************************************************
;
;      CheckForMSMouse
;
;      This procedure checks to see if a Microsoft mouse or IBM mouse 
;      is attached to the auxiliary interface of the 8042.
;
;      ENTRY      Mouse interrupts disabled.
;
;      EXIT      Carry clear      A PS/2 style mouse was found
;            AX            MS, IBM , Logitech
;            Mouse interface disabled.
;            Mouse is reset.
;
;            Carry set      PS/2 mouse was not found
;
;      ALTERS      AX, CX
;
;      CALLS      SendToMouse, SendToMouseAndTestData, GetMouseData
;            WriteControlRegister, ResetPS2Mouse
;


CheckForMSMouse      PROC      NEAR

      mov      al,CMD8042_ENABLE_AUX      ; Make sure aux interface is enabled
      call      WriteControlRegister      ; so we can talk to the mouse.
      jc      cfmm_leave            ; Leave if we can't enable it.

      mov      cx,5                  ; Only 5 tries at this!

cfmm_retry:
      call      ResetPS2Mouse            ; Try to reset the mouse.
      jc      cfmm_failure            ; Try again if we failed.
      
;      call    CheckForLogitechPS2     ; Check for Logitech PS/2 Mouse.
;      jc      noLogiTech                   ; Logitech mouse not found
;      mov     ax,LT_PS2MOUSE
;      jmp     short cfmm_cleanup      ; mouse type determined

;
; The following sequence of commands checks to see if the mouse that we
;      just succeeded in resetting is a Microsoft mouse. 
;
; Note that we have to check to determine if the mouse responds with either
;      MCMD_ACK or with what was sent because of the differences between
;      the 200ppi and 400ppi PS/2 style mouse. The 200ppi mouse responds
;      with MCMD_ACKs, while the 400ppi mouse responds with whatever was
;      sent to it.
;
noLogiTech:
      mov      al,MCMD_ECHO            ; Send Select Echo Command.
      call      SendToMouse
      jc      cfmm_failure            ; Try again if we failed.

      mov      ax,'GG'                  ; Next "command" in signature.
      call      SendToMouseAndTestData
      jc      cfmm_failure            ; Try again if we failed.

      mov      ax,'DD'                  ; Next "command" in signature.
      call      SendToMouseAndTestData
      jc      cfmm_failure            ; Try again if we failed.

      mov      ax,'GG'                  ; Next "command" in signature.
      call      SendToMouseAndTestData
      jc      cfmm_failure            ; Try again if we failed.

      mov      al,MCMD_KILL_ECHO      ; Leave Echo mode.
      call      SendToMouse
      jnc      cfmm_signature_sent      ; If successful, have sent signature.

cfmm_failure:
      loop      cfmm_retry            ; Try again.
;
; at this point there is no mouse on AUX port at all
;

       stc                              ; carry set for no mouse at all.
       jmp      short cfmm_cleanup

;
; At this point, we have sent the signature string to the mouse.
;      If we have a Microsoft mouse out there, it will respond to a
;      Microsoft extended command.  If there is no response we have an
;      IBM mouse.
;
; Note that we have to test to see if the mouse responds with either a
;      MCMD_ACK or a MCMD_RESEND. This is because a 200ppi mouse responds
;      with MCMD_ACK, while a 400ppi mouse responds with MCMD_RESEND.
;

cfmm_signature_sent:
      mov      ax,(MCMD_RESEND SHL 8) + MCMD_MS_VERS ; Set command and return.
      call      SendToMouseAndTestData
      jc      cfmm_cleanup            ; Leave if unable to send command.

; IBM Mouse fails on this call because it will not send data in
; response to the above Microsoft version command

        call      GetMouseData            ; Read current version number.
      jc      noVerNumber            ; must have IBM mouse
        mov     ax,MSPS2_MOUSE          ; return MS PS2 Mouse.
      jmp      short cfmm_cleanup      ; mouse type determined

noVerNumber:
      mov      ax,IBMPS2_MOUSE              ; ax = IBM PS/2 mouse found.
      clc                        ; clear carry to show mouse presence
;
; Disable the auxiliary interface before leaving.
;

cfmm_cleanup:
      ; carry set here means no PS/2 mouse
      pushf                        ; Save state of carry.
      push      ax                  ; Save version number.

cfmm_disable_aux:
      mov      al,CMD8042_DISABLE_AUX      ; Disable the auxiliary interface.
      call      WriteControlRegister
      jc      cfmm_disable_aux      ; Make sure aux interface is disabled.

      pop      ax                  ; Restore version number.
      popf                        ; Restore carry.

;
; We have found a Microsoft mouse.  The version number is in AL.
;

cfmm_leave:                         

        ; carry clear means mouse present, al indicates its type
      ; carry set upon return means no mouse found

      ret

CheckForMSMouse      ENDP

;*****************************************************************************
;
;       SendToMouse
;
;       This procedure sends a byte of command or data to the mouse.
;               Because it appears that the mouse always acknowledges the
;               sending of this byte, this is also tested for here.
;
;       ENTRY   AL              Byte of command/data to send
;
;       EXIT    Carry clear     Command/data byte sent, mouse sent MCMD_ACK
;               Interrupts enabled.
;
;               Carry set       Command/data byte not sent, or mouse never
;                                sent back MCMD_ACK
;               Interrupts enabled.
;
;       ALTERS  AX
;
;       CALLS   SendToMouseAndTestData
;


SendToMouse     PROC    NEAR

        mov     ah,MCMD_ACK           ; We will only check for MCMD_ACK being
                                      ; returned from mouse.

IF2
.ERRNZ  ($ - SendToMouseAndTestData)  ; Because we fall into it.
ENDIF

SendToMouse     ENDP

;*****************************************************************************
;
;       SendToMouseAndTestData
;
;       This procedure sends a byte of command or data to the mouse.
;               It will then test to ensure that the mouse either sent
;               back what the caller is expecting or MCMD_ACK.
;
;       ENTRY   AL              Byte of command/data to send
;               AH              Byte of data the mouse is to return
;
;       EXIT    Carry clear     Command/data byte sent, mouse responded
;                                correctly
;               Interrupts enabled.
;
;               Carry set       Command/data byte not sent, or mouse never
;                                responded correctly
;               Interrupts enabled.
;
;       ALTERS  AX
;
;       CALLS   WriteControlRegister, WriteInputBuffer, CheckMouseData
;


SendToMouseAndTestData  PROC    NEAR

        push    ax                      ; Save command/data byte.
        mov     al,CMD8042_WRITE_AUX    ; Tell 8042 we want to send a byte to
        call    WriteControlRegister    ; the auxiliary device.
        pop     ax                      ; Get saved command/data byte.
        jc      stmatd_leave            ; Leave if error.

                                        ; INTERRUPTS DISABLED!
        call    WriteInputBuffer        ; Send data/command to aux dev.
                                        ; INTERRUPTS ENABLED!
        jc      SendToMouseAndTestData  ; Retry if unsuccessful.

        mov     al,MCMD_ACK             ; Make sure mouse acknowledges that
                                        ; command/data byte was sent.

if2     ; Pass 2 of the assembler
.errnz  ($ - CheckMouseData)            ; Because we drop into it
endif   ; Pass 2 of the assembler

SendToMouseAndTestData  ENDP

;*****************************************************************************
;
;       CheckMouseData
;
;       This procedure checks that the next byte that the mouse sends us is
;               what we were expecting it to send.
;
;       Note that this procedure cannot be called with interrupts disabled.
;               If it is, interrupts will potentially be left off for a
;               long time.
;
;       ENTRY   AL              One possible data byte the mouse can send
;               AH              A second valid data byte the mouse can send
;               Interrupts enabled
;
;       EXIT    Carry clear     Mouse sent data we were expecting
;
;               Carry set       Mouse didn't send data, or sent something we
;                               weren't expecting.
;
;       ALTERS  none
;
;       CALLS   GetMouseData
;

CheckMouseData  PROC    NEAR

        push    bp                      ; Save BP.
        push    ax                      ; Save data bytes to test for.
        mov     bp,sp                   ; Address data bytes on stack.
        call    GetMouseData            ; Get next byte of data from mouse.
        jc      cmd_leave               ; Leave if error.
        cmp     al,ah                   ; Is it what we were expecting?
        je      cmd_leave               ; Yes! Leave with carry clear.
        cmp     al,byte ptr [bp]        ; Is it other byte we were expecting?
        je      cmd_leave               ; Yes! Leave with carry clear.
        stc                             ; Nope, show error and leave.

cmd_leave:                              ; Carry set/clear appropriately.
        pop     ax                      ; Restore data bytes.
        pop     bp                      ; Restore BP.

stmatd_leave:                           ; Carry set/clear appropriately.
        ret

CheckMouseData  ENDP

;*****************************************************************************
;
;       GetMouseData
;
;       This procedure will try to read a byte of data from the mouse. It will
;               only try 15 * 64K times before timing out.
;
;       Note that this procedure cannot be called with interrupts disabled.
;               If it is, interrupts will potentially be left off for a
;               long time.
;
;       ENTRY   Interrupts enabled
;
;       EXIT    Carry clear     The byte of data was read successfully
;               AL              The byte of data that was read
;
;               Carry set       The byte of data could not be read
;
;       ALTERS  none
;
;       CALLS   none
;


GetMouseData    PROC    NEAR

        push    cx                      ; Save registers that we use.
        push    ax

        mov     ah,15                   ; Set up to loop 15*64K times.
        xor     cx,cx

gmd_loop:
        in      al,STATUS_8042          ; Get current 8042 status.
        test    al,O_FULL               ; Is the output buffer full?
        jz      gmd_buffer_empty        ; Nope, keep trying.
        test    al,A_FULL               ; Yes! Now is data from the mouse?
        jnz     gmd_got_aux_data        ; Yes!! Go and read it.

gmd_buffer_empty:                       ; No data from the mouse yet.
        loop    gmd_loop                ; Keep trying the desired number of
        dec     ah                      ; times.
        jnz     gmd_loop

;
; We timed out waiting for the mouse to send data.
;

        pop     ax                      ; Restore registers.
        pop     cx
        stc                             ; Show failure.
        ret

;
; There is mouse data to be read!
;

gmd_got_aux_data:                       ; Carry is clear already.
        pop     ax                      ; Restore AX so can return value in AL.
        in      al,DATA_8042            ; Read byte of data from mouse.
        pop     cx                      ; Restore other register.
        ret

GetMouseData    ENDP

;*****************************************************************************
;
;       ResetPS2Mouse
;
;       This procedure attempts to reset a PS/2 mouse. It does this by sending
;               a reset command to the mouse and checking to ensure that
;               it passed its diagnostics.
;
;       ENTRY   Mouse interrupts disabled.
;
;       EXIT    Carry clear     Mouse has been reset correctly.
;               AL              ID Code
;
;               Carry set       No mouse or couldn't reset it.
;
;       ALTERS  AX
;
;       CALLS   SendToMouse, CheckMouseData, GetMouseData
;


ResetPS2Mouse   PROC    NEAR

        push    cx                      ; Save register.
        mov     cx,3                    ; Will only try 3 times.

rpm_retry:
        mov     al,MCMD_RESET           ; Send the reset command to the mouse.
        call    SendToMouse
        jc      rpm_failure             ; Try again if we failed.

        mov     ax,(MCMD_DIAG_OK SHL 8) + MCMD_DIAG_OK ; Set valid data byte.
        call    CheckMouseData          ; Make sure mouse responds with it.
        jc      rpm_failure             ; Try again if we failed.

        call    GetMouseData            ; Make sure mouse also sends ID code.
        jnc     rpm_leave               ; If it has, leave with success.

rpm_failure:
        loop    rpm_retry               ; Keep trying.

rpm_leave:                              ; Carry set/clear appropriately.
        pop     cx                      ; Restore register.
        ret

ResetPS2Mouse   ENDP

;*****************************************************************************
;
;      CheckForLogiTechPS2
;
;       look for LogiTech PS/2 mouse
;
;      ENTRY      none
;
;      EXIT      Carry clear      Logitech PS/2 mouse found
;
;      ALTERS      AX
;
;      CALLS      SendToMouse, SendToMouseAndTestData
;

CheckForLogitechPS2 PROC NEAR

      ; the next sequence of commands is the way to see if the
      ; particular mouse is present
      
      mov      ax,0C203h
      mov      bh,0h
      int      15h
      jc       logiTech_failure             

      mov      ax,0C206h
      mov      bh,1h
      int      15h
      jc       logiTech_failure             

      mov      ax,0C206h
      mov      bh,1h
      int      15h
      jc       logiTech_failure             

      mov      ax,0C206h
      mov      bh,1h
      int      15h
      jc       logiTech_failure             

      mov      ax,0C206h
      mov      bh,0h
      int      15h
      jc       logiTech_failure             

      or       cl,cl
      stc
      jz       logitech_failure

      clc

logiTech_failure:
      ret

CheckForLogitechPS2 ENDP

;*****************************************************************************
;
;  TestForLogitechSerial 
;
;     This procedure will detect the presence of a Logitech Series C
;     serial mouse is present
;
;*****************************************************************************
TestForLogitechSerial PROC NEAR

      call      setupForLogitechSerial ; set COM port to talk to this mouse

      address   IER RXB             ; Get address of Interrupt Enable Reg.
      DelayIn   al,dx               ; Get current contents of IER and
      xor       al,al               ; Disable all interrupts at the
      DelayOut  dx,al               ; COM port level.
      address   MCR IER             ; Get address of Modem Control Reg.
      mov       al,MC_DTR           ; Set DTR active; RTS, OUT1, and OUT2
      DelayOut  dx,al               ; inactive. This powers down mouse.
      mov       cx,9d               ; wait for 1/2 second to pwrdwn mouse
      call      SetupForWait        ; Set up BX:CX and ES:DI properly for
      assume    es:nothing          ; upcoming delay loop.

      address   LSR MCR             ; Get address of Line Status Reg.
;
; Now, we wait the specified amount of time, throwing away any stray
; data that we receive. This gives the mouse time to properly reset
; itself.
;
lt_waitloop:
      DelayIn   al,dx               ; Read and ignore any stray data.
      call      IsWaitOver          ; Determine if we've delayed enough.
      jnc       lt_waitloop         ; If not, keep waiting.

      address   MCR LSR             ; Get address of Modem COntrol Reg.
      mov       al,MC_DTR+MC_RTS+MC_OUT2 ; Set DTR, RTS, and OUT2 active
                                         ; OUT1 inactive.
      DelayOut  dx,al               ; This should power up the mouse.

      mov       cx,9d               ; wait for 1/2 second to pwrup mouse
      call      SetupForWait        ; Set up BX:CX and ES:DI properly for
      assume    es:nothing          ; upcoming delay loop.
                                    ; ask for current baud rate
lt_waitloop2:
      call      IsWaitOver          ; Determine if we've delayed enough.
      jnc       lt_waitloop2

      address   TXB MCR             ; Get address of Transmit Buffer.
      mov       al, 073h            ; Ask for status - send 's'
      DelayOut  dx,al

      mov       cx,1                ; Wait at least 55.5 ms for response.
      call      SetupForWait
      assume    es:nothing
      
      address   RXB TXB             ; Get address of Receive Buffer.

lt_waitloop3:
      DelayIn   al,dx               ; Get character that was sent to us.
      call      IsWaitOver
      jnc       lt_waitloop3

      and       al,10111111b        ; Mask off the mode bit. bit 6.
      cmp       al, 0fh             ; al = Fh means command understood
      jne       no_lt_found

      mov       ax,LT_MOUSE
      jmp short lt_leave

no_lt_found:
      mov       ax,NO_MOUSE

lt_leave:
      ret

TestForLogitechSerial ENDP

;*****************************************************************************
;
; setupForLogitechSerial
;
; This procedure will set up the given COM port so that it can talk to
;   a Logitech C series serial mouse.
;
; ENTRY DX   Base address of COM port to set up
;
; EXIT  COM port set up, all interrupts disabled at COM port
;
; ALTERS  AX
;
; CALLS none
;
setupForLogitechSerial  PROC  NEAR

  push  dx    ; Save base I/O address.
  address LCR RXB   ; Get address of Line Control Reg.
  mov al,LC_DLAB  ; Set up to access divisor latches.
  DelayOut  dx,al
  address LATMSB  LCR ; Get address of high word of divisor
  mov al,HIGH DIV_1200  ; latch and set it with value for
  DelayOut  dx,al   ; 1200 baud.
  address LATLSB LATMSB ; Get address of low word of divisor
  mov al,LOW DIV_1200 ; latch and set it with value for
  DelayOut  dx,al   ; 1200 baud.
  address LCR  LATLSB ; Get address of Line Control Reg.
  mov al,LC_BITS8 + LC_STOP1 + LC_PNONE
  DelayOut  dx,al   ; Set 7,n,1; disable access to divisor.
  address IER  LCR    ; Get address of Int. Enable Register
  xor al,al   ; Disable all interrupts at the COM
  DelayOut  dx,al   ; port level.
  address LSR  IER    ; Get address of Line Status Reg.
  DelayIn al,dx   ; Read it to clear any errors.
  pop dx    ; Restore base I/O address
  ret

setupForLogitechSerial  ENDP

sEnd     CODE

end
