;
; Name:         VideoID
;
; Function:     Detects the presence of various video subsystems and
;                associated monitors.
;
; Caller:       Microsoft C:
;
;                       void VideoID(VIDstruct);
;
;                       struct
;                       {
;                         char VideoSubsystem;
;                         char Display;
;                       }
;                               *VIDstruct[2];
;
;               Subsystem ID values:
;                                0  = (none)
;                                1  = MDA
;                                2  = CGA
;                                3  = EGA
;                                4  = MCGA
;                                5  = VGA
;                                6  = 8514/A
;                                7  = XGA
;                               80h = HGC
;                               81h = HGC+
;                               82h = Hercules InColor
;
;               Display types:   0  = (none)
;                                1  = MDA-compatible monochrome
;                                2  = CGA-compatible color
;                                3  = EGA-compatible color
;                                4  = PS/2-compatible monochrome
;                                5  = PS/2-compatible color
;                                6  = 8514/A Monitor
;
;
;       The values returned in VIDstruct[0].VideoSubsystem and
;       VIDstruct[0].Display indicate the currently active subsystem.
;
;----------------------------------------------------------------------------


cssize equ 2    ;Used for small memory model

ARGpVID         EQU     [bp+4+cssize]   ; Pointer to structure

VIDstruct       STRUC                   ; corresponds to C data structure

Video0Type      DB      ?               ; first subsystem type
Display0Type    DB      ?               ; display attached to first
                                        ;  subsystem
Mode0           DB      ?               ; Mode of first subsystem
NumCols0        DB      ?               ; Number of columns on first
iMemory0        DW      ?               ; Memory in first system
Video1Type      DB      ?               ; second subsystem type
Display1Type    DB      ?               ; display attached to second
                                        ;  subsystem
Mode1           DB      ?               ; Mode of second subsystem
NumCols1        DB      ?               ; Number of columns on second
iMemory1        DW      ?               ; Memory in second system

VIDstruct       ENDS


Device0          EQU     word ptr Video0Type[di]
Device1          EQU     word ptr Video1Type[di]


MDA              EQU     1               ; subsystem types
CGA              EQU     2
EGA              EQU     3
MCGA             EQU     4
VGA              EQU     5
Eighty514        EQU     6
XGA              EQU     7
HGC              EQU     80h
HGCPlus          EQU     81h
InColor          EQU     82h

MDADisplay       EQU     1               ; display types
CGADisplay       EQU     2
EGAColorDisplay  EQU     3
VGAMonoDisplay   EQU     4
VGAColorDisplay  EQU     5
Eighty514Display EQU     6

TRUE             EQU     1
FALSE            EQU     0


DGROUP          GROUP   _DATA

_TEXT           SEGMENT byte public 'CODE'
                ASSUME  cs:_TEXT,ds:DGROUP

                PUBLIC  _VideoID
                EXTRN   C ArcnetCardPresent:FAR

_VideoID        PROC    far

                push    bp              ; preserve caller registers
                mov     bp,sp
                push    es              ; Save caller's ES
                push    si
                push    di

; initialize the data structure that will contain the results

                les     di,ARGpVID  ; ES:DI -> start of data structure

                mov     es:Device0,0       ; zero these variables
                mov     es:Device1,0

; look for the various subsystems using the subroutines whose addresses are
;  tabulated in TestSequence; each subroutine sets flags in TestSequence
;  to indicate whether subsequent subroutines need to be called

                mov     byte ptr TestSequence,TRUE
                mov     byte ptr XGAflag,FALSE
                mov     byte ptr Eighty514flag,FALSE
                mov     byte ptr EGAflag,TRUE
                mov     byte ptr CGAflag,TRUE
                mov     byte ptr Monoflag,TRUE

                mov     cx,NumberOfTests
                mov     si,offset DGROUP:TestSequence

L01:            lodsb                   ; AL := flag
                test    al,al
                lodsw                   ; AX := subroutine address
                jz      L02             ; skip subroutine if flag is false

                push    si
                push    cx
                call    ax              ; call subroutine to detect
                                        ;  subsystem
                pop     cx
                pop     si

L02:            loop    L01

; determine which subsystem is active

                call    FindActive

                pop     di              ; restore caller registers and
                                        ;  return
                pop     si
                pop     es
                mov     sp,bp
                pop     bp
                ret

_VideoID        ENDP


; FindXGA
;
; Note:  XGA adapters MAY be able to be detected by checking port
; 2110H.  Read the port, toggle bit 0, write to the port, wait,
; read the port and see if the port's value is the changed.  If it
; did, it is an XGA.  Don't forget to toggle the bit back again,
; and it might not hurt to disable interrupts (CLI) during the check.
;
; Bit 0 toggles the XGA's coprocessor between Intel and Motorola
; modes (backwards storage for 16 and 32 bit values).  Also, the XGA
; standard allows for up to six XGA adapters to exist in a system
; at one time, each port address is 16 (10H) higher than the other.
; Checking 2120H, 2130H, 2140H, 2150H, and 2160H may be well advised.
;
; In the short time we had to hand over our code, I was not able to
; determine how to detect the monitor type attached to the XGA.
; Sorry about that.  Also, I was not able to test this XGA detection
; theory out (except to see that VGAs did NOT retain the toggling of
; bit zero).
;


FindXGA         PROC    near

         mov  bx, OFFSET DGROUP:rgXgaPorts ; Store the XGA port address array location

XgaTestLoop:
         mov  dx, [bx]            ; Get the XGA's port address
         and  dx, dx              ; Jump out if it's zero
         jz   fnXgaDone

         in   al, dx              ; Read the port
         mov  ch, al              ; Store the "original" port value

         xor  al, 1               ; change bit 0

         cli                      ; Clear all interrupts.  This is
                                  ;   important because this routine
                                  ;   changes the way the XGA functions.

         out  dx, al              ; Output the new value to the XGA port.

         jmp  short XgaWait01     ; Pause briefly, to allow the change
XgaWait01:                        ;   to take effect.
         jmp  short XgaWait02
XgaWait02:
         jmp  short XgaWait03
XgaWait03:

         in   al, dx              ; Get the "changed" value from the port.
         mov  cl, al              ; Store the "changed" value.

         mov  al, ch              ; Send the "original" back to the port.
         out  dx, al

         jmp  short XgaWait04     ; Pause briefly, to allow the change
XgaWait04:                        ;   to take effect.
         jmp  short XgaWait05
XgaWait05:
         jmp  short XgaWait06
XgaWait06:

         sti                      ; Restore all interrupts.

; I consider it a sucessful test if only bit zero changed.  More changes
;   might indicate that I am communicating with a different device.

         mov  ch, cl              ; Copy the "changed" port value to bh
         mov  ah, al              ; Copy the "original" port value to ah
         and  ch, 0FEh            ; Mask off bit 0 of "changed" value.
         and  ah, 0FEh            ; Mask off bit 0 of "original" value.

         cmp  ch, ah              ; Q: Do they match?
         jne  fnXgaNotFound       ; N: This is not an XGA port.

         cmp  cl, al              ; Q: Since bits 1-7 match, do all 8
                                  ;    bits match?
         jne  fnFoundXga          ; N: This is an XGA port.

; Keep looking
fnXgaNotFound:

         inc  bx                  ; Bump the port pointer.
         inc  bx
         jmp  short XgaTestLoop   ; Test the next port.


; This is an XGA.
fnFoundXga:

         mov  al, XGA             ; Say it is an XGA adapter.

; Currently, I do not have a test for the monitor.  I will assume it
;   is an 8514/A Monitor until I have an accurate test.

         mov  ah,Eighty514Display ; AH := Display type.

         call FoundDevice         ; Store the answer.

fnXgaDone:
         ret

FindXGA         ENDP


;
; Find8514
;
; This function detects the presence of an 8514 display card.
;
; The way we do this is to first write to the Error Term Register and then
; make sure we can read back the value we wrote. Then we check to see
; what kind of monitor is attached since the 8514 can function like a VGA.
;

Find8514        PROC    near

ERR_TERM        equ     92e8h     ; 8514 error term register.
SUBSYS_STAT     equ     42e8h     ; 8514 Subsystem status register.

         call ArcnetCardPresent
         or   ax,ax               ; if Arcnet Present do not do the 8514/a
         jnz  fn8514NotFound      ;   test as the Arcnet Card will be reset.

         mov  dx, ERR_TERM        ; load DX with port address (error term port
         in   ax, dx              ; Store original port value ...
         mov  ax, bx              ; ... in BX
         mov  ax, 5555h           ; load AX with value to write to port.
         out  dx, ax              ; Write the data.

         mov  cx, 10h             ; Wait for the port to respond
wait001: loop wait001

         in   ax, dx              ; Read the data back in.
         push ax                  ; Store the value on the stack for a moment
         mov  ax, bx              ; Restore the original value of the port
         out  dx, ax
         pop  ax                  ; Now restore the value we wish to check
         cmp  ax, 5555h           ; Q: is 8524 present ?
         jne  fn8514NotFound      ;   N: indicate 8514 not present.
                                  ;   Y: 8514 is present, now check monitor.
         ;
         ; Now we need to determine what type of monitor is attached to the
         ; 8514 card. To do this we check ID bits 0,1,2 in the subsystem
         ; status register. Depending on the Monitor attached we return:
         ;
         ; 8503 Display = VGAMonoDisplay
         ; 8512 Display = VGAColorDisplay
         ; 8513 Display = VGAColorDisplay
         ; 8514 Display = Eighty514Display
         ;
         mov  dx,SUBSYS_STAT      ; Now, we have the adapter. Check monitor.
         in   ax,dx               ; Get word from SUBSYS_STAT
         test ax,0040h            ; Check Bit 2.
         jz   Disp_8514           ; Bit 2 == 0 = 8514 Display = GAD_8514.
         test ax,0020h            ; Bit 1 == 0 = 8503 Display = VGA_MONO.
         jz   Disp_8503

         mov  ah,VGAColorDisplay  ; AH := Display type
         jmp  short fn8514Found

Disp_8503:
         mov  ah,VGAMonoDisplay   ; AH := Display type
         jmp  short fn8514Found

Disp_8514:
         mov  ah,Eighty514Display ; AH := Display type
         jmp  short fn8514Found

fn8514Found:
         mov  al,Eighty514        ; AL := subystem type
         call FoundDevice

; reset flags for subsystems that have been ruled out

         mov  byte ptr CGAflag,FALSE
         mov  byte ptr EGAflag,FALSE
         jmp  short fn8514Done

fn8514NotFound:

; Set the flag for subsystem yet to be checked

         mov  byte ptr XGAflag,TRUE

fn8514Done:
         ret

Find8514        ENDP

;
; FindVGA
;
;       This subroutine uses INT 10H function 1Ah to determine the video
;        BIOS Display Combination Code (DCC) for each video subsystem
;        present.
;

FindVGA         PROC    near

                mov     ax,1A00h
                int     10h             ; call video BIOS for info

                cmp     al,1Ah
                jne     L13             ; exit if function not supported
                                        ;  (i.e., no MCGA or VGA in system)

; convert BIOS DCCs into specific subsystems & displays

                mov     cx,bx
                xor     bh,bh           ; BX := DCC for active subsystem
                or      ch,ch
                jz      L11             ; jump if only one subsystem
                                        ;  present

                mov     bl,ch           ; BX := inactive DCC
                add     bx,bx
                mov     ax,[bx+offset DGROUP:DCCtable]

                mov     es:Device1,ax

                mov     bl,cl
                xor     bh,bh           ; BX := active DCC

L11:            add     bx,bx
                mov     ax,[bx+offset DGROUP:DCCtable]

                mov     es:Device0,ax

; reset flags for subsystems that have been ruled out

                mov     byte ptr CGAflag,FALSE
                mov     byte ptr EGAflag,FALSE
                mov     byte ptr Monoflag,FALSE

; set flag for subsystem that may be possible

                mov     byte ptr Eighty514Flag,TRUE

                push    ds                 ; Save DS -- Needed for large model
                push    es                 ; Put ES into DS -- see above
                pop     ds
                lea     bx,Video0Type[di]  ; if the BIOS reported an
                                           ;  MDA...
                cmp     byte ptr [bx],MDA
                pop     ds                 ; Restore DS -- Needed for large model
                je      L12

                push    ds                 ; Save DS -- Needed for large model
                push    es                 ; Put ES into DS -- see above
                pop     ds
                lea     bx,Video1Type[di]
                cmp     byte ptr [bx],MDA
                pop     ds                 ; Restore DS -- Needed for large model
                jne     L13

L12:            mov     word ptr [bx],0    ; ... Hercules can't be ruled
                                           ;      out
                mov     byte ptr Monoflag,TRUE

L13:            ret



FindVGA         ENDP


;
; FindEGA
;
; Look for an EGA.  This is done by making a call to an EGA BIOS function
;  which doesn't exist in the default (MDA, CGA) BIOS.

FindEGA         PROC    near            ; Caller:       AH = flags
                                        ; Returns:      AH = flags
                                        ;               Video0Type and
                                        ;                Display0Type
                                        ;                updated

                mov     bl,10h          ; BL := 10h (return EGA info)
                mov     ah,12h          ; AH := INT 10H function number
                int     10h             ; call EGA BIOS for info
                                        ; if EGA BIOS is present,
                                        ;  BL <> 10H
                                        ;  CL = switch setting
                cmp     bl,10h
                je      L22             ; jump if EGA BIOS not present

                mov     al,cl
                shr     al,1            ; AL := switches/2
                mov     bx,offset DGROUP:EGADisplays
                xlat                    ; determine display type from
                                        ;  switches
                mov     ah,al           ; AH := display type
                mov     al,EGA          ; AL := subystem type
                call    FoundDevice

                cmp     ah,MDADisplay
                je      L21             ; jump if EGA has a monochrome
                                        ;  display

                mov     CGAflag,FALSE   ; no CGA if EGA has color display
                jmp     short L22

L21:            mov     Monoflag,FALSE  ; EGA has a mono display, so MDA
                                        ;  and Hercules are ruled out
L22:            ret

FindEGA         ENDP


;
; FindCGA
;
;       This is done by looking for the CGA's 6845 CRTC at I/O port 3D4H.
;

FindCGA         PROC    near            ; Returns:      VIDstruct updated

                mov     dx,3D4h         ; DX := CRTC address port
                call    Find6845
                jc      L31             ; jump if not present

                mov     al,CGA
                mov     ah,CGADisplay
                call    FoundDevice

L31:            ret

FindCGA         ENDP


;
; FindMono
;
;       This is done by looking for the MDA's 6845 CRTC at I/O port 3B4H.
;       If a 6845 is found, the subroutine distinguishes between an MDA
;       and a Hercules adapter by monitoring bit 7 of the CRT Status byte.
;       This bit changes on Hercules adapters but does not change on an
;       MDA. The various Hercules adapters are identified by bits 4 through
;       6 of the CRT Status value:
;
;               000b = HGC
;               001b = HGC+
;               101b = InColor card

FindMono        PROC    near            ; Returns:      VIDstruct updated

                mov     dx,3B4h         ; DX := CRTC address port
                call    Find6845
                jc      L44             ; jump if not present

                mov     dl,0BAh         ; DX := 3BAh (status port)
                in      al,dx
                and     al,80h
                mov     ah,al           ; AH := bit 7 (vertical sync on
                                        ;        HGC)

                mov     cx,8000h        ; do this 32768 times 
L41:            in      al,dx
                and     al,80h          ; isolate bit 7
                cmp     ah,al
                loope   L41             ; wait for bit 7 to change

                jne     L42             ; if bit 7 changed, it's a Hercules

                mov     al,MDA          ; if bit 7 didn't change, it's an
                                        ;  MDA
                mov     ah,MDADisplay
                call    FoundDevice
                jmp     short L44

L42:            in      al,dx
                mov     dl,al           ; DL := value from status port

                mov     ah,MDADisplay   ; assume it's a monochrome display

                mov     al,HGC          ; look for an HGC
                and     dl,01110000b    ; mask off bits 4 thru 6
                jz      L43

                mov     al,HGCPlus      ; look for an HGC+
                cmp     dl,00010000b
                je      L43             ; jump if it's an HGC+

                mov     al,InColor      ; Check for InColor card
                mov     ah,EGAColorDisplay
                cmp     dl,01010000b
                je      L43             ; jump if it's an InColor card

                mov     al,HGC          ; Fall through to a HGC
                mov     ah,MDADisplay

L43:            call    FoundDevice

L44:            ret

FindMono        ENDP


;
; Find6845
;
;       This routine detects the presence of the CRTC on an MDA, CGA, or
;       HGC. The technique is to write and read register 0Fh of the chip
;       (Cursor Location Low). If the same value is read as written,
;       assume the chip is present at the specified port address.
;

Find6845        PROC    near            ; Caller:       DX = port addr
                                        ; Returns:      cf set if not
                                        ;                present
                mov     al,0Fh
                out     dx,al           ; select 6845 reg 0Fh (Cursor Low)
                inc     dx

                in      al,dx           ; AL := current Cursor Low value
                mov     ah,al           ; preserve in AH
                mov     al,66h          ; AL := arbitrary value
                out     dx,al           ; try to write to 6845

                mov     cx,100h

L51:            loop    L51             ; wait for 6845 to respond

                in      al,dx
                xchg    ah,al           ; AH := returned value
                                        ; AL := original value
                out     dx,al           ; restore original value

                cmp     ah,66h          ; test whether 6845 responded
                je      L52             ; jump if it did (cf is reset)

                stc                     ; set carry flag if no 6845 present

L52:            ret

Find6845        ENDP


;
; FindActive
;
;       This subroutine stores the currently active device as Device0.  The
;       current video mode determines which subsystem is active.
;

FindActive      PROC    near

                cmp     word ptr es:Device1,0
                je      L63                     ; exit if only one
                                                ;  subsystem

                cmp     es:Video0Type[di],4     ; exit if MCGA or VGA
                                                ;  present
                jge     L63                     ;  (INT 10H function 1AH
                cmp     es:Video1Type[di],4     ;  already did the work)
                jge     L63

                mov     ah,0Fh
                int     10h                     ; AL := current BIOS video
                                                ;        mode

                and     al,7
                cmp     al,7                    ; jump if monochrome
                je      L61                     ;  (mode 7 or 0Fh)

                cmp     es:Display0Type[di],MDADisplay
                jne     L63                     ; exit if Display0 is color
        jmp   short L62

L61:            cmp     es:Display0Type[di],MDADisplay
                je      L63                     ; exit if Display0 is
                                                ;  monochrome

L62:            mov     ax,es:Device0           ; make Device0 currently
                                                ;  active
                xchg    ax,es:Device1
                mov     es:Device0,ax

L63:            ret

FindActive      ENDP

;
; FoundDevice
;
;       This routine updates the list of subsystems.
;

FoundDevice     PROC    near               ; Caller:  AH = display #
                                           ;          AL = subsystem #
                                           ; Destroys: BX
                push    ds                 ; Save DS -- Needed for large model
                push    es                 ; Put ES into DS -- Needed for large model
                pop     ds

                lea     bx,Video0Type[di]
                cmp     byte ptr [bx],0
                je      L71                ; jump if 1st subsystem

                lea     bx,Video1Type[di]  ; must be 2nd subsystem

L71:            mov     [bx],ax            ; update list entry

                pop     ds                 ; Restore DS -- Needed for large model
                ret

FoundDevice     ENDP

_TEXT           ENDS


_DATA           SEGMENT word public 'DATA'

EGADisplays     DB      CGADisplay      ; 0000b, 0001b  (EGA switch values)
                DB      EGAColorDisplay ; 0010b, 0011b
                DB      MDADisplay      ; 0100b, 0101b
                DB      CGADisplay      ; 0110b, 0111b
                DB      EGAColorDisplay ; 1000b, 1001b
                DB      MDADisplay      ; 1010b, 1011b

DCCtable        DB      0,0             ; translate table for INT 10h func
                                        ;  1Ah
                DB      MDA,MDADisplay
                DB      CGA,CGADisplay
                DB      0,0
                DB      EGA,EGAColorDisplay
                DB      EGA,MDADisplay
                DB      0,0
                DB      VGA,VGAMonoDisplay
                DB      VGA,VGAColorDisplay
                DB      0,0
                DB      MCGA,EGAColorDisplay
                DB      MCGA,VGAMonoDisplay
                DB      MCGA,VGAColorDisplay





TestSequence    DB      ?               ; this list of flags and addresses
                DW      FindVGA         ;  determines the order in which
                                        ;  this program looks for the
Eighty514flag   DB      ?               ;  various subsystems
                DW      Find8514

XGAflag         DB      ?
                DW      FindXGA

EGAflag         DB      ?
                DW      FindEGA

CGAflag         DB      ?   
                DW      FindCGA

Monoflag        DB      ?   
                DW      FindMono

NumberOfTests   EQU     ($-TestSequence)/3

rgXgaPorts      DW      2110h           ; Port addresses for XGA adapters.
                DW      2120h
                DW      2130h
                DW      2140h
                DW      2150h
                DW      2160h
                DW      0
_DATA           ENDS

                END
