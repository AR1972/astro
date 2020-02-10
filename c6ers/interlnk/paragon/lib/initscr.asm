;***
;* $Workfile:   initscr.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   11 Sep 1990  8:46:06  $
;***

%               .MODEL MEDIUM, PASCAL

; Hercules Graphics InColor Card constants

DMC_PORT        EQU     03B8H       ; Display mode control port
STATUS_PORT     EQU     03BAH       ; Display status port
INDEX_REG       EQU     03B4H       ; 6845 index register
DATA_REG        EQU     03B5H       ; 6845 data register

XMODE_REG       EQU     14H         ; character mode option port
UNDERSCORE_REG  EQU     15H         ; register to set underscore
OVERSTRIKE_REG  EQU     16H         ; register to set overstrike
EXCEPTION_REG   EQU     17H         ; bit 0-3  cursor color
                                    ; bit 4    palette enable/disable
                                    ; bit 5    normal/alternate attributes
                                    ; bit 6-7  unused
PALETTE_REG     EQU     1CH

ID_MASK         EQU     01110000B   ; to detect the presence of
ID_CODE         EQU     01010000B   ; a GB222 (InColor card)

MOTOROLA_6845   EQU     3DAH

REVERSE_BIT     EQU     1
UNDERLINE_BIT   EQU     2
NORMAL_BIT      EQU     4
BOLD_BIT        EQU     8

BIOS_SEG    SEGMENT AT 40H

            ORG     49H
crt_mode    DB      ?
crt_cols    DW      ?
crt_len     DW      ?
crt_start   DW      ?
cursor_posn DW      ?
            
            ORG     84H
crt_rows    DB      ?

BIOS_SEG    ENDS

                .DATA?
                EXTRN   HerculesInColor:BYTE
                EXTRN   __attrib:WORD
                EXTRN   _desqview:BYTE
                EXTRN   _cursor_column:BYTE
                EXTRN   _cursor_row:BYTE
                EXTRN   _cursor_location:WORD
                EXTRN   _cursor_value:WORD
                EXTRN   _scr_rows:BYTE
                EXTRN   _scr_cols:BYTE
                EXTRN   is_mono:BYTE

topview         DB      ?
HerculesReset   DB      ?               ; Non-zero if users palette was loaded
                                        ; Zero if to reset to Default palette

                .DATA
                EXTRN   _display_segment:WORD
                EXTRN   _display_offset:WORD
                EXTRN   _retrace_wait:BYTE
                EXTRN   _force_mono:BYTE
                EXTRN   _on_cursor_value:WORD
                EXTRN   _off_cursor_value:WORD

old_video_mode  DB      0FFH

; EGA/VGA compatable palette defined below.  This area is also used for the
; buffer when reading the user desired palette from the HPAL file.
;
HerculesPalette DB      0, 1, 2, 3, 4, 5, 20, 7, 56, 57, 58, 59, 60, 61, 62, 63
                DB      00010011B       ; Palette enabled, alternate attributes
                                        ; Cursor Color set to palette index 3
                DB      01101101B       ; Underscore color = index 6, line = 13
PALETTE_LENGTH  EQU     ($ - HerculesPalette)

                EXTRN   PASCAL LoadHerculesPalette:FAR
                EXTRN   PASCAL SetTopview:FAR

                .CODE   FAR_TEXT


;*** The following macro generates code to wait for horizontal retrace
;*** intervals.  This is necessary to prevent screen snow when using the
;*** IBM color adapter.  Interrupts are disabled during the wait.
;*** DX must have the address of the Motorola 6845 status port.
;*** AL is clobbered.

RetraceWait     MACRO
                LOCAL   WaitForLow, WaitForHigh

WaitForLow:     IN      AL, DX
                RCR     AL, 1
                JC      WaitForLow

                CLI

WaitForHigh:    IN      AL, DX
                RCR     AL, 1
                JNC     WaitForHigh

                ENDM

CheckHercules   PROC    NEAR    USES ES SI

                mov     ax, 40H
                mov     es, ax
                mov     si, 6CH
                mov     HerculesInColor, 0  ;Pre-set for not Hercules
                mov     dx, STATUS_PORT     ; record state
                in      al, dx
                and     al, 80h             ; save bit 7 for test
                mov     bl, al

                mov     cx, es:[si]

@@:             in      al, dx              ; take another reading
                and     al, 80h             ; again, save bit seven
                cmp     al, bl
                jne     hgc_ok              ; if bit 7 changes, then it

                mov     ax, es:[si]
                sub     ax, cx
                cmp     ax, 2
                jb      @B                  ;No - keep looking

                jmp     SHORT exit          ; Test failed - leave flag zero

hgc_ok:         in      al, dx              ; test for GB222
                and     al, ID_MASK         ; clear all but bits 4, 5, and 6

                cmp     al, ID_CODE         ; test ID bits
                jne     exit                ; exit if failed - not a GB222

                inc     HerculesInColor     ; It is a GB222, set flag non-zero

exit:           ret

CheckHercules   ENDP


; SetHerculesPalette    loads the HerculesPalette data into the InColor palette
;                       registers, enables the palette and the alternate
;                       attribute set.

SetHerculesPalette  PROC

                mov     dx, INDEX_REG
                mov     al, PALETTE_REG ; point at GB222 palette registers
                out     dx, al
                inc     dx              ; increment to 6845 data register
                in      al, dx          ; reset the palette pointer

                mov     cx, 16          ; 16 registers in the palette
                mov     si, OFFSET HerculesPalette

@@:             lodsb                   ; load 1st 16 bytes into color index
                out     dx, al          ; registers
                loop    @B

                dec     dx              ; Set Palette, Attribute set, and cursor color
                mov     al, EXCEPTION_REG
                out     dx, al
                inc     dx
                lodsb
                out     dx, al

                dec     dx              ; Set the underscore position and color
                mov     al, UNDERSCORE_REG
                out     dx, al
                inc     dx
                lodsb
                out     dx, al

                mov     dx, DMC_PORT
                mov     al, 00001000B   ; Page zero, blink off, enable video,
                out     dx, al          ; text mode

                ret

SetHerculesPalette  ENDP

; Sets Hercules InColor card to default (hardware) settings.  This routine is
; called if the users palette could not be loaded for any reason.
;
SetHerculesDefault  PROC

                mov     dx, INDEX_REG
                mov     al, EXCEPTION_REG
                out     dx, al
                inc     dx
                mov     al, 00100000B   ; Normal attributes, palette disabled
                out     dx, al

                dec     dx
                mov     al, UNDERSCORE_REG
                out     dx, al
                inc     dx
                mov     al, 0           ; reset underscore register
                out     dx, al

                ret

SetHerculesDefault  ENDP

init_scr        PROC    FAR USES BP DI SI

                LOCAL   mode_set:WORD

;* extern int _far _pascal init_scr(void);

                mov     _scr_rows, 25
                mov     _scr_cols, 80
                xor     ax, ax
                mov     _retrace_wait, al
                mov     mode_set, ax
                mov     ah, 1AH             ;AH = 0x1A, AL = 0
                INT     10H
                cmp     al, 1AH
                jne     check_ega

                cmp     bl, 2
                jne     determine_mode

need_retrace:   inc     _retrace_wait
                jmp     short determine_mode

check_ega:      mov     ah, 12H
                mov     bl, 10H
                INT     10H
                cmp     bl, 10H
                jne     determine_mode

                INT     11H         ;Get BIOS equipment list
                and     al, 30H
                cmp     al, 30H
                jne     need_retrace

determine_mode: mov     ah, 15
                INT     10H
                mov     old_video_mode, al
                cmp     ah, 80              ; Insure we are in 80 column mode
                jne     set_mode

                cmp     al, 7
                je      check_rows

                cmp     al, 3
                jne     set_mode

check_rows:     mov     bx, 40H
                mov     es, bx
                mov     bl, es:crt_rows
                cmp     bl, 24              ; 25 line mode OK
                je      save_rows

                cmp     bl, 42              ; 43 line mode OK
                je      save_rows

                cmp     bl, 49
                je      save_rows

set_mode:       mov     ax, 3
                INT     10H
                inc     mode_set
                mov     bl, es:crt_rows
                cmp     bl, 24              ; 25 line mode OK
                je      save_rows

                cmp     bl, 42
                je      save_rows

                cmp     bl, 49
                jne     determine_seg

save_rows:      inc     bl
                mov     _scr_rows, bl

determine_seg:  mov     ah, 3
                INT     10H
                mov     _cursor_location, dx
                mov     _cursor_value, cx
                mov     ah, 15
                INT     10H
                cmp     al, 7
                je      mono_seg

color_seg:      mov     _display_segment, 0B800H
                mov     is_mono, 0
                mov     _on_cursor_value, 0607H
                mov     _off_cursor_value, 2607H
                jmp     short check_dv

mono_seg:       mov     _display_segment, 0B000H
                mov     is_mono, 1
                mov     _on_cursor_value, 0B0CH
                mov     _off_cursor_value, 2B0CH
                mov     _retrace_wait, 0

                call    CheckHercules   ; See if we are on an InColor Card
                cmp     HerculesInColor, 0
                je      check_dv

                call    SetHerculesPalette  ; Set Palette to EGA/VGA colors

    ; If using an InColor card, load (but do not set) the users default palette.
    ; Restore_screen may be called from the critical error handler.  During
    ; critical error handling the users palette could not be loaded as the load
    ; process involves DOS I/O.  However, since we load the users palette here,
    ; there is no problem calling restore_screen from the critical error
    ; handler.

                push    ds              ; Seg:offset of palette buffer
                mov     ax, OFFSET HerculesPalette
                push    ax

                mov     ax, PALETTE_LENGTH
                push    ax              ; buffer length
                call    LoadHerculesPalette
                mov     HerculesReset, al

check_dv:       mov     _desqview, 0
                mov     topview,  0
                mov     cx, 'DE'
                mov     dx, 'SQ'
                mov     ax, 2B01H
                int     21H
                cmp     al, 0FFH
                je      @F

                inc     _desqview

@@:             mov     es, _display_segment
                mov     di, _display_offset
                mov     ah, 0FEH
                INT     10H                     ;Check for TopView windowing
                mov     ax, es
                cmp     ax, _display_segment
                jne     display_change

                cmp     di, _display_offset
                je      init_done

display_change: mov     _display_segment, es
                mov     _display_offset,  di
                mov     _retrace_wait, 0
                cmp     _desqview, 0
                jne     init_done

                inc     topview                 ;Topview (needs update calls)
                mov     al, topview
                call    SetTopview

init_done:      mov     ax, mode_set
                ret

init_scr        ENDP

restore_scr     PROC    FAR USES BP DI SI, restore_mode:WORD

;* extern void _far _pascal restore_scr(int restore_mode);

                cmp     HerculesInColor, 0
                je      reset_mode

                cmp     HerculesReset, 0
                je      @F

                call    SetHerculesPalette
                jmp     short reset_mode

@@:             call    SetHerculesDefault

reset_mode:     cmp     restore_mode, 0
                je      restore_ret

                mov     al, old_video_mode
                xor     ah, ah
                INT     10H

restore_ret:    ret

restore_scr     ENDP


                END
