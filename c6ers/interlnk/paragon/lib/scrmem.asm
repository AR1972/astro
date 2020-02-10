;***
;* $Workfile:   scrmem.asm  $
;* $Revision:   1.3  $
;*   $Author:   Dave Sewell  $
;*     $Date:   11 Sep 1990  8:46:08  $
;***

%               .MODEL memmodel, PASCAL

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

BIOS_SEG    ENDS

                .DATA?
                PUBLIC  HerculesInColor ; Only referenced in initscr.asm
                PUBLIC  _desqview
                PUBLIC  __attrib
                PUBLIC  _cursor_column
                PUBLIC  _cursor_row
                PUBLIC  _cursor_location
                PUBLIC  _cursor_value

                PUBLIC  is_mono

HerculesInColor DB      ?               ; Non-zero if InColor card present
_desqview       DB      ?
is_mono         DB      ?

_cursor_location    LABEL   WORD
_cursor_column  DB      ?
_cursor_row     DB      ?

_cursor_value   DW      ?

__attrib        DW      ?

                .DATA
                EXTRN   _display_segment:WORD
                EXTRN   _display_offset:WORD
                EXTRN   _retrace_wait:BYTE
                EXTRN   _force_mono:BYTE
                EXTRN   _on_cursor_value:WORD
                EXTRN   _off_cursor_value:WORD

old_video_mode  DB      0FFH

            IF  @CodeSize
                .CODE   PARAGON_TEXT
            ELSE
                .CODE
            ENDIF

topview         DB      ?               ;Put this in code segment for easy ref.

                PUBLIC  fix_attribute

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

TopviewUpdate   PROC    NEAR

                cmp     cs:topview, 0
                jz      @F

                push    ax
                push    bp
                push    di
                push    si
                mov     ah, 0FFH
                int     10H
                pop     si
                pop     di
                pop     bp
                pop     ax

@@:             ret

TopviewUpdate   ENDP

SetTopView      PROC    FAR

                mov     topview, al
                ret

SetTopView      ENDP

update_cursor   PROC    NEAR    USES AX DX

;Enter with DX = cursor position, returns DI = offset

                mov     al, 160
                mul     dh
                shl     dl, 1
                xor     dh, dh
                add     ax, dx
                mov     di, ax
                add     di, _display_offset
                ret

update_cursor   ENDP

locate          PROC    USES BP DI SI, row_col:WORD
;***
;* locate -- locate cursor
;*
;* extern void pascal locate(int row_col);
;*
;* int row;         desired screen row (0 - 24)
;* int column;      screen column (0 - 79)
;***

                mov     dx, row_col
                mov     WORD PTR _cursor_column, dx
                MOV     AH, 2               ;locate cursor bios function
                XOR     BH, BH              ;page zero

                INT     10H

                ret

locate          ENDP

set_cursor      PROC    NEAR    USES BP DI SI

                mov     _cursor_value, cx
                mov     ah, 1
                int     10h
                ret

set_cursor      ENDP

cursor_on       PROC    
;***
;* cursor_on -- turn on cursor
;*
;* extern void pascal cursor_on(void);
;***

                mov     cx, _on_cursor_value
                call    set_cursor
                ret

cursor_on       ENDP

cursor_off      PROC    USES BP DI SI
;***
;* cursor_off -- turn off cursor
;*
;* extern void pascal cursor_off(void);
;***

                mov     cx, _off_cursor_value
                call    set_cursor
                ret

cursor_off      ENDP

restore_cursor  PROC    value:WORD
;***
;* restore_cursor -- restore cursor to specified value
;*
;* extern void pascal restore_cursor(unsigned value);
;***

                mov     cx, value
                call    set_cursor
                ret

restore_cursor  ENDP

set_attribute   PROC    attr:WORD
;
; extern void pascal set_attribute(int attr);

                mov     ax, attr
                mov     __attrib, ax
                ret

set_attribute   ENDP

dispmem         PROC    USES    DI SI DS, pos:WORD, buff:FAR PTR, cnt:WORD
;
; extern void pascal dispmem(int pos, byte far *buff, int cnt);

                mov     ax, __attrib
                call    fix_attribute       ;Fix attribute if force_mono set
                mov     dx, pos
                call    update_cursor       ;Now DI has screen offset
                mov     bx, _display_segment
                mov     es, bx
                mov     cx, cnt
                mov     bl, _retrace_wait   ;Save retrace wait flag
                lds     si, buff

                mov     dx, MOTOROLA_6845   ;Address of 6845 status register
                jcxz    dispmem_ret         ;Insure we have something

                or      bl, bl              ;Need to wait for retrace?
                jnz     slow_disp           ;Yes -- Go Do it

fast_disp:      lodsb                       ;Get  a byte of text
                stosw                       ;Save out text & attribute
                loop    fast_disp           ;Go for next char.

                jmp     short dispmem_ret   ;All done

slow_disp:      lodsb                       ;Get a byte of text
                mov     bl, al              ;Save character/attribute

                RetraceWait

                mov     al, bl              ;Recover the character
                stosw                       ;Store character and attribute
                sti                         ;Enable interrupts again
                loop    slow_disp

dispmem_ret:    call    TopviewUpdate
                ret

dispmem         ENDP

clear           PROC    USES DI SI, ulpos:WORD, lrpos:WORD

;extern void pascal clear(int ulpos, int lrpos);

                mov     ax, __attrib
                call    fix_attribute
                mov     bh, ah
                mov     dx, lrpos
                mov     cx, ulpos
                xor     al, al
                mov     ah, 6

                INT     10H

                ret

clear           ENDP

scroll          PROC    USES DI SI, \
                ulpos:WORD, lrpos:WORD, attr:WORD, count:BYTE, \
                direction:WORD

;extern void pascal scroll(int ulpos, int lrpos, int attr, int count, int direction);
;
; int ulpos;    Position of upper left corner
; int lrpos;    Position of lower right corner
; int attrib;   Attribute to use for filling
; int count;    Number of lines to scroll
; int direction;    1 = down, 0 = up

                mov     cx, ulpos
                mov     dx, lrpos
                mov     ax, attr
                call    fix_attribute
                mov     bh, ah
                mov     al, count
                mov     ah, 6
                shr     BYTE PTR direction, 1
                adc     ah, 0                   ;Make it a 7 if DOWN specified
                INT     10H
                ret

scroll          ENDP

fill            PROC    USES DI SI, ulpos:WORD, height_width:WORD, fill_ch:BYTE

;extern void pascal fill(int ulpos, int height_width, int c);

                mov     ax, _display_segment
                mov     es, ax
                mov     dx, ulpos
                call    update_cursor
                mov     si, di              ;Keep destination ptr in SI
                mov     dx, height_width    ;Get height/width pair
                or      dh, dh
                jz      fill_done

                or      dl, dl
                jz      fill_done

                mov     ax, __attrib         ;Get attribute
                call    fix_attribute
                mov     al, fill_ch         ;Get character
                mov     bx, 2 * 80          ;Length of a screen row in bytes
                xor     ch, ch

fill_loop:      mov     di, si              ;Set destination pointer
                mov     cl, dl              ;Set up count
                push    di
                push    cx
                cmp     _retrace_wait, 0
                je      fast_fill

                push    bx
                push    dx
                mov     dx, MOTOROLA_6845   ;Address of 6845 status register
                mov     bl, al

slow_fill:      RetraceWait
                mov     al, bl
                stosw
                sti
                loop    slow_fill

                pop     dx
                pop     bx
                jmp     short end_row

fast_fill:      rep     stosw               ;Fill the line

end_row:        pop     cx
                pop     di
                dec     dh                  ;Decrement # of lines left
                jz      fill_done           ;All done!

                add     si, bx              ;Point to start of next line
                jmp     fill_loop           ;Go back & fill next line

fill_done:      call    TopviewUpdate
                ret

fill            ENDP

shade           PROC    USES DI SI, ulpos:WORD, height_width:WORD

; extern void pascal shade(int ulpos, int height_width);
;
; Change the attribute of a display zone
;

                mov     ax, _display_segment
                mov     es, ax
                mov     dx, ulpos
                call    update_cursor
                mov     si, di              ;Keep destination ptr in SI
                mov     dx, height_width    ;Get height/width pair
                mov     ax, __attrib         ;Get attribute
                call    fix_attribute
                mov     al, ah
                mov     bx, 2 * 80          ;Length of a screen row in bytes
                xor     ch, ch

shade_loop:     mov     di, si                  ;Set destination pointer
                mov     cl, dl                  ;Set up count
                push    di
                push    cx
                cmp     _retrace_wait, 0
                je      shade_each

                push    bx
                push    dx
                mov     dx, MOTOROLA_6845   ;Address of 6845 status register
                mov     bl, al

slow_shade:     inc     di                      ;Skip past char
                RetraceWait                     ;Wait for retrace
                mov     al, bl
                stosb
                sti
                loop    slow_shade

                pop     dx
                pop     bx
                jmp     short shade_next

shade_each:     inc     di                      ;Skip past char
                stosb                           ;Store attribute
                loop    shade_each              ;Do until end of line

shade_next:     pop     cx
                pop     di
                dec     dh                      ;Decrement # of lines left
                jz      shade_done              ;All done!

                add     si, bx                  ;Point to start of next line
                jmp     short shade_loop        ;Go back & fill next line

shade_done:     call    TopviewUpdate
                ret

shade           ENDP

save_zone       PROC    USES DI SI DS, \
                ulpos:WORD, height_width:WORD, buff:FAR PTR

;extern void pascal save_zone(int ulpos, int height_width, byte far *buff);

; Copy the display info in a zone into a far buffer
;
                mov     dx, ulpos
                call    update_cursor
                mov     cx, di
                mov     dx, height_width        ;Height, width to DX
                les     di, buff                ;Buffer pointer to ES:[DI]
                mov     bp, cx                  ;Screen offset goes in BP
                mov     ah, _retrace_wait       ;Get retrace flag to AH
                mov     ds, _display_segment    ;Point to screen memory
                mov     bx, 80 * 2              ;Length of 1 line in bytes
                xor     ch, ch

save_loop:      mov     si, bp                  ;Set destination pointer
                mov     cl, dl                  ;Set up count
                or      ah, ah
                jz      fast_save

                push    dx
                mov     dx, MOTOROLA_6845

slow_save:      RetraceWait
                movsw
                sti
                loop    slow_save

                pop     dx
                jmp     short save_next

fast_save:
            rep movsw                           ;Copy the line

save_next:      dec     dh                      ;Decrement # of lines left
                jz      save_done               ;All done!

                add     bp, bx                  ;Point to start of next line
                jmp     short save_loop         ;Go back & fill next line

save_done:      ret

save_zone       ENDP

restore_zone    PROC    USES DI SI DS, \
                ulpos:WORD, height_width:WORD, buff:FAR PTR

;extern void pascal restore_zone(int ulpos, int height_width, byte far *buff);

;
; Restore a display zone from a buffer
;

                mov     dx, ulpos
                call    update_cursor
                mov     cx, di                  ;Temporarily put offset in CX
                mov     dx, height_width        ;Height, width to DX
                mov     es, _display_segment    ;Point to screen memory
                mov     ah, _retrace_wait       ;Get retrace flag to AH
                lds     si, buff                ;Buffer pointer to DS:[SI]
                mov     bp, cx                  ;Screen offset goes in BP
                mov     bx, 80 * 2              ;Length of 1 line in bytes
                xor     ch, ch

rest_loop:      mov     di, bp                  ;Set destination pointer
                mov     cl, dl                  ;Set up count
                push    di
                push    cx
                or      ah, ah
                jz      fast_rest

                push    bx
                push    dx
                mov     dx, MOTOROLA_6845

slow_rest:      lodsw
                mov     bx, ax
                RetraceWait
                mov     ax, bx
                stosw
                sti
                loop    slow_rest

                pop     dx
                pop     bx
                jmp     short rest_next

fast_rest:
            rep movsw                           ;Copy the line

rest_next:      pop     cx
                pop     di
                dec     dh                      ;Decrement # of lines left
                jz      rest_done               ;All done!

                add     bp, bx                  ;Point to start of next line
                jmp     short rest_loop         ;Go back & fill next line

rest_done:      call    TopviewUpdate
                ret

restore_zone    ENDP


fix_attribute   PROC    NEAR

;Entry:
;   AL = desired attribute for color screen
;   AH may have bits set indicating monochrome adapter attributes as follows:
;       01 - reverse video
;       02 - underline
;Return:
;   AL and AH = physical screen attribute
;
                mov     __attrib, ax
                cmp     is_mono, 0
                jz      color_adapter

;--- Using B000 segment.  Is this a Hercules InColor Card?

                cmp     HerculesInColor, 0
                jne     color_adapter           ;Then leave attributes alone

                test    ah, UNDERLINE_BIT
                jnz     underline

                jmp short   other_tests

;--- Set for underline

underline:      and     al, 88H
                or      al, 1
                jmp     short fix_ret

color_adapter:  cmp     _force_mono, 0
                je      fix_ret

other_tests:    test    ah, REVERSE_BIT
                jnz     mono_reverse

                test    ah, NORMAL_BIT
                jnz     mono_normal

                test    ah, BOLD_BIT
                jnz     mono_bold

                or      al, 07H
                and     al, 8FH
                jmp     short fix_ret

mono_normal:    mov     al, 07H
                jmp short   fix_ret

mono_bold:      mov     al, 0FH
                jmp short   fix_ret

mono_reverse:   or      al, 70H
                and     al, 0F0H

fix_ret:        mov     ah, al
                ret

fix_attribute   ENDP

                END
