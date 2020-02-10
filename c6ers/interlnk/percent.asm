;***
;* $Workfile:   percent.asm  $
;* $Revision:   1.0  $
;*   $Author:   Dave Sewell  $
;*     $Date:   13 Oct 1989 11:28:00  $
;*
;*****************************************************************************

%               .MODEL memmodel

	IF	@CodeSize
X	EQU	6
	ELSE
X	EQU	4
	ENDIF
                .CODE

;extern int _fastcall percent(long current, long total);
;

@percent        PROC
                PUBLIC  @percent

                push    bp
                mov     bp, sp
                mov     bx, [BP + X]        ;Low order word of total
                mov     cx, [BP + X + 2]    ;High order word of total

;Now DX:AX has current, CX:BX has total

                cmp     dx, cx
                ja      one_hundred

scale_loop:     or      cx, cx
                jz      calculate

                shr     dx, 1
                rcr     ax, 1
                shr     cx, 1
                rcr     bx, 1
                jmp     scale_loop

calculate:      cmp     ax, bx
                jae     one_hundred

                mov     dx, 100
                mul     dx              ;Now DX:AX = scaled current * 100
                cmp     bx, dx
                jbe     zero

                div     bx
                jmp     short percent_done

zero:           xor     ax, ax
                jmp     short percent_done

one_hundred:    mov     ax, 100

percent_done:   pop     bp
                ret     4               ;Pop off the stack arguments!!!

@percent        ENDP

            END
