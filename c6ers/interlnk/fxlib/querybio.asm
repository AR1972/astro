INCLUDE         model.inc
INCLUDE         fastlynx.inc
    
        		.CODE	text

@FxQueryBios	PROC    USES CX SI ES
                PUBLIC  @FxQueryBios

;* void _far _fastcall FxQueryBios(struct FxBiosInfo _near *);

                int     11H
                mov     al, ah
                mov     cl, 6
                shr     ah, cl          ; Isolate # of printer ports in ah
                shr     al, 1
                and     al, 7           ; Isolate # of serial ports in AL
                cmp     al, 4
                jbe     save_counts

                mov     al, 4

save_counts:    mov     dx, ax          ; DH = # serial ports, DL = # printer
                mov     ax, 40H
                mov     es, ax
                xor     si, si
                xor     cx, cx

serial_loop:    cmp     cl, dl
                jae     setup_parallel

                mov     ax, es:[si]
                or      ax, ax
                jz      setup_parallel

                mov     [bx + si].FxBiosInfo.serial_address, ax
                inc     cl
                add     si, 2
                jmp     serial_loop

setup_parallel: xor     si, si

parallel_loop:  cmp     ch, dh
                jae     store_nums

                mov     ax, es:[si + 8]
                or      ax, ax
                jz      store_nums
                
                mov     [bx + si].FxBiosInfo.parallel_address, ax
                inc     ch
                add     si, 2
                jmp     parallel_loop

store_nums:     mov     word ptr [bx].FxBiosInfo.num_serial, cx
                ret

@FxQueryBios    ENDP

	            END
