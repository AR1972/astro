        dosseg
        .model large

        public          is386
        public          _GetSuperXmsInfo

.code
;
;       Identify the current CPU being executed.
;       Return with EAX=1 for i486 CPU or EAX=0 for 386 CPU.
;       Leave ESP, EBP, EBX, ESI, and EDI unchanged.
;
is386           PROC FAR
.386
        mov     edx,esp         ; Save current stack pointer to align it
        and     esp,not 3       ; Align stack to avoid AC fault
        pushfd                  ; Push EFLAGS
        pop     eax             ; Get EFLAGS value
        mov     ecx,eax         ; Save original EFLAGS
        xor     eax, 40000H     ; Flip AC bit in EFLAGS
        push    EAX             ; Copy to EFLAGS
        popfd
        pushfd                  ; Get new EFLAGS value
        pop     eax             ; Put into eax
        xor     eax,ecx         ; See if AC bit changed
                                ; EAX=0H if 386 CPU, 40000H if i486 CPU
        shr     eax,18          ; EAX=0H if 386 CPU, 1H if i486 CPU
        and     eax,1           ; Ignore all other bits
        push    ecx
        popfd                   ; Restore original EFLAGS register
        mov     esp,edx         ; Restore original stack pointer
        ret
is386           endp



;
;       Determine the Super Extended Memory amounts
;
_GetSuperXmsInfo   PROC FAR


        push    bp              ; Setup code for being called from 'C'
        mov     bp,sp

        mov     ah,88H          ; Call for super XMS info

        call Dword Ptr [bp + 6] ; Call the XMS driver.

        or      bl,bl           ; Check for error condition
        jnz     gsxi_error_exit ; Jump out if there's an error

        mov     bx, [bp + 10]        ; Address of where to put the answer
        mov     Dword Ptr [bx], eax  ; Largest free
        mov     bx, [bp + 12]        ; Address of where to put the answer
        mov     Dword Ptr [bx], edx  ; Total free

; Some day, the highest address may be useful/accurate, but not today ...
;       mov     bx, [bp + 14]        ; Address of where to put the answer
;       mov     Dword Ptr [bx], ecx  ; Highest address

        jmp short gsxi_exit     ; Information is retrieved, exit

gsxi_error_exit:
        mov     ax,1            ; Set an error code
        jmp short gsxi_ret      ; and return

gsxi_exit:
        xor     ax,ax           ; No error

gsxi_ret:
        pop     bp              ; Clean up and return
        ret

_GetSuperXmsInfo   endp

        END
