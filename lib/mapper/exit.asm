
?PLM=1      ; PASCAL Calling convention is DEFAULT
?WIN=0      ; Windows calling convention
?386=0      ; Use 386 code?

public DOSEXIT
.xlist
include macros.inc
include cmacros.inc
.list
.286p
.MODEL small
.CODE
; =============================================================================
; Action:
; 0      EXIT_THREAD   The current thread is ended. If the current thread
;                      is the last thread, the process is ended.
; 1      EXIT_PROCESS  The process and all threads in it is ended.
; =============================================================================
DOSEXIT PROC FAR

Result = word ptr 6
Action = word ptr 8

        pushall
        mov ax, [bp+Action]
        cmp ax, 1
        jg  Done
        mov ax, [bp+Result]
        mov ah, 4Ch
        int 21h     ; DOS - 2+ - QUIT WITH EXIT CODE (EXIT)
                    ; AL = exit code
        xor ax, ax
Done:
        popall
        retf 4

DOSEXIT ENDP
END

