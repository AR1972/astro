.386p

LINE_STATUS    EQU  5

MICROSOFT_OEM_ID    EQU  0
SEWELL_OEM_ID       EQU  (298 SHL 5)
INTERLNK_ID         EQU  0

VILD_Device_ID EQU  SEWELL_OEM_ID OR INTERLNK_ID

INCLUDE VMM.INC
INCLUDE VTD.INC
INCLUDE VCD.INC
INCLUDE VSD.INC

Declare_Virtual_Device VILD, 0, 72, VILD_Control, VILD_Device_ID, \
		       Undefined_Init_Order, VILD_V86_API_Handler, 

VxD_DATA_SEG

old_priority_flags  dd  ?

can_use_flag_addr   dd  0

VxD_DATA_ENDS

VxD_LOCKED_CODE_SEG

;******************************************************************************
;
;   VILD_Control
;
;   DESCRIPTION:    dispatch control messages to the correct handlers
;
;   ENTRY:
;
;   EXIT:           Carry clear if no error
;
;   USES:
;
;==============================================================================

BeginProc VILD_Control

    cmp     eax, Sys_Critical_Exit
    jne     SHORT VILD_Ctrl_Exit

    mov     ecx, [can_use_flag_addr]
    jecxz   VILD_Ctrl_Exit

    mov     BYTE PTR [ecx], 0

VILD_Ctrl_Exit:
    clc
    ret

EndProc VILD_Control

VxD_LOCKED_CODE_ENDS

VxD_CODE_SEG

;******************************************************************************
;
;   VILD_V86_API_Handler
;

BeginProc VILD_V86_API_Handler

    pushfd
    push    eax
    mov     al, [ebp.Client_al]

    cmp     al, 0
    je      short Serial_Idle

    cmp     al, 1
    je      Set_VMStat_High_Pri_Back

    cmp     al, 2
    je      Reset_VMStat_High_Pri_Back

    cmp     al, 5
    je      short Parallel_Idle

    cmp     al, -1
    je      short Set_OK_Flag


    pop     eax
    popfd
    ret

EndProc VILD_V86_API_Handler


BeginProc Set_OK_Flag

    push    edi   

;
; This should be called only once by only one user, but we'll check
; to make sure.  If a second user is calling this then turn off the
; first one's flag.
;
    mov     edi, [can_use_flag_addr]
    test    edi, edi
    jz      short SOF_Not_Totally_Stupid
    mov     BYTE PTR [edi], 0
SOF_Not_Totally_Stupid:

    movzx   eax, [ebp.Client_DS]
    shl     eax, 4
    movzx   edi, [ebp.Client_DI]
    add     edi, eax
    mov     BYTE PTR [edi], 1
    mov     [can_use_flag_addr], edi

    pop     edi
    pop     eax
    popfd
    ret

 
EndProc Set_OK_Flag


BeginProc Serial_Idle
    
    push    edx
    mov     dx, [ebp.Client_dx]
    add     dl, LINE_STATUS
    in      al, dx
    sub     dl, LINE_STATUS
    test    al, 20h
    jz      short send_byte_fail

    mov     al, [ebp.Client_bl]
    out     dx, al

send_byte_fail:
    add     dl, LINE_STATUS
    in      al, dx
    sub     dl, LINE_STATUS
    test    al, 1
    jz      short set_error

    in      al, dx
    mov     [ebp.Client_al], al
    and     [ebp.Client_flags], 0FFFEh  ; clear carry
    jmp     short return

set_error:    
    or      [ebp.Client_flags], 1       ; set carry

return:
    pop     edx
    pop     eax
    popfd
    ret

EndProc Serial_Idle
    

BeginProc Parallel_Idle
    
    push    edx
    mov     dx, [ebp.Client_dx]
    mov     al, [ebp.Client_bl]
    out     dx, al
    inc     dx
    in      al, dx
    mov     [ebp.Client_al], al
    pop     edx
    pop     eax
    popfd
    ret

EndProc Parallel_Idle
    

BeginProc Set_VMStat_High_Pri_Back

    push    ecx
    push    edx
    push    esi
    VMMcall Get_Time_Slice_Priority
    mov     [old_priority_flags], eax
    or      eax, VMStat_High_Pri_Back
    VMMcall Set_Time_Slice_Priority
    pop     esi
    pop     edx
    pop     ecx
    pop     eax
    popfd
    ret

EndProc Set_VMStat_High_Pri_Back

BeginProc Reset_VMStat_High_Pri_Back

    push    ecx
    push    edx
    push    esi
    VMMcall Get_Time_Slice_Priority
    mov     eax, [old_priority_flags]
    VMMcall Set_Time_Slice_Priority
    pop     esi
    pop     edx
    pop     ecx
    pop     eax
    popfd
    ret

EndProc Reset_VMStat_High_Pri_Back

VxD_CODE_ENDS

end
