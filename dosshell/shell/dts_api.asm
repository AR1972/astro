;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

; ZZZZZZ Are there bugs with offset calculations?? Is DGROUP the right
; assume for DS? I am unable to access variable DoQuickCompare from 'C'
; The offset is off by 64 bytes!!

?WIN = 0                ;Not windows;
?PLM = 1                ;DO use pl/m
include cmacros.inc

;;;BUG BUG should share .inc file with others!

MAX_NUM_PROGRAMS equ 16
MAX_PROGRAM_LENGTH equ 80
MAX_TITLE  equ 30

F_DTS_API_INITED   equ 0000000000001000b ;1=>The DTS init has been posted;set by shell
F_SWAP_NOT_OK      equ 0000000000010000b ;1=>external tasker incompatible;set by shell
extrn _C_GET_GLOBAL_SWITCH_DATA:FAR

Switch_Entry   STRUC
       Program_Name    db MAX_PROGRAM_LENGTH  DUP(?); Program name
       Program_Title   db MAX_TITLE DUP (?)      ; switcher screen title
       Conv_Req        dw ?                      ; min conv mem required
       XMS_Req         dw ?                      ; XMS required
       XMS_Want        dw ?                      ; XMS desired
       HK_Scan_Code_1  db ?                      ; 1st directed hot key scan code
       HK_Scan_Code_2  db ?                      ; 2nd directed hot key scan code
       HK_Shift_State  db ?                      ; ALT/CTRL/SHIFT states
       Program_Flags   dw ?                      ; Special flags for program
       Next_In_List    db ?                      ; Next program in Z order list
       Program_Id      dw ?                      ; internal program ID
       Path_Id         db ?                      ; internal id of swap path
       Shell_Cookie    dw ?                      ; magic cookie for shell
Switch_Entry   ENDS


Switch_Info    STRUC
       Switcher_Id     db   ?                    ; low nibble is unique id
       CPU_Type        dw   ?                    ; CPU type flag bits
       SFT_Size        dw   ?                    ; SFT SIZE
       Parameters      db   130 dup (?)          ; parameters to the program
       Grabber_Name    db   80 dup (?)           ; grabber path and name
       Swap_Path1      db   68 dup (?)           ; first swap drive and path 
       Swap_Path2      db   68 dup (?)           ; second swap drive & path
       Min_Path1       dw   ?                    ; min k space to be left on drv1
       Min_Path2       dw   ?                    ; min k space to be left on drv2
       XMS_Handle      dw   ?                    ; Handle of locked XMS block
       XMS_Size        dd   ?                    ; size of block in bytes
       Int_15_Users_Id dw   ?                    ; ID of app using INT 15 memory
       Id_Serial       dw   ?                    ; running app serial number
       Exit_Code       dw   ?                    ; switcher exit code
       Num_Lines       db   ?                    ; start up screen lines
       Global_Flags    db   ?                    ; various global descisions
       Screen_Back     db   ?                    ; back ground screen color
       Title_Fore      db   ?                    ; Title text color
       Title_Back      db   ?                    ; Title back ground color
       Num_Programs    db   ?                    ; number of programs in the list
       First_In_List   db   ?                    ; first program in Z order list
       Program_List    db SIZE Switch_Entry *MAX_NUM_PROGRAMS DUP(?)
Switch_Info    ENDS


sBegin data
sEnd data

sBegin code
    assumes cs, code
    assumes ds, DGROUP

Switch_Call_Back STRUC
	SCBI_Next       dd      ?
	SCBI_Entry_Pt   dd      ?
	SCBI_Reserved   dd      ?
	SCBI_API_Ptr    dd      ?
Switch_Call_Back ENDS

Parent_Switcher_CallBack dd 0
do_resume_on_exit db 0
Switcher_Call_Entry dd ?
Switcher_Call_Chain dd ?

;;; Call Out to DTS task switchers usint the DTS API
;;; 
;;; 
;;; local statics
inDTSax dw  ?
inDTSbx dw  ?
inDTScx dw  ?
inDTSdx dw  ?
DTS_API proc near
	mov inDTSax,ax
	mov inDTSbx,bx
	mov inDTScx,cx
	mov inDTSdx,dx

	mov ax,4b01h
	xor bx,bx
	mov es,bx
	mov cx,bx  ; note that the call in address is NULL! BUG BUG
	mov dx,bx
	int 2fh
	;;; returns in es:bx of a switch_call_back_info data block
chain_loop:
	mov ax,es
	or  ax,bx
	jz  chain_done
	mov ax,WORD PTR ES:[BX].SCBI_Next[0]
	mov WORD PTR Switcher_Call_Chain[0],ax
	mov ax,WORD PTR ES:[BX].SCBI_Next[2]
	mov WORD PTR Switcher_Call_Chain[2],ax
	mov ax,WORD PTR ES:[BX].SCBI_Entry_Pt[0]
	mov WORD PTR Switcher_Call_Entry[0],ax
	mov ax,WORD PTR ES:[BX].SCBI_Entry_Pt[2]
	mov WORD PTR Switcher_Call_Entry[2],ax
	
	;;; set call-in handler to NULL
	xor di,di
	mov es,di
	mov ax,inDTSax
	mov bx,inDTSbx
	mov cx,inDTScx
	mov dx,inDTSdx
	mov si,0D055h
	call DWORD PTR [Switcher_Call_Entry]
	or ax,ax
	jnz  chain_done
	mov  bx,WORD PTR Switcher_Call_Chain[0]
	mov  es,WORD PTR Switcher_Call_Chain[2]
	jmp chain_loop
chain_done:
	ret
DTS_API endp


cProc DTS_API_Exit, PUBLIC <si,di,ds,es>
cBegin DTS_API_Exit
	;;; If we suspended the parent switcher, we should resume him!
	cmp cs:do_resume_on_exit,1
	jne dont_resume
	mov ax,3        ;; resume switcher
	xor di,di       ;; note NULL callback
	mov es,di       
	mov cx,0D055h
	call DWORD PTR [Parent_Switcher_CallBack]
dont_resume:

	call _C_GET_GLOBAL_SWITCH_DATA
	;; dx:ax -> global switcher data
	mov es,dx
	mov bx,ax
	mov bl,es:[bx].Global_Flags
	test bl,F_DTS_API_INITED
	jz doneDTS_API_Exit
	
	mov ax,7        ;;Exit Switcher
	mov bx,1        ;;BUG BUG this should see if we are the only switcher
	call DTS_API    

doneDTS_API_Exit:
cEnd DTS_API_Exit

;;; Returns 1 if DosTaskSwitcher API handlers said its okay to switcher
;;;        -1    if no DTS handler responded
;;;         0 if DTS handlers refused us switching
cProc DTS_API_Init, PUBLIC <si,di,ds,es>
cBegin DTS_API_Init

	;;; Only do DTS initialization on first launch!
	call _C_GET_GLOBAL_SWITCH_DATA
	;; dx:ax -> global switcher data
	mov es,dx
	mov bx,ax
	mov bl,es:[bx].Global_Flags
	test bl,F_DTS_API_INITED
	jnz     DTS_already_inited


	mov     ax,4B02h        ; Detect_Switcher
	xor     bx,bx           ; bx is defined to be zero in spec DTS API 0.6
	mov     es,bx           ; es:di = 0
	mov     di,bx
	mov     cx,0D055h       ; signature for DOS5 just in case (not doc'd!)
	int     2fh
	or      ax,ax
	jnz     no_DTS_Responder

	;;; we are running under a DTS API switcher
	;;; es:di = call in address of parent switcher 
	;;; Now we are supposed to attempt to suspend parent switcher
	;;; It can refuse if it wishes. However, we are supposed to ignore
	;;; the error it gives anyway, just in case it decides we should
	;;; run our switcher anyway; it will let us know when we Init_Switcher
	mov ax,2
	mov WORD PTR Parent_Switcher_CallBack,di
	mov WORD PTR Parent_Switcher_CallBack[2],es
	xor di,di
	mov es,di
	mov cx,0D055h
	;;; we do a direct call to the parent_switcher on this call...
	call DWORD PTR [Parent_Switcher_CallBack]
	;;; so spec 0.6 is a bit confusing. AX=1 advises us not to run, so
	;;; be it
	cmp ax,1
	je  DTS_SaidNo
	;;; if ax is zero, we have suspended the parent; so we should
	;;; resume him on exit!
	or  ax,ax
	jnz parent_not_suspended
	mov cs:do_resume_on_exit,1
parent_not_suspended:
No_DTS_Responder:
	;;; otherwise we might be able to run our switcher, so now Init_Switcher
	;;; this needs to be done with the DTS APIs chain method so all
	;;; switchers will have a say
	xor ax,ax
	call DTS_API
	or ax,ax
	jnz DTS_SaidNO
	jmp short DTS_SaidOK
DTS_Already_Inited:
	;;;WARNING assumes bl is flags from above!
	test bl,F_SWAP_NOT_OK
	jnz DTS_SaidNOalready
	jmp short DTS_SaidOKalready
	
;No_DTS_Responder:
;
;       call _C_GET_GLOBAL_SWITCH_DATA
;       ;; dx:ax -> global switcher data
;       mov es,dx
;       mov bx,ax
;       or  BYTE PTR es:[bx].Global_Flags,F_DTS_API_INITED
;       mov ax,-1
;       jmp short doneDTS
DTS_SaidNO: 
	call _C_GET_GLOBAL_SWITCH_DATA
	;; dx:ax -> global switcher data
	mov es,dx
	mov bx,ax
	or  BYTE PTR es:[bx].Global_Flags,F_SWAP_NOT_OK OR F_DTS_API_INITED
DTS_SaidNOalready:      
	xor ax,ax
	jmp short doneDTS
DTS_SaidOK:
	call _C_GET_GLOBAL_SWITCH_DATA
	;; dx:ax -> global switcher data
	mov es,dx
	mov bx,ax
	and  BYTE PTR es:[bx].Global_Flags,NOT F_SWAP_NOT_OK
	or  BYTE PTR es:[bx].Global_Flags,F_DTS_API_INITED

DTS_SaidOKalready:
	mov ax,1
doneDTS:
cEnd DTS_API_Init

 
;;INPUTS none
;;OUTPUTS 1 if running under tasker; 0 otherwise
;;      (if 1, don't allow enabling of task switcher)
;;
cProc RunningUnderMStasker, PUBLIC <si,di,ds,es>
cBegin RunningUnderMStasker
	call DTS_API_Init
	or ax,ax
	jz already_tasking
	cmp ax,1
	je say_OK
;;;detection code for older MS taskers (<=W3.0)
	mov     ax,4680h ;switcher call out
	int     2fh
	or      ax,ax
	jz      already_tasking
	mov     ax,1600h ;enhanced mode callout
	int     2fh
	test    al,7fh   ;if not low bits set 
			 ;there is a random himem which might set hi bit
	;or      al,al
	jnz already_tasking
say_OK:
	xor ax,ax
tasktestdone:

cEnd RunningUnderMStasker
already_tasking:
	mov     ax,1
	jmp short tasktestdone


sEnd   code

end
