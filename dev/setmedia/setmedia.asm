;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

; 
;This device driver will hook onto interrupt 13 if there is a 1.44Meg
;floppy drive in the system.  Any call to Set Media Type (function ah=18h)
;with ch=80,cl=09 (720k) on a 1.44Meg drive will be monitored.  
;If the real int 13 call fails, the hook code will
;return appropriate return values (including a pointer to a default
;disk parameter table) and set no carry (no error)
;
;


_TEXT segment byte public 'CODE'
	assume cs:_TEXT,ds:_TEXT,es:NOTHING

	org     0

Header:
	dd      -1                      ;device chain link--filled in by dos
DevAttr dw      0C840h                  ;character device attribute word
	dw      Strategy                ;Strategy entry point
	dw      Interrupt               ;Interrupt entry point
	db      '_stmedia'              ;logical device name

RHPtr   dd      ?                       ;Request Header pointer filled in by 
					;Strategy routing

Strategy        proc    far

	mov     WORD PTR cs:[RHPtr],bx
	mov     WORD PTR cs:[RHPTR+2],es
	ret

Strategy        endp

Interrupt       proc    far
	push    ds
	push    es
	push    di
	push    bx
	push    dx

	push    cs
	pop     ds

	les     di,DWORD PTR cs:[RHPtr] ;load Request Header pointer into es:di
	mov     bl,es:[di+2]            ;get command code from RHPtr into bl

	or      bl,bl                   ;We only handle command zero (Init)
	jnz     error_out

	;;;Initialize Driver does all driver initialization. It is 
	;;;located in the transient Code/Data
	;;;Returns driver tail in es:di
	
	mov     word ptr es:[di+3],0100h ;Completed code passed in packet
	call    Initialize_Driver

Return_To_Dos:
	pop     dx
	pop     bx
	pop     di
	pop     es
	pop     ds
	ret

error_out:
	mov     word ptr es:[di+3],8003h ;bad command code in request header
	jmp     short Return_To_Dos

Interrupt       endp

;;; RESIDENT DATA AREA anything here is not freed after initialization

Int_13_Chain    dd      ?


MAX_FLOPPY_DRIVES equ   5               ;maximum number of floppys we support

;;; The following is a default drive parameter for a 720k floppy
Parameter_Table_For_720k_Floppy:
	Specify1        db      0DFh
	Specify2        db      002h
	MotorWait       db      025h
	SecSize         db      002h      
	EOT             db      009h
	RWGap           db      01Bh
	DTL             db      0FFh
	FmtGap          db      050h
	Fill            db      0F6h
	HdSettle        db      00Fh
	MotorStrt       db      004h
	Res1            db      000h
	Res2            db      000h

Floppy_Drive_Info db (MAX_FLOPPY_DRIVES+1) dup (0FFh)


;;;
;;; Int_13_Hook monitors int 13 calls for Set Media Type and
;;; ensures success of that call
;;;
Int_13_Hook     proc    far

	cmp     ah,18h                          ;Set Media Type?
	je      Catch_Set_Media_Type            ;If so, monitor call
Call_Int_13_Chain:                              ;else
	jmp     DWORD PTR cs:[Int_13_Chain]     ;chain to next handler

Catch_Set_Media_Type:
	cmp     ch,79                           ;Are we setting media
	jne     Call_Int_13_Chain               ;to 720k floppy?
	cmp     cl,09h
	jne     Call_Int_13_Chain
	test    dl,80h                          ;ensure it is a floppy..
	jnz     Call_Int_13_Chain

	push bx                                 ;See if this is a 1.44M drive
	xor bx,bx                               ;by walking the table we
Check_Next_Table_Info:                          ;build at startup
	cmp cs:Floppy_Drive_Info[bx],dl         ;drive in table?
	je  Is_In_Table                         ;if so, monitor call
	inc bl        
	cmp bx,MAX_FLOPPY_DRIVES                ;don't pass end of table!
	jb  Check_Next_Table_Info       
	;;; if we get here, the drive was not in our table
Not_In_Table:
	pop bx                                  ;restore stack 

	jmp     short   Call_Int_13_Chain       ;chain on to handler

Is_In_Table:                                    ;Monitor the call...
	pop bx

	pushf                                   ;by setting iret frame
	call    DWORD PTR cs:[Int_13_Chain]     ;and calling next handler

	jnc     CLC_IRET                        ;don't hack if no error!

Use_Our_Table:                                  ;"real" didn't work, so fake it
	xor     ah,ah                           ;set no error 
	push    cs                              ;set return in es:di
	pop     es                              ;to point to valid dp table
	mov     di,OFFSET cs:Parameter_Table_For_720k_Floppy

CLC_IRET:
	push    bp                              ;be sure caller gets no carry
	mov     bp,sp                           ;by modifying the flag
	and     WORD PTR ss:[bp+6],0FFFEh       ;on the stack
	pop     bp                              ;thanks to bp
	iret                                    ;return no carry to caller
Int_13_Hook     endp


;
;       WARNING!        Transient Code/Data begins here
;                       All code and data after this point
;                       is freed after initialization call zero
;
SignOn:  
;        db      0Dh,0Ah
;        db      'Int 13 Set Media Type Fix Driver'
;        db      0Dh,0Ah,'$'

Initialize_Driver proc near

	push    es                      ;save pointer to Request Header
	push    di                      ;

	;;;Don't put up a sign-on banner, as it just clutters up the screen.
	;mov     ah,09h                  ;Print out a the sign on message
	;mov     dx,offset       SignOn
	;int     21h

	;;;Now we determine what drives need our fix. Return value
	;;;in BX is number of drives needing the fix. Zero means
	;;;there are no drives that need monitoring, so we don't need to load
	call    Setup_Floppy_Drive_Info_Table 
	or      bx,bx                   ; don't load if table is empty
	jz      Dont_load

	;;;     If we get here, we do need to monitor some floppy drive

	call    Hook_ISR_13             ;Hook int 13 interrrupt routine
	pop     di                      ;restore Request Header pointer
	pop     es                      ;into es:di

	mov     WORD PTR es:[di+0Eh],offset SignOn ;resident up to Signon
	mov     WORD PTR es:[di+10h],cs            ;the rest is free'd
	
	xor     ax,ax                   ;return no error
	jmp     short   return_resident_init
Dont_Load:
	;;;     If we get here, we don't need to load at all

	pop     di                      ;restore Request Header pointer
	pop     es

	mov     WORD PTR es:[di+0Eh],offset Header ;return pointer to header
	mov     WORD PTR es:[di+10h],cs            ;to indicate don't load

	mov     ax,7FFFh                           ;also clear high bit
	and     WORD PTR cs:DevAttr,ax             ;of attribute word
						   ;so next driver will load
						   ;on top of us

	xor     ax,ax                              ;return no error
return_resident_init:
	ret
Initialize_Driver endp

;;;     Returns zero in BX if table is empty
;;;
Setup_Floppy_Drive_Info_Table proc near

	push    es              ;save necessary but smashed registers
	push    bp
	push    di
	push    si
	push    cx

	xor     dx,dx           ;dx is drive id starting at zero
	mov     bx,dx           ;bx is table index starting at zero
	
Check_Next_Floppy_Id:
	mov     bp,bx           ;save off table index into bp
	mov     si,dx           ;save off current drive id into si

	mov     ah,08h          ;Get Drive Parameters
	int     13h             ;
	jc      Not_A_144MFloppy_Drive
	;;; Mohanraj says don't trust bl return,check actual size
	; cmp     bl,04h
	cmp     cx,4f12h
	jne     Not_A_144MFloppy_Drive

	mov     bx,bp           ;restore table index
	mov     dx,si           ;restore drive id
	mov     BYTE PTR Floppy_Drive_Info[bx],dl  ; save drive id in table
	

	inc     bx              ;set up for next table index

	cmp     dl,7Fh          ;7F is last possible floppy drive id
	jae     Done_Setting_Up

	inc     dl              ;set up for next drive id
	 
	cmp     bx,MAX_FLOPPY_DRIVES   ;don't go past the end of the table!                          
	jae     Done_Setting_Up

	jmp     short Check_Next_Floppy_Id

Not_A_144MFloppy_drive:
	mov     bx,bp           ;restore table index
	mov     dx,si           ;restore drive id

	cmp     dl,7Fh          ;7F is last floppy id, dont go further!
	jae     Done_Setting_Up
	inc     dl              
	jmp     short Check_Next_Floppy_Id

Done_Setting_Up:
	pop     cx              ;restore registers we smashed
	pop     si
	pop     di
	pop     bp
	pop     es
	ret
Setup_Floppy_Drive_Info_Table endp


Hook_ISR_13     proc    near

	    push es
	    push di
	    
	    mov ax,3513h                 ; save interrupt 13 vector
	    int 21h                      ; returns in es:bx

	    mov WORD PTR cs:Int_13_Chain,bx
	    mov ax,es
	    mov WORD PTR cs:Int_13_Chain[2],ax

	    mov dx,offset Int_13_Hook    ;patch interrupt 13 vector with
	    mov ax,2513h                 ;our own routine
					 ;assumes ds already holds code segment
	    int 21h

	    pop di
	    pop es
	    ret

Hook_ISR_13     endp
_TEXT ends

end
