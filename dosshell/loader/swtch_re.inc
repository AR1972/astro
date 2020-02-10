;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

MAX_NUM_PROGRAMS equ 16
MAX_PROGRAM_LENGTH equ 80
MAX_TITLE  equ 30

; DONOT CHANGE THE SIGNIFICANCE OF THE BIT FLAGS WITHOUT MAKING CORRESPONDING
; CHANGES IN THE SWITCHER INCLUDE FILE WOAPIF.INC

F_NO_SWITCH        equ 0000000000000001b ;program can not be switched away from
F_GRAPHICS         equ 0000000000000010b ;graphics mode
F_NO_PAUSE         equ 0000000000000100b ;Pause after exit
;F_DTS_API_INITED   equ 0000000000001000b ;1=>The DTS init has been posted;set by shell
;F_SWAP_NOT_OK      equ 0000000000010000b ;1=>external tasker incompatible;set by shell
F_UNUSED_3         equ 0000000010000000b ;
F_UNUSED_4         equ 0000000000100000b ;
F_SHELL            equ 0000000001000000b ;program is the shell

F_NO_ALT_TAB       equ 0000000100000000b ;ALT+TAB to be ignored
F_NO_ALT_ESC       equ 0000001000000000b ;ALT+ESC to be ignored
F_NO_CTRL_ESC      equ 0000010000000000b ;CTRL+ESC to be ignored
F_UNUSED_5         equ 0000100000000000b ;
F_UNUSED_6         equ 0001000000000000b ;
F_UNUSED_7         equ 0010000000000000b ;
F_UNUSED_8         equ 0100000000000000b ;
F_FREE             equ 1000000000000000b ;free program slot

; equates for global flags

GF_INT_6F_TOBE_DONE     equ     00000001b ;INT 6f to be done
GF_IRQ9_GLOBAL          equ     00000010b ;IRQ 9 to be globally handled
GF_NET_ASYNC_SWITCH_OK  equ     00000100b ;OK to switch out on async net request

GF_DTS_API_INITED   equ 0000000000001000b ;1=>The DTS init has been posted;set by shell
GF_SWAP_NOT_OK      equ 0000000000010000b ;1=>external tasker incompatible;set by shell

;GF_UNUSED_1             equ     00001000b ;
;GF_UNUSED_2             equ     00010000b ;
GF_UNUSED_3             equ     00100000b ;
GF_UNUSED_4             equ     01000000b ;
GF_UNUSED_5             equ     10000000b ;

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

;; THIS IS DECLARED IN LOADER.ASM NOW gresSwitchInfo db SIZE Switch_Info DUP(?)

GlobalSwitcherId dw ?

GET_DATA proc
;;;BUG BUG WHY WAS THIS    mov ax,CSEG
    mov ax,cs
    mov ds,ax
    ret
GET_DATA endp

;;;
;;; INPUT
;;; ds:di -> Switch_Info struct
;;; bl = index to the program array
;;; OUTPUT
;;;  ds:si -> bl'th program entry
;;;  ax is toasted
DoGet_Ith_Program_Entry proc
;;get bl'th guy in array -> si
     mov al,SIZE Switch_Entry
     mul bl    ; ax now holds offset in program list of first entry
     lea si,[di].Program_List
     add si,ax  ; ds:si bl'th first entry in list
     ret
DoGet_Ith_Program_Entry endp

;;;
;;; INPUT
;;; ds:di -> Switch_Info struct
;;; bl = index to the program in the linked list
;;; OUTPUT
;;;  ds:si -> bl'th program entry
;;;  ax,cx are toasted
Get_Ith_List_Program_Entry proc
;;get bl'th guy in list -> si
      xor ch,ch
      mov cl,bl
      mov bl,[di].First_In_List
      call DoGet_Ith_Program_Entry
      or cx,cx
      jz got_ith
walk_link:
      mov bl,[si].Next_In_List
      call DoGet_Ith_Program_Entry
      loop walk_link
got_ith:
     ret
Get_Ith_List_Program_Entry endp

PUBLIC _ToTop_Ith_List_Program_Entry
;;;
;;; INPUT
;;; ds:di -> Switch_Info struct
;;; bl = index to the program in the linked list
;;; OUTPUT
;;;  es:si -> bl'th program entry and that is moved to the top
;;;  ax,dx is toasted
_ToTop_Ith_List_Program_Entry proc 

      push bx                           ;save logical node number
      or   bl,bl                        ;is it already at top ?
      jz   totop_exit                   ;yes, not much to do 

      cmp  bl,[di].Num_Programs         ;last in the list ?
      jnz  neither_first_nor_last       ;no.
      dec  bl                           ;previous node
      call Get_Ith_List_Program_Entry   ;DS:SI -> previous node.
      mov  al,[si].Next_In_List         ;node # of node we wanna move to top
      mov  [di].First_In_List,al        ;new top node all other links OK
      xor  bl,bl                        ;need pointer to top node
      jmp  short totop_exit             ;done

neither_first_nor_last:

      push bx                           ;save
      mov  bl,[di].Num_Programs         ;last node in list
      call Get_Ith_List_Program_Entry   ;DS:SI -> last node in the list
      mov  dx,si                        ;save pointer
      pop  bx                           ;restore
      push dx                           ;save pointer to last node

; get a pointer to the desired node.

      push bx                           ;save
      call Get_Ith_List_Program_Entry   ;DS:SI -> node we want to move to top
      pop  bx                           ;get back node number
      mov  al,[si].Next_In_List         ;get the node following it

; link the previous node to the next node.

      push si                           ;save the pointer to the node.
      push ax                           ;al has pointer to next node
      dec  bl                           ;prev node, can't be < 0
      call Get_Ith_List_Program_Entry   ;ptr to prev node
      pop  ax                           ;get back saved ptr to next node
      mov  ah,[si].Next_In_List         ;get the physical node num of this node
      mov  [si].Next_In_List,al         ;link prev to next, skiping this node

; make the desired node the head of the list.

      mov  al,ah                        ;save desired nodes physical index
      xchg [di].First_In_List,ah        ;point start to this node

; chain the desired node to the original top of the list

      pop  si                           ;get back ptr to this node
      mov  [si].Next_In_List,ah         ;chain to the original top node

; make last node point to the desired node.

      pop  si                           ;was pushed into DX, ptr to last node
      mov  [si].Next_In_List,al         ;last points to new first

; need pointer to top node.

      xor  bl,bl                        ;now the node is at top

totop_exit:

      call Get_Ith_List_Program_Entry   ;get back a pointer to the top node
      pop  bx                           ;restore original node number
      mov  dx,ds                        ;get the ptr to the node in dx:ax
      mov  ax,si
      ret

_ToTop_Ith_List_Program_Entry endp


PUBLIC _GET_ITH_PROGRAM_STRING
;;; INPUT
;;;     ds:di -> Switch_Info struct
;;; OUTPUT
;;;     es:si -> Program string of Ith list entry
_GET_ITH_PROGRAM_STRING proc
      call Get_Ith_List_Program_Entry
      lea si,[si].Program_Name
      push ds
      pop es
     ret
_GET_ITH_PROGRAM_STRING endp


PUBLIC _GET_FREE_ENTRY
; returns in bl index of free entry, -1 if none
_GET_FREE_ENTRY proc
     mov cx,MAX_NUM_PROGRAMS-1
continue_find_free:
     mov bl,cl
     call DoGet_Ith_Program_Entry
     test [si].Program_Flags,F_FREE
     jne found_one
     loop continue_find_free
no_free:
     mov bl,-1
     stc
     ret
found_one:
     clc
     ret
_GET_FREE_ENTRY endp

PUBLIC _ADD_PROGRAM_TO_LIST
;;; INPUT
;;;     ds:di -> Switch_Info struct
;;;     es:bp-> program name (first char for now!)
;;;
_ADD_PROGRAM_TO_LIST proc

     call _GET_FREE_ENTRY
     jnc  @f
     jmp  too_many_programs
@@:
     inc [di].Num_Programs
     mov cl,[di].First_In_List
     call DoGet_Ith_Program_Entry
;;; si -> new entry
     mov [si].Next_In_List,cl 
     mov [si].Program_Flags,0

     ;set various other variables that are essential
     mov [si].Conv_Req,0                ;0k conventional memory needed
     mov [si].Xms_Req,0                 ;XMS memory needed
     mov [si].Xms_Want,0                ;XMS memory desired

     mov [si].HK_Scan_Code_1,0          ;1st directed hot key scan code
     mov [si].HK_Scan_Code_2,0          ;2nd directed hot key scan code
     mov [si].HK_Shift_State,0          ;directed hot key shift state


     push si
     xchg si,bp
     xchg bp,di
     mov ax,es
     mov dx,ds
     mov ds,ax
     mov es,dx
     mov cx,MAX_PROGRAM_LENGTH
     cld
     rep movsb
     mov  ds,dx
     mov  es,ax
     mov  di,bp
     pop  si
  
;; copy the file name part of the program name as the title.

     push di
     lea  di,[si].Program_Title
     lea  si,[si].Program_Name
     mov  dx,si

; get to the end of the string first.

@@:
	lodsb
	or      al,al
	jnz     @b
	dec     si
	dec     si
;
; now walk back till '\', ':' or dx

@@:
	cmp     si,dx
	jbe     pt_2
	mov     al,[si]
	cmp     al,':'
	je      pt_1
	cmp     al,'\'
	je      pt_1
	dec     si
	jmp     @b
pt_1:
ifdef DBCS
	call	CheckDBCSTailByte	; start-DX, current-SI
	jnz	@f			; if this is not DBCS tail byte
	dec	si
	jmp	short @b		; search next
@@:
endif
	inc     si
pt_2:

; now copy the title over.

@@:
	lodsb   
	mov     [di],al
	inc     di
	inc     cx
	or      al,al
	jnz     @b
	pop     di

     mov [di].First_In_List,bl

     ; set that last guy to point to front
     push bx
     mov bl, [di].Num_Programs
     call Get_Ith_List_Program_Entry
     pop dx
     mov [si].Next_In_List,dl

     clc
     ret
too_many_programs:
     ret
_ADD_PROGRAM_TO_LIST endp

ifdef DBCS	; ### if DBCS ###
;
;	Test if the character is DBCS Lead Byte
;
;	input:	AL = character to check
;	outpit:	ZF = 1 if DBCS Lead Byte
;

DBCSLeadByteTable	dd	0

IsDBCSLeadByte		proc	near
	push	ax
	push	si
	push	ds
	lds	si,cs:DBCSLeadByteTable
	cmp	word ptr cs:DBCSLeadByteTable+2,0
	jnz	idlb_check		; if table is already set
	push	ax
	mov	ax,6300h
	int	21h			; get DBCS lead byte table
	pop	ax
	mov	word ptr cs:DBCSLeadByteTable,si
	mov	word ptr cs:DBCSLeadByteTable+2,ds
idlb_check:
	cmp	word ptr [si],0
	jz	idlb_not		; if end of table
	cmp	al,[si]
	jb	idlb_next		; if below low value
	cmp	al,[si+1]
	jbe	idlb_yes		; if below high value
idlb_next:
	add	si,2			; do next
	jmp	short idlb_check
idlb_not:
	or	al,1			; reset ZF
	jmp	short idlb_end
idlb_yes:
	and	al,0			; set ZF
idlb_end:
	pop	ds
	pop	si
	pop	ax
	ret
IsDBCSLeadByte		endp

;
;	Check if the character position is at Tail Byte of DBCS
;
;	input:	ds:dx = start address of the string
;		ds:si = character position to check
;	output:	ZF = 1 if at Tail Byte
;
CheckDBCSTailByte	proc	near
	push	ax
	push	cx
	push	si
	mov	cx,si			; save character position
cdtb_check:
	cmp	si,dx
	jz	cdtb_next		; if at the top
	dec	si			; go back
	mov	si,[bx]			; get character
	call	IsDBCSLeadByte
	jz	cdtb_check		; if DBCS lead byte do next
	inc	si			; adjust
cdtb_next:
	sub	cx,si			; if the length is odd then
	xor	cl,1			; the character position is
	test	cl,1			; at the tail byte
	pop	si
	pop	cx
	pop	ax
	ret
CheckDBCSTailByte	endp
endif		; ### end if DBCS ###


PUBLIC _GO_Z_NEXT
;;; Reorder the program list such that the next Z order program is
;;; brought to front and the Z order is maintained.
;;; ie first in list is second and second is first
;;; INPUT
;;;     ds:di -> Switch_Info struct
;;; OUTPUT
;;;     NO CY
;;;     ds:di -> Switch_Info struct
;;;     CY
;;;         Less than two programs in list
;;;     ax,bx,cx,dx are toasted
_GO_Z_NEXT proc
	cmp [di].Num_Programs,1
	jb  outofhere
	mov bl,[di].First_In_List
	call DoGet_Ith_Program_Entry
;HEAD -> first->Second->Third
	mov bl,[si].Next_In_List ;
	mov al,SIZE Switch_Entry
	mul bl   ;ax now holds offset in program list of second entry
	lea bp,[di].Program_List
	add bp,ax  ; ds:bp holds second entry in list
	xchg si,bp
	mov cl,[si].Next_In_List
	xchg si,bp
	mov dl,[si].Next_In_List
	mov [si].Next_In_List,cl
;HEAD -> SI=First->third ; BP=Second->third
	mov al,[di].First_In_List
	xchg si,bp
	mov [si].Next_In_List,al
	xchg si,bp
;HEAD -> SI=First->third ; BP=Second->First
	mov [di].First_In_List,dl
	clc
;HEAD=Second -> SI=First->third ;
	;; set that last guy to point to front
	mov bl, [di].Num_Programs
	push dx
	call Get_Ith_List_Program_Entry
	pop dx
	mov [si].Next_In_List,dl
outofhere:
	ret
_GO_Z_NEXT endp


PUBLIC _DELETE_PROGRAM_FROM_LIST
;;; INPUT
;;;     ds:di -> Switch_Info struct
;;;     bl is which element in list order
;;;
_DELETE_PROGRAM_FROM_LIST proc
    or bl,bl
    jz special
    dec bl
dodelete:
    call Get_Ith_List_Program_Entry
    mov bl,[si].Next_In_List
    mov bp,si
    call DoGet_Ith_Program_Entry
    mov dl,[si].Next_In_List
    mov [si].Program_Flags,F_FREE
    mov word ptr [si].Program_Id,0
    mov byte ptr [si].Path_Id,0
    mov si,bp
    mov [si].Next_In_List,dl
    dec [di].Num_Programs
    ret
special:
    call _GO_Z_NEXT
    mov bl,0
    jmp short dodelete
_DELETE_PROGRAM_FROM_LIST endp

PUBLIC _GO_Z_PREV
;;; Reorder the program list such that the previous Z order program is
;;; brought to front and the Z order is maintained.
;;; ie last goes to first
;;; INPUT
;;;     ds:di -> Switch_Info struct
;;; OUTPUT
;;;     NO CY
;;;     ds:di -> Switch_Info struct
;;;     CY
;;;         Less than two programs in list
;;;     ax,bx,cx,dx are toasted
_GO_Z_PREV proc
    mov bl,[di].Num_Programs
    cmp bl,1
    jb prevout
    dec bl
    mov dl,[di].First_In_List
    call Get_Ith_List_Program_Entry
    mov bl,[si].Next_In_List
    mov [di].First_In_List,bl
    ;;call DoGet_Ith_Program_Entry
    ;;mov [si].Next_In_List,dl
    clc
prevout:
    ret
_GO_Z_PREV endp

PUBLIC _GO_NEXT
;;; Reorder the program list such that the previous Z order program is
;;; brought to front and the Z order is maintained.
;;; ie last goes to first
;;; INPUT
;;;     ds:di -> Switch_Info struct
;;; OUTPUT
;;;     NO CY
;;;     ds:di -> Switch_Info struct
;;;     CY
;;;         Less than two programs in list
;;;     ax,bx,cx,dx are toasted
_GO_NEXT proc
       mov cl,[di].Num_Programs
       cmp cl,1
       jb nextout
       mov bl,[di].First_In_List
       call DoGet_Ith_Program_Entry
       mov al,[si].Next_In_List
       mov [di].First_In_List,al
nextout:
    ret
_GO_NEXT endp

PUBLIC _GET_LIST_LENGTH
;;; INPUT
;;;     ds:di -> Switch_Info struct
;;; OUTPUT
;;;;    ax = number of items in list (zero based,  -1 == 0)
_GET_LIST_LENGTH proc
       xor ax,ax
       mov al,[di].Num_Programs
       ret
_GET_LIST_LENGTH endp

PUBLIC _GET_GLOBAL_SWITCH_DATA
;;; INPUT
;;;     ds:di -> Switch_Info struct
;;; OUTPUT
;;;    dx:ax -> global data structure
_GET_GLOBAL_SWITCH_DATA proc
       ;;; This was set-up on entry. Swap ds and get out
       mov dx,ds
       mov ax,di        
       ret
_GET_GLOBAL_SWITCH_DATA endp

PUBLIC _GET_ITH_ENTRY_DATA
_GET_ITH_ENTRY_DATA proc
      call Get_Ith_List_Program_Entry
      push ds
      pop es
     ret
_GET_ITH_ENTRY_DATA endp

PUBLIC _ADD_PARAMS
;;; INPUT
;;;     ds:di -> Switch_Info struct
;;;     es:bp -> command line parameters
;;;     bl = command line length
;;; OUTPUT
;;;
_ADD_PARAMS proc
       mov [di].Parameters,bl
       inc BYTE PTR [di].Parameters
       mov [di].Parameters+1,' '
       xor cx,cx
       mov cl,bl
       push es
       push ds
       pop  es
       pop  ds
       lea di,[di].Parameters+2
       mov si,bp
       cld
       rep movsb
       mov WORD PTR es:[di],0Dh
       ret
_ADD_PARAMS endp

PUBLIC _ADD_PARAMS
;;; INPUT
;;;     ds:di -> Switch_Info struct
;;; OUTPUT
;;;     ax    -> exit code
_GET_EXITCODE proc
       mov ax,[di].Exit_Code
       ret
_GET_EXITCODE endp
PUBLIC _GET_EXITCODE


csShell    db 'MS-DOS Shell',0
;;;           db 67 dup (0) 
;csGrabName db 'd:\vgacolor.gr2',0
;csSwap1Path db 'd:\',0
;csSwap2Path db 'c:\',0

PUBLIC _INIT_PROGRAM_LIST
;;; INPUT
;;;     ds:di -> Switch_Info struct
_INIT_PROGRAM_LIST proc

     ;;mov [di].Commands,1     ; NULL command
     ;;bug bug mov [di].Commands,0     ; NULL command
     mov [di].Num_Programs,-1 ; No Programs

;; copy the grabber name 

;     push       es
;     push       ds
;     push       ds
;     pop        es
;     push       cs
;     pop        ds
;     push       di
;     add        di,Grabber_Name
;     mov        cx,80
;     lea        si,csGrabName
;@@:
;     lodsb
;     stosb
;     or         al,al
;     jnz        @b
;
;     pop        di
;     push       di
;     add        di,Swap_Path1
;     mov        cx,80
;     lea        si,csSwap1Path
;@@:
;     lodsb
;     stosb
;     or         al,al
;     jnz        @b
;
;     pop        di
;     push       di
;     add        di,Swap_Path2
;     mov        cx,80
;     lea        si,csSwap2Path
;@@:
;     lodsb
;     stosb
;     or         al,al
;     jnz        @b
;
;     pop        di
;     pop        ds
;     pop        es
     mov        [di].Id_Serial,0
     mov        [di].Int_15_Users_Id,0
     mov        [di].Num_Lines,25
     mov        [di].Global_Flags,0
     mov        [di].Screen_Back,70h
     mov        [di].Title_Fore,0
     mov        [di].Title_Back,30h
     mov        [di].Num_Lines,25 
     ;;;  BUG BUG this following assumes we get called only after dts inited!
     mov        [di].Global_Flags,GF_IRQ9_GLOBAL OR GF_DTS_API_INITED
;     mov        [di].SFT_Size,53        ;; SFT size (!bug !bug)
;     mov        [di].CPU_Type,0         ;; (!bug !bug)
;     mov        [di].Min_Path1,50       
;     mov        [di].Min_Path2,150      
     mov        [di].XMS_Handle,0
     mov        word ptr [di][0].XMS_Size,0
     mov        word ptr [di][2].XMS_Size,0


     ;;; mark all entries as freee
     mov cx,MAX_NUM_PROGRAMS-1
initnext:
     mov bx,cx
     call DoGet_Ith_Program_Entry
     mov [si].Program_Flags,F_FREE
     mov [si].Program_Id,0
     mov [si].Path_Id,0
     loop initnext
     mov [di].First_In_List,0; First program is zero'th

; now add the shell to the list.

     push es
     push bp
     push cs
     pop  es
     lea  bp,csShell
     call _ADD_PROGRAM_TO_LIST
     pop  es
     pop  bp

; get the top program and set the F_SHELL flag

     mov bl,[di].First_In_List
     call DoGet_Ith_Program_Entry
     or  [si].Program_Flags,F_SHELL

     ret
_INIT_PROGRAM_LIST endp




chain_int2f DD  ?

Switcher_Hook proc
    push ds
    push cs

;;; hook int2f so we can handle switcher stuff
    pop ds
    mov ax,352Fh                 ; save interrupt 2f
    int 21h
    mov WORD PTR chain_int2f,bx
    mov ax,es
    mov WORD PTR chain_int2f[2],ax
    mov dx,offset handle_lists          ;patch interrupt 2f with
    mov ax,252fh                        ;our own routine
    int 21h

;;;  DTS API indicates that we must get a unique ID for our switcher
;;;  If no-one is around to answer, we have to ourselves become the
;;;  Id handler
    mov ax,4b03h
    xor bx,bx 
    mov es,bx
    mov di,bx
    mov cx,0D055h 
    int 2fh ;; allocate switcher id
    or bx,bx
    jnz Idsalreadyhandled
    ;;; we have to make ourselves a new id, and handle IDs from now on
    mov BYTE PTR cs:gresSwitchInfo.Switcher_Id,1  ;; we are ID 1
    mov cs:GlobalSwitcherId,1
    jmp short doneswitcherhooks
idsalreadyhandled:
    mov cs:gresSwitchInfo.Switcher_Id,bl
    mov cs:GlobalSwitcherId,0
doneswitcherhooks:
    pop ds
    ret
Switcher_Hook endp

Switcher_UnHook proc
    push ds
    push cs
;;; first we de-allocate our switcher_id, if we have one
    cmp GlobalSwitcherID,0
    jnz noIDfree  ;; we were handling IDs
    push es
    push di
    push bx
    mov ax,4b04h ;; Free_Switcher_ID
    xor bx,bx
    mov es,bx
    mov di,bx
    mov bl,cs:gresSwitchInfo.Switcher_Id
    int 2fh    ;; do free_switcher id
    pop bx
    pop di
    pop es
noIDfree:
    pop ds
    lds dx, chain_int2f
    mov  ax, 252fh
    int  21h         ; Restore old int 2f handler
    pop ds
    ret
Switcher_UnHook endp

CINIT_PROGRAM_LIST        equ 0
CADD_PROGRAM_TO_LIST      equ 1
CGO_Z_NEXT                equ 2
CGO_Z_PREV                equ 3
CDELETE_PROGRAM_FROM_LIST equ 4
CGO_NEXT                  equ 5
CGET_ITH_PROGRAM_STRING   equ 6
CGET_LIST_LENGTH          equ 7
CGET_GLOBAL_SWITCH_DATA   equ 8
CGET_ITH_ENTRY_DATA       equ 9
CADD_PARAMS               equ 10
CGET_EXITCODE             equ 11
CTOTOP_ITH_LIST_PE        equ 12
MAX_HANDLER_CALL          equ 12

;extrn _INIT_PROGRAM_LIST:NEAR

TaskListHandler LABEL WORD
	DW          _INIT_PROGRAM_LIST
	DW          _ADD_PROGRAM_TO_LIST
	DW          _GO_Z_NEXT
	DW          _GO_Z_PREV
	DW          _DELETE_PROGRAM_FROM_LIST
	DW          _GO_NEXT
	DW          _GET_ITH_PROGRAM_STRING
	DW          _GET_LIST_LENGTH
	DW          _GET_GLOBAL_SWITCH_DATA
	DW          _GET_ITH_ENTRY_DATA
	DW          _ADD_PARAMS
	DW          _GET_EXITCODE
	DW          _TOTOP_ITH_LIST_PROGRAM_ENTRY
handle_lists proc far
    cmp ax,4a05h ; MULTI_PURPOSE multmultallocchma+1
    je  do_handleit
    cmp ax,4b03h ; Dos Switcher API alloc ID
    je  alloc_id
    cmp ax,4b04h  ; Dos Switcher API free ID
    je  free_id
chain_on:
    jmp DWORD PTR[chain_int2f]
do_handleit:
    cmp si,MAX_HANDLER_CALL
    ja  chain_on
    push bx
    push cx
    push ds
    push bp

    call GET_DATA
    lea di,gresSwitchInfo
    shl si,1
    call cs:TaskListHandler[si]

    ;;; ds:di returns gresSwitchInfo
    ;;; es:bp returns far string

    pop bp
    pop ds
    pop cx
    pop bx
    iret
alloc_id:
    cmp cs:GlobalSwitcherId,0
    je  chain_on  ;;; we arent' handling them
    call Alloc_Switcher_Id
    jmp short APIExit
free_id:
    cmp cs:GlobalSwitcherId,0
    je  chain_on  ;;; we arent' handling them
    call Free_Switcher_Id
APIexit:
    xor ax,ax  ; carry cleared
    iret        
       
handle_lists endp

;;; INPUT 
;;;    none
;;; OUTPUT bx = new id (or zero if none allocated)
;;; AX modified
Alloc_Switcher_Id proc near
    push dx

    mov ax,GlobalSwitcherId
    cmp ax,0FFFFh       ;; are there any free ids?
    je  no_free_ids     ;; no, error out

    mov dx,1 
    mov bx,dx  ;; bx is the id counter; 1 is our ID!
next_id:

    shl dx,1    ;;NOTE we do this up front because we are ID 1
    inc bx      ;;NOTE we do this up front because we are ID 1
    test ax,dx
    jz do_alloc_id
    jmp short next_id
do_alloc_id:
    or ax,dx
    mov cs:GlobalSwitcherId,ax
    ;;; bx holds return ID
done_alloc_id:
    pop dx
    ret       
no_free_ids:
    xor bx,bx
jmp short done_alloc_id         
Alloc_Switcher_Id endp

;;; INPUT 
;;;     bx ID to free
;;; OUTPUT
;;;     bx zero if ID freed
;;;     bx -1 if ID not freed
;;; Ax modified    
Free_Switcher_Id proc near
    push dx
    cmp bx,1   ;; id 1 is us!
    jbe invalid_id
    
    cmp bx,16
    ja  invalid_id ;; only 16 valid ids

    mov ax,GlobalSwitcherId
    mov dx,1
skip_id:
    dec bx     ;; up front since we know that bx is not 1 (our id)
    jz  dofree_id
    shl dx,1
    jmp short skip_id
dofree_id:
    test ax,dx
    jz invalid_id ;; this is was not set!
 
    not dx      ;; make a mask
    and ax,dx
    xor bx,bx   
done_free:
    pop dx
    ret
invalid_id:
    mov bx,-1
    jmp short done_free
Free_Switcher_Id endp
