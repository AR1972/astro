;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;;;
;;; This file contains C interface routines to the 
;;; global int2f switcher functions.
;;; All they do is preserve registers, set up parameters, and call out
;;;
;;; See swtch_re.inc for parameters/return values
_C_GET_LIST_LENGTH proc far
	push bp
	push ds
	push es
	push di
	push si

	mov ax,4a05h
	mov si,CGET_LIST_LENGTH
	int 2fh

	pop si
	pop di
	pop es
	pop ds
	pop bp
	retf
_C_GET_LIST_LENGTH endp
PUBLIC _C_GET_LIST_LENGTH



 _C_GET_ITH proc far
	push bp
	mov bp,sp
	push ds
	push es
	push di
	push si

	mov bx,[bp+6]

	mov ax,4a05h
	mov si,CGET_ITH_PROGRAM_STRING
	int 2fh

	mov dx,es
	mov ax,si

	pop si
	pop di
	pop es
	pop ds
	pop bp
	retf
_C_GET_ITH endp
PUBLIC _C_GET_ITH

_C_ADD_PARAMS proc far
	push bp
	mov  bp,sp
	push ds
	push es
	push di
	push si
	push ax
	push dx

	mov es,[bp+8]
	mov bx,[bp+10]
	mov bp,[bp+6]


	mov ax,4a05h
	mov si,CADD_PARAMS
	int 2fh
	pop dx
	pop ax
	pop si
	pop di
	pop es
	pop ds
	pop bp
	retf
_C_ADD_PARAMS endp
PUBLIC _C_ADD_PARAMS

_C_GET_EXITCODE proc far
	push bp
	mov  bp,sp
	push ds
	push es
	push di
	push si

	mov ax,4a05h
	mov si,CGET_EXITCODE
	int 2fh
	pop si
	pop di
	pop es
	pop ds
	pop bp
	retf
_C_GET_EXITCODE endp
PUBLIC _C_GET_EXITCODE


_C_ADD_PROGRAM proc far
	push bp
	mov  bp,sp
	push ds
	push es
	push di
	push si
	push ax
	push dx

	mov es,[bp+8]
	mov bp,[bp+6]

	mov ax,4a05h
	mov si,CADD_PROGRAM_TO_LIST
	int 2fh
	pop dx
	pop ax
	pop si
	pop di
	pop es
	pop ds
	pop bp
	retf
_C_ADD_PROGRAM endp
PUBLIC _C_ADD_PROGRAM


_C_DELETE_PROGRAM proc far
	push bp
	mov  bp,sp
	push ds
	push es
	push di
	push si

	mov bx,[bp+6]

	mov ax,4a05h
	mov si,CDELETE_PROGRAM_FROM_LIST
	int 2fh

	pop si
	pop di
	pop es
	pop ds
	pop bp
	retf
_C_DELETE_PROGRAM endp
PUBLIC _C_DELETE_PROGRAM


_C_INIT_PROGRAM_LIST proc far
	push bp
	push ds
	push si
	push di
	push es

	mov ax,4a05h
	mov si,CINIT_PROGRAM_LIST
	int 2fh

	pop es
	pop di
	pop si
	pop ds
	pop bp
     retf
_C_INIT_PROGRAM_LIST endp
PUBLIC _C_INIT_PROGRAM_LIST


_C_GO_Z_NEXT proc far
	push bp
	push ds
	push si
	push di
	push es

	mov ax,4a05h
	mov si,CGO_Z_NEXT
	int 2fh

	pop es
	pop di
	pop si
	pop ds
	pop bp
	retf
_C_GO_Z_NEXT endp
PUBLIC _C_GO_Z_NEXT

_C_GO_Z_PREV proc far
	push ds
	push si
	push di
	push es

	mov ax,4a05h
	mov si,CGO_Z_PREV
	int 2fh

	pop es
	pop di
	pop si
	pop ds
       retf
_C_GO_Z_PREV endp
PUBLIC _C_GO_Z_PREV

_C_GO_NEXT proc far
	push ds
	push si
	push di
	push es

	mov ax,4a05h
	mov si,CGO_NEXT
	int 2fh

	pop es
	pop di
	pop si
	pop ds
	retf
_C_GO_NEXT endp
PUBLIC _C_GO_NEXT


_C_GET_GLOBAL_SWITCH_DATA proc far
	push ds
	push si
	push di
	push es

	mov ax,4a05h
	mov si,CGET_GLOBAL_SWITCH_DATA
	int 2fh
	;;; out put in dx:ax
	;mov dx,es
	;mov ax,di

	pop es
	pop di
	pop si
	pop ds
	retf
_C_GET_GLOBAL_SWITCH_DATA endp
PUBLIC _C_GET_GLOBAL_SWITCH_DATA

_C_GET_ITH_ENTRY_DATA proc far
	push bp
	mov  bp,sp
	push ds
	push es
	push di
	push si

	mov bx,[bp+6]

	mov ax,4a05h
	mov si,CGET_ITH_ENTRY_DATA
	;;; output in es:di
	int 2fh
	mov dx,es
	mov ax,di

	pop si
	pop di
	pop es
	pop ds
	pop bp
	retf
_C_GET_ITH_ENTRY_DATA endp
PUBLIC _C_GET_ITH_ENTRY_DATA
