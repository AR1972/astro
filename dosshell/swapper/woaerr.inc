;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; defines all recognized error type codes.				     ;
;----------------------------------------------------------------------------;

; define errors first where windows is still active

ER_NO_MEMORY		equ	1	;no windows memory error
ER_NO_LIST_MEMORY       equ	2       ;no memory for window table list
ER_LOW_MEMORY		equ	3	;not enogh memory to run app
ER_COMM_NOLOAD		equ	4	;comm active, cannot load oldapp
ER_NO_XMS_DRV		equ	5	;XMS driver not loaded
ER_LOW_XMS_MEM		equ	6	;insufficient XMS memory


; define the barrier where windows code is there, screen is ok too!

ER_WND_OK_SCR_OK	equ	7	; < 7 proper windows active

ER_WND_SWAP_OUT		equ	8	;windows swap out error
ER_RELOCATE		equ	9	;special relocation error
ER_WND_SWAP_OUT_BAD	equ	99	;swap out err after memory destroyed

; define the second barrier where windows code is there, OEM disabled

ER_WND_OK_SCR_GONE	equ	10	;windows code there,OEM disabled


ER_GRABBER_LOAD		equ	11	;grabber load error
ER_WND_SWAP_IN		equ	12	;windows swap in error
ER_APP_SWAP_OUT		equ	13	;app swap out error
ER_APP_SWAP_IN		equ	14	;app swap in error    
ER_LIST_SWAP		equ	15	;windows table swap failure
ER_EXEC_FAILS		equ	16	;exec call fails

; define error which do not go through the generalized error handler and 
; occur when windows is still around.

ER_WINOLDAP_ACTIVE	equ	17	;winoldap active as stub
ER_BAD_INIT_DIR		equ	18	;bad initial directory.
ER_ANOTHER_SW_ACTIVE	equ	19	;another switcher is active
ER_TOO_MANY_SWITCHERS	equ	20	;too many switchers in the system
ER_APP_NOT_STARTED_YET	equ	21	;the app has not been started yet

; define error when app can't be EXECed because of CreateSession fails

ER_SWAPI_CREATE_FAILS	equ	80	;create session fails

comment ~ */

#define ER_NO_MEMORY	       1
#define ER_NO_LIST_MEMORY      2
#define ER_LOW_MEMORY	       3
#define ER_COMM_NOLOAD         4
#define ER_NO_XMS_DRV	       5
#define ER_LOW_XMS_MEM	       6
#define	ER_WND_SWAP_OUT	       8
#define ER_RELOCATE            9
#define	ER_GRABBER_LOAD	      11	
#define	ER_WND_SWAP_IN	      12
#define	ER_APP_SWAP_OUT	      13	
#define	ER_APP_SWAP_IN	      14	
#define ER_LIST_SWAP	      15
#define ER_EXEC_FAILS	      16
#define ER_WINOLDAP_ACTIVE    17
#define	ER_BAD_INIT_DIR	      18
#define ER_ANOTHER_SW_ACTIVE  19
#define ER_TOO_MANY_SWITCHERS 20
#define ER_APP_NOT_STARTED_YET 21	

#define ER_SWAPI_CREATE_FAILS 80


/* end comment ~ ;*/










			     
