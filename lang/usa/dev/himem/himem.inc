; dummy file for himem.inc
;
ifndef INC_LIST
.xlist
endif
;
HISEG_ORG = 0
TDUMP = 1
KEEP_CS = 0
NUM_A20_RETRIES = 5
DEBUG_VERS = 9
COMMAND = 0
STATUS = 0
ERR_NOTIMPLEMENTED = 1 
ERR_VDISKFOUND = 2
ERR_A20 = 3
ERR_HMAINUSE = 4
ERR_HMANOTEXIST = 5
ERR_HMAMINSIZE = 6
ERR_HMANOTALLOCED = 7
HIDATA segment
HANDLE dw 0
HIDATA ends
FLAGS = 0
CLOCK = 0
BASE = 0
LEN = 0

; unsure what this macro looked like setting
; to nop will cause chaos, without a doubt
popff macro
	nop
endm

pusha macro
	push ax
	push cx
	push dx
	push bx
	push sp
	push bp
	push si
	push di
endm

popa macro
	pop di
	pop si
	pop bp
	pop sp
	pop bx
	pop dx
	pop cx
	pop ax
endm

.list
