; dummy file for dev/emm386
;
ifndef INC_LIST
.xlist
endif
;
; allocmem.asm defines
;
UNLOCK_ROM	= 0	
LOCK_ROM = 0
;
; modeset.asm defines
;
DISABLE_NMI = 0
NMI_CMD = 0
ENABLE_NMI = 0
KYBD_CMD = 0
KYBD_DATA = 0
NULL_8042_CMD = 0
KYBD_STATUS = 0
IN_BUF_FULL = 0
;
; except.asm defines
;
PPI = 0
PPO = 0
PPO_MASK_IOCHECK = 0
PPI_IOCHECK = 0
PPO_MASK_PCHECK = 0
PPI_PCHECK = 0
;
; int15.asm defines
;
LIMIT = 0
PORT386S = 0
NMI386S = 0
;
; errhndlr.asm defines
;
MASTER = 0
SLAVE = 0
DIS_MSTSLV = 0
BOOT_KEY = 0
CONTINUE_KEY = 0
ENTER_KEY = 0
;
; a20trap.asm defines
;
KYBD_RD_OUTPUT = 0
KYBD_WR_OUTPUT = 0
ENABLE_A20 = 0
KYBD_RD_ECHO = 0
PS2_PORTA = 0
;
.list
