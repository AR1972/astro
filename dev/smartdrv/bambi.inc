MULT_BAMBI              equ     04a10h          ;int 2f multiplex interface
BAMBI_SIGNATURE         equ     0BABEh
BAMBI_GET_STATS         equ     0               
BAMBI_COMMIT_ALL        equ     1
BAMBI_REINITIALIZE      equ     2
BAMBI_CACHE_DRIVE       equ     3
BAMBI_GET_INFO          equ     4
BAMBI_GET_BUFFER_INFO   equ     5
BAMBI_DONT_CACHE_DRIVE  equ     6
BAMBI_GET_ORIGINAL_DD_HEADER equ 7

CACHE_DRIVE_GET         equ     0
CACHE_DRIVE_READ_ENABLE equ     1
CACHE_DRIVE_READ_DISABLE equ    2
CACHE_DRIVE_WRITE_ENABLE equ    3
CACHE_DRIVE_WRITE_DISABLE equ   4


ERROR_NO_XMS_DRIVER     equ     1       ;errors returned by xms.asm
ERROR_ALLOCATION_FAILEd equ     2


COMMAND_LINE_OFFSET     equ     80h     ;offset from starting segment of command line
COMMAND_LINE_TERMINATOR equ     0Dh     ;carriage return ends command line
MAX_COMMAND_LINE_LENGTH equ     80h     ;maximum length of command line is 128 chars
COMMAND_SWITCH_CHAR     equ     '/'     ;the switch indicator
ELEMENTS_FLAG           equ     'n'
ELEMENT_SIZE_FLAG       equ     'e'
FLUSH_FLAG              equ     'c'
STATUS_FLAG             equ     's'
DRIVE_FLAG              equ     'd'
BUFFER_FLAG             equ     'b'
RESET_FLAG              equ     'r'
VERBOSE_FLAG		equ	'v'
QUIET_FLAG		equ	'q'
BEEP_FLAG               equ     '!'     ;bug bug debugging
LOAD_LOW_FLAG           equ     'l'
COMMIT_FLAG             equ     'y'


SCANCODE_RETRY          equ     13h
ASCIICODE_RETRY         equ     'R'
SCANCODE_IGNORE         equ     17h
ASCIICODE_IGNORE        equ     'I'



READ_CACHE              equ     1
WRITE_CACHE             equ     2
NO_CACHE                equ     0

;constants for drvtype.asm
INVALID_TYPE    EQU 0
FLOPPY_TYPE     EQU 1
REMOTE_TYPE     EQU 2
HARDDISK_TYPE   EQU 3
RAMDRIVE_TYPE   EQU 4 
CDROM_TYPE      EQU 5
MEMORY_TYPE     EQU 6
MAGIC_TYPE      EQU 7


;constants for detect processor
CPU086088       equ     0
CPU186          equ     1       ;not detected here
CPU286          equ     2
CPU386          equ     4
CPU486ORABV     equ     8



MINIMUM_CACHE_SIZE      equ     16      ;minimum legal cache size in K (must be >= 16)


smart_win_access        struc                   ;meaning (maintained by)
	accessing_swap_file     db      ?       ;current access swap file? (windows)
	enhanced_mode_flags     db      ?       ;current write is lazy? (smartdrv,windows)
	delay_write_drive_unit  db      ?       ;unit of lazy write (smartdrv)
	max_delayed_write_size  dw      ?       ;maximum lazy write size (smartdrv)
smart_win_access        ends

THIS_WRITE_IS_LAZY      equ     1
WRITE_IS_PENDING        equ     2
ENHANCED_MODE_WIN       equ     4



Win386_Startup_Info_Struc STRUC
	SIS_Version_High        db      ?
	SIS_Version_Low         db      ?
	SIS_Next_Dev_Ptr        dd      ?
	SIS_Virt_Dev_File_Ptr   dd      ?
	SIS_Reference_Data      dd      ?
	SIS_Instance_Data_Ptr   dd      ?
Win386_Startup_Info_Struc ENDS


;BAMBI_VERSION_BCD      equ     0400h           
;BAMBI_VERSION_BCD      equ     0401h           ;7/30/92 scottq
BAMBI_VERSION_BCD       equ     0410h           ;11/11/92 scottq

;;;MAGIC DRIVE INTERFACE added 7/30/92 Scottq
MD_STAMP                equ "DM"        
MAGICDRV_2F             equ 4a11h
MAGICDRV_DETECT         equ    0
MAGICDRV_MAP            equ    1
MAGICDRV_SWAP           equ    2
MAGICDRV_SMART1         equ    3
MAGICDRV_SMART2         equ    4
