;
; M003 -- created this file -- moved strings here from dataseg.asm
;
; This file contains all the strings that need to be translated by
; internationalization folks. Leave the lines which have an EQU alone!
; Just work on the strings that are in single quotes.
;
;

; The following message is printed out when our exec fails. That is when
; minicmd determines that it is not an intrinsic command, it tries
; to launch it as an extrinsic one. Most likely cause is that the user
; did not specify a valid FULL path name!
;
LOAD_MSG	db	'Cannot load program'
LOAD_MSG_LEN	EQU	$ - LOAD_MSG


; The following message is printed out when there is a "goto" command in
; the initialization batch file but the label is not to be found.
;
NO_LABEL_MSG	db	'Cannot find label'
NO_LABEL_MSG_LEN EQU	$ - NO_LABEL_MSG


INIT_BAT_NAME	db	'\ROMINIT.BAT', 0	; INITIALIZATION file name!

;
; Following are the KEYWORDS used in the INTRINSIC commands!
;
SET_STR		db	'SET'
SET_STR_LEN	EQU	$ - SET_STR

ECHO_STR	db	'ECHO'
ECHO_STR_LEN	EQU	$ - ECHO_STR

IF_STR		db	'IF'
IF_STR_LEN	EQU	$ - IF_STR

GOTO_STR	db	'GOTO'
GOTO_STR_LEN	EQU	$ - GOTO_STR

NOT_STR		db	'NOT'
NOT_STR_LEN	EQU	$ - NOT_STR

ERRLVL_STR	db	'ERRORLEVEL'
ERRLVL_STR_LEN	EQU	$ - ERRLVL_STR

EXIST_STR	db	'EXIST'
EXIST_STR_LEN	EQU	$ - EXIST_STR

ON_STR		db	'ON'
ON_STR_LEN	EQU	$ - ON_STR

OFF_STR		db	'OFF'
OFF_STR_LEN	EQU	$ - OFF_STR

;
; Message strings printed out when user types ECHO\r to figure out whether
; ECHO is ON or OFF.
;
ECHOISON_STR	db	'ECHO is on'
ECHOISON_STR_LEN EQU	$ - ECHOISON_STR

ECHOISOFF_STR	db	'ECHO is off'
ECHOISOFF_STR_LEN EQU	$ - ECHOISOFF_STR

;
; These are the strings used to create the Critical Error Query of the form:
; "Abort, Retry, Fail?"
; Note that these should be terminated by a '$'!
;
ABORT_STR	db	'Abort',  '$'
RETRY_STR	db	'Retry',  '$'
IGNORE_STR	db	'Ignore', '$'
FAIL_STR	db	'Fail',   '$'
CRIT_SEP_STR	db	', ',     '$'
CRIT_Q_STR	db	'?',      '$'

; Note that these have to be terminated by '$' as I use fn 09 and also
; scan for the '$' to find the start of the next message!
;
; The various critical errror messages. Note that they are terminated by
; '$' This is used to signify the end of 1 message and the start of
; the next. Luckily, the error codes are contiguous and except for
; 13 and 14 all have valid specific messages. To save code/data size
; I decided to have the code scan for the Nth message (for error code N)
; and print it out rather than having a table of offsets to the strings!
;
; A critical error displayed is of the form:
;
; <CrMsg-N> [READ|WRIT]ING [DRIVE <drive-letter>:| DEVICE <dev-name>]
;
; or it is the FAT_ERR_MSG
;
UNKNOWN_MSG_ERR_CODE	EQU	13 ; This is the general crit. err. msg!
MAX_DRIVER_ERR_CODE	EQU	15
FIRST_CRIT_ERR_MSG	LABEL	BYTE
CrMsg0		db	'Write protect error', '$'
CrMsg1		db	'Invalid unit', '$'
CrMsg2		db	'Not ready', '$'
CrMsg3		db	'Invalid device request', '$'
CrMsg4		db	'Data error', '$'
CrMsg5		db	'Invalid device request parameters', '$'
CrMsg6		db	'Seek error', '$'
CrMsg7		db	'Invalid media type', '$'
CrMsg8		db	'Sector not found', '$'
CrMsg9		db	'Printer out of paper error', '$'
CrMsg10		db	'Write fault error', '$'
CrMsg11		db	'Read fault error', '$'
CrMsg12		db	'General failure', '$'
CrMsg13		db	'Critical error occured', '$' ; un-assigned error 
CrMsg14		db	'Critical error occured', '$' ; numbers.
CrMsg15		db	'Invalid disk change', '$'

READING_STR	db	'reading', '$'
WRITING_STR	db	'writing', '$'
DRIVE_STR	db	'drive', '$'
DEVICE_STR	db	'device', '$'

; This is a special critical error message!
FAT_ERR_MSG	db	'FAT image in memory corrupted', '$'

