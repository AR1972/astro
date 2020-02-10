; This file contains the constants used in minicmd.com.

STDERR	EQU	2
STDOUT	EQU	1
CR	EQU	0dh
LF	EQU	0ah
SPACE	EQU	' '
TAB	EQU	09h
COLON	EQU	':'
EQUALS	EQU	'='

; This is the prompt character used by the minicmd.com to prompt the user
; for input (provided ECHO is ON)
;
PROMPT_CHAR	EQU	':'

; The initial value for last_pgm_exit_code -- used in the ERRORLEVEL stuff
; of IFs.
;
INIT_ERRLVL	EQU	0

; Maximum allowable input from user including the CR!
;
MAX_INP_LENGTH	EQU	128


; In the critical error handler DOS passes in AH the following information:
; Bit 3:	0 = FAIL response not allowed
;		1 = FAIL response allowed
; Bit 4:	0 = RETRY response not allowed
;		1 = RETRY response allowed
; Bit 5:	0 = IGNORE response not allowed
;		1 = IGNORE response allowed
; Bit 6:	? = reserved. I am using this to signify, if ABORT is
;		    allowed or not. Note that we cannot ABORT the minicmd.com
;		    if it generated the critical error, so we don't have any
;		    one to return to if we abort ourselves!!!


FAIL_BIT_MASK	EQU	00001000b
RETRY_BIT_MASK	EQU	00010000b
IGNORE_BIT_MASK	EQU	00100000b

ABORT_BIT_MASK	EQU	01000000b



; offset in char dev header where the DEV attr word is located.
;
OFFSET_DEV_ATTR	EQU	04h

; offset in char dev header where the DEV name (LPT1, PRN, etc) is.
;
OFFSET_DEV_NAME	EQU	0AH

; the length of the device name in the char device header -- if it is
; only 3 chars long like in PRN, the remaining 5 are padded with blanks!
DEV_NAME_LEN	EQU	8


; The following are the keys user is allowed to input to the Critical Error
; Action query and the code that is returned by the handler to DOS.

ABORT_KEY	EQU	'A'
ABORT_KEY_CODE	EQU	2

RETRY_KEY	EQU	'R'
RETRY_KEY_CODE	EQU	1

IGNORE_KEY	EQU	'I'
IGNORE_KEY_CODE	EQU	0

FAIL_KEY	EQU	'F'
FAIL_KEY_CODE	EQU	3
