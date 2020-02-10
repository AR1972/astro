;	CP-DOS error codes
;
;	SCCSID = @(#)error.inc	1.5 87/12/16
;
; WARNING. If modifying this file may have to modify files
; src\h\error.h
; src\doscall1\dosio.asm
; src\cmd\bind\api\d_eclass.asm
;
; CP-DOS calls all return error codes through AX.  If an error occurred then
; the carry bit will be set and the error code is in AX.  If no error occurred
; then the carry bit is reset and AX contains returned info.
;
; Since the set of error codes is being extended as we extend the operating
; system, we have provided a means for applications to ask the system for a
; recommended course of action when they receive an error.
;
; The GetExtendedError system call returns a universal error, an error
; location and a recommended course of action.	The universal error code is
; a symptom of the error REGARDLESS of the context in which GetExtendedError
; is issued.

;
; These are the 2.0 error codes
;
NO_ERROR			EQU	0
ERROR_INVALID_FUNCTION		EQU	1
ERROR_FILE_NOT_FOUND		EQU	2
ERROR_PATH_NOT_FOUND		EQU	3
ERROR_TOO_MANY_OPEN_FILES	EQU	4
ERROR_ACCESS_DENIED		EQU	5
ERROR_INVALID_HANDLE		EQU	6
ERROR_ARENA_TRASHED		EQU	7
ERROR_NOT_ENOUGH_MEMORY 	EQU	8
ERROR_INVALID_BLOCK		EQU	9
ERROR_BAD_ENVIRONMENT		EQU	10
ERROR_BAD_FORMAT		EQU	11
ERROR_INVALID_ACCESS		EQU	12
ERROR_INVALID_DATA		EQU	13
;**** reserved			EQU	14	; *****
ERROR_INVALID_DRIVE		EQU	15
ERROR_CURRENT_DIRECTORY 	EQU	16
ERROR_NOT_SAME_DEVICE		EQU	17
ERROR_NO_MORE_FILES		EQU	18
;
; These are the universal int 24 mappings for the old INT 24 set of errors
;
ERROR_WRITE_PROTECT		EQU	19
ERROR_BAD_UNIT			EQU	20
ERROR_NOT_READY 		EQU	21
ERROR_BAD_COMMAND		EQU	22
ERROR_CRC			EQU	23
ERROR_BAD_LENGTH		EQU	24
ERROR_SEEK			EQU	25
ERROR_NOT_DOS_DISK		EQU	26
ERROR_SECTOR_NOT_FOUND		EQU	27
ERROR_OUT_OF_PAPER		EQU	28
ERROR_WRITE_FAULT		EQU	29
ERROR_READ_FAULT		EQU	30
ERROR_GEN_FAILURE		EQU	31
;
; These are the new 3.0 error codes reported through INT 24
;
ERROR_SHARING_VIOLATION 	EQU	32
ERROR_LOCK_VIOLATION		EQU	33
ERROR_WRONG_DISK		EQU	34
ERROR_FCB_UNAVAILABLE		EQU	35
ERROR_SHARING_BUFFER_EXCEEDED	EQU	36
;
; New OEM network-related errors are 50-79
;
ERROR_NOT_SUPPORTED		EQU	50
ERROR_NETWORK_ACCESS_DENIED	EQU	65
;
; End of INT 24 reportable errors
;
ERROR_FILE_EXISTS		EQU	80
ERROR_DUP_FCB			EQU	81	; *****
ERROR_CANNOT_MAKE		EQU	82
ERROR_FAIL_I24			EQU	83
;
; New 3.0 network related error codes
;
ERROR_OUT_OF_STRUCTURES 	EQU	84
ERROR_ALREADY_ASSIGNED		EQU	85
ERROR_INVALID_PASSWORD		EQU	86
ERROR_INVALID_PARAMETER 	EQU	87
ERROR_NET_WRITE_FAULT		EQU	88
;
; New error codes for 4.0
;
ERROR_NO_PROC_SLOTS		EQU	89	; no process slots available
ERROR_NOT_FROZEN		EQU	90
ERR_TSTOVFL			EQU	91	; timer service table overflow
ERR_TSTDUP			EQU	92	; timer service table duplicate
ERROR_NO_ITEMS			EQU	93	; There were no items to operate upon
ERROR_INTERRUPT 		EQU	95	; interrupted system call

ERROR_TOO_MANY_SEMAPHORES	EQU	100	; user/sys open sem limit hit
ERROR_EXCL_SEM_ALREADY_OWNED	EQU	101	; exclusive sem already owned
ERROR_SEM_IS_SET		EQU	102	; SemClose found sem set
ERROR_TOO_MANY_SEM_REQUESTS	EQU	103	; too many exclusive sem reqs
ERROR_INVALID_AT_INTERRUPT_TIME EQU	104	; operation invalid at int time
ERROR_SEM_OWNER_DIED		EQU	105	; waitsem found owner died
ERROR_SEM_USER_LIMIT		EQU	106	; too many procs have this sem
ERROR_DISK_CHANGE		EQU	107	; insert disk b into drive a
ERROR_DRIVE_LOCKED		EQU	108	; drive locked by another process
ERROR_BROKEN_PIPE		EQU	109	; write on pipe with no reader
;
; New error codes for 5.0
;
ERROR_OPEN_FAILED		EQU	110	; open/created failed due to
						; explicit fail command
ERROR_BUFFER_OVERFLOW		EQU	111	; buffer passed to system call
						; is too small to hold return
						; data.
ERROR_DISK_FULL 		EQU	112	; not enough space on the disk
						; (DOSNEWSIZE/w_NewSize)
ERROR_NO_MORE_SEARCH_HANDLES	EQU	113	; can't allocate another search
						; structure and handle.
						; (DOSFINDFIRST/w_FindFirst)
ERROR_INVALID_TARGET_HANDLE	EQU	114	; Target handle in DOSDUPHANDLE
						; is invalid
ERROR_PROTECTION_VIOLATION	EQU	115	; Bad user virtual address
ERROR_VIOKBD_REQUEST		EQU	116
ERROR_INVALID_CATEGORY		EQU	117	; Category for DEVIOCTL in not
						; defined
ERROR_INVALID_VERIFY_SWITCH	EQU	118	; invalid value passed for
						; verify flag
ERROR_BAD_DRIVER_LEVEL		EQU	119	; DosDevIOCTL looks for a level
						; four driver.	If the driver
						; is not level four we return
						; this code
ERROR_CALL_NOT_IMPLEMENTED	EQU	120	; returned from stub api calls.
						; This call will disappear when
						; all the api's are implemented.
ERROR_SEM_TIMEOUT		EQU	121	; Time out happened from the
						; semaphore api functions.
ERROR_INSUFFICIENT_BUFFER	EQU	122	; Some call require the
					; application to pass in a buffer
					; filled with data.  This error is
					; returned if the data buffer is too
					; small.  For example: DosSetFileInfo
					; requires 4 bytes of data.  If a
					; two byte buffer is passed in then
					; this error is returned.
					; error_buffer_overflow is used when
					; the output buffer in not big enough.
ERROR_INVALID_NAME		EQU	123	; illegal character or malformed
						; file system name
ERROR_INVALID_LEVEL		EQU	124	; unimplemented level for info
						; retrieval or setting
ERROR_NO_VOLUME_LABEL		EQU	125	; no volume label found with
						; DosQFSInfo command
ERROR_MOD_NOT_FOUND		EQU	126	; w_getprocaddr,w_getmodhandle
ERROR_PROC_NOT_FOUND		EQU	127	; w_getprocaddr

ERROR_WAIT_NO_CHILDREN		EQU	128	; CWait finds to children

ERROR_CHILD_NOT_COMPLETE	EQU	129	; CWait children not dead yet

;This is a temporary fix for the 4-19-86 build this should be changed when
; we get the file from MS
ERROR_DIRECT_ACCESS_HANDLE	EQU	130	; handle operation is invalid
						; for direct disk access
						; handles
ERROR_NEGATIVE_SEEK		EQU	131	; application tried to seek
						; to negative offset
ERROR_SEEK_ON_DEVICE		EQU	132	; application tried to seek
						; on device or pipe
;
; The following are errors generated by the join and subst workers
;

ERROR_IS_JOIN_TARGET		EQU	133
ERROR_IS_JOINED 		EQU	134
ERROR_IS_SUBSTED		EQU	135
ERROR_NOT_JOINED		EQU	136
ERROR_NOT_SUBSTED		EQU	137
ERROR_JOIN_TO_JOIN		EQU	138
ERROR_SUBST_TO_SUBST		EQU	139
ERROR_JOIN_TO_SUBST		EQU	140
ERROR_SUBST_TO_JOIN		EQU	141
ERROR_BUSY_DRIVE		EQU	142
ERROR_SAME_DRIVE		EQU	143
ERROR_DIR_NOT_ROOT		EQU	144
ERROR_DIR_NOT_EMPTY		EQU	145
ERROR_IS_SUBST_PATH		EQU	146
ERROR_IS_JOIN_PATH		EQU	147
ERROR_PATH_BUSY 		EQU	148
ERROR_IS_SUBST_TARGET		EQU	149
ERROR_SYSTEM_TRACE		EQU	150	; system trace error

ERROR_INVALID_EVENT_COUNT	equ	151	; DosMuxSemWait errors
ERROR_TOO_MANY_MUXWAITERS	equ	152	;
ERROR_INVALID_LIST_FORMAT	equ	153	;
ERROR_LABEL_TOO_LONG		equ	154	; volume label is too big
ERROR_TOO_MANY_TCBS		equ	155	; can't create another tcb
						; in this PTDA
ERROR_SIGNAL_REFUSED		equ	156	; Signal refused
ERROR_DISCARDED 		equ	157	; segment is discarded
ERROR_NOT_LOCKED		equ	158	; segment was not locked
ERROR_BAD_THREADID_ADDR 	equ	159	; bad thread id address
ERROR_BAD_ARGUMENTS		equ	160	; bad environment pointer

						; passed to exec
ERROR_BAD_PATHNAME		equ	161	; bad pathname passed to exec
ERROR_SIGNAL_PENDING		equ	162	; signal already pending
ERROR_UNCERTAIN_MEDIA		equ	163	; ERROR_I24 mapping
ERROR_MAX_THRDS_REACHED 	equ	164	; No more proc slots
ERROR_MONITORS_NOT_SUPPORTED	equ	165	; ERROR_I24 mapping

;	following error code added for DOSGETRESOURCE [loader related]

ERROR_INVALID_SEGMENT_NUMBER	EQU	180

;	following error codes are used by FreeDynLink [loader]

ERROR_INVALID_CALLGATE		EQU	181
ERROR_INVALID_ORDINAL		EQU	182
ERROR_ALREADY_EXISTS		EQU	183
ERROR_NO_CHILD_PROCESS		EQU	184	; No child process to wait for
ERROR_INVALID_FLAG_NUMBER	EQU	186	; Invalid flag number
ERROR_SEM_NOT_FOUND		EQU	187	; Semaphore does not exist

;	following error codes have added  to make the loader error
;	messages distinct

ERROR_INVALID_STARTING_CODESEG		EQU	188
ERROR_INVALID_STACKSEG			EQU	189
ERROR_INVALID_MODULETYPE		EQU	190
ERROR_INVALID_EXE_SIGNATURE		EQU	191
ERROR_EXE_MARKED_INVALID		EQU	192
ERROR_BAD_EXE_FORMAT			EQU	193
ERROR_ITERATED_DATA_EXCEEDS_64k 	EQU	194
ERROR_INVALID_MINALLOCSIZE		EQU	195
ERROR_DYNLINK_FROM_INVALID_RING 	EQU	196
ERROR_IOPL_NOT_ENABLED			EQU	197
ERROR_INVALID_SEGDPL			EQU	198
ERROR_AUTODATASEG_EXCEEDS_64k		EQU	199
ERROR_RING2SEG_MUST_BE_MOVABLE		EQU	200
ERROR_RELOC_CHAIN_XEEDS_SEGLIM		EQU	201
ERROR_INFLOOP_IN_RELOC_CHAIN		EQU	202

ERROR_ENVVAR_NOT_FOUND			EQU	203
ERROR_NOT_CURRENT_CTRY			EQU	204
ERROR_NO_SIGNAL_SENT			EQU	205
ERROR_FILENAME_EXCED_RANGE		EQU	206	; if filename > 8.3
ERROR_RING2_STACK_IN_USE		EQU	207	; for FAPI
ERROR_META_EXPANSION_TOO_LONG		EQU	208	; if "*a" > 8.3
ERROR_INVALID_SIGNAL_NUMBER		EQU	209
ERROR_THREAD_1_INACTIVE 		EQU	210
ERROR_INFO_NOT_AVAIL			EQU	211 ;@@ PTM 5550
ERROR_LOCKED				EQU	212
ERROR_BAD_DYNALINK			EQU	213 ;@@ PTM 5760
ERROR_TOO_MANY_MODULES			EQU	214
ERROR_NESTING_NOT_ALLOWED		EQU	215

ERROR_CANNOT_SHRINK			EQU	216	;attempt made to shrink
							;ring 2 stack
ERROR_ZOMBIE_PROCESS			EQU	217
ERROR_STACK_IN_HIGH_MEMORY		EQU	218
ERROR_NO_CHILDREN			EQU	228
ERROR_INVALID_SCREEN_GROUP		EQU	229
;
; Error codes 230 - 249 are reserved for MS Networks
;

;	REMINDER: don't forget to update error.h

;
;	All user-defined error codes from device drivers are mapped into the
;	range 0xFF00 - 0xFFFF
;

ERROR_USER_DEFINED_BASE 	EQU	0FF00h



;	Device driver error codes

ERROR_I24_WRITE_PROTECT 		EQU	0
ERROR_I24_BAD_UNIT			EQU	1
ERROR_I24_NOT_READY			EQU	2
ERROR_I24_BAD_COMMAND			EQU	3
ERROR_I24_CRC				EQU	4
ERROR_I24_BAD_LENGTH			EQU	5
ERROR_I24_SEEK				EQU	6
ERROR_I24_NOT_DOS_DISK			EQU	7
ERROR_I24_SECTOR_NOT_FOUND		EQU	8
ERROR_I24_OUT_OF_PAPER			EQU	9
ERROR_I24_WRITE_FAULT			EQU	0Ah
ERROR_I24_READ_FAULT			EQU	0Bh
ERROR_I24_GEN_FAILURE			EQU	0Ch
ERROR_I24_DISK_CHANGE			EQU	0Dh
ERROR_I24_WRONG_DISK			EQU	0Fh
ERROR_I24_UNCERTAIN_MEDIA		EQU	10h
ERROR_I24_CHAR_CALL_INTERRUPTED 	EQU	11h
ERROR_I24_NO_MONITOR_SUPPORT		EQU	12h
ERROR_I24_INVALID_PARAMETER		EQU	13h

; THE FOLLOWING ARE MASKS FOR THE allowed action for hard error Daemon service

ALLOWED_FAIL			EQU	1h
ALLOWED_ABORT			EQU	2h
ALLOWED_RETRY			EQU	4h
ALLOWED_IGNORE			EQU	8h
ALLOWED_DETACHED		EQU	8000h

;NOTE: ABORT is ALWAYS allowed

I24_OPERATION			EQU	1h	;Z if READ,NZ if Write
I24_AREA			EQU	6h	; 00 if DOS
						; 01 if FAT
						; 10 if root DIR
						; 11 if DATA
I24_CLASS			EQU	80h	;Z if DISK, NZ if FAT or char

;	GetExtendedError CLASSes ACTIONs LOCUSs

; Values for error CLASS

ERRCLASS_OUTRES 	EQU	1	; Out of Resource
ERRCLASS_TEMPSIT	EQU	2	; Temporary Situation
ERRCLASS_AUTH		EQU	3	; Permission problem
ERRCLASS_INTRN		EQU	4	; Internal System Error
ERRCLASS_HRDFAIL	EQU	5	; Hardware Failure
ERRCLASS_SYSFAIL	EQU	6	; System Failure
ERRCLASS_APPERR 	EQU	7	; Application Error
ERRCLASS_NOTFND 	EQU	8	; Not Found
ERRCLASS_BADFMT 	EQU	9	; Bad Format
ERRCLASS_LOCKED 	EQU	10	; Locked
ERRCLASS_MEDIA		EQU	11	; Media Failure
ERRCLASS_ALREADY	EQU	12	; Collision with Existing Item
ERRCLASS_UNK		EQU	13	; Unknown/other
ERRCLASS_CANT		EQU	14	; Can't perform requested operation
ERRCLASS_TIME		EQU	15	; Timeout

; Values for error ACTION

ERRACT_RETRY		EQU	1	; Retry
ERRACT_DLYRET		EQU	2	; Delay Retry, retry after pause
ERRACT_USER		EQU	3	; Ask user to regive info
ERRACT_ABORT		EQU	4	; abort with clean up
ERRACT_PANIC		EQU	5	; abort immediately
ERRACT_IGNORE		EQU	6	; ignore
ERRACT_INTRET		EQU	7	; Retry after User Intervention

; Values for error LOCUS

ERRLOC_UNK		EQU	1	; No appropriate value
ERRLOC_DISK		EQU	2	; Random Access Mass Storage
ERRLOC_NET		EQU	3	; Network
ERRLOC_SERDEV		EQU	4	; Serial Device
ERRLOC_MEM		EQU	5	; Memory

; Abnormal termination codes

TC_NORMAL		EQU	0
TC_HARDERR		EQU	1
TC_GP_TRAP		EQU	2
TC_SIGNAL		EQU	3
