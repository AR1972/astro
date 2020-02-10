;***
;oscall.inc - 20-May-86  -  Dos 3/5 macros and definitions
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This include file contains structures and definitions for
;	the Operating system interface.
;
;******************************************************************************

	SUBTTL	"Operating system data structures and definitions"

;******************************************************************************
;* Disk and Device I/O data structures
;******************************************************************************

VOLUME_OR_SUBDIR	EQU	18H	;DOSQFILEMODE mask for subdir or vol label

SearchInfo	STRUC			;information used in DosFindFirst etc.
	CreatDate	DW	?	;date file was created
	CreatTime	DW	?	;time file was created
	AcessDate	DW	?	;date of last file access
	AccessTime	DW	?	;time of last access
	WriteDate	DW	?	;date of last write to file
	WriteTime	DW	?	;time of last write
	EOFPtr		DD	?	;end of file pointer
	EOAPtr		DD	?	;end of allocated space pointer
	fBlock		DW	?	;blocking flag
	FileAttribute	DW	?	;file attribute (hidden, archive, etc)
	cbFileName	DB	?	;length of file name
	szFileName	DB	128 DUP(?)	;file name
SearchInfo	ENDS


FsInfo	STRUC				;file system information
	FsID		DD	?	;file system ID
	cSecUnit	DD	?	;Number of sectors per allocation unit
	cUnits		DD	?	;Number of allocation units
	cUnitsAvail	DD	?	;Number of available allocation units
	cBytesSec	DW	?	;Number of bytes per sector
FsInfo	ENDS

;******************************************************************************
;* Miscellaneous data structures
;******************************************************************************

TimeDate	STRUC
	Hour		DB	?	;current hour
	Minute		DB	?	;current minute
	Second		DB	?	;current second
	Hundredth	DB	?	;current hundredth of a second
	Day		DB	?	;current day
	Month		DB	?	;current month
	Year		DW	?	;current year
	TimeZone	DW	?	;Time zone minutes from GMT
	WeekDay 	DB	?	;current day of the week
TimeDate	ENDS

;******************************************************************************
;* Keyboard data structures
;******************************************************************************

CharData	STRUC
	AsciiCode	DB	?	;ASCII character code
	ScanCode	DB	?	;Character Scan code
	Status		DW	?	;0=no chars avail
					;1=char is available (interim flag off)
					;2=means Interim char available
					;3=final char avail (interim flag on)
	fShift		DW	?	;state of shift keys
	TimeStamp	DB 4 DUP(?)	;hr min sec sec/100
CharData	ENDS


KbdStatus	STRUC
	KLength 	DW	5	;high byte reserved by DOS
					;low byte = length of KbdStatus struct
	ModeMask	DW	?	;input mode bit fields
					;High byte = 0 (reserved)
					;low byte bits
					; 7 - 0 reserved
					; 6 - 1 = modifying CR character
					; 5 - 1 = modifying interim char flags
					; 4 - 1 = modifying shift state
					; 3 - 1 = COOKED input mode
					; 2 - 1 = RAW input mode
					; 1 - 1 = ECHO off
					; 0 - 1 = ECHO on
	CRChar		DW	?	;high byte = 0, low byte = ASCII char
					;being defined as new Carriage return
					;character
	InterimFlag	DW	?	;high byte = 0, low byte bit fields
					; 7 - 1 = Interim char flag = ON
					; 6 - 0 reserved
					; 5 - 1 = on-the-spot conversion
					; 4/0 - 0 reserved
	ShiftState	DW	?	;high byte = 0, low byte bit fields
					; 7 - 1 = Insert ON
					; 6 - 1 = CapsLock ON
					; 5 - 1 = NumLock ON
					; 4 - 1 = ScrollLock ON
					; 3 - 1 = Alt key down
					; 2 - 1 = Ctrl key down
					; 1 - 1 = Left Shift key down
					; 0 - 1 = Right Shift key down
KbdStatus	ENDS

;******************************************************************************
;* Screen I/O data structures
;******************************************************************************

ScreenModeData	STRUC
	SLength DW	12D	;Length of ModeData including Length (in bytes)
	Mode	DB	?	;type: bit 0 = color adapter
				;	   1 = graphics mode
				;	   2 = color burst disable
	Color	DB	?	;colors: 0=mono, 1=2, 2=4, 4=16
	ColNum	DW	?	;number of columns
	RowNum	DW	?	;number of Rows
	HorRes	DW	?	;Horizontal pixel resolution
	VerRes	DW	?	;Vertical pixel resolution
ScreenModeData	ENDS

CursorData	STRUC
	StartLine	DW	?	;Starting Scan line for cursor
	EndLine 	DW	?	;Ending Scan line for cursor
	CWidth		DW	?	;0=1Column, !0=pixel width
	Attribute	DW	?	;-1=Hidden, else normal(text) or
					;color attribute(graphics)
CursorData	ENDS


CharCell	STRUC
	Char	DB	?	;character
	Attrib	DB	?	;color Attribute of Char
CharCell	ENDS

;******************************************************************************
;* Program EXEC and environment constants and structures
;******************************************************************************

	SYNC_EXEC	EQU	0	;[1]synchronous execution
	ASYNC_EXEC	EQU	1	;[1]asynchronous execution, throw away
					;[1]exit code
	ASYNC_EXEC_CODE EQU	2	;[1]asynchronous execution, remember
					;[1]exit code

	NO_SYS_TRACE	EQU	0	;[1]EXEC without system debugging
	SYS_TRACE	EQU	1	;[1]EXEC with system debugging

;******************************************************************************
;* OFFSETS of items in the GDT and LDT info segments
;******************************************************************************

;GDT TIME offsets

GDT_ELAPSED_SEC EQU	0	;[2]seconds from 1-1-1970
GDT_MILLISEC	EQU	4	;[2]milliseconds
GDT_HOUR	EQU	8	;[2]current HOUR
GDT_MINUTE	EQU	9	;[2]current MINUTE
GDT_SECOND	EQU	0AH	;[2]current SECOND
GDT_HUNDRED	EQU	0BH	;[2]current HUNDREDTH of a second
GDT_TIMEZONE	EQU	0CH	;[2]minutes from GMT
GDT_TIMER	EQU	0EH	;[2]timer interval (.0001)

;GDT DATE offsets

GDT_DAY 	EQU	10H	;[2]current DAY
GDT_MONTH	EQU	11H	;[2]current MONTH
GDT_YEAR	EQU	12H	;[2]current YEAR
GDT_WEEKDAY	EQU	14H	;[2]current WEEKDAY

;GDT Version offsets

GDT_MINOR_VER	EQU	15H	;[2]OS minor version number
GDT_MAJOR_VER	EQU	16H	;[2]OS major version number
GDT_REVISION	EQU	17H	;[2]OS revision letter

;GDT system status offsets

GDT_SCREENGROUP EQU	18H	;[2]current screen group
GDT_MAX_SCREEN	EQU	19H	;[2]maximum number of screen groups
GDT_HUGE_SHIFT	EQU	1AH	;[2]HUGE segment shift count
GDT_PROTECTONLY EQU	1BH	;[2]protect mode only flag

;GDT scheduler parameter offsets

GDT_DYNAMIC	EQU	1CH	;[2]dynamic variation flag
GDT_MAX_WAIT	EQU	1DH	;[2]maximum wait period
GDT_MIN_SLICE	EQU	1EH	;[2]minimum time slice
GDT_MAX_SLICE	EQU	20H	;[2]maximum time slice

;LDT offsets

LDT_PID 	EQU	0	;[2]process ID
LDT_PARENT_PID	EQU	2	;[2]parent process ID
LDT_PRIORITY	EQU	4	;[2]priority of current thread
LDT_THREAD_ID	EQU	6	;[2]thread ID
LDT_SCREENGROUP EQU	8	;[2]process screen group
LDT_SUBSCREEN	EQU	0AH	;[2]sub screen group
LDT_FOREGROUND	EQU	0CH	;[2]process in foreground flag

	.LIST
