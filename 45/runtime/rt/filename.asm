	TITLE	FILENAME - filename scanning routines
;***
; FILENAME - filename scanning routines
;
;	Copyright <C> 1987, Microsoft Corporation
;
; Contains functions:
;	B$IValidatePath
;	B$GET_PATHNAME
;	B$ADD_EXT
;	B$GET_CUR_DRIVE
;	B$GET_CUR_DIR
;	B$GET_DEV_NUM
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	USESEG	CONST
	USESEG	_DATA
	USESEG	_BSS
	USESEG	DK_TEXT

	INCLUDE seg.inc
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE idmac.inc
	INCLUDE rtps.inc	; constants shared with QBI

;	Device Name Generator

DEVMAC	MACRO	ARG
	DB	'&ARG'
	DB	DN_&ARG
	ENDM


	externFP OpenCheckDrive	

sBegin	CONST
	staticB BAD_CHAR_TBL,<34,"+,:;<=>[]|">	; illegal filename chars
	BAD_CHAR_CNT	EQU ($ - BAD_CHAR_TBL)	; table length (w/o space)
	staticB	,<" ">,1	; reject spaces for B$IValidatePath
sEnd	CONST

sBegin	_DATA
	globalB	szWildDefault,<"*"> ; "*.bas" (MUST be before b$BAS_EXT!!!)
	globalB	b$BAS_EXT,<".BAS",0>	
	globalB	b$EXE_EXT,<".EXE",0>	
	EVEN			; sd's must be word-aligned
	staticW	fakeSD,0,2	; phoney SD for B$IValidatePath QB5 code.
sEnd	_DATA

sBegin	_BSS
	globalW b$PN_NAME,,1	; addr of first char in filename.ext
	staticW	FIRST_SLASH,?,1	; address of first "\" in pathname, for
				; underflow checking
	staticW	b$BadCharCnt,,1	; # of bytes of BAD_CHAR_TBL to use
sEnd	_BSS

	; special flag values
SLASH_SEEN		= FILNAML + 1	; last char was a "\"
SLASHDOT_SEEN		= FILNAML + 2	; last chars were "\."
SLASHDOTDOT_SEEN	= FILNAML + 3	; last chars were "\.."


assumes CS,DK_TEXT
sBegin	DK_TEXT

	externNP B$UPCASE
	externNP B$ERR_BFN
	externNP B$ERR_DNA


DEVICE_NAME_TABLE LABEL BYTE
	DEVNAM
	DB	0


;*** 
;B$IValidatePath -- interpreter filename parser (FAR)
;
;Purpose:
;	Interpreter reachable routine to call B$GET_PATHNAME and B$ADD_EXT.
;	Added with revision [9].
;Entry:
;	pSource  = DGROUP offset of filename to be parsed
;	cbSource = length of filename to be parsed
;	pDestBuf = DGROUP offset of destination buffer for parsed filename
;	pExt	 = DGROUP offset of extension to add (if none present)
;Exit:
;	AX = length of parsed filename (NOT including null terminator)
;Uses:
;	per conv.
;Preserves:
;	per conv.
;Exceptions:
;	Invalid file name, Device unavailable 
;******************************************************************************
cProc	B$IValidatePath,<FAR,PUBLIC>,<ES,SI,DI>
ParmW	pSource 			;offset of filename to be parsed
ParmW	cbSource			;length of filename to be parsed
ParmW	pDestBuf			;offset of dest buffer
ParmW	pExt				;offset of extension to add
cBegin

	PUSH	DS			;ES=DS
	POP	ES
	MOV	AX,[pSource]		; AX = ptr to filename string
	MOV	CX,[cbSource]		; CX = length of filename
	MOV	BX,OFFSET DGROUP:fakeSD	; BX = ptr to filename SD
	MOV	[BX],CX			; set up phony SD
	MOV	[BX+2],AX		
	MOV	DI,[pDestBuf]		;[DI] = ptr to destination buffer
	MOV	b$BadCharCnt,BAD_CHAR_CNT+1 ; reject spaces in filenames
	CALL	B$GET_PATHNAME_CORE	; returns CX=length (includes null)
					; and AL = flags
	MOV	SI,[pExt]		;[SI] = ptr to extension
	CALL	B$ADD_EXT		;add extension if one not already there
	XCHG	AX,CX			;[AX] = length of parsed filename
	DEC	AX			; don't include null byte in count
cEnd
ERBFN3:					
	JMP	ERBFN2			


	SUBTTL	General File Name Parser

;*** 
; B$GET_PATHNAME
;
;Purpose:
;
;	Generates the fully-qualified pathname from a filename.
;	Converts name to upper case, and removes "\..\" and "\.\" from the
;	pathname.  Resulting pathname is null-terminated.  Appends the drive
;	letter and a ":" to the front if it doesn't have one already.
;
;	Names within pathname do not have '*'s expanded to '?'s.  No checks
;	are done for too many slashes or length.  These checks are not
;	necessary, since the DOS calls that take as input this processed
;	pathname should perform those checks.  Both forward and backward
;	slashes are accepted as path characters.
;
;	An error is given if a filename has more than one dot.
;	Filenames without extensions that are longer than 8 characters have
;	a "." inserted after the 8th position. 
;	The extension portion is truncated to 3 chars.
;
;Algorithm:
;
;
;Entry:
;	BX    = pointer to filename string descriptor
;	DI    = address to place processed pathname (DGROUP offset)
;
;Exit:
;	AL = FLAGS -- bits FN_HAS_EXT and FN_WILD set appropriately.
;	CX = length of pathname (including the null byte)
;	Upper case, fully-specified pathname filled in.
;
;Uses:
;	none
;
;Preserves:
;	BX,DX
;
;Exceptions:
;	Invalid file name.
;
;******************************************************************************
labelNP	<PUBLIC,B$GET_PATHNAME>			
	mov	b$BadCharCnt,BAD_CHAR_CNT	; use default # of bad chars

cProc	B$GET_PATHNAME_CORE,<NEAR>,<ES,SI,BX,DX>	

cBegin

	PUSH	DS		; ES=DS
	POP	ES		
	MOV	SI,[BX+2]	; DS:SI = pointer to filename
	MOV	CX,[BX]		; CX = length of filename
				; ES:DI = pointer to destination buffer
	PUSH	DI		; save start of processed pathname
	LEA	BX,[DI+FILNAML] ; BX = address for overflow check

; place the drive letter into pathname

	DEC	CX		; filename length < 2?
	JLE	NO_DRIVE_SPEC	; brif so -- no drive specified
	CMP	BYTE PTR [SI+1],':' ; is next filename char a ":"?
	JNE	NO_DRIVE_SPEC	; brif not -- no drive specified
	LODSW			; get first two chars of filename into AX
	DEC	CX		; adjust count
	CALL	B$UPCASE	; convert drive letter to upper case
	JMP	SHORT GOT_DRIVE	; store specified drive in pathname

NO_DRIVE_SPEC:			; no drive specified at start of filename
	INC	CX		; restore filename length
	CALL	B$GET_CUR_DRIVE ; put current DRIVE_LETTER: into AX

GOT_DRIVE:			; AX contains "DRIVE_LETTER:"
	STOSW			; store "DRIVE_LETTER:" into pathname
	MOV	FIRST_SLASH,DI	; save address for underflow check
	cCall	OpenCheckDrive,<ax>	; put up a message box if we are
					; switching logical drives
					; PRESERVES ALL REGISTERS
	SUB	AX,':' SHL 8 + 'A'-1 ; convert AX to drive number (A=1,B=2,etc)
	XCHG	DX,AX		; DX = drive number (DH = 0)
	JCXZ	USE_CURDIR	; brif end of filename -- use current directory
	MOV	AL,[SI]		; get next char into AL
	CALL	CHK_FOR_SLASH	; slash char?
	JE	PROCESS_NAME	; brif so -- don't use current directory

USE_CURDIR:
	CALL	B$GET_CUR_DIR	; get current directory for the proper drive,
				; and store it into the pathname buffer

	XOR	AL,AL		; search for null byte
	PUSH	CX		; save count
	MOV	CX,-1		; search for up to 64K chars
	REPNE	SCASB		; set DI to position past null byte
	DEC	DI		; [DI] = null byte at end of pathname
	CMP	BYTE PTR [DI-1],'\' ; root directory?
	JE	Not_Root_Dir	; brif so -- don't add a "\"
	MOV	AL,'\'		; place a "\" between the current directory
	STOSB			; and the filename
Not_Root_Dir:			
	POP	CX		; restore count

PROCESS_NAME:
	CALL	SLASH_END	; indicate slash char seen, and set b$PN_NAME.
	JCXZ	CheckDir	; brif end of filename -- just return the
				; current directory for the proper drive

	; Register usage:
	;    DS:SI = pointer to next char in filename
	;    ES:DI = pointer to next char in processed pathname
	;	AL = next char in filename
	;	AH = KANJI Flags
	;		0  => not Double Byte Character
	;		1  => Double Byte Character
	;	BX = pathname buffer end address (for overflow check)
	;	CX = number of remaining chars in filename
	;	DH = flags:
	;		FN_WILD -- name portion of pathname contains a wildcard
	;		FN_HAS_EXT -- name portion of pathname has extension
	;	DL = last char(s) flag:
	;		SLASH_SEEN	-- last char in path is "\"
	;		SLASHDOT_SEEN	-- last chars in path are "\."
	;		SLASHDOTDOT_SEEN - last chars in path are "\.."
	;		otherwise	-- number of chars since last "\"
	; On stack:
	;	pathname buffer address


; Now add specified filename to current drive:path, processing "." and ".."

NAME_LOOP:
			; get next filename char
	LODSB			; AL = next filename character

	CALL	B$UPCASE	; convert to upper case
	STOSB			; store char from filename into pathname
	CMP	DI,BX		; are we now past the end of the buffer?
	JA	ERBFN2		;brif so -- illegal pathname

			; process the char

	CMP	AL,'.'		; is char a DOT?
	JNE	CHK_SLASH	; brif not -- check for slash

	CMP	DL,SLASHDOTDOT_SEEN ; seen "\..."?
	JAE	ERBFN2		;brif so -- invalid pathname
	CMP	DL,SLASH_SEEN	; seen "\." or "\.."?
	JAE	INC_COUNT	; brif so -- advance state
	TEST	DH,FN_HAS_EXT	; seen two dots since last "\"? ("x.y.z")
	JNZ	ERBFN2		; brif so -- invalid pathname.
; This handles name of the form "xxxxxxxxy.z" too, since "xxxxxxxxy"
; gets changed to "xxxxxxxx.y" before the ".z" is added.
DOT_EXIT:			
	OR	DH,FN_HAS_EXT	; set extension present flag
	JMP	SHORT ResetCount ; reset count to 1 (just the ".")

CHK_SLASH:
	CALL	CHK_FOR_SLASH	; is it a slash?
DJMP	JNE	CHK_BAD_CHAR	; brif not -- check for invalid char

	CALL	PROCESS_SLASH
	JMP	SHORT END_LOOP	; go to the end of the loop

ERBFN2:				; centrally located
DJMP	JMP	SHORT ERBFN	

VALID_CHAR:			
	CMP	AL,'*'		; is it an asterix?
	JE	IS_WILD_CHAR	; brif so
	CMP	AL,'?'		; is it a question mark?
	JNZ	CHK_DEFAULT	; brif not -- perform default char processing
IS_WILD_CHAR:
	OR	DH,FN_WILD	; flag wild card present

CHK_DEFAULT:
	CMP	DL,SLASHDOT_SEEN ; seen "\.x" (ext w/o name) or "\..x"?
	JAE	ERBFN		; brif so -- illegal pathname
	CMP	DL,SLASH_SEEN	; have we seen "\x"?
	JNE	INC_COUNT	; brif not -- just increment count
ResetCount:			
	XOR	DL,DL		; reset char count/last char flag to 0

INC_COUNT:
	INC	DX		; have seen another char

	CMP	DL,SLASH_SEEN	; processing name portion?
	JAE	END_LOOP	; brif not -- process next char
	CMP	DL,4		; valid length for either extension or name?
	JBE	END_LOOP	; brif so
	TEST	DH,FN_HAS_EXT	; seen a dot already?
	JNZ	BackUpChar	; brif so -- truncate extension to 3 chars

	CMP	DL,9		; 9 characters in name now?
	JB	END_LOOP	; brif < -- process next char
	MOV	AL,'.'		; pretend we now have a '.'
	XCHG	AL,[DI-1]	; turn "xxxxxxxxy" into "xxxxxxxx.y"
	STOSB			
INSERTED_DOT:
	OR	DH, FN_HAS_EXT	; Indicate the presence of the DOT
	MOV	DL, 1		; extenstion has 1 character (the DOT)
	JMP	SHORT INC_COUNT ; Include the new character in the count

				; here if truncating extension
BackUpChar:			; back up 1 char in output fname
	dec	di		; back up over char
	dec	dx		; one less char

END_LOOP:
	LOOP	NAME_LOOP	; process next char, if any

; filename has now been almost completely processed
	CMP	DL,SLASH_SEEN	; does current pathname end with "\." or "\.."?
	JBE	CheckDir	; brif not -- add the null byte
		;Special code to handle directory names
	INC	DI		; pretend there's another "\" on the end
	CALL	PROCESS_SLASH	; and process as if there is a slash char,
				; removing the dots.
CheckDir:			
	CMP	DL,SLASH_SEEN	; does it end with a '\'?
	JNE	ADD_NULL	; brif not -- just add the null byte
	OR	DH,FN_IS_DIR	; set directory flag so B$ADD_EXT won't
				; try to add an extension to directory name
ADD_NULL:
	XCHG	AX,CX		; set AX = 0, since CX will always = 0 here
	STOSB			; store terminating null byte

; complete null-terminated pathname without "\." or "\.." is now in buffer.

	MOV	CX,DI		; one past the last position (past null byte)
	POP	DI		; restore DI to point to the processed pathname
	SUB	CX,DI		; and set CX to the count of chars in pathname
				; (including the null)
	MOV	AL,DH		; return flags in AL
cEnd

ERBFN:				; centrally located
	jmp	B$ERR_BFN	; give bad filename error

CHK_BAD_CHAR:		; Check if char is a valid filename character.
	CMP	AL,' '		; control character?
	JB	ERBFN		; brif so -- bad filename

			; search table of invalid chars for current char
	PUSH	DI		; save registers
	PUSH	CX		
	MOV	DI,OFFSET DGROUP:BAD_CHAR_TBL ; table to search
	MOV	CX,b$BadCharCnt ; number of chars to search (changes)
	REPNE	SCASB		; search for char in table
	JZ	ERBFN		; brif char found -- bad filename
	POP	CX		; restore registers
	POP	DI		
	JMP	VALID_CHAR	; valid char -- continue processing





;*** 
;PROCESS_SLASH -- perform processing for a "\" in the pathname
;
;Purpose:
;	Eliminates "\.\" and "\dir\..\" from pathname string, and checks
;	for underflow (".." at the root directory level).
;
;Entry: (same conditions as within pathname processing loop)
;	ES:DI = pointer to next char in processed pathname
;	DH    = flags:
;		    FN_WILD -- name portion of pathname contains a wildcard
;		    FN_HAS_EXT -- name portion of pathname has extension
;	DL    = last char(s) flag:
;		    SLASH_SEEN	    -- last char in path is "\"
;		    SLASHDOT_SEEN   -- last chars in path are "\."
;		    SLASHDOTDOT_SEEN - last chars in path are "\.."
;Exit:
;	DI updated
;	DH = NOT FN_WILD and NOT FN_HAS_EXT
;	DL = SLASH_SEEN
;	b$PN_NAME = address of last "\" in pathname.
;
;Uses:
;	AL
;
;Preserves:
;
;Exceptions:
;	Bad File Name
;
;******************************************************************************
PROCESS_SLASH:			;Transform "\.\" or "\dir_name\..\" to "\"
	CMP	DL,SLASHDOT_SEEN ; seen "\.\"?
	JNE	CHK_SLASHDOTDOT	; brif not -- check for "\..\"
	DEC	DI		; remove the last "\"
	DEC	DI		; remove the "."
	JMP	SHORT SLASH_END	; indicate slash char seen

CHK_SLASHDOTDOT:
	CMP	DL,SLASHDOTDOT_SEEN ; seen "\..\"?
	JNE	SLASH_END	; brif not -- indicate slash char seen
	SUB	DI,4		; get rid of the "\..\"
				; [DI] = addr of the first "\"

REMOVE_SLASHES:
	DEC	DI		; remove another char
	MOV	AL,[DI]		; get deleted char
	CALL	CHK_FOR_SLASH	; just removed a previous "\"?
	JE	REMOVE_SLASHES	; remove another char

	CMP	DI,FIRST_SLASH	; have we deleted the first "\"?
	JB	ERBFN		; brif so -- underflow -- too many "\..\"'s
REMOVE_NAME:
	DEC	DI		; remove another char of the name
	MOV	AL,[DI]		; get deleted char
	CALL	CHK_FOR_SLASH	; just removed a previous "\"?
	JNE	REMOVE_NAME	; brif not -- delete another character
	INC	DI		; put back the last "\"

SLASH_END:
	MOV	DX,SLASH_SEEN	; indicate slash character seen, no wildcards,
				; and no extension
	MOV	b$PN_NAME,DI	; set address of name.ext in pathname
	RET			; return to caller


;*** 
; CHK_FOR_SLASH -- check if char is "\" or "/"
;
;Purpose:
;	Since we're to allow both forward and backwards slashes in the
;	pathname, check if char is "\" or "/".  Extracted to save code.
;
;Entry:
;	AL = char to check.
;Exit:
;	ZF if AL = "\" or "/", NZ otherwise.
;Uses:
;	None
;Preserves:
;	All
;Exceptions:
;	None
;
;******************************************************************************
CHK_FOR_SLASH:			; check for forward slash and back slash
	CMP	AL,'\'		; is it a back slash?
	JE	CHK_SLASH_EXIT	; brif so -- return to caller
	CMP	AL,'/'		; is it a forward slash?
CHK_SLASH_EXIT:
	RET			; return to caller


;*** 
; B$ADD_EXT -- add extension to pathname
;
;Purpose:
;	Adds a 4-byte extension to a name, updating the count and checking
;	for overflow.  It first checks to see that there is not already an
;	extension on the last name of the file.
;
;Entry:
;	ES:DI = address of null-terminated pathname to append extension to
;	CX = index into the pathname of the char past the char to overwrite
;		(usually the pathname length returned from B$GET_PATHNAME)
;	DS:SI = address of 4-bytes to be appended to name
;	AL = flags returned from B$GET_PATHNAME
;
;Exit:
;	if not already an extension
;	    extension appended to name at ES:DI
;	    CX = new length of name (INCLUDING the NULL)
;
;Uses:
;	SI,AX
;
;Preserves:
;	BX,DX
;
;Exceptions:
;	Bad file name
;
;******************************************************************************
cProc	B$ADD_EXT,<NEAR,PUBLIC>,<DI>
cBegin

	TEST	AL,FN_HAS_EXT or FN_IS_DIR ; does the filename already have
				; an extension (or end with a '\')?
	JNZ	NO_EXTENSION	; brif so -- don't add an extension

	DEC	CX		; CX = # chars w/o the null
	ADD	DI,CX		; set destination pointer to point to null byte

NextChar:			
	INC	cx		; advance count
	CMP	CX,FILNAML	; will we overflow with this char?
	JA	ERBFN		; brif so -- Bad file name error
	LODSB			; get char
	STOSB			; store it
	OR	AL,AL		; got the null yet?
	JNZ	NextChar	

NO_EXTENSION:
cEnd



;*** 
; B$GET_CUR_DRIVE -- get current drive letter and number
;
;Purpose:
;
;	get current drive number (1-26) and letter (A-Z) from DOS
;
;Entry:
;	none
;
;Exit:
;	AL    = current drive letter (A-Z)
;	AH    = ':'
;
;Uses:
;	DX
;
;Preserves:
;	BX,CX
;
;Exceptions:
;	None
;
;******************************************************************************
cProc	B$GET_CUR_DRIVE,<NEAR,PUBLIC>
cBegin

	CALLOS	GDRV		; AL = default drive  (A=0, B=1, etc)
	CBW			; clear high byte
	ADD	AX,':' SHL 8 + 'A' ; return drive letter in AL and ':' in AH

cEnd


;*** 
; B$GET_CUR_DIR -- return current directory for a given drive number.
;
;Purpose:
;
;Algorithm:
;	Add "\" to string
;	Get current directory from DOS into string
;
;Entry:
;	ES:DI = address of area to place current directory
;	DX = drive number (1-26)
;	ES=DS
;
;	Note: IF FV_FARSTR, DS is not necessarily DGROUP on entry.
;
;Exit:
;	current directory (null-terminated) loaded into buffer
;
;Uses:
;	AX,DX
;
;Preserves:
;	BX,CX
;
;Exceptions:
;	Device not available
;
;******************************************************************************
cProc	B$GET_CUR_DIR,<NEAR,PUBLIC>,<CX>
cBegin

	MOV	AL,'\'		; add a beginning "\"
	STOSB

	PUSH	SI		; save register
	MOV	SI,DI		; set SI to current location in buffer
	CALLOS	CURDIR,ERDNA	; load current directory into DS:SI
	POP	SI		; restore register
cEnd


ERDNA:
	jmp	B$ERR_DNA	; give Device unavailable error


;*** 
; B$GET_DEV_NUM
;
;Purpose:
;	Checks if the first 5 chars of a string are equal to some BASIC
;	device, and if so, returns the device number for that device.
;	Accepts names of the form "XXXX:yyyyyy" as a valid devices, and
;	copies the options string for valid devices into the specified buffer.
;
;Entry:
;	BX    = pointer to filename string descriptor
;
;Exit:
;	AL = device number if device, or 0 if not a valid BASIC device
;	FLAGS = result of "OR AL,AL"
;
;Uses:
;	None
;
;Preserves:
;	BX,CX,DX
;
;Exceptions:
;	none
;
;******************************************************************************

cProc	B$GET_DEV_NUM,<NEAR,PUBLIC>,<ES,SI,DI,CX>	
cBegin
	MOV	SI,[BX+2]	; [DS:SI] = pointer to string data
	CMP	WORD PTR [BX],5	; length < 5?
	JB	NOT_DEV		; brif so -- not a valid BASIC device
	CMP	BYTE PTR [SI+4],':' ; name of the form "xxxx:" ?
	JNZ	NOT_DEV		; brif not -- not a BASIC device

; No checks will need to be done for KANJI characters, because there are
; none in our tables.  The call to B$UPCASE will not matter, as it can not
; do anything to make a match succeed. (The first byte of a KANJI character
; will not be converted, nor will it be in the table.)

	MOV	DI,OFFSET DEVICE_NAME_TABLE
	PUSH	CS		; set ES = CS for table reference
	POP	ES

DEV_LOOP:			; for each device in table
	CMP	BYTE PTR ES:[DI],0 ; end of table?
	JZ	NOT_DEV		; brif so -- not a valid BASIC device
	PUSH	SI		; save start of name
	MOV	CX,4		; compare 4 chars
CMP_LOOP:
	LODSB			; get character from the name into AL
	CALL	B$UPCASE	; convert to upper case
	SCASB			; does it match char in the device table?
	JE	NEXT_CHAR	; brif so -- try next char
	ADD	DI,CX		; skip over remaining chars and dev # in table
	POP	SI		; retrieve start of name
	JMP	SHORT DEV_LOOP	; and try to match the next entry
NEXT_CHAR:
	LOOP	CMP_LOOP	; falls through if device matched

	POP	SI		; clean off stack
	MOV	AL,ES:[DI]	; return device number in AL
	OR	AL,AL		; set flags for calling routine
	JMP	SHORT DEV_EXIT
NOT_DEV:
	XOR	AL,AL		; not a device -- clear AL and flags
DEV_EXIT:
cEnd

sEnd	DK_TEXT
	END
