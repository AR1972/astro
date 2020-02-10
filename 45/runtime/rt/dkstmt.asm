	TITLE	DKSTMT - Disk I/O Interfaces
	page	56,132
;***
;DKSTMT - Disk I/O Interfaces
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains disk I/O interfaces for BASIC statements.
;
;	This module came from iodisk.asm, which had been split into three
;	modules, -- diskio.asm, dkopen.asm and fileio.asm.  Diskio.asm
;	contains all drivers for disk I/O except disk open and close.
;	Disk open is in dkopen.asm.  This module contains interfaces for
;	file I/O statements.
;
;BASIC Syntax mapping to included runtime entry points:
;
;- FILES Statement:
;
;     FILES Statement:
;
;     FILES [filespec]
;	 |
;     B$FILS
;
;- KILL Statement:
;
;     KILL filespec
;	 |
;     B$KILL
;
;- LOCK Statement:
;
;     LOCK [#]filenum [{, record} | [start] TO end]
;	 |
;     B$LOCK
;
;- NAME Statement:
;
;     NAME oldname AS newname
;	 |
;     B$NAME
;
;- RESET Statement:
;
;     RESET
;	 |
;     B$REST
;
;- UNLOCK Statement:
;
;     UNLOCK [#]filenum [{, record} | [start] TO end]
;	  |
;	B$LOCK
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	;Runtime Macro Defintions

;Code segmetns
	useSeg	DK_TEXT
	useSeg	ER_TEXT
	useSeg	NH_TEXT
	useSeg	RT_TEXT		
;Data segments
	useSeg	CONST
	useSeg	_DATA
	useSeg	_BSS

	INCLUDE seg.inc

	INCLUDE ascii.inc
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE idmac.inc		
	INCLUDE messages.inc
	INCLUDE rtps.inc		
	INCLUDE string.inc		

	SUBTTL	local constant definitions
	page

sBegin	CONST

	externB	b$EightSpaces		; buffer of 8 spaces
	staticB DirStr,"<DIR> "

sEnd	CONST

sBegin	_DATA


sEnd	_DATA

sBegin	_BSS

	externB b$PATHNAM 		; defined in GWINI.ASM
	externB b$Buf2 		; defined in GWINI.ASM
	b$PATHNAM2 EQU b$Buf2		
	externW b$PN_NAME		



sEnd	_BSS

	SUBTTL	code externals
	page

sBegin	DK_TEXT


	externNP	B$GET_PATHNAME	
	externNP	B$GET_CUR_DIR	
	externFP	B$PSI4		
	externNP	B$PUTS		
	externNP	B$OUTCNT		
	externNP	B$$WCHT
	externNP	B$$TCR
	externNP	B$TTY_GPOS
	externNP	B$TTY_GWID
	externNP	B$CLOSF
	externNP	B$CHKFOPEN	
	externNP	B$DOS3CHECK	;check for >= DOS 3.00
	externNP	B$MUL32	

sEnd	DK_TEXT

sBegin	NH_TEXT

	externNP	B$STDALCTMP
	externNP	B$LHFDBLOC	

sEnd	NH_TEXT

sBegin	ER_TEXT

	externNP	B$PUTNUM	; output message

	externNP	B$ERR_FC
	externNP	B$ERR_BFN
	externNP	B$ERR_BRN
	externNP	B$ERR_FAE
	externNP	B$ERR_FNF
	externNP	B$ERR_IFN
	externNP	B$ERR_ACD
	externNP	B$ERR_FWP
	externNP	B$ERR_AFE
	externNP	B$ERR_PNF	
	externNP	B$ERR_RAD	

sEnd	ER_TEXT

sBegin	RT_TEXT				
sEnd	RT_TEXT				


	PAGE

	assumes CS,DK_TEXT

sBegin	DK_TEXT

;***
;B$KILL - KILL filename
;
;Purpose:
;
;Entry:
; fname = filename sdesc
;
;Exit:
;
;Uses:
; Per convention
;
;Exceptions:
;	B$ERR_FNF -- for file not found
;	B$ERR_ACD -- for access denied
;	B$ERR_FAO -- for file already open
;
;******************************************************************************
cProc	B$KILL,<FAR,PUBLIC>,<SI,DI,ES> 
parmSD	fname			
cbegin				


	PUSH	DS		; set ES=DS for string operations
	POP	ES		
	MOV	BX,fname	; get *sd of filename
	MOV	DI,OFFSET DGROUP:b$PATHNAM ; Where to put the pathname
	CALL	B$GET_PATHNAME	; generate pathname, checking for errors
				; sets ES = DS
	CALL	B$STDALCTMP	;delete temporaries
	CALL	SearchNoDir	; find first file, or give "file not found"

KILL_NEXT:
;	DI --> pointer to processed filename

	PUSH	DI		; save pathname buffer address
	MOV	DI,b$PN_NAME	; DI = address past last "\" in pathname
				; (filename.ext field of pathname)
	mov	si,offset dgroup:b$PATHNAM2+30 ; Where dos put the filename

				; store name into the name field of pathname
dkk22:
	lodsb			;get a byte from the filename
	stosb			;store it into the pathname
	or	al,al		;end ?
	jnz	dkk22		;no

	pop	di		;--> pathname
	CALL	B$CHKFOPEN	; Check for conflict with open files
				; (doesn't return if error)

				; delete the link
	mov	dx,di		; DX = address of pathname to delete
	CALLOS	unlink,,,,dx
	JB	KILL_ERR	; give proper error message
	CALLOS	FINDN		;find next
	JNB	KILL_NEXT	; found next, continue


cEnd				

KILL_ERR:
	CMP	AX,ERRACD	;test if access error
	JNE	ERFNF		; if not, then KILL defaults to file not
				; found error
	JMP	B$ERR_ACD	  ;treat as access error
ERFNF:				
	JMP	B$ERR_FNF	; File not found

;***
; B$NAME - Name statement
;
;Purpose:
; Rename a file
;
;Entry:
; oldname = Name of existing file
; newname = name to change to
;
;Exit:
;
;Uses:
; Per convention
;
;Exceptions:
; B$ERR_FAE if newname already exists.
;
;******************************************************************************
cProc	B$NAME,<FAR,PUBLIC>,<ES,SI,DI> 
parmSD	oldname 		
parmSD	newname 		
cBegin				


	PUSH	DS		; set ES=DS
	POP	ES		

	GetpSD	BX,newname	
	MOV	DI,OFFSET DGROUP:b$PATHNAM2 
	CALL	NAME_COMMON	; do common processing for second name

	GetpSD	BX,oldname	; source pathname sdesc
	MOV	DI,OFFSET DGROUP:b$PATHNAM 
	CALL	NAME_COMMON	; do common processing for first name

	MOV	DX,OFFSET DGROUP:b$PATHNAM ;source
	MOV	DI,OFFSET DGROUP:b$PATHNAM2 ;destination
	CALLOS	RENAME		
	JC	NAME_ERR	


cEnd				

NAME_ERR: 			
	CMP	AL,ERRFNF	; file not found?
	JE	ERFNF		; brif so
	CMP	AL,ERRPNF	; path not found?
	JE	ERPNF		; brif so
	CMP	AL,ERRACD	; access denied?
	JE	ERFAE		; brif so -- File already exists
	CMP	AL,ERRNSD	; not same device?
	JE	ERRAD		; brif so -- Rename across disks
				; default to Illegal function call error
				; for NAME if not one of the above.
ERFC:
	JMP	B$ERR_FC	; Illegal function call
ERFAE:				
	JMP	B$ERR_FAE	; File already exists
ERPNF:
	JMP	B$ERR_PNF	; Path not found
ERRAD:				
	JMP	B$ERR_RAD	; Rename across disks
ERBFN:				
	JMP	B$ERR_BFN	; Bad file name
ERCFNF2:			
	JMP	SHORT ERFNF	; give "file not found" error

;*** 
; NAME_COMMON - common processing.  Added as part of [14].
;
;Purpose:
;	Common processing for each filename of B$NAME.
;
;	Algorithm:
;		call B$GET_PATHNAME
;		if (wildcards in filename) then error		
;		deallocate the string temp
;		if (file already open) then error
;
;Entry:
;	DI =	*space for pathname
;	BX =	pointer to user-specified filename string descriptor
;
;Exit:
;	DI =	*processed pathname
;	CX =	length of pathname (including null byte)
;
;Uses:
;	per convention
;
;Preserves:
;
;Exceptions:
;	B$ERR_BFN -- for bad file name
;	B$ERR_FAO -- for file already open
;
;******************************************************************************
NAME_COMMON:
	CALL	B$GET_PATHNAME	; Scan file name
				; sets ES = DS
	cCall	B$STDALCTMP	; deallocate string temp
	TEST	AL,FN_WILD	; wildcards in filename?
	JNZ	ERBFN 		; brif so -- error
				; [DI] = processed pathname
				; [CX] = number of bytes in pathname
	JMP	B$CHKFOPEN	; check for file already open and return
				; (doesn't return if error)

;*** 
; Search/SearchNoDir - search for file
;
;Purpose:
;	Search for files
;Entry:
;	b$PATHNAM has pathname to search for
;Exit:
;	Conditions after DOS search function:
;		b$PATHNAM2 has directory entry for first matching file
;		DX -> b$PATHNAM
;	ZF ==> file found, NZ ==> error
;
;Uses:
;	AX,CX,DX
;
;Preserves:
;
;Exceptions:
;
;******************************************************************************
Search:				
	mov	cx,ATTR_SUBDIR	; Search attributes
	SKIP	2		; eat the XOR and fall into common code

cProc	SearchNoDir,<NEAR>	; no directory
cBegin				
	xor	cx,cx		;no search attributes (files only)

	MOV	DX,OFFSET DGROUP:b$PATHNAM2 
	CALLOS	BUFF		;Set buffer address
	MOV	DX,OFFSET DGROUP:b$PATHNAM ; Address of pathname to search
	CALLOS	FINDF		; Use new one
	JC	ERCFNF2		; brif error -- file not found
cEnd				

	SUBTTL	B$REST - Reset statement
	PAGE
;***
; B$REST - Reset statement
;
;Purpose:
; Runtime entry. Close all open files.
;
;******************************************************************************
cProc	B$REST,<FAR,PUBLIC,FORCEFRAME> 
cBegin
	CALL	B$CLOSF		;Close all files

	CALLOS	GDRV		;Get drive number
	PUSH	AX
	CALLOS	REST		;Restore
	POP	AX
	MOV	DL,AL
	CALLOS	SDRV		;Set drive number

cEnd				

	SUBTTL	FILE interface
	page
newlin: 			;moved here from below
	CALL	B$TTY_GPOS
	XCHG	AL,AH
	ADD	AL,18		;Position after next file name
	CALL	B$TTY_GWID
	CMP	AL,AH
	RET


;***
;B$FILS - FILES Statement
;
;Purpose:
; Print a listing of all files in the current directory, which match the given
; string pattern
;
;Input:
; fname = filename sd
;
;Output:
; None
;
;Modifies:
; Per convention
;
;Exceptions:
; Control could be transfered to error code, such as B$ERR_FNF
;
;******************************************************************************
cProc	B$FILS,<FAR,PUBLIC>,<ES,DI,SI> 
parmW	fname
cBegin


	PUSH	DS		; set ES=DS for string operations
	POP	ES		
	MOV	BX,fname
	MOV	DI,OFFSET DGROUP:b$PATHNAM 
	call	B$GET_PATHNAME	; convert to standard pathname format
	CALL	B$STDALCTMP	;delete string if temp

	PUSH	DI		; save pathname buffer address
	
	MOV	AX,[DI]		; load drive letter and colon into AX
	MOV	DI,OFFSET DGROUP:b$PATHNAM2 ; buffer for current directory
	PUSH	DI		; save buffer address (for printing)
	STOSW			; store drive letter and ":" into buffer
	SUB	AX,':' SHL 8 + 'A' - 1 ; convert AX to drive number (1-26)
	XCHG	AX,DX		; DX = drive number

	CALL	B$GET_CUR_DIR 	; store current directory path for this
				; drive into buffer, after the drive letter

	; print the current directory for specified drive
	POP	AX		; AX = buffer offset
	PUSH	DX		; save drive number
	MOV	DX,DS		; DX = buffer segment
	CALL	B$PUTS		; print the null-terminated buffer
	CALL	B$$TCR		; print a CR

	; add "*.*" to the pathname if necessary
				; CX and DX preserved from above
	POP	DX		; restore drive number
	POP	DI		; restore pathname buffer address
	PUSH	DX		; save drive number for free space check
	ADD	DI,CX		; one past null byte
	DEC	DI		; DI = address of null byte
	CMP	DI,b$PN_NAME	; does pathname end with a "\"?
	JNZ	NO_ADD_EXT	; brif not -- don't add the extention
	MOV	AX,".*"		; add "*." to the pathname
	STOSW			
	MOV	AX,"*"		; add "*0" to the pathname
	STOSW			

NO_ADD_EXT:			

	CALL	Search		; search for first file with this name
				; or give "file not found"
d2off:
	mov	si,offset dgroup:b$PATHNAM2+30 ; name
	mov	cx,8
	mov	dx,"."
	call	outnam
	mov	cx,4
	xor	dx,dx
	call	outnam
	mov	si,offset dgroup:b$EightSpaces	
	cmp	byte ptr b$PATHNAM2+21,ATTR_SUBDIR ; check for directory attr
	jne	d2space
	mov	si,offset dgroup:DirStr
d2space:
	mov	cx,6		; print 6 chars
	CALL	B$OUTCNT		; print CX bytes

	call	newlin
	jl	findn
	call	B$$TCR
findn:
	callos	findn
	jnb	d2off

	CALL	B$$TCR		; list of files done -- output a CR
	POP	DX		; get back drive number
	CALLOS	FREES		;get disk free space
	mul	cx		;[ax]=bytes/sect*sect/alloc
	mul	bx		;[dxax]=[ax]*free alloc units
	PUSH	DX		; pass the I4
	PUSH	AX		
	call	B$PSI4		; and output that
	MOV	AX,MS_BYTESFREE ;output "Bytes free"
	cCall	B$PUTNUM	;output the message


cEnd				

outnam:
	mov	al," "
	cmp	byte ptr[si],dh ;end of string?
	je	atend
	cmp	byte ptr[si],dl ;possible end of name
	je	atend
	lodsb
atend:
	call	B$$WCHT		;write char
	loop	outnam
	ret

	page

	SUBTTL	LOCK/UNLOCK interfaces
	page

ERCFNF: JMP	B$ERR_FNF	;file not found
ERCAFE: JMP	B$ERR_AFE	;advanced feature

;***
;B$LOCK -- LOCK/UNLOCK statement interface
;void B$LOCK (I2 channel, I4 first_rec, I4 last_rec, U2 mode)
;
;Purpose:
;	Lock or unlock an opened file according to the value of mode.  The
;	file must be opened using the new OPEN syntax with the lock clause,
;	and DOS version must be 3.0 or above;otherwise a runtime error will
;	occur.	Between 2.0 and 3.0, the range of record number is slightly
;	different.  In 2.0, a 24-bit record number is supported, whereas a
;	31-bit record number is supported.
;
;	Note: BASICA 3.2 supports 32-bit record number.
;
;	BASCOM 2.0 uses two separate interfaces, $ULK & $LK0, for UNLOCK and
;	LOCK statement respectively.  In BASCOM 3.0, a single interface is
;	used with mode containing valuable information. The beauty of the mode
;	is that the first bit tells whether it is a lock or an unlock, and the
;	second bit tells whether it is to lock/unlock the whole file or not.
;
;	The syntax for LOCK/UNLOCK is as follows:
;	LOCK/UNLOCK [#]channel [,[first-rec] [TO last-rec]]
;
;	B$CHAN WILL NOT be called prior to B$LOCK, this routine has to
;	check the file is opened or not.
;
;	The 'first-rec' is the 'from' record # and 'last-rec' is the 'to'
;	record #.  The default 'first-rec' is 1 and the default 'last-rec'
;	is the same record number as the 'first-rec.'  For example,
;	"LOCK #1, TO 3" will generate first=1, last=3 & mode=2, and
;	"UNLOCK #1, 5" will generate first=5, last=5 & mode=3.
;
;	NOTE: Currently, record number is only meaningful to the random file.
;		Sequential file is always lock/unlocked the whole file, and
;		there is no range check of 'first-rec' or 'last-rec' for a
;		sequential file.  (the compiler or the interpreter parser
;		does check whether the parameter is a numerical type.)
;
;	The low order byte of mode may contain the following values according
;	to different situations (the high order word should wlays be IGNORED,
;	as the interpeter uses it):
;
;	mode = 0 -- lock entire file
;	mode = 2 -- lock from first to last
;
;	mode = 1 -- unlock entire file
;	mode = 3 -- unlock from first to last
;
;	The reason is illustrated as the following bit map.
;	mode:
;		_________________________________
;		| 0 | 0 | 0 | 0 | 0 | 0 | X | Y |
;		---------------------------------
;		MSB				LSB
;	where:
;	X is used to represent whether to lock/unlock the whole file, and
;	Y is used to decide whether it is a lock or is an unlock.
;
;	The runtime may then use one AND, instead of one AND & one SHR, before
;	call DOS lock/unlock function. (The value needed in AL is 0 for lock
;	and 1 for unlock.  Refer to the next paragraph.)
;
;	The DOS function call for lock/unlock a file needs:
;	[AH]	=	5CH
;	[AL]	=	0	-- lock
;			1	-- unlock
;	[BX]	=	file handle
;	[CX]	=	offset high
;	[DX]	=	offset low
;	[SI]	=	length high
;	[DI]	=	length low
;Entry:
;	Parameters are pushed in stack
;	int		channel
;	long int	first_rec
;	long int	last_rec
;	ushort		mode
;Exit:
;	none
;Uses:
;	none
;Exceptions:
;
;*******************************************************************************

cProc	B$LOCK,<PUBLIC,FAR>,<SI,DI>	;push di,si
	ParmW	Channel 	;I2 file number
	ParmD	First		;I4 first record number
	ParmD	Last		;I4 last record number
	ParmW	Mode		;U2 mode
cBegin				;set up stack frame
	cCall	B$DOS3CHECK	;must be DOS 3.0 or above
	JB	ERCAFE		;Brif not, give "advanced feature"
	MOV	BX,Channel	;get file number
	CALL	B$LHFDBLOC	;NZ then SI has the pointer to FDB
	JZ	ERCIFN		; Brif not, give "bad file number"
	FDB_PTR ES,SI,SI	;(ES:)[SI] = *FDB
	MOV	BX,FileDB.FD_HANDLE ; get file handle in BX
	PUSH	BX		;save file handle
	TEST	Mode,LOCK_1stToLast ; test the "entire file" bit
	JZ	LockEntire	; jump if we want to lock it all
	TEST	FileDB.FD_MODE,MD_RND+MD_BIN	
				; is opened for random or binary ?
	JNZ	 LockOffset	; Brif yes, record number is meaningful

LockEntire:			
	XOR	CX,CX		;starting record
	MOV	DX,CX		;(0-relative)
	MOV	SI,CX		;offset
	DEC	SI		;SI=0FFFFh
	MOV	DI,SI		;lock the whole file
	JMP	SHORT LockFile	;go lock it
ERCIFN: JMP	B$ERR_IFN	; bad file number
LockOffset:
	MOV	CX,Off_First	; get first record number low
	MOV	DX,Seg_First	; get first record number high
	OR	DX,DX		; negative number?
	JS	ERCBRN		; Brif yes, give "bad record number"
	SUB	CX,1		;decrement one, set carry if not big enough
	SBB	DX,0		;if CY ([DX|CX] = 0), then give error
	PUSH	CX		;save 0-relative first rec# low
	PUSH	DX		;save 0-relative first rec# high
	JB	ERCBRN		;give "bad record number"
	MOV	AX,CX		;get a copy of rec# low
	OR	AX,DX		;is 0 ?
	MOV	AX,FileDB.FD_VRECL ; [AX] = multiplicant (also used below)
	JZ	LockLen 	;Brif yes, no translate needed, to get length

	PUSH	AX		;save multiplicand
	cCall	B$MUL32	;[DX|CX]=[DX|CX]*[AX], if CY then overflow
				; generates Bad record number on overflow
	POP	AX		
LockLen:
	POP	SI		;get first 0-relative first rec# high
	POP	DI		;get first 0-relative first rec# low
	PUSH	CX		;push low offset
	PUSH	DX		;push high offset
	MOV	CX,Off_Last	; get Second record number low
	MOV	DX,Seg_Last	; get Second record number high
	MOV	BX,CX		
	OR	BX,DX		; Make sure non-zero
	JZ	ERCBRN		
	OR	DX,DX		; negative number?
	JS	ERCBRN		; Brif yes, give "bad record number"
	SUB	CX,DI		;calculate how many records
	SBB	DX,SI		;[DX|CX]= second# - (first# - 1)
	JB	ERCBRN		; Brif second > first, give bad record num
				; [AX] = multiplicant, set up above...
	cCall	B$MUL32	;[DX|CX]=[DX|CX]*[AX], if CY then overflow
				; generates Bad record number on overflow
	MOV	DI,CX		;DI=length low
	MOV	SI,DX		;SI=length high
	POP	CX		;CX=offset high
	POP	DX		;DX=offset low
LockFile:
	POP	BX		;get back file handle
	MOV	AX,Mode 	;get mode
	AND	AL,1		;lock or unlock
	CALLOS	LOCKING,ULKERR	;go lock it
cEnd				;pop si,di and exit to caller

ULKERR:
	CMP	AX,ERRIVH	;invalid handle ?
	JZ	ERCIFN		;bad file number
	JMP	B$ERR_FWP	;now "Permission Denied"
ERCBRN: JMP	B$ERR_BRN	; bad record number


cEnd

sEnd	DK_TEXT

	END
