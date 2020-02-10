   TITLE UiPaths.asm - Configurable path-searching functions

COMMENT	\

--------- --- ---- -- ---------- ----
COPYRIGHT (C) 1988 BY MICROSOFT, INC.
--------- --- ---- -- ---------- ----

\
;============================================================================
; Module: UiPaths.asm - Configurable path-searching functions
; System: Quick BASIC Interpreter
;============================================================================

	.xlist
	include		version.inc
	UIPATHS_ASM = ON

	include cw/version.inc
	include cw/windows.inc
	include cw/edityp.inc
	include cw/dlg.inc		

	includeOnce	architec	
	includeOnce	heap
	includeOnce	rtps
	includeOnce	qbimsgs
	includeOnce	ui
	includeOnce	uiint		
	.list

assumes	DS,DATA
assumes	SS,DATA
assumes	ES,NOTHING


;
; external declarations for runtime routines used by FindFile
;
	EXTRN	B$FindFileOnPaths:FAR


	externFP NormFileNameFar		

	externFP HelpAlloc			

;-------------------------------------------------------------------------
;		DATA Segment Declarations
;-------------------------------------------------------------------------
sBegin	DATA


	externB bufLib				; literal "LIB"
	EXTRN	CB_bufLib:ABS			; length of "LIB"
	externB bufPath 			; literal "PATH"
	EXTRN	CB_bufPath:ABS			; length of "PATH

	bufInclude  db	"INCLUDE"
	CB_bufInclude	=   $ - bufInclude

	labelW	tSearchPathBds			; table of bd's
	globalB bdLibPath,0,<SIZE bd>		; bd for lib path
	globalB bdExePath,0,<SIZE bd>		; bd for exe path
	globalB bdInclPath,0,<SIZE bd>		; bd for include path
	globalB bdHelpPath,0,<SIZE bd>		; bd for help path


	externW iHelpId 			
	externB HelpFlags			
	externB b$Buf1				; buffer for saving dir
	externB b$Buf2				; buffer for dialog box fns
	externW b$PN_NAME			; *to name.ext in pathname
						; file type radio buttons
	staticB	szDot,<".",0>			
	externB	b$BAS_EXT			; ".BAS"
	externB	szWildDefault			; "*.BAS"
	globalB fFileCreate,0			; NZ if doing File/Create

sEnd	DATA


sBegin	UI					
assumes CS,UI					

	externNP SetCurDir2			
	externNP GetCurDrive2			
	externNP GetCurDriveDir			
	externNP SetCurDrive2			
	externNP RetryError			


	externNP AppendBuf			;[14]

labelW	tDefEnvVars			; table of default env vars (FAR)
	dw	DATAOFFSET bufLib		; pointer to "LIB"
	dw	CB_bufLib			; length of "LIB"
	dw	0				;	    placeholder
	dw	DATAOFFSET bufPath		; pointer to "PATH"
	dw	CB_bufPath			; length of "PATH"
	dw	0				;	    placeholder
	dw	DATAOFFSET bufInclude		; pointer to "INCLUDE"
	dw	CB_bufInclude			; length of "INCLUDE"
	dw	0				;	    placeholder
	dw	DATAOFFSET bufPath		; pointer to "PATH"
	dw	CB_bufPath			; length of "PATH"

; The following file types are used to index the preceding table, so they
; better have the right values.

.errnz	bdLibPath  - tSearchPathBds - LIBFILE	
.errnz	bdExePath  - tSearchPathBds - EXEFILE	
.errnz	bdInclPath - tSearchPathBds - INCFILE	
.errnz	bdHelpPath - tSearchPathBds - HELPFILE	

;****
;FindAndOpenFile (pszFile, fWrite, fFileType)
;
;Purpose:
;	Searches the current directory, the appropriate user-specified
;	search path, and the appropriate environment variable for the
;	file pszFile and opens the file based on fWrite.
;
;Input:
;	pszFile   - near pointer to null-terminated file name to open
;	fWrite	  - write only mode flag:
;			T => open file in write only mode
;			F => open file in read only mode
;	fFileType - type of file being searched for (determines search path)
;			LIBFILE  => search user-specified library path
;			EXEFILE  => search user-specified exe-file path
;			INCFILE  => search user-specified include path
;			HELPFILE => search user-specified help path
;Output:
;	if successful
;	   AX == file handle
;	else
;	   AX == 0
;
;****
cProc	FindAndOpenFile,<PUBLIC,FAR>,<DS>
	ParmW	pszFile
	ParmW	fWrite
	ParmW	fFileType
cBegin
DbAssertRel b$fInt24Err,ne,0,UI,<FindAndOpenFile: int 24 errors not ignored>
	cCall	<far ptr FindFile>,<pszFile,fFileType>
	or	ax,ax			; found file?
	jz	FindOpenExit		; no, return with AX = 0

	mov	ds,dx
	xchg	ax,dx			; ds:dx = ptr to pathname to open
	xor	ax,ax			; get a zero
	cmp	ax,[fWrite]		; set CY if fWrite is TRUE
	rcl	ax,1			; set AX = 1 if fWrite is TRUE

; Now AX = 0 if fWrite is FALSE or 1 if fWrite is TRUE.  That means
; AL = the mode which we need for the int 21h for the OPEN.

	mov	ah,3dh			; fn # for open file
	int	21h			; open the file
	jnc	FindOpenExit		; return with handle in AX if success

DbAssertRel	ax,ne,2,UI,<FindAndOpenFile: OPEN got File Not Found error>
DbAssertRel	ax,ne,3,UI,<FindAndOpenFile: OPEN got Path Not Found error>
DbAssertRel	ax,ne,12,UI,<FindAndOpenFile: OPEN got Invalid Access error>

	xor	ax,ax			; open failed, return zero
FindOpenExit:
cEnd

;****
;FindFile (pszFile, fFileType)
;
;Purpose:
;	Searches the current directory, the appropriate user-specified
;	search path, and the appropriate environment variable for the
;	file pszFile and returns a pointer to a fully-qualified pathfile
;	name where the file was found.
;
;Input:
;	pszFile   - near pointer to null-terminated file name to open
;	fFileType - type of file being searched for (determines search path)
;			LIBFILE  => search user-specified library path
;			EXEFILE  => search user-specified exe-file path
;			INCFILE  => search user-specified include path
;			HELPFILE => search user-specified help path
;Output:
;	if successful
;	   DX:AX == pointer to pathfile name where file was found
;	else
;	   AX == 0
;
;****
cProc	FindFile,<PUBLIC,FAR>		
	ParmW	pszFile
	ParmW	fFileType
cBegin

	mov	bx,[fFileType]
	cCall	B$FindFileOnPaths,<ds,pszFile,[bx].tSearchPathBds.BD_pb,ds,cs:[bx].tDefEnvVars,cs:[bx+2].tDefEnvVars>	

; B$FindFileOnPaths returns exactly what we want to return here so no
; furthur processing is necessary.

cEnd


;****
;OpenFileOnPath (pszFile, fWrite) -- Help engine callback.
;
;Purpose:
;
;	Searches the current directory, the appropriate user-specified
;	search path, and the appropriate environment variable for the
;	file pszFile and opens the file based on fWrite.  If pszFile
;	already contains a path (ie: a "\") no searching is done; the
;	specified path/file name is used without modification.
;
;	NOTE: CAN cause heap movement when doing the alloc for the virtual
;	file handle.  This SHOULD NOT be a problem, since the help engine
;	doesn't hold any "locks" when it does the call to OpenFileOnPath.
;
;	See HENGINE.ASM for a discussion of virtual file handles.
;
;Input:
;	pszFile   far pointer to null-terminated file name to open
;		      Filename may have environment variable prepended
;		      to it in the form: $ENVVAR:filename
;	fWrite	  write only mode flag:
;		      T => open file in write only mode
;		      F => open file in read only mode
;Output:
;	if successful
;	   AX == file handle
;	else
;	   AX == 0
;
;****

cProc	OpenFileOnPath,<PUBLIC,FAR>,<SI,DI>
	ParmD	pszFile
	ParmW	fWrite
cBegin

	push	iHelpId			; msg boxes could trash this

DbAssertRel	fWrite,e,0,UI,<OpenFileOnPath: fWrite != 0>	
DbAssertTst	HelpFlags,z,HLP_INHELP,UI,<OpenFileOnPath: recursion lock set>
	or	HelpFlags,HLP_INHELP	; set recursion lock

RetryOpen:
	les	si,[pszFile]		; es:di points to input string

; Parse the input filename to isolate environment variable if present
; and set up pointers for searching.
; Note: we can't test for the presence of ENVVAR just by testing for the
;	first char being '$' because that is a legal first char for a
;	filename.

	lods	byte ptr es:[si]	; get 1st character
	mov	bx,si			; save this pointer
	cmp	al,'$'			; first char == '$' ?
	je	EnvVarCheck		; yes, check for possible env var
NoEnvVar:
; No $ENVVAR: was specified in filename, set up defaults.
; (Defaults are user-specified HELP path and environment variable PATH.

	push	es
	push	[off_pszFile]		; push far ptr to filename
	push	es			; push second copy for possible
	push	[off_pszFile]		; display in the MessageBox

OpenNoEnvVar:
	mov	dx,[bdHelpPath].BD_pb	; dx = ptr to user-specified HELP path
	push	ds
	pop	es
	mov	bx,DATAOFFSET bufPath	; es:bx = ptr to constant "PATH"
	mov	cx,CB_bufPath		; cx = length of constant "PATH"
DJMP	jmp	SHORT CallFind		; finish up

EnvVarCheck:
	xor	cx,cx			; init counter
EnvVarLoop:
	lods	byte ptr es:[si]	; get next character
	or	al,al			; end of string?
	jz	NoEnvVar		; yes, then there's no env var present
	inc	cx			; count this char
	cmp	al,':'			; found end of env var?
	jne	EnvVarLoop		; no, try next character
	dec	cx			; don't count last ':' as part of name

DbAssertRel	cx,nz,0,UI,<OpenFileOnPath: null environment variable specified>

; If we fall through to here, $ENVVAR: was found.
;	es:bx points to environment variable name part of string
;	cx = length of environment variable name
;	es:si points to filename part of input string

	push	es
	push	si			; push far ptr to filename part
	push	es			; save second copy for error msg
	push	si			
	push	cx			; save env var length

; Determine which user-specified search path we will be using.	If the
; ENVVAR we just found is INCLUDE, then we'll use bdInclPath.  Otherwise
; we'll use bdHelpPath.

	cmp	cx,CB_bufInclude
	jne	HaveBdPtr		; can't match if not the same length
	mov	di,bx			; es:di points to ENVVAR
	mov	si,DATAOFFSET bufInclude ; ds:si points to constant "INCLUDE"
	mov	dx,[bdInclPath].BD_pb	; assume INCLUDE
	repz	cmpsb			; compare the strings
	jz	HaveBdPtr		; they matched. use INCLUDE
	mov	dx,[bdHelpPath].BD_pb	; use HELP

HaveBdPtr:
	pop	cx			; restore env var length

; First parm to B$FindFileOnPaths is the far ptr to filename we pushed above.

CallFind:
	cCall	B$FindFileOnPaths,<dx,es,bx,cx>
	or	ax,ax			; found the file?
	jnz	OpenFileFound		; brif so

OpenFileNotFound:			
	pop	si			; ES:SI = base file name pointer
	pop	es			
	;put up a message, and re-try the open if the user wants
	cCall	NoHelpFile,<ES,SI>
	cmp	ax,IDCANCEL		; cancel?
	jne	RetryOpen		; brif not -- try again
	or	HelpFlags,HLP_FAILFNF	; set failure flag
	xor	ax,ax			; set error flag
	jmp	SHORT OpenFileExit2	; exit

OpenFileFound:			; DX:AX = fully-qualified file name
	mov	si,dx			; SI:DI = * fully-qualified filename
	xchg	di,ax			

	push	di			; save register
	mov	es,si			; ES:DI = * string
	xor	ax, ax			
	mov	cx, 0ffffH		
	repne scasb			
	neg	cx			; cx = 2 greater than length
	pop	di			
	dec	cx			; cx = 1 greater than length
					; (room for name and NULL)
	cCall	HelpAlloc,<cx>		; allocate CX bytes, AX = handle
	or	ax,ax			; error? (failure flag will be set)
	je	OpenFileExit		; brif so -- exit with ax = 0
				; AX is now a "virtual file handle"

	push	ax			; save virtual file handle
	xchg	bx,ax			; bx = memory handle
	xor	ax,ax			; [bx]:ax = filename address
	cCall	fstrcpy2,<[bx],ax,si,di> ; copy filename to buffer
	pop	ax			; AX = virtual file handle

OpenFileExit:				; exit with AX = file handle
	pop	bx			; discard file name pointer
	pop	bx			
OpenFileExit2:				
	and	HelpFlags,NOT HLP_INHELP ; clear recursion lock
	pop	iHelpId			; restore (possibly) trashed help id

cEnd

;Split out of OpenFileOnPath with [14]
;***
;NoHelpFile - Inform user that a help file does not exit
;
;Purpose:
;
;Entry:
;	fpszFilename - far pointer to zero terminated file name
;
;Exit:
;	AX = IDCANCEL or IDRETRY
;
;Uses:
;	Per C Convention
;****

cProc	NoHelpFile,<NEAR,PUBLIC>,<SI,DI>	
parmD	fpszFilename
localV	npszLine2,80			; Order is important!
localV	npszLine1,80			
cBegin
	lea	ax,npszLine1		; pointer to first line
	push	ax			; buffer to append to
	PUSHI	ax,MSG_NoHelp		; Message number to append
	cCall	AppendBuf		; put message in npszLine1
	xchg	di,ax			; DI = ptr to next free byte in buffer

	; copy LONG string to stack (uses both stack buffers)
	cCall	fstrcpy2,<ds, di, seg_fpszFilename, off_fpszFilename>	

	push	uierr			; save old uierr
	mov	uierr,0			; clear it so we can check later
					; (FileSpec can set it)

	cCall	FileSpec,<di>		; AX = * name portion (upper-cased)

	xchg	si,ax			; SI = *name portion of parsed name

	cmp	uierr,0			; got error? (like Disk Not Ready)
	pop	uierr			; restore old uierr
	jz	UseParsedName		; brif not -- use parsed name

		; restore original name trashed by FileSpec that failed
	cCall	fstrcpy2,<ds, di, seg_fpszFilename, off_fpszFilename>	
	mov	si,di			; just use incoming name

UseParsedName:				

	push	ds			; ES = DGROUP
	pop	es			

CopyLoop:			; copy name portion to first part of buffer
	lodsb				; get a byte
	stosb				; save it
	cmp	al,0			; 0 terminator?
	jnz	CopyLoop		; no, go for next byte
	dec	di			; back up over null

	push	di			; buffer to append to
	PUSHI	ax,MSG_NoHelp1		; Message number to append
	cCall	AppendBuf		; put message onto end of npszLine1
	xchg	ax,si			; si = ptr to end of buffer
	mov	Byte Ptr [si],0 	; zero terminate buffer

	lea	di,npszLine2		; get pointer to second line
	push	di			; buffer to append to
	mov	ax,MSG_NoQHelp2 	; Assume QHELP help file
	test	[cmdSwitches],CMD_SW_QHELP ; /QHELP help file?
	jnz	nhf10			;  YES, go get it
	mov	ax,MSG_NoHelp2		;  NO, EDIT/QBASIC help file

nhf10:
	push	ax
	cCall	AppendBuf		; put message into npszLine2
	xchg	ax,si			; si = ptr to end of buffer
	mov	Byte PTr [si],0 	; zero terminate buffer

	;; Do third line of text

	mov	ax,MSG_NoQHelp3 	; Assume QHELP help file
	test	[cmdSwitches],CMD_SW_QHELP ; /QHELP help file?
	jnz	nhf20			;  YES, go get it
	mov	ax,MSG_NoHelp3		;  NO, EDIT/QBASIC help file

nhf20:
	push	ax
	cCall	ListStdMsg		; fill it in

	mov	ax,OFFSET DGROUP:BufStdMsg ; get a ptr to the source
	sub	sp,CB_bufStdMsg 	; create buffer on stack
					; note: this can not be done
					; in the prolog, as we may not
					; have stack space for it and
					; the call to FileSpec
	mov	si,sp			; SI = ptr to temporary buffer
	cCall	fstrcpy2,<ds, si, ds, ax>; copy from BufStdMsg to our buffer
	lea	ax,npszLine1		; pointer to first line
	push	ax			; first parameter to MessageBox
	push	di			; ptr to second line of text
	push	si			; ptr to third line of text
	mov	ax, MB_OK	        ; Just OK button if QHELP mode
	test	[cmdSwitches],CMD_SW_QHELP ; /QHELP help file?
	jnz	nhf30			;  YES, go get it
	mov	ax, MB_RETRYCANCEL	; RETRY and a CANCEL button
nhf30:
        push    ax
					; (Help button won't be put on)
	cCall	UIMessageBox		; display the message box
	add	sp,CB_bufStdMsg 	; remove temporary buffer

	test	[cmdSwitches],CMD_SW_QHELP      ; return cancel if QHELP mode
	jz	nhf40			
        mov     ax, IDCANCEL
nhf40:
cEnd



	page

;****
;ValidateFile ()
;
;Purpose:
;
;	Added with revision [15].
;
;	Obtains and validates filenames from dialogs before COW gets its
;	fingers on the name.  Validates the file, and puts up an error
;	msgbox if it is incorrect.  Adds the appropriate extension.
;
;	By checking things now, we avoid problems of switching logical
;	drives, int 24 errors, etc.
;
;Input:
;	None
;
;Output:
;	AX =	TRUE if name is proper
;		FALSE if name isn't
;Uses:
;	Per convention
;
;****

cProc	ValidateFile,<PUBLIC,NEAR>,<SI,DI>
cBegin

TEMP_BUF_SIZE = (FILNAML+1) AND 0FFFEh	; max length of name, made even
isFileTypeModule = 0			; Constant only available in C for now

	push	iHelpId			; save dialog box help id (trashed
					; when we put up a message box)
TryAgain:				
	mov	ax,TEMP_BUF_SIZE	; ax = max length of name
	sub	sp,ax			; sp = *temp buffer
	mov	si,sp			; si = *temp buffer (on stack)
	mov	bx,tmcSzFileName	; bx = file name tmc
	cCall	GetTmcText,<bx, si, ax>	; get filename into temp buffer

	cCall	GetCurDrive2		; AL = current drive letter
	push	ax			; save for later
	cmp	byte ptr [si],0		; null filename?
	je	NotDirName		; brif so -- don't try to CD

	cCall	ChangeDir2		; try to change directory to the
					; entire pathname.
	mov	b$PN_NAME,OFFSET DGROUP:szWildDefault ; default to "*.bas"
	or	ax,ax			; success? (file is directory name)
	jz	ValidateDone		; brif so -- put up "*.bas"

NotDirName:				
	mov	di,OFFSET DGROUP:b$BAS_EXT ; di = * default extension to add

	cCall	CbSzUi,<si>		; ax = # chars in filename

	cCall	ChangeDir,<si,ax,di>	; parse pathname into b$Buf2, and
					; change current drive and directory
					; to that of the pathname

ValidateDone:				
	pop	bx			; bl = original drive letter
	add	sp,TEMP_BUF_SIZE	; trash temporay space now, before
					; we need more stack (msgbox, help)

	or	ax,ax			; any errors?
	jnz	GotError		; brif so -- display a message box

	mov	dx,b$PN_NAME		; dx = *new name portion
	mov	ax,ER_IFN		; prepare to give "bad file name"
	cmp	fFileCreate,0		; File/Create?
	jz	NoError			; brif not -- we're ok
	cmp	dx,OFFSET DGROUP:szWildDefault ; trying to create directory?
	jne	NoError			; brif not -- we're ok
GotError:				; AX = error code
					; BL = original drive letter
	xchg	si,ax			; SI = error code (save across call)
	cCall	SetCurDrive2,<bx>	; restore original drive

	cCall	RetryError,<si>		; give them a chance to retry
	cmp	ax,IDRETRY		; retryable error, and <RETRY> hit?
	je	TryAgain		; brif so -- try again
	cmp	ax,IDCANCEL		; retryable error, and <CANCEL>?
	je	ErrorExit		; brif so -- exit

	mov	bx,MB_OK		; just put up an OK box
	cCall	MsgBoxStd,<bx,si>	; display the message box
	
ErrorExit:				
	xor	ax,ax			; exit with AX = FALSE
	jmp	SHORT ValidateExit

NoError:				
	mov	ax,tmcSzFileName	; ax = filename tmc
	cCall	SetTmcText,<ax, dx>	;  write out name to COW, (converted
					; to upper case, truncated)
	mov	al,1			; exit with AX = TRUE

ValidateExit:				; AX = TRUE ==> success, FALSE ==> fail
	pop	iHelpId			; restore dialog box help id
DbAssertRel	uierr,e,0,UI,<ValidateFile: uierr not 0 on exit>	
cEnd

;****
;ChangeDir (pbPath, cbPath, szExt)
;
;Purpose:
;
;	Added with revision [15].
;
;	Validates a file spec, and changes the current drive and directory
;	to that of the give file.
;
;Input:
;	pbPath = pathname to validate (not necessarily null-terminated)
;	cbPath = size of pbPath
;	szExt   = extension to add if none present
;
;Output:
;	AX = error code, or 0 if no error
;	if no error:	b$Buf2 contains parsed file name
;			b$PN_NAME points to name portion of file name
;			current drive and directory set to that of pbPath
;Uses:
;	Per convention
;
;****
cProc	ChangeDir,<NEAR,PUBLIC>,<si,di>
	parmW	pbPath
	parmW	cbPath
	ParmW	szExt
cBegin

	mov	si,pbPath		; si = *pathname
	mov	ax,MSG_MustSpecifyName	; assume null filename
	cmp	byte ptr [si],0		; null filename?
	jz	ChangeDirExit		; brif so -- "must specify name" error

DbAssertRel	uierr,e,0,UI,<ChangeDir: uierr not 0 on entry>
	push	si			; parm #1, path
	push	cbPath			; parm #2, size of path
	mov	si,OFFSET DGROUP:b$Buf2	; si = *dest buffer (for later)
	push	si			; parm #3, dest buffer
	push	szExt			; parm #4, extension
	cCall	NormFileNameFar		; parse path into b$Buf2
					; AX = -1 ==> failure (uierr = error)
	inc	ax			; failure?
	mov	ax,0			; prepare to clear uierr
	xchg	ax,uierr		; clear uierr, ax = error code
	jz	ChangeDirExit		; brif failure

;NOTE: this section WILL work for KANJI after all!  The pathname validation code
; only accepts single-byte slashes, and sets b$PN_NAME to point after the last
; slash it sees.  So it is safe to back up over the backslash.
	mov	di,b$PN_NAME		; di = *first char of name
	push	di			; save for later
	cmp	di,(OFFSET DGROUP:b$Buf2) + 3	; is file in root directory?
	je	RootDir			; brif so - don't back up over '\'
	dec	di			; back up over the '\'

RootDir:
	push	[di]			; save char
	mov	byte ptr [di],0		; chop off name portion

	cCall	ChangeDir2		; try to change directory to the path
					; portion of the pathname in SI (b$Buf2)
					; AX = error code, or 0 if success

	pop	[di]			; restore '\' in name
	pop	di			; di = b$PN_NAME
	cmp	byte ptr [di],0		; no name portion? (directory name
					; ending with a '\')
	jnz	ChangeDirExit		; brif not -- b$PN_NAME is right

	mov	b$PN_NAME,OFFSET DGROUP:szWildDefault ; default to "*.bas"

ChangeDirExit:
cEnd

;****
;ChangeDir2 ()
;
;Purpose:
;
;	Added with revision [17].
;
;	Attempts to change the current drive and directory.
;
;Input:
;	si = *pathname to change directory to
;
;Output:
;	AX = error code, or 0 if no error
;	if no error, current drive and directory set.
;	NOTE: DRIVE MAY BE SET EVEN THOUGH ERROR OCCURRED SETTING DIRECTORY
;
;Uses:
;	Per convention
;
;****
cProc	ChangeDir2,<NEAR>
cBegin
	DbAssertRelB	[si],ne,0,UI,<ChangeDir2: Null filename>

	DbAssertRel	b$fInt24Err,ne,0,UI,<ValidateFile: Int 24 error hooked>
	cCall	HookInt24		; reset int 24 flag for later check

	cCall	SetCurDir2,<si>		; set the current drive & directory

	push	ax			; save SetCurDir2 error code
	cCall	fInt24Err		; check for int 24's first, ax = error
					; code, or 0 if none
	pop	bx			; bx = SetCurDir2 error code
	or	ax,ax			; int 24 error?
	jnz	ChangeDir2Exit		; brif so -- exit

	inc	bx			; BX = -1 ==> failure
	jnz	ChangeDir2Exit		; brif no error -- exit with ax = 0
	mov	ax,ER_PNF		; give "path not found" error
ChangeDir2Exit:
cEnd


;****
;ValidatePath (pbPath, cbPath, szExt)
;
;Purpose:
;
;	Added with revision [25].
;
;	Checks that a pathame is well-formed, then validates the path portion
;	of the name by CD'ing to it.  Restores original drive/dir when done.
;
;	Does NOT reject directory names.
;	Does NOT reject wild cards.
;
;Input:
;	pbPath = pathname to validate (not necessarily null-terminated)
;	cbPath = size of pbPath
;	szExt   = extension to add if none present
;
;Output:
;	uierr = error code, or 0 if no error
;	If no error, validated filename stored in b$Buf2
;
;Uses:
;	Per convention, plus b$Buf1 & b$Buf2
;
;****
cProc	ValidatePath,<NEAR,PUBLIC>
parmW	pbPath
parmW	cbPath
parmW	szExt
cBegin
	DbChk	HoldBuf1		; uses b$Buf1 and b$Buf2
	DbChk	HoldBuf2

	mov	ax,offset DGROUP:b$Buf1	; where to save current drive/dir
	push	ax			; save addr for later
	cCall	GetCurDriveDir,<ax>	; save current drive/dir in b$Buf1

	cCall	ChangeDir,<pbPath,cbPath,szExt>	; validate the file, and
					; (possibly) change directory.  Store
					; fully-qualified pathname in b$Buf2

	cCall	SetUiErrCond,<ax>	; set uierr = error code, if
					; error code non-zero and no
					; existing error

	pop	bx			; BX = &b$Buf1
	push	ax			; save error code
	cCall	SetCurDir2,<bx>		; restore current drive/dir from b$Buf1
	pop	ax			; return AX = error code, or 0 if OK

	DbChk	FreeBuf1		; done with b$Buf1 and b$Buf2
	DbChk	FreeBuf2

cEnd

sEnd	UI				

end
