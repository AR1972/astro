	TITLE	hengine.asm - utilities for new help engine.
;*** 
;hengine.asm
;
;	Copyright <C> 1985-1988, Microsoft Corporation
;
;Purpose:
;	Callbacks and utilities for new help engine.
;
;	    Contains:
;		HelpAlloc
;		HelpDealloc
;		HelpLock
;		HelpUnlock
;		ReadHelpFile
;		CloseFile
;
;	    Support routines located elsewhere:
;		OpenFileOnPath (uipaths.asm)
;
;
;*******************************************************************************

	.xlist
	include	version.inc
	.list
	HENGINE_ASM = ON


	include cw/version.inc
	include cw/windows.inc
	include cw/edityp.inc

	IncludeOnce qbimsgs		
	IncludeOnce heap
	IncludeOnce pointers
	IncludeOnce array
	IncludeOnce uiint

assumes DS,DATA
assumes ES,DATA
assumes SS,DATA

	subttl	DATA segment definitions.
	page

sBegin	DATA
	globalW	curHelpFile,0		; REAL file handle of currently-open
					; help file.  0 if none open.
	externB fHelpAlloc		
	externB HelpFlags		
	externW iHelpId			
	externB fHelpAlloc		
sEnd	DATA

externFP GlobalAlloc			
externFP GlobalFree			
externFP OpenCheckDrive			

sBegin	UI
assumes CS,UI

	subttl	Help engine callbacks
	page

externNP RetryError			
externNP CloseFileNear			
externNP NoHelpFile			

;*** 
;HelpAlloc -- Help engine callback.
;
;Purpose:
;	Allocates a block of memory for the help engine.
;
;Entry:
;	cbMem = number of bytes to allocate
;
;Exit:
;	AX = handle to memory block, or NULL if memory could not be allocated.
;
;	NOTE:  Only help callback allowed to cause heap movement
;Uses:
;	Per convention
;
;Exceptions:
;	Returns NULL if OOM
;*******************************************************************************
cProc	HelpAlloc,<FAR,PUBLIC>
parmW	cbMem
cBegin

DbAssertRelB	fHelpAlloc,ne,0,UI,<HelpAlloc:fHelpAlloc not set>

	mov	cx,[cbMem]		; CX = # bytes to allocate
	jcxz	AllocExit		; brif 0 bytes requested -- return NULL

	xor	dx,dx			; DX:CX = # to allocate
	cCall	GlobalAlloc,<dx,dx,cx>	; AX = handle to memory
	xchg	cx,ax			; CX = handle


	or	cx,cx			; did allocation succeed?
	jnz	AllocExit		; yes, exit
	or	HelpFlags,HLP_FAILOOM	; indicate We failed due to OOM
	cCall	GiveHelpOOM		; register OOM error


AllocExit:				; CX = handle, or 0 if unsuccessful
	xchg	ax,cx			; return AX = handle

DbAssertRel	ax,ne,-1,UI,<HelpAlloc:Handle is invalid>
cEnd


;*** 
;HelpDealloc, CloseFile -- Help engine callbacks.
;
;Purpose:
;	Deallocates a block of memory previously allocated by HelpAlloc.
;
;	If 'handle' is NULL, ignores the call.
;
;	CloseFile only needs to deallocate the filename buffer pointed to
;	by the virtual file handle (which is just a memory handle).
;
;Entry:
;	handle = handle of block to deallocate
;
;Exit:
;	None
;
;	NOTE:  CANNOT cause heap movement
;Uses:
;	Per convention
;
;Exceptions:
;	None
;*******************************************************************************
labelFP <PUBLIC, CloseFile>		; TRICKY! just close current help
					; file and do a dealloc
	xor	cx,cx			; prepare to zero and test
	xchg	cx,[curHelpFile]	; CX = real file handle
	jcxz	@F			; brif not open -- nothing to do
	cCall	CloseFileNear,<cx>	; close the file
	call	fInt24Err		; ignore any int 24 errors that may
					; have occurred, reset b$fint24err,
					; and do a disk reset
@@:					

cProc	HelpDealloc,<FAR,PUBLIC>
parmW	handle
cBegin

	DbHeapMoveOff			; callback can't cause movement

	mov	bx,[handle]		; BX = handle of memory to dealloc
	or	bx,bx			; is the handle NULL?
	jz	DeallocExit		; brif NULL -- we're done
	cmp	WORD PTR [bx],NOT_OWNER ; has it already been deallocated?
	je	DeallocExit		; brif so -- we're done

	cCall	GlobalFree,<BX> 	; deallocate the memory


DeallocExit:

	DbHeapMoveOn			; heap movement ok now
cEnd


;*** 
;HelpLock -- Help engine callback.
;
;Purpose:
;	Locks a block of memory for the help engine.
;
;Entry:
;	handle = handle of block to lock
;
;	Supposed to lock down the heap entry.  If 'handle' is NULL, returns
;	(char far *)0.  We don't have to really lock the blocks, so this is
;	just a dereferencing function.
;
;Exit:
;	DX:AX = far address of locked memory block
;
;	NOTE:  CANNOT cause heap movement
;Uses:
;	Per convention
;
;Exceptions:
;	None
;*******************************************************************************
cProc	HelpLock,<FAR,PUBLIC>
parmW	handle
cBegin

	; we don't really need to lock/unlock the blocks, since the help
	; engine doesn't hold any locks between its calls

	mov	dx,[handle]			; BX = handle
	or	dx,dx				; NULL handle?
	jz	NullHandle			; brif so -- return 0
	inc	dx				; is it -1?
	jz	NullHandle			; yes, treat as if 0
	dec	dx				; restore DX
	mov	bx,dx				; BX = handle
	mov	dx,[bx]				; DX:AX = block addr (offset 0)
NullHandle:
	xor	ax,ax
cEnd



;*** 
;HelpUnlock -- Help engine callback.
;
;Purpose:
;	Unlocks a block of memory previously locked by HelpLock.
;
;	Supposed to unlock the heap entry.  If 'handle' is NULL, ignores
;	the call.  NOP for us, since we don't really do locks.
;
;Entry:
;	handle = handle of block to unlock
;
;Exit:
;	None
;
;	NOTE:  CANNOT cause heap movement
;Uses:
;	Per convention
;
;Exceptions:
;	None
;*******************************************************************************
cProc	HelpUnlock,<FAR,PUBLIC>
parmW	handle
cBegin
	; we don't really need to lock/unlock the blocks, since the help
	; engine doesn't hold any locks between its calls
cEnd


;*** 
;ReadHelpFile -- Help engine callback.
;
;Purpose:
;	Reads data from a file previously opened by OpenFileOnPath
;
;Entry:
;	vfhandle = virtual file handle of file to read
;	fpos    = DWORD byte position to read from
;	pData   = where to put the data read
;	cbBytes = # bytes to read
;
;Exit:
;	DX:AX = #bytes actually read if cbBytes != 0
;	DX:AX = file size if cbBytes == 0
;
;	NOTE:  CANNOT cause heap movement
;Uses:
;	Per convention
;
;Exceptions:
;	None
;*******************************************************************************
cProc	ReadHelpFile,<FAR,PUBLIC>
parmW	vfhandle
parmD	fpos
parmD	pData
parmW	cbBytes
cBegin

	DbHeapMoveOff				; callback can't cause movement
DbAssertTst	HelpFlags,z,HLP_INHELP,UI,<ReadHelpFile: Recursion lock set>
	or	HelpFlags,HLP_INHELP		; set recursion lock

	push	b$fInt24Err			; save old int 24 state
	cCall	HookInt24			; ignore int 24 errors, and
						; return error in b$fInt24Err

RetryRead:					
	mov	bx,vfhandle			; BX = virtual file handle
	cCall	ObtainHelpHandle		; AX = real help file handle
	or	ax,ax				; error?
	je	ErrorExit			; brif so -- return 0
	xchg	bx,ax				; BX = real file handle

	mov	cx,cbBytes			; return file size if
	jcxz	ReturnFileSize			;   (cbBytes == 0)
	push	cx				; save for later

	;seek fpos bytes from start in file vfhandle

	mov	dx,word ptr (fpos)		; CX:DX = where to read
	mov	cx,word ptr (fpos+2)
	mov	ax,4200h			; seek from start
	int	21h				; DX:AX = new offset from start

	pop	cx				; CX = #bytes to read
	jc	CheckRetry			; brif error

	;read cbBytes of data into pData from file vfhandle

	push	ds				; save DS
	lds	dx,pData
	mov	ah,3fh				; read CX bytes into DS:DX
	int	21h				; AX = #bytes read
	pop	ds				; restore DS
	jnc	NoError				; brif no error -- exit

CheckRetry:				; handle errors
	cCall	fInt24Err			; AX = error code (or 0)
						; resets b$fInt24Err and
						; does a disk reset
	cCall	RetryError,<ax>			; put up a box asking if
						; we want to retry IF we
						; got an error, and IF we
						; can retry on this error
	cmp	ax,IDRETRY			; was <RETRY> button hit?
	je	RetryRead			; brif so -- try again

ErrorExit:					
	xor	ax,ax				; return NULL (error)
	or	HelpFlags,HLP_FAILFNF		
						; fall through to clear DX,
						; and exit

NoError:
	cmp	b$fInt24Err,UNDEFINED		; did we get an int24 error?
	jne	CheckRetry			; brif so
	xor	dx,dx				; DX:AX = result
	jmp	SHORT ReadExit			

ReturnFileSize:
	xor	dx,dx				; DX:CX = 0
	mov	ax,4202h			; seek from end
	int	21h				; DX:AX = file size
	jc	CheckRetry			; brif error

ReadExit:
	pop	b$fInt24Err			; restore old int 24 state
	and	HelpFlags,NOT HLP_INHELP	; release recursion lock
	DbHeapMoveOn			; heap movement ok now
cEnd

;added with revision [9]
; Guide to VIRTUAL FILE HANDLES in the help system
;
;	Virtual file handles were added to solve 2 major problems:
;	1. Up to 3 system file handles were being used by the help system,
;	   making these unavailable at execution time.
;	2. The users could switch diskettes while one of the help files was
;	   open, causing havoc.  There is no easy way to check that the file
;	   has not been switched since the last access. 
;
;	The value used for the virtual file handle is a pointer to the segment
;	field of a BDL allocated by GlobalAlloc (through HelpAlloc).
;	The BDL contains the fully-qualified pathame of the help file.
;
;	The filename is determined once, in OpenFileOnPath, and placed in this
;	Buffer.  CloseFile has to only delete this buffer.  ObtainHelpHandle
;	returns the real handle if file aleady open, or opens the file and
;	saves the real handle for future lookup.  CloseCurHelpFile closes
;	the currently open help file, if one is open.  CloseCurHelpFile should
;	be called before any user input is possible, and before any other
;	help files can be accessed.


;added with revision [9]
;*** 
;ObtainHelpHandle -- Map virtual handle to real handle
;
;Purpose:
;	If help file isn't already open, open it.  The true file handle is kept
;	in curHelpFile.  The fully-qualified help file name is kept in a BDL,
;	the segment of which is pointed to by the virtual file handle.
;
;Entry:
;	BX = virtual file handle
;
;Exit:
;	AX = file handle, or 0 if error
;
;Uses:
;	Per convention
;
;Exceptions:
;	Calls NoHelpFile if open unsuccessful.
;*******************************************************************************
cProc	ObtainHelpHandle,<NEAR>
cBegin
	push	iHelpId			; save across possible MsgBox
DbAssertRel b$fInt24Err,e,-1,UI,<ObtainHelpHandle: int 24 on entry>

	mov	ax,curHelpFile		; AX = handle of currently-open help
					; file
	or	ax,ax			; help file already open?
	jnz	HandleExit		; brif so -- return this handle

	mov	es,[bx]			; ES = filename segment

RetryOpen:
	cCall	OpenCheckDrive,<es:[0]> ; make sure we don't
					; have to switch diskettes
					; PRESERVES ALL REGISTERS
	push	ds			; save segment
	push	es			; DS = filename segment
	pop	ds
	xor	dx,dx			; DS:DX = * to filename
	mov	ax,3d00h		; open for read access
	int	21h			; AX = file handle
	pop	ds			; restore DS = DGROUP
	jnc	SaveHandleExit		; brif no error
	
;	cmp	al,4			; no file handles left?
;	jne	NotErrNoHandles		; brif not
;   PUSHI	ER_TMF			; give "too many files" msgbox later
;	call	SetUiErr		
;	jmp	SHORT NoHandleExit	; exit with error

NotErrNoHandles:			
	push	es			; save filename segment for later
	push	es			; NoHelpFile parms
	push	dx			; (fInt24Err trashes DX)
	cCall	fInt24Err		; ignore any int 24 errors that may
					; have occurred, reset b$fInt24err,
					; and do a disk reset
	cCall	NoHelpFile		; issue a message to the user,
					; giving them a chance to retry
					; NOTE: PARMS pushed above
	pop	es			; restore filename segment
	cmp	ax,IDRETRY		; retry specified?
	je	RetryOpen		; brif so -- try again

NoHandleExit:				
	xor	ax,ax			; AX = 0 ==> error

SaveHandleExit:
DbAssertRel b$fInt24Err,e,-1,UI,<ObtainHelpHandle: open worked, but got int 24>
	mov	curHelpFile,ax		; save file handle

HandleExit:
	pop	iHelpId			; restore (possibly) trashed iHelpId
cEnd

;added with revision [9]
;*** 
;CloseCurHelpFile -- Close current open help file, if any.
;
;Purpose:
;	Close the currently-open help file, if any.  This frees up a file
;	handle, and eliminates the problem of the user switching disks
;	while the help file is open.
;
;Entry:
;	curHelpFile = REAL file handle of currently-open help file (0 if none)
;
;Exit:
;	None.
;
;Uses:
;	BX,CX,ES
;
;Preserves:
;	AX,DX
;
;Exceptions:
;	None
;*******************************************************************************
cProc	CloseCurHelpFile,<NEAR,PUBLIC>,<DX,AX>	; save caller's return values
cBegin
	xor	cx,cx			; prepare to zero & test
	xchg	cx,[curHelpFile]	; CX = real file handle
	jcxz	CloseExit		; brif not open -- nothing to do

	cCall	CloseFileNear,<cx>	; close the file
	cCall	fInt24Err		; ignore any int 24 errors that may
					; have occurred, reset b$fInt24err,
					; and do a disk reset

CloseExit:
cEnd

sEnd	UI


	end
