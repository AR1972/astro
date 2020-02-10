;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This file has most of the initialization code. All the procedures in this  ;
; file are needed only during the initialization phase and are defined in a  ;
; main code segment. WOA will ignore this segment when it does swapping etc  ;
; as all of the code in this segment id discardable in any case.             ;
;                                                                            ;
; History:								     ;
;									     ;
;	 Fri June-15-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 Adapted for the Dos Task Switcher.				     ;
;									     ;
;        Thu May-03-1990.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created for Windows. (Added the History legend)    		     ;
;----------------------------------------------------------------------------;

	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include macros.mac
	include	njmp.mac
	include woaerr.inc
	include woakeys.inc
	include dosx.inc
	include	grabber.inc
	include woaswch.inc
	.list

	.8086				;must have to run on 8086s too.

;----------------------------------------------------------------------------;
; define the external function calls.					     ;
;----------------------------------------------------------------------------;


	;-------------------------------------------------------;
	; external FAR OLDAPP procedures.                       ;
	;-------------------------------------------------------;


;----------------------------------------------------------------------------;

sBegin	Data

	;-------------------------------------------------;
	; declare all external data seg variables.        ;
	;-------------------------------------------------;

externB		DataSegEnd		;end of _DATA segment
externB		EmsFlag			;EMS present or not.
externB		XmsFlag			;XMS present or not
externB		SwitcherColors		;colors for switcher screen
externB		WoaSwap1Path		;first swap path.
externB		WoaSwap2Path		;second swap path.
externW		Swap1MinK		;min size to be left on 1st path
externW		Swap2MinK		;min size to be left on 2nd path
externB		ErrorType		;WOA error type
externW		hApp			;window handle for this instance
externB		CurrentPathId		;swap path id for current AP.
externB		StartRestartId		;start or restart
externW		HighMemXmsHandle 	;handle of the high mem XMS block
externW 	StubSegSize   		;size of real mode stub segment
externW 	WoaCsSize		;size of protected mode code seg
externW 	WoaDsSize		;size of winoldap data segment
externD		XmsBankSize	    	;size of XMS to be allocated
externW		XmsHeapWalkSel		;scratch selector for walking xms heap
externW 	WoaStubSel		;sel for current loaction of stub code
externW		ArenaWalkSel	   	;temp selector for walkimg arena chain
externW		ArenaRWSel	   	;temp sel for reading/writing arenas
externW 	AppXmsBaseSel		;selector to base of apps XMS block
externB		DosAppSwapFileName	;app swap file name
externW 	DosAppNumber		;app number to be run in this instance
externB		FileTemplate		;for deleting files
externB		WoaSwapFilePrefix   	;'~WOA'
externB	 	DiskSwap1Drive	    	;swap drive index for drive 1
externB	 	DiskSwap2Drive	    	;swap drive index for drive 1
externD		lpXmsControl		;XMS control function address
externW		AppMinMem		;minimum memory required
externW		AppMinXmsK		;minimum XMS memory required
externW		AppMaxXmsK		;maximum xms required
externW		LowMemSel		;pointer to start of block
externB		LowMemBlockType		;type of the low mem block
externW		LowMemArenaSel		;pointer to its arena
externW		LowMemParaSize		;size of available low memory in para
externD		HighMemSize		;size of available high heap
externW		WoaPDBSeg		;WOA's PSP
externW		SegResizeBlock		;block to resize in real mode
externW		SizeReservedArea 	;size of area reserved area at heap st.
externW		UsableLowHeapSel	;sel/seg for reusable part of low heap
externW		SegAfterReservedArea	;segment after the reserved area
externW		StartScreenLines	;startup no of screen lines
externB		SwitcherID		;ID of this switcher (bits 14 & 15)

	;-------------------------------------------------------;
	; define any locally used constants.			;
	;-------------------------------------------------------;


	;-------------------------------------------------------;
	; define any external constants.       			;
	;-------------------------------------------------------;

		externA   IS_WINOLDAP_ACTIVE	;(WOAMGR.ASM)

	;------------------------------------------------------;
	; define public names.				       ;
	;------------------------------------------------------;

		public	GetSegmentLimits

sEnd	Data
;----------------------------------------------------------------------------;
; declare external FAR Winoldap procedures.				     ;
;----------------------------------------------------------------------------;

	externFP ErrorHandler			;(WOAMGR.ASM)
	externFP FarCopyBasicSwap1FileName     	;(WOAMGR.ASM)
	externFP FarCopyBasicSwap2FileName     	;(WOAMGR.ASM)
	externFP FarAppendUniqueNumber		;(WOAMGR.ASM)
	
;----------------------------------------------------------------------------;
; Declare the the code segment and any labels that we want to refernce in it;
;----------------------------------------------------------------------------;

	;-------------------------------------------------;
	; define external data references.		  ;
	;-------------------------------------------------;

sBegin	Code

	externB		CodeSegEnd		;end of _TEXT segment

sEnd 	Code

sBegin	StubSeg	

;----------------------------------------------------------------------------;
; declare external data refernces.					     ;
;----------------------------------------------------------------------------;

externB		WoaGrabberName		;(WOARLM.ASM)
externB		WoaGrabberSwapPath	;(WOARLM.ASM)
externB		WoaPath			;(WOARLM.ASM)
externB		WoaParams		;(WOARLM.ASM)
externB		WoaFcb1			;(WOARLM.ASM)
externB		WoaFcb2			;(WOARLM.ASM)
externB		Woa6fValue		;(WOARLM.ASM)
externB		WoaIrq9Global		;(WOARLM.ASM)
externB		WoaNetAsyncSwitching	;(WOARLM.ASM)
externW		WoaCpuType		;(WOARLM.ASM)
externW		WoaFileEntrySize	;(WOARLM.ASM)
externB		WoaBehavior		;(WOARLM.ASM)
externB		WoaHotKeys		;(WOARLM.ASM)
externW		WoaStartScreenLines	;(WOARLM.ASM)
externB		WoaAltTabDisabled	;(WOARLM.ASM)
externB		WoaShftAltTabDisabled	;(WOARLM.ASM)
externB		WoaAltEscDisabled	;(WOARLM.ASM)
externB		WoaShftAltEscDisabled	;(WOARLM.ASM)
externB		WoaCtrlEscDisabled	;(WOARLM.ASM)
externB		WoaSwitcherID		;(WOARLM.ASM)


sEnd	StubSeg
;----------------------------------------------------------------------------;

sBegin	StubSeg

; InitStartLine MUST be at the very start of the one time initialization code,
; it is the address which will be used to discard the one time initialization
; code after itis done.

	public	InitStartLine

InitStartLine	equ	$	;MAKE ME FIRST!  Everything after this will
				;  be discarded after initialization


externB XmsStartLine			;start of XMS handler in real mode stub
externB	StubSegEnd			;end of _WOARLMSEG segment

	;-------------------------------------------------;
	; define local entry points                       ;
	;-------------------------------------------------;


 	assumes	cs,StubSeg
	assumes	ds,Data
	assumes	es,nothing

;----------------------------------------------------------------------------;
; define temporary strings.						     ;
;----------------------------------------------------------------------------;

szEmsName		db	'EMMXXXX0'	;EMM device name

;----------------------------------------------------------------------------;
; define local constants.						     ;
;----------------------------------------------------------------------------;

CMD_LINE_PARAM_LEN	equ	2		;length of command line param
VALID_CMD_LINE_PARAM	equ	0FE20H		;valid parameter

;----------------------------------------------------------------------------;
; include the HELP text message. This is a separate file for international   ;
; conversions.								     ;
;----------------------------------------------------------------------------;

	include		woahelp.inc

;----------------------------------------------------------------------------;
; ParseCommandLine:							     ;
;									     ;
; DOSSWAP expects a special parameter from the DOSSHELL which tells it that  ;
; it is being run from the SHELL and not from a DOS prompt. This is actually ;
; a space followed by a special character which is kinda difficult to type   ;
; in from the keyboard (At this point it is 0FEH, 08H would probably have    ;
; been the best!). If we do not find this special parameter, we will print   ;
; out a help text and exit.						     ;
;----------------------------------------------------------------------------;
cProc	ParseCommandLine,<FAR,PASCAL,PUBLIC>

cBegin

; command line shoule be at DS:80H.

	mov	si,80h			;default DTA address
	lodsb				;get length of command line 
	cmp	al,CMD_LINE_PARAM_LEN	;is it the length that we expect ?
	jnz	PCL_PrintHelpText	;no, print help text and exit

; check the validity of the parameter.

	lodsw				;get the next two words
	.errnz	CMD_LINE_PARAM_LEN - 2
	cmp	ax,VALID_CMD_LINE_PARAM ;is it the one that we expect ?
	jz	PCL_Ret			;yes, carry is clear too.

PCL_PrintHelpText:

; we don't expect any parameters. Simply print an error message an exit

	push	ds			;save
	push	cs
	pop	ds			;ds points has stub seg
	mov	dx, StubSegOFFSET HelpText
	mov	ah,9
	int	21h			;print help message
	pop	ds			;restore
	stc				;error exit, but no error code

PCL_Ret:

cEnd
;----------------------------------------------------------------------------;
; InitializeWoa:							     ;
;									     ;
; This routine setus up some WOA variables by reafing SYSTEM.INI etc. This   ;
; code is executed only once and the advantage of having it in a separate    ;
; code segment is that we will ignore all the code in this segment after     ;
; the initializations are done.                                              ;
;----------------------------------------------------------------------------;

cProc	InitializeWoa,<FAR,PASCAL,PUBLIC>

	parmW	StartRestart		;start or restart

cBegin
	
;----------------------------------------------------------------------------;
; find out if EMS is present or not, setting flag appropriately.   	     ;
;----------------------------------------------------------------------------;

	mov	ax,3567h		;want to get INT 67 vector
	int	21h			;ES:BX has the vector
	mov	di,10			;want to inspect offset 10
	mov	si,StubSegOFFSET szEmsName ;DS:SI points to exepected name
	mov	cx,8			;need to compare 8 bytes
	cld				;set proper direction flag
	push	ds			;save
	smov	ds,cs			;load code
	repz	cmpsb		    	;compare names
	pop	ds			;restore
	jnz	@f			;names do not match, driver absent
	mov	EmsFlag,0ffh		;EMS driver prsent
@@:
;----------------------------------------------------------------------------;
; Parse the FCBS in the command line for the EXEC call.			     ;
;----------------------------------------------------------------------------;

	cmp	StartRestart,1			;if restart
	jz	@f				;skip this
	mov	si,StubSegOFFSET WoaParams+1	;point to the parameters
	mov	di,StubSegOFFSET Woafcb1 	;fill in the first FCB 
	smov	es,cs				;all in same segment
	mov	ax,2901h	
	int	21h				;PARSE DOS call
	call	find_next_arg			;looks for further args
	jnz	@f				;no second parameter
	mov	di,StubSegOFFSET Woafcb2
	mov	ax,2901h
	int	21h				;parse and fill in fcb2
@@:

; deal with memory requirements.

	cCall	AllocateXms		;allocate all of XMS if HIMEM is loaded

; check for extended memory requirements, if the requirements are not 
; satisfied, invoke the error handler and never return.

	cCall	CheckXmsRequirements	;XMS memory requirement tests

; now get the sizes of various segments etc.

; get the limit fields and save them

	call	GetSegmentLimits	;set segment limits -- sets StubSegSize
	cmp	StartRestart,0		;is it start ?
	jnz	@f			;yes

; now test to see if the applications conventional and extended mode memory 
; requirements can be satisfied or not. If memory requrements cannot be met, 
; the following routine will never return

	call	IsThereEnoughMemory	;check conventional/XMS mem requirements

@@:

	clc				;no error

cEnd
;----------------------------------------------------------------------------;
; IsThereEnoughMemory:							     ;
;									     ;
; This routine checks to see if enough memory can be released to meet the    ;
; apllications conventional.						     ;
;----------------------------------------------------------------------------;

IsThereEnoughMemory proc near

	xor	ax,ax			;initialize size of resident area

; now in the resident area of conventional memory the main code and data
; segments will allways be resident in real mode winoldap (this is not true for
; protected mode winoldap). So initialize the size of the real mode resident 
; area with the size of the main code and data segments.

	mov	ax,WoaCsSize		;size of the main code segment
	add	ax,WoaDsSize		;add in the size of the data segment

;----------------------------------------------------------------------------;
; code independent of real or protected mode.				     ;
;----------------------------------------------------------------------------;

; now check convetional memory requirements.

	mov	bx,AppMinMem		;get minimum req. in KB
	cmp	bx,0			;was minimum limit specified ?
	jz	ThereIsEnoughMemory	;memory requirements will be met

; get the size of the stub segment

	add	ax,StubSegSize		;size of the stub seg

; bx has the minimum memory required in Kbytes, we need to mutiply that by 64
; to get the number of paragraphs.

	shiftr	ax,4			;get number of paragraphs
	inc	ax			;round up
	shiftl	bx,6			;ax has min requirement in paragraphs

; if there is not enough memory to run the application, get out and complain

	neg	ax			;need to subtract from total memory
	add	ax,LowMemParaSize	;size of low heap area
	cmp	bx,ax			;is there enough memory
	jbe	ThereIsEnoughMemory	;yes there is

; we cannot run the appilaction

	mov	ErrorType,ER_LOW_MEMORY	;error type
	jmp	ErrorHandler		;get out

ThereIsEnoughMemory:

	ret

IsThereEnoughMemory  endp
;----------------------------------------------------------------------------;
; GetSegmentLimits:						             ;
;									     ;
; This routine gets the limits of the various winoldap segments. It also     ;
; obtains the sel values for the block allocated to the switcher by Dos and  ;
; also it's size and a sel for it's arena.				     ;
;----------------------------------------------------------------------------;

GetSegmentLimits  proc near

	push	es			;save
	mov	ax,CodeOFFSET CodeSegEnd;end label in _TEXT segment
	add	ax,15			;take into next para	
	and	ax,0fff0h		;paragraph allign
	mov	WoaCsSize,ax		;save it

	mov	ax,DataOFFSET DataSegEnd;end label in _DATA segment
	add	ax,15			;take into next para	
	and	ax,0fff0h		;paragraph allign
	mov	WoaDsSize,ax	

; if the XMS handler is not needed we will discard it.

	mov	bx,StubSegOFFSET InitStartLine	;discardable code line
	mov	ax,wptr XmsBankSize	;if there is an XMS requirement
	or	ax,wptr [XmsBankSize+2] ;  the XMS handler can't be
	jnz	@f
	mov	bx,StubSegOFFSET XmsStartLine	;discard XMS code
@@:
	add	bx,15			;take into next para	
	and	bx,0fff0h		;paragraph allign
	mov	StubSegSize,bx		;usable stub area

	pop	es			;restore es

	mov	ah,62h			;get PSP
	int	21h			;BX has the PSP segment
	mov	WoaPDBSeg,bx		;save it
	mov	LowMemSel,bx		;save it
	dec	bx			;point to the arena
	mov	LowMemArenaSel,bx	;save it
	push	es			;save
	mov	es,bx			;point to the arena
	mov	ax,es:[3]		;get the size of block in paragraphs
	mov	LowMemParaSize,ax	;save it
	mov	al,es:[0]		;get the block type
	mov	LowMemBlockType,al	;save it too
	pop	es			;restore

; save the sel for the block that will be resized.

	mov	ax,LowMemSel		;selector for the block
	mov	SegResizeBlock,ax	;save it.

; get the maximum of the 3 segments.

	mov	ax,_TEXT		;get code segment
	mov	bx,WoaCsSize		;and its size
	cmp	ax,DGROUP		;compare with DS seg
	ja	@f			;no need to swap
	mov	ax,DGROUP		;load ds
	mov	bx,WoaDsSize		;load ds's size
@@:
	cmp	ax,_WOARLMSEG		;comapare with stub
	ja	@f			;no need to swap
	mov	ax,_WOARLMSEG		;load stub seg
	mov	bx,StubSegSize		;and its size
@@:
	shiftr	bx,4			;convert to para
	add	ax,bx			;get to end of segment
	sub	ax,LowMemSel		;subtract the PSP seg
	mov	SizeReservedArea,ax	;size of reserved area
	add	ax,LowMemSel		;segment where stub seg will be loaded
	mov	SegAfterReservedArea,ax	;address of segment after reserved area
	mov	UsableLowHeapSel,ax	;seg for usable area


	ret

GetSegmentLimits endp
;----------------------------------------------------------------------------;
; CheckXmsRequirements:							     ;
;									     ;
; If no XMS memory is required by the application, a flag would be set to    ;
; indicate that.							     ;
;									     ;
; If XMS is required but no XMS driver is loaded we will exit and report the ;
; error.								     ;
;									     ;
; We will then find out details about the XMS block allocated for the high   ;
; heap and find out whether we can meet the oldapps requiremsts.	     ;
;----------------------------------------------------------------------------;

cProc	CheckXmsRequirements,<NEAR,PUBLIC,PASCAL>

cBegin


;----------------------------------------------------------------------------;
; real mode specific code. In real mode we need the address of the XMS contr-;
; -ol fuction, so get the address and save it.				     ;
;									     ;
; (note: this instance may not need any XMS memory, but this instance may    ;
;  later handle some old app which does need XMS and will then be needing    ;
;  the address of the control function).				     ;
;----------------------------------------------------------------------------;

	cmp	AppMinXmsK,0		;is there any requirement ?
	jnz	XmsMemoryDesired	;yes

; as no xms memory is required, set the XMS flag off.

	mov	wptr [XmsBankSize],0	;no XMS memory needed
	mov	wptr [XmsBankSize+2],0  ;reset hiword too
	jmp	short CheckXmsReqRet	;go back.

XmsMemoryDesired:

; test to see if XMS driver is loaded.

	mov	ErrorType,ER_NO_XMS_DRV ;anticipating no XMS driver
	cmp	XmsFlag,0ffh		;driver loaded ?
	njnz	NearErrorHandler	;no xms driver loaded

; XMS driver is loaded, check to see if we can support apps requirements.

	mov	cx,wptr [HighMemSize+2]
	mov	dx,wptr [HighMemSize]	;CX:DX has size of high heap block

; get the number of K bytes this amounts to.

	mov	dl,dh			;divide by 256
	mov	dh,cl			
	mov	cl,ch			
	xor	ch,ch			;we have divided by 256, now by 4
	shr	cx,1			;get lsb into carry
	rcr	dx,1			;get it into dx
	shr	cx,1			;get lsb into carry
	rcr	dx,1			;CX:DX has mem available in K
	or	cx,cx			;if cx > 0 we have enough
	jnz	GrantXmsMemory		;enough is available

	mov	ErrorType,ER_LOW_XMS_MEM;anticipate not enough memory
	cmp	AppMinXmsK,-1		;all of memory needed ?
	jz	AllOfXmsNeeded		;yes
	cmp	dx,AppMinXmsK		;compare with min requirement
	njb	NearErrorHandler	;not enough for minimum requirement

GrantXmsMemory:

; we have made sure that minimum requirement is met, but we must try to grant
; the maximum requirement.

	mov	ax,AppMaxXmsK		;this is what we want to give
	cmp	ax,-1			;do we need all ?
	jz	AllOfXmsNeeded		;yes
	cmp	dx,AppMaxXmsK		;compare with max requirement.
	jbe	AllOfXmsNeeded		;availabe is < desired, grant all
	jmp	short SetXmsRequired	;set up size to be granted

AllOfXmsNeeded:

;----------------------------------------------------------------------------;
; In real mode, DX has the size of the XMS block rounded down to a K.	     ;
;----------------------------------------------------------------------------;

	mov	ax,dx			;all of XMS to be granted

;----------------------------------------------------------------------------;

SetXmsRequired:

	mov	bx,1024			;need to do Kilo to byte conversion
	mul	bx			;DX:AX has apps xms bank size

SaveXmsSize:

	mov	wptr [XmsBankSize+2],dx	;save high word
	mov	wptr [XmsBankSize],ax	;save low word

CheckXmsReqRet:

cEnd
;----------------------------------------------------------------------------;
; AllocateXms:								     ;
;									     ;
; This routine gets the information about the XMS block to use. The XMS block;
; handle is will be saved in the global switch structure by the first inst.  ;
; of the switcher (it would be 0 otherwise) and all other instances would use;
; it to get the size.							     ;
;----------------------------------------------------------------------------;

cProc	AllocateXms,<NEAR,PUBLIC,PASCAL>,<es,di>

cBegin

; check to see if XMS driver is installed or not.

	mov	XmsFlag,0		;assume no xms.
	mov	ax,4300h		;installation check
	int	2fh
	cmp	al,80h			;failure ?
	njne	AllocateXmsRet		;yes, no driver installed.
	mov	XmsFlag,0ffh		;XMS exists

; get the XMS entry point address.

	push	es			;will get thrashed
	mov	ax,4310h		;code to get the address
	int	2fh			;address in es:bx
	mov	wptr [lpXmsControl+2],es;save segment of control function
	mov	wptr [lpXmsControl],bx	;save the offset
	pop	es			;restore

; get a pointer to the global switch structure and find out whether a privious
; instance had allocated the block or not.

	mov	ax,4a05h		;opcode
	mov	si,CGET_GLOBAL_SWITCH_DATA
	int	2fh			;dx:ax has the long pointer
	mov	es,dx			;load it into es
	mov    	di,ax			;es:di -> info structure
	mov	dx,es:[di].XMS_Handle	;get the stored handle
	or	dx,dx			;if zero then not allocated.
	jz	DoAllocateXMS		;must allocate it.

; the XMS block has already been allocated, save the handle and get the size.

	mov	HighMemXmsHandle,dx	;save the handle
	mov	ax,wptr es:[di][0].XMS_Size
	mov	word ptr [HighMemSize],ax;save low word of size
	mov	ax,wptr es:[di][2].XMS_Size
	mov	word ptr [HighMemSize+2],ax;save high word
	jmp	short AllocateXMSRet	;done.

DoAllocateXMS:

; get the size of the largest available block.

	mov	ah,08h			;query amt. of free memory
	call	lpXmsControl		;AX has largest free block size in K
	cmp	bl,80h			;error ?
	jz	AllocateXmsRet		;no XMS available
	cmp	bl,81h			;error ?
	jz	AllocateXmsRet		;no XMS available

; allocate the block and get it's handle.

	push	ax			;save the size of the largest block.
	mov	dx,ax			;requested size = size of largest block
	mov	ah,09h			;allocate XMS block
	call	lpXmsControl		;AX has largest free block size in K
	or	ax,ax			;failed ?
	pop	ax			;get back size
	jz	AllocateXmsRet		;allocation failed.

; save the handle and the size of the block.

	mov	HighMemXmsHandle,dx	;save the handle
	mov	bx,1024			;need size in KB
	xor	dx,dx			;clear out before mul
	mul	bx			;dx:ax has size in bytes
	mov	word ptr [HighMemSize],ax;save low word of size
	mov	word ptr [HighMemSize+2],dx;save high word

; now save the block information in the global structure.

	mov	wptr es:[di][0].XMS_Size,ax
	mov	wptr es:[di][2].XMS_Size,dx
	mov	dx,HighMemXmsHandle	;get the handle
	mov	es:[di].XMS_Handle,dx	;save it

; finally lock the block.

	mov	ah,0ch			;lock XMS block
	call	lpXmsControl		;AX has largest free block size in K

AllocateXmsRet:

cEnd
;----------------------------------------------------------------------------;
; GetSizeFromArena:							     ;
; 									     ;
; This routine will be assembled only in real mode and it looks into the     ;
; arena associated with a segment whose values in in ax and returns the size ;
; in bytes in AX.							     ;
;----------------------------------------------------------------------------;


GetSizeFromArena  proc  near

	dec	ax			;get to the arena
	mov	es,ax			;have es pointing to arena
	mov	ax,es:[3]		;get the size in paragraphs
	shiftl	ax,4			;get it in bytes
	ret

GetSizeFromArena endp

;----------------------------------------------------------------------------;
; GetSwapFilePathPrefix:						     ;
;									     ;
; This routine gets the path prefix for the swap files to be used. 	     ;
;								             ;
;	. If the [NowWindowsAppf] section in SYSTEM.INI has an entry specifi-;
;         -ed for the key, 'swapdisk=', then that will be the path for temp  ;
;         files,else						             ;
;	. the 'GetTempFileName' function is invoked to get the complete      ;
;	  path name of the temp drive and part of the name upto the unique   ;
;         number is extracted from it.					     ;
;----------------------------------------------------------------------------;

GetSwapFilePathPrefix  proc  near

	cld				;do not take chances with this

; copy the paths. First the first swap path

	mov	si,DataOFFSET WoaSwapFilePrefix
	mov	di,DataOFFSET WoaSwap1Path
	call	AppendDsSiToDsDi

; now the second swap path.

	mov	si,DataOFFSET WoaSwapFilePrefix
	mov	di,DataOFFSET WoaSwap2Path
	call	AppendDsSiToDsDi

; prepare the grabber swap file name from the second swap path

	mov	di,DataOFFSET DosAppSwapFileName;prepare name here
	call	FarCopyBasicSwap2FileName;copy the name until the unique num
	mov	ax,0ffeeh		;irrelevant, grabber will redo this
	call	FarAppendUniqueNumber	;just have a win style file name
	mov	si,DataOFFSET DosAppSwapFileName
	mov	di,StubSegOFFSET WoaGrabberSwapPath
	push	es			;save
	smov	es,ds			;es:si ppints to file name
	call	CopyEsSiToCsDi		;copy over to the stubsegment
	pop	es

	ret

GetSwapFilePathPrefix endp
;----------------------------------------------------------------------------;
; AppendDsSiToDsDi:							     ;
;									     ;
; Appends the strung at DS:SI to the string at ES:DI and makes sure that the ;
; composite string end with a '\'					     ;
;----------------------------------------------------------------------------;

AppendDsSiToDsDi proc near

; first get to the end of the path.

	push	es
	smov	es,ds			;es=ds

@@:
	mov	al,es:[di]		;get the next byte
	inc	di			;next byte
	or	al,al			;NULL ?
	jnz	@b			;no.
	sub	di,2			;go back to last character
	mov     al,es:[di]		;get the last character
	cmp	al,'\'			;path separator ?
	jz	@f			;yes
	mov	al,'\'			;load it, zero in high byte
@@:
	stosb				;append a path separator

; now copy the path prefix.

@@:

	lodsb				;get the next byte
	stosb				;save it
	or	al,al			;NULL copied ?
	jnz	@b			;no.
	pop	es			;restore
	ret

AppendDsSiToDsDi endp
;----------------------------------------------------------------------------;
; GetSwapFileInformation:						     ;
;									     ;
; Gets information for building up swap file names.			     ;
;----------------------------------------------------------------------------;

cProc	GetSwapFileInformation,<NEAR,PUBLIC,PASCAL>,<si,di>

cBegin


; get the path prefix to be used for all swap files.

	call	GetSwapFilePathPrefix	;get the prefix

; obtain the drive letter for the swap drive from the basic swap name. 

	mov	al,WoaSwap1Path		;get the first letter
	and	al,0dfh			;convert to upper case
	mov	DiskSwap1Drive,al	;save

	mov	al,WoaSwap2Path		;get the first letter
	and	al,0dfh			;convert to upper case
	mov	DiskSwap2Drive,al	;save

cEnd
;----------------------------------------------------------------------------;
; find_next_arg:							     ;
;                Takes ES:SI over DOS delimiters and if poitions it to point ;
; to the next valid dos argument, or resturns NZ if end of line is reached   ;
;----------------------------------------------------------------------------;

find_next_arg	proc near

	push	ds			;save
	smov	ds,es			;load it in ds:si

find_next_arg_1:

	lodsb				;get the next byte
	cmp	al,0dh			;end of command line?
	je	no_more_args		;there is no second argument
	call	delim			;is this a DOS delimiter?
	jz	find_next_arg_1		;yes, skip over it
	dec	si			;point to this char
	xor	al,al		        ; set zero flag
	jmp	short find_next_arg_ret
no_more_args:
	dec	si
	or	al,al			;If end then AL is non zero
find_next_arg_ret:
	pop	ds			;restore
	ret

find_next_arg	endp

delim	proc	near
	cmp	al,' '			;space ?
	jz	delim_ret
	cmp	al,'='
	jz	delim_Ret
	cmp	al,','			;comma ?
	jz	delim_ret
	cmp	al,';'			;semicolon ?
	jz	delim_ret
	cmp	al,09H			;tab
	jz	delim_ret
	cmp	al,0AH			; Line Feed
delim_ret:
	ret

Delim	endp
;----------------------------------------------------------------------------;
; GetCompleteFileName:							     ;
;									     ;
; This routine takes a long pointer to a complete or partial file name and   ;
; returns the complete file name in the same buffer. If the file cannot be   ;
; found, AX will be set to -1. Also it assumes that the file name with the   ;
; NULL terminator is not more than 64 characters long.			     ;
;----------------------------------------------------------------------------;

cProc	GetCompleteFileName,<NEAR,PUBLIC,PASCAL>

	parmD	lpName			;input file name

cBegin

	cld				;do not take chances with this
cEnd
;----------------------------------------------------------------------------;
; DeleteTempFiles:							     ;
;									     ;
; This routine goes through a sequence of find first/find next calls and     ;
; deletes all woa and grabber left over temp files if any. The current DTA   ;
; address is saved and this routine uses a DTA on the stack for these calls  ;
;									     ;
;  The grabber files will not be deleted if we have an instance of Windows   ;
;  3.0 winoldap active or if this is not the first instance of the switcher. ;
;  								             ;
; The grabber swap files can only be in the second swap path (the grabbers   ;
; do not support two swap paths and Winoldap lets the grabber know only of   ;
; the second swap path.). There will thus be 3 groups to delete (woa swap    ;
; files in two swap paths and grabber swap file groups in the second path).  ;
;----------------------------------------------------------------------------;

cProc	DeleteTempFiles,<NEAR,PUBLIC,PASCAL>,<es,si,di>

	localD	CurrentDta		;saves current DTA address here
	localV	NewDta,128		;used for find/first and find next.

cBegin
	
; get and save the current DTA address

	mov	ah,2fh			;get DTA address call
	int	21h			;es:bx has the current address
	mov	seg_CurrentDta,es	;save segment
	mov	off_CurrentDta,bx	;save offset

; set the new DTA address to stack.

	lea	dx,NewDta		;will have it here on stack.
	push	ds			;save
	smov	ds,ss			;ds:dx is the new DTA area
	mov	ah,1ah			;set DTA address
	int	21h			;new DTA on stack
	pop	ds			;restore

; now delete the WOA temp files.

	mov	cx,3			;3  chains to delete

DeleteAChain:
	
; get the template of the next chain based on the value of cx:
; CX = 3 implies woa files in second path, CX = 2 implies grabber files in
; second path and CX = 1 implies woa files in first path.

; We should not try to delete grabber files if the SwitcherID is not 1 or if
; Windows 3.0 winoldap is active.

	cmp	cx,2			;trying to delete grabber files ?
	jnz	DTF_OkToDeleteChain	;no.
	cmp	SwitcherID,1		;first instance of switcher ?
	jnz	DTF_SkipThisChain	;no, do not delete grabber files

; check to see if another winoldap is active as a real mode stub.

	mov	ax,IS_WINOLDAP_ACTIVE	;woa traps this in real mode
	int	2fh			;see if woa is in chain
	or	ax,ax			;is woa active as a stub ?
	jz      DTF_SkipThisChain	;yes, donot delete any files

DTF_OkToDeleteChain:

	call	GetTempFileTemplate	;ds:dx has the file name template
	push	cx			;save
	mov	cx,3			;include hidden and read-only files
	mov	ah,4eh			;find first call
	int	21h			;get the first in chain
	jc	DoNextChain		;no such file

DeleteTheFile:

; recreate the complete path-file name from the file name that we just found

	pop	cx			;restore loop index
	push	cx			;save it back
	call	CopyBasicTempPrefix	;get the basic temp prefix
	sub	di,4			;back over '~DOS'
	lea	si,NewDta+1eh		;file name here
	push	ds			;save
	smov	ds,ss			;ds:dx has the file to delete

; now copy the file name over to complete the path file name

@@:

	lodsb				;get the next character of file name
	stosb				;copy it
	or	al,al			;are we done ?
	jnz	@b			;no.

; now delete the file after setting normal attributes.

	pop	ds			;restore ds
	mov	ax,4301h		;set file attributes
	xor	cx,cx			;normal attributes
	int	21h			;normal attributes set
	mov	ah,41h			;delete file code
	int	21h			;one file deleteted
	mov	ah,4fh			;find next file in chain
	int	21h			;did we get another
	jc	DoNextChain		;no.
	jmp	short DeleteTheFile	;delete this and look for more

DoNextChain:

	pop	cx			;get back count

DTF_SkipThisChain:

	loop	DeleteAChain		;delete this chain too.

; now set back the original DTA address and go back.

	push	ds			;save
	mov	dx,off_CurrentDta	;old dta offset
	mov	ds,seg_CurrentDta	;old dta segment
	mov	ah,1ah			;set DTA code
	int	21h			;DTA address restored
	pop	ds			;restore data segment

DeleteTempFilesRet:

cEnd

	;------------------------------------------;
	; get template for temp files based on the ;
	; value of CX.				   ;
	; CX = 3 => woa temp files in swap path2   ;
	; CX = 2 => grb temp files in swap path2   ;
	; CX = 1 => woa temp files in swap path1   ;
	;------------------------------------------;

GetTempFileTemplate proc near

	call	CopyBasicTempPrefix	;get the basic path prefix
	cmp	cx,2			;grabber template ?
	jz	GetGrbTemplate		;get grabbers file template

; now append a nibble for the switcher ID.

	mov	al,SwitcherID		;get the ID
	and	al,0fh			;only a nibble of ID
	add	al,30h			;convert numbers to ascii
	cmp	al,'9'			;did we go above 9 ?
	jbe	GTFT_ALHasAscii		;no, we are ok.
	add	al,7h			;convert to (A-F)

GTFT_ALHasAscii:

	stosb				;save it
	mov	ax,'.*'			;follow it up with '*.*'
	stosw				;save it
	mov	ax,'*'			;ah will have terminating NULL
	stosw				;template created
	mov	dx,DataOFFSET FileTemplate
	ret

GetGrbTemplate:

	sub	di,3			;step back over 'DOS'
	mov	ax,'RG'			;nedd to have 'GRB' instead
	stosw				;'GR' saved
	mov	al,'B'			;last part of 'GRB'
	stosb				;initial part done.
	mov	ax,'.*'			;follow it up with '*.*'
	stosw				;save it
	mov	ax,'*'			;ah will have terminating NULL
	stosw				;template created
	mov	dx,DataOFFSET FileTemplate
	ret

GetTempFileTemplate  endp

	;--------------------------------------------;
	; gets the basic swap path prefix based on   ;
	; the value of cx. CX = 2 or 3 implies use   ;
	; swap path 2, CX = 1 implies use path 1     ;
	;--------------------------------------------;

CopyBasicTempPrefix proc near

	mov	di,DataOFFSET FileTemplate	
	test	cx,2			;2 or 3 
	jnz	@f			;yes.
	call	FarCopyBasicSwap1FileName;get prefix for path 1
	ret
@@:
	call	FarCopyBasicSwap2FileName;get prefix for path 2
	ret

CopyBasicTempPrefix endp
;----------------------------------------------------------------------------;
; GetSwitcherInfo:							     ;
;							 		     ;
; Gets info from the global switch structure.				     ;
;----------------------------------------------------------------------------;

cProc	GetSwitcherInfo,<FAR,PUBLIC,PASCAL>,<es>

	localD	OtherSwitcherAddr	;call in address of the 'other' switcher

cBegin

; get a pointer to the global block.

	mov	ax,4a05h		;opcode
	mov	si,CGET_GLOBAL_SWITCH_DATA
	int	2fh			;dx:ax has the long pointer
	mov	es,dx			;load it into es
	mov	di,ax			;es:di -> info structure

; scan through the list and if all ProgramID's are zero, then reset the
; Id_Serial field.

	lea	si,[di].Program_List	;es:si points to the first program entry
	mov	cx,MAX_NUM_PROGRAMS	;get the max number of entries
	xor	ax,ax			;will build a count here

LoopAllEntries:

	test	es:[si].Program_Flags,F_FREE
	jnz	ContinueLoop		;this is a free entry
	cmp	es:[si].Program_Id,0	;has this been run before ?
	jnz	OutOfLoop		;yes, break out of loop

ContinueLoop:

	add	si, SIZE Switch_Entry	;es:si -> next program
	inc	ax			;one more fresh entry obtained
	loop	LoopAllEntries		;continue lloking

OutOfLoop:

	cmp	ax,MAX_NUM_PROGRAMS	;all entries fresh ?
	jb	@f			;no.
	mov	es:[di].Id_Serial,0	;reset this.

@@:

; copy the SwitcherId from the global structure.

	mov	al,es:[di].Switcher_Id	;ID allocated to this swicher
	mov	SwitcherID,al		;save it.

; get a pointer to the first program in the list.

	lea	si,[di].Program_List	;start of array
	xor	ah,ah			;clear out high byte
	mov	al,es:[di].First_In_List;get index of first entry
	mov	bl,SIZE Switch_Entry	;size of each entry
	mul	bl			;ax has start offset
	add	si,ax			;es:si -> first program in list.

; copy program related variables.

	mov	ax,es:[si].Conv_Req	;get conventional memory requirements
	mov	AppMinMem,ax		;save minimum memory required
	mov	ax,es:[si].XMS_Req	;amount of xms required
	mov	AppMinXmsK,ax		;save as min xms requirement
	mov	ax,es:[si].XMS_Want	;xms desired
	mov	AppMaxXmsK,ax		;save it

; if AppMaxXmsK is < AppMinXmsK, set it to AppMinXmsK.

	cmp	ax,AppMinXmsK		;ax could be -1 too.
	jae	@f			;all is fine
	mov	ax,AppMinXmsK		;minimum
	mov	AppMaxXmsK,ax		;save it
@@:

; copy global variables and flags from the switch_info structure.
	
	xor	ah,ah			;reset
	mov	al,es:[di].Num_Lines	;start up no of screen lines

; if the screen lines is zero, we must get it from the BIOS area

 	or	al,al			;uninitialized ?
	jnz	@f			;no.
	push	es			;save
	mov	bx,40h			;bios data area
	mov	es,bx			;es points to BIOS data segment
	mov	bx,84h			;location where screen lines saved
	mov	al,es:[bx]		;get the no of screen lines
	inc	ax			;actually it is one more
	pop	es			;restore
@@:
	mov	cs:[WoaStartScreenLines],ax;save it
	mov	StartScreenLines,ax	;save a local copy too.

	mov	al,es:[di].Screen_Back	;back ground screen color
	mov	[SwitcherColors],al	;save it
	mov	al,es:[di].Title_Back	;title back ground color
	mov	[SwitcherColors+1],al	;save it
	mov	al,es:[di].Title_Fore	;title text color
	mov	[SwitcherColors+2],al	;save it
	mov	ax,es:[di].CPU_Type	;get the CPU type
	mov	cs:[WoaCpuType],ax	;save it
	mov	ax,es:[di].SFT_Size	;get the file entry size
	mov	cs:[WoaFileEntrySize],ax;save it

; set the global flags

	mov	al,es:[di].Global_Flags	;get the global flags
	mov	cs:[Woa6fValue],0  	;assume int 6f not to be done
	test	al,GF_INT_6F_TOBE_DONE	;int 6f to be done ?
	jz	@f			;no.
	mov	cs:[Woa6fValue],0ffh		;int 6f to be done.
@@:
	mov	cs:[WoaIrq9Global],0	;assume IRQ 9 to be handled globally
	test	al,GF_IRQ9_GLOBAL	;is IRQ 9 global ?
	jnz	@f			;yes
	mov	cs:[WoaIrq9Global],0ffh	;don't handle IRQ 9
@@:
	mov	cs:[WoaNetAsyncSwitching],0	;assume cannot switch out on async net
	test	al,GF_NET_ASYNC_SWITCH_OK;is it ok to switch out ?
	jz	@f			;no
	mov	cs:[WoaNetAsyncSwitching],0ffh	;ok to switch out
@@:

; copy various behaviour bits and hot key states.

	mov	cs:[WoaBehavior],0 	;initialize
	mov	cs:[WoaHotKeys],0	;initialize
	mov	ax,es:[si].Program_Flags
	and	al,F_NO_SWITCH+F_GRAPHICS+F_NO_PAUSE
	mov	cs:[WoaBehavior],al 	;save it
	and	ax,F_NO_ALT_TAB+F_NO_ALT_ESC+F_NO_CTRL_ESC
	mov	cs:[WoaHotkeys],ah	;save

; also set bytes in the local hot key list for keys that are disabled.

	test	ax,F_NO_ALT_TAB		;ALT+TAB disabled ?
	jz	@f			;no.
	mov	cs:[WoaAltTabDisabled],0ffh
	mov	cs:[WoaShftAltTabDisabled],0ffh
@@:
	test	ax,F_NO_ALT_ESC		;ALT+ESC disabled ?
	jz	@f			;no.
	mov	cs:[WoaAltEscDisabled],0ffh
	mov	cs:[WoaShftAltEscDisabled],0ffh
@@:
	test	ax,F_NO_CTRL_ESC    	;ALT+ESC disabled ?
	jz	@f			;no.
	mov	cs:[WoaCtrlEscDisabled],0ffh
@@:

; copy the swither ID.

	mov	al,SwitcherID		;get the ID
	mov	cs:[WoaSwitcherID],al	;pass it on

; now copy the program name.

	push	di			;save
	push	si			;save program entry pointer
	mov	di,StubSegOFFSET WoaPath;save program name here.
	lea	si,[si].Program_Name	;es:si -> program name
	call	CopyEsSiToCsDi		;copy the name .
	pop	si			;get back start of entry
	pop	di			;restore.

; now copy the parameters

	push	di			;save
	push	si			;save program entry pointer
	lea	si,[di].Parameters	;es:si -> parametsrs
	mov	di,StubSegOFFSET WoaParams;command parameters
	call	CopyEsSiToCsDi		;copy the name .
	pop	si			;get back start of entry
	pop	di			;restore.

; get the program ID and the path ID.

	mov	al,es:[si].Path_Id	;get the path ID
	mov	CurrentPathId,al	;save it
	mov	ax,es:[si].Program_Id	;id for the app
	mov	hApp,ax			;save it.
	mov	StartRestartId,al	;start if 0 else restart
	or	ax,ax			;is it a fresh start ?
	jnz	@f			;no.		     

; build the DosAppNumber.

	mov	ax,es:[di].Id_Serial	;get the serial id
	mov	DosAppNumber,ax		;save it.
	inc	ax			;increment it
	mov	es:[di].Id_Serial,ax	;update it.

; build the hApp id. This is basically the slot in the global structure where
; the program details have been built. Or in otherwords, it is First_In_List.
; we also need to combine the SwitcherID with it.

	mov	ah,SwitcherID		;get the ID
	shiftl	ah,4			;only four 4 bits are significant
	mov	al,es:[di].First_In_List;rest of the task id
	mov	hApp,ax			;app's id
	mov	es:[si].Program_Id,ax	;save it
@@:

; get the grabber file name.

	mov	si,di			;es:si -> SwitchInfo structure
	push	si			;save
	lea	si,es:[si].Grabber_Name	;es:si -> points to the grabber name
	mov	di,StubSegOFFSET WoaGrabberName;will copy grabber name here
	call	CopyEsSiToCsDi		;copy the name .
	pop	si			;es:si -> SwitchInfo

; copy the two swap file paths.

	push	si			;save
	lea	si,es:[si].Swap_Path1	;es:si -> points to the swap path
	mov	di,DataOFFSET WoaSwap1Path;will copy path here
	call	CopyEsSiToDsDi		;copy the name .
	pop	si			;restore pointer to entry

	push	si			;save
	lea	si,es:[si].Swap_Path2	;es:si -> points to the swap path
	mov	di,DataOFFSET WoaSwap2Path;will copy path here
	call	CopyEsSiToDsDi		;copy the name .
	pop	si			;restore pointer to entry

; load the minimum space information for the two swap paths.

	mov	ax,es:[si].Min_Path1	;min space for path 1
	mov	Swap1MinK,ax		;save it
	mov	ax,es:[si].Min_Path2	;min space for path 2
	mov	Swap2MinK,ax		;save it

; get information about swap file.					     

	call	GetSwapFileInformation	      

; If DosAppNumber is 0 and this is not a restart, we must delete all 
; temporary files

	cmp	DosAppNumber,0		;first app ?
	jnz	@f			;no.
	cmp	StartRestartID,0	;if tart
	jnz	@f			;delete files.
	cCall	DeleteTempFiles		;delete all temporary files.
@@:

	clc				;successful completion of routine
	jmp	short GetSwitcherInfoRet

GetSwitcherInfoErr:

	stc				;error, cannot proceed

GetSwitcherInfoRet:

cEnd
;----------------------------------------------------------------------------;
; CopyEsSiToDsDi:								     ;
;									     ;
; Copies a NULL terminated string from es:si to ds:si.			     ;
;----------------------------------------------------------------------------;

CopyEsSiToDsDi proc near

	mov	al,es:[si]		;load a byte
	mov	ds:[di],al		;save it
	inc	si			;bump src ptr
	inc	di			;bump target ptr
	or	al,al			;NULL copied ?
	jnz	CopyEsSiToDsDi		;continue
	ret				;done

CopyEsSiToDsDi endp
;----------------------------------------------------------------------------;
; CopyEsSiToCsDi:								     ;
;									     ;
; Copies a NULL terminated string from es:si to cs:si.			     ;
;----------------------------------------------------------------------------;

CopyEsSiToCsDi proc near

	mov	al,es:[si]		;load a byte
	mov	cs:[di],al		;save it
	inc	si			;bump src ptr
	inc	di			;bump target ptr
	or	al,al			;NULL copied ?
	jnz	CopyEsSiToCsDi		;continue
	ret				;done

CopyEsSiToCsDi endp
;----------------------------------------------------------------------------;
; NearErrorHanlder:							     ;
;									     ;
; From here we jump into the main code segment error handler.		     ;
;----------------------------------------------------------------------------;

NearErrorHandler:

	jmp	ErrorHandler		;hApp jump into _TEXT segment

;----------------------------------------------------------------------------;
sEnd StubSeg

end
