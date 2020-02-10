;*
;*	CW : Character Windows
;*
;*	loaddrv.asm : installable driver loader routines

	include kernel.inc
	include kmem.inc
	include indrv.inc
	include inscr.inc
	include	inkbd.inc
	include insyd.inc
	include	_loaddrv.inc

;*****************************************************************************

;*	App call backs for memory management

;; REVIEW: hard coded driver allocation!
externFP	<LpwAllocDriverMem,FreeDriverMem>

IFDEF DOS5
externFP	<DosOpen, DosClose>
externFP	<DosChgFilePtr, DosRead>
externFP	<DosCreateCSAlias>
ENDIF

sBegin	DATA
    assumes CS,DGROUP
    assumes DS,DGROUP

ifndef	DOS5
externW	chDrivePhantom			; set to "b:" on one floppy systems.
endif	; !DOS5

externW		<inkj>		; keyboard
externW		<insj>		; screen (character)
externW		<inyj>		; system

ifdef	DUAL
ifndef	DOS5
LDRVDATA = 1
endif
else	;!DUAL
LDRVDATA = 1
endif	;!DUAL

ifdef	LDRVDATA

labelW	<PUBLIC, rgindvCw>		;* INDV structures for csd, kbd

labelW	<PUBLIC, indvKbd>		;* Keyboard
	DB	indtKeyboard,0
	DW	dataOffset inkj
	DW	cpfnKbdMin
	DW	cpfnKbdMin
	DW	0
	DW	0

labelW	<PUBLIC, indvCsd>		;* Character Screen
	DB	indtCharacterScreen,0
	DW	dataOffset insj
	DW	cpfnCsdMin
	DW	cpfnCsdMin
	DW	0
	DW	0

labelW	<PUBLIC, indvSyd>		;* System Services
	DB	indtSystem,0
	DW	dataOffset inyj
	DW	cpfnSydMin
	DW	cpfnSydMin
	DW	0
	DW	0

	PUBLIC	cindvCw
cindvCw	EQU	($-rgindvCw)/cbIndvMin

else	;!LDRVDATA

externW	rgindvCw

endif	;!LDRVDATA

IFDEF DOS5
externB		inosDrv				;* INOS info (from loaddrv.asm)
ENDIF ;DOS5

sEnd	DATA


ifndef	LDRVDATA

externA	cindvCw

endif	;!LDRVDATA

;*****************************************************************************

sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,DGROUP
    assumes SS,DGROUP



;********** RerrLoadDrvFd **********
;*	entry:	hFile => valid file handle
;*		rgindv => cindv INDV records
;*	* Loads drivers from given file
;*	exit:	0	ok, otherwise error code: rerrBadRead,
;*						  rerrBadFormat,
;*						  rerrNoMemory,
;*						  rerrBadFile,
;*						  rerrBadMemReq,
;*						  rerrBadVersion
;*
;*	On error, no service is partially installed.  However, only some
;*	services may have been installed.
;*
;*	nop if cindv == 0 


rldf_badopen:
	mov	ax,rerrBadFile		; restrict error returned
rldf_Abort1:
	jmp	rldf_Abort

;----------------------------------------

cProc	RerrLoadDrvFd, <PUBLIC,FAR>,<DI,SI>
    parmW	hFile
    parmW	rgindv
    parmW	cindv
IFDEF DOS5
    localW	action
    localW	HI_tmp
    localW	LO_tmp
ENDIF ;DOS5
    localW	HI_lfaMT
    localW	LO_lfaMT
    localW	HI_lpwCSeg
    localW	LO_lpwCSeg
    localW	HI_lfaReadMT
    localW	LO_lfaReadMT
    localW	pindv
    localW	cb			; count of bytes in rgbBuff
    localV	rgbBuff,cbBuff		; buffer for master table entries
cBegin	RerrLoadDrvFd

	;*	set indv's to not loaded

	xor	ax,ax
	mov	cx,cindv
	jcxz	rldf_Abort1		; no service will be loaded
	mov	di,rgindv

@@:	jcxz	@F
	mov	[di].psLoadedIndv,ax
	mov	[di].cpfnLoadedIndv,ax
	add	di,cbIndvMin
	loop	@B
@@:

IFNDEF DOS5
	;*	seek to EOF magic tag

	mov	bx,hFile
	mov	ax,-cbIndhMin
	cwd
	mov	cx,dx
	mov	dx,ax
	mov	ax,4202h		; offset is from EOF
	int	21h
	jc	rldf_AbFileshort2

	;*	compute absolute offset to EOF

	add	ax,cbIndhMin
	adc	dx,0
	mov	LO_lfaReadMT,ax
	mov	HI_lfaReadMT,dx
ELSE
	;*	seek to EOF magic tag

	mov	ax,-cbIndhMin
	cwd
	lea	bx,LO_lfaReadMT		; HI_ is in appropriate position
	cCall	DosChgFilePtr,<hFile,dx,ax,2,ss,bx>
	or	ax,ax
	jnz	rldf_AbFileshort2

	;*	compute absolute offset to EOF

	mov	ax,cbIndhMin
	add	LO_lfaReadMT,ax
	adc	HI_lfaReadMT,0
ENDIF ;DOS5

	;*	read magic tag and dlfaTable

	lea	dx,rgbBuff
IFNDEF DOS5
	mov	ah,3fh
	AssertEQ bx,hFile
	mov	cx,cbIndhMin
	int	21h
	jc	rldf_AbFileshort2
ELSE
	lea	ax,cb
	cCall	DosRead,<hFile,ss,dx,cbIndhMin,ss,ax>
	or	ax,ax
	jnz	rldf_AbFileshort2
	mov	ax,cb
ENDIF ;DOS5

	cmp	ax,cbIndhMin		; check read
	mov	ax,rerrBadRead
	jne	rldf_AbFileshort1

	mov	ax,rerrBadFormat
	mov	cx,rgbBuff+LO_rgchMagicIndh
					; check magic word ("DRV0")
	cmp	cx,'RD'
	jne	rldf_AbFileshort1		; invalid format
	mov	cx,rgbBuff+HI_rgchMagicIndh
	cmp	cx,'0V'
	je	@F
rldf_AbFileshort2:
	mov	ax,rerrBadRead
rldf_AbFileshort1:
	jmp	rldf_AbFile		; invalid format
@@:

	;*	compute offset of master table
	
	mov	dx,LO_lfaReadMT
	mov	cx,HI_lfaReadMT
	sub	dx,rgbBuff+LO_dlfaTableIndh
	sbb	cx,rgbBuff+HI_dlfaTableIndh

	;*	save lfa of master table

	mov	LO_lfaMT,dx
	mov	HI_lfaMT,cx

	;*	prep for master table read

	mov	LO_lfaReadMT,dx
	mov	HI_lfaReadMT,cx
	mov	cx,cbBuff

	;*
	;*	Read from lfaReadMT into rgbBuff
	;*	cx	bytes to read
	;*
	;*	cb	bytes read
	;*	lfaReadMT
	;*		adjusted to next byte to be read
	;*	di	points to first INDS (smart about table header)

rldf_ReadMT:

	;*	seek to lfaReadMT

IFNDEF DOS5
	push	cx
	mov	ax,4200h
	mov	bx,hFile
	mov	dx,LO_lfaReadMT
	mov	cx,HI_lfaReadMT
	int	21h
	pop	cx
	jc	rldf_AbFileshort2
ELSE
	lea	ax,LO_tmp		; dummy
	cCall	DosChgFilePtr,<hFile,HI_lfaReadMT,LO_lfaReadMT,0,ss,ax>
	or	ax,ax
	jnz	rldf_AbFileshort2
ENDIF ;DOS5

	;*	read master table entries

	lea	dx,rgbBuff
IFNDEF DOS5
	mov	ah,3fh
	AssertEQ bx,hFile
	int	21h
	jc	rldf_AbFileshort2
	mov	cb,ax			; save bytes read
ELSE
	lea	ax,cb
	cCall	DosRead,<hFile,ss,dx,cx,ss,ax>
	or	ax,ax
	jnz	rldf_AbFileshort2
	mov	ax,cb
ENDIF ;DOS5

	or	ax,ax
	mov	ax,rerrBadRead
	jnz	@F
	jmp	rldf_AbFile		; check for read beyond EOF
@@:

	;*	cx:dx == lfaRead

	mov	dx,LO_lfaReadMT
	mov	cx,HI_lfaReadMT

	lea	di,rgbBuff		; di points to inds struct

	;*	if we just read from lfaMT, check magic, load number of
	;*		services, and increase di after INDT

	cmp	dx,LO_lfaMT
	jne	not_first_mt_read
	cmp	cx,HI_lfaMT
	jne	not_first_mt_read

	;*	check magic word

	mov	ax,rerrBadFormat
	mov	bx,rgbBuff+LO_rgchMagicIndt
					; check magic word ("DRV0")
	cmp	bx,'RD'
	jne	rldf_AbFileshort1	; invalid format
	mov	bx,rgbBuff+HI_rgchMagicIndt
	cmp	bx,'0V'
	je	@F
	jmp	rldf_AbFile		; invalid format
@@:

	;*	si == number of services

	mov	si,rgbBuff+cindsIndt	; si services in file

	;*	di => INDS

	lea	di,rgbBuff+rgindsIndt
not_first_mt_read:

	;*	adjust lfaReadMT

	add	dx,cb
	adc	cx,0
	mov	LO_lfaReadMT,dx
	mov	HI_lfaReadMT,cx

	;*
	;*	Load next service
	;*	si	number of services left to load
	;*	di	current inds structure
	;*

rldf_Load:

	;*	are we done?

	xor	ax,ax
	dec	si
	jge	@F
	jmp	rldf_done		; load completed
@@:

	;*	have we run out of services in this block?

	lea	ax,rgbBuff
	add	ax,cb
	cmp	ax,di
	ja	@F

	;*	read next block of INDS structures

	inc	si			;* retry this service
	mov	cx,cbBlock
	jmp	rldf_ReadMT
@@:

	;*	should we load this service?

	push	di

;*	* first see if operating system is supported
IFDEF DOS5
	test	[di].floadInds,floadProtMode
ELSE
	test	[di].floadInds,floadRealMode
ENDIF ;!DOS5
	jz	rldf_indtNotfound

	mov	ah,[di].indtInds

	;*	search rgindv

	mov	di,rgindv
	mov	cx,cindv
rldf_nextindv:
	cmp	ah,[di].indtIndv
	jne	@F
					; found indv
	mov	pindv,di
	clc				; flag load
	jmp	short rldf_endIndtLookup
@@:
	add	di,cbIndvMin
	loop	rldf_nextindv
rldf_indtNotfound:
	stc				; no service info, flag no load

rldf_endIndtLookup:

	pop	di
	jnc	@F
	jmp	rldf_loadnext		; unknown service
@@:

	;*
	;*	Load service segment
	;*

	;*	allocate code area

	mov	ax,[di].cbCodeInds
	inc	ax			; round up
	AssertNE ax,0
	shr	ax,1			; convert cb -> cw
	test	[di].floadInds,floadFixed

IFNDEF LATER
	AssertNZ			; only know about fixed segments
	mov	bx,fmemFixed
ENDIF

	cCall	<LpwAllocDriverMem>,<ax,bx>
	mov	cx,ax
	or	cx,dx
	jnz	@F
	mov	ax,rerrNoMemory
	jmp	rldf_AbFile
@@:
IFDEF DEBUG				; valid pointers are either ds:offset
					;  or other:0000
	mov	bx,ds
	cmp	bx,dx
	je	@F
	AssertEQ ax,0
@@:
ENDIF ;DEBUG

	;*	save pointer to code area

	mov	LO_lpwCSeg,ax
	mov	HI_lpwCSeg,dx

	;*	seek to service segment

	mov	dx,LO_lfaMT
	mov	cx,HI_lfaMT
	sub	dx,[di].LO_dlfaCodeInds	; calculate lfaCode
	sbb	cx,[di].HI_dlfaCodeInds
IFNDEF DOS5
	mov	ax,4200h
	mov	bx,hFile
	int	21h
	jc	rldf_AbCodeshort2
ELSE
	lea	ax,LO_tmp		; dummy
	cCall	DosChgFilePtr,<hFile,cx,dx,0,ss,ax>
	or	ax,ax
	jnz	rldf_AbCodeshort2
ENDIF ;DOS5

	;*	read in code segment

IFNDEF DOS5
	push	ds
	mov	ah,3fh
	mov	bx,hFile
	mov	cx,[di].cbCodeInds
	mov	dx,HI_lpwCSeg
	mov	ds,dx
	mov	dx,LO_lpwCSeg
	int	21h
	pop	ds
	jc	rldf_AbCodeshort2
ELSE
	lea	ax,cb
	cCall	DosRead,<hFile,HI_lpwCSeg,LO_lpwCSeg,[di].cbCodeInds,ss,ax>
	or	ax,ax
	jnz	rldf_AbCodeshort2
	mov	ax,cb
ENDIF ;DOS5

	cmp	ax,[di].cbCodeInds	; check for read error
	jne	rldf_AbCodeshort2

	;*	if load type is non-standard, this service is completed

	test	[di].floadInds,floadStandard
	jnz	@F
	jmp	rldf_loadnext		; non-standard driver
@@:

	;*	allocate data area

	mov	bx,HI_lpwCSeg		; es:bx points to service header
	mov	es,bx
	mov	bx,LO_lpwCSeg

	mov	ax,es:[bx].cbDataInsh
	inc	ax
	shr	ax,1			; convert cb -> cw
	cmp	ax,0
	jne	@F
	xor	ax,ax			; no data to allocate
	mov	dx,ax
	jmp	short rldf_load11	; continue with link
@@:
	push	ax			; save word count for later
	mov	cx,fmemFixed
	test	es:[bx].fNearDataInsh,0ffffh
	jz	@F
	mov	cx,fmemNear
	cmp	ax,cbNearMemServiceMax / 2
	jbe	@F
	mov	ax,rerrBadMemReq	; asking for too much near mem!
	jmp	rldf_AbCode
@@:
	cCall	<LpwAllocDriverMem>,<ax,cx>
	;*	put word count in cx
	pop	cx
	or	ax,ax			; 0? failure
	jnz	@F
	or	dx,dx
	jnz	@F
	mov	ax,rerrNoMemory
rldf_AbCodeshort1:
	jmp	rldf_AbCode

rldf_AbCodeshort2:
	mov	ax,rerrBadRead
	jmp	rldf_AbCode
@@:
rldf_load11:
IFDEF DEBUG				; valid pointers are either ds:offset
					;  or other:0000
	mov	bx,ds
	cmp	bx,dx
	je	@F
	AssertEQ ax,0
@@:
ENDIF ;DEBUG

	;*	patch code segment with pointer to data segment and pinos

	mov	es,HI_lpwCSeg			; es => code segment
	xor	bx,bx
	mov	es:[bx].LO_lpwDataInsh,ax
	mov	es:[bx].HI_lpwDataInsh,dx

;*	* set "pinos"
IFNDEF DOS5
	mov	es:[bx].pinosInsh,0
ELSE
	mov	es:[bx].pinosInsh,dataOffset inosDrv
ENDIF

IFDEF DOS5
	;*	create CS alias

	lea	ax,HI_lpwCSeg
	mov	bx,HI_lpwCSeg
	cCall	DosCreateCSAlias,<bx,ss,ax>
	or	ax,ax
	jz	@F
	mov	HI_lpwCSeg,bx		; restore selector on failure
	jmp	rldf_AbCode
@@:
	;*	restore registers used (es,ax,bx,cx,dx are important)
	;*	cx,dx not changed

	mov	es,HI_lpwCSeg
	xor	bx,bx
	mov	ax,es:[bx].LO_lpwDataInsh
ENDIF ;DOS5

	;*	clear data segment to zeros (assume word count in cx)

	;*	es:0 => code segment INSH
	;*	dx:ax => data segment
	;*	cx	 data size (words)

	push	es
	push	di
	mov	di,ax
	mov	es,dx
	xor	ax,ax
	rep stosw
	pop	di
	pop	es

	;*	get function count and rgpfn

	mov	dx,es:[bx].cpfnInsh	; function count
	add	bx,cbInshMin		; rgpfn

	;*	At this point we have:
	;*		es:bx => HI_lpwCSeg:rgpfn
	;*		dx    == cpfn
	;*		ds:di => inds
	;*		pindv OR psld => link info
	
	;*	link service to app

	push	di
	push	ds

	;*	check version

;	check version
;	link
;	set return values of indv struct

	mov	di,pindv
	cmp	dx,ss:[di].cpfnNeedMinIndv
	jae	@F				;* we have at least enough
rldf_linkBadver:
	mov	ax,rerrBadVersion
	stc
	jmp	short rldf_linkdone
@@:
	mov	cx,ss:[di].cpfnNeedMacIndv	;* the max # we want
	cmp	dx,cx
	jae	@F				;* just copy the ones we want
	mov	cx,dx				;* copy what we have < NeedMac
@@:
	mov	di,ss:[di].rglpfnIndv

	;*	link to app

	mov	dx,ds			; segment switch for rep stosw
	mov	ax,es
	mov	ds,ax
	mov	es,dx
	;	ds => HI_lpwCSeg
	;	es => DGROUP

	push	cx			; save cfpn for later

	push	cx
	push	di
	shl	cx,1
	mov	ax,ds
	rep stosw			; fill app jump table with segment value
	pop	di
	pop	cx

@@:					; fill in offsets
	mov	ax,[bx]
	mov	es:[di],ax
	inc	bx
	inc	bx
	add	di,4
	loop	@B

	pop	cx			; restore cpfn

	;*	set return values in indv

	mov	bx,pindv
	mov	ss:[bx].cpfnLoadedIndv,cx
	mov	cx,HI_lpwCSeg
	mov	ss:[bx].psLoadedIndv,cx
	clc				; linkage completed

rldf_linkdone:
	pop	ds
	pop	di
	jc	rldf_AbData

rldf_loadnext:

	add	di,cbIndsMin
	jmp	rldf_Load

rldf_AbData:
	;*	Code and data segments allocated but couldn't link to app.
	;*	Free data then go on to free code and close hFile.

	push	ax			; return code in ax
	mov	ax,HI_lpwCSeg
	mov	es,ax
	mov	ax,es:LO_lpwDataInsh
	mov	dx,es:HI_lpwDataInsh
	cCall	<far ptr FreeDriverMem>,<dx,ax>
	pop	ax
	
rldf_AbCode:
	;*	Something went wrong before completing code segment that
	;*	was allocated - free it and close hFile.

	push	ax			; return code in ax
	mov	ax,LO_lpwCSeg
	mov	dx,HI_lpwCSeg
	cCall	<far ptr FreeDriverMem>,<dx,ax>
	pop	ax
	
rldf_done:
rldf_AbFile:
rldf_Abort:

cEnd	RerrLoadDrvFd


;********** RerrLoadDrv **********
;*	entry:	szFile => full path
;*		rgindv => cindv INDV records
;*	* Loads drivers from given file
;*	exit:	see RerrLoadDrvFd


rlds_badopen:
	mov	ax,rerrBadFile		; restrict error returned
	jmp	short rlds_end

;----------------------------------------

cProc	RerrLoadDrv, <PUBLIC,FAR>,<DI,SI>
    parmW	szFile
    parmW	rgindv
    parmW	cindv
    localW	hFile
cBegin	RerrLoadDrv

	;*	open file

IFDEF	DOS5

	lea	ax,hFile
	lea	bx,action
	cCall	DosOpen,<ss,szFile,ss,ax,ss,bx,0,0,0,01h,openRO,0,0>
	or	ax,ax
	jnz	rlds_badopen

ELSE	; !DOS5

	mov	dx,szFile

	mov	bx,dx
	mov	ax,[bx]			; Get first two letters
	or	al,20h			; If the file is "B:xxx" on a single
	cmp	ax,chDrivePhantom	;   floppy system, then no can do.
	je	rlds_badopen

	mov	ax,3d00h
	int	21h
	mov	hFile,ax
	jc	rlds_badopen

ENDIF	; !DOS5

	cCall	RerrLoadDrvFd,<hFile, rgindv, cindv>

	push	ax			; return code in ax
IFNDEF DOS5
	mov	ah,3eh			; close file
	mov	bx,hFile
	int	21h
ELSE
	cCall	DosClose,<hFile>
ENDIF ;DOS5
	pop	ax			; ignore return code from close

rlds_end:

cEnd	RerrLoadDrv


;********** RerrLoadCwDrvFd **********
;*	entry:	hFile => valid file handle
;*	* Loads kbd and csd drivers from given file
;*	exit:	same as RerrLoadDrvFd

cProc	RerrLoadCwDrvFd, <PUBLIC,FAR>
    parmW	hFile
cBegin	RerrLoadCwDrvFd

	mov	ax,dataOffset rgindvCw
	mov	cx,cindvCw
	cCall	RerrLoadDrvFd,<hFile, ax, cx>

cEnd	RerrLoadCwDrvFd


;********** RerrLoadCwDrv **********
;*	entry:	szFile => full file name of driver
;*	* Loads kbd and csd drivers from given file
;*	exit:	same as RerrLoadDrv

cProc	RerrLoadCwDrv, <PUBLIC,FAR>
    parmW	szFile
cBegin	RerrLoadCwDrv

	mov	ax,dataOffset rgindvCw
	mov	cx,cindvCw
	cCall	RerrLoadDrv,<szFile, ax, cx>

cEnd	RerrLoadCwDrv


sEnd	KERNEL

;*****************************************************************************

	END
