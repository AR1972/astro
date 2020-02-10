;*
;*	CW : Character Windows
;*
;*	utildrv.asm : installable driver utility routines

	include kernel.inc
	include kmem.inc
	include indrv.inc
	include inscr.inc
	include	inkbd.inc
	include insyd.inc
	include	_loaddrv.inc

;*****************************************************************************

;*	App call backs for memory management

externFP	<LpwAllocDriverMem,FreeDriverMem>

;*****************************************************************************

sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,DGROUP
    assumes SS,DGROUP


;********** FreeIndv ***************
;*	entry:	pindv => indv of the service to be freed
;*	* Frees memory allocated to for the given service
;*	* NOTE: any routine pointers into this service will be invalid
;*	*	and are not fixed up, a new service must be loaded before
;*	*	any calls

cProc	FreeIndv, <PUBLIC,FAR>
    parmW	pindv
cBegin	FreeIndv

	mov	bx,pindv
	mov	es,[bx].psLoadedIndv
	mov	ax,es:LO_lpwdataInsh
	mov	dx,es:HI_lpwdataInsh
	Save	<es>
	cCall	<far ptr FreeDriverMem>,<dx,ax>
	xor	ax,ax
	cCall	<far ptr FreeDriverMem>,<es,ax>

cEnd	FreeIndv


sEnd	KERNEL

;*****************************************************************************

	END
