;*
;*	CW : Character Windows
;*
;*	syd_std.asm : standard defaults
;*


ifndef DoSoundSyd_NonDefault
;*****************************************************************************
;********** DoSoundSyd **********
;*	* SYD entry point (see documentation for interface)

cProc	DoSoundSyd, <FAR, PUBLIC, ATOMIC>
    parmW  req
cBegin	DoSoundSyd

cEnd	DoSoundSyd
;*****************************************************************************
endif	;* DoSoundSyd_NonDefault



ifndef LGetTimeSyd_NonDefault
;*****************************************************************************
;********** LGetTimeSyd **********
;*	* SYD entry point (see documentation for interface)

cProc	LGetTimeSyd, <FAR, PUBLIC, ATOMIC>
cBegin	LGetTimeSyd

	xor	ax,ax
	xor	dx,dx

cEnd	LGetTimeSyd
;*****************************************************************************
endif	;* LGetTimeSyd_NonDefault

