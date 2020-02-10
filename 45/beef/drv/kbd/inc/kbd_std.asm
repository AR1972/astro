;*
;*	CW : Character Windows
;*
;*	kbd_std.asm : standard KBD defaults (empty procedures)
;*


ifndef PollKeyboardKbd_NonDefault
;*****************************************************************************
;********** PollKeyboardKbd **********
;*	* KBD entry point (see documentation for interface)

cProc	PollKeyboardKbd, <FAR, PUBLIC, ATOMIC>
cBegin	PollKeyboardKbd

cEnd	PollKeyboardKbd

;*****************************************************************************
endif	;* PollKeyboardKbd_NonDefault



ifndef SetShiftKkKbd_NonDefault
;*****************************************************************************
;********** SetShiftKkKbd **********
;*	* KBD entry point (see documentation for interface)

cProc	SetShiftKkKbd, <FAR, PUBLIC, ATOMIC>
    parmW  kkNew
cBegin	SetShiftKkKbd

cEnd	SetShiftKkKbd

;*****************************************************************************
endif	;* SetShiftKkKbd_NonDefault



ifndef ChAlternateKeytopKbd_NonDefault
;*****************************************************************************
;********** ChAlternateKeytopKbd **********
;*	* KBD entry point (see documentation for interface)

cProc	ChAlternateKeytopKbd, <FAR, PUBLIC, ATOMIC>
    parmB  chOriginal
cBegin	ChAlternateKeytopKbd

cEnd	ChAlternateKeytopKbd

;*****************************************************************************
endif	;* ChAlternateKeytopKbd_NonDefault



ifndef FlushKeyRgchKbd_NonDefault
;*****************************************************************************
;********** FlushKeyRgchKbd **********
;*	* KBD entry point (see documentation for interface)

cProc	FlushKeyRgchKbd, <FAR, PUBLIC, ATOMIC>
    parmDP rgchOriginal
cBegin	FlushKeyRgchKbd

cEnd	FlushKeyRgchKbd

;*****************************************************************************
endif	;* FlushKeyRgchKbd_NonDefault
