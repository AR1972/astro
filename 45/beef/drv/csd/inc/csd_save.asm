;*
;*	CW : Character Windows
;*
;*	csd_save.asm : stub routines for screen saving (not supported
;*			by default)
;*

ifndef CbSizeVidsCsd_NonDefault
;*****************************************************************************
;********** CbSizeVidsCsd **********
;*	* CSD entry point (see documentation for interface)

cProc	CbSizeVidsCsd, <FAR, PUBLIC, ATOMIC>
cBegin	CbSizeVidsCsd

	xor	ax,ax			;* return 0 => not available

cEnd	CbSizeVidsCsd
;*****************************************************************************
endif	;* CbSizeVidsCsd_NonDefault


ifndef FSaveVidsCsd_NonDefault
;*****************************************************************************
;********** FSaveVidsCsd ********
;*	* CSD entry point (see documentation for interface)

cProc	FSaveVidsCsd, <FAR, PUBLIC, ATOMIC>
    parmDP pvidsSave
    parmDP pinst
cBegin	FSaveVidsCsd

	xor	ax,ax					;* failure

cEnd	FSaveVidsCsd
;*****************************************************************************
endif	;* FSaveVidsCsd_NonDefault


ifndef FRestoreVidsCsd_NonDefault
;*****************************************************************************
;********** FRestoreVidsCsd ********
;*	* CSD entry point (see documentation for interface)

cProc	FRestoreVidsCsd, <FAR, PUBLIC, ATOMIC>
    parmDP pvidsRestore
cBegin	FRestoreVidsCsd

	xor	ax,ax					;* failure

cEnd	FRestoreVidsCsd

;*****************************************************************************
endif	;* FRestoreVidsCsd_NonDefault


ifndef SaveVidDataCsd_NonDefault
;*****************************************************************************
;********** SaveVidDataCsd ********
;*	* CSD entry point (see documentation for interface)

cProc	SaveVidDataCsd, <FAR, PUBLIC, ATOMIC>
    parmDP pvidsSaveData
    parmD  lpwBuffer
cBegin	SaveVidDataCsd

cEnd	SaveVidsDataCsd
;*****************************************************************************
endif	;* SaveVidsDataCsd_NonDefault


ifndef RestoreVidDataCsd_NonDefault
;*****************************************************************************
;********** RestoreVidDataCsd ********
;*	* CSD entry point (see documentation for interface)

cProc	RestoreVidDataCsd, <FAR, PUBLIC, ATOMIC>
    parmDP pvidsRestoreData
    parmD  lpwBuffer
cBegin	RestoreVidDataCsd

cEnd	RestoreVidDataCsd
;*****************************************************************************
endif	;* RestoreVidDataCsd_NonDefault


ifndef EnableVidsMonitorCsd_NonDefault
;*****************************************************************************
;********** EnableVidsMonitorCsd ********
;*	entry:	fMonitorOn => monitor should be on
;*	* enable/disable INT 10 monitor
;*	exit:	n/a

cProc	EnableVidsMonitorCsd, <FAR, PUBLIC, ATOMIC>
    parmW  fMonitorOn
cBegin	EnableVidsMonitorCsd

cEnd	EnableVidsMonitorCsd
;*****************************************************************************
endif	;* EnableVidsMonitorCsd_NonDefault

