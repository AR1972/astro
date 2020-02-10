;*
;*	CW : Character Oriented Windows
;*
;*	inswap.asm : screen swapping (for installable screen)

.xlist
	include	user.inc

	include uisa.inc		;* for isa's
	include	screen.inc		;* screen stuff

	include	indrv.inc
	include	inscr.inc
.list

;*****************************************************************************


sBegin	DATA
    assumes DS,DGROUP

externB <instCur>		;* INST type
externB <insj>			;* INSJ type

sEnd	DATA


;*****************************************************************************


sBegin	SCREEN
    assumes CS,SCREEN
    assumes DS,DGROUP
    assumes SS,DGROUP


;*	* High level routines
;*	* (assume cPublic == FAR)



;********** CbSizeVids **********
;*	entry:	n/a
;*	* return the size of the VIDS structure needed for the current screen
;*		mode
;*	exit:	AX = size, or 0 => screen save not available

labelFP	<PUBLIC, CbSizeVids>
	jmp	insj.lpfnCbSizeVidsCsdInsj



;********** FSaveVids ********
;*	entry:	pvids = near pointer to VIDS structure
;*		pinst = near pointer to INST for new mode
;*	* fill *pvids with state of current screen mode (not screen data)
;*	exit:	AX != 0 if ok, == 0 if error

labelFP	<PUBLIC, FSaveVids>
	jmp	insj.lpfnFSaveVidsCsdInsj



;********** FRestoreVids ********
;*	entry:	pvids = near pointer to VIDS structure
;*	* restore video state with data in *pvids (not screen data)
;*	exit:	AX != 0 if ok, == 0 if error

labelFP	<PUBLIC, FRestoreVids>
	jmp	insj.lpfnFRestoreVidsCsdInsj



;********** SaveVidData ********
;*	entry:	pvids = near pointer to VIDS structure
;*		lpwBuffer = buffer to save data
;*	* save screen data into buffer
;*	exit:	n/a

labelFP	<PUBLIC, SaveVidData>
	jmp	insj.lpfnSaveVidDataCsdInsj



;********** RestoreVidData ********
;*	entry:	pvids = near pointer to VIDS structure
;*		lpwBuffer = buffer to save data (NULL => just clear screen)
;*	* restore screen data from buffer
;*	exit:	n/a

labelFP	<PUBLIC, RestoreVidData>
	jmp	insj.lpfnRestoreVidDataCsdInsj



;********** EnableVidsMonitor ********
;*	entry:	fOn => enable or disable monitor for video state
;*	* hook or unhook monitor for video state (eg. INT 10H hooks)
;*	exit:	n/a

labelFP	<PUBLIC, EnableVidsMonitor>
	jmp	insj.lpfnEnableVidsMonitorCsdInsj





sEnd	SCREEN

;*****************************************************************************

	END
