	PAGE	,132
	TITLE	GWPLAYF - GW BASIC 2.0 Multi-Voice Play Function
;***
; GWPLAYF - GW BASIC 2.0 Multi-Voice Play Function
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;	Multi Voice Play Function Processor
;
; BASIC Syntax mapping to included runtime entry points:
;
; - PLAY Function:
;
;      v = PLAY(n)
;	     |
;	   B$FPLY
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	SN_TEXT 	
	USESEG	_DATA 		

	INCLUDE seg.inc 	


	INCLUDE	ibmunv.inc	

sBegin	_DATA			
	externW	b$SNDQCB	
sEnd	_DATA			

assumes CS,SN_TEXT		
sBegin	SN_TEXT 		


;***
;B$FPLY - play function
;
;Purpose:
;	Runtime Entry Point.
;	Syntax: X = PLAY (n)
;Entry:
;	numVoice = number of voice to get info for
;Exit:
;	[AX] = $donote(1,n), number of notes left in Q for this voice
;Uses:
;	Per convention
;Exceptions:
;	None
;****
cProc	B$FPLY,<PUBLIC,FAR>	
parmW	numVoice		
cBegin				


; this code extraced from B$RDPLAY to improve granularity and save code.
	MOV	AX,[b$SNDQCB].QUNOTE ; AX = the remaining notes in queue
				; (from sound-block)
cEnd				

sEnd	SN_TEXT			
	END
