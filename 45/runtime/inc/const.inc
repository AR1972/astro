;***
; const.inc - 13-Jul-87
;***
	.XLIST
;***
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	
; This include file contains constants for use with global flags.
;
;******************************************************************************


;	Equates for use with b$IOFLAG.

RED_INP		= 01H		; redirected input
RED_OUT		= 02H		; redirected output
LPR_ECHO	= 04H		; echo screen output to the line printer
F_KDSP		= 08H		; displaying function keys
IN_INPUT	= 10H		; output editing to screen and not to
				; 	printer or redirected stdout.
F_EDIT		= 20H		; in edit mode (in INPUT statement)
SCN_INIT	= 40H		; screen initialized -- screen IO done.
SCN_SCROLL	= 80H		; need to scroll the screen upon first
				; screen operation because we entered
				; on the last line of the screen.


;	Equates for use with b$CtrlFlags.

;[2]NoSTACKINIT = 01H		; disable B$STACKINIT
ULInited	= 02H		; User Library initialization done
TraceOn		= 04H		; Trace is on
DSwitch		= 08H		; /D specified
NoInitBcVars	= 10H		; Set if BC_VARS are already initialized
Detached	= 20H		;[7] Set if running as a DETACHed process


;	Equates for use with b$DebugFlags.  Added with [1].

BUF1_HELD	= 01H		; b$Buf1 is in use
BUF2_HELD	= 02H		; b$Buf2 is in use
BUF3_HELD	= 04H		; b$Buf3 is in use



;	Equates for use with b$DblFlags.  Added with [3]
;
; K_Interim_On must be 01H for INTERIM character support to work.  Many of
; the flags that were merged into this variable were local to a single
; file and could probably be overlaped if needed.
;

K_Interim_On	= 01H		;Are Interim characters currently on?  (0 = no)
K_Interim_Sup	= 02H		;Are we supporting Interim characters  (0 = no)
K_AddBlank	= 08H		;Do we replace last KANJI chr with ' ' (0 = no)
K_IgnBlank	= 10H		;Do we ignore blanks in our scan?      (0 = no)
K_Print 	= 20H		;Did we print the first half of a KANJI(0 = no)
K_FullMap	= 40H		;[5]Need to restore B$FIN scan pointer?(0 = no)

;	Equates for use with b$OGFlags.  Added with [6].

OGLine		= 0001H 	;Optional Line-drawing routine supported
OGBox		= 0002H 	;Optional Box-drawing routine supported
OGPutGet	= 0004H 	;Optional Put/Get routine supported
OGCircle	= 0008H 	;Optional Circle-drawing routine supported
OGCircleClip	= 0010H 	;Clipping and arbitrary aspect ratios supported
				;  in optional circle routine (not valid
				;  without OGCircle)
OGPaint 	= 0020H 	;Optional Painting routine supported
OGPaintClip	= 0040H 	;Viewport clipping supported in optional paint
				;  routine (not valid without OGPaint)
OGPaintTile	= 0080H 	;Tiling supported in optional paint routine
				;  (not valid without OGPaint)

	.LIST
