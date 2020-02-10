	Title	Sharesr

;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;				   $SALUT (0,36,41,44)
				   include SHAREHDR.INC
;
;     Label: "The DOS SHARE Utility"
;
;******************* END OF SPECIFICATIONS *************************************

				   NAME Sharsr

				   .xlist
				   .xcref

				   include SYSMSG.INC

				   .cref
				   .list

				   MSG_UTILNAME <SHARE>

Share				   SEGMENT BYTE PUBLIC 'SHARE'

				   PUBLIC SYSDISPMSG, SYSLOADMSG, SYSPARSE

				   ASSUME CS:Share,DS:nothing,ES:nothing

					   ; include Message Code


				   .xlist
				   .xcref


				   MSG_SERVICES <MSGDATA>

				   MSG_SERVICES <LOADmsg>

				   MSG_SERVICES <DISPLAYmsg,CHARmsg>

				   MSG_SERVICES <SHARE.CLA,SHARE.CL1,SHARE.CL2>

				   .cref
				   .list


false				   =	0

DateSW				   equ	false
TimeSW				   equ	false
CmpxSW				   equ	false
KeySW				   equ	false
DrvSW				   equ	false
FileSW				   equ	false
QusSW				   equ	false
Val2SW				   equ	false
Val3SW				   equ	false


				   ; include parse.asm
				   .xlist
				   .xcref
				   include version.inc
				   include parse.asm
				   include msgdcl.inc
				   .cref
				   .list



Share				   ENDS
END

