;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

M001 08/13/90 SMR	RESCODE.ASM	The INT 17 handler retruns 0ffh when
					an infinite retry loop is broken by
					a CTRL BRK by the user.
M002 09/07/90 PYS	MODECOM.ASM	2677: Check port status to see if we
					were able to do a normal or extended
					init to the com port.
M003 09/10/90 PYS	MODEPRIN.ASM	2717: Mode lpt1,,p on redirected
					printer prints out garbage.
M004 09/11/90 PYS	MODECOM.ASM	2761: Correction of 2677 created this
					bug. Detection of no extended init
					support redone.
M005 09/29/90 SMR	INVOKE.ASM	device_type was not being set to
					COMx when get_device_retry_type was
					called from status_for_everything

M006 10/12/90 NSM	MODECP.ASM	P3366:Mode was reading the font file 
			MODEMES.ASM	into its own stack. Changed to leave 
			MODEDEFS.INC	room for the stack and use the remain. 
			MODECPMS.INC	mem for reading font file; If no mem 
					available after leaving room for stack,
					quit saying "insuff.mem".

M007 01/07/91 SMR	RESCODE.ASM	Bug#4058. Instead of checking just
					for DSR, check for DSR OR CTS to find
					whether printer is ready or not.


