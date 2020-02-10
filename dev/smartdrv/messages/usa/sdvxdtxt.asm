PAGE 58,132
;******************************************************************************
TITLE SDTEXT.ASM -- Text messages for SmartDrv VxD
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1991
;
;   Title:	SDTEXT.ASM -- Text messages for SmartDrv VxD
;
;   Version:	1.00
;
;   Date:	22-Nov-1991
;
;   Author:	RAL
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   22-Nov-1991 RAL Text messages for SmartDrv VxD
;
;==============================================================================


	.386p

	.XLIST
	INCLUDE VMM.INC
	.LIST


	PUBLIC	SDVxD_Error_Title_Msg
	PUBLIC	SDVxD_Write_Error_Msg
	PUBLIC	SDVxD_Write_Drive_Letter

VxD_DATA_SEG

SDVxD_Error_Title_Msg db "SERIOUS DISK ERROR", 0

SDVxD_Write_Error_Msg LABEL BYTE
		db 'A serious disk error has occurred while writing to drive '
SDVxD_Write_Drive_Letter db "?"
		db '.  Continue will retry the operation.', 0

VxD_DATA_ENDS

	END
