;msdos.inc - MSDOS 2.x and 3.x low level macros and definitions.

;
; This is the simplest type of macro for making MSDOS 2.x/3.x calls.
; It does no error checking or status returns.
;
; The "function" is an integer constant only specifying the true call.
;
; The "string" is optional for specifying the value put into DX and is
; treated as an offset within dgroup.
;
; The "length" is option for specifying an integer value for placement
; in the CX register.
;

callos	macro function,string,length

ifnb <string>
	lea	dx,string
endif

ifnb <length>
	mov	cx,length
endif

	mov	ah,function
	int	21h

	endm

;
; Some common functions used in BIC
;

Msdos_Display_Char		= 2	;Send char to display
Msdos_Print_Char		= 5	;Send char to printer
Msdos_Put_Display		= 6	;Send char to display, no ctrl-c check.
Msdos_Get_Kybd			= 7	;Get key with no echo or ctrl-c check.
Msdos_Put_String		= 9	;Send $ terminated string to display.
Msdos_Get_Kybd_Status		= 11	;Get keyboard status.
Msdos_Reset_Drive		= 13	;Reset Current drive.
Msdos_Select_Drive		= 14	;Select drive.
Msdos_Get_Current_Drive		= 25	;Get current disk drive number.
Msdos_Set_DMA			= 26	;Set current DMA address.
Msdos_Get_Date			= 2Ah	;Get current Date.
Msdos_Get_Time			= 2Ch	;Get current Time.
Msdos_Get_DMA			= 2Fh	;Get current DMA address.
Msdos_Get_DOS_Version		= 30h	;Get's current DOS version number.
Msdos_Keep_Process		= 31h	;Terminate and stay resident.
Msdos_Get_Disk_Free		= 36h	;Get current Free disk blocks.
Msdos_Create_Directory		= 39h	;Create sub directory.
Msdos_Remove_Directory		= 3Ah	;Remove sub directory.
Msdos_Change_Directory		= 3Bh	;Change sub directories.
Msdos_Create_File		= 3Ch	;Create file (2.x style)
Msdos_Open_File			= 3Dh	;Open file (2.x style)
Msdos_Close_File		= 3Eh	;Close file (2.x style)
Msdos_Read_File			= 3Fh	;Read file (2.x style)
Msdos_Write_File		= 40h	;Write file (2.x style)
Msdos_Kill_File			= 41h	;Delete file (2.x style)
Msdos_Move_File_Pointer		= 42h	;Lseek() function.
Msdos_Change_File		= 43h	;Chmod() function.
Msdos_IO_Ctl			= 44h	;IO Control function for devices.
Msdos_Get_Current_Directory	= 47h	;Returns text of current directory.
Msdos_Alloc_Memory		= 48h	;Allocate additional memory.
Msdos_Free_Memory		= 49h	;Free additional memory.
Msdos_Modify_Memory		= 4Ah	;Modify allocated additional memory.
Msdos_Exec			= 4Bh	;Load and/or execute program.
Msdos_Terminate			= 4Ch	;Terminate job.
Msdos_Get_Child_Status		= 4Dh	;Gets Child's return status.
Msdos_Find_File			= 4Eh	;Find first occurance of file.
Msdos_Find_Next			= 4Fh	;Find next occurance of file.
Msdos_Move_File			= 56h	;Move file entry to newname/directory.
Msdos_Get_Error			= 59h	;Get MSDOS's extended error code.
Msdos_Create_Temporary_File	= 5Ah	;Create unique file.
Msdos_Create_New_File		= 5Bh	;Create file with unique name.
Msdos_Lock_Unlock_File		= 5Ch	;3.x file locking code.
