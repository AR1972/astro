COPYRIGHT_1             db 13,10,"Copyright (C) 1987-1990  Central Point Software, Inc. ",13,10,10,'$'
COPYRIGHT_3             db 13,10,"Creates an image of the system area.",13,10,'$'
BAD_VERSION             db 13,10,"DOS version must be 2.0 or higher.",13,10,'$'
NETWORK_INSTALLED       db 13,10,"The MIRROR command cannot operate on a network drive.",13,10,'$'
MSG                     db 13,10,"The MIRROR process was successful.",13,10,'$'
FAIL_MSG                db 13,10,"The MIRROR process was unsuccessful.",13,10,'$'
DRIVE_MSG               db 13,10,"Drive "
DRV_NAME_LEN            equ ($ - DRIVE_MSG)
                        db "x ",'$'
READ_ERROR_MSG          db "could not be processed.",13,10,'$'
PROCESSING_MSG          db "being processed.",13,10,'$'
DRIVE_SPEC_ERROR        db "Error reading system areas.",13,10,'$'
WRITE_ERROR_MSG         db "Error writing the MIRROR image file.",13,10,'$'
WRITE_SYS_ERROR_MSG     db "Error writing system areas.",13,10,'$'
TRACK_ERROR_MSG         db "Error tracking the MIRROR image file.",13,10,'$'
OPEN_MSG                db "error. Could not open the MIRROR image file.",13,10,'$'
NO_SAPACE               db "error. Insufficient space for the MIRROR image file.",13,10,'$'
CLOSE_MSG               db "error. Could not close the MIRROR image file.",13,10,'$'
NO_MEM_MSG              db "error. Insufficent memory to read all system info.",13,10,'$'
BOOT_BAD                db "error. The boot sector for this drive"
                        db 13,10,"is incompatible with the MIRROR command.",13,10,'$'
FULL_MSG                db "error. Insufficient space for the MIRROR control file.",13,10,'$'
NO_SPOTS_ERROR_MSG      db "error. There are no entries available in the root directory"
                        db 13,10,"of the drive.  Delete one or more files from the root directory.",13,10,'$'
NO_DIRS_ERROR_MSG       db 13,10,"file in the root directory of the hard drive.",13,10,'$'
UPDATE_OURS_ERROR_MSG   db "error. Cannot update the MIRROR control file.",13,10,'$'
CANT_REUSE_ERROR_MSG    db "error. There already is a MIRROR control"
                        db 13,10,"file but it was not located in the last 25 percent of the"
                        db 13,10,"data area. It has now been deleted and processing"
                        db 13,10,"will continue.",13,10,'$'
FOUND_ONE_LIKE_US_MSG   db "error. Data was found in a file that "
                        db 13,10,"appears to be a MIRROR control file. Cannot continue.",13,10,'$'

