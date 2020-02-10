Revision tags for DOSSHELL\SHELL directory
------------------------------------------

Actually I made some changes to MAIN.C, TASKMAN.C and UTILS.C on June 04, 1991
to use initial CWD to be used as swap directory for ROMDOS purposes.
Later, when we decided to add the INI file variable SWAPDISK to specify
primary and secondary swap paths on Aug. 14, 1991, these changes were undone!


M010 SHK 08/13/91 UTILS.C	User can now control primary/secondary swap
		  TASKMAN.C	paths using the 'swapdisk' DOSSHELL.INI var.
				If this is not set, the DOSSHELL env variable
				is used, if set. Else default 50 behaviour.
				This change was prompted by ROMDOS -- dir
				where DOSSHELL is residing might not be 
				write-able!

M011 SHK 07/17/91 SCREEN.C	New mouse compatibilty message added. This
				now displays the mouse version number also.
		  TEXT.C	Same as above. Text strings for this msg.
		  UTILS.C	Modified fns FDlgmswarn() and WarnMouseIsOld()
				to put up our new mouse compatibility message.

M012 SHK 07/31/91 FILEOPS.C	Fixed bug in fn CopyIt(). It used to screw up
				if file size was between 64K-512 and 64K-256!

M013 SHK 10/02/91 TEXT.C	Certain mouse drivers don't respond to the
		  UTILS.C	GetMouseVersion() call. In these cases we
		  MOUSEBUG.ASM	get garbage back (whatever was in BX). In
				these cases we print out mouse version
				"Unknown" and also before we make the call
				zero out BX and check if it is still zero!

M014 SHK 10/03/91 SCREEN.C	Mouse Version number in the "old-mouse"
				warning	message is printed in hex now!
