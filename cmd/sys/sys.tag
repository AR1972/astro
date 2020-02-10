;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */


M000 - LF, 8/31/90, Bug 1047 - sys now copies command.com onto the
target disk.  Failure to do so (e.g. could not find command.com) is
NOT an error, but will result in a warning msg to the user.  The
main routine added is Copy_Command, which gets called in the main
routine iff the transfer of standard system files (io.sys, msdos.sys)
was successful.  Supporting routines are Get_Comspec, Env_length,
and String_len.

M001 - PYS, 9/18/90, Bug 2839 - sys won't copy command.com if
a parsing error occured.

M002 - PYS, 9/20/90, Bug 10 - sys remove the opposite operating
system if present.

M003 - PYS, 9/26/90, Bug 3167 - sys takes an ~54K or maximum of
memory to copy command.com.

M004 - PYS, 10/16/90, Bug 3492 - sys check for free space before moving
entries in the root directoy.

M005 - DLB, 11/16/90, Bug 2817 - Fixed problems involving large cluster #s.

M006 - DLB, 11/19/90, No bug report - Fixed incorrect tests for last FAT entry.

M007 - DLB, 11/20/90, Bug 4135 - Fixed over-write of root directory during the flush
		      of FAT buffer to FAT #2 in Check_FAT.

M008 - DLB, 11/21/90, Bug 4222 - Update CDS following move of subdirectory.

M009 - DLB, 01/09/91, Bug 4940 - Do not use COMSPEC unless it specifies
                                 COMMAND.COM.

M010 - DLB, 01/09/91, Bug 5000 - Send "System transferred" msg to STDOUT
                                 instead of STDERR.

M011 - DLB, 02/05/91  Bug 5632 - Fix computation of Transfer buffer size
				 (cbBuf). Display error message and exit if
                                 buffer too small.

M012    MD, 5/22/91     sys1.asm - Detect ROM version, exit with incorrect
                                   version error.
