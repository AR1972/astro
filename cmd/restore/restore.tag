;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

M000 - LF  Aug 6, 1990  In the new section of code IBM added to implement
the /Y switch, I've disabled the 'seconds checking' (i.e. making sure that
the seconds component of the switch the user entered matches the second
component in the timestamp for the file) because most users don't think to
add the seconds in a time specification, not to mention that I can't think
offhand of any easy way for them to have gotten that information to begin
with.  The seconds check would render this switch basically useless, so
I've commented it out.

M001 - NS Sept 25,1990. Fixed the problem of treating Network drives as 
removable drives. Backup was treating them as non-removable, did backup on a 
subdir; but restore was looking for files in the root; all this was mainly 
due to bugs in code.  Added code to issue ioctl cat 9 call to find out whether
a drive is net or not if we get an error in ioctl cat 8 call.
Cleaned up some of the prototypes and function formal parameters to get less
warnings in build.

