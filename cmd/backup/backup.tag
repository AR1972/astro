M000 - LF, Aug 9 1990 Bug #2236 - Basically, the routine check_drive_validity
(and subsequently all of Backup) depended on the first two arguments being
the source and target drives.  So I added a small loop which goes through
argv and makes sure that if there are drive arguments (actually it checks
for non-option arguments, i.e. arguments not beginning with '/') they
precede all other arguments.

M001 - LF, Aug 13 1990 Bug #2134 - the problem was that Backup didn't
check for Assigned/Joined/Substed drives, and if source drive happenned
to be assigned to target drive, making them the same drive, Backup didn't
catch this & consequently refuse to run, like it should.  So I added
code at the end of check_drive_validity which calls NameTrans (int 21
function 60) on source drive & checks to see if that == target, and
then calls it on target and checks to see if that == source, and refuses
to run if so.

M002 - LF, Sept 9 1990 Bug #2628 - fix number M001 had put the checks
for a/j/s drives at the end of the routine check_drive_validity, which
is a procedure which checks on the general validity of the drives, and
so is the most logical candidate of where to do the NameTrans check.
Unfortunately, NameTrans called with a drive argument that is a floppy
with no diskette/ unformatted diskette in it, dies, i.e. gets a critical
error.  And so I removed all the a/s/j stuff into its own routine,
check_asj, which I subsequently call at the FIRST available time after
the point where we know for a fact the diskette has been entered.

M003 - NS, Sept 24, 90 P3058 - If the target is a hardfile, and if backup has
been done once already, hitting ctrl-c at the warning message deletes the old
backup. The fix was to look at a flag (backup_started) for both removable and
non-removable targets before deleting the backup dir for ctrl-c-cleanup.
Also fixed p3065 - Issue a ioctl cat 9 call to determine whether a drive is
net or not and then treat redirected drives as non-removable; also cleaned up
some of the compiler warnings.
Also fixed P2954 & P2964 - Fixed code to get FORMAT.COM to format target disk.


M0004 - NSM Oct 9, 90 - P2954 - Backup and device names 
Moved the std set of reserved device names into a char array instead of
hundreds of "if ( (strcmp)||..)" sequences.
Also added check for device type if a findfirst on the soure file name 
succeeds. This will eliminate backing up other installed devices such
as  CLOCK$ etc... Enough of device name troubles..

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

M005	01/21/91 JDB	BACKUP.C	5100 - Allow user to try another disk
                        BACKUP.H	       if one cannot be formatted.

M006    02/05/91 MD     _parse.asm      5656 - Remove spurious IBMCOPYRIGHT flag

M007	02/06/91 JDB	BACKUP.C	5702 - Allow /F:2880 and all other
                        BACKPARS.H	       2.88M string combinations.

M008	02/07/91 JDB	BACKUP.C        5629 - Fixed /S option - if the file
                                               given does not exist in the 
                                               current directory, but DOES
                                               exist in a subdirectory.

M009	02/08/91 JDB	BACKUP.C	5756 - Fixed error condition for
                        BACKUP.H               write-protected disks.

M010	02/12/91 JDB	BACKUP.C	5477 - Do not set inheritance bit
                                               (open mode) for Extended Open
                                               to satisfy redirector of
                                               3+ Share Network.

M011	02/13/91 JDB	BACKUP.C	???? - Move message strings from
                        BACKUP.H               backup.h to common usa-ms.msg
                        BACKUP.SKL             file.

