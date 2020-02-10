Revision tags for COMMAND directory
-----------------------------------

M00	DBO	07/10/90	Bug #1234.  Separate translatable text from
				source files.  Create include files in
				messages directory.
M01	MD	07/13/90	Modified CLS and DIR to use ROM BIOS data
				area to determine screen height, in the
				absence of ANSI.SYS.  Affects TCMD2A.ASM
				and DIR.ASM

M002	SR	07/16/90	Bug #1621. Carousel seems to depend on the
				resize call to update the memory arena it
				builds to load command.com. Moved the resize
				call to the start to take care of this.

M003	SR	07/16/90	Added support for LoadHigh command.
				Files: RDATA.ASM, STUB.ASM, RUCODE.ASM
				       TDATA.ASM, TMISC1.ASM, UINIT.ASM
				       MAKEFILE, COMMAND.LNK		
				New File: LOADHI.ASM

M004	SR	07/17/90	Reworked initialization so that transient
				is moved to its final location only at 
				EndInit time by allocating the largest 
				available block, moving to the top of it and
				then freeing the block. Previously, init code
				would assume its block to be the largest 
				block, an assumption not valid if command.com
				is loaded into UMBs.
				Files: INIT.ASM, RDATA.ASM

M005	SR	07/20/90	Numerous hacks for Carousel:
				1. Set PSP at start to fix lie problem.
				2. Do not hook int 2fh if not first 
				command.com.
				3. Renormalize pointer at int 2fh entry
				point.
				Files: INIT.ASM, STUB.ASM

M006	SR	07/20/90	Fix batch file processing to handle the case
				when the batch segment is in an UMB.
				Files: TBATCH.ASM

M007	SA	08/01/90	Bug #1817. Modified DIR.ASM to allow /p to be
				used with /b.

M008	SA	08/01/90	Bug #1465. Deleted /h parameter.  Removed any
				internal handling of /?.
				Files: DIR.ASM, TDATA.ASM

M009	SR	08/01/90	Nesting of LoadHigh commands was causing
				unlinking of the UMBs. Fixed so that we 
				remember the UMB state before the Exec and
				restore it on return from the Exec.
				Files: LOADHI.ASM, RUCODE.ASM, STUB.ASM

M010	SA	08/05/90	Added support for /l (lowercase) option.
				Files: DIR.ASM, TDATA.ASM

M011	SA	08/05/90	Bug#714. Loaded first FCB with parsed drive
				number from within DIR.ASM.  This was not 
				being loaded by MS-DOS when the drive letter
				was preceded by any switch.

M012	DBO	08/06/90	Bug #1834: DIR and VOL are leaving APPEND
				disabled.  Problem is in resident HeadFix
				routine, where APPEND state is restored.

M013	SR	08/06/90	Changed code to use the new call to get
				info about whether DOS is in HMA, ROM etc.
				New call is Fn. 3306h.
				Files: INIT.ASM, TCMD2A.ASM

M014	DBO	08/08/90	Added /? messages for batch commands.

M015	SR	08/09/90	Increased default environment size from 160
				to 256 bytes
				Files: INIT.ASM

M016	SR	08/09/90	Give proper message on attempt to loadhigh
				batch files instead of "File not found".
				Also added "Invalid filename" message for
				LoadHigh.
				Files: LOADHI.ASM, TDATA.ASM, TRANMSG.ASM
				       COMMAND.SKL	

M017	MD	08/10/90	Fix for bug #1 - eliminate extraneous DEC
				of environment string.	TBATCH.ASM

M018	MD	08/12/90	Increment the screen height by 1 when
				obtained from ROM BIOS.  TCMD2A, DIR.

M019	SA	08/13/90	Bug#674.  Allow 'dir path\.ext'. This
				functionality was lost with the addition of
				some code in TENV2.ASM which checked for
				a '.' after the pathname.  Now do explicit
				check for '. ' or '..', and continue 
				successfuly if not found.

M020	SR	08/20/90	Bug #1921. Changed GetBatByt to not do reads
				from batch file once EOF is hit. REXX seems
				to depend on this.
				Files: TBATCH2.ASM

M021	SR	08/23/90	Fixed Ctrl-C hang at date/time prompt. Old bug
				that seems to have reappeared.
				Files: COMMAND1.ASM

M022	MD	08/29/90	Set correct video page before setting cursor
				position in REG_CLS.  File TCMD2A.ASM

M023	SA	08/31/90	Prevent DIR from failing when it encounters
				a subdirectory for which len(pathname)>MAXPATH.
				Now DIR just skips over that subdirectory.
				Files: DIR.ASM

M024	SR	09/05/90	Fixed bug #710 about comspec getting trashed 
				under certain conditions.

M025	SR	09/12/90	Removed code to call routines to turn critical
				error processing on for files. This used to
				generate a critical error if an attempt was
				made to read a file beyond EOF.
				Files: TCODE.ASM, TPIPE.ASM, TMISC1.ASM,
				TPRINTF.ASM

M026	SR	9/12/90		Fixed environment trashing on second Command
				if new comspec is changed.

M027	SR	9/20/90		Bug #2827. Actually, a pretty big bug. Some
				INIT seg variables were being used after 
				resize to resident. 
				Files: RDATA.ASM

M028	DBO	09/24/90	If country=US, sort filenames strictly in
				ASCII sequence, so DIR behavior matches
				MS-DOS SHELL.

M029	DBO	10/02/90	Bug #3212.  Make DEL /P respond to Y or N
				without ENTER.

M030	SR	10/3/90		Problem discovered by David Thielen while 
				booting command.com on DOS 6.0. Int 2fh 4a02h
				to get HMA address assumes a return value.
				Should not do this because no one may handle 
				it.

M031	SR	10/11/90	Changed open mode used by copy to deny write
				to fix sharing errors across the network
				while reading the same files. Bug #
				Files: COPY.ASM, COPYPR2.ASM

M032	DBO	10/23/90	Allow VER /R to report DOS internal revisions
				A through Z.

M033	DBO	10/25/90	Check for valid msg # in resident message
				retriever.

M034	DBO	10/25/90	Allow exit after executing int 2Eh command.
				Bug #2957.

M035	SR	10/27/90	Enable interrupts at start of dispatch code.
				Files: STUB.ASM

M036	SR	10/27/90	Free up environment segment passed to us by
				Exec. Leaves a hole but at least it is better
				than not freeing it.
				Files: RDATA.ASM

M037	SR	11/1/90		Bug #1745 & #3438. Added code to check for the EOF
				case and do special handling to fix these 
				problems.
				Files: TBATCH.ASM, TBATCH2.ASM

M038	SR	11/5/90		For Novell RPL. This dynamically changes a
				arenas on us and so command.com does a 
				checksum at the wrong place and tries to 
				reload transient which cannot be done until
				login is run.

M039	SR	11/19/90	Bug #4270. Loadhigh was not passing the 
				whitespace between the program name and its
				parameters as part of the program's command
				line. Fixed to do this.
				Files: LOADHI.ASM

M040	SR	11/28/90	Bug #3834. We restore the user dir on exit if
				this happens to be a transient command.com.

M041	SR	11/29/90	Bug #3806. On a del /p, if we get an access
				denied error on a file, we continue on to the
				next file instead of stopping.

M042	SR	12/13/90	Bug #4660. If messages are kept around, need
				to take care of the dummy segment that comes
				between the resident data & code segments.

M043	SR	12/17/90	Bug #4702. On devices, we should not do a 
				LSEEK to find out the filesize. Check if the
				handle belongs to a device or file before 
				doing the LSEEK.

M044	SR	12/17/90	Bug #4689. For Ventura's sake, do not free 
				the environment passed to us by Exec. Freeing
				it is good for memory savings, but leaves a
				hole behind that Ventura does not like.

M045	SR	12/21/90	Bug #2121. Chcp now gives a proper error 
				message if country.sys is missing instead of
				just saying "File not foun".

M046	SR	1/2/91		Bug #4935. Changed sharing permissions while
				opening files for copying so that we dont get
				sharing violations.

M047	SR	1/8/91		Bug #902,#4463. Fixed copy so that it 
				recognizes concatenation before it writes the
 				first file out.
				Files: COPY.ASM.

M048	SR	1/16/91		Bug #902,#5101. This bug was introduced as a
				result of change M047. It has all been fixed
				now.

M049	SR	1/16/91		Bug #5075. Changed the command.com stub
				scheduler so that all the entry points are now
				completely reentrant and also a lot faster.

M050    SHK	02/02/91	Bug #5586. When using the /p switch of the del
				command, Norton 5.0's Diskmon does not save
				and restore DTA correctly. So, we set this
				again befor we do our FindNext!

M051	SR	2/13/91		Bug #5863. The loadhigh code was jumping to
				the wrong spot to execute and so redirections
				were getting done twice so that the open on
				the file would still remain active to create
				sharing violations on later opens. 

M052	SMR	03/07/91	B#6297. Yanked command.com license expiration
				message.

=============================================================================
changes after ESP2
=============================================================================

M053 08/29/91 SMR makefile.oak	Control OBJ file copying by making it dependent
				on VERSION.INC. B#2556.


