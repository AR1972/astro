;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1981-1991
; *                      All Rights Reserved.
; */
M000 7/17/90 CAS	sysinit1.asm	moved FEMsg into msbio.cl6
			msbio.skl

M001 7/19/90 SMR	MSBIO1.ASM      Changed the A20 enabling strategy
					in the int 13 handler

M002 7/19/90 SMR	SYSCONF.ASM	Leaving the UMBs in an unlinked
			SYSINIT1.ASM	state

M003 7/20/90 SMR	SYSCONF.ASM	Fixed parse bug with DOS= line

M004 7/25/90 SMR	SYSCONF.ASM	Fixed bug number 2005

M005 7/25/90 SMR	SYSCONF.ASM	Fixed a bug in umb_insert

M006 7/25/90 SMR	SYSINIT2.ASM	Removed an unused procedure skip_delim

M007 7/25/90 SMR	SYSCONF.ASM	DeviceHi flag was not reset

M008 8/02/90 CAS	msint13.asm	Fixed bug 335 by removing find_bds
					 call in DMA-wrap code

M009 8/03/90 SMR	SYSCONF.ASM	Fixed bug # 2091. Problem with
					miltiple DDs in a single file.

M010 8/03/90 SMR	MSINIT.ASM	IFDEFd usage of extended keyboard
					read function (for LOTUS METRO)

M011 8/07/90 CAS	msinit.asm	rewrote lots of spaghetti code
			msbio1.asm	used for initializing hard drive
					partitions.  fixed bugs 2141, 2204,
					1866 and 1809 and prepared for
					zenith/etc. support

M012 8/13/90 SMR	SYSINIT1.ASM	Changed sysinit NOT to allocate mem
					for DOS from DOS when DOS=LOW

M013 8/13/90 SMR	MSCHAR.ASM	Fix for bug #66

M014 8/17/90 CAS	msint13.asm	Fix bug #2285, eliminating fault
					condition by accessing word 0ffffh
					on '386 in rare cases

M015 8/17/90 CAS	msbio2.asm	Fix bug #1553, patch around bug in
					certain old COMPAQ '286 disk BIOS's
					by installing a special int13 hook
					that sets DS:=40h on some calls

M016 8/20/90 SMR	SYSCONF.ASM	Enabled dummy /X option for buffers=
					so that old config.sys files don't
					have any problems.


M017 8/20/90 SMR	SYSCONF.ASM	Dummied out KEEP count in FCBS=

M018 8/21/90 CAS	msinit.asm	enabled multiple primary DOS partitions

M019 8/23/90 CAS	msinit.asm	final cleanup pass on M018 stuff

M020 8/27/90 SMR	SYSCONF.ASM	Updating sysinit's PSP:2 to take care
					of .COM files loading at device driver
					loadtime using periscope's sysload.sys

M021 8/28/90 SMR	MSINIT.ASM	Fix for AT&T 6300 WGS ROM BIOS bug

M022 8/29/90 MRW	SYSINIT1.ASM	Improved method for calling seg_reinit

M023 8/29/90 SMR	SYSCONF.ASM	Increased country='s buffer from 2K to
					6K.

M024 8/30/90 MRW	SYSINIT1.ASM	Have LoadDOSHiOrLow call TryToMovDOSHi
					instead of doing all the work itself.

M025 8/31/90 MRW	SYSINIT1.ASM    Restructuring to facilitate ROMDOS:
					call dos_segreinit before calling
					AllocFreeMem.

M026 9/02/90 SMR	MSCHAR.ASM	Putting back the buggy 4.01 code
					in AUXIN (timeout bug)

M027 9/06/90 SMR	SYSCONF.ASM	Fix for bug #2631

M028 9/10/90 SMR	MSBIO1.ASM	Added INT 10h to the list of INT vec
					tors to be restored by BIOS at INT 19

M029 9/12/90 CAS	msinit.asm	fixing bug #784, >26 logical drives
			sysinit1.asm
			sysimes.asm
			msbio.skl

M030 9/12/90 CAS	msint13.asm	fixing bug 2572
			msbio1.asm

M031 9/13/90 CAS	msinit.asm	fixing bug 526

M032 9/21/90 CAS	msinit.asm	a little optimization I couldn't resist

M033 9/24/90 SMR	MSBIO1.ASM	Saved & restored EOT field in DPT
			MSDISK.ASM

M034 9/25/90 CAS	msinit.asm	fix bug 2947 - zero TOTAL&BIGTOTALSEC

M035 9/26/90 CAS	msinit.asm	fix bug 3066, >2 floppies

M036 9/26/90 SMR	MSINIT.ASM	Added VDISK header clearing code at
			MSBIO1.ASM	INT 19 & CTRL ALT DEL

M037 9/26/90 SMR	MSINIT.ASM	Changed 2.88 MB drives FF to 9
			SYSINIT2.ASM

M038 9/27/90 CAS	msinit.asm	fix bug 3040, windows on 2nd primary

M039 10/01/90 SMR	MSDISK.ASM	Changed read_sector proc so that
					error 6 is not counted in the retry
					count.

M040 10/01/90 CAS	msinit.asm	fixed bug with certain partitions
					introduced by bug fix M034

M041 10/10/90 SMR	MSBIO1.ASM	PCKWIK support in INT 13 handler
					(save/restore A20 status for every
					 transition between HMA & LOW mem
					 in INT 13 handler)

M042 10/10/90 CAS	msinit.asm	implementing unformatted_media bit
					 on invalid bpbs and logical sectored
					 disks

M043 10/17/90 SR	msinit.asm	Added special code to check for IBM
			msbio.skl	machines and boot on them only. This
					code will be activated by a build
					flag.

M044 10/19/90 SMR	MSCHAR.ASM	fix for bug # 3479

M045 10/22/90 SMR	SYSCONF.ASM	Backing out "Refusal to load" OS/2 DDs

M046 10/24/90 CAS	msint13.asm	Restoring "lost" ECC bug fixes

M047 10/25/90 CAS	sysinit2.asm	Restoring "lost" DRIVPARM fixes

M048 10/27/90 SMR	MSINIT.ASM	using AAM & AAD for bin_to_bcd &
					bcd_to_bin

M049 10/29/90 HKN	sysconf.asm	initialize the device header of block
					devices with unit count.

M050 10/29/90 SMR	SYSCONF.ASM	Changed parse limit of buffers from
					10000 to 99

M051 10/30/90 HKN	sysinit2.asm	Put a blank at the end of devicehigh
					line in organize routine.

M052 10/31/90 SMR	MSBIO2.ASM	Resolved the confusion of media byte
			MSDISK.ASM	F9. BIOS used to default to 1.2M
					drive if formfactor was != ffsmall
					& mediabyte = f9.
;

M053 11/20/90 CAS	msinit.asm	recognizing OS/2 2.0 boot records

M054 11/30/90 CAS	msinit.asm	allowed MSDOS.SYS loading from beyond
			msload.asm	first 64K of FAT.

M055 12/10/90 PYS	msinit.asm	Corrected wait for system event calls.
			mschar.asm

M056 12/13/90 SMR	MSLOAD.ASM	Added RPL support, so that RPL's fake
			SYSINIT1.ASM	INT 13 code can be safe from SYSINIT &
					transient portion of COMMAND.COM

M057 12/18/90 SMR	MSDISK.ASM	Updating SDSB properly on SetOwner call
			MSDIOCTL.ASM

M058 1/2/91   SR	Msbio1.asm	Bug #4925. The CMS floppy driver
					assumes 70:b4 to be the address of
					the old int 13h handler save address
					and patched it with their address.
					This was true in DOS 4.01. Juggled
 					stuff to get this pointer at 70:b4.

M059 01/10/91 SMR	SYSCONF.ASM	Treat tick rollover byte as a count
			MSBIO1.ASM	instead of flag. However added a /t
			MSCHAR.ASM	option in switches= to revert to old
			MSDISK.ASM	way of handling rollover byte. B#5002

M060 01/11/91 SMR	MSDIOCTL.ASM	B#5065. Added retries to INT 13 ah=18h
					call made during format.

M061 01/28/91 CAS	msbio1.asm	B#5172 -- Align disksector for AMI bug

M062 1/29/91 CAS	msbio2.asm	B#5230 -- timing problem in old IBM AT
					ROM Int13 patch.

M063 01/29/91 SMR	SYSCONF.ASM	B#4984. Added SWITCHES=/W to suppress
					loading of Wina20.386 from the boot
					drive.

M064 01/29/91 SMR	MSBIO1.ASM	B#5070. Made changes NOT to preserve
					A20 status across INT 13 calls &
					Ensured A20 is ON before calling BIOS
					CODE in HMA.

M065 02/05/91 CAS	msinit.asm	B#4945, COMPAQ buggy ROM check changed

M066 02/12/91 SMR	MSDIOCTL.ASM	B#5833. Modification M060 was required
					only for Toshiba machines and the
					problem on Toshiba machines can be
					solved by the 'setmedia' driver. So
					the retry for ah=18 call is not
					required. Also because of the retry &
					disk reset, format becomes slow on
					IBM external floppy disk drives which
					does not support the set_media_type
					call.

M067 02/25/91 HKN	sysinit1.asm	Set up the environment ptr to point to NULL when calling exec for
					install= command.

M068 03/07/91 SMR	MSINIT.ASM	B#6271. Added missed out RPL support.

M069 03/08/91 MD        SYSINIT1.ASM    Bug 6327.  Closing NMI window.

M070 06/03/91 MD        MSBIO.SKL       New message added for POWER.EXE.

=============================================================================
FIXES after ESP2
=============================================================================

M071 08/29/91 SMR	ROMRDHI.ASM	Made ROMDRIVEBASE_LO more readable
					(Bug# 2535 in DOS51 database)

M072 08/29/91 NSM	SYSINIT1.ASM	Do Power_Init after DOSHI stub is loaded
					(Bug# 2547 in DOS51 database)

M073 09/09/91 DBO	MSBIO1.ASM	Add far code pointers in power.asm to
					seg_reinit list.

M074 09/11/91 DBO	MSBIO1.ASM	Power management driver supplies
			MSCHAR.ASM	clock driver, replacing system clock
			POWER.ASM	driver.
			PTIME.ASM
			MSINIT.ASM

M075 09/11/91 SMR	POWER.ASM	B#2670. Get Idle Algorithm was
					trashing IDLE_FLG

M076 09/11/91 SMR	POWER.ASM	B#2668. Statistics were not being
					copied if the user buffer is bigger
					than the required size.

M077 09/11/91 SMR	MULT.INC	B#2669. Registered POWER's 2f channels
			POWER.INC	in mult.inc
			POWER.ASM
			PTIME.ASM
			LPC.ASM

M078 09/11/91 SMR	POWER.ASM	Use HLT even if the processor is
					in V86 mode

M079 09/11/91 SMR	POWER.ASM	PWR_API returns 0 in AX instead
					of carry flag

M080 09/11/91 DBO	POWER.ASM	Power management clock hooks int 6C
					for time/date updates.

M081 09/12/91 SMR	POWER.ASM	Call P_UpdFromCMOS directly if power is
					standalone

M082 09/12/91 SMR	POWER.ASM	Load ES:DI before dispatching a CLOCK$
					call.

M083 09/13/91 SMR	MSINIT.ASM	Take out bios' INT 6C hook if POWER
					built into BIOS.
M084 09/17/91 NSM	MSBIO1.ASM	Remove IOCTL support for Built-in POWER
					(Bug #2701)
			POWER.ASM	Bug #2704: Zero out AX for no_error in
			POWER.INC	PWR_Services proc instead of in 
					int-2f handler; (version check returns
					version no in AX)

M085 09/19/91 SMR	POWER.ASM	Return break address from Clock driver
					init too!

M086 09/19/91 SMR	POWER.ASM	Save/Restore SI in Get/Set stat call

M087 09/23/91 NSM	POWER.ASM	B#2756 Change Signature to 504dH

M088 09/25/91 NSM	POWER.ASM	B#2730 Hang in a GET_STAT call corrected 
M089 09/25/91 NSM	POWER.ASM	UI related changes to I2F service code

M090 09/25/91 NSM	POWER.ASM	WIN enh mode slows down I1C rate. To
			PTIME.ASM	fix this, we update our time from CMOS
			POWER.INC	periodically under WIN enh mode.
					(B# 2729)

M091 09/25/91 NSM	POWER.ASM	To be nice to user on APM machines in
					POWER STD mode, we update our time from
					CMOS periodically just as we do in win
					ENH mode.

I070 10/01/91 JAH	SYSINIT1.ASM	IBM fix to work around bug in Tortuga
					ROM BIOS which causes machine to hang
					when clearing the HMA.

M092 10/18/91 NSM	POWER.ASM	(Bug#: 2814,2832, & 2862)
			POWER.INC	Change CLOCK again. We don't maintain
			PTIME.ASM	Tick count anymore and depend on BIOS
					Tick Count. But we do maintain daycount
					;We don't increment when a rollover
					happens;instead, we get latest date from
					CMOS, at the time of next read.
					Also Check added for Prev.loaded copy of
					POWER and if POWER already exists, we
					disable it and take over.

M093  10/25/91 NSM	POWER.ASM	Reg (CX) trashing while making suspend/
					stand-by APM call in Int1C handler was
					fixed.
					Also code to update the APM pollcount
					counter was added. (we were updating
					only the refernce counter while changing
					APM poll count leaving the current 
					counter alone).

M094  10/29/91 NSM	POWER.ASM	B#2923 & 4. Trashing of AX and BL	
					registers in some of the INT 2f service
					routines were fixed.


M095  10/30/91 JAH	MSBIO2.ASM	B#2914. Dislaying wrong drive letter
					on single drive system and prompting
					for drive B:.

M096           NSM

M097  11/07/91 SMR	MSBIO1.ASM	B#2958. Define endfloppy label after
					ROMDRIVE & POWER code, so that we
					do not overwrite ROMDRIVE and POWER on
					a harddiskless system

M098  11/19/91 SMR	POWER.ASM	Moving POWER into UMB automatically

M099  11/26/91 SMR	POWER.ASM	Remember PSP of 2f instead of maintaining
					a flag in the app's PSP

M100  11/26/91 SMR	POWER.ASM	Instance the PSPOf2fApp variable


M101  11/26/91 NSM	PTIME.ASM	Issue int 1a fn 0 instead of looking
					at 40:6ch for time_read (and also
					look for date rollover).

M102  11/27/91 SMR	POWER.ASM	Reverse the order of instancing
					& Do definite idle on INT 2a

M103  12/03/91 NSM	POWER.ASM	Check for IDLE_DETECTION OFF before
					doing idles for I2f,I2a,and blocked
					keyboard IO (at I2fIdleEntry instead
					of at Do_Idle)

M104  12/03/91 SMR	POWER.ASM	Do APM Command line settings only in
			LPC.ASM		the second copy of POWER if moved
					into a UMB

M105  12/04/91 SMR	POWER.ASM	Fake out arena header & sub-arena header
					so that MEM.EXE will see POWER properly.


M106  12/06/91 NSM	POWER.ASM	Change adaptation algorithm ( do avg
					every 16 samples).

M107  12/06/91 SMR	POWER.ASM	Issue int 1A to clear BIOS rollover flag

M108  12/10/91 NSM	POWER.ASM	Put APM I2f service proc within IFDEF 
					INCL_APM directives
