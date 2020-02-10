
M001	8/12/91 NSM	bios\POWER.ASM	APM support code added
			bios\POWER.INC	New int 2f interface to POWER$ 
					created
		new ->	bios\APMEQU.INC part of old IOCTL interface removed
			LPC.ASM

M002	8/21/91 NSM	bios\POWER.ASM Removed comments from Commented code in
				       in I14 hook (Bug#1639 - printing slowed
					down when printing to COM port).
				       Added call to CPU_BUSY after a RESUME.

M003	8/29/91 NSM	bios\POWER.ASM   Removed rest of IOCTL support code
					 Properly update POWER_STATUS flag
					  when APM is connected to and enabled.
		        bios\apmequ.inc  Corrected APM_SIGNATURE.
			lpc.asm		 Corrected I2F call to toggle SOUND.

M004		NSM	bios\power.asm	Moved CheckV86 calls only for DOING HLT and not
			?		for OEM_IDLE or for APM_IDLE (2587)
					Also fixed the problem with multiple resumes.
					If we get a resume without a initial suspend notifi.
					then we ignore this resume command now.
					Bug#2574 Count CPU_IDLE time at Int 1c


M005	9/08/92 DBO	bios\power.asm	Straighten out far calls in BIOS
					-resident driver.

M006

M007 09/11/91 SMR	MULT.INC	B#2669. Registered POWER's 2f channels
			POWER.INC	in mult.inc
			POWER.ASM
			PTIME.ASM
			LPC.ASM

M008 09/11/91 SMR	POWERMSG.INC	POWER.EXE renamed to SAVEPWR.EXE
			MAKEFILE

M009 09/11/91 SMR	LPC.ASM		Build non-debug version
			MAKEFILE

M010 09/12/91 SMR	LPC.ASM		PWR_API returns 0 in AX instead of no
					carry flag in case of no error
M011 09/17/91 NSM	LPC.ASM		AX==0 should not be checked for Install_
					Check call.	(Bug#2704)

M089 09/25/91 NSM	LPC.ASM		UI changes
			POWERMSG.INC	Message changes related to UI

M012 12/04/91 SMR	LPC.ASM		Match lengths before comparing
					TOKENS

M013 12/06/91 SMR	POWERMSG.INC	Make ADV:MIN level to 3

