=============================================================================
changes after ESP2
=============================================================================

M001 08/29/91 SMR	ROMHEAD.ASM	B#2499. Break the 'make' if we cannot
					find minicmd image below 1 Meg.


M002 08/30/91 SHK	MINICMD.ASM	B#2518. Make sure that there is a
					WORD 0 at the end of the ENV (to be
					compatible with COMMAND.COM).

M003 09/03/91 SHK	DATASEG.ASM	moved strings out to dataseg.inc to
					help international.
			DATASEG.INC	strings moved in here from dataseg.asm
			MAKEFILE	dependency added on dataseg.inc
			RAMMAKE		dependency added on dataseg.inc

M004 09/05/91 SHK	MINICMD.ASM	Removed almost all BUG BUGs and
					commented these lines better.

M005 09/09/91 SHK	DATASEG.ASM	Added an STI instruction at the
					beginning of Crit. Err. Handler.
					Added tags to the top of the file too.

M006 09/09/91 SHK	MINICMD.ASM	Changed a JZ to a JBE in function
					fn process_initfile. Added tags to
					the top of this file too.
