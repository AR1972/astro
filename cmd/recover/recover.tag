M001 20-Aug-91	DLB recover.asm	Bug #1622: Recover was using reserved field
		    makefile	of FCB to get pointer to SFT; the meaning of
				this field changed in DOS 5 when SHARE.EXE
				is not loaded. In DOS 5, there is no need
				to update SFT when SHARE is not loaded; the
				SFT is re-generated from the FCB for local
				non-shared files.

M002 20-Aug-91	DLB recover.asm	Bug #????: Fix problem adjusting size of
				recovered files.

M003 20-Aug-91	DLB recover.asm Bug #????: Fix 12-bit FAT trashing bug;
				When marking an even cluster # as bad, Recover
				was corrupting the lowest nibble of next
				cluster.
