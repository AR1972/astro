M000	SR	08/24/90	Fixed 2 problems:
				1. Insufficient memory message not printed
				2. Unnecessary reserving of 64K above itself
				because of which it would not load in UMBs.

M001	SR	08/31/90	The stupid guys had sp = 0 even though they
				were allocating a stack segment. How did it
				work for so long?

M002    MD      09/17/90        Fix for IBM bug: FASTOPEN C:=(,20) would
                                hang system.  Name cache size not properly
                                initialized in this case.

M003	MD	09/19/90	Make code and equate for default name cache
				size consistent.

M004    MD      10/09/90        Removed obsolete messages from SKL file

M005    MD      10/14/90        Removed reference to IBMCOPYRIGHT

M006	NSM	11/07/90	Added check for the presence of DosShell

M007	SR	1/22/91		Bug #5263. Fastopen was not initializing the
				far ptr to call the rename routine to point
				at the EMS page.
