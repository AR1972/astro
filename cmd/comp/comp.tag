M000    9/24/90 NSM     COMP.C         Fix for PTRS 2656 2933 2977
                        COMP.H         Turn off APPEND before Search
                        MESSAGES.H     Fix Comp's output inconsistency &
                                       process switches that are run together

M001	9/26/90 NSM	COMP.C	       Checked for EOF while doing GETS
			MESSAGES.H     Treat '/' as == '\\' in file names.

M002    10/10/90 NSM	COMP.C		Problems with redirection.
			MESSAGES.H	getche is not redirected and so was 
					forced to use gets to get the answer
					for "COMPARE MORE FILES ?".

M003    12/27/90 MD     COMP.C          Correct problems with handling 
                                        of /? option.

M004    12/27/90 MD     MESSAGES.H      Correct syntax description of /N

M005	1/10/91  JDB    COMP.C          4134 - Only beep when DOS 4.0 beeped.

M006    1/21/91  JDB    COMP.C          5340 - Close the open files and free
                        COMP.H                 allocated buffer on exit.

M007	1/24/91  JDB	COMP.C		5292 - Define YES and NO for
			MESSAGES.H             international.

M008	2/4/91	 JDB	COMP.C		5515 - Fix Wildcard Compatibility
			COMP.H                 with DOS 4.01.

M009	2/15/91	 JDB	COMP.C		5929 - Fix STDIN redirection problem.

M010	2/21/91	 JDB	COMP.C		5982 - Fix /N=xxx problem when entered
                                               at the Option: prompt.

M011	2/22/91	 JDB	COMP.C		5985 - Fix /N=xxx problem when xxx is
                                               larger than one or both of the
                                               files.

M012	2/26/91  JDB	COMP.C		5929 - Fix redirection of stdin 
                        COMP.H                 thru command.com.

