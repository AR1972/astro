;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

M000	SR	08/03/90	Fixed it so that Share does not load on a 
				Share /?does not install it. It displays
				help text and then exits.

M001	SR	9/10/90		Rewrote the int 2fh handler to get rid of all
				the special calls from Share and Ifsfunc. Also
				removed all the /NC support from Share.
				Files: GSHARE.ASM, GSHARE2.ASM.

M002	SR	9/26/90		Bug #3013 fixed. Check for message class also
				because parse error message and Share 
				installed message have same message numbers.
				Files:GSHARE2.ASM

M003	SR	10/1/90		Bug #2425 fixed. Do not insert duplicate SFT
				into Share database.
				Files:GSHARE.ASM

M004	HKN	10/18/90	To determine whether the SFT is a net SFT or
				a device we must check the sf_flags field. 
				bug #3584

M005	SR	10/16/90	Bug #2914 fixed. Share ignores /NC switch
				now.
				Files:GSHARE2.ASM

M006	SR	10/22/90	Bug #3583. Share now gives an error message
				if the same switch is used twice and uses
				only the value specified first.
				Files: GSHARE2.ASM, SHARE.SKL.

M007	SR	11/16/90	Bug #4240. Share now checks to see if the
				Dosshell switcher is currently active and
				does not install if it is present.

