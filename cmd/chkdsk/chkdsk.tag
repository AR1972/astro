
M001	NSM	1/16/91		Fixed problem in reporting total mem size.
				call INT 12 to get mem size instead of reporting
				top_of_mem from PSP. Do not add XBDA size to 
				total mem if XBDA is moved into conv. mem by
				EMM386. 
				Chkdsk2.asm

M002	NSM	1/21/91		Fixed a stack overrun check. Made the fatal
		P5195		msgs dsp work properly (by making it display the
				msgs it is supposed to display!). 
				Also fixed a possible theoretical hang (disk
				corruption ?) by declaring a 2k sector buffer.
				By a comb. of 2 bugs, chkdsk was actally using
				less than a 512 byte buffer as its sector buffer
				overrunning its stack area.
				chkdisp.asm     - included sysgetmsg 
				chkdsk.skl      - new messages  
				chkdsk1.asm     - corrected stk overrun chk 
				chkequ.inc      - new equates for msgs 
				chkmsg.inc      - minor chgs  
				chkproc.asm     - msg no  for FATAL msg 
				chkproc2.asm    - FATAL proc corrected 
M003	NSM	1/30/91		Report proper FREE MEM report at end. See if
		P5195		we are loaded in conv. mem and if so take the
				top-of-mem figure to report free size. If we
				are loaded in UMB, then try allocating 0ffff 
				paras, and report the returned value as the 
				FREE mem size	
				chkdata.inc : a new var for top-of-mem	
				chkdsk1.asm : init top-of-mem var on load 
						new code for finding free mem
						if loaded in UMB
				chkdsk2.asm : new code for reporting free mem
				chkequ.inc  : new equate for UMB_HEAD


M004    MD      02/05/91        chkinit.asm : removed obsolete references
                                                to IBMCOPYRIGHT

M005	NSM	02/21/91	chkmsg.inc: changed some of the msgs to
				redirect to STDOUT instead of STDERR
				(P6022)
				chkdisp.asm: Saved and restored BP and ES.
				This used to cause hangs. (P6021)
				chkdsk1.asm: Commit STDOUT to avoid lost
				 alloc.chain problems (P6052)

M106    NSM     06/17/91	Fixed bug #1594 by zeroing high byte of CX
                                before moving FAT_COUNT to CL.
