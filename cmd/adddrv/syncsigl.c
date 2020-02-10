;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/***********************************************************************
 *
 *  SINCSIGL.C
 *
 *   public:
 *	set_sig_trap
 *	sig_stat
 *
 *  private:
 *	on_sig
 *	sig_flags
 *
 *  NOTE:
 *	After a signal invokes the handler, on_sig, the function sig_stat must
 *	be called to reenable that signal.  If this is not done soon, and many
 *	such signals occur, signals may be lost or overflow may occur.
 *	(This arrangement avoids loss of signals due to race conditions, but
 *	 is dependent on the os being able to handle an unlimited number of
 *	 signals of a given type.)
 *
 *  WARNING:
 *	    THIS MODULE MUST BE COMPILED WITH STACKPROBES DISABLED.
 *	    Since it contains Interrupt handlers that can be called from
 *	    within DOS (INT 24 time), __chkstk may not have consistent
 *	    information to work with.
 ***********************************************************************
 */
/***	Modification History
 *
 *	84/09/26  bryanwi	Original code
 *	84/12/06  bryanwi	Changed to handle signals with arguments, in
 *				accordance with new C signal interface.
 *				This adds a new argument to on_sig, which
 *				ignores it.
 *
 *	M000	04/02/85	gregti
 *
 *    - Added code to conditionally set up proper signal handling based
 *	on DOS version in use, ie. CTRL-C handling for pre 4.0 and signals
 *	for 4.0 and later.
 *
 *	M001	04/10/85	gregti
 *
 *    - Added definitions needed by above for get/set int vector and for
 *	the value of the int23 vector itself.
 *
 *	M002	06/06/86	alecb
 *
 *    - Fixed bug which resulted in possible corruption of the preserved int 24
 *	24 vector leading to a system crash if a floppy error occured during
 *	a restore operation.
 *----------------------------------------------------------------------
 *	MSKK01	June 1, 1987	akik
 *		Modify to ADDDRV command.
 *----------------------------------------------------------------------
 */

#include	"common.h"
#include	"types.h"

int (far *old23)();		/* M000 - Stores old int23 handler addr    */
int (far *old24)();		/* M002 - Stores old int24 handler addr    */

/* static array sig_flags is shared by the three functions in this file */
static int sig_flags[NSIG];



/*** set_sig_trap  --  set up synchronous signal trap
 *
 * Set_sig_trap inititalizes the static status variable for the indicated signal
 * to indicate that no such signal has yet occurred, and then calls signal to
 * establish on_sig as the handler for that signal.
 *
 * entry: sigtype  --  type of signal to set up for (See signal.h)
 *
 * externals: sig_flags
 *
 * effects: initializes sig_flags[sigtype], establishes on_sig as the handler
 *
 */
void set_sig_trap(sigtype)
	int	sigtype;
{
	void	on_sig();

	sig_flags[sigtype] = FALSE;

      /* Initialize Control C and INT 24 local handling. */
	switch (sigtype)
	{
	    case SIGINT:
		    setup23();
		break;

	    case SIGHDERR:
		    setup24();
		break;
	    default:;
	}
}



/*** on_sig  --  synchronous signal handler
 *
 * On_sig sets sig_flags[sigtype].
 *
 * entry: sigtype  --  type of signal which was raised
 *	  sigarg   --  argument associated with the signal
 *
 * externals: sig_flags
 *
 * effects: sets sig_flags[sigtype]
 *
 */
void on_sig(sigtype, sigarg)
	int	sigtype;
	int	sigarg;
{
	sig_flags[sigtype] = TRUE;
}



/*** sig_stat  --  report whether or not a given signal has occured
 *
 * Sig_stat checks sig_flags[sigtype] to see if that particular signal has
 * occured, if so, it returns true, otherwise false.  In either case
 * sig_flags[sigtype] will be reset.  After all this is done, on_sig will
 * be reestablished as the signal handler.
 *
 * entry: sigtype  --  type of signal to check on
 *
 * return: true if signal type sigtype has occurred, otherwise false
 *
 * externals: sig_flags
 *
 * effects: resets sig_flags[sigtype], reestablishes on_sig as handler
 *	    if the signal has been raised.
 *
 * warning: set_sig_trap MUST be called before this routine, or the results
 *	    sig_stat returns will be meaningless.  This constraint applies to
 *	    each kind of signal INDIVIDUALLY.
 *
 */
int sig_stat(sigtype)
	int	sigtype;
{
	int	tflag;

	tflag = sig_flags[sigtype];
	sig_flags[sigtype] = FALSE;
	return(tflag);
}



/* Setup23
 *
 *	    Setup23 installs the INT 23 handler ctlc.
 *
 *  INPUTS
 *	    None
 *
 *  RETURNS
 *	    None
 *
 *  SIDE EFFECTS
 *	    Int 23 vector set
 */
void	setup23()
{
	union REGS ir ;
	struct SREGS sr ;
	extern void ctlc() ;
	void (*pctlc)() = ctlc ;
	unsigned long pc = pctlc ;

	ir.h.ah = GETINT ;		/* 0x35 Get int vector		   */
	ir.h.al = INT23 ;		/* 0x23 Control-C interrupt	   */
	intdosx(&ir,&ir,&sr) ;

	FP_SEG(old23) = sr.es ; 	/* Old int23 handler ptr (seg)	   */
	FP_OFF(old23) = ir.x.bx ;	/* Old int23 handler ptr (off)	   */

	ir.h.ah = SETINT ;		/* 0x25 Set int vector		   */
	ir.h.al = INT23 ;		/* 0x23 Control-C interrupt	   */
	segread(&sr) ;			/* Get segment of ctlc()	   */
	sr.ds = sr.cs ; 		/* Load segment of ctlc()	   */
	ir.x.dx = FP_OFF(pc) ;		/* Load offset of ctlc()	   */
	intdosx(&ir,&ir,&sr) ;
}



/* reset23
 *
 *	    reset23 un-installs the INT 23 handler ctlc.
 *
 *  INPUTS
 *	    None
 *
 *  RETURNS
 *	    None
 *
 *  SIDE EFFECTS
 *	    Int 23 vector reset
 */

void	reset23()				/* M000 - Reset CTRL-C handler	   */
{
	union REGS ir ;
	struct SREGS sr ;

	ir.h.ah = SETINT ;		/* 0x25 Set int vector		   */
	ir.h.al = INT23 ;		/* 0x23 Control-C interrupt	   */
	sr.ds = FP_SEG(old23) ;
	ir.x.dx = FP_OFF(old23) ;
	intdosx(&ir,&ir,&sr) ;
}

/* Setup24
 *
 *	    Setup24 installs the INT 24 handler CritErr.
 *
 *  INPUTS
 *	    None
 *
 *  RETURNS
 *	    None
 *
 *  SIDE EFFECTS
 *	    Int 24 vector set
 */

void	setup24()
{
	union REGS ir ;
	struct SREGS sr ;
	extern void CritErr();
	void (*pCritErr)() = CritErr;
	unsigned long pCritHandler  = pCritErr;

	ir.h.ah = GETINT ;		/* 0x35 Get int vector		   */
	ir.h.al = INT24 ;		/* 0x23 Control-C interrupt	   */
	intdosx(&ir,&ir,&sr) ;

	FP_SEG(old24) = sr.es ; 	/* Old int23 handler ptr (seg)	   */
	FP_OFF(old24) = ir.x.bx ;	/* Old int23 handler ptr (off)	   */

	ir.h.ah = SETINT ;		/* 0x25 Set int vector		   */
	ir.h.al = INT24 ;		/* 0x23 Control-C interrupt	   */
	segread(&sr) ;			/* Get segment of CritErr()	   */
	sr.ds = sr.cs ; 		/* Load segment of CritErr()	   */
	ir.x.dx = FP_OFF(pCritHandler) ;/* Load offset of CirtErr()	   */
	intdosx(&ir,&ir,&sr) ;
}

/* reset24
 *
 *	    reset24 un-installs the INT 24 handler CritErr.
 *
 *  INPUTS
 *	    None
 *
 *  RETURNS
 *	    None
 *
 *  SIDE EFFECTS
 *	    Int 24 vector reset
 */
void	reset24()				/* M000 - Reset CTRL-C handler	   */
{
	union REGS ir ;
	struct SREGS sr ;

	ir.h.ah = SETINT ;		/* 0x25 Set int vector		   */
	ir.h.al = INT24 ;		/* 0x23 Control-C interrupt	   */
	sr.ds = FP_SEG(old24) ;
	ir.x.dx = FP_OFF(old24) ;
	intdosx(&ir,&ir,&sr) ;
}

/*--------------------------------------<SYNCSIGL.C end>--*/
