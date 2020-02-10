/* Executor.h - type definitions and data primarily for the executor */

#undef EXECUTOR_H       /* it will have been defined as 0 in switch.h */
#define EXECUTOR_H ON   /* to prevent duplicate #include's */

/*========================================================================
  Executor static data
  ======================================================================*/

/* Begin of Statement Flags                                             */
EXTERNAL ushort PLM BosFlags;   /* Begin of statement flags */

/* NOTE: FBOSDEBUG should have the highest value of the FBOS bits 	*/
/* NOTE: FBOSSTOP  should have the next highest value.					*/
/* NOTE: FBOSEVENT should have the next highest value. 					*/
/* NOTE: see more complete comments in executor.inc for the reasons 
			for these order dependancies											*/

#define FBOSRESETSTK 0x1    /* Set by the CLEAR support code. Causes the stack
			       to be reset at next BOS/BOL (both SP & BP).
			    */
#define FBOSEVENT    0x2    /* Set by event handlers. Tells bos to service event
			    */
#define FBOSSTOP     0x4    /* Set by runtime when user hits ctl-break.
			    */
#define FBOSDEBUG    0x8    /* Set by B$FERROR when a runtime error occurs in
                               a module for which no error handler exists.
			       B$FERROR moves the oTxt back to the last opBos
			       prior to where the error occured, sets this bit,
			       and dispatches Also set by user interface when
			       any breakpoints are set, any watch expressions
			       are active, or when program tracing is active.
			       Causes next bos to enter UserInterface()
			    */

/*
   Current frame pointer, only valid when in the user interface (i.e. after
   UserInterface is called and before it returns).
   Used to impliment PStep.
*/
EXTERNAL ushort PLM FrameCur;
