/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * MSD.C - Source file with the main() routine and supporting
 *   start-up routines.
 *********************************************************************/


/* Include Files */

#include "msd.h"
#include "_msd.h"

#ifdef CW_INCLUDED

#include "cgraphic.h"
#include <process.h>

AY FAR PASCAL GetUILine (void);


#endif /* CW_INCLUDED */


/*********************************************************************
 * main - Beginning of MSD program
 *
 * argc   - Count of arguments
 * argv[] - Array of strings containing the arguments
 ********************************************************************/

INT _cdecl main (INT argc, PSZ argv[])
{
  BOOL fReturnValue;           /* Return value from called functions */


  /* Reset disk to flush disk cache */

  bdos (0x0D, 0, 0);

  /* Initialization */

  InitParm1 (argv[1]);

  /* Process the command line */

  fReturnValue = ProcessCmdLine (argc, argv);


  /* Set some global variables */

  SetMiscGlobals (argv[0]);

  if (fReturnValue)
    return (fReturnValue);


  /* If we're in Report Only mode, get the info and print it out */

  if (fReportOnly)
    return (ReportOnly (pszReportFilename));


#ifdef CW_INCLUDED

  /* Otherwise, start the CW interface */

  BeginCwInterface();

#endif
}
