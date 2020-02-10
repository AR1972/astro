/***
*vardebug.c - non-release code to validate varmgr.c
*
*  Copyright <C> 1986, Microsoft Corporation
*
*Purpose:
*  contains routines used for debugging the variable manager.
*
*******************************************************************************/

static	char tmpNameAry[60];  /* array to temporarily hold name of variables  */
static	cTotEntries;	      /* count of entries in a hash table	      */
static	cMaxChain;	      /* count of entries in longest hash chain       */
unsigned short fOutAll = 0;   /* if FALSE, don't print all info when dumping
                                 tables; used for internal testing            */
char fSkipDrive = 0;	      /* if non-zero, bash the drive letter when
				 printing a filename so test suites can be run
				 from any drive.			 */

static unsigned short recurseLevel = 0;
			      /* used for recursing in record variables      */

#pragma data_seg(C_STRINGS)

#include "version.h"
