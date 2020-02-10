/*** 
*qbidata.c - allocate global variable storage for EXTERNAL variables
*
*	Copyright <C> 1986, Microsoft Corporation
*
*Purpose:
*	Variables declared with the EXTERNAL declaration are included as
*	'EXTERNAL' in every other module, but not in this module - - - this
*	module is linked in for the express purpose of allocating storage for
*	these variables (mrsCur, etc.) in just one place. DEFINE_VARIABLES
*	should be OFF in every module but this one (see interp.h & switch.h).
*
*******************************************************************************/
#define DEFINE_VARIABLES ON  /* note: must do this BEFORE version.h included! */

#include "version.h"

#if !CONTEXT_H
#include "context.h"
#endif

#if !EXECUTOR_H
#include "executor.h"
#endif

#if !HEAP_H
#include "heap.h"
#endif

#if !PARSER_H
#include "parser.h"
#endif

#if !PSINT_H
#include "psint.h"
#endif

#if !RTINTERP_H
#include "rtinterp.h"
#endif

#if !SCANNER_H
#include "scanner.h"
#endif

#if !TXTINT_H
#include "txtint.h"
#endif

#if !TXTMGR_H
#include "txtmgr.h"
#endif

#if !UI_H
#include "ui.h"
#endif

#if !VARIABLE_H
#include "variable.h"
#endif
