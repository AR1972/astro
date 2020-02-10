/*** 
*dshcmd.c - dumb shell debug command processor
*
*	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
*
*Purpose:
*	For versions of QB-technology based BASIC built without the real user-
*	interface.
*
*******************************************************************************/

#include "version.h"

#if !CONTEXT_H
#include "context.h"
#endif

#if !CONINT_H
#include "conint.h"
#endif

#if !EXECUTOR_H
#include "executor.h"
#endif

#if !HEAP_H
#include "heap.h"
#endif

#if !LISTER_H
#include "lister.h"
#endif

#if !NAMES_H
#include "names.h"
#endif

#if !PARSER_H
#include "parser.h"
#endif

#ifndef RTINTERP_H
# include "rtinterp.h"
#endif

#if !RTTEMP_H
# include "rttemp.h"
#endif

#if !SCANNER_H
#include "scanner.h"
#endif

#if !TXTMGR_H
# include "txtmgr.h"
#endif

#if !UI_H
#include "ui.h"
#endif

#if !UTIL_H
#include "util.h"
#endif

#if !VARIABLE_H
#include "variable.h"
#endif
