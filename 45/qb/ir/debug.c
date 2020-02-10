/*** 
*debug.c - utility routines for internal debugging of the interpreter
*
*	Copyright <C> 1986, Microsoft Corporation
*
*Purpose:
*	This module provides general purpose utilities for writing internal
*	debugging and other non-release code for the interpreter.
*
*******************************************************************************/
#include "version.h"

#if !CONTEXT_H
#include "context.h"
#endif

#if !HEAP_H
#include "heap.h"
#endif

#if !TXTMGR_H
#include "txtmgr.h"
#endif

#if !VARIABLE_H
#include "variable.h"
#endif
