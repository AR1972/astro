/*==========================================================================
*
*  Module:  txtdebug.c
*  System:  Quick BASIC Interpreter
*  Subsystem:  Text Manager
*
*=========================================================================*/
#include "version.h"

#if !CONINT_H
# include "conint.h"
#endif

#if !CONTEXT_H
# include "context.h"
#endif

#if !OPTABLES_H
# include "optables.h"
#endif

#if !OPCODES_H
# include "opcodes.h"
#endif

#if !PARSER_H
# include "parser.h"
#endif

#if !PRSTAB_H
# include "prstab.h"
#endif

#if !QBIMSGS_H
# include "qbimsgs.h"
#endif

#if !RTINTERP_H
# include "rtinterp.h"	/* only needed for DebChkStruct() */
#endif

#if !SCANNER_H
# include "scanner.h"
#endif

#if !TXTINT_H
# include "txtint.h"
#endif

#if !TXTMGR_H
# include "txtmgr.h"
#endif

#if !UI_H
# include "ui.h"
#endif

#if !VARIABLE_H
# include "variable.h"	/* only needed for DebChkStruct() */
#endif
