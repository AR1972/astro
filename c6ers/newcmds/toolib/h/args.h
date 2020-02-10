#ifndef ARGS_H
#define ARGS_H (1)

// args.h - definitions for argument parsing

#ifndef MAPSTR_H
#include <mapstr.h>
#endif // MAPSTR_H

// basic lookup structures
typedef char *ArgName;
typedef int *ArgOrder;

// argument detection routine
typedef int (*ArgTestFunc) ( char *, char ** );

int argStdNameTest (
        char *szArg,
        char *szMrk,
        char **pszText
        );

extern MapItemPtr argMapSplit (
        char *szArg,
        char *szSplit,
        MapItemPtr pMapTable,
        char **pszValu
        );

// configuration routines
extern void argConfig (
        MapItemPtr pNames,
        ArgOrder piSeqn,
        int cSeqn,
        char *szMrk,
        char *szSep
        );

extern void argSeqn (
        ArgOrder piSeqn,
        int cSeqn
        );

// argv transfer
extern void argOpen (
        char **argv
        );

extern char **argShut ( void );

// argument handling
extern int argName (
        int bad,
        ArgName *pszText
        );

extern int argPeek (
        int bad,
        ArgName *pszText
        );

extern char *argValu ( void );

#endif // ARGS_H
