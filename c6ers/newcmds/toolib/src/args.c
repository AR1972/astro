// args.c -- Command line argument support
//

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "strpfx.h"

#include "args.h"

////////////////////
// EXPORTED STATE //
////////////////////

static
int isName (
    char *szArg,
    char **pszText
    );

ArgTestFunc argIsName = isName;

////////////////////
// INTERNAL STATE //
////////////////////

static char** pszArgCurr;

static MapItemPtr pArgNames;

static ArgOrder piArgSeqn;
static int cArgSeqn;
static int mArgSeqn;

static char* szArgMrk;
static char* szArgSep;

/////////////////////////////////
// Support for "bound" options //
/////////////////////////////////

static char* szArgValu;

////////////////////////////////////////
// argRead                            //
// - Map the current option to a code //
////////////////////////////////////////

static
int argRead (
    int bad,
    ArgName *pszText,
    char **pszValu,
    int bump
    )
{
    // assume we fail, for easy exit
    *pszText = NULL;
    *pszValu = NULL;

    // any thing to look at?
    if ( pszArgCurr == NULL ) return bad;
    if ( *pszArgCurr == NULL ) return bad;

    // explicit argument
    if ( (*argIsName) ( *pszArgCurr, pszText )) {
        MapItemPtr found;

        found = argMapSplit ( *pszText, szArgSep, pArgNames, pszValu );

        pszArgCurr += bump;

        return ( found ? found->value : bad );
	}

    // positional values
    if ( cArgSeqn < mArgSeqn ) {
        int found;

        found = piArgSeqn [ cArgSeqn ];

        cArgSeqn += bump;
        pszArgCurr += bump;
        *pszValu = *pszText;
	*pszText = NULL;

        return found;
	}

    // no apparent arguments
    return bad;
    }

/////////////////////////////////////////////
// isName                                  //
// - StdName wrapper with local test chars //
/////////////////////////////////////////////

static
int isName (
    char *szArg,
    char **pszText
    )
{
    return argStdNameTest ( szArg, szArgMrk, pszText );
    }

/////////////////////
// PUBLIC ROUTINES //
/////////////////////

/////////////////////////////////////////////
// argStdNameTest                          //
// - Determine if current option is a name //
/////////////////////////////////////////////

int argStdNameTest (
    char *szArg,
    char *szMrk,
    char **pszText
    )
{
    // not an arg if it doesn't starts with a mark character
    if ( strchr ( szMrk, *szArg ) == NULL ) {
	*pszText = szArg;
	return 0;
	}

    // not an arg if the mark character is repeated
    if (( strlen ( szArg ) >= 2 ) && ( szArg[0] == szArg[1] )) {
	*pszText = szArg + 1;
	return 0;
	}

    // must be an argument
    *pszText = szArg + 1;
    return ~0;
    }

////////////////////////////////////////////////////////////
// argMapSplit                                            //
// - Determine if the current option has a bound argument //
////////////////////////////////////////////////////////////

MapItemPtr argMapSplit (
    char *szArg,
    char *szSplit,
    MapItemPtr pMapTable,
    char **pszValu
    )
{
    char *szFlag;
    MapItemPtr item;
    int cFlag;

    char *szBreak = strpbrk ( szArg, szSplit );

    // easy case:  no options with argument
    if ( szBreak == NULL ) {
        *pszValu = NULL;
        return mapAbbrStr ( szArg, pMapTable, striprefix );
	}

    // hard case:  option appended to argument
    // - build temp string for lookup
    cFlag = szBreak - szArg;

    szFlag = malloc ( cFlag + 1 );

    if ( szFlag == NULL ) {
        *pszValu = NULL;
	return NULL;
	}

    strncpy ( szFlag, szArg, cFlag );
    szFlag [ cFlag ] = '\0';

    // set up values for argOpts
    *pszValu = szBreak + 1;

    // map the string
    item = mapAbbrStr ( szFlag, pMapTable, striprefix );

    // cleanup and go
    free ( szFlag );
    return item;
    }

///////////////////////////////////////
// argOpen			     //
// - Establish the list of arguments //
///////////////////////////////////////

void
argOpen (
    char **argv
    )
{
    // establish internal state
    pszArgCurr = argv;
    szArgValu = NULL;
    }

/////////////////////////////////////
// argShut			   //
// - Release the list of arguments //
/////////////////////////////////////

char **
argShut ( void )
{
    char **argv = pszArgCurr;

    // clear the internal state;
    pszArgCurr = NULL;
    szArgValu = NULL;

    // return the current argument
    return argv;
    }

////////////////////////////////////////
// argSeqn			      //
// - (Re)Set the positional arguments //
////////////////////////////////////////

void
argSeqn (
    ArgOrder piSeqn,
    int mSeqn
    )
{
    piArgSeqn = piSeqn;
    mArgSeqn = mSeqn;
    cArgSeqn = 0;
    }

//////////////////////////////////
// argConfig			//
// -  Create the internal state //
//////////////////////////////////

void
argConfig (
    MapItemPtr pNames,
    ArgOrder piSeqn,
    int mSeqn,
    char* szMrk,
    char* szSep
    )
{
    // establish internal state
    pArgNames = pNames;

    szArgMrk = ( szMrk == NULL ) ? "/-" : szMrk ;
    szArgSep = ( szSep == NULL ) ? ":=" : szSep ;

    argSeqn ( piSeqn, mSeqn );
    }

////////////////////////////////////////
// argName			      //
// - Map the current option to a code //
////////////////////////////////////////

int
argName (
    int bad,
    ArgName *pszText
    )
{
    return argRead ( bad, pszText, &szArgValu, 1 );
    }

////////////////////////////////////////
// argPeek                            //
// - Map the current option to a code //
////////////////////////////////////////

int
argPeek (
    int bad,
    ArgName *pszText
    )
{
    char *szTemp;

    return argRead ( bad, pszText, &szTemp, 0 );
    }

////////////////////////////////////////////
// argValu				  //
// - Get the next argument value (if any) //
////////////////////////////////////////////

char *
argValu ( void )
{
    char *szValu;

    // check for bound/positional values
    if ( szArgValu != NULL ) {
	szValu = szArgValu;

	// could actually parse out comma separated options here

	// make sure we don't hand out the same value twice
	if ( *szArgValu == '\0' ) return NULL;
	szArgValu += strlen ( szArgValu );

	// return bound value
	return szValu;
	}

    // no "bound" options
    if ( pszArgCurr == NULL ) return NULL;
    if ( *pszArgCurr == NULL ) return NULL;
    if ( (*argIsName) ( *pszArgCurr, &szValu )) return NULL;

    // Unbound value found - advance over it.
    // Also, since an unbound value confuses the
    //   position, shut down positional argument support
    pszArgCurr++;
    cArgSeqn = mArgSeqn;

    // done
    return szValu;
    }
