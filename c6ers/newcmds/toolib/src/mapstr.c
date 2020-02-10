// mapstr.c -- match abbreviated strings from list
//

#include <stdlib.h>

#include "mapstr.h"

// Find the "best" argument mapping
MapItemPtr
mapAbbrStr (
    MapAbbr szArg,
    MapItemPtr pTable,
    MapTest pTest
    )
{
    MapItemPtr pFirst = NULL;	// first partial match
    MapItemPtr pAmbig = NULL;	// ambigous partial match

    // search list for exact and partial matches
    while ( pTable->szLabel ) {
	char *pchPos = (*pTest) ( szArg, pTable->szLabel );

	// example quality of match
	if ( pchPos != NULL ) {

	    // exact matches cause an immediate return
	    if ( *pchPos == '\0' )
		return pTable;

	    // first partial match is remembered
	    if ( pFirst == NULL )
		pFirst = pTable;

	    // the last ambigous partial match is remembered
	    // can't reject immediately: exact match may be pending
	    else if ( pFirst->value != pTable->value )
		pAmbig = pTable;
	    }

	// advance for next interation
	pTable++;
	}

    // No exact match.	If no matches, table is the
    // "unknown" argument value.  If two or more partial
    // matches, ambig is set and no well defined match.
    if ( pFirst == NULL ) return pTable;
    if ( pAmbig == NULL ) return pFirst;
    return NULL;
    }
