#include <stdlib.h>
#include <string.h>

#include "strpfx.h"

// Determine if a string is a prefix
// - case insensitive variant

char*
striprefix (
    char* szTest,
    char* szFull )
{
    int cch = strlen ( szTest );

    // if no match, return NULL
    if ( strnicmp ( szTest, szFull, cch ))
	return NULL;

    // success.  return where prefix failed
    return szFull + cch;
    }
