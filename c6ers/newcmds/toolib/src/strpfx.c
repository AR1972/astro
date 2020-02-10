#include <stdlib.h>
#include <string.h>

#include "strpfx.h"

// Determine if a string is a prefix
// - case sensitive varient

char*
strprefix (
    char* szTest,
    char* szFull )
{
    int cch = strlen ( szTest );

    // if no match, return NULL
    if ( strncmp ( szTest, szFull, cch ))
	return NULL;

    // success.  return where prefix failed
    return szFull + cch;
    }
