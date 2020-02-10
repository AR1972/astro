// string.c - string functions carried over from Lattice C
//
// Modifications
//	28-Dec-90   leeca   Converted from .ASM for memory model independence
//

#include <string.h>

char *strbscan (
    char *str,
    char *set
    )
{
    return str + strcspn ( str, set );
    }

char *strbskip (
    char *str,
    char *set
    )
{
    return str + strspn ( str, set );
    }
