#include "..\h\tools.h"
#include <ctype.h>

flagType fMatch (pat, text)
char *pat, *text;
{
    switch (*pat) {
    case '\0':
        return (flagType) (*text == '\0');
    case '?':
	return (flagType) (*text != '\0' && fMatch (pat + 1, text + 1));
    case '*':
        do {
	    if (fMatch (pat + 1, text))
                return (flagType) TRUE;
        } while (*text++);
        return FALSE;
    default:
	return (flagType) (toupper (*text) == toupper (*pat) && 
			 	fMatch (pat + 1, text + 1));
        }
}
