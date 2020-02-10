/* forsemi.c - walk a semicolon separated sstring */

#include "..\h\tools.h"
#include <stdarg.h>

flagType forsemi (register char *p,
		  flagType (*proc)(char *, va_list),
		  ...)
{
    char	*p1, c;
    flagType	f;
    va_list	ap;


    do {
	p1 = strbscan (p, ";");
	c = *p1;
	*p1 = 0;
	va_start(ap, proc);
	f = (*proc)(p, ap);
	va_end(ap);
	p = p1;
	*p++ = c;
	if (f)
	    return TRUE;
    } while (c);
    return FALSE;
}
