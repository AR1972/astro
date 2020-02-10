/*
	COW : Character Oriented Windows

	assert.c : AssertPrintf() procedures
*/

#define COW
#include <cow.h>

#include <kmem.h>
#include <kinput.h>
#include <uwindow.h>
#include <umenu.h>			/* Menu structure */
#include <uscreen.h>
#include <cwdebug.h>			/* debugging support */

#ifdef DEBUG	/* entire file for debug only */

extern WORD PASCAL psDebug;	/* 0 if no debugger */

VOID FAR PASCAL	CwOutSz(CHAR *);

/* forward*/
VOID FAR cdecl	AssertPrintf(char *, ...);
static char *	PchConvertHexW(char *, int);
static char *	PchConvertHexL(char *, long);
static char *	PchConvertDecW(char *, int, int);
static void cdecl	sprintf(char *, char *, ...);


VOID FAR cdecl
AssertPrintf(szFmt, ...)
/*
  -- printf to debugging console or main console
*/
char *szFmt;
	{
	int cch = 0;
	char rgch[256];
	char *szT = rgch;

	long *pl = (long *) (&szFmt + 1);

	sprintf(rgch, szFmt, pl[0], pl[1], pl[2], pl[3], pl[4]);

	while (*szT++)
		cch++;

	if (psDebug)
		PrDebugRgch((LPSTR) rgch, cch);
	else
		{
		CwOutSz(rgch);
		}
	}


static char rgchHex[] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'A', 'B', 'C', 'D', 'E', 'F' };


static char *
PchConvertHexW(pch, w)
/*
  -- fill character array with hex word
*/
char *pch;
int w;
	{
	*pch++ = rgchHex[(w >> 12)&15];
	*pch++ = rgchHex[(w >> 8)&15];
	*pch++ = rgchHex[(w >> 4)&15];
	*pch++ = rgchHex[w&15];
	return (pch);
	}

static char *
PchConvertHexL(pch, l)
/*
  -- fill character array with hex long
*/
char *pch;
long l;
	{
	pch = PchConvertHexW(pch, HIWORD(l));
	return PchConvertHexW(pch, LOWORD(l));
	}


static char *
PchConvertDecW(pch, w, width)
/*
  -- fill character array with decimal word
*/
char *pch;
int w;
int width;
	{
	int wDecMask = 10000;
	BOOL fUse = FALSE;
	int width2;

	if (w < 0)
		{
		*pch++ = '-';
		w = -w;
		}

	for (wDecMask = 10000, width2 = 5; wDecMask != 0; wDecMask /= 10, width2--)
		{
		char ch = '0';

		while (w >= wDecMask)
			{
			ch++;
			w -= wDecMask;
			}

		if (fUse || ch != '0' || wDecMask == 1)
			{
			*pch++ = ch;
			fUse = TRUE;
			}
		else if (width >= width2)
		        *pch++ = ' ';
	        }

	return (pch);
	}


#ifdef FOR_QC
VOID FAR cdecl
dfprintf(fh, szFmt, ...)
/*
  -- fprintf under debugging to file or debugging console
*/
int  fh;
char *szFmt;
	{
	int cch = 0;
	char rgch[256];
	char *szT = rgch;

	long *pl = (long *) (&szFmt + 1);

	sprintf(rgch, szFmt, pl[0], pl[1], pl[2], pl[3], pl[4]);

	while (*szT++)
		cch++;

        if (fh == -1)
	    PrDebugRgch((LPSTR) rgch, cch);
	else
	    DBWrite(fh, rgch, cch);
	}
#endif


VOID FAR cdecl
dprintf(szFmt, ...)
/*
  -- printf to debugging console
*/
char *szFmt;
	{
	int cch = 0;
	char rgch[256];
	char *szT = rgch;

	long *pl = (long *) (&szFmt + 1);

	sprintf(rgch, szFmt, pl[0], pl[1], pl[2], pl[3], pl[4]);

	while (*szT++)
		cch++;

	PrDebugRgch((LPSTR) rgch, cch);
	}


static void cdecl
sprintf(buf, sz, ...)
/* returns pointer to 0 at end of output string */
char *buf, *sz;
	{
	char ch;
	char *szT;
	WORD *parg;
	WORD width;

	parg = (WORD *) (&sz + 1);

	while (*sz != '\0')
		{
		if ((ch = *sz++) == '%')
			{
			width = 0;
			if (*sz >= '0' && *sz < '9')
			  width = *sz++ - '0';
			switch (ch = *sz++)
				{
			case '%':
				*buf++ = '%';
				break;
			case 'd':
				buf = PchConvertDecW(buf, *parg++, width);
				break;
			case 'x':
				buf = PchConvertHexW(buf, *parg++);
				break;
			case 'l':
				switch (*sz++)
					{
				case 'x':
					buf = PchConvertHexL(buf, *(long *)parg);
					parg += sizeof(long) / sizeof(WORD);
					break;
				default:
					*buf++ = 'l';
					sz--;
					}
				break;
			case 's':
				szT = (char *) *parg++;
				while (*szT)
					*buf++ = *szT++;
				break;
			case 'c':
				*buf++ = (char) *parg++;
				break;
			default:
				*buf++ = ch;
				break;
				}
			}
		else if (ch == '\n')
			{
			/* newline => CR/LF */
			*buf++ = '\r';
			*buf++ = '\n';
			}
		else
			{
			*buf++ = ch;
			}
		}
	*buf = '\0';
	}

#endif /* DEBUG (entire file) */

