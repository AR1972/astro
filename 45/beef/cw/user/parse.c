/*
	COW : Character Oriented Windows

	parse.c : Parsing functions for SDM

??????? rewrite so non-recursive ????????????
*/

#define COW
#include <cow.h>

#include <usdm.h>
#include <udialog.h>

#include "sdm.h"

/* forward */
STATIC int	WParseSz(char *, WORD *);
STATIC int	CchIntToPpch(int, char **);
STATIC VOID	SzFromInt(char *, int);



STATIC BOOL
WParseSz(sz, pichErr)
/*
  -- parse string to integer
  -- return wError if invalid
  -- return wNinch if empty
*/
REGISTER char *sz;
WORD *pichErr;		/* where to put index to error if bad */
	{
	int val = 0;
	char ch;
	int fNeg;
	char *szStart = sz;

	/* skip leading spaces */
	while (*sz == ' ')
		sz++;

	/* if null string, return special ninch value */
	if (*sz == '\0')
		return wNinch;

	/* check for negative number */
	if (fNeg = (*sz == '-'))
		sz++;

	/* parse number up to space or end of string */
	while ((ch = *sz) != '\0' && ch != ' ')
		{
		/* if character not a digit or value too large then error */
		if (ch < '0' || ch > '9' || val >= 3277 ||
		     (val >= 3276 && ch > '5'))
			{
			*pichErr = sz - szStart;
			return wError;
			}

		/* accumulate result */
		val = val * 10 + (ch - '0');
		sz++;
		}

	/* skip trailing spaces */
	while (*sz == ' ')
		sz++;

	*pichErr = sz - szStart;	/* set in case number == wError */
	/* if not at end of string, error */
	if (*sz != '\0')
		return wError;

	/* account for sign */
	return (fNeg ? -val : val);
	}



STATIC int
CchIntToPpch(w, ppch)
int w;
char **ppch;
	{
	int cch = 0;

	if (w < 0)
		{
		*(*ppch)++ = '-';
		w = -w;
		cch++;
		}

	if (w >= 10)
		{
		cch += CchIntToPpch(w / 10, ppch);
		w %= 10;
		}

	*(*ppch)++ = (char) ('0' + w);
	return(cch + 1);
	}



STATIC VOID	
SzFromInt(sz, w)
char *sz;
int w;
	{
	char *pch;
	int cch;

	AssertSz(w != wError, "SzFromInt: wError");
	/* if value is ninch, return null string */
	if (w == wNinch)
		cch = 0;
	else
		{
		pch = &sz[0];
		cch = CchIntToPpch(w, &pch);
		}
	sz[cch] = 0;
	}



PUBLIC WORD FARPUBLIC
ParseInt(tmm, sz, hObj, tmc, wParam, bArg)
/*
  -- simple int parse/format
*/
WORD tmm;
char *sz;
HANDLE hObj;
TMC tmc;
WORD wParam;		/* ignored */
WORD bArg;
	{
	StartPublic();
	int * pw = (int *) PvParseArg(hObj, bArg);
	WORD ichErr;

	Unreferenced(wParam);

	switch(tmm)
		{
	default:
		Assert(FALSE);
		break;

	case tmmParse:
		if ((*pw = WParseSz(sz, &ichErr)) != wError)
			{
			ReturnPublic(TRUE, WORD);
			}
		else
			{
			SetTmcSel(tmc, ichErr, 0x7fff);
			Alert("Parse Error");
			ReturnPublic(FALSE, WORD);
			}
		/*break*/

	case tmmFormat:
		SzFromInt(sz, *pw);
		ReturnPublic(0, WORD);
		/*break*/

	case tmmCwVal:
		ReturnPublic(sizeof(int)/sizeof(WORD), WORD);
		/*break*/
		}
	}



#ifdef LATER	/* maybe */

PUBLIC unsigned FARPUBLIC
ParseUns(tmm, sz, ppval, tmc)
int tmm;
char *sz;
int **ppval;
TMC tmc;
/*
  -- unsigned int parse/format function
*/
	{
	Alert("ParseUns() not yet implemented");
	return(FALSE);
	}



PUBLIC long FARPUBLIC
ParseFloat(tmm, sz, ppval, tmc)
/*
  -- float parse/format function
*/
int tmm;
char *sz;
int **ppval;
TMC tmc;
	{
	Alert("ParseFloat() not yet implemented");
	return(FALSE);
	}

#endif /*LATER*/
