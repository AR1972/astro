/*
	COW : Character Oriented Windows

	strings.c : common string pool (multiused strings)

	(compiles with minimal headers)
	NOTE : the types of data in this file must track strings.h
*/

#ifndef COW
/* International build */
/* define language in compile flags
#else	/* normal build */
#include <version.h>
#endif

#include <itl.h>

char	szEmptyString[]		=	"";
char	szOkString[]		=	szOk;
char	szCancelString[]	=	szCancel;
char 	szYesString[]		=	szYes;
char	szNoString[]		=	szNo;
char	szRetryString[]		=	szRetry;
char	szAbortString[]		=	szAbort;
#ifdef HELP_BUTTON
char	szHelpString[]		=	szHelp;
#endif

/* Array of bytes containing info (see MBB structure for details) */
/* 3 bytes per option */
char pascal rgmbb[] =
	{
	0,				0,		0,
	sizeof(szOkString)-1,		cchOk+2,	'\0',
	sizeof(szCancelString)-1,	cchCancel+2,	'\0',
	sizeof(szNoString)-1,		cchNo+2,	chAccelNo,
	sizeof(szYesString)-1,		cchYes+2,	chAccelYes,
	sizeof(szRetryString)-1,	cchRetry+2,	chAccelRetry,
	sizeof(szAbortString)-1,	cchAbort+2,	chAccelAbort
#ifdef HELP_BUTTON
	, sizeof(szHelpString)-1,	  cchHelp+2,	  chAccelHelp
#endif
	};

char pascal mpmbcchButton[] =
	{
#ifdef HELP_BUTTON
#ifdef BUTTON_CENTER
	(cchOk+2) + 3 + (cchHelp+2),
	(cchYes+2) + 3 + (cchNo+2) + 3 + (cchCancel+2) + 3 + (cchHelp+2),
	(cchRetry+2) + 3 + (cchCancel+2) + 3 + (cchHelp+2),
	(cchOk+2) + 3 + (cchCancel+2) + 3 + (cchHelp+2),
	(cchAbort+2) + 3 + (cchHelp+2),
	(cchYes+2) + 3 + (cchNo+2) + 3 + (cchHelp+2),
	(cchRetry+2) + 3 + (cchHelp+2)
#else
	(cchOk+2) + 2 + (cchHelp+2) + 2,
	(cchYes+2) + 2 + (cchNo+2) + 2 + (cchCancel+2) + 2 + (cchHelp+2) + 2,
	(cchRetry+2) + 2 + (cchCancel+2) + 2 + (cchHelp+2) + 2,
	(cchOk+2) + 2 + (cchCancel+2) + 2 + (cchHelp+2) + 2,
	(cchAbort+2) + 2 + (cchHelp+2) + 2,
	(cchYes+2) + 2 + (cchNo+2) + 2 + (cchHelp+2) + 2,
	(cchRetry+2) + 2 + (cchHelp+2) + 2
#endif /*BUTTON_CENTER*/
#else /*HELP_BUTTON*/
#ifdef BUTTON_CENTER
	(cchOk+2),
	(cchYes+2) + 3 + (cchNo+2) + 3 + (cchCancel+2),
	(cchRetry+2) + 3 + (cchCancel+2),
	(cchOk+2) + 3 + (cchCancel+2),
	(cchAbort+2),
	(cchYes+2) + 3 + (cchNo+2),
	(cchRetry+2)
#else
	(cchOk+2) + 2,
	(cchYes+2) + 2 + (cchNo+2) + 2 + (cchCancel+2) + 2,
	(cchRetry+2) + 2 + (cchCancel+2) + 2,
	(cchOk+2) + 2 + (cchCancel+2) + 2,
	(cchAbort+2) + 2,
	(cchYes+2) + 2 + (cchNo+2) + 2,
	(cchRetry+2) + 2
#endif /*BUTTON_CENTER*/
#endif /*HELP_BUTTON*/
	};
