/*
	CW : Character Oriented Windows

	ukanji.h : KANJI (DBCS) specific definitions
*/

/***BEGIN_PUBLIC***/

BOOL		FARPUBLIC FIsDbcsChar(ACHAR);		/* OPTIONAL */
WORD		FARPUBLIC CchLenDbcs(char *);		/* OPTIONAL */
char *		FARPUBLIC PchNextDbcs(char *);		/* OPTIONAL */
char *		FARPUBLIC PchPrevDbcs(char *, char *);	/* OPTIONAL */

/***END_PUBLIC***/
