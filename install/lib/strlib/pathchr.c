/***************************************************************************/
/*																									*/
/*	PATHCHR.C																					*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Validates a character as a valid path and file name character. 			*/
/* 																								*/
/* IsValidPathChar( char Char )															*/
/* 																								*/
/* ARGUMENTS:	Char	- Character to be tested										*/
/* RETURNS: 	int	- TRUE if a valid character else FALSE 					*/
/*                                                                         */
/* johnhe - 03/23/89																			*/
/***************************************************************************/

#define		FALSE 	0
#define		TRUE		1

int IsValidPathChar( char Char )
{
	int	IsOk;

	switch( Char )
	{
		case	' '  :
		case	'\t' :
		case	0x0d :
		case	'/'  :
		case	':'  :
		case	';'  :
		case	'='  :
		case	'<'  :
		case	'>'  :
		case	'|'  :
			IsOk = FALSE;
			break;
		default	  :
			IsOk = TRUE;
			break;
	}
	return( IsOk );
}
