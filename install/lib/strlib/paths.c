/***************************************************************************/
/*																									*/
/*	PATHS.C																						*/
/*																									*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Gets the PATH variable from the enviroment and then copies it to a		*/
/* specified buffer and seperates the string into individual paths and		*/
/* fills in an array of pointers to the beginning of each string.				*/
/*																									*/
/*	void GetPathStrings( char **apszPaths, char *chBuffer, int BufSize )		*/
/*																									*/
/*	ARGUMENTS:	apszPaths	- Array of pointers to be filled in					*/
/*					chBuffer		- Buffer to copy the string into						*/
/*					BufSize		- Size of passed buffer in bytes						*/
/*	RETURNS:		void																			*/
/*																									*/
/* johnhe - 01/13/89																			*/
/***************************************************************************/

#include		<stdio.h>
#include		<stdlib.h>
#include 	<string.h>

#include 	<strlib.h>

void GetPathStrings( char **apszPaths, char *chBuffer, int BufSize )
{
	register		i;						/* Index for array apszPaths		*/
	char			*szEnvironment;	/* Pointer to eviro PATH string	*/
	char			*pchEnd;				/* Pointer to end of PATH string	*/

										/* Make sure there is a path enviro variable	*/
	if ( (szEnvironment = getenv( "PATH" )) != NULL )
	{
															/* Copy string to work buffer	*/
		i = strlen( szEnvironment );
		++i;

		if ( i < BufSize )
			BufSize = i;

 		strncpy( chBuffer, szEnvironment, BufSize - 1 );
		*(chBuffer + BufSize - 1 ) = EOL;

		RemoveSpaces( chBuffer );					/* Clean up the string			*/
		pchEnd = strchr( chBuffer, EOL ); 		/* Find end of string			*/
		ReplaceChar( chBuffer, ';', EOL );		/* Convert to individ string	*/

		for ( i = 0; chBuffer < pchEnd; i++ )
		{
			apszPaths[i] = chBuffer;				/* Save pointer to this path	*/
			chBuffer = strchr( chBuffer, EOL ) + 1; /* Find end of this path	*/
		}
	}

	apszPaths[i] = NULL;								/* Mark end of array				*/
}
