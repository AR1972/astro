/********************************* INDEX.C *********************************/
/* Copyright (c) 1989 - Microsoft Corp.												*/
/* All rights reserved																		*/
/*																									*/
/* Program to read a message file and create a header file with the offsets*/
/* to each entry in the file.																*/
/*																									*/
/* EXAMPLE:																						*/
/* INDEX  infile_name.ext  outfile_name.ext											*/
/***************************************************************************/

#include    <stdio.h>
#include    <string.h>
#include    <malloc.h>

#define		EOL		0

/***************************************************************************/
int main( int argc, char *argv[] );
int ProcessMessage( char *Buffer );
/***************************************************************************/


/***************************************************************************/
/* Reads a help file and creates a header file with the offsets to each		*/
/* entry in the file as a #define value. The help file must have the			*/
/* following layout.																			*/
/*																									*/
/*   .LABEL_TEXT                                                           */
/*   Body of the text up to 25 lines                                       */
/*   .LABEL2_TEXT                                                          */
/*   Body of the text up to 25 lines                                       */
/*   ......                                                                */
/*   .END                                                                  */
/*																									*/
/* The resulting file which is created has this format, where xxxx is a    */
/* long numeric value.                                                     */
/*                                                                         */
/*	#define	LABEL_TEXT		xxxx															*/
/*	#define	LABEL_LINES		xxxx															*/
/*	#define	LABEL2_TEXT		xxxx															*/
/*	#define	LABEL2_LINES	xxxx															*/
/*	#define  ......_TEXT		xxxx															*/
/* #define	......_LINES	xxxx															*/
/*																									*/
/* A second file is created which contains the text from the message file  */
/* in a form which is assemble-able with the Microsoft MASM 5.1.				*/
/*																									*/
/* Blank lines and line which start with a simicolon are ignored and only	*/
/* the text within quote is used. Quotes are not allowed within the text	*/
/* at this time.																				*/
/*																									*/
/*																									*/
/* EXAMPLE TEXT FILE:																		*/
/*																									*/
/*																									*/
/* ; =============================================================			*/
/*	; =============================================================			*/
/*	; All of these lines are the same length to give a better					*/
/*	; appearance on the screen																*/
/*	;																								*/
/*																									*/
/*	.REMINDER_TEXT																				*/
/*																									*/
/*	"<ENTER>    = Continue the installation"											*/
/*	"<ENTER>    = Accept entry and continue"											*/
/*	"<ESC>      = Return to previous screen"											*/
/*	"<ESC>      = Exit installation program"											*/
/*	"<CTRL>+<C> = Exit installation program"											*/
/*																									*/
/*	.END_TEXT																					*/
/*																									*/
/***************************************************************************/

int main( int argc, char *argv[] )
{
   FILE		*sFile, *hFile, *aFile;		/* Source and header files */
	char		*szBuffer, *szPtr;				/* Work szBuffer and pointer */
	char		*szLastDef;						/* The last define string */
	unsigned Offset = 0;	/* Current offset */
	int		iNumBytes, iNumLines;
	int		Status = 0;


   if ( argc < 3 )
      Status = 8;
   else if ( (sFile = fopen( argv[1], "rb" )) == NULL )
      Status = 1;
   else if ( (hFile = fopen( argv[2], "wb" )) == NULL )
      Status = 2;
	else if ( (aFile = fopen( "MESSAGE.ASM", "wb" )) == NULL )
		Status = 3;
   else if ( (szBuffer = malloc( 200 )) == NULL )
      Status = 4;
	else if ( (szLastDef = malloc( 200 )) == NULL )
		Status = 4;
   else
   {
		fprintf( aFile, "DOSSEG\r\n"  );
		fprintf( aFile, ".MODEL\tSMALL,C\r\n"  );
		fprintf( aFile, "\r\n\r\n.DATA\r\n\r\n" );
		fprintf( aFile, "\tPUBLIC chMessage\r\n" );
		fprintf( aFile, "chMessage\tLABEL BYTE\r\n\r\n" );

		fprintf( hFile, "void\tGetMessage( char **Ptr, unsigned Offset_u );\r\n" );
		fprintf( hFile, "void\tGetMessStr( char *szBuf, unsigned Offset_u, int StrNum );\r\n\r\n" );

		while( !feof( sFile) && Status == 0 )
		{
			while( !feof( sFile) && Status == 0 )
   	   {
				if( *szBuffer == '.' )
				{
											/* Convert newline to EOL */
					*(strchr( szBuffer, '\r')) = '\0';
					strcpy( szLastDef, szBuffer + 1 );	/* Save definition */
					if (fprintf( hFile, "#define\t\t%-25s %#4.4x\r\n",
									 szLastDef, Offset ) == 0 )
						Status = 4;
					break;
				}
				else if ( fgets( szBuffer, 132, sFile ) != szBuffer )
					Status = 4;
			}
			if ( Status == 0 )
			{
				fgets( szBuffer, 132, sFile );
				iNumLines = 0;
			}
			while ( !feof( sFile) && *szBuffer != '.' && Status == 0 )
			{
				if ( (iNumBytes = ProcessMessage( szBuffer )) >= 0 )
				{
					iNumBytes++;
					iNumLines++;
					if ( iNumBytes > 1 )
						szPtr = "db\t\"%s\",0\r\n";
					else
						szPtr = "db\t0\r\n";

					if ( fprintf( aFile, szPtr, szBuffer ) == 0 )
						Status = 4;
					else
						Offset += iNumBytes;
				}
				if ( iNumBytes == -2 )
					Status = -2;
				else
					fgets( szBuffer, 132, sFile ) != szBuffer;
			}
			if ( Status == 0 )
			{ 
				Offset++;
				fprintf( aFile, "db\t-1\r\n" );
				strcpy( strchr( szLastDef, EOL ) - 4, "LINES" );
				fprintf( hFile, "#define\t\t%-25s %#4.4x\r\n", szLastDef,
							++iNumLines );		/* Add 1 line for NULL */
			}
		}
	}
	if ( Status == 0 )
		fprintf( aFile, "\r\nEND\r\n" );

	fcloseall();
	if ( Status != 0 )
		printf( "\nError status = %d\n", Status );
	return( Status );
}


int ProcessMessage( char *szBuffer )
{
	int	iCount;
	char	*pszPtr;

	pszPtr = szBuffer;
	while ( *pszPtr != '\"' && *pszPtr != EOL && *pszPtr != ';' )
		pszPtr++;
	
	if ( *pszPtr == '\"' )
	{
		pszPtr++;
		for( iCount = 0;
			  *pszPtr != '\"';
			  iCount++, szBuffer++, pszPtr++ )
		{
			*szBuffer = *pszPtr;
			if ( *pszPtr == EOL )
			{
				iCount = -2 ;		/* Missing end quote */
				break;
			}
		}
		*szBuffer = EOL;
	}
	else
		iCount = -1;				/* Ingore this line */

	return( iCount );
}
