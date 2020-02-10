/***************************************************************************/
/*                                                                         */
/*	GETCHAR.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Misc. input functions																	*/
/*                                                                         */
/* johnhe - 12/01/89																			*/
/***************************************************************************/

#include    <stdio.h>
#include    <stdlib.h>

#include		<alias.h>
#include		<bios_io.h>
#include		<window.h>

/***************************************************************************
 * Globals
 ***************************************************************************/

int					AbortEnabled = TRUE;

/***************************************************************************/
/* Functions for getting input from the keyboard.                          */
/***************************************************************************/

static int			Extension;			/* Scan code for last char from GetChar*/
static int			UnGottenChar;		/* Place to stuff an UnGet() character */
static int			IsWaiting;			/* TRUE if UnGottenChar is valid 		*/

extern void			Help( void );
extern void			ProgramAbort( void );

/***************************************************************************/
/* The argument will be returned by the next call to GetChar(). Similar to */
/* stuffing a character into the keyboard buffer. The function does not 	*/
/* que the ungotten characters so this character is lost if another call	*/
/* to the function is done before a call to GetChar().							*/
/* 																								*/
/* void UnGetChar( int Char ) 															*/
/* 																								*/
/* ARGUMENTS:	Char	- Character to return on next call to GetChar() 		*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void UnGetChar( int Char )
{
	UnGottenChar = Char;
   IsWaiting = TRUE;
}

/***************************************************************************/
/* Returns the scan code for the last character returned by GetChar()      */
/* 																								*/
/* int GetCharExtension( void )															*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	int	- Scan code for last charcter returned by GetChar()	*/
/* 																								*/
/***************************************************************************/

int GetCharExtension( void )
{
	return( (int)Extension );
}

/***************************************************************************/
/* Flushes the keyboard buffer.															*/
/* 																								*/
/* void KbdFlush( void )																	*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	void																			*/
/* 																								*/
/***************************************************************************/

void KbdFlush( void )
{
	while( KbdIsWaiting() )
		KbdGetKey();
}

/***************************************************************************/
/* Waits for a character to be entered at the keyboard. Returns the ascii  */
/* value of the character entered and saves the scan code for the next     */
/* call GetCharExtension(). If a character has been returned with          */
/* UnGetChar() it will be the one returned. Also does a check for F3			*/
/* and if abort is allowed will offer the option to the user. Always 		*/
/* flushes the keyboard buffer before getting a character.						*/
/* 																								*/
/* int GetChar( void )																		*/
/* 																								*/
/* ARGUMENTS:	NONE																			*/
/* RETURNS: 	int	- Low byte of word returned by int 16h function 0.		*/
/* 																								*/
/***************************************************************************/

int GetChar( void )
{
															/*lint -e727						*/
	int				Char;								/* Char from the keyboard		*/
	static int		IsF3;								/* Prevents recursive F3		*/
	static int		IsHelp;

   if ( IsWaiting == TRUE )
	{														/* See if a char was ungotten */
		Char = UnGottenChar;
      IsWaiting = FALSE;
   }
   else
   {
#ifndef DBCS
		KbdFlush();										/* Flush the keyboard buffer	*/
#endif
		do
		{
			Char = KbdGetKey();

			if ( Char == (F1 << 8) )
			{
				IsHelp = TRUE;
				Help();
				IsHelp = FALSE;
			}
#ifndef	HD_BACKUP
			else if ( Char == (F5 << 8) && !IsHelp && !IsF3 )
			{
				if ( GetBackGroundColor() != 0x07 )
				{
					SetDefaultColors( 0 );
					StripScreenColor();
				}
			}
#endif

			if ( Char == (F3 << 8) )
			{
				if ( !AbortEnabled )
					break;

				/* 1st F3 displays confirmation prompt; 2nd F3 aborts. */
				if ( IsF3 == FALSE )
				{
					IsF3 = TRUE;
					AllowAbort();
					IsF3 = FALSE;
				}
				else
					ProgramAbort();
			}	
		}
		while( Char == (F1 << 8) || Char == (F3 << 8) );

		Extension = (int)((unsigned)(Char) >> 8);
	
   }
	return( (Char & 0xff) );
															/*lint +e727 */
}

