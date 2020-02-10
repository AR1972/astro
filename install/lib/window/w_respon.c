/***************************************************************************/
/*																									*/
/*	W_RESPON.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Waits for a character to input by the user. The inputted character must */
/* match a character in the argument array or an error beep will be        */
/* sounded. If the first element of the argument array is 0 the function   */
/* will accept any keystroke as a valid character and return. Case of the  */
/* characters in not consider as everything is converted to uppercase.     */
/*                                                                         */
/*	int GetResponse( int ValidResponse[] )												*/
/*                                                                         */
/* ARGUMENTS:	ValidResponse 	- Array of valid keystrokes                  */
/* RETURNS: 	int			 	- User's validated input                     */
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include 	<stdlib.h>

#include		<alias.h>
#include		<window.h>
#include		<bios_io.h>

int	GetChar( void );

int GetResponse( int ValidResponse[] )
{
	register		i;
	register		Input;
	int			Tmp;

   do
   {
      Input = GetChar();
      Input &= 0xff;    							/* Strip high byte of int */
      if ( ValidResponse[0] != 0 )
      {
         Tmp = Input;      /* Need to do this because toupper() is a macro */
         Input = toupper( Tmp );
         for( i = 0; ValidResponse[i] != 0; i++ )
            if ( Input == ValidResponse[i] )
               break;
         if ( ValidResponse[i] == 0 )     /* Check for end of array */
           Input = 0; /* No match found */
      }
   }
   while ( Input == 0 );
   return( Input );
}
