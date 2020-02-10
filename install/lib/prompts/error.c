/***************************************************************************/
/*																									*/
/*	ERROR.C																						*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Non-fatal error function. An array of pointers to a string defining the */
/* error and a pointer to a string suggesting a possible correction. Will  */
/* display an error message in a window on the screen and wait for a any   */
/* key to be pressed. A buffer to store the orginal contents under the     */
/* error window must be allocated at the start of the program since there  */
/* is no guarentee that there will be sufficient memory available when the */
/* the error function is called.                                           */
/*                                                                         */
/*	void	Error( char **ErrorString )													*/
/*																									*/
/* ARGUMENTS:	ErrorString 	- 		pointer to string identifing the error.*/
/* 				CorrectionString - pointer to string detailing the error    */
/* RETURNS: 	void                                                        */
/*																									*/
/*	EXTERNS:		ErrorMessageText	- Declared in EXTERN.C							*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include 	<alias.h>
#include		<window.h>
#include		<bios_io.h>

void	Error( char **ErrorString )
{
	char					*apszText[ MAX_STRINGS ];
	int					iHelpFlags;
	static int			Response[] = {CR, 0};

	extern unsigned	ErrorMessageText;

	GetMessage( apszText, ErrorMessageText );
   apszText[2] = ErrorString[0];
   apszText[3] = ErrorString[1];

	PushHelp( 0xffff );							/* No help is set yet				*/
	iHelpFlags = GetHelpFlags();				/* Save current help line			*/
	HelpLine( EXIT_HLP | CONT_HLP );			/* Set new help line					*/
	
	PromptWindow( apszText, Response, GetErrorColor(), ErrorBuffer );

	HelpLine( iHelpFlags );						/* Restore original help line		*/
	PopHelp();
}
