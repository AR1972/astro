/***************************************************************************/
/*																									*/
/*	W_PUTWIN.C																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Displays an un-titled window in the screen as defined in the window     */
/* definition structure.                                                   */
/*                                                                         */
/*	void PutWindow ( struct  WindowStruct *Win )										*/
/*                                                                         */
/* ARGUMENTS:	Win	- Ptr to initialized window definition structure.     */
/*	RETURNS:		void																			*/
/*																									*/
/* johnhe - 03/15/89																			*/
/***************************************************************************/

#include		<alias.h>
#include		<window.h>
#include		<bios_io.h>

#define		SHADOW	0x08

#ifndef DBCS			/* if Not DBCS ### */

void PutWindow ( struct  WindowStruct *Win )
{
   int         DupFactor, VertDupFactor, Row, Col, i;
   enum        {tlc, trc, blc, brc, hor, ver };

   static char Border[2][6] = { { 'É', '»', 'È', '¼', 'Í', 'º' },
                                { 'Ú', '¿', 'À', 'Ù', 'Ä', '³' } };

      /* Set width of box between the 2 corners */
   DupFactor = Win->Right - Win->Left - 1;
   VertDupFactor = Win->Bottom - Win->Top - 1;

      /* Draw top of box */

   VideoPutCharAttr( Win->Top, Win->Left, Border[Win->Type][tlc],
                     Win->BorderColor );
   VideoDupCharAttr( Win->Top, Win->Left + 1, Border[Win->Type][hor],
                     Win->BorderColor, DupFactor );
   VideoPutCharAttr( Win->Top, Win->Right, Border[Win->Type][trc],
                     Win->BorderColor );

      /* Draw sides of box */
   Col = Win->Left;
   Row = Win->Top + 1;
   for ( i = 0; i < 2; i++ )
   {

      VideoVertDupCharAttr( Row, Col, Border[Win->Type][ver],
                         Win->BorderColor, VertDupFactor );
      Col = Win->Right;

   }
      /* Draw bottom of box */

   VideoPutCharAttr( Win->Bottom, Win->Left, Border[Win->Type][blc],
                     Win->BorderColor );
   VideoDupCharAttr( Win->Bottom, Win->Left + 1 , Border[Win->Type][hor],
                     Win->BorderColor, DupFactor );
   VideoPutCharAttr( Win->Bottom, Win->Right, Border[Win->Type][brc],
                     Win->BorderColor );

   // Check interior size of box.
   if (Win->Top + 1 <= Win->Bottom - 1 && Win->Left + 1 <= Win->Right - 1)
      /* Clear the inside of the box */

      VideoScrollDn(Win->Top + 1, Win->Left + 1, Win->Bottom - 1,
         Win->Right - 1, 0, Win->Color);

		/* Add the show if Shadow flag is set */
	if ( Win->IsShadow && VideoIsColor() )
	{
		VideoDupAttr( Win->Bottom+1, Win->Left+2, SHADOW, DupFactor+2 );
		VideoVertDupAttr( Win->Top+1, Win->Right+1, SHADOW, VertDupFactor+1 );
		VideoVertDupAttr( Win->Top+1, Win->Right+2, SHADOW, VertDupFactor+1 );
	}
}

#else			/* ### if DBCS ### */

void PutWindow ( struct  WindowStruct *Win )
{
   int         DupFactor, VertDupFactor, Row, Col, i;
   enum        {tlc, trc, blc, brc, hor, ver };

#ifdef JAPAN	/* if KEISEN */
   static int Border[2][6] = { { K_THICK_TOP_LEFT, K_THICK_TOP_RIGHT, K_THICK_BOTOM_LEFT, K_THICK_BOTOM_RIGHT, K_THICK_HORIZ_LINE, K_THICK_VERT_LINE },
                                { K_THIN_TOP_LEFT, K_THIN_TOP_RIGHT, K_THIN_BOTOM_LEFT, K_THIN_BOTOM_RIGHT, K_THIN_HORIZ_LINE, K_THIN_VERT_LINE } };
#else
   static char Border[2][6] = { { 'É', '»', 'È', '¼', 'Í', 'º' },
                                { 'Ú', '¿', 'À', 'Ù', 'Ä', '³' } };
#endif

      /* Set width of box between the 2 corners */
   DupFactor = Win->Right - Win->Left - 1;
   VertDupFactor = Win->Bottom - Win->Top - 1;

/* #ifdef DBCS */
    for ( Row = Win->Top; Row <= Win->Bottom; Row++)
    {
	if (CheckLead(Row, Win->Right))
	{
	    VideoPutCharRowCol(Row, Win->Right + 1, SPC);
	    VideoPutCharRowCol(Row, Win->Right, SPC);
	}
    }
    for ( Row = Win->Top; Row <= Win->Bottom; Row++)
    {
	if (CheckTail(Row, Win->Left))
	{
	    VideoPutCharRowCol(Row, Win->Left, SPC);
	    VideoPutCharRowCol(Row, Win->Left - 1, SPC);
	}
    }
/* #endif */

      /* Draw top of box */

#ifdef JAPAN	/* if KEISEN */

    VideoScrollDn(Win->Top, Win->Left, Win->Bottom,
         Win->Right, 0, Win->Color);

   VideoPutDBCharAttr( Win->Top, Win->Left, Border[Win->Type][tlc],
                     Win->BorderColor );
   VideoDupDBCharAttr( Win->Top, Win->Left + 2, Border[Win->Type][hor],
                     Win->BorderColor, (DupFactor-2)/2 );
   VideoPutDBCharAttr( Win->Top, Win->Right - 1, Border[Win->Type][trc],
                     Win->BorderColor );

#else			/* if Not JAPAN */

   VideoPutCharAttr( Win->Top, Win->Left, Border[Win->Type][tlc],
                     Win->BorderColor );
   VideoDupCharAttr( Win->Top, Win->Left + 1, Border[Win->Type][hor],
                     Win->BorderColor, DupFactor );
   VideoPutCharAttr( Win->Top, Win->Right, Border[Win->Type][trc],
                     Win->BorderColor );

#endif

      /* Draw sides of box */
   Col = Win->Left;
   Row = Win->Top + 1;
   for ( i = 0; i < 2; i++ )
   {

#ifdef JAPAN	/* if KEISEN */

      VideoVertDupDBCharAttr( Row, Col, Border[Win->Type][ver],
                         Win->BorderColor, VertDupFactor );
      Col = Win->Right-1;

#else			/* if Not JAPAN */

      VideoVertDupCharAttr( Row, Col, Border[Win->Type][ver],
                         Win->BorderColor, VertDupFactor );
      Col = Win->Right;

#endif

   }
      /* Draw bottom of box */

#ifdef JAPAN	/* if KEISEN */

   VideoPutDBCharAttr( Win->Bottom, Win->Left, Border[Win->Type][blc],
                     Win->BorderColor );
   VideoDupDBCharAttr( Win->Bottom, Win->Left + 2 , Border[Win->Type][hor],
                     Win->BorderColor, (DupFactor-2)/2 );
   VideoPutDBCharAttr( Win->Bottom, Win->Right -1, Border[Win->Type][brc],
                     Win->BorderColor );

#else			/* if Not JAPAN */

   VideoPutCharAttr( Win->Bottom, Win->Left, Border[Win->Type][blc],
                     Win->BorderColor );
   VideoDupCharAttr( Win->Bottom, Win->Left + 1 , Border[Win->Type][hor],
                     Win->BorderColor, DupFactor );
   VideoPutCharAttr( Win->Bottom, Win->Right, Border[Win->Type][brc],
                     Win->BorderColor );

   // Check interior size of box.
   if (Win->Top + 1 <= Win->Bottom - 1 && Win->Left + 1 <= Win->Right - 1)
      /* Clear the inside of the box */

      VideoScrollDn(Win->Top + 1, Win->Left + 1, Win->Bottom - 1,
         Win->Right - 1, 0, Win->Color);

#endif

		/* Add the show if Shadow flag is set */
	if ( Win->IsShadow && VideoIsColor() )
	{
/* #ifdef DBCS */
	    for ( Row = Win->Top + 1; Row <= Win->Bottom + 1; Row++)
	    {
		if (CheckLead(Row, Win->Right+2))
		{
		    VideoPutCharRowCol(Row, Win->Right + 3, SPC);
		    VideoPutCharRowCol(Row, Win->Right + 2, SPC);
		}
	    }
/* #endif */
		VideoDupAttr( Win->Bottom+1, Win->Left+2, SHADOW, DupFactor+2 );
		VideoVertDupAttr( Win->Top+1, Win->Right+1, SHADOW, VertDupFactor+1 );
		VideoVertDupAttr( Win->Top+1, Win->Right+2, SHADOW, VertDupFactor+1 );
	}
}
#endif			/* ### end if DBCS ### */

