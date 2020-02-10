/*
	COW : Character Oriented Windows
	(COW USER WINDOW)

	box.c : Boxes
*/

#define COW
#include <cow.h>

#include <uwindow.h>
#include <uscreen.h>
#include <uisa.h>
#include "window.h"
#include "screen.h"
#include "util.h"

#include "shadow.h"

/*	* Installable Boxes (init in FInitScreen()) */
PUBLIC char chShadow;
PUBLIC WORD diShadow = diShadowInit;

PUBLIC BOX PASCAL boxSingle;
PUBLIC BOX PASCAL boxDouble;

#ifdef WINDOW_OVERLAP

PUBLIC AX daxShadow = daxShadowInit;
PUBLIC AY dayShadow = dayShadowInit;

PUBLIC BOX PASCAL boxActiveWindowOut;
PUBLIC BOX PASCAL boxInactiveWindowOut;
PUBLIC BOX PASCAL boxActiveWindowIn;
PUBLIC BOX PASCAL boxInactiveWindowIn;

PRIVATE PWND pwndClip;

#endif /*WINDOW_OVERLAP*/



VOID FARPRIVATE
DrawBoxArc(pbox, parc, di, fRight, fBottom, szTitle)
/*
  -- draw box at absolute coordinates
  -- fRight and fBottom if FALSE will simplify the draw for scroll bars
*/
REGISTER BOX * pbox;
REGISTER ARC * parc;
WORD	di;
BOOL	fRight;
BOOL	fBottom;
char *	szTitle;
	{
	AX axLeft, axRight;
	AY ayTop, ayBottom;

	BeginDraw();

	axRight = parc->axRight - daxBorder;
	ayBottom = parc->ayBottom - dayBorder;

#ifdef WINDOW_OVERLAP
	if (pwndClip == NULL)
		{
#endif /*WINDOW_OVERLAP*/

	/* Corners */

	/* top left */
	CharOutAbs(parc->axLeft, parc->ayTop, pbox->chTopLeftCorner, di);
	/* top right */
	CharOutAbs(axRight, parc->ayTop, pbox->chTopRightCorner, di);

	/* bottom left */
	CharOutAbs(parc->axLeft, ayBottom, pbox->chBottomLeftCorner, di);
	/* bottom right */
	CharOutAbs(axRight, ayBottom, pbox->chBottomRightCorner, di);

#ifdef WINDOW_OVERLAP
		}
	else
		{
		if (parc->ayTop <= pwndClip->arcClipping.ayBottom)
			{
			if (parc->axLeft <= pwndClip->arcClipping.axRight)
				CharOutAbs(parc->axLeft, parc->ayTop, 
						pbox->chTopLeftCorner, di);
			if (parc->axRight <= pwndClip->arcClipping.axRight)
				CharOutAbs(axRight, parc->ayTop,
						pbox->chTopRightCorner, di);
			}
		if (parc->ayBottom <= pwndClip->arcClipping.ayBottom)
			{
			if (parc->axLeft <= pwndClip->arcClipping.axRight)
				CharOutAbs(parc->axLeft, ayBottom,
						pbox->chBottomLeftCorner, di);
			if (parc->axRight <= pwndClip->arcClipping.axRight)
				CharOutAbs(axRight, ayBottom,
						pbox->chBottomRightCorner, di);
			}
		}
			
#endif /*WINDOW_OVERLAP*/

	/* adjust for border */
	axLeft = parc->axLeft + daxBorder;
	ayTop = parc->ayTop + dayBorder;

#ifdef WINDOW_OVERLAP
	if (pwndClip != NULL)
		{
		axRight = min(axRight, pwndClip->arcClipping.axRight);
		ayBottom = min(ayBottom, pwndClip->arcClipping.ayBottom);
		}
#endif /*WINDOW_OVERLAP*/
	
	if (axLeft < axRight)
		{
		/* always fill top */
#ifdef WINDOW_OVERLAP
		if ((pwndClip == NULL)||
			(parc->ayTop <= pwndClip->arcClipping.ayBottom))
#endif /*WINDOW_OVERLAP*/
		FillArc(axLeft, parc->ayTop, axRight, ayTop, pbox->chTopSide, di);
		if (fBottom)
#ifdef WINDOW_OVERLAP
		if ((pwndClip == NULL)||
			(parc->ayBottom <= pwndClip->arcClipping.ayBottom))
#endif /*WINDOW_OVERLAP*/
			FillArc(axLeft, ayBottom, axRight, parc->ayBottom, pbox->chBottomSide, di);
		}

	if (ayBottom > ayTop)
		{
		/* always fill left */
#ifdef WINDOW_OVERLAP
		if ((pwndClip == NULL)||
			(parc->axLeft <= pwndClip->arcClipping.axRight))
#endif /*WINDOW_OVERLAP*/
		FillArc(parc->axLeft, ayTop, axLeft, ayBottom,
		    pbox->chLeftSide, di);
		if (fRight)
#ifdef WINDOW_OVERLAP
		if ((pwndClip == NULL)||
			(parc->axRight <= pwndClip->arcClipping.axRight))
#endif /*WINDOW_OVERLAP*/
			FillArc(axRight, ayTop, parc->axRight, ayBottom,
			    pbox->chRightSide, di);
		}

	if (szTitle != NULL)
#ifdef WINDOW_OVERLAP
	if ((pwndClip == NULL)||
		(parc->ayTop <= pwndClip->arcClipping.ayBottom))
#endif /*WINDOW_OVERLAP*/
		{
		WORD	cch;
		int	cchMac;

		/* print title at top */
		if ((cch = strlen(szTitle)) != 0 &&
		    (cchMac = parc->axRight - parc->axLeft - 4) > 0)
			{
			/* enough room for title */
			if (cch > cchMac)
				cch = cchMac;

#ifdef WINDOW_OVERLAP
			axLeft += (parc->axRight - daxBorder 
						- axLeft - cch) / 2;
			if (axLeft + cch > axRight)
				cch = (axRight > axLeft) ? axRight - axLeft : 0;
#else
			axLeft += (axRight - axLeft - cch) / 2;
#endif /*WINDOW_OVERLAP*/
#ifdef WINDOW_OVERLAP
			if (axLeft-1 < axRight)
#endif /*WINDOW_OVERLAP*/
			CharOutAbs(axLeft-1, parc->ayTop, (ACHAR) ' ', di);
#ifdef WINDOW_OVERLAP
			if (axLeft < axRight)
#endif /*WINDOW_OVERLAP*/
			TextOutAbs(axLeft, parc->ayTop, szTitle, cch, di);
#ifdef WINDOW_OVERLAP
			if (axLeft + cch < axRight)
#endif /*WINDOW_OVERLAP*/
			CharOutAbs(axLeft + cch, parc->ayTop, ' ', di);
			}
		}

	EndDraw();

	}



VOID FARPUBLIC
ShadowArc(parc)
/*
  -- throw a shadow at the right bottom of a box
*/
REGISTER PARC parc;
	{
	AssertSz(parc->axRight - parc->axLeft > daxShadow
	    && parc->ayBottom - parc->ayTop > dayShadow,
	    "Box too small to shadow");

#ifdef WINDOW_OVERLAP
	if (pwndClip == NULL)
#endif /*WINDOW_OVERLAP*/
	if (diShadow == 0 ||
	    parc->axRight + daxShadow > axMac ||
	    parc->ayBottom + dayShadow > ayMac)
		return;	/* no shadowing or too big to shadow */

#ifdef WINDOW_OVERLAP
	if (pwndClip != NULL)
		{
		/* fill right hand side */
		if ((parc->axRight < pwndClip->arcClipping.axRight) &&
		    (parc->ayTop + dayShadow <= pwndClip->arcClipping.ayBottom))
			FillArc(parc->axRight, parc->ayTop+dayShadow,
				min(parc->axRight + daxShadow,
					 pwndClip->arcClipping.axRight),
				min(parc->ayBottom,
					 pwndClip->arcClipping.ayBottom),
	   			chShadow, diShadow);

		/* now fill bottom */
		if ((parc->ayBottom < pwndClip->arcClipping.ayBottom) &&
		    (parc->axLeft + daxShadow <= pwndClip->arcClipping.axRight))
			FillArc(parc->axLeft+daxShadow, parc->ayBottom,
				min(parc->axRight + daxShadow,
					 pwndClip->arcClipping.axRight),
				min(parc->ayBottom + dayShadow,
					 pwndClip->arcClipping.ayBottom),
	    			chShadow, diShadow);
		}
	else
#endif /*WINDOW_OVERLAP*/
		{
	/* fill right hand side */
	FillArc(parc->axRight, parc->ayTop+dayShadow, parc->axRight+daxShadow,
	    parc->ayBottom, chShadow, diShadow);
	/* now fill bottom */
	FillArc(parc->axLeft+daxShadow, parc->ayBottom, parc->axRight+daxShadow,
	    parc->ayBottom+dayShadow, chShadow, diShadow);
		}
	}
