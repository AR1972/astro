
/*
	COW : Character Oriented Windows

	rect.c : Rectangle support
*/

#define COW
#include <cow.h>

#define WINDOW
#include <uwindow.h>
#include <inscreen.h>



PUBLIC BOOL FARPUBLIC
PtInRect(prrc, rx, ry)
/*
  -- retuurn TRUE if point is in the rectangle
  -- Works for both rrcs and prrc
*/
RX rx;
RY ry;
REGISTER PRRC prrc;
	{
	StartPublic();
	ReturnPublic(rx >= prrc->rxLeft &&
		rx < prrc->rxRight &&
		ry >= prrc->ryTop &&
		ry < prrc->ryBottom, BOOL);
	}



PUBLIC VOID FARPUBLIC
SetRect(prrc, rxLeft, ryTop, rxRight, ryBottom)
/*
  -- set the coordinates of a rectangle
*/
REGISTER PRRC prrc;
RX rxLeft, rxRight;
RY ryTop, ryBottom;
	{
	StartPublic();
	prrc->rxLeft = rxLeft;
	prrc->ryTop = ryTop;
	prrc->rxRight = rxRight;
	prrc->ryBottom = ryBottom;
	StopPublic();
	}




PUBLIC BOOL FARPUBLIC
IntersectRect(prrcDest, prrcSrc1, prrcSrc2)
/*
  -- fille *prrcDest with the intersection of two rectangles
*/
REGISTER PRRC prrcSrc1;
REGISTER PRRC prrcSrc2;
REGISTER PRRC prrcDest;
	{
	StartPublic();
	prrcDest->rxLeft = max(prrcSrc1->rxLeft, prrcSrc2->rxLeft);
	prrcDest->rxRight = min(prrcSrc1->rxRight, prrcSrc2->rxRight);
	prrcDest->ryTop = max(prrcSrc1->ryTop, prrcSrc2->ryTop);
	prrcDest->ryBottom = min(prrcSrc1->ryBottom, prrcSrc2->ryBottom);
	if (IsRectEmpty(prrcDest))
		{
		prrcDest->rxLeft = 0;
		prrcDest->rxRight = 0;
		prrcDest->ryTop = 0;
		prrcDest->ryBottom = 0;
		ReturnPublic(FALSE, BOOL);
		}
	else
		ReturnPublic(TRUE, BOOL);
	}



PUBLIC VOID FARPUBLIC
UnionRect(prrcDest, prrcSrc1, prrcSrc2)
/*
  -- fill *prrcDest with the union of two rectangles
*/
REGISTER PRRC prrcSrc1, prrcSrc2;
PRRC prrcDest;
	{
	StartPublic();
	if (IsRectEmpty(prrcSrc1))
		*prrcDest = *prrcSrc2;
	else if (IsRectEmpty(prrcSrc2))
		*prrcDest = *prrcSrc1;
	else
		{
		prrcDest->rxLeft = min(prrcSrc1->rxLeft, prrcSrc2->rxLeft);
		prrcDest->rxRight = max(prrcSrc1->rxRight, prrcSrc2->rxRight);
		prrcDest->ryTop = min(prrcSrc1->ryTop, prrcSrc2->ryTop);
		prrcDest->ryBottom =
			max(prrcSrc1->ryBottom, prrcSrc2->ryBottom);
		}
	StopPublic();
	}



PUBLIC BOOL FARPUBLIC
IsRectEmpty(prrc)
/*
  -- return TRUE if rectangle is empty
*/
REGISTER PRRC prrc;
	{
	StartPublic();
	ReturnPublic(prrc->rxLeft >= prrc->rxRight ||
		prrc->ryTop >= prrc->ryBottom, BOOL);
	}



PUBLIC WORD FARPUBLIC
CwSizeRrc(prrc)
/*
  -- return the size (in words) needed to save the rectangle
  -- NOTE : also works for ARC's
*/
REGISTER PRRC prrc;
	{
	WORD	cw;

	cw = ((prrc)->rxRight-(prrc)->rxLeft) *
	    ((prrc)->ryBottom-(prrc)->ryTop);

#ifdef SCREEN_FFONT
	if (fFontAvailable)
		cw *= 2;
#endif /*SCREEN_FFONT*/

	return cw;
	}
