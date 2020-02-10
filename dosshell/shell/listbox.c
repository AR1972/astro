;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****   listbox.c - list box manager routines
**
**   Date      Author   Modification
** --------   --------  ------------------------------------------------
**  7/??/89   scottq    wrote all the functions
**  7/20/89   t-jeffro UserListProc now calls the listproc with proper x,y
**                      coords instead of filler.
**  7/21/89   scottq    made UserListProc go away, now called directly
*/
#include <common.h>
#include <icons.h>
#include <menus.h>
#include <filemgr.h>
#include <prot.h>

#define NOFILECLICKED (0)
#define DESELFILEONMOUSEUP (1)
#define DESELALLONMOUSEUP (2)
#define NOACTIONONMOUSEUP (3)

int  NextFocusLineChange(ListBoxData *,int);

extern BYTE gMouseX, gMouseY; /* this is here for slime with the scroll bar */
extern BYTE gMouseDownX, gMouseDownY;
extern WORD gCnx;
extern VOID     MySetColor(WORD color);

WORD gListKeyDelay = 18;


/*  NAME
 *      CalcBarRect
 *  DESCRIPTION
 *      Calculates the graphical absolute rectangle for the scrollbar belonging
 *      to the list.
 *  ARGUMENTS
 *      ListBoxData* is a pointer to the listbox in question
 *      RECT* is a near pointer to the destination rectanble
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      none
 *  STATE
 *      none
 *  COMMENTS
 *      Note that this calculates the rectangle in pixels, not characters!
 *  WARNINGS
 *  HACKS
 */

VOID CalcBarRect(ListBoxData *TestList,RECT *br)
{
#ifdef CUA1
    br->yTop   = (TestList->scroll.rect.ayTop+2)*CHEIGHT;
    br->yBottom= (TestList->scroll.rect.ayBottom-2)*CHEIGHT;
    br->xLeft  = TestList->scroll.rect.axLeft*CWIDTH;
    br->xRight = TestList->scroll.rect.axRight*CWIDTH;
#else
    br->yTop   = (TestList->scroll.rect.ayTop+1)*CHEIGHT;
    br->yBottom= (TestList->scroll.rect.ayBottom-1)*CHEIGHT;
    br->xRight = TestList->scroll.rect.axRight*CWIDTH;
    if(gisgraph)
	br->xLeft  = TestList->scroll.rect.axLeft*CWIDTH;
    else
	br->xLeft = br->xRight-1;
#endif
}

/*  NAME
 *      CalcThumbRect
 *  DESCRIPTION
 *      Calculates the graphical absolute rectangle for the scrollbar thumb
 *      belonging to the listbox.
 *  ARGUMENTS
 *      ListBoxData* is a pointer to the listbox in question
 *      RECT* is a near pointer to the destination rectanble
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      none
 *  STATE
 *      none
 *  COMMENTS
 *      Note that this calculates the rectangle in pixels, not characters!
 *  WARNINGS
 *  HACKS
 */

VOID CalcThumbRect(ListBoxData *TestList,RECT *tr)
{
#ifdef CUA1
    WORD theight;
    if (TestList->scroll.maxvalue <= 0)
    {
       tr->yTop   = (TestList->scroll.rect.ayTop+2) * CHEIGHT;
       tr->yBottom= (TestList->scroll.rect.ayBottom-2) * CHEIGHT;
    }
    else
    {
       theight = (WORD)(((long)((TestList->rect.ayBottom-TestList->rect.ayTop-4)*
		 (TestList->rect.ayBottom-TestList->rect.ayTop-4) * CHEIGHT))/
		 (TestList->scroll.maxvalue+TestList->numlinesonscreen));
       if (gisgraph)
       {
	  if (theight <= 10)
	       theight = 10;
       }
       else
       {
	  if (theight <= 1)
	       theight = 1;
       }
       tr->yTop =(WORD)((TestList->scroll.rect.ayTop+2)*CHEIGHT+
		 ((long)TestList->scroll.value*
		 ((long)(TestList->scroll.rect.ayBottom-
		 TestList->scroll.rect.ayTop-4)*CHEIGHT-theight))/
		 TestList->scroll.maxvalue);
       tr->yBottom = tr->yTop + theight;
    }
    tr->xLeft = (TestList->scroll.rect.axLeft)*CWIDTH+1;
    tr->xRight= (TestList->scroll.rect.axRight)*CWIDTH-1;
#else
    WORD theight;
    DWORD ttop;
    //theight = CHEIGHT;

    if (TestList->scroll.maxvalue <= 0)
    {
       tr->yTop   = (TestList->scroll.rect.ayTop+1) * CHEIGHT;
       tr->yBottom= (TestList->scroll.rect.ayBottom-1) * CHEIGHT;
    }
    else
    {

       theight = (WORD)(((long)((TestList->rect.ayBottom-TestList->rect.ayTop-2)*
		 (TestList->rect.ayBottom-TestList->rect.ayTop-2)))/
		 (TestList->scroll.maxvalue+TestList->numlinesonscreen));

	  if (theight <= 1)
		   theight = 1;

	  tr->yTop =(TestList->scroll.rect.ayTop+1)*CHEIGHT;

	  if(TestList->scroll.maxvalue != 0)
	  {
	      /* In order to get the scroll bar to appear to work
	       * smoothly, we cheat here so that the thumb will
	       * always align with mouse cursor, even though
	       * the correlation of the thumb and the scrolledness
	       * will be off
	       * This may cause the thumb to jump after the user
	       * lets go of the thumb.
	       * Not so easy at it seems to get this to work well!
	       */
	       if(TestList->scroll.dragging &&
		   gMouseY>TestList->scroll.rect.ayTop+
		       TestList->scroll.mouseoffset &&
		   (int)gMouseY<(int)(TestList->scroll.rect.ayBottom-
		       (int)theight-(int)TestList->scroll.mouseoffset) &&
		   gMouseX>=TestList->scroll.rect.axLeft-1 &&
		   gMouseX<TestList->scroll.rect.axRight+1)
		       tr->yTop = (gMouseY-TestList->scroll.mouseoffset)*CHEIGHT;
	      else
	      {
		  ttop=((DWORD)TestList->scroll.value)*(TestList->numlinesonscreen-2-theight);
		  tr->yTop += (WORD) ((ttop/(TestList->scroll.maxvalue))*CHEIGHT);
	      }
	      //ttop = (TestList->scroll.maxvalue+(TestList->numlinesonscreen-2-theight-1))/(TestList->numlinesonscreen-2-theight);
	      //tr->yTop += TestList->scroll.value/ttop;
	  }
	  tr->yBottom = tr->yTop + theight*CHEIGHT;
    }
    tr->xRight= (TestList->scroll.rect.axRight)*CWIDTH-1;
    if(gisgraph)
    {
       tr->xLeft = (TestList->scroll.rect.axLeft)*CWIDTH+1;
    }
    else
       tr->xLeft = tr->xRight-1;

#endif
}

/*  NAME
 *      DrawScrollBar
 *  DESCRIPTION
 *      Draws the scrollbar on the screen in the current graphics mode.
 *  ARGUMENTS
 *      ListBoxData* is a pointer to the listbox in question
 *      Init is TRUE if this is the first time the scroll bar is drawn.
 *      The entire scrollbar is drawn if Init is TRUE, otherwise only the
 *      Thumb area is updated.
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      none
 *  STATE
 *      Changes graphics pen color/state. All routines are expected to set
 *      this as desired before drawing, so this is ok.
 *  COMMENTS
 *      Draws scrollbar in either graphics mode, or character, depending on
 *      the global flag gisgraphics.
 *  WARNINGS
 *  HACKS
 */

VOID DrawScrollBar(ListBoxData *TestList,BOOL init)
{
    RECT bRect;
    RECT tRect;
    RECT tmpRect;
    RRC  rbRect;
    RRC  rtRect;
    WORD fore,back;

    if (TestList->stupid)
	return;

    FEnableMouseNest(FALSE);
    CalcThumbRect(TestList,&tRect);
    if (gisgraph)
    {
	CalcBarRect(TestList,&bRect);
	if (init)
	{
	  tmpRect.yTop=    bRect.yTop;
	  tmpRect.yBottom= bRect.yBottom;
	  tmpRect.xLeft=  bRect.xLeft;
	  tmpRect.xRight=  bRect.xRight;

	  if(gisgraph)
	  {
	  tmpRect.yTop   += TestList->pwd->arcWindow.ayTop*CHEIGHT;
	  tmpRect.yBottom+= TestList->pwd->arcWindow.ayTop*CHEIGHT;
	  tmpRect.xLeft  += TestList->pwd->arcWindow.axLeft*CWIDTH;
	  tmpRect.xRight += TestList->pwd->arcWindow.axLeft*CWIDTH;

	  GetIsaColor(isaScrollbar,&fore,&back);
	  MySetColor(back);

	     SetAreaPat(0);
	     SetLinePat(1);
	     Rectangle(&tmpRect);
	  }
	}
	tmpRect.yTop=    bRect.yTop;
	tmpRect.yBottom= tRect.yTop+1;
	tmpRect.xLeft=  bRect.xLeft;
	tmpRect.xRight=  bRect.xRight;

	if(gisgraph)
	{
	  GetIsaColor(isaScrollbar,&fore,&back);
	  MySetColor(back);

	   SetAreaPat(1);
	   SetLinePat(0);
	  tmpRect.yTop   += TestList->pwd->arcWindow.ayTop*CHEIGHT;
	  tmpRect.yBottom+= TestList->pwd->arcWindow.ayTop*CHEIGHT;
	  tmpRect.xLeft  += TestList->pwd->arcWindow.axLeft*CWIDTH;
	  tmpRect.xRight += TestList->pwd->arcWindow.axLeft*CWIDTH;


	   if (tmpRect.yTop < tmpRect.yBottom)
	   {
	    /* above the thumb */
	     Rectangle(&tmpRect);
	   }
	}
	tmpRect.yTop=    tRect.yTop+1;
	tmpRect.yBottom= tRect.yBottom;
	tmpRect.xLeft=  tRect.xLeft;
	tmpRect.xRight=  tRect.xRight;
	if ((gisgraph) && (tmpRect.yTop < tmpRect.yBottom))
	{
	  tmpRect.yTop   += TestList->pwd->arcWindow.ayTop*CHEIGHT;
	  tmpRect.yBottom+= TestList->pwd->arcWindow.ayTop*CHEIGHT;
	  tmpRect.xLeft  += TestList->pwd->arcWindow.axLeft*CWIDTH;
	  tmpRect.xRight += TestList->pwd->arcWindow.axLeft*CWIDTH;


	    if(TestList->scroll.dragging)
	    {
	       SetAreaPat(0);
	       SetLinePat(1);
	  GetIsaColor(isaScrollbar,&fore,&back);
	  MySetColor(back);

	       Rectangle(&tmpRect);
	       tmpRect.xLeft+=1;
	       tmpRect.xRight-=1;
	       tmpRect.yTop+=1;
	       tmpRect.yBottom-=1;
	    }
	    SetLinePat(0);
	    SetAreaPat(1);
	   GetIsaColor(isaElevator,&fore,&back);
	   MySetColor(back);

	    /* the thumb */
	    Rectangle(&tmpRect);
	}
	tmpRect.yTop=    tRect.yBottom;
	tmpRect.yBottom= bRect.yBottom+1;
	tmpRect.xLeft=  bRect.xLeft;
	tmpRect.xRight=  bRect.xRight;
	if (gisgraph)
	{
	  tmpRect.yTop   += TestList->pwd->arcWindow.ayTop*CHEIGHT;
	  tmpRect.yBottom+= TestList->pwd->arcWindow.ayTop*CHEIGHT;
	  tmpRect.xLeft  += TestList->pwd->arcWindow.axLeft*CWIDTH;
	  tmpRect.xRight += TestList->pwd->arcWindow.axLeft*CWIDTH;

	   SetAreaPat(1);
	   SetLinePat(0);
	  GetIsaColor(isaScrollbar,&fore,&back);
	  MySetColor(back);

	  if (tmpRect.yTop < tmpRect.yBottom)
	  {
	  /* below the thumb */
	    Rectangle(&tmpRect);
	  }
	}
    }
    else
    {
#ifdef CUA1
	rbRect.ryTop    = (BYTE)TestList->scroll.rect.ayTop+2;
	rbRect.ryBottom = (BYTE)TestList->scroll.rect.ayBottom-2;
	rbRect.rxLeft   = (BYTE)TestList->scroll.rect.axLeft;
	rbRect.rxRight  = (BYTE)TestList->scroll.rect.axRight;
	FillRrc(TestList->pwd,&rbRect,cinch._chScrollbar,isaScrollbar);
	rtRect.ryTop    = (BYTE)tRect.yTop;
	rtRect.ryBottom = (BYTE)tRect.yBottom;
	if (tRect.yBottom - tRect.yTop == 0)
	    ++ rtRect.ryBottom;
	rtRect.rxLeft   = (BYTE)tRect.xLeft-1;
	rtRect.rxRight  = (BYTE)tRect.xRight+1;
	FillRrc(TestList->pwd,&rtRect,cinch._chElevator,isaElevator);

#else
	/* top region of scroll bar */
	rbRect.ryTop    = (BYTE)TestList->scroll.rect.ayTop+1;
	rbRect.rxLeft   = (BYTE)TestList->scroll.rect.axLeft+1;
	rbRect.rxRight  = (BYTE)TestList->scroll.rect.axLeft+2;
	rbRect.ryBottom = (BYTE)tRect.yTop;
	FillRrc(TestList->pwd,&rbRect,cinch._chScrollbar,isaScrollbar);
	/* thumb */
	rtRect.ryTop    = (BYTE)tRect.yTop;
	rtRect.ryBottom = (BYTE)tRect.yBottom;
	if (tRect.yBottom - tRect.yTop == 0)
	    ++ rtRect.ryBottom;
	rtRect.rxLeft   = (BYTE)tRect.xLeft+1;
	rtRect.rxRight  = (BYTE)tRect.xRight+1;
	FillRrc(TestList->pwd,&rtRect,cinch._chElevator,isaElevator);
	/* bottom region of scroll bar */
	rbRect.ryTop = (BYTE)tRect.yBottom;
	rbRect.rxLeft   = (BYTE)TestList->scroll.rect.axLeft+1;
	rbRect.rxRight  = (BYTE)TestList->scroll.rect.axLeft+2;
	rbRect.ryBottom = (BYTE)TestList->scroll.rect.ayBottom-1;
	FillRrc(TestList->pwd,&rbRect,cinch._chScrollbar,isaScrollbar);

#endif
    }
    FEnableMouseNest(TRUE);

}

/*  NAME
 *      DrawScrollBarArrow
 *  DESCRIPTION
 *      Draws the scrollbar arrows and chevrons on the srceen in graphics mode.
 *  ARGUMENTS
 *      ListBoxData* is a pointer to the listbox in question
 *      UporDown is TRUE if we are drawing the Up arrow and chevron
 *      UporDown is FALSE if we are drwing the Down arrow and chevron
 *      enabled is TRUE if black
 *      enabled is FALSE if grey
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      none
 *  STATE
 *      Changes graphics pen color/state. All routines are expected to set
 *      this as desired before drawing, so this is ok.
 *  COMMENTS
 *      Draws scrollbar arrows in either graphics mode, or character, depending on
 *      the global flag gisgraphics.
 *  WARNINGS
 *  HACKS
 */

VOID DrawScrollBarArrow(ListBoxData *TestList,BOOL UporDown)
{
    WORD x;
    WORD y;

    if (UporDown)
    {
      y = TestList->scroll.rect.ayTop;
    }
    else
    {
      y = TestList->scroll.rect.ayBottom-1;
    }
    x = TestList->scroll.rect.axLeft;
    if (gisgraph)
    {
	 y   += TestList->pwd->arcWindow.ayTop;
	 x   += TestList->pwd->arcWindow.axLeft;

       SetAreaPat(0);
       SetLinePat(1);
       if (UporDown)
       {
	  PlotBmp(UpArrow,x*CWIDTH,y*CHEIGHT,isaBackground);
#ifdef CUA1
	  PlotBmp(UpChevron,x*CWIDTH,(y+1)*CHEIGHT,isaBackground);
	  Move(x*CWIDTH,(y+1)*CHEIGHT);
	  Draw((x+2)*CWIDTH-1,(y+1)*CHEIGHT);
#endif
	  Move(x*CWIDTH,y*CHEIGHT);
	  Draw(x*CWIDTH,(y+2)*CHEIGHT);
       }
       else
       {
	  PlotBmp(DownArrow,x*CWIDTH,y*CHEIGHT,isaBackground);
#ifdef CUA1

	  PlotBmp(DownChevron,x*CWIDTH,(y-1)*CHEIGHT,isaBackground);
	  Move(x*CWIDTH,(y)*CHEIGHT);
	  Draw((x+2)*CWIDTH-1,(y)*CHEIGHT);
#endif
	  Move(x*CWIDTH,(y-1)*CHEIGHT);
	  Draw(x*CWIDTH,(y+1)*CHEIGHT);
	 }
    }
    else
    {
#ifdef UGLYSCROLLBARS
	if(UporDown)
	{
	   TextOut(TestList->pwd,TestList->scroll.rect.axLeft,TestList->rect.ayTop,"",2,isaScrollbar);
	   TextOut(TestList->pwd,TestList->scroll.rect.axLeft,TestList->rect.ayTop+1,"\xf0\xf0",2,isaScrollbar);
	}
	else
	{
	   TextOut(TestList->pwd,TestList->scroll.rect.axLeft,TestList->scroll.rect.ayBottom-1,"",2,isaScrollbar);
	   TextOut(TestList->pwd,TestList->scroll.rect.axLeft,TestList->scroll.rect.ayBottom-2,"\xf0\xf0",2,isaScrollbar);
	}
#else
	if(UporDown)
	{
	   CharOut(TestList->pwd,TestList->scroll.rect.axLeft+1,TestList->rect.ayTop,chUpArrow,isaScrollbar);
	}
	else
	{
	   CharOut(TestList->pwd,TestList->scroll.rect.axLeft+1,TestList->scroll.rect.ayBottom-1,chDownArrow,isaScrollbar);
	}
#endif
    }
}

/*  NAME
 *      UpdateScrollBar
 *  DESCRIPTION
 *      Calls DrawScrollBar to redraw the scroll bar and thumb in the
 *      current state (maxvalue,value).
 *  ARGUMENTS
 *      ListBoxData* is a pointer to the listbox in question
 *      maxvalue is the maximum number of entries in the list
 *      value is the value the thumb should be set to.
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      none
 *  STATE
 *      Changes graphics pen color/state. All routines are expected to set
 *      this as desired before drawing, so this is ok.
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

VOID UpdateScrollBar(ListBoxData *TestList,int value,int maxvalue)
{
	WORD actualvalue;
    if (TestList->stupid)
	return;

   /*
	* the value may be beyond the maxvalue, but the scroll bar
	* should not relfect this!
	*/
   actualvalue = max(value,0);
   TestList->scroll.maxvalue = max(maxvalue,0);
   TestList->scroll.value = min(actualvalue,maxvalue);
   DrawScrollBar(TestList,!TestList->scroll.inited);
   /*
	* restore value to actual setting if it was too large
	*/
   TestList->scroll.value = actualvalue;
   TestList->scroll.inited = TRUE;
   TestList->scroll.doupdate = FALSE;
}

#define GSP_NONE 0
#define GSP_THUMB 1
#define GSP_LINEDOWN 2
#define GSP_LINEUP 3
#define GSP_PAGEDOWN 4
#define GSP_PAGEUP 5
#define PAGETICKS 2

WORD wStartPart;

/*  NAME
 *    GetScrollPart
 *  DESCRIPTION
 *    Identifies the part of the scroll bar where y is
 *  ARGUMENTS
 *    y is the y value of the mouse
 *    tRect is the rectangle of the thumb
 *    tRect is the rectangle of the scroll bar
 *  RETURN VALUE
 *    scroll part
 *  EFFECTS
 *    none
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

WORD NEAR PASCAL GetScrollPart(WORD x, WORD y, RECT *tRect, ARC *sRect)
{
    if(y<sRect->ayTop || y>=sRect->ayBottom)
	return(GSP_NONE);
    else if(x>=sRect->axLeft-gisgraph && x<sRect->axRight+1 &&
	    wStartPart==GSP_THUMB)
	return(GSP_THUMB);
    else if(x<=sRect->axLeft-gisgraph || x>=sRect->axRight)
	return(GSP_NONE);
    else if(y == sRect->ayTop)
	return(GSP_LINEDOWN);
    else if(y == sRect->ayBottom - 1)
	return(GSP_LINEUP);
#ifdef CUA1
    else if(y == sRect->ayTop + 1)
	return(GSP_PAGEDOWN);
    else if(y == sRect->ayBottom - 2)
	return(GSP_PAGEUP);
#else
    else if(y < tRect->yTop)
	return(GSP_PAGEDOWN);
    else if(y >= tRect->yBottom)
	return(GSP_PAGEUP);
#endif
    else
	return(GSP_THUMB);
}

BOOL ScrollPause(BOOL reset)
{
/* This puts a delay after an initial call to ScrollPause */
    static DWORD nextTick = 0;

    BOOL retval;

    retval = ClockTicks() >= nextTick;

    if(reset)
/* We initiate a new delay */
	nextTick = ClockTicks() + (reset==1 ? 4 : reset);

    return(retval);
}

/*  NAME
 *    ScrollBarMouse
 *  DESCRIPTION
 *    Handles mouse messages that are directed to the listbox's scrollbar
 *  ARGUMENTS
 *    ListBoxData* is a pointer to the listbox in question
 *    message is the mouse message that caused the scroll
 *  RETURN VALUE
 *    none
 *  EFFECTS
 *    none
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

BOOL PASCAL ScrollBarMouse(ListBoxData *TestList, WORD message)
{
    BOOL bMDownInScroll;
    RECT trect, prect;
    RRC  rrect;
    WORD newvalue, theight, fore, back, wPart;
    DWORD ticks;

    if(TestList->stupid)
	return(FALSE);
    if(TestList->scroll.dragging && message==WM_MOUSEIDLE)
	return(TRUE);

    CalcThumbRect(TestList, &trect);
/* thumb is in pixels (equal chars in text), mouse is in chars! */
    if(gisgraph) {
	trect.yTop    = trect.yTop    /CHEIGHT;
	trect.yBottom = trect.yBottom /CHEIGHT;
	trect.xLeft   = trect.xLeft   /CWIDTH;
	trect.xRight  = trect.xRight  /CWIDTH;
    }

    if(message == WM_LBUTTONDOWN) {
	TestList->scroll.mouseoffset = gMouseY - trect.yTop;
	wStartPart = GSP_NONE;
	if((wStartPart = GetScrollPart(gMouseX, gMouseY, &trect,
		&TestList->scroll.rect)) == GSP_THUMB &&
		(gMouseX<TestList->scroll.rect.axLeft ||
		gMouseX>TestList->scroll.rect.axRight))
	    wStartPart = GSP_NONE;
    }

/* We check to make sure mouse was originally pressed in the scroll bar */
    bMDownInScroll = !(gMouseDownX<TestList->scroll.rect.axLeft ||
	    gMouseDownX>TestList->scroll.rect.axRight ||
	    gMouseDownY<TestList->scroll.rect.ayTop ||
	    gMouseDownY>=TestList->scroll.rect.ayBottom);
    wPart = GetScrollPart(gMouseX, gMouseY, &trect, &TestList->scroll.rect);
    
    if(bMDownInScroll) {
	if(wPart != wStartPart)
	    return(TRUE);
    } else if(wPart!=GSP_LINEDOWN && wPart!=GSP_LINEUP)
	return(FALSE);

    if(message == WM_MOUSEIDLE) {
	if(!ScrollPause(FALSE))
	    return(TRUE);
    } else
	ScrollPause(TRUE);

    switch(wPart) {
    case(GSP_THUMB):
	theight = trect.yBottom - trect.yTop;
	if(!(TestList->numlinesonscreen - 2 - theight))
	    break;

	TestList->scroll.dragging = TRUE;
	newvalue = (TestList->scroll.maxvalue + TestList->numlinesonscreen -
		theight - 3)/(TestList->numlinesonscreen - 2 - theight);
	newvalue *= gMouseY - TestList->scroll.mouseoffset -
		TestList->scroll.rect.ayTop - 1;
	newvalue -= TestList->numlinesscrolled;
	DoScrollListBox(TestList, newvalue, FALSE);
	break;

    case(GSP_LINEDOWN):
	DoScrollListBox(TestList, -1, TRUE);
	break;

    case(GSP_LINEUP):
	DoScrollListBox(TestList, 1, TRUE);
	break;

#ifdef CUA1
    case(GSP_PAGEDOWN):
	PageDown(TestList);
	break;

    case(GSP_PAGEUP):
	PageUp(TestList);
	break;
#else
    case(GSP_PAGEDOWN):
	rrect.ryTop    = TestList->scroll.rect.ayTop + 1;
	rrect.rxLeft   = TestList->scroll.rect.axLeft;
	rrect.rxRight  = TestList->scroll.rect.axRight;
	rrect.ryBottom = (RY) trect.yTop;

	FEnableMouseNest(FALSE);
	if(gisgraph) {
	    rrect.ryTop    += TestList->pwd->arcWindow.ayTop;
	    rrect.ryBottom += TestList->pwd->arcWindow.ayTop;
	    rrect.rxLeft   += TestList->pwd->arcWindow.axLeft;
	    rrect.rxRight  += TestList->pwd->arcWindow.axLeft;

	    prect.yTop    = rrect.ryTop    *CHEIGHT +1;
	    prect.yBottom = rrect.ryBottom *CHEIGHT -1;
	    prect.xLeft   = rrect.rxLeft   *CWIDTH  +1;
	    prect.xRight  = rrect.rxRight  *CWIDTH  -1;

	    SetAreaPat(1);
	    SetLinePat(0);
	    GetIsaColor(isaHilite, &fore, &back);
	    MySetColor(back);
	    Rectangle(&prect);
	} else {
	    ++rrect.rxLeft;
	    FillRrc(TestList->pwd, &rrect, cinch._chScrollbar, isaHilite);
	}
	FEnableMouseNest(TRUE);

	ticks = ClockTicks();
	while(ClockTicks() - PAGETICKS < ticks) /* do nothing */ ;
	PageDown(TestList);
	break;

    case(GSP_PAGEUP):
	rrect.ryTop = (RY) trect.yBottom;
	rrect.rxLeft= TestList->scroll.rect.axLeft;
	rrect.rxRight=TestList->scroll.rect.axRight;
	rrect.ryBottom = TestList->scroll.rect.ayBottom-1;

	FEnableMouseNest(FALSE);
	if(gisgraph) {
	    rrect.ryTop   += TestList->pwd->arcWindow.ayTop;
	    rrect.ryBottom+= TestList->pwd->arcWindow.ayTop;
	    rrect.rxLeft  += TestList->pwd->arcWindow.axLeft;
	    rrect.rxRight += TestList->pwd->arcWindow.axLeft;

	    prect.yTop = rrect.ryTop*CHEIGHT+1;
	    prect.yBottom = rrect.ryBottom*CHEIGHT-1;
	    prect.xLeft = rrect.rxLeft*CWIDTH+1;
	    prect.xRight = rrect.rxRight*CWIDTH-1;

	    SetAreaPat(1);
	    SetLinePat(0);
	    GetIsaColor(isaHilite,&fore,&back);
	    MySetColor(back);

	    Rectangle(&prect);
	} else {
	    ++rrect.rxLeft;
	    FillRrc(TestList->pwd,&rrect,cinch._chScrollbar,isaHilite);
	}
	FEnableMouseNest(TRUE);

	ticks = ClockTicks();
	while(ClockTicks() - PAGETICKS < ticks);
	PageUp(TestList);
	break;
#endif
    default:
	break;
    }

    return(TRUE);
}

/*  NAME
 *      GetNumItems
 *  DESCRIPTION
 *      Calls the WListProc for the ListBox to get the number of items
 *      in the list
 *  ARGUMENTS
 *      ListBoxData* is a pointer to the listbox in question
 *  RETURN VALUE
 *      number of items in the list
 *  EFFECTS
 *      none
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
int GetNumItems(ListBoxData *TestList)
{
    return(TestList->ListProc(tmmCount, NULL, 0, TestList->tmc, 0, 0, 0));
}

VOID DoSetTitle(ListBoxData *TestList,char *title)
{
    TestList->title = title;
}

VOID DoDrawTitle(ListBoxData *TestList)
{
#if 1
     RECT lRect;
     WORD xval;
     WORD len;
     WORD width;
     char blankline[256];
     WORD fore,back;
     if (TestList->stupid)
			return;

   for(len=0;len<90;len++)
		blankline[len]=' ';

	FEnableMouseNest(FALSE);
	

   TextOut(TestList->pwd,TestList->rect.axLeft,
	    TestList->rect.ayTop-1,
			blankline,TestList->rect.axRight-TestList->rect.axLeft+!gisgraph,
			TestList->hasglobalfocus?isaHilite:isaTitlebar);

   len = strlen(TestList->title);
   width = TestList->rect.axRight - TestList->rect.axLeft;
   if (len > width)
	len = TestList->rect.axLeft;
   xval = TestList->rect.axLeft + width/2 - len/2;
	TextOut(TestList->pwd,(RX)xval,TestList->rect.ayTop-1,TestList->title,-1,
		TestList->hasglobalfocus?isaHilite:isaTitlebar); //gisgraph?isaBackground:isaWhiteOnBlack);
   lRect.xLeft = TestList->rect.axLeft * CWIDTH-1;
   if (lRect.xLeft < 0)
		lRect.xLeft = 0;
   lRect.yTop   = (TestList->rect.ayTop-1) * CHEIGHT - 1;
   lRect.xRight = TestList->rect.axRight * CWIDTH;
   lRect.yBottom = TestList->rect.ayTop *CHEIGHT;
   if (gisgraph)
   {
		GetIsaColor(isaBackground,&fore,&back);
		MySetColor(fore);

	SetAreaPat(0);
	SetLinePat(1);
		lRect.yTop   += TestList->pwd->arcWindow.ayTop*CHEIGHT;
		lRect.yBottom+= TestList->pwd->arcWindow.ayTop*CHEIGHT;
		lRect.xLeft  += TestList->pwd->arcWindow.axLeft*CWIDTH;
		lRect.xRight += TestList->pwd->arcWindow.axLeft*CWIDTH;
		/*
		* we don't want the line for the listbox to be adjacent
		* to the menu's underline
		*/
	   lRect.yTop += 1;
	   /*
	    * When in CGA graphics, we don't want to draw the top
	    * line at all, since it will over-write the menu text
	    */
	   if(CHEIGHT <= SMALLHEIGHT)
	   {
	       /* bottom only BUG BUG this is not general
			  * and assumes that lists on top will always end
			  * at screen edges!
			  */
	       //Move(lRect.xRight,lRect.yBottom);
	       //Draw(lRect.xLeft,lRect.yBottom);
	   }
	   else
	   {
		  Rectangle(&lRect);
		   /* this is an extra kludge! for mismatched colors */
		   GetIsaColor(isaBorders,&fore,&back);
		   MySetColor(fore);

		   Move(lRect.xRight,lRect.yTop);
		   Draw(lRect.xLeft,lRect.yTop);
		   GetIsaColor(isaBackground,&fore,&back);
		   MySetColor(fore);

	   }
	}
	FEnableMouseNest(TRUE);
#else
   WORD len;
	WORD slen;
   WORD fore,back;
	ISA  theisa;
	WORD titlestart;
	char thechar;
	int i;
	int x;
	int y;

   if (TestList->stupid)
		return;
	/* In order to reduce screen flicker, we draw the title one character
    * at a time, and then draw its graphics box before going on to the
    * next character.
	 */
	len = TestList->rect.axRight-TestList->rect.axLeft+!gisgraph;
	theisa = TestList->hasglobalfocus?isaHilite:isaTitlebar;
	slen = strlen(TestList->title);
	if(slen > len)
	{
		titlestart = 0;
	}
	else
	{
		titlestart = len/2 - (slen+1)/2;
	}
	FEnableMouseNest(FALSE);
	if(gisgraph)
	{
		GetIsaColor(isaBackground,&fore,&back);
		MySetColor(fore);
	SetAreaPat(0);
	SetLinePat(1);
	}
	for(i=0;i<len;i++)
	{
		if((i < titlestart) || (i>titlestart+slen))
		{
			thechar= ' ';
		}
		else
		{
			thechar = TestList->title[i-titlestart];
		}
		CharOut(TestList->pwd,(RX) TestList->rect.axLeft+i,
									 (RY) TestList->rect.ayTop-1,thechar,theisa);
		if(gisgraph)
		{
			if(i == 0) /* draw the left hand border */
			{
				x =TestList->rect.axLeft*CWIDTH -1;
				if(x<0)
					x = 0;
				y =  (TestList->rect.ayTop-1)*CHEIGHT;
				Move(x,y);
				y =  (TestList->rect.ayTop)*CHEIGHT-1;
				Draw(x,y);
			}
			else
			if(i==len-1) /* draw the right hand border */
			{
				x =TestList->rect.axRight*CWIDTH-1;
				if(x<0)
					x = 0;
				y =  (TestList->rect.ayTop-1)*CHEIGHT;
				Move(x,y);
				y =  (TestList->rect.ayTop)*CHEIGHT-1;
				Draw(x,y);
			}
			/* draw the top boarder */
			Move((TestList->rect.axLeft+i)*CWIDTH,(TestList->rect.ayTop-1)*CHEIGHT);
			Draw((TestList->rect.axLeft+i+1)*CWIDTH,(TestList->rect.ayTop-1)*CHEIGHT);
			/* draw the bottom boarder */
			Move((TestList->rect.axLeft+i)*CWIDTH,(TestList->rect.ayTop)*CHEIGHT-1);
			Draw((TestList->rect.axLeft+i+1)*CWIDTH,(TestList->rect.ayTop)*CHEIGHT-1);
			

		}
		FEnableMouseNest(TRUE);

	}
#endif
}

VOID SetTitle(ListBoxData *TestList,char *title)
{
    if (TestList->stupid)
	return;
	DoSetTitle(TestList,title);
	DoDrawTitle(TestList);
}

/*  NAME
 *      FrameListBox
 *  DESCRIPTION
 *      Draws the outline and scroll bar for the listbox. This should be
 *      done as little as possible, as it will cause flicker.
 *  ARGUMENTS
 *      ListBoxData* is a pointer to the listbox in question
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      none
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
VOID FrameListBox(ListBoxData *TestList)
{
    RECT lRect;
    BOX  tboxinfo;
    WORD fore,back;

    if (TestList->stupid)
	return;

    if(gisgraph)
       DoDrawTitle(TestList);
	DrawScrollBarArrow(TestList,TRUE);
	DrawScrollBarArrow(TestList,FALSE);

    if (gisgraph)
    {
	lRect.xLeft = TestList->rect.axLeft * CWIDTH-1;
	if (lRect.xLeft < 0)
	    lRect.xLeft = 0;
	lRect.yTop  = TestList->rect.ayTop * CHEIGHT - 1;
	lRect.xRight = TestList->rect.axRight * CWIDTH;
	lRect.yBottom = TestList->rect.ayBottom *CHEIGHT+1;

	lRect.yTop   += TestList->pwd->arcWindow.ayTop*CHEIGHT;
	lRect.yBottom+= TestList->pwd->arcWindow.ayTop*CHEIGHT;
	lRect.xLeft  += TestList->pwd->arcWindow.axLeft*CWIDTH;
	lRect.xRight += TestList->pwd->arcWindow.axLeft*CWIDTH;

	    GetIsaColor(isaBackground,&fore,&back);
	    MySetColor(fore);

	SetAreaPat(0);
	SetLinePat(1);
	if(CHEIGHT <= SMALLHEIGHT)
	{
	    /* bottom */
	    //BUG BUG this assumes that we never draw the bottom
	    // line of a listbox if it is on the second to last
	    // line of the screen and we are in small graphics mode!
	    if(lRect.yBottom != (ayMac-1)*CHEIGHT+1)
	    {
	      Move(lRect.xRight,lRect.yBottom);
	      Draw(lRect.xLeft,lRect.yBottom);
	    }
	    /* top */
	    Move(lRect.xRight,lRect.yTop);
	    Draw(lRect.xLeft,lRect.yTop);
	    /* right */
	    Move(lRect.xRight-1,lRect.yBottom);
	    Draw(lRect.xRight-1,lRect.yTop);
	    /* left */
	    Move(lRect.xLeft,lRect.yTop);
	    Draw(lRect.xLeft,lRect.yBottom);

	}
	else
	   Rectangle(&lRect);
    }
    else
    {
	tboxinfo.chTopLeftCorner      = cinch._chTopLeftCorner1;
	tboxinfo.chTopRightCorner     = cinch._chTopRightCorner1;
	tboxinfo.chBottomLeftCorner   = cinch._chBottomLeftCorner1;
	tboxinfo.chBottomRightCorner  = cinch._chBottomRightCorner1;
	tboxinfo.chTopSide            = cinch._chTopSide1;
	tboxinfo.chBottomSide         = cinch._chBottomSide1;
	tboxinfo.chLeftSide           = cinch._chLeftSide1;
	tboxinfo.chRightSide          = cinch._chRightSide1;
	TestList->rect.ayTop -= 1;
	TestList->rect.ayBottom += 1;
	TestList->rect.axRight += 1;
	DrawBox(TestList->pwd, (RRC *)&TestList->rect, &tboxinfo, TestList->color);
	TestList->rect.ayTop += 1;
	TestList->rect.ayBottom -= 1;
	TestList->rect.axRight -= 1;
	DoDrawTitle(TestList);
    }
    UpdateScrollBar(TestList,TestList->numlinesscrolled,GetNumItems(TestList)-TestList->numlinesonscreen);
}

/*  NAME
 *      MakeListStupid
 *  DESCRIPTION
 *      The listbox will no longer have a scroll bar or boundary
 *  ARGUMENTS
 *      ListBoxData* is a pointer to the listbox in question .
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      none
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
VOID MakeListStupid(ListBoxData *TestList)
{
	TestList->stupid = TRUE;
	Always_Explicit(TestList);
}

/*  NAME
 *      ListBoxInit
 *  DESCRIPTION
 *      Initializes the data structurs for the listbox.
 *  ARGUMENTS
 *      ListBoxData* is a pointer to the listbox in question .
 *      ListProc is the user List procedure which handles tmm messages.
 *      PWND is a pointer to the listboxe's window
 *      Top/left/bottom/right is the character coordinates of the listbox.
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      none
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
VOID ListBoxInit(ListBoxData *TestList,WORD (*ListProc)(),PWND pwd,WORD top,WORD left,WORD bottom,WORD right,char *title, WORD tmc,WORD startfocusabsolute,WORD startfocusrelative)
{

    int temp ;

    TestList->halted = FALSE;

    TestList->ListProc = ListProc;
    TestList->tmc = tmc;
    TestList->rect.ayTop = (BYTE)top;
    TestList->rect.ayBottom = (BYTE)bottom - !gisgraph;
    TestList->rect.axRight = (BYTE)right - !gisgraph;
    TestList->rect.axLeft = (BYTE)left;
    TestList->pwd = pwd;
    TestList->numlinesonscreen = TestList->rect.ayBottom-TestList->rect.ayTop;
    TestList->update = TRUE;

    temp = GetNumItems(TestList) ;

    TestList->scroll.value = 0;
    TestList->scroll.maxvalue = 0;
    TestList->scroll.rect.ayTop = (BYTE)TestList->rect.ayTop;
    TestList->scroll.rect.ayBottom = (BYTE)TestList->rect.ayBottom;
    TestList->scroll.rect.axRight = (BYTE)TestList->rect.axRight;
    TestList->scroll.rect.axLeft = (BYTE)TestList->rect.axRight-2;
    TestList->scroll.dragging = FALSE;
    TestList->scroll.inited = FALSE;
    TestList->scroll.doupdate = TRUE;
    TestList->inited = FALSE;
    TestList->blank = TRUE;
    TestList->stupid = FALSE;
    TestList->hasglobalfocus = TRUE;
    DoSetTitle(TestList,title);

    TestList->nextlinetoupdate = 0;
		
	 /* -1 for startfocus's means they have been set already; use whats there */
	 if(startfocusrelative == -1)
			startfocusrelative = TestList->focusitem-TestList->numlinesscrolled;
	 if(startfocusabsolute == -1)
			startfocusabsolute = TestList->focusitem;

    if (startfocusrelative >= TestList->numlinesonscreen)
	startfocusrelative = TestList->numlinesonscreen-1 ;

    TestList->numlinesscrolled = startfocusabsolute-startfocusrelative;
    if ( (TestList->numlinesscrolled < 0) ||
	       (TestList->numlinesscrolled >= temp) )
	 TestList->numlinesscrolled = 0 ;

    TestList->focusitem = startfocusabsolute;
    TestList->anchor = TestList->focusitem;
    if ( (startfocusabsolute < 0) ||
	       (startfocusabsolute >= temp) )
	 TestList->focusitem = TestList->numlinesscrolled;
    TestList->ListProc(tmmSetFocus, NULL, TestList->focusitem, TestList->tmc,
	    0, 0, 0);

    TestList->drawahead= -1;
    TestList->drawbehind = -1;
    TestList->mode = 1; /* default to implicit mode */
    TestList->alwaysexplicit = FALSE; /* default to allow implicit mode */
	 TestList->color = isaBackground;
	 TestList->LastClicked = NOFILECLICKED;
	 TestList->lastmousestate = 0;
	 TestList->lastmouseY = 0;
	 TestList->lastmouseX = 0;
}

VOID ListBoxHalt(ListBoxData *TestList)
{
    RRC rrect;
    rrect.ryTop = TestList->rect.ayTop-1;
    rrect.ryBottom = TestList->rect.ayBottom;
    rrect.rxLeft = TestList->rect.axLeft;
    rrect.rxRight = TestList->rect.axRight;

    FEnableMouseNest(FALSE);
    FillRrc(TestList->pwd, &rrect, ' ', TestList->color);
    FEnableMouseNest(TRUE);
    TestList->halted = TRUE;
}


/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

BOOL DrawListItem(ListBoxData *TestList,WORD isz,BOOL isfocus)
{
    WORD redrawline;
    TestList->blank = FALSE;

    if (TestList->drawbehind == -1)
    {
       redrawline = TestList->numlinesscrolled;
    }
    else
    {
	redrawline = TestList->drawbehind;
    }
    redrawline += isz;
    if(redrawline>=TestList->numlinesscrolled &&
	    redrawline<TestList->numlinesscrolled+TestList->numlinesonscreen)
	TestList->ListProc(tmmDrawItem, NULL, redrawline,
		       TestList->tmc, TestList->rect.axLeft+1,
		       TestList->rect.ayTop+isz, isfocus & TestList->hasglobalfocus);
    return(TRUE);
}

VOID GlobalFocusBox(ListBoxData *TestList,BOOL yesorno)
{
   if(yesorno)
	gCurrentTMC = TestList->tmc;

	if (TestList->halted)
		return;

   TestList->hasglobalfocus = yesorno;
   DoDrawTitle(TestList);

/* Following #ifdefed out code was introduced as a hack to make the item
 * with focus to be selected when one moves the focus to the file listbox
 * when one is not in add mode.
 */
#if 0
	if((yesorno) && (TestList->mode) &&
				 (!TestList->alwaysexplicit) && (!(TestList->lastmousestate&MK_LBUTTON)))
	{
	  TestList->ListProc(tmmSelect, NULL,
			     TestList->focusitem,
				   TestList->tmc,0,TestList->rect.ayTop+TestList->focusitem-TestList->numlinesscrolled,0);      
	}
#endif

   InsertListItem(TestList,TestList->focusitem);
}


/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

VOID DoUpdateListBox(ListBoxData *TestList,WORD nlines)
{
      int i;

      if (TestList->halted)
	return;
      if (TestList->update)
      {
	if(TestList->numlinesscrolled &&
		TestList->numlinesscrolled+TestList->numlinesonscreen
		> GetNumItems(TestList))
	    DoScrollListBox(TestList, 0, FALSE);
	 FEnableMouseNest(FALSE);
	 for(i=0;i<nlines;i++)
	 {
	     if (TestList->nextlinetoupdate < TestList->numlinesonscreen)
	     {
		  DrawListItem(TestList,TestList->nextlinetoupdate,
		      TestList->focusitem == TestList->numlinesscrolled
		  +TestList->nextlinetoupdate);
		  ++TestList->nextlinetoupdate;
	     }
	     else
	     {
		    TestList->update = FALSE;
		    break;
	     }
	 }
	 FEnableMouseNest(TRUE);
      }
      if(TestList->scroll.doupdate)
      {
	UpdateScrollBar(TestList,TestList->numlinesscrolled,
	   GetNumItems(TestList)-TestList->numlinesonscreen);
      }
}
/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

#if 0
VOID QuickRedisplayList(ListBoxData *TestList)
{
      if (TestList->halted)
	return;
    TestList->numlinesscrolled = 0;
    TestList->focusitem = 0;
    InsertListItem(TestList,0);
}
#endif

/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

VOID DoRedisplayList(ListBoxData *TestList)
{
    RRC rrect;
      if (TestList->halted)
	return;
    rrect.ryTop = TestList->rect.ayTop;
    rrect.ryBottom = TestList->rect.ayBottom;
    rrect.rxLeft = TestList->rect.axLeft+!gisgraph;
    rrect.rxRight = TestList->rect.axRight - 2;

    if (!TestList->blank)
    {
      FEnableMouseNest(FALSE);
      FillRrc(TestList->pwd, &rrect, ' ', TestList->color);
      FEnableMouseNest(TRUE);
      TestList->blank = TRUE;
    }
    TestList->numlinesscrolled = 0;
    TestList->focusitem = 0;
    TestList->ListProc(tmmSetFocus, NULL, TestList->focusitem, TestList->tmc,
	    0, 0, 0);
    InsertListItem(TestList,0);
}
/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

VOID DoScrollListBox(ListBoxData *TestList, int amt, BOOL updatenow)
{
    int amount, scrolllinesleft;
    WORD numitems;

    numitems = GetNumItems(TestList);
    scrolllinesleft = numitems - TestList->numlinesonscreen -
	    TestList->numlinesscrolled;

    if(amt > 0)
	amount = min(amt, scrolllinesleft);
    else
	amount = max(amt, -TestList->numlinesscrolled);

    if(numitems <= TestList->numlinesonscreen)
	amount = -TestList->numlinesscrolled;
    else if(scrolllinesleft < 0)
	amount = min(amount, scrolllinesleft);

    if(!amount)
	return;
#if 0 /* I don't see how you can pass this test (assuming numlinesscrolled>0) */
    if(amount>0 && numitems<TestList->numlinesscrolled +
	    TestList->numlinesonscreen))
	amount = 0;
#endif

    TestList->numlinesscrolled += amount;
    FEnableMouseNest(FALSE);
    UpdateScrollBar(TestList, TestList->numlinesscrolled,
	    numitems-TestList->numlinesonscreen);
    FEnableMouseNest(TRUE);
// #define FOCUSVISIBLE
#ifdef FOCUSVISIBLE
    if(TestList->focusitem < TestList->numlinesscrolled) {
	TestList->focusitem = TestList->numlinesscrolled;
    } else if(TestList->focusitem >=
	    TestList->numlinesscrolled+TestList->numlinesonscreen) {
	TestList->focusitem =
		TestList->numlinesscrolled+TestList->numlinesonscreen-1;
    }
    TestList->ListProc(tmmSetFocus, NULL, TestList->focusitem, TestList->tmc,
	    0, 0, 0);
#endif
    TestList->update = TRUE;
    TestList->nextlinetoupdate = 0;
    if(updatenow)
	DoUpdateListBox(TestList, TestList->numlinesonscreen);
}

/*      NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
int NextFocusLineChange(ListBoxData *TestList, int amt)
{
    int nextfocus, numitems;

    numitems = GetNumItems(TestList);
    nextfocus = TestList->focusitem;
    if(amt > 0)
	nextfocus += min(amt, numitems - 1 - nextfocus);
    else
	nextfocus += max(amt, -nextfocus);

    if(nextfocus < 0)
	return(0);
    if(nextfocus >= numitems)
	return(numitems - 1);
    return(nextfocus);
}

/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

VOID FocusLineChange(ListBoxData *TestList, int amt)
{
    int numitems, nextfocusitem, saveFocus;

    if(!amt || !(numitems=GetNumItems(TestList)))
	return;
    nextfocusitem = NextFocusLineChange(TestList, amt);

    saveFocus = TestList->focusitem;
    TestList->focusitem = nextfocusitem;
    TestList->ListProc(tmmSetFocus, NULL, TestList->focusitem,
	    TestList->tmc, 0, 0, 0);

    if(nextfocusitem>=TestList->numlinesscrolled+TestList->numlinesonscreen ||
	    nextfocusitem<TestList->numlinesscrolled) {
	nextfocusitem -= TestList->numlinesscrolled;
	if(nextfocusitem > 0)
	    nextfocusitem -= TestList->numlinesonscreen - 1;
	DoScrollListBox(TestList, nextfocusitem, !TestList->halted);
    } else if(!TestList->halted) {
	DrawListItem(TestList, saveFocus-TestList->numlinesscrolled,
		FALSE);
	DrawListItem(TestList, TestList->focusitem-TestList->numlinesscrolled,
		TRUE);
    }
}

/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */

VOID PageDown(ListBoxData *TestList)
{
   if (GetNumItems(TestList) != 0)
		DoScrollListBox(TestList,-TestList->numlinesonscreen,TRUE);
}
/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
VOID PageUp(ListBoxData *TestList)
{
   if (GetNumItems(TestList) != 0)
		DoScrollListBox(TestList,TestList->numlinesonscreen,TRUE);
}

/*  NAME
 *      Get_Focus_Line
 *  DESCRIPTION
 *      Return the line number of the line that is the "focus" in the
 *  listbox.  This will return the *last* item that had the focus in the
 *  case that the listbox itself does not have the focus.
 *  ARGUMENTS
 *      Pointer to the list box
 *  RETURN VALUE
 *      line number of focus item
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
WORD Get_Focus_Line(ListBoxData *TestList)
{
   return(TestList->focusitem);
}

/*
 *Select range from anchor to toitem
 */
VOID SelectRange(ListBoxData *TestList,WORD toitem)
{
    WORD startrange;
    WORD endrange;

	 /* for stupid list boxes, we don't do range selects! */
	 if (TestList->stupid)
		return ;

    startrange = min(TestList->anchor,toitem);
    endrange = max(TestList->anchor,toitem);
    InsertListItem(TestList,startrange);
    do
    {
	   TestList->ListProc(tmmSelect, (char *) ISRANGESELECT,
	   startrange,
	   TestList->tmc,-1,-1,0);
	   ++startrange;

    }while(startrange <= endrange);
}

/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
BOOL ListMouse(ListBoxData *TestList,WORD x,WORD y,WORD message,BOOL state)
{
    int thisclick;

    if(TestList->halted)
	return(FALSE);

    switch(message) {
    case WM_LBUTTONUP:
	TestList->lastmousestate = (BYTE)state;
	break;

    case WM_MOUSEIDLE:
	break;

    case WM_LBUTTONDOWN:
	gMouseDownX = (BYTE)x;
	gMouseDownY = (BYTE)y;
    case WM_MOUSEMOVE:
	gMouseX = (BYTE)x;
	gMouseY = (BYTE)y;
    default:
	TestList->lastmouseX = (BYTE)x;
	TestList->lastmouseY = (BYTE)y;
	TestList->lastmousestate = (BYTE)state;
	break;
    }

/* Set thisclick to the index of the item that was last clicked */
    thisclick = TestList->numlinesscrolled + gMouseDownY - TestList->rect.ayTop;

    if(!(state&MK_LBUTTON)) /* mouse is not down */ {
	switch(TestList->LastClicked) {
	case DESELFILEONMOUSEUP:
	    TestList->ListProc(tmmDeSelect, NULL, thisclick,
		    TestList->tmc, 0, 0, 0);
	    InsertListItem(TestList, thisclick);
	    break;

	case DESELALLONMOUSEUP:
	    TestList->ListProc(tmmDeselectAll, NULL, 0, TestList->tmc, 0, 0, 0);
	    if(!TestList->ListProc(tmmQuerySelect, NULL, thisclick,
		    TestList->tmc, 0, 0, 0))
		TestList->ListProc(tmmSelect, NULL, thisclick, TestList->tmc,
			TestList->lastmouseX, TestList->lastmouseY, 0);
	    InsertListItem(TestList, thisclick);
	    break;
	}
	TestList->LastClicked = NOFILECLICKED;

	if(TestList->scroll.dragging) {
	    TestList->scroll.dragging = FALSE;
	    DrawScrollBar(TestList, FALSE);
	    return(TRUE);
	} else {
#ifndef NODIRECT
	    TestList->ListProc(tmmDrop, NULL, 0, TestList->tmc, x, y,0);
#endif
	    return(FALSE);
	}
    }

/* Mouse is down */
/* Return TRUE if the scroll bar handles the message */
    if(ScrollBarMouse(TestList, message)) {
	return(TRUE);
    }

#ifndef NODIRECT
/* Send the PickUp message if we have one pending and this is a MOUSEMOVE */
    if(TestList->LastClicked!=NOFILECLICKED && message==WM_MOUSEMOVE) {
	TestList->ListProc(tmmPickUp, NULL, thisclick,
		TestList->tmc, x, y, 0);
/* We only want to send one tmmPickUp message */
	TestList->LastClicked = NOFILECLICKED;
    }
#endif

/* Return FALSE if mouse is not in the listbox */
    if(x<TestList->rect.axLeft || x>TestList->rect.axRight ||
	    y<TestList->rect.ayTop-1 || y>=TestList->rect.ayBottom+!gisgraph) {
	return(FALSE);
    }

/* Return TRUE if mouse is in the title bar or the bottom border */
    if(y<TestList->rect.ayTop ||
	    y>=TestList->rect.ayBottom) {
	return(TRUE);
    }

    switch(message) {
    case WM_MOUSEMOVE:
#ifndef NODIRECT
/* Set thisclick to the index of the item under the cursor */
	thisclick = TestList->numlinesscrolled + y - TestList->rect.ayTop;

		  /* If the mouse moved within the range of the contents of the
			* listbox, change focus, else just ignore it. This test is used
			* to take care of the problem when the user clicks at the empty
			* region of a partially filled listbox and mouse the mouse to
			* another blank spot in it. Previously, the focus used to move
			* to the last item and IBM complained about it. This could slow
			* down stuff on mouse moves!
			*/
	if(thisclick < GetNumItems(TestList))
			FocusLineChange(TestList, thisclick-TestList->focusitem);
#endif
	break;

    case WM_LBUTTONDBLCLK:
	if(thisclick >= GetNumItems(TestList))
	    break;

/* Select only the double clicked item, and then activate it */
	TestList->ListProc(tmmDeselectAll, NULL, 0, TestList->tmc, 0, 0, 0);
	TestList->ListProc(tmmSelect, NULL, thisclick, TestList->tmc, x, y, 0);
	FocusLineChange(TestList, thisclick-TestList->focusitem);
	DoUpdateListBox(TestList, TestList->numlinesonscreen);
	TestList->ListProc(tmmActivate, NULL, thisclick, TestList->tmc, x, y,0);

/* You can't drag after double-clicking */
	TestList->LastClicked = NOFILECLICKED;
	break;

    case WM_LBUTTONDOWN:
	if(thisclick >= GetNumItems(TestList))
	    break;

/* default is to do nothing on the corresponding mouse up */
	TestList->LastClicked = NOACTIONONMOUSEUP;

	if(state&MK_SHIFT) {
/* shift selects a contiguous range of items */
	    if(!(state&MK_CONTROL))
		TestList->ListProc(tmmDeselectAll, NULL, 0,
			TestList->tmc, 0, 0, 0);

/* select from the "anchor point" to here */
	    SelectRange(TestList, thisclick);
	} else {
	    if(state&MK_CONTROL) {
		if(TestList->ListProc(tmmQuerySelect, NULL, thisclick,
			TestList->tmc, 0, 0, 0))
		    TestList->LastClicked = DESELFILEONMOUSEUP;
	    } else {
		if(TestList->ListProc(tmmQuerySelect, NULL, thisclick,
			TestList->tmc, 0, 0, 0))
		    TestList->LastClicked = DESELALLONMOUSEUP;
		else
		    TestList->ListProc(tmmDeselectAll, NULL, 0,
			    TestList->tmc, 0, 0, 0);
	    }

/* clicked line always gets selected; if control was down and it was
 * previously selected, it will get deselected when the mouse comes up.
 */
	    FocusLineChange(TestList, thisclick-TestList->focusitem);
	    TestList->ListProc(tmmSelect, NULL, thisclick,
		    TestList->tmc, x, y, 0);

	    Set_Anchor(TestList, thisclick);
	    InsertListItem(TestList, thisclick);
	}
	break;

    // case WM_MOUSEIDLE:
    default:
	break;
    }

    return(TRUE);
}


/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
VOID InsertListItem(ListBoxData *TestList,WORD isz)
{
	 int numitems;

    if (TestList->halted)
	 return;
    if (isz < TestList->numlinesscrolled)
    {
		TestList->nextlinetoupdate = 0;
    }
    else
       TestList->nextlinetoupdate = min(TestList->nextlinetoupdate,isz-TestList->numlinesscrolled);

	 if (TestList->focusitem >= (numitems=GetNumItems(TestList)))
		TestList->focusitem = max(0,numitems-1);
    TestList->scroll.doupdate = TRUE;
    TestList->update = TRUE;

}

/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
VOID ListKey(ListBoxData *TestList, WORD key, WORD state)
{
	static char typed[20];

	TMM tmmselectmode;
	int nextfocus;
	char itemstring[256], upperKey;
	int i, j, lastitem, enditem;

	if(TestList->halted)
		return;

	switch((key)) {
	case VK_UP:
		nextfocus = NextFocusLineChange(TestList, -1);
		goto SingleLineChange;

	case VK_DOWN:
		nextfocus = NextFocusLineChange(TestList, 1);
		goto SingleLineChange;

	case VK_PRIOR:
		nextfocus = NextFocusLineChange(TestList, -TestList->numlinesonscreen);
		goto SingleLineChange;

	case VK_NEXT:
		nextfocus = NextFocusLineChange(TestList, TestList->numlinesonscreen);
		goto SingleLineChange;

	case VK_HOME:
		nextfocus = NextFocusLineChange(TestList, -TestList->focusitem);
		goto SingleLineChange;

	case VK_END:
		nextfocus = NextFocusLineChange(TestList,
				GetNumItems(TestList)-TestList->focusitem);

SingleLineChange:
		if(TestList->mode)  /*implicit */
			TestList->ListProc(tmmDeselectAll, NULL, 0,
					TestList->tmc, 0, 0, 0);

		if(state&KK_SHIFT) {
/* select from the "anchor point" to here */
			SelectRange(TestList, nextfocus);
		} else if(TestList->mode) {
/* selected line now has "selected" emphasis too */
			TestList->ListProc(tmmSelect, NULL, nextfocus,
					TestList->tmc, 0, TestList->rect.ayTop+nextfocus
					-TestList->numlinesscrolled, 0);
			Set_Anchor(TestList, nextfocus);
		}
		FocusLineChange(TestList, nextfocus-TestList->focusitem);
		break;

	case ' ':
		if(!(state&KK_CONTROL) && (TestList->mode)) {
			tmmselectmode = tmmSelect;
			TestList->ListProc(tmmDeselectAll, NULL, 0,
					TestList->tmc, 0, 0, 0);
		} else {
			tmmselectmode = tmmToggleSelect;
			InsertListItem(TestList, 0);
		}

		if(state&KK_SHIFT) {
			SelectRange(TestList, TestList->focusitem);
		} else {
			TestList->ListProc(tmmselectmode, NULL, TestList->focusitem,
					TestList->tmc, 0, TestList->rect.ayTop+TestList->focusitem
					-TestList->numlinesscrolled, 0);
			Set_Anchor(TestList,TestList->focusitem);
		}
		break;

	case 13:
		TestList->ListProc(tmmActivate, NULL, TestList->focusitem,
				TestList->tmc, 0, TestList->rect.ayTop+TestList->focusitem
				-TestList->numlinesscrolled, 0);
		break;

	case VK_F8:
		if(state&KK_SHIFT && !TestList->alwaysexplicit) {
			TestList->mode = !TestList->mode;
			TestList->ListProc(TestList->mode ? tmmImplicit : tmmExplicit,
					NULL, 0, TestList->tmc, 0, 0, 0);
		}
		break;

	default:
		if(key>' ' && key<256) {
			if((lastitem = GetNumItems(TestList) - 1) < 0)
				break;

			if(!gListKeyDelay || ScrollPause(gListKeyDelay))
				i = 0;
			else if((i=strlen(typed)) >= sizeof(typed)-1)
				break;
			typed[i  ] = upperKey = (BYTE)toupper(key);
			typed[i+1] = '\0';

			nextfocus = -1;
			enditem = TestList->focusitem;

			if(i) {
				i = enditem;
				TestList->ListProc(tmmGetItemString, itemstring, i,
						TestList->tmc, 0, 0, 0);
				goto SkipFirstTest;
			} else {
				i = enditem>=lastitem ? 0 : enditem+1;
			}

			for( ; i!=enditem; (i>=lastitem ? i=0 : ++i)) {
				itemstring[0]=0;
				TestList->ListProc(tmmGetItemString, itemstring, i,
						TestList->tmc, 0, 0, 0);
				if(itemstring[0] == 0)
					break;
				if(toupper(itemstring[0])==upperKey && nextfocus<0)
					nextfocus = i;
SkipFirstTest:
				for(j=0; typed[j]; ++j)
					if(toupper(itemstring[j])!=typed[j])
						break;
				if(!typed[j]) {
					nextfocus = i;
					goto SingleLineChange;
				}
			}

			if(nextfocus >= 0) {
				typed[0] = upperKey;
				typed[1] = '\0';
				goto SingleLineChange;
			} else
				typed[0] = '\0';
		}
	}
}

extern BOOL IsMenuDown(void);

VOID UpdateListBox(ListBoxData *TestList)
{
    if (TestList->halted)
		return;
	 if(IsMenuDown())
		return; // do nothing now; since .inited is not set, will happen later

      FEnableMouseNest(FALSE);
      TestList->scroll.inited = FALSE;
      FrameListBox(TestList);
      FEnableMouseNest(TRUE);
      InsertListItem(TestList,0);
}



/*  NAME
 *  DESCRIPTION
 *  ARGUMENTS
 *  RETURN VALUE
 *      TRUE if really idle, FALSE otherwise
 *  EFFECTS
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
    MSG     pmsg;
BOOL ListBoxIdle(ListBoxData *TestList)
{
    int     i;
    int     numlinestodraw;
    int     message;
    BOOL    amidle=TRUE;

	 /* if a menu is down, we don't do stuff. This is because
     * graphics cannot clip to the menu, so graphics users will
     * trompt on the menu
     */
    if ((TestList->halted) || (IsMenuDown()))
		return(amidle);/* idle */

    if (!TestList->inited)
    {
      UpdateListBox(TestList);
      TestList->inited = TRUE;
      TestList->drawahead= -1;
      TestList->drawbehind = -1;
      amidle = FALSE;
    }

    if(TestList->lastmousestate&MK_LBUTTON) /* mouse is down */
			ListMouse(TestList,TestList->lastmouseX,TestList->lastmouseY,
				WM_MOUSEIDLE, TestList->lastmousestate);
    //DoUpdateListBox(TestList,TestList->numlinesonscreen);
    if(TestList->drawahead != -1)
    {
		TestList->drawbehind = TestList->numlinesscrolled;
		TestList->nextlinetoupdate = TestList->drawahead;
		amidle = FALSE;
    }
    else
    {
		if(TestList->drawbehind!=-1)
		{
		InsertListItem(TestList,0);
		amidle = FALSE;
		}
		TestList->drawbehind = -1;
    }
    i = TestList->numlinesonscreen;
		numlinestodraw = i/4;

	/* ZZZZZZZZ Scott, make sure that I need to put in following check!! */
	/* Actually value shouldn't be < 0, but the following <= check is safe! */
	if (numlinestodraw <= 0)
		numlinestodraw = 1 ; /* Will guarantee termination of foll. loop! */
    /*
     * if they aren't dragging, just update the who thing, otherwise
     * do it in bits, so we break out for a scroll
     */
    if (!TestList->scroll.dragging)
    {
		DoUpdateListBox(TestList,i);
		i=0;
    }
    else
    while(i > 0)
    {
		DoUpdateListBox(TestList,numlinestodraw);
		i-=numlinestodraw;
		if( /* (TestList->scroll.dragging) && */ PeekMessage(&pmsg))
		{
		message = pmsg.message;
		UngetMessage(&pmsg);
		if((message == WM_MOUSEMOVE) || (message == WM_CHAR))
		{
		  break;
		}
		amidle = FALSE;
		}
    }
    if(i>0)
    {
       TestList->drawahead = TestList->nextlinetoupdate;
    }
    else
       TestList->drawahead = -1;
    return(amidle);
}
