;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/** This module enhances the CW graphics capabilities
**/

#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>

VOID  MySetColor(WORD color);
VOID  MySetColor(WORD color)
{
#define NUMCOLORCACHE 10

    static WORD lastcolor[NUMCOLORCACHE] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
    static WORD pcov[NUMCOLORCACHE];
    static WORD rgcoi[3];
    static WORD rgbval[NUMCOLORCACHE];
    static WORD nexttrash = 0;
    int i;

    /* FGetColorPalette is so slow we don't want to do it very often! */
    for(i=0;i<NUMCOLORCACHE;i++)
    {
	if (lastcolor[i] == color)
	{
	  SetColor(pcov[i],rgbval[i]);
	  return;
	}
    }
    /* If we got here, the color is not chached, so
     * we have to trash a color in the cache
     */
    nexttrash = (nexttrash+1)%NUMCOLORCACHE;
       FGetColorPalette(color,&pcov[nexttrash],rgcoi);
       rgbval[nexttrash] = (((((WORD)rgcoi[0]) << 10)&0x7C00) | ((((WORD)rgcoi[1])<<5)&0x3E0) | (rgcoi[2]&0x1F));
       pcov[nexttrash] = color;
       lastcolor[nexttrash] = color;
    SetColor(pcov[nexttrash],rgbval[nexttrash]);
}

/*
 * What PlotBmp Does:
 *  To reduce flashing on the screen (given that there is no srccopy mode
 *  in BitBlt), we erase the pixels which should not be set(black) by blting
 *  the complement of the bitmap through the background color. Then we
 *  blt the actual bitmap to the screen. Thus the fewest number of pixels
 *  *change their color* on the screen.
 *  This is of course an ugly slimy kludge from the nether world since the
 *  drivers should support a srccopy mode.
 *
 */
VOID PlotBmp(BITMAP *bmp,WORD x,WORD y,ISA color)
{
    WORD fore,back;
    GetIsaColor(color,&fore,&back);
    MySetColor(fore);
    Move(x,y);
    FEnableMouseNest(FALSE) ;
    TextOut(&MainWind, (RX) (x/CWIDTH), (RY) (y/CHEIGHT), "    ",bmp->cbRow,color);
    BitBlt(&bmp->rectBound,bmp->lrgb,bmp->cbRow,FALSE);
    FEnableMouseNest(TRUE) ;
}
#if 0
VOID GetBmp(BITMAP *bmp,WORD x,WORD y)
{
	Move(x,y);
	SetColor(0,0x7FFF);
	SetAreaPat(0);
	SetLinePat(1);
	BitRead(&bmp->rectBound,bmp->lrgb,bmp->cbRow);
}
#endif


VOID FrameCharRect(WORD top, WORD left, WORD bottom, WORD right, WORD linepat,ISA color)
{
    RECT lRect;
    WORD fore,back;

    if (gisgraph)
    {
	 lRect.xLeft = left * CWIDTH;
	 lRect.yTop  = top * CHEIGHT;
	 lRect.xRight = right * CWIDTH;
	 lRect.yBottom = bottom *CHEIGHT;

	 FEnableMouseNest(FALSE) ;
	 GetIsaColor(color,&fore,&back);
	 MySetColor(fore);

	 SetAreaPat(0); // Set clear area == 0
	 SetLinePat(linepat);

	 Rectangle(&lRect);
	 FEnableMouseNest(TRUE) ;
    }
}

VOID FrameDialog(WORD top, WORD left, WORD bottom, WORD right)
{
    RECT lRect;
    WORD fore,back;

    lRect.xLeft = left * CWIDTH;
    lRect.yTop	= top * CHEIGHT; /*-3;*/
    lRect.xRight = right * CWIDTH;
    lRect.yBottom = bottom *CHEIGHT;

    FEnableMouseNest(FALSE);

    GetIsaColor(isaBorders,&fore,&back);
    MySetColor(fore);

    SetAreaPat(0);
    SetLinePat(1);

    Rectangle(&lRect);
    lRect.xLeft  += 2;
    lRect.yTop	 += 2;
    lRect.xRight -= 2;
    lRect.yBottom -= 2;
    Rectangle(&lRect);
    lRect.xLeft  += 1;
    lRect.yTop	 += 1;
    lRect.xRight -= 1;
    lRect.yBottom -= 1;
    Rectangle(&lRect);

    FEnableMouseNest(TRUE);
}

VOID FrameButton(WORD top, WORD left, WORD bottom, WORD right)
{
    RECT lRect;
    int i;
    int round;
    WORD fore,back;
    if(!gisgraph)
	return;
    GetIsaColor(isaPushButton,&fore,&back);
    MySetColor(back);
    SetAreaPat(0);
    SetLinePat(1);


     lRect.xLeft = left * CWIDTH     + CWIDTH;
     lRect.yTop  = top * CHEIGHT     ;
     lRect.xRight = right * CWIDTH   - CWIDTH;
     lRect.yBottom = bottom *CHEIGHT ;

     FEnableMouseNest(FALSE);
     round = 1;
     for(i=0;i<(CHEIGHT+1)/2;i++)
     {
	MySetColor(back);

	Move(lRect.xLeft + CWIDTH,lRect.yTop+i);
	Draw(lRect.xLeft-round+1,lRect.yTop+i);

	Move(lRect.xRight - CWIDTH,lRect.yTop+i);
	Draw(lRect.xRight+round-1,lRect.yTop+i);

	Move(lRect.xLeft + CWIDTH,lRect.yTop+CHEIGHT-i-1);
	Draw(lRect.xLeft-round+1,lRect.yTop+CHEIGHT-i-1);

	Move(lRect.xRight - CWIDTH,lRect.yTop+CHEIGHT-i-1);
	Draw(lRect.xRight+round-1,lRect.yTop+CHEIGHT-i-1);
	if(i == 0)
	{
	    round+=2;
	}
	if(i == 1)
	    ++round;
	if(i == 3)
	    ++round;

     }
     round = 1;
     for(i=0;i<(CHEIGHT+1)/2;i++)
     {

	MySetColor(fore);
	Move(lRect.xLeft-round,lRect.yTop+i);
	Draw(lRect.xLeft-round,lRect.yTop+i);
	Move(lRect.xRight+round,lRect.yTop+i);
	Draw(lRect.xRight+round,lRect.yTop+i);

	Move(lRect.xLeft-round,lRect.yTop+CHEIGHT-i-1);
	Draw(lRect.xLeft-round,lRect.yTop+CHEIGHT-i-1);
	Move(lRect.xRight+round,lRect.yTop+CHEIGHT-i-1);
	Draw(lRect.xRight+round,lRect.yTop+CHEIGHT-i-1);
	if(i == 0)
	{
	    Move(lRect.xLeft-round-1,lRect.yTop+i);
	    Draw(lRect.xLeft-round-1,lRect.yTop+i);
	    Move(lRect.xRight+round+1,lRect.yTop+i);
	    Draw(lRect.xRight+round+1,lRect.yTop+i);

	    Move(lRect.xLeft-round-1,lRect.yTop+CHEIGHT-i-1);
	    Draw(lRect.xLeft-round-1,lRect.yTop+CHEIGHT-i-1);
	    Move(lRect.xRight+round+1,lRect.yTop+CHEIGHT-i-1);
		Draw(lRect.xRight+round+1,lRect.yTop+CHEIGHT-i-1);

	    round+=2;
	}
	if(i == 1)
	    ++round;
	if(i == 3)
	    ++round;
	 }
	 /* BUG BUG if you don't set the color here, buttons look like
	  * donkey dicks on hercules graphics.
	  * We don't know why exactly, but there they are!
	  */
	 if(fMonochrome)
		SetColor(0,0x7FFF);

	 Move(lRect.xLeft, lRect.yTop-1);
	 Draw(lRect.xRight, lRect.yTop-1);
	 Move(lRect.xLeft, lRect.yTop+CHEIGHT);
	 Draw(lRect.xRight, lRect.yTop+CHEIGHT);

	 FEnableMouseNest(TRUE);
}


VOID FrameCharRectInset(WORD top, WORD left, WORD bottom, WORD right,WORD inx,WORD iny,ISA color)
{
    RECT lRect;
    WORD fore,back;
#define SCREENMINX 0
#define SCREENMINY 0
#define SCREENMAXX (axMac * CWIDTH)
#define SCREENMAXY (ayMac * CHEIGHT)

    lRect.xLeft =  (left * CWIDTH)-inx;
    if (lRect.xLeft < SCREENMINX)
	lRect.xLeft = SCREENMINX;
    if (lRect.xLeft > SCREENMAXX)
	lRect.xLeft = SCREENMAXX;

    lRect.yTop	=  (top * CHEIGHT)-iny;
    if (lRect.yTop < SCREENMINY)
	lRect.yTop = SCREENMINY;
    if (lRect.yTop > SCREENMAXY)
	lRect.yTop = SCREENMAXY;

    lRect.xRight = (right * CWIDTH)+inx;
    if (lRect.xRight < SCREENMINX)
	lRect.xRight = SCREENMINX;
    if (lRect.xRight > SCREENMAXX)
	lRect.xRight = SCREENMAXX;

    lRect.yBottom =(bottom *CHEIGHT)+iny;
    if (lRect.yBottom < SCREENMINY)
	lRect.yBottom = SCREENMINY;
    if (lRect.yBottom > SCREENMAXY)
	lRect.yBottom = SCREENMAXY;


    SetAreaPat(0);
    SetLinePat(1);
    GetIsaColor(color,&fore,&back);
	MySetColor(fore);
    Rectangle(&lRect);

}

void EasyDrawBox(PWND pwd, BYTE top, BYTE left, BYTE bottom, BYTE right,ISA isa)
{
    RRC trect;
    BOX  tboxinfo;

    tboxinfo.chTopLeftCorner	  = cinch._chTopLeftCorner1;
    tboxinfo.chTopRightCorner	  = cinch._chTopRightCorner1;
    tboxinfo.chBottomLeftCorner   = cinch._chBottomLeftCorner1;
    tboxinfo.chBottomRightCorner  = cinch._chBottomRightCorner1;
    tboxinfo.chTopSide		  = cinch._chTopSide1;
    tboxinfo.chBottomSide	  = cinch._chBottomSide1;
    tboxinfo.chLeftSide 	  = cinch._chLeftSide1;
    tboxinfo.chRightSide	  = cinch._chRightSide1;
    trect.ryTop = top;
    trect.rxLeft = left;
    trect.ryBottom = bottom;
    trect.rxRight = right;
    FEnableMouseNest(FALSE) ;
    DrawBox(pwd, &trect, &tboxinfo, isa);
    FEnableMouseNest(TRUE) ;
} /* EasyDrawBox */

/* draws a line from (x1, y1) to (x2, y2) */
void EasyDrawLine(BYTE x1, BYTE y1, BYTE x2, BYTE y2, BOOL fdec)
{
    AX ty1, ty2 ;

    FEnableMouseNest(FALSE) ;
    /* ZZZZZ Should this nonsense be done each time we do a draw */
    SetAreaPat(0);
    SetLinePat(1);
    SetColor(0,0x7FFF);
    ty1 = y1*CHEIGHT ;
    ty2 = y2*CHEIGHT ;
    if (fdec)
    {
	ty1-- ; ty2-- ;
    }
    Move(x1*CWIDTH, ty1) ;
    Draw(x2*CWIDTH, ty2) ;
    FEnableMouseNest(TRUE) ;
} /* EasyDrawLine */
