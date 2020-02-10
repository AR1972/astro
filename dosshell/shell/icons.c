;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****	icons.c - contains all the icons
**
*/
#include <common.h>
#include <icons.h>
#include <menus.h>
#include <filemgr.h>
#include <prot.h>


char bigRadioButtonUnSelData[] =
{
/*		   3210321032103210 */
	0x03,0xC0,	/*		 XXXX		*/
    0x0C,0x30,	/*     XX    XX     */
    0x30,0x0C,	/*   XX        XX   */
	0x20,0x04,	/*	 X			X	*/
	0x40,0x02,	/*	X			  X */
	0x40,0x02,	/*	X			  X */
	0x40,0x02,	/*	X			  X */
	0x40,0x02,	/*	X			  X */
	0x20,0x04,	/*	 X			  X	*/
    0x30,0x0C,	/*   XX        XX   */
    0x0C,0x30,	/*     XX    XX     */
	0x03,0xC0  /*		 XXXX		*/
};

char bigRadioButtonSelData[] =
{
/*		   3210321032103210 */
	0x03,0xC0,	/*		 XXXX		*/
    0x0C,0x30,	/*     XX    XX     */
    0x30,0x0C,	/*   XX        XX   */
	0x27,0xE4,	/*	 X	 XXXXXX	 X	*/
	0x4F,0xF2,	/*	X	XXXXXXXX  X */
	0x4F,0xF2,	 /* X	XXXXXXXX  X */
	0x4F,0xF2,	 /* X	XXXXXXXX  X */
	0x4F,0xF2,	 /* X	XXXXXXXX  X */
	0x27,0xE4,	/*	 X	 XXXXXX	 X	*/
    0x30,0x0C,	/*   XX        XX   */
    0x0C,0x30,	/*     XX    XX     */
	0x03,0xC0  /*		 XXXX	   */
};


char bigUpArrowData[] =
{
    0x00,0x00,
    0x00,0x00,
    0x00,0x00,
    0x01,0x80,	/*	   XX		*/
    0x03,0xC0,	/*	  XXXX		*/
    0x07,0xE0,	/*	 XXXXXX 	*/
    0x0F,0xF0,	/*	XXXXXXXX	*/
	0x1F,0xF8,	/* XXXXXXXXXX	*/
    0x03,0xC0,	/*	  XXXX		*/
    0x03,0xC0,	/*	  XXXX		*/
    0x03,0xC0,	/*	  XXXX		*/
    0x03,0xC0,	/*	  XXXX		*/
    0x03,0xC0	/*	  XXXX		*/
};

char bigDownArrowData[] =
{
    0x00,0x00,
    0x00,0x00,
    0x00,0x00,
    0x03,0xC0,	/*	  XXXX		*/
    0x03,0xC0,	/*	  XXXX		*/
    0x03,0xC0,	/*	  XXXX		*/
    0x03,0xC0,	/*	  XXXX		*/
    0x03,0xC0,	/*	  XXXX		*/
	0x1F,0xF8,	/* XXXXXXXXXX	*/
    0x0F,0xF0,	/*	XXXXXXXX	*/
    0x07,0xE0,	/*	 XXXXXX 	*/
    0x03,0xC0,	/*	  XXXX		*/
    0x01,0x80,	/*	   XX		*/
};


char bigProgramIconData[] =
{						/*	 000011112222333344445555	*/
	0x00,0x00,0x00,
	0x7f,0xff,0xf0,		/*	  xxxxxxxxxxxxxxxxxxx	*/
	0x40,0x00,0x10,		/*	  x 				     x	*/
	0x40,0x00,0x10,		/*	  x 				     x	*/
	0x7f,0xff,0xf0, 	   /*	  xxxxxxxxxxxxxxxxxxx	*/
	0x40,0x00,0x10,		/*	  x 				     x	*/
	0x40,0x00,0x10, 	   /*	  x                 x	*/
	0x40,0x00,0x10,		/*	  x   	 	  	     x	*/
	0x40,0x00,0x10, 	   /*	  x                 x	*/
	0x40,0x00,0x10, 	   /*	  x   	 	  	     x	*/
	0x40,0x00,0x10, 	   /*	  x                 x	*/
	0x40,0x00,0x10, 	   /*	  x 				     x	*/
	0x7f,0xff,0xf0,		/*	  xxxxxxxxxxxxxxxxxxx	*/

};

char bigProgramIconDataInvert[] =
{						/*	 000011112222333344445555	*/
	0xff,0xff,0xf8,		/*	 xxxxxxxxxxxxxxxxxxxxx		*/
	0x80,0x00,0x08,		/*	 x					      x		*/
	0xbf,0xff,0xe8,		/*	 x xxxxxxxxxxxxxxxxx x		*/
	0xbf,0xff,0xe8,		/*	 x xxxxxxxxxxxxxxxxx x		*/
	0x80,0x00,0x08, 	   /*	 x					      x		*/
	0xbf,0xff,0xe8,		/*	 x xxxxxxxxxxxxxxxxx x		*/
	0xbf,0xff,0xe8, 	   /*	 x xxxxxxxxxxxxxxxxx x     */
	0xbf,0xff,0xe8,		/*	 x xxxxxxxxxxxxxxxxx x		*/
	0xbf,0xff,0xe8, 	   /*	 x xxxxxxxxxxxxxxxxx x     */
	0xbf,0xff,0xe8, 	   /*	 x xxxxxxxxxxxxxxxxx x		*/
	0xbf,0xff,0xe8, 	   /*	 x xxxxxxxxxxxxxxxxx x     */
	0xbf,0xff,0xe8,		/*	 x xxxxxxxxxxxxxxxxx x		*/
	0x80,0x00,0x08,		/*	 x					      x		*/
	0xff,0xff,0xf8, 	   /*	 xxxxxxxxxxxxxxxxxxxxx		*/

};


char bigProgManIconData[] =
{						/*	 000011112222333344445555	*/
	0x00,0x00,0x00,
	0x7f,0xff,0xf0,		/*	  xxxxxxxxxxxxxxxxxxx	*/
	0x40,0x00,0x10,		/*	  x 				x	*/
	0x40,0x00,0x10,		/*	  x 				x	*/
	0x7f,0xff,0xf0, 	/*	  xxxxxxxxxxxxxxxxxxx	*/
	0x40,0x00,0x10,		/*	  x 				x	*/
	0x4f,0xff,0x90, 	/*	  x  xxxxxxxxxxxxx	x	*/
	0x48,0x88,0x90,		/*	  x  x	 x	 x	 x	x	*/
	0x4f,0xff,0x90, 	/*	  x  xxxxxxxxxxxxx	x	*/
	0x48,0x88,0x90, 	/*	  x  x	 x	 x	 x	x	*/
	0x4f,0xff,0x90, 	/*	  x  xxxxxxxxxxxxx	x	*/
	0x40,0x00,0x10, 	/*	  x 				x	*/
	0x7f,0xff,0xf0,		/*	  xxxxxxxxxxxxxxxxxxx	*/

};

char bigProgManIconInvertData[] =
{						/*	 000011112222333344445555	*/
	0xff,0xff,0xf8,		/*	 xxxxxxxxxxxxxxxxxxxxx		*/
	0x80,0x00,0x08,		/*	 x					 x		*/
	0xbf,0xff,0xe8,		/*	 x xxxxxxxxxxxxxxxxx x		*/
	0xbf,0xff,0xe8,		/*	 x xxxxxxxxxxxxxxxxx x		*/
	0x80,0x00,0x08, 	/*	 x					 x		*/
	0xbf,0xff,0xe8,		/*	 x xxxxxxxxxxxxxxxxx x		*/
	0xb0,0x00,0x68, 	/*	 x xx			  xx x		*/
	0xb7,0x77,0x68,		/*	 x xx xxx xxx xxx xx x		*/
	0xb0,0x00,0x68, 	/*	 x xx			  xx x		*/
	0xb7,0x77,0x68, 	/*	 x xx xxx xxx xxx xx x		*/
	0xb0,0x00,0x68, 	/*	 x xx			  xx x		*/
	0xbf,0xff,0xe8,		/*	 x xxxxxxxxxxxxxxxxx x		*/
	0x80,0x00,0x08,		/*	 x					 x		*/
	0xff,0xff,0xf8, 	/*	 xxxxxxxxxxxxxxxxxxxxx		*/

};



char smallProgManIconData[] =
{						/*	 000011112222333344445555	*/
	0x7f,0xff,0xf0,		/*	  xxxxxxxxxxxxxxxxxxx	*/
	0x40,0x00,0x10,		/*	  x 				x	*/
	0x4f,0xff,0x90, 	/*	  x  xxxxxxxxxxxxx	x	*/
	0x48,0x88,0x90, 	/*	  x  x	 x	 x	 x	x	*/
	0x48,0x88,0x90,		/*	  x  x	 x	 x	 x	x	*/
	0x4f,0xff,0x90, 	/*	  x  xxxxxxxxxxxxx	x	*/
	0x40,0x00,0x10, 	/*	  x 				x	*/
	0x7f,0xff,0xf0,		/*	  xxxxxxxxxxxxxxxxxxx	*/

};

char smallProgManIconInvertData[] =
{						/*	 000011112222333344445555	*/

	0x80,0x00,0x08, 	/*	 x					 x		*/
	0xbf,0xff,0xe8,		/*	 x xxxxxxxxxxxxxxxxx x		*/
	0xb0,0x00,0x68, 	/*	 x xx			  xx x		*/
	0xb7,0x77,0x68,		/*	 x xx xxx xxx xxx xx x		*/
	0xb7,0x77,0x68, 	/*	 x xx xxx xxx xxx xx x		*/
	0xb0,0x00,0x68, 	/*	 x xx			  xx x		*/
	0xbf,0xff,0xe8,		/*	 x xxxxxxxxxxxxxxxxx x		*/
	0x80,0x00,0x08,		/*	 x					 x		*/


};




char bigFileIconData[] =	   /*	000011114444555566667777   */
{
    0x00,0x00,0x00,			/*   12345678123412345678  */
    0x7F,0xFC,0x00,			/*    XXXXXXXXXXXXX	   */
    0x40,0x06,0x00,			/*    X 	  XX	   */
    0x5E,0x65,0x00,			/*    X XXXX  XX  X X	   */
    0x40,0x04,0x80,			/*    X 	  X  X	   */
    0x59,0xC4,0x40,			/*    X XX  XXX   X   X    */
    0x40,0x07,0xE0,			/*    X 	  XXXXXX   */
    0x5E,0x70,0x20,			/*    X XXXX  XXX      X   */
    0x40,0x00,0x20,			/*    X 	       X   */
    0x40,0x00,0x20,			/*    X 	       X   */
    0x40,0x00,0x20,			/*    X 	       X   */
    0x7F,0xFF,0xE0,			/*    XXXXXXXXXXXXXXXXXX   */
    0x00,0x00,0x00
};

char bigFileIconDataInvert[] =	   /*	000011114444555566667777   */
{					/*   12345678123412345678   */
    0xff,0xfc,0x00,			/*   xxxxxxxxxxxxxx	    */
    0x80,0x02,0x00,			/*   x		   x	    */
    0xbf,0xf9,0x00,			/*   x xxxxxxxxxxx  x	    */
    0xA1,0x9a,0x80,			/*   x x    xx	xx x x	    */
    0xbf,0xfb,0x40,			/*   x xxxxxxxxxxx xx x     */
    0xA6,0x3b,0xa0,			/*   x x  xx   xxx xxx x    */
    0xbf,0xf8,0x10,			/*   x xxxxxxxxxxx	x   */
    0xA1,0x8f,0xd0,			/*   x x    xx	 xxxxxx x   */
    0xbf,0xff,0xd0,			/*   x xxxxxxxxxxxxxxxx x   */
    0xbf,0xff,0xd0,			/*   x xxxxxxxxxxxxxxxx x   */
    0xbf,0xff,0xd0,			/*   x xxxxxxxxxxxxxxxx x   */
    0x80,0x00,0x10,			/*   x			x   */
    0xff,0xff,0xf0			/*   xxxxxxxxxxxxxxxxxxxx   */
};

#ifdef USEOLDPILLICON
char bigProgramIconDataInvert[]= // 000011112222333344445555
{					//    12345678123412345678
	0x1f,0xff,0x80, 		//	 xxxxxxxxxxxxxx
	0x20,0x00,0x40, 		//	x	       x
	0x5f,0xff,0xA0, 		//     x xxxxxxxxxxxxxx x
	0xbf,0xff,0xd0, 		//    x xxxxxxxxxxxxxxxx x
	0xb0,0x00,0xd0, 		//    x xx	      xx x
	0xbf,0xff,0xd0, 		//    x xxxxxxxxxxxxxxxx x
	0xb0,0x03,0xd0, 		//    x xx	    xxxx x
	0xbf,0xff,0xd0, 		//    x xxxxxxxxxxxxxxxx x
	0xb0,0x00,0xd0, 		//    x xx	      xx x
	0xbf,0xff,0xd0, 		//    x xxxxxxxxxxxxxxxx x
	0x5f,0xff,0xA0, 		//     x xxxxxxxxxxxxxx x
	0x20,0x00,0x40, 		//	x	       x
	0x1f,0xff,0x80, 		//	 xxxxxxxxxxxxxx
};

char bigProgramIconData[] =	   //	000011112222333344445555
{
	0x00,0x00,0x00, 		//    12345678123412345678
	0x1f,0xff,0x80, 		//	 xxxxxxxxxxxxxx
	0x20,0x00,0x40, 		//	x	       x
	0x40,0x00,0x20, 		//     x		x
	0x4f,0xff,0x20, 		//     x  xxxxxxxxxxxx	x
	0x40,0x00,0x20, 		//     x		x
	0x4f,0xfC,0x20, 		//     x  xxxxxxxxxx	x
	0x40,0x00,0x20, 		//     x		x
	0x4f,0xff,0x20, 		//     x  xxxxxxxxxxxx	x
	0x40,0x00,0x20, 		//     x		x
	0x20,0x00,0x40, 		//	x	       x
	0x1f,0xff,0x80, 		//	 xxxxxxxxxxxxxx
	0x00,0x00,0x00
};
#endif	
char bigDirExpansionIconData[] =
{					//	00001111222244445555
	0x00,0x00,0x00, 		//
	0x1E,0x00,0x00, 		//	   xxxx
	0x21,0x00,0x00, 		//	  x    x
	0x40,0xff,0x80, 		//	 x	xxxxxxxxx
	0x7f,0x00,0x40, 		//	 xxxxxxx	 x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x20,0x20, 		//	 x	  x	  x
	0x40,0x20,0x20, 		//	 x	  x	  x
	0x40,0xf8,0x20, 		//	 x	xxxxx	  x
	0x40,0x20,0x20, 		//	 x	  x	  x
	0x40,0x20,0x20, 		//	 x	  x	  x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x7f,0xff,0xe0, 		//	 xxxxxxxxxxxxxxxxxx
	0x00,0x00,0x00,
	0x00,0x00,0x00
};
	
char bigDirCollapseIconData[] =
{
					//	00001111222244445555
	0x00,0x00,0x00, 		//
	0x1E,0x00,0x00, 		//	   xxxx
	0x21,0x00,0x00, 		//	  x    x
	0x40,0xff,0x80, 		//	 x	xxxxxxxxx
	0x7f,0x00,0x40, 		//	 xxxxxxx	 x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0xF8,0x20, 		//	 x	xxxxx	  x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x7f,0xff,0xe0, 		//	 xxxxxxxxxxxxxxxxxx
	0x00,0x00,0x00,
	0x00,0x00,0x00
};

char bigDirNoZoomIconData[] =
{					//	00001111222244445555
	0x00,0x00,0x00, 		//
	0x1E,0x00,0x00, 		//	   xxxx
	0x21,0x00,0x00, 		//	  x    x
	0x40,0xff,0x80, 		//	 x	xxxxxxxxx
	0x7f,0x00,0x40, 		//	 xxxxxxx	 x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x40,0x00,0x20, 		//	 x		  x
	0x7f,0xff,0xe0, 		//	 xxxxxxxxxxxxxxxxxx
	0x00,0x00,0x00,
	0x00,0x00,0x00
};



char bigFloppyIconData[] =	   /*	000011112222333344445555   */
{
	0x00,0x00,0x00,		/*                              */
	0x7f,0xff,0xfc,		/*    xxxxxxxxxxxxxxxxxxxxx     */
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x41,0xff,0x06,		/*    x     xxxxxxxxx     xx	*/
	0x41,0x01,0x06,		/*    x     x       x     xx	*/
	0x4f,0xff,0xe6,		/*    x  xxxxxxxxxxxxxxx  xx	*/
	0x41,0x01,0x06,		/*    x     x       x     xx	*/
	0x41,0xff,0x06,		/*    x     xxxxxxxxx     xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x7f,0xff,0xfe,		/*    xxxxxxxxxxxxxxxxxxxxxx	*/
	0x3f,0xff,0xfe,		/*     xxxxxxxxxxxxxxxxxxxxx	*/
};

char bigFloppyIconDataInvert[] =   /*	000011112222333344445555   */
{
	0xff,0xff,0xfe,		/*   xxxxxxxxxxxxxxxxxxxxxxx	*/
	0x80,0x00,0x03,		/*   x                     xx	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbe,0x00,0xf9,		/*   x xxxxx         xxxxx  x	*/
	0xbe,0xfe,0xf9,		/*   x xxxxx xxxxxxx xxxxx  x   */
	0xb0,0x00,0x19,		/*   x xx               xx  x	*/
	0xbe,0xfe,0xf9,		/*   x xxxxx xxxxxxx xxxxx  x   */
	0xbe,0x00,0xf9,		/*   x xxxxx         xxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0x80,0x00,0x01,		/*   x                      x	*/
	0xc0,0x00,0x01,		/*   xx                     x   */
	0x7f,0xff,0xff		/*    xxxxxxxxxxxxxxxxxxxxxxx	*/
};

char bigHDIconData[] =		   /*	000011112222333344445555   */
{
	0x00,0x00,0x00,		/*                              */
	0x00,0x00,0x00,		/*                              */
	0x7f,0xff,0xfc,		/*    xxxxxxxxxxxxxxxxxxxxx     */
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x07,0xe6,		/*    x           xxxxxx  xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x7f,0xff,0xfe,		/*    xxxxxxxxxxxxxxxxxxxxxx	*/
	0x3f,0xff,0xfe,		/*     xxxxxxxxxxxxxxxxxxxxx	*/
	0x00,0x00,0x00		/*                              */
};

char bigHDIconDataInvert[] =	   /*	000011112222333344445555   */
{
	0x00,0x00,0x00,		/*                              */
	0xff,0xff,0xfe,		/*   xxxxxxxxxxxxxxxxxxxxxxx	*/
	0x80,0x00,0x03,		/*   x                     xx	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xf8,0x19,		/*   x xxxxxxxxxxx      xx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0x80,0x00,0x01,		/*   x                      x	*/
	0xc0,0x00,0x01,		/*   xx                     x   */
	0x7f,0xff,0xff		/*    xxxxxxxxxxxxxxxxxxxxxxx	*/
};
#if 0
char bigRemoteIconData[] =		   /*	000011112222333344445555   */
{
	0x00,0x00,0x00,		/*                              */
	0x00,0x00,0x00,		/*                              */
	0x7f,0xff,0xfc,		/*    xxxxxxxxxxxxxxxxxxxxx     */
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06, 	/*	  x 				  xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x7f,0xff,0xfe,		/*    xxxxxxxxxxxxxxxxxxxxxx	*/
	0x3f,0xff,0xfe,		/*     xxxxxxxxxxxxxxxxxxxxx	*/
	0x00,0x00,0x00		/*                              */
};

char bigRemoteIconDataInvert[] =	   /*	000011112222333344445555   */
{
	0x00,0x00,0x00,		/*                              */
	0xff,0xff,0xfe,		/*   xxxxxxxxxxxxxxxxxxxxxxx	*/
	0x80,0x00,0x03,		/*   x                     xx	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9, 	/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0xbf,0xff,0xf9,		/*   x xxxxxxxxxxxxxxxxxxx  x	*/
	0x80,0x00,0x01,		/*   x                      x	*/
	0xc0,0x00,0x01,		/*   xx                     x   */
	0x7f,0xff,0xff		/*    xxxxxxxxxxxxxxxxxxxxxxx	*/
};

#endif

char bigRemoteIconData[] =
{						/*	 000011112222333344445555	*/
	0x00,0x00,0x00,		/*                              */
	0x7f,0xff,0xfc,		/*    xxxxxxxxxxxxxxxxxxxxx     */
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x51,0x7d,0xf6, 	/*	  x x	x xxxxx	xxxxx xx	*/
	0x59,0x40,0x46,		/*	  x xx	x x		  x	  xx	*/
	0x55,0x78,0x46,		/*	  x x x x xxxx	  x	  xx	*/
	0x53,0x40,0x46,		/*	  x x  xx x		  x	  xx	*/
	0x51,0x7c,0x46,		/*	  x x	x xxxxx	  x	  xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x40,0x00,0x06,		/*    x                   xx	*/
	0x7f,0xff,0xfe,		/*    xxxxxxxxxxxxxxxxxxxxxx	*/
	0x3f,0xff,0xfe,		/*     xxxxxxxxxxxxxxxxxxxxx	*/
};

char bigRemoteIconDataInvert[] =
{						/*	 000011112222333344445555	*/
	0xff,0xff,0xfe,		/*	 xxxxxxxxxxxxxxxxxxxxxxx	*/
	0x80,0x00,0x03,		/*	 x					   xx	*/
	0xbf,0xff,0xf9,		/*	 x xxxxxxxxxxxxxxxxxxx	x	*/
	0xbf,0xff,0xf9,		/*	 x xxxxxxxxxxxxxxxxxxx	x	*/
	0xae,0x82,0x09, 	/*	 x x xxx x	   x	 x	x	*/
	0xa6,0xbf,0xb9,		/*	 x x  xx x xxxxxxx xxx	x	*/
	0xaa,0x87,0xb9,		/*	 x x x x x	  xxxx xxx	x	*/
	0xac,0xbf,0xb9,		/*	 x x xx  x xxxxxxx xxx	x	*/
	0xae,0x83,0xb9,		/*	 x x xxx x	   xxx xxx	x	*/
	0xbf,0xff,0xf9,		/*	 x xxxxxxxxxxxxxxxxxxx	x	*/
	0xbf,0xff,0xf9,		/*	 x xxxxxxxxxxxxxxxxxxx	x	*/
	0x80,0x00,0x01,		/*	 x						x	*/
	0xc0,0x00,0x01,		/*	 xx 					x	*/
	0x7f,0xff,0xff		/*	  xxxxxxxxxxxxxxxxxxxxxxx	*/
};

char bigRamDriveIconData[] =
{
						/*	000011112222333344445555	*/
	0x1B,0x6D,0xB6,		/*	   xx xx xx xx xx xx xx 	*/
	0x3f,0xff,0xfe,		/*	  xxxxxxxxxxxxxxxxxxxxx 	*/
	0x20,0x00,0x02,		/*	  x 				  x 	*/
	0x20,0x00,0x02,		/*	  x 				  x 	*/
	0x27,0x19,0x12,		/*	  x  xxx   xx  x   x  x 	*/
	0x24,0xA5,0xB2,		/*	  x  x	x x  x xx xx  x 	*/
	0x27,0xBD,0x52,		/*	  x  xxxx xxxx x x x  x 	*/
	0x25,0x25,0x12,		/*	  x  x x  x  x x   x  x 	*/
	0x24,0xA5,0x12,		/*	  x  x	x x  x x   x  x 	*/
	0x20,0x00,0x02,		/*	  x 				  x 	*/
	0x20,0x00,0x02,		/*	  x 				  x 	*/
	0x3f,0xff,0xfe,		/*	  xxxxxxxxxxxxxxxxxxxxx 	*/
	0x1B,0x6D,0xB6,		/*	   xx xx xx xx xx xx xx 	*/
	0x00,0x00,0x00		/*								*/
};

char bigRamDriveIconDataInvert[] =
{						/*	000011112222333344445555   */
	0x3f,0xff,0xfe, 	/*	  xxxxxxxxxxxxxxxxxxxxx    */
	0x64,0x92,0x49, 	/*	 xx  x	x  x  x  x	x  x   */
	0x40,0x00,0x01, 	/*	 x					   x   */
	0x5f,0xff,0xfd, 	/*	 x xxxxxxxxxxxxxxxxxxx x   */
	0x58,0xe6,0xed, 	/*	 x xx	xxx  xx xxx xx x   */
	0x5b,0x5a,0x4d, 	/*	 x xx xx x xx x  x	xx x   */
	0x58,0x42,0xad, 	/*	 x xx	 x	  x x x xx x   */
	0x5a,0xda,0xed, 	/*	 x xx x xx xx x xxx xx x   */
	0x5b,0x5a,0xed, 	/*	 x xx xx x xx x xxx xx x   */
	0x5f,0xff,0xfd, 	/*	 x xxxxxxxxxxxxxxxxxxx x   */
	0x5f,0xff,0xfd, 	/*	 x xxxxxxxxxxxxxxxxxxx x   */
	0x40,0x00,0x01, 	/*	 x					   x   */
	0x64,0x92,0x49, 	/*	 xx  x	x  x  x  x	x  x   */
	0x3f,0xff,0xfe, 	/*	  xxxxxxxxxxxxxxxxxxxxx    */
};

char smallRamDriveIconData[] =
{
						/*	000011112222333344445555	*/
	0x3f,0xff,0xfe,		/*	  xxxxxxxxxxxxxxxxxxxxx 	*/
	0x20,0x00,0x02,		/*	  x 				  x 	*/
	0x27,0x19,0x12,		/*	  x  xxx   xx  x   x  x 	*/
	0x24,0xA5,0xB2,		/*	  x  x	x x  x xx xx  x 	*/
	0x27,0xBD,0x52,		/*	  x  xxxx xxxx x x x  x 	*/
	0x25,0x25,0x12,		/*	  x  x x  x  x x   x  x 	*/
	0x20,0x00,0x02,		/*	  x 				  x 	*/
	0x3f,0xff,0xfe,		/*	  xxxxxxxxxxxxxxxxxxxxx 	*/

};

char smallRamDriveIconDataInvert[] =
{						/*	000011112222333344445555   */
	0x7f,0xff,0xff, 	/*	 xxxxxxxxxxxxxxxxxxxxxxx   */
	0x5f,0xff,0xfd, 	/*	 x xxxxxxxxxxxxxxxxxxx x   */
	0x58,0xe6,0xed, 	/*	 x xx	xxx  xx xxx xx x   */
	0x5b,0x5a,0x4d, 	/*	 x xx xx x xx x  x	xx x   */
	0x58,0x42,0xad, 	/*	 x xx	 x	  x x x xx x   */
	0x5a,0xda,0xed, 	/*	 x xx x xx xx x xxx xx x   */
	0x5f,0xff,0xfd, 	/*	 x xxxxxxxxxxxxxxxxxxx x   */
	0x7f,0xff,0xff, 	/*	 xxxxxxxxxxxxxxxxxxxxxxx   */

};


char bigCDRomIconData[] =
{				/*   000011112222333344445555	*/
	0x00,0x00,0x00,		/*                              */
	0x07,0xff,0xe0, 	/*	  xxxxxxxxxxxxxx	 */
	0x18,0x00,0x18, 	/*	xx		xx	 */
	0x21,0xc7,0x84, 	/*     x    xxx   xxxx	  x	 */
	0x22,0x24,0x44, 	/*     x   x   x  x   x   x	 */
	0x42,0x04,0x22, 	/*    x    x	  x    x   x	 */
	0x42,0x04,0x22, 	/*    x    x	  x    x   x	 */
	0x42,0x04,0x22, 	/*    x    x	  x    x   x	 */
	0x22,0x24,0x44, 	/*     x   x   x  x   x   x	 */
	0x21,0xc7,0x84, 	/*     x    xxx   xxxx	  x	 */
	0x18,0x00,0x18, 	/*	xx		xx	 */
	0x07,0xff,0xe0, 	/*	  xxxxxxxxxxxxxx	 */
	0x00,0x00,0x00		/*                              */
};

char bigCDRomIconDataInvert[] =
{				/*   000011112222333344445555	*/

	0x07,0xff,0xe0, 	/*	  xxxxxxxxxxxxxx	   */
	0x18,0x00,0x18, 	/*	xx		xx	 */
	0x27,0xff,0xe4, 	/*     x  xxxxxxxxxxxxxx  x	 */
	0x5e,0x38,0x7A, 	/*    x xxxx   xxx    xxxx x	 */
	0x5d,0xdb,0xba, 	/*    x xxx xxx xx xxx xxx x	 */
	0xbd,0xfb,0xdd, 	/*   x xxxx xxxxxx xxxx xxx x	 */
	0xbd,0xfb,0xdd, 	/*   x xxxx xxxxxx xxxx xxx x	 */
	0xbd,0xfb,0xdd, 	/*   x xxxx xxxxxx xxxx xxx x	 */
	0x5d,0xdb,0xba, 	/*    x xxx xxx xx xxx xxx x	 */
	0x5e,0x38,0x7A, 	/*    x xxxx   xxx    xxxx x	 */
	0x27,0xff,0xe4, 	/*     x  xxxxxxxxxxxxxx  x	 */
	0x18,0x00,0x18, 	/*	xx		xx	 */
	0x07,0xff,0xe0		/*	  xxxxxxxxxxxxxx	   */
};

char smallCDRomIconData[] =
{				/*   000011112222333344445555	*/
	0x18,0x00,0x18, 	/*	xx		xx	 */
	0x21,0xc7,0x84, 	/*     x    xxx   xxxx	  x	 */
	0x22,0x24,0x44, 	/*     x   x   x  x   x   x	 */
	0x42,0x04,0x22, 	/*    x    x	  x    x   x	 */
	0x42,0x04,0x22, 	/*    x    x	  x    x   x	 */
	0x22,0x24,0x44, 	/*     x   x   x  x   x   x	 */
	0x21,0xc7,0x84, 	/*     x    xxx   xxxx	  x	 */
	0x18,0x00,0x18		/*	xx		xx	 */
};

char smallCDRomIconDataInvert[] =
{
	0x27,0xff,0xe4, 	/*     x  xxxxxxxxxxxxxx  x	 */
	0x5e,0x38,0x7A, 	/*    x xxxx   xxx    xxxx x	 */
	0x5d,0xdb,0xba, 	/*    x xxx xxx xx xxx xxx x	 */
	0xbd,0xfb,0xdd, 	/*   x xxxx xxxxxx xxxx xxx x	 */
	0xbd,0xfb,0xdd, 	/*   x xxxx xxxxxx xxxx xxx x	 */
	0x5d,0xdb,0xba, 	/*    x xxx xxx xx xxx xxx x	 */
	0x5e,0x38,0x7A, 	/*    x xxxx   xxx    xxxx x	 */
	0x27,0xff,0xe4		/*     x  xxxxxxxxxxxxxx  x	 */
};


struct BITMAP  bigProgManIcon =
{
	bigProgManIconData,
	3,
	0, 0, 24, 13
};
struct BITMAP  bigProgManIconInvert =
{
	bigProgManIconInvertData,
	3,
	0, 0, 24, 14
};

struct BITMAP  smallProgManIcon =
{
	smallProgManIconData,
	3,
	0, 0, 24, 8
};
struct BITMAP  smallProgManIconInvert =
{
	smallProgManIconInvertData,
	3,
	0, 0, 24, 8
};




struct BITMAP  bigUpArrow =
{
    bigUpArrowData,
    2,
    0,0,16,13		 /* left,top,right,bottom*/
};

struct BITMAP  bigDownArrow =
{
    bigDownArrowData,
    2,
    0,0,16,13		 /* left,top,right,bottom*/
};

struct BITMAP  bigFileIcon =
{
    bigFileIconData,
    3,
    0,0,24,13		 /* left,top,right,bottom*/
};

struct BITMAP  bigFileIconInvert =
{
    bigFileIconDataInvert,
    3,
    0,0,24,13		 /* left,top,right,bottom*/
};

struct BITMAP  bigProgramIcon =
{
    bigProgramIconData,
    3,
    0,0,24,13		 /* left,top,right,bottom*/
};

struct BITMAP  bigProgramIconInvert =
{
    bigProgramIconDataInvert,
    3,
    0,0,24,14		 /* left,top,right,bottom*/
};

struct BITMAP  bigDirExpansionIcon =
{
	bigDirExpansionIconData,
    3,
	0,0,24,15		 /* left,top,right,bottom*/
};

struct BITMAP  bigDirCollapseIcon =
{
	bigDirCollapseIconData,
    3,
	0,0,24,15		 /* left,top,right,bottom*/
};

struct BITMAP  bigDirNoZoomIcon =
{
	bigDirNoZoomIconData,
    3,
	0,0,24,15		 /* left,top,right,bottom*/
};

struct BITMAP bigFloppyIcon =
{
    bigFloppyIconData,
    3,
	0, 0, 24, 13	/* l, t, r, b */
};

struct BITMAP bigFloppyIconInvert =
{
    bigFloppyIconDataInvert,
    3,
	0, 0, 24, 14	/* l, t, r, b */
};

struct BITMAP bigHDIcon =
{
    bigHDIconData,
    3,
    0, 0, 24, 13	/* l, t, r, b */
};

struct BITMAP bigHDIconInvert =
{
    bigHDIconDataInvert,
    3,
    0, 0, 24, 14	/* l, t, r, b */
};

struct BITMAP bigRemoteIcon =
{
    bigRemoteIconData,
    3,
    0, 0, 24, 13	/* l, t, r, b */
};

struct BITMAP bigRemoteIconInvert =
{
    bigRemoteIconDataInvert,
    3,
    0, 0, 24, 14	/* l, t, r, b */
};

struct BITMAP bigRamDriveIcon =
{
	bigRamDriveIconData,
	3,
	0, 0, 24, 14	/* l, t, r, b */
};

struct BITMAP bigRamDriveIconInvert =
{
	bigRamDriveIconDataInvert,
	3,
	0, 0, 24, 14	/* l, t, r, b */
};

struct BITMAP bigCDRomIcon =
{
	bigCDRomIconData,
    3,
	0, 0, 24, 13	/* l, t, r, b */
};

struct BITMAP bigCDRomIconInvert =
{
	bigCDRomIconDataInvert,
    3,
	0, 0, 24, 13	/* l, t, r, b */
};


struct BITMAP  bigRadioButtonUnSel =
{
	bigRadioButtonUnSelData,
	2,
	0, 0, 16, 12
};

struct BITMAP  bigRadioButtonSel =
{
    bigRadioButtonSelData,
    2,
    0,0,16,12		 /* left,top,right,bottom*/
};


char smallRadioButtonUnSelData[] =
{
                //8421842184218421
    0x03,0xC0,  //      XXXX
    0x0C,0x30,  //    XX    XX
    0x30,0x0C,  //  XX        XX
    0x40,0x02,  // X            X
    0x30,0x0C,  //  XX        XX
    0x0C,0x30,  //    XX    XX
    0x03,0xC0   //      XXXX
};

char smallRadioButtonSelData[] =
{
                //8421842184218421
    0x03,0xC0,  //      XXXX
    0x0C,0x30,  //    XX    XX
    0x31,0x8C,  //  XX   XX   XX
    0x47,0xE2,  // X   XXXXXX   X
    0x31,0x8C,  //  XX   XX   XX
    0x0C,0x30,  //    XX    XX
    0x03,0xC0   //      XXXX
};


char smallUpArrowData[] =
{
                //8421842184218421
    0x00,0x00,  //
    0x01,0x80,  //       XX
    0x03,0xC0,  //      XXXX
    0x07,0xE0,  //     XXXXXX
    0x0F,0xF0,  //    XXXXXXXX
    0x03,0xC0,  //      XXXX
    0x03,0xC0,  //      XXXX
    0x03,0xC0   //      XXXX
};

char smallDownArrowData[] =
{
                //8421842184218421
    0x00,0x00,  //
    0x03,0xC0,  //      XXXX
    0x03,0xC0,  //      XXXX
    0x03,0xC0,  //      XXXX
    0x0F,0xF0,  //    XXXXXXXX
    0x07,0xE0,  //     XXXXXX
    0x03,0xC0,  //      XXXX
    0x01,0x80,  //       XX
};

char smallFileIconData[] =
{
                     //842184218421842184218421
    0x00,0x00,0x00,  //
    0x1F,0xFF,0xF0,  //   XXXXXXXXXXXXXXXXX
    0x10,0x00,0x28,  //   X              X X
    0x15,0x55,0x3C,  //   X X X X X X X  XXXX
    0x12,0xAA,0x84,  //   X  X X X X X X    X
    0x10,0x00,0x04,  //   X                 X
    0x1F,0xFF,0xFC,  //   XXXXXXXXXXXXXXXXXXX
    0x00,0x00,0x00,  //
};

char smallFileIconDataInvert[] =
{
                     //842184218421842184218421
    0x7F,0xFF,0xF8,  // XXXXXXXXXXXXXXXXXXXX
    0x60,0x00,0x0C,  // XX                 XX
    0x6F,0xFF,0xD6,  // XX XXXXXXXXXXXXXX X XX
    0x6A,0xAA,0xC3,  // XX X X X X X X XX    XX
    0x6D,0x55,0x7B,  // XX XX X X X X X XXXX XX
    0x6F,0xFF,0xFB,  // XX XXXXXXXXXXXXXXXXX XX
    0x60,0x00,0x03,  // XX                   XX
    0x7F,0xFF,0xFF,  // XXXXXXXXXXXXXXXXXXXXXXX
};


char smallProgramIconData[] =
{
                     //842184218421842184218421
    0x00,0x00,0x00,  //
    0x07,0xFF,0xF0,  //     XXXXXXXXXXXXXXX
    0x08,0x00,0x08,  //    X               X
    0x15,0x45,0x54,  //   X X X X   X X X X X
    0x15,0x72,0x54,  //   X X X XXX  X  X X X
    0x08,0x00,0x08,  //    X               X
    0x07,0xFF,0xF0,  //     XXXXXXXXXXXXXXX
    0x00,0x00,0x00   //
};
	
char smallProgramIconDataInvert[]=
{
                     //842184218421842184218421
    0x1F,0xFF,0xFC,  //   XXXXXXXXXXXXXXXXXXX
    0x38,0x00,0x0E,  //  XXX               XXX
    0x77,0xFF,0xF7,  // XXX XXXXXXXXXXXXXXX XXX
    0x6A,0x69,0x2B,  // XX X X  XX X  X  X X XX
    0x6A,0x63,0x2B,  // XX X X  XX   XX  X X XX
    0x77,0xFF,0xF7,  // XXX XXXXXXXXXXXXXXX XXX
    0x38,0x00,0x0E,  //  XXX               XXX
    0x1F,0xFF,0xFC   //   XXXXXXXXXXXXXXXXXXX
};

char smallDirExpansionIconData[] =
{
                     //842184218421842184218421
    0x3F,0x00,0x00,  //   xxxx
    0x40,0xff,0xf8,  // x      xxxxxxxxxxxxx
    0x7f,0x00,0x04,  // xxxxxxx             x
    0x40,0x08,0x02,  // x          x         x
    0x40,0x7f,0x02,  // x       xxxxxxx      x
    0x40,0x08,0x02,  // x          x         x
    0x40,0x00,0x02,  // x                    x
    0x7f,0xff,0xfe,  // xxxxxxxxxxxxxxxxxxxxxx
};
	
char smallDirCollapseIconData[] =
{
                     //842184218421842184218421
    0x3F,0x00,0x00,  //   xxxx
    0x40,0xff,0xf8,  // x      xxxxxxxxxxxxx
    0x7f,0x00,0x04,  // xxxxxxx             x
    0x40,0x00,0x02,  // x                    x
    0x40,0x7f,0x02,  // x       xxxxxxx      x
    0x40,0x00,0x02,  // x                    x
    0x40,0x00,0x02,  // x                    x
    0x7f,0xff,0xfe,  // xxxxxxxxxxxxxxxxxxxxxx
};

char smallDirNoZoomIconData[] =
{
                     //842184218421842184218421
    0x3F,0x00,0x00,  //  XXXXXX
    0x40,0xff,0xf8,  // X      XXXXXXXXXXXXX
    0x7f,0x00,0x04,  // XXXXXXX             X
    0x40,0x00,0x02,  // X                    X
    0x40,0x00,0x02,  // X                    X
    0x40,0x00,0x02,  // x                    x
    0x40,0x00,0x02,  // X                    X
    0x7f,0xff,0xfe,  // XXXXXXXXXXXXXXXXXXXXXX
};



char smallFloppyIconData[] =
{
                     //842184218421842184218421
    0x00,0x00,0x00,  //
    0x7f,0xff,0xfC,  // XXXXXXXXXXXXXXXXXXXXX
    0x43,0xE0,0x04,  // X    XXXXX          X
    0x4f,0xff,0xe4,  // X  XXXXXXXXXXXXXXX  X
    0x43,0xE0,0x04,  // X    XXXXX          X
    0x40,0x00,0x04,  // X                   X
    0x7f,0xff,0xfC,  // XXXXXXXXXXXXXXXXXXXXX
    0x00,0x00,0x00   //
};

char smallFloppyIconDataInvert[] =
{
                     //842184218421842184218421
    0xFF,0xFF,0xFE,  //XXXXXXXXXXXXXXXXXXXXXXX
    0x80,0x00,0x02,  //X                     X
    0xBC,0x1F,0xFA,  //X XXXX     XXXXXXXXXX X
    0xB0,0x00,0x1A,  //X XX               XX X
    0xBC,0x1F,0xFA,  //X XXXX     XXXXXXXXXX X
    0xBF,0xFF,0xFA,  //X XXXXXXXXXXXXXXXXXXX X
    0x80,0x00,0x02,  //X                     X
    0xFF,0xFF,0xFE,  //XXXXXXXXXXXXXXXXXXXXXXX
};

char smallHDIconData[] =
{
                     //842184218421842184218421
    0x00,0x00,0x00,  //
    0x7f,0xff,0xfc,  // xxxxxxxxxxxxxxxxxxxxx
    0x40,0x00,0x04,  // x                   x
    0x40,0x00,0x04,  // x                   x
    0x4F,0x00,0x04,  // x  XXXX             x
    0x40,0x00,0x04,  // x                   x
    0x7f,0xff,0xfC,  // xxxxxxxxxxxxxxxxxxxxx
    0x00,0x00,0x00,  //
};

char smallHDIconDataInvert[] =
{
                     //842184218421842184218421
    0xFF,0xFF,0xFE,  //XXXXXXXXXXXXXXXXXXXXXXX
    0x80,0x00,0x02,  //X                     X
    0xBF,0xFF,0xFA,  //X XXXXXXXXXXXXXXXXXXX X
    0xBF,0xFF,0xFA,  //X XXXXXXXXXXXXXXXXXXX X
    0xB0,0xFF,0xFA,  //X XX    XXXXXXXXXXXXX X
    0xBF,0xFF,0xFA,  //X XXXXXXXXXXXXXXXXXXX X
    0x80,0x00,0x02,  //X                     X
    0xFF,0xFF,0xFE,  //XXXXXXXXXXXXXXXXXXXXXXX
};

#if 0
char smallRemoteIconData[] =
{
                     //842184218421842184218421
    0x00,0x00,0x00,  //
    0x7f,0xff,0xfc,  // xxxxxxxxxxxxxxxxxxxxx
    0x40,0x00,0x04,  // x                   x
    0x40,0x00,0x04,  // x                   x
    0x40,0x00,0x04,  // x                   x
    0x40,0x00,0x04,  // x                   x
    0x7f,0xff,0xfC,  // xxxxxxxxxxxxxxxxxxxxx
    0x00,0x00,0x00,  //
};

char smallRemoteIconDataInvert[] =
{
                     //842184218421842184218421
    0xFF,0xFF,0xFE,  //XXXXXXXXXXXXXXXXXXXXXXX
    0x80,0x00,0x02,  //X                     X
    0xBF,0xFF,0xFA,  //X XXXXXXXXXXXXXXXXXXX X
    0xBF,0xFF,0xFA,  //X XXXXXXXXXXXXXXXXXXX X
    0xBF,0xFF,0xFA,  //X XXXXXXXXXXXXXXXXXXX X
    0xBF,0xFF,0xFA,  //X XXXXXXXXXXXXXXXXXXX X
    0x80,0x00,0x02,  //X                     X
    0xFF,0xFF,0xFE,  //XXXXXXXXXXXXXXXXXXXXXXX
};
#endif

char smallRemoteIconData[] =
{						/*	 000011112222333344445555	*/
	0x7f,0xff,0xfc,		/*    xxxxxxxxxxxxxxxxxxxxx     */
	0x40,0x00,0x04,		/*	  x 				  x 	*/
	0x51,0x7d,0xf4, 	/*	  x x	x xxxxx	xxxxx x 	*/
	0x59,0x40,0x44,		/*	  x xx	x x		  x	  x 	*/
	0x55,0x78,0x44,		/*	  x x x x xxxx	  x	  x 	*/
	0x53,0x40,0x44,		/*	  x x  xx x		  x	  x 	*/
	0x51,0x7c,0x44,		/*	  x x	x xxxxx	  x	  x 	*/
	0x7f,0xff,0xfe,		/*	  xxxxxxxxxxxxxxxxxxxxx 	*/
};

char smallRemoteIconDataInvert[] =
{						/*	 000011112222333344445555	*/
	0xff,0xff,0xfe,		/*	 xxxxxxxxxxxxxxxxxxxxxxx */
	0xbf,0xff,0xfa,		/*	 x xxxxxxxxxxxxxxxxxxx x	*/
	0xae,0x82,0x0a, 	/*	 x x xxx x	   x	 x x	*/
	0xa6,0xbf,0xba,		/*	 x x  xx x xxxxxxx xxx x	*/
	0xaa,0x87,0xba,		/*	 x x x x x	  xxxx xxx x	*/
	0xac,0xbf,0xba,		/*	 x x xx  x xxxxxxx xxx x	*/
	0xae,0x83,0xba,		/*	 x x xxx x	   xxx xxx x	*/
	0xff,0xff,0xfe		/*	 xxxxxxxxxxxxxxxxxxxxxxx	*/
};


struct BITMAP  smallUpArrow =
{
    smallUpArrowData,
    2,
    0,0,16,8            // left,top,right,bottom
};

struct BITMAP  smallDownArrow =
{
    smallDownArrowData,
    2,
    0,0,16,8            // left,top,right,bottom
};

struct BITMAP smallFileIcon =
{
    smallFileIconData,
    3,
    0,0,24, 8           // left,top,right,bottom
};

struct BITMAP  smallFileIconInvert =
{
    smallFileIconDataInvert,
    3,
    0,0,24, 8           // left,top,right,bottom
};

struct BITMAP  smallProgramIcon =
{
    smallProgramIconData,
    3,
    0,0,24, 8           // left,top,right,bottom
};

struct BITMAP  smallProgramIconInvert =
{
    smallProgramIconDataInvert,
    3,
    0,0,24, 8           // left,top,right,bottom
};

struct BITMAP  smallDirExpansionIcon =
{
    smallDirExpansionIconData,
    3,
    0,0,24, 8           // left,top,right,bottom
};

struct BITMAP  smallDirCollapseIcon =
{
    smallDirCollapseIconData,
    3,
    0,0,24, 8           // left,top,right,bottom
};

struct BITMAP  smallDirNoZoomIcon =
{
    smallDirNoZoomIconData,
    3,
    0,0,24, 8           // left,top,right,bottom
};

struct BITMAP smallFloppyIcon =
{
    smallFloppyIconData,
    3,
    0, 0, 24,  8        // left,top,right,bottom
};

struct BITMAP smallFloppyIconInvert =
{
    smallFloppyIconDataInvert,
    3,
    0, 0, 24,  8        // left,top,right,bottom
};

struct BITMAP smallHDIcon =
{
    smallHDIconData,
    3,
    0, 0, 24,  8        // left,top,right,bottom
};

struct BITMAP smallHDIconInvert =
{
    smallHDIconDataInvert,
    3,
    0, 0, 24,  8        // left,top,right,bottom
};

struct BITMAP smallRemoteIcon =
{
    smallRemoteIconData,
    3,
    0, 0, 24,  8        // left,top,right,bottom
};

struct BITMAP smallRamDriveIcon =
{
	smallRamDriveIconData,
    3,
    0, 0, 24,  8        // left,top,right,bottom
};

struct BITMAP smallRamDriveIconInvert =
{
	smallRamDriveIconDataInvert,
    3,
    0, 0, 24,  8        // left,top,right,bottom
};

struct BITMAP smallCDRomIcon =
{
	smallCDRomIconData,
    3,
    0, 0, 24,  8        // left,top,right,bottom
};

struct BITMAP smallCDRomIconInvert =
{
	smallCDRomIconDataInvert,
    3,
    0, 0, 24,  8        // left,top,right,bottom
};

struct BITMAP smallRemoteIconInvert =
{
    smallRemoteIconDataInvert,
    3,
    0, 0, 24,  8         // left,top,right,bottom
};

struct BITMAP  smallRadioButtonUnSel =
{
    smallRadioButtonUnSelData,
    2,
    0, 0, 16,  7         // left,top,right,bottom
};

struct BITMAP  smallRadioButtonSel =
{
    smallRadioButtonSelData,
    2,
    0,0,16, 7            // left,top,right,bottom
};

#if 0
/* ZZZZZZZZZZZZZZZZZZZZZZZZZZZ Unused icons ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ */

char bigRightArrowData[]=
{
/*		  321032103232103210 */
	0x00,0x00,	/*			*/
    0x00,0x00,	/*		    */
    0x00,0x00,	/*		    */
    0x00,0x00,	/*		    */
    0x00,0x40,	/*	     X	    */
    0x00,0x70,	/*	     XXX    */
    0x1F,0xFC,	/*   XXXXXXXXXXXXX  */
    0x1F,0xFE,	/*   XXXXXXXXXXXXXX */
    0x1F,0xFC,	/*   XXXXXXXXXXXXX  */
    0x00,0x70,	/*	     XXX    */
    0x00,0x40,	/*	     X	    */
    0x00,0x00,	/*		    */
    0x00,0x00,	/*		    */
    0x00,0x00,	/*		    */
};


char bigLeftArrowData[] =
{
/*		  321032103232103210  */
    0x00,0x00,	/*		    */
    0x00,0x00,	/*		    */
    0x00,0x00,	/*		    */
    0x00,0x00,	/*		    */
    0x01,0x00,	/*	 X	    */
    0x07,0x00,	/*     XXX	    */
    0x1F,0xFC,	/*   XXXXXXXXXXXXX  */
    0x3F,0xFC,	/*  XXXXXXXXXXXXXX  */
    0x1F,0xFC,	/*   XXXXXXXXXXXXX  */
    0x07,0x00,	/*     XXX	    */
    0x01,0x00,	/*	 X	    */
    0x00,0x00,	/*		    */
    0x00,0x00,	/*		    */
    0x00,0x00,	/*		    */
};

char bigColorLeftData[]=
{
		     /*765432107654321076543210*/
   0X3F,0xFF,0xFC,   /*  XXXXXXXXXXXXXXXXXXXX  */
   0x60,0x00,0x06,   /* XX		    XX */
   0xA0,0x40,0x05,   /*X X	X	    X X*/
   0xA0,0xC0,0x05,   /*X X     XX	    X X*/
   0xA3,0xFF,0x85,   /*X X   XXXXXXXXXXX    X X*/
   0xA7,0xFF,0x85,   /*X X  XXXXXXXXXXXX    X X*/
   0xA3,0xFF,0x85,   /*X X   XXXXXXXXXXX    X X*/
   0xA0,0xC0,0x05,   /*X X     XX	    X X*/
   0xA0,0x40,0x05,   /*X X	X	    X X*/
   0xA0,0x00,0x05,   /*X X		    X X*/
   0xBF,0xFF,0xFD,   /*X XXXXXXXXXXXXXXXXXXXX X*/
   0xA0,0x00,0x05,   /*X X		    X X*/
   0x40,0x00,0x02,   /* X		     X */
   0x3F,0xFF,0xFC    /*  XXXXXXXXXXXXXXXXXXXX  */
};

char bigColorRightData[]=
{
		     /*765432107654321076543210*/
   0X3F,0xFF,0xFC,   /*  XXXXXXXXXXXXXXXXXXXX  */
   0x60,0x00,0x06,   /* XX		    XX */
   0xA0,0x02,0x05,   /*X X	     X	    X X*/
   0xA0,0x03,0x05,   /*X X	     XX     X X*/
   0xA3,0xFF,0xC5,   /*X X   XXXXXXXXXXXX   X X*/
   0xA3,0xFF,0xE5,   /*X X   XXXXXXXXXXXXX  X X*/
   0xA3,0xFF,0xC5,   /*X X   XXXXXXXXXXXX   X X*/
   0xA0,0x03,0x05,   /*X X	     XX     X X*/
   0xA0,0x02,0x05,   /*X X	     X	    X X*/
   0xA0,0x00,0x05,   /*X X		    X X*/
   0xBF,0xFF,0xFD,   /*X XXXXXXXXXXXXXXXXXXXX X*/
   0xA0,0x00,0x05,   /*X X		    X X*/
   0x40,0x00,0x02,   /* X		     X */
   0x3F,0xFF,0xFC    /*  XXXXXXXXXXXXXXXXXXXX  */
};

char smallRightArrowData[]=
{
                //8421842184218421
    0x00,0x00,  //
    0x00,0x40,  //         X
    0x00,0x70,  //         XXX
    0x1F,0xFC,  //   XXXXXXXXXXX
    0x1F,0xFE,  //   XXXXXXXXXXXX
    0x1F,0xFC,  //   XXXXXXXXXXX
    0x00,0x70,  //         XXX
    0x00,0x40,  //         X
};


char smallLeftArrowData[] =
{
                //8421842184218421
    0x00,0x00,  //
    0x01,0x00,  //       X
    0x07,0x00,  //     XXX
    0x1F,0xFC,  //   XXXXXXXXXXX
    0x3F,0xFC,  //  XXXXXXXXXXXX
    0x1F,0xFC,  //   XXXXXXXXXXX
    0x07,0x00,  //     XXX
    0x01,0x00,  //       X
};

struct BITMAP	bigRightArrow =
{
    bigRightArrowData,
    2,
    0,0,16,14		 /* left,top,right,bottom*/
};
struct BITMAP  bigLeftArrow =
{
    bigLeftArrowData,
    2,
    0,0,16,14		 /* left,top,right,bottom*/
};

struct BITMAP  bigColorLeft=
{
    bigColorLeftData,
    3,
    0,0,24,14
};
struct BITMAP  bigColorRight=
{
    bigColorRightData,
    3,
    0,0,24,14
};

struct BITMAP  smallRightArrow =
{
    smallRightArrowData,
    2,
    0,0,16,8            // left,top,right,bottom
};

struct BITMAP  smallLeftArrow =
{
    smallLeftArrowData,
    2,
    0,0,16,8            // left,top,right,bottom
};

char bigPgDnIconData[] =	   /*  |   D   |   D   |   D   |   D	 */
{				//  32103210321032103210321032103210
	0x00,0x00,0x00,0x00,	/*				      */
	0x1F,0xFF,0xFF,0xF8,	/*     xxxxxxxxxxxxxxxxxxxxxxxxxx     */
	0x2C,0x7F,0x0F,0xF4,	/*    x xx   xxxxxxx	xxxxxxxx x    */
	0x6B,0xA3,0x76,0x36,	/*   xx x xxx x   xx xxx xx   xx xx   */
	0x6B,0x5D,0x75,0xD6,	/*   xx x xx x xxx x xxx x xxx x xx   */
	0x68,0xE1,0x75,0xD6,	/*   xx x   xxx    x xxx x xxx x xx   */
	0x6B,0xFD,0x0D,0xD6,	/*   xx x xxxxxxxx x	xx xxx x xx   */
	0x6B,0xF3,0xFF,0xF6,	/*   xx x xxxxxx  xxxxxxxxxxxxxx xx   */
	0x6F,0xFF,0xFF,0xF6,	/*   xx xxxxxxxxxxxxxxxxxxxxxxxx xx   */
	0x60,0x00,0x00,0x06,	/*   xx 			 xx   */
	0x6F,0xFF,0xFF,0xF6,	/*   xx xxxxxxxxxxxxxxxxxxxxxxxx xx   */
	0x5F,0xFF,0xFF,0xFA,	/*   x xxxxxxxxxxxxxxxxxxxxxxxxxx x   */
	0x3F,0xFF,0xFF,0xFC,	/*    xxxxxxxxxxxxxxxxxxxxxxxxxxxx    */
	0x00,0x00,0x00,0x00,	/*				     */
};

struct BITMAP  bigPgDnIcon =
{
	bigPgDnIconData,
	4,
	0, 0, 32, 14
};

char smallPgDnIconData[] =
{
                          //84218421842184218421842184218421
    0x3F,0xFF,0xFF,0xFC,  //  xxxxxxxxxxxxxxxxxxxxxxxxxxxx
    0x61,0xFF,0xC3,0xFE,  // xxx   xxxxxxxxxxx    xxxxxxxxx
    0x6E,0x8F,0xDD,0x8E,  // xx xxx x   xxxxxx xxx xx   xxx
    0x6D,0x77,0xDD,0x76,  // xx xx x xxx xxxxx xxx x xxx xx
    0x63,0x87,0xDD,0x76,  // xx   xxx    xxxxx xxx x xxx xx
    0x6F,0xF7,0xC3,0x76,  // xx xxxxxxxx xxxxx    xx xxx xx
    0x6F,0xCF,0xFF,0xFE,  // xx xxxxxx  xxxxxxxxxxxxxxxxxxx
    0x3F,0xFF,0xFF,0xFC,  //  xxxxxxxxxxxxxxxxxxxxxxxxxxxx
};

struct BITMAP  smallPgDnIcon =
{
    smallPgDnIconData,
    4,
    0, 0, 32,  8        // left,top,right,bottom
};


char bigPgUpIconData[] =	   /*  |   D   |   D   |   D   |   D	 */
{				//  32103210321032103210321032103210
	0x00,0x00,0x00,0x00,	/*				      */
	0x1F,0xFF,0xFF,0xF8,	/*     xxxxxxxxxxxxxxxxxxxxxxxxxx     */
	0x2C,0x7F,0x77,0xF4,	/*    x xx   xxxxxxx xxx xxxxxxx x    */
	0x6B,0xA3,0x76,0x36,	/*   xx x xxx x   xx xxx xx   xx xx   */
	0x6B,0x5D,0x75,0xD6,	/*   xx x xx x xxx x xxx x xxx x xx   */
	0x68,0xE1,0x75,0xB6,	/*   xx x   xxx    x xxx x xx xx xx   */
	0x6B,0xFD,0x84,0x76,	/*   xx x xxxxxxxx xx	 x   xxx xx   */
	0x6B,0xF3,0xF5,0xF6,	/*   xx x xxxxxx  xxxxxx x xxxxx xx   */
	0x6F,0xFF,0xFF,0xF6,	/*   xx xxxxxxxxxxxxxxxxxxxxxxxx xx   */
	0x60,0x00,0x00,0x06,	/*   xx 			 xx   */
	0x6F,0xFF,0xFF,0xF6,	/*   xx xxxxxxxxxxxxxxxxxxxxxxxx xx   */
	0x5F,0xFF,0xFF,0xFA,	/*   x xxxxxxxxxxxxxxxxxxxxxxxxxx x   */
	0x3F,0xFF,0xFF,0xFC,	/*    xxxxxxxxxxxxxxxxxxxxxxxxxxxx    */
	0x00,0x00,0x00,0x00,	/*				     */
};

struct BITMAP  bigPgUpIcon =
{
	bigPgUpIconData,
	4,
	0, 0, 32, 14
};

char smallPgUpIconData[] =
{
                          //84218421842184218421842184218421
    0x3F,0xFF,0xFF,0xFC,  //  xxxxxxxxxxxxxxxxxxxxxxxxxxxx
    0x61,0xFF,0xDD,0xFE,  // xxx   xxxxxxxxxxx xxx xxxxxxxx
    0x6E,0x8F,0xDD,0x8E,  // xx xxx x   xxxxxx xxx xx   xxx
    0x6D,0x77,0xDD,0x76,  // xx xx x xxx xxxxx xxx x xxx xx
    0x63,0x87,0xDD,0x76,  // xx   xxx    xxxxx xxx x xxx xx
    0x6F,0xF7,0xE1,0x0E,  // xx xxxxxxxx xxxxxx    x    xxx
    0x6F,0xCF,0xFD,0x7E,  // xx xxxxxx  xxxxxxxxxx x xxxxxx
    0x3F,0xFF,0xFF,0xFC,  //  xxxxxxxxxxxxxxxxxxxxxxxxxxxx
};

struct BITMAP  smallPgUpIcon =
{
    smallPgUpIconData,
    4,
    0, 0, 32,  8        // left,top,right,bottom
};

/* ZZZZZZZZZZZZZZZZZZZZZZZZZZZ Unused icons ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ */
#endif
