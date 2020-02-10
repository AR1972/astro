/*
	COW : Character Oriented Windows

	charset.h : special characters and other customizing constants
		(IBM specific)
	ONLY USE SAFE CHARACTERS (VGA codepage 850)
*/

/* Boxes */
#define chTopSide1		((char) 196)
#define chBottomSide1		chTopSide1
#define chLeftSide1		((char) 179)
#define chRightSide1		chLeftSide1

#define chTopSide2		((char) 205)
#define chBottomSide2		chTopSide2
#define chLeftSide2		((char) 186)
#define chRightSide2		chLeftSide2

#define chTopLeftCorner1	((char) 218)
#define chTopRightCorner1	((char) 191)
#define chBottomLeftCorner1	((char) 192)
#define chBottomRightCorner1	((char) 217)
#define chMiddleLeft1		((char) 195)
#define chMiddleRight1		((char) 180)

#define chTopLeftCorner2	((char) 201)
#define chTopRightCorner2	((char) 187)
#define chBottomLeftCorner2	((char) 200)
#define chBottomRightCorner2	((char) 188)

#define chUpArrow		((char) 24)
#define chDownArrow		((char) 25)
#define chRightArrow		((char) 26)
#define chLeftArrow		((char) 27)

#define	chBullet		((char) 7)
#define	chMiddleDot		((char) 0xFA)		/* middle dot */

#define chScrollbar		((char) 0xb0)		/* hash */
#define chElevator		' '

#define	chShadowInit		((char) 0xb1)		/* other hash */


#ifdef WINDOW_OVERLAP
#define	chClose			((char) 0xDC)
#define	chZoomIn		((char) 0x1E)
#endif /*WINDOW_OVERLAP*/
