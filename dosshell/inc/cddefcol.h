;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*
	COW : Character Oriented Windows

	color.h : color info header file

*/

typedef struct _sa
	{
	union
		{
		struct
			{
			BYTE caMono;		/* initial B/W COLOR */
			BYTE caColor;		/* initial COLOR */
			} init;
		struct
			{
			BYTE bFiller;
			BYTE caSa;		/* the actual drawing ca */
			} draw;
		BYTE *rgcaFill;		/* special fill mode array */
		} u;
	} SA;		/* screen attribute */

typedef struct COLORTABLEENTRY
	{
	    ISA background;
	    ISA foreground;
	}COLORTABLEENTRY;


#define coBlack			0x0
#define coBlue			0x1
#define coGreen			0x2
#define coRed			0x4
#define coCyan			(coBlue + coGreen)
#define coMagenta		(coBlue + coRed)
#define coYellow		(coRed + coGreen)
#define coWhite			(coRed + coGreen + coBlue)


#define caMake(bg, fg, blink, hi) (fg | (blink << 7) | (bg << 4) | (hi << 3))

#define	caNull	   0	/* invalid ca */

#define	caBlackW	caMake(coWhite, coBlack, 0, 0)	/* Black on white */
#define	caBlue		caMake(coBlack, coBlue, 0, 0)
#define	caGreen		caMake(coBlack, coGreen, 0, 0)
#define	caRed		caMake(coBlack, coRed, 0, 0)
#define	caCyan		caMake(coBlack, coCyan, 0, 0)
#define	caMagenta	caMake(coBlack, coMagenta, 0, 0)
#define	caYellow	caMake(coBlack, coYellow, 0, 0)
#define	caWhite		caMake(coBlack, coWhite, 0, 0)
#define caBrightWhite	caMake(coBlack, coWhite, 0, 1)
#define caGrey		caMake(coBlack, coBlack, 0, 1)

#define isBright 8
