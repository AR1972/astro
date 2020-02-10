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


#define coBlack			0x0
#define coBlue			0x1
#define coGreen			0x2
#define coRed			0x4
#define coCyan			(coBlue + coGreen)
#define coMagenta		(coBlue + coRed)
#define coOrange		(coRed + coGreen)
#define coWhite			(coRed + coGreen + coBlue)
#define	CoBright(co)		((co) | 8)
#define	coGrey			CoBright(coBlack)
#define	coBrightWhite		CoBright(coWhite)
#define coYellow		CoBright(coOrange)

#define CaMake(coFore, coBack)	(((coBack) << 4) | (coFore) )

/* valid monochrome colors */
#define	caWhite			CaMake(coWhite, coBlack)
#define	caBlackOnWhite		CaMake(coBlack, coWhite)
#define	caBrightWhite		CaMake(coBrightWhite, coBlack)
#define	caBrightBlack		CaMake(coBlack, coBrightWhite)
		/* black on bright background */

/* color on standard background (grey) */
#define Ca(coFore)		CaMake(coFore, coGrey)
