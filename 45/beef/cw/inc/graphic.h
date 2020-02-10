/*
	CW: Character Oriented Windows
	
	graphic.h: Graphics Interface Layer
*/

#include <ingxd.h>				/* installable driver */

/***BEGIN_PUBLIC***/

extern BOOL PASCAL fPrinting;

/*****************************************************************************/
/* Graphic Drawing Procedures */

BOOL	FARPUBLIC FLoadGsd(char *);
BOOL	FARPUBLIC FLoadGpd(char *);
VOID	FARPUBLIC SetPrinting(BOOL);

BOOL	FARPUBLIC FInitGraphics(VOID *, VOID FAR *);
VOID	FARPUBLIC TermGraphics(void);
VOID	FARPUBLIC Move(WORD, WORD);
VOID	FARPUBLIC Draw(WORD, WORD);
VOID	FARPUBLIC SetAreaPat(WORD);
VOID	FARPUBLIC SetLinePat(WORD);
VOID	FARPUBLIC SetLineWeight(WORD);
VOID	FARPUBLIC SetColor(WORD, WORD);
VOID	FARPUBLIC Text(char far *, WORD, WORD, WORD, int);
VOID	FARPUBLIC Rectangle(struct _rect far *);
VOID	FARPUBLIC Arc(struct _rect far *, int, int);
VOID	FARPUBLIC Polygon(struct _polygon far *);
VOID	FARPUBLIC BitBlt(struct _rect far *, BYTE far *, WORD, BOOL);

/***END_PUBLIC***/

/*****************************************************************************/
/* COW private info */

extern INDJ	indjGsd;		/* jump vectors for screen driver */
extern INDJ	indjGpd;		/* jump vectors for printer driver */

/*****************************************************************************/
