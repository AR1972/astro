/*
	COW: Character Oriented Windows
	
	ucolor.h: USER Color control
*/

/***BEGIN_PUBLIC***/

#define	SetSysColor(isa, coBack, coFore, fBlink, fHilite) \
	SetIsaColor(isa,				\
 	    (coFore) + ((fHilite) ? 8 : 0),	\
 	    (coBack) + ((fBlink) ? 8 : 0))

VOID		FARPUBLIC SetIsaColor(ISA, WORD, WORD);
VOID		FARPUBLIC GetIsaColor(ISA, WORD *, WORD *);
VOID		FARPUBLIC SetIsaRgca(ISA, BYTE *);
VOID		FARPUBLIC SetIsaFfont(ISA, WORD);		/*OPTIONAL*/

/***END_PUBLIC***/

