/*
	COW: Character Oriented Windows
	
	uscreen.h: Public definitions for USER screen and box
*/

/***BEGIN_PUBLIC***/

#ifndef NOCOLOR
#define	DiMake(dm, isa)		((WORD) ((dm) | (isa)))

/*	-- non-special modes --	*/
#define	dmNormal		0
#define	DiNormal(isa)		((WORD) dmNormal | (isa))
#define	dmText			0x100
#define	dmTextOnly		dmText
#define	dmForeBack		0x200
#define	dmAttrOnly		dmForeBack
#define	dmFore			0x300
#define	dmBack			0x400
#define	dmTextFore		0x500
#define	dmTextBack		0x600

/*	-- special modes --	*/
#define	dmSpecialMin		0x700
#define	dmTextMapB		0x700
#define	dmTextMapF		0x800
#define	dmMapB			0x900
#define	dmMapF			0xA00

/* special FFONT control */
#define	fdmKeepFfont		0x8000

#endif /*!NOCOLOR*/

extern BYTE PASCAL fMonochrome;		/* TRUE => monochrome screen */
extern char PASCAL chShadow;		/* shadow character */
extern WORD PASCAL diShadow;		/* shadow draw mode, 0 => no shadow */
VOID		FARPUBLIC SetCursorBlock(BOOL);

/***END_PUBLIC***/
