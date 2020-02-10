
struct tsnFile
    {
    int		wMagic;		/* identifies file as a help file	*/
    int		wAppl;		/* Application specific word		*/
    int		wSignature;	/* signature of app that uses it -	*/
				/* quickbasic, works, windows		*/
    int		cid;		/* count of topics in file		*/
    int		cwmp;		/* count of words in mapping section	*/
    int		cbm;		/* count of bitmaps			*/
    int		cbkeys;		/* count of text in keyphrase section	*/
    int		cbCookies;	/* count of bytes in magic cookie section*/
    };

#define CTOPIC_LEVELS_MAX 5
struct tsnTopic
    {
    unsigned short int	cLevels;
    unsigned short int	rgCLn[CTOPIC_LEVELS_MAX];
    };

struct tsnCookies
    {
    unsigned char	cRunspace;
    unsigned char	cKeybase;
    unsigned char	cKeybase2;
    unsigned char	cbKeys;
    unsigned char	cDefinition;
    unsigned char	cTopic;
    };

/* a help file is the above structure, followed by these sections:	*/
/*	(sorted) topic position array - array of double word file pos's	*/
/*	(sorted) mapping array - cwmp words				*/
/*	key phrase table - each phrase is separated by a null		*/
/*	bitmap size array - one double word per bitmap			*/
/*	magic cookie section.  application dependant.			*/
/*	bitmaps - indexed by bitmap size array				*/
/*	(sorted) compressed topics					*/

/*---------- Other Cruft -----------------------------------------------*/

#define cbRunMin    4           /* minimum length to get compression    */

#define wMagicHELP 	((int)0111213)	/* HELP file magic word         */

#define wSignatureBasic	0x4251	/* `QB' */

#define C_MIN					0x81
#define C_KEYPHRASE0			0x81		/* 1st keyphrase cookie						*/
#define C_KEYPHRASE1			0x82		/* 2nd keyphrase cookie						*/
#define C_KEYPHRASE2			0x83		/* 2nd keyphrase cookie						*/
#define C_KEYPHRASE_SPACE0	0x84		/* 1st keyphrase + space cookie			*/
#define C_KEYPHRASE_SPACE1	0x85		/* 2nd keyphrase + space cookie			*/
#define C_KEYPHRASE_SPACE2	0x86		/* 2nd keyphrase + space cookie			*/
#define C_RUNSPACE			0x87		/* Cookie for runs of spaces 				*/
#define C_RUN					0x88		/* Cookie for runs of non-space			*/
#define C_QUOTE				0x89		/* Cookie to quote non-cookies			*/
#define C_CONTEXT				0x8a		/* Context										*/
#define C_CONTEXT_SPACE		0x8b		/* Context + SPACE							*/
#define C_MAX					0x8b
