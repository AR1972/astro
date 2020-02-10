/***************************************************************************/
/* 																								*/
/* STRLIB.H																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Function prototypes and structure definitions for used by callers of 	*/
/* functions in STRLIB.C.																	*/
/* 																								*/
/* Created 03-23-89 - johnhe																*/
/* TABS = 3																						*/
/***************************************************************************/


#ifndef	EOL
	#define		EOL	'\0'
#endif

#ifndef	CTRL_C
	   /* Key codes */

	#define  CTRL_C   03
	#define	BEL      7
	#define	BS       8
	#define	CR       '\r'
	#define	CTRL_X   24
	#define  CTRL_R   18
	#define	TAB		9
	#define	SPC		32
	#define	QST		'?'
	#define	PERCENT	'%'
	#define	BLK		'Û'
	#define	LF			'\n'

	#define	CHAR_0	'0'
	#define	COLON		':'

	#define  ESC      0x1b
	#define  LEFT     0x4b
	#define  RIGHT    0x4d
	#define  UP       0x48
	#define  DOWN     0x50
	#define  PAGE_UP  0x49
	#define  PAGE_DN  0x51
	#define  END      0x4f
	#define  HOME     0x47
	#define  INS      0x52
	#define  DEL      0x53
	#define  CENTER   0x4c

	#define	LCASE_A	'a'
	#define	LCASE_B	'b'
	#define	LCASE_C	'c'
	#define	LCASE_D	'd'
	#define	LCASE_E	'e'
	#define	LCASE_F	'f'
	#define	LCASE_G	'g'
	#define	LCASE_H	'h'
	#define	LCASE_I	'i'
	#define	LCASE_J	'j'
	#define	LCASE_K	'k'
	#define	LCASE_L	'l'
	#define	LCASE_M	'm'
	#define	LCASE_N	'n'
	#define	LCASE_O	'o'
	#define	LCASE_P	'p'
	#define	LCASE_Q	'q'
	#define	LCASE_R	'r'
	#define	LCASE_S	's'
	#define	LCASE_T	't'
	#define	LCASE_U	'u'
	#define	LCASE_V	'v'
	#define	LCASE_W	'w'
	#define	LCASE_X	'x'
	#define	LCASE_Y	'y'
	#define	LCASE_Z	'z'

	#define	UCASE_A	'A'
	#define	UCASE_B	'B'
	#define	UCASE_C	'C'
	#define	UCASE_D	'D'
	#define	UCASE_E	'E'
	#define	UCASE_F	'F'
	#define	UCASE_G	'G'
	#define	UCASE_H	'H'
	#define	UCASE_I	'I'
	#define	UCASE_J	'J'
	#define	UCASE_K	'K'
	#define	UCASE_L	'L'
	#define	UCASE_M	'M'
	#define	UCASE_N	'N'
	#define	UCASE_O	'O'
	#define	UCASE_P	'P'
	#define	UCASE_Q	'Q'
	#define	UCASE_R	'R'
	#define	UCASE_S	'S'
	#define	UCASE_T	'T'
	#define	UCASE_U	'U'
	#define	UCASE_V	'V'
	#define	UCASE_W	'W'
	#define	UCASE_X	'X'
	#define	UCASE_Y	'Y'
	#define	UCASE_Z	'Z'

	#define  F1       0x3b
	#define  F2       0x3c
	#define  F3       0x3d
	#define  F4       0x3e
	#define  F5       0x3f
	#define  F6       0x40
	#define  F7       0x41
	#define  F8       0x42
	#define  F9       0x43
	#define  F10      0x44

#endif

/***************************************************************************/

extern void		BuildPath			( char *szPath, char chDrive, char *szDir,
											  char *szName );
extern long 	GetMaxHugeSize 	( void );
extern int		GetNumberStrings	( char **Strings );
extern unsigned InsertChar			( char *String, int Char );
extern unsigned MaxStrLen			( char **Strings );
extern void		PadStr				( char *szStr, char chChar, int Len );
extern char		*ParseFileName		( char *szPath );
extern unsigned RemoveSpaces		( char *szString );
extern unsigned RemoveTrailing	( char *String, char Char );
extern void		ReplaceChar			( char *szStr, char chChar, char chNewChar );
extern unsigned ShiftStringLeft	( char *String );
extern int		StrSearch			( char *szSearchStr, char **szStrs );
extern void		UnGetChar			( int Char );
extern int		IndexMaxInt 		( int aiMatch[], int iLen );
extern int		IsValidPathChar	( char Char	);
extern int		FindExtMatch		( char *szFile, char **szExt );
extern unsigned ExtractNextWord	( char *szStr, char *szBuffer, int iMax );
extern char 	*SkipLeadingWhite ( char *szPtr );
extern char 	*SkipNonWhite		( char *szPtr );
extern char 	*SkipWord			( char *szPtr );
extern int		IsWhite				( char ch );
extern int		FindParam			( char *szStr, char ch );

extern void		GetPathStrings		( char **apszPaths, char *chBuffer,
											  int BufSize );

extern void		DirToFileName		( char *Str, char *Dir );
extern int		FindString			( char far *Buffer, char far *String,
											  unsigned BufferSize );

extern char far *NormalizePtr		( char far *Ptr );
extern char	far *HugeAdd			( char far *Ptr, long lBytes );


#ifdef DBCS
extern int	IsDBCSLeadByte(unsigned char);
extern int	CheckDBCSTailByte(unsigned char *,unsigned char *);
extern unsigned char	*DBCSstrupr(unsigned char *str);
extern unsigned char	*DBCSstrchr(unsigned char *str,unsigned char c);
#endif


