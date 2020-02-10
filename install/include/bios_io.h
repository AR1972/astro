/***************************************************************************/
/*                                                                         */
/*	BIOS_IO.H																					*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Include file for video definitions and function prototypes              */
/* Include file for keyboard function definitions and prototypes.          */
/*                                                                         */
/* Created 880305 - johnhe                                                 */
/***************************************************************************/

/***************************************************************************/
/* Misc. definitions that should always be defined                         */
/***************************************************************************/
#ifndef     FALSE
   #define     FALSE          0
#endif
#ifndef     TRUE
   #define     TRUE           1
#endif
#ifndef     OFF
   #define     OFF            0
#endif
#ifndef     ON
   #define     ON             1
#endif
#ifndef     EOL
   #define     EOL            '\0'
#endif

/***************************************************************************/
/* Definitons for video and keyboard functions                             */
/***************************************************************************/


#define     MONO_NORMAL    07
#define     MONO_REVERSE   112
#define     FORWARD     1
#define     REVERSE     0
#define     COLOR_80    3
#define     COLOR_40    1
#define     MONO        7
#define     HRES_BW     6


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

/************************************************************************/

struct COUNTRY_INFO
{
	unsigned			DateFmt;
	char				CurrencySym[5];
	char				ThousandSep[2];
	char				DecimalSep[2];
	char				DateSep[2];
	char				TimeSep[2];
	char				Bits;
	char				CurrencyPlaces;
	char				TimeFmt;
	char far 		*CaseMapAddr;
	char				DataSep[2];
	char				Reserved[10];
};

/************************************************************************/

unsigned char     ScreenWidth;
unsigned char     ScreenLength;
unsigned char     DisplayType;

/************************************************************************/

#define		COLOR_AREAS  	9

unsigned char		*uchAreaColors;		/* uchAreaColors[ COLOR_AREAS ] */

enum  AREA_COLORS { vuchHeaderColor, vuchBackGroundColor, vuchTitleColor,
						  vuchStatusColor, vuchRevBackColor, vuchPromptColor,
						  vuchErrorColor, vuchBoxColor, vuchBarColor,
						  vuchGageColor };

#define	GetHeaderColor()  			uchAreaColors[ vuchHeaderColor ]
#define	GetBackGroundColor()			uchAreaColors[ vuchBackGroundColor ]
#define	GetTitleColor()				uchAreaColors[ vuchTitleColor	]
#define	GetStatusColor()				uchAreaColors[ vuchStatusColor ]
#define	GetRevBackColor()				uchAreaColors[ vuchRevBackColor ]
#define	GetPromptColor()				uchAreaColors[ vuchPromptColor ]
#define	GetErrorColor()				uchAreaColors[ vuchErrorColor	]
#define	GetBoxColor()					uchAreaColors[ vuchBoxColor ]
#define	GetBarColor()					uchAreaColors[ vuchBarColor ]
#define	GetGageColor()					uchAreaColors[ vuchGageColor ]

#define	SetHeaderColor( x )			uchAreaColors[ vuchHeaderColor ] = x
#define	SetBackGroundColor( x )		uchAreaColors[ vuchBackGroundColor ] = x
#define	SetTitleColor( x )			uchAreaColors[ vuchTitleColor ] = x
#define	SetStatusColor( x )			uchAreaColors[ vuchStatusColor ] = x
#define	SetRevBackColor( x )			uchAreaColors[ vuchRevBackColor ] = x
#define	SetPromptColor( x )			uchAreaColors[ vuchPromptColor ] = x
#define	SetErrorColor( x )			uchAreaColors[ vuchErrorColor ] = x
#define	SetBoxColor( x )				uchAreaColors[ vuchBoxColor ] = x
#define	SetBarColor( x )				uchAreaColors[ vuchBarColor ] = x
#define	SetGageColor( x )				uchAreaColors[ vuchGageColor ] = x

#define	VideoGetWidth()				ScreenWidth

/************************************************************************/
   /* Video function prototypes */

void	MinVideoInitial	( void );			/* M100 - New function	*/
void  VideoInitial		( void );
int   GetOemDisplayID	( void );
int   VideoIsColor		( void );
void  VideoCls 			( int Color );

int   VideoGetMode		( void );
void  VideoSetMode		( int NewMode );

void  VideoSaveMode		( void );
void  VideoRestoreMode	( void );

void  VideoCursOff		( void );
void  VideoNormalCurs	( void );
void  VideoBlockCurs 	( void );
void  VideoSetRowCol 	( int Row, int Col );
int   VideoGetRowCol 	( void );

int   VideoGetCursSize	( void );
void  VideoSetCursSize	( unsigned	Size );

void  VideoPutChar		( int Character );
void  VideoPutCharRowCol( int Row, int Col, int Character );
void  VideoPutCharAttr	( int Row, int Col, int Character, int Color);

void  VideoPuts			( char *String );
void  VideoPutsRowCol	( int Row, int Col, char *String );
void  VideoPutsAttrRowCol( int Row, int Col, char *String,
	  							int Attribute );

void  VideoDupCharAttr	( int Row, int Col, int Character,
	  						  int Attribute, int Count );
void  VideoVertDupCharAttr( int Row, int Col, int DupChar,
	  							  int Attrib, int DupFactor );
void  VideoDupAttr		( int Row, int Col, int Attribute, int Count );
void 	VideoVertDupAttr	( int Row, int Col, int Attribute, int Count );

void  VideoScrollDn		( int StartRow, int StartCol, int EndRow,
	  						  int EndCol, int Lines, int BlankAttribute );
void  VideoScrollUp		( int StartRow, int StartCol, int EndRow,
	  						  int EndCol, int Lines, int BlankAttribute );

void  VideoGetBlock		( int Row, int Col, char *awSource,
	  						  int WordCount );
void  VideoPutBlock		( int Row, int Col, char *awSource,
											  int WordCount );
#ifdef JAPAN

void  VideoPutDBCharAttr	( int Row, int Col, int Character, int Color);
void  VideoDupDBCharAttr	( int Row, int Col, int Character,
	  						  int Attribute, int Count );
void  VideoVertDupDBCharAttr( int Row, int Col, int DupChar,
										  int Attrib, int DupFactor );
#endif

	/* Keyboard input functions */

int	KbdGetKey				( void );
int	KbdIsWaiting			( void );
int	KbdGetStatus			( void );


	/* DOS Ctrl break check functions */

int	GetBreakSetting		( void );
void	UpdateBreakSetting	( int );

/* */

void	RebootSystem		( void );
unsigned GetMemoryFree		( void );
unsigned GetModelBytes     ( void );
void	DisableAppend			( void );
void	EnableAppend			( void );
void	ToggleAssign			( void );

int	GetDosDataVersion 	( void );
int	IsIBM 					( void );

int	IsNotWindows			( void );

int   GetCountryInfo			( void *Buf34Bytes );

extern void interrupt cdecl far NewInt1b ( void );
extern void interrupt cdecl far NewInt23 ( void );


/************************************************************************/
/* Defines for hardtest.asm 															*/
/* define IDs for machines, displays, and mice			 						*/
/************************************************************************/

#if 0

#define UNKNOWN_MACHINE        0
#define IBMPC_XT_AT            1
#define IBMPS2_25_30           2
#define IBMPS2_50_60_80        3
#define ATT_PC                 4
#define HP_VECTRA              5
#define ZENITH_PC              6

/* Display ID's. These are returned from the GetDisplayID() func */

#define UNKNOWN_DISPLAY        0
#define COMPAQ_PLASMA          1
#define ATT_VDC400_MONO        2
#define IBMCGA                 3
#define HPMULTIMODE            4
#define VGA                    5
#define EGAHIRES_BW            6
#define EGA_COLOR              7
#define EGAHIRES_MONO          8
#define MCGA                   9
#define GAD_8514               10
#define CTVGA                  11
#define HERC_HIRES_MONO        12
#define EGA_64K                13
#define EGA_128K               14
#define EGA_192K               15
#define EGA_256K               16
#define VIDEO7VGA              17
#define VGA_MONO               18

/* Display ID's These are returned from the low level VideoAdapter() func */

#define CGA                    1
#define FAILED                 -1

/* Memory amounts returned from the EGAmemory() func. */

#define MEM_64K                0
#define MEM_128K               1
#define MEM_192K               2
#define MEM_256K               3

/* Mouse ID's returned by GetMouseType().

   n.b., values 1 through 5 should not be changed or reassigned.  They match
   the return values from the mouse driver INT 33h's function 36.
*/

#define UNKNOWN_MOUSE          0    /* Can't tell whether or not a mouse is
                                       installed... */

#define BUS_MOUSE              1    /* Type of mouse installed. */
#define SERIAL_MOUSE           2
#define INPORT_MOUSE           3
#define IBMPS2_MOUSE           4
#define HP_MOUSE               5

#define NO_MOUSE               6    /* No mouse installed. */

#endif


/*
** Display types returned by GetOEMDisplayID().  Equated similarly in
** video.asm.  These display values MUST match the manifest constants in
** lib\bios\video.asm!
*/

#define MONO_DISPLAY          0
#define CGA_DISPLAY           1
#define EGA_DISPLAY           2
#define EGA_MONO_DISPLAY      3
#define VGA_DISPLAY           4
#define VGA_MONO_DISPLAY      5
#define HERC_DISPLAY          6
#define MCGA_DISPLAY          7
#define GAD_8514              8


/* Hardware support functions */

extern unsigned	GetConvMem		( void );

extern unsigned 	IsConvertible	( void );
extern int      	GetCpuType		( void );
extern unsigned 	GetExtMemSize	( void );
extern unsigned 	GetRamDrive		( void ); /* Returns RAM-Drive version	*/
extern unsigned 	GetEmmVersion	( void ); /* Returns EMM386 version		*/
extern unsigned 	GetHimemVer		( void ); /* Returns HIMEM versison		*/
extern unsigned 	GetSmartDrvVer	( void ); /* Returns SMARTDrv version	*/

/************************************************************************/

#ifdef DBCS
/* DBCS function prototypes */

int	CheckLead 	( int Row, int Col );
int	CheckTail 	( int Row, int Col );
#endif

/************************************************************************/

#ifdef JAPAN
/* Kanji graphic characters */

#define		K_THICK_TOP_LEFT		0x84AC
#define		K_THICK_TOP_RIGHT		0x84AD
#define		K_THICK_BOTOM_LEFT		0x84AF
#define		K_THICK_BOTOM_RIGHT		0x84AE
#define		K_THICK_HORIZ_LINE		0x84AA
#define		K_THICK_VERT_LINE		0x84AB
#define		K_THICK_LEFT_TEE		0x84B0
#define		K_THICK_RIGHT_TEE		0x84B2

#define		K_THIN_TOP_LEFT			0x84A1
#define		K_THIN_TOP_RIGHT		0x84A2
#define		K_THIN_BOTOM_LEFT		0x84A4
#define		K_THIN_BOTOM_RIGHT		0x84A3
#define		K_THIN_HORIZ_LINE		0x849F
#define		K_THIN_VERT_LINE		0x84A0
#define		K_THIN_LEFT_TEE			0x84A5
#define		K_THIN_RIGHT_TEE		0x84A7
#endif






