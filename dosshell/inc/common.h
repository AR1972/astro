;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****	common.h - standard include file for all shell modules
**
**   Date      Author	Modification
** --------   --------	------------------------------------------------
**  7/01/89   scottq	created file
**  7/31/89   t-jeffro	added "tmc" to ListBoxData.
*/

/* Standard defines */
typedef unsigned char  uchar;
typedef unsigned short ushort;
typedef unsigned int   uint;
typedef unsigned long  ulong;

#define NULL	0
#define STATIC static
#define DLG_CONST				/* near templates */

/* Following is the minimum version number(major) of DOS that is needed to have
 * the shell to come up.
 */
#define MIN_MAJOR_VERSION 5

#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <fcntl.h>
#include <io.h>
#include <dos.h>
#include <string.h>
#include <cwindows.h>
#include <csdm.h>
#include <csdmtmpl.h>

/* for unreferenced formals -- To avoid warnings from the compiler!! */
#define UnReferenced(x) ((void) x)

#if 1
TMC		FARPUBLIC TmcDoDlgAxAy(VOID *, HCAB, AX, AY);
#define TmcDoDlg(pdlg, hcab)	TmcDoDlgAxAy(pdlg, hcab,(AX) ((PDLG)pdlg)->crcDlg.x,(AY) ((PDLG)pdlg)->crcDlg.y)
#endif

#include <cgraphic.h>
#include <cddefcol.h>
#include <..\iniparse\parse.h>
#include <..\iniparse\symbols.h>
#include <listbox.h>

/**** GLOBALS ****/
extern	      MSG     msg;
extern	      INST    ginst;
extern	      INGD    gingd;
extern	      INDV    gindv;
extern	      BOOL    gisgraph;
extern	      INCH    cinch;
extern	      WND     MainWind;


extern BOOL gisgraph;
extern INST    ginst;
extern INCH    cinch;

extern ListBoxData TreeList[2];
extern ListBoxData FileList[2];

#define SMALLHEIGHT 13	    // How many pixels/char to	user big bitmaps

#define CHECKBOX_LEFTCHAR ((char) '[')
#define CHECKBOX_CHECKCHAR ((char) 'X')
#define CHECKBOX_RIGHTCHAR ((char) ']')


#define isaWhiteOnBlack 21
#define isaBlackOnWhite 22
#define isaTitlebar	23
#define isaDriveicon 24
#define isaDrivebox  25
#define isaSelect  26
#define isaBorders  27
#define isaHotLink  28
#define isaShellMouse  29
#define isaMessagebar  30


/* for exiting with loader */
#define VOODOOOFFSET  (2+2+2)
#define EXITPTROFFSET (0)

#define Shell_Beep() if (gBeeps_On) \
						Beep()
/* All Cancel buttons should map to tmcCancel so escape will work */
#define tmcgroupcancel	tmcCancel
#define tmcpaddcancel	tmcCancel
#define tmcassoccancel	tmcCancel
#define tmcassoc2cancel	tmcCancel
#define tmcattrcancel	tmcCancel
#define tmccopycancel	tmcCancel
#define tmccreatdircancel tmcCancel
#define tmcdelfilescancel tmcCancel
#define tmcdispcancel	  tmcCancel
#define tmcerrcancel	  tmcCancel
#define tmcfileoptscancel tmcCancel
#define tmclocatecancel   tmcCancel
#define tmcproccancel	  tmcCancel
#define tmcproccancel	  tmcCancel
#define tmcrenamecancel   tmcCancel
#define tmcshowinfocancel tmcCancel
#define tmcusercancel	  tmcCancel
#define tmccolor5nothing  tmcCancel
#define tmcpaddCB		  tmcCancel
#define tmcoutofmementer  tmcCancel
#define tmcpasswordCB	  tmcCancel
#define tmchelpEB		  tmcCancel
#define tmcrunCB		  tmcCancel
#define tmcnewCB		  tmcCancel
#define tmcuserCB		  tmcCancel
#define tmccolorcancel tmcCancel
#define tmcscreencancel tmcCancel

/* hid's for dialogs */

#define hidRUN			'A'
#define hidADDGROUP	    'B'
#define hidUSER 	    'C'
#define hidASSOCIAT	    'D'
#define hidATTR0	    'E'
#define hidMOVE 	    'F'
#define hidCREATDIR	    'G'
#define hidDELITEM		'H'
#define hidCRIT 		'I'
#define hidDELFILES	    'J'
#define hidDELGROUP	    'K'
#define hidDISPOPT	    'L'
#define hidERR		    'M'
#define hidFILEOPTS	    'N'
#define hidLOCATE	    'O'
#define hidOUTOFMEM	    'P'
#define hidPROG 	    'Q'
#define hidRENAME	    'R'
#define hidNEW			'S'
#define hidMOUSECONFIRM	'T'
#define hidDELETECONFIRM 'U'
#define hidREPLACECONFIRM 'V'
#define hidCOLOR	    'W'
#define hidSCREEN	    'X'
#define hidSHOWINFO	    'Y'
#define hidHELP 	    'Z'
#define hidPASSWORD	    '1'
#define hidADDDIAL	    '2'
#define hidADDPROG	    '3'
#define hidATTR 		'4'
#define hidASSOC2		'5'
#define hidABOUT		'6'
#define hidADDDIALO		'7'
#define hidADVANCED		'8'

/* These are for dialogs that get used for more than one thing */
#define hidCOPY			'9'
#define hidCHANGEGROUP	'0'
#define hidCHANGEPROG	':'

extern TMC gCurrentTMC;
extern char gErrHelpId ;

extern WORD gFocus ;
#define WhoHasGlobalFocus() (glob.FocusId)

typedef BYTE MyBOOL ;

#define MANIP_MOVE 1
#define MANIP_COPY 2
#define MANIP_RUN  3

/* Following is the data structure used by the screen mode manager to store
 * relevant information about acceptable screen modes for the adpater
 * on which we are running. "Res_Ordinal" is the one field that needs more
 * description: Suppose we had several modes within a given resolution, say
 * we had several HIGH res graphics modes (numlines > 50 in graphics mode),
 * the first one will be 1, the next 2, etc -- used to print the string in
 * the screen mode changing dialog's listbox!
 */	
struct S_Mode {
	BYTE	ModeInd ; 	/* The value of screen mode passed to set screen mode	*/
	BYTE	Res_Ordinal;/* Ordinal value within each resolution					*/
	BYTE	NumLines ;	/* Num. of screen lines supported in this screen mode	*/
	MyBOOL	fIsGraphicsMode ; /* Is this mode a graphics mode? */
} ;


/* Graphics definitions we need that are not defined in cgraphic.h */
#define CWIDTH	 ginst.inft.dxChar
#define CHEIGHT  ginst.inft.dyChar
typedef struct BITMAP
{
    char far *lrgb;
    WORD  cbRow;
    RECT rectBound;
}BITMAP;
