/*******************************************************************************
	CW: Character Windows
	
	winapi.h	: Windows compatible API's. 

	21-May-91 MWP : Created. For inclusion in BROADSWORD only.


******************************************************************************/
#ifdef BROADSWORD



typedef unsigned short	HANDLE;
typedef unsigned short	HMODULE;


// The following code is intended to provide mapping between Windows and
// CW constructs.


// For Windows compatability, we need a HDC type. For CW, DC's don't
// really exist, so we use the PWND instead.


typedef WORD HICON;

typedef WORD HCURSOR;

typedef struct _point
	{
	int	x;
	int	y;
	} POINT;

typedef POINT	*PPOINT;
typedef POINT NEAR	*NPOINT;
typedef POINT FAR		*LPPOINT;


typedef DWORD COLORREF;

#define RGB(r,g,b)  ((DWORD)(((BYTE)(r) | ((WORD)((g) & 0xffL ) << 8) ) | (((DWORD)(BYTE)((b) & 0xffL))<<16)))


#define GetRValue(rgb)	    ((BYTE)(rgb))
#define GetGValue(rgb)	    ((BYTE)(((WORD)(rgb)) >> 8))
#define GetBValue(rgb)	    ((BYTE)((rgb)>>16))

typedef struct tagCREATESTRUCT
  {
    LPSTR	lpCreateParams;
    HANDLE	hInstance;
    HANDLE	hMenu;
    HWND	hwndParent;
    int 	cy;
    int 	cx;
    int 	y;
    int 	x;
    WORD	style;
    LPSTR	lpszName;
    LPSTR	lpszClass;
    WORD		dwExStyle;
  } CREATESTRUCT;

typedef CREATESTRUCT FAR    *LPCREATESTRUCT;

#ifndef NOSCROLL


// Scroll Bar Constants

#define SB_HORZ	0
#define SB_VERT	1
#define SB_CTL		2
#define SB_BOTH	3

#endif


#ifndef NOSYSMETRICS

/* GetSystemMetrics() codes */
#define SM_CXSCREEN	    0
#define SM_CYSCREEN	    1
#define SM_CXVSCROLL	    2
#define SM_CYHSCROLL	    3
#define SM_CYCAPTION	    4
#define SM_CXBORDER	    5
#define SM_CYBORDER	    6
#define SM_CXDLGFRAME	    7
#define SM_CYDLGFRAME	    8
#define SM_CYVTHUMB	    9
#define SM_CXHTHUMB	    10
#define SM_CXICON	    11
#define SM_CYICON	    12
#define SM_CXCURSOR	    13
#define SM_CYCURSOR	    14
#define SM_CYMENU	    15
#define SM_CXFULLSCREEN     16
#define SM_CYFULLSCREEN     17
#define SM_CYKANJIWINDOW    18
#define SM_MOUSEPRESENT     19
#define SM_CYVSCROLL	    20
#define SM_CXHSCROLL	    21
#define SM_DEBUG	    22
#define SM_SWAPBUTTON	    23
#define SM_RESERVED1	    24
#define SM_RESERVED2	    25
#define SM_RESERVED3	    26
#define SM_RESERVED4	    27
#define SM_CXMIN	    28
#define SM_CYMIN	    29
#define SM_CXSIZE	    30
#define SM_CYSIZE	    31
#define SM_CXFRAME	    32
#define SM_CYFRAME	    33
#define SM_CXMINTRACK	    34
#define SM_CYMINTRACK	    35
#define SM_CMETRICS	    36

int FAR PASCAL GetSystemMetrics(int);

#endif /* NOSYSMETRICS */

/************
#define SM_CXSCREEN        80 // (VideoInfo.width)
#define SM_CYSCREEN        25 // (VideoInfo.length)
#define SM_MOUSEPRESENT    TRUE // (IsMouseInstalled())
#define SM_RESTOREDIR      FALSE // (bRestoreDirectory)
#define SM_CXVSCROLL        1
#define SM_CYHSCROLL        1
#define SM_CYCAPTION        1
#define SM_CXBORDER         1
#define SM_CYBORDER         1
#define SM_CXDLGFRAME       1
#define SM_CYDLGFRAME       1
#define SM_CYVTHUMB         1
#define SM_CXHTHUMB         1
#define SM_CXICON           8
#define SM_CYICON           4
#define SM_CXCURSOR         1
#define SM_CYCURSOR         1
#define SM_CYMENU           1
#define SM_CXFULLSCREEN     SM_CXSCREEN
#define SM_CYFULLSCREEN     SM_CYSCREEN-SM_CYMENU
#define SM_CYKANJIWINDOW    SM_CYSCREEN
#define SM_CYVSCROLL        1
#define SM_CXHSCROLL        1
#define SM_DEBUG            0
#define SM_SWAPBUTTON       0
#define SM_CXMIN            1
#define SM_CYMIN            1
#define SM_CXSIZE           1
#define SM_CYSIZE           1
#define SM_CXFRAME          1
#define SM_CYFRAME          1
#define SM_CXMINTRACK       1
#define SM_CYMINTRACK       1
#define SM_RESERVED1	    24
#define SM_RESERVED2	    25
#define SM_RESERVED3	    26
#define SM_RESERVED4	    27
*************/

#define GetSystemMetrics(x)      (x)


#define OLD_API	1

#define CW_USEDEFAULT	0xffff

// A whole wack of Edit stuff.

#define WM_CLEAR			0x800b
#define WM_UNDO			0x800c
#define WM_SYSCOMMAND	0x800d


#ifdef FULL_EDIT

/* Edit Control Styles */
#define ES_LEFT             0x0000
#define ES_CENTER           0x0001
#define ES_RIGHT            0x0002
#define ES_MULTILINE        0x0004
#define ES_UPPERCASE        0x0008
#define ES_LOWERCASE        0x0010

#define ES_PASSWORD         0x0001
#define ES_AUTOVSCROLL      0x0002
#define ES_AUTOHSCROLL      0x0004
#define ES_NOHIDESEL        0x0008
// #define ES_OEMCONVERT       0x0010

/* Edit Control Notification Codes */
#define EN_SETFOCUS	    0x0100
#define EN_KILLFOCUS	    0x0200
#define EN_CHANGE	    0x0300
#define EN_UPDATE	    0x0400
#define EN_ERRSPACE	    0x0500
#define EN_MAXTEXT	    0x0501
#define EN_HSCROLL	    0x0601
#define EN_VSCROLL	    0x0602

#ifndef NOWINMESSAGES

/* Edit Control Messages */
#define EM_GETSEL	   (WM_USER+0)
#define EM_SETSEL	   (WM_USER+1)
#define EM_GETRECT	   (WM_USER+2)
#define EM_SETRECT	   (WM_USER+3)
#define EM_SETRECTNP	   (WM_USER+4)
#define EM_SCROLL	   (WM_USER+5)
#define EM_LINESCROLL	   (WM_USER+6)
#define EM_GETMODIFY	   (WM_USER+8)
#define EM_SETMODIFY	   (WM_USER+9)
#define EM_GETLINECOUNT    (WM_USER+10)
#define EM_LINEINDEX	   (WM_USER+11)
#define EM_SETHANDLE	   (WM_USER+12)
#define EM_GETHANDLE	   (WM_USER+13)
#define EM_GETTHUMB	   (WM_USER+14)
#define EM_LINELENGTH	   (WM_USER+17)
#define EM_REPLACESEL	   (WM_USER+18)
#define EM_SETFONT	   (WM_USER+19)
#define EM_GETLINE	   (WM_USER+20)
#define EM_LIMITTEXT	   (WM_USER+21)
#define EM_CANUNDO	   (WM_USER+22)
#define EM_UNDO 	   (WM_USER+23)
#define EM_FMTLINES	   (WM_USER+24)
#define EM_LINEFROMCHAR    (WM_USER+25)
#define EM_SETWORDBREAK    (WM_USER+26)
#define EM_SETTABSTOPS	   (WM_USER+27)
#define EM_SETPASSWORDCHAR (WM_USER+28)
#define EM_EMPTYUNDOBUFFER (WM_USER+29)
#define EM_MSGMAX          (WM_USER+30)

#endif /* NOWINMESSAGES */


typedef WORD ICH;


typedef struct tagED
	{
	char *	hText;             // Block of text we are editing 
	ICH		cchAlloc;          // Number of chars we have allocated for hText 
	ICH		cchTextMax;        // Max number bytes allowed in edit control 
	ICH		cch;               // Current number of bytes of actual text 
   int      cLines;            // Number of lines of text 
	BYTE		charPasswordChar;  // If non null, display this character instead
										 // of the real text. So that we can implement
										 // hidden text fields. 

	BYTE		chFillDialog;

	HBRUSH	hbrDisabled;		// Disabled color brush. 
	HBRUSH	hbrEbBk;				// Background color brush.
	HBRUSH	hbrHiliteText;		// Hilite color brush


    ICH     ichMinSel;         // Selection extent.  MinSel is first selected                                   char 
    ICH     ichMaxSel;         // MaxSel is first unselected character 
    ICH     ichCaret;          // Caret location. Caret is on left side of                                   char 
    int     iCaretLine;        // The line the caret is on. So that if word
										 //	 wrapping, we can tell if the caret is at end
										 //	 of a line of at beginning of next line... 
    ICH     screenStart;       // Index of left most character displayed on
										 // screen for sl ec and index of top most line
										 // for multiline edit controls 
    int     ichLinesOnScreen;  // Number of lines we can display on screen 
    WORD    xOffset;           // x (horizontal) scroll position in pixels
										 // (for multiline text horizontal scroll bar) 
    WORD    cPasswordCharWidth;// Width of password char 
    HWND    hwnd;              // Window for this edit control 
    RECT    rcFmt;             // Client rectangle 
    HWND    hwndParent;        // Parent of this edit control window 
                               // These vars allow us to automatically scroll
										// when the user holds the mouse at the bottom
										// of the multiline edit control window. 
	 POINT   ptPrevMouse;       // Previous point for the mouse for system timer
    WORD    prevKeys;          // Previous key state for the mouse 
    WORD    fSingle       : 1; // Single line edit control? (or multiline) 
    WORD    fNoRedraw     : 1; // Redraw in response to a change? 
    WORD    fMouseDown    : 1; // Is mouse button down? when moving mouse 
    WORD    fFocus        : 1; // Does ec have the focus ? 
    WORD    fDirty        : 1; // Modify flag for the edit control 
    WORD    fDisabled     : 1; // Window disabled? 
    WORD    fNonPropFont  : 1; // Fixed width font? 
    WORD    fBorder       : 1; // Draw a border? 
    WORD    fAutoVScroll  : 1; // Automatically scroll vertically 
    WORD    fAutoHScroll  : 1; // Automatically scroll horizontally 
    WORD    fNoHideSel    : 1; // Hide sel when we lose focus? 
    WORD    fKanji        : 1;
    WORD    fFmtLines     : 1; // For multiline only. Do we insert CR CR LF at word wrap breaks? 
    WORD    fWrap         : 1; // Do word wrapping? 
    WORD    fCalcLines    : 1; // Recalc ped->chLines array? (recalc line breaks? )
    WORD    fEatNextChar  : 1; // Hack for ALT-NUMPAD stuff with combo boxes.
											// If numlock is up, we want to eat the next
											// character generated by the keyboard driver
											// if user enter num pad ascii value...  
    WORD    fStripCRCRLF:1;     // CRCRLFs have been added to text. Strip them
											// before doing any internal edit control
											// stuff 
    WORD    fInDialogBox:1;     // True if the ml edit control is in a dialog
											// box and we have to specially treat TABS and ENTER 
    int     *chLines;          // index of the start of each line 
    WORD    format;            // Left, center, or right justify multiline	text
    LPSTR   (FAR *lpfnNextWord)(); // Next word function 
    ICH     maxPixelWidth;     // Width (in pixels) of longest line 
    WORD    undoType;          // Current type of undo we support 
    HANDLE  hDeletedText;      // Handle to text which has been deleted (for undo )
    ICH     ichDeleted;        // Starting index from which text was deleted
    ICH     cchDeleted;        // Count of deleted characters in buffer 
    ICH     ichInsStart;       // Starting index from which text was inserted 
    ICH     ichInsEnd;         // Ending index of inserted text 
//    HANDLE  hFont;             // Handle to the font for this edit control.
    int     aveCharWidth;      // Ave width of a character in the hFont 
    int     lineHeight;        // Height of a line in the hFont 
    int     charOverhang;      // Overhang associated with the hFont      
    int     cxSysCharWidth;    // System font ave width 
    int     cySysCharHeight;   // System font height 
    HWND    listboxHwnd;       // ListBox hwnd. Non null if we are a combo box
    int     *pTabStops;	       // Points to an array of tab stops; First
										// element contains the number of elements in
										// the array 
    HANDLE  charWidthBuffer;  
	} ED;

typedef ED *PED;


#endif //FULL_EDIT


#define GetDC(hWnd)	(hWnd)

#define ReleaseDC(hWnd, hDC)

#define UpdateWindow(pwnd)  XSendMessage((pwnd), WM_PAINT, 0, 0L )

VOID	FARPUBLIC InvalidateRect	( PVOID, NPRRC );			/*OPTIONAL*/

VOID FARPUBLIC GetClientRect(PVOID, RRC *);					/*OPTIONAL*/

VOID	FARPUBLIC SiblingToTop		(PVOID, BOOL);          /*OPTIONAL*/	

PWND	FARPUBLIC GetTopSibling	(PVOID);							/*OPTIONAL*/

BOOL	FARPUBLIC IsTopSibling	(PVOID);							/*OPTIONAL*/

#define GetWindowRect(hwnd, parc )	*(parc) = (hwnd)->arcWindow	

#define GetUpdateRect(hwnd, prrc, bErase )	*(prrc) = (hwnd)->rrcInvalid

VOID	FARPUBLIC FillRect	(PVOID, NPRRC, ACHAR, WORD);/*OPTIONAL*/

#define GetDesktopWindow()	pwndDesktop

PWND FARPUBLIC GetActiveWindow ( VOID );/*OPTIONAL*/

// winget.c
PWND FARPUBLIC GetWindow ( PVOID, WORD );
PWND FARPUBLIC GetTopWindow ( PVOID );

// GetWindow() Constants 

#define GW_HWNDFIRST	0
#define GW_HWNDLAST	1
#define GW_HWNDNEXT	2
#define GW_HWNDPREV	3
#define GW_OWNER		4
#define GW_CHILD		5

//wincoord.c
VOID FARPUBLIC ScreenToClient ( PVOID, NPARC, NPRRC );
VOID FARPUBLIC ClientToScreen ( PVOID, NPARC, NPRRC );

// winenum.c
BOOL FARPUBLIC EnumWindows ( PFFN, DWORD );
BOOL FARPUBLIC EnumChildWindows ( PVOID, PFFN, DWORD );

// winpos.c
VOID FARPUBLIC CloseWindow ( PVOID );

//winwhere.c
HWND FARPUBLIC WindowFromPoint ( AX, AY );
VOID	FARPUBLIC SetWindowPos ( PWND,HWND,AX,AY,BYTE,BYTE,WORD );

#ifndef NOCOLOR


// Color Types 
#define CTLCOLOR_MSGBOX		0
#define CTLCOLOR_EDIT		1
#define CTLCOLOR_LISTBOX	2
#define CTLCOLOR_BTN		3
#define CTLCOLOR_DLG		4
#define CTLCOLOR_SCROLLBAR	5
#define CTLCOLOR_STATIC		6
#define CTLCOLOR_MAX		8     /* three bits max */


//wincolor.c
BOOL FARPUBLIC SetSysColors ( WORD,WORD FAR *,WORD FAR * );
DWORD FARPUBLIC GetSysColor ( WORD );

// Defines for colors. These are Windows compatible, and we map
// them to internal isa defines within uisa.h ( ugh. )

#define COLOR_SCROLLBAR			0
#define COLOR_BACKGROUND		1
#define COLOR_ACTIVECAPTION	2
#define COLOR_INACTIVECAPTION	3
#define COLOR_MENU				4
#define COLOR_WINDOW				5
#define COLOR_WINDOWFRAME		6	
#define COLOR_MENUTEXT			7
#define COLOR_WINDOWTEXT		8
#define COLOR_CAPTIONTEXT		9
#define COLOR_ACTIVEBORDER		10
#define COLOR_INACTIVEBORDER	11
#define COLOR_APPWORKSPACE		12
#define COLOR_HIGHLIGHT			13
#define COLOR_HIGHLIGHTTEXT	14
#define COLOR_BTNFACE			15
#define COLOR_BTNSHADOW			16
#define COLOR_GRAYTEXT			17
#define COLOR_BTNTEXT			18
#define COLOR_ENDCOLORS 		COLOR_BTNTEXT

#endif

//sysmenu.c

// VOID** FARPUBLIC GetSystemMenu( PVOID, BOOL );

BOOL FARPUBLIC OpenSystemMenu ( PVOID );


//winmenu.c
#define	MF_BYPOSITION	0x0001
#define	MF_BYCOMMAND	0x0002
#define	MF_CHECKED		0x0004
#define	MF_UNCHECKED	0x0008
#define	MF_ENABLED		0x0010
#define	MF_DISABLED		0x0020
#define	MF_HILITE		0x0040
#define	MF_UNHILITE		0x0080


//wincaret.c

VOID FARPUBLIC ShowCaret ( PVOID );
VOID FARPUBLIC	SetCaretPos ( BYTE, BYTE );

VOID FARPUBLIC SetCaretBlinkTime ( WORD );
VOID FARPUBLIC	HideCaret ( PVOID );
VOID FARPUBLIC	GetCaretPos ( VOID * );
WORD FARPUBLIC	GetCaretBlinkTime (VOID );
VOID FARPUBLIC	DestroyCaret (VOID );
VOID FARPUBLIC	CreateCaret ( PVOID, HANDLE, WORD, WORD );

//wintext.c

DWORD FARPUBLIC GetTextExtent ( HDC, LPSTR, int );
int FARPUBLIC DrawText ( HDC, LPSTR, int, LPRECT, WORD );


// DrawText() Format Flags 

#define DT_TOP				0x0000
#define DT_LEFT			0x0000
#define DT_CENTER			0x0001
#define DT_RIGHT			0x0002
#define DT_VCENTER		0x0004
#define DT_BOTTOM			0x0008
#define DT_WORDBREAK		0x0010
#define DT_SINGLELINE	0x0020
#define DT_EXPANDTABS	0x0040
#define DT_TABSTOP		0x0080
#define DT_NOCLIP			0x0100
#define DT_EXTERNALLEADING  0x0200
#define DT_CALCRECT		0x0400
#define DT_NOPREFIX		0x0800
#define DT_INTERNAL		0x1000



typedef struct tagTEXTMETRIC
  {
    int 	tmHeight;
    int 	tmAscent;
    int 	tmDescent;
    int 	tmInternalLeading;
    int 	tmExternalLeading;
    int 	tmAveCharWidth;
    int 	tmMaxCharWidth;
    int 	tmWeight;
    BYTE	tmItalic;
    BYTE	tmUnderlined;
    BYTE	tmStruckOut;
    BYTE	tmFirstChar;
    BYTE	tmLastChar;
    BYTE	tmDefaultChar;
    BYTE	tmBreakChar;
    BYTE	tmPitchAndFamily;
    BYTE	tmCharSet;
    int 	tmOverhang;
    int 	tmDigitizedAspectX;
    int 	tmDigitizedAspectY;
  } TEXTMETRIC;

typedef TEXTMETRIC	    *PTEXTMETRIC;
typedef TEXTMETRIC NEAR     *NPTEXTMETRIC;
typedef TEXTMETRIC FAR	    *LPTEXTMETRIC;

BOOL FARPUBLIC GetTextMetrics ( HDC, LPTEXTMETRIC );

//winclip.c

BOOL FARPUBLIC	OpenClipboard ( PVOID );
HWND FARPUBLIC	GetClipboardOwner ( VOID );
HWND FARPUBLIC	GetClipboardViewer ( VOID );
HANDLE FARPUBLIC	GetClipboardData ( WORD );
BOOL FARPUBLIC	CloseClipboard ( VOID );

#ifndef NOCLIPBOARD

// Predefined Clipboard Formats 
#define CF_TEXT 	    1
#define CF_BITMAP	    2
#define CF_METAFILEPICT     3
#define CF_SYLK 	    4
#define CF_DIF		    5
#define CF_TIFF 	    6
#define CF_OEMTEXT	    7
#define CF_DIB		    8
#define CF_PALETTE	    9

#define CF_OWNERDISPLAY     0x0080
#define CF_DSPTEXT	    0x0081
#define CF_DSPBITMAP	    0x0082
#define CF_DSPMETAFILEPICT  0x0083

/* "Private" formats don't get GlobalFree()'d */
#define CF_PRIVATEFIRST     0x0200
#define CF_PRIVATELAST	    0x02FF

/* "GDIOBJ" formats do get DeleteObject()'d */
#define CF_GDIOBJFIRST	    0x0300
#define CF_GDIOBJLAST	    0x03FF

#endif // NOCLIPBOARD 

//winrect.c
VOID FARPUBLIC InflateRect ( LPRECT, int, int );
VOID FARPUBLIC OffsetRect ( LPRECT, int, int );

//winscroll.c
VOID FARPUBLIC ScrollWindow(PVOID, int, int, LPRECT, LPRECT );



// The idStaticPath parameter to DlgDirList can have the following values
// ORed if the list box should show other details of the files along with
// the name of the files;

#define LBD_UPPERCASE  	0x8001	// Should the file name be in upper case 
#define LBD_SIZE			0x8002	// Should the file size be shown
#define LBD_DATE			0x8004	// Date stamp of the file to be shown ?
#define LBD_TIME			0x8008	// Time stamp of the file to be shown ?
#define LBD_ATTRIBUTE	0x8010	// The dos attributes of the file ?
#define LBD_FULLDETAILS 0x801E	// Name, size, date and time 
#define LBD_SENDDETAILS 0x8020	// In DlgDirSelect(), along with file name
											// all other details also will be returned

// DlgDirList flags values 
#define DDL_NORMAL	    0x0000
#define DDL_READWRITE    0x0001
#define DDL_READONLY	    0x0002
#define DDL_HIDDEN	    0x0004
#define DDL_SYSTEM	    0x0008
#define DDL_DIRECTORY    0x0010
#define DDL_ARCHIVE	    0x0020

#define DDL_POSTMSGS	    0x2000
#define DDL_DRIVES	    0x4000
#define DDL_EXCLUSIVE    0x8000
#define DDL_VALID			 0xe03f	/* ;Internal */


// A lot of these typedef's and macros should be made globally available.

typedef char *PSTR;

#define MAKEPOINT(l)		(*((POINT FAR *)&(l)))

#define lstrlen(sz) fstrlen((char far *)(sz))

#define lstrcpy(dest,src) fstrcpy((LPSTR)(dest), (LPSTR)(src))

typedef int near *PINT;

#define LCopyStruct(lpSrc,lpDest,cch) bltbytex(lpSrc,lpDest,cch)

#define SwapHandle(hHandle) 

DWORD FARPUBLIC SendDlgItemMessage ( HWND,WORD,WORD,WORD,DWORD );

#ifdef OLD_API
#include "old_api.h"
#endif

#endif //BROADSWORD

