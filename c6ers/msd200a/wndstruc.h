/*******************************************************************************

	This header contains various CW window structure definitions, and for
	the early versions of CW, the macros for accessing all the varius extra
	words of information that existed beyond the basic WND structure.

	Created: 3-January-1991 MWP

	 6-Feb-91 MWP : Moved function prototypes for Get/SetWindowWord to uwindows.h
	11-Feb-91 MWP : Added wDropState to DROP_WND struc.
	14-Feb-91 MWP : Added definition for dialog file processing windows.
	15-Feb-91 MWP : Added pwndAccel variable to static window structure. This
						 allows us to maintain an accelerator for static text, and
						 not have the control that is to get the focus be defined
						 exactly after the static control within dialogs. 
						 BRILLIANT!

	 5-Mar-91 MWP : Re-sync the EDIT and PEDIT struc definitions. They were not
						 totally aligned, causing code in sedit to overwrite the end
						 of a pwnd allocation.

	 7-Mar-91 MWP : Create a DESKTOP window struc. 	

	11-Mar-91 MWP : Added wFrameHeight to Frame window def.

	 1-Apr-91 MWP : Added hidHelp parameter to dialog window struc.

	 3-Jun-91 MWP : Fixed incorrect casting of drop down window macros.
******************************************************************************/

/***BEGIN_PUBLIC***/

// Control Procedure 
typedef WORD	(FARPUBLIC *PWFN_CTL)(WORD, char *, WORD, WORD, WORD, WORD);


// BLADE enhanced version.

#ifdef BLADE

#ifdef BROADSWORD

#ifdef FULL_EDIT

#ifdef BASED
	typedef struct _winwnded _based(pWndSeg)  WND_WINEDIT;
#else
	typedef struct _winwnded WND_WINEDIT;
#endif

typedef WND_WINEDIT *PWND_WINEDIT;


typedef struct _winwnded
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	PED ped;
	};

#define GetEditPed(pwnd) (((PWND_WINEDIT)pwnd)->ped)
#define SetEditPed(pwnd, ped) ( ((PWND_WINEDIT)pwnd)->ped = ped)

#endif

#endif


#ifdef BASED
	typedef struct _wndmle _based(pWndSeg) WND_MLE;
#else
	typedef struct _wndmle WND_MLE;
#endif

typedef WND_MLE *PWND_MLE;

typedef struct _wndmle
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;
#endif
	WORD	padding;
	WORD  pEF_Structure;
	};

#ifdef BASED
	typedef struct _wnddesktop _based(pWndSeg) WND_DESKTOP;
#else
	typedef struct _wnddesktop WND_DESKTOP;
#endif

typedef WND_DESKTOP *PWND_DESKTOP;

typedef struct _wnddesktop
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	BYTE  bFill;
	WORD  isaDesktop;
	};



#ifdef BASED
	typedef struct _wndgeneric _based(pWndSeg) WND_GEN;
#else
	typedef struct _wndgeneric WND_GEN;
#endif

typedef WND_GEN *PWND_GEN;

typedef struct _wndgeneric
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD aclDialog;
 	char *szDialog;	
 	WORD cchDialog;			//	 6 bytes
	};



#ifdef BASED
	typedef struct _wndgenpic _based(pWndSeg) WND_GENPIC;
#else
	typedef struct _wndgeneric WND_GENPIC;
#endif

typedef WND_GENPIC *PWND_GENPIC;

typedef struct _wndgenpic
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD aclDialog;
 	char *szDialog;	
 	WORD cchDialog;			//	 6 bytes
	PWFN_CTL pfnGenProc;
 	WORD wParam;				//	 6 bytes
	};

// General window stuff

// #define	PwfnCtlGeneral(pwnd)	(*((PWFN_CTL *) &pwnd->rgwExtra[cwExtraMin]))
#define	PwfnCtlGeneral(pwnd)	((PWND_GENPIC)pwnd)->pfnGenProc
#define	WParamGeneral(pwnd)	((PWND_GENPIC)pwnd)->wParam


#define SetAccelerator(pwnd, Position, szText) \
	 ((PWND_GEN)(pwnd))->aclDialog = (((BYTE)((Position) - 1)) << 8) + \
					    *((unsigned char *)(szText) + ((Position) - 1))

#ifdef BASED
	typedef struct _wndgroup _based(pWndSeg) WND_GROUP;
#else
	typedef struct _wndgroup WND_GROUP;
#endif

typedef WND_GROUP *PWND_GROUP;

typedef struct _wndgroup
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;
#endif
	WORD aclDialog;
 	char *szDialog;
	PWND pwndAccel;	
	};


#ifdef BASED
	typedef struct _wndstatic _based(pWndSeg) WND_STATIC;
#else
	typedef struct _wndstatic WND_STATIC;
#endif

typedef WND_STATIC *PWND_STATIC;

typedef struct _wndstatic
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;						// 31 bytes
#endif
	WORD aclDialog;
 	char *szDialog;
	PWND pwndAccel;	
	};

#ifdef BASED
	typedef struct _dlgwnd _based(pWndSeg)  WND_DLG;
#else
	typedef struct _dlgwnd WND_DLG;
#endif

typedef WND_DLG *PWND_DLG;

typedef struct _dlgwnd
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD aclDialog;
 	char *szDialog;	
 	WORD wParamEnd;
	VOID *pmbsDialog;
	PLFN pfnUserProc;			// 12 bytes
 	WORD hidHelp;
	};

// so that rspAppIdle can determine where the current msg box dialog is 
extern PWND_DLG	pwndDlg;		// Again, app should never use 


typedef struct _drop DROP;
typedef DROP *PDROP;

typedef struct _Drop {
		WORD Dummy;
		WORD wDropFlags;
		PWND pwndDropListBox;
		PWND pwndDropButton;
		PWND pwndDropEdit;
		PLFN_WNDPROC pfnDropListBoxProc;
		PLFN_WNDPROC pfnDropEditProc;
	};


#ifdef BASED
	typedef struct _wnddrop _based(pWndSeg)  WND_DROP;
#else
	typedef struct _wnddrop WND_DROP;
#endif

typedef WND_DROP *PWND_DROP;

typedef struct _wnddrop	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD wDropState;
	WORD wDropFlags;			// TRUE ( 1 ) = Combo, FALSE ( 0 ) = Plain
	PWND pwndDropListBox;
	PWND pwndDropButton;
	PWND pwndDropEdit;
	PLFN_WNDPROC pfnDropListBoxProc;
	PLFN_WNDPROC pfnDropEditProc;		// 20 bytes
	};

#define PwndDropListBox(pwnd) ( (PWND_LIST)( (PWND_DROP)pwnd )->pwndDropListBox ) 
#define PwndDropButton(pwnd) 	( (PWND_BTN)( (PWND_DROP)pwnd )->pwndDropButton )
#define PwndDropEdit(pwnd)		( (PWND_EDIT)( (PWND_DROP)pwnd )->pwndDropEdit)
#define PfnDropListBoxProc(pwnd) ( ( (PWND_DROP)pwnd )->pfnDropListBoxProc)
#define PfnDropEditProc(pwnd)		( ( (PWND_DROP)pwnd )->pfnDropEditProc)


#ifdef BASED
	typedef struct _wnddroplist _based(pWndSeg)  WND_DROP_LIST;
#else
	typedef struct _wnddroplist WND_DROP_LIST;
#endif

typedef WND_DROP_LIST *PWND_DROP_LIST;

typedef struct _wnddroplist	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	PWFN_CTL pfnLBFtn;
	WORD wParam;
	WORD wUnknown;
	WORD iszTopLb; 			// first item in Display 
	WORD cszLb;					// # of strings in list in list 
	WORD iszCurLb;				// currently selected item 
	WORD hmemMpiszoffLb;		// array of offsets in string buffer
	WORD hmemGpszLb;			// string buffer pool 
	WORD offLb; 				// next string buffer pointer 
	WORD offMaxLb; 			// size of string buffer 
	WORD iszMacLb; 			// max isz before index buf is grown
	WORD fSelectedLb;			// do we have a selection ? 
	WORD isaColor;				// colour of the listbox 
	WORD isaHiliteColor;		// colour of the listbox hilite
	WORD ctickRepLb;			// scrolling rate 
	WORD drxItemLb;			// width of a single item (add 1 for space) 
	WORD citemWidthLb;		// max # of items wide 	  // 36 bytes
	PWND_DROP pwndDrop;
	};



#ifdef BASED
	typedef struct _wnddlgdir _based(pWndSeg)  WND_DLGDIR;
#else
	typedef struct _wnddlgdir WND_DLGDIR;
#endif

typedef WND_DLGDIR *PWND_DLGDIR;

typedef struct _wnddlgdir	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	PWND	pwndDirDrivesBox;
	PWND	pwndFilesBox;
	PWND	pwndDirEdit;
	PWND  pwndDirStatic;
	};


#define PwndDlgDirEdit(pwnd)	( ( (PWND_DLGDIR)pwnd )->pwndDirEdit)
#define PwndDirStatic(pwnd)	( ( (PWND_DLGDIR)pwnd )->pwndDirStatic)


typedef struct _Scroll SCROLL;
typedef SCROLL *PSCROLL;

typedef struct _Scroll {
	WORD ctickRepSb;	 	// # of ticks to repeat
	short ptCurSb; 			// current position on scroll line
	short ptMinSb; 			// minimum position on scroll line
	short ptMaxSb; 			// end position on scroll line 
	BYTE ptElevatorSb;		// elevator position
	BYTE dummy; 				// word alignment
	};


#ifdef BASED
	typedef struct _wndscroll _based(pWndSeg)  WND_SCROLL;
#else
	typedef struct _wndscroll WND_SCROLL;
#endif

typedef WND_SCROLL *PWND_SCROLL;

typedef struct _wndscroll
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;			
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD ctickRepSb;	 		// # of ticks to repeat
	short ptCurSb; 				// current position on scroll line
	short ptMinSb; 				// minimum position on scroll line
	short ptMaxSb; 				// end position on scroll line 
	short ptElevatorSb;		// elevator position (lower byte)	// 10 bytes
	};

typedef struct _List LISTBOX;
typedef LISTBOX *PLISTBOX;


typedef struct _List {
	PWFN_CTL pfnLBFtn;
	WORD wParam;
	WORD wUnknown;
	WORD iszTopLb; 			// first item in Display 
	WORD cszLb;					// # of strings in list in list 
	WORD iszCurLb;				// currently selected item 
	WORD hmemMpiszoffLb;		// array of offsets in string buffer
	WORD hmemGpszLb;			// string buffer pool 
	WORD offLb; 				// next string buffer pointer 
	WORD offMaxLb; 			// size of string buffer 
	WORD iszMacLb;				// max isz before index buf is grown
	WORD fSelectedLb;			// do we have a selection ? 
	WORD isaColor;				//	colour of the listbox 
	WORD isaHiliteColor;		// colour of the listbox hilite
	WORD ctickRepLb;			// scrolling rate 
	WORD drxItemLb;			// width of a single item (add 1 for space) 
	WORD citemWidthLb;		// max # of items wide 
	};


#ifdef BASED
	typedef struct _wndlist _based(pWndSeg)  WND_LIST;
#else
	typedef struct _wndlist WND_LIST;
#endif

typedef WND_LIST *PWND_LIST;

typedef struct _wndlist	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	PWFN_CTL pfnLBFtn;
	WORD wParam;
	WORD wUnknown;
	WORD iszTopLb; 			// first item in Display 
	WORD cszLb;					// # of strings in list in list 
	WORD iszCurLb;				// currently selected item 
	WORD hmemMpiszoffLb;		// array of offsets in string buffer
	WORD hmemGpszLb;			// string buffer pool 
	WORD offLb; 				// next string buffer pointer 
	WORD offMaxLb; 			// size of string buffer 
	WORD iszMacLb; 			// max isz before index buf is grown
	WORD fSelectedLb;			// do we have a selection ? 
	WORD isaColor;				// colour of the listbox 
	WORD isaHiliteColor;		// colour of the listbox hilite
	WORD ctickRepLb;			// scrolling rate 
	WORD drxItemLb;			// width of a single item (add 1 for space) 
	WORD citemWidthLb;		// max # of items wide 	  // 36 bytes
	};


#define	PwfnCtlLb(pwnd)	(((PWND_LIST)pwnd)->pfnLBFtn)
#define	WParamLb(pwnd)		(((PWND_LIST)pwnd)->wParam)

// special listbox window specific isa's 

#define IsaListbox(pwnd)			(((PWND_LIST)pwnd)->isaColor)
#define IsaHiliteListbox(pwnd)	(((PWND_LIST)pwnd)->isaHiliteColor)

typedef struct _Edit EDIT;
typedef EDIT *PEDIT;

typedef struct _Edit {
	WORD aclDialog;
 	char *szDialog;	
 	WORD cchDialog;
	WORD isaEb;					// color 
	WORD isaSelEb;				// selected color 
	BYTE chFillDialog;			// fill char for trailing spaces 
	WORD ichMacEb;				// last character in edit buffer 
	WORD ichLeftEb;				// leftmost character displayed 
	WORD ichCursorEb;			// current cursor position, to the left of  insertion point 
	WORD ichSelEb;				// start of selection 
	WORD fNoBracketEb;			// don't show brackets ?? 
	WORD wEb;						// random flags 
	WORD cchMaxEb;				// for fixed length edit items 
	WORD isaDisabledEb;			// disabled color 
	char *szWildCardEb;			// auxilary edit field 
	BOOL fCWAllocated;
	BOOL Dummy;
};

#ifdef BASED
	typedef struct _wnded _based(pWndSeg)  WND_EDIT;
#else
	typedef struct _wnded WND_EDIT;
#endif

typedef WND_EDIT *PWND_EDIT;

typedef struct _wnded
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD aclDialog;
 	char *szDialog;	
 	WORD cchDialog;
	WORD isaEb;					// color 
	WORD isaSelEb;				// selected color 
	BYTE chFillDialog;		// fill char for trailing spaces 
	WORD ichMacEb;				// last character in edit buffer 
	WORD ichLeftEb;			// leftmost character displayed 
	WORD ichCursorEb;			// current cursor position, to the left of  insertion point 
	WORD ichSelEb;				// start of selection 
	WORD fNoBracketEb;		// don't show brackets ?? 
	WORD wEb;					// random flags 
	WORD cchMaxEb;				// for fixed length edit items 
	WORD isaDisabledEb;		// disabled color 
	char *szWildCardEb;		// auxilary edit field 
	BOOL fCWAllocated;
	BOOL Dummy;					// 34 bytes
	};


#define ShowEditBracket(pwnd, fNoBracket ) (((PWND_EDIT)pwnd)->fNoBracketEb = !(fNoBracket)	)
#define SetEditFillCharacter(pwnd, ch)		 (((PWND_EDIT)pwnd)->chFillDialog = (BYTE)(ch) )
#define SetEditDisabledIsa(pwnd, isa)		 (((PWND_EDIT)pwnd)->isaDisabledEb = (isa) )
#define SetEditSelectedIsa(pwnd, isa)		 (((PWND_EDIT)pwnd)->isaSelEb = (isa) )
#define SetEditIsa(pwnd, isa)					 (((PWND_EDIT)pwnd)->isaEb = (isa) )

// special edit window specific isa's 
#define IsaEdit(pwnd)			(((PWND_EDIT)pwnd)->isaEb)
#define IsaSelEdit(pwnd)		(((PWND_EDIT)pwnd)->isaSelEb )
#define IsaDisabledEdit(pwnd)	(((PWND_EDIT)pwnd)->isaDisabledEb)

#ifdef BASED
	typedef struct _btnwnd  _based(pWndSeg) WND_BTN;
#else
	typedef struct _btnwnd WND_BTN;
#endif

typedef WND_BTN *PWND_BTN;

typedef struct _btnwnd
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD aclDialog;
 	char *szDialog;	
 	BYTE cchDialog;
	BYTE wButton;
#ifndef WIN_BTN
	PWND_BTN pwndButtonNext;		//	 8 bytes
#endif
	};


#ifndef WIN_BTN
// walking the list of radio buttons 
#define	PwndButtonNext(pwnd) ((PWND_BTN) (pwnd)->pwndButtonNext)

// is it a (radio) button? 
#define	FPwndIsButton(pwnd) (((pwnd)->style & WS_TYPE) == WS_BUTTON)
#define	FButtonIsRadio(pwnd) (((pwnd)->style & WS_SUBSTYLE) == BS_RADIOBUTTON)

// bits :
//  0..1 '=> bst (button state)
//  2 => fButtonDown
//  3 => first in group
//
// button states 


#ifdef BLADE

#define bstOff		(BYTE)0
#define bstOn		(BYTE)1
#define bstGreyed	(BYTE)2 
#define bstMax		(BYTE)3


// Macros for bit accesses 


#define BstOfWnd(pwnd) ((pwnd)->wButton & 3)

#define SetWndBst(pwnd, bst) (pwnd)->wButton = ((pwnd)->wButton & ~((BYTE)3)) | (bst) 

#define FButtonDown(pwnd) ((pwnd)->wButton & 4)

#define SetFButtonDown(pwnd, fDown) \
	{if (fDown) (pwnd)->wButton |= (BYTE)4; \
	  else (pwnd)->wButton &= ~(BYTE)4;}

#define FFirstButton(pwnd) ((pwnd)->wButton & 8)

#define SetFFirstButton(pwnd) (pwnd)->wButton = ((pwnd)->wButton |= (BYTE)8)

#define SetNextGroupButton(pwnd, pwndNext )  (pwnd)->pwndButtonNext =(pwndNext)


#else

#define bstOff		0
#define bstOn		1
#define bstGreyed	2 
#define bstMax		3


// Macros for bit accesses 


#define BstOfWnd(pwnd) ((pwnd)->wButton & 3)

#define SetWndBst(pwnd, bst) (pwnd)->wButton = ((pwnd)->wButton & ~(3)) | (bst) 

#define FButtonDown(pwnd) ((pwnd)->wButton & 4)

#define SetFButtonDown(pwnd, fDown) \
	{if (fDown) (pwnd)->wButton |= 4; \
	  else (pwnd)->wButton &= ~4;}

#define FFirstButton(pwnd) ((pwnd)->wButton & 8)

#define SetFFirstButton(pwnd) (pwnd)->wButton = ((pwnd)->wButton |= 8)

#define SetNextGroupButton(pwnd, pwndNext )  (pwnd)->pwndButtonNext =(pwndNext)

#endif //BLADE


#endif //WIN_BTN


#ifdef BASED
	typedef struct _framewnd _based(pWndSeg)  WND_FRAME;
#else
	typedef struct _framewnd WND_FRAME;
#endif

typedef WND_FRAME *PWND_FRAME;

typedef struct _framewnd
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;				// 31 bytes
#endif
	BOOL  fWinActive;
	char *szFrameText;
	VOID **hFrameMenu;
	WORD 	 axyLastPos;
	WORD 	 rxyLastPos;
	BOOL	 fZoomFrame;
	ARC	 arcMenuArc;				//	16 bytes
	WORD	 wFrameHeight;
	PLFN_WNDPROC pfnClientWndProc;
	};

#else	 // Non-Blade stuff follows

typedef PWND PWND_MLE;
typedef PWND PWND_SCROLL;
typedef PWND PWND_EDIT;
typedef PWND PWND_BTN;
typedef PWND PWND_DLG;
typedef PWND PWND_GEN;
typedef PWND PWND_STATIC;
typedef PWND PWND_LIST;
typedef PWND PWND_DROP_LIST;
typedef PWND PWND_DROP;
typedef PWND PWND_FRAME;
typedef PWND PWND_DESKTOP;
typedef PWND PWND_GROUP;

#define	GetWindowWord(pwnd, iw) 	((pwnd)->rgwExtra[(iw)])
#define	SetWindowWord(pwnd, iw, w) {(pwnd)->rgwExtra[(iw)] = (w);}


// special edit window specific isa's 

#define IsaEdit(pwnd)			((pwnd)->rgwExtra[3])
#define IsaSelEdit(pwnd)		((pwnd)->rgwExtra[4])
#define IsaDisabledEdit(pwnd)	((pwnd)->rgwExtra[13])


// Common portion of all dialog windows 

#define	cwExtraMin	1		// at least 1 field for all items 
									// not used for dialog box 

#define	aclDialog	rgwExtra[0]	// dialog accelerator 

// Text info for : edit, static and button controls 

#define cwExtraText	(cwExtraMin+2)	// 2 more for text controls 
#define szDialog	rgwExtra[1]			// also Dialog Box caption 
#define cchDialog	rgwExtra[2]			// edit, static & buttons 

// flag indicating whether EndDialog has been
// called.  Used to prevent multiple calls
// to PostQuitMessage 

#define wParamEnd		rgwExtra[2]


// sizes of rgwExtra for the various controls 

#define cwExtraDialog	(cwExtraMin+2)		// szDialog + wParamEnd 

#define cwExtraSdmDlg	(cwExtraDialog+((sizeof(SDS)+1)/2))	// ... + sds 

#define cwExtraMsgBox	(cwExtraDialog+1) // ... + pmbs 
#define cwExtraStatic	(cwExtraText)		// simple text 
#define cwExtraButton	(cwExtraText+2)	// text + button state + next radio
#define cwExtraGroup    (cwExtraText+2)	// text 


#define cwExtraEdit		(cwExtraText+11)	// see sedit.h for details 
#define cwExtraEditAux	(cwExtraEdit+1)	
#define cwExtraGeneral	(cwExtraMin+3)		// see general.h for details 

#ifndef LISTBOX_HORIZ
	#define cwExtraListBox	(cwExtraMin+15)	// see _listbox.h for details 
#else
	#define cwExtraListBox	(cwExtraMin+17)	// see _listbox.h for details 
#endif


#define cwExtraDropDown		(cwExtraMin+9)		// 10
#define cwExtraDropHolder 	(cwExtraMin+1)		// 2
#define cwExtraDropButton 	(cwExtraMin)		// 1


#define wButton 	rgwExtra[cwExtraText]

// flag word for button control 

#define	pwndButtonNext	rgwExtra[cwExtraText+1]	 // next radio button 

//#if cwExtraButton != cwExtraText+2
//.....
//#endif

// walking the list of radio buttons 
#define	PwndButtonNext(pwnd) ((PWND_BTN) (pwnd)->pwndButtonNext)

// is it a (radio) button? 
#define	FPwndIsButton(pwnd) (((pwnd)->style & WS_TYPE) == WS_BUTTON)
#define	FButtonIsRadio(pwnd) (((pwnd)->style & WS_SUBSTYLE) == BS_RADIOBUTTON)

// bits :
//  0..1 '=> bst (button state)
//  2 => fButtonDown
//  3 => first in group
//
// button states 



#define bstOff		0
#define bstOn		1
#define bstGreyed	2 
#define bstMax		3


// Macros for bit accesses 


#define BstOfWnd(pwnd) ((pwnd)->wButton & 3)

#define SetWndBst(pwnd, bst) (pwnd)->wButton = ((pwnd)->wButton & ~(3)) | (bst) 

#define FButtonDown(pwnd) ((pwnd)->wButton & 4)

#define SetFButtonDown(pwnd, fDown) \
	{if (fDown) (pwnd)->wButton |= 4; \
	  else (pwnd)->wButton &= ~4;}

#define FFirstButton(pwnd) ((pwnd)->wButton & 8)

#define SetFFirstButton(pwnd) (pwnd)->wButton = ((pwnd)->wButton |= 8)

#define SetNextGroupButton(pwnd, pwndNext )  (pwnd)->pwndButtonNext =(pwndNext)



#define cwExtraFrame (cwExtraText + 4)	

#define fWinActive   rgwExtra[1]
#define szFrameText  rgwExtra[2]
#define hFrameMenu   rgwExtra[3]

#define axyLastPos	rgwExtra[4]
#define rxyLastPos	rgwExtra[5]
#define fZoomFrame	rgwExtra[6]
#define arcMenuArc   rgwExtra[7]

// General window stuff

#define	PwfnCtlGeneral(pwnd)	(*((PWFN_CTL *) &pwnd->rgwExtra[cwExtraMin]))
#define	WParamGeneral(pwnd)	(pwnd->rgwExtra[cwExtraMin+2])

//#if cwExtraGeneral != cwExtraMin+3
//.....
//#endif

// General window stuff


//listbox stuff

// customization 

// extra word usage 

// 0,1,2 are listbox function and wParam (for on-demand) 

#define iszTopLb 	rgwExtra[cwExtraMin+3]			// first item in Display 
#define cszLb		rgwExtra[cwExtraMin+4]			// # of strings in list in list 
#define iszCurLb	rgwExtra[cwExtraMin+5]			// currently selected item 
#define hmemMpiszoffLb	rgwExtra[cwExtraMin+6]	// array of offsets in string buffer
#define hmemGpszLb	rgwExtra[cwExtraMin+7]		// string buffer pool 
#define offLb 		rgwExtra[cwExtraMin+8]			// next string buffer pointer 
#define offMaxLb 	rgwExtra[cwExtraMin+9]			// size of string buffer 
#define iszMacLb 	rgwExtra[cwExtraMin+10]			// max isz before index buf is grown
#define fSelectedLb	rgwExtra[cwExtraMin+11]		// do we have a selection ? 
#define isaColor	rgwExtra[cwExtraMin+12]			// colour of the listbox 
#define isaHiliteColor	rgwExtra[cwExtraMin+13]	// colour of the listbox hilite
#define ctickRepLb	rgwExtra[cwExtraMin+14]		// scrolling rate 

#ifndef LISTBOX_HORIZ
	#define	cwExtraNeeded	(cwExtraMin+15)
#else
	// Horizontal scrolling extra info 

	#define	drxItemLb	rgwExtra[cwExtraMin+15]		// width of a single item (add 1 for space) 
	#define	citemWidthLb	rgwExtra[cwExtraMin+16]	// max # of items wide 
	#define	cwExtraNeeded	(cwExtraMin+17)
#endif

//#if cwExtraListBox != cwExtraNeeded
//.....
//#endif

#define	PwfnCtlLb(pwnd)	(*((PWFN_CTL *) &pwnd->rgwExtra[cwExtraMin]))
#define	WParamLb(pwnd)		(pwnd->rgwExtra[cwExtraMin+2])


// special listbox window specific isa's 

#define IsaListbox(pwnd)			((pwnd)->rgwExtra[13])
#define IsaHiliteListbox(pwnd)	((pwnd)->rgwExtra[14])

//listbox stuff

//Scrollbar defs

#define cwExtraScroll	5	// size of rgwExtra for scroll windows 

// scroll bar definitions 

#define ctickRepSb	rgwExtra[0]	// # of ticks to repeat 
#define ptCurSb		rgwExtra[1]	// current position on scroll line 
#define ptMinSb		rgwExtra[2]	// minimum position on scroll line 
#define ptMaxSb		rgwExtra[3]	// end position on scroll line 
#define ptElevatorSb	rgwExtra[4]	// elevator position (lower byte) 

//Scrollbar defs



//Editbox defs

// extra word usage (filled by SDM) 

#define	isaEb		rgwExtra[cwExtraText+0]			// color 
#define	isaSelEb	rgwExtra[cwExtraText+1]			// selected color 
#define chFillDialog	rgwExtra[cwExtraText+2]		// fill char for trailing spaces 
#define ichMacEb	rgwExtra[cwExtraText+3]			// last character in edit buffer 
#define ichLeftEb	rgwExtra[cwExtraText+4]			// leftmost character displayed 
#define ichCursorEb	rgwExtra[cwExtraText+5]		// current cursor position, to the left of
															   // insertion point 
#define ichSelEb	rgwExtra[cwExtraText+6]			// start of selection 
#define	fNoBracketEb	rgwExtra[cwExtraText+7]	// don't show brackets ?? 
#define wEb		rgwExtra[cwExtraText+8]				// random flags 
#define cchMaxEb	rgwExtra[cwExtraText+9]			// for fixed length edit items 
#define isaDisabledEb	rgwExtra[cwExtraText+10]// disabled color 
#define szWildCardEb	rgwExtra[cwExtraText+11]	// auxilary edit field 

//#if cwExtraEdit != cwExtraText+11
//.....
//#endif

//#if cwExtraEditAux != cwExtraText+12
//.....
//#endif

//Editbox defs


// Message box defs

// this structure is defined only so that C will allocate enough extra
// words in the msg box window structures put on the stack 

typedef struct _wndm
	{
	WND	wnd;
	WORD 	rgwExtraPlus[cwExtraMsgBox-1];
	} WNDM;

// message box state 
#define pmbsDialog		rgwExtra[cwExtraDialog]

// Message box defs


// Dropdown defs

// dropdown rgwExtra aliases 
#define wDropFlags      rgwExtra[cwExtraMin]
#define pwndDropListBox rgwExtra[cwExtraMin+1]
#define pwndDropButton  rgwExtra[cwExtraMin+2]
#define pwndDropEdit    rgwExtra[cwExtraMin+3]
#define pwndDropHolder  rgwExtra[cwExtraMin+4]
#define pfnDropListBoxProc	rgwExtra[cwExtraMin+5]	// actually two words 
#define pfnDropEditProc	rgwExtra[cwExtraMin+7]		// actually two words 


#define PwndDropListBox(pwnd)		(*(PWND*)&((pwnd)->pwndDropListBox))
#define PwndDropButton(pwnd)		(*(PWND*)&((pwnd)->pwndDropButton))
#define PwndDropEdit(pwnd)			(*(PWND*)&((pwnd)->pwndDropEdit))
#define PwndDropHolder(pwnd)		(*(PWND*)&((pwnd)->pwndDropHolder))
#define PfnDropListBoxProc(pwnd) (*(PLFN_WNDPROC*)&((pwnd)->pfnDropListBoxProc))
#define PfnDropEditProc(pwnd)		(*(PLFN_WNDPROC*)&((pwnd)->pfnDropEditProc))

// dropdown holding window rgwExtra aliases and macros 

#define pwndHolderDrop rgwExtra[cwExtraMin]

#define PwndHolderDrop(pwnd) (*(PWND*)&((pwnd)->pwndHolderDrop))

// Dropdown defs

extern PWND_DLG	pwndDlg;		// Again, app should never use 

#endif //	BLADE

extern PWND_DESKTOP PASCAL pwndDesktop;

/***END_PUBLIC***/
