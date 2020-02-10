;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/* ZZZZZZ There are some variables which need to be unsigned long, unsigned
	int instead of plain long, int -- Otherwise close to 2**32 problems may
	occur!! */

/* ZZZZ The message queue is not flushed by DosErrorBox?? Retry problem!! */

/****	view.c - implements "view file" command for file manager
**	
*/
#include <common.h>
#include <menus.h>
#include <filemgr.h>
#include <prot.h>
#include <dos.h>
#include <share.h>
#include <text.h>
#include <help.h>
#include <icons.h>
#include <ctype.h>
#include <assert.h>

extern MPVKEYID gAccelerators[];
extern MPVKEYID *pAccel;

extern VOID FAR *	FAR PASCAL LpbAllocWorkFar(WORD) ;
extern BOOL FreeUnusedFMMemory(void) ;
extern void DoFullRefresh(void) ;

extern BOOL ScrollPause(BOOL reset) ;

extern MENUINFO ViewMenuBar ;
extern MENUINFO FileMgrMenuBar ;
extern BOOL isup[2];

#define HexDigit(i) ((unsigned char) ((i) < 10 ? '0' + (i) : 'A' + (i)-10))

//	This is a full-screen child of the file mgr window.
WND ViewWind = wndGeneric(2, WS_CHILD, TRUE, 0, 0, 0, 0,
			ViewWindProc, &MainWind, NULL, NULL)
			{ 0 }
endWndGeneric;
extern BOOL ViewRead(long start_fpos) ;
extern void AddViewAccelerators(void);

/* MAXPAGES determines the max number of pages of text that can be viewed.
** It needs to be high enough that users don't complain much.
** MAXPAGELINES is the highest number of lines of the file which can
** be displayed at a time.
** GRANULARITY: assume that GRANULARITY == 20.  Then offsets[0] is the number
** of bytes between the start of line 0 and the start of line 20, offset[1]
** contains the byte count between line 20 and line 40, etc.
** WARNING! v_buffer[] must be able to hold at least GRANULARITY lines at a time
** in both hex and ASCII modes.
** VIEWBUFSIZE is the size of v_buffer[].
*/

#define FILETEXTY 2 /* Y-coordinate at which the first file text line begins */

/* 7 lines are lost 4 header lines + 2 on top and 1 at bottom */
#define LINESLOST 5

// #define VIEWBUFSIZE		4096
unsigned VIEWBUFSIZE ;

// #define MAXPAGES		100
// #define MAXPAGELINES		42  
// ZZZZ Actually MAXPAGELINES should be 50-LINESLOST = 38!!
// #define GRANULARITY		32					
// #define OFFSETSIZE		((MAXPAGES*MAXPAGELINES)/GRANULARITY+1)
#define OFFSETSIZE 			60
/*
* An OFFSETSIZE of 60 will allow us an indexed line accesses in files that
* have a maximum of 60*GRANULARITY lines. In the worst case GRANULARITY will
* be 36. See the calculations in fn ViewFile. The case when the whole file
* fits into the buffer, GRANULARITY is useless!!
*/
unsigned GRANULARITY ;
#define GR_MARGIN 		8

#define KEY_CR			13
#define KEY_LF			10
#define KEY_FILLER	'.'
#define KEY_CTRL_Z		26

unsigned long ViewFileSize ;
BOOL ascii;	// TRUE=ASCII display, FALSE=hex

static unsigned offsets[OFFSETSIZE];
static int lastoffset;					// last valid offset

static RY linesperpage;						// lines per page
static long  bufbeginline ;	/* the line that begins at &v_buffer[0] */
static long  buffocusline ; /* the line that is at loc >= &v_buffer[0] */
static long  buffocuspos ;  /* the offset into the v_buffer of bufposline */
static long curline;				// first displayed line of curpage

/* After every dos-read, VerifyPage sets bufbeginline, buffocusline,
	buffocuspos! Note that only if bufbeginline!=-1, the other two
	have meaningful values */

#if DEBUG
static long lastline;		// last line of file ZZZ unused at present.
#endif

unsigned char far *v_buffer;		// pointer to file data.
static unsigned buflen;				// # of valid chars in v_buffer
static long filepos;				// file position of start of v_buffer

/* following two variables store the linenumber and its filepos -- when
	we run out of our array of offsets for line boundaries! */
static long cachedline ;
static long cachedfilepos ;

static int handle;							// file handle for reads
static unsigned char path[1+MAX_PATH];		// the file itself

/* Following are relative co-ordinates in ViewWind as to where the first
   non-blank characters go on left and top */
#define TITLE_RX   4
#define TITLE_RY   0
#define  HEADER_SEPY 5

/****	Printable - is given character printable?
**
**	ENTRY
**			c - character to examine
**	EXIT
**			TRUE if printable, FALSE if not.
**	WARNING
**		This will need to be changed for internationalization.
*/
BOOL FAR Printable(c)
unsigned char c;
{
	switch (c)
	{
		case KEY_TAB:
		case KEY_CR:
		case KEY_LF:
		case KEY_BS:
		case KEY_CTRL_Z:
			return TRUE;
		default:
			if (c >= 32 && c <= 254 && c != 127)
				return TRUE;
			else
				return FALSE;
	}
} /* proc Printable */

/* Atleast 'low' KBytes are required! -- Allocate close to 1K of Max
	allocation possible. Max of 'highend'Kbytes is what it tries!!  */
unsigned FAR AllocClosestTo1K(unsigned char far **buffer, unsigned lowend,
														unsigned highend)
{
	unsigned prevhighend ;
	BOOL not_satisfied ;

	/* Make sure that we can allocate at least as many bytes as the lowest
	 * bound requested. If not we just say sorry, right away. This makes
	 * our loop later on always succeeed.
	 */
	if  (!(*buffer = LpbAllocWorkFar(lowend<<10)))
	{
		*buffer = NULL ;
		return 0 ;
	}

	FreeWorkFar(*buffer) ;
	prevhighend = highend+1 ;
	not_satisfied = TRUE ;
	while (not_satisfied)
	{
		if ( !(*buffer = LpbAllocWorkFar(highend<<10)) )
		{
				prevhighend = highend ;
				highend = (highend + lowend) / 2  ;			
				if (highend <= (lowend+1) )
					not_satisfied = FALSE ;
		}
		else
		{
			FreeWorkFar(*buffer) ;
			if (highend < (prevhighend - 1))
			{
				lowend = highend ; 
				highend = prevhighend-1 ;
			}
			else
			{
				lowend = highend ;
				not_satisfied = FALSE ;
			}
		}
	} /* while */
	*buffer = (unsigned char far *) LpbAllocWorkFar(lowend<<10) ;
	return (lowend<<10) ;
} /* AllocClosestTo1K */
		
/****	ViewFile - view the selected file in either hex or ASCII form
**		This fn, only called when a single file is selected, implements
**	the View File command, allowing the user to examine the contents of a
**	file.  Active keys are up and down arrow, which scroll by one line,
**	and page up/page down which scroll by a screenful less one line.
**	ENTRY
**		no parms
**	EXIT
**		no return
**	RESTRICTION:
**		As currently implemented, the fn uses a fixed-size table to store
**	the size of each page of text in ASCII mode.  There are MAXVIEWPAGES
**	entries in this table; thus this is the number of pages which can be
**	viewed in ASCII mode.  Trying to go beyond this point produces...what?
*/
BOOL ViewFile(PTREE tree, PENTRY node, char *unused1, int count, 
												int total, BOOL unused2)
{
	unsigned l, h ;
	char statusline[STATUS_LEN] ;
	struct find_t findinfo;
	int ret ;
	int dummylen ;

	UnReferenced(unused1) ;
	UnReferenced(unused2) ;

	Tree2Path(tree, node, path, &dummylen) ;

	/* Put up dialog box saying file of size 0 can't be viewed */
	FormCountStatusLine(statusline, szOpViewFile, path, count, total,
																			STATUS_COUNT_START) ;

	do
	{
		ret = shell_findfirst(path, _A_HIDDEN | _A_SYSTEM,	&findinfo);
		if (ret)
			// ZZZZZ modify HELP_VIEWOPEN to be appropriate one!
			ret = GetResponse(statusline, szFileNotFound, BT_FILERETRYSKIPQUIT,
																			HELP_VIEWOPEN) ;
		else
			ret = ACT_OK ;
	}  while (ret == ACT_RETRY) ;

	if (ret != ACT_OK)
	{
		/* Mark that we are not in ViewFile! Checked in WindProc!! */
		gpszFileOpCaption = NullString ;
		return ret ;
	}

	ViewFileSize = findinfo.size ;

	if (ViewFileSize != node->x.f.size)
	{
		/* The file's size changed behind our back. The user changed this
		 * info when outside the shell and the shell does not re-read the
		 * directory structure in these cases unless specifically forced
		 * by the user.
		 */
		node->x.f.size = ViewFileSize ;
		node->dtlx.dt.time = findinfo.wr_time;
		node->dtlx.dt.date = findinfo.wr_date;

		/* ZZZ We don't update the atributes field as if it was not a hidden
		 * or system file and now became one, it should be made to disappear
		 * from the file list (if display hidden/system files is turned off).
		 * This will confuse users.
		 */
	}

	if (ViewFileSize == 0)
	{
		/* Use BT_QUIT if we don't want to provide the user any choices to	*/
		/* select, except to to click on the dialog box button.				*/
		/* return GetResponse(statusline, szFileEmpty, BT_QUIT, 0) ; */
		ret = GetResponse(statusline, szFileEmpty, BT_FILESKIPQUIT, HELP_VIEWEMPTYFILE) ;

		/* Mark that we are not in ViewFile! Checked in WindProc!! */
		gpszFileOpCaption = NullString ;

		return ret ;
	}

	if (ViewFileSize >= ((unsigned) 32 << 10))
		h = 32 ;
	else
	if (ViewFileSize <= 1024)
		h = 1 ;
	else
		h =  (unsigned) ( (ViewFileSize - 1)/1024 + 1 ) ; 

	/*  lower bound need not exceed 4K bytes!! -- 1 page will fit in this! */
	if (h <= 4)
		l = h ;
	else
		l = 4 ; /* 4K is the minimum we want as it can fit 1 page of display */

	while (1)
	{
		/* try to get 4K or greater */
		VIEWBUFSIZE = AllocClosestTo1K(&v_buffer, l, h) ;

		if (!VIEWBUFSIZE)
		{
			if (!FreeUnusedFMMemory())
			{
				OutOfMemory() ;
				/* Mark that we are not in ViewFile! Checked in WindProc!! */
				gpszFileOpCaption = NullString ;
				return ACT_CANCEL ;
			}
			// else retry alloc, as we just freed some mem -- stay in while loop!
		}
		else
			break ; // Our alloc succeeded, proceed with ViewFile
	} /* while */	

	/* Note that we want to make sure that v_buffer can hold atleast
		GRANULARITY lines. Each line can be a max of (axMac+2) chars long!!
		We give ourselves a safety margin of GR_MARGIN in buffer */
	/* When we get a buffer of only 4K and filesize > 4K, GRANULARITY will
		be = 36. Even in this case, since OFFSETSIZE=60, we can have indexed
		access to start of the first 2160 (=36*60) lines of any file, which
		is very good by any standards */
	GRANULARITY = (VIEWBUFSIZE/(axMac+2)) - GR_MARGIN ;

	/* The help line for view at bottom of screen */
	MessageBar(szViewMessage, isaMenu,TRUE) ;

	glob.InFileMgr = FALSE;				// turn off list box updates

	MoveWindow(&ViewWind, 0, 2);
	SetWindowSize(&ViewWind, axMac, ayMac-3); // expand to full screen

	EnableWindow(&ViewWind, TRUE);

	// SetCapture(&ViewWind);

	/* Set menu bar to MainWind itself instead of ViewWind, as MainWind's
	 * WindProc routine draws boxes around popped down menus, etc. We
	 * don't want to duplicate that code here.
	 */
	setmenubar(&ViewMenuBar, &MainWind) ;

	/* ZZZZZ for some reason these accelerators don't work! */
	/* these is code in the WIndProc later to handle F1 key! -- Remove
	 * it when this is figured out.
	 */
	AddViewAccelerators();

	SetFocus(&ViewWind);
	/* BUG BUG WARNING THIS IS LAZY SLIME FROM BUSYDLG.C! */
	isup[0] = FALSE;
	isup[1] = FALSE;
	/***/

	return ACT_CANCEL;
} /* proc ViewFile */

extern void AddFileManAccelerators(void) ;
extern void RefreshFMScreen(BOOL erase);

void FAR WrapUpView(void)
{
	FreeWorkFar(v_buffer) ;

	/* ZZZZ No error checking done here */
	_dos_close(handle);

	/*
	 * Back to Filemanager state
	 */
	// ReleaseCapture();
	EnableWindow(&ViewWind, FALSE);
	EnableWindow(&MainWind, TRUE);
	SetWindowSize(&ViewWind, 0, 0);
	SetFocus(&MainWind);

	glob.InFileMgr = TRUE; // Turn on listBox updates!

	/* Put back file manager accelerators */
	AddFileManAccelerators() ;

	RefreshFMScreen(TRUE);
	/* Mark that we are not in ViewFile! Checked in WindProc!! */
	gpszFileOpCaption = NullString ;

} /* WrapUpView */

/* ZZZZZ  This proc is called from MainWind's WindProc also. It is called
 * by CW for WM_SETFOCUS and a few other messages. If we do a 
 * SetCapture(&ViewWind), then CW sends the mouse messages, etc but then
 * the View Menu Bar doesn't work!! So, the kludged way it has been got working
 * is to have the MainWind's WindProc to pass on its message to this routine
 * in case we are in "ViewFile". So these params are those sent to MainWind!
 * Thus the RX, RY, etc are relative to MainWind and not ViewWind!!!!
 */
long FAR PASCAL ViewWindProc(PWND pwnd, WORD message, WORD wParam,
																					DWORD lParam)
{
	long bufpos;						// offset of current page in v_buffer[]
	long oldcurline;
	int action;							// result of error handling
	BOOL update;						// time to print, or Beep
	int ret;							// dos call return
	long i;
	RX rx ;
	RY ry ;
	RRC rrcClient ;	/* temp */
	static WORD scroll_key = 0 ; /* the key to repeat when mouse held down! */
	WORD state ;

	update = FALSE ;

	switch (message)
	{
		case WM_SETFOCUS:
			GetClientRrc(&ViewWind,&rrcClient);
			FillRrc(&ViewWind,&rrcClient,' ',isaBackground);

			DisplayHeader(path);	// put some info at the top

			// FrameMenuBar doesn't care about window.
			FrameMenuBar(&MainWind) ;

			// open selected file
			do {
				action = ACT_OK;
				ret = _dos_open(path, O_RDONLY | SH_DENYWR, &handle);
				if (ret)
					action = DOSErrorBox(szErrorOpen, ret, HELP_VIEWOPEN);
			} while (action == ACT_RETRY);

			if ( (action == ACT_CANCEL) || (action == ACT_SKIP) )
			{
				WrapUpView() ;
				return TRUE ;
			}

			linesperpage = ayMac-LINESLOST;
			lastoffset = -1;
#ifdef DEBUG
			lastline = -1 ;
#endif
			curline = 0;					// first line of file
			bufbeginline = -1 ; /* Initialized with trash line number */

			/* default initializations */
			cachedfilepos = 0 ;
			cachedline = 0 ;

			/* Read in a page's worth.  Since curline == 0, this will contain
			** the correct page, whether the file is text or binary.
			*/
			bufpos = VerifyPage(curline, TRUE);

			/* Error during read? */
			if (bufpos == -3)
				return TRUE ;

			/* ZZZZZ Remove this Printable stuff if found not very useful! */
			// Determine whether it's a text file
			ascii = TRUE;
			for (i=0; i < 128 && i < buflen; i++)
			{
				if (!Printable(v_buffer[i]))
				{
					ascii = FALSE;
					break;
				}
			}
			DisplayPage(bufpos, linesperpage, ascii); // put onscreen
			break;
#if 0
		case WM_KILLFOCUS:
			/* ZZZ error checks not needed here?! */
			_dos_close(handle);

			ReleaseCapture();
			EnableWindow(&ViewWind, FALSE);
			SetWindowSize(&ViewWind, 0, 0);

			break;
#endif
		
		case WM_LBUTTONDOWN:
		case WM_LBUTTONDBLCLK:

			ScrollPause(TRUE) ;

			rx = LOBYTE(lParam) ;
			ry = HIBYTE(lParam) ;
			wParam = 0 ; /* A dummy key -- a value that won't be set below */
			/*  (TITLE_RY+2) is the y-coord of the PgUp, PgDn Icons */
			if (ry == (TITLE_RY+2))
			{
				if ( (rx >= (RX)TITLE_RX+PGUPSTARTIND) && (rx < (RX)TITLE_RX+PGUPSTARTIND+PGUPLEN) )
					scroll_key = wParam = VK_PRIOR ;
				else
				if ( (rx >= (RX)TITLE_RX+PGDNSTARTIND) && (rx < (RX)TITLE_RX+PGDNSTARTIND+PGDNLEN) )
					scroll_key = wParam = VK_NEXT ;
				else
				if ( (rx >= (RX)TITLE_RX+LINEUPSTARTIND) && (rx < (RX)TITLE_RX+LINEUPSTARTIND+LINEUPLEN) )
					scroll_key = wParam = VK_UP ;
				else
				if ( (rx >= (RX)TITLE_RX+LINEDNSTARTIND) && (rx < (RX)TITLE_RX+LINEDNSTARTIND+LINEDNLEN) )
					scroll_key = wParam = VK_DOWN ;
				else
					scroll_key = 0 ;
			}
			else
			{
				scroll_key = 0 ;
			}
#if 0
// Following code handles mouse clicks on the bottom status line on the screen
// This refers to the Enter, ESC, F9 messages!
			else
			/* (ayMac-1) is the y-coord line that has the Enter, Esc, F9 line */
			if (ry == (ayMac-1))
			{
				/* Now figure out the coordinates of Enter, Esc, F9 */
				if ( (rx >= (RX) 2) && (rx <= (RX)10) )
					wParam = VK_NEXT ;
				else
				if ( (rx >= (RX)13) && (rx <= (RX)22) )
					wParam = ESCAPE ;
				else
				if ( (rx >= (RX)25) && (rx <= (RX)36) )
					wParam = VK_F9 ;
			}
#endif
			/* recursive call! -- send equivalent keyboard character message */
			if (wParam != 0) 
				ViewWindProc(pwnd, WM_CHAR, wParam, lParam) ;
			break ;

		case WM_MOUSEMOVE:
		case WM_LBUTTONUP:
			scroll_key = 0 ;
			break ;

		case WM_CHAR:
			oldcurline = curline ;
			state = HIWORD(lParam) ; /* state of the keyboard's shift/alt/ctrl */

			switch (wParam)
			{
				case VK_HOME:
					if (curline > 0)
					{
						/* ZZZZZ fix this to prevent re-reading disk on HOME key*/
						bufbeginline = -1 ;
						curline = 0 ;
						update = TRUE ;
					}
					break ;
				case VK_UP:
					if (curline > 0)
					{
						curline--;
						update = TRUE;
					}
					break;
				case VK_DOWN:
						curline++;
						update = TRUE;
					break;
				case VK_PRIOR:					// page up
					if (curline >= linesperpage)
					{
						update = TRUE;
						/* Scroll up 1 line less than a full page */
						curline -= (linesperpage-1) ;
					} else if (curline > 0)
					{
						update = TRUE;
						curline = 0;
					}
					break;
				case 13:  // ZZZZ enter also behaves like pagedown???
				case VK_NEXT:					// page down
					/* Scroll down 1 line less than a full page */
					curline += (linesperpage - 1) ;
					update = TRUE ;
					break ;
				case VK_ESCAPE:
				case ESCAPE:
					WrapUpView() ;
					return TRUE ;

				case VK_F5:
					if (state & KK_SHIFT)
					{
						DoFullRefresh() ;
					}
					return TRUE ;

				case VK_F1:
					HelpBox() ;
					return TRUE ;

				case VK_F9:					// toggle hex/ascii
					/* Ignore Shift+F9 and Ctrl+F9 and Alt+F9 */
					if (	(state & KK_CONTROL) || (state & KK_SHIFT) ||
							(state & KK_ALT) )
					{
						break ;
					}

					bufpos = VerifyPage(curline, ascii);

					/* Error during read? */
					if (bufpos == -3)
						return TRUE ;

#ifndef NOCONSISTENCY
					if (bufpos < 0)
					{
						printf("***VK_F9 -- bufpos = %d\n", bufpos) ;
						exit(0) ;
					}
#endif
					if (ascii)
					{
						ascii = FALSE;
						curline = (filepos+bufpos) / 16 ;
					} else
					{
						long l = 0;
						long goal ;

						goal = filepos+bufpos ;

						/* ZZZZ Hack to get repeated switching between modes
							working correctly -- If cachedfilepos is not too
							far ahead make it the goal */
						if ( ( (cachedfilepos-goal) >= 0 ) && 
								( (cachedfilepos-goal) < 16 ) )
							goal = cachedfilepos ;
						else
						{

							/* If our goal is too close to EOF -- we should
								move back atleast 1 line from EOF! In worst
								case it will be axMac chars followed by \r, \l*/
							if ( goal > ((ViewFileSize-1) - (axMac+2)) )
							{
								goal = (ViewFileSize-1) - (axMac+2) ;
								if (cachedfilepos > goal)
									goal = cachedfilepos ; 
							}
							else
								/* case when we are too close to file start
									and say there is just 1 long line 
									in file. We don't want to hit EOF!! */
								if (goal < (axMac+2))
									goal = 0 ;
						}
						// Get to within GRANULARITY lines of the same offset.
						ascii = TRUE;
						curline = 0;
						for (i=0; i <= lastoffset; i++)
						{
							if (l+offsets[i] <= goal)
							{
								l += offsets[i];
								curline += GRANULARITY;
							} else
								break;
						}
						
						// Get to within one line of the same offset.
						if (l < goal)
						{
							if ((goal >= cachedfilepos) && (cachedfilepos > l))
							{
								curline = cachedline ;
							}
							/* Dummy first argument */
							bufpos = VerifyPage(curline, ascii);

							/* Error during read? */
							if (bufpos == -3)
								return TRUE ;

							/* if goal is same as cachedfilepos, fn VerifyPage
								would have loaded it correctly, else we need
								to do the following loop! */
							if (goal != cachedfilepos)
							{
								do {
									i = SeekFwd(bufpos, (long)1);
#ifndef NOCONSISTENCY
					if (bufpos == -2)
					{
						printf("***goal reaching -- bufpos = %d\n", bufpos) ;
						exit(0) ;
					}
#endif
									if (i != -1)
									{
										bufpos = i;
										curline++;
									}
									else
									{
										/* Dummy first argument */
										bufpos=VerifyPage(curline, ascii);

										/* Error during read? */
										if (bufpos == -3)
											return TRUE ;
									}
								} while (filepos+bufpos < goal);
							}
						} /* if part of reaching goal */
					} /* else part of switching view modes */
					update = TRUE;
					break;

				default:
					break ;
			}

			if (update)
			{
				update = FALSE;
				bufpos = VerifyPage(curline, ascii);

				/* Error during read? */
				if (bufpos == -3)
					return TRUE ;

				if (bufpos == -2)
				{
					curline = oldcurline ;
					Shell_Beep() ;
					return TRUE ;
				}

#ifndef NOCONSISTENCY
				if (bufpos == -1)
				{
					printf("***ViewFile-- bufpos = -1\n") ;
					exit(0) ;
				}
#endif

				DisplayPage(bufpos, linesperpage, ascii);
			}
			else
				Shell_Beep() ;

			break ;

		case WM_MOUSEIDLE:
			if (scroll_key != 0)
			{
				/* Delayed scroll repetition */
				if (ScrollPause(FALSE))
					ViewWindProc(pwnd, WM_CHAR, scroll_key, (DWORD)0) ;
			}
			break ;

	} /* message switch */

	return TRUE ;
} /* proc ViewWindProc */

/****	VerifyPage - load in the appropriate page if it's not already in memory
**
**	ENTRY
**			the_line - first line of the page you want
**			ascii    - TRUE for ascii display mode, FALSE for hex mode
**	EXIT
**		Returns the offset of the page within v_buffer[].
*/
long FAR VerifyPage(long line, BOOL ascii)
{
	long begin;				// file position of page start
	long end;				// file position of page end
	int i;					// counter

	if (!ascii)
	{
		long tbegin ;

		// hex display lines are always 16 bytes long - very easy.
		begin = line * 16;

		if (begin >= ViewFileSize)
		{
			return -2 ;
		}

		end = begin + linesperpage * 16;

		if (end >= ViewFileSize)
			end = ViewFileSize-1 ;

		if (begin < filepos || end >= filepos+buflen)
		{
			/* tbegin is used to determine where to start loading data
				from the file */
			tbegin = begin ;
			if ( tbegin < filepos )
			{
				/* seeking backwards -- Keep it so that we keep the location we
					want -- (i.e., old value of begin) in the middle of
					buffer so that scanning further backward won't necessarily
					touch the disk */
				tbegin = tbegin - VIEWBUFSIZE/2 ;
				if (tbegin < 0)
					tbegin = 0 ;
			}

			/* load so that we don't waste buffer space when close to EOF */
			if (tbegin > (ViewFileSize-VIEWBUFSIZE+1))
				tbegin = ViewFileSize-VIEWBUFSIZE+1 ;

			// Load in desired chunk.
			if (!ViewRead(tbegin))
				return -3; /* junk value */
			/* trash the v_buffer line information as we are in hex mode */
			/* It is quite a pain to keep track of line info in hex mode */
			/* It will slow down hex mode unnecessarily. The assumption is */
			/* that too many view mode switches won't happen. */
			bufbeginline = -1 ;
		}
		return (begin - filepos);
	} else
	{
		long bufpos;				// current point in v_buffer
		long tbufpos ;
		long templine ;
		long prevoffset;		// old file position

		/* First check to see if requested page is within our v_buffer */
		/* if so we don't have to hit the disk, else we need to load in */
		/* data from the disk */
		if ( (bufbeginline != -1) && (bufbeginline <= line) )
		{
			/* Requested line could be in our v_buffer */
			if (line == bufbeginline)
				return (long) 0 ;
		
			if (line == buffocusline)
				return buffocuspos ;

			/* buffocusline is >= buf beginline, so we should start at 
				buffocusline, if possible!! */
			if (line > buffocusline)
			{
				templine = buffocusline ;
				bufpos = buffocuspos ;
			}
			else
			{
				templine = bufbeginline ;
				bufpos = 0 ;
			}
			bufpos = SeekFwd(bufpos, line-templine) ;
			if (bufpos == -2)
				return -2 ;

			if (bufpos != -1)
			{
				/* v_buffer has the start of 'line' */
				/* now check to see if this whole page is in memory */
				tbufpos = SeekFwd(bufpos, (long) linesperpage) ;
				if (tbufpos != -1)
				{
					/* -2 is also acceptable as full page is in v_buffer */
					buffocusline = line ;
					buffocuspos = bufpos ;

					return buffocuspos ;
				}
				/* else 'line' begins in v_buffer but full page not present */
				/* ZZZZ Maybe we should load starting at this line here instead
					of flowing thru to the code below */
			}
			/* else 'line' starts beyond our v_buffer */ 
		} 
		/* else 'line' is in fileposition prior to what v_buffer holds */

		/* If the requested line is within the area we have covered before,
		** the offset can be found directly, modulo GRANULARITY.  Otherwise,
		** we need to count forward from the last offset entry a line at a
		** time, making sure to load new data into v_buffer when necessary.
		*/
		if (line < (lastoffset+1) * GRANULARITY)
		{
			// Go to nearest anchor point before desired line.
			bufbeginline = filepos = 0;
			for (i=(int)(line/GRANULARITY)-1; i >= 0; i--)
			{
				bufbeginline += GRANULARITY ;
				filepos += offsets[i];
			}
			if (!ViewRead(filepos))
				return -3; /* junk value */

			buffocusline = bufbeginline ;
			buffocuspos = 0 ;

			/* Manually count out the remaining lines.  The desired start line
			** will be in the v_buffer because it is less than GRANULARITY lines
			** from the start; this is why v_buffer[] must be big enough to store
			** GRANULARITY lines.
			*/
			bufpos = SeekFwd((long) 0, (long) (line % GRANULARITY));

#ifndef NOCONSISTENCY
			if (bufpos < 0)
			{
				printf("*** Within granularity -- bufpos = %d\n", bufpos) ;
				exit(0) ;
				// return -2 ;
			}
#endif
			tbufpos = SeekFwd(bufpos, (long) linesperpage) ;
			if (tbufpos != -1)
			{
				/* -2 is also acceptable */
				buffocusline = line ;
				return (buffocuspos = bufpos) ;
			}
			/* else page starts in our buffer but full page doesn't exist */
			/* so load so that this page begins our buffer */

			lseek(handle, bufpos+filepos, SEEK_SET);
			filepos += bufpos;
			
			if (!ViewRead(filepos))
				return -3; /* junk value */

			/* We don't store the cachedfilepos, cachedline here as we are
				still within our array of offsets!! */

			bufbeginline = buffocusline = line ;

			return (buffocuspos=0) ;
		} else
		{
			long next;				// next line offset
			long anchorline;

			// Go to last anchor point.
			filepos = 0;

			for (i=lastoffset; i >= 0; i--)
				filepos += offsets[i];

			anchorline = (1+lastoffset) * GRANULARITY;
			if  ( (line > anchorline) && (lastoffset >= OFFSETSIZE-1) &&
						(cachedline <= line) )
			{
				anchorline = cachedline ;
				filepos = cachedfilepos ;
			}

			if (!ViewRead(filepos))
				return -3; /* junk value */

			bufbeginline = buffocusline = anchorline ;
			buffocuspos = 0 ;

			prevoffset = filepos;

			/* Count forward the remaining lines.  We may need to read in new
			** data.
			*/
			if (line > anchorline)
			{
				long bufposline = 0;

				bufpos = 0;					// start at beginning of v_buffer
				do {
					next = SeekFwd(bufpos, (long)1);
					if ( next == -2 )
					{
						/* EOF reached */
#ifdef DEBUG
						lastline = anchorline+bufposline;
#endif
						return -2 ;
					}
					if (next == -1)			// overflowed v_buffer
					{
						if (!ViewRead(bufpos+filepos))
							return -3; /* junk value */
						bufbeginline = buffocusline = (anchorline+bufposline);
						buffocuspos = 0 ;
						bufpos = 0;
						next = SeekFwd((long) 0, (long) 1); // 1 line fwd

#ifndef NOCONSISTENCY
						if (next < 0)
						{
							printf("*** Seeking shouldn't return %d\n", next) ;
							exit(0) ;
						}
#endif

					}
					bufpos = next;
					
					/* If we just crossed a new anchor point, record the file
					** position.  Don't record if it would overflow the table.
					*/
					if (++bufposline % GRANULARITY == 0 && lastoffset < OFFSETSIZE-1)
					{
						offsets[++lastoffset] = (unsigned) (filepos + bufpos - prevoffset);
						prevoffset = filepos + bufpos;
					}
				} while (line > anchorline+bufposline);

				/* Have located offset of requested line. If a full page
					is present in buffer starting here, we don't have to
					reload data from disk! */
				tbufpos = SeekFwd(bufpos, (long)linesperpage) ;
				if (tbufpos != -1)
				{
					/* -2 is also acceptable */
					buffocusline = line ;
					cachedline = line ;
					cachedfilepos = filepos+bufpos ;
					return (buffocuspos = bufpos) ;
				}
				/* else page starts in our buffer but full page doesn't exist */
				/* so load so that this page begins our buffer */

				if (!ViewRead(bufpos+filepos))
					return -3; /* junk value */

				bufpos = 0;

				/* store this line info in our cache for future use! */
				cachedline = line ;
				cachedfilepos = filepos ;
			}
			bufbeginline = buffocusline = line ;
			return (buffocuspos = 0) ;
		} /* else part of out of array offsets */
	} /* else part of text mode */
} /* proc VerifyPage */

/****	SeekFwd - count forward the given number of lines in the v_buffer
**
**	ENTRY
**			bufpos - offset of start of current line (in v_buffer[])
**			lines  - number of lines to move forward
**	EXIT
**		Returns the offset of the new line, or -1 if the given line is not
**	completely contained in v_buffer[], or -2 if EOF is reached.
*/
long FAR SeekFwd(bufpos, lines)
long bufpos;
long lines;
{
	unsigned char far *p = v_buffer+bufpos;		// ptr to step through v_buffer
	unsigned char far *boundary = v_buffer+buflen;// end of useful v_buffer data
	long curline = 0;						// which line are we on now
	int curchar = 0;						// character position

#ifndef NOCONSISTENCY
	if (bufpos < 0)
	{
		printf("***SeekFwd called with bufpos = %d\n", bufpos) ;
		exit(0) ;
	}
#endif
	
	/* seeking forward 0 lines leves you at the same line position */
	if (lines == (long) 0)
		return bufpos ;

	while (p < boundary)
	{
		if (lines <= curline)
			break;

		switch (*p)
		{
			case KEY_TAB:
				curchar += TABSIZE - (curchar % TABSIZE); // move to next tab stop
				break;
			case KEY_CR:
				curchar = 0;
				curline++;
				if (p[1] == KEY_LF)			// skip over newline after cr
					p++;
				break;

#if 0
/* ZZZZZZ KEY_LF is treated as just another control character */
			case KEY_LF:	// LF is treated as CR-LF
				curchar = 0;
				curline++;
				break;
#endif

			default:
#ifdef DBCS
				if (IsDBCSLeadByte(*p))
				{
					if (curchar+1 < axMac)
					{
						curchar++;
						p++;
					}
					else
					{
						curchar = 0;
						curline++;
						p--;
					}
				}
#endif
				curchar++;
				break;
		}
		p++;
		
		if (curchar >= axMac)
		{
			/* ZZZZZ possible bug when running off boundary! Note: DisplayPage
				handles this correctly. But we can't do it here as we may
				not have cushion space at the end of v_buffer! */
			if ( (*(p-1) != KEY_CR) && (*(p-2) != KEY_CR) )
			{
				/* We ran out of the screen boundary but without seeing
				a newline or CR. So If we now see a nl or CR skip past
				them */
				if (*p == KEY_CR)
				{
					p++ ;
					if (*p == KEY_LF)
						p++ ;
				}
			}
			curchar = 0;
			curline++;
		}
	} /* while */
	if (curline == lines) 
	{
		if ( (p >= boundary) && ((filepos+buflen) >= ViewFileSize) )
			/* the current line (terminated by nl or cr) ends exactly at EOF */
			return -2 ;
		else
			/* If I don't do the following casting, the compiler would
			 * generate the wrong code for the return value. The boundary
			 * case is when (p-v_buffer) is exactly = 8000h in which case
			 * the return value generated is FFFF8000h (as the compiler
			 * generates a CWD instruction). I am guaranteed that the buffer
			 * we use does not exceed 32K bytes.
			 */
			return ((long) ( (unsigned) (p-v_buffer)) );
	}
	else
	{
		if ( (filepos+buflen) >= ViewFileSize )
			/* current line is terminated by EOF */
			return -2 ;
		else
			return -1;
	}
} /* proc SeekFwd */

#ifdef CLONEVIEW
	#define HexModeDispChar(c) ( isalnum(c) ? (c) : KEY_FILLER )
	#define NormalModeDispChar(c) ( (((c) >= 32) && ((c) < 127) ) ? (c) : ' ' )
#else
	#define HexModeDispChar(c) ( ((c) >= 32) ? (c) : KEY_FILLER )
	#define NormalModeDispChar(c) ( ((c) >= 32)  ? (c) : ' ' )
#endif

/****	DisplayPage - put a page of the file onscreen
**
**	ENTRY
**			bufpos - starting position of the page
**			lines  - number of lines to display
**			ascii  - TRUE to display in ASCII mode, FALSE for hex mode
**	EXIT
**			none
*/
void FAR DisplayPage(bufpos, lines, ascii)
long bufpos;
RY lines;
BOOL ascii;
{
	unsigned char line[1+MAXCOLS];			// image of the current line
	unsigned char far *p = v_buffer+bufpos;		// ptr to step through v_buffer
	RY curline = 0;							// which line are we on now
	int byteidx;							// index of byte w/in row (hex only)
	unsigned char *linep = line;			// ptr within line[]
	unsigned char far *boundary = v_buffer+buflen;// far edge of valid v_buffer chars
	BOOL print = FALSE;						// time to print the line
	BOOL newline = FALSE;					// new line; form file position
	int i;									// junk integer

#ifdef DBCS
	int	dbcs_flag = 0;
	BOOL	dbcs_adj = FALSE;
#endif

	FEnableMouseNest(FALSE) ;

	if (ascii)
	{
		
		if (buflen < VIEWBUFSIZE)
		{
			/* The v_buffer is not full! We always try to read VIEWBUFSIZE 
				characters from start. Note that v_buffer beginning is a line
				beginning in ASCII mode! Also note that v_buffersize is atleast
				1 page big with a few extra bytes (atleast 2)  of 
				cushioning!! */
			/* The following initializations saves us on look aheads like p[1],
			etc when we are at boundaries!! */
			v_buffer[buflen] = v_buffer[buflen+1] = 0 ; /* any value that is not
										a KEY_LF or KEY_CR */
		}

		while (p < boundary)
		{
			if (curline >= lines)
				break;

			switch (*p)
			{
				case KEY_TAB:
					byteidx = TABSIZE - ((linep-line) % TABSIZE);
					for (i=0; i < byteidx; i++)
						*(linep++) = ' ';
					break;
				case KEY_CR:
#ifdef OUR_LF
				case KEY_LF:
#endif
					// This handles lines ending in cr, lf, and cr/lf.
					if (*p == KEY_CR && p[1] == KEY_LF)
						p++;
					print = TRUE;
					break;
				default:
#ifdef DBCS
					if (IsDBCSLeadByte(*p))
					{
						if (linep-line+1 < axMac)
						{
							*(linep++) = *p;
							p++;
							*(linep++) = *p;
						}
						else
						{
							*(linep++) = ' ';
							p--;
						}
					}
					else
#endif
					*(linep++) = NormalModeDispChar(*p) ;
					break;
			}
			p++;

			if (linep-line >= axMac)		// line is running off screen
			{
				if ( (*(p-1) != KEY_CR) && (*(p-2) != KEY_CR) )
				{
					/* Screen boundary determined the end of this line.
					If we now find a nl or CR skip past them */
					if (*p == KEY_CR)
					{
						p++ ;
						if (*p == KEY_LF)
							p++ ;
					}
				}
				print = TRUE;
			}
			if (p >= boundary)				// ready to exit loop
				print = TRUE;

			if (print)
			{
				print = FALSE;
				while (linep-line < axMac)
					*(linep++) = ' ';
				TextOut(&ViewWind, 0, curline+FILETEXTY, line, axMac, 0);
			 	curline++;
				linep = line;
			}
		} /* while */
	} else
	{
		// Hex display.  Init the fixed characters in line.
		for (linep=line; linep < (line+axMac); linep++)
			*linep = ' ' ;
		line[0] = line[11] = line[79] = line[58] = VERTLINE;
		*linep = EOS;

		newline = TRUE;
		while (p < boundary)
		{
			if (curline >= lines)
				break;
			if (newline)
			{
				long l;

				newline = FALSE;
				l = filepos + (p-v_buffer);	// file position of current byte
				for (i=6; i >= 1; i--)		// show current position in file
				{
					line[2+i] = HexDigit(l % 16);
					l /= 16;
				}
				byteidx = 0;
				linep = line+14;
			}
			
#ifdef DBCS
			if (dbcs_flag == 1)
				dbcs_flag++;
			else
				dbcs_flag = IsDBCSLeadByte(*p) ? 1 : 0;
#endif
			
			*linep++ = HexDigit(*p / 16);
			*linep++ = HexDigit(*p % 16);
			if (byteidx % 4 == 3)
				linep += 3;
#ifdef DBCS
			if (!dbcs_adj)
				line[61+byteidx] = HexModeDispChar(*p) ;
			else
			{
				line[61+byteidx] = ' ';
				dbcs_adj = FALSE;
			}
#else
			line[61+byteidx] = HexModeDispChar(*p) ;
#endif
			byteidx++;
			p++;
			if (byteidx >= 16)
			{
#ifdef DBCS
				if (dbcs_flag == 1)
				{
					line[61+byteidx] = HexModeDispChar(*p) ;
					dbcs_adj = TRUE;
				}
				else
					line[61+byteidx] = ' ';
#endif
				byteidx = 0;
				TextOut(&ViewWind, 0, curline+FILETEXTY, line, axMac, 0);
				curline++;
				newline = TRUE;
			}
		} /* while */

		// Finish the last line and print it.
		if (byteidx)
		{
			linep = line+14+2*byteidx+3*(byteidx/4);
			while (byteidx < 16)
			{
				*linep++ = ' ';
				*linep++ = ' ';

				if (byteidx % 4 == 3)
					linep += 3;
				line[61+byteidx] = ' ';
				byteidx++;
			}
			TextOut(&ViewWind, 0, curline+FILETEXTY, line, axMac, 0);
			curline++ ;
		}
	} /* Hex mode */

	// clear any lines which were not drawn (occurs only at EOF)
	if (curline < lines)
	{
		for (linep=line+axMac-1; linep >= line; linep--)
			*linep = ' ';
		if (!ascii)
		{
			line[0] = line[11] = line[79] = line[58] = VERTLINE;
		}
		do {
			TextOut(&ViewWind, 0, curline+FILETEXTY, line, axMac, 0);
		} while (++curline < lines);
	}

	FEnableMouseNest(TRUE) ;

} /* proc DisplayPage */

/****	DisplayHeader - display the header for the File...View command
**
**	ENTRY
**			file - path of file to display
**	EXIT
**			none
*/

#define BOX_MARGIN 1

VOID FAR DisplayHeader(char *file)
{
	unsigned len ;
	char szViewTitle[64] ;
	int filenamestart ;
	int i ;
	char blanks[91] ;

#if 0
	char statusline[STATUS_LEN] ;
#endif

	assert(axMac < 91) ;

	for (i=0 ; i < axMac ; i++)
	{
		blanks[i] = ' ' ;
	}

	/* Note that this box needs to be drawn before the header lines in text
	 * mode and after the header lines in graphics mode because of the top line
	 * getting screwed up!
	 */
	// if (!gisgraph)
	// {
	    EasyDrawBox(&ViewWind, TITLE_RY, 0, TITLE_RY+2, axMac, 0) ;
		 /* Now erase the top line in this box, as it looks bad, when
		  * we put our text message on this line.
		  */
		 TextOut(&ViewWind, (RX)BOX_MARGIN, TITLE_RY, blanks, axMac-(BOX_MARGIN*2), 0) ; 
	// }

	len = strlen(szViewFileMsg1) ;

	FEnableMouseNest(FALSE) ;

	TextOut(&ViewWind, TITLE_RX, TITLE_RY, szViewFileMsg1, -1, 0);

#if 0
	FormStringStatusLine(statusline, szOpViewFile, file, STATUS_LEN) ;
	TextOut(&ViewWind, TITLE_RX, TITLE_RY+2, statusline, -1, 0);
#else
	/* Now, form the ViewFile title --- "DOSShell - filename" */
	strcpy(szViewTitle, szDOSShellTitle) ;
	len = strlen(szViewTitle) ;
	szViewTitle[len++] = (char) ViewTitleSep ;
	szViewTitle[len++] = ' ' ;

	filenamestart = FindLastComponent(file) ;
	if (file[filenamestart] == '\\')
		filenamestart++ ;

	strcpy(szViewTitle+len, file+filenamestart) ;

	/* we need to add a trailing blank, as we have a leading blank */
	len = strlen(szViewTitle) ;
	szViewTitle[len++] = ' ' ;
	szViewTitle[len] = '\0' ;

	UpdateMainTitleBar(szViewTitle) ;
#endif

	FEnableMouseNest(TRUE) ;

#if 0
	if (gisgraph)
	{
		FrameCharRect(TITLE_RY+2, 0, TITLE_RY+4, axMac, 1,isaBackground) ;
	}
#endif

} /* fn DisplayHeader */

/* This function is invoked from the menu to display file in Hex Mode! */
VOID HexFileView(void)
{
	/* This fn should not be called if already in Hex mode! */
	/* The menu should be disabled!!								  */
	assert(ascii) ;

	/* Basically, toggle file view!! -- treat like F9 from key board! */
	ViewWindProc(&ViewWind, WM_CHAR, VK_F9, (DWORD)0) ;
} /* HexFileView */

/* This function is invoked from the menu to display file in ASCII Mode! */
VOID AsciiFileView(void)
{
	/* This fn should not be called if already in ASCII mode! */
	/* The menu should be disabled!!								    */
	assert(!ascii) ;

	/* Basically, toggle file view!! -- treat like F9 key from keyboard! */
	ViewWindProc(&ViewWind, WM_CHAR, VK_F9, (DWORD)0) ;
} /* AsciiFileView */

VOID ExitFileView(void)
{
	/* Basically, treat like ESC key from keyboard! */
	ViewWindProc(&ViewWind, WM_CHAR, VK_ESCAPE, (DWORD)0) ;
} /* ExitFileView */

BOOL ViewRead(long start_fpos)
{
	int action, ret ;
	BOOL do_lseek = TRUE ;
	BOOL read_attempted = FALSE ;

	do
	{
		if (do_lseek)
		{
			if (lseek(handle, start_fpos, SEEK_SET) != -1L)
			{
				ret = 0 ; /* succesful lseek */
				filepos = start_fpos ;
				do_lseek = FALSE ;
			}
			else
				ret = errno ;
		}
		else
		{
			ret = _dos_read(handle, v_buffer, VIEWBUFSIZE, &buflen);
			read_attempted = TRUE ;
		}

		if (ret)
			action = DOSErrorBox(szReadError, ret, HELP_VIEWREAD);
		else
			action = ACT_OK ;

	/* Keep retrying, if action is ACT_RETRY or if the lseek was succesful
	 * (in which case do_lseek would be FALSE) and read has not yet been
	 * attempted.
	 */
	} while ( (action == ACT_RETRY) || ((!do_lseek) && (!read_attempted)) );

	if (action != ACT_OK)
	{
		WrapUpView() ;
		return FALSE ; 
	}
	return TRUE ;
} /* fn ViewRead */

void RefreshViewFileScreen(BOOL fClearAndRedraw)
{
	RRC rrcClient ;
	long bufpos ;

	if (fClearAndRedraw)
	{
		GetClientRrc(&ViewWind,&rrcClient);
		FillRrc(&ViewWind,&rrcClient,' ',isaBackground);

		DisplayHeader(path);	// put some info at the top

		// FrameMenuBar doesn't care about window.
		FrameMenuBar(&MainWind) ;
	}

	bufpos = VerifyPage(curline, ascii) ;

	if (bufpos == -3)
		return ;

	DisplayPage(bufpos, linesperpage, ascii) ;

} /* RefreshViewFileScreen */
