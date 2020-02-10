;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****	filemgr.c - file mgr proc's called from main() and from menus
**
**   Date      Author	Modification
** --------   --------	------------------------------------------------
**  7/17/89   t-jeffro	Moved from main.c
*/
#include <common.h>
#include <filemgr.h>
#include <text.h>
#include <menus.h>
#include <icons.h>
#include <assert.h>

#ifdef KANJI
#include <kkcfltr.h>
#endif

// Lets you compile in debug mode without running out of near heap
//  #define DEBUG  
#ifndef DEBUG
#include <prot.h>
#else
extern BOOL FAR Get_Date_and_Time_Strings(unsigned int theDate, unsigned int theTime,
      char *datestr, char *timestr, BOOL force) ;
extern VOID FEnableMouseNest(BOOL onoroff) ;
extern  int Tree2Path(PTREE tree,PENTRY node,char *str, int *plength);
extern  void InitFileMgr(void );
extern  void far *pascal far LpbAllocWorkFar(unsigned short cb);
extern  void pascal far OutOfMemory(void );
extern  void DoExit(void );
extern  void strfcpy(char far *dst,char far *src);
extern  int GetDiskInfo(PTREE tree,char far *disklabel,unsigned long far *disksize,
											unsigned long far *diskavail) ;
extern	int fstrncmp(char const far *nstr,char const far *fstr,int len);
extern int	Check_Spooler(void) ;
extern	void UpdateMainTitleBar(char *szTitle);
extern  void setmenubar(struct _mnu * * *amenu,struct _wnd *towind);
extern  void FrameMenuBar(struct _wnd *pwnd);
extern  void InitIconCache(void );
extern VOID PutUpBusyDialog(int ith);
extern VOID TakeDownBusyDialog(int ith);
extern  void UpdateListBox(struct ListBoxData *TestList);
extern unsigned char far cdecl GET_WAIT_FLAG(void);
extern VOID DoSwapIn(void) ;
extern  void DoSingleTree(void );
extern  void DoDoubleTree(void );
extern  void DoFlatDisplay(void );
extern void EnableDisableForSearchMode(BOOL doenable1, BOOL doeanble2) ;
extern  BOOL MarkAllTreeMatches(struct th_t far *tree,int doit);
extern  void DeselectTree(struct th_t far *tree);
extern void SetUpTreeForSort(PTREE tree) ;
extern	void SystemSort(PTREE tree, int doit);
extern void SortDirectory(PTREE tree, PENTRY dir) ;
extern  void ExitFileMgr(void );
extern void InitializeStartPrograms(void);
extern	int FileMgrIdle(void );
extern  void UpdateFileTree(int set);
extern  void UpdateDrives(void );
extern  void DoRedisplayList(struct ListBoxData *TestList);
extern	int ListBoxIdle(struct ListBoxData *TestList);
extern	void ListKey(struct ListBoxData *TestList,unsigned short key, unsigned short state);
extern  int GetNextFile(char *path,PTREE tree,PENTRY rptr,int first, int ParentPathLength);
extern  int LoadCompactDir(PTREE tree, PENTRY new);
extern  void Marklastdir(PTREE tree);
extern  void InsertListItem(struct ListBoxData *TestList,unsigned short isz);
extern	void PlotBmp(struct BITMAP *bmp,unsigned short x,unsigned short y,ISA color);
extern  void FocusLineChange(struct ListBoxData *TestList,int amt);
extern  void JunkTree(PTREE tree);
extern  void InitGlobalFocus(unsigned short focusstart);
extern  PENTRY AddFile(PTREE tree);
#endif
#include <errno.h>

extern void  Get_CWD(char *);
extern void InvalidateIconCache(AX axLeft, AY ayTop, AX axRight, AY ayBottom);
extern VOID ListBoxPath(PTREE tree,PENTRY node,int whichtree) ;
extern VOID RefreshStartPrograms(VOID);
extern void MarkGlobalFocus(WORD focus) ;
extern void HandleSpecialCaseNovell(PTREE tree) ;
extern void RefreshViewFileScreen(BOOL fClearAndRedraw) ;
extern VOID MySetColor(WORD color);
extern BOOL IsMenuDown(void) ;
extern void SetGlobalFocusBox(WORD GlobalFocus) ;

extern BOOL gMouseDown;
extern int gNumHelps; /* number of times help has been re-entered */
extern GlobalIdle(void);
extern char far *gIconCache;
extern BOOL gfRepaintonidle;

/*
**							  Global variables
*/
global_t glob;						
char gFMGRStatus[FMGRSTATUSMAX];
extern struct ListBoxData ProgramList;
BOOL PDisable ; // Whether print.com has been run or not.
BOOL gfFMVisited = FALSE ; // says whether the FM was ever visited by the user.

/* Flag specifying whether sorts are by ascending/descending order. */
WORD gDescendingOrder ;

/* List boxes */
struct ListBoxData TreeList[2];
struct ListBoxData FileList[2];

filelistinfo listinfo[2];	 /* binds tree to list box -- 1 extra for Search */

drivelist    gDriveList[NUMDRIVELISTS];
BYTE	     gHighlightedDrive[2];
BYTE	     gSelectedDrive[2];

/* Menu stuff for DoFileMgr
*/
extern MENUINFO MainMenuBar;
extern MENUINFO FileMgrMenuBar;

extern BOOL gfFileMgrStartUp ; /*whether user starts up in FileMgr mode or not*/

extern WORD gCnx;

/* Message Bar text displayed at bottom of screen. This is used to restore 
 * screen in case we put up a swap floppy message and destroy it.
 */
char *gpszNonSwap ;

BOOL gfFMBeingInited ;

/*Number of spaces to the right of the ADD mode msg at the bottom of screen*/
#define ADD_MSG_RSIDE_SPACES 10 
#define TIME_STR_LEN 6

extern WORD gMouseY;
/* Displays the message 'message' at bottom of screen in the color 'isa'. In
 * graphics mode, a box is also drawn at the bottom row.
 */
VOID MessageBar(char *message, ISA isa,WORD force)
{
	WORD i;
	WORD j;
	char datestring[12];
	static char timestring[12];
	char padded[92] ; // shell won't run in any mode > 90 col mode!!
	BOOL turnmouseoff;
	int ADDMessageLen ;

	if(isa == isaMenu)
		isa = isaMessagebar;

	ADDMessageLen = strlen(szAdd) ;

#ifdef KANJI
	if ( ((force==-1) ||
		 Get_Date_and_Time_Strings(0, 0, datestring,timestring,force))
		 && (!fKkcEnabled || !FActiveKkc()) )
#else
	if ((force==-1) ||
		 Get_Date_and_Time_Strings(0, 0, datestring,timestring,force))
#endif
    {
       for(i=0; (message[i] && (i < axMac)); i++)
		   padded[i]=message[i];

	  for(j=i;j<axMac-ADD_MSG_RSIDE_SPACES-ADDMessageLen;j++)
		      padded[j] = ' ';

	  // implicit/explicit (ADD mode) state of focused list box
	  switch(WhoHasGlobalFocus())
	  {
			case FILE0:
			case FILE1:
				// Explicit or Add mode
				if(FileList[WhoHasGlobalFocus() == FILE1].mode == 0)
				{
					strncpy(padded+j, szAdd, ADDMessageLen) ;
					padded[j+ADDMessageLen] = ' ' ; // put a trailing blank
					j += (ADDMessageLen+1) ;
					break;
				}
				// else fall through

			default:
	  			for(;j<=axMac-ADD_MSG_RSIDE_SPACES;j++)
		      	padded[j] = ' ';

	  }
     padded[j++] = ' ';
	  i = j;
	  for(; j<i+TIME_STR_LEN; j++)
			  padded[j] = timestring[j-i];
     padded[j++] = ' ';
	  i = j;

     for(;i<axMac;i++)
		   padded[i] = ' ';

	   if (message != szSwapDiskMessage)
		   gpszNonSwap = message ; // gpszNonSwap is used by TakeDownSwapMsg

		if(gMouseY >=ayMac-2)
		{
		   turnmouseoff = TRUE;
         FEnableMouseNest(FALSE);
		}
		else
		{
			turnmouseoff = FALSE;
		}
#if 1
		 /* In order to reduce screen flicker, we draw each character
        * one at a time, and draw the line above it before continuing
		  */
       if (gisgraph)
       {
            WORD fore, back;

		   	SetAreaPat(0);
		   	SetLinePat(1);
            GetIsaColor(isaBackground, &fore, &back);
            MySetColor(fore);
       }
		 for(i=0;i<axMac;i++)
		 {
#ifdef KANJI
			if (IsDBCSLeadByte(padded[i]))
			{
				CharOut(&MainWind,(RX) i,(RY) ayMac-1,(padded[i]&0xff)|(padded[i+1]<<8),isa);
				i++;
			}
			else
				CharOut(&MainWind,(RX) i,(RY) ayMac-1,padded[i],isa);
#else
			CharOut(&MainWind,(RX) i,(RY) ayMac-1,padded[i],isa);
#endif
       	if (gisgraph)
       	{
	   		if(CHEIGHT > SMALLHEIGHT)
	   		{
		   		Move(i*CWIDTH,((ayMac-2)+1)*CHEIGHT);
		   		Draw((i+1)*CWIDTH,((ayMac-2)+1)*CHEIGHT);
	   		}
       	}
		 }
#endif
#if 0
       TextOut(&MainWind,0,(RY) ayMac-1,padded,axMac,isa);

       /* Draw box around message bar at bottom of screen. */
       if (gisgraph)
       {
	   	if(CHEIGHT > SMALLHEIGHT)
	   	{
		   	SetAreaPat(0);
		   	SetLinePat(1);
		   	SetColor(0,0x7FFF);
		   	Move(0,((ayMac-2)+1)*CHEIGHT);
		   	Draw((axMac)*CWIDTH,((ayMac-2)+1)*CHEIGHT);
	   	}
       }
#endif
		 if(turnmouseoff)
          FEnableMouseNest(TRUE);
    }
} /* MessageBar */

/* This routine is called from assembly with stack pointing to a local
 * stack of 256 bytes only!! So, don't use too many locals or have too
 * many nested function calls!!
 */
void PutUpSwapMsg(int swapdrive)
{
	extern unsigned char gDriveLetterOffset ; /* offset of drive letter ('?') in str */

	/* low byte of  'swapdrive' specifies the code for new drive  */
	/* high byte of 'swapdrive' specifies the code for disk in drive */
	szSwapDiskMessage[gDriveLetterOffset] = (uchar)('A' + (swapdrive & 0x0F)) ;

	/* ZZZZZ should this be Beep() ? If beeps disabled, it will
	 *  probably not attract user attention
	 */
	Shell_Beep() ;

	/* -1 as last param specifies not to make DOS call to get time */
	MessageBar(szSwapDiskMessage, isaAlert, -1) ;
	Shell_Beep() ;
}

void TakeDownSwapMsg(void)
{
	/* -1 as last param specifies not to make DOS call to get time */
	MessageBar(gpszNonSwap, isaMenu, -1) ;
}

#ifdef YUKKY
VOID DriveMessage(char *message)
{
    char padded[256];
    int i;
    for(i=0;((message[i]) && (i<axMac));i++)
	padded[i]=message[i];
    for(;i<axMac;i++)
    {
	padded[i] = ' ';
    }
    TextOut(&MainWind,(RX) 0,(RY) 2,padded,axMac,isaDrivebox);
}
#endif

/****	FileMgrStatusBar - display current directory in status bar
**
**	ENTRY
**		tree - tree of current directory
**		node - node of current directory
**	EXIT
**		none
*/
VOID FileMgrStatusBar(PTREE tree, PENTRY node)
{
    WORD i;
	 static BYTE lastlen=255;
    BYTE len;
    char padded[256];
	 int dummylen ;

	 if (!IsMenuDown())
	 {
    	padded[0] = ' ';
    	padded[1] = ' ';
    	Tree2Path(tree, node, &padded[2], &dummylen);
    	len = (BYTE) strlen(padded);
    	for(i=len;i<min(axMac,lastlen);i++)
			padded[i]=' ';
	 	lastlen = len;
    	FEnableMouseNest(FALSE);
    	TextOut(&MainWind,0,(RY) FMGRSB,padded,i,isaDrivebox);
    	FrameMenuBar(&MainWind);
    	FEnableMouseNest(TRUE);
	 }
}

VOID CalcDriveIconPositions(void)
{
   BYTE    IconX;
	BYTE	IconY;
	int 	i;

    IconX = DRIVEICONSTARTX;
    IconY = DRIVEICONSTARTY;

		for(i=0;i<glob.DriveCount;i++)
		{
			gDriveList[0][i].IconX = IconX;
			gDriveList[0][i].IconY = IconY;
			gDriveList[1][i].IconX = IconX;
			gDriveList[1][i].IconY = IconY+ (ayMac-LISTBOXTOP)/2 + 2 ;

			IconX += DRIVESHIFT;
			if(i== (DRIVESPERLINE-1) )
			{
				IconX = DRIVEICONSTARTX;
				++IconY;
			}
		}
 }



/*
 * Given a drive number 0 = default, 1 = A, 2 = B, etc. returns the drive
 * type FLOPPY_TYPE, or REMOTE_TYPE, or HARDDISK_TYPE, or 0 for 
 * non-existence.
 * USES intdos function!!
 * if intdos function was unsuccesful returns drivetype of 0.
*/
int getdrivetype(int driveno) ; /* function written in assembly */


/*
**							     Procedures
*/
/****	InitFileMgr - one-time initialization of file manager structures
**
**
**	ENTRY
**		none
**	EXIT
**		none
**	EFFECTS:
**	    voluminous.
*/
VOID InitFileMgr()
{
	char c;
	char test[1+MAX_PATH];
	treehdr far *tree;
	treehdr far *lasttree = NULL;
	int typ;
	BYTE	whichdriveicon;
	int token ;

	gfFMBeingInited = TRUE ;

	// Set lists focus to 0 as default; may get reset later if swap in
	Set_List_Focus(&FileList[0],0);
	Set_List_Focus(&FileList[1],0);

	strcpy(test, "A:\\");
    glob.DriveCount = 0;
	/*
	 * WARNING! Assumes Token order of sortkeys!!!!
	 */
	glob.SortKey = Get_KeyWord_Assignment(TK_SAVESTATE,TK_SORTKEY)-
			 TK_NAME;
	if ((glob.SortKey < SORT_NAME) || ( glob.SortKey > SORT_DISK))
	    glob.SortKey = SORT_NAME ;
	SortCmp = SortFnArr[glob.SortKey] ;

    for (c='A'; c <= 'Z'; c++)
    {
	    test[0] = c;
#if 0
		/* We don't make any such assumptions anymore, previously, we
		 * used to hit the floppy to figure out what it was!
		/* If c == 'A' or 'B' we assume its presence and is local-removable */
	    if (c < 'C')
			typ = FLOPPY_TYPE;
		else
#endif
		/* translate 'A' to 1, 'B' to 2, etc. */
		if (!(typ = getdrivetype(c - 'A' +1)))
			continue; /* This drive letter doesn't exist so, check next one */

		/* drive exists, add to list.  */
		tree = LpbAllocWorkFar(sizeof(treehdr));
		// printf("tree struct size=%d\n", sizeof(treehdr)) ; 
		if (!tree)
		{
			OutOfMemory();
			DoExit();
#ifdef DEBUG
			printf("*** Couldn't read drives\n") ;
			exit(0) ;
#endif
		}

		gDriveList[0][glob.DriveCount].tree = tree;
		gDriveList[1][glob.DriveCount].tree = tree;

		glob.DriveCount++;

		if (lasttree)
			lasttree->next = tree;
		else
			glob.drives = tree;
		lasttree = tree;
		
		/* Init tree structure.
		*/
		strfcpy(tree->root, test);
		strcpy(glob.MatchPat, szStarDotStar);
		tree->next = NULL;
		tree->head = NULL;
		tree->pagehead = NULL;
		tree->pagetail = NULL;
		tree->freeind = PAGESIZE ; /* will mean no free entry in page */
		tree->holecount = 0 ; /* Initially no holes are present */
		tree->DirCount = 0;
		tree->VisibleDirCount = 0;
		tree->filecount = 0;
		tree->Diskfilecount = 0;
		tree->FirstFile = NULL;
		tree->LastFile = NULL;
		tree->FirstDirectory = NULL;
		tree->LastDirectory = NULL;
		tree->Started = FALSE;
		tree->Compacted = FALSE;
		tree->fdiskinfoknown = FALSE ;
		tree->SortRequired = TRUE ;

		tree->SelDir = NULL ; // Initial default directory for each drive is
									 // its root directory.
		tree->SelLine = 0 ;	 // Root directory will be the first line (0).

	 	/* Default value for mpat */
		strfcpy(tree->mpat, szStarDotStar) ;
		tree->nummatches = RECALCNUMMATCHES;
		tree->skey = tree->tmode = -1 ; /* Garbage - Non-Sort order/tree mode */
		
		/* Initially, the initialization to "matchespattern" bit for each file
		 * is set to TRUE, hence this is equivalent to being TRUE for the 
		 * tree!
		 */
		tree->DisplayHiddenFiles = TRUE ;

	   tree->NumSel = 0 ; /* Initially no file in tree is selected */
		tree->SizeSel = 0 ;
		/*
		 * We only get this information here&now so we can check the volume
		 * label to see if it is the Microsoft Ramdrive (c)...We don't have
		 * a reliable way to detect any other ramdisk
		 */

		if(typ == HARDDISK_TYPE)
		{
		   if (GetDiskInfo(tree, tree->VolLabel, &tree->SizeTotal,
																			&tree->SizeAvail))
		   {
			   tree->fdiskinfoknown = TRUE ;
			   if (	(fstrncmp(tree->VolLabel, szMSRamdrive,
																strlen(szMSRamdrive)) == 0) ||
			   		(fstrncmp(tree->VolLabel, szVDisk, strlen(szVDisk)) == 0) ||
			   		(fstrncmp(tree->VolLabel, szRDv, strlen(szRDv)) == 0)
					)
					typ = RAMDRIVE_TYPE;
		   }
		}
		tree->DriveType = typ;
		     
	}

	Get_CWD(test); //internal getcwd command
   c = test[0];
	tree = glob.drives;

	CalcDriveIconPositions();
	whichdriveicon = 0;
	while (tree && tree->root[0] != c)
	{
		++whichdriveicon;
		tree = tree->next;
	}
    
	// Init globals. 
	gHighlightedDrive[0] = whichdriveicon;
	gHighlightedDrive[1] = whichdriveicon;
	gSelectedDrive[0] = whichdriveicon;
	gSelectedDrive[1] = whichdriveicon;

	/* Initialize the two trees to have default as ROOT dir & 1st file of it */
	listinfo[0].tree = listinfo[1].tree = tree;
	listinfo[0].files = listinfo[1].files = NULL;

	/* The trees in the queue of length 2 (last 2) that retain selections
	   even when trees are switched for display */
	glob.SelTree[0] = glob.SelTree[1] = tree ;
	glob.SelRepInd = 0 ;
    
	/* ZZZZ At start up time don't allow starting in System Tree Mode unless
	 * we are coming back from a program launching from the shell. In this
	 * case we succesfully had been in system tree mode. This is because
	 * we could run into compact tree mode!!
	 */

	/* WARNING! Assumes ordering and the numbers 1 thru 3 */
	glob.TreeMode = Get_KeyWord_Assignment(TK_SAVESTATE,TK_FILEMGRMODE) - 
					     TK_SINGLETREE + 1 ;
	/* Maps it to a number 1 thru 3 which are TR_SINGLE thru TR_SYSTEM */
#ifndef NOGROUPINFILE
	if ( (glob.TreeMode < TR_SINGLE) || (glob.TreeMode > TR_SYSTEM) )
		glob.TreeMode = TR_SHARE;
#else
	if ( (glob.TreeMode < TR_SINGLE) || (glob.TreeMode > TR_SYSTEM) )
		glob.TreeMode = TR_SINGLE ;
#endif

	/* glob.MaxTree will be set right when we get into the correct tree mode */
	glob.FocusBox = glob.MaxTree = 0; 
	MarkGlobalFocus(DRIVELIST0) ;

	/* Safe value is glob.VerifyOverwrite = TRUE; */
	token = Get_KeyWord_Assignment(TK_SAVESTATE, TK_RCONFIRM) ;
	if (token == TK_DISABLED)
		glob.VerifyOverwrite = FALSE ;
	else
		glob.VerifyOverwrite = TRUE ;

	/* Safe value is glob.VerifyDelete = TRUE; */
	token = Get_KeyWord_Assignment(TK_SAVESTATE, TK_DCONFIRM) ;
	if (token == TK_DISABLED)
		glob.VerifyDelete = FALSE ;
	else
		glob.VerifyDelete = TRUE ;

	/* Safe value is glob.CrossDirSel = FALSE; */
	token = Get_KeyWord_Assignment(TK_SAVESTATE, TK_CROSSDIRSEL) ;
	if (token == TK_ENABLED)
		glob.CrossDirSel = TRUE ;
	else
		glob.CrossDirSel = FALSE ;

	/* Safe value is glob.MouseConfirm = TRUE; */
	token = Get_KeyWord_Assignment(TK_SAVESTATE, TK_MCONFIRM) ;
	if (token == TK_DISABLED)
		glob.MouseConfirm = FALSE ;
	else
		glob.MouseConfirm = TRUE ;

	/* By default, do not display hidden/system files */
	token = Get_KeyWord_Assignment(TK_SAVESTATE, TK_DISPHIDDENFILES) ;
	if (token == TK_ENABLED)
		glob.DisplayHiddenFiles = TRUE ;
	else
		glob.DisplayHiddenFiles = FALSE ;

	/* By default, we sort in ascending order */
	token = Get_KeyWord_Assignment(TK_SAVESTATE, TK_SORTORDER) ;
	if (token == TK_DESCENDING)
		gDescendingOrder = MAGIC_XCHG_MASK ;
	else
		gDescendingOrder = 0 ;


	/* Select root directory & its first file by default */
	glob.lineselected[0] = glob.lineselected[1] = 0 ;

	PDisable = Check_Spooler() ;

	gfFMBeingInited = FALSE ;

} /* proc InitFileMgr */

void AddFileManAccelerators(void);
/*
 * Sets up the File Manager window for use by drawing menu,title,messages
 * and erasing content region
 */
VOID SetUpScreen(void)
{
	RRC rrcClient;
	ISA isa ;

	/* The sequence here is important, so drawing will look good
    * even when going slowly
    */
	UpdateMainTitleBar(szDOSShellTitle);
#ifndef NOGROUPINFILE
	if ( (glob.TreeMode == TR_SHARE) && (WhoHasGlobalFocus() == GROUPBOX) )
	{
		 setmenubar(&MainMenuBar,&MainWind);
	}
	else
	{
		setmenubar(&FileMgrMenuBar,&MainWind);
	}
#else
	setmenubar(&FileMgrMenuBar,&MainWind);
#endif
	AddFileManAccelerators();
	// FrameMenuBar(&MainWind);

	/* draw top portion of screen where drives go */
	GetClientRrc(&MainWind,&rrcClient);
	rrcClient.ryTop +=2; /* exclude title and menu and drive status */
	rrcClient.ryBottom =rrcClient.ryTop+3;

	isa = (ISA) ((glob.TreeMode == TR_SEARCH) ? isaBackground : isaDrivebox) ;
	FillRrc(&MainWind,&rrcClient,' ',isa);

	/* else no drive icons region exists in Search Mode */

	/* draw bottom portion of screen where list boxes will go*/
	GetClientRrc(&MainWind,&rrcClient);
	rrcClient.ryTop +=5; /* exclude title and menu and drivebox */
	rrcClient.ryBottom -=1; /*exclude status bar */
	FillRrc(&MainWind,&rrcClient,' ',isaBackground);

	/* draw the drive box background for the double tree */
	if(glob.TreeMode == TR_DOUBLE)
	{
		rrcClient.ryTop = Get_List_Rect(&TreeList[0]).ayBottom+!gisgraph;
		rrcClient.ryBottom = rrcClient.ryTop+3;
		FillRrc(&MainWind,&rrcClient,' ',isaDrivebox);
	}
#ifndef NOGROUPINFILE
	if(glob.TreeMode == TR_SHARE)
	{
		rrcClient.ryTop = Get_List_Rect(&TreeList[0]).ayBottom+!gisgraph;
		rrcClient.ryBottom = rrcClient.ryTop+1;
		FillRrc(&MainWind,&rrcClient,' ',isaDrivebox);
	}
#endif

	if (glob.TreeMode == TR_SEARCH)
		MessageBar(szSearchMessage, isaMenu, TRUE);
	else
		MessageBar(szFileMessage, isaMenu,TRUE);
    /*
     * Since we just erased the screen, the file & program icons
     * are no longer there, so we must redraw them
     */
	if(gisgraph)
	{
		InitIconCache();
	}

} /* SetUpScreen */
void RefreshFMScreen(BOOL erase)
{
	int ith ;
	int filefocusline ;

	TakeDownBusyDialog(0);
	TakeDownBusyDialog(1);
	
	if(erase)
		SetUpScreen() ;
	FileMgrStatusBar(listinfo[glob.FocusBox].tree,
						listinfo[glob.FocusBox].files);

	for (ith = 0 ; ith <= glob.MaxTree ; ith++ )
	{
#ifndef NOGROUPINFILE
		if((ith == 0) && (glob.TreeMode == TR_SHARE))
		{
		    RefreshStartPrograms();
		    listinfo[1].UpdateDrives = FALSE;

		}
		if((ith == 0) || ((ith ==1) &&(glob.TreeMode != TR_SHARE)))
#endif
		{
		  listinfo[ith].UpdateDrives = TRUE;
		  filefocusline = Get_List_Focus(&FileList[ith]) ;

		  // Not_Blank(&TreeList[ith]);
		  // Not_Blank(&FileList[ith]);
		  // DoRedisplayList(&TreeList[ith]);
		  // DoRedisplayList(&FileList[ith]);
		  UpdateListBox(&TreeList[ith]);
		  UpdateListBox(&FileList[ith]);
		  // FocusLineChange(&TreeList[ith], glob.lineselected[ith]) ;

		  // FocusLineChange(&FileList[ith], filefocusline) ;
		}
	}
	if ( (glob.TreeMode == TR_SYSTEM) && (WhoHasGlobalFocus() != FILE0) )
		gfFlatLeftInfoDrawn = FALSE ;

} /* RefreshFMScreen */

/****	DoFileMgr - run file mgr windows (activated from menu)
**	This fn enables the file manager portion of the shell.  It turns
**	on the list boxes, and changes the menu bar.
**
**	EFFECTS:
**	    enables listboxes TreeList and FileList, inits several members of
**	glob.
*/

/* Following variable is used to inform the routines DoSingleTree,
   DoDoubleTree, DoFlatDisplay as to whether they have been called from
   DoFileMgr at startup or from the menu pull downs. */

BOOL ginitial = FALSE ; /* This has to be the default state */

extern VOID FAR DoShareMode(void);


/* Mark the glob variable and also the INI parsed stuff as being in the FM */
void MarkAsInFM(void)
{
	glob.InFileMgr = TRUE;
	Set_KeyWord_Assignment(TK_SAVESTATE,TK_STARTUP,TK_FILEMGR);
	gfFMVisited = TRUE ;
} /* MarkAsInFM */


VOID DoFileMgr(void)
{
#ifndef NOLOADER
	if ( gfFileMgrStartUp && GET_WAIT_FLAG() )
	{
		gfFileMgrStartUp = FALSE ; /* Reset it back to FALSE -- default state */
		/* ZZZZZ */
		/* Time to swap tree structure in !! */
		DoSwapIn() ;
	}
#endif

	 /* The variable ginitial is looked at by the DoSingle... routines to
	  * decide whether to sort the tree, etc.
	  */
    ginitial = TRUE  ;

    switch(glob.TreeMode)
    {
		case TR_DOUBLE:
		    DoDoubleTree();	/* init for double tree display	*/
	    	break;

		case TR_SYSTEM:
		    DoFlatDisplay();
		    break;
#ifndef NOGROUPINFILE
		case TR_SHARE:
		    DoShareMode();
		    break;
#endif
		default:
			DoSingleTree();				/* init for single tree display */
    }
	/* swap in the tree now if it is there.
	 * do it after the window has been drawn so the user is
	 * not presented with a blank screen for too long
	 */

	ginitial = FALSE ;

	MarkAsInFM() ;

} /* proc DoFileMgr */


/* Puts the tree back in original shape -- i.e., the state in which it was
	before we entered the search mode. */
void HandleSearchQuit(void)
{
	/* Re-enable the menu-items that were non-reachable in Search Mode */
	EnableDisableForSearchMode(TRUE, TRUE) ;

	strcpy(glob.MatchPat, gSavedMatchPat) ;
	MarkAllTreeMatches(listinfo[0].tree, FALSE) ;
	glob.TreeMode = gSavedTreeMode ;
	SortCmp = SortFnArr[glob.SortKey = gSavedSortOrder] ;

	/* Clear all selections when leaving Search mode! */
	if (listinfo[0].tree->NumSel > 0)
		DeselectTree(listinfo[0].tree) ;

	/* Sort the tree & put it in appropriate state -- ZZZZ If bg sort is taken
		off then this can be removed?? */
	if (glob.TreeMode == TR_SYSTEM)
		SystemSort(listinfo[0].tree, FALSE) ;
	else
#ifdef OLDCOMPLETETREESORT
		FormDirectoryOrder(listinfo[0].tree, FALSE) ;
#else
		if (listinfo[0].tree->skey != glob.SortKey)
		{
			SetUpTreeForSort(listinfo[0].tree) ;
			SortDirectory(listinfo[0].tree, NULL) ;
			SortDirectory(listinfo[0].tree, listinfo[0].files) ;
		}
#endif

	/* Restore the global focus back to what it was before the search was
	 * initiated. If the file listbox has 0 files now, move it to the
	 * corresponding directory listbox to its left.
	 */
	if (gSavedGlobalFocus == FILE0)
	{
		if (GetNumItems(&FileList[0]) == 0)
			gSavedGlobalFocus = TREE0 ;
	}
	else if (gSavedGlobalFocus == FILE1)
	{
		if (GetNumItems(&FileList[1]) == 0)
			gSavedGlobalFocus = TREE1 ;
	}

	SetGlobalFocusBox(glob.FocusId = gSavedGlobalFocus) ;

} /* HandleSearchQuit */

/* Stores information like SortOrder, SortKey, etc (the persistent FM
 * state from the global variables into the INIPARSE data structure.
 */
/* ZZZZZZ If exiting is slow because of this, we can do these assignements
 * each time these values change!!
 */
void StoreFMState(void)
{
    Set_KeyWord_Assignment(TK_SAVESTATE, TK_SORTKEY, (TK_NAME+glob.SortKey));

    Set_KeyWord_Assignment(TK_SAVESTATE, TK_SORTORDER, 
								(gDescendingOrder ? TK_DESCENDING : TK_ASCENDING));
    Set_KeyWord_Assignment(TK_SAVESTATE, TK_DISPHIDDENFILES, 
								(glob.DisplayHiddenFiles ? TK_ENABLED : TK_DISABLED));
    Set_KeyWord_Assignment(TK_SAVESTATE,TK_RCONFIRM,
								(glob.VerifyOverwrite ? TK_ENABLED : TK_DISABLED));
    Set_KeyWord_Assignment(TK_SAVESTATE,TK_DCONFIRM,
								(glob.VerifyDelete ? TK_ENABLED : TK_DISABLED));
    Set_KeyWord_Assignment(TK_SAVESTATE,TK_MCONFIRM,
								(glob.MouseConfirm ? TK_ENABLED : TK_DISABLED));
    Set_KeyWord_Assignment(TK_SAVESTATE,TK_CROSSDIRSEL,
								(glob.CrossDirSel ? TK_ENABLED : TK_DISABLED));
} /* StoreFMState */

/****	ExitFileMgr - leave File Manager section of shell
**	It returns to the main Shell screen and menu.
**
**	EFFECTS:
**	    removes lists boxes, restores main menu bar.  Modifies
**	glob.InFileMgr.
*/
VOID FAR ExitFileMgr()
{
	if (glob.TreeMode == TR_SEARCH)
		HandleSearchQuit() ;
    TakeDownBusyDialog(0);
    TakeDownBusyDialog(1);
    glob.InFileMgr = FALSE;
    Set_KeyWord_Assignment(TK_SAVESTATE,TK_STARTUP,TK_STARTPGRMS);

	 StoreFMState() ;

    InitializeStartPrograms();
}

#ifndef NOGROUPINFILE
VOID DoStartProgramsIdle(void);
#endif

/****	FileMgrIdle - run bground processing for File Manager
**	    This fn updates the file mgr list boxes, and adds another disk
**	file to the displayed trees.
**	EXIT:
**	    TRUE if no background tasking, FALSE otherwise
**	EFFECTS:
**	    updates screen, adds a file to the visible tree(s).  
*/
BOOL FileMgrIdle()
{
	unsigned i;										// current list box
	BOOL notreallyidle;
#if 0  
	MouseIdle(); //now done in mouse idle!
#endif
	notreallyidle = gMouseDown;

	// If the tree has not been read in yet, begin reading it in.
	// This is where we read the tree in the background.
	// Don't do it when in help (since graphics takes so much memory)
	// or when in a dialog box not in file manager (unneccessary speed
	// hit)
	if ((!((gCnx == cnxDialog) && (!glob.InFileMgr))) && !(gNumHelps>=0))
	{
	     for (i=0; i <= glob.MaxTree; i++)
	     {
		     if (!listinfo[i].tree->Started)
		     {
			     listinfo[i].tree->Started = TRUE;
			     listinfo[i].tree->ContinueTree = TRUE;

			     /* The sort modes for the tree will be set on actual sorting! */
			     /* tree->mpat is always assumed to be "*.*" at read in time */
			     /* It is initilaized in InitFileMgr that way. Thus all file nodes
				start with matchespattern field = TRUE at start */
		     }
		     // only if interactive:if (!Is_List_Captured(&TreeList[i]))
		     notreallyidle |= listinfo[i].tree->ContinueTree;
		     UpdateFileTree(i);
	     }
	}
    // Update screen if in File Manager. Don't do if a dialog is
    // up or a menu is down
    if ((glob.InFileMgr) && (gCnx==cnxNull))
    {
		// Update any listbox sections which need it.
		for (i=0; i <= glob.MaxTree; i++)
		{
			if (listinfo[i].UpdateDrives)
			{
				UpdateDrives();
				listinfo[i].UpdateDrives = FALSE;
			}
			/* ZZZZ Is this check needed or will we have UpdateFiles, etc TRUE
				only if tree is completely read in? */
			if(!listinfo[i].tree->ContinueTree)
			{
			   TakeDownBusyDialog(i);
			   if (listinfo[i].UpdateTree)
			   {
					/* Invalidate the icon cache in the tree listbox region! */
					/* The tree listbox is going to be cleared and re-drawn*/
					InvalidateIconCache(Get_List_Rect(&TreeList[i]).axLeft,
									Get_List_Rect(&TreeList[i]).ayTop,
									Get_List_Rect(&TreeList[i]).axRight,
									Get_List_Rect(&TreeList[i]).ayBottom);
				   DoRedisplayList(&TreeList[i]);
				   listinfo[i].UpdateTree = FALSE;
				   notreallyidle = TRUE;
			   }
			   if (listinfo[i].UpdateFiles)
			   {
#if 0
					/* Invalidate the icon cache in the file listbox region! */
					/* The file listbox is going to be cleared and re-drawn*/
					InvalidateIconCache(Get_List_Rect(&FileList[i]).axLeft,
									Get_List_Rect(&FileList[i]).ayTop,
									Get_List_Rect(&FileList[i]).axRight,
									Get_List_Rect(&FileList[i]).ayBottom);
#endif
				   FileList[i].blank = TRUE;
				   DoRedisplayList(&FileList[i]);

					/* If mode is Implicit mode, automatically select first file in
					 * File listbox. IF cross-dir-selection is ON we don't want to
					 * do such default selection as it is potentially dangerous.
					 */
					if ( (FileList[i].mode) && (!glob.CrossDirSel) )
						ListKey(&FileList[i], ' ', 0) ;

				   listinfo[i].UpdateFiles = FALSE;
				   notreallyidle = TRUE;
			   }

			   notreallyidle |= !ListBoxIdle(&FileList[i]);
			   notreallyidle |= !ListBoxIdle(&TreeList[i]);
			}
		} /* for */
#ifndef NOGROUPINFILE
		if(glob.TreeMode == TR_SHARE)
		{
		    DoStartProgramsIdle();
		}
#endif
    } /* if in FileMgr */

	/* if the last dialog/menu up didn't repaint, do it now */
	if(gfRepaintonidle)
	{
		if(gCnx == cnxNull)
		{
			InitIconCache();
			if(glob.InFileMgr)
				RefreshFMScreen(FALSE);
			else
			{
				if (m_fPerformingViewFile())
					/* Don't clear and redraw */
					RefreshViewFileScreen(FALSE) ;
				else
					RefreshStartPrograms();
			}
			gfRepaintonidle = FALSE;
	   }
	}

    return(!notreallyidle);
} /* proc FileMgrIdle */

/****	UpdateFileTree - Add a file to the current drive tree.
**	This fn adds a file to the listbox's tree; if it is a directory then the
**	directory list box is refreshed.
**
**	ENTRY
**		slot - which list box set: 0 if not in 2-tree mode.  In two-tree mode,
**			0 = upper list box set
**			1 = lower list box set
**	EXIT
**		TRUE if work done, FALSE if not
*/
VOID UpdateFileTree(set)
int set;

{
	treehdr far *tree = listinfo[set].tree;	// the listbox tree
	PENTRY ret;								// NULL if at end of files
	int i ;
 /*
	NOTE: doing multiple addfiles here does not enhance speed
	significantly.
	It could mess up compact mode anyway.
 */
	/* If there are more entries to add to the tree or file boxes,
	** format the next line and add it to the tree box.
	*/
	if (tree->ContinueTree)
	{
		ret = AddFile(tree);
		if(ret)
		{
		    if(glob.InFileMgr)
		       PutUpBusyDialog(set);
		}
		else
		{
			/* Tree has been completely read in. So, now sort the tree
			   in appropriate order, Mark All matches correctly if pattern
			   has changed. Get Disk Information for system tree mode 
			   Redisplay the file tree(s). */
			tree->ContinueTree = FALSE;

			if (tree->Compacted)
			{
				/* No files have been read into this tree. Note: In
				   compact mode only directories are read in. */
				for (i = 0 ; i <= glob.MaxTree ; i++)
				{
					if ( (listinfo[i].tree == tree)  && 
					      ( (i!=1) || (listinfo[1].tree!=listinfo[0].tree) ||
									listinfo[0].files != listinfo[1].files ) )
						LoadCompactDir(listinfo[i].tree, listinfo[i].files) ;
				}

			}
#ifdef OLDSORT
			if ( (glob.TreeMode == TR_SYSTEM) && (glob.SortKey != SORT_DISK) )
				SystemSort(listinfo[0].tree, TRUE) ;
			else
				FormDirectoryOrder(tree, TRUE) ;
#else
			tree->SortRequired = TRUE ;
#endif

			HandleSpecialCaseNovell(tree) ;

			MarkAllTreeMatches(tree, FALSE) ;

			/* Mark the 'lastdir' bit for all directory nodes!! */
			Marklastdir(tree) ;

			if (glob.InFileMgr)
			{
				/* If the same tree is being displayed in
				 * two windows, disable both dialogs, otherwise
				 * only disable the right one!
				 */
				if(listinfo[set].tree->head == listinfo[0].tree->head)
				{
				   TakeDownBusyDialog(0);
				}
				if(listinfo[set].tree->head == listinfo[1].tree->head)
				{
				   TakeDownBusyDialog(1);
				}

				DoRedisplayList(&TreeList[set]) ;
				DoRedisplayList(&FileList[set]) ;
		    }
		}
	}
} /* proc UpdateFileTree */

/* In System tree mode (flat mode), erase the left part of the screen where
 * the "showInfo" stuff is drawn.
 */
void EraseFlatLeft(void)
{
	RRC rrcClient;

	/* FileList[0]'s rectangle will be the system tree mode's right side box */
	rrcClient.rxRight = Get_List_Rect(&FileList[0]).axLeft - 1	;
	rrcClient.rxLeft = 0 ;
	rrcClient.ryTop = Get_List_Rect(&FileList[0]).ayTop ;
	rrcClient.ryBottom = Get_List_Rect(&FileList[0]).ayBottom ;
    FillRrc(&MainWind,&rrcClient,' ',isaBackground);


}  /* EraseFlatLeft */


void SetFocusDrive(WORD list, BYTE whichdriveicon)
{
    gHighlightedDrive[list] = whichdriveicon ;
}

BYTE GetFocusDrive(WORD list)
{
    return(gHighlightedDrive[list]);
}

BYTE GetSelectedDrive(WORD list)
{
    return(gSelectedDrive[list]);
}


/****	DrawDrive - paint disk icon and letter in appropriate spot
**
**	ENTRY
**	EXIT
**	WARNING:
**	EFFECTS:
*/
VOID DrawDrive(WORD list,WORD drive,ISA highlight)
{	  
	char letter;
	WORD x, y;
	treehdr far *drivetree;
	BITMAP *pbitmap ;
	int seldrive;

	if(glob.TreeMode == TR_SEARCH)
	   return;

	drivetree = gDriveList[list][drive].tree;
	
	letter = drivetree->root[0];
	x = gDriveList[list][drive].IconX;
	y = gDriveList[list][drive].IconY;
	seldrive = GetSelectedDrive(list);
	if (gisgraph)
	{
	     switch(drivetree->DriveType)
	     {
		case HARDDISK_TYPE :
		   pbitmap = (drive==seldrive) ? HDIconInvert : HDIcon ;
		   break ;

		case RAMDRIVE_TYPE :
		   pbitmap = (drive==seldrive) ? RamDriveIconInvert : RamDriveIcon ;
		   break ;

		case CDROM_TYPE :
		   pbitmap = (drive==seldrive) ? CDRomIconInvert : CDRomIcon ;
		   break ;

		case FLOPPY_TYPE :
		   pbitmap = (drive==seldrive) ? FloppyIconInvert : FloppyIcon ;
		   break ;

		case REMOTE_TYPE :
		default:
		   pbitmap = (drive==seldrive) ? RemoteIconInvert : RemoteIcon ;
		   break ;
		 } /* switch */

		if(!(gIconCache && (gIconCache[y*MAXCOLS+x] == (char) (drive == seldrive))))
		{
			 PlotBmp(pbitmap, x*CWIDTH, y*CHEIGHT,isaDrivebox);
			if(gIconCache)
				gIconCache[y*MAXCOLS+x] = (char)(drive == seldrive);
		}
		CharOut(&MainWind, (RX) x+3, (RY) y, letter, highlight);

	}
	else
	{
	   CharOut(&MainWind, (RX) x, (RY) y, '[', highlight);
	   CharOut(&MainWind, (RX) x+1, (RY) y, letter, highlight);
	   CharOut(&MainWind, (RX) x+2, (RY) y, ':', highlight);
	   CharOut(&MainWind, (RX) x+3, (RY) y, ']', highlight);
	}

	
} /* proc DrawDrive */

PTREE HighlightDrive(WORD list, WORD drive, ISA highlight)
{
	WORD i,numlists;
	BYTE focusdrive ;

	numlists = (glob.TreeMode == TR_DOUBLE) ? 2 : 1;

	if ( (highlight==isaHilite) || (highlight == isaSelect) )
		focusdrive = gHighlightedDrive[list] = (BYTE) drive ;
	else
		focusdrive = gHighlightedDrive[list] ;


	FEnableMouseNest(FALSE) ; // will help reduce mouse flicker!

	if(highlight==isaSelect)
	{
		 DrawDrive(list, gSelectedDrive[list]=(BYTE)drive,
					(ISA) ((focusdrive == (BYTE)drive) ? isaHilite : isaSelect));
	}
	else
	if(highlight==isaHilite)
	{
		DrawDrive(list, drive, (ISA) ((gHighlightedDrive[list] == (BYTE)drive) ? isaHilite :isaSelect));
	}
	else /* This means that we want to draw plain drive selection icon! */
		DrawDrive(list, drive, (ISA) (gSelectedDrive[list]==(BYTE)drive?isaSelect:isaDriveicon));

    for (i = 0; i < glob.DriveCount; i++)
		if (i != drive)
		{
		   if(gSelectedDrive[list] == (BYTE) i)
			   DrawDrive(list, i, (ISA)((gHighlightedDrive[list] == gSelectedDrive[list]) ? isaHilite : isaSelect));
		   else
			   DrawDrive(list, i, (ISA) ((gHighlightedDrive[list] == (BYTE)i) ? isaHilite :isaDriveicon));

		}

	FEnableMouseNest(TRUE) ;

	return(gDriveList[list][drive].tree) ;
}

VOID SelectDrive(WORD list,WORD drive)
{
	BOOL newtree;
	PTREE drivetree;

	drivetree = HighlightDrive(list,drive,isaSelect);

	newtree = (drivetree != listinfo[0].tree) || 
	 						(glob.MaxTree && (drivetree != listinfo[1].tree));

	/* ZZZZ Is there a problem with foll. in compact mode? */
	if (newtree && (drivetree->Started) && !drivetree->ContinueTree)
	{
		if (glob.TreeMode == TR_SYSTEM)
			SystemSort(drivetree, FALSE) ;
		else
#ifdef OLDCOMPLETETREESORT
			FormDirectoryOrder(drivetree, FALSE) ;
#else
			SetUpTreeForSort(drivetree) ;
			 /* sort root directory of drive tree */
		SortDirectory(drivetree, NULL) ;
#endif

		MarkAllTreeMatches(drivetree, FALSE) ;
	}
	/* The new tree selected might be a different guy from	the trees
	 * already seen
	 */
	if ((drivetree != glob.SelTree[0]) && (drivetree != glob.SelTree[1]))
	{
		glob.SelTree[glob.SelRepInd] = drivetree ;
		glob.SelRepInd = !glob.SelRepInd ;/* toggle SelRepInd */
	}
	else
	{
		/* tree selected is one of the trees already in our
		 * queue update the index to the guy not selected. Check
		 * glob.SelTree[1] as it is always valid
		 */
		glob.SelRepInd = (drivetree == glob.SelTree[1]) ? 0 : 1 ;
	}

	/* Take busy dialog down, anyway! In case we currently have busy dialog
	 * up with say drive 'C' and he does a "Ctrl+C" we would take down
	 * the busy dialog and then put it back up!
	 */
	TakeDownBusyDialog(list) ;

	/* In System tree mode, erase the flatleft portion of screen? */
	if (glob.TreeMode == TR_SYSTEM)
		EraseFlatLeft() ;

	listinfo[list].tree = drivetree;
	listinfo[list].files = drivetree->SelDir ; // put back in last sel dir
	glob.lineselected[list] = drivetree->SelLine ;  // check mark line

	// move focus to first file.
	Set_List_Focus(&FileList[list], 0);

	// move focus to selected directory.
	Set_List_Focus(&TreeList[list], 0);
	FocusLineChange(&TreeList[list], glob.lineselected[list]) ;

	/* ZZZZZ As a side effect, any files that were previously
	 * selected in the file listbox would get deselected.
	 * If I don't do this LoadCompactDir() sometimes it will say no
	 * files in the root dir (even thouh there are files).
	 */
	if ( (drivetree->Started) &&
			(!drivetree->ContinueTree) &&
			(drivetree->Compacted) &&
			(newtree)
		 )
	{
		LoadCompactDir(drivetree, listinfo[list].files) ;
		listinfo[list].UpdateFiles = TRUE ;
	}

	/* In SystemTree mode, When we do FlatLeft this is taken care of correctly!!*/
	/* Update the directory status line displaying selected dir. */
	FileMgrStatusBar(drivetree, listinfo[list].files) ;

	ListBoxPath(drivetree, listinfo[list].files, list) ;

	/* trash our icon cache as the tree box, file box are going to be cleared
	 * and re-drawn again if we put a busy dialog, etc.
	 */
	InitIconCache() ;

	InsertListItem(&TreeList[list], 0) ;  
	InsertListItem(&FileList[list], 0) ;

	gfFlatLeftInfoDrawn = FALSE ;
}

VOID UpdateDrives(void)
{
#if 0
	WORD i,j;
	WORD numlists;
#endif

	ISA highlight1, highlight2 ;

	/* By default assume that the drive icons are not highlighted! */
	highlight1 = highlight2 = isaDriveicon ;

	if (WhoHasGlobalFocus() == DRIVELIST0)
		highlight1 = isaHilite ;
	else
	/* In our case DRIVELIST1 and GROUPBOX have the same #define!! */
	if (WhoHasGlobalFocus() == DRIVELIST1)
			highlight2 = isaHilite ;

	HighlightDrive(0, GetFocusDrive(0), highlight1) ;

	if (glob.TreeMode == TR_DOUBLE)
		HighlightDrive(1, GetFocusDrive(1), highlight2) ;
} /* UpdateDrives */


void DriveListKey(WORD list,WORD key, WORD state)
{
	BYTE lastfocus ;

	UnReferenced(state) ;

	lastfocus = GetFocusDrive(list);
	switch(key)
	{
		case VK_RIGHT:
			   lastfocus = (lastfocus+1)%glob.DriveCount;
			   HighlightDrive(list,lastfocus,isaHilite);
			break;

		case VK_LEFT:
			   lastfocus = (lastfocus+glob.DriveCount-1)%glob.DriveCount;
			   HighlightDrive(list,lastfocus,isaHilite);
			break;

		case ' ':
			   SelectDrive(list,lastfocus);
			break;

		case '\r':
				/* previously Shift+Enter used to cause re-reading! */
			   // if (state & KK_SHIFT)
					JunkTree(gDriveList[list][lastfocus].tree) ;

				/* ZZZZ should I save currently viewed dir on this box and
				 * try to restore it later! Will be difficult as disk info
				 * could have changed and this dir might not be present
				 * on re-read!
				 */
				 if ((glob.TreeMode == TR_DOUBLE) &&
							(listinfo[list].tree == listinfo[1-list].tree))
				 {
					 listinfo[1-list].files = NULL;	  // root directory
					 glob.lineselected[1-list] = 0 ;

				 }

				 SelectDrive(list,lastfocus);
			break;
	} /* switch */
} /* DriveListKey */

BOOL DriveMouse(WORD mx,WORD my,WORD msg)
{
    WORD i,j,numlists;

    /*
     * when we are in the search "dialog" (haha) the drives
     * are not supposed to be accessable.
     */
    if(glob.TreeMode == TR_SEARCH)
		return(FALSE);

	numlists = (glob.TreeMode == TR_DOUBLE) ? 2 : 1;
#ifndef NODIRECT
    if((msg == WM_LBUTTONDOWN) ||
       (msg == WM_LBUTTONDBLCLK) ||
       ((msg == WM_MOUSEMOVE) && (gMouseDown)) )
#else
    if((msg == WM_LBUTTONDOWN) || (msg == WM_LBUTTONDBLCLK))
#endif
		for (i = 0; i < glob.DriveCount; i++)
			for(j = 0; j < numlists ; j++)
			{
				if ((my == gDriveList[j][i].IconY) &&
						(mx >= gDriveList[j][i].IconX) &&
						(mx <= gDriveList[j][i].IconX+4))
				{
					/* Drive needs to be re-read in case of a mouse
					 * double-click! It will happen in the file manager
					 * idle cycle if we junk the tree now.
					 */
					if(msg == WM_LBUTTONDBLCLK)
					{
						JunkTree(gDriveList[j][i].tree) ;

						/* ZZZZ should I save currently viewed dir on this box and
						 * try to restore it later! Will be difficult as disk info
						 * could have changed and this dir might not be present
						 * on re-read!
						 */
						 if ((glob.TreeMode == TR_DOUBLE) &&
									(listinfo[j].tree == listinfo[1-j].tree))
						 {
							 listinfo[1-j].files = NULL;	  // root directory
							 glob.lineselected[1-j] = 0 ;
						 }

					}
#ifndef NODIRECT
				       if (msg != WM_MOUSEMOVE)
				       {
					  SelectDrive(j,i);
				       }
				       else
				       {
					    if(gMouseDown)
					       HighlightDrive(j,i,isaHilite);
				       }
#else
					  SelectDrive(j,i);
#endif
					InitIconCache() ;

					/* Move the focus to the drive icons! */
					InitGlobalFocus((j == 0) ? DRIVELIST0 : DRIVELIST1) ;

					return(TRUE);
				}
			}
    return(FALSE);
}

/* DoDiskReread() is invoked from the menu. The selected drive is the
 * one we want to re-read instead of the focus drive?
 */
void DoDiskReread(void)
{
	SetFocusDrive(glob.FocusBox, GetSelectedDrive(glob.FocusBox) ) ;

	/* Make it look like the "Enter" key was pressed to re-read the disk */
	DriveListKey(glob.FocusBox, '\r', 0) ;

	InitGlobalFocus(glob.FocusBox ? DRIVELIST1 : DRIVELIST0) ;
		
} /* DoDiskReread */

/* Function is called when user presses Ctrl+F5 -- Keyboard accelerator
 * This function should be called only from within the FM. Note that
 * this assumption holds right now as the accelerator Ctrl+F5 is active
 * only when the FM menu is put up!
 */
void RereadSelectedDir(void)
{
	/* If in System Tree mode (All Files mode), Ctrl+F5 does not mean
	 * anything really. We could do a DoDiskReread in this case if we
	 * wanted -- same as F5.
	 */
	if ( (glob.TreeMode == TR_SEARCH) || (glob.TreeMode == TR_SYSTEM) )
	{
		Shell_Beep() ;
		return ;
	}

	/* Note that we must have started reading this tree as we are in the FM */ 
	assert(listinfo[glob.FocusBox].tree->Started) ;


	/* Also, if the tree has not been fully read in as yet, don't do
	 * anything. Ofcourse, this could be treated the same as an F5.
	 */
	if (listinfo[glob.FocusBox].tree->ContinueTree)
		return ;

	/* Basically, if we are here the mode is TR_SINGLE, TR_DOUBLE, or
	 * TR_SHARE.
	 */
	assert ( (glob.TreeMode == TR_SINGLE) ||
			   (glob.TreeMode == TR_DOUBLE) ||
			   (glob.TreeMode == TR_SHARE)
			 ) ;

	LoadCompactDir(listinfo[glob.FocusBox].tree,
															listinfo[glob.FocusBox].files) ;
	listinfo[glob.FocusBox].UpdateFiles = TRUE ;

	/* See if we in dual tree mode and if the same directory is displayed
	 * in the other listbox too. In that case, we need to update it too.
	 */
	if ( (glob.TreeMode == TR_DOUBLE) &&
		  (listinfo[0].tree == listinfo[1].tree) &&
		  (listinfo[0].files == listinfo[1].files)
		)
	{
		FileList[1-glob.FocusBox].blank = TRUE;
		DoRedisplayList(&FileList[1-glob.FocusBox]);
	}
	
} /* RereadSelectedDir */
