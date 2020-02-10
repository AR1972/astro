;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <icons.h>
#include <text.h>
#include <assert.h>

extern void SetFocusDrive(WORD list, BYTE whichdriveicon) ;
extern int MouseConfirmationDialog(char *message, char *dpath) ;
extern VOID MakeDirNameFit(char *longname,char *pattern,char *destination,int maxlen) ;
extern BOOL FIsExecutableFile(char far *ext) ;
extern unsigned CountFilesToOperateOn(void) ;

extern WORD gEditBoxModifier;
extern void ProgramCursor(void) ;
extern void Arrow(void) ;
extern void Set_Task_Name(char far *name);

BOOL gfManipulating = FALSE; /* is something being manipulated? */
BYTE gManipType = 0;
PTREE gSrcManipTree;
PTREE gDestManipTree;

PENTRY gSrcManipNode;
PENTRY gDestManipNode;

#define GetNumOfManipulatedFiles() gNumManipulatedFiles

unsigned int gNumManipulatedFiles ;

extern BOOL DestIsDir;

extern void Cancel(void);

int gTreeSaveFocus[2];
int gFileSaveFocus[2];
WORD gFocusBeforeManipStart ;
extern struct ListBoxData TreeList[2];
extern BOOL gNeedToUpdateFileList[2];

MCB gProgramCursor;

extern BOOL gNotMonochrome;

VOID Arrow(void)
{
		WORD fore,back;
	 
		/* Set the hot spot of the mouse to the tip of the cursor - top left */
      gProgramCursor.colHot = 0;
      gProgramCursor.rowHot = 0;

		/* first set up the graphics cursor to an Arrow symbol. */
      gProgramCursor.rgwXorMaskGfx[0]  = 0x0000;
      gProgramCursor.rgwXorMaskGfx[1]  = 0x0000;
      gProgramCursor.rgwXorMaskGfx[2]  = 0x4000;
      gProgramCursor.rgwXorMaskGfx[3]  = 0x6000;
      gProgramCursor.rgwXorMaskGfx[4]  = 0x7000;
      gProgramCursor.rgwXorMaskGfx[5]  = 0x7800;
      gProgramCursor.rgwXorMaskGfx[6]  = 0x7c00;
      gProgramCursor.rgwXorMaskGfx[7]  = 0x7e00;
      gProgramCursor.rgwXorMaskGfx[8]  = 0x7f00;
      gProgramCursor.rgwXorMaskGfx[9]  = 0x7c00;
      gProgramCursor.rgwXorMaskGfx[10] = 0x6c00;
      gProgramCursor.rgwXorMaskGfx[11] = 0x4600;
      gProgramCursor.rgwXorMaskGfx[12] = 0x0600;
      gProgramCursor.rgwXorMaskGfx[13] = 0x0300;
      gProgramCursor.rgwXorMaskGfx[14] = 0x0300;
      gProgramCursor.rgwXorMaskGfx[15] = 0x0000;


      gProgramCursor.rgwAndMaskGfx[0]  = ~0x0000;
      gProgramCursor.rgwAndMaskGfx[1]  = ~0xc000;
      gProgramCursor.rgwAndMaskGfx[2]  = ~0xe000;
      gProgramCursor.rgwAndMaskGfx[3]  = ~0xf000;
      gProgramCursor.rgwAndMaskGfx[4]  = ~0xf800;
      gProgramCursor.rgwAndMaskGfx[5]  = ~0xfc00;
      gProgramCursor.rgwAndMaskGfx[6]  = ~0xfe00;
      gProgramCursor.rgwAndMaskGfx[7]  = ~0xff00;
      gProgramCursor.rgwAndMaskGfx[8]  = ~0xff80;
      gProgramCursor.rgwAndMaskGfx[9]  = ~0xffc0;
      gProgramCursor.rgwAndMaskGfx[10] = ~0xfe00;
      gProgramCursor.rgwAndMaskGfx[11] = ~0xef00;
      gProgramCursor.rgwAndMaskGfx[12] = ~0xcf00;
      gProgramCursor.rgwAndMaskGfx[13] = ~0x0780;
      gProgramCursor.rgwAndMaskGfx[14] = ~0x0780;
      gProgramCursor.rgwAndMaskGfx[15] = ~0x0380;

		/* now set up the text cursor */
		if((ginst.coMac <= 2) || (!gNotMonochrome) || fMonochrome)       // in monochrome, we must use block
	   {
			GetIsaColor(isaBlackOnWhite,&fore,&back);
			gProgramCursor.wAndMaskText = 0;
#ifdef JAPAN
			gProgramCursor.wXorMaskText =   fore << 16 | back << 8 | '\x7f'; //? this is wrong'\xb0';
#else
			gProgramCursor.wXorMaskText =   fore << 16 | back << 8 | '²'; //? this is wrong'\xb0';
#endif
	   }
	   else
	   {
	GetIsaColor(isaShellMouse, &fore, &back);
	gProgramCursor.wAndMaskText = 0x00FF;
	gProgramCursor.wXorMaskText = (back<<12) | (fore<<8);
	   }
      SetMouseCursor(&gProgramCursor);
}


#ifndef NODIRECT
#if 0
WORD CancelCursorData[] = 
{                                                               /* 1234123412341234     */
	0x03C0,                                 /*            XXXX                              */
	0x0C30,                                 /*     XX    XX            */
	0x1008,                            /*    X        X        */
   0x2804,                                      /*   X X        X       */
	0x2404,                                 /*   X  X       X       */
   0x4202,                                      /*  X    X       X      */
	0x4102,                                 /*  X     X      X      */
	0x4082,                       /*  X      X     X      */
	0x4042,                       /*  X       X    X      */
	0x2024,                       /*   X       X  X       */
	0x2014,                       /*   X        X X       */
	0x1008,                                 /*    X        X                */
	0x0C30,                                 /*     XX        XX                     */
	0x03C0,                                 /*              XXXX                            */
	0x0000,                                                                                                   
	0x0000
	
};

#endif

WORD CancelCursorData[] = 
{                                                               /* 1234123412341234     */
	0x03C0,                                 /*            XXXX                              */
	0x0FF0,                                 /*     XXXXXXXX            */
	0x1C38,                            /*    XXX    XXX        */
   0x381C,                                      /*   XXX      XXX       */
	0x340C,                                 /*   XX X      XX       */
   0x6206,                                      /*  XX   X      XX      */
	0x6106,                                 /*  XX    X     XX      */
	0x6086,                       /*  XX     X    XX      */
	0x6046,                       /*  XX      X   XX      */
	0x302C,                       /*   XX      X XX       */
	0x381C,                       /*   XXX      XXX       */
	0x1C38,                                 /*    XXX    XXX                */
	0x0FF0,                                 /*     XXXXXXXX                         */
	0x03C0,                                 /*              XXXX                            */
	0x0000,                                                                                                   
	0x0000
	
};


WORD CancelCursorMask[] = 
{                                                               /* 1234123412341234     */
	0x03C0,                                 /*            XXXX                              */
	0x0FF0,                                 /*     XXXXXXXX            */
	0x1FF8,                            /*    XXXXXXXXXX        */
   0x3FFC,                                      /*   XXXXXXXXXXXX       */
	0x3FFC,                                 /*   XXXXXXXXXXXX       */
   0x7FFE,                                      /*  XXXXXXXXXXXXXX      */
	0x7FFE,                                 /*  XXXXXXXXXXXXXX      */
	0x7FFE,                       /*  XXXXXXXXXXXXXX      */
	0x7FFE,                       /*  XXXXXXXXXXXXXX      */
	0x3FFC,                       /*   XXXXXXXXXXXX       */
	0x3FFC,                       /*   XXXXXXXXXXXX       */
	0x1FF8,                                 /*    XXXXXXXXXX                */
	0x0FF0,                                 /*     XXXXXXXX       */
	0x03C0,                                 /*              XXXX             */
	0x0000,                                                                                                   
	0x0000
	
};


WORD FileCursorData[] = 
{                                                               /* 1234123412341234     */
	0xFFC0,                                 /*      XXXXXXXXXX                              */
	0x8060,                                 /* X        XX                     */
	0xBB50,                            /* X XXX XX X X         */
   0x8048,                                      /* X        X  X        */
	0xB744,                                 /* X XX XXX X   X       */
   0x8042,                                      /* X        X    X      */
	0xBB7F,                                 /* X XXX XX XXXXXXX     */
	0x8001,                       /* X              X     */
	0x8001,                       /* X              X     */
	0x8001,                       /* X              X     */
	0x8001,                       /* X              X     */
	0xFFFF,                                 /* XXXXXXXXXXXXXXXX             */
	0x0000,
	0x0000,
	0x0000,
	0x0000
	
};

WORD FileCursorMask[] = 
{                                                               /* 1234123412341234     */
	0xFFC0,                                 /*      XXXXXXXXXX                              */
	0xFFE0,                                 /* XXXXXXXXXXX                     */
	0xFFF0,                            /* XXXXXXXXXXXX         */
   0xFFF8,                                      /* XXXXXXXXXXXXX        */
	0xFFFC,                                 /* XXXXXXXXXXXXXX       */
   0xFFFE,                                      /* XXXXXXXXXXXXXXX      */
	0xFFFF,                                 /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                       /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                       /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                       /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                       /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                                 /* XXXXXXXXXXXXXXXX             */
	0x0000,
	0x0000,
	0x0000,
	0x0000
	
};


WORD ProgramCursorData[] = 
{                                                               /* 1234123412341234     */
	0xFFFF,                                 /*      XXXXXXXXXXXXXXXX                */
	0x8001,                                 /* X                    X     */
	0x8001,                            /* X              X     */
   0xFFFF,                                      /* XXXXXXXXXXXXXXXX     */
	0x8001,                                 /* X              X     */
   0x8001,                                      /* X              X     */
	0x8001,                                 /* X              X     */
	0x8001,                       /* X              X     */
	0x8001,                       /* X              X     */
	0x8001,                       /* X              X     */
	0x8001,                       /* X              X     */
	0xFFFF,                                 /* XXXXXXXXXXXXXXXX             */
	0x0000,
	0x0000,
	0x0000,
	0x0000
	
};

WORD ProgramCursorMask[] = 
{                                                               /* 1234123412341234     */
	0xFFFF,                                 /*      XXXXXXXXXXXXXXXX                */
	0xFFFF,                                 /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                            /* XXXXXXXXXXXXXXXX     */
   0xFFFF,                                      /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                                 /* XXXXXXXXXXXXXXXX     */
   0xFFFF,                                      /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                                 /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                       /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                       /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                       /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                       /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                                 /* XXXXXXXXXXXXXXXX             */
	0x0000,
	0x0000,
	0x0000,
	0x0000
	
};


WORD MultiCursorData[] = 
{                                                               /* 1234123412341234     */
	0x0FFF,                                 /*          XXXXXXXXXXXX                */
	0x0801,                                 /*     X                X     */
	0x0801,                            /*     X          X     */
   0x3801,                                      /*   XXX          X     */
	0x2801,                                 /*   X X          X     */
   0x2801,                                      /*   X X          X     */
	0xE801,                                 /* XXX X          X     */
	0xAFFF,                       /* X X XXXXXXXXXXXX     */
	0xA004,                       /* X X          X       */
	0xA004,                       /* X X          X       */
	0xBFFC,                       /* X XXXXXXXXXXXX       */
	0x8010,                                 /* X          X                 */
	0x8010,                                 /*      X                         X                     */
	0xFFF0,                                 /* XXXXXXXXXXXX                 */
	0x0000,
	0x0000
	
};

WORD MultiCursorMask[] = 
{                                                               /* 1234123412341234     */
	0x0FFF,                                 /*          XXXXXXXXXXXX                */
	0x0FFF,                                 /*     XXXXXXXXXXXX     */
	0x0FFF,                            /*     XXXXXXXXXXXX     */
   0x3FFF,                                      /*   XXXXXXXXXXXXXX     */
	0x3FFF,                                 /*   XXXXXXXXXXXXXX     */
   0x3FFF,                                      /*   XXXXXXXXXXXXXX     */
	0xFFFF,                                 /* XXXXXXXXXXXXXXXX     */
	0xFFFF,                       /* XXXXXXXXXXXXXXXX     */
	0xFFFC,                       /* XXXXXXXXXXXXXX       */
	0xFFFC,                       /* XXXXXXXXXXXXXX       */
	0xFFFC,                       /* XXXXXXXXXXXXXX       */
	0xFFF0,                                 /* XXXXXXXXXXXX                 */
	0xFFF0,                                 /*      XXXXXXXXXXXX                    */
	0xFFF0,                                 /*      XXXXXXXXXXXX                    */
	0x0000,                                                                                                 
	0x0000
	
};

VOID ProgramCursor(void)
{
		WORD fore,back;
		int i;
		WORD *thisicon;
		WORD *thismask;

		/* Make the hot spot of the program cursor its center */
      gProgramCursor.colHot = 8;
      gProgramCursor.rowHot = 8;

		
		if(GetNumOfManipulatedFiles() > 1)
		{
			thisicon = MultiCursorData;
			thismask = MultiCursorMask;
		}
		else
      if(FIsExecutableFile(gSrcManipNode->name+NAMELEN))
		{
			thisicon = ProgramCursorData;
			thismask = ProgramCursorMask;
		}
		else
		{
			thisicon = FileCursorData;
			thismask = FileCursorMask;
		}
		for(i=0;i<16;i++)
		{
			gProgramCursor.rgwXorMaskGfx[i] =  thisicon[i] ^ thismask[i];
			gProgramCursor.rgwAndMaskGfx[i] =  ~thismask[i];        
		}
    GetIsaColor(isaShellMouse, &fore, &back);
    gProgramCursor.wAndMaskText = 0;
#ifdef JAPAN
    gProgramCursor.wXorMaskText = (back<<12) | (fore<<8) | (BYTE)'\xa5';
#else
    gProgramCursor.wXorMaskText = (back<<12) | (fore<<8) | '';
#endif
      SetMouseCursor(&gProgramCursor);

}

VOID Cancel(void)
{
		WORD fore,back;
		int i;

		/* Set the hot spot of the Cancel cursor -- (a NOT circle) to the
		 * its center.
		 */
      gProgramCursor.colHot = 8;
      gProgramCursor.rowHot = 8;

		for(i=0;i<16;i++)
		{
			gProgramCursor.rgwXorMaskGfx[i] =  CancelCursorData[i] ^ CancelCursorMask[i];
			gProgramCursor.rgwAndMaskGfx[i] =  ~CancelCursorMask[i];        
		}

    GetIsaColor(isaShellMouse, &fore, &back);
    gProgramCursor.wAndMaskText = 0;
#ifdef JAPAN
    gProgramCursor.wXorMaskText = (back<<12) | (fore<<8) | '!';
#else
    gProgramCursor.wXorMaskText = (back<<12) | (fore<<8) | '';
#endif
      SetMouseCursor(&gProgramCursor);
}


#endif

/* sets the manipulation type to MANIP_MOVE or MANIP_COPY -- called by
 * fn MonitorManipulation().
 */
void SetMoveOrCopyManipulation(void)
{
	/* If destination tree is a different one, DEFAULT is copy, else move */
	if (gDestManipTree != gSrcManipTree)
	{
		if (gEditBoxModifier & KK_ALT) // is the alt key is pressed
			gManipType = MANIP_MOVE ;
		else
			gManipType = MANIP_COPY ;
	}
	else
	{
		if (gEditBoxModifier & KK_CONTROL) // is the ctrl key is pressed
			gManipType = MANIP_COPY ;
		else
			gManipType = MANIP_MOVE ;
	}
} /* SetMoveOrCopyManipulation */


VOID BeginManipulation(treehdr far *tree, PENTRY node)
{
	int i;

	if (gfManipulating)
		return;

	if ( (!node) || (node->attribs & _A_SUBDIR) )
	{
		return;
	}

	gfManipulating = TRUE;
	gSrcManipTree = tree;
	gSrcManipNode = node;

	gNumManipulatedFiles = CountFilesToOperateOn() ;

	for(i=0; i<=glob.MaxTree; ++i)
	{
	gTreeSaveFocus[i] = TreeList[i].focusitem;
	gFileSaveFocus[i] = FileList[i].focusitem;
    }
	 /* Remember the following, as we need to restore the focus location
	  * when we complete the direct manipulation operation.
	  */
	 gFocusBeforeManipStart = WhoHasGlobalFocus() ;
    ProgramCursor();
}

VOID EndManipulation(void)
{
	char cpath[MAX_PATH+1];
	char tempbuffer[256];
	char LaunchInDir[MAX_PATH+1];
	char ProgName[NAMELEN+EXTLEN+2] ;
	char ArgName[NAMELEN+EXTLEN+2] ;
	char ProgTitle[NAMELEN+EXTLEN+2];
	int i;
	int dummylen ;
	int ret ;

	if(!gfManipulating)
		return;

	Arrow();
	for(i=0; i<=glob.MaxTree; ++i)
	{
	TreeList[i].focusitem = gTreeSaveFocus[i];
	TreeList[i].ListProc(tmmSetFocus, NULL, gTreeSaveFocus[i],
												TreeList[i].tmc, 0, 0, 0);
	TreeList[i].update = TRUE;
	TreeList[i].nextlinetoupdate = 0;
	gNeedToUpdateFileList[i] = listinfo[i].UpdateFiles = FALSE;
	FileList[i].focusitem = gFileSaveFocus[i];
	FileList[i].ListProc(tmmSetFocus, NULL, gFileSaveFocus[i],
															FileList[i].tmc, 0, 0, 0);
	FileList[i].update = TRUE;
	FileList[i].nextlinetoupdate = 0;
	}

	/* Put back the focus to where it was before starting the direct
	 * manipulation. Also, in case we went through drive icons the focus
	 * and selection icons may be different. Fix these also.
	 */
   InitGlobalFocus(gFocusBeforeManipStart) ;

	SetFocusDrive(0, GetSelectedDrive(0) ) ;
	if (glob.MaxTree)
		SetFocusDrive(1, GetSelectedDrive(1) ) ;

	switch(gManipType)
	{
	  case MANIP_RUN:
			if (glob.MouseConfirm)
			{
				Internal2Normal(ProgName, gDestManipNode->name) ;
				Internal2Normal(ArgName, gSrcManipNode->name) ;
				FormStringWithoutPlaceHolders(tempbuffer,szMouseLaunchProgConfirm,
						(char far *)ProgName, (char far *)ArgName) ;

				/* Argument 2 -- destination path is dummy for this case! */
				if (!MouseConfirmationDialog(tempbuffer, NULL))
					break ;
			}
			
			/* Use tempbuffer to hold the programstring now. */
			Tree2Path(gDestManipTree, gDestManipNode, tempbuffer, &dummylen);
			strcat(tempbuffer,szBlank);
			Tree2Path(gSrcManipTree, gSrcManipNode, cpath, &dummylen);
			strcat(tempbuffer,cpath);

			/* ZZZZZ Is following correct?? */
			Tree2Path(gDestManipTree, FindParent(gDestManipNode),LaunchInDir, &dummylen);

			Internal2Normal(ProgTitle,gDestManipNode->name);
			Set_Task_Name(ProgTitle);

			DoCommand(tempbuffer, LaunchInDir);
			break;

	  case MANIP_MOVE:
	  case MANIP_COPY:
	     Tree2Path(gDestManipTree, gDestManipNode, cpath, &dummylen) ;

		  /* Put up confirmation dialog on direct manipulation before
			* performing the move or copy to the destination directory
			* if the user wants to be prompted for mouse confirm.
			*/
		  if (glob.MouseConfirm)
		  {
				/* The following code is optimized by the compiler correctly */
				if (gManipType == MANIP_MOVE)
					ret = MouseConfirmationDialog(szMouseMoveFilesConfirm, cpath) ;
				else
					ret = MouseConfirmationDialog(szMouseCopyFilesConfirm, cpath) ;
				
				/* Did the user cancel the mouse operation */
				if (!ret)
					break ;
		  }

		  /* We can only do direct manipulation of files right now!
			* fn DoFileOp() looks at who has focus before deciding whether to
			* perform file operation or directory operation.
			*/
		  if ((WhoHasGlobalFocus() != FILE0) && (WhoHasGlobalFocus() != FILE1))
		  {
				Shell_Beep() ;
		  }
		  else
		  {
				/* DoFileOp looks at this BOOL. This indicates whether the path
				 * we are passing to DoFileOp() is a directory or not.
				 */
				DestIsDir = TRUE ;
				if (gManipType == MANIP_MOVE)
				{
					gpszFileOpCaption = szMoveFileCaption ;
					DoFileOp(OP_MOVE, cpath) ;
				}
				else
				{
					gpszFileOpCaption = szCopyFileCaption ;
					DoFileOp(OP_COPY, cpath) ;
				}
		  }
		  break ;

	 default:
		 break ;
	} /* switch */

	MessageBar((glob.TreeMode == TR_SEARCH) ? szSearchMessage : szFileMessage,
																					isaMenu,TRUE);
	gfManipulating = FALSE;
	gManipType = 0;
}

extern BYTE GetFocusDrive(WORD list);
extern drivelist    gDriveList[NUMDRIVELISTS];
extern BOOL gfActivate;
extern BOOL FIsExecutableFile(char far *ext) ;


/* This function is invoked when the mouse is moved to a spot where the
 * file(s) that has(have) been dragged can't be dropped. The mouse cursor is
 * turned into a "Cancel" like symbol. A diagonal bar within a circle.
 */
void CancelDirectManipulation(char *Message)
{
	gManipType = 0;
	strcat(Message,szCancel);
	Cancel();
} /* CancelDirectManipulation */

/*      INTERNATIONALIZE HERE!!!
 * This the amount of columns reserved for the time string, manip
 * string's beginning section (namely "Copy file-name to") and the
 * message saying ADD mode or not at the right.
 */
#define STATUS_RESERVED 40

/* When one is performing direct manipulation, every time the mouse is moved
 * this routine is invoked. This sets up the mouse cursor appropriately
 * and also the status bar message at the bottom of the screen is also
 * updated to reflect what will happen if the mouse btton goes up.
 * (x, y) are the current mouse co-ordinates on the screen. Origin (0,0)
 * is the top left of screen.
 */
VOID MonitorManipulation(BYTE x, BYTE y)
{
	static char Message[91];
	char dest_path[MAX_PATH+1] ;
	char CompressedDirPath[65] ;
	int dummylen ;
	char Name1[15];
	char Name2[15] ;
	char *pszSrc, *pszDest ;
	int ith;
	int lastfocus;
	WORD GlobalFocus ;

	if(!gfManipulating)
		return;

	strcpy(Message,szBlank);

	GlobalFocus = WhoHasGlobalFocus() ;

	if(y <= 2)
	{
		CancelDirectManipulation(Message) ;
	}
	else
    switch (GlobalFocus)
    {
	   case DRIVELIST1:
	   case DRIVELIST0:
			if ( (GlobalFocus == DRIVELIST1) && (glob.TreeMode == TR_SHARE) )
			{
				CancelDirectManipulation(Message) ;
				break;
			}

			ith = WhoHasGlobalFocus() == DRIVELIST0 ? 0 : 1;
			lastfocus = GetFocusDrive(ith);

			/* Is the mouse outside the drive icon region? The drive icon in
			* question is the one that has the focus.
			*/
			if (    (gDriveList[ith][lastfocus].IconY != y)||
					(x < gDriveList[ith][lastfocus].IconX) ||
					(x >= (gDriveList[ith][lastfocus].IconX + (BYTE) (DRIVESHIFT-1)))
				)
			{
				CancelDirectManipulation(Message) ;
				break;
			}

			ProgramCursor();

			gDestManipTree = gDriveList[ith][lastfocus].tree;
			gDestManipNode = gDestManipTree->SelDir ;

			/* sets the variable gManipType */
			SetMoveOrCopyManipulation() ;

			Tree2Path(gDestManipTree, gDestManipNode, dest_path, &dummylen) ;
			MakeDirNameFit(dest_path, NULL, CompressedDirPath,
																	(axMac - STATUS_RESERVED));
			if (GetNumOfManipulatedFiles() > 1)
			{
				pszSrc = szFiles ;
			}
			else
			{
				Internal2Normal(Name1, gSrcManipNode->name) ;
				pszSrc = Name1 ;
			}

			dummylen = strlen(Message) ;

			FormStringWithoutPlaceHolders(Message+dummylen,
				(gManipType == MANIP_MOVE) ? szMoveStatusBarMsg : szCopyStatusBarMsg,
				(char far *)pszSrc, (char far *)CompressedDirPath) ;

			break;

	   case FILE1:
			if(glob.TreeMode == TR_SHARE)
			{
				CancelDirectManipulation(Message) ;
			break;
			}
		case FILE0:
			ith = ((GlobalFocus == FILE0) ? 0 : 1) ;

			/* Is the mouse outside the file listbox region? */
			if (    (y < Get_List_Rect(&FileList[ith]).ayTop) || 
					(y >= Get_List_Rect(&FileList[ith]).ayBottom)
				)
			{
				CancelDirectManipulation(Message) ;
				break;
			}

			gDestManipTree = listinfo[ith].tree;
			gDestManipNode = ( (glob.TreeMode == TR_SYSTEM) ||
					( (glob.TreeMode == TR_SEARCH) && (gfSearchDisk) ) ) ?
					GetNthFlatMatchedFile(gDestManipTree, Get_List_Focus(&FileList[ith])) :
					GetNthMatchedFile(listinfo[ith].files, Get_List_Focus(&FileList[ith]), gDestManipTree) ;

			if((!gDestManipNode) ||
			(!FIsExecutableFile(gDestManipNode->name+NAMELEN)) ||
				(gDestManipNode == gSrcManipNode))
			{
			/* is not executable (or null!) */
				CancelDirectManipulation(Message) ;
			}
			else
			{
				ProgramCursor();
				gManipType = MANIP_RUN;
				dummylen = strlen(Message) ;

				Internal2Normal(Name1, gDestManipNode->name) ;
				Internal2Normal(Name2, gSrcManipNode->name) ;
				FormStringWithoutPlaceHolders(Message+dummylen, szRunStatusBarMsg,
						(char far *)Name1, (char far *)Name2) ;
			}
			break;

		case TREE1:
			if(glob.TreeMode == TR_SHARE)
			{
				CancelDirectManipulation(Message) ;
				break;
			}

		case TREE0:

			ith = ( (GlobalFocus == TREE0) ? 0 : 1) ;

			/* Is the mouse outside the directory listbox region? */
			if (    (y < Get_List_Rect(&TreeList[ith]).ayTop) || 
					(y >= Get_List_Rect(&TreeList[ith]).ayBottom)
				)
			{
				CancelDirectManipulation(Message) ;
				break;
			}

			ProgramCursor();

			gDestManipTree = listinfo[ith].tree;
			gDestManipNode = Get_List_Focus(&TreeList[ith])?GetNthVisibleDir(gDestManipTree, Get_List_Focus(&TreeList[ith])) : NULL;

			/* sets the variable gManipType */
			SetMoveOrCopyManipulation() ;

			dummylen = strlen(Message) ;

			if(GetNumOfManipulatedFiles() > 1)
			{
				pszSrc = szFiles ;
			}
			else
			{
				Internal2Normal(Name1, gSrcManipNode->name) ;
				pszSrc = Name1 ;
			}

			if(gDestManipNode)
			{
			Internal2Normal(Name2, gDestManipNode->name) ;
				pszDest = Name2 ;
			}
			else
			{
				pszDest = RootName ;
			}

			FormStringWithoutPlaceHolders(Message+dummylen,
				(gManipType == MANIP_MOVE) ? szMoveStatusBarMsg : szCopyStatusBarMsg,
				(char far *)pszSrc, (char far *)pszDest) ;              
			break;

		default:
			CancelDirectManipulation(Message) ;
		break;
    } /* switch */

    MessageBar(Message, isaMenu,TRUE);

} /* MonitorManipulation */

