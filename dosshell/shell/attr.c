;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****   attr.c - functions relating to changing file attributes.
**
**   Date      Author   Modification
** --------   --------  ------------------------------------------------
**  11/08/89  harikris  Created file
*/
#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <text.h>
#include <help.h>
#include <attr.hs>
#include <attr.sdm>

extern BYTE ErrorCrit ;

extern void ResetTreeMatchedFilesOpt(PTREE tree) ;
extern void MarkMatchesBit(PENTRY fil) ;

/* re-use of variable used by FDlgErr in errbox.c */
extern char *QStatus ;

#define MAXATTRIBUTES 4 /* Warning! 4 is the number of strings in
			   pszAttrStrings[] of text.c -- they need to tally */
BYTE gAttrChar[MAXATTRIBUTES] ;

int gAttr_Change_Mode ;

/* On Exit returns value got from GetResponse() */
int ChangeAttributes(PTREE tree, PENTRY node, char *placeholder, 
												int count, int total, BOOL verify)
{
	int action;
	char path[MAX_PATH+1] ;
	TMC tmc ;
	HCABattr h ;
	unsigned attributes ;
	int code ;
	char statusline[STATUS_LEN];
	BOOL fPrevHiddenOrSystem ;
	BOOL fNowHiddenOrSystem ;
	int dummylen ;

	UnReferenced(placeholder) ;
	UnReferenced(verify) ;

	/* First time through -- so we find the info as to whether to
	 * handle files one at a time or as a whole.
	 */
	if (count == 1)
	{
		/* If more than 1 file selected, we prompt the user whether to modify
		 * singly or all at once.
		 */
		if (total > 1)
		{
			action = GetAttrResponse();
			switch (action)
			{
				case ACT_CANCEL:
						return action ; /* return ACT_CANCEL and do nothing here */

				case ACT_ALL:
						gAttr_Change_Mode = action      /* same as ACT_ALL */ ;
						break ;

				case ACT_SINGLE:
				default:
						gAttr_Change_Mode = ACT_SINGLE ;
						break;
			}
		}
		else
		{
			/* Only 1 file has been selected totally */
			gAttr_Change_Mode = ACT_SINGLE ;
		}
	} /* count == 1  and more than 1 file selected */

	h = HcabAlloc(cabiCABattr);

	if (!h)
	{
		OutOfMemory() ;
		return ACT_NOMEM ;
	}
	InitCab(h, cabiCABattr) ;

	/* Handle the change Attributes section of that node */
	/* If it is ACT_SINGLE, then each time ask for the new settings
	   after displaying the old settings. Otherwise ask for the settings
	   when count == 1 only, later just use the settings */

	Tree2Path(tree, node, path, &dummylen) ;
	FormCountStatusLine(statusline, szStFile, path, count, total,
												STATUS_COUNT_START) ;
	action = ACT_OK ;
	/* In the case of ACT_ALL only when (count == 1) we will do this */
	if ( (count == 1) || (gAttr_Change_Mode == ACT_SINGLE) )
	{
		/* Warning! Assuming order of Attributes in display */
		gAttrChar[0] = (BYTE) ( ( (gAttr_Change_Mode == ACT_SINGLE) && 
								(node->attribs & _A_HIDDEN) ) ? SELCHAR : ' ') ;
		gAttrChar[1] = (BYTE) ( ( (gAttr_Change_Mode == ACT_SINGLE) && 
								(node->attribs & _A_SYSTEM) ) ? SELCHAR : ' ') ;
		gAttrChar[2] = (BYTE) ( ( (gAttr_Change_Mode == ACT_SINGLE) && 
								(node->attribs & _A_ARCH) ) ? SELCHAR : ' ') ;
		gAttrChar[3] = (BYTE) ( ( (gAttr_Change_Mode == ACT_SINGLE) && 
								(node->attribs & _A_RDONLY) ) ? SELCHAR : ' ') ;
		QStatus = (gAttr_Change_Mode != ACT_SINGLE) ? NullString : statusline ;

		SzToCab(h, szEnterButton, Iag(CABattr, pszattrEB));
		SzToCab(h, szCancelButton, Iag(CABattr, pszattrCB));
		SzToCab(h, szHelpButton, Iag(CABattr, pszattrHB));

		tmc = MyTmcDoDlg(&dlgattr,  h);
		if (tmc == tmcCancel)
		{
			FreeCab(h) ;
			return ACT_CANCEL;
		}
	}
	/* Now we have the attribute settimgs in gAttrChar,use it to set the
	   files attributes on the disk */

	attributes = node->attribs ;

	/* zero out the hidden, system, archive, and read-only bits */
	attributes &= (~ (_A_HIDDEN | _A_SYSTEM | _A_ARCH | _A_RDONLY) ) ; 

	/* Note: index 0=>hidden, 1=>read-only, 2=>archive */
	/* Set the three attributes mentioned above based on gAttrChar */
	/* Following code avoids jumps, but is almost un-readable */
	attributes |=  ( (-(gAttrChar[0] == SELCHAR)) & _A_HIDDEN ) ;
	attributes |=  ( (-(gAttrChar[1] == SELCHAR)) & _A_SYSTEM ) ;
	attributes |=  ( (-(gAttrChar[2] == SELCHAR)) & _A_ARCH ) ;
	attributes |=  ( (-(gAttrChar[3] == SELCHAR)) & _A_RDONLY ) ;

	PutUpStatusMessage(statusline, count) ;

	if (node->attribs == (BYTE) attributes) 
		/* ZZZZ should I still try to write it, as I might be using old
			info and things might have changed! */
		/* Specified attributes are the same as before. It is
			terrible to unnecessarily hit the disk. This is where the floopy
			line status monitor will come in handy */
		; /* Do Nothing! */
	else
	{
		do
		{
			ErrorCrit = 0xFF ;
			code = _dos_setfileattr(path, attributes) ;
			if (code)
			{
				/* Did a critical error occur? -- say, disk write protected! */
				if (ErrorCrit != 0xFF)
					action = GetResponse(statusline, 
										szCriticalMessages[ErrorCrit], 
										BT_FILERETRYSKIPQUIT, HELP_ERRSETATTR);
				else
					action = DOSErrorBox(statusline, code, HELP_ERRSETFILEATTR) ;
			}
			else
			{
				/* If we changed the attributes of the file so that it's
				 * hidden/system attributes have changed, we need to figure
				 * out if it is visible now!
				 */
				fPrevHiddenOrSystem = node->attribs & (_A_HIDDEN | _A_SYSTEM) ;
				fNowHiddenOrSystem = attributes & (_A_HIDDEN | _A_SYSTEM) ;

				node->attribs = (BYTE) attributes ;
				action = ACT_OK ;

				if (fNowHiddenOrSystem ^ fPrevHiddenOrSystem)
				{
					MarkMatchesBit(node) ;
					ResetTreeMatchedFilesOpt(tree) ;
				}

			}
		} while (action == ACT_RETRY) ;
	}

	TakeDownStatusMessage(count, total) ;

	FreeCab(h);
	return action;
} /* proc ChangeAttributes */


ListBoxData AttrList;

void  MakeAttrLine(RX x, RY y, WORD isz, WORD bArg)
{
	unsigned char messagestr[40]; /* 40 > max lenth of any message */
	int i;
	int len;
	RX rxGraphics ;
	RY ryGraphics ;

	/* the '-2' handles the scroll bar width even though it is a stupid lbox */
    len = Get_List_Rect(&AttrList).axRight-Get_List_Rect(&AttrList).axLeft-2;

	*messagestr = ' ' ;
    for(i=1;pszAttrStrings[isz][i-1];i++)
    {
		messagestr[i] = pszAttrStrings[isz][i-1];
    }
	/* 1 character width has been taken up by the SELCHAR at the start of line*/
    for(;i<len-1;i++)
    {
		messagestr[i] = ' ';
    }
	CharOut(AttrList.pwd, x, y,gAttrChar[isz], isaDialogBox) ;


	TextOut(AttrList.pwd, x+1, y,messagestr, len-1,
				(bArg & TF_ISFOCUS) ? isaHilite : isaDialogBox);

	/* Graphics is done relative to top of screen -- i.e. in absolute
	 * co-ordinates and not wrt to a window!!
	 */
	rxGraphics = AttrList.pwd->arcWindow.axLeft + x + 1 ;
	ryGraphics = AttrList.pwd->arcWindow.ayTop + y  ;

#if 0
	/* In case of a mouse click, we want to temporarily change focus from
	 *      say one of the dialog buttons to the listbox and then go back!
	 */
	if (bArg & TF_ISFOCUS)
		SetFocus(AttrList.pwd) ;
#endif
	/* The FALSE in the param list forces fn DrawFocusMarker to always draw
	 * the focus box in graphics mode!!
	 */
	DrawFocusMarker(AttrList.pwd, x-2, y, rxGraphics, ryGraphics,
								len-1, bArg & TF_ISFOCUS, FALSE, isaDialogBox) ;

} /* MakeAttrLine */


WORD PASCAL ListProcAttrList(WORD tmm, char *sz, WORD isz, TMC tmc, 
																WORD x, WORD y, WORD bArg)
{
	RX xval;

	UnReferenced(tmc) ;
	UnReferenced(x) ;

	xval = Get_List_Rect(&AttrList).axLeft;
	switch (tmm) {
		case tmmCount:
			return(MAXATTRIBUTES) ;
		case tmmGetItemString:
			if (isz < MAXATTRIBUTES)
				strcpy(sz, pszAttrStrings[isz]);
			else
				*sz = '\0' ;
			break ;

		case tmmSelect:
		case tmmToggleSelect:
			/* Never draw on a select, only mark selections! */
			if (isz < MAXATTRIBUTES)
				gAttrChar[isz] = (BYTE)((gAttrChar[isz] == ' ') ? SELCHAR : ' ');

			break ;

		case tmmDrawItem:
		    if(isz < MAXATTRIBUTES)
			  MakeAttrLine((RX) xval, (RY) y+1, isz, bArg) ;
			break;
		case tmmDeselectAll:
		case tmmSetFocus:
		default:
			break;
	}
	return TRUE;
}

LONG FAR PASCAL pfnAttrList(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
	UnReferenced(pwnd) ;

    switch(message)
    {
	case WM_CHAR:
		if ( (wParam == VK_ESCAPE) || (wParam == ESCAPE) ||
			  (wParam == '\t') || (wParam == '\r') )
			return(FALSE);

		ListKey(&AttrList,wParam, HIWORD(LParam));
		break;

	case WM_KILLFOCUS:
		GlobalFocusBox(&AttrList,FALSE);
		break;

	case WM_SETFOCUS:
		GlobalFocusBox(&AttrList,TRUE);
		break;

	case WM_PAINT:
		UpdateListBox(&AttrList);
		break;
	} /* switch */
    return(TRUE);
}


BOOL FAR PASCAL FDlgattr(WORD dlm, TMC tmc, WORD wNew, WORD wOld, WORD wParam)
{
	PWND  dwind, lwind;
	WORD  mx,my;

	UnReferenced(tmc) ;

	dwind = PwndParent(PwndOfListbox(tmcOK));
	switch(dlm)
	{
	    case dlmInit:
			SetUpDialog(tmcOK,szChangeFileCaption);
			SetUpButtonForGraphics(tmcOK);
			SetUpButtonForGraphics(tmcCancel);
			SetUpButtonForGraphics(tmcattrhelp);
	       /* The next few lines initialize a listbox. The list
			* box will hang just below, and have the same x coordinates
			* as tmcattrlist
			*/
		   lwind = PwndOfListbox(tmcattrlist);

		   /* We don't want to draw focus cursor in the attr listbox window */
		   EnableCursor(lwind, FALSE) ;

		   SetWindowProc(lwind,pfnAttrList);

	       ListBoxInit(&AttrList,ListProcAttrList,dwind,
			lwind->arcWindow.ayTop -dwind->arcWindow.ayTop,
			lwind->arcWindow.axLeft-dwind->arcWindow.axLeft,
			lwind->arcWindow.ayTop-dwind->arcWindow.ayTop+MAXATTRIBUTES+1,
			lwind->arcWindow.axRight-dwind->arcWindow.axLeft+1,
			NullString, tmcattrlist,0,0);
		   MakeListStupid(&AttrList);

		   /* Initially the focus is not in listbox - It is on the OK button */
		   GlobalFocusBox(&AttrList,FALSE);
			gCurrentTMC = tmcOK;

			Shell_SetTmcText(tmcattrstatus, QStatus);
			Shell_SetTmcText(tmcattrmsg1, szAttrLine1) ;
			Shell_SetTmcText(tmcattrmsg2, szAttrLine2) ;
		break;

		case dlmIdle:
			ListBoxIdle(&AttrList);
		break;

		case dlmClientMouse:
			dwind = PwndParent(PwndOfListbox(tmcOK));
			my = HIBYTE(wParam)-1;
			mx = LOBYTE(wParam);
			if(ListMouse(&AttrList,mx,my,wNew,wOld))
				SetFocusTmc(tmcattrlist);
			break;


	case dlmClick:
		 if(tmc == tmcattrhelp)
			Help(hemDialog, hidATTR, NULL, 0);

		 SetFocusTmc(gCurrentTMC) ;
		 break ;
	} /* switch */
	return(TRUE);
} /* FDlgAttr */
