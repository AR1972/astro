;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****   errbox.c - file mgr generic error & message boxes
**      
**   Date      Author   Modification
** --------   --------  ------------------------------------------------
**  08/07/89  t-jeffro  created file
**  11/01/89  scottq    rewrote most of this to use listbox code
**  11/08/89  harikris  made changes to GetResponse, DosErrrBox -- param
**                      list different and handles ChangeAttribute.
**  12/06/89  harikris  wrote functions GetAttrResponse, CriticalDialog.
**  12/06/89  harikris  Fixed uses of the listbox code (Inits, len, etc).
**      03/29/90  harikris  wrote function FDelItemDialog
*/
#include <common.h>
#include <menus.h>
#include <filemgr.h>
#include <prot.h>
#include <text.h>
#include <err.hs>                                  /* files for dialogue box               */
#include <err.sdm>                                 /* ...*/
#include <assert.h>

extern VOID FInitMouseNest(void) ;

/* WARNING! 'attr0's and 'critnew's, delitem's tmcs should be the same as 'err's
	    tmcs as they both use the same dialog handler 'FDlgerr' */

#define tmcattr0help        tmcerrhelp
#define tmcattr0status      tmcerrstatus
#define tmcattr0msg             tmcquerymsg
#define tmcattr0list        tmcerrlist

#include <attr0.hs>
#include <attr0.sdm>

#define tmccritnewhelp     tmcerrhelp
#define tmccritnewstatus        tmcerrstatus
#define tmccritnewmsg      tmcquerymsg
#define tmccritnewlist     tmcerrlist

#include <critnew.hs>
#include <critnew.sdm>

#define tmcdelitemhelp      tmcerrhelp
#define tmcdelitemstatus        tmcerrstatus
#define tmcdelitemmsg       tmcquerymsg
#define tmcdelitemlist      tmcerrlist

#include <delitem.hs>
#include <delitem.sdm>


/*              Note that all the following "globals" are static because no other
**              modules should need to examine or change them.
*/

/* We currently have a maximum of 3 lines in the choices we give user!
	example: BT_RETRYQUIT below! */
static struct {
	char *msg;            // message for that line
	int retval;           // we return this if selected
} lines[3];                   // one rec for each msg line

static int maxline;           // last valid index in lines[]

static int curline;           // selected line

/* The following three are global and are reused in fn ChangeAttributes */
char *QStatus;                // Status line in GetResponse box.

char *QMessage;              // the error msg to respond to

ListBoxData ErrList;


/*
*  This routine handles the case of a listbox (without scroll bars - 'stupid')
*  within a dialog box. This is typically called when an error has occured, etc
*  and the user is required to choose one of several options.
*  The user's selection is communicated to the caller using the global variable
*  'curline' -- Example: If there are 3 choices, curline will be set to 0, 1, or
*       2 (i.e., the base is 0). The global variable 'ErrList' has the listbox
*  coordinates. The caller takes responsibility to initialize this before
*  control passes to this routine.
*/
WORD PASCAL ListProcErrList(WORD tmm, char *sz, WORD isz, TMC tmc,
											WORD x, WORD y, WORD bArg)
{
	RX xval;
	unsigned char messagestr[60]; /* 60 >= max length of any message */
	int i;
	ISA isa ;
	int len;

	UnReferenced(tmc) ;
	UnReferenced(x) ;

	xval = Get_List_Rect(&ErrList).axLeft;
	switch (tmm) {
		case tmmCount:
			return(maxline+1);

		case tmmGetItemString:
			/* Note that at this time we don't have > 10 items */
			assert(isz < 10) ;

			/* Return character '1' for item 0, '2' for item 1, etc  */
			/* This enables moving thru choices without having to use*/
			/* the up/down arrow keys. One can use the appr. digit.  */
			*sz = (char) isz + '1' ;
			*(sz+1) = '\0' ;
			break ;

		case tmmSetFocus:
		case tmmSelect:
			/* ignore these messages! */
		   break;

		case tmmActivate:
		     if ( (maxline <  0) || (isz > maxline) )
					return 0 ; /* garbage value!! -- Bad select message */
		     curline = isz;
		     EndDlgTmc(tmcOK);
		     break;

		case tmmDrawItem:
			/* The '-2' in the following line takes care of the scrollbar
				width even though it is a 'stupid' listbox */
		len = Get_List_Rect(&ErrList).axRight - xval - 2;
		    if ( (maxline >= 0) && (isz <= maxline) )
		    {
		      messagestr[0] = (BYTE) ' ' ;
		      messagestr[1] = (BYTE) '1'+isz;
		      for(i=2;lines[isz].msg[i-2];i++)
		      {
			   messagestr[i] = lines[isz].msg[i-2];
		      }
		      for(;i<len;i++)
		      {
			  messagestr[i] = ' ';
		      }
			  if (bArg & TF_ISFOCUS)
			  {
				 curline = isz ; /* this is the line with focus!! -- So if the
									user types enter this what he desires */
				 isa = (ISA) bArg ;
			  }
			  else
				isa = isaDialogBox ;
		      TextOut(ErrList.pwd,(RX) xval,(RY) y+1,messagestr,len,isa);
		    }
			break;

		default:
			break;
	}
	return TRUE;
} /* ListProcErrList */

char *gpszFileOpCaption ; /* File Operation caption in the dialog box */

LONG FAR PASCAL pfnErrList(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
    UnReferenced(pwnd);
    UnReferenced(wParam);
    UnReferenced(LParam);

    switch(message) {
    case WM_PAINT:
	UpdateListBox(&ErrList);
	break;

    default:
	return(FALSE);
    } /* switch */

    return(TRUE);
}

/* Warning! that this dialog is not re-entrant as it uses global variables like
	gpszFileOpCaption, curline, ErrList, etc. So Dialog boxes that use this
	function should be mutually exclusive (no two of them should be active
	at the same time). Currently critnew, attr0, err, use this function! */
BOOL FAR PASCAL FDlgerr(WORD dlm, TMC tmc, WORD wNew, WORD wOld, WORD wParam)
{
	PWND  dwind;
	PWND  lwind;

	UnReferenced(tmc) ;

	dwind = PwndParent(PwndOfListbox(tmcOK));
	switch(dlm)
	{
	    case dlmInit:
		   SetUpDialog(tmcOK,gpszFileOpCaption) ;

		   SetUpButtonForGraphics(tmcOK) ;
		   SetUpButtonForGraphics(tmcCancel) ;
		   SetUpButtonForGraphics(tmcerrhelp) ;

	       /* The next few lines initialize a listbox. The list
		  box will hang just below, and have the same x coordinates
		  as tmcerrlist
	       */
	       lwind = PwndOfListbox(tmcerrlist);
		   SetWindowProc(lwind, pfnErrList);
	       ListBoxInit(&ErrList,ListProcErrList,dwind,
							lwind->arcWindow.ayTop-dwind->arcWindow.ayTop,
							lwind->arcWindow.axLeft-dwind->arcWindow.axLeft,
							lwind->arcWindow.ayTop-dwind->arcWindow.ayTop+4,
							lwind->arcWindow.axRight-dwind->arcWindow.axLeft+1,
							NullString,0,0,0);
	       MakeListStupid(&ErrList);

	       Shell_SetTmcText(tmcerrstatus, QStatus);
	       Shell_SetTmcText(tmcquerymsg, QMessage);

	    if(((PDLG)&dlgerr)->hid == hidERR ||
		    ((PDLG)&dlgerr)->hid == hidCRIT)
			    gCurrentTMC = tmcerrhelp;
		   break;

	    case dlmIdle:
			ListBoxIdle(&ErrList);
			break;

	    case dlmClientMouse:
			ListMouse(&ErrList, LOBYTE(wParam), HIBYTE(wParam)-1, wNew, wOld);
			break;

	    case dlmKey:
			/* ZZZZZZ shift state right?? */
			ListKey(&ErrList,wNew, 0);
			break;

		case dlmSetFocus:
			gCurrentTMC = tmc;
			break ;

		case dlmClick:
			if(tmc == tmcerrhelp)
				Help(hemDialog, ((PDLG)&dlgerr)->hid, NULL, 0);

			 SetFocusTmc(gCurrentTMC) ;
			 break ;

	}
	return(TRUE);
}

/* Set up the various options and ask the user to select one of the many
*  options displayed to him. This routine gets the response of the user */
int GetResponse(char *statusline, char *msg, int boxtype, unsigned helpid)
{
	HCABerr h = HcabAlloc(cabiCABerr);
	TMC tmc;

	if (!h)
	{
		OutOfMemory() ;
		return ACT_NOMEM ;
	}

	gErrHelpId = (char) helpid ;
	((PDLG)&dlgerr)->hid = hidERR;

	InitCab(h, cabiCABerr) ;

	SzToCab(h, szEnterButton, Iag(CABerr, pszerrEB));
	SzToCab(h, szCancelButton, Iag(CABerr, pszerrCB));
	SzToCab(h, szHelpButton, Iag(CABerr, pszerrHB));

	/* Init "list box" lines -- i.e., the various choices. */
	switch (boxtype)
	{
		case BT_SKIPQUIT:
			lines[0].msg = szSkipLine;
			lines[0].retval = ACT_SKIP;
			lines[1].msg = szCancelLine;
			lines[1].retval = ACT_CANCEL;
			maxline = 1;
			break;
		case BT_RETRYSKIPQUIT:
			lines[0].msg = szRetryLine;
			lines[0].retval = ACT_RETRY;
			lines[1].msg = szSkipLine;
			lines[1].retval = ACT_SKIP;
			lines[2].msg = szCancelLine;
			lines[2].retval = ACT_CANCEL;
			maxline = 2;
			break;
		case BT_SKIPRETRY:
			lines[0].msg = szSkipLine;
			lines[0].retval = ACT_SKIP;
			lines[1].msg = szRetryLine;
			lines[1].retval = ACT_RETRY;
			maxline = 1;
			break;
		case BT_FILESKIPQUIT:
			lines[0].msg = szSkipFileLine;
			lines[0].retval = ACT_SKIP;
			lines[1].msg = szCancelLine;
			lines[1].retval = ACT_CANCEL;
			maxline = 1;
			break;
		case BT_FILERETRYSKIPQUIT:
			lines[0].msg = szRetryFileLine;
			lines[0].retval = ACT_RETRY;
			lines[1].msg = szSkipFileLine;
			lines[1].retval = ACT_SKIP;
			lines[2].msg = szCancelLine;
			lines[2].retval = ACT_CANCEL;
			maxline = 2;
			break;
		case BT_QUIT:
		default:
			maxline = -1;
			break;
	} /* switch boxtype */

	QStatus = statusline ;
	QMessage = msg;

	curline = 0;

	/* Re-enable the mouse as it could have been disabled when we put up
	 * the status message.
	 */
	FInitMouseNest() ;

	tmc = MyTmcDoDlg(&dlgerr,  h); /* causes execution of FDlgErr */

	FreeCab(h);

	if ( (tmc == tmcOK) && (curline >= 0) && (curline <= maxline) )
		return lines[curline].retval;
	else
		return ACT_CANCEL;

} /* GetResponse */


/****   DOSErrorBox     - pops up dlg in response to given DOS error.
**
**      ENTRY
**                      statusline  -  The status message like Copying File...
**                      err   - DOS error code
**                      helpid- help id to pass to GetResponse()
**      EXIT
**                      action code, see GetResponse() for enumeration
**      EFFECTS
*/

int DOSErrorBox(char *statusline, WORD err, unsigned helpid)
{
	int boxtype;                                            // which form of listbox to use
	char *msg;                                                      // error message to display

	msg = DOSErrorMsg(err, &boxtype);
	return GetResponse(statusline, msg, boxtype, helpid);
} /* proc DOSErrorBox */

char *DOSErrorMsg(int errcode, int *box)
{
	int boxtype;
	char *msg;

	switch (errcode)
	{
		case 1: case 4: case 6: case 7: case 9:
		case 10:	case 11: case 16:
		case 18: case 35: case 36: case 38:
					msg = szConfused;	boxtype = BT_QUIT;	break;

		case 2: msg = szFileNotFound;   boxtype = BT_SKIPQUIT;	break;
		case 3: msg = szPathNotFound;   boxtype = BT_SKIPRETRY;	break;

		case 5: case 13: case 17:
			msg = szAccessDenied;	boxtype = BT_SKIPRETRY;	break;

		case 8: case 12:
			msg = szOutOfMemory;    boxtype = BT_QUIT;	break;

		case 14: msg = szUnknown;	boxtype = BT_QUIT;	break;
		case 15: msg = szInvalidDrive;   boxtype = BT_SKIPQUIT;          break;
		case 19: msg = szWriteProtect;   boxtype = BT_RETRYSKIPQUIT; break;

		case 20: case 22: case 23: case 24: case 25: case 26: case 27: case 31:
					msg = szBadDisk; boxtype = BT_RETRYSKIPQUIT; break;

		case 21: msg = szDriveNotReady;  boxtype = BT_RETRYSKIPQUIT; break;

		case 28: case 29:
			msg = szWriteError; boxtype = BT_RETRYSKIPQUIT; break;

		case 30:msg = szReadError; boxtype = BT_RETRYSKIPQUIT; break;

		case 32: case 33:
					msg = szFileLocked; boxtype = BT_RETRYSKIPQUIT; break;

		case 34:msg = szWrongDisk; boxtype = BT_RETRYSKIPQUIT; break;

		case 37:
		default: msg = szUnknown;                boxtype = BT_RETRYSKIPQUIT;     break;
	}
	if (box)
		*box = boxtype;
	return msg;
} /* proc DOSErrorMsg */

int GetAttrResponse(void)
{
	HCABattr0 h = HcabAlloc(cabiCABattr0);
	TMC tmc;

	if (!h)
	{
		OutOfMemory() ;
		return ACT_NOMEM ;
	}

	((PDLG)&dlgerr)->hid = hidATTR0;
	InitCab(h, cabiCABattr0) ;

	SzToCab(h, szEnterButton, Iag(CABattr0, pszattr0EB));
	SzToCab(h, szCancelButton, Iag(CABattr0, pszattr0CB));
	SzToCab(h, szHelpButton, Iag(CABattr0, pszattr0HB));

	lines[0].msg = szModifySingly;
	lines[0].retval = ACT_SINGLE;
	lines[1].msg = szModifyTogether;
	lines[1].retval = ACT_ALL;
	maxline = 1;

	QStatus = NullString ;
	QMessage = NullString ;

	curline = 0;

	tmc = MyTmcDoDlg(&dlgattr0,  h); /* Actually executes FDlgErr */

	FreeCab(h);

	if (tmc == tmcOK)
		return lines[curline].retval;
	else
		return ACT_CANCEL;

} /* GetAttrResponse */

extern BYTE ErrorCrit ;

BOOL CriticalDialog()
{
	HCABcritnew     h;
	TMC tmc ;
    char *fcaption ;

	/* Save the FileOp Caption -- so that we can restore it once this critical
		error dialog is done. This global (gpsz...) is used by FDlgerr */
	fcaption = gpszFileOpCaption ;
	gpszFileOpCaption = szCritWarning ;

	h = HcabAlloc(cabiCABcritnew);
	if (!h)
	{
	    OutOfMemory() ;
	    return 1 ; /* garbage value -- return don't read disk again!! */
	}
	DialogIsAlert(TRUE) ;
	((PDLG)&dlgerr)->hid = hidCRIT;

	InitCab(h, cabiCABcritnew) ;

	SzToCab(h, szEnterButton, Iag(CABcritnew, pszcritnewEB));
	SzToCab(h, szCancelButton, Iag(CABcritnew, pszcritnewCB));
	SzToCab(h, szHelpButton, Iag(CABcritnew, pszcritnewHB));

	lines[0].msg = szCritDoRead ;
	lines[0].retval = 0;
	lines[1].msg = szCritDontRead ;
	lines[1].retval = 1;
	maxline = 1;

	QStatus = szCriticalMessages[ErrorCrit] ;
	QMessage = NullString ;

	curline = 0;

	tmc = MyTmcDoDlg(&dlgcritnew,h) ; /* Actually executes FDlgErr */

	FreeCab(h);

	/* restore fileop caption */
	gpszFileOpCaption = fcaption ;

	DialogIsAlert(FALSE) ;

	if (tmc == tmcOK)
		return lines[curline].retval;
	else
		return 1; /* Don't retry */
} /* CriticalDialog */

/* Puts up a dialog box & confirms whether to delete the item or not.
 * Returns a BOOL of TRUE iff the user says OK to delete.
 */
BOOL FDelItemDialog(char *theMessage)
{
	HCABdelitem     h;
	TMC tmc ;

	gpszFileOpCaption = szDeleteItemCaption ;

	h = HcabAlloc(cabiCABdelitem);
	if (!h)
	{
	    OutOfMemory() ;
	    return FALSE ;      /* do not delete item -- don't know what user wants! */
	}
	InitCab(h, cabiCABdelitem) ;
	((PDLG)&dlgerr)->hid = hidDELITEM;

	SzToCab(h, szEnterButton, Iag(CABdelitem, pszdelitemEB));
	SzToCab(h, szCancelButton, Iag(CABdelitem, pszdelitemCB));
	SzToCab(h, szHelpButton, Iag(CABdelitem, pszdelitemHB));

	lines[0].msg = szDeleteItem ;
	lines[0].retval = TRUE ;
	lines[1].msg = szDontDeleteItem ;
	lines[1].retval = FALSE ;
	maxline = 1;

	QStatus = NullString ;
	QMessage = theMessage ;

	curline = 0;

	tmc = MyTmcDoDlg(&dlgdelitem, h) ; /* Actually executes FDlgErr */

	FreeCab(h);

	if (tmc == tmcOK)
		return lines[curline].retval;
	else
		return FALSE ;
} /* FDelItemDialog */
