;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <filemgr.h>
#include <text.h>
#include <menus.h>
#include <prot.h>

BYTE ErrorCrit;
extern BOOL gfFMBeingInited ;

#if 0
/ZZZZZ removed!! *****************************/
#include <crit.hs>
#include <crit.sdm>
 /*
 *
 */
extern char *szCriticalAction[];

ListBoxData CritList;
BYTE critline;

WORD PASCAL ListProcCritList(tmm, sz, isz, tmc, x, y, bArg)
WORD tmm;
char *sz;
WORD isz;
TMC tmc;
WORD x, y, bArg;
{
	RX xval;
	char messagestr[50];
	int i;
	int len;

	xval = Get_List_Rect(&CritList).axLeft +1;
	switch (tmm) {
		case tmmCount:
			return(2);
		case tmmSetFocus:
			/* ignore this message! */
		     break;
		case tmmSelect:
		     critline = isz;
		     EndDlgTmc(tmccritenter);
		     break;
		case tmmDrawItem:
		{
		    if(isz < 2)
		    {
		      len = Get_List_Rect(&CritList).axRight -
			    Get_List_Rect(&CritList).axLeft - 4;
		      messagestr[0] = '1'+isz;
		      for(i=1;szCriticalAction[isz][i-1];i++)
		      {
			   messagestr[i] = szCriticalAction[isz][i-1];
		      }
		      for(;i<len;i++)
		      {
			  messagestr[i] = ' ';
		      }
		      TextOut(CritList.pwd, xval+1, y+1,messagestr, len, bArg);
		    }
		}
			break;
		default:
			break;
	}
	return TRUE;
}


BOOL FAR PASCAL
FDlgcrit(dlm, tmc, wNew, wOld, wParam)
WORD    dlm;
TMC     tmc;
WORD    wNew, wOld, wParam;
{
	PWND  dwind;
	PWND  lwind;
	WORD  mx,my;
	int   i;
	char str[15];

	dwind = PwndParent(PwndOfListbox(tmccritcancel));
	switch(dlm)
	{
	    case dlmInit:
	    {
	       SetUpDialog(tmccritenter,szCritWarning);
	       //if(gisgraph)
	       //{
		SetupButtonForGraphics(tmccritenter);
		SetupButtonForGraphics(tmccritcancel);
		SetupButtonForGraphics(tmccrithelp);
	       //}
	       /* The next few lines initialize a listbox. The list
		  box will hang just below, and have the same x coordinates
		  as tmccritlist
	       */
	       lwind = PwndOfListbox(tmccritlist);
	       ListBoxInit(&CritList,ListProcCritList,dwind,
		    lwind->arcWindow.ayTop -dwind->arcWindow.ayTop,
		    lwind->arcWindow.axLeft-dwind->arcWindow.axLeft,
		    lwind->arcWindow.ayTop-dwind->arcWindow.ayTop+5,
		    lwind->arcWindow.axRight-dwind->arcWindow.axLeft,
		    "",0,0,0);
		MakeListStupid(&CritList);
		Shell_SetTmcText(tmccritmessage,szCriticalMessages[ErrorCrit]);
	    }
	    break;
	    case dlmIdle:
	    {
		ListBoxIdle(&CritList);
	    }
	    break;
	    case dlmClientMouse:
	    {
		dwind = PwndParent(PwndOfListbox(tmccritcancel));
		my = HIBYTE(wParam)-1;
		mx = LOBYTE(wParam);
		ListMouse(&CritList,mx,my,wNew,wOld);
	    }
	    break;
	    case dlmKey:
	    {
		ListKey(&CritList,wNew, 0);
	    }
	    break;
	}
	return(TRUE);

}

/*
 * RETURNS TRUE if call should be aborted,
 * FALSE if call should be retried
 */
BOOL CriticalDialog()
{
	BOOL retval;
	HCABcrit        h;

	h = HcabAlloc(cabiCABcrit);
	if (!h)
	{
	    OutOfMemory() ;
	    return ;
	}
	InitCab(h, cabiCABcrit) ;
	critline = 0;
	if(MyTmcDoDlg(&dlgcrit,h) == tmccritenter)
	{
	    retval = (critline != 0);
	}
	else
		retval = TRUE ;
	FreeCab(h);
	return(retval);
}
/ZZZ above code removed!! *********************************************/
#endif

unsigned shell_findfirst(path,attributes,buffer)
char *path;
unsigned attributes;
struct find_t *buffer;
{
    unsigned ret;
    while(1)
    {
       ErrorCrit = 0xFF;
       ret = _dos_findfirst(path,attributes,buffer);
       if ( (ErrorCrit != 0xFF) && (!gfFMBeingInited) )
       {
		if (CriticalDialog())
				break;
       }
       else
			break;
    }
    return(ret);
}
