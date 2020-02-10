;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <ctype.h>
#include "common.h"
#include "menus.h"
#include "filemgr.h"
#include "prot.h"
#include "text.h"
#include "showinfo.hs"
#include "showinfo.sdm"

DWORD FAR PASCAL PfnShowInfoInfo(PWND pwnd, WORD message, WORD wParam,
		DWORD lParam)
{
    PENTRY node ;

    UnReferenced(lParam) ;
    UnReferenced(wParam) ;

    switch(message) {
    case WM_PAINT:
/* Compiler should hopefully optimize CSEs below */
        node = (glob.TreeMode==TR_SEARCH && gfSearchDisk) ?
                GetNthFlatMatchedFile( listinfo[glob.FocusBox].tree,
                    Get_List_Focus(&FileList[glob.FocusBox]) ) :
                GetNthMatchedFile( listinfo[glob.FocusBox].files,
                    Get_List_Focus(&FileList[glob.FocusBox]), 
                    listinfo[glob.FocusBox].tree) ;
        DispFlatLeft(node, listinfo[glob.FocusBox].tree, pwnd,0,0,isaDialogBox);
        break;

    default:
        return(FALSE);
    }

    return(TRUE);
}

VOID FAR ShowInfo(VOID)
{
	HCABshowinfo	  h;
	PTREE tree ;
	BOOL fFakingDiskInfoKnown = FALSE ;

	h = HcabAlloc(cabiCABshowinfo);

	if (!h)
	{
		OutOfMemory() ;
		return ;
	}

	tree = listinfo[glob.FocusBox].tree ;

	if (!tree->fdiskinfoknown)
	{
		if (!GetDiskInfo(tree, tree->VolLabel, &tree->SizeTotal,
				       										&tree->SizeAvail))
		{
			fFakingDiskInfoKnown = TRUE ;
		}

		/* Set the following field to TRUE anyway, otherwise when the
		 * dialog procedure causes an invocation of DispFlatLeft() thru
		 * fn pfnShowInfoInfo, the disk will be touched again when it
		 * does not have to! We will set the field back to FALSE in
		 * case GetDiskInfo() failed, i.e., variable fFakingDiskInfoKnown
		 * being true ;
		 */
		tree->fdiskinfoknown = TRUE ;
	}
	
	InitCab(h, cabiCABshowinfo) ;

	SzToCab(h, szClose, Iag(CABshowinfo, pszshowinfoCB));
	SzToCab(h, szHelpButton, Iag(CABshowinfo, pszshowinfoHB));

	MyTmcDoDlg(&dlgshowinfo,  h);

	if (fFakingDiskInfoKnown)
		tree->fdiskinfoknown = FALSE ;

	FreeCab(h);

}

BOOL FAR PASCAL FDlgshowinfo(WORD dlm, TMC tmc, WORD wNew,
		WORD wOld, WORD wParam)
{
	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	switch (dlm) {
	case dlmInit:
		SetUpDialog(tmcCancel, szShowInformationCaption);

		SetWindowProc(PwndOfTmc(tmcshowinfoinfo), PfnShowInfoInfo);
		SetUpButtonForGraphics(tmcCancel);
		SetUpButtonForGraphics(tmcshowinfohelp);
		break;

	case dlmClick:
		if (tmc == tmcCancel)
			break ;

		if(tmc == tmcshowinfohelp)
			Help(hemDialog, hidSHOWINFO, NULL, 0);

		SetFocusTmc(tmcCancel) ;
		break ;

	}
	return(TRUE);
} /* FDlgshowinfo */

