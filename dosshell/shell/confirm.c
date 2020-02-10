;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <text.h>
#include <assert.h>

#include <mconfirm.hs>
#include <mconfirm.sdm>

#include <dconfirm.hs>
#include <dconfirm.sdm>

#include <rconfirm.hs>
#include <rconfirm.sdm>

extern VOID MakeDirNameFit(char *longname,char *pattern,char *destination,int maxlen) ;
extern char * PASCAL MySetMessageText(TMC tmc, char *message, int nWid) ;
extern void cdecl FormStringWithoutPlaceHolders(char far *dest, char far *src, ...) ;

extern BYTE gManipType;

/* Used by fn DeleteConfirmationDialog() */
char *gpszDeleteConfirmCaption ;        // Delete Directory/File Confirmation
char *gpszDeleteConfirmMsg ;            // message text

/* Used by fn ReplaceConfirmationDialog() */
char *gReplaceSrcPath ;
char *gReplaceDestPath ;
char *gpszReplaceSrcInfo ;
char *gpszReplaceDestInfo ;
BOOL gfDestIsReadOnly ;

/* Used by fn MouseConfirmationDialog() */
char *gpszMouseConfirmMsg ;     // message text
char *gConfirmationPath ;               // dest for copy/move by direct manipulating

/* Dialog procedure for the MouseConfirmation Dialog for Move/Copy/Launch       */
/* gCurrentTMC is being set in this function so that "Help" can be                      */
/* provided when user hits "F1"                                                                                                                 */
BOOL FAR PASCAL FDlgmouseconfirm(WORD dlm, TMC tmc,
											WORD wNew, WORD wOld, WORD wParam)
{
	PWND lwind ;
	int  itemwidth ;
	char CompressedPath[60] ;
	char Message[256] ;
	char *pszTempMsg ;

	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	/* Actually help is the only case to be handled here. Otherwise,
	   we just dismiss the dialog and caller looks at the tmc value on exit */
	if (dlm == dlmInit)
	{
		SetUpDialog(tmcOK, szMouseConfirmCaption);
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcCancel);

		/* we assume that all three 3 tmc text's have same width, so we just
		 * calculate the width of the first one and use it for all three.
		 */
		lwind = PwndOfTmc(tmcmouseconfirmMsg1);
		itemwidth = lwind->arcWindow.axRight - lwind->arcWindow.axLeft ;

		/* If it is not a Launch (Run) operation, we need to put in the
		 * destination path (directory) after compressing it.
		 */
		if (gManipType != MANIP_RUN)
		{
			/* use -2 so that we can put in the '?' char and NULL. INTERNATIONALIZE!!!
			 * here if that language does not have a '?' or has a longer
			 * question mark sequence.
			 */
			MakeDirNameFit(gConfirmationPath, NULL, CompressedPath, itemwidth-2) ;
			FormStringWithoutPlaceHolders(Message, gpszMouseConfirmMsg,
					(char far *)CompressedPath) ;
			gpszMouseConfirmMsg = Message ;
		}

		/* It is OK even if message is not wide enuf for 2/3 lines. The
		 * remaining lines will remain blank.
		 */
		pszTempMsg = MySetMessageText(tmcmouseconfirmMsg1,
														gpszMouseConfirmMsg, itemwidth) ;
		pszTempMsg = MySetMessageText(tmcmouseconfirmMsg2,
														pszTempMsg, itemwidth) ;

		MySetMessageText(tmcmouseconfirmMsg3, pszTempMsg, itemwidth) ;

		gCurrentTMC = tmcOK;
	}
	else
	if (dlm == dlmSetFocus)
	{
	    gCurrentTMC = tmc;
	}

	return TRUE ;
} /* FDlgmouseconfirm */


/* This function puts up the mouse confirmation dialog before performing
 * a move or copy or launch operation.
 *
 * INPUT:
 *              message:        the message to be displayed in the dialog box.
 *              dpath:  the destination path to which copy/move is done.
 *
 * OUTPUT: boolean to indicate whether user chose YES or not!
 */
int MouseConfirmationDialog(char *message, char *dpath)
{
	HCABmouseconfirm hcab ;
	TMC tmc ;

	hcab = HcabAlloc(cabiCABmouseconfirm) ;

	/* ZZZZZ Do I put up an outofmemory dialog now?? */
	if (!hcab)
	{
		OutOfMemory() ;
		return FALSE ;
	}
	InitCab(hcab, cabiCABmouseconfirm) ;

	SzToCab(hcab, szYesButton, Iag(CABmouseconfirm, pszmouseconfirmYB));
	SzToCab(hcab, szNoButton, Iag(CABmouseconfirm, pszmouseconfirmNB));

	gpszMouseConfirmMsg = message ;

	/* Store the destination path pointer in this variable. Will be used
	 * by the fn FDlgMouseConfirm() as we can't pass in params to it.
	 */
	gConfirmationPath = dpath ;

	tmc = MyTmcDoDlg(&dlgmouseconfirm, hcab) ;

	FreeCab(hcab) ;

	return (tmc == tmcOK) ;
} /* MouseConfirmationDialog */

/* Dialog procedure for the DeleteConfirmation Dialog for Delete directory      */
/* or Delete file.                                                                                                                                                      */
/* gCurrentTMC is being set in this function so that "Help" can be                      */
/* provided when user hits "F1"                                                                                                                 */
BOOL FAR PASCAL FDlgdeleteconfirm(WORD dlm, TMC tmc,
											WORD wNew, WORD wOld, WORD wParam)
{
	PWND lwind ;
	int  itemwidth ;
	char CompressedPath[60] ;
	char FormattedText[70] ; // this has the  '?' character or whatever!
	int nlen ;

	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	/* Actually help is the only case to be handled here. Otherwise,
	   we just dismiss the dialog and caller looks at the tmc value on exit */
	if (dlm == dlmInit)
	{
		SetUpDialog(tmcOK, gpszDeleteConfirmCaption);
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcdeleteconfirmNo);
		SetUpButtonForGraphics(tmcCancel);

		Shell_SetTmcText(tmcdeleteconfirmMsg, gpszDeleteConfirmMsg) ;

		lwind = PwndOfTmc(tmcdeleteconfirmPath);
		itemwidth = lwind->arcWindow.axRight - lwind->arcWindow.axLeft ;

		/* get the length of the message less 2 for the %1 in the message */
		nlen = strlen(szDeleteConfirm) - 2 ;

		MakeDirNameFit(gConfirmationPath, NULL, CompressedPath,
																	itemwidth-nlen) ;
		FormStringWithoutPlaceHolders(FormattedText, szDeleteConfirm,
																(char far *)CompressedPath) ; 

		Shell_SetTmcText(tmcdeleteconfirmPath, FormattedText) ;

		gCurrentTMC = tmcOK;
	}
	else
	if (dlm == dlmSetFocus)
	{
	    gCurrentTMC = tmc;
	}

	return TRUE ;
} /* FDlgdeleteconfirm */


/* This function puts up the delete confirmation dialog before performing
 * a delete operation (of file/directory)
 *
 * INPUT:
 *              dpath:          the full path of the file/dir to be deleted
 *              fIsDelFile:     Whether is a Delete File or Delete Directory dialog
 *              Message:                Any message to be put up in the dialog
 *
 * OUTPUT: 
 *              ACT_FORCE, if the user selected the YES button.
 *              ACT_SKIP, if the user selected the NO button.
 *              ACT_CANCEL, if the user selected the CANCEL button.
 */
int DeleteConfirmationDialog(char *dpath, BOOL fIsDelFile, char *Message)
{
	HCABdeleteconfirm hcab ;
	TMC tmc ;
	int ret ;

	hcab = HcabAlloc(cabiCABdeleteconfirm) ;

	/* ZZZZZ DO I put up an outofmemory dialog now?? */
	if (!hcab)
	{
		OutOfMemory() ;
		return FALSE ;
	}
	InitCab(hcab, cabiCABdeleteconfirm) ;

	SzToCab(hcab, szYesButton, Iag(CABdeleteconfirm, pszdeleteconfirmYB));
	SzToCab(hcab, szNoButton, Iag(CABdeleteconfirm, pszdeleteconfirmNB));
	SzToCab(hcab, szCancelButton, Iag(CABdeleteconfirm, pszdeleteconfirmCB));

	/* Store the deleted path pointer in this variable. Will be used
	 * by the fn FDlgdeleteconfirm() as we can't pass in params to it.
	 */

	gConfirmationPath = dpath ;

	gpszDeleteConfirmMsg = Message ;
	gpszDeleteConfirmCaption = fIsDelFile ? szDelFileConfirmCaption :
														 szDelDirConfirmCaption ;

	tmc = MyTmcDoDlg(&dlgdeleteconfirm, hcab) ;

	FreeCab(hcab) ;

	switch (tmc)
	{
		case tmcOK:
			ret = ACT_FORCE ;
			break ;

		case tmcdeleteconfirmNo:
			ret = ACT_SKIP ;
			break ;
		
		case tmcCancel:
		default:
			ret = ACT_CANCEL ;
	} /* switch */

	return ret ;
} /* DeleteConfirmationDialog */


/* Dialog procedure for the ReplaceConfirmation Dialog for file copy/move       */
/* gCurrentTMC is being set in this function so that "Help" can be                      */
/* provided when user hits "F1"                                                                                                                 */
BOOL FAR PASCAL FDlgreplaceconfirm(WORD dlm, TMC tmc,
											WORD wNew, WORD wOld, WORD wParam)
{
	PWND lwind ;
	int  itemwidth ;
	char Temp[70] ;

	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	/* Actually help is the only case to be handled here. Otherwise,
	   we just dismiss the dialog and caller looks at the tmc value on exit */
	if (dlm == dlmInit)
	{
		SetUpDialog(tmcOK, szReplaceFileConfirmCaption);
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcreplaceconfirmNo);
		SetUpButtonForGraphics(tmcCancel);

		FormStringWithoutPlaceHolders(Temp, szReplaceFile, (char far *)
								(gfDestIsReadOnly ? szReadOnlyInParens : szBlank) ) ;
		Shell_SetTmcText(tmcReplaceFile, Temp) ;

		lwind = PwndOfTmc(tmcReplacePath);
		itemwidth = lwind->arcWindow.axRight - lwind->arcWindow.axLeft ;

		/* Make Temp hold the CompressedReplacePath */
		MakeDirNameFit(gReplaceDestPath, NULL, Temp, itemwidth) ;               
		Shell_SetTmcText(tmcReplacePath, Temp) ;

		Shell_SetTmcText(tmcReplaceInfo, gpszReplaceDestInfo) ;
	
		Shell_SetTmcText(tmcWithFile, szWithFile) ;
		lwind = PwndOfTmc(tmcWithPath);
		itemwidth = lwind->arcWindow.axRight - lwind->arcWindow.axLeft ;
		/* Make Temp hold the CompressedWithPath */
		MakeDirNameFit(gReplaceSrcPath, NULL, Temp, itemwidth) ;                
		Shell_SetTmcText(tmcWithPath, Temp) ;

		Shell_SetTmcText(tmcWithInfo, gpszReplaceSrcInfo) ;

		gCurrentTMC = tmcOK;
	}
	else
	if (dlm == dlmSetFocus)
	{
	    gCurrentTMC = tmc;
	}

	return TRUE ;
} /* FDlgreplaceconfirm */

#define MAXSIZESIZE 30


/* This function puts up the replace confirmation dialog before performing
 * a file replace on move/copy file operation.
 *
 * INPUT:
 *
 * OUTPUT: 
 *              ACT_FORCE, if the user selected the YES button.
 *              ACT_SKIP, if the user selected the NO button.
 *              ACT_CANCEL, if the user selected the CANCEL button.
 */
int ReplaceConfirmationDialog(char *srcpath, char *destpath, PENTRY srcnode,
															struct find_t destfindinfo)
{
	HCABreplaceconfirm hcab ;
	TMC tmc ;
	int ret ;
	char szSrcInfo[65] ;
	char szDestInfo[65] ;
	int i, j ;
	char szSize[MAXSIZESIZE] ;

	hcab = HcabAlloc(cabiCABreplaceconfirm) ;

	/* ZZZZZ DO I put up an outofmemory dialog now?? */
	if (!hcab)
	{
		OutOfMemory() ;
		return FALSE ;
	}

	gfDestIsReadOnly = destfindinfo.attrib & _A_RDONLY ;

	/* Set the following globals so that the dialog procedure can access these
	 * values to display in the dialog box.
	 */
	gReplaceSrcPath = srcpath ;
	gReplaceDestPath = destpath ;

   Get_Date_and_Time_Strings(srcnode->dtlx.dt.date, 0, szSrcInfo, NULL, 0);
   i = 8;
	szSrcInfo[i++] = (char) ' ' ;
	szSrcInfo[i++] = (char) ' ' ;
	szSrcInfo[i++] = (char) LBRACKET_CHAR ;

	/* pad with blanks to start with -- used by fn CopyNumberForTextOut()! */
	for (j = 0 ; j < MAXSIZESIZE ; j++)
		szSize[j] = ' ' ;
 
	CopyNumberForTextOut(szSize+MAXSIZESIZE-1, srcnode->x.f.size, FALSE) ;

	/* locate the start of the string that holds the formatted number! */
	j = 0 ;
	while (szSize[j] == ' ')
		j++ ;

	/* copy the formatted number string into our Info String */
	for( ; j < MAXSIZESIZE ; )
		szSrcInfo[i++] = szSize[j++] ;

	szSrcInfo[i++] = (char) ' ' ;   

	/* Copy the word bytes into our Info string */
	strcpy(szSrcInfo+i, szBytes) ;
	i += strlen(szBytes) ;

	szSrcInfo[i++] = (char) RBRACKET_CHAR ;
	szSrcInfo[i] = '\0' ;

	Get_Date_and_Time_Strings(destfindinfo.wr_date, 0, szDestInfo, NULL, 0);
	i = 8 ;
	szDestInfo[i++] = (char) ' ' ;
	szDestInfo[i++] = (char) ' ' ;
	szDestInfo[i++] = (char) LBRACKET_CHAR ;

	CopyNumberForTextOut(szSize+MAXSIZESIZE-1, destfindinfo.size, TRUE) ;

	/* locate the start of the string that holds the formatted number! */
	j = 0 ;
	while (szSize[j] == ' ')
		j++ ;

	/* copy the formatted number string into our Info String */
	for( ; j < MAXSIZESIZE ; )
		szDestInfo[i++] = szSize[j++] ;

	szDestInfo[i++] = (char) ' ' ;  

	/* Copy the word bytes into our Info string */
	strcpy(szDestInfo+i, szBytes) ;
	i += strlen(szBytes) ;

	szDestInfo[i++] = (char) RBRACKET_CHAR ;
	szDestInfo[i] = '\0' ;

	gpszReplaceSrcInfo = szSrcInfo;
	gpszReplaceDestInfo = szDestInfo;

	InitCab(hcab, cabiCABreplaceconfirm) ;

	SzToCab(hcab, szYesButton, Iag(CABreplaceconfirm, pszreplaceconfirmYB));
	SzToCab(hcab, szNoButton, Iag(CABreplaceconfirm, pszreplaceconfirmNB));
	SzToCab(hcab, szCancelButton, Iag(CABreplaceconfirm, pszreplaceconfirmCB));


	tmc = MyTmcDoDlg(&dlgreplaceconfirm, hcab) ;

	FreeCab(hcab) ;

	switch (tmc)
	{
		case tmcOK:
			ret = ACT_FORCE ;
			break ;

		case tmcreplaceconfirmNo:
			ret = ACT_SKIP ;
			break ;
		
		case tmcCancel:
		default:
			ret = ACT_CANCEL ;
	} /* switch */

	return ret ;
} /* ReplaceConfirmationDialog */

