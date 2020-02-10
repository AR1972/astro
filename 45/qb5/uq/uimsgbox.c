/*** 
*uimsgbox.c - Simple Dialog Box management functions
*
*	Copyright <C> 1985-1988, Microsoft Corporation
*
*Purpose:
*	Currently just an interface to CW for message boxes, dlgPrompt, and
*	compiler and quick-lib error handling.
*
*******************************************************************************/

/* First, include version definition header */
#include <version.h>

/* Next, include COW's interface headers */
#include <cw/version.h>
#include <cw/windows.h>
#include <cw/edityp.h>
#include <uiext.h>
#include <uinhelp.h>			//[7]

/* include dialog information */
#include <cw/dlg.h>
#include "uimsgbox.hs"
#include "uimsgbox.sdm"

/* Next, include QBI's headers */
#ifndef QBIMSGS_H
#include <qbimsgs.h>
#endif

#ifndef UI_H
#include <ui.h>
#endif

#ifndef UIINT_H
#include <uiint.h>
#endif

ushort NEAR MsgBoxStd2(ushort, ushort, ushort);
void NEAR strspcat (char *, int);
void NEAR MsgBoxCompErr(void);
ushort NEAR MsgBoxPsz(ushort, char *);	//[11]


/* Values setup by runtime for MsgBoxCompErr */
extern ushort b_ErrLin;    /* line # of last runtime error (ERL) */
extern ushort b_fCompErr;  /* Compiler error, set to value of uierr */
extern char far *b_ErrMod; /* far ptr to 0-terminated module name */
extern long b_ErrAdr;      /* far ptr where runtime call was made */

/* BASIC runtime functions */
extern void far B_ASSN(char far *, ushort, char far *, ushort);

void (near pascal *pMsgBoxHook)() = NULL;
char far *fpPFSText;

char szNull;		  //[23] A static NULL string
extern ushort iHelpId;
extern ushort HelpFlags;				//[14]
extern VOID FAR Help(WORD, WORD, VOID *, WORD);		//[7]


/****************************************************************************
* ushort MsgBoxStd2(mbType, iMsg1, iMsg2)
* Purpose:
*  Display a 2 line generic dialog box and wait for user's reply.
*
* Entry:
*  mbType = MB_OK, MB_YESNOCANCEL, or MB_RETRYCANCEL
*  iMsg1 and iMsg2 are standard BASIC message ids (ER_xxx or MSG_xxx)
*     from qbimsgs.h
*
* Exit:
*  returns IDOK, IDYES, IDNO, IDCANCEL, or IDRETRY,
*     depending upon which button was pushed by the user.
*
****************************************************************************/
ushort NEAR MsgBoxStd2(mbType, iMsg1, iMsg2)
ushort mbType;
ushort iMsg1;
ushort iMsg2;
{
    char Msg1[CB_bufStdMsg], Msg2[CB_bufStdMsg];	// [20]

    ListStdMsg (iMsg1);
    strcpy (Msg1, bufStdMsg);
    ListStdMsg (iMsg2);
    strcpy (Msg2, bufStdMsg);		// [19]

    return (UIMessageBox (Msg1, Msg2, NULL, mbType));

} /* MsgBoxStd2 */

/****************************************************************************
* ushort MsgBoxBd(mbType, bdMsg)
* Purpose:
*  Display a generic dialog box and wait for user's reply.
*
* Entry:
*  mbType = MB_OK, MB_YESNOCANCEL, or MB_RETRYCANCEL
*  bdMsg points to a bd (buffer descriptor) for the ASCII message to be
*     displayed.  For example MsgBoxBd(MB_OK, (bd *)&ps.bdErr);
*
* Exit:
*  returns IDOK, IDYES, IDNO, IDCANCEL, or IDRETRY,
*     depending upon which button was pushed by the user.
*
****************************************************************************/
ushort NEAR MsgBoxBd(mbType, bdMsg)
ushort mbType;
bd *bdMsg;
{
    //[9] null terminate the message in bdMsg

    if (BdAppend(bdMsg, &szNull, 1) == 0) {	//[23] try to grow the BD
	MsgBoxStd(MB_OK,ER_OM); 		//[20] ran out of memory, give
						//[20] OM error instead of bdMsg
	return (IDCANCEL);			//[20] return CANCEL to be safe
    }

    return MsgBoxPsz(mbType, bdMsg->pb);	//[11]
}


/****************************************************************************
* ushort MsgBoxPsz(mbType, PszMsg)
* Purpose:
*  Display a generic dialog box and wait for user's reply.
*  Added with revision [11].
*
* Entry:
*  mbType = MB_OK, MB_YESNOCANCEL, or MB_RETRYCANCEL
*  pszMsg points to a string to be displayed.
*
* Exit:
*  returns IDOK, IDYES, IDNO, IDCANCEL, or IDRETRY,
*     depending upon which button was pushed by the user.
*
****************************************************************************/
ushort NEAR MsgBoxPsz(mbType, pszMsg)
ushort mbType;
char *pszMsg;
{
    char Msg[CB_bufStdMsg];			//[21]

    strcpy (Msg, pszMsg);			//[21]
    return (UIMessageBox (Msg, NULL, NULL, mbType));
}

/****************************************************************************
* ushort MsgBoxStdBd(mbType, iMsg, bdMsg)
* Purpose:
*  Display a generic dialog box, with a variable message on 2nd line
*  and wait for user's reply.
*
* Entry:
*  mbType = MB_OK, MB_YESNOCANCEL, or MB_RETRYCANCEL
*  iMsg is a standard BASIC message id (ER_xxx or MSG_xxx) from qbimsgs.h
*  bdMsg points to a bd (buffer descriptor) for the ASCII message to be
*     displayed on 2nd line.
*
* Exit:
*  Caller's grs.oRsCur is preserved.
*  returns IDOK, IDYES, IDNO, IDCANCEL, or IDRETRY,
*     depending upon which button was pushed by the user.
*
****************************************************************************/
ushort NEAR MsgBoxStdBd(mbType, iMsg, bdMsg)
ushort mbType;
ushort iMsg;
bd *bdMsg;
{
    char Msg[CB_bufStdMsg];	//[20]

    iHelpId = iMsg;		//[12] as ListStdMsg may change it if it

    ListStdMsg (iMsg);
    strcpy (Msg, bufStdMsg);	//[19]

    //[9] null terminate the message in bdMsg

    if (BdAppend(bdMsg, &szNull, 1) == 0) {	//[23]
	MsgBoxStd(MB_OK,ER_OM); 		//[20] give OM error instead
	return (IDCANCEL);			//[20] return CANCEL to be safe
    }

    return (UIMessageBox (Msg, bdMsg->pb, NULL, mbType));
} /* MsgBoxStdBd */

/****************************************************************************
* TMC PromptForString(iMsg, fpBuffer, cchMac)
* Purpose:
*   Displays a message box with one message, one edit field, and OK button,
*   and a CANCEL button.
*
* Entry:
*  iMsg - Message to display as prompt
*  fpBuffer - Initial string, and where the result is placed.
*  cchMac - Max length of string to store in fpBuffer.
*
* Exit:
*  returns tmcOK, or tmcCANCEL
*     depending upon which button was pushed by the user.
*  if OK button is pushed
*     Copies the string entered in the edit field to fpBuffer.
*
****************************************************************************/
TMC NEAR PromptForString (iMsg, fpBuffer, cchMac)
ushort iMsg;
char far *fpBuffer;
ushort cchMac;
{
    TMC tmc = tmcCancel;	// [13]
    HCABPrompt hcabPrompt;
    char buffer[128];		//[11] max size of cow edit field

    DbAssert (fDebugScr != 0);

    DbAssert (uierr == 0);	//[11]
    hcabPrompt = (HCABPrompt) HcabAlloc(cabiCABPrompt);
    /* [2] Catch HcabAlloc error */
    if (uierr) {
	return (tmcCancel);	//[26] return error
    }

    iHelpId = iMsg;

    ListStdMsg (iMsg);
    SzToCab (hcabPrompt, bufStdMsg, Iag (CABPrompt, szPromptHeader));
    fstrcpy2 ((char far *) buffer, fpBuffer);			// [6]
    SzToCab (hcabPrompt, buffer, Iag (CABPrompt, szPrompt));	// [6]
    // [13] if SzToCab memory allocation failed, free cab and exit.
    if (uierr)
	goto EndPromptForString;

    if ((tmc = TmcDoDlgFar (&dlgPrompt, sizeof (dlgPrompt), hcabPrompt)) == tmcOK) {
	SzFromCab (hcabPrompt, buffer, cchMac, Iag (CABPrompt, szPrompt));
	fstrcpy2 (fpBuffer, (char far *) buffer);
    }


EndPromptForString:		// [13]
    FreeCab (hcabPrompt);	/* [4] */
    return (tmc);
}

/****************************************************************************
* void MsgBoxCompErr
* Purpose:
*  Display a Compiled Code Runtime Error dialog box
*          +------------------------------------+
*          |      <Runtime error message>       |
*          | In Quick library module: xxxxxxxx  |
*          | At offset: xxxx     ERL: xxxxx     |
*          +------------------------------------+
*  and wait for user's reply.
*
* Entry:
*  uierr = error code (ER_xxx or MSG_xxx from qbimsgs.h)
*  b_ErrLin = line # of last runtime error (ERL)
*  b_ErrMod = far ptr to 0-terminated module name
*  b_ErrAdr = far ptr where runtime call was made
*
****************************************************************************/
#define COMP_ERR_WIDTH 50
#define CB_MOD_NAME 8

void NEAR strspcat (sz, n)
register char *sz;
register int n;
{
    while (*sz++) ;
    --sz;			//[22] back up one char to point at null
    while (n--) *sz++ = ' ';
    *sz = '\0';
}


void NEAR MsgBoxCompErr()
{
    DbAssert(FALSE);		//[24] shouldn't ever get here
} /* MsgBoxCompErr */


/*** 
*VOID NEAR DoDlgGreetingBox ()
*Purpose:
*	Do the Greeting Box dialog.
*
*	If the dialog returns tmcSurvival, display the survival guide in the
*	help window.
*
*Entry:
*	None.
*
*Exit:
*	None.
*
*Exceptions:
*	None.
*******************************************************************************/
VOID NEAR DoDlgGreetingBox ()
{
    HCABGreetingBox hcab;

    DbAssert (uierr == 0);	//[11]
    hcab = (HCABGreetingBox) HcabAlloc (cabiCABGreetingBox);
    if (uierr)
	return;

    (*hcab)->u.sab = (cmdSwitches & CMD_SW_ED) ? sabGreetingQedit : sabGreetingQbas;    //[25]
    if (TmcDoDlgFar (&dlgGreetingBox, sizeof (dlgGreetingBox), hcab) != tmcCancel)
	Help (hemWindowHid,helpSurvivalId,0,0);	//[10] display survival help

    FreeCab (hcab);
}
