/***
*uiint.h - Defines to the QB user interface
*
*	Copyright <C> 1985-1988 Microsoft Corporation
*
*Purpose:
* NOTE: When making changes to this file, be sure to make equivalent
*	changes to file UIINT.INC
*
*******************************************************************************/
#undef UIINT_H
#define UIINT_H ON         /* remember that this file has been included */

#include <uidec.h>
#include <uimenu.h>

#define Unreferenced(x) ((void)x)

/*------------------------- Window Definitions -------------------------------*/


/* maximum physical line width of edit field.  COW library's edit.c
   must change if infinite length lines are allowed */
#define MAX_EDITLINE	256

#define MAX_SEARCH_PATH	127+1		//[6] path size + null
extern WORD b$fInt24Err;		//[6]


// a-emoryh
#define MAX_PATH  80
#define cDEVICES  4

#define DEV_LPT1  0
#define DEV_LPT2  1
#define DEV_LPT3  2
#define DEV_COM1  3
#define DEV_COM2  4


extern WND wndMain;
extern WND wndDebug;
extern WND wndHelp;
extern WND wndStatus;

/* Window id's */
#define idWndMain	1
#define idWndEdit	2
#define idWndDebug	3
#define idWndHelp	4
#define idWndStatus	5
#define idWndScrollV	6
#define idWndScrollH	7

/* Window stlye bits used by the user interface */
#define UIWS_MOVECURSOR	0x0001		//[09]

#define hbufHelp	0xFFFE

/* register set types - as passed to RsMake() */
#define RS_scrap	(char)0	/* for Scrap */
#define RS_cmdWnd	(char)1	/* for Command Window */
#define RS_module       (char)2
#define RS_includeFile  (char)3
#define RS_document     (char)4
#define RS_sub          (char)5
#define RS_function     (char)6

/* flag parms for GetRsName() */
#define RSN_fFullName	(uchar) 1
#define RSN_fIndent	(uchar) 2

/* Direct Mode Action flags */
#define FDM_ShowStmt 1
#define FDM_GetCmd 2
#define FDM_ExecCmd 4
#define FDM_ShowWatch 8


/*[4] Masks to eliminate certain characters or sets of characters in */
/*[4] a call to GetEditWordMask.  Note that these constants are not  */
/*[4] abitrary, and must relate to the table in UIWORD.ASM.	     */

#define GEW_DFLTMASK	0x5E03	/* default (normal) search characters */
#define GEW_NODOTMASK	0x5E01	/* Do not include . in search	      */
#define GEW_HELPMASK	0x1E01	/* Do not include . or ! in search    */
#define GEW_VARMASK	0x0000	/* Rules for variable help search     */
#define GEW_VARDOTMASK	0x0002	/* variable help with . allowed       */

/* Return codes from routines in uinhelp.asm */

#define HELP_OK 	0
#define HELP_NF 	1
#define HELP_HANDLED	2	//[7] We got a help error that someone else
				//[7] will handle (by putting up a dialog box)
#define HELP_NOVARHELP	4	//[11] unable to regenerate variable help

/* flag values for HelpFlags */

#define HLP_GOTBUF	0x01	/* help buffer is allocated and initialized */
#define HLP_NOSHRINK	0x02	/* do not call HelpShrink in ShrinkHelp */
#define HLP_COMPRESS	0x04	/* help has been compressed (need to */
				/* not re-open help window when UI restarts) */
#define HLP_INHELP	0x08	/* We are in help system. Do not re-enter */
#define HLP_VARHELP	0x10	/* We are displaying Variable Help */
#define HLP_FAILOOM	0x20	/* We failed callback (insufficient memory) */
#define HLP_FAILFNF	0x40	/* We failed callback (file not found) */
#define HLP_RTERR	0x80	/* Next call to ReportError is for a runtime
				   generated error, not for an interpreter
				   error [8]*/

// Flag values for HelpFlags2

#define HLP2_DLGBOX	0x01	/* [10] this help will go in a dialog box */


/* Prefix characters for context strings [NOTE: used in SED script] */

#define PREFIX_MESSAGE	'-'

/* base number of Special Interpreter Version Help Ids.  The message box
   that is displayed is identical to a runtime error, but the help topic
   is different */

#define HELP_INTERPBASE 2000	//[8]
