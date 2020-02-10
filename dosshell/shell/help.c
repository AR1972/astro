;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****************************************************************/
/*************************** INCLUDES ***************************/
/****************************************************************/

#include <common.h>

/* To help generate the help text, un-commnet the following line.
 * This will make the shell print the context string
 * that it is using to search in the help file.
 */
// #define    PRINTDEBUGCONTEXTSTR 1

#include <help.hs>
#include <help.sdm>

#include <filemgr.h>
#include <prot.h>
#include <text.h>

/****************************************************************/
/*************************** CONSTANTS **************************/
/****************************************************************/

typedef struct tagHelpState {
    long lHelpOffset;
    int nHelpScroll;
} HelpState;

#define MAXHELPLINE  68
#ifdef DBCS
int     FarCheckDBCSTailByte(unsigned char far *, unsigned char far *);
int     CheckDBCSTailByte(unsigned char *, unsigned char *);
#else
#define FarCheckDBCSTailByte(x,y) (FALSE)
#define CheckDBCSTailByte(x,y) (FALSE)
#endif
#define CONTEXTSTARTCHAR '!'
#define CONTEXTSTARTENDCHAR '@'
#define CONTEXTENDCHAR '@'
#define HOTLINKCHAR '~'

/* the chunksize we read help in */
#define MAXHELPCHUNK 8192
/* the maximum size for a single help entry */
#define MAXHELPSIZE 2048
#define REENTERHELP ((MAXHELPCHUNK-2*MAXHELPSIZE)/sizeof(HelpState))

#define MAXCONTEXTLEN 7 /* example: "!1DMN! " */
#define LINKEXTRA (MAXCONTEXTLEN + 4)
/* just an artificial limit so things don't get out of hand */
#define MAXCONTEXTSPERTEXT 50
char gCurContext[MAXCONTEXTLEN+1];
char gHotLink[MAXCONTEXTLEN+1];

#define PROGRAMCONTEXTOFFSET 0
#define MODECONTEXTOFFSET    1
#define MODESUBCONTEXTOFFSET 2
#define MODESUBSUBCONTEXTOFFSET 3
#define NULLCONTEXTOFFSET    4

#define CONTEXTCHARINDEX   'I'
#define CONTEXTCHARKEY       'K'
#define CONTEXTCHARBASICS  'B'
#define CONTEXTCHARCOMMANDS 'Q'
#define CONTEXTCHARPROCEDURES 'R'
#define CONTEXTCHARUSING 'U'
#define CONTEXTCHARSPECIAL 'H'

#define CONTEXTCHAROK 'O'
#define CONTEXTCHARCANCEL 'C'
#define CONTEXTCHARCOPY 'C'
#define CONTEXTCHARREORDER 'R'

#define CONTEXTCHARPROGMAN  'P'
#define CONTEXTCHARSINGLE   '1'
#define CONTEXTCHARDOUBLE   '2'
#define CONTEXTCHARSYSTEM   'Y'
#define CONTEXTCHARSEARCH   'S'
#define CONTEXTCHARVIEW     'V'
#define CONTEXTCHARTASKMAN  'T'

#define CONTEXTCHARNULL     'N'
#define CONTEXTCHARMENU     'M'
#define CONTEXTCHARDIALOG   'D'

#define INDEXMODE  0
#define KEYMODE    1
#define BASICSMODE 2
#define COMMANDSMODE 3
#define PROCEDURESMODE 4
#define USINGMODE 5
#define HELPONHELPMODE 6
#define HOTLINKMODE 7
#define REGULARHELP 8
#define EXTRABUFFER (MAXCONTEXTLEN*MAXCONTEXTSPERTEXT)
#define HOTCHAR '"'

#define NOHELPAVAIL (-1)
#define USERDEFHELP (-2)

/****************************************************************/
/*************************** GLOBALS ****************************/
/****************************************************************/

char gErrHelpId ;

int gNumHelps = -1; /* number of times help has been re-entered */
BOOL gHelpErr = FALSE; /* Gets set if we have a help error condition */
ListBoxData HelpList;
char far *gCurHelp = NULL; // we only need one of these
HelpState far *gHelpState = NULL; // we only need one of these
WORD gHelpChunk;

WORD gHelpSize;        // we only need one of these
WORD gNumHelpLines;
WORD far *gHelpLines;
TMC gCurrentTMC = tmcNull;
WORD gCWContext = 0;
WORD gCWhid = 0;
VOID *gCWpv = NULL;
char cMode[] = {
    CONTEXTCHARINDEX, CONTEXTCHARKEY, CONTEXTCHARBASICS, CONTEXTCHARCOMMANDS,
    CONTEXTCHARPROCEDURES, CONTEXTCHARUSING, CONTEXTCHARSPECIAL
} ;
WORD gSpecialMode = REGULARHELP;
int gHotLinkLine = 0; // see below

extern char gStartUpDir[];
extern int gStartUpDirEnd; /* location where the NULL goes in the above name */
extern char userCommand[];
#define szTitle userCommand
extern BOOL fDrawItem;
extern BOOL fRedrawItem;
extern BOOL gInCopy, gInReorder;
extern char *szCurDialogCaption;
extern ListBoxData ProgramList, TaskList;
extern BOOL gTaskListEnabled;

/****************************************************************/
/*************************** FUNCTION DECLARATIONS **************/
/****************************************************************/

VOID FAR HelpBox(void);
VOID FAR KeyBox(void);
VOID FAR IndexBox(void);
VOID FAR UsingHelpBox(void);

VOID NEAR PASCAL GoNextHotLink(BOOL bOnlyInView);
VOID NEAR PASCAL GoPrevHotLink(void);

extern TOKEN Get_Focus_Item(void);

/****************************************************************/
/*************************** PRIVATE FUNCTIONS ******************/
/****************************************************************/

int PASCAL TextLineLen(char far *lpText, int nMaxLen, char cEndChar)
{
    int i, nLastWord;
    BOOL bSpace = FALSE;
#ifdef DBCS
    char far *lpStartText = lpText;
#endif

    if(*lpText==cEndChar && (!cEndChar || *(lpText+1)==cEndChar))
	return(-1);

    for(i=0, nLastWord=0; ; ++i, ++lpText) {
	if(*lpText==cEndChar && !FarCheckDBCSTailByte(lpStartText, lpText)
		&& (!cEndChar || *(lpText+1)==cEndChar))
	    return(i);

	switch(*lpText) {
	case('\n'):
	case('\r'):
	    if(!FarCheckDBCSTailByte(lpStartText, lpText))
		return(i);
	    break;

#ifndef JAPAN
	case(' '):
	    if(!FarCheckDBCSTailByte(lpStartText, lpText))
		bSpace = TRUE;
	    break;
#endif

	case('^'):
	    if(gHelpState[gNumHelps].lHelpOffset==USERDEFHELP &&
		    !FarCheckDBCSTailByte(lpStartText, lpText) &&
		    (*(lpText+1)=='m' || *(lpText+1)=='M')) {
		*lpText     = '\r';
		*(lpText+1) = '\n';
		return(i);
	    }
/* NOTE: we fall through here */
	default:
	    if(bSpace) {
		bSpace = FALSE;
		nLastWord = i;
	    }
	    break;
	}

	if(i >= nMaxLen-1) {
	    if(bSpace) {
		for(++lpText, ++i; *lpText==' '; ++lpText, ++i)
		    /* skip trailing spaces */ ;
		return(i);
	    }

	    if(!nLastWord && FarCheckDBCSTailByte(lpStartText, lpText))
		return(i-1);

	    return(nLastWord ? nLastWord : i);
	}
    }
}

VOID NEAR PASCAL Get_Ith_Help_Line(char *line, int ith)
{
    int i, nLineLen;
    char far *lpHelpText = gCurHelp + gHelpLines[-ith];

/* Fill the buffer to the newline or MAXHELPLINE chars */
    nLineLen = TextLineLen(lpHelpText, MAXHELPLINE, CONTEXTENDCHAR);
    if(nLineLen > MAXHELPLINE-1)
	nLineLen = MAXHELPLINE - 1;
    for(i=nLineLen; i>0; --i)
	*line++ = *lpHelpText++;

/* Pad the rest with spaces */
    for(i=nLineLen; i<MAXHELPLINE; ++i)
	*line++ = ' ';
    *line = '\0';
}

VOID NEAR PASCAL SetUpCurHelp(void)
{
    int i = 0, j, nLineLen;
    char far *lpHelpText = gCurHelp;

    gNumHelpLines = 0;

    if(gHelpState[gNumHelps].lHelpOffset == NOHELPAVAIL ||
	    (nLineLen=TextLineLen(lpHelpText, MAXHELPLINE, CONTEXTENDCHAR))<0) {
	FormStringWithoutPlaceHolders(lpHelpText, "\n%1@@",
		(char far *)szNoHelpAvail);
	nLineLen = 0;
    }

    for( ; i<nLineLen; ++i)
	szTitle[i] = *lpHelpText++;
    szTitle[i] = '\0';
    SetTitle(&HelpList, szTitle);

    while(*lpHelpText=='\n' || *lpHelpText=='\r') {
	++i;
	++lpHelpText;
    }

    for( ; ; ++gNumHelpLines) {
	gHelpLines[-gNumHelpLines] = i;
	nLineLen = TextLineLen(lpHelpText, MAXHELPLINE, CONTEXTENDCHAR);
	if(nLineLen < 0)
	    break;

	i += nLineLen;
	lpHelpText += nLineLen;
	if(i >= (gHelpSize-gNumHelpLines*sizeof(WORD)))
	    goto FillWithBlank;

	for( ; *lpHelpText=='\r'; ++i, ++lpHelpText)
	    /* do nothing */ ;
	if(*lpHelpText == '\n') {
	    ++i;
	    ++lpHelpText;
	}
    }

/* Add the end of help marker */
    *lpHelpText++ = '\n';
    gHelpLines[-gNumHelpLines++] = ++i;
    nLineLen = strlen(szHelpEnd);
    for(j=(MAXHELPLINE-nLineLen)/2, i+=j+nLineLen; j>0; --j)
	*lpHelpText++ = ' ';
    RepeatMove(lpHelpText, szHelpEnd, nLineLen);
    lpHelpText += nLineLen;

/* Fill the rest of the gHelpLines array with blank lines */
FillWithBlank:
    *lpHelpText = '\r';
    for( ; gNumHelpLines<HelpList.numlinesonscreen; ++gNumHelpLines)
	gHelpLines[-gNumHelpLines] = i;

    GoNextHotLink(FALSE);
    HelpList.focusitem = gHotLinkLine;
}

VOID NEAR PASCAL Free_Help(void)
{
    if(gHelpState)
	FreeWorkFar(gHelpState);

    gHelpState = NULL;
}

VOID NEAR PASCAL Get_Current_Context(void)
{
    BOOL bNull;
    char topcontextchar;
    char contextchar;
    char subcontextchar;
    char subsubcontextchar;

    topcontextchar = ' ';
    contextchar = ' ';
    subcontextchar = ' ';
    subsubcontextchar = ' ';

    bNull = !(gCWContext==hemMenu || gCWContext==hemMenuItem ||
	    gCWContext==hemDialog);

    if(gSpecialMode < HOTLINKMODE) {
	topcontextchar = cMode[gSpecialMode];
    } else if(gSpecialMode == HOTLINKMODE) {
	topcontextchar = gHotLink[1];
/* Does the tail byte check need to be done?
 * I don't think we allow non-ASCII context strings
 */
	if(gHotLink[2] != HOTLINKCHAR ||
		CheckDBCSTailByte(gHotLink,&gHotLink[2])) {
	    contextchar = gHotLink[2];
	    if(gHotLink[3] != HOTLINKCHAR ||
		    CheckDBCSTailByte(gHotLink,&gHotLink[3])) {
		subcontextchar = gHotLink[3];
		if(gHotLink[4] != HOTLINKCHAR ||
			CheckDBCSTailByte(gHotLink,&gHotLink[4])) {
		    subsubcontextchar = gHotLink[4];
		}
	    }
	}
    } else {
	if(m_fPerformingViewFile())
	    topcontextchar = CONTEXTCHARVIEW;
	else if(!glob.InFileMgr || glob.TreeMode==TR_SHARE) {
/* We are either in program manager view, or shared mode */
	    if(ProgramList.hasglobalfocus) {
		if(!bNull)
		    topcontextchar = CONTEXTCHARPROGMAN;
		else if(gInCopy) {
		    topcontextchar = CONTEXTCHARSPECIAL;
		    contextchar = CONTEXTCHARCOPY;
		    goto ContextFound;
		} else if(gInReorder) {
		    topcontextchar = CONTEXTCHARSPECIAL;
		    contextchar = CONTEXTCHARREORDER;
		    goto ContextFound;
		} else
		    topcontextchar = CONTEXTCHARPROGMAN;
	    } else if(gTaskListEnabled && TaskList.hasglobalfocus && bNull)
		topcontextchar = CONTEXTCHARTASKMAN;
	    else
		topcontextchar = CONTEXTCHARSINGLE;
	} else {
	    switch(glob.TreeMode) {
	    case TR_SEARCH:
		topcontextchar = CONTEXTCHARSEARCH;
		break;

	    case TR_DOUBLE:
		topcontextchar = CONTEXTCHARDOUBLE;
		break;

	    case TR_SYSTEM:
		topcontextchar = CONTEXTCHARSYSTEM;
		break;

	    /* case TR_SINGLE: */
	    default:
		topcontextchar = CONTEXTCHARSINGLE;
		break;
	    }
	}

	switch(gCWContext) {
	case hemMenu:
	case hemMenuItem:
	    contextchar = CONTEXTCHARMENU;
	    subcontextchar = ((((unsigned char )gCWhid)&0x0F0) >> 4) + 'A';
	    subsubcontextchar = (((unsigned char) gCWhid)&0x0F) + 'A';
	    break;

	case hemDialog:
	    if(gCurrentTMC == tmcOK) {
		topcontextchar = CONTEXTCHAROK;
	    } else if(gCurrentTMC == tmcCancel) {
		topcontextchar = CONTEXTCHARCANCEL;
	    } else {
		contextchar = CONTEXTCHARDIALOG;
		subcontextchar = (char) gCWhid;
		if(subcontextchar == hidERR) {
		    subsubcontextchar = (char) gErrHelpId ;
		} else {
		    subsubcontextchar = (char) (gCurrentTMC+'A') ;
		}
	    }
	    break;

	default:
	    contextchar = CONTEXTCHARNULL;
	}
    }

ContextFound:

	 /* HACK!!! The help file does not have the context strings for all
	  * topics in SEARCH mode. Hence we now modify all the SEARCH context
	  * strings except the "SN" (default search help) to make it look
	  * like SINGLE tree mode.
	  */
	 if ( (topcontextchar == CONTEXTCHARSEARCH) &&
		   (contextchar != CONTEXTCHARNULL)
		 )
	 {
		topcontextchar = CONTEXTCHARSINGLE ;
	 }

    gCurContext[PROGRAMCONTEXTOFFSET] = topcontextchar;
    gCurContext[MODECONTEXTOFFSET] = contextchar;
    gCurContext[MODESUBCONTEXTOFFSET] = subcontextchar;
    gCurContext[MODESUBSUBCONTEXTOFFSET] = subsubcontextchar;
    gCurContext[NULLCONTEXTOFFSET] = 0;

    gSpecialMode = REGULARHELP;

#ifdef PRINTDEBUGCONTEXTSTR
    gHelpErr = TRUE;
    ShellMessageBox("Context string:", gCurContext);
    gHelpErr = FALSE;
#endif
}

int Get_Cur_Help(void)
{
    int i, k;
    char far *gHelpText;

    for(i=0, gHelpText=gCurHelp; i<gHelpSize; ++i, ++gHelpText) {
/*
 * Find the correct text for this context.
 * the format is !context!...@text...%
 */
	if(*gHelpText==CONTEXTSTARTCHAR &&
		!FarCheckDBCSTailByte(gCurHelp, gHelpText)) {
/* we have found a '!', now see if the string following
 * is the correct context...
 */
	    for(k=0, ++i, ++gHelpText; *gHelpText==gCurContext[k];
		    ++k, ++i, ++gHelpText)
		if(k > MAXCONTEXTLEN)
/* something is wrong with the help file, so
 * pretend we can't find the context string
 */
		    return(-1);

	    if(*gHelpText == CONTEXTSTARTCHAR) {
		if(gCurContext[k] == ' ' || gCurContext[k] == '\0') {
/* We found the context string; now look for the @ */
		    for(k=0; *gHelpText!=CONTEXTSTARTENDCHAR ||
			    FarCheckDBCSTailByte(gCurHelp, gHelpText);
			    ++k, ++i, ++gHelpText)
			if(k > EXTRABUFFER-MAXCONTEXTLEN)
/* something is wrong with the help file, so
 * pretend we can't find the context string
 */
			    return(-1);

/* We found the string and the '@' char */
		    return(i);
		}

/* If we get to here, we found a trailing '!' without
 * finding the whole context string, so let's try again
 */
		--i;
		--gHelpText;
	    }
	}
    }

/* We didn't find the string */
    return(-1);
}

VOID NEAR PASCAL SetUpUserHelp(void)
{
    char far *szHelpTitle;
    char far *szHelpText;
    TOKEN tkTemp, tkTemp1;

    gHelpSize = gHelpChunk;

    if((tkTemp=Get_Focus_Item()) < 0)
	goto Error1;
    else if(tkTemp==TK_PROGRAMSTARTER) {
	szHelpTitle = szMainGroup;
	szHelpText = szMainHelp;
    } else if((tkTemp=Get_Symbol_Value(Token_To_Symbol(tkTemp))) >= 0) {
	tkTemp1 = Get_KeyWord_Assignment(tkTemp, TK_TITLE);
	szHelpTitle = tkTemp1>=0 ? Get_Token_Identifier(tkTemp1) : (char far *)szNoItemTitle;
	tkTemp1 = Get_KeyWord_Assignment(tkTemp, TK_HELP);
	szHelpText = tkTemp1>0 ? Get_Token_Identifier(tkTemp1) : (char far *)szNoHelpAvail;
    } else {
Error1:
	szHelpTitle = szNoItemTitle;
	szHelpText = szNoHelpAvail;
    }

    FormStringWithoutPlaceHolders(gCurHelp, szHelpFor, szHelpTitle, szHelpText);
}

BOOL NEAR PASCAL IsUserContext(void)
{
    if(gInCopy || gInReorder)
	return(FALSE);

    if(gCurContext[0] != CONTEXTCHARPROGMAN)
	return(FALSE);
    if(gCurContext[1] == CONTEXTCHARNULL) {
	if(gCurContext[2] != ' ' || gCurContext[3] != ' ')
	    return(FALSE);
    } else if(gCurContext[1] == CONTEXTCHARDIALOG) {
	if(gCurContext[2] != hidUSER)
	    return(FALSE);
    } else
	return(FALSE);

    return(TRUE);
}

BOOL Get_Help(BOOL bBack)
{
    int i, fhandle, ind;
    BOOL bNotEOF;
    BOOL bHelpFileAvail = TRUE;
    char szErrorMessage[150];

    gNumHelpLines = 0;
    if(bBack) {
	if(gHelpState[gNumHelps].lHelpOffset==NOHELPAVAIL)
	    goto Error1;
    } else
	gHelpState[gNumHelps].lHelpOffset = NOHELPAVAIL;

    if(gNumHelps==0 && (IsUserContext() ||
	    (bBack && gHelpState[0].lHelpOffset==USERDEFHELP))) {
	gHelpState[0].lHelpOffset = USERDEFHELP;
	SetUpUserHelp();
	goto Error1;
    }

    ind = gStartUpDirEnd;
#ifdef DBCS
    if(gStartUpDir[ind-1] != '\\' || CheckDBCSTailByte(gStartUpDir,&gStartUpDir[ind-1]))
#else
    if(gStartUpDir[ind-1] != '\\')
#endif
	gStartUpDir[ind++] = '\\';
    strcpy(gStartUpDir+ind, szHelpFileName);
    if(_dos_open(gStartUpDir, O_RDONLY, &fhandle)) {
	FormStringWithoutPlaceHolders(szErrorMessage, szShellFileNotFound,
		(char far *)szHelpFileName);
	gHelpErr = TRUE;
	ShellMessageBox(szHelpErrTitle, szErrorMessage);
	gHelpErr = FALSE;
	bHelpFileAvail = FALSE;
	goto Error2;
    }

    if(bBack)
	goto FoundOffset;
	
/* initially read in the first EXTRABUFFER bytes */
    if(_dos_read(fhandle, gCurHelp, EXTRABUFFER, (unsigned *)&gHelpSize))
	goto Error3;

/* Fill in the rest of the buffer */
    for(bNotEOF=TRUE, gHelpState[gNumHelps].lHelpOffset=0;
	    bNotEOF && !_dos_read(fhandle, gCurHelp+EXTRABUFFER,
	    gHelpChunk, (unsigned *)&gHelpSize);
	    gHelpState[gNumHelps].lHelpOffset+=gHelpChunk) {
/* We look for the context string only in the first part of the buffer,
   until we hit EOF, when we'll look at the whole buffer.  This is so
   that if we find a context string, we can be sure that the whole help
   text is already in the buffer */
	if(gHelpSize < gHelpChunk) {
	    gHelpSize += EXTRABUFFER;
	    bNotEOF = FALSE;
	}

	if((i=Get_Cur_Help()) >= 0) {
	    for(++i; gCurHelp[i]=='\n' || gCurHelp[i]=='\r'; ++i)
		/* skip the '@', '\n', and '\r' */ ;
	    gHelpState[gNumHelps].lHelpOffset += i; /* skip the '@' */
FoundOffset:
	    lseek(fhandle, gHelpState[gNumHelps].lHelpOffset, SEEK_SET);
	    if(_dos_read(fhandle, gCurHelp, gHelpChunk, (unsigned *)&gHelpSize))
		goto Error4;
	    goto Error3; /* actually not an error */
	}

/* move the end of the buffer to the beginning */
	for(i=0; i<EXTRABUFFER; ++i)
	    gCurHelp[i] = gCurHelp[i+gHelpSize];
    }

Error4:
    gHelpState[gNumHelps].lHelpOffset = NOHELPAVAIL;
Error3:
    _dos_close(fhandle);
Error2:
    gStartUpDir[gStartUpDirEnd] = 0;
Error1:
    SetUpCurHelp();
    return(bHelpFileAvail);
}

BOOL FindHotLinks(char *outputline, int *hotstart, int *hotend,
	int *hotcontextstart, int *hotcontextend)
{
    int i;

/* Look for the beginning of a hot string */
    for(i=0; outputline[i]!=HOTCHAR ||
	    CheckDBCSTailByte(outputline,&outputline[i]); ++i)
	if(i >= MAXHELPLINE)
	    goto NotFound;
    *hotstart = i;

/* Look for the end of the hot string */
    for(++i; outputline[i]!=HOTCHAR ||
	    CheckDBCSTailByte(outputline,&outputline[i]); ++i)
	if(i >= MAXHELPLINE)
	    goto NotFound;
/* Look for the context string immediately following */
    if(outputline[i+1] != HOTLINKCHAR ||
	    CheckDBCSTailByte(outputline,&outputline[i+1]))
	goto NotFound;
    *hotend = i;
    *hotcontextstart = ++i;

/* Look for the end of the context string */
    for(++i; outputline[i]!=HOTLINKCHAR ||
	    CheckDBCSTailByte(outputline,&outputline[i]); ++i)
	if(i >= MAXHELPLINE)
	    goto NotFound;
    *hotcontextend = i;

    return(TRUE);
NotFound:
    *hotstart = -1;
    *hotend = -1;
    *hotcontextstart = -1;
    *hotcontextend = -1;
    return(FALSE);
}

WORD PASCAL ListProcHelpList(WORD tmm, char *sz, WORD isz, TMC tmc,
	WORD x, WORD y, WORD bArg)
{
    RX xval;
    char temp[MAXHELPLINE+LINKEXTRA+3];
    int hotstart, hotend, hotcontextstart, hotcontextend;
    ISA color;
    int i,j;

    UnReferenced(sz);
    UnReferenced(tmc);
    UnReferenced(x);

    xval = Get_List_Rect(&HelpList).axLeft;
    switch(tmm) {
    case tmmCount:
	return(gNumHelpLines);
	break;

    case tmmSetFocus:
	break;

    case tmmSelect:
	break;

    case tmmActivate:
	Get_Ith_Help_Line(temp, isz);
	if(FindHotLinks(temp, &hotstart, &hotend,
		&hotcontextstart, &hotcontextend)) {
	    for(j=0, i=hotcontextstart; i<=hotcontextend; ++j, ++i)
		gHotLink[j] = temp[i];
	    gHotLink[j] = 0;
	    gSpecialMode = HOTLINKMODE;
	    HelpBox();
	}
	break;

    case tmmGetItemString:
	if(isz < gNumHelpLines) {
	    Get_Ith_Help_Line(temp, isz);

	    if(FindHotLinks(temp, &hotstart, &hotend,
		    &hotcontextstart, &hotcontextend)) {
		for(i=hotstart+1; temp[i]==' '; ++i) ;
		RepeatMove(sz, temp+i, hotend-i);
		*(sz+hotend-i) = '\0';
	    } else
		*sz = '\0';
	}else
	{
		*sz = 0;
	}
	break;

    case tmmDrawItem:
	if(isz < gNumHelpLines) {
	    *temp = ' ';
	    Get_Ith_Help_Line(temp+1, isz);

	    if(FindHotLinks(temp, &hotstart, &hotend,
		    &hotcontextstart, &hotcontextend)) {
/* Put enough spaces at the end of the string so the printable part is the
   same length as the original string */
		i = MAXHELPLINE + 3 + hotcontextend - hotcontextstart;
		temp[i] = '\0';
		for(--i; i>=MAXHELPLINE; --i)
		    temp[i] = ' ';

		if(bArg)
		    gHotLinkLine = isz;

		color = (ISA)(bArg ? isaHilite : isaHotLink) ;
		TextOut(HelpList.pwd, (RX)xval+1, (RY)y,
			temp, hotstart, isaDialogBox);
		TextOut(HelpList.pwd, (RX)xval+1+hotstart, (RY)y,
			temp+hotstart+1, hotend-hotstart-1, color);
		TextOut(HelpList.pwd, (RX)xval+hotend, (RY)y,
			temp+hotcontextend+1, -1, isaDialogBox);
		DrawFocusMarker(HelpList.pwd, xval+1, (RY) y,
			HelpList.pwd->arcWindow.axLeft+xval+1+hotstart,
			HelpList.pwd->arcWindow.ayTop +y, hotend-hotstart-1,
			bArg & TF_ISFOCUS, FALSE,
			(ISA) (gisgraph ? isaHilite : isaDialogBox));
	    } else {
		TextOut(HelpList.pwd, (RX)xval+1,(RY)y,
			temp, -1, isaDialogBox);
	    }
	}
	break;

    default:
	break;
    }

    return TRUE;
}

VOID NEAR PASCAL GoNextHotLink(BOOL bOnlyInView)
{
    char temp[MAXHELPLINE+2];
    int i, maxLine, minLine;
    int hotstart, hotend, hotcontextstart, hotcontextend;

    if(bOnlyInView) {
	minLine = gHotLinkLine + 1;
	if(minLine < HelpList.numlinesscrolled)
	    minLine = HelpList.numlinesscrolled;

	maxLine = HelpList.numlinesscrolled + HelpList.numlinesonscreen;
	if(maxLine > gNumHelpLines)
	    maxLine = gNumHelpLines;
    } else {
	minLine = 0;
	maxLine = gNumHelpLines;
    }

    for(i=minLine; i<maxLine; ++i) {
	Get_Ith_Help_Line(temp, i);
	if(FindHotLinks(temp, &hotstart, &hotend,
		&hotcontextstart, &hotcontextend)) {
	    gHotLinkLine = i;
	    return;
	}
    }

    gHotLinkLine = -1;
}

VOID NEAR PASCAL GoPrevHotLink(void)
{
    char temp[MAXHELPLINE+2];
    int i, maxLine, minLine;
    int hotstart, hotend, hotcontextstart, hotcontextend;

    minLine = HelpList.numlinesscrolled;
    if(gHotLinkLine < 0)
	gHotLinkLine = HelpList.numlinesscrolled + HelpList.numlinesonscreen;

    maxLine = HelpList.numlinesscrolled + HelpList.numlinesonscreen;
    if(maxLine > gHotLinkLine - 1)
	maxLine = gHotLinkLine - 1;

    for(i=maxLine; i>=minLine; --i) {
	Get_Ith_Help_Line(temp, i);
	if(FindHotLinks(temp, &hotstart, &hotend,
		&hotcontextstart, &hotcontextend)) {
	    gHotLinkLine = i;
	    return;
	}
    }

    gHotLinkLine = -1;
}

LONG FAR PASCAL Pfnhelplist(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
    UnReferenced(pwnd);
    UnReferenced(wParam);

    switch(message) {
    case WM_CHAR:
	switch(wParam) {
	case(ESCAPE):
	    return(FALSE);
	    break;

	case('\t'):
	    if(HIWORD(LParam)&KK_SHIFT)
		GoPrevHotLink();
	    else
		GoNextHotLink(TRUE);

	    if(gHotLinkLine == -1)
		return(FALSE); //no hotlinks, so tab back to buttons
	    FocusLineChange(&HelpList, gHotLinkLine-HelpList.focusitem);
	    break;

	case(VK_UP):
	    DoScrollListBox(&HelpList, -1, TRUE);
	    break;

	case(VK_DOWN):
	    DoScrollListBox(&HelpList, 1, TRUE);
	    break;

	case(VK_PRIOR):
	    DoScrollListBox(&HelpList, 1-HelpList.numlinesonscreen, TRUE);
	    break;

	case(VK_NEXT):
	    DoScrollListBox(&HelpList, HelpList.numlinesonscreen-1, TRUE);
	    break;

	default:
	    ListKey(&HelpList, wParam, 0);
	    EnableCursor(pwnd, FALSE);
	    break;
	}
	break;

    case WM_SETFOCUS:
	gHotLinkLine = -1;
	if(gCurrentTMC == tmcCancel)
	    GoPrevHotLink();
	else
	    GoNextHotLink(TRUE);
	if(gHotLinkLine != -1)
	    FocusLineChange(&HelpList, gHotLinkLine-HelpList.focusitem);

	GlobalFocusBox(&HelpList, TRUE);
	EnableCursor(pwnd, FALSE);
	break;

    case WM_KILLFOCUS:
	GlobalFocusBox(&HelpList, FALSE);
	break;
    }

    return(TRUE);
}

extern BOOL gDialogIgnoreNextPaint;
BOOL FAR PASCAL FDlghelp(WORD dlm, TMC tmc, WORD wNew, WORD wOld, WORD wParam)
{
    PWND dwind;
    PWND lwind;
    static BOOL fListBoxInited;

    dwind = PwndParent(PwndOfListbox(tmcCancel));
    switch(dlm) {
    case dlmInit:
/* We get the context before the HelpBox'listbox button gets focus */
/* But we can't Get_Help until after the list box is OK */
	Get_Current_Context();

	fListBoxInited = FALSE ;

	SetUpDialog(tmcCancel, szHelpBoxTitle);
	SetUpButtonForGraphics(tmcCancel);
	SetUpButtonForGraphics(tmchelpBB);
	SetUpButtonForGraphics(tmchelpIB);
	SetUpButtonForGraphics(tmchelpKB);
	SetUpButtonForGraphics(tmchelpHB);

	lwind = PwndOfListbox(tmchelplist);
	SetWindowProc(lwind, Pfnhelplist);

	ListBoxInit(&HelpList, ListProcHelpList, lwind, 1, 0,
		lwind->arcWindow.ayBottom - lwind->arcWindow.ayTop - 1,
		lwind->arcWindow.axRight  - lwind->arcWindow.axLeft,
		szTitle, tmchelplist, 0, 0);
	Halt_Listbox(&HelpList);
       
	GlobalFocusBox(&HelpList, TRUE);
	Set_List_Color(&HelpList, isaDialogBox);
	break;

    case dlmIdle:
	if(!fListBoxInited) {
	    fListBoxInited = TRUE;
	    UnHalt_Listbox(&HelpList);

	    if(!Get_Help(FALSE))
		EndDlgTmc(tmcCancel);
	    SetFocusTmc(tmchelplist);
	}
	ListBoxIdle(&HelpList);
	break;

    case dlmTerm:
	gDialogIgnoreNextPaint = TRUE;
	break;

    case dlmKey:
	switch(wNew) {
	case(VK_UP):
	case(VK_DOWN):
	case(VK_PRIOR):
	case(VK_NEXT):
	case(VK_HOME):
	case(VK_END):
	    EnableCursor(dwind,FALSE);
	    SendMessage(PwndOfListbox(tmchelplist), WM_CHAR, wNew, 0L);
	    SetFocusTmc(tmchelplist);
	    break;
	}
	break;

    case dlmClientMouse:
	if(ListMouse(&HelpList, LOBYTE(wParam)-1, HIBYTE(wParam)-2, wNew, wOld))
		  {
			   if(!HelpList.hasglobalfocus)
				{
					SetFocusTmc(tmchelplist);
		ListMouse(&HelpList, LOBYTE(wParam)-1, HIBYTE(wParam)-2, wNew, wOld);
				}

		  }
	break;

    case dlmClick:
	switch(tmc) {
	case(tmchelpBB):
	    if(gNumHelps == 0) {
		gHelpErr = TRUE;
		ShellMessageBox(szHelpErrTitle, szHelpNoBack);
		gHelpErr = FALSE;
		break;
	    }
	    --gNumHelps;
	    gCurHelp -= sizeof(HelpState);
	    gHelpChunk += sizeof(HelpState);

	    Get_Help(TRUE);
	    DoScrollListBox(&HelpList, gHelpState[gNumHelps].nHelpScroll -
		    HelpList.numlinesscrolled, FALSE);
	    InsertListItem(&HelpList, 0);
	    break;

	case(tmchelpIB):
	    IndexBox();
	    break;

	case(tmchelpKB):
	    KeyBox();
	    break;

	case(tmchelpHB):
	    gSpecialMode = HELPONHELPMODE;
	    HelpBox();
	    break;
	}
    }

    return(TRUE);
}

/****************************************************************/
/*************************** PUBLIC FUNCTIONS *******************/
/****************************************************************/


VOID FAR HelpBox(void)
{
/* NUMSLICES is the number of allocations we use to save the graphics
 * behind the help dialog box. We need to do this since CW cannot
 * handle >64k. Also, we need smaller chunks so the allocations won't
 * fail when memory is fragmented.
 */
#define NUMSLICES 20
    HCABhelp h;
    BYTE FAR *graphicssave[NUMSLICES];
    WORD cbSave, gSaveTMC;
    WORD top, left, bottom, right, middle, cheight;
    int i;
    char *szSaveDialogCaption;

    if(gHelpErr || gNumHelps>=REENTERHELP-1) {
	Shell_Beep();
	return;
    }

    if(gNumHelps >= 0) {
	gHelpState[gNumHelps].nHelpScroll = HelpList.numlinesscrolled;
	++gNumHelps;
	gCurHelp += sizeof(HelpState);
	gHelpChunk -= sizeof(HelpState);

	DoScrollListBox(&HelpList, -HelpList.numlinesscrolled, FALSE);
	Get_Current_Context();
	if(Get_Help(FALSE)) {
	    InsertListItem(&HelpList, 0);
	} else {
	    --gNumHelps;
	    gCurHelp -= sizeof(HelpState);
	    gHelpChunk += sizeof(HelpState);
	    Get_Help(TRUE);
	}
    } else {
	gNumHelps = 0;
	gHelpChunk = MAXHELPCHUNK;

	Free_Help();
	if(!(gCurHelp=(char far *)LpbAllocWorkFar(gHelpChunk+EXTRABUFFER))) {
	    OutOfMemory();
	    goto Error1;
	}
	gHelpLines = (WORD far *)(gCurHelp+MAXHELPCHUNK-sizeof(WORD));
	gHelpState = (HelpState far *)gCurHelp;
	gCurHelp += sizeof(HelpState);
	gHelpChunk -= sizeof(HelpState);

	if(!(h=HcabAlloc(cabiCABhelp))) {
	    OutOfMemory();
	    goto Error2;
	}

	InitCab(h, cabiCABhelp);

	SzToCab(h, szClose, Iag(CABhelp, pszhelpEB));
	SzToCab(h, szBack, Iag(CABhelp, pszhelpBB));
	SzToCab(h, szIndex, Iag(CABhelp, pszhelpIB));
	SzToCab(h, szKeys, Iag(CABhelp, pszhelpKB));
	SzToCab(h, szHelpButton, Iag(CABhelp, pszhelpHB));

#if 1
	if(gisgraph) {
/* save behind the whole dialog in small chunks */
	    left = ((PDLG)&dlghelp)->crcDlg.x;
	    right = ((PDLG)&dlghelp)->crcDlg.x + ((PDLG)&dlghelp)->crcDlg.dx+2;
	    top = ((PDLG)&dlghelp)->crcDlg.y;
	    bottom = ((PDLG)&dlghelp)->crcDlg.y + ((PDLG)&dlghelp)->crcDlg.dy+2;
	    cheight = (bottom - top + NUMSLICES - 1)/NUMSLICES;

	    middle = top + cheight;
	    for(i=0; i<NUMSLICES && top<bottom; ++i) {
		cbSave = (right-left) * (middle-top);
		graphicssave[i] = MyLpbSaveGraphicArc(LpbAllocWorkFar, cbSave,
			(AX)left, (AY)top, (AX)right, (AY)middle);
		top = middle;
		middle += cheight;
	    }
	}
#endif
	FEnableMouse(FALSE) ;

	szSaveDialogCaption = szCurDialogCaption;
	gSaveTMC = gCurrentTMC;
	TmcDoDlg(&dlghelp, h);
	gCurrentTMC = gSaveTMC;
	szCurDialogCaption = szSaveDialogCaption;

#if 1
	if(gisgraph) {
	    top = ((PDLG)&dlghelp)->crcDlg.y;

	    middle = top + cheight;
	    for(i=0; i<NUMSLICES && top<bottom; ++i) {
		if(graphicssave[i])
		    MyRestoreGraphicArc(FreeWorkFar, (AX)left, (AY)top,
			    (AX)right, (AY)middle, graphicssave[i]);
		top = middle;
		middle += cheight;
	    }
	}
#endif
#if 0
			InitIconCache();
			if(glob.InFileMgr)
				RefreshFMScreen(FALSE);
			else
				 RefreshStartPrograms();

#endif
#if 1
		  /* Following is code to re-draw dialogs behing help dialog boxes */
		  /* Invariably we run out of memory in this case to save graphics */

			   gDialogIgnoreNextPaint = FALSE;
		  if (gCWContext == hemDialog)
		  {
				// DrawWindow(NULL) ;
				fDrawItem = TRUE;
				fRedrawItem = FALSE;

				DrawWindow(PwndParent(PwndOfTmc(tmcCancel))) ;
		  }


#endif

	FreeCab(h);
Error2:
	Free_Help();
Error1:
	gNumHelps = -1;
	gSpecialMode = REGULARHELP;
    }
}

VOID FAR IndexBox(void)
{
    gSpecialMode = INDEXMODE;
    HelpBox();
}

VOID FAR KeyBox(void)
{
    gSpecialMode = KEYMODE;
    HelpBox();
}

VOID FAR BasicsBox(void)
{
    gSpecialMode = BASICSMODE;
    HelpBox();
}

VOID FAR CommandsBox(void)
{
    gSpecialMode = COMMANDSMODE;
    HelpBox();
}

VOID FAR ProceduresBox(void)
{
    gSpecialMode = PROCEDURESMODE;
    HelpBox();
}

VOID FAR UsingHelpBox(void)
{
    gSpecialMode = USINGMODE;
    HelpBox();
}

BOOL gfDisplayDelayedHelp = FALSE ;

void ClearHelpContext(void)
{
	gCWContext = 0;
	gCWhid = 0;
	gCWpv = NULL;
}

/*
 * this is CW's entry point to help
 */
VOID FAR PASCAL Help(WORD hem, WORD hid,VOID *pv,WORD kk)
{
/* The #if 0 code below was needed before because we could have the Help
 * dialog come up even when the menu was popped down. I changed this. We
 * no longer have the Help dialog up with the menu pulled down. This was
 * done to circumvent a known CW bug regarding Menu strings being blanked
 * when in a low memory situation. The following behaviour is present
 * in WORD 5.5 too according to antoine.
 */
#if 0
	char chMyTopSide1;
#endif

	MSG fake_esc_msg ;

	UnReferenced(kk) ;

	gCWpv = pv;
	gCWContext = hem;
	gCWhid = hid;

	if ((hem == hemMenu) || (hem == hemMenuItem))
	{
		gfDisplayDelayedHelp = TRUE ;

		/* Add a fake message to the queue to cause the menu to be closed. This
		 * is a WM_CHAR message to the (fake?) menu window with ESC key(wParam).
		 * Low word of lParam is the char repeat count (1 in this case).
		 */
		fake_esc_msg.pwnd = GetFocus() ;
		fake_esc_msg.message = WM_CHAR ;
		fake_esc_msg.wParam = LOBYTE(VK_ESCAPE) ;
		fake_esc_msg.lParam = (DWORD) ((((WORD)VK_ESCAPE) << 16)) + 1 ;
		// fake_esc_msg.time = 0 ;      // DOC says that this value is not reliable!

		UngetMessage(&fake_esc_msg) ;

	}
	else
	{
#if 0 
		chMyTopSide1 = inch._chTopSide1;
		inch._chTopSide1 = ' ';
#endif

		HelpBox();
		ClearHelpContext() ;

#if 0
		inch._chTopSide1 = chMyTopSide1;
#endif
	}

}


#ifdef DBCS
int     FarCheckDBCSTailByte(str,point)
unsigned char far *str;
unsigned char far *point;
{
	unsigned char far *p;

	p = point;
	while (p != str)
	{
		p--;
		if (!IsDBCSLeadByte(*p))
		{
			p++;
			break;
		}
	}
	return ((point - p) & 1 ? TRUE : FALSE);
}
#endif

