;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <filemgr.h>
#include <text.h>
#include <menus.h>
#include <tasklist.h>

// Lets you compile in debug mode without running out of near heap
//  #define DEBUG  
#ifndef DEBUG
#include <prot.h>
#else
extern  VOID FARPUBLIC Shell_SetTmcText(TMC tmc, char *text);
extern  void SetUpDialog(unsigned short tmcchild,char *title);
extern  void SetUpButtonForGraphics(unsigned short tmc);
extern void SetUpCheckBox(TMC tmc) ;
extern  void SetUpEditBox(unsigned short tmc, BOOL fRealOrNot, WORD maxcount, BOOL fHasInitialFocus);
extern  void SetUpRadioGroupForGraphics(unsigned short group, TMC prevtmc, TMC nexttmc);
extern  void SetUpRadiobuttonForGraphics(unsigned short tmc,int index);
VOID FAR PASCAL Help(WORD hem, WORD hid,VOID *pv,WORD kk);
extern  void pascal far OutOfMemory(void );
extern TMC      MyTmcDoDlg(VOID *pdlg, HCAB hcab) ;
extern TOKEN Delete_Ith_Element(TOKEN list,int ith);
extern unsigned char far cdecl GET_WAIT_FLAG(void);
extern  void SetTitle(struct ListBoxData *TestList,char *title);
extern  void InitIconCache(void );
extern  void FocusLineChange(struct ListBoxData *TestList,int amt);
extern  void InsertListItem(struct ListBoxData *TestList,unsigned short isz);
extern  void DoRedisplayList(struct ListBoxData *TestList);
extern  int Tree2Path(PTREE tree,PENTRY node,char *str, int *plength);
extern  void strfcpy(char far *dst,char far *src);
extern  unsigned int myfstrlen(unsigned char far *s);
extern  void PlotBmp(struct BITMAP *bmp,unsigned short x,unsigned short y,ISA color);
extern void DrawFocusMarker(PWND pwnd, RX rxText, RY ryText, RX rxGraphics,
				RY ryGraphics, WORD len, BOOL isfocus, BOOL issel, ISA isa) ;
extern  void far StartAProgram(void);
extern VOID ShellMessageBox(char *messagetitle, char *message) ;
extern  void far AddProgram(void);
extern  void far AddGroup(void);
extern  void far ChangeGroup(void);
extern  void far ChangeProgram(void);
extern  void MessageBar(char *message, ISA isa,WORD force);
extern  VOID ListBoxInit(ListBoxData *TestList,WORD (*ListProc)(),
	PWND pwd,WORD top,WORD left,WORD bottom,WORD right,char *title, 
	WORD tmc,WORD startfocusabsolute,WORD startfocusrelative);
extern  void UpdateMainTitleBar(char *szTitle);
extern  void setmenubar(struct _mnu * * *amenu,struct _wnd *towind);
extern  void FrameMenuBar(struct _wnd *pwnd);
extern  void NextGlobalFocus(void );
extern  void GlobalFocusBox(struct ListBoxData *TestList,int yesorno);
extern  int ListBoxIdle(struct ListBoxData *TestList);
extern  void UpdateListBox(struct ListBoxData *TestList);
extern  void ListKey(struct ListBoxData *TestList,unsigned short key, unsigned short state);
extern  int ListMouse(struct ListBoxData *TestList,unsigned short x,unsigned short y,unsigned short message,int state);
extern  void DoSingleTree(void );
extern  void DoDoubleTree(void );
extern  void DoFlatDisplay(void );
extern  void DoShareMode(void );

#endif
#include <groups.h>
#include <icons.h>


#include <addgroup.hs>
#include <addgroup.sdm>
#include <addprog.hs>
#include <addprog.sdm>
#include <adddialo.hs>
#include <adddialo.sdm>
#include <advanced.hs>
#include <advanced.sdm>
#include <password.hs>
#include <password.sdm>
#include <new.hs>
#include <new.sdm>

extern void DrawDialogBorderAndTitle(PWND pwnd) ;
extern BOOL Buffered_Write_Ini_File(BOOL bFreeFM);

extern BOOL gTaskListEnabled;
extern BOOL gSwitchingEnabled;

extern BOOL gfFMVisited ;

extern BOOL fDrawItem;
extern BOOL fRedrawItem;
extern BOOL gfRepaintonidle;
extern char *szCurDialogCaption ;

WORD PASCAL ListProcTaskList(WORD tmm, char *sz, WORD isz, TMC tmc,WORD x, WORD y, WORD bArg);
#ifndef NOSWITCHER
BOOL TaskManKey(WORD wParam,DWORD LParam);
BOOL TaskManIdle(void);
BOOL TaskManMouse(WORD mx,WORD my,WORD message,BYTE state);
VOID InitTaskMan(WORD top,WORD left,WORD bottom,WORD right);
extern void Set_Task_Name(char far *name);
extern VOID Set_Task_Info(TOKEN);
extern TOKEN Get_Identifier_Token(char far *identifier);
extern ListBoxData TaskList;
extern BOOL SetupCommand(char far *commands, char *startdir,BOOL dolaunch,TOKEN info);
#endif

extern WORD GetIthGroupOrProgramIndex(TOKEN group, int isz);
extern TOKEN Duplicate_Token(TOKEN src);
extern void Insert_Symbol(TOKEN, TOKEN, int);

extern char *gpszNonSwap ;

extern char gStartUpDir[] ;
extern int gStartUpDirEnd; /* location where the NULL goes in the above name */
extern char gStartInDir[] ;

extern char far *gIconCache;
#ifndef NODIRECT
VOID Arrow(void);
VOID ProgramCursor(void);
VOID FAR Advanced(void);
#endif
VOID DoUserCommand(char *startdir);
VOID FAR DoCopyProgram(void);
VOID DoReorderGroup(void);

#ifdef USA
   #define MAXUSERTITLE 23
#else
   #define MAXUSERTITLE 27 // IPG- increased prog item lengths 23 to 27 for intl vers.
#endif
#define MAXUSERINSTRUCTIONS 105
#define MAXUSERPROMPT 18
#define MAXUSERDEFAULT 64
#define MAXUSERCOMMAND 256
#define MAXUSERPASSWORD 20
#define MAXUSERHELP 255
extern MENUINFO MainMenuBar;
extern char userTitle[MAXUSERTITLE];
extern char userInstructions[MAXUSERINSTRUCTIONS];
extern char userPrompt[MAXUSERCOMMAND];
extern char userDefault[MAXUSERDEFAULT];
extern char userCommand[MAXUSERCOMMAND];
extern char userPassword[MAXUSERPASSWORD];

BOOL gPauseChecked; /* pause on exit on/off? */
WORD gInitPMFocus = 0;
char currentPassword[MAXUSERPASSWORD];

#define IsGroupOrProgramType(tkTemp) ((tkTemp)==TK_GROUP||(tkTemp)==TK_PROGRAM)

/* Note: PROGLINELEN must be at least 2 more than MAXUSERTITLE */
#ifndef NOSWITCHER
   #ifdef USA
      #define PROGLINELEN 25
   #else
      #define PROGLINELEN 29 //IPG- for international versions
   #endif
#else
#define PROGLINELEN 40
#endif

struct ListBoxData ProgramList;
char ProgramTitle[MAXUSERTITLE+1];
TOKEN gGroupLevel = TK_PROGRAMSTARTER;
BOOL ischangedialog;
BOOL gInCopy = FALSE;
BOOL gInReorder = FALSE;
TOKEN gCopyProgram; /* used for reorder group as well! */
TOKEN gReorderType;
TOKEN gReorderItem;

TOKEN token,commandtoken,filemgrtoken;

int FarToNearsz(char *temp, char far *str, int max)
{
	register int i = 0;

	if(str) {
		for(--max; i<max; ++i) {
			if(str[i])
				temp[i] = str[i];
			else
				break;
		}
	}
	temp[i] = '\0';

	return(i);
}


char RotChar(char c)
{
    int ret;
    ret = c+13;
    ret = 'A' + ret%26;
    return((char)ret);
}

/*
 * warning; tmp and a can be the same location!
 */
VOID MakeEncryptPass(char *tmp,char *a)
{
    int i;
    for(i=0;i<MAXUSERPASSWORD;i++)
    {
	if(a[i] == 0)
	    break;
	else
	   tmp[i] = RotChar(a[i]);
    }
    tmp[i] = 0;
}
/*
 * a is the attempted password string, b is the encrypted on from .ini
 */
BOOL PasswordMatch(char *a,char *b)
{
    int i;
    char tmppass[MAXUSERPASSWORD];

    MakeEncryptPass(tmppass,a);

    for(i=0;i<MAXUSERPASSWORD;i++)
    {
	if ((tmppass[i] ==0) && (b[i] == 0))
	    return(TRUE);
	if (tmppass[i] != b[i])
	    return(FALSE);
    }
    return(TRUE);
}

DWORD FAR PASCAL Pfnpasswordbox(PWND, WORD, WORD, DWORD);

BOOL FAR PASCAL
FDlgpassword(dlm, tmc, wNew, wOld, wParam)
WORD    dlm;
TMC     tmc;
WORD    wNew, wOld, wParam;
{
    char tstr[MAXUSERPASSWORD];

	UnReferenced(tmc);
	UnReferenced(wNew);
	UnReferenced(wOld);
	UnReferenced(wParam);

	if(dlm == dlmInit)
	{
		 SetUpDialog(tmcOK,szPasswordCaption);
		 SetUpButtonForGraphics(tmcOK);
		 SetUpButtonForGraphics(tmcpasswordHB);
		 SetUpButtonForGraphics(tmcCancel);

		 SetUpEditBox(tmcpassword, TRUE,MAXUSERPASSWORD, TRUE);
		 SetWindowProc(PwndOfListbox(tmcpassword), Pfnpasswordbox);
	} else if(dlm == dlmTerm) {
	    GetTmcText(tmcpassword, tstr, MAXUSERPASSWORD);
	    currentPassword[strlen(tstr)] = '\0';
	} else if(dlm == dlmSetFocus)
	gCurrentTMC = tmc;
	else if(dlm == dlmClick) {
		 if(tmc == tmcpasswordHB)
			Help(hemDialog, hidPASSWORD, NULL, 0);

		 SetFocusTmc(gCurrentTMC) ;
    }
	return(TRUE);
}

BOOL PasswordBox(void)
{
    BOOL retval;
    HCABpassword        h;

    retval = FALSE;
    h = HcabAlloc(cabiCABpassword);
    if (!h)
    {
       OutOfMemory() ;
       return (FALSE);
    }
    InitCab(h, cabiCABpassword) ;

    SzToCab(h, NullString, Iag(CABpassword, pszpassword));

    SzToCab(h, szEnterButton, Iag(CABpassword, pszpasswordEB));
    SzToCab(h, szCancelButton, Iag(CABpassword, pszpasswordCB));
    SzToCab(h, szHelpButton, Iag(CABpassword, pszpasswordHB));

    if (MyTmcDoDlg(&dlgpassword,        h) == tmcOK)
    {
	if(PasswordMatch(currentPassword,userPassword))
	{
	    retval = TRUE;
	}
	else
	 Shell_Beep();
    }
    else
	 retval = FALSE;
    FreeCab(h);
    return(retval);
}
int Num_Items_In_Group(TOKEN token)
{
    int i;
    int numitems;
    int length;
    TOKEN token2;

    length = Get_List_Length(token);
    numitems = 0;
    for(i=0;i<length;i++)
    {
	 token2 = Get_Ith_Element(token,i+1);
	 token2 = Get_Symbol_Type(Token_To_Symbol(token2));
	 if((token2 == TK_GROUP) || (token2 == TK_PROGRAM))
	 {
	     ++numitems;
	 }
    }
    return(numitems);
}

void PASCAL VariableToString(TOKEN tkList, TOKEN tkItem, char *szItem, int nMax)
{
	if((tkItem=Get_KeyWord_Assignment(tkList, tkItem)) > 0)
		FarToNearsz(szItem, Get_Token_Identifier(tkItem), nMax);
	else
		*szItem = '\0';
}

BOOL PASCAL VariableToCab(TOKEN section, TOKEN variable,
	HCAB h, WORD iag)
{
    char tstr[256];
    BOOL retval;

    retval = TRUE;
    VariableToString(section, variable, tstr, sizeof(tstr));
    if(variable == TK_PASSWORD && *tstr) {
	currentPassword[0] = 0;
	strcpy(userPassword, tstr);
	if(!PasswordBox())
	{
	    retval = FALSE;
	    tstr[0] = 0;
	}
	else
	{
		strcpy(tstr, currentPassword);
	}
    }
    SzToCab(h, tstr, iag);
    return(retval);
}

void PASCAL CabToVariable(HCAB h, WORD iag,
	TOKEN section, TOKEN variable)
{
    TOKEN tkTemp;
    char tstr[256];

    SzFromCab(h, tstr, sizeof(tstr), iag);
    if(*tstr) {
	if(variable == TK_PASSWORD)
	    MakeEncryptPass(tstr, tstr);
	if((tkTemp=Get_Identifier_Token(tstr)) <= 0)
	    return;
    } else
	tkTemp = TK_UNKNOWN;
    Set_KeyWord_Assignment(section, variable, tkTemp);
}


/*
 * Get a token to the group's parent.
 * root is the highest level to look at. This is used so
 * we can be recursive. Regular call should use TK_PROGRAMSTARTER
 * as the root. NOTE: group does not have to be a group, a program
 * will also work!
 */
TOKEN GroupParent(TOKEN root, TOKEN group)
{
    TOKEN tkTemp1, tkTemp2, tkTemp3;
    int i, index = 0;
    TOKEN retval;
    int length;

    if(root<=0 || group<=0)
	return(TK_NOTHING);

    if(root == TK_PROGRAMSTARTER)
	tkTemp3 = Get_KeyWord_Assignment(root, TK_GROUP);
    else
	tkTemp3 = Get_Symbol_Value(Token_To_Symbol(root));

    if(tkTemp3 <= 0)
	return(TK_NOTHING);

    length = Get_List_Length(tkTemp3);
    for(i=0; i<length; ++i) {
	tkTemp1 = Get_Ith_Element(tkTemp3, i+1);
	if(Get_Symbol_Value(Token_To_Symbol(tkTemp1))==group ||
		tkTemp1==group) /* we have found our group! */ {
	    gInitPMFocus = index;
	    return(root);
	}

	tkTemp2 = Get_Symbol_Type(Token_To_Symbol(tkTemp1));
	if(IsGroupOrProgramType(tkTemp2)) {
	    ++index;

	    if(tkTemp2 == TK_GROUP) {
/* recurse into this group */
		if((retval=GroupParent(tkTemp1, group)) > 0)
		    return(retval); /* we found the guy in here */
	    }
	}
    }

/*
 * we never found the guy under this root, so his parent is
 * not under this root
 */
    return(TK_NOTHING);
}

//bug bug this is necessary because the damn thing is recursive
BOOL gLoadEqualsProcessed = FALSE;

void Get_StartupItem(TOKEN tkStartGroup)
{
    TOKEN tkItem, tkType, tkValue;
    TOKEN tkGroup;
    int i, length, index = -1;

    if(tkStartGroup == TK_PROGRAMSTARTER)
	tkGroup = Get_KeyWord_Assignment(tkStartGroup, TK_GROUP);
    else
	tkGroup = tkStartGroup;
    if(tkGroup <= 0)
	return;

    length = Get_List_Length(tkGroup);
    for(i=1; i<=length; ++i) {
	tkItem = Get_Ith_Element(tkGroup, i);
	tkType = Get_Symbol_Type(Token_To_Symbol(tkItem));
	tkValue = Get_Symbol_Value(Token_To_Symbol(tkItem));

	if(IsGroupOrProgramType(tkType))
	    ++index;

	if(tkType == TK_GROUP) {
/* recurse into this group */
	    Get_StartupItem(tkValue);

	    if(gLoadEqualsProcessed && gGroupLevel!=TK_NOTHING)
		return;
	} else if(tkType == TK_SPECIAL) {
	    if(tkValue == TK_DEFAULT) {
		if(gGroupLevel == TK_NOTHING) {
		    gGroupLevel = tkStartGroup;
		    gInitPMFocus = index;
		    if(gLoadEqualsProcessed)
			return;
		} else {
		    Delete_Ith_Element(tkGroup, i);
		    --i;
		    --length;
		}
	    }
	} else if(tkType==TK_PROGRAM &&
		!gLoadEqualsProcessed && !GET_WAIT_FLAG()) {
	    if(Get_KeyWord_Assignment(tkValue, TK_LOAD) == TK_ENABLED) {
		VariableToString(tkValue,TK_COMMAND,userCommand,MAXUSERCOMMAND);
		Set_Task_Info(tkValue);
		SetupCommand(userCommand, NULL, FALSE, tkValue);
	    }
	}
    }

    return;
}

void Set_StartupItem(void)
{
    TOKEN tkSaveGroup, tkItem;

    tkSaveGroup = gGroupLevel;
    gGroupLevel = TK_NOTHING;
    Get_StartupItem(TK_PROGRAMSTARTER);
    if(gGroupLevel != TK_PROGRAMSTARTER)
	++gInitPMFocus;
    if(gGroupLevel==tkSaveGroup && gInitPMFocus==Get_List_Focus(&ProgramList)) {
	return;
    }

/* This will clear the old "special = default" line. */
    if(gGroupLevel == TK_PROGRAMSTARTER)
	gGroupLevel = Get_KeyWord_Assignment(gGroupLevel, TK_GROUP);
    Set_KeyWord_Assignment(gGroupLevel, TK_SPECIAL, TK_UNKNOWN);

    gInitPMFocus = Get_List_Focus(&ProgramList);
    if((gGroupLevel=tkSaveGroup) == TK_PROGRAMSTARTER)
	tkSaveGroup = Get_KeyWord_Assignment(gGroupLevel, TK_GROUP);
    else
	--gInitPMFocus;

    if((tkItem=SubClassSymbol(TK_SPECIAL)) > 0) {
	Set_Symbol_Value(Token_To_Symbol(tkItem), TK_DEFAULT);
	Insert_Symbol(tkSaveGroup, tkItem,
		GetIthGroupOrProgramIndex(tkSaveGroup, gInitPMFocus));
    }
}

TOKEN Get_Progman_Item(int i)
{
    int numitems;
    TOKEN token;
    BOOL doorpresent;

    if(gGroupLevel == TK_PROGRAMSTARTER) {
	token = Get_KeyWord_Assignment(gGroupLevel, TK_GROUP);
	if(token < 0) /* there is no group at all */ {
/* we need to create a group! */
	    if((token=SubClassSymbol(TK_GROUP)) <= 0)
		return(TK_NOTHING);
	    Set_KeyWord_Assignment(gGroupLevel, TK_GROUP, token);
	}
	doorpresent = FALSE;
    } else {
	token = gGroupLevel;
	doorpresent = TRUE;
    }

    if(!(numitems=Num_Items_In_Group(token)+doorpresent) || i>=numitems)
	return(-1);

    i -= doorpresent;
    if(i < 0) /* it's the door */ {
	token = GroupParent(TK_PROGRAMSTARTER, gGroupLevel);
	if(token <= 0)
	    token = TK_PROGRAMSTARTER;
    } else {
	i = GetIthGroupOrProgramIndex(token, i);
	token = Get_Ith_Element(token, i);
    }

    return(token);
}

TOKEN Get_Focus_Item(void)
{
    return(Get_Progman_Item(Get_List_Focus(&ProgramList)));
}


#define MAXGROUPTITLELEN 76
VOID SetGroupTitle(TOKEN token)
{
    char *str;

    if(token == TK_PROGRAMSTARTER) {
	str = szMainGroup;
    } else {
	VariableToString(token, TK_TITLE, ProgramTitle, sizeof(ProgramTitle));
	if(ProgramTitle[0])
	    str = ProgramTitle;
	else
	    str = szNoItemTitle;
    }
    SetTitle(&ProgramList, str);
}

void UpLevel(void)
{
    if(gGroupLevel == TK_PROGRAMSTARTER) {
	Shell_Beep();
	return;
    }

    gInitPMFocus = 0; /* default value */
    if((gGroupLevel=GroupParent(TK_PROGRAMSTARTER, gGroupLevel)) <= 0) {
	Shell_Beep();
	gGroupLevel = TK_PROGRAMSTARTER;
    }

    if(gGroupLevel != TK_PROGRAMSTARTER) {
	++gInitPMFocus;
	gGroupLevel = Get_Symbol_Value(Token_To_Symbol(gGroupLevel));
    }

    SetGroupTitle(gGroupLevel);
    InitIconCache();
    FocusLineChange(&ProgramList, gInitPMFocus - Get_List_Focus(&ProgramList));
    InsertListItem(&ProgramList, 0);
}

#if 0
int RemoveQuotes(char *temp, char far *str)
{
    register int i;

    if(*str == '"')
	*str++;

    for(i=0; i<MAXUSERTITLE && *str && *str!='"'; ++i)
	*temp++ = *str++;
    *temp = '\0';

    return(i);
}
#endif

int BuildOutString(char *temp, char far *str)
{
	register int i, saveI;

	for(i=saveI=str?FarToNearsz(temp, str, MAXUSERTITLE+1):0;
			i<PROGLINELEN+4; ++i)
		temp[i] = ' ';
	return(saveI);
}

WORD PASCAL ListProcProgramList(WORD tmm, char *sz, WORD isz, TMC tmc,
											WORD x, WORD y, WORD bArg)
{
    char far *str;
    char temp[80];
    TOKEN token,token2;
    BITMAP *pbitmap ;
    char icontype;
    int len;
    BOOL isgroup;
    char RunInDir[MAX_PATH+1] ;
    char *pStartDir ;
    int dummylen ;

    UnReferenced(tmc);

    switch(tmm) {
#ifndef NODIRECT
#if 0
//we dont do this . too bad
		case tmmPickUp:
		     ProgramCursor();
		     break;
		case tmmDrop:
		     Arrow();
		     break;
#endif
#endif
    case tmmCount:
	if(gGroupLevel == TK_PROGRAMSTARTER) {
	    if((token=Get_KeyWord_Assignment(gGroupLevel, TK_GROUP)) > 0)
		return(Num_Items_In_Group(token));
	    else
		return(0);
	} else
	    return(Num_Items_In_Group(gGroupLevel) + 1);
	break;

    case tmmSetFocus:
	break;

    case tmmActivate:
	if(gInReorder) {
	    DoReorderGroup();
	    break;
	}

	if(gGroupLevel!=TK_PROGRAMSTARTER && !isz) {
	    UpLevel();
	    break;
	}
	    
	if((token=Get_Progman_Item(isz)) < 0) {
	    Shell_Beep();
	    break;
	}

	if(token==TK_PROGRAMSTARTER)
	    goto DisplayGroup;
	else if(Get_Symbol_Type(Token_To_Symbol(token))==TK_GROUP) {
	    userPassword[0] = userPrompt[0] = 0;
/* get the password into userpassword */
	    if((token2=Get_KeyWord_Assignment(Get_Symbol_Value(
		    Token_To_Symbol(token)), TK_PASSWORD)) > 0) {
		FarToNearsz(userPassword, Get_Token_Identifier(token2),
			MAXUSERPASSWORD);
		if(*userPassword && !PasswordBox())
		    return(TRUE);
	    }

	    token = Get_Symbol_Value(Token_To_Symbol(token));
DisplayGroup:
	    gGroupLevel = token;
	    SetGroupTitle(gGroupLevel);
	    InitIconCache();
	    DoRedisplayList(&ProgramList);
	} else {
/* Run the program */
	    token = Get_Symbol_Value(Token_To_Symbol(token));
	    Set_Task_Info(token);

/* get the command string into usercommand */
	    token2 = Get_KeyWord_Assignment(token,TK_COMMAND);
	    if(token2 > 0)
		FarToNearsz(userCommand, Get_Token_Identifier(token2),
			MAXUSERCOMMAND);
	    else 
		Shell_Beep();

	    token2 = Get_KeyWord_Assignment(token, TK_PAUSE);
/* If token2 is invalid or not disabled, set global save
 * state to enabled. This is looked at when exiting a
 * launched program!
 */
	    if(token2 != TK_DISABLED)
		Set_KeyWord_Assignment(TK_SAVESTATE, TK_PAUSE, TK_ENABLED);
	    else
		Set_KeyWord_Assignment(TK_SAVESTATE, TK_PAUSE, TK_DISABLED);

	    userPassword[0] = 0;
	    userPrompt[0] = 0;
/* get the password into userpassword */
	    token2 = Get_KeyWord_Assignment(token, TK_PASSWORD);
	    if(token2 > 0)
		FarToNearsz(userPassword, Get_Token_Identifier(token2),
			MAXUSERPASSWORD);
	    if(token2>0 && userPassword[0] && !PasswordBox())
		return(TRUE);

/* Change directory to the startup dir specified by user */
	    token2 = Get_KeyWord_Assignment(token, TK_DIRECTORY);
	    if(token2>0) 
	    {
		FarToNearsz(temp, Get_Token_Identifier(token2), 64);
		if(temp[1] != ':') // user did not specify a drive letter
		{
			FarToNearsz(temp+2,Get_Token_Identifier(token2),64);
			if(gfFMVisited) /* if file manager has current drive */
			{
				/* get current selected drive letter */
				Tree2Path(listinfo[glob.FocusBox].tree,
			       listinfo[glob.FocusBox].files, RunInDir, &dummylen) ;
				temp[0] = RunInDir[0];
			}
			else
			{
				/* use startup directory */
				temp[0] = gStartInDir[0]; // copy drive letter
			}
			temp[1] = ':';
		}
		pStartDir = temp;
	    } else 
	    {
		pStartDir = gStartInDir ;
		if(gfFMVisited) 
		{
		    Tree2Path(listinfo[glob.FocusBox].tree,
								listinfo[glob.FocusBox].files, RunInDir, &dummylen) ;
		    pStartDir = RunInDir ;
		}
	    }

	    DoUserCommand(pStartDir);
	}
	break;

    case tmmGetItemString:
    case tmmDrawItem:
	if((token=Get_Progman_Item(isz)) < 0) {
	    BuildOutString(temp, NULL);
	    TextOut(&MainWind, (RX)x, (RY)y, temp, PROGLINELEN+4,isaBackground);
	    break;
	    break;
	}

	if(token == TK_PROGRAMSTARTER) {
	    isgroup = TRUE;
	    str = szMainGroup;
	} else {
	    isgroup = Get_Symbol_Type(Token_To_Symbol(token)) == TK_GROUP;
	    if((token=Get_Symbol_Value(Token_To_Symbol(token)))<=0)
		break;

	    if((token=Get_KeyWord_Assignment(token, TK_TITLE)) <= 0)
		str = szNoItemTitle;
	    else
		str = Get_Token_Identifier(token);
	}

	if(tmm == tmmGetItemString) {
	    FarToNearsz(sz, str, MAXUSERTITLE+1);
	    return(TRUE);
	}

	if(isgroup && !gisgraph) {
	    len = BuildOutString(temp+1, str) + 2;
	    temp[0]   = '[';
	    temp[len-1] = ']';
	} else
	    len = BuildOutString(temp, str);

	if(gisgraph) {
	    if(isgroup) {
		if(bArg & TF_ISFOCUS) {
		    pbitmap = GroupIconInvert;
		    icontype = 2;
		} else {
		    pbitmap = GroupIcon ;
		    icontype = 1;
		}
	    } else {
		if(bArg & TF_ISFOCUS) {
		    pbitmap = ProgManIconInvert ;
		    icontype = 3;
		} else {
		    pbitmap = ProgManIcon;
		    icontype = 4;
		}
	    }

	    if(!(gIconCache && (gIconCache[y*MAXCOLS+x] == icontype))) {
		PlotBmp(pbitmap, x*CWIDTH, y*CHEIGHT, isaBackground);
		if(gIconCache) {
		    gIconCache[y*MAXCOLS+x] = icontype;
		}
	    }
	}

	TextOut(&MainWind, (RX)x+4, (RY)y, temp, len, bArg&TF_ISFOCUS);
	TextOut(&MainWind, (RX)x+4+len, (RY)y, temp+len, PROGLINELEN-len,
		isaBackground);
	DrawFocusMarker(&MainWind, (RX)1+2*gisgraph, (RY)y, (RX)x+4, (RY)y,
		len, bArg&TF_ISFOCUS, FALSE, isaBackground) ;
	break;

    default:
	break;
    }
    return TRUE;
}

extern DWORD (FAR *Pfneditbox_chain)(PWND,WORD,WORD,DWORD);
extern WORD gEditBoxScanCode;
extern WORD gEditBoxKey;
extern WORD gEditBoxLastKey;
extern VOID SetUpSpecialEditBoxForGraphics(TMC tmc);
extern VOID HookISR9(void);
extern VOID UnHookISR9(void);
extern WORD GetLastScanCode(void);

WORD gEditBoxLastMod;
WORD gEditBoxModifier;
WORD gScanInfo=0;
#define ISEXTENDED 0x1000

VOID UpdateShiftKk(WORD kkNew,WORD kkOld)
{
	UnReferenced(kkOld) ;

	gEditBoxLastMod = gEditBoxModifier;
	gEditBoxModifier = kkNew;
}

extern DWORD FAR PASCAL Pfneditbox(PWND pwnd, WORD message, WORD wParam, DWORD LParam);
DWORD FAR PASCAL Pfnspecialeditbox(PWND pwnd, WORD message, WORD wParam, DWORD LParam)
{
	WORD thekey;
	char Temp_Str[30];
	char charstr[10];
	char *keystring;
	BOOL added;
	int i;
	
	switch(message)
	{
		case WM_CHAR:
		{
			switch(wParam)
			{
				case VK_LEFT:
				case VK_RIGHT:
				case VK_UP:
				case VK_DOWN:
				case VK_TAB:
				case KEY_TAB:
				case VK_ESCAPE:
				case ESCAPE:
				case VK_RETURN:
				case 13: //enter key
					goto passthrough;
				break;
			}
			Temp_Str[0] = 0;
		   added = FALSE;
			thekey = HIWORD(LParam)&KK_VK;
			if((wParam != VK_ALT) && 
				(gEditBoxModifier & (KK_ALT|KK_CONTROL|KK_SHIFT))||
				(wParam == VK_BACK) || (wParam == 8) || (wParam == 127))
			{
					if((wParam == VK_BACK) || (wParam == 8) ||
					   (((wParam == 127))&&(!(gEditBoxModifier&(KK_ALT|KK_CONTROL|KK_SHIFT)))) )
					{
						gScanInfo = 0;
						strcat(Temp_Str,szNone);
					}
					else
					{
						/* scan code used is only the top 3 nibbles */
						/* we use top nibble to determine if it was the extended keyboard */
						gScanInfo = GetLastScanCode() & 0xFFF;
						if(gEditBoxModifier & KK_ALT)
						{
							strcat(Temp_Str,szAlt);                         
							added = TRUE;
	
						}
						if(gEditBoxModifier & KK_CONTROL)
						{
							if(added)
								strcat(Temp_Str,szPlusSign);
							strcat(Temp_Str,szCtrl);                                
							added = TRUE;
						}
						if(gEditBoxModifier & KK_SHIFT)
						{
							if(added)
								strcat(Temp_Str,szPlusSign);
							strcat(Temp_Str,szShift);                               
						}
						switch(wParam)
						{
							case VK_F1:
								keystring = szF1;
								break;
							case VK_F2:
								keystring = szF2;
								break;
							case VK_F3:
								keystring = szF3;
								break;
							case VK_F4:
								keystring = szF4;
								break;
							case VK_F5:
								keystring = szF5;
								break;
							case VK_F6:
								keystring = szF6;
								break;
							case VK_F7:
								keystring = szF7;
								break;
							case VK_F8:
								keystring = szF8;
								break;
							case VK_F9:
								keystring = szF9;
								break;
							case VK_F10:
								keystring = szF10;
								break;
							case VK_F11:
								keystring = szF11;
								break;
							case VK_F12:
								keystring = szF12;
								break;
							case VK_INSERT:
								gScanInfo |= ISEXTENDED;
								keystring = szInsert;
								break;
							case VK_DELETE:
							case 127: //also delete
								gScanInfo |= ISEXTENDED;
								keystring = szDelete;
								break;
						   case VK_HOME:
								gScanInfo |= ISEXTENDED;
								keystring = szHome;
								break;
							case VK_END:
								gScanInfo |= ISEXTENDED;
								keystring = szEnd;
								break;
							case VK_PRIOR:
								gScanInfo |= ISEXTENDED;
								keystring = szPageUp;
								break;
							case VK_NEXT:
								gScanInfo |= ISEXTENDED;
								keystring = szPageDown;
								break;
							case ' ':
								keystring = szSpace;
								break;
							default:
								charstr[0] = (char) thekey&KK_VK;
								if(!((charstr[0] >= 'A') && (charstr[0] <= 'Z')))
									charstr[0] = (char) toupper(wParam);
								charstr[1] = 0;
								keystring = charstr;
						}
						strcat(Temp_Str,szPlusSign);
						strcat(Temp_Str,keystring);
					}
					/* bug bug this is dumb */
					for(i=strlen(Temp_Str);i<24;i++)
					{
					strcat(Temp_Str," ");
					}
					/* WARNING do not use
					 * Shell_SetTmcText here or
					 * the text will not display!
					 */
					SetTmcText(tmcshortcut,Temp_Str);
				}
			}
		   return(TRUE);
		}
passthrough:
	return Pfneditbox(pwnd, message, wParam, LParam) ;
} /* Pfnfakeeditbox */

VOID SetUpSpecialEditBoxForGraphics(TMC tmc)
{
     PWND dwind;

     dwind = PwndOfListbox(tmc);
     Pfneditbox_chain = GetWindowProc(dwind);
	  dwind->id = tmc;

	  SetWindowProc(dwind, Pfnspecialeditbox) ;
}


BOOL FAR PASCAL FDlgaddprogram(dlm, tmc, wNew, wOld, wParam)
WORD    dlm;
TMC     tmc;
WORD    wNew, wOld, wParam;
{
	char Temp_Str[max(MAXUSERCOMMAND,MAXUSERTITLE)+1] ;

	UnReferenced(tmc);
	UnReferenced(wNew);
	UnReferenced(wOld);
	UnReferenced(wParam);

	switch(dlm)
	{
		case dlmInit:
		{
			SetTmcVal(tmcpaddprompt, gPauseChecked);

			SetUpDialog(tmcOK,ischangedialog?szChangeProgramCaption:szAddProgramCaption);
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcCancel);
		SetUpButtonForGraphics(tmcpaddHB);
			SetUpButtonForGraphics(tmcadvanced);
			SetUpCheckBox(tmcpaddprompt) ;

			SetUpEditBox(tmcpaddtitle, TRUE,MAXUSERTITLE, TRUE);
			SetUpEditBox(tmcpaddcommand, TRUE, MAXUSERCOMMAND, FALSE);
			SetUpEditBox(tmcaddpassword, TRUE,MAXUSERPASSWORD, FALSE);
			SetUpEditBox(tmcpaddstartupdir, TRUE,ALLOWED_PATH, FALSE);
		SetUpSpecialEditBoxForGraphics(tmcshortcut);
		   //HookISR9();
		}
		break;
		case dlmTerm:
		{
			if (tmc == tmcOK)
			{
				GetTmcText(tmcpaddcommand, Temp_Str, MAXUSERCOMMAND);
				if (Temp_Str[0] == '\0')
				{
					Shell_Beep() ;
					SetFocusTmc(tmcpaddcommand) ;
					return FALSE ;
				}
				GetTmcText(tmcpaddtitle, Temp_Str, MAXUSERTITLE);
				if ((Temp_Str[0] == '\0'))
				{
					Shell_Beep() ;
					SetFocusTmc(tmcpaddtitle) ;
					return FALSE ;
				}

				gPauseChecked = GetTmcVal(tmcpaddprompt);
			}
		}
		break;
		case dlmClick:
		{
			if(tmc == tmcadvanced)
			{
				Advanced();

				/* In case we ran out of memory trying to save graphics behind the
				 * "Advanced Dialog" -- The "add" dialog, repaint it entirely.
				 */
				//if (gisgraph && gfRepaintonidle)
				/* ZZZZZ How do I know if we indeed ran out of memory instead
				 * of just always repainting?
				 */
				if (gisgraph)
				{
				fDrawItem = TRUE;
				fRedrawItem=FALSE;

					/* We need to set dialogcaption again as we changed it just
					 * when we put up the "Advanced" dialog
					 */
					szCurDialogCaption = (ischangedialog) ? szChangeProgramCaption : 
																		 szAddProgramCaption;

					// Make it repaint the whole dialog window with borders, etc.
				// SendMessage(PwndParent(PwndOfListbox(tmcOK)), 
					//                                                                                                                      WM_PAINT, 0, 0L);

					// make it draw only the dialog borders.
					DrawDialogBorderAndTitle(PwndParent(PwndOfListbox(tmcOK)));

					/* ZZZZZZZ */
					/* If one wanted to optimize for code size, one can actually
					 * make use of the fact that the tmc values are actually in
					 * order and one can have a loop here. However, if the dialog
					 * items are reordered or deleted could cause problems.
					 */
					/* repaint all the editboxes, check boxes, etc */
				SendMessage(PwndOfListbox(tmcpaddprogtitle), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcpaddtitle), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcpaddcommands), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcpaddcommand), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcpaddstartdir), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcpaddstartupdir), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcpaddappshortcutkey), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcshortcut), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcpaddprompt), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcpaddpasswd), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcaddpassword), WM_PAINT, 0, 0L);

					/* Repaint all the buttons */
				SendMessage(PwndOfListbox(tmcOK), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcCancel), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcpaddHB), WM_PAINT, 0, 0L);
				SendMessage(PwndOfListbox(tmcadvanced), WM_PAINT, 0, 0L);

					/* Reset this variable so that we don't keep drawing the
					 * buttons, etc.
					 */
					fRedrawItem = TRUE ;
				}
				
				SetFocusTmc(tmcOK);
			} else if(tmc == tmcpaddHB) {
				Help(hemDialog, ((PDLG)&dlgaddprogram)->hid, NULL, 0);
			SetFocusTmc(gCurrentTMC) ;
	    }
		 break ;
		}
		break;
		case dlmSetFocus:
	   gCurrentTMC = tmc;
	   break ;

	  
	}

	return(TRUE);
}

WORD gVideoMode;
BOOL gAltEsc, gAltTab, gCtrlEsc, gPrevent;

BOOL FAR PASCAL FDlgadvanced(dlm, tmc, wNew, wOld, wParam)
WORD    dlm;
TMC     tmc;
WORD    wNew, wOld, wParam;
{
    UnReferenced(tmc);
    UnReferenced(wNew);
    UnReferenced(wOld);
    UnReferenced(wParam);

    if(dlm == dlmInit) {
	SetTmcVal(tmcvideomode,gVideoMode);
	SetTmcVal(tmcalttab,gAltTab);
	SetTmcVal(tmcaltesc,gAltEsc);
	SetTmcVal(tmcctrlesc,gCtrlEsc);
	SetTmcVal(tmcprevent,gPrevent);

	SetUpDialog(tmcOK,szAdvanced);
	SetUpButtonForGraphics(tmcOK);
	SetUpButtonForGraphics(tmcCancel);
	SetUpButtonForGraphics(tmcadvancedHB);

	SetUpEditBox(tmcaddhelptext, TRUE, MAXUSERHELP, TRUE);

	SetUpEditBox(tmckbrequired, TRUE,5, FALSE); // 5 digits for kb required! big program!
	SetUpEditBox(tmcxmsrequired, TRUE,5, FALSE);
	SetUpEditBox(tmcxmslimit, TRUE, 5, FALSE);
		  SetUpCheckBox(tmcalttab) ;
		  SetUpCheckBox(tmcaltesc) ;
		  SetUpCheckBox(tmcalttab) ;
		  SetUpCheckBox(tmcctrlesc) ;
		  SetUpCheckBox(tmcprevent) ;
	if(gisgraph) {
	    SetUpRadioGroupForGraphics(tmcvideomode, tmcxmslimit, tmcprevent);
	    SetUpRadiobuttonForGraphics(tmctextmode,0);
	    SetUpRadiobuttonForGraphics(tmcgraphicsmode,1);
	}
    } else if(dlm == dlmTerm) {
	if(tmc == tmcOK) {
	    gVideoMode = GetTmcVal(tmcvideomode);
	    gAltEsc = GetTmcVal(tmcaltesc);
	    gAltTab = GetTmcVal(tmcalttab);
	    gCtrlEsc = GetTmcVal(tmcctrlesc);
	    gPrevent = GetTmcVal(tmcprevent);
	}
    } else if(dlm == dlmSetFocus)
	gCurrentTMC = tmc;
	else if(dlm ==  dlmClick) {
		 if(tmc == tmcadvancedHB)
			Help(hemDialog, hidADVANCED, NULL, 0);

		 SetFocusTmc(gCurrentTMC) ;
    }

    return(TRUE);
}

BOOL FAR PASCAL
FDlgadddialog(dlm, tmc, wNew, wOld, wParam)
WORD    dlm;
TMC     tmc;
WORD    wNew, wOld, wParam;
{
	UnReferenced(tmc);
	UnReferenced(wNew);
	UnReferenced(wOld);
	UnReferenced(wParam);
	if(dlm == dlmInit)
	{
		SetUpDialog(tmcOK,ischangedialog?szChangeProgramCaption:szAddProgramCaption);
	   SetUpButtonForGraphics(tmcOK);
	   SetUpButtonForGraphics(tmcCancel);
		SetUpButtonForGraphics(tmcdialHB);
		SetUpEditBox(tmcdialtitle, TRUE,MAXUSERTITLE, TRUE);
		SetUpEditBox(tmcdialinformation, TRUE,MAXUSERINSTRUCTIONS, FALSE);
		SetUpEditBox(tmcdialprompt, TRUE,MAXUSERPROMPT, FALSE);
		SetUpEditBox(tmcdialdefault, TRUE,MAXUSERDEFAULT, FALSE);

	} else if(dlm == dlmSetFocus)
	gCurrentTMC = tmc;
	else if(dlm == dlmClick) {
		 if(tmc == tmcdialHB)
			Help(hemDialog, hidADDDIALO, NULL, 0);

		 SetFocusTmc(gCurrentTMC) ;
    }
	return(TRUE);
}

BOOL FAR PASCAL
FDlgaddgroup(dlm, tmc, wNew, wOld, wParam)
WORD    dlm;
TMC     tmc;
WORD    wNew, wOld, wParam;
{
    char Temp_Str[MAXUSERTITLE+1];

	UnReferenced(tmc);
	UnReferenced(wNew);
	UnReferenced(wOld);
	UnReferenced(wParam);

    switch(dlm) {
	case dlmInit:
		SetUpDialog(tmcOK,ischangedialog?szChangeGroupCaption:szAddGroupCaption);
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcgrouphelp);
		SetUpButtonForGraphics(tmcCancel);

		SetUpEditBox(tmcgrouptitle, TRUE,MAXUSERTITLE, TRUE);
		SetUpEditBox(tmcgrouphelptext, TRUE,MAXUSERHELP, FALSE);
		SetUpEditBox(tmcgrouppassword, TRUE,MAXUSERPASSWORD, FALSE);
	break;

	case dlmSetFocus:
	gCurrentTMC = tmc;
	break;

	case dlmClick:
		if(tmc == tmcgrouphelp)
	    Help(hemDialog, ((PDLG)&dlgaddgroup)->hid, NULL, 0);

		SetFocusTmc(gCurrentTMC) ;
	break;

    case dlmTerm:
	if(tmc == tmcOK) {
	    GetTmcText(tmcgrouptitle, Temp_Str, MAXUSERTITLE);
	    if(Temp_Str[0] == '\0') {
		Shell_Beep() ;
		SetFocusTmc(tmcgrouptitle) ;
		return FALSE ;
	    }
	}
    }

	return(TRUE);
}

VOID StartAProgram()
{
		if(gTaskListEnabled && TaskList.hasglobalfocus) 
		{
			ListProcTaskList(tmmActivate, NULL, TaskList.focusitem, 0, 0, 0, 0);
		}
		else
		{
		ListProcProgramList(tmmActivate, NULL, ProgramList.focusitem, 0, 0, 0, 0);
		}
}

#define CHARPROMPT '%'

TOKEN GetParamDialog(TOKEN list, int param)
{
    TOKEN token, token2;
    char tstr[10];
    int i, length;

    length = Get_List_Length(list);

    for(i=0; i<length; ++i) {
	token = Get_Ith_Element(list, i+1);
	if(Get_Symbol_Type(Token_To_Symbol(token)) != TK_DIALOG)
	    continue;

	token = Get_Symbol_Value(Token_To_Symbol(token));
	if((token2=Get_KeyWord_Assignment(token, TK_PARAMETER)) > 0) {
	    FarToNearsz(tstr, Get_Token_Identifier(token2), sizeof(tstr));
	    if(tstr[0]==CHARPROMPT && (int)tstr[1]-'0'==param)
		return(token);
	}
    }

    return(-1);
}

TOKEN gPropertyItem;
BOOL FAR AddDialog(TOKEN list, int param)
{
    TOKEN token, token2;
    char tstr[256];
    BOOL retval = FALSE;        
    HCABadddialog h;

    if(!(h=HcabAlloc(cabiCABadddialog))) {
	OutOfMemory() ;
	return(FALSE) ;
    }
    InitCab(h, cabiCABadddialog) ;

    if((token=GetParamDialog(list, param)) <= 0) {
		if((token2=SubClassSymbol(TK_DIALOG))<=0 ||
			(token=SubClassSymbol(TK_DIALOG))<=0)
	    goto Error1;
		Append_Symbol(list, token2);
		Set_Symbol_Value(Token_To_Symbol(token2), token);
    }

    VariableToCab(token, TK_TITLE, h, Iag(CABadddialog, pszdialtitle));
    VariableToCab(token, TK_INFO, h, Iag(CABadddialog, pszdialinformation));
    VariableToCab(token, TK_PROMPT, h, Iag(CABadddialog, pszdialprompt));
    VariableToCab(token, TK_DEFAULT, h, Iag(CABadddialog, pszdialdefault));

    tstr[0] = (char) param+'0'; /*WARNING param must be between 0 and 9! */
    tstr[1] = 0;
    SzToCab(h, tstr, Iag(CABadddialog, pszdialparamnum));
    SzToCab(h, szEnterButton, Iag(CABadddialog, pszdialEB));
    SzToCab(h, szCancelButton, Iag(CABadddialog, pszdialCB));
    SzToCab(h, szHelpButton, Iag(CABadddialog, pszdialHB));

    if(MyTmcDoDlg(&dlgadddialog,  h) == tmcOK) {
	CabToVariable(h, Iag(CABadddialog, pszdialtitle), token, TK_TITLE);
	CabToVariable(h, Iag(CABadddialog, pszdialinformation), token, TK_INFO);
	CabToVariable(h, Iag(CABadddialog, pszdialprompt), token, TK_PROMPT);
	CabToVariable(h, Iag(CABadddialog, pszdialdefault), token, TK_DEFAULT);

	tstr[0] = CHARPROMPT;
/* BUG BUG param *must* be between 1 and 9 */
	tstr[1] = (char) param + (char)'0';
	tstr[2] = (char) 0;
	Set_KeyWord_Assignment(token, TK_PARAMETER, Get_Identifier_Token(tstr));
	retval = TRUE;
    }

    FreeCab(h);
    InitIconCache();
Error1:
    return(retval);
}

/*
 * after each % is a number pertaining to a parameter
 */
BOOL FindParameters(TOKEN list,char *commandstring)
{
#define NUMPARAMS 10
   int i;
   int len;
   int param;
   BOOL parametersdone [NUMPARAMS];

   /*
    * We haven't done dialogs for any parameter
    * yet
    */
	for(i=0;i<NUMPARAMS;i++)
		parametersdone[i] = FALSE;
   len = strlen(commandstring);
   for(i=0;i<len;i++)
   {
		if(commandstring[i] == '%')
		{
			param = commandstring[i+1] - '0';
		//param = atoi(&commandstring[i+1]); THIS WONT WORK FOR %S
		if((param > 0) && (param < 10))
		{
				++i;
				/*
				* We need to find information about the
				* dialog for this parameter.
				* We should not ask twice for the same parameter!
				*/
				if(!parametersdone[param])
				{
				parametersdone[param] = TRUE;
				if(!AddDialog(list,param)) //user canceled
					{
						return(FALSE);
					}
				}
		}
		}
	}
	return(TRUE);
}

VOID FAR DoGroup(void)
{
    TOKEN tkGroup, list;
    HCABaddgroup h;

    if(!(h = HcabAlloc(cabiCABaddgroup))) {
	OutOfMemory();
	return;
    }
    InitCab(h, cabiCABaddgroup);

    if(ischangedialog) {
	((PDLG)&dlgaddgroup)->hid = hidCHANGEGROUP;
	if((tkGroup=Get_Focus_Item())<=0 || tkGroup==TK_PROGRAMSTARTER ||
		(tkGroup=Get_Symbol_Value(Token_To_Symbol(tkGroup)))<=0)
	    goto Error1;

	VariableToCab(tkGroup, TK_TITLE, h, Iag(CABaddgroup, pszgrouptitle));
	VariableToCab(tkGroup, TK_HELP, h, Iag(CABaddgroup, pszgrouphelp));
	if(!VariableToCab(tkGroup, TK_PASSWORD,
		h, Iag(CABaddgroup, pszgrouppassword)))
	    goto Error1;
    } else {
	((PDLG)&dlgaddgroup)->hid = hidADDGROUP;
	SzToCab(h, NullString, Iag(CABaddgroup, pszgrouptitle));
	SzToCab(h, NullString, Iag(CABaddgroup, pszgrouphelp));
	SzToCab(h, NullString, Iag(CABaddgroup, pszgrouppassword));
    }

    SzToCab(h, szCancelButton, Iag(CABaddgroup, pszaddgCB));
    SzToCab(h, szEnterButton, Iag(CABaddgroup, pszaddgSB));
    SzToCab(h, szHelpButton, Iag(CABaddgroup, pszaddgHB));

    if(MyTmcDoDlg(&dlgaddgroup, h) == tmcOK) {
	if(!ischangedialog) {
	    if((list=SubClassSymbol(TK_GROUP))<=0 ||
		    (tkGroup=SubClassSymbol(list))<=0)
		goto Error1;
	    Set_Symbol_Value(Token_To_Symbol(list), tkGroup);
	    Append_Symbol(gGroupLevel!=TK_PROGRAMSTARTER ? gGroupLevel :
		    Get_KeyWord_Assignment(gGroupLevel, TK_GROUP), list);
	}

	CabToVariable(h, Iag(CABaddgroup, pszgrouptitle), tkGroup, TK_TITLE);
	CabToVariable(h, Iag(CABaddgroup, pszgrouphelp), tkGroup, TK_HELP);
	CabToVariable(h, Iag(CABaddgroup, pszgrouppassword),
		tkGroup, TK_PASSWORD);

	if(!Buffered_Write_Ini_File(FALSE))
	    ShellMessageBox(szNoIniTitle, szNoIniMsg1);
    }

Error1:
    FreeCab(h);
    InsertListItem(&ProgramList, 0);
    InitIconCache();
}

VOID FAR DoProgram(void)
{
    TOKEN text, program, grouplevel, token;
    char tstr[256];
    int i;
    BOOL bFirstTime = TRUE;
    HCABaddprogram h;

    if(!(h = HcabAlloc(cabiCABaddprogram))) {
	OutOfMemory();
	return ;
    }

/* Get all the correct stuff from the .ini file */
/* Get_Focus_Item must be called before determining the grouplevel to make sure
 * there is a grouplevel
 */
    token = Get_Focus_Item();

    if(!(program=SubClassSymbol(TK_PROGRAM)))
	goto Error1;

    if(ischangedialog) {
	if(token<=0 || Get_Symbol_Type(Token_To_Symbol(token))!=TK_PROGRAM) {
	    Shell_Beep();
	    goto Error1;
	}

	((PDLG)&dlgaddprogram)->hid = hidCHANGEPROG;
	token = Duplicate_Token(Get_Symbol_Value(Token_To_Symbol(token)));
    } else {
	((PDLG)&dlgaddprogram)->hid = hidADDPROG;
	token = SubClassSymbol(program);
    }
    if(token <= 0) {
	Shell_Beep();
	goto Error1;
    }

    Set_Symbol_Value(Token_To_Symbol(program), token);
    gPropertyItem = token; /* save for AddDialog!,and Advanced too! */

    if(gGroupLevel == TK_PROGRAMSTARTER)
	grouplevel = Get_KeyWord_Assignment(gGroupLevel, TK_GROUP);
    else
	grouplevel = gGroupLevel;

redoprogram:
    InitCab(h, cabiCABaddprogram);

    VariableToCab(token, TK_TITLE, h, Iag(CABaddprogram, pszpaddtitle));
    VariableToCab(token, TK_COMMAND, h, Iag(CABaddprogram, pszpaddcommand));
    VariableToCab(token, TK_DIRECTORY, h,Iag(CABaddprogram, pszpaddstartupdir));
    gPauseChecked = Get_KeyWord_Assignment(token, TK_PAUSE) != TK_DISABLED;
    if(bFirstTime) {
	if(!VariableToCab(token, TK_PASSWORD,
		h, Iag(CABaddprogram, pszaddpassword)))
	    goto Error1;
    } else
	SzToCab(h, userPassword, Iag(CABaddprogram, pszaddpassword));
    VariableToCab(token, TK_SHORTCUT, h, Iag(CABaddprogram, pszshortcut));

    gScanInfo = 0;
    SzToCab(h, szEnterButton, Iag(CABaddprogram, pszpaddEB));
    SzToCab(h, szCancelButton, Iag(CABaddprogram, pszpaddCB));
    SzToCab(h, szHelpButton, Iag(CABaddprogram, pszpaddHB));

    if(MyTmcDoDlg(&dlgaddprogram, h) != tmcOK)
	goto Error1;

    SzFromCab(h, tstr, sizeof(tstr), Iag(CABaddprogram, pszshortcut));
    if(*tstr) {
/*strip off trailing spaces*/
	for(i=strlen(tstr)-1; i>0 && tstr[i]==' '; --i) /* empty loop */ ;
	tstr[i+1] = 0;
	text = Get_Identifier_Token(tstr);
    } else
	text = TK_UNKNOWN;
    Set_KeyWord_Assignment(token, TK_SHORTCUT, text);

    if(!strcmp(tstr, szNone))
	Set_KeyWord_Assignment(token, TK_SHORTCUTCODE, TK_UNKNOWN);
    else if(text!=TK_UNKNOWN && gScanInfo) {
/* if there was a hotkey string, there must be a scancode */
	itoa(gScanInfo, tstr, 10);
	text = Get_Identifier_Token(tstr);
	Set_KeyWord_Assignment(token, TK_SHORTCUTCODE, text);
    }

    CabToVariable(h, Iag(CABaddprogram, pszpaddcommand), token, TK_COMMAND);
    CabToVariable(h, Iag(CABaddprogram, pszpaddtitle), token, TK_TITLE);
    CabToVariable(h, Iag(CABaddprogram, pszaddpassword), token, TK_PASSWORD);
    SzFromCab(h, userPassword, sizeof(userPassword),
	    Iag(CABaddprogram, pszaddpassword));
    bFirstTime = FALSE;
    CabToVariable(h,Iag(CABaddprogram, pszpaddstartupdir), token, TK_DIRECTORY);
    Set_KeyWord_Assignment(token, TK_PAUSE,
	    gPauseChecked ? TK_ENABLED : TK_DISABLED);

    SzFromCab(h, tstr, sizeof(tstr), Iag(CABaddprogram, pszpaddcommand));
    if(!FindParameters(token, tstr))
	goto redoprogram;

    if(ischangedialog) {
	i = Get_List_Focus(&ProgramList) -
		(gGroupLevel==TK_PROGRAMSTARTER ? 0 : 1);
	Delete_Ith_Element(grouplevel, GetIthGroupOrProgramIndex(grouplevel,i));

	if(--i < 0)
	    i = 0;
	else
	    i = GetIthGroupOrProgramIndex(grouplevel, i);
	Insert_Symbol(grouplevel, program, i);
    } else
	Append_Symbol(grouplevel, program);

    InsertListItem(&ProgramList, 0);

    if(!Buffered_Write_Ini_File(FALSE))
	ShellMessageBox(szNoIniTitle, szNoIniMsg1);

Error1:
    FreeCab(h);
    InitIconCache();
}

VOID FAR Advanced(void)
{
    TOKEN token;
    HCABadvanced h;

    if(!(h = HcabAlloc(cabiCABadvanced))) {
	OutOfMemory();
	return;
    }
    InitCab(h, cabiCABadvanced);

    if((token=gPropertyItem) <= 0)
	goto Error1;

    VariableToCab(token, TK_HELP, h, Iag(CABadvanced, pszaddhelptext));
    VariableToCab(token, TK_KBREQUIRED, h, Iag(CABadvanced, pszkbrequired));
    VariableToCab(token, TK_XMSREQUIRED, h, Iag(CABadvanced, pszxmsrequired));
    VariableToCab(token, TK_XMSLIMIT, h, Iag(CABadvanced, pszxmslimit));

    gVideoMode = Get_KeyWord_Assignment(token, TK_SCREENMODE)
	    == TK_GRAPHICS ? 1 : 0;
    gAltEsc = Get_KeyWord_Assignment(token, TK_ALTESC) == TK_DISABLED;
    gAltTab = Get_KeyWord_Assignment(token, TK_ALTTAB) == TK_DISABLED;
    gCtrlEsc = Get_KeyWord_Assignment(token, TK_CTRLESC) == TK_DISABLED;
    gPrevent = (Get_KeyWord_Assignment(token, TK_PREVENT) == TK_ENABLED);

    SzToCab(h, szEnterButton, Iag(CABadvanced, pszadvancedEB));
    SzToCab(h, szCancelButton, Iag(CABadvanced, pszadvancedCB));
    SzToCab(h, szHelpButton, Iag(CABadvanced, pszadvancedHB));

    if(MyTmcDoDlg(&dlgadvanced, h) == tmcOK) {
	CabToVariable(h, Iag(CABadvanced, pszaddhelptext), token, TK_HELP);
	CabToVariable(h, Iag(CABadvanced, pszkbrequired), token, TK_KBREQUIRED);
	CabToVariable(h, Iag(CABadvanced, pszxmsrequired),token,TK_XMSREQUIRED);
	CabToVariable(h, Iag(CABadvanced, pszxmslimit), token, TK_XMSLIMIT);

	Set_KeyWord_Assignment(token, TK_SCREENMODE,
		gVideoMode == 0 ? TK_TEXT : TK_GRAPHICS);
	Set_KeyWord_Assignment(token, TK_ALTTAB,
		gAltTab == 0 ? TK_ENABLED : TK_DISABLED);
	Set_KeyWord_Assignment(token, TK_ALTESC,
		gAltEsc == 0 ? TK_ENABLED : TK_DISABLED);
	Set_KeyWord_Assignment(token, TK_CTRLESC,
		gCtrlEsc == 0 ? TK_ENABLED : TK_DISABLED);
	Set_KeyWord_Assignment(token, TK_PREVENT,
		gPrevent != 0 ? TK_ENABLED : TK_DISABLED);
    }
Error1:
    FreeCab(h);
    InitIconCache();
}

VOID FAR ChangeProgram()
{
    TOKEN token;

    if((token=Get_Focus_Item())<=0 || token==TK_PROGRAMSTARTER)
	return;

    ischangedialog = TRUE;
    if(Get_Symbol_Type(Token_To_Symbol(token)) == TK_PROGRAM)
	DoProgram();
    else
	DoGroup();
}

int GroupOrProgram;
BOOL FAR PASCAL FDlgnew(WORD dlm, TMC tmc, WORD wNew,WORD wOld, WORD wParam)
{

	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;
	switch (dlm) {
	case dlmInit:
	{

		SetUpDialog(tmcOK, szNewProgObjCaption);
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcCancel);
		if (gisgraph)
		{
		   SetUpRadioGroupForGraphics(tmcnewgroup, tmcCancel, tmcOK);
		   SetUpRadiobuttonForGraphics(tmcnewgroupitem,0);
		   SetUpRadiobuttonForGraphics(tmcnewprogramitem,1);
		}
		SetTmcVal(tmcnewgroup,1);
	}
	case dlmTerm:
	    GroupOrProgram = GetTmcVal(tmcnewgroup);
	    break;
	case tmcCancel:
	    break;
	case dlmSetFocus:
	    gCurrentTMC = tmc;
	    break ;
	}
	return(TRUE);
}


VOID FAR AddProgram()
{
     HCABnew        h;
     TMC tmc;
     h = HcabAlloc(cabiCABnew);
     if (!h)
     {
		OutOfMemory() ;
		return ;
     }
     InitCab(h, cabiCABnew) ;

     SzToCab(h, szEnterButton, Iag(CABnew, psznewEB));
     SzToCab(h, szCancelButton, Iag(CABnew, psznewCB));

     tmc = MyTmcDoDlg(&dlgnew, h);
     FreeCab(h);
     if(tmc == tmcOK)
     {
		 if(GroupOrProgram) /* Program */
		 {
			ischangedialog = FALSE;
			DoProgram();
		 }
		 else
		 {
			AddGroup();
		 }
     }
}


VOID FAR AddGroup()
{
    ischangedialog = FALSE;
    DoGroup();
}

VOID FAR ChangeGroup()
{
    ischangedialog = TRUE;
    DoGroup();
}

WORD GetIthGroupOrProgramIndex(TOKEN group,int isz)
{

	int length;
	int i;
	int numitems;
	TOKEN token2,token4,tokentemp;

 if (group>0)
	{

		length = Get_List_Length(group);
		numitems =0;
		for(i=1;i<=length;i++)
		{
		       token4 = Get_Ith_Element(group,i);
		       tokentemp = Get_Symbol_Type(Token_To_Symbol(token4));

		       if(IsGroupOrProgramType(tokentemp))
		       {
			     token2 = token4;
			     ++numitems;
		       }
		       if(numitems-1 == isz)
		       {
			     return(i);
		       }
		}
	}
	else
	{
		return(-1);
	}
}



VOID CopyProgram(void)
{
    gInReorder = FALSE;
    if((gCopyProgram=Get_Focus_Item())>0 &&
	    Get_Symbol_Type(Token_To_Symbol(gCopyProgram))==TK_PROGRAM &&
	    (gCopyProgram=Get_Symbol_Value(Token_To_Symbol(gCopyProgram)))>0) {
	gInCopy = TRUE;
	MessageBar(szCopyProgram, isaMenu,TRUE);
    } else {
	gInCopy = FALSE;
	gCopyProgram = TK_NOTHING;
    }
    InitIconCache();
}

VOID FAR DoCopyProgram()
{
    TOKEN groups, token, dup;

    if(gGroupLevel == TK_PROGRAMSTARTER)
	groups = Get_KeyWord_Assignment(gGroupLevel, TK_GROUP);
    else
	groups = gGroupLevel;

    if(groups>0 && (dup=Duplicate_Token(gCopyProgram))>0 &&
	    (token=SubClassSymbol(TK_PROGRAM))>0) {
	Set_Symbol_Value(Token_To_Symbol(token), dup);
	Append_Symbol(groups, token);
    } else
	Shell_Beep();

    MessageBar(szFileMessage, isaMenu,TRUE);
    InsertListItem(&ProgramList,0);
    SetGroupTitle(gGroupLevel);
    gInCopy = FALSE;
    InitIconCache();
}

extern char *bigProgramIconData;

VOID ReorderGroup(void)
{
    TOKEN tkGroup;
    WORD element;

    gInReorder = TRUE;
    gInCopy = FALSE;

    element = Get_List_Focus(&ProgramList);
    if(gGroupLevel == TK_PROGRAMSTARTER) {
	tkGroup = Get_KeyWord_Assignment(gGroupLevel, TK_GROUP);
    } else {
	tkGroup = gGroupLevel;
	--element;
    }

    if(tkGroup > 0) {
	gReorderItem = GetIthGroupOrProgramIndex(tkGroup, element);
	MessageBar(szReorderGroup, isaMenu, TRUE);
	InitIconCache();
    } else {
	gInReorder = FALSE;
	Shell_Beep();
    }
}

VOID FAR DoReorderGroup(void)
{
    TOKEN tkGroup, tkItem;
    int element;

    gInReorder = FALSE;

    element = Get_List_Focus(&ProgramList) - 1;
    if(gGroupLevel == TK_PROGRAMSTARTER) {
	tkGroup = Get_KeyWord_Assignment(gGroupLevel,TK_GROUP); 
    } else {
	tkGroup = gGroupLevel;
	--element;
    }

    if(tkGroup > 0) {
	tkItem = Get_Ith_Element(tkGroup, gReorderItem);
	Delete_Ith_Element(tkGroup, gReorderItem);

	if(element < 0)
	    element = 0;
	else
	    element = GetIthGroupOrProgramIndex(tkGroup, element);

	Insert_Symbol(tkGroup, tkItem, element);
	InsertListItem(&ProgramList, 0);
	InitIconCache();
    } else
	Shell_Beep();

    MessageBar(szFileMessage, isaMenu, TRUE);
}

void AddProgManAccelerators(void);

VOID InitStartProgramList(WORD top,WORD left,WORD bottom,WORD right)
{
    if(!gLoadEqualsProcessed) {
	gInitPMFocus = -1; /* default value */
	gGroupLevel = TK_NOTHING;
	Get_StartupItem(TK_PROGRAMSTARTER);
	Set_Task_Info(TK_NOTHING);
	gLoadEqualsProcessed=TRUE; //see get_startupgroup bug bug
	if(gGroupLevel <= 0)
	    gGroupLevel = TK_PROGRAMSTARTER;
	if(gGroupLevel != TK_PROGRAMSTARTER)
	    ++gInitPMFocus;
    } else
	gInitPMFocus = Get_List_Focus(&ProgramList);

    ListBoxInit(&ProgramList,ListProcProgramList,&MainWind,
	    top,left,bottom,right,szMainGroup,999,0,0);
/* gInitPMFocus is set in Get_StartupItem */
    FocusLineChange(&ProgramList, gInitPMFocus - Get_List_Focus(&ProgramList));
    SetGroupTitle(gGroupLevel);
}
VOID InitializeStartPrograms(VOID)
{
      RRC rrcClient;


      UpdateMainTitleBar(szDOSShellTitle);
      setmenubar(&MainMenuBar,&MainWind);
      AddProgManAccelerators();
      GetClientRrc(&MainWind,&rrcClient);
      rrcClient.ryTop +=2; /* exclude title and menu */
      rrcClient.ryBottom -=1; /*exclude status bar */
      FillRrc(&MainWind,&rrcClient,' ',isaBackground);
      gInCopy = FALSE;
      gInReorder = FALSE;
#if 0
      /*
       * If there is no setting for command or filemanager,
       * then we don't have an ini file or it is messed up.
       * so we add them here so when we save the ini they will
       * be present
       */
       token = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_GROUP);
       if(token < 0)  /* there is no group listing */
       {
	    commandtoken = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_COMMAND);
	    filemgrtoken = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_FILEMGR);

	    if(commandtoken ) && (filemgrtoken != TK_DISABLED)
	    {
	       /*
		* looks like the ini file either doesn't exist, or
		* is messed up, so we be sure to use command and
		* filemgr!
		*/
	       commandtoken = TK_ENABLED;
	       filemgrtoken = TK_ENABLED;
	    }
       }
       else
       {

       }

	    return(Get_List_Length(token));
#endif
      FrameMenuBar(&MainWind);
      MessageBar(szProgMessage, isaMenu,TRUE);
#ifndef NOSWITCHER
		if(gTaskListEnabled)
	   {
#define TASKADJ  1
	InitStartProgramList(3,0,ayMac-1,axMac/2-TASKADJ);
	InitTaskMan(3,axMac/2-TASKADJ,ayMac-1,axMac);
	GlobalFocusBox(&TaskList,FALSE);
	GlobalFocusBox(&ProgramList,TRUE);
	   }
		else
#endif
		{
	InitStartProgramList(3,0,ayMac-1,axMac);
		}
      InitIconCache();
}

extern WORD gCnx;
BOOL DoStartProgramsIdle(VOID)
{
#ifndef NOSWITCHER
		if(gTaskListEnabled)
	   return(ListBoxIdle(&ProgramList)|TaskManIdle());
	   else
#endif
	   return(ListBoxIdle(&ProgramList));

}
VOID RefreshTaskMan(VOID);
VOID RefreshStartPrograms(VOID)
{
      //DoRedisplayList(&ProgramList);
		InsertListItem(&ProgramList,0);
      UpdateListBox(&ProgramList);
		RefreshTaskMan();
}


BOOL StartProgramsIdle(VOID)
{
    if (!(glob.InFileMgr || m_fPerformingViewFile()))
    {
	if(gCnx != cnxDialog)

	   return(DoStartProgramsIdle());
    }
    else
	return(TRUE);
}

VOID GroupManagerKey(WORD ,DWORD );
VOID GroupManagerKey(WORD wParam,DWORD LParam)
{
#ifndef NOSWITCHER
	 if(gTaskListEnabled)
	 {
	if(wParam == '\t')
      {
			if(!glob.InFileMgr)
			{
			GlobalFocusBox(&TaskList,!TaskList.hasglobalfocus);
			GlobalFocusBox(&ProgramList,!ProgramList.hasglobalfocus);
			}
			else
			{
				NextGlobalFocus();
			}
			return;
      }
	 }
#endif
#ifndef NOSWITCHER
      if((!gTaskListEnabled) || (!TaskManKey(wParam,LParam)))
	
#endif
      {
      if((gInCopy) || (gInReorder))
      {
	 /* escape for F3 will exit from copy mode */
	 if ((LOBYTE(wParam) == 0x1b) || (wParam == VK_F3))
	 {
	   gInCopy = FALSE;
	   gInReorder = FALSE;
		MessageBar(szFileMessage, isaMenu,TRUE);
	   // Shell_Beep();
	 }
	 else
	 if(wParam == VK_F2)
	 {
	    if(gInCopy)
	      DoCopyProgram();
#if 0
	    else
	      DoReorderGroup();
#endif
	 }
	 else
		ListKey(&ProgramList,wParam,HIWORD(LParam));
	 return;
      }
      else
      {
			if (LOBYTE(wParam) == 0x1b)
			{
				UpLevel();
			}
			else
			ListKey(&ProgramList,wParam,HIWORD(LParam));
      }
    }
}

VOID GroupManagerMouse(WORD mx,WORD my,WORD message,BYTE state)
{
       if ((message == WM_LBUTTONDOWN)&&
	   (my == Get_List_Rect(&ProgramList).ayTop-1) &&
	   (mx > Get_List_Rect(&ProgramList).axLeft) &&
	   (mx < Get_List_Rect(&ProgramList).axLeft+3))
       {
	    UpLevel();
       }
       else
       {
#ifndef NOSWITCHER
			if(gTaskListEnabled)
			{
				if((my >= Get_List_Rect(&ProgramList).ayTop-1) &&
			   (my <= Get_List_Rect(&ProgramList).ayBottom) &&
			(mx >= Get_List_Rect(&ProgramList).axLeft) &&
			   (mx <= Get_List_Rect(&ProgramList).axRight))
			{
			if(ListMouse(&ProgramList,mx,my,message,state))
			{
						if(!ProgramList.hasglobalfocus)
						{
						GlobalFocusBox(&ProgramList,TRUE);
						   GlobalFocusBox(&TaskList,FALSE);
						}
			}
			   }
			else
			   {
			   if(TaskManMouse(mx,my,message,state))
			{
						if((!TaskList.hasglobalfocus) && (C_GET_LIST_LENGTH()))
						{
						   GlobalFocusBox(&TaskList,TRUE);
						GlobalFocusBox(&ProgramList,FALSE);
						}
			      }
			}
			}
			else
#endif
		ListMouse(&ProgramList,mx,my,message,state);
      }
}

extern void MarkAsInFM(void) ;

/* The following are for switching to the other view modes from
 * progman. We do it differently since the file manager must be inited
 * as this was once a "separate" program.
 * These cannot be macros, as they are called from menu items.
 */
VOID PDoSingleTree(void)
{
	DoSingleTree();
	MarkAsInFM() ;
}

VOID PDoDoubleTree(void)
{
	DoDoubleTree();
	MarkAsInFM() ;
}

VOID PDoFlatDisplay(void)
{
	DoFlatDisplay();

	/* If the tree is in compact tree mode, we refuse to get into this
	 * FM mode from the PM mode.
	 */
	if (!listinfo[0].tree->Compacted)
		MarkAsInFM() ;
}

VOID PDoShareMode(void)
{
	DoShareMode();
	MarkAsInFM() ;
}

