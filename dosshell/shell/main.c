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
#include <time.h>
#include <direct.h>

extern GlobalIdle(void);
extern TOKEN Get_Identifier_Token(char far *identifier);
extern void Shell_TTY_Out(char *str);
extern void Get_CWD(char *);

extern BOOL AddTask(char far *programname, char far *parameters,char far *defaulttitle,TOKEN properties);

struct CountryBuffer {
    int Date_Format;
    char Currency_Symbol[5];
    char Thousands_Sep[2];
    char Decimal_Sep[2];
    char Date_Sep[2];
    char Time_Sep[2];
    char Currency_Pos;
    char Num_Decimals;
    char Time_Format;
    long Case_Mapping;
    char Data_Sep[2];
    char Reserved[10];
} ;
struct CountryBuffer NationData;

MSG     msg;
INST    ginst;
INGD    gingd;
INDV    gindv;
BOOL    gisgraph = FALSE ;
INCH    cinch;

BOOL gfStillInitializing; /* used in allocs to bail if we're gonna toast */
BOOL gfScreenModeChanged; /* used to restore screen in case we bail out early */
BOOL gfSwapHandlerInstalled;

char gStartUpDir[1+MAX_PATH];
char gStartInDir[1+MAX_PATH];
int gStartUpDirEnd; /* location where the NULL goes in the above name */

BOOL gBeeps_On; /* Whether to have Beeps turned on or not on error */

gBeeps_On = TRUE ;

extern MENUINFO MainMenuBar;
extern MENUINFO FileMgrMenuBar;
extern BYTE ErrorCrit;

extern VOID setmenubar(MENUINFO *amenu,PWND towind);
extern BOOL GetInternationalData(void);
extern void InstallSwapHandler(void) ;
extern void     PrintHelpText(void);
extern VOID SetUpExitToDos(void); /* see init.c */
extern BOOL AsynchUpdateTreeList(void);

VOID DeleteBatchFile(void);

extern WND ViewWind;
extern BOOL gMouseDown;
extern BYTE gMouseX;    /* X value of last mouse-down */
extern BYTE gMouseY;    /* Y value of last mouse-down */

/* INTERNATIONALIZE HERE!
 * We still don't have international am/pm symbols
*/
/*
 * sets up datestr to be in format ' 00-00-00 '
 * sets up timestr to be in format ' 12:20 am '
 *
 * if force is not set, and time (minutes) has not changed, returns
 * false and does not format strings.
 * if force is set, formats strings and returns true
 * if force is not set, and time has changed, formats strings and returns true
 */

#define NumToAsc(num,str) (temp=(num)/10, *(str++)=(char)((temp%10)+'0'), *(str++)=(char)((num)-temp*10+'0'))

BOOL FAR Get_Date_and_Time_Strings(unsigned int theDate, unsigned int theTime,
      char *datestr, char *timestr, BOOL force)
{
    static struct tm lasttime;
    struct tm temptime, *newtime;
    time_t long_time;
    register int temp;
    int date1, date2, date3;  /* for international date order */

    if(!theDate && !theTime) {
	time(&long_time);
	newtime = localtime(&long_time);
    } else {
	temptime.tm_year = 80 + (theDate>>0x09);
	temptime.tm_mon  = ((theDate>>0x05)&0x0f) - 1;
	temptime.tm_mday = theDate&0x1f;
	temptime.tm_hour = theTime>>0x0b;
	temptime.tm_min  = (theTime>>0x05)&0x3f;
#if 0 /* We don't use the seconds */
	temptime.tm_sec  = 2*(theTime&0x1f);
#endif

	newtime = &temptime;
	goto FormatDateAndTime;
    }

    if (force || (newtime->tm_min != lasttime.tm_min))
    {
	lasttime = *newtime;
FormatDateAndTime:
	if(datestr) {
	    if(NationData.Date_Format == 1) {
		date1 = newtime->tm_mday;
		date2 = newtime->tm_mon + 1;
		date3 = newtime->tm_year;
	    } else if(NationData.Date_Format == 2) {
		date1 = newtime->tm_year;
		date2 = newtime->tm_mon + 1;
		date3 = newtime->tm_mday;
	    } else {
		date1 = newtime->tm_mon + 1;
		date2 = newtime->tm_mday;
		date3 = newtime->tm_year;
	    }

	    NumToAsc(date1, datestr);
	    *(datestr++) = *NationData.Date_Sep;
	    NumToAsc(date2, datestr);
	    *(datestr++) = *NationData.Date_Sep;
	    NumToAsc(date3, datestr);
	}

	if(timestr) {
	    if(NationData.Time_Format&0x01) { /* A 24-hour clock */
		timestr[5] = ' ';
		NumToAsc(newtime->tm_hour, timestr);
	    } else { /* A 12-hour clock */
		temp = newtime->tm_hour;
		if(temp < 12) {
		    if(temp == 0)
			temp = 12;
		    timestr[5] = 'a';
		} else {
		    if(temp != 12)
			temp = temp - 12;
		    timestr[5] = 'p';
		}
		if((temp/10)>0)
		    *(timestr++) = (char) ((temp/10) + '0');
		else
		    *(timestr++) = (char) ' ';
		*(timestr++) = (char) ((temp%10) + '0');
	    }
	    *(timestr++) = *NationData.Time_Sep;
	    NumToAsc(newtime->tm_min, timestr);
	}

	return(TRUE);
    } else {
       return(FALSE);
    }
}

/*
 * draw the top title bar--
 */
VOID FAR UpdateMainTitleBar(char *szTitle)
{
    char *titlestring;
    int i;
    WORD len;
    WORD titlestart;
    char padded[100];

	 titlestring = szTitle ;

    len = strlen(titlestring);
    titlestart = axMac/2 - len/2;
    for(i=0;i<axMac;i++)
    {
		padded[i] = ' ';
    }
    FEnableMouseNest(FALSE);

    TextOut(&MainWind,(RX) 0,(RY) 0, padded,titlestart,isaHilite);
	 TextOut(&MainWind,(RX) titlestart,0,titlestring,len,isaHilite);
    TextOut(&MainWind,(RX) titlestart+len,(RY)0, padded,axMac-(titlestart+len),isaHilite);
    if (gisgraph)
    {
	if(CHEIGHT > SMALLHEIGHT)
	{
	     SetAreaPat(0);
	     SetLinePat(1);
	     SetColor(0,0x7FFF);
	     Move(0,CHEIGHT-1);
	     Draw((axMac)*CWIDTH,CHEIGHT-1);
	}
    }
    FEnableMouseNest(TRUE);
}

VOID FAR PauseBeforeScreenErase(VOID)
{
#ifndef NOLOADER
    if(Get_KeyWord_Assignment(TK_SAVESTATE,TK_PAUSE) != TK_DISABLED)
    {
	if (GET_WAIT_FLAG())
	{
		 /* clear out keyboard buffer */
		 while(kbhit())
			getch();

		 Shell_TTY_Out(szPressAKey);
		 /* wait for key to be hit */
	    while(!kbhit())
				;
		 /* eat key */
	    getch();
	}
	Set_KeyWord_Assignment(TK_SAVESTATE,TK_PAUSE,TK_ENABLED);
    }
#endif
}


VOID ParseCommandLine(void)
{
    char far *commandline;
    char lastfound=0;
    char tstr[256], *szWhichRes;
    TOKEN tkRes;

#ifndef NOLOADER
    if(GET_WAIT_FLAG())
	return;

	/*      We use the previous bytes to store pause flag status, a far pointer, etc
	 *      See loader.asm for exact details.
	 */
	commandline = GET_COMMAND_PTR()+9;
#if 0
	printf("%d, ", *(commandline-2)) ;
	printf("%d, ", *(commandline-1)) ;
	printf("%d, ", *commandline) ;
	printf("%d, ", *(commandline+1)) ;
	printf("%d, ", *(commandline+2)) ;
	printf("%d, ", *(commandline+3)) ;
	printf("\n") ;
	getchar() ;
#endif
    for( ; ; ++commandline) {
	switch(*commandline) {
	case('\0'):
	case('\r'):
	    goto AllDone;
	    break;

	case('/'):
	    ++commandline;
	    switch(lastfound=(char)toupper(*commandline)) {
	    case 'T':
		Set_KeyWord_Assignment(TK_SAVESTATE, TK_SCREENMODE, TK_TEXT);
		break;

	    case 'G':
		Set_KeyWord_Assignment(TK_SAVESTATE, TK_SCREENMODE,TK_GRAPHICS);
		break ;

#ifdef ERICLIKESBEEPCONTROL
	    case 'B' : // /BEEP
		Set_KeyWord_Assignment(TK_SAVESTATE, TK_BEEP, TK_ENABLED);
		break ;

	    case 'N' : // /NOBEEP
		Set_KeyWord_Assignment(TK_SAVESTATE, TK_BEEP, TK_DISABLED);
		break ;
#else
	    case 'B' :
		Set_KeyWord_Assignment(TK_SAVESTATE, TK_FORCEMONO, TK_ENABLED);
		break ;
#endif

#ifdef SWAPMOUSESWITCH
	    case 'S' : // /SWAPMOUSE
		Set_KeyWord_Assignment(TK_SAVESTATE, TK_SWAPMOUSE,
			Get_KeyWord_Assignment(TK_SAVESTATE, TK_SWAPMOUSE)
			== TK_DISABLED ? TK_ENABLED : TK_DISABLED);
				break ;
#endif
#if 0
	    case '?' : // /HELP
	    case 'H' :
#endif
	    default  :
		PrintHelpText();
		SetUpExitToDos();
		exit(0);
	    }
	    break;

	case(':'):
	    ++commandline;
	    switch(lastfound) {
	    case 'G':
	    case 'T':
		switch(toupper(*commandline)) {
		case 'L':
		    tkRes = TK_LOWRES;
		    goto MakeKeyword;

		case 'M':
		    tkRes = TK_MEDIUMRES;
		    goto MakeKeyword;

		case 'H':
		    tkRes = TK_HIGHRES;
		    goto MakeKeyword;

MakeKeyword:
		    strfcpy(tstr, Get_Token_Identifier(tkRes));
		    for(szWhichRes=tstr; *szWhichRes; ++szWhichRes)
			/*do nothing */ ;
		    for(++commandline; *commandline>='0' && *commandline<='9';
			    ++commandline)
			*(szWhichRes++) = *commandline;
		    *szWhichRes = '\0';

		    Set_KeyWord_Assignment(TK_SAVESTATE, TK_RESOLUTION,
			    Get_Identifier_Token(tstr));
		    break;

		default:
		    break;
		}
				break;

	    default:
		break;
	    }

		    lastfound = 0;
	    break;

	default:
	    break;
	}
    }
AllDone:
    ;
#endif
}

extern char far * cdecl GET_STARTUP_NAME(VOID);
VOID FAR SetUpStartUpDirectory(VOID)
{
#ifndef NOLOADER
    strfcpy(gStartUpDir,GET_STARTUP_NAME());
    gStartUpDirEnd=FindLastComponent(gStartUpDir);
    gStartUpDir[gStartUpDirEnd]= 0;

#else
    strcpy(gStartUpDir,".\\");
	gStartUpDirEnd=2;
#endif
	Get_CWD(gStartInDir);

}

extern char *gpszNonSwap ;
/*
 * This is where all background tasking is done, so it should
 * be called often.
 */
VOID MainIdle(void)
{
   static  int     arbitrarycounter;

		   /* Add more idle procs here if required.
		   */
#ifdef PROF
		   ClockOn();
#endif
		   /* note we need to call both idles always!
		    * thus, the & must be & and not && so it
		    * won't short circuit!
		    */
    if(gMouseDown && m_fPerformingViewFile()) {
	ViewWindProc(&ViewWind, WM_MOUSEIDLE, 1,
		((DWORD)gMouseY<<24) | ((DWORD)gMouseX<<16));
    }
		   if (FileMgrIdle() & StartProgramsIdle() & AsynchUpdateTreeList())
		   {
		      GlobalIdle(); /* Tell the world we're idle */
		   }
		   /*
		    * We poll the time to see if it changed. But we
		    * don't really need to do it all the time, so
		    * we use "arbitrarycounter" to determine if we should
		    * check to see if we should update it. Assumes we
		    * are idle 64k times a minute
		    */
		   if (arbitrarycounter++ == 0)
			MessageBar(gpszNonSwap, isaMenu,FALSE);
#ifdef PROF
		   ClockOff();
#endif
	fPollKeyboard=TRUE;

}

WORD gCnx = cnxNull;
/*
 * Called by CW during menu and dialog idle time
 */
WORD FARPUBLIC RspAppIdle(WORD cnx,DWORD Lparam)
{
    if(cnx == cnxDialog)
    {
		PDLG    pdlg = (PDLG)LOWORD(Lparam);

		if (pdlg->pfnDlg != NULL)
		{
		     /* send dlmIdle to appropriate dialog proc */
		     (*pdlg->pfnDlg)(dlmIdle, 0, 0, 0, 0);
		}

    }
    gCnx = cnx;
     /*
      * do not idle when in a critcal dialog!
      */
     if( ErrorCrit == 0xFF)
     {
	MainIdle();
     }
     gCnx = cnxNull;
     return(rspContinue);
}

/*
 * we don't use the standard argv handling, so why
 * have it around?
 */
void cdecl _setargv(void)
{
}
#if 0
void cdecl _setenvp(void)
{
	static char * foo;
	foo = environ;
	
	unlink("foo");
}
#endif
/* WARNING
 * this is a replacement for the C call malloc. This SHOULD NEVER BE
 * CALLED IN YOUR CODE! This function is called at startup by
 * C-runtime to setup the environment. Since we don't want to carry
 * around a bunch of code we don't use, we just use our own allocation
 * Note that this memory is never freed! This is very important, since
 * the freework function can tromp
 * The real malloc sucks in about .5k of code
 */
void * cdecl malloc(unsigned int s)
{
	return(PbAllocWork(s));
}


BOOL gfEarlyExit = FALSE ;

extern Dos_Version(void);
void VersionCheck(void) 
{
	/* Perform version check!! We use the new "int 2f" issued by the BIOS
	 * to swap disks on a single floppy system -- "A:", "B:" on 1 physical drive
	 */
	if (Dos_Version() < MIN_MAJOR_VERSION)
	{
		Shell_TTY_Out(szIncorrectDosVersion) ;
		gfEarlyExit = TRUE ;
		DoExit() ;
	}
}

extern WORD GetLastScanCode(void);

/*
 * Looks for '!' signature in the ROM, followed 18 bytes later by 
 * '01' which is the ROM BIOS version number.  
 * Thus, this will only detect Tandy's with version 1.0 ROM.
 */
#define IsTandy1000 ( (*((BYTE FAR *)0xf000c000L) == 0x21) && (*((WORD FAR *)0xf000c012L) == 0x3130))

/*
 * The Tandy 1000 returns some strange keyboard messages, so we have to
 * trap them and change them to the correct messages.  GetLastScanCode
 * helps us determine what the correct message is, but it will return
 * useless values if a modifier key is changed, so we have to store the
 * last useful value in wSaveScanCode.
 */
VOID FAR PASCAL Tandy1000KeyboardHook(WORD message, WORD wParam, DWORD lParam)
{
    static WORD wSaveScanCode = 0;
    WORD wLastScanCode;

#if 0
    char buf[80];

    if(message == WM_CHAR)
	com1("\nWM_CHAR    ");
    else if(message == WM_KEYUP)
	com1("\nWM_KEYUP   ");
    else if(message == WM_KEYDOWN)
	com1("\nWM_KEYDOWN ");
    else {
	sprintf(buf, "\n%-10d ", message);
	com1(buf);
    }
    sprintf(buf, "%04x 0x%04x 0x%04x ", wParam, HIWORD(lParam),
	    GetLastScanCode()&0xff7f);
    com1(buf);
    if(message==WM_CHAR && !(wParam&0xff00)) {
	buf[0] = LOBYTE(wParam);
	buf[1] = '\0';
	com1(buf);
    }
#endif

    if(message == WM_CHAR) {
	switch(wParam) {
	case(VK_HOME):
	    wLastScanCode = GetLastScanCode()&0x7f;
	    if(wLastScanCode == 0x47 || wLastScanCode == 0x58)
		wSaveScanCode = wLastScanCode;
	    if(wSaveScanCode == 0x47)
		wParam = '\\';
	    break;

	case(VK_LEFT):
	    wLastScanCode = GetLastScanCode()&0x7f;
	    if(wLastScanCode == 0x4b || wLastScanCode == 0x2b)
		wSaveScanCode = wLastScanCode;
	    if(wSaveScanCode == 0x4b)
		wParam = '|';
	    break;

	case(VK_UP):
	    wLastScanCode = GetLastScanCode()&0x7f;
	    if(wLastScanCode == 0x48 || wLastScanCode == 0x29)
		wSaveScanCode = wLastScanCode;
	    if(wSaveScanCode == 0x48)
		wParam = '~';
	    break;

	case(VK_DOWN):
	    wLastScanCode = GetLastScanCode()&0x7f;
	    if(wLastScanCode == 0x50 || wLastScanCode == 0x4a)
		wSaveScanCode = wLastScanCode;
	    if(wSaveScanCode == 0x50)
		wParam = '`';
	    break;

	case('-'):
	    wLastScanCode = GetLastScanCode()&0x7f;
	    if(wLastScanCode == 0x58 || wLastScanCode == 0x0c)
		wSaveScanCode = wLastScanCode;
	    if(wSaveScanCode == 0x58)
		wParam = VK_HOME;
	    break;
	}
    }
    InsertKeyboardMessage(message, wParam, lParam);
}


#ifdef HASHHITTEST
char b1[80];
extern int ghashhits;
extern int ghashmisses;
extern int ghashnotpresent;
extern int gnohashnotpresent;
#endif

extern VOID Do_Read_Ini_File(void);

/*
**                              Main program
*/
void cdecl main(int argc, char *argv[])
{
	UnReferenced(argc) ;
	UnReferenced(argv) ;

	/* Get information on the country we're in... */
	GetInternationalData();

	/* In case we bail out because of low memory situations, we want to
	 * restore the screen to appropriate state before quitting in case we
	 * modified the screen mode.
	 */
	gfScreenModeChanged = FALSE;

	/* The following field is used to determine whether to bail out or not in
	 * a low memory situation. If we run out of memory when we are initializing
	 * we will bail out.
	 */
	gfStillInitializing = TRUE;

	/* used to de-install swap handler on exitting the shell */
	gfSwapHandlerInstalled = FALSE ;

	VersionCheck() ;

	SetUpStartUpDirectory();
	/*
	 *  parse the shell.ini file
	 */
	Do_Read_Ini_File();
	/* if we are returning from a program, we must pause before
	 * we initialize the screen so the user can read the output
	 * of the last program
	 */
	PauseBeforeScreenErase();
	if (!InitializeShell())
	    DoExit();

    //if(IsTandy1000) check is disfunctional, so we used an ini switch
	if(Get_KeyWord_Assignment(TK_SAVESTATE,TK_TANDY1000) == TK_ENABLED)
		HookKeyboardMessage(TRUE, Tandy1000KeyboardHook);

   /*  Allocate memory for the OutOfMemory Dialog Box HCAB structure now.
	* When we are actually, out of memory, we may not be able to allocate
	* enuf memory to put up the dialog box. This way, we are guaranteed to
	* atleast be able to say: "Out Of Memory" in a neat dialog box!
	*/
	if (!AllocateHcabForOutOfMem())
		DoExit();

	/* Set up the collating table for sorts to be done later! */
	SetCollatingTable() ;

	/* If we have to use the collating sort, set sort functions to the
	 * collating sorts!
	 */
	if (!FDoQuickCompare())
	{
		SortFnArr[SORT_NAME] = name_cmp ;
		SortFnArr[SORT_EXT]  = ext_cmp ;

		/* This is the variable that is used to call the sort function! Update
		 * it correctly.
		 */
		if (*SortCmp == quick_name_cmp)
		{
			*SortCmp = name_cmp ;
		}
		else if (*SortCmp == quick_ext_cmp)
		{
			*SortCmp = ext_cmp ;
		}
	}

	InstallSwapHandler() ;
	/*
	 * WARNING taskmaninit must happen first!
	 */
	DeleteBatchFile();

	gfSwapHandlerInstalled = TRUE ;
	gfStillInitializing = FALSE;

#ifdef HASHHITTEST
	ErrorCrit = 34; 
	itoa(ghashhits, b1, 10);
	strcat(b1, "\n");
	itoa(ghashmisses, b1+strlen(b1), 10);
	strcat(b1, "\n");
	itoa(gnohashnotpresent, b1+strlen(b1), 10);
	ShellMessageBox("HASH RESULTS", b1);
	ErrorCrit = 0xFF;
#endif  

	/*
	 *      Handle messages (keyboard,mouse)
	 */
	MainIdle();
	while (1)
	{
	     if (PeekMessage(&msg))
	     {
		   DispatchMessage(&msg);
	     }
	     else
	     {
			MainIdle();
	     }
	     fPollKeyboard=TRUE;

	}
 }
