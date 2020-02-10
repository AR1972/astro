;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <process.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <user.hs>
#include <user.sdm>
#include <text.h>
#include <tasklist.h>
#include <help.h>

int BatchOff;
char far *BatchFile;
extern VOID FarToNearsz(char *temp,char far *str,int max);
extern BOOL AddTask(char far *programname, char far *parameters,char far *defaulttitle,TOKEN properties,WORD magiccookie);
extern void PASCAL CabToVariable(HCAB, WORD, TOKEN, TOKEN);
extern BOOL PASCAL VariableToCab(TOKEN, TOKEN, HCAB, WORD);
extern void PASCAL VariableToString(TOKEN, TOKEN, char *, int);
extern TOKEN GetParamDialog(TOKEN list, int param);
extern BOOL gSwitchingEnabled;
extern WORD gEditBoxModifier;
extern BOOL gLoadEqualsProcessed;

#define MAXUSERTITLE 30
#define MAXUSERINSTRUCTIONS 106
#define MAXUSERPROMPT 18
#define MAXUSERDEFAULT 64
#define MAXUSERCOMMAND 256

#define MAXPARAM 10
#define MAXUSERPASSWORD 20
char userTitle[MAXUSERTITLE];
char userInstructions[MAXUSERINSTRUCTIONS];
char userPrompt[MAXUSERCOMMAND];
char userDefault[MAXUSERDEFAULT];
char userCommand[MAXUSERCOMMAND];
char userPassword[MAXUSERPASSWORD];
int gthisparam;
char userParam[MAXPARAM][MAXUSERDEFAULT];
BOOL gfUserCanceled;
BOOL FAR PASCAL FDlguser(WORD dlm, TMC tmc, WORD wNew, WORD wOld, WORD wParam)
{
	PWND twind ;
	int tlen, textlen ;
	int i, lastind  ;
	char tempstr[90] ;

	UnReferenced(tmc) ;
	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

    switch(dlm)
    {
	case    dlmInit:
	{
		 twind = PwndOfListbox(tmcinstructions1);
		 tlen = twind->arcWindow.axRight - twind->arcWindow.axLeft + 1 ;
		 textlen = strlen(userInstructions) ;
		 lastind = tlen ;
		 for (i = 0 ; i < min(tlen, textlen) ; i++)
		 {
			 if (userInstructions[i] == ' ')
				lastind = i ;
			 tempstr[i] = userInstructions[i] ;
		 }
		 if(i >= tlen)
		 {
			tempstr[lastind] = '\0' ;
			/* BUG BUG 1 could clip */
			Shell_SetTmcText(tmcinstructions2, &userInstructions[lastind+1]);
		 }
		 else
		 {
			tempstr[textlen] = 0;
		 }

		 Shell_SetTmcText(tmcinstructions1, tempstr);
		 Shell_SetTmcText(tmcprompt, userPrompt);
		 if(userTitle[0] == 0)
		 {
			 strncpy(userTitle,userCommand,MAXUSERTITLE-1);
			 userTitle[MAXUSERTITLE-1] = 0;
			 if(strlen(userTitle) >= MAXUSERTITLE-5) //ellipses
			 {
				userTitle[MAXUSERTITLE-5] = 0;
				strcat(userTitle,szEllipses);
			 }
		 }
	    SetUpDialog(tmcprompt,userTitle);
		 SetUpButtonForGraphics(tmcOK);
		 SetUpButtonForGraphics(tmcCancel);
		 SetUpButtonForGraphics(tmcuserHB);

		 SetUpEditBox(tmcuserquery, TRUE,MAXUSERCOMMAND, TRUE);

	}
	case dlmTerm:
	{
		GetTmcText(tmcuserquery,userPrompt,MAXUSERDEFAULT);
		strcpy(userParam[gthisparam],userPrompt);
	}
	break;
	case dlmSetFocus:
	{
		gCurrentTMC = tmc;
	}
	break;

	case dlmClick:
		 if(tmc == tmcuserHB)
			Help(hemDialog, hidUSER, NULL, 0);

		 SetFocusTmc(gCurrentTMC) ;
		 break ;
    }
    return(TRUE);
}

extern TOKEN gNext_Task_Info;
extern char gNext_Task_Name[];

void DoUserDialog(void)
{
    HCABuser h;
    TOKEN thistoken;
    int thisparam;
    BOOL bSaveDef = FALSE;

    if(gfUserCanceled)
	return;

    if(!(h=HcabAlloc(cabiCABuser))) {
	OutOfMemory() ;
	return ;
    }
    InitCab(h, cabiCABuser);

    if(gNext_Task_Info > 0) {
	thisparam = *Get_Token_Identifier(gLookahead) - '0';
	if((thistoken=GetParamDialog(gNext_Task_Info, thisparam)) > 0) {
	    VariableToString(thistoken, TK_TITLE, userTitle, MAXUSERTITLE);
	    VariableToString(thistoken, TK_INFO,
		    userInstructions, MAXUSERINSTRUCTIONS);
	    VariableToString(thistoken, TK_PROMPT, userPrompt, MAXUSERCOMMAND);
	    VariableToString(thistoken, TK_DEFAULT, userDefault,MAXUSERDEFAULT);

/* INTERNATIONALIZE here: 'F' and 'L' in the switch below */
	    if(userDefault[0]=='%' && !userDefault[2]) {
		switch(toupper(userDefault[1])) {
		case 'F':
/* This inserts the File that has the focus in the file list
 * into the dialog box as a default value
 */
		    if(glob.InFileMgr)
#if 0
			FormSelFileList(userDefault, MAXUSERDEFAULT-13);
#else
			ListProcFileList(tmmGetItemString, userDefault,
				FileList[0].focusitem, 0, 0, 0, 0);
#endif
		    else
			*userDefault = '\0';
		    break;

		case 'L':
/* This inserts the Last string that was used as a parameter
 * for this dialog box
 */
		    VariableToString(thistoken, TK_STARTUP,
			    userDefault, MAXUSERDEFAULT);
		    bSaveDef = TRUE;
		    break;
		}
	    }

#define ALLMODS (KK_ALT|KK_SHIFT|KK_CONTROL)
#define SKIPBOXMODS (KK_SHIFT|KK_CONTROL)
	    if((!gLoadEqualsProcessed && !GET_WAIT_FLAG()) ||
		    (gEditBoxModifier&ALLMODS)==SKIPBOXMODS) {
/* Accept default without prompt if SHIFT and CONTROL down
 * or if we are doing initial loading
 */
		strcpy(userPrompt, userDefault);
		strcpy(userParam[thisparam], userPrompt);
		goto DoAppend;
	    }

	    SzToCab(h, userDefault, Iag(CABuser, pszuserquery));
	}
    }

    SzToCab(h, szEnterButton, Iag(CABuser, pszuserEB));
    SzToCab(h, szCancelButton, Iag(CABuser, pszuserCB));
    SzToCab(h, szHelpButton, Iag(CABuser, pszuserHB));

    if(MyTmcDoDlg(&dlguser,h) == tmcOK) {
DoAppend:
	Append_Command(userPrompt);
	if(bSaveDef)
	    CabToVariable(h, Iag(CABuser, pszuserquery), thistoken, TK_STARTUP);
    } else
	gfUserCanceled = TRUE;

    FreeCab(h);
}

VOID Append_Command(char far *str)
{
    int i;
    i = 0;
    while(str[i])
    {
	BatchFile[BatchOff] = str[i];
	i++;
	BatchOff++;
    }
    BatchFile[BatchOff] = '\0';
}

VOID GetDialogParam()
{
    switch(gLookahead)
    {
	case TK_T:
	{
	    match(gLookahead);
	    FarToNearsz(userTitle,Get_Token_Identifier(gLookahead),MAXUSERTITLE);
	    match(gLookahead);

	}
	break;
	case TK_P:
	{
	    match(gLookahead);
	    FarToNearsz(userPrompt,Get_Token_Identifier(gLookahead),MAXUSERCOMMAND);
	    match(gLookahead);
	}
	break;
	case TK_D:
	{
	    match(gLookahead);
	    FarToNearsz(userDefault,Get_Token_Identifier(gLookahead),MAXUSERDEFAULT);
	    match(gLookahead);

	}
	break;
	case TK_R:
	case TK_L:
	case TK_M:
	case TK_C:
	break;
    }
}

VOID UserDialog()
{
    userTitle[0] = NULL;
    userInstructions[0] = NULL;
    userPrompt[0] = NULL;
    userDefault[0] = NULL;

    while((gLookahead != TK_EOF) && (gLookahead != TK_RIGHTBRACKET))
    {
	switch (gLookahead)
	{
	    case TK_SLASH:
	    {
		match(gLookahead);
		GetDialogParam();
	    }
	    break;
	    default:
		match(gLookahead);
	}
    }
    match(TK_RIGHTBRACKET);
    DoUserDialog();
}

extern int gNumWhiteSpaces;
extern BOOL gfInRun;

VOID GetNextCmd()
{
	 int param;
	 char temp[3]; 
	 int i;

    switch (gLookahead)
    {
		case TK_EOF:
	   break;
		case TK_COMMANDSEP:
		{
			Append_Command(szNewCmd);
			Append_Command(szStartCmd);
			match(gLookahead);
		}
		break;
		case TK_LEFTBRACKET:
		{
			//Append_Command("\r\n");
			/* if we are in the run dialog box,
			 * we should ignore the language stuff
			 */
			if(gfInRun)
			{
				goto defaultcase;
			}
			else
			{
				match(gLookahead);
				UserDialog();
			}
		}
		break;
		case TK_VARSENTINEL:
			match(gLookahead); // %
			if(gLookahead != TK_EOF)
			{
				FarToNearsz(temp,Get_Token_Identifier(gLookahead),2);
				param = temp[0] - '0';
			}
			else
			{
				param = 0;
			}
			if((param > 0) &&(param < 10))
			{
				gthisparam = param;
				/* if this param has already been prompted for */
				if(userParam[param][0])
					Append_Command(userParam[param]);
				else
				   DoUserDialog();
				match(gLookahead); // number
			}
			else
			{
				Append_Command("%");
			}
			for(i=0;i<gNumWhiteSpaces;i++)
				Append_Command(" ");

		break;


		default:
		{
defaultcase:
			Append_Command(Get_Token_Identifier(gLookahead));

			match(TK_WILD);
			for(i=0;i<gNumWhiteSpaces;i++)
				Append_Command(" ");

		}
		break;
    };

}


/*  NAME
 *      ParseCmds
 *  DESCRIPTION
 *      start parsing the command
 *  ARGUMENTS
 *      none
 *  RETURN VALUE
 *      none
 *  EFFECTS
 *      memory allocation, new symbols, the works
 *  STATE
 *  COMMENTS
 *  WARNINGS
 *  HACKS
 */
void ParseCmds(char far *commands)
{
    WORD size;
	 int i;
	
    BatchOff = 0;
    BatchFile[0] = 0;
    size = myfstrlen(commands);

    Init_ParseText(0, commands, 0, size);

	 gthisparam = 0;
	 for(i=0;i<MAXPARAM;i++)
		userParam[i][0] = 0;
    /*
     * now we use @ on every line
     * Append_Command("@echo off");
     */
    Append_Command(szStartCmd);
    /*
     * the lexer needs to be kick started by matching TK_WILD token
     */
    match(TK_WILD);
    /*
     * keep parsing until EOF is reached
     */
    while(gLookahead != TK_EOF)
    {
	GetNextCmd();
    };
}

#define BATCHSIZE 512

extern char gStartUpDir[65];
extern int gStartUpDirEnd; /* location where the NULL goes in the above name */

extern void Set_Task_Name(char far *name);
extern void Set_Task_Info(TOKEN list);


/*
 * fill in name with name of command batch file for this process
 * magiccookie is a random number used as a prefix to the batchfile.
 * this number can be used as a global identifier for the batchfile.
 * (We use it in the global structure to identify the batch file
 * on a process after it exits.)
 */
VOID GetBatchName(char *name,int *magiccookie)
{
		int ind;
		static BYTE magiccounter = 0;

		get_temp_dir(name,TRUE);
		ind = strlen(name) ;
#ifdef DBCS
		if (name[ind-1] != '\\' || CheckDBCSTailByte(name,&name[ind-1]))
#else
		if (name[ind-1] != '\\')
#endif
		{
		    name[ind++] = '\\' ;
		}
		
		if(!*magiccookie) // if cookie wasnt passed in, make one
		{
			/* if tasking is not being done, we use PID since
			 * it will be unique, and we can retrieve it later!
			 */
			if(!gSwitchingEnabled)
			{
				*magiccookie = getpid();
			}
			else
			{
				/* The magic cookie should be unique, as
				* we use it to delete the file later (deletebatchfile)
				* You cannot only use ClockTicks, as two batches could 
				* be made in that about of time(load=), so we use a counter 
				* also. Note that the counter alone also won't work, as
				* multiple shells could be running.
				*/
				*magiccookie = (int) (ClockTicks()+magiccounter++) ;
			}
		}
		itoa((int) *magiccookie,name+ind,16);
		strcat(name,szCmdBat);
}

/*
 * Delete the batch files
 */
VOID DeleteBatchFile(void)
{
	char name[91];
	Switch_Info far *switchdata;
	int i;
	int magiccookie;
	int ind;

	if(gSwitchingEnabled)
	{
		switchdata = C_GET_GLOBAL_SWITCH_DATA();
		for(i=0;i<MAX_NUM_PROGRAMS;i++)
		{
			/* If its a free entry, and has a cookie.. */
			if((magiccookie=switchdata->Program_list[i].Shell_Cookie) &&
				(switchdata->Program_list[i].Program_Flags & F_FREE))
			{

				get_temp_dir(name,TRUE);
				ind = strlen(name) ;
#ifdef DBCS
				if (name[ind-1] != '\\' || CheckDBCSTailByte(name,&name[ind-1]))
#else
				if (name[ind-1] != '\\')
#endif
				{
					name[ind++] = '\\' ;
				}
				itoa(magiccookie,name+ind,16);
				strcat(name,szCmdBat);  
				 _dos_setfileattr(name, 0); /* remove any read-only attrib on file */
				unlink(name);

			//switchdata->Program_list[i].Shell_Cookie = 0;
			}
		}
	}                                      
	else
	{
		magiccookie = getpid();
		get_temp_dir(name,TRUE);
		ind = strlen(name) ;
#ifdef DBCS
		if (name[ind-1] != '\\' || CheckDBCSTailByte(name,&name[ind-1]))
#else
		if (name[ind-1] != '\\')
#endif
		{
			name[ind++] = '\\' ;
		}
		itoa(magiccookie,name+ind,16);
		strcat(name,szCmdBat);  
		unlink(name);

	}
}

VOID GetCommandString(char *commandstring,char *parameters,char *batname,int *magiccookie)
{
		if(commandstring) //!NULL
		get_comspec(commandstring);
		strcpy(parameters,szCmdBatRun);

		GetBatchName(batname,magiccookie);      
		strcat(parameters,batname);
	 
}

BOOL SetupCommand(char far *commands, char *startdir,BOOL dolaunch,TOKEN info)
{
   int fhandle;
   int size;
   char commandcom[128];
   char batname[91];
   char commandstring[256];
	int magiccookie;
	int ret ;
	int action ;
	char *tempcaption ;



   /* commands == NULL implies we want to launch command.com by itself! */
   if(commands!= NULL)
   {
	  
		BatchFile = LpbAllocWorkFar(BATCHSIZE);
		if (!BatchFile)
	      return(FALSE);

		gfUserCanceled = FALSE;
		ParseCmds(commands);
/* We didn't really change the ini file here, just made some temp symbols */
		if(gfUserCanceled)
			return(FALSE);

		magiccookie = 0; // we need a new unique cookie
		GetCommandString(commandcom,commandstring,batname,&magiccookie);
		
		do
		{
			action = ACT_OK ;

			if ((_dos_creat(batname,_A_NORMAL,&fhandle)) != 0)
			{
				_dos_setfileattr(batname, 0); /* remove any read-only attrib */
				if ((ret = _dos_creat(batname,_A_NORMAL,&fhandle)) != 0)
				{
					/* Put the drive letter in the message */
					szErrCreatTmpFile[DRIVEOFFSET_szErrCreatTmpFile] = batname[0];

					tempcaption = gpszFileOpCaption ;
					gpszFileOpCaption = szErrorCaption ;
					action = DOSErrorBox(szErrCreatTmpFile, ret,
																		HELP_ERRCREATEDEST) ;
					gpszFileOpCaption = tempcaption ;
				}
			}
		} while (action == ACT_RETRY) ;

		if (action != ACT_OK)
			return(FALSE) ;

		_dos_write(fhandle,BatchFile,myfstrlen(BatchFile),&size);
		_dos_close(fhandle);
		FreeWorkFar(BatchFile);
		if(dolaunch)
		{
			if (startdir && (startdir[0]))
				UnixChdir(startdir) ;
			LaunchProgram(commandcom,commandstring,magiccookie);
		}
		else
		{
			if(gSwitchingEnabled)
			{
				AddTask(commandcom,commandstring,gNext_Task_Name,info,magiccookie);
			}
		}
   }
   else
   {
		/* Launch command.com!! -- We don't want  to prompt user on
		* exiting command.com, so set global wait state to disabled -- Will be
		* written out in DOSSHELL.INI file!
		*/
	get_comspec(commandcom);

		Set_KeyWord_Assignment(TK_SAVESTATE,TK_PAUSE,TK_DISABLED);

		/* place the user in correct startup directory before launching his prog */
		if (startdir && (startdir[0]))
			UnixChdir(startdir) ;
	Set_Task_Name(szCommand); 
		Set_Task_Info(TK_NOTHING);
		LaunchProgram(commandcom,"",0);
   }
	return(TRUE);
}

/* User is placed in startdir if startdir is non-NULL, else the start up
 * directory has already been set by the caller
 */
extern WORD gEditBoxModifier;
extern ListBoxData TaskList;

VOID DoCommand(char far *commands, char *startdir)
{
		/* hack for Ericst; don't actually launch if the shift key
       * is held down..
       */
		if((gSwitchingEnabled) && (gEditBoxModifier & KK_SHIFT))
		{
			SetupCommand(commands, startdir, FALSE,gNext_Task_Info);
		   gNext_Task_Info=TK_NOTHING; //be sure
			InsertListItem(&TaskList,0);
		}
		else
			SetupCommand(commands, startdir, TRUE ,TK_NOTHING);

}

VOID DoUserCommand(char *startdir)
{
	DoCommand(userCommand, startdir);
}
