/* Revision history since 5.0 Golden:
 *
 *  M010 SHK 08/13/91 		User can now control primary/secondary swap
 *							 		paths using the 'swapdisk' DOSSHELL.INI var.
 *							 		If this is not set, the DOSSHELL env variabl
 *							 		is used, if set. Else default 50 behaviour.
 *							 		This change was prompted by ROMDOS -- dir
 *							 		where DOSSHELL is residing might not be 
 *							 		write-able! Added fn SetUpSecondarySwapPath().
 */


;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*
 * This module holds the bulk of the code for the task list
 */

/* WARNING this is a hardcoded number for the number of bytes in the SFT
 */
#define DOS5SFTSIZE 0x3B

#include <common.h>
#include <filemgr.h>
#include <text.h>
#include <menus.h>
#include <prot.h>
#include <warn.hs>
#include <warn.sdm>
#include <tasklist.h>

extern char gStartUpDir[];
extern char gStartInDir[];
extern int gStartUpDirEnd; /* location where the NULL goes in the above name */
extern BYTE StartupNumLines; // number of screen lines at startup

extern BOOL VerifyTempPath(char *path) ;
VOID FarToNearsz(char *temp,char far *str,int max);
void strfcpy(char far *dst, char far *src);
VOID GetCommandString(char *commandstring,char *parameters,char *batname,int *magiccookie);
WORD DetectProcessor(void);
VOID InitGlobalSwitchData(BOOL setorgetscreenmode);

extern VOID FirstMouseInit(void);

extern WORD LinesToMode(WORD lines);
extern WORD ReturnScreenMode;
extern unsigned char far cdecl GET_WAIT_FLAG(void);

ListBoxData TaskList;

TOKEN gNext_Task_Info = TK_NOTHING;
char gNext_Task_Name[MAX_TITLE+2];

BOOL gTaskListEnabled = TRUE; 
BOOL gSwitchingEnabled = TRUE; 
BOOL gAlreadyTasking;

BOOL gfOurISR9Installed = FALSE ;

void DisableTaskList(void)
{ 
	if(gTaskListEnabled && TaskList.hasglobalfocus)
	{
		if(C_GET_LIST_LENGTH() <= 0)  //zero based, one is shell
				PrevGlobalFocus();  // go back to applist
	}

	gTaskListEnabled = FALSE; 
	if(glob.InFileMgr)
		DoFileMgr();
	else
	   InitializeStartPrograms();

   Set_KeyWord_Assignment(TK_SAVESTATE,TK_TASKLIST,TK_DISABLED);
}

void EnableTaskList(void)
{
	gTaskListEnabled = TRUE;
	C_INIT_PROGRAM_LIST();

	   
	InitGlobalSwitchData(TRUE);


	if(glob.InFileMgr)
		DoFileMgr();
	else
	   InitializeStartPrograms();
   Set_KeyWord_Assignment(TK_SAVESTATE,TK_TASKLIST,TK_ENABLED);
}
/* 
 * This function toggles the state of switcher enableing 
 */
void EnableSwitching(void)
{
   if(gSwitchingEnabled = !gSwitchingEnabled)
	{
		EnableTaskList();
	Set_KeyWord_Assignment(TK_SAVESTATE,TK_SWITCHING,TK_ENABLED);
	}
	else
	{
		DisableTaskList();
	Set_KeyWord_Assignment(TK_SAVESTATE,TK_SWITCHING,TK_DISABLED);
	}
}

#if 0
// no longer used
void DisableSwitching(void)
{
   gSwitchingEnabled = FALSE;
   Set_KeyWord_Assignment(TK_SAVESTATE,TK_SWITCHING,TK_DISABLED);
}
#endif

void Set_Task_Info(TOKEN list)
{
	 gNext_Task_Info = list;
}
void Set_Task_Name(char far *name)
{
	if(name)
		FarToNearsz(gNext_Task_Name,name,MAX_TITLE);
	else
		gNext_Task_Name[0] = 0;
}

VOID ReInitTaskParams(void)
{
	char restartparameters[70];
	char batchname[70];
	int magiccookie;

	Switch_Entry far * SE;

	SE = (Switch_Entry far *) C_GET_ITH(0);
   if(SE->Program_Flags & FSHELL)
	{
		SE = (Switch_Entry far *) C_GET_ITH(1);   
	}
	
	magiccookie = SE->Shell_Cookie; //near!
	GetCommandString(NULL,restartparameters,batchname,&magiccookie);
	C_ADD_PARAMS(restartparameters,strlen(restartparameters));

}



VOID Switch_To_Ith_Task(int isz)
{
	Switch_Entry far * SE;
	int i;

	SE = (Switch_Entry far *) C_GET_ITH(0);
	if(SE->Program_Flags & FSHELL)
	{
		C_GO_NEXT();
	}        

	for(i=0;i<isz;i++)
	{
		 C_GO_NEXT();                    
		 SE = (Switch_Entry far *) C_GET_ITH(0);
		 if(SE->Program_Flags & FSHELL)
		 {
				C_GO_NEXT();
		 }       

	}
	C_GO_Z_NEXT(); /* Lauch will undo this! BUG BUG from hell */
	LaunchProgram(NullString,NullString,0);
}

WORD PASCAL ListProcTaskList(WORD tmm, char *sz, WORD isz, TMC tmc,WORD x, WORD y, WORD bArg)
{
	RX xval;
	char temp[256];
	Switch_Entry far * SE;
	int i;
	int aftershell;
	int num;
	int len;

	UnReferenced(sz);
	UnReferenced(tmc);
	UnReferenced(x);
	UnReferenced(bArg);

	xval = Get_List_Rect(&TaskList).axLeft;
	switch (tmm) {
		case tmmCount:
			if(gTaskListEnabled)
			{
			   
	   len = C_GET_LIST_LENGTH()+1-1; //zero based, but one is shell

			  if(TaskList.hasglobalfocus && (len == 0))
				PrevGlobalFocus(); //*
			  return(len);
			}
			else
			{
				return(0);
			}
		break;
		case tmmSetFocus:
			 break;
		case tmmSelect:
			 break;
		case tmmActivate:
			 if(isz < (num=C_GET_LIST_LENGTH())+1-1)
			 {
					Switch_To_Ith_Task(isz);
			 }
			 break;
		case tmmDrawItem:
	  case tmmGetItemString:
		{
			  if(isz < C_GET_LIST_LENGTH()+1-1) // zero base, one is shell
			  {

				  /* we need to be sure we skip the dosshell, so we walk
				   * list to see if it was before or after in the list
				   * so we will be sure to skip it!
					*/
					aftershell = 0;
					for(i=0;i<=isz;i++)
					{
						 SE = (Switch_Entry far *) C_GET_ITH(i);
						 if(SE->Program_Flags & FSHELL)
						 {
							 ++aftershell;
							 break;
						 }
					}

					SE = (Switch_Entry far *) C_GET_ITH(isz+aftershell);
					fstrncpy((char far *)temp, SE->Program_Title,MAX_TITLE);
					len = strlen(temp); // get length of title; 
					if(SE->HK_Scan_Code_2) // there is a hot key
					{
						temp[len] = ' ';
					   len = strlen(temp); // get length of title; hot key follows

					}
				   if(tmm == tmmGetItemString)
				{
						strcpy(sz,temp);
						return(TRUE);
					}

					for(i=strlen(temp);i<Get_List_Rect(&TaskList).axRight - Get_List_Rect(&TaskList).axLeft - 4;i++)
						temp[i] = ' ';
					temp[i] =0;

					TextOut(TaskList.pwd, (RX) xval+2,(RY) y,temp,len, bArg);
					TextOut(TaskList.pwd, (RX) xval+2+len,(RY) y,&temp[len],-1, isaBackground);

				   DrawFocusMarker(&MainWind, (RX)xval+1+2*gisgraph, (RY)y, (RX)xval+2, (RY)y, len, bArg&TF_ISFOCUS,
													FALSE, isaBackground) ;

			  }
				else
				{
					for(i=0;i<Get_List_Rect(&TaskList).axRight - Get_List_Rect(&TaskList).axLeft - 3;i++)
						temp[i] = ' ';
					temp[i] =0;
					TextOut(TaskList.pwd, (RX) xval+1,(RY) y,temp,-1, isaBackground);
				}
		}
			break;
		default:
			break;
	}
	return TRUE;
}
BOOL TaskManIdle(void)
{
	   return(ListBoxIdle(&TaskList));
}

BOOL TaskManKey(WORD wParam,DWORD LParam)
{
	if(TaskList.hasglobalfocus)
	{
	   ListKey(&TaskList,wParam,HIWORD(LParam));
	   return(TRUE);
	}
	return(FALSE);
}

BOOL TaskManMouse(WORD mx,WORD my,WORD message,BYTE state)
{
	return(ListMouse(&TaskList,mx,my,message,state));
}

/* M010 -- added this function to support user control of the
 * primary/secondary swap paths thru the INI file/DOSSHELL
 * environment variable.
 */

/* The swappath is expected to have a '\\' at its end. */
void SetUpSecondarySwapPath(char far *swappath, char *default_path)
{
	TOKEN swapdisk_tok ;
	char buffer[257] ;
	char *p ;
	int len ;

	swappath[0] = '\0' ;

	swapdisk_tok = Get_KeyWord_Assignment(TK_SAVESTATE, TK_SWAPDISK) ;
	if (swapdisk_tok >= 0)
	{
		/* copy from far memory to near memory and NULL terminate it
		 * so that we can use C-functions like strtok(), etc.
		 */
		strfncpy(buffer, Get_Token_Identifier(swapdisk_tok), 256) ;
		buffer[256] = '\0' ; /* Null terminate, just in case */

		/* Note that swapdisk variable in the INI file can have two paths
		 * in it -- the primary swap path followed by the secondary path.
		 * We are interested in the optional second path!
		 */
		if (strtok(buffer, " \t"))
		{
			if ( (p = strtok(NULL, " \t")) && VerifyTempPath(p) )
				strfcpy(swappath, p) ;
		}

	}

	/* if the path has not been set yet, look at the DOSSHELL env variable! */
	if (swappath[0] == '\0')
	{
		/* See if DOSSHELL is set to a valid dir! */
		p = getenv(szIniFileEnvVar);
		if (p && VerifyTempPath(p) )
				strfcpy(swappath, p);
	}

	/* if the path is still not set, use the default value passed in! */
	if (swappath[0] == '\0')
	{
		strfcpy(swappath, default_path) ;
	}

	/* Make sure that we put a terminating '\\' as expected by the caller */
	len = myfstrlen(swappath) ;

	if (swappath[len-1] != '\\')
	{
	   swappath[len++] = '\\' ;
		swappath[len] = '\0';
	}

} /* SetUpSecondarySwapPath */


VOID InitGlobalSwitchData(BOOL setorgetscreenmode)
{
	char tempdir[256];
	int ind;
	TOKEN minpath;  
	char far *minpathident;
	TOKEN grabdir;

	Switch_Info far * Switchdata;
	Switchdata = C_GET_GLOBAL_SWITCH_DATA();

	ind = gStartUpDirEnd ;
	strcpy(tempdir,gStartUpDir);

#ifdef DBCS
	if (tempdir[ind-1] != '\\' || CheckDBCSTailByte(tempdir,&tempdir[ind-1]))
#else
	if (tempdir[ind-1] != '\\')
#endif
	{
	   tempdir[ind++] = '\\' ;
		tempdir[ind] = 0;
	}

	/* Now set up secondary swap path -- Note that we expect it to have
	 * a '\\' char at its end.
	 */
	SetUpSecondarySwapPath(Switchdata->Swap_Path2, tempdir) ; /* M010 */

	/* tell switcher where the grabber is */
	/* WARNING tempdir is trashed after setting up grabber! */
	grabdir = Get_KeyWord_Assignment(TK_SAVESTATE,TK_VIDEODIR);
	if(grabdir > 0)
	{
		strfcpy(tempdir,Get_Token_Identifier(grabdir));
		ind = strlen(tempdir);
		if (tempdir[ind-1] != '\\')
		{
			tempdir[ind++] = '\\' ;
			tempdir[ind] = 0;
		}
	}
	strcpy(tempdir+ind,szGrabberName);
	strfcpy(Switchdata->Grabber_Name,tempdir);
	/* WARNING tempdir may not be correct now! */

	/* tell switcher were primary swap path is */
   get_temp_dir(tempdir,TRUE); /* get TMP= path */
	ind = strlen(tempdir) ;
#ifdef DBCS
	if (tempdir[ind-1] != '\\' || CheckDBCSTailByte(tempdir,&tempdir[ind-1]))
#else
	if (tempdir[ind-1] != '\\')
#endif
   {
		tempdir[ind++] = '\\' ;
	   tempdir[ind] = 0;
	}
	strfcpy(Switchdata->Swap_Path1,tempdir);

	minpath = Get_KeyWord_Assignment(TK_SAVESTATE,TK_RESERVETEMP);
	if(minpath >0) //if user has specified this variable
	{
		minpathident = Get_Token_Identifier(minpath);
		fstrncpy(tempdir,minpathident,12);              
		Switchdata->Min_Path1 = atoi(tempdir);
	}
	else
	{
		/* use up all of primary swap path before reverting to secondary */
		Switchdata->Min_Path1 = 0;
	}
	/* detect CPU */
	Switchdata->CPU_Type = DetectProcessor();
	/* always allow use of all of secondary swap path */
	Switchdata->Min_Path2 = 0;
	Switchdata->SFT_Size = DOS5SFTSIZE;
	if(setorgetscreenmode)
	{
		Switchdata->Num_Lines = StartupNumLines; 
		//return screenmode is correct
	}
	else
	{
		StartupNumLines = Switchdata->Num_Lines; 
		//returnscreenmode needs adjusting
		ReturnScreenMode =  LinesToMode(StartupNumLines);
	}
}

VOID RefreshTaskMan(VOID)
{
		if(gTaskListEnabled)
		{
	//DoRedisplayList(&TaskList);
			InsertListItem(&TaskList,0);
	UpdateListBox(&TaskList);
		}
}


VOID InitTaskMan(WORD top,WORD left,WORD bottom,WORD right)
{
   ListBoxInit(&TaskList,ListProcTaskList,&MainWind,top,left,bottom,right,
																szActiveTaskList,666,0,0);
}

extern WORD gCnx;
extern WORD gScanInfo;
extern WORD GetLastScanCode(void);
/* If the hotkey is handled, then we don't return from this
 * routine at all, but rather go to the appropriate app
 */
extern BOOL gInDialog;
VOID FAR PASCAL HotKeyCheck(WORD message,WORD wParam,DWORD lParam)
{
	Switch_Entry far * SE;
	int numtasks;
	int i;
	WORD scaninfo;
	int wasshell;

	/* if no dialog is popped up, see if hot key was pressed */
	if(gInDialog <= 0)
	{
	scaninfo = GetLastScanCode() & 0xFFF;
		if(gSwitchingEnabled)
		{
			wasshell = 0;
			numtasks = C_GET_LIST_LENGTH(); // zero based, one is shell
			for(i=0;i<=numtasks;i++)
			{
				SE = (Switch_Entry far *) C_GET_ITH(i);
		if(SE->Program_Flags & FSHELL)
				{
					wasshell = 1;
				}
				else
				{
					if( (SE->HK_Shift_State==HIBYTE(scaninfo) ) && 
				(SE->HK_Scan_Code_2 == LOBYTE(scaninfo)) )
					{
						/*user hit hotkey for this app, switch to it! */
						Switch_To_Ith_Task(i-wasshell); 
					}
				}
			}
		}
	}
	InsertKeyboardMessage(message,wParam,lParam);
}

VOID RestartTask(void)
{
	ReInitTaskParams();
	Switch_To_Ith_Task(0);
}

VOID HookISR9(void);

/*
 * FirstInitTaskMan initializes all global task manager variables,
 * Also, it checks for errors from the last switch out, and puts
 * up dialog with appropriate message before continuing on.
 * Finally, In the event the error code indicates that the program
 * had not been run yet, we run that program. This case happens
 * when there are programs with load= set and the user has
 * hot-keyed to that program from within the switcher. 
 */
VOID FirstInitTaskMan(void)
{
	char szErrorMessage[150];
	WORD ec;
	RRC rrcClient;
	

	gAlreadyTasking = RunningUnderMStasker();
   if((!gAlreadyTasking) && (Get_KeyWord_Assignment(TK_SAVESTATE,TK_SWITCHING) == TK_ENABLED))
	{
		gSwitchingEnabled = TRUE;
		gTaskListEnabled = TRUE;
		/* only clear the program list on first initialization if
		 * tasking is set; otherwise we just swapped back to shell
		 */
		if(!GET_WAIT_FLAG())
		{
			/* returnscreenmode is correct from startup */
			C_INIT_PROGRAM_LIST();
			InitGlobalSwitchData(TRUE);
		}
		else
		{
			/* returnscreenmode is not correct, get global */
			InitGlobalSwitchData(FALSE);
		}
		
	}
	else
	{
		/* returnscreenmode is correct from startup */
		gSwitchingEnabled = FALSE;
		gTaskListEnabled = FALSE;
	}
	/* if the last program launch to switcher failed, put up a
	* dialog box saying why.
	*/
	if(gSwitchingEnabled) /* we think switcher is running */
	{
#ifndef NOLOADER
		if (GET_WAIT_FLAG())    /* this is not the first run */
#endif
		{
/*BUG BUG set the contexts? */
			ec = C_GET_EXITCODE();
			if(ec) /* there was an error .... */
			{
			   /* This next means the task was not started before
			* hot-keyed too; we need to start it now. It is already
				 * the first task in the list
				*/
				if((ec>>8) == ER_APP_NOT_STARTED_YET)
				{
					RestartTask();
					//no fall through!              
				}
				C_DELETE_PROGRAM(0);/* take bogus program from list */

				/* We are going to put up an error message dialog box up.
				 * The screen has not been initialized as yet and so, in
				 * graphics mode, if we put up the error dialog and then take
				 * it down, the region behind the dialog will have garbage --
				 * This is soon cleared up but it looks ugly.
				 * So, we, now, clear the screen before putting up the
				 * dialog. Also, we need to initialize the mouse now as it
				 * has not yet been initialized.
				 */
				GetClientRrc(&MainWind,&rrcClient);
				FillRrc(&MainWind,&rrcClient,' ',isaBackground);
				FirstMouseInit();
				FInitMouseNest();
				switch(ec>>8)
				{
					case ER_NO_MEMORY:         
					case ER_NO_LIST_MEMORY: 
					case ER_LOW_MEMORY:        
					ShellMessageBox(szSwitchErrTitle, szSwitchLowMem1);
						break;

					case ER_NO_XMS_DRV:        
					case ER_LOW_XMS_MEM:       
					ShellMessageBox(szSwitchErrTitle, szSwitchLowXMS1);
						break;
					case ER_GRABBER_LOAD:           
						FormStringWithoutPlaceHolders(szErrorMessage,
				szShellFileNotFound, (char far *)szGrabberName);
						ShellMessageBox(szSwitchErrTitle, szErrorMessage);
						break;

					case ER_WND_SWAP_IN:       
					case ER_APP_SWAP_OUT:           
					case ER_APP_SWAP_IN:            

					case ER_EXEC_FAILS:        
						ShellMessageBox(szSwitchErrTitle, szSwitchFailed);
						break;
					case ER_LIST_SWAP:         
					case ER_WND_SWAP_OUT:      
					case ER_RELOCATE:        
					case ER_WINOLDAP_ACTIVE:
					case ER_BAD_INIT_DIR:      
					case ER_COMM_NOLOAD:     
					default:
						ShellMessageBox(szSwitchErrTitle, szSwitchGeneral);
						break;
				}
			}
		}
	}
	HookKeyboardMessage(TRUE,HotKeyCheck);
	HookISR9();
	gfOurISR9Installed = TRUE ; // By default initialized to FALSE!

	/* We must have the TaskList initialized because calls will go to it
	* to determine the number of items in the task list, even when the
	* task list is not on the screen. Here, we just initialize with 
	* some random values -- all we really want to get set is the task lists
	* function pointer!
	*/
   ListBoxInit(&TaskList,ListProcTaskList,&MainWind,0,0,1,1,NullString,666,0,0);
	Halt_Listbox(&TaskList);
	TaskList.hasglobalfocus = FALSE; //WARNING must do this
}

BOOL FAR PASCAL FDlgwarn(WORD dlm, TMC tmc, WORD wNew, WORD wOld, WORD wParam)
{
	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	switch(dlm) {
	case dlmInit:
	SetUpDialog(tmcOK, szCritWarning);
	SetUpButtonForGraphics(tmcOK);
	SetUpButtonForGraphics(tmcCancel);

	Shell_SetTmcText(tmcwarnline1, szWarnLine1);
	Shell_SetTmcText(tmcwarnline2, szWarnLine2);
	Shell_SetTmcText(tmcwarnline3, szWarnLine3);
	break;

	case dlmSetFocus:
	gCurrentTMC = tmc;
	break;
	}
	return(TRUE);
}

/* WARNING this generates a name created by the dosswap.exe
 * If that code changes this will be incorrect!
 * BUG BUG
 */
VOID MakeSwapName(char *name,BYTE Switcher_Id,WORD Entry_Index)
{
		strcpy(name,"~DOS");
	   if((name[4] = Switcher_Id + '0')> '9')
			name[4]+= 7;
	   name[5] = '0';
	   name[6] = '0';
	   if((name[7] = (char) (Entry_Index + '0'))> '9')
			name[7]+= 7;
	   name[8] = 0;
		strcat(name,".TMP");            
}

extern VOID DeleteBatchFile(void);
extern BOOL InitXMS(void);
extern WORD FreeXMSHandle(WORD handle);

VOID DeleteTask(void)
{

	HCABwarn h;
	Switch_Entry far * SE;
	int numtasks;
	int i;
	int wasshell;
	int delindex;
	char swapfile[67];
	char name[13];
	Switch_Info far * Switchdata;

	if(C_GET_LIST_LENGTH() <= 0) //zero based
	{
		Shell_Beep();
		return;
	}
   h = HcabAlloc(cabiCABwarn);
	if (!h)
	{
		OutOfMemory() ;
		return;
	}
	SzToCab(h, szEnterButton, Iag(CABwarn, pszwarnEB));
	SzToCab(h, szCancelButton, Iag(CABwarn, pszwarnCB));

	szWarnLine1 = szTaskWarnLine1;
	szWarnLine2 = szTaskWarnLine2;
	szWarnLine3 = szTaskWarnLine3;

	if (MyTmcDoDlg(&dlgwarn,  h) == tmcOK)
	{
		wasshell = 0;
		numtasks = C_GET_LIST_LENGTH(); // zero based, one is shell
		for(i=0;i<=(Get_List_Focus(&TaskList));i++)
		{
			SE = (Switch_Entry far *) C_GET_ITH(i);
	 if(SE->Program_Flags & FSHELL)
			{
				wasshell = 1;
			}
		}
	   delindex = Get_List_Focus(&TaskList)+wasshell;
		SE = (Switch_Entry far *) C_GET_ITH(delindex);
		

		Switchdata = C_GET_GLOBAL_SWITCH_DATA();

		/* Kind of hacky here; we need the table index 
	   * of the item to generate the name the dosswap uses
		 * dosswap uses it to ensure unique ids
		*/
		for(i=0;i<MAX_NUM_PROGRAMS;i++)
		{
			if(Switchdata->Program_list[i].Program_Id == SE->Program_Id)
				break;
		}

		strfcpy(swapfile,Switchdata->Swap_Path1); //always ends in '\' !
		MakeSwapName(name,Switchdata->Switcher_Id,i);
	   strcat(swapfile,name);

	  _dos_setfileattr(swapfile, 0); /* Remove any attribs */
		if(unlink(swapfile) != 0)
		{
				strfcpy(swapfile,Switchdata->Swap_Path2); //always ends in '\' !
				MakeSwapName(name,Switchdata->Switcher_Id,i);
			strcat(swapfile,name);
			_dos_setfileattr(swapfile, 0); /* Remove any attribs */
				unlink(swapfile);
		}
		
		C_DELETE_PROGRAM(delindex);
		DeleteBatchFile();
	
	/* update listbox */     
		InsertListItem(&TaskList,0);
		if(C_GET_LIST_LENGTH() <= 0)  //zero based, one is shell
	   {
			/* Delete global XMS handle if allocated */
			if(Switchdata->XMS_Handle)
			{
				if(InitXMS())
					FreeXMSHandle(Switchdata->XMS_Handle);
			}
			
			/* ZZZZ The ListProctaskList() would make sure that the task list
			 * will not have the focus if it has 0 items in the list. see the
			 * tmmCount case of the switch there! The InsertListItem() call
			 * above will make the listbox ask for the count of lbox items.
			 */
			// PrevGlobalFocus();  // go back to applist
	   }
	}

	FreeCab(h);
} 

