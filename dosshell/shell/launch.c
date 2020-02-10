;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <associat.hs>
#include <associat.sdm>
#include <assoc2.hs>
#include <assoc2.sdm>
#include <run.hs>
#include <run.sdm>
#include <text.h>
#include <help.h>
#include <ctype.h>
#include <tasklist.h>

/* The length of the EXT list string when one is typing in a list of
 * associations separated by space(s). We have placed a limit of 20 EXTs
 * per program -- arbitrary limit. 79 = 20*3 + 19. 3 is the MAX size
 * of an extension and 19 is the number of spaces between them.
 */

#define ASSOC_EXT_LEN 79 


extern void OurUnHookISR9(void) ;
extern VOID StoreFMState(void) ;
extern TOKEN Get_Identifier_Token(char far *identifier); 
extern VOID ReInitTaskParams(void);
extern VOID FarToNearsz(char *temp, char far *str, int max);

extern TOKEN gNext_Task_Info;
extern char gNext_Task_Name[MAX_TITLE+2];
extern BOOL gfFMVisited ;
extern BOOL gfOurISR9Installed ;

extern int cdecl chdir (char *path) ;
extern void RemoveSwapHandler(void) ;
extern BOOL Buffered_Write_Ini_File(BOOL bFreeFM) ;
extern BOOL FIsExecutableFile(char far *ext) ;

extern WORD ReturnScreenMode;
extern char gStartUpDir[];
extern char gStartInDir[];
extern int gStartUpDirEnd; /* location where the NULL goes in the above name */

extern char *QStatus ; /* re-use of variable to pass status line */

extern TMC gCurrentTMC;
extern char userPrompt[];
extern BOOL gTaskListEnabled;
extern BOOL gSwitchingEnabled; 
extern BOOL gfSwapHandlerInstalled;


PENTRY GetSelectedNode(PENTRY firstfile)
{
	PENTRY node;
	node = firstfile;

	 while (node != NULL)
	 {
		 if (node->SELECTED)
			 break;
		 node = node->x.f.snext;
	 }
	 return(node);
}

/****   fstrncmp - compares far to far string
**
**      ENTRY
**              nstr - string in far memory
**              fstr  - string in far memory
**              len  - maximum length to check
**      EXIT
**              0 if equal, 1 if not
*/
int fstrncmp(nstr, fstr, len)
const char far *nstr;
const char far *fstr;
int len;

{
	int i;

	for (i=0; i < len && *fstr == *nstr && *nstr; i++, fstr++, nstr++)
		;
	if (i != len && *nstr != *fstr)
		return 1;
	else
		return 0;
}

VOID strfcat(char far *dest,char far *src)
{
	while(*dest)
	dest++;
	strfcpy(dest,src);
}

/* Delete existing associations of 'name' from the association list.    */
/* 'isprog' == TRUE means that name is a program name, else it is an    */
/*      extension name. 'name' is a string without any enclosing quotes!        */
VOID Delete_Associations(char *name, BOOL isprog)
{
	char far *tname;
	int i,j;
	TOKEN tokenassoc;
	TOKEN token_prog_ext;
	TOKEN tokentemp;

	tokenassoc = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_ASSOCIATIONS);
	token_prog_ext = TK_UNKNOWN;

	if (!((tokenassoc == TK_UNKNOWN) || (tokenassoc == TK_NOTHING)))
	{
	/* delete any existing associations for this program or extension */
	j = Get_List_Length(tokenassoc);
	for(i=1;i<=j;i++)
	{
	   tokentemp = Get_Symbol_Value(Token_To_Symbol(
		Get_Ith_Element(tokenassoc,i)));
	   token_prog_ext = (isprog) ? 
							Get_KeyWord_Assignment(tokentemp,TK_PROGRAM) :
							Get_KeyWord_Assignment(tokentemp,TK_EXT) ;

	   tname = Get_Token_Identifier(token_prog_ext);
	   /* If program name matches delete the existing association */
	   if (fstrncmp(tname, name, 66) == 0)
	   {
			Delete_Ith_Element(tokenassoc,i);

			/* An extension can be associated with only 1 program -- So if */
			/* we have deleted one entry, then we are done. On the other   */
			/* hand, a program can be associated with more than 1 extension */
			if (!isprog)
				return ;

			--j;
			--i;
	   }
	} /* for */
	} /* if */
} /* Delete_Associations */


/*
 * Returns the program associated with the extension -- in FM Internal format
 */

#define NOEXT_CHAR '.'  // The char denoting file with no extension.

char far *Get_Association(char far *extension)
{
	TOKEN tokenassoc;
	TOKEN tokenext;
	TOKEN tokenprog;
	TOKEN tokentemp;
	int i,j;
	char ext[EXTLEN+1];

	/* If file has an extension, move it into 'ext', else use the string "." */
	if (*extension)
	{
		ext[0] = extension[0];
		ext[1] = extension[1];
		ext[2] = extension[2];
	ext[3] = 0;
	}
	else
	{
		ext[0] = (char) NOEXT_CHAR ;
		ext[1] = '\0' ;
	}

	tokenassoc = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_ASSOCIATIONS);
	tokenprog = TK_UNKNOWN;

	if ((tokenassoc == TK_UNKNOWN) || (tokenassoc == TK_NOTHING))
		return(NULL);

	j = Get_List_Length(tokenassoc);
	for(i=1;i<=j;i++)
	{
		tokentemp = Get_Symbol_Value(Token_To_Symbol(
		Get_Ith_Element(tokenassoc,i)));
		tokenext = Get_KeyWord_Assignment(tokentemp,TK_EXT);
		if (fstrncmp(Get_Token_Identifier(tokenext),(char far *) ext,3) == 0)
		{
			tokenprog = Get_KeyWord_Assignment(tokentemp,TK_PROGRAM);
			break;
		}
	}
	if((tokenprog==TK_UNKNOWN) || (tokenprog == TK_NOTHING))
		return(NULL);

	return(Get_Token_Identifier(tokenprog));
}


/* Makes an association between 'program' and 'extension' and adds it to */
/* the association list. It is the caller's responsibility to delete any */
/* pre-existing associations if that is desired.                                                 */
VOID Make_Association(char *program, char *extension)
{
	TOKEN tokenassoc;
	TOKEN tokentemp;
	TOKEN tokennewassoc;
	TOKEN tokennewprogram;
	TOKEN tokennewext;
	TOKEN listtemp;

	tokenassoc = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_ASSOCIATIONS);
	if(tokenassoc < 0) // no association section defined yet
	{
		Set_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_ASSOCIATIONS,TK_SPECIAL);
		tokenassoc = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_ASSOCIATIONS);
	}

	if((tokennewassoc=SubClassSymbol(TK_ASSOCIATION))<=0 ||
		(listtemp=SubClassSymbol(tokennewassoc))<=0 ||
		(tokennewprogram=SubClassSymbol(TK_PROGRAM))<=0 ||
		(tokennewext=SubClassSymbol(TK_EXT))<=0)
		return;

	tokentemp = Get_Identifier_Token(program);
	Set_Symbol_Value(Token_To_Symbol(tokennewprogram),tokentemp);

	tokentemp = Get_Identifier_Token(extension);
	Set_Symbol_Value(Token_To_Symbol(tokennewext),tokentemp);

	Append_Symbol(listtemp,tokennewprogram);
	Append_Symbol(listtemp,tokennewext);
	Set_Symbol_Value(Token_To_Symbol(tokennewassoc),listtemp);

	Append_Symbol(tokenassoc,tokennewassoc);
} /* Make_Association */

/*Makes the association of 'program' with each of the extensions in 'extlist'*/
/* 'program' is the full path name -- example C:\XENIX\BIN\VI.EXE                       */
VOID Make_Association_List(char *program,char *extlist)
{
	char ext[5] ;
	int k ;

	/* Perform the assciation for each extension in the extension list */
	while (TRUE)
	{
		while (isspace(*extlist)) 
			extlist++ ; /* This loop skips all leading white space */

		if (*extlist)
		{
			/*  Use only the first 3 characters -- need to ignore rest! */
			for (k = 0 ; k < 3 ; k++)
			{
				if ( (!isspace(*extlist)) && (*extlist) )
					ext[k] = *extlist++ ;
				else
					break ; /* extension is < 3 characters long! */
			}
			ext[k] = '\0' ;
			/* Delete any prev associations for extension 'ext' */
			Delete_Associations(ext, FALSE) ;

			Make_Association(program, ext) ;

			/* Now look for the white space character terminating this      */
			/* extension -- i.e., ignore extra spurious input chars         */
			while ( (!isspace(*extlist))  && (*extlist) )
				extlist++ ;
		}
		else
			break ; /* All extensions in extension list exhausted */
	} /* while */
} /* Make_Association_List */

/*
 * Returns a list of extensions associated to program name in list
 */
VOID Get_Association_List(char far *name,char far *list)
{
	TOKEN tokenassoc;
	TOKEN tokenext;
	TOKEN tokenprog;
	TOKEN tokentemp;
	int i,j;
	char far *progname ;

	list[0] = 0;
	tokenassoc = Get_KeyWord_Assignment(TK_PROGRAMSTARTER,TK_ASSOCIATIONS);
	tokenprog = TK_UNKNOWN;

	if ((tokenassoc == TK_UNKNOWN) || (tokenassoc == TK_NOTHING))
	return;
	j = Get_List_Length(tokenassoc);
	for(i=1;i<=j;i++)
	{
	   tokentemp = Get_Symbol_Value(Token_To_Symbol(
		Get_Ith_Element(tokenassoc,i)));
	   tokenprog = Get_KeyWord_Assignment(tokentemp,TK_PROGRAM);

	   /* ZZZZ We actually need fstrcmpi. It would be nice to have the
			parser routines to store stuff in its data structs only in
			upper-case! -- Prevents case insensitive compares!! */
	   progname = Get_Token_Identifier(tokenprog) ;

	   /* A large number > path lenth takes care of compare till EOS!! */
	   if(fstrncmp(progname, name, 90) == 0)
	   {
		tokenext = Get_KeyWord_Assignment(tokentemp,TK_EXT);

		strfcat(list,Get_Token_Identifier(tokenext));
		strfcat(list," ");
	   }
	}
}

#ifndef NOLOADER
char far *realdma_ptr;
#endif

/* fstrncpy() -
**
** DESCRIPTION
**      Copies exactly len bytes from far null-terminated source string 
**      to destination string.
*/
void fstrncpy(d, s, len)
unsigned char far *d;
unsigned char far *s;
unsigned int    len;
	{
	while (len--)
		*d++ = *s++;
	}

#ifndef NOSWITCHER
extern WORD gScanInfo;


VOID AdjustTaskTitle(char far *dest,char far * title)
{
	int i,j;
	int len;              
	Switch_Entry far *SE;
	char temp[MAX_TITLE+MAX_NUM_PROGRAMS+16]; 


/* The following makes sure the length is at most MAX_TITLE-1 long */
	FarToNearsz(temp, title, MAX_TITLE);

	len = C_GET_LIST_LENGTH();
	i=0;
	while(i<=len)
	{
		SE = (Switch_Entry far *) C_GET_ITH(i);
		for(j=0;temp[j] && (SE->Program_Title[j] == temp[j]);)
			++j;
		if(temp[j]==SE->Program_Title[j])
		{
			//strings are equal
			if(j >= MAX_TITLE-5) // 5 chars for hot-key
				break; // do not extend past max title
			temp[j] = '.';
			temp[j+1] = 0;

			i=0;
		}
		else
		{
			++i;
		}

	}
	/* be sure the destination is cleared with NULLS so old hotkey
	 * stuff doesn't get included!
	 */
	for(i=0;i<MAX_TITLE;i++)
		dest[i] = 0;
	strfcpy(dest,temp);
   
}

BOOL AddTask(char far *programname, char far *parameters,char far *defaulttitle,TOKEN properties,WORD magiccookie)
{
	char szErrorMessage[150];

	   Switch_Entry far *SE;
		TOKEN token;
		char temp[12];
		char far *title;
		WORD hotkey;
		int len,tlen;

		if((programname && programname[0]) && (C_GET_LIST_LENGTH() >= MAXTASKS))
		{
			/* BUGBUG HID! */
		strcpy(szErrorMessage, szSwitchFailed);
		strcat(szErrorMessage, "\n");
		strcat(szErrorMessage, szTooManyTasks);
			ShellMessageBox(szSwitchErrTitle, szErrorMessage);                      
			return(FALSE);  //did not succeed in adding task
		}
		if((programname) && (programname[0]))
		{
		C_ADD_PROGRAM((char far *) programname);
		C_ADD_PARAMS(parameters,myfstrlen(parameters));

		SE = (Switch_Entry far *) C_GET_ITH(0);
		AdjustTaskTitle(SE->Program_Title,defaulttitle);
		//strfcpy(SE->Program_Title,defaulttitle);

			SE->Shell_Cookie = magiccookie;


			token = Get_KeyWord_Assignment(TK_SAVESTATE,TK_PAUSE);
			if(token == TK_DISABLED)
		 SE->Program_Flags |= F_NO_PAUSE;
			else
				 SE->Program_Flags &= ~F_NO_PAUSE;


			/* Here is where we set up the parameters in the 
			 * global data structure for the initial launch of
			 * a program item
			 */
			if(properties > 0)
			{
				token = Get_KeyWord_Assignment(properties,TK_TITLE);
				if(token > 0)
					title = Get_Token_Identifier(token);
				else
					title = NULL;
				if(title && title[0])
				{
					/* We will also add in the hot key sequence if it will fit */
				AdjustTaskTitle(SE->Program_Title,title);
					tlen = myfstrlen(SE->Program_Title);
					token = Get_KeyWord_Assignment(properties,TK_SHORTCUT);
					if(token>0)
					{
						title = Get_Token_Identifier(token);
						len = myfstrlen(title);
						/* we save the hot-key after the name's null */
						if(tlen+len < MAX_TITLE - 4) //+3  for parens + 1 for NULL
						{
							strfcpy(&SE->Program_Title[tlen+1]," (");
							strfcpy(&SE->Program_Title[tlen+3],title);
							strfcpy(&SE->Program_Title[tlen+3+len],")");
						}
					}
		
				}

				token = Get_KeyWord_Assignment(properties,TK_KBREQUIRED);
				if(token>0)
					title = Get_Token_Identifier(token);
				else
					title = NULL;
				if(title && title[0])
				{
					fstrncpy(temp,title,12);
					SE->Conv_Req = atoi(temp);
				}
				else
				{
						SE->Conv_Req = 0;
				}

				token = Get_KeyWord_Assignment(properties,TK_XMSREQUIRED);
				if(token>0)
					title = Get_Token_Identifier(token);
				else
					title = NULL;
				if(title && title[0])
				{
					fstrncpy(temp,title,12);
					SE->XMS_Req = atoi(temp);
				}
				else
				{
					SE->XMS_Req = 0;
				}

				token = Get_KeyWord_Assignment(properties,TK_XMSLIMIT);
				if(token > 0)
					title = Get_Token_Identifier(token);
				else
					title = NULL;
				if(title && title[0])
				{
					fstrncpy(temp,title,12);
					SE->XMS_Want = atoi(temp);
				}
				else
				{
					SE->XMS_Want = 0;
				}

				token = Get_KeyWord_Assignment(properties,TK_SCREENMODE);
				if(token == TK_GRAPHICS)
					SE->Program_Flags |= F_GRAPHICS;

				token = Get_KeyWord_Assignment(properties,TK_ALTTAB);
				if(token == TK_DISABLED)
					SE->Program_Flags |= F_NO_ALT_TAB;
				token = Get_KeyWord_Assignment(properties,TK_ALTESC);
				if(token == TK_DISABLED)
					SE->Program_Flags |= F_NO_ALT_ESC;
				token = Get_KeyWord_Assignment(properties,TK_CTRLESC);
				if(token == TK_DISABLED)
					SE->Program_Flags |= F_NO_CTRL_ESC;
				token = Get_KeyWord_Assignment(properties,TK_PREVENT);
				if(token == TK_ENABLED)
					SE->Program_Flags |= F_NO_SWITCH;

		token = Get_KeyWord_Assignment(properties,TK_SHORTCUTCODE);
				if(token > 0)
				{
					title = Get_Token_Identifier(token);
					hotkey = 0;
					if(title[0])
					{
						fstrncpy(temp,title,12);
						hotkey = atoi(temp);
					}
					SE->HK_Scan_Code_1 = (BYTE) (hotkey&0x1000?0x0E0:0); // tell switcher its on the extended keyboard
					SE->HK_Scan_Code_2 = LOBYTE(hotkey);
					SE->HK_Shift_State = HIBYTE(hotkey)&0x0F;       //hi nibble is special extended signature
				}
			}
		}
		else
		{

			/* alt-tabing to next guy in the list */
			C_GO_Z_NEXT();
			/* If the program has not been run yet, we must set up the parameters
			* again. So we use RestartTask.
			*/
   
			SE = (Switch_Entry far *) C_GET_ITH(0);
			if(SE->Program_Id == 0)
				ReInitTaskParams();

		}
		return(TRUE);

}

#endif
VOID LaunchProgram(char far *programname,char far *parameters,int magiccookie)
{
#if 0
		Switch_Info far *switchdata;
		int i;
#endif
#ifndef NOLOADER
	   int ind ;
#ifndef NOSWITCHER
	if(glob.InFileMgr)
	   DoSwapOut();

	if(gSwitchingEnabled)
	{
		/* add the program we are about to run to the global
	   * task list
	   */
		if (!AddTask(programname,parameters,gNext_Task_Name,gNext_Task_Info,magiccookie))
			return;
		/* we let the switcher do the pausing if it is around*/
		Set_KeyWord_Assignment(TK_SAVESTATE,TK_PAUSE,TK_DISABLED);
	 
	   ind = gStartUpDirEnd ;
#ifdef DBCS
	   if (gStartUpDir[ind-1] != '\\' || CheckDBCSTailByte(gStartUpDir,&gStartUpDir[ind-1]))
#else
	   if (gStartUpDir[ind-1] != '\\')
#endif
	   {
		 gStartUpDir[ind++] = '\\' ;
	   }
		strcpy(gStartUpDir+ind,szSwitcher);
		strfcpy(programname,(char far *)gStartUpDir);
	   parameters = szSwitcherMagic;
		gStartUpDir[gStartUpDirEnd]=0;
	}
	else
	{
#if 0
WE CANNOT DO THIS BECAUSE THE SWITCHDATA IS NO LONGER RESIDENT!
		/*  BUG BUG
		 * We still need to save the magic cookie, so we can delete the
	   * damn batch file
	   */

		switchdata = C_GET_GLOBAL_SWITCH_DATA();
		for(i=0;i<MAX_NUM_PROGRAMS;i++)
		{
			if(switchdata->Program_list[i].Program_Flags & F_FREE)
			{
				switchdata->Program_list[i].Shell_Cookie = magiccookie;
				break;
			}
		}
#endif
	}
#endif


	   realdma_ptr = GET_COMMAND_PTR();
	   fstrncpy((char far *)realdma_ptr,programname,64);
	   realdma_ptr = GET_ARGS_PTR();

	   fstrncpy((char far *)realdma_ptr+2,(char far *)parameters,53);
	   realdma_ptr[0] = (char) myfstrlen(parameters)+1;
	   realdma_ptr[1] = ' ';
	   realdma_ptr[myfstrlen(parameters)+2] = '\r';

		/* Store the FM state variables in the INI parsed variables so that they
		 * get written out to the INI file when we launch a program.
		 */ 
		StoreFMState() ;

	if(!Buffered_Write_Ini_File(TRUE))
	{
		ShellMessageBox(szNoIniTitle, szNoIniMsg1);
	}

	   /* Warning! can't make the following call after EndCow() etc as that guy will
		 * do an UnHookIS9() and the order in which we unhook will be messed
		 * up and the keyboard could lock up!!
		 */
		OurUnHookISR9() ;

	   // chdir(gStartInDir);  // DoCommand() sets the correct start up directory.
#ifdef KANJI
	TermKkc();
#endif
	   SetScreenMode(ReturnScreenMode);
	   FEnableMouse(FALSE);
	   EndScreen(TRUE);
	   EndCow(TRUE);
		if(gfSwapHandlerInstalled)
			RemoveSwapHandler() ;
	   //DisableUMBS();
	   exit(0);
#endif
}

PENTRY glaunchnode ; // File node to be launched.
PTREE glaunchtree ; // the tree to which above node belongs. This value is valid
					// only if glaunchNode is non-NULL!

/* Following specifies whether fn 'LaunchBox' was invoked from the menu or
 * thru a 'DoubleClick' on file -- i.e., an activate message!
 */
BOOL gfActivate = FALSE ;

VOID FAR LaunchBox(void)
{

	char programstring[256];
	char associated[80];
	char LaunchInDir[MAX_PATH+1];
	int dummylen ;

	char far * progrun;

	if (!gfActivate)
	{
		if (listinfo[glob.FocusBox].tree->NumSel > 0)
		{
			glaunchtree = listinfo[glob.FocusBox].tree ;
			glaunchnode = GetSelectedNode(glaunchtree->FirstFile);
		}
		else
			glaunchnode = NULL ;
	}
	else
	{
		/* We entered this fn from an Activate message in file listbox
		 * In this case, glaunchnode, glaunchtree are already set correctly
		 */
		gfActivate = FALSE ; // set it back to default state!!
	}
	if (glaunchnode)
	{
		progrun = Get_Association(glaunchnode->name+NAMELEN);
		if (progrun == NULL)
		{
		   /* File has no association with it! -- Is file not an executable
			* file (.COM, .EXE, or .BAT)? If it is a non-executable should
			* I beep at user as a warning?
			* ZZZZZ should a dialog box warn the user? -- a painful user
			* interface.
			*/
		   if (!FIsExecutableFile(glaunchnode->name+NAMELEN))
		   {
				Shell_Beep() ;
				return ;
		   }

		   Tree2Path(glaunchtree, glaunchnode, programstring, &dummylen);
		   associated[0] = NULL;
		}
		else
		{
		   strfcpy(programstring,(char far *)progrun);
		   Tree2Path(glaunchtree,glaunchnode,associated, &dummylen);
		}
		/* program string holds name of program, associated holds
		 * argument file name (association)
		 */

		strcat(programstring," ");
		strcat(programstring,associated);
		/*
		 * Always pause after file manager launch...
		 */
		Set_KeyWord_Assignment(TK_SAVESTATE,TK_PAUSE,TK_ENABLED);
		userPrompt[0] = 0;

		/* Note that LaunchBox can be called only from within the FM
		 * with a file in file listbox! SO FocusBox will be OK!
		 */
		Tree2Path(listinfo[glob.FocusBox].tree, listinfo[glob.FocusBox].files,
														 LaunchInDir, &dummylen) ;
		//DoCommand(programstring, gStartInDir);
		DoCommand(programstring, LaunchInDir);
	}
	else
		Shell_Beep() ;

} /* LaunchBox */


int AssociateBox(PTREE tree,PENTRY node,char *path, int count,
							   int total, BOOL verify)
{
	char tstr1[90];
	char tstr2[90];
	char buffer[90];
	char ext[EXTLEN+1] ;
	char statusline[STATUS_LEN] ;
	char statusline2[STATUS_LEN] ;
	char far *assoc_prog ;
	int ret ;
	TMC tmc ;
	int dummylen ;
	HCABassociate h ;
	HCABassoc2 h2 ;

	UnReferenced(path) ;
	UnReferenced(verify) ;

	Tree2Path(tree, node, buffer, &dummylen);
	FormCountStatusLine(statusline, szFileName, buffer, count, total,
							STATUS_COUNT_START) ;

	if (FIsExecutableFile(node->name+NAMELEN))
	{
		if (!(h = HcabAlloc(cabiCABassociate)))
		{
			OutOfMemory() ;
			return ACT_NOMEM;
		}
		InitCab(h, cabiCABassociate) ;


		/* It is a .EXE, .COM, or .BAT file -- ask user for extension list */
		QStatus = statusline ; /* So that FDlgAssociate can print this status */

		/* Display the current set of associate extensions */
		Get_Association_List(buffer, tstr1);
		SzToCab(h, tstr1, Iag(CABassociate, pszassocextensions));

		SzToCab(h,szEnterButton , Iag(CABassociate, pszassocEB));
		SzToCab(h,szCancelButton , Iag(CABassociate, pszassocCB));
		SzToCab(h,szHelpButton  , Iag(CABassociate, pszassocHB));

		if ( (tmc = MyTmcDoDlg(&dlgassociate,  h)) == tmcOK)
		{
			/* The +1 is for the EOS character ('\0') */
			SzFromCab(h, tstr2, ASSOC_EXT_LEN+1, 
										Iag(CABassociate, pszassocextensions));
			/* Perform the new associations only if the new assoc string
			 *is different. Quite often one just looks at the "associations"
			 * of a file and types "enter".
			 */
			if (strcmp(tstr1, tstr2) != 0)
			{
#ifdef DBCS
				DBCSstrupr(tstr2) ; /* to store in upper case! */
#else
				strupr(tstr2) ; /* to store in upper case! */
#endif

				/* It is so much cleaner & easier to just delete old assocs
				 * of 'buffer' and add these new associations, instead of
				 * checking for pre-existence of desired new associations and
				 * not deleting.
				 */
				Delete_Associations(buffer, TRUE) ;
				Make_Association_List(buffer,tstr2);
			}
			ret = ACT_OK ;
		}
		else
		if (tmc == tmcCancel)
			ret = ACT_CANCEL ;

		FreeCab(h) ;
	}
	else
	{
		/* we are trying to associate an extension with a program that the user
		 * will be prompted to supply now.
		 */

		/* ZZZZZ We now allow associating files that have no extension also. */
#if 0
		/* Does file have no extension? We don't allow this case!! */
		if ( *(node->name+NAMELEN) == '\0' )
		{
			/* ZZZZZ Should we have a special character for files with
			 * no extensions -- say '.' when trying to associate such
			 * files with an executable program??
			 */
			ret = GetResponse(statusline, szExtensionInvalid,
							  BT_FILESKIPQUIT, HELP_INVALIDASSOCEXT);
		}
		else
#endif
		{
			if (!(h2 = HcabAlloc(cabiCABassoc2)))
			{
				OutOfMemory() ;
				return ACT_NOMEM;
			}
			InitCab(h2, cabiCABassoc2) ;

			strfncpy(ext, node->name+NAMELEN, EXTLEN) ;
			ext[EXTLEN] = '\0' ; /* just to make sure there is an EOS */

			FormStringWithoutPlaceHolders(statusline2, szAreAssocLine,
																			(char far *) ext) ;

			QStatus = statusline2 ;

			if ( (assoc_prog = Get_Association(node->name+NAMELEN)) == NULL )
				tstr1[0] = '\0' ;
			else
				strfcpy(tstr1, assoc_prog) ; /* copy from far memory to near */

			SzToCab(h2, tstr1, Iag(CABassoc2, pszassoc2program));

			SzToCab(h2, szEnterButton, Iag(CABassoc2, pszassoc2EB));
			SzToCab(h2, szCancelButton, Iag(CABassoc2, pszassoc2CB));
			SzToCab(h2, szHelpButton, Iag(CABassoc2, pszassoc2HB));


			if ( (tmc = MyTmcDoDlg(&dlgassoc2, h2)) == tmcOK)
			{
				SzFromCab(h2, tstr2, sizeof(tstr2),
										Iag(CABassoc2, pszassoc2program));
				/* Perform the new associations only if the new assoc string
				 * is different. Quite often one just looks at the "associations"
				 * of a file and types "enter".
				 */
				if (strcmp(tstr1, tstr2) != 0)
				{
#ifdef DBCS
					DBCSstrupr(tstr2) ; /* to store in upper case! */
#else
					strupr(tstr2) ; /* to store in upper case! */
#endif
					/* Transform a NULL extension to '.' as that is how we store
					 * it in the INI file.
					 */
					if (ext[0] == '\0')
					{
						ext[0] = (char) NOEXT_CHAR ;
						ext[1] = '\0' ;
					}

					/* Delete any previous associations for extension 'ext' */
					Delete_Associations(ext, FALSE) ;

					/* Add new association, if tstr2 is not a NULL string */
					if (*tstr2)
					{
						Make_Association(tstr2, ext) ;
					}
				}
				ret = ACT_OK ;
			}
			else
				if (tmc == tmcCancel)
					ret = ACT_CANCEL ;

			FreeCab(h2);
		}
	}
	return ret ;
} /* AssociateBox */


VOID DoAssociateBox(void)
{
	gpszFileOpCaption = szAssociateCaption ;

	DoFileOp(OP_ASSOCIATE, NULL);
}


BOOL FAR PASCAL FDlgassoc2(WORD dlm, TMC tmc, WORD wNew,
													WORD wOld, WORD wParam)
{
	UnReferenced(tmc) ;
	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	if (dlm == dlmInit)
	{
		SetUpDialog(tmcOK,szAssociateCaption);
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcCancel);
		SetUpButtonForGraphics(tmcassoc2help);
		SetUpEditBox(tmcassoc2program, TRUE,USERS_MAX_TYPEABLE_PATH, TRUE);
		Shell_SetTmcText(tmcassoc2status,QStatus);
	}
	else if (dlm == dlmSetFocus)
	{
		gCurrentTMC = tmc;
	}
	else if (dlm== dlmClick)
	{
		 if(tmc == tmcassoc2help)
			Help(hemDialog, hidASSOC2,NULL,0);

		 SetFocusTmc(gCurrentTMC) ;
	}

	return(TRUE);
}

BOOL FAR PASCAL FDlgassociate(WORD dlm, TMC tmc, WORD wNew,
													WORD wOld, WORD wParam)
{
	UnReferenced(tmc) ;
	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	if (dlm == dlmInit)
	{
		SetUpDialog(tmcOK,szAssociateCaption);
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcCancel);
		SetUpButtonForGraphics(tmcassochelp);
		SetUpEditBox(tmcassocextensions, TRUE, ASSOC_EXT_LEN, TRUE);
		Shell_SetTmcText(tmcassocstatus,QStatus);
	}
	else if (dlm == dlmSetFocus)
	{
		gCurrentTMC = tmc;
	}
	else if (dlm== dlmClick)
	{
		 if(tmc == tmcassochelp)
			Help(hemDialog, hidASSOCIAT,NULL,0);

		 SetFocusTmc(gCurrentTMC) ;
	}

	return(TRUE);
}

BOOL FAR PASCAL FDlgrun(WORD dlm, TMC tmc, WORD wNew, WORD wOld, WORD wParam)
{
	UnReferenced(wNew);
	UnReferenced(wOld);
	UnReferenced(wParam);

	if (dlm == dlmInit)
	{
		SetUpDialog(tmcOK,szRunCaption);
		SetUpButtonForGraphics(tmcOK);
		SetUpButtonForGraphics(tmcCancel);
		SetUpEditBox(tmccommandline, TRUE,255, TRUE);
	}
	else if (dlm == dlmSetFocus)
	{
		gCurrentTMC = tmc;
	}
	return(TRUE);
}

void Set_Task_Name(char far *name);
BOOL gfInRun = FALSE;
VOID FAR Run(void)
{
	HCABrun        h;
	TMC tmc;
	char commands[256];
	char RunInDir[MAX_PATH+1] ;
	char *pStartDir ;
	int dummylen ;

	h = HcabAlloc(cabiCABrun);
	if (!h)
	{
		OutOfMemory() ;
		return ;
	}
	InitCab(h, cabiCABrun) ;

	SzToCab(h, NullString, Iag(CABrun, pszcommandline));
	SzToCab(h, szEnterButton, Iag(CABrun, pszrunEB));
	SzToCab(h, szCancelButton, Iag(CABrun, pszrunCB));

	tmc = MyTmcDoDlg(&dlgrun, h);
	SzFromCab(h, commands, 256, Iag(CABrun, pszcommandline));
	FreeCab(h);
	if(tmc == tmcOK)
	{
		/*
		 * Always pause after run...
		 */
		Set_KeyWord_Assignment(TK_SAVESTATE,TK_PAUSE,TK_ENABLED);
		userPrompt[0] = 0;

#if 0
/* ZZZZZ Scott & I discussed this and decided that we won't do
 * "run" like "open/launch" as in run, we can execute commands internal to
 * command.com, etc
 */

		/* Now check for "commands" having any association!
		 * Note that if we had a space on this line, the command the user
		 * typed is probably something like "word f1.doc". In this case
		 * we don't need to check for associations.
		 */
		if (strchr(commands, ' ') == NULL)
		{
			ind = FindLastComponent(commands) ;
			if ( (pPeriod = strchr(commands+ind, '.')) != NULL)
			{
				strncpy(extension, (pPeriod+1), EXTLEN) ;
				extension[EXTLEN] = '\0' ; // Null-terminate just in case!

				if (!FIsExecutableFile(extension))
				{
					/* check for association! */
					progrun = Get_Association(extension);
					if (progrun == NULL)
					{
						/* ZZZZZ Put up a dialog saying can't run?? */
						Shell_Beep() ;
						return ;
					}
					else
					{
						Shell_Beep() ; Shell_Beep() ;
					}
				}
				/* else it is an executable file -- no problem! */
			}

		}
		/* else execute user typed command directly */
#endif

		/* By default we would want to use the directory in which the user
		 * was when he started the DOSSHELL. This will be valid if Run is
		 * done from PM and the FM has not been visited.
		 */
		pStartDir = gStartInDir ;

		if (gfFMVisited)
		{
			Tree2Path(listinfo[glob.FocusBox].tree,
							   listinfo[glob.FocusBox].files, RunInDir, &dummylen) ;
			pStartDir = RunInDir ;
		}
		Set_Task_Name(commands);
		gfInRun = TRUE;
		DoCommand(commands, pStartDir);
		/* we only get here in case of error condition */
		gfInRun = FALSE;
	}
} /* Run */
