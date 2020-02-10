/* Revision history since 5.0 Golden:
 *
/* M010 SHK	08/13/91	Modified this function to support user control of the
 * 						primary swap path thru the INI file/DOSSHELL environment
 *							variable.
 *
 * M011 SHK	07/31/91	Modified fns FDlgmswarn() and WarnMouseIsOld()
 *							to put up our new mouse compatibility message.
 *
 *	M013 SHK 10/02/91 If mouse version number is unknown, we print appr. msg.
 *
 */

;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/* harikris: created Sep. 25, '89                       */
/* Added function pmatch - Sep. 26 '89                  */
#include <common.h>
#include <filemgr.h>
#include <menus.h>
#include <prot.h>
#include <text.h>
#include <errno.h>
#include <assert.h>
#include <ctype.h>
#include <message.hs>
#include <message.sdm>
#include <about.hs>
#include <about.sdm>
#include <mswarn.hs>
#include <mswarn.sdm>

extern BOOL gfRepaintonidle;
extern char gStartUpDir[] ;
extern int gStartUpDirEnd ;


extern BOOL fgSymbolTableModified ;
extern BOOL fNoEffectiveChange(void) ;
extern void FreeFMMemory(void) ;

#ifdef KANJI
extern char szmHelpShellAbout[] ;
#endif

void strfcpy(char far *dst, char far *src)
{
	for ( ; *src ; )
		*(dst++) = *(src++) ;
	
	*dst = '\0' ;
}

void strfncpy(char far *dst, char far *src, unsigned cnt)
{
	/* Assumption is that cnt is not negative */
	for ( ;  cnt ; cnt-- )
		*(dst++) = *(src++) ;
}

/****************************************************************/
/* INPUT :                                                                                                              */
/* 'pat' is a null terminated pattern possibly having the meta  */
/*      characters '?' and '*'.                                                                         */
/* 'mode' indicates whether to match extensions or name + extension*/
/* 'str' is the name/extension of file to be matched.                   */
/*      It is null terminated or not depending on its length -- see */
/*      the data structure associated with each file in our tree        */
/* OUTPUT:                                                                                                              */
/*      TRUE on succesful match, else FALSE                                                     */
/****************************************************************/
BOOL pmatch(char far *pat, char far *str, BYTE mode)
{
	int i ;
	char far *ext_start ;

	if (mode == FULLNAME)
	{
		i = NAMELEN ;
		ext_start = str + NAMELEN ;
	}
	else
		i = EXTLEN ;

	/* loop as many times as the lengths or until match over */
	for (; (*pat != '*') && i ; i-- )
	{
		switch (*pat) {

		case '?' :
			/* '?' matches a null character too -- so don't advance our
				string only if *str is non-null. We have only one NULL character
				in string if at all any */
			if (*str)
				str++ ;
			pat++ ;
			break ;

		case '.' :
			if (mode == EXTENSION) 
				/* ZZZZ Actually we shouldn't have a '.' in pattern to match
				extensions -- ill-formed pattern but we ignore it and say
				that the pattern matches the string!!! */
				return(TRUE) ;
			else
			{
				/* The '.' is treated as "*." if it is the first character in
					the pattern. */
				if ((i == NAMELEN) || (!*str))
					return(pmatch(pat+1, ext_start, EXTENSION)) ;
				else
					return(FALSE) ;
			}       

		case NULL :
			/* NULL character in pattern -- only matches null in string */
			return (!*str) ;

		default :
#ifdef DBCS
			if (IsDBCSLeadByte(*pat) || IsDBCSLeadByte(*str))
			{
				if (*(pat++) != *(str++) || *(pat++) != *(str++))
					return (FALSE);
			}
			else
#endif
			if (toupper(*pat) != toupper(*str))
				return (FALSE) ;
			else 
			{
				pat++ ; str++  ;
			}
		} /* switch */
	} /* for */

	/* if we are here pattern has been succesfully matched so far */
	if (mode == EXTENSION)
		return(TRUE) ;

	/* else skip forward in pattern until EOS or until '.' and then
		match extensions.                                                                                       */
	while (*pat)
	{
		if (*(pat++) == '.')
			return (pmatch(pat, ext_start, EXTENSION)) ;
	}
	return (TRUE) ;
}


VOID get_comspec(char *buffer)
{
   char *envptr;

   envptr = getenv(szComSpec);
   if(envptr)
	 strcpy(buffer,envptr);
   else
   /* BUGBUG can this ever happen?
   */
	buffer[0] = 0;
}

/* Function verifies whether full path name specified by "path" is a valid
 * directory that we can use to create some temp files fast!! So, we don't
 * want to use floppies ('A', 'B'). If it is some other drive, we make sure
 * that the path exists. This routine should NOT change directories as
 * the caller has set up the directory right to do a launch of a program, etc.
 */
BOOL VerifyTempPath(char *path)
{
	char uppercasedrive ;
	unsigned attribs ;
	char TrueName[128] ;
	int result ;
	int temp ;

	BOOL ret = TRUE ;

	uppercasedrive = (char) toupper(*path) ;

	/* We don't want floppies to be our temp paths */
	if (getdrivetype(uppercasedrive - 'A' + 1) == FLOPPY_TYPE)
		ret = FALSE;
	else
	{
		/* If we can't get the attributes of the path, it doesn't exist! */
		if ( (result = _dos_getfileattr(path, &attribs)) != 0 )
		{
			/* Is parent the ROOT dir? If so, _dos_getfileattr call
			 * fails on networks like NOVELL! The root is always a
			 * valid directory! Note that if path E:\  or some such stuff
			 * was specified and drive E: were invalid result will not
			 * be ENOENT!
			 */
			temp = strlen(path) ;
#ifdef DBCS
			if ((result == ENOENT) && (temp == 3) && (path[temp-1] == PATHCHAR) && !CheckDBCSTailByte(path,&path[temp-1]))
#else
			if ((result == ENOENT) && (temp == 3) && (path[temp-1] == PATHCHAR))
#endif
			{
				ret = TRUE ;
			}
			else
			{
				ret = FALSE ;
			}
		}
		else
		{
			/* ZZZZZZZZZZZZZ */
			/* path exists, but for root directory DOS screws up on get_attr!
			 * The attrib field is garbage in this case!!
			 * So, we get true name, to get rid of '.', '..', etc. After this
			 * if we find the last character a '\\' it is the root directory.
			 */
			if (translate_name(path, TrueName))
				strcpy(TrueName, path) ;

#ifdef DBCS
			if (TrueName[strlen(TrueName)-1] == '\\' && !CheckDBCSTailByte(TrueName,&TrueName[strlen(TrueName)-1]))
#else
			if (TrueName[strlen(TrueName)-1] == '\\')
#endif
				ret = TRUE ;
			else
				ret = (attribs & _A_SUBDIR) ;
		}
	}

	return ret ;
}

/* M010 -- modified this function to support user control of the
 * primary swap path thru the INI file/DOSSHELL environment variable.
 */
VOID get_temp_dir(char *buffer, BOOL UseTemp)
{
	char *envptr;
	TOKEN swapdisk_tok ;

	buffer[0] = 0;

	if(UseTemp)
	{
		swapdisk_tok = Get_KeyWord_Assignment(TK_SAVESTATE, TK_SWAPDISK) ;
		if (swapdisk_tok >= 0)
		{
			strfncpy(buffer, Get_Token_Identifier(swapdisk_tok), ALLOWED_PATH) ;
			buffer[ALLOWED_PATH] = '\0' ; /* Null terminate just in case */

			/* Note that swapdisk variable in the INI file can have two paths
			 * in it -- the primary swap path followed by the secondary path.
			 * Make buffer point to only to the null terminated primary
			 * swap path.
			 */
			/* note that return value of foll. call will be buffer itself! */
			strtok(buffer, " \t") ;

			/* Make sure that it is a valid path. If not, don't use it! */
			if (!VerifyTempPath(buffer))
				buffer[0] = '\0' ;
		}

		if (buffer[0] == 0)
		{
			/* See if TEMP is set to a valid dir! */
	   	envptr = getenv(szTempEnvDirectory);
	   	if ( (envptr) && VerifyTempPath(envptr) )
				strcpy(buffer,envptr);
	   	else
	   	{
				/* See if TMP is set to a valid dir! */
				envptr = getenv(szTmpEnvDirectory);
				if (envptr && VerifyTempPath(envptr) )
					strcpy(buffer,envptr);
				else
				{
					/* See if DOSSHELL is set to a valid dir! */
					envptr = getenv(szIniFileEnvVar);
					if (envptr && VerifyTempPath(envptr) )
						strcpy(buffer,envptr);
				}
	   	}
		}
	}

	/* If swapdisk is not set in the INI file, or if there is no TEMP, TMP,
	 * DOSSHELL vars set in the environment, use the startup directory!
	 */

	if(buffer[0] == 0)
	{
		strcpy(buffer,gStartUpDir);
	}
} /* get_temp_dir */

/*
NOTE:   The following two routines exist only because LpbSaveGraphicArc
*               and RestoreGraphicArc are declared as NEAR PASCAL routines in the
*               CW library. In order to access them, we have these two intermediate
*               routines "MyLpbSave..." and "MyRestore..." that are placed in the
*               same segment as the CW library routines (using the alloc_text pragma). 
*               These in turn make near calls to these routines and we are able to use
*               the routines that CW does not give us direct access to. This indirection
*               is not too bad, as speed is not critical here.
*/
// #pragma alloc_text(CW_USER,MyLpbSaveGraphicArc)
BYTE FAR *MyLpbSaveGraphicArc(LPFN_LPB pfn, WORD cbSGA, AX left, AY top,
						  AX right, AY bottom)
{
	UnReferenced(cbSGA) ; /* Was used ina previous CW version */

	return LpbSaveGraphicArc(pfn, left, top, right, bottom) ;
	// return LpbSaveGraphicArc(pfn, cbSGA, left, top, right, bottom) ;
} /* MyLpbSaveGraphicArc */


// #pragma alloc_text(CW_USER,MyRestoreGraphicArc)
VOID MyRestoreGraphicArc(LPFN pfn, AX left, AY top, AX right, AY bottom, 
						   BYTE FAR *lpbSGA)
{
	RestoreGraphicArc(pfn, left, top, right, bottom, lpbSGA) ;
} /* MyRestoreGraphicArc */

/*
*   'glpbSGA' is pointer to the saved graphics rectangle. Used by routines
*       PutUpStatusMessage and TakeDownStatusMessage only!
*/
BYTE FAR *glpbSGA ;
BOOL gfStatusMsgUp = FALSE ;

RRC grrc = {STATUSMSGLEFT, STATUSMSGTOP, STATUSMSGRIGHT, STATUSMSGBOTTOM} ;

void DrawStatusMessageBackGround(int len)
{
	FillRrc(&MainWind, &grrc, ' ', isaDialogBox) ;

	if (gisgraph)
		FrameCharRect(grrc.ryTop, grrc.rxLeft, grrc.ryBottom, 
								grrc.rxLeft+len, 1,isaBorders) ;
	else
		EasyDrawBox(&MainWind, grrc.ryTop, grrc.rxLeft, grrc.ryBottom,
							grrc.rxLeft+len, isaDialogBox) ;
} /* DrawStatusMessageBackGround */


/*
*  These two routines: 'PutUpStatusMessage' & 'TakeDownStatusMessage' are not
*  re-entrant as they use the global variables 'glpbSGA', 'gfStatusMsgUp'.
*  This is perefectly acceptable in our context.
*  It draws the message-box only if (count == 1), i.e., when file-op is
*  being performed on the first selected file. When (count > 1), it assumes
*  that the box is up and so it only does 'TextOut' of 'msg'.
*/
VOID PutUpStatusMessage(char *msg, int count)
{
	WORD cbSave ;
	BYTE len ;

	/* If count == 1, this is the first time, a call to "PutUpStatusMessage()"
	 * has been made, so put it up! It is possible that the user skipped
	 * performing the operation on file 1 but went thru with the rest!
	 */
	if ( (count == 1) || (!gfStatusMsgUp) )
	{
		gfStatusMsgUp = TRUE ; /* indication to others that message put up */
	
		/*len can't exceed screen width, hence OK. +4 is for the filler blanks*/
		len = (BYTE) strlen(msg)+4 ; 
		grrc.rxRight = grrc.rxLeft + len ;
	
		cbSave = len * (grrc.ryBottom - grrc.ryTop);
		
		if (gisgraph)
		{
			glpbSGA = MyLpbSaveGraphicArc(LpbAllocWorkFar, cbSave, grrc.rxLeft,
								grrc.ryTop, grrc.rxLeft+len, grrc.ryBottom);
			/* if glpbSGA is NULL no memory available -- problem!! */
			/* Take down status message will repaint screen properly */
		}
		else
		{
			cbSave = CbSizeRrc(&grrc) ;
			if (glpbSGA = LpbAllocWorkFar(cbSave))
			{
				SaveRrc(&MainWind, &grrc, glpbSGA) ;
			}
			/* else no memory available -- problem!! */
			/* Take down status message will repaint screen properly */
		}
		DrawStatusMessageBackGround(len) ;
	}
	else
	{
		/* Did a dialog pop up and not restore stuff behind it when it was
		 * dismissed? In this case, the background might not be restored.
		 * Som redraw it now.
		 */
		if (gfRepaintonidle)
		{
			DrawStatusMessageBackGround(strlen(msg)+4) ;
		}
	}

	/* Disable mouse, as file-op is being performed! */
	FEnableMouseNest(FALSE) ;
	/*
		Note that the Mouse in not turned back ON in this routine! It will be
		turned ON by TakeDownStatusMessage. If needed before then,
		it should be turned on specifically , ex: by dialog box routines!
	*/

	TextOut(&MainWind, grrc.rxLeft+2, grrc.ryTop+2, msg, -1, isaDialogBox) ;

} /* PutUpStatusMessage */

/*
*   Takes down the Status Message that is curently on the screen, if possible.
*   If it cannot restore the screen to its original state, it returns FALSE,
*   else TRUE.
*       Actually does the above, only when (count == total), i.e., the file-op
*       has been performed on the last of a bunch of selected files. Otherwise,
*       the message is left up with only the Mouse being turned back ON.
*/
BOOL TakeDownStatusMessage(int count, int total)
{
	if (count == total)
	{
		gfStatusMsgUp = FALSE ;
	
		/* ZZZZZZ Hack to get around a CW problem!!! If I do not erase this
		 * stuff before restoring the graphics, when I pull down the menu
		 * and ESC to dismiss it, there is an ugly repaint problem!
		 */
		FillRrc(&MainWind, &grrc, ' ', isaBackground) ;

		/* ZZZZZ If (glpbSGA == NULL), we cannot use MyRestore... as the 
			SaveGraphicsArc didn't get enuf memory to save the screen.
		   It is the caller's responsibility to redraw the screen?!
		*/
		if (glpbSGA)
		{
			if (gisgraph)
			{
				MyRestoreGraphicArc(FreeWorkFar,grrc.rxLeft, grrc.ryTop,
									grrc.rxRight, grrc.ryBottom, glpbSGA);
			}
			else
			{
				RestoreRrc(&MainWind, &grrc, glpbSGA) ;
				FreeWorkFar(glpbSGA) ;
	
			}
		}
		else
		{
			// This is being done in the Hack above!
			// FillRrc(&MainWind, &grrc, ' ', isaBackground) ;
			gfRepaintonidle = TRUE ;
		}
	}

	FEnableMouseNest(TRUE) ;

	return (glpbSGA != NULL) ;
} /* TakeDownMessage */


/* Finds the length of the longest name for directories under 'dir'.
 * This lenght includes the '\\' char that will precede the sub-dirs in
 * the complete path name. This is a recursive routine.
 */
int find_longest(PENTRY dirnode)
{
	int temp_len, max_len ;
	PENTRY node ;
	char temp_normal_name[NAMELEN+EXTLEN+2] ;

	max_len = 0 ;
	node = dirnode->x.d.child ;
	while(node)
	{
		if (node->attribs & _A_SUBDIR)
		{
			/* the '1' is for the '\' that will precede node name in path */
			temp_len = 1 + Internal2Normal(temp_normal_name, node->name) +
							find_longest(node) ;

			if (temp_len > max_len)
				max_len = temp_len ;
		}

		node = (node->nosib) ? NULL : node->sibling ;
	}
	return max_len ;
} /* find_longest */

/* 'dirpath' is the full path name of the directory that is to be renamed by the
 * caller. Actually, 'dirpath' is the translated path name, i.e., substs, 
 * network names, etc have been un-referenced!
 * 'dirnode' is the PENTRY node in the tree. Note that it can't be NULL
 * as a root directory can't be renamed!!
 * 'len_difference' is the length difference between new name and old name!
 * On renaming a directory, certain deeply nested directories/files under
 * it might become un-accessible. This routine makes sure that such badness
 * won't ensue.
 */
BOOL path_len_check(char *dirpath, PENTRY dirnode, int len_difference)
{
	/* Is new_name_length > old_name_length? In this case, we
	 * have to worry about nesting problems ELSE if the problem was
	 * not existent before the rename, it won't be there now.
	 */
	if (len_difference > 0)
	{
		/* complex case, perform actual testing using tree traversals. */
		if (len_difference + strlen(dirpath) + find_longest(dirnode) > 
																ALLOWED_PATH)
			return FALSE ;
	}
	return TRUE ;
} /* path_len_check */

char far *gWriteBuffer ;
unsigned gWriteBufferSize ;
unsigned gWriteBufferFreeInd ;
unsigned gWriteErrs;

void SetUpWriteBuffer(unsigned low, unsigned high)
{
	gWriteBufferSize = AllocClosestTo1K(&gWriteBuffer, low, high) ;
	gWriteBufferFreeInd = 0 ;
} /* SetUpWriteBuffer */


/* Writes out any last piece that was still in buffer that needs to be written
 * out to disk.
 */
unsigned FlushBufferedWrite(int fhandle)
{
	unsigned dummy ;
	unsigned ret ;

	if (gWriteBuffer && (gWriteBufferFreeInd > 0) )
	{
		if((ret=_dos_write(fhandle, gWriteBuffer, gWriteBufferFreeInd, &dummy))
				|| dummy<gWriteBufferFreeInd)
			++gWriteErrs;
		gWriteBufferFreeInd = 0 ;
	}
	return ret ;

} /* FlushBufferedWrite */

unsigned Buffered_dos_write(int fhandle, char far *str, unsigned count,
		unsigned *pbytes)
{
	unsigned ret ;
	unsigned nLeft;

/* Were we un-successful in allocating storage to buffer writes? */
/* If so, do a direct write to disk.                                                     */
	if (!gWriteBuffer) {
		if((ret=_dos_write(fhandle, str, count, pbytes)) || *pbytes<count)
			++gWriteErrs;
		return(ret);
	}

	*pbytes = count;

/* While the new string to be added exceeds buffer bounds */
	nLeft = gWriteBufferSize - gWriteBufferFreeInd;
	while(count > nLeft) {
/* Add what we can to the string and flush the buffer */
		RepeatMove(gWriteBuffer+gWriteBufferFreeInd, str, nLeft);
		count -= nLeft;
		str += nLeft;
		gWriteBufferFreeInd = gWriteBufferSize;

		if(ret = FlushBufferedWrite(fhandle)) {
			*pbytes = 0 ;
			return(ret);
		}
		nLeft = gWriteBufferSize;
	}

	RepeatMove(gWriteBuffer+gWriteBufferFreeInd, str, count);
	gWriteBufferFreeInd += count ;
	return(0); /* indicate succesful write to caller. */
} /* Buffered_dos_write */


void Set_StartupItem(void);

/* INI file size buffer we try to allocate -- This is in Kbytes. Our test
 * INI file is less than 16K -- so this number should be good enuf. Of course
 * if the INI file happens to be larger than 20K, we would re-use this
 * buffer more than once.
 */
#define INI_SIZE_IN_K  20

BOOL gfUseINITdir; // write to INIT directory, or dosshell directory

/* This the routine that sets up the buffer to do the buffered writes and
 * the writes out the INI file.
 * WARNING! It is being assumed that the INI file will be written out at the
 * time of exiting the shell or launching a program -- In both cases the
 * shell relinquishes control to some other program. So, we can free up
 * almost all the memory that has been allocated to the shell at this point.
 * That way, we will be able to allocate a big enuf buffer to do the buffered
 * writes. Most of the memory that we allocate dynamically is being used
 * to store the tree/file data structures in the file manager.
 */
BOOL Buffered_Write_Ini_File(BOOL bFreeFM)
{
	unsigned wSaveAttr;
	int ind;
	char *envptr;
	char inifilename[128];
	char tchar;
	int tlen;

	Set_StartupItem();

/* If symbol table is not changed from what it was at startup, we don't
 * write it out!
 */
	if(!fgSymbolTableModified && fNoEffectiveChange())
	return(TRUE);

	if(!gfUseINITdir)
	{
usedefaultlocation:
	strfcpy(inifilename,gStartUpDir);
	ind = gStartUpDirEnd ;
#ifdef DBCS
	if(inifilename[ind-1] != '\\' || CheckDBCSTailByte(inifilename,&inifilename[ind-1]))
#else
	if(inifilename[ind-1] != '\\')
#endif
		inifilename[ind++] = '\\';
	strcpy(inifilename+ind, szShellIni);
	}
	else
	{
	
	envptr = getenv(szIniFileEnvVar);
	/* this should never fail since we have the gfUseINITdir var, but
	 * lets be sure!
	 */
	if(!envptr)
		goto usedefaultlocation;
	strfcpy(inifilename,envptr);
	/* be sure there is a trainling '\'! */
	tchar = inifilename[(tlen=strlen(inifilename))-1];
#ifdef DBCS
	if(((tchar != '\\') && (tchar != '/')) || CheckDBCSTailByte(inifilename,&inifilename[tlen-1]))
#else
	if((tchar != '\\') && (tchar != '/'))
#endif
	{
		inifilename[tlen] = '\\';
		inifilename[tlen+1] = 0;
	}
	strcat(inifilename,szShellIni);

	}
	if(_dos_getfileattr(inifilename, &wSaveAttr))
		wSaveAttr = 0;
	_dos_setfileattr(inifilename, 0);

/* If we are here, we are forced to write out INI file. We want to
 * have buffered writes to speed it up!
 */

/* Now free up the tree/file data structures */
	if(bFreeFM)
	FreeFMMemory() ;

	SetUpWriteBuffer(1, INI_SIZE_IN_K) ;
	if(!Write_Ini_File(inifilename))
	goto Error1;

	fgSymbolTableModified = FALSE;

Error1:
	FreeWorkFar(gWriteBuffer);
	gWriteBuffer = NULL;

	_dos_setfileattr(inifilename, wSaveAttr);

	return(!fgSymbolTableModified);
} /* Buffered_Write_Ini_File */

VOID Do_Read_Ini_File(void)
{
	int ind;
	char *envptr;
	char ininame[128];
	char tchar;
	int tlen;

	gfUseINITdir = TRUE;

	envptr = getenv(szIniFileEnvVar);
	if(envptr)
	{
		strfcpy(ininame,envptr);
		/* be sure there is a trainling '\'! */
		tchar = ininame[(tlen=strlen(ininame))-1];
#ifdef DBCS
		if(((tchar != '\\') && (tchar != '/')) || CheckDBCSTailByte(ininame,&ininame[tlen-1]))
#else
		if((tchar != '\\') && (tchar != '/'))
#endif
		{
			ininame[tlen] = '\\';
			ininame[tlen+1] = 0;
		}
		strcat(ininame,szShellIni);

	}
	if(!envptr || !Read_Ini_File(ininame))
	{
		gfUseINITdir = FALSE;
		ind = gStartUpDirEnd ;
#ifdef DBCS
		if(gStartUpDir[ind-1] != '\\' || CheckDBCSTailByte(gStartUpDir,&gStartUpDir[ind-1]))
#else
		if (gStartUpDir[ind-1] != '\\')
#endif
		{
			gStartUpDir[ind++] = '\\' ;
		}
		strcpy(gStartUpDir+ind,szShellIni);

		if(!Read_Ini_File(gStartUpDir))
			Read_Ini_File(NULL); //use defaults
		gStartUpDir[gStartUpDirEnd]=0;
	}
}

extern Shell_TTY_Out(char *str);

/* Prints the help text in response to the /H, /? option to the DOSSHELL */
void PrintHelpText(void)
{
	Shell_TTY_Out(szShellHelp1) ;
	Shell_TTY_Out(szShellHelp2) ;
	Shell_TTY_Out(szShellHelp3) ;
	Shell_TTY_Out(szShellHelp4) ;
	Shell_TTY_Out(szShellHelp5) ;   
   //no longer used Shell_TTY_Out(szShellHelp6) ;
} /* PrintHelpText */


/* This function returns whether extension 'ext' is a valid executable file.
 * It says whether "ext" refers to a .EXE, .COM, or .BAT file
 */
BOOL FIsExecutableFile(char far *ext)
{
	return ( (fstrncmp(ExeStr, ext, 3) == 0) ||
			 (fstrncmp(ComStr, ext, 3) == 0) ||
			 (fstrncmp(BatStr, ext, 3) == 0)
		   ) ;
} /* FIsExecutableFile */

#ifdef DBCS
/*
	DBCS enabled strupr
*/
unsigned char   *DBCSstrupr(unsigned char *str)
{
	unsigned char   *s;

	s = str;
	while (*s)
	{
		if (IsDBCSLeadByte(*s))
			s++;
		else
			*s = toupper(*s);
		s++;
	}
	return (str);
}

/*
	DBCS enabled strchr
*/
unsigned char   *DBCSstrchr(unsigned char *str,unsigned char c)
{
	while (*str)
	{
		if (IsDBCSLeadByte(*str))
			str++;
		else if (*str == c)
			return (str);
		str++;
	}
	if (c == '\0')
		return (str);
	else
		return 0;
}
#endif

char * PASCAL MySetMessageText(TMC tmc, char *message, int nWid)
{
	 int nLineLen ;
	char cSave;

	if((nLineLen=TextLineLen(message, nWid, '\0')) < 0)
	return(message);

	cSave = message[nLineLen];
	message[nLineLen] = '\0';
	Shell_SetTmcText(tmc, message);
	message += nLineLen;
	*message = cSave;

	while(*message=='\n' || *message=='\r')
	++message;
	return(message);
}

char *gMessageTitle; /* title to the message dialog box */
char *gMessage1;
BOOL FAR PASCAL FDlgmessage(WORD dlm, TMC tmc, WORD wNew, WORD wOld, WORD wParam)
{
	PWND lwind;
	int nWid;

	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	switch(dlm) {
	case dlmInit:
	SetUpDialog(tmcmessage1, gMessageTitle) ;

	SetUpButtonForGraphics(tmcCancel); /* this is called the enter button */

	lwind = PwndOfListbox(tmcmessage1);
	nWid = lwind->arcWindow.axRight - lwind->arcWindow.axLeft + 1;

	gMessage1 = MySetMessageText(tmcmessage1, gMessage1, nWid);
	gMessage1 = MySetMessageText(tmcmessage2, gMessage1, nWid);
	MySetMessageText(tmcmessage3, gMessage1, nWid);
	break;

	case dlmSetFocus:
	gCurrentTMC = tmc;
	break ;
	}
	return(TRUE);
}

/*
 * Generic message box. Input title and message
 */
VOID ShellMessageBox(char *messagetitle, char *message)
{
	HCABmessage h;

	if(!(h=HcabAlloc(cabiCABmessage))) {
	OutOfMemory();
	return;
	}
	InitCab(h, cabiCABmessage) ;

	gMessageTitle = messagetitle;
	gMessage1 = message;

	SzToCab(h, szClose, Iag(CABmessage, pszmessageEB));

	MyTmcDoDlg(&dlgmessage, h); 

	FreeCab(h);
} 

BOOL FAR PASCAL FDlgabout(WORD dlm, TMC tmc, WORD wNew, WORD wOld, WORD wParam)
{
	UnReferenced(tmc) ;
	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	switch(dlm)
	{
		case dlmInit:
		 {
			// WARNING see text.c (to skip a ~)
#ifdef KANJI
		   SetUpDialog(tmcCancel,szmHelpShellAbout) ;
#else
		   SetUpDialog(tmcCancel,&szmHelpAbout[1]) ;
#endif
		   SetUpButtonForGraphics(tmcCancel) ; /* this is called the enter button */
		 }
		case dlmSetFocus:
			gCurrentTMC = tmc;
			break ;
	}
	return(TRUE);
}

/*
 * About BOX
 * This is different from message because the spacing is wierdo
 */
VOID ShellAboutBox(void)
{
	HCABmessage h;

   h = HcabAlloc(cabiCABabout);
	if (!h)
	{
		OutOfMemory() ;
		return;
	}
	InitCab(h, cabiCABabout) ;

	SzToCab(h, szClose, Iag(CABabout, pszaboutEB));

	MyTmcDoDlg(&dlgabout,  h); 
	FreeCab(h);
} 


/* This function does a change directory to the directory specified by
 * 'dir'. Note that 'dir' needs to be a complete path name. For example,
 * a valid 'dir' is "C:\LSH\SHELL"
 * The reason for this is that the C-function "chdir()" does not do a
 * real change directory across drives but a UNIX' "cd" does this!
 */
int UnixChdir(char *dir)
{
	int TotalNumDrives ;
	int drive_ind ;

	/* Set drive ind by converting the drive letter to the appropriate
	 * drive number: 1 = drive A, 2 = drive B, etc.
	 */
	drive_ind = toupper(*dir) - 'A' + 1 ; // OK even if dir is NULL - see below

	/* Try to change to the directory only if we think the user has
	 * specified a valid absolute path -- DriveLetter followed by a COLON.
	 */
	if (dir && (drive_ind > 0) && (drive_ind < 27) && (dir[1] == ':') )
	{
		_dos_setdrive(drive_ind, &TotalNumDrives) ;

		return chdir(dir) ;
	}

	return -1 ; /* error */

} /* UnixChdir */

/*
 *      MakeDirNameFit translates a directory name and match pattern into
 * a string that will fit in maxlen characters.
 * The algorithm clips the end of the path name and puts in ellipses.
 * Thus, c:\foo\bar\cabbage\rabbits\*.foo fit in 16 characters goes to
 *       c:\foo\b..\*.foo
 * NOTE:
 *      pattern can be NULL; thus either no pattern or pattern is already
 *      
 * WARNING don't forget that this is characters displayed, not bytes used,
 * so destination must have room for NULL character!
 * WARNING! maxlen must never be < 8+3+1!
 */
VOID MakeDirNameFit(char *longname,char *pattern,char *destination,int maxlen)
{
	char    tempspace[128]; //be sure we don't tromp!
	int     longlen;
	int     templen;
	int     patternlen;
	int     outputlen;
	int     lasttermpathcharoffset,lasttermoffset;

	longlen = strlen(longname);
	strcpy(tempspace,longname);
	if(!pattern)
	{
		/* Is the name already within the bounds? If so trivial case! */
		if (longlen <= maxlen)
		{
			strcpy(destination,tempspace);
			return ;
		}

		/* pattern is NULL, so we figure out what the parent directory is and
		 * hence the pattern -- simplifies matters as we can follow our
		 * usual algorithm when the pattern is Non-Null.
		 */
		lasttermpathcharoffset = FindLastComponent(longname);
		if(longname[lasttermpathcharoffset] == PATHCHAR)
			lasttermoffset = lasttermpathcharoffset+1;
		else
			lasttermoffset = lasttermpathcharoffset;
		tempspace[lasttermpathcharoffset] = NULL;
		longlen = lasttermpathcharoffset;
		pattern = &longname[lasttermoffset];
	}
	patternlen = strlen(pattern);

	templen = longlen+patternlen+1; /* +1 for pathchar */
	if(templen > maxlen)
	{
		outputlen = patternlen+1+3; /* +1 for '\', +3 for '...' */

		assert(outputlen <= maxlen);

		/* put in ellipses */
		strcpy(&tempspace[maxlen-outputlen],szEllipses);
		tempspace[maxlen-outputlen+3] = (char) PATHCHAR ;
		tempspace[maxlen-outputlen+4] = '\0' ;
		if(pattern)
			strcpy(&tempspace[maxlen-outputlen+3+1],pattern);
	}
   else
	{
		if(pattern != NULL)     /* if there is a path to be appended */
		{
			/* put a '/' character at the end */
#ifdef DBCS
			if(tempspace[longlen-1] != PATHCHAR || CheckDBCSTailByte(tempspace,&tempspace[longlen-1]))
#else
			if(tempspace[longlen-1] != PATHCHAR)
#endif
			{
				tempspace[longlen] = PATHCHAR;
				++longlen;
			}
			strcpy(&tempspace[longlen],pattern);
		}
	}
	strcpy(destination,tempspace);
}


void Convert2AbsolutePath(char *dest, char *src)
{
	char *psz ;
	int pathlen ;
	PTREE tree ;
	BYTE dummy_driveind ;
	PENTRY dest_dir ;
	char TrueName[MAX_PATH+1] ;
	char *ptr ;
	int fTrailingPathChar;
	int len ;

	/* skip any leading spaces. */
	while ((*src == ' ') && (*src) )
		src++ ;

	/* remove any trailing blanks -- actually if path were of the form
	 * "ABC  DEF", etc this will cause it to become "ABC" which is fine.
	 */
	if (psz = strchr(src, ' '))
		*psz = '\0' ;

	/* Remember whether there was a trailing PATHCHAR in the path */
	len = strlen(src) ;
	fTrailingPathChar = FALSE ;
#ifdef DBCS
	if (len && (src[len-1] ==  '\\' && !CheckDBCSTailByte(src,&src[len-1])))
#else
	if (len && (src[len-1] ==  '\\'))
#endif
		fTrailingPathChar = TRUE ;

	/* Convert to upper case -- all path names, etc in DOS are upper case */
#ifdef DBCS
	DBCSstrupr(src) ;
#else
	strupr(src) ;
#endif

	/* Has the user specified a path like "\abc\cde", etc. In this case get
	 * the drive letter from the focus/selected dir.
	 */
	if (*src == '\\')
	{
		/* get the drive letter from tree */
		dest[0] = listinfo[glob.FocusBox].tree->root[0] ;
		dest[1] = ':' ;
		strcpy(dest+2, src) ;
	}
	/* If the user has specified a relative path. examples: "abc", "abc\def",
	 * "" (empty string), get selected/focus dir and create abs path.
	 */
	else if (!(*src) || (src[1] != ':') )
	{
		Tree2Path(listinfo[glob.FocusBox].tree, listinfo[glob.FocusBox].files,
														dest, &pathlen) ;

#ifdef DBCS
		if ( (dest[pathlen-1] != '\\' || CheckDBCSTailByte(dest,&dest[pathlen-1])) && (*src) )
#else
		if ( (dest[pathlen-1] != '\\') && (*src) )
#endif
			dest[pathlen++] = '\\' ;

		strcpy(dest+pathlen, src) ;
	}
	/* Has the user specified a drive letter? examples:
	 * "a:", "a:abcd", "a:\abc\def". Convert cases 1 and 2 to full path
	 * names, case 3 is already OK.
	 */
	else if (src[1] == ':')
	{
		tree = FindTree(src, &dummy_driveind) ;
		if (!tree)
		{
			/* tree not found! Just use root on that drive! */
			dest[0] = src[0] ; dest[1] = ':' ; dest[2] = '\\' ; dest[4] = '\0' ;
		}
		else
		{
			/* Is it already a full path specification (case 3 above)? Don't have
			 * to do anything special. Just copy src to dest.
			 */
			if (src[2] == '\\')
			{
				strcpy(dest, src) ;
			}
			else // convert relative path to full path (cases 1 and 2 above)
			{
				/* If the tree to which copy is requested is not the one in
				 * focus, use the default directory on that tree.
				 */
				if ( !(listinfo[glob.FocusBox].tree == tree) )
					dest_dir = tree->SelDir ; // default directory for tree
				else
					dest_dir = listinfo[glob.FocusBox].files ;

				Tree2Path(tree, dest_dir, dest, &pathlen);

				/* Is it of the form "a:abcd" (case 2 above)? */
				if (src[2])
				{
					/* If it is not the root directory append \ to it */
#ifdef DBCS
					if (dest[pathlen-1] != '\\' || CheckDBCSTailByte(dest,&dest[pathlen-1]))
#else
					if (dest[pathlen-1] != '\\')
#endif
						dest[pathlen++] = '\\' ;

					/* Copy all the stuff after the ':' from src */
					strcpy(dest+pathlen, src+2) ;
				}
				/* else path is of the form "a:" (case 1 above)! -- Don't
				 * have to do anything extra for this case. 'dest' is already
				 * set up right.
				 */
			
			} /* (src[2] == '\\') else */

		} /* if (!tree) else */
	}
	else
	{
		strcpy(dest, src) ;
	}

	/* Now this path in "dest" could still have '.' and '..' Get rid of these
	 * and handle this right.
	 */
	/* If translate name was succesful, remove the server name, if any
	 * and put back the drive letter.
	 */
	if (!translate_name(dest, TrueName))
	{
		/* Is there a server name in the beginning like \\trojan\os1?
		 * that is, are the first 2 chars \\?
		 */
		if ( (* ((WORD *)TrueName)) == 0x5C5C )
		{
#ifdef DBCS
			ptr = DBCSstrchr(TrueName+2, '\\') ;
#else
			ptr = strchr(TrueName+2, '\\') ;
#endif

			assert(ptr != NULL) ;

#ifdef DBCS
			ptr = DBCSstrchr(ptr+1, '\\') ;
#else
			ptr = strchr(ptr+1, '\\') ;
#endif
			if (!ptr)
			{
				// The first 3 chars are already OK as 'dest' is an abs path!
				dest[3] = '\0' ; // ROOT dir case.
			}
			else
				strcpy(dest+2, ptr) ; // replace server name by drive letter!
		}
		else
			strcpy(dest, TrueName) ; // got rid of '.' and '..' if any!
	}
	/* else translate_name failed -- leave dest alone */
	
	/* If the source had a trailing PATHCHAR and after the name translation
	 * this character is lost, put it back at the end.
	 */
	if (fTrailingPathChar)
	{
		len = strlen(dest) ;
#ifdef DBCS
		if (dest[len-1] != '\\' || CheckDBCSTailByte(dest,&dest[len-1]))
#else
		if (dest[len-1] != '\\')
#endif
		{
			dest[len++] = '\\' ;
			dest[len] = '\0' ;
		}
	}

} /* Convert2AbsolutePath */

/* This function decides whether we need to perform the requested file
 * operation (like delete, copy, move, etc) on the files selected in the
 * file listbox with focus or on the files selected in the entire tree.
 */
#if 0
BOOL FPerformOpOnDirFilesAlone(void)
{
	return (	(glob.TreeMode == TR_DOUBLE) &&
				(listinfo[0].tree == listinfo[1].tree) &&
				(!glob.CrossDirSel)
			 ) ;
} /* FPerformOpOnDirFilesAlone */
#else
BOOL FPerformOpOnDirFilesAlone(void)
{
	return (	(!glob.CrossDirSel) &&
				(glob.TreeMode != TR_SYSTEM) &&
				( (glob.TreeMode != TR_SEARCH) || (!gfSearchDisk) )
			 ) ;
} /* FPerformOpOnDirFilesAlone */
#endif


/* This function decides whether to perform a directory operation or not.
 * In general, if the focus is in the directory listbox or on the
 * drive lists, we say that it is a directory operation.
 */
BOOL FPerformDirOperation(void)
{
	BOOL ret ;
	WORD FocusList ;

	FocusList = WhoHasGlobalFocus() ;
	
	/* Don't allow performing of directory ops in SYSTEM/SEARCH mode! */
	if ( (glob.TreeMode == TR_SYSTEM) || (glob.TreeMode == TR_SEARCH) )
		return FALSE ;

	ret = (FocusList == DRIVELIST0) || (FocusList == TREE0) ;

	if ( (!ret) && (glob.TreeMode == TR_DOUBLE) )
	{
		ret = (FocusList == DRIVELIST1) || (FocusList == TREE1) ;
	}

	return ret ;

} /* FPerformDirOperation */

/* If we have an empty file listbox, move focus to the corresponding
 * directory listbox. This can happen after file operations, like
 * Move, Delete, File Display Options, Change Attributes.
 */
void HandleEmptyFileListBoxes(void)
{
	if (WhoHasGlobalFocus() == FILE0)
	{
		if (GetNumItems(&FileList[0]) == 0)
			InitGlobalFocus(TREE0) ;
	}
	else if ( (glob.TreeMode == TR_DOUBLE) && (WhoHasGlobalFocus() == FILE1) )
	{
		if (GetNumItems(&FileList[1]) == 0)
			InitGlobalFocus(TREE1) ;
	}
	
} /* HandleEmptyFileListBoxes */

/* This function can be easily described by giving examples of what it
 * does:
 *        Input:        FormStringWithoutPlaceHolders(dest,"%1 loves %2",(char far *)"Rama",(char far *)"Sita") ;
 *        Output: "Rama loves Sita"
 *        Input:        FormStringWithoutPlaceHolders(dest,"%2 %1 loves",(char far *)"Rama",(char far *)"Sita") ;
 *   Output: "Sita Rama loves"
 *
 * Note that there is really no limit on the number of replaceable arguments
 * However, all replaceable parameters need to be strings (far pointers)
 * and say there is a %5 there should be atleast 5 replaceable arguments
 * otherwise, this function will use garbage from the stack!
 * dest is the location where the string formed is placed.
 * src can be without any placeholders (%1, %2, etc)
 */
void cdecl FormStringWithoutPlaceHolders(char far *dest, char far *src, ...)
{
	int IthArg ;
	char far *replacestr ;

	/* Do until we reach the end of source (EOS (null) char) */
	while(*dest++ = *src)
	{
		// Increment source as we have only incremented destination above
		if(*src++ == '%')
		{
			if (*src == '%')
			{
				/* One can use %% to get a single percentage char in message */
				src++ ;
				continue ;
			}

			dest--; // dest has been INCed one too much.

			// Get which argument it is: like 2 for %2
			IthArg = *src++ - '0' + 1 ;

			/* Get the address of the replacing string from the stack.
			* Note that in 'C' the last argument is pushed first. Also,
			* here, the stack grows from high address to low address.
			*/
			replacestr = *((&dest)+ IthArg) ;

			// Copy the replacement string into destination
			while (*replacestr)
				*dest++ = *replacestr++ ;
		}
	} /* while */
} /* FormStringWithoutPlaceHolders */

char *gpszMouseVerMsg ;

// M011 Modified this function to put up our new mouse compatibility message!

BOOL FAR PASCAL FDlgmswarn(WORD dlm, TMC tmc, WORD wNew, WORD wOld, WORD wParam)
{
	PWND lwind ;
	char *psztemp ;
	int itemwidth ;

	UnReferenced(tmc) ;
	UnReferenced(wNew) ;
	UnReferenced(wOld) ;
	UnReferenced(wParam) ;

	switch(dlm)
	{
		case dlmInit:
		 {
		   SetUpDialog(tmcCancel,szCritWarning) ;
		   SetUpButtonForGraphics(tmcCancel) ; /* this is called the enter button */
		   SetUpButtonForGraphics(tmcenablemouse) ; /* this is called the enter button */

			lwind = PwndOfTmc(tmcmousevermsg1);
			itemwidth = lwind->arcWindow.axRight - lwind->arcWindow.axLeft ;

			/* This message is allowed to overflow onto a 2nd line */
	      psztemp = MySetMessageText(tmcmousevermsg1, gpszMouseVerMsg,
																					itemwidth);
			MySetMessageText(tmcmousevermsg2, psztemp, itemwidth) ;
		 }
		case dlmSetFocus:
			gCurrentTMC = tmc;
			break ;
	}
	return(TRUE);
}

// M011 Modified this function to put up our new mouse compatibility message!

TMC WarnMouseIsOld(char *pszMouseVersion)
{
	HCABmswarn h;
	TMC  retval;
	char MouseVersionMsg[161] ;
	
	if(!(h=HcabAlloc(cabiCABmswarn))) {
	OutOfMemory();
	return tmcCancel ; // don't use the mouse!
	}
	InitCab(h, cabiCABmswarn) ;

	// M013 If mouse version is unknown -- Major version number is 0, we
	// we use the string unknown for the version number!
	if(pszMouseVersion[0] == '0')
		pszMouseVersion = UnknownMouse;

	FormStringWithoutPlaceHolders(MouseVersionMsg, MouseVersionMsgTemplate,
											(char far *)pszMouseVersion ) ;

	/* use the following to pass this string to dilaog proc FDlgmswarn */
	gpszMouseVerMsg = MouseVersionMsg ;

	retval = MyTmcDoDlg(&dlgmswarn, h); 

	FreeCab(h);
	 return(retval);
} 


