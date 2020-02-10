/***************************************************************************
 * AUTOCONF.C
 *
 * Microsoft Confidential
 * Copyright (c) Microsoft Corporation 1990-1991
 * All Rights Reserved
 *
 * Functions for creating a config.sys and autoexec.bat file for the DOS
 * 5.0 OEM installation program.
 *
 * Change log:
 *
 *   	Date       #		Description
 * 	--------  ----	----------------------------------------------------
 *      02/28/90        Created
 *      02/27/91  M001  Added codes.h.
 *      03/21/91  M003  If Country installed Code Page does not match Country
 *                      default Code Page, add Code Page to "COUNTRY="
 *			statement.
 ***************************************************************************/

#include    <stdio.h>
#include    <stdlib.h>
#include    <memory.h>
#include    <string.h>
#include    <dos.h>
#include    <io.h>
#include    <malloc.h>

#include    <oem.h>
#include    <autoconf.h>
#include    <codes.h>                   /* M001 */
#include    <alias.h>
#include    <bios_io.h>
#include    <disk_io.h>
#include    <strlib.h>
#include    <window.h>
#include    <message.h>

/***************************************************************************/

#define     FILE_BUF_SIZE     2048     /* Size of file buffer in bytes     */

/***************************************************************************/

char  *apszFile[] = { "X:\\CONFIG.SYS", "X:\\AUTOEXEC.BAT" };
char  *apszOld[]  = { "X:\\CONFIG.OLD", "X:\\AUTOEXEC.OLD" };

static char *szBuf;                          /* Buffer to build file in    */
static char *szEndLine = "\r\n";             /* End of line characters     */

/*
** Signals to CreatAutoexec() from CreatConfig() that display.sys has been
** loaded.
*/
static int  fDisplayLoaded = FALSE;
static int  fEGADisplayLoaded = FALSE;

/***************************************************************************/

extern void AutoConfig(void);
static unsigned CreatAutoexec(void);
static unsigned CreatConfig(void);

static char chBootDrv;

/***************************************************************************/
/* Main function for creating autoexec.bat and config.sys files. First     */
/* renames an existing file of the same name to *.OLD and then call a      */
/* function for each of the files.                                         */
/*                                                                         */
/* void AutoConfig( void )                                                 */
/*                                                                         */
/* ARGUMENTS:  NONE                                                        */
/* RETURNS:    void                                                        */
/*                                                                         */
/***************************************************************************/

void AutoConfig( void )
{
   char              *apszError[ ERROR_LINES ];
   register          i;
   register          iStatus;
   int               iFile;
   unsigned          uToWrite;
   unsigned          uWritten;

   static unsigned   Errors[] = { CONFIG_SYS_ERROR, AUTOEXEC_BAT_ERROR };
   /*
   ** n.b., CreatConfig() sets some flags which CreatAutoexec() requires.
   ** So CreatConfig() must be called before CreatAutoexec().
   */
   static unsigned   (*Func[])(void) = { CreatConfig, CreatAutoexec };

   chBootDrv = vInfo.chDestin;
   if ( chBootDrv != vInfo.chFirstHd )
      chBootDrv = 'A';

   szBuf = GetMemory( FILE_BUF_SIZE );
                                                /* Loop once for each file */
   for ( i = 0; i < 2; i++ )
   {
      apszOld[i][0] = apszFile[i][0] = vInfo.chDestin;

                                    /* There may be old files so save them */

      _dos_setfileattr( apszOld[i], _A_NORMAL );
      unlink( apszOld[i] );
      rename( apszFile[i], apszOld[i] );

      uToWrite = (*Func[i])();            /* Create the text for the file  */

                                          /* Next create the file          */

      if ( uToWrite > 0 )
      {
         DisplayFileStatus( apszFile[i]+ 3, WRITE );
         iStatus = ERROR;
         if ( _dos_creat( apszFile[i], 0, &iFile ) == OK )
         {
            if ( _dos_write( iFile, szBuf, uToWrite, &uWritten )  == OK &&
                 uWritten == uToWrite )
               iStatus = OK;

            iStatus |= (int)_dos_close( iFile );
         }
         if ( iStatus != OK )
         {
            GetMessage( apszError, Errors[i] );
            Error( apszError );
         }
      }
   }

   FreeMemory( szBuf );
}

/***************************************************************************/
/* Creates a new CONFIG.SYS file in the file buffer. First checks for      */
/* extended memory and if available forces DOS to load into the high       */
/* memory area.  Next a check is done to see if a COUNTRY line should be   */
/* added.                                                                  */
/*                                                                         */
/* unsigned CreatConfig( void )                                            */
/*                                                                         */
/* ARGUMENTS:  NONE                                                        */
/* RETURNS:    unsigned - Number of bytes in the new file.                 */
/*                        NOTE: this number may be zero which will signal  */
/*                              that no CONFIG.SYS file will be created.   */
/*                                                                         */
/***************************************************************************/

unsigned CreatConfig( void )
{
   char        *szPtr, *szDOSPath;
   unsigned    fIsConvertible;
	char			szTmp[20];

   szPtr = szBuf;
   *szPtr = EOL;
   szDOSPath = vInfo.szPath + 2;

	strcat( szPtr, DEVICE_STR );                    /* "DEVICE=SETVER.EXE"	*/
   szPtr = strchr( szPtr, EOL );
   BuildPath( szPtr, chBootDrv, szDOSPath, SETVER_STR );
   strcat( szPtr, szEndLine );

                     /* If extended memory is available put DOS in himem	*/
   if ( vInfo.Hw.Cpu > 0 && vInfo.Hw.ExtMem >= HIMEM_K_SIZE )
   {
      strcat( szPtr, DEVICE_STR );                    /* "DEVICE=HIMEM"    */
      szPtr = strchr( szPtr, EOL );
      BuildPath( szPtr, chBootDrv, szDOSPath, HIMEM_STR );
      strcat( szPtr, szEndLine );
      strcat( szPtr, XMA_STR );                       /* "DOS=HIGH"        */
      strcat( szPtr, szEndLine );
   }

	if ( vInfo.Hw.VideoType == EGA_DISPLAY )
	{
		strcat( szBuf, DEVICE_STR );							/* "DEVICE=EGA.SYS"  */
		szPtr = strchr( szBuf, EOL );
		BuildPath( szPtr, chBootDrv, vInfo.szPath + 2, EGA_SYS );
      strcat( szPtr, szEndLine );
	}

   /* If country is not the default add "COUNTRY=" line.  */
   /* n.b., we need to install NLSFUNC only to do a CHCP. */
   if ( vInfo.Country != COUNTRY_NONE )
   {
      strcat( szPtr, COUNTRY_STR );                   /* "COUNTRY="        */
      strcat( szPtr, CountryCode[vInfo.Country].pszName ); /* Country code */

      /* M003: If Country installed Code Page does not match Country
       *       default Code Page, add Code Page to "COUNTRY=" statement.
       */
      strcat( szPtr, SEPARATOR_STR );
      if( CountryCode[vInfo.Country].uCP1 != CountryCode[vInfo.Country].uCP2 )
         strcat( szPtr, itoa(CountryCode[vInfo.Country].uCP1, szTmp, 10) );
      strcat( szPtr, SEPARATOR_STR );

      if ( vInfo.Flag.fHardInstall )
      {
         szPtr = strchr(szPtr, EOL);
         BuildPath( szPtr, chBootDrv, szDOSPath, COUNTRYDRV_STR );
      }
      else
         strcat( szPtr, COUNTRYDRV_STR );

      strcat( szPtr, szEndLine);

      /*
      ** Add DISPLAY.SYS for EGA, VGA, and LCD displays, but not for
      ** monochrome, CGA, or Hercules displays.
      **
      ** EGA and VGA displays use EGA parameter for con.
      ** IBM PC Convertibles use LCD parameter for con.
      */

      /*
      ** n.b., the IsConvertible() call must come first in this predicate.
      ** We must make certain the value of fIsConvertible has always been set,
      ** since we use it inside the body.
      */
      if ( (fIsConvertible = IsConvertible()) ||
           (vInfo.Hw.VideoType == EGA_DISPLAY) ||
           (vInfo.Hw.VideoType == EGA_MONO_DISPLAY) ||
           (vInfo.Hw.VideoType == VGA_DISPLAY) ||
           (vInfo.Hw.VideoType == VGA_MONO_DISPLAY) )
      {
         strcat(szPtr, DEVICE_STR);
         szPtr = strchr(szPtr, EOL);
         BuildPath(szPtr, chBootDrv, szDOSPath, DISPLAY_START_STR);

         /* Tell CreatAutoexec() we're loading display.sys in config.sys. */
         fDisplayLoaded = TRUE;

         if (fIsConvertible)
            strcat(szPtr, LCD_STR);
         else
         {
            /*
            ** Tell CreatAutoexec() we're loading the EGA version of
            ** display.sys in config.sys.
            */
            fEGADisplayLoaded = TRUE;
            strcat(szPtr, EGA_STR);          /* EGA or VGA -- both use EGA */
         }

         strcat(szPtr, DISPLAY_END_STR);
         strcat(szPtr, szEndLine);
      }
   }

   strcat( szPtr, FILES_STR );                        /* FILES= line       */
   strcat( szPtr, szEndLine );

   #ifdef   STACKS_STR
      strcat( szPtr, STACKS_STR );                    /* STACKS= line      */
      strcat( szPtr, szEndLine );
   #endif


#ifdef JAPAN			/* ### if JAPAN ### */

// Add ANSI.SYS
      strcat(szPtr, DEVICE_STR);
      szPtr = strchr(szPtr, EOL);
      BuildPath(szPtr, chBootDrv, szDOSPath, ANSI_STR);

/* if KKC */
//
//	Install Kana Kanji Convert Device Driver
//

   if (vInfo.Flag.fKKC)		/* If Install KKC */
   {

// Add KKCFUNC.SYS
      strcat(szPtr, KKC_REM_STR);
      strcat(szPtr, DEVICE_STR);
      szPtr = strchr(szPtr, EOL);
      BuildPath(szPtr, chBootDrv, szDOSPath, KKCFUNC_STR);

// This is just a sample for OEM, so we store 'REM' here
      strcat(szPtr, KKC_REM_STR);

// At here 'REM DEVICE=A:\KKC.SYS" will be made
      strcat(szPtr, DEVICE_STR);
      szPtr = strchr(szPtr, EOL);
      BuildPath(szPtr, chBootDrv, szDOSPath, KKC_STR);

// Add option for input mode '/M0','/M1','/M2','/M3','/M4' or '/M5'
      strcat(szPtr, KKC_INPUT_STR);
      szPtr = strchr(szPtr, EOL);
      *(szPtr) = vInfo.KKC.input + '0';
      *(szPtr+1) = EOL;

// Add option for convert mode '/HB', '/HS' or '/HR'
      if (vInfo.KKC.convert == 0)
         strcat(szPtr, KKC_CONVERT0_STR);
      else if (vInfo.KKC.convert == 1)
         strcat(szPtr, KKC_CONVERT1_STR);
      else if (vInfo.KKC.convert == 2)
         strcat(szPtr, KKC_CONVERT2_STR);

// Add option for dictionary learning mode '/L'
      if (vInfo.KKC.learn)
         strcat(szPtr, KKC_LEARN_STR);

// Add options for dictionary drive '/A:'
      strcat(szPtr, KKC_DRIVE_STR);
      szPtr = strchr(szPtr, EOL);
      *(szPtr) = vInfo.KKC.drive + 'A';
      *(szPtr+1) = ':';
      *(szPtr+2) = EOL;

// Add option for code system '/J'
      if (vInfo.KKC.code)
         strcat(szPtr, KKC_CODE_STR);

// Add option for system directory '/SYS=A:\'
      strcat(szPtr, KKC_SYS_STR);
      szPtr = strchr(szPtr, EOL);
      BuildPath(szPtr, chBootDrv, szDOSPath, KKC_CRLF_STR);

   }


#endif				/* ### end if JAPAN ### */

   szPtr = strchr( szBuf, EOL );
   return( (UINT)(szPtr - szBuf) );
}

/***************************************************************************/
/* Creates a new AUTOEXEC.BAT file in the file buffer. First adds all of   */
/* the standard items which are used on all systems and then based on the  */
/* user options may add the keyboard driver, mouse or dos shell.           */
/*                                                                         */
/* unsigned CreatAutoexec( void )                                          */
/*                                                                         */
/* ARGUMENTS:  NONE                                                        */
/* RETURNS:    unsigned - The number of bytes in the new file.             */
/*                                                                         */
/***************************************************************************/

unsigned CreatAutoexec( void )
{
   char     *szPtr, *szDOSPath;
	char		szTmp[20];

   szPtr = szBuf;
   *szPtr = EOL;
   szDOSPath = vInfo.szPath + 2;

   /* Load for 286's and up with enough memory */
   if ( vInfo.Hw.Cpu >= 1 && vInfo.Hw.ExtMem >= SMARTDRV_MIN_K )
   {
       BuildPath( szPtr, chBootDrv, szDOSPath, SMARTDRV_STR );
       strcat( szPtr, szEndLine );            /* "DEVICE=SMARTDRV.EXE"     */
   }
   
   strcat( szPtr, AUTO_STR );             /* Add standard autoexec items   */
   
   if ( vInfo.Flag.fNoClock )             /* If no clock installed add a   */
      strcat( szPtr, TIME_DATE_STR );     /* time and date string          */

                                          /* Append the dos path string    */
                                          /* and TEMP= string              */
   if ( vInfo.Flag.fHardInstall )
   {
      strcat( szPtr, PATH_STR );
      szPtr = strchr( szPtr, EOL );
      BuildPath( szPtr, chBootDrv, szDOSPath, "" );
      RemoveTrailing( szPtr + 3, '\\' );  /* Remove trailing separator     */
      strcat( szPtr, szEndLine );         /* Add the CR LF characters      */

      strcat( szPtr, TEMP_STR );
      szPtr = strchr( szPtr, EOL );
      BuildPath( szPtr, chBootDrv, szDOSPath, "" );
      RemoveTrailing( szPtr + 3, '\\' );  /* Remove trailing seperater     */
      strcat( szPtr, szEndLine );         /* Add the CR LF characters      */
   }

   /* If keyboard is not the default, install KEYB. */
   if (( vInfo.Keyboard != KEYB_NONE ) || (vInfo.Country != COUNTRY_NONE))
   {
      /*
      ** If display.sys was loaded in config.sys, add codepage support for
      ** KEYB in autoexec.bat.
      */
      if (fDisplayLoaded)
      {
                                          /* Add MODE CON CP PREP... line. */
         strcat(szPtr, CP_START_STR);
         strcat(szPtr, CP_PREP_STR);

         /* Add appropriate country codepage for KEYB. */
         strcat(szPtr, itoa(CountryCode[vInfo.Country].uCP1, szTmp, 10));

         strcat(szPtr, CP_MID_STR);

         szPtr = strchr(szPtr, EOL);
         BuildPath(szPtr, chBootDrv, szDOSPath,
                   fEGADisplayLoaded ? EGA_STR : LCD_STR);

         strcat(szPtr, CP_END_STR);
         strcat(szPtr, szEndLine);

                                        /* Add MODE CON CP SELECT... line. */
         strcat(szPtr, CP_START_STR);
         strcat(szPtr, CP_SELECT_STR);

         /* Add appropriate country codepage for KEYB. */
         strcat(szPtr, itoa(CountryCode[vInfo.Country].uCP1, szTmp, 10));
         strcat(szPtr, szEndLine);
      }

      if (vInfo.Keyboard != KEYB_NONE)
      {
          /* Add KEYB line. */
          strcat(szPtr, KEYB_STR);
          strcat(szPtr, KeybCode[vInfo.Keyboard].pszName);    /* Keyboard code */
          strcat(szPtr, NULL_OPT_STR);
          szPtr = strchr(szPtr, EOL);
          BuildPath(szPtr, chBootDrv, szDOSPath, KEYBDRV_STR);
          strcat(szPtr, szEndLine);
      }
   }

#ifdef INSTALL_MOUSE
                                                   /* See if adding mouse  */
   if ( vInfo.Flag.fMouse )
   {
      if ( vInfo.Flag.fHardInstall )
      {
         szPtr = strchr( szPtr, EOL );
         BuildPath( szPtr, chBootDrv, szDOSPath, MOUSE_STR );
      }
      else
         strcat( szPtr, MOUSE_STR );

      strcat( szPtr, szEndLine );
   }
#endif
                                                /* See if adding dos shell */
   if ( vInfo.Flag.fShell && vInfo.Flag.fHardInstall )
   {
      szPtr = strchr( szPtr, EOL );
      BuildPath( szPtr, chBootDrv, szDOSPath, SHELL_STR );
      strcat( szPtr, szEndLine );
   }

   szPtr = strchr( szPtr, EOL );
   return( (UINT)(szPtr - szBuf) );
}

