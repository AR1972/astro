/***************************************************************************/
/*                                                                         */
/* SWITCH.C                                                                */
/*                                                                         */
/*    Copyright (c) 1991 - Microsoft Corp.                                 */
/*    All rights reserved.                                                 */
/*    Microsoft Confidential                                               */
/*                                                                         */
/* Command line switch processing functions for DOS/WINDOWS merge.         */
/*                                                                         */
/***************************************************************************/


#include    <stdio.h>
#include    <stdlib.h>
#include    <dos.h>
#include    <string.h>
#include    <fcntl.h>
#include    <share.h>

#include    <alias.h>
#include    <bios_io.h>
#include    <disk_io.h>
#ifdef UPGRADE_PROGRAM
   #include    <global.h>
#else
   #include    <oem.h>
#endif
#include    <message.h>
#include    <strlib.h>
#include    <window.h>

#include    <install.h>

/************************************************************************/

#define     SWITCH_CHAR    '/'

#if (UPGRADE_PROGRAM != 0) && (UJANUS != 0)
char szNuCmdLine[MAXSTRLEN] = { EOL }; /* Non-Upgrade Setup command-line */
#endif

static void near  InvalidSwitch  ( char *szArgStr );
static void near  CommandHelp    ( void );
static void near  InvalidSwitchFmt( char *szArgStr );
static int  near  ExtractSwitchPath( char *szArg, char *szBuf );


/************************************************************************/
/*                                                                      */
/* void ProcessSwitches( int argc, char **argv )                        */
/*                                                                      */
/* ARGUMENTS:  argc  -  Number of command line switches.                */
/*             argv  -   Array of ptrs to command line switch strings   */
/* RETURNS  :  void                                                     */
/*                                                                      */
/*                                                                      */
/************************************************************************/

void ProcessSwitches( int argc, char **argv )
{
   register    i;
   int         fDosSw;                    /* Dos-only switch */
   int         fWinSw;                    /* Win-only switch */
   char        *szPtr;
   char        *szCmdLine;
   char        *szWinCmdLine;
   char        szTmp[MAXSTRLEN];
#ifdef UPGRADE_PROGRAM
   int         fUpgSw;                    /* Upgrade-only switch */
#endif

   /* Init. Install structure members. */

   Install.Software = SOFTWARE_DEFAULT;
   Install.Method = METHOD_EXPRESS;
   Install.Done.fWinInf = FALSE;
   Install.Done.fNet = FALSE;
   Install.Done.fBackup = FALSE;
   Install.Flags.fAdmin = FALSE;

#ifdef UPGRADE_PROGRAM
   Install.Flags.fUninstall = TRUE;
#else
   Install.Flags.fUninstall = FALSE;
#endif

   szCmdLine = Install.szCmdLine;         /* Original Setup command-line tail */
   szCmdLine[0] = EOL;                    /* Ensure NULL terminated. */
   szWinCmdLine = Install.szWinCmdLine;   /* Win Setup command-line tail */
   szWinCmdLine[0] = EOL;                 /* Ensure NULL terminated. */

   for ( i = 1; i < argc; i++ )
   {
      szPtr = argv[i] + 1;

      if ( strchr( szPtr, SWITCH_CHAR ) || strchr( szPtr, '-' ) )
         InvalidSwitchFmt( argv[i] );

      if ( argv[i][0] != SWITCH_CHAR && argv[i][0] != '-' )
      {
         InvalidSwitch( argv[i] );
      }

      fDosSw = fWinSw = FALSE;         /* Assume neither */

#ifdef UPGRADE_PROGRAM
      fUpgSw = FALSE;                  /* Assume not Upgrade-only switch */
#endif

      switch( toupper( *szPtr ) )
      {
#ifdef JANUS
         case  'A':                    /* Admin. Setup */
            fWinSw = TRUE;             /* Win-only */
            Install.Software = SOFTWARE_WINONLY;
            Install.Flags.fAdmin = TRUE;
            break;
#endif

         case  'B':        				/* Don't use color in displays   */
            fDosSw = fWinSw = TRUE;
            vInfo.Args.fIsMono = TRUE;
#ifdef OEM_PROGRAM
				vInfo.fOemCmdLineOK = TRUE;	/* Don't need to boot setup */
#endif
            break;

#ifdef JANUS
         case  'C':
            fWinSw = TRUE;             /* Win-only */
            break;
#endif

#ifdef BACKUP_SUPPORT
         case  'D':                    /* Set by hdbkup.exe (not used)  */
            fDosSw = TRUE;             /* Dos-only */
            vInfo.Args.fDoneBackup = TRUE;
            break;
#endif

#ifndef OEMBASE
         case  'E':                    /* Extras -- Maintenance mode */
            fDosSw = TRUE;             /* Dos-Only */
            Install.Flags.fMaintenance = TRUE;  /* Turn on Maintenance mode flag */
#ifdef OEM_PROGRAM
				vInfo.fOemCmdLineOK = TRUE;	/* Don't need to boot setup */
#endif
            break;
#endif	/* ifndef OEMBASE */

#ifndef UJANUS
         case  'F':                    /* Create MS-DOS emergency boot floppy */
            fDosSw = TRUE;             /* Dos-Only */
            vInfo.Args.fFloppy = TRUE;
#ifdef OEM_PROGRAM
				vInfo.fOemCmdLineOK = TRUE;	/* Don't need to boot setup */
#endif
            break;
#endif	/* ifndef UJANUS */

#ifdef UPGRADE_PROGRAM
         case  'G':                    /* Skip Uninstall procedure */
            fUpgSw = TRUE;             /* Upgrade-only */
            fDosSw = TRUE;             /* Affects only Dos Setup */
            Install.Flags.fUninstall = FALSE;
            Install.Done.fNet = TRUE;     /* Skip Net Warning screen */
            Install.Done.fBackup = TRUE;  /* Skip Backup procedure */
            break;
#endif

#ifdef UPGRADE_PROGRAM
         case  'H':
            fDosSw = fWinSw = TRUE;

            /* Change to Batch install unless we must do Custom install */
            if ( Install.Method != METHOD_CUSTOM )
            {
               Install.Method = METHOD_BATCH;
               Install.Done.fNet = TRUE;     /* Skip Net Warning screen */
               Install.Done.fBackup = TRUE;  /* Skip Backup procedure */
            }
            break;
#endif

#ifdef UPGRADE_PROGRAM
         case  'I':                    /* Use only basic video check    */
            fWinSw = TRUE;
            Install.Method = METHOD_CUSTOM;  /* Force custom install    */
            fDosSw = TRUE;
            vInfo.Args.fNoVideoChk = TRUE;
            break;
#endif

#ifdef UPGRADE_PROGRAM
         case  'J':                    /* Disable certain features. */
            fUpgSw = TRUE;             /* Upgrade-only */
            fDosSw = TRUE;             /* Affects only Dos Setup */
            Install.Done.fNet = TRUE;     /* Skip Net Warning screen */
            Install.Done.fBackup = TRUE;  /* Skip Backup procedure */
            break;
#endif

#ifdef DEBUG
         case  'L':                    /* DEBUG switch */
            fWinSw = TRUE;             /* Win-only */
            break;
#endif

#ifdef UPGRADE_PROGRAM
         case  'M':                    /* Do minimum MS-DOS install to harddisk */
            fUpgSw = TRUE;             /* Upgrade-only */
            fDosSw = TRUE;             /* Dos-only */
            vInfo.Args.fMininum = TRUE;
            Install.Software = SOFTWARE_DOSONLY;
            break;
#endif

#if 0 /* Cannot do SETUP /N from distribution disks; must do SETUP /A first */
         case  'N':                    /* Net setup */
            fWinSw = TRUE;             /* Win-only */
            Install.Software = SOFTWARE_WINONLY;
            break;
#endif

#ifdef JANUS
         case  'O':
            fWinSw = TRUE;             /* Win-only */
            Install.Done.fWinInf = TRUE;
            break;
#endif

#ifdef UPGRADE_PROGRAM
         case  'Q':                    /* Manual installation */
            vInfo.Args.fManual = TRUE;
            Install.Flags.fUninstall = FALSE;   /* turn off uninstall */
            Install.Software = SOFTWARE_DOSONLY;    /* don't load Win opts. */
            fDosSw = TRUE;             /* DOS only */
            break;
#endif

#ifdef BACKUP_SUPPORT
         case  'R':                    /* Set by hdbkup.exe (not used)  */
            fDosSw = TRUE;             /* Dos-only */
            vInfo.Args.fRootChkDone = TRUE;
            break;
#endif

#ifdef UPGRADE_PROGRAM
         case  'S':                    /* Specifies path to setup files */
            fWinSw = TRUE;
            fDosSw = TRUE;
            Install.Done.fSrcPath = TRUE;
            if ( ExtractSwitchPath( argv[ i ], szTmp ) != OK )
               InvalidSwitchFmt( argv[ i ] );
            strcpy( vInfo.szSource, szTmp );
            vInfo.chSource = vInfo.szSource[ 0 ];
            break;
#endif

#ifdef JANUS
         case  'T':
            fWinSw = TRUE;             /* Win-only */
            break;
#endif

#ifdef UPGRADE_PROGRAM
         case  'U':
            fDosSw = TRUE;             /* Dos-only */
            vInfo.Args.fAllowBadPart = TRUE;
            break;
#endif

#ifdef DEBUG
         case  'V':                    /* DEBUG switch */
            fWinSw = TRUE;             /* Win-only */
            break;
#endif

#ifdef JANUS
         case  'W':                    /* Do Win-Only Setup; don't pass switch */
            fUpgSw = TRUE;             /* Upgrade-only */
            Install.Software = SOFTWARE_WINONLY;
            break;
#endif

#ifdef DEBUG
         case  'X':                    /* DEBUG switch */
            fWinSw = TRUE;
            break;
#endif

#ifdef JANUS
         case  'Y':
            fWinSw = TRUE;             /* Win-only */
            break;
#endif

#ifdef UPGRADE_PROGRAM
         case  'Z':
            fUpgSw = TRUE;             /* Upgrade-only */
            fDosSw = TRUE;             /* Dos-only */
            vInfo.Args.fNoBernoulli = TRUE;
            break;
#endif

         case  '?':
#ifdef OEM_PROGRAM
				vInfo.fOemCmdLineOK = TRUE;	/* Don't need to boot setup */
#endif
            CommandHelp();
            ProgramExit( 0 );
            break;

         default:
            InvalidSwitch( argv[ i ] );
      }

      /* Build copy of original Setup command-line tail. */

      strcat( szCmdLine, " ");               /* Preface with space */
      strcat( szCmdLine, argv[i] );          /* Add switch to cmd-line copy */

      /* Build Win Setup command-line tail. */

      if ( fWinSw )
      {
         strcat( szWinCmdLine, " ");         /* Preface with space */
         strcat( szWinCmdLine, argv[i] );    /* Add switch to cmd-line copy */
      }

#if (UPGRADE_PROGRAM != 0) && (UJANUS != 0)
      /* Build Non-upgrade Setup command-line tail. */

      if ( !fUpgSw )
      {
         strcat( szNuCmdLine, " ");          /* Preface with space */
         strcat( szNuCmdLine, argv[i] );     /* Add switch to cmd-line copy */
      }
#endif
   }

#ifdef UPGRADE_PROGRAM
   /* If floppy-install specified, mask out /M flag */
   if (vInfo.Args.fFloppy)
      vInfo.Args.fMininum = FALSE;
#endif

}

/************************************************************************/
/* Displays an invalid switch message along with command line switch    */
/* that caused the error.                                               */
/*                                                                      */
/* void near InvalidSwitch( char *szArgStr )                            */
/*                                                                      */
/* ARGUMENTS:  szArgStr -  Ptr to the offending command line argument   */
/* RETURNS:    void     -  Exits the program with error level set       */
/*                                                                      */
/************************************************************************/

void near InvalidSwitch( char *szArgStr )
{
   char     *apszText[ SWITCH_ERROR_LINES ];

   GetMessage( apszText, SWITCH_ERROR_TEXT );
   VideoPuts( apszText[0] );
   VideoPuts( szArgStr );
   VideoPutChar( CR );
   VideoPutChar( 0x0a );
   ProgramExit( -1 );
}

/************************************************************************/
/* Displays an invalid switch format message along with command line    */
/* switch that caused the error.                                        */
/*                                                                      */
/* void near InvalidSwitchFmt( char *szArgStr )                         */
/*                                                                      */
/* ARGUMENTS:  szArgStr -  Ptr to the offending command line argument   */
/* RETURNS:    void     -  Exits the program with error level set       */
/*                                                                      */
/************************************************************************/

void near InvalidSwitchFmt( char *szArgStr )
{
   char     *apszText[ SWFMT_ERROR_LINES ];

   GetMessage( apszText, SWFMT_ERROR_TEXT );
   VideoPuts( apszText[0] );
   VideoPuts( szArgStr );
   VideoPutChar( CR );
   VideoPutChar( 0x0a );
   ProgramExit( -1 );
}


/************************************************************************/
/* Function which given a ptr to a command line argument will copy the  */
/* path appended to it and copy it to a caller supplied buffer. If the  */
/* drive letter was not specified the drive the program was executed    */
/* from will be used.                                                   */
/*                                                                      */
/* int near ExtractSwitchPath( char *szArg, char szBuf )                */
/*                                                                      */
/* ARGUMENTS:  szArg -  Ptr to a command line argument in the format    */
/*                      /S:C:\path1\path2\...\pathx                     */
/* RETURNS:    int   -  OK if a valid path is extracted else ERROR      */
/*                                                                      */
/************************************************************************/

int near ExtractSwitchPath( char *szArg, char *szBuf )
{
   char     *szPtr;

   szPtr = szArg + 2;

                                             /* See if path is missing  */
   if ( *(szPtr++) != ':' || *szPtr == EOL )
      return( ERROR );

      /* At this point szPtr will be pointing to start of path string   */
   if ( *(szPtr + 1) != ':' ) /* See if user didn't specified the drive */
   {
      strcpy( szBuf, vInfo.szSource );       /* Use the default drive   */
      if ( *szPtr == '\\' )
         szPtr++;                            /* Skip over path char     */
      strcpy( szBuf + 3, szPtr );
   }
   else
      strcpy( szBuf, szPtr );                /* User specified drive    */

   strupr( szBuf );

   if ( *(szBuf + 2) != '\\' )         /* Make sure not a relative path */
      return( ERROR );

   RemoveTrailing( szBuf + 3, '\\' );

   if ( !IsValidPath( szBuf + 2, (UINT)((int)*szBuf - 0x40), FALSE ) )
      return( ERROR );

   return( OK );
}

/************************************************************************/
/* Display the command line help and then exits to DOS.                 */
/*                                                                      */
/* void CommandHelp( void )                                             */
/*                                                                      */
/* ARGUMENTS:  NONE                                                     */
/* RETURNS:    void                                                     */
/*                                                                      */
/************************************************************************/

void near CommandHelp( void )
{
   register    i;

#if defined (UPGRADE_PROGRAM)

   char        *apszText[ HELP_SWITCH_LINES ];
   GetMessage( apszText, HELP_SWITCH_TEXT );

#elif defined (UJANUS)

   char        *apszText[ BUSETUP_HELP_SWITCH_LINES ];
   GetMessage( apszText, BUSETUP_HELP_SWITCH_TEXT );

#elif defined (OEMFULL)

   char        *apszText[ OEMFULL_HELP_SWITCH_LINES ];
   GetMessage( apszText, OEMFULL_HELP_SWITCH_TEXT );

#elif defined (OEMBASE)

   char        *apszText[ OEMBASE_HELP_SWITCH_LINES ];
   GetMessage( apszText, OEMBASE_HELP_SWITCH_TEXT );

#endif  /* UPGRADE_PROGRAM */


   for( i = 0; apszText[ i ] != NULL; i++ )
   {
      VideoPuts( apszText[i] );
      VideoPutChar( CR );
      VideoPutChar( 0x0a );
   }
}
