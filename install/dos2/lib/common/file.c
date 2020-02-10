/*
 *  file.c     Read a file into memory. For DosWin Setup. Also contains
 *             config.sys and autoexec.bat munging. Lots of stuff is far
 *             pascal because we need to call from windows code.
 *  MC
 *
 *  Modification History:
 *
 *  3/24/89  Mike Colee	 Wrote it
 *
 */

#include <dos.h>
#include <malloc.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <io.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "sulib.h"

#define PHILSMAXPATH         256

/* Globaly used pointers to non-translatable text strings. */

char   *pszDEVICE   =               "device";
char   *pszMOUSE    =               "mouse";
char   *pszMSSYS    =               "mouse.sys";
char   *pszHPSYS    =               "mousehp.sys";
char   *pszADD_Y    =               "add_y";
char   *pszSYSEXT   =               ".sys";
char   *pszCOMEXT   =               ".com";
char   *pszPATH     =               "PATH";
char   *pszTEMP     =               "TEMP";
char   *pszSET      =               "set";

/*
 *  Global flag indicating type of setup.
 */

BOOL bIsNetSetup = FALSE;

/* Global far pointers to buffers that will contain autoexec.bat and
   config.sys contents. These can be modified by the DOS or windows
   portion of setup in case the buffer needs to be expanded. */

LPSTR    lpConfigBuff;
LPSTR    lpAutoBuff;

BOOL  gbAutoMod;      /* becomes true if autoexec buffer is modified */
BOOL  gbSysMod;       /* becomes true if config buffer is modified */

PSTR    pConfigWin = CONFIG_WIN;
PSTR    pAutoWin   = AUTOEXEC_WIN;

/* Another stupid global needed bucause of another hack to the config
   munger. Some wierd machines will need a himem switch. This global
   will either contain the switch string or will be NULL if no switch
   is needed. */

extern  char  szHimemSwitch[];

BOOL   gbBufValid = FALSE;  /* Global flag used to detect first call to
                               fnProcessConfig from subsiquent call.    */

/* Functions resolved here that are only used here !! (Local Prototypes ) */

BOOL     fnEditPath(char*);
void     fnModifyDeviceEntry(int, int, PSTR, PSTR);
unsigned fnWriteFiles(LPSTR, char*, char*);
BOOL     fnPathline(LPSTR);
void     fnTerminate(char*);
LPSTR    fnCreateFileBuffer(unsigned);
char     *fnMyStrPath(char*, char*);
BOOL     fnCheckDevice(char*,char*,BOOL);
BOOL     fnFileExists(char*);
BOOL     fnMouseline(LPSTR, PSTR);
BOOL     fnTempLine(LPSTR);

/* unsigned FAR PASCAL fnProcessFile(&iEga,&iHimem,&iSmartdrvMin,&iSmartdrvMAX,&iLim,&iRamdrive,Opt_Flag)
 *
 *  iHimem, iSmartsrv, and iRamdrive are the values of their respective
 *  devices. iHimem will be (1) true if himem is present or is to be
 *  installed. iRamdrive and iSmartdrv are the sizes of the drivers to
 *  be installed or are the sizes of the currently installed devices.
 *
 *  Function will operate on config.sys file according to function chosen
 *  with Opt_Flag. Opt_Flag options are:
 *
 *       RETURN_PRAMS : Function returns size of smartdrive, ramdrive, and
 *                      weather himem.sys is installed. These values are
 *                      returned in pointer provided as func args. Return
 *                      value indicates success or failure.
 *
 *       SET_PRAMS :    Function sets parameters for smartdrive, ramdrive
 *                      and will add himem to config.sys. Parameters are
 *                      set according to values given in func args. A null
 *                      argument leaves that value un affected. Return value
 *                      indicates success or failure. Success indicates that
 *                      the newly modified buffer will need to be written.
 *
 *                      The SET_PRAMS option is also affected by the value
 *                      of the given device parameter. If the value is
 *                      zero the device entry will be un-affected. If a
 *                      positive value is given, that will become the size
 *                      of the installed devive. If a -1 value is given, the
 *                      device will be removed from config.sys
 *
 *       CHECK_COMPAT:  Function will check config.sys for any of the device
 *                      drivers listed in setup.inf as "incompatable". Any
 *                      drivers found will be removed. This option will also
 *                      make sure the files = spec is greater than or = to
 *                      30. Return value indicates true or false as to 
 *                      weather a config buffer will need to be written.
 *
 *       WRITE_SYS   :  Function will rename the current config.sys to
 *                      config.win then write out the newly modified buffer
 *                      as config.sys. Return indicates success or failure.
 *
 *       WRITE_BAK   :  Function will write out the newly modified buffer
 *                      as config.win. Return indicates sucess or failure.
 *
 *       WRITE_NONE  :  Throws out the modified buffer (read free).
 *
 *       MUNGE_AUTO  :  Will add szSetupPath to the path statement in the
 *                      users autoexec.bat file. also will set temp
 *                      environment variable if necessary.
 *
 *       ASSURE_OPEN :  This is a NOP call used only to assure buffers
 *                      for autoexec.bat and config.sys have been opened.
 *
 *  ASSURE_MOUSE_POSITION: This call should be made to assure EGA.SYS will
 *                         be installed before mouse.sys. sigh ...
 *
 */
unsigned FAR PASCAL fnProcessFile(piEGA,piHimem,piSmartdrvMIN,piSmartdrvMAX,piLim,piRamdrive,Opt_Flag)
int       *piEGA;
int       *piHimem;
int       *piSmartdrvMIN;
int       *piSmartdrvMAX;
int       *piLim;
int       *piRamdrive;
unsigned  Opt_Flag;
{
   char szConfigPath[MAXPATHLEN];    /* String buffer to hold fully qualified
                                        path to config.sys file.   */

   char szAutoexecPath[MAXPATHLEN];      /* String buffer to hold fully qualified
                                            path to autoexec.bat file. */
   unsigned    AutoExecWork = 0; /* bit field to determine work needed */
   unsigned    fRetVal = 0;
   unsigned    fRetVal2 = 0;
   int         fhDst;
   char        szTmpPath[15];
   char        szConfigLine[MAXCMDLINELEN];
   char        szLookPath[256];
   int         n = NO_ACTION;            /* do nothing to these entries */
   BOOL        bTmpRetVal = TRUE;


   if (!piHimem)       piHimem       = &n;  /* do nothing to these entries */
   if (!piSmartdrvMIN) piSmartdrvMIN = &n;  /* do nothing to these entries */
   if (!piSmartdrvMAX) piSmartdrvMAX = &n;  /* do nothing to these entries */
   if (!piLim)         piLim         = &n;  /* do nothing to these entries */
   if (!piRamdrive)    piRamdrive    = &n;  /* do nothing to these entries */
   if (!piEGA)         piEGA         = &n;  /* do nothing to these entries */

   if (!gbBufValid)          // If first call or buffer previously written
   {

      #ifndef	DOSONLY
         extern BOOL	   bIsUpgrade;
         extern char    *szTmpConfigPath, *szTmpAutoPath;

         if ( !bIsUpgrade )
         {
            fnGetFilePath(szConfigPath,CONFIG_SYS);
            fnGetFilePath(szAutoexecPath,AUTOEXEC_BAT);
         }
         else
         {

            strcpy( szConfigPath, szTmpConfigPath );
            strcpy( szAutoexecPath, szTmpAutoPath );
         }
      #else
         fnGetFilePath(szConfigPath,CONFIG_SYS);
         fnGetFilePath(szAutoexecPath,AUTOEXEC_BAT);

      #endif

      if (! (lpConfigBuff = fnLoadFile(szConfigPath)) ) {
         lpConfigBuff = fnCreateFileBuffer(CONFIG); // Open failure so create
         if ( lpConfigBuff )
            gbSysMod = TRUE;
         else
            return FALSE;
      }
      else
         gbSysMod = FALSE;               // Config.sys buffer not modified.
      
      if (! (lpAutoBuff = fnLoadFile(szAutoexecPath)) ) {
         lpAutoBuff = fnCreateFileBuffer(AUTOEXEC); // Open failure so create
         if ( lpAutoBuff )
            gbAutoMod = TRUE;
         else
            return FALSE;
      }
      else
         gbAutoMod = FALSE;              // Autoexec.bat buffer not modified.
         
      gbBufValid = TRUE;                 // Ok, files loaded !
   }

   #ifndef DOSONLY

   /* Ok, we have to check for any incompatable devices in config.sys and
      also make sure number of file handles is >= 30. */

   if ( Opt_Flag & CHECK_COMPAT ) {
      int            i = 0;
      PINF           pinfSectP;
      BOOL           bNR;           // No Replace flag for files = line.

      if (bNR = fnUpdateDevice(FILES,RETURN_PRESENCE,szConfigLine)) {
         while ( !ISDIGIT(szConfigLine[i]) && !ISEOF(szConfigLine[i]) )
            ++i;
         if ( atoi(szConfigLine + i) < 30 )    // File less than 30 ?
            bNR = FALSE;
      }
      if ( !bNR ) {
         fnUpdateDevice(FILES,REMOVE_DEVICE,szConfigLine);
         strcpy(szTmpPath,FILES);          // Yes, build files = 30 line.
         strcat(szTmpPath,NUM_FILES);
         if (bTmpRetVal &= fnUpdateDevice(szTmpPath,ADD_DEVICE,szConfigLine))
            gbSysMod = TRUE;
      }
      pinfSectP = infFindSection(NULL,COMPATIBILITY);
      while ( pinfSectP != NULL ) {
         char buf[40];
	      fartonear(buf, pinfSectP);
         if (fnUpdateDevice(buf,RETURN_PRESENCE,szConfigLine)) {
            if (bTmpRetVal &= fnUpdateDevice(buf,REMOUT_DEVICE,szConfigLine))
               gbSysMod = TRUE;
         }
         pinfSectP = infNextLine(pinfSectP);
      }
      if ( bTmpRetVal )
         fRetVal |= CHECK_COMPAT_SUCCESS;
   }

   #endif // DOSONLY

   if ( Opt_Flag & RETURN_PRAMS ) {
      if (fnUpdateDevice(HIMEM,RETURN_PRESENCE,szConfigLine))
         *piHimem = TRUE;
      else
         *piHimem = FALSE;

      if (fnUpdateDevice(EGASYS,RETURN_PRESENCE,szConfigLine))
         *piEGA = TRUE;
      else
         *piEGA = FALSE;

      if (fnCheckDevice(CACHE_SECT,szConfigLine,NO_REMOVE)) {
         *piSmartdrvMAX = fnGetInstalledSize(szConfigLine,1);
         *piSmartdrvMIN = fnGetInstalledSize(szConfigLine,2);
      }
      else {
         *piSmartdrvMAX = NO_ACTION; // no Smartdrive installed.
         *piSmartdrvMIN = NO_ACTION; // no Smartdrive installed.
      }

      if (fnCheckDevice(VDISK_SECT,szConfigLine,NO_REMOVE))
         *piRamdrive = fnGetInstalledSize(szConfigLine,1);
      else
         *piRamdrive = NO_ACTION; // no Ramdrive installed.

      if (fnCheckDevice(LIM_SECT,szConfigLine,NO_REMOVE)) 
         *piLim = fnGetInstalledSize(szConfigLine,1);
      else
         *piLim = NO_ACTION; // no Limulator installed.

      fRetVal |= RET_PRAMS_SUCCESS;
   }

   if ( Opt_Flag & SET_PRAMS ) {
      fnModifyDeviceEntry(*piSmartdrvMAX,*piSmartdrvMIN,SMARTDRV,CACHE_SECT);
      fnModifyDeviceEntry(*piRamdrive,0,RAMDRIVE,VDISK_SECT);
      fnModifyDeviceEntry(*piLim,0,LIMDRIVER,LIM_SECT);
      fnModifyDeviceEntry(*piEGA,0,EGASYS,NULL);
      fnModifyDeviceEntry(*piHimem,0,HIMEM,NULL);
      fRetVal |= SET_PRAMS_SUCCESS;
   }

   /* Make sure at all costs that ega.sys is installed before mouse.sys */

   if ( Opt_Flag & ASSURE_MOUSE_POSITION ) {
      if ( fnUpdateDevice(MOUSE_SYS,RETURN_PRESENCE,szConfigLine) ) {
         fnUpdateDevice(MOUSE_SYS,REMOVE_DEVICE,NULL);
         fnUpdateDevice(szConfigLine,ADD_DEVICE,NULL);
         gbSysMod = TRUE;
      }
   }

   /* What we want to do here is rename original config.sys to config.bak
      and write out modified buffer as config.sys */

   if ( Opt_Flag & WRITE_SYS )
	{
	#ifndef	DOSONLY
	{
		extern	int	bIsUpgrade;

		if ( bIsUpgrade )		/* If Upgrade need to write 		*/
		{							/* to root as a unique name		*/
			extern char		*szAutoBat;
			extern char		*szConfSys;
			extern char		szWinAuto[];
			extern char		szWinConf[];
			extern int far	CreateUniqueName( char *szBuf, char *szFile );
			static int		SaveFile( PSTR szFile, LPSTR lpBuf );

			CreateUniqueName( szWinAuto, szAutoBat );
			fRetVal |= SaveFile( szWinAuto, lpAutoBuff );

			CreateUniqueName( szWinConf, szConfSys );
			fRetVal2 |= SaveFile( szWinConf, lpConfigBuff );
		}
		else
		{
	      if ( gbSysMod )
	         fRetVal2 |= fnWriteFiles(lpConfigBuff,CONFIG_BAK,CONFIG_SYS);
	      if ( gbAutoMod )
	         fRetVal |= fnWriteFiles(lpAutoBuff,AUTOEXEC_BAK,AUTOEXEC_BAT);
		}
	}
	#else
	{
      if ( gbSysMod )
         fRetVal2 |= fnWriteFiles(lpConfigBuff,CONFIG_BAK,CONFIG_SYS);
      if ( gbAutoMod )
         fRetVal |= fnWriteFiles(lpAutoBuff,AUTOEXEC_BAK,AUTOEXEC_BAT);
	}
	#endif

      if (( fRetVal & WRITE_SUCCESS) && (fRetVal2 & WRITE_SUCCESS) )
         Opt_Flag |= WRITE_NONE;
      else 
		{
         if (!(fRetVal2 & WRITE_SUCCESS))
            fRetVal = fRetVal2;
      }
   }

   /* What we do here is write out the modified buffer as config.win */

   if ( Opt_Flag & WRITE_BAK ) {
      if ( gbSysMod )
         fRetVal2 |= fnWriteFiles(lpConfigBuff,NULL,pConfigWin);
      if ( gbAutoMod )
         fRetVal |= fnWriteFiles(lpAutoBuff,NULL,pAutoWin);
      if (( fRetVal & WRITE_SUCCESS) && (fRetVal2 & WRITE_SUCCESS) )
         Opt_Flag |= WRITE_NONE;
      else {
         if (!(fRetVal2 & WRITE_SUCCESS))
            fRetVal = fRetVal2;
      }
   }

   /* All we need to do here is throw out the buffer. */

   if ( Opt_Flag & WRITE_NONE ) {
      FFREE(lpConfigBuff);                        // Free the config buffer
      FFREE(lpAutoBuff);                          // Free the autoexec buffer
      gbSysMod = gbAutoMod = gbBufValid = FALSE;  // Invalidate buffer
      fRetVal |= WRITE_SUCCESS;
   }

   /* The autoexec munging code is only used in the windows part of setup. */

   #ifndef DOSONLY

   /* Ok, let's add the installation path to autoexec.bat */

   if ( Opt_Flag & MUNGE_AUTO ) {

      if (! getenv(pszTEMP) )        // If no temp environment var, argg !...
         AutoExecWork |= DO_TEMP;

      if (! fnMyStrPath(getenv(pszPATH),szSetupPath) )
         AutoExecWork |= DO_PATH;

      if ( bIsNetSetup ) {
         if (! fnMyStrPath(getenv(pszPATH),szDiskPath) )
            AutoExecWork |= DO_PATH;
      }
      
      if ( fnModifyPath(AutoExecWork,NULL))    // Now munge away !!
         gbAutoMod = TRUE;
   }

   #endif // DOSONLY

   if (gbSysMod)
      fRetVal |= CONFIG_DIRTY;
   
   if (gbAutoMod)
      fRetVal |= AUTOEXEC_DIRTY;

   return fRetVal;
}

/* BOOL fnCheckDevice(char *Device_Sect,char *szConfigLine,BOOL bRemoveFlag);
 *
 * Function will check users config.sys file for all [type] device
 * drivers listed in setup.inf under the [type] section. If one of
 * these drivers is found the line from config.sys will be returned in
 * the buffer provided. The device line in question may also be romoved
 * from the config.sys file.
 * 
 * ENTRY: Device_Sect  - This argument specifies the device type to be
 *                       searched for. One of: CACHE, LIM, RAMDRIVE.
 *
 *        szConfigLine - Buffer of sufficent size to hold the line from
 *                       the config.sys file.
 * 
 *        bRemoveFlag  - Booleen flag set to TRUE if device in question
 *                       should be removed from config.sys file.
 * 
 * EXIT: Boolean as to success or failure of function. TRUE if device found.
 *       FALSE if device not found.
 *
 */
BOOL fnCheckDevice(Device_Sect,szConfigLine,bRemoveFlag)
char       *Device_Sect;
char       *szConfigLine;
BOOL       bRemoveFlag;
{
   char   szNearBuf[MAX_INF_LINE_LEN];
   PINF   pinfSectP;
   BOOL   RetVal = FALSE;

   pinfSectP = infFindSection(NULL,Device_Sect);

   while ( pinfSectP ) {
      fartonear(szNearBuf,pinfSectP);
      if ( fnUpdateDevice(szNearBuf,RETURN_PRESENCE,szConfigLine) ) {
         if ( bRemoveFlag )
            fnUpdateDevice(szNearBuf,REMOUT_DEVICE,szConfigLine);
         RetVal = TRUE;
         break;
      }
      pinfSectP = infNextLine(pinfSectP);
   }

   return RetVal;
}

/* void fnGetFilePath(char* sDestBuff, char* sFileName);
 *
 * Function returns fully qualified path for either autoexec.bat or
 * config.sys. Pretty simple assumption here. Get list of fixed diskes,
 * assume first in list is boot drive ???
 *
 * ENTRY: sDestBuff  String pointer to buffer large enough to contain fully
 *                   qualified path to file.
 *
 *        sFileName  Pointer to file name to be path qualified.
 *
 * EXIT:  None
 *
 */
void FAR PASCAL fnGetFilePath(sDestBuff,sFileName)
char    *sDestBuff;
char    *sFileName;
{
   unsigned        Disks[26];

   /* If the installation destination is a remote drive, create new
      autoexec.bat and config.sys files in the destination directory. */

   if ( DosIsRemote((UPCASE(szSetupPath[0]) - 'A')) == REMOTE )
      strcpy(sDestBuff,szSetupPath); 
   else {                            
      GetFixedDisks(Disks);
      sDestBuff[0] = (char)(Disks[0] + 'A');
      sDestBuff[1] = ':';
      sDestBuff[2] = '\\';
      sDestBuff[3] = '\0';
   }
   if ( sFileName )
      catpath(sDestBuff,sFileName);
}

/* unsigned fnWriteFiles(szBuff,NewNameofOrig,BuffName);
 *
 * Function will write the contents of the buffer pointed to by BuffName.
 *
 * ENTRY: 
 *
 *
 */
unsigned fnWriteFiles(szBuff,NewNameofOrig,BuffName)
LPSTR    szBuff;
char     *NewNameofOrig;
char     *BuffName;
{
   char     szTmpBuffNew[MAXPATHLEN];
   char     szTmpBuffName[MAXPATHLEN];
   int      fhDst;

   fnGetFilePath(szTmpBuffNew,BuffName); // Qualify filename for buffer dest.
   if ( NewNameofOrig ) {
      fnGetFilePath(szTmpBuffName,NewNameofOrig); // Qualify filename for buff
      DosDelete(szTmpBuffName);
      DosRename(szTmpBuffNew,szTmpBuffName);      // Ren config.sys config.bak
   }
   _dos_setfileattr(szTmpBuffNew,_A_NORMAL);      // Make sure not read only.
   if (_dos_creat(szTmpBuffNew,_A_NORMAL,&fhDst) != 0) // Create file
      return CREATE_FAIL;
   if (fnWriteFile(szBuff,fhDst))           // Write new config.sys
      return WRITE_FAIL;
   return WRITE_SUCCESS;
}

/* unsigned fnGetInstalledSize(PSTR szConfigLine, int FieldNum);
 *
 * Function will return installed size of either ramdrive or smartdrive
 * given an entire device = line from config.sys.
 *
 * NOTES   : It seems that one of the few things you can depend on in a
 *           config.sys device = entry is that there will be a fully 
 *           qualified device driver name. This means that there will be
 *           a period (.) followed by three chars. (Thank God for this).
 *
 * ENTRY   : Near pointer to line from config.sys ( ramdrive or smartdrv ).
 *         : FieldNum = numeric field following device driver name to be
 *           returned.
 *         
 * RETURNS : Size of installed device as an unsigned value. A return value
 *           of zero indicates default size in use.
 *
 */
unsigned fnGetInstalledSize(szConfigLine,FieldNum)
PSTR     szConfigLine;
int      FieldNum;
{
   unsigned     i = 0;
   unsigned     uFieldCnt = 1;

   while ( szConfigLine[i] != '.' && szConfigLine[i] )
      ++i;

   ++i;    /* point to first char after file type period. */

   /* Now look for a numeric value in the field given as the second function
      argument. */

   while (!ISDIGIT(szConfigLine[i]) && szConfigLine[i] ) // find a digit.
      ++i;

   while ( uFieldCnt != FieldNum ) {
      while (ISDIGIT(szConfigLine[i]) && szConfigLine[i])  // Find next Field
         ++i;
      while (!ISDIGIT(szConfigLine[i]) && szConfigLine[i]) // find a digit.
         ++i;
      ++uFieldCnt;
   }

   return (atoi(szConfigLine + i));   // Convert to int and return value !

}

/* void fnConstructDeviceEntry(PSTR, PSTR, int, int);
 *
 * Function will create a device = entry line that is ready to be placed
 * into the config.sys file. The device will be ramdrive, smartdrv, or
 * himem depending on the device name given. The size of the device is
 * given in iActionValue, and the newly constructed line will be returned
 * in szConfigLine.
 *
 * ENTRY   : szConfigLine, buffer large enough to hold the config line. This
 *           buffer will contain the config line if it is presently in the
 *           config.sys file. (ie, were changing an entry).
 *
 *         : szDeviceName, the name of the device for which were
 *           constructing the new config line for.
 *
 *         : iActionValue, The size of the the ramdrive or smartdrive. If
 *           zero, default size will be used ( no size entry ). Ignored
 *           if himem is the device being operated on.
 *
 * RETURNS : None.
 *
 */

void fnConstructDeviceEntry(szConfigLine,szDeviceName,iActionValue,iMin)
PSTR     szConfigLine;
PSTR     szDeviceName;
int      iActionValue;
int      iMin;
{
   char         szPath[MAXPATHLEN];
   char         szInt_to_String[8];
   char         *p;
   int          i;

#ifndef  DOSONLY
   extern BOOL  bIsUpgrade;
   void far     wsBuildConfigLine( char *szPath, char *szDeviceName );

   if ( bIsUpgrade )
      wsBuildConfigLine(szConfigLine, szDeviceName );
   else
   {
      if ( !strcmpi( szDeviceName, HIMEM ) )
         fnGetFilePath( szPath, szDeviceName );
      else
      {
         ExpandFileName("0:",szPath);      // Get path to device driver.
         catpath( szPath, szDeviceName );  // Cat with device driver name.
      }
      strcpy(szConfigLine,pszDEVICE);     // Copy in device =
      i = strlen(szConfigLine);
      szConfigLine[i++] = EQUAL;
      szConfigLine[i] = NULL;
      strcat( szConfigLine,szPath );     // Copy in the fully qualified driver
     }
#else
      if ( !strcmpi( szDeviceName, HIMEM ) )
         fnGetFilePath( szPath, szDeviceName );
      else
      {
         ExpandFileName("0:",szPath);      // Get path to device driver.
         catpath( szPath, szDeviceName );  // Cat with device driver name.
      }
      strcpy(szConfigLine,pszDEVICE);     // Copy in device =
      i = strlen(szConfigLine);
      szConfigLine[i++] = EQUAL;
      szConfigLine[i] = NULL;
      strcat( szConfigLine,szPath );     // Copy in the fully qualified driver
#endif

   if (iActionValue && (strcmpi(szDeviceName,HIMEM)) &&
      (strcmpi(szDeviceName,EGASYS))) {
      i = strlen(szConfigLine);
      szConfigLine[i++] = SPACE;
      szConfigLine[i] = NULL;
      p = itoa(iActionValue,szInt_to_String,10);
      strcat(szConfigLine,szInt_to_String);
      if ( iMin ) {
         i = strlen(szConfigLine);
         szConfigLine[i++] = SPACE;
         szConfigLine[i] = NULL;
         p = itoa(iMin,szInt_to_String,10);
         strcat(szConfigLine,szInt_to_String);
      }
   }
   if (! (strcmpi(szDeviceName,RAMDRIVE)) )
      strcat(szConfigLine,EMM_SWITCH);

   if (! (strcmpi(szDeviceName,HIMEM)) && szHimemSwitch[0] ) {
      strcat(szConfigLine," /M:");
      strcat(szConfigLine,szHimemSwitch);
   }
}

/* LPSTR fnCreateFileBuffer(unsigned fType);
 *
 * This function is called in the cases where the user does not have either
 * an autoexec.bat or config.sys file. In these cases we create a buffer on
 * the far heap and build the appropiate file and return a long pointer to
 * the buffer.
 *
 * ENTRY: fType - This is a flag value specifing which file buffer to create
 *                must be either AUTOEXEC or CONFIG.
 *
 * EXIT:  LPSTR - Returns long pointer to newly created buffer.
 *
 */
LPSTR fnCreateFileBuffer(fType)
unsigned    fType;
{

   LPSTR          lpBuf;  /* far pointer to buffer. */
   LPSTR            pTo;  /* Used to keep pointer into new buffer. */
   unsigned         len;  /* used to calculate length of buff for autoexec */
   char   TmpWork1[128];  /* temp work buffers for string construction. */
   char   TmpWork2[128];

   if ( fType == CONFIG ) {
      /* First, create files = line */
      strcpy(TmpWork1,FILES" "NUM_FILES);
      /* Now, create buffers = line */
      strcpy(TmpWork2,BUFFERS" "NUM_FILES);
   }
   else {
      TmpWork1[0] = '\0';
      strcpy(TmpWork2,pszPATH);
      strcat(TmpWork2," ");
      strcat(TmpWork2,szSetupPath);
   }

   /* Now calculate length of buffer required for new file and place newly
      created strings into new buffer. Finaly return pointer to buffer     */

   len = (strlen(TmpWork1) + strlen(TmpWork2) + 8 );
   lpBuf = FALLOC(len);                              /* Allocate buffer */
   if (!lpBuf) 
      return NULL;  /* if allocation failes, return null pointer. */
   /* Now, write newly constructed strings into buffer ! */
   pTo = lpBuf;
   pTo = fnCopyBuf(TmpWork1,pTo,REMAINDER);
   *pTo = CR, ++pTo, *pTo = LF, ++pTo;       /* attach a cr,lf pair. */
   pTo = fnCopyBuf(TmpWork2,pTo,REMAINDER);
   *pTo = CR, ++pTo, *pTo = LF, ++pTo;       /* attach a cr,lf pair. */
   *pTo = '\0';                              /* terminate new buffer.    */
   return lpBuf;                             /* return pointer to buffer */
}

/* void fnModifyDeviceEntry(int, int, PSTR, PSTR);
 *
 *  Function will operate on a single line from config.sys This function
 *  is only called by fnProcessConfig. This function is given an option
 *  value, a pointer to the config.sys buffer, and the device name to be
 *  operated on. Depending on action value the following operations are
 *  possible.
 *
 *   NO_ACTION      : No action taken.
 *
 *   REMOVE_DEVICE  : Device = line will be removed from config.sys buffer.
 *
 *   default        : Device = line will be added to config.sys buffer. If
 *                    the action value is zero, the default size for that
 *                    device will be used. Otherwise the size given will
 *                    be used.
 *
 *   RETURN: None
 *	
 */ 
void fnModifyDeviceEntry(iActionValue, iMin, szDeviceName, szDeviceSect)
int          iActionValue;
int          iMin;
PSTR         szDeviceName;
PSTR         szDeviceSect;
{

   static char        szConfigLine[MAXPATHLEN];

   szConfigLine[0] = '\0';

   switch (iActionValue) {
      case NO_ACTION:
         break;
      case REMOVE_DEVICE:
         if ( szDeviceSect )
            fnCheckDevice(szDeviceSect,szConfigLine,YES_REMOVE);
         fnUpdateDevice(szDeviceName,REMOVE_DEVICE,NULL);
         gbSysMod = TRUE;
         break;
      default:   // Install device with given numerical size or default.
         if ( szDeviceSect )
            fnCheckDevice(szDeviceSect,szConfigLine,YES_REMOVE);
         fnUpdateDevice(szDeviceName,REMOVE_DEVICE,NULL);
         fnConstructDeviceEntry(szConfigLine,szDeviceName,iActionValue,iMin);
         if ( strcmpi(HIMEM,szDeviceName) )
            fnUpdateDevice(szConfigLine,ADD_DEVICE,szConfigLine);
         else
            fnUpdateDevice(szConfigLine,ADD_DEVICE_FIRST,szConfigLine);
         gbSysMod = TRUE;
   }
}

/* LPSTR fnLoadFile(szFile);
 *
 *  Reads the entire file into a far malloc'd buffer and returns a pointer
 *  to the buffer. End of lines are indicated by cr,lf pairs. EOF, or in
 *  this case, the end of the buffer is indacated by a single NULL byte.
 *
 *  ENTRY: szFile	- File name to load, null terminated string required.
 *	
 *
 *  RETURNS: Far pointer to loaded buffer or NULL on Failure.
 *
 */
LPSTR NEAR PASCAL fnLoadFile(szFile)
PSTR  szFile;
{
   LPSTR        lpBuf = NULL;
   WORD         fh = -1;
   char         szDst[MAXPATHLEN];
   unsigned     len;

   /* First off, Open the file */
   /* first try to open passed parameter as is */

   fh = FOPEN(szFile);

   if (fh == -1)
      goto error_close;

   /* Find size of file and allocate buffer in far heap */

   len = (WORD)FSEEK(fh,0L,SEEK_END); /* find File size in Bytes */
   FSEEK(fh,0L,SEEK_SET);

   if ( len >= 65000 )     /* limit on size of files we can load. */
      goto error_close;

   lpBuf = FALLOC(len+1);  /* Allocate buffer plus one 1 for 0 terminator */
   if (!lpBuf)
      goto error_close;

   /*	Read ALL of the file into memory, Including comments, ect. */
   
   if ( FREAD(fh,lpBuf,len) != len ) {
      FFREE(lpBuf);
      lpBuf = NULL;
   }
   else
      lpBuf[len] = 0;

error_close:

   FCLOSE(fh);
   return lpBuf;

}

/* BOOL NEAR PASCAL fnUpdateDevice(szDeviceName,Opt_Flag,szConfigLine);
 *
 *  Function will add, delete or determine presence of a device = driver
 *  or files = in the users config.sys file.
 *
 *  ENTRY: szDeviceName: Name of device you intend to operate on. ie 
 *                       SMARTDRV.SYS ect. In the case of ADD_DEVICE
 *                       must point to a complete Config.sys device = entry
 *                       or files = entry.
 *
 *                       Example:  device = c:\dev\vt52.sys /c /l
 *                       Example:  files  = 30
 *
 *                       In the case of REMOVE_DEVICE of RETURN_PRESENCE
 *                       should point to either a device driver name or
 *                       If operating on the files = line, must point to
 *                       string "files"
 *
 *         Opt_Flag    : One of four mutually exclusive options.
 *
 *         1.) ADD_DEVICE:      Adds a new device = line or files = line.
 *         3.) ADD_DEVICE_FIRST Adds new device = line as the FIRST device.
 *         3.) REMOVE_DEVICE:   Removes entire device = line or files = line.
 *         4.) RETURN_PRESENCE: Return BOOL as to presence of device = line
 *                              or files = line.
 *         5.) REMOUT_DEVICE:   Comment out the device line in question.
 *
 *        szConfigLine : In the case of RETURN_PRESENCE This must be a near
 *                       pointer to a buffer of sufficient size to hold the
 *                       requested line from config.sys (MAXCMDLINELEN).
 *                       
 *
 *  NOTES: When adding a device = line to config.sys, it will be placed into
 *         the files as the second device = line. When adding a files = line
 *         it will be made the first line in the file.
 *
 *  RETURNS: On add or remove return value indicates success or failure.
 *           On return_presence, return value indicates presence
 *           or lack of presence of config.sys line.
 *	
 *               
 */
BOOL NEAR PASCAL fnUpdateDevice(szDeviceName,Opt_Flag,szConfigLine)
PSTR      szDeviceName;
unsigned  Opt_Flag;
PSTR      szConfigLine;
{

   BOOL           bFiles = FALSE;
   LPSTR          lpFileOffset = lpConfigBuff;
   LPSTR          lpFileBuf = lpConfigBuff;
   LPSTR          pTo;
   LPSTR          lpTmpHold;
   unsigned       iCharCnt;
   char           szDevice[7];

   /* Initial seek to beginning of first line in config.sys buffer. */

   while ( ISWHITE(*lpFileOffset) ) 
      ++lpFileOffset;
   if ( ISEOF(*lpFileOffset) )
      return FALSE;

   if ( (Opt_Flag == ADD_DEVICE) || (Opt_Flag == ADD_DEVICE_FIRST) ) {

      /* Allocate new buff large enough for old + new device + CR,LF,NULL
         + another possible CR,LF,NULL */

      pTo = FALLOC(fnLstrlen(lpFileBuf) + strlen(szDeviceName) + 6);
      if (!pTo)
         return FALSE;

      lpConfigBuff = pTo;               // Save pointer to head of new buff.
      if (strnicmp(szDeviceName,FILES,strlen(FILES))) {
         // While not to device section yet, copy lines to new buffer.
         while (strncmpinf(pszDEVICE,lpFileOffset,6) && !ISEOF(*lpFileOffset)) {
            pTo = fnCopyBuf(lpFileOffset,pTo,fnLinelenl(lpFileOffset));
            lpFileOffset = fnNextConfigLine(lpFileOffset);
         }
         if ( Opt_Flag != ADD_DEVICE_FIRST ) {
            while ( !ISEOF(*lpFileOffset) ) {
               pTo = fnCopyBuf(lpFileOffset,pTo,fnLinelenl(lpFileOffset));
               lpFileOffset = fnNextConfigLine(lpFileOffset);
            }
         }
      }
      // Put new device line in.
      if ( ISEOF(*lpFileOffset) && !ISCRLF(*(lpFileOffset-1)) ) {
         *pTo = CR,++pTo;
         *pTo = LF,++pTo;
      }
      pTo = fnCopyBuf((LPSTR)szDeviceName,pTo,strlen(szDeviceName)); 
      *pTo = CR,++pTo;
      *pTo = LF,++pTo;
      fnCopyBuf(lpFileOffset,pTo,REMAINDER); // attach remainder of buff.
      FFREE(lpFileBuf);                      // Free the old buffer.
      return TRUE;                           // Return success !
   }
   else { 
      // We need to either remove or verify presence of a device.
      pTo = lpFileOffset;
      if (!strcmpi(szDeviceName,FILES)) {
         strcpy(szDevice,FILES);
         bFiles = TRUE;
      }
      else 
         strcpy(szDevice,pszDEVICE);

      /* Now, while we have not hit the end of the buffer ... */
      while (!(ISEOF(*lpFileOffset))) {
         pTo = lpFileOffset;
         if ( !strncmpinf(szDevice,lpFileOffset,strlen(szDevice)) ) {
            while ( *lpFileOffset != EQUAL && !ISEOL(*(lpFileOffset+1)) )
               ++lpFileOffset;
            ++lpFileOffset;
            while ( ISFILL(*lpFileOffset) && *lpFileOffset )
               ++lpFileOffset;
            while ( !ISWHITE(*lpFileOffset) && *lpFileOffset )
               ++lpFileOffset;
            --lpFileOffset;
            iCharCnt = 0;
            while ( !DEVICESEP(*lpFileOffset) && (lpFileOffset > pTo) )
               --lpFileOffset, ++iCharCnt;
            ++lpFileOffset;
            if ( bFiles || !strncmpinf(szDeviceName,lpFileOffset,iCharCnt) &&
               iCharCnt == strlen(szDeviceName) ) {
               switch(Opt_Flag) {        // !!! we found the device !!!.
                  case RETURN_PRESENCE:
                     fnCopyBuf(pTo,(LPSTR)szConfigLine,(fnLinelenl(pTo)));
                     fnTerminate(szConfigLine);
                     return TRUE;
                  case REMOVE_DEVICE:
                     lpFileOffset = fnNextConfigLine(lpFileOffset);
                     fnCopyBuf(lpFileOffset,pTo,REMAINDER);
                     return TRUE;
                  case REMOUT_DEVICE:
                     *pTo = CR,++pTo;
                     *pTo = LF,++pTo;
                     lpFileOffset = fnNextConfigLine(lpFileOffset);
                     fnCopyBuf(lpFileOffset,pTo,REMAINDER);
                     return TRUE;
               }
            }
         }
         lpFileOffset = fnNextConfigLine(lpFileOffset);
      }
      return FALSE;
   }
}

/* INT NEAR PASCAL strncmpinf(PSTR pch1, LPSTR pch2, int n);
 *
 * Function compares N chars of strings without regaurd to case. I needed
 * this so that I could compare a string in near mem with a string in far
 * mem.
 *
 * ENTRY: Near string pointer, far string pointer, number of chars to comp.
 *
 * RETURNS: 0 if compare == ok. non zero if strings do not compare.
 *
 */
int NEAR PASCAL strncmpinf(pch1, pch2, n)
PSTR      pch1;
LPSTR     pch2;
int       n;
{
   while (*pch1 && --n > 0 && toupper(*pch1) == toupper(*pch2))
      *pch1++,*pch2++;
   return toupper(*pch1) != toupper(*pch2);
}

/* LPSTR NEAR PASCAL fnCopyBuff(LPSTR pFrom, LPSTR pTo, unsigned iCnt);
 *
 * Function moves the remaining contents of a text buffer from one location
 * within the buffer to a new location within the buffer. This is used to
 * either remove or make room for an entry in the file buffer.
 *
 * ENTRY: pointers To and From designate where the remaining protion of the
 *        buffer will be moved to. The new buffer will be NULL terminated.
 *
 * EXIT:  Returns pointer to next available char position in the buffer.
 *
 */
LPSTR fnCopyBuf(pFrom,pTo,iCnt)
LPSTR     pFrom;
LPSTR     pTo;
unsigned  iCnt;
{
   // While not End of buffer or end of count.

   while ( *pFrom != 0 && iCnt != 0 ) {
      *pTo = *pFrom;                          // Do the move.
      ++pTo;
      ++pFrom;          // Increment buffer poointers.
      --iCnt;           // Decrement count.
   }
   if ( *pFrom == '\0' )
      *pTo = *pFrom;      // Terminate newly expanded or contracted buffer.
   return pTo;
}

/* unsigned fnLstrlen(LPSTR)
 *
 * Returns length of string not including null terminating
 * char (far pointer version).
 *
 * ENTRY:    LPSTR to buffer
 * EXIT:     length of string.
 * WARNING:
 * EFFECTS:  No global data effected.
 *
 */
unsigned fnLstrlen(lpBuf)
LPSTR  lpBuf;
{

   unsigned i = 0;

   while ( *lpBuf++ != '\0' )
      ++i;
   return i;

}

/* int fnLinelenl(LPSTR)
 *
 * Returns length of buffer line up to and including any LF and CR chars.
 *
 * (far pointer version).
 *
 * ENTRY:    LPSTR to buffer
 * EXIT:     length of line.
 * WARNING:
 * EFFECTS:  No global data effected.
 *
 */
int fnLinelenl(lpBuf)
LPSTR  lpBuf;
{

   unsigned i = 0;

   while (!ISEOL(*lpBuf))
      ++i,++lpBuf;

   while ( ISCRLF(*lpBuf) )
      ++i,++lpBuf;

   return i;

}

/* LPSTR fnNextConfigLine(LPSTR lpFileOffset);
 *
 * Advances Far pointer into config.sys files to the first non-white char
 * of the next line in the buffer containing the file. Will return null
 * in the EOF case.
 *
 * ENTRY: Far pointer into buffer holding file.
 *
 * EXIT:  Far pointer into buffer holding file. NULL on EOF.
 *
 */
LPSTR fnNextConfigLine(lpFileOffset)
LPSTR    lpFileOffset;
{

   while ( !ISEOL(*lpFileOffset) )   //seek end of line.
      ++lpFileOffset;
 
   /* seek to beginning of next line, or end of buffer. */
 
   while ( ISWHITE(*lpFileOffset) )
      ++lpFileOffset;

   return lpFileOffset;
}

/* int fnWriteFile(LPSTR szBuffer, int fh);
 *
 *  Function will determine length of buffer, write buffer out to file
 *  handle provided. Close the files, return condition of opertion.
 *
 *
 *
 */
int NEAR PASCAL fnWriteFile(szBuffer,fh)
LPSTR    szBuffer;
int      fh;
{

   unsigned   len;
   int        condition;

   len = fnLstrlen(szBuffer);
   if ( FWRITE(fh,szBuffer,len) != len )   /* Write Error ! */
      condition = FERROR();
   else
      condition = ERROR_OK;

   FCLOSE(fh);
   return condition;
}

/* void fnTerminate(char* Buffer);
 *
 * Function seeks to the end of the buffer
 * appended and NULL teriminates the buffer.
 *
 * ENTRY: buffer - Pointer to buffer containing path.
 *
 * EXIT:  None
 *
 */
void fnTerminate(Buffer)
char      *Buffer;
{
   unsigned     Index;

   Index = fnLinelenl(Buffer);
   --Index;

   while(ISWHITE(Buffer[Index]))  //Back up to some meat.
      --Index; 

   Buffer[Index+1] = '\0';
}

/* BOOL fnMystrstr(char *szSrcStr, char *szSearchStr);
 *
 * Function will return BOOL value as to weather the Search string exists
 * any where within the source string. The difference between this func
 * the C run time func is that this one is simpler and is also not case
 * sensitive.
 *
 * ENTRY: szSrcStr    - Char buffer to be searched.
 *
 *        szSearchStr - String that will be searched for.
 *
 * EXIT:  BOOL value as to weather or not string was found.
 *
 *
 * WARNING: Source and search strings MUST be null terminated.
 *          
 *
 */
BOOL fnMystrstr(szSrcStr, szSearchStr)
char       *szSrcStr;
char       *szSearchStr;
{
   unsigned      len;             // Get length of search string.

   len = strlen(szSearchStr);

   while ( !ISEOL(*szSrcStr) ) {
      if ( ! strnicmp(szSrcStr,szSearchStr,len))
         return TRUE;
      ++szSrcStr;
   }
   return FALSE;

}

#ifndef DOSONLY

/* BOOL fnModifyPath(unsigned WorkNeeded, PSTR szMouseName);
 *
 * This function can add the windows installation path to the users path
 * statement in autoexec.bat This function will also add a temp environment
 * variable to the users autoexec.bat file if specified.
 *
 * ENTRY: WorkNeeded - bit field that will specify any combination of the
 *        following options:
 *
 *        DO_PATH  : This option tells the func to munge the users path.
 *
 *        DO_MOUSE : This option will search for a mouse driver installation
 *                   and replace it with a new installation line which will
 *                   install the new mouse driver from the windows
 *                   installation directory.
 *
 * EXIT : Function returns a boolean as to the success, or failure.
 *
 */
BOOL NEAR PASCAL fnModifyPath(WorkNeeded,szMouseName)
unsigned     WorkNeeded;
PSTR         szMouseName;
{
   LPSTR          lpFileOffset = lpAutoBuff;
   LPSTR          pHead = lpAutoBuff;
   LPSTR          pTo;
   char           TmpWorkSpace[PHILSMAXPATH]; // large enough for path construction.
   char           *szSearch;         // used to search through a string.
   unsigned       len;               // used to calc size of new buffer.
   BOOL           bNew      = FALSE;
   BOOL           bMouseFnd = TRUE;
   BOOL           bPathFnd  = TRUE;
   BOOL           bTempFnd  = TRUE;
   BOOL           bBufMod   = FALSE;
   
   if (! WorkNeeded )
      return bBufMod;

   if ( WorkNeeded & DO_PATH )
      bPathFnd = FALSE;

   if ( WorkNeeded & DO_MOUSE )
      bMouseFnd = FALSE;

   if ( WorkNeeded & DO_TEMP )
      bTempFnd = FALSE;

   /* Initial seek to beginning of first line in autoexec.bat buffer. */

   while ( ISWHITE(*lpFileOffset) && *lpFileOffset != '\0' ) 
      ++lpFileOffset;
   if (*lpFileOffset == NULL)
      return FALSE;

   /* Allocate new buff big enough for the following new items: */

   len = (fnLstrlen(lpFileOffset) + /* Length of current autoexec.bat    */
   (strlen(szSetupPath))          + /* path, 1 for path.                 */
   7 );                             /* 2 CR, 2 LF, 2 NULL chars, 1 ';'   */

   if ( bIsNetSetup )               // If were adding a net source dir to the
      len += strlen(szDiskPath);    // path allocate space for it too !

   if ( WorkNeeded & DO_MOUSE )     // If we have to add a mouse install ?
      len += MAXPATHLEN;

   if ( WorkNeeded & DO_TEMP )         // strlen of installation dir + length
      len += (strlen(szSetupPath)+16); // of "set temp=tempCR,LF,NULL"

   pTo = FALLOC(len);
   if (!pTo)
      return FALSE;
   lpAutoBuff = pTo;               // Save pointer to head of new buff.

   /* Now, start looking through the autoexec.bat file for relevent info */
   
   while ( !ISEOF(*lpFileOffset) )  {     // While not at end of autoexec.bat
      bTempFnd |= fnTempLine(lpFileOffset);        // check if line is temp ?
      if ( (WorkNeeded & DO_PATH) && !bPathFnd ) {
         if ( fnPathline(lpFileOffset) ) {  // check if path ?
            bPathFnd = TRUE;

            /* A somewhat dangerous but usually safe assumption: The path
               will be less than PHILSMAXPATH chars. */

            fnCopyBuf(lpFileOffset,(LPSTR)TmpWorkSpace,fnLinelenl(lpFileOffset));
            fnTerminate(TmpWorkSpace);           // Find end, NULL terminate.
            if ( fnEditPath(TmpWorkSpace) ) {    // Edit the path.
               pTo = fnCopyBuf(TmpWorkSpace,pTo,strlen(TmpWorkSpace));
               *pTo = CR,++pTo;
               *pTo = LF,++pTo;
               bNew = TRUE;
               bBufMod = TRUE;
            }
         }
      }
      if ( (WorkNeeded & DO_MOUSE) && !bMouseFnd ) {
         if ( fnMouseline(lpFileOffset,pszMOUSE) ) {  // Mouse Line ?
            bMouseFnd = TRUE;
            if (! stricmp(szMouseName,pszADD_Y) ) {
               fnCopyBuf(lpFileOffset,(LPSTR)TmpWorkSpace,fnLinelenl(lpFileOffset));
               fnTerminate(TmpWorkSpace);
               if (! fnMystrstr(TmpWorkSpace,"\Y") )
                  strcat(TmpWorkSpace," /Y");
            }
            else {
               strcpy(TmpWorkSpace,szSetupPath);
               catpath(TmpWorkSpace,szMouseName);
               strcat(TmpWorkSpace,pszCOMEXT);
               strcat(TmpWorkSpace," /Y");
            }
            pTo = fnCopyBuf(TmpWorkSpace,pTo,strlen(TmpWorkSpace));
            *pTo = CR,++pTo;
            *pTo = LF,++pTo;
            bNew = TRUE;
            bBufMod = TRUE;
         }
      }
      if (! bNew )
        pTo = fnCopyBuf(lpFileOffset,pTo,fnLinelenl(lpFileOffset));
      bNew = FALSE;
      lpFileOffset = fnNextConfigLine(lpFileOffset);
      if ( bPathFnd && bMouseFnd && bTempFnd) {      // If all work is done !
         fnCopyBuf(lpFileOffset,pTo,REMAINDER);
         break;                                     // Then were done !!!
      }
   }
   if (! bPathFnd ) {      // Now if a path was not found, add one.
      if ( ISEOF(*lpFileOffset) && !ISCRLF(*(lpFileOffset-1)) ) {
         *pTo = CR,++pTo;
         *pTo = LF,++pTo;
      }
      strcpy(TmpWorkSpace,pszPATH);
      strcat(TmpWorkSpace,"=");
      strcat(TmpWorkSpace,szSetupPath);
      pTo = fnCopyBuf((LPSTR)(TmpWorkSpace),pTo,strlen(TmpWorkSpace));
      *pTo = CR,++pTo;
      *pTo = LF,++pTo;
      bBufMod = TRUE;
      pTo = fnCopyBuf(lpFileOffset,pTo,REMAINDER);
   }
   if (! bTempFnd ) {           // And if a temp was not found add one.
      if ( ISEOF(*lpFileOffset) && !ISCRLF(*(lpFileOffset-1)) && bPathFnd ) {
         *pTo = CR,++pTo;
         *pTo = LF,++pTo;
      }
      strcpy(TmpWorkSpace,pszSET);     // construct set statement.
      strcat(TmpWorkSpace," ");
      strcat(TmpWorkSpace,pszTEMP);
      strcat(TmpWorkSpace,"=");
      strcat(TmpWorkSpace,szSetupPath);
      catpath(TmpWorkSpace,pszTEMP);
      pTo = fnCopyBuf((LPSTR)(TmpWorkSpace),pTo,strlen(TmpWorkSpace));
      *pTo = CR,++pTo;
      *pTo = LF,++pTo;
      *pTo = '\0';
      szSearch = TmpWorkSpace;
      while ( *szSearch != '=' )
         ++szSearch;
      ++szSearch;
      DosMkDir(szSearch);             // Make the directory.
      bBufMod = TRUE;
   }
   if ( ISEOF(*lpFileOffset) )
      *pTo = '\0';                   // Assure buffer terminated !!!
   FFREE(pHead);                     // Free the old buffer.
   return bBufMod;                   // Return state of buffer
}

/* BOOL fnEditPath(char *ExistingPath);
 *
 * Function will add the szSetupPath to the users path statement in the
 * autoexec.bat file. The szSetupPath will be the first location in the
 * path statement. The path can be in about a million fucked up forms
 * depending on the level of stupidity in the user. Here are a few to
 * think about. DOS will accept any of these.
 *
 *   PATH=C:\foo;C:\foobar;...
 *   SET PATH=C:\foo;C:\foobar;...
 *   PATH C:\foo;C:\foobar;...
 *   PATH =C:\foo;C:\foobar;...
 *   PATH= C:\foo;C:\foobar;...
 *   PATH = C:\foo;C:\foobar;...
 *   
 * ENTRY: ExistingPath - near buffer containing the path statement to be
 *                       edited.
 *
 * EXIT:  BOOL, TRUE  = Path succesfully modified.
 *              FALSE = Path was too long after modification (path >= 128).
 *
 */
BOOL fnEditPath(ExistingPath)
char       *ExistingPath;
{
   char      TmpWorkSpace[PHILSMAXPATH];
   char      NewPathBuff[PHILSMAXPATH];
   char      *Path;
   char      *InsertPoint;
   unsigned  i = 0;

   Path = ExistingPath;
   while ( *Path != 'h' && *Path != 'H' ) {  /* look for end of PATH */
      TmpWorkSpace[i] = *Path;
      ++Path, ++i;
   }
   TmpWorkSpace[i] = *Path++;             /* Cat on the 'h' or 'H'       */
   TmpWorkSpace[i+1] = *Path++;           /* Cat on a ' ' or an '='      */
   TmpWorkSpace[i+2] = '\0';              /* Terminate.                  */

   /* There may be extranious crapola here so lets rid ourselvs of it. */
   
   while ( *Path == ' ' || *Path == '=' || *Path == '\t' )
      ++Path;

   strcpy(NewPathBuff,Path);

   /* now we have a "path =" or "path " null terminated string. Now we need
      to assure that in all cases of path editing, the users local windows
      installation directory comes before a shared windows directory. This
      of course is only relevent if we are doing a net setup. */

   strcat(TmpWorkSpace,szSetupPath);     /* Now cat the new path seg */
   strcat(TmpWorkSpace,";");             /* Cat on the seperator     */

   if ( (InsertPoint = fnMyStrPath(NewPathBuff,szSetupPath)) ) {
      *InsertPoint++ = '\0';
      while ( !ISEOL(*InsertPoint) && *InsertPoint != ';' )
         ++InsertPoint;
      if ( *InsertPoint == ';' )
         ++InsertPoint;
      strcat(NewPathBuff,InsertPoint);
   }
   if ( bIsNetSetup ) {
      strcat(TmpWorkSpace,szDiskPath);     /* Now cat the new path seg */
      strcat(TmpWorkSpace,";");            /* Cat on the seperator     */
      if ( (InsertPoint = fnMyStrPath(NewPathBuff,szDiskPath)) ) {
         *InsertPoint++ = '\0';
         while ( !ISEOL(*InsertPoint) && *InsertPoint != ';' )
            ++InsertPoint;
         if ( *InsertPoint == ';' )
            ++InsertPoint;
         strcat(NewPathBuff,InsertPoint);
      }
   }
   strcat(TmpWorkSpace,NewPathBuff);       /* Now the rest of the path */

   /* Last but not least, check to make sure we don't insert a path that
      is too long into the dudes autoexec.bat, also check to make sure
      we don't edit the path when there was no reason to */
   
   if ( strlen(TmpWorkSpace) >= PHILSMAXPATH )
      return FALSE;
   else {
      if (! stricmp(TmpWorkSpace,ExistingPath) )
         return FALSE;
      strcpy(ExistingPath,TmpWorkSpace);   /* Put path back into orig buf */
      return TRUE;
   }
}

/* BOOL fnPathline(LPSTR);
 *
 * Function determines if the string pointed to by sStrPtr is a path statement
 * of any one of three possible forms:
 *
 *   PATH=C:\foo;C:\foobar;...
 *   SET PATH=C:\foo;C:\foobar;...
 *   PATH C:\foo;C:\foobar;...
 *
 * Function returns true or false as to weather or not the string is a path
 * or not.
 *
 */
BOOL fnPathline(sStrPtr)
LPSTR    sStrPtr;
{
   if ( !strncmpinf(pszPATH,sStrPtr,strlen(pszPATH)) ) // First, check the easy case
      return TRUE;
   else {
      if ( !strncmpinf(pszSET,sStrPtr,strlen(pszSET)) ) {
         sStrPtr += strlen(pszSET);            // Advance past set statement
         while ( ISFILL(*sStrPtr) )            // Seek for beginning of path
            ++sStrPtr;
         if ( !strncmpinf(pszPATH,sStrPtr,strlen(pszPATH)) )
            return TRUE;
      }
   }
   return FALSE;
}

/* char *fnMyStrPath(char *szPath, char *szSearchStr);
 *
 * Function will return BOOL value as to weather the Search string exists
 * any where within the source string. The difference between this func
 * the C run time func is that this one is simpler and is also not case
 * sensitive.
 *
 * ENTRY: szPath      - Char buffer to be searched.
 *
 *        szSearchStr - String that will be searched for.
 *
 * EXIT:  char* - Returns pointer to first char of string that matches.
 *
 *
 * WARNING: Source and search strings MUST be null terminated.
 *          
 *
 */
char *fnMyStrPath(szPath, szSearchStr)
char       *szPath;
char       *szSearchStr;
{
   unsigned      len;             // Get length of search string.

   len = strlen(szSearchStr);

   while (! ISEOL(*szPath) ) {
      if ( ! strnicmp(szPath,szSearchStr,len)) {
         if ( ISEOL(*(szPath + len)) || *(szPath + len) == ';' )
            return szPath;
      }
      ++szPath;
   }
   return NULL;
}

/* BOOL fnMouseline(LPSTR szLine, PSTR szMouseName);
 *
 * Function will make the determination as to weather or not a given line
 * (usually from an autoexec.bat file) is a mouse installation line or
 * not.
 *
 * ENTRY: szLine - The line from the autoexec.bat file for which the
 *                 determination will be made.
 *
 * EXIY: Boolean as to weather the line is a mouse installation line or
 *       not.
 * 
 */
BOOL fnMouseline(szLine,szMouseName)
LPSTR  szLine;
PSTR   szMouseName;
{
   char      TmpWorkBuf[MAXCMDLINELEN*2];
   unsigned  index;

   fnCopyBuf(szLine,TmpWorkBuf,fnLinelenl(szLine));

   fnTerminate(TmpWorkBuf);

   if (! fnMystrstr(TmpWorkBuf,szMouseName) )
      return FALSE;
   else {
      if ( strlen(TmpWorkBuf) < 128 && fnFileExists(TmpWorkBuf) )
         return TRUE;
      else
         return FALSE;
   }
}

/* BOOL fnTempLine(LPSTR);
 *
 * Function determines if the string pointed to by sStrPtr is a temp
 * environment variable definition of the following form.
 *
 *   SET TEMP=C:\foo
 *
 * Function returns true or false as to weather or not the string is a temp
 * environment variable or not.
 *
 */
BOOL fnTempline(sStrPtr)
LPSTR    sStrPtr;
{

   while ( ISWHITE(*sStrPtr) )    // May be whitespace before set statement.
      ++sStrPtr;

   if (! strncmpinf("set temp",sStrPtr,8)) {
      sStrPtr += 8;
      if ( *sStrPtr == '=' || *sStrPtr == ' ' || *sStrPtr == '\t' )
         return TRUE;
   }
   return FALSE;

}

/* BOOL fnFileExists(char *szFile);
 *
 * Function will determine if the given filename (weather qualified or not)
 * exists.
 *
 * ENTRY: szFile - File name, qualifed or not.
 *
 * EXIT: Boolean as to existance of file.
 *
 * WARNING: Function assumes you will be looking for a .com file.
 *
 */
BOOL fnFileExists(szFile)
char    *szFile;
{
   int             fh;
   char            szTmpBuf[PHILSMAXPATH];
   char            *tmp;
   unsigned        Disks[26];

   if ( szFile[strlen(szFile) - 4] != '.' ) {
      tmp = szFile;
      tmp += (strlen(szFile) - 1);
      while ( *tmp && UPCASE(*tmp) != 'E' )
         --tmp;
      *(tmp+1) = '\0';
      strcat(szFile,".com");
   }

   /* First see if the file exists by trying to open it. */

   fh = FOPEN(szFile);  // Try to open the file.

   if ( fh == -1 ) {               /* May need to prepend a drive spec. */
      if ( szFile[1] != ':' ) {
         GetFixedDisks(Disks);
         szTmpBuf[0] = (char)(Disks[0] + 'A');
         szTmpBuf[1] = ':';
         szTmpBuf[2] = '\0';
         strcat(szTmpBuf,szFile);
         fh = FOPEN(szTmpBuf);        // Try to open the file.
      }
   }

   if ( fh != -1 ) {
      FCLOSE(fh);
      return TRUE;
   }

   /* Next, see if the file exists anywhere on the path. */

   _searchenv(szFile,pszPATH,szTmpBuf);
   if ( *szTmpBuf )
      return TRUE;
   else
      return FALSE;

}

/* void FAR PASCAL fnTweekMouse(PSTR szMouseDrv,PSTR szMouseName);
 *
 * Function will look into the autoexec.bat and config.sys files and 
 * replace an existing mouse.com/mouse.sys file installation entry with
 * a new entry line which will install a new mouse driver from the windows
 * installation directory.
 *  
 *  ENTRY: szMouseDrv  - Buffer of sufficent size to hold the name of the
 *                       mouse driver + the logocal drive volume.
 *
 *         szMouseName - Name of mouse driver to be upgraded. One of:
 *                       hpmouse,mouse.
 *	
 *  RETURNS: No formal return, fills buffer provided with name of DOS
 *                             mouse driver.
 *
 */
void FAR PASCAL fnTweekMouse(szMouseDrv,szMouseName)
PSTR  szMouseDrv;
PSTR  szMouseName;
{
   char     szConfigLine[MAXCMDLINELEN*2];
   char     szMouseLnConst[MAX_INF_LINE_LEN];
   char     szProfileBuf[MAX_INF_LINE_LEN];
   char     *pMouseFnd;
   BOOL     IsAddY = FALSE;
   PINF     pinfSectP;

   if (! stricmp(szMouseName,pszADD_Y) )
      IsAddY = TRUE;

   strcpy(szMouseLnConst,szMouseName);
   strcpy(szProfileBuf,szMouseName);
   fnProcessFile(NULL,NULL,NULL,NULL,NULL,NULL,ASSURE_OPEN);

   if ( fnModifyPath(DO_MOUSE,szMouseName) ) {
      gbAutoMod = TRUE;
      if ( IsAddY )
         szProfileBuf[0] = '\0';
      else
         strcat(szProfileBuf,pszCOMEXT);
   }
   else {
      strcat(szMouseLnConst,pszSYSEXT);
      strcat(szProfileBuf,pszSYSEXT);

      /* First, find out what mouse driver if any is in config.sys. */

      if ( fnUpdateDevice(pszMSSYS,RETURN_PRESENCE,szConfigLine) )
         pMouseFnd = pszMSSYS;
      else if ( fnUpdateDevice(pszHPSYS,RETURN_PRESENCE,szConfigLine) )
         pMouseFnd = pszHPSYS;
      else
         pMouseFnd = NULL;

      /* Second, are we doing an add_y switch operation ? */

      if ( IsAddY && pMouseFnd && !fnMystrstr(szConfigLine,"/Y") ) {
         fnUpdateDevice(pMouseFnd,REMOUT_DEVICE,szConfigLine);
         strcat(szConfigLine," /Y");
         fnUpdateDevice(szConfigLine,ADD_DEVICE,szConfigLine);
         szProfileBuf[0] = '\0';
         gbSysMod = TRUE;
      }
      else if ( IsAddY && !pMouseFnd ) {
         strcpy(szMouseLnConst,pszMOUSE);
         strcat(szMouseLnConst,pszSYSEXT);
         strcpy(szProfileBuf,szMouseLnConst);
         strcat(szMouseLnConst," /Y");
         fnConstructDeviceEntry(szConfigLine,szMouseLnConst,0,0);
         fnUpdateDevice(szConfigLine,ADD_DEVICE,szConfigLine);
         gbSysMod = TRUE;
      }
      else if ( !IsAddY ) {
         if ( pMouseFnd )
            fnUpdateDevice(pMouseFnd,REMOUT_DEVICE,szConfigLine);
         strcat(szMouseLnConst," /Y");
         fnConstructDeviceEntry(szConfigLine,szMouseLnConst,0,0);
         fnUpdateDevice(szConfigLine,ADD_DEVICE,szConfigLine);
         gbSysMod = TRUE;
      }
      /* the case of (IsAddY && pMouseFnd && fnMystrstr(szConfigLine,"/Y")
         will work ok even though szProfileBuf contains add_y.sys. This
         will remain true as long as one never adds a profile string of
         add_y.sys to the [dos.mouse] section of setup.inf. */
   }
      /* Last thing to do, get name of DOS mouse driver to copy. */
   
   infGetProfileString(NULL,DOS_MOUSE_SECT,szProfileBuf,szMouseLnConst);
   infParseField(szMouseLnConst,1,szMouseDrv);
}

// write out a file.  
//
// in:
//	szFile	file to write.
//	lpBuf	null terminated string
// returns
// 	TRUE	success
//	FALSE	faile


static int SaveFile( PSTR szFile, LPSTR lpBuf )
{
	int			fh;
	unsigned		utowrite;
	unsigned    BufLen;
	char			buf[ MAXPATHLEN ];


	for ( BufLen = 0; lpBuf[ BufLen ] != '\0'; BufLen++ )
		;

	if (szFile[1] != ':')
	{
		ExpandFileName( szSetupPath, buf );
		catpath( buf, szFile );
	}
	else
		strcpy(buf, szFile);

	if ( _dos_creat( buf, 0, &fh) == 0 )
	{
		if ( _dos_write( fh, lpBuf, BufLen, &utowrite ) == 0 &&
			  utowrite == BufLen )
		{
			_dos_close( fh );
			return WRITE_SUCCESS;
		}
		close( fh );		
		return WRITE_FAIL;
	}
	else
		return CREATE_FAIL;
}

#endif  // DOSONLY
