/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * NETINFO.C - Source file for obtaining network information.
 ********************************************************************/


/* Include Files */

#include "msd.h"

#define INCL_DOSERRORS 1
#include "bseerr.h"
#undef INCL_DOSERRORS

float flFloatFix = (float) 0.0;

/*********************************************************************
 * GetNetworkInfo - Gets the network information.
 *
 * pNetInfo     - Pointer to network information structure.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetNetworkInfo (NETWORK_STRUCT *pNetInfo, BOOL fMinimumInfo)
{
  WORD wIndex;                  /* Index to network name string array */
  static BOOL fFirstTime = 1;   /* Set so that network information is */
                                /*   only obtained once.              */

  /* Obtain network information only if this is the first time through */

/*if (fFirstTime)
    {                */
      fFirstTime = 0;

      /* Initialize NetInfo structure fields */
      memset (pNetInfo, '\0', sizeof (NETWORK_STRUCT));
#if HEAP_DEBUG
      HeapCheck ("After memset");
#endif


      /* Check for NetBIOS running */
      if (Netbios_Installed (pNetInfo))
        {
          pNetInfo->fNetworkActive     = TRUE;
          pNetInfo->fNetBiosCompatible = TRUE;
        }
      else
        pNetInfo->fNetBiosCompatible = FALSE;
#if HEAP_DEBUG
      HeapCheck ("After Netbios_Installed");
#endif


      /* Begin checking for the different networks */

      /* Check for Novell Netware running */
      if (Novell_Installed (pNetInfo))
        {
          pNetInfo->fNetworkActive = TRUE;
          pNetInfo->wNetworkType   = NET_NOVELL;
          Get_Novell_Info (pNetInfo);
        }
#if HEAP_DEBUG
      HeapCheck ("After Novell_Installed");
#endif


      /* Check for Banyan VINES running */
      if (BanyanRunning())
        {
          pNetInfo->fNetworkActive = TRUE;
          pNetInfo->wNetworkType   = NET_BANYAN;
        }
#if HEAP_DEBUG
      HeapCheck ("After BanyanRunning");
#endif


      /* Check for Artisoft LANtastic running */
      if (LANtasticRunning (pNetInfo))
        {
          pNetInfo->fNetworkActive = TRUE;
          pNetInfo->wNetworkType   = NET_LANTASTIC;
        }
#if HEAP_DEBUG
      HeapCheck ("After LANtasticRunning");
#endif


      /* Check for LanMan running */
      if (LanManager_Installed (pNetInfo) == 0)
        {
#if HEAP_DEBUG
      HeapCheck ("After LanManager_Installed");
#endif
          pNetInfo->wNetworkType = LAN_Basic_Enhanced (pNetInfo);

          if (fMinimumInfo == FALSE)
            {
              if (pNetInfo->fNetworkActive == TRUE)
                Adapter_Status (pNetInfo);

#if HEAP_DEBUG
      HeapCheck ("After Adapter_Status");
#endif
              pNetInfo->fServerConnection = Server_Connection();
#if HEAP_DEBUG
      HeapCheck ("After Server_Connection");
#endif

              if (pNetInfo->wNetworkMajor >= 2)
                {
                  if ((pNetInfo->wNetworkMajor == 2) &&
                      (pNetInfo->wNetworkMinor == 0))
                    {
                      Get_CSD_Info (pNetInfo);
#if HEAP_DEBUG
      HeapCheck ("After GetCSD_Info");
#endif
                    }
                  else
                    {
                      if (pNetInfo->wNetworkType == NET_LANMAN_ENHANCED)
                        GetLM_VersionInfo (pNetInfo);
#if HEAP_DEBUG
      HeapCheck ("After GetLM_VersionInfo");
#endif
                    }
                }
            }
        }


      /* Get Windows for Workgroups/Workgroup Connection information */
      if (pNetInfo->wNetworkType != NET_LANTASTIC && IsWorkgrpSysInstalled())

        {
          pNetInfo->wNetworkType  = NET_WORKGROUP_CLIENT;
          pNetInfo->wNetworkMajor = 0;
          pNetInfo->wNetworkMinor = 0;
        }


      /* Get MSNET information */
      if ((wDosMajor > 3) ||       /* Check to make sure we are running */
         ((wDosMajor == 3) &&      /*  at least DOS 3.1 for net support */
          (wDosMinor > 0)))        /*  before we do the MSNET testing   */
        {
          /* Check for MSNET running */
          if (Msnet_Installed())
            {
#if HEAP_DEBUG
      HeapCheck ("After Msnet_Installed");
#endif
              /* A network is running */
              pNetInfo->fNetworkActive   = TRUE;

              /* It is MSNET compatible */
              pNetInfo->fMsnetCompatible = TRUE;

              /* Get the machine name */
              Get_Msnet_Machine_Name (pNetInfo);
#if HEAP_DEBUG
      HeapCheck ("After Get_Msnet_Machine_Name");
#endif
            }
          else
            pNetInfo->fMsnetCompatible = FALSE;/* It is not MSNET compatible */
        }

      /* Check for the PC-NFS network */
      if (pNetInfo->fNetworkActive   == TRUE &&
          pNetInfo->fMsnetCompatible == TRUE &&
          pNetInfo->wNetworkType     == 0    &&
          PcNfsInstalled()           == TRUE)
        {
          pNetInfo->wNetworkType = NET_PC_NFS;
        }


/*  } */

  /* If the network was not determined yet, say it is unknown */
  if (pNetInfo->wNetworkType == 0 && pNetInfo->fNetworkActive == TRUE)
    if (pNetInfo->fMsnetCompatible == TRUE)
      pNetInfo->wNetworkType = NET_MS_NET_COMPATIBLE;
    else
      pNetInfo->wNetworkType = NET_UNKNOWN_NET;

  /* Set the network's name */
  wIndex = pNetInfo->wNetworkType;
  strcpy (pNetInfo->szNetworkType, paszNetworkTypes[wIndex]);
#if HEAP_DEBUG
      HeapCheck ("After strcpy network type");
#endif


  return (FALSE);
}


/*******************************************************************
*
* Function Netbios_Installed
*
* This function checks for the existence of a Netbios driver.
* It returns a 1 if NetBIOS is detected, 0 if not.
*
*
* Local Variables Used
* --------------------
*
* inregs, outregs : Used to read and write the general purpose
*                   registers.
* segments        : Used to read and write the segment registers.
* PresenceNcb     : The NCB used to issue the test NetBIOS request.
*
*******************************************************************/

int Netbios_Installed (NETWORK_STRUCT * pNetInfo)

{
  union REGS inregs, outregs;
  struct SREGS segments;
  struct Ncb PresenceNcb;

  /* Check the INT 5C vector to see if it's set to anything */

  inregs.x.ax = 0x355C;
  int86x (0x21, &inregs, &outregs, &segments);

  pNetInfo->wNetBIOSSegment = segments.es;
  pNetInfo->wNetBIOSOffset  = outregs.x.bx;

  /* If the INT 5C vector is set to either 0x0000 or 0xF000, */
  /*   the vector is not set, return a failure status        */

  if ((segments.es == 0x0000) || (segments.es == 0xF000))
    return (0);          /* No Netbios */

  /* INT 5C vector is set to something. Issue an invalid       */
  /*   NetBIOS request and check the return value to determine */
  /*   if NetBIOS is really installed                          */

  ClearNcb (&PresenceNcb);

  PresenceNcb.ncb_command = NETBIOS_INVALID_COMMAND;  /* Defined in netbios2.h    */

  NetbiosRequest(&PresenceNcb);

  if (PresenceNcb.ncb_retcode == 0x03)    /* The value returned by NetBIOS for an invalid command */
    return (1);      /* NetBIOS is really there */
  else
    return (0);      /* Something else is using INT 5C */
}


/*******************************************************************
*
* Function ClearNcb
*
* This function clears out a Ncb structure setting all fields to
* 0x00.
*
*
* Local Variables Used
* --------------------
*
* index   : Used for loop control.
* CharPtr : Used to point to the NCB.
*
*******************************************************************/

VOID ClearNcb (struct Ncb * pNcbPtr)

{
  /* memset (pNcbPtr, '\0', sizeof (ZeroNcb)); */
  memset (pNcbPtr, '\0', sizeof (struct Ncb));
}


/*******************************************************************
*
* Function NetbiosRequest
*
* This function issues a NetBIOS request.
*
*
* Local Variables Used
* --------------------
*
* inregs, outregs : Used to read and write the general purpose
*                   registers.
* segments        : Used to read and write the segment registers.
* NcbPtr          : A far pointer to a structure of type NCB used
*                   to issue the NetBIOS request.
*
*******************************************************************/

VOID NetbiosRequest (struct Ncb * NcbPointer)

{
  union REGS inregs, outregs;
  struct SREGS segments;
  struct Ncb FAR *NcbPtr = (struct Ncb FAR *) NcbPointer;

  segread (&segments);

  /* Load ES:BX with the address of the NCB */

  segments.es = FP_SEG (NcbPtr);
  inregs.x.bx = FP_OFF (NcbPtr);

  /* Issue the NetBIOS request */

  int86x (NetbiosInt5C, &inregs, &outregs, &segments);
}


/*******************************************************************
*
* Function Novell_Installed
*
* This function checks for the existence of a Novell Netware
* network. It returns 1 if Netware is detected, 0 if not
*
*
* Local Variables Used
* --------------------
*
* inregs, outregs : Used to read and write the general purpose
*                   registers.
*
*******************************************************************/

BOOL Novell_Installed (NETWORK_STRUCT *pNetInfo)

{
  union REGS inregs, outregs;

  inregs.x.ax = 0x7A00;
  inregs.x.bx = 0x0000;
  int86(0x2F, &inregs, &outregs);

  if (outregs.h.al == 0xFF)
    {
      /* Novell is installed */

      pNetInfo->fIpxInstalled = TRUE;
      return (TRUE);
    }
  else
    {
      /* Novell is not installed */

      pNetInfo->fIpxInstalled = FALSE;
      return (FALSE);
    }
}


/*******************************************************************
*
* Function Get_Novell_Info
*
* This function gets the DOS environment type and machine type as
* Novell sees it.
*
*
* Local Variables Used
* --------------------
*
* inregs, outregs : Used to read and write the general purpose
*                   registers.
* segments        : Used to read and write the segment registers.
* SourceIndex     : Used to parse the string returned from Novell
*                   for the environment information.
*
*******************************************************************/

VOID Get_Novell_Info (NETWORK_STRUCT *pNetInfo)

{
  union REGS inregs, outregs;   /* Registers for int86x             */
  struct SREGS segs;            /* Segments for int86x              */
  CHAR chBuffer[80];            /* Local string buffer              */
  PSZ  pszString = chBuffer;    /* String pointer to chBuffer       */


  /* Get the string information */

  inregs.h.ah = 0xEA;
  inregs.h.al = 0x01;
  segread (&segs);
  segs.es = segs.ds;
  inregs.x.di = (unsigned int) FP_OFF ((CHAR FAR *) pszString);

  int86x(0x21, &inregs, &outregs, &segs);

  /* Set the operating system string */
  strcpy (pNetInfo->szNovellShellOs, pszString);

  /* Set the shell OS version string */
  pszString += strlen (pszString) + 1;
  strcpy (pNetInfo->szNovellShellOsVersion, pszString);

  /* Set the hardware type */
  pszString += strlen (pszString) + 1;
  strcpy (pNetInfo->szNovellHdwType, pszString);


  /* Get the numeric information */

  inregs.h.ah = 0xEA;
  inregs.h.al = 0x00;
  segread (&segs);
  segs.es = (unsigned int) FP_SEG (pszString);
  inregs.x.di = (unsigned int) FP_OFF (pszString);

  int86x(0x21, &inregs, &outregs, &segs);

  /* Set the operating system type */
  pNetInfo->wNovellShellOs = (WORD) outregs.h.ah;

  /* Set the hardware type */
  pNetInfo->wNovellHdwType = (WORD) outregs.h.al;

  /* Set the shell version number */
  pNetInfo->wShellMajor    = (WORD) outregs.h.bh;
  pNetInfo->wShellMinor    = (WORD) outregs.h.bl;
  pNetInfo->wShellRevision = (WORD) outregs.h.cl;

  /* Set the shell type (XMS/EMS/Conv) */
  if (pNetInfo->wShellMajor * 100 + pNetInfo->wShellMinor >= 301)
    pNetInfo->wShellType   = (WORD) outregs.h.ch;


  /* Get the station number */
  inregs.h.ah = 0xDC;
  int86 (0x21, &inregs, &outregs);
  pNetInfo->wStationNmbr = (WORD) outregs.h.al;


  /* Get the physical station number */
  inregs.h.ah = 0xEE;
  int86 (0x21, &inregs, &outregs);
  pNetInfo->wPhysicalStaNmbr1 = (WORD) outregs.x.cx;
  pNetInfo->wPhysicalStaNmbr2 = (WORD) outregs.x.bx;
  pNetInfo->wPhysicalStaNmbr3 = (WORD) outregs.x.ax;


  /* Is SPX installed */
  inregs.x.bx = 0x0010;
  inregs.h.al = 0x00;
  int86 (0x7A, &inregs, &outregs);
  pNetInfo->fSpxInstalled = (outregs.h.al == 0xFF) ? TRUE : FALSE;


  /* Is ODI/LSL.COM installed */
  inregs.x.ax = 0xC000;
  inregs.x.bx = 0x0000;
  int86 (0x2F, &inregs, &outregs);
  pNetInfo->fOdiLslInstalled = (outregs.h.al == 0xFF) ? TRUE : FALSE;
}


/*******************************************************************
*
* Function BanyanRunning()
*
* This function detects Banyan VINES if it is running. It returns a
* 1 if VINES was detected, 0 if not.
*
* Detection scheme
* ----------------
*
* To detect if VINES is currently running:
*
* MOV  AX,D701h
* MOV  BX,0
* INT  2F
*
* Returns: AX = 0 if workstation VINES software is enabled
*          BX = interrupt number being used (60 - 66)
*
*
* To get the VINES version number, use the interrupt number returned
* in BX above <banint> and do the following:
*
* MOV AX, 0700h
* LDS DX, <ptr-to-ULONG>
* INT <banint>
*
* Returns: AX = 0
*
* The version number is returned as:
*
* (VINES rev * 10000) + edit level - 50.
*
* If the edit level is < 50, it is a beta level. For example:
*
* 31505 = VINES 3.10 (0)
* 31051 = VINES 3.10 (1)
* 40010 = VINES 4.00 (Beta-10)
* 40050 = VINES 4.00 (0)
*
*
* NOTE: The version detection is currently commented out because it
*       hangs after returning the version number correctly. More on
*       this later.
*
*******************************************************************/

int BanyanRunning (VOID)

{
  unsigned int uiVinesIsThere;
/*  int iVinesINTNumber;        */
/*  unsigned long *pulVersion;  */

  _asm

  {
    mov   ax,0D701h
    mov   bx,0h
    int   2Fh
    mov   uiVinesIsThere,ax
  }


/*  This is commented out because it does not currently work
  if (uiVinesIsThere == 0)
    {
      _asm

        {
          mov   ax,0D701h
          mov   bx,0h
          int   2Fh
          mov   uiVinesIsThere,ax
          mov   iVinesINTNumber,bx
        }


      if (!uiVinesIsThere)

      _asm

        {
          mov   ax,0700h
          lds   dx,pulVersion
          int   <banint>
        }
    }
*/

  return (!uiVinesIsThere);
}


/***********************************************************************
*
* Artisoft LANtastic detection
*
* Run-time testing for LANtastic NOS (all versions). Returns 1 if
* LANtastic is present, 0 if not.
*
* Detection scheme
* ----------------
*
* First, determine whether or not a NOS is running (may not be LANtastic)
* by issuing a multiplex interrupt (2Fh). This will tell if redirector,
* server, or LANPUP software is installed:
*
* INPUT   AX  B800h
*
* OUTPUT  AL  0      If neither redirector or server is running
*             NZ     Redirector, server, or LANPUP is running
*
*         BL  Contains bits indicating which software is running
*             (Multiple bits will be set when several are running):
*
*             10000000b  Redirector has popup receive message capability
*             01000000b  Server software is running
*             00001000b  Redirector software is running
*             00000010b  LANPUP software is running
*
*             At a minimum, the redirector bit will be set if LANtastic
*             is running.
*
* After it has been determined if a NOS is running, determine if it is
* LANtastic by issuing one of LANtastic's extended DOS calls:
*
*         mov  ax, 5F9Ah      ; Get message processing flags in DL
*         int  21h
*         jc   not_LANtastic
*         jmp  is_LANtastic
*
* If LANtastic is running, determine what the version number is by
* issuing the following multiplex interrupt:
*
* INPUT   AX  B809h
*
* OUTPUT  AH  Major version number
*         AL  Minor version number
*
*         The version numbers are returned as decimal numbers. For example,
*         version 2.53 would be returned as:
*
*         AL     2
*         AH     53 decimal or 35 hex
*
****************************************************************************/

int LANtasticRunning (NETWORK_STRUCT *pNetInfo)

{
  int iRedirMask  = 0x08;     /* 00001000b */
  int iServerMask = 0x40;     /* 01000000b */
  int iPopUpMask  = 0x02;     /* 00000010b */

  union REGS inregs, outregs;
  int iFalse = 0, iTrue = 1;


  /* Issue multiplex interrupt 2fH to determine if some type
     of a NOS is running */

  inregs.x.ax = 0xB800;
  int86 (0x2F, &inregs, &outregs);


  /* If AL is non-zero (something is running) see if BL has bit 4
     turned on (00001000b) to see if it is possibly LANtastic. If
     it is possibly LANtastic, issue an extended DOS call to see
     if the NOS is LANtastic. */

  if ((outregs.h.al) && (outregs.h.bl & iRedirMask))
    {
      /* redirector software is running. Check for server and LANPUP
         software */

      pNetInfo->fLANtasticRedir = TRUE;

      if (outregs.h.bl & iServerMask)
        pNetInfo->fLANtasticServer = TRUE;

      if (outregs.h.bl & iPopUpMask)
        pNetInfo->fLANtasticPopUp = TRUE;


      /* Issue an extended DOS call to determine if LANtastic is running. */

      inregs.x.ax = 0x5F9A;
      intdos (&inregs, &outregs);


      /* If the carry flag is set this is not LANtastic, otherwise it is */

      if (outregs.x.cflag)
        pNetInfo->fLANtasticPresent = FALSE;
      else
        {
          /* LANtastic is running, get the version number */

          pNetInfo->fLANtasticPresent = TRUE;

          inregs.x.ax = 0xB809;
          int86 (0x2F, &inregs, &outregs);

          pNetInfo->wLANtasticVersionMajor = outregs.h.ah;
          pNetInfo->wLANtasticVersionMinor = outregs.h.al;
        }
    }

  return (pNetInfo->fLANtasticPresent);
}


/*******************************************************************
*
* Function Msnet_Installed
*
* This function checks for the existence of a Microsoft Network
* compatible network. It returns 1 if MSNET is detected, 0 if not.
*
*
* Local Variables Used
* --------------------
*
* inregs, outregs : Used to read and write the general purpose
*                   registers.
*
*******************************************************************/

BOOL Msnet_Installed (VOID)

{
  union REGS inregs, outregs;

  inregs.h.ah = 0x00;
  int86 (0x2A, &inregs, &outregs);

  if (outregs.h.ah != 0)
    return (TRUE);  /* MSNet is installed */
  else
    return (FALSE); /* MSNet is not installed */
}


/*******************************************************************
*
* Function Get_Msnet_Machine_Name
*
* This function gets the name of the local computer running MSNet.
*
*
* Local Variables Used
* --------------------
*
* inregs, outregs : Used to read and write the general purpose
*                   registers.
* segments        : Used to read and write the segment registers.
*
* Returns: TRUE if an error occured
*
*******************************************************************/

BOOL Get_Msnet_Machine_Name (NETWORK_STRUCT *pNetInfo)

{
  union REGS inregs, outregs;
  struct SREGS segments;
  CHAR FAR *pszMachineName = NULL;


  /* After this interrupt call, the MSNet machine name will be in
     a null terminated string in MsnetMachine */

  /* Set a pointer to the place in the structure */

  pszMachineName = (CHAR FAR *) pNetInfo->szMsnetMachineName;

  inregs.x.ax = 0x5E00;
  segments.ds = (unsigned int) FP_SEG (pszMachineName);
  inregs.x.dx = (unsigned int) FP_OFF (pszMachineName);
  int86x (0x21, &inregs, &outregs, &segments);

  if (outregs.x.cflag)  /* an error was returned */
    {
      pNetInfo->szMsnetMachineName[0] = -1;
      pNetInfo->szMsnetMachineName[0] = '\0';
      return (TRUE);
    }

  return (FALSE);
}


/*******************************************************************
*
* This Function calls NetWkstaGetInfo. If the call gives back an error
* other than 0, NERR_WkstaNotStarted, NERR_NotLoggedOn, or NERR_buftosmall
* then either Lan Manager is not running or Lan Manager is in a state we don't
* want to mess with. For example, the lanman.ini file isn't there.
*
* LOCAL vars
*     err      - holds return error of netwkstagetinfo
*     ta       - contains total available bytes from netwkstagetinfo
*     wkstabuf - buffer info for netwkstagetinfo
*     wksta0   - stuct mapping to wkstabuf which hold info from netwkstagetinfo
**********************************************************************/

int LanManager_Installed (NETWORK_STRUCT * pNetInfo)
{
  unsigned short int err=0, ta=0;
  char wkstabuf[BUFSIZ];
  struct wksta_info_0 *wksta0;

  /* Get wksta info - if error we still could be a basic workstation */
  err = NetWkstaGetInfo (NULL, 0, wkstabuf, BUFSIZ, &ta);

  /* if we get an error, just fill in stuff incase we try to print this */
  if (err != 0)
    {
#if 0
      strcpy (pNetInfo->szLanRoot, pszUnknown);
      strcpy (pNetInfo->szUserName, "Not Logged on");
      strcpy (pNetInfo->szPrimaryDomain, pszNone);
      pNetInfo->wNetworkMajor = 0x00;
      pNetInfo->wNetworkType = NET_UNKNOWN_NET;

      /* Net work may not be running, try autoexec.bat - this is */
      /*   for basic test, too.                                  */

      /* GetLanAutoexec will fill in version & lanroot if it can */
      err = GetLanAutoexec (pNetInfo);
#endif
      return err;
    }
  else
    {
      /* there was no error so fill in netinfo struct */

      pNetInfo->fNetworkActive = TRUE;

      wksta0 = (struct wksta_info_0 *) wkstabuf;
      _fmemcpy (pNetInfo->szLanRoot, wksta0->wki0_root, _MAX_PATH);

      if (wksta0->wki0_username[0] != '\0')
          _fmemcpy (pNetInfo->szUserName, wksta0->wki0_username, 16);

      if (wksta0->wki0_langroup[0] != '\0')
          _fmemcpy (pNetInfo->szPrimaryDomain, wksta0->wki0_langroup, 16);

      pNetInfo->wNetworkMajor = wksta0->wki0_ver_major;
      pNetInfo->wNetworkMinor = wksta0->wki0_ver_minor;

      pNetInfo->wNetworkType = NET_LANMAN;

      pNetInfo->fAPI_Support = TRUE;

      if (wksta0->wki0_mailslots != 0)
          pNetInfo->fMailslot_Support = TRUE;

      return 0;
    }
}


/*******************************************************************
*
* GetLanAutoexec function gets c:\autoexec.bat and searches for lanman info
* Possible info is LAN version & LAN ROOT. This function is only called
* if NetWkstaGetInfo fails which it will always do if LAN is basic.
*
**********************************************************************/

int GetLanAutoexec (NETWORK_STRUCT * pNetInfo)
{
  FILE * fileAuto;              /* File handle for AUTOEXEC.BAT  */
  FILE_INFO FAR * ffi;          /* Stores File Info structs      */
  FILE_INFO FAR * ffi2;         /* 2nd copy of File Info structs */
  CHAR chBuffer[_MAX_PATH];     /* Filled with ReadLine          */
  CHAR szFilePath[_MAX_PATH];   /* Stores path to AUTOEXEC.BAT   */
  CHAR * pBuffer;               /* Needed with strstr            */
  CHAR * pCase;                 /* Needed with strupr on buffer  */
  CHAR * pRem;                  /* Needed with strupr on buffer  */
  INT  i;                       /* Looping variable              */
  INT  iVersion;                /* Version Number                */


  /* Find the AUTOEXEC.BAT file */
  ffi = FindFile ("AUTOEXEC.BAT", NULL, SEARCH_BOOT_DRIVE, '\0');
  if (ffi == NULL)
    return (1);

  ffi2 = ffi;


  /* Look through all the AUTOEXEC.BAT files */
  while (ffi != NULL && ffi->fpNextFileInfo != NULL)
    {
      _fstrcpy (szFilePath, ffi->fpszPathToFile);


      /* OPEN file Autoexec.bat */
      fileAuto = OpenFile (szFilePath, "rb", FALSE);

      if (fileAuto == NULL)
          continue;

      /* Get lines of Autoexec file and Find LANMAN. */
      /*   If found get version & lanroot            */

      while (ReadLine (chBuffer, 80, fileAuto, FALSE) != EOF)
        {
          pCase = strupr (chBuffer);
          pBuffer = strstr (pCase, "REM ====== LANMAN ");
          if (pBuffer != NULL)
            {
              /* Make pBuffer point to major version number */
              pBuffer += 18;

              /* Get major version of LANMAN */
              iVersion = atoi (pBuffer);
              pNetInfo->wNetworkMajor = iVersion;

              /* Point to minor version of LANMAN */
              while (*pBuffer != '.' && *pBuffer != '\0')
                ++pBuffer;

              if (*pBuffer != '\0')
                ++pBuffer;

              iVersion = atoi (pBuffer);
              pNetInfo->wNetworkMinor = iVersion;

              /* get Lan Root */
              while (ReadLine (chBuffer, 80, fileAuto, FALSE) != EOF)
                {
                  pCase = strupr (chBuffer);
                  pBuffer = strstr (pCase, "REM ====== LANMAN ");
                  if (pBuffer != NULL)
                    {
                      CloseFile (fileAuto);
                      FreeFileInfo (ffi2);
                      return 1;
                    }

                  pBuffer = strstr (pCase, "SET PATH=");
                  pRem = strstr (pCase, "REM ");
                  if ((pBuffer != NULL) && (pRem == NULL))
                    {
                      pNetInfo->wNetworkType = NET_LANMAN_BASIC;
                      pBuffer += 9;
                      for (i=0; ((i < _MAX_PATH - 2) &&
                                 (*pBuffer != ';')     &&
                                 (*pBuffer != '\0')); i++)
                        {
                          pNetInfo->szLanRoot[i] = *pBuffer;
                          pBuffer += 1;
                        }

                      i--;
                      while (pNetInfo->szLanRoot[i] != '\\' && i)
                        i--;

                      pNetInfo->szLanRoot[i] = '\0';
                      CloseFile (fileAuto);
                      FreeFileInfo (ffi2);
                      return (0);
                    }
                }
            }
        }

      fclose (fileAuto);

      /* Point to the next File info structure */
      ffi = (FILE_INFO FAR *) (ffi->fpNextFileInfo);
    }

  FreeFileInfo (ffi2);
  return 1;
}


/*******************************************************************
*
* Procedure Adapter_Status
*
*     This procedure calls a net_adapter_status ncb call and a
*     netbios_session_status ncb call to get information on the
*     user's net card. The information gather is placed into the
*     NetInfo structure.
* Local Vars:
*     NCB,
*     AdapterStatus, SessionStatus are structure filled after the
*     ncb call.  NetCardNumber maintains which card is being
*     queried.  i is for the for loop on the number of netcards.
*******************************************************************/

void Adapter_Status (NETWORK_STRUCT * pNetInfo)
{
  struct Ncb     NCB;
  struct astat   AdapterStatus;
  struct sstat   SessionStatus;
  unsigned char  NetCardNumber=0;
  int            i, iSameNetID;

  ClearNcb (&NCB);
  /* loop until we run out of cards */
  for (NetCardNumber = 0;
       NCB.ncb_retcode == 0 && NetCardNumber < NUMBER_OF_CARDS;
       ++NetCardNumber)
    {
      /* setup the NCB for an adapter status call*/
      NCB.ncb_command      = NETBIOS_ADAPTER_STATUS;
      NCB.ncb_buffer       = (char far *) &AdapterStatus;
      NCB.ncb_length       = sizeof (struct astat);
      NCB.ncb_callname[0]  = '*';
      NCB.ncb_post         = 0;
      NCB.ncb_lana_num     = NetCardNumber;

      NetbiosRequest(&NCB);

      /* if the request was successfull, get the net card */
      /*   id & place it in the NetInfo struct            */
      if (NCB.ncb_retcode == 0)
        {
          iSameNetID = 0;
          for (i=0; i<LENGTH_OF_UNIT_ID; i++)
            {
              pNetInfo->ncsNets[NetCardNumber].bUnitID_Number[i] =
                  AdapterStatus.as_uid[i];

              if (NetCardNumber > 0)
                {
                  if (pNetInfo->ncsNets[NetCardNumber].bUnitID_Number[i] ==
                      pNetInfo->ncsNets[NetCardNumber-1].bUnitID_Number[i])
                    iSameNetID += 1;
                }
            }
          if (iSameNetID == LENGTH_OF_UNIT_ID)
              NCB.ncb_retcode = 1;

        }
      else
        break;

     if (NCB.ncb_retcode > 0)
       break;

      /* setup the NCB for an session status call*/
      NCB.ncb_command     = NETBIOS_SESSION_STATUS;
      NCB.ncb_buffer      = (char far *) &SessionStatus;
      NCB.ncb_length      = sizeof (struct sstat);
      NCB.ncb_callname[0] = '\0';
      NCB.ncb_name[0]     = '*';
      NCB.ncb_post        = 0;
      NCB.ncb_lana_num    = NetCardNumber;

      NetbiosRequest(&NCB);

      /* if the request was successful, place status info in the netinfo struct */
      if (NCB.ncb_retcode == 0)
        {
          if (SessionStatus.ss_numsess <= NUMBER_OF_SESSIONS)
            pNetInfo->ncsNets[NetCardNumber].bNumber_Of_Sessions =
                SessionStatus.ss_numsess;
          else
            {
              pNetInfo->ncsNets[NetCardNumber].bNumber_Of_Sessions = 0;
              SessionStatus.ss_numsess = 0;
            }

          for (i = 0; i < (int) SessionStatus.ss_numsess && 
                      i < NUMBER_OF_SESSIONS; i++)
            memcpy (pNetInfo->ncsNets[NetCardNumber].snRemoteSessionNames[i].szSessionName,
                    SessionStatus.ss_struct[i].ss_rname,16);
        }
      else
        break;

      ClearNcb (&NCB);
    }

  /* if the number of cards detected is over NUMBER_OF_CARDS, */
  /*   we have an error                                       */
  if (NetCardNumber >= NUMBER_OF_CARDS)
    NetCardNumber = 0;

  /* load netinfo with number of netcards */
  pNetInfo->wNumberOfNets = NetCardNumber;
}


/*******************************************************************
*
* Server_Connection procedure tries to establish a session with a server
* to validate the ability of the network to see into the LAN.
*
* LOCAL:
*    tbuff    - time of day buff from NetRemoteTOD
*    tod      - pointer to tbuf
*    sbuff    - hold the names of servers available from NetServerEnum
*    svr_name - holds server name from sbuff = \\name - \\ are appended
*    psrv     - pointer maps over sbuf
*    ser, ste, err - return values
*
**********************************************************************/

BOOL Server_Connection (VOID)
{
  char tbuff[sizeof (struct time_of_day_info)];
  struct time_of_day_info * tod = (struct time_of_day_info *) tbuff;
  char sbuff[BUFSIZ/4], svr_name[CNLEN+3];
  struct server_info_0 *psrv;
  unsigned short ser, ste, err;

  /* Get a list of severs from the browser. The browser maintains the list */
  err = NetServerEnum (NULL, 0, sbuff, sizeof(sbuff), &ser, &ste);

  /* Only check for 8 servers maximum. The browser table has a list of */
  /* servers we have connected to already. Since we have already connected */
  /* to these servers we just want to verify their existance. 8 is used in */
  /* in case a server has been brought down before the browser is updated. */
  /* 8 is also used because anything over this causes Beeping Death. */
  if (ser > (BUFSIZ/4))
      ser=BUFSIZ/4;

  /* if error other than more date, we exit because the enum failed */
  if ((err!=0) && (err!=ERROR_MORE_DATA))
      return (FALSE);

  /* loop through until we successfully connect to a server */
  for (psrv = (struct server_info_0 *)sbuff; ser--; ++psrv)
    {
      strcpy (svr_name, "\\\\");
      strcat (svr_name, psrv->sv0_name);

      /* this is a quick command which if successfull means we have an established connection */
      err = NetRemoteTOD (svr_name, tbuff, sizeof (tbuff));

      /* if we connected to someone, exit. Otherwise we loop until we do or until 8 servers have been processed */
      if (err==0)
        return (TRUE);
    }

  return (FALSE);
}


/*******************************************************************
*
* LAN_Basic_Enhanced proc finds out if the user is running DOS
* Enhanced or DOS Basic LAN Manager. This is done by finding a file
* that must be in the tree which is LAN Manager Specific. This is
* not a good way to do it, but it is the only way.
*
* LOCAL:
*    bufStat - return for stat command
*    path    - hold path of file needed for basic or enhanced
*
* Returns:  NET_LANMAN_BASIC    - If Basic LANMAN detected
*           NET_LANMAN_ENHANCED - If Enhanced LANMAN detected
*           NET_LANMAN          - If Basic or Enhanced could not
*                                 be determined.
**********************************************************************/

BOOL LAN_Basic_Enhanced (NETWORK_STRUCT * pNetInfo)
{
  struct stat bufStat;
  char path[_MAX_PATH];
  int err;

  /* copy (lanroot)\netprog\netwksta.exe into a buffer */
  /* this file must exist if the LAN Manager running is enhanced */
  strcpy (path, pNetInfo->szLanRoot);
  strcat (path, "\\netprog\\netwksta.exe");

  err = stat (path, &bufStat);

  /* if no error, then we have enhanced lan manager */
  if (err == 0)
    return (NET_LANMAN_ENHANCED);

  /* copy (lanroot)\basic\lanman.ini into a buffer */
  /* this file must exist if the LAN Manager running is basic */
  strcpy (path, pNetInfo->szLanRoot);
  strcat (path, "\\basic\\lanman.ini");

  err = stat (path, &bufStat);

  /* if no error, then we have basic lan manager */
  if (err == 0)
    return (NET_LANMAN_BASIC);

  return (NET_LANMAN);
}


/*******************************************************************
*
* Get_CSD_Info looks at the lanman.csd to get info about time and version
* of the csd. It does this by opening (LANROOT)\lanman.csd and parsing
* the file for info, then closing the file.
*
* Local Vars:
*     fileCSD = file pointer for open of lanman.csd
*     buffer  = for read line of lanman.csd
*     result, pbuffer = for file operations
*     CSD_Path = (lanroot)\lanman.csd
*
* Returns: TRUE if an error occured.
**********************************************************************/

int Get_CSD_Info (NETWORK_STRUCT * pNetInfo)
{
  FILE *fileCSD;
  char chBuffer[81], *pbuffer, CSD_Path[_MAX_PATH];
  INT  iResult;
  int  i;

  /* create (lanroot)\lanman.csd */
  strcpy (CSD_Path,pNetInfo->szLanRoot);
  strcat (CSD_Path, "\\Lanman.csd");

  /* OPEN (LANROOT)\lanman.csd */
  fileCSD = OpenFile (CSD_Path, "rb", FALSE);

  if (fileCSD == NULL)
      return (TRUE);

  /* Get first line of lanman.csd */
  iResult = ReadLine (chBuffer, 80, fileCSD, FALSE);

  if (iResult == EOF)
    {
      CloseFile (fileCSD);
      return (TRUE);
    }

  /* Get second line of lanman.csd */
  iResult = ReadLine (chBuffer, 80, fileCSD, FALSE);

  if (iResult == EOF)
    {
      CloseFile (fileCSD);
      return (TRUE);
    }

  /* search for ID in the line, if not there error out, if so get date */
  pbuffer = strstr (chBuffer, "ID");

  if (pbuffer == NULL)
    {
      CloseFile (fileCSD);
      return (TRUE);
    }

  /* skip to date field */
  pbuffer += 3;

  /* loop through extra zero's */
  while (*pbuffer == '0')
    ++pbuffer;

  /* copy date into netinfo */
  for (i = 0; i < 15              &&
              ((*pbuffer != '\n') &&
               (*pbuffer != '\r') &&
               (*pbuffer != '\0') &&
               (i < 6));              i++, pbuffer++)
    pNetInfo->szLanManager_Date[i] = *pbuffer;

  pNetInfo->szLanManager_Date[i] = '\0';

  /* Get third line of lanman.csd */
  iResult = ReadLine (chBuffer, 80, fileCSD, FALSE);

  if (iResult == EOF)
    {
      CloseFile (fileCSD);
      return (TRUE);
    }

  /* search for LM */
  pbuffer = strstr (chBuffer, "LM");

  /* skip to CSD field */
  pbuffer += 2;

  /* loop through extra zero's */
  while (*pbuffer == '0')
    ++pbuffer;

  /* copy current csd level into netinfo */
  for (i=0; i < 6 &&
            ((*pbuffer != '\n') &&
             (*pbuffer != '\r') &&
             (*pbuffer != '\0') &&
             (i < 6));              i++, pbuffer++)
    pNetInfo->szLanManager_CSD[i] = *pbuffer;

  pNetInfo->szLanManager_CSD[i] = '\0';

  CloseFile (fileCSD);
  return (FALSE);
}


/*******************************************************************
*
* GetLM_VersionInfo uses NetConfigGet to parse the lanman.ini which
* has release date, current patch levels and last patch levels.
* LAN Manager 2.1 has this format. Whether LM 3.0 will be this way is
* unclear.
*
* This is for Enhanced LM's. Basic can use api's and has different ini struct
*
* Local Vars:
*    err - return for NetConfigGet
*    len - for NetConfigGet
*    buf - return string of NetConfigGet
*
**********************************************************************/

VOID GetLM_VersionInfo (NETWORK_STRUCT * pNetInfo)
{
  unsigned short int err, len;
  char buf[BUFSIZ], *pbegin;
  int i;

  err = NetConfigGet ("VERSION", "LAN_MANAGER", buf, BUFSIZ, &len);

  /* buf = x.x.x [major].[minor].[patch] */
  if (err == 0)
    {
      /* skip to minor version */
      if (pbegin = strstr (buf, "."))
        {
          ++pbegin;

          /* skip to patch number */
          if (pbegin = strstr (pbegin, "."))
            {

              for (i = 0, ++pbegin; i < 7 && *pbegin != NULL; ++i, ++pbegin)
                pNetInfo->szLanManager_CSD[i] = *pbegin;

              pNetInfo->szLanManager_CSD[i] = '\0';
            }
        }
    }
}


/*********************************************************************
 * SprintNetworkInfo - Put Network information into a set of strings
 *                     to be printed or displayed.
 *
 * pNetInfo     - Network information structure.
 * szSumStrings - Summary strings for summary information.
 *
 * Returns:  NULL if an error occured.
 *********************************************************************/

QSZ * SprintNetworkInfo (NETWORK_STRUCT *pNetInfo,
                         CHAR szSumStrings[][MAX_SUMM_INFO + 5])
{
  WORD wNmbrStrings;        /* Number of strings                     */
  WORD wNmbrChars;          /* Number of characters in the strings   */
  WORD i;                   /* Index variable                        */
  CHAR chBuffer[80];        /* Local string                          */
  QSZ  *pqszStrings;        /* Location for storing string pointers  */
  WORD wAlignColumn;        /* Column to align titles                */


  /* Summary Strings */
  if (szSumStrings != NULL)
    {
      szSumStrings[0][0] ='\0';
      szSumStrings[1][0] ='\0';

      if (pNetInfo->fNetworkActive == FALSE              &&
          pNetInfo->wNetworkType != NET_LANMAN           &&
          pNetInfo->wNetworkType != NET_LANMAN_BASIC     &&
          pNetInfo->wNetworkType != NET_LANMAN_ENHANCED)
        strncpy (szSumStrings[0], paszNetworkTypes[NET_NO_NETWORK],
                 MAX_SUMM_INFO);
      else
        {
          /* Network name */
          /* fixed bug 2414: network was getting reported as "MS Workgroup
             Clien" because only MAX_SUMM_INFO (18) chars were getting copied
             by strnspy.  Can't increase MAX_SUMM_INFO because that will
             break other things.  Added XTRA_LEN_HACK to copy 1 more character...
             wanted to make it longer to avoid this problem with future network
             strings, but receiving buffer is only 23 bytes.  -jeremys */

#define XTRA_LEN_HACK 1

          strncpy (szSumStrings[0], pNetInfo->szNetworkType, MAX_SUMM_INFO
                +XTRA_LEN_HACK);
          strncat (szSumStrings[0], " ",
                   MAX_SUMM_INFO + XTRA_LEN_HACK - strlen (szSumStrings[0]));

          /* Network version */
          if (pNetInfo->wNetworkType == NET_NOVELL)
            {
              sprintf (szSumStrings[1], "Shell %d.%02d.%02d",
                       pNetInfo->wShellMajor,
                       pNetInfo->wShellMinor,
                       pNetInfo->wShellRevision);
            }
          else if (pNetInfo->wNetworkMajor > 0)
            {
              sprintf (chBuffer, "%d.%02d", pNetInfo->wNetworkMajor,
                       pNetInfo->wNetworkMinor);
              if (strlen (chBuffer) + strlen (szSumStrings[0]) > MAX_SUMM_INFO)
                {
                  i = 1;
                  szSumStrings[i][0] = '\0';
                }
              else
                i = 0;

              strncat (szSumStrings[i], chBuffer,
                       MAX_SUMM_INFO - strlen (szSumStrings[i]));
            }
        }

      /* Remove trailing spaces from the first line */
      i = strlen (szSumStrings[0]) - 1;
      while (i >= 1 && szSumStrings[0][i] == ' ')
        szSumStrings[0][i--] = '\0';


      return (NULL);
    }



  /* Overestimate the amount of space required for the strings */

  if (pNetInfo->fNetworkActive == FALSE                    &&
      pNetInfo->wNetworkType != NET_LANMAN                 &&
      pNetInfo->wNetworkType != NET_LANMAN_BASIC           &&
      pNetInfo->wNetworkType != NET_LANMAN_ENHANCED        &&
      pNetInfo->wNetworkType != NET_WORKGROUP_CLIENT)
    {
      wAlignColumn = 18;
      wNmbrStrings = 2;
      wNmbrChars   = wAlignColumn + strlen (pszNo) + 1;
    }
  else
    {
      wAlignColumn = 26;
      wNmbrStrings = 90;
      wNmbrChars   = 3000;
    }


  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the information in place */

  i = 0;

  {
    /* Network Detected */

    QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NETWORK_DETECTED],
                 wAlignColumn);

    if (pNetInfo->fNetworkActive == TRUE               ||
        pNetInfo->wNetworkType == NET_LANMAN           ||
        pNetInfo->wNetworkType == NET_LANMAN_BASIC     ||
        pNetInfo->wNetworkType == NET_LANMAN_ENHANCED)
      Qstrcat (pqszStrings[i], pszYes);
    else
      Qstrcat (pqszStrings[i], pszNo);

    /* Set the next pointer */
    PrepNextString (pqszStrings, i++);

    /* We're done here if there was no network */
    if (pNetInfo->fNetworkActive == FALSE              &&
        pNetInfo->wNetworkType != NET_LANMAN           &&
        pNetInfo->wNetworkType != NET_LANMAN_BASIC     &&
        pNetInfo->wNetworkType != NET_LANMAN_ENHANCED)
      {
        pqszStrings[i] = NULL;
        return (pqszStrings);
      }
  }

  {
    /* Network Name */

    QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NETWORK_NAME],
                 wAlignColumn);
    Qstrcat (pqszStrings[i], pNetInfo->szNetworkType);

    /* Set the next pointer */
    PrepNextString (pqszStrings, i++);
  }

  {
    /* Network Version */
    if ((pNetInfo->wNetworkType  == NET_NOVELL  &&
         pNetInfo->szNovellNetwareVersion[0] != '\0')  ||
        (pNetInfo->wNetworkMajor != 0  ||
         pNetInfo->wNetworkMinor != 0))
      {
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_VERSION],
                     wAlignColumn);

        if (pNetInfo->wNetworkType == NET_NOVELL)
          {
            Qstrcat (pqszStrings[i], pNetInfo->szNovellNetwareVersion);
          }
        else
          {
            sprintf (chBuffer, "%d.%02d", pNetInfo->wNetworkMajor,
                     pNetInfo->wNetworkMinor);
            Qstrcat (pqszStrings[i], chBuffer);
          }

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* MS-DOS Network Functions: [Not] Supported */

    QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_MSNET],
                 wAlignColumn);

    if (pNetInfo->fMsnetCompatible)
      Qstrcat (pqszStrings[i], pszSupported);
    else
      Qstrcat (pqszStrings[i], pszNotSupported);

    /* Set the next pointer */
    PrepNextString (pqszStrings, i++);
  }

  {
    /* MSNET Machine Name */

    if (pNetInfo->fMsnetCompatible)
      {
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_COMPUTER_NAME],
                     wAlignColumn);

        Qstrcat (pqszStrings[i], pNetInfo->szMsnetMachineName);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* NetBIOS Installed */

    QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NETBIOS],
                 wAlignColumn);

    if (pNetInfo->fNetBiosCompatible)
      Qstrcat (pqszStrings[i], pszYes);
    else
      Qstrcat (pqszStrings[i], pszNo);

    /* Set the next pointer */
    PrepNextString (pqszStrings, i++);
  }

  {
    /* NetBIOS Address */

    if (pNetInfo->fNetBiosCompatible)
      {
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NETBIOS_ADDRESS],
                     wAlignColumn);

        sprintf (chBuffer, "%04X:%04X", pNetInfo->wNetBIOSSegment,
                 pNetInfo->wNetBIOSOffset);

        Qstrcat (pqszStrings[i], chBuffer);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  /* LANMAN Information */

  {
    if (pNetInfo->wNetworkType == NET_LANMAN                 ||
        pNetInfo->wNetworkType == NET_LANMAN_BASIC           ||
        pNetInfo->wNetworkType == NET_LANMAN_ENHANCED        ||
        pNetInfo->wNetworkType == NET_WORKGROUP_CLIENT)
      {
        /* LAN Manager Root */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_LANMAN_ROOT],
                     wAlignColumn);

        Qstrcat (pqszStrings[i], pNetInfo->szLanRoot);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* User Name */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_LANMAN_USER_NAME],
                     wAlignColumn);

        Qstrcat (pqszStrings[i], pNetInfo->szUserName);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* Primary Domain */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_LANMAN_DOMAIN],
                     wAlignColumn);

        Qstrcat (pqszStrings[i], pNetInfo->szPrimaryDomain);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* Server Connection */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_LANMAN_SERVER],
                     wAlignColumn);

        if (pNetInfo->fServerConnection == TRUE)
          Qstrcat (pqszStrings[i], "Established");
        else
          Qstrcat (pqszStrings[i], "Not Established");

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* Mailslot Support */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_LANMAN_MAILSLOT],
                     wAlignColumn);

        if (pNetInfo->fMailslot_Support == TRUE)
          Qstrcat (pqszStrings[i], pszYes);
        else
          Qstrcat (pqszStrings[i], pszNo);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* API Support */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_LANMAN_API],
                     wAlignColumn);

        if (pNetInfo->fAPI_Support == TRUE)
          Qstrcat (pqszStrings[i], pszYes);
        else
          Qstrcat (pqszStrings[i], pszNo);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* LAN Manager Date */
        if (pNetInfo->szLanManager_Date[0] != '\0')
          {
            QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_LANMAN_DATE],
                         wAlignColumn);

            if (strlen(pNetInfo->szLanManager_Date) == 5)
                sprintf (chBuffer, "0%c/%c%c/%c%c",
                         pNetInfo->szLanManager_Date[0],
                         pNetInfo->szLanManager_Date[1],
                         pNetInfo->szLanManager_Date[2],
                         pNetInfo->szLanManager_Date[3],
                         pNetInfo->szLanManager_Date[4]);
            else
                sprintf (chBuffer, "%c%c/%c%c/%c%c",
                         pNetInfo->szLanManager_Date[0],
                         pNetInfo->szLanManager_Date[1],
                         pNetInfo->szLanManager_Date[2],
                         pNetInfo->szLanManager_Date[3],
                         pNetInfo->szLanManager_Date[4],
                         pNetInfo->szLanManager_Date[5]);

            Qstrcat (pqszStrings[i], chBuffer);

            /* Set the next pointer */
            PrepNextString (pqszStrings, i++);
          }


        /* LAN Manager Date */
        if (pNetInfo->szLanManager_CSD[0] != '\0')
          {
            QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_LANMAN_PATCH_LEVEL],
                         wAlignColumn);

            Qstrcat (pqszStrings[i], pNetInfo->szLanManager_CSD);

            /* Set the next pointer */
            PrepNextString (pqszStrings, i++);
          }
      }
  }

  /* Novell Information */

  {
    if (pNetInfo->wNetworkType == NET_NOVELL)
      {
        /* Shell version */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NOVELL_SHELL_VER],
                     wAlignColumn);

        sprintf (chBuffer, "%d.%02d.%02d", pNetInfo->wShellMajor,
                 pNetInfo->wShellMinor, pNetInfo->wShellRevision);

        Qstrcat (pqszStrings[i], chBuffer);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* Shell Type */
        if (pNetInfo->wShellMajor * 100 + pNetInfo->wShellMinor >= 301 &&
            pNetInfo->wShellType <= 2)
          {
            QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NOVELL_SHELL_TYPE],
                         wAlignColumn);

            switch (pNetInfo->wShellType)
              {
                case 0:
                  Qstrcat (pqszStrings[i], pszConventional);
                  break;

                case 1:
                  Qstrcat (pqszStrings[i], "Expanded Memory (EMS)");
                  break;

                case 2:
                  Qstrcat (pqszStrings[i], "Extended Memory (XMS)");
                  break;

                default:
                  Qstrcat (pqszStrings[i], pszUnknown);
              }

            /* Set the next pointer */
            PrepNextString (pqszStrings, i++);
          }


        /* Shell OS */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NOVELL_SHELL_OS],
                     wAlignColumn);
        if (pNetInfo->wNovellShellOs == 0)
          Qstrcat (pqszStrings[i], pszMsDos);
        else
          Qstrcat (pqszStrings[i], pNetInfo->szNovellShellOs);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* Shell OS Version */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NOVELL_SHELL_OS_VER],
                     wAlignColumn);
        Qstrcat (pqszStrings[i], pNetInfo->szNovellShellOsVersion);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* Hardware Type */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NOVELL_HDW_TYPE],
                     wAlignColumn);
        Qstrcat (pqszStrings[i], pNetInfo->szNovellHdwType);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* Station number */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NOVELL_STATION_NMBR],
                     wAlignColumn);
        sprintf (chBuffer, "%d", pNetInfo->wStationNmbr);
        Qstrcat (pqszStrings[i], chBuffer);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* Physical station number */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NOVELL_PHYSICAL_NMBR],
                     wAlignColumn);
        sprintf (chBuffer, "%04X:%04X:%04X",  pNetInfo->wPhysicalStaNmbr1,
                 pNetInfo->wPhysicalStaNmbr2, pNetInfo->wPhysicalStaNmbr3);
        Qstrcat (pqszStrings[i], chBuffer);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* IPX Installed */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NOVELL_IPX],
                     wAlignColumn);
        Qstrcat (pqszStrings[i], (pNetInfo->fIpxInstalled) ? pszYes : pszNo);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* SPX Installed */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NOVELL_SPX],
                     wAlignColumn);
        Qstrcat (pqszStrings[i], (pNetInfo->fSpxInstalled) ? pszYes : pszNo);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);


        /* ODI/LSL Installed */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_NOVELL_ODI_LSL],
                     wAlignColumn);
        Qstrcat (pqszStrings[i], (pNetInfo->fOdiLslInstalled) ? pszYes : pszNo);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* LANtastic Information */

    if (pNetInfo->wNetworkType == NET_LANTASTIC)
      {
        /* Server */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_LANTASTIC_SERVER],
                     wAlignColumn);

        if (pNetInfo->fLANtasticServer)
          Qstrcat (pqszStrings[i], pszYes);
        else
          Qstrcat (pqszStrings[i], pszNo);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);



        /* Redir */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_LANTASTIC_REDIR],
                     wAlignColumn);

        if (pNetInfo->fLANtasticRedir)
          Qstrcat (pqszStrings[i], pszYes);
        else
          Qstrcat (pqszStrings[i], pszNo);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);



        /* Server */
        QstrcpyAlign (pqszStrings[i], paszNetworkTitles[NET_LANTASTIC_POPUP],
                     wAlignColumn);

        if (pNetInfo->fLANtasticPopUp)
          Qstrcat (pqszStrings[i], pszYes);
        else
          Qstrcat (pqszStrings[i], pszNo);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

#if HEAP_DEBUG
      HeapCheck ("Before NetBiosStrings");
#endif

  /* Add NetBIOS strings */
  if (pNetInfo->wNumberOfNets != 0)
    NetBiosStrings (pNetInfo, pqszStrings, &i);

#if HEAP_DEBUG
      HeapCheck ("After NetBiosStrings");
#endif

  /* Additional LANMAN information */
  if (pNetInfo->wNetworkType == NET_LANMAN                 ||
      pNetInfo->wNetworkType == NET_LANMAN_BASIC           ||
      pNetInfo->wNetworkType == NET_LANMAN_ENHANCED        ||
      pNetInfo->wNetworkType == NET_WORKGROUP_CLIENT)
    {
      /* Add the list of workstation services */
      GetServicesInfo (pqszStrings, &i);
#if HEAP_DEBUG
      HeapCheck ("After GetServicesInfo");
#endif

      /* Add the PROTOCOL.INI information */
      GetProtocolInfo (pNetInfo, pqszStrings, &i);
#if HEAP_DEBUG
      HeapCheck ("After GetProtocolInfo");
#endif

      if (pNetInfo->wNetworkType != NET_WORKGROUP_CLIENT)
        {
          /* Add the driver information from LANMAN.INI */
          GetLanmanIniDriverInfo (pNetInfo, pqszStrings, &i);
#if HEAP_DEBUG
          HeapCheck ("After GetLanmanIniDriverInfo");
#endif
        }
    }


  /* Set the last pointer to NULL */
  pqszStrings[i] = NULL;

  /* Return the pointer to pqszStrings */
  return (pqszStrings);
}


/*********************************************************************
 * NetBiosStrings - Adds the NetBIOS strings to pqszStrings.
 *
 * pNetInfo     - Network information structure.
 * pqszStrings  - String array to add the NetBIOS strings to.
 * pI           - Pointer to current line counter.
 *
 * Returns:  NULL if an error occured.
 *********************************************************************/

VOID NetBiosStrings (NETWORK_STRUCT * pNetInfo,
                     QSZ            * pqszStrings,
                     INT            * pI)
{
  CHAR chBuffer[80];        /* Local character buffer          */
  WORD i, j, n;             /* Looping variables               */
  WORD wIndex = *pI;        /* Index to pqszStrings            */
  WORD wNmbrSessions = 0;   /* Count of the number of sessions */


  /* Blank line */
  pqszStrings[wIndex][0] = '\0';
  PrepNextString (pqszStrings, wIndex++);

  /* Title line */
  Qstrcpy (pqszStrings[wIndex], paszNetworkTitles[NET_LANMAN_NET_CARD]);
  PrepNextString (pqszStrings, wIndex++);

  for (i = 0; i < pNetInfo->wNumberOfNets; ++i)
    {
      sprintf (chBuffer, "  Net%2.2d ID: %2.2X%2.2X%2.2X%2.2X%2.2X%2.2X",
               i + 1,
               pNetInfo->ncsNets[i].bUnitID_Number[0],
               pNetInfo->ncsNets[i].bUnitID_Number[1],
               pNetInfo->ncsNets[i].bUnitID_Number[2],
               pNetInfo->ncsNets[i].bUnitID_Number[3],
               pNetInfo->ncsNets[i].bUnitID_Number[4],
               pNetInfo->ncsNets[i].bUnitID_Number[5]);

      Qstrcpy (pqszStrings[wIndex], chBuffer);
      PrepNextString (pqszStrings, wIndex++);

      sprintf (chBuffer, "  Active Sessions for Net%2.2d", i + 1);
      Qstrcpy (pqszStrings[wIndex], chBuffer);
      PrepNextString (pqszStrings, wIndex++);

      /* Indent for the session names */
      Qstrcpy (pqszStrings[wIndex], "      ");

      /* Add the session names */
      for (j = 0; j < (WORD) pNetInfo->ncsNets[i].bNumber_Of_Sessions &&
                  j < NUMBER_OF_SESSIONS; ++j)
        {
          if (pNetInfo->ncsNets[i].snRemoteSessionNames[j].szSessionName[0] != '*')
            {

              for (n = 0;
                   n < 79 &&
                   pNetInfo->ncsNets[i].snRemoteSessionNames[j].szSessionName[n] > ' '; n++)
                chBuffer[n] = pNetInfo->ncsNets[i].snRemoteSessionNames[j].szSessionName[n];

              chBuffer[n] = '\0';

              /* Is a ", " needed */
              if (wNmbrSessions++ > 0)
                Qstrcat (pqszStrings[wIndex], pszCommaSpace);

              /* Does this session name need to be word wrapped */
              if (Qstrlen (pqszStrings[wIndex]) +
                  strlen (chBuffer) + 2 > REPORT_WIDTH)
                {
                  PrepNextString (pqszStrings, wIndex++);
                  Qstrcpy (pqszStrings[wIndex], "      ");
                }

              /* Add the session name */
              Qstrcat (pqszStrings[wIndex], chBuffer);
            }
        }

      PrepNextString (pqszStrings, wIndex++);
    }

  *pI = wIndex;
}


/*********************************************************************
 * GetServicesInfo calls NetServicEnum and NetServiceGetInfo to
 * detect services that may be installed or uninstalled on DOS LANMAN
 * enhanced.
 *
 * LOCAL vars
 *     err      - holds return error of netwkstagetinfo
 *     ta       - contains total available bytes from netwkstagetinfo
 *     wkstabuf - buffer info for netwkstagetinfo
 *     wksta0   - stuct mapping to wkstabuf which hold info from
 *                netwkstagetinfo
 *
 * Returns: TRUE if an error occured.
 *********************************************************************/

BOOL GetServicesInfo (QSZ *pqszStrings, INT *pI)
{
  unsigned short int err = 0, en = 0, ta = 0;
  char sbuf[BUFSIZ], sinfobuf[BUFSIZ];
  struct service_info_0 *service0;
  struct service_info_1 *service1;
  WORD wIndex = *pI;        /* Index to pqszStrings */


  /* Clear out the input buffers */
  memset (sbuf, '\0', BUFSIZ);
  memset (sinfobuf, '\0', BUFSIZ);

  /* Blank line */
  pqszStrings[wIndex][0] = '\0';
  PrepNextString (pqszStrings, wIndex++);

  /* Title line */
  Qstrcpy (pqszStrings[wIndex], paszNetworkTitles[NET_LANMAN_WKSTA_SVS]);
  PrepNextString (pqszStrings, wIndex++);


  /* Get list of services loaded */
  err = NetServiceEnum (NULL, 0, sbuf, sizeof (sbuf), &en, &ta);

  if ((err != 0) || (en == 0))
    {
      Qstrcpy (pqszStrings[wIndex], "  No Services Loaded");
      PrepNextString (pqszStrings, wIndex++);
    }

  if (err != 0)
    return (FALSE);

  /* loop through all services. See if installed. add to buffer */
  for (service0 = (struct service_info_0 *)sbuf; en--; ++service0)
    {
      /* Clear out the string */
      Qmemset (pqszStrings[wIndex], ' ', 21);
      pqszStrings[wIndex][21] = '\0';

      /* Load the service name */
      Qstrncpy (&pqszStrings[wIndex][2], service0->svci0_name,
               strlen (service0->svci0_name));


      /* See if the service is installed */
      err = NetServiceGetInfo (NULL, service0->svci0_name, 1,
                               sinfobuf, sizeof(sinfobuf), &ta);

      service1 = (struct service_info_1 *)sinfobuf;

      /* Put in the answer */
      if ((service1->svci1_status & SERVICE_INSTALL_STATE) == SERVICE_INSTALLED)
        Qstrcat (pqszStrings[wIndex], "Installed");

      if ((service1->svci1_status & SERVICE_INSTALL_STATE) == SERVICE_UNINSTALLED)
        Qstrcat (pqszStrings[wIndex], "Uninstalled");

      if ((service1->svci1_status & SERVICE_INSTALL_STATE) == SERVICE_UNINSTALL_PENDING)
        Qstrcat (pqszStrings[wIndex], "Install Pending");

      if ((service1->svci1_status & SERVICE_INSTALL_STATE) == SERVICE_INSTALL_PENDING)
        Qstrcat (pqszStrings[wIndex], "Uninstall Pending");

      PrepNextString (pqszStrings, wIndex++);
    }

  *pI = wIndex;

  return (FALSE);
}


/*********************************************************************
 * GetProtocolInfo - Puts PROTOCOL.INI informatione into pqszStrings.
 *
 * LOCAL:
 *    fileProtocolIni - file pointer of fopen
 *    ProtocolPath    - holds (lanroot)\protocol.ini
 *    pBuffer       - needed for strstr.
 *
 * Returns: TRUE if an error occured.
 *********************************************************************/

BOOL GetProtocolInfo (NETWORK_STRUCT *pNetInfo, QSZ *pqszStrings, INT *pI)
{
  FILE * fileProtocolIni;
  CHAR szProtocolPath[_MAX_PATH];
  CHAR chBuffer[80];
  CHAR *pBuffer = NULL;
  CHAR NifPath[_MAX_PATH];
  CHAR NifFile[32];
  INT  i;
  WORD wIndex = *pI;        /* Index to pqszStrings */
  BOOL fProtFound = FALSE;  /* TRUE when "[PROT" is found */


  /* Blank line */
  pqszStrings[wIndex][0] = '\0';
  PrepNextString (pqszStrings, wIndex++);

  /* Title line */
  Qstrcpy (pqszStrings[wIndex], paszNetworkTitles[NET_LANMAN_PROTOCOLS]);
  PrepNextString (pqszStrings, wIndex++);


  /* Create (lanroot)\protocol.ini path */
  strcpy (szProtocolPath, pNetInfo->szLanRoot);
  strcat (szProtocolPath, "\\PROTOCOL.INI");

  /* OPEN file (lanroot)\protocol.ini */
  fileProtocolIni = OpenFile (szProtocolPath, "rb", FALSE);
  if (fileProtocolIni == NULL)
    return (TRUE);


  /* Find the [PROT section */
  while (ReadLine (chBuffer, 80, fileProtocolIni, FALSE) != EOF)
    {
      static PSZ pszProt = "[PROT";


      /* Find the [PROTMAN] or [PROTOCOL MANAGER] section */
      if (fProtFound == FALSE)
        {
          if (memicmp (pszProt, chBuffer, 5) == 0)
            fProtFound = TRUE;
        }

      if (fProtFound == TRUE)
        {
          /* Break out if another section is found */
          if (chBuffer[0] == '[' && memicmp (pszProt, chBuffer, 5) != 0)
            break;

          /* Add the lines in the [PROTMAN] or [PROTOCOL MANAGER] section */
          if ((chBuffer[0] != '\r') && (chBuffer[0] != '\n'))
            {
              Qstrcpy (pqszStrings[wIndex], "  ");
              Qstrcat (pqszStrings[wIndex], chBuffer);

              PrepNextString (pqszStrings, wIndex++);
            }
        }
    }


  /* Rewind the file to look for .NIF filenames */
  rewind (fileProtocolIni);


  /* Get lines of protocol.ini and put them into the pszBufferPosition */
  while (ReadLine (chBuffer, 80, fileProtocolIni, FALSE) != EOF)
    {
      /* Search for possible nif file */
      if (chBuffer[0] == '[')
        {
          pBuffer = strstr (chBuffer, "_NIF");

          if (pBuffer != NULL)
            {
              /* create nif file path */
              strcpy (NifPath, pNetInfo->szLanRoot);
              strcat (NifPath, "\\drivers\\nif\\");

              /* create nif file */
              for (i=1; i < 79 && chBuffer[i] != '_'; i++)
                NifFile[i - 1]=chBuffer[i];

              NifFile[i - 1]= '.';

              for (i = i + 1; i < 79 && chBuffer[i] != ']'; i++)
                NifFile[i-1]=chBuffer[i];

              NifFile[i - 1] = '\0';

              strcat (NifPath, NifFile);

              *pI = wIndex;

              /* Add model number to PROTOCOL.INI info */
              GetNifModelInfo (NifPath, pqszStrings, pI);
            }
        }
    }

  /* Close PROTOCOL.INI */
  CloseFile (fileProtocolIni);

  *pI = wIndex;

  return (FALSE);
}


/*********************************************************************
 * GetNifModelInfo - Puts Model line in .NIF file into pqszStrings.
 *
 * LOCAL:
 *    fileNif  - file pointer of fopen
 *    buffer - used for fget
 *    pBuffer  - needed with strstr
 *    pCase  - needed with strupr. used on buffer.
 *
 * Returns: TRUE if an error occured.
 *********************************************************************/

BOOL GetNifModelInfo (PSZ pszNifFile, QSZ * pqszStrings, INT * pI)
{
  FILE * fileNif;
  CHAR chBuffer[81], * pBuffer, * pCase;

  /* OPEN file pszNifFile */
  fileNif = OpenFile (pszNifFile, "rb", FALSE);

  if (fileNif == NULL)
    return (TRUE);

  /* Get lines of Nif file and put Model line into pqszStrings */
  while (ReadLine (chBuffer, 80, fileNif, FALSE) != EOF)
    {
      pCase = strupr (chBuffer);
      pBuffer = strstr (pCase, "MODEL");

      if (pBuffer != NULL)
        {
          Qstrcpy (pqszStrings[*pI], "  ");
          Qstrcat (pqszStrings[*pI], chBuffer);
          PrepNextString (pqszStrings, (*pI)++);
          break;
        }
    }

  /* Close the .NIF file */
  CloseFile (fileNif);

  return (FALSE);
}


/*********************************************************************
 * GetLanmanIniDriverInfo - Puts Lanman Driver info from LANMAN.INI
 *                          into pqszStrings.
 *
 * LOCAL:
 *    fileLanmanIni    - file pointer of OpenFile
 *    LanmanPath       - holds (lanroot)\lanman.ini
 *    buffer           - used for fget
 *    LanmanDriverPath - holds (lanroot)\drivers. used for strstr with
 *                       chBuffer.
 *    pBuffer          - needed with strstr
 *    pCase            - needed with strupr. used on buffer.
 *
 * Returns: TRUE if an error occured.
 **********************************************************************/

BOOL GetLanmanIniDriverInfo (NETWORK_STRUCT *pNetInfo,
                             QSZ            *pqszStrings,
                             INT            *pI)
{
  FILE *fileLanmanIni;
  CHAR szLanmanPath[_MAX_PATH];
  CHAR chBuffer[REPORT_WIDTH * 3 - 4];
  CHAR szLanmanDriverPath[_MAX_PATH];
  CHAR *pBuffer = NULL;
  CHAR *pCase = NULL;
  PSZ  pszString;


  /* Blank line */
  pqszStrings[*pI][0] = '\0';
  PrepNextString (pqszStrings, (*pI)++);

  /* Title line */
  Qstrcpy (pqszStrings[*pI], paszNetworkTitles[NET_LANMAN_INI]);
  PrepNextString (pqszStrings, (*pI)++);


  /* create (lanroot)\lanman.ini */
  strcpy (szLanmanPath, pNetInfo->szLanRoot);
  strcat (szLanmanPath, "\\LANMAN.INI");

  /* create (lanroot)\drivers */
  strcpy (szLanmanDriverPath, pNetInfo->szLanRoot);
  strcat (szLanmanDriverPath, "\\DRIVERS");

  /* OPEN file (lanroot)\lanman.ini */
  fileLanmanIni = OpenFile (szLanmanPath, "rb", FALSE);
  if (fileLanmanIni == NULL)
    return (TRUE);

  /* Get lines of lanman.ini and search for lanroot\drivers. If found */
  /*   put the line into the pqszStrings.                             */

  while (ReadLine (chBuffer, REPORT_WIDTH * 3 - 5, fileLanmanIni, FALSE) != EOF)
    {
      if ((chBuffer[0] != '\r') && (chBuffer[0] != '\n'))
        {
          pCase   = strupr(chBuffer);
          pBuffer = strstr(pCase, szLanmanDriverPath);
          if (pBuffer != NULL)
            {
              Qstrcpy (pqszStrings[*pI], chBuffer);
              pszString = &chBuffer[0];

              /* Wrap the string if necessary */
              while (Qstrlen (pqszStrings[*pI]) > REPORT_WIDTH)
                {
                  pqszStrings[*pI][REPORT_WIDTH] = '\0';
                  pszString += REPORT_WIDTH;

                  PrepNextString (pqszStrings, (*pI)++);

                  Qstrcpy (pqszStrings[*pI], pszString);
                }

              PrepNextString (pqszStrings, (*pI)++);
            }
        }
    }

  /* Close LANMAN.INI */
  CloseFile (fileLanmanIni);

  return (FALSE);
}


/*********************************************************************
 * PcNfsInstalled - Detects if PCNFS.SYS is loaded.
 *
 * This is the mail from a developer at Sun, given to me by Tom Trinneer
 * from Sun Microsystems here in Bellevue (email tom.trinneer@west.sun.com
 *
 * The customer can use the DOS GetMachineName call (sorry, I forget
 * the number - I'm dialled in from home - check the DOS Tech Ref).
 * If it returns a name, a network is installed. To see if it's PC-NFS,
 * try opening the device "PC-NFS$$". If the device can be opened, PC-NFS
 * is loaded. This is not 100% unfortunately; it is possible (but unlikely)
 * that PCNFS.SYS has been loaded in CONFIG.SYS but that another network
 * has in fact been started. There are other, more elaborate checks, but
 * those require knowledge of PC-NFS undocumented interfaces.
 *
 * Returns: TRUE if the network was detected.
 **********************************************************************/

BOOL PcNfsInstalled (VOID)
{
  struct stat statBuffer;   /* File status buffer                */

  /* If the device "PC-NFS$$" does exist, return TRUE */
  if (stat ("PC-NFS$$", &statBuffer) == 0)
    return (TRUE);
  else
    return (FALSE);
}


/***********************************************************************
 * IsWorkgrpSysInstalled - Determines if WORKGRP.SYS/NET$HLP$ is
 *                         installed in memory.
 *
 * Returns: TRUE if WORKGRP.SYS/NET$HLP$ is found, FALSE otherwise.
 ***********************************************************************/

WORD IsWorkgrpSysInstalled (void)
{
  FILE *file;   /* File handle */


  /* Determine if NET$HELP$ exists */

  if ((file = fopen ("NET$HLP$", "r")) != NULL)
    {
      fclose (file);
      return (TRUE);
    }
  else
    return (FALSE);
}
