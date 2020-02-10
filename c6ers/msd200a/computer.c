/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * COMPUTER.C - Source file for obtaining the computer data:  BIOS,
 *              Processor, and Keyboard.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/* Computer names */

PSZ ppszComputerNames[] =
  {
    "Unknown",
    "Amstrad", "Wang", "Apricot", "Bull ", "Toshiba", "NEC ", "Texas Inst",
    "Austin", "Cumulus", "Ergo", "Hynudai", "Nokia", "Ogivar", "Siemens",
    "Tandem", "Tandon", "Mitsubishi", "Matsushita", "Sharp", "DEC ",
    "Hewlett Packard", "Hewlett-Packard", "Epson", "Leading Edge", "NCR ",
    "Acer", "ALR ", "Norsk", "Altec", "Altos", "Televideo", "Dell ",
    "Commodore", "CompuAdd", "Gateway", "Hauppauge", "Northgate", "Goldstar",
    "Laser", "Normerel", "Panasonic", "Swan", "Orange", "Xerox", "Zeos",
    "Parallan", "Ampro", "AT&T", "Club ", "Data General", "Sperry", "Unisys",
    "Arche", "PC Brand", "Elonex", "Emerson", "DFI", "SDI ", "AIR ", "CSS ",
    "Dolsh", "Mitsuba", "Osicom", "Polywell", "SAI ", "Micro Express",
    "Packard Bell", "PC Craft", "Headstart", "Telex", "Mitac", "Everex",
    "DTK ", "Koyro", "Tandy", "Wyse", "Zenith", "Amkly", "AST ", "Grid",
    "Cheetah", "Psion", "US Micro", "Smith Corona", "Reply", "Dolch",
    "Aquiline", "Comtrade", "Cardinal", "Compudyne", "Schmit", "Victor",
    "Chicony", "Brother", "Elonex"
  };


/* BIOS names */

PSZ ppszBiosNames[] =
  {
    "Unknown",
    "Phoenix", "AMI ", "American Megatrends", "Award", "DTK", "Micronics",
    "ERSO", "Datatech", "Data Tech", "Olivetti", "Intel", "Seiko", "Ultima",
    "Mylex", "Artek", "Zenith", "(C)ZDS ", "Western Digital",
    "Compaq", "Monolithic", "IBM",
    NULL
  };


COMPUTER_STRUCT FAR *fpComputer = NULL;   /* Stored computer info */

/*********************************************************************
 * GetComputerInfo - Gets the computer related information:  BIOS and
 *                   computer manufacturer, BIOS version, and
 *                   keyboard type.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetComputerInfo (COMPUTER_STRUCT *pComputer, BOOL fMinimumInfo)
{
  union  REGS inregs, outregs;  /* Register structures   */
  BYTE FAR * fbByte = NULL;     /* Far pointer to a byte */
  PSZ pszString     = NULL;     /* String pointer        */
  WORD i;                       /* Looping variable      */


  /* We obtain the computer information only once, then store the    */
  /*   computer information for later requests.  If a second request */
  /*   is made (ie., for IRQ info, etc.), this routine will copy the */
  /*   stored computer information to pComputer and return           */
  /*   immediately.  This is done because obtaining the computer     */
  /*   information after obtaining information about a serial mouse  */
  /*   hard hangs the computer.                                      */

  if (fpComputer != NULL)
    {
      _fmemcpy ((COMPUTER_STRUCT FAR *) pComputer, fpComputer,
                 sizeof (COMPUTER_STRUCT));
      return (FALSE);
    }


  {
    /* Get Computer Name */

    i = GetRomName (GET_COMPUTER_NAME, ppszComputerNames);
    strcpy (pComputer->szComputerName, ppszComputerNames[i]);
    pComputer->wComputerName = i;
  }

  {
    /* Get BIOS Manufacturer */

    i = GetRomName (GET_BIOS_MFGR, ppszBiosNames);
    strcpy (pComputer->szBiosMfgr, ppszBiosNames[i]);
    pComputer->wBiosMfgr = i;
  }

  {
    /* Get IRQ and related Information */

    GetComputerIrqInfo (pComputer);
  }

  {
    /* If mimimum information requested, return now */

    /* Removed */
    /*
    if (fMinimumInfo)
      return (FALSE);
    */
  }

  {
    /* Get BIOS Version */

    GetRomVersion (pComputer->aszBiosVersion,
                   (CHAR FAR *) 0xF0000000, 0xFFFF);
  }

  {
    /* Get the BIOS Date */

    GetRomDate ((CHAR FAR *) 0xF0000000, 0xFFFF, pComputer->szBiosDate);
  }

  {
    /* Determine the BIOS Category */

    GetBiosCategory (pComputer);
  }


  {
    /* Determine Keyboard type */

    /* 0040:0096 is keyboard status byte 3:               */
    /*   If bit 4 is set, an enhanced keyboard is present */

    fbByte = (BYTE FAR *) 0x00400096;

    pszString = (*fbByte & 0x10) ? pszEnhanced : pszNonEnhanced;

    strcpy (pComputer->szKeyboardType, pszString);
  }

  {
    /* Determine the presence of a DMA Controller Chip */

    int86 (0x11, &inregs, &outregs);

    /* If the AX register, bit 8 == 0, then there is a DMA controller. */
    /*   Since AX bit 8 == AH bit 0, I use regs.h.ah.                  */

    pComputer->fDmaChipPresent = !(outregs.h.ah & 1);
  }

  /* Store the computer structure for future use */
  fpComputer = _fmalloc (sizeof (COMPUTER_STRUCT));
  if (fpComputer != NULL)
    {
      _fmemcpy (fpComputer, (COMPUTER_STRUCT FAR *) pComputer,
                sizeof (COMPUTER_STRUCT));
    }

  return (FALSE);
}


/*********************************************************************
 * GetComputerIrqInfo - Gets the IRQ related computer information,
 *                      (Cascaded IRQ2, Co-processor, Real Time
 *                      Clock).
 *
 * pComputer - Computer information structure.
 *********************************************************************/

BOOL GetComputerIrqInfo (COMPUTER_STRUCT *pComputer)
{
  union  REGS inregs, outregs;  /* Register structures         */
  struct SREGS sregs;           /* Segement register structure */
  BYTE FAR * fbByte = NULL;     /* Far pointer to a byte       */
  PSZ pszString     = NULL;     /* String pointer              */


  /* We obtain the computer information only once, then store the    */
  /*   computer information for later requests.  If a second request */
  /*   is made (ie., for IRQ info, etc.), this routine will copy the */
  /*   stored computer information to pComputer and return           */
  /*   immediately.  This is done because obtaining the computer     */
  /*   information after obtaining information about a serial mouse  */
  /*   hard hangs the computer.                                      */

  if (fpComputer != NULL)
    {
      _fmemcpy ((COMPUTER_STRUCT FAR *) pComputer, fpComputer,
                 sizeof (COMPUTER_STRUCT));
      return (FALSE);
    }


  {
    /* Get System Configuration Call */

    segread (&sregs);
    inregs.x.ax = 0;
    inregs.x.bx = 0;
    inregs.x.cx = 0;
    inregs.x.dx = 0;
    inregs.x.si = 0;
    inregs.x.di = 0;
    inregs.x.cflag = 0;
    inregs.h.ah = 0xC0;
    int86x (0x15, &inregs, &outregs, &sregs);

    /* Set the flag if this call is not supported on this BIOS */
    pComputer->fConfigCallSupported = !outregs.x.cflag;

    if (!pComputer->fConfigCallSupported)
      {
        /* If the "System Configuration Call" is not supported, */
        /*   the Computer Type byte is at F000:FFFE.            */

        fbByte = (BYTE FAR *) 0xF000FFFE;
        pComputer->bComputerType      = *fbByte;
        pComputer->bSubModel          = 0;
        pComputer->bRevisionLevel     = 0;
        pComputer->wComputerClass     = COMPUTER_CLASS_XT;
        pComputer->fFixedDiskUsesDMA3 = FALSE;
        pComputer->fCascadeIntLvl2    = FALSE;
        pComputer->fRealTimeClock     = FALSE;
      }
    else
      {
        /* Otherwise, get the information */

        fbByte = (BYTE FAR *)
                   (((DWORD) sregs.es << 16) + (DWORD) outregs.x.bx);

        pComputer->bComputerType      = fbByte[2];
        pComputer->bSubModel          = fbByte[3];
        pComputer->bRevisionLevel     = fbByte[4];
        pComputer->fFixedDiskUsesDMA3 = (fbByte[5] & 0x80) ? TRUE : FALSE;
        pComputer->fCascadeIntLvl2    = (fbByte[5] & 0x40) ? TRUE : FALSE;
        pComputer->fRealTimeClock     = (fbByte[5] & 0x20) ? TRUE : FALSE;


        /* Determine Bus type and computer category */

        if (fbByte[5] & 0x02)
          {
            strcpy (pComputer->szBusType, pszMicroChannel);
            pComputer->wBusType  = MICRO_CHANNEL;
            pComputer->wComputerClass = COMPUTER_CLASS_PS2;
          }
        else
          {
            /* Is this an EISA bus? */

            segread (&sregs);
            inregs.x.ax = 0;
            inregs.x.bx = 0;
            inregs.x.cx = 0;
            inregs.x.dx = 0;
            inregs.x.si = 0;
            inregs.x.di = 0;
            inregs.x.cflag = 0;
            inregs.x.ax = 0xD804;
            inregs.x.cx = 0x0000;
            int86 (0x15, &inregs, &outregs);
            if (outregs.h.ah != 0x86)
              {
                strcpy (pComputer->szBusType, pszEisaBus);
                pComputer->wBusType  = EISA_BUS;
                pComputer->wComputerClass = COMPUTER_CLASS_AT;
              }
            else
              {
                if (pComputer->fCascadeIntLvl2)
                  {
                    strcpy (pComputer->szBusType, pszAtBus);
                    pComputer->wBusType  = AT_BUS;
                    pComputer->wComputerClass = COMPUTER_CLASS_AT;
                  }
                else
                  {
                    strcpy (pComputer->szBusType, pszXtBus);
                    pComputer->wBusType  = XT_BUS;
                    pComputer->wComputerClass = COMPUTER_CLASS_XT;
                  }
              }
          }
      }

    /* Get Extended BIOS Data Segment */

    if (fbByte[5] & 0x04)
      {
        segread (&sregs);
        inregs.x.ax = 0;
        inregs.x.bx = 0;
        inregs.x.cx = 0;
        inregs.x.dx = 0;
        inregs.x.si = 0;
        inregs.x.di = 0;
        inregs.x.cflag = 0;
        inregs.h.ah = 0xC1;
        int86x (0x15, &inregs, &outregs, &sregs);

        if (outregs.x.cflag || sregs.es < 0x1000)
          {
            pComputer->wExtAreaSeg = 0;
            pComputer->wExtAreaLen = 0;
          }
        else
          {
            /* The first byte in the extended BIOS data segment */
            /*   tells the size (in K) of the segment           */
            pComputer->wExtAreaSeg = sregs.es;
            fbByte = (BYTE FAR *) ((DWORD) pComputer->wExtAreaSeg << 16);
            pComputer->wExtAreaLen = *fbByte;

            /* Assume it doesn't exist if it's too large */

          }
      }
  }

  {
    WORD wChipTypes;    /* Stores the chip types                    */
    WORD wProcessor;    /* "chips()" encoding for processor type    */
    WORD wCoProcessor;  /* "chips()" encoding for co-processor type */


    /* Determine Processor and Co-processor Types */

    wChipTypes   = chips();

    wProcessor   = wChipTypes / 10;
    wCoProcessor = wChipTypes % 10;


    /* Set the values for the processor */

    switch (wProcessor)
      {
      case 8:
          pComputer->wProcessor = _8088;
          pszString = pszIntel8088;
          break;

        case 18:
          pComputer->wProcessor = _80186;
          pszString = pszIntel80186;
          break;

        case 28:
          pComputer->wProcessor = _80286;
          pszString = pszIntel80286;
          break;

        case 38:
          pComputer->wProcessor = _80386;
          pszString = pszIntel80386;
          break;

        case 48:
          /* If a 80837 was detected, it is a i486 */
          /*   If not, it was a 486SX              */

          if (wCoProcessor == 3)
            {
              pComputer->wProcessor = _80486;
              pszString = pszInteli486;
            }
          else
            {
              pComputer->wProcessor = _80486SX;
              pszString = pszIntel486SX;
            }

          break;

        case 20:
          pComputer->wProcessor = _NECV20;
          pszString = pszNECV20;
          break;

        default:
          pComputer->wProcessor = 0;
          pszString = "";
     }

    /* Set the string in the structure */
    strcpy (pComputer->szProcessor, pszString);


    /* Set co-processor type */

    switch (wCoProcessor)
      {
        case 0:
          pComputer->wCoProcessor = _NOCOPROCESSOR;
          pszString = pszNone;
          break;

        case 1:
          pComputer->wCoProcessor = _8087;
          pszString = pszIntel8087;
          break;

        case 2:
          pComputer->wCoProcessor = _80287;
          pszString = pszIntel80287;
          break;

        case 3:
          if (wProcessor == 48)
            {
              pszString = pszInternal;
              pComputer->wCoProcessor = _INTERNAL;
            }
          else
            {
              pszString = pszIntel80387;
              pComputer->wCoProcessor = _80387;
            }

          break;

        default:
          pComputer->wCoProcessor = _NOCOPROCESSOR;
          pszString = pszUnknown;
      }

    /* Set the string in the structure */
    strcpy (pComputer->szCoProcessor, pszString);
  }

  return (FALSE);
}


/*********************************************************************
 * SprintComputerInfo - Put "Computer" information into a set of
 *                      strings to be printed or displayed.
 *
 * pComputer    - Pointer to computer information structure.
 * szSumStrings - Strings to store the summary information.
 *
 * Returns:  NULL if an error occured, or if summary information
 *           obtained.
 *********************************************************************/

QSZ * SprintComputerInfo (COMPUTER_STRUCT *pComputer,
                          CHAR szSumStrings[][MAX_SUMM_INFO + 5])
{
  WORD wNmbrStrings;        /* Number of strings                     */
  WORD wNmbrChars;          /* Number of characters in the strings   */
  WORD wIndex;              /* Index to the structure of TSR data    */
  WORD i;                   /* Looping variable                      */
  QSZ  *pqszStrings = NULL; /* Location for storing string pointers  */


  /* Set the summary info, if required */
  if (szSumStrings != NULL)
    {
      WORD wLength;   /* String length */


      /* Computer name */
      strncpy (szSumStrings[0], pComputer->szComputerName, MAX_SUMM_INFO);

      /* Remove trailing spaces */
      wLength = strlen (szSumStrings[0]);

      while (--wLength > 0 && szSumStrings[0][wLength] == ' ')
        szSumStrings[0][wLength] = '\0';

      /* Slash */
      strncat (szSumStrings[0], "/",
               MAX_SUMM_INFO - strlen (szSumStrings[0]));

      /* BIOS Manufacturer */
      strncat (szSumStrings[0], pComputer->szBiosMfgr,
               MAX_SUMM_INFO - strlen (szSumStrings[0]));

      /* Processor and Co-Processor */
      strncpy (szSumStrings[1], pComputer->szProcessor, MAX_SUMM_INFO);
      if (pComputer->wCoProcessor != _NOCOPROCESSOR &&
          pComputer->wCoProcessor != _INTERNAL)
        {
          strncat (szSumStrings[1], "/",
                   MAX_SUMM_INFO - strlen (szSumStrings[1]));
          strncat (szSumStrings[1], pComputer->szCoProcessor,
                   MAX_SUMM_INFO - strlen (szSumStrings[1]));
        }

      return (NULL);
    }

  /* Overestimate the amount of space required for the strings */

  wNmbrStrings = 19;
  wNmbrChars   = sizeof (COMPUTER_STRUCT) +
                 (wNmbrStrings * MAX_COMPUTER_TITLE_LENGTH) + 50;


  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the information in place */

  for (i = 0, wIndex = 0; paszComputerTitles[wIndex] != NULL ; ++i, ++wIndex)
    {
      WORD wIndent;                         /* Amount to indent       */
      CHAR chBuffer[MAX_BIOS_VERSION_LEN];  /* Buffer for string data */
      PSZ  pszString = NULL;                /* String pointer         */


      /* Title for this line of data */

      wIndent = MAX_COMPUTER_TITLE_LENGTH -
                strlen (paszComputerTitles[wIndex]);

      Qmemset (pqszStrings[i], ' ', wIndent);

      Qstrcpy (&pqszStrings[i][wIndent], paszComputerTitles[wIndex]);


      /* Place the appropriate information on the line */

      switch (wIndex)
        {
          case COMP_NAME:
            pszString = pComputer->szComputerName;
            break;

          case COMP_BIOS_MFGR:
            pszString = pComputer->szBiosMfgr;
            break;

          case COMP_BIOS_VERSION_1:
            pszString = pComputer->aszBiosVersion[0];

            /* Skip blank BIOS version lines */
            if (pComputer->aszBiosVersion[1][0] == '\0')
              wIndex += 2;
            break;

          case COMP_BIOS_VERSION_2:
            pszString = pComputer->aszBiosVersion[1];

            /* Skip blank BIOS version lines */
            if (pComputer->aszBiosVersion[2][0] == '\0')
              ++wIndex;
            break;

          case COMP_BIOS_VERSION_3:
            pszString = pComputer->aszBiosVersion[2];
            break;

          case COMP_BIOS_CATEGORY:
            pszString = pComputer->szBiosCategory;
            break;

          case COMP_BIOS_ID_BYTES:
            sprintf (chBuffer, "%02X %02X %02X", pComputer->bComputerType,
                     pComputer->bSubModel, pComputer->bRevisionLevel);
            pszString = chBuffer;

            /* Determine if the submodel and revision level */
            /*   bytes are available                        */
            if (pComputer->fConfigCallSupported == FALSE)
              chBuffer[2] = '\0';
            break;

          case COMP_BIOS_DATE:
            pszString = pComputer->szBiosDate;
            break;

          case COMP_PROCESSOR:
            pszString = pComputer->szProcessor;
            break;

          case COMP_COPROCESSOR:
            pszString = pComputer->szCoProcessor;
            break;

          case COMP_KEYBOARD:
            pszString = pComputer->szKeyboardType;
            break;

          case COMP_BUS_TYPE:
            pszString = pComputer->szBusType;
            break;

          case COMP_DMA_CTLRER:
            pszString = (pComputer->fDmaChipPresent) ? pszYes : pszNo;
            break;

          case COMP_CASCADE_IRQ2:
            pszString = (pComputer->fCascadeIntLvl2) ? pszYes : pszNo;
            break;

          case COMP_BIOS_DATA_SEG:
            if (pComputer->wExtAreaSeg)
              {
                sprintf (chBuffer, "%04X %dk", pComputer->wExtAreaSeg,
                         pComputer->wExtAreaLen);
                pszString = chBuffer;
              }
            else
              pszString = pszNone;
            break;

          default:
            pszString = "";
        }

      /* Put the information on the line */
      Qstrcat (pqszStrings[i], pszString);

      /* Set the next pointer */
      PrepNextString (pqszStrings, i);
    }

  /* Set the last pointer to NULL */

  pqszStrings[i] = NULL;

  /* Return the pointer to ppszStrings */

  return (pqszStrings);
}


/*********************************************************************
 * GetRomName - Finds a name in the computer/video card's ROM.
 *
 * fSearchType - Video bios search, or computer name search.
 * ppszStrings - List of names to search for.
 *
 * Returns:  String pointer to computer name, and sets wStringNmbr to
 *           the string number in ppszStrings that was found.
 *********************************************************************/

WORD GetRomName (BOOL fSearchType, PSZ *ppszStrings)
{
  WORD i, u;                /* Looping Variables */
  ADDR_LENGTH *pal = NULL;  /* Address length structure to use */

  /* Addresses to search for computer information*/
  static ADDR_LENGTH alComputer[] =
    {
      { 0xF0000000, 256 },
      { 0xF0001000, 256 }, /* This was added for Zenith    */
      { 0xF00012A0,  32 }, /* This was added for Telex     */
      { 0xF0006000, 256 }, /* This was added for Headstart */
      { 0xF0008000, 256 },
      { 0xF0008D00, 256 }, /* This was added for DTK       */
      { 0xF000A140,  16 }, /* This was added for Mitac     */
      { 0xF000A440,  32 }, /* This was added for Intel     */
      { 0xF000AF70,  64 }, /* This was added for Acer      */
      { 0xF000B3F0, 256 }, /* This was added for Acer      */
      { 0xF000C000, 256 },
      { 0xF000D000, 256 },
      { 0xF000E000, 256 },
      { 0xF000E300, 128 }, /* This was added for Northgate */
      { 0, 0 }
    };

  /* Addresses to search for video information */

  static ADDR_LENGTH alVideoNames[] =
    {
      { 0xC0000000, 512 },
      { 0xC0003F90,  17 }, /* Added for Compaq EGA         */
      { 0xC0000400, 128 }, /* Added for Genoa Spectra      */
      { 0xC0007F00,  64 }, /* Added for Tandy              */
      { 0xC0005F00, 512 }, /* Compaq video cards           */
      { 0, 0 }
    };

  static ADDR_LENGTH alVideoModels[] =
    {
      { 0xC0000000, 512 },
      { 0xC0000400, 256 }, /* Added for Genoa Spectra      */
      { 0xC0000A00, 512 }, /* Added for Video Seven 1024i  */
      { 0xC0000C00, 256 }, /* Added for Genoa Super EGA    */
      { 0xC0003B00, 128 }, /* Added for Tecmar VGA/AD      */
      { 0xC0007A00, 512 }, /* Added for Video Seven FastWrite */
      { 0, 0 }
    };


  /* Choose the correct search areas */

  if (fSearchType == GET_COMPUTER_NAME  ||
      fSearchType == GET_BIOS_MFGR)
    pal = alComputer;
  else if (fSearchType == GET_VIDEO_NAME)
    pal = alVideoNames;
  else
    pal = alVideoModels;


  /* Search for matches */

  for (i = 1; ppszStrings[i] != NULL; ++i)
    for (u = 0; pal[u].wLength != 0; ++u)
      if (fbiInstr ((CHAR FAR *) pal[u].dwAddress, ppszStrings[i],
                    pal[u].wLength))
        return (i);

  return (0);
}


/*********************************************************************
 * GetRomVersion - Finds version numbers stored in ROM.
 *
 * Returns:  TRUE if a version was found, FALSE otherwise
 *********************************************************************/

BOOL GetRomVersion (CHAR pachVersionStrings[][MAX_BIOS_VERSION_LEN],
                    CHAR FAR *fpSearchArea,
                    WORD wSearchLength)
{
  BOOL fFoundFlag = TRUE;   /* Set to TRUE if the item was found        */
  WORD wStringSegment;      /* Segment of the string that was found     */
  WORD wStringOffset;       /* Offset of the string that was found      */
  CHAR FAR *fpChars = NULL; /* Far pointer to the string of characters  */
  CHAR FAR *fpMatch = NULL; /* Used to detect if a match occured        */
  CHAR chSearchChar = '.';  /* Character to search for                  */
  WORD i, u;                /* Looping variables                        */
  WORD iIndex = 0;          /* Index to pachVersionStrings              */
  WORD wRomLength;          /* Length of ROM string start to ver number */

                            /* Version number search strings            */
  static PSZ  paszSearchStrings[] =
    {
      "Ver",
      "Rev",
      "Rel",
      "v0",
      "v1",
      "v2",
      "v3",
      "v4",
      "v5",
      "v6",
      "v7",
      "v8",
      "v9",
      "v 0",
      "v 1",
      "v 2",
      "v 3",
      "v 4",
      "v 5",
      "v 6",
      "v 7",
      "v 8",
      "v 9",
      NULL
    };

  /* Clear out the strings */

  for (i = 0; i < MAX_BIOS_VERSION_STRINGS; ++i)
    pachVersionStrings[i][0] = '\0';


  /* Search for a period with a digit on either side */

  while (iIndex < MAX_BIOS_VERSION_STRINGS && fFoundFlag)
    {
      _asm
        {
          push  di                ; Save DI
          les   di,fpSearchArea   ; Point to search area
          mov   cx,wSearchLength  ; Search the entire distance
          mov   al,chSearchChar   ;   For a period

        ContinueSearch:
          repnz scasb             ; Search

          jnz   not_found         ; If we went to the end, drop out

          mov   ah,es:[di]        ; Is the next character a number
          cmp   ah,'9'            ; Is the next character a number
          jg    keep_looking      ; If > '9', keep looking

          cmp   ah,'0'
          jl    keep_looking      ; If < '0', keep looking

          mov   ah,es:[di-2]      ; Is the previous character a number
          cmp   ah,'9'
          jg    keep_looking      ; If > '9', keep looking

          cmp   ah,'0'
          jl    keep_looking      ; If < '0', keep looking

          jmp   was_found         ; Otherwise, we found it

        keep_looking:             ; This isn't a version/date, keep looking
          inc   di
          dec   cx
          jnz   ContinueSearch

        not_found:
          mov   fFoundFlag,FALSE  ; Set the flag saying we didn't find it
          jmp   done

        was_found:
          mov   fFoundFlag,TRUE   ; We found it
          mov   wSearchLength,cx  ; Set the variables to continue the search
          mov   wStringOffset,di
          mov   wStringSegment,es

        done:
          pop   di
        }

      if (iIndex < MAX_BIOS_VERSION_STRINGS && fFoundFlag)
        {
          /* Set the pointer for continuing the search */
          fpSearchArea = (CHAR FAR *)
                           ((DWORD) wStringSegment << 16) +
                           (DWORD) wStringOffset;

          /* Set the pointer for printing the string */
          fpChars      = fpSearchArea;
          wRomLength   = 0;

          /* Search for the beginning of the string */
          while (wRomLength < MAX_BIOS_VERSION_LEN - 8 &&
                 *fpChars >= ' ' && *fpChars <= 127 && *fpChars != '$')
            --fpChars, ++wRomLength;

          ++fpChars;


          /* Can one of the search strings be found */

          for (u = 0;
               paszSearchStrings[u] != NULL &&
               (fpMatch = fbiInstr (fpChars,
                                    paszSearchStrings[u],
                                    wRomLength)) == 0;
               ++u)
            ;


          /* If fpMatch is non-zero, we found a match */

          if (fpMatch)
            {
              /* Skip leading whitespace */
              for (; *fpChars == ' '; ++fpChars)
                ;

              /* Copy the string into the pachVersionStrings */
              for (i = 0; i < MAX_BIOS_VERSION_LEN - 1 &&
                   *fpChars >= ' ' && *fpChars <= 127 && *fpChars != '$';
                   ++i, ++fpChars)
                pachVersionStrings[iIndex][i] = *fpChars;

              pachVersionStrings[iIndex][i] = '\0';

              /* Bump the index to the version strings */
              ++iIndex;
            }
        }
    }

  return (TRUE);
}



/*********************************************************************
 * GetRomDate - Finds the most recent date in the computer/video
 *              card's ROM.  When GetRomDate encounters a date, it
 *              checks the previously found date to see if the new
 *              date is more recent.
 *
 * fpSearchArea  - Area to search for a date (ie., F000:0000).
 * wSearchLength - Length of search (ie., FFFFH).
 * pchDateString - Location to store the date.
 *
 * Returns:  TRUE if a date was found, FALSE otherwise
 *********************************************************************/

BOOL GetRomDate (CHAR FAR * fpSearchArea,
                 WORD wSearchLength,
                 CHAR * pchDateString)
{
  BOOL fFoundFlag = TRUE;  /* Set to TRUE if the item was found       */
  WORD wStringSegment = 0; /* Segment of the string that was found    */
  WORD wStringOffset  = 0; /* Offset of the string that was found     */
  CHAR FAR *fpChars = NULL;/* Far pointer to the string of characters */
  CHAR chSearchChar = '/'; /* Date separator character                */
  CHAR chPrevDate[MAX_BIOS_DATE];   /* First date found               */
  CHAR chCurrDate[MAX_BIOS_DATE];   /* Date currently being examined  */
  WORD i;                  /* Looping variable                        */
  WORD wLength;            /* Number of characters to move            */


  /* Clear out the previous date */
  memset (chPrevDate, '\0', MAX_BIOS_DATE);



  while (fFoundFlag)
    {
      {
        _asm
          {
            cld
            push  di                ; Save DI
            les   di,fpSearchArea   ; Point to search area
            mov   cx,wSearchLength  ; Search the entire distance
            mov   al,chSearchChar   ;   for the date separator

          ContinueSearch:
            repnz scasb             ; Search


            jnz   not_found         ; If we went to the end, drop out

            cmp   al,es:[di + 2]    ; Is a date separator 2 chars away
            jne   keep_looking      ; If not, keep looking

            mov   ah,es:[di]        ; Is the next character a number
            cmp   ah,'9'
            jg    keep_looking      ; If > '9', keep looking

            cmp   ah,'0'
            jl    keep_looking      ; If < '0', keep looking

            mov   ah,es:[di - 2]    ; Is the previous character a number
            cmp   ah,'9'
            jg    keep_looking      ; If > '9', keep looking

            cmp   ah,'0'
            jl    keep_looking      ; If < '0', keep looking

            jmp   was_found         ; Otherwise, we found it

          keep_looking:             ; This isn't a version/date, keep looking
; ----- This is added in case the BIOS ends with a '/'
            or    cx,cx             ; Are we at the end
            jz    not_found         ; If we are, drop out.
; -----
            inc   di
            dec   cx
            jnz   ContinueSearch

          not_found:
            mov   fFoundFlag,FALSE  ; Set the flag saying we didn't find it
            jmp   done

          was_found:
            mov   fFoundFlag,TRUE   ; We found it
            mov   wSearchLength,cx  ; Set the variables to continue the search
            mov   wStringOffset,di
            mov   wStringSegment,es

          done:
            pop   di
          }
        }


      if (fFoundFlag)
        {

          /* Set the pointer for continuing the search */
          fpSearchArea = (CHAR FAR *)
                           ((DWORD) wStringSegment << 16) +
                           (DWORD) wStringOffset;


          /* Set the pointer to the beginning of the date */

          fpChars      = fpSearchArea - 3;

          /* Copy the year into chCurrDate */
          chCurrDate[0] = fpChars[6];
          chCurrDate[1] = fpChars[7];
          chCurrDate[2] = chSearchChar; /* The 1st "/" for YY/MM/DD */


          /* Copy the month & day into chCurrDate              */
          /*   (Process properly if this is a one digit month) */

          if (*fpChars > '9' || *fpChars < '0')
            {
              ++fpChars;
              chCurrDate[3] = '0';
              i = 4;
              wLength = 4;
            }
          else
            {
              i = 3;
              wLength = 5;
            }

          _fmemcpy ((CHAR FAR *) &chCurrDate[i], fpChars, wLength);


          /* Compare the dates, to see which is more recent */
          if (memcmp (chPrevDate, chCurrDate, MAX_BIOS_DATE - 1) < 0)
            memcpy (chPrevDate, chCurrDate, MAX_BIOS_DATE - 1);
        }
    }


  /* If we did not find a date */

  if (chPrevDate[0] == '\0')
    {
      pchDateString[0] = '\0';
      return (FALSE);
    }


  /* Put the date from chPrevDate's YY/MM/DD format */
  /*   into pchDateString's MM/DD/YY format         */

  pchDateString[5] = chSearchChar;
  pchDateString[6] = chPrevDate[0];
  pchDateString[7] = chPrevDate[1];

  memcpy (pchDateString, &chPrevDate[3], 5);

  pchDateString[8] = '\0';

  return (TRUE);
}


/*********************************************************************
 * fbiInstr - far, buffer, case-insensitive, INSTR function
 *
 * fpSearchArea      Far pointer to the area of memory to search
 *                        (ie, 0xF0000000 for ROM BIOS area)
 *
 * pszSearchString   String you are looking for in the distant
 *                        memory area pointed to by cfpAreaToSearch
 *                        (ie, "COPYRIGHT")
 *
 * wSearchLength     How much memory to search through
 *                        (ie, 0x8000 or 0xFFFF)
 *
 * Returns: Far pointer to the string if it was found, NULL if the
 *          string was not found.
 *********************************************************************/

CHAR FAR *fbiInstr (CHAR FAR *fpSearchArea,
                    PSZ      pszSearchString,
                    WORD     wSearchLength)
{
  BOOL fFoundFlag = TRUE;   /* Set to TRUE if the item was found    */
  CHAR chSearchChar1;       /* First character of search string     */
  CHAR chSearchChar2;       /* Second character of search string    */
  CHAR chSearchChar3;       /* Third character of search string     */
  CHAR chSearchChar4;       /* Fourth character of search string    */
  WORD wStringSegment;      /* Segment of the string that was found */
  WORD wStringOffset;       /* Offset of the string that was found  */
  WORD wLength;             /* Length of search string              */


  /* Set wLength to the length of the search string */
  wLength = strlen (pszSearchString);


  /* Get the first four characters to scan for */
  chSearchChar1 = pszSearchString[0];
  chSearchChar2 = pszSearchString[1];
  chSearchChar3 = pszSearchString[2];
  chSearchChar4 = pszSearchString[3];


  /* Search for the first two characters */

  while (fFoundFlag)
    {
      _asm
        {
          mov   cx,wSearchLength  ; Search the entire distance
          mov   bh,chSearchChar1  ;   for the first four characters
          mov   bl,chSearchChar2
          mov   dh,chSearchChar3
          mov   dl,chSearchChar4

          and   bx,0xDFDF         ; Fast "toupper()" all 4 characters
          and   dx,0xDFDF

          push  ds                ; Save DS
          push  si                ; Save SI
          lds   si,fpSearchArea   ; Point to search area

          push  si

        ContinuePop:              ; For continuing searches
          pop   si

        ContinueSearch:
          lodsb                   ; Search
          and   al,0xDF           ; Fast "toupper()"
          cmp   bh,al             ; Is it a match?

          loopne  ContinueSearch  ; Loop if not found

          push  si                ; Save SI, in case this didn't match.
                                  ;   (DS remains the same, so preserving
                                  ;   it is not necessary).

          and   cx,cx             ; Is CX == 0?
          jz    not_found         ; If so, we're done

          and   bl,bl             ; Is the 2nd character a NULL?
          jz    was_found         ; If so, we found it

          lodsb                   ; Otherwise, check the 2nd character

          and   al,0xDF           ; Fast "toupper()"
          cmp   bl,al             ; Is it a match?

          loopne  ContinuePop     ; Loop if not found

          and   cx,cx             ; Is CX == 0 (end of search length)?
          jz    not_found         ; We're done (no match)

          and   dh,dh             ; Is the 3rd character a NULL?
          jz    was_found         ; If so, we found it

          lodsb                   ; Otherwise, check the 3rd character

          and   al,0xDF           ; Fast "toupper()"
          cmp   dh,al             ; Is it a match?

          loopne  ContinuePop     ; Loop if not found

          and   cx,cx             ; Is CX == 0 (end of search length)?
          jz    not_found         ; We're done (no match)

          and   dl,dl             ; Is the 4th character a NULL?
          jz    was_found         ; If so, we found it

          lodsb                   ; Otherwise, check the 4th character

          and   al,0xDF           ; Fast "toupper()"
          cmp   dl,al             ; Is it a match?

          loopne  ContinuePop     ; Loop if not found

          and   cx,cx             ; Is CX == 0 (end of search length)?
          jz    not_found         ; We're done (no match)

          jmp   was_found         ; Otherwise, we have a match

        not_found:
          pop   ax                ; Remove the SI from the stack
          mov   fFoundFlag,FALSE  ; Set the flag saying we didn't find it
          jmp   done

        was_found:                ; Adjust SI to point
          pop   si                ;   directly at the string
          dec   si

          mov   fFoundFlag,TRUE   ; We found it
          mov   wSearchLength,cx  ; Set the variables to continue the search
          mov   wStringOffset,si
          mov   wStringSegment,ds

        done:
          pop   di
          pop   ds
        }

      if (fFoundFlag)
        {

          /* Set the pointer for continuing the search */
          fpSearchArea = (CHAR FAR *)
                           ((DWORD) wStringSegment << 16) +
                           (DWORD) wStringOffset;

          /* If the strings fully match, return the address of the string */
          if (!(_fmemicmp (fpSearchArea, (CHAR FAR *) pszSearchString,
                          wLength)))
            return (fpSearchArea);
          else
            /* Otherwise, keep searching */
            fpSearchArea = (wLength < 4) ? fpSearchArea + wLength :
                           fpSearchArea + 4;
        }
      else
        return (NULL);
    }
}


/*********************************************************************
 * fbMemString - far, buffer, string locator.  Looks forward and
 *               backwards for the first non-printing ASCII characters
 *               to locate the beginning and ending of a string in
 *               RAM.  Skips white space at the beginning and end of
 *               the string.
 *
 * pszfString1   Far pointer to the area of memory containing the
 *               string.
 *
 * pwLength      Changed to reflect the length of the string.
 *
 * returns CHAR FAR * to beginning of string, and pwLength is changed
 *         to reflect the length of the string.
 *********************************************************************/

CHAR FAR *fbMemString (register CHAR FAR *pszfString,
                       register WORD *pwLength)
{
  int iNewLineCount = 0;    /* Counts the number of newline characters */
  int iCountSinceNewLine = 0; /* Number of characters since the last newline */
  int iCharCount    = 0;    /* Counts the number of characters preceeding */
                            /*   the found string */

  /* Search backwards to beginning of string or 192 bytes */
  /*   or three lines, whichever is less                  */
  while (++iCharCount < 192 && iNewLineCount <= 3 && 
         ((*pszfString >= ' ' && *pszfString <= 126 &&
           *pszfString != '$' && *pszfString != '@') ||
           *pszfString == '\r' || *pszfString == '\n' ||
           *pszfString == '\t'))
    {
      if (*pszfString == '\r' || iCountSinceNewLine > REPORT_WIDTH - 10)
        {
          ++iNewLineCount;
          iCountSinceNewLine = 0;
        }
      else
        ++iCountSinceNewLine;

      --pszfString;
    }

  ++pszfString;
  iNewLineCount = 0;

  /* Skip over whitespace at the beginning of the string */
  while (*pszfString == ' ' || *pszfString == '\r' ||
         *pszfString == '\n' || *pszfString == '\t')
    ++pszfString;

  *pwLength = 0;
  iCountSinceNewLine = 0;

  /* Search forward to end of readable text */
  while ((pszfString[*pwLength] >= ' ' && pszfString[*pwLength] <= 126 &&
          pszfString[*pwLength] != '$' && pszfString[*pwLength] != '@') ||
          pszfString[*pwLength] == '\r' || pszfString[*pwLength] == '\n' ||
          pszfString[*pwLength] == '\t')
    {
      if (pszfString[*pwLength] == '\r')
        if (++iNewLineCount > 4)
          break;
        else
          iCountSinceNewLine = 0;


      if (++iCountSinceNewLine > REPORT_WIDTH - 10)
        if (++iNewLineCount > 4)
          break;
        else
          iCountSinceNewLine = 0;

      ++(*pwLength);
    }

  /* Search backward to first printable character which will */
  /*   skip over whitespace at the end of the string */
  while (pszfString[*pwLength] <= ' ' || pszfString[*pwLength] > 127 ||
         pszfString[*pwLength] == '$' || pszfString[*pwLength] == '@')
    --(*pwLength);

  *pwLength = (++(*pwLength)> 240) ? 240 : *pwLength;

  return (pszfString);
}


/**********************************************************************
 * GetBiosCageory - Uses bComputerType, bSubModel, and bRevisionLevel
 *                           to determine the computer descriptions.
 *
 * pComputer - Computer information structure.
 *
 * Returns:  VOID
 **********************************************************************/

#define UNKNOWN_COMPUTER  paszCommonStrings[0]
#define IBM_PC            paszCommonStrings[1]
#define IBM_PCJR          paszCommonStrings[2]
#define IBM_CONVERTIBLE   paszCommonStrings[3]
#define IBM_XT            paszCommonStrings[4]
#define IBM_XT_286        paszCommonStrings[5]
#define IBM_AT            paszCommonStrings[6]
#define IBM_PS2_25        paszCommonStrings[7]
#define IBM_PS2_25_30     paszCommonStrings[8]
#define IBM_PS2_30        paszCommonStrings[9]
#define IBM_PS2_30_286    paszCommonStrings[10]
#define IBM_PS2_50        paszCommonStrings[11]
#define IBM_PS2_50Z       paszCommonStrings[12]
#define IBM_PS2_55SX      paszCommonStrings[13]
#define IBM_PS2_60        paszCommonStrings[14]
#define IBM_PS2_70        paszCommonStrings[15]
#define IBM_PS2_P70       paszCommonStrings[16]
#define IBM_PS2_70_80     paszCommonStrings[17]
#define IBM_PS2_80        paszCommonStrings[18]
#define IBM_PS2_90        paszCommonStrings[19]
#define IBM_PS1           paszCommonStrings[20]
#define IBM_PS2_65SX      paszCommonStrings[21]

static char *paszCommonStrings[] =
  {
    "Not Determined",
    "IBM PC",
    "IBM PCjr",
    "IBM PC Convertible",
    "IBM PC/XT",
    "IBM PC/XT 286",
    "IBM PC/AT",
    "IBM PS/2 Model 25",
    "IBM PS/2 Model 25/30",
    "IBM PS/2 Model 30 286",
    "IBM PS/2 Model 30",
    "IBM PS/2 Model 50",
    "IBM PS/2 Model 50Z",
    "IBM PS/2 Model 55SX",
    "IBM PS/2 Model 60",
    "IBM PS/2 Model 70",
    "IBM PS/2 Model P70",
    "IBM PS/2 Model 70/80",
    "IBM PS/2 Model 80",
    "IBM PS/2 Model 90",
    "IBM PS/1",
    "IBM PS/2 Model 65SX"
  };

VOID GetBiosCategory (COMPUTER_STRUCT *pComputer)
{
  CHAR chBuffer[80];          /* Buffer for concatination of strings */
  PSZ pszString = chBuffer;   /* String pointer.  Pointing pszString */
                              /*   to chBuffer allows easy           */
                              /*   concatination of strings          */

  switch (pComputer->bComputerType)
    {
      case 0xff:
        {
          switch (pComputer->bRevisionLevel)
            {
              case 00:
              case 01:
                pszString = IBM_PC;
                break;

              case 02:
                pszString = IBM_XT;
                break;

              default:
                pszString = IBM_PC;
                break;
            }
          break;
        }

      case 0xfe:
        pszString = IBM_XT;
        break;

      case 0xfd:
        pszString = IBM_PCJR;
        break;

      case 0xfc:
        {
          switch (pComputer->bSubModel)
            {
              case 00:
              case 01:
                pszString = IBM_AT;
                break;

              case 02:
                pszString = IBM_XT_286;
                break;

              case 03:
                pszString = IBM_AT;
                break;

              case 04:
                {
                  switch (pComputer->bRevisionLevel)
                    {
                      case 00:
                        pszString = IBM_PS2_50;
                        break;

                      case 03:
                        pszString = IBM_PS2_50Z;
                        break;

                      default:
                        pszString = IBM_PS2_50;
                        break;
                    }
                  break;
                }

              case 05:
                pszString = IBM_PS2_60;
                break;

              case 9:
                pszString = IBM_PS2_30_286;
                break;

              case 0x0B:
                pszString = IBM_PS1;
                break;

              case 0x81:
                pszString = "Phoenix PC/AT Compatible BIOS";
                break;

              default:
                pszString = IBM_AT;
                break;
            }
          break;
        }

      case 0xfb:
        pszString = IBM_XT;
        break;

      case 0xfa:
        {
          switch (pComputer->bSubModel)
            {
              case 00:
                pszString = IBM_PS2_30;
                break;

              case 01:
                pszString = IBM_PS2_25;
                break;

              default:
                pszString = IBM_PS2_30;
                break;
            }
          break;
        }

      case 0xf9:
        pszString = IBM_CONVERTIBLE;
        break;

      case 0xf8:
        {
          switch (pComputer->bSubModel)
            {
              case 0:
              case 1:
                pszString = IBM_PS2_80;
                break;

              case 4:
              case 9:
              case 0x0A:
                pszString = IBM_PS2_70;
                break;

              case 0x0C:
                pszString = IBM_PS2_55SX;
                break;

              case 0x0D:
                pszString = IBM_PS2_70;
                break;

              case 0x14:
              case 0x16:
                pszString = IBM_PS2_90;
                break;

              case 0x1C:
                pszString = IBM_PS2_65SX;
                break;

              case 0x0B:
              case 0x50:
                pszString = IBM_PS2_P70;
                break;

              default:
                pszString = IBM_PS2_70_80;
                break;
            }
          break;
        }

      default:
        pszString = UNKNOWN_COMPUTER;
    }

  /* Set the value in the structure */

  strcpy (pComputer->szBiosCategory, pszString);
}
