/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * VIDEO.C - Source file for determing video adapter information.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/* Video Display Types */

PSZ  ppszDisplayName[] =
  {
    "",
    "TTL Monochrome",
    "CGA Monitor",
    "EGA Monitor",
    "VGA Monochrome",
    "VGA Color",
    "8514/A Display"
  };


/* Adapter Types (IBM) */

PSZ  ppszIbmName[] =
  {
    "None",
    "MDA",
    "CGA",
    "EGA",
    "MCGA",
    "VGA",
    "8514/A",
    "XGA",
    "TIGA"
  };

#define VIDEO_TYPE_CGA  2
#define VIDEO_TYPE_TIGA 8


/* Adapter Types (Hercules) */

PSZ  ppszHercName[] =
  {
    "Hercules or Compatible",
    "Hercules Graphics Card +",
    "Hercules InColor"
  };


/* List of Adapter Names */

PSZ  ppszAdapterNames[] =
  {
    "Unknown",
    "Hercules", "Cirrus", "Zenith", "Quadram", "Radius", "Mitsubishi",
    "Emerson", "Ogivar", "Tandy", "Olivetti", "US Video", "Persyst",
    "ALR ", "ATI ", "BTC ", "Boca", "Cardinal", "Chicony", "CompuAdd",
    "DFI ", "DTK ", "Focus", "GBM ", "Hewlett-Packard", "Hewlett Packard",
    "Leading Tech", "Lava", "Logos", "LSI ", "Matrox", "NEC ", "Orchid",
    "Packard Bell", "PC Brand", "PC Craft", "PC Tech", "Prism", "QDI",
    "RasterOps", "Sigma", "STB ", "Swan", "ViewSonic", "Wyse", "Wang",
    "Dieco", "Metheus", "Monolithic", "Packard Bell", "Bell",
    "Colorgraphics", "AST ",

    /* Chipset makers */
    "Ahead", "Appian", "Oak ", "Sigma", "Video Seven", "Video-7", "Genoa",
    "Chips and Tech", "Tseng", "Tecmar", "Paradise", "NEC ", "ATI ",
    "Everex", "Phoenix", "Award", "Compaq", "Quadtel", "Headland",
    "Trident", "Western Digital", "MaxLogic", "Diamond", "AMI ", "PCG ",
    "Tatung", "Amdek", "AST ",
    NULL
  };



#define VID_NAME_HERCULES   1


/* List of Adapter Models */

PSZ  ppszAdapterModels[] =
  {
    "",
    "FastWrite", "V-RAM", "Z449", "Z549", "Workstation", "QuadVGA",
    "Wizard", "Enhancer", "Integra", "VGAudio", "VideoFlex", "SpeedSTAR",
    "Viewpoint TC", "Viewpoint", "Vision", "2theMax", "Mirage",
    "Splitword", "Graphics Station", "Ultra", "MaxVGA", "Ultimate",
    "UltraVGA", "MicroPAQ", "ProDesigner IIs", "ProDesigner II", "Photon",
    "Plus", "Supra", "Basic", "Elite", "Legend", "PowerGraph",
    "PowerView", "Spectrum", "VGA Professional", "Paradise",
    "VRAM II", "VRAM", "Hi-Rez",
    "Super VGA+", "SuperVGA", "Super VGA", "1024i", "Vega VGA", "VEGA",
    "OVGA", "PVGA", "RVGA", "SVGA", "TVGA", "EGA2 ", "VGA2 ",
    "VGA+", "SVGA", "8514/A", "8514A", "8514", "DVGA ", "MVGA",
    "VGA III", "VGA II", "MEGA", "PEGA", "SEGA", "VEGA Deluxe",
    "VEGA",
    NULL
  };


/*********************************************************************
 * GetVideoInfo - Gets the video adapter information.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetVideoInfo (VIDEO_STRUCT *pVideo, BOOL fMinimumInfo)
{
  union  REGS inregs, outregs;  /* Register structures for int86()   */
  struct SREGS sregs;           /* Segment register structure        */
  WORD   i;                     /* Local WORD variable               */


  {
    /* Get Adapter Type */

    /* Detect video subsystems using videoid.asm. After this call    */
    /*   the fields Subsystem and Display in the structure pointed   */
    /*   to by pVideo are set to reflect the video type of a primary */
    /*   and secondary video adapter                                 */

    /* VIDEOID.ASM from Programmer's Library, page 511 of   */
    /*   Programmer's Guide to PC and PS/2 Video Systems by */
    /*   Richard Wilton                                     */

    VideoID ((VIDEO_STRUCT FAR *) pVideo);

    /* Put the higher end display first */
    if (pVideo->bSubsystem0 < 80 && pVideo->bSubsystem1 < 80)
      if (pVideo->bSubsystem0 < pVideo->bSubsystem1)
        {
          BYTE bHold;   /* Hold value for swapping */

          bHold               = pVideo->bSubsystem0;
          pVideo->bSubsystem0 = pVideo->bSubsystem1;
          pVideo->bSubsystem1 = bHold;
        }


    /* TIGA detection */
    GetTigaInfo (pVideo);

    strcpy (pVideo->szAdapterType, SubsystemName(pVideo->bSubsystem0));
  }

  {
    /* Get Adapter Name */

    i = GetRomName (GET_VIDEO_NAME, ppszAdapterNames);
    strcpy (pVideo->szAdapterName, ppszAdapterNames[i]);
    pVideo->wAdapterName = i;

    /* If this was a Hercules Graphics Card + or a Hercules InColor card */
    /*   set the Manufacturer Name to "Hercules"                         */

    if (i == 0 && (pVideo->bSubsystem0 == 0x81 ||
        pVideo->bSubsystem0 == 0x82))
      {
        strcpy (pVideo->szAdapterName, ppszAdapterNames[VID_NAME_HERCULES]);
        pVideo->wAdapterName = VID_NAME_HERCULES;
      }
  }

  {
    /* Get Adapter Model */

    i = GetRomName (GET_VIDEO_MODEL, ppszAdapterModels);
    strcpy (pVideo->szAdapterModel, ppszAdapterModels[i]);
    pVideo->wAdapterModel = i;
  }

  {
    /* Get VESA version information */

    BYTE  bBuffer[256];          /* 256 byte buffer for VESA information */
    BYTE  FAR *fbByte = NULL;    /* Pointer to the BYTE buffer           */
    CHAR  FAR *fpChar = NULL;    /* Far CHAR pointer (to get OEM name)   */
    DWORD FAR *fpdwDword = NULL; /* Far DWORD pointer (to get OEM name)  */


    fbByte = (BYTE FAR *) bBuffer;

    /* Clear out the VESA OEM Name */

    pVideo->szVesaOemName[0] = '\0';


    /* Call the VESA "Get Information" call */

    inregs.x.ax = 0x4F00;
    sregs.es    = FP_SEG (fbByte);
    inregs.x.di = FP_OFF (fbByte);

    int86x (0x10, &inregs, &outregs, &sregs);

    /* Determine if this card is a VESA card */

    if (memcmp ((CHAR *) bBuffer, "VESA", 4) == 0)
      {
        pVideo->bVesaVersionMajor = bBuffer[5];
        pVideo->bVesaVersionMinor = bBuffer[4];

        /* Point to the OEM name */
        fpdwDword = (DWORD FAR *) &bBuffer[6];
        fpChar    = (CHAR FAR *) *fpdwDword;

        /* Copy the OEM name into the structure */
        if (fpChar != NULL)
          {
            for (i = 0; i < MAX_VESA_OEM_NAME - 1 &&
                        fpChar[i] >= ' ' && fpChar[i] <= 127; ++i)
              pVideo->szVesaOemName[i] = fpChar[i];

            pVideo->szVesaOemName[i] = '\0';
          }
      }
    else
      {
        pVideo->bVesaVersionMajor = 0;
        pVideo->bVesaVersionMinor = 0;
      }
  }

  {
    /* If mimimum information requested, return now */

    if (fMinimumInfo)
      return (FALSE);
  }

  {
    /* Get the display type */

    strcpy (pVideo->szDisplayType, DisplayName(pVideo->bDisplay0));
  }

  {
    /* Get the secondary adapter type */

    strcpy (pVideo->sz2ndAdapterType, SubsystemName(pVideo->bSubsystem1));
  }

  {
    /* The following interrupt gets the display mode in AL, the     */
    /*   number of columns in AH, and the active video display page */
    /*   in BH as mentioned on pg 196 of the PC SourceBook -- for   */
    /*   ALL monitor types                                          */

    inregs.h.ah=0x0F;
    int86(0x10,&inregs,&outregs);
    pVideo->bMode0 = outregs.h.al;
    pVideo->bNmbrCols0 = outregs.h.ah;
  }

  {
    /* The following interrupt gets the active video display page as   */
    /*   mentioned on pg 525 of Advanced MSDOS -- for EGA and VGA ONLY */

    inregs.h.ah=0x12;
    inregs.h.bl=0x10;
    int86(0x10,&inregs,&outregs);
    pVideo->wMemory0 = outregs.h.bl;
  }

  {
    /* Set the the number of rows on the display */

    BYTE FAR * fbByte = NULL;  /* Far pointer to a byte */

    /* Point to the location of the number of rows on the display */
    fbByte = (BYTE FAR *) 0x00400084;

    if (pVideo->bSubsystem0 > VIDEO_TYPE_CGA && pVideo->bSubsystem0 < 0x80)
      pVideo->bNmbrRows = fbByte[0] + (BYTE) 1;
    else
      pVideo->bNmbrRows = 25;
  }

  {
    /* Get the Video BIOS Date and Version, if it exists */

    BYTE FAR * fbRomSignature = NULL;  /* Location of the ROM Signature */


    /* Set the location for the video card's ROM BIOS */

    fbRomSignature = (BYTE FAR *) 0xC0000000;


    /* 55H AAH are the ROM signature bytes, followed by the ROM */
    /*   length / 512                                           */

    if (fbRomSignature[0] != 0x55 || fbRomSignature[1] != 0xAA)
      {
        fbRomSignature = (BYTE FAR *) 0xE0000000;

        if (fbRomSignature[0] != 0x55 && fbRomSignature[1] != 0xAA)
          fbRomSignature = (BYTE FAR *) 0x00000000;
      }

    if (fbRomSignature)
      {
        /* Get Video ROM Date */

        GetRomDate (fbRomSignature, ((WORD) fbRomSignature[2]) << 9,
                    pVideo->szVideoBiosDate);

        /* Get Video ROM Version */

        GetRomVersion (pVideo->aszVideoBiosVersion,
                       fbRomSignature, ((WORD) fbRomSignature[2]) << 9);
      }
    else
      {
        /* Clear out the BIOS date and version strings */

        pVideo->szVideoBiosDate[0] = '\0';

        for (i = 0; i < MAX_BIOS_VERSION_STRINGS; ++i)
          pVideo->aszVideoBiosVersion[i][0] = '\0';
      }
  }

  return (FALSE);
}


/*********************************************************************
 * SprintVideoInfo - Put video information into a set of strings to be
 *                   printed or displayed.
 *
 * pVideo       - Pointer to video information structure.
 * szSumStrings - Summary string area.
 *
 * Returns:  NULL if an error occured.
 *********************************************************************/

QSZ * SprintVideoInfo (VIDEO_STRUCT *pVideo,
                       CHAR szSumStrings[][MAX_SUMM_INFO + 5])
{
  WORD wNmbrStrings;        /* Number of strings                     */
  WORD wNmbrChars;          /* Number of characters in the strings   */
  WORD wIndex;              /* Index to the structure of TSR data    */
  WORD i;                   /* Looping variable                      */
  QSZ  *pqszStrings = NULL; /* Location for storing string pointers  */


  /* Summary Strings */
  if (szSumStrings != NULL)
    {
      /* Adapter Type (CGA/EGA/VGA/etc) */
      strncpy (szSumStrings[0], pVideo->szAdapterType, MAX_SUMM_INFO);
      strncat (szSumStrings[0], pszCommaSpace,
               MAX_SUMM_INFO - strlen (szSumStrings[0]));

      /* Adapter Manufacturer */
      if (pVideo->szAdapterName[0] != '\0')
        strncat (szSumStrings[0], pVideo->szAdapterName,
                 MAX_SUMM_INFO - strlen (szSumStrings[0]));

      /* Adapter Model */
      strncpy (szSumStrings[1], pVideo->szAdapterModel, MAX_SUMM_INFO);

      return (NULL);
    }


  /* Overestimate the amount of space required for the strings */

  wNmbrStrings = 20;
  wNmbrChars   = sizeof (VIDEO_STRUCT) +
                 (wNmbrStrings * MAX_VIDEO_TITLE_LENGTH) + 50;


  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);



  /* Put the information in place */

  for (i = 0, wIndex = 0; paszVideoTitles[wIndex] != NULL ; ++i, ++wIndex)
    {
      WORD wIndent;                       /* Amount to indent       */
      CHAR chBuffer[MAX_VESA_OEM_NAME];   /* Buffer for string data */
      PSZ  pszString = NULL;              /* String pointer         */


      /* Title for this line of data */

      wIndent = MAX_VIDEO_TITLE_LENGTH -
                strlen (paszVideoTitles[wIndex]);

      Qmemset (pqszStrings[i], ' ', wIndent);

      Qstrcpy (&pqszStrings[i][wIndent], paszVideoTitles[wIndex]);


      /* Place the appropriate information on the line */

      switch (wIndex)
        {
          case VID_ADAPTER_TYPE:
            pszString = pVideo->szAdapterType;
            break;

          case VID_NAME:
            pszString = pVideo->szAdapterName;
            break;

          case VID_MODEL:
            pszString = pVideo->szAdapterModel;
            break;

          case VID_DISPLAY_TYPE:
            pszString = pVideo->szDisplayType;
            break;

          case VID_MODE:
            sprintf (chBuffer, "%d", pVideo->bMode0);
            pszString = chBuffer;
            break;

          case VID_NMBR_COLUMNS:
            sprintf (chBuffer, "%d", pVideo->bNmbrCols0);
            pszString = chBuffer;
            break;

          case VID_NMBR_ROWS:
            sprintf (chBuffer, "%d", pVideo->bNmbrRows);
            pszString = chBuffer;
            break;

          case VID_BIOS_VERSION_1:
            pszString = pVideo->aszVideoBiosVersion[0];

            /* Skip blank BIOS version lines */
            if (pVideo->aszVideoBiosVersion[1][0] == '\0')
              wIndex += 2;
            break;

          case VID_BIOS_VERSION_2:
            pszString = pVideo->aszVideoBiosVersion[1];

            /* Skip blank BIOS version lines */
            if (pVideo->aszVideoBiosVersion[2][0] == '\0')
              ++wIndex;
            break;

          case VID_BIOS_VERSION_3:
            pszString = pVideo->aszVideoBiosVersion[2];
            break;

          case VID_BIOS_DATE:
            pszString = pVideo->szVideoBiosDate;
            break;

          case VID_VESA_COMPAT:
            if (pVideo->bVesaVersionMajor == 0 &&
                pVideo->bVesaVersionMinor == 0)
              {
                pszString = pszNo;
                wIndex += 2;
              }
            else
              pszString = pszYes;
            break;

          case VID_VESA_VERSION:
            sprintf (chBuffer, "%X.%02X", pVideo->bVesaVersionMajor,
                     pVideo->bVesaVersionMinor);
            pszString = chBuffer;
            break;

          case VID_VESA_OEM:
            pszString = pVideo->szVesaOemName;
            break;

          case VID_2NDARY_ADAPTER:
            pszString = pVideo->sz2ndAdapterType;
            if (pVideo->wTigaInterrupt == 0)
              wIndex += 2;
            break;

          case VID_TIGA_VERSION:
            sprintf (chBuffer, "%d.%02d", pVideo->wTigaMajor,
                     pVideo->wTigaMinor);
            pszString = chBuffer;
            break;

          case VID_TIGA_INT:
            sprintf (chBuffer, "%XH / %04X:%04X ", pVideo->wTigaInterrupt,
                     FP_SEG (pVideo->dwTigaIntAddress),
                     FP_OFF (pVideo->dwTigaIntAddress));

            /* Mention if the signature was found */
            if (pVideo->fTigaSignatureFound == TRUE)
              strcat (chBuffer, "TIGA Signature Found");
            else if (pVideo->fTigaSignatureFound == FALSE)
              strcat (chBuffer, "TIGA Signature Not Found");

            pszString = chBuffer;
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

  /* Return the pointer to pqszStrings */

  return (pqszStrings);
}


/*********************************************************************
 * SubsystemName - This function returns the type of video adapter.
 *********************************************************************/

PSZ  SubsystemName (BYTE bType)
{
  if (bType & 0x80)
    return (ppszHercName[bType & 0x7F]);
  else
    return (ppszIbmName[bType]);
}


/*********************************************************************
 * DisplayType - This function returns the type of display.
 *********************************************************************/

PSZ  DisplayName (BYTE bType)
{
  return (ppszDisplayName[bType]);
}


/*********************************************************************
 * GetTigaInfo - Determines the TIGA driver information.
 *
 * pVideo - Video information structure
 *
 * Returns:  TRUE if the TIGA communications driver (CD) is found.
 *********************************************************************/

BOOL GetTigaInfo (VIDEO_STRUCT *pVideo)
{
  PSZ  pszTigaEnvString;    /* Pointer to TIGA environment variable */
  PSZ  pszIntIdent;         /* Interrupt number identifier          */
  WORD wTigaIntNumber;      /* TIGA CD interrupt number             */
  union REGS regs;          /* Registers for int86 call             */
  struct SREGS sregs;       /* Segment regs for int86x     */
  CHAR FAR *fpszSignature;  /* pointer to signature "TIGA" */


  /* Get the TIGA environment string */
  pszTigaEnvString = getenv ("TIGA");
  if (pszTigaEnvString == NULL)
    return (FALSE);


  /* Find the interrupt number */
  pszIntIdent = strstr (pszTigaEnvString, " -i0");
  if (pszIntIdent != NULL)
    {
      /* Parse for the interrupt number */
      sscanf (&pszIntIdent[5], "%x", &wTigaIntNumber);
    }
  else
    wTigaIntNumber = 0x7F;

  /* Set the interrupt number in the struture */
  pVideo->wTigaInterrupt = wTigaIntNumber;


  /* Get the interrupt address */
  regs.h.ah = 0x35;
  regs.h.al = (BYTE) wTigaIntNumber;
  int86x (0x21, &regs, &regs, &sregs);
  pVideo->dwTigaIntAddress = ((DWORD) sregs.es << 16) + regs.x.bx;

  if (sregs.es == 0 && regs.x.bx == 0)
    return (FALSE);


  /* Determine if the TIGA CD is installed and operating */
  regs.x.ax = 0x4321;
  int86 (wTigaIntNumber, &regs, &regs);

  /* AX == 0 if TIGA CD is installed */
  if (regs.x.ax == 0)
    {
      /* Obtain the TIGA version */
      regs.h.ah = 1;
      int86 (wTigaIntNumber, &regs, &regs);

      if (regs.x.bx != 0x1234)
        {
          /* BX != 0x1234 means TIGA 1.1 */
          pVideo->wTigaMajor          = 1;
          pVideo->wTigaMinor          = 1;
          pVideo->fTigaSignatureFound = 0xFFFF;
        }
      else
        {
          /* Otherwise, the TIGA version number is in CH.CL (decimal) */
          pVideo->wTigaMajor = (WORD) regs.h.ch;
          pVideo->wTigaMinor = (WORD) regs.h.cl;
        }

      /* Look for the "TIGA\0" signature at the interrupt address. */
      /*   (Only valid for TIGA 2.2 and above)                     */
      if (pVideo->wTigaMajor * 100 + pVideo->wTigaMinor >= 220)
        {
          /* Point to the signature */
          fpszSignature = (CHAR FAR *) pVideo->dwTigaIntAddress + 2;
          if (_fmemcmp (fpszSignature, "TIGA", 5) == 0)
            pVideo->fTigaSignatureFound = TRUE;
          else
            pVideo->fTigaSignatureFound = FALSE;
        }
      else
        pVideo->fTigaSignatureFound = 0xFFFF;


      /* Set the highest class of display adapter into the */
      /*   Secondary Display field.                        */
      if ((WORD) (pVideo->bSubsystem1) ^ 0x180 <
          (WORD) (pVideo->bSubsystem0 ^ 0x180))
        pVideo->bSubsystem1 = pVideo->bSubsystem0;

      pVideo->bSubsystem0 = VIDEO_TYPE_TIGA;
    }
}
