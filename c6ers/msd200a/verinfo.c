/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * VERINFO.C - Source file for determining file versions.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/*********************************************************************
 * GetFileVersion - Gets the version information from a file.
 *
 * pszFilename  - Filename from which to get the version information.
 * fMinimumInfo - Gets minimum version information when TRUE.
 *
 * Returns:  Pointer to version information, NULL if an error occured.
 *********************************************************************/

BYTE * GetFileVersion (PSZ pszFilename, BOOL fMinimumInfo)
{
  WORD  wSize;        /* Size of the file's version info              */
  DWORD dwHandle;     /* Handle returned by GetFileVersionInfoSize    */
  BYTE * pVer;        /* File version info structure                  */
  BYTE * pVer32;      /* File version info structure (32 bit aligned) */
  BOOL fReturnValue;  /* Return value from GetFileVersionInfo         */


  /* Determine the size of the version info structure */
  wSize = (WORD) GetFileVersionInfoSize ((CHAR  FAR *) pszFilename,
                                         (DWORD FAR *) &dwHandle);
  if (wSize == 0)
    return (NULL);


  /* Allocate the memory required for the "structure" */
  pVer = malloc (wSize + 4);
  if (pVer == NULL)
    return (NULL);


  /* Align the pointer on 32 bits */
  pVer32 = DWORDUP (pVer);


  /* Get the version information */
  fReturnValue = GetFileVersionInfo ((CHAR FAR *) pszFilename, dwHandle,
                                     (DWORD) wSize, (BYTE FAR *) pVer32);


  /* Return the value */
  if (fReturnValue == FALSE)
    {
      free (pVer);
      return (NULL);
    }
  else
    return (pVer);
}


/*********************************************************************
 * SprintFileVersion - Put the version information into displayable
 *                     strings.
 *
 * pszFilename  - Filename from which to get the version information.
 * fMinimumInfo - Gets minimum version information when TRUE.
 *
 * Returns:  Pointer to array of strings, NULL if an error occured.
 *********************************************************************/

QSZ * SprintFileVersion (BYTE * pVer)
{
  QSZ  *pqszStrings;  /* Pointer to string pointer array          */
  WORD wMaxFree;      /* Maximum number of free bytes to allocate */
  WORD wNmbrStrings;  /* Number of string pointers available      */
  WORD wNmbrChars;    /* Number of character pointers available   */
  WORD wCharCount = 0;/* Total number of chars stored in memory   */
  WORD i = 0;         /* Looping variable                         */
  WORD wStringLen;    /* Length of a string                       */
  WORD * pWord;       /* WORD pointer                             */
  CHAR chBuffer[80];  /* Local character buffer                   */
  WORD wAlignColumn = 19; /* Alignment column                     */
  BYTE * pVer2 = pVer;    /* Original pointer value               */

  /* Values for the VS_VERSION_INFO "structure" */

  WORD wTotLength;          /* Length of VS_VERSION_BLOCK (w/out padding) */
  WORD wValLength;          /* Length of VS_FIXEDFILEINFO data            */
  VS_FIXEDFILEINFO *pValue; /* Pointer to VS_FIXEDFILEINFO struct         */


  /* Allocate memory for the strings */
  wMaxFree     = _memmax() - 2048;
  wNmbrStrings = wMaxFree / (32 + sizeof (PSZ)) + 1;
  wNmbrChars   = wMaxFree - (wNmbrStrings * sizeof (PSZ));
  wNmbrStrings = MAX_MSD_STRINGS + 1;
  wNmbrChars   = REPORT_WIDTH * (MAX_MSD_STRINGS + 1);

  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Interpret the version information */
  {
    /* Align pVer onto a 32 bit boundary */
    pVer = DWORDUP (pVer);

    /* Get wTotLength and wValLength */
    pWord = (WORD *) pVer;
    wTotLength = *pWord;
    wValLength = *(++pWord);

    /* Key */
    QstrcpyAlign (pqszStrings[i], "Key: ", wAlignColumn);
    Qstrcat (pqszStrings[i], &pVer[4]);
    PrepNextString (pqszStrings, i);
    wCharCount += Qstrlen (pqszStrings[i++]) + 1;


    /* Find VS_FIXEDFILEINFO data */
    wStringLen = strlen (&pVer[4]);
    pVer = &pVer[4 + wStringLen + 1];
    pVer = DWORDUP (pVer);
    pValue = (VS_FIXEDFILEINFO *) pVer;


    /* Interpret the VS_FIXEDFILEINFO data */

    /* Signature */
    QstrcpyAlign (pqszStrings[i], "Signature: ", wAlignColumn);
    sprintf (chBuffer, "%04X%04X",
             FP_SEG (pValue->dwSignature),
             FP_OFF (pValue->dwSignature));
    Qstrcat (pqszStrings[i], chBuffer);
    PrepNextString (pqszStrings, i);
    wCharCount += Qstrlen (pqszStrings[i++]) + 1;


    /* Structure version */
    QstrcpyAlign (pqszStrings[i], "Structure Version: ", wAlignColumn);
    sprintf (chBuffer, "%u.%u",
             FP_SEG (pValue->dwStrucVersion),
             FP_OFF (pValue->dwStrucVersion));
    Qstrcat (pqszStrings[i], chBuffer);
    PrepNextString (pqszStrings, i);
    wCharCount += Qstrlen (pqszStrings[i++]) + 1;


    /* File Version */
    QstrcpyAlign (pqszStrings[i], "File Version: ", wAlignColumn);
    sprintf (chBuffer, "%u.%u.%u.%u",
             FP_SEG (pValue->dwFileVersionMS),
             FP_OFF (pValue->dwFileVersionMS),
             FP_SEG (pValue->dwFileVersionLS),
             FP_OFF (pValue->dwFileVersionLS));
    Qstrcat (pqszStrings[i], chBuffer);
    PrepNextString (pqszStrings, i);
    wCharCount += Qstrlen (pqszStrings[i++]) + 1;


    /* Product Version */
    QstrcpyAlign (pqszStrings[i], "Product Version: ", wAlignColumn);
    sprintf (chBuffer, "%u.%u.%u.%u",
             FP_SEG (pValue->dwProductVersionMS),
             FP_OFF (pValue->dwProductVersionMS),
             FP_SEG (pValue->dwProductVersionLS),
             FP_OFF (pValue->dwProductVersionLS));
    Qstrcat (pqszStrings[i], chBuffer);
    PrepNextString (pqszStrings, i);
    wCharCount += Qstrlen (pqszStrings[i++]) + 1;


    /* File Flags */
    QstrcpyAlign (pqszStrings[i], "File Flags: ", wAlignColumn);

    if (VS_FF_DEBUG & pValue->dwFileFlagsMask & pValue->dwFileFlags)
      Qstrcat (pqszStrings[i], "Debug ");

    if (VS_FF_INFOINFERRED & pValue->dwFileFlagsMask & pValue->dwFileFlags)
      Qstrcat (pqszStrings[i], "Inferred ");

    if (VS_FF_PATCHED & pValue->dwFileFlagsMask & pValue->dwFileFlags)
      Qstrcat (pqszStrings[i], "Patched ");

    if (VS_FF_PRERELEASE & pValue->dwFileFlagsMask & pValue->dwFileFlags)
      Qstrcat (pqszStrings[i], "Prerelease ");

    if (VS_FF_PRIVATEBUILD & pValue->dwFileFlagsMask & pValue->dwFileFlags)
      Qstrcat (pqszStrings[i], "Private-Build ");

    if (VS_FF_SPECIALBUILD & pValue->dwFileFlagsMask & pValue->dwFileFlags)
      Qstrcat (pqszStrings[i], "Special-Build ");

    PrepNextString (pqszStrings, i);
    wCharCount += Qstrlen (pqszStrings[i++]) + 1;


    /* File Operating System */
    QstrcpyAlign (pqszStrings[i], "File Operating System: ", wAlignColumn);

    if ((VOS_UNKNOWN & pValue->dwFileOS) == 0)
      Qstrcat (pqszStrings[i], "Unknown");

    if (VOS_DOS & pValue->dwFileOS)
      Qstrcat (pqszStrings[i], "MS-DOS");

    if (VOS_OS216 & pValue->dwFileOS)
      Qstrcat (pqszStrings[i], "16 Bit OS/2");

    if (VOS_OS232 & pValue->dwFileOS)
      Qstrcat (pqszStrings[i], "32 Bit OS/2");

    if (VOS_NT & pValue->dwFileOS)
      Qstrcat (pqszStrings[i], "MS-NT");

    /* Add a separator if an extention is used
    if ((VOS_BASE & pValue->dwFileOS) != 0)
      Qstrcat (pqszStrings[i], " - ");

    if (VOS_WINDOWS16 & pValue->dwFileOS)
      Qstrcat (pqszStrings[i], "16 Bit Windows");

    if (VOS_PM16 & pValue->dwFileOS)
      Qstrcat (pqszStrings[i], "16 Bit Presentation Manager");

    if (VOS_PM32 & pValue->dwFileOS)
      Qstrcat (pqszStrings[i], "32 Bit Presentation Manager");

    if (VOS_WINDOWS32 & pValue->dwFileOS)
      Qstrcat (pqszStrings[i], "32 Bit Windows");

    PrepNextString (pqszStrings, i);
    wCharCount += Qstrlen (pqszStrings[i++] + 1;


    /* File Type */
    QstrcpyAlign (pqszStrings[i], "File Type: ", wAlignColumn);

    switch (pValue->dwFileType)
      {
        case VFT_UNKNOWN:
          Qstrcat (pqszStrings[i], "Unknown");
          break;

        case VFT_APP:
          Qstrcat (pqszStrings[i], "Application");
          break;

        case VFT_DLL:
          Qstrcat (pqszStrings[i], "Dynamic Link Library");
          break;

        case VFT_DRV:
          switch (pValue->dwFileSubtype)
            {
              case VFT_UNKNOWN:
                Qstrcat (pqszStrings[i], "Unknown");
                break;

              case VFT2_DRV_PRINTER:
                Qstrcat (pqszStrings[i], "Printer");
                break;

              case VFT2_DRV_KEYBOARD:
                Qstrcat (pqszStrings[i], "Keyboard");
                break;

              case VFT2_DRV_LANGUAGE:
                Qstrcat (pqszStrings[i], "Language");
                break;

              case VFT2_DRV_DISPLAY:
                Qstrcat (pqszStrings[i], "Display");
                break;

              case VFT2_DRV_MOUSE:
                Qstrcat (pqszStrings[i], "Pointing Device");
                break;

              case VFT2_DRV_NETWORK:
                Qstrcat (pqszStrings[i], "Network");
                break;

              case VFT2_DRV_SYSTEM:
                Qstrcat (pqszStrings[i], "System");
                break;

              case VFT2_DRV_INSTALLABLE:
                Qstrcat (pqszStrings[i], "Installable");
                break;

              case VFT2_DRV_SOUND:
                Qstrcat (pqszStrings[i], "Sound");
                break;

              default:
                Qstrcat (pqszStrings[i], "Unknown");
                break;
            }

          Qstrcat (pqszStrings[i], " Driver");
          break;

        case VFT_FONT:
          switch (pValue->dwFileSubtype)
            {
              case VFT2_UNKNOWN:
                Qstrcat (pqszStrings[i], "Unknown Font Type");
                break;

              case VFT2_FONT_RASTER:
                Qstrcat (pqszStrings[i], "Raster Font");
                break;

              case VFT2_FONT_VECTOR:
                Qstrcat (pqszStrings[i], "Vector Font");
                break;

              case VFT2_FONT_TRUETYPE:
                Qstrcat (pqszStrings[i], "True Type Font");
                break;

              default:
                Qstrcat (pqszStrings[i], "Unknown Font Type");
                break;
            }
          break;

        case VFT_VXD:
          Qstrcat (pqszStrings[i], "VxD");
          break;

        case VFT_STATIC_LIB:
          Qstrcat (pqszStrings[i], "Static Library");
          break;

        default:
          Qstrcat (pqszStrings[i], "Unknown");
      }

    PrepNextString (pqszStrings, i);
    wCharCount += Qstrlen (pqszStrings[i++]) + 1;


    /* File date */

    /* This will have to wait until I learn how to calculate */
    /*   the number of 100-nanosecond intervals have occured */
    /*   since midnight, January 1, 1601 (the NT standard    */
    /*   time/date stamp).                                   */
  }

  {
    /* String information */

    WORD wSfiTotLength;     /* StringFileInfo total length              */
    WORD wSfiValLength;     /* StringFileInfo value length              */
    BYTE * pSfiVer;         /* Pointer to start of StringFileInfo block */


    /* Add a blank line */
    pqszStrings[i][0] = '\0';
    PrepNextString (pqszStrings, i);
    wCharCount += Qstrlen (pqszStrings[i++]) + 1;


    /* Set pVer to point to StringFileInfo */
    pVer += sizeof (VS_FIXEDFILEINFO);
    pVer = DWORDUP (pVer);
    pSfiVer = pVer;

    /* Set the length variables */
    pWord = (WORD *) pVer;
    wSfiTotLength = *pWord;
    wSfiValLength = *(++pWord);
    pVer += 4;

    /* Add "StringFileInfo" */
    Qstrcpy (pqszStrings[i], pVer);
    PrepNextString (pqszStrings, i);
    wCharCount += Qstrlen (pqszStrings[i++]) + 1;

    /* Point to the first child */
    pVer += strlen (pVer) + 1;
    pVer = DWORDUP (pVer);


    /* Get the String Table Blocks */
    while (pVer < &pSfiVer[wSfiTotLength])
      {
        WORD wStTotLength;    /* StringTable total length              */
        WORD wStValLength;    /* StringTable value length              */
        WORD wLanguageId;     /* Language ID of string info block      */
        WORD wCodePage;       /* CodePage of string info block         */
        WORD u;               /* Index variable                        */
        BYTE * pStVer = pVer; /* Pointer to start of StringTable block */


        /* Set the length variables */
        pWord = (WORD *) pVer;
        wStTotLength = *pWord;
        wStValLength = *(++pWord);
        pVer += 4;

        /* Interpret the 8 digit hex number */
        strncpy (chBuffer, pVer, 4);
        chBuffer[4] = ' ';
        strncpy (&chBuffer[5], &pVer[4], 4);
        chBuffer[9] = '\0';
        sscanf (chBuffer, "%x %x", &wLanguageId, &wCodePage);

        /* Determine the Language ID */
        Qstrcpy (pqszStrings[i], "Microsoft Language ID: ");
        for (u = 0;
             u < MAX_LANGUAGE_IDS && rgLang[u].wLanguageId != wLanguageId;
             ++u)
          ;

        Qstrcat (pqszStrings[i], rgLang[u].pszLanguageId);
        PrepNextString (pqszStrings, i);
        wCharCount += Qstrlen (pqszStrings[i++]) + 1;

        /* Determine the CodePage */
        Qstrcpy (pqszStrings[i], "Windows CodePage: ");
        for (u = 0;
             u < MAX_CODEPAGE && rgCP[u].wCodePage != wCodePage;
             ++u)
          ;

        Qstrcat (pqszStrings[i], rgCP[u].pszCodePage);
        PrepNextString (pqszStrings, i);
        wCharCount += Qstrlen (pqszStrings[i++]) + 1;


        /* Point to the first child */
        pVer += strlen (pVer) + 1;
        pVer = DWORDUP (pVer);


        /* Get the String blocks */
        while (pVer < &pStVer[wStTotLength])
          {
            CHAR chBuffer[80];    /* String buffer                    */
            WORD wSTotLength;     /* StringTable total length         */
            WORD wSValLength;     /* StringTable value length         */
            BYTE * pSVer = pVer;  /* Pointer to start of String block */


            /* Set the length variables */
            pWord = (WORD *) pVer;
            wSTotLength = *pWord;
            wSValLength = *(++pWord);
            pVer += 4;

            /* Add the key string */
            strcpy (chBuffer, pVer);
            strcat (chBuffer, ": ");
            wAlignColumn = max (wAlignColumn, strlen (chBuffer));
            QstrcpyAlign (pqszStrings[i], chBuffer, wAlignColumn);

            /* Add the value string */
            pVer += strlen (pVer) + 1;
            pVer = DWORDUP (pVer);
            Qstrcat (pqszStrings[i], pVer);
            PrepNextString (pqszStrings, i);
            wCharCount += Qstrlen (pqszStrings[i++]) + 1;

            /* Point to the next sibling */
            pVer = pSVer + wSTotLength;
            pVer = DWORDUP (pVer);
          }
      }
  }

  /* The last string must be a NULL pointer */
  pqszStrings[i] = NULL;


  /* Free up what memory we can */
  _expand (pqszStrings, (i + 1) * sizeof (QSZ));
  Qexpand (pqszStrings[0], wCharCount);

  return (pqszStrings);
}
