/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * BROWSE.C - Source file for the memory browsing functions.
 ********************************************************************/


/* Include Files */

#include "msd.h"

PSZ paszBiosSearchStrings[] =
  {
    "VERSION",
    "COPYRIGHT",
    "TECH",
    "INC.",
    "LTD",
    "COPR.",
    "COMPA",
    "BIOS",
    "CCOO",
    "(C)",
    "ALL RIGHTS RESERVED",
    "REV ",
    "RESI",
    NULL
  };


/*********************************************************************
 * GetBrowseInfo - Gets the browser strings and stores them in the
 *                 structure.
 *
 * pwndCaller        - PWND of the calling dialog box.
 * pszSearchString   - String to search for
 * fpSearchArea      - Area to search
 * wSearchLength     - Length of search
 *
 * Returns:  PSZ * to strings that were found, or NULL if an error
 *           occured.
 *********************************************************************/

QSZ * GetBrowseInfo (PSZ      pszTitle,
                     PSZ      pszSearchString,
                     CHAR FAR *fpSearchArea,
                     DWORD    dwSearchLength)
{
  MEM_STRING_DATA           /* Structure for storing located strings */
      msdStrings[MAX_MSD_STRINGS + 1];
  int  iBrowseResult;        /* Result of the video browse            */
  QSZ  *pqszStrings = NULL;  /* Array of string pointers to display   */
  PSZ  *ppszSearchStrings = NULL;  /* Array of search strings         */
  WORD wSearchLength;        /* Actual search length                  */
  PSZ  paszUserSearchStrings[2];  /* Local array of search strings.   */
                                  /*   Used for user provided string. */


  /* Set the search length */
  if (dwSearchLength > 0xFFFF)
    wSearchLength = 0xFFFF;
  else
    wSearchLength = (WORD) dwSearchLength;


  /* Determine if a search string was given to us */
  if (pszSearchString == NULL || pszSearchString[0] == '\0')
    ppszSearchStrings = paszBiosSearchStrings;
  else
    {
      paszUserSearchStrings[0] = pszSearchString;
      paszUserSearchStrings[1] = NULL;

      ppszSearchStrings = paszUserSearchStrings;
    }


  /* Prepare for the search */
  BrowseInit (fpSearchArea, msdStrings, MAX_MSD_STRINGS);

  /* Perform the search */
  iBrowseResult = Browse (ppszSearchStrings,
                          fpSearchArea,
                          wSearchLength,
                          msdStrings,
                          MAX_MSD_STRINGS);

  /* Convert the info to displayable strings */
  pqszStrings = SprintBrowseInfo (msdStrings);

  return (pqszStrings);
}


/*********************************************************************
 * SprintBrowseInfo - Converts browsed strings into a form displayable
 *                    within an info window.
 *
 * pmsdStrings - The collection of strings and lengths to convert
 *
 * Returns:  String array for display or printing, or NULL on error.
 *********************************************************************/

QSZ * SprintBrowseInfo (MEM_STRING_DATA *pmsdData)
{
  WORD wNmbrStrings;        /* Number of strings                     */
  WORD wNmbrChars;          /* Number of characters in the strings   */
  WORD i, wMsdIndex;        /* Index variables                       */
  INT  iLength;             /* Length of far string                  */
  WORD wLength;             /* Copy of string length                 */
  INT  c;                   /* Character from far memory location    */
  INT  iLineCount;          /* Number of lines in this browse string */
  CHAR FAR *fpString = NULL;/* Far pointer to the string             */
  QSZ  *pqszStrings = NULL; /* Location for storing string pointers  */
  CHAR chBuffer[40];        /* sprintf buffer                        */


  /* Overestimate the amount of space this will require */
  for (i = 0; i < MAX_MSD_STRINGS && pmsdData[i].cfpString != NULL; ++i)
    ;

  wNmbrStrings = (i * MAX_BROWSE_LINES) + 1;
  wNmbrChars   = wNmbrStrings * 72;

  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the information in place */

  for (i = 0, wMsdIndex = 0;
       wMsdIndex < MAX_MSD_STRINGS && pmsdData[wMsdIndex].cfpString != NULL;
       ++wMsdIndex)
    {
      sprintf (chBuffer, "%04X:%04X ",
               FP_SEG (pmsdData[wMsdIndex].cfpString),
               FP_OFF (pmsdData[wMsdIndex].cfpString));
      Qstrcpy (pqszStrings[i], chBuffer);

      iLength  = pmsdData[wMsdIndex].iStringLen;
      fpString = pmsdData[wMsdIndex].cfpString;

      iLineCount = 1;

      while (iLength--)
        {
          c = *(fpString++);

          switch (c)
            {
              case '\n':
                break;

              case '\r':
                /* Are we at the maximum number of lines */
                if (++iLineCount > MAX_BROWSE_LINES)
                  {
                    /* No more lines for this one */
                    iLength = 0;
                  }
                else
                  {
                    PrepNextString (pqszStrings, i++);
                    Qmemset (pqszStrings[i], ' ', 10);
                    pqszStrings[i][10] = '\0';
                  }
                break;

              /* This is a printing character */
              default:

                wLength = Qstrlen (pqszStrings[i]);

                /* Is the line too long to add the character */
                if (wLength >= REPORT_WIDTH)
                  {
                    /* Are we at the maximum number of lines */
                    if (++iLineCount > MAX_BROWSE_LINES)
                      {
                        /* No more lines for this one */
                        iLength = 0;

                        break;
                      }
                    else
                      {
                        PrepNextString (pqszStrings, i++);
                        Qmemset (pqszStrings[i], ' ', 10);
                        pqszStrings[i][10] = '\0';
                        wLength = 10;
                      }
                  }

                pqszStrings[i][wLength]     = (CHAR) c;
                pqszStrings[i][wLength + 1] = '\0';

                break;
            }
        }

      PrepNextString (pqszStrings, i++);
    }

  pqszStrings[i] = NULL;

  return (pqszStrings);
}


/*********************************************************************
 * ListOptionRoms - Creates a list of the option ROMs installed in
 *                  the system.
 *
 * Returns:  Array of strings with Option ROM names, addresses, and
 *           sizes, or NULL on an error.
 *********************************************************************/

PSZ * ListOptionRoms (VOID)
{
  WORD wNmbrStrings;             /* Number of strings                   */
  WORD wNmbrChars;               /* Number of characters in the strings */
  ROM_MAP *pRomMap = NULL;       /* ROM area locations and sizes        */
  INT  i;                        /* Looping variable                    */
  WORD wIndex = 0;               /* Index to ppszStrings                */
  WORD wSize;                    /* Size of various structs             */
  PSZ  *ppszStrings = NULL;      /* String buffer                       */
  COMPUTER_STRUCT *pCpu = NULL;  /* For getting BUS type (PS/2)         */
  WORD wBusType;                 /* Stores the BUS type                 */


  /* Get the computer IRQ info (contains bus type) */

  wSize = GetInfoSize (IDI_COMPUTER_RECORD, FALSE);

  if (wSize == 0)
    return (NULL);


  /* Allocate enough room for the info structure */
  pCpu = malloc (wSize);

  if (pCpu == NULL)
    {
      OutOfMemory();
      return (NULL);
    }

  /* Zero out the structure */
  memset (pCpu, '\0', wSize);

  /* Fill the structure with the information */
  if (GetComputerIrqInfo (pCpu))
    {
      free (pCpu);
      return (NULL);
    }

  /* Store the bus type */
  wBusType = pCpu->wBusType;
  free (pCpu);


  /* Make room for the ROM Map */

  pRomMap = malloc (sizeof (ROM_MAP));
  if (pRomMap == NULL)
    {
      OutOfMemory();
      return (NULL);
    }

  memset (pRomMap, '\0', sizeof (ROM_MAP));

  /* Get the ROM Map */
  GetRomMap (pRomMap, wBusType);


  /* Allocate space for the pointer area and string area */

  wNmbrStrings = 10;
  wNmbrChars   = wNmbrStrings * 34;

  if ((ppszStrings = calloc (wNmbrStrings + 1, sizeof (PSZ))) != NULL)
    ppszStrings[0] = malloc (wNmbrChars);

  if (ppszStrings == NULL || ppszStrings[0] == NULL)
    {
      free (ppszStrings);
      free (pRomMap);
      OutOfMemory();
      return (NULL);
    }


  /* Load the ROM BIOS information into ppszStrings */
  for (i = 0; i < 2 && pRomMap->wRomBiosLoc[i] != 0; ++i)
    {
      /* BIOS information */
      if (i == 0)
        {
          if (pRomMap->wRomBiosLoc[1] == 0)
            {
              sprintf (ppszStrings[wIndex],
                       "ROM BIOS              %04X   %6lu",
                       pRomMap->wRomBiosLoc[i],
                       pRomMap->dwRomBiosSize[i]);

              ppszStrings[wIndex + 1] = ppszStrings[wIndex] +
                                        strlen (ppszStrings[wIndex]) + 1;
              ++wIndex;
            }
          else
            {
              sprintf (ppszStrings[wIndex],
                       "ROM BIOS part 1       %04X   %6lu",
                       pRomMap->wRomBiosLoc[i],
                       pRomMap->dwRomBiosSize[i]);

              ppszStrings[wIndex + 1] = ppszStrings[wIndex] +
                                        strlen (ppszStrings[wIndex]) + 1;
              ++wIndex;
            }
        }
      else
        {
          sprintf (ppszStrings[wIndex],
                   "ROM BIOS part 2       %04X   %6lu",
                   pRomMap->wRomBiosLoc[i],
                   pRomMap->dwRomBiosSize[i]);

          ppszStrings[wIndex + 1] = ppszStrings[wIndex] +
                                    strlen (ppszStrings[wIndex]) + 1;
          ++wIndex;
        }
    }


  /* Load the Option ROM information into the listbox */
  for (i = 9; i >=0; --i)
    {
      switch (pRomMap->wOptRomLoc[i])
        {
          case 0x0000:
            continue;

          case 0xC000:
            /* Video ROM */
            sprintf (ppszStrings[wIndex],
                     "Video ROM BIOS        C000  %7lu",
                     pRomMap->dwOptRomSize[i]);
            ppszStrings[wIndex + 1] = ppszStrings[wIndex] +
                                      strlen (ppszStrings[wIndex]) + 1;
            ++wIndex;
            break;

          default:
            /* Generic Option ROM */
            sprintf (ppszStrings[wIndex],
                     "Option ROM            %04X  %7lu",
                     pRomMap->wOptRomLoc[i],
                     pRomMap->dwOptRomSize[i]);
            ppszStrings[wIndex + 1] = ppszStrings[wIndex] +
                                      strlen (ppszStrings[wIndex]) + 1;
            ++wIndex;
            break;
        }
    }

  /* Set the last one to NULL */
  ppszStrings[wIndex] = NULL;

  free (pRomMap);

  return (ppszStrings);
}


/*********************************************************************
 * ReportBrowseInfo - Writes the browser information to the file.
 *
 * fileReportFile - File handle for reporting.
 *
 * Returns: TRUE if an error occured.
 *********************************************************************/

BOOL ReportBrowseInfo (FILE *fileReportFile)
{
  QSZ * pqszStrings;            /* Browser Strings                     */
  PSZ * ppszRomStrings;         /* List of ROM Areas                   */
  WORD  wSearchSegment;         /* Segment to search                   */
  DWORD dwSearchLength;         /* Search Length                       */
  INT   i;                      /* Looping variable                    */
  BOOL  fReturnValue = FALSE;   /* Return value from various functions */
  CHAR  chBuffer[80];           /* sscanf buffer                       */


  /* Get the list of ROMs */
  ppszRomStrings = ListOptionRoms();
  if (ppszRomStrings == NULL)
    return (TRUE);


  /* Report the strings in each one */
  for (i = 0; ppszRomStrings[i] != NULL && fReturnValue == FALSE; ++i)
    {
      /* Obtain the search parameters */
      Qstrcpy (chBuffer, ppszRomStrings[i]);
      sscanf (&chBuffer[TSR_ADDRESS_COL], "%04X", &wSearchSegment);
      dwSearchLength = atol (&chBuffer[TSR_SIZE_COL]);

      /* Get the strings from that ROM area */
      pqszStrings = GetBrowseInfo (ppszRomStrings[i],
                                   NULL,
                                   (CHAR FAR *) ((DWORD) wSearchSegment << 16),
                                   dwSearchLength);

      /* Write out the strings */
      if (pqszStrings != NULL)
        {
          fReturnValue = WriteInfo (fileReportFile,
                                    chBuffer,
                                    pqszStrings);
        }
      else
        return (TRUE);

      /* Free up the allocated memory */
      FreeStringSpace (pqszStrings);
    }

  /* Free up the list of ROMs */
  free (ppszRomStrings[0]);
  free (ppszRomStrings);

  return (fReturnValue);
}


int iStringCount;               /* Total number of strings found */
int iLastNmbrFound;             /* Last number of strings found  */


/*************************************************************************
 * BrowseInit - Prepares for the browsing action.
 *
 * msdFoundStrings - MSD strings to be cleared out.
 *
 * iMaxMSDStrings  - Number of strings to clear.
 *************************************************************************/

VOID BrowseInit (char far *cfpSearchArea,
                 MEM_STRING_DATA *msdFoundStrings,
                 int iMaxMSDStrings)
{
  ClearMSDArray (msdFoundStrings, iMaxMSDStrings);

  iStringCount = 0;
  iLastNmbrFound = 0;
}


/*************************************************************************
 * Browse() - Browses through memory for strings.
 *
 * paszSearchString - Pointer to array of strings to search for in memory.
 *
 * cfpSearchArea - Far pointer to the area of memory to search.
 *
 * uiLengthOfSearch - Number of bytes to search at that cfpSearchArea.
 *
 * msdFoundStrings - Array to store the strings that were found.
 *
 * iMaxMSDStrings - Maximum number of strings to store in MSD array.
 *
 * Returns:  Number of strings found.
 *************************************************************************/

int Browse (char **paszSearchStrings,
            char far *cfpSearchArea,
            unsigned int uiLengthOfSearch,
            MEM_STRING_DATA *msdFoundStrings,
            int iMaxMSDStrings)
{
  register int i1;    /* Looping variables                 */
  int iSearchResult;  /* To store the result of the search */


  /* Get strings from memory browser */

  for (i1 = 0; paszSearchStrings[i1] != NULL; ++i1)
    {
      iStringCount = max((iSearchResult =
                         SingleStringMemSearch (msdFoundStrings,
                                                iMaxMSDStrings,
                                                paszSearchStrings[i1],
                                                cfpSearchArea,
                                                uiLengthOfSearch)),
                         iStringCount);

#if 0
      if (iSearchResult == SEARCH_CANCELED)
        return (SEARCH_CANCELED);
#endif

      iLastNmbrFound = iStringCount;
    }

  return (iStringCount);
}


/**********************************************************************
 * ClearMSDArray - Clears out MEM_STRING_DATA structures
 *
 * msd           - Pointer to the structure to clear
 *
 * iStringCount  - Number of elements in the structure
 **********************************************************************/

void ClearMSDArray (MEM_STRING_DATA *msd, int iStringCount)
{
  register int i;

  for (i = 0; i < iStringCount; ++i)
    {
      msd[i].cfpString = NULL;
      msd[i].iStringLen = 0;
    }
}


/***********************************************************************
 * SingleStringMemSearch - Returns the number of strings found
 *                         during the memory search.
 *
 * MEM_STRING_DATA *msdFoundArray
 *              - This is the pointer to the array that will hold
 *                the strings that are found.
 *
 * int iMaxArray
 *              - The total number of strings msdFoundArray can hold.
 *
 * char *pszSearchString
 *              - Pointer to the strings to search for.
 *
 * char far *cfpAreaToSearch
 *              - Area of memory to search (ie 0xF0000000).
 *
 * unsigned int uiLengthOfSearch
 *              - Total number of bytes to search (ie, 0xFFFF).
 ***********************************************************************/

int SingleStringMemSearch (MEM_STRING_DATA *msdFoundArray,
                           int iMaxArray,
                           char *pszSearchString,
                           char far *cfpAreaToSearch,
                           unsigned int uiLengthOfSearch)
{
  int iLength;                         /* Length of string returned */
                                       /*   from fbiInstr() */
  int iMaxInArray = 0;                 /* Holds the count of strings */
                                       /*   stored in the structure */
  unsigned int uiLengthToEndOfSearch;  /* As strings are found, */
                                       /*   uiSizeOfSearch will look ahead */
                                       /*   only as far as is necessary */
  char far *pszfMatch;                  /* Pointer to the matched character */
  char far *pszfString;                 /* Pointer to the true beginning of */
                                        /*   the string */

  if (pszSearchString == NULL || pszSearchString[0] == '\0')
    return (-1);

  uiLengthToEndOfSearch = uiLengthOfSearch;

  while ((pszfMatch = fbiInstr (cfpAreaToSearch,
                                pszSearchString,
                                uiLengthToEndOfSearch)) != NULL)
    {
#if 0
      if (KbHit())
        {
          if ((c = GetCh()) == ESC)
            return (SEARCH_CANCELED);
          else if (c == '\0')
            GetCh();
        }
#endif

      pszfString = fbMemString (pszfMatch, &iLength);

      iMaxInArray = max (AddToFoundArray (pszfString, iLength,
                                          msdFoundArray, iMaxArray),
                         iMaxInArray);

      /* Begin searching one byte after the found string */

      cfpAreaToSearch = pszfMatch + 1;

      /* If a string was found, set uiLengthToEndOfSearch to the */
      /* number of bytes left to search */

      uiLengthToEndOfSearch  =
          uiLengthOfSearch -
          ((unsigned) ((long int) cfpAreaToSearch & 0x0000ffff));
    }

#if 0
  if (KbHit())
    {
      if ((c = GetCh()) == ESC)
        return (SEARCH_CANCELED);
      else if (c == '\0')
        GetCh();
    }
#endif

  return (iMaxInArray);
}

/***********************************************************************
 * AddToFoundArray - Adds pszfString and iLength to the msdArray if
 *                   msdArray does not already contain this string.
 *
 * char far *pszfString - Far pointer to the area of memory containing
 *                       the string to store.
 *
 * int iStringLength   - Length of the string in memory.
 *
 * MEM_STRING_DATA *msdArray
 *                     - Pointer to structure for storing the
 *                       far string pointers and lengths.
 *
 * int iMaxArray       - Maximum size of the array (so we don't go
 *                       outside the bounds of the array.
 *
 * Global Variable:
 * fIncludeDuplicates  - Flag to determine wether to include duplicate
 *                       strings in the found array.
 *
 * Returns:  The element the string was stored in, or iMaxArray if the
 *           array was full.
 ***********************************************************************/

int AddToFoundArray (char far *pszfString,
                     int iStringLength,
                     MEM_STRING_DATA *msdArray,
                     int iMaxArray)
{
  register int i = 0;    /* Looping variable */

  /* Search to see if this pointer is already in the array */
  while (i < iMaxArray &&
         msdArray[i].cfpString != NULL &&
         msdArray[i].cfpString != pszfString)
    ++i;

  if (i == iMaxArray)
    return (iMaxArray);

  i = 0;

  /* Check to see if this string's contents has already been recorded */

  while (i < iMaxArray &&
         msdArray[i].cfpString != NULL &&
         _fmemcmp (msdArray[i].cfpString, pszfString,
                   max (msdArray[i].iStringLen, iStringLength)))
    ++i;

  /* Add the string if it was not already in the array */

  if (i < iMaxArray && msdArray[i].cfpString == NULL)
    {
      msdArray[i].cfpString  = pszfString;
      msdArray[i].iStringLen = iStringLength;
    }

  return (i + 1);
}
