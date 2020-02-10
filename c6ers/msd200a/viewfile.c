/**********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * VIEWFILE.C - Source file for viewing a file.
 *********************************************************************/


/* Include Files */

#include "msd.h"


/*********************************************************************
 * ViewFile - Views the specified file.
 *
 * pszFilename - Filename to view.
 * fHexDump    - TRUE if a hex dump is requested.
 *
 * Returns: Pointer to the array of strings, NULL if an error occured.
 *********************************************************************/

QSZ * ViewFile (PSZ pszFilename, BOOL fHexDump)
{
  INT  iReturnValue;  /* Return Value from ReadLine               */
  QSZ  *pqszStrings;  /* Pointer to string pointer array          */
  WORD wNmbrStrings;  /* Number of string pointers available      */
  WORD wNmbrChars;    /* Number of character pointers available   */
  WORD wCharCount = 0;/* Total number of chars stored in memory   */
  WORD wLineCount = 1;/* Total number of lines stored in memory   */
  WORD i = 0;         /* Looping variable                         */
  FILE *fileInput;    /* File to view                             */
  CHAR chBuffer[REPORT_WIDTH + 1];  /* Input buffer               */
  QSZ *pqszNewPointer;/* Stores return from realloc               */
  QSZ qszNewPointer;  /* Stores return from Qexpand               */


  /* Allocate almost all of the available memory */
  wNmbrStrings = 1;
  wNmbrChars   = 65500;

  pqszStrings = calloc (wNmbrStrings + 1, sizeof (QSZ));
  if (pqszStrings == NULL)
    {
      OutOfMemory();
      return (NULL);
    }

  for (pqszStrings[0] = NULL;
       pqszStrings[0] == NULL && wNmbrChars > 100;
       wNmbrChars -= 100)
    {
      pqszStrings[0] = Qmalloc (wNmbrChars);
    }

  if (pqszStrings[0] == NULL)
    {
      free (pqszStrings);
      OutOfMemory();
      return (NULL);
    }

  /* Read the file */
  fileInput = OpenFile (pszFilename, "rb", TRUE);

  if (fileInput == NULL)
    {
      FreeStringSpace (pqszStrings);
      return (NULL);
    }

  while ((iReturnValue = ReadLine (chBuffer,
                                   REPORT_WIDTH,
                                   fileInput,
                                   FALSE))        != EOF)
    {
      wCharCount += (WORD) iReturnValue;
      ++wLineCount;

      /* Allocate room for a new pointer */
      pqszNewPointer = realloc (pqszStrings, (wLineCount + 1) * sizeof (QSZ));
      if (pqszNewPointer == NULL)
        {
          ShowError (MB_OK | 0x8000, "Insufficient memory to read entire file",
                     pszFilename, NULL);
          break;
        }
      else
        pqszStrings = pqszNewPointer;

      /* Allocate room for the new line */
      qszNewPointer = Qexpand (pqszStrings[0], wCharCount);
      if (qszNewPointer == NULL)
        {
          ShowError (MB_OK | 0x8000, "Insufficient memory to read entire file",
                     pszFilename, NULL);
          break;
        }
      else
        pqszStrings[0] = qszNewPointer;

      Qstrcpy (pqszStrings[i], chBuffer);

      PrepNextString (pqszStrings, i++);
    }

  /* The last string must be a NULL pointer */
  pqszStrings[i] = NULL;


  /* Close the file */
  CloseFile (fileInput);


  return (pqszStrings);
}


/*********************************************************************
 * FindAndViewFile - Finds and views the specified file.
 *
 * pszFilename  - Filename to find and view.
 * fSearchFlags - Flags to use when searching.
 * fHexDump     - TRUE if a hex dump is requested.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL FindAndViewFile (PSZ pszFilename, BOOL fSearchFlags, BOOL fHexDump)
{
  FILE_INFO FAR * ffi;        /* File Information structure             */
  FILE_INFO FAR * ffi2;       /* 2nd File Information structure         */
  CHAR chBuffer[_MAX_PATH];   /* Local storage for fully qualified path */
  QSZ  *pqszStrings;          /* String array for file contents         */
  PWND pwndReturnValue;       /* Return value from CreateInfoWnd        */


  /* Inform the user that we are searching */
  DisplayStatus (ST_SEARCHING);

  /* Find the requested file */
  ffi = FindFile (pszFilename, NULL, fSearchFlags, '\0');

#if HEAP_DEBUG
  HeapCheck ("After FindFile");
#endif

  /* Return TRUE if no files were found */
  if (ffi == NULL || ffi->fpNextFileInfo == NULL)
    {
      FreeFileInfo (ffi);
      PostMessage (pwndStatusLine, WM_PAINT, NULL, NULL);
      return (TRUE);
    }

  /* Did we find more than one file */
  ffi2 = (FILE_INFO FAR *) ffi->fpNextFileInfo;
  if (ffi2->fpNextFileInfo != NULL)
    {
      pfiDlg = (FILE_INFO FAR *) ffi;
      if (DialogBox (&dlgViewWhichFile, ViewWhichFileDlg) == IDOK)
        strcpy (chBuffer, pszInsertFilename);
      else
        {
          FreeFileInfo (ffi);
          PostMessage (pwndStatusLine, WM_PAINT, NULL, NULL);
          return (TRUE);
        }
    }
  else
    {
      /* Single file -- put the filename into chBuffer */
      _fstrcpy (chBuffer, ffi->fpszPathToFile);
    }

  FreeFileInfo (ffi);

#if HEAP_DEBUG
  HeapCheck ("After FreeFileInfo");
#endif

  /* Read the file */
  pqszStrings = ViewFile (chBuffer, fHexDump);
  if (pqszStrings == NULL)
    {
      PostMessage (pwndStatusLine, WM_PAINT, NULL, NULL);
      return (TRUE);
    }

#if HEAP_DEBUG
  HeapCheck ("After ViewFile");
#endif

  /* Display the file in an Info Window */
  pwndReturnValue = CreateInfoWnd (chBuffer, pqszStrings, FALSE);

#if HEAP_DEBUG
  HeapCheck ("After CreateInfoWnd");
#endif

  PostMessage (pwndStatusLine, WM_PAINT, NULL, NULL);

  if (pwndReturnValue == NULL)
    return (TRUE);
  else
    return (FALSE);
}
