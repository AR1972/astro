/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * SUMINFO.C - Source file for obtaining summary information.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/*********************************************************************
 * GetSummaryInfo - Gets the summary information.
 *
 * pSum         - Pointer to network information structure.
 * fMinimumInfo - TRUE if minimum info is requested.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetSummaryInfo (SUMMARY_STRUCT *pSum, BOOL fMinimumInfo)
{
  BOOL fReturnValue = FALSE;  /* Return value from various GetInfo calls */
  WORD wSize;                 /* Size, in bytes, to store the data       */
  WORD i;                     /* Looping variable                        */
  VOID *pStructForInfo = NULL;/* Pointer to a generic struct             */
  QSZ  *pqszStrings = NULL;   /* String array pointer                    */


  /* There is no minimum info from this routine */
  if (fMinimumInfo)
    return (FALSE);


  /* Zero out the summary info structure */
  memset (pSum, '\0', sizeof (SUMMARY_STRUCT));


  /* Fill the structures with the information */

  for (i = IDI_FIRST_RECORD_TO_SUMMARIZE;
       i <= IDI_LAST_RECORD_TO_SUMMARIZE;
       ++i)
    {
      /* Skip out if /I was used and this item is not available */
      if (fFastStart && rgfReportItemFlag[i] == FALSE)
        continue;

      /* Obtain the info structure's size */
      wSize = GetInfoSize (rwRecordTypes[i], FALSE);

      if (wSize == 0)
        {
          fReturnValue = TRUE;
          break;
        }

      /* Allocate enough room for the info structure */
      pStructForInfo = malloc (wSize);

      if (pStructForInfo == NULL)
        {
          CHAR chBuffer[80];
          sprintf (chBuffer, "After IDI_ %d", i);
          OutOfMemory();
          fReturnValue = TRUE;
          break;
        }

      /* Zero out the structure */
      memset (pStructForInfo, '\0', wSize);

      /* Fill the structure with the information */
      fReturnValue = GetInfo (rwRecordTypes[i],
                              pStructForInfo,
                              TRUE,   /* Obtain minimum info          */
                              FALSE,  /* No header record             */
                              TRUE);  /* Assume we're doing a report  */

      if (fReturnValue)
        {
          free (pStructForInfo);
          continue;
        }

      /* Obtain the summary strings */
      pqszStrings = SprintInfo (rwRecordTypes[i],
                                pStructForInfo,
                                &(pSum->szSumStrings[i * 2]),
                                TRUE);

      /* Free up the memory allocated for the summary info */
      free (pStructForInfo);
    }

  return (FALSE);
}


/*********************************************************************
 * SprintSummaryInfo - Put summary information into a set of strings
 *                     to be printed or displayed.
 *
 * fMemoryType - Type(s) of memory to put into strings (MEM_ALL to
 *               show all types of memory).
 * pMem        - Pointer to the structure describing the memory types.
 *********************************************************************/

QSZ * SprintSummaryInfo (PSZ paszButtonNames[], SUMMARY_STRUCT *pSum)
{
  WORD wNmbrStrings;        /* Number of strings                     */
  WORD wNmbrChars;          /* Number of characters in the strings   */
  WORD i, i1;               /* Index variables                       */
  CHAR chBuffer[80];        /* Local string                          */
  QSZ  *pqszStrings = NULL; /* Location for storing string pointers  */
  WORD wLongestLine = 0;    /* Longest line of summary information   */
  WORD wLength;             /* Length of a string                    */


  /* Determine the longest string of summary information */
  for (i = IDI_FIRST_RECORD_TO_SUMMARIZE; i <= IDI_LAST_RECORD_TO_SUMMARIZE; ++i)
    {
      wLength = strlen (pSum->szSumStrings[i * 2]) + 2 +
                strlen (pSum->szSumStrings[i * 2 + 1]);

      if (wLongestLine < wLength)
        wLongestLine = wLength;
    }


  /* Overestimate the amount of space required for the strings */

  wNmbrStrings = IDI_LAST_RECORD_TO_SUMMARIZE -
                 IDI_FIRST_RECORD_TO_SUMMARIZE + 2;
  wNmbrChars   = (wLongestLine + SUMMARY_ALIGN + 1) * wNmbrStrings;

  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the information in place */

  for (i = 0, i1 = IDI_FIRST_RECORD_TO_SUMMARIZE;
       i1 <= IDI_LAST_RECORD_TO_SUMMARIZE;
       ++i, ++i1)
    {
      /* Button name */
      sprintf (chBuffer, "%s: ", paszButtonNames[i1]);
      QstrcpyAlign (pqszStrings[i], chBuffer, SUMMARY_ALIGN);


      /* Summary Strings */
      Qstrcat (pqszStrings[i], pSum->szSumStrings[i1 * 2]);
      if (pSum->szSumStrings[i1 * 2 + 1][0] != '\0')
        {
          wLength = Qstrlen (pqszStrings[i]);

          if (i1 != IDI_DISK_DRIVE_RECORD &&
              pqszStrings[i][wLength - 2] != ',')
            Qstrcat (pqszStrings[i], pszCommaSpace);

          Qstrcat (pqszStrings[i], pSum->szSumStrings[i1 * 2 + 1]);
        }

      PrepNextString (pqszStrings, i);
    }


  /* Set the last pointer to NULL */

  pqszStrings[i] = NULL;

  /* Return the pointer to pqszStrings */

  return (pqszStrings);
}
