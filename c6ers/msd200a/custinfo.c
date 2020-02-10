/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * CUSTINFO.C - Source file for obtaining the customer data.
 ********************************************************************/


/* Include Files */

#include "msd.h"

/********************************************************************
 * GetCustInfo - Fills in the customer's name and address
 *
 * pCustInfo    - Pointer to structure to be filled with customer
 *                  data.
 * fMinimumInfo - TRUE, if minimum info is requested.
 *
 * Returns: TRUE if an error occured.
 ********************************************************************/

BOOL GetCustInfo (CUSTINFO *pCustInfo, BOOL fMinimumInfo)
{
  BOOL fReturnValue = FALSE;  /* Return value from various functions    */
  INT  i;                     /* Looping variable                       */

  /* There is no minimum info to return from this routine */

  if (fMinimumInfo)
    return (FALSE);

  /* If doing a report only, use a TTY type interface */

  if (fReportOnly)
    {
      /* Something needs to be included to read customer data from */
      /*   the MSD.INI file, as MSD.INI should take the default    */

      /* Display the title lines */

      for (i = 0; fReturnValue == FALSE && paszMsdTitleLines[i] != NULL; ++i)
        fReturnValue = PutString (paszMsdTitleLines[i]);

      /* Get the customer information from the customer */

      for (i = 0; fReturnValue == FALSE && i < MAX_CUST_INFO_TITLES; ++i)
        {
          fReturnValue = GetCustInfoLine (paszCustInfoTitle[i],
                                          pCustInfo->chCustInfo[i]);
        }

      if (fReturnValue == FALSE)
        fReturnValue = PutString ("\nGenerating Report ...\n");

      return (fReturnValue);
    }
#if CW_INCLUDED
  else
    {
      /* We're using CW */

      pCustInfoDlg = pCustInfo;
      if (DialogBox (&dlgCustInfo, CustInfoCmdDlg) == IDCANCEL)
        return (TRUE);
      else
        return (FALSE);
    }
#endif
}

/********************************************************************
 * GetCustInfoLine - Gets a line of data from the customer in TTY
 *                   mode.
 *
 * pszPromptString - Prompt string to print before accepting input.
 * pchInputString  - String to store the input.
 *
 * Returns: TRUE if an error occured.
 ********************************************************************/

BOOL GetCustInfoLine (PSZ pszPromptString, CHAR *pchInputString)
{
  BOOL fReturnValue = FALSE;    /* Return value */
  INT  i;                       /* Looping variable */

  if (pszPromptString != NULL)
    for (i = 0; fReturnValue == FALSE && pszPromptString[i] != '\0'; ++i)
      {
        fReturnValue = putchar (pszPromptString[i]);

        if (fReturnValue == EOF)
          fReturnValue = TRUE;
        else
          fReturnValue = FALSE;
      }

  if (fReturnValue == FALSE)
    fReturnValue = _DosGetLine (pchInputString, MAX_CUST_INFO);

  return (fReturnValue);
}

/********************************************************************
 * SprintCustInfo - Put the customer's name and address into a set of
 *                  strings to be printed.
 *
 * pCustInfo   - Pointer to record containing the data.
 * fReportFlag - TRUE if printing to a report.
 *
 * Returns: Pointer to the array of string pointers.  NULL pointer
 *          indicates that an error occured, or that no customer
 *          information was given.
 ********************************************************************/

QSZ * SprintCustInfo (CUSTINFO *pCustInfo)
{
  WORD wNmbrStrings = 0;    /* Number of stub strings                    */
  WORD wNmbrChars = 0;      /* Number of characters in the stub strings  */
  WORD wLength;             /* Customer String Length                    */
  WORD wTitleLength;        /* Length of title before customer string    */
                            /*   ie. "Company Name: " == 14              */
  WORD i;                   /* Looping variable                          */
  WORD wStringIndex;        /* Index to strings being built              */
  QSZ  *pqszStrings = NULL; /* Location for storing string pointers      */
  PSZ  pszCustString = NULL;/* String pointer to customer name data      */


  /* Count up the number of characters and strings in pszStubStrings */
  for (i = 0; i < MAX_CUST_INFO_TITLES; ++i)
    {
      /* Set the string pointer */

      pszCustString = &pCustInfo->chCustInfo[i][0];

      wLength = strlen (pszCustString);

      if (wLength > 0)
        {
          wTitleLength = strlen (paszCustInfoTitle[i]);

          wNmbrChars += wTitleLength + wLength + 1;

          ++wNmbrStrings;
        }
    }

  /* Bail out if there is no customer information */
  if (wNmbrStrings == 0)
    return (NULL);

  /* Bump the number of strings up one to handle the trailing NULL pointer */
  ++wNmbrStrings;

  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the strings in place */
  for (wStringIndex = 0, i = 0; i < MAX_CUST_INFO_TITLES; ++i)
    {
      /* Set the string pointer */

      pszCustString = &pCustInfo->chCustInfo[i][0];

      if (strlen (pszCustString) > 0)
        {
          Qstrcpy (pqszStrings[wStringIndex], paszCustInfoTitle[i]);
          Qstrcat (pqszStrings[wStringIndex], pszCustString);

          pqszStrings[wStringIndex + 1] =
            pqszStrings[wStringIndex] +
            Qstrlen (pqszStrings[wStringIndex]) + 1;

          ++wStringIndex;
        }
    }

  /* Set the last pointer to NULL */
  pqszStrings[wStringIndex] = NULL;

  /* Return the pointer to pqszStrings */
  return (pqszStrings);
}
