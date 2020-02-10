/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * LPTINFO.C - Source file for parallel printer port detection code.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/*********************************************************************
 * GetLptInfo - Gets the LPT port information.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetLptInfo (LPT_STRUCT *pLpt, BOOL fMinimumInfo)
{
  WORD i;                           /* Looping variable    */
  WORD wLptStatus[MAX_LPT_PORTS];   /* Printer port status */


  {
    /* Zero out the count of LPT ports */

    pLpt->wNmbrLptPorts = 0;


    /* Count the number of LPT ports in the system */

    for (i = 0; i < MAX_LPT_PORTS; i++)
      {
        wLptStatus[i] = _bios_printer (_PRINTER_STATUS, i, 0);

        if (!(wLptStatus[i] & 6))
          ++(pLpt->wNmbrLptPorts);
      }

    if (fMinimumInfo)
      return (FALSE);
  }

  {
    /* Set the values in the structure for each port */

    /* 40:8 is the port address of the first valid LPT port */
    WORD FAR * fwPortAddress = (WORD FAR *) 0x00400008;

    for (i = 0; i < MAX_LPT_PORTS; i++)
      {
        pLpt->LptxInfo[i].fLptPortDetected
                                    = (wLptStatus[i] & 0x06) ? FALSE : TRUE;
        pLpt->LptxInfo[i].wPortAddress
                                    = *(fwPortAddress++);
        pLpt->LptxInfo[i].fOnLine   = (wLptStatus[i] & 0x10) ? TRUE  : FALSE;
        pLpt->LptxInfo[i].fPaperOut = (wLptStatus[i] & 0x20) ? TRUE  : FALSE;
        pLpt->LptxInfo[i].fIoError  = (wLptStatus[i] & 0x08) ? TRUE  : FALSE;
        pLpt->LptxInfo[i].fTimeOut  = (wLptStatus[i] & 0x01) ? TRUE  : FALSE;
        pLpt->LptxInfo[i].fBusy     = (wLptStatus[i] & 0x80) ? FALSE : TRUE;
        pLpt->LptxInfo[i].fAck      = (wLptStatus[i] & 0x40) ? TRUE  : FALSE;
      }
  }

  return (FALSE);
}


/*********************************************************************
 * SprintLptInfo - Put LPT port information into a set of strings to
 *                 be printed or displayed.
 *
 * Returns:  NULL if an error occured.
 *********************************************************************/

QSZ * SprintLptInfo (LPT_STRUCT *pLpt,
                     CHAR szSumStrings[][MAX_SUMM_INFO + 5])
{
  WORD wNmbrStrings;        /* Number of strings                     */
  WORD wNmbrChars;          /* Number of characters in the strings   */
  WORD wUnderlineLength;    /* Length of the underline string        */
  WORD wLptIndex;           /* Index to the structure of LPT data    */
  WORD i;                   /* Looping variables                     */
  QSZ  *pqszStrings = NULL; /* Location for storing string pointers  */


  /* Summary information */
  if (szSumStrings != NULL)
    {
      sprintf (szSumStrings[0], "%d", pLpt->wNmbrLptPorts);
      return (NULL);
    }


  /* Calculate the amount of space required for the strings */

  wUnderlineLength = strlen (pszLptUnderline);

  wNmbrChars   = strlen (pszLptHeader1) + 1 +
                 strlen (pszLptHeader2) + 1 +
                 wUnderlineLength       + 1 +
                 (MAX_LPT_PORTS * (wUnderlineLength + 1));

  /* The underline string is expected to be as long as a line of */
  /*   LPT port info.                                            */

  wNmbrStrings = MAX_LPT_PORTS + 4;

  /* "+ 4" is for the 2 header lines, the underline, and the NULL */
  /*   pointer at the end of the array.                           */


  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the first three strings in place */

  Qstrcpy (pqszStrings[0], pszLptHeader1);
  pqszStrings[1] = pqszStrings[0] + Qstrlen (pqszStrings[0]) + 1;

  Qstrcpy (pqszStrings[1], pszLptHeader2);
  pqszStrings[2] = pqszStrings[1] + Qstrlen (pqszStrings[1]) + 1;

  Qstrcpy (pqszStrings[2], pszLptUnderline);
  pqszStrings[3] = pqszStrings[2] + wUnderlineLength + 1;

  /* Put the LPT port information in place */

  for (i = 3, wLptIndex = 0; wLptIndex < MAX_LPT_PORTS;
       ++i, ++wLptIndex)
    {
      WORD wLength;       /* Current length of string */
      CHAR chBuffer[80];  /* Buffer for string data   */

      /* Clear out the string */
      Qmemset (pqszStrings[i], ' ', wUnderlineLength);

      /* Port */
      wLength = sprintf (chBuffer, "LPT%d:", wLptIndex + 1);
      Qstrncpy (&pqszStrings[i][LPT_PORT_COL], chBuffer, wLength);

      if (pLpt->LptxInfo[wLptIndex].fLptPortDetected)
        {
          PSZ pszString = NULL;  /* Pointer to a string */

          /* Port Address */
          wLength = sprintf (chBuffer, "%04XH",
                             pLpt->LptxInfo[wLptIndex].wPortAddress);
          Qstrncpy (&pqszStrings[i][LPT_PORT_ADDR_COL], chBuffer, wLength);

          /* Online */
          pszString = (pLpt->LptxInfo[wLptIndex].fOnLine) ?
                      pszYes : pszNo_;
          Qstrncpy (&pqszStrings[i][LPT_ON_LINE_COL], pszString,
                   Qstrlen (pszString));

          /* Paper Out */
          pszString = (pLpt->LptxInfo[wLptIndex].fPaperOut) ?
                      pszYes : pszNo_;
          Qstrncpy (&pqszStrings[i][LPT_PAPER_OUT_COL], pszString,
                   Qstrlen (pszString));

          /* I/O Error */
          pszString = (pLpt->LptxInfo[wLptIndex].fIoError) ?
                      pszYes : pszNo_;
          Qstrncpy (&pqszStrings[i][LPT_I_O_ERROR_COL], pszString,
                   Qstrlen (pszString));

          /* Time Out */
          pszString = (pLpt->LptxInfo[wLptIndex].fTimeOut) ?
                      pszYes : pszNo_;
          Qstrncpy (&pqszStrings[i][LPT_TIME_OUT_COL], pszString,
                   Qstrlen (pszString));

          /* Busy */
          pszString = (pLpt->LptxInfo[wLptIndex].fBusy) ?
                      pszYes : pszNo_;
          Qstrncpy (&pqszStrings[i][LPT_BUSY_COL], pszString,
                   Qstrlen (pszString));

          /* ACK */
          pszString = (pLpt->LptxInfo[wLptIndex].fAck) ?
                      pszYes : pszNo;
          Qstrcpy (&pqszStrings[i][LPT_ACK_COL], pszString);
        }
      else
        {
          /* Not a valid port, fill with dashes */

          pqszStrings[i][LPT_PORT_ADDR_COL + 2] = '-';
          pqszStrings[i][LPT_ON_LINE_COL   + 1] = '-';
          pqszStrings[i][LPT_PAPER_OUT_COL + 1] = '-';
          pqszStrings[i][LPT_I_O_ERROR_COL + 1] = '-';
          pqszStrings[i][LPT_TIME_OUT_COL  + 1] = '-';
          pqszStrings[i][LPT_BUSY_COL      + 1] = '-';
          pqszStrings[i][LPT_ACK_COL       + 1] = '-';
          pqszStrings[i][LPT_ACK_COL       + 2] = '\0';
        }

      /* Set the pointer for the next string */
      pqszStrings[i + 1] = pqszStrings[i] + Qstrlen (pqszStrings[i]) + 1;
    }

  /* Set the last pointer to a NULL */
  pqszStrings[i] = NULL;

  /* Return the pointer to pqszStrings */
  return (pqszStrings);
}
