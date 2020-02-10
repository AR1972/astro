/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * COMINFO.C - Source file for serial port detection code.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/*********************************************************************
 * GetComInfo - Gets the COM port information.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetComInfo (COM_STRUCT *pCom, BOOL fMinimumInfo)
{
  WORD  i, wWait;           /* Looping variables                    */
  WORD  wIndex;             /* Index for strings                    */
  WORD  wPort;              /* COM port address                     */
  WORD  wLCR;               /* Line Control Register value          */
  WORD  wMSR;               /* Modem Status Register value          */
  WORD  wNewIID;            /* Used to store changed IID value      */
  DWORD dwBaudRateDivisor;  /* Used to calculate the baud rate      */
  BOOL  fArcnetCard;        /* True if the 02E0-02EF range should   */
                            /*   be avoided                         */

  /* 40:0 is the port address of the first valid COM port */
  WORD FAR * fwPortAddress = (WORD FAR *) 0x00400000;


  /* Determine if an Arcnet card is present at 02E0 */
  fArcnetCard = ArcnetCardPresent();

  /* Set the values in the structure for each port */

  /* Zero out the count of COM ports */
  pCom->wNmbrComPorts = 0;

  for (i = 0; i < MAX_COM_PORTS; ++i)
    {
      {
        /* Load the port address from the DOS Device table, if it exists */
        pCom->ComxInfo[i].wPortAddress = *(fwPortAddress++);
        wPort = pCom->ComxInfo[i].wPortAddress;

        /* Determine if the port exists in the right range */

        if (wPort >= 0x220  &&  wPort <= 0x3FF)
          {
            ++(pCom->wNmbrComPorts);
            pCom->ComxInfo[i].fComPortDetected = TRUE;
          }
        else
          pCom->ComxInfo[i].fComPortDetected = FALSE;

        /* If the Arcnet card is present, disable COM4 testing */
        if (fArcnetCard && wPort >= 0x2E0 && wPort <= 0x2FF)
          pCom->ComxInfo[i].fComPortDetected = FALSE;

        if (fMinimumInfo || !(pCom->ComxInfo[i].fComPortDetected))
          continue;
      }

      {
        /* Read the Line Control Register Port */

        wLCR = inp (wPort + 3);

        /* If the DLAB bit (bit 7) is 1, we can obtain the Baud Rate    */
        /*   Divisor from *fwPortAddress (LSB) and *fwPortAddress + 1   */
        /*   (MSB)                                                      */

        /* If the DLAB bit is not set, set it, then read the Baud Rate  */
        /*   Divisor                                                    */

        if (!(0x80 & wLCR))
          outp (wPort + 3, 0x80 | wLCR);

        /* Pause briefly for the port to respond */
        for (wWait = 0; wWait < 25; ++wWait)
          ;

        dwBaudRateDivisor = (WORD) (inp (wPort)) +
                            (WORD) (inp (wPort + 1) << 8);

        /* Pause briefly for the port to respond */
        for (wWait = 0; wWait < 25; ++wWait)
          ;

        /* If the DLAB bit was not set originally, put it back to it's  */
        /*   original state.                                            */

        if (!(0x80 & wLCR))
          outp (wPort + 3, wLCR);
      }

      {
        /* Calculate the BAUD rate */

        if (dwBaudRateDivisor == 0)
          pCom->ComxInfo[i].dwBaudRate = 0;
        else
          pCom->ComxInfo[i].dwBaudRate = 115200L / dwBaudRateDivisor;
      }

      {
        /* Parity - If bit 3 of the LCR register is off, parity is none. */
        /*          none.  If bit 3 is on, bits 4 and 5 come into play   */
        /*          like this:                                           */
        /*                                                               */
        /*              Bits: 5 4                                        */
        /*                    0 0 - Odd parity                           */
        /*                    0 1 - Even parity                          */
        /*                    1 0 - Mark parity                          */
        /*                    1 1 - Space parity                         */

        if ((wLCR & 0x08) == 0)
          pCom->ComxInfo[i].wParity = 0;
        else
          pCom->ComxInfo[i].wParity = ((wLCR & 0x30) >> 4) + 1;

        memset (pCom->ComxInfo[i].szParity, '\0', MAX_PARITY);

        wIndex = pCom->ComxInfo[i].wParity;
        strcpy (pCom->ComxInfo[i].szParity, paszParityDesc[wIndex]);
      }

      {
        /* Data Bits - Stored in the LCR in bits 0 and 1: */
        /*                                                */
        /*              Bits: 1 0                         */
        /*                    0 0 - 5 data bits           */
        /*                    0 1 - 6 data bits           */
        /*                    1 0 - 7 data bits           */
        /*                    1 1 - 8 data bits           */

        pCom->ComxInfo[i].wDataBits = (wLCR & 0x03) + 5;
      }

      {
        /* Stop Bits - Stored in the LCR in bit 2.  Off is 1 stop bit, */
        /*             On is two stop bits.                            */
        /*             Note:  If data bits is 5 and LCR bit 2 is on,   */
        /*                    stop bits = 1.5.                         */

        pCom->ComxInfo[i].wStopBits = (wLCR & 0x04) ? 2 : 1;

        if (pCom->ComxInfo[i].wDataBits == 5 &&
            pCom->ComxInfo[i].wStopBits == 2)
          pCom->ComxInfo[i].wStopBits = 3;
      }

      {
        /* Read the Modem Status Register values */

        wMSR = inp (wPort + 6);
      }

      {
        /* Carrier Detect is bit 7 of the MSR */

        pCom->ComxInfo[i].fCarrierDetect = (wMSR & 0x80) ? TRUE : FALSE;
      }

      {
        /* Ring Indicator is bit 6 of the MSR */

        pCom->ComxInfo[i].fRingIndicator = (wMSR & 0x40) ? TRUE : FALSE;
      }

      {
        /* Data Set Ready (DSR) is bit 5 of the MSR */

        pCom->ComxInfo[i].fDataSetReady = (wMSR & 0x20) ? TRUE : FALSE;
      }

      {
        /* Clear to Send is bit 4 of the MSR */

        pCom->ComxInfo[i].fClearToSend = (wMSR & 0x10) ? TRUE : FALSE;
      }

      {
        /* Determine the UART chip type.  This is accomplished by       */
        /*   outputing 11000001 to the FIFO Control Register (FCR).     */
        /*   Then, bits 6 and 7 are checked to see if they are on.  The */
        /*   list below shows how the chip type is determined:          */
        /*                                                              */
        /* Bits: 7 6                                                    */
        /*       0 0 - INS8250 UART                                     */
        /*       0 1 - Unknown                                          */
        /*       1 0 - NS16550 UART                                     */
        /*       1 1 - NS16550AN UART                                   */
        /*                                                              */
        /* This may cause problems with TSR communications programs and */
        /*   COM programs running in OS/2 or Windows while MSD runs,    */
        /*   because in order to determine the chip type, I must modify */
        /*   the behavior of the UART chip.                             */

        /* Turn on bits 6 and 7 */
        outp (wPort + 2, 0xC1);

        /* Pause for a moment to allow the port to take the change. */

        for (wWait = 0; wWait < 25; ++wWait)
          ;

        /* Read the changed value */
        wNewIID = inp (wPort + 2);

        /* Set the chip type, based on bits 6 and 7 */
        wIndex = pCom->ComxInfo[i].wUartChip = wNewIID >> 6;

        memset (pCom->ComxInfo[i].szUartChip, '\0', MAX_UART_CHIP);
        strcpy (pCom->ComxInfo[i].szUartChip, paszUartChips[wIndex]);
      }
    }

  return (FALSE);
}


/*********************************************************************
 * SprintComInfo - Put Com port information into a set of strings to
 *                 be printed or displayed.
 *
 * Returns:  NULL if an error occured.
 *********************************************************************/

QSZ * SprintComInfo (COM_STRUCT *pCom,
                     CHAR szSumStrings[][MAX_SUMM_INFO + 5])
{
  WORD wNmbrStrings;         /* Number of strings                    */
  WORD wNmbrChars;           /* Number of characters in the strings  */
  WORD wComIndex;            /* Index to the structure of COM data   */
  WORD i;                    /* Looping variables                    */
  QSZ  *pqszStrings = NULL;  /* Location for storing string pointers */


  /* Summary information */
  if (szSumStrings != NULL)
    {
      sprintf (szSumStrings[0], "%d", pCom->wNmbrComPorts);
      return (NULL);
    }


  /* Calculate the amount of space required for the strings */

  wNmbrChars   = (MAX_COM_TITLES + 3) * (MAX_COM_INFO_LINE + 1);

  wNmbrStrings = MAX_COM_TITLES + 3;


  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);



  /* Put the first two strings in place */

  Qmemset (pqszStrings[0], ' ', COM_TITLE_WIDTH + COM_INFO_COL_WIDTH - 5);
  Qstrcpy (&pqszStrings[0][COM_TITLE_WIDTH + COM_INFO_COL_WIDTH - 5],
          pszComHeader);
  pqszStrings[1] = pqszStrings[0] + Qstrlen (pqszStrings[0]) + 1;

  Qmemset (pqszStrings[1], ' ', COM_TITLE_WIDTH + COM_INFO_COL_WIDTH - 5);
  Qstrcpy (&pqszStrings[1][COM_TITLE_WIDTH + COM_INFO_COL_WIDTH - 5],
          pszComUnderline);
  pqszStrings[2] = pqszStrings[1] + Qstrlen (pqszStrings[1]) + 1;


  /* Put the COM port information in place */

  for (i = 2, wComIndex = 0; wComIndex < MAX_COM_TITLES;
       ++i, ++wComIndex)
    {
      WORD wPort;             /* Port number for each display line */
      WORD wLength;           /* Current length of string          */
      WORD wInfoColumn;       /* Column to place the information   */
      CHAR chBuffer[80];      /* Buffer for string data            */
      PSZ  pszString = NULL;  /* String pointer                    */

      /* Clear out the string */
      Qmemset (pqszStrings[i], ' ', MAX_COM_INFO_LINE);
      pqszStrings[i][MAX_COM_INFO_LINE] = '\0';

      /* Put on the title */
      Qstrncpy (pqszStrings[i], paszComTitles[wComIndex],
               strlen (paszComTitles[wComIndex]));

      for (wPort = 0; wPort < MAX_COM_PORTS; ++wPort)
        {
          wInfoColumn = wPort * COM_INFO_COL_WIDTH + COM_TITLE_WIDTH;


          /* Put the N/A under the Port Address for ports that don't exist */

          if (wComIndex == COM_PORT_ADDRESS &&
              pCom->ComxInfo[wPort].fComPortDetected == FALSE)
            {
              QstrncpyAlign (&pqszStrings[i][wInfoColumn], pszNA,
                            strlen (pszNA), COM_INFO_COL_WIDTH);
            }


          /* Add the strings only if the COM port exists */

          if (pCom->ComxInfo[wPort].fComPortDetected)
            {
              switch (wComIndex)
                {
                  case COM_PORT_ADDRESS:
                    {
                      sprintf (chBuffer, "%04XH",
                               pCom->ComxInfo[wPort].wPortAddress);

                      QstrncpyAlign (&pqszStrings[i][wInfoColumn], chBuffer,
                                    strlen (chBuffer), COM_INFO_COL_WIDTH);
                      break;
                    }

                  case COM_BAUD_RATE:
                    {
                      sprintf (chBuffer, "%Lu",
                               pCom->ComxInfo[wPort].dwBaudRate);
                      QstrncpyAlign (&pqszStrings[i][wInfoColumn], chBuffer,
                                    strlen (chBuffer), COM_INFO_COL_WIDTH);
                      break;
                    }

                  case COM_PARITY:
                    {
                      wLength = strlen (pCom->ComxInfo[wPort].szParity);

                      QstrncpyAlign (&pqszStrings[i][wInfoColumn],
                                    pCom->ComxInfo[wPort].szParity,
                                    wLength, COM_INFO_COL_WIDTH);
                      break;
                    }

                  case COM_DATA_BITS:
                    {
                      sprintf (chBuffer, "%d",
                               pCom->ComxInfo[wPort].wDataBits);
                      QstrncpyAlign (&pqszStrings[i][wInfoColumn], chBuffer,
                                    strlen (chBuffer), COM_INFO_COL_WIDTH);
                      break;
                    }

                  case COM_STOP_BITS:
                    {
                      if (pCom->ComxInfo[wPort].wStopBits < 3)
                        {
                          sprintf (chBuffer, "%d",
                                   pCom->ComxInfo[wPort].wStopBits);
                          QstrncpyAlign (&pqszStrings[i][wInfoColumn],
                                        chBuffer,
                                        strlen (chBuffer),
                                        COM_INFO_COL_WIDTH);
                        }
                      else
                        {
                          wLength = strlen (psz1point5);
                          QstrncpyAlign (&pqszStrings[i][wInfoColumn],
                                        psz1point5,
                                        wLength,
                                        COM_INFO_COL_WIDTH);
                        }
                      break;
                    }

                  case COM_CARRIER_DETECT:
                    {
                      pszString = (pCom->ComxInfo[wPort].fCarrierDetect) ?
                                  pszYes : pszNo;

                      wLength = strlen (pszString);

                      QstrncpyAlign (&pqszStrings[i][wInfoColumn], pszString,
                                    wLength, COM_INFO_COL_WIDTH);
                      break;
                    }

                  case COM_RING_INDICATOR:
                    {
                      pszString = (pCom->ComxInfo[wPort].fRingIndicator) ?
                                  pszYes : pszNo;

                      wLength = strlen (pszString);

                      QstrncpyAlign (&pqszStrings[i][wInfoColumn], pszString,
                                    wLength, COM_INFO_COL_WIDTH);
                      break;
                    }

                  case COM_DATA_SET_READY:
                    {
                      pszString = (pCom->ComxInfo[wPort].fDataSetReady) ?
                                  pszYes : pszNo;

                      wLength = strlen (pszString);

                      QstrncpyAlign (&pqszStrings[i][wInfoColumn], pszString,
                                    wLength, COM_INFO_COL_WIDTH);
                      break;
                    }

                  case COM_CLEAR_TO_SEND:
                    {
                      pszString = (pCom->ComxInfo[wPort].fClearToSend) ?
                                  pszYes : pszNo;

                      wLength = strlen (pszString);

                      QstrncpyAlign (&pqszStrings[i][wInfoColumn], pszString,
                                    wLength, COM_INFO_COL_WIDTH);
                      break;
                    }

                  case COM_UART_CHIP:
                    {
                      wLength = strlen (pCom->ComxInfo[wPort].szUartChip);

                      QstrncpyAlign (&pqszStrings[i][wInfoColumn],
                                    pCom->ComxInfo[wPort].szUartChip,
                                    wLength, COM_INFO_COL_WIDTH);
                      break;
                    }
                }
            }
        }

      PrepNextString (pqszStrings, i);
    }

  /* Set the last pointer to a NULL */
  pqszStrings[i] = NULL;

  /* Return the pointer to pqszStrings */
  return (pqszStrings);
}
