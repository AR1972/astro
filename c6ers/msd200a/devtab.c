/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * DEVTAB.C - Device Driver detection routines.
 *********************************************************************/


/* Include Files */

#include "msd.h"


/* Made global to this file.  This prevents filling the structure with */
/*   more device driver info than was previously allocated (on the     */
/*   extremely odd case of finding more device drivers when we         */
/*   actually get the driver info than when we counted up how many     */
/*   there were.  There is a way in DOS for an .EXE to install a       */
/*   device driver.  Under multitasking systems, the addition of a new */
/*   device driver while this program is running is a rare, though     */
/*   real possibility.                                                 */

WORD wDeviceDriverCount = 0;        /* The number of device drivers */
                                    /*   installed in memory        */

/*********************************************************************
 * GetDeviceDriverSize - Gets number of bytes required to store the
 *                       installed device driver's data.
 *
 * Returns:  The bytes required for the structure.
 *********************************************************************/

WORD GetDeviceDriversSize (VOID)
{
  CHAR FAR * fpDevHeader = NULL;          /* Pointer to the device header  */
  CHAR FAR * FAR * fpNextDevHeader = NULL;/* Pointer to next device header */

  wDeviceDriverCount = 0;  /* Zero out the driver count */

  /* Point to the first device driver */

  fpDevHeader = FindFirstDevHeader();

  /* Count device drivers until the header's offset is 0xFFFF */
  /*   (0xFFFF signifies the end of the list                  */

  while (FP_OFF (fpDevHeader) != 0xFFFF)
    {
      ++wDeviceDriverCount;

      /* Point to the next device driver */

      fpNextDevHeader = (CHAR FAR * FAR *) fpDevHeader;
      fpDevHeader     = *fpNextDevHeader;
    }

  /* Account for the zeroed out Device Driver record at the end */
  ++wDeviceDriverCount;

  /* Return the number of bytes required to store the structure */

  return (wDeviceDriverCount * sizeof (DEVICE_DRIVER_STRUCT));
}


/*********************************************************************
 * GetDeviceDriverInfo - Fills structure with installed device
 *                       driver's data.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetDeviceDriversInfo (DEVICE_DRIVER_STRUCT *pDevStruct,
                           BOOL fMinimumInfo)
{
  WORD wDevIndex;                     /* Index to pDevStruct             */
  WORD i;                             /* Looping variable                */
  CHAR FAR * fpDevHeader   = NULL;    /* Pointer to the device header    */
  CHAR FAR * fpDevFilename = NULL;    /* Pointer to the DEVICE= filename */
  WORD FAR * fwWordPointer = NULL;    /* WORD pointer to obtain WORD     */
                                      /*   values                        */
  CHAR FAR * FAR * fpNextDevHeader = NULL;
                                      /* Pointer to next device header   */


  /* There is no minimum info to return from this routine */

  if (fMinimumInfo)
    return (FALSE);


  /* Point to the first device driver */

  fpDevHeader = FindFirstDevHeader();

  /* Count device drivers until the header's offset is 0xFFFF */
  /*   (0xFFFF signifies the end of the list                  */

  for (wDevIndex = 0;
       wDevIndex < wDeviceDriverCount - 1 &&
       FP_OFF (fpDevHeader) != 0xFFFF;
       ++wDevIndex)
    {
      pDevStruct[wDevIndex].dwHeader = (DWORD) fpDevHeader;

      /* Set the WORD pointer */
      fwWordPointer = (WORD FAR *) fpDevHeader;

      pDevStruct[wDevIndex].wAttributes = fwWordPointer[2];

      /* Set the values for character or block devices */

      if (pDevStruct[wDevIndex].wAttributes & 0x8000)
        {
          /* Bit 15 is set, this is a character device */

          _fmemcpy ((CHAR FAR *) pDevStruct[wDevIndex].szDeviceName,
                    &fpDevHeader[10], 8);

          pDevStruct[wDevIndex].szDeviceName[8] = '\0';

          pDevStruct[wDevIndex].wUnits = 0;
        }
      else
        {
          /* Bit 15 is clear, this is a block device */

          strcpy (pDevStruct[wDevIndex].szDeviceName, pszBlockDevice);

          pDevStruct[wDevIndex].wUnits = (WORD) fpDevHeader[10];
        }


      /* Get the DEVICE= filename */

      fpDevFilename = (CHAR FAR *)
                          ((((DWORD) FP_SEG (fpDevHeader) - 1) << 16) +
                                     FP_OFF (fpDevHeader));

      /* Make sure all characters are printable */
      for (i = 8; i < 16 && fpDevFilename[i] >= ' '; ++i)
        ;

      /* Copy the DEVICE= filename to the structure if it's a Device (D) */
      /*   or an installable file system (I).                            */
      if (i == 16 && (fpDevFilename[0] == 'D') || (fpDevFilename[0] == 'I'))
        {
          _fmemcpy ((CHAR FAR *) pDevStruct[wDevIndex].szDriverFilename,
                    &fpDevFilename[8], 8);
          pDevStruct[wDevIndex].szDriverFilename[8] = '\0';
        }
      else
          pDevStruct[wDevIndex].szDriverFilename[0] = '\0';


      /* Point to the next device driver */

      fpNextDevHeader = (CHAR FAR * FAR *) fpDevHeader;
      fpDevHeader     = *fpNextDevHeader;
    }

  /* Set the last record to zeroes */
  pDevStruct[wDevIndex].dwHeader            = 0;
  pDevStruct[wDevIndex].wAttributes         = 0;
  pDevStruct[wDevIndex].szDeviceName[0]     = '\0';
  pDevStruct[wDevIndex].szDriverFilename[0] = '\0';
  pDevStruct[wDevIndex].wUnits              = 0;

  return (FALSE);
}


/*********************************************************************
 * SprintDeviceDriverInfo - Put installed device driver information
 *                          into a set of strings to be printed or
 *                          displayed.
 *
 * Returns:  NULL if an error occured.
 *********************************************************************/

QSZ * SprintDeviceDriverInfo (DEVICE_DRIVER_STRUCT *pDevStruct)
{
  WORD wNmbrStrings;        /* Number of strings                     */
  WORD wNmbrChars;          /* Number of characters in the strings   */
  WORD wUnderlineLength;    /* Length of the underline string        */
  WORD wDevIndex;           /* Index to the structure of device data */
  WORD i, u, x;             /* Looping variables                     */
  QSZ  *pqszStrings = NULL; /* Location for storing string pointers  */


  /* Calculate the amount of space required for the strings */

  wUnderlineLength = strlen (pszDeviceUnderline);

  wNmbrChars   = strlen (pszDeviceHeader) + 1 +
                 wUnderlineLength         + 1 +
                 (wDeviceDriverCount * (wUnderlineLength + 1));

  /* The underline string is expected to be as long as a line of */
  /*   device driver info.                                       */

  wNmbrStrings = wDeviceDriverCount + 3;

  /* "+ 3" is for the header line, the underline, and the NULL */
  /*   pointer at the end of the array.                        */


  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the first two strings in place */

  Qstrcpy (pqszStrings[0], pszDeviceHeader);
  pqszStrings[1] = pqszStrings[0] + Qstrlen (pqszStrings[0]) + 1;

  Qstrcpy (pqszStrings[1], pszDeviceUnderline);
  pqszStrings[2] = pqszStrings[1] + wUnderlineLength + 1;

  /* Put the device driver information in place */

  for (i = 2, wDevIndex = 0; pDevStruct[wDevIndex].dwHeader != 0;
       ++i, ++wDevIndex)
    {
      WORD wLength;       /* Current length of string */
      CHAR chBuffer[80];  /* Buffer for string data   */

      /* Fill the line with spaces */
      Qmemset (pqszStrings[i], ' ', wUnderlineLength);
      pqszStrings[i][wUnderlineLength] = '\0';


      /* Device Name */

      Qstrncpy (pqszStrings[i], pDevStruct[wDevIndex].szDeviceName,
               strlen (pDevStruct[wDevIndex].szDeviceName));


      /* Device Filename */

      Qstrncpy (&pqszStrings[i][DEV_FILENAME_COL],
              pDevStruct[wDevIndex].szDriverFilename,
              strlen (pDevStruct[wDevIndex].szDriverFilename));


      /* Units */

      if ((pDevStruct[wDevIndex].wAttributes & 0x8000) == 0)
        {
          wLength = sprintf (chBuffer, "% 3d", pDevStruct[wDevIndex].wUnits);

          Qstrncpy (&pqszStrings[i][DEV_UNITS_COL], chBuffer, wLength);
        }


      /* Header */

      wLength = sprintf (chBuffer, "%04X:%04X",
                         FP_SEG (pDevStruct[wDevIndex].dwHeader),
                         FP_OFF (pDevStruct[wDevIndex].dwHeader));

      Qstrncpy (&pqszStrings[i][DEV_HEADER_COL], chBuffer, wLength);


      /* Attributes */

      itoa (pDevStruct[wDevIndex].wAttributes, chBuffer, 2);

      wLength = strlen (chBuffer);

      /* Set the Attribute area to periods */
      Qmemset (&pqszStrings[i][DEV_ATTRIBUTE_COL], '.', 16);

      for (x = 0, u = DEV_ATTRIBUTE_COL + 16 - wLength;
           x < wLength; ++x, ++u)
        {
          if (chBuffer[x] == '1')
            pqszStrings[i][u] = '1';
        }

      /* Set the next pointer */
      PrepNextString (pqszStrings, i);
    }

  /* Set the last pointer to NULL */

  pqszStrings[i] = NULL;

  /* Return the pointer to pqszStrings */

  return (pqszStrings);
}


/*********************************************************************
 * FindFirstDevHeader - Finds the pointer to the list of DOS device
 *                      drivers.
 *
 * Returns:  Far pointer to the first DOS device driver (NUL).
 *********************************************************************/

CHAR FAR * FindFirstDevHeader (VOID)
{
  CHAR FAR *fpDevHeader = NULL;
  union REGS regsIn, regsOut;
  struct SREGS sregs;

  /* Get the address of the start of DOS' list of lists (ES:BX) */

  regsIn.h.ah = 0x52;
  int86x (0x21, &regsIn, &regsOut, &sregs);

  fpDevHeader = (VOID FAR *)
                  (((long)(sregs.es) << 16) + (long)(regsOut.x.bx));


  /* Add the correct offset for the version of DOS being run

       DOS Version     Offset
       -----------     ------
           3.0           28H
         3.1-3.3         22H
           4.x           22H
           5.x           22H
  */

  if (wDosMajor == 3 && wDosMinor == 0)
    fpDevHeader = fpDevHeader + 0x28;
  else
    fpDevHeader = fpDevHeader + 0x22;

  return (fpDevHeader);
}
