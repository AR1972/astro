/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * TSRLIST.C - TSR Program detection routines.
 *********************************************************************/


/* Include Files */

#include "msd.h"


/* Made global to this file.  This prevents filling the structure with */
/*   more TSR info than was previously allocated (on the extremely odd */
/*   case of finding more TSRs when we actually get the driver info    */
/*   than when we counted up how many there were.  Under multitasking  */
/*   systems, the addition of a new TSR installing while this program  */
/*   is running is a rare, though potential, possibility.              */

WORD wTsrCount = 0;        /* The number of TSRs installed in memory */


/*********************************************************************
 * GetTsrInfoSize - Gets number of bytes required to store the data
 *                  about the TSRs installed in the system.
 *
 * Returns:  The bytes required for the structure.
 *********************************************************************/

WORD GetTsrInfoSize (VOID)
{
  CHAR FAR * fpMcbHeader    = NULL;   /* Pointer to the MCB header         */
  CHAR FAR * fpDeviceMemory = NULL;   /* Pointer into device driver memory */
  DWORD      dwNextDeviceMemory = 0;  /* Pointer into next device driver   */
                                      /*   memory                          */
  DWORD      dwNextMcbHeader = 0;     /* Pointer to next MCB header        */
  WORD FAR * fwWordPointer  = NULL;   /* WORD pointer to obtain WORD data  */
                                      /*   from the MCB.                   */
  WORD wMcbParagraphs = 0;            /* Size of the MCB in paragraphs     */
  BOOL fEndOfList = FALSE;            /* TRUE when last MCB accounted for  */


  /* Zero out the TSR count */
  wTsrCount = 0;

  /* Add the UMB chain to the DOS MCB chain */
  LinkUmbToDosChain (TRUE);

  /* Point to the first MCB */

  fpMcbHeader = FindFirstMcbHeader();

  /* Count MCBs until the MCB type is 'Z'  */
  /*   ('Z' signifies the end of the list) */

  while (fEndOfList == FALSE)
    {
      ++wTsrCount;

      /* Get the size of the MCB */

      fwWordPointer  = (WORD FAR *) (fpMcbHeader + 1);
      wMcbParagraphs = fwWordPointer[1];

      /* Determine if this was the last MCB */

      if (*fpMcbHeader == 'Z')
        fEndOfList = TRUE;
      else
        {
          /* Determine if there are device drivers in this list */
          if (wDosMajor >= 4 && wDosMajor < 10 &&
              fpMcbHeader[8] == 'S' && fpMcbHeader[9] == 'D')
            {
              fpDeviceMemory = (CHAR FAR *)
                             ((((DWORD) FP_SEG (fpMcbHeader) + 1) << 16) + 0);

              /* Is this a valid DOS data area subsegment control block */
              while (strchr ("DEIFXCBLS", fpDeviceMemory[0]))
                {
                  /* If this is one we are interested in, */
                  /*   bump the TSR count                 */
                  /* if (fpDeviceMemory[0] == 'D' || fpDeviceMemory[0] == 'I') */
                    ++wTsrCount;

                  /* Point to the next device */
                  fwWordPointer      = (WORD FAR *) (fpDeviceMemory + 1);
                  dwNextDeviceMemory = *fwWordPointer;
                  fwWordPointer      = (WORD FAR *) (fpDeviceMemory + 3);
                  dwNextDeviceMemory += *fwWordPointer;
                  fpDeviceMemory     = (CHAR FAR *) (dwNextDeviceMemory << 16);
                }
            }

          /* Point to the next MCB */
          dwNextMcbHeader = (DWORD) fpMcbHeader +
                            ((DWORD) (wMcbParagraphs + 1) << 16);
          fpMcbHeader     = (CHAR FAR *) dwNextMcbHeader;
        }
    }

  /* Account for the zeroed out TSR record at the end of the struct */
  ++wTsrCount;

  /* Set the UMB/DOS MCB chain back to normal */
  LinkUmbToDosChain (FALSE);

  /* Return the number of bytes required to store the structure */
  return (wTsrCount * sizeof (TSR_PROGRAMS_STRUCT));
}


/*********************************************************************
 * GetTsrInfo - Fills the TSR_PROGRAMS_STRUCT with information about
 *              the TSR programs installed in the system.
 *
 * Returns:  The bytes required for the structure.
 *********************************************************************/

BOOL GetTsrInfo (TSR_PROGRAMS_STRUCT *pTsrStruct, BOOL fMinimumInfo)
{
  CHAR FAR * fpMcbHeader     = NULL;  /* Pointer to the MCB header         */
  DWORD      dwNextMcbHeader = 0;     /* Pointer to next MCB header        */
  CHAR FAR * fpPspAddress    = NULL;  /* Address of the MCB's PSP          */
  CHAR FAR * fpParentPspAddress = NULL; /* Address of parent's PSP         */
  CHAR FAR * fpTsrName       = NULL;  /* Points to TSR program's name      */
                                      /*   environment (for program name)  */
  CHAR FAR * fpCommandLine   = NULL;  /* Command line parameters           */
  WORD wMcbParagraphs        = 0;     /* Size of the MCB in paragraphs     */

  CHAR FAR * fpDeviceMemory  = NULL;  /* Pointer into device driver subMCB */
  CHAR FAR * fpDeviceStruct  = NULL;  /* Pointer to device driver itself   */
  DWORD      dwNextDeviceMemory = 0;  /* Pointer into next device driver   */
                                      /*   memory                          */
  WORD FAR * fwWordPointer   = NULL;  /* WORD pointer to obtain WORD data  */
                                      /*   from the MCB.                   */
  WORD i = 0;                         /* Looping variable                  */
  WORD wIndex;                        /* Index to structure                */
  BOOL fEndOfList = FALSE;            /* TRUE when last MCB accounted for  */
  WORD wCharCount = 0;                /* Number of characters in the       */
                                      /*   command line.                   */


  /* There is no minimum info to return from this routine */

  if (fMinimumInfo)
    return (FALSE);


  /* Add the UMB chain to the DOS MCB chain */
  LinkUmbToDosChain (TRUE);

  /* Point to the first MCB */
  fpMcbHeader = FindFirstMcbHeader();

  /* Count MCBs until the MCB type is 'Z'  */
  /*   ('Z' signifies the end of the list) */

  for (wIndex = 0; wIndex < wTsrCount - 1 && fEndOfList == FALSE; ++wIndex)
    {
      /* Store the address of the MCB */

      pTsrStruct[wIndex].wAddress = FP_SEG (fpMcbHeader);


      /* Get the size of the MCB */

      fwWordPointer  = (WORD FAR *) (fpMcbHeader + 1);
      wMcbParagraphs = fwWordPointer[1];

      pTsrStruct[wIndex].dwBlockSize = (DWORD) wMcbParagraphs << 4;

      /* Caluclate PSP and parent's PSP */

      fpPspAddress  = (CHAR FAR *) ((DWORD) fwWordPointer[0] << 16);
      fwWordPointer = (WORD FAR *) (fpPspAddress + 16);
      fpParentPspAddress = (CHAR FAR *) ((DWORD) *fwWordPointer << 16);


      /* Get the MCB's Owner's name */

      /* If the PSP Segment is 0000H it's free,     */
      /*   if it's 0008H, it's the DOS System Area. */

      if (FP_SEG (fpPspAddress) == 0x0000)
        {
          strcpy (pTsrStruct[wIndex].szTsrName, pszFreeMemory);
          pTsrStruct[wIndex].szParameters[0] = '\0';
        }
      else if (FP_SEG (fpPspAddress) == 0x0008)
        {
          /* Determine if it is the device driver area, etc */

          if (wDosMajor >= 4 && wDosMajor < 10)
            {
              /* "SC" is System Code / Locked out UMBs */
              if (wDosMajor >= 5 &&
                  fpMcbHeader[8] == 'S' && fpMcbHeader[9] == 'C')
                {
                  /* If the address + block size > A0000, it's */
                  /*   a locked out region.                    */

                  if (((DWORD) (pTsrStruct[wIndex].wAddress) << 4) +
                      pTsrStruct[wIndex].dwBlockSize > 0xA0000)
                    strcpy (pTsrStruct[wIndex].szTsrName, pszExcludedUmbArea);
                  else
                    strcpy (pTsrStruct[wIndex].szTsrName, pszDosSystemCode);
                  pTsrStruct[wIndex].szParameters[0] = '\0';
                }
              /* "SD" is System Data / Device Drivers, etc */
              else if (wDosMajor == 4 || (wDosMajor >= 5 &&
                       fpMcbHeader[8] == 'S' && fpMcbHeader[9] == 'D'))
                {
                  strcpy (pTsrStruct[wIndex].szTsrName, pszDosSystemData);
                  pTsrStruct[wIndex].szParameters[0] = '\0';

                  fpDeviceMemory = (CHAR FAR *)
                                 ((((DWORD) FP_SEG (fpMcbHeader) + 1) << 16) + 0);

                  /* Is this a valid DOS data area subsegment control block */
                  for (++wIndex;
                       strchr ("DEIFXCBLS", fpDeviceMemory[0]) &&
                       wIndex < wTsrCount - 1 && fEndOfList == FALSE;
                       ++wIndex)
                    {
                      pTsrStruct[wIndex].szParameters[0] = '\0';

                      switch (fpDeviceMemory[0])
                        {
                          case 'D':
                          case 'I':
                            /* If this is a device driver or installable */
                            /*   file system, put the device name and    */
                            /*   device filename into the structure.     */

                            /* Address */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 1);
                            pTsrStruct[wIndex].wAddress = *fwWordPointer;

                            /* Point to the Device Name */
                            fpDeviceStruct = (CHAR FAR *)
                                             (((DWORD) (*fwWordPointer)) << 16);

                            /* Size */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 3);
                            pTsrStruct[wIndex].dwBlockSize =
                                ((DWORD) (*fwWordPointer)) << 4;

                            /* Device Filename */
                            pTsrStruct[wIndex].szTsrName[0] = ' ';
                            pTsrStruct[wIndex].szTsrName[1] = ' ';
                            _fmemcpy (&(pTsrStruct[wIndex].szTsrName[2]),
                                      &fpDeviceMemory[8], 8);
                            pTsrStruct[wIndex].szTsrName[10] = '\0';

                            /* Device Name */
                            fwWordPointer = (WORD FAR *) (fpDeviceStruct + 4);
                            if (*fwWordPointer & 0x8000)
                              {
                                /* Character Device */

                                _fmemcpy (pTsrStruct[wIndex].szParameters,
                                          &fpDeviceStruct[10], 8);
                                pTsrStruct[wIndex].szParameters[8] = '\0';
                              }
                            else
                              {
                                /* Block Device */

                                strcpy (pTsrStruct[wIndex].szParameters,
                                        pszBlockDevice);
                              }

                            break;

                          case 'E':
                            strcpy (pTsrStruct[wIndex].szTsrName,
                                    pszDeviceAppenage);

                            /* Address */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 1);
                            pTsrStruct[wIndex].wAddress = *fwWordPointer;

                            /* Size */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 3);
                            pTsrStruct[wIndex].dwBlockSize =
                                ((DWORD) (*fwWordPointer)) << 4;

                            break;

                          case 'F':
                            strcpy (pTsrStruct[wIndex].szTsrName,
                                    pszFileHandles);

                            /* Address */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 1);
                            pTsrStruct[wIndex].wAddress = *fwWordPointer;

                            /* Size */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 3);
                            pTsrStruct[wIndex].dwBlockSize =
                                ((DWORD) (*fwWordPointer)) << 4;

                            break;

                          case 'X':
                            strcpy (pTsrStruct[wIndex].szTsrName,
                                    pszFCBS);

                            /* Address */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 1);
                            pTsrStruct[wIndex].wAddress = *fwWordPointer;

                            /* Size */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 3);
                            pTsrStruct[wIndex].dwBlockSize =
                                ((DWORD) (*fwWordPointer)) << 4;

                            break;

                          case 'C':
                          case 'B':
                            strcpy (pTsrStruct[wIndex].szTsrName,
                                    pszBuffers);

                            /* Address */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 1);
                            pTsrStruct[wIndex].wAddress = *fwWordPointer;

                            /* Size */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 3);
                            pTsrStruct[wIndex].dwBlockSize =
                                ((DWORD) (*fwWordPointer)) << 4;

                            break;

                          case 'L':
                            strcpy (pTsrStruct[wIndex].szTsrName,
                                    pszDirectories);

                            /* Address */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 1);
                            pTsrStruct[wIndex].wAddress = *fwWordPointer;

                            /* Size */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 3);
                            pTsrStruct[wIndex].dwBlockSize =
                                ((DWORD) (*fwWordPointer)) << 4;

                            break;

                          case 'S':
                            strcpy (pTsrStruct[wIndex].szTsrName,
                                    pszStacksArea);

                            /* Address */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 1);
                            pTsrStruct[wIndex].wAddress = *fwWordPointer;

                            /* Size */
                            fwWordPointer = (WORD FAR *) (fpDeviceMemory + 3);
                            pTsrStruct[wIndex].dwBlockSize =
                                ((DWORD) (*fwWordPointer)) << 4;

                            break;
                        }

                      /* Point to the next device */
                      fwWordPointer      = (WORD FAR *) (fpDeviceMemory + 1);
                      dwNextDeviceMemory = *fwWordPointer;
                      fwWordPointer      = (WORD FAR *) (fpDeviceMemory + 3);
                      dwNextDeviceMemory += *fwWordPointer;
                      fpDeviceMemory     = (CHAR FAR *) (dwNextDeviceMemory << 16);
                    }
                  --wIndex;
                }
              /* We'll call this the DOS System Area, otherwise */
              else
                {
                  strcpy (pTsrStruct[wIndex].szTsrName, pszDosSystemArea);
                  pTsrStruct[wIndex].szParameters[0] = '\0';
                }
            }
          else
            {
              strcpy (pTsrStruct[wIndex].szTsrName, pszDosSystemArea);
              pTsrStruct[wIndex].szParameters[0] = '\0';
            }
        }
      else
        {
          /* If the PSP == the parent's PSP, it's COMMAND.COM */

          if (fpPspAddress == fpParentPspAddress)
            strcpy (pTsrStruct[wIndex].szTsrName, pszCommandCom);
          else
            {
              /* Otherwise, we have to find the name.  First, search */
              /*   in the environment area.                          */

              CHAR chEnvironName[80];   /* Buffer for storing what the */
                                        /*   environment claims the is */
                                        /*   the name of the TSR       */

              fwWordPointer = (WORD FAR *) (fpPspAddress + 0x002C);
              fpTsrName = (CHAR FAR *) ((DWORD) *fwWordPointer << 16);

              /* The name of the program that owns this MCB is located */
              /*   2 bytes after 2 zero bytes and a word count.  Loop  */
              /*   through the environment block until 2 back-to-back  */
              /*   zero bytes are located.                             */

              while (!(fpTsrName[0] == 0 && fpTsrName[1] == 0))
                ++fpTsrName;

              fpTsrName += 4;

              /* fpTsrName is now pointing to the fully qualified path    */
              /*   to the program name.  Parse for just the program name. */

              for (i = 0; i < MAX_TSR_NAME - 1 &&
                          *fpTsrName > ' '    &&
                          *fpTsrName < 127;      ++fpTsrName)
                {
                  if (*fpTsrName == '\\')
                    i = 0;
                  else
                    chEnvironName[i++] = *fpTsrName;
                }

              /* If this was the real name, it should end with a zero   */
              /*   byte.  If it was garbage, we should have dropped out */
              /*   because fpTsrName pointed to something other than a  */
              /*   zero byte, or the length of the string is zero.      */
              if (*fpTsrName == '\0')
                chEnvironName[i] = '\0';
              else
                chEnvironName[0] = '\0';

              /* The name from the environment is now ready.  Next,     */
              /*   get the MCB owner (DOS 4.0 and above).  If the MCB   */
              /*   owner is the same as the beginning of the name from  */
              /*   the environment, use chEnvironName.  Otherwise, use  */
              /*   the MCB name.                                        */

              if (wDosMajor < 4)
                {
                  /* If there is no name, put in "???" */
                  if (strlen (chEnvironName) == 0)
                    strcpy (pTsrStruct[wIndex].szTsrName, "???");
                  else
                    strcpy (pTsrStruct[wIndex].szTsrName, chEnvironName);
                }
              else
                {
                  /* Obtain the program name from the MCB */

                  fpTsrName = fpMcbHeader + 8;

                  for (i = 0; i < 8             &&
                              *fpTsrName >  ' ' &&
                              *fpTsrName <  127 &&
                              *fpTsrName == (CHAR) toupper (*fpTsrName);
                              ++i, ++fpTsrName)
                    {
                      pTsrStruct[wIndex].szTsrName[i] = *fpTsrName;
                    }

                  pTsrStruct[wIndex].szTsrName[i] = '\0';

                  /* Determine if this is a valid program name.  If we */
                  /*   have not taken 8 characters from the MCB, we    */
                  /*   should have run into a zero byte (the name is   */
                  /*   null terminated if there are less than 8        */
                  /*   characters).                                    */

                  if (i < 8 && (*fpTsrName != '\0' ||
                      pTsrStruct[wIndex].szTsrName[0] == '\0'))
                    {
                      /* Use the environment name */
                      strcpy (pTsrStruct[wIndex].szTsrName, chEnvironName);
                    }

                  /* The MCB contains a valid name.  Check to see if the */
                  /*   environment contains a more complete name (ie,    */
                  /*   the environment contains an extention, the MCB    */
                  /*   does not.                                         */

                  else if (strnicmp (chEnvironName,
                                     pTsrStruct[wIndex].szTsrName,
                                     strlen (pTsrStruct[wIndex].szTsrName)) == 0)
                    {
                      strcpy (pTsrStruct[wIndex].szTsrName, chEnvironName);
                    }

                  if (strnicmp (pszCommand, pTsrStruct[wIndex].szTsrName,
                                strlen (pszCommand)) == 0)
                    {
                      /* The word "command" was found, put COMMAND.COM */
                      /*   into the TSR Name field.                    */

                      strcpy (pTsrStruct[wIndex].szTsrName, pszCommandCom);
                    }

                  if (pTsrStruct[wIndex].szTsrName[0] == '\0')
                    {
                      /* A valid program name could not be found */

                      strcpy (pTsrStruct[wIndex].szTsrName, "???");
                    }
                }
            }

          /* Get the command line parameters */

          fpCommandLine = fpPspAddress + 0x0080;

          /* The first byte is the number of characters in the */
          /*   command line.                                   */

          wCharCount = fpCommandLine[0];
          if (wCharCount > MAX_TSR_CMD_LINE - 1)
            wCharCount = MAX_TSR_CMD_LINE - 1;

          ++fpCommandLine;

          for (i = 0; i < wCharCount &&
                      fpCommandLine[i] >= ' ' &&
                      fpCommandLine[i] <= 127;   ++i)
            pTsrStruct[wIndex].szParameters[i] = fpCommandLine[i];

          pTsrStruct[wIndex].szParameters[i] = '\0';
        }


      /* Determine if this was the last MCB */

      if (*fpMcbHeader == 'Z')
        fEndOfList = TRUE;
      else
        {
          /* Point to the next MCB */

          dwNextMcbHeader = (DWORD) fpMcbHeader +
                            ((DWORD) (wMcbParagraphs + 1) << 16);
          fpMcbHeader     = (CHAR FAR *) dwNextMcbHeader;
        }
    }

  /* Set the last record to zeroes */
  pTsrStruct[wIndex].wAddress        = 0;
  pTsrStruct[wIndex].dwBlockSize     = 0;
  pTsrStruct[wIndex].szTsrName[0]    = '\0';
  pTsrStruct[wIndex].szParameters[0] = '\0';

  /* Set the UMB/DOS MCB chain back to normal */
  LinkUmbToDosChain (FALSE);

  return (FALSE);
}


/*********************************************************************
 * SprintTsrInfo - Put TSR program information into a set of strings
 *                 to be printed or displayed.
 * fIncludeParms - TRUE if command line parameters are to be included.
 *
 * Returns:  NULL if an error occured.
 *********************************************************************/

QSZ * SprintTsrInfo (TSR_PROGRAMS_STRUCT *pTsrStruct,
                     BOOL fIncludeParms)
{
  WORD wNmbrStrings;        /* Number of strings                     */
  WORD wNmbrChars;          /* Number of characters in the strings   */
  WORD wUnderlineLength;    /* Length of the underline string        */
  WORD wIndex;              /* Index to the structure of TSR data    */
  WORD i;                   /* Looping variable                      */
  QSZ  *pqszStrings = NULL; /* Location for storing string pointers  */


  /* Calculate the amount of space required for the strings */

  wUnderlineLength = strlen (pszTsrUnderline);

  if (fIncludeParms)
    {
      wNmbrChars   = strlen (pszTsrHeader) + 1 +
                     wUnderlineLength      + 1 +
                     (wTsrCount * (wUnderlineLength + 1));

      if (wNmbrChars > _memmax() - 100)
        fIncludeParms = FALSE;
    }

  if (fIncludeParms == FALSE)
    wNmbrChars   = strlen (pszTsrHeader) + 1 +
                   wUnderlineLength      + 1 +
                   (wTsrCount * (TSR_CMD_LINE_COL + 1));

  /* The underline string is expected to be as long as a line of */
  /*   TSR program info.                                         */

  wNmbrStrings = wTsrCount + 3;

  /* "+ 3" is for the header line, the underline, and the NULL */
  /*   pointer at the end of the array.                        */


  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);

  /* Put the first two strings in place */

  Qstrcpy (pqszStrings[0], pszTsrHeader);
  pqszStrings[1] = pqszStrings[0] + Qstrlen (pqszStrings[0]) + 1;

  Qstrcpy (pqszStrings[1], pszTsrUnderline);
  pqszStrings[2] = pqszStrings[1] + wUnderlineLength + 1;

  /* Put the TSR information in place */

  for (i = 2, wIndex = 0;
       !(pTsrStruct[wIndex].wAddress  == 0 &&
       pTsrStruct[wIndex].dwBlockSize == 0);
       ++i, ++wIndex)
    {
      WORD wLength;       /* Current length of string */
      CHAR chBuffer[80];  /* Buffer for string data   */

      /* Fill the line with spaces */
      Qmemset (pqszStrings[i], ' ', wUnderlineLength);
      pqszStrings[i][wUnderlineLength] = '\0';


      /* TSR Name */

      Qstrcpy (pqszStrings[i], pTsrStruct[wIndex].szTsrName);

      wLength    = Qstrlen (pqszStrings[i]);
      pqszStrings[i][wLength] = ' ';


      /* Size */

      wLength = sprintf (chBuffer, "%6lu",
                         pTsrStruct[wIndex].dwBlockSize);

      Qstrcpy (&pqszStrings[i][TSR_SIZE_COL], chBuffer);

      pqszStrings[i][TSR_SIZE_COL + wLength] = ' ';


      /* Address */

      wLength = sprintf (chBuffer, "%04X", pTsrStruct[wIndex].wAddress);

      Qstrcpy (&pqszStrings[i][TSR_ADDRESS_COL], chBuffer);

      pqszStrings[i][TSR_ADDRESS_COL + wLength] = ' ';


      /* Command line parameters */

      if (fIncludeParms)
        Qstrcpy (&pqszStrings[i][TSR_CMD_LINE_COL],
                pTsrStruct[wIndex].szParameters);


      /* Set the next pointer */
      PrepNextString (pqszStrings, i);
    }

  /* Set the last pointer to NULL */

  pqszStrings[i] = NULL;

  /* Return the pointer to pqszStrings */

  return (pqszStrings);
}


/*********************************************************************
 * FindFirstMcbHeader - Finds the pointer to the first memory control
 *                      block (MCB).
 *
 * Returns:  Far pointer to the first MCB.
 *********************************************************************/

CHAR FAR * FindFirstMcbHeader (VOID)
{
  union REGS regsIn, regsOut;   /* Register structures for int86x call */
  struct SREGS sregs;           /* Segment registers for int86x call   */
  WORD wMcbSegment        = 0;  /* Segment of first MCB                */
  WORD FAR *fwWordPointer = NULL; /* A far pointer to a WORD           */

  /* Get the address of the start of DOS' list of lists (ES:BX) */

  regsIn.h.ah = 0x52;
  int86x (0x21, &regsIn, &regsOut, &sregs);

  /* Get first MCB Segment */

  fwWordPointer = (WORD FAR *)
                  ((((long) (sregs.es) << 16) + (long) (regsOut.x.bx)) - 2);

  wMcbSegment = *fwWordPointer;

  /* Return pointer to the first MCB */

  return ((CHAR FAR *) ((DWORD) wMcbSegment << 16));
}


/*********************************************************************
 * LinkUmbToDosChain - Links or unlinks UMBs to the DOS memory control
 *                     block chain.
 * fLinkFlag - TRUE  - Link UMBs to the DOS MCB chain.
 *             FALSE - Set the chain back to the default.
 *********************************************************************/

VOID LinkUmbToDosChain (BOOL fLinkFlag)
{
  union REGS regsin, regsout;         /* Register structs for int86 call   */
  static BOOL fDefaultLink = 0xFFFF;  /* Set to DOS Default of UMBs linked */
                                      /*   or unlinked                     */

  /* Are we running under a valid version of DOS */
  if (wDosMajor >= 5 && wDosMajor < 10)
    {
      /* Set the default, if necessary */
      if (fDefaultLink == 0xFFFF)
        {
          regsin.x.ax = 0x5802;
          int86 (0x21, &regsin, &regsout);

          if (regsout.x.cflag == 0)
            fDefaultLink = (BOOL) regsout.h.al;
          else
            return;
        }

      /* Now, set the link as per the request */
      regsin.x.ax = 0x5803;

      if (fLinkFlag)
        {
          /* Link the UMB chain to the DOS MCB chain */
          regsin.x.bx = 0x0001;
          int86 (0x21, &regsin, &regsout);
        }
      else
        {
          /* Set the UMB chain link to the default state */
          regsin.x.bx = fDefaultLink;
          int86 (0x21, &regsin, &regsout);
        }
    }
}
