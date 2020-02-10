/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * RPTINFO.C - Source file for reporting specific code.
 ********************************************************************/


/* Include Files */

#include "msd.h"


PSZ  pszHeaderLine = NULL; /* Header string */

/********************************************************************
 * ReportOnly - Generates an MSD report from the "/F" command line
 *              parameter.
 *
 * pszRptFilename - Name of file for "/F" report
 *
 * This routine will get and print each record, one record at a time.
 *
 * Returns: TRUE if an error condition occured.
 ********************************************************************/

BOOL ReportOnly (PSZ pszRptFilename)
{
  FILE *fileReportFile;         /* File handle for report file        */
  VOID *pStructForInfo = NULL;  /* Pointer to structure with data     */
  WORD wStructSize;             /* Stores memory requirements for     */
                                /*   GetInfo record                   */
  WORD wRecordType;             /* Record Type                        */
  QSZ  *pqszStrings;            /* Array of strings to print          */
  BOOL fReturnValue = FALSE;    /* Stores return value from GetInfo   */
  BOOL fFirstPass = TRUE;       /* Used for printing the first header */
  INT  i, u;                    /* Looping variables                  */
  INT  iLoopStart;              /* Starting value for loop            */
  INT  iLoopEnd;                /* Ending value for loop              */
  CHAR chBuffer[_MAX_PATH];     /* Local string                       */

  /* Open the report file */

  fileReportFile = OpenFile (pszRptFilename, pszOutput, TRUE);

  if (fileReportFile == NULL)
    return (TRUE);

  /* Set the buffer size to 0 so all characters will be written to */
  /*   the disk immediately.  This will help us determine where    */
  /*   MSD crashed, and will give us as much information as MSD    */
  /*   could provide before crashing.                              */

  /* setvbuf (fileReportFile, NULL, _IONBF, 0); */

  /* Set page, line, and column counts to 0 */

  wLineCount   = 0;
  wColumnCount = 0;
  wPageCount   = 0;

  if (fFirstPass && rgfReportItemFlag[IDI_CUSTOMER_INFORMATION] == FALSE)
    {
      fFirstPass = FALSE;

      /* Put on the initial page break */
      fReturnValue = WritePageBreak (fileReportFile);
      if (fReturnValue)
        return (fReturnValue);
    }

  /* If this is a /S summary only report, just print the summary */
  if (fSummaryOnly)
    {
      iLoopStart = IDI_SUMMARY_SCREEN;
      iLoopEnd   = IDI_SUMMARY_SCREEN + 1;
    }
  else
    {
      iLoopStart = 1;
      iLoopEnd   = NMBR_OF_RECORDS;
    }

  /* Report the information from the buttons */

  for (i = iLoopStart; i < iLoopEnd && !fReturnValue; ++i)
    {
#if HEAP_DEBUG
      sprintf (chBuffer, "i = %d", i);
      HeapCheck (chBuffer);
#endif
      /* Is the information available for this item */

      if (rgfReportItemFlag[i] == FALSE)
        continue;


      /* Determine the memory required to store the record */

      wStructSize = GetInfoSize (rwRecordTypes[i], 0);

      if (wStructSize == 0)
        continue;  /* NULL usually means I had an invalid record type */


      /* Allocate memory for the structure */

      pStructForInfo = malloc (wStructSize);

      if (pStructForInfo == NULL)
        {
          OutOfMemory();
          return (TRUE);
        }


      /* Zero out the structure */
      memset (pStructForInfo, '\0', wStructSize);


      /* Fill it with the GetInfo data */

      fReturnValue = GetInfo (rwRecordTypes[i],
                              pStructForInfo,
                              FALSE,    /* Do not obtain minimum info */
                              FALSE,    /* No include header record   */
                              TRUE);    /* We are doing a report      */
#if HEAP_DEBUG
      sprintf (chBuffer, "After GetInfo %d", i);
      HeapCheck (chBuffer);
#endif

      if (fReturnValue)
        {
          free (pStructForInfo);
          return (TRUE);
        }

      pqszStrings = SprintInfo (rwRecordTypes[i],
                                pStructForInfo,
                                NULL,
                                TRUE);  /* We are doing a report      */
#if HEAP_DEBUG
      sprintf (chBuffer, "After SprintInfo %d", i);
      HeapCheck (chBuffer);
#endif

      if (fFirstPass && !fSummaryOnly)
        {
          fFirstPass = FALSE;

          /* Put on the initial page break */
          fReturnValue = WritePageBreak (fileReportFile);
          if (fReturnValue)
            return (fReturnValue);
        }


      /* Write the strings to the report file */

      if (pqszStrings != NULL)
        if (fSummaryOnly)
          {
            /* Print out the summary information */

            for (u = 0; !fReturnValue && pqszStrings[u] != NULL; ++u)
              fReturnValue = WriteLine (pqszStrings[u], fileReportFile, TRUE);
          }
        else
          {
            wRecordType = rwRecordTypes[i];

            fReturnValue = WriteInfo (fileReportFile,
                                      paszButtonNames[wRecordType],
                                      pqszStrings);
          }
#if HEAP_DEBUG
      sprintf (chBuffer, "After WriteInfo %d", i);
      HeapCheck (chBuffer);
#endif

      /* Flush the report buffer */
      fflush (fileReportFile);

      /* Free up the allocated memory */
      free (pStructForInfo);
      FreeStringSpace (pqszStrings);
#if HEAP_DEBUG
      sprintf (chBuffer, "After three free's %d", i);
      HeapCheck (chBuffer);
#endif
    }

  /* If we're doing just the summary, then we're done */
  if (fSummaryOnly)
    return (fReturnValue);

  /* Report the Memory Browser information */

  if (rgfReportItemFlag[i] == TRUE && !fReturnValue)
    {
      fReturnValue = ReportBrowseInfo (fileReportFile);

      /* Flush the report buffer */
      fflush (fileReportFile);
    }

  /* Check to see if we are only printing one item */

  if (iLoopStart != iLoopEnd + 1)
    {
      /* Report the files (AUTOEXEC.BAT, etc) */

      for (i = RIF_START;
           i < MAX_REPORT_ITEM_FLAGS && !fReturnValue; ++i)
        {
          FILE_INFO FAR * ffi;    /* File Information structure        */
          FILE_INFO FAR * ffi2;   /* File Information structure (copy) */


          /* Is the information available for this item */

          if (rgfReportItemFlag[i] == FALSE)
            continue;

          ffi = FindFile (rgszSystemFiles[i - RIF_START], NULL,
                          rgwSystemFiles[i - RIF_START], '\0');
          ffi2 = ffi;

          while (ffi != NULL && ffi->fpNextFileInfo != NULL && !fReturnValue)
            {
              /* Put the filename into chBuffer */
              _fstrcpy (chBuffer, ffi->fpszPathToFile);

              /* Print the file on the report */
              fReturnValue = ReportFile (chBuffer, chBuffer, fileReportFile);

              /* Flush the report buffer */
              fflush (fileReportFile);

              /* Point to the next File info structure */
              ffi = (FILE_INFO FAR *) (ffi->fpNextFileInfo);
            }

          FreeFileInfo (ffi2);
        }
    }

  /* Send a form feed at the end of the document */
  if (fReturnValue == FALSE)
    fReturnValue = WriteChar ('\f', fileReportFile);

  CloseFile (fileReportFile);

  free (pszHeaderLine);

  if (fReportOnly && !fSummaryOnly)
    putch ('\n');

  return (fReturnValue);
}


#if CW_INCLUDED

/*********************************************************************
 * ReportFromCW - Generates a report from the CW version of MSD.
 *
 * Globals:
 * wReportToIndex - Index to paszReporTo.  == 7 if pszReportFilename
 *                  already contains a filename for the report.
 *********************************************************************/

BOOL ReportFromCW (VOID)
{
  BOOL fReturnValue;  /* Return value from ReportOnly() */


  /* Set the flag indicating that a report is underway */
  fReportFlag = TRUE;


  /* Report to the appropriate port/file */
  if (wReportToIndex == 7)
    {
      fReturnValue = ReportOnly (pszReportFilename);
      free (pszReportFilename);
    }
  else
    {
      fReturnValue = ReportOnly (paszReportTo[wReportToIndex]);
    }


  /* Set the flag indicating that the report is over */
  fReportFlag = FALSE;


  /* Inform the user that the report is finished */
  DisplayStatus (ST_REPORT_COMPLETED);


  return (fReturnValue);
}

#endif


/********************************************************************
 * WriteInfo - Writes a set of strings to the output file
 *
 * fileReportFile - File handle to write strings to
 * pszTitle       - Title of information being printed
 *                  (NULL for no separator line)
 * ppszStrings    - Array of strings to print
 *
 * Globals:
 *   wLineCount   - Used for determining the need for a page break
 *
 * Returns: TRUE if an error condition occured.
 ********************************************************************/

BOOL WriteInfo (FILE *fileOutput,
                PSZ  pszTitle,
                QSZ  *pqszStrings)
{
  BOOL fReturnValue = FALSE;  /* Return value from various functions    */
  INT  i;                     /* Index to pqszStrings, looping variable */
  WORD wNmbrLines = 0;        /* Number of lines in this string array   */
  WORD wLongestLine = 0;      /* The length of the longest line         */
  WORD wLength;               /* The length of the current line         */
  INT  iIndent;               /* Number of characters to indent on each */
                              /*   line, for centering purposes         */
  INT  iIndentCount;          /* Variable for outputting the indent     */

  /* Determine the number of lines and the length of the longest line */

  while (pqszStrings[wNmbrLines] != NULL)
    {
      if ((wLength = Qstrlen (pqszStrings[wNmbrLines++])) > wLongestLine)
        wLongestLine = wLength;
    }

  /* Determine the left indent required to center the text */

  if (wNmbrLines)
    {
      iIndent = (REPORT_WIDTH / 2) - (wLongestLine / 2) - 1;

      if (iIndent < 0)
        iIndent = 0;

      /* Account for the separator line */

      if (pszTitle != NULL && pszTitle[0] != '\0')
        wNmbrLines += 3;

      /* Determine if a page break is required */

      if (wNmbrLines + wLineCount > LINES_PER_PAGE)
        fReturnValue = WritePageBreak (fileOutput);

      /* Write the separator line, if requested */

      if (pszTitle != NULL && pszTitle[0] != '\0')
        fReturnValue = WriteSepLine (fileOutput, pszTitle);

      /* Write the strings */

      i = 0;

      while (!fReturnValue && pqszStrings[i] != NULL)
        {
          iIndentCount = iIndent;

          while (!fReturnValue && iIndentCount--)
            fReturnValue = WriteChar (' ', fileOutput);

          if (!fReturnValue)
            fReturnValue = WriteLine (pqszStrings[i++], fileOutput, TRUE);
        }

      /* Flush the buffer contents */

      if (!fReturnValue)
        {
          fReturnValue = fflush (fileOutput);

          /* Adjust the return value appropriately */

          if (fReturnValue == EOF)
            fReturnValue = TRUE;
          else
            fReturnValue = FALSE;
        }
    }

  return (fReturnValue);
}

/********************************************************************
 * WriteSepLine - Writes a separator line to the output file
 *
 * fileOutput - File handle to write strings to
 * pszTitle   - Title of record type being printed
 *              (NULL for no separator line)
 *
 * Returns: TRUE if an error condition occured.
 ********************************************************************/

BOOL WriteSepLine (FILE *fileOutput, PSZ pszTitle)
{
  BOOL fReturnValue;            /* Return value from Write___ functions  */
  CHAR chBuffer[REPORT_WIDTH];  /* Buffer for building output string     */
  INT  iLen;                    /* String length                         */
  INT  iDashCount;              /* Number of dashes to the left of title */
  INT  i;                       /* Index to chBuffer                     */

  /* Fill in all the dashes */

  memset (chBuffer, '-', REPORT_WIDTH - 1);

  /* Determine the length of the title */

  iLen = strlen (pszTitle);
  iDashCount = ((REPORT_WIDTH - iLen) / 2) - 2;
  i = iDashCount;

  /* Put in a leading space, the name, and the trailing space */

  chBuffer[i++] = ' ';

  strcpy (&chBuffer[i], pszTitle);

  i += iLen;
  chBuffer[i] = ' ';
  chBuffer[REPORT_WIDTH - 1] = '\0';

  /* Now, output the separator line */

  fReturnValue = WriteChar ('\n', fileOutput);
  if (fReturnValue)
    return (fReturnValue);

  fReturnValue = WriteLine (chBuffer, fileOutput, TRUE);
  if (fReturnValue)
    return (fReturnValue);

  fReturnValue = WriteChar ('\n', fileOutput);
  if (fReturnValue)
    return (fReturnValue);
}

/********************************************************************
 * WritePageBreak - Writes a separator line to the output file
 *
 * fileOutput - File handle to write strings to
 *
 * Returns: TRUE if an error condition occured.
 ********************************************************************/


BOOL WritePageBreak (FILE *fileOutput)
{
  static struct dosdate_t daDate;   /* Dos date structure               */
  static struct dostime_t tiTime;   /* Dos time structure               */
  BOOL fReturnValue = FALSE;        /* Return Value from Write___ funcs */
  INT  i;                           /* Looping variable                 */

  /* Create the header line only on the first page */

  if (wPageCount == 0)
    {
      ++wPageCount;

      /* Create the header line */
      pszHeaderLine = malloc (REPORT_WIDTH);

      if (pszHeaderLine == NULL)
        {
          OutOfMemory();
          return (TRUE);
        }

      /* Obtain the current date and time */

      _dos_getdate (&daDate);
      _dos_gettime (&tiTime);
    }

  if (fReportOnly)
    {
      CHAR chBuffer[20];
      WORD i = 0;

      /* Display the page number on the screen */
      sprintf (chBuffer, "\rPage %u", wPageCount);

      while (chBuffer[i])
        putch (chBuffer[i++]);
    }
#if CW_INCLUDED
  else
    {
      CHAR chBuffer[20];

      /* Display the page number on the status line */
      sprintf (chBuffer, "Page %u", wPageCount);
      ShowStatus (chBuffer);
    }
#endif


  /* Write the formfeed, followed by 2 linefeeds */

  if (wPageCount > 1)
    {
      fReturnValue = WriteChar ('\f', fileOutput);
      if (fReturnValue)
        return (fReturnValue);
    }

  for (i = 0; !fReturnValue && i < 2; ++i)
    fReturnValue = WriteChar ('\n', fileOutput);

  if (fReturnValue)
    return (fReturnValue);


  /* Create header string */

  sprintf (pszHeaderLine,
           pszHeaderFormatString,
           pszVersionNumber,
           daDate.month,
           daDate.day,
           daDate.year % 100,
           (tiTime.hour > 12) ? tiTime.hour-12 : tiTime.hour,
           tiTime.minute,
           (tiTime.hour > 12) ? "pm" : "am",
           wPageCount++);


  /* Print the header line */

  fReturnValue = WriteLine (pszHeaderLine, fileOutput, TRUE);
  if (fReturnValue)
    return (fReturnValue);


  /* Print the underline, followed by 1 linefeed */

  i = REPORT_WIDTH;

  while (!fReturnValue && i--)
    fReturnValue = WriteChar ('=', fileOutput);

  if (fReturnValue)
    return (fReturnValue);

  for (i = 0; !fReturnValue && i < 1; ++i)
    fReturnValue = WriteChar ('\n', fileOutput);

  if (fReturnValue)
    return (fReturnValue);

  return (FALSE);
}


/********************************************************************
 * ReportFile - Writes a set of strings to the output file
 *
 *
 * Returns: TRUE if an error condition occured.
 ********************************************************************/

BOOL ReportFile (PSZ  pszTitle,
                 PSZ  pszFilename,
                 FILE *fileOutput)
{
  BOOL fReturnValue = FALSE;  /* Return value from various functions    */
  FILE *fileInput;            /* Input file handle                      */
  CHAR chBuffer[80];          /* Input buffer                           */


  /* Determine if a page break is required */
  if (wLineCount + 6 > LINES_PER_PAGE)
    fReturnValue = WritePageBreak (fileOutput);


  /* Write the separator line, if requested */
  if (pszTitle != NULL && pszTitle[0] != '\0')
    fReturnValue = WriteSepLine (fileOutput, pszTitle);


  /* Open the file */
  fileInput = OpenFile (pszFilename, "rb", TRUE);
  if (fileInput == NULL)
    return (TRUE);


  /* Write the strings */
  while (fReturnValue == FALSE &&
         ReadLine (chBuffer, REPORT_WIDTH, fileInput, FALSE) != EOF)
    {
      fReturnValue = WriteLine (chBuffer, fileOutput, TRUE);
    }

  /* Flush the buffer contents */

  if (!fReturnValue)
    {
      if (fflush (fileOutput) == EOF)
        fReturnValue = TRUE;
      else
        fReturnValue = FALSE;
    }

  CloseFile (fileInput);

  return (fReturnValue);
}


#if CW_INCLUDED

/*********************************************************************
 * TestPrinter - Performs the printer test.
 *
 * fPostscript - TRUE if the Postscript test is requested
 * f8BitTest   - TRUE if the 8-bit ASCII test is requested
 * fSerialTest - TRUE if serial port, FALSE for parallel port
 * wPort       - Printer port number for the test
 *********************************************************************/

BOOL TestPrinter (BOOL fPostscript,
                  BOOL f8BitTest,
                  BOOL fSerialTest,
                  WORD wPort)
{
  VOID *pPortStruct = NULL;   /* Structure for storing printer port data */
  COM_STRUCT *pCom;           /* COM Port data struct pointer            */
  LPT_STRUCT *pLpt;           /* LPT Port data struct pointer            */
  WORD wSize;                 /* Size of the structure                   */
  WORD wReturnValue;          /* Return value from various functions     */
  CHAR szPort[5];             /* Port's filename                         */


  /* Set up the port's filename */
  if (fSerialTest)
    strcpy (szPort, "COM");
  else
    strcpy (szPort, "LPT");

  szPort[3] = (CHAR) (wPort + '1');
  szPort[4] = '\0';


  /* Obtain the appropriate port information */

  if (fSerialTest)
    wSize = GetInfoSize (IDI_COM_RECORD, FALSE);
  else
    wSize = GetInfoSize (IDI_LPT_RECORD, FALSE);


  if (wSize == 0)
    return (TRUE);

  pPortStruct = malloc (wSize);

  if (pPortStruct == NULL)
    {
      OutOfMemory();
      return (TRUE);
    }

  /* Zero out the structure */
  memset (pPortStruct, '\0', wSize);

  if (fSerialTest)
    wReturnValue = GetInfo (IDI_COM_RECORD, pPortStruct, FALSE, FALSE, FALSE);
  else
    wReturnValue = GetInfo (IDI_LPT_RECORD, pPortStruct, FALSE, FALSE, FALSE);

  if (wReturnValue)
    {
      free (pPortStruct);
      return (wReturnValue);
    }


  /* Is the port available */

  if (fSerialTest)
    {
      pCom = pPortStruct;

      if (pCom->ComxInfo[wPort].fComPortDetected == FALSE)
        {
          wReturnValue = MessageBox (szPort,
                                     "Was not detected.",
                                     "Do you wish to continue?",
                                     MB_YESNO | 0x8000 | MB_DEFBUTTON2);

          if (wReturnValue == IDNO)
            return (FALSE);
        }
    }
  else
    {
      pLpt = pPortStruct;

      if (pLpt->LptxInfo[wPort].fLptPortDetected == FALSE)
        {
          wReturnValue = MessageBox (szPort,
                                     "Was not detected.",
                                     "Do you wish to continue?",
                                     MB_YESNO | 0x8000 | MB_DEFBUTTON2);

          if (wReturnValue == IDNO)
            return (FALSE);
        }
    }


  /* Is the port ready?  Check while waiting about 0.5 seconds */

  if (fSerialTest == FALSE)
    {
      /* Point to the DOS timer click count */
      BYTE FAR * fpDosClick = (BYTE FAR *) 0x0040006C;
      WORD wClickCount   = 10;          /* Number of clicks to count    */
      BOOL fPrinterReady = FALSE;       /* TRUE when printer is ready   */
      BYTE bClickValue   = *fpDosClick; /* Current value of the counter */


      /* Port ready check */
      while (fPrinterReady == FALSE && wClickCount)
        {
          while (bClickValue == *fpDosClick &&
                 pLpt->LptxInfo[wPort].fLptPortDetected == FALSE)
            {
              GetInfo (IDI_LPT_RECORD, pLpt, FALSE, FALSE, FALSE);
            }

          /* Is the printer ready */
          if (pLpt->LptxInfo[wPort].fLptPortDetected == TRUE)
            {
              fPrinterReady = TRUE;
            }
          else
            {
              /* Reduce the number of click counts to check by 1 */
              --wClickCount;

              /* Set up to watch for the next click */
              bClickValue = *fpDosClick;
            }
        }


      /* Last chance */
      if (fPrinterReady == FALSE)
        {
          wReturnValue = MessageBox (szPort,
                                     "Does not appear to be ready",
                                     "Do you wish to continue?",
                                     MB_YESNO | 0x8000 | MB_DEFBUTTON2);

          if (wReturnValue == IDNO)
            return (FALSE);
        }
    }


  PrintTest (szPort, f8BitTest, fPostscript);

  return (FALSE);
}


/*********************************************************************
 * PrintTest - Prints the printer test to the specified port.
 *
 * pszPort - Filename of the printer port.
 * f8BitTest - TRUE if 8 bit test requested, FALSE for 7 bit test.
 * fPostscript - TRUE if Postscript codes required, FALSE for TTY.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL PrintTest (PSZ pszPort, BOOL f8BitTest, BOOL fPostscript)
{
  FILE *fpOut;        /* File handle for output                   */
  WORD i;             /* Looping variable                         */
  WORD wMaxChar;      /* Maximum character to print               */
  WORD wMaxCol;       /* Maximum character to print in column one */
  WORD wX;            /* X-Coordinate for postscript test         */
  WORD wY;            /* Y-Coordinate for postscript test         */
  static struct dosdate_t daDate;   /* Dos date structure         */
  static struct dostime_t tiTime;   /* Dos time structure         */
  BOOL fReturnValue;  /* Return value from various functions      */
  CHAR chBuffer1[80]; /* Buffer for printed line                  */
  CHAR chBuffer2[80]; /* Buffer for printed line                  */


  /* Open the file */
  fpOut = OpenFile (pszPort, "w", TRUE);
  if (fpOut == NULL)
    return (TRUE);


  /* Postscript Test */
  if (fPostscript)
    {
      /* Obtain the current date and time */
      _dos_getdate (&daDate);
      _dos_gettime (&tiTime);

      /* Output the header information */
      fReturnValue = _WriteLine (pszPostscriptTest1, fpOut);

      sprintf (chBuffer1, pszPostscriptTest2,
               daDate.month,
               daDate.day,
               daDate.year % 100,
               (tiTime.hour > 12) ? tiTime.hour-12 : tiTime.hour,
               tiTime.minute,
               (tiTime.hour > 12) ? "pm" : "am");

      fReturnValue = _WriteLine (chBuffer1, fpOut);

      fReturnValue = _WriteLine (pszPostscriptTest3, fpOut);

      /* Output the characters */
      wMaxChar = (f8BitTest) ? 255 : 127;
      wMaxCol  = ((wMaxChar - 32) / 6);

      wX = 100;
      wY = 600;

      for (i = 32; i <= wMaxChar && fReturnValue == FALSE; ++i)
        {
          sprintf (chBuffer1, "%d %d moveto (%d:) show %d %d moveto (\\%o) show\r\n",
                   wX, wY, i, wX + 30, wY, i);
          fReturnValue = _WriteLine (chBuffer1, fpOut);

          /* Move the Y value */
          wY -= 14;

          if (i >= wMaxCol + 32)
            {
              wMaxCol += ((wMaxChar - 32) / 6) + 1;
              wX      += 72;
              wY       = 600;
            }
        }

      _WriteLine ("showpage\r\n", fpOut);
    }
  else
    {
      /* Print the header */
      wLineCount   = 0;
      wColumnCount = 0;
      wPageCount   = 0;

      fReturnValue = WritePageBreak (fpOut);
      if (fReturnValue)
        return (fReturnValue);


      /* Output subheader information */
      for (i = 0; i < 2 && fReturnValue == FALSE; ++i)
        fReturnValue = WriteChar ('\n', fpOut);

      for (i = 0; i < 23 && fReturnValue == FALSE; ++i)
        fReturnValue = WriteChar (' ', fpOut);

      if (fReturnValue)
        return (fReturnValue);

      fReturnValue = WriteLine ("Standard Text Printer Test:\n", fpOut, TRUE);
      if (fReturnValue)
        return (fReturnValue);


      /* Output the characters */
      wMaxChar = (f8BitTest) ? 255 : 127;
      wMaxCol  = ((wMaxChar - 32) / 6) + 1;

      for (i = 32; i < wMaxCol + 32; ++i)
        {
          sprintf (chBuffer1, "           %3d: %c   %3d: %c   %3d: %c   %3d: %c   %3d: %c",
                   i,                 i,
                   i + wMaxCol,       i + wMaxCol,
                   i + (wMaxCol * 2), i + (wMaxCol * 2),
                   i + (wMaxCol * 3), i + (wMaxCol * 3),
                   i + (wMaxCol * 4), i + (wMaxCol * 4));

          if (i + (wMaxCol * 5) <= wMaxChar)
            {
              sprintf (chBuffer2, "   %3d: %c",
                       i + (wMaxCol * 5), i + (wMaxCol * 5));
            }
          else
            chBuffer2[0] = '\0';

          strcat (chBuffer1, chBuffer2);

          fReturnValue = WriteLine (chBuffer1, fpOut, TRUE);
          if (fReturnValue)
            return (fReturnValue);
        }

      fReturnValue = WriteChar ('\014', fpOut);
    }

  CloseFile (fpOut);

  return (fReturnValue);
}

#endif
