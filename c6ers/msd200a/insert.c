/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * INSERT.C - Source file for the Insert Command function.
 ********************************************************************/


/* Include Files */

#include "msd.h"

extern char EditBuf[256];

/*********************************************************************
 * InsertCommand - Performs the Insert Command function.
 ********************************************************************/

VOID InsertCommand (VOID)
{
  BOOL fSearchFlags;        /* Search flags for FindFile */
  FILE_INFO FAR *pfiNext;   /* File info structure       */
  CHAR chBuffer[80];        /* String buffer             */


  /* Bring up the dialog box */
  if (DialogBox (&dlgInsertCmd1, InsertCmdDlg1) != IDOK)
    return;


  /* Boot files are to be found on the boot drive */
  if (stricmp (pszInsertFilename, "CONFIG.SYS") == 0 ||
      stricmp (pszInsertFilename, "AUTOEXEC.BAT") == 0)
    {
      if (wDosMajor >= 4 && wDosMajor < 10)
        fSearchFlags = SEARCH_BOOT_DRIVE;
      else
        fSearchFlags = SEARCH_BOOT_DRIVE   |
                       SEARCH_FLOPPIES     |
                       SEARCH_LOCAL_DRIVES |
                       SEARCH_ROOT;
    }
  else
    fSearchFlags = SEARCH_LOCAL_DRIVES   |
                   SEARCH_ROOT           |
                   RECURSE_INTO_SUB_DIRS;


  if (strchr (pszInsertFilename, '\\') || strchr (pszInsertFilename, ':'))
    {
      /* A pathname was given */
      strcpy (EditBuf, pszInsertFilename);
    }
  else
    {
      /* Search for the file */
      sprintf (chBuffer, "Searching for %s ...", pszInsertFilename);
      ShowStatus (chBuffer);
      pfiDlg = FindFile (pszInsertFilename, NULL, fSearchFlags, '\0');
      PostMessage (pwndStatusLine, WM_PAINT, NULL, NULL);

      /* Were any files found */
      if (pfiDlg == NULL || pfiDlg->fpNextFileInfo == NULL)
        {
          MessageBox (pszInsertFilename, "Could not be found", NULL, MB_OK | 0x8000);
          return;
        }

      pfiNext = (FILE_INFO FAR *) pfiDlg->fpNextFileInfo;

      /* Give the user a choice if more than one file was found */
      if (pfiNext != NULL && pfiNext->fpNextFileInfo != NULL)
        {
          if (DialogBox (&dlgInsertCmd3, InsertCmdDlg3) != IDOK)
            return;
        }
      else
        {
          _fstrcpy (EditBuf, pfiDlg->fpszPathToFile);
          pszInsertFilename = EditBuf;
        }
    }

  /* Search for and add or replace strings as necessary. */
  /*   Actual code to accomplish this will be in the     */
  /*   dialog box code.                                  */

  DialogBox (&dlgInsertCmd4, InsertCmdDlg4);
  FreeFileInfo (pfiDlg);
}


/*********************************************************************
 * ChangeFile - Performs the requested change to the file.
 *
 * pszFilename     - Filename to change.
 * pszSection      - Section to change.
 * pszCommand      - Command to add or replace.
 * pszSearchString - String to search for.
 * wReplaceLine    - Line to replace.  HD_REPLACE_ALL to replace all
 *                   occurances of the search string.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL ChangeFile (PSZ  pszFilename,
                 PSZ  pszSection,
                 PSZ  pszCommand,
                 PSZ  pszSearchString,
                 WORD wReplaceLine)
{
  FILE *fpInsertFile;               /* File handle for the input file  */
  FILE *fpOutputFile;               /* File handle for the output file */
  CHAR szTempPathname[_MAX_PATH];   /* Path to .TMP file               */
  CHAR szBakPathname[_MAX_PATH];    /* Path to .BAK file               */
  INT  i;                           /* Looping variable                */
  BOOL fReturnValue;                /* Return value from various calls */


  /* Create temporary file */

  strcpy (szTempPathname, pszFilename);

  /* Clear out the original file's name */

  for (i = strlen (szTempPathname) - 1; i >= 0 &&
                                        szTempPathname[i] != '\\'; --i)
    ;

  szTempPathname[i + 1] = '\0';

  if (CreateTempFile (szTempPathname) != 0)
    {
      MessageBox, ("Error Creating File", szTempPathname, NULL,
                   MB_OK | 0x8000);
      return (TRUE);
    }


  if ((fpOutputFile = OpenFile (szTempPathname, "w", TRUE)) == NULL)
    return (TRUE);

  if ((fpInsertFile = OpenFile (pszFilename, "rb", TRUE)) == NULL)
    {
      CloseFile (fpOutputFile);
      return (TRUE);
    }

  if (wReplaceLine == 0)  /* Add Line */
    {
      if (pszSection[0] != '\0' &&
          FindSection (pszSection, fpInsertFile, fpOutputFile) == TRUE)
        {
          /* We are adding the section to the file */
          fReturnValue = OutputLine ("", fpOutputFile);
          if (fReturnValue)
            return (fReturnValue);

          fReturnValue = OutputLine (pszSection, fpOutputFile);
          if (fReturnValue)
            return (fReturnValue);
        }

      fReturnValue = OutputLine (pszCommand, fpOutputFile);
      if (fReturnValue)
        return (fReturnValue);

      WriteThrough (fpInsertFile, fpOutputFile);
    }
  /* Replace All */
  else if (wReplaceLine == HD_REPLACE_ALL)
    {
      if (pszSection[0] != '\0')
        FindSection (pszSection, fpInsertFile, fpOutputFile);

      HandleDuplicates ((WORD) HD_REPLACE_ALL, pszSearchString, pszCommand,
                        fpInsertFile, fpOutputFile, NULL);
    }
  else  /* Replace single line */
    {
      if (pszSection[0] != '\0')
        FindSection (pszSection, fpInsertFile, fpOutputFile);

      HandleDuplicates (wReplaceLine, pszSearchString,
                        pszCommand, fpInsertFile,
                        fpOutputFile, NULL);
    }

  /* Close both files */

  CloseFile (fpInsertFile);
  CloseFile (fpOutputFile);

  /* Delete .BAK file */

  strcpy (szBakPathname, pszFilename);

  /* Clear out the original file's extention, if it exists */

  for (i = strlen (szBakPathname) - 1; i >= 0 &&
                                       szBakPathname[i] != '.'; --i)
    ;

  if (i != -1)
    szBakPathname[i] = '\0';

  strcat (szBakPathname, ".BAK");

  DeleteFile (szBakPathname);

  /* Rename main file to .BAK */
  RenameFile (pszFilename, szBakPathname);

  /* Rename temp file to main filename */
  RenameFile (szTempPathname, pszFilename);

  return (FALSE);
}


/*********************************************************************
 * ReadCommands - Reads the "Insert Commands" commands from the
 *                MSD.INI file and adds them to the listbox pointed
 *                to by pwnd.
 *
 * pwnd - Listbox for storing the commands.
 ********************************************************************/

VOID ReadCommands (PWND pwnd)
{
  FILE *fp;   /* File handle to the MSD.INI file */
  CHAR szIniLine[MAX_INI_LINE_LENGTH + 1];  /* Input line from MSD.INI */
  WORD i;     /* Looping variable */


  /* Clear out the listbox */
  SendMessage (pwnd, LB_RESETCONTENT, NULL, NULL);

  /* Open the file */
  fp = OpenIniFile();

  if (fp != NULL && FindSection ("[Commands]", fp, NULL) == FALSE)
    {
      /* Read the lines from the MSD.INI file */
      while (ReadLine (szIniLine, MAX_INI_LINE_LENGTH, fp, FALSE) != EOF)
        {
          /* Break out if this is the end of the section */
          if (szIniLine[0] == '[')
            break;

          /* Ignore comment lines and blank lines */
          if (szIniLine[0] == ';' || szIniLine[0] == '\0')
            continue;

          /* Add the line to the list box */
          AddIniLine (pwnd, szIniLine);
        }

      CloseFile (fp);
    }
  else
    {
      /* Default MSD.INI file settings */
      for (i = 0; paszDefaultMsdIni[i] != NULL; ++i)
        AddIniLine (pwnd, paszDefaultMsdIni[i]);
    }

  return;
}


/*********************************************************************
 * FindSection - Finds the specified section entry in fpInsertFile.
 *               All information with fpInsertFile up to and including
 *               the section line are written to fpOutputFile.
 *
 * pszSection   - Section to search for.
 * fpInsertFile - File to read.
 * fpOutputFile - File to write.  NULL if no write action is
 *                necessary.
 *
 * Returns:  TRUE if an error occured, or the section was not found.
 *           FALSE if the section was found.
 *********************************************************************/

BOOL FindSection (PSZ  pszSection, FILE *fpInsertFile, FILE *fpOutputFile)
{
  CHAR chBuffer [MAX_INI_LINE_LENGTH + 1];  /* Character buffer - no line */
                                            /*   is assumed to be more    */
                                            /*   than MAX_LINE_LENGTH     */
                                            /*   bytes in length.         */
  WORD i1, i2;                              /* Looping variables          */

  /* If the file does not have a section entry, return FALSE */

  if (pszSection[0] == '\0')
    return (TRUE);

  /*************************/
  /* Find the section here */
  /*************************/

  /* Read the line */

  while (ReadLine (chBuffer, MAX_INI_LINE_LENGTH, fpInsertFile, FALSE) != EOF)
    {
      /* Write it out to the output line */

      if (fpOutputFile != NULL)
        if (OutputLine (chBuffer, fpOutputFile))
          return (TRUE);

      /* Skip whitespace at beginning of chBuffer */

      for (i1 = 0; i1 < MAX_INI_LINE_LENGTH && chBuffer[i1] != '\0' &&
                   (chBuffer[i1] == ' ' || chBuffer[i1] == '\t');  ++i1)
        ;

      /* Compare the two strings, up to the end of pszSection */

      for (i2 = 0; i1 < MAX_INI_LINE_LENGTH && chBuffer[i1] != '\0' &&
                   pszSection[i2] != '\0' &&
                   toupper (pszSection[i2]) == toupper (chBuffer[i1]);
                   ++i1, ++i2)
        ;

      /* Did we find a match -- ie, did we get to the end of pszSection */

      if (pszSection[i2] == '\0')
        return (FALSE);
    }

  /* Section was not found */

  return (TRUE);
}


/**********************************************************************
 * HandleDuplicates - Searches for, and optionally replaces command
 *                    strings within a file.  When replacing, writes
 *                    out the changed file.
 *
 * WORD wFunction - wFunction:
 *               HD_SEARCH  - Search for duplicate commands.  Add
 *                            duplicate commands to lbxList.
 *          HD_REPLACE_ALL  - Replace all matching commands with one
 *                            pszReplace string.  pszReplace will
 *                            replace first matching string.
 *                      0-n - Replace n occurance of search string
 *                            with pszReplace.
 * pszSearch      - String to search for.
 * pszReplace     - String to replace.
 * fpInfile       - Input file to search.
 * fpOutfile      - File to contain changes.
 * pwndList       - List box for storing matching strings.
 *
 * Returns: TRUE if an error occured.
 **********************************************************************/

BOOL HandleDuplicates (WORD wFunction,
                       PSZ  pszSearch,
                       PSZ  pszReplace,
                       FILE *fpInfile,
                       FILE *fpOutfile,
                       PWND pwndList)
{
  char chBuffer [MAX_INI_LINE_LENGTH + 1];  /* Character buffer - no line is */
                                        /*   assumed to be more than     */
                                        /*   MAX_INI_LINE_LENGTH bytes in    */
                                        /*   length.                     */
  char chBuffer2[MAX_INI_LINE_LENGTH + 1];  /* Used for adding "REPLACE " to */
                                        /*   the beginning of the line   */
                                        /*   in the list box.            */
  BOOL fMatch;                          /* Flag for match                */
  WORD wMatchCount = 0;                 /* Incriments for each match     */


  while (ReadLine (chBuffer, MAX_INI_LINE_LENGTH, fpInfile, FALSE) != EOF)
    {
      /* Set match flag */

      fMatch = HdMatch (pszSearch, chBuffer);

      switch (wFunction)
        {
          case HD_SEARCH: /* Search for duplicates */

            /* Does chBuffer match the search string */

            if (fMatch)
              {
                chBuffer2[0] = '\0';
                strcat (chBuffer2, "REPLACE ");
                strcat (chBuffer2, chBuffer);

                /* Add item to the listbox */
                SendMessage (pwndList, LB_ADDSTRING,
                             ((WORD) (isaNil) << 8) + TRUE,
                             (DWORD) ((CHAR FAR *) (chBuffer2)));
              }

            break;

          case HD_REPLACE_ALL: /* Replace all duplicates */

            /* Is this the first match */

            if (fMatch)
              if (++wMatchCount == 1)
                {
                  /* Replace first match with replace string */
                  if (OutputLine (pszReplace, fpOutfile))
                    return (TRUE);
                }

            /* If there was no match, output the line */

            if (!fMatch)
              if (OutputLine (chBuffer, fpOutfile))
                return (TRUE);

            break;

          default:  /* Replace wFunction duplicate line */

            /* Is this the correct matching line */

            if (fMatch && ++wMatchCount == wFunction)
              {
                /* Replace correct match with replace string */

                if (OutputLine (pszReplace, fpOutfile))
                  return (TRUE);

                /* Write out the rest of the file */
                WriteThrough (fpInfile, fpOutfile);
              }
            else
              if (OutputLine (chBuffer, fpOutfile))
                return (TRUE);

            break;
        }
    }

  return (FALSE);
}


/**********************************************************************
 * HdMatch - Match checking routine for HandleDuplicates
 *
 * pszSearch   - Search string to compare for matched strings.
 * pszFullLine - Line to compare to see if a match occured.
 *
 * Returns: TRUE if match occured, FALSE if not.
 **********************************************************************/

BOOL HdMatch (char *pszSearch, char *pszFullLine)
{
  WORD i;   /* Looping variable */

  for (i = 0; i < strlen (pszSearch) &&
              toupper (pszSearch[i]) == toupper (pszFullLine[i]); ++i)
    ;

  if (i == strlen (pszSearch))
    return (TRUE);
  else
    return (FALSE);
}


/*********************************************************************
 * OpenIniFile - Opens the MSD.INI file.
 *
 * Global:
 *   pszPathToProgram - Path to the .EXE file.
 *
 * Returns: File handle to MSD.INI.  NULL if an error occured.
 *********************************************************************/

FILE * OpenIniFile (VOID)
{
  CHAR szIniPathname[_MAX_PATH];      /* Used to store .INI filename */
  static WORD i;                      /* Looping variable */
  static PSZ  pszIniFilename = "MSD.INI";  /* .INI filename */


  /* Copy the pathname to MSD.EXE to szIniPathname */

  strcpy (szIniPathname, pszPathToProgram);

  /* Search for the first "\" (it should be a fully qualified path, */
  /*   as in "C:\WINDOWS\MSD.EXE".                                  */

  for (i = strlen (szIniPathname); i > 0 && szIniPathname[i] != '\\'; --i)
    ;

  if (i == 0) /* szIniPathname did not contain a fully qualified path */
    {
      /* Hope there is a MSD.INI file in the current directory */

      strcpy (szIniPathname, pszIniFilename);
    }
  else /* A fully qualified path was found */
    {
      /* Remove MSD.EXE */

      szIniPathname[++i] = '\0';

      /* Add MSD.INI */

      strcat (szIniPathname, pszIniFilename);
    }

  /* Open the .INI file */

  return (OpenFile (szIniPathname, "rb", FALSE));
}


/**********************************************************************
 * WriteThrough - Writes all characters through from input file to
 *                output file.
 *
 * fpInputFile  - Input file.
 * fpOutputFile - Output file.
 *
 * Returns: TRUE if an error occured.
 **********************************************************************/

BOOL WriteThrough (FILE *fpInputFile, FILE *fpOutputFile)
{
  CHAR chBuffer[256];         /* Input buffer                        */
  BOOL fReturnValue = FALSE;  /* Return value from various functions */

  while (fReturnValue == FALSE)
    {
      if (ReadLine (chBuffer, 255, fpInputFile, FALSE) == EOF)
        fReturnValue = TRUE;

      if (fReturnValue == FALSE)
        fReturnValue = OutputLine (chBuffer, fpOutputFile);
    }

  return (fReturnValue);
}


/**********************************************************************
 * AddIniLine - Adds a line to the list box
 *
 * pszString    - Comma delimited string to add to the list box.
 * plbxCommands - Listbox to fill.
 **********************************************************************/

VOID AddIniLine (PWND pwndList, PSZ pszString)
{
  static char chBuffer[80];      /* Stores expanded string */
  static int i, i1, i2;          /* Looping varaibles */
  static int fQuotedString;      /* Flag.  1 means this is a quoted string */
  static int aiColumns[] =  /* Locations of the columns in list box */
    {
      INI_COMMAND_COL,
      INI_SECTION_COL,
      INI_FILENAME_COL,
      79
    };


  /* Clear out chBuffer */

  memset (chBuffer, ' ', 79);

  chBuffer[80] = '\0';

  /* Start loop to fill in the various sections */

  for (i = 0; aiColumns[i] < 79; ++i)
    {

      /* Skip any white space at the beginning of the string */

      for (i1 = 0; pszString[i1] != '\0' && pszString[i1] != ',' &&
                  (pszString[i1] == '\t' || pszString[i1] == ' ');   ++i1)
        ;

      /* Is the first character a quote */

      if (pszString[i1] == '\"')
        {
          ++i1;
          fQuotedString = 1;
        }
      else
        fQuotedString = 0;

      /* Copy the command to chBuffer */

      for (i2 = aiColumns[i]; i2 < aiColumns[i + 1] &&
                              pszString[i1] != '\0' &&
                              ((fQuotedString && pszString[i1] != '\"') ||
                              (!fQuotedString && pszString[i1] != ','));
                              ++i1, ++i2)
        chBuffer[i2] = pszString[i1];

      /* Search for the next section */

      if (fQuotedString && pszString[i1] != '\0')
        {
          for (++i1; pszString[i1] != '\0' && pszString[i1] != ','; ++i1)
            ;
        }

      if (pszString[i1] != '\0')
        {
          /* Skip white space after the comma */

          for (++i1; pszString[i1] != '\0' && pszString[i1] != ',' &&
                    (pszString[i1] == '\t' || pszString[i1] == ' '); ++i1)
            ;
        }

      pszString += i1;
    }

  chBuffer[i2] = '\0';

  SendMessage (pwndList, LB_ADDSTRING, ((WORD) (isaNil) << 8) + TRUE,
               (DWORD) ((CHAR FAR *) (chBuffer)));
}
