;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
/**************************************************************************/
/*                                                                        */
/*  UTILITY NAME:      Label                                              */
/*                                                                        */
/*  SOURCE FILE NAME:  Label.C                                            */
/*                                                                        */
/*  STATUS:            Label Utility, DOS Version 5.00                    */
/*                                                                        */
/**************************************************************************/

#include <io.h>
#include <dos.h>
#include <stdlib.h>
#include <string.h>
#include "label.h"
#include "messages.h"


/*********************************************************************/
/* Routine: Main                                                     */
/*                                                                   */
/* Function:  First make sure we are running over the correct DOS    */
/*            version.  Then see if user wanted help text.  If so,   */
/*            display and exit.  Process the command line to see if  */
/*            the drive and label were given.  If drive was not      */
/*            given, get the current drive.  If no label was given,  */
/*            display the current label and then get new one from    */
/*            the user.  Then delete the old one and create the new. */
/*********************************************************************/

main(int argc, char *argv[])
{
  char ans[2];
  union REGS reg;                                /* M003 */

  _dos_setvect(0x23, ctrlc_hdlr);
  atexit(ExitLabel);

  /* First make sure we are running on a correct DOS version */
  if ((_osmajor < MAJOR_VERSION) || ((_osmajor == MAJOR_VERSION) && 
      (_osminor < MINOR_VERSION)))
  {
    DumbPrint(STDERR, msg_tbl[BAD_DOS_VERSION], LEN_UNKNOWN);
    DumbPrint(STDERR, newline, NEW_LEN);
    return (BAD_DOS_VERSION);
  }
  
  /* Check for "/?" on the command line. If found, output the options  */
  /* help message, and then exit. */
  if (CheckOptionsHelp(argc, argv))
    exit(OK);

  /* Process the command line, see what we've got & haven't got */
  process_commandline(argc, argv);
  
  /* If no drive was specified, get default drive */
  if (*Drive == '?')
    GetDrive();

  /* Save current directory and move to root directory - M003 */
  GetCurDir(reg, curdir);                        /* get current directory */
  if (curdir[0] != 0)                            /* root dir? */
  {
    *rootdir = *Drive;
    SetCurDir(reg, rootdir);                     /* change to root dir */
  }
   
  /* If no label was specified, show current one first and then */
  /* get new one interactively */
  if (*Label == EOL)
  {
    DisplayLabel();
    GetLabel();
  }
    
  /* If they entered an empty label (Carriage Return) then ask them if */
  /* they want to delete the existing volume label */
  if ((*Label == EOL) && (!Nolabel))
  {
    DumbPrint(STDOUT, newline, NEW_LEN);
    do
    {
      DumbPrint(STDOUT, msg_tbl[DEL_CUR_VOL], LEN_UNKNOWN);
      mygets(ans,2);
    } while ( ((*ans = (char)toupper(*ans)) != YES) && (*ans != NO) );
    
    if (toupper(*ans) == NO)
      exit(OK);                                  /* M003 */
  }

  /* Delete the old volume label */
  DeleteLabel();
  
  /* Create the new one, if there is one to create */
  if (*Label != '\0')
  {
    if (CreateLabel())
    {
      DumbPrint(STDOUT, newline, NEW_LEN);
      DumbPrint(STDOUT, msg_tbl[TOO_MANY_FILES], LEN_UNKNOWN);
      DumbPrint(STDOUT, newline, NEW_LEN);
      exit(TOO_MANY_FILES);                      /* M003 */
    }
  }
  exit(OK);                                      /* M003 */
}

  
/*********************************************************************/
/* Routine:   CheckOptionsHelp                                       */
/*                                                                   */
/* Function:  Checks for "/?" on the command line (any of the argv   */
/*            except argv[0]).  If found, display the options help   */
/*            message, and returns TRUE, so main() will know it      */
/*            should exit.                                           */
/*                                                                   */
/* Arguments: Receives argc and argv from main().                    */
/*            put the answer.                                        */
/*                                                                   */
/* Returns:   TRUE if /? found, FALSE otherwise.                     */
/*********************************************************************/

int CheckOptionsHelp(int c, char *v[])
{ 
  for (c--, v++; c; c--, v++)
  {
    if (v[0][0] == '/' && v[0][1] == '?')
    {
      DumbPrint(STDOUT, msg_tbl[HELP_MSG_1], LEN_UNKNOWN);
      DumbPrint(STDOUT, newline, NEW_LEN);
      DumbPrint(STDOUT, msg_tbl[HELP_MSG_2], LEN_UNKNOWN);
      return (TRUE);
    }
  }
  return (FALSE);		/* /? not found, no message */
}


/********************************************************************/
/* Routine:  Process_commandline                                    */
/*                                                                  */
/* Function: Go through the command line and pick off the drive and */
/*           label if these were given.  Note that because volume   */
/*           labels can have spaces, if the label was given on the  */
/*           command line, it would be spread out over argv[i] for  */
/*           more than one i.                                       */
/********************************************************************/

void process_commandline(int ac, char *av[])
{
  LoopIndex i;

  for (i = 1; i < ac; i++)
  {

#ifdef DBCS
    if (LegitDrive((av[i]) = DBCSstrupr(av[i])))
#else
    if (LegitDrive((av[i]) = strupr(av[i])))
#endif

    {
      if (*Drive == '?')
      {
        /* Save the drive letter */
        *Drive = *av[i];
      }
      else
      {    
        DumbPrint(STDERR, msg_tbl[MULTIPLE_DRIVE], LEN_UNKNOWN);
        DumbPrint(STDERR, newline, NEW_LEN);
        exit(MULTIPLE_DRIVE);
      }
      
      /* See if the drive letter is the first parameter */
      if (i != 1)
        DriveNotFirst = TRUE;

      /* See if the label is tacked right onto the drive letter. */
      /* Verify label if it is. */
      if (IllegitLabelWithDrive(av[i]))
        return;
    }
    else
    {
      if (IllegitimateLabel(av[i])) 
        return;
      else
        SaveLabel(av[i]);
    }
  }
  /*  M002
   *  Check for a double quote on the original command line.  The C
   *  initialization code will delete the quotes, so we must check the
   *  psp for them.  If any quotes are found, then an error should be
   *  returned.
   */
  if (CheckCmdLineQuotes())
  {
    strcpy(Label,"");
    DumbPrint(STDERR, msg_tbl[LABEL_SYNTAX_ERR], LEN_UNKNOWN);
    DumbPrint(STDERR, newline, NEW_LEN);
    return;
  }
}


/********************************************************************/
/* Routine:  DisplayLabel                                           */
/*                                                                  */
/* Function: Display the volume label and serial number (if any).   */
/*           First set the DTA to be global variable FCB, then do   */
/*           findfirst to find a file in the root with volume       */
/*           attribute.  We need the FCB-findfirst because the      */
/*           handle-based one returns the filename (i.e. vol label) */
/*           as eight characters - dot - three characters.          */
/********************************************************************/

void DisplayLabel()
{
  unsigned char serialbuf[26];
  union REGS reg;
  struct SREGS sreg;

  /* first set the dta to be fcb so information returned is put there */
  reg.x.ax = 0x1a00;
  reg.x.dx = (unsigned)fcb;
  intdos(&reg, &reg);

  /* now try to find the volume label */
  fcb[DRIVE_BYTE] = *Drive - 'A' + 1;
  reg.x.ax = 0x1100;
  reg.x.dx = (unsigned)fcb;
  intdos(&reg, &reg);
  if (reg.h.al)
  {
    DumbPrint(STDOUT, msg_tbl[VOL_IN_DRIVE], LEN_UNKNOWN);
    DumbPrint(STDOUT, Drive, 1);
    DumbPrint(STDOUT, msg_tbl[HAS_NO_LABEL], LEN_UNKNOWN);
    DumbPrint(STDOUT, newline, NEW_LEN);
  }
  else
  {
    Nolabel = FALSE;
    fcb[ENDNAME] = '\0';
    DumbPrint(STDOUT, msg_tbl[VOL_IN_DRIVE], LEN_UNKNOWN);
    DumbPrint(STDOUT, Drive, 1);
    DumbPrint(STDOUT, msg_tbl[IS], LEN_UNKNOWN);
    DumbPrint(STDOUT, &fcb[NAME], LEN_UNKNOWN);
    DumbPrint(STDOUT, newline, NEW_LEN);
  }

  /* Now print out the volume serial number, if it exists */
  segread(&sreg);
  reg.x.ax = 0x6900;
  reg.h.bl = *Drive - 'A' + 1;
  reg.x.dx = (unsigned)serialbuf;
  intdosx(&reg, &reg, &sreg);
  if (!reg.x.cflag)
  {
    DumbPrint(STDOUT, msg_tbl[VOL_SER], LEN_UNKNOWN);
    PrintHex(serialbuf[5]);
    PrintHex(serialbuf[4]);
    DumbPrint(STDOUT, msg_tbl[DASH], LEN_UNKNOWN);
    PrintHex(serialbuf[3]);
    PrintHex(serialbuf[2]);
    DumbPrint(STDOUT, newline, NEW_LEN);
  }
}


/********************************************************************/
/* Routine:  GetDrive                                               */
/*                                                                  */
/* Function: Get the current drive, and write the drive character   */
/*           into the global variable Drive                         */
/********************************************************************/

void GetDrive()
{
  unsigned driveno;

  _dos_getdrive(&driveno);
  Drive[0] = (char) driveno + 'A'- 1;
  LegitDrive(Drive);		     /* check for validity of drive */
}


/********************************************************************/
/* Routine:  GetLabel                                               */
/*                                                                  */
/* Function: Get the label from the user.  Spin in a loop until     */
/*           user enters a label with valid characters.  Then make  */
/*           sure it's not too long, & copy it into global variable */
/*           Label.                                                 */
/********************************************************************/

void GetLabel()
{ 
  char temp[MAX_LABEL_LENGTH+1]; 

  do
  {
    DumbPrint(STDERR, msg_tbl[ENTER_LABEL], LEN_UNKNOWN);
    mygets(temp,MAX_LABEL_LENGTH+1);
  } while (IllegitimateLabel(temp));

  strcpy(Label, temp);
}


/********************************************************************/
/* Routine: LegitDrive                                              */
/*                                                                  */
/* Function:  Make sure the drive in question is valid for labeling */
/*            purposes.  Therefore it has to be a valid drive (if   */
/*            you can open CON on a given drive, it is valid), but  */
/*            it cannot be a network drive, and it cannot be a      */
/*            drive which has been JOINed, SUBSTed, or ASSIGNed     */
/********************************************************************/

Boolean LegitDrive(char *s)
{
  char buf1[128], buf2[128];
  struct SREGS sreg;
  union REGS reg;

  /* Is this argument supposed to be a drive ? */
  if (s[1] != ':')
    return (FALSE);

  /* Save drive letter in temporary storage */
  *TempDrive = *s;

  /* Make sure this is a valid drive */
  con_fcb[DRIVE_BYTE] = *TempDrive - 'A' + 1;
  reg.x.ax = 0x0f00;
  reg.x.dx = (unsigned)con_fcb;
  intdos(&reg, &reg);
  if (reg.h.al)
  {
    DumbPrint(STDERR, msg_tbl[INVALID_DRIVE], LEN_UNKNOWN);
    DumbPrint(STDERR, newline, NEW_LEN);
    exit(INVALID_DRIVE);
  }
  reg.x.ax = 0x1000;
  reg.x.dx = (unsigned)con_fcb;
  intdos(&reg, &reg);			/* now close the file */

  /* Make sure user is not trying to label a network drive */
  reg.h.ah = IOCTL;
  reg.h.al = REMOTE;
  reg.h.bl = (char)(*TempDrive - 'A' + 1);
  intdos(&reg, &reg);
  if (reg.x.dx & BIT_12_MASK)
  {
    DumbPrint(STDERR, msg_tbl[REM_MEDIA], LEN_UNKNOWN);
    DumbPrint(STDERR, newline, NEW_LEN);
    exit(REM_MEDIA);                          
  }

  /* Make sure the user is not trying to label a drive which has */
  /* been ASSIGNed, JOINed, or SUBSTed                           */
  strcpy(buf1, TempDrive);
  strcat(buf1, "\\");
  segread(&sreg);
  reg.x.si = (unsigned)buf1;
  reg.x.di = (unsigned)buf2;
  reg.x.ax = 0x6000;
  intdosx(&reg, &reg, &sreg);
  if (*buf1 != *buf2)
  {
    DumbPrint(STDERR, msg_tbl[TRANSLATED_DRIVE], LEN_UNKNOWN);
    DumbPrint(STDERR, newline, NEW_LEN);
    exit(TRANSLATED_DRIVE);
  }

  return (TRUE);
}


/********************************************************************/
/* Routine: IllegitLabelWithDrive                                   */
/*                                                                  */
/* Function: See if the label given by the user is connected to the */
/*           drive letter (no space in between drive and label).    */
/*           If so, then check to see that the label given is       */
/*           legitimate.                                            */
/********************************************************************/

Boolean IllegitLabelWithDrive(char *s)
{
  if (s[2] != EOL)
  {
    if (IllegitimateLabel(&s[2]))
      return (TRUE);

    SaveLabel(&s[2]);
    return (FALSE);
  }
}


/********************************************************************/
/* Routine: SaveLabel                                               */
/*                                                                  */
/* Function: Saves the label in the global variable.  If it isn't   */
/*           the first entry, then it adds a space and then copies  */
/*           the label.                                             */
/*           Also, if the drive letter was NOT the first parameter  */
/*           and the label is not empty, then an error is returned. */
/*              e.g  label foo a: bar  => yields an error.          */
/********************************************************************/

void SaveLabel(char *string)
{
  if (firsttime)
    firsttime = FALSE;
  else
  {
    if (DriveNotFirst)
    {
      DumbPrint(STDERR, msg_tbl[INVALID_DRIVE_SYNTAX], LEN_UNKNOWN);
      DumbPrint(STDERR, newline, NEW_LEN);
      exit(INVALID_DRIVE_SYNTAX);
    }
    else
      strcat(Label, " ");
  }
  strcat(Label, string);
}


/********************************************************************/
/* Routine: IllegitimateLabel                                       */
/*                                                                  */
/* Function: Make sure the volume label given by the user is of the */
/*           appropriate format.  This means it must be less than   */
/*           twelve characters long, and none of the characters can */
/*           be any of those listed in the BadChars array (see file */
/*           defs.h)                                                */
/********************************************************************/

Boolean IllegitimateLabel(char *s)
{
  int length, LabelLen;
  LoopIndex i, j;

#ifdef DBCS
  s = DBCSstrupr(s);
#else
  s = strupr(s);
#endif

  /* Make sure label is not too long */
  length = strlen(s);
  LabelLen = strlen(Label);
  if ((length > MAX_LABEL_LENGTH) ||
      ((LabelLen > 0) && ((length + LabelLen + 1) > MAX_LABEL_LENGTH)))
  {
    strcpy(Label,"");
    DumbPrint(STDERR, msg_tbl[LABEL_TOO_LONG], LEN_UNKNOWN);
    DumbPrint(STDERR, newline, NEW_LEN);
    return (LABEL_TOO_LONG);
  }

#ifdef DBCS
  if (!CheckDBCSTailByte(s,s + length - 1) && IsDBCSLeadByte(s[length - 1]))
  {
    s[length - 1] = '\0';
    length--;
  }
#endif


  /* Make sure all characters are legitimate */
  for (i = 0; i < length; i++)
  {

#ifdef DBCS
    if (IsDBCSLeadByte(s[i]))
      i++;
    else
    {
#endif

      for (j = 0; j < BAD_CHARS; j++)
      {
        if (s[i] == BadChars[j])
        {
          strcpy(Label,"");
          DumbPrint(STDERR, msg_tbl[LABEL_SYNTAX_ERR], LEN_UNKNOWN);
          DumbPrint(STDERR, newline, NEW_LEN);
          return (LABEL_SYNTAX_ERR);
        }
      }

      if ((unsigned char)s[i] < (unsigned char)SPACEBAR)   /* M002 */
      {
        strcpy(Label,"");
        DumbPrint(STDERR, msg_tbl[LABEL_SYNTAX_ERR], LEN_UNKNOWN);
        DumbPrint(STDERR, newline, NEW_LEN);
        return (LABEL_SYNTAX_ERR);
      }

#ifdef DBCS
    }
#endif

  }
  return (FALSE);
}


/********************************************************************/
/* Routine:  DeleteLabel                                            */
/*                                                                  */
/* Function: Get rid of current volume label.  I had to go through  */
/*           intdos because remove/unlink didn't seem to work       */
/********************************************************************/

void DeleteLabel()
{
  char findstring[7];
  struct find_t  findstr;
  union REGS reg;

  strcpy(findstring, "?:\\*.*");
  *findstring = *Drive;
  if (!_dos_findfirst(findstring, _A_VOLID, &findstr))
  {
    strcpy(OldLabel, findstr.name);
    fcb[DRIVE_BYTE] = *Drive - 'A' + 1;
    reg.x.ax = 0x1300;
    reg.x.dx = (unsigned)fcb;
    intdos(&reg, &reg);
  }
}


/********************************************************************/
/* Routine:  CreateLabel                                            */
/*                                                                  */
/* Function: Create the new volume label file.  We need an FCB-type */
/*           create because the C runtime library function create   */
/*           truncates the name to eight characters.                */
/********************************************************************/

int CreateLabel()
{
  union REGS reg;
  LoopIndex i;
  int length;

  creat_fcb[DRIVE_BYTE] = *Drive - 'A' + 1;
  length = strlen(Label);
  for (i = 0; i < length; i++)
    creat_fcb[NAME + i] = Label[i];

  reg.x.ax = 0x1600;                             /* create the file */
  reg.x.dx = (unsigned)creat_fcb;
  intdos(&reg, &reg);

  if (!reg.h.al)
  {
    reg.x.ax = 0x1000;                           /* close the file */
    reg.x.dx = (unsigned)creat_fcb;
    intdos(&reg, &reg);
    return (OK);
  }
  else
    return (TOO_MANY_FILES);
}


/********************************************************************/
/* Routine: DumbPrint                                               */
/*                                                                  */
/* Function: Write text to STDOUT and STDERR.  The reason for not   */
/*           using fprintf is because it costs ~3k in executable    */
/*           (disk space is currently a priority.)   The handle and */
/*           string must be specified, but a length argument of     */
/*           zero means call strlen to find out the arg.  Requiring */
/*           a length arg means we can print out single chars and   */
/*           non-ASCIIZ strings as well as ASCIIZ strings.          */
/********************************************************************/

void DumbPrint(int hndle, char strng[], int len)
{
  union REGS reg;

  if (!len)
    len = strlen(strng);

  reg.h.ah = 0x40;
  reg.x.bx = hndle;
  reg.x.cx = len;
  reg.x.dx = (unsigned)strng;
  intdos(&reg, &reg);
}


/********************************************************************/
/* Routine: PrintHex                                                */
/*                                                                  */
/* Function: Print out a character (a byte) as two 4 bit hex values.*/
/*           Do this by converting each 4 bit number into its       */
/*           corresponding character, and using DumbPrint to print  */
/*           the value.                                             */
/********************************************************************/

void PrintHex(char c)
{
  unsigned char temp;

  /* First print high nibble */
  temp = c;
  temp = temp >> 4;
  if (temp < 10)
    temp += '0';
  else
    temp += ('A' - 10);
  DumbPrint(STDOUT, &temp, 1);

  /* Then print low nibble */
  temp = c & 0xf;
  if (temp < 10)
    temp += '0';
  else
    temp += ('A' - 10);
  DumbPrint(STDOUT, &temp, 1);
}


/********************************************************************/
/* Routine: mygets                                                  */
/*                                                                  */
/* Function: Simulates GETS taking care of EOF.                     */
/********************************************************************/

void mygets(char *buff, unsigned int length)
{
  char *sptr;
  int i;
  union REGS reg;                      /* structure for doing int21's with */

  if (input_redir == -1)
  {
    reg.h.ah = 0x44;
    reg.h.al = 0x0;                    /* get device info */
    reg.x.bx = 0x0;                    /* for STDIN */
    intdos(&reg, &reg);
    if (reg.x.dx & 0x80)
      input_redir = 0;
    else
      input_redir = 1;
  }

  if (input_redir)
  {
    reg.h.ah = 0xb;                    /* check input status */
    intdos(&reg,&reg);
    if (reg.h.al == 0)                 /* EOF ? */
    {
      puts(msg_tbl[UNEXP_EOF]);
      exit(UNEXP_EOF);                           /* M003 */
    }
  }

  /* else  - NOT EOF; go get input */

  getsbuf[0] = (char) length;          /* add 1 for CR */
  getsbuf[2] = 0;
  reg.x.dx = (unsigned) getsbuf;
  reg.h.ah = 0xa;                      /* buffered input from STDIN */
  intdos(&reg,&reg);

  puts("\r");

  sptr = &getsbuf[2];
  if (*sptr == EOFCHAR)
  {
    puts(msg_tbl[UNEXP_EOF]);
    exit(UNEXP_EOF);                             /* M003 */
  }
  for (i = 0; i < getsbuf[1]; i++)
    *buff++ = *sptr++;
  *buff = '\0';                        /* null terminate the string */
}


/********************************************************************/
/* Routine: CheckCmdLineQuotes                                      */
/*                                                                  */
/* Function: Check the original command line for any double quotes. */
/*           The C initialization code removes the double quotes.   */
/*           If any double quotes are entered on the original       */
/*           command line, then an error is returned.               */
/* M002                                                             */
/********************************************************************/

Boolean CheckCmdLineQuotes()
{
  union REGS reg;
  static unsigned char far *psp;

  /* first get the segment address of PSP */
  reg.x.ax = 0x6200;
  intdos(&reg, &reg);

  /* get address of original command line */
  FP_OFF(psp) = CMDLINE_OFFSET;
  FP_SEG(psp) = reg.x.bx;

  /* check for double quotes */
  for (; *psp != CARRIAGE_RETURN; psp++)
  {
    if (*psp == DBL_QUOTE)
      return (TRUE);              /* found a double quote */
  }
  return (FALSE);                 /* double quote NOT found */
}


/***************************************************************************/
/* Routine:  ExitLabel                                                     */
/*                                                                         */
/* Function: Exits the label utility by returning the user to the original */
/*           directory and returning the appropriate error code.           */
/* M003                                                                    */
/***************************************************************************/

void ExitLabel()
{
  union REGS reg;
  
  if (curdir[0] != 0)                        /* originally at root? */
    SetCurDir(reg, curdir);                  /* change to original dir */
}


/***************************************************************************/
/* Routine:  ctrlc_hdlr                                                    */
/*                                                                         */
/* Function: This is the ctrlc_handler routine.  It simply does an exit    */
/*           so that the "atexit" routine will be executed.                */
/* M003                                                                    */
/***************************************************************************/

void far _interrupt ctrlc_hdlr()
{
  exit(OK);
}


#ifdef DBCS

/********************************************************************/
/* Routine: DBCSstrupr                                              */
/*                                                                  */
/* Function: DBCS enabled strupr.                                   */
/********************************************************************/

unsigned char *DBCSstrupr(unsigned char *str)
{
  unsigned char *s;

  s = str;
  while (*s)
  {
    if (IsDBCSLeadByte(*s))
      s++;
    else
      *s = toupper(*s);
    s++;
  }
  return (str);
}


/********************************************************************/
/* Routine: IsDBCSLeadByte                                          */
/*                                                                  */
/* Function: Test if the character is DBCS lead byte.               */
/*                                                                  */
/*     input:  c = character to test                                */
/*     output: TRUE if leadbyte                                     */
/********************************************************************/

int IsDBCSLeadByte(unsigned char c)
{
  static unsigned char far *DBCSLeadByteTable = 0;

  union REGS inregs,outregs;
  struct SREGS segregs;
  unsigned char far *p;

  if (DBCSLeadByteTable == 0)
  {
    inregs.x.ax = 0x6300;              /* get DBCS lead byte table */
    intdosx(&inregs, &outregs, &segregs);
    FP_OFF(DBCSLeadByteTable) = outregs.x.si;
    FP_SEG(DBCSLeadByteTable) = segregs.ds;
  }

  p = DBCSLeadByteTable;
  while (p[0] || p[1])
  {
    if (c >= p[0] && c <= p[1])
      return (TRUE);
    p += 2;
  }
  return (FALSE);
}


/********************************************************************/
/* Routine: CheckDBCSTailByte                                       */
/*                                                                  */
/* Function: Check if the character point is at the tail byte.      */
/*                                                                  */
/*     input:  *str = strart pointer of the string                  */
/*             *point = character pointer to check                  */
/*     output: TRUE if at the tail byte                             */
/********************************************************************/

int CheckDBCSTailByte(unsigned char *str, unsigned char *point)
{
  unsigned char *p;

  p = point;
  while (p != str)
  {
    p--;
    if (!IsDBCSLeadByte(*p))
    {
      p++;
      break;
    }
  }
  return ((point - p) & 1 ? TRUE : FALSE);
}

#endif

