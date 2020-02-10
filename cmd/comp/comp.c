/*************************************************************************/
/*                                                                       */
/*  FILE:    Comp.C                                                      */
/*                                                                       */
/*  SYNTAX:  comp <filespec1> <filespec2> </d> </a> </l> </n=xxxx> </c>  */
/*                                                                       */
/*  PURPOSE: Compares two files and displays the first 10 differences    */
/*           between them.  The parameters filespec1 and filespec2 can   */
/*           each have wildcards, in which case comp will compare sets   */
/*           of files, two at a time.  Normally the differences print    */
/*           out as hex bytes; options affect them as follows:           */
/*              /d  -  print out as decimal (ints)                       */
/*              /a  -  as (ascii) characters                             */
/*              /l  -  print the line number of the mismatch             */
/*                     (rather than the byte offset)                     */
/*              /n with number  -  only the first arg # of lines         */
/*                                 get compared                          */
/*              /c  -  do a case-insensitive compare.                    */
/*                                                                       */
/*  LABEL:   Microsoft Confidential                                      */
/*           Copyright (c) Microsoft Corporation 1991                    */
/*           All Rights Reserved.                                        */
/*                                                                       */
/*************************************************************************/


#include <dos.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <ctype.h>
#include <string.h>
#include <io.h>
#include "version.h"
#include "comp.h"
#include "messages.h"


/**************************************************************************/
/* First check if the user wanted help; if so, print out help text and    */
/* quit, even if parameters other than '?' were passed to comp.  Next we  */
/* call getmem to retrieve enough memory for the two buffers which will   */
/* hold the two files being compared.  Now we call ppargs, passing it the */
/* argument vector (and count.)  Ppargs insures that both file            */
/* specifications are legitimate, by checking  to see that they are not   */
/* empty (if they are, prompt the user) & that they contain a full file   */
/* specification (i.e. not just a drive letter, or directory designation.)*/
/* If file specifications needed to be gotten interactively, ppargs also  */
/* asks the user for options.  PPargs also checks the given switches one  */
/* one as it encounters them. If no switches are given and when the user  */
/* didn't give both file names, then ppargs also request for options and  */
/* processes them. Upon returning it remains only to expand any      	  */
/* wildcards in the two filespecs.  Wildcards do not play an equivalent   */
/* role in both filespecs.  A wildcard in the spec for file1 has the      */
/* standard use of a wildcard, i.e. "find all files matching this         */
/* specification," a la FindFirst and FindNext.  A wildcard in the spec   */
/* for file2 specifies pattern matching WITH RESPECT TO FILE1, so that    */
/* the wildcard character in the spec for file2 gets replaced with as     */
/* many characters from file1 as needed to flesh out the spec in a way    */
/* that "matches" file1.  If no such expansion is possible, this is       */
/* considered a failure and comp returns an error code. Finally, we call  */
/* the compare routine which compares the two files in a manner dependent */
/* upon the options which have been specified.                            */
/**************************************************************************/

int main(int argc, char *argv[])
{
  char *locargv[MAX_PARAMS];      /* another (local) argument vector */
  char answer[2];	          /* answer to "Compare more files (Y/N) ? */
  int ret;                        /* return value */
  Index i;                        /* loop counter */
  union REGS reg;                 /* structure for doing int21's */

  /* set up exit routine */
  atexit(ExitComp);

  /* Do DOS version check */
  reg.h.ah = VERSION_NUM;
  reg.h.al = 0x0;
  intdos(&reg, &reg);
  if ( (reg.h.al < MAJ_VER_NUM) || 
       ((reg.h.al == MAJ_VER_NUM) && (reg.h.ah < MIN_VER_NUM)) )
  {
    Beep();
    printf("%s\n", msg_tbl[INCORRECT_DOS_VER]);
    return (INCORRECT_DOS_VER);
  }

  /*
   *  Check for "/?" on the command line. If found, output the options
   *  help message, and then exit.
   */
  if (CheckOptionsHelp(argc, argv))
    return (SUCCESS);             /* not an error to ask for help */

  reg.h.ah = 0x44;
  reg.h.al = 0x0;                       /* get device info */
  reg.x.bx = 0x0;                       /* for STDIN */
  intdos(&reg, &reg);
  if (reg.x.dx & 0x80)
    input_redir = 0;
  else
    input_redir = 1;

  /* Get space for the two buffers */
  if ((bufsiz = getmem(&buf1, &buf2)) == NO_MEMORY)
  {
    Beep();
    printf("%s\n", msg_tbl[NO_MEM_AVAIL]);
    return (NO_MEM_AVAIL);
  }

  /* get the current append status */
  reg.x.ax = GET_APPEND_STATUS;
  int86(APPEND_INT,&reg, &reg);
  append_stat = reg.x.bx;

  do
  {
    /* Initialize possible storage space for command line parameters */
    for (i = 0; i < MAX_PARAMS; i++)
      locargv[i] = NULLPTR;

    /* Make sure filespecs are valid */
    if (ret = ppargs(argv, &argc, locargv))
    {
      if (ret != NO_MEM_AVAIL)
        continue;
      else
        exit(NO_MEM_AVAIL);
    }

    /* Parse the two filenames and compare the two files */
    ParseFileNames(locargv[1], locargv[2]);

    /* Free the memory used for storing command line arguments */
    for (i = 0; i < MAX_PARAMS; i++)
    {
      if (locargv[i] != NULLPTR)
        free((void *)locargv[i]);
    }

    /*
     *  The routine getche used to be used here.  However, when the input is
     *  redirected,  getche still uses int21 - fn 1 instead of using file
     *  read on handle 0.  Problems with compiler, probably, so changed
     *  getche to gets.
     */
    while (TRUE)
    {
      fprintf(stderr, "%s", msg_tbl[COMPARE_MORE]);
      compgets(answer,2);
      *answer = toupper(*answer);
      if ((*answer == YES) || (*answer == NO))             /* M007 */
        break;
    }
    argc = 1;

  } while (*answer == YES);                                /* M007 */

  exit(SUCCESS);
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
  int i;

  for (c--, v++; c; c--, v++)
  {
    if (v[0][0] == '/' && v[0][1] == '?')
    {
      /* print out the help message */
      for (i=0; i < USER_HELP_LINES; i++)
        printf("%s\n", msg_tbl[USER_HELP1 + i]);
      return (TRUE);                        /* /? found, message printed */
    }
  }
  /* M003 - move this return out of the FOR loop */
  return (FALSE);                         /* /? not found, no message */
}


/*********************************************************************/
/* Routine:   comp                                                   */
/*                                                                   */
/* Function:  Compare the two files.                                 */
/*********************************************************************/

unsigned comp(Pathname file1, Pathname file2)  
{
  unsigned char *ptr1, *ptr2;        /* ptrs to move along the buffers */
  int  handle1, handle2;             /* handles for the two files */
  unsigned mismatch = 0;             /* count of the number of mismatches */
  unsigned char_cnt,                 /* # of buffer chars compared so far */
	   cr_count1 = 1,            /* # of Carr. Ret. seen in first file */
           cr_count2 = 1,            /* # of Carr. Ret. seen in second file */
	   read1, read2;             /* # of chars read into buf1, buf2 */
  unsigned long total_cnt = 0;       /* # of file chars compared so far */


  /* Tell user we are comparing the files */
  printf("%s%s%s%s%s\n", msg_tbl[COMPARING], file1, msg_tbl[AND], file2,
                         msg_tbl[ELLIPSES]);

  /* Open the two files */
  if ((open_file(file1, &handle1)) || (open_file(file2, &handle2)))
  {
    _dos_close(handle1);
    return (CANT_OPEN_FILE);
  }

  /* If the user did not ask to compare only a certain number of lines, */
  /* make sure the file sizes are the same before doing the compare     */
  if (!limit)
  {
    if (filelength(handle1) != filelength(handle2))
    {
      Beep();
      printf("%s\n", msg_tbl[DIFFERENT_SIZES]);
      _dos_close(handle1);                       /* M006 */
      _dos_close(handle2);                       /* M006 */
      return (DIFFERENT_SIZES);
    }
  }

  /* Repeatedly read one bufferful from each file and compare the bytes */
  do
  {
    if ((read_file(handle1, (void far *)buf1, bufsiz, &read1, file1)) ||
        (read_file(handle2, (void far *)buf2, bufsiz, &read2, file2)))
    {
      _dos_close(handle1);
      _dos_close(handle2);
      return (CANT_READ_FILE);
    }

    /* (Re)initialize data */
    char_cnt = 0;
    ptr1 = buf1;
    ptr2 = buf2;

    /*
     *  Run through the buffers and compare bytes.
     *
     *  If the user either asked to have the line count (rather than byte    
     *  offset) of the mismatches reported, or wanted only the first n lines 
     *  to be compared, we need to count Carriage Returns, which is how we   
     *  determine the line number.  Note that this is not an extremely       
     *  accurate method, i.e. the two files may differ at a character which  
     *  happens to be a CR, but for the purposes of this program, counting   
     *  Carriage Returns in the first file is a sufficient means of getting  
     *  the line count.
     */
    if (count_lines || limit)
    {
      while ((char_cnt++ <= read1) && (char_cnt <= read2) &&
             (mismatch < MAX_NUM_DIFF))                    /* M011 */
      {

#ifdef DBCS
	if (IsDBCSLeadByte(*ptr1) &&
            (char_cnt < read1) && (char_cnt < read2))      /* M011 */
        {
	  if (ptr1[0] != ptr2[0] || ptr1[1] != ptr2[1])
	  {
            if (asci)
            {
	      printf("%s%u\n%s%c%c\n%s%c%c\n", msg_tbl[COMP2_ERR], cr_count1,
                      msg_tbl[FILE1], ptr1[0], ptr1[1], msg_tbl[FILE2],
                      ptr2[0], ptr2[1]);
            }
	    else
            {
	      printf("%s%u\n%s%X%X\n%s%X%X\n", msg_tbl[COMP2_ERR], cr_count1,
                      msg_tbl[FILE1], ptr1[0], ptr1[1], msg_tbl[FILE2],
                      ptr2[0], ptr2[1]);
            }
	    mismatch++;
	  }
          char_cnt++;
          ptr1++;
          ptr2++;
	}
        else
        {
#endif

	  if (*ptr1 != *ptr2)
          {
            if ( (!no_case) || ((abs(*ptr1 - *ptr2)) != CASE_DIFFERENCE) )
            {
              printf(fmt, msg_tbl[COMP2_ERR], cr_count1, msg_tbl[FILE1],
                     *ptr1, msg_tbl[FILE2], *ptr2); 
              mismatch++;
            }
	  }

#ifdef DBCS
        }
#endif

        /* Count carriage returns */                       /* M011 Start */
        if (*ptr1 == NEWLINE)                         
        {
          cr_count1++;
          if (limit && (cr_count1 > limit))
            goto gtho;
        }
        if (*ptr2 == NEWLINE)
        {
          cr_count2++;
          if (limit && (cr_count2 > limit))
            goto gtho;
        }

        /*
         *  Make sure we haven't reached the end of one file before
         *  reaching the end of the other file (before reaching limit).
         */
        if (((char_cnt == read1) && (char_cnt < read2)) ||
            ((char_cnt > read1) && (char_cnt == read2)))
        {
          Beep();
          printf(msg_tbl[FILE1_LINES], cr_count1 - 1);
          mismatch++;
          goto gtho;
        }
        if (((char_cnt == read2) && (char_cnt < read1)) ||
            ((char_cnt > read2) && (char_cnt == read1)))
        {
          Beep();
          printf(msg_tbl[FILE2_LINES], cr_count2 - 1);
          mismatch++;
          goto gtho;
        }                                                  /* M011 End */

        ptr1++;
        ptr2++;                                            
      }
    }

    /*
     *  If we don't need to know the line count, there is no need to take
     *  the hit of checking each character to see whether it is a CR, so we
     *  have a separate loop here which does the same thing but excludes the
     *  check.
     */
    else
    {
      while ((char_cnt < read1) && (mismatch < MAX_NUM_DIFF))
      {
#ifdef DBCS
        if (IsDBCSLeadByte(*ptr1) && char_cnt+1 < read1)
	{
	  if (ptr1[0] != ptr2[0] || ptr1[1] != ptr2[1])
	  {
	    if (asci)
            {
	      printf("%s%lX\n%s%c%c\n%s%c%c\n", msg_tbl[COMP_ERR],
                      total_cnt + char_cnt, msg_tbl[FILE1], ptr1[0], ptr1[1],
                      msg_tbl[FILE2], ptr2[0], ptr2[1]);
            }
            else
            {
	      printf("%s%lX\n%s%X%X\n%s%X%X\n", msg_tbl[COMP_ERR],
                      total_cnt + char_cnt, msg_tbl[FILE1], ptr1[0], ptr1[1],
                      msg_tbl[FILE2], ptr2[0], ptr2[1]);
            }
            mismatch++;
          }
          char_cnt++;
          ptr1++;
          ptr2++;
	}
        else
        {
#endif
	  if (*ptr1 != *ptr2)
          {
	    if ( (!no_case) || ((abs(*ptr1 - *ptr2)) != CASE_DIFFERENCE) )
            {
	      printf(lfmt, msg_tbl[COMP_ERR], total_cnt + char_cnt,  
	             msg_tbl[FILE1], *ptr1, msg_tbl[FILE2], *ptr2);
	      mismatch++;
	    }
	  }
#ifdef DBCS
        }
#endif
        char_cnt++;
        ptr1++;
        ptr2++;
      }
    }

    total_cnt += char_cnt;

    if (mismatch > 9)
    {
      Beep();
      printf("\r\n%s\n", msg_tbl[TEN_MISM]);
      break;
    }

  } while ( (read1 == bufsiz) || (read2 == bufsiz) );      /* M011 */

gtho:                                /* get out */
  _dos_close(handle1);
  _dos_close(handle2);

  if (!mismatch)
  {
    printf("%s\n", msg_tbl[SUCCESS]);
    return (SUCCESS);
  }
  else                               /* M005 */
    printf("\r\n");                  /* print final carriage return */
} 


/*********************************************************************/
/* Routine:   open_file                                              */
/*                                                                   */
/* Function:  Open file specified by fname and return handle in      */
/*            location handleptr.                                    */
/*********************************************************************/

unsigned open_file(Pathname fname, int *handleptr)
{
  int retcode;
  union REGS reg;                 /* structure for doing intDos's */

  /*
   *  We must turn off APPEND before we start searching, otherwise
   *  we may end up comparing a file to itself.
   *  Turning APPEND off before FindFirst and Restoring it immediately
   *  after is much better than
   *    -  i) using FCB searching  as in 3.31
   *    - ii) turning off in the beginning and restoring at the
   *          end; this entails trapping Ctrl-C vector for
   *          cleanup
   *  Turn off APPEND
   */
  if (append_stat & 0xFF)
  {
    reg.x.ax = SET_APPEND_STATUS;
    reg.x.bx = append_stat & 0xFF00;        /* turn off APPEND */
    int86(APPEND_INT,&reg, &reg);
  }

  retcode = _dos_open(fname, O_RDONLY, handleptr); 

  /* restore the original append status */
  if (append_stat & 0xFF)
  {
    reg.x.ax = SET_APPEND_STATUS;
    reg.x.bx = append_stat; /* restore old value */
    int86(APPEND_INT,&reg, &reg);
  }
  /* end of append changes */
  if (retcode)
  {
    Beep();
    printf("%s%s\n", msg_tbl[CANT_OPEN_FILE], fname);          
    return (CANT_OPEN_FILE);                 
  }
  else
    return (SUCCESS);
}


/*********************************************************************/
/* Routine:   read_file                                              */
/*                                                                   */
/* Function:  Read file specified by fname.                          */
/*********************************************************************/
unsigned read_file(int handle, void far *location, unsigned int limit,
                   unsigned *num_read, Pathname fname)
{
  if (_dos_read(handle, location, limit, num_read))
  {
    Beep();
    printf("%s%s\n", msg_tbl[CANT_READ_FILE], fname);
    return (CANT_READ_FILE);
  }
  return (SUCCESS);
}


/*********************************************************************/
/* Routine:   getmem                                                 */
/*                                                                   */
/* Function:  Try to get 60K worth of memory, split between the two  */
/*            buffers. If not, repeatedly decrease the request by    */
/*            10K each time until we get some.                       */
/*********************************************************************/

int getmem(char **p1, char **p2)
{
  unsigned  bufsize;

  bufsize = 60000;
  while ( (*p1 = (char *)malloc(bufsize)) == NULL)
  {
    if (bufsize)
      bufsize -= 10000;
  }
  *p2 = *p1 + (bufsize / 2);      
  return (bufsize / 2);            
}


/*********************************************************************/
/* Routine:   getoneopt                                              */
/*                                                                   */
/* Function:  Set various control flags depending on which options   */
/*            the user has specified.                                */
/*              d = print out differing bytes as decimal numbers.    */
/*              a = print out differing bytes as characters (ascii). */
/*              n = only compare the first n lines of a file         */
/*                  (determined by counting Carriage Returns in the  */
/*                  the first file, - its a crude count because the  */
/*                  second file may not have same number of CR's).   */
/*              c = do a case insensitive comparison.                */
/*              l = tell them on which LINE there was a byte         */
/*                  mismatch as well as the byte number itself.      */
/*********************************************************************/

void getoneopt(char *av)
{
  char *temptr,*curopt,curchr;    /* to process switches in the same arg */

  if ((*av != SWITCH_CHAR_1) && (*av != SWITCH_CHAR_2))
  {
    Beep();
    printf("%s\n", msg_tbl[SYNT_ERR]);
    exit(SYNT_ERR);
  }

  curopt = av;
  while (TRUE)
  {
    switch (toupper(*(curopt + 1)))
    {
      case DECIMAL_SWITCH :
          decimal = TRUE;
	  if (asci)                    /* reset asci if set previously */
            asci = FALSE;
	  strcpy(lfmt, lfmt_str);
	  strcpy(fmt, fmt_str);
	  setfmt(lfmt, 'd');
	  setfmt(fmt, 'd');
	  break;
      case ASCII_SWITCH :
          asci = TRUE;
	  if (decimal)                 /* reset decimal if set previously */
            decimal = FALSE;
	  strcpy(lfmt, lfmt_str);
	  strcpy(fmt, fmt_str);
	  setfmt(lfmt, 'c');
	  setfmt(fmt, 'c');
	  break;
      case LIMIT_SWITCH :
          if ( *(curopt+2) != '=')
          {
            Beep();
	    printf("%s\n", msg_tbl[NEED_DELIM_CHAR]);
	    break;                     /* ignore limit_switch */
	  }
	  temptr = curopt + 3;
	  while ((curchr = *temptr) != '\0')
          {
	    if (!( (curchr >= '0') && (curchr <= '9') ))
            {
	      if ((curchr == SWITCH_CHAR_1) || (curchr == SWITCH_CHAR_2))
	      	break;
              Beep();
	      printf("%s\n%s\n", msg_tbl[BAD_NUMERIC_ARG], av);
	      exit(BAD_NUMERIC_ARG);
	    }
	    limit = (10 * limit) + (*temptr - '0');
	    temptr++;
	  }
	  curopt = temptr - 2;         /* to make up for the +2 later */
	  break;
      case NO_CASE_SWITCH :
          no_case = TRUE;
	  break;
      case LINE_CT_SWITCH :
          count_lines = TRUE;
	  break;
      default:
          Beep();
	  printf("%s %s\n",msg_tbl[INV_SWITCH],curopt);
	  exit(SYNT_ERR);
	  break;
    }
    curopt += 2;                       /* skip switch char and option char */
    if ((*curopt == SWITCH_CHAR_1) || (*curopt == SWITCH_CHAR_2))
      continue;
    else
      break;
  }
}


/*********************************************************************/ 
/* Routine:   ppargs                                                 */
/*                                                                   */
/* Function:  Pre-process command line : make sure the two filespecs */
/*            have legitimate filenames, query for options if        */
/*            original command line was incomplete.                  */
/*********************************************************************/

unsigned ppargs(char *argv[], int *argc, char *locargv[])
{
  int i, nooffiles, noofopts;
  unsigned att;                        /* storage for file attribute */
  char lastchr;

  /* Check to make sure user didn't pass too many arguments */
  if (*argc > MAX_PARAMS)
  {
    Beep();
    printf("%s\n", msg_tbl[TOO_MANY_ARGS]);
    return (TOO_MANY_ARGS);
  }

  /* Initialize variables */
  nooffiles = noofopts = 0;
  limit = 0;                                     /* M010 */
  
  /* Set local argument vector to point at the command line args given */
  for (i = 1; i < *argc; i++)
  {
    if ( (*argv[i] == SWITCH_CHAR_1) || (*argv[i] == SWITCH_CHAR_2))
    {
      noofopts++;
      locargv[noofopts+2] = argv[i];
      getoneopt(argv[i]);              /* process this option */
    }
    else if (*argv[i])                 /* treat this as a file spec */
    {
      nooffiles++;
      if (nooffiles > 2 )
      {
        Beep();
        printf("%s\n", msg_tbl[SYNT_ERR]);
        exit(SYNT_ERR);
      }
      locargv[nooffiles] = argv[i];    /* process the file name later */
    }
  }

  /* If any of the filename arguments were not given on the command line,  */
  /* get them now.  The following depends on the assumption that the first */
  /* two arguments are the filespecs, and any subsequent arguments are     */
  /* options.                                                              */
  for (i = nooffiles + 1; i <= 2; i++)
  {
    fprintf(stderr, "%s", msg_tbl[i + PPARG_MSG_OFF]);
    locargv[i] = (char *)malloc(MAXFLNM);
    if (locargv[i] == NULL)
    {
      Beep();
      printf("%s\n", msg_tbl[NO_MEM_AVAIL]);
      return (NO_MEM_AVAIL);
    }
    compgets(locargv[i],MAXFLNM+1);
  }

  /* Check whether the filespec was a drive designation or a partial       */
  /* filespec, i.e. directory specification only.  If so, tack on a "*.*"  */
  for (i = 1; i <= 2; i++)
  {
    if (dronly(locargv[i]))
    {
      locargv[i] = extend_length(locargv[i]);
      strcat(locargv[i],"*.*");
    }
    else if (!_dos_getfileattr(locargv[i], &att))
    {
      if (att & _A_SUBDIR)
      {
        lastchr = locargv[i][strlen(locargv[i]) -1];
        if (lastchr == '/')
          lastchr = '\\';

#ifdef DBCS
        if (lastchr != '\\' ||
            CheckDBCSTailByte(locargv[i],&locargv[i][strlen(locargv[i]) - 1]))
#else
        if (lastchr != '\\')
#endif
        { 
          locargv[i] = extend_length(locargv[i]);
	  strcat(locargv[i],"\\*.*");
	}
	else
        {
          locargv[i] = extend_length(locargv[i]);
          strcat(locargv[i],"*.*");
        }
      }
    }
  }

  /* Now we query for options, but only if the original command line  */
  /* was incomplete, i.e. even file(s) were not specified.  Otherwise */
  /* either user already specified options or doesn't want them.      */
  if ((nooffiles < 2) && !noofopts)
  {
    /*
     *  Really want ot start at 3, but need to increment at start of loop,
     *  so compensate.
     */
    i = noofopts + 2;
    do
    {
      i++;
      locargv[i] = (char *)malloc(OPT_SIZE); 
      if (locargv[i] == NULL)
      {
        Beep();
        printf("%s\n", msg_tbl[NO_MEM_AVAIL]);
        return (NO_MEM_AVAIL);
      }
      fprintf(stderr, "%s", msg_tbl[OPTION_REQUEST]);
      compgets(locargv[i],OPT_SIZE+1);
      if (*locargv[i] != '\0')
      { 
        noofopts++;
        getoneopt(locargv[i]);          /* process this option */
      }
      if (noofopts >= MAX_OPTIONS) 
        break;
    } while (*locargv[i] != '\0');
  }

  /* cleanup code for processing options */
  if ((!decimal) && (!asci))
  {
    setfmt(lfmt, 'X');
    setfmt(fmt, 'X');
  }
  return (SUCCESS);
}


/*********************************************************************/
/* Routine:   dronly                                                 */
/*                                                                   */
/* Function:  Determine whether the string in question designates a  */
/*            drive.                                                 */
/*********************************************************************/

int dronly(char *s)
{ 
  return ( (*(s + 1) == ':') && (*(s + 2) == '\0') );
}


/**************************************************************************/
/* Routine:   set_fmt                                                     */
/* Function:  Replace the characters '?' and '!' in the format string     */
/*            with the character specified.                               */
/* Arguments: A string containing format information for printf in which  */
/*            output specification information is missing (with           */
/*            placeholders ? and ! present instead), and the character    */
/*            with which to replace them.                                 */
/* Side effects: none                                                     */
/**************************************************************************/

void setfmt(char *fmtstr, char c)
{
  char *replace;

  if ((replace = strchr(fmtstr, '?')) != NULL)
    *replace = c;
  if ((replace = strchr(fmtstr, '!')) != NULL)
    *replace = c;
}


/**************************************************************************/
/* Routine:  extend_length                                                */
/* Function: make the string sufficiently long to add "\*.*" or "*.*" or  */
/*           "*" to it.                                                   */
/* Arguments: an arbitrary string                                         */
/**************************************************************************/

char *extend_length(char *s)
{
  char *news;

  news = (char *)malloc(strlen(s) + ENOUGH_EXTRA);
  strcpy(news, s);
  return (news);
}


/**************************************************************************/
/* Routine:  has_extension                                                */
/* Function: check for extensions in the string.                          */
/* Arguments: an arbitrary string                                         */
/**************************************************************************/

Boolean has_extension(char *s)
{
  int i;
  char *p1, *p2;

  p1 = s + strlen(s) - 1;
  for (i = 1; i <= 4; i++)
  {
    if (*p1 == '.')
      break;
    p1--;
  }
  p2 = strrchr(s, '\\');
  if (!p2) 
    p2 = strrchr(s,'/');
  if ((*p1 == '.') && !(p2 && (p2 > p1)))
    return (TRUE);
  else
    return (FALSE);
}

	
/*********************************************************************/ 
/* Routine:   compgets                                               */
/*                                                                   */
/* Function:  Simulate a fgets() to take care of buffer overflow.    */
/*********************************************************************/

void compgets(char *buff, int length)
{
  char *sptr;
  int i;
  union REGS reg;                      /* structure for doing int21's */

  if (input_redir)
  {
    reg.h.ah = 0xb;                    /* check input status */
    intdos(&reg,&reg);
    if (reg.h.al == 0)                 /* EOF ? */
    {
      Beep();
      printf("%s\n", msg_tbl[UNEXP_EOF]);
      exit(UNEXP_EOF);
    }
  }

  /* else  - NOT EOF; go get input */
  GetInput(&length);

  sptr = getsbuf;
  if (*sptr == EOFCHAR)
  {
    Beep();
    printf("%s\n", msg_tbl[UNEXP_EOF]);
    exit(UNEXP_EOF);
  }
  for (i = 0; i < length; i++)
    *buff++ = *sptr++;
  *buff = '\0';                        /* null terminate the string */
}


/*********************************************************************/ 
/* Routine:   GetInput                                               */
/*                                                                   */
/* Function:  Gets the input from stdin and outputs it to stderr.    */
/*            The number of characters read is returned.             */
/*            Outputing the input to stderr allows the input to be   */
/*            echoed to the screen when the output is redirected.    */
/*                                                                   */
/* Note:      This routine uses the global getsbuf buffer to save    */
/*            the input.  It assumes that the caller sends in a      */
/*            length that is LESS THAN the size of getsbuf.          */
/*********************************************************************/

void GetInput(int *length)
{
  register int NumRead = 0;            /* number of bytes read */
  register char c;                     /* character read in */


  /* read a character from stdin without echoing it */
  c = ReadStdin();                          /* M012 */

  /*  M012
   *  If the input has been redirected and the first character read in
   *  is a newline, then ignore the newline and read the next character.
   *  The reason for this is that when command.com redirects stdin to a
   *  file containing the comp command, the newline after the carriage
   *  return is NOT read when using int 21 function 08h.
   */
  if ((input_redir) && (c == NEWLINE))
    c = ReadStdin();
  
  while (TRUE)
  {
    /* don't allow user to type more than needed */
    if (c == BACKSPACE)
    {
      if (NumRead > 0)
      {
        /* write the backspace to stderr */
        WriteStderr(c);

        /* decrement count of number of bytes read */
        NumRead--;

        /* delete the character from the screen */
        WriteStderr(SPACEBAR);
        WriteStderr(BACKSPACE);
      }
      else
        Beep();                        /* sound beep for error */
    }
    else if (c == RETURNCHAR)
    {
      /* write the return character to stderr */
      WriteStderr(c);

      /* if stdin redirected, read new line character */
      if (input_redir)                      /* M009 - Start */
      {
        /* read the new line character from stdin */
        ReadStdin();                        /* M012 */
      }
      /* echo newline character */
      WriteStderr(NEWLINE);                 /* M009 - End */

      /* break out of loop, done */
      break;
    }
    else if (c == EOFCHAR)                  /* M009 - Start */
    {
      Beep();
      printf("\n%s\n", msg_tbl[UNEXP_EOF]);
      exit(UNEXP_EOF);
    }                                       /* M009 - End */
    else if (NumRead < (*length - 1)) 
    {
      /* write the character to stderr */
      WriteStderr(c);

      /* save the character in getsbuf and increment counter */
      getsbuf[NumRead] = c;
      NumRead++;
    }
    else
      Beep();                               /* sound beep for error */

    /* read the next character from stdin without echoing it */
    c = ReadStdin();                        /* M012 */
  }

  /* return the number of bytes read from stdin */
  *length = NumRead;
}


/*********************************************************************/ 
/* Routine:   ReadStdin                                              */
/*                                                                   */
/* Function:  Read a character from stdin without echoing the        */
/*            character and return the character to the calling      */
/*            routine.                                               */
/* M012                                                              */
/*********************************************************************/

char ReadStdin()
{
  union REGS reg;                      /* structure for doing int21's */
    
  reg.h.ah = 0x08;                     /* read character without echo */
  intdos(&reg, &reg);
  return (reg.h.al);                   /* return the character */
}


/*********************************************************************/ 
/* Routine:   WriteStderr                                            */
/*                                                                   */
/* Function:  Writes a character to stderr.                          */
/*********************************************************************/

void WriteStderr(char ch)
{
  union REGS reg;                      /* structure for doing int21's */
    
  reg.h.ah = 0x40;                     /* write to a device */
  reg.x.bx = 0x2;                      /* stderr */
  reg.x.cx = 0x1;                      /* write 1 byte */
  reg.x.dx = (unsigned) &ch;           /* character to write */
  intdos(&reg,&reg);
}


/*********************************************************************/ 
/* Routine:   ParseFileNames                                         */
/*                                                                   */
/* Function:  Parses the two given filenames and then compares the   */
/*            appropriate filenames.  This routine handles wildcard  */
/*            characters in both filenames.                          */
/*********************************************************************/

void ParseFileNames(char *file1, char *file2)
{
  struct find_t struct1;                  /* for findfirst/findnext */
  Pathname final1, final2;                /* final path and filename */
  Pathname inter2;                        /* intermediate filename for file2 */
  char *fname1, *fname2;                  /* filename begins here */
  int Wildcard2;                          /* if file2 contains a wildcard */


  /* Convert filenames to uppercase. */
  strupr(file1);
  strupr(file2);

  /* Copy current pathnames to final destination holders */
  strcpy(final1, file1);
  strcpy(final2, file2);

  /* Find position of filename for both pathnames */
  fname1 = FindFileName(final1);
  fname2 = FindFileName(final2);

  /*
   *  If a "*" is found by itself, make it "*.*".  The reason for this is
   *  that dos_findfirst treats "*" as "*." instead of "*.*".
   */
  CheckWildcard(final1, fname1);
  CheckWildcard(final2, fname2);

  /*
   *  Copy final2 filename to intermediate holder (contains wildcards).
   *  This is needed because final2 will be overwritten for each
   *  iteration through the loop.
   */
  strcpy(inter2, final2);

  /* Get name of first file1 to compare (handles wildcards). */
  if (_dos_findfirst(final1, _A_RDONLY, &struct1))
  {
    /* dos_findfirst failed, so print message */
    Beep();
    printf("%s%s\n", msg_tbl[CANT_OPEN_FILE], file1);
    return;
  }

  /* Set flag for whether or not file2 has a wildcard */
  Wildcard2 = HasWildcard(inter2);

  do
  {
    /*
     *  Create full pathname for file 1 with original path and new file name.
     *  This assumes that dos_findfirst returns an uppercase filename.
     */
    strcpy(fname1, struct1.name);

    if ( Wildcard2 )
    {
      /* Expand wildcards in file2 and compare the two files. */
      if (ExpandFile2(fname1, fname2))           /* M008 */
      {                                /* expansion of file2 failed */
        Beep();
        printf("%s     %s\n%s\n\n\n", final1, file2, msg_tbl[COULD_NOT_EXP]);
      }
      else
        comp(final1, final2);          /* compare the two files */
      
      strcpy(final2, inter2);          /* recopy original with wildcards */
    }                                            /* M008 */
    else
    {
      /* no wildcards in file2, so just compare final1 and final2 */
      comp(final1, final2);
    }
  } while (!_dos_findnext(&struct1));
}


/*********************************************************************/ 
/* Routine:   FindFileName                                           */
/*                                                                   */
/* Function:  Returns the pointer to the start of the filename in    */
/*            the given pathname.                                    */
/*********************************************************************/

char *FindFileName(char *pathname)
{
  int len1;
  char *ptr;

  
  len1 = strlen(pathname);
  ptr = pathname + len1 - 1;           /* position ptr at end of string */

#ifdef DBCS
  while ( ((len1-- && (*ptr != '\\')) && (*ptr != ':') && (*ptr != '/')) ||
          (len1 > 0 && CheckDBCSTailByte(pathname, ptr)) )
#else
  while ( (len1-- && (*ptr != '\\')) && (*ptr != ':') && (*ptr != '/') )
#endif
    ptr--;

    return (ptr + 1);
}


/*********************************************************************/ 
/* Routine:   HasWildcard                                            */
/*                                                                   */
/* Function:  Returns TRUE if string has a wildcard character.       */
/*            Otherwise, it returns FALSE.                           */
/*********************************************************************/

Boolean HasWildcard(char *file)
{
  if ( strchr(file, '?') || strchr(file, '*') )
    return (TRUE);
  return (FALSE);
}


/*********************************************************************/ 
/* Routine:   CheckWildcard                                          */
/*                                                                   */
/* Function:  If a "*" is found by itself in the filename, then it   */
/*            is changed to "*.*".  The reason for this is that      */
/*            dos_findfirst treats "*" as "*." instead of "*.*".     */
/*********************************************************************/

void CheckWildcard(char *path, char *file)
{
  char *ptr;                  /* ptr to wildcard in filename */
  
  if ( (!strchr(file, '.')) && (ptr = strchr(file, '*')) )
  {
    if (strlen(path) <= (MAXFLNM - 3))
    {
      /* change the '*' to '*.*' */
      *(ptr+1) = '.';
      *(ptr+2) = '*';
      *(ptr+3) = 0;                    /* end of string marker */
    }
  }
}


/*********************************************************************/ 
/* Routine:   ExpandFile2                                            */
/*                                                                   */
/* Function:  Expands the wildcards found in File2 to match File1.   */
/*            This routine is ONLY called if wildcards exist in      */
/*            File2.                                                 */
/* M008                                                              */
/*********************************************************************/

int ExpandFile2(char *File1, char *File2)
{
  char *Ptr1, *Ptr2;                   /* temp ptrs to filenames */
  char *Dot1, *Dot2;                   /* ptrs to dots in filenames */
  char *End1, *End2;                   /* ptrs to ends of filenames */
  char Storage[5];                     /* temp storage for file extension */
  char *TempFile2;                     /* temp pointer to File2 */

  /*
   *  Initialize pointers to filename sections.
   *  If a filename does not contain an extension, then Dot = End.
   */
  End1 = File1 + strlen(File1);
  End2 = File2 + strlen(File2);
  if (!(Dot1 = strrchr(File1, '.')))
    Dot1 = End1;
  if (!(Dot2 = strrchr(File2, '.')))
    Dot2 = End2;

  /*
   *  Expand '*' wildcard inside File2 by copying characters from
   *  File1 to File2.
   */
  if (Ptr2 = strchr(File2, '*'))
  {
    if (Ptr2 < Dot2)                   /* in first 8 of 8.3 for File2 */
    {
      strcpy(Storage, Dot2);           /* save extension of File2 */
      Ptr1 = File1 + (Ptr2 - File2);
      for (; Ptr1 < Dot1; Ptr1++, Ptr2++)
        *Ptr2 = *Ptr1;
      *Ptr2 = 0;
      End2 = End2 + (Ptr2 - Dot2);     /* reset End2 to new location */
      Dot2 = Ptr2;                     /* reset Dot2 to new location */
      strcat(File2, Storage);          /* recopy saved extension to File2 */
    }
    if (Ptr2 = strchr(File2, '*'))     /* in extension of 8.3 for File2 */
    {
      Ptr1 = Dot1 + (Ptr2 - Dot2);
      for (; Ptr1 < End1; Ptr1++, Ptr2++)
        *Ptr2 = *Ptr1;
      *Ptr2 = 0;                       /* null terminate string */
      End2 = Ptr2;                     /* reset End2 to new location */
    }
  }

  /*
   *  Expand '?' wildcard inside File2 by copying a character from
   *  File1 to File2.
   */
  TempFile2 = File2;
  while (Ptr2 = strchr(TempFile2, '?'))
  {
    if (Ptr2 < Dot2)                   /* in first 8 of 8.3 for File2 */
    {
      Ptr1 = File1 + (Ptr2 - File2);
      if (Ptr1 < Dot1)
        *Ptr2 = *Ptr1;
      else                             /* nothing in File1 matches '?' in File2 */
        return (FAILURE);              /*   return error */
    }
    else                               /* in extension of 8.3 for File2 */
    {
      Ptr1 = Dot1 + (Ptr2 - Dot2);
      if (Ptr1 < End1)
        *Ptr2 = *Ptr1;
      else                             /* nothing in File1 matches '?' in File2 */
        return (FAILURE);              /*   return error */
    }
    TempFile2 = Ptr2 + 1;              /* TempFile2 is now 1 after current '?' */
  }

  /* If the dot is the last character in the string, remove it. */
  if ((Ptr2 = strrchr(File2, '.')) == (End2 - 1))
    *Ptr2 = 0;

  return (SUCCESS);
}


/*********************************************************************/ 
/* Routine:   ExitComp                                               */
/*                                                                   */
/* Function:  Frees the allocated buffer whenever exit is used.      */
/* M006                                                              */
/*********************************************************************/

void ExitComp()
{
  free((void *)buf1);
}



#ifdef DBCS

/*********************************************************************/ 
/* Routine:   IsDBCSLeadByte                                         */
/*                                                                   */
/* Function:  Test if the character is DBCS lead byte.               */
/*                                                                   */
/*      input:  c = character to test                                */
/*      output: TRUE if leadbyte                                     */
/*********************************************************************/

int IsDBCSLeadByte(unsigned char c)
{
  static unsigned char far *DBCSLeadByteTable = NULL;
  union REGS inregs,outregs;
  struct SREGS segregs;
  unsigned char far *p;

  if (DBCSLeadByteTable == NULL)
  {
    inregs.x.ax = 0x6300;           /* get DBCS lead byte table */
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


/*********************************************************************/ 
/* Routine:   CheckDBCSTailByte                                      */
/*                                                                   */
/* Function:  Check if the character point is at tail byte.          */
/*                                                                   */
/*      input:  *str = strart pointer of the string                  */
/*              *point = character pointer to check                  */
/*      output: TRUE if at the tail byte                             */
/*********************************************************************/

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
  return ( (point - p) & 1 ? TRUE : FALSE );
}

#endif

