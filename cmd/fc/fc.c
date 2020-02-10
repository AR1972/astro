;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
/****************************************************************************
    File Compare

    Fcom compares two files in either a line-by-line mode or in a strict
    byte-by-byte mode.

    The byte-by-byte mode is simple; merely read both files and print the
    offsets where they differ and the contents.

    The line compare mode attempts to isolate differences in ranges of lines.
    Two buffers of lines are read and compared.  No hashing of lines needs
    to be done; hashing only speedily tells you when things are different,
    not the same.  Most files run through this are expected to be largely
    the same.  Thus, hashing buys nothing.


***********************************************************************
The algorithm that immediately follows does not work.  There is an error
somewhere in the range of lines 11 on. An alternative explanation follows.
                                                            KGS
************************************************************************

    [0]     Fill buffers
    [1]     If both buffers are empty then
    [1.1]       Done
    [2]     Adjust buffers so 1st differing lines are at top.
    [3]     If buffers are empty then
    [3.1]       Goto [0]

    This is the difficult part.  We assume that there is a sequence of inserts,
    deletes and replacements that will bring the buffers back into alignment.

    [4]     xd = yd = FALSE
    [5]     xc = yc = 1
    [6]     xp = yp = 1
    [7]     If buffer1[xc] and buffer2[yp] begin a "sync" range then
    [7.1]       Output lines 1 through xc-1 in buffer 1
    [7.2]       Output lines 1 through yp-1 in buffer 2
    [7.3]       Adjust buffer 1 so line xc is at beginning
    [7.4]       Adjust buffer 2 so line yp is at beginning
    [7.5]       Goto [0]
    [8]     If buffer1[xp] and buffer2[yc] begin a "sync" range then
    [8.1]       Output lines 1 through xp-1 in buffer 1
    [8.2]       Output lines 1 through yc-1 in buffer 2
    [8.3]       Adjust buffer 1 so line xp is at beginning
    [8.4]       Adjust buffer 2 so line yc is at beginning
    [8.5]       Goto [0]
    [9]     xp = xp + 1
    [10]    if xp > xc then
    [10.1]      xp = 1
    [10.2]      xc = xc + 1
    [10.3]      if xc > number of lines in buffer 1 then
    [10.4]          xc = number of lines
    [10.5]          xd = TRUE
    [11]    if yp > yc then
    [11.1]      yp = 1
    [11.2]      yc = yc + 1
    [11.3]      if yc > number of lines in buffer 2 then
    [11.4]          yc = number of lines
    [11.5]          yd = TRUE
    [12]    if not xd or not yd then
    [12.1]      goto [6]

    At this point there is no possible match between the buffers.  For
    simplicity, we punt.

    [13]    Display error message.

EXPLANATION 2

    This is a variation of the Largest Common Subsequence problem.  A
    detailed explanation of this can be found on p 189 of Data Structures
    and Algorithms by Aho Hopcroft and Ulman.

    FC maintains two buffers within which it tries to find the Largest Common
    Subsequence (The largest common subsequence is simply the pattern in
    buffer1 that yields the most matches with the pattern in buffer2, or the
    pattern in buffer2 that yields the most matches with the pattern in buffer1)

    FC makes a simplifying assumption that the contents of one buffer can be
    converted to the contents of the other buffer by deleting the lines that are
    different between the two buffers.

    Two indices into each buffer are maintained:

            xc, yc == point to the last line that has been scanned up to now

            xp, yp == point to the first line that has not been exhaustively
                      compared to lines 0 - #c in the other buffer.

    FC now makes a second simplifying assumption:
        It is unnecessary to do any calculations on lines that are equal.

    Hence FC scans File1 and File two line by line until a difference is
    encountered.

    When a difference is encountered the two buffers are filled such that
    the line containing the first difference heads the buffer. The following
    exhaustive search algorithm is applied to find the first "sync" occurance.
    (The below is simplified to use == for comparison.  In practice more than
    one line needs to match for a "sync" to be established).

            FOR xc,yc = 1; xc,yx <= sizeof( BUFFERS ); xc++, yc++

                FOR xp,yp = 1; xp,yp <= xc,yc; xp++, yp++

                    IF ( BUFFER1[xp] == BUFFER2[yc] )

                        Then the range of lines BUFFER1[ 1 ... xp ] and
                        BUFFER2[ 1 ... yc ] need to be deleted for the
                        two files to be equal.  Therefore DISPLAY these
                        ranges, and begin scanning both files starting at
                        the matching lines.
                    FI

                    IF ( BUFFER1[yp] == BUFFER2[xc] )

                        Then the range of lines BUFFER2[ 1 ... yp ] and
                        BUFFER1[ 1 ... xc ] need to be deleted for the
                        two files to be equal.  Therefore DISPLAY these
                        ranges, and begin scanning both files starting at
                        the matching lines.
                    FI
                FOREND
            FOREND

    If a match is not found within the buffers, the message "RESYNC FAILED"
    is issued and further comparison is aborted since there is no valid way
    to find further matching lines.

END EXPLANATION 2

    Certain flags may be set to modify the behavior of the comparison:

    -a      abbreviated output.  Rather than displaying all of the modified
            ranges, just display the beginning, ... and the ending difference
    -b      compare the files in binary (or byte-by-byte) mode.  This mode is
            default on .EXE, .OBJ, .LIB, .COM, .BIN, and .SYS files
    -c      ignore case on compare (cmp = strcmpi instead of strcmp)
    -l      compare files in line-by-line mode
    -lb n   set the size of the internal line buffer to n lines from default
            of 100
    -w      ignore blank lines and white space (ignore len 0, use strcmps)
    -t      do not untabify (use fgets instead of fgetl)
    -n      output the line number also
    -NNNN   set the number of lines to resynchronize to n which defaults
            to 2.  Failure to have this value set correctly can result in
            odd output:
              file1:        file2:
                    abcdefg       abcdefg
                    aaaaaaa       aaaaaab
                    aaaaaaa       aaaaaaa
                    aaaaaaa       aaaaaaa
                    abcdefg       abcdefg

            with default sync of 2 yields:          with sync => 3 yields:

                    *****f1                             *****f1
                    abcdefg                             abcdefg
                    aaaaaaa                             aaaaaaa
                    *****f2                             aaaaaaa
                    abcdefg                             *****f2
                    aaaaaab                             abcdefg
                    aaaaaaa                             aaaaaab
                                                        aaaaaaa
                    *****f1
                    aaaaaaa
                    aaaaaaa
                    abcdefg
                    *****f2
                    aaaaaaa
                    abcdefg

WARNING:
        This program makes use of GOTO's and hence is not as straightforward
        as it could be!  CAVEAT PROGRAMMER.
****************************************************************************/


#include "tools.h"
#include "messages.h"
#include "fc.h"
#include <versionc.h>
#include <malloc.h>
#include <process.h>
#include <dos.h>
#include <version.h>


/**************************************************************************/
/* main                                                                   */
/**************************************************************************/

int main (int c, byte *v[])
{
  int i;
  int j;
  int fileargs;
  char *strpbrk(), *slash;
  char n[2][MAXFNAME];


  /* Issue error message if DOS version is not within valid range. */
  if ((EXPECTED_VERSION_MAJOR != _osmajor) ||      
      (EXPECTED_VERSION_MINOR != _osminor))       
  {    
    usage (Bad_ver, NULL);   
    return (1);
  }   

  funcRead = (int (*) ())FNADDR(fgetl);

  fileargs = 0;
  for (i=1; i < c; i++)
  {
    /*
     *  If argument doesn't begin with a /, parse a filename off of it
     *  then examine the argument for following switches.
     *
     */
    if (*v[i] != '/')
    {
      if (fileargs == 2)
      {
        usage(msg_tbl[TOO_MANY_FNAMES], NULL);
        return (1);
      }
    
      slash = strchr( v[i],'/');
      if ( slash )
      {
        *slash='\0'  ;
        strcpy(n[fileargs++],v[i]);
        *slash = '/';
      }
      else
        strcpy(n[fileargs++],v[i]);
    }
    
    for (j=0; j < strlen( v[i] ); j++)
    {
      if (*(v[i]+j) == '/')
      {
        switch ( toupper( *(v[i]+j+1)) )
        {
          case '?' :
              usage(NULL, HELPTEXT);
    	      return (0);
    	      break;
          case 'A' :
              fAbbrev = TRUE;
              break;
          case 'B' :
              fBinary = TRUE;
              break;
          case 'C' :
              fCase = FALSE;
              break;
#ifdef  DEBUG
          case 'D' :
              fDebug = TRUE;
              break;
#endif
          case 'W' :
              fIgnore = TRUE;
              break;
          case 'L' :
              if (toupper(*(v[i]+j+2)) == 'B')
              {
                cLine = ntoi ((v[i]+j+3),10);
                break;
              }
              else
                fLine = TRUE;
              break;
          case 'N' :
              fNumb = TRUE;
              break;
          case 'T' :
              funcRead =(int (*) ())FNADDR(fgets);
              break;
          default:
              if (*strbskip((v[i]+j+1),"0123456789") == 0)
                ctSync = ntoi ((v[i]+j+1), 10);
              else
                usage (msg_tbl[INVALID_SWITCH], NULL);
        }
      }
    }  
  }
  if (fileargs != 2)
  {
    usage (msg_tbl[NOT_ENUF_FILES], NULL);
    return (1);
  }
  if (ctSync != -1)
    fLine = TRUE;
  else
    ctSync = 2;

  if (cLine == -1)
    cLine = 100;

  if (!fBinary && !fLine)
  {
    extention (n[0], line);

    for (i=0; extBin[i]; i++)
    {
      if (!strcmpi (extBin[i], line))
        fBinary = TRUE;
    }
    if (!fBinary)
      fLine = TRUE;
  }

  if (fBinary && (fLine || fNumb))
  {
    usage (BadSw, NULL);
    return (1);
  }

  if (fIgnore)
  {
    if (fCase)
      fCmp = FNADDR(strcmps);
    else
      fCmp = FNADDR(strcmpis);
  }
  else
  {
    if (fCase)
      fCmp = FNADDR(strcmp);
    else
      fCmp = FNADDR(strcmpi);
  }

  /* Parse the two filenames and compare the two files */
  ParseFileNames(n[0], n[1]);

  return (0);
}


/***************************************************************************/
/* Usage has changed to take a third arg, pcode (print code), in order     */
/* to be able to specify printing more than one message at a call - before */
/* usage would check p, and if it was a null pointer, would print UseMes1, */
/* (now hlp_tbl[1]), otherwise it would print whatever p pointed to, which */
/* works if you only want one msg at a time.  pcode is an extensible means */
/* of being able to add all sorts of print capabilities. (leaf, april 90)  */
/***************************************************************************/

void usage (unsigned char *p, unsigned int pcode)
{
  int i;

  if (pcode == HELPTEXT)
  {
    for (i = 0; i < HELP_TEXT_LEN; i++)
      printf(hlp_tbl[i]);
  }
  else
  {
    if (p)
      printf ("FC: %s\n", p);
    else  
      printf (hlp_tbl[1]);
  }
}


/**************************************************************************/
/* BinaryCompare                                                          */
/**************************************************************************/

int BinaryCompare (unsigned char *f1, unsigned char *f2)
{
  register int c1, c2;
  long pos;
  FILE *fh1, *fh2;
  flagType fSame;

  fSame = TRUE;
  if ((fh1 = fopen (f1, "rb")) == NULL)
  {
    sprintf (line, BadOpn, f1, error ());
    usage (line, NULL);
    return (1);
  }

  if ((fh2 = fopen (f2, "rb")) == NULL)
  {
    sprintf (line, BadOpn, f2, error ());
    usage (line, NULL);
    fclose(fh1);
    return (1);
  }
  pos = 0L;

  while (TRUE)
  {
    if ((c1 = getc (fh1)) != EOF)
    {
      if ((c2 = getc (fh2)) != EOF)
      {
        if (c1 != c2)
        {
          fSame = FALSE;
          printf ("%08lX: %02X %02X\n", pos, c1, c2);
        }
      }
      else
      {
        sprintf (line, LngFil, f1, f2);
        usage (line, NULL);
	fclose(fh1);
	fclose(fh2);
	return (1);
      }
    }
    else
    {
      if ((c2 = getc (fh2)) == EOF)
      {
        if (fSame)
            usage (NoDif, NULL);
        fclose(fh1);
	fclose(fh2);
        return (0);
      }
      else
      {
        sprintf (line, LngFil, f2, f1);
        usage (line, NULL);
	fclose(fh1);
	fclose(fh2);
	return (1);
      }
    }
    pos++;
  }
  fclose(fh1);
  fclose(fh2);
  return (0);
}


/**************************************************************************/
/* Compare a range of lines.                                              */
/**************************************************************************/

flagType compare (int l1, register int s1, int l2, register int s2, int ct)
{
#ifdef  DEBUG
  if (fDebug)
    printf ("compare (%d, %d, %d, %d, %d)\n", l1, s1, l2, s2, ct);
#endif

  if (ct == 0 || s1+ct > l1 || s2+ct > l2)
    return (FALSE);

  while (ct--)
  {

#ifdef  DEBUG
    if (fDebug)
      printf ("'%s' == '%s'? ", buffer1[s1].text, buffer2[s2].text);
#endif

    if ((*fCmp)(buffer1[s1++].text, buffer2[s2++].text))
    {

#ifdef  DEBUG
      if (fDebug)
        printf ("No\n");
#endif
      return (FALSE);
    }
  }

#ifdef  DEBUG
  if (fDebug)
    printf ("Yes\n");
#endif

  return (TRUE);
}


/**************************************************************************/
/* LineCompare                                                            */
/**************************************************************************/

void LineCompare (unsigned char *f1, unsigned char *f2)
{
  FILE *fh1, *fh2;
  int l1, l2, i, xp, yp, xc, yc;
  flagType xd, yd, fSame;
  int line1, line2;

  fSame = TRUE;
  if ((fh1 = fopen (f1, "rb")) == NULL)
  {
    sprintf (line, BadOpn, f1, error ());
    usage (line, NULL);
    return;
  }

  if ((fh2 = fopen (f2, "rb")) == NULL)
  {
    sprintf (line, BadOpn, f2, error ());
    usage (line, NULL);
    fclose(fh2);
    return;
  }

  if ((buffer1 = (struct lineType *)malloc (cLine * (sizeof *buffer1))) == NULL ||
      (buffer2 = (struct lineType *)malloc (cLine * (sizeof *buffer1))) == NULL)
  {
    usage (NoMem, NULL);
    fclose(fh1);
    fclose(fh2);
    return;
  }

  l1 = l2 = 0;
  line1 = line2 = 0;

l0:

#ifdef  DEBUG
  if (fDebug)
    printf ("At scan beginning\n");
#endif

  l1 += xfill (buffer1+l1, fh1, cLine-l1, &line1);
  l2 += xfill (buffer2+l2, fh2, cLine-l2, &line2);

  if (l1 == 0 && l2 == 0)
  {
    if (fSame)
      usage (NoDif, NULL);
    free((void *)buffer1);
    free((void *)buffer2);
    fclose(fh1);
    fclose(fh2);
    return;
  }
  xc = min (l1, l2);

  for (i=0; i < xc; i++)
  {
    if (!compare (l1, i, l2, i, 1))
      break;
  }

  if (i != xc)
    i = max (i-1, 0);

  l1 = adjust (buffer1, l1, i);
  l2 = adjust (buffer2, l2, i);

  /* KLUDGE ALERT!! GOTO USED */
  if (l1 == 0 && l2 == 0)
    goto l0;

  l1 += xfill (buffer1+l1, fh1, cLine-l1, &line1);
  l2 += xfill (buffer2+l2, fh2, cLine-l2, &line2);

#ifdef  DEBUG
  if (fDebug)
    printf ("buffers are adjusted, %d, %d remain\n", l1, l2);
#endif

  xd = yd = FALSE;
  xc = yc = 1;
  xp = yp = 1;

l6:

#ifdef  DEBUG
  if (fDebug)
    printf ("Trying resync %d,%d  %d,%d\n", xc, xp, yc, yp);
#endif

  i = min (l1-xc,l2-yp);
  i = min (i, ctSync);

  if (compare (l1, xc, l2, yp, i))
  {
    fSame = FALSE;
    printf ("***** %s\n", f1);
    dump (buffer1, 0, xc);
    printf ("***** %s\n", f2);
    dump (buffer2, 0, yp);
    printf ("*****\n\n");

    l1 = adjust (buffer1, l1, xc);
    l2 = adjust (buffer2, l2, yp);

    /* KLUDGE ALERT!! GOTO USED */
    goto l0;
  }
  i = min (l1-xp, l2-yc);
  i = min (i, ctSync);

  if (compare (l1, xp, l2, yc, i))
  {
    fSame = FALSE;
    printf ("***** %s\n", f1);
    dump (buffer1, 0, xp);
    printf ("***** %s\n", f2);
    dump (buffer2, 0, yc);
    printf ("*****\n\n");

    l1 = adjust (buffer1, l1, xp);
    l2 = adjust (buffer2, l2, yc);

    /* KLUDGE ALERT!! GOTO USED */
    goto l0;
  }
  if (++xp > xc)
  {
    xp = 1;
    if (++xc >= l1)
    {
      xc = l1;
      xd = TRUE;
    }
  }
  if (++yp > yc)
  {
    yp = 1;
    if (++yc >= l2)
    {
      yc = l1;
      yd = TRUE;
    }
  }
  if (!xd || !yd)
    goto l6;
  fSame = FALSE;

  if (l1 >= cLine || l2 >= cLine)
    printf ("%s\n", ReSyncMes);

  printf ("***** %s\n", f1);
  dump (buffer1, 0, l1-1);
  printf ("***** %s\n", f2);
  dump (buffer2, 0, l2-1);
  printf ("*****\n\n");
  free((void *)buffer1);
  free((void *)buffer2);
  fclose(fh1);
  fclose(fh2);
  return;
}


/**************************************************************************/
/* Return number of lines read in.                                        */
/**************************************************************************/

xfill (struct lineType *pl, FILE *fh, int ct, int *plnum)
{
  int i;

#ifdef  DEBUG
  if (fDebug)
    printf ("xfill (%04x, %04x)\n", pl, fh);
#endif

  i = 0;
  while (ct-- && (*funcRead) (pl->text, MAXARG, fh) != NULL)
  {
    if (funcRead == (int (*) ())FNADDR(fgets))
      pl->text[strlen(pl->text)-1] = 0;
    if (fIgnore && !strcmps (pl->text, ""))
    {
      pl->text[0] = 0;
      ++*plnum;
    }
    if (strlen (pl->text) != 0 || !fIgnore)
    {
      pl->line = ++*plnum;
      pl++;
      i++;
    }
  }

#ifdef  DEBUG
  if (fDebug)
    printf ("xfill returns %d\n", i);
#endif

  return (i);
}


/**************************************************************************/
/* Adjust returns number of lines in buffer.                              */
/**************************************************************************/

adjust (struct lineType *pl, int ml, int lt)
{
#ifdef  DEBUG
  if (fDebug)
    printf ("adjust (%04x, %d, %d) = ", pl, ml, lt);
  if (fDebug)
    printf ("%d\n", ml-lt);
#endif

  if (ml <= lt)
    return (0);

#ifdef  DEBUG
  if (fDebug)
    printf ("move (%04x, %04x, %04x)\n", &pl[lt], &pl[0], sizeof (*pl)*(ml-lt));
#endif

  Move((unsigned char far *)&pl[lt], (char far *)&pl[0], sizeof (*pl)*(ml-lt));
  return ml-lt;
}


/**************************************************************************/
/* dump                                                                   */
/*      dump outputs a range of lines.                                    */
/*                                                                        */
/*  INPUTS                                                                */
/*          pl      pointer to current lineType structure                 */
/*          start   starting line number                                  */
/*          end     ending line number                                    */
/*                                                                        */
/*  CALLS                                                                 */
/*          pline, printf                                                 */
/**************************************************************************/

void dump (struct lineType *pl, int start, int end)
{
  if (fAbbrev && end-start > 2)
  {
    pline (pl+start);
    printf ("...\n");
    pline (pl+end);
  }
  else
  {
    while (start <= end)
      pline (pl+start++);
  }
}


/**************************************************************************/
/* PrintLINE                                                              */
/*      pline prints a single line of output.  If the /n flag             */
/*  has been specified, the line number of the printed text is added.     */
/*                                                                        */
/*  Inputs                                                                */
/*          pl      pointer to current lineType structure                 */
/*          fNumb   TRUE if /n specified                                  */
/**************************************************************************/

void pline (struct lineType *pl)
{
  if (fNumb)
    printf ("%5d:  ", pl->line);
  printf ("%s\n", pl->text);
}


/**************************************************************************/
/*        strcmpi will compare two string lexically and return one of     */
/*  the following:                                                        */
/*    - 0    if the strings are equal                                     */
/*    - 1    if first > the second                                        */
/*    - (-1) if first < the second                                        */
/*                                                                        */
/*	This was written to replace the run time library version of       */
/*  strcmpi which does not correctly compare the european character set.  */
/*  This version relies on a version of toupper which uses IToupper.      */
/**************************************************************************/

int strcmpi(unsigned char *str1, unsigned char *str2)
{
  unsigned char c1, c2;

#ifdef DBCS
  while (TRUE)
  {
    c1 = *str1++;
    c2 = *str2++;
    if (c1 == '\0' || c2 == '\0')
      break;
    if (IsDBCSLeadByte(c1) && IsDBCSLeadByte(c2))
    {
      if (c1 == c2)
      {
        c1 = *str1++;
        c2 = *str2++;
        if (c1 != c2)
          break;
      }
      else
        break;
    }
    else if (IsDBCSLeadByte(c1) || IsDBCSLeadByte(c2))
      return (IsDBCSLeadByte(c1) ? 1 : -1);
    else
      if ((c1 = toupper(c1)) != (c2 = toupper(c2)))
        break;
  }
  return (c1 == c2 ? 0 : (c1 > c2 ? 1 : -1));
#else
  while ((c1 = toupper(*str1++)) == (c2 = toupper(*str2++)))
  {
    if (c1 == '\0')
      return (0);
  }

  if (c1 > c2)
    return (1);
  else
    return (-1);
#endif
}


/**************************************************************************/
/* Skip white space.  Return pointer to first non-white character.        */
/**************************************************************************/
unsigned char *skipwhite(unsigned char *p)
{
#ifdef DBCS
  while (*p)
  {
    if (ISSPACE(*p))
      p++;
    else if (*p == DB_SP_HI && *(p+1) == DB_SP_LO)
    {
      *p++ = ' ';
      *p++ = ' ';
    }
    else
      break;
  }
#else
  while (ISSPACE(*p))
    p++;
#endif
  return(p);
}


/**************************************************************************/
/* Compare two strings, ignoring white space, case is significant, return */
/* 0 if identical, <>0 otherwise.  Leading and trailing white space is    */
/* ignored, internal white space is treated as single characters.         */
/**************************************************************************/
strcmps (unsigned char *p1, unsigned char *p2)
{
  unsigned char *q;

  p1 = skipwhite(p1); 		     /* skip any leading white space */
  p2 = skipwhite(p2);

  while (TRUE)
  {
    if (*p1 == *p2)
    {
      if (*p1++ == 0)             /* quit if at the end */
        return (0);
      else
        p2++;

#ifdef DBCS
      if (checkspace(p1))
#else
      if (ISSPACE(*p1))           /* compress multiple spaces */
#endif
      {
        q = skipwhite(p1);
	p1 = (*q == 0) ? q : q - 1;
      }

#ifdef DBCS
      if (checkspace(p2))
#else
      if (ISSPACE(*p2))
#endif
      {
        q = skipwhite(p2);
	p2 = (*q == 0) ? q : q - 1;
      }
    }
    else
      return *p1-*p2;
  }
}


/**************************************************************************/
/* Compare two strings, ignoring white space, case is not significant,    */
/* return 0 if identical, <>0 otherwise.  Leading and trailing white      */
/* space is ignored, internal white space is treated as single characters.*/
/**************************************************************************/
strcmpis (unsigned char *p1, unsigned char *p2)
{
  unsigned char *q;
#ifdef DBCS
  unsigned char c1,c2;
#endif

  p1 = skipwhite(p1);                  /* skip any leading white space */
  p2 = skipwhite(p2);

  while (TRUE)
  {
#ifdef DBCS
    c1 = *p1;
    c2 = *p2;

    if (IsDBCSLeadByte(c1) && IsDBCSLeadByte(c2) && c1 == c2)
    {
      c1 = *++p1;
      c2 = *++p2;
    }
    else if (IsDBCSLeadByte(c1) || IsDBCSLeadByte(c2))
      return (c1 - c2);
    else
    {
      c1 = toupper(c1);
      c2 = toupper(c2);
    }
    if (c1 == c2)
#else
      if (toupper(*p1) == toupper(*p2))
#endif
      {
        if (*p1++ == 0)                /* quit if at the end */
	  return (0);
	else
	  p2++;
#ifdef DBCS
	if (checkspace(p1))
#else
	if (ISSPACE(*p1))              /* compress multiple spaces */
#endif
	{
	  q = skipwhite(p1);
	  p1 = (*q == 0) ? q : q - 1;
	}
#ifdef DBCS
	if (checkspace(p2))
#else
	if (ISSPACE(*p2))
#endif
	{
	  q = skipwhite(p2);
	  p2 = (*q == 0) ? q : q - 1;
	}
      }
      else
        return *p1-*p2;
  }
}


/**************************************************************************/
/* Routine:  has_extension                                                */
/* Arguments: an arbitrary string                                         */
/* Function: check for extensions in the string.                          */
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
/* Routine:   ParseFileNames                                         */
/*                                                                   */
/* Function:  Parses the two given filenames and then compares the   */
/*            appropriate filenames.  This routine handles wildcard  */
/*            characters in both filenames.                          */
/*********************************************************************/

void ParseFileNames(char *file1, char *file2)
{
  struct find_t struct1;                    /* for findfirst/findnext */
  char final1[MAXFNAME], final2[MAXFNAME];  /* final path and filename */
  char inter2[MAXFNAME];                    /* intermediate filename for file2 */
  char *fname1, *fname2;                    /* filename begins here */
  int Wildcard2;                            /* if file2 contains a wildcard */


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
    printf("%s%s\n", msg_tbl[FILES_NOT_FOUND_MSG], file1);
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
      if (ExpandFile2(fname1, fname2))           /* M001 */
      {                                /* expansion of file2 failed */
        printf("%s     %s\n%s\n\n\n", final1, file2, msg_tbl[COULD_NOT_EXP_MSG]);
      }
      else
        comp(final1, final2);          /* compare the two files */
      
      strcpy(final2, inter2);          /* recopy original with wildcards */
    }                                            /* M001 */
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
    if (strlen(path) <= (MAXFNAME - 3))
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
/* M001                                                              */
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
/* Routine:   comp                                                   */
/*                                                                   */
/* Function:  Compares the two files.                                */
/*********************************************************************/

void comp(char *file1, char *file2)
{
  printf("%s%s%s%s\n",msg_tbl[COMPARING], file1, msg_tbl[AND], file2);
  if (fBinary)
    BinaryCompare (file1, file2);
  else
    LineCompare (file1, file2);

  printf("\n");
}	



#ifdef DBCS

/**************************************************************************/
/* Routine:  checkspace                                                   */
/* Arguments: an arbitrary string                                         */
/* Function: Determine whether there is a space in the string.            */
/* Side effects: none                                                     */
/**************************************************************************/

int checkspace(unsigned char *s)
{
  if (ISSPACE(*s) || (*s == DB_SP_HI && *(s+1) == DB_SP_LO))
    return (TRUE);
  else
    return (FALSE);
}

#endif

