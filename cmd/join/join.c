;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
/*    */
/**************************************************************************/
/*                                                                        */
/*  UTILITY NAME:      Join                                               */
/*                                                                        */
/*  SOURCE FILE NAME:  Join.C                                             */
/*                                                                        */
/*  STATUS:            Join Utility, DOS Version 4.0                      */
/*                                                                        */
/*  FUNCTIONAL DESCRIPTION:  This utility allows the splicing of a        */
/*    physical drive to a pathname on another physical drive such that    */
/*    operations performed using the pathname as an argument take place   */
/*    on the physical drive.                                              */
/*                                                                        */
/*  SYNTAX:            [d:][path]JOIN                      or             */
/*                     [d:][path]JOIN d: d:\directory      or             */
/*                     [d:][path]JOIN d: /D                               */
/*            where:                                                      */
/*                     [d:][path] to specify the drive and path that      */
/*                     contains the JOIN command file, if it is not       */
/*                     in the current directory of the default drive.     */
/*                                                                        */
/*                     d: to specify the drive to be connected to a       */
/*                     directory on another drive.                        */
/*                                                                        */
/*                     d:\directory to specify the directory that         */
/*                     you will join a drive under.  The directory        */
/*                     must be at the root and only one level deep.       */
/*                                                                        */
/*                     /D to disconnect a join.  You must specify the     */
/*                        drive letter of the drive whose join you        */
/*                        want to delete.                                 */
/*                                                                        */
/*  LINKS:                                                                */
/*    CDS.C       - Functions to get/set DOS CDS structures               */
/*    DPB.C       - Functions to get DOS DPB structures                   */
/*    ERRTST.C    - Drive and path validity testing functions             */
/*    SYSVAR.C    - Functions to get/set DOS System Variable structures   */
/*    COMSUBS.LIB - DOS DBCS function calls                               */
/*    MAPPER.LIB  - DOS function calls                                    */
/*    SLIBC3.LIB  - C library functions                                   */
/*    _MSGRET.SAL - Assembler interface for common DOS message services   */
/*    _PARSE.SAL  - Assembler interface for common DOS parser             */
/*                                                                        */
/*  ERROR HANDLING:    Error message displayed and utility is terminated. */
/*                                                                        */
/**************************************************************************/

#include <string.h>

#include "cds.h"
#include "ctype.h"
#include "dos.h"
#include "joinpars.h"                  /* Parser structures */
#include "jointype.h"
#include "stdio.h"
#include "sysvar.h"
#include "join.h"


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   main (program entry point)                         */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Preload message file                            */
/*                        Get the command line parameters                 */
/*                        Parse the command line by calling SYSPARSEC     */
/*                        Verify the correctness of the parameters        */
/*                        Check for splice deletion switch                */
/*                        Determine if directory not empty                */
/*                        Check source and destination drives not same    */
/*                        Determine if network or shared drive            */
/*                        Determine if currently spliced                  */
/*                        Determine if existing dir or can't mkdir        */
/*                        Print messages by calling SYSDISPMSG            */
/*                                                                        */
/*  EXTERNAL ROUTINES:    SYSLOADMSG                                      */
/*                        SYSDISPMSG                                      */
/*                        SYSPARSE                                        */
/*                                                                        */
/**************************************************************************/

int main(c, v)
int c;
char *v[];
{
  struct CDSType CDS;                  /* pointer to CDS structure */
  int delflag;                         /* delete splice flag */
  int drv;                             /* drive number */
  int index;                           /* loop counter */

  
  load_msg();                               /* Point to msgs & chks DOS ver */
  for (index = 1; index <= c; index++)      /* Loop through end of cmd line */
  {
    strcat(source,v[index]);                /* Add the argument */
    strcat(source," ");                     /* Separate with a space */
  }
  Parser_Prep(source);                      /* Initialization for the parser */

  delflag = ParseIt();

  GetVars(&SysVars);                        /* Access to DOS data structures */
  strcpy(fix_es_reg,NULL);                  /* (Set es reg correct) */

  if (c == 1)
    DoList();                               /* list splices */
  else
  {
    drv = p_drive[0] - 'A';                 /* Convert to drive # */
    if (!fGetCDS(drv, &CDS))
      dispmsg_terminate(MSG_BADPARM,cmdln_drive);

    strcpy(fix_es_reg,NULL);                /* (Set es reg correct) */
    if (delflag)                            /* Deassigning perhaps? */
      Delete(CDS,drv);
    else
      Insert(CDS,drv);
  }
  exit(0);
  return(0);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   ParseIt                                            */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Parse the command line entered by the user.     */
/*                        Returns whether or not the /d switch was used.  */
/*                                                                        */
/*  INPUT:             none                                               */
/*                                                                        */
/*  OUTPUT:            command line is parsed                             */
/*                     returns whether or not /d switch was used          */
/*                                                                        */
/**************************************************************************/

int ParseIt()
{
  char far * fptr;                     /* Pointer to parser's buffer */
  int delflag       = FALSE;           /* delete splice flag */
  int fchar         = 0;               /* Parser filespec chars */
  int index;                           /* Used in creating cmdline string */
  int more_to_parse = TRUE;            /* While parsing cmdline */
  int pdrive_flg    = FALSE;           /* Is there a drive letter? */
  int pflspec_flg   = FALSE;           /* Is there a filespec? */

  
  while (more_to_parse)                      /* test the flag */
  {
    index = 0;                              /* Init array index */
    parse(&inregs,&outregs);                /* call the parser */
    if (outregs.x.ax == P_No_Error)         /* if no error */
    {
      /* if result is drive letter */
      if (outregs.x.dx == (unsigned short)&rslt1)
      {
        /* save the drive letter */
        p_drive[0] = (char)*(rslt1.p_result_buff);
        p_drive[0] += (char)ASCII_DRIVE;
        p_drive[1] = COLON;
        pdrive_flg = TRUE;                  /* and set the flag */
        /* Copy whatever parser just parsed */
        for (inregs.x.si; inregs.x.si < outregs.x.si; inregs.x.si++)
        {
          cmdln_drive[index] = *(char *)inregs.x.si;
          index++;
        }
      }
      else
      {
        /* if result is filespec */
        if (outregs.x.dx == (unsigned short)&rslt2)
        {
          /* Point to parser's buffer */
          for (fptr = rslt2.fp_result_buff; (char)*fptr != NULL; fptr++)
          {
            p_filespec[fchar] = (char)*fptr;     /* Copy char */
            fchar++;
          }
          strcpy(fix_es_reg,NULL);               /* (Set es reg correct) */
          pflspec_flg = TRUE;                    /* and set the flag */
          /* Copy whatever parser just parsed */
          for (inregs.x.si; inregs.x.si < outregs.x.si; inregs.x.si++)
          {
            cmdln_flspec[index] = *(char *)inregs.x.si;
            index++;
          }
        }
        else   /* must be &rslt3 (switches) */
        {
	  if ((char *)rslt3.P_SYNONYM_Ptr == p_swi1.sp_keyorsw)      /* /D */
	  {
	    if (!delflag)                   /* Check for dup switch */
	      delflag = TRUE;               /* it's /D switch */
	    else                            /* else it's a dup switch */
	      dispmsg_terminate(MSG_INVSWTCH, p_swi1.sp_keyorsw);
	  }
	  else   /* assume it's /? */
            DisplayOptionsExit();
        }
      }
    }
    else
    {
      if (outregs.x.ax != P_RC_EOL)         /* there must be an error */
      {
        /* Copy whatever parser just parsed */
        for (inregs.x.si; inregs.x.si < outregs.x.si; inregs.x.si++)
        {
          cmdln_invalid[index] = *(char *)inregs.x.si;
          index++;
        }
        switch (outregs.x.ax)               /* See what error parser may have found */
        {
          case P_Too_Many :
              /* Too Many Parameters - more_to_parse = FALSE */
              dispmsg_terminate(MSG_PARMNUM,cmdln_invalid);
              break;
          case P_Not_In_SW :
              /* Invalid Switch - more_to_parse = FALSE */
              dispmsg_terminate(MSG_INVSWTCH,cmdln_invalid);
              break;
          case P_Op_Missing   :             /* Required operand missing */
          case P_Not_In_Key   :             /* Not in kywrd list provided */
          case P_Out_Of_Range :             /* Out of range specified */
          case P_Not_In_Val   :             /* Not in val list provided */
          case P_Not_In_Str   :             /* Not in strg list provided */
          case P_Syntax       :             /* Incorrect syntax */
              /* more_to_parse = FALSE */
              dispmsg_terminate(MSG_BADPARM,cmdln_invalid);
              break;
          default :
              /* more_to_parse = FALSE */
              dispmsg_terminate(MSG_BADPARM,cmdln_invalid);
              break;
        }
      }
      else
        more_to_parse = FALSE;               /* End of the cmdline */
    }
    inregs.x.cx = outregs.x.cx;             /* Move the count */
    inregs.x.si = outregs.x.si;             /* Move the pointer */
  }

  /* If drive & no flspec or delete, then display error msg & exit utility */
  if (pdrive_flg && !(pflspec_flg || delflag))
    dispmsg_terminate(MSG_BADPARM,cmdln_drive);

  /* If filespec & no drive, then display error msg & exit utility */
  if (pflspec_flg && !pdrive_flg)
    dispmsg_terminate(MSG_BADPARM,cmdln_flspec);

  /* If delete & no drive, then display error msg & exit utility */
  if (delflag && !pdrive_flg)
    dispmsg_terminate(MSG_BADPARM, p_swi1.sp_keyorsw);

  /* If drive, filespec, & /D, then display error msg & exit utility */
  if (pdrive_flg && pflspec_flg && delflag)
    dispmsg_terminate(MSG_PARMNUM, p_swi1.sp_keyorsw);

  return(delflag);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   DoList                                             */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Prints list of current joins                    */
/*                                                                        */
/*  INPUT:             none                                               */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/**************************************************************************/

void DoList()
{
  int i;                                    /* loop counter */
  struct CDSType CDS;                       /* pointer to CDS structure */

  for (i=0; fGetCDS(i, &CDS); i++)
  {
    if (TESTFLAG(CDS.flags,CDSSPLICE))
      printf("%c: => %s\n", i+'A', CDS.text);
  }
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   Delete                                             */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Cancel join command for drive specified.        */
/*                                                                        */
/*  INPUT:             CDS - pointer to CDS structure                     */
/*                     drv - drive number                                 */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/**************************************************************************/

void Delete(CDS,drv)
struct CDSType CDS;
int drv;
{
  if (!TESTFLAG(CDS.flags, CDSSPLICE))  /* If NOT spliced */
    dispmsg_terminate(MSG_BADPARM, p_swi1.sp_keyorsw);

  if (fPathErr(CDS.text))               /* If prefix of current directory */
    dispmsg_terminate(MSG_BADPARM,cmdln_drive);

  CDS.text[0] = (char)(drv) + 'A';
  CDS.text[1] = ':';
  CDS.text[2] = '\\';
  CDS.text[3] = 0;
  CDS.cbEnd = 2;

  if (drv >= SysVars.cDrv)
    CDS.flags = FALSE;
  else
    CDS.flags = CDSINUSE;
  GetVars(&SysVars);
  strcpy(fix_es_reg,NULL);              /* (Set es reg correct) */
  SysVars.fSplice--;
  PutVars(&SysVars);
  strcpy(fix_es_reg,NULL);              /* (Set es reg correct) */
  fPutCDS(drv, &CDS);
  strcpy(fix_es_reg,NULL);              /* (Set es reg correct) */
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   Insert                                             */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Joins a disk drive to a directory on another    */
/*                        disk drive.                                     */
/*                                                                        */
/*  INPUT:             CDS - pointer to CDS structure                     */
/*                     drv - drive number                                 */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/**************************************************************************/

void Insert(CDS,drv)
struct CDSType CDS;
int drv;
{
  struct findType findbuf;             /* findfirst structure */
  char path [MAXPATHLEN],*p;           /* pointers to path */
  int dstdrv;                          /* dest. drive number */

  
  if (TESTFLAG(CDS.flags,CDSSPLICE))    /* If now spliced */
    dispmsg_terminate(MSG_BADPARM,cmdln_drive);

  rootpath(p_filespec,path);            /* Get root path */

  /* Start - Can't move current drive */
  if (drv == getdrv() || !fPhysical(drv) || fShared(drv))
  {                                     /* Determine if NET error */
    if (fNet(drv) || fShared(drv))
      dispmsg_terminate(MSG_NETERR, NULL);
    dispmsg_terminate(MSG_BADPARM,cmdln_drive);
  }

  strcpy(fix_es_reg,NULL);              /* (Set es reg correct) */
  /* OR current directory prefix */
  if (fPathErr(path) || *strbscan(path+3, "/\\") != 0)
    dispmsg_terminate(MSG_BADPARM,cmdln_flspec);

  strcpy(fix_es_reg,NULL);              /* (Set es reg correct) */
  if (fNet(path[0] - 'A') || fShared(path[0] - 'A'))
    dispmsg_terminate(MSG_NETERR, NULL);

  strcpy(fix_es_reg,NULL);              /* (Set es reg correct) */
  dstdrv = *path - 'A';                 /* Check src and dst drvs different */
  if (drv == dstdrv)                      
    dispmsg_terminate (MSG_BADPARM,cmdln_flspec);
  if (mkdir(path) == -1)                /* If can't mkdir or if no dir or */
  {                                     /* if note is file  */
    if (ffirst(path, A_D, &findbuf) == -1 || !TESTFLAG(findbuf.attr,A_D))
      dispmsg_terminate(MSG_BADPARM,cmdln_flspec);

    p = path + strlen(path);
    strcat(p, "\\*.*");

    if (ffirst(path, 0, &findbuf) != -1)     /* If dir not empty */
      dispmsg_terminate(MSG_DIRNEMP,cmdln_flspec);

    *p = 0;
  }

  strcpy(CDS.text, path);
  CDS.flags = CDSINUSE | CDSSPLICE;
  fPutCDS(drv, &CDS);
  strcpy(fix_es_reg,NULL);              /* (Set es reg correct) */
  GetVars(&SysVars);
  strcpy(fix_es_reg,NULL);              /* (Set es reg correct) */
  SysVars.fSplice++;
  PutVars(&SysVars);
  strcpy(fix_es_reg,NULL);              /* (Set es reg correct) */
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   load_msg                                           */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Load the set of SUBST Utility messages to       */
/*                        become available for display_msg call.          */
/*                                                                        */
/*  ERROR EXIT:        Utility will be terminated by sysloadmsg if        */
/*                     version check is incorrect.                        */
/*                                                                        */
/*  EXTERNAL REF:      SYSLOADMSG                                         */
/*                                                                        */
/**************************************************************************/

void load_msg()
{
   sysloadmsg(&inregs,&outregs);            /* Load utility messages */
   if (outregs.x.cflag & CARRY)             /* If problem loading msgs */
   {
     sysdispmsg(&outregs,&outregs);         /* then display the err msg */
     exit(ERRORLEVEL1);                     /* and exit utility */
   }
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   display_msg                                        */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  The correct message called by main is displayed */
/*                        to standard out.                                */
/*                                                                        */
/*  INPUT:             msg_num   (message number to display)              */
/*                     outline   (substitution parameter)                 */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/*  ERROR EXIT:        Display error message corresponding to number      */
/*                     returned in AX.                                    */
/*                                                                        */
/*  EXTERNAL REF:      SYSDISPMSG                                         */
/*                                                                        */
/**************************************************************************/

void display_msg(msg_num,outline)
int msg_num;                                /* Message number #define'd */
char *outline;                              /* Substitution parameter */
{
  unsigned char function;                   /* Y/N response or press key? */
  unsigned int message,                     /* Message number to display */
               msg_class,                   /* Which class of messages? */
               sub_cnt,                     /* Number of substitutions? */
               handle;                      /* Display where? */

  struct sublist
  {
    unsigned char size;                     /* Points to next sublist */
    unsigned char reserved;                 /* Required for disp msg */
    unsigned far *value;                    /* Data pointer */
    unsigned char id;                       /* Id of substitution parm (%1) */
    unsigned char flags;                    /* Format of data - (a0sstttt) */
    unsigned char max_width;                /* Maximum field width */
    unsigned char min_width;                /* Minimum field width */
    unsigned char pad_char;                 /* char to pad field */
  } sublist;

  switch (msg_num)                          /* Which msg to display? */
  {
    case MSG_NOMEM :
        function  = NO_INPUT;               /* Y/N response or press key? */
        message   = 8;                      /* Message number to display */
        msg_class = EXT_ERR_CLASS;          /* Which class of messages? */
        sub_cnt   = SUBCNT0;                /* Number of substitutions? */
        handle    = STDERR;                 /* Display where? */
        break;
    case MSG_PARMNUM :
        function  = NO_INPUT;               /* Y/N response or press key? */
        message   = 1;                      /* Message number to display */
        msg_class = PARSE_ERR_CLASS;        /* Which class of messages? */
        sub_cnt   = SUBCNT1;                /* Number of substitutions? */
        handle    = STDERR;                 /* Display where? */
        break;
    case MSG_DIRNEMP :
        function  = NO_INPUT;               /* Y/N response or press key? */
        message   = 2;                      /* Message number to display */
        msg_class = UTILITY_CLASS;          /* Which class of messages? */
        sub_cnt   = SUBCNT1;                /* Number of substitutions? */
        handle    = STDERR;                 /* Display where? */
        break;
    case MSG_BADPARM :
        function  = NO_INPUT;               /* Y/N response or press key? */
        message   = 10;                     /* Message number to display */
        msg_class = PARSE_ERR_CLASS;        /* Which class of messages? */
        sub_cnt   = SUBCNT1;                /* Number of substitutions? */
        handle    = STDERR;                 /* Display where? */
        break;
    case MSG_NETERR :
        function  = NO_INPUT;               /* Y/N response or press key? */
        message   = 12;                     /* Message number to display */
        msg_class = UTILITY_CLASS;          /* Which class of messages? */
        sub_cnt   = SUBCNT1;                /* Number of substitutions? */
        handle    = STDERR;                 /* Display where? */
        break;
    case MSG_INVSWTCH :
        function  = NO_INPUT;               /* Y/N response or press key? */
        message   = 3;                      /* Message number to display */
        msg_class = PARSE_ERR_CLASS;        /* Which class of messages? */
        sub_cnt   = SUBCNT1;                /* Number of substitutions? */
        handle    = STDERR;                 /* Display where? */
        break;

    default:
        if (msg_num >= MSG_OPTIONS_FIRST && msg_num <= MSG_OPTIONS_LAST)
        {
          function  = NO_INPUT;
          message   = msg_num;
          msg_class = UTILITY_CLASS;
          sub_cnt   = SUBCNT0;
          handle    = STDOUT;
        }
        else
          exit(ERRORLEVEL1);
        break;
  }

  switch (msg_num)
  {
    case MSG_NOMEM :
        inregs.x.ax = message;                   /* Insufficient memory */
        inregs.x.bx = handle;                    /* STDERR */
        inregs.x.cx = sub_cnt;                   /* SUBCNT0 */
        inregs.h.dl = function;                  /* NO_INPUT */
        inregs.h.dh = (unsigned char)msg_class;  /* Extended, Parse or Utility */
        sysdispmsg(&inregs,&outregs);            /* Call common msg service */
        break;
    case MSG_INVSWTCH :                          /* Invalid switch */
    case MSG_DIRNEMP  :                          /* Directory not empty */
    case MSG_PARMNUM  :                          /* Too many parameters */
    case MSG_BADPARM  :
        sublist.value = (unsigned far *)outline;      /* Invalid parameter */
        sublist.size = SUBLIST_LENGTH;
        sublist.reserved = RESERVED;
        sublist.id = 0;
        sublist.flags = STR_INPUT;
        sublist.max_width = MAXWIDTH;
        sublist.min_width = MINWIDTH;
        sublist.pad_char = (unsigned char)BLNK;
        inregs.x.ax = message;                   /* Cannot JOIN a network drive */
        inregs.x.bx = handle;                    /* STDERR */
        inregs.x.si = (unsigned int)&sublist;    /* Point to substitution buffer */
        inregs.x.cx = sub_cnt;                   /* SUBCNT1 */
        inregs.h.dl = function;                  /* STR_INPUT */
        inregs.h.dh = (unsigned char)msg_class;  /* Extended, Parse or Utility */
        sysdispmsg(&inregs,&outregs);            /* Call common msg service */
        break;
    case MSG_NETERR :
        sublist.value = (unsigned far *)replparm_JOIN;  /* Cannot JOIN net drive */
        sublist.size = SUBLIST_LENGTH;
        sublist.reserved = RESERVED;
        sublist.id = SUB_ID1;
        sublist.flags = STR_INPUT;
        sublist.max_width = MAXWIDTH;
        sublist.min_width = MINWIDTH;
        sublist.pad_char = (unsigned char)BLNK;
        inregs.x.ax = message;                   /* Cannot JOIN network drive */
        inregs.x.bx = handle;                    /* STDERR */
        inregs.x.si = (unsigned int)&sublist;    /* Point to substitution buffer */
        inregs.x.cx = sub_cnt;                   /* SUBCNT1 */
        inregs.h.dl = function;                  /* STR_INPUT */
        inregs.h.dh = (unsigned char)msg_class;  /* Extended, Parse or Utility */
        sysdispmsg(&inregs,&outregs);            /* Call common msg service */
        break;
    default:
        if (msg_num >= MSG_OPTIONS_FIRST && msg_num <= MSG_OPTIONS_LAST)
        {
          inregs.x.ax = message;
          inregs.x.bx = handle;
          inregs.x.cx = sub_cnt;
          inregs.h.dl = function;
          inregs.h.dh = (unsigned char)msg_class;
          sysdispmsg(&inregs,&outregs);
        }
        else
          exit(ERRORLEVEL1);
        break;
  }

  if (outregs.x.cflag & CARRY)              /* Is the carry flag set? */
  {                                         /* Then setup regs for extd-err */
    inregs.x.bx = STDERR;
    inregs.x.cx = SUBCNT0;
    inregs.h.dl = NO_INPUT;
    inregs.h.dh = EXT_ERR_CLASS;
    sysdispmsg(&inregs,&outregs);           /* Call to display ext_err msg */
    exit(ERRORLEVEL1);
  }
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   DisplayOptionsExit                                 */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Display the options help message to the user,   */
/*                        and then exit so they may try again.            */
/*                                                                        */
/*  INPUT:             no value passed.                                   */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/*  ERROR EXIT:        none                                               */
/*                                                                        */
/*  EXTERNAL REF:      display_msg()                                      */
/*                     exit()                                             */
/*                                                                        */
/**************************************************************************/

void DisplayOptionsExit()
{
  int MsgNbr;

  /*
   * Display all the message lines in the
   * options help message.
   */
  for (MsgNbr = MSG_OPTIONS_FIRST; MsgNbr <= MSG_OPTIONS_LAST; MsgNbr++)
    display_msg(MsgNbr, (char *)0);

  /*
   * Exit so the user may try again.
   */
  exit(0);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dispmsg_terminate                                  */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Display the message, then terminate the utility.*/
/*                                                                        */
/*  INPUT:             msg_num     (#define'd message to display)         */
/*                     outline     (substitution parameter)               */
/*                                                                        */
/**************************************************************************/

void dispmsg_terminate(msg_num,outline)
int msg_num;                                /* Message number #define'd */
char *outline;                              /* Substitution parameter */
{
  display_msg(msg_num,outline);             /* First, display the msg */
  exit(ERRORLEVEL1);                        /* Then, terminate utility */
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   Parser_Prep                                        */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Initialize all structures for the parser.       */
/*                                                                        */
/*  INPUT:             source (command line string)                       */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/*  EXTERNAL REF:      parse                                              */
/*                                                                        */
/**************************************************************************/

void Parser_Prep(source)
char *source;                                    /* Commandline */
{
  p_p.p_parmsx_address = &p_px;                  /* Addr of extended parm list */
  p_p.p_num_extra = 0;                           /* No extra declarations */

  p_px.p_minp = MINPOSITION;
  p_px.p_maxp = MAXPOSITION;
  p_px.p_control1 = &p_con1;                     /* Point to 1st control blk */
  p_px.p_control2 = &p_con2;                     /* Point to 2nd control blk */
  p_px.p_maxs = 2;                               /* Specify # of switches */
  p_px.p_switch1 = &p_swi1;                      /* Point to the switch blk */
  p_px.p_switch2 = &p_swi2;                      /* Point to the switch blk */
  p_px.p_maxk = 0;                               /* Specify # of keywords */

  p_con1.p_match_flag = DRVONLY_OPT;             /* Drive only & optional */
  p_con1.p_function_flag = NOCAPPING;            /* Cap result by file table */
  p_con1.p_result_buf = (unsigned int)&rslt1;    /* Point to result blk */
  p_con1.p_value_list = (unsigned int)&novals;   /* Point to no value list */
  p_con1.p_nid = 0;                              /* Not a switch id */

  p_con2.p_match_flag = FILESPEC_OPT;            /* File spec & optional */
  p_con2.p_function_flag = CAPRESULT;            /* Cap result by file table */
  p_con2.p_result_buf = (unsigned int)&rslt2;    /* Point to result blk */
  p_con2.p_value_list = (unsigned int)&novals;   /* Point to no value list */
  p_con2.p_nid = 0;                              /* Not a switch id */

  p_swi1.sp_match_flag = SWITCH_OPT;             /* Optional (switch) */
  p_swi1.sp_function_flag = NOCAPPING;           /* Cap result by file table */
  p_swi1.sp_result_buf = (unsigned int)&rslt3;   /* Point to result blk */
  p_swi1.sp_value_list = (unsigned int)&novals;  /* Point to no value list */
  p_swi1.sp_nid = 1;                             /* One switch allowed */
  strcpy(p_swi1.sp_keyorsw,D_SWITCH);            /* Identify the switch */

  p_swi2.sp_match_flag = SWITCH_OPT;             /* Optional (switch) */
  p_swi2.sp_function_flag = NOCAPPING;           /* Cap result by file table */
  p_swi2.sp_result_buf = (unsigned int)&rslt3;   /* Point to result blk */
  p_swi2.sp_value_list = (unsigned int)&novals;  /* Point to no value list */
  p_swi2.sp_nid = 1;                             /* One switch allowed */
  strcpy(p_swi2.sp_keyorsw,O_SWITCH);            /* Identify the switch */

  inregs.x.cx = 0;                               /* Operand ordinal */
  inregs.x.di = (unsigned int)&p_p;              /* Address of parm list */
  inregs.x.si = (unsigned int)source;            /* Make DS:SI point to source */
}

