;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*    */
/**************************************************************************/
/*                                                                        */
/*  UTILITY NAME:      Subst                                              */
/*                                                                        */
/*  SOURCE FILE NAME:  Subst.C						  */
/*                                                                        */
/*  STATUS:            Subst Utility, DOS Version 4.0                     */
/*                                                                        */
/*  FUNCTIONAL DESCRIPTION:  This utility allows the substitution of a    */
/*  physical drive for a pathname on another drive such that operations   */
/*  performed using the physical drive as an argument take place on the   */
/*  pathname.                                                             */
/*                                                                        */
/*  SYNTAX:            [d:][path]SUBST                     or             */
/*                     [d:][path]SUBST d: d:path           or             */
/*                     [d:][path]SUBST d: /D                              */
/*            where:                                                      */
/*                     [d:][path] to specify the drive and path that      */
/*                     contains the SUBST command file                    */
/*                                                                        */
/*                     d: specifies the drive letter that you want        */
/*                     to use to refer to another drive or path.          */
/*                                                                        */
/*                     d:path to specify the drive or path that you       */
/*                     want to refer to with a nickname.                  */
/*                                                                        */
/*                     /D to delete a substitution.  You must specify     */
/*                        the letter of the drive whose substitution      */
/*                        you want to delete.                             */
/*                                                                        */
/*  LINKS:                                                                */
/*    CDS.C       - Functions to get/set DOS CDS structures               */
/*    DPB.C       - Functions to get DOS DPB structures                   */
/*    ERRTST.C    - Drive and path validity testing functions             */
/*    SYSVAR.C    - Functions to get/set DOS System Variable structures   */
/*    COMSUBS.LIB - DOS DBCS function calls                               */
/*    MAPPER.LIB  - DOS function calls                                    */
/*    SLIBC3.LIB  - C library functions                                   */
/*    _MSGRET.ASM - Assembler interface for common DOS message services   */
/*    _PARSE.ASM  - Assembler interface for common DOS parser             */
/*                                                                        */
/*  ERROR HANDLING:    Error message displayed and utility is terminated. */
/*                                                                        */
/**************************************************************************/

#include "cds.h"
#include "dos.h"
#include "fcntl.h"
#include "jointype.h"
#include "malloc.h"
#include "string.h"
#include "substpar.h"                       /* Parser structures */
#include "sysvar.h"
#include "subst.h"


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   main (program entry point)                         */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Preload message file                            */
/*                        Get the command line parameters                 */
/*                        Parse the command line by calling SYSPARSE      */
/*                        Verify the correctness of the parameters        */
/*                        Check for deletion switch                       */
/*                        Check source path and destination not same      */
/*                        Determine if source or destination is network   */
/*                        Determine if currently spliced                  */
/*                        Print messages by calling SYSDISPMSG            */
/*                                                                        */
/*  EXTERNAL ROUTINES:    SYSLOADMSG                                      */
/*                        SYSDISPMSG                                      */
/*                        SYSPARSE                                        */
/*                                                                        */
/**************************************************************************/

void main(c, v)
int c;
char *v[];
{
  int delflag = FALSE;                      /* Deletion specified */


  load_msg();                               /* Point to msgs & chk DOS ver */
  for (index = 1; index <= c; index++)      /* Loop through end of cmd line */
  {
    strcat(source,v[index]);                /* Add the argument */
    strcat(source," ");                     /* Separate with a space */
  }
  Parser_Prep(source);                      /* Initialization for the parser */

  delflag = ParseIt();                      /* Parse the command line */

  GetVars(&SysVars);

  if (c == 1)                               /* display all tree aliases */
    Display();
  else
  {
    if (delflag)                            /* Are we to delete a subst? */
    {
      if (!fDelete(p_drive))
        dispmsg_terminate(MSG_BADPARM,cmdln_drive);
    }
    else
      Insert(p_drive,p_filespec);
  }
  exit(0);
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
  char far * fptr;                          /* Pointer to parser's buffer */
  int delflag       = FALSE;                /* Deletion specified */
  int fchar         = 0;                    /* Parser filespec chars */
  int more_to_parse = TRUE;                 /* While parsing cmdline */
  int pdrive_flg    = FALSE;                /* Is there a drive letter? */
  int pflspec_flg   = FALSE;                /* Is there a filespec? */


  while (more_to_parse)                     /* test the flag */
  {
    index = 0;                              /* Init array index */
    parse(&inregs,&outregs);                /* call the parser */
    if (outregs.x.ax == P_No_Error)         /* if no error */
    {
      /* If result is a drive letter */
      if (outregs.x.dx == (unsigned short)&rslt1)
      {
        /* save the drive letter and set the flag */
        p_drive[0] = (char )(rslt1.p_result_buff[0]);
        p_drive[0] += (char)ASCII_DRIVE;
        p_drive[1] = COLON;
        pdrive_flg = TRUE;
        /* Copy whatever the parser just parsed */
        for (inregs.x.si; inregs.x.si < outregs.x.si; inregs.x.si++)
        {
          cmdln_drive[index] = *(char *)inregs.x.si;
          index++;
        }
      }
      else
      {
        /* If result is filespec */
        if (outregs.x.dx == (unsigned short)&rslt2)
        {
          /* From begginning of buffer til nul */
          for (fptr = rslt2.fp_result_buff; (char)*fptr != NULL; fptr++)
          {
            /* copy from result field buffer */
            p_filespec[fchar] = (char)*fptr;
            fchar++;
          }
          pflspec_flg = TRUE;               /* and set the flag */
          /* Copy whatever the parser just parsed */
          for (inregs.x.si; inregs.x.si < outregs.x.si; inregs.x.si++)
          {
            cmdln_flspec[index] = *(char *)inregs.x.si;
            index++;
          }
        }
        else   /* check for switches */
        {
          /* Copy whatever the parser just parsed */
          for (inregs.x.si; inregs.x.si < outregs.x.si; inregs.x.si++)
          {
            cmdln_switch[index] = *(char *)inregs.x.si;
            index++;
          }
          if (rslt3.P_SYNONYM_Ptr == &p_swi2.sp_keyorsw[0])       /* /? */
          {
            DisplayOptions();               /* show valid options */
            exit(0);                        /* so they can retry  */
          }
          else if (rslt3.P_SYNONYM_Ptr == &p_swi1.sp_keyorsw[0])  /* /D */
          {
            if (!delflag)                   /* Check for dup switch */
              delflag = TRUE;               /* it's /D switch */
            else                            /* else it's a duplicate switch */
              dispmsg_terminate(MSG_INVSWTCH,cmdln_switch);
          }
          else                                                    /* unknown */
            dispmsg_terminate(MSG_INVSWTCH,cmdln_switch);
        }
      }
    }
    else
    {
      if (outregs.x.ax != P_RC_EOL)         /* there must be an error */
      {
        /* Copy whatever the parser just parsed */
        for (inregs.x.si; inregs.x.si < outregs.x.si; inregs.x.si++)
        {
          cmdln_invalid[index] = *(char *)inregs.x.si;
          index++;
        }
        switch (outregs.x.ax)               /* See what error parser found */
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
              display_msg(MSG_BADPARM,cmdln_invalid);
              exit(ERRORLEVEL1);
        }
      }
      else                                  /* End of the cmdline */
        more_to_parse = FALSE;
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
    dispmsg_terminate(MSG_BADPARM,cmdln_switch);

  /* If drive, filespec, & /D, then display error msg & exit utility */
  if (pdrive_flg && pflspec_flg && delflag)
    dispmsg_terminate(MSG_PARMNUM,cmdln_switch);

  return(delflag);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   BackFix                                            */
/*                                                                        */
/**************************************************************************/

char *BackFix(p)
char *p;
{
  char *p1;
  char *p2;

  p2 = p-1;
  while (*(p2 = strbscan(p1 = p2+1,"\\")) != NULL);

  /* p1 points to char after last path sep.                */
  /* If this is a NULL, p already has a trailing path sep. */

  if (*p1 != NULL)
  {
    if ((p1 = malloc(strlen(p)+2)) == NULL)
      dispmsg_terminate(MSG_NOMEM, (char *)0);
    else
    {
      strcpy(p1, p);
      strcat(p1, "\\");
      p = p1;
    }
  }
  return(p);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   fDelete                                            */
/*                                                                        */
/**************************************************************************/

char fDelete(v)
char *v;
{
  struct CDSType CDS;
  int drive;

  /* Only 2 characters in the drive specifier */
  /* (and move before the call to BackFix)    */

  if (strlen(v) != 2 || v[1] != ':')
    return(FALSE);

  v = BackFix(v);
  drive = *v - 'A';

  /* If CDS doesn't exist OR was not substed OR is the current drive */
  if (!fGetCDS(drive, &CDS) || !TESTFLAG(CDS.flags,CDSLOCAL) ||
      drive == getdrv())
    dispmsg_terminate(MSG_BADPARM,cmdln_drive);

  strcpy(CDS.text, "A:\\");                 /* Set-up text of curr directory */
  CDS.text[0] += (char)drive;
  CDS.cbEnd = 2;                            /* Set backup limit */

  /* If physical, then mark as inuse and set-up DPB pointer */
  CDS.flags = drive >= SysVars.cDrv ? FALSE : CDSINUSE;
  CDS.pDPB = drive >= SysVars.cDrv ? 0L : GetDPB(drive);

  fPutCDS(drive, &CDS);
  return(TRUE);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   Insert                                             */
/*                                                                        */
/**************************************************************************/

void Insert(s, d)
char *s, *d;
{
  struct CDSType CDS;
  int drives, drived;
  char buf[MAXPATHLEN];


  rootpath(d, buf);

  /* Ensure destination not just a drive */
  if (strlen(d) == 2 && d[1] == ':')
    dispmsg_terminate(MSG_BADPARM,cmdln_flspec);

  /* Destination must exist, try root first */
  if (strlen(buf) == 3)
  {
    if (buf[1] != ':' || (buf[2]) != PathChr)
      dispmsg_terminate(MSG_BADPARM,cmdln_flspec);
  }

  /* path verification was treated as an ELSE condition */
  /* else                          Must be subdir... make sure */

  if (open(buf,O_BINARY) != -1)
    dispmsg_terminate(MSG_BADPARM,cmdln_flspec);
  else
  {
    if (access(buf,NULL) == -1)
      dispmsg_terminate(MSG_BADPATH,cmdln_flspec);
  }

  s = BackFix(s);
  d = BackFix(buf);
  drives = *s - 'A';
  drived = *d - 'A';

  /* Source can't be a net drive, is reuse of CDS */
  if (fNet(drives))
    dispmsg_terminate(MSG_NETERR, (char *)0);

  /* Destination can't be a net drive either */
  if (fNet(drived))
    dispmsg_terminate(MSG_NETERR, (char *)0);

  /* If src or dest invalid; or dest too long; or drives the same; or can't */
  /* get CDS for source; or source is current drive; or drive is net,       */
  /* splices or substed already; or destination is not physical             */

  if (drives < 0 || drives >= SysVars.cCDS || drives == drived ||
      !fGetCDS(drives, &CDS) || drives == getdrv() ||
      TESTFLAG(CDS.flags,CDSNET|CDSSPLICE))
    dispmsg_terminate(MSG_BADPARM,cmdln_drive);

  if (TESTFLAG(CDS.flags,CDSLOCAL))
    /* dispmsg_terminate(MSG_INUSE,cmdln_drive); */
    dispmsg_terminate(MSG_INUSE,(char *)0);

  if (drived < 0 || drived >= SysVars.cCDS || strlen(d) >= DIRSTRLEN ||
      !fPhysical(drived))
    dispmsg_terminate(MSG_BADPARM,cmdln_flspec);

  /* Chop trailing \ if not at root */
  if (strlen(d) != 3)
    d[strlen(d)-1] = 0;

  strcpy(CDS.text, d);
  CDS.cbEnd = strlen(CDS.text);
  if (CDS.cbEnd == 3)
    CDS.cbEnd--;
  CDS.flags = CDSINUSE|CDSLOCAL;
  if ((CDS.pDPB = GetDPB(drived)) == -1L)
    dispmsg_terminate(MSG_BADPARM,cmdln_flspec);

  CDS.ID = -1L;
  fPutCDS(drives, &CDS);
  return;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   Display                                            */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Displays current list of substs.                */
/*                                                                        */
/**************************************************************************/

void Display()
{
  struct CDSType CDS;
  int i;

  for (i=0; fGetCDS(i, &CDS); i++)
  {
    if (TESTFLAG(CDS.flags,CDSLOCAL))
    {
      if (CDS.cbEnd == 2)
        CDS.cbEnd ++;
      CDS.text[CDS.cbEnd] = 0;
      printf("%c: => %s\n", i+'A', CDS.text);
    }
  }
  return;
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
  sysloadmsg(&inregs,&outregs);             /* Load utility messages */
  if (outregs.x.cflag & CARRY)              /* If problem loading msgs */
  {
    sysdispmsg(&outregs,&outregs);          /* then display the err msg */
    exit(ERRORLEVEL1);
  }
  return;
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
    unsigned char reserved;                 /* Required for sysdispmsg */
    unsigned far *value;                    /* Data pointer */
    unsigned char id;                       /* Id of substitution parm (%1) */
    unsigned char flags;                    /* Format of data - (a0sstttt) */
    unsigned char max_width;                /* Maximum field width */
    unsigned char min_width;                /* Minimum field width */
    unsigned char pad_char;                 /* char to pad field */
  } sublist;


  switch (msg_num)                          /* Which msg to display? */
  {
    case MSG_PARMNUM :                      /* Incorrect Number of Parameters */
        function  = NO_INPUT;               /* Y/N response or press key? */
        message   = 2;                      /* Message number to display */
        msg_class = UTILITY_CLASS;          /* Which class of messages? */
        sub_cnt   = SUBCNT1;                /* Number of substitutions? */
        handle    = STDERR;                 /* Display where? */
        break;
    case MSG_BADPATH :                      /* Invalid Path */
        function  = NO_INPUT;               /* Y/N response or press key? */
        message   = 3;                      /* Message number to display */
        msg_class = EXT_ERR_CLASS;          /* Which class of messages? */
        sub_cnt   = SUBCNT1;                /* Number of substitutions? */
        handle    = STDERR;                 /* Display where? */
        break;
    case MSG_INUSE :                        /* In Use */
        function  = NO_INPUT;
        message   = 5;
        msg_class = UTILITY_CLASS;
        sub_cnt   = SUBCNT0;
        handle    = STDERR;
	break;
    case MSG_NOMEM :                        /* Insufficient Memory */
        function  = NO_INPUT;               /* Y/N response or press key? */
        message   = 8;                      /* Message number to display */
        msg_class = EXT_ERR_CLASS;          /* Which class of messages? */
        sub_cnt   = SUBCNT0;                /* Number of substitutions? */
        handle    = STDERR;                 /* Display where? */
        break;
    case MSG_BADPARM :                      /* Invalid Parameter */
        function  = NO_INPUT;               /* Y/N response or press key? */
        message   = 10;                     /* Message number to display */
        msg_class = PARSE_ERR_CLASS;        /* Which class of messages? */
        sub_cnt   = SUBCNT1;                /* Number of substitutions? */
        handle    = STDERR;                 /* Display where? */
        break;
    case MSG_NETERR :                       /* Cannot SUBST network drive */
        function  = NO_INPUT;               /* Y/N response or press key? */
        message   = 12;                     /* Message number to display */
        msg_class = UTILITY_CLASS;          /* Which class of messages? */
        sub_cnt   = SUBCNT1;                /* Number of substitutions? */
        handle    = STDERR;                 /* Display where? */
        break;
    case MSG_INVSWTCH :                     /* Invalid Switch */
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
    case MSG_INUSE :                             /* In use */
    case MSG_NOMEM :                             /* Insufficient memory */
        inregs.x.ax = message;
        inregs.x.bx = handle;                    /* STDERR */
        inregs.x.cx = sub_cnt;                   /* SUBCNT0 */
        inregs.h.dl = function;                  /* NO_INPUT */
        inregs.h.dh = (unsigned char)msg_class;  /* Extended, Parse or Utility */
        sysdispmsg(&inregs,&outregs);            /* Call common msg service */
        break;
    case MSG_INVSWTCH :                          /* Invalid switch */
    case MSG_PARMNUM  :                          /* Incorrect num of parms */
    case MSG_BADPARM  :                          /* Invalid parameter */
    case MSG_BADPATH  :                          /* Path not found */
        sublist.value = (unsigned far *)outline;
        sublist.reserved = RESERVED;
        sublist.id = SUB_ID0;
        sublist.flags = STR_INPUT;
        sublist.max_width = MAXWIDTH;
        sublist.min_width = MINWIDTH;
        sublist.pad_char = (unsigned char)BLNK;
        inregs.x.ax = message;
        inregs.x.bx = handle;                    /* STDERR */
        inregs.x.si = (unsigned int)&sublist;    /* Point to the sub buffer */
        inregs.x.cx = sub_cnt;                   /* SUBCNT1 */
        inregs.h.dl = function;                  /* STR_INPUT */
        inregs.h.dh = (unsigned char)msg_class;  /* Extended, Parse or Utility */
        sysdispmsg(&inregs,&outregs);            /* Call common msg service */
        break;
    case MSG_NETERR :                            /* Cannot SUBST network drive */
        sublist.value = (unsigned far *)replparm_SUBST;
        sublist.reserved = RESERVED;
        sublist.id = SUB_ID1;
        sublist.flags = STR_INPUT;
        sublist.max_width = MAXWIDTH;
        sublist.min_width = MINWIDTH;
        sublist.pad_char = (unsigned char)BLNK;
        inregs.x.ax = message;                   /* Cannot SUBST network drive */
        inregs.x.bx = handle;                    /* STDERR */
        inregs.x.si = (unsigned int)&sublist;    /* Point to the sub buffer */
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

    if (outregs.x.cflag & CARRY)            /* Is the carry flag set? */
    {                                       /* Then setup regs for extd-err */
      inregs.x.bx = STDERR;
      inregs.x.cx = SUBCNT0;
      inregs.h.dl = NO_INPUT;
      inregs.h.dh = EXT_ERR_CLASS;
      sysdispmsg(&inregs,&outregs);         /* Call to display ext_err msg */
      exit(ERRORLEVEL1);
    }
    return;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   DisplayOptions                                     */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Display the options help message to the user.   */
/*                                                                        */
/*  INPUT:             no value passed.                                   */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/*  ERROR EXIT:        none                                               */
/*                                                                        */
/*  EXTERNAL REF:      SYSDISPMSG                                         */
/*                                                                        */
/**************************************************************************/

void DisplayOptions()
{
  int MsgNbr;

  /*
   * Display all the message lines in the
   * options help message.
   */
  for (MsgNbr = MSG_OPTIONS_FIRST; MsgNbr <= MSG_OPTIONS_LAST; MsgNbr++)
    display_msg(MsgNbr, (char *)0);
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
  p_p.p_parmsx_address = &p_px;                  /* Address of extended parm list */
  p_p.p_num_extra = 0;                           /* No extra declarations */

  p_px.p_minp = MINPOSITION;
  p_px.p_maxp = MAXPOSITION;
  p_px.p_control1 = &p_con1;                     /* Point to 1st control blk */
  p_px.p_control2 = &p_con2;                     /* Point to 2nd control blk */
  p_px.p_maxs = 2;                               /* Specify # of switches */
  p_px.p_switch = &p_swi1;                       /* Point to 1st switch blk */
  p_px.p_switch2 = &p_swi2;                      /* Point to 2nd switch blk */
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
  return;
}


