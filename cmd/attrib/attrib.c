/* 0 */
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*    Utility Name:     ATTRIB.EXE                                           */
/*                                                                           */
/*    Source File Name: ATTRIB.C                                             */
/*                                                                           */
/*    Utility Function:                                                      */
/*                                                                           */
/*       Allows you to set or reset the Archive bit, the Read-Only bit,      */
/*       the System bit, the Hidden bit, and the Extended Attributes.        */
/*       Also allows you to display the current setting of those             */
/*       attributes.                                                         */
/*                                                                           */
/*    Status:           ATTRIB Utility, DOS Version 4.0                      */
/*                                                                           */
/*    Entry Point: inmain(line)                                              */
/*                                                                           */
/*    Input:       line = DOS command line parameters                        */
/*                                                                           */
/*    Exit-normal: attribute set, or attribute printed to output device      */
/*                                                                           */
/*    Exit-error:  error message written to standard error device            */
/*                                                                           */
/*    Internal References:                                                   */
/*                                                                           */
/*      Routines:                                                            */
/*                                                                           */
/*    External References:                                                   */
/*                                                                           */
/*       Routines:                                                           */
/*              parse()          module=_parse.sal                           */
/*              sysloadmsg()     module=_msgret.sal                          */
/*              sysdispmsg()     module=_msgret.sal                          */
/*              getpspbyte()     module=new_c.sal                            */
/*              putpspbyte()     module=new_c.sal                            */
/*              segread()        module=dos.h(C library)                     */
/*              intdosx()        module=dos.h(C library)                     */
/*              intdos()         module=dos.h(C library)                     */
/*                                                                           */
/*    Notes:                                                                 */
/*    Syntax (Command Line)                                                  */
/*                                                                           */
/*  Old version:                                                             */
/*  ATTRIB [+R|-R] [+A|-A] [d:][path]filename[.ext] [[id]|[id=value]] [/S]   */
/*  New version:                                                             */
/*  ATTRIB {+,-}{R,S,A,H} [d:] same as before...                             */
/*                                                                           */
/*            where:                                                         */
/*                                                                           */
/*                 +R = Make file ReadOnly by setting READONLY bit           */
/*                 -R = Reset READONLY bit                                   */
/*                 +A = Set ARCHIVE bit                                      */
/*                 -A = Reset ARCHIVE bit                                    */
/*                 +S = Make file a system file                              */
/*                 -S = Reset SYSTEM bit                                     */
/*                 +H = Make file hidden                                     */
/*                 -H = Reset HIDDEN bit                                     */
/*                                                                           */
/*                 id = Set or display the extended attribute named by id.   */
/*                      Only one id processed per invocation. id can be *.   */
/*                                                                           */
/*                 /S = Process subdirectories also                          */
/*                                                                           */
/*    Copyright 1988 Microsoft Corporation				     */
/*                                                                           */
/*    Revision History:                                                      */
/*                                                                           */
/*               Modified 6/22/87   v. 4.0			             */
/*               Rewritten 9/28/87   v. 4.0 		      - AN000	     */
/*                        - fixed check for "." & ".."        - AN001        */
/*               PTM 3195 - changed Extended attribute MSGs   - AN002        */
/*               PTM 3588 - Do C exit not DOS exit.           - AN003        */
/*               PTM 3783 - Fix for hang problem.             - AN004        */
/*               Lea F. Oct 89 - Allow +-S, +-H options                      */
/*                                                                           */
/*   NOTE:                                                                   */
/*     When extended attributes are added back in, make sure you change the  */
/*     attrib.skl file back to the original DOS 4.0 ext. attr. error msgs.   */
/*                                                                           */
/*     Also, this C program requires a special lib when linking to take care */
/*     of the fact that the c lib saves the DOS environment on the heap and  */
/*     if the environment is > 32k, STACK OVERFLOW will occur.               */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

/****************************************************************************
M001 : Undid the M000 modification. And implemented set attributes for
		 Subdirectory entries only when they are explicitly specified and
		 thru wild card specifications
****************************************************************************/

#include <stdio.h>                                                     /*;AN000;*/
#include <io.h>                                                        /*;AN000;*/
#include <dos.h>                                                       /*;AN000;*/
#include <string.h>                                                    /*;AN000;*/
#include <process.h>
#include "parse.h"                                                     /*;AN000;*/
#include "msgret.h"                                                    /*;AN000;*/
#include "attrib.h"                                                    /*;AN000;*/
#include "proto.h"

/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/* Beginning of code (variables declared in attrib.h)                        */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

/*
 * inmain() - This routine receives control from an assembler routine and from
 *            here, main is called. This routine first parses the command line
 *            and then does the appropriate action.
 */
WORD inmain(line)                                                           /*;AN000;*/
   char *line;                                                         /*;AN000;*/
{                                                                      /*;AN000;*/
   return (main(line));                                                         /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     main()                                           */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Parse the command line, makes a full path-filename, does the       */
/*        appropriate function                                               */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD main(line)                                                        /*;AN000;*/
   char *line;                                                         /*;AN000;*/
{                                                                      /*;AN000;*/
   WORD status;                                                        /*;AN000;*/

   WORD Parse_it();         /* forward declaration */                  /*;AN000;*/
   WORD Make_fspec();       /*   "         "       */                  /*;AN000;*/
   WORD Do_dir();           /*   "         "       */                  /*;AN000;*/
   void Error_exit();       /*   "         "       */                  /*;AN000;*/
   void Parse_err();        /*   "         "       */                  /*;AN000;*/

   /* initialize control variables */
   status = NOERROR;                                                   /*;AN000;*/
   descending = FALSE;                                                 /*;AN000;*/
   set_reg_attr = FALSE;                                               /*;AN000;*/
   pmask = mmask = 0x0;                                                /*;AN000;*/
   file[0] = '\0';                                                     /*;AN000;*/
   error_file_name[0] = '\0';                                          /*;AN000;*/

   /* load messages */
   sysloadmsg(&inregs,&outregs);                                       /*;AN000;*/
   if (outregs.x.cflag & CARRY) {                                      /*;AN000;*/
      sysdispmsg(&outregs,&outregs);                                   /*;AN000;*/
      exit(11);                                                       /*;AN000;*/
      }

   Check_appendx();        /* check APPEND /X status */                /*;AN000;*/
   
   Get_DBCS_vector();      /* get double byte table */                 /*;AN000;*/

   /* parse command line */
   status = Parse_it(line);                                            /*;AN000;*/
   if (status != NOERROR) {                                            /*;AN000;*/
      Parse_err(status);                                               /*;AN000;*/
      }                                                                /*;AN000;*/


   /* Initialize any variables need for next phase of program */
   segread(&segregs);     /* init segment registers for DOS calls */   /*;AN000;*/

   /* make full filespec (drive + full path + filename) */
   strcpy(error_file_name,fspec);                                      /*;AN000;*/
   status = Make_fspec(fspec);                                         /*;AN000;*/
   if (status == NOERROR) {                                            /*;AN000;*/

      /* now do the work! */
      did_attrib_ok = FALSE;  /* needed if file not found and no */    /*;AN000;*/
                              /* error detected in Attrib().     */
		if (descending)
			WildCard = TRUE ;		/* descending is equivalent to Wild Card */

      status = Do_dir(fspec,file);                                     /*;AN000;*/
      if (status == NOERROR && did_attrib_ok == FALSE)                 /*;AN000;*/
         status = FILENOTFOUND;                                        /*;AN000;*/
      }                                                                /*;AN000;*/

   /* determine if there was an error after attempt to do attrib function */
   /* NOTE: for ext. attr. calls, add 200 to the return code to get the   */
   /* ----  error code for this switch.                                   */
   switch(status) { /* Extended error codes */                         /*;AN000;*/
      case 0:                                                          /*;AN000;*/
             break;                                                    /*;AN000;*/
      case 2:       /* File not found */                               /*;AN000;*/
             Error_exit(ERR_EXTENDED,2,ONEPARM);                       /*;AN000;*/
             break;                                                    /*;AN000;*/
      case 3:       /* Path not found */                               /*;AN000;*/
             Error_exit(ERR_EXTENDED,3,ONEPARM);                       /*;AN000;*/
             break;                                                    /*;AN000;*/
      case 5:       /* Access Denied */                                /*;AN000;*/
             Error_exit(ERR_EXTENDED,5,ONEPARM);                       /*;AN000;*/
             break;                                                    /*;AN000;*/
      case 15:      /* Invalid drive specification */                  /*;AN000;*/
             Error_exit(ERR_EXTENDED,15,ONEPARM);                      /*;AN000;*/
             break;                                                    /*;AN000;*/
      default:      /* Access Denied */                                /*;AN000;*/
             Error_exit(ERR_EXTENDED,5,ONEPARM);                       /*;AN000;*/
             break;                                                    /*;AN000;*/
      }                                                                /*;AN000;*/
   Reset_appendx();                                                    /*;AN000;*/

return(status);

}  /* end of inmain */                                                 /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*    Subroutine Name: Display_msg                                           */
/*                                                                           */
/*    Subroutine Function:                                                   */
/*       Display the requested message to a given output device              */
/*                                                                           */
/*    Input:                                                                 */
/*        (1) Number of the message to be displayed (see ATTRIB.SKL)         */
/*        (2) Output device handle                                           */
/*        (3) Number of substitution parameters (%1,%2)                      */
/*        (4) Offset of sublist control block                                */
/*        (5) Message Class, 0=no input, 1=input via INT 21 AH=1             */
/*                                                                           */
/*    Output:                                                                */
/*        The message is written to the given output device.  If input       */
/*        was requested, the character code of the key pressed is returned   */
/*        in outregs.x.ax.                                                   */
/*                                                                           */
/*    Normal exit: Message written to handle                                 */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              Sysdispmsg (module _msgret.sal)                              */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

void Display_msg(msgnum,msghan,msgparms,msgsub,msginput)               /*;AN000;*/
   int   msgnum;                                                       /*;AN000;*/
   int   msghan;                                                       /*;AN000;*/
   int   msgparms;                                                     /*;AN000;*/
   int   *msgsub;                                                      /*;AN000;*/
   char  msginput;                                                     /*;AN000;*/
{
   inregs.x.ax = msgnum;                                               /*;AN000;*/
   inregs.x.bx = msghan;                                               /*;AN000;*/
   inregs.x.cx = msgparms;                                             /*;AN000;*/
   inregs.h.dh = utility_msg_class;                                    /*;AN000;*/
   inregs.h.dl = msginput;                                             /*;AN000;*/
   inregs.x.si = (WORD)msgsub;                                         /*;AN000;*/
   sysdispmsg(&inregs,&outregs);                                       /*;AN000;*/

   /* check for error printing message */
   if (outregs.x.cflag & CARRY) {                                      /*;AN000;*/
      outregs.x.bx = (WORD) STDERR;                                    /*;AN000;*/
      outregs.x.si = NOSUBPTR;                                         /*;AN000;*/
      outregs.x.cx = NOSUBCNT;                                         /*;AN000;*/
      outregs.h.dl = exterr_msg_class;                                 /*;AN000;*/
      sysdispmsg(&outregs,&outregs);                                   /*;AN000;*/
      }                                                                /*;AN000;*/
}


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Get_far_str()                                    */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        copies a filename from source to the target. The source is offset  */
/*        from the code segment instead of the data segment.                 */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit: target = source                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

void Get_far_str(target,source,length)                                 /*;AN000;*/
   char *target;                                                       /*;AN000;*/
   DWORD *source;               /* segment = cs register */            /*;AN000;*/
   WORD  length;                                                       /*;AN000;*/
{                                                                      /*;AN000;*/
   char far *fptr;                                                     /*;AN000;*/
   WORD i;                                                             /*;AN000;*/

   if (length == 0) {                                                  /*;AN000;*/

      /* copy string in data segment */
      for (fptr = (char far *) *((DWORD *)source);(char)*fptr != NUL;) /*;AN000;*/
         *target++ = (char) *fptr++;                                   /*;AN000;*/
      *target = *fptr;  /*EOS character */                             /*;AN000;*/
      }                                                                /*;AN000;*/
   else {

      /* copy string in data segment */
      for (fptr = (char far *) *((DWORD *)source),i=0;i < length;i++)  /*;AN000;*/
         *target++ = (char) *fptr++;                                   /*;AN000;*/
      *target = 0x0;    /*EOS character */                             /*;AN000;*/
      }
   strcpy(fix_es_reg,NUL); /* fix for es reg. after using far ptr */   /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Dexit(s)                                          */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Does a DOS terminate.                                              */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit: target = source                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

void Dexit(s)                                                          /*;AN000;*/
   WORD  s;                                                            /*;AN000;*/
{                                                                      /*;AN000;*/
   Reset_appendx();             /* Reset APPEND /X status */           /*;AN000;*/
   exit(s);
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Dallocate()                                      */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Does a DOS allocate of length (in paragraphs).                     */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit: target = source                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD *Dallocate(s)                                                     /*;AN000;*/
   WORD s;       /* length in bytes */                                 /*;AN000;*/
{                                                                      /*;AN000;*/
   WORD length;  /*length in paragraphs */                             /*;AN000;*/

   length = (s / 16 + 1);                                              /*;AN000;*/
   inregs.x.bx = length;                                               /*;AN000;*/
   inregs.x.ax = 0x4800;                                               /*;AN000;*/
   intdos(&inregs,&outregs);                                           /*;AN000;*/
   if (outregs.x.cflag & CARRY) {                                      /*;AN000;*/
      Error_exit(ERR_EXTENDED,8,NOSUBCNT);                             /*;AN000;*/
      }                                                                /*;AN000;*/
   return((WORD *)outregs.x.ax);                                       /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Dfree()                                          */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Does a DOS de-allocate.                                            */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit: target = source                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

void Dfree(segment)                                                    /*;AN000;*/
   WORD segment;                                                       /*;AN000;*/
{                                                                      /*;AN000;*/
   segregs.es = segment;                                               /*;AN000;*/
   inregs.x.ax = 0x4900;                                               /*;AN000;*/
   intdosx(&inregs,&outregs,&segregs);                                 /*;AN000;*/
   if (outregs.x.cflag & CARRY) {                                      /*;AN000;*/
      Error_exit(ERR_EXTENDED,8,NOSUBCNT);                             /*;AN000;*/
      }                                                                /*;AN000;*/
   strcpy(fix_es_reg,NUL); /* fix for es reg. after using far ptr */   /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Copy_far_ptr()                                   */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Copies a far ptr declared in any form to a real far ptr variable.  */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit: status = NOERROR                                          */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

void Copy_far_ptr(p1_addr, p2_addr)                                    /*;AN000;*/
   DWORD *p1_addr;                                                     /*;AN000;*/
   WORD  *p2_addr;                                                     /*;AN000;*/
{                                                                      /*;AN000;*/
   WORD  *dptr, *tptr;                                                 /*;AN000;*/

   dptr = (WORD *)p2_addr;                                             /*;AN000;*/
   tptr = (WORD *)p1_addr;                                             /*;AN000;*/

   *tptr++ = *dptr++;                                                  /*;AN000;*/
   *tptr = *dptr;                                                      /*;AN000;*/
   strcpy(fix_es_reg,NUL);       /* fix ES register */                 /*;AN000;*/
}                                                                      /*;AN000;*/

/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Parse_it()                                       */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Parses the command line and returns error status.                  */
/*                                                                           */
/*    Input:  line                                                           */
/*                                                                           */
/*    Output: various control variables are set                              */
/*                                                                           */
/*    Normal exit: status = NOERROR                                          */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD Parse_it(line)                                                    /*;AN000;*/
   char *line;                                                         /*;AN000;*/
{                                                                      /*;AN000;*/
   WORD  i;                                                            /*;AN000;*/
   WORD  status;                                                       /*;AN000;*/
   WORD  no_value;                                                     /*;AN000;*/
   WORD  got_fn;         /* got filename - required parameter */       /*;AN000;*/
   WORD  pa,                                                           /*;AN000;*/
         ma,                                                           /*;AN000;*/
         pr,                                                           /*;AN000;*/
         mr,                                                           /*;AN000;*/
	 ps,							       /*;AN005;*/
	 ms,							       /*;AN005;*/
	 ph,							       /*;AN005;*/
	 mh;							       /*;AN005;*/
   char *ptr;
   BYTE  p_mask[8],
         m_mask[8];                                                    /*;AN000;*/

   /* do setup for parser */
   for (i=0; i<8; i++) {                                               /*;AN000;*/
      p_mask[i] = m_mask[i] = 0;                                       /*;AN000;*/
      }                                                                /*;AN000;*/
   do_reg_attr = TRUE;                                                 /*;AN000;*/
   set_reg_attr = FALSE;                                               /*;AN000;*/
   no_value = TRUE;            /* no value found for keyword */        /*;AN000;*/
   got_fn = FALSE;             /* no filename yet */                   /*;AN000;*/

   inregs.x.si = (WORD)line;      /* Make DS:SI point to source */     /*;AN000;*/
   inregs.x.cx = 0;               /* Operand ordinal */                /*;AN000;*/
   inregs.x.di = (WORD)&p_p1;     /* Address of parm list */           /*;AN000;*/
   status = p_no_error;           /* Init no error condition */        /*;AN000;*/

   /* loop until error or end_of_line */
   while (status == p_no_error) {                                      /*;AN000;*/
      parse(&inregs,&outregs);                                         /*;AN000;*/
      status = outregs.x.ax;       /* get error status */              /*;AN000;*/

      /* check for errors, continue if none */
      if (status == p_no_error) {                                      /*;AN000;*/

         /* check if first positional */
         if (outregs.x.dx == (WORD)&pos1_buff) {                       /*;AN000;*/
            if (*(char *)pos1_buff.p_result_buff[0] == '+')  {         /*;AN000;*/
               p_mask[0] |= pos1_buff.p_item_tag;                      /*;AN000;*/
               }                                                       /*;AN000;*/
            else {                                                     /*;AN000;*/
               m_mask[0] |= pos1_buff.p_item_tag;                      /*;AN000;*/
               }                                                       /*;AN000;*/
            set_reg_attr = TRUE;                                       /*;AN000;*/
            do_reg_attr = FALSE;                                       /*;AN000;*/
            }                                                          /*;AN000;*/

         /* check if second positional */
         if (outregs.x.dx == (WORD)&pos2_buff) {                       /*;AN000;*/
            if (*(char *)pos2_buff.p_result_buff[0] == '+') {          /*;AN000;*/
               p_mask[1] |= pos2_buff.p_item_tag;                      /*;AN000;*/
               }                                                       /*;AN000;*/
            else {                                                     /*;AN000;*/
               m_mask[1] |= pos2_buff.p_item_tag;                      /*;AN000;*/
               }                                                       /*;AN000;*/
            set_reg_attr = TRUE;                                       /*;AN000;*/
            do_reg_attr = FALSE;                                       /*;AN000;*/
            }                                                          /*;AN000;*/

         /* check if third positional - added Oct 1989, by leaf */
         if (outregs.x.dx == (WORD)&pos3_buff) {
            if (*(char *)pos3_buff.p_result_buff[0] == '+')  {
               p_mask[2] |= pos3_buff.p_item_tag;
               }
            else {
               m_mask[2] |= pos3_buff.p_item_tag;
               }
            set_reg_attr = TRUE;
            do_reg_attr = FALSE;
            }

         /* check if fourth positional - added Oct 1989 */
	 if (outregs.x.dx == (WORD)&pos4_buff) {
            if (*(char *)pos4_buff.p_result_buff[0] == '+') {
               p_mask[3] |= pos4_buff.p_item_tag;
               }
            else {
               m_mask[3] |= pos4_buff.p_item_tag;
               }
            set_reg_attr = TRUE;
            do_reg_attr = FALSE;
            }

         /* check if fifth positional */
         if (outregs.x.dx == (WORD)&pos5_buff) {                       /*;AN000;*/

            /* copy filename from far string to data segment string */
            Get_far_str(fspec,(DWORD *)pos5_buff.p_result_buff,0);     /*;AN000;*/
            got_fn = TRUE;                                             /*;AN000;*/
            }                                                          /*;AN000;*/

         /* check if sixth positional */
         if (outregs.x.dx == (WORD)&pos6_buff) {                       /*;AN000;*/
            if (*(char *)pos6_buff.p_result_buff[0] == '+')  {         /*;AN000;*/
               p_mask[4] |= pos6_buff.p_item_tag;                      /*;AN000;*/
               }                                                       /*;AN000;*/
            else {                                                     /*;AN000;*/
               m_mask[4] |= pos6_buff.p_item_tag;                      /*;AN000;*/
               }                                                       /*;AN000;*/
            set_reg_attr = TRUE;                                       /*;AN000;*/
            do_reg_attr = FALSE;                                       /*;AN000;*/
            }                                                          /*;AN000;*/

         /* check if seventh positional */
         if (outregs.x.dx == (WORD)&pos7_buff) {                       /*;AN000;*/
            if (*(char *)pos7_buff.p_result_buff[0] == '+') {          /*;AN000;*/
               p_mask[5] |= pos7_buff.p_item_tag;                      /*;AN000;*/
               }                                                       /*;AN000;*/
            else {                                                     /*;AN000;*/
               m_mask[5] |= pos7_buff.p_item_tag;                      /*;AN000;*/
               }                                                       /*;AN000;*/
            set_reg_attr = TRUE;                                       /*;AN000;*/
            do_reg_attr = FALSE;                                       /*;AN000;*/
            }                                                          /*;AN000;*/

         /* check if eighth positional - added Oct 1989 */
         if (outregs.x.dx == (WORD)&pos8_buff) {
            if (*(char *)pos8_buff.p_result_buff[0] == '+') {
               p_mask[6] |= pos8_buff.p_item_tag;
               }
            else {
               m_mask[6] |= pos8_buff.p_item_tag;
               }
            set_reg_attr = TRUE;
            do_reg_attr = FALSE;
            }

         /* check if ninth positional - added Oct 1989 */
         if (outregs.x.dx == (WORD)&pos9_buff) {
            if (*(char *)pos9_buff.p_result_buff[0] == '+') {
               p_mask[7] |= pos9_buff.p_item_tag;
               }
            else {
               m_mask[7] |= pos9_buff.p_item_tag;
               }
            set_reg_attr = TRUE;
            do_reg_attr = FALSE;
            }


         /* check for '/S' switch */
         if (outregs.x.dx == (WORD)&sw_buff) {                         /*;AN000;*/

            /* check if duplicate switch */
            if (descending == TRUE) {                                  /*;AN000;*/
                status = p_syntax;                                     /*;AN000;*/
                }                                                      /*;AN000;*/
            descending = TRUE;                                         /*;AN000;*/
            }                                                          /*;AN000;*/

         /* check for '/?' switch */
         if (outregs.x.dx == (WORD)&sw2_buff)
			{
				for (i = MSG_OPTIONS_FIRST; i <= MSG_OPTIONS_LAST; i++)
            	Display_msg(i, STDOUT, NOSUBPTR, NOSUBCNT, NOINPUT);          
				Dexit(0);
         }    


         }     /* if no error */                                       /*;AN000;*/

      /* error, check if this is first positional, if so try again */
      /* using the second positional because they are optional    */
      else if (inregs.x.cx == 0 || inregs.x.cx == 1 ||                 /*;AN000;*/
               inregs.x.cx == 2 || inregs.x.cx == 3 || inregs.x.cx == 4 ||
               inregs.x.cx == 5 || inregs.x.cx == 6 ||
               inregs.x.cx == 7 || inregs.x.cx == 8) {                 /*;AN000;*/
         inregs.x.cx++;    /* try next positional */                   /*;AN000;*/

           /* Check for a filename beginning with '+' because parser will drop */
         /* the plus sign anyways, and we need to flag it as an error        */
         for(ptr=(char *)inregs.x.si; *ptr == ' '; ptr++)              /*;AN000;*/
            /* NULL statement */ ;                                     /*;AN000;*/
         if ((*ptr == '+') || (*ptr == '-'))                           /*;AN000;*/
            status = p_syntax;                                         /*;AN000;*/
         else                                                          /*;AN000;*/
            status = p_no_error;                                       /*;AN000;*/
         strcpy(fix_es_reg,NUL);                                       /*;AN000;*/

         continue;  /* go back up to while loop */                     /*;AN000;*/
         }                                                             /*;AN000;*/

      if (status == p_no_error) {                                      /*;AN000;*/
         inregs.x.cx = outregs.x.cx;       /* update CX for parser */  /*;AN000;*/
         inregs.x.si = outregs.x.si;       /* update SI for parser */  /*;AN000;*/
         }                                                             /*;AN000;*/
      }   /* while loop */                                             /*;AN000;*/

   /* check error status and if at end of line */
   if (status == p_rc_eol) {                                           /*;AN000;*/
      status = p_no_error;                                             /*;AN000;*/
      }                                                                /*;AN000;*/

   /* Check for filename on command line */
   if (!got_fn && status == p_no_error) {                              /*;AN000;*/
       strcpy(fspec, "*.*");
      }                                                                /*;AN000;*/

   /* Check for drive designation */
   if ( (status == p_no_error) && (fspec[1] == ':') && (fspec[2] == '\0') )
       strcat(fspec, "*.*");

   /* Check for keyword and equal sign but no value */
   if (!no_value) {                                                    /*;AN000;*/
      status = p_syntax;                                               /*;AN000;*/
      }                                                                /*;AN000;*/

   /* check for duplicate +R +R (or S,H,A) or -R -R (S,H,A) */	       /*;AN005;*/
     for (pr=0,mr=0,pa=0,ma=0,ps=0,ms=0,ph=0,mh=0,i=0; i<4; i++) {     /*;AN005;*/
      if (p_mask[i] & READONLY)                                        /*;AN000;*/	
	 pr++;							       /*;AN000;*/
      if (m_mask[i] & READONLY) 				       /*;AN005;*/
         mr++;                                                         /*;AN000;*/
      if (p_mask[i] & ARCHIVE)                                         /*;AN000;*/
         pa++;                                                         /*;AN000;*/
      if (m_mask[i] & ARCHIVE)                                         /*;AN000;*/
         ma++;                                                         /*;AN000;*/
      if (p_mask[i] & SYSTEM)					       /*;AN005;*/
	 ps++;							       /*;AN005;*/
      if (m_mask[i] & SYSTEM)					       /*;AN005;*/
	 ms++;							       /*;AN005;*/
      if (p_mask[i] & HIDDEN)					       /*;AN005;*/
	 ph++;							       /*;AN005;*/
      if (m_mask[i] & HIDDEN)					       /*AN005;*/
	 mh++;							       /*AN005;*/
      }                                                                /*;AN000;*/
     if ((pr > 1) || (mr > 1) || (pa > 1) || (ma > 1) || (ps > 1)  
         || (ms > 1) || (ph > 1) || (mh > 1))  {	
      status = p_syntax;                                               /*;AN000;*/
      }                                                                /*;AN000;*/
   else {                                                              /*;AN000;*/
      for (pmask=0,mmask=0,i=0; i<8; i++) {                            /*;AN000;*/
         pmask |= p_mask[i];                  /* combine masks */      /*;AN000;*/
         mmask |= m_mask[i];                                           /*;AN000;*/
         }                                                             /*;AN000;*/
  	}      							   /*;AN000;*/

   /* check for duplicate -R +R or -A +A (S,H) */
   if ((pmask & mmask & READONLY) || (pmask & mmask & SYSTEM)
       || (pmask & mmask & HIDDEN) ||
       (pmask & mmask & ARCHIVE)) {				       /*;AN000;*/
      status = p_syntax;                                               /*;AN000;*/
      }                                                                /*;AN000;*/
   return(status);                                                     /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Make_fspec()                                     */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Makes a full path-filename from the filename & current directory   */
/*        information.                                                       */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD Make_fspec(fspec)                                                 /*;AN000;*/
   char *fspec;                                                        /*;AN000;*/
{                                                                      /*;AN000;*/
   WORD Check_DBCS();       /* forward declaration */                  /*;AN000;*/

   char path[256];                                                     /*;AN000;*/
   WORD status;                                                        /*;AN000;*/
   WORD i,j;                                                           /*;AN000;*/

   status = NOERROR;                                                   /*;AN000;*/

   strcpy(path,fspec);                                                 /*;AN000;*/

   /* Check if user did not enter a drive letter */
   if (fspec[1] != ':') {                                              /*;AN000;*/
      inregs.x.ax = 0x1900;         /* Get current drive */            /*;AN000;*/
      intdos(&inregs,&outregs);                                        /*;AN000;*/
      fspec[0] = (char)('A' + (outregs.x.ax & 0xff));                  /*;AN000;*/
      fspec[1] = ':';                                                  /*;AN000;*/
      fspec[2] = NUL;                                                  /*;AN000;*/
      strcat(fspec,path);                                              /*;AN000;*/
      }                                                                /*;AN000;*/

   /* Check if user didn't enter a path in filename */
   if (!Check_DBCS(fspec,2,'\\')) {                                    /*;AN000;*/
      strcpy(path,&fspec[2]);                                          /*;AN000;*/
      fspec[2] = '\\';                                                 /*;AN000;*/
      inregs.x.ax = 0x4700;            /* Get current directory */     /*;AN000;*/
      inregs.x.si = (WORD)(&fspec[3]);                                 /*;AN000;*/
      inregs.h.dl = fspec[0] - 'A' +1;                                 /*;AN000;*/
      intdos(&inregs,&outregs);                                        /*;AN000;*/
      status = outregs.x.ax;                                           /*;AN000;*/

      if (!(outregs.x.cflag & CARRY)) {                                /*;AN000;*/
         status = NOERROR;                                             /*;AN000;*/
         if (!Check_DBCS(fspec,strlen(fspec)-1,'\\'))                  /*;AN000;*/
            strcat(fspec,"\\");                                        /*;AN000;*/
         strcat(fspec,path);                                           /*;AN000;*/
         }                                                             /*;AN000;*/
      }                                                                /*;AN000;*/

   /* seperate the file specification into path and filename */
   for (i=strlen(fspec);(i>=0) && (!Check_DBCS(fspec,i,'\\')); i--)    /*;AN000;*/
      /* null statement */ ;                                           /*;AN000;*/
   i++;                                                                /*;AN000;*/
   j = 0;                                                              /*;AN000;*/
   while (fspec[i+j] != '\0') {                                        /*;AN000;*/
      file[j] = fspec[i+j];                                            /*;AN000;*/
      fspec[i+j] = '\0';                                               /*;AN000;*/
      j++;                                                             /*;AN000;*/
      }                                                                /*;AN000;*/
   file[j] = '\0';                                                     /*;AN000;*/

   /* Check for filenames of: . (current dir) .. (parent dir) */
   if (strcmp(file,".") == 0)                                          /*;AN001;*/
      strcpy(file,"*.*");                                              /*;AN001;*/
   else if (strcmp(file,"..") == 0) {                                  /*;AN001;*/
      strcat(fspec,"..\\");                                            /*;AN001;*/
      strcpy(file,"*.*");                                              /*;AN001;*/
      }

	for (i=0; file[i]; i++) {
		if ( (file[i]=='?') || (file[i]=='*') ) {
			WildCard = TRUE ;
			break ;
		}
	}

   return(status);                                                     /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Dta_save()                                       */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*                  saves an area in the PSP, but who knows.                 */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

void Dta_save(t,l)
   char   *t;
   unsigned l;
{
   unsigned i;

   for (i = 0; i < l; i++) *(t+i) = getpspbyte(0x80+i)
      /* null statement */  ;
 }


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Dta_restore()                                    */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*              Restores the data that was saved in Dta_save().              */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit: target = source                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

void Dta_restore(t,l)
   char     *t;
   unsigned l;
{
   unsigned i;

   for (i = 0; i < l; i++) putpspbyte(0x80+i,*(t+i))
      /* null statement */  ;
}


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Find_first()                                     */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD Find_first(s,f,a,r)
   char  *s;
   char  *f;
   WORD  *a;
   WORD  *r;
{
   WORD  status;
   WORD  i;

   *f = '\0';

   inregs.x.ax = 0x4e00;             /* DOS find first */
   inregs.x.cx = (*a & 0x00ff );
   inregs.x.dx = (WORD)s;
   intdos(&inregs,&outregs);
   status = outregs.x.ax;

   /* Check for no errors */
   if (!(outregs.x.cflag & CARRY)) {
      for (i = 0; i < 14; i++)
          *f++ = getpspbyte(0x80+30+i);
      *r = getpspbyte(0x80+21);  
      status = NOERROR;
      }
   return(status);
}


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Find_next()                                      */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD Find_next(f,r)
   char  *f;
   WORD  *r;
{
   WORD  status;
   WORD  i;
   char  *t;

   t = f;
   *f = '\0';

   inregs.x.ax = 0x4f00;          /* DOS find next */
   intdos(&inregs,&outregs);
   status = outregs.x.ax;

   if (!(outregs.x.cflag & CARRY)) {
      for (i = 0; i < 14; i++)
          *f++ = getpspbyte(0x80+30+i);
      *r = getpspbyte(0x80+21); 
      status = NOERROR;
      }
   return(status);
}


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Get_reg_attrib()                                 */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*          Does a DOS get attribute byte.                                   */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit: return = 0                                                */
/*                                                                           */
/*    Error exit: return = error code                                        */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD Get_reg_attrib(fspec,attr_byte)                                   /*;AN000;*/
   char *fspec;                                                        /*;AN000;*/
   BYTE *attr_byte;                                                    /*;AN000;*/
{                                                                      /*;AN000;*/
   WORD status;                                                        /*;AN000;*/

   inregs.x.ax = (WORD)0x4300;                                         /*;AN000;*/
   inregs.x.dx = (WORD)fspec;                                          /*;AN000;*/
   intdos(&inregs,&outregs);                                           /*;AN000;*/
   status = outregs.x.ax;                                              /*;AN000;*/

   /* Check for error */                                               /*;AN000;*/
   if (!(outregs.x.cflag & CARRY)) {                                   /*;AN000;*/
      *attr_byte = (BYTE)outregs.h.cl;                                 /*;AN000;*/
      status = NOERROR;                                                /*;AN000;*/
      }                                                                /*;AN000;*/
   return(status);                                                     /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Ext_open()                                       */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*          Does a DOS extended open of a filename and returns a file handle.*/
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit: handle = file handle, return = 0                          */
/*                                                                           */
/*    Error exit: handle = ?; return = error code                            */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD Ext_open(fspec,handle)                                            /*;AN000;*/
   char *fspec;                                                        /*;AN000;*/
   WORD *handle;                                                       /*;AN000;*/
{                                                                      /*;AN000;*/
   WORD status;                                                        /*;AN000;*/

   inregs.x.ax = (WORD)0x6c00;                                         /*;AN000;*/
   inregs.x.bx = (WORD)0x80;                                           /*;AN000;*/
   inregs.x.cx = (WORD)0x0;                                            /*;AN000;*/
   inregs.x.dx = (WORD)0x0101;                                         /*;AN000;*/
   inregs.x.si = (WORD)fspec;                                          /*;AN000;*/
   inregs.x.di = (WORD)&plist;                                         /*;AN000;*//*;AN000;*/
   intdos(&inregs,&outregs);                                           /*;AN000;*/
   status = outregs.x.ax;                                              /*;AN000;*/

   /* Check for error */                                               /*;AN000;*/
   if (!(outregs.x.cflag & CARRY)) {                                   /*;AN000;*/
      *handle = outregs.x.ax;                                          /*;AN000;*/
      status = NOERROR;                                                /*;AN000;*/
      }                                                                /*;AN000;*/
   return(status);                                                     /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Set_reg_attrib()                                 */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*          Sets the attribute byte of a file (not extended attributes).     */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD Set_reg_attrib(fspec,attr_byte)                                   /*;AN000;*/
   char *fspec;                                                        /*;AN000;*/
   BYTE attr_byte;                                                     /*;AN000;*/
{                                                                      /*;AN000;*/
   WORD status;                                                        /*;AN000;*/

   /* set attribute byte (archive & read-only bits) */
   inregs.x.ax = 0x4301;          /* do DOS chmod call  */             /*;AN000;*/
   inregs.x.dx = (WORD)fspec;                                          /*;AN000;*/
   inregs.h.ch = 0x0;                                                  /*;AN000;*/
   inregs.h.cl = (BYTE)attr_byte;                                      /*;AN000;*/
   intdos(&inregs,&outregs);                                           /*;AN000;*/
   status = outregs.x.ax;                                              /*;AN000;*/
                                                                       /*;AN000;*/
   /* Check for error */
   if (!(outregs.x.cflag & CARRY))
      status = NOERROR;                                                /*;AN000;*/
   return(status);                                                     /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     CheckYN()                                        */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*          Check for a valid Yes/No answer.                                 */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD CheckYN(fspec)                                                    /*;AN000;*/
   char *fspec;                                                        /*;AN000;*/
{                                                                      /*;AN000;*/
   WORD answer;                                                        /*;AN000;*/
   WORD key;                                                           /*;AN000;*/

   while (TRUE) {                                                      /*;AN000;*/
      msg_str2.sub_value_seg = segregs.ds;                             /*;AN000;*/
      msg_str2.sub_value = (WORD)fspec;                                /*;AN000;*/
      Display_msg(11,STDOUT,ONEPARM,(int *)&msg_str2,INPUT);           /*;AN000;*/
      key = outregs.x.ax;          /* get key from AX */               /*;AN000;*/
      inregs.x.dx = key;           /* put key in DX */                 /*;AN000;*/
      inregs.x.ax = 0x6523;        /* check Y/N */                     /*;AN000;*/
      intdos(&inregs,&outregs);                                        /*;AN000;*/
      answer = outregs.x.ax;                                           /*;AN000;*/
      Display_msg(14,STDOUT,NOSUBPTR,NOSUBCNT,NOINPUT);                /*;AN000;*/

      if (answer == YES || answer == NO)                               /*;AN000;*/
         break;                                                        /*;AN000;*/
      }                                                                /*;AN000;*/
   return(answer);                                                     /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*    Subroutine Name: Convert_date                                          */
/*                                                                           */
/*    Subroutine Function:                                                   */
/*       Convert date word returned by DOS to the form required by the       */
/*       message retriever.                                                  */
/*                                                                           */
/*       DOS returns:   yyyyyyym mmmddddd                                    */
/*                                                                           */
/*                      y = 0-119 (1980-2099)                                */
/*                      m = 1-12                                             */
/*                      d = 1-31                                             */
/*                                                                           */
/*       Message retriever requires:  yyyyyyyy yyyyyyyy mmmmmmmm dddddddd    */
/*                                                                           */
/*    Input:                                                                 */
/*        (1) Date word in form given by DOS                                 */
/*        (2) Address of first word to place result (yyyyyyyy yyyyyyyy)      */
/*        (3) Address of second word to place result (mmmmmmmm dddddddd)     */
/*                                                                           */
/*    Output:                                                                */
/*        Double word result updated with date in form required by           */
/*        message retriever.                                                 */
/*                                                                           */
/*    Normal exit: Result word updated                                       */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

void Convert_date(dosdate,msgdate1,msgdate2)
   WORD dosdate;
   WORD *msgdate1;
   WORD *msgdate2;
{
   WORD     day,month,year;

   year = dosdate;
   year = ((year >> 1) & 0x7f00) + 80*256;   /* DOS year + 80 */
   year = (year >> 8) & 0x007f;                                        /*;AN000;*/
   day = dosdate;
   day = (day << 8) & 0x1f00;
   month = dosdate;
   month = (month >> 5) & 0x000f;
   *msgdate1 = year;
   *msgdate2 = month | day;
}


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*    Subroutine Name: Convert_time                                          */
/*                                                                           */
/*    Subroutine Function:                                                   */
/*       Convert time word returned by DOS to the form required by the       */
/*       message retriever.                                                  */
/*                                                                           */
/*       DOS returns:   hhhhhmmm mmmsssss                                    */
/*                                                                           */
/*                      h = hours (0-23)                                     */
/*                      m = minutes (0-59)                                   */
/*                      s = seconds/2                                        */
/*                                                                           */
/*       Message retriever requires:  hhhhhhhh mmmmmmmm ssssssss hhhhhhhh    */
/*                                                                           */
/*    Input:                                                                 */
/*        (1) Time word in form given by DOS                                 */
/*        (2) Address of first word to place result (hhhhhhhh hhhhhhhh)      */
/*        (3) Address of second word to place result (ssssssss 00000000)     */
/*                                                                           */
/*    Output:                                                                */
/*        Double word result updated with time in form required by           */
/*        message retriever.                                                 */
/*                                                                           */
/*    Normal exit: Result word updated                                       */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

void Convert_time(dostime,msgtime1,msgtime2)
   WORD dostime;
   WORD *msgtime1;
   WORD *msgtime2;
{
   WORD     hours,minutes,seconds;

   hours = dostime;
   hours = hours >> 11 & 0x001f;
   seconds = dostime;
   seconds = seconds & 0x001f * 2;   /* seconds * 2 */
   minutes = dostime;
   minutes = minutes << 3 & 0x3f00;
   *msgtime1 = hours | minutes;
   *msgtime2 = seconds;
}


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Regular_attrib()                                 */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Handles all function for archive, read-only, system                */
/*        and hidden bit.                                                    */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD Regular_attrib(fspec)                                             /*;AN000;*/
   char *fspec;                                                        /*;AN000;*/
{                                                                      /*;AN000;*/
   WORD status;                                                        /*;AN000;*/
   WORD i;                                                             /*;AN000;*/
   char string[16];                                                    /*;AN000;*/

   /* get attributes */
   if ((status = Get_reg_attrib(fspec,&attr)) != NOERROR) {            /*;AN000;*/
      return(status);                                                  /*;AN000;*/
      }                                                                /*;AN000;*/
    

   /* Check whether to display values or set new ones */
   if (set_reg_attr) {                                                 /*;AN000;*/
   	/* turn off subdirectory bit, else we will be trying to set it */
    	attr = attr & (~SUBDIR) ;
      if( (attr & HIDDEN) && !(pmask & HIDDEN) && !(mmask & HIDDEN) )  {
         msg_str2.sub_value_seg = segregs.ds;                             
         msg_str2.sub_value = (WORD)fspec;                             
         Display_msg(217,STDOUT,ONEPARM,(int *)&msg_str2,NOINPUT);
       } 
      else
        if( (attr & SYSTEM) && !(pmask & SYSTEM) && !(mmask & SYSTEM) )  {
           msg_str2.sub_value_seg = segregs.ds;              
           msg_str2.sub_value = (WORD)fspec;               
           Display_msg(216,STDOUT,ONEPARM,(int *)&msg_str2,NOINPUT);  
         }
        else  {
            attr = (attr & (~mmask)) | pmask;                                /*;AN000;*/
            status = Set_reg_attrib(fspec,attr);                             /*;AN000;*/
           }
      }                                                                /*;AN000;*/
   else {                                                              /*;AN000;*/
      for (i = 0; i < 8; i++)                                          /*;AN000;*/
         if ((attr & bits[i]) != 0 )                                   /*;AN000;*/
	    string[i] = as[i];					       /*;AN000;*/
         else                                                          /*;AN000;*/
            string[i] = ' ';                                           /*;AN000;*/
      for (i=8; i < 16; i++)                                           /*;AN000;*/
         string[i] = ' ';                                              /*;AN000;*/
      string[16] = '\0';                                               /*;AN000;*/

      msg_str.sub_value_seg = segregs.ds;                              /*;AN000;*/
      msg_str.sub_value = (WORD)string;                                /*;AN000;*/
      msg_str1.sub_value_seg = segregs.ds;                             /*;AN000;*/
      msg_str1.sub_value = (WORD)fspec;                                /*;AN000;*/
      Display_msg(9,STDOUT,TWOPARM,(int *)&msg_str,NOINPUT);           /*;AN000;*/
      }                                                                /*;AN000;*/

   did_attrib_ok = TRUE;                                               /*;AN000;*/
   return(status);                                                     /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Special_attrib()                                 */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*          Handles all function for special attributes. For example, "DATE" */
/*          is a special attribute because it is not an extended attribute,  */
/*          but ATTRIB does support its function.                            */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD Special_attrib(handle,fspec,id)                                   /*;AN000;*/
   WORD handle;                                                        /*;AN000;*/
   char *fspec;                                                        /*;AN000;*/
   WORD id;                                                            /*;AN000;*/
{                                                                      /*;AN000;*/
   WORD status;                                                        /*;AN000;*/
   DWORD filesize;                                                     /*;AN000;*/
   long size;                                                          /*;AN000;*/
   long filelength();                                                  /*;AN000;*/

   msg_str1.sub_value_seg = segregs.ds;                                /*;AN000;*/
   msg_str1.sub_value = (WORD)fspec;                                   /*;AN000;*/

   /* determine which info to get by using ID */
   if (id == A_FILESIZE) {       /* get filesize */                    /*;AN000;*/

      /* get file size, if error return error code */
      if ((size = filelength(handle)) == (long)-1) {                   /*;AN000;*/
         return(FILENOTFOUND);                                         /*;AN000;*/
         }                                                             /*;AN000;*/
      filesize = (DWORD)size;                                          /*;AN000;*/
      msg_dword.sub_value = (WORD)&filesize;                           /*;AN000;*/
      msg_dword.sub_value_seg = (WORD)segregs.ds;                      /*;AN000;*/
      Display_msg(9,STDOUT,TWOPARM,(int *)&msg_dword,NOINPUT);         /*;AN000;*/
      }                                                                /*;AN000;*/

   else if (id == A_DATE) {                                            /*;AN000;*/
      inregs.x.ax = 0x5700;      /* get date */                        /*;AN000;*/
      inregs.x.bx = handle;                                            /*;AN000;*/
      intdos(&inregs,&outregs);                                        /*;AN000;*/
      status = outregs.x.ax;                                           /*;AN000;*/
      if (outregs.x.cflag & CARRY)                                     /*;AN000;*/
         return(status);                                               /*;AN000;*/

      Convert_date(outregs.x.dx,&msg_date.sub_value,&msg_date.sub_value_seg); /*;AN000;*/
      Display_msg(9,STDOUT,TWOPARM,(int *)&msg_date,NOINPUT);          /*;AN000;*/
      }                                                                /*;AN000;*/

   else if (id == A_TIME) {                                            /*;AN000;*/
      inregs.x.ax = 0x5700;      /* get time */                        /*;AN000;*/
      inregs.x.bx = handle;                                            /*;AN000;*/
      intdos(&inregs,&outregs);                                        /*;AN000;*/
      status = outregs.x.ax;                                           /*;AN000;*/
      if (outregs.x.cflag & CARRY)                                     /*;AN000;*/
         return(status);                                               /*;AN000;*/

      Convert_time(outregs.x.cx,&msg_time.sub_value,&msg_time.sub_value_seg); /*;AN000;*/
      Display_msg(9,STDOUT,TWOPARM,(int *)&msg_time,NOINPUT);          /*;AN000;*/
      }                                                                /*;AN000;*/

   did_attrib_ok = TRUE;                                               /*;AN000;*/
   return(NOERROR);                                                    /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Attrib()                                         */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*          Determine what functions the user wants and then gets the        */
/*          extended attributes and regular attributes and then checks       */
/*          the attributes against what the user wanted and either do it or  */
/*          return error.                                                    */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD Attrib(path,file)                                                 /*;AN000;*/
   char *path,                                                         /*;AN000;*/
        *file;                                                         /*;AN000;*/
{                                                                      /*;AN000;*/
   char fspec[128];                                                    /*;AN000;*/
   WORD status;                                                        /*;AN000;*/

   strcpy(fspec,path);     /* make full filename */                    /*;AN000;*/
   strcat(fspec,file);                                                 /*;AN000;*/

   /* Check if setting archive bit or readonly bit */
   if (set_reg_attr || do_reg_attr) {                                  /*;AN000;*/
      if ((status = Regular_attrib(fspec)) != NOERROR) {               /*;AN000;*/
         return(status);                                               /*;AN000;*/
         }                                                             /*;AN000;*/
      }                                                               /*;AN000;*/

   return(status);                                                     /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Do_dir()                                         */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Given the full path-filename, determine if descending option is    */
/*        set and then recursively (if option set) find all files in this    */
/*        directory and all files in any subdirectories (if option set).     */
/*        For each directory call Attrib()  which will process a file.       */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

WORD Do_dir(path,file)
   char *path,
        *file;
{
   char     dta_area[128];
   char     subdirectory[256];
   char     next[32];
   WORD     status;
   WORD     search_attrib = ALL3;
   WORD     file_attrib;

   next[0] = '\0';                                                     /*;AN000;*/
   Dta_save(dta_area,128);
   status = NOERROR;                                                   /*;AN000;*/

   /* first, but only if descending, scan for subdirectories */
   if (descending) {
      strcpy(subdirectory,path);
      strcat(subdirectory,"*.*");

      status = Find_first(subdirectory,next,&search_attrib, &file_attrib);

      while (status == NOERROR) {
         if ( (next[0] != '.') && (file_attrib & SUBDIR) ) {
             strcpy(subdirectory,path);
             strcat(subdirectory,next);
             strcat(subdirectory,"\\");
             status = Do_dir(subdirectory,file);
             }

         if (status == NOERROR) {
            strcpy(subdirectory,path);
            strcat(subdirectory,"*.*");

            status = Find_next(next,&file_attrib);
            }
         }     /* while */
      }     /* if descending */

   if (status == NOMOREFILES)
      status = NOERROR;

   /* now, search this directory for files that match */
   if (status == NOERROR) {
      strcpy(subdirectory,path);
      strcat(subdirectory,file);

      status = Find_first(subdirectory,next,&search_attrib, &file_attrib);
      while(status == NOERROR) {

         /* Check that this file is not a '.' or '..' directory file */
			/* If a wild card is specified we don't set attr for SUBDIRs */

         if ( (next[0] != '.') &&
              !(((file_attrib & SUBDIR) && WildCard)) ) {
            status = Attrib(path,next);
         }

         if (status == NOERROR) {
              status = Find_next(next,&file_attrib);
            }
         }      /* while */
       }
   if (status == NOMOREFILES)
      status = NOERROR;

   if (status != NOERROR) {                                           /*;AN000;*/
      }

   Dta_restore(dta_area,128);
   return(status);
}


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Check_appendx()                                  */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Check APPEND /X status.  If it is not active,                      */
/*        do nothing. If it is active, then turn it off                      */
/*        and set flag indicating that fact.                                 */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: append_active_flg                                              */
/*                                                                           */
/*    Normal exit: flag set if /X active                                     */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

void Check_appendx()                                                   /*;AN000;*/
{                                                                      /*;AN000;*/
   void interrupt far ctl_break_handler();                             /*;AN000;*/
   extern crit_err_handler();                                          /*;AN000;*/
   WORD *ptr;                                                          /*;AN000;*/

   inregs.x.ax = 0xb700;         /* Is appendx installed ? */          /*;AN000;*/
   int86(0x2f,&inregs,&outregs);                                       /*;AN000;*/
   if (outregs.h.al) {                                                 /*;AN000;*/
      inregs.x.ax = 0xb702;           /* Get version */                /*;AN000;*/
      int86(0x2f,&inregs,&outregs);                                    /*;AN000;*/
      if (outregs.x.ax == 0xffff) {                                    /*;AN000;*/
         inregs.x.ax = 0xb706;        /* Get /X status */              /*;AN000;*/
         int86(0x2f,&inregs,&outregs);                                 /*;AN000;*/
         append_x_status = outregs.x.bx;  /* save status to restore */ /*;AN000;*/

         /* turn off append /x */
         inregs.x.ax = 0xb707;        /* Set /X status */              /*;AN000;*/
         inregs.x.bx = append_x_status & INACTIVE;                     /*;AN000;*/
         int86(0x2f,&inregs,&outregs);                                 /*;AN000;*/
         }                                                             /*;AN000;*/
      }                                                                /*;AN000;*/

   /* get critical error handler vector for later */
   inregs.x.ax = 0x3524;       /* get critical error vector */         /*;AN000;*/
   intdosx(&inregs,&outregs,&segregs);                                 /*;AN000;*/
   ptr = (WORD *)&old_int24_off;                                       /*;AN000;*/
   *ptr++ = (WORD)outregs.x.bx;                                        /*;AN000;*/
   *ptr = (WORD)segregs.es;                                            /*;AN000;*/

   /* set crtl-c & critical error handler vector */
   segread(&segregs);
   inregs.x.ax = 0x2523;        /* crtl-c - int 23 */                  /*;AN000;*/
   inregs.x.dx = (WORD) ctl_break_handler;                               /*;AN000;*/
   segregs.ds = (WORD) segregs.cs;                                     /*;AN000;*/
   intdosx(&inregs,&outregs,&segregs);                                 /*;AN000;*/

   inregs.x.ax = 0x2524;        /* critical err - int 24 */            /*;AN000;*/
   inregs.x.dx = (WORD) crit_err_handler;                              /*;AN000;*/
   segregs.ds = (WORD) segregs.cs;                                     /*;AN000;*/
   intdosx(&inregs,&outregs,&segregs);                                 /*;AN000;*/
   strcpy(fix_es_reg,NUL);      /* restore ES register */              /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Reset_appendx()                                  */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Reset APPEND /X status.  If it is not active,                      */
/*        do nothing. If it is active, then turn it on                       */
/*        and set flag indicating that fact.                                 */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: append_active_flg                                              */
/*                                                                           */
/*    Normal exit: flag set if /X active                                     */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/

void Reset_appendx()                                                   /*;AN000;*/
{                                                                      /*;AN000;*/
   if (append_x_status != 0)  {                                        /*;AN000;*/
      inregs.x.ax = 0xb707;                                            /*;AN000;*/
      inregs.x.bx = append_x_status;                                   /*;AN000;*/
      int86(0x2f,&inregs,&outregs);                                    /*;AN000;*/
      }                                                                /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Check_DBCS()                                     */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Given an array and a position in the array, check if the character */
/*        is a non-DBCS character.                                           */
/*                                                                           */
/*    Input:  array, character position, character                           */
/*                                                                           */
/*    Output: TRUE - if array[position-1] != DBCS character  AND             */
/*                      array[position] == character.                        */
/*            FALSE - otherwise                                              */
/*    Normal exit: none                                                      */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
WORD Check_DBCS(array,position,character)                              /*;AN000;*/
   char *array;                                                        /*;AN000;*/
   WORD position;                                                      /*;AN000;*/
   char character;                                                     /*;AN000;*/
{                                                                      /*;AN000;*/
   BYTE far *ptr;                                                      /*;AN000;*/
   WORD i;                                                             /*;AN000;*/
   char c;                                                             /*;AN000;*/
   char darray[128];        /* DBCS array, put "D" in every position*/ /*;AN000;*/
                            /* that corresponds to the first byte   */
                            /* of a DBCS character.                 */
   for (i=0;i<128;i++)                                                 /*;AN000;*/
      darray[i] = ' ';                                                 /*;AN000;*/

   /* Check each character, starting with the first in string, for DBCS */
   /* characters and mark each with a "D" in the corresponding darray.  */
   for (i=0;i<position;i++) {                                          /*;AN000;*/
      c = array[i];                                                    /*;AN000;*/

      /* look thru DBCS table to determine if character is first byte */
      /* of a double byte character                                   */
      for (ptr=DBCS_ptr; (WORD)*(WORD far *)ptr != 0; ptr += 2) {      /*;AN000;*/

         /* check if byte is within range values of DOS DBCS table */
         if (c >= *ptr && c <= *(ptr+1)) {                             /*;AN000;*/
            darray[i] = 'D';                                           /*;AN000;*/
            i++;           /* skip over second byte of DBCS */         /*;AN000;*/
            break;
            }
         }                                                             /*;AN000;*/
      }                                                                /*;AN000;*/

   /* if character is not DBCS then check to see if it is == to character */
   if (darray[position-1] != 'D' && character == array[position]) {    /*;AN000;*/
      return (TRUE);                                                   /*;AN000;*/
      }                                                                /*;AN000;*/
   else                                                                /*;AN000;*/
      return (FALSE);                                                  /*;AN000;*/
}                                                                      /*;AN000;*/


/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Get_DBCS_vector()                                */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Gets the double-byte table vector.                                 */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit: none                                                      */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
void Get_DBCS_vector()                                                 /*;AN000;*/
{                                                                      /*;AN000;*/
    WORD *ptr;                                                         /*;AN000;*/
    WORD *buffer;                                                      /*;AN000;*/
    DWORD far *addr_ptr;                                               /*;AN000;*/

    /* allocate a buffer for DBCS table vector */
    buffer = Dallocate(5);      /* at least 5 bytes big */             /*;AN000;*/

    inregs.x.ax = 0x6507;      /* get extended country info */         /*;AN000;*/
    inregs.x.bx = -1;             /* use active code page */           /*;AN000;*/
    inregs.x.cx = 5;              /* 5 bytes of return data */         /*;AN000;*/
    inregs.x.dx = -1;             /* use default country */            /*;AN000;*/
    inregs.x.di = 0;              /* buffer offset */                  /*;AN000;*/
    segregs.es = (WORD)buffer;    /* buffer segment */                 /*;AN000;*/
    intdosx(&inregs,&outregs,&segregs);                                /*;AN000;*/
    strcpy(fix_es_reg,NUL);                                            /*;AN000;*/

    outregs.x.di++;            /* skip over id byte */                 /*;AN000;*/

    /* make a far ptr from ES:[DI] */
    addr_ptr = 0;                                                      /*;AN000;*/
    ptr = (WORD *)&addr_ptr;                                           /*;AN000;*/
    *ptr = (WORD)outregs.x.di;   /* get offset */                      /*;AN000;*/
    ptr++;                                                             /*;AN000;*/
    *ptr = (WORD)segregs.es;     /* get segment */                     /*;AN000;*/
    DBCS_ptr = (BYTE far *)*addr_ptr;                                  /*;AN000;*/
    DBCS_ptr += 2;               /* skip over table length */          /*;AN000;*/

    /* DBCS_ptr points to DBCS table */                                /*;AN000;*/
    strcpy(fix_es_reg,NUL);                                            /*;AN000;*/
}                                                                      /*;AN000;*/



/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Error_exit()                                     */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        displays an extended error message with - filename                 */
/*                                                                           */
/*    Input:  error_file_name[] must contain name of file, if needed for     */
/*            message output.                                                */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
void Error_exit(msg_class,ext_err_num,subcnt)                          /*;AN000;*/
                                                                       /*;AN000;*/
   int msg_class;                                                      /*;AN000;*/
   int ext_err_num;                                                    /*;AN000;*/
   int subcnt;                                                         /*;AN000;*/
{                                                                      /*;AN000;*/
   segread(&segregs);                                                  /*;AN000;*/
   msg_error.sub_value_seg = segregs.ds;                               /*;AN000;*/
   msg_error.sub_value = (WORD)error_file_name;                        /*;AN000;*/
   inregs.x.ax = (WORD)ext_err_num;                                    /*;AN000;*/
   inregs.x.bx = STDERR;                                               /*;AN000;*/
   inregs.x.cx = subcnt;                                               /*;AN000;*/
   inregs.h.dh = (BYTE)msg_class;                                      /*;AN000;*/
   inregs.h.dl = NOINPUT;                                              /*;AN000;*/
   inregs.x.si = (WORD)&msg_error;                                     /*;AN000;*/
   sysdispmsg(&inregs,&outregs);                                       /*;AN000;*/

   /* check for error printing message */
   if (outregs.x.cflag & CARRY) {                                      /*;AN000;*/
      outregs.x.bx = (WORD) STDERR;                                    /*;AN000;*/
      outregs.x.si = NOSUBPTR;                                         /*;AN000;*/
      outregs.x.cx = NOSUBCNT;                                         /*;AN000;*/
      outregs.h.dl = exterr_msg_class;                                 /*;AN000;*/
      sysdispmsg(&outregs,&outregs);                                   /*;AN000;*/
      }                                                                /*;AN000;*/

   Dexit(1);
}                                                                      /*;AN000;*/



/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     Parse_err()                                      */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        displays an parser   error message with - filename                 */
/*                                                                           */
/*    Input:  error_file_name[] must contain name of file, if needed for     */
/*            message output.                                                */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
void Parse_err(error_num)                                              /*;AN000;*/

   WORD error_num;                                                     /*;AN000;*/
{                                                                      /*;AN000;*/
   char *cptr;                                                         /*;AN000;*/
   char *sptr;                                                         /*;AN000;*/

   /* take out leading spaces, point to beginning of parameter */
   for (sptr = (char *)inregs.x.si; ( (sptr < (char *)outregs.x.si) && (*sptr == BLANK) ); sptr++)  /*;AN000;*/
      /* null statement */ ;                                           /*;AN000;*/

   /* find end of this parameter in command line and put end-of-string there */
   for (cptr = sptr; ((int)cptr) < outregs.x.si && *cptr != BLANK; cptr++)    /*;AN000;*/
      /* null statement */ ;                                           /*;AN000;*/
   *cptr = NUL;                                                        /*;AN000;*/
   strcpy(error_file_name,sptr);                                       /*;AN000;*/

   /* check for messages with no parameter */
   if (error_num == p_op_missing)                                      /*;AN000;*/
      Error_exit(ERR_PARSE,error_num,NOSUBCNT);                        /*;AN000;*/
   else                                                                /*;AN000;*/
      Error_exit(ERR_PARSE,error_num,ONEPARM);                         /*;AN000;*/
}                                                                      /*;AN000;*/




/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
/*                                                                           */
/*     Subroutine Name:     ctl_break_handler                                  */
/*                                                                           */
/*     Subroutine Function:                                                  */
/*        Crtl-break interrupt handler.                                      */
/*                                                                           */
/*    Input:  none                                                           */
/*                                                                           */
/*    Output: none                                                           */
/*                                                                           */
/*    Normal exit:                                                           */
/*                                                                           */
/*    Error exit: None                                                       */
/*                                                                           */
/*    Internal References:                                                   */
/*              None                                                         */
/*                                                                           */
/*    External References:                                                   */
/*              None                                                         */
/*                                                                           */
/*ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ*/
void interrupt far ctl_break_handler()
{
   Dexit(3);
}
