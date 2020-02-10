;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1983 - 1991
; *                      All Rights Reserved.
; */
/******************************************************************************
*
*  Change Log:
*
*    Date    Who   #                      Description
*  --------  ---  ---  ------------------------------------------------------
*  05/23/90  EGH  C18  Added support for /MBR switch to update the master boot
*                      record.
*  01/09/91  EGH  C34  Added support for fixed disks greater than 4G.
*  01/09/91  EGH  C36  Added code to support 8 fixed disks.
*
******************************************************************************/
#include "dos.h"                                                        /* AN000 */
#include "fdisk.h"                                                      /* AN000 */
#include "extern.h"                                                     /* AN000 */
#include "parse.h"                                                      /* AN000 */
#include "string.h"                                                     /* AN000 */
#include "subtype.h"                                                    /* AN000 */
#include "msgret.h"                                                     /* AN000 */

void DisplayOptionsExit(void);
void exit(int);

/*  */
/******************************************************************************/
/*Routine name:  PARSE_COMMAND_LINE                                           */
/******************************************************************************/
/*                                                                            */
/*Description:   Sets up flags, preloads messages, and parses the command     */
/*               line for switchs.                                            */
/*                                                                            */
/*Called Procedures:                                                          */
/*                                                                            */
/*Change History: Created        5/30/87         DRM                          */
/*                                                                            */
/*Input: None                                                                 */
/*                                                                            */
/*Output: None                                                                */
/*                                                                            */
/******************************************************************************/


char parse_command_line(argc,argv)                                      /* AN000 */

char *argv[];                                /* array of pointer arguments AN000 */
int  argc;

BEGIN                                                                   /* AN000 */


        char    cmd_line[128];                                          /* AN000 */
        char    finished;                                               /* AN000 */
        int     i;                                                      /* AN000 */
        char    parse_good;                                             /* AN000 */
        char    far *cmdline;

		  argc;	/* defeat warning messages */
		  argv;

		  parse_init();                                                   /* AN000 */

        /* Initialize parse_flag to true and don't change unless error */
        parse_good = TRUE;                                              /* AN000 */

        regs.h.ah = (unsigned char) 0x62;
        intdosx(&regs, &regs, &segregs);

        FP_OFF(cmdline) = 0x81;
        FP_SEG(cmdline) = regs.x.bx;

        i = 0;
        while ( *cmdline != (char) '\x0d' ) cmd_line[i++] = *cmdline++;
        cmd_line[i++] = (char) '\x0d';
        cmd_line[i++] = (char) '\0';

        regs.x.si = (unsigned)cmd_line;                                 /* AN000 make DS:SI point to source */
        regs.x.cx = u(0);                                               /* AN000 operand ordinal (whatever that means) */
        regs.x.dx = u(0);                                               /* AN000 operand ordinal (whatever that means) */
        regs.x.di = (unsigned)&p_p;                                     /* AN000 address of parm list */
        Parse_Ptr = (unsigned)cmd_line;                                 /* AN010 */
        regs.x.si = (unsigned)cmd_line;                                 /* AN010 */

        finished = FALSE;
        while ( !finished )                                            /* AN000 */
         BEGIN                                                         /* AN000 */

          Parse_Ptr = regs.x.si;                                       /* AN010  point to next parm       */
          parse(&regs,&regs);                                          /* AN000 Call DOS PARSE service routines*/

          if (regs.x.ax == u(NOERROR))                                 /* AN000 If there was an error*/
             BEGIN
              if (regs.x.dx == (unsigned)&p_buff)                      /* AN000 */
                check_disk_validity();                                 /* AN000 It's a drive letter */

              if (regs.x.dx == (unsigned)&sp_buff)                     /* AN000 */
                process_switch();                                      /* AN000 It's a switch*/
             END
          else
             BEGIN
              if (regs.x.ax == u(0xffff))
                 finished = TRUE;                                      /* AN000 Then we are done*/
              else
                BEGIN
                 Parse_msg(regs.x.ax,STDERR,Parse_err_class);          /* AN010  */
                 parse_good = FALSE;
                 finished = TRUE;                                      /* AN000 Then we are done*/
                END
              END
          END /* End WHILE */                                          /* AN000 */

        return(parse_good);                                             /* AN000 Return to caller*/

END     /* end parser */                                                /* AN000 */


/*  */
/******************************************************************************/
/*Routine name:  INIT_PARSE                                                   */
/******************************************************************************/
/*                                                                            */
/*Description:   Sets up ALL VALUES AND STRUCTS FOR PARSER.                   */
/*                                                                            */
/*Called Procedures:                                                          */
/*                                                                            */
/*Change History: Created        6/15/87         DRM                          */
/*                                                                            */
/*Input: None                                                                 */
/*                                                                            */
/*Output: None                                                                */
/*                                                                            */
/******************************************************************************/


void parse_init()                                                       /* AN000 */

BEGIN                                                                   /* AN000 */


  primary_flag            = FALSE;                                      /* AN000 */
  extended_flag           = FALSE;                                      /* AN000 */
  logical_flag            = FALSE;                                      /* AN000 */
  disk_flag               = FALSE;                                      /* AN000 */
  quiet_flag              = FALSE;                                      /* AN000 */

  status_flag             = FALSE;
  mbr_flag                = FALSE;                                      /*C18*/

  p_p.p_parmsx_ptr        = (unsigned)&p_px;                            /* AN000 Address of extended parm list */
  p_p.p_num_extra         = uc(1);                                      /* AN000 */
  p_p.p_len_extra_delim   = uc(1);                                      /* AN000 */
  p_p.p_extra_delim       = c(SEMICOLON);                               /* AN000 */

  p_px.p_minp             = uc(0);                                      /* AN000 1 required positional */
  p_px.p_maxp             = uc(1);                                      /* AN000 1 maximum positionals */
  p_px.p_con1_ptr         = (unsigned)&p_con;                           /* AN000 pointer to next control blk */
  p_px.p_maxs             = uc(5);  /* 4 to 5; 3 to 4; SR: Changed from 2 to 3 */        /* AN000 number of switches */
  p_px.p_swi1_ptr         = (unsigned)&p_swi1;                          /* AN000 pointer to next control blk */
  p_px.p_swi2_ptr         = (unsigned)&p_swi2;                          /* AN000 pointer to next control blk */

  p_px.p_swi3_ptr         = (unsigned)&p_swi3;
  p_px.p_swi4_ptr         = (unsigned)&p_swi4;
  p_px.p_swi5_ptr         = (unsigned)&p_swi5;                          /*C18*/

  p_px.p_maxk             = uc(NOVAL);                                  /* AN000 no keywords */

  p_con.p_match_flag     = u(0x8001);                                   /* AN000 DRIVE NUMBER 1 OR 2 optional */
  p_con.p_function_flag  = u(0x0000);                                   /* AN000 DO NOTHING FOR FUNCTION FLAG */
  p_con.p_buff1_ptr      = (unsigned)&p_buff;                           /* AN000 */
  p_con.p_val1_ptr       = (unsigned)&p_val;                            /* AN000 */
  p_con.p_nid            = uc(0);                                       /* AN000 */

  p_swi1.sp_match_flag    = u(0x8000);                                   /* AN000 Optional (switch) */
  p_swi1.sp_function_flag = u(0x0000);                                   /* AN000 DO NOTHING FOR FUNCTION FLAG */
  p_swi1.sp_buff1_ptr     = (unsigned)&sp_buff;                          /* AN000 */
  p_swi1.sp_val1_ptr      = (unsigned)&sp_val;                           /* AN000 */
  p_swi1.sp_nid           = uc(3);                                       /* AN000 3 switches allowed */
  strcpy((char *) p_swi1.sp_switch1,PRI);                                /* AN000 /a switch */
  strcpy((char *) p_swi1.sp_switch2,EXT);                                /* AN000 /a switch */
  strcpy((char *) p_swi1.sp_switch3,LOG);                                /* AN000 /a switch */

  p_swi2.sp_match_flag    = u(0x0001);                                   /* AN000 Optional (switch) */
  p_swi2.sp_function_flag = u(0x0000);                                   /* AN000 DO NOTHING FOR FUNCTION FLAG */
  p_swi2.sp_buff1_ptr     = (unsigned)&sp_buff;                          /* AN000 */
  p_swi2.sp_val1_ptr      = (unsigned)NOVAL;                             /* AN000 */
  p_swi2.sp_nid           = uc(1);                                       /* AN000 3 switches allowed */
  strcpy((char *) p_swi2.sp_switch4,QUIET);                                /* AN000 /a switch */

  p_swi3.sp_match_flag    = u(0x0001);
  p_swi3.sp_function_flag = u(0x0000);
  p_swi3.sp_buff1_ptr     = (unsigned)&sp_buff;
  p_swi3.sp_val1_ptr      = (unsigned)NOVAL;
  p_swi3.sp_nid           = uc(1);
  strcpy((char *) p_swi3.sp_switch5,STATUS);

  p_swi4.sp_match_flag    = u(0x0000);
  p_swi4.sp_function_flag = u(0x0000);
  p_swi4.sp_buff1_ptr     = (unsigned)&sp_buff;
  p_swi4.sp_val1_ptr      = (unsigned)NOVAL;
  p_swi4.sp_nid           = uc(1);
  strcpy((char *) p_swi4.sp_switch6,OPTIONS);

  p_swi5.sp_match_flag    = u(0x0001);                                  /*C18*/
  p_swi5.sp_function_flag = u(0x0000);                                  /*C18*/
  p_swi5.sp_buff1_ptr     = (unsigned)&sp_buff;                         /*C18*/
  p_swi5.sp_val1_ptr      = (unsigned)NOVAL;                            /*C18*/
  p_swi5.sp_nid           = uc(1);                                      /*C18*/
  strcpy((char *) p_swi5.sp_switch7,MBR);                               /*C18*/

  p_val.p_values         =  uc(1);                                      /* AN000 - Number of values items returned */
  p_val.p_range          =  uc(1);                                      /* AN000 - Number of ranges */
  p_val.p_range_one      =  uc(1);                                      /* AN000 - range number one */
  p_val.p_low_range      =  ul(1);                                      /* AN000 - low value for range */
/*C36  p_val.p_high_range     =  ul(7); *//*SR; Changed from 2 to 7 */         /* AN000 - high value for range */
  p_val.p_high_range     =  ul(MAX_HDISK);                              /*C36*/         /* AN000 - high value for range */

  sp_val.p_values      =  uc(1);                                        /* AN000 - Number of values items returned */
  sp_val.p_range       =  uc(1);                                        /* AN000 - Number of ranges */
  sp_val.p_range_one   =  uc(1);                                        /* AN000 - range number one */
  sp_val.p_low_range   =  ul(1);                                        /* AN000 - low value for range */
/*C34  sp_val.p_high_range  =  ul(4000); */                                    /* AN000 - high value for range */
  sp_val.p_high_range  =  ul(8064); /* max disk size=7.875GB=8064MB */  /*C34*/

  return;                                                               /* AN000 */

END
                                                                        /* AN000 */
/*  */
/******************************************************************************/
/*Routine name:  CHECK_DISK_VALIDITY                                          */
/******************************************************************************/
/*                                                                            */
/*Description:   Checks the return buffer from parse for the positional       */
/*               value to be equal to 0 or 1.                                 */
/*                                                                            */
/*Called Procedures:                                                          */
/*                                                                            */
/*Change History: Created        6/18/87         DRM                          */
/*                                                                            */
/*Input: None                                                                 */
/*                                                                            */
/*Output: None                                                                */
/*                                                                            */
/******************************************************************************/


void check_disk_validity()                                              /* AN000 */

BEGIN                                                                   /* AN000 */

        disk_flag = (FLAG)TRUE;                                         /* AN000 */
        cur_disk_buff = ((char)p_buff.p_value - 1);                     /* AN000 */
        return;                                                         /* AN000 */
END                                                                     /* AN000 */

/*  */
/******************************************************************************/
/*Routine name:  PROCESS_SWITCH                                               */
/******************************************************************************/
/*                                                                            */
/*Description:   This function looks at the return buffer of the parse and    */
/*               determins the switch, places value in buffer, and sets       */
/*               flag for specific switch.                                    */
/*                                                                            */
/*Called Procedures:                                                          */
/*                                                                            */
/*Change History: Created        6/18/87         DRM                          */
/*                                                                            */
/*Input: None                                                                 */
/*                                                                            */
/*Output: None                                                                */
/*                                                                            */
/******************************************************************************/


void process_switch()                                                   /* AN000 */

BEGIN                                                                   /* AN000 */


       BEGIN                                                            /* AN000 */
           if (sp_buff.p_synonym == (unsigned)p_swi1.sp_switch1)        /* AN000 */
            BEGIN                                                       /* AN000 */
             primary_flag = (FLAG)TRUE;                                 /* AN000 */
             primary_buff = (unsigned)sp_buff.p_value;                  /* AN000 */
            END                                                         /* AN000 */

           if (sp_buff.p_synonym == (unsigned)p_swi1.sp_switch2)        /* AN000 */
            BEGIN                                                       /* AN000 */
             extended_flag = (FLAG)TRUE;                                /* AN000 */
             extended_buff = (unsigned)sp_buff.p_value;                 /* AN000 */
            END                                                         /* AN000 */

           if (sp_buff.p_synonym == (unsigned)p_swi1.sp_switch3)        /* AN000 */
            BEGIN                                                       /* AN000 */
             logical_flag = (FLAG)TRUE;                                 /* AN000 */
             logical_buff = (unsigned)sp_buff.p_value;                  /* AN000 */
            END                                                         /* AN000 */

           if (sp_buff.p_synonym == (unsigned)p_swi2.sp_switch4)        /* AN000 */
            BEGIN                                                       /* AN000 */
             quiet_flag = (FLAG)TRUE;                                   /* AN000 */
            END                                                         /* AN000 */

           if (sp_buff.p_synonym == (unsigned)p_swi3.sp_switch5)
            BEGIN
             status_flag = (FLAG)TRUE;
            END

           if (sp_buff.p_synonym == (unsigned)p_swi4.sp_switch6)
            BEGIN
             DisplayOptionsExit();
            END

           if (sp_buff.p_synonym == (unsigned)p_swi5.sp_switch7)        /*C18*/
            BEGIN                                                       /*C18*/
             mbr_flag = (FLAG)TRUE;                                     /*C18*/
            END                                                         /*C18*/

       END                                                              /* AN000 */
        return;                                                         /* AN000 Return to caller*/
END     /* end parser */                                                /* AN000 */

/************************************************************************/                                                       /* ;an000; */
/* Parse_Message                - This routine will print only those    */
/*                                messages that require 1 replaceable   */
/*                                parm.                                 */
/*                                                                      */
/*      Inputs  : Msg_Num       - number of applicable message          */
/*                Handle        - display type                          */
/*                Message_Type  - type of message to display            */
/*                Replace_Parm  - pointer to parm to replace            */
/*                                                                      */
/*      Outputs : message                                               */
/*                                                                      */
/*      Date    : 03/28/88                                              */
/*      Version : DOS 4.00                                              */
/************************************************************************/

void Parse_msg(Msg_Num,Handle,Message_Type)                             /* AN010 */
                                                                        /* AN010 */
int             Msg_Num;                                                /* AN010 */
int             Handle;                                                 /* AN010 */
unsigned char   Message_Type;                                           /* AN010 */

BEGIN                                                                   /* AN010 */
char    far *Cmd_Ptr;                                                   /* AN010 */


        BEGIN                                                           /* AN010 */
        segread(&segregs);                                              /* AN010 */
        FP_SEG(Cmd_Ptr) = segregs.ds;                                   /* AN010 */
        FP_OFF(Cmd_Ptr) = regs.x.si;                                    /* AN010 */
        *Cmd_Ptr        = '\0';                                         /* AN010 */

        FP_SEG(sublistp[0].value) = segregs.ds;                         /* AN010 */
        FP_OFF(sublistp[0].value) = Parse_Ptr;                          /* AN010 */
        sublistp[0].size      = Sublist_Length;                         /* AN010 */
        sublistp[0].reserved  = Reserved;                               /* AN010 */
        sublistp[0].id        = 0;                                      /* AN010 */
        sublistp[0].flags     = Char_Field_ASCIIZ+Left_Align;           /* AN010 */
        sublistp[0].max_width = 80;                                     /* AN010 */
        sublistp[0].min_width = 01;                                     /* AN010 */
        sublistp[0].pad_char  = Blank;                                  /* AN010 */

        regs.x.ax = Msg_Num;                                            /* AN010 */
        regs.x.bx = Handle;                                             /* AN010 */
        regs.x.cx = SubCnt1;                                            /* AN010 */
        regs.h.dl = No_Input;                                           /* AN010 */
        regs.h.dh = Message_Type;                                       /* AN010 */
        regs.x.si = (unsigned int)&sublistp[0];                         /* AN010 */
        sysdispmsg(&regs,&regs);                                        /* AN010 */
        END                                                             /* AN010 */
        return;                                                         /* AN010 */
END                                                                     /* AN010 */


/************************************************************************/                                                       /* ;an000; */
/* DisplayOptionsExit           - Displays the options help message     */
/*                                lines onto standard output, and then  */
/*                                exits the program so the user can     */
/*                                retry the command.                    */
/*                                                                      */
/*      Inputs  : MSG_OPTIONS_FIRST - #defined with the number of the   */
/*                                    first message line to output.     */
/*                MSG_OPTIONS_LAST  - #defined with the number of the   */
/*                                    last message line to output.      */
/*                                                                      */
/*      Outputs : Options help lines as defined in FDISK.SKL            */
/*                Exits to DOS                                          */
/*                                                                      */
/*      Date    : 04/23/90  c-PaulB                                     */
/*      Version : DOS 5.00                                              */
/************************************************************************/

void
DisplayOptionsExit()
BEGIN
	int	iMsgNbr;

	/*
	 * Display each of the options message lines.
	 * Parse_message can't be used because it tries to display
	 * the text of an offending command.
	 */

	for (iMsgNbr = MSG_OPTIONS_FIRST;     /* defined in parse.h */
		iMsgNbr <= MSG_OPTIONS_LAST;
		iMsgNbr++)
	BEGIN
		  regs.x.ax = iMsgNbr;             /* message #     */
        regs.x.bx = STDOUT;              /* output handle */
        regs.x.cx = SUBCNT0;             /* # of subs     */
        regs.h.dl = No_Input;            /* input flag    */
        regs.h.dh = UTILITY_MSG_CLASS;   /* message class */
        regs.x.si = 0;                   /* sub list ptr  */
        sysdispmsg(&regs,&regs);
	END

	/*
	 * Exit the program so the user can retry the command.
	 */

	exit(ERR_LEVEL_0);

END

/* end of fdparse.c */


