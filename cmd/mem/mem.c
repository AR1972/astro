;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1988 - 1991
; *                      All Rights Reserved.
; */

/*----------------------------------------------------------------------+
|                                                                       |
|                                                                       |
|       Title:          MEM                                             |
|                                                                       |
|       Syntax:                                                         |
|                                                                       |
|               From the DOS command line:                              |
|                                                                       |
|               MEM                                                     |
|                       - Used to display DOS memory map summary.       |
|                                                                       |
|               MEM /CLASSIFY                                           |
|                       - Lists modules' memory use.                    |
|                                                                       |
|               MEM /DEBUG                                              |
|                       - Used to display a detailed DOS memory map.    |
|                                                                       |
|               MEM /FREE                                               |
|                       - Lists free memory, in various forms.          |
|                                                                       |
|               MEM /MODULE                                             |
|                       - Details a single module's memory use.         |
|                           						|
|=======================================================================|
|   				Revision History                  	|   
|=======================================================================|
|                                                                       |
|       AN001 - PTM P2914 -> This PTM relates to MEM's ability to report|
|                            the accurate total byte count for EM       |
|                            memory.                                    |
|                                                                       |
|       AN002 - PTM P3477 -> MEM was displaying erroneous base memory   |
|                            information for "Total" and "Available"    |
|                            memory.  This was due to incorrect logic   |
|                            for RAM carving.                           |
|                                                                       |
|       AN003 - PTM P3912 -> MEM messages do not conform to spec.       |
|               PTM P3989                                               |
|                                                                       |
|               Date: 1/28/88                                           |
|                                                                       |
|       AN004 - PTM P4510 -> MEM does not give correct DOS size.        |
|                                                                       |
|               Date: 4/27/88                                           |
|                                                                       |
|       AN005 - PTM P4957 -> MEM does not give correct DOS size for     |
|                            programs loaded into high memory.          |
|                                                                       |
|               Date: 6/07/88                                           |
|									|
|   M000   SR   8/27/90   Added new Ctrl-C handler to delink UMBs	|
|									|
|   M003  NSM  12/28/90   Added a New switch /Classify which groups	|
|         			groups programs in conv and UMB and 	|
|				gives sizes in decimal and hex.         |
|                                                                       |
|   T-RICHJ     5/27/92   Rewrote 70% or so; different options, added   |
|                         mprintf(), reworked internal memory           |
|                         structure, replaced old command-line parser   |
|                                                                       |
+----------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/

#include "ctype.h"
#include "stdio.h"
#include "dos.h"
#include "string.h"
#include "stdlib.h"
#include "msgdef.h"
#include "version.h"         /* MSKK02 07/18/89 */
#include "mem.h"

/*---------------------------------------------------------------------------*/

/* All global declarations go here */

   char   *SingleDrive = "%c:";
   char   *MultipleDrives = "%c: - %c:";
   char   *UnOwned = "----------";

#if IBMCOPYRIGHT                                                        /*EGH*/
        char    *Ibmbio = "IBMBIO";                                     /*EGH*/
        char    *Ibmdos = "IBMDOS";                                     /*EGH*/
#else                                                                   /*EGH*/
        char    *Ibmbio = "IO    "; 
        char    *Ibmdos = "MSDOS "; 
#endif                                                                  /*EGH*/

   char  LinkedIn = 0;   /* Flag set when mem links in UMBs :M000 */
   void (interrupt far *OldCtrlc)(); /* Old Ctrlc handler save vector :M000*/

/*---------------------------------------------------------------------------*/

   struct sublistx sublist[10];

   struct SYSIVAR far *SysVarsPtr;

   char ddrivername[MAX_DDRIVER_REP][9];
   int  ddriveridx = 0;

   unsigned far *ArenaHeadPtr;

   char      OwnerName[128];
   char      TypeText[128];
   char      cmd_line[128];

   unsigned  long UMB_Head;
   unsigned  LastPSP=0;

   char      UseArgvZero = TRUE;
   char      EMSInstalledFlag = (char) 2;

   union  REGS  InRegs;
   union  REGS  OutRegs;
   struct SREGS SegRegs;

   int    BlockDeviceNumber;

   int       NoCR = 0;

   int       DataLevel;
   int       PageBreak;
   int       num_lines;        /* number of lines we've printed on the page */

   char      ModName[40];      /* MEM/M option name */

   struct mem_classif mem_table;

/*---------------------------------------------------------------------------*/

char *ArgPos;  /* Used to report which argument caused an error */

void interrupt cdecl far MemCtrlc (unsigned es, unsigned ds,
         unsigned di, unsigned si, unsigned bp, unsigned sp,
         unsigned bx, unsigned dx, unsigned cx, unsigned ax );

void
main (argc, argv)
int   argc;
char      **argv;
{
   unsigned char   UMB_Linkage;
   unsigned int    rc=0;      /* init to NO ERROR */

   if (argc)  (void)*argv;

   sysloadmsg(&InRegs,&OutRegs);
   if ((OutRegs.x.cflag & CarryFlag) == CarryFlag)
      {
      sysdispmsg(&OutRegs,&OutRegs);
      exit(1);
      }

   init_data();  /* Initialize memory totals */
   DataLevel = PageBreak = 0;
   num_lines = 0;

   if ((rc = parse_cmd (argc, argv)) != 0)
      {
      Parse_Message (rc, (char far *)ArgPos);
      exit (1);
      }

   if (PageBreak)
      {                          /* Find the # of lines on the console      */
      PageBreak = get_lines();   /* (error sets PageBreak to 0, which turns */
      }                          /* it off nicely)                          */

   /*
    * Store the current Ctrl-C handler and replace with our
    * Ctrl-C handler :M000
    */

   OldCtrlc = _dos_getvect( 0x23 ); /* M000 */
   _dos_setvect( 0x23, MemCtrlc );   /* M000 */

   /*
    * Check the UMB link state and do that nasty...
    *
    */

   InRegs.x.ax = GET_UMB_LINK_STATE; /* save current linkstate of UMBs */
   intdos(&InRegs, &OutRegs);

   LinkedIn = 0;

   if (! (UMB_Linkage = OutRegs.h.al))
      {                /* UMBs not presently linked, so do it now */
      InRegs.x.ax = SET_UMB_LINK_STATE;
      InRegs.x.bx = LINK_UMBS;
      intdos(&InRegs, &OutRegs);
      LinkedIn++;   /* Indicate that we have linked in UMBs :M000 */
      }

   rc = DisplayBaseDetail();      /* go show the memory state, and    */
				  /* restore original UMB link state: */
   if (!UMB_Linkage)      /* weren't linked originally */
      {
      InRegs.x.ax = SET_UMB_LINK_STATE;
      InRegs.x.bx = UNLINK_UMBS;
      intdos(&InRegs, &OutRegs);  /* take 'em out again */
      LinkedIn--;
      }

   _dos_setvect (0x23, OldCtrlc);   /* M000 */

		/* if no error in DisplayBaseDetail,    */
		/* go display other things as necessary */

   if (!rc)
      {
      GetSummary();  /* Fills in a few other things */

      switch (DataLevel)
         {
	 case  0:  DispMEMSummary();
	          break;
	          break;
         case  1:  DisplayClassification();
         case  2:  DisplaySummary();
	          break;
         }
      }

   /* If user did not issue Ctrl-C till here, we just remove the handler */

   exit (rc);
}

/*---------------------------------------------------------------------------*/

unsigned long AddressOf(Pointer)
char far *Pointer;
{
   unsigned long SegmentAddress,OffsetAddress;

   SegmentAddress = (unsigned long) (FP_SEG(Pointer)) * 16l;
   OffsetAddress  = (unsigned long) (FP_OFF(Pointer));

   return ((SegmentAddress + OffsetAddress)/16L);

}

/******************************************************************************
 *
 * init_data() resets various totals in <mem_table>, to prepare for later
 * counts.  It also zeroes the two arrays of data.
 *
 */

void
init_data ()
{
   int   i;

   mem_table.conv_ttl = mem_table.conv_free  = (unsigned long)0;
   mem_table.umb_ttl  = mem_table.umb_free   = (unsigned long)0;
   mem_table.xms_ttl  = mem_table.xms_free   = (unsigned long)0;
   mem_table.ems_ttl  = mem_table.ems_free   = (unsigned long)0;
   mem_table.int_15h  = mem_table.conv_large = (unsigned long)0;

   mem_table.hma        = (unsigned int)0;
   mem_table.noof_progs = mem_table.noof_umbs = 0;

   mem_table.xmsMvers = mem_table.xmsmvers = 0;
   mem_table.xmsMdrvr = mem_table.xmsmdrvr = 0;
   mem_table.emsMvers = mem_table.emsmvers = 0;

   for (i = 0; i < MAX_DDRIVER_REP; i++)
      {
      ddrivername[i][0] = 0;
      }
   ddriveridx = 0;

   for (i = 0; i < MAX_CLDATA_INDEX; i++)
      {
      mem_table.files[i].psp_addr = -1;  /* A 0 would mean free space */
      mem_table.files[i].conv_ttl = mem_table.files[i].umb_ttl = 0L;

      mem_table.umbs[i].umb_free = mem_table.umbs[i].umb_ttl   = 0L;
      mem_table.umbs[i].umb_addr = mem_table.umbs[i].umb_large = 0L;
      }
}

/******************************************************************************
 *
 * parse_cmd() interepetes the following options from the command-line:
 *
 *      /C        or /CLASSIFY      - DataLevel = 1
 *      /D        or /DEBUG         - DataLevel = 2
 *      /F        or /FREE          - DataLevel = 3
 *      /M module or /MODULE module - DataLevel = 4
 *      none of /C,/D,/F,/M         - DataLevel = 0
 *
 *      /P        or /PAGE          - PageBreak = 1
 *
 *      /H or /?  or /HELP          - Helptext displayed; immediate exit
 *
 * It accepts arguments as "MEM/C/P" or "MEM/P/M:module" or
 * "MEM /Mmodule" or "MEM/M = module /P" or "MEM/Mmodule/P" or
 * "MEM /debug/page" or... well, just about anything.
 *
 * If it returns 0, everything's fine... otherwise, it returns the error
 * message that should be printed before exiting.
 *
 */

#define SWITCHCHAR '/'

int
parse_cmd (agc, agv)
int        agc;
char          **agv;
{
   char  cmdline[256], *ptr;
   int   i;
   char far *fptr;

/*
 * First we reconstruct the command-line... argv[][] is a little too
 * clumsy for what we're gonna do in a sec.
 *
 */

   cmdline[0] = 0;
   for (i = 1; i < agc; i++)
      {
      strcat (cmdline, agv[i]);
      strcat (cmdline, " ");
      }
   strupr (cmdline);               /* Make it case-insensitive */

/*
 * That done, let's go...
 *
 */

   ArgPos = cmdline;
   for ( ; *ArgPos; )
      {
      while (*ArgPos && strchr (" \t", *ArgPos))  /* Skip whitespace */
         ArgPos++;

      if (! *ArgPos)  break;   /* In the event that it ended in whitespace */

      if (*ArgPos != SWITCHCHAR)
         {
	 for (i = ArgPos - cmdline; i >= 0; i--)
	    if (cmdline[i] == SWITCHCHAR)  break;
	 i = max (i, 0);
	 for ( ; *ArgPos; ArgPos++)
	    if (strchr (" /\t", *ArgPos))  break;
	 *ArgPos = 0;          /* Null-terminate after this option...      */
	 ArgPos = &cmdline[i]; /* ...and point to the start of the option. */
	 return p_not_in_key;  /* They've given "MEM garbage" or something */
	 }
      switch (*(ArgPos+1))
         {
	 case 'C':  if (DataLevel != 0)
	               {
		       *(ArgPos+2) = 0;
		       return p_too_many;
	               }
		    DataLevel = 1;
		    ArgPos += (! strncmp (ArgPos+1, "CLASSIFY", 8)) ? 9 : 2;
		   break;
	 case 'D':  if (DataLevel != 0)
	               {
		       *(ArgPos+2) = 0;
		       return p_too_many;
	               }
		    DataLevel = 2;
		    ArgPos += (! strncmp (ArgPos+1, "DEBUG", 5)) ? 6 : 2;
		   break;
	 case 'F':  if (DataLevel != 0)
	               {
		       *(ArgPos+2) = 0;
		       return p_too_many;
	               }
		    DataLevel = 3;
		    ArgPos += (! strncmp (ArgPos+1, "FREE", 4)) ? 5 : 2;
		   break;

	 case 'P':  if (PageBreak != 0)
	               {
		       *(ArgPos+2) = 0;
		       return p_too_many;
	               }
		    PageBreak = 3;
		    ArgPos += (! strncmp (ArgPos+1, "PAGE", 4)) ? 5 : 2;
		   break;

	 case 'M':  if (DataLevel != 0)
	               {
		       *(ArgPos+2) = 0;
		       return p_too_many;
	               }
		    ptr = ArgPos;   /* Remember where we are, in case of err */
		    DataLevel = 4;
		    ArgPos += (! strncmp (ArgPos+1, "MODULE", 6)) ? 7 : 2;

		    while (*ArgPos && strchr (" :\t", *ArgPos))
		       ArgPos++;                       /* Skip delimiters  */

		    i = 0;
		    while (*ArgPos && !strchr (" /\t", *ArgPos))
		       {
		       ModName[i] = *ArgPos;
		       ArgPos++;  i++;
		       }
		    ModName[i] = 0;         /* Terminate the name */
		    if (i == 0)  /* They didn't give a module name */
		       {
		       InRegs.x.ax = ModuleName;
		       InRegs.h.dh = Utility_Msg_Class;
		       sysgetmsg(&InRegs,&SegRegs,&OutRegs);
		       FP_OFF(fptr) = OutRegs.x.si;
		       FP_SEG(fptr) = SegRegs.ds;

		       for (ptr = cmdline; *fptr; ptr++, fptr++)
		          *ptr = *fptr;
		       *ptr = 0;

		       ArgPos = cmdline;    /* So complain, and list the */
		                            /* paramter as missing from  */
		       return p_op_missing; /* the option "/MODULE".     */
		       }
		   break;

	 case 'H':
	 case '?':  if (DataLevel != 0 || PageBreak != 0)
	               {
		       *(ArgPos+2) = 0;
		       return p_too_many;
	               }
		     for (i = MSG_OPTIONS_FIRST; i <= MSG_OPTIONS_LAST; i++)
			mprintf (i, "");
		     exit (0);
		   break;

	 default:   *(ArgPos+2) = 0;
	            return p_not_in_sw;
         }
      }

   return 0;
}

/************************************************************************/
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
/************************************************************************/

void
Parse_Message (Msg_Num, parse_ptr)
int            Msg_Num;  
char               far *parse_ptr;
{                          
   if (! parse_ptr)
      InRegs.x.cx = 0;    
   else
      {
      sublist[1].value     = (unsigned far *)parse_ptr;
      sublist[1].size      = Sublist_Length; 
      sublist[1].reserved  = Reserved;      
      sublist[1].id        = 0;            
      sublist[1].flags     = Char_Field_ASCIIZ+Left_Align;
      sublist[1].max_width = 40;
      sublist[1].min_width = 01;
      sublist[1].pad_char  = Blank; 

      InRegs.x.cx = 1;
      }

   InRegs.x.ax = Msg_Num;      
   InRegs.x.bx = STDERR;      
   InRegs.h.dl = No_Input;  
   InRegs.h.dh = Parse_Err_Class; 
   InRegs.x.si = (unsigned int)&sublist[1]; 
   sysdispmsg(&InRegs,&OutRegs); 
}                                  

/* M003 END */

