/**************************************************************************/
/*                                                                        */
/*  UTILITY NAME:      Backup                                             */
/*                                                                        */
/*  SOURCE FILE NAME:  Backup.C                                           */
/*                                                                        */
/*  STATUS:    BACKUP utility, DOS Version 4.00                           */
/*             Written using the C programming language.                  */
/*                                                                        */
/*  LABEL:     Microsoft Confidential                                     */
/*             Copyright (c) Microsoft Corporation 1991                   */
/*             All Rights Reserved.                                       */
/*                                                                        */
/**************************************************************************/
/*                                                                        */
/*  COMPILER/LINKER INVOCATION:                                           */
/*                                                                        */
/*     cc /AS /Os /Zep /W3 /DLINT_ARGS /UDEBUG backup.c;                  */
/*     link backup,,,mapper+comsubs                                       */
/*                                                                        */
/*     Note: You MUST(!) use the PACKED option (/Zp) to make sure data    */
/*           structures are not aligned !!!                               */
/*                                                                        */
/*  FUNCTION:                                                             */
/*     BACKUP will back up files from one disk(ette) to another. Accepts  */
/*     global characters, other parameters are defined to allow a more    */
/*     restrictive BACKUP procedure.  Compacts data into one large file   */
/*     and a control file containing directory information. Allows        */
/*     FORMATTING target diskette, intelligent error recovery, and proper */
/*     handling of file sharing and sharing errors. Optionally creates a  */
/*     log file for tracking purposes.  Sets errorlevels on termination   */
/*     to indicate result.                                                */
/*                                                                        */
/*  RESTRICTIONS:                                                         */
/*     The BACKUP Utility program will be version checked to run ONLY on  */
/*     DOS version 4.00.  BACKUP performs a file by file backup using the */
/*     DOS file system, ie. it is not an image type backup.               */
/*                                                                        */
/*  SYNTAX:                                                               */
/*     BACKUP [d:][path] filename [.ext]] d: [/S] [/F[:size]]             */
/*     [/L[:fn]] [/M] [/A] [T:hh:mm:ss] [/D:mm-dd-yy]                     */
/*                                                                        */
/*     [/F[:size]] undocumented                                           */
/*                                                                        */
/*  SOURCE HISTORY:                                                       */
/*	New for DOS 3.3 and OS/2                                          */
/*                                                                        */
/**************************************************************************/


#include <process.h>              /* "C" supplied include files */
#include <malloc.h>
#include <direct.h>
#include <string.h>
#include <dos.h>
#include <stdlib.h>
#include <conio.h>
#include <doscalls.h>             /* OS/2 include file */
#include "backup.h"               /* BACKUP structures, defines, ...*/
#include "backpars.h"             /* defines and structs for DOS Parser */
#include <version.h>              /* symbols to determine compatibility */


/*
 *  DATA STRUCTURES
 */
WORD rc;                          /* return code from DOS calls */

unsigned selector;                /* like a segment address */
                                  
struct node *curr_node;           /* pointer to "node" structure for the */
                                  /* directory currently being processed */
                                  
struct node *last_child;          /* pointer to "node" structure for the */
                                  /* last directory discovered in the */
                                  /* directory currently being processed */
                                  
struct subst_list sublist;        /* message substitution list */
                                  
struct p_parms parms;             /* parser data structure */
struct p_parmsx parmsx;           /* parser data structure */
struct p_pos_blk pos1;            /* parser data structure */
struct p_pos_blk pos2;            /* parser data structure */
struct p_sw_blk sw1;              /* parser data structure */
struct p_sw_blk sw2;              /* parser data structure */
struct p_sw_blk sw3;              /* parser data structure */
struct p_sw_blk sw4;              /* parser data structure */
struct p_sw_blk sw5;              /* parser data structure */
struct p_sw_blk sw6;              
struct p_sw7_blk sw7;             /* for /? */

struct p_result_blk pos_buff;     /* parser data structure */
struct switchbuff sw_buff;        /* parser data structure */
struct timebuff	time_buff;        /* parser data structure */
struct datebuff	date_buff;        /* parser data structure */
struct val_list_struct value_list;
struct val_table_struct value_table;
char curr_parm[128];              /* current parameter being parsed */

DWORD noval = 0;                  /* value list for PARSER */

struct FileFindBuf dta;           /* return area for Find First/Next */
struct FileFindBuf *dta_addr;     /* pointer to above */
union REGS inregs, outregs;       /* register set */


/*
 *  DATA AREAS
 */
WORD dirhandle;                   /* dirhandle field, for FindFirst, FindNext */
BYTE dirhandles_open = FALSE;     /* flag indicating at least 1 open dirhandle */

WORD def_drive;                   /* storage for default drive (1=A,2=B,...) */
BYTE src_drive_letter;            /* aSCII drive letter, source drive */
BYTE tgt_drive_letter;            /* aSCII drive letter, target drive */
BYTE src_def_dir[PATHLEN+20];     /* default dir on source, drive letter omitted */

BYTE src_drive_path_fn[PATHLEN+20];  /* d:\path\fn - fully qualified spec */
BYTE src_drive_path[PATHLEN+20];  /* d:\path - fully qualified drive and path */
BYTE src_fn[PATHLEN];             /* file spec - *.* if no filespec entered */
BYTE ext[3];                      /* filename extension */

WORD files_backed_up = 0;         /* ctr - # files backed up on current target */
BYTE diskettes_complete = 0;      /* # diskettes already filled and complete */
DWORD curr_db_begin_offset;       /* offset in control file of current Directory Block */
DWORD curr_fh_begin_offset;       /* offset in control file of current File Header */
WORD handle_source  = 0xffff;     /* handle for source file */
WORD handle_target  = 0xffff;     /* handle for target file */
WORD handle_control = 0xffff;     /* handle for control file */
WORD handle_logfile = 0xffff;     /* handle for log file */
DWORD part_size;                  /* # bytes from file on disk (files that span) */
DWORD cumul_part_size;            /* # bytes from all disks for particular file */

BYTE logfile_path[PATHLEN+20];    /* d:\path\filename - drive, path, and name */
BYTE format_path[PATHLEN+20];     /* full path to FORMAT.COM */
BYTE format_size[128];            /* if user enters "/F:size" this will be "size" */

char user_response[2];            /* user response of yes/no question - M011 */


/*
 *  PROGRAM CONTROL FLAGS
 */
BYTE do_subdirs = FALSE;          /* user parameters, /S */
BYTE do_add = FALSE;              /* user parameters, /A */
BYTE do_modified = FALSE;         /* user parameters, /M */
BYTE do_format_parms = FALSE;     /* user parameters, /F */
BYTE do_logfile = FALSE;          /* user parameters, /L */
BYTE do_time = FALSE;             /* user parameters, /T */
BYTE do_date = FALSE;             /* user parameters, /D */

BYTE buffers_allocated = FALSE;   /* if file buffers were allocated */
BYTE curr_dir_set = FALSE;        /* if current directory on source was changed */
BYTE def_drive_set = FALSE;       /* if default drive was changed */

BYTE control_opened = FALSE;      /* if file opened or not */
BYTE logfile_opened = FALSE;      /* if logfile file is opened */
BYTE source_opened = FALSE;       /* if file opened or not */
BYTE target_opened = FALSE;       /* if file opened or not */

BYTE doing_first_target = TRUE;   /* if first target is being processed */
BYTE backup_started = FALSE;      /* if backup started - useful for cleanups */

BYTE source_removable;            /* if source drive is removable */
BYTE target_removable;            /* if target drive is removable */

BYTE file_spans_target;           /* if first target is being processed */
BYTE disk_full = FALSE;           /* if the disk is full */
BYTE logfile_on_target = FALSE;   /* if user wants logfile on target drive */
BYTE got_path_validity = FALSE;   /* if input path not verified */
BYTE checking_target = FALSE;     /* if checking target disk to see if formatted */
BYTE deleting_files = FALSE;      /* if deleting files from target */ /* M009 */

BYTE new_directory = TRUE;        /* if file to be backed up is in different dir */
BYTE found_a_file = FALSE;        /* if a file was found to be backed up */
BYTE back_it_up = FALSE;          /* if file found and conforms to specified params */
BYTE ext_attrib_buff[3];          /* buffer for extended attributes */
struct parm_list ea_parmlist;     /* parameter list for extended open */


/*
 *  APPEND STUFF
 */
BYTE append_indicator = 0xff;     /* if support for APPEND /X is active */
WORD original_append_func;        /* APPEND functions on program entry, */
                                  /*   restored on program exit */


/*
 *  OTHER STUFF
 */
BYTE span_seq_num;                /* which part of a spanning file is on current target */

WORD data_file_alloc_size = 0xffff;  /* # paragraphs to allocate for data file */

DWORD data_file_tot_len = 0;      /* data file current length */
DWORD ctl_file_tot_len = 0;       /* control file current length on disk */

WORD ctry_date_fmt;
BYTE ctry_time_fmt;
WORD ctry_date_sep;
WORD ctry_time_sep;

WORD user_specified_time = 0;     /* time entered in response to /T */
WORD user_specified_date = 0;     /* date entered in response to /T */

BYTE return_code = RETCODE_NO_ERROR;  /* save area for DOS ErrorLevel */

BYTE need_to_swap_again = FALSE;  /* exchange disks before spawning FORMAT */

char *device_names[] = { "LPT1",
			 "LPT2",
			 "PRN",
			 "CON",
			 "NUL",
			 "AUX",
			 "LPT1:",
			 "LPT2:",
			 "PRN:",
			 "CON:",
			 "NUL:",
			 "AUX:" };
#define NOOFSTD_DEVICES	12             /* look above for standard device names */



/*********************************************************************/
/* Routine:  main                                                    */
/*                                                                   */
/* Function: Backs up files from source to target drive.             */
/*********************************************************************/

int main(int argc, char *argv[])
{
  /* initialize data structures, check DOS version, preload messages */
  init();
  
  /* look for /? - will exit if found */
  check_options_help(argc, argv);

  def_drive = get_current_drive();     /* save default drive number */
  check_drive_validity(argc, argv);    /* validate input drive(s) */
  get_drive_types();                   /* see if source and target removable */
  get_country_info();                  /* get country dependent info */
  parser(argc, argv);                  /* parse input line, init switches & flags */
  check_path_validity(argv);           /* validate source file spec */

  /* save default directories on def drive, source and target */
  save_current_dirs();

  alloc_buffer();                      /* allocate IO buffer */
  check_appendX();                     /* APPEND /X status, turn off if active */
  set_vectors();                       /* set vectors for Int 23h and 24h */

  /* do the BACKUP */
  do_backup();

  return (0);
}


/*********************************************************************/
/* Routine:   init                                                   */
/*                                                                   */
/* Function:  Preloads messages, checks the DOS version, and         */
/*            initializes the data structures.                       */
/*********************************************************************/

void init()
{
/*
 *  Preload messages and check DOS version.
 */
  sysloadmsg(&inregs,&outregs);

  if (outregs.x.cflag & CARRY)         /* if there was an error */
  {
    sysdispmsg(&outregs, &outregs);    /* display error message */
    return_code = RETCODE_ERROR;       /* set the return code */
    terminate();                       /* terminate */
  }

/*
 *  Setup message subst list for message retriever.
 */
  sublist.sl_size1 = SUBLIST_SIZE;
  sublist.sl_size2 = SUBLIST_SIZE;
  sublist.one = 1;		 
  sublist.two = 2;		 
  sublist.zero1 = 0;		 
  sublist.zero2 = 0;		 

  ext_attrib_buff[0] = 0; 	 
  ext_attrib_buff[1] = 0; 	 

  date_buff.month = 0;		 
  date_buff.day	= 0;		 
  date_buff.year = 0;	 

  time_buff.hours = 0;	 
  time_buff.minutes = 0;	 
  time_buff.seconds = 0;	 
  time_buff.hundreds = 0;	 

  dta_addr = (struct FileFindBuf *)&dta;  /* get address of FindFile buffer */
}


/*********************************************************************/
/* Routine:   check_options_help                                     */
/*                                                                   */
/* Function:  Check the command line for /?.  The parser cannot      */
/*            be relied on for this, because main() first calls      */
/*            check_drive_validity, which will error out if two      */
/*            valid drive letters are not found in argv[].           */
/*                                                                   */
/*            If /? is found, call display_options_exit(), which     */
/*            will display the options help message, and then        */
/*            terminate the program.                                 */
/*********************************************************************/

void check_options_help(int argc, char *argv[])
{
  /*
   *  Check every argument (except the first, which
   *  is "backup", looking for "/?".  If found,
   *  call off to display_options_exit().
   */
  for (argc--, argv++; argc; argc--, argv++)
  {
    if (argv[0][0] == '/' && argv[0][1] == '?' && argv[0][2] == '\0')
      display_options_exit();
  }
}


/*********************************************************************/
/* Routine:   parser                                                 */
/*                                                                   */
/* Function:  Parse the command line.                                */
/*********************************************************************/

void parser(int argc, char *argv[])
{
  char cmd_line[128];
  char not_finished = TRUE;
  int x;

  parse_init();                        /* initialize parser data structures */

  /* copy command line parameters to local area */
  cmd_line[0] = NUL;
  for (x = 1; x <= argc; x++)
  {
   strcat(cmd_line, argv[x]);
   if (x != argc)
     strcat(cmd_line, " ");
  }
  strcat(cmd_line, "\r");

  inregs.x.si = (WORD)&cmd_line[0];    /* ds:si points to cmd line */

  while (not_finished)                 /* for all strings in command line */
  {
    inregs.x.dx = 0;                   /* reserved */
    inregs.x.di = (WORD)&parms;        /* es:di -> address of parm list */
    parse(&inregs, &outregs);          /* call DOS SYSPARSE service routines */

    x = 0;                             /* save parsed parameter */
    for (inregs.x.si; inregs.x.si < outregs.x.si; inregs.x.si++)
    {
      curr_parm[x] = *(char *)inregs.x.si;
      x++;
    }
    curr_parm[x] = NUL;

    inregs = outregs;                  /* reset registers */
    if (outregs.x.ax != (WORD)NOERROR)
    {
      if (outregs.x.ax != (WORD)EOL)
        parse_error(outregs.x.ax, outregs.x.dx);
      not_finished = FALSE;
    }

    if (not_finished)                  /* if there was not an error */
    {
      if (outregs.x.dx == (WORD)&sw_buff)
        process_switch();              /* it's a switch */

      if (outregs.x.dx == (WORD)&time_buff)
      {                                /* it's a TIME parameter */
        if (do_time)
          parse_error(outregs.x.ax, outregs.x.dx);

        check_time(time_buff.hours, time_buff.minutes, time_buff.seconds,
                   time_buff.hundreds);

        /* See "NOTE FROM PARSER SUBROUTINE" in backup.h */
        time_buff.seconds = time_buff.seconds / 2;

        /* TIME bit format hhhhhmmmmmmxxxxx */
        user_specified_time = (time_buff.hours * 0x800) +
                              (time_buff.minutes * 32) + time_buff.seconds;
      }

      if (outregs.x.dx == (WORD)&date_buff)
      {
        if (do_date)
          parse_error(outregs.x.ax, outregs.x.dx);

        check_date(date_buff.year, date_buff.month, date_buff.day);

        /* DATE bit format yyyyyyymmmmddddd */
        user_specified_date = ((date_buff.year - 1980) * 512) +
                              (date_buff.month * 32) + (date_buff.day);
      }
    }
  }
  check_for_device_names(argv);
}


/*********************************************************************/
/* Routine:   parse_error                                            */
/*                                                                   */
/* Function:  There was a parse error.  Display appropriate          */
/*            error message and terminate.                           */
/*********************************************************************/

void parse_error(WORD ax, WORD dx)
{
  sublist.value1 = &curr_parm[0];
  sublist.flags1 = LEFT_ALIGN + CHAR_FIELD_ASCIIZ;
  sublist.pad_char1 = ' ';
  sublist.one = 0;
  sublist.max_width1 = (BYTE)strlen(curr_parm);
  sublist.min_width1 = sublist.max_width1;

  if (dx == (WORD)&time_buff)
    display_it(INV_TIME, STDERR, 1, NOWAIT, (BYTE)UTIL_MSG);
  else
  {
    if (dx == (WORD)&date_buff)
      display_it(INV_DATE, STDERR, 1, NOWAIT, (BYTE)UTIL_MSG);
    else
      display_it (ax, STDERR, 1, NOWAIT, (BYTE)PARSEERROR);
  }
  return_code = RETCODE_ERROR;
  clean_up_and_exit();
}


/*********************************************************************/
/* Routine:   check_date                                             */
/*                                                                   */
/* Function:  A date parameter was entered. Validate it.             */
/*********************************************************************/

void check_date(WORD year, BYTE month, BYTE day)	
{
  if (year > 2099 || year < 1980)
    error_exit(INV_DATE);

  if (month > 12 || month < 1)
    error_exit(INV_DATE);

  if (day > 31 || day  < 1)
    error_exit(INV_DATE);

  /* Verify day not greater then 30 if Apr, Jun, Sep, Nov */
  if ((day > 30) && (month == 4 || month == 6 || month == 9 || month == 11))
    error_exit(INV_DATE);

  if (month == 2)                      /* deal with February */
  {
    if (day >  29)                     /* if Feb 30 or above */
      error_exit(INV_DATE);            /*   then Bad Date */

    if ((year % 4) != 0)               /* if not a leap year */
      if (day >	28)                    /*   if Feb 29 or above */
        error_exit(INV_DATE);          /*     then Bad Date */
  }

  do_date = TRUE;
}


/*********************************************************************/
/* Routine:   check_time                                             */
/*                                                                   */
/* Function:  A time parameter was entered. Validate it.             */
/*********************************************************************/

void check_time(BYTE hours, BYTE minutes, BYTE seconds, BYTE hundreds)
{
  if (hours > 23 || hours < 0)
    error_exit(INV_TIME);

  if (minutes >= 60 || minutes < 0)
    error_exit(INV_TIME);

  if (seconds >= 60 || seconds < 0)
    error_exit(INV_TIME);

  if (hundreds > 99 || hundreds < 0)
    error_exit(INV_TIME);

  do_time = TRUE;
}


/*********************************************************************/
/* Routine:   parse_init                                             */
/*                                                                   */
/* Function:  Initialize the parser data structures.                 */
/*********************************************************************/

void parse_init()
{
  /* Initialize PARMS data structure */
  parms.parmsx_ptr = (WORD)&parmsx;
  parms.p_num_extra = 1;
  parms.p_len_extra_delim = 1;
  parms.p_extra_delim[0] = ';';
  parms.p_extra_delim[1] = NUL;

  /* Initialize PARMSX data structure */
  parmsx.p_minpos = 2;
  parmsx.p_maxpos = 2;
  parmsx.pos1_ptr = (WORD)&pos1;
  parmsx.pos2_ptr = (WORD)&pos2;
  parmsx.num_sw  = 7;
  parmsx.sw1_ptr = (WORD)&sw1;
  parmsx.sw2_ptr = (WORD)&sw2;
  parmsx.sw3_ptr = (WORD)&sw3;
  parmsx.sw4_ptr = (WORD)&sw4;
  parmsx.sw5_ptr = (WORD)&sw5;
  parmsx.sw6_ptr = (WORD)&sw6;
  parmsx.sw7_ptr = (WORD)&sw7;
  parmsx.num_keywords = 0;

  /* Initialize POS1 data structure */
  pos1.match_flag = FILESPEC;
  pos1.function_flag = 0;
  pos1.result_buf = (WORD)&pos_buff;
  pos1.value_list = (WORD)&noval;
  pos1.nid = 0;

  /* Initialize POS2 data structure */
  pos2.match_flag = DRIVELETTER;
  pos2.function_flag = 0;
  pos2.result_buf = (WORD)&pos_buff;
  pos2.value_list = (WORD)&noval;
  pos2.nid = 0;

  /* Initialize SW1 data structure */
  sw1.p_match_flag = 0;
  sw1.p_function_flag = 0;
  sw1.p_result_buf = (WORD)&sw_buff;
  sw1.p_value_list = (WORD)&noval;
  sw1.p_nid = 4;
  strcpy(sw1.switch1, "/S");
  strcpy(sw1.switch2, "/M");
  strcpy(sw1.switch3, "/A");

  /* Initialize SW2 data structure */
  sw2.p_match_flag = DATESTRING;
  sw2.p_function_flag = 0;
  sw2.p_result_buf = (WORD)&date_buff;
  sw2.p_value_list = (WORD)&noval;
  sw2.p_nid = 1;
  strcpy(sw2.switch1, "/D");

  /* Initialize SW3 data structure */
  sw3.p_match_flag = TIMESTRING;
  sw3.p_function_flag = 0;
  sw3.p_result_buf = (WORD)&time_buff;
  sw3.p_value_list = (WORD)&noval;
  sw3.p_nid = 1;
  strcpy(sw3.switch1, "/T");

  /* Initialize SW4 data structure */
  sw4.p_match_flag = SSTRING + OPTIONAL;
  sw4.p_function_flag = CAP_FILETABLE;
  sw4.p_result_buf = (WORD)&sw_buff;
  sw4.p_value_list = (WORD)&noval;
  sw4.p_nid = 1;
  strcpy(sw4.switch1, "/L");

  /* Initialize SW5 data structure */
  sw5.p_match_flag = SSTRING + OPTIONAL;
  sw5.p_function_flag = CAP_CHARTABLE;
  sw5.p_result_buf = (WORD)&sw_buff;
  sw5.p_value_list = (WORD)&value_list;
  sw5.p_nid = 1;
  strcpy(sw5.switch1, "/F");

  /* Initialize SW6 data structure */
  sw6.p_match_flag = 0;
  sw6.p_function_flag = 0;
  sw6.p_result_buf = (WORD)&sw_buff;	
  sw6.p_value_list = (WORD)&noval;
  sw6.p_nid = 1;		
  strcpy(sw6.switch1, "/HD");          /* high density floppies */

  /* Initialize SW7 data structure */
  sw7.p_match_flag = 0;
  sw7.p_function_flag = 0;
  sw7.p_result_buf = (WORD)&sw_buff;
  sw7.p_value_list = (WORD)&noval;
  sw7.p_nid = 1;
  strcpy(sw7.switch7, "/?");           /* /? options help */

  /* Initialize value list data structure */
  value_list.nval = 3;
  value_list.num_ranges = 0;
  value_list.num_choices = 0;
  value_list.num_strings = 33;                             /* M007 */
  value_list.val01 = (WORD)&value_table.val01[0];
  value_list.val02 = (WORD)&value_table.val02[0];
  value_list.val03 = (WORD)&value_table.val03[0];
  value_list.val04 = (WORD)&value_table.val04[0];
  value_list.val05 = (WORD)&value_table.val05[0];
  value_list.val06 = (WORD)&value_table.val06[0];
  value_list.val07 = (WORD)&value_table.val07[0];
  value_list.val08 = (WORD)&value_table.val08[0];
  value_list.val09 = (WORD)&value_table.val09[0];
  value_list.val10 = (WORD)&value_table.val10[0];
  value_list.val11 = (WORD)&value_table.val11[0];
  value_list.val12 = (WORD)&value_table.val12[0];
  value_list.val13 = (WORD)&value_table.val13[0];
  value_list.val14 = (WORD)&value_table.val14[0];
  value_list.val15 = (WORD)&value_table.val15[0];
  value_list.val16 = (WORD)&value_table.val16[0];
  value_list.val17 = (WORD)&value_table.val17[0];
  value_list.val18 = (WORD)&value_table.val18[0];
  value_list.val19 = (WORD)&value_table.val19[0];
  value_list.val20 = (WORD)&value_table.val20[0];
  value_list.val21 = (WORD)&value_table.val21[0];
  value_list.val22 = (WORD)&value_table.val22[0];
  value_list.val23 = (WORD)&value_table.val23[0];
  value_list.val24 = (WORD)&value_table.val24[0];
  value_list.val25 = (WORD)&value_table.val25[0];
  value_list.val26 = (WORD)&value_table.val26[0];
  value_list.val27 = (WORD)&value_table.val27[0];
  value_list.val28 = (WORD)&value_table.val28[0];          /* M007 Start */
  value_list.val29 = (WORD)&value_table.val29[0];
  value_list.val30 = (WORD)&value_table.val30[0];
  value_list.val31 = (WORD)&value_table.val31[0];
  value_list.val32 = (WORD)&value_table.val32[0];
  value_list.val33 = (WORD)&value_table.val33[0];          /* M007 End */

  /* Initialize FORMAT value table */
  strcpy(value_table.val01, "160");    
  strcpy(value_table.val02, "160K");   
  strcpy(value_table.val03, "160KB");  
  strcpy(value_table.val04, "180");    
  strcpy(value_table.val05, "180K");   
  strcpy(value_table.val06, "180KB");  
  strcpy(value_table.val07, "320");    
  strcpy(value_table.val08, "320K");   
  strcpy(value_table.val09, "320KB");  
  strcpy(value_table.val10, "360");    
  strcpy(value_table.val11, "360K");   
  strcpy(value_table.val12, "360KB");  
  strcpy(value_table.val13, "720");    
  strcpy(value_table.val14, "720K");   
  strcpy(value_table.val15, "720KB");  
  strcpy(value_table.val16, "1200");   
  strcpy(value_table.val17, "1200K");  
  strcpy(value_table.val18, "1200KB"); 
  strcpy(value_table.val19, "1.2");    
  strcpy(value_table.val20, "1.2M");
  strcpy(value_table.val21, "1.2MB");
  strcpy(value_table.val22, "1440");
  strcpy(value_table.val23, "1440K");
  strcpy(value_table.val24, "1440KB");
  strcpy(value_table.val25, "1.44");
  strcpy(value_table.val26, "1.44M");
  strcpy(value_table.val27, "1.44MB");
  strcpy(value_table.val28, "2880");                       /* M007 Start */
  strcpy(value_table.val29, "2880K");
  strcpy(value_table.val30, "2880KB");
  strcpy(value_table.val31, "2.88");
  strcpy(value_table.val32, "2.88M");
  strcpy(value_table.val33, "2.88MB");                     /* M007 End */
}


/*********************************************************************/
/* Routine:   find_format                                            */
/*                                                                   */
/* Function:  Search for the FORMAT utility.	If found, then       */
/*            build a full path to it from the root. If not          */
/*            found, tell the user and terminate.                    */
/*********************************************************************/

#define MAX_ENV_PATH	130

void find_format(int check_current)
{
  BYTE found_it = FALSE;
  BYTE no_more = FALSE;
  int findex, pindex;
  BYTE done = FALSE;
  char path[MAX_ENV_PATH];
  int path_len;                        /* length returned by get_path */

/*
 *  First try current directory.
 */
  if (check_current)
  {
    format_path[0] = '.';
    format_path[1] = NUL;

    /* Build full path */
    xlat(format_path, format_path);

    /* If at root, remove trailing backslash */
    if (strlen(format_path) == 3 && format_path[1] == ':')
      format_path[2] = NUL;

    strcat(format_path, "\\FORMAT.COM");

    /* Now look for it */
    if ( exist(format_path) )
    {
      found_it = TRUE;
      return;
    }
  }

  if (!((path_len = get_path(path, MAX_ENV_PATH)) == -1 || 
	path_len == 0 || path[0] == NUL))
  {
    pindex = 0;
    while (!found_it && path[pindex] != NUL)
    {
      for (findex = 0; path[pindex] != NUL && path[pindex] != ';'; pindex++)
      {
        format_path[findex] = path[pindex];
        findex++;
      }

      if (path[pindex]==';')
        pindex++;

      format_path[findex] = NUL;
      xlat(format_path, format_path);
      if (strlen(format_path) == 3 && format_path[1] == ':')
        format_path[2] = NUL;
      strcat(format_path, "\\FORMAT.COM");
      if ( exist(format_path) )
        found_it = TRUE;
    }
  }

  if (!found_it)
  {
    /*
     *  If FORMAT is not available in the current dir, in the PATH, then
     *  get it from Drive A . (loop here to get a floppy with FORMAT in it)
     */
    do
    {
      display_msg(CRLF);                         /* M011 */
      if (!do_format_parms)
        display_msg(DISK_UNFORMAT);              /* M011 */
      display_msg(NO_FORMAT_FND);                /* M011 */
      if (GetYesNo() == YES)                     /* M011 */
      {
        display_msg(CRLF);                       /* M011 */
        display_msg(INSERT_FORMAT);              /* M011 */
	need_to_swap_again = TRUE;
	strcpy(format_path, "A:\\FORMAT.COM");
	if (exist(format_path))
          found_it = TRUE;
      }
      else
      {
        display_msg(CRLF);                       /* M011 */
        error_exit(CANT_FIND_FORMAT);
      }
    } while (!found_it);
  }
}


/*********************************************************************/
/* Routine:   get_path                                               */
/*                                                                   */
/* Function:  Finds the environment pointer in the PSP, and          */
/*            searches the envirnment for a PATH statement.          */
/*            If found, copies it to the buffer address passed in.   */
/*                                                                   */
/*    Entry:  pointer to buffer, buffer length                       */
/*    Exit:   length of string, -1 if exceeded buffer length         */
/*********************************************************************/

int get_path(char *p, int length)
{
  char *env_ptr;
  int  path_len;

  env_ptr = getenv("PATH");

  /* check for length error */
  if ((path_len = strlen(env_ptr)) >= length)
    path_len = -1;
  else                            /* copy only if no length error */
    strcpy(p, env_ptr);

  return (path_len);
}


/*********************************************************************/
/* Routine:   xlat                                                   */
/*                                                                   */
/* Function:  Performs name translate function.  Calls DOS function  */
/*            call 60h to build full path from root using the "src"  */
/*            passed in, places resultant path at "tgt".             */
/*********************************************************************/

void xlat(char *tgt, char *src)					   
{
  union	REGS xregs;

  xregs.x.ax = 0x6000;                 /* Name Xlat */
  xregs.x.bx = 0;                      /* Drive */
  xregs.x.si = (WORD)src;              /* Source */
  xregs.x.di = (WORD)tgt;              /* Target */
  intdos(&xregs, &xregs);              /* Blammo! */
}


/*********************************************************************/
/* Routine:   check_drive_validity                                   */
/*                                                                   */
/* Function:  Verify that at least the target drive letter is        */
/*            is entered. Verify that they are valid drives.         */
/*********************************************************************/

void check_drive_validity(int argc, char *argv[])
{
  char *charptr;
  int i, j;
  char *t;
  BYTE specified_drive;

  if (argc < 2)
    error_exit(NO_SOURCE);

  /* First make sure drives (if any) precede options in argv */
  for (i = 1; ( (i < (argc - 1)) && (i <= 2)); i++)
  {
    for (j = i + 1; j < argc; j++)
    {
      if ( (*(argv[i]) == '/') && (*(argv[j]) != '/') )
      {
	charptr = argv[i];
	argv[i] = argv[j];
	argv[j] = charptr;
      }
    }
  }

  /*
   *  Verify the source.
   */
  *argv[1] = (BYTE)com_toupper(*argv[1]);
  t = argv[1];
  t++;
  if (*t == ':')                  /* check specified source drive */
  {
    if (*argv[1] < 'A')
      error_exit(INV_DRIVE);
    if (*argv[1] > 'Z')
      error_exit(INV_DRIVE);
    src_drive_letter = *argv[1];
  }                               /* use default drive for source */
  else
    src_drive_letter = (BYTE)def_drive + 'A' - 1;

  /*
   *  Verify the target.
   */
  if (argc < 3)
    error_exit(NO_TARGET);

  *argv[2] = (BYTE)com_toupper(*argv[2]);

  if (*argv[2] < 'A')
    error_exit(INV_DRIVE);
  if (*argv[2] > 'Z')
    error_exit(INV_DRIVE);

  /* Verify drive letter followed by ":" */
  t = argv[2];
  t++;
  if (*t != ':')
    error_exit(NO_TARGET);

  /* Make sure drive letters are different */
  if (src_drive_letter == *argv[2])
    error_exit(SRC_AND_TGT_SAME);

  /* Is source a valid drive? */
  specified_drive = src_drive_letter - 'A' + 1;
  set_default_drive(specified_drive);
  if (get_current_drive() != specified_drive)
    error_exit(INV_DRIVE);

  /* Is target a valid drive? */
  specified_drive = *argv[2] - 'A' + 1;
  set_default_drive(specified_drive);
  if (get_current_drive() != specified_drive)
    error_exit(INV_DRIVE);

  set_default_drive(def_drive);   /* reset default drive to original one */
  def_drive_set = FALSE;

  tgt_drive_letter = *argv[2];
}


/*********************************************************************/
/* Routine:   check_for_device_names                                 */
/*                                                                   */
/* Function:  Make sure the user is not trying to restore a reserved */
/*            device name.  Reserved device names are stored in an   */
/*            array called "device_names".                           */
/*********************************************************************/

void check_for_device_names(char *argv[])
{
  union REGS qregs;
  char target[128];
  char *t, *temp_ptr;
  int pathlen, i;

#define CAPITALIZE_STRING 0x6521

  /*
   *  Capitalize the given string for comparing against standard set of
   *  reserved device names.
   */
  qregs.x.ax = CAPITALIZE_STRING;
  qregs.x.dx = (WORD)argv[1];
  pathlen = strlen(argv[1]);
  qregs.x.cx = pathlen;
  intdos(&qregs, &qregs);	
  strcpy(target, argv[1]);

  /*
   *  None of the reserved device names are more than 5 characters;
   *  if (length of src file name <= 5 characters ) {
   *    compare the src file name as such against the std set of res.dev.names
   *  else {             // file name is more than 5 characters
   *    Find the last '\\' in the path name;
   *    if (no BACKSLASH) this CANNOT be a device name; (since name is >5 ch)
   *    if (comp after '\\' is more than 5 chars)
   *      then also it cannot be a device name
   *    else            /* upto 5 chars only in the last comp of name
   *      compare the last comp (after BACKSLASH) to std set of dev names
   */
  if (pathlen <= 5)
    t = &target[0];
  else
  {
    temp_ptr = com_strrchr(target, BACKSLASH);   /* DBCS ?? */
    if (temp_ptr == NUL)
      return;
    if ((target + pathlen - temp_ptr) > 6)
      return;
    else
      t = temp_ptr + 1;
  }

  for (i = 0; i < NOOFSTD_DEVICES; i++)
  {
    if (!strcmp(t, device_names[i]))
    {	
      /* supply args for the INVALID PARAM error below */
      sublist.value1 = (char far *)t;
      sublist.flags1 = LEFT_ALIGN + CHAR_FIELD_ASCIIZ;
      sublist.one = 0;
      sublist.max_width1 = (BYTE)strlen(t);
      sublist.min_width1 = sublist.max_width1;

      display_it (INVPARM, STDERR, 1, NOWAIT, (BYTE)PARSEERROR); 
      return_code = RETCODE_ERROR;
      clean_up_and_exit();              /* does not return */
    }
  }
}


/*********************************************************************/
/* Routine:   check_path_validity                                    */
/*                                                                   */
/* Function:  Verify that the path entered by the user exists.       */
/*            Build a full path from the root, place it in           */
/*            src_drive_path.  Extract filespec and place it         */
/*            in user_filespec.                                      */
/*********************************************************************/

void check_path_validity(char *argv[])
{
  WORD dhandle;
  char globals = FALSE;
  int x;
  char *foo, *foo2;
  union	REGS qregs;

  strcpy(src_drive_path_fn, argv[1]);  /* copy argv[1] to string area */

  for (x = 0; x < strlen(src_drive_path_fn); x++)
  {
    if (src_drive_path_fn[x] == BACKSLASH)
      if (src_drive_path_fn[x+1] == BACKSLASH)
#ifdef DBCS
        if (!CheckDBCSTailByte(src_drive_path_fn, &src_drive_path_fn[x]))
#endif
        error_exit(INV_PATH);
  }

  if (strlen(src_drive_path_fn) == 1)
  {
    if (*src_drive_path_fn == '.')            
      strcpy(src_drive_path_fn, "*.*");
  }

  if (strlen(src_drive_path_fn) == 2)  /* if only drive entered, make it d:*.* */
  {
    if (src_drive_path_fn[1] == ':')
      strcat(src_drive_path_fn, "*.*");
  }

  if (src_drive_path_fn[strlen(src_drive_path_fn)-1] == BACKSLASH)
  {
#ifdef DBCS
    if (!CheckDBCSTailByte(src_drive_path_fn,
                           &src_drive_path_fn[strlen(src_drive_path_fn)-1]))
#endif
    strcat(src_drive_path_fn, "*.*");
  }

  xlat(src_drive_path_fn, src_drive_path_fn);

  /* Handle UNC format  ( \\srv\name\path\path\file ) (Remote drive) */
  if (src_drive_path_fn[0] == BACKSLASH)
  {
    if (src_drive_path_fn[1] == BACKSLASH)
    {
      foo = strchr(src_drive_path_fn+3, BACKSLASH);
      if (foo == NUL)
        error_exit(INV_PATH);

      foo2 = foo + 1;
      foo = strchr(foo2, BACKSLASH);
      if (foo == NUL)
        error_exit(INV_PATH);

      sprintf(src_drive_path_fn, "%c:%s", src_drive_letter, foo);
    }
  }

  /* See if there are global characters specified */
  if (com_strchr(src_drive_path_fn, '?') != NUL)
    globals = TRUE;
  else
    if (com_strchr(src_drive_path_fn, '*') != NUL)
      globals = TRUE;

  if (src_drive_path_fn[3] == BACKSLASH)     /*	don't let user enter d:\\ */
  {
    if (src_drive_path_fn[2] == BACKSLASH)
#ifdef DBCS
      if (!CheckDBCSTailByte(src_drive_path_fn, &src_drive_path_fn[2]))
#endif
      error_exit(INV_PATH);
  }

  if (source_removable)                /* if backing up from a diskette */
    display_msg(INSERTSOURCE);         /* ask user for disk, wait for input */

  /*
   *  If single drive system, this eliminates double prompting
   *  for user to "Insert diskette for drive %1".
   */
  qregs.x.ax = SETLOGICALDRIVE;
  qregs.h.bl = src_drive_letter - 'A' + 1;
  intdos(&qregs, &qregs);

  /* Check for Invalid Path and figure out if it is a file or directory */
  find_first(&src_drive_path_fn[0], &dhandle, dta_addr,
             (SUBDIR + SYSTEM + HIDDEN));

  if (rc != NOERROR)                   /* if there was an error */
  {
    if (rc == 3)                       /*   if it was Path Not Found */
      error_exit(INV_PATH);            /*     Terminate */
    /*
     *  M008 -
     *  Do NOT quit out of the backup for any other error code.  Since
     *  this routine does NOT check for the /s option, quitting out
     *  here is premature if the "filename" does not exist in the
     *  current directory.  If the file does not exist in any of the
     *  subdirectories either, this error will be caught later on in 
     *  the do_backup routine (after it calls the routine find_first_file).
     */
  }
  else
    findclose(dhandle);

  /* 
   *  Look for installed device names.  These names are also not allowed
   *  as BACKUP source file names.
   */
  if (dta.attributes & DEVICEATT)      /* is this a device ? */
  {
    /* set the sublist vars for INVPARM message properly */
    sublist.value1 = (char far *)&src_drive_path_fn[0];  
    sublist.flags1 = LEFT_ALIGN + CHAR_FIELD_ASCIIZ;    
    sublist.one = 0;				      
    sublist.max_width1 = (BYTE)strlen(src_drive_path_fn);
    sublist.min_width1 = sublist.max_width1;	     
    error_exit(INVPARM);	/* if device, quit backup */
  }

  if ((dta.attributes & SUBDIR) == SUBDIR)  /* if subdirectory name, */
  {
    if (dta.file_name[0] != '.')            /* add to the end of it "\*.*" */
    {
      if (!globals)                         /* only if no global chars */
        strcat(src_drive_path_fn, "\\*.*");
    }
  }

  /*
   *  Build src_drive_FN.
   */
  strcpy(src_drive_path, src_drive_path_fn);

  /* Remove last BACKSLASH to get the pathname */
  foo = com_strrchr(src_drive_path, BACKSLASH);

  if (foo != NUL)
  {
    if ((foo - src_drive_path) > 2)
      *foo = NUL;
    else
    {                             /* foo must = 2 */
      foo++;
      *foo = NUL;
    }
  }

  /*
   *  Build src_fn.
   */
  foo = com_strrchr(src_drive_path_fn, BACKSLASH);
  if (foo == NUL)
    foo = &src_drive_path_fn[2];
  else
    foo++;                        /* skip over last non-DBCS char */
  strcpy(src_fn, foo);

  got_path_validity = TRUE;
}


/*********************************************************************/
/* Routine:   alloc_buffers                                          */
/*                                                                   */
/* Function:  Attempt to allocate a (64k-1) buffer. If               */
/*            fails, decrement buff size by 512 and keep             */
/*            trying. If can't get at least a 2k buffer,             */
/*            give up.                                               */
/*********************************************************************/

void alloc_buffer()
{
  alloc_seg();

  while ((rc != NOERROR) && (data_file_alloc_size > 2048))
  {
    data_file_alloc_size = data_file_alloc_size - 512;
    alloc_seg();
  }

  if (rc == NOERROR  &&  data_file_alloc_size > 2048)
    buffers_allocated = TRUE;
  else
    error_exit(INSUFF_MEMORY);
}


/*********************************************************************/
/* Routine:   process_switch                                         */
/*                                                                   */
/* Function:  Identify the parameter and set program control         */
/*            flags as appropriate.                                  */
/*********************************************************************/

void process_switch()
{
  char far *y;
  int i = 0;
  char temp_str[PATHLEN+20];

  /*
   *  If there's a /? switch,
   *  display the options help and exit.
   */
  if (sw_buff.sw_synonym_ptr == (WORD)&sw7.switch7[0])
    display_options_exit();

  if (sw_buff.sw_synonym_ptr == (WORD)&sw1.switch1[0])     /* /S */
    do_subdirs = TRUE;

  if (sw_buff.sw_synonym_ptr == (WORD)&sw5.switch1[0])    /* /F */
  {
    if (!target_removable)
      error_exit(CANT_FORMAT_HARDFILE);

    do_format_parms = TRUE;
    format_size[0] = ' ';            /* can't do strcpy during parse */
    format_size[1] = '/';
    format_size[2] = 'F';
    format_size[3] = ':';
    format_size[4] = NUL;

    i = 4;                           /* copy size */
    for (y = (char *)sw_buff.sw_string_ptr; *y != NUL; y++)
    {
      format_size[i] = (BYTE)*y;
      i++;
    }

    /* Handle case where user only enters /F */
    if (format_size[4] == NUL	|| format_size[4] < '0' || format_size[4] > '9')
      format_size[0] = NUL;
  }

  if (sw_buff.sw_synonym_ptr == (WORD)&sw1.switch2[0])   /* /M */
    do_modified = TRUE;

  if (sw_buff.sw_synonym_ptr == (WORD)&sw1.switch3[0])   /* /A */
    do_add = TRUE;

  if (sw_buff.sw_synonym_ptr == (WORD)&sw4.switch1[0])   /* /L */
  {
    do_logfile = TRUE;
    i = 0;                           /* copy filespec */
    for (y = (char far *)sw_buff.sw_string_ptr; *y != NUL; y++)
    {
      temp_str[i] = (BYTE)*y;
      i++;
    }
    temp_str[i] = NUL;

    if (strlen(temp_str) == 0)       /* use default logfile? */
      sprintf(temp_str, "%c:\\BACKUP.LOG", src_drive_letter);

    xlat(logfile_path, temp_str);

    if ((BYTE)logfile_path[0] == tgt_drive_letter)
      logfile_on_target = TRUE;
  }
}


/*********************************************************************/
/* Routine:   save_current_dirs                                      */
/*                                                                   */
/* Function:  Save the current directory on default drive.           */
/*            Later when we terminate we must restore it.            */
/*********************************************************************/

void save_current_dirs()
{
  src_def_dir[0] = BACKSLASH;
  get_current_dir(src_drive_letter -'A' + 1, &src_def_dir[1]);
}


/*********************************************************************/
/* Routine:   open_logfile                                           */
/*                                                                   */
/* Function:  User specified the /L parameter for a BACKUP           */
/*            log file. First try to open it. If it doesn't          */
/*            exist then create it.                                  */
/*********************************************************************/

void open_logfile()
{
  handle_logfile = extended_open(OPEN_IT, 0, (char far *)logfile_path,
                                 (WORD)(DENYWRITE+WRITEACCESS));
  if (rc == NOERROR)
    lseek(handle_logfile, EOFILE, (DWORD)0);
  else                                 /* create the file */
     handle_logfile = extended_open(CREATE_IT, (WORD)ARCHIVE,
                                    (char far *)logfile_path,
                                    (WORD)(WRITEACCESS));

  if (rc != NOERROR)                   /* terminate if can't open logfile */
    error_exit(CANT_OPEN_LOGFILE);

  display_msg(LOGGING);                /* tell user where we are logging */
  datetime();                          /* put date and time of BACKUP in logfile */

  logfile_opened = TRUE;               /* logfile is open */
}


/*********************************************************************/
/* Routine:   set_vectors                                            */
/*                                                                   */
/* Function:  Hook control break and critical vector to              */
/*            allow BACKUP to gracefully terminate.                  */
/*********************************************************************/

void set_vectors()
{
  #define  NULL 0

  setsignal(ACTIONHOOK, CTRLC);        /* handle CTRL_C */
  setsignal(ACTIONHOOK, CTRLBREAK);    /* handle CTRL_BREAK */
  (void)set_int24_vector(NULL);        /* set critical error vector (int 24h) */
}


/*********************************************************************/
/* Routine:   check_appendX                                          */
/*                                                                   */
/* Function:  Check APPEND /X status.  If it is not active,          */
/*            do nothing. If it is active, then turn it off          */
/*            and set flag indicating that we must reset it later.   */
/*********************************************************************/

void check_appendX()
{
  union REGS gregs;                    /* register set */

  gregs.x.ax = INSTALL_CHECK;          /* get installed state */
  int86(0x2f, &gregs, &gregs);

  /*
   *  1) See if append is active
   *  2) If so, figure out if DOS or PCNET version
   */
  if (gregs.h.al == 0)                 /* zero if not installed */
    append_indicator = NOT_INSTALLED;
  else                                 /* see which APPEND it is */
  {
    gregs.x.ax = GET_APPEND_VER;
    int86(0x2f, &gregs, &gregs);

    if (gregs.h.al == (BYTE)-1)        /* -1 if DOS version */
      append_indicator = DOS_APPEND;
    else
      append_indicator = NET_APPEND;
  }

  /*
   *  If it is the DOS append
   *    1) Get the current append functions (returned in BX)
   *    2) Reset append with /X support off
   */
  if (append_indicator == DOS_APPEND)
  {
    gregs.x.ax = GET_STATE;            /* get active APPEND functions */
    int86(0x2f, &gregs, &gregs);
    original_append_func = gregs.x.bx;

    gregs.x.ax = SET_STATE;
    gregs.x.bx = gregs.x.bx & (!APPEND_X_BIT);
    int86(0x2f, &gregs, &gregs);
  }
}


/*********************************************************************/
/* Routine:   get_drive_types                                        */
/*                                                                   */
/* Function:  For the source and target drives, figure out           */
/*            if they are removable or not.                          */
/*********************************************************************/

void get_drive_types()
{
  #define REMOVABLE 0

  WORD drivehandle;
  char drive_spec[3];

  /*
   *  Check Source drive.
   */
  drive_spec[0] = src_drive_letter;
  drive_spec[1] = ':';
  drive_spec[2] = NUL;

  /* Device open, source drive */
  drivehandle = handle_open(drive_spec, OPENDASD + DENYNONE);

  /* Now see if it is removable */
  if (ioctl(drivehandle) == REMOVABLE)
    source_removable = TRUE;
  else
    source_removable = FALSE;

  close_file(drivehandle);

  /*
   *  Check Target drive.
   */
  drive_spec[0] = tgt_drive_letter;
  drive_spec[1] = ':';
  drive_spec[2] = NUL;

  drivehandle = handle_open(drive_spec, OPENDASD + DENYNONE);

  if (ioctl(drivehandle) == REMOVABLE)
    target_removable = TRUE;
  else
    target_removable = FALSE;

  close_file(drivehandle);
}


/*********************************************************************/
/* Routine:   do_backup                                              */
/*                                                                   */
/* Function:  BACKUP all files that should be backed up.             */
/*********************************************************************/

void do_backup()
{
  set_default_dir();              /* set default dir to where source files are */

  find_first_file();              /* find first file to be backed up */
  if (back_it_up)                 /* if found one.... */
  {
    get_first_target();           /* get first disk (or last if /A specified) */
    check_asj();                  /* now safe to check assignd, subst */
    do
    {
      open_source_file();         /* open the file we found */
      if (source_opened)          /* if succeessful open of source */
        do_copy();                /* copy to target, handle files that span disks. */
      find_next_file();           /* search for another file */
    } while (back_it_up);         /* while there are files to back up */
    display_msg(CRLF);
  }
  else                            /* no files found */
  {
    display_msg(NONEFNDMSG);
    return_code = RETCODE_NO_FILES;    /* no files to be backed up */
  }

  clean_up_and_exit();
}


/*********************************************************************/
/* Routine:   find_first_file                                        */
/*                                                                   */
/* Function:  Find the first file conforming to user entered spec.   */
/*            If necessary, look on other directory levels also.     */
/*********************************************************************/

void find_first_file()
{
  char loop_done = FALSE;

  back_it_up = FALSE;             /* have not found a file yet ! */
  find_the_first();               /* sets the "found_a_file" flag */

  if (found_a_file)               /* if found a file */
  {
    do
    {
      if (found_a_file)           /* if you got one, then */
        see_if_it_should_be_backed_up();  /* check against entered params */

      if (!back_it_up)            /* if it shouldn't be processed... */
        find_the_next();          /* find another (sets "found_a_file" flag) */
      else
        loop_done = TRUE;         /* otherwise done */

      if (!found_a_file)          /* Don't remove this ! */
        loop_done = TRUE;         /* This has gotta stay ! */
    } while (!loop_done);
  }
}


/*********************************************************************/
/* Routine:   find_next_file                                         */
/*                                                                   */
/* Function:  Find the next file conforming to user entered spec.    */
/*********************************************************************/

void find_next_file()
{
  char loop_done = FALSE;

  back_it_up = FALSE;
  do
  {
    find_the_next();
    if (found_a_file)
    {
      see_if_it_should_be_backed_up();
      if (back_it_up)
        loop_done = TRUE;
    }
    else
      loop_done = TRUE;
  } while (!loop_done);
}


/*********************************************************************/
/* Routine:   find_the_first                                         */
/*                                                                   */
/* Function:  Find the first file conforming to user entered spec.   */
/*            Searches in current directory, if one not found then   */
/*            goes to the next level and repeats.                    */
/*********************************************************************/

void find_the_first()
{
  char loop_done = FALSE;
  char file_spec[PATHLEN];

  found_a_file = FALSE;
  sprintf(file_spec, "%c:%s", src_drive_letter, src_fn);

  do
  {
    /* Find file conforming to user-entered file spec */
    find_first(&file_spec[0], &dirhandle, dta_addr, (SYSTEM + HIDDEN));
    if (rc == NOERROR)
    {                             /* if no error */
      found_a_file = TRUE;        /* then we found a file */
      loop_done = TRUE;           /* and we are done here */
    }
    else                          /* if there was an error */
    {
      if (do_subdirs)             /* and if user said /S  */
      {
  	change_levels();          /* change DIR (sets "new_directory") */
  	if (!new_directory)       /* if there aren't any */
  	  loop_done = TRUE;       /* then were done */
      }
      else
        loop_done = TRUE;
    }
  } while (!loop_done);
}


/*********************************************************************/
/* Routine:   find_the_next                                          */
/*                                                                   */
/* Function:  Find the next file conforming to user entered spec.    */
/*********************************************************************/

void find_the_next()
{
  char loop_done = FALSE;

  found_a_file = FALSE;

  find_next(dirhandle, dta_addr);
  if (rc == NOERROR)
  {
    found_a_file = TRUE;
    loop_done = TRUE;
  }
  else
  {
    do
    {
      if (do_subdirs)
      {
        change_levels();               /* change DIR to next dir level */
  	if (!new_directory)            /* if we were successful */
  	  loop_done = TRUE;            /* then indicate that fact */
  	else                           /* otherwise */
  	{
  	  find_the_first();            /* look for first file at this level */
  	  loop_done = TRUE;
  	}
      }
      else
        loop_done = TRUE;
    } while (!loop_done);
  }
}


/*********************************************************************/
/* Routine:   change_levels                                          */
/*                                                                   */
/* Function:  Change directory to next one in the linked list        */
/*            of directories to be processed.                        */
/*********************************************************************/

void change_levels()
{
  new_directory = FALSE;
  remove_node();
}


/*********************************************************************/
/* Routine:   alloc_node                                             */
/*                                                                   */
/* Function:  Allocates a node for the linked list of subdirectories */
/*            to be processed.                                       */
/*********************************************************************/

struct node * alloc_node(unsigned int path_len)
{
  struct node *pointer;
  unsigned int malloc_size;

  malloc_size = (unsigned int) (sizeof(struct node far *) + path_len + 2);

#if defined(DEBUG)
  printf("\nMALLOCING NODE, SIZE=%04Xh...", malloc_size);
#endif

  pointer = (struct node *)malloc(malloc_size);

#if defined(DEBUG)
  if (pointer != NUL)
    printf("SUCCESSFUL, PTR=%u", (unsigned)pointer);
  else
    printf("ERROR, PTR=%u", (unsigned)pointer);
#endif

  if (pointer == NUL)
    error_exit(INSUFF_MEMORY);
  else
    return (pointer);
}


/*********************************************************************/
/* Routine:   alloc_first_node                                       */
/*                                                                   */
/* Function:  Allocate the first node in the linked list.            */
/*********************************************************************/

void alloc_first_node()
{
#if defined(DEBUG)
  printf("\nINSERTING FIRST NODE=%s", src_drive_path);
#endif

  curr_node = alloc_node(strlen(src_drive_path + 1));
  last_child = curr_node;
  strcpy(curr_node->path, src_drive_path);
  curr_node->np = NUL;
}


/*********************************************************************/
/* Routine:   insert_node                                            */
/*                                                                   */
/* Function:  Insert next node in the linked list of subdirectories  */
/*            to be processed.                                       */
/*********************************************************************/

void insert_node(char *path_addr)
{
  struct node *temp;              /* temporary pointer to a node */
  struct node *newnode;           /* same thing */

#if defined(DEBUG)
  printf("\nINSERTING NODE=%s", *path_addr);
#endif

  temp = last_child->np;
  newnode = alloc_node(strlen(path_addr));
  last_child->np = newnode;
  newnode->np = temp;
  strcpy(newnode->path, path_addr);
  last_child = newnode;
}


/*********************************************************************/
/* Routine:   remove_node                                            */
/*                                                                   */
/* Function:  CHDIR to the next level to be processed.               */
/*            Release the node for that directory.                   */
/*********************************************************************/

void remove_node()
{
  struct node *temp;

  temp = curr_node;
  last_child = curr_node->np;
  if (curr_node->np != NUL)
  {
    rc = chdir(last_child->path);
    if (rc == NOERROR)
    {
      new_directory = TRUE;
      strcpy(src_drive_path, last_child->path);

#if defined(DEBUG)
      printf("\nFREE NODE %u", (unsigned)curr_node);
#endif

      free((char *)curr_node);
      curr_node = last_child;

      if (do_subdirs)                  /* place all subdirs in linked list */
        find_all_subdirs();
    }
  }
}


/*********************************************************************/
/* Routine:   find_all_subdirs                                       */
/*                                                                   */
/* Function:  User entered "/S" parameter. Search for all            */
/*            subdirectory entries at this level. Place              */
/*            them all in the linked list of directories to          */
/*            be processed.                                          */
/*********************************************************************/

void find_all_subdirs()
{
  WORD dhandle;
  char global[6];
  char full_path[PATHLEN+20];
  struct FileFindBuf tempdta;

  sprintf(global, "%c:*.*", src_drive_letter);

  /* Find all subdirectory entries in current directory. */
  find_first(&global[0], &dhandle, &tempdta, (SUBDIR + SYSTEM + HIDDEN));
  while (rc == NOERROR)
  {
    if ((tempdta.attributes & SUBDIR) == SUBDIR) /* if it's a subdirectory */
    {
      if (tempdta.file_name[0] != '.')           /* but not "." or ".." */
      {

#ifdef DBCS
        if (src_drive_path[strlen(src_drive_path) - 1] != BACKSLASH ||
            CheckDBCSTailByte(src_drive_path,
                              &src_drive_path[strlen(src_drive_path)-1]))
#else
        if (src_drive_path[strlen(src_drive_path) - 1] != BACKSLASH)
#endif

          sprintf(full_path, "%s\\%s", src_drive_path, tempdta.file_name);
        else
          sprintf(full_path, "%s%s",  src_drive_path, tempdta.file_name);

        insert_node((char *)full_path);     /* save it in the linked list */
      }
    }
    find_next(dhandle, &tempdta);
  }
}


/*********************************************************************/
/* Routine:   get_first_target                                       */
/*                                                                   */
/* Function:  We are ready for the target disk. If it is a           */
/*            diskette, ask user to put one in. Remember             */
/*            to correctly handle /A if user wants it.               */
/*********************************************************************/

void get_first_target()
{
  if (target_removable)
    get_diskette();
  else
    get_hardfile();

  if (do_logfile)
    open_logfile();               /* open or create logfile */

  if (!do_add)
    put_disk_header();
}


/*********************************************************************/
/* Routine:   get_next_target                                        */
/*                                                                   */
/* Function:  We are ready for the next target diskette.             */
/*            Ask user to insert it.  Format if required.            */
/*            Create files, reset variables.                         */
/*********************************************************************/

void get_next_target()
{
  doing_first_target = FALSE;
  files_backed_up = 0;
  display_msg(CRLF);

  get_diskette();                 /* get it */

  disk_full = FALSE;

  if (do_logfile)
  {
    if (logfile_on_target)        /* and if logfile on the target drive */
      open_logfile();             /* open or create it */
  }

  if (file_spans_target)
    show_path();             /* show full path from root to stdout and logfile */

  put_disk_header();
  put_new_fh();
}


/*********************************************************************/
/* Routine:   see_if_it_should_be_backed_up                          */
/*                                                                   */
/* Function:  We found a file, its directory information is          */
/*            at the DTA structure. Don't backup a subdirectory      */
/*            or volume label. If /M specified, only backup files    */
/*            with archive bit set. Don't BACKUP 0 length files.     */
/*            If /D: and/or /T: specified, only backup appropriate   */
/*            files.                                                 */
/*********************************************************************/

void see_if_it_should_be_backed_up()
{
  BYTE temp[PATHLEN+20];

  back_it_up = TRUE;

  if ((dta.attributes & SUBDIR) == SUBDIR)  /* is it a directory name ? */
    back_it_up = FALSE;                     /* don't want to back it up */

  if ((dta.attributes & VOLLABEL) == VOLLABEL) /* is it a volumelabel ? */
    back_it_up = FALSE;                        /* don't want to back it up */

  if (do_modified)                          /* check ARCHIVE bit */
  {
    if ((dta.attributes & ARCHIVE) != ARCHIVE)
      back_it_up = FALSE;
  }

  if (do_time)                              /* check TIME parameter */
  {
    if (do_date)
    {
      /*
       *  If date entered, only files modified after specified time
       *  AND ONTHE DATE ENTERED will be prodessed.  Files dated
       *  after that will ignore time parameter.
       */
      if (dta.write_date == user_specified_date)
  	if (dta.write_time < user_specified_time)
  	  back_it_up = FALSE;
    }
    else
    {
      /*
       *  If user entered time with NO DATE PARM, then
       *  files modified on or after specified time will
       *  be processed, regardless of date.
       */
      if (dta.write_time < user_specified_time)
        back_it_up = FALSE;
    }
  }

  if (do_date)                              /* check DATE parameter */
  {
    if (dta.write_date < user_specified_date)
      back_it_up = FALSE;
  }

#define SAME 0

  /*
   *  If we are processing the root directory and we are looking
   *  at any of these files, then do not back them up.
   */
  if (strcmp(src_drive_path + 2, "\\") == SAME)
  {
    if (strcmp(dta.file_name,"IBMBIO.COM")  == SAME ||
        strcmp(dta.file_name,"IBMDOS.COM")  == SAME ||
        strcmp(dta.file_name,"IO.SYS")      == SAME ||
        strcmp(dta.file_name,"MSDOS.SYS")   == SAME ||
        strcmp(dta.file_name,"COMMAND.COM") == SAME ||
        strcmp(dta.file_name,"CMD.EXE")     == SAME)
      back_it_up = FALSE;
  }
  
  if (do_logfile)
  {
    strcpy(temp, src_drive_path);

    if (strlen(temp) == 3)
      temp[2] = NUL;

    sprintf(temp, "%s\\%s", temp, dta.file_name);

    if (strcmp(logfile_path, temp) == SAME)
      back_it_up = FALSE;
  }
}


/*********************************************************************/
/* Routine:   get_diskette                                           */
/*                                                                   */
/* Function:  Get the diskette from user. If unformatted             */
/*            and user entered /F, then try to FORMAT it.            */
/*            Create target files on root of diskette.               */
/*********************************************************************/

void get_diskette()
{
  union REGS qregs;

  /* repeat until either create succeeds or user chooses to quit */
  while (TRUE)                           /* M009 */
  {
    /* repeat until either deletion succeeds or user chooses to quit */
    while (TRUE)                         /* M009 */
    {
      /* repeat until either format succeeds or user chooses to quit */
      while (TRUE)                       /* M005 */
      {
        if (!do_add)
        {
          display_msg(INSERTTARGET);
          display_msg(ERASEMSG);
        }
        else
        {
          if (doing_first_target)
            display_msg(LASTDISKMSG);
          else
          {
            display_msg(INSERTTARGET);
            display_msg(ERASEMSG);
          }
        }
    
        /*
         *  If single drive system, eliminates double prompting
         *  for user to "Insert diskette for drive %1".
         */
        qregs.x.ax = SETLOGICALDRIVE;
        qregs.h.bl = tgt_drive_letter - 'A' + 1;
        intdos(&qregs, &qregs);
    
        if (target_removable)
        {
          if (format_target())                   /* M005 Start */
          {
            /* error formatting - see if user wants to continue */
            display_msg(CONTINUE_NEW_DISK);      /* M011 */
            if (GetYesNo() == YES)               /* M011 */
            {
              /* user wants to continue */
              display_msg(CRLF);                 /* M011 */
              continue;                     /* try again */
            }
            else
            {
              /* user wants to quit */
              display_msg(CRLF);                 /* M011 */
              return_code = RETCODE_ERROR;
              clean_up_and_exit();       /* does not return */
            }
          }
          else
            break;                       /* format okay, so break */
        }
      }                                          /* M005 End */
    
      backup_started = TRUE;
    
      if (do_add)                        /* if adding files */
      {
        if (doing_first_target)          /* and if its the first target */
          check_last_target();           /* verify that its a valid one */
      }
    
      display_msg(BUDISKMSG);
      display_msg(SEQUENCEMSG);

      /* delete all files in target drive root */
      if (delete_files(ROOTDIR))                 /* M009 Start */
      {
        /* error deleting files - see if user wants to continue */
        display_msg(CONTINUE_NEW_DISK);          /* M011 */
        if (GetYesNo() == YES)                   /* M011 */
        {
          /* user wants to continue */
          display_msg(CRLF);                     /* M011 */
          continue;                      /* try again */
        }
        else
        {
          /* user wants to quit */
          display_msg(CRLF);                     /* M011 */
          return_code = RETCODE_ERROR;
          clean_up_and_exit();           /* does not return */
        }
      }
      else
        break;                           /* delete okay, so break */
    }

    /* create target files */
    if (create_target())
    {
      /* error creating files - see if user wants to continue */
      display_msg(CONTINUE_NEW_DISK);            /* M011 */
      if (GetYesNo() == YES)                     /* M011 */
      {
        /* user wants to continue */
        display_msg(CRLF);                       /* M011 */
        continue;                        /* try again */
      }
      else
      {
        /* user wants to quit */
        display_msg(CRLF);                       /* M011 */
        return_code = RETCODE_ERROR;
        clean_up_and_exit();             /* does not return */
      }
    }
    else
      break;                             /* create okay, so break */
  }                                              /* M009 End */
}


/*********************************************************************/
/* Routine:   get_hardfile                                           */
/*                                                                   */
/* Function:  Target is a hardfile. FORMATTING hardfile is           */
/*            not allowed by BACKUP.  Create target files            */
/*            in BACKUP directory of disk.                           */
/*********************************************************************/

void get_hardfile()
{
  char dirname[15];

  sprintf(dirname, "%c:\\BACKUP\\*.*", tgt_drive_letter);
  if ( exist(&dirname[0]) )
  {
    if (!do_add)
      display_msg(FERASEMSG);
    backup_started = TRUE;
    delete_files(BACKUPDIR);      /* delete \BACKUP\*.* of target drive if not do_add */
  }
  else
  {
    sprintf(dirname, "%c:\\BACKUP", tgt_drive_letter);
    mkdir(dirname);
  }

  display_msg(BUDISKMSG);
  if (create_target())                           /* M009 */
    error_exit(INVTARGET);                       /* M009 */
}


/*********************************************************************/
/* Routine:   check_last_target                                      */
/*                                                                   */
/* Function:  User entered /A parameter. Make sure that              */
/*            we are not adding to a BACKUP diskette created         */
/*            with the disgusting old BACKUP format.                 */
/*            Make sure there is a BACKUP.xxx and CONTROL.xxx        */
/*            file out there.  Make sure it was the last target      */
/*            and get the sequence number.                           */
/*********************************************************************/

void check_last_target()
{
  WORD dhandle;
  WORD bytes_read;
  BYTE flag;
  char path[25];
  struct FileFindBuf tempdta;

  if (target_removable)           /* make sure no old BACKUP on here */
    sprintf(path, "%c:\\BACKUPID.@@@", tgt_drive_letter);
  else
    sprintf(path, "%c:\\BACKUP\\BACKUPID.@@@", tgt_drive_letter);

  if ( exist(path) )
    error_exit(INVTARGET);

  if (target_removable)           /* build path to control file */
    sprintf(path, "%c:\\CONTROL.*", tgt_drive_letter);
  else
    sprintf(path, "%c:\\BACKUP\CONTROL.*", tgt_drive_letter);

  /* Find the control file */
  find_first(&path[0], &dhandle, &tempdta, (SYSTEM + HIDDEN));
  if (rc != NOERROR)              /* if got one, then close dirhandle */
    error_exit(NOTLASTMSG);       /* otherwise, terminate */
  findclose(dhandle);

  /* Add drive letter to control file name */
  sprintf(path, "%c:%s", tgt_drive_letter, tempdta.file_name);

  /* Open the control file */
  handle_control = extended_open(OPEN_IT, 0, (char far *)path,
                                 (WORD)(DENYWRITE + READACCESS));
  if (rc != NOERROR)              /* if can't open it, strange error */
    error_exit(NOTLASTMSG);

  /* Get diskette sequence number */
  lseek(handle_control, BOFILE, (DWORD)9);
  bytes_read = handle_read(handle_control, 1, (char far *)&diskettes_complete);
  diskettes_complete--;           /* this disk is no longer "complete" */

  /* Seek to DH_LastDisk and read that byte */
  lseek(handle_control, BOFILE, (DWORD)138);
  bytes_read = handle_read(handle_control, 1, (char far *)&flag);

  if (flag != LAST_TARGET)        /* if it wasn't last target, terminate */
    error_exit(NOTLASTMSG);

  close_file(handle_control);     /* close the control file */
  control_opened = FALSE;         /* and say it isn't open */
}


/*********************************************************************/
/* Routine:   format_target                                          */
/*                                                                   */
/* Function:  See if the target is formatted. If not, try            */
/*            to format it.                                          */
/*********************************************************************/

int format_target()
{
  char format_parms[35];

  if (do_add)                          /* if the /A was specified */
    if (doing_first_target)            /* if it's first BACKUP.XXX diskette */
      return (NOERROR);                /* return to calling procedure */

  /*
   *  See if diskette is unformatted
   */
  rc = NOERROR;
  check_unformatted();	               /* check disk, set rc accordingly */
  
  /* If hard error, then FORMAT the target */
  if (rc != NOERROR)
  {
    if (def_drive == (tgt_drive_letter - 'A' + 1))
      find_format(FALSE);              /* dont search unformatted disk */
    else
      find_format(TRUE);
    rc = 0xFFFF;                       /* f_f will have reset it to 0 */
    display_msg(CRLF);

    sprintf(format_parms, "%c:", tgt_drive_letter);

    if (do_format_parms)
    {
      if (format_size[0] != NUL)
        strcat(format_parms, format_size);
    }
    if (need_to_swap_again)
      strcat(format_parms, " /V:BACKUP");	
    else
      strcat(format_parms, " /BACKUP /V:BACKUP");   /* use secret switch */

    checking_target = TRUE;            /* set flag to indicate format */
    rc = (spawnlp(P_WAIT, format_path, "FORMAT", format_parms, NUL));  
    if (rc != NOERROR)
    {
      /*
       *  If FORMAT returns error it could only be one of the following:
       *      i)   User aborted format (ctrl-c)
       *      ii)  User gave negative response to prompt, "Proceed with
       *           format?"
       *      iii) User entered wrong /F: entry
       *  We take care of case 1 by looking for ctrl_break and aborting
       *    the backup.
       *  We take care of all other error conditions from format by
       *    displaying an error message and returning.
       */

      if (rc == RETCODE_CTL_BREAK)     /* user hit ctl-break during format */
        control_break_handler ();      /* call the control break routine */
      else
      {						   
        display_msg(ERR_EXEC_FORMAT);  /* "Error executing FORMAT" */
        display_msg(INVTARGET);        /* "Target cannot be used for backup" */
        return (rc);                   /* M005 */
      }
    }
    checking_target = FALSE;           /* all done formatting */
    display_msg (CRLF);                /* display carrige ret */
    return (NOERROR);                  /* return to calling proc */
  }
  return (NOERROR);                    /* M005 */
}


/*********************************************************************/
/* Routine:   check_unformatted                                      */
/*                                                                   */
/* Function:  Check if the target disk is unformatted.               */
/*                                                                   */
/*    Helper routine for format_target                               */
/*    No entry parameters.                                           */
/*    No return values, but global rc is set to NOERROR if target    */
/*      is formatted.                                                */
/*********************************************************************/

#define HOOK	0
#define UNHOOK	1

void check_unformatted()
{
  WORD temp_rc;                   /* saver for global return code */

  do_dos_error(HOOK);             /* replace hard error handler */
  rc = NOERROR;                   /* reset return code */

  /* If this generates a hard error, then format target */
  checking_target = TRUE;
  disk_free_space();
  checking_target = FALSE;

  temp_rc = rc;
  do_dos_error(UNHOOK);           /* unhook hard error handler */
  rc = temp_rc;
}


/*********************************************************************/
/* Routine:   set_default_dir                                        */
/*                                                                   */
/* Function:  Sets the default directory.                            */
/*********************************************************************/

void set_default_dir()
{
  /*
   *  If there IS a backslash and if length is greater than 3, 
   *  then change dir to there.
   */
  if (com_strchr(src_drive_path, BACKSLASH) != NUL)
  {
    if (strlen(src_drive_path) >= 3)
    {
      rc = chdir(src_drive_path);
      if (rc == NOERROR)
      {
        src_drive_path[2] = BACKSLASH;
        get_current_dir(src_drive_letter - 'A' + 1, &src_drive_path[3]);
      }
      else
        error_exit(INV_PATH);
    }
  }
  curr_dir_set = TRUE;

  if (do_subdirs)                 /* if processing subdirectories too, */
  {
    alloc_first_node();           /* put current level in linked list */
    find_all_subdirs();           /* get all directory entries in that level */
  }
}


/*********************************************************************/
/* Routine:   label_target_drive                                     */
/*                                                                   */
/* Function:  Create volume label BACKUP.xxx on target               */
/*            diskette drive.                                        */
/*********************************************************************/

void label_target_drive()
{
  char fsbuf[20];

  build_ext(diskettes_complete + 1);

  sprintf(fsbuf, "%c:BACKUP.%s", tgt_drive_letter, ext);

  replace_volume_label(&fsbuf[0]);
}


/*********************************************************************/
/* Routine:   build_ext                                              */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

void build_ext(int num)
{
  if (num < 10)
    sprintf(ext, "00%u", num);
  else
  {
    if (num < 100)
      sprintf(ext, "0%u", num);
    else
      sprintf(ext, "%u", num);
  }
}


/*********************************************************************/
/* Routine:   create_target                                          */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

int create_target()                              /* M009 */
{
  char path[25];

  if (do_add)
  {
    if (doing_first_target)
    {
      if (open_target())                         /* M009 Start */
        return (INVTARGET);
      else
        return (NOERROR);                        /* M009 End */
    }
  }

  build_ext(diskettes_complete + 1);

  if (target_removable)
    sprintf(path, "%c:\\BACKUP.%s", tgt_drive_letter, ext);
  else
    sprintf(path, "%c:\\BACKUP\\BACKUP.%s", tgt_drive_letter, ext);

  handle_target = extended_open(CREATE_IT, (WORD)ARCHIVE, (char far *)path,
                                (WORD)(READWRITE));
  if (rc == NOERROR)
    target_opened = TRUE;
  else
    return (INVTARGET);                          /* M009 */

  if (target_removable)
    sprintf(path, "%c:\\CONTROL.%s", tgt_drive_letter, ext);
  else
    sprintf(path, "%c:\\BACKUP\\CONTROL.%s", tgt_drive_letter, ext);

  handle_control = extended_open(CREATE_IT, (WORD)ARCHIVE, (char far *)path,
                                 (WORD)(READWRITE));
  if (rc == NOERROR)
    control_opened = TRUE;
  else
    return (INVTARGET);                          /* M009 */

  data_file_tot_len = (DWORD)0;
  ctl_file_tot_len = (DWORD)0;

  return (NOERROR);                              /* M009 */
}


/*********************************************************************/
/* Routine:   open_target                                            */
/*                                                                   */
/* Function:  Opens the target.  This is only done if /A was         */
/*            specified and it is the first target.                  */
/*********************************************************************/

int open_target()                                /* M009 */
{
  char path[PATHLEN+20];

  /* Open BACKUP.xxx File */
  build_ext(diskettes_complete + 1);

  if (target_removable)
    sprintf(path, "%c:\\BACKUP.%s", tgt_drive_letter, ext);
  else
    sprintf(path, "%c:\\BACKUP\\BACKUP.%s", tgt_drive_letter, ext);

  /* Turn off readonly bit on BACKUP.xxx */
  set_attribute(path, (WORD)(get_attribute(path) & (WORD)READONLYOFF));
  /* Open it */
  handle_target = extended_open(OPEN_IT, 0, (char far *)path,
                                (WORD)(DENYALL + READWRITE));
  if (rc == NOERROR)
    target_opened = TRUE;
  else
    return (INVTARGET);                          /* M009 */
    
  /* Open CONTROL.xxx File */
  if (target_removable)
    sprintf(path, "%c:\\CONTROL.%s", tgt_drive_letter, ext);
  else
    sprintf(path, "%c:\\BACKUP\\CONTROL.%s", tgt_drive_letter, ext);

  set_attribute(path, (WORD)(get_attribute(path) & (WORD)READONLYOFF));
  handle_control = extended_open(OPEN_IT, 0, (char far *)path,
                                 (WORD)(DENYALL + READWRITE));
  if (rc == NOERROR)
    control_opened = TRUE;
  else
    return (INVTARGET);                          /* M009 */

  data_file_tot_len = (DWORD)lseek(handle_target, EOFILE, (DWORD)0);
  ctl_file_tot_len = (DWORD)lseek(handle_control, EOFILE, (DWORD)0);

  return (NOERROR);                              /* M009 */
}


/*********************************************************************/
/* Routine:   delete_files                                           */
/*                                                                   */
/* Function:  Delete all files in the root directory of target       */
/*            diskette, or in the BACKUP directory of the target     */
/*            hardfile.  If error occurs deleting file, try to       */
/*            reset the attribute to 0 and try it again.             */
/*********************************************************************/

int delete_files(char dirlevel)                  /* M009 */
{
  BYTE delete_path[25];
  struct FileFindBuf tempdta;
  struct FileFindBuf *tempdta_addr;
  WORD dhandle;
  BYTE delete_it;

  /*
   *  Don't delete files if we are adding files to an existing
   *  BACKUP and this is the first target.
   */
  if (do_add)
  {
    if (doing_first_target)
      return (NOERROR);                          /* M009 */
  }

  tempdta_addr = (struct FileFindBuf *)&tempdta;

  if (dirlevel == ROOTDIR)
    sprintf(delete_path, "%c:\\*.*", tgt_drive_letter);
  else
    sprintf(delete_path, "%c:\\BACKUP\\*.*", tgt_drive_letter);

  /* Find a file to delete */
  find_first((char *)&delete_path[0], &dhandle, tempdta_addr, (SYSTEM + HIDDEN));
  while (rc == NOERROR)
  {
    delete_it = TRUE;
    if (dirlevel == ROOTDIR)
      sprintf(delete_path, "%c:\\%s", tgt_drive_letter, tempdta.file_name);
    else
      sprintf(delete_path, "%c:\\BACKUP\\%s", tgt_drive_letter, tempdta.file_name);

    if (logfile_on_target)
    {
      if (strcmp(delete_path, logfile_path) == SAME)
        delete_it = FALSE;
    }

    if (delete_it == TRUE)
    {
      deleting_files = TRUE;                     /* M009 */
      delete(delete_path);
      /* fatal error if rc is file_not_found - cannot write to disk */
      if (rc == FILE_NOT_FOUND)                  /* M009 */
        return (rc);                             /* M009 */
      if (rc != NOERROR)
      {
        set_attribute(delete_path, (WORD)0);
        delete(delete_path);
        /* fatal error here, cannot write to disk */
        if (rc != NOERROR)                       /* M009 */
          return (rc);                           /* M009 */
      }
      deleting_files = FALSE;                    /* M009 */
    }
    find_next(dhandle, tempdta_addr);
  }
  return (NOERROR);                              /* M009 */
}


/*********************************************************************/
/* Routine:   exist                                                  */
/*                                                                   */
/* Function:  Does a FIND FIRST of the filespec passed at PATH_ADDR. */
/*            If so, returns TRUE, otherwise returns FALSE.          */
/*********************************************************************/

WORD exist(char *path_addr)
{
  WORD dhandle;
  WORD temprc;
  struct FileFindBuf tempdta;

  find_first(path_addr, &dhandle, &tempdta, (SUBDIR + SYSTEM + HIDDEN));
  temprc = rc;
  if (rc == NOERROR)
    findclose(dhandle);

  if (temprc != NOERROR)
    return (FALSE);
  else
    return (TRUE);
}


/*********************************************************************/
/* Routine:   open_source_file                                       */
/*                                                                   */
/* Function:  Try to open the source file at the DTA structure.      */
/*            If after MAX_RETRY_OPEN_COUNT attempts you cannot      */
/*            open it, then display an appropriate message and       */
/*            continue. If it was opened, then get the files         */
/*            extended attributes.                                   */
/*********************************************************************/

void open_source_file()
{
  int num_attempts = 0;
  char done = FALSE;
  char file_to_be_backup[20];

  source_opened = FALSE;          /* source is not opened yet */
  file_spans_target = FALSE;      /* file does not span diskettes */
  span_seq_num = 1;               /* first disk containing part of this file */
  show_path();                    /* show full path from root to stdout/logfile */
  sprintf(file_to_be_backup, "%c:%s", src_drive_letter, dta.file_name);

  do
  {                               /* attempt open */
    handle_source = extended_open(OPEN_IT, 0, (char far *)file_to_be_backup,
                                  (WORD)(DENYWRITE + READACCESS));
    if (rc != NOERROR)            /* check for error */
    {                             /* handle share errors */
      num_attempts++;             /* increment number of attempts */
      if (num_attempts == MAX_RETRY_OPEN_COUNT)
      {                           /* compare with max number */
        file_sharing_error();     /* share error opening the file */
        done = TRUE;
      }
    }
    else
    {
      source_opened = TRUE;       /* set flag indicating file is open */
      done = TRUE;                /* done in this loop */
      put_new_fh();               /* write file header to control file */
    }
  } while (!done);
}


/*********************************************************************/
/* Routine:   file_sharing_error                                     */
/*                                                                   */
/* Function:  Handle the file sharing error that just occurred.      */
/*                                                                   */
/*********************************************************************/

void file_sharing_error()
{
  union	REGS reg;

  display_msg(CRLF);
  display_msg(CONFLICTMSG);            /* say "Last file not backed" */
  return_code = RETCODE_SHARE_ERROR;   /* set errorlevel */

  if (do_logfile)
  {
    reg.x.ax = LASTNOTBACKUP;
    reg.x.bx = handle_logfile;
#define MSG_LEN 33
    reg.x.cx = (WORD)MSG_LEN;
    update_logfile(&reg, &reg);        /* in source file _msgret.sal */
  }
}


/*********************************************************************/
/* Routine:   far_ptr                                                */
/*                                                                   */
/* Function:  Gets the far pointer address from a segment:offset.    */
/*********************************************************************/

char far *far_ptr(WORD seg, WORD off)
{
  char far *p;

  PUT_SEG(p, seg);
  PUT_OFF(p, off);
  return (p);
}


/*********************************************************************/
/* Routine:   do_copy                                                */
/*                                                                   */
/* Function:  Copy the source file to the BACKUP.xxx file            */
/*            If there are extended attributes, write them           */
/*            to the BACKUP.xxx file.                                */
/*********************************************************************/

void do_copy()
{
  WORD bytes_read;
  WORD bytes_to_read = data_file_alloc_size;     /* read size = buffer size */
  char done = FALSE;
  char file_to_be_backup[20];

  part_size = (DWORD)0;
  cumul_part_size = (DWORD)0;

  if (source_opened)
  {
    do
    {
      bytes_read = handle_read(handle_source, bytes_to_read, far_ptr(selector, 0));
      if (bytes_read == 0)
        done = TRUE;
      else
  	write_to_target(bytes_read);

      if (bytes_read < bytes_to_read)
        done = TRUE;
    } while (!done);

    close_file(handle_source);         /* close the source file handle */
    source_opened = FALSE;             /* source is not open */
    sprintf(file_to_be_backup, "%c:%s", src_drive_letter, dta.file_name);
    reset_archive_bit(file_to_be_backup); /* reset archive bit on source file */
    files_backed_up++;                 /* increment number of files backed up */
  }
}


/*********************************************************************/
/* Routine:   show_path                                              */
/*                                                                   */
/* Function:  Display to stdout the full path from root.             */
/*            If we are logging, put full path there too.            */
/*********************************************************************/

void show_path()
{
  char done_path[PATHLEN + 20];
  char logfile_entry[PATHLEN + 22];
  WORD written = 0;

#ifdef DBCS
  if (src_drive_path[strlen(src_drive_path) - 1] != BACKSLASH ||
      CheckDBCSTailByte(src_drive_path,&src_drive_path[strlen(src_drive_path) - 1]))
#else
  if (src_drive_path[strlen(src_drive_path) - 1] != BACKSLASH)
#endif

    sprintf(done_path, "%s\\%s", src_drive_path, dta.file_name);
  else
    sprintf(done_path, "%s%s", src_drive_path, dta.file_name);

  /* Display logfile path on screen */
  done_path[0] = 0xd;
  done_path[1] = 0xa;
  handle_write(STDOUT, strlen(done_path), (char far *)&done_path[0]);

  if (do_logfile)
  {
    build_ext(diskettes_complete + 1);
    sprintf(logfile_entry, "\15\12%s  %s", ext, &done_path[2]);
    written = handle_write(handle_logfile, strlen(logfile_entry),
                           (char far *)&logfile_entry[0]);
    if ( written != strlen(logfile_entry) || (rc != NOERROR) )
    {
      display_msg(LOGFILE_TARGET_FULL);
      do_logfile = FALSE;
    }
  }
}


/*********************************************************************/
/* Routine:   reset_archive_bit                                      */
/*                                                                   */
/* Function:  Sets the attribute of the source file to what          */
/*            it was before, except the archive bit is reset.        */
/*********************************************************************/

void reset_archive_bit(char *path_addr)
#define ARCHIVE_MASK 223
{
  WORD attrib;

  attrib = get_attribute(path_addr);
  attrib = attrib & (WORD)ARCHIVE_MASK;
  set_attribute(path_addr, attrib);
}


/*********************************************************************/
/* Routine:   write_to_target                                        */
/*                                                                   */
/* Function:  Write a specified # of bytes to                        */
/*            target. Handle disk full conditions                    */
/*            and everything else.                                   */
/*********************************************************************/

void write_to_target(WORD bytes_to_write)
{
  WORD bytes_written;
  WORD written;

  bytes_written = handle_write(handle_target, bytes_to_write, far_ptr(selector, 0));
  written = bytes_written;

  if (bytes_written == bytes_to_write)      /* if we wrote it all... */
  {
    part_size += (DWORD)written;            /* update size of this part */
    cumul_part_size += (DWORD)written;      /* update size of this part */
    data_file_tot_len += (DWORD)written;    /* update length of BACKUP.xxx file */
  }
  else
  {
    /* fill up current target */
    written = write_till_target_full(bytes_to_write,0);
    bytes_written += written;               /* update # bytes written */
    part_size += (DWORD)written;            /* update size of this part */
    cumul_part_size += (DWORD)written;      /* update size of this part */
    data_file_tot_len += (DWORD)written;    /* update length of BACKUP.xxx file */
    close_out_current_target();             /* update CONTROL.xxx, close files */
    get_next_target();                      /* get next disk from user */

    /* write rest of buffer */
    written = handle_write(handle_target, bytes_to_write - bytes_written,
                           far_ptr(selector, bytes_written));
    bytes_written += written;               /* update # bytes written */
    part_size = (DWORD)written;             /* update size of this part */
    cumul_part_size += (DWORD)written;      /* update size of this part */
    data_file_tot_len += (DWORD)written;    /* update length of BACKUP.xxx */
  }
}


/*********************************************************************/
/* Routine:   write_till_target_full                                 */
/*                                                                   */
/* Function:  Find out how much space is left on the disk,           */
/*            and use it all up.                                     */
/*********************************************************************/

WORD write_till_target_full(WORD bytes_to_write, WORD begin_offset)
{
  WORD written;
  WORD bfree;

  bytes_to_write;                 /* kill warning msg */

  bfree = (unsigned) disk_free_space();
  written = handle_write(handle_target, bfree, far_ptr(selector, begin_offset));

  return (written);
}


/*********************************************************************/
/* Routine:   close_out_current_target                               */
/*                                                                   */
/* Function:  Update CONTROL.xxx file, close it, close BACKUP.xxx,   */
/*            make files READONLY, die if backing up to hardfile.    */
/*********************************************************************/

void close_out_current_target()
{
  BYTE last = LAST_TARGET;

  disk_full = TRUE;               /* the disk is full */

  if (part_size != 0)             /* if we wrote something...*/
  {
    file_spans_target = TRUE;     /* this file spans diskettes */
    files_backed_up++;            /* increment # files backed up on this target */
  }

  if (files_backed_up > 0)        /* if backed up something */
  {
    /* increment Num_Entries field in directory block and NextDB field */
    update_db_entries(files_backed_up);
  }

  update_fh_entries();            /* update the fields in file header */

  if (!target_removable)
  {
    /* Update DH_LastDisk == LAST_DISK */
    lseek(handle_control, BOFILE, (DWORD)(DHLENGTH - 1));
    handle_write(handle_control,1, (char far *)&last);
  }

  if (control_opened)             /* if the control file is open */
  {
    close_file(handle_control);   /* close it */
    control_opened = FALSE;       /* set flag to show it isn't open */
  }

  if (target_opened)
    close_file(handle_target);    /* close files */

  target_opened = FALSE;          /* indicate that target is not open */

  if (file_spans_target)          /* if file spans to another diskette */
    span_seq_num++;               /*   increment the sequence number */

  mark_files_read_only();         /* set ReadOnly Attr of BACKUP/CONTROL files */

  if (logfile_on_target)          /* if logfile resides on target drive */
  {
    close_file(handle_logfile);   /* close it */
    logfile_opened = FALSE;       /* and set flag to show it's not open */
  }

  if (!target_removable)          /* if target is a hardfile */
  {
    display_msg(LASTNOTBACKUP);   /* say "Last file not backed up" */
    error_exit(FDISKFULLMSG);     /* give error message and quit */
  }

  diskettes_complete++;		/* increment number of diskettes complete */
}


/*********************************************************************/
/* Routine:   mark_as_not_last_target                                */
/*                                                                   */
/* Function:  Sets the field in the disk header indicating           */
/*            this is not the last target.                           */
/*********************************************************************/

void mark_as_not_last_target()
{
  BYTE last = NOT_LAST_TARGET;
  DWORD	db_offset;
  DWORD	pointer;

  /* Update DH_LastDisk = NOT_LAST_TARGET */
  lseek(handle_control, BOFILE, (DWORD)(DHLENGTH - 1));
  handle_write(handle_control, 1, (char far *)&last);

  /* Get first DB_NextDB */
  pointer = lseek(handle_control, BOFILE, (DWORD)(DHLENGTH + 66));
  handle_read(handle_control, 4, (char far *)&db_offset);

  /* Get offset of last Dir Block */
  while (db_offset != (DWORD)LAST_DB)
  {
    pointer = lseek(handle_control, BOFILE, (DWORD)db_offset + 66);
    handle_read(handle_control, 4, (char far *)&db_offset);
  }

  /* Change DB_NextDB field to point to EOF */
  lseek(handle_control, BOFILE, (DWORD)pointer);
  handle_write(handle_control, 4, (char far *)&ctl_file_tot_len);

  lseek(handle_control, EOFILE, (DWORD)0);
}


/*********************************************************************/
/* Routine:   mark_as_last_target                                    */
/*                                                                   */
/* Function:  Sets the field in the disk header indicating           */
/*            this is the last target.  Also updates the             */
/*            directory block to indicate the number of              */
/*            files that are backed up.                              */
/*********************************************************************/

void mark_as_last_target()
{
  BYTE last = LAST_TARGET;

  /* Update DH_LastDisk == LAST_DISK */
  lseek(handle_control, BOFILE, (DWORD)(DHLENGTH - 1));
  handle_write(handle_control, 1, (char far *)&last);

  /* Update DB_NumEntries == FILES_BACKED_UP */
  lseek(handle_control, BOFILE,(DWORD)(curr_db_begin_offset + 64));
  handle_write(handle_control, 2, (char far *)&files_backed_up);

  /* Update FH Entries */
  update_fh_entries();
}


/*********************************************************************/
/* Routine:   update_db_entries                                      */
/*                                                                   */
/* Function:  Updates the DB entries.                                */
/*********************************************************************/

void update_db_entries(WORD entries)
{
  lseek(handle_control, BOFILE, (DWORD)(curr_db_begin_offset + 64));

  /* Update DB_num_entries */
  handle_write(handle_control, 2, (char far *)&entries);

  /* Update DB_NextDB only if we are not at the end of a disk */
  if (!disk_full)
    handle_write(handle_control, 4, (char far *)&ctl_file_tot_len);

  lseek(handle_control, EOFILE, (DWORD)0);
}


/*********************************************************************/
/* Routine:   update_fh_entries                                      */
/*                                                                   */
/* Function:  Update following fields in Current File Header:        */
/*                                                                   */
/*              FH_Flags: Indicate file successfully processed.      */
/*                        Indicate if this is last part or not.      */
/*                                                                   */
/*              FH_PartSize: Indicate number of bytes written.       */
/*********************************************************************/

void update_fh_entries()
{
  BYTE flag;

  if (!file_spans_target)
    flag = (BYTE)(LASTPART + SUCCESSFUL);
  else
    flag = (BYTE)(NOTLASTPART + SUCCESSFUL);

  if (!target_removable)
  {
    if (disk_full)
      flag = (BYTE)(LASTPART + NOTSUCCESSFUL);
  }

  /* Go to FLAG field */
  lseek(handle_control, BOFILE, (DWORD)(curr_fh_begin_offset + 13));

  /* write the FLAG field to control file */
  handle_write(handle_control, 1, (BYTE far *)&flag);

  /* go to PARTSIZE field */
  lseek(handle_control, CURRPOS, (DWORD)10);

  /* write the PARTSIZE field to control file */
  handle_write(handle_control, 4, (char far *)&part_size);

  lseek(handle_control, EOFILE, (DWORD)0);  /* go back to end-of-file */
}


/*********************************************************************/
/* Routine:   mark_files_read_only                                   */
/*                                                                   */
/* Function:  Set the READ-ONLY attribute on BACKUP.xxx and          */
/*            CONTROL.xxx.                                           */
/*********************************************************************/

void mark_files_read_only()
{
  char path[25];

  build_ext(diskettes_complete + 1);

  if (target_removable)
  {
    sprintf(path, "%c:\\CONTROL.%s", tgt_drive_letter, ext);
    set_attribute(path, (WORD)(ARCHIVE + READONLY));
    sprintf(path, "%c:\\BACKUP.%s", tgt_drive_letter, ext);
    set_attribute(path, (WORD)(ARCHIVE + READONLY));
  }
  else
  {
    sprintf(path, "%c:\\BACKUP\\CONTROL.%s", tgt_drive_letter, ext);
    set_attribute(path, (WORD)(ARCHIVE + READONLY));
    sprintf(path, "%c:\\BACKUP\\BACKUP.%s", tgt_drive_letter, ext);
    set_attribute(path, (WORD)(ARCHIVE + READONLY));
  }

  if (target_removable)
    label_target_drive();
}


/*********************************************************************/
/* Routine:   put_disk_header                                        */
/*                                                                   */
/* Function:  Fills in the disk header.                              */
/*********************************************************************/

void put_disk_header()
{
  struct Disk_Header dh;
  int i;

  dh.DH_Length = DHLENGTH;                  /* DH_Length */
  strcpy(dh.DH_Identifier, "BACKUP  ");     /* DH_Identifier */
  dh.DH_Sequence = diskettes_complete + 1;  /* DH_Sequence */
  for (i = 0; i <= 128; i++)
    dh.DH_reserved[i] = NUL;                /* DH_Reserved */
  dh.DH_LastDisk = NOT_LAST_TARGET;         /* DH_LastDisk - Assume NOT LAST TARGET */

  write_to_control_file((char far *)&dh, DHLENGTH);
  put_new_db();
}

/*********************************************************************/
/* Routine:   put_new_db                                             */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

void put_new_db()
{
  struct Dir_Block db;
  int i;

  if (files_backed_up > 0)
    update_db_entries(files_backed_up);     /* update entries in previous db */

  curr_db_begin_offset = ctl_file_tot_len;  /* use for updating when done w/ cur dir */

  db.DB_Length = DBLENGTH;             /* length, in bytes, of dir block */
  for (i = 0; i <= 63; i++)
    db.DB_Path[i] = NUL;               /* ascii path of this dir, drive omitted */

  strcpy(db.DB_Path, &src_drive_path[3]);
  db.DB_NumEntries = 0;                     /* # filenames currently in list */
  db.DB_NextDB = (DWORD)LAST_DB;            /* offset of next directory block */

  write_to_control_file((char far *)&db, DBLENGTH);
  new_directory = FALSE;
  files_backed_up = 0;
}


/*********************************************************************/
/* Routine:   put_new_fh                                             */
/*                                                                   */
/* Function:  We are about to backup a file. Write the               */
/*            file header to the control file.                       */
/*********************************************************************/

void put_new_fh()
{
  struct File_Header fh;
  int i;

  /*
   *  If adding files, and it is the last diskette from previous backup,
   *  and we have not backed up ANY yet, then mark this diskette as NOT
   *  the last.
   */
  if (do_add)
  {
    if (doing_first_target)
      if (files_backed_up == 0)
        mark_as_not_last_target();
  }

  if (new_directory)              /* if file is in a different directory */
    put_new_db();                 /* create new directory block */

  curr_fh_begin_offset = ctl_file_tot_len;

  fh.FH_Length = FHLENGTH;                  /* length in bytes of file header */ 
  for (i = 0; i <= 11; i++)
    fh.FH_FName[i] = NUL;
  strcpy(fh.FH_FName, dta.file_name);       /* ascii file name */

  fh.FH_FLength	= (DWORD)dta.file_size;     /* length of file */
  fh.FH_FSequence = span_seq_num;           /* sequence # for files that span */
  fh.FH_BeginOffset = data_file_tot_len;    /* offset where segment begins */ 
  fh.FH_Attribute = dta.attributes;         /* file attribute from directory */    
  fh.FH_FTime = dta.write_time;             /* time when file was last modified */ 
  fh.FH_FDate = dta.write_date;             /* date when file was last modified */ 
  fh.FH_Flags = LASTPART + SUCCESSFUL;

  if (file_spans_target)
  {
    /* length of this part of file */ 
    fh.FH_PartSize  = (DWORD)(dta.file_size - cumul_part_size);
    file_spans_target = FALSE;
  }
  else
    fh.FH_PartSize = (DWORD)dta.file_size;  /* length of this part of file */ 

  write_to_control_file((char far *)&fh, FHLENGTH);
}


/*********************************************************************/
/* Routine:   write_to_control_file                                  */
/*                                                                   */
/* Function:  Write to the control file and update counters.         */
/*********************************************************************/

void write_to_control_file(char far *address, unsigned short len)
{
  WORD written;

  written = handle_write(handle_control, len, address);
  ctl_file_tot_len = ctl_file_tot_len + (DWORD)written;
}


/*********************************************************************/
/* Routine:   control_break_handler                                  */
/*                                                                   */
/* Function:  Set errorlevel and call routines to close files        */
/*            and terminate.                                         */
/*********************************************************************/

void control_break_handler()
{
  return_code = RETCODE_CTL_BREAK;
  clean_up_and_exit();
}


/*********************************************************************/
/* Routine:   display_it                                             */
/*                                                                   */
/* Function:  Display the requested message to the standard output   */
/*            device.                                                */
/*                                                                   */
/*   Input:                                                          */
/*	   1) (WORD) Number of the message to be displayed.          */
/*	   2) (WORD) Handle to be written to.                        */
/*	   3) (WORD) Substitution Count                              */
/*	   4) (WORD) Flag indicating user should "Strike any key..." */
/*	   5) (WORD) Num indicating message class                    */
/*                                                                   */
/*   Output:                                                         */
/*	   The message corresponding to the requested msg number     */
/*	   will be written to the requested handle.  If requested,   */
/*	   substitution text will be inserted as required.  The      */
/*         Substitution List is global and, if used, will be         */
/*         initialized by DISPLAY_MSG before calling this routine.   */
/*                                                                   */
/*   Normal Exit:                                                    */
/*	   Message will be successfully written to requested handle. */
/*                                                                   */
/*   Error Exit:                                                     */
/*	   None.                                                     */
/*********************************************************************/

void display_it(int msg_number, WORD handle, int subst_count, BYTE waitflag,
                BYTE class)            /* 1=DOSerror, 2=PARSE, -1=Utility */
{
  inregs.x.ax = msg_number;
  inregs.x.bx = handle;
  inregs.x.cx = subst_count;
  inregs.h.dh = class;
  inregs.h.dl = (BYTE)waitflag;
  inregs.x.si = (WORD)(char far *)&sublist;

  sysdispmsg(&inregs, &outregs);

  user_response[0] = outregs.h.al;     /* user input, if any  -  M011 */
}


/*********************************************************************/
/* Routine:   display_msg                                            */
/*                                                                   */
/* Function:  Display the messages referenced by variable            */
/*            MSG_NUM to either STDOUT or STDERR.  In some cases     */
/*            insert text into the body of the message.              */
/*********************************************************************/

void display_msg(int msg_num)
{
  switch (msg_num)
  {
    case NONEFNDMSG :
        {
          display_it (msg_num, STDOUT, 0, NOWAIT, (BYTE)UTIL_MSG);
          break;
        }
    case INSUFF_MEMORY     :
    case ERR_EXEC_FORMAT   :
    case INV_PATH          :
    case INV_DATE          :
    case INV_TIME          :
    case NO_SOURCE         :
    case NO_TARGET         :
    case SRC_AND_TGT_SAME  :
    case BAD_DOS_VER       :
    case INV_DRIVE         :
    case CANT_OPEN_LOGFILE :
    case INVTARGET         :
    case SRCISTGT          :
    case TGTISSRC          :
    case NOTLASTMSG        :
    case CONFLICTMSG       :
    case CRLF              :
    case CANT_FIND_FORMAT  :
    case LASTNOTBACKUP     :
    case DISK_UNFORMAT     :                     /* M011 */
        {
          display_it (msg_num, STDERR, 0, NOWAIT, (BYTE)UTIL_MSG);
          break;
        }
    case LOGFILE_TARGET_FULL :
    case INSERT_FORMAT       :                   /* M011 */
        {
          display_it (msg_num, STDERR, 0, NOWAIT, (BYTE)UTIL_MSG);
          display_it (PRESS_ANY_KEY, STDERR, 0, WAIT, (BYTE)UTIL_MSG);
          break;
        }
    case NO_FORMAT_FND     :                     /* M011 Start */
    case CONTINUE_NEW_DISK :
        {
          display_it (msg_num, STDERR, 0, YESNOWAIT, (BYTE)UTIL_MSG);
          break;  
        }                                        /* M011 End */
    case LOGGING :
        {
          sublist.value1 = (char far *)&logfile_path[0];
          sublist.flags1 = LEFT_ALIGN + CHAR_FIELD_ASCIIZ;
          sublist.pad_char1 = ' ';
          sublist.max_width1 = (BYTE)strlen(logfile_path);
          sublist.min_width1 = sublist.max_width1;
          display_it (msg_num, STDOUT, 1, NOWAIT, (BYTE)UTIL_MSG);
          break;
        }
    case CANT_FORMAT_HARDFILE :
        {
          sublist.value1 = (char far *)&tgt_drive_letter;
          sublist.flags1 = LEFT_ALIGN + CHAR_FIELD_ASCIIZ;
          sublist.pad_char1 = ' ';
          sublist.max_width1 = 1;
          sublist.min_width1 = 1;
          display_it (msg_num, STDERR, 1, NOWAIT, (BYTE)UTIL_MSG);
          break;
        }
    case BUDISKMSG    :
    case FDISKFULLMSG :
        {
          sublist.value1 = (char far *)&tgt_drive_letter;
          sublist.flags1 = LEFT_ALIGN + CHAR_FIELD_ASCIIZ;
          sublist.pad_char1 = ' ';
          sublist.max_width1 = 1;
          sublist.min_width1 = 1;
          display_it (msg_num, STDERR, 1, NOWAIT, (BYTE)UTIL_MSG);
          break;
        }
    case ERASEMSG    :
    case FERASEMSG   :
    case LASTDISKMSG :
        {
          sublist.value1 = (char far *)&tgt_drive_letter;
          sublist.flags1 = LEFT_ALIGN + CHAR_FIELD_ASCIIZ;
          sublist.pad_char1 = ' ';
          sublist.max_width1 = 1;
          sublist.min_width1 = 1;
          display_it (msg_num, STDERR, 1, NOWAIT, (BYTE)UTIL_MSG);
          display_it (PRESS_ANY_KEY, STDERR, 0, WAIT, (BYTE)UTIL_MSG);
          break;
        }
    case INSERTSOURCE :
        {
       	  sublist.value1 = (char far *)&src_drive_letter;
       	  sublist.flags1 = LEFT_ALIGN + CHAR_FIELD_ASCIIZ;
       	  sublist.pad_char1 = ' ';
       	  sublist.max_width1 = 1;
       	  sublist.min_width1 = 1;
       	  display_it (msg_num, STDERR, 1, NOWAIT, (BYTE)UTIL_MSG);
       	  display_it (PRESS_ANY_KEY, STDERR, 0, WAIT, (BYTE)UTIL_MSG);
       	  break;
        }
    case SEQUENCEMSG :
        {
       	  build_ext(diskettes_complete + 1);
       	  if (diskettes_complete + 1 < 100)
       	  {
       	    sublist.value1 = (char far *)&ext[1];
       	    sublist.max_width1 = 2;
       	  }
       	  else
       	  {
       	    sublist.value1 = (char far *)&ext[0];
       	    sublist.max_width1 = 3;
       	  }
       	  sublist.flags1 = LEFT_ALIGN + CHAR_FIELD_ASCIIZ;
       	  sublist.pad_char1 = ' ';
       	  sublist.min_width1 = sublist.max_width1;
       	  display_it (msg_num, STDOUT, 1, NOWAIT, (BYTE)UTIL_MSG);
       	  break;
        }
    case INSERTTARGET :
        {
       	  build_ext(diskettes_complete + 1);
       	  if (diskettes_complete + 1 < 100)
       	  {
       	    sublist.value1 = (char far *)&ext[1];
       	    sublist.max_width1 = 2;
       	  }
       	  else
       	  {
       	    sublist.value1 = (char far *)&ext[0];
       	    sublist.max_width1 = 3;
       	  }

       	  sublist.flags1 = LEFT_ALIGN + CHAR_FIELD_ASCIIZ;
       	  sublist.pad_char1 = ' ';
       	  sublist.min_width1 = sublist.max_width1;

       	  sublist.value2 = (char far *)&tgt_drive_letter;
       	  sublist.flags2 = LEFT_ALIGN + CHAR_FIELD_ASCIIZ;
       	  sublist.pad_char2 = ' ';
       	  sublist.max_width2 = 1;
       	  sublist.min_width2 = 1;

       	  display_it (msg_num, STDERR, 2, NOWAIT, (BYTE)UTIL_MSG);
       	  break;
        }
    case INVPARM :
        {
       	  display_it (INVPARM, STDERR, 1, NOWAIT, (BYTE)PARSEERROR);
       	  break;
        }
  }
}


/*********************************************************************/
/* Routine:   display_options_exit                                   */
/*                                                                   */
/* Function:  Display the options help message, and then exit        */
/*            so the user can retry the command.                     */
/*********************************************************************/

void display_options_exit()
{
  int MsgNbr;

  /*
   *  Display all the message lines in the
   *  options help message.
   */
  for (MsgNbr = MSG_OPTIONS_FIRST; MsgNbr <= MSG_OPTIONS_LAST; MsgNbr++)
  {
    display_it(MsgNbr, STDOUT, 0, NOWAIT, (BYTE)UTIL_MSG);
  }

  /*
   *  Set the return code to show no error,
   *  and exit so the user can retry the command.
   */
  return_code = 0;
  clean_up_and_exit();            /* does not return */
}


/*********************************************************************/
/* Routine:   error_exit                                             */
/*                                                                   */
/* Function:  Display appropriate error message, set                 */
/*            the return code, and call clean_up_and_exit.           */
/*********************************************************************/

void error_exit(int error_type)
{
  display_msg(error_type);
  return_code = RETCODE_ERROR;
  clean_up_and_exit();
}


/*********************************************************************/
/* Routine:   restore_default_directories                            */
/*                                                                   */
/* Function:  Restore the original current directory on the          */
/*            source drive.                                          */
/*********************************************************************/

void restore_default_directories()
{
  char path[PATHLEN + 20];

  sprintf(path, "%c:%s", src_drive_letter, src_def_dir);
  chdir(path);
}


/*********************************************************************/
/* Routine:   clean_up_and_exit                                      */
/*                                                                   */
/* Function:  Update BACKUP and CONTROL files.                       */
/*            Close open files.                                      */
/*            Mark BACKUP, CONTROL file read only                    */
/*            Restore default drive and directories                  */
/*            Deallocate buffers                                     */
/*********************************************************************/

void clean_up_and_exit()
{
  char name[15];

  if (source_opened)
  {
    close_file(handle_source);
    source_opened = FALSE;             /* indicate source is not open */
  }

  if (target_opened)
  {
    close_file(handle_target);
    target_opened  = FALSE;            /* indicate target is not open */
  }

  if (control_opened)
  {
    mark_as_last_target();
    close_file(handle_control);
    control_opened = FALSE;
    mark_files_read_only();
  }

  if (logfile_opened)
  {
    close_file(handle_logfile);
    logfile_opened = FALSE;
  }

  if ( (files_backed_up == 0) && (!checking_target) && (!do_add) && 
       (backup_started) && (!deleting_files) )        /* M009 */
  {
    deleting_files = TRUE;                            /* M009 */
    /*
     *  Note:  We do NOT want to reset "deleting_files" to FALSE, since
     *  we're exitting.                                  M009
     */
    if (target_removable)
    {
      build_ext(diskettes_complete + 1);
      sprintf(name, "%c:\\BACKUP.%s", tgt_drive_letter, ext);
      set_attribute(name,(WORD)0);
      delete(name);

      sprintf(name, "%c:\\CONTROL.%s", tgt_drive_letter, ext);
      set_attribute(name, (WORD)0);
      delete(name);
    }
    else
    {
      delete_files(BACKUPDIR);
    }
  }

  if (def_drive_set)
    set_default_drive(def_drive);

  if (curr_dir_set)
    restore_default_directories();

  if (buffers_allocated)
    free_seg(selector);

  terminate();
}


/*********************************************************************/
/* Routine:   GetYesNo                                               */
/*                                                                   */
/* Function:  Checks if the user input is Yes or No.                 */
/*            Returns 0 if No, 1 if Yes, 2 if neither yes or no.     */
/* M011                                                              */
/*********************************************************************/

int GetYesNo()
{
  inregs.x.ax = 0x6523;
  inregs.h.dl = user_response[0];
  intdos(&inregs, &outregs);
  return (outregs.x.ax);
}



/*********************************************************************/
/*                                                                   */
/*                     DOS FAMILY API CALLS                          */
/*                                                                   */
/*********************************************************************/


/*********************************************************************/
/* Routine:   handle_open                                            */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

WORD handle_open(char *path_addr, WORD mode)
{
  WORD handle;
  WORD action;

#if defined(DEBUG)
  printf("\nDOSOPEN FILE=%s, MODE=%04Xh...", path_addr, mode);
#endif

  rc = DOSOPEN((char far *)path_addr,   /* path address */
	    (unsigned far *)&handle,    /* return area for handle */
	    (unsigned far *)&action,    /* return area for action performed */
	    (DWORD)0,                   /* file Size */
	    (WORD)0,                    /* file attribute */
	    (WORD)1,                    /* flag: only open file if it exists */
	    (WORD)mode,                 /* mode */
	    (DWORD)0                    /* reserved */
	   );

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL, HANDLE=%04Xh", handle);
  else
    printf("ERROR, RC=%04Xh", rc);
#endif

  return (handle);
}


/*********************************************************************/
/* Routine:   lseek                                                  */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

DWORD lseek(WORD handle,
            BYTE method,     /* 0=BOF+Offset, 1=CurrPos+Offset, 2=EOF+Offset */
            DWORD distance)
{
  DWORD pointer;

#if defined(DEBUG)
  printf("\nDOSCHGFILEPTR HANDLE=%04Xh, METHOD=%02Xh, DIST=%08lXh...", handle,
           method, distance);
#endif

  rc = DOSCHGFILEPTR(handle, distance, method, (DWORD far *)&pointer);

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL, POINTER=%08lXh", pointer);
  else
    printf("ERROR, RC=%04Xh", rc);
#endif

  return ((DWORD)pointer);
}


/*********************************************************************/
/* Routine:   handle_read                                            */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

WORD handle_read(WORD handle, WORD length, char far *address)
{
  WORD num_read;

#if defined(DEBUG)
  printf("\nDOSREAD HANDLE=%04Xh, BYTES=%04Xh, ADDR(off:seg)=%04X:%04X...",
          handle, length, address);
#endif

  rc = _dos_read(handle, address, length, (unsigned *)&num_read);

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("READ %04Xh", num_read);
  else
    printf("ERROR, RC=%04Xh", rc);
#endif

  return (num_read);
}


/*********************************************************************/
/* Routine:   handle_write                                           */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

WORD handle_write(WORD handle, WORD length, char far *address)
{
  WORD written;

#if defined(DEBUG)
  printf("\nDOSWRITE HANDLE=%04Xh, BYTES=%04Xh, ADDR(off:seg)=%04X:%04X...",
           handle, length, address);
#endif

  if (length != 0)
    rc = DOSWRITE(handle, address, length, (unsigned far *)&written);
  else
  {
    written = 0;
    rc = NOERROR;
  }

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("WROTE %04Xh", written);
  else
    printf("ERROR, RC=%04Xh", rc);
#endif

  return (written);
}

/*********************************************************************/
/* Routine:   close_file                                             */
/*                                                                   */
/* Function:  Close the file handle specified.                       */
/*********************************************************************/

void close_file(WORD handle)
{
#if defined(DEBUG)
  printf("\nDOSCLOSE HANDLE=%04Xh...", handle);
#endif

  rc = DOSCLOSE(handle);

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL");
  else
    printf("ERROR, RC=%04Xh", rc);
#endif
}

/*********************************************************************/
/* Routine:   get_attribute                                          */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

WORD get_attribute(char *path_addr)
{
  WORD attribute;

#if defined(DEBUG)
  printf("\nDOSQFILEMODE %s...", path_addr);
#endif

  rc = DOSQFILEMODE((char far *)path_addr, (unsigned far *)&attribute, (DWORD)0);

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL, ATTRIB=%04Xh", attribute);
  else
    printf("ERROR, RC=%04Xh", rc);
#endif

  return (attribute);
}


/*********************************************************************/
/* Routine:   set_attribute                                          */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

void set_attribute(char *path_addr, WORD attribute)
{
#if defined(DEBUG)
  printf("\nDOSSETFILEMODE FILE=%s, ATTRIB=%04Xh...", path_addr, attribute);
#endif

  rc = DOSSETFILEMODE((char far *)path_addr, attribute, (DWORD)0);

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL");
  else
    printf("ERROR, RC=%04Xh", rc);
#endif
}


/*********************************************************************/
/* Routine:   get_current_drive                                      */
/*                                                                   */
/* Function:  Returns the current drive (1=A, 2=B).                  */
/*********************************************************************/

WORD get_current_drive()
{
  WORD drive;                     /* 1 = a */
  DWORD drivemap;

#if defined(DEBUG)
  printf("\nDOSQCURDISK DRIVE (1=A)...");
#endif

  rc = DOSQCURDISK((unsigned far *)&drive, (DWORD far *)&drivemap);

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL, DRIVE=%04Xh", drive);
  else
    printf("ERROR, RC=%04Xh", rc);
#endif

  return (drive);
}

/*********************************************************************/
/* Routine:   set_default_drive                                      */
/*                                                                   */
/* Function:  Changes the current drive (1=A, 2=B).                  */
/*********************************************************************/

void set_default_drive(WORD drive)
{
#if defined(DEBUG)
  printf("\nDOSSELECTDISK (1=A) TO %04Xh...", drive);
#endif

  rc = DOSSELECTDISK(drive);

  if (rc == NOERROR)
    def_drive_set = TRUE;

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL");
  else
    printf("ERROR, RC=%04Xh", rc);
#endif
}


/*********************************************************************/
/* Routine:   get_current_dir                                        */
/*                                                                   */
/* Function:  Gets the current directory.                            */
/*********************************************************************/

void get_current_dir(WORD drive,       /* 0=default, 1=a, ... */
                     char *path_addr)  /* pointer to path buffer */ 
{
  WORD path_buff_len = PATHLEN + 20;

#if defined(DEBUG)
  printf("\nDOSQCURDIR DRIVE (0=def) %04Xh...", drive);
#endif

  rc = DOSQCURDIR(drive, (char far *)path_addr, (unsigned far *)&path_buff_len);

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL, CURRENT DIR IS = \\%s", path_addr);
  else
    printf("ERROR, RC=%04Xh", rc);
#endif
}


/*********************************************************************/
/* Routine:   find_first                                             */
/*                                                                   */
/* Function:  Finds the first file fitting the given path.           */
/*********************************************************************/

void find_first(char *path_addr, WORD *dirhandle_addr,
                struct FileFindBuf *dta_address, WORD attrib)
{
  WORD numentries = 1;
  WORD temprc;

  *dirhandle_addr = 0xffff;

#if defined(DEBUG)
  printf("\nDOSFINDFIRST DIRH=%04Xh, FILE=%s...", *dirhandle_addr, path_addr);
#endif

  rc = DOSFINDFIRST((char far *)path_addr, (unsigned far *)dirhandle_addr,
	            attrib, (struct FileFindBuf far *)dta_address,
	            (WORD)(sizeof(struct FileFindBuf)),
                    (unsigned far *)&numentries, (DWORD)0);

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL, NAME=%s, ATTR=%04Xh, SIZE=%08lXh, DIRH=%04Xh",
            (*dta_address).file_name, (*dta_address).attributes,
            (*dta_address).file_size, *dirhandle_addr);
  else
    printf("ERROR, DIRH=%04Xh, RC=%04Xh", *dirhandle_addr, rc);
#endif

  if (rc != NOERROR)
  {
    temprc = rc;
    findclose(*dirhandle_addr);
    rc = temprc;
  }
}


/*********************************************************************/
/* Routine:   find_next                                              */
/*                                                                   */
/* Function:  Finds the next file fitting the given path.            */
/*********************************************************************/

void find_next(WORD dirhandle, struct FileFindBuf *dta_address)
{
  WORD temprc;
  WORD numentries = 1;

#if defined(DEBUG)
  printf("\nDOSFINDNEXT, DIRH=%04Xh...", dirhandle);
#endif

  rc = DOSFINDNEXT(dirhandle, (struct FileFindBuf far *)dta_address,
                   (WORD)(sizeof(struct FileFindBuf)+12),
                   (unsigned far *)&numentries);

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL, NAME=%s, DIRH=%04Xh",
            (*dta_address).file_name, dirhandle);
  else
    printf("ERROR, RC=%04Xh", rc);
#endif

  if (rc != NOERROR)
  {
    temprc = rc;
    findclose(dirhandle);
    rc = temprc;
  }
}


/*********************************************************************/
/* Routine:   find_close                                             */
/*                                                                   */
/* Function:  Closes the file opened by findfirst or findnext.       */
/*********************************************************************/

void findclose(WORD dirhandle)
{

#if defined(DEBUG)
  printf("\nDOSFINDCLOSE DIRH=%04Xh...", dirhandle);
#endif

  rc = DOSFINDCLOSE(dirhandle);
  dirhandles_open = FALSE;

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL");
  else
    printf("ERROR, RC=%04Xh", rc);
#endif
}


/*********************************************************************/
/* Routine:   delete                                                 */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

void delete(char *path_addr)
{
#if defined(DEBUG)
  printf("\nDOSDELETE FILE %s...", path_addr);
#endif

  rc = DOSDELETE((char far *)path_addr, (DWORD)0);

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL");
  else
    printf("ERROR, RC=%04Xh", rc);
#endif
}


/*********************************************************************/
/* Routine:   disk_free_space                                        */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

long disk_free_space()
{
  struct FSAllocate fsa;
  long odd_bytes, size;

#if defined(DEBUG)
  printf("\nDOSQFSINFO (0=def) DRIVE=%04Xh...", tgt_drive_letter - 'A' + 1);
#endif

  rc = DOSQFSINFO(
	     (WORD)tgt_drive_letter - 'A' + 1,   /* drive 0=def, 1=a... */
	     (WORD)1,                            /* level */
	     (char far *)&fsa,                   /* return info */
	     (WORD)(sizeof(struct FSAllocate))   /* size of return info buffer */
	   );

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL, FREESPACE=%08lXh",
             fsa.sec_per_unit * fsa.avail_units * fsa.bytes_sec);
  else
    printf("ERROR, RC=%04Xh", rc);
#endif

  if (rc == NOERROR)
  {
    odd_bytes = data_file_tot_len % (fsa.sec_per_unit * fsa.bytes_sec);
    if (odd_bytes)                     /* if file is not cluster aligned */
      size = fsa.sec_per_unit * (fsa.avail_units + 1) *
             fsa.bytes_sec - odd_bytes;
    else                               /* file is cluster aligned */
      size = fsa.sec_per_unit * fsa.avail_units * fsa.bytes_sec;
  }
  else                                 /* error occurred */
    size = 0;

  return (size);
}


/*********************************************************************/
/* Routine:   replace_volume_label                                   */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

void replace_volume_label(char *label_addr)
{
#if defined(DEBUG)
  printf("\nDOSSETFSINFO (0=def) DRIVE=%04Xh, LEN=%04Xh...",
           tgt_drive_letter - 'A' + 1, label_addr[0]);
#endif

  rc = DOSSETFSINFO(
            (WORD)tgt_drive_letter - 'A' + 1,    /* drive 0=def, 1=a... */
            (WORD)2,                             /* level */
            (char far *)label_addr,              /* buffer */
            (WORD)LABELLEN + 1                   /* buffer size */
       );

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL");
  else
    printf("ERROR, RC=%04Xh", rc);
#endif
}


/*********************************************************************/
/* Routine:   terminate                                              */
/*                                                                   */
/* Function:  Terminates the process and returns the errorlevel to   */
/*            DOS.                                                   */
/*********************************************************************/

#define TERMINATE  0x4C00
void terminate()
{
  if (append_indicator == DOS_APPEND)  /* if append /x was reset */
  {

#if defined(DEBUG)
    printf("\nINT2Fh,(SET APPEND) AX=%04Xh TO %04Xh...",
            SET_STATE, original_append_func);
#endif
    inregs.x.ax = SET_STATE;
    inregs.x.bx = original_append_func;
    int86(0x2f, &inregs, &outregs);
  }

  exit(return_code);
}


/*********************************************************************/
/* Routine:   ioctl                                                  */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

WORD ioctl(WORD devhandle)
{

#define ISDEVREMOVABL  0x20
#define CATEGORY       8          /* 1=serial,3=display,5=printer,8=disk */
#define GET_DRIVE_ATTR 9          /* ioctl cat for get device drive attr */
#define NETWORK_DRIVE  0x1000     /* returned by cat 9 for net devices */

  union REGS reg;
  BYTE data_area;

#if defined(DEBUG)
  printf("\nDOSDEVIOCTL HANDLE=%04Xh...", devhandle);
#endif

  if (devhandle > 0xffe4)
  {
    devhandle = -(int)devhandle;
    devhandle--;
  }
  reg.h.ah = 0x44;
  reg.h.al = CATEGORY;
  reg.x.bx = devhandle;
  intdos(&reg, &reg);
  /* check for redirected drive */
  if (reg.x.cflag)                /* cat 8 failed; probably remote drv */
  {
    reg.h.ah = 0x44;
    reg.h.al = GET_DRIVE_ATTR;
    reg.x.bx = devhandle;
    intdos(&reg, &reg);
    if (reg.x.cflag)              /* error now also; impossible - since */
      error_exit(INV_DRIVE);      /* we have already checked the drive */
    data_area = (char) ((reg.x.dx & NETWORK_DRIVE) ? 1 : 0); 
    /* if remote drive assume non-remov */
  }
  else
  {
    data_area = reg.h.al;
    rc = NOERROR;
  }
	
#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL, DATA_AREA(0=REMOVABLE) SET TO %02Xh", data_area);
  else
    printf("ERROR, RC=%04Xh", rc);
#endif

  return (data_area);
}


/*********************************************************************/
/* Routine:   alloc_seg                                              */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

void alloc_seg()
{
  union REGS iregs, oregs;

#if defined(DEBUG)
  printf("\nDOSALLOCSEG SIZE=%04Xh...", data_file_alloc_size);
#endif

  iregs.h.ah = 0x48;
  iregs.x.bx = data_file_alloc_size ? 
      ( data_file_alloc_size % 16 ? (data_file_alloc_size / 16) + 1 : 
        data_file_alloc_size / 16  ) : 0x4096;

  intdos(&iregs, &oregs);
  /* BUGBUG */
  if (oregs.x.cflag)
  {
    printf("COULD NOT ALLOCATE SEGMENT\n");
    exit(1);
  }
  rc = oregs.x.cflag;
  selector = (unsigned)oregs.x.ax;

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL, SELECTOR=%04Xh, SIZE=%04Xh",
             selector, data_file_alloc_size);
  else
    printf("ERROR, RC=%04Xh", rc);
#endif
}


/*********************************************************************/
/* Routine:   free_seg                                               */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

void free_seg(unsigned selector)
{

#define	invalid_selector 6

#if defined(DEBUG)
  printf("\nDOSFREESEG (%04Xh)...", selector);
#endif

  if (_dos_freemem(selector))
    rc = invalid_selector;
  else
    rc = NOERROR;

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL");
  else
    printf("ERROR, RC=%04Xh", rc);
#endif
}


/*********************************************************************/
/* Routine:   setsignal                                              */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

void setsignal(WORD action, WORD signum)
{
  DWORD old_sig_handler;
  WORD old_sig_action;

#if defined(DEBUG)
  printf("\nDOSSETSIGHANDLER ACTION=%04Xh,SIGNUM=%04Xh...", action, signum);
#endif

  rc = DOSSETSIGHANDLER(
             (void far *)control_break_handler,  /* signal handler address */
             (DWORD far *)&old_sig_handler,      /* addr of previous handler */
             (unsigned far *)&old_sig_action,    /* addr of previous action */
             action,                             /* request type (2=hook) */
             signum                              /* signal number */
       );

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL");
  else
    printf("ERROR, RC=%04Xh", rc);
#endif
}


/*********************************************************************/
/* Routine:   do_dos_error                                           */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

void do_dos_error(WORD flag)
{
#if defined(DEBUG)
  printf("\nDOSERROR, FLAG=%04Xh...", flag);
#endif

  rc =  DOSERROR(flag);

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL");
  else
    printf("ERROR, RC=%04Xh", rc);
#endif
}


/*********************************************************************/
/* Routine:   get_country_info                                       */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

void get_country_info()
{

#define USACOUNTRY 1
#define DEFAULT_COUNTRY 0
#define DEFAULT_CODEPAGE 0

  struct ctry_info_blk buff;
  struct countrycode ctrystuff;        /* added for CPDOS 1.1 */
  WORD data_len;

  ctrystuff.country = (WORD)DEFAULT_COUNTRY;
  ctrystuff.codepage = (WORD)DEFAULT_CODEPAGE;

#if defined(DEBUG)
  printf("\nDOSGETCTRYINFO COUNTRY=%04Xh...", ctrystuff.country);
#endif

  rc = DOSGETCTRYINFO(
            (unsigned)sizeof(struct ctry_info_blk),   /* length of return area */
            (struct countrycode far *)&ctrystuff,     /* country Code */
            (char far *)&buff,                        /* return area */
            (unsigned far *)&data_len                 /* len of returned area */
       );

#if defined(DEBUG)
  if (rc == NOERROR)
    printf("SUCCESSFUL");
  else
    printf("ERROR, RC=%04Xh", rc);
#endif

  if (rc == NOERROR)
  {
    ctry_date_fmt = buff.date_format;
    ctry_time_fmt = buff.time_format;
    ctry_date_sep = buff.date_separator;
    ctry_time_sep = buff.time_separator;

#if defined(DEBUG)
    printf("\nDATE SEPERATOR=%c", ctry_date_sep);
    printf("\nTIME SEPERATOR=%c", ctry_time_sep);
    printf("\nDATE FORMAT=%u", ctry_date_fmt);
    printf("\nTIME FORMAT=%u", ctry_time_fmt);
#endif

  }
}


/*********************************************************************/
/* Routine:   datetime                                               */
/*                                                                   */
/* Function:  Put date and time in logfile.                          */
/*********************************************************************/

void datetime()
{
  struct DateTime buff;
  char date[12];
  char time[12];
  char datetimestring[25];
  WORD written = 0;

#if defined(DEBUG)
  printf("\nDOSGETDATETIME...");
#endif

  _dos_getdate( (struct dosdate_t *) ((char *)&buff + 4) );
  _dos_gettime( (struct dostime_t *)&buff);
  *((char *)&buff + 10) = *((char *)&buff + 8);
  *((int *)&buff + 4) = 360;
  rc = NOERROR;

#if defined(DEBUG)
  if (rc == NOERROR)
     printf("SUCCESSFUL");
   else
     printf("ERROR, RC=%04Xh", rc);
#endif

  /* Build time string */
  sprintf(time, "%u%c%02u%c%02u", buff.hour, ctry_time_sep, buff.minutes,
                                  ctry_time_sep, buff.seconds);

  /* Build date string */
  switch (ctry_date_fmt)
  {
    case USA:
      sprintf(date, "%u%c%02u%c%04u", buff.month, ctry_date_sep, buff.day,
                                      ctry_date_sep, buff.year);
      break;

    case EUR:
      sprintf(date, "%u%c%02u%c%04u", buff.day, ctry_date_sep, buff.month,
                                      ctry_date_sep, buff.year);
      break;

    case JAP:
      sprintf(date, "%04u%c%02u%c%02u", buff.year, ctry_date_sep, buff.month,
                                        ctry_date_sep, buff.day);
      break;

    default:
      break;
  }

  datetimestring[0] = 0x0d;
  datetimestring[1] = 0x0a;
  sprintf(datetimestring + 2, "%s  %s", date, time);

  written = handle_write(handle_logfile, strlen(datetimestring),
                         (char far *)&datetimestring[0]);
  if ( written != strlen(datetimestring) || (rc != NOERROR) )
  {
    display_msg(LOGFILE_TARGET_FULL);
    do_logfile = FALSE;
  }
}



/*********************************************************************/
/* Routine:   extended_open                                          */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

#define EXTENDEDOPEN	0x6c00

WORD extended_open(WORD flag, WORD attr, char far *path_addr, WORD mode)
{
  union REGS inreg, outreg;

  ea_parmlist.ext_attr_addr = (DWORD)(char far *)&ext_attrib_buff[0];
  ea_parmlist.num_additional = 0;

#if defined(DEBUG)
  if (flag == CREATE_IT)
    printf("\nEXTENDED OPEN - CREATE, FILE %s...", path_addr);
  else
    printf("\nEXTENDED OPEN - OPEN, FILE %s...", path_addr);
#endif

  rc = NOERROR;
  inreg.x.ax = EXTENDEDOPEN;
  inreg.x.bx = mode;                             /* M010 */
  inreg.x.cx = attr;
  inreg.x.dx = flag + NO_CP_CHECK;
  inreg.x.si = (WORD)path_addr;

  inreg.x.di = (WORD)&ea_parmlist;

  intdos(&inreg, &outreg);
  if (outreg.x.cflag & CARRY)          /* if there was an error */
    rc = outreg.x.ax;                  /* set the return code */

#if defined(DEBUG)
  if (outreg.x.cflag & CARRY)
    printf("ERROR, RC=%04Xh", outreg.x.ax);
  else
    printf("SUCCESSFUL, HANDLE=%04Xh", outreg.x.ax);
#endif

  return (outreg.x.ax);
}


#ifdef DBCS
/*********************************************************************/
/* Routine:   IsDBCSLeadByte                                         */
/*                                                                   */
/* Function:  Test if the character is a DBCS lead byte.             */
/*                                                                   */
/*      input:  c = character to test                                */
/*      output: TRUE if leadbyte                                     */
/*********************************************************************/

int IsDBCSLeadByte(unsigned char c)
{
  static unsigned char far *DBCSLeadByteTable = NULL;
  union REGS inregs, outregs;
  struct SREGS segregs;
  unsigned char far *p;
  int stat;

  if (DBCSLeadByteTable == NULL)
  {
    inregs.x.ax = 0x6300;              /* get DBCS lead byte table */
    intdosx(&inregs, &outregs, &segregs);
    FP_OFF(DBCSLeadByteTable) = outregs.x.si;
    FP_SEG(DBCSLeadByteTable) = segregs.ds;
  }

  p = DBCSLeadByteTable;
  stat = FALSE;
  while (p[0] || p[1])
  {
    if (c >= p[0] && c <= p[1])
    {
      stat = TRUE;
      break;
    }
    p += 2;
  }

  /*
   *  Since the DOSCALL for 'xlat' & 'extended_open' needs
   *  to set ES & DS register, but they are using 'intdos'
   *  instead of 'intdosx', so we must set ES here.
   */
  p = (char far *)src_drive_path;      /* get DS value */
  if (*p) {}                           /* ES <- DS */

  return (stat);
}


/*********************************************************************/
/* Routine:   CheckDBCSTailByte                                      */
/*                                                                   */
/* Function:  Check if the character point is at the tail byte.      */
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
  return ((point - p) & 1 ? TRUE : FALSE);
}
#endif


/*********************************************************************/
/* Routine:   check_asj                                              */
/*                                                                   */
/* Function:                                                         */
/*********************************************************************/

void check_asj()
{
  struct SREGS sreg;
  union REGS reg;
  char buf1[128];
  char buf2[128];

  /*
   *  Make sure source is not Assigned, Substed, or Joined and 
   *  therefore equal to target
   */
  segread(&sreg);
  reg.x.si = (unsigned)buf1;
  reg.x.di = (unsigned)buf2;
  reg.x.ax = 0x6000;
  buf1[0] = src_drive_letter;
  strcpy(buf1 + 1, ":\\");
  intdosx(&reg, &reg, &sreg);
  if (*buf2 == tgt_drive_letter)  
    error_exit(SRCISTGT);

  /* Make sure target is not A/S/J and therefore equal to source */
  segread(&sreg);  
  reg.x.si = (unsigned)buf1; 
  reg.x.di = (unsigned)buf2; 
  reg.x.ax = 0x6000;
  buf1[0] = tgt_drive_letter; 
  intdosx(&reg, &reg, &sreg); 
  if (*buf2 == src_drive_letter)  
    error_exit(TGTISSRC); 
}

