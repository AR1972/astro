;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/**************************************************************************/
/*                                                                        */
/*  UTILITY NAME:      Replace                                            */
/*                                                                        */
/*  SOURCE FILE NAME:  Replace.C                                          */
/*                                                                        */
/*  STATUS:            Replace Utility, DOS Version 5.00                  */
/*                                                                        */
/*  FUNCTIONAL DESCRIPTION:  REPLACE is an external DOS utility that      */
/*                           allows a user to selectively replace         */
/*                           files on the target with files of the        */
/*                           same name from the source.  The user can     */
/*                           also selectively add files from the source   */
/*                           to the target.                               */
/*                                                                        */
/*  SYNTAX:            [d:][path]REPLACE[d:][path]filename[.ext]          */
/*                     [d:][path] [/A][/P][/R][/S][/U][/W]                */
/*            where:                                                      */
/*                     [d:][path] before REPLACE specifies the drive      */
/*                     and path that contains the REPLACE command file,   */
/*                     if it is not the current directory of the          */
/*                     default drive.                                     */
/*                                                                        */
/*                     [d:][path]filename[.ext] specifies the names of    */
/*                     the files on the source that are to be replaced    */
/*                     on the target or added to the target.  The file    */
/*                     name can contain global file name characters.      */
/*                                                                        */
/*                     [d:][path] specifies the target drive and          */
/*                     directory.  The files in this directory are        */
/*                     the ones that are to be replaced, if /A is         */
/*                     specified the source files are copied to this      */
/*                     directory.  The default is the directory on the    */
/*                     current drive.                                     */
/*                                                                        */
/*                     /A copies all files specified by the source that   */
/*                        do not exist on the target.                     */
/*                                                                        */
/*                     /P prompts as each file is encountered on the tar- */
/*                        get, allowing selective replacing or adding.    */
/*                                                                        */
/*                     /R replaces files that are read-only on the target.*/
/*                                                                        */
/*                     /S searches all directories of the target for      */
/*                        files matching the source file name.            */
/*                                                                        */
/*                     /U replaces updated date/time attribute source     */
/*                        files to the target.                            */
/*                                                                        */
/*                     /W waits for you to insert a diskette before be-   */
/*                        ginning to search for source files.             */
/*                                                                        */
/*         ** NOTE **  /A + /S and A/ + /U cannot be used together.       */
/*                                                                        */
/*  LINKS:                                                                */
/*    COMSUBS.LIB - DOS DBCS function calls                               */
/*    MAPPER.LIB  - DOS function calls                                    */
/*    SLIBC3.LIB  - C library functions                                   */
/*    _MSGRET.SAL - Assembler interface for common DOS message services   */
/*    _PARSE.SAL  - Assembler interface for common DOS parser             */
/*    _REPLACE.SAL- Assembler control break and critical error handlers   */
/*                                                                        */
/*  ERROR HANDLING:    Error message is displayed and utility is          */
/*                     then terminated (with appropriate error level).    */
/*                                                                        */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *  */

#define LINT_ARGS
#include <process.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "comsub.h"                    /* DBCS functions */
#include "dos.h"                       /* Used for the REGS union */
#include "replacep.h"                  /* Parser structures */
#include "io.h"
#include "sys\types.h"
#include "sys\stat.h"
#include "fcntl.h"
#include "io.h"
#include "replace.h"


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   main                                               */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Get addressability to msgs through SYSLOADMSG   */
/*                        Parse the command line by calling SYSPARSE      */
/*                        Allocate the data buffer to be used for copy    */
/*                        Make a fully qualified source path              */
/*                        Append current directory string                 */
/*                        Create a list of files to be replaced           */
/*                        Read and write files                            */
/*                        De-allocate data buffer                         */
/*                        Print messages by calling SYSDISPMSG            */
/*                                                                        */
/*  INPUT:             Command line arguments                             */
/*                                                                        */
/*  EXIT-NORMAL:       Information message displayed.                     */
/*                                                                        */
/*  EXIT-ERROR:        Error message displayed, appendx restored,         */
/*                     errorlevel set.                                    */
/*                                                                        */
/*  EXTERNAL ROUTINES:    SYSLOADMSG                                      */
/*                        SYSDISPMSG                                      */
/*                        SYSPARSE                                        */
/*                        crit_err_handler                                */
/*                        ctl_brk_handler                                 */
/*                                                                        */
/**************************************************************************/

void main(argc, argv)
int  argc ;
char *argv[] ;
{
  /*  Forward declarations */
  char *com_strchr();                       /* To search for DBCS "\\" */
  unsigned char *com_strrchr();             /* To search for DBCS "\\" */
  
  /*  Local variables */
  int index;                                /* Forming string for parser */


  /* Begin Here */
  load_msg();                               /* Point to msgs & chk DOS ver */
  for (index = 1; index <= argc; index++)   /* Form string for parser */
  {
    strcat(source,argv[index]);             /* Add the argument */
    strcat(source," ");                     /* Separate with a space */
  }
  parser_prep(source);                      /* Initialization for parser */

  ParseIt();                                /* Parse the command line */

  /*
   *  Allocate the data buffer to be used during the copy operations
   */
  length = 0x1000;
  rstatus = dallocate(length);                   /* Allocate buffer */
  if (rstatus == INSUFFMEM)                      /* Not enough mem? */
  {                                              /* then alloc what's available */
    length = outregs.x.bx;                       
    rstatus = dallocate(length);
  }

  if (rstatus != 0)                              /* If can't alloc at all */
  {
    display_msg(MSG_NOMEM, NULLARG);             /* no space for copies */
    dexit(ERRLEVEL8);                            
  }

  segment = outregs.x.ax;
  length  = (length << 4);                       /* Convert to bytes */
  if (length == 0) length = 0xffff;

  /*
   *  If the wait switch was on the command line, wait to continue
   */
  if (waiting)
  {
    display_msg(MSG_START, NULLARG);             /* Press any key... */
    inregs.x.ax = 0x0C08;
    intdos(&inregs,&outregs);
    rstatus = (outregs.x.cflag & CARRY);
    if ( (rstatus == NOERROR) && ((outregs.x.ax & 0x00ff) == 0) )
    {
      inregs.x.ax = 0x0100;
      intdos(&inregs,&outregs);
      rstatus = (outregs.x.cflag & CARRY);
    }
  }
  
  GetFQPathNames(source,target);

  dfree(segment);

  switch(rstatus)
  {
    case 0  : break;
    case 2  : display_msg(MSG_ERRFNF,errfname);       /* file not found */
	      break;
    case 3  : display_msg(MSG_ERRPNF,errfname);       /* path not found */
	      break;
    case 5  : display_msg(MSG_ERRACCD,errfname);      /* access denied */
	      break;
    case 15 : display_msg(MSG_ERRDRV,errfname);       /* invalid drive */
	      break;
    default : dexit(ERRLEVEL1);                       
	      break;
  }

  if (add)
    if (counted == 0)
      display_msg(MSG_NONEADDE, NULLARG);             /* no files added */
    else
      display_msg(MSG_SOMEADDE,(char *)&counted);     /* %1 files added */
  else
    if (counted == 0)
      display_msg(MSG_NONEREPL, NULLARG);             /* no files replaced */
    else
      display_msg(MSG_SOMEREPL,(char *)&counted);     /* %1 files replaced */

  restore();                                          /* Cleanup before exit */
  dexit(rstatus);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   ParseIt                                            */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Parse the command line entered by the user.     */
/*                                                                        */
/*  INPUT:             none                                               */
/*                                                                        */
/*  OUTPUT:            command line is parsed                             */
/*                                                                        */
/**************************************************************************/

void ParseIt()
{
  char switch_buffer[3];               /* Gets switch from parser */
  char far * fptr;                     /* Pts to parser's buf for flspc */
  int fchar            = 0;            /* Index into p_sfilespec */
  int index;                           /* Forming string for parser */
  int i;                               /* Loop counter */
  int more_to_parse    = TRUE;         /* While parsing cmdline */
  unsigned have_source = FALSE;        /* Flag */
  unsigned have_target = FALSE;        /* Flag */

  
  while (more_to_parse)                          /* Test loop flag */
  {
    index = 0;                                   /* Init array index */
    parse(&inregs,&outregs);                     /* Call the parser */
    if (outregs.x.ax == P_No_Error)              /* If no error */
    {
      /* if result is filespec & we don't have the source */
      if ((outregs.x.dx == (unsigned short)&rslt1) && !(have_source))
      {
	/* get the filespec from parser */
	for (fptr = rslt1.fp_result_buff; (char)*fptr != NULL; fptr++)
	{
	  p_sfilespec[fchar] = (char)*fptr;      /* get the character */
	  fchar++;                               /* Move the char ptr */
	}
	strcpy(fix_es_reg,NULL);                 /* (Set es reg correct) */
	have_source = TRUE;                      /* Set the flag */
	fchar       = 0;                         /* Reset char ptr */
      }
      else
      {
	/* if result is filespec & we do have the source */
	if ((outregs.x.dx == (unsigned short)&rslt1) && (have_source))
	{
	 /* get the filespec from parser */
	  for (fptr = rslt1.fp_result_buff; (char)*fptr != NULL; fptr++)
	  {
	    p_path[fchar] = (char)*fptr;         /* get the character */
	    fchar++;                             /* Move the char ptr */
	  }
	  strcpy(fix_es_reg,NULL);               /* (Set es reg correct) */
	  have_target = TRUE;                    /* Set the flag */
        }
	else
	{
	  /* Copy whatever parser just parsed */
	  for (inregs.x.si; inregs.x.si < outregs.x.si; inregs.x.si++)
	  {
	    cmdln_switch[index] = *(char *)inregs.x.si;
	    index++;
	  }
	  /* Copy switch into buf and verify which switch */
	  strcpy(switch_buffer,(char *) rslt2.P_SYNONYM_Ptr);
	  switch (switch_buffer[1])
	  {                                             
	    case '?' :
                for (i = MSG_OPTIONS_FIRST; i <= MSG_OPTIONS_LAST; i++)
                  display_msg(i, (char *)0);
                restore();
                dexit(ERRLEVEL0);                /* Terminate utility */
	    case 'A' :
                if (!add)                        /* /A switch */
                  add = TRUE;
                else                             /* It's a dup switch */
                  display_exit(MSG_BADSWTCH,cmdln_switch,ERRLEVEL11);
                break;
	    case 'P' :
                if (!prompt)                     /* /P switch */
		  prompt = TRUE;
		else                             /* It's a dup switch */
		  display_exit(MSG_BADSWTCH,cmdln_switch,ERRLEVEL11);
		break;                      
	    case 'R' :
                if (!readonly)                   /* /R switch */
		  readonly = TRUE;          
		else                             /* It's a dup switch */
		  display_exit(MSG_BADSWTCH,cmdln_switch,ERRLEVEL11);
		break;                      
	    case 'S' :
                if (!descending)                 /* /S switch */
		  descending = TRUE;        
		else                             /* It's a dup switch */
		  display_exit(MSG_BADSWTCH,cmdln_switch,ERRLEVEL11);
		break;                      
	    case 'U' :
                if (!update)                     /* /U switch */
		  update = TRUE;            
		else                             /* It's a dup switch */
		  display_exit(MSG_BADSWTCH,cmdln_switch,ERRLEVEL11);
		break;                      
	    case 'W' :
                if (!waiting)                    /* /W switch */
		  waiting = TRUE;
                else                             /* It's a dup switch */
		  display_exit(MSG_BADSWTCH,cmdln_switch,ERRLEVEL11);
		break;
	    default :
                display_exit(MSG_BADSWTCH,cmdln_switch,ERRLEVEL11); 
		break;
	  }
	}
      }
    }
    else
    {
      if (outregs.x.ax != P_RC_EOL)              /* Is the parser */
      {
        /* Copy whatever parser just parsed */
	for (inregs.x.si; inregs.x.si < outregs.x.si; inregs.x.si++)
	{
	  cmdln_invalid[index] = *(char *)inregs.x.si;
	  index++;
	}
	switch (outregs.x.ax)                    /* returning an error? */
	{
	  case P_Too_Many :
              /* Too many parms, more_to_parse = FALSE */
	      display_exit(MSG_XTRAPARM,cmdln_invalid,ERRLEVEL11);
	      break;
	  case P_Syntax :
              /* Bad syntax, more_to_parse = FALSE */
              display_exit(MSG_BADPARM,cmdln_invalid,ERRLEVEL11);
	      break;
	  case P_Not_In_SW :
              /* Invalid switch, more_to_parse = FALSE */
              display_exit(MSG_BADSWTCH,cmdln_invalid,ERRLEVEL11);
	      break;
	  case P_Op_Missing :
              /* Source required, more_to_parse = FALSE */
              display_msg(MSG_NOSOURCE, NULLARG);
	      dexit(ERRLEVEL11);
	      break;
	}
      }
      else
	more_to_parse = FALSE;              /* End of the cmdline */
    }
    inregs.x.cx = outregs.x.cx;             /* Move the count */
    inregs.x.si = outregs.x.si;             /* Move the pointer */
  }

  /*
   *  Verify the correctness of the parameters
   */
  if ((add && descending) || (add && update))    /* A+S or A+U */
  {
    display_msg(MSG_INCOMPAT, NULLARG);          /* Incompatible switches */
    dexit(ERRLEVEL11);
  }
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   GetFQPathNames                                     */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Get fully qualified pathnames for the source    */
/*                        and the target.                                 */
/*                                                                        */
/*  INPUT:             target    (contains current directory)             */
/*                     save      (pathname to fix)                        */
/*                                                                        */
/*  OUTPUT:            target    (fully qualified pathname)               */
/*                                                                        */
/**************************************************************************/

void GetFQPathNames(source,target)
char *source;                               /* source buffer */
char *target;                               /* target buffer */
{
  char save[MAXMINUS1];                     /* temporary storage buffer */
  int first_time_thru_loop    = TRUE;       /* flag, skip code if > 256 files */
  int need_to_reset_filecount = FALSE;      /* flag, get to 0 element of array */
  int search_more_files       = TRUE;       /* flag, loop > 256 files */
  unsigned filecount          = 0;          /* number of files */
  struct filedata files[500];               /* 256 is true limit, but */
				            /* 500 to accomodate segment wrap */

  /*
   *  Make a fully qualified source path
   */
  strcpy(source,p_sfilespec);               /* Copy filespec recvd from parser */
  strcpy(save,source);
  strcpy(errfname,source);
  if (source[1] != ':')                     /* If no drive letter entered */
    GetCurrentDrive(source,rstatus,1,save);           /* macro call */

  /*
   *  Append current directory string
   */
  strcpy(errfname,source);
  if (source[2] != '\\')                    /* If not path from root */
  {
    strcpy(save,&source[2]);
    strcpy(&source[3],save);
    GetCurrentDirectory(source,rstatus);              /* macro call */
      
    /* If we got it, add it to user entered path */
    if (rstatus == NOERROR)
    {
      AppendBackslash(source);                        /* macro call */
      strcat(source,save);
    }
  }

  rstatus = check_appendx_install();             /* Check append/x install */
  if (rstatus != NOERROR)                        /* If append/x is installed */
  {
    append_installed = TRUE;
    x_status = check_appendx();                  /* Get the status */
    set_appendx(x_status & INACTIVE);            /* Set it inactive */
  }

  setup_ctl_brk();                               /* Ctl brk vec now pts here */
  setup_crit_err();                              /* Crit err vec now pts here */

  /*
   *  Create a list of the files that we might just replace
   */
#if SELECT_SUPPORT
  check_select();                                /* See if invoked by SELECT */
#endif

  /* Find the first file */
  strcpy(errfname,source);
  rstatus = dsearchf(source,&files[filecount],SATTRIB);

  while (search_more_files)                      /* Search & replace/add */
  {
    while ( (filecount < MAXMINUS1) && (rstatus == NOERROR) )
    {
      filecount++;
      /* Because of prev structure, only way to get to 0 element */
      if (need_to_reset_filecount)
      {
	filecount = 0;
	need_to_reset_filecount = FALSE;
      }
      rstatus = dsearchn(&files[filecount]);
    }

    if (rstatus == NOMOREFILES)
    {
      rstatus = NOERROR;
      search_more_files = FALSE;                 /* Set for loop test */
      if (filecount == 1)                        /* Find the true filecount */
        only_one_valid_file = TRUE;              
      if (filecount > 1)                         /* Last file is no good */
        filecount--;                             /* so back counter up */
    }

    /* Bypass this if done already */
    if ((first_time_thru_loop) && (rstatus == NOERROR) )
    {
      first_time_thru_loop = FALSE;              /* > 256 files to replace */
      if (filecount == 0)
	display_exit(MSG_NONFOUND,source,ERRLEVEL2);

      if (rstatus == NOERROR)
      {
	/*
         *  Fixup the source directory path so that it is useable.
         */
        FixSourcePath(source);
        
	/*
         *  Fixup the target path.
         */
        FixTargetPath(target);
      }
    }

    if (rstatus == NOERROR)
    {
      if (!add)
        rstatus = dodir(source,target,files,filecount);
      else
        rstatus = doadd(source,target,files,filecount);
    }

    filecount = 0;                               /* Reset for next loop */
    need_to_reset_filecount = TRUE;              /* Get to 0 element */

    if (rstatus != NOERROR)                      /* If there was an error */
      search_more_files = FALSE;                 /*   somewhere, drop out */
  }
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   FixSourcePath                                      */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Fixes the source path so that it is useable.    */
/*                                                                        */
/*  INPUT:             source 	(contains pathname to fix)                */
/*                                                                        */
/*  OUTPUT:            source contains true pathname                      */
/*                                                                        */
/**************************************************************************/

void FixSourcePath(source)
char *source;
{
  int i;                              /* loop counter */
  int backslash_char = FALSE;         /* flag, DBCS backslash */

  
  for (i = strlen(source) - 1;
       (i >= 0) && (!backslash_char) && (source[i] != ':'); i--)
  {
    if ((source[i] == '\\') && (i != 0))
    {
      dbcs_search[0] = source[i-1];        /* Copy char to srch for DBCS */
      dbcs_search[1] = source[i];          /* Copy char to srch for DBCS */
      if (com_strchr(dbcs_search,'\\') != NULL)
      {
        /* If there is a pointer, then backslash exists */
        backslash_char = TRUE;
        i++;
      }
    }
    else
    {
      if ((source[i] == '\\') && (i == 0))
      {
        backslash_char = TRUE;
        i++;
      }
    }
  }
  if (i <= 0)
  {
    i = 0;
    source[0] = NULL;
  }
  dbcs_search[0] = source[i-1];
  dbcs_search[1] = source[i];
  if ( ((com_strchr(dbcs_search,'\\'))!= NULL) || (source[i] == ':') )
    source[i+1] = NULL;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   FixTargetPath                                      */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Fixes the target path so that it contains a     */
/*                        fully qualified pathname.                       */
/*                                                                        */
/*  INPUT:             target 	(contains pathname to fix)                */
/*                                                                        */
/*  OUTPUT:            target contains true pathname                      */
/*                                                                        */
/**************************************************************************/

void FixTargetPath(target)
char *target;                              /* target pathname */
{
  char save[MAXMINUS1];                     /* temporary storage buffer */


  strcpy(target,p_path);                   /* Copy path recvd from parser */
  if (target[0] == NULL)
    GetCurrentDrive(target,rstatus,0,NULL);     /* macro call */

  strcpy(errfname,target);
  if ( (strlen(target) == 2) && (target[1] == ':') && (rstatus == NOERROR) )
    GetCurrentDirectory(target,rstatus);        /* macro call */

  strcpy(save,target);
  strcpy(errfname,target);
  if (target[1] != ':')
    GetCurrentDrive(target,rstatus,1,save);     /* macro call */

  strcpy(errfname,target);
  if (target[2] != '\\')
  {
    strcpy(save,&target[2]);
    strcpy(&target[3],save);
    GetCurrentDirectory(target,rstatus);        /* macro call */
    if (rstatus == NOERROR)
    {
      AppendBackslash(target);                  /* macro call */
      /* If target is not cur dir, then append subdir to path */
      if (save[0] != '.')
        strcat(target,save);
      else
      {
        /* Handle .\ and ..\ pathnames */
        FixPath(target,save);
      }
    }
  }
  strcpy(errfname,target);
  /* Copy chars to search for DBCS */
  dbcs_search[0] = target[strlen(target)-2];
  dbcs_search[1] = target[strlen(target)-1];
  if ( ((com_strchr(dbcs_search,'\\')) != &dbcs_search[1]) &&
       (rstatus == NOERROR) )
    strcat(target,"\\");
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   FixPath                                            */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Handle pathnames that contain .\ and ..\        */
/*                                                                        */
/*  INPUT:             target    (contains current directory)             */
/*                     save      (pathname to fix)                        */
/*                                                                        */
/*  OUTPUT:            target    (fully qualified pathname)               */
/*                                                                        */
/**************************************************************************/

void FixPath(target,save)
char *target;
char *save;

{
  int catlen;                          /* number of bytes to concatenate */
  char *posptr;                        /* pointer to current position */
  char *endptr;                        /* pointer to end of string */
  char *nextptr;                       /* pointer to next position in save */


  posptr = save;
  endptr = posptr + strlen(save);
  
  /* Loop until reach the end of the string */
  while (posptr < endptr)
  {
    if (*posptr == '.')                /* relative position */
    {
      if (*(posptr+1) == '.')          /* previous directory */
      {
        /*
         *  If target is parent dir, then delete end backslash and
         *  delete cur dir name from path
         */
        target[strlen(target) - 1] = NULL;
        *((unsigned char *)com_strrchr(target,'\\') + 1) = NULL;
        posptr += 3;
      }
      else                             /* current directory */
      {
        /* Target is current directory, so simply advance pointer */
        posptr += 2;
      }  
    }
    else                               /* directory name */
    {
      /* Have a directory name, so copy it over to target string */
      if ((nextptr = com_strchr(posptr,'.')) == NULL)
        nextptr = endptr;
      catlen = (int)nextptr - (int)posptr;
      strncat(target,posptr,catlen);
      posptr += catlen;
    }
  }
  AppendBackslash(target);                            /* macro call */
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dodir                                              */
/*                                                                        */
/**************************************************************************/

unsigned dodir(source,target,files,filecount)
char     *source;
char     *target;
struct   filedata *files;
unsigned filecount;
{
  char     dta_area[128];
  char     subdirectory[MAX];
  int      index;
  unsigned status;
  struct   filedata file;

  dta_save(dta_area,128);
  strcpy(subdirectory,target);
  strcat(subdirectory,"*.*");
  status = dsearchf(subdirectory,&file,TATTRIB);
  while (status == NOERROR)
  {
    if ( ((file.attribute & SUBDIR) != 0) &&
       (descending) && (file.name[0] != '.') )
    {
      strcpy(subdirectory,target);
      strcat(subdirectory,file.name);
      strcat(subdirectory,"\\");
      status = dodir(source,subdirectory,files,filecount);   /* Call self again */
      strcpy(subdirectory,target);
      strcat(subdirectory,"*.*");
    }
    else
    {
      /* If there is a file and it is not a subdirectory name */
      index = findfile(files,&file,filecount);
      if ( (index >= 0) && ((file.attribute & SUBDIR) == 0) )
      {
	/* If update switch set, check date & time */
	if (update)
        {
	  /*
	   *  If src.date < tgt.date OR Src.dt == tgt.dt AND
	   *     src.tm <= tgt.tm - do nothing
	   */
	  if ((files[index].date < file.date)  ||
	     ((files[index].date == file.date) &&
	     (files[index].time <= file.time)));
	  /* Else src is newer - do cpy */
	  else
	    status = docopy(source,target,&file,files[index].time,
			    files[index].date);
        }
	else                                /* Update switch was not set */
	  status = docopy(source,target,&file,files[index].time,files[index].date);
      }
    }
    if (status == NOERROR)
      status = dsearchn(&file);
  }

  dta_restore(dta_area,128);
  if (status == NOMOREFILES)
    status = NOERROR;
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   doadd                                              */
/*                                                                        */
/**************************************************************************/

unsigned doadd(source,target,files,filecount) 
char     *source;
char     *target;
struct   filedata *files;
unsigned filecount;
{
  char     path[MAX];
  int      index;
  unsigned status = NOERROR;
  struct   filedata *f;
  struct   filedata dummy;

  if (only_one_valid_file)                  /* Eliminate extra loop */
    filecount--;                                                       

  for (index = 0; (index <= filecount) && (status == NOERROR); index++)
  {
    f = files+index;
    strcpy(path,target);
    strcat(path,f->name);
    status = dsearchf(path,&dummy,TATTRIB);
    
    /* Check for null filename or Process single file */
    if (((status == NOMOREFILES) && (f->name[0] != NULL)) ||
       ((index==filecount)&&(f->name[0]!=NULL)&&(status==NOMOREFILES)))
    {

#if SELECT_SUPPORT
      if (detect_select_flag)                    /* If SELECT invoked */
      {
	status = check_select_list(f);           /* Check SELECT filelist */
	
        /* If okay to add, call copy opertion */    
	if (status)
	  status = docopy(source,target,f,f->time,f->date);
      }
      else
#endif
	status = docopy(source,target,f,f->time,f->date);
    }
    else
      status = NOERROR;                          /* Skip this null file */
  }
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   findfile                                           */
/*                                                                        */
/**************************************************************************/

unsigned findfile(files,file,filecount)
struct   filedata *files;
struct   filedata *file;
unsigned filecount;
{
  int i;
  unsigned status;

  if (only_one_valid_file)                  /* Eliminate extra loop */
    filecount--;

  for (i = 0; i <= filecount; i++)
  {
    if (same(files->name,file->name))
    {

#if SELECT_SUPPORT
      if (detect_select_flag)               /* If invoked by SELECT */
      {
	status = check_select_list(file);   /* Check SELECT filelist */
	if (status)                         /* If ok to copy */
	  return(i);                        /* Return index to filename */
      }
      else
#endif

        return(i);
    }
    files++;
  }
  return(ERRLEVELNEG1);                  
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   docopy                                             */
/*                                                                        */
/**************************************************************************/

unsigned docopy(sdir,tdir,file,time,date)
char     *sdir;
char     *tdir;
struct   filedata *file;
unsigned time;
unsigned date;
{
  char     source[MAX];
  char     target[MAX];
  unsigned source_handle;
  unsigned status;
  unsigned target_handle;
  unsigned try;
  int      src, dest;                  /* file handles for size check */
  long     src_len, dest_len;          /* file lengths for size check */
  long     freespace;                  /* free space on disk */
  unsigned drive;                      /* target drive number */
  struct diskfree_t space;             /* structure for get disk free call */

  /* create the path names and check for equivalence */

  strcpy(source,sdir);
  strcat(source,file->name);
  strcpy(target,tdir);
  strcat(target,file->name);

  status = strcmp(source,target);
  if (status == NOERROR)
  {
    display_msg(MSG_WARNSAME,source);       /* File cannot be copied... */
    return(status);
  }

  /* We can replace!  if prompting, check to see */
  /* if this file is to be replaced or not       */

  while ( (prompt) && (not_valid_input) )   /* Flag set in dcompare */
  {
    if (add)
      display_msg(MSG_QADD,target);         /* Add? filename */
    else
      display_msg(MSG_QREPLACE,target);     /* Replace? filename */
    status = dcompare();                    
  }
  not_valid_input = TRUE;                   /* Prepare for next file in loop */
  if (status == 2)
    return(ERRLEVEL0);                      

  /* indicate what we are replacing */

  if (add)
    display_msg(MSG_ADDING,target);         /* Adding filename */
  else
  {
    display_msg(MSG_REPLACIN,target);       /* Replacing filename */

    /* check for available space if replacing */
    src = open(source,O_RDONLY);
    dest = open(target,O_RDONLY);
    if (src != -1 && dest != -1)       /* found both files, check sizes */
    {
      src_len = filelength(src);
      dest_len = filelength(dest);
      drive = target[0] - 'A' + 1;
      _dos_getdiskfree(drive, &space);

      /* compute free space on disk */
      freespace = space.bytes_per_sector * space.sectors_per_cluster *
                  (long)space.avail_clusters;

      /* check free space against space needed */
      if (src_len - dest_len > freespace)
      {
        /* print error and exit if not enough space */
        strcpy(errline,target);
        display_msg(MSG_ERRDSKF,errline);
        target_full = TRUE;
        close(src);
        close(dest);
        return(NOERROR);                /* return no error anyway */
      }
    }
    close(src);
    close(dest);
  }

  /* open the input file */

  status = dopen(source);                   /* Extended open */
  if (status != 0)
  {
    strcpy(errfname,source);
    return(status);
  }

  source_handle = outregs.x.ax; 

  /* create the output file */
  /* if we are to overwrite READONLY files, set the mode so we can */

  if (!add)
  {
    inregs.x.cx = 0;
    status = dchmod(target,0);
    if (status != 0)
    {
      strcpy(errfname,target);
      dclose(source_handle);
      return(status);
    }
    file->attribute = (char)outregs.x.cx; 
    if (readonly)
      outregs.x.cx = outregs.x.cx & 0xfffe;
    if (file->attribute != (char)outregs.x.cx)
    {
      status = dchmod(target,1);
      if (status != 0)
      {
	strcpy(errfname,target);
	dclose(source_handle);
	return(status);
      }
    }
  }

  status = dcreate(target,-1);              /* Create the target file */

  strcpy(filename,target);                  /* Note that existing file*/
  if (status != 0)                          /*  will be deleted       */
  {
    strcpy(errfname,target);
    dclose(source_handle);
    return(status);
  }

  target_handle = outregs.x.ax;

  /* now, copy all of the data from the in file to the out file */

  try = length;
  while ( (try == length) && (status == NOERROR) )
  {
    status = dread(source_handle,segment,0,try);
    if (status == NOERROR)
    {
      try    = outregs.x.ax;
      status = dwrite(target_handle,segment,0,try);
      if (disk_full)                        /* RW If the target disk fills up */
      {
	strcpy(errline,filename);           /* save target filename */
	dclose(target_handle);              /* RW Close target file */
	ddelete(filename);                  /* RW Then delete target file */
	display_msg(MSG_ERRDSKF,errline);   /* Error disk full */
	status = TARGETFULL;                /* RW Tell me too */
      }
    }
  }

  if (status == NOERROR)
  {
    inregs.x.ax = 0x5701;                   /* Set files date and time */
    inregs.x.bx = target_handle;            
    inregs.x.cx = time;                     
    inregs.x.dx = date;                     
    intdos(&inregs,&outregs);               
    if (outregs.x.cflag & CARRY)            /* If the carry flag is set */
      status = outregs.x.ax;                /*   get returned error */
    else                                    /* else                 */
      status = (outregs.x.cflag & CARRY);   /*   set status to NOERROR */
  }

  if (status == NOERROR)
  {
    status = dclose(target_handle);         /* Close target file */
    if (status != NOERROR)
      strcpy(errfname,target);
    else
    {
      if ((file->attribute & ARCHIVE) != ARCHIVE)
	file->attribute += ARCHIVE;         /* Set archive bit ON */
      inregs.x.cx = file->attribute;
      if (!add)
	status = dchmod(target,1);          /* Reset attributes on target */
      if (status != NOERROR)
	strcpy(errfname,target);
      counted++;                            /* Increment num files processed */
    }
  }

  if (disk_full)                            /* RW If the target disk got full */
  {
    status      = NOERROR;                  /* RW Then we've done all we can do */
    disk_full   = FALSE;                    /* RW So forget about it */
    target_full = TRUE;                     
  }

  dclose(source_handle);
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   same                                               */
/*                                                                        */
/**************************************************************************/

int same(s,t)                               
char *s;
char *t;
{
  while ( (*s != NULL) && (*t != NULL) )    
  {                                         
    if ( *s != *t )                         
      return(FALSE);                        /* Removed "casemap" */
    s++;                                    
    t++;                                    
  }                                         
  if ( *s != *t )
    return(FALSE);                          /* Removed "casemap" */
  return(TRUE);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dallocate                                          */
/*                                                                        */
/**************************************************************************/

unsigned dallocate(s)
unsigned s;
{
  unsigned status;

  inregs.x.bx = s;                          /* Num of paragraphs requested */
  inregs.x.ax = 0x4800;                     /* Int21 - allocate memory */
  intdos(&inregs,&outregs);
  if (outregs.x.cflag & CARRY)              /* If the carry flag is set */
    status = outregs.x.ax;                  /*   get returned error */
  else                                      /* else */
    status = (outregs.x.cflag & CARRY);     /*   set status to NOERROR */
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dfree                                              */
/*                                                                        */
/**************************************************************************/

unsigned dfree(s)
unsigned s;
{
  unsigned status;

  segregs.es  = s;                          
  inregs.x.ax = 0x4900;                     
  intdosx(&inregs,&outregs,&segregs);       
  if (outregs.x.cflag & CARRY)              /* If the carry flag is set */
    status = outregs.x.ax;                  /*   get returned error */
  else                                      /* else                 */
    status = (outregs.x.cflag & CARRY);     /*   set status to NOERROR */
  strcpy(fix_es_reg,NULL);
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dcreate                                            */
/*                                                                        */
/**************************************************************************/

unsigned dcreate(n,parm_value)
char     *n;
unsigned parm_value;
{
  unsigned status;

  inregs.x.ax = 0x6c00;                     /* Extended Create */
  inregs.x.bx = 8321;                       /* Mode */
  inregs.x.cx = 0;                          /* Create attribute */
  inregs.x.dx = 0x12;                       /* Function flag */
  inregs.x.di = parm_value;                 /* Parm list value */
  inregs.x.si = (unsigned)(n);              /* Target file to create */
  intdos(&inregs,&outregs);                 /* Int 21 */
  if (outregs.x.cflag & CARRY)              /* If the carry flag is set */
    status = outregs.x.ax;                  /*   get returned error */
  else                                      /* else                 */
    status = (outregs.x.cflag & CARRY);     /*   set status to NOERROR */
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dopen                                              */
/*                                                                        */
/**************************************************************************/

unsigned dopen(n)
char *n;
{
  unsigned status;

  inregs.x.ax = 0x6c00;                     /* Extended open */
  inregs.x.bx = 8320;                       /* Open mode (flags) */
  inregs.x.cx = 0;                          /* Create attr (ignore) */
  inregs.x.dx = 257;                        /* Function control (flags) */
  inregs.x.si = (unsigned)(n);              /* File name to open */
  inregs.x.di = -1;                         /* Parm list (null) */
  intdos(&inregs,&outregs);                 /* Int 21 */
  if (outregs.x.cflag & CARRY)              /* If the carry flag is set */
    status = outregs.x.ax;                  /*   get returned error */
  else                                      /* else                 */
    status = (outregs.x.cflag & CARRY);     /*   set status to NOERROR */
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   ddelete                                            */
/*                                                                        */
/**************************************************************************/

unsigned ddelete(n)
char *n;                                    /* File to be deleted */
{
  unsigned status;

  inregs.x.ax = 0x4100;                     
  inregs.x.dx = (unsigned)(n);              
  intdos(&inregs,&outregs);                 
  if (outregs.x.cflag & CARRY)              /* If the carry flag is set */
    status = outregs.x.ax;                  /*   get returned error */
  else                                      /* else                 */
    status = (outregs.x.cflag & CARRY);     /*   set status to NOERROR */
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dread                                              */
/*                                                                        */
/**************************************************************************/

unsigned dread(h,s,o,l)
unsigned h;
unsigned s;
unsigned o;
unsigned l;
{
  unsigned status;

  inregs.x.ax = 0x3f00;                     /* Read from file or device */
  inregs.x.bx = h;                          /* File handle */
  segregs.ds  = s;                          /* Buffer segment */
  inregs.x.dx = o;                          /* Buffer offset */
  inregs.x.cx = l;                          /* Num of bytes to be read */
  intdosx(&inregs,&outregs,&segregs);
  if (outregs.x.cflag & CARRY)              /* If the carry flag is set */
    status = outregs.x.ax;                  /*   get returned error */
  else                                      /* else                 */
    status = (outregs.x.cflag & CARRY);     /*   set status to NOERROR */
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dwrite                                             */
/*                                                                        */
/**************************************************************************/

unsigned dwrite(handle,segment,offset,length)
unsigned handle;
unsigned segment;
unsigned offset;
unsigned length;
{
  unsigned status;
  unsigned write_len;                       /* Save area for num of bytes to write */

  inregs.x.ax = 0x4000;                     /* Write to file or device */
  inregs.x.bx = handle;                     
  segregs.ds  = segment;                    
  inregs.x.dx = offset;                     
  inregs.x.cx = length;                     
  write_len = length;                       
  intdosx(&inregs,&outregs,&segregs);
  if (outregs.x.cflag & CARRY)              /* If the carry flag is set */
    status = outregs.x.ax;                  /*   get returned error */
  else                                      /* else                 */
    status = (outregs.x.cflag & CARRY);     /*   set status to NOERROR */
  if (status == NOERROR)                    /* If there was not an error */
  {
    if (write_len != outregs.x.ax)          /* and didn't write reqtd num bytes */
      disk_full = TRUE;                     /* Then, disk is full. Ret error */
  }
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dclose                                             */
/*                                                                        */
/**************************************************************************/

unsigned dclose(h)
unsigned h;
{
  unsigned status;

  inregs.x.ax = 0x3e00;                     /* Close a file handle */
  inregs.x.bx = h;                          /* File han ret by open/create */
  intdos(&inregs,&outregs);
  if (outregs.x.cflag & CARRY)              /* If the carry flag is set */
    status = outregs.x.ax;                  /*   get returned error */
  else                                      /* else                 */
    status = (outregs.x.cflag & CARRY);     /*   set status to NOERROR */
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dchmod                                             */
/*                                                                        */
/**************************************************************************/

unsigned dchmod(n,a)
char     *n;
unsigned a;
{
  unsigned status;

  inregs.x.ax = 0x4300 | (a & 0x00ff);       /* Change file mode */
  inregs.x.dx = (unsigned)(n);               /* Ptr to asciiz path name */
  intdos(&inregs,&outregs);
  if (outregs.x.cflag & CARRY)               /* If the carry flag is set */
    status = outregs.x.ax;                   /*   get returned error */
  else                                       /* else */
    status = (outregs.x.cflag & CARRY);      /*   set status to NOERROR */
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dsearchf                                           */
/*                                                                        */
/**************************************************************************/

unsigned dsearchf(s,t,a)
char     *s;
struct   filedata *t;
unsigned a;
{
  unsigned i;
  unsigned status;

  inregs.x.ax = 0x4e00;                     /* Find first matching file */
  inregs.x.cx = a;                          /* Attrib used in search */
  inregs.x.dx = (unsigned)(s);              /* Asciiz string ptr */
  intdos(&inregs,&outregs);
  if (outregs.x.cflag & CARRY)              /* If the carry flag is set */
    status = outregs.x.ax;                  /*   get returned error */
  else                                      /* else */
    status = (outregs.x.cflag & CARRY);     /* set status to NOERROR */
  if (status == NOERROR)
  {
    t->attribute = getbyte(_psp,0x80+21);
    t->time      = getword(_psp,0x80+22);
    t->date      = getword(_psp,0x80+24);
    t->size      = getdword(_psp,0x80+26);
    for (i = 0; i < 15; i++)
      t->name[i] = getbyte(_psp,0x80+30+i);
    strcpy(fix_es_reg,NULL);
  }
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dsearchn                                           */
/*                                                                        */
/**************************************************************************/

unsigned dsearchn(t)
struct filedata *t;
{
  unsigned i;
  unsigned status;

  inregs.x.ax = 0x4f00;                     /* Find next matching file */
  intdos(&inregs,&outregs);                 /* DTA contains prev call info */
  if (outregs.x.cflag & CARRY)              /* If the carry flag is set */
    status = outregs.x.ax;                  /*   get returned error */
  else                                      /* else */
    status = (outregs.x.cflag & CARRY);     /*   set status to NOERROR */
  if (status == NOERROR)
  {
    t->attribute = getbyte(_psp,0x80+21);
    t->time      = getword(_psp,0x80+22);
    t->date      = getword(_psp,0x80+24);
    t->size      = getdword(_psp,0x80+26);
    for (i = 0; i < 15; i++)
      t->name[i] = getbyte(_psp,0x80+30+i);
    strcpy(fix_es_reg,NULL);
  }
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dexit                                              */
/*                                                                        */
/**************************************************************************/

unsigned dexit(s)
unsigned s;
{
  if (target_full)                     /* If unable to copy any files */
    s = ERRLEVEL8;                     /* Insufficient memory */
  exit(s);                             /* terminate program */
  return(NULLARG);           /* note that the return never happens */
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dta_save                                           */
/*                                                                        */
/**************************************************************************/

void dta_save(t,l)
char     *t;
unsigned l;
{
  unsigned i;

  for (i = 0; i < l; i++)
    *(t+i) = getbyte(_psp,0x80+i);
  strcpy(fix_es_reg,NULL);
  return;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dta_restore                                        */
/*                                                                        */
/**************************************************************************/

void dta_restore(t,l)
char     *t;
unsigned l;
{
  unsigned i;

  for (i = 0; i < l; i++)
    putbyte(_psp,0x80+i,*(t+i));
    strcpy(fix_es_reg,NULL);
  return;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   getbyte                                            */
/*                                                                        */
/**************************************************************************/

char getbyte(msegment,moffset)
unsigned int msegment;
unsigned int moffset;
{
  char far * cPtr;

  FP_SEG(cPtr) = msegment;
  FP_OFF(cPtr) = moffset;
  strcpy(fix_es_reg,NULL);
  return(*cPtr);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   getword                                            */
/*                                                                        */
/**************************************************************************/

unsigned getword(msegment,moffset)
unsigned int msegment;
unsigned int moffset;
{
  unsigned far * uPtr;

  FP_SEG(uPtr) = msegment;
  FP_OFF(uPtr) = moffset;
  strcpy(fix_es_reg,NULL);
  return(*uPtr);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   getdword                                           */
/*                                                                        */
/**************************************************************************/

long getdword(msegment,moffset)
unsigned int msegment;
unsigned int moffset;
{
  long far * lPtr;

  FP_SEG(lPtr) = msegment;
  FP_OFF(lPtr) = moffset;
  strcpy(fix_es_reg,NULL);
  return(*lPtr);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   putbyte                                            */
/*                                                                        */
/**************************************************************************/

void putbyte(msegment,moffset,value)
unsigned int msegment;
unsigned int moffset;
char     value;
{
  char far * cPtr;

  FP_SEG(cPtr) = msegment;
  FP_OFF(cPtr) = moffset;
  *cPtr        = value;
  strcpy(fix_es_reg,NULL);
  return;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   putword                                            */
/*                                                                        */
/**************************************************************************/

void putword(msegment,moffset,value)
unsigned int msegment;
unsigned int moffset;
unsigned value;
{
  unsigned far * uPtr;

  FP_SEG(uPtr) = msegment;
  FP_OFF(uPtr) = moffset;
  *uPtr        = value;
  strcpy(fix_es_reg,NULL);
  return;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   putdword                                           */
/*                                                                        */
/**************************************************************************/

void putdword(msegment,moffset,value)
unsigned int msegment;
unsigned int moffset;
long     value;
{
  long far * lPtr;

  FP_SEG(lPtr) = msegment;
  FP_OFF(lPtr) = moffset;
  *lPtr        = value;
  strcpy(fix_es_reg,NULL);
  return;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   load_msg                                           */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Load the set of REPLACE Utility messages to     */
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
  sysloadmsg(&inregs,&outregs);        /* Load utility messages */
  if (outregs.x.cflag & CARRY)
  {
    sysdispmsg(&outregs,&outregs);     /* If load error, display err msg */
    dexit(ERRLEVEL1);
  }
  return;                              /* Return with no error */
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   dcompare                                           */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Check a Y/N response using an Int21 Get         */
/*                        Extended Country information.                   */
/*                                                                        */
/*  INPUT:             function  (character to check) (global)            */
/*                                                                        */
/*  OUTPUT:            status    (carry flag if set or response)          */
/*                                                                        */
/*  NORMAL EXIT:       AX=0=No  (status = 2)                              */
/*                     AX=1=Yes (status = 1)                              */
/*                                                                        */
/**************************************************************************/

unsigned dcompare()
{
  unsigned status;                     /* Receives error cond or Y/N */

  inregs.x.dx = outregs.x.ax;          /* Char rec'd by msg hndlr to be ckd */
  inregs.x.ax = 0x6523;                /* 65=Get-Ext-Cty 23=Y/N chk */
  intdos(&inregs,&outregs);            /* Int21 call */

  /* If carry flag is set or invalid return code, then input is not valid */
  if ((outregs.x.cflag & CARRY) || (outregs.x.ax > 1))
    not_valid_input = TRUE;            /* input is not valid */
  else
  {
    not_valid_input = FALSE;           /* input is valid */
    if (outregs.x.ax == 0)
      status = 2;                      /* 2 = No */
    else
      status = 1;                      /* 1 = Yes */
  }
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   display_msg                                        */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  The correct message called by main is displayed */
/*                        to standard out or standard error.              */
/*                                                                        */
/*  INPUT:             msg_num   (message number to display)              */
/*                     outline   (string for replacement parm)            */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/*  NORMAL EXIT:       The correct message called will be displayed to    */
/*                     standard out or standard error.                    */
/*                                                                        */
/*  ERROR EXIT:        Display error message corresponding to number      */
/*                     returned in AX.                                    */
/*                                                                        */
/*  EXTERNAL REF:      SYSDISPMSG                                         */
/*                                                                        */
/**************************************************************************/

void display_msg(msg_num,outline)
int  msg_num;                          /* Message number #define'd */
char *outline;                         /* String for replacemnt parm */
{
  unsigned char function;              /* Y/N response or press key? */
  unsigned int message,                /* Message number to display */
	       msg_class,              /* Which class of messages? */
	       sub_cnt,                /* Number of substitutions? */
	       handle;                 /* Display where? */

  struct sublist
  {
    unsigned char size;                /* Points to next sublist */
    unsigned char reserved;            /* Required for syddispmsg */
    unsigned far  *value;              /* Data pointer */
    unsigned char id;                  /* Id of substitution parm (%1) */
    unsigned char flags;               /* Format of data - (a0sstttt) */
    unsigned char max_width;           /* Maximum field width */
    unsigned char min_width;           /* Minimum field width */
    unsigned char pad_char;            /* char to pad field */
  } sublist;

  switch (msg_num)                     /* Which msg to display? */
  {
    case MSG_NOMEM :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 8;                 /* Message number to display */
	msg_class = EXT_ERR_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT0;           /* Number of substitutions? */
	handle    = STDERR;            /* Display where? */
	break;
    case MSG_INCOMPAT :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 11;                /* Message number to display */
	msg_class = PARSE_ERR_CLASS;   /* Which class of messages? */
	sub_cnt   = SUBCNT0;           /* Number of substitutions? */
	handle    = STDERR;            /* Display where? */
	break;
    case MSG_NOSOURCE :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 2;                 /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT0;           /* Number of substitutions? */
	handle    = STDERR;            /* Display where? */
	break;
    case MSG_NONEREPL :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 3;                 /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT0;           /* Number of substitutions? */
	handle    = STDOUT;            /* Display where? */
	break;
    case MSG_NONEADDE :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 4;                 /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT0;           /* Number of substitutions? */
	handle    = STDOUT;            /* Display where? */
	break;
    case MSG_START :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 21;                /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT0;           /* Number of substitutions? */
	handle    = STDERR;            /* Display where? */
	break;
    case MSG_ERRFNF :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 2;                 /* Message number to display */
	msg_class = EXT_ERR_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDOUT;            /* Display where? */
	break;
    case MSG_ERRPNF :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 3;                 /* Message number to display */
	msg_class = EXT_ERR_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDOUT;            /* Display where? */
	break;
    case MSG_ERRACCD :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 5;                 /* Message number to display */
	msg_class = EXT_ERR_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDOUT;            /* Display where? */
	break;
    case MSG_ERRDRV :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 15;                /* Message number to display */
	msg_class = EXT_ERR_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDERR;            /* Display where? */
	break;
    case MSG_BADPARM :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 10;                /* Message number to display */
	msg_class = PARSE_ERR_CLASS;   /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDERR;            /* Display where? */
	break;
    case MSG_WARNSAME :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 11;                /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDERR;            /* Display where? */
	break;
    case MSG_ERRDSKF :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 12;                /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDERR;            /* Display where? */
	break;
    case MSG_REPLACIN :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 13;                /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDOUT;            /* Display where? */
	break;
    case MSG_ADDING :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 14;                /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDOUT;            /* Display where? */
	break;
    case MSG_SOMEREPL :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 15;                /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDOUT;            /* Display where? */
	break;
    case MSG_SOMEADDE :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 16;                /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDOUT;            /* Display where? */
	break;
    case MSG_NONFOUND :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 17;                /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDOUT;            /* Display where? */
	break;
    case MSG_QREPLACE :
	function  = DOS_CON_INPUT;     /* Y/N response or press key? */
	message   = 22;                /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDERR;            /* Display where? */
	break;
    case MSG_QADD :
	function  = DOS_CON_INPUT;     /* Y/N response or press key? */
	message   = 23;                /* Message number to display */
	msg_class = UTILITY_CLASS;     /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDERR;            /* Display where? */
	break;
    case MSG_XTRAPARM :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 1;                 /* Message number to display */
	msg_class = PARSE_ERR_CLASS;   /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDERR;            /* Display where? */
	break;
    case MSG_BADSWTCH :
	function  = NO_INPUT;          /* Y/N response or press key? */
	message   = 3;                 /* Message number to display */
	msg_class = PARSE_ERR_CLASS;   /* Which class of messages? */
	sub_cnt   = SUBCNT1;           /* Number of substitutions? */
	handle    = STDERR;            /* Display where? */
	break;
    default :
      if (msg_num >= MSG_OPTIONS_FIRST && msg_num <= MSG_OPTIONS_LAST)
      {
	function  = NO_INPUT;                 
	message   = msg_num;           /* Message number to display */
	msg_class = UTILITY_CLASS;
	sub_cnt   = SUBCNT0;           /* Number of substitutions? */
	handle    = STDOUT;            /* Display where? */
	break;
      }
  }

  switch (msg_num)
  {
    case MSG_NOMEM    :                          /* Insufficient memory */
    case MSG_INCOMPAT :                          /* Invalid parameter combo */
    case MSG_NOSOURCE :                          /* Source path required */
    case MSG_NONEREPL :                          /* No files replaced */
    case MSG_NONEADDE :                          /* No files added */
    case MSG_START    :
	inregs.x.ax = message;                   /* Press any key... */
	inregs.x.bx = handle;                    /* STDERR or STDOUT */
	inregs.x.cx = sub_cnt;                   /* SUBCNT0 */
	inregs.h.dl = function;                  /* NO_INPUT */
	inregs.h.dh = (unsigned char)msg_class;  /* Extended, Parse or Utility */
	sysdispmsg(&inregs,&outregs);            /* Call common msg service */
	break;
    case MSG_BADPARM  :                          /* Invalid parameter */
    case MSG_XTRAPARM :                          /* Too many parameters */
    case MSG_BADSWTCH :                          /* Invalid switch */
    case MSG_ERRFNF   :                          /* File not found */
    case MSG_ERRPNF   :                          /* Path not found */
    case MSG_ERRACCD  :                          /* Access denied */
    case MSG_ERRDRV   :                          /* Invalid drive specification */
    case MSG_WARNSAME :                          /* File cannot be copied... */
    case MSG_ERRDSKF  :
	sublist.value     = (unsigned far *)outline;  /* Insufficient disk space */
	sublist.size      = SUBLIST_LENGTH;
	sublist.reserved  = RESERVED;
	sublist.id        = 0;
	sublist.flags     = STR_INPUT;
	sublist.max_width = 0;
	sublist.min_width = 1;
	sublist.pad_char  = BLNK;
	inregs.x.ax       = message;                  /* Which message? */
	inregs.x.bx       = handle;                   /* STDERR or STDOUT */
	inregs.x.si       = (unsigned int)&sublist;   /* SUBCNT0 */
	inregs.x.cx       = sub_cnt;                  /* NO_INPUT */
	inregs.h.dl       = function;                 /* Extended, Parse or Utility */
	inregs.h.dh       = (unsigned char)msg_class; /* Call common msg service */
	sysdispmsg(&inregs,&outregs);
	break;
    case MSG_SOMEREPL :                               /* %1 file(s) replaced */
    case MSG_SOMEADDE :
	sublist.value     = (unsigned far *)outline;  /* %1 file(s) added */
	sublist.size      = SUBLIST_LENGTH;
	sublist.reserved  = RESERVED;
	sublist.id        = 1;
	sublist.flags     = DEC_INPUT;
	sublist.max_width = 0;
	sublist.min_width = 1;
	sublist.pad_char  = BLNK;
	inregs.x.ax       = message;                  /* Which message? */
	inregs.x.bx       = handle;                   /* STDERR or STDOUT */
	inregs.x.si       = (unsigned int)&sublist;   /* SUBCNT1 */
	inregs.x.cx       = sub_cnt;                  /* NO_INPUT */
	inregs.h.dl       = function;                 /* Extended, Parse or Utility */
	inregs.h.dh       = (unsigned char)msg_class; /* Call common msg service */
	sysdispmsg(&inregs,&outregs);
	break;
    case MSG_REPLACIN :                               /* Replacing %1 */
    case MSG_ADDING   :                               /* Adding %1 */
    case MSG_NONFOUND :                               /* No files found */
    case MSG_QREPLACE :                               /* Replace %1? (Y/N) */
    case MSG_QADD     :
	sublist.value     = (unsigned far *)outline;  /* Add %1? (Y/N) */
	sublist.size      = SUBLIST_LENGTH;
	sublist.reserved  = RESERVED;
	sublist.id        = 1;
	sublist.flags     = STR_INPUT;
	sublist.max_width = 0;
	sublist.min_width = 1;
	sublist.pad_char  = BLNK;
	inregs.x.ax       = message;                  /* Which message? */
	inregs.x.bx       = handle;                   /* STDERR or STDOUT*/
	inregs.x.si       = (unsigned int)&sublist;   /* SUBCNT1 */
	inregs.x.cx       = sub_cnt;                  /* NO_INPUT or DOS_CON_INPUT */
	inregs.h.dl       = function;                 /* Extended, Parse or Utility */
	inregs.h.dh       = (unsigned char)msg_class; /* Call common msg service */
	sysdispmsg(&inregs,&outregs);
	break;
    default           :
      if (msg_num >= MSG_OPTIONS_FIRST && msg_num <= MSG_OPTIONS_LAST)
      {
	inregs.x.ax = message;
	inregs.x.bx = handle;
	inregs.x.cx = sub_cnt;
	inregs.h.dl = function;
	inregs.h.dh = (unsigned char)msg_class;
	sysdispmsg(&inregs,&outregs);
	break;
      }
      else
      {
	restore();
	dexit(ERRLEVEL1);
	break;
      }
  }

  strcpy(fix_es_reg,NULL);             /* (Set es reg correct) */
  if (outregs.x.cflag & CARRY)         /* Is the carry flag set? */
  {                                    /* Setup regs for extd-err */
    inregs.x.bx = STDERR;
    inregs.x.cx = SUBCNT0;
    inregs.h.dl = NO_INPUT;
    inregs.h.dh = EXT_ERR_CLASS;
    sysdispmsg(&inregs,&outregs);      /* Call to display ext_err msg */
    restore();
    dexit(ERRLEVEL1);
  }
  return;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   check_appendx_install                              */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Determine if append and correct version is      */
/*                        currently installed.                            */
/*                                                                        */
/*  INPUT:             none                                               */
/*                                                                        */
/*  OUTPUT:            status (TRUE or FALSE)                             */
/*                                                                        */
/**************************************************************************/

unsigned check_appendx_install()
{
  unsigned status = FALSE;

  inregs.x.ax = GETX_INSTALL;          /* Get Append /x status */
  int86(0x2f,&inregs,&outregs);        /* Make the call */
  if (outregs.h.al)
  {
    inregs.x.ax = GETX_VERSION;
    int86(0x2f,&inregs,&outregs);
    if (outregs.x.ax == X_INSTALLED)
      status = TRUE;
  }
  return(status);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   check_appendx                                      */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Get the append /x status.                       */
/*                                                                        */
/*  INPUT:             none                                               */
/*                                                                        */
/*  OUTPUT:            bx (contains append bits set)                      */
/*                                                                        */
/**************************************************************************/

unsigned check_appendx()
{
  inregs.x.ax = GETX_STATUS;           /* Get Append /x status */
  int86(0x2f,&inregs,&outregs);        /* Make the call */
  return(outregs.x.bx);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   set_appendx                                        */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Set the append /x status.                       */
/*                                                                        */
/*  INPUT:             set_state (turn appendx bit off or reset original) */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/**************************************************************************/

void set_appendx(set_state)
unsigned set_state;
{
  inregs.x.ax = SETX_STATUS;           /* Set Append /x status */
  inregs.x.bx = set_state;
  int86(0x2f,&inregs,&outregs);
  return;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   parser_prep                                        */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Initialize all structures for the parser.       */
/*                                                                        */
/*  INPUT:             source (command line string)                       */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/**************************************************************************/

void parser_prep(source)
char *source;                          /* Commandline */
{
  p_p.p_parmsx_address    = &p_px;               /* Address of extd parm list */
  p_p.p_num_extra         = 0;                   /* No extra declarations */

  p_px.p_minp             = MINPOSITION;         /* 1 required positional */
  p_px.p_maxp             = MAXPOSITION;         /* 2 maximum positionals */
  p_px.p_control1         = &p_con1;             /* pointer to next control blk */
  p_px.p_control2         = &p_con2;             /* pointer to next control blk */
  p_px.p_maxs             = 1;                   /* Specify # of switches */
  p_px.p_switch           = &p_swit;             /* Point to the switch blk */
  p_px.p_maxk             = 0;                   /* Specify # of keywords */

  p_con1.p_match_flag     = REQ_FILESPEC;        /* File spec required */
  p_con1.p_function_flag  = CAPRESULT;           /* Cap result by file table */
  p_con1.p_result_buf     = (unsigned int)&rslt1;
  p_con1.p_value_list     = (unsigned int)&novals;
  p_con1.p_nid            = 0;

  p_con2.p_match_flag     = OPT_FILESPEC;        /* File spec & optional */
  p_con2.p_function_flag  = CAPRESULT;           /* Cap result by file table */
  p_con2.p_result_buf     = (unsigned int)&rslt1;
  p_con2.p_value_list     = (unsigned int)&novals;
  p_con2.p_nid            = 0;

  p_swit.sp_match_flag    = OPT_SWITCH;          /* Optional (switch) */
  p_swit.sp_function_flag = NOCAPPING;           /* Cap result by file table */
  p_swit.sp_result_buf    = (unsigned int)&rslt2;
  p_swit.sp_value_list    = (unsigned int)&novals;
  p_swit.sp_nid           = 7;                   /* One switch allowed */
  strcpy(p_swit.sp_keyorsw1,A_SW);               /* Identify the switch */
  strcat(p_swit.sp_keyorsw2,P_SW);               /* Identify the switch */
  strcat(p_swit.sp_keyorsw3,R_SW);               /* Identify the switch */
  strcat(p_swit.sp_keyorsw4,S_SW);               /* Identify the switch */
  strcat(p_swit.sp_keyorsw5,U_SW);               /* Identify the switch */
  strcat(p_swit.sp_keyorsw6,W_SW);               /* Identify the switch */
  strcat(p_swit.sp_keyorsw7,Q_SW);

  inregs.x.si = (unsigned int)source;            /* Make DS:SI point to source */
  inregs.x.cx = 0;                               /* Operand ordinal */
  inregs.x.di = (unsigned int)&p_p;              /* Address of parm list */
  return;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   display_exit                                       */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Display the message, then terminate the utility.*/
/*                                                                        */
/*  INPUT:             msg_num     (#define'd message to display)         */
/*                     outline     (sublist substitution)                 */
/*                     error_code  (errorlevel return code)               */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/**************************************************************************/

void display_exit(msg_num,outline,error_code)
int  msg_num;                          /* Message number #define'd */
char *outline;
int  error_code;
{
  display_msg(msg_num,outline);        /* First, display the msg */
  restore();
  dexit(error_code);                   /* Then, terminate utility */
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   setup_ctl_brk                                      */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Change the CTL BRK vector to point to handler   */
/*                        routine.                                        */
/*                                                                        */
/*  INPUT:             none                                               */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/**************************************************************************/

void setup_ctl_brk()
{
  /* set the ctl brk vector to point to us */
  segread(&segregs);
  inregs.x.ax = SETVEC_CTLBRK;                   /* Set vector,ctl brk */
  inregs.x.dx = (unsigned)ctl_brk_handler;       /* Offset points to us */
  segregs.ds  = segregs.cs;
  intdosx(&inregs,&outregs,&segregs);            /* Int 21 */
  return;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   setup_crit_err                                     */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Change the critical error vector to point to    */
/*                        the handler routine.                            */
/*                                                                        */
/*  INPUT:             none                                               */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/**************************************************************************/

void setup_crit_err()
{
  /* get and save original vector pointers */
  inregs.x.ax = GETVEC_CRITERR;               /* Get vector,crit err */
  intdosx(&inregs,&outregs,&segregs);         /* Int 21 */
  oldint24 = outregs.x.bx;                    /* Save orig offset */
  *((unsigned *)(&oldint24)+1) = segregs.es;
  strcpy(fix_es_reg,NULL);

  /* set the crit err vector to point to us */
  segread(&segregs);
  inregs.x.ax = SETVEC_CRITERR;               /* Set vector,crit err */
  inregs.x.dx = (unsigned)crit_err_handler;   /* Offset points to us */
  segregs.ds  = segregs.cs;
  intdosx(&inregs,&outregs,&segregs);         /* Int 21 */
  strcpy(fix_es_reg,NULL);
  return;
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   restore                                            */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Restore the original appendx before exiting.    */
/*                                                                        */
/*  INPUT:             none                                               */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/**************************************************************************/

void restore()
{
  /* restore append/x status */
  if (append_installed)
    set_appendx(x_status);        /* Reset append/x status */
  return;
}


#if SELECT_SUPPORT

/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   check_select                                       */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Perform INT 2F function to detect if SELECT     */
/*                        invoked REPLACE utility.  If so, store          */
/*                        returned information (number of files,          */
/*                        inclusion/exclusion list, and filenames).       */
/*                                                                        */
/*  INPUT:             none                                               */
/*                                                                        */
/*  OUTPUT:            none                                               */
/*                                                                        */
/**************************************************************************/

void check_select()
{
  int i;
  int index;
  struct SREGS segsloc;                /* Structure to move data seg */


  inregs.x.ax =  (unsigned int) DETECT_SELECT;   /* 1C01 */
  int86x(0x2f,&inregs,&outregs,&segregs);
  if (outregs.h.al == SELECT_PRESENT)            /* 0FFH if present, else 1 */
  {
    detect_select_flag = TRUE;
    num_select_files   = outregs.x.cx;
    exclusion_list     = outregs.h.bl;           /* Returns 0 or 1 */

    /*
     *  Move the data from Select's data segment into Replace's
     */
    segread(&segsloc);                           /* Read from Select data seg */
    movedata(segregs.ds, outregs.x.si, segsloc.ds,
	     (unsigned int)select_files_temp, outregs.x.cx * 12);

    /* Get every filename */
    for (index = 0; index < num_select_files; index++)
    {
      for (i = 0; i < 12; i++)                   /* Filename length */
      {
	/* Replace blanks with nulls so names will be null terminated */
	if (select_files_temp[index].name[i] == BLANK)
	  select_files[index].name[i] = NULL;
	else
	  select_files[index].name[i] = select_files_temp[index].name[i];
      }
      select_files[index].name[13] == NULL;      /* Null terminate strings */
    }
  }
  strcpy(fix_es_reg,NULL);
}


/**************************************************************************/
/*                                                                        */
/*  SUBROUTINE NAME:   check_select_list                                  */
/*                                                                        */
/*  SUBROUTINE FUNCTION:  Compare SELECT list of filenames with possible  */
/*                        files to be added/replaced.  If match is        */
/*                        found, check inclusion/exclusion flag and       */
/*                        return decision to copy file or not.            */
/*                                                                        */
/*  INPUT:             Possible list of files to add/replace              */
/*                                                                        */
/*  OUTPUT:            True/False                                         */
/*                                                                        */
/**************************************************************************/

unsigned check_select_list(fl)
struct filedata *fl;
{
  int index;
  int samefile = FALSE;

  for (index = 0; ((index < num_select_files) && (!samefile)); index++)
  {
    /* For each possible file, check against SELECT list */
    samefile = same(fl->name,select_files[index].name);
  }

  /* Truth table */
  if ((exclusion_list) == (char)(samefile))
    return(FALSE);
  return(TRUE);
}

#endif  /* SELECT_SUPPORT */


