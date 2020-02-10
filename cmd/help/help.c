/* Help command for DOS 5.0
*
*  This help command has two functions: it displays a list of commands
*  with one line descriptions of what they do, and it tries to display
*  more detailed help for any individual command by invoking it with
*  the /? option.  It is driven from a help database text file, DOSHELP.HLP,
*  which contains the list of recognized commands and their one-line
*  descriptions.  Since the help file is a simple text file, it can be
*  easily extended by third parties.
*
*  The help file has a simple format: each line is a separate record,
*  terminated by a CR-LF combination.  A record can be a comment, which
*  is preceded by an at sign in the first column, or a command name,
*  beginning in the first column, followed by whitespace and the command
*  description.
*
*  Revision History
*  ================
*  MD  10/3/90              First version
*/

#include <dos.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <io.h>
#include <process.h>
#include <string.h>
#include <conio.h>
#include "proto.h"
#include "message.h"

/* name of help text database file */
#define  HELP_FILE_NAME     "DOSHELP.HLP"

/* marker used to preface comments in help file */
#define  COMMENT_MARK       '@'
                              
/* maximum size of line in help file */
#define  MAX_LINE           160

/* return codes for various functions */
#define  READ_ERROR         2             /* file i/o error occurred */
#define  FILE_ERROR         2             /* file i/o error occurred */
#define  OK                 1             /* OK return result */
#define  NO_HELP            3             /* couldn't find help on command */

/* bits used for DOS Device Information IOCTL */
#define  DEVICE_BIT         0x80          /* handle is device */
#define  STDOUT_BIT         0x02          /* handle is standard output */

#define  TRUE               1
#define  FALSE              0

/* table of names of internal commands, used by is_internal() */
char *name_table[] = {
       "BREAK",
       "CALL",
       "CD",
       "CHCP",
       "CHDIR",
       "CLS",
       "COPY",
       "CTTY",
       "DATE",
       "DEL",
       "DIR",
       "ECHO",
       "ERASE",
       "EXIT",
       "FOR",
       "GOTO",
       "IF",
       "LH",
       "LOADHIGH",
       "MD",
       "MKDIR",
       "PATH",
       "PAUSE",
       "PROMPT",
       "RD",
       "REM",
       "REN",
       "RENAME",
       "RMDIR",
       "SET",
       "SHIFT",
       "TIME",
       "TYPE",
       "VER",
       "VERIFY",
       "VOL",
       NULL
};


int
main(int argc, register char *argv[])
{

       if (argc == 1)                     /* generic help requested */
              return(dump_list());        /* display the whole help file */

       else if (argv[1][0] == '/' && argv[1][1] == '?') /* help /? */
       {
              help_on_help();             /* display help on help */
              return(0);
       }
       else
       {
              return(dump_command(argv[1]));
       }
}


/* display help text for this command */
void help_on_help()
{
       fputs(usage,stdout);
}


/*** dump_list - display contents of entire help file.  Search for the file,
* display each valid record.
*
*      Entry: nothing
*      Exit : returns 0 if everything is okay
*             returns FILE_ERROR if read error occurred
*/
int
dump_list()
{
       register FILE *help_handle;        /* handle for help file */
       char rec_buf[MAX_LINE];            /* buffer for record read */
       register int  read_result;         /* return code from file read */
       int  out_con;                      /* boolean: output device is CON */
       int  screen_height;                /* height of screen in rows */

       /* go get the file, error if not found */
       if ((help_handle = find_help_file()) == NULL)
       {
              fputs(file_not_found,stdout);
              return(FILE_ERROR);
       }

       /* check to see if our output is to CON or something else.
       *  This info is used in send_record to determine if we
       *  should monitor screen length
       */
       out_con = check_console(stdout);

       /* also get the screen height for send_record 
       *  get_screen_height returns rows in low byte, cols in high byte.
       *  just use the low byte here.
       */
       screen_height = get_screen_height() & 0xff;

       /* read each record from the file and display it */
       while ((read_result = get_next_record(help_handle,rec_buf)) == OK)
              send_record(rec_buf, out_con, screen_height);

       /* report read error if one occurred */
       if (read_result == READ_ERROR)
       {
              fputs(read_error,stdout);
              fclose(help_handle);
              return(FILE_ERROR);
       }

       fclose(help_handle);
       return(0);                         /* everything okay */
}


/*** find_help_file - locate the help file, and return a file pointer
*    to it.
*
*      Entry: nothing
*      Exit : file pointer to opened file if found
*             NULL if file not found
*/
FILE *
find_help_file()
{
       char pathname[_MAX_PATH];          /* storage for pathname */

       /* check the current directory for the file */
       if (access(HELP_FILE_NAME,4) == 0)
              /* file found in current directory, return pointer */
              return(fopen(HELP_FILE_NAME,"r"));
       
       else                                /* not in curdir, check path */
       {
                                           /* search the path for the file */
              _searchenv(HELP_FILE_NAME,"PATH",pathname);

              if (*pathname == '\0')        /* return NULL if not found */
                     return(NULL);
              
              /* have the path now, return pointer to file */
              return(fopen(pathname,"r"));
       }
}



/*** get_next_record - read record from help file into supplied buffer.
*    Skip over any comment records, which begin with semicolon.  Record
*    length is limited to MAX_LINE size, but this is not checked.  Compress
*    any leading white space.
*
*      Entry: pointer to help file, address of record buffer
*      Exit : OK if no error, record buffer filled in
*             NULL on EOF
*             READ_ERROR if read error occurs
*/
int
get_next_record(FILE *help_handle, register char *rec_buf)
{
       register char *p;                           

       if (fgets(rec_buf, MAX_LINE, help_handle) != NULL)
       {                                  /* read the record fine */
              p = rec_buf;                /* start of record */

#ifdef NO_WHITE_SPACE
              /* skip any white space at start of record */
              while (isspace(*p))
                     p++;                 /* skip white space */
#endif

              if (*p == COMMENT_MARK)     /* if this record is a comment */
                                          /* get next record recursively */
                     return(get_next_record(help_handle, rec_buf));

#ifdef NO_WHITE_SPACE
              else if (p != rec_buf)      /* compress white space if any */
                          strcpy(rec_buf, p);
#endif                     

              return(OK);                 /* everything's fine */
       }

       /* abnormal return from fgets.  Check if error or just EOF */
       if (feof(help_handle))
              return(NULL);               /* return NULL on end of file */
       else
              return(READ_ERROR);         /* assume some read problem */
}



/*** send_record - display record on screen.  Output is paged based on
*    screen size.  Top line is used to display general instructions.
*    ANSI is used to determine screen height, if present, or else the
*    ROM BIOS data area is examined.
*
*    Screen height stuff is ignored if output is not to display.
*
*      Entry: rec_buf contains text to be displayed, help_handle is file pointer
*             out_con is 0 if output device is not CON
*             screen_height is height of screen in rows
*      Exit : nothing
*/
void
send_record(register char *rec_buf, int out_con, int screen_height)
{
       static int lines_out = 0;       /* number of lines already displayed */
       char c;

       /* check if output redirected, and just dump buffer to stdout if so */
       if (!out_con)
       {
              fputs(rec_buf,stdout);
              return;
       }

       /* this path if output is not redirected */

       if (lines_out == 0)                /* if nothing displayed yet */
       {
              fputs(general_help,stdout);         /* send the general instructions */
              lines_out++;                /* count the line */
       }

       fputs(rec_buf,stdout);                     /* display this record */
       lines_out++;                       /* count the line */

       if (lines_out == screen_height - 2) /* if displayed a screen full */
       {
            /* display the prompt for more */
              fputs(more_prompt, stdout);

              /* get the key to continue */
              if ((c = (char) getch()) == (char) 0x00 || c == (char) 0xE0)
                     getch();             /* two gets needed if first not ASCII */
              putchar('\n');              /* echo a new line */
              lines_out = 0;              /* reset the line count */
       }
}


/*** check_console - check this file pointer to see if it is really the
*    console device.  NOTE assumption that C file handle obtained from
*    fileno() is the same as the DOS file handle
*
*      Entry: file pointer of file to check
*      Exit : TRUE if file is console, FALSE if not
*/
int
check_console(FILE *fp)
{
       union REGS inregs, outregs;

       inregs.x.ax = 0x4400;              /* IOCTL Get Device Information */
       inregs.x.bx = fileno(fp);          /* file handle */
       intdos(&inregs, &outregs);         /* call DOS */
       if (outregs.x.cflag)               /* carry set, error occurred */
              return(TRUE);               /* assume console */
       if (!(outregs.x.dx & DEVICE_BIT))  /* bit not set, indicates file */
              return(FALSE);
       if (outregs.x.dx & STDOUT_BIT)     /* device is standard output? */
              return(TRUE);               /* then assume it is CON */
       return(FALSE);                     /* else assume some other device */
}


/*** dump_command - display help text for named command.  Works by
*    invoking the named command with the /? option.  We check the help
*    text database for the command name first, as an assurance that
*    the command supports /?
*
*      Entry: name contains name of command to display
*      Exit : 0 if everything OK
*             FILE_ERROR if file not found
*             NO_HELP if no help available
*/
int
dump_command(register char *name)
{
       register FILE *help_handle;               /* pointer to help file */
       int exec_err;                             /* return from spawn call */           

       /* go get the file, error if not found */
       if ((help_handle = find_help_file()) == NULL)
       {
              fputs(file_not_found,stdout);
              return(FILE_ERROR);
       }

       /* ensure name passed in is in upper case for lookups */
       get_ucase_tab();                   /* prepare for case mapping */
       dos_toupper(name);

       /* search help database for name of command */
       if (!lookup_name(name, help_handle))
       {
              fputs(help_not_available,stdout);
              return(NO_HELP);
       }

       /* command is in database, so we assume it supports /?.  Exec it
        * using COMMAND if it is internal, directly if external.
        *
        * M000 - APPEND is treated as internal, because it handles its 
        * command line through private channels with COMMAND.COM.
        */
       if (is_internal(name) || !(strcmp(name,"APPEND")))
              exec_err = spawnlp(P_WAIT, "COMMAND.COM", "COMMAND", "/c",
                                 name, "/?", NULL);
       else
              exec_err = spawnlp(P_WAIT, name, name, "/?", NULL);

       if (exec_err == -1)
       {
              fputs(help_not_available,stdout);
              return(NO_HELP);
       }

       return(0);
}
       


/*** lookup_name - search for a particular command name in the help text
*    database.  Just uses a dumb linear search for now.  Assumes the
*    commands in the database are in alphabetical order, excluding comments
*    Also assumes passed in name is in upper case.
*
*      Entry: name to search for, pointer to help file
*      Exit : TRUE if name found, FALSE if not
*/
int
lookup_name(register char *name, FILE *help_handle)
{
       char rec_buf[MAX_LINE];            /* buffer for records read */
       register char *p;
       int  lex_val;                      /* lexical value from strcmp() */

       while (get_next_record(help_handle, rec_buf) == OK)
       {
              /* we now have a valid help record in the buffer.
               * Check the name at the record start against the one
               * we are looking for.  We assume the records are
               * alphabetical, so if the record in the file is lexically
               * greater than the name, then we know we won't find it,
               * and can quit.
               *
               * To do the comparison, first insert a null in place of
               * the first white space in the record, to form a null
               * terminated string.  If the record starts with white
               * space, we assume it is not a command, and just skip
               * the record.  Then map record to upper case using
               * the DOS conversion table.
               */
#ifndef NO_WHITE_SPACE              /* if get_next_record eliminates white
                                     * space, this code is not needed 
                                     */
              if (*rec_buf == ' ')
                     continue;      /* just get next record */
#endif

              p = rec_buf;
              while (!isspace(*p))
                     p++;
              *p = '\0';           /* insert null over first space */

              dos_toupper(rec_buf); /* convert record text to upper case */
              
              if ((lex_val = strcmp(name,rec_buf)) == 0)
                     return(TRUE);        /* name found, okay to launch it */
              else if (lex_val < 0)
                     return(FALSE);       /* passed the name, won't find it */
       }

       /* get here if EOF or error occurred before found name */
       return(FALSE);
}



/*** dos_toupper - convert a string to upper case using the DOS upper case
*    table.
*
*      Entry: string to be converted
*      Exit : string is remapped as needed
*/
void
dos_toupper(register char *string)
{
       while (*string)
       {                           
              *string = make_upper(*string);
              string++;
       }
}



/*** is_internal - determine if a given command represents an internal
*    or external command.  If we try to exec an internal command on
*    its own, without COMMAND/C, we are told the command can't be found.
*    If we exec an external command using Command, Command puts up an
*    unpleasant error message if it can't find it, and doesn't tell us.
*    So we have a special hack here to distinguish between internal and
*    external commands.
*
*      Entry: pointer to command name
*      Exit : TRUE if command is internal, FALSE if not
*/
int
is_internal(register char *name)
{
       register int i = 0;                /* index into internal name table */
       int lex_val;                       /* lexical value from strcmp() */

       while (name_table[i] != NULL)      /* while still more entries */
       {
              if (lex_val = strcmp(name, name_table[i++]) == 0)
                     return(TRUE);        /* found name, is internal */
              if (lex_val < 0)
                     return(FALSE);       /* passed name, won't find it */
       }
       
       return(FALSE);                     /* reached end of table, not found */
}


