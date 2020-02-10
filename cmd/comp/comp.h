/*************************************************************************/
/*                                                                       */
/*  FILE:    Comp.H                                                      */
/*                                                                       */
/*  PURPOSE: This file contains the constants, globals, structure        */
/*           definitions, and function declarations for the comp         */
/*           utility.                                                    */
/*                                                                       */
/*  LABEL:   Microsoft Confidential                                      */
/*           Copyright (c) Microsoft Corporation 1991                    */
/*           All Rights Reserved.                                        */
/*                                                                       */
/*************************************************************************/


/* Definitions relating to the pathname */
#define  MAXPATH  67
#define  FLNAM    12
#define  MAXFLNM  MAXPATH + FLNAM + 1       /* Add 1 for EOL char ! */

typedef  char  Pathname[MAXFLNM];
typedef  int   Flag;
typedef  int   Boolean;
typedef  int   Index;


/* Random definitions */
#define  TRUE               1
#define  FALSE              0
#define  CASE_DIFFERENCE    32
#define  MAX_PARAMS         8
#define  OPT_SIZE           10
#define  MAX_OPTIONS        5
#define  MAX_NUM_DIFF       10
#define  NO_MEMORY          0
#define  NULLPTR            0
#define  ENOUGH_EXTRA       5
#define  VERSION_NUM        0x30
#define  MAJ_VER_NUM        5
#define  MIN_VER_NUM        0
#define  APPEND_INT         0x2f
#define  GET_APPEND_STATUS  0xb706
#define  SET_APPEND_STATUS  0xb707

#define  EOFCHAR	    0x1a
#define  BACKSPACE          '\b'
#define  RETURNCHAR         '\r'
#define  NEWLINE            '\n'
#define  SPACEBAR           ' '
#define  BEEPCHAR           '\a'


/*
 *  Global Variables
 */
unsigned limit = 0;                    /* # of lines to comp, if specified */
Flag decimal = FALSE,                  /* mismatched bytes displayed as decimal */
     asci = FALSE,                     /* mismatched bytes display as chars */
     no_case = FALSE,                  /* do case-insensitive compare */
     count_lines = FALSE;              /* count CR characters */
char *buf1,  *buf2,                    /* buffers to read the 2 files into */
     fmt[] = "%s%u\n%s%?\n%s%!\n",     /* the one used by printf */
     fmt_str[] = "%s%u\n%s%?\n%s%!\n", /* a copy we might need to reset to */
     lfmt[] = "%s%lX\n%s%?\n%s%!\n",   /* the one used by printf */
     lfmt_str[] = "%s%lX\n%s%?\n%s%!\n";    /* a copy */
unsigned int bufsiz; 
unsigned int append_stat;              /* current status of append */
char getsbuf[128];		       /* buffer for all gets's in comp */
unsigned char input_redir;             /* TRUE if input redirected */

char crlfstr[] = "\r\n$";


/*
 *  Forward Declarations
 */
int main(int, char **);
int CheckOptionsHelp(int, char **);
unsigned comp(char *, char *);
unsigned open_file(char *, int *);
unsigned read_file(int, void far *, unsigned, unsigned *, char *);
int getmem(char **, char **);
void getoneopt(char *);
unsigned ppargs(char **, int *, char **);
int dronly(char *);
void setfmt(char *, char);
char *extend_length(char *);
Boolean has_extension(char *);
void compgets(char *, int);
void GetInput(int *);
char ReadStdin(void);                            /* M012 */
void WriteStderr(char);
void ParseFileNames(char *, char *);
char *FindFileName(char *);
Boolean HasWildcard(char *);
void CheckWildcard(char *, char *);
int ExpandFile2(char *, char *);                 /* M008 */
void ExitComp(void);


#ifdef DBCS
int IsDBCSLeadByte(unsigned char);
int CheckDBCSTailByte(unsigned char *, unsigned char *);
#endif



/*
 *   Macro Definitions
 */

/*********************************************************************/ 
/* Routine:   Beep                                                   */
/*                                                                   */
/* Function:  Sounds a beep to stderr.  Sending the beep to stderr   */
/*            allows the beep to be heard when the output is         */
/*            redirected.                                            */
/*                                                                   */
/* DEFINED AS A MACRO                                                */
/*********************************************************************/

#define Beep()                                                        \
{                                                                     \
  WriteStderr(BEEPCHAR);                                              \
}

