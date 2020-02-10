;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
/*
 *  This file contains the constants, globals, structure definitions,
 *  and extern declarations for the label utility.
 */

/*
 *  Constant Declarations
 */ 
#define  MAX_LABEL_LENGTH  11
#define  OK  0
#define  EOL  '\0'
#define  EOFCHAR 0x1a
#define  TRUE  1
#define  FALSE 0
#define  DRIVE_BYTE  7
#define  NAME  8
#define  ENDNAME  19
#define	 LEN_UNKNOWN  0
#define  STDOUT	1
#define  STDERR 2

#define SPACEBAR ' '                             /* M002 */
#define CARRIAGE_RETURN '\r'                     /* M002 */
#define DBL_QUOTE '"'                            /* M002 */
#define CMDLINE_OFFSET 0x81                      /* M002 */

/*  DOS version numbers */
#define  MAJOR_VERSION  5                        /* M001 */
#define  MINOR_VERSION  0

/* Definitions for determining if network drive */
#define  BIT_12_MASK  0x1000
#define  IOCTL	0x44
#define  REMOTE 0x9



/*
 *  Typedef Declarations
 */
typedef  int  LoopIndex;
typedef  int  Boolean;
typedef  int  ErrorCode;



/*
 *  Global Variables
 */
#define BAD_CHARS 21                                       /* M002 */
char BadChars[BAD_CHARS] = "*?/\\|.,;:+=<>[]()&^\"";       /* M002 */

char Drive[3] = "?:";
char TempDrive[3] = "?:";
char Label[13] = "";
char OldLabel[13] = "";
char fcb[128] = {0xff, 0, 0, 0, 0, 0, 0x8, 0, '?', '?', '?', '?', '?', '?', '?', '?', '?', '?', '?'};
char con_fcb[44] = {0xff, 0, 0, 0, 0, 0, 0, 0, 'C', 'O', 'N',' ',' ',' ',' ',' ',' ',' ',' '};
char creat_fcb[44] = {0xff, 0, 0, 0, 0, 0, 0x8, 0, ' ', ' ', ' ',' ',' ',' ',' ',' ',' ',' ',' '};
Boolean Nolabel = TRUE;
Boolean firsttime = TRUE;
Boolean DriveNotFirst = FALSE;
char newline[2] = {0xd, 0xa};
#define	NEW_LEN  2

/* buffers for changing directories */
#define PATHLEN 64                               /* M003 */
char curdir[PATHLEN] = {0};                      /* M003 */
char rootdir[4] = "?:\\";                        /* M003 */

/* buffer for mygets */
char getsbuf[MAX_LABEL_LENGTH+3];	/* 2 extra for header */
char input_redir = -1;			/* TRUE if input redirected */



/*
 *  Function Prototypes
 */
int	main(int, char * []);
void	DumbPrint(int,char *,int);
int	CheckOptionsHelp(int, char **);
void	process_commandline(int, char **);
void	GetDrive(void);
void	DisplayLabel(void);
void	GetLabel(void);
char    *gets(char *);
void	puts(char *);
void	DeleteLabel(void);
int	CreateLabel(void);
Boolean LegitDrive(char *);
Boolean IllegitLabelWithDrive(char *);
void    SaveLabel(char *);
Boolean IllegitimateLabel(char *);
void	PrintHex(char);
void	mygets(char *,unsigned);
Boolean CheckCmdLineQuotes(void);                /* M002 */
void	ExitLabel(void);                         /* M003 */
void far _interrupt ctrlc_hdlr(void);            /* M003 */


#ifdef DBCS
unsigned char *DBCSstrupr(unsigned char *str);
int	IsDBCSLeadByte(unsigned char);
int	CheckDBCSTailByte(unsigned char *,unsigned char *);
#endif



/*
 *   Macro Definitions
 */

/***************************************************************************/
/* Routine:  GetCurDir                                                     */
/*                                                                         */
/* Function: Gets the current directory.  It gets the ASCIIZ string that   */
/*           that describes the path from the root to the current          */
/*           directory, and the name of that directory.                    */
/*           The name returned does NOT contain the drive letter or the    */
/*           leading backslash.                                            */
/* M003                                                                    */
/***************************************************************************/

#define GetCurDir(reg, dir)                                                 \
{                                                                           \
  reg.x.ax = 0x4700;                        /* get current directory */     \
  reg.h.dl = 0;                                                             \
  reg.x.si = (unsigned)dir;                                                 \
  intdos(&reg, &reg);                                                       \
}


/***************************************************************************/
/* Routine:  SetCurDir                                                     */
/*                                                                         */
/* Function: Sets the current directory using the specified drive and      */
/*           path.                                                         */
/* M003                                                                    */
/***************************************************************************/

#define SetCurDir(reg, dir)                                                 \
{                                                                           \
  reg.x.ax = 0x3B00;                        /* set current directory */     \
  reg.x.dx = (unsigned)dir;                                                 \
  intdos(&reg, &reg);                                                       \
}

