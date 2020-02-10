;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*
 *  This file contains the constants, globals, structure definitions,
 *  extern declarations, and macro definitions for the subst utility.
 *
 *  Date:       10-10-90
 */


/*
 *  Parse Equates
 */
#define ASCII_DRIVE       'A'-1             /* Convert to Ascii drive */
#define CAPRESULT         0x0001            /* Cap result by file table */
#define DRVONLY_OPT       0x0101            /* Drive only & optional */
#define ERRORLEVEL1       1                 /* Parsing error occurred */
#define FALSE             0
#define FILESPEC_OPT      0x0201            /* File spec & optional */
#define MAX               256               /* Define a limit */
#define MAXPOSITION       2                 /* Max positionals in cmdline */
#define MINPOSITION       0                 /* Min positionals in cmdline */
#define NOCAPPING         0x0000            /* Do not cap result */
#define NULL              0
#define SWITCH_OPT        0x0000            /* Optional switch */

/*
 *  Message Equates
 */
#define MSG_PARMNUM       1                 /* Incorrect Num of Parms */
#define MSG_BADPATH       2                 /* Path not Found */
#define MSG_NOMEM         3                 /* Insufficient memory */
#define MSG_BADPARM       4                 /* Invalid parameter */
#define MSG_NETERR        5                 /* Cannot %1 a network drv */
#define MSG_INVSWTCH      6                 /* Invalid switch */
#define MSG_INUSE	  7

#define MSG_OPTIONS_FIRST 300
#define MSG_OPTIONS_LAST  306

#define BLNK              ' '               /* For sublist.pad_char */
#define CARRY             0x0001            /* To test carry from msg hndlr */
#define D_SWITCH          "/D"              /* For switch id */
#define O_SWITCH          "/?"              /* /? for options help */
#define EXT_ERR_CLASS     0x01              /* DOS Extended error class */
#define MAXWIDTH          0                 /* 0 ensures no padding */
#define MINWIDTH          1                 /* At least 1 char in parm */
#define NO_INPUT          0x00              /* No input characters */
#define PARSE_ERR_CLASS   0x02              /* Parse error class */
#define RESERVED          0                 /* Reserved byte field */
#define STDERR            0x0002            /* Standard error device handle */
#define STDOUT            0x0001            /* Std output device handle */
#define STR_INPUT         16                /* Byte def for sublist.flags */
#define SUB_ID0           0                 /* 0 for error substitution */
#define SUB_ID1           1                 /* Only 1 replaceable parameter */
#define SUBCNT0           0                 /* 0 substitutions in message */
#define SUBCNT1           1                 /* 1 substitution in message */
#define SUBLIST_LENGTH    11                /* Length of sublist structure */
#define UTILITY_CLASS     0x0ff             /* Utility message class */

/*
 *  Miscellaneous
 */
char cmdln_drive[64]   = {0};               /* Save user's input in   */
char cmdln_flspec[64]  = {0};               /* order to pass to error */
char cmdln_invalid[64] = {0};
char cmdln_switch[64]  = {0};               /* message, if needed     */
char p_drive[3];                            /* Recvs drive ltr from parser */
char p_filespec[64];                        /* Recvs filespec from parser */
char replparm_SUBST[]  = "SUBST";           /* Cannot SUBST a network drv */
                                           
unsigned char source[MAX] = {0};            /* buffer for string manipulation */

int index;                                  /* Used in creating cmdline string */

struct sysVarsType SysVars;

/*
 *  Parse Structures
 */
struct p_parms  p_p;                        /* # of extras & pts to descrptn */
struct p_parmsx p_px;                       /* min/max parms & pts to controls */
struct p_control_blk p_con1;                /* 1st posit parm in cmd str */
struct p_control_blk p_con2;                /* 2nd posit parm in cmd str */
struct p_switch_blk p_swi1;                 /* /D switch in cmd str */
struct p_switch_blk p_swi2;                 /* /D switch in cmd str */
struct p_result_blk rslt1;                  /* Result blk rtrnd from parser */
struct p_fresult_blk rslt2;                 /* Result blk rtrnd from parser */
struct p_result_blk rslt3;                  /* Result blk rtrnd from parser */
struct noval novals = {0};                  /* Value list not used */

union REGS inregs, outregs;                 /* Define register variables */


/*
 *  External Function Prototypes
 */
extern	int   access(char *, char *);
extern	void  exit(int);
extern	char  fGetCDS(int, struct CDSType *);
extern	int   fNet(int);
extern	int   fPathErr(char *);
extern	int   fPhysical(int);
extern	char  fPutCDS(int, struct CDSType *);
extern	int   fShared(int);
extern	long  GetDPB(int);
extern	int   getdrv(void);
extern	void  GetVars(struct sysVarsType *);
extern	int   open(char *, int);
extern	void  parse(union REGS*, union REGS*);
extern	void  printf(const char *, ...);
extern	void  PutVars(struct sysVarsType *);
extern	void  rootpath(char *, char *);
extern	char  *strbscan(char *, char *);                                                      /* SM extern'd */
extern	void  sysdispmsg(union REGS*, union REGS*);
extern	void  sysloadmsg(union REGS*, union REGS*);
              

/*
 *  Internal Function Prototypes
 */
extern  void  main(int, char **);
extern  int   ParseIt(void);
extern  char  *BackFix(char *);
extern  char  fDelete(char *);
extern  void  Insert(char *, char *);
extern  void  Display(void);
extern  void  load_msg(void);
extern  void  display_msg(int, char*);
extern  void  DisplayOptions(void);
extern  void  dispmsg_terminate(int, char*);
extern  void  Parser_Prep(char *);


