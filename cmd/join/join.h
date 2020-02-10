;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
/*
 *  This file contains the constants, globals, structure definitions,
 *  extern declarations, and macro definitions for the join utility.
 *
 *  Date:       10-12-90
 */

 
/*
 *  Parse Equates
 */
#define ASCII_DRIVE       'A'-1        /* Convert to Ascii drive */
#define CAPRESULT         0x0001       /* Cap result by file table */
#define DRVONLY_OPT       0x0101       /* Drive only & optional */
#define ERRORLEVEL1       1            /* Parsing error occurred */
#define FILESPEC_OPT      0x0201       /* File spec & optional */
#define MAXPOSITION       2            /* Max positionals in cmdline */
#define MINPOSITION       0            /* Min positionals in cmdline */
#define NOCAPPING         0x0000       /* Do not cap result */
#define SWITCH_OPT        0x0000       /* Optional switch */


/*
 *  Message Equates
 */
#define MSG_NOMEM         1            /* Insufficient memory */
#define MSG_PARMNUM       2            /* Too many parameters */
#define MSG_DIRNEMP       3            /* Directory not empty */
#define MSG_BADPARM       4            /* Invalid parameter */
#define MSG_NETERR        5            /* Cannot %1 a network drive */
#define MSG_INVSWTCH      6            /* Invalid switch */
#define MSG_OPTIONS_FIRST 300          
#define MSG_OPTIONS_LAST  307          
                                       
#define BLNK              ' '          /* For sublist.pad_char */
#define CARRY             0x0001       /* Check carry flag */
#define D_SWITCH          "/D"         /* Only 1 switch */
#define O_SWITCH          "/?"         /* Only 1 switch */
#define EXT_ERR_CLASS     0x01         /* DOS Extended error class */
#define FALSE             0            
#define MAX               256          
#define MAXWIDTH          0            /* 0 ensures no padding */
#define MINWIDTH          1            /* At least 1 char in parm */
#define NO_HANDLE         0xffff       /* No handle specified */
#define NO_INPUT          0x00         /* No input characters */
#define NO_REPLACE        0x00         /* No replacable parameters */
#define NULL              0            
#define PARSE_ERR_CLASS   0x02         /* Parse error class */
#define RESERVED          0            /* Reserved byte field */
#define STDERR            0x0002       /* Standard error device handle */
#define STDOUT            0x0001       /* Std output device handle */
#define STR_INPUT         16           /* Byte def for sublist.flags */
#define SUB_ID1           1            /* Only 1 replaceable parameter */
#define SUBCNT0           0            /* 0 substitutions in message */
#define SUBCNT1           1            /* 1 substitution in message */
#define SUBLIST_LENGTH    11           /* Length of sublist structure */
#define UTILITY_CLASS     0x0ff        /* Utility message class */

                                       
/*                                     
 *  Miscellaneous                      
 */                                    
char cmdln_drive[64]   = {0};          /* Save user's input in   */
char cmdln_flspec[64]  = {0};          /* order to pass to error */
char cmdln_invalid[64] = {0};          
char fix_es_reg[1];                    /* Corrects es reg after type-"far" */
char p_drive[3]        = {0};          /* Recvs drive ltr from parser */
char p_filespec[64]    = {0};          /* Recvs filespec from parser */
char replparm_JOIN[]   = "JOIN";       /* Cannot JOIN a network drv */
                                      
unsigned char source[MAX] = {0};       /* buffer for string manipulation */

struct sysVarsType SysVars;


/*
 *  Parse Structures
 */
struct p_parms  p_p;                   /* # of extras & pts to descrptn */
struct p_parmsx p_px;                  /* min/max parms & pts to controls */
struct p_control_blk p_con1;           /* 1st posit parm in cmd str */
struct p_control_blk p_con2;           /* 2nd posit parm in cmd str */
struct p_switch_blk p_swi1;            /* /D switch in cmd str */
struct p_switch_blk p_swi2;            /* /D switch in cmd str */
struct p_result_blk rslt1;             /* Result blk rtrnd from parser */
struct p_fresult_blk rslt2;            /* Result blk rtrnd from parser */
struct p_result_blk rslt3;             /* Result blk rtrnd from parser */
struct noval novals = {0};             /* Value list not used */
                                       
union REGS inregs, outregs;            /* Define register variables */

                                       
/*                                     
 *  External Function Prototypes       
 */                                    
extern  void  exit(int);                 
extern  int   ffirst(char *, unsigned int, struct findType *);
extern  int   fNet(int);
extern  int   fPathErr(char *);
extern  int   fPhysical(int);
extern  int   fShared(int);
extern  int   getdrv(void);
extern  void  GetVars(struct sysVarsType *);
extern  int   mkdir(char *);
extern  void  parse(union REGS *, union REGS *);
extern  void  PutVars(struct sysVarsType *);
extern  void  rootpath(char *, char *);
extern  char  *strbscan(char *, char *);                                                      /* SM extern'd */
extern  void  sysdispmsg(union REGS *, union REGS *);
extern  void  sysloadmsg(union REGS *, union REGS *);

/* these are already declared extern in cds.h */
   char fGetCDS(int, struct CDSType *);
   char fPutCDS(int, struct CDSType *);


/*
 *  Internal Function Prototypes
 */
extern  int   main(int, char **);
extern  int   ParseIt(void);
extern  void  DoList(void);
extern  void  Delete(struct CDSType,int);
extern  void  Insert(struct CDSType,int);
extern  void  load_msg(void);
extern  void  display_msg(int, char *);
extern  void  DisplayOptionsExit(void);
extern  void  dispmsg_terminate(int, char *);
extern  void  Parser_Prep(char *);

