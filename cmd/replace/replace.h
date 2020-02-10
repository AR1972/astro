;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*
 *  This file contains the constants, globals, structure definitions,
 *  extern declarations, and macro definitions for the replace utility.
 *
 *  Date:       10-10-90
 */


#define  SELECT_SUPPORT 1      /* true to include special SELECT support code */


/*
 *  Errorlevel Codes
 */
#define ERRLEVELNEG1      -1
#define ERRLEVEL0          0           /* No error */
#define ERRLEVEL1          1           /* Invalid function number */
#define ERRLEVEL2          2           /* File not found */
#define ERRLEVEL3          3           /* Path not found */
#define ERRLEVEL8          8           /* Insufficient memory */
#define ERRLEVEL11        11           /* Invalid format */

/*
 *  Int 21h Functions 
 */
#define DETECT_SELECT     0x1C01       /* Int 2F, SELECT invoked? */
#define GETEXTERR         0x5900       /* Int 21, get extended err */

#define GETVEC_CRITERR    0x3524       /* Int 21,get vector,criterr */
#define GETVEC_CTLBRK     0x3523       /* Int 21,get vector,ctlbrk */
#define GETX_INSTALL      0xB700       /* Is Append/x installed? */
#define GETX_STATUS       0xB706       /* Get the Append/x status */
#define GETX_VERSION      0xB702       /* Is it the DOS Append/x? */
#define SETVEC_CRITERR    0x2524       /* Int 21,set vector,criterr */
#define SETVEC_CTLBRK     0x2523       /* Int 21,set vector,ctlbrk */
#define SETX_STATUS       0xB707       /* Set the Append/x status */

/*
 *  Int 21h Return Codes
 */
#define INSUFFMEM          8
#define NOERROR            0
#define NOMOREFILES       18
#define TARGETFULL        -1

/*
 *  Message Equates
 */
#define BLNK              ' '          /* For sublist.pad_char */
#define CARRY             0x0001
#define DEC_INPUT         161          /* Byte def for sublist.flags */
#define DOS_CON_INPUT     0xC1         /* Input for Y/N response */
#define EXT_ERR_CLASS     0x01         /* DOS Extended error class */
#define NO_INPUT          0x00         /* No input characters */
#define PARSE_ERR_CLASS   0x02         /* Parse error class */
#define RESERVED          0            /* Reserved byte field */
#define STDERR            0x0002       /* Standard error device handle */
#define STDOUT            0x0001       /* Std output device handle */
#define STR_INPUT         16           /* Byte def for sublist.flags */
#define SUBCNT0           0            /* 0 substitutions in message */
#define SUBCNT1           1            /* 1 substitution in message */
#define SUBLIST_LENGTH    11           /* Length of sublist structure */
#define UTILITY_CLASS     0x0ff        /* Utility message class */

/*
 *  Messages
 */
#define MSG_NOMEM         1            /* Insufficient memory */
#define MSG_INCOMPAT      2            /* Invalid parameter combo */
#define MSG_NOSOURCE      3            /* Source path required */
#define MSG_NONEREPL      4            /* No files replaced */
#define MSG_NONEADDE      5            /* No files added */
#define MSG_START         6            /* Press any key to continue */
#define MSG_ERRFNF        7            /* File not Found */
#define MSG_ERRPNF        8            /* Path not Found */
#define MSG_ERRACCD       9            /* Access denied */
#define MSG_ERRDRV        10           /* Invalid drive specification */
#define MSG_BADPARM       11           /* Invalid parameter */
#define MSG_WARNSAME      12           /* File cannot be copied...*/
#define MSG_ERRDSKF       13           /* Insufficient disk space */
#define MSG_REPLACIN      14           /* Replacing %1 */
#define MSG_ADDING        15           /* Adding %1 */
#define MSG_SOMEREPL      16           /* %1 file(s) replaced */
#define MSG_SOMEADDE      17           /* %1 file(s) added */
#define MSG_NONFOUND      18           /* No files found */
#define MSG_QREPLACE      19           /* Replace %1? (Y/N) */
#define MSG_QADD          20           /* Add %1? (Y/N) */
#define MSG_XTRAPARM      21           /* Too many parameters */
#define MSG_BADSWTCH      22           /* Invalid switch */

#define MSG_OPTIONS_FIRST 300
#define MSG_OPTIONS_LAST  309

/*
 *  Parse Equates
 */
#define A_SW              "/A"         /* For switch id /A */
#define P_SW              "/P"         /* For switch id /P */
#define R_SW              "/R"         /* For switch id /R */
#define S_SW              "/S"         /* For switch id /S */
#define U_SW              "/U"         /* For switch id /U */
#define W_SW              "/W"         /* For switch id /W */
#define Q_SW              "/?"
#define CAPRESULT         0x0001       /* Cap result by file table */
#define MAXPOSITION       2            /* Max positionals allowed */
#define MINPOSITION       1            /* Min positionals allowed */
#define NOCAPPING         0x0000       /* Do not capitalize */
#define OPT_FILESPEC      0x0201       /* Filespec & optional */
#define OPT_SWITCH        0x0001       /* Optional (switch) */
#define REQ_FILESPEC      0x0200       /* Filespec required */

/*
 *  Miscellaneous
 */
#define ARCHIVE           0x20         /* Archive bit file attribute */
#define BUF               512
#define FALSE             0
#define INACTIVE          0x0000       /* Append/x inactive status */
#define MAX               256
#define MAXMINUS1         255
#define NULL              0
#define SATTRIB           0
#define SUBDIR            0x10
#define TATTRIB           SUBDIR
#define TRUE              !FALSE
#define X_INSTALLED       0xffff       /* Set the Append/x status */


#if SELECT_SUPPORT                     /* Include only if SELECT calls REPLACE */

#define SELECT_PRESENT    MAXMINUS1    /* AL will = 0FFH after INT2F or 0 */
#define BLANK             32           /* Used to change Selects blanks */

struct selectfilestmp {
  char name[12];                       /* filename */
};

struct selectfiles {
  char name[13];                       /* filename */
};

struct selectfiles select_files[MAX] =
{
  0
};                                     /* array for SELECT list */

struct selectfilestmp select_files_temp[MAX] =
{
  0
};                                     /* array for SELECT list */

char          detect_select_flag  = 0;      /* T/F */
char          exclusion_list      = 0;      /* SELECT */
unsigned      num_select_files    = 0;

void          check_select(void);           /* global */
unsigned      check_select_list(struct filedata *);

#endif                  /* SELECT_SUPPORT */

struct filedata                        /* Files used in copy operations */
{
  char     attribute;
  unsigned time;
  unsigned date;
  long     size;
  char     name[15];
};

struct parm_list                       /* To be passed to Extd Create */
{                                      
  unsigned ea_list_offset;             /*  List structure (filled in) */
  unsigned ea_list_segment;
  unsigned number;                     /*  ID for iomode */
  char     format;                     /*  Format for iomode */
  unsigned iomode;                     /*  (Mainly sequential) */
} eaparm_list = { -1, -1, 1, 6, 2 };

/*
 *  Parse Structures
 */
struct p_parms  p_p;                   /* # of extras & pts to descrptn */
struct p_parmsx p_px;                  /* min/max parms & pts to controls */
struct p_control_blk p_con1;           /* 1st posit parm in cmd str */
struct p_control_blk p_con2;           /* 2nd posit parm in cmd str */
struct p_switch_blk  p_swit;           /* /A /P /R /S /U /W */
struct p_fresult_blk rslt1;            /* Result blk rtrnd from parser */
struct p_result_blk rslt2;             /* Result blk rtrnd from parser */
struct noval novals = {0};             /* Value list not used */


/*
 *  Global Variables
 */
union REGS inregs, outregs;            /* Define register variables */
struct SREGS segregs;                  /* Segment regs for Int21 */

char append_installed    = FALSE;
char attr_list[BUF]      = {0};        /* Buf for list of attributes */
char cmdln_invalid[64]   = {0};
char cmdln_switch[64]    = {0};        /* message, if needed     */
char disk_full           = FALSE;      /* RW Flag - Disk full on target */
char ea_flag             = FALSE;
char errfname[MAX]       = {0};
char errline[MAX]        = {0};        
char filename[MAX]       = {0};        /* RW Save area for filename being copied */
char fix_es_reg[2]       = {0};        /* Corrects es reg after type-"far" */
char not_valid_input     = TRUE;       /* Flag for Y/N message */
char only_one_valid_file = FALSE;      /* Flag indicating valid filecount */
char outline[MAX]        = {0};        
char p_path[64]          = {0};        /* Recvs path from parser */
char p_sfilespec[64]     = {0};        /* Recvs filespec from parser */
char source[MAXMINUS1]   = {0};
char target[MAXMINUS1]   = {0};
char target_full         = FALSE;      /* Check flag at exit */

unsigned char dbcs_search[3] = {0};

unsigned rstatus;                      /* return status of operations */
unsigned add        = FALSE;           /* /A switch */
unsigned counted    = 0;               /* Num replaced or added */
unsigned descending = FALSE;           /* /S switch */
unsigned length;
unsigned prompt     = FALSE;           /* /P switch */
unsigned readonly   = FALSE;           /* /R switch */
unsigned segment;
unsigned update     = FALSE;           /* /U switch */
unsigned waiting    = FALSE;           /* /W switch */
unsigned x_status   = 0;               /* Original append/x state */
unsigned _psp;

long oldint24;                         /* Rcv cntrl from, then ret to */


/*
 *  Extern Declarations
 */
extern  void main(int argc,char * *argv);
extern  void ParseIt(void);
extern  void GetFQPathNames(char *source, char*target);
extern  void FixSourcePath(char *source);
extern  void FixTargetPath(char *target);
extern  void FixPath(char *target, char *save);
extern  unsigned int dodir(char *source,char *target,struct filedata *files,unsigned int filecount);
extern  unsigned int doadd(char *source,char *target,struct filedata *files,unsigned int filecount);
extern  unsigned int findfile(struct filedata *files,struct filedata *file,unsigned int filecount);
extern  unsigned int docopy(char *sdir,char *tdir,struct filedata *file,unsigned int time,unsigned int date);
extern  int same(char *s,char *t);
extern  unsigned int dallocate(unsigned int s);
extern  unsigned int dfree(unsigned int s);
extern  unsigned int dcreate(char *n,unsigned int parm_value);
extern  unsigned int dopen(char *n);
extern  unsigned int ddelete(char *n);
extern  unsigned int dread(unsigned int h,unsigned int s,unsigned int o,unsigned int l);
extern  unsigned int dwrite(unsigned int handle,unsigned int segment,unsigned int offset,unsigned int length);
extern  unsigned int dclose(unsigned int h);
extern  unsigned int dchmod(char *n,unsigned int a);
extern  unsigned int dsearchf(char *s,struct filedata *t,unsigned int a);
extern  unsigned int dsearchn(struct filedata *t);
extern  unsigned int dexit(unsigned int s);
extern  void dta_save(char *t,unsigned int l);
extern  void dta_restore(char *t,unsigned int l);
extern  char getbyte(unsigned int msegment,unsigned int moffset);
extern  unsigned int getword(unsigned int msegment,unsigned int moffset);
extern  long getdword(unsigned int msegment,unsigned int moffset);
extern  void putbyte(unsigned int msegment,unsigned int moffset,char value);
extern  void putword(unsigned int msegment,unsigned int moffset,unsigned int value);
extern  void putdword(unsigned int msegment,unsigned int moffset,long value);
extern  void load_msg(void);
extern  unsigned int dcompare(void);
extern  void display_msg(int msg_num,char *outline);
extern  unsigned int check_appendx_install(void);
extern  unsigned int check_appendx(void);
extern  void set_appendx(unsigned int set_state);
extern  void parser_prep(char *source);
extern  void display_exit(int msg_num,char *outline,int error_code);
extern  void setup_ctl_brk(void);
extern  void setup_crit_err(void);
extern  void restore(void);
extern  void check_select(void);
extern  unsigned int check_select_list(struct filedata *fl);

extern  void crit_err_handler(void);            /* Assembler routine */
extern  void ctl_brk_handler(void);             /* Assembler routine */

extern  void parse(union REGS *, union REGS *);
extern  void sysdispmsg(union REGS *, union REGS *);
extern  void sysloadmsg(union REGS *, union REGS *);


/*
 *  Macro Definitions
 */

/***	GetCurrentDrive
 *
 *  	This macro gets the current drive and returns it in the
 *      location given if the status is NOERROR.
 *
 *	DEFINED AS A MACRO
 *
 * 	ENTRY	pszStore - pointer to location to store drive letter
 *              status   - status
 *              ifCat    - tells whether or not to concatenate string 2
 *              pszStr2  - pointer to string to concatenate
 *
 *	EXIT	pszStore - contains current drive 
 */

#define GetCurrentDrive(pszStore,status,ifCat,pszStr2)                       \
{                                                                            \
                                                                             \
    inregs.x.ax = 0x1900;                   /* Get current drive */          \
    intdos(&inregs,&outregs);               /* Int 21h */                    \
    if (status == NOERROR)                  /* If no error */                \
    {                                                                        \
      /* Insert current drive letter */                                      \
      pszStore[0] = (char)('A' + (outregs.x.ax & 0xff));                     \
      pszStore[1] = ':';                                                     \
      pszStore[2] = NULL;                                                    \
      if (ifCat)                                                             \
        strcat(pszStore,pszStr2);                                            \
    }                                                                        \
}


/***	GetCurrentDirectory
 *
 *  	This macro gets the current directory and returns it in the
 *      location given.  It also returns the status of the operation.
 *
 *	DEFINED AS A MACRO
 *
 * 	ENTRY	pszStore - pointer to location to store drive letter
 *              status   - status, to be returned
 *
 *	EXIT	pszStore contains current drive
 *		status contains result
 */

#define GetCurrentDirectory(pszStore,status)                                 \
{                                                                            \
    pszStore[2]   = '\\';                                                    \
    inregs.x.ax = 0x4700;                        /* get current dir */       \
    inregs.x.si = (unsigned)(&pszStore[3]);                                  \
    inregs.x.dx = pszStore[0] - 'A' + 1;                                     \
    intdos(&inregs,&outregs);                                                \
    if (status = (outregs.x.cflag & CARRY))      /* set status */            \
      status = outregs.x.ax;                     /* error case */            \
}


/***	AppendBackslash
 *
 *  	This macro appends a backslash character to the end of a string
 *      if one does not already exist.
 *
 *	DEFINED AS A MACRO
 *
 * 	ENTRY	pszStore - pointer to location to store drive letter
 *
 *	EXIT	pszStore has ending backslash
 */

#define AppendBackslash(pszStore)                                            \
{                                                                            \
    char *backptr;                       /* pointer to backslash */          \
                                                                             \
    /* If no backslash, then add backslash to end */                         \
    if ((backptr = com_strrchr(pszStore,'\\')) == NULL)                      \
      strcat(pszStore,"\\");                                                 \
    else                                                                     \
    {                                                                        \
      /* If backslash not last char, then add backslash to end */            \
      if (backptr != &pszStore[strlen(pszStore)-1])                          \
        strcat(pszStore,"\\");                                               \
    }                                                                        \
}


