/*************************************************************************/
/*                                                                       */
/*  FILE:    Backup.H                                                    */
/*                                                                       */
/*  PURPOSE: For the BACKUP utility, this file has the required          */
/*           BACKUP defines, message numbers, structures,                */
/*           and subroutine declarations.                                */
/*                                                                       */
/*  LABEL:   Microsoft Confidential                                      */
/*           Copyright (c) Microsoft Corporation 1991                    */
/*           All Rights Reserved.                                        */
/*                                                                       */
/*************************************************************************/


/*
 *  Utility #DEFINES...
 */
#define DHLENGTH 139         /* length, in bytes, of a Disk Header */
#define DBLENGTH  70         /* length, in bytes, of a Directory Block */
#define FHLENGTH  34         /* length, in bytes, of a File Header */

#define BYTE  unsigned char
#define WORD  unsigned short
#define DWORD unsigned long

#define NOERROR 0
#define FILE_NOT_FOUND 2                         /* M009 */
#define NUL 0

#define YES 1                                    /* M011 */
#define NO  0                                    /* M011 */

#define FALSE 0
#define TRUE  !FALSE

#define BACKSLASH 0x5c

/* bit mask, ANDed with current attribute to turn off readonly bit */
#define READONLYOFF 254

/* File Attributes */
#define READONLY    0x01
#define HIDDEN      0x02
#define SYSTEM      0x04
#define VOLLABEL    0x08
#define SUBDIR      0x10
#define ARCHIVE     0x20
#define DEVICEATT   0x40

/* Sharing Modes */
#define DENYALL     0x10
#define DENYWRITE   0x20
#define DENYREAD    0x30
#define DENYNONE    0x40

/* Access Modes */
#define READACCESS  0x00
#define WRITEACCESS 0x01
#define READWRITE   0x02
#define NO_INHERIT  0x80          /* inheritance bit */
#define NO_CP_CHECK 0x100

#define SYNCHRONOUS    0x4000     /* OS/2 File Write-Through */
#define NOTSYNCHRONOUS 0x0000     /* OS/2 File Write-Through */
#define OPENDASD       0x8000     /* OS/2 Open a DASD device */
#define OPEN_IT        0x01
#define CREATE_IT      0x12


/* predefined handles */
#define STDIN  0x00
#define STDOUT 0x01
#define STDERR 0x02

/* LSEEK move methods */
#define BOFILE	0
#define CURRPOS 1
#define EOFILE	2

/* CHMOD functions */
#define GET 0
#define SET 1

/* Parser Match Flags */
#define SSTRING     0x2000
#define DATESTRING  0x1000
#define TIMESTRING  0x0800
#define FILESPEC    0x0200
#define DRIVELETTER 0x0100
#define OPTIONAL    0x0001

/* Parser Function Flags */
#define CAP_FILETABLE 0x0001
#define CAP_CHARTABLE 0x0002

#define LABELLEN 11

#define SETLOGICALDRIVE 0x440F


/* Append Functions */
#define INSTALL_CHECK  0xB700
#define NOT_INSTALLED  0
#define GET_APPEND_VER 0xB702
#define NET_APPEND     1
#define DOS_APPEND     2
#define GET_STATE      0xB706
#define SET_STATE      0xB707

#define APPEND_X_BIT   0x8000



#define ACTIONHOOK 2
#define CTRLC      1
#define CTRLBREAK  4

#define EOL           -1
#define QUOTED_STRING 9
#define RET_DATE      7
#define RET_TIME      8

#define CPSW_ACTIVE     1
#define CPSW_NOTACTIVE  0
#define GET_CPSW        0x3303

#define CARRY 0x0001



/*
 *  Utility-specific Definitions.
 */
#define ROOTDIR   0
#define BACKUPDIR 1

#define PUT_SEG(fp,seg)  (*((unsigned *)&(fp)+1)) = (unsigned) seg
#define PUT_OFF(fp,off)  (*((unsigned *)&(fp))) = (unsigned) off

#define MAXMSGLEN            160
#define PATHLEN               64
#define MAX_RETRY_OPEN_COUNT   5

/* Errorlevels */
#define RETCODE_NO_ERROR       0
#define RETCODE_NO_FILES       1
#define RETCODE_SHARE_ERROR    2
#define RETCODE_CTL_BREAK      3
#define RETCODE_ERROR          4
#define RETCODE_NEG_PROMPT     5


/*************************************************************************/
/*                 NOTE FROM PARSER SUBROUTINE !!!!!                     */
/*************************************************************************/
/* The SECONDS bits in the DOS Directory are in 2-second increments.     */
/* Therefore, div by 2,	take the integer portion and use in search.      */
/* Note that files can be backed up that were modified 1 second before   */
/* the time that a user enters, which is better than not backing up a    */
/* file that was modified at exactly that time.                          */
/*************************************************************************/


/*-----------------------*/
/*    BACKUP messages    */
/*-----------------------*/

/*
 *  Be sure to add a case statement with your
 *  message's #define to display_message in
 *  backup.c
 */
#define BAD_DOS_VER            1
#define INSUFF_MEMORY          2

#define INV_DRIVE              6
#define INV_DATE               7
#define INV_TIME               8

#define INVPARM               10
#define INV_PATH              11
#define NO_SOURCE             12
#define NO_TARGET             13
#define SRC_AND_TGT_SAME      14
#define ERR_EXEC_FORMAT       15
#define CANT_FIND_FORMAT      16
#define CANT_OPEN_LOGFILE     17
#define LOGGING               18
#define NOTLASTMSG            19
#define ERASEMSG              20
#define FERASEMSG             21
#define BUDISKMSG             22
#define SEQUENCEMSG           23
#define NONEFNDMSG            24
#define INSERTSOURCE          25
#define INSERTTARGET          26
#define CONFLICTMSG           27
#define LASTDISKMSG           28
#define INVTARGET             29
#define LASTNOTBACKUP         30
#define FDISKFULLMSG          31
#define LOGFILE_TARGET_FULL   32
#define PRESS_ANY_KEY         33
#define CRLF                  34
#define CANT_FORMAT_HARDFILE  35
#define SRCISTGT              40
#define TGTISSRC              41
#define DISK_UNFORMAT         42                 /* M011 */
#define NO_FORMAT_FND         43                 /* M011 */
#define INSERT_FORMAT         44                 /* M011 */
#define CONTINUE_NEW_DISK     45                 /* M011 */

#define	MSG_OPTIONS_FIRST    300
#define MSG_OPTIONS_LAST     310

/*
 *  Message Classes 
 */
#define EXTENDED    1
#define PARSEERROR  2
#define UTIL_MSG   -1

#define NOWAIT    0
#define WAIT      0xC8
#define YESNOWAIT 0xC1                           /* M011 */


/*
 *  CONTROL BLOCK FOR EACH BACKUP DISKETTE.
 *
 *  This structure will make up the first dh_dhlength bytes
 *  of the control.xxx file on the backup target.
 *  It identifies the disk as being a backup, and includes
 *  diskette sequence number and a flag indicating if this
 *  is the last target.
 */
#define LAST_TARGET     0xFF
#define NOT_LAST_TARGET 0x00

struct Disk_Header
{
    BYTE  DH_Length;              /* length, in bytes, of disk header */
    BYTE  DH_Identifier[8];       /* identifies disk as a backup */
    BYTE  DH_Sequence;            /* backup diskette seq num (1-255) */
    BYTE  DH_reserved [128];      /* save area for nothing */
    BYTE  DH_LastDisk;            /* indicates if this is last target */
                                  /*   0xFF if last target, 0 otherwise */
};


/*
 *  DIRECTORY BLOCK.
 *
 *  This structure is written to the control.xxx file at least once
 *  for each subdirectory, including the root, backed up. It contains
 *  the path to that directory, the number of files from that
 *  directory that are backed up on current target, and the offset
 *  of the next directory block on that diskette, if one exists.
 *  If there are no other directory blocks, it equals 0xffffffff.
 */
#define LAST_DB 0xFFFFFFFF

struct Dir_Block
{
    BYTE  DB_Length;         /* length, in bytes, of dir block */
    BYTE  DB_Path[63];       /* path of this directory, drive letter omitted */
    WORD  DB_NumEntries;     /* number of filenames currently in list */
    DWORD DB_NextDB;         /* offset of next dir block */
                             /*   0xffffffff if no more on current target */
};


/*
 *  CONTROL BLOCK FOR EACH BACKED UP FILE.
 *
 *  This structure will be repeated after the directory block once.
 *  For each file backed up from that directory, it contains the
 *  filename, directory information, and other necessary information.
 */
#define NOTLASTPART 0
#define LASTPART    1

#define NOTSUCCESSFUL 0
#define SUCCESSFUL    2

#define EXT_ATTR 4

struct File_Header
{
    BYTE  FH_Length;         /* length, in bytes, of file header */
    BYTE  FH_FName[12];      /* ascii file name (from directory) */
    BYTE  FH_Flags;          /* bit 0=1 if last part of file */
                             /* bit 1=1 if it is backed up successfully */
                             /* bit 2=1 if Extended Attributes are backed up */
    DWORD FH_FLength;        /* total length of the file (from directory) */
    WORD  FH_FSequence;      /* sequence #, for files that span */
    DWORD FH_BeginOffset;    /* offset in BACKUP.xxx where this segment begins */
    DWORD FH_PartSize;       /* length of part of file on current target */
    WORD  FH_Attribute;      /* file attribute (from directory) */
    WORD  FH_FTime;          /* time file was last modified (from directory) */
    WORD  FH_FDate;          /* date file was last modified (from directory) */
};


/*
 *  This is the structure that is used in the linked list of       
 *  directories that need to be processed (if /s option specified).
 */
struct node
{
    struct node *np;
    char        path[PATHLEN+15];
};


/*
 *  This is the structure that is used by the dos function
 *  "return country information".
 */
struct ctry_info_blk
{
    WORD  country_code;
    WORD  code_page;
    WORD  date_format;
#define USA  0
#define EUR  1
#define JAP  2
    BYTE  currency_symbol[5];
    WORD  thousands_separator;
    WORD  decimal_Separator;
    WORD  date_separator;
    WORD  time_separator;
    BYTE  currency_format;
    BYTE  num_sig_dec_dig_in_currency;
    BYTE  time_format;
    DWORD case_map_call;
    WORD  data_list_separator;
    WORD  reserved[5];
};


/*
 *  Substitution List for Message Retriever.
 *
 *  This structure is used by the DOS Message Handler service routines.
 */
#define SUBLIST_SIZE 11

#define LEFT_ALIGN        0x0
#define RIGHT_ALIGN       0x80

#define CHAR_FIELD_CHAR   0x0
#define CHAR_FIELD_ASCIIZ 0x10

/* unsigned binary to decimal character */
#define UNSGN_BIN_BYTE	  0x11
#define UNSGN_BIN_WORD	  0x21
#define UNSGN_BIN_DWORD   0x31

/* Message substitution list structure */
struct subst_list
{
    BYTE     sl_size1;       /* size of list */
    BYTE     zero1;          /* reserved */
    char far *value1;        /* time, date, or ptr to data item */
    BYTE     one;            /* n of %n */
    BYTE     flags1;         /* data type flags */
    BYTE     max_width1;     /* maximum FIELD width */
    BYTE     min_width1;     /* minimum FIELD width */
    BYTE     pad_char1;      /* character for pad FIELD */

    BYTE     sl_size2;       /* size of list */
    BYTE     zero2;          /* reserved */
    char far *value2;        /* time, date, or ptr to data item */
    BYTE     two;            /* n of %n */
    BYTE     flags2;         /* data Type flags */
    BYTE     max_width2;     /* maximum FIELD width */
    BYTE     min_width2;     /* minimum FIELD width */
    BYTE     pad_char2;      /* character for pad FIELD */
};


/*
 *  EXTENDED OPEN parameter list.
 */
#define EXTATTBUFLEN 4086

struct parm_list
{
    DWORD ext_attr_addr;
    WORD  num_additional;
    BYTE  id_io_mode;
    WORD  io_mode;
};


/*
 *  Subroutine Declarations and Extern Declarations.
 */
int cdecl sprintf(char *, char *, ...);
int cdecl printf(char *, ...);

void   alloc_buffer(void);
void   alloc_first_node(void);
struct node * alloc_node(unsigned int);
void   check_appendX(void);
void   alloc_seg(void);
void   build_ext(int);
void   change_levels(void);
void   check_date(WORD, BYTE, BYTE);
void   check_DOS_version(void);
void   check_drive_validity(int, char * []);
void   check_asj(void);
void   check_for_device_names(char * []);
void   check_last_target(void);
void   check_options_help(int, char *[]);
void   check_path_validity(char * []);
void   check_time(BYTE,BYTE,BYTE,BYTE);
void   clean_up_and_exit(void);
void   close_file(WORD);
void   close_out_current_target(void);
void   control_break_handler(void);
int    create_target(void);                      /* M009 */
void   datetime(void);
void   delete(char *);
int    delete_files(char);                       /* M009 */
long   disk_free_space(void);
void   display_it(int, WORD, int, BYTE, BYTE);
void   display_msg(int);
void   display_options_exit(void);
void   do_backup(void);
void   do_copy(void);
void   do_dos_error(WORD);
void   error_exit(int);
WORD   exist(char *);
WORD   extended_open(WORD, WORD, char far *, WORD);
void   file_sharing_error(void);
char   far * far_ptr(WORD, WORD);
void   findclose(WORD);
void   find_all_subdirs(void);
void   find_first(char *, WORD *, struct FileFindBuf *, WORD);
void   find_first_file(void);
void   find_format(int);
void   find_next(WORD, struct FileFindBuf *);
void   find_next_file(void);
void   find_the_first(void);
void   find_the_next(void);
int    format_target(void);
void   check_unformatted(void);
void   free_seg(unsigned);
WORD   get_attribute(char *);
void   get_current_dir(WORD, char *);
WORD   get_current_drive(void);
void   get_country_info(void);
void   get_diskette(void);
void   get_drive_types(void);
void   get_first_target(void);
void   get_hardfile(void);
void   get_next_target(void);
int    get_path(char *, int);
int    GetYesNo(void);                           /* M011 */
WORD   handle_open(char *, WORD);
WORD   handle_read(WORD, WORD, char far *);
WORD   handle_write(WORD, WORD, char far *);
void   init(void);
void   insert_node(char *);
WORD   ioctl(WORD);
void   label_target_drive(void);
DWORD  lseek(WORD, BYTE, DWORD);
int    main(int, char * []);
void   mark_as_last_target(void);
void   mark_as_not_last_target(void);
void   mark_files_read_only(void);
void   open_logfile(void);
void   open_source_file(void);
int    open_target(void);                        /* M009 */
void   parser(int,char * []);
void   parse_error(WORD, WORD);
void   parse_init(void);
void   process_switch(void);
void   put_disk_header(void);
void   put_new_db(void);
void   put_new_fh(void);
void   remove_last_backslash_from_BDS(void);
void   remove_node(void);
void   replace_volume_label(char *);
void   reset_archive_bit(char *);
void   restore_default_directories(void);
void   save_current_dirs(void);
void   see_if_it_should_be_backed_up(void);
void   set_attribute(char *, WORD);
void   set_vectors(void);
void   set_default_dir(void);
void   set_default_drive(WORD);
void   setsignal(WORD, WORD);
void   show_path(void);
char   *strcat(char *, const char *);
size_t strlen(const char *);
char   *strcpy(char *, const char *);
char   *strncpy(char *, const char *, unsigned int);
int    strncmp(const char *, const char *, unsigned int);
int    strcmp(const char *, const char *);
void   terminate(void);
void   update_db_entries(WORD);
void   update_fh_entries(void);
WORD   write_till_target_full(WORD, WORD);
void   write_to_control_file(char far *, WORD);
void   write_to_target(WORD);
void   xlat(char *, char *);

#ifdef DBCS
int    IsDBCSLeadByte(unsigned char);
int    CheckDBCSTailByte(unsigned char *, unsigned char *);
#endif


extern void sysloadmsg(union REGS *, union REGS *);
extern void update_logfile(union REGS *, union REGS *);
extern void sysdispmsg(union REGS *, union REGS *);
extern void parse(union REGS *, union REGS *);
extern void far pascal set_int24_vector(unsigned);



/*
 *  From COMSUB.H
 */

/* convert character to uppercase */
extern int com_toupper( unsigned char );    /* char to be converted */

/* search the first occurrence of a character in a string */
extern char *com_strchr( unsigned char *,   /* source string */
                         unsigned char );   /* character to be searched */

/* search the last character occurrence in a string */
extern unsigned char *com_strrchr( unsigned char *,   /* source string */
                                   unsigned char );   /* target string */

