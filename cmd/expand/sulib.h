/*
 * SULIB.H - Windows/DOS Setup common code
 *
 *           Copyright (c) 1988-1991 Microsoft Corporation
 *           Microsoft Confidential
 *           All Rights Reserved.
 *
 *  Modification History:
 *
 *  3/23/89  Toddla  combined common.h and prototypes into this file
 *               
 */

    #define FAR       far
    #define NEAR      near
    #define LONG      long
    #define VOID      void
    #define PASCAL    pascal
    typedef unsigned char   BYTE;
    typedef unsigned short  WORD;
    typedef unsigned long   DWORD;
    typedef int             BOOL;
    typedef char           *PSTR;
    typedef char NEAR      *NPSTR;
    typedef char FAR       *LPSTR;
    typedef int  FAR       *LPINT;
    int         _ret;
    unsigned    _error;
   #ifndef NULL
      #if (_MSC_VER >= 600)
         #define NULL   ((void *)0)
      #elif (defined(M_I86SM) || defined(M_I86MM))
         #define NULL   0
      #else
         #define NULL   0L
      #endif
   #endif

/* These macros are for near heap only */

    #define FOPEN(sz)                ((_ret=-1),(_error=_dos_open(sz,O_RDONLY,&_ret)),_ret)
    #define FCREATE(sz)              ((_ret=-1),(_error=_dos_creat(sz,_A_NORMAL,&_ret)),_ret)
    #define FCLOSE(fh)               ((_error=_dos_close(fh)))
    #define FREAD(fh,buf,len)        ((_error=_dos_read(fh,buf,len,&_ret)),_ret)
    #define FWRITE(fh,buf,len)       ((_error=_dos_write(fh,buf,len,&_ret)),_ret)
    #define FERROR()                 _error
    #define FSEEK(fh,off,i)          lseek(fh,(long)(off),i)

    #define ALLOC(n)                 malloc(n)
    #define FREE(p)                  free(p)
    #define SIZE(p)                  _msize(p)
    #define REALLOC(p,n)             realloc(p,n)

/* here are some macros for allocating and freeing far heap space */

    #define FALLOC(n)                _fmalloc(n)
    #define FFREE(n)                 _ffree(n)

/* flags for _lseek */

#define  SEEK_CUR 1
#define  SEEK_END 2
#define  SEEK_SET 0


/* M001: This path length constant has been made large enough to account for
 * drive letter, directory name, file name, terminating null, plus some pad
 * on both FAT and Installable File Systems (IFS), which may implement
 * long file names.
 */
#define MAXFILESPECLEN 260

#define ISEOF(c)     ((c) == '\0' || (c) == CNT_Z)
#define ISSEP(c)     ((c) == '='  || (c) == ',')
#define ISWHITE(c)   ((c) == ' '  || (c) == '\t' || (c) == '\n' || (c) == '\r')
#define ISFILL(c)    ((c) == ' '  || (c) == '\t')
#define ISEOL(c)     ((c) == '\n' || (c) == '\r' || (c) == '\0' || (c) == CNT_Z)
#define ISCRLF(c)    ((c) == '\n' || (c) == '\r')
#define ISNOISE(c)   ((c) == '"')
#define ISDIGIT(c)   ((c) >= '0' && (c) <= '9')
#define ISHEX(c)     (ISDIGIT(c) || ISCHAR(c))
#define ISCHAR(c)    (((c) >= 'A' && (c) <= 'Z') || ((c) >= 'a' && (c) <= 'z'))
#define SLASH(c)     ((c) == '/' || (c) == '\\')
#define DEVICESEP(c) ((c) == '/' || (c) == '\\' || (c) == '=' || (c) == ' ' || (c) == '\t')
#define UPCASE(c)    ((c) >= 'a' && (c) <= 'z' ? (c) & 0xdf : (c))
#define HEXVAL(c)    (ISDIGIT(c) ? (c) - '0' : UPCASE(c) - 'A' + 10)

#define CHSEPSTR                "\\"
#define EQUAL                   '='
#define SPACE                   ' '
#define LF                      0x0A
#define CR                      0x0D

/* DOS ERROR CODES */

#define ERROR_OK            0x00
#define ERROR_FILENOTFOUND  0x02    /* File not found */
#define ERROR_PATHNOTFOUND  0x03    /* Path not found */
#define ERROR_NOFILEHANDLES 0x04    /* Too many open files */
#define ERROR_ACCESSDENIED  0x05    /* Access denied */
#define ERROR_INVALIDHANDLE 0x06    /* Handle invalid */
#define ERROR_FCBNUKED      0x07    /* Memory control blocks destroyed */
#define ERROR_NOMEMORY      0x08    /* Insufficient memory */
#define ERROR_FCBINVALID    0x09    /* Memory block address invalid */
#define ERROR_ENVINVALID    0x0A    /* Environment invalid */
#define ERROR_FORMATBAD     0x0B    /* Format invalid */
#define ERROR_ACCESSCODEBAD 0x0C    /* Access code invalid */
#define ERROR_DATAINVALID   0x0D    /* Data invalid */
#define ERROR_UNKNOWNUNIT   0x0E    /* Unknown unit */
#define ERROR_DISKINVALID   0x0F    /* Disk drive invalid */
#define ERROR_RMCHDIR       0x10    /* Attempted to remove current directory */
#define ERROR_NOSAMEDEV     0x11    /* Not same device */
#define ERROR_NOFILES       0x12    /* No more files */
#define ERROR_13            0x13    /* Write-protected disk */
#define ERROR_14            0x14    /* Unknown unit */
#define ERROR_15            0x15    /* Drive not ready */
#define ERROR_16            0x16    /* Unknown command */
#define ERROR_17            0x17    /* Data error (CRC) */
#define ERROR_18            0x18    /* Bad request-structure length */
#define ERROR_19            0x19    /* Seek error */
#define ERROR_1A            0x1A    /* Unknown media type */
#define ERROR_1B            0x1B    /* Sector not found */
#define ERROR_WRITE         0x1D    /* Write fault */
#define ERROR_1C            0x1C    /* Printer out of paper */
#define ERROR_READ          0x1E    /* Read fault */
#define ERROR_1F            0x1F    /* General failure */
#define ERROR_SHARE         0x20    /* Sharing violation */
#define ERROR_21            0x21    /* File-lock violation */
#define ERROR_22            0x22    /* Disk change invalid */
#define ERROR_23            0x23    /* FCB unavailable */
#define ERROR_24            0x24    /* Sharing buffer exceeded */
#define ERROR_32            0x32    /* Unsupported network request */
#define ERROR_33            0x33    /* Remote machine not listening */
#define ERROR_34            0x34    /* Duplicate name on network */
#define ERROR_35            0x35    /* Network name not found */
#define ERROR_36            0x36    /* Network busy */
#define ERROR_37            0x37    /* Device no longer exists on network */
#define ERROR_38            0x38    /* NetBIOS command limit exceeded */
#define ERROR_39            0x39    /* Error in network adapter hardware */
#define ERROR_3A            0x3A    /* Incorrect response from network */
#define ERROR_3B            0x3B    /* Unexpected network error */
#define ERROR_3C            0x3C    /* Remote adapter incompatible */
#define ERROR_3D            0x3D    /* Print queue full */
#define ERROR_3E            0x3E    /* Not enough room for print file */
#define ERROR_3F            0x3F    /* Print file was deleted */
#define ERROR_40            0x40    /* Network name deleted */
#define ERROR_41            0x41    /* Network access denied */
#define ERROR_42            0x42    /* Incorrect network device type */
#define ERROR_43            0x43    /* Network name not found */
#define ERROR_44            0x44    /* Network name limit exceeded */
#define ERROR_45            0x45    /* NetBIOS session limit exceeded */
#define ERROR_46            0x46    /* Temporary pause */
#define ERROR_47            0x47    /* Network request not accepted */
#define ERROR_48            0x48    /* Print or disk redirection paused */
#define ERROR_50            0x50    /* File already exists */
#define ERROR_51            0x51    /* Reserved */
#define ERROR_52            0x52    /* Cannot make directory */
#define ERROR_53            0x53    /* Fail on Int 24H (critical error) */
#define ERROR_54            0x54    /* Too many redirections */
#define ERROR_55            0x55    /* Duplicate redirection */
#define ERROR_56            0x56    /* Invalid password */
#define ERROR_57            0x57    /* Invalid parameter */
#define ERROR_58            0x58    /* Net write fault */


/* LZCopy() return error codes.  These must be negative. */

#define LZERROR_BADINHANDLE   -1  /* Invalid input handle */
#define LZERROR_BADOUTHANDLE  -2  /* Invalid output handle */
#define LZERROR_READ          -3  /* Bad compressed file format */
#define LZERROR_WRITE         -4  /* Out of space for output file */
#define LZERROR_GLOBALLOC     -5  /* Insufficient memory for buffers */
#define LZERROR_GLOBLOCK      -6  /* Bad global handle */


