/*
** expand.h -	 housekeeping for DOS Lempel-Ziv command-line decompression
**              modules
**
**              Copyright (c) 1989-1991 Microsoft Corporation
**              Microsoft Confidential
**              All Rights Reserved.
*/


#include "doslzexp.h"


// Constants
/////////////
#define EXIT_SUCCESS    0              // main() return codes
#define EXIT_FAILURE    1

#define WRITEHDR_OK     503            // WriteHdr() successful return value
#define LZENCODE_OK     504            // LZEncode() successful return value

#define CHAR_MASK       0xffL          // used to cast long's to char's in
                                       // file I/O

#define chPATH_SEP      '\\'

#define NO_DOSH         (-20)          // output DOS file handle used as flag
                                       // to prevent writing output to disk

#define NIL             cbRingBufMax   // index for root of binary search
                                       // trees

#define FILE_SIZE_BREAK 100000L        // boundary between 'small' files and
                                       // 'big' files for status display

// % steps between "% done" status displays
#define BIG_FILE_STEP   1           // uncompressed size >  FILE_SIZE_BREAK
#define SMALL_FILE_STEP 5           // uncompressed size <= FILE_SIZE_BREAK

#define pszTEMP_FILE_PREFIX   "DD"

/* Begin M001 */
#define	NO_ERROR 0
#define	OK  0
#define	EOL  '\0'
#define	CHAR_PERIOD '.'
#define	CHAR_UNDERSCORE '_'
#define  CHAR_EOF 0x1A
#define  CHAR_ESC 0x1B
#define  CHAR_ABORT CHAR_ESC
#define	ABORT -1
#define	MAX_ROW_LEN 80

/* File Information structure from \INC\FIND.INC.	This should be
 * the same as structure find_t, except that here we show the structure
 * of the reserved members.
 */
struct FIND_BUF
{
	char		FIND_BUF_DRIVE;			/* drive of search */
	char		FIND_BUF_NAME[11];		/* formatted name */
	char		FIND_BUF_SATTR;			/* attribute of search */
	unsigned	FIND_BUF_LASTENT;			/* LastEnt */
	unsigned FIND_BUF_DIRSTART;		/* DirStart */
	char		FIND_BUF_NETID[4];		/* Reserved for NET */
	char		FIND_BUF_ATTR;				/* attribute found */
	unsigned	FIND_BUF_TIME;				/* time */
	unsigned	FIND_BUF_DATE;				/* date */
	unsigned FIND_BUF_SIZE_L;			/* low(size) */
	unsigned FIND_BUF_SIZE_H;			/* high(size) */
	char		FIND_BUF_PNAME[13];		/* packed name */
};
/* End M001 */

// command-line directives:
// The directives should be single characters.  They appear in the
// instructions as defined here, but may be entered in upper or lower case on
// the command line.
#define chENCODE_TO_DIFFERENT_FILE    'e'
#define chDECODE_TO_DIFFERENT_FILE    'd'
#define chENCODE_IN_PLACE             'c'
#define chDECODE_IN_PLACE             'b'
#define chCOPY_STAMP                  's'
#define chQUERY_SAVINGS               'q'


// Macros
//////////
//  IPG - Made PrintInstructions() into two Macros because INTL strings too long
#define PrintInstructions1()           printf(pszHELP_TEXT1);\
                                       printf(pszHELP_TEXT2)
#define PrintInstructions2()           printf(pszHELP_TEXT3);\
                                       printf(pszHELP_TEXT4)

#define PrintBanner()                  printf(pszBANNER_TEXT)


#include "expand.pro"
#include "dosdir.pro"

