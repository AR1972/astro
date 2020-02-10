/* File: rtps.h - Runtime constants                                     */
/* NOTE: When making changes to this file, be sure to make equivalent   */
/*       changes to file rtps.inc                                       */
/* [2] - 07/16-90 timke    Define BINSAV_ constants for QB45C.		*/
/* [1] - 10/31/88 Markcha  Changed BINSAV_ and QLB_ constants for QBJ	*/

#undef RTPS_H
#define RTPS_H ON       /* remember that this file has been included */

/* [LINE] INPUT prompt flags */
#define FINP_QSupress 1
			/* set if "prompt" was followed by a comma,
			   not a semicolon, indicating "? " is not to be
			   output after prompt. */
#define FINP_CrLf 2
			/* set if INPUT was followed by optional ";",
			   meaning CrLf is not to be output when user
			   presses return. */
#define FINP_Prompt 4	/* set if the optional SDPrompt argument is included. */

/* File LOCK & UNLOCK Constants */
#define LOCK_UNLOCK 1		/* set if operation is UNLOCK, not LOCK */
#define LOCK_1stToLast 2	/* set if only part of file */
#define LOCK_RtMask 3		/* bits which get passed to runtime */
#define LOCK_Def1stArg 4	/* set if 1st arg defaulted */
#define LOCK_DefLastArg 8	/* set if last arg defaulted */

/* File Open mode, access, locking flags */
#define MD_SQI 1		/* for INPUT */
#define MD_SQO 2		/* for OUTPUT */
#define MD_RND 4		/* [for RANDOM] */
#define MD_APP 8		/* for APPEND */
#define MD_BIN 16		/* for BINARY */
#define MD_DEFAULT MD_RND	/* [for RANDOM] */
#define ACCESS_READ 1		/* READ */
#define ACCESS_WRITE 2		/* WRITE */
#define ACCESS_BOTH 3		/* READ WRITE */
#define LOCK_READ 0x30		/* LOCK READ */
#define LOCK_WRITE 0x20		/* LOCK WRITE */
#define LOCK_BOTH 0x10		/* LOCK READ WRITE */
#define LOCK_SHARED 0x40	/* LOCK SHARED */

/* DOS does not allow you to do a CD (change directory) to a path > 64,
 * but once you are in a directory whose path is 64 bytes long, DOS
 * will let you open a 64 byte relative path, making the maximum length
 * of a filename accessible by DOS = 128.  Since the runtime just has 1
 * static copy of a filename buffer, they decided to let running programs
 * access files whose full path > 64 (even though you can't get into the
 * lowest directory from DOS.
 *   The user interface chooses to limit the path to 64 for the directory,
 * and 12 for the filename (and 1 for a 0-byte terminator).  This is for
 * two reasons:
 *   - The directory that source files are saved in should be accessible by
 *     all programs (including command.com).  
 *   - There are several layers of functions in the user interface code that
 *     each need a copy of a filename.  The increased stack demands of going
 *     to 128 are prohibitive.
 */
#define FILNAML 128+1		/* max. filename length for runtime */
#define FILNAML64 64+12+1	/* max. filename length for user interface */

/* Runtime Value Types */
#define VT_I2 0x02		/* short integer */
#define VT_I4 0x14		/* long integer */
#define VT_R4 0x04		/* 32 bit real */
#define VT_R8 0x08		/* 64 bit real */
#define VT_SD 0x03		/* string */

/* Runtime bits for b$CtrlFlags */
#define NoSTACKINIT 0x01h   /* Doesn't call B$STACKINIT when set during
			      B$RUNINI or B$CHNINI calls. */

/* Extended Out of Memory error codes for b$errinfo. */
#define OMErr_OM   0x00h    /* generic OM error                               */
#define OMErr_NH   0x01h    /* out of near heap space ( DS > 64k )            */
#define OMErr_FH   0x02h    /* out of far heap space ( out of system memory )	*/
#define OMErr_STK  0x03h    /* out of stack space                            	*/
#define OMErr_Proc 0x04h    /* out of Procedure text table space ( > 64k )    */
#define OMErr_Mod  0x05h    /* out of Module text table space ( > 64k )       */
#define OMErr_Inc  0x06h    /* out of Include file text table space ( > 64k ) */
#define OMErr_Doc  0x07h    /* out of Document file text table space ( > 64k )	*/


/* File type constants for user-option path searching			*/
#define LIBFILE    0x00     /* search user-specified LIB path		*/
#define EXEFILE    0x06     /* search user-specified EXE path		*/
#define INCFILE    0x0c     /* search user-specified INCLUDE path	*/
#define HELPFILE   0x12     /* search user-specified HELP path		*/



/* QBI version & format revision constants (used by U.L. & Binary SAVE/LOAD)  */
/* if BINSAV_CBSPECHDR is changed, check MAX_SPECHDRSIZE in BinSav.ASM	      */

#define BINSAV_CBSPECHDR     0x0010
#if EI_EB
#define BINSAV_BASICVersion  0x0002	/* BASIC version number for EB */
#define BINSAV_REVISION_BYTE 0x01	/* binary save format revision byte */
#endif					//[1]

#if FL_JAPAN				//[2]
#define BINSAV_BASICVersion  0x0003	/*[1] BASIC version number for QBJ */
#define BINSAV_REVISION_BYTE 0x00	/*[1] binary save format revision byte */
#define QLB_BASICVersion     0x0003	/*[1] BASIC version QLB's compat. with */
#define QLB_REVISION_BYTE    0x00	/*[1] QuickLib format revision byte */
#endif

#if FL_TAIWAN				//[2]
#define BINSAV_BASICVersion  0x0004	/* BASIC version number for QBC */
#define BINSAV_REVISION_BYTE 0x00	/* binary save format revision byte */
#define QLB_BASICVersion     0x0004	/* BASIC version QLB's compat. with */
#define QLB_REVISION_BYTE    0x00	/* QuickLib format revision byte */
#endif

#ifndef BINSAV_BASICVersion		//[2] default version (American QB)
#define BINSAV_BASICVersion  0x0001	/* BASIC version number for QB */
#define BINSAV_REVISION_BYTE 0x00	/* binary save format revision byte */
#define	QLB_BASICVersion     0x0000	/* BASIC version QLB's compat. with */
#define	QLB_REVISION_BYTE    0x13	/* QuickLib format revision byte */
#endif					//[1]

/* Minimum space for NMALLOC buffer. */

#define NMALLOC_MIN	     0x0006	/* Minimum NMALLOC space needed */
