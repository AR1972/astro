/*          Data Decompression Program
            written by Steve Zeck and David Dickman

            Copyright (c) 1989-1991 by Microsoft Corporation
            Microsoft Confidential
            All Rights Reserved.

   This program will decompress files using the Lempel-Zev compress algorithm.
   A header is prepended to the file being compressed which contains a
   signature block and other information, like the size of the uncompressed
   file.  Other compression algorithms may be added fairly easily.  The
   compressed file header contains tags for the algorithm used in compression
   and the version of the program which compressed the file.  The last file
   modification date is preserved through compression and decompression.

   Fragmented Compression:
   ----------------------

   Compression of files which, even when compressed, occupy more than one
   floppy disk is handled in a primitive way.  Once compress runs out of disk
   space during a write, it asks for a new disk, and continues compression
   into a file of the same output name on the new disk.  Decompressing a
   fragmented output file takes more work.  The compressed output files must
   be appended together in their output order before decompressing the
   resulting unfragmented compressed file.  Of course, this requires a hard
   disk or some medium with sufficient space to hold the unfragmented
   compressed file.

   For example, suppose a large program called "bogusxl.exe" still occupies
   900Kb even when compressed.  It is to be shipped on 360Kb floppy disks.
   Working from a directory containing the original version of bogusxl.exe on
   drive c, bogusxl.exe might be compressed into bogusxl.out with the
   following command line:

   C:>compress e bogusxl.exe bogusxl.out

   compress will try to fit as much of the compressed data on each diskette
   as possible.  Assuming compress is handed empty 360Kb formatted floppy
   disks, bogusxl.out will be split into three fragments: 360Kb on disk 1,
   360Kb on disk 2, and 180Kb on disk 3 (in order).  The compressed data file
   on each disk is named bogusxl.out.  You might want to rename the files on
   the floppy disks.  e.g., bogusxl.1 on disk 1, bogusxl.2 on disk 2, and
   bogusxl.3 on disk 3.  Now we need to create an unfragmented version (e.g.,
   on hard drive c: on the purchaser's pc).

   <working from the target directory on c:>
   <insert disk 1 in drive a:>
   C:>copy a:bogusxl.1 c:

   <insert disk 2 in drive a:>
   C:>copy a:bogusxl.2 c:

   <insert disk 3 in drive a:>
   C:>copy a:bogusxl.3 c:

   Now to stitch together the three fragments in order.

   C:>copy bogusxl.1 /b + bogusxl.2 /b + bogusxl.3 /b bogusxl.out

   However, the date and time stamp of bogusxl.out no longer matches that of
   the original version of bogusxl.exe.  To fix this, use the 's' (set time
   and date) option of compress to copy bogusxl.1's stamp to bogusxl.out:

   C:>compress s bogusxl.1 bogusxl.out

   Now bogusxl.out is an unfragmented compressed version of bogusxl.exe, so
   it just needs to be decompressed.

   C:>compress d bogusxl.out bogusxl.exe

   And we can get rid of the compressed files...

   C:>del bogusxl.1
   C:>del bogusxl.2
   C:>del bogusxl.3
   C:>del bogusxl.out

   So now an uncompressed copy of bogusxl.exe with matching date and time
   stamp is on the user's hard drive.

   (A future version of compress may try to mitigate this multi-file
   rigamarole.)

****************************************************************************/
/*
   Notes:
   -----

   Fragmented compression needs to be tidied up.  e.g., a fragment number
   (UCHAR) could be added to the compressed file header to help automate
   fragmented decompression.  The 's' command line option is an especially
   klugey example of the messy fragmented compression handling.
*/

/* History:
 *
 * 08/07/91	M001	Added the following functionality:
 *						1) Do not allow source file to overwrite itself.
 *						2) If source file cannot be found, try file name with 3rd
 *							character of extension set = '_'.
 *						3) Display status messages as each file is expanded (like
 *							COPY, but display status even if just expanding 1 file):
 *								A:\EGA.SY_ -> C:\DOS\EGA.SYS
 *							Each message will appear on a new line, so if multiple
 *							files are expanded, the screen will scroll.
 *						4) Number of input parameters:
 *							a) = 0: Prompt for source file and destination file/path
 *							b) = 1: Prompt for destination file/path
 *							c) = 2:	Source must be file name
 *										Destination can be file name or existing
 *										 directory name (path)
 *							d) > 2:	First parameters (sources) must be file names
 *										Last parameter (destination) must be existing
 *										directory name (path)
 *						5) Destination file name:
 *							a) Can be explicitly specified when only 1 file is
 *								being expanded.
 *							b) When no destination file name is specified, the
 *								source file name is used as specified on command-line
 *								(i.e. if "EXPAND A:\EGA.SYS  C:\DOS" finds A:\EGA.SY_,
 *								it will display the status message
 *								"A:\EGA.SY_ -> C:\DOS\EGA.SYS"
 *								and create the file C:\DOS\EGA.SYS).
 *
 * 11/09/89	CC		Copied from compress.c and isolated
 *						expand features.	Retained former features since we
 *						may want them later (see #ifdef OLD's).
 */



// Headers
///////////
#include <stdio.h>
#include <stdlib.h>				/* M001 */
#include <string.h>
#include <ctype.h>
#include <conio.h>
#include <io.h>
#include <dos.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <malloc.h>

#include "expand.h"
#include "expmsgs.h"

#include <decomp.h>

// M001: Local Prototypes
//////////////////////////

static unsigned fnFileCopy(int fhDst,int fhSrc);
static BOOL PromptUser(int, char **);
static int Quit(int);
static int ProcessInput(int, char **);
static int GetString(char *, int);
static int GetSourceHandle(char *);
static BOOL IsSourceDest(char *, char *);

// Globals
///////////
static long cblInSize,                 // size in bytes of original file
            cblOutSize = 0L;           // size in bytes of output file

static int iCurMatch,                  // index and length of longest match
           cbCurMatch;                 // (set by LZInsertNode())

static int leftChild[cbRingBufMax + 1],    // left, right children and parents
           rightChild[cbRingBufMax + 257], // (these arrays make up the binary
           parent[cbRingBufMax + 1];       // search trees)

static char *pszInFile,                  // names of input and output files
            *pszOutFile;

static UCHAR uchAlgorithm,                // algorithm label
             uchVersion;                  // version id

static int doshIn,                     // input and output DOS file handles
           doshOut;

static int wOriginalDrive;

static int iFiles;					/* M001: # of files successfully expanded */


// DOS file handles are used for file references in this module since using
// FILE *'s for file references poses a problem.  fclose()'ing a file which
// was fopen()'ed in write "w" or append "a" mode stamps the file with the
// current date.  This undoes the effect of copyCreateDate().  Could also get
// around this fclose() problem by first fclose()'ing the file, and
// fopen()'ing it again in read "r" mode.
//
// Using file handles also allows us to bypass stream buffering, so read's
// and write's may be done with whatever buffer size is desired.  Also, the
// lower-level DOS file handle functions are faster than their stream
// counterparts.


#ifdef DBCS
/*
	Test if the character is DBCS lead byte

	input:	c = character to test
	output:	TRUE if leadbyte
*/

int	IsDBCSLeadByte(c)
unsigned char c;
{
	static unsigned char far *DBCSLeadByteTable = NULL;

	union REGS inregs,outregs;
	struct SREGS segregs;
	unsigned char far *p;

	if (DBCSLeadByteTable == NULL)
	{
		inregs.x.ax = 0x6300;		/* get DBCS lead byte table */
		intdosx(&inregs, &outregs, &segregs);
		FP_OFF(DBCSLeadByteTable) = outregs.x.si;
		FP_SEG(DBCSLeadByteTable) = segregs.ds;
	}

	p = DBCSLeadByteTable;
	while (p[0] || p[1])
	{
		if (c >= p[0] && c <= p[1])
			return TRUE;
		p += 2;
	}
	return FALSE;
}

/*
	Check if the character point is at tail byte

	input:	*str = strart pointer of the string
		*point = character pointer to check
	output:	TRUE if at the tail byte
*/

int	CheckDBCSTailByte(str,point)
unsigned char *str,*point;
{
	unsigned char *p;

	p = point;
	while (p != str)
	{
		p--;
		if (!IsDBCSLeadByte(*p))
		{
			p++;
			break;
		}
	}
	return ((point - p) & 1 ? TRUE : FALSE);
}
#endif


BOOL HelpSwitchPresent(int argc, char *argv[])
{
   int i;

	for (i = 1; i < argc; i++)
      if (strcmp(argv[i], pszHELP_SWITCH) == 0)
         return(TRUE);

   return(FALSE);
} // HelpSwitchPresent()



void CatPathAndFileName(char *pszPath, char *pszFileName)
{
   if (*pszFileName != '\0')
   {
#ifdef DBCS
      if ((! SLASH(pszPath[strlen(pszPath) - 1]) &&
          (pszPath[strlen(pszPath) - 1]) != ':') ||
          CheckDBCSTailByte(pszPath,&pszPath[strlen(pszPath) - 1]))
#else
      if (! SLASH(pszPath[strlen(pszPath) - 1]) &&
          (pszPath[strlen(pszPath) - 1]) != ':')
#endif
         strcat(pszPath, CHSEPSTR);

      strcat(pszPath, pszFileName);
   }
} // CatPathAndFileName()



char *ExtractFileName(char *pszPath)
{
   char *psz;

   for (psz = pszPath; *psz != '\0'; psz++)
      ;

#ifdef DBCS
   for ( ; psz >= pszPath && ((! SLASH(*psz) && *psz != ':') ||
   		CheckDBCSTailByte(pszPath,psz)) ; psz--)
#else
   for ( ; psz >= pszPath && ! SLASH(*psz) && *psz != ':'; psz--)
#endif
      ;

   return(++psz);
} // ExtractFileName()


char ExtractDrive(char *pszFileName)
{
   return(pszFileName[1] == ':' ?
          pszFileName[0] :
          (char)('a' + wOriginalDrive));
}



// WriteOutBuf()
//
// Dumps output buffer to output (compressed) file.  Prompts for new floppy
// disk if old one if full.  Continues dumping to output file of same name on
// new floppy disk.  The only error codes it may return are
// LZERROR_BADINHANDLE and LZERROR_BADOUTHANDLE (from copyCreateDate()).
//
int WriteOutBuf(UCHAR uch,    // first character to be added to the empty
                              // buffer after the full buffer is written
                int doshDest) // output file handle
{
   unsigned ucbToWrite,       // number of bytes to write from buffer
            ucbWritten,       // number of bytes actually written
            ucbTotWritten;    // total number of bytes written to output
   char chDrive;
   int f;                     // holds copyCreateDate() return value

   // how much of the buffer should be written to the output file?
   ucbTotWritten = ucbToWrite = (unsigned)(puchOutBuf - rguchOutBuf);
   // reset pointer to beginning of buffer
   puchOutBuf = rguchOutBuf;

   // do not write to an output file if given the 'q' command line option
   if (doshDest == NO_DOSH)
   {
      cblOutSize += (long)ucbTotWritten;
      return((int)uch);
   }

   while ((ucbWritten = FWRITE(doshDest, puchOutBuf, ucbToWrite)) != ucbToWrite)
   {
      // ran out of disk space
      if (DosRemoveable(chDrive = ExtractDrive(pszOutFile)))
         do
         {
            // shut down the old output file
            if ((f = CopyCreateDate(doshIn, doshDest)) != COPYCREATEDATE_OK)
               return(f);
            FCLOSE(doshDest);
            printf(pszINSERT_NEW_DISK, chDrive);
            getchar();
         // open a new output file of the same name
         } while ((doshDest = FCREATE(pszOutFile)) == -1);
      else
      {
         // shut down the old output file
         FCLOSE(doshIn);
         FCLOSE(doshDest);
         if (remove(pszOutFile))
            printf(pszNO_DELETE_OLD, pszOutFile);
         printf(pszNOT_ENOUGH_DISK, chDrive);
         return(LZERROR_WRITE);
      }

      // check to see if some buffer data remains to be written
      if (ucbWritten > 0
          && _error == 0U)
      {
         // account for partial writes
         ucbToWrite -= ucbWritten;
         puchOutBuf += ucbWritten;
      }
   }

   cblOutSize += (long)ucbTotWritten;

   // add the next character to the buffer
   return((int)(*puchOutBuf++ = uch));
}  // WriteOutBuf()



// WriteHdr()
//
// Writes compressed file header to output file.  Could add fragment number
// to write in header as argument to WriteHdr().  The only error codes that
// may be returned are LZERROR_BADINHANDLE and LZERROR_BADOUTHANDLE.
//
// header format:
//                8 bytes  -->   compressed file signature
//                1 byte   -->   algorithm label
//                1 byte   -->   version id
//                4 bytes  -->   uncompressed file size (LSB to MSB)
//
//       total = 14 bytes
//
int WriteHdr(int doshDest)    // output file handle
{
   int i,
       f;            // writeUChar return value
   UCHAR uch;        // temporary storage for next header byte to write

   // move to beginning of output file
   if (doshDest != NO_DOSH && FSEEK(doshDest, 0L, SEEK_SET) != 0L)
      return((int)LZERROR_BADOUTHANDLE);

   // write the compressed file signature
   for (i = 0; i < cbCompSigLength; i++)
   {
      uch = (UCHAR)(*(szCompSig + i));
      if ((f = writeUChar(uch)) != (int)(uch))
         return(f);
   }

   // write out algorithm label and version number
   if ((f = writeUChar(uchAlgorithm)) != (int)uchAlgorithm)
      return(f);
   if ((f = writeUChar(uchVersion)) != (int)uchVersion)
      return(f);

   // write out input file size (long ==> 4 bytes)
   // LSB first, MSB last
   uch = (UCHAR)(cblInSize & CHAR_MASK);              // LSB
   if ((f = writeUChar(uch)) != (int)uch)
      return(f);
   uch = (UCHAR)((cblInSize >> 8) & CHAR_MASK);
   if ((f = writeUChar(uch)) != (int)uch)
      return(f);
   uch = (UCHAR)((cblInSize >> 16) & CHAR_MASK);
   if ((f = writeUChar(uch)) != (int)uch)
      return(f);
   uch = (UCHAR)((cblInSize >> 24) & CHAR_MASK);      // MSB
   if ((f = writeUChar(uch)) != (int)uch)
      return(f);

   return(WRITEHDR_OK);
}  // WriteHdr()



// LZInitTree
//
// Initializes trees for LZ compression.
//
void LZInitTree(void)
{
   int i;

/* For i = 0 to cbRingBufMax - 1, rightChild[i] and leftChild[i] will be the
   right and left children of node i.  These nodes need not be initialized.
   Also, parent[i] is the parent of node i.  These are initialized to
   NIL (= N), which stands for 'not used.'
   For i = 0 to 255, rightChild[cbRingBufMax + i + 1] is the root of the tree
   for strings that begin with character i.  These are initialized to NIL.
   n.b., there are 256 trees.
*/

   for (i = cbRingBufMax + 1; i <= cbRingBufMax + 256; i++)
      rightChild[i] = NIL;

   for (i = 0; i < cbRingBufMax; i++)
      parent[i] = NIL;

   return;
}  // LZInitTree()



// LZInsertNode()
//
// Inserts string of length cbStrMax, rguchRingBuf[r..r + cbStrMax - 1], into one of
// the trees (rguchRingBuf[r]'th tree) and returns the longest-match position
// and length via the global variables iCurMatch and cbCurMatch.  If
// cbCurMatch == cbStrMax, removes the old node in favor of the new one,
// since the old one will be deleted sooner.  n.b., r plays a double role, as
// both tree node and position in buffer.
//
void LZInsertNode(int r)      // node to insert
{
   int  i, p, cmp;
   UCHAR FAR *key;

   cmp = 1;
   key = &rguchRingBuf[r];
   p = cbRingBufMax + 1 + key[0];

   rightChild[r] = leftChild[r] = NIL;
   cbCurMatch = 0;

   FOREVER
   {
      if (cmp >= 0)
      {
         if (rightChild[p] != NIL)
            p = rightChild[p];
         else
         {
            rightChild[p] = r;
            parent[r] = p;
            return;
         }
      }
      else
      {
         if (leftChild[p] != NIL)
            p = leftChild[p];
         else
         {
            leftChild[p] = r;
            parent[r] = p;
            return;
         }
      }

      for (i = 1; i < cbStrMax; i++)
         if ((cmp = key[i] - rguchRingBuf[p + i]) != 0)
            break;

      if (i > cbCurMatch)
      {
         iCurMatch = p;
         if ((cbCurMatch = i) >= cbStrMax)
            break;
      }
   }

   parent[r] = parent[p];
   leftChild[r] = leftChild[p];
   rightChild[r] = rightChild[p];

   parent[leftChild[p]] = r;
   parent[rightChild[p]] = r;

   if (rightChild[parent[p]] == p)
      rightChild[parent[p]] = r;
   else
      leftChild[parent[p]] = r;

   parent[p] = NIL;        // remove p
   return;
}  // LZInsertNode()



// LZDeleteNode()
//
// Deletes node p from tree.
//
void LZDeleteNode(int p)      // node to delete
{
   int  q;

   if (parent[p] == NIL)
      // node p is not in the tree
      return;

   if (rightChild[p] == NIL)
      q = leftChild[p];
   else if (leftChild[p] == NIL)
      q = rightChild[p];
   else
   {
      q = leftChild[p];
      if (rightChild[q] != NIL)
      {
         do
         {
            q = rightChild[q];
         } while (rightChild[q] != NIL);

         rightChild[parent[q]] = leftChild[q];
         parent[leftChild[q]] = parent[q];
         leftChild[q] = leftChild[p];
         parent[leftChild[p]] = q;
      }
      rightChild[q] = rightChild[p];
      parent[rightChild[p]] = q;
   }
   parent[q] = parent[p];

   if (rightChild[parent[p]] == p)
      rightChild[parent[p]] = q;
   else
      leftChild[parent[p]] = q;

   parent[p] = NIL;        // remove p
   return;
}  // LZDeleteNode()


/* int GetSourceHandle(char *pszSource)
 *
 * Get Source file handle.
 *
 * ENTRY:	pszSource -> Source filename
 *
 * EXIT: 	returns file handle; file handle = -1 if source file not found.
 *
 * NOTES:	If the pszSource filename can't be found, we try an alternate
 *				filename by substituting the '_' for the 3rd character of the
 *				pszSource filename extension.
 *
 * HISTORY: M001	Created.
 *
 */

int GetSourceHandle(char *pszSource)
{
	int  i;
	int  doshSrc;								/* Source file handle */
	char szAltSource[MAXFILESPECLEN];	/* Alternate source file name */

	if ( (doshSrc = FOPEN(pszSource)) != -1 )
		return(doshSrc);
	else
	{
		if( (i = strlen(pszSource)) > (MAXFILESPECLEN - 1) ||
			 i < 5 ||
			 pszSource[i-4] != CHAR_PERIOD )
		{
			return(doshSrc);
		}
		else
			strcpy(szAltSource, pszSource);

		/* Change 3rd character of extension to '_' */

		szAltSource[i-1] = CHAR_UNDERSCORE;

		if( (doshSrc = FOPEN(szAltSource)) == -1 )
			return(doshSrc);
		else
		{
			/* Alternate filename found, so reflect change in pszSource. */

			pszSource[i-1] = CHAR_UNDERSCORE;
			return(doshSrc);
		}
	}
}



BOOL GetIOHandles(char *pszSource,char *pszDest,int *pdoshSource,int *pdoshDest)
{
	if (pszSource != NULL)
	{
		/* M001: Find and open source file */
		if ((*pdoshSource = GetSourceHandle(pszSource)) == -1)
		{
			printf(pszNO_OPEN_INPUT, pszSource);
			return(FALSE);
		}

      // Move to the end of the input file to find its length,
      // then return to the beginning.
      if ((cblInSize = FSEEK(*pdoshSource, 0L, SEEK_END)) < 0L ||
          FSEEK(*pdoshSource, 0L, SEEK_SET) != 0L)
      {
         FCLOSE(*pdoshSource);
         printf(pszNO_READ_INPUT, pszSource);
         return(FALSE);
      }
   }
   else
      *pdoshSource = -1;

   // Set up output DOS file handle.
   if (pszDest != NULL)
	{

		/* M001: Verify that source and destination files are not the same file.
		 * Must do this BEFORE creating destination file, since if source
		 * == dest., the dest. create operation will truncate source file to
		 * zero length.
		 */
		if( IsSourceDest(pszSource, pszDest) )
		{
			printf(pszSOURCE_IS_DEST, pszSource);
			return(FALSE);
		}

      if ((*pdoshDest = FCREATE(pszDest)) == -1)
      {
         FCLOSE(*pdoshSource);
         printf(pszNO_OPEN_OUTPUT, pszDest);
         return(FALSE);
      }
   }
   else
      *pdoshDest = -1;

   return(TRUE);
} // GetIOHandles()


/* BOOL Decompress( char *pszSource, char *pszDest );
 *
 * Decompress source to destination.
 *
 * ENTRY: pszSource -> Source pathname
 * 		 pszDest   -> Destination pathname
 *
 * EXIT: TRUE if successful; FALSE otherwise.
 *
 * HISTORY: M001	Added progress messages.
 *
 */

BOOL Decompress(char *pszSource,
                char *pszDest)
{
   int doshSource,      // input file handle
		 doshDest,			// output file handle
		 fAppsComp, 		// 'apps' compression
		 f;
   FH FHIn;             // structure holding header information from
                        // compressed input file (used for decoding)
	long lWritten;

   // Set up source and dest file handles.

   if (! GetIOHandles(pszSource, pszDest, &doshSource, &doshDest))
      return(FALSE);

	// Determine if file was compressed using one of the special applications
	// division methods.

	fAppsComp = WReadHeaderInfo(doshSource) > 0;

   // make sure source file is in compressed format
   // make sure file shows Lempel-Ziv algorithm indicator

	if (! fAppsComp &&
		(! GetHdr(&FHIn, doshSource) || ! ChkHdr(FHIn) ||
		 (FHIn.uchAlgorithm != uchALG_LEMPEL_ZIV)))
   {
      BOOL rc = FALSE;

		printf(pszINPUT_NOT_COMPRESSED_FMT, pszSource); 		/* M001 */

		/* seek to beginning of file because of header read */
		if ( FSEEK(doshSource, 0L, SEEK_SET) == 0L )
		{
			/* M001: Display progress message. */
			printf( pszPROGRESS, strupr(pszSource), strupr(pszDest) );

			/* Copy file */
			rc = (BOOL) fnFileCopy(doshDest,doshSource);
		}
      FCLOSE(doshDest);
      FCLOSE(doshSource);

		if( rc == TRUE )
			iFiles++;		/* M001: Update file count */

      return(rc);
   }

   // set up globals for reading and writing
   pszInFile = pszSource;
   pszOutFile = pszDest;
   doshIn = doshSource;
   doshOut = doshDest;

	// compressed header found, so decompress file

	/* M001: Display progress message. */
	printf( pszPROGRESS, strupr(pszSource), strupr(pszDest) );

	if (fAppsComp)
	{
		f = LZDECODE_OK;
		lWritten = LcbDecompressToFile( doshSource, doshDest, LCBNIL, 0L, TRUE );
		if (lWritten < 0L)
			if (lWritten == rcWriteError)
				f = LZERROR_BADOUTHANDLE;
			else if (lWritten == rcReadError)
				f = LZERROR_BADINHANDLE;
			else
				f = LZERROR_READ;
	}

	if (!fAppsComp)
		f = LZDecode(doshSource, doshDest);

	if (f != LZDECODE_OK)
   {
      if (f == LZERROR_BADINHANDLE)
         printf(pszNO_READ_INPUT, pszSource);
      else if (f == LZERROR_BADOUTHANDLE)
         printf(pszNO_WRITE_OUTPUT, pszDest);
      else if (f == LZERROR_READ)
         printf(pszFORMAT_ERROR, pszSource);
      else  // (f == LZERROR_WRITE), actually this case will never occur
            // since the multi-disk version of writeOutBuf() can only
            // return LZERROR_BADINHANDLE or LZERROR_BADOUTHANDLE as error
            // conditions
         printf(pszOUT_OF_SPACE, pszDest);

      FCLOSE(doshSource);
      FCLOSE(doshDest);
      return(EXIT_FAILURE);
	}

	/* M001: At this point, source file has been expanded to destination, so
	 * update file count.
	 */
	iFiles++;

   f = CopyCreateDate(doshSource, doshDest);

   if (doshDest != NO_DOSH && f != COPYCREATEDATE_OK)
   {
      printf(pszNO_COPY_TIME);
      if (f == LZERROR_BADINHANDLE)
         printf(pszFROM_INPUT, pszSource);
      else  // (f == LZERROR_BADOUTHANDLE)
         printf(pszTO_OUTPUT, pszDest);

      FCLOSE(doshSource);
      FCLOSE(doshDest);
      return(FALSE);
   }

   FCLOSE(doshSource);
   FCLOSE(doshDest);
   return(TRUE);
} // Decompress()


/* unsigned fnFileCopy(int doshDest, int doshSource);
 *
 * Function copies files given a source and destination file handle.
 * will preserve source file date and time.
 *
 * ENTRY: doshDest   - destination file handle.
 *
 *        doshSource - Source file handle.
 *
 * EXIT: unsigned as to success or failure of file copy function.
 *
 *       Zero == Success.    Non Zero == Failure.
 *
 */
unsigned fnFileCopy(fhDst,fhSrc)
int    fhDst;
int    fhSrc;
{
   LPSTR       lpBuf = NULL;             /* copy buffer pointer */
   unsigned    size;
   unsigned    date;
   unsigned    time;
   int         f = 0;

   _dos_getftime(fhSrc,&date,&time);
   lpBuf = FALLOC(4096*sizeof(char));  // 4kb on far heap for a copy buffer.
   if (! lpBuf )
      return(FALSE);

   while (size = FREAD(fhSrc,lpBuf,4096)) {
      if (FWRITE(fhDst,lpBuf,size) != size) {
         /* write error? */
         f = FERROR();
	      if (f == ERROR_OK)
            f = ERROR_WRITE;
         break;
      }
   }
   FFREE(lpBuf);
   if (! f )
   {
      _dos_setftime(fhDst,date,time);
      f = TRUE;
   }
   return f;
}


/* BOOL IsSourceDest( char *pszSource, char *pszDest );
 *
 * Checks if source and destination files are the same file.
 *
 * ENTRY:	pszSource -> source filename
 * 			pszDest	 -> destination filename
 *
 * EXIT: 	TRUE if files are the same; FALSE otherwise.
 *
 * NOTES:	If DOS version >= 3.1, we use the DOS Truename function.	Else,
 * 			we do a DosFindFirst() and compare the reserved fields in the
 * 			returned FIND_BUF structure.	(Versions of DOS prior to 3.1 did
 * 			not support networks, so we can be fairly certain that the
 * 			reserved fields will be consistent.)
 *
 * 			If we experience an error during this function, we skip the
 * 			verification test and exit FALSE, as if the files were different.
 *
 * HISTORY: M001	Created.
 *
 */

BOOL IsSourceDest( char *pszSource, char *pszDest )
{
	int i;
	union REGS inregs, outregs;
	struct SREGS segregs;
	char szSrcAbs[MAXFILESPECLEN];		/* Absolute source pathname */
	char szDestAbs[MAXFILESPECLEN];		/* Absolute dest. pathname */
	struct find_t SrcFileInfo; 			/* Source file information buffer */
	struct find_t DestFileInfo;			/* Dest. file information buffer */
#ifdef DEBUG
	struct FIND_BUF * s;
#endif

	/* For DOS versions >= 3.1, use DOS Truename function */

	if( _osmajor > 3 || (_osmajor == 3 && _osminor >= 1) )
	{
		/* Get segment register values, and ensure they are the same (for safety:
		 * they should be the same since this is SMALL model).
		 */
		segread( &segregs );
		if( segregs.ds != segregs.es || segregs.ds != segregs.ss )
			return(FALSE);		/* Skip test */

		/* Get absolute source pathname */
		inregs.h.ah = 0x60;								/* DOS Truename function */
		inregs.x.si = (unsigned) pszSource;			/* Relative source path */
		inregs.x.di = (unsigned) szSrcAbs;			/* Buffer */
		intdosx( &inregs, &outregs, &segregs );
		if( outregs.x.cflag != 0 )
			return(FALSE); 	/* Skip test */

		/* Get absolute destination pathname */
		inregs.h.ah = 0x60;								/* DOS Truename function */
		inregs.x.si = (unsigned) pszDest;			/* Relative dest path */
		inregs.x.di = (unsigned) szDestAbs;			/* Buffer */
		intdosx( &inregs, &outregs, &segregs );
		if( outregs.x.cflag != 0 )
			return(FALSE); 	/* Skip test */

#ifdef DEBUG
		printf("Source = %s, Dest = %s\n", szSrcAbs, szDestAbs);
#endif

		if( strcmpi(szSrcAbs, szDestAbs) == 0 )
			return(TRUE);		/* Source == Destination */
	}
	else			/* DOS version >= 2.1 */
	if( (_osmajor == 3 && _osminor == 0) || (_osmajor == 2 && _osminor >= 1) )
	{
		/* Use _dos_findfirst(); compare file information structures. */

		/* Clear File Information Buffers */

		memset(&SrcFileInfo,  0, sizeof(struct find_t));
		memset(&DestFileInfo, 0, sizeof(struct find_t));

		/* Source file */
		if( _dos_findfirst(pszSource, _A_NORMAL, &SrcFileInfo) != 0 )
			return(FALSE); 	/* Skip test */

		/* Dest. file */
		if( _dos_findfirst(pszDest, _A_NORMAL, &DestFileInfo) != 0 )
			return(FALSE); 	/* Skip test */

#ifdef DEBUG
		s = (struct FIND_BUF *) &SrcFileInfo;
		printf(	"Source:  Drive=0x%hx  Name=%.11s  SAttr=0x%hx\n" \
					"LastEnt=0x%hx  DirStart=0x%hx  NetId=%.4s  Attr=0x%hx\n" \
					"Time=0x%hx  Date=0x%hx  Size=0x%lx  Name=%s\n\n",
					s->FIND_BUF_DRIVE,			/* drive of search */
					s->FIND_BUF_NAME,				/* formatted name */
					s->FIND_BUF_SATTR,			/* attribute of search */
					s->FIND_BUF_LASTENT,			/* LastEnt */
					s->FIND_BUF_DIRSTART,		/* DirStart */
					s->FIND_BUF_NETID,			/* Reserved for NET */
					SrcFileInfo.attrib,			/* attribute found */
					SrcFileInfo.wr_time,			/* time */
					SrcFileInfo.wr_date,			/* date */
					SrcFileInfo.size, 			/* size */
					SrcFileInfo.name );			/* packed name */
		s = (struct FIND_BUF *) &DestFileInfo;
		printf(	"Dest:    Drive=0x%hx  Name=%.11s  SAttr=0x%hx\n" \
					"LastEnt=0x%hx  DirStart=0x%hx  NetId=%.4s  Attr=0x%hx\n" \
					"Time=0x%hx  Date=0x%hx  Size=0x%lx  Name=%s\n\n",
					s->FIND_BUF_DRIVE,			/* drive of search */
					s->FIND_BUF_NAME,				/* formatted name */
					s->FIND_BUF_SATTR,			/* attribute of search */
					s->FIND_BUF_LASTENT,			/* LastEnt */
					s->FIND_BUF_DIRSTART,		/* DirStart */
					s->FIND_BUF_NETID,			/* Reserved for NET */
					DestFileInfo.attrib,			/* attribute found */
					DestFileInfo.wr_time,		/* time */
					DestFileInfo.wr_date,		/* date */
					DestFileInfo.size,			/* size */
					DestFileInfo.name );			/* packed name */
#endif

		/* Zero-out NET_ID members, in case they contain garbage. */
		for(i=0; i<4; i++)
		{
			((struct FIND_BUF *) &SrcFileInfo)->FIND_BUF_NETID[i] = '\0';
			((struct FIND_BUF *) &DestFileInfo)->FIND_BUF_NETID[i] = '\0';
		}

		if( memcmp(&SrcFileInfo, &DestFileInfo, sizeof(struct find_t)) == 0 )
			return(TRUE);		/* Source == Destination */
	}

	return(FALSE); 			/* Source != Destination */
}


/* BOOL PromptUser( int argc, char *$argv[] );
 *
 * Prompt the user for the source and destination files.
 *
 * ENTRY: $argv[] 	array of pointers to input parameters.
 *
 * EXIT: TRUE if user entered valid parameters; FALSE otherwise.
 * 		If TRUE, then $argv[1] -> source parameter
 * 					and  $argv[2] -> destination parameter
 *
 * HISTORY: M001	Created.
 *
 */

BOOL PromptUser( int argc, char *$argv[] )
{
	int i;

	/* Prompt for Source file, if needed. */

	if( argc == 1 )
	{
		printf(pszSRC_PROMPT1);
		while(TRUE)
		{
			printf(pszSRC_PROMPT2);

			i = GetString( $argv[1], MAX_ROW_LEN - sizeof(pszSRC_PROMPT2) );

			if( i == ABORT )
				return(FALSE);
			else
			if( i > 0 )
				break;
		}
	}

	/* Prompt for Destination file */

	printf(pszDEST_PROMPT1);
	while(TRUE)
	{
		printf(pszDEST_PROMPT2);

		i = GetString( $argv[2], MAX_ROW_LEN - sizeof(pszDEST_PROMPT2) );

		if( i == ABORT )
			return(FALSE);
		else
		if( i > 0 )
			break;
	}

	return(TRUE);
}


/* int Quit(int);
 *
 * Cleanup and exit program.
 *
 * ENTRY: Exit code.
 *
 * EXIT: None
 *
 * NOTES: NULL memory pointers are ignored by the free() and _ffree()
 * 		 functions, so we always exit through Quit(), even if the
 * 		 memory pointers haven't been initialized yet. (All the global
 * 		 memory pointers are static or external, so they are initialized
 *			 to NULL by default.)
 *
 * HISTORY: M001	Created.
 *
 */
int Quit(int rc)
{
	FreeBuffers();
	return(rc);
}


/* int GetString(char *chBuf, int iBufSiz)
 *
 * Read a line from STDIN into a buffer (chBuf).  The line is
 * terminated when the buffer is full (buffer size is specified by
 *	iBufSiz and includes a terminating NULL character) or when a newline
 * is encountered ('\n').	The line is terminated by a NULL character ('\0');
 * the newline ('\n') is not retained.	The function returns the number of
 * characters in the buffer chBuf, not including the NULL ('\0'), or ABORT
 * if the CHAR_ABORT character is encountered.
 *
 * ENTRY:	chBuf -> destination buffer
 * 			iBufSize = Maximum buffer size (including terminating NULL char.)
 *
 * EXIT: 	returns number of characters in the buffer chBuf, not including
 * 			the terminating NULL char., or ABORT if the CHAR_ABORT is
 * 			encountered.
 *
 * NOTES:	This function does not handle the case of strings which wrap.
 * 			So, in order for the BACKSPACE key to erase all the input
 * 			characters, the value of iBufSiz should limit input to 1
 * 			screen row.
 *
 * HISTORY: M001	Created.
 *
 */

int GetString(char *chBuf, int iBufSiz)
{
	int i;
	char chIn;
	int iBufEnd = iBufSiz - 1;

	/* Save characters in chBuf[] until buffer is full.	Don't
	 * exit loop until Carriage return or EOF or ABORT received.
	 */

	for( i=0;
		  ((chIn = (char) getch()) != CHAR_EOF) &&
		  (chIn != '\r') &&
		  (chIn != CHAR_ABORT); )
	{
		if( isprint(chIn) )
		{
			if( i < iBufEnd )
			{
				putch(chIn);
				chBuf[i++] = chIn;
			}
			else
				putch('\a');
		}
		else
		if( chIn == '\b' )
		{
			if( i > 0 )
			{
				putch('\b');
				putch(' ');
				putch('\b');
				chBuf[i--] = NULL;
				continue;
			}
		}
		else
			putch('\a');
	}
	chBuf[i] = EOL;		/* Zero-terminate string */

	if( chIn == CHAR_ABORT )
		i = ABORT;

	printf("\n");

#ifdef DEBUG0
	printf("i=%d, input=%s\n", i, chBuf);
#endif

	fflush(stdin);			/* Flush input buffer */

	return(i);
}


/* ProcessInput( int $argc, char *$argv[] )
 *
 * Decompress source file(s) to destination.
 *
 * ENTRY:	$argc		Number of input parameters
 * 			$argv[]	Array of ptrs to input parameters
 *
 * EXIT: 	EXIT_SUCCESS or EXIT_FAILURE.
 *
 * NOTES:
 *
 * HISTORY: M001	Created.
 *
 */

int ProcessInput( int $argc, char *$argv[] )
{
	int i;
	int rc = EXIT_SUCCESS;
	char rgchDestFileName[MAXFILESPECLEN];	/* destination file name */
	BOOL bIsDir;									/* IsDir() return value */
   long cblTotIn = 0L,
        cblTotOut = 0L;

   // initialize globals
   uchAlgorithm = uchALG_LEMPEL_ZIV;
   uchVersion = uchVER_1;

   // keep track of original drive
   wOriginalDrive = GetCurDrive();

	// Is the last parameter a directory?
	bIsDir = IsDir((LPSTR)$argv[$argc - 1]);

	if ($argc != 3 && ! bIsDir)
   {
		printf(pszNO_DEST_DIR, $argv[$argc - 1]);
		return(EXIT_FAILURE);
   }

   // Set up input, output, and ring buffers, and associated work
   // pointers.
   if (! InitBuffers())
   {
      printf(pszNOT_ENOUGH_MEM);
		return(EXIT_FAILURE);
   }

	if ($argc == 3 && ! bIsDir)
   {
		// CASE 1: expand a file to a different file

		if ( !Decompress($argv[1], $argv[2]) )
			rc = EXIT_FAILURE;
   }
	else
	{
		// CASE 2: expand a file or files to a file or files with the same name
		// in a different directory specified in $argv[$argc - 1].

		for (i = 1; i < $argc - 1; i++)
		{
			InitBufferPtrs();
			cblOutSize = 0L;
			strcpy(rgchDestFileName, $argv[$argc - 1]);
			CatPathAndFileName(rgchDestFileName, ExtractFileName($argv[i]));

			if ( !Decompress($argv[i], rgchDestFileName) )
			{
				rc = EXIT_FAILURE;
				break;
			}

			cblTotIn += cblInSize;
			cblTotOut += cblOutSize;
		}

#ifdef DEBUG0
		printf("\n       %ld bytes expanded to %ld bytes, savings: %d%%\n",
				  cblTotIn, cblTotOut, (int)(100 - 100 * cblTotOut / cblTotIn));
#endif
	}

	if( iFiles == 1 )
		printf( pszONE_FILE, iFiles);
	else
	if( iFiles > 1 )
		printf( pszTOTAL_FILES, iFiles);

	return(rc);
}


/* main(int argc, char *argv[] )
 *
 * Decompress source file(s) to destination.
 *
 * ENTRY:	argc		Number of input parameters
 * 			argv[]	Array of ptrs to input parameters
 *
 * EXIT: 	EXIT_SUCCESS or EXIT_FAILURE.
 *
 * NOTES:	This function primarily parses the command-line. The function
 * 			ProcessInput() does most of the real work.
 *
 * HISTORY: M001	main() now does command-line parsing; ProcessInput() does
 * 					most everything else.
 *
 */

int main(int argc, char *argv[])
{
	int  i;
	char *$argv[3];						/* Dummy argv[] for user prompted input. */
	char szSource[MAXFILESPECLEN];	/* Prompted source input. */
	char szDest[MAXFILESPECLEN];		/* Prompted destination input. */


	/* Parse command-line */

	if( HelpSwitchPresent(argc, argv) ) 	/* EXPAND /?: display help	*/
   {
      PrintInstructions1(); // IPG - These two lines used to be one macro
      PrintInstructions2(); // IPG - called PrintInstructions()
      		Quit(EXIT_FAILURE);
	}
	else
	if( argc == 1 || argc == 2	)	/* Missing parameters: prompt user */
	{
		/* Init. dummy argv[] so user input can be treated
		 * as command-line input parameters.
		 */

		$argv[0] = argv[0];		/* program name */

		if( argc == 2 )			/* Use cmd-line source input, if available */
			$argv[1] = argv[1];
		else
			$argv[1] = szSource; /* source input buffer */

		$argv[2] = szDest;		/* destination input buffer */

		/* Prompt user for source, $argv[1], and destination, $argv[2]. */
		if ( !PromptUser(argc, $argv) )
			Quit(EXIT_FAILURE);

		Quit( ProcessInput( 3, $argv ) );
	}
	else
	if( argc > 2 )
	{
		Quit( ProcessInput( argc, argv ) );
	}
	else
	{
		printf(pszINVALID);
		Quit(EXIT_FAILURE);
	}

	return(EXIT_SUCCESS);
}
