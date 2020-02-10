/*          DOS Lempel-Ziv Data Decompression Module
            (C) Copyright 1989 by Microsoft
            written by Steve Zeck and David Dickman

   This module contains functions and variables common to the LZ command-line
   module (compress.c), the DOS library module (lzcopy.c), and the Windows
   DLL (lzexpand.dll).
*/


// Headers
///////////

#if defined(LZDLL)

#include <windows.h>
#include <winexp.h>

#else

#include <string.h>
#include <io.h>
#include <dos.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <malloc.h>

#endif

#include "doslzexp.h"


// Globals
//////////
UCHAR FAR *rguchInBuf;     // input buffer for reads
UCHAR FAR *puchInBufEnd;   // pointer past end of rguchInBuf[]
UCHAR FAR *puchInBuf;      // pointer to next byte to read from rguchInBuf[]

UCHAR FAR *rguchOutBuf;    // output buffer for writes
UCHAR FAR *puchOutBufEnd;  // pointer past end of rguchOutBuf[]
UCHAR FAR *puchOutBuf;     // pointer to last byte to write from rguchOutBuf[]

UCHAR FAR *rguchRingBuf;   // ring buffer

// flag indicating whether or not rguchInBuf[0], which holds the last byte
// from the previous input buffer, should be read as the next input byte
// (only used so that at least one unReadUChar() can be done at all input
// buffer positions)
BOOL bLastUsed;

unsigned ucbIOBufLen;      // length of input and output buffers
// (actually, rguchInBuf[] has length ucbIOBufLen + 1 since rguchInBuf[0] is
// used when bLastUsed is TRUE)




// copyCreateDate()
//
// Copies create date of file with DOS handle doshfrom to file with DOS
// handle doshTo.  Returns LZERROR_BADINHANDLE or LZERROR_BADOUTHANDLE as
// error codes.
//
// n.b., stream-style I/O routines like fopen() and fclose() may counter the
// intended effect of this function.  fclose() writes the current date to any
// file it's called with which was opened in write "w" or append "a" mode.
// One klugey way to get around this in order to modify the date of a file
// opened for writing or appending by fopen() is to fclose() the file and
// fopen() it again in read "r" mode.  Then pass it to copyCreateDate().
//
int CopyCreateDate(int doshFrom,       // date and time stamp origin handle
                   int doshTo)         // target handle
{
   // _dos function prototypes
   extern unsigned _dos_getftime(int, unsigned *, unsigned *);
   extern unsigned _dos_setftime(int, unsigned, unsigned);

#if defined(LZDLL)
   static
#endif
   unsigned uFrom_date, uFrom_time;

   if (_dos_getftime(doshFrom, &uFrom_date, &uFrom_time) != 0u)
      return((int)LZERROR_BADINHANDLE);

   if (_dos_setftime(doshTo, uFrom_date, uFrom_time) != 0u)
      return((int)LZERROR_BADOUTHANDLE);

   return(COPYCREATEDATE_OK);
}  // copyCreateDate()



// ReadInBuf()
//
// Reads input buffer from input file.  Returns c cast as an int if the read
// is successful.  If the read is unsuccessful, returns LZERROR_BADINHANDLE or
// END_OF_INPUT.
//
int ReadInBuf(UCHAR LZPTR *puch, // first character to be read from the new
                                 // input buffer after reading from disk
              int doshSource)    // DOS input file handle
{
   unsigned ucbRead;             // number of bytes actually read

   rguchInBuf[0] = *(puchInBufEnd - 1);

   if ((ucbRead = FREAD(doshSource, &rguchInBuf[1], ucbIOBufLen))
       != ucbIOBufLen)
#if defined(LZDLL)
      if (ucbRead == (unsigned)(-1))
#else
      if (_error != 0U)
#endif
         // given incorrect input file handle
         return((int)LZERROR_BADINHANDLE);
      else if (ucbRead > 0U)
         // read last ucbRead bytes of input file, change input buffer end to
         // account for shorter read
         puchInBufEnd = &rguchInBuf[1] + ucbRead;
      else  // (ucbRead == 0U)
         // couldn't read any bytes from input file (EOF)
         return(END_OF_INPUT);

   // reset read pointer to beginning of input buffer
   puchInBuf = &rguchInBuf[1];

   if (bLastUsed)
   {
      // return the last character from the previous input buffer
      bLastUsed = FALSE;
      return((int)(*puch = rguchInBuf[0]));
   }
   else
      // return the first character from the new input buffer
      return((int)(*puch = *puchInBuf++));
}  // ReadInBuf()



// GetHdr()
//
// Puts compressed file header information from file with DOS handle
// doshSource into the file info structure pointed to by pFH.  Returns TRUE
// if input file size was long enough to have contained valid header
// information.  Returns FALSE if file was too short to have contained header
// information, or if a read error occurred when trying to get the header.
//
BOOL GetHdr(FH LZPTR *pFH,       // pointer to destination info structure
            int doshSource)      // DOS input file handle
{
   UCHAR rguchBuf[cbHdrSize];    // storage for compressed file header
   long cblInSize;               // length of input file
   int i, j;

   // get file length to compare to size of compressed file header
   if ((cblInSize = FSEEK(doshSource, 0L, SEEK_END)) == -1L)
      return(FALSE);

   // make sure file length >= size of compressed file header
   if (cblInSize < (long)cbHdrSize)
      return(FALSE);

   // move to beginning of input file
   if (FSEEK(doshSource, 0L, SEEK_SET) != 0L)
      return(FALSE);

   // put entire compressed file header into rguchBuf[]
   if (FREAD(doshSource, (UCHAR FAR *)rguchBuf, cbHdrSize) != cbHdrSize)
      return(FALSE);

   // put compressed file signature into rguchMagic[] of struct
   for (i = 0; i < cbCompSigLength; i++)
      pFH->rguchMagic[i] = rguchBuf[i];

   // get algorithm label and version id
   pFH->uchAlgorithm = rguchBuf[i++];
   pFH->uchVersion = rguchBuf[i++];

   // extract uncompressed file size, LSB --> MSB (4 bytes in long)
   pFH->cbulUncompSize = 0UL;
   for (j = 0; j < 4; j++)
      pFH->cbulUncompSize |= ((ULONG)(rguchBuf[i + j])) << (8 * j);

   // add compressed file size
   pFH->cbulCompSize = (ULONG)cblInSize;

   return(TRUE);              // file header read ok
}  // GetHdr()



// ChkHdr()
//
// Examines the compressed file signature in the given header info structure
// to see if it matches the real signature.  Returns non-0 if the bytes
// match (compressed file with signature).  Returns 0 if the bytes don't
// match (uncompressed file).
//
int ChkHdr(FH FHIn)           // header info structure to check
{
   int i;
   // storage for FHIn's compressed file signature (used to make it an sz)
   char rgchBuf[cbCompSigLength + 1];

   // copy file info structure's compressed file signature into rgchBuf[] to
   // make it an sz
   for (i = 0; i < cbCompSigLength; i++)
      rgchBuf[i] = FHIn.rguchMagic[i];

   rgchBuf[i] = '\0';

#if defined(LZDLL)
   return(lstrcmp((LPSTR)rgchBuf, (LPSTR)szCompSig) == 0);
#else
   return(strcmp(rgchBuf, szCompSig) == 0);
#endif
}  // ChkHdr()



// InitBuffers()
//
// Allocates far heap space (DOS) or fixed global heap space (Windows) for
// the input, output, and ring buffers.  Initializes working far pointers
// used to manipulate these buffers.  Returns TRUE if there was sufficient
// heap space to allocate the buffers, FALSE if not.  The ring buffer must be
// allocated at the fixed size used during encoding for decoding to proceed
// properly.  However, the input and output buffers hog as much heap space as
// they can get, up to ucbIOBufMax.  ucbIOBufLen is set when the input and
// output buffers are successfully allocated.  n.b., InitBuffers() will try
// to allocate buffer space down to (ucbIOBufMax % ucbIOBufStep).
//
BOOL InitBuffers(void)
{
      // set up ring buffer, n.b., extra cbStrMax - 1 bytes used to
      // facilitate string comparisons near end of ring buffer
      if ((rguchRingBuf = (UCHAR FAR *)FALLOC(cbRingBufMax + cbStrMax - 1))
          == NULL)
         return(FALSE);

      for (ucbIOBufLen = ucbIOBufMax; ucbIOBufLen > 0U;
                                      ucbIOBufLen -= ucbIOBufStep)
      {
         // try to set up input buffer, n.b., extra byte because
         // rguchInBuf[0] holds last byte from previous input buffer
         if ((rguchInBuf = (UCHAR FAR *)FALLOC(ucbIOBufLen + 1U)) == NULL)
            continue;

         // try to set up output buffer
         if ((rguchOutBuf = (UCHAR FAR *)FALLOC(ucbIOBufLen)) == NULL)
         {
            FFREE(rguchInBuf);
            continue;
         }

         // buffers allocated OK, so set up working pointers
         InitBufferPtrs();

         return(TRUE);
      }

      return(FALSE);
}  // InitBuffers()





// LZDecode()
//
// Decompresses input file into output file using LZ algorithm.  Returns
// LZDECODE_OK if everything went alright.  If any errors occurred, returns
// LZERROR_BADINHANDLE, LZERROR_BADOUTHANDLE, LZERROR_READ, or LZERROR_WRITE.
// n.b., LZDecode() starts decoding from the current position of the input
// file pointer.
//
int LZDecode(int doshSource,  // compressed input file to decode
             int doshDest)    // decompressed output file
{
   int i,
       cb,                    // number of bytes to unpack
       f;                     // holds ReadUchar() return values
   int oStart,                // buffer offset for unpacking
       iCurBuf;               // ring buffer offset
   UCHAR uch1, uch2;          // input byte holders
   unsigned ufFlags = 0U;     // holds high order description byte

   bLastUsed = FALSE;

   for (i = 0; i < cbRingBufMax - cbStrMax; i++)
      rguchRingBuf[i] = uchBUF_CLEAR;

   iCurBuf = cbRingBufMax - cbStrMax;

   f = ReadUchar(uch1);
   if (f != (int)uch1)
      return((int)LZERROR_BADINHANDLE);

   FOREVER
   {
      if (f == END_OF_INPUT)        // EOF reached
         break;

      // high order byte counts the number of bits used in the low order byte
      if (((ufFlags >>= 1) & 0x100) == 0)
      {
         // set bit mask describing the next 8 bytes
         ufFlags = ((unsigned)uch1) | 0xff00;
         f = ReadUchar(uch1);
         if (f != (int)uch1)
            return((int)LZERROR_READ);
      }

      if (ufFlags & 1)
      {
         // just store the literal character in the buffer
         if ((f = writeUChar(uch1)) != (int)uch1)
            return((int)LZERROR_WRITE);

         rguchRingBuf[iCurBuf++] = uch1;
         iCurBuf &= cbRingBufMax - 1;
      }
      else
      {
         // extract the buffer offset and count to unpack
         f = ReadUchar(uch2);
         if (f != (int)uch2)
            return((int)LZERROR_READ);

         cb = (int)uch2;
         oStart = (cb & 0xf0) << 4 | (int)uch1;
         cb = (cb & 0x0f) + cbIndex;

         for (i = 0; i <= cb; i++)
         {
            uch1 = rguchRingBuf[(oStart + i) & (cbRingBufMax - 1)];

            if ((f = writeUChar(uch1)) != (int)uch1)
	       return((int)LZERROR_WRITE);

            rguchRingBuf[iCurBuf++] = uch1;
            iCurBuf &= cbRingBufMax - 1;
         }
      }

      f = ReadUchar(uch1);
   }

   // flush buffer to output file
   // (uchFLUSH isn't actually written to the output file)
   if ((f = WriteOutBuf(uchFLUSH, doshDest)) != (int)uchFLUSH)
      return((int)LZERROR_WRITE);

   return(LZDECODE_OK);
}  // LZDecode()
