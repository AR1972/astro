/*          DOS Lempel-Ziv Data Decompression Module
            (C) Copyright 1989 by Microsoft
            written by David Dickman
            LZ code by Steve Zeck

  note:
 	this module is compiled twice.  with LZDLL defined to be linked
 	with the LZEXPAND.DLL windows code.  without this, to build the
 	LZCOPY.LIB dos library.  be sure things work in both cases.

*/

#ifdef LZDLL

#include <windows.h>
#include <winexp.h>

#else

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <io.h>
#include <dos.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <malloc.h>
#include <errno.h>

#endif

#include "lzcopy.h"


// Globals

static long cblOutSize;        // size in bytes of output file



// WriteOutBuf()
//
// NOTE! not to be confused with WriteOutBuf() in compress.c
//
// Dumps output buffer to output file.  Returns c cast as an int if the write
// is successful.  If the write is unsuccessful, returns LZERROR_BADOUTHANDLE or
// LZERROR_WRITE.
//
// returns:
//	< 0	error code (all errors are < 0)
//	> 0	success, char as described above
//
int WriteOutBuf(UCHAR uch,    // first character to be added to the empty
                              // buffer after the full buffer is written
                int doshDest) // DOS output file handle
{
   unsigned ucbToWrite,        // number of bytes to write from rguchOutBuf[]
            ucbWritten;        // number of bytes actually written

   // how many bytes should be written from rguchOutBuf[]?
   ucbToWrite = (unsigned)(puchOutBuf - rguchOutBuf);

   ucbWritten = FWRITE(doshDest, rguchOutBuf, ucbToWrite);
   
   if (ucbWritten != ucbToWrite)
      return LZERROR_WRITE;

#ifndef LZDLL
   if (FERROR())
         return LZERROR_WRITE;
#endif

   // keep track of bytes written
   cblOutSize += (long)ucbWritten;

   // reset write pointer to beginning of output buffer
   puchOutBuf = rguchOutBuf;

   // add the next character to the new buffer
   return ((int)(*puchOutBuf++ = uch));
}  // WriteOutBuf()



/***************************************************************************
 *
 * long DOSLZCopy()
 *
 * Copies input file with DOS handle doshSource to output file with DOS
 * handle doshDest.  If the input file has a LZ compressed file header, 
 * it is decompressed into the output file using LZ decoding.  If the 
 * input file does not have a LZ compressed file header, it is directly 
 * copied to the output file.
 *
 * in:
 *	doshSource	source file handle
 *	doshDest	dest file handle
 *
 * returns:
 *	# bytes copied	success
 *	< 0	failure for various LZERROR_ reasons
 *
 ***************************************************************************/

long DOSLZCopy(int doshSource, int doshDest)
{
   FH FHIn;          // structure holding header information from
                     // compressed input file (used for decoding)
   unsigned ucbRead;  // number of bytes actually read into rguchOutBuf[]
                     // during direct copy
   int f;            // holds LZDecode() return value

   cblOutSize = 0L;

   if (! InitBuffers())
      return LZERROR_GLOBALLOC;

   // check for LZ compressed file header
   if (! GetHdr((FH LZPTR *)&FHIn, doshSource) || ! ChkHdr(FHIn)
       || FHIn.uchAlgorithm != uchALG_LEMPEL_ZIV)
   {

      /************************ not compressed ****************************/

      // uncompressed file (straight DOS copy)
      // move to beginning of input file
      if (FSEEK(doshSource, 0L, SEEK_SET) != 0L)
      {
         FreeBuffers();
         return LZERROR_BADINHANDLE;
      }


      while ((ucbRead = FREAD(doshSource, (LPSTR)rguchOutBuf, ucbIOBufLen)) > 0U
#ifdef LZDLL
             && ucbRead != (unsigned)(-1))
#else
             && (FERROR() == 0))
#endif
      {
         if (FWRITE(doshDest, rguchOutBuf, ucbRead) != ucbRead) {

            FreeBuffers();

            return LZERROR_WRITE;
         }

         cblOutSize += (long)ucbRead;
      }

#ifdef LZDLL
      // here, ucbRead ==  0,		EOF (proper loop termination)
      //               == -1,		bad DOS handle
      if (ucbRead == (unsigned)(-1))
#else
      // here, FERROR() == 0U,		EOF (proper loop termination)
      //                != 0U,		bad DOS handle
      if (FERROR() != 0U)
#endif
      {
         FreeBuffers();
         return LZERROR_READ;
      }

   } else {

      /************************ compressed ****************************/

      // compressed file (LZ decompression)

      // move to beginning of compressed data and decompress file

      if (FSEEK(doshSource, (long)cbHdrSize, SEEK_SET) != (long)cbHdrSize)
      {
         FreeBuffers();
         return LZERROR_BADINHANDLE;
      }

      if ((f = LZDecode(doshSource, doshDest)) != LZDECODE_OK)
      {
         FreeBuffers();
         return f;
      }
   }

   // copy date and time stamp
   // doshSource and doshDest known to be valid DOS file handles

   CopyCreateDate(doshSource, doshDest);

   FreeBuffers();

   return cblOutSize;		// # bytes generated

}  // DOSLZCopy()
