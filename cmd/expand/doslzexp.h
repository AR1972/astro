/*
** doslzexp.h - housekeeping for DOS Lempel-Ziv decompression command-line
**              and library modules
**
**              Copyright (c) Microsoft Corporation 1989-1991
**              Microsoft Confidential
**              All Rights Reserved.
*/

#define LZPTR NEAR

#include "sulib.h"


// Constants
/////////////
#define FALSE           0              // booleans
#define TRUE            1

#define END_OF_INPUT       500         // readInBuf() EOF flag for input file

#define LZDECODE_OK        501         // LZDecode() successful return value
#define COPYCREATEDATE_OK  502         // copyCreateDate() successful return
                                       // value

#define cbRingBufMax    4096           // size of ring buffer
#define cbIndex         2              // encode string into position and
                                       // length
#define cbStrMax        16             // upper limit for match length

#define ucbIOBufMax     32768U         // max size of rguchInBuf[] used for
                                       // reads and rguchOutBuf[] used for
                                       // writes
#define ucbIOBufStep    512U           // step down value used in attempting
                                       // to allocate rguchInBuf[] and
                                       // rguchOutBuf[]

#define uchBUF_CLEAR ((UCHAR) ' ')  // rguchRingBuf[] initializer

#define szCompSig       "SZDD\x88\xf0\x27\x33"  // compressed file signature:
                                                // "SZDDˆð'3"
#define cbCompSigLength 8              // length of signature (bytes)
                                       // (no '\0' terminator)

#define uchFLUSH     ((UCHAR) 'F')  // dummy character used to flush
                                       // outBuf[] to output file

#define uchALG_LEMPEL_ZIV  ((UCHAR) 'A')  // Lempel-Ziv algorithm label
#define uchVER_1           ((UCHAR) '\0') // start with version 1

// length of entire compressed file header (used as offset to start of
// compressed data)
#define cbHdrSize       14

// (14 == cbCompSigLength * sizeof(char) + 2 * sizeof(UCHAR) + sizeof(long))
//     == cbCompSigLength + algorithm + version + uncompressed length



// Macros
//////////
#define FOREVER         for(;;)

// read a character (buffered) from input file - returns character read cast
// as int if successful, or one of readInBuf()'s error codes if unsuccessful
#define ReadUchar(uch)        ((puchInBuf < puchInBufEnd) ?\
                               (int)(uch = *puchInBuf++) :\
                               ReadInBuf((UCHAR LZPTR *)&uch, doshSource))

// put at most one character back into the buffered input,
// n.b., can be used at most (puchInBuf - &rguchInBuf[1]) times (i.e., at
// beginning of buffer, can only be used once)
#define UnreadUchar()         ((puchInBuf == &rguchInBuf[1]) ?\
                               (bLastUsed = TRUE) :\
                               (int)(UCHAR LZPTR *)(--puchInBuf))

// write a character (buffered) to output file - returns given character
// argument cast as int if successful, or one of WriteOutBuf()'s error codes
// if unsuccessful
#define writeUChar(uch)       ((puchOutBuf < puchOutBufEnd) ?\
                               (int)(*puchOutBuf++ = uch) :\
                               WriteOutBuf(uch, doshDest))

#define InitBufferPtrs()      {  puchInBufEnd = &rguchInBuf[1] + ucbIOBufLen;\
                                 puchInBuf = &rguchInBuf[1] + ucbIOBufLen;\
                                 puchOutBufEnd = rguchOutBuf + ucbIOBufLen;\
                                 puchOutBuf = rguchOutBuf;\
                              }
                           

// free the far/global heap space used by the buffers
#define FreeBuffers()         {  FFREE(rguchInBuf);\
                                 FFREE(rguchOutBuf);\
                                 FFREE(rguchRingBuf);\
                              }


// Types
/////////
typedef unsigned char UCHAR;
typedef unsigned long ULONG;

// n.b., the compressed file header does not contain the file size of the
// compressed file since this is readily obtainable through filelength() or
// lseek().  The file info structure, however, does contain the compressed
// file size.
typedef struct tagFH          // file info structure (compressed file header
                              // + some additional information)
{
    UCHAR rguchMagic[cbCompSigLength]; // magic array of compressed file
                                       // signature bytes

    UCHAR uchAlgorithm;       // algorithm label
    UCHAR uchVersion;         // version id

    ULONG cbulUncompSize;     // uncompressed file size
    ULONG cbulCompSize;       // compressed file size (not stored in header)
} FH;



// Globals available to outside modules
////////////////////////////////////////
extern UCHAR FAR *rguchInBuf;       // input buffer for reads
extern UCHAR FAR *puchInBufEnd;     // pointer past end of rguchInBuf[]
extern UCHAR FAR *puchInBuf;        // pointer to next byte to read from
                                    // rguchInBuf[]

extern UCHAR FAR *rguchOutBuf;      // output buffer for writes
extern UCHAR FAR *puchOutBufEnd;    // pointer past end of rguchOutBuf[]
extern UCHAR FAR *puchOutBuf;       // pointer to last byte to write from
                                    // rguchOutBuf[]

extern UCHAR FAR *rguchRingBuf;     // ring buffer

// flag indicating whether or not rguchInBuf[0], which holds the last byte
// from the previous input buffer, should be read as the next input byte
// (only used so that at least one unReadUChar() can be called at all input
// buffer positions)
extern BOOL bLastUsed;

extern unsigned ucbIOBufLen;        // length of input and output buffers
// (actually, rguchInBuf[] has length ucbIOBufLen + 1 since rguchInBuf[0] is
// used when bLastUsed is TRUE)



#include "doslzexp.pro"
