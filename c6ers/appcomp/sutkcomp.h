/* TS = none */
/*
**  SUTKCOMP.H  --  internal header for compression module for Setup Toolkit.
*/

#include  "os2types.h"
#include  "retcodes.h"


/* The compressed file header format can be found in doc\header.doc. */


#define  cbMagic        8
#define  rgbMagicValue  "KWAJ\x88\xf0\x27\xd1"


/* bit masks for accessing flags in rgfsFlags[] */
#define  bmAnotherUsFlag   0x8000

/* First Bank of FsFlags */
#define  bmSrcLength       0x0001
#define  bmChecksum        0x0002
#define  bmArgs            0x0004
#define  bmBaseName        0x0008
#define  bmExtension       0x0010
#define  bmText            0x0020
#ifdef EAS
#define  bmEAs             0x0040
#endif /* EAS */

  /* global header info variables */
extern  SHORT   vwAlgType;
extern  LONG    vlcbSrcLength;
extern  BOOL    vfChecksum;
extern  USHORT  vusChecksum;
extern  USHORT  vcbArgs;
extern  BYTE *  vrgbArgs;
extern  CHAR    vszBaseName[9];
extern  CHAR    vszExtension[4];
extern  SZ      vszText;
extern  USHORT  vcbText;
#ifdef EAS
extern  CHAR    far *vfrgbEAs;
extern  CHAR    *vrgbEAs;
extern  USHORT  vuscbEAs;
#endif /* EAS */

  /* other global variables needed in toolkit layer */
extern  LONG    vlibChecksum;               /* patch location of usChecksum */
extern  LONG    vlibSrcLength;              /* patch location of lcbSrclength */


/*
**  File I/O macros.
**  FWriteXX returns TRUE if it works, FALSE on errors.
**    (Note:  fh == -1 is not considered an error in FWriteXX; it always
**    returns TRUE.  This happens when just calculating a length.)
*/

#define EOF  (-1)

extern  BYTE  _bWrite;
#define FWriteByte(fh, b) \
      ((BOOL)((fh == -1) ? TRUE : (_bWrite = b, write(fh, &_bWrite, 1) == 1)))
#define FWriteNBytes(fh, rgb, n) \
      ((BOOL)((fh == -1) ? TRUE : (write(fh, rgb, n) == (int)n)))
#define FWriteUs(fh, us) \
      ((BOOL)(FWriteByte(fh, (BYTE)us) && FWriteByte(fh, (BYTE)(us >> 8))))
#define FWriteUl(fh,ul) \
      ((BOOL)(FWriteByte(fh, (BYTE)ul) && FWriteByte(fh, (BYTE)(ul >> 8)) \
      && FWriteByte(fh, (BYTE)(ul >> 16)) && FWriteByte(fh,(BYTE) (ul >> 24))))


/***** EXTERN PROCS *****/

  /* routines from sutkcomp.c */
extern  SHORT   far  WReadHeaderInfo(int fhSrc);
extern  SHORT   far  CbWriteFileHeader(int fhDest);
extern  BOOL    far  FFreeHeaderInfo(void);
extern  BOOL         FReadNBytes(int fh, BYTE *pb, int n);
extern  BOOL         FReadUs(int fh, USHORT *pw);
extern  BOOL         FReadUl(int fh, ULONG *pul);
extern  BOOL         FPatchUs(int fh, LONG lib, USHORT us);
extern  BOOL         FPatchUl(int fh, LONG lib, ULONG ul);
extern  USHORT       CbReadFar(int fh, BYTE far * fp, USHORT cb);

#define FReadByte(fh, pb)  FReadNBytes(fh, pb, 1)


  /* routines from compress.c */
extern  LONG    far  LcbCompressToFile(SHORT wAlgType, int fhSrc, int fhDest,
                           LONG lcbDestMax);
extern  LONG    far  LcbCalculateCompressedLength(SHORT wAlgType, int fhSrc,
                           LONG lcbDestMax);


  /* routines from decomp.c */
extern  LONG    far  LcbDecompFile(int fhSrc, int fhDest, LONG lcbDestMax,
                           LONG libStart, BOOL fHeaderAlreadyRead,
                           BYTE far * fpbBuf, LONG lcbBuf, PFNWFROMW pfn,
                           int cProgTicks, PFNWFROMW pfnYield);
extern  LONG    far  LcbDecompressToFile(int fhSrc, int fhDest, LONG lcbDestMax,
                           LONG libStart, BOOL fHeaderAlreadyRead);
extern  LONG    far  LcbDecompressToBuffer(int fhSrc, BYTE far * fpbBuf,
                           LONG lcbBuf, LONG libStart, BOOL fHeaderAlreadyRead);
extern  LONG    far  LcbCalculateDecompressedLength(int fhSrc,
                           BOOL fHeaderAlreadyRead);


  /* routines from zk1\zk1comp.c */
extern  LONG    Lcb_ZK1_CompressToFile(int fhSrc, int fhDest,
                           LONG lcbDestMax);

  /* routines from zk1\zk1deco.c */
extern  LONG    Lcb_ZK1_DecompressToFile(int fhSrc, int fhDest, LONG lcbDestMax,
                           LONG libStart);
extern  LONG    Lcb_ZK1_DecompressToBuffer(int fhSrc, BYTE far * fpbBuf,
                           LONG lcbBuf, LONG libStart);

  /* routines from zk1\zk1share.c */
extern  void    WriteOutBuff(BYTE bToAdd);
extern  BOOL    FAllocateGlobals(LONG lcbDestMax, BOOL fCompressing);
extern  void    FreeGlobals(void);
extern  USHORT  ReadByte(int fh);

  /* routines from jjj1\jjj1comp.c */
extern  LONG    Lcb_JJJ1_CompressToFile(int fhSrc, int fhDest,
                           LONG lcbDestMax);

  /* routines from jjj1\jjj1deco.c */
extern  LONG    Lcb_JJJ1_DecompressToFile(int fhSrc, int fhDest,
                           LONG lcbDestMax, LONG libStart,
                           BYTE far * fpbBuf, LONG lcbBuf);
extern  LONG    Lcb_JJJ1_DecompressToBuffer(int fhSrc, BYTE far * fpbBuf,
                           LONG lcbBuf, LONG libStart);

  /* routines from nc_xor1\nc_xor1.c */
extern  LONG    Lcb_NC_XOR1_StraightCopy(int fhSrc, int fhDest, LONG lcbDestMax,
                           BYTE far * fpbBufDest, LONG libStart, BOOL fXor);

  /* Buffered Read/Write Stuff */
#define WriteByte(b) \
        if (fpbOutBufCur < fpbOutBufEnd) *fpbOutBufCur++=b; else WriteOutBuff(b);
#define WriteUs(us) \
        WriteByte((BYTE)us); WriteByte((BYTE)us >> sizeof(BYTE))
#define WriteUl(ul) \
        WriteByte((BYTE)ul); \
        WriteByte((BYTE)ul >> sizeof(BYTE));  \
        WriteByte((BYTE)ul >> (2 * sizeof(BYTE))); \
        WriteByte((BYTE)ul >> (3 * sizeof(BYTE)))

extern  BOOL        fDestFull;
extern  BOOL        fWriteError;
extern  BYTE far *  fpbBufDest;
extern  int         fhSrcGlobal;
extern  LONG        lcbDest;
extern  LONG        lcbDestStop;
extern  LONG        lcbSkipOut;
extern  int         fhDestGlobal;

extern  BYTE far *  fpbOutBuf;
extern  BYTE far *  fpbOutBufCur;
extern  BYTE far *  fpbOutBufEnd;

extern  BYTE far *  fpbInBuf;
extern  BYTE far *  fpbInBufCur;
extern  BYTE far *  fpbInBufEnd;

extern  BOOL  FAllocateReadWriteGlobals(LONG lcbDestMax);
extern  void  FreeReadWriteGlobals(void);
