/* TS = NONE */
/*** API.H:  Extern procs for the compression and decompression libraries. ***/


  /* procedures in SUCOMP.LIB */
extern  SHORT   far  CbWriteFileHeader(int fhDest);
extern  LONG    far  LcbCompressToFile(SHORT wAlgType, int fhSrc, int fhDest,
                           LONG lcbDestMax);
extern  LONG    far  LcbCalculateCompressedLength(SHORT wAlgType, int fhSrc,
                           LONG lcbDestMax);

  /* procedures in SUDECOMP.LIB */
extern  SHORT   far  WReadHeaderInfo(int fhSrc);
extern  BOOL    far  FFreeHeaderInfo(void);   /* also in SUCOMP.LIB */
extern  LONG    far  LcbDecompFile(int fhSrc, int fhDest, LONG lcbDestMax,
                           LONG libStart, BOOL fHeaderAlreadyRead,
                           BYTE far * pfbBuf, LONG lcbBuf, PFNWFROMW pfn,
                           int cProgTicks, PFNWFROMW pfnYield);
extern  LONG    far  LcbDecompressToFile(int fhSrc, int fhDest, LONG lcbDestMax,
                           LONG libStart, BOOL fHeaderAlreadyRead);
extern  LONG    far  LcbDecompressToBuffer(int fhSrc, BYTE far * fpbBuf,
                           LONG lcbBuf, LONG libStart, BOOL fHeaderAlreadyRead);
extern  LONG    far  LcbCalculateDecompressedLength(int fhSrc,
                           BOOL fHeaderAlreadyRead);

  /* global variables for file header info before writing or after reading */
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
