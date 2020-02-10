// Prototypes for doslzexp.c

// writeOutBuf() must be supplied by another module
extern int WriteOutBuf(unsigned char uch, int doshDest);

// functions defined in doslzexp.c
extern int CopyCreateDate(int doshFrom, int doshTo);
extern int ReadInBuf(unsigned char LZPTR *puch, int doshSource);
extern int GetHdr(struct tagFH LZPTR *pFH, int doshSource);
extern int ChkHdr(struct tagFH FHIn);
extern BOOL InitBuffers(void);
extern int LZDecode(int doshSource, int doshDest);
