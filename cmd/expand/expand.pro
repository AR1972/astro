// Prototypes for compress.c

extern void CatPathAndFileName(char *pszPath, char *pszFileName);
extern char *ExtractFileName(char *pszPath);
extern int WriteOutBuf(unsigned char c,
                       int doshDest);
extern int WriteHdr(int doshDest);
extern void LZInitTree(void);
extern void LZInsertNode(int r);
extern void LZDeleteNode(int p);
extern int LZEncode(int doshSource, int doshDest);
extern BOOL GetIOHandles(char *pszSource,
                         char *pszDest,
                         int *pdoshSource,
                         int *pdoshDest);
extern BOOL Compress(char *pszSource,
                     char *pszDest,
                     BOOL bWriteToFile);
extern BOOL Decompress(char *pszSource,
                       char *pszDest);
extern int main(int argc, char **argv);
