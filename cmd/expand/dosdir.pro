// Prototypes for dosdir.asm

extern int FAR GetCurDrive(void);
extern int FAR GetCurDir(LPSTR lpszDirBuf);
extern int FAR SetDrive(int wDrive);
extern int FAR SetDir(LPSTR lpszDirName);
extern int FAR IsDir(LPSTR lpszDir);
extern int FAR DosRemoveable(int wDrive);
