/* ttypes.h - type definitions for tools library */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#undef max
#undef min

/* assembly routines */
#if defined(DOS)
flagType int25 (char, char far *, unsigned int, unsigned int);
flagType int26 (char, char far *, unsigned int, unsigned int);
#endif
// flagType kbhit ();  // already defined by C6
int getch (void);
void cursor (int, int);
#if defined(DOS) || defined(OS2)
void Move (void far *, void far *, unsigned int);
void Fill (char far *, char, unsigned int);
#elif defined(NT)
#define Move(src, dest, count)	memmove(dest, src, count)
#define Fill(dest, val, count)	memset(dest, val, count)
#endif
char *strbscan (char const *, char const *);
char *strbskip (char const *, char const *);
char *strncrlfend (char *, int);
flagType strpre (char *, char *);
long getlpos (void);
void getlinit (char far *, int, int);
int getl (char *, int);
int   max (int, int);
int   min (int, int);
#if defined(DOS) || defined(OS2)
long lmax (long, long);
long lmin (long, long);
#elif defined(NT)
#define lmax(x, y)  max(x, y)
#define lmin(x, y)  min(x, y)
#endif


/* c routines */
#define lower(x)    (strlwr(x))
#define upper(x)    (strupr(x))
#define MakeStr(x)  (strdup(x))
#define strend(x)   ((x)+strlen(x))

#if defined(DOS)
void ZSleep(long);
#elif defined(OS2)
#define ZSleep(x)   DosSleep(x)
#elif defined(NT)
#define ZSleep(x)   Sleep(x)
#endif
flagType delnode (char *);
char  *error(void);
long fexpunge(char *, FILE *);
char  *fcopy(char *, char *);
int fgetl(char	*, int, FILE  *);
int fputl(char	*, int, FILE  *);
int ffirst(char *, int, struct findType *);
int fnext(struct findType * );
void findclose(struct findType * );
char forsemi(char  *,flagType (*)(char *, va_list), ... );
long freespac(int);
long sizeround(long, int);
#if defined(OS2)
struct spawnInfo * rspawnl(char  *,char  *,char  *, ... );
struct spawnInfo * rspawnv(char  *,char  *,char  *,char  *[0]);
#elif defined(DOS)
int rspawnl(char  *,char  *,char  *, ... );
int rspawnv(char  *,char  *,char  *,char  *[0]);
#endif
#if defined(OS2) || defined(NT)
char * fastcopy (HANDLE hfSrcParm, HANDLE hfDstParm);
#endif
void mapenv (char *src, char *dst);
char  *ismark(char  *);
FILE  *swopen(char  *,char  *);
int swclose(FILE  *);
int swread(char  *,int ,FILE  *);
flagType swgoto (FILE *, char *);
char *swmatch (FILE *, char *);
int swparse(char *, char **, char **, char **, char **);
char  *swfind(char  *,FILE *,char  *);
char *getenvini(char  *,char  *);
char fPathChr(char );
char fSwitChr(char );
flagType findpath(char	*,char	*, flagType );
FILE  *pathopen(char  *,char  *,char  *);
int forfile(char *, int, void (*)(char *, struct findType *, va_list), ... );
int EnFile (char *, int, void (*)(char *, struct findType *, va_list), ... );
int rootpath(char  *,char  *);
int sti(char  *,int );
int ntoi(char  *,int );
int strcmps(const char  *,const char  *);
int strcmpis(const char  *,const char  *);
char *strrchrs(char *, char *);
int upd(char  *,char  *,char  *);
int drive(char	*,char	*);
int extention(char  *,char  *);
int filename(char  *,char  *);
int filenamx(char  *,char  *);
int fileext(char *, char *);
int path(char  *,char  *);
int curdir(char *, char );
int getattr(char *);
int MkPath(char *);
int fdelete(char  *);
char *fmove(char  *, char *);
char *fappend(char  *, HANDLE);
long ctime2l(char *);
struct tm *ctime2tm(char *);
long date2l(int, int, int, int, int, int);
VECTOR *VectorAlloc(int);
flagType fAppendVector(VECTOR **, void *);
int pipe( int [] );
int pgetl( char *, int, int );
enum exeKind exeType ( char * );
char *strExeType( enum exeKind );
flagType fMatch (char *, char *);

extern char * (*tools_alloc) (unsigned int);

#if defined(DOS)
int Connect (char *path, char *con, char *sub);
flagType fDisconnect (int drive);
#endif
char *pathcat (char *pDst, char *pSrc);
int setattr (char *pname, int attr);

/*  swchng.c */
flagType swchng (char *strSwFile, char *strTag, char *strLHS, char *strRHS, flagType fNoUndel);
flagType swchnglhs (char *strSwFile, char *strTag, char *strLHS, char *strRHS);
flagType swchange (char *strSwFile, char *strTag, char *strLHS, char *strRHS,
    flagType fNoUndel, flagType *fError);
flagType swchanglhs (char *strSwFile, char *strTag, char *strLHS, char *strRHS,
    flagType *fError);

/*  heapdump.c */
int	heapdump ( FILE *fp, int iFormat );

/*  heapchk.c */
int	heapinfo (void);


/*  pname.c */
char *pname (char *);
unsigned short IsMixedCaseSupported (char *);

#if defined(DOS)
/* msexec.c */
int do_exec (char *xfn, char *pars, int spwn, unsigned needed, char **envp);
#endif


#if defined(NT)
/* ztoolasm.c */
HANDLE	z_handle(int crtfh);
#endif
