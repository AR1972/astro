/* ttypes.h - type definitions for tools library */
/*
*  HISTORY:
*   29-May-87   danl    remove strcmpi
*                       int strcmpi (char *, char *);
*/

#include <stdio.h>

#undef max
#undef min

/* assembly routines */
flagType int25 (char, char far *, unsigned int, unsigned int);
flagType int26 (char, char far *, unsigned int, unsigned int);
// flagType kbhit ();  // already defined by C6
int getch (void);
void cursor (int, int);
void Move (void far *, void far *, unsigned int);
void Fill (char far *, char, unsigned int);
char *strbscan (char *, char *);
char *strbskip (char *, char *);
char *strncrlfend (char *, int);
flagType strpre (char *, char *);
long getlpos (void);
void getlinit ( char far *, int, int);
int getl (char *, int);
int   max (int, int);
int   min (int, int);
long lmax (long, long);
long lmin (long, long);


/* c routines */
#define lower(x)    (strlwr(x))
#define upper(x)    (strupr(x))
#define MakeStr(x)  (strdup(x))
#define strend(x)   ((x)+strlen(x))

flagType delnode (char *);
char  *error(void);
long fexpunge(char  *,FILE *);
char  *fcopy(char *,char *);
int fgetl(char	*,int ,FILE  *);
int fputl(char	*,int ,FILE  *);
int ffirst(char *,int ,struct findType * );
int fnext(struct findType * );
void findclose(struct findType * );
char forsemi(char  *,flagType ( *)(char *, unsigned **), ... );
long freespac(int );
long sizeround(long ,int );
#if defined(OS2)
struct spawnInfo * rspawnl(char  *,char  *,char  *, ... );
struct spawnInfo * rspawnv(char  *,char  *,char  *,char  *[0]);
char * fastcopy (unsigned short hfSrcParm, unsigned short hfDstParm);
#else
int rspawnl(char  *,char  *,char  *, ... );
int rspawnv(char  *,char  *,char  *,char  *[0]);
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
char fPFind(char  *,unsigned int * *);
char findpath(char  *,char  *,char );
FILE  *pathopen(char  *,char  *,char  *);
int forfile(char  *,int ,void (*)(char *, struct findType *, ...), ... );
int EnFile (char *, int, void (*)(char *, struct findType *, ...), ... );
int rootpath(char  *,char  *);
int sti(char  *,int );
int ntoi(char  *,int );
int strcmps(const char  *,const char  *);
int strcmpis(const char  *,const char  *);
int upd(char  *,char  *,char  *);
int drive(char	*,char	*);
int extention(char  *,char  *);
int filename(char  *,char  *);
int filenamx(char  *,char  *);
int fileext(char *, char *);
int path(char  *,char  *);
int curdir(unsigned char *,char );
int getattr(char *);
int fdelete(char  *);
char *fmove(char  *, char *);
char *fappend(char  *, int);
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

char * (*tools_alloc) (unsigned int);

int Connect (char *path, char *con, char *sub);
flagType fDisconnect (int drive);
char *pathcat (char *pDst, char *pSrc);
int setattr (char *pname, int attr);

/*  swchng.c */
flagType swchng (char *strSwFile, char *strTag, char *strLHS, char *strRHS, flagType fNoUndel);
flagType swchange (char *strSwFile, char *strTag, char *strLHS, char *strRHS,
    flagType fNoUndel, flagType *fError);

/*  heapdump.c */
int	heapdump ( FILE *fp, int iFormat );

/*  heapchk.c */
int	heapinfo (void);


/*  pname.c */
char *pname (char *);
unsigned short IsMixedCaseSupported (char *);

/* msexec.c */
int do_exec (char *xfn, char *pars, int spwn, unsigned needed, char **envp);
