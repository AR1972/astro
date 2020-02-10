;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
/*
 *  This file contains the constants, globals, structure definitions,
 *  extern declarations, and macro definitions for the FC utility.
 */


/* #define  DEBUG  FALSE */

/*
 *  Constant Definitions
 */
#define byte  unsigned char
#define word  unsigned short

#define SUCCESS 0
#define HELPTEXT  1
#define MAXFNAME 80

extern unsigned char _ctype_[];
#define _SPACE        0x8              /* tab, carriage return, new line, */
#define ISSPACE(c)     ( (_ctype_+1)[c] & _SPACE )


/*
 *   Structure Definitions
 */
struct lineType {
    int     line;                      /* line number */
    unsigned char    text[MAXARG];     /* body of line */
};

struct lineType *buffer1,
                *buffer2;

byte line[MAXARG];                     /* single line buffer */

byte *extBin[] = { ".EXE", ".OBJ", ".LIB",
                            ".COM", ".BIN", ".SYS", NULL };

/*
 *  Global Declarations
 */
int ctSync  = -1,                      /* number of lines required to sync */
    cLine   = -1;                      /* number of lines in internal buffs */

flagType fAbbrev = FALSE,              /* abbreviated output */
         fBinary = FALSE,              /* binary comparison */
         fLine   = FALSE,              /* line comparison */
         fNumb   = FALSE,              /* display line numbers */
         fCase   = TRUE,               /* case is significant */
         fIgnore = FALSE;              /* ignore spaces and blank lines */

#ifdef  DEBUG
  flagType fDebug = FALSE;
#endif


/*
 *  Extern Declarations for routines defined in ttypes.h
 */
extern int fgetl();
extern int strcmp();
extern byte toupper(int c);
extern char *strstr();
extern char *strrchr();
extern char *strupr();


/*
 *  Forward Declarations
 */
int main(int c, byte *v[]);
void usage (unsigned char *p,unsigned int pcode);
int BinaryCompare(unsigned char *f1,unsigned char *f2);
char compare(int l1,int s1,int l2,int s2,int ct);
void LineCompare(unsigned char *f1,unsigned char *f2);
int xfill(struct lineType *pl,struct _iobuf *fh,int ct,int *plnum);
int adjust(struct lineType *pl,int ml,int lt);
void dump(struct lineType *pl,int start,int end);
void pline(struct lineType *pl);
int strcmpi(unsigned char *str1,unsigned char *str2);
int strcmps(unsigned char *p1,unsigned char *p2);
int strcmpis(unsigned char *p1,unsigned char *p2);
Boolean has_extension(char *s);
void ParseFileNames(char *, char *);
char *FindFileName(char *);
Boolean HasWildcard(char *);
void CheckWildcard(char *, char *);
int ExpandFile2(char *, char *);                 /* M001 */
void comp(char *, char *);

int fgetl(char *buf,int len,struct _iobuf *fh);
int fputl(char *buf,int len,struct _iobuf *fh);
char *error(void);
int max(int a, int b);
int min(int a, int b);
char *strpbrk(char *string1, char *string2);

#ifdef DBCS
int	checkspace(unsigned char *);
int	IsDBCSLeadByte(unsigned char);
int	CheckDBCSTailByte(unsigned char *,unsigned char *);
#endif


/*
 *  Function Pointers
 */
int (*funcRead) (),                    /* function to use to read lines */
    (*fCmp) ();                        /* function to use to compare lines */


/*
 *  DOS major/minor version numbers
 */
extern unsigned char _NEAR _CDECL _osmajor;
extern unsigned char _NEAR _CDECL _osminor;


