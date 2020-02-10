/*
    -- std.h standard types
*/

#define TRUE 1
#define FALSE 0

#define CDECL cdecl
#define CONST

#define FOREVER while(1)
#define BLOCK

#ifdef D86
#define szROText "rt"
#endif
#ifdef X86
#define szROText "r"
#endif

typedef int BOOL;
typedef char *SZ;
typedef unsigned char BYTE;
typedef BYTE *PB;
typedef unsigned short WORD;
typedef WORD *PW;
typedef unsigned long LONG;

void CDECL ProcessArgs(int, char **);
void CDECL FatalError();
SZ CDECL SzGetArg(void);

extern SZ szNameProgram;	/* Program name (not extension) */
extern int cargCur;		/* Current # of args left */

