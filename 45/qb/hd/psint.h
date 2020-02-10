/* File: psint.h - Defines which apply to the QBI Parser                */
/* NOTE: When making changes to this file, be sure to make equivalent   */
/*       changes to file psint.inc                                      */

#undef PSINT_H
#define PSINT_H ON         /* remember that this file has been included */

#if !HEAP_H
# include "heap.h"
#endif

/**======================================================================**
 **==       Internal Interface to Parser Component                     ==**
 **==       This file is only included by parser-component modules     ==**
 **======================================================================**/

#define EmitOpcode(x) Emit16(x)

/* Types of literal constants recognized by Fin and FetchToken/ScanToken */
#define  LIT_I2   0     /* % suffix */
#define  LIT_B2   1     /* &B prefix */
#define  LIT_O2   2     /* &O prefix */
#define  LIT_H2   3     /* &H prefix */
#define  LIT_I4   4     /* & suffix */
#define  LIT_B4   5     /* &&B prefix */
#define  LIT_O4   6     /* &&O prefix */
#define  LIT_H4   7     /* &&H prefix */
#define  LIT_R4   8     /* ! suffix */
#define  LIT_R8   9     /* # suffix */
#define  LIT_STR  10    /* "xxx" */

/**==================== value descriptor =================================
 ** this structure describes literal values returned by the lexical analyzer
 **=======================================================================**/

typedef struct litValue {
   ushort errCode;            /* error code reported by Fin (0 if none) */
   char type;                 /* ET_I2, ET_I4, ET_R4, ET_R8, ET_STR */
   char litType;              /* LIT_I2, LIT_I4, LIT_R4, LIT_R8,
                                 LIT_B2, LIT_H2, LIT_O2, LIT_J2, LIT_K2,
                                 LIT_B4, LIT_H4, LIT_O4, LIT_STR */
   union {
      short I2;               /* 16 bit signed integer literal */
      ushort USI2;            /* 16 bit unsigned integer literal */
      long I4;                /* 32 bit signed integer literal */
      ulong USI4;             /* 32 bit unsigned integer literal */
      FLOAT R4;               /* 32 bit real literal */
      DOUBLE R8;              /* 64 bit real literal */
      ushort cbStr;           /* byte count of string literal recognized
                                 by FetchTok() */
      } value;
   } litValue;

/**==================== token descriptor ==================================
 ** tok - (token descriptor) describes tokens returned by the lexical analyzer
 **=======================================================================**/

typedef struct tok {
   ushort class;              /*  token class - CL_xxx defined below */
   ushort oSrc;               /* offset into ps.bdpSrc where token started */
   union dsc {
      struct rw {
         ushort iRw;          /* unique index for res word (IRW_ELSE etc) */
         ushort rwf;          /* flags for this reserved word (RWF_xxx)
                                 which begin with this opcode */
         ushort iOperator;    /* index into operator table for operator with
                                 this name.  UNDEFINED if none exists. */
         char *pArgs;         /* pointer into reserved word table to list
                                 of args for reserved word */
         } rw;                /* valid when class = resword */
      struct id {
         ushort oNam;         /* name table offset for identifier */
         ushort oTyp;         /* Explicit enumerated type i.e. ET_I2 */
         ushort vmFlags;      /* varmgr flags FNVLVAL etc. - see variable.h */
         char charFirst;      /* Ucase(1st letter of id) */
         char termchar;
	 uchar lexFlags;      /* lexical analyzer flags */
	 uchar filler;	      /* pad to even byte length */
         } id;                /* valid when class = id */
      litValue lit;           /* valid when class = lit */
      char unknownChar;       /* valid when class = unknownChar */
      } dsc;
   } tok;

/* tok.id.lexFlags flags */
#define FLX_hasPeriod 01   /* TRUE if current token has a '.' in its name */

/* Enumerations for tok.class */
#define CL_RESWORD 0       /* reserved word or special character like & */
#define CL_ID 1            /* alphanumeric identifier */
#define CL_LIT 2           /* numeric or string constant */
#define CL_UNKNOWNCHAR 3   /* lexical analyzer encountered an unknown char */

/* PARSE_RESULT is the type returned by Parse() and functions which
   recognize non-terminals.  The values are PR_NotFound, PR_GoodSyntax,
   and PR_BadSyntax.
*/
#define PARSE_RESULT char
#define PR_BadSyntax ((char)(-1))
#define PR_NotFound (char)0
#define PR_GoodSyntax (char)1

/* Indecies into table of operators used by RcExp() */
#define IOP_mark 0
#define IOP_RParen 1
#define IOP_Imp 2
#define IOP_Eqv 3
#define IOP_Xor 4
#define IOP_Or 5
#define IOP_And 6
#define IOP_Not 7
#define IOP_EQ 8
#define IOP_LT 9
#define IOP_GT 10
#define IOP_LE 11
#define IOP_GE 12
#define IOP_NE 13
#define IOP_Add 14
#define IOP_Minus 15
#define IOP_Mod 16
#define IOP_Idiv 17
#define IOP_Mult 18
#define IOP_Div 19
#define IOP_Plus 20
#define IOP_UMinus 21
#define IOP_Pwr 22
#define IOP_LParen 23
#define NUM_OPERATORS 24

typedef ushort (pascal near *PUshortFunc)();
   /* ptr to near function which returns ushort */

#define ND_NONTERMINAL ND_BRANCH+1
#define ND_EXT_NONTERMINAL ND_BRANCH+2
#define ND_TOKEN ND_BRANCH+3

/* parser component global data declarations */
/* These are actually declared in qbidata.c, which defines EXTERNAL as "" */

#define LOOK_AHEAD 9
EXTERNAL tok tLookAhead[LOOK_AHEAD];   /* token circular queue */
EXTERNAL uchar cTokScan;         /* count of tokens scanned by ScanTok.
                                    Used by NtParse and NtStatementList to
                                    see if any tokens have been consumed
                                    (so they know to return PR_NotFound) */
EXTERNAL tok *pTokScan;          /* points to last token scanned by ScanTok */
EXTERNAL tok *pTokPeek;          /* points to last token scanned by PeekTok */
EXTERNAL tok *pTokLast;          /* points to last token scanned by FetTok */
EXTERNAL tok *pTokLastConsumed;  /* points to last token consumed by ScanTok */
EXTERNAL char fFirstStmtOnLine;  /* TRUE if any code emitted for this stmt */
EXTERNAL ushort cIdArgs;         /* used by code generation routines which
                                    need to output an argument count */

extern uchar tState[];              /* parse state table */
extern short tIntNtDisp[];          /* table of state driven non-terminals */
extern PUshortFunc tExtNtDisp[];    /* table of native-code non-terminals */
extern ushort tExtNtHelp[];         /* err msg for each native non-terminal */
extern uchar *tRw[];                /* reserved word table */
extern char mpIRWtoChar[];          /* table which maps 1 char reserved words
                                       to their char (like +,* etc.) */

/* Declarations for parser component functions */
PARSE_RESULT near CheckRw(char *, ushort);
PARSE_RESULT near ErrExpExpression(void);
PARSE_RESULT near NtEndStatement(void);
PARSE_RESULT near NtStatementList(void);
PARSE_RESULT near NtExp(void);
PARSE_RESULT near NtIdAryElem(void);
PARSE_RESULT near NtImpliedLetOrCall(boolean);
PARSE_RESULT near NtIntrinsic(void);
PARSE_RESULT near NtLit(void);
PARSE_RESULT near NtLitI2(void);
PARSE_RESULT near NtParse(ushort);
ushort NEAR BindVar(struct tok *);
boolean near ConsumeRw(ushort);
boolean near TestLn(void);
boolean near TestPeek(ushort);
boolean near TestScan(ushort);
ushort near RelOp();
ushort NEAR SubRef(ushort);

void near Emit16(ushort);
void near Emit16_0(void);
void near Emit32(ulong);
void near EmitSrcCompress(ushort ,ushort);
void near EmitSrc(ushort ,ushort);
void near FetchToken(struct tok *);
void near FetTok(void);
void near Peek1Tok(void);
void near PeekNextTok(void);
void near ResetExpStack(void);
void near ScanLit(struct tok *);
void near ScanTok(void);
void near ListIRWtoErrBuf(ushort);
void near PErrState(ushort);
void near PErrExpectedOr(void);
void near PErrExpMsg(ushort);
void near PErrExpRw(ushort);
void near PErrMsg(ushort);
void near PErrState(ushort);
void near ParseErr(ushort, ushort);
void near ParseErr0(ushort);
void near ParseErrOm(void);
