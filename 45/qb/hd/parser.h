/* File: parser.h - Defines which apply to the QBI Parser               */
/* NOTE: !!!see PARSER.INC for comments!!!                              */
/* NOTE: When making changes to this file, be sure to make equivalent   */
/*       changes to file PARSER.INC                                     */

#undef PARSER_H
#define PARSER_H ON        /* remember that this file has been included */

#if !HEAP_H
# include "heap.h"
#endif

#define CB_PCODE_MIN 20

typedef struct psType {
   char ldDummy[4];
   bdp bdpSrc;
   ushort otxLine;
   uchar flags;
   uchar tEtCur[26];
   uchar filler;
   bdp bdpDst;
   bd bdErr;
   ushort errCode;
   ushort oSrcErr;
} psType;

/* Masks for ps.errCode */
#define PSERR_fAsciiMsg 0x8000
#define PSERR_fRude 0x4000
#define PSERR_fAlert 0x2000
/* bits 0x1000, 0x0800, and 0x400 are unused */
#define PSERR_errCode 0x03FF

void FAR ParseInit(void);
void FAR ParseNewInit(void);
boolean FAR SetPsBufSz(char *);
boolean near ParseLine(void);
boolean FAR ListStdMsgToBd(ushort, bd *);
void NEAR MakeOpReParse(void);
ushort FAR ListIRW(ushort);

/* parser component global data declarations */
/* These are actually declared in qbidata.c, which defines EXTERNAL as "" */
EXTERNAL psType ps;                 /* parser's Entry/Exit parm structure */
