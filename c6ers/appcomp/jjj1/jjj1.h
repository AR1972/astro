/*
**  JJJ1.H -- Defines and externs for JJJ1 compression/decompression
**
**  Code adapted from ZK1 code by Jeff Johnson, 10/12/90 added a triple
**  huffman compressor to the window compressor
*/

#include "..\lzss\lz.h"


typedef struct _HTREE 
{
   int   parent, left, right;
   long  weight;
} HTREE;

typedef struct _CODETABLE
{
   unsigned int  usCode;
   BYTE          cbCode, nextCode;
} CODETABLE;

#define  HUFFMAN  0

#define  NOTABLE      0
#define  COMPRESS1BIT 1
#define  COMPRESS2BIT 2
#define  COMPRESSNONE 3

#define  cbLitMax   32

  /***** EXTERNS *****/
extern  USHORT      iPowers[16];

extern long far *   fpbAnalysis4a;
extern long far *   fpbAnalysis4b;
extern long far *   fpbAnalysis5;
extern long far *   fpbAnalysis6;
extern long far *   fpbAnalysis8;

extern HTREE far *  fphtArr;

extern CODETABLE far *  fpct4a;
extern CODETABLE far *  fpct4b;
extern CODETABLE far *  fpct5;
extern CODETABLE far *  fpct6;
extern CODETABLE far *  fpct8;

extern BYTE far *       fpbLookup4a;
extern BYTE far *       fpbLookup4b;
extern BYTE far *       fpbLookup5;
extern BYTE far *       fpbLookup6;
extern BYTE far *       fpbLookup8;

extern  BOOL  FAllocateJJJGlobals(LONG lcbDestMax, BOOL fCompressing);
extern  void  FreeJJJGlobals(void);
extern  BOOL  BuildCodeTable(CODETABLE far *fpct, int cbct);
