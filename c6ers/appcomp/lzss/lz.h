/* TS = none */
/*
**  LZ.H -- Defines and externs for LZ compression/decompression
*/

typedef struct _ND {
    struct _ND  *pNDright;          // left and right node
    struct _ND  *pNDleft;
    struct _ND  *pNDpar;            // parent node
    USHORT       ibRingBuf;
} ND;

#define  cbBufMax   4096            /* size of ring buffer */
#define  cbIndex       2            /* encode string into position and length */
extern   USHORT     cbStrMax;


  /***** EXTERNS *****/
extern  USHORT  iMatchCur;
extern  USHORT  cbMatchCur;

extern  ND *  rgRoot;
extern  ND *  rgND;
extern  ND    nilND;
extern  BYTE far *  ringBuf;

extern  BOOL  FAllocateLZGlobals(LONG lcbDestMax, BOOL fCompressing);
extern  void  FreeLZGlobals(void);

extern  void  LZInitTree(void);
extern  void  LZDeleteNode(SHORT iND);
extern  void  LZInsertNode(SHORT iString);
