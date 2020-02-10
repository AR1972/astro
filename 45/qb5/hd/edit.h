/***	EDIT.H - Structures and constants for the Document Text Manager
*
* GLOBAL
*	None
*
* LOCAL
*	None
*
* DESCRIPTION
*	Constants, structures and function prototypes required to deal
*	with the TXTMGR and EDITMGR at a low level.
*
*	NOTE: any changes should also be made to EDIT.INC
*
*/

#if	!HEAP_H
#include <heap.h>
#endif

#define LOCAL	static
#define GLOBAL


#define pDocumentBuf oPastLastVar

/*--------------------------- Debug Macros ------------------------*/

#define DbHeapMoveOn()
#define DbHeapMoveOff()
#define DbChkPBufInfo(pbufinfo)


/*--------------------------- Constants ---------------------------*/

/* Incremental buffer size for allocation.
*/

#define CBSRCBLK			512

/* Number of document buffers available to the system
*/

#define CBUFINFO			5

/* Maximum number of lines in a document buffer.  This is forced by
 * some edit manager strangeness at 32K-a little bit. If these were
 * fixed, we could go to 64K (minus a little bit because of CW strangeness)
*/

#define CLNMAX				32700

/*----------------------- BUFINFO Structure -----------------------*/

/* NOTE: I am using a FHD structure to inforce the fact that this */
/* NOTE: heap entry is NOT a BDL.  It is a FHD, as BDLs can not   */
/* NOTE: support a size > 64K, and FHDs can.			  */

typedef struct
{
	WORD	oData;
	WORD	hData;
	WORD	pNext;
	WORD	cPara;
} FHD;

typedef FHD *PFHD;

typedef struct
{
	FHD	fhd;		/* File descriptor of in-memory file */
	WORD	cln;		/* Number of lines in the buffer */
	DWORD	cb;		/* Buffer size */
	DWORD	obNext;		/* Offset in buffer of next available byte */
	WORD	olnCache;	/* Line number of last line looked at */
	DWORD	obCache;	/* Offset in buffer of last line looked at */
} BUFINFO;

typedef BUFINFO *PBUFINFO;


/*----------------------- Exported variables -----------------------*/


/*---------------------- Function Prototypes ----------------------*/

/* TEXT.ASM
*/

extern VOID	NEAR	PASCAL	DeTab(char far *, char far *, WORD, WORD);
extern WORD	NEAR	PASCAL	ExpandTabs(char far *, char far *, WORD, WORD);
extern WORD	NEAR	PASCAL	CbFindLine(DWORD *, WORD);
extern VOID	NEAR	PASCAL	BigMoveUp(DWORD, DWORD, DWORD);
extern VOID	NEAR	PASCAL	BigMoveDown(DWORD, DWORD, DWORD);
extern DWORD	NEAR	PASCAL	LinearAddr(char far *);
extern char far *(NEAR PASCAL	SegAddr(DWORD));

extern BOOL	FAR	PASCAL	FhdAlloc(PFHD, DWORD);
extern BOOL	FAR	PASCAL	FhdRealloc(PFHD, DWORD);
extern VOID	FAR	PASCAL	FhdDealloc(PFHD);


/* TEXTMGR.C
*/

extern PBUFINFO FAR	PASCAL	NewBuf(VOID);
extern VOID	FAR	PASCAL	FreeBuf(PBUFINFO);
extern WORD	FAR	PASCAL	AppendLineBuf(PBUFINFO, char **);
extern VOID	FAR	PASCAL	CompressBufs(VOID);

extern WORD	FAR	PASCAL	S_LinesInBuf(PBUFINFO);
extern WORD	FAR	PASCAL	S_CbGetLineBuf(PBUFINFO, WORD, WORD, char *);
extern BOOL	NEAR	PASCAL	S_ReplaceLineBuf(PBUFINFO, WORD, WORD, char **);
extern BOOL	NEAR	PASCAL	S_InsertLineBuf(PBUFINFO, WORD, WORD, char **, BOOL);
extern VOID	NEAR	PASCAL	S_InsertBufInBuf(PBUFINFO, WORD, PBUFINFO);
extern VOID	NEAR	PASCAL	S_DeleteLinesBuf(PBUFINFO, WORD, WORD);
