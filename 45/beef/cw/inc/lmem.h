/*
	LMEM.H : Local Memory Manager Exports
*/

/*	* Heap Compaction Flags	*/
#define	fcmpCompact		1	/* compact data */
#define	fcmpCompactHandles	2	/* compact handles */

/*	* Out Of Memory Errors	*/
#define	merrAllocMoveable	1	/* allocating a moveable block */
#define	merrAllocFixed		2	/* allocating a fixed block */
#define	merrReallocMoveable	3	/* growing a moveable block */
#define	merrAllocBlock		4	/* allocating block of handles */

/*	* Special Zeros		*/
/* NOTE: hard coded numbers to get around compiler limitations */
#define pvZero	((VOID NEAR *)0x12)  /* a pointer to 0 length block, 1st byte is 0 */
#define ppvZero ((VOID NEAR * NEAR *)0x14) /* a pointer to pvZero */

/*	* Allocating a fixed block in a new heap always returns pvFixedMin */
/* NOTE: hard coded numbers to get around compiler limitations */
#define pvFixedMin ((VOID NEAR *)0x18)

/* global flag set after every compact */
extern BOOL PASCAL fCompactedHeap;


#ifdef DEBUG

/*	Debug global variables */
extern BOOL PASCAL fShakeHeap;
extern BOOL PASCAL fCheckHeap;
extern BOOL PASCAL fCheckHeapFree;	/* extra free checking -- slow */

/*	* function for introducing memory failures */
#ifndef MKHDR
extern BOOL (FAR PASCAL *lpfnFailLmemFilter)(WORD, SB, WORD);	/*OPTIONAL*/
#endif


/*	* Heap Info structure */

typedef struct _ckl
	{
	unsigned cblkFixed;
	unsigned cblkMoveable;
	unsigned chUsed;
	unsigned chFree;
	unsigned cbFixed;
	unsigned cbMoveable;
	} CKL;

#endif /*DEBUG*/

/* Error trapping */
extern int pascal cfailLmemError;
#define	DisarmLmemError()	(cfailLmemError++)
#define	RearmLmemError()	(cfailLmemError--)

/* Fast way to find length of block (in current SB only) */
#define	CbSizePpvSbCur(ppv) 	(*((*(WORD **)(ppv))-1))

/* Procedure Prototypes */
#ifdef CC
VOID		FAR PASCAL CreateHeap(SB);
WORD		FAR PASCAL CbCompactHeap(SB, WORD);
WORD		FAR PASCAL CbAvailHeap(SB);
VOID NEAR * NEAR *  FAR PASCAL PpvAllocCb(SB, WORD);
BOOL		FAR PASCAL FReallocPpv(SB, VOID NEAR * NEAR *, WORD);
VOID		FAR PASCAL FreePpv(SB, VOID NEAR * NEAR *);
WORD		FAR PASCAL CbSizePpv(SB, VOID NEAR * NEAR *);	/* CC ONLY */
VOID NEAR *	FAR PASCAL PvAllocFixedCb(SB, WORD);
WORD		FAR PASCAL CbSizeFixedPv(SB, VOID NEAR *);	/* CC ONLY */
VOID		FAR PASCAL LockHeap(SB);			/* OPTIONAL */
VOID		FAR PASCAL UnlockHeap(SB);			/* OPTIONAL */

BOOL		FAR PASCAL FCreateWindowsHeap(SB, WORD);

VOID NEAR * NEAR * FAR PASCAL HFirstAllocBlock(SB, WORD);	/* OPTIONAL */
VOID		FAR PASCAL FreeHandleBlock(SB, VOID NEAR * NEAR *); /* OPTIONAL */
VOID		FAR PASCAL SwapHandles(SB, VOID NEAR * NEAR *, VOID NEAR * NEAR *); /* OPTIONAL */

#ifdef DEBUG
VOID		FAR PASCAL CheckHeap(SB);
VOID		FAR PASCAL ShakeHeapSb(SB);
VOID		FAR PASCAL GetHeapInfo(SB, CKL FAR *);
BOOL		FAR PASCAL FCheckHandle(SB, VOID NEAR * NEAR *);
#endif /*DEBUG*/
/* Pseudo debug functions */
VOID NEAR * NEAR * FAR PASCAL PpvWalkHeap(SB, VOID NEAR * NEAR *);
VOID NEAR *	FAR PASCAL PvWalkFixedHeap(SB, VOID NEAR *);
#endif /*CC*/

