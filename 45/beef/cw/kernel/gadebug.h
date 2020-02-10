/*
	COW : Character Oriented Windows

	gacheck.h : header info for checking global heap
*/

#include "handle.h"


/*	* Additional Types */
typedef int (FAR PASCAL *FARPROC)();		/* Medium Model */
typedef int (NEAR PASCAL *NEARPROC)();		/* Medium Model */

typedef WORD *pWORD;

typedef struct
    {
    WORD Offset;
    WORD Segment;
    } FARADDR;
typedef FARADDR *pFARADDR;


typedef struct
	{
	BYTE            ga_sig;
	WORD            ga_owner;
	WORD            ga_size;
	BYTE            ga_flags;
	WORD            ga_prev;
	WORD            ga_next;
	HANDLEENTRY    *ga_handle;
	HANDLEENTRY    *ga_lruprev;
	HANDLEENTRY    *ga_lrunext;
	} GLOBALARENA;

typedef GLOBALARENA far *LPGLOBALARENA;

typedef struct
	{
	WORD	        hi_check;
	WORD            hi_freeze;
	WORD            hi_count;
	WORD            hi_first;
	WORD            hi_last;
	BYTE            hi_ncompact;
	BYTE            hi_dislevel;
	WORD            hi_distotal;
	HANDLETABLE    *hi_htable;
	HANDLEENTRY    *hi_hfree;
	WORD            hi_hdelta;
	NEARPROC        hi_hexpand;

	WORD            gi_minsize;
	WORD            gi_lrulock;
	HANDLEENTRY    *gi_lruchain;
	WORD            gi_lrucount;
	} GLOBALINFO;

typedef GLOBALINFO far *LPGLOBALINFO;

#define GA_SIGNATURE	0x4D
#define GA_ENDSIG	0x5A
#define GA_HOLESIG      0x4A
#define GA_FIXED 1
#define GA_ALIGN GA_FIXED
#define GA_MASK  (~ GA_ALIGN)

#define lpGlobalArena( w ) (LPGLOBALARENA)((DWORD)(w) << 16)
#define lpHandleEntry( w ) (LPHANDLEENTRY)((DWORD)lpGlobalHeap | (WORD)(w))
#define lpHandleTable( w ) (LPHANDLETABLE)((DWORD)lpGlobalHeap | (WORD)(w))
