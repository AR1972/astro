/*
	COW : Character Oriented Windows

	lacheck.h : header info for checking local heap
*/

#include "handle.h"

/*	* Additional Types */
typedef int (FAR PASCAL *FARPROC)();		/* Medium Model */
typedef int (NEAR PASCAL *NEARPROC)();		/* Medium Model */

WORD	NEAR * PASCAL pLocalHeap;

#define LOCALARENA struct localarena

LOCALARENA {
    LOCALARENA     *la_prev;
    LOCALARENA     *la_next;
    HANDLEENTRY    *la_handle;
};
typedef LOCALARENA *PLOCALARENA;

typedef struct {
    WORD            hi_check;
    WORD            hi_freeze;
    WORD            hi_count;
    PLOCALARENA     hi_first;
    PLOCALARENA     hi_last;
    BYTE            hi_ncompact;
    BYTE            hi_dislevel;
    WORD            hi_distotal;
    HANDLETABLE    *hi_htable;
    HANDLEENTRY    *hi_hfree;
    WORD            hi_hdelta;
    NEARPROC        hi_hexpand;

    FARPROC         li_notify;
    WORD            li_lock;
    WORD            li_extra;
#ifdef DEBUG
    WORD	    li_cbOverhead;
    WORD	    li_cblkOverhead;
#endif /*DEBUG*/
} LOCALINFO;

typedef LOCALINFO *PLOCALINFO;

#define LA_BUSY     1
#define LA_MOVEABLE 2
#define LA_ALIGN   (LA_MOVEABLE+LA_BUSY)
#define LA_MASK    (~ LA_ALIGN)
