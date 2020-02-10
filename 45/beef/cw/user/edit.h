/*
	COW : Character Oriented Windows

	edit.h : edit cow private interface
*/

/*****************************************************************************/

/* Edit Control Text Styles */
#define ES_LEFT 		0	/* left justified text */
#define ES_CENTER 		1	/* centered text */
#define ES_RIGHT 		2	/* right justified text */

#define	ES_SECRET		4	/* secret edit field */

/*****************************************************************************/

#ifdef EDIT_FULLMGR
#include "medit.h"			/* multi-line edit */
#else
#include "sedit.h"			/* single-line edit */
#endif

PRIVATE DWORD FARPUBLIC InternalEditWndProc(PWND, WORD, WORD, DWORD);

#ifdef EDIT_FULLMGR
VOID	FARPUBLIC InitEditWnd(PWND, BYTE *, WORD);
#endif

/*****************************************************************************/

/* Characters */
#define	chFillEdit	chMiddleDot	/* middle dot */
#ifdef EDIT_SECRET
#define	chSecret	'.'		/* single character for secret edit items */
#endif

/*****************************************************************************/
