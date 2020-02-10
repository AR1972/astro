/*
	COW : Character Oriented Windows

	dialog.h : dialog specific stuff
*/

/* File find info; it's the DOS 3 structure, but we mimic it for OS/2. */
typedef struct _fde
	{
	char	reserved[21];	/* MSDOS requires this	*/
	char	atr;		/* File attribute	*/
	WORD	wTime;		/* File time of last write */
	WORD	wDate;		/* File date of last write */
	DWORD	cbFile;		/* File size in bytes	*/
	char	szName[13];	/* File name packed	*/
	} FDE;	/* Find directory entry */

#ifndef DOS5
/* FCB for fast dirlist fill */
typedef struct _fcb
	{
	BYTE	dn;		/* drive number */
	char	rgchFile[8];	/* file name -- blank filled */
	char	rgchExt[3];	/* extension -- blank filled */
	BYTE	reserved[25];	/* DOS usage */
	} FCB;	/* File Control Block */

#endif

#define	atrFile		0
#define	atrDir		0x10
#define atrError	((WORD) -1)	/* error return from AtrOfPath */

