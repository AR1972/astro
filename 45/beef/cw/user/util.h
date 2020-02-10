/*
	COW : Character Oriented Windows

	util.h : utilities, cow private interface
		Cmerge definitions from util.asm + sdmasm.asm
*/


#ifdef DUAL
#define	NEARorFAR	FAR
#else /*!DUAL*/
#define	NEARorFAR	NEAR
#endif /*!DUAL*/

/* BLTS : both : source, destination, count_of_bytes */
VOID *	FARPRIVATE	bltbyte(VOID *, VOID *, WORD);
VOID	FARPRIVATE	bltbytex(VOID far *, VOID far *, WORD);

VOID	FARPRIVATE	stringcpy(char *, char *, WORD);
WORD	FARPRIVATE	fstrlen(char far *);
short	FARPRIVATE	fstrcmp(char far *, char far *);
VOID	FARPRIVATE	fstrcpy(char far *, char far *);
#define strlen(sz) fstrlen((char far *) (sz))
#define strcpy(sz1, sz2) fstrcpy((char far *) (sz1), (char far *) (sz2))

#ifdef EXTRAS
int	FARPRIVATE	CchRealLenSz(char *);
#else
/* no special character => a character is a character */
#define	CchRealLenSz(sz)	strlen(sz)
#endif

#define strlen(sz) fstrlen((char far *) (sz))

/*	* NEAR helper procedures */

/* CORE segment */
VOID	NEARorFAR PASCAL	DisableInterrupts(void);
VOID	NEARorFAR PASCAL	EnableInterrupts(void);

#ifdef DIRLIST	/* in LISTBOX segment */
BOOL	NEARorFAR PASCAL	FFindNext(struct _fde *);
BOOL	NEARorFAR PASCAL	FFindFirst(struct _fde *, char *, WORD);
VOID	NEARorFAR PASCAL	FindClose(void);
WORD	NEARorFAR PASCAL	AtrOfPath(char *);
char	NEARorFAR PASCAL	GetCurDrive(void);
VOID	NEARorFAR PASCAL	SetCurDrive(char);
VOID	NEARorFAR PASCAL	GetCurDir(char, char *);
BOOL	NEARorFAR PASCAL	FSetCurDir(char *);

#ifndef DOS5	/* fast fill for DOS 3 */
VOID	NEARorFAR PASCAL	PrepareWild(struct _fcb *, char *);
BOOL	NEARorFAR PASCAL	FMatchWild(struct _fcb *, char *);
BOOL	NEARorFAR PASCAL	FValidDir(char *);
#endif /*!DOS5*/

#endif /*DIRLIST*/

#ifdef SCROLL
WORD	NEAR PASCAL	TranslatePosition(WORD, WORD, WORD, WORD, WORD, BOOL);
#endif

#ifdef SDM
VOID	NEAR PASCAL	FillBuf(char *, char, WORD);
#endif
