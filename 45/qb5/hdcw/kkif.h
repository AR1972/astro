/*	KKIF.H	*/

/*	GENERAL DEFINITIONS	*/

#define	FAILURE	-1
#define	SUCCESS	0
#define	THROUGH	1

/*	MS$KANJI CONSTANTS and DEFINITIONS	*/

typedef struct	{
	short	wType ;		/* Data type of wAscii			*/
	short	wScan ;		/* Key scan code			*/
	short	wAscii ;	/* Ascii code				*/
	short	wShift ;	/* Shift key status			*/
	short	wExShift ;	/* Extended Shift key status		*/
									
	short	cchResult ;	/* Length of Result string		*/
	LPSTR	lpchResult ;	/* Pointer to Result string buffer	*/
									
	short	cchMode ;	/* Length of Mode string		*/
	LPSTR	lpchMode ;	/* Pointer to Mode string buffer	*/
	LPSTR	lpattrMode ;	/* Pointer to Mode attribute buffer	*/
									
	short	cchSystem ;	/* Length of System string		*/
	LPSTR	lpchSystem ;	/* Pointer to System string buffer	*/
	LPSTR	lpattrSystem ;	/* Pointer to System attribute buffer	*/
									
	short	cchBuf ;	/* Length of Display string		*/
	LPSTR	lpchBuf ;	/* Pointer to Display string buffer	*/
	LPSTR	lpattrBuf ;	/* Pointer to Display attribute buffer	*/
	short	cchBufCursor ;	/* Cursor position in Display buffer	*/
									
	char	Reserved[34] ;	/* All elements must be set to 0	*/
} DATAPARM, FAR *LPDATAPARM ;

int	(FAR PASCAL KKOpen)  (DATAPARM far *lpKKData);
int     (FAR PASCAL KKClose) (void);
int	(FAR PASCAL KKJoin)  (DATAPARM far *lpKKData);
int	(FAR PASCAL KKFree)  (void);
int	(FAR PASCAL KKInOut) (DATAPARM far *lpKKData);
BOOL	(FAR PASCAL fKKMode) (void);
