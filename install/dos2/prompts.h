
/* Defines information for file copy errors */

struct CurrentFile
{

	unsigned char		chReadWrite;
	unsigned char		chValidErrors;
	char					szFileName[13];

} vCurrentFile;


extern int		PromptForDisk( char chSourceDrv );

extern void 	RebootPrompt		( void );
extern void 	DisplayFileStatus	( char *szFileName, int Type );
extern int		PromptSelect		( char * *szStrings, char * *szOpts );
extern void 	ProcessCopyError	( char *szFileName, int Type );
extern int		AbortPrompt			( void );
extern void 	WriteProtectPrompt( char Disk );
extern void 	NotReadyPrompt		( char Disk );
extern void 	FatalDiskError		( char Disk );
extern void 	FatalError			( int ErrorNumber );
extern void 	FileStatusUpdate	( void );

/* PROMPTS lib */
extern int		PromptForDefault( void );
extern int		PromptForDiskFmt( int iDrvType );
extern void 	FmtStat( unsigned uTrack, unsigned uHead );
extern void		ProcessDiskError(int ErrorType);
extern void 	Error(char * *ErrorString);


