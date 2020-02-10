/******
* Text Manager routines - Supplied by the application 
*
*******************************************************************/

void TEXT_MGR StartBigEdit(void);
void TEXT_MGR EndBigEdit(void);
ushort TEXT_MGR LinesInBuf(ushort);
bool TEXT_MGR InsertLineBuf(ushort, ushort, ushort, char *);
ushort TEXT_MGR hbufScrap(void);
void  TEXT_MGR InsertBufInBuf(ushort, ushort, ushort);
ushort TEXT_MGR cbGetLineBuf(ushort, ushort, ushort, char *);
void TEXT_MGR FreeScrap(void);
void TEXT_MGR DeleteLinesBuf(ushort, ushort, ushort);
bool TEXT_MGR ReplaceLineBuf(ushort, ushort, ushort, char *);
LineAttr *(TEXT_MGR GetLineAttrs(ushort));
bool TEXT_MGR fReadOnlyBuf(ushort);

char COW GetEditMgrState(void);
void COW SetTabs (ushort);
ushort COW GetTabs (void);
ISA COW SetInverseIsa (ISA);
WORD COW fInitColorTable (void);

/* Other Application supplied routines */
void APPL AppAlertOmf(void);
void APPL SetAbortFlag(void);
void COW  DrawToggles(void);
bool COW IsWordChar(byte);	/* Far for use in CW editmgr */
bool APPL IsLabelChar(byte);
void APPL strupr(char *);
void APPL lower(char *);
char APPL ToUpperNoAccent(char);
void APPL SwitchDiskettes(char);

char *(APPL UiStackAlloc(ushort));
void APPL UiStackFree(ushort);

ushort APPL OpenFile(char *);
ushort APPL CreateFile(char *);
bool   APPL DeleteFile(char *);
bool   APPL CloseFileNear(ushort);
ushort APPL WriteFile(ushort, char *, ushort );
ushort APPL ReadFile(ushort, char *, ushort );
void   APPL FlushFile(ushort);

char *(LIBC strcat(char *, char *));
char *(LIBC strcpy(char *, char *));
char *(LIBC strncpy(char *, char *, ushort));
char *(LIBC memset(char *, char, ushort));
char *(LIBC memmove(char *, char *, ushort));
ushort LIBC strlen(char *);
int LIBC strcmp (char *, char *);
int LIBC strcmpi (char *, char *);
char *(LIBC memchr(char *, char, ushort));
char *(LIBC memcmp(char *, char *, ushort));
int LIBC atoi (char *);
char *(LIBC itoa (int, char *, int));

short FAR PASCAL NormFileNameFar (char *, ushort, char *, char *);
extern char *b$PN_NAME;
extern char b$BAS_EXT;		// ".BAS"
extern char b$Buf1;		// buffer of length FILNAML
extern char b$Buf2;		// buffer of length FILNAML

#define	DbChkHoldBuf1() {;}
#define	DbChkHoldBuf2() {;}
#define	DbChkHoldBuf3() {;}
#define	DbChkFreeBuf1() {;}
#define	DbChkFreeBuf2() {;}
#define	DbChkFreeBuf3() {;}
