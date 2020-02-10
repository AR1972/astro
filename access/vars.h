/*  VARS.H  */


/*	SERIAL PORT VARIABLES  */


#if defined ALL_LINK
	extern unsigned int skCommPortAddr;
	extern volatile BYTE injectByte;
	extern volatile BYTE forcedInt9Flag;
	extern unsigned int combase;
	extern BYTE fmouse_id;
	extern unsigned int skBaudRate;
	extern BYTE skCommIRQ;
	extern BYTE vector;
	extern BYTE comp_id;
	extern BYTE finject_keys;
	extern BYTE serialKeysOn;
#endif

BYTE skCompId;
BYTE tempSkCompId;
BOOL singleUserSetup;
BOOL skWindowCompatible;
BOOL inSerialKeys;
BOOL inPauseFlag;
BOOL needTimerHelp;
BOOL prevPauseState;
BOOL weInjectedFlag;
BYTE fatalErrorFlag;
BYTE handleFatalError;
/*volatile BYTE mouseDataRcd;*/
unsigned int waitForInt9Timeout;
unsigned int sendSoftHandshakeStatus;


unsigned int timerCount;
functionPtrType injectKeysVector;
/* serial */
struct serialDataType commBuf[COMMBUFLEN];
struct serialDataType *putPosSBuf;
struct serialDataType *getPosSBuf;
unsigned int numCommChars;

unsigned int extendSeg;
BYTE kbFlag;
volatile BYTE kbFlag1;
BYTE kbFlag2;
BYTE altKeypad;
BYTE mouType;
unsigned int mouCommPortAddr;
int byteCount;



/* GIDEI variables */
struct asciiTableType	*asciiTblPtr;
struct aliasTableType	*aliasPtr;
functionPtrType			serVector;
functionPtrType			codeVector;
functionPtrType			cmdVector;
functionPtrType			scanDownTransVector;
functionPtrType			scanUpTransVector;
struct stackType			ptrStack[MAXPOINTERSTACK];		/* holds pointers when BEGIN used */
struct stackType			*ptrStackPtr;						/* pointer into the stack */
BYTE							buf[CODEBUFFERLEN];				/* holds GIDEI codes temporarily */
int							spos;
int							rpos;
volatile outBufferType	dataBlk;								/* global outputBuf item */
outBufferType				outputBuf[OUTPUTBUFLEN];		/* holds keys to type and mouse actions */
volatile outBufferType	*putPosOBuf;
volatile outBufferType	*getPosOBuf;
struct listType			tmpLst;
struct listType			kHold;
struct listType			kLock;
struct kStateType			kState;
BYTE							aliasStr[MAXALIASLEN+5];
BYTE							lastCode;
BYTE							serByte;
BYTE							gCode;
BYTE							passAll;
BYTE							stdErrorFlag;
BYTE							feCount;
BYTE							waitForClear;
BYTE							beginOK;
int							tmpDist;
int							tmpStatus;
int							mouseX;
int							mouseY;
int							mouseState;
int							nullCount;
unsigned int				tempskBaudRate;

extern struct aliasTableType nullTable[];
extern struct aliasTableType gideiAliasTable[];
extern struct aliasTableType commandsAliasTable[];
extern struct aliasTableType kbdAliasTable[];
extern struct aliasTableType kbdModelAliasTable[];
extern struct aliasTableType kbdDescriptionAliasTable[];
extern struct aliasTableType kbdVersionAliasTable[];
extern struct aliasTableType kbdIndicatorAliasTable[];
extern struct aliasTableType mouseAliasTable[];
extern struct aliasTableType genAliasTable[];
extern struct aliasTableType commAliasTable[];
extern struct aliasTableType baudrateAliasTable[];
extern struct aliasTableType mouButtonAliasTable[];
extern struct asciiTableType asciiTable[];
extern struct aliasTableType keyAliasTable[];
extern BYTE okKeyTbl[];
extern BYTE IBMextendedScanCodeSet1[];

extern BYTE capsKeysTbl[];
extern BYTE keyPadKeysTbl[];
extern struct scanTblType scanTbl[];

extern void noOpRoutine(void);

