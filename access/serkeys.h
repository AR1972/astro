/*  serkeys.H   */

void doBeep(void);
void doReboot(void);
void handleBreak(void);
BOOL inPauseCondition(void);
void clearInPause(void);
void setInPause(void);
void clearOutComm(void);

void serialKeysCommInit(void);
void serialKeysDisable(void);
void initCommPort(void);
void setBaudRate(void);
void turnOffHandshake(void);
void turnOnHandshake(void);
void disableComm(void);
void enableComm(void);
void clearOutComm(void);
void disableKeyEnhance(void);

BOOL waitingForIndicatorUpdate(void);
void getBiosFlags(void);
void putBiosFlags(void);
BOOL keyBufferFull(void);
BOOL keyBufferEmpty(void);
void putInKbdBuffer(unsigned int);

void putInMouseBuffer(signed char, signed char, signed char);
void sendMouseData(void);

int convertStringToInt(void);

extern BYTE * mouBufTailPtr;
extern BYTE * mouBufHeadPtr;
extern BYTE mouBuffer[];
extern BYTE altSuppressedTbl[];
extern BYTE shiftSuppressedTbl[];
extern BYTE ctrlSuppressedTbl[];
extern BYTE tryingToWriteKeyboardData;

