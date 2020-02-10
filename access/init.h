/*  INIT.H   */

void selectInjectMethod(void);
void serialKeysInit(void);
void serialKeysStartupInit(void);
void serialKeysStateInit(void);
void initGIDEI(void);

void serialKeysBegin(void);

void errorCode(BYTE errorNum);
void writeCommPort(void);
void errDetect(void);
