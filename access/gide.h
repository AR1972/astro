/*  GIDE.H   */


void charHandler(void);
void determineFormat(void);
void processAlias(void);
BOOL aliasUsedInStandard(BYTE aliasLen);
BOOL tblSearch(struct aliasTableType * tblPtr);
void passAllCodes(void);

void processGideiCode(void);
void processGideiClear(void);
void processGideiEnd(void);
void processGideiBlockTransfer(void);
void processBlock(void);
void processBytes(void);

void processCommand(void);

BOOL pushPointers(void);
BOOL restorePointers(void);
BOOL popPointers(void);
BOOL storeByte(BYTE theByte);
BOOL retrieveByte(void);
