/* KBD.H  */


void processKbdPress(void);
void processKbdCombine(void);
void processKbdHold(void);
void processKbdLock(void);
void processKbdRel(void);
BOOL validKCode(void);

void processKbd(void);
void processKbdIndicator(void);
void processKbdVersion(void);
void processKbdModel(void);
void processKbdDescription(void);
void processKbdUnknown(void);


BOOL inLst(struct listType *listPtr, BYTE searchChar);

void removeKeyFromHoldList(BYTE theKey);
void removeKeyFromLockList(BYTE theKey);

void doPressLst(void);
void doCombineLst(void);
