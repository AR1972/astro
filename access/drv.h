/*  drv.h  */


void kickStartSerialKeys(void);
void doSerial (void);

BOOL getCommByte(void);

void hardwareInjectKeysRoutine(BYTE scanCode);
void D2InjectRoutine(BYTE scanCode);
void write3fInjectRoutine(BYTE scanCode);
void write20InjectRoutine(BYTE scanCode);
void softInt9InjectKeysRoutine(BYTE scanCode);
void write60InjectRoutine(BYTE scanCode);
void int15InjectRoutine(BYTE scanCode);

void putInOutputBuf(void);
void getOutputBufChar(void);

void upScanPS2 (BYTE keynum);
void downScanPS2 (BYTE keynum);
void upScanAT (BYTE keynum);
void downScanAT (BYTE keynum);

void injectPS2Mouse(void);
void injectSerialMouse(void);
void injectMouse(void);
