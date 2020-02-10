extern int dummyInt;

   /* start function protocols */

void Initialize(void);
void SetColorsForColor(void);
void SetColorsForMono(void);
void SetColorsForLCD(void);
void GetEquipmentListAndParameters(void);
void GetSerialPortInformation(void);
void PatchMenus(void);
void CleanUp(int installFlag, int forceDialogFlags);
void WaitForAnyKey(void);
void ClearKeyboardBuffer(void);
void DummyFunction(void);
int DisplayStartupScreenText(int line, int center, int titleFlag, int autoLoadFlag);
int DisplayStartUpScreen(int autoLoadFlag);
int RunCountdownTimer(int line);

   /* end Function Protocols */
