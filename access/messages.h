#define MESSAGES	1

typedef struct
{
	char *text;
	int lastRecord;
} HELP_SCREEN_RECORD;

extern char *SlowNotValidWithBounceAlertText;
extern char *BounceNotValidWithSlowAlertText;
extern char *NoSlowWithBounceAlertText;
extern char *NoNoteWithFlashAlertText;
extern char *SerialKeysNotLoadedAlertText;
extern char *CannotChangeCommPortAlertText;
extern char *NoMouseFoundAlertText;
extern char *SerKeysMouseConflictAlertText;
extern char *NoSerialPortAvailableAlertText;
extern char *OneSerialPortWithMouseAlertText;
extern char *NoCommTwoAlertText;
extern char *ComputerNotFoundAlertText;
extern char *LoadSuccessfulAlertText;
extern char *LoadFailedAlertText;
extern char *SaveSuccessfulAlertText;
extern char *SaveFailedAlertText;


extern HELP_SCREEN_RECORD helpTextSaveParameters[], helpTextInstallAccess[], helpTextCancelAccess[];
extern HELP_SCREEN_RECORD helpTextMenu[], helpTextGeneral[], helpTextSticky[], helpTextMouse[], helpTextToggle[];
extern HELP_SCREEN_RECORD helpTextSerial[], helpTextKeyboard[], helpTextShowSounds[], helpTextTimeOut[], helpTextMisc[];



