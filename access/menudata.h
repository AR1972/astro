#define MENUDATA	1

typedef struct
{
	char *leftText;
	char *midText;
	char *rightText;
   char hotKey;
	int selectable;
	int hot;
	void (*dialogProcPtr)(void);
	void (*helpProcPtr)(void);
	int lastRecord;
   char *hintText;
} MENU_LIST_RECORD;


typedef struct
{
	char *leftText;
	char *midText;
	char *rightText;
   char hotKey;
	MENU_LIST_RECORD *menuList;
	int  lastRecord;
} MENU_BAR_RECORD;


typedef struct
{
	char *leftText;
	char *midText;
	char *rightText;
	char hotKey;
	int selectable;
	int hot;
	unsigned paramValue;
	int lastRecord;
} OPTION_LIST_RECORD;


typedef struct
{
	char *leftText;
	char *midText;
	char *rightText;
	char hotKey;
   int   selectable;
	OPTION_LIST_RECORD *optionList;
	void *paramPtr;
   int paramLength;
	int lastRecord;
	char *hintText;
} DIALOG_LIST_RECORD;


extern MENU_BAR_RECORD menuList[];
extern DIALOG_LIST_RECORD stickyKeysDialogList[], keyboardDialogList[], mouseKeysDialogList[], toggleKeysDialogList[];
extern DIALOG_LIST_RECORD serialKeysDialogList[], timeOutDialogList[], miscellaneousDialogList[], quitAccessDialogList[];


extern MENU_LIST_RECORD fileMenuList[], adjustMenuList[], helpMenuList[];

extern OPTION_LIST_RECORD dummyOptionList[];

extern OPTION_LIST_RECORD stickyKeysOnOption[], stickyFeedbackOption[], stickyTwoKeysOffOption[];
extern OPTION_LIST_RECORD stickyAudibleOption[], stickyClickOption[], stickyTriStateOption[];
extern DIALOG_LIST_RECORD stickyKeysDialogList[];


extern OPTION_LIST_RECORD slowKeysOnOption[], slowFeedbackOption[], slowClickOption[], slowAcceptanceOption[];
extern OPTION_LIST_RECORD repeatDelayOption[], repeatRateOption[], bounceTimeOption[];
extern DIALOG_LIST_RECORD keyboardDialogList[];


extern OPTION_LIST_RECORD mouseKeysOnOption[], mouseFeedbackOption[], mouseVelocityOption[], mouseAccelerationOption[];
extern DIALOG_LIST_RECORD mouseKeysDialogList[];


extern OPTION_LIST_RECORD toggleKeysOnOption[], toggleFeedbackOption[];
extern DIALOG_LIST_RECORD toggleKeysDialogList[];


extern OPTION_LIST_RECORD serialKeysOnOption[], serialBaudOption[], serialPortOption[];
extern DIALOG_LIST_RECORD serialKeysDialogList[];


extern OPTION_LIST_RECORD timeOutOnOption[], timeOutTimeOption[], timeOutFeedbackOption[];
extern DIALOG_LIST_RECORD timeOutDialogList[];


extern OPTION_LIST_RECORD visualNoteOnOption[];
extern OPTION_LIST_RECORD screenFlashOnOption[];
extern DIALOG_LIST_RECORD showSoundsDialogList[];


extern OPTION_LIST_RECORD spaceSaverOption[], computerOption[];
extern DIALOG_LIST_RECORD miscellaneousDialogList[];


/*extern DIALOG_LIST_RECORD quitAccessDialogList[];*/
