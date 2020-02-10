#if !defined(MESSAGES)
#include "messages.h"		/* messages.h needed for declaration of HELP_SCREEN_RECORD */
#endif

#if !defined(MENUDATA)
#include "menudata.h"		/* messages.h needed for declaration of DIALOG_LIST_RECORD */
#endif

#define  PK_DONE           1
#define  PK_NOT_DONE       2
#define  PK_REDRAW_SCREEN  3


	/* begin Prototypes */

void DisplayNotImplementedMessage(char *messageText);

void AboutAccess(void);
void QuitAccess(int callingRoutine);
void InstallAccess(void);
void CancelAccess(void);

void StickyKeys(void);
void MouseKeys(void);
void ToggleKeys(void);
void SerialKeys(void);
void KeyboardPackage(void);
void ShowSounds(void);
void TimeOut(void);
void Miscellaneous(void);

void CancelAccessHelp(void);
void InstallAccessHelp(void);
void SaveParametersHelp(void);

void MenuHelp(void);
void GeneralInfo(void);
void StickyKeysHelp(void);
void MouseKeysHelp(void);
void ToggleKeysHelp(void);
void SerialKeysHelp(void);
void KeyboardPackageHelp(void);
void ShowSoundsHelp(void);
void TimeOutHelp(void);
void MiscellaneousHelp(void);

void DisplayTextInHelpDialogBox(char *dialogText, int height, int width);
void ManageHelpDialogBox(HELP_SCREEN_RECORD *helpScreen, char *title);
int PrintOptionLists(DIALOG_LIST_RECORD *currentDialogList, int currentOption, int numberOfOptionLists,
							int top, int left, int bottom, int right, int alignmentColumn);
int ProcessKeystroke(DIALOG_LIST_RECORD *currentDialogList, int *currentOptionPtr, int numberOfOptionLists, void (*helpProcPtr)(void));
void ManageDialogBox(DIALOG_LIST_RECORD *currentDialogList, char *title, void (*helpProcPtr)());
int CalculateDialogBoxSize(DIALOG_LIST_RECORD *currentDialogList, int *top, int *left, int *bottom, int *right,
									int extraHeight, int extraWidth);
void DrawDialogBox(int top, int left, int bottom, int right, short borderFG, long borderBG, short insideFG, long insodeBG, char *title);
void DisplayAlertBox(char *alertText, int alertBoxHeight, int alertBoxWidth);
void PrintOkCancelButtons(int line, int width, int okHot, int cancelHot);
void SetOptionListHotFlags(DIALOG_LIST_RECORD *currentDialogList, int numberOfOptionLists);
void GetOptionListHotFlags(DIALOG_LIST_RECORD *currentDialogList, int numberOfOptionLists);

	/* end Prototypes */
