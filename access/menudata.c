#include "defines.h"
#include "globals.h"
#include "access.h"
#include "datablk.h"
#include "menudata.h"
#include "messages.h"
#include "dialogs.h"


MENU_LIST_RECORD fileMenuList[] =
{
	" ", "A", "bout AccessDOS... ", 'A',  SELECTABLE_YES,   FALSE, AboutAccess,		AboutAccess, 			FALSE,
         "View Information about this version of AccessDOS.",
	"",  "",  "",                   '\0', SELECTABLE_NEVER, FALSE, DummyFunction,		DummyFunction,			FALSE,
         "",
	" ", "S", "ave Settings...   ", 'S',  SELECTABLE_YES,   TRUE,  SaveParameters,	SaveParametersHelp,	FALSE,
         "Save the current AccessDOS settings.",
	"",  "",  "",                   '\0', SELECTABLE_NEVER, FALSE, DummyFunction,		DummyFunction,			FALSE,
         "",
	" ", "I", "nstall/Run...     ", 'I',  SELECTABLE_YES,   FALSE, InstallAccess,		InstallAccessHelp,	FALSE,
         "Install/Run AccessDOS with the current Settings.",
	" ", "C", "ancel/Quit...     ", 'C',  SELECTABLE_YES,   FALSE, CancelAccess,		CancelAccessHelp,		TRUE,
         "Cancel/Quit AccessDOS ignoring any changes made to settings.",
	"�", "�", "컴컴컴컴컴컴컴컴컴", '\0', SELECTABLE_NEVER, FALSE, DummyFunction,		DummyFunction,			FALSE,
         ""
};

MENU_LIST_RECORD adjustMenuList[] =
{
	" ", "S", "tickyKeys...        ", 'S',  SELECTABLE_YES,   TRUE,  StickyKeys,			StickyKeysHelp,		FALSE,
         "Make Adjustments to the StickyKeys feature.",
	" ", "M", "ouseKeys...         ", 'M',  SELECTABLE_YES,   FALSE, MouseKeys,			MouseKeysHelp,			FALSE,
         "Make Adjustments to the MouseKeys feature.",
	" ", "T", "oggleKeys...        ", 'T',  SELECTABLE_YES,   FALSE, ToggleKeys,			ToggleKeysHelp,		FALSE,
         "Make Adjustments to the ToggleKeys feature.",
	" ", "K", "eyboard Response... ", 'K',  SELECTABLE_YES,   FALSE, KeyboardPackage,	KeyboardPackageHelp,	FALSE,
         "Make Adjustments to the Keyboard Response feature.",
	"",  "",  "",                     '\0', SELECTABLE_NEVER, FALSE, DummyFunction,		DummyFunction,			FALSE,
         "",
	" S", "e", "rialKeys...        ", 'E',  SELECTABLE_YES,   FALSE, SerialKeys,			SerialKeysHelp,		FALSE,
         "Make Adjustments to the SerialKeys feature.",
	"",  "",  "",                     '\0', SELECTABLE_NEVER, FALSE, DummyFunction,		DummyFunction,			FALSE,
         "",
	" S", "h", "owSounds...        ", 'H',  SELECTABLE_YES,   FALSE, ShowSounds,			ShowSoundsHelp,		FALSE,
         "Make Adjustments to the ShowSounds feature.",
	" Time", "O", "ut...           ", 'O',  SELECTABLE_YES,   FALSE, TimeOut,				TimeOutHelp,			FALSE,
         "Make Adjustments to the TimeOut feature.",
	"",  "",  "",                     '\0', SELECTABLE_NEVER, FALSE, DummyFunction,		DummyFunction,			FALSE,
         "",
	" M", "i", "scellaneous...     ", 'I',  SELECTABLE_YES,   FALSE, Miscellaneous,		MiscellaneousHelp,	TRUE,
         "Make Miscellaneous adjustments.",
	"�", "�", "컴컴컴컴컴컴컴컴컴컴", '\0', SELECTABLE_NEVER, FALSE, DummyFunction,		DummyFunction,			FALSE,
         ""
};

MENU_LIST_RECORD helpMenuList[] =
{
	" Men", "u", " Help...              ", 'U',  SELECTABLE_YES,   TRUE,  MenuHelp,					MenuHelp,				FALSE,
         "View help on using the menu.",
	" ", "G", "eneral Help...           ", 'G',  SELECTABLE_YES,   FALSE, GeneralInfo,				GeneralInfo,			FALSE,
         "View some General Help Information.",
	"",  "",  "",                          '\0', SELECTABLE_NEVER, FALSE, DummyFunction,			DummyFunction,			FALSE,
         "",
	" ", "S", "tickyKeys Help...        ", 'S',  SELECTABLE_YES,   FALSE, StickyKeysHelp,			StickyKeysHelp,		FALSE,
         "View help for the StickyKeys feature.",
	" ", "M", "ouseKeys Help...         ", 'M',  SELECTABLE_YES,   FALSE, MouseKeysHelp,			MouseKeysHelp,			FALSE,
         "View help for the MouseKeys feature.",
	" ", "T", "oggleKeys Help...        ", 'T',  SELECTABLE_YES,   FALSE, ToggleKeysHelp,			ToggleKeysHelp,		FALSE,
         "View help for the ToggleKeys feature.",
	" ", "K", "eyboard Response Help... ", 'K',  SELECTABLE_YES,   FALSE, KeyboardPackageHelp,	KeyboardPackageHelp,	FALSE,
         "View help for Keyboard Response.",
	" S", "e", "rialKeys Help...        ", 'E',  SELECTABLE_YES,   FALSE, SerialKeysHelp,			SerialKeysHelp,		FALSE,
         "View help for the SerialKeys feature.",
	" S", "h", "owSounds Help...        ", 'H',  SELECTABLE_YES,   FALSE, ShowSoundsHelp,			ShowSoundsHelp,		FALSE,
         "View help for the ShowSounds feature.",
	" Time", "O", "ut Help...           ", 'O',  SELECTABLE_YES,   FALSE, TimeOutHelp,				TimeOutHelp,			FALSE,
         "View help for the TimeOut feature.",
	" M", "i", "scellaneous Features... ", 'I',  SELECTABLE_YES,   FALSE, MiscellaneousHelp,		MiscellaneousHelp,	TRUE,
         "View help for miscellaneous features.",
	"�", "�", "컴컴컴컴컴컴컴컴컴컴컴컴�", '\0', SELECTABLE_NEVER, FALSE, DummyFunction,			DummyFunction,			FALSE,
         ""
};


MENU_BAR_RECORD menuList[] =
{
	" ", "F", "ile ",   SCAN_F, fileMenuList,   FALSE,
	" ", "A", "djust ", SCAN_A, adjustMenuList, FALSE,
	" ", "H", "elp ",   SCAN_H, helpMenuList,   TRUE
};


OPTION_LIST_RECORD dummyOptionList[] =
{
   "", "", "", '\0', SELECTABLE_NEVER, FALSE, FALSE, TRUE
};


OPTION_LIST_RECORD stickyKeysOnOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD stickyFeedbackOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD stickyTwoKeysOffOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD stickyAudibleOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD stickyClickOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD stickyTriStateOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

DIALOG_LIST_RECORD stickyKeysDialogList[] =
{
	" ", "StickyKeys On........................:", " ", '\0', SELECTABLE_YES, stickyKeysOnOption,     &sStickyKeysOn,   1, FALSE,
         "Do you want StickyKeys On?",
	" ", "Sound when turned on or off..........:", " ", '\0', SELECTABLE_YES, stickyFeedbackOption,   &sOnOffFeedback,   1, FALSE,
         "Do you want to hear when StickyKeys turns on or off?",
	" ", "Sound when pressing a StickyKey......:", " ", '\0', SELECTABLE_YES, stickyAudibleOption,    &sAudibleFeedback, 1, FALSE,
         "Do you want to hear when you press a StickyKey?",
	" ", "Sound when pressing any key..........:", " ", '\0', SELECTABLE_YES, stickyClickOption,      &sClickOn,         1, FALSE,
         "Do you want to hear key clicks?",
	" ", "Turn off when pressing 2 keys at once:", " ", '\0', SELECTABLE_YES, stickyTwoKeysOffOption, &sTwoKeysOff,      1, FALSE,
         "Do you want StickyKeys to turn off when two keys are pressed simultaneously?",
	" ", "Lock MOD key when pressed twice......:", " ", '\0', SELECTABLE_YES, stickyTriStateOption,   &sTriState,        1, FALSE,
         "The MOD keys are the SHIFT, CONTROL, and ALTERNATE keys.",
   " ", " ", " ", '\0', SELECTABLE_NEVER, dummyOptionList, &dummyInt, 2, TRUE, " "
};




OPTION_LIST_RECORD slowKeysOnOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD slowFeedbackOption[] =
{
	" ", "On",  " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "Off", " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD slowClickOption[] =
{
	" ", "On",  " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "Off", " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD slowAcceptanceOption[] =
{
	" ", "Off", " ", '\0', SELECTABLE_YES, TRUE,   0,  FALSE,
	" ", "0.3", " ", '\0', SELECTABLE_YES, FALSE,  5,  FALSE,
	" ", "0.5", " ", '\0', SELECTABLE_YES, FALSE,  9,  FALSE,
	" ", "0.7", " ", '\0', SELECTABLE_YES, FALSE, 12,  FALSE,
	" ", "1.0", " ", '\0', SELECTABLE_YES, FALSE, 18,  FALSE,
	" ", "1.4", " ", '\0', SELECTABLE_YES, FALSE, 25,  FALSE,
	" ", "2.0", " ", '\0', SELECTABLE_YES, FALSE, 36,  TRUE
};

OPTION_LIST_RECORD repeatDelayOption[] =
{
	" ", "2.0",  " ", '\0', SELECTABLE_YES, FALSE, 36,    FALSE,
	" ", "1.5",  " ", '\0', SELECTABLE_YES, FALSE, 27,    FALSE,
	" ", "1.0",  " ", '\0', SELECTABLE_YES, FALSE, 18,    FALSE,
	" ", "0.75", " ", '\0', SELECTABLE_YES, FALSE, 13,    TRUE
};

OPTION_LIST_RECORD repeatRateOption[] =
{
	" ", "Off",  " ", '\0', SELECTABLE_YES, FALSE, 32760, FALSE,
	" ", "1.5",  " ", '\0', SELECTABLE_YES, TRUE,     27, FALSE,
	" ", "1.0",  " ", '\0', SELECTABLE_YES, FALSE,    18, FALSE,
	" ", "0.75", " ", '\0', SELECTABLE_YES, FALSE,    13, FALSE,
	" ", "0.5",  " ", '\0', SELECTABLE_YES, FALSE,     9, FALSE,
	" ", "0.25", " ", '\0', SELECTABLE_YES, FALSE,     4, TRUE
};

OPTION_LIST_RECORD bounceTimeOption[] =
{
	" ", "Off",  " ", '\0', SELECTABLE_YES, TRUE,  32760, FALSE,
	" ", "2.0",  " ", '\0', SELECTABLE_YES, FALSE,    36, FALSE,
	" ", "1.5",  " ", '\0', SELECTABLE_YES, FALSE,    27, FALSE,
	" ", "1.0",  " ", '\0', SELECTABLE_YES, FALSE,    18, FALSE,
	" ", "0.75", " ", '\0', SELECTABLE_YES, FALSE,    13, FALSE,
	" ", "0.5",  " ", '\0', SELECTABLE_YES, FALSE,     9, TRUE
};

DIALOG_LIST_RECORD keyboardDialogList[] =
{
	" ", "Enable Group.....:", " ",                  '\0', SELECTABLE_YES,   slowKeysOnOption,		&fFilterKeysOn,	1, FALSE,
         "Do you want to Enable this group of Keyboard functions as currently set?",
	" ", "Sound on Enable..:", " ",                  '\0', SELECTABLE_YES,   slowFeedbackOption,		&fOnOffFeedback,	1, FALSE,
         "Do you want to hear when this group is enabled or disabled?",
   " ", " ", " ",                                   '\0', SELECTABLE_NEVER, dummyOptionList,			&dummyInt,			2, FALSE,
         " ",
   " ", "RepeatKey Settings:", " ",                 '\0', SELECTABLE_TITLE, dummyOptionList,			&dummyInt,			2, FALSE,
         " ",
	" ", "Repeat Rate (seconds/character):", " ",    '\0', SELECTABLE_YES,   repeatRateOption,		&fRepeatTicks,		2, FALSE,
         "How fast do you want the keys to repeat (or do you want repeat turned off)?",
	" ", "Delay until repeat (seconds)...:", " ",    '\0', SELECTABLE_YES,   repeatDelayOption,		&fDelayTicks,		2, FALSE,
         "How many seconds should the keyboard wait before starting to repeat keys?",
   " ", "SlowKey Settings:", " ",                   '\0', SELECTABLE_TITLE, dummyOptionList,			&dummyInt,			2, FALSE,
         " ",
	" ", "Acceptance Delay (seconds).....:", " ",    '\0', SELECTABLE_YES,   slowAcceptanceOption,	&fWaitTicks,		2, FALSE,
         "How many seconds do you want the keyboard to wait before accepting a key?",
	" ", "Key Click Feedback.............:", " ",    '\0', SELECTABLE_YES,   slowClickOption,			&fClickOn,			1, FALSE,
         "Do you want to hear a sound when a key is depressed and when it is accepted?",
   " ", "BounceKeys Settings:", " ",                '\0', SELECTABLE_TITLE, dummyOptionList,			&dummyInt,			2, FALSE,
         " ",
  	" ", "Debounce time (seconds)........:", " ",    '\0', SELECTABLE_YES,   bounceTimeOption,		&fBounceTicks,		2, TRUE,
        "A key must be released for this long before the same key will be re-typed."
};



OPTION_LIST_RECORD mouseKeysOnOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD mouseFeedbackOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD mouseVelocityOption[] =
{
	" ",  "10", " ", '\0', SELECTABLE_YES, TRUE,   1, FALSE,
	" ",  "20", " ", '\0', SELECTABLE_YES, FALSE,  2, FALSE,
	" ",  "30", " ", '\0', SELECTABLE_YES, FALSE,  3, FALSE,
	" ",  "40", " ", '\0', SELECTABLE_YES, FALSE,  4, FALSE,
	" ",  "60", " ", '\0', SELECTABLE_YES, FALSE,  6, FALSE,
	" ",  "80", " ", '\0', SELECTABLE_YES, FALSE,  8, FALSE,
	" ", "120", " ", '\0', SELECTABLE_YES, FALSE, 12, FALSE,
	" ", "180", " ", '\0', SELECTABLE_YES, FALSE, 18, FALSE,
	" ", "360", " ", '\0', SELECTABLE_YES, FALSE, 36, TRUE
};

OPTION_LIST_RECORD mouseAccelerationOption[] =
{
	" ", "1", " ", '\0', SELECTABLE_YES, TRUE,  1, FALSE,
	" ", "2", " ", '\0', SELECTABLE_YES, FALSE, 2, FALSE,
	" ", "3", " ", '\0', SELECTABLE_YES, FALSE, 3, FALSE,
	" ", "4", " ", '\0', SELECTABLE_YES, FALSE, 4, TRUE
};

DIALOG_LIST_RECORD mouseKeysDialogList[] =
{
	" ", "MouseKeys On............:", " ", '\0', SELECTABLE_YES, mouseKeysOnOption,       &mMouseKeysOn,    1, FALSE,
         "Do you want MouseKeys On?",
	" ", "Sound when turned on/off:", " ", '\0', SELECTABLE_YES, mouseFeedbackOption,     &mOnOffFeedback,  1, FALSE,
         "Do you want to hear when MouseKeys turns on or off?",
	" ", "Max Speed (pix/sec).....:", " ", '\0', SELECTABLE_YES, mouseVelocityOption,     &mMaxSpeed,       2, FALSE,
         "Select the maximum speed of the mouse. (Pixels per second)",
	" ", "Time to Max Speed (secs):", " ", '\0', SELECTABLE_YES, mouseAccelerationOption, &mTimeToMaxSpeed, 2, FALSE,
         "Select the number of seconds it takes to reach the maximum speed.",
   " ", " ", " ", '\0', SELECTABLE_NEVER, dummyOptionList, &dummyInt, 2, TRUE, " "
};




OPTION_LIST_RECORD toggleKeysOnOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD toggleFeedbackOption[] =
{
	" ", "On",  " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "Off", " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

DIALOG_LIST_RECORD toggleKeysDialogList[] =
{
	" ", "ToggleKeys On..............:", " ", '\0', SELECTABLE_YES, toggleKeysOnOption,   &tToggleKeysOn,  1, FALSE,
         "Do you want ToggleKeys On?",
	" ", "Sound when turned on or off:", " ", '\0', SELECTABLE_YES, toggleFeedbackOption, &tOnOffFeedback, 1, FALSE,
         "Do you want to hear when ToggleKeys turns on or off?",
   " ", " ", " ", '\0', SELECTABLE_NEVER, dummyOptionList, &dummyInt, 2, TRUE, " "
};




OPTION_LIST_RECORD serialKeysOnOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD serialBaudOption[] =
{
	" ", "300",   " ", '\0', SELECTABLE_YES, TRUE,  BAUD300,   FALSE,
	" ", "600",   " ", '\0', SELECTABLE_YES, FALSE, BAUD600,   FALSE,
	" ", "1200",  " ", '\0', SELECTABLE_YES, FALSE, BAUD1200,  FALSE,
	" ", "2400",  " ", '\0', SELECTABLE_YES, FALSE, BAUD2400,  FALSE,
	" ", "4800",  " ", '\0', SELECTABLE_YES, FALSE, BAUD4800,  FALSE,
	" ", "9600",  " ", '\0', SELECTABLE_YES, FALSE, BAUD9600,  TRUE
};

OPTION_LIST_RECORD serialPortOption[] =
{
	" ", "COM1", " ", '\0', SELECTABLE_YES,   FALSE, 0x03f8, FALSE,
	" ", "COM2", " ", '\0', SELECTABLE_YES,   FALSE, 0x02f8, FALSE,
	" ", "COM3", " ", '\0', SELECTABLE_YES,   FALSE, 0,      FALSE,
	" ", "COM4", " ", '\0', SELECTABLE_YES,   FALSE, 0,      TRUE
};

DIALOG_LIST_RECORD serialKeysDialogList[] =
{
  	" ", "Install SerialKeys:", " ", '\0', SELECTABLE_YES, serialKeysOnOption, &skSerialKeysOn, 1, FALSE,
         "Do you want to install SerialKeys?",
  	" ", "Serial Port.......:", " ", '\0', SELECTABLE_YES, serialPortOption,   &skPortAddress,  2, FALSE,
         "Select the Serial Port to use for SerialKeys.",
  	" ", "Baud Rate.........:", " ", '\0', SELECTABLE_YES, serialBaudOption,   &skBaudRate,     2, FALSE,
         "Select the Baud Rate of your serial port.",
   " ", " ", " ", '\0', SELECTABLE_NEVER, dummyOptionList, &dummyInt, 2, TRUE, " "
};




OPTION_LIST_RECORD timeOutOnOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD timeOutTimeOption[] =
{
	" ", "5",  " ", '\0', SELECTABLE_YES, FALSE,  5400, FALSE,
	" ", "10", " ", '\0', SELECTABLE_YES, FALSE, 10800, FALSE,
	" ", "15", " ", '\0', SELECTABLE_YES, FALSE, 16200, FALSE,
	" ", "30", " ", '\0', SELECTABLE_YES, FALSE, 32400, TRUE
};

OPTION_LIST_RECORD timeOutFeedbackOption[] =
{
	" ", "On",  " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "Off", " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

DIALOG_LIST_RECORD timeOutDialogList[] =
{
	" ", "TimeOut On........................:", " ", '\0', SELECTABLE_YES,   timeOutOnOption,       &toTimeOut,       1, FALSE,
         "Do you want TimeOut to automatically turn AccessDOS off after idle delay?",
	" ", "Idle time before turnoff (minutes):", " ", '\0', SELECTABLE_YES,   timeOutTimeOption,     &toValue,         2, FALSE,
         "How many minutes should AccessDOS wait before automatically turning off?",
	" ", "Sound when turned off.............:", " ", '\0', SELECTABLE_YES,   timeOutFeedbackOption, &toOnOffFeedback, 1, FALSE,
         "Do you want to hear when TimeOut turns AccessDOS off?",
   " ", " ", " ", '\0', SELECTABLE_NEVER, dummyOptionList, &dummyInt, 2, TRUE, " "
};



OPTION_LIST_RECORD visualNoteOnOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD screenFlashOnOption[] =
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

DIALOG_LIST_RECORD showSoundsDialogList[] =
{
   " ", "Visual Note On..........:", " ", '\0', SELECTABLE_YES, visualNoteOnOption, &fHearingOn, 1, FALSE,
         "Do you want a visual indicator when your computer makes sounds?",
   " ", "Screen Flash On.........:", " ", '\0', SELECTABLE_YES, screenFlashOnOption, &fVideoFlash, 1, TRUE,
         "Do you want the screen to flash when your computer makes sounds?"
};



OPTION_LIST_RECORD spaceSaverOption[] = 
{
	" ", "Yes", " ", '\0', SELECTABLE_YES,   TRUE,  TRUE,  FALSE,
	"",  " ",   "",  '\0', SELECTABLE_NEVER, FALSE, FALSE, FALSE,
	" ", "No",  " ", '\0', SELECTABLE_YES,   FALSE, FALSE, TRUE
};

OPTION_LIST_RECORD computerOption[] = 
{
	" ", "1", " ",   '\0', SELECTABLE_YES, TRUE,  1, FALSE,
	" ", "2",   " ", '\0', SELECTABLE_YES, FALSE, 2, FALSE,
	" ", "3",  " ",  '\0', SELECTABLE_YES, FALSE, 3, TRUE
};

DIALOG_LIST_RECORD miscellaneousDialogList[] =
{
   " ", "Space Saving Keyboard.............:", " ", '\0', SELECTABLE_YES,   spaceSaverOption, &fSpaceSaver,  1, FALSE,
         "Are you using IBM's Space Saving keyboard on your computer?",
   " ", "Computer Type.....................:", " ", '\0', SELECTABLE_NEVER, computerOption,   &compDialogId, 1, TRUE,
         "1 = PC/PC-XT/PC-AT w/ 84 Key;  2 = PC-AT or Clone w/ 101 Key; 3 = '386 PC"
};



/*
DIALOG_LIST_RECORD quitAccessDialogList[] =
{
	"  ", "Install AccessDOS",                 "  ", '\0', SELECTABLE_YES, dummyOptionList, &dummyInt, 1, FALSE,
         "Install AccessDOS using the parameters as set in this session.",
	"  ", "Quit without installing AccessDOS", "  ", '\0', SELECTABLE_YES, dummyOptionList, &dummyInt, 1, FALSE,
         "Quit AccessDOS without installing.",
	"  ", "Cancel",                            "  ", '\0', SELECTABLE_YES, dummyOptionList, &dummyInt, 1, TRUE,
         "Press Escape to return to the menu."
};
*/
