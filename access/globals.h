
	/* the following buffers are used to construct lines of text for printing with _outtext()
		backgroundString holds a copy of the characters that make up the desktop */
extern char charBuffer[], tempBuffer[], backgroundString[];
extern int charBufferLength, tempBufferLength;


	/* The following variables hold the original values for the screen color and cursor size so we can reset after */
extern long oldBkColor;
extern short oldTextColor, oldTextCursor;

	/* Used to hold the values for reading the keyboard using _bios_keybrd() */
extern int keybrdRead, keybrdReady, keybrdShiftStatus;

	/* address of shared parameter block in the first instance of ADOS.COM */
extern unsigned char _far *currentParameters;

	/* the variables below are local copies of data from the shared parameter block */
extern unsigned char skSerialKeysOn, skSerialKeysLoaded, skInterrupt;
extern unsigned int  skPortAddress, skBaudRate, oldPortAddress;
extern unsigned int  serialPort1, serialPort2, serialPort3, serialPort4, numberOfSerialPorts;

extern unsigned char fFilterKeysOn, fOnOffFeedback;
extern unsigned int  fWaitTicks, fDelayTicks, fRepeatTicks, fBounceTicks;
extern unsigned char fMaxDefault, fClickOn, fBounceOn, fDialogFilterKeysOff;

extern unsigned char sStickyKeysOn, sOnOffFeedback, sAudibleFeedback, sTriState, sTwoKeysOff, sDialogStickyKeysOff, sClickOn;

extern unsigned char mMouseKeysOn, mOnOffFeedback;
extern unsigned int  mMaxSpeed, mTimeToMaxSpeed;
extern unsigned char mDialogMouseKeysOff, mMouseKeysOverride;

extern unsigned char tToggleKeysOn, tOnOffFeedback, tDialogToggleKeysOff;

extern unsigned char toTimeOut, toOnOffFeedback;
extern unsigned int  toValue;
extern unsigned char toDialogTimeOutOff;

extern unsigned char compDialog, compDialogId, compDialogAction, fSpaceSaver;
extern unsigned char fComputerNotFound, fHearingOn, fVideoFlash, fAccessAlreadyLoaded;

extern unsigned char eqButton1, eqButton2, eqCurrentButton, eqComputerId, eqMouseDriver, eqMouseId;
extern unsigned int  eqCommBase, eqExtendedSegment;
extern unsigned char eqVector, eqInjectKeys, eqVideoType, eqComputerFlag;

	/* global storage used to construct the configuration filename from the data on the command line */
extern char configurationFilename[], *configurationFileExtension;

	/* boolean flags to determine whether or not to install AccessDOS, quit the user interface,
		what video mode to run in, and whether or not to display debug information throughout the user inferface */
extern int displayModeFlag, installFlag, quitFlag, debug;

   /* Colors for various features of the menu */
extern long desktopBG, titleBarBG, hintBarBG, menuBarBG, menuBarHotBG, menuBarHotKeyBG;
extern long menuLineBG, menuLineHotBG, menuLineDeadBG, menuLineHotKeyBG, menuBoxBG;
extern long dialogBoxBG, dialogBoxHotBG, dialogBoxDeadBG, dialogBoxBorderBG, dialogBoxArrowBG, alertBoxBG;
extern short desktopFG, titleBarFG, hintBarFG, menuBarFG, menuBarHotFG, menuBarHotKeyFG;
extern short menuLineFG, menuLineHotFG, menuLineDeadFG, menuLineHotKeyFG, menuBoxFG;
extern short dialogBoxFG, dialogBoxHotFG, dialogBoxDeadFG, dialogBoxBorderFG, dialogBoxArrowFG, alertBoxFG;
