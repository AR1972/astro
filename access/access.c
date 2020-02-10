#include <bios.h>          /* Microsoft Library Includes */
#include <graph.h>
#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <dos.h>
#include <string.h>
#include <memory.h>
#include <process.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>

#include "defines.h"       /* Access DOS Header Files */
#include "messages.h"
#include "menudata.h"
#include "access.h"
#include "dialogs.h"
#include "datablk.h"
#include "frontend.h"

#define peek(addr)		 (*(unsigned char far *)addr)

	/* the following buffers are used to construct lines of text for printing with _outtext()
		backgroundString holds a copy of the characters that make up the desktop */
char charBuffer[2*LINE_LENGTH+1], tempBuffer[2*LINE_LENGTH+1], backgroundString[LINE_LENGTH+1];
int charBufferLength, tempBufferLength;

	/* The following variables hold the original values for the screen color and cursor size so we can reset after */
long oldBkColor;
short oldTextColor, oldTextCursor;

	/* Used to hold the values for reading the keyboard using _bios_keybrd() */
int keybrdRead, keybrdReady, keybrdShiftStatus;

	/* address of shared parameter block in the first instance of ADOS.COM */
unsigned char _far *currentParameters;

int dummyInt;		/* used for in menus as place holder */

	/* the variables below are local copies of data from the shared parameter block */
unsigned char skSerialKeysOn, skSerialKeysLoaded, skInterrupt;
unsigned int  skPortAddress, skBaudRate, oldPortAddress;
unsigned int  serialPort1, serialPort2, serialPort3, serialPort4, numberOfSerialPorts;

unsigned char fFilterKeysOn, fOnOffFeedback;
unsigned int  fWaitTicks, fDelayTicks, fRepeatTicks, fBounceTicks;
unsigned char fMaxDefault, fClickOn, fBounceOn, fDialogFilterKeysOff;

unsigned char sStickyKeysOn, sOnOffFeedback, sAudibleFeedback, sTriState, sTwoKeysOff, sDialogStickyKeysOff, sClickOn;

unsigned char mMouseKeysOn, mOnOffFeedback;
unsigned int  mMaxSpeed, mTimeToMaxSpeed;
unsigned char mDialogMouseKeysOff, mMouseKeysOverride;

unsigned char tToggleKeysOn, tOnOffFeedback, tDialogToggleKeysOff;

unsigned char toTimeOut, toOnOffFeedback;
unsigned int  toValue;
unsigned char toDialogTimeOutOff;

unsigned char compDialog, compDialogId, compDialogAction, fSpaceSaver;
unsigned char fComputerNotFound, fHearingOn, fVideoFlash, fAccessAlreadyLoaded;

unsigned char eqButton1, eqButton2, eqCurrentButton, eqComputerId, eqMouseDriver, eqMouseId;
unsigned int  eqCommBase, eqExtendedSegment;
unsigned char eqVector, eqInjectKeys, eqVideoType, eqComputerFlag;

	/* boolean flags to determine whether or not to install AccessDOS, quit the user interface,
		what video mode to run in, and whether or not to display debug information throughout the user inferface */
int installFlag = FALSE, quitFlag = FALSE, displayModeFlag = DEFAULT_MODE, debug = FALSE;

   /* Colors for various features of the menu */
long desktopBG, titleBarBG, hintBarBG, menuBarBG, menuBarHotBG, menuBarHotKeyBG;
long menuLineBG, menuLineHotBG, menuLineDeadBG, menuLineHotKeyBG, menuBoxBG;
long dialogBoxBG, dialogBoxHotBG, dialogBoxDeadBG, dialogBoxBorderBG, dialogBoxArrowBG, alertBoxBG;
short desktopFG, titleBarFG, hintBarFG, menuBarFG, menuBarHotFG, menuBarHotKeyFG;
short menuLineFG, menuLineHotFG, menuLineDeadFG, menuLineHotKeyFG, menuBoxFG;
short dialogBoxFG, dialogBoxHotFG, dialogBoxDeadFG, dialogBoxBorderFG, dialogBoxArrowFG, alertBoxFG;

	/* global storage used to construct the configuration filename from the data on the command line */
char driveLetter[_MAX_DRIVE], directoryName[_MAX_DIR], fileName[_MAX_FNAME], fileExtension[_MAX_EXT];
char configurationFilename[_MAX_PATH], *configurationFileExtension = "CFG";

int main(int argc, char *argv[])
{
	int autoLoadFlag = FALSE, forceDialogFlags = FALSE;
	int aCommandLineSwitchFlag = FALSE, xCommandLineSwitchFlag = FALSE;
	int i, desiredOperation;
	int CommandSwitchWatch = FALSE;
		/* read the command line and set the appropriate flags */
   for (i=1; i<argc; i++)
   {
      char *charPtr = argv[i];

      if (*charPtr == '-' || *charPtr == '/')
      {
         charPtr++;

         switch (toupper(*charPtr))
         {
			 	case 'X' :	xCommandLineSwitchFlag = TRUE;
							 	break;

            case 'A' :  aCommandLineSwitchFlag = TRUE;
                        break;

            case 'C' :  displayModeFlag = COLOR_MODE;
                        break;

            case 'M' :  displayModeFlag = MONO_MODE;
                        break;

            case 'L' :  displayModeFlag = LCD_MODE;
                        break;

            case 'D' :  debug = TRUE;
                        break;

	     case 'I' :  CommandSwitchWatch = TRUE;
								break;
		
            case 'S' :	if (toupper(*(charPtr+1)) == 'K') break;		/* SerialKeys -- access.c doesn't need to do anything */

            default  :  

/* changed 11/92 _outtext due to a request since it caused DOS line 25 read problems, and also to conform more
closely with what other DOS 5.0 programs output for messages when typping /? for help   */

			 /* _outtext("\n\nAn invalid command line switch was entered\n");  don't need this */
                        /* _outtext("Valid switches are:\n");  or this */
			
			 printf(" Starts AccessDOS, which is a package of DOS extensions \n");
			 printf(" for keyboard, mouse and sound access.\n\n");
			 printf(" ados [/A] [/C] [/M] [/L] [/X] \n\n");
                        printf("   /A -- Automatically install AccessDOS. \n");
                        printf("   /C -- force AccessDOS to run in Color mode.\n");
                        printf("   /M -- force AccessDOS to run in Monochrome mode.\n");
                        printf("   /L -- force AccessDOS to run in LCD mode.\n");
                        printf("   /X -- force AccessDOS to run in minimal mode.\n");

/*	     		_outtext("   /I -- inhibit mouse hook in Task Switcher mode.\n\n");  keep hidden */
                        exit(FALSE);
                        break;
         }
      }
   }

		/* create the configuration file name from the path passed on the command line */
   _splitpath(argv[0], driveLetter, directoryName, fileName, fileExtension);
   _makepath(configurationFilename, driveLetter, directoryName, fileName, configurationFileExtension);

	Initialize();

	if ((fAccessAlreadyLoaded)&&(CommandSwitchWatch))
		{
/* removed _outtext here also, see note above */
			printf("\n\nYou cannot change MouseKeys status after AccessDOS \n");
			printf("is loaded.  Please unload AccessDOS and use this switch \n");
			printf("when installing AccessDOS.\n\n");
			exit(FALSE);
		}

	if (aCommandLineSwitchFlag) autoLoadFlag = TRUE;
	else autoLoadFlag = FALSE;

   if (fComputerNotFound)
   {
       int height = 11, width = 58;
	_clearscreen(_GCLEARSCREEN);
       DisplayAlertBox(ComputerNotFoundAlertText, height, width);
       autoLoadFlag = FALSE;
   }

	if (fAccessAlreadyLoaded)
	{																				/* if AccessDOS is already running */
		autoLoadFlag = FALSE;												/*		force the user to the menu */

	}
	else																			/* if this is the initial running of AccessDOS */
	{
		if (eqMouseId == 2) mMaxSpeed = 4;								/* Slow the default mouse speed for a Serial Mouse */
		if (xCommandLineSwitchFlag && !aCommandLineSwitchFlag)	/* if there is an X on the command line, there was an error */
		{																			/*		opening the configuration file, (the X should have */
			int height = 9, width = 60;									/* 	been trapped by the parent program and control should */
																					/* 	not have been passed to us here) */
			DisplayAlertBox(LoadFailedAlertText, height, width);
			autoLoadFlag = FALSE;
		}
		if (aCommandLineSwitchFlag)										/* if there is an A on the command line, make sure that the */
		{																			/* file can be read before starting the count down */
			int height = 9, width = 60, fileHandle = open(configurationFilename, O_RDWR | O_BINARY);
			if (fileHandle == -1)											/* if the file cannot be read, display error and go to menu */
			{
				DisplayAlertBox(LoadFailedAlertText, height, width);
				autoLoadFlag = FALSE;
			}
			else close(fileHandle);
		}
	}

	if (fAccessAlreadyLoaded)
		{
		desiredOperation = CONTROL_PANEL;
		}
	else if (autoLoadFlag) /* if starting with '/a', do count down screen */
		{
		desiredOperation = DisplayStartUpScreen(autoLoadFlag);		
		}
	else /* for all other cold starts, go to menus  when not previously loaded! */
		{
		desiredOperation = CONTROL_PANEL;
		}
		 
	switch (desiredOperation)
   {
		case CONTROL_PANEL	:	ManageMenu();
										break;

		case AUTOLOAD			:	if (LoadParameters()) installFlag = forceDialogFlags = TRUE;
                              else ManageMenu();
										break;
   }
	CleanUp(installFlag, forceDialogFlags);

	if (installFlag) return(INSTALL_ACCESS);
	else return (ABORT_ACCESS);
}  /* end main() */


void Initialize(void)
{
   keybrdRead        = _KEYBRD_READ;
   keybrdReady       = _KEYBRD_READY;
   keybrdShiftStatus = _KEYBRD_SHIFTSTATUS;

   /* If bit 4 of the byte at 0x0040:0x0096 is set, the new keyboard
    * is present.
    */
/*
	if(peek(0x00400096) & 0x10)
   {
      keybrdRead = _NKEYBRD_READ;
      keybrdReady = _NKEYBRD_READY;
      keybrdShiftStatus = _NKEYBRD_SHIFTSTATUS;
   }
*/

	GetInterruptNumber();
   GetEquipmentListAndParameters();
   PatchMenus();

		/* if there was not a switch on the command line for the video mode,
			set mode as detected by the Find_Computer routine (eqVideoType) */
   if (displayModeFlag == DEFAULT_MODE)
		switch (eqVideoType)
		{
			case 1 :														/* Monochrome Display Adapter */
			case 4 :	displayModeFlag = MONO_MODE;	break;	/* Monochrome EGA */

			case 3 :														/* CGA */
			case 5 :														/* EGA */
			case 7 : displayModeFlag = COLOR_MODE;	break;	/* VGA */

			default: displayModeFlag = MONO_MODE;	break;	/* Unknown video */
		}

   switch (displayModeFlag)
   {
      case COLOR_MODE   :  _setvideomode(_TEXTC80);
                           SetColorsForColor();
                           break;

      case MONO_MODE    :  _setvideomode(_TEXTBW80);
                           SetColorsForMono();
                           break;

      case LCD_MODE     :  _setvideomode(_TEXTMONO);
                           SetColorsForLCD();

										/* make all characters lowercase and Hot key uppercase */
									{
										int i, j;
										MENU_LIST_RECORD *currentMenuList;
										char *charPtr;

										i=0;
										do
										{
											currentMenuList = menuList[i].menuList;
											j=0;
											do
											{
												charPtr = currentMenuList[j].leftText;
												do
												{
													*charPtr = (char) tolower(*charPtr);
													charPtr++;
												}	while (*charPtr);

												charPtr = currentMenuList[j].midText;
												*charPtr = (char) toupper(*charPtr);

												charPtr = currentMenuList[j].rightText;
												do
												{
													*charPtr = (char) tolower(*charPtr);
													charPtr++;
												}	while (*charPtr);

											} while (currentMenuList[j++].lastRecord == FALSE);
										} while (menuList[i++].lastRecord == FALSE);
									}
                           break;
   }
	oldTextCursor = _settextcursor(0x2000);

   memset(backgroundString, 'ฑ', 80);     /* <176> = ฐ; <177> = ฑ; <178> = ฒ */
   backgroundString[80] = '\0';
}  /* end Initialize() */


void SetColorsForColor(void)
{
   desktopFG         = CYAN;
   desktopBG         = BLUE;

   titleBarFG        = BLACK;
   titleBarBG        = CYAN;

   hintBarFG         = YELLOW;
   hintBarBG         = BLUE;

   menuBarFG         = YELLOW;
   menuBarBG         = BLUE;
   menuBarHotFG      = BLUE;
   menuBarHotBG      = WHITE;
   menuBarHotKeyFG   = LIGHT_CYAN;
   menuBarHotKeyBG   = BLUE;

   menuLineFG        = YELLOW;
   menuLineBG        = BLUE;
   menuLineHotFG     = BLUE;
   menuLineHotBG     = WHITE;
   menuLineDeadFG    = WHITE;
   menuLineDeadBG    = BLUE;
   menuLineHotKeyFG  = LIGHT_CYAN;
   menuLineHotKeyBG  = BLUE;

   menuBoxFG         = YELLOW;
   menuBoxBG         = BLUE;

   dialogBoxFG       = YELLOW;
   dialogBoxBG       = BLUE;
	dialogBoxHotFG		= BLUE;
	dialogBoxHotBG		= WHITE;
	dialogBoxDeadFG	= WHITE;
	dialogBoxDeadBG	= BLUE;
   dialogBoxBorderFG = BRIGHT_WHITE;
   dialogBoxBorderBG = BLUE;
   dialogBoxArrowFG  = BRIGHT_WHITE;
   dialogBoxArrowBG  = BLUE;

   alertBoxFG        = BRIGHT_WHITE;
   alertBoxBG        = RED;
}  /* end SetColordForColor() */


void SetColorsForMono(void)
{
   desktopFG         = BLACK;
   desktopBG         = WHITE;

   titleBarFG        = BRIGHT_WHITE;
   titleBarBG        = BLACK;

   hintBarFG         = BLACK;
   hintBarBG         = WHITE;

   menuBarFG         = BLACK;
   menuBarBG         = WHITE;
   menuBarHotFG      = WHITE;
   menuBarHotBG      = BLACK;
   menuBarHotKeyFG   = BRIGHT_WHITE;
   menuBarHotKeyBG   = BLACK;

   menuLineFG        = BRIGHT_WHITE;
   menuLineBG        = BLACK;
   menuLineHotFG     = BLACK;
   menuLineHotBG     = WHITE;
   menuLineDeadFG    = WHITE;
   menuLineDeadBG    = BLACK;
   menuLineHotKeyFG  = WHITE;
   menuLineHotKeyBG  = BLACK;

   menuBoxFG         = BRIGHT_WHITE;
   menuBoxBG         = BLACK;

   dialogBoxFG       = WHITE;
   dialogBoxBG       = BLACK;
	dialogBoxHotFG		= BLACK;
	dialogBoxHotBG		= WHITE;
	dialogBoxDeadFG	= WHITE;
	dialogBoxDeadBG	= BLACK;
   dialogBoxBorderFG = BRIGHT_WHITE;
   dialogBoxBorderBG = BLACK;
   dialogBoxArrowFG  = BRIGHT_WHITE;
   dialogBoxArrowBG  = BLACK;

   alertBoxFG        = BLACK;
   alertBoxBG        = WHITE;
}  /* end SetColorsForMono() */


void SetColorsForLCD(void)
{
   desktopFG         = BLACK;
   desktopBG         = BLACK;

   titleBarFG        = WHITE;
   titleBarBG        = BLACK;

   hintBarFG         = BLACK;
   hintBarBG         = WHITE;

   menuBarFG         = BLACK;
   menuBarBG         = WHITE;
   menuBarHotFG      = WHITE;
   menuBarHotBG      = BLACK;
   menuBarHotKeyFG   = BLACK;
   menuBarHotKeyBG   = WHITE;

   menuLineFG        = BRIGHT_WHITE;
   menuLineBG        = BLACK;
   menuLineHotFG     = BLACK;
   menuLineHotBG     = WHITE;
   menuLineDeadFG    = WHITE;
   menuLineDeadBG    = BLACK;
   menuLineHotKeyFG  = WHITE;
   menuLineHotKeyBG  = BLACK;

   menuBoxFG         = BRIGHT_WHITE;
   menuBoxBG         = BLACK;

   dialogBoxFG       = WHITE;
   dialogBoxBG       = BLACK;
	dialogBoxHotFG		= BLACK;
	dialogBoxHotBG		= WHITE;
	dialogBoxDeadFG	= WHITE;
	dialogBoxDeadBG	= BLACK;
   dialogBoxBorderFG = BRIGHT_WHITE;
   dialogBoxBorderBG = BLACK;
   dialogBoxArrowFG  = BRIGHT_WHITE;
   dialogBoxArrowBG  = BLACK;

   alertBoxFG        = WHITE;
   alertBoxBG        = BLACK;
}  /* end SetColorsForLCD() */


void GetEquipmentListAndParameters(void)
{
   GetAddressOfParameterBlock();       /* this routine checks for the existance of ACCESS.COM in RAM and aborts if not found */
	GetAccessParameters();
/*
   if (fAccessAlreadyLoaded == FALSE)
   {
	   FindMouse();
	   FindComputer();
   	SetEquipmentParameters();
   	GetAccessParameters();
   }
   else GetEquipmentParameters();
*/
   GetEquipmentParameters();
	GetSerialPortInformation();

	if (debug)
	{
   	DisplayEquipmentParameters();
   	DisplayCurrentParameters();
	}
}  /* end GetEquipmentListAndParameters() */


void GetSerialPortInformation(void)
{
	_segment biosSegment = 0x040;
	unsigned int _based(biosSegment) *commPorts = 0;
	int i;

	numberOfSerialPorts = 0;
	serialPort1 = serialPort2 = serialPort3 = serialPort4 = 0;

	if (debug) _settextposition(11, 1);

	for (i=0; i<4; i++)
	{
		if (debug)
		{
			charBufferLength = sprintf(charBuffer, "40:%02X=0x%04X  ", i, commPorts[i]);
			_outtext(charBuffer);
		}

		if (commPorts[i])
		{
			++numberOfSerialPorts;
			switch (commPorts[i])
			{
				case 0x03f8	: serialPort1 = commPorts[i]; break;

				case 0x02f8	: serialPort2 = commPorts[i]; break;

				case 0x03e8	:
				case 0x3220	:
				case 0xb220	: serialPort3 = commPorts[i]; break;

				case 0x02e8	:
				case 0x3228	:
				case 0xb228	: serialPort4 = commPorts[i]; break;

				default		: --numberOfSerialPorts; break;
			}
		}
	}

	if (debug)
	{
		charBufferLength = sprintf(charBuffer, "\nnumberOfSerialPorts found: %d\n", numberOfSerialPorts);
		_outtext(charBuffer);
		charBufferLength = sprintf(charBuffer, "serial1: 0x%04X  serial2: 0x%04X  serial3: 0x%04X  serial4: 0x%04X\n",
											serialPort1, serialPort2, serialPort3, serialPort4);
		if (charBufferLength > LINE_LENGTH) exit(ABORT_ACCESS);
		else _outtext(charBuffer);
	}
}	/* end GetSerialPortInformation() */


void PatchMenus(void)
{
	int i, validSerialPort = FALSE;

	if (serialPort1) serialPortOption[0].paramValue = serialPort1;
	else serialPortOption[0].selectable = SELECTABLE_NO;
	if (serialPort2) serialPortOption[1].paramValue = serialPort2;
	else serialPortOption[1].selectable = SELECTABLE_NO;
	if (serialPort3) serialPortOption[2].paramValue = serialPort3;
	else serialPortOption[2].selectable = SELECTABLE_NO;
	if (serialPort4) serialPortOption[3].paramValue = serialPort4;
	else serialPortOption[3].selectable = SELECTABLE_NO;

   if (eqMouseId == 0)
   {
      i = 0;
      do
      {
         if (adjustMenuList[i].dialogProcPtr == MouseKeys)
         {
            adjustMenuList[i].selectable = SELECTABLE_NO;
				adjustMenuList[i].hotKey = '\0';
				if (displayModeFlag == LCD_MODE)
				{
					*(adjustMenuList[i].leftText) = '(';
 					*((adjustMenuList[i].rightText)+strlen(adjustMenuList[i].rightText)-1) = ')';
				}
            break;
         }
      } while (adjustMenuList[i++].lastRecord == FALSE);
   }

	if (eqCommBase == 0x3f8) serialPortOption[0].selectable = SELECTABLE_NO;	/* mouse is on comm1 so serialKeys can't be */
	if (eqCommBase == 0x2f8) serialPortOption[1].selectable = SELECTABLE_NO;	/* mouse is on comm2 so serialKeys can't be */

	for (i=0; i<4; i++) validSerialPort |= (serialPortOption[i].selectable == SELECTABLE_YES);
   if (validSerialPort == FALSE)
   {
      i = 0;
      do
      {
         if (adjustMenuList[i].dialogProcPtr == SerialKeys)
         {
            adjustMenuList[i].selectable = SELECTABLE_NO;
				adjustMenuList[i].hotKey = '\0';
				if (displayModeFlag == LCD_MODE)
				{
					*(adjustMenuList[i].leftText) = '(';
 					*((adjustMenuList[i].rightText)+strlen(adjustMenuList[i].rightText)-1) = ')';
				}
            break;
         }
      } while (adjustMenuList[i++].lastRecord == FALSE);
   }
	else
	{
		for (i=0; i<4; i++)
		{
			if (serialPortOption[i].selectable == SELECTABLE_YES)
			{
				serialPortOption[i].hot = TRUE;
				skPortAddress = serialPortOption[i].paramValue;
				break;
			}
		}
	}

   if (fComputerNotFound)
   {
      i = 0;
      do
      {
         if (miscellaneousDialogList[i].optionList == computerOption)
         {
            miscellaneousDialogList[i].selectable = SELECTABLE_YES;
            break;
         }
      } while (miscellaneousDialogList[i++].lastRecord == FALSE);
      compDialog = TRUE;
   }

		/* if the computer is a PS/2 model 25 or 30/86 then we can't flash the video */
	if (eqComputerId == 6)
	{
      i = 0;
      do
      {
         if (showSoundsDialogList[i].optionList == screenFlashOnOption)
         {
            showSoundsDialogList[i].selectable = SELECTABLE_NEVER;
            break;
         }
      } while (showSoundsDialogList[i++].lastRecord == FALSE);
	}
}  /* end PatchMenus() */


void CleanUp(int installFlag, int forceDialogFlags)
{
   _settextwindow(1, 1, 25, 80);
   _clearscreen(_GCLEARSCREEN);

   if (installFlag) SetAccessParameters(currentParameters, forceDialogFlags);
   if (fComputerNotFound)
   {
/*      FindComputer();    /* call this again so Mark can reset the computer type */
      SetEquipmentParameters();
   }

	if (debug)
	{
   	DisplayEquipmentParameters();
   	DisplayCurrentParameters();
	}

	_setvideomode(_DEFAULTMODE);
	if (eqComputerId == 6) _settextcursor(0x0707);
	else _settextcursor(oldTextCursor);
}	/* end CleanUp() */


void WaitForAnyKey(void)
{
   _bios_keybrd(keybrdRead);
}  /* end WaitForAnyKey() */


void ClearKeyboardBuffer(void)
{
	while (_bios_keybrd(keybrdReady)) _bios_keybrd(keybrdRead);
}	/* ClearKeyboardBuffer() */


void DummyFunction(void)
{
}	/* end DummyFunction() */


int DisplayStartupScreenText(int line, int center, int titleFlag, int autoLoadFlag)
{
	int timeOutLine;

	if (titleFlag)
	{
		charBufferLength = sprintf(charBuffer, "AccessDOS 1.1");
   	_settextposition(line++, center-charBufferLength/2);
   	_outtext(charBuffer);
	}
	line++;

	charBufferLength = sprintf(charBuffer, "A package of DOS extensions");
   _settextposition(line++, center-charBufferLength/2);
   _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "for keyboard, mouse and sound access. ");
   _settextposition(line++, center-charBufferLength/2);
   _outtext(charBuffer);

	if (autoLoadFlag)
	{
		timeOutLine = line+1;
		line += 8;
	}
	else
	{
		timeOutLine = 0;
		line += 3;
	}

	charBufferLength = sprintf(charBuffer, "Trace R&D Center");
   _settextposition(line++, center-charBufferLength/2);
   _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "Waisman Center and Industrial Engineering Department");
   _settextposition(line++, center-charBufferLength/2);
   _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "University of Wisconsin-Madison");
   _settextposition(line++, center-charBufferLength/2);
   _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "Copyright - Board of Regents, University of Wisconsin System");
   _settextposition(line++, center-charBufferLength/2);
   _outtext(charBuffer);
   line++;

	charBufferLength = sprintf(charBuffer, "Support for this work was provided in part by");
   _settextposition(line++, center-charBufferLength/2);
   _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "IBM Corporation");
   _settextposition(line++, center-charBufferLength/2);
   _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "and by the");
   _settextposition(line++, center-charBufferLength/2);
   _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "National Institute on Disability and Rehabilitation Research (NIDRR)");
   _settextposition(line++, center-charBufferLength/2);
   _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "US Department of Education under Grant #H133E80021");
   _settextposition(line++, center-charBufferLength/2);
   _outtext(charBuffer);

   return timeOutLine;
}  /* end DisplayStartupScreenText() */


int DisplayStartUpScreen(int autoLoadFlag)
{
	int line, timeOutLine;
	int key, ascii;
   int keyPressed = FALSE, helpMessageFlag = FALSE;

	if (autoLoadFlag)
   {
      line = 3;
      charBufferLength = sprintf(charBuffer, "");
   }
   else
   {
   	line = 5;
   	charBufferLength = sprintf(charBuffer, "Press any key to continue.");
   }

//  	OutputHintBar(charBuffer, JUSTIFY_CENTER, helpMessageFlag);
//	OutputTitleBar();

//	_settextwindow(2, 1, 24, 80);
   _settextcolor(dialogBoxFG);
   _setbkcolor(dialogBoxBG);
   _clearscreen(_GWINDOW);
	_settextwindow(1, 1, 25, 80);

// removed since we do not want to display any text in this screen on cold install
// unless user does '/a' command line switch, then give them the count down option   
//   timeOutLine = DisplayStartupScreenText(line, 40, displayModeFlag == COLOR_MODE, autoLoadFlag);

// borrowed from 'DisplayStartupScreenText' since it is no longer called
	if (autoLoadFlag)
	{
		timeOutLine = line+1;
		line += 8;
	}
	else
	{
		timeOutLine = 0;
		line += 3;
	}

	_settextwindow(1, 1, 25, 80);

   if (autoLoadFlag) keyPressed = RunCountdownTimer(timeOutLine);

   if (autoLoadFlag == FALSE)
   {
		ClearKeyboardBuffer();
      _bios_keybrd(keybrdRead);
      return(CONTROL_PANEL);
   }

   if (keyPressed == TRUE) key = _bios_keybrd(keybrdRead);
   else key = 0;
   ascii = key & 0x00ff;
   if (ascii == ENTER)  return(CONTROL_PANEL);
   if (ascii == ESCAPE) return(ABORT);
   return(AUTOLOAD);
}  /* end DisplayStartUpScreen */


int RunCountdownTimer(int line)
{
   unsigned lastSecond;
   struct dostime_t time;
	int secsRemaining = TIMEOUT_SECONDS, timeLine, keyPressed = FALSE;

	charBufferLength = sprintf(charBuffer, "ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป");
   _settextposition(line++, 41-charBufferLength/2);
   _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "บ    Press Escape now to abort loading AccessDOS.                      บ");
   _settextposition(line++, 41-charBufferLength/2);
   _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "บ    Press Enter now to go to the main menu.                           บ");
   _settextposition(line++, 41-charBufferLength/2);
   _outtext(charBuffer);
   timeLine = line;
	charBufferLength = sprintf(charBuffer, "บ    Press any other key (or wait 20 seconds) to install AccessDOS     บ");
	_settextposition(line++, 41-charBufferLength/2);
   _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "บ          with the settings saved in the ADOS.CFG file.               บ");
   _settextposition(line++, 41-charBufferLength/2);
   _outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ");
   _settextposition(line++, 41-charBufferLength/2);
   _outtext(charBuffer);

	ClearKeyboardBuffer();

      /* wait for next second */
   _dos_gettime(&time);
   lastSecond = time.second;
   do
   {
      _dos_gettime(&time);
   } while (lastSecond == time.second);
   lastSecond = time.second;

	while (secsRemaining)
	{
		if (_bios_keybrd(keybrdReady))
		{
			keyPressed = TRUE;
			break;
		}
		charBufferLength = sprintf(charBuffer, "บ    Press any other key (or wait %2d seconds) to install AccessDOS     บ",
											secsRemaining);
		_settextposition(timeLine, 41-charBufferLength/2);
      _outtext(charBuffer);

      while (lastSecond == time.second) _dos_gettime(&time);
      lastSecond = time.second;
		secsRemaining--;
	}
   return keyPressed;
}  /* end RunCountdownTimer() */
