#include <graph.h>
#include <stdio.h>
#include <bios.h>
#include <string.h>
#include <ctype.h>

#include "defines.h"
#include "globals.h"
#include "menudata.h"
#include "messages.h"
#include "frontend.h"
#include "access.h"
#include "dialogs.h"

#define	ROUTINE_INSTALL	1
#define	ROUTINE_CANCEL		2


void DisplayNotImplementedMessage(char *messageText)
{
	int helpMessageFlag = FALSE;

	_setbkcolor(dialogBoxBG);
	_settextcolor(dialogBoxFG);
	_settextwindow(3, 1, 24, 80);
	_clearscreen(_GWINDOW);
	charBufferLength = sprintf(charBuffer, "%s", messageText);
	_settextposition(12, 40-charBufferLength/2);
	_outtext(charBuffer);
	charBufferLength = sprintf(charBuffer, "(function not implemented yet)");
	_settextposition(13, 40-charBufferLength/2);
	_outtext(charBuffer);
	OutputHintBar("", JUSTIFY_LEFT, helpMessageFlag);
	charBufferLength = sprintf(charBuffer, "Press any key to continue.");
	_settextposition(1, 40-charBufferLength/2);
	_outtext(charBuffer);

	WaitForAnyKey();
}	/* DisplayNotImplementedMessage() */

void AboutAccess(void)
{
	int top=5, bottom=22, left=3, right=78;
	int height, width, line=2;
	int helpMessageFlag = FALSE, titleFlag = TRUE, autoLoadFlag = FALSE;

	height = bottom-top+1;
	width = right-left+1;

	DrawDialogBox(top, left, bottom, right, dialogBoxBorderFG, dialogBoxBorderBG, dialogBoxFG, dialogBoxBG, "About AccessDOS");
	line = DisplayStartupScreenText(line, width/2+1, titleFlag, autoLoadFlag);
	OutputHintBar("Press any key to continue.", JUSTIFY_CENTER, helpMessageFlag);
	ClearKeyboardBuffer();
	WaitForAnyKey();
}	/* end AboutAccess() */


void QuitAccess(int callingRoutine)
{
   unsigned key;
   int scanCode, ascii, notDone = TRUE, helpMessageFlag = FALSE;
   int top=11, bottom=15, left=15, right=65, width;
	char *title;

	width = right-left+1;

	if (callingRoutine == ROUTINE_INSTALL) title = "Install/Run AccessDOS";
	else title = "Cancel/Quit AccessDOS";

	DrawDialogBox(top, left, bottom, right, dialogBoxBorderFG, dialogBoxBorderBG, dialogBoxFG, dialogBoxBG, title);

	if (callingRoutine == ROUTINE_INSTALL)
		charBufferLength = sprintf(charBuffer, "Are you sure you want to Install/Run AccessDOS?");
	else
		charBufferLength = sprintf(charBuffer, "Are you sure you want to Cancel/Quit AccessDOS?");
	_settextposition(2, width/2-charBufferLength/2+1);
	_outtext(charBuffer);

	charBufferLength = sprintf(charBuffer, "Press Enter to proceed");
	_settextposition(4, width/2-charBufferLength/2+1);
	_outtext(charBuffer);

	charBufferLength = sprintf(charBuffer, "Press Escape to return to menu");
	_settextposition(5, width/2-charBufferLength/2+1);
	_outtext(charBuffer);

	if (callingRoutine == ROUTINE_INSTALL)
		OutputHintBar("This will install AccessDOS or, if already running, change the settings.", JUSTIFY_LEFT, helpMessageFlag);
	else
		OutputHintBar("This will quit AccessDOS or, if already running, cancel any changes.", JUSTIFY_LEFT, helpMessageFlag);

   while (notDone)
   {
         /* Get a key and process it */
	   key = _bios_keybrd(keybrdRead);
	   ascii = key & 0x00ff;
	   scanCode = ascii ? 0 : key >> 8;

	   if (ascii)
	   {
         notDone = FALSE;
			switch (ascii)
			{
				case ENTER		:	switch (callingRoutine)
            						{
               						case ROUTINE_INSTALL : installFlag = TRUE;	break;
               						case ROUTINE_CANCEL	: installFlag = FALSE;	break;
            						}
										quitFlag = TRUE;
										break;

				case ESCAPE		:	installFlag = FALSE;
										quitFlag = FALSE;
										break;

				case 'C'			:
				case 'c'			:
				case 'Q'			:
				case 'q'			:	if (callingRoutine == ROUTINE_CANCEL)
										{
											installFlag = FALSE;
											quitFlag = TRUE;
										}
										break;

				case 'R'			:
				case 'r'			:
				case 'I'			:
				case 'i'			:	if (callingRoutine == ROUTINE_INSTALL)
										{
											installFlag = TRUE;
											quitFlag = TRUE;
										}
										break;

				default			:	putchar('\07');
										ClearKeyboardBuffer();
										notDone = TRUE;
										break;
			}
      }
	   else
		{
			putchar('\07');
			ClearKeyboardBuffer();
		}
   }
}	/* end QuitAccess() */


void InstallAccess(void)
{
	QuitAccess(ROUTINE_INSTALL);
}	/* end InstallAccess() */


void CancelAccess(void)
{
	QuitAccess(ROUTINE_CANCEL);
}	/* end CancelAccess() */


void StickyKeys(void)
{
   ManageDialogBox(stickyKeysDialogList, "StickyKeys", StickyKeysHelp);
}	/* end StickyKeys() */


void MouseKeys(void)
{
	ManageDialogBox(mouseKeysDialogList, "MouseKeys", MouseKeysHelp);
   if (eqMouseId == FALSE && mMouseKeysOn)
   {
      int height = 10, width = 58;
      DisplayAlertBox(NoMouseFoundAlertText, height, width);
      mMouseKeysOn = FALSE;
   }
}	/* end MouseKeys() */


void ToggleKeys(void)
{
	ManageDialogBox(toggleKeysDialogList, "ToggleKeys", ToggleKeysHelp);
}	/* end ToggleKeys() */


void SerialKeys(void)
{
   unsigned int numberOfSerialPorts = (_bios_equiplist() & 0x0e00) >> 9;

	ManageDialogBox(serialKeysDialogList, "SerialKeys", SerialKeysHelp);
   if (skSerialKeysOn)
   {
      if (skSerialKeysLoaded == FALSE && fAccessAlreadyLoaded)
      {
         int height = 11, width = 58;
         DisplayAlertBox(SerialKeysNotLoadedAlertText, height, width);
         skSerialKeysOn = FALSE;
/*
		} else if (numberOfSerialPorts == 0)
		{
			int height = 9, width = 58;
			DisplayAlertBox(NoSerialPortAvailableAlertText, height, width);
			skSerialKeysOn = FALSE;
      } else if (numberOfSerialPorts == 1 && eqCommBase == COM1)
      {
         int height = 13, width = 68;
         DisplayAlertBox(OneSerialPortWithMouseAlertText, height, width);
         skSerialKeysOn = FALSE;
      } else if (numberOfSerialPorts == 1 && skPortAddress == COM2)
      {
         int height = 10, width = 58;
         DisplayAlertBox(NoCommTwoAlertText, height, width);
         skPortAddress = COM1;
      } else if (!fAccessAlreadyLoaded && skPortAddress == eqCommBase)
      {
         int height = 13, width = 58;
         DisplayAlertBox(SerKeysMouseConflictAlertText, height, width);
         switch (eqCommBase)
         {
            case COM1  :  skPortAddress = COM2; break;
            case COM2  :  skPortAddress = COM1; break;
         }
*/
      } else if (skSerialKeysLoaded && skPortAddress != oldPortAddress)
      {
         int height = 11, width = 58;
         DisplayAlertBox(CannotChangeCommPortAlertText, height, width);
         skPortAddress = oldPortAddress;
      }

		if (skPortAddress == serialPort1 || skPortAddress == serialPort3) skInterrupt = 4;
		if (skPortAddress == serialPort2 || skPortAddress == serialPort4) skInterrupt = 3;
   }
}	/* end SerialKeys() */


void KeyboardPackage(void)
{
	int i=0, repeatRateSelectableState;


		/* set the selectable flag for the 'delay until repeat' option */
		/* if the repeat rate is 'Off' then do not allow adjustment of the 'delay until repeat' */
		/* repeat rate is 'Off' when it has the value 32760 */
   if (fRepeatTicks == 32760) repeatRateSelectableState = SELECTABLE_NO;
   else repeatRateSelectableState = SELECTABLE_YES;
   do
   {
      if (keyboardDialogList[i].optionList == repeatDelayOption)
      {
         keyboardDialogList[i].selectable = repeatRateSelectableState;
         break;
      }
   } while (keyboardDialogList[i++].lastRecord == FALSE);

		/* manage the Keyboard Response Group dialog box */
	ManageDialogBox(keyboardDialogList, "Keyboard Response", KeyboardPackageHelp);

		/* validate the responses */
		/* if repeat rate is 'Off' set delay ticks to 32760 */
		/* if repeat rate is not 'Off' and delay ticks has the value 32760, give it the default time of 1 second (18 ticks ) */
   if (fRepeatTicks == 32760)	fDelayTicks = 32760;
   else if (fDelayTicks == 32760) fDelayTicks = 18;

		/* can't have both bounceKeys and slowKeys on at the same time */
   fBounceOn = (char) (fBounceTicks != 32760);
   if (fFilterKeysOn)
   {
      if (fWaitTicks && fBounceOn)
      {
         int height = 4, width = 38;
         DisplayAlertBox(NoSlowWithBounceAlertText, height, width);
         fFilterKeysOn = FALSE;
      }
   }
}	/* end KeyboardPackage() */


void ShowSounds(void)
{
	ManageDialogBox(showSoundsDialogList, "ShowSounds", ShowSoundsHelp);
   if (fHearingOn && fVideoFlash)
   {
      int height = 4, width = 48;
      DisplayAlertBox(NoNoteWithFlashAlertText, height, width);
      fHearingOn = fVideoFlash = FALSE;
   }
}	/* end ShowSounds() */


void TimeOut(void)
{
	ManageDialogBox(timeOutDialogList, "TimeOut", TimeOutHelp);
}	/* end TimeOut() */


void Miscellaneous(void)
{
	ManageDialogBox(miscellaneousDialogList, "Miscellaneous Adjustments", MiscellaneousHelp);
}	/* end Miscellaneous() */




void SaveParametersHelp(void)
{
   ManageHelpDialogBox(helpTextSaveParameters, "Save Settings Help");
}	/* end SaveParametersHelp() */


void InstallAccessHelp(void)
{
   ManageHelpDialogBox(helpTextInstallAccess, "Install Access Help");
}	/* end InstallAccessHelp() */


void CancelAccessHelp(void)
{
   ManageHelpDialogBox(helpTextCancelAccess, "Cancel Access Help");
}	/* end CancelAccessHelp() */


void MenuHelp(void)
{
   ManageHelpDialogBox(helpTextMenu, "Menu Help");
}	/* end MenuHelp() */


void GeneralInfo(void)
{
   ManageHelpDialogBox(helpTextGeneral, "General Information");
}	/* end GeneralInfo() */


void StickyKeysHelp(void)
{
   ManageHelpDialogBox(helpTextSticky, "StickyKeys Help");
}	/* end StickyKeysHelp() */


void MouseKeysHelp(void)
{
   ManageHelpDialogBox(helpTextMouse, "MouseKeys Help");
}	/* end MouseKeysHelp() */


void ToggleKeysHelp(void)
{
   ManageHelpDialogBox(helpTextToggle, "ToggleKeys Help");
}	/* end ToggleKeysHelp() */


void SerialKeysHelp(void)
{
   ManageHelpDialogBox(helpTextSerial, "SerialKeys Help");
}	/* end SerialKeysHelp() */


void KeyboardPackageHelp(void)
{
   ManageHelpDialogBox(helpTextKeyboard, "Keyboard Response Help");
}	/* end KeyboardPackageHelp() */


void ShowSoundsHelp(void)
{
   ManageHelpDialogBox(helpTextShowSounds, "ShowSounds Help");
}	/* end ShowSoundsHelp() */


void TimeOutHelp(void)
{
   ManageHelpDialogBox(helpTextTimeOut, "TimeOut Help");
}	/* end TimeOutHelp() */


void MiscellaneousHelp(void)
{
   ManageHelpDialogBox(helpTextMisc, "Miscellaneous Help");
}	/* end MiscellaneousHelp() */


void DisplayTextInHelpDialogBox(char *dialogText, int height, int width)
{
   int i, line=1, lineWidth=0;
	int bold, quit = FALSE;
   char currentChar;

	currentChar = dialogText[i=0];
   charBuffer[charBufferLength=0] = '\0';
	bold = FALSE;
   _setbkcolor(dialogBoxBG);
	_settextcolor(dialogBoxFG);
	_settextposition(line, 2);

	while (quit == FALSE)
	{
         /* If the current character is a space, gather all spaces and print */
      if (currentChar == ' ')
      {
         while (currentChar == ' ')
         {
            charBuffer[charBufferLength] = currentChar;
            charBuffer[++charBufferLength] = '\0';
            currentChar = dialogText[++i];
         }
         lineWidth += charBufferLength;
         _outtext(charBuffer);
         charBuffer[charBufferLength=0] = '\0';
      }

         /* get the next word */
		while (currentChar != ' ' && currentChar != '\n' && currentChar != '\0' && currentChar != '^')
		{
         charBuffer[charBufferLength] = currentChar;
         charBuffer[++charBufferLength] = '\0';
         currentChar = dialogText[++i];
		}

         /* if the word will fit on the line, print it; else put it on the next line */
         /* don't forget to adjust the lineWidth as necessary */
      if (lineWidth + charBufferLength > width)
      {
         _settextposition(++line, 2);
         lineWidth = 0;
         if (line > height)
         {
            putchar('\07');
            break;
         }
      }
      if (charBufferLength)    /* don't print if there is nothing in the buffer */
      {
         _outtext(charBuffer);
         lineWidth += charBufferLength;
         charBuffer[charBufferLength=0] = '\0';
      }

         /* if currentChar is a newline then start next line */
      if (currentChar == '\n')
      {
         _settextposition(++line, 2);
         lineWidth = 0;
         currentChar = dialogText[++i];
         if (line > height)
         {
            putchar('\07');
            break;
         }
      } else

			/* if the current character is a '^' toggle the bold attribute */
		if (currentChar == '^')
		{
			if (bold)
			{
				bold = FALSE;
				_settextcolor(dialogBoxFG);
			}
			else
			{
				bold = TRUE;
				_settextcolor(dialogBoxArrowFG);
			}
         currentChar = dialogText[++i];
		} else

            /* if the currentChar is a NULL then this is the end of the dialogText and we can quit */
		if (currentChar == '\0') quit = TRUE;
	}
}  /* end DisplayTextInHelpDialogBox() */


void ManageHelpDialogBox(HELP_SCREEN_RECORD *helpScreen, char *title)
{
	unsigned key;
	int ascii, scanCode;
	int top=5, bottom=22, left=3, right=78;
	int width, height, i=0, quit = FALSE, helpMessageFlag = FALSE, legalKey;

	width = right-left+1;
	height = bottom-top+1;

   DrawDialogBox(top, left, bottom, right, dialogBoxBorderFG, dialogBoxBorderBG, dialogBoxFG, dialogBoxBG, title);
	OutputHintBar("Press ESC to exit help   PgDn or Enter = Next Screen   PgUp = Previous Screen", JUSTIFY_LEFT, helpMessageFlag);

	do
	{
		_settextwindow(top, left, bottom, right);
      _setbkcolor(dialogBoxBG);
   	_settextcolor(dialogBoxFG);
		_clearscreen(_GWINDOW);
	   DisplayTextInHelpDialogBox(helpScreen[i].text, height, width-2);
		
      do
      {
		   key = _bios_keybrd(keybrdRead);
		   ascii = key & 0x00ff;
		   scanCode = ascii ? 0 : key >> 8;
         legalKey = TRUE;

		   if (ascii)
		   {
			   switch (ascii)
			   {
				   case ENTER	:	if (helpScreen[i].lastRecord != TRUE) i++;
									   else legalKey = FALSE;
									   break;

				   case ESCAPE	:	quit = TRUE;
									   break;

				   default		:	legalKey = FALSE;
									   break;
			   }
		   }
		   else switch (scanCode)
		   {
			   case PAGE_DOWN	:	if (helpScreen[i].lastRecord != TRUE) i++;
									   else legalKey = FALSE;
									   break;

			   case PAGE_UP	:	if (i) i--;
									   else legalKey = FALSE;
									   break;

			   default			:	legalKey = FALSE;
									   break;
		   }

		   if (legalKey == FALSE)
			{
				putchar('\07');
				ClearKeyboardBuffer();
			}
      } while (legalKey == FALSE);
	} while (quit == FALSE);
}  /* end ManageHelpDialogBox */


int CalculateDialogBoxSize(DIALOG_LIST_RECORD *currentDialogList, int *top, int *left, int *bottom, int *right,
									int extraHeight, int extraWidth)
{
	int titleWidth, maxTitleWidth = 0, settingsWidth, maxSettingsWidth = 0;
   int i, height, width, numberOfOptionLists = 0;

	while (currentDialogList[numberOfOptionLists++].lastRecord == FALSE);

	for (i=0; i<numberOfOptionLists; i++)
	{
		if (currentDialogList[i].selectable != SELECTABLE_TITLE)
		{
			int j, numberOfSettings;
      	OPTION_LIST_RECORD *currentOptionList;

			titleWidth = strlen(currentDialogList[i].leftText) + strlen(currentDialogList[i].midText) +
                   	strlen(currentDialogList[i].rightText);
			if (titleWidth > maxTitleWidth) maxTitleWidth = titleWidth;

			currentOptionList = currentDialogList[i].optionList;
   		numberOfSettings = 0;
			while (currentOptionList[numberOfSettings++].lastRecord == FALSE);

			settingsWidth = 0;
			for (j=0; j<numberOfSettings; j++)
			{
				settingsWidth += strlen(currentOptionList[j].leftText) + strlen(currentOptionList[j].midText) +
                          	strlen(currentOptionList[j].rightText);
			}
      	if (settingsWidth > maxSettingsWidth) maxSettingsWidth = settingsWidth;
		}
	}

   if ((height = numberOfOptionLists+extraHeight) > 20) height = 20;
   if ((width = maxTitleWidth+maxSettingsWidth+extraWidth) > 78) width = 78;
	if (width > 59) width = ((width+1) >> 1) << 1;      /* make width an even number >= original width */

	*top    = 14-height/2;
	*bottom = *top+height-1;
	*left   = 41-width/2;
	*right  = *left+width-1;

	return maxTitleWidth+5;
}	/* end CalculateDialogBoxSize() */


void DrawDialogBox(int top, int left, int bottom, int right, short borderFG, long borderBG, short insideFG, long insideBG, char *title)
{
   int i, height, width;

	ClearDesktop();

	height = bottom-top+1;
	width = right-left+1;

   _settextcolor(borderFG);
   _setbkcolor(borderBG);
   memset(charBuffer, '\0', LINE_LENGTH+1);
	memset(charBuffer, 'อ', width+2);
	charBuffer[0] = 'ษ';
	charBuffer[width+1] = 'ป';
	_settextposition(top-1, left-1);
	_outtext(charBuffer);

	memset(charBuffer, ' ', width+2);
	charBuffer[0] = 'บ';
	charBuffer[width+1] = 'บ';
	for (i=top; i<= bottom; i++)
	{
		_settextposition(i, left-1);
		_outtext(charBuffer);
	}

	memset(charBuffer, 'อ', width+2);
	charBuffer[0] = 'ศ';
	charBuffer[width+1] = 'ผ';
	_settextposition(bottom+1, left-1);
	_outtext(charBuffer);

	if (*title)
	{
		charBufferLength = sprintf(charBuffer, "ต %s ฦ", title);
		_settextposition(top-1, left+(width/2-charBufferLength/2));
		_outtext(charBuffer);
	}

	_settextwindow(top, left, bottom, right);
   _settextcolor(insideFG);
   _setbkcolor(insideBG);
   _clearscreen(_GWINDOW);
}	/* end DrawDialogBox() */


int PrintOptionLists(DIALOG_LIST_RECORD *currentDialogList, int currentOption, int numberOfOptionLists,
							int top, int left, int bottom, int right, int alignmentColumn)
{
	int i, j, numberOfSettings, line = 2, helpMessageFlag = FALSE;
   OPTION_LIST_RECORD *currentOptionList;

	for (i=0; i<numberOfOptionLists; i++)
	{
      _settextwindow(top, left, bottom, right);
      if (currentDialogList[i].selectable != SELECTABLE_NEVER)
      {
			_settextposition(line, 2);
																				 
				/* fill charBuffer with the title of the option */
			charBufferLength = sprintf(charBuffer, "%s%s%s", currentDialogList[i].leftText,
												currentDialogList[i].midText, currentDialogList[i].rightText);

				/* if this is not a SELECTABLE_TITLE item, right-justify title into tempBuffer */
			if (currentDialogList[i].selectable != SELECTABLE_TITLE)
			{
				tempBufferLength = sprintf(tempBuffer, "%*.*s", alignmentColumn-5, alignmentColumn-5, charBuffer);

					/* Add arrow or spaces to the front and place into charBuffer */
				if (i == currentOption)
				{
					charBufferLength = sprintf(charBuffer, "อออ%s", tempBuffer);
				}
				else
				{
					charBufferLength = sprintf(charBuffer, "    %s", tempBuffer);
				}
			}
			switch (currentDialogList[i].selectable)
			{
				case SELECTABLE_YES		:	if (i == currentOption)
													{
         											_settextcolor(dialogBoxArrowFG);
         											_setbkcolor(dialogBoxArrowBG);
													}
													else
													{
            										_settextcolor(dialogBoxFG);
            										_setbkcolor(dialogBoxBG);
													}
													break;

				case SELECTABLE_NO		:	_settextcolor(dialogBoxDeadFG);
													_setbkcolor(dialogBoxDeadBG);
													break;

				case SELECTABLE_TITLE	:	_settextcolor(dialogBoxFG);
													_setbkcolor(dialogBoxBG);
													break;
			}
			_outtext(charBuffer);

			currentOptionList = currentDialogList[i].optionList;
   		numberOfSettings = 0;
			while (currentOptionList[numberOfSettings++].lastRecord == FALSE);

				/* print the settings for this option */
			for (j=0; j<numberOfSettings; j++)
			{
				if (currentOptionList[j].selectable == SELECTABLE_NO ||
					 currentDialogList[i].selectable == SELECTABLE_NO)
				{
					_settextcolor(dialogBoxDeadFG);
					_setbkcolor(dialogBoxDeadBG);
				}
				else if (currentOptionList[j].hot)
				{
					_settextcolor(dialogBoxHotFG);
					_setbkcolor(dialogBoxHotBG);
				}
            else
            {
				   _settextcolor(dialogBoxFG);
				   _setbkcolor(dialogBoxBG);
            }
				charBufferLength = sprintf(charBuffer, "%s%s%s", currentOptionList[j].leftText,
													currentOptionList[j].midText, currentOptionList[j].rightText);
				_outtext(charBuffer);
			}

			if (i == currentOption) charBufferLength = sprintf(charBuffer, "อออ");
			else charBufferLength = sprintf(charBuffer, "    ");
			_settextcolor(dialogBoxArrowFG);
			_setbkcolor(dialogBoxArrowBG);
			_settextposition(line, right-left-3);
			_outtext(charBuffer);

					/* print the hint bar */
			if (i == currentOption) OutputHintBar(currentDialogList[i].hintText, JUSTIFY_LEFT, helpMessageFlag);
      }
		line++;
	}
   return line;
}  /* end PrintOptionLists() */


void PrintOkCancelButtons(int line, int width, int okHot, int cancelHot)
{
	int helpMessageFlag = FALSE;
	_settextcolor(dialogBoxArrowFG);
	_setbkcolor(dialogBoxArrowBG);

		/* print the top line of the OK button */
	if (!cancelHot) charBufferLength = sprintf(charBuffer, "ษออออออออออป");
	else charBufferLength = sprintf(charBuffer, "            ");
	_settextposition(line++, width/2-charBufferLength/2+1);
	_outtext(charBuffer);

		/* print the middle line of the OK button */
	if (okHot) charBufferLength = sprintf(charBuffer, "อออ");
	else	charBufferLength = sprintf(charBuffer, "    ");
	_settextposition(line, 2);
	_outtext(charBuffer);

	if (!cancelHot) charBufferLength = sprintf(charBuffer, "บ    OK    บ");
	else charBufferLength = sprintf(charBuffer, "     OK     ");
	_settextposition(line, width/2-charBufferLength/2+1);
	_outtext(charBuffer);

	if (okHot) charBufferLength = sprintf(charBuffer, "อออ");
	else	charBufferLength = sprintf(charBuffer, "    ");
	_settextposition(line++, width-4);
	_outtext(charBuffer);

		/* print the bottom line of the OK button / top line of Cancel button */
	if (cancelHot) charBufferLength = sprintf(charBuffer, "ษออออออออออป");
	else charBufferLength = sprintf(charBuffer, "ศออออออออออผ");
	_settextposition(line++, width/2-charBufferLength/2+1);
	_outtext(charBuffer);

		/* print the middle line of the cancel button */
	if (cancelHot) charBufferLength = sprintf(charBuffer, "อออ");
	else charBufferLength = sprintf(charBuffer, "    ");
	_settextposition(line, 2);
	_outtext(charBuffer);

	if (cancelHot) charBufferLength = sprintf(charBuffer, "บ  Cancel  บ");
	else charBufferLength = sprintf(charBuffer, "   Cancel   ");
	_settextposition(line, width/2-charBufferLength/2+1);
	_outtext(charBuffer);

	if (cancelHot) charBufferLength = sprintf(charBuffer, "อออ");
	else charBufferLength = sprintf(charBuffer, "    ");
	_settextposition(line++, width-4);
	_outtext(charBuffer);

		/* print the bottom line of the Cancel button */
	if (cancelHot)	charBufferLength = sprintf(charBuffer, "ศออออออออออผ");
	else charBufferLength = sprintf(charBuffer, "            ");
	_settextposition(line++, width/2-charBufferLength/2+1);
	_outtext(charBuffer);

		/* print the hint bar if needed */
	if (cancelHot)
	{
		OutputHintBar("Press Enter now or Escape anytime to ignore these settings.", JUSTIFY_LEFT, helpMessageFlag);
	}
   else if (okHot)
	{
		OutputHintBar("Press Enter to accept these settings.", JUSTIFY_LEFT, helpMessageFlag);
	}
}	/* end PrintOkCancelButtons() */


int ProcessKeystroke(DIALOG_LIST_RECORD *currentDialogList, int *currentOptionPtr, int numberOfOptionLists, void (*helpProcPtr)(void))
{
	unsigned key;
	int ascii, scanCode;
   int returnValue = PK_NOT_DONE;
   int currentSetting = 0, numberOfSettings, currentOption = *currentOptionPtr;
	OPTION_LIST_RECORD *currentOptionList;

		/* get the current setting information */
	currentOptionList = currentDialogList[currentOption].optionList;
   numberOfSettings = 0;
	do
	{
		if (currentOptionList[numberOfSettings].hot) currentSetting = numberOfSettings;
	}
	while (currentOptionList[numberOfSettings++].lastRecord == FALSE);

	key = _bios_keybrd(keybrdRead);
	ascii = key & 0x00ff;
	scanCode = ascii ? 0 : key >> 8;

	if (ascii)
	{
      int legalKey = FALSE;

      switch (ascii)
      {
         case ENTER  :  returnValue = PK_DONE;
                        legalKey = TRUE;

									/* if currentOption > numberOfOptionLists, Cancel is highlighted */
								if (currentOption <= numberOfOptionLists)
	                        GetOptionListHotFlags(currentDialogList, numberOfOptionLists);
                        break;

         case ESCAPE :  returnValue = PK_DONE;
                        legalKey = TRUE;
                        break;

         default     :  {
                           int i;

				               ascii = toupper(ascii);

					               /* Is Key a menu line hot key? */
				               for (i=0; i<numberOfSettings; i++)
				               {
					               if (ascii == currentOptionList[i].hotKey)
					               {
						               currentOptionList[currentSetting].hot = FALSE;
						               currentOptionList[currentSetting=i].hot = TRUE;
                                 legalKey = TRUE;
						               break;
					               }
				               }
                        }
		}
      if (legalKey == FALSE)
		{
			putchar('\07');
			ClearKeyboardBuffer();
		}
	}
	else switch (scanCode)
	{
		case UP			:	do
                        {
                           if (currentOption) currentOption--;
   								else currentOption = numberOfOptionLists-1;
                        } while (currentOption < numberOfOptionLists && currentDialogList[currentOption].selectable != SELECTABLE_YES);
								break;

		case DOWN		:	do
                        {
                           if (++currentOption == numberOfOptionLists) currentOption = 0;
                        } while (currentOption < numberOfOptionLists && currentDialogList[currentOption].selectable != SELECTABLE_YES);
								break;

		case LEFT		:	if (currentOption < numberOfOptionLists)
                        {
                           currentOptionList[currentSetting].hot = FALSE;
                           do
                           {
									   if (currentSetting) currentSetting--;
									   else currentSetting = numberOfSettings-1;
                           } while (currentOptionList[currentSetting].selectable != SELECTABLE_YES);
									currentOptionList[currentSetting].hot = TRUE;
                        }
                        else putchar('\07');
								break;

		case RIGHT		:	if (currentOption < numberOfOptionLists)
                        {
                           currentOptionList[currentSetting].hot = FALSE;
                           do
                           {
   									if (++currentSetting == numberOfSettings) currentSetting = 0;
                           } while (currentOptionList[currentSetting].selectable != SELECTABLE_YES);
									currentOptionList[currentSetting].hot = TRUE;
                        }
                        else putchar('\07');
								break;

      case F1_KEY    :  (helpProcPtr)();
                        returnValue = PK_REDRAW_SCREEN;
                        break;

      default        :  putchar('\07');
								ClearKeyboardBuffer();
								break;
	}
   *currentOptionPtr = currentOption;
   return returnValue;
}  /* end ProcessKeystroke() */


void ManageDialogBox(DIALOG_LIST_RECORD *currentDialogList, char *title, void (*helpProcPtr)())
{
	int numberOfOptionLists, currentOption=0;
   int line, top, bottom, left, right, width = 0, titleWidth = 0;
	int returnValue = PK_REDRAW_SCREEN;

   numberOfOptionLists = 0;
	while (currentDialogList[numberOfOptionLists++].lastRecord == FALSE);

   SetOptionListHotFlags(currentDialogList, numberOfOptionLists);
	do
	{
      if (returnValue == PK_REDRAW_SCREEN)
      {
	      int extraHeight = 7, extraWidth = 11;
	      titleWidth = CalculateDialogBoxSize(currentDialogList, &top, &left, &bottom, &right, extraHeight, extraWidth);
	      DrawDialogBox(top, left, bottom, right, dialogBoxBorderFG, dialogBoxBorderBG, dialogBoxFG, dialogBoxBG, title);
			width = right-left+1;
         returnValue = PK_NOT_DONE;
      }
			/* Print the option lists in the dialog box */
      line = PrintOptionLists(currentDialogList, currentOption, numberOfOptionLists, top, left, bottom, right, titleWidth);
		_settextwindow(top, left, bottom, right);
/*
		PrintOkCancelButtons(line, width, currentOption == numberOfOptionLists, currentOption > numberOfOptionLists);
*/

		_settextcolor(dialogBoxFG);
		_setbkcolor(dialogBoxBG);
		charBufferLength = sprintf(charBuffer, "Press Enter to accept");
		_settextposition(bottom-top-2, width/2-charBufferLength/2+1);
		_outtext(charBuffer);
		charBufferLength = sprintf(charBuffer, "Press Esc to cancel");
		_settextposition(bottom-top-1, width/2-charBufferLength/2+1);
		_outtext(charBuffer);
		charBufferLength = sprintf(charBuffer, "Press F1 for more information");
		_settextwindow(top, left, bottom, right);
		_settextposition(bottom-top+1, width/2-charBufferLength/2+1);
		_outtext(charBuffer);

		returnValue = ProcessKeystroke(currentDialogList, &currentOption, numberOfOptionLists, helpProcPtr);
	} while (returnValue != PK_DONE);
   _settextwindow(1, 1, 25, 80);
}	/* end ManageDialogBox() */


void DisplayAlertBox(char *alertText, int alertBoxHeight, int alertBoxWidth)
{
   int top, bottom, left, right;
   int line, count, quit = FALSE, helpMessageFlag = FALSE;
   char *currentLine, currentChar;

	top    = 13-alertBoxHeight/2;
	bottom = top+alertBoxHeight-1;
	left   = 41-alertBoxWidth/2;
	right  = left+alertBoxWidth-1;

   DrawDialogBox(top, left-1, bottom, right+1, alertBoxFG, alertBoxBG, alertBoxFG, alertBoxBG, "ALERT");

	currentLine = alertText;
	currentChar = currentLine[count=0];

   line = 1;
	while (quit == FALSE)
	{
		while (currentChar != '\n' && currentChar != '\0' && count < alertBoxWidth)
			currentChar = currentLine[++count];
		if (count == alertBoxWidth)
		{
			while (currentChar != ' ' && count) currentChar = currentLine[--count];
			if (count == 0) count = alertBoxWidth;
		}

		strncpy(charBuffer, currentLine, count+1);
		charBuffer[count] = '\0';
		_settextposition(line++, 2);
		_outtext(charBuffer);

		if (currentLine[count] == 0) quit = TRUE;
      else if (line == alertBoxHeight-1)
      {
         charBufferLength = sprintf(charBuffer, "Press any key to continue");
         _settextposition(alertBoxHeight, (alertBoxWidth-charBufferLength)/2+2);
         _outtext(charBuffer);
         putchar('\07');
         WaitForAnyKey();
         _clearscreen(_GWINDOW);
         line = 2;
      }
      else
		{
			currentLine += count+1;
			count = 0;
			currentChar = currentLine[count];
		}
	}

   charBufferLength = sprintf(charBuffer, "Press any key to continue");
   _settextposition(alertBoxHeight, (alertBoxWidth-charBufferLength)/2+2);
   _outtext(charBuffer);
	ClearKeyboardBuffer();
   WaitForAnyKey();
}  /* end DisplayAlertBox() */


void SetOptionListHotFlags(DIALOG_LIST_RECORD *currentDialogList, int numberOfOptionLists)
{
	int i, j;
   OPTION_LIST_RECORD *currentOptionList;

      /* Initialize the data structure of the current feature from the global variables */
   for (i=0; i<numberOfOptionLists; i++)
   {
      currentOptionList = currentDialogList[i].optionList;
      j=0;
      do
      {
				/* if the option is not selectable, it can not be hot */
			if (currentOptionList[j].selectable != SELECTABLE_YES)
				currentOptionList[j].hot = FALSE;
			else switch (currentDialogList[i].paramLength)
         {
            case 1 : currentOptionList[j].hot = ((*(unsigned char far *) (currentDialogList[i].paramPtr) ==
																  (unsigned char) currentOptionList[j].paramValue) ? TRUE : FALSE); break;
            case 2 : currentOptionList[j].hot = ((*(unsigned int  far *) (currentDialogList[i].paramPtr) ==
																  (unsigned int)  currentOptionList[j].paramValue) ? TRUE : FALSE); break;
         }
      } while (currentOptionList[j++].lastRecord == FALSE);
   }
}	/* end SetOptionListHotFlags() */


void GetOptionListHotFlags(DIALOG_LIST_RECORD *currentDialogList, int numberOfOptionLists)
{
	int i, j;
   OPTION_LIST_RECORD *currentOptionList;

      /* Update global variables from data structure of current feature */
   for (i=0; i<numberOfOptionLists; i++)
   {
      currentOptionList = currentDialogList[i].optionList;
      j=0;
      do
      {
         switch (currentDialogList[i].paramLength)
         {
            case 1 : if (currentOptionList[j].hot) (*(unsigned char far *) (currentDialogList[i].paramPtr) =
																	 (unsigned char) currentOptionList[j].paramValue); break;
            case 2 : if (currentOptionList[j].hot) (*(unsigned int  far *) (currentDialogList[i].paramPtr) =
																	 (unsigned int)  currentOptionList[j].paramValue); break;
         }
      } while (currentOptionList[j++].lastRecord == FALSE);
   }
}	/* end GetOptionListHotFlags() */
