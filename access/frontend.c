#include <bios.h>
#include <graph.h>
#include <io.h>
#include <stdio.h>
#include <stdlib.h>
#include <dos.h>
#include <string.h>
#include <memory.h>
#include <ctype.h>

#include "defines.h"
#include "globals.h"
#include "access.h"
#include "frontend.h"
#include "menudata.h"
#include "messages.h"
#include "dialogs.h"


void OutputTitleBar(void)
{
   _settextwindow(1, 1, 1, 80);
   _settextcolor(titleBarFG);
   _setbkcolor(titleBarBG);
   _clearscreen(_GWINDOW);
   charBufferLength = sprintf(charBuffer, "AccessDOS 1.1");
   _settextposition(1, 40-charBufferLength/2);
   _outtext(charBuffer);
}	/* end OutputTitleBar() */


void ClearDesktop(void)
{
	int i;

   _settextwindow(1, 1, 25, 80);
   _setbkcolor(desktopBG);
   _settextcolor(desktopFG);
   for (i=3; i<25; i++)
   {
      _settextposition(i, 1);
      _outtext(backgroundString);
   }
}	/* end ClearDesktop() */


void OutputHintBar(char *hintText, int justifyFlag, int helpMessageFlag)
{
   _settextwindow(25, 1, 25, 80);
   _settextcolor(hintBarFG);
   _setbkcolor(hintBarBG);
   _clearscreen(_GWINDOW);
	charBufferLength = sprintf(charBuffer, "%s", hintText);
   switch (justifyFlag)
   {
      case JUSTIFY_LEFT    :  _settextposition(1, 2); break;
      case JUSTIFY_CENTER  :  _settextposition(1, 40-charBufferLength/2); break;
      case JUSTIFY_RIGHT   :  _settextposition(1, 79-charBufferLength); break;
   }
	_outtext(charBuffer);

	if (helpMessageFlag)
	{
		charBufferLength = sprintf(charBuffer, "<F1 = Help>");
		_settextposition(1, 80-charBufferLength);
		_outtext(charBuffer);
	}
}	/* end OutputHintBar() */


void RedrawScreen(int helpMessageFlag)
{
	OutputTitleBar();
	ClearDesktop();
	OutputHintBar("", JUSTIFY_LEFT, helpMessageFlag);
}	/* end RedrawScreen() */


void ManageMenu(void)
{
	int leftMargin, numberOfMenuLists, currentMenu, lastMenu;
   int currentMenuLine, numberOfMenuLines;
	int redrawScreen = TRUE, helpMessageFlag = TRUE;
	MENU_LIST_RECORD *currentMenuList;
	unsigned key;
	int ascii, scanCode;

   quitFlag = FALSE;

   numberOfMenuLists = 0;
	while (menuList[numberOfMenuLists++].lastRecord == FALSE);

	currentMenu = 1;
   lastMenu = -1;
	do
	{
		if (redrawScreen)
		{
			RedrawScreen(helpMessageFlag);
			redrawScreen = FALSE;
			lastMenu = -1;
		}

		if (lastMenu != currentMenu)
		{
         int i;

			currentMenuList = menuList[currentMenu].menuList;

			numberOfMenuLines = 0;
			while (currentMenuList[numberOfMenuLines++].lastRecord == FALSE);

         for (i=0; i<numberOfMenuLines; i++)
            if (currentMenuList[i].hot) currentMenuLine = i;

         leftMargin = PrintMenuBar(currentMenu);
		}

      lastMenu = currentMenu;
		if (currentMenu >= 0 && currentMenu < numberOfMenuLists)
			PrintMenuList(currentMenu, leftMargin);
		
			/* Get a key and process it into ascii and scan codes */
		key = _bios_keybrd(keybrdRead);
		ascii = key & 0x00ff;
		scanCode = ascii ? 0 : key >> 8;

		if (ascii)
		{
         int legalKey = FALSE;

			if (ascii == ENTER)
			{
				currentMenuLine = 0;
            while (currentMenuList[currentMenuLine].hot == FALSE) currentMenuLine++;

            (currentMenuList[currentMenuLine].dialogProcPtr)();
            redrawScreen = TRUE;
            legalKey = TRUE;
			}
			else
			{
            int i;

				ascii = toupper(ascii);

					/* Is Key a menu line hot key? */
				for (i=0; i<numberOfMenuLines; i++)
				{
					if (ascii == currentMenuList[i].hotKey)
					{
						currentMenuList[currentMenuLine].hot = FALSE;
						currentMenuList[currentMenuLine=i].hot = TRUE;
                  legalKey = TRUE;

            		(currentMenuList[currentMenuLine].dialogProcPtr)();
            		redrawScreen = TRUE;
						break;
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
			case LEFT		:	if (--currentMenu < 0) currentMenu = numberOfMenuLists-1;
									break;

			case RIGHT		:	if (++currentMenu == numberOfMenuLists) currentMenu = 0;
									break;

			case UP			:	currentMenuLine = 0;
									while (currentMenuList[currentMenuLine].hot == FALSE) currentMenuLine++;
									currentMenuList[currentMenuLine].hot = FALSE;
									if (currentMenuLine == 0) currentMenuLine = numberOfMenuLines;
									while (currentMenuList[--currentMenuLine].selectable != SELECTABLE_YES);
									currentMenuList[currentMenuLine].hot = TRUE;
									break;

			case DOWN		:	currentMenuLine = 0;
									while (currentMenuList[currentMenuLine].hot == FALSE) currentMenuLine++;
									currentMenuList[currentMenuLine].hot = FALSE;
									if (currentMenuLine == numberOfMenuLines-1) currentMenuLine = -1;
									while (currentMenuList[++currentMenuLine].selectable != SELECTABLE_YES);
									currentMenuList[currentMenuLine].hot = TRUE;
									break;

			case F1_KEY		:	(currentMenuList[currentMenuLine].helpProcPtr)();
									redrawScreen = TRUE;
									break;

         default        :  {
                              int i, legalKey = FALSE;

                              for (i=0; i<numberOfMenuLists; i++)
                              {
                                 if (scanCode == menuList[i].hotKey)
                                 {
                                    currentMenu = i;
                                    legalKey = TRUE;
                                 }
                              }
                              if (legalKey == FALSE)
										{
											putchar('\07');
											ClearKeyboardBuffer();
										}
                           }
									break;
		}
	} while (quitFlag == FALSE);
}	/* end ManageMenu() */


int PrintMenuBar(int currentMenu)
{
	int i, previousMenuTitleHot = FALSE;
	int startColumn = 0, leftMargin = 0;

	ClearDesktop();

	_settextwindow(2, 1, 2, 80);
	_setbkcolor(menuBarBG);
	_settextcolor(menuBarFG);
	_clearscreen(_GWINDOW);
	_settextwindow(1, 1, 25, 80);

	_settextposition(2, 1);
	i=0;
	do
	{
		if (i == currentMenu)
		{
			leftMargin = startColumn+1;
         _settextcolor(menuBoxFG);
         _setbkcolor(menuBarBG);
         startColumn += sprintf(charBuffer, "³");
         _outtext(charBuffer);

			_setbkcolor(menuBarHotBG);
			_settextcolor(menuBarHotFG);
			startColumn += sprintf(charBuffer, "%s%s%s", menuList[i].leftText,
										  menuList[i].midText, menuList[i].rightText);
			_outtext(charBuffer);
			previousMenuTitleHot = TRUE;
		}
		else
		{
			if (previousMenuTitleHot)
			{
				_setbkcolor(menuBarBG);
				_settextcolor(menuBoxFG);
				startColumn += sprintf(charBuffer, "³");
				_outtext(charBuffer);
				previousMenuTitleHot = FALSE;
			}
			else
			{
				_setbkcolor(menuBarBG);
				_settextcolor(menuBarFG);
				startColumn += sprintf(charBuffer, " ");
				_outtext(charBuffer);
			}

				/* output the non-highlighted text of this menu title */
			_setbkcolor(menuBarBG);
			_settextcolor(menuBarFG);
			startColumn += sprintf(charBuffer, "%s%", menuList[i].leftText);
			_outtext(charBuffer);

				/* output the Highlight letter of this menu title */
			_setbkcolor(menuBarHotKeyBG);
			_settextcolor(menuBarHotKeyFG);
			startColumn += sprintf(charBuffer, "%s", menuList[i].midText);
			_outtext(charBuffer);

				/* output the rest of the menu title */
			_setbkcolor(menuBarBG);
			_settextcolor(menuBarFG);
			startColumn += sprintf(charBuffer, "%s", menuList[i].rightText);
			_outtext(charBuffer);
		}
	}
	while (menuList[i++].lastRecord == FALSE);

		/* output the vertical bar if the last menu title was hot */
	if (previousMenuTitleHot)
	{
		_setbkcolor(menuBarBG);
		_settextcolor(menuBoxFG);
		startColumn += sprintf(charBuffer, "³");
		_outtext(charBuffer);
	}
	else
	{
		_setbkcolor(menuBarBG);
		_settextcolor(menuBarFG);
		startColumn += sprintf(charBuffer, " ");
		_outtext(charBuffer);
	}

	return(leftMargin);
}	/* end PrintMenuBar() */


void PrintMenuList(int currentMenu, int leftMargin)
{
	int line=3, menuTitleLength, menuLineLength, numberOfMenuLines, currentMenuLine, helpMessageFlag = TRUE;
   char *hintTextPtr = '\0';
	MENU_LIST_RECORD *currentMenuList = menuList[currentMenu].menuList;

	menuTitleLength = strlen(menuList[currentMenu].leftText) +
							strlen(menuList[currentMenu].midText) +
							strlen(menuList[currentMenu].rightText);
	menuLineLength  = strlen(currentMenuList[0].leftText) +
							strlen(currentMenuList[0].midText) +
							strlen(currentMenuList[0].rightText);
	numberOfMenuLines = 0;
	while (currentMenuList[numberOfMenuLines++].lastRecord == FALSE);

		/* output top line menu box */
	_settextwindow(1, 1, 25, 80);
	_setbkcolor(menuBoxBG);
	_settextcolor(menuBoxFG);
	_settextposition(line,leftMargin);
	charBufferLength = sprintf(charBuffer, "Ã%s%s%s¿",
										currentMenuList[numberOfMenuLines].leftText,
										currentMenuList[numberOfMenuLines].midText,
										currentMenuList[numberOfMenuLines].rightText);
	_outtext(charBuffer);
	_settextposition(line++, leftMargin+menuTitleLength+1);
	charBufferLength = sprintf(charBuffer, "Á");
	_outtext(charBuffer);

		/* output each line of the menu */
	for (currentMenuLine = 0; currentMenuLine < numberOfMenuLines; currentMenuLine++)
	{
			/* output left edge of menu line */
		_settextposition(line++, leftMargin);
		_setbkcolor(menuBoxBG);
		_settextcolor(menuBoxFG);
		if (currentMenuList[currentMenuLine].selectable == SELECTABLE_NEVER)
			charBufferLength = sprintf(charBuffer, "Ã");
		else
			charBufferLength = sprintf(charBuffer, "³");
		_outtext(charBuffer);

			/* output body of menu line */
		if (currentMenuList[currentMenuLine].hot)
		{
         hintTextPtr = currentMenuList[currentMenuLine].hintText;
			_setbkcolor(menuLineHotBG);
			_settextcolor(menuLineHotFG);
			charBufferLength = sprintf(charBuffer, "%s%s%s", 
												currentMenuList[currentMenuLine].leftText,
												currentMenuList[currentMenuLine].midText,
												currentMenuList[currentMenuLine].rightText);
			_outtext(charBuffer);
		}
		else switch (currentMenuList[currentMenuLine].selectable)
		{
			case SELECTABLE_YES		:	_setbkcolor(menuLineBG);
												_settextcolor(menuLineFG);
												charBufferLength = sprintf(charBuffer, "%s", currentMenuList[currentMenuLine].leftText);
												_outtext(charBuffer);
												_setbkcolor(menuLineHotKeyBG);
												_settextcolor(menuLineHotKeyFG);
												charBufferLength = sprintf(charBuffer, "%s", currentMenuList[currentMenuLine].midText);
												_outtext(charBuffer);
												_setbkcolor(menuLineBG);
												_settextcolor(menuLineFG);
												charBufferLength = sprintf(charBuffer, "%s", currentMenuList[currentMenuLine].rightText);
												_outtext(charBuffer);
												break;

			case SELECTABLE_NO		:	_setbkcolor(menuLineDeadBG);
												_settextcolor(menuLineDeadFG);
												charBufferLength = sprintf(charBuffer, "%s%s%s",
																					currentMenuList[currentMenuLine].leftText,
																					currentMenuList[currentMenuLine].midText,
																					currentMenuList[currentMenuLine].rightText);
												_outtext(charBuffer);
												break;

			case SELECTABLE_NEVER	:	_setbkcolor(menuBoxBG);
												_settextcolor(menuBoxFG);
												charBufferLength = sprintf(charBuffer, "%s%s%s", 
																					currentMenuList[numberOfMenuLines].leftText,
																					currentMenuList[numberOfMenuLines].midText,
																					currentMenuList[numberOfMenuLines].rightText);
												_outtext(charBuffer);
												break;
		}

			/* output right edge of menu line */
		_setbkcolor(menuBoxBG);
		_settextcolor(menuBoxFG);
		if (currentMenuList[currentMenuLine].selectable == SELECTABLE_NEVER)
			charBufferLength = sprintf(charBuffer, "´");
		else
			charBufferLength = sprintf(charBuffer, "³");
		_outtext(charBuffer);
	}	/* for each line of current menu list */

		/* output the bottom line of the menu box */
	_setbkcolor(menuBoxBG);
	_settextcolor(menuBoxFG);
	_settextposition(line++, leftMargin);
	charBufferLength = sprintf(charBuffer, "À%s%s%sÙ",
										currentMenuList[numberOfMenuLines].leftText,
										currentMenuList[numberOfMenuLines].midText,
										currentMenuList[numberOfMenuLines].rightText);
	_outtext(charBuffer);

      /* Print the Hint Bar */
	OutputHintBar(hintTextPtr, JUSTIFY_LEFT, helpMessageFlag);
}	/* end PrintMenuList() */
