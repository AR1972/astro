/*
	COW : Character Oriented Windows

	prompt.c : Default PromptSwapDisk routine
		(english only, app should provide their own)
*/

#define COW
#include <cow.h>
#include <itl.h>
#include <udialog.h>

STATIC char szDrive[] = "Please insert in drive ?:";
#define cchPleaseInsert	25


VOID FARPUBLIC PromptSwapDisk(char far *, WORD);



VOID FARPUBLIC
PromptSwapDisk(lszPath, ifile)
/*
  -- build the prompt string and call message box to get the swap disk
	replaced
  -- NOTE: "lszPath" must be copied before calling MessageBox
	(we can at most call MessageBox at this time)
*/
char far * lszPath;
WORD	ifile;
	{
	REGISTER char *pchSrc;
	REGISTER char *pchDest;
	char	szBuff[50];

	Unreferenced(ifile);

	pchDest = szBuff;
	pchSrc = "Cannot find ";
	while ((*pchDest++ = *pchSrc++) != '\0')
		;
	pchDest--;
	/* now copy the name */
	InitSpecialMessageBox(lszPath);
	szDrive[cchPleaseInsert - 2] = lszPath[0];

	/* copy the string */
	while ((*pchDest++ = *lszPath++) != '\0')
		;

	MessageBox(szBuff, szDrive, NULL, MB_RETRY);
	EndSpecialMessageBox();
	}
