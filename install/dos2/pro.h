/***************************************************************************

INCLUDE FILE: Progress.h


   Copyright (C) Microsoft, 1991

HISTORY:

   Modified by:      Date:       Comment:

   TL                11/01/87    Created



***************************************************************************/

BOOL EXPORT ProDlgProc(HWND, WORD, WORD, DWORD);
BOOL PUBLIC ProInit(HANDLE,HANDLE);
void PUBLIC ProClear(HWND hDlg);
HWND PUBLIC ProOpen(HWND,int,char *);
BOOL PUBLIC ProClose(void);
BOOL PUBLIC ProSetBarRange(int);
BOOL PUBLIC ProSetBarPos(int);
BOOL PUBLIC ProDeltaPos(int);
BOOL PUBLIC ProSetText (int,LPSTR);
void PUBLIC ProToTop (void);
BOOL FAR cdecl ProPrintf (int,LPSTR,...);

#define ID_BAR	      100
#define CLS_STEXT    "stext"
