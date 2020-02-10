;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*
	CW : Character Oriented Windows

	kkcfltr.h : Kana Kanji Conveter Filter
*/

/***BEGIN_PUBLIC***/

/* Public variables for kkc filter */

extern BOOL PASCAL fKkcAvailable;	/* TRUE -> kkif drv. initialized */
extern BOOL PASCAL fKkcEnabled;		/* TRUE -> kkc enabled */
extern KKCV PASCAL kkcv;			/* Common area for kid and kkc filter */
extern BOOL PASCAL fKkcMessage;		/* TRUE -> converted string pending */


/* kkc filter mode */

#define	kkmodeNil		0
#define	kkmodeLYR		1		/* layer displays undetermined string */
#define	kkmodeAPP		2		/* ap displays undetermined string */


#ifndef NOPROCS
BOOL FARPUBLIC FInitKkc(PWND);
VOID FARPUBLIC TermKkc(VOID);
BOOL FARPUBLIC SetModeKkc(WORD);
BOOL FARPUBLIC FSuspendKkc(BOOL);
BOOL FARPUBLIC FEnableKkc(BOOL);
BOOL FARPUBLIC FActivateKkc(BOOL);
BOOL FARPUBLIC FActiveKkc(VOID);
BOOL FARPUBLIC FSetPosKkc(WORD);
BOOL FARPUBLIC GetPosKkc(VOID);
BOOL FARPUBLIC FFlushKkc(VOID);
VOID FARPUBLIC SetWindowKkc(PRRC);
VOID FARPUBLIC SetCursorKkc(RX, RY);
VOID FARPUBLIC TextOutAttrX(PWND, RX, RY, PRRC, CHAR FAR *, CHAR FAR *, short, short);
#endif /* !NOPROCS */

/***END_PUBLIC***/


//BOOL FARPRIVATE FKKCMsg(PMSG);
//BOOL FARPRIVATE KKCFilter(PMSG);
