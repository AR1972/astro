
/* WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
	If you make any changes to this file, you must make the identical
	change to UIMENU.INC (in this directory)!
 WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING */

/* Menu constants */
#define CBMAX_CALLS 15 /* proc names are truncated after 15 characters */

#define midFile 		0
#define midEdit 		1
#define midSearch		2
#define midView 		3
#define midRun			4
#define midDebug		5
#define midCalls		6
#define midOptns		7
#define midHelp 		8
#define midCount		9

#define miFileBase		midCount
#define miFileNew		0
#define miFileOpen		1
#define miFileMerge		2
#define miFileSave		3
#define miFileSaveAs		4
#define miFileSaveAll		5
#define miFileCreate		6
#define miFileLoad		7
#define miFileUnload		8
#define miFilePrint		9
#define miFileShell		10
#define miFileExit		11
#define miFileExitQH		12
#define miFileCount		13

#define miEditBase (miFileBase + miFileCount)
#define miEditUndo		0
#define miEditCut		1
#define miEditCopy		2
#define miEditClear		3
#define miEditPaste		4
#define miEditNewSub		5
#define miEditNewFunc		6
#define miEditCount		7

#define miViewBase (miEditBase + miEditCount)
#define miViewSubs		0
#define miViewNextProc		1
#define miViewSplit		2
#define miViewNextStmt		3
#define miViewOutScrn		4
#define miViewInclFile		5
#define miViewInclLns		6
#define miViewCount		7

#define miSearchBase (miViewBase + miViewCount)
#define miSearchFind		0
#define miSearchSel		1
#define miSearchNext		2
#define miSearchChange		3
#define miSearchLabel		4
#define miSearchCount		5

#define miRunBase (miSearchBase + miSearchCount)
#define miRunStart		0
#define miRunRestart		1
#define miRunContinue		2
#define miRunSetCmd		3
#define miRunMakeExe		4
#define miRunMakeLib		5
#define miRunSetMain		6
#define miRunCount		7

#define miDebugBase (miRunBase + miRunCount)
#define miDebugStep		0			//[1]
#define miDebugPStep 		1			//[1]
#define miDebugAddWatch 	2			//[1]
#define miDebugInstantWatch 	3			//[1]
#define miDebugWatchPoint	4			//[1]
#define miDebugDelWatch 	5			//[1]
#define miDebugDelAllWatch	6			//[1]
#define miDebugTraceOn		7			//[1]
#define miDebugHistoryOn	8			//[1]
#define miDebugToggleBp 	9			//[1]
#define miDebugClearAllBp	10			//[1]
#define miDebugBreakOnErr	11			//[1]
#define miDebugSetNextStmt	12			//[1]
#define miDebugCount		13			//[1]

#define miOptnsBase (miDebugBase + miDebugCount)
#define miOptnsDisplay		0
#define miOptnsPaths		1
#define miOptnsMouse		2
#define miOptnsSyntax		3
#define miOptnsFullMenu 	4
#define miOptnsCount		5

#define miHelpBase (miOptnsBase + miOptnsCount)
#define miHelpIndex		0
#define miHelpTable		1
#define miHelpSyntax		2
#define miHelpHelp		3
#define miHelpStarted		4		//[2]
#define miHelpKeyboard		5		//[2]
#define miHelpAbout		6		//[2]
#define miHelpHowToUse		7
#define miHelpCount		8		//[2]

#define miNoMenuBase (miHelpBase + miHelpCount)		//[1]

#define miNextWindow		0
#define miPreviousWindow	1
#define miViewPrev		2
#define miViewFull		3
#define miGotoCursor		9
#define miHelpKey		10
#define miEditCut2		12
#define miEditClear2		13
#define miViewPrevProc		14
#define miWndGrow		15
#define miWndShrink		16
#define miWndRestore		17
#define miWndMaximize		18

/* For the new help system, HelpClose is an accelerator (and defined here).
   For all other help systems, HelpClose is a menu item and is defined with
   the Help Menu */

#define miHelpClose		19
#define miHelpBack		20
#define miHelpNext		21
#define miEnter			22



#define midFileNew		(miFileBase + miFileNew)
#define midFileOpen		(miFileBase + miFileOpen)
#define midFileMerge		(miFileBase + miFileMerge)
#define midFileSave		(miFileBase + miFileSave)
#define midFileSaveAs		(miFileBase + miFileSaveAs)
#define midFileSaveAll		(miFileBase + miFileSaveAll)
#define midFileCreate		(miFileBase + miFileCreate)
#define midFileLoad		(miFileBase + miFileLoad)
#define midFileUnload		(miFileBase + miFileUnload)
#define midFilePrint		(miFileBase + miFilePrint)
#define midFileShell		(miFileBase + miFileShell)
#define midFileExit		(miFileBase + miFileExit)
#define midFileExitQH		(miFileBase + miFileExitQH)

#define midEditUndo		(miEditBase + miEditUndo)
#define midEditCut		(miEditBase + miEditCut)
#define midEditCopy		(miEditBase + miEditCopy)
#define midEditClear		(miEditBase + miEditClear)
#define midEditPaste		(miEditBase + miEditPaste)
#define midEditNewSub		(miEditBase + miEditNewSub)
#define midEditNewFunc		(miEditBase + miEditNewFunc)

#define midViewSubs		(miViewBase + miViewSubs)
#define midViewNextProc 	(miViewBase + miViewNextProc)
#define midViewSplit		(miViewBase + miViewSplit)
#define midViewNextStmt 	(miViewBase + miViewNextStmt)
#define midViewOutScrn		(miViewBase + miViewOutScrn)
#define midViewInclFile 	(miViewBase + miViewInclFile)
#define midViewInclLns		(miViewBase + miViewInclLns)

#define midSearchFind		(miSearchBase + miSearchFind)
#define midSearchSel		(miSearchBase + miSearchSel)
#define midSearchNext		(miSearchBase + miSearchNext)
#define midSearchChange 	(miSearchBase + miSearchChange)
#define midSearchLabel		(miSearchBase + miSearchLabel)

#define midRunStart		(miRunBase + miRunStart)
#define midRunRestart		(miRunBase + miRunRestart)
#define midRunContinue		(miRunBase + miRunContinue)
#define midRunSetCmd		(miRunBase + miRunSetCmd)
#define midRunMakeExe		(miRunBase + miRunMakeExe)
#define midRunMakeLib		(miRunBase + miRunMakeLib)
#define midRunSetMain		(miRunBase + miRunSetMain)

#define midStep 		(miDebugBase + miDebugStep)	//[1]
#define midPStep 		(miDebugBase + miDebugPStep)	//[1]
#define midDebugAddWatch	(miDebugBase + miDebugAddWatch)
#define midDebugInstantWatch	(miDebugBase + miDebugInstantWatch)
#define midDebugWatchPoint	(miDebugBase + miDebugWatchPoint)
#define midDebugDelWatch	(miDebugBase + miDebugDelWatch)
#define midDebugDelAllWatch	(miDebugBase + miDebugDelAllWatch)
#define midDebugTraceOn 	(miDebugBase + miDebugTraceOn)
#define midDebugHistoryOn	(miDebugBase + miDebugHistoryOn)
#define midDebugToggleBp	(miDebugBase + miDebugToggleBp)
#define midDebugClearAllBp	(miDebugBase + miDebugClearAllBp)
#define midDebugBreakOnErr	(miDebugBase + miDebugBreakOnErr)
#define midDebugSetNextStmt	(miDebugBase + miDebugSetNextStmt)

#define midHelpSyntax		(miHelpBase + miHelpSyntax)
#define midHelpHelp		(miHelpBase + miHelpHelp)
#define midHelpIndex		(miHelpBase + miHelpIndex)
#define midHelpTable		(miHelpBase + miHelpTable)

#define midHelpStarted		(miHelpBase + miHelpStarted)	//[2]
#define midHelpKeyboard 	(miHelpBase + miHelpKeyboard)	//[2]
#define midHelpAbout		(miHelpBase + miHelpAbout)	//[2]
#define midHelpHowToUse 	(miHelpBase + miHelpHowToUse)

#define midOptnsDisplay 	(miOptnsBase + miOptnsDisplay)
#define midOptnsPaths		(miOptnsBase + miOptnsPaths)
#define midOptnsMouse		(miOptnsBase + miOptnsMouse)
#define midOptnsSyntax		(miOptnsBase + miOptnsSyntax)
#define midOptnsFullMenu	(miOptnsBase + miOptnsFullMenu)

/* The following id's aren't really menu items at all.	They are
   just so we can use the menu dispatcher to Process some control Keys */
#define midViewPrev		(miNoMenuBase + miViewPrev)
#define midViewFull		(miNoMenuBase + miViewFull)
#define midGoUntilCursor	(miNoMenuBase + miGotoCursor)
#define midHelpKey		(miNoMenuBase + miHelpKey)
#define midNextWindow		(miNoMenuBase + miNextWindow)
#define midPreviousWindow	(miNoMenuBase + miPreviousWindow)
#define midEditCut2		(miNoMenuBase + miEditCut2)
#define midEditClear2		(miNoMenuBase + miEditClear2)
#define midViewPrevProc 	(miNoMenuBase + miViewPrevProc)
#define midWndGrow		(miNoMenuBase + miWndGrow)
#define midWndShrink		(miNoMenuBase + miWndShrink)
#define midWndRestore		(miNoMenuBase + miWndRestore)
#define midWndMaximize		(miNoMenuBase + miWndMaximize)

#define midHelpClose		(miNoMenuBase + miHelpClose)
#define midHelpBack		(miNoMenuBase + miHelpBack)
#define midHelpNext		(miNoMenuBase + miHelpNext)
#define midEnter		(miNoMenuBase + miEnter)
