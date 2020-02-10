
; WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
;
;	If you make any changes to this file, you must make the identical
;	change to UIMENU.H (in this directory)!
;
; WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING

; Menu constants
CBMAX_CALLS		EQU 15 	; proc names are truncated after 15 characters

midFile 		EQU 0
midEdit 		EQU 1
midSearch		EQU 2
midView 		EQU 3
midRun			EQU 4
midDebug		EQU 5
midCalls		EQU 6
midOptns		EQU 7
midHelp 		EQU 8
midCount		EQU 9

miFileBase		EQU midCount
miFileNew		EQU 0
miFileOpen		EQU 1
miFileMerge		EQU 2
miFileSave		EQU 3
miFileSaveAs		EQU 4
miFileSaveAll		EQU 5
miFileCreate		EQU 6
miFileLoad		EQU 7
miFileUnload		EQU 8
miFilePrint		EQU 9
miFileShell		EQU 10
miFileExit		EQU 11
miFileExitQH		EQU 12
miFileCount		EQU 13

miEditBase		EQU (miFileBase + miFileCount)
miEditUndo		EQU 0
miEditCut		EQU 1
miEditCopy		EQU 2
miEditClear		EQU 3
miEditPaste		EQU 4
miEditNewSub		EQU 5
miEditNewFunc		EQU 6
miEditCount		EQU 7

miViewBase		EQU (miEditBase + miEditCount)
miViewSubs		EQU 0
miViewNextProc		EQU 1
miViewSplit		EQU 2
miViewNextStmt		EQU 3
miViewOutScrn		EQU 4
miViewInclFile		EQU 5
miViewInclLns		EQU 6
miViewCount		EQU 7

miSearchBase		EQU (miViewBase + miViewCount)
miSearchFind		EQU 0
miSearchSel		EQU 1
miSearchNext		EQU 2
miSearchChange		EQU 3
miSearchLabel		EQU 4
miSearchCount		EQU 5

miRunBase		EQU (miSearchBase + miSearchCount)
miRunStart		EQU 0
miRunRestart		EQU 1
miRunContinue		EQU 2
miRunSetCmd		EQU 3
miRunMakeExe		EQU 4
miRunMakeLib		EQU 5
miRunSetMain		EQU 6
miRunCount		EQU 7

miDebugBase		EQU (miRunBase + miRunCount)
miDebugStep		EQU 0		
miDebugPStep		EQU 1		
miDebugAddWatch 	EQU 2		
miDebugInstantWatch	EQU 3		
miDebugWatchPoint	EQU 4		
miDebugDelWatch 	EQU 5		
miDebugDelAllWatch	EQU 6		
miDebugTraceOn		EQU 7		
miDebugHistoryOn	EQU 8		
miDebugToggleBp 	EQU 9		
miDebugClearAllBp	EQU 10		
miDebugBreakOnErr	EQU 11		
miDebugSetNextStmt	EQU 12		
miDebugCount		EQU 13		

miOptnsBase		EQU (miDebugBase + miDebugCount)
miOptnsDisplay		EQU 0
miOptnsPaths		EQU 1
miOptnsMouse		EQU 2
miOptnsSyntax		EQU 3
miOptnsFullMenu 	EQU 4
miOptnsCount		EQU 5

miHelpBase		EQU (miOptnsBase + miOptnsCount)
miHelpIndex		EQU 0
miHelpTable		EQU 1
miHelpSyntax		EQU 2
miHelpHelp		EQU 3
miHelpStarted		EQU 4					
miHelpKeyboard		EQU 5					
miHelpAbout		EQU 6					
miHelpHowToUse		EQU 7
miHelpCount		EQU 8

miNoMenuBase		EQU (miHelpBase + miHelpCount)	
miNextWindow		EQU 0
miPreviousWindow	EQU 1
miViewPrev		EQU 2
miViewFull		EQU 3
miGotoCursor		EQU 9
miHelpKey		EQU 10
miEditCut2		EQU 12
miEditClear2		EQU 13
miViewPrevProc		EQU 14
miWndGrow		EQU 15
miWndShrink		EQU 16
miWndRestore		EQU 17
miWndMaximize		EQU 18

; For the new help system, HelpClose is an accelerator (and defined here).
; For all other help systems, HelpClose is a menu item and is defined with
; the Help Menu

miHelpClose		EQU 19
miHelpBack		EQU 20
miHelpNext		EQU 21
miEnter			EQU 22



midFileNew		EQU (miFileBase + miFileNew)
midFileOpen		EQU (miFileBase + miFileOpen)
midFileMerge		EQU (miFileBase + miFileMerge)
midFileSave		EQU (miFileBase + miFileSave)
midFileSaveAs		EQU (miFileBase + miFileSaveAs)
midFileSaveAll		EQU (miFileBase + miFileSaveAll)
midFileCreate		EQU (miFileBase + miFileCreate)
midFileLoad		EQU (miFileBase + miFileLoad)
midFileUnload		EQU (miFileBase + miFileUnload)
midFilePrint		EQU (miFileBase + miFilePrint)
midFileShell		EQU (miFileBase + miFileShell)
midFileExit		EQU (miFileBase + miFileExit)
midFileExitQH		EQU (miFileBase + miFileExitQH)

midEditUndo		EQU (miEditBase + miEditUndo)
midEditCut		EQU (miEditBase + miEditCut)
midEditCopy		EQU (miEditBase + miEditCopy)
midEditClear		EQU (miEditBase + miEditClear)
midEditPaste		EQU (miEditBase + miEditPaste)
midEditNewSub		EQU (miEditBase + miEditNewSub)
midEditNewFunc		EQU (miEditBase + miEditNewFunc)

midViewSubs		EQU (miViewBase + miViewSubs)
midViewNextProc 	EQU (miViewBase + miViewNextProc)
midViewSplit		EQU (miViewBase + miViewSplit)
midViewNextStmt 	EQU (miViewBase + miViewNextStmt)
midViewOutScrn		EQU (miViewBase + miViewOutScrn)
midViewInclFile 	EQU (miViewBase + miViewInclFile)
midViewInclLns		EQU (miViewBase + miViewInclLns)

midSearchFind		EQU (miSearchBase + miSearchFind)
midSearchSel		EQU (miSearchBase + miSearchSel)
midSearchNext		EQU (miSearchBase + miSearchNext)
midSearchChange 	EQU (miSearchBase + miSearchChange)
midSearchLabel		EQU (miSearchBase + miSearchLabel)

midRunStart		EQU (miRunBase + miRunStart)
midRunRestart		EQU (miRunBase + miRunRestart)
midRunContinue		EQU (miRunBase + miRunContinue)
midRunSetCmd		EQU (miRunBase + miRunSetCmd)
midRunMakeExe		EQU (miRunBase + miRunMakeExe)
midRunMakeLib		EQU (miRunBase + miRunMakeLib)
midRunSetMain		EQU (miRunBase + miRunSetMain)

midStep			EQU (miDebugBase + miDebugStep)		
midPStep		EQU (miDebugBase + miDebugPStep)	
midDebugAddWatch	EQU (miDebugBase + miDebugAddWatch)
midDebugInstantWatch	EQU (miDebugBase + miDebugInstantWatch)
midDebugWatchPoint	EQU (miDebugBase + miDebugWatchPoint)
midDebugDelWatch	EQU (miDebugBase + miDebugDelWatch)
midDebugDelAllWatch	EQU (miDebugBase + miDebugDelAllWatch)
midDebugTraceOn 	EQU (miDebugBase + miDebugTraceOn)
midDebugHistoryOn	EQU (miDebugBase + miDebugHistoryOn)
midDebugToggleBp	EQU (miDebugBase + miDebugToggleBp)
midDebugClearAllBp	EQU (miDebugBase + miDebugClearAllBp)
midDebugBreakOnErr	EQU (miDebugBase + miDebugBreakOnErr)
midDebugSetNextStmt	EQU (miDebugBase + miDebugSetNextStmt)

midOptnsDisplay		EQU (miOptnsBase + miOptnsDisplay)
midOptnsPaths		EQU (miOptnsBase + miOptnsPaths)
midOptnsMouse		EQU (miOptnsBase + miOptnsMouse)
midOptnsSyntax		EQU (miOptnsBase + miOptnsSyntax)
midOptnsFullMenu	EQU (miOptnsBase + miOptnsFullMenu)

midHelpSyntax		EQU (miHelpBase + miHelpSyntax)
midHelpHelp		EQU (miHelpBase + miHelpHelp)
midHelpIndex		EQU (miHelpBase + miHelpIndex)
midHelpTable		EQU (miHelpBase + miHelpTable)
midHelpStarted		EQU (miHelpBase + miHelpStarted)	
midHelpKeyboard 	EQU (miHelpBase + miHelpKeyboard)	
midHelpAbout		EQU (miHelpBase + miHelpAbout)		
midHelpHowToUse 	EQU (miHelpBase + miHelpHowToUse)

; The following id's aren't really menu items at all.	They are
; just so we can use the menu dispatcher to Process some control Keys
midViewPrev		EQU (miNoMenuBase + miViewPrev)
midViewFull		EQU (miNoMenuBase + miViewFull)
midGoUntilCursor	EQU (miNoMenuBase + miGotoCursor)
midHelpKey		EQU (miNoMenuBase + miHelpKey)
midNextWindow		EQU (miNoMenuBase + miNextWindow)
midPreviousWindow	EQU (miNoMenuBase + miPreviousWindow)
midEditCut2		EQU (miNoMenuBase + miEditCut2)
midEditClear2		EQU (miNoMenuBase + miEditClear2)
midViewPrevProc 	EQU (miNoMenuBase + miViewPrevProc)
midWndGrow		EQU (miNoMenuBase + miWndGrow)
midWndShrink		EQU (miNoMenuBase + miWndShrink)
midWndRestore		EQU (miNoMenuBase + miWndRestore)
midWndMaximize		EQU (miNoMenuBase + miWndMaximize)

midHelpClose		EQU (miNoMenuBase + miHelpClose)
midHelpBack		EQU (miNoMenuBase + miHelpBack)
midHelpNext		EQU (miNoMenuBase + miHelpNext)
midEnter		EQU (miNoMenuBase + miEnter)
