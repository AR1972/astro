/******************************************************************************

  menu.h : menu public interface

******************************************************************************/

extern HMNU hmnuMenuBar;
extern HMNU hmnuMenuTest;
extern HMNU hmnuMenuPop;
extern MPVKEYID *prgmpvkeyid;

#define PMENUITEM PMTM

VOID FAR MenuCommand(PWND, WORD);


// for simplicity all menu ids are public 

#define midFile   0x100

#define midFind         (midFile+1)
#define midReport       (midFile+2)
#define midAutoexecBat  (midFile+3)
#define midConfigSys    (midFile+4)
#define midSystemIni    (midFile+5)
#define midWinIni       (midFile+6)
#define midMsmailIni    (midFile+7)
#define midProtocolIni  (midFile+8)
#define midDblSpaceIni	(midFile+9)
#define midMemMakerSts	(midFile+0x0A)
#define midQuit 	(midFile+0x0B)


#define midUtil   0x200

#define midBlockDisplay (midUtil+1)
#define midBrowser      (midUtil+2)
#define midInsert       (midUtil+3)
#define midPrtTst       (midUtil+4)
#define midBlackWhite   (midUtil+5)


#define midHelp   0x300
#define midAbout    (midHelp+1)
