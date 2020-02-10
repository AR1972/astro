/******************************************************************************

  smenu.h : (contains data)
  included by menu.c for Roman menus

******************************************************************************/


#define item1(sz, id, num) id, 0x0100, (WORD) sz, (WORD) num,
#define itemS 0, 0x0004, (WORD) NULL,
#define itemD(sz, id, num) id, 0x0101, (WORD) sz, (WORD) num,
#define itemB(sz, id, sub, num) id, 0x0240, (WORD) sz, (WORD) num, (WORD) sub,

WORD menuFile[]=
  {
  0, 13,
  item1("~Find File...", midFind, "")
  item1("~Print Report ...", midReport, "")
  itemS
  item1("~1 AUTOEXEC.BAT", midAutoexecBat, "")
  item1("~2 CONFIG.SYS", midConfigSys, "")
  item1("~3 SYSTEM.INI", midSystemIni, "")
  item1("~4 WIN.INI", midWinIni, "")
  item1("~5 MSMAIL.INI", midMsmailIni, "")
  item1("~6 PROTOCOL.INI", midProtocolIni, "")
  item1("~7 DBLSPACE.INI", midDblSpaceIni, "")
  item1("~8 MEMMAKER.STS", midMemMakerSts, "")
  itemS
  item1("E~xit          F3", midQuit, "")
  };

WORD *pmenuFile = &menuFile[0];

WORD menuUtil[]=
  {
  0, 6,
  item1("~Memory Block Display ...", midBlockDisplay, "")
  item1("Memory ~Browser ...", midBrowser, "")
  item1("~Insert Command ...", midInsert, "")
  item1("~Test Printer ...", midPrtTst, "Paste it in")
  itemS
  item1("Black & ~White         F5", midBlackWhite, "")
  };

WORD *pmenuUtil = &menuUtil[0];

WORD menuHelp[]=
  {
  0, 1,
  item1("~About ...", midAbout, "")
  };

WORD *pmenuHelp = &menuHelp[0];


WORD menuBlade[]=
  {
  0, 3,
  itemB("~File", midFile, &pmenuFile, "")
  itemB("~Utilities", midUtil, &pmenuUtil, "")
  itemB("~Help", midHelp, &pmenuHelp, "")
  };

WORD *pmenuBlade = &menuBlade[0];
