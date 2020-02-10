/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * MSD.H - Main header file
 *
 * Note:  _MSD.H and MSD.H are interdependent.
 ********************************************************************/

#include <ctype.h>
#include <conio.h>
#include <dos.h>
#include <bios.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <direct.h>
#include <sys\types.h>
#include <sys\stat.h>
#include "NETBIOS2.H"
#include "NETCONS.H"
#include "NETERR.H"
#include "CONFIG.H"
#include "REMUTIL.H"
#include "WKSTA.H"
#include "SERVER.H"
#include "SERVICE.H"
#include "STRINGS.H"

#undef  _MAX_PATH
#define _MAX_PATH 126 + 13


/* Include a CW_INCLUDED define in the make file, or uncomment */
/*   "#define CW_INCLUDED" to include the CW code.  Turn the   */
/*   warning level down to 3 when compiling CW code.           */

/*
#define CW_INCLUDED 1
*/

#ifdef CW_INCLUDED

#define BLADE       1    /* Enable CW 3.1 extensions and features. */
#define ADM         1    /* Use 3.1 dialog manager.                */
#define DEBUG       1    /* Include debugging CW info.             */
#define CC          1    /* Use normal MS C 6.0 compile.           */
#define SMM         1    /* The CW menu manager                    */
#define WIN_BTN     1    /* For my non-CW buttons                  */

#define BASED       1

/* To reduce warnings, I undef these */

#undef min
#undef max
#undef NULL

#ifdef BASED

extern _segment pWndSeg;

#endif

#define WM_DLGUSER 20

#define LINT_ARGS   1

#include "cwindows.h"
#include "csdm.h"
#include "scr.h"
#include "menu.h"
#include "csdmtmpl.h"


/* colors from isaUserMin to isaUserMax-1 */
/*   are available for application use    */

#define isaSummaryBlank       (isaUserMin)
#define isaSummaryText        (isaUserMin+1)
#define isaInfoActive         (isaUserMin+2)
#define isaInfoActiveBorder   (isaUserMin+3)
#define isaInfoInactive       (isaUserMin+4)
#define isaStatusLine         (isaUserMin+5)
#define isaAlternate          (isaUserMin+6)
#define isaMemoryMap          (isaUserMin+7)
#define isaSummaryBtnShadow   (isaUserMin+8)
#define isaMessageBtn1        (isaUserMin+9)
#define isa3DBtnHilite        (isaUserMin+10)
#define isaTemp               (isaUserMin+15)


typedef struct _ISA_PAIR
{
  WORD wFore;   /* Foreground color */
  WORD wBack;   /* Background color */
} ISA_PAIR;

extern ISA_PAIR aisapColor[];
extern ISA_PAIR aisapBlackAndWhite[];

#define STATIC
#define REGISTER register
#define Unreferenced(x)         ((void)x)

#define MainProcCallID       1
#define MainMenuCallID       2
#define InfoWndCallID        3
#define InfoTxtWndCallID    24
#define StatusLineCallID     5

#define MainProcExtraBytes  20

#define NumBtns            13
#define BaseBtnID          10
#define SUM_BUTTON_WIDTH   21
#define SUM_COLUMN_ONE      1
#define SUM_COLUMN_TWO     46

/* Defines for Window Extra Bytes */

#define WEB_WINDOW_TITLE      1
#define WEB_MIN_SCROLL        2
#define WEB_MAX_SCROLL        3
#define WEB_NMBR_LINES        4
#define WEB_SCROLLABLE_LINES  5
#define WEB_PQSZ_TEXT         6
#define WEB_KEEP_FOCUS        7

/* Defines for the info windows */

#define BaseCtrlID      20
#define OkButtonID      21
#define CancelButtonID  22
#define BrowseButtonID  23
#define InfoTxtID       24
#define InfoScrollID    25

/* Common PWNDs */

extern PWND_DESKTOP PASCAL pwndDesktop;
extern PWND pwndSummaryFrame;
extern PWND pwndMainFrame;
extern PWND pwndStatusLine;

extern PLFN_WNDPROC OldMsgBoxProc;
extern PLFN_WNDPROC OldEditItemProc;


/* User defined window messages */

#define WM_INFO_WND_CLOSED          WM_USER + 1
#define WM_NEW_STATUS_LINE_STRING   WM_USER + 2
#define MY_EN_SETFOCUS              WM_USER + 3


/* Status string defines */
#define ST_WORKING                  10001
#define ST_WARN_WINDOWS_USER        10002
#define ST_REPORT_COMPLETED         10003
#define ST_SEARCHING                10004
#define ST_INSERT_DLG1              10005
#define ST_INSERT_DLG2              10006
#define ST_INSERT_DLG3              10007
#define ST_INSERT_DLG4              10008
#define ST_CUST_INFO                10009
#define ST_VIEW_WHICH_FILE          10010
#define ST_FINDFILE2                10011


/* The value of IDD_USER is up to you. Just make sure that it is  */
/*   greater than 10.  The first 10 values are for pre-defined CW */
/*   controls like IDOK and IDCANCEL...                           */

#define IDD_USER 20


/* Dialog Box Template */

typedef struct _DIALOG_BOX {
                DLGHEADTEMPLATE dlgHeader;
                DLGITEMTEMPLATE rgtm[];
              } DIALOG_BOX;


/* Dialog Boxes */

extern DIALOG_BOX dlgReport;
extern DIALOG_BOX dlgFindFile1;
extern DIALOG_BOX dlgFindFile2;
extern DIALOG_BOX dlgInsertCmd1;
extern DIALOG_BOX dlgInsertCmd2;
extern DIALOG_BOX dlgInsertCmd3;
extern DIALOG_BOX dlgInsertCmd4;
extern DIALOG_BOX dlgTestPrinter;
extern DIALOG_BOX dlgCustInfo;
extern DIALOG_BOX dlgAbout;
extern DIALOG_BOX dlgMemoryBlockDisplay;
extern DIALOG_BOX dlgMemoryBrowser;
extern DIALOG_BOX dlgViewWhichFile;
extern DIALOG_BOX dlgWarnWindowsUser;

#define SetAccelerator(pwnd, Position, szText) \
   ((PWND_GEN)(pwnd))->aclDialog = (((BYTE)((Position) - 1)) << 8) + \
         *((unsigned char *)(szText) + ((Position) - 1))

extern BOX boxInfoBox;


#else /* CW not included */

#define VOID  void
#define WORD  unsigned int
#define DWORD unsigned long
#define BOOL  unsigned int
#define CHAR  char
#define BYTE  unsigned char
#define FAR   _far
#define FALSE 0
#define TRUE  1
#define BIT   unsigned
#define BITS  unsigned

#endif

#define PSZ      char *
#define INT      int
#define UCHAR    unsigned char
#define LPWORD   WORD FAR *
#define WINAPI  _far _pascal
typedef unsigned int    UINT;
typedef const char FAR* LPCSTR;


/* FAR_INFO_DATA controls how the info data will be stored */
/*   TRUE means the info string data will be far.          */

#define FAR_INFO_STRINGS     TRUE

#ifdef  FAR_INFO_STRINGS

#define QSZ         CHAR FAR *
#define Qmalloc     _fmalloc
#define Qfree       _ffree
#define Qmemset     _fmemset
#define Qstrcpy     _fstrcpy
#define Qstrcat     _fstrcat
#define Qstrlen     _fstrlen
#define Qstrncpy    _fstrncpy
#define Qexpand     _fexpand

#else

#define QSZ         CHAR *
#define Qmalloc     malloc
#define Qfree       free
#define Qmemset     memset
#define Qstrcpy     strcpy
#define Qstrcat     strcat
#define Qstrlen     strlen
#define Qstrncpy    strncpy
#define Qexpand     _expand

#endif


/* Defining LIB is unnecessary as the #defines it provides exist already */
/* #define LIB */
#include "ver.h"

#define DWORDUP(x) ((BYTE *) (((WORD) x & 0x03) ? ((WORD) x | 0x03) + 1 : (WORD) x))
#define ParseLine  IsMsDos

/* Record Types */

#define IDI_ALL_RECORDS                0
#define IDI_MSD_HEADER_RECORD          1
#define IDI_CUSTOMER_INFORMATION       2
#define IDI_SUMMARY_SCREEN             3
#define IDI_COMPUTER_RECORD            4
#define IDI_MEMORY_RECORD              5
#define IDI_VIDEO_RECORD               6
#define IDI_NETWORK_RECORD             7
#define IDI_OS_VERSION_RECORD          8
#define IDI_MOUSE_RECORD               9
#define IDI_OTHER_ADAPTERS_RECORD     10
#define IDI_DISK_DRIVE_RECORD         11
#define IDI_LPT_RECORD                12
#define IDI_COM_RECORD                13
#define IDI_IRQ_RECORD                14
#define IDI_TSR_PROGRAMS_RECORD       15
#define IDI_DEVICE_DRIVERS_RECORD     16

#define IDI_FIRST_RECORD_TO_SUMMARIZE IDI_COMPUTER_RECORD
#define IDI_LAST_RECORD_TO_SUMMARIZE  IDI_COM_RECORD
#define IDI_SECOND_COLUMN             IDI_MOUSE_RECORD
#define NMBR_OF_RECORDS               17

extern WORD rwRecordTypes[];  /* Record type numbers in a convenient array */
extern WORD wMaxRecords;


/* Error Indications for ShowError */

#ifdef CW_INCLUDED
#define ERR_OK_BUTTON                 MB_OK
#else
#define ERR_OK_BUTTON                 1
#endif


/* FindFile defines */

#define SEARCH_FLOPPIES          0x0001
#define SEARCH_LOCAL_DRIVES      0x0002
#define SEARCH_NET_DRIVES        0x0004
#define SEARCH_ROOT              0x0100
#define RECURSE_INTO_SUB_DIRS    0x0200
#define SEARCH_VERSION           0x0400
#define SEARCH_LANMAN_ROOT       0x2000
#define SEARCH_WINDIR            0x4000
#define SEARCH_BOOT_DRIVE        0x8000


/* Report defines */

#define LINES_PER_PAGE  58
#define REPORT_WIDTH    72


/* Global Variables */

extern BOOL fCwIsReady;       /* TRUE when CW is initialized           */
extern BOOL fBlackWhite;      /* TRUE for black and white operation    */
extern BOOL fFastStart;       /* TRUE for no initial detection         */
extern BOOL fReportFlag;      /* TRUE if a report is underway          */
extern BOOL fReportOnly;      /* TRUE if "/F filename" was used, or if */
                              /*   CW_INCLUDED is commented out        */
extern BOOL fSummaryOnly;     /* TRUE if MSD is to report summary      */
                              /*   information only to a file          */
extern BOOL fCriticalError;   /* Critical Error Flag                   */
extern BOOL fWindowsRunning;  /* TRUE if windows is running            */

WORD wDosMajor;           /* Stores the DOS version */
WORD wDosMinor;


/* Variables for CW */

#ifdef CW_INCLUDED

extern BYTE fAdjustMsgBox;

extern HMNU hmnuMenuBar;
extern PBOX pboxInfoBox;        /* Box description for the info window */

#endif


/* Variables for reporting */

extern WORD wLineCount;         /* Current line number          */
extern WORD wPageCount;         /* Current page number          */
extern WORD wColumnCount;       /* Current column number        */
extern WORD wReportIndent;      /* Size of report's left indent */

extern PSZ  pszReportFilename;  /* Pointer to command line report filename */


/* Global Strings */

extern PSZ  paszReportTo[];
extern WORD wReportToIndex;

extern PSZ  pszVersionNumber;

extern PSZ  pszNull;
extern PSZ  pszYes;
extern PSZ  pszNo;
extern PSZ  pszNo_;
extern PSZ  pszNone;
extern PSZ  pszUnknown;
extern PSZ  pszInsufMemory;
extern PSZ  pszErrorOpening;
extern PSZ  pszErrorClosing;
extern PSZ  pszErrorWriting;
extern PSZ  pszErrorReading;

extern PSZ  pszCon;

extern PSZ  pszOutput;          /* Used for OpenFile/fopen */

extern BOOL rgfReportItemFlag[];

#define RIF_MEMORY_BROWSER         17
#define RIF_AUTOEXEC_BAT           18
#define RIF_CONFIG_SYS             19
#define RIF_SYSTEM_INI             20
#define RIF_WIN_INI                21
#define RIF_LANMAN_INI             22
#define RIF_PROTOCOL_INI           23
#define RIF_START                  RIF_AUTOEXEC_BAT

#define MAX_REPORT_ITEM_FLAGS	   26

#define MAX_INI_LINE_LENGTH       255

#define HD_SEARCH               65530
#define HD_REPLACE_ALL          65531

extern PSZ  paszDefaultMsdIni[];

extern char * rgszSystemFiles[];
extern WORD iszMax2;
extern WORD rgwSystemFiles[];


#define INI_COMMAND_COL             0
#define INI_SECTION_COL            49
#define INI_FILENAME_COL           60

extern PSZ  pszInsertCommand;
extern PSZ  pszInsertSection;
extern PSZ  pszInsertFilename;

extern PSZ  pszHeaderFormatString;

extern PSZ  pszPostscriptTest1;
extern PSZ  pszPostscriptTest2;
extern PSZ  pszPostscriptTest3;


/* "Computer" strings */

/* Length of longest string in the "Computer" titles */
#define MAX_COMPUTER_TITLE_LENGTH  19

extern PSZ  paszComputerTitles[];

#define COMP_NAME             0
#define COMP_BIOS_MFGR        1
#define COMP_BIOS_VERSION_1   2
#define COMP_BIOS_VERSION_2   3
#define COMP_BIOS_VERSION_3   4
#define COMP_BIOS_CATEGORY    5
#define COMP_BIOS_ID_BYTES    6
#define COMP_BIOS_DATE        7
#define COMP_PROCESSOR        8
#define COMP_COPROCESSOR      9
#define COMP_KEYBOARD        10
#define COMP_BUS_TYPE        11
#define COMP_DMA_CTLRER      12
#define COMP_CASCADE_IRQ2    13
#define COMP_BIOS_DATA_SEG   14

extern PSZ  pszEnhanced;
extern PSZ  pszNonEnhanced;

extern PSZ  pszXtBus;
extern PSZ  pszAtBus;
extern PSZ  pszMicroChannel;
extern PSZ  pszEisaBus;

#define XT_BUS         1
#define AT_BUS         2
#define MICRO_CHANNEL  3
#define EISA_BUS       4

extern PSZ  pszIntel8088;
extern PSZ  pszIntel80188;
extern PSZ  pszIntel80186;
extern PSZ  pszIntel80286;
extern PSZ  pszIntel80386;
extern PSZ  pszNECV20;
extern PSZ  pszIntel8087;
extern PSZ  pszIntel80287;
extern PSZ  pszIntel80387;
extern PSZ  pszInternal;
extern PSZ  pszInteli486;
extern PSZ  pszIntel486SX;

#define _8088          0
#define _8086          1
#define _80188         2
#define _80186         3
#define _80286         4
#define _80386         5
#define _NECV20        6
#define _NECV30        7
#define _NOCOPROCESSOR 8
#define _INTERNAL      9
#define _8087         10
#define _80287        11
#define _80387        12
#define _80486        13
#define _80486SX      14

#define COMPUTER_CLASS_XT   1
#define COMPUTER_CLASS_AT   2
#define COMPUTER_CLASS_PS2  3


/* Memory Titles */

extern PSZ  paszMemoryTitles[];

#define MEM_MAP_WIDTH  35

#define MEM_CONV_ALIGN 11 + MEM_MAP_WIDTH
#define MEM_EXT_ALIGN   7 + MEM_MAP_WIDTH
#define MEM_EMS_ALIGN  18 + MEM_MAP_WIDTH
#define MEM_XMS_ALIGN  21 + MEM_MAP_WIDTH
#define MEM_VCPI_ALIGN 18 + MEM_MAP_WIDTH
#define MEM_DPMI_ALIGN 15 + MEM_MAP_WIDTH
#define MEM_MAX_ALIGN  21 + MEM_MAP_WIDTH

#define MEM_CONV_STRINGS     10
#define MEM_EXT_STRINGS      4
#define MEM_EMS_STRINGS      7
#define MEM_XMS_STRINGS      13
#define MEM_VCPI_STRINGS     6
#define MEM_DPMI_STRINGS     5
#define MEM_LEGEND_STRINGS   2

#define MEM_640K_LIMIT   27


#define MT_CONV_TITLE              0
#define MT_CONV_TOTAL              1
#define MT_CONV_AVAIL              2

#define MT_EXT_TITLE               3
#define MT_EXT_TOTAL               4

#define MT_UMB_TITLE               5
#define MT_UMB_TOTAL               6
#define MT_UMB_TOTAL_FREE          7
#define MT_UMB_LARGEST_FREE        8

#define MT_EMS_TITLE               9
#define MT_EMS_VERSION            10
#define MT_EMS_PAGE_FRAME         11
#define MT_EMS_TOTAL              12
#define MT_EMS_AVAIL              13

#define MT_XMS_TITLE              14
#define MT_XMS_VERSION            15
#define MT_XMS_DRIVER_VER         16
#define MT_XMS_A20_LINE           17
#define MT_XMS_HMA                18
#define MT_XMS_AVAIL              19
#define MT_XMS_LARGEST_AVAIL      20
#define MT_SXMS_AVAIL             21
#define MT_SXMS_LARGEST_AVAIL     22
#define MT_XMS_TOTAL_UMB_AVAIL    23
#define MT_XMS_LARGEST_UMB_AVAIL  24

#define MT_VCPI_TITLE             25
#define MT_VCPI_DETECTED          26
#define MT_VCPI_VERSION           27
#define MT_VCPI_AVAIL             28

#define MT_DPMI_TITLE             29
#define MT_DPMI_DETECTED          30
#define MT_DPMI_VERSION           31


#define MEM_ALL            0
#define MEM_SUMMARY        1
#define MEM_CONVENTIONAL   2
#define MEM_EXTENDED       3
#define MEM_EMS            4
#define MEM_XMS            5
#define MEM_VCPI           6
#define MEM_DPMI           7
#define MEM_VISUAL_MAP     8

extern PSZ  pszNoPageFrame;
extern PSZ  pszEnabled;
extern PSZ  pszNotEnabled;
extern PSZ  pszError;


/* Video strings */

/* Length of longest string in the Video titles */
#define MAX_VIDEO_TITLE_LENGTH  24

extern PSZ  paszVideoTitles[];

#define VID_ADAPTER_TYPE      0
#define VID_NAME              1
#define VID_MODEL             2
#define VID_DISPLAY_TYPE      3
#define VID_MODE              4
#define VID_NMBR_COLUMNS      5
#define VID_NMBR_ROWS         6
#define VID_BIOS_VERSION_1    7
#define VID_BIOS_VERSION_2    8
#define VID_BIOS_VERSION_3    9
#define VID_BIOS_DATE        10
#define VID_VESA_COMPAT      11
#define VID_VESA_VERSION     12
#define VID_VESA_OEM         13
#define VID_2NDARY_ADAPTER   14
#define VID_TIGA_VERSION     15
#define VID_TIGA_INT         16


/* Network Types */

extern PSZ  paszNetworkTypes[];

#define NET_NO_NETWORK                0
#define NET_UNKNOWN_NET               1
#define NET_MS_NET_COMPATIBLE         2
#define NET_LANMAN                    3
#define NET_LANMAN_BASIC              4
#define NET_LANMAN_ENHANCED           5
#define NET_NOVELL                    6
#define NET_BANYAN                    7
#define NET_LANTASTIC                 8
#define NET_PC_NFS                    9
#define NET_WORKGROUP_CLIENT         10

#define MAX_NETWORK_TYPE             40


extern PSZ  paszNetworkTitles[];

#define NET_NETWORK_DETECTED       0
#define NET_NETWORK_NAME           1
#define NET_VERSION                2
#define NET_MSNET                  3
#define NET_COMPUTER_NAME          4
#define NET_NETBIOS                5
#define NET_NETBIOS_ADDRESS        6

#define NET_LANMAN_ROOT            7
#define NET_LANMAN_USER_NAME       8
#define NET_LANMAN_DOMAIN          9
#define NET_LANMAN_SERVER         10
#define NET_LANMAN_MAILSLOT       11
#define NET_LANMAN_API            12
#define NET_LANMAN_DATE           13
#define NET_LANMAN_PATCH_LEVEL    14

#define NET_LANMAN_NET_CARD       15
#define NET_LANMAN_WKSTA_SVS      16
#define NET_LANMAN_PROTOCOLS      17
#define NET_LANMAN_INI            18

#define NET_NOVELL_SHELL_VER      19
#define NET_NOVELL_SHELL_TYPE     20
#define NET_NOVELL_SHELL_OS       21
#define NET_NOVELL_SHELL_OS_VER   22
#define NET_NOVELL_HDW_TYPE       23
#define NET_NOVELL_STATION_NMBR   24
#define NET_NOVELL_PHYSICAL_NMBR  25
#define NET_NOVELL_IPX            26
#define NET_NOVELL_SPX            27
#define NET_NOVELL_ODI_LSL        28

#define NET_LANTASTIC_SERVER      29
#define NET_LANTASTIC_REDIR       30
#define NET_LANTASTIC_POPUP       31

extern PSZ pszSupported;
extern PSZ pszNotSupported;


/* Operating System Version strings */

extern PSZ  paszOsVersionTitles[];

#define MAX_OS_VERSION_LINE_LEN  64

#define OS_VERSION            0
#define OS_INTERNAL_REV       1
#define OS_OEM_SERIAL         2
#define OS_USER_SERIAL        3
#define OS_OEM_VERSION        4
#define OS_DOS_LOCATION       5
#define OS_BOOT_DRIVE         6
#define OS_DOSSHELL           7
#define OS_WINDOWS            8
#define OS_DESQVIEW           9
#define OS_3270              10
#define OS_DOUBLE_DOS        11
#define OS_TASKVIEW          12
#define OS_TOPVIEW           13
#define OS_PATH_TO_PROGRAM   14
#define MAX_OS_TITLES        16

extern PSZ  pszOs2;
extern PSZ  pszMsDos;
extern PSZ  pszRom;
extern PSZ  pszHma;
extern PSZ  pszConventional;
extern PSZ  pszWin286;
extern PSZ  pszWin386;
extern PSZ  pszWindows;
extern PSZ  pszRealMode;
extern PSZ  pszStandardMode;
extern PSZ  pszEnhancedMode;
extern PSZ  psz_2dotX;
extern PSZ  pszPCControlProgram;
extern PSZ  pszWorkstationProgram;
extern PSZ  pszActive;
extern PSZ  pszDosShell;
extern PSZ  pszEnvironmentStr;
extern PSZ  pszEnvironInvalid;
extern PSZ  pszTemp;
extern PSZ  pszTmp;

extern PSZ  pszPathToProgram;
extern BOOL fTempPathInvalid;
extern BOOL fTmpPathInvalid;


/* Mouse strings */

extern PSZ  paszMouseTitles[];

#define MOU_HARDWARE              0
#define MOU_DRIVER_MFGR           1
#define MOU_DOS_DRIVER_TYPE       2
#define MOU_DRIVER_FILE_TYPE      3
#define MOU_DOS_DRIVER_VERSION    4
#define MOU_MS_DRIVER_VERSION     5
#define MOU_MOUSE_IRQ             6
#define MOU_COM_PORT              7
#define MOU_COM_PORT_ADDRESS      8
#define MOU_NMBR_BUTTONS          9
#define MOU_H_SENSITIVITY        10
#define MOU_H_CURSOR_RATIO       11
#define MOU_V_SENSITIVITY        12
#define MOU_V_CURSOR_RATIO       13
#define MOU_THRESHOLD_SPEED      14
#define MOU_LANGUAGE             15
#define MOU_MOUSE_INI_PATH       16

#define MAX_MOUSE_TITLE          26

#define MAX_MOUSE_RATIO_STRINGS  20

extern PSZ  paszMouseMfgrs[];
extern PSZ  paszMouseTypes[];
extern PSZ  paszMouseLanguages[];
extern PSZ  paszMouseRatios[];
extern PSZ  paszDriverFileTypes[];
WORD FAR _pascal IsMsDos (PSZ);

extern PSZ  pszBallPoint;
extern PSZ  pszDeviceNotDetected;
extern PSZ  pszColonOne;
extern PSZ  pszUndefined;
extern PSZ  pszUnexplainedError;

#define MAX_MOUSE_LINE_LEN    52

#define IBMPS2_MOUSE           4
#define NO_MOUSE_INSTALLED     6
#define LOGITECH_PS2_MOUSE     8
#define MSPS2_MOUSE            9
#define PS2_STYLE_MOUSE       11

#define MOUSE_MFGR_MICROSOFT   1
#define MOUSE_MFGR_PC_MOUSE    2
#define MOUSE_MFGR_LOGITECH    3


/* Other Adapter strings */

extern PSZ paszOtherTitles[];

#define OTHER_GAME_ADAPTER     0
#define OTHER_JOY_A_X          1
#define OTHER_JOY_A_Y          2
#define OTHER_JOY_A_BUTTON_1   3
#define OTHER_JOY_A_BUTTON_2   4
#define OTHER_JOY_B_X          5
#define OTHER_JOY_B_Y          6
#define OTHER_JOY_B_BUTTON_1   7
#define OTHER_JOY_B_BUTTON_2   8
#define OTHER_SOUND_DEVICE     9
#define OTHER_SOUND_IRQ       10
#define OTHER_SOUND_PORT      11

extern PSZ  pszGameAdapter;
extern PSZ  pszDetected;
extern PSZ  pszNotDetected;



/* Disk Drive strings */

#define DISK_DRIVE_COL         2
#define DISK_TYPE_COL          7
#define DISK_EXTRA_INFO_COL   DISK_TYPE_COL + 2
#define DISK_FREE_SPACE_COL   45
#define DISK_TOTAL_FREE_COL   57

#define MAX_DRIVES_PER_LINE    4

extern PSZ  pszDiskHeader;
extern PSZ  pszDiskUnderline;

extern PSZ  pszCylinders;
extern PSZ  pszHeads;
extern PSZ  pszBytesPerSector;
extern PSZ  pszSectorsPerTrack;
extern PSZ  pszCommaSpace;

extern PSZ  pszJoinInstalled;
extern PSZ  pszSubstInstalled;
extern PSZ  pszShareInstalled;
extern PSZ  pszAssignInstalled;
extern PSZ  pszAssign;
extern PSZ  pszAppendInstalled;
extern PSZ  pszAppendPath;
extern PSZ  pszMscdexInstalled;
extern PSZ  pszMscdex1x;
extern PSZ  pszLastdrive;

extern PSZ  pszMscdex;

extern PSZ  paszDriveTypes[];


/* LPT strings */

extern PSZ  pszLptHeader1;
extern PSZ  pszLptHeader2;
extern PSZ  pszLptUnderline;

#define LPT_PORT_COL         0
#define LPT_PORT_ADDR_COL    8
#define LPT_ON_LINE_COL     18
#define LPT_PAPER_OUT_COL   26
#define LPT_I_O_ERROR_COL   34
#define LPT_TIME_OUT_COL    42
#define LPT_BUSY_COL        50
#define LPT_ACK_COL         58


/* COM strings */

extern PSZ  pszComHeader;
extern PSZ  pszComUnderline;
extern PSZ  psz1point5;
extern PSZ  pszNA;
extern PSZ  pszOn;
extern PSZ  pszOff;
extern PSZ  paszComTitles[];
extern PSZ  paszUartChips[];
extern PSZ  paszParityDesc[];

#define MAX_COM_TITLES      10
#define MAX_COM_PORTS        4  /* Maximum number of COM ports detectable */
#define COM_TITLE_WIDTH     21  /* Length of longest title string         */
#define COM_INFO_COL_WIDTH  11  /* Distance between columns               */
#define MAX_COM_INFO_LINE   (COM_TITLE_WIDTH + (COM_INFO_COL_WIDTH * MAX_COM_PORTS))
                                /* Maximum COM info line length           */

#define COM_PORT_ADDRESS     0
#define COM_BAUD_RATE        1
#define COM_PARITY           2
#define COM_DATA_BITS        3
#define COM_STOP_BITS        4
#define COM_CARRIER_DETECT   5
#define COM_RING_INDICATOR   6
#define COM_DATA_SET_READY   7
#define COM_CLEAR_TO_SEND    8
#define COM_UART_CHIP        9


/* IRQ data */

extern WORD rwIrqRecordTypes[];

extern PSZ  paszPcIrqDescriptions[];
extern PSZ  paszAtIrqDescriptions[];

extern PSZ  pszIrqHeader;
extern PSZ  pszIrqUnderline;

#define IRQ_ADDRESS_COL        5
#define IRQ_DESCRIPTION_COL   16
#define IRQ_DETECTED_COL      34
#define IRQ_HANDLED_BY_COL    54

extern PSZ  pszCom[];

extern PSZ  pszBIOS;

/* TSR strings */

extern PSZ  pszTsrHeader;
extern PSZ  pszTsrUnderline;

extern PSZ  pszFreeMemory;
extern PSZ  pszDosSystemArea;
extern PSZ  pszCommandCom;
extern PSZ  pszCommand;
extern PSZ  pszExcludedUmbArea;
extern PSZ  pszVideoRom;
extern PSZ  pszHardDiskRom;
extern PSZ  pszOptionRom;
extern PSZ  pszVideoRam;
extern PSZ  pszDosSystemCode;
extern PSZ  pszDosSystemData;
extern PSZ  pszDeviceAppenage;
extern PSZ  pszFileHandles;
extern PSZ  pszFCBS;
extern PSZ  pszBuffers;
extern PSZ  pszDirectories;
extern PSZ  pszStacksArea;
extern PSZ  pszIntTable;
extern PSZ  pszRomDataArea;
extern PSZ  pszDosDataArea;

#define TSR_ADDRESS_COL     22
#define TSR_SIZE_COL        29
#define TSR_CMD_LINE_COL    37


/* Device driver strings */

extern PSZ  pszDeviceHeader;
extern PSZ  pszDeviceUnderline;

extern PSZ  pszBlockDevice;

#define DEV_FILENAME_COL    14
#define DEV_UNITS_COL       24
#define DEV_HEADER_COL      31
#define DEV_ATTRIBUTE_COL   42


/* File Viewer strings */

extern PSZ  pszDot;
extern PSZ  pszDotDot;

extern PSZ  pszPathNotThere;

/* Global String Arrays */

extern PSZ  paszButtonNames[];

#define MAX_BUTTON_NAME_LEN   14

extern PSZ  paszMainBtnArray[];
extern PSZ  paszCommandLineHelp[];
extern PSZ  paszMsdTitleLines[];
extern PSZ  paszCustInfoTitle[];


/* Summary area */

#define MAX_SUMM_INFO        18
#define SUMMARY_ALIGN  MAX_BUTTON_NAME_LEN + 2


/********************************************************************/
/*                         Data Structures                          */
/********************************************************************/

/* MSD Header Structure */

typedef struct _MSD_HEADER
{
  CHAR chMsdVersion[15];  /* MSD Version number 2.00.00xx */
} MSD_HEADER;


#define MAX_CUST_INFO_TITLES  8
#define MAX_CUST_INFO        50

/* Customer data array */

typedef struct _CUSTINFO
{
  CHAR chCustInfo[MAX_CUST_INFO_TITLES][MAX_CUST_INFO + 1];
} CUSTINFO;

#if CW_INCLUDED
extern CUSTINFO *pCustInfoDlg;  /* Customer information for the dialog box */
#endif


/* Summary structure */

typedef struct _SUMMARY_STRUCT
{
  /* Storage for the summary info strings */

  CHAR szSumStrings[NMBR_OF_RECORDS * 2][MAX_SUMM_INFO + 5];
} SUMMARY_STRUCT;

extern SUMMARY_STRUCT  *pSum;


/* Computer structure */

#define MAX_BIOS_MFGR             25
#define MAX_COMPUTER_NAME         25
#define MAX_BIOS_VERSION_LEN      50
#define MAX_BIOS_VERSION_STRINGS   3
#define MAX_BIOS_DATE              9
#define MAX_BIOS_CATEGORY         32
#define MAX_PROCESSOR             18
#define MAX_KEYBOARD_TYPE         15
#define MAX_BUS_TYPE              15

typedef struct _COMPUTER_STRUCT
{
  WORD wComputerName;                     /* Computer Name (numeric)       */
  CHAR szComputerName[MAX_COMPUTER_NAME]; /* Computer Name (string)        */
  WORD wBiosMfgr;                         /* BIOS mfgr's name (numeric)    */
  CHAR szBiosMfgr[MAX_BIOS_MFGR];         /* BIOS mfgr's name (string)     */
  CHAR aszBiosVersion[MAX_BIOS_VERSION_STRINGS][MAX_BIOS_VERSION_LEN];
                                          /* BIOS version                  */
  WORD wBiosCategory;                     /* BIOS Category (numeric)       */
  CHAR szBiosCategory[MAX_BIOS_CATEGORY]; /* BIOS Category (string)        */
  WORD wComputerClass;                    /* XT - AT - PS/2 classification */
  BOOL fConfigCallSupported;              /* TRUE if config call supported */
  BYTE bComputerType;                     /* Computer type byte            */
  BYTE bSubModel;                         /* Computer Submodel             */
  BYTE bRevisionLevel;                    /* BIOS revision level           */
  CHAR szBiosDate[MAX_BIOS_DATE];         /* BIOS date                     */
  WORD wProcessor;                        /* Type of processor (numeric)   */
  CHAR szProcessor[MAX_PROCESSOR];        /* Type of processor (string)    */
  WORD wCoProcessor;                      /* Type of coprocessor (numeric) */
  CHAR szCoProcessor[MAX_PROCESSOR];      /* Type of coprocessor (string)  */
  WORD wKeyboardType;                     /* Keyboard type (numeric)       */
  CHAR szKeyboardType[MAX_KEYBOARD_TYPE]; /* Keyboard type (string)        */
  WORD wBusType;                          /* Type of BUS (numeric)         */
  CHAR szBusType[MAX_BUS_TYPE];           /* Type of BUS (string)          */
  BOOL fFixedDiskUsesDMA3;                /* Fixed Disk BIOS uses DMA #3   */
  BOOL fCascadeIntLvl2;                   /* Cascaded interrupt 2 (IRQ2)   */
  BOOL fRealTimeClock;                    /* TRUE: real time clock present */
  BOOL fDmaChipPresent;                   /* TRUE if DMA chip is present   */
  WORD wExtAreaSeg;                       /* Segment of extended BIOS area */
  WORD wExtAreaLen;                       /* Length of extended BIOS area  */
} COMPUTER_STRUCT;


/* Address - Length structure: Used in obtaining computer names */

typedef struct _ADDR_LENGTH
{
  DWORD dwAddress;  /* Address to search */
  WORD  wLength;    /* Length of search  */
} ADDR_LENGTH;

#define GET_COMPUTER_NAME   1
#define GET_BIOS_MFGR       2
#define GET_VIDEO_NAME      3
#define GET_VIDEO_MODEL     4


/* Memory structures */

#define ZERO_SEGMENT  0x0000  /* The first segment of memory                       */
#define SEGMENT_INC   0x0400  /* The amount to increment the segment address by    */
#define START_OFFSET  0x0000  /* The starting offset within the current segment    */
#define ENDING_OFFSET 0x4000  /* The ending offset within the current segment      */
#define OFFSET_INC    0x0400  /* The amount to increment the offset by             */
#define TEST_VALUE1   0x4A    /* The first test value used to test for RAM         */
#define TEST_VALUE2   0x4B    /* The second test value used to test for RAM        */
#define NUM_OF_ROWS   64      /* The number of rows in the memory map              */
#define NUM_OF_COLS   17      /* The number of columns in the memory map plus      */
                              /* to hold an ending null char for winWrtStrnAttrib  */

#define RAM 0                 /* Value returned by RamRomWomCheck for RAM          */
#define ROM 1                 /* Value returned by RamRomWomCheck for ROM          */
#define WOM 2                 /* Value returned by RamRomWomCheck for WOM          */
#define SEARCH_LENGTH 1024    /* The number of bytes for RamRomWomCheck to check   */

#define SIXTEEN_KB   0        /* Specify a memory browse on a 16K block            */
#define SIXTYFOUR_KB 1        /* Specify a memory browse on a 64K block            */
#define FIRST_MB     2        /* Specify a memory browse on the first meg          */

#define DISPLAY_RAM             176
#define DISPLAY_ROM             219
#define DISPLAY_WOM             ' '
#define DISPLAY_EMS             'P'
#define DISPLAY_USED_UMB        'U'
#define DISPLAY_FREE_UMB        'F'
#define DISPLAY_FREE_XMS_UMB    'X'
#define DISPLAY_NOT_CERTAIN     250
#define REPORT_RAM              '#'
#define REPORT_ROM              'R'
#define REPORT_WOM              ' '
#define REPORT_EMS              'P'
#define REPORT_USED_UMB         'U'
#define REPORT_FREE_UMB         'F'
#define REPORT_FREE_XMS_UMB     'X'
#define REPORT_NOT_CERTAIN      '.'


typedef struct _DPMI_STRUCT
{
  unsigned long ulLrgBlkFreeBy;    /* Largest available block in bytes                */
  long          lMaxUnLockPg;      /* Maximum unlocked page allocation                */
  unsigned long ulMaxLockPg;       /* Maximum locked page allocation                  */
  unsigned long ulTotLinPg;        /* Total linear address space in pages             */
  unsigned long ulTotUnLockPg;     /* Total number of unlocked pages                  */
  unsigned long ulTotPhysFreePg;   /* Total number of free physical pages             */
  unsigned long ulTotPhysPg;       /* Total number of physical pages                  */
  unsigned long ulTotLinFreePg;    /* Total number of free linear pages               */
  unsigned long ulSizeFilePg;      /* Size of paging file/partition in pages          */
  unsigned long ulReserved[3];     /* Reserved                                        */

} DPMI_STRUCT;

typedef struct _MEMORY_STRUCT
{

/* Conventional and extended memory information */

  long lConv_Mem;      /* Amount of conventional RAM, bytes                */
  long lFree_Conv_Mem; /* Amount of free conventional memory, bytes        */
  int  iCMOSExtended;  /* Amount of CMOS installed extended memory, Kbytes */
  int  iExt_Mem;       /* Amount of extended RAM, Kbytes                   */

/* Expanded (EMS) information */

  int iEmm_Is_There;       /* EMM driver present. 1 = Yes, 0 = No                                  */
  int iEmm_Ver_Err;        /* Error getting EMM version, 0 = no, <> 0 = error code                 */
  int iEmm_Size_Err;       /* Error getting EM size, 0 = No, <> 0 = error code                     */
  int iEmm_VersionMajor;   /* Major version of the EMM driver                                      */
  int iEmm_VersionMinor;   /* Minor version of the EMM driver                                      */
  int iTotal_Emm_Pages;    /* Number of 16K expanded memory pages                                  */
  int iFree_Emm_Pages;     /* Number of free 16K expanded memory pages                             */
  int iPageFrameAddress;   /* The segment address of the EMS page frame                            */
  int iLIM40Functionality; /* Whether or not LIM 4.0 mappable pages were detected. 1 = yes, 0 = no */

/* XMS memory information */

  int iXmm_Is_There;      /* XMM driver present, 1 = Yes, 0 = No                                          */
  int iLargest_Free_Xm;   /* Largest free extended memory block bytes                                     */
  int iTotal_Free_Xm;     /* Total free extended memory blocks, bytes                                     */
  int iXmm_Free_Err;      /* Error getting largest and/or total free XM blocks, 0 = no, <> 0 = error code */

  unsigned char uchXmm_Spec_VersionMajor;   /* Major version of the XMM spec being used */
                                            /*  stored as a hex value                   */
  unsigned char uchXmm_Spec_VersionMinor;   /* Minor version of the XMM spec being used */
                                            /*  stored as a hex value                   */
  unsigned char uchXmm_Driver_VersionMajor; /* Major version of the XMM driver          */
                                            /*  stored as a hex value                   */
  unsigned char uchXmm_Driver_VersionMinor; /* Minor version of the XMM driver          */
                                            /*  stored as a hex value                   */

  int iA20Status;     /* The status of the A20 address line, 1 = enabled, 0 = disabled  */
  int iHMAStatus;     /* The status of the HMA. 1 = avaialble, 0 = not available        */
  int iXMSError;      /* Error codes returned when requesting the HMA                   */

/* SXMS memory information (compliments XMS memory information) */

  BOOL  fSxmsAvailable;     /* TRUE if Super Extended memory available */
  DWORD dwSxmsLargestFree;  /* Largest free block in Kb                */
  DWORD dwSxmsTotalFree;    /* Total SXMS free in Kb                   */
  DWORD dwSxmsHighestAddr;  /* Highest ending address of any block     */

/* XMS UMB information */

  BOOL  fXmsUmbAvailable;     /* TRUE if XMS UMBs are available */
  DWORD dwXmsUmbFree;         /* Total bytes free in XMS UMBs   */
  DWORD dwXmsUmbLargestFree;  /* Largest free block of XMS UMBs */

  /* Error Code   Meaning                                 */
  /* ----------   -------                                 */
  /*   0      No errors detected                          */
  /*   1      HMA request feature not implemented         */
  /*   2      VDISK was detected                          */
  /*   3      HMA does not exist                          */
  /*   4      HMA already in use                          */
  /*   5      Unknown reason                              */
  /*   6      Unable to release HMA (Used when releasing) */

/* VCPI Information */

  int iVCPIPresent;       /* Whether or not VCPI is present, 1 = Yes, 0 = No */
  int iVCPIMajorVersion;  /* Major version of VCPI driver                    */
  int iVCPIMinorVersion;  /* Minor version of VCPI driver                    */
  int iVCPIAvailMemory;   /* Available VCPI memory, Kbytes                   */

/*DPMI Information */

  int iDPMIPresent;          /* Whether or not DPMI is present, 1 = Yes, 0 = No */
  int iDPMIMajorVersion;     /* Major version of DPMI driver                    */
  int iDPMIMinorVersion;     /* Minor version of DPMI driver                    */
  unsigned long ulRealModeDPMIMemAvail; /* The amount of real mode DPMI memory avaialable  */

  DPMI_STRUCT DPMIMemInfo;   /* Structure for holding DPMI info */

  WORD  wOptionRomAddress[10];/* Option ROM Starting Addresses */
  DWORD dwOptionRomLength[10];/* Option ROM lengths            */
  WORD  wBiosStartAddress;    /* System BIOS start address     */
  DWORD dwBiosLength;         /* System BIOS length            */

/* UMB Information */

  BOOL  fUmbsAvailable;       /* TRUE if UMBs are available    */
  DWORD dwTotalUmbs;          /* Total UMB areas in the system */
  DWORD dwFreeUmbs;           /* Total free UMB space          */
  DWORD dwLargestFreeUmb;     /* Largest single free UMB area  */

/* The arrays to hold the visual memory map */

  BYTE abMemoryMap [NUM_OF_ROWS][NUM_OF_COLS];
  BYTE abMemMapOverlay [NUM_OF_ROWS][NUM_OF_COLS];

} MEMORY_STRUCT;


typedef struct _ROM_MAP
{
/* ROM locations and lengths */

  WORD  wRomBiosLoc[2];       /* ROM BIOS locations                 */
  DWORD dwRomBiosSize[2];     /* ROM BIOS sizes                     */
                              /*   I've arbitrarily split PS/2 ROMs */
                              /*   into E000 and F000 segments      */

  WORD  wOptRomLoc[10];       /* Option ROM locations               */
  DWORD dwOptRomSize[10];     /* Option ROM sizes                   */
} ROM_MAP;


typedef struct _MEM_MAP
{
  BYTE abMap[NUM_OF_ROWS][NUM_OF_COLS + 5];
} MEM_MAP;


/* Video structure */

#define MAX_ADAPTER         32
#define MAX_ADAPTER_NAME    32
#define MAX_ADAPTER_MODEL   32
#define MAX_DISPLAY_TYPE    32
#define MAX_VESA_OEM_NAME   50

typedef struct _VIDEO_STRUCT
{
                                            /* Primary display information */
  BYTE bSubsystem0;                         /* Video adapter type          */
  BYTE bDisplay0;                           /* Display type (TTL, color)   */
  BYTE bMode0;                              /* Video mode number           */
  BYTE bNmbrCols0;                          /* Number of columns (40/80)   */
  WORD wMemory0;                            /* Video memory installed      */

                                            /* Secondary display data      */
  BYTE bSubsystem1;                         /* Video adapter type          */
  BYTE bDisplay1;                           /* Display type (TTL, color)   */
  BYTE bMode1;                              /* Video mode number           */
  BYTE bNmbrCols1;                          /* Number of columns (40/80)   */
  WORD wMemory1;                            /* Video memory installed      */

  BYTE bNmbrRows;                           /* # of rows (25/43/50)        */
  CHAR szAdapterType[MAX_ADAPTER];          /* Adapter type (string)       */
  WORD wAdapterName;                        /* Adapter name (numeric)      */
  CHAR szAdapterName[MAX_ADAPTER_NAME];     /* Adapter name (string)       */
  WORD wAdapterModel;                       /* Adapter model (numeric)     */
  CHAR szAdapterModel[MAX_ADAPTER_MODEL];   /* Adapter model (string)      */
  CHAR szDisplayType[MAX_DISPLAY_TYPE];     /* Display type (string)       */
  CHAR sz2ndAdapterType[MAX_ADAPTER];       /* 2ndary adapter type (str)   */

  CHAR aszVideoBiosVersion[MAX_BIOS_VERSION_STRINGS][MAX_BIOS_VERSION_LEN];
  CHAR szVideoBiosDate[MAX_BIOS_DATE];      /* Video BIOS date             */
                                            /* Video BIOS version          */
  BYTE bVesaVersionMajor;                   /* VESA version number (major) */
  BYTE bVesaVersionMinor;                   /* VESA version number (minor) */
  CHAR szVesaOemName[MAX_VESA_OEM_NAME];    /* VESA OEM Name               */
  WORD wTigaInterrupt;                      /* TIGA s/w interrupt number   */
  DWORD dwTigaIntAddress;                   /* TIGA interrupt address      */
  WORD wTigaMajor;                          /* TIGA major version number   */
  WORD wTigaMinor;                          /* TIGA minor version number   */
  BOOL fTigaSignatureFound;                 /* TRUE if "TIGA" found        */
} VIDEO_STRUCT;


/* Network Structure */

#define MAX_MSNET_MACHINE_NAME  16 + 1
#define NUMBER_OF_CARDS         8
#define NUMBER_OF_SESSIONS     16
#define LENGTH_OF_UNIT_ID       6

typedef struct _SESSION_NAME
  {
    CHAR szSessionName[16];
  } SESSION_NAME;

typedef struct _NET_CARD_SESSION
  {
    BYTE bUnitID_Number[LENGTH_OF_UNIT_ID];
    BYTE bNumber_Of_Sessions;
    SESSION_NAME snRemoteSessionNames[NUMBER_OF_SESSIONS];
  } NET_CARD_SESSION;

typedef struct _NETWORK_STRUCT
{
  /*** General Network Information *************************************/

  BOOL  fNetworkActive;           /* TRUE if a network is active        */
  WORD  wNetworkType;             /* Type of network installed          */
  CHAR  szNetworkType[MAX_NETWORK_TYPE];  /* Type of network (string)   */
  WORD  wNetworkMajor;            /* Version number of network          */
  WORD  wNetworkMinor;

  /*** MS-NET Information ***********************************************/

  BOOL  fMsnetCompatible;         /* TRUE if MS-NET compatible network  */
  CHAR  szMsnetMachineName[MAX_MSNET_MACHINE_NAME + 1]; /* Machine's name */

  /*** NetBIOS Information **********************************************/

  BOOL  fNetBiosCompatible;       /* TRUE if NetBIOS compatible network */
  WORD  wNetBIOSSegment;          /* Segment address of NetBIOS handler */
  WORD  wNetBIOSOffset;           /* Offset of NetBIOS handler          */

  /*** LANMAN Information ***********************************************/

  BOOL  fMailslot_Support;        /* TRUE if supported */
  BOOL  fAPI_Support;             /* TRUE if supported */
  BOOL  fServerConnection;        /* TRUE if connection to a server has been established, 0 otherwise */
  CHAR  szLanRoot[_MAX_PATH];
  CHAR  szUserName[16];
  CHAR  szPrimaryDomain[16];
  CHAR  szLanManager_Date[16];
  CHAR  szLanManager_CSD[7];
  WORD  wNumberOfNets;
  NET_CARD_SESSION ncsNets[NUMBER_OF_CARDS];

  /*** Novell Information ***********************************************/

  WORD  wShellMajor;              /* Version number of shell (numeric) */
  WORD  wShellMinor;
  WORD  wShellRevision;
  CHAR  szNovellShellOsVersion[40];/* Version number of shell OS (string) */
  WORD  wShellType;               /* XMS, EMS, Conventional memory     */

  CHAR  szNovellNetwareVersion[40];

  WORD  wNovellShellOs;           /* 00=MS-DOS                         */
  CHAR  szNovellShellOs[40];
  WORD  wNovellHdwType;           /* 00=IBM_PC 01=Victor9000           */
  CHAR  szNovellHdwType[40];
  WORD  wStationNmbr;             /* Station number (digit)            */
  WORD  wPhysicalStaNmbr1;        /* Physical station number part 1    */
  WORD  wPhysicalStaNmbr2;        /* Physical station number part 2    */
  WORD  wPhysicalStaNmbr3;        /* Physical station number part 3    */
  BOOL  fIpxInstalled;            /* TRUE if IPX installed             */
  BOOL  fSpxInstalled;            /* TRUE if SPX installed             */
  BOOL  fOdiLslInstalled;         /* TRUE if ODI link support layer in */

  /*** LANtastic Information ********************************************/

  BOOL fLANtasticPresent;         /* TRUE if LANtastic is running   */
  BOOL fLANtasticServer;          /* TRUE if running as a server    */
  BOOL fLANtasticRedir;           /* TRUE if running redirector     */
  BOOL fLANtasticPopUp;           /* TRUE if running popup software */
  WORD wLANtasticVersionMajor;    /* The major version number       */
  WORD wLANtasticVersionMinor;    /* The minor version number       */

} NETWORK_STRUCT;


/* OS Version structure */

#define MAX_OEM_VER          80
#define MAX_OEM_VER_STRINGS   5
#define MAX_PATH_TO_PROGRAM  _MAX_PATH + 1

typedef struct _OS_VERSION_STRUCT
{
  WORD  wDosMajor;               /* DOS major version                      */
  WORD  wDosMinor;               /* DOS minor version                      */
  WORD  wDosRevision;            /* DOS revision                           */
  WORD  wOemSerialNumber;        /* OEM serial number                      */
  DWORD dwUserSerialNumber;      /* User serial number                     */
  WORD  wDosLoadedFlags;         /* 0=Conventional,8=ROM,16=HMA            */
  CHAR  chDosBootDrive;          /* Boot drive                             */
  BOOL  fOs2Installed;           /* TRUE if OS/2 is running                */
  BOOL  fDosShellTaskSwitcher;   /* TRUE if DOSSHELL task switcher in use  */
  WORD  wWindowsType;            /* Type of Windows running                */
  WORD  wWindowsMajor;           /* Windows major for Win >= 3.0           */
  WORD  wWindowsMinor;           /* Windows minor for Win >= 3.0           */
  WORD  wDesqViewMajor;          /* DESQview major version                 */
  WORD  wDesqViewMinor;          /* DESQview minor version                 */
  WORD  w3270Installed;          /* 0=No, 1=Control Pgm, 2=Workstation Pgm */
  WORD  w3270Major;              /* 3270 program major version             */
  WORD  w3270Minor;              /* 3270 program minor version             */
  BOOL  fDoubleDosInstalled;     /* TRUE if DoubleDOS installed            */
  BOOL  fTaskViewInstalled;      /* TRUE if TaskView installed             */
  WORD  wTopViewMajor;           /* TopView major version                  */
  WORD  wTopViewMinor;           /* TopView minor version                  */
  CHAR  szPathToProgram[MAX_PATH_TO_PROGRAM];
                                 /* Fully qualified path to program        */
  CHAR  szOemVer[MAX_OEM_VER_STRINGS][MAX_OEM_VER];
                                 /* Stores the OEM version number strings  */
} OS_VERSION_STRUCT;

#define NO_WINDOWS          0
#define WIN_286             1
#define WIN_386             2
#define WIN_REAL_MODE       3
#define WIN_STANDARD_MODE   4
#define WIN_ENHANCED_MODE   5
#define WIN_UNKNOWN_MODE    6

#define NO_3270_PROGRAM            0
#define _3270_PC_CONTROL_PROGRAM   1
#define _3270_WORKSTATION_PROGRAM  2


/* Mouse structure */

#define MAX_MOUSE_HARDWARE_TYPE      32
#define MAX_MOUSE_DRIVER_MFGR        32
#define MAX_MOUSE_DRIVER_TYPE        32
#define MAX_COM_PORT_LEN             10
#define MAX_MOUSE_LANGUAGE           11
#define MAX_DRIVER_FILE_TYPE         10
#define MAX_MOUSE_INI_PATH           _MAX_PATH + 1

typedef struct _MOUSE_STRUCT
{
  BOOL  fHardwareInstalled;   /* TRUE if mouse device is detected       */
  WORD  wMouseHardwareType;   /* Mouse hardware detected (numeric)      */
  CHAR  szMouseHardwareType[MAX_MOUSE_HARDWARE_TYPE];
                              /* Mouse hardware detected (string)       */
  WORD  wDriverMfgr;          /* Manufacturer of mouse driver (numeric) */
  CHAR  szDriverMfgr[MAX_MOUSE_DRIVER_MFGR];
                              /* Manufacturer of mouse driver (string)  */
  WORD  wMouseDriverType;     /* Driver detected mouse (Inport/Bus/etc) */
  CHAR  szMouseDriverType[MAX_MOUSE_DRIVER_TYPE];
                              /* Driver detected mouse (string)         */
  WORD  wMsMajorVersion;      /* Mouse driver major version             */
  WORD  wMsMinorVersion;      /* Mouse driver minor version             */
  WORD  wOemMajorVersion;     /* OEM's Mouse driver version (major)     */
  WORD  wOemMinorVersion;     /* OEM's Mouse driver version (minor)     */
  WORD  wIrq;                 /* IRQ used by the mouse                  */
  WORD  wComPort;             /* COM port used by the mouse (numeric)   */
  CHAR  szComPort[MAX_COM_PORT_LEN]; /* COM port use by mouse (string)  */
  WORD  wComPortAddress;      /* COM port's port address                */
  WORD  wNmbrButtons;         /* Number of buttons on mouse             */
  WORD  wHMickeys;            /* Horizontal mickeys                     */
  WORD  wVMickeys;            /* Vertical mickeys                       */
  WORD  wThresholdSpeed;      /* Doublespeed threshold                  */
  WORD  wLanguage;            /* Language for mouse messages (numeric)  */
  CHAR  szLanguage[MAX_MOUSE_LANGUAGE];
                              /* Language for mouse messages (string)   */
  WORD  wDriverFileType;      /* 0=Call not supported, 1=.COM, 2=.SYS   */
  CHAR  szDriverFileType[MAX_DRIVER_FILE_TYPE];
                              /* Driver file type (string)              */
  CHAR  szMouseIniPath[MAX_MOUSE_INI_PATH];
                              /* Fully qualified path to MOUSE.INI      */
} MOUSE_STRUCT;

#define MOUSE_NO_MOUSE_DRIVER    0
#define MOUSE_MICROSOFT_DRIVER   1
#define MOUSE_LOGITECH_DRIVER    2
#define MOUSE_PC_MOUSE_DRIVER    3

#define MOUSE_UNKNOWN_FILE       0
#define MOUSE_COM_FILE           1
#define MOUSE_SYS_FILE           2

#define SERIAL_MOUSE             2
#define LOGITECH_SERIAL_MOUSE    7
#define BALLPOINT_MOUSE         80


/* Other Adapters Information Struct */

#define MAX_SOUND_NAME          40

typedef struct _OTHER_STRUCT
{
  BOOL fGameInstalled;    /* TRUE if game adapter installed  */
  WORD wJoystickAX;       /* "A" Joystick's X value          */
  WORD wJoystickAY;       /* "A" Joystick's Y value          */
  WORD wJoystickBX;       /* "B" Joystick's X value          */
  WORD wJoystickBY;       /* "B" Joystick's Y value          */
  BOOL fButtonA1;         /* "A" Joystick's 1st button value */
  BOOL fButtonA2;         /* "A" Joystick's 2st button value */
  BOOL fButtonB1;         /* "B" Joystick's 1st button value */
  BOOL fButtonB2;         /* "B" Joystick's 2st button value */

  BOOL fSoundInstalled;   /* TRUE if sound adapter installed */
  WORD wSoundName;        /* Sound board (numeric)           */
  CHAR szSoundName[MAX_SOUND_NAME]; /* Sound board (string)  */
  WORD wSoundPort;        /* I/O port used by sound adapter  */
  WORD wSoundIrq;         /* IRQ used by sound adapter       */
  WORD wSoundDma;         /* DMA channel used by sound adp.  */
} OTHER_STRUCT;


/* Disk Drive Information Struct */

#define DISK_UNKNOWN_DRIVE             0
#define DISK_FLOPPY_DRIVE              1
#define DISK_525_360K                  2
#define DISK_525_12M                   3
#define DISK_35_720K                   4
#define DISK_SINGLE_DENSITY_8_INCH     5
#define DISK_DOUBLE_DENSITY_8_INCH     6
#define DISK_FIXED_DISK                7
#define DISK_TAPE_DRIVE                8
#define DISK_35_144M                   9
#define DISK_OPTICAL_DISK             10
#define DISK_35_288M                  11
#define DISK_REMOTE_DRIVE             12
#define DISK_RAM_DISK                 13
#define DISK_CD_ROM_DRIVE             14
#define DISK_SUBST_DRIVE              15
#define DISK_ASSIGN_DRIVE             16

#define MAX_DRIVE_LETTERS     33
#define MAX_DRIVE_TYPE_LEN    32
#define MAX_ASSIGN_TABLE      26
#define MAX_APPEND_PATH       _MAX_PATH + 1

typedef struct _SINGLE_DRIVE_INFO
{
  CHAR  chDriveLetter;        /* Drive letter for this drive              */
  WORD  wDriveType;           /* Drive type (360k floppy, 1.2M floppy ... */
  CHAR  szDriveType[MAX_DRIVE_TYPE_LEN];  /* Drive type (string)          */
  DWORD dwFreeBytes;          /* Free bytes on this disk                  */
  DWORD dwTotalBytes;         /* Size of disk                             */
  WORD  wCylinders;           /* Number of cylinders                      */
  WORD  wHeads;               /* Number of heads                          */
  WORD  wBytesPerSector;      /* Bytes per sector                         */
  WORD  wSectorsPerTrack;     /* Sectors per track                        */
  WORD  wCmosDriveType;       /* The CMOS drive type (ie, 02, 48, etc).   */
  WORD  wCmosCylinders;       /* CMOS reported number of cylinders        */
  WORD  wCmosHeads;           /* CMOS reported number of heads            */
  WORD  wCmosSectorsPerTrack; /* CMOS reported sectors per track          */
  BOOL  fBiosSupported;       /* TRUE if this is a BIOS supported drive   */
} SINGLE_DRIVE_INFO;

typedef struct _ASSIGN_TABLE
{
  CHAR chAssignTo;            /* ASSIGN table ie, C=D, X=F, etc           */
  CHAR chAssignFrom;          /*   C is to, D is from                     */
} ASSIGN_TABLE;

typedef struct _DISK_STRUCT
{
  WORD wNmbrDrives;           /* Total number of drives in the system */
  CHAR chLastDrive;           /* Highest available drive letter       */
  CHAR chCurrentDrive;        /* Current drive letter                 */
  BOOL fJoinInstalled;        /* TRUE if JOIN.EXE is installed        */
  BOOL fSubstInstalled;       /* TRUE if SUBST.EXE is installed       */
  BOOL fShareInstalled;       /* TRUE if SHARE.EXE is installed       */
  BOOL fAssignInstalled;      /* TRUE if ASSIGN.EXE is installed      */
  ASSIGN_TABLE atAssignTable[MAX_ASSIGN_TABLE];  /* ASSIGNed drives   */
  BOOL fAppendInstalled;      /* TRUE if APPEND.EXE is installed      */
  CHAR szAppendPath[MAX_APPEND_PATH]; /* APPEND path                  */
  WORD wMscdexMajor;          /* Major version of MSCDEX.EXE          */
  WORD wMscdexMinor;          /* Minor version of MSCDEX.EXE          */
  SINGLE_DRIVE_INFO asdi[MAX_DRIVE_LETTERS];  /* Drive information    */
} DISK_STRUCT;

typedef struct _CMOS_DRIVE_TYPE
{
  WORD wNmbrCylinders;       /* 00h */
  BYTE bNmbrHeads;           /* 02h */
  WORD wReducedWriteCyl;     /* 03h */
  WORD wWritePrecomp;        /* 05h */
  BYTE bMaxEccBurstLength;   /* 07h */
  BYTE bControlByte;         /* 08h */
  BYTE bStandardTimeout;     /* 09h */
  BYTE bFormattingTimeout;   /* 0Ah */
  BYTE bDriveCheckTimeout;   /* 0Bh */
  WORD wLandingZone;         /* 0Ch */
  BYTE bSectorsPerTrack;     /* 0Eh */
  BYTE bReserved;            /* 0Fh */
} CMOS_DRIVE_TYPE;


/* LPT Information Structure */

#define MAX_LPT_PORTS       3

typedef struct _LPT_X_STRUCT
{
  BOOL fLptPortDetected;  /* TRUE if parallel port detected  */
  WORD wPortAddress;      /* Port address for LPT port       */
  BOOL fOnLine;           /* TRUE if printer on-line         */
  BOOL fPaperOut;         /* TRUE if paper out sensor is on  */
  BOOL fIoError;          /* TRUE if I/O error occured       */
  BOOL fTimeOut;          /* TRUE if a timeout error occured */
  BOOL fBusy;             /* TRUE if port is busy            */
  BOOL fAck;              /* TRUE if ACK line on             */
} LPT_X_STRUCT;

typedef struct _LPT_STRUCT
{
  WORD wNmbrLptPorts;                     /* Total count of LPT ports */
  LPT_X_STRUCT LptxInfo[MAX_LPT_PORTS];   /* Individual LPT port data */
} LPT_STRUCT;


/* COM Information Structure */

/* The serial port information I received is primarily from the MS-DOS */
/*   Encyclopedia, Article 6, starting on page 167.  The information   */
/*   about the 8250 UART chip begins on page 172.                      */

#define MAX_UART_CHIP   8
#define MAX_PARITY      6

typedef struct _COM_X_STRUCT
{
  BOOL  fComPortDetected; /* TRUE if serial port detected          */
  WORD  wPortAddress;     /* Port address for the serial port      */
  DWORD dwBaudRate;       /* Actual Baud Rate                      */
  WORD  wParity;          /* Parity 0=N 1=O 2=E 3=Mark 4=Space     */
  CHAR  szParity[MAX_PARITY];   /* Parity (string)                 */
  WORD  wDataBits;        /* Data bits                             */
  WORD  wStopBits;        /* Stop bits 1=1 2=2 3=1.5               */
  BOOL  fCarrierDetect;   /* Carrier Detect                        */
  BOOL  fRingIndicator;   /* Ring Indicator active                 */
  BOOL  fDataSetReady;    /* DSR line indicator                    */
  BOOL  fClearToSend;     /* CTS line indicator                    */
  WORD  wUartChip;        /* UART chip type (numeric)              */
  CHAR  szUartChip[MAX_UART_CHIP];  /* UART chip type (string)     */
} COM_X_STRUCT;

typedef struct _COM_STRUCT
{
  WORD wNmbrComPorts;                     /* Total count of COM ports */
  COM_X_STRUCT ComxInfo[MAX_COM_PORTS];   /* Individual COM port data */
} COM_STRUCT;


/* IRQ Structure */

#define MAX_IRQ_DESCRIPTION        20
#define MAX_IRQ_DETECTED_STRINGS    4
#define MAX_IRQ_DETECTED_LEN       21
#define MAX_IRQ_HANDLED_BY         20

typedef struct _IRQ_X_STRUCT
{

  BOOL  fDetected;                        /* TRUE if something detected    */
  DWORD dwIrqAddress;                     /* IRQ interrupt vector          */
                                          /* Items detected (COM, Mouse)   */
  CHAR  szDetected[MAX_IRQ_DETECTED_STRINGS][MAX_IRQ_DETECTED_LEN];
  CHAR  szHandledBy[MAX_IRQ_HANDLED_BY];  /* What is located at the IRQ's  */
                                          /*   address (BIOS, TSR, etc).   */
} IRQ_X_STRUCT;

#define MAX_IRQ_X_STRUCTS          16

typedef struct _IRQ_STRUCT
{
  WORD wNmbrIrqs;                           /* Number of IRQ's in system */
  IRQ_X_STRUCT IrqxInfo[MAX_IRQ_X_STRUCTS]; /* One item for each IRQ */
} IRQ_STRUCT;


/* TSR Program List Structure */

#define MAX_TSR_NAME       19
#define MAX_TSR_CMD_LINE   32

typedef struct _TSR_PROGRAMS_STRUCT
{
  WORD  wAddress;                         /* Address of the  MCB    */
  DWORD dwBlockSize;                      /* Size of the MCB        */
  CHAR  szTsrName[MAX_TSR_NAME];          /* MCB owner name         */
  CHAR  szParameters[MAX_TSR_CMD_LINE];   /* Command line paramters */
} TSR_PROGRAMS_STRUCT;


/* Device Driver Structure */

typedef struct _DEVICE_DRIVER_STRUCT
{
  DWORD dwHeader;         /* Address of the device driver's header.    */
  WORD  wAttributes;      /* 16 bits of attributes for the driver.     */
                          /*   Bit 15 is set for character devices.    */
  CHAR  szDeviceName[15]; /* Device name for character devices.        */
  CHAR  szDriverFilename[9];  /* DEVICE=Filename for device.           */
  WORD  wUnits;           /* Number of units for block devices.        */
} DEVICE_DRIVER_STRUCT;


/* File Finder structure */

typedef struct _FILE_INFO
{
  BYTE  bAttrib;              /* File attributes                        */
  WORD  wTime;                /* Time of last write to file             */
  WORD  wDate;                /* Date of last write to file             */
  DWORD dwSize;               /* Size of the file                       */
  DWORD dwFileVersionMS;      /* File version (Most Significant DWORD)  */
  DWORD dwFileVersionLS;      /* File version (Least Significant DWORD) */
  CHAR FAR *fpszPathToFile;   /* Fully qualified path to the file       */
  VOID FAR *fpNextFileInfo;   /* Next FILE_INFO structure in the chain  */
} FILE_INFO;

extern FILE_INFO FAR *pfiDlg;   /* File Info for dialog processing */

typedef struct _DATE_INFO
{
  BITS  Day   : 5;
  BITS  Month : 4;
  BITS  Year  : 7;
} DATE_INFO;

#define SIZE_COLUMN   47
#define DATE_COLUMN   63

#define FIND_FILE_LINE_LENGTH  71


/* Test Printer structure */

typedef struct _TEST_PRINTER
{
  BOOL fPostscript;   /* TRUE if the Postscript test is requested     */
  BOOL f8BitTest;     /* TRUE if the 8-bit ASCII test is requested    */
  BOOL fSerialTest;   /* TRUE if serial port, FALSE for parallel port */
  WORD wPort;         /* Printer port number for the test             */
} TEST_PRINTER;

extern TEST_PRINTER tpValue;


/* Browsing structure */

typedef struct _MEM_STRING_DATA
{
  char far  *cfpString;
  int       iStringLen;
} MEM_STRING_DATA;

#define MAX_MSD_STRINGS   24
#define MAX_BROWSE_LINES   4

extern QSZ * pqszBrowseStrings;
extern PSZ   pszBrowseTitle;


/* Status line help structure */

typedef struct _STATUS_LINE_STRINGS
{
  WORD wStringNmbr;
  PSZ  pszString;
} STATUS_LINE_STRINGS;



/***************/
/* Prototyping */
/***************/

/**************************** MSD.C *********************************/

BOOL ProcessCmdLine (INT argc, PSZ argv[]);


/**************************** SUMMARY.C *****************************/

#ifdef CW_INCLUDED

VOID BeginCwInterface (VOID);

BOOL GetCwSumInfo (VOID);

#ifdef BASED

BOOL FAR PASCAL WndSegInit (VOID);

_segment pWndSeg;

#endif /* BASED */

VOID SetIsaColors (BOOL fBlackWhiteFlag);

VOID FAR InitMainWindows(void);

STATIC LONG FAR PASCAL MainWndProc (PWND, WORD, WORD, DWORD);
STATIC LONG FAR PASCAL StatusLineProc (PWND, WORD, WORD, DWORD);

VOID InitMsgBoxPlateBtns (VOID);

PWND FAR CreateMsgBtn ( szWindowName,
                        wStyle, wExStyle,
                        X, Y,
                        nWidth, nHeight,
                        hwndDlg,
                        id );

LONG FAR
MyMsgBoxProc (REGISTER PWND_DLG pwnd,
              WORD  message,
              WORD  wParam,
              DWORD lParam);

VOID InitMyEditItem (VOID);

LONG FAR
MyEditItemProc (REGISTER PWND_EDIT pwnd,
                WORD  message,
                WORD  wParam,
                DWORD lParam);

VOID WarnWindowsUser (VOID);

STATIC BOOL PASCAL CreateChildWindows (PVOID);
STATIC BOOL PASCAL WriteSummaryText (PVOID);

STATIC BOOL FARPUBLIC FrameFilter (PMSG, WORD *);

/* AUXCOW.C */

VOID FAR PASCAL Exit (INT);

BOOL FHelpMsg (PMSG pmsg, WORD cnx);

VOID FARPUBLIC Help (WORD hem, WORD hid, VOID *pv, WORD kk);

VOID ExamineDisplay (VOID);

#endif /* CW_INCLUDED */


/**************************** GETINFO.C *****************************/

INT  GetInfoSize (WORD wRecordType, BOOL fHeaderRecord);

BOOL GetInfo (WORD wRecordType,
              VOID *pStructForInfo,
              BOOL fMinimumInfo,
              BOOL fHeaderRecord,
              BOOL fReportFlag);


/**************************** SPRNINFO.C ****************************/

QSZ * SprintInfo (WORD wRecordType,
                  VOID *pStructWithInfo,
                  CHAR szSumStrings[][MAX_SUMM_INFO + 5],
                  BOOL fReportFlag);


/**************************** RPTINFO.C *****************************/

BOOL ReportOnly (PSZ pszRptFilename);

BOOL ReportFromCW (VOID);

BOOL WriteInfo (FILE *fileOutput,
                PSZ  pszTitle,
                QSZ  *pqszStrings);

BOOL WriteSepLine (FILE *fileOutput, PSZ pszTitle);

BOOL WritePageBreak (FILE *fileOutput);

BOOL ReportFile (PSZ  pszTitle,
                 PSZ  pszFilename,
                 FILE *fileOutput);

BOOL TestPrinter (BOOL fPostscript,
                  BOOL f8BitTest,
                  BOOL fSerialTest,
                  WORD wPort);

BOOL PrintTest (PSZ pszPort, BOOL f8BitTest, BOOL fPostscript);


/**************************** SHOWINFO.C ****************************/

#ifdef CW_INCLUDED

PWND ShowInfo (WORD wRecordType);

STATIC PWND CreateInfoWnd (PSZ  pszWindowTitle,
                           QSZ  *pqszStrings,
                           BOOL fKeepFocus);

STATIC LONG FAR PASCAL InfoWndProc (PWND, WORD, WORD, DWORD);

STATIC VOID InfoTxtWndScroll (PWND pwnd, WORD wParam, DWORD lParam);

STATIC LONG FAR PASCAL InfoTxtWndProc (PWND, WORD, WORD, DWORD);

STATIC BOOL PASCAL CreateInfoChildWindows (PWND pwndParent);

STATIC BOOL PASCAL CreateInfoTxtChildren (PWND pwndParent);

VOID DrawPlateButton1 (PWND  pwnd,
                       WORD  message,
                       WORD  wParam,
                       DWORD lParam);

#endif /* CW_INCLUDED */


/**************************** DIALOGS.C *****************************/

#ifdef CW_INCLUDED

LONG FAR PASCAL ReportDlg (PWND  pwnd,
                           WORD  message,
                           WORD  wParam,
                           DWORD lParam);

LONG FAR PASCAL FindFileDlg1 (PWND  pwnd,
                              WORD  message,
                              WORD  wParam,
                              DWORD lParam);

LONG FAR PASCAL FindFileDlg2 (PWND  pwnd,
                              WORD  message,
                              WORD  wParam,
                              DWORD lParam);

LONG FAR PASCAL InsertCmdDlg1 (PWND  pwnd,
                               WORD  message,
                               WORD  wParam,
                               DWORD lParam);

LONG FAR PASCAL InsertCmdDlg2 (PWND  pwnd,
                               WORD  message,
                               WORD  wParam,
                               DWORD lParam);

LONG FAR PASCAL InsertCmdDlg3 (PWND  pwnd,
                               WORD  message,
                               WORD  wParam,
                               DWORD lParam);

LONG FAR PASCAL InsertCmdDlg4 (PWND  pwnd,
                               WORD  message,
                               WORD  wParam,
                               DWORD lParam);

LONG FAR PASCAL TestPrinterDlg (PWND  pwnd,
                                WORD  message,
                                WORD  wParam,
                                DWORD lParam);

LONG FAR PASCAL CustInfoCmdDlg (PWND  pwnd,
                                WORD  message,
                                WORD  wParam,
                                DWORD lParam);

LONG FAR PASCAL AboutDlg (PWND  pwnd,
                          WORD  message,
                          WORD  wParam,
                          DWORD lParam);

LONG FAR PASCAL MemoryBlockDisplayDlg (PWND  pwnd,
                                       WORD  message,
                                       WORD  wParam,
                                       DWORD lParam);

LONG FAR PASCAL MemoryBrowserDlg (PWND  pwnd,
                                  WORD  message,
                                  WORD  wParam,
                                  DWORD lParam);

VOID UpdateMemMap (PWND pwnd, MEM_MAP *pmmMap);

LONG FAR PASCAL ViewWhichFileDlg (PWND  pwnd,
                                  WORD  message,
                                  WORD  wParam,
                                  DWORD lParam);

LONG FAR PASCAL WarnWindowsUserDlg (PWND pwnd,
                                    WORD message,
                                    WORD wParam,
                                    DWORD lParam);

#endif /* CW_INCLUDED */


/**************************** MSDSYS.C ******************************/

FILE * OpenFile (PSZ pszFilename, PSZ pszMode, BOOL fShowError);

BOOL CloseFile (FILE *fp);

WORD CreateTempFile (PSZ pszPathname);

WORD DeleteFile (PSZ pszPathname);

WORD RenameFile (PSZ pszPathname1, PSZ pszPathname2);

FILE_INFO FAR *FindFile (PSZ  pszFilename,
                         PSZ  pszPathname,
                         BOOL fSearchFlags,
                         CHAR chDriveLetter);

BOOL FindOnBootDrive (FILE_INFO FAR * FAR *ppFileInfo,
                      DISK_STRUCT         *pDisk,
                      OS_VERSION_STRUCT   *pOsVer,
                      PSZ                 pszFilename,
                      BOOL                fSearchFlags);

BOOL FindFileOnDrive (FILE_INFO FAR * FAR *ppFileInfo,
                      PSZ  pszFilename,
                      BOOL fSearchFlags,
                      CHAR chDriveLetter);

BOOL FindFileInCwd (FILE_INFO FAR * FAR *ppFileInfo,
                    PSZ  pszFilename,
                    BOOL fSearchFlags);

BOOL GetFindFileInfo (DISK_STRUCT       **ppDisk,
                      OS_VERSION_STRUCT **ppOsVer,
                      BOOL              fSearchFlags);

VOID FreeFileInfo (FILE_INFO FAR *pFileInfo);

VOID ProceduralLangChk (INT argc, PSZ argv[], BOOL fFlag);

QSZ * AllocStringSpace (WORD wNmbrStrings,
                        WORD wNmbrChars);

VOID FreeStringSpace (QSZ *pqszStrings);

WORD DisplayLen (QSZ qszString);

QSZ  QstrcpyAlign (QSZ qszString1, QSZ qszString2, WORD wIndent);

QSZ  QstrcatAlign (QSZ qszString1, QSZ qszString2, WORD wIndent);

QSZ  QstrncpyAlign (QSZ  qszString1,
                    QSZ  qszString2,
                    WORD wNmbrChars,
                    WORD wIndent);

QSZ PrepNextString (QSZ *ppszStrings, WORD i);

VOID ShowError (BOOL fFlags,
                PSZ  pszString1,
                PSZ  pszString2,
                PSZ  pszString3);

BOOL WriteLine (QSZ qszString, FILE *fileOutput, BOOL fFilterFlag);

BOOL _WriteLine (PSZ pszString, FILE *fileOutput);

BOOL WriteChar (CHAR chChar, FILE *fileOutput);

BOOL OutputLine (PSZ pszString, FILE *fileOutput);

BOOL _WriteChar (CHAR chChar, FILE *fileOutput);

INT ReadLine (PSZ  pszString,
              WORD wMaxChar,
              FILE *fileInput,
              BOOL fHexDump);

INT ReadChar (FILE *fileInput);

BOOL _DosGetLine (CHAR *pchInputString, INT iMaxChar);

BOOL PutString (PSZ pszString);

VOID FAR CriticalErrorHandler (WORD wDevError,
                               WORD wErrCode,
                               BYTE FAR *fpDeviceHeader);

VOID DisplayStatus (WORD wNumber);

VOID ShowStatus (PSZ pszMessage);

BOOL ParseCommandLine (INT *pi, INT argc, PSZ argv[]);

VOID CmdLineHelp (VOID);

BOOL SetMiscGlobals (PSZ);

VOID InitParm1 (PSZ);

VOID MemoryFence (VOID);

#if HEAP_DEBUG

VOID HeapCheck (PSZ pszDescription);

VOID HeapShowStatus (INT iStatus, PSZ pszDescription);

#define OutOfMemory()   NoMemory (__FILE__, __LINE__)
VOID NoMemory (PSZ pszFile, WORD wLine);
#define maxParsedLine pszMsDos
#define minParsedLine maxParsedLine
#define fParserBitmask wMaxRecords

#else

#define HeapCheck()
#define HeapShowStatus()
#define OutOfMemory()   NoMemory()
#define maxParsedLine pszMsDos
#define minParsedLine maxParsedLine
#define fParserBitmask wMaxRecords
VOID NoMemory (VOID);

#endif

VOID BiosStringOutAt (PSZ pszString, WORD wAttrib, WORD wLine, WORD wCol);

VOID BiosStringOut (PSZ pszString, WORD wAttrib);

VOID BiosCharOutAt (WORD wChar,
                    WORD wAttrib,
                    WORD wCopies,
                    WORD wLine,
                    WORD wCol);

VOID BiosCharOut (WORD wChar, WORD wAttrib, WORD wCopies);

VOID BiosLocate (WORD wLine, WORD wCol);

VOID BiosClearScreen (WORD wAttrib);

VOID BiosScrollUp (WORD wX1, WORD wY1,
                   WORD wX2, WORD wY2,
                   WORD wAttrib,
                   WORD wNmbrLines);

VOID BiosDrawFilledBox (WORD wX1, WORD wY1,
                        WORD wX2, WORD wY2,
                        WORD wBorderAttrib,
                        WORD wInteriorAttrib);

VOID BiosDrawBox (WORD wX1, WORD wY1,
                  WORD wX2, WORD wY2,
                  WORD wAttrib);


/*************************** CUSTINFO.C *****************************/

BOOL GetCustInfo (CUSTINFO *pCustInfo, BOOL fMinimumInfo);

BOOL GetCustInfoLine (PSZ pszPromptString, CHAR *pchInputString);

QSZ * SprintCustInfo (VOID *pCustInfo);


/**************************** SUMINFO.C *****************************/

BOOL GetSummaryInfo (SUMMARY_STRUCT *pSum, BOOL fMinimumInfo);

QSZ * SprintSummaryInfo (PSZ paszButtonNames[], SUMMARY_STRUCT *pSum);


/*************************** COMPUTER.C *****************************/

BOOL GetComputerInfo (COMPUTER_STRUCT *pComputer, BOOL fMinimumInfo);

BOOL GetComputerIrqInfo (COMPUTER_STRUCT *pComputer);

QSZ * SprintComputerInfo (COMPUTER_STRUCT *pComputer,
                          CHAR szSumStrings[][MAX_SUMM_INFO + 5]);

WORD GetRomName (BOOL fSearchType, PSZ *ppszStrings);

BOOL GetRomVersion (CHAR pachVersionStrings[][MAX_BIOS_VERSION_LEN],
                    CHAR FAR *fpSearchArea,
                    WORD wSearchLength);

BOOL GetRomDate (CHAR FAR * fpSearchArea,
                 WORD wSearchLength,
                 CHAR * pchDateString);

CHAR FAR *fbiInstr (CHAR FAR *fpSearchArea,
                    PSZ      pszSearchString,
                    WORD     wSearchLength);

CHAR FAR *fbMemString (CHAR FAR *pszfString,
                       WORD *pwLength);

VOID GetBiosCategory (COMPUTER_STRUCT *pComputer);

WORD FAR _cdecl chips (VOID);


/**************************** MEMINFO.C *****************************/

BOOL GetMemInfo (MEMORY_STRUCT *pMem, BOOL fMinimumInfo);

int MInitialize (MEMORY_STRUCT FAR * pMem_Info);

int Get_Conv_Mem_Info (MEMORY_STRUCT FAR * pMem_Info);

int Get_Ext_Mem_Info (MEMORY_STRUCT FAR * pMem_Info);

int GetCMOSExtended (MEMORY_STRUCT FAR * pMem_Info);

int Test_For_Emm (MEMORY_STRUCT FAR * pMem_Info);

int Get_Emm_Info(MEMORY_STRUCT FAR * pMem_Info);

int GetVCPIInfo (MEMORY_STRUCT FAR * pMem_Info);

int GetDPMIInfo (MEMORY_STRUCT FAR * pMem_Info);

int QueryDPMIMemInfo (MEMORY_STRUCT FAR * pMem_Info);

int Get_Xmm_Info(MEMORY_STRUCT FAR * pMem_Info, WORD wProcessorType);

WORD _cdecl _far GetSuperXmsInfo (DWORD XMSControl,
                                  DWORD * SxmsLargestFree,
                                  DWORD * SxmsTotalFree);

VOID GetRomMap (ROM_MAP *pRomMap, WORD wBusType);

int Get_VisualMemMap (MEMORY_STRUCT FAR * pMem_Info, WORD wBusType);

BOOL VisualMapOverlay (MEMORY_STRUCT FAR * pMem);

int FillMemMap (MEMORY_STRUCT FAR * pMem, int iRow, int iCol,
                   int iNumOfChars, unsigned char uchFillChar);

int FillMemMapOverlay (MEMORY_STRUCT FAR * pMem, int iRow, int iCol,
                       int iNumOfChars, unsigned char uchFillChar);

int _fastcall RamRomWomCheck (unsigned uSegment,
                              unsigned uOffset,
                              unsigned uLength,
                              BOOL     fWindowsRunning);

int _fastcall OptionRomCheck (unsigned uSegment, unsigned uOffset);

QSZ * SprintMemInfo (BOOL fMemoryType,
                     MEMORY_STRUCT *pMem,
                     CHAR szSumStrings[][MAX_SUMM_INFO + 5],
                     BOOL fOverlayFlag);

VOID CopyVisualMemMap (MEMORY_STRUCT *pMem,
                       QSZ  qszString,
                       WORD i,
                       BOOL fOverlayFlag);


/*************************** VIDEO.C ********************************/

BOOL GetVideoInfo (VIDEO_STRUCT *pVideo, BOOL fMinimumInfo);

QSZ * SprintVideoInfo (VIDEO_STRUCT *pVideo,
                       CHAR szSumStrings[][MAX_SUMM_INFO + 5]);

PSZ  SubsystemName (BYTE bType);

PSZ  DisplayName (BYTE bType);

BOOL GetTigaInfo (VIDEO_STRUCT *pVideo);

VOID _cdecl FAR VideoID (VIDEO_STRUCT FAR *fpVideo);


/**************************** NETINFO.C *****************************/

BOOL GetNetworkInfo (NETWORK_STRUCT *pNetInfo, BOOL fMinimumInfo);

int Netbios_Installed (NETWORK_STRUCT *pNetInfo);

VOID ClearNcb (struct Ncb * pNcbPtr);

VOID NetbiosRequest (struct Ncb * NcbPointer);

BOOL Novell_Installed (NETWORK_STRUCT *pNetInfo);

VOID Get_Novell_Info (NETWORK_STRUCT *pNetInfo);

int BanyanRunning (VOID);

int LANtasticRunning (NETWORK_STRUCT *pNetInfo);

BOOL Msnet_Installed (VOID);

BOOL Get_Msnet_Machine_Name (NETWORK_STRUCT *pNetInfo);

int LanManager_Installed (NETWORK_STRUCT * pNetInfo);

int GetLanAutoexec (NETWORK_STRUCT * pNetInfo);

void Adapter_Status (NETWORK_STRUCT * pNetInfo);

BOOL Server_Connection (VOID);

BOOL LAN_Basic_Enhanced (NETWORK_STRUCT * pNetInfo);

int Get_CSD_Info (NETWORK_STRUCT * pNetInfo);

VOID GetLM_VersionInfo (NETWORK_STRUCT * pNetInfo);

QSZ * SprintNetworkInfo (NETWORK_STRUCT *pNetInfo,
                         CHAR szSumStrings[][MAX_SUMM_INFO + 5]);

VOID NetBiosStrings (NETWORK_STRUCT * pNetInfo,
                     QSZ            * pqszStrings,
                     INT            * pI);

BOOL GetServicesInfo (QSZ *pqszStrings, INT *pI);

BOOL GetProtocolInfo (NETWORK_STRUCT *pNetInfo, QSZ *pqszStrings, INT *pI);

BOOL GetNifModelInfo (PSZ pszNifFile, QSZ * pqszStrings, INT * pI);

BOOL GetLanmanIniDriverInfo (NETWORK_STRUCT *pNetInfo,
                             QSZ            *pqszStrings,
                             INT            *pI);

BOOL PcNfsInstalled (VOID);

WORD IsWorkgrpSysInstalled (void);

WORD GetTransitionVersion (void);


/**************************** OSINFO.C ******************************/

BOOL GetOsVersionInfo (OS_VERSION_STRUCT *pOsVer, BOOL fMinimumInfo);

QSZ * SprintOsVersionInfo (OS_VERSION_STRUCT *pOsVer,
                           CHAR szSumStrings[][MAX_SUMM_INFO + 5]);

VOID OsErr (VOID);

BOOL WinVerDetect (WORD *pwWindowsType,
                   WORD *pwWindowsMajor,
                   WORD *pwWindowsMinor,
                   WORD *pfDosShell);

BOOL GetDosOemStrings (OS_VERSION_STRUCT *pOsVer);


/*************************** MOUSINFO.C *****************************/

BOOL GetMouseInfo (MOUSE_STRUCT *pMouse);

VOID NoMouseDriver (MOUSE_STRUCT *pMouse);

QSZ * SprintMouseInfo (MOUSE_STRUCT *pMouse,
                       CHAR szSumStrings[][MAX_SUMM_INFO + 5]);

BOOL AltPS2MouseChk (VOID);

WORD FAR _cdecl fnHardMouseDetect (VOID);

WORD FAR _cdecl ArcnetCardPresent (VOID);


/*************************** OTHRINFO.C *****************************/

BOOL GetOtherInfo (OTHER_STRUCT *pOther, BOOL fMinimumInfo);

BOOL GetGameAdapterValues (OTHER_STRUCT *pOther);

QSZ * SprintOtherInfo (OTHER_STRUCT *pOther,
                       CHAR szSumStrings[][MAX_SUMM_INFO + 5]);


/*************************** DISKINFO.C *****************************/

BOOL GetDiskInfo (DISK_STRUCT *pDisk, BOOL fMinimumInfo);

VOID _cdecl GetCmosDiskInfo (WORD wHardDriveNmbr,
                             DISK_STRUCT *pDisk,
                             WORD wDriveIndex);

BOOL ValidDrive (BYTE bDrive);

BYTE DosGetCurrentDrive (VOID);

BYTE DosSetCurrentDrive (BYTE bDrive);

VOID GetDiskProgList (DISK_STRUCT *pDisk, WORD wDosVersion);

QSZ * SprintDiskInfo (DISK_STRUCT *pDisk,
                      CHAR szSumStrings[][MAX_SUMM_INFO + 5]);


/**************************** LPTINFO.C *****************************/

BOOL GetLptInfo (LPT_STRUCT *pLpt, BOOL fMinimumInfo);

QSZ * SprintLptInfo (LPT_STRUCT *pLpt,
                     CHAR szSumStrings[][MAX_SUMM_INFO + 5]);


/**************************** COMINFO.C *****************************/

BOOL GetComInfo (COM_STRUCT *pCom, BOOL fMinimumInfo);

QSZ * SprintComInfo (COM_STRUCT *pCom,
                     CHAR szSumStrings[][MAX_SUMM_INFO + 5]);


/**************************** IRQINFO.C *****************************/

BOOL GetIrqInfo (IRQ_STRUCT *pIrq, BOOL fMinimumInfo);

BOOL SetIrqDetectedStrings (IRQ_STRUCT       *pIrq,
                            COMPUTER_STRUCT  *pComputer,
                            MOUSE_STRUCT     *pMouse,
                            DISK_STRUCT      *pDisk,
                            LPT_STRUCT       *pLpt,
                            COM_STRUCT       *pCom);

BOOL SetIrqHandledByStrings (IRQ_STRUCT           *pIrq,
                             TSR_PROGRAMS_STRUCT  *pTsr,
                             DEVICE_DRIVER_STRUCT *pDevice);

QSZ * SprintIrqInfo (IRQ_STRUCT *pIrq);

BOOL GetSwIntTable (VOID);


/**************************** TSRLIST.C *****************************/

WORD GetTsrInfoSize (VOID);

CHAR FAR * FindFirstMcbHeader (VOID);

QSZ * SprintTsrInfo (TSR_PROGRAMS_STRUCT *pTsrStruct,
                     BOOL fIncludeParms);

BOOL GetTsrInfo (TSR_PROGRAMS_STRUCT *pTsrStruct, BOOL fMinimumInfo);

VOID LinkUmbToDosChain (BOOL fLinkFlag);


/**************************** DEVTAB.C ******************************/

WORD GetDeviceDriversSize (VOID);

BOOL GetDeviceDriversInfo (DEVICE_DRIVER_STRUCT *pDevStruct,
                           BOOL fMinimumInfo);

QSZ * SprintDeviceDriverInfo (DEVICE_DRIVER_STRUCT *pDevStruct);

CHAR FAR * FindFirstDevHeader (VOID);


/*************************** VIEWFILE.C *****************************/

QSZ * ViewFile (PSZ pszFilename, BOOL fHexDump);

BOOL FindAndViewFile (PSZ pszFilename, BOOL fSearchFlags, BOOL fHexDump);


/**************************** INSERT.C ******************************/

VOID InsertCommand (VOID);

BOOL ChangeFile (PSZ  pszFilename,
                 PSZ  pszSection,
                 PSZ  pszCommand,
                 PSZ  pszSearchString,
                 WORD wReplaceLine);

VOID ReadCommands (PWND pwnd);

BOOL FindSection (PSZ  pszSection, FILE *fpInsertFile, FILE *fpOutputFile);

BOOL HandleDuplicates (WORD wFunction,
                       PSZ  pszSearch,
                       PSZ  pszReplace,
                       FILE *fpInfile,
                       FILE *fpOutfile,
                       PWND pwndList);

BOOL HdMatch (char *pszSearch, char *pszFullLine);

FILE * OpenIniFile (VOID);

BOOL WriteThrough (FILE *fpInputFile, FILE *fpOutputFile);

VOID AddIniLine (PWND pwndList, PSZ pszString);


/**************************** BROWSE.C ******************************/

QSZ * GetBrowseInfo (PSZ      pszTitle,
                     PSZ      pszSearchString,
                     CHAR FAR *fpSearchArea,
                     DWORD    dwSearchLength);

QSZ * SprintBrowseInfo (MEM_STRING_DATA *pmsdData);

PSZ * ListOptionRoms (VOID);

BOOL ReportBrowseInfo (FILE *fileReportFile);

VOID BrowseInit (char far *cfpSearchArea,
                 MEM_STRING_DATA *msdFoundStrings,
                 int iMaxMSDStrings);

int Browse (char **paszSearchStrings,
            char far *cfpSearchArea,
            unsigned int uiLengthOfSearch,
            MEM_STRING_DATA *msdFoundStrings,
            int iMaxMSDStrings);

void ClearMSDArray (MEM_STRING_DATA *msd, int iStringCount);

int SingleStringMemSearch (MEM_STRING_DATA *msdFoundArray,
                           int iMaxArray,
                           char *pszSearchString,
                           char far *cfpAreaToSearch,
                           unsigned int uiLengthOfSearch);

int AddToFoundArray (char far *pszfString,
                     int iStringLength,
                     MEM_STRING_DATA *msdArray,
                     int iMaxArray);


/**************************** VERINFO.C *****************************/

typedef struct _LANGUAGE_ID
{
  WORD wLanguageId;
  PSZ  pszLanguageId;
} LANGUAGE_ID;

#define MAX_LANGUAGE_IDS 44
extern LANGUAGE_ID rgLang[];

typedef struct _CODEPAGE_ID
{
  WORD wCodePage;
  PSZ  pszCodePage;
} CODEPAGE_ID;

extern CODEPAGE_ID rgCP[];
#define MAX_CODEPAGE 11

BYTE * GetFileVersion (PSZ pszFilename, BOOL fMinimumInfo);

QSZ * SprintFileVersion (BYTE * pVer);
