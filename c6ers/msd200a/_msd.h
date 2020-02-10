/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * _MSD.H - Header file for use within MSD.C
 *
 * Note:  _MSD.H and MSD.H are interdependent.
 ********************************************************************/

#ifdef CW_INCLUDED

/* Comment this line out when we're sure the 'C' startup */
/*   code will not jump to space.                        */
float flTemp = (float) 0.0;

#include "SRMENU.H"
#include "dialogs.adm"

#endif /* CW_INCLUDED */


/* Global Variables */

BOOL fCwIsReady      = FALSE;  /* TRUE when CW is initialized           */
BOOL fBlackWhite     = FALSE;  /* TRUE for black and white operation    */
BOOL fFastStart      = FALSE;  /* TRUE for no initial detection         */
BOOL fReportFlag     = FALSE;  /* TRUE if a report is underway          */
BOOL fReportOnly     = FALSE;  /* TRUE if "/F filename" was used, or if */
                               /*   CW_INCLUDED is not defined          */
BOOL fSummaryOnly    = FALSE;  /* TRUE if MSD is to report summary      */
                               /*   information only to a file          */
BOOL fCriticalError  = FALSE;  /* Critical Error Flag                   */
BOOL fWindowsRunning = FALSE;  /* TRUE if windows is running            */

WORD wDosMajor;           /* Stores the DOS version */
WORD wDosMinor;

#ifdef CW_INCLUDED

BYTE fAdjustMsgBox = FALSE;

PWND pwndSummaryFrame = NULL;
PWND pwndMainFrame    = NULL;
PWND pwndStatusLine   = NULL;

/* For subclassing MessageBox types */
PLFN_WNDPROC OldMsgBoxProc;

LONG FAR MyMsgBoxProc (REGISTER PWND_DLG, WORD, WORD, DWORD);

/* For subclassing Edit Item types */
PLFN_WNDPROC OldEditItemProc;

LONG FAR MyEditItemProc (REGISTER PWND_EDIT, WORD, WORD, DWORD);


HMNU hmnuMenuBar = (HMNU) &pmenuBlade;

ISA_PAIR aisapColor[isaMax] =
  {
    {  7,  8 },   /*  0 = isaBackground                                   */

    { 15,  0 },   /*  1 = isaHilite                                       */
    {  8, 15 },   /*  2 = isaGreyed                                       */
    {  0, 15 },   /*  3 = isaEnabled                                      */
    {  8, 15 },   /*  4 = isaDisabled                                     */
    {  0, 15 },   /*  5 = isaAlert                                        */

    {  0, 15 },   /*  6 = isaDialogBox,     isaStatic, isaButton, isaEdit */
    {  0,  7 },   /*  7 = isaPushButton                                   */
    {  8,  7 },   /*  8 = isaButtonDown                                   */
    {  0, 15 },   /*  9 = isaListBox                                      */
    {  0, 15 },   /* 10 = isaScrollbar                                    */

    {  7,  0 },   /* 11 = isaElevator                                     */
    {  0, 15 },   /* 12 = isaMenuBox                                      */
    {  0, 15 },   /* 13 = isaMenu                                         */
    { 15,  0 },   /* 14 = isaMenuSelected                                 */
    { 12, 15 },   /* 15 = isaMenuHilite                                   */

    {  7,  0 },   /* 16 = isaMenuHiliteSel                                */
    { 12,  0 },   /* 17 = isaItemHiliteSel                                */
    { 12, 15 },   /* 18 = isaDialogAccel                                  */
    { 12, 15 },   /* 19 = isaDialogAccelBor                               */
    {  8,  8 },   /* 20 = isaShadow                                       */

    {  7,  0 },   /* 21 = isaDialogBar                                    */
    {  7,  0 },   /* 22 = isaDialogTitle                                  */
    {  0,  0 },   /* 23 = isa3DGroupBoxIn,  isa3DGroupBoxUL               */
    { 15,  7 },   /* 24 = isa3DGroupBoxOut, isa3DGroupBoxBR               */
    {  0,  7 },   /* 25 = isa3DListboxIn,   isa3DListboxUL                */

    { 15,  7 },   /* 26 = isa3DListboxOut,  isa3DListboxBR                */
    { 15,  7 },   /* 27 = isa3DPushBtnIn,   isa3DPushBtnUL                */
    {  8,  7 },   /* 28 = isa3DPushBtnOut,  isa3DPushBtnBR                */
    { 14,  7 },   /* 29 = isaButtonAccel,   isaSystemMax                  */
    { 15,  1 },   /* 30 = isaUserMin,       isaSummaryBlank               */

    { 15,  1 },   /* 31 = isaSummaryText                                  */
    {  0, 15 },   /* 32 = isaInfoActive                                   */
    {  0, 15 },   /* 33 = isaInfoActiveBorder                             */
    {  0,  8 },   /* 34 = isaInfoInactive                                 */
    {  0,  7 },   /* 35 = isaStatusLine                                   */

    {  7,  0 },   /* 36 = isaAlternate                                    */
    { 14,  0 },   /* 37 = isaMemoryMap                                    */
    {  7,  1 },   /* 38 = isaSummaryBtnShadow                             */
    {  7, 15 },   /* 39 = isaMessageBtn1                                  */
    {  4,  7 },   /* 40 = isa3DBtnHilite                                  */

    {  7,  0 },   /* 41 =                                                 */
    {  7,  0 },   /* 42 =                                                 */
    {  7,  0 },   /* 43 =                                                 */
    {  7,  0 },   /* 44 =                                                 */
    {  7,  0 }    /* 45 = isaMax,           isaUserMax, isaTemp           */
  };

ISA_PAIR aisapBlackAndWhite[isaMax] =
  {
    {  7,  0 },   /*  0 = isaBackground                                   */

    {  7,  0 },   /*  1 = isaHilite                                       */
    {  7,  0 },   /*  2 = isaGreyed                                       */
    {  0,  7 },   /*  3 = isaEnabled                                      */
    {  0,  7 },   /*  4 = isaDisabled                                     */
    {  0,  7 },   /*  5 = isaAlert                                        */

    {  0,  7 },   /*  6 = isaDialogBox,     isaStatic, isaButton, isaEdit */
    {  0,  7 },   /*  7 = isaPushButton                                   */
    {  7,  0 },   /*  8 = isaButtonDown                                   */
    {  0,  7 },   /*  9 = isaListBox                                      */
    {  0,  7 },   /* 10 = isaScrollbar                                    */

    {  7,  0 },   /* 11 = isaElevator                                     */
    {  0,  7 },   /* 12 = isaMenuBox                                      */
    {  0,  7 },   /* 13 = isaMenu                                         */
    {  7,  0 },   /* 14 = isaMenuSelected                                 */
    {  7,  0 },   /* 15 = isaMenuHilite                                   */

    {  7,  0 },   /* 16 = isaMenuHiliteSel                                */
    {  7,  0 },   /* 17 = isaItemHiliteSel                                */
    {  7,  0 },   /* 18 = isaDialogAccel                                  */
    {  7,  0 },   /* 19 = isaDialogAccelBor                               */
    {  0,  0 },   /* 20 = isaShadow                                       */

    {  7,  0 },   /* 21 = isaDialogBar                                    */
    {  7,  0 },   /* 22 = isaDialogTitle                                  */
    {  0,  0 },   /* 23 = isa3DGroupBoxIn,  isa3DGroupBoxUL               */
    {  0,  7 },   /* 24 = isa3DGroupBoxOut, isa3DGroupBoxBR               */
    {  0,  7 },   /* 25 = isa3DListboxIn,   isa3DListboxUL                */

    {  0,  7 },   /* 26 = isa3DListboxOut,  isa3DListboxBR                */
    {  0,  7 },   /* 27 = isa3DPushBtnIn,   isa3DPushBtnUL                */
    {  0,  7 },   /* 28 = isa3DPushBtnOut,  isa3DPushBtnBR                */
    {  0,  7 },   /* 29 = isaButtonAccel,   isaSystemMax                  */
    {  7,  0 },   /* 30 = isaUserMin,       isaSummaryBlank               */

    {  7,  0 },   /* 31 = isaSummaryText                                  */
    {  0,  7 },   /* 32 = isaInfoActive                                   */
    {  0,  7 },   /* 33 = isaInfoActiveBorder                             */
    {  0,  7 },   /* 34 = isaInfoInactive                                 */
    {  0,  7 },   /* 35 = isaStatusLine                                   */

    {  7,  0 },   /* 36 = isaAlternate                                    */
    {  7,  0 },   /* 37 = isaMemoryMap                                    */
    {  7,  0 },   /* 38 = isaSummaryBtnShadow                             */
    {  7,  0 },   /* 39 =                                                 */
    {  7,  0 },   /* 40 =                                                 */

    {  7,  0 },   /* 41 =                                                 */
    {  7,  0 },   /* 42 =                                                 */
    {  7,  0 },   /* 43 =                                                 */
    {  7,  0 },   /* 44 =                                                 */
    {  7,  0 }    /* 45 = isaMax,           isaUserMax, isaTemp           */
  };

#endif /* CW_INCLUDED */

/* Variables for reporting */

WORD wLineCount;          /* Current line number   */
WORD wPageCount;          /* Current page number   */
WORD wColumnCount = 0;    /* Current column number */
WORD wReportIndent = 40 - (REPORT_WIDTH / 2);  /* Left indent for reports */

PSZ  pszReportFilename = NULL;  /* Pointer to report filename */


/* Global Strings */

PSZ  paszReportTo[] =     /* Filenames to report to */
  {
    "LPT1",
    "LPT2",
    "LPT3",
    "COM1",
    "COM2",
    "COM3",
    "COM4",
    NULL
  };

WORD wReportToIndex = 0;  /* Index to paszReportTo */

FILE_INFO FAR *pfiDlg;    /* File Info for dialog processing */
TEST_PRINTER tpValue;     /* Printer test info structure */


QSZ * pqszBrowseStrings = NULL;  /* Browsed strings are returned here */
PSZ   pszBrowseTitle    = NULL;  /* Title for browsed strings         */

#define MSD_VERSION_NUMBER "2.01"
PSZ  pszVersionNumber   = MSD_VERSION_NUMBER;

PSZ  pszNull            = "";
PSZ  pszYes             = "Yes";
PSZ  pszNo              = "No";
PSZ  pszNo_             = "No ";
PSZ  pszNone            = "None";
PSZ  pszUnknown         = "Unknown";

PSZ  pszInsufMemory     = "Insufficient Memory";
PSZ  pszErrorOpening    = "Error opening";
PSZ  pszErrorClosing    = "Error Closing File";
PSZ  pszErrorWriting    = "Error Writing";
PSZ  pszErrorReading    = "Error Reading";

PSZ  pszCon             = "CON";

PSZ  pszHeaderFormatString = "   Microsoft Diagnostics version %s   %2d/%02d/%02d   %2d:%02d%s   Page%3d";

PSZ  pszPostscriptTest1 = "initgraphics\r\n"
                          "/Times-Roman findfont 18 scalefont setfont\r\n"
                          "100 700 moveto\r\n"
                          "(Microsoft Corporation ) show\r\n"
                          "100 680 moveto\r\n"
                          "(Postscript Printer Connection and Processor Test) show\r\n"
                          "100 660 moveto\r\n"
                          "(Postscript version: )show 8 0 rmoveto version show\r\n";
PSZ  pszPostscriptTest2 = "100 640 moveto\r\n"
                          "(Date:  %d/%02d/%d   Time:  %d:%02d %s) show\r\n";
PSZ  pszPostscriptTest3 = "/Times-Roman findfont 12 scalefont setfont\r\n";

PSZ  pszOutput          = "w";  /* Used for OpenFile/fopen */

BOOL rgfReportItemFlag[] =
  {
    FALSE,  /* IDI_ALL_RECORDS           */
    FALSE,  /* IDI_MSD_HEADER_RECORD     */
    TRUE,   /* IDI_CUSTOMER_INFORMATION  */
    TRUE,   /* IDI_SUMMARY_SCREEN        */
    TRUE,   /* IDI_COMPUTER_RECORD       */
    TRUE,   /* IDI_MEMORY_RECORD         */
    TRUE,   /* IDI_VIDEO_RECORD          */
    TRUE,   /* IDI_NETWORK_RECORD        */
    TRUE,   /* IDI_OS_VERSION_RECORD     */
    TRUE,   /* IDI_MOUSE_RECORD          */
    TRUE,   /* IDI_OTHER_ADAPTERS_RECORD */
    TRUE,   /* IDI_DISK_DRIVE_RECORD     */
    TRUE,   /* IDI_LPT_RECORD            */
    TRUE,   /* IDI_COM_RECORD            */
    TRUE,   /* IDI_IRQ_RECORD            */
    TRUE,   /* IDI_TSR_PROGRAMS_RECORD   */
    TRUE,   /* IDI_DEVICE_DRIVERS_RECORD */
    TRUE,   /* Memory Browser            */
    TRUE,   /* AUTOEXEC.BAT              */
    TRUE,   /* CONFIG.SYS                */
    TRUE,   /* WIN.INI                   */
    TRUE,   /* SYSTEM.INI                */
    TRUE,   /* MSMAIL.INI                */
    TRUE,   /* PROTOCOL.INI		 */
    TRUE,   /* DBLSPACE.INI		 */
    TRUE    /* MEMMAKER.STS		 */
  };


PSZ  paszDefaultMsdIni[] =
  {
    "FILES=40, , CONFIG.SYS",
    "BUFFERS=20, , CONFIG.SYS",
    "SET TEMP=C:\\WINDOWS\\TEMP, , AUTOEXEC.BAT",
    NULL
  };


char * rgszSystemFiles[] =
  {
    "AUTOEXEC.BAT",
    "CONFIG.SYS",
    "SYSTEM.INI",
    "WIN.INI",
    "MSMAIL.INI",
    "PROTOCOL.INI",
    "DBLSPACE.INI",
    "MEMMAKER.STS",
    NULL
  };

WORD iszMax2 = sizeof (rgszSystemFiles) / sizeof (PSZ);

WORD rgwSystemFiles[] =
  {
    SEARCH_BOOT_DRIVE,
    SEARCH_BOOT_DRIVE,
    SEARCH_WINDIR,
    SEARCH_WINDIR,
    SEARCH_WINDIR,
    SEARCH_LANMAN_ROOT,
    SEARCH_LOCAL_DRIVES|SEARCH_ROOT|RECURSE_INTO_SUB_DIRS,
    SEARCH_LOCAL_DRIVES|SEARCH_ROOT|RECURSE_INTO_SUB_DIRS
  };

PSZ  pszInsertCommand  = NULL;
PSZ  pszInsertSection  = NULL;
PSZ  pszInsertFilename = NULL;


/* "Computer" strings */

PSZ  paszComputerTitles[] =
  {
        "Computer Name: ",
    "BIOS Manufacturer: ",
         "BIOS Version: ",
                       "",
                       "",
        "BIOS Category: ",
        "BIOS ID Bytes: ",
            "BIOS Date: ",
            "Processor: ",
     "Math Coprocessor: ",
             "Keyboard: ",
             "Bus Type: ",
       "DMA Controller: ",
        "Cascaded IRQ2: ",
    "BIOS Data Segment: ",
    NULL
  };

PSZ  pszEnhanced        = "Enhanced";
PSZ  pszNonEnhanced     = "Non-Enhanced";

PSZ  pszXtBus           = "ISA/XT/Classic Bus";
PSZ  pszAtBus           = "ISA/AT/Classic Bus";
PSZ  pszMicroChannel    = "Micro Channel";
PSZ  pszEisaBus         = "EISA";

PSZ  pszIntel8088       = "8088 or 8086";
PSZ  pszIntel80188      = "80188";
PSZ  pszIntel80186      = "80186";
PSZ  pszIntel80286      = "80286";
PSZ  pszIntel80386      = "80386";
PSZ  pszNECV20          = "NECV20 or NECV30";
PSZ  pszIntel8087       = "8087";
PSZ  pszIntel80287      = "80287";
PSZ  pszIntel80387      = "80387";
PSZ  pszInternal        = "Internal";
PSZ  pszInteli486       = "486DX";
PSZ  pszIntel486SX      = "486SX";


/* Memory strings */

PSZ  paszMemoryTitles[] =
  {
    "Conventional Memory",
                   "Total: ",
               "Available: ",

    "Extended Memory",
                   "Total: ",

    "MS-DOS Upper Memory Blocks",
              "Total UMBs: ",
         "Total Free UMBs: ",
      "Largest Free Block: ",

    "Expanded Memory (EMS)",
             "LIM Version: ",
      "Page Frame Address: ",
                   "Total: ",
               "Available: ",

    "XMS Information",
             "XMS Version: ",
          "Driver Version: ",
        "A20 Address Line: ",
        "High Memory Area: ",
               "Available: ",
      "Largest Free Block: ",
          "Available SXMS: ",
       "Largest Free SXMS: ",
      "Total Free XMS UMB: ",
    "Largest Free XMS UMB: ",

    "VCPI Information",
           "VCPI Detected: ",
                 "Version: ",
        "Available Memory: ",

    "DPMI Information",
           "DPMI Detected: ",
                 "Version: ",
  };

PSZ  pszNoPageFrame = "No Page Frame";
PSZ  pszEnabled     = "Enabled";
PSZ  pszNotEnabled  = "Not Enabled";
PSZ  pszError       = "Error";


/* Video strings */

PSZ  paszVideoTitles[] =
  {
        "Video Adapter Type: ",
              "Manufacturer: ",
                     "Model: ",
              "Display Type: ",
                "Video Mode: ",
         "Number of Columns: ",
            "Number of Rows: ",
        "Video BIOS Version: ",
                            "",
                            "",
           "Video BIOS Date: ",
    "VESA Support Installed: ",
              "VESA Version: ",
             "VESA OEM Name: ",
         "Secondary Adapter: ",
              "TIGA Version: ",
    "TIGA Interrupt/Address: ",
    NULL
  };


/* Network Types */

PSZ  paszNetworkTypes[] =
  {
    "No Network",
    "Unknown Network",
    "MS-NET Compatible",
    "LANMAN",
    "LANMAN Basic",
    "LANMAN Enhanced",
    "Novell",
    "Banyan",
    "LANtastic",
    "PC-NFS",
    "MS Workgroup Client",
    NULL
  };

PSZ  paszNetworkTitles[] =
  {
    /* Generic network strings */

            "Network Detected: ",
                "Network Name: ",
             "Network Version: ",
    "MS-DOS Network Functions: ",
               "Computer Name: ",
             "NetBIOS Present: ",
      "NetBIOS INT 5C Address: ",

    /* LANMAN strings */

                "Network Root: ",
                   "User Name: ",
/*            "Primary Domain: ", */
                   "Workgroup: ",
           "Server Connection: ",
            "Mailslot Support: ",
                 "API Support: ",
            "LAN Manager Date: ",
         "Current Patch Level: ",

    "NetBIOS Card Information:",
    "Workstation Services:",
    "PROTOCOL.INI Information:",
    "LANMAN.INI Driver Information:",

    /* Novell strings */

               "Shell Version: ",
            "Shell Located in: ",
                    "Shell OS: ",
            "Shell OS Version: ",
               "Hardware Type: ",
              "Station Number: ",
     "Physical Station Number: ",
               "IPX Installed: ",
               "SPX Installed: ",
           "ODI/LSL Installed: ",

    /* LANtastic strings */


            "LANtastic Server: ",
             "LANtastic Redir: ",
             "LANtastic Popup: ",
  };

PSZ pszSupported    = "Supported";
PSZ pszNotSupported = "Not Supported";


/* Operating System strings */

PSZ  paszOsVersionTitles[] =
  {
              "Operating System: ",
             "Internal Revision: ",
             "OEM Serial Number: ",
            "User Serial Number: ",
            "OEM Version String: ",
                "DOS Located in: ",
                    "Boot Drive: ",
        "DOSSHELL Task Switcher: ",
             "Microsoft Windows: ",
              "DESQview Version: ",
    "3270 ", /* Control Program: */
           "DoubleDOS Installed: ",
            "TaskView Installed: ",
               "TopView Version: ",
               "Path to Program: ",
    NULL
  };

PSZ  pszOs2                = "OS/2";
PSZ  pszMsDos              = "  Non MS-DOS";
PSZ  pszRom                = "ROM";
PSZ  pszHma                = "HMA";
PSZ  pszConventional       = "Conventional Memory";
PSZ  pszWin286             = "Windows/286";
PSZ  pszWin386             = "Windows/386";
PSZ  pszWindows            = "Windows";
PSZ  pszRealMode           = "Real Mode";
PSZ  pszStandardMode       = "Standard Mode";
PSZ  pszEnhancedMode       = "Enhanced Mode";
PSZ  psz_2dotX             = " 2.x";
PSZ  pszPCControlProgram   = "PC Control Program: ";
PSZ  pszWorkstationProgram = "Workstation Program: ";
PSZ  pszActive             = "Active";
PSZ  pszDosShell           = "DOSSHELL";
PSZ  pszEnvironmentStr     = "Environment Strings";
PSZ  pszEnvironInvalid     = "Environment string is invalid";
PSZ  pszTemp               = "TEMP";
PSZ  pszTmp                = "TMP";

PSZ  pszPathToProgram      = NULL;  /* Global variable -- stores argv[0] */
BOOL fTempPathInvalid      = FALSE;
BOOL fTmpPathInvalid       = FALSE;


/* Mouse strings */

PSZ  paszMouseMfgrs[] =
  {
    "No Mouse Driver",
    "Microsoft",
    "PC Mouse",
    "Logitech",
  };

PSZ  paszMouseTypes[] =
  {
    "Unknown Mouse Type",
    "Bus Mouse",
    "Serial Mouse",
    "InPort Mouse",
    "IBM PS/2 Mouse",
    "Hewlett-Packard Mouse",
    "Not Detected",
    "Logitech Serial Mouse",
    "Logitech PS/2 Mouse",
    "Microsoft PS/2 Mouse",
    "Ballpoint Mouse",
    "PS/2 Style Mouse"
  };

PSZ  paszMouseLanguages[] =
  {
    "English",
    "French",
    "Dutch",
    "German",
    "Swedish",
    "Finnish",
    "Spanish",
    "Portugese",
    "Italian"
  };

PSZ  paszMouseRatios[] =
  {
    "0.31",
   "0.062",
   "0.125",
    "0.25",
   "0.375",
     "0.5",
   "0.625",
    "0.75",
   "0.875",
       "1",
    "1.25",
     "1.5",
    "1.75",
       "2",
    "2.25",
     "2.5",
    "2.75",
       "3",
    "3.25",
     "3.5",
    NULL
  };

PSZ  paszDriverFileTypes[] =
  {
    "",
    ".COM File",
    ".SYS File"
  };

PSZ  paszMouseTitles[] =
  {
               "Mouse Hardware: ",
          "Driver Manufacturer: ",
              "DOS Driver Type: ",
             "Driver File Type: ",
           "DOS Driver Version: ",
     "Microsoft Driver Version: ",
                    "Mouse IRQ: ",
               "Mouse COM Port: ",
       "Mouse COM Port Address: ",
      "Number of Mouse Buttons: ",
       "Horizontal Sensitivity: ",
        "Mouse to Cursor Ratio: ",
         "Vertical Sensitivity: ",
        "Mouse to Cursor Ratio: ",
              "Threshold Speed: ",
               "Mouse Language: ",
            "Path to MOUSE.INI: ",
    NULL
  };


PSZ  pszBallPoint          = "BallPoint Mouse";
PSZ  pszDeviceNotDetected  = "Device Not Detected";
PSZ  pszColonOne           = " : 1";
PSZ  pszUndefined          = "Undefined";
PSZ  pszUnexplainedError   = "Non-fatal error detected in MSD.EXE: error %u\n(Please contact Windows 3.1 beta support)";


/* Other Adapter strings */

PSZ paszOtherTitles[] =
  {
        "Game Adapter: ",
      "Joystick A - X: ",
                   "Y: ",
            "Button 1: ",
            "Button 2: ",
      "Joystick B - X: ",
                   "Y: ",
            "Button 1: ",
            "Button 2: ",
/*
        "Sound Device: ",
           "Sound IRQ: ",
            "I/O Port: ",
*/
    NULL
  };

PSZ  pszGameAdapter        = "Game Adapter";
PSZ  pszDetected           = "Detected";
PSZ  pszNotDetected        = "Not Detected";


/* Disk Drive strings */

PSZ  pszDiskHeader         = "Drive  Type                                  Free Space  Total Size";
PSZ  pszDiskUnderline      = "-----  ------------------------------------  ----------  ----------";

PSZ  pszCylinders          = "%u Cylinders";
PSZ  pszHeads              = "%u Heads";
PSZ  pszBytesPerSector     = "%u Bytes/Sector";
PSZ  pszSectorsPerTrack    = "%u Sectors/Track";
PSZ  pszCommaSpace         = ", ";

PSZ  pszJoinInstalled      = "JOIN Installed";
PSZ  pszSubstInstalled     = "SUBST Installed";
PSZ  pszShareInstalled     = "SHARE Installed";
PSZ  pszAssignInstalled    = "ASSIGN Installed";
PSZ  pszAssign             = "ASSIGN";
PSZ  pszAppendInstalled    = "APPEND Installed";
PSZ  pszAppendPath         = "APPEND Path: ";
PSZ  pszMscdexInstalled    = "MSCDEX Version %u.%02u Installed";
PSZ  pszMscdex1x           = "MSCDEX Version %u.x Installed";
PSZ  pszLastdrive          = "LASTDRIVE=%c:";

PSZ  paszDriveTypes[] =
  {
    "Unknown Drive Type",
    "Floppy Drive",
    "Floppy Drive, 5.25\" 360K",
    "Floppy Drive, 5.25\" 1.2M",
    "Floppy Drive, 3.5\" 720K",
    "Floppy",
    "Floppy",
    "Fixed Disk",
    "Tape Drive",
    "Floppy Drive, 3.5\" 1.44M",
    "Optical Disk",
    "Floppy Drive, 3.5\" 2.88M",
    "Remote Drive",
    "RAM Disk",
    "CD-ROM Drive",
    "SUBST Drive",
    "ASSIGN Drive"
  };


/* IRQ data */

PSZ  pszIrqHeader    = "IRQ  Address    Description       Detected            Handled By";
PSZ  pszIrqUnderline = "---  ---------  ----------------  ------------------  ----------------";

PSZ  paszPcIrqDescriptions[] =
  {
    "Timer",
    "Keyboard",
    "I/O Channel",
    "COM2:, COM4:",
    "COM1:, COM3:",
    "Fixed Disk",
    "Floppy Disk",
    "LPT1:"
  };

PSZ  paszAtIrqDescriptions[] =
  {
    "Timer Click",
    "Keyboard",
    "Second 8259A",
    "COM2: COM4:",
    "COM1: COM3:\0COM4:",
    "LPT2:",
    "Floppy Disk",
    "LPT1:",
    "Real-Time Clock",
    "Redirected IRQ2",
    "(Reserved)",
    "(Reserved)",
    "(Reserved)",
    "Math Coprocessor",
    "Fixed Disk",
    "(Reserved)"
  };

PSZ  pszCom[] =
  {
    "",
    "COM1:",
    "COM2:",
    "COM3:",
    "COM4:"
  };

PSZ  pszBIOS               = "BIOS";


/* LPT strings */

PSZ  pszLptHeader1         = "         Port     On     Paper    I/O    Time";
PSZ  pszLptHeader2         = "Port   Address   Line     Out    Error    Out    Busy     ACK";
PSZ  pszLptUnderline       = "-----  -------   ----    -----   -----   ----    ----     ---";


/* COM strings */

PSZ  pszComHeader          = "COM1:      COM2:      COM3:      COM4:";
PSZ  pszComUnderline       = "-----      -----      -----      -----";

PSZ  psz1point5            = "1.5";
PSZ  pszNA                 = "N/A";
PSZ  pszOn                 = "On";
PSZ  pszOff                = "Off";

PSZ  paszComTitles[] =
  {
    "Port Address",
    "Baud Rate",
    "Parity",
    "Data Bits",
    "Stop Bits",
    "Carrier Detect (CD)",
    "Ring Indicator (RI)",
    "Data Set Ready (DSR)",
    "Clear To Send (CTS)",
    "UART Chip Used"
  };

PSZ  paszUartChips[] =   /* UART chip names */
  {
    "8250",
    "Unknown",
    "16550",
    "16550AF"
  };

PSZ  paszParityDesc[] = /* Parity descriptions */
  {
    "None",
    "Odd",
    "Even",
    "Mark",
    "Space"
  };


/* TSR strings */

PSZ  pszTsrHeader       = "Program Name        Address   Size   Command Line Parameters";
PSZ  pszTsrUnderline    = "------------------  -------  ------  --------------------------------";

PSZ  pszFreeMemory      = "Free Memory";
PSZ  pszDosSystemArea   = "System Area";
PSZ  pszCommandCom      = "COMMAND.COM";
PSZ  pszCommand         = "COMMAND";
PSZ  pszExcludedUmbArea = "Excluded UMB Area";
PSZ  pszVideoRom        = "  Video ROM BIOS";
PSZ  pszHardDiskRom     = "  Disk Controller";
PSZ  pszOptionRom       = "  Option ROM";
PSZ  pszVideoRam        = "  Video Memory";
PSZ  pszDosSystemCode   = "System Code";
PSZ  pszDosSystemData   = "System Data";
PSZ  pszDeviceAppenage  = "  Device Appendage";
PSZ  pszFileHandles     = "  File Handles";
PSZ  pszFCBS            = "  FCBS";
PSZ  pszBuffers         = "  BUFFERS";
PSZ  pszDirectories     = "  Directories";
PSZ  pszStacksArea      = "  Default Handlers";
PSZ  pszIntTable        = "Interrupt Table";
PSZ  pszRomDataArea     = "ROM Data Area";
PSZ  pszDosDataArea     = "System Data Area";


/* Device driver strings */

PSZ  pszDeviceHeader    = "Device        Filename  Units    Header      Attributes";
PSZ  pszDeviceUnderline = "------------  --------  -----  ---------  ----------------";

PSZ  pszBlockDevice     = "Block Device";


/* File Viewer strings */

PSZ  pszDot             = ".";
PSZ  pszDotDot          = "..";


PSZ  pszPathNotThere    = "Path does not exist";


PSZ  paszButtonNames[] =
  {
    "",
    "MSD Version Number",
    "Customer Information",
    "Summary Information",
    "Computer",
    "Memory",
    "Video",
    "Network",
    "OS Version",
    "Mouse",
    "Other Adapters",
    "Disk Drives",
    "LPT Ports",
    "COM Ports",
    "IRQ Status",
    "TSR Programs",
    "Device Drivers",
  };

PSZ  paszMainBtnArray[] =
  {
    "Computer...",
    "Memory...",
    "Video...",
    "Network...",
    "OS Version...",
    "Mouse...",
    "Other Adapters...",
    "Disk Drives...",
    "LPT Ports...",
    "COM Ports...",
    "IRQ Status...",
    "TSR Programs...",
    "Device Drivers...",
  };

SUMMARY_STRUCT *pSum;


#ifdef CW_INCLUDED

PSZ  paszCommandLineHelp[] =
  {
"Provides detailed technical information about your computer.\n"
"\n"
"MSD [/I] [/F[drive:][path]filename] [/P[drive:][path]filename]\n"
"    [/S[drive:][path][filename]]\n"
"\n"
"MSD [/B][/I]\n"
"\n"
"  /B                         Runs MSD using a black and white color scheme.\n"
"  /I                         Bypasses initial hardware detection.\n"
"  /F[drive:][path]filename   Requests input and writes an MSD report to the\n"
"                             specified file.\n"
"  /P[drive:][path]filename   Writes an MSD report to the specified file\n"
"                             without first requesting input.\n"
"  /S[drive:][path][filename] Writes a summary MSD report to the specified\n"
"                             file. If no filename is specified, output is to\n"
"                             the screen.\n"
"\n"
"Use MSD [/B] [/I] to examine technical information through the MSD interface.\n",
    NULL
  };

#else /* CW_INCLUDED */

PSZ  paszCommandLineHelp[] =
  {
"Provides detailed technical information about your computer.\n"
"\n"
"MSD [/S[drive:][path][filename]]\n"
"\n"
"  /S[drive:][path][filename] Writes a summary MSD report to the specified\n"
"                             file. If no filename is specified, output is to\n"
"                             the screen.\n",
    NULL
  };

#endif /* CW_INCLUDED */


/* Box description for the info window */

#ifdef CW_INCLUDED

BOX boxInfoBox =
  {
    'þ',
    '¿',
    'À',
    'Ù',
    'Í',
    'Ä',
    '³',
    '³'
  };

PBOX pboxInfoBox = &boxInfoBox;

#endif /* CW_INCLUDED */


/* Title lines, displayed during the /F report to file */

PSZ  paszMsdTitleLines[] =
  {
    "\nMSD Microsoft Diagnostics Version " MSD_VERSION_NUMBER,
    "Copyright (C) Microsoft Corporation 1990-92\n",
    "Please enter your Name, Company Name,",
    "Address (two lines), City, State, ZIP code",
    "telephone number, and comment on the lines below:\n",
    NULL
  };

PSZ  paszCustInfoTitle[] =
  {
    "        Name: ",
    "Company Name: ",
    "    Address1: ",
    "    Address2: ",
    " City/ST/Zip: ",
    "     Country: ",
    "       Phone: ",
    "    Comments: ",
    NULL
  };

CUSTINFO *pCustInfoDlg;   /* Customer information for the dialog box */


/* Record Types */

WORD rwRecordTypes[] =
  {
    IDI_ALL_RECORDS,
    IDI_MSD_HEADER_RECORD,
    IDI_CUSTOMER_INFORMATION,
    IDI_SUMMARY_SCREEN,
    IDI_COMPUTER_RECORD,
    IDI_MEMORY_RECORD,
    IDI_VIDEO_RECORD,
    IDI_NETWORK_RECORD,
    IDI_OS_VERSION_RECORD,
    IDI_MOUSE_RECORD,
    IDI_OTHER_ADAPTERS_RECORD,
    IDI_DISK_DRIVE_RECORD,
    IDI_LPT_RECORD,
    IDI_COM_RECORD,
    IDI_IRQ_RECORD,
    IDI_TSR_PROGRAMS_RECORD,
    IDI_DEVICE_DRIVERS_RECORD,
    0
  };

WORD wMaxRecords = 15;

WORD rwIrqRecordTypes[] =
  {
    IDI_COMPUTER_RECORD,
    IDI_MOUSE_RECORD,
    IDI_DISK_DRIVE_RECORD,
    IDI_LPT_RECORD,
    IDI_COM_RECORD,
    0
  };


/* Language ID */

LANGUAGE_ID rgLang[] =
  {
    { 0x0401, "Arabic" },
    { 0x0402, "Bulgarian" },
    { 0x0403, "Catalan" },
    { 0x0404, "Traditional Chinese" },
    { 0x0804, "Simplified Chinese" },
    { 0x0405, "Czech" },
    { 0x0406, "Danish" },
    { 0x0407, "German" },
    { 0x0807, "Swiss German" },
    { 0x0408, "Greek" },
    { 0x0409, "U.S. English" },
    { 0x0809, "U.K. English" },
    { 0x040A, "Castilian Spanish" },
    { 0x080A, "Mexican Spanish" },
    { 0x040B, "Finnish" },
    { 0x040C, "French" },
    { 0x080C, "Belgian French" },
    { 0x0C0C, "Canadian French" },
    { 0x100C, "Swiss French" },
    { 0x040D, "Hebrew" },
    { 0x040E, "Hungarian" },
    { 0x040F, "Icelandic" },
    { 0x0410, "Italian" },
    { 0x0810, "Swiss Italian" },
    { 0x0411, "Japanese" },
    { 0x0412, "Korean" },
    { 0x0413, "Dutch" },
    { 0x0813, "Belgian Dutch" },
    { 0x0414, "Norwegian - Bokmal" },
    { 0x0814, "Norwegian - Nynorsk" },
    { 0x0415, "Polish" },
    { 0x0416, "Brazilian Portuguese" },
    { 0x0417, "Rhaeto-Romanic" },
    { 0x0418, "Romanian" },
    { 0x0419, "Russian" },
    { 0x041A, "Croato-Serbian (Latin)" },
    { 0x081A, "Serbo-Croatian (Cyrillic)" },
    { 0x041B, "Slovak" },
    { 0x041C, "Albanian" },
    { 0x041D, "Swedish" },
    { 0x041E, "Thai" },
    { 0x041F, "Turkish" },
    { 0x0420, "Urdu" },
    { 0x0421, "Bahasa" },
    { 0xFFFF, "Unknown" }
  };


CODEPAGE_ID rgCP[] =
  {
    {  932, "Windows, Japan (Shift-JIS X-0208)" },
    {  949, "Windows, Korea (Shift-KSC 5601)" },
    {  950, "Windows, Taiwan (GB5)" },
    { 1200, "Unicode" },
    { 1250, "Windows, Latin-2 (Easter European)" },
    { 1251, "Windows, Cryillic" },
    { 1252, "Windows, Multilingual (ANSI)" },
    { 1253, "Windows, Greek" },
    { 1254, "Windows, Turkish" },
    { 1255, "Windows, Hebrew" },
    { 1256, "Windows, Arabic" },
    {65535, "Unknown" }
  };
