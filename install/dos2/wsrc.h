/*  defines for Janus resources live here. */

/*  These two window classes are used for the progress dialog and for
 *  dialogs needing sub-classed (code-free!) help and/or exit processing.
 */
#define PRO_CLASS      "PRO"
#define CLS_MYDLGS     "mydlg"

/*                                  
 *  Dialog control ID's             
 */
#define ID_NULL        -1  
#define ID_EDIT        11
#define ID_TEXT        12
#define WM_MYSETFOCUS  3999

/* STATIC TEXT FIELD ID's */

#define ID_STATUS0     4000
#define ID_STATUS1     (ID_STATUS0 + 1)
#define ID_STATUS2     (ID_STATUS0 + 2)
#define ID_STATUS3     (ID_STATUS0 + 3)
#define ID_STATUS4     (ID_STATUS0 + 4)
#define ID_STATUS5     (ID_STATUS0 + 5)           

/* BUTTON ID's */

#define ID_OK          1 /* These are equal to the defines in windows.h */
#define ID_CANCEL      2 /* These are equal to the defines in windows.h */
#define ID_ABORT       4
#define ID_RETRY       5
#define ID_IGNORE      6
#define ID_HELP        4100
#define ID_EXIT		  4101
#define ID_REBOOT      4112

/* RADIO BUTTON ID's */

#define ID_SEARCH       4300
#define ID_SPECIFY      4301
#define ID_MODIFY       4302
#define ID_REVIEW       4303
#define ID_NOMODIFY     4304
#define ID_HIGH_DENSITY 4305
#define ID_LOW_DENSITY  4306

/* Miscelaneous defines */

#define ID_EXITSETUP          8233  // Exit Setup Message, exit now! Really!

/* dialog resource IDs */

#define DLG_EXPRESSINSERTDISK 17
#define DLG_PROGRESS          18
#define DLG_COPYERROR         19
#define DLG_UNINSTALL         35
#define DLG_FMTTYPE           36
#define DLG_FMTSTATUS         39
#define DLG_AUTOSTART         43
#define DLG_DOSREBOOT         44
#define DLG_DOSCONFIGSYS      45
#define DLG_BADDOSEXIT        46
#define DLG_UNINSTALLINFO     47

/* strings IDs */

#define IDS_PLEASE_INSERT      306

#define 	IDS_COPYING     		 317
#define 	IDS_WAITCOPY    		 319
#define  IDS_WAITCOPYRECHD		 320
#define  IDS_WAITCOPYRECFL		 321
#define	IDS_WAITOLDDOS	 		 322
#define  IDS_WAITDEL				 323
#define	IDS_WAITREN				 324
#define  IDS_DISKS             325
#define  IDS_OEMDISKS          326
#define 	IDS_OUTOFDISK	 		 359
#define  IDS_INTO_DRIVE        487
#define  IDS_DOSCPYERROR       500  // Leave 500 - 507 unused. See dos2.rc line 172 for reason.
#define  IDS_FATAL_CAPTION     600
#define  IDS_NONFATAL_CAPTION  601
#define  IDS_NOTREADY          602
#define  IDS_WRITEPROTECT      603
#define	IDS_FERROR      		 700
#define  IDS_FERROREXIT  		 750
#define  IDS_FERRORABORT 		 751
#define	IDS_PERCENT		 		 760
#define	IDS_CPYERRPREFIX 		 770
#define  IDS_HELPFILE          775
#define  IDS_HELP_ERROR        777
#define  IDS_DOSERROPT   		 780
#define	IDS_DSKPREPERR  		 790
#define  IDS_DOS         		 799
#define  IDS_FSTATUS     		 800
#define  IDS_DOSREADME   		 810
#define  IDS_END_README        (IDS_DOSREADME  + 9)

#define  IDS_KB1200            900
#define  IDS_KB360             901

/* Misc #defines for DOS/Windows merge */

#define  ID_AUTOSTART    ID_STATUS0
#define	MAX_DOSCPYERR   6
#define  MAX_DSKPREPERR  3

/*
 *  String ID's (These are in groups of 16, as they will be stored by USER)
 */

#define IDS_EXITNOTSETUP         355
#define IDS_EXITCAP              357
#define IDS_HELP                1000
#define IDS_ERROR              20000


