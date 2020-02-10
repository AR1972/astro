/*
 *  This file contains help context numbers. Each of the values refers to
 *  a dialog. The HELPCONTEXT dialog control defined below is used to place
 *  help context information into dialog templates.
 *
 */

#define HELPCONTEXT(n)            LTEXT "", n, 0, 0, 0, 0

/*
 *  These are the help context values for each of the dialogs that contain
 *  help context information.
 */

/* Help screen context numbers. */

#define  DOS2GUI_INDEX             0x1000

#define  HLP_BADDOSEXIT            0x1001
#define  HLP_UNINSTALL             0x1002
#define  HLP_FMTTYPE               0x1003
#define  HLP_EXPRESSINSERTDISK     0x1004
#define  HLP_UNINSTALLINFO         0x1005

/*  Min and max Context ID values, we use these in the code to validate
 *  the help context numbers.
 */

#define  HELP_CONTEXT_MIN          0x1001  // Set me to the smallest help ID.
#define  HELP_CONTEXT_MAX          0x1005  // Set me to the largest help ID.
