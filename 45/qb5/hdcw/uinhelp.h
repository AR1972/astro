/***
*uinhelp.h - Defines and data for the New Help System
*
*   Copyright <C> 1985-1988 Microsoft Corporation
*
*Purpose:
* NOTE: When making changes to this file, be sure to make equivalent
*   changes to file UINHELP.INC
*
*******************************************************************************/

#define UINHELP_H   /* flag that file has already been included */


/* These constants define the source of calls to Help().  Each type of caller
 * passes parameters in a different way and expects the text to be displayed
 * differently
 */

#define hemWindowHid    hemUserMin+1    /* Display help (ID = hid) in window */
#define hemAllWindow    hemUserMin  /* F1 pressed in any window */


#define WM_FIRSTHELPMSG WM_USER+20  //[8] WM_USER+15 is the last editmgr
                    //[8] message.Use +20 in case of change

#define WM_HELPBACK WM_FIRSTHELPMSG
#define WM_HELPNEXT WM_FIRSTHELPMSG + 1
#define WM_HELPDISP WM_FIRSTHELPMSG + 2
#define WM_HELPCURTOPIC WM_FIRSTHELPMSG + 3
#define WM_HELPNXTTOPIC WM_FIRSTHELPMSG + 4
#define WM_HELPCHGTOPIC WM_FIRSTHELPMSG + 5
#define WM_HELPRESTORETOPIC  WM_FIRSTHELPMSG + 6    //[7]
#define WM_HELPTITLE    WM_FIRSTHELPMSG + 7
#define WM_HELPLINE WM_FIRSTHELPMSG + 8
#define WM_HELPATTR WM_FIRSTHELPMSG + 9
#define WM_HELPFILESIZE WM_FIRSTHELPMSG + 10

#define NUM_HELPMSG 11              //[7]

// a-emoryh - Max size of command-line topic, in Dos6 QHelp.
#define cbmaxTOPIC  40
