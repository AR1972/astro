;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1983 - 1991
; *                      All Rights Reserved.
; */
/******************************************************************************
*
*  Change Log:
*
*    Date    Who   #                      Description
*  --------  ---  ---  ------------------------------------------------------
*  03/05/90  EGH  C01  Problem fixed - Some added messages for deleting more
*                      than one primary partition were defined in the code
*                      and were not being properly displayed.  Fix was to
*                      remove them from the code and put them in the standard
*                      internationalized form.  This resulted in the creation
*                      of menu_55, menu_56, and error_37.
*  03/08/90  EGH  C08  Problem fixed - The "Change current fixed disk drive"
*                      menu was very hard to read.  Fix was to make the column
*                      labels shorter and rearrange the layout slightly.  This
*                      resulted in changing menu_47 - menu_54.
*  03/09/90  EGH  C10  Problem fixed - The use of highlighting was somewhat
*                      inconsistent.  Fix was to change the hightlighting of
*                      various menus.
*  03/22/90  EGH  C14  Added support for deleting non-DOS partitions.  This
*                      resulted in the creation of menu_57, menu_58, menu_59,
*                      status_13, error_38, and error_39.
*  03/22/90  EGH  C15  Added code to recognize additional partition types.
*  05/24/90  EGH  C20  Problem fixed - a second reboot screen was added that
*                      does not instruct the user to insert a bootable DOS
*                      diskette.  This resulted in the creation of menu_60.
*  05/25/90  EGH  C21  Added a prompt for the volume label when deleting a
*                      primary DOS partition to be consistent with deleting
*                      logical DOS drives.
*  08/27/90  EGH  C25  Added support for displaying the volume label when
*                      creating/deleting primary partitions.
*  12/13/90  EGH  C35  Header messages at the top of the screens should not
*                      be centered because this may not leave enough room
*                      for international translations.
*
******************************************************************************/
/*           FDISK MESSAGE FILE                                         */

/************************************************************************/
/* Please log all modifications to this file:                           */
/*----------------------------------------------------------------------*/
/* Date: 04/04/86                                                       */
/* Changed by: Mark T	                                                */
/* Message changed: menu_1 - menu_38                                    */
/* Reason: Creation of file                                             */
/*----------------------------------------------------------------------*/
/* Date: 05/04/87                                                       */
/* Changed by: Dennis M	                                                */
/* Message changed: menu_1 - menu_44                                    */
/* Reason: DOS 3.3						        */
/*----------------------------------------------------------------------*/
/* Date:                                                                */
/* Changed by:                                                          */
/* Message changed:                                                     */
/* Reason:                                                              */
/*----------------------------------------------------------------------*/
/***********************************************************************/

/************************************************************************/
/*  FDISK MESSAGES                                                      */
/*                                                                      */
/* Portions of the screen that are handled in the msg are indicated on  */
/* the listing of the screen with the message name given. If the text   */
/* message is defined in another screen, then the name is followed by   */
/* a "#" character                                                      */
/*                                                                      */
/* NOTE TO TRANSLATORS                                                  */
/* The characters inside the <> and the  are control characters and   */
/* should not be translated. The Control characters are defined as      */
/* follows:                                                             */
/*                                                                      */
/* <H> - Highlight the following text                                   */
/* <R> - Regular text                                                   */
/* <U> - Underline the following text                                   */
/* <B> - Blink the following text                                       */
/* <O> - Turn off Blink                                                 */
/* <Y> - Print YES character, as set by define                          */
/* <N> - Print NO character, as set by define                           */
/* <W> - Sound the beep                                                 */
/* <S> - Save cursor position for later use                             */
/* <G> - Cursor position left justified and regular proceed to right    */
/* <C> - Clear the screen out from control char to end of line          */
/* <I> - Insert character from Insert[] string. This string must be set */
/*       up prior to displaying the message. The first <I> will insert  */
/*       Insert[0], the second Insert[1], etc....This will move the     */
/*       cursor one postition. The Insert%% string will be initialized  */
/*                                                                      */
/*                                                                      */
/*  Multiple control characters can be between the <>.                  */
/*                                                                      */
/*  The  indicates Row and column for the text and has the format of  */
/*  rrcc where the numbers are decimal and zero based .first row/col  */
/*  is 00. The numbers are in decimal, and must be 2 characters, which  */
/*  means rows/cols 0-9 should be listed as 00-09. For example, the 5th */
/*  row, 3rd column on the screen would be listed as 0402.            */
/*                                                                      */
/*  The column number is always the column desired. The row number is   */
/*  an offset from the previous row. For example, if the text just      */
/*  printed is on row 6, and the next text should be printed 2 rows     */
/*  down in column 0, then the control strin would be 0201. The first */
/*  row specified in the message is assumed to be based off of row 0,   */
/*  it would actually specify the actual row for the start of the msg   */
/*  to be printed.                                                      */
/*                                                                      */
/* NOTE: ALWAYS SAVE THIS FILE WITH NO TABS CHARACTERS                  */
/************************************************************************/


/************************************************************************/
/*                                                                      */
/* Define Area for text variables                                       */
/*                                                                      */
/************************************************************************/

#define ACTIVE_PART  'A'   /* Character to indicate active status */
#define DRIVE_INDICATOR ':' /* Character displayed to indicate drive letter */

/*                                                                      */
/*                                                                      */
/* The following character strings are required to display the          */
/* menu screens for FDISK. The messages have a label type of: menu_xx   */
/*                                                                      */
/*                                                                      */

/*******************************************************************************************************/
/*  Screen for DO_MAIN_MENU                                                                            */
/*                                                                                                     */
/*       ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*       ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*     ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*     00³                               MS-DOS Version 6                                ³menu_1      */
/*     01³                            Fixed Disk Setup Program                      ³menu_1      */
/*     02³                    (C)Copyright Microsoft Corp. 1983 - 1993                        ³menu_1      */
/*     03³                                                                                ³            */
/*     04³                                 FDISK Options                                  ³menu_2      */
/*     05³                                                                                ³            */
/*     06³    Current fixed disk drive: #                                                 ³menu_5      */
/*     07³                                                                                ³            */
/*     08³    Choose one of the following:                                                ³menu_3      */
/*     09³                                                                                ³            */
/*     10³    1.  Create DOS Partition or Logical DOS Drive                               ³menu_2      */
/*     11³    2.  Set active partition                                                    ³menu_2      */
/*     12³    3.  Delete DOS Partition or Logical DOS Drive                               ³menu_2      */
/*     13³    4.  Display partition information                                           ³menu_2      */
/*     14³    5.  Change current fixed disk drive                                            ³menu_4      */
/*     15³                                                                                ³            */
/*     16³                                                                                ³            */
/*     17³    Enter choice: [#]                                                           ³menu_7      */
/*     18³                                                                                ³            */
/*     19³                                                                                ³            */
/*     20³    Warning! No partitions are set active - disk 1 is not startable unless      ³menu_6      */
/*     21³    a partition is set active.                                                  ³            */
/*     22³                                                                                ³            */
/*     23³                                                                                ³            */
/*     24³    Press ESC to exit FDISK                                                     ³menu_2      */
/*     ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/*******************************************************************************************************/


/*C35----------------------------------------------------------*/
 char far *menu_1 =

"0004<R>1001\
 0104<R>1002\
 0104<R>1003";



/*C35----------------------------------------------------------*/
 char far *menu_2 =

#ifdef JAPAN
"0404<H>1004\
 0604<H>1005\
 0104<H>1006\
 0104<H>1007\
 0104<H>1008\
 1004<R>1009";
#else
"0404<H>1004\
 0604<H>1005\
 0104<H>1006\
 0104<H>1007\
 0104<H>1008\
 1104<R>1009";
#endif

/*-------------------------------------------------------------*/
 char far *menu_3 =

"0804<R>1010";

/*-------------------------------------------------------------*/
 char far *menu_4 =

"1404<H>1011";

/*-------------------------------------------------------------*/
 char far *menu_5 =

"0604<R>1012";

/*-------------------------------------------------------------*/
 char far *menu_6 =

"2004<H>1013\
 0104<R>1014";

/*C10----------------------------------------------------------*/
 char far *menu_7 =

"1704<R>1015";

/*******************************************************************************************************/
/*  Screen for CREATE_PARTITION                                                                        */
/*                                                                                                     */
/*       ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*       ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*     ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*     00³                                                                                ³            */
/*     01³                                                                                ³            */
/*     02³                                                                                ³            */
/*     03³                                                                                ³            */
/*     04³                  Create DOS Partition or Logical DOS Drive                     ³menu_8      */
/*     05³                                                                                ³            */
/*     06³    Current fixed disk drive: #                                                 ³menu_5 #    */
/*     07³                                                                                ³            */
/*     08³    Choose one of the following:                                                ³menu_3 #    */
/*     09³                                                                                ³            */
/*     10³    1.  Create Primary DOS Partition                                            ³menu_9      */
/*     11³    2.  Create Extended DOS Partition                                           ³menu_9      */
/*     12³    3.  Create logical DOS Drive(s) in the Extended DOS Partition               ³menu_10     */
/*     13³                                                                                ³            */
/*     14³                                                                                ³            */
/*     15³                                                                                ³            */
/*     16³                                                                                ³            */
/*     17³    Enter choice: [ ]                                                           ³menu_7 #    */
/*     18³                                                                                ³            */
/*     19³                                                                                ³            */
/*     20³                                                                                ³            */
/*     21³                                                                                ³            */
/*     22³                                                                                ³            */
/*     23³                                                                                ³            */
/*     24³    Press ESC to return to FDISK Options                                        ³menu_11     */
/*     ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/*******************************************************************************************************/

/*C35----------------------------------------------------------*/
 char far *menu_8 =

"0404<H>1016";

/*-------------------------------------------------------------*/
 char far *menu_9 =

"1004<H>1017\
 0104<H>1018";

/*-------------------------------------------------------------*/
 char far *menu_10 =

"1204<H>1019";

/*-------------------------------------------------------------*/
 char far *menu_11 =

#ifdef JAPAN
"2304<R>1020";
#else
"2404<R>1020";
#endif


/*******************************************************************************************************/
/*  Screen for DOS_CREATE_PARTITION                                                                    */
/*                                                                                                     */
/*       ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*       ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*     ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*     00³                                                                                ³            */
/*     01³                                                                                ³            */
/*     02³                                                                                ³            */
/*     03³                                                                                ³            */
/*     04³                           Create Primary DOS Partition                         ³menu_12     */
/*     05³                                                                                ³            */
/*     06³    Current fixed disk drive: #                                                 ³menu_5 #    */
/*     07³                                                                                ³            */
/*     08³    Do you wish to use the maximum available size for a Primary DOS Partition   ³menu_13     */
/*     09³    and make the partition active (Y/N).....................? [Y]               ³menu_13     */
/*     10³                                                                                ³            */
/*     11³                                                                                ³            */
/*     12³                                                                                ³            */
/*     13³                                                                                ³            */
/*     14³                                                                                ³            */
/*     15³                                                                                ³            */
/*     16³                                                                                ³            */
/*     17³                                                                                ³            */
/*     18³                                                                                ³            */
/*     19³                                                                                ³            */
/*     20³                                                                                ³            */
/*     21³                                                                                ³            */
/*     22³                                                                                ³            */
/*     23³                                                                                ³            */
/*     24³    Press ESC to return to FDISK Options                                        ³menu_11     */
/*     ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/*******************************************************************************************************/

/*C35----------------------------------------------------------*/
 char far *menu_12 =

"0404<H>1021";

/*-------------------------------------------------------------*/
 char far *menu_13 =

"0804<R>1022\
 0104<R>1023";

/*-------------------------------------------------------------*/
 char far *menu_45 =

"0804<R>1022\
 0104<R>1024";


/*******************************************************************************************************/
/*  Screen for INPUT_DOS_CREATE                                                                        */
/*                                                                                                     */
/*       ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*       ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*     ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*     00³                                                                                ³            */
/*     01³                                                                                ³            */
/*     02³                                                                                ³            */
/*     03³                                                                                ³            */
/*     04³                           Create Primary DOS Partition                         ³menu_12 #   */
/*     05³                                                                                ³            */
/*     06³    Current fixed disk drive: #                                                 ³menu_5 #    */
/*     07³                                                                                ³            */
/*     08³    Partition Status   Type    Size in Mbytes   Percentage of Disk Used         ³menu_14     */
/*     09³     ## #        #    #######       ####        ###%                            ³            */
/*     10³     ## #        #    #######       ####        ###%                            ³            */
/*     11³     ## #        #    #######       ####        ###%                            ³            */
/*     12³     ## #        #    #######       ####        ###%                            ³            */
/*     13³                                                                                ³            */
/*     14³    Total disk space is #### Mbytes (1 Mbyte = 1048576 bytes)                   ³menu_15     */
/*     15³    Maximum space available for partition is #### Mbytes (###%)                 ³menu_16     */
/*     16³                                                                                ³            */
/*     17³                                                                                ³            */
/*     18³    Enter partition size in Mbytes or percent of disk space (%) to              ³menu_39     */
/*     19³    create a Primary DOS Partition..................................[####]      ³            */
/*     20³                                                                                ³            */
/*     21³                                                                                ³            */
/*     22³                                                                                ³            */
/*     23³                                                                                ³            */
/*     24³    Press ESC to return to FDISK Options                                        ³menu_11     */
/*     ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/****************************************************************/

/*C10-C25------------------------------------------------------*/
 char far *menu_14 =

"0804<R>1025\
 0104<R> <II> <HI>         <RI>    <IIIIIII>   <IIIIIIIIIII>   <IIII>   <IIIIIIII>   <IIII>\
 0104<R> <II> <HI>         <RI>    <IIIIIII>   <IIIIIIIIIII>   <IIII>   <IIIIIIII>   <IIII>\
 0104<R> <II> <HI>         <RI>    <IIIIIII>   <IIIIIIIIIII>   <IIII>   <IIIIIIII>   <IIII>\
 0104<R> <II> <HI>         <RI>    <IIIIIII>   <IIIIIIIIIII>   <IIII>   <IIIIIIII>   <IIII>";

/*-------------------------------------------------------------*/
 char far *menu_15 =

"1404<R>1026";

/*-------------------------------------------------------------*/
 char far *menu_16 =

"1504<RC>1027";

/*-------------------------------------------------------------*/
 char far *menu_39 =

"1804<RC>1028\
 0104<RC>1029";


/****************************************************************************************************/
/*  Screen for EXT_CREATE_PARTITION                                                                 */
/*                                                                                                  */
/*    ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*    ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*  ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*  00³                                                                                ³            */
/*  01³                                                                                ³            */
/*  02³                                                                                ³            */
/*  03³                                                                                ³            */
/*  04³                           Create Extended DOS Partition                        ³menu_17     */
/*  05³                                                                                ³            */
/*  06³    Current fixed disk drive: #                                                 ³menu_5 #    */
/*  07³                                                                                ³            */
/*  08³    Partition Status   Type    Size in Mbytes   Percentage of Disk Used         ³menu_14 #   */
/*  09³     ##  #       #   #######       ####                   ###%                  ³            */
/*  10³     ##  #       #   #######       ####                   ###%                  ³            */
/*  11³     ##  #       #   #######       ####                   ###%                  ³            */
/*  12³     ##  #       #   #######       ####                   ###%                  ³            */
/*  13³                                                                                ³            */
/*  14³    Total disk space is #### Mbytes (1 Mbyte = 1048576 bytes)                   ³menu_15 #   */
/*  15³    Maximum space available for partition is #### Mbytes (##%)                  ³menu_16 #   */
/*  16³                                                                                ³            */
/*  17³                                                                                ³            */
/*  18³    Enter partition size in Mbytes or percent of disk space (%) to              ³menu_42 #   */
/*  19³    create an Extended DOS Partition................................[####]      ³            */
/*  20³                                                                                ³            */
/*  21³                                                                                ³            */
/*  22³                                                                                ³            */
/*  23³                                                                                ³            */
/*  24³    Press ESC to continue                                                       ³menu_46     */
/*  ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/***************************************************************/

/*C35----------------------------------------------------------*/
 char far *menu_17 =

"0404<H>1030";

/*-------------------------------------------------------------*/
 char far *menu_42 =

"1804<RC>1028\
 0104<RC>1031";


/*-------------------------------------------------------------*/
 char far *menu_46 =

#ifdef JAPAN
"2304<R>1032";
#else
"2404<R>1032";
#endif




/*****************************************************************************************************/
/*  Screen for VOLUME_CREATE                                                                         */
/*                                                                                                   */
/*     ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*     ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*   ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*   00³                                                                                ³            */
/*   01³            Create Logical DOS Drive(s) in the Extended Partition               ³menu_18     */
/*   02³                                                                                ³            */
/*   03³Drv Volume Label  Mbytes  System  Usage  Drv Volume Label  Mbytes  System  Usage³menu_19/20  */
/*   04³##  ###########    ####  ########  ###%  ##  ###########    ####  ########  ###%³            */
/*   05³##  ###########    ####  ########  ###%  ##  ###########    ####  ########  ###%³            */
/*   06³##  ###########    ####  ########  ###%  ##  ###########    ####  ########  ###%³            */
/*   07³##  ###########    ####  ########  ###%  ##  ###########    ####  ########  ###%³            */
/*   08³##  ###########    ####  ########  ###%  ##  ###########    ####  ########  ###%³            */
/*   09³##  ###########    ####  ########  ###%  ##  ###########    ####  ########  ###%³            */
/*   10³##  ###########    ####  ########  ###%  ##  ###########    ####  ########  ###%³            */
/*   11³##  ###########    ####  ########  ###%  ##  ###########    ####  ########  ###%³            */
/*   12³##  ###########    ####  ########  ###%  ##  ###########    ####  ########  ###%³            */
/*   13³##  ###########    ####  ########  ###%  ##  ###########    ####  ########  ###%³            */
/*   14³##  ###########    ####  ########  ###%  ##  ###########    ####  ########  ###%³            */
/*   15³##  ###########    ####  ########  ###%                                         ³            */
/*   16³                                                                                ³            */
/*   17³    Total Extended DOS partition size is #### Mbytes (1 Mbyte = 1048576 bytes)  ³menu_17     */
/*   18³    Maximum space available for logical drive is #### Mbytes (###%)             ³menu_22     */
/*   19³                                                                                ³            */
/*   20³    Enter logical drive size in Mbytes or percent of disk space (%)...[####]    ³menu_40     */
/*   21³                                                                                ³            */
/*   22³                                                                                ³            */
/*   23³                                                                                ³            */
/*   24³    Press ESC to return to FDISK Options                                        ³menu_11     */
/*   ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/***************************************************************/

/*C35----------------------------------------------------------*/
 char far *menu_18 =

"0104<HC>1033";

/*C10----------------------------------------------------------*/
 char far *menu_19 =

"0300<R>1034\
0100<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0100<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0100<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0100<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0100<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0100<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>";

/*----------------------------------------------------------*/
 char far *menu_43 =

"1000<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0100<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0100<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0100<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0100<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0100<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>";

/*C10----------------------------------------------------------*/
 char far *menu_20 =

"0341<R>1034\
0141<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0141<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0141<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0141<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0141<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0141<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>";

/*---------------------------------------------------------*/
 char far *menu_44 =

"1041<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0141<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0141<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0141<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>\
0141<H><II>  <RIIIIIIIIIII>    <IIII>  <IIIIIIII>  <IIII>";

/*-------------------------------------------------------------*/
 char far *menu_21 =

"1704<RC>1035";

/*-------------------------------------------------------------*/
 char far *menu_22 =

"1804<RC>1036";

/*-------------------------------------------------------------*/
 char far *menu_40 =

"2004<RC>1037";


/*****************************************************************************************************/
/*  Screen for CHANGE_ACTIVE_PARTITION                                                               */
/*                                                                                                   */
/*     ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*     ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*   ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*   00³                                                                                ³            */
/*   01³                              Set Active Partition                              ³menu_23     */
/*   02³                                                                                ³            */
/*   03³                                                                                ³            */
/*   04³                                                                                ³            */
/*   05³                                                                                ³            */
/*   06³    Current fixed disk drive: #                                                 ³menu_5 #    */
/*   07³                                                                                ³            */
/*   08³    Partition Status   Type    Size in Mbytes   Percentage of Disk Used         ³menu_14 #   */
/*   09³     ## #        #   #######       ####         ###%                            ³            */
/*   10³     ## #        #   #######       ####         ###%                            ³            */
/*   11³     ## #        #   #######       ####         ###%                            ³            */
/*   12³     ## #        #   #######       ####         ###%                            ³            */
/*   13³                                                                                ³            */
/*   14³    Total disk space is #### Mbytes (1 Mbyte = 1048576 bytes)                   ³menu_15 #   */
/*   15³                                                                                ³            */
/*   16³    Enter the number of the partition you want to make active............:[#]   ³menu_24     */
/*   17³                                                                                ³            */
/*   18³                                                                                ³            */
/*   19³                                                                                ³            */
/*   20³                                                                                ³            */
/*   21³                                                                                ³            */
/*   22³                                                                                ³            */
/*   23³                                                                                ³            */
/*   24³    Press ESC to return to FDISK Options                                        ³menu_11     */
/*   ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/*****************************************************************************************************/

/*C35----------------------------------------------------------*/
 char far *menu_23 =

"0404<H>1038";

/*-------------------------------------------------------------*/
 char far *menu_24 =

"1604<R>1039";


/*****************************************************************************************************/
/*  Screen for DELETE_PARTITION                                                                      */
/*                                                                                                   */
/*     ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*     ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*   ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*   00³                                                                                ³            */
/*   01³                                                                                ³            */
/*   02³                                                                                ³            */
/*   03³                                                                                ³            */
/*   04³                   Delete DOS Partition or Logical DOS Drive                    ³menu_25     */
/*   05³                                                                                ³            */
/*   06³    Current fixed disk drive: #                                                 ³menu_5 #    */
/*   07³                                                                                ³            */
/*   08³    Choose one of the following:                                                ³menu_3 #    */
/*   09³                                                                                ³            */
/*   10³    1.  Delete Primary DOS Partition                                            ³menu_26     */
/*   11³    2.  Delete Extended DOS Partition                                           ³menu_26     */
/*   12³    3.  Delete Logical DOS Drive(s) in the Extended DOS Partition               ³menu_27     */
/*   13³    4.  Delete Non-DOS Partition                                                ³menu_57     */
/*   14³                                                                                ³            */
/*   15³                                                                                ³            */
/*   16³                                                                                ³            */
/*   17³    Enter choice: [ ]                                                           ³menu_7 #    */
/*   18³                                                                                ³            */
/*   19³                                                                                ³            */
/*   20³                                                                                ³            */
/*   21³                                                                                ³            */
/*   22³                                                                                ³            */
/*   23³                                                                                ³            */
/*   24³    Press ESC to return to FDISK Options                                        ³menu_11     */
/*   ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/*****************************************************************************************************/

/*C35----------------------------------------------------------*/
 char far *menu_25 =

"0404<H>1040";

/*-------------------------------------------------------------*/
 char far *menu_26 =

"1004<HC>1041\
 0104<HC>1042";

/*-------------------------------------------------------------*/
 char far *menu_27 =

"1204<HC>1043";

/*C14----------------------------------------------------------*/
 char far *menu_57 =

"1304<HC>1125";

/*****************************************************************************************************/
/*  Screen for DOS_DELETE                                                                            */
/*                                                                                                   */
/*     ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*     ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*   ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*   00³                                                                                ³            */
/*   01³                                                                                ³            */
/*   02³                                                                                ³            */
/*   03³                                                                                ³            */
/*   04³                           Delete Primary DOS Partition                         ³menu_28     */
/*   05³                                                                                ³            */
/*   06³    Current fixed disk drive: #                                                 ³menu_5 #    */
/*   07³                                                                                ³            */
/*   08³    Partition Status   Type    Size in Mbytes   Percentage of Disk Used         ³menu_14 #   */
/*   09³     ## #        #   #######       ####         ###%                            ³menu_14 #   */
/*   10³     ## #        #   #######       ####         ###%                            ³            */
/*   11³     ## #        #   #######       ####         ###%                            ³            */
/*   12³     ## #        #   #######       ####         ###%                            ³            */
/*   13³                                                                                ³            */
/*   14³    Total disk space is #### Mbytes (1 Mbyte = 1048576 bytes)                   ³menu_15 #   */
/*   15³                                                                                ³            */
/*   16³    Warning! Data in the deleted Primary DOS Partition will be lost.            ³menu_29     */
/*   17³    What Primary DOS Partition do you want to delete? [ ]                         ³            */
/*   18³                                                                                ³            */
/*   19³    Do you wish to continue (Y/N).................? [N]                         ³menu_56     */
/*   20³                                                                                ³            */
/*   21³                                                                                ³            */
/*   22³                                                                                ³            */
/*   23³                                                                                ³            */
/*   24³    Press ESC to return to FDISK Options                                        ³menu_11     */
/*   ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/*****************************************************************************************************/

/*C35----------------------------------------------------------*/
 char far *menu_28 =

"0404<H>1044";

/*C21----------------------------------------------------------*/
 char far *menu_29 =

"1904<HBC>1045";

/*C01-C21------------------------------------------------------*/
 char far *menu_55 =

"1904<HBC>1045\
 0104<R>1123";

/*C01-C21------------------------------------------------------*/
 char far *menu_56 =

"2204<RC>1046";

/*****************************************************************************************************/
/*  Screen for EXT_DELETE                                                                            */
/*                                                                                                   */
/*     ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*     ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*   ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*   00³                                                                                ³            */
/*   01³                                                                                ³            */
/*   02³                                                                                ³            */
/*   03³                                                                                ³            */
/*   04³                           Delete Extended DOS Partition                        ³menu_30     */
/*   05³                                                                                ³            */
/*   06³    Current fixed disk drive: #                                                 ³menu_5 #    */
/*   07³                                                                                ³            */
/*   08³    Partition Status   Type    Size in Mbytes   Percentage of Disk Used         ³menu_14 #   */
/*   09³     ## #        #   #######       ####         ###%                            ³menu_14 #   */
/*   10³     ## #        #   #######       ####         ###%                            ³            */
/*   11³     ## #        #   #######       ####         ###%                            ³            */
/*   12³     ## #        #   #######       ####         ###%                            ³            */
/*   13³                                                                                ³            */
/*   14³    Total disk space is #### Mbytes (1 Mbyte = 1048576 bytes)                   ³menu_15 #   */
/*   15³                                                                                ³            */
/*   16³    Warning! Data in the deleted Extended DOS partition will be lost.           ³menu_31     */
/*   17³    Do you wish to continue (Y/N).................? [N]                         ³menu_31     */
/*   18³                                                                                ³            */
/*   19³                                                                                ³            */
/*   20³                                                                                ³            */
/*   21³                                                                                ³            */
/*   22³                                                                                ³            */
/*   23³                                                                                ³            */
/*   24³    Press ESC to return to FDISK Options                                        ³menu_11     */
/*   ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/*****************************************************************************************************/

/*C35----------------------------------------------------------*/
 char far *menu_30 =

"0404<H>1047";

/*-------------------------------------------------------------*/
 char far *menu_31 =

"1604<HBC>1048\
 0104<RC>1049";

/******************************************************************************************************/
/*  Screen for VOL_DELETE                                                                             */
/*                                                                                                    */
/*     ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³             */
/*     ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³             */
/*   ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´             */
/*   00³                                                                                ³             */
/*   01³            Delete Logical DOS Drive(s) in the Extended DOS Partition           ³menu_32      */
/*   02³                                                                                ³             */
/*   03³Drv Volume Label  MBytes  System  Usage  Drv Volume Label  MBytes  System  Usage³menu_19/20 # */
/*   04³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³             */
/*   05³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³             */
/*   06³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³             */
/*   07³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³             */
/*   08³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³             */
/*   09³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³             */
/*   10³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³             */
/*   11³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³             */
/*   12³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³             */
/*   13³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³             */
/*   14³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³             */
/*   15³##  #############  ####  ########  ###%                                         ³             */
/*   16³                                                                                ³             */
/*   17³    Total Extended DOS Partition size is #### Mbytes (1 Mbyte = 1048576 bytes)  ³menu_21      */
/*   18³                                                                                ³             */
/*   19³    Warning! Data in a deleted Logical DOS Drive will be lost.                  ³menu_33      */
/*   20³    What drive do you want to delete...........................? [ ]            ³menu_33      */
/*   21³    Enter Volume Label.............................? [             ]            ³menu_41      */
/*   22³    Are you sure (Y/N).............................? [N]                        ³menu_34      */
/*   23³                                                                                ³             */
/*   24³    Press ESC to return to FDISK Options                                        ³menu_11      */
/*   ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ             */
/******************************************************************************************************/

/*C35----------------------------------------------------------*/
 char far *menu_32 =

"0104<H>1050";

/*-------------------------------------------------------------*/
 char far *menu_33 =

"1904<HBC>1051\
 0104<RC>1052";

/*-------------------------------------------------------------*/
 char far *menu_34 =

"2204<R>1053";

/*-------------------------------------------------------------*/
 char far *menu_41 =

"2104<R>1054";

/*C14*************************************************************************************************/
/*  Screen for NON_DOS_DELETE                                                                        */
/*                                                                                                   */
/*     ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*     ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*   ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*   00³                                                                                ³            */
/*   01³                                                                                ³            */
/*   02³                                                                                ³            */
/*   03³                                                                                ³            */
/*   04³                            Delete Non-DOS Partition                            ³menu_58     */
/*   05³                                                                                ³            */
/*   06³    Current fixed disk drive: #                                                 ³menu_5 #    */
/*   07³                                                                                ³            */
/*   08³    Partition Status   Type    Size in Mbytes   Percentage of Disk Used         ³menu_14 #   */
/*   09³     ## #        #   #######       ####         ###%                            ³menu_14 #   */
/*   10³     ## #        #   #######       ####         ###%                            ³            */
/*   11³     ## #        #   #######       ####         ###%                            ³            */
/*   12³     ## #        #   #######       ####         ###%                            ³            */
/*   13³                                                                                ³            */
/*   14³    Total disk space is #### Mbytes (1 Mbyte = 1048576 bytes)                   ³menu_15 #   */
/*   15³                                                                                ³            */
/*   16³    Warning! Data in the deleted Non-DOS Partition will be lost.                ³menu_59     */
/*   17³    What Non-DOS partition do you want to delete..? [ ]                         ³            */
/*   18³                                                                                ³            */
/*   19³    Do you wish to continue (Y/N).................? [N]                         ³menu_56     */
/*   20³                                                                                ³            */
/*   21³                                                                                ³            */
/*   22³                                                                                ³            */
/*   23³                                                                                ³            */
/*   24³    Press ESC to return to FDISK Options                                        ³menu_11     */
/*   ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/*****************************************************************************************************/

/*C14-C35------------------------------------------------------*/
 char far *menu_58 =

"0404<H>1126";

/*C14----------------------------------------------------------*/
 char far *menu_59 =

"1604<HBC>1127\
 0104<RC>1128";

/******************************************************************************************************/
/*  Screen for DISPLAY_PARTITION_INFORMATION                                                          */
/*                                                                                                    */
/*     ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³             */
/*     ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³             */
/*   ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´             */
/*   00³                                                                                ³             */
/*   01³                                                                                ³             */
/*   02³                                                                                ³             */
/*   03³                                                                                ³             */
/*   04³                           Display Partition Information                        ³menu_35      */
/*   05³                                                                                ³             */
/*   06³    Current fixed disk drive: #                                                 ³menu_5 #     */
/*   07³                                                                                ³             */
/*   08³    Partition Status   Type    Size in Mbytes   Percentage of Disk Used         ³menu_14 #    */
/*   09³     ## #        #   #######       ####         ###%                            ³menu_14 #    */
/*   10³     ## #        #   #######       ####         ###%                            ³             */
/*   11³     ## #        #   #######       ####         ###%                            ³             */
/*   12³     ## #        #   #######       ####         ###%                            ³             */
/*   13³                                                                                ³             */
/*   14³    Total disk space is #### Mbytes (1 Mbyte = 1048576 bytes)                   ³menu_15 #    */
/*   15³                                                                                ³             */
/*   16³                                                                                ³             */
/*   17³    The Extended DOS partition contains Logical DOS Drives.                     ³menu_36      */
/*   18³    Do you want to display the logical drive information (Y/N)......? [Y]       ³menu_36      */
/*   19³                                                                                ³             */
/*   20³                                                                                ³             */
/*   21³                                                                                ³             */
/*   22³                                                                                ³             */
/*   23³                                                                                ³             */
/*   24³    Press ESC to return to FDISK Options                                        ³menu_11     */
/*   ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ             */
/******************************************************************************************************/

/*C35----------------------------------------------------------*/
 char far *menu_35 =

"0404<H>1055";

/*-------------------------------------------------------------*/
 char far *menu_36 =

"1704<RC>1056\
 0104<RC>1057";

/*****************************************************************************************************/
/*  Screen for DISPLAY_VOLUME_INFORMATION                                                            */
/*                                                                                                   */
/*     ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*     ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*   ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*   00³                                                                                ³            */
/*   01³                     Display Logical DOS Drive Information                      ³menu_37     */
/*   02³                                                                                ³            */
/*   03³Drv Volume Label  Mbytes  System  Usage  Drv Volume Label  Mbytes  System  Usage³menu_19/20  */
/*   04³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³            */
/*   05³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³            */
/*   16³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³            */
/*   17³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³            */
/*   18³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³            */
/*   19³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³            */
/*   10³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³            */
/*   11³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³            */
/*   12³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³            */
/*   13³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³            */
/*   14³##  #############  ####  ########  ###%  ##  #############  ####  ########  ###%³            */
/*   15³##  #############  ####  ########  ###%                                         ³            */
/*   16³                                                                                ³            */
/*   17³    Total Extended DOS partition size is #### Mbytes (1 Mbyte = 1048576 bytes)  ³menu_17     */
/*   18³                                                                                ³            */
/*   19³                                                                                ³            */
/*   20³                                                                                ³            */
/*   21³                                                                                ³            */
/*   22³                                                                                ³            */
/*   23³                                                                                ³            */
/*   24³    Press ESC to return to FDISK Options                                        ³menu_11     */
/*   ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/*****************************************************************************************************/

/*C35----------------------------------------------------------*/
 char far *menu_37 =

"0104<H>1058";


/*C08*************************************************************************************************/
/*  Screen for CHANGE_DISK_DRIVE                                                                     */
/*                                                                                                   */
/*     ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*     ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*   ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*   00³                        Change Current Fixed Disk Drive                         ³            */
/*   01³                                                                                ³            */
/*   02³  Disk   Drv  MBytes   Free   Usage       Disk   Drv   MBytes   Free   Usage    ³            */
/*   03³    #          ####    ####    ###%         #           ####                    ³            */
/*   04³          ##   ####                               ##    ####                    ³            */
/*   05³          ##   ####                         #           ####    ####    ###%    ³            */
/*   06³          ##   ####                               ##    ####                    ³            */
/*   07³    #          ####    ####    ###%               ##    ####                    ³            */
/*   08³          ##   ####                               ##    ####                    ³            */
/*   09³          ##   ####                         #           ####    ####    ###%    ³            */
/*   10³          ##   ####                               ##    ####                    ³            */
/*   11³          ##   ####                               ##    ####                    ³            */
/*   12³          ##   ####                         #           ####    ####    ###%    ³            */
/*   13³          ##   ####                               ##    ####                    ³            */
/*   14³    #          ####    ####    ###%               ##    ####                    ³            */
/*   15³          ##   ####                         #           ####    ####    ###%    ³            */
/*   16³          ##   ####                               ##    ####                    ³            */
/*   17³          ##   ####                               ##    ####                    ³            */
/*   18³          ##   ####                               ##    ####                    ³            */
/*   19³                                                                                ³            */
/*   20³    (1 MByte = 1048576 bytes)                                                                                   ³            */
/*   21³    Enter Fixed Disk Drive Number (1-7).......................[#]               ³menu_52     */
/*   22³                                                                                ³menu_53     */
/*   23³                                                                                ³            */
/*   24³    Press ESC to return to FDISK Options                                        ³menu_11     */
/*   ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/*****************************************************************************************************/
/*C08-C35------------------------------------------------------*/
 char far *menu_47 =

"0004<H>1117";                                                       /* BN000 */

/*C08-C10------------------------------------------------------*/
 char far *menu_48 =

"0200<R>1118\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>";

/*C08-----------------------------------------------------------*/
 char far *menu_49 =

"\
1104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0104<HI>     <RII>    <IIII>    <IIII>    <IIII>";

/*C08-C10-------------------------------------------------------*/
 char far *menu_50 =

"0241<R>1118\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>";

/*C08-----------------------------------------------------------*/
 char far *menu_51 =

"\
1145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>\
0145<HI>     <RII>    <IIII>    <IIII>    <IIII>";

/*C08-----------------------------------------------------------*/
 char far *menu_52 =

"2004<R>1119";                                                       /* BN000 */

/*C08-C10------------------------------------------------------*/
 char far *menu_53 =

"2104<R>1120";                                                       /* BN000 */

/*C08-C35------------------------------------------------------*/
 char far *menu_54 =

"0104<H>1122";                                                       /* BN000 */




/*****************************************************************************************************/
/*  Screen for SYSTEM_REBOOT                                                                         */
/*                                                                                                   */
/*     ³00000000001111111111222222222233333333334444444444555555555566666666667777777777³            */
/*     ³01234567890123456789012345678901234567890123456789012345678901234567890123456789³            */
/*   ÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´            */
/*   00³                                                                                ³            */
/*   01³                                                                                ³            */
/*   02³                                                                                ³            */
/*   03³                                                                                ³            */
/*   04³                                                                                ³            */
/*   05³                                                                                ³            */
/*   06³                                                                                ³            */
/*   07³                                                                                ³            */
/*   08³                                                                                ³            */
/*   09³                                                                                ³            */
/*   10³                                                                                ³            */
/*   11³                                                                                ³            */
/*   12³                                                                                ³            */
/*   13³    System will now restart                                                     ³menu_38     */
/*   14³                                                                                ³            */
/*   15³    Insert DOS diskette in drive A:                                             ³menu_38     */
/*   16³    Press any key when ready . . .                                              ³menu_38     */
/*   17³                                                                                ³            */
/*   18³                                                                                ³            */
/*   19³                                                                                ³            */
/*   20³                                                                                ³            */
/*   21³                                                                                ³            */
/*   22³                                                                                ³            */
/*   23³                                                                                ³            */
/*   24³                                                                                ³             */
/*   ÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ            */
/*****************************************************************************************************/

/*-------------------------------------------------------------*/
 char far *menu_38 =

"1304<H>1059\
 0204<R>1060\
 0104<R>1061";

/*C20----------------------------------------------------------*/
 char far *menu_60 =

"1304<H>1059\
 0304<R>1061";


/*                                                                      */
/*                                                                      */
/* The following character strings are required to display the          */
/* status messages indicating successful operation. These messages      */
/* have the form: status_xx                                             */
/*                                                                      */
/* Note: In order to overlay any previous message on the screen, these  */
/*       messages are all 2 lines long. The second line may only be     */
/*       a blank line. If 2 lines are needed for translation, use the   */
/*       second line for text.  Exceptions are those msgs on line 0,    */
/*       and status_3.                                                  */
/*                                                                      */

/*-------------------------------------------------------------*/
 char far *status_1 =

"2104<CH>1062\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *status_2 =

"2104<CH>1063\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *status_3 =

"0004<H>1064";
/* NOTE - the rrcc must be the first thing in this string */

/*-------------------------------------------------------------*/
 char far *status_4 =

"2104<CH>1065\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *status_5 =

"2104<CH>1066\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *status_6 =

"2104<CH>1067\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *status_7 =

"2204<CH>1068";

/*-------------------------------------------------------------*/
 char far *status_8 =

"2104<CH>1069\
 0100<C>";

/*-------------------------------------------------------------*/
 char far *status_9 =

"1004<CH>1070\
 0100<C>";

/*-------------------------------------------------------------*/
 char far *status_10 =

"1804<CH>1071";

/*-------------------------------------------------------------*/
 char far *status_11 =

"0004<H>1072";
/* NOTE - the rrcc must be the first thing in this string */

/*-------------------------------------------------------------*/
 char far *status_12 =

"2104<CH>1073\
 0100<CW>";

/*C14----------------------------------------------------------*/
 char far *status_13 =

"2104<CH>1129\
 0100<CW>";

/*-------------------------------------------------------------*/

/*                                                                      */
/*                                                                      */
/* The following character strings are required to display the          */
/* error messages. These have form: error_xx                            */
/*                                                                      */
/* Note: In order to overlay any previous message on the screen, these  */
/*       messages are all 2 lines long. The second line may only be     */
/*       a blank line. If 2 lines are needed for translation, use the   */
/*       second line for text.  Exceptions are those msgs on line 0.    */
/*       and those messages that start on line 23                       */
/*                                                                      */


/*-------------------------------------------------------------*/
 char far *error_1 =

"0004<CH>1074\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_2 =

"2204<CH>1075\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_3 =

"2204<CH>1076\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_4 =

"0004<CHW>1077";

/*-------------------------------------------------------------*/
 char far *error_5 =

"0004<CHW>1078";

/*-------------------------------------------------------------*/
 char far *error_6 =

"2204<CH>1079\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_7 =

"2204<CH>1080\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_8 =

"2204<CH>1081\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_9 =

"2204<CH>1082\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_10 =

"2204<CH>1083\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_12 =

"2204<CH>1084\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_13 =

"2204<CH>1085\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_14 =

"2204<CH>1086\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_15 =

"2204<CH>1087\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_16 =

"2204<CH>1088\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_17 =

"2204<CH>1089";

/*-------------------------------------------------------------*/
 char far *error_19 =

"2204<CH>1090\
 0104<CH>1091";

/*-------------------------------------------------------------*/
 char far *error_20 =

"2204<CH>1092\
 0104<CH>1093";

/*-------------------------------------------------------------*/
 char far *error_21 =

"2204<CH>1094";

/*-------------------------------------------------------------*/
 char far *error_22 =

"2204<CH>1095";

/*-------------------------------------------------------------*/
 char far *error_23 =

"2204<C>\
 0104<CHI>1096";

/*-------------------------------------------------------------*/
 char far *error_24 =

"2204<CH>1097";

/*-------------------------------------------------------------*/
 char far *error_25 =

"2204<CH>1098\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_26 =

"2204<CH>1099";

/*-------------------------------------------------------------*/
 char far *error_27 =

"2204<CH>1100";

/*-------------------------------------------------------------*/
 char far *error_28 =

"2204<CH>1101\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_29 =

"2204<CH>1102\
 0100<CW>";

/*-------------------------------------------------------------*/
 char far *error_30 =

"2204<CHB>1103";

/*-------------------------------------------------------------*/
 char far *error_31 =

"2304<CH>1104";

/*-------------------------------------------------------------*/
 char far *error_32 =

"2204<CH>1105\
 0104<CH>1106";

/*-------------------------------------------------------------*/
 char far *error_33 =

"2200<C>\
 0104<CH>1107";

/*-------------------------------------------------------------*/
 char far *error_34 =

"2204<CH>1108";

/*-------------------------------------------------------------*/
 char far *error_35 =

"2204<CH>1109\
 0104<CH>1110";

/*-------------------------------------------------------------*/
 char far *error_36 =

"2204<CH>1111\
 0100<CW>";

/*C01----------------------------------------------------------*/
 char far *error_37 =

"2204<CH>1124\
 0100<CW>";

/*C14----------------------------------------------------------*/
 char far *error_38 =

"2204<CH>1130\
 0100<CW>";

/*C14----------------------------------------------------------*/
 char far *error_39 =

"2204<CH>1131\
 0100<CW>";

/*-------------------------------------------------------------*/


/*                                                                      */
/*                                                                      */
/* The following message is only included as an aide to debug message   */
/* strings during translations. The FDISK message formatter will attempt*/
/* to the best of its ability to catch invalid message strings and      */
/* print the error. This message should NEVER appear for a user, so it  */
/* is not neccessary to translate this message                          */
/*                                                                      */
/*                                                                      */


/*-------------------------------------------------------------*/

 char far *debug_msg =

"2200<HWB>1112";

/*-------------------------------------------------------------*/
 char far *internal_error =

"2204<HBW>1113";
/*                                                                      */
/* The following are not translatable. They are the partition names     */
/*                                                                      */


char *REMOTE		= "1121";


 char *DOS_part      = "PRI DOS";
 char *XENIX_part    = " XENIX ";
 char *EXTENDED_part = "EXT DOS";
 char *BAD_BLOCK_part= " Table ";
 char *PCIX_part     = " PC/IX ";
 char *HPFS_part     = " HPFS  ";                                       /*C15*/
 char *NOVELL_part   = "NOVELL ";                                       /*C15*/
 char *CPM_part      = " CP/M  ";                                       /*C15*/
 char *NON_DOS_part  = "Non-DOS";

