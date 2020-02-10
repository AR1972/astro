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
*  03/22/90  EGH  C14  Added support for deleting non-DOS partitions.  This
*                      resulted in the creation of menu_57, menu_58, menu_59,
*                      status_13, error_38, and error_39.
*  03/22/90  EGH  C15  Added code to recognize additional partition types.
*  05/24/90  EGH  C20  Added code to determine whether or not the system 
*                      diskette needs to be inserted before a reboot.  This
*                      resulted in the creation of menu_60.
*
******************************************************************************/
/*           FDISK MESSAGE FILE                                         */

/************************************************************************/
/* Please log all modifications to this file:                           */
/*----------------------------------------------------------------------*/
/* Date: 04/04/86                                                       */
/* Changed by: Mark T                                                   */
/* Message changed: menu_1 - menu_38                                    */
/* Reason: Creation of file                                             */
/*----------------------------------------------------------------------*/
/* Date: 05/04/87                                                       */
/* Changed by: Dennis M                                                 */
/* Message changed: menu_1 - menu_44                                    */
/* Reason: DOS 3.3                                                      */
/*----------------------------------------------------------------------*/
/* Date:                                                                */
/* Changed by:                                                          */
/* Message changed:                                                     */
/* Reason:                                                              */
/*----------------------------------------------------------------------*/
/************************************************************************/

#define ACTIVE_PART  'A'   /* Character to indicate active status */
#define DRIVE_INDICATOR ':' /* Character displayed to indicate drive letter */

/*-------------------------------------------------------------*/
 extern char far *menu_1;                                               /* AN000 */
 extern char far *menu_2;                                               /* AN000 */
 extern char far *menu_3 ;                                              /* AN000 */
 extern char far *menu_4 ;                                              /* AN000 */
 extern char far *menu_5 ;                                              /* AN000 */
 extern char far *menu_6 ;                                              /* AN000 */
 extern char far *menu_7 ;                                              /* AN000 */
 extern char far *menu_8 ;                                              /* AN000 */
 extern char far *menu_9 ;                                              /* AN000 */
 extern char far *menu_10 ;                                             /* AN000 */
 extern char far *menu_11 ;                                             /* AN000 */
 extern char far *menu_12 ;                                             /* AN000 */
 extern char far *menu_13 ;                                             /* AN000 */
 extern char far *menu_14 ;                                             /* AN000 */
 extern char far *menu_15 ;                                             /* AN000 */
 extern char far *menu_16 ;                                             /* AN000 */
 extern char far *menu_39 ;                                             /* AN000 */
 extern char far *menu_17 ;                                             /* AN000 */
 extern char far *menu_42 ;                                             /* AN000 */
 extern char far *menu_18 ;                                             /* AN000 */
 extern char far *menu_19 ;                                             /* AN000 */
 extern char far *menu_43 ;                                             /* AN000 */
 extern char far *menu_20 ;                                             /* AN000 */
 extern char far *menu_44 ;                                             /* AN000 */
 extern char far *menu_21 ;                                             /* AN000 */
 extern char far *menu_22 ;                                             /* AN000 */
 extern char far *menu_40 ;                                             /* AN000 */
 extern char far *menu_23 ;                                             /* AN000 */
 extern char far *menu_24 ;                                             /* AN000 */
 extern char far *menu_25 ;                                             /* AN000 */
 extern char far *menu_26 ;                                             /* AN000 */
 extern char far *menu_27 ;                                             /* AN000 */
 extern char far *menu_28 ;                                             /* AN000 */
 extern char far *menu_29 ;                                             /* AN000 */
 extern char far *menu_30 ;                                             /* AN000 */
 extern char far *menu_31 ;                                             /* AN000 */
 extern char far *menu_32 ;                                             /* AN000 */
 extern char far *menu_33 ;                                             /* AN000 */
 extern char far *menu_34 ;                                             /* AN000 */
 extern char far *menu_41 ;                                             /* AN000 */
 extern char far *menu_35 ;                                             /* AN000 */
 extern char far *menu_36 ;                                             /* AN000 */
 extern char far *menu_37 ;                                             /* AN000 */
 extern char far *menu_38 ;                                             /* AN000 */
 extern char far *menu_45 ;                                             /* AN000 */
 extern char far *menu_46 ;                                             /* AN000 */
 extern char far *menu_47 ;
 extern char far *menu_48 ;
 extern char far *menu_49 ;
 extern char far *menu_50 ;
 extern char far *menu_51 ;
 extern char far *menu_52 ;
 extern char far *menu_53 ;
 extern char far *menu_54 ;
 extern char far *menu_55 ;                                             /*C01*/
 extern char far *menu_56 ;                                             /*C01*/
 extern char far *menu_57 ;                                             /*C14*/
 extern char far *menu_58 ;                                             /*C14*/
 extern char far *menu_59 ;                                             /*C14*/
 extern char far *menu_60 ;                                             /*C20*/

/*-------------------------------------------------------------*/
 extern char far *status_1 ;                                            /* AN000 */
 extern char far *status_2 ;                                            /* AN000 */
 extern char far *status_3 ;                                            /* AN000 */
 extern char far *status_4 ;                                            /* AN000 */
 extern char far *status_5 ;                                            /* AN000 */
 extern char far *status_6 ;                                            /* AN000 */
 extern char far *status_7 ;                                            /* AN000 */
 extern char far *status_8 ;                                            /* AN000 */
 extern char far *status_9 ;                                            /* AN000 */
 extern char far *status_10 ;                                           /* AN000 */
 extern char far *status_11 ;                                           /* AN000 */
 extern char far *status_12 ;                                           /* AN000 */
 extern char far *status_13 ;                                           /*C14*/
/*-------------------------------------------------------------*/
 extern char far *error_1 ;                                             /* AN000 */
 extern char far *error_2 ;                                             /* AN000 */
 extern char far *error_3 ;                                             /* AN000 */
 extern char far *error_4 ;                                             /* AN000 */
 extern char far *error_5 ;                                             /* AN000 */
 extern char far *error_6 ;                                             /* AN000 */
 extern char far *error_7 ;                                             /* AN000 */
 extern char far *error_8 ;                                             /* AN000 */
 extern char far *error_9 ;                                             /* AN000 */
 extern char far *error_10 ;                                            /* AN000 */
 extern char far *error_12 ;                                            /* AN000 */
 extern char far *error_13 ;                                            /* AN000 */
 extern char far *error_14 ;                                            /* AN000 */
 extern char far *error_15 ;                                            /* AN000 */
 extern char far *error_16 ;                                            /* AN000 */
 extern char far *error_17 ;                                            /* AN000 */
 extern char far *error_19 ;                                            /* AN000 */
 extern char far *error_20 ;                                            /* AN000 */
 extern char far *error_21 ;                                            /* AN000 */
 extern char far *error_22 ;                                            /* AN000 */
 extern char far *error_23 ;                                            /* AN000 */
 extern char far *error_24 ;                                            /* AN000 */
 extern char far *error_25 ;                                            /* AN000 */
 extern char far *error_26 ;                                            /* AN000 */
 extern char far *error_27 ;                                            /* AN000 */
 extern char far *error_28 ;                                            /* AN000 */
 extern char far *error_29 ;                                            /* AN000 */
 extern char far *error_30 ;                                            /* AN000 */
 extern char far *error_31 ;                                            /* AN000 */
 extern char far *error_32 ;                                            /* AN000 */
 extern char far *error_33 ;                                            /* AN000 */
 extern char far *error_34 ;                                            /* AN000 */
 extern char far *error_35 ;                                            /* AN000 */
 extern char far *error_36 ;                                            /* AN000 */
 extern char far *error_37 ;                                            /*C01*/
 extern char far *error_38 ;                                            /*C14*/
 extern char far *error_39 ;                                            /*C14*/
/*-------------------------------------------------------------*/
 extern char far *debug_msg ;                                           /* AN000 */
 extern char far *internal_error ;                                      /* AN000 */
/*-------------------------------------------------------------*/
 extern char *REMOTE;

 extern char *DOS_part ;                                            /* AN000 */
 extern char *XENIX_part ;                                          /* AN000 */
 extern char *EXTENDED_part ;                                       /* AN000 */
 extern char *BAD_BLOCK_part ;                                      /* AN000 */
 extern char *PCIX_part ;                                           /* AN000 */
 extern char *HPFS_part ;                                               /*C15*/
 extern char *NOVELL_part ;                                             /*C15*/
 extern char *CPM_part ;                                                /*C15*/
 extern char *NON_DOS_part ;                                        /* AN000 */

