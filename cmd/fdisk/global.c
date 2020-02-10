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
*  05/23/90  EGH  C18  Added support for /MBR switch to update the master boot
*                      record.
*
******************************************************************************/

#include "fdisk.h"
#include "dos.h"

/*  */
/*                                                                          */
/****************************************************************************/
/* Declare Global variables                                                */
/****************************************************************************/
/*                                                                          */



char    cur_disk;
char    good_disk[MAX_HDISK];
unsigned char    number_of_drives;
char    reboot_flag;
char    errorlevel;
char    max_partition_size;
char    sort[24];
char    no_fatal_error;
char    valid_input;
unsigned char   video_mode;
unsigned char   display_page;
unsigned char   video_attribute;                                        /* AN006 */


unsigned        total_disk[MAX_HDISK];
unsigned        total_mbytes[MAX_HDISK];                                        /* AN000 */
unsigned char   max_sector[MAX_HDISK];
unsigned        max_head[MAX_HDISK];                                            /* AC004 */
unsigned        required_cyls[MAX_HDISK];

unsigned       input_row;
unsigned       input_col;
char           insert[800];                                             /* AC000 */
char           *pinsert = insert;

extern unsigned char   master_boot_record[MAX_HDISK][512];
unsigned char   boot_record[512];

char            next_letter;                                            /* AN000 */
char            primary_flag;                                           /* AC000 */
char            extended_flag;                                          /* AC000 */
char            logical_flag;                                           /* AC000 */
char            disk_flag;                                              /* AC000 */
char            quiet_flag;                                             /* AC000 */

char            status_flag;
char            mbr_flag;                                               /*C18*/

unsigned        primary_buff;                                           /* AC000 */
unsigned        extended_buff;                                          /* AC000 */
unsigned        logical_buff;                                           /* AC000 */
char            cur_disk_buff;                                          /* AC000 */
unsigned long   NOVAL = (unsigned long) 0;                              /* AC000 */
FLAG            PercentFlag;                                            /* AC000 */

FLAG            mono_flag;                                              /* AC006 */

char            Yes;                                                    /* AC012 */
char            No;                                                     /* AC012 */

unsigned        Parse_Ptr;                                              /* AN010 */

char				primary_partition_count;
/*  */
/*                                                                          */
/****************************************************************************/
/* Define Global structures                                                 */
/****************************************************************************/
/*                                                                          */

struct entry part_table[MAX_HDISK][4];
struct entry ext_table[MAX_HDISK][24];
struct freespace free_space[24];
struct KeyData *input_data;
struct dx_buffer_ioctl dx_buff;                                         /* AN000 */
struct diskaccess disk_access;                                          /* AN002 */
struct SREGS segregs;
struct sublistx sublistp[1];                                            /* AN010 */

/*                                                                          */
/****************************************************************************/
/* Define UNIONS                                                            */
/****************************************************************************/
/*                                                                          */

union REGS regs;

