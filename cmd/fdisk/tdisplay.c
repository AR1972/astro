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
*  03/08/90  EGH  C00  Cleaned up build by removing unused variables, declaring
*                      functions properly, changing long JMPs to short JMPs,
*                      etc.
*  03/22/90  EGH  C15  Added code to recognize additional partition types.
*  08/01/90  EGH  C24  Problem fixed - on a two drive system with only a single
*                      primary partition on drive 2, the display partition
*                      information screen reported the partiton as drive D:.
*  08/27/90  EGH  C25  Added support for displaying the volume label when
*                      creating/deleting primary partitions.                     logical DOS drives.
*  10/19/90  EGH  C32  Added code to handle multiple primary DOS partitions.
*
******************************************************************************/

#include "dos.h"                                                        /* AN000 */
#include "fdisk.h"                                                      /* AN000 */
#include "extern.h"                                                     /* AN000 */
#include "subtype.h"                                                    /* AN000 */
#include "fdiskmsg.h"                                                   /* AN000 */
#include "stdio.h"
#include "string.h"
#include "memory.h"

/*  */
char table_display()

BEGIN


    unsigned    i;
/*C00    unsigned    x;  */
    unsigned    io;
    char       *ThisPartitionType;
    char        ThisPartitionLetter[3];
    FLAG        partition_found;
    char        partition_num;

    /* initialize all the inserts to blanks */
/*C25    memset(insert,c(' '),4*21); */
    memset(insert,c(' '),4*38);                                         /*C25*/
    io = u(0);

    /* Get current drive letters */                                     /*C32*/
    get_letters();                                                      /*C32*/

    /* Sort the partitions */
    sort_part_table(c(4));                                             /* AC000 */

    /* loop thru the partitions, only print stuff if it is there */
    partition_found = FALSE;
    partition_num = c(0);                                              /* AC000 */

    for (i=u(0); i < u(4); i++)                                        /* AC000 */
        BEGIN

        if (part_table[cur_disk][sort[i]].sys_id != uc(0))             /* AC000 */
            BEGIN

            partition_found = TRUE;

            strcpy(ThisPartitionLetter,"  ");
            switch(part_table[cur_disk][sort[i]].sys_id)
                BEGIN
                case DOSNEW:                                           /* AN000 */
                case DOS16:
                case DOS12:
                    ThisPartitionType = DOS_part;
/*C32               part_table[cur_disk][sort[i]].drive_letter = table_drive_letter(); */      /* AN000 */
                    sprintf(ThisPartitionLetter,"%c%c",
                            part_table[cur_disk][sort[i]].drive_letter,
                            ( part_table[cur_disk][sort[i]].drive_letter == c(' ') ) ? ' ' : ':');
                    break;
                case EXTENDED:
                    ThisPartitionType = EXTENDED_part;
                    break;
                case BAD_BLOCK:
                    ThisPartitionType = BAD_BLOCK_part;
                    break;
                case XENIX1:
                    ThisPartitionType = XENIX_part;
                    break;
                case XENIX2:
                    ThisPartitionType = XENIX_part;
                    break;
                case PCIX:
                    ThisPartitionType = PCIX_part;
                    break;
                case HPFS:                                              /*C15*/
                    ThisPartitionType = HPFS_part;                      /*C15*/
                    break;                                              /*C15*/
                case NOVELL:                                            /*C15*/
                    ThisPartitionType = NOVELL_part;                    /*C15*/
                    break;                                              /*C15*/
                case CPM:                                               /*C15*/
                    ThisPartitionType = CPM_part;                       /*C15*/
                    break;                                              /*C15*/
                default:
                    ThisPartitionType = NON_DOS_part;
                    break;
                END

/*C25       io += sprintf(&insert[io],"%-2.2s%c%c%-7.7s%4.0d%3.0d%%",                     */
/*C25                  ThisPartitionLetter,                                               */
/*C25                  partition_num+'1',                                                 */
/*C25                  (part_table[cur_disk][sort[i]].boot_ind == uc(0x80)) ? 'A' : ' ',  */
/*C25                  ThisPartitionType,                                                 */
/*C25                  part_table[cur_disk][sort[i]].mbytes_used,                         */
/*C25                  part_table[cur_disk][sort[i]].percent_used);                       */

            io += sprintf(&insert[io],"%-2.2s%c%c%-7.7s%-11.11s%4.0d%-8.8s%3.0d%%",       /*C25*/
                       ThisPartitionLetter,                                               /*C25*/
                       partition_num+'1',                                                 /*C25*/
                       (part_table[cur_disk][sort[i]].boot_ind == uc(0x80)) ? 'A' : ' ',  /*C25*/
                       ThisPartitionType,                                                 /*C25*/
                       part_table[cur_disk][sort[i]].vol_label,                           /*C25*/
                       part_table[cur_disk][sort[i]].mbytes_used,                         /*C25*/
                       part_table[cur_disk][sort[i]].system,                              /*C25*/
                       part_table[cur_disk][sort[i]].percent_used);                       /*C25*/

            partition_num++;

            END

        END

    /* Do a clearscreen to erase previous data */
    clear_screen(u(8),u(0),u(12),u(79));                               /* AC000 */

    if (partition_found) display(menu_14);
        else display(status_8);

    /* Return true if partitions exist, false otherwise */
    if (partition_found) return(TRUE);

    return(FALSE);

END

/*  */
#if 0	/******************** New routine replaces this ********/

char table_drive_letter()

BEGIN
    char drive_letter;

    /* Put in drive letter in display */
    if (cur_disk == c(0))                                             /* AC000 */
       BEGIN
        /* There is a primary partition on 80h, so drive C: */
        drive_letter = c('C');                                        /* AC000 */
       END
    else
       BEGIN
        /* We are on drive 81h, so assume D: */
        drive_letter = c('D');                                        /* AC000 */

        /* See if primary exists on 80h drive */

        /* Check for primary on drive 80h */
        if (!(find_partition_type(uc(DOS12)) || find_partition_type(uc(DOS16)) || find_partition_type(uc(DOSNEW)))) /* AC000 */
           BEGIN
            drive_letter = c('C');                                    /* AC000 */
           END
       END
    return(drive_letter);
END
#endif	/***************** New routine (below) ********************/

#if 0   /******************** New routine replaces this ********/       /*C24*/
char table_drive_letter()

BEGIN
char drive_letter;

    /* Put in drive letter in display */
    drive_letter = c('C') + cur_disk;                            /* AC000 */

    /* Check for primary on drive 80h */
    if (!(find_partition_type(uc(DOS12)) || find_partition_type(uc(DOS16)) || find_partition_type(uc(DOSNEW)))) /* AC000 */
       BEGIN
        drive_letter = c('C');                                    /* AC000 */
       END

    return(drive_letter);
END
#endif  /***************** New routine (below) ********************/    /*C24*/

#if 0   /* Routine no longer needed */                                  /*C32*/
char table_drive_letter()                                               /*C24*/
                                                                        /*C24*/
BEGIN                                                                   /*C24*/
char drive_letter;                                                      /*C24*/
char temp;                                                              /*C24*/
                                                                        /*C24*/
    /* Initialize drive letter */                                       /*C24*/
    drive_letter = c(0);                                                /*C24*/
                                                                        /*C24*/
    /* Save current disk */                                             /*C24*/
    temp = cur_disk;                                                    /*C24*/
                                                                        /*C24*/
    /* Count the number of primary partitons on the preceeding disks */ /*C24*/
    cur_disk = c(0);                                                    /*C24*/
    while(cur_disk < temp)                                              /*C24*/
       {                                                                /*C24*/
       if (find_partition_type(uc(DOS12)) ||                            /*C24*/
           find_partition_type(uc(DOS16)) ||                            /*C24*/
           find_partition_type(uc(DOSNEW)))                             /*C24*/
           drive_letter++;                                              /*C24*/
       cur_disk++;                                                      /*C24*/
       }                                                                /*C24*/
                                                                        /*C24*/
    /* Restore current disk */                                          /*C24*/
    cur_disk = temp;                                                    /*C24*/
                                                                        /*C24*/
    /* Get drive letter */                                              /*C24*/
    drive_letter += c('C');                                             /*C24*/
                                                                        /*C24*/
    return(drive_letter);                                               /*C24*/
END                                                                     /*C24*/
#endif                                                                  /*C32*/


void get_letters()                                                      /*C32*/
BEGIN                                                                   /*C32*/
                                                                        /*C32*/
unsigned char i,j;                                                      /*C32*/
unsigned char active_primary_count;                                     /*C32*/
unsigned char pri_part;                                                 /*C32*/
                                                                        /*C32*/
    /* initialize next letter */                                        /*C32*/
    next_letter = c(SEA);                                               /*C32*/
                                                                        /*C32*/
    /* get primary DOS partition letters on all drives */               /*C32*/
    for (j = uc(0); j < number_of_drives; j++)                          /*C32*/
       BEGIN                                                            /*C32*/
        /* initialize variables */                                      /*C32*/
        primary_partition_count = 0;                                    /*C32*/
        active_primary_count = 0;                                       /*C32*/
        pri_part = 0xFF;                                                /*C32*/
                                                                        /*C32*/
        /* Look at all partitions */                                    /*C32*/
        for (i=u(0); i < u(4); i++)                                     /*C32*/
           BEGIN                                                        /*C32*/
            /* Compute e letter */                                      /*C32*/
            if ( (part_table[j][i].sys_id == DOS12) ||                  /*C32*/
                 (part_table[j][i].sys_id == DOS16) ||                  /*C32*/
                 (part_table[j][i].sys_id == DOSNEW)   )                /*C32*/
               BEGIN                                                    /*C32*/
                part_table[j][i].drive_letter = c(' ');                 /*C32*/                         /* AN000 */
                if ((primary_partition_count == 0) ||                   /*C32*/
                    ((part_table[j][i].boot_ind == 0x80) &&             /*C32*/
                     (active_primary_count == 0)))                      /*C32*/
                    pri_part = c(i);                                    /*C32*/
                if (part_table[j][i].boot_ind == 0x80)                  /*C32*/
                    active_primary_count++;                             /*C32*/
                primary_partition_count++;                              /*C32*/
               END                                                      /*C32*/
           END                                                          /*C32*/
        if ((pri_part != 0xFF) && (next_letter <= c('Z')))              /*C32*/
            part_table[j][pri_part].drive_letter = next_letter++;       /*C32*/
       END                                                              /*C32*/
                                                                        /*C32*/
    /* get logical DOS drive letters on all drives */                   /*C32*/
    for (j = uc(0); j < number_of_drives; j++)                          /*C32*/
       BEGIN                                                            /*C32*/
        /* Look at all partitions */                                    /*C32*/
        for (i=u(0); i < u(23); i++)                                    /*C32*/
           BEGIN                                                        /*C32*/
            /* Set drive letter */                                      /*C32*/
            if ( (ext_table[j][i].sys_id == DOS12) ||                   /*C32*/
                 (ext_table[j][i].sys_id == DOS16) ||                   /*C32*/
                 (ext_table[j][i].sys_id == DOSNEW)   )                 /*C32*/
                if (next_letter <= c('Z'))                              /*C32*/
                    ext_table[j][i].drive_letter = next_letter++;       /*C32*/
                else                                                    /*C32*/
                    ext_table[j][i].drive_letter = c(' ');              /*C32*/
           END                                                          /*C32*/
       END                                                              /*C32*/
                                                                        /*C32*/
    /* get enhdisk partition letters on all drives */                   /*C32*/
    for (j = uc(0); j < number_of_drives; j++)                          /*C32*/
       BEGIN                                                            /*C32*/
        /* Look at all active primary partitions */                     /*C32*/
        for (i=u(0); i < u(4); i++)                                     /*C32*/
           BEGIN                                                        /*C32*/
            /* Set drive letter */                                      /*C32*/
            if (((part_table[j][i].sys_id == DOS12) ||                  /*C32*/
                 (part_table[j][i].sys_id == DOS16) ||                  /*C32*/
                 (part_table[j][i].sys_id == DOSNEW)) &&                /*C32*/
                (part_table[j][i].boot_ind == 0x80))                    /*C32*/                             /* AN000 */
                if ((part_table[j][i].drive_letter == c(' ')) &&        /*C32*/
                    (next_letter <= c('Z')))                            /*C32*/
                    part_table[j][i].drive_letter = next_letter++;      /*C32*/                         /* AN000 */
           END                                                          /*C32*/
        /* Look at all non-active primary partitions */                 /*C32*/
        for (i=u(0); i < u(4); i++)                                     /*C32*/
           BEGIN                                                        /*C32*/
            /* Set drive letter */                                      /*C32*/
            if (((part_table[j][i].sys_id == DOS12) ||                  /*C32*/
                 (part_table[j][i].sys_id == DOS16) ||                  /*C32*/
                 (part_table[j][i].sys_id == DOSNEW)) &&                /*C32*/
                (part_table[j][i].boot_ind != 0x80))                    /*C32*/                             /* AN000 */
                if ((part_table[j][i].drive_letter == c(' ')) &&        /*C32*/
                    (next_letter <= c('Z')))                            /*C32*/
                    part_table[j][i].drive_letter = next_letter++;      /*C32*/                         /* AN000 */
           END                                                          /*C32*/
       END                                                              /*C32*/
                                                                        /*C32*/
    return;                                                             /*C32*/
                                                                        /*C32*/
END                                                                     /*C32*/
