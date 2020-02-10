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
*  03/19/90  EGH  C11  Problem - the change current disk menu hangs if one of
*                      the drives is bad.  Fix is to not use the routine
*                      get_part_free_space().  This fix will also eliminate the
*                      delay that occurs when switching to this menu.
*  05/24/90  EGH  C19  Problem - garbage was appearing on the change current
*                      disk screen when the maximum number of logical drives
*                      were present.  Fix is to initialize all 32 entries of
*                      the variable INSERT rather than just 31.
*  10/01/90  EGH  C26  Problem - on the change current disk screen, garbage
*                      characters were appearing as drive letters on partitons
*                      past z:.
*  10/19/90  EGH  C32  Added code to handle multiple primary DOS partitions.
*
******************************************************************************/

#include "dos.h"                                                        /* AN000 */
#include "fdisk.h"                                                      /* AN000 */
#include "extern.h"                                                     /* AN000 */
#include "subtype.h"                                                    /* AN000 */
#include "fdiskmsg.h"                                                   /* AN000 */
#include "string.h"
#include "stdio.h"
#include "memory.h"

/*  */
char volume_display()

BEGIN

    unsigned    i;

/*C32    unsigned          k; */

/*C00    unsigned    x;      */
    char        drive_found;
    char        drive_letter;
    char        drive_num;
/*C32    char        temp;   */
    char        first_display;
    char        second_display;
    char        third_display;
    char        fourth_display;
    unsigned    insert_offset;

    first_display = FALSE;
    second_display = FALSE;
    third_display = FALSE;
    fourth_display = FALSE;

#if 0                                                                   /*C32*/
    /* See what the starting drive letter is */
    drive_letter = c(SEA);                                          /* AC000 */

    /* See if primary on drive 1 */
    temp = cur_disk;

    for (i=0; i < number_of_drives; i++)
        {
/*C00   cur_disk = i;         */
        cur_disk = c(i); /*C00*/

        if ((find_partition_type(uc(DOS12))) ||
            (find_partition_type(uc(DOS16))) ||
            (find_partition_type(uc(DOSNEW)))) /* AC000 */

            /* There is a Primary partition on drive 1, so increment for first logical drive */
            drive_letter++;
        }

    /* Are we on drive 2? If so, we got to find all the drives on drive 1 */

    /* Get drive information and put into array */
    for (k=0; k < temp; k++)
        {
        /* Next, we need to see what is on drive 1 */
        for (i=u(0); i < u(23); i++)                            /* AC000 */
            BEGIN
            /* See if there is a logical drive we understand in PC-DOS land */
            if ( (ext_table[k][i].sys_id == uc(DOS12)) ||
                 (ext_table[k][i].sys_id == uc(DOS16)) ||
                 (ext_table[k][i].sys_id == uc(DOSNEW)) )                    /* AC000  */
                BEGIN
                /* Found one, so kick up the first available drive letter */
                drive_letter++;
                END
            END
        }

    /* Reset the cur_drive to where it was */
    cur_disk = temp;
#endif                                                                  /*C32*/

    /* get the current drive letters */                                 /*C32*/
    get_letters();                                                      /*C32*/

    /* loop thru the partitions, only print stuff if it is there */

    /* Get the drives in order by location on disk */
    sort_ext_table(c(23));                                          /* AC000 */

    /* initialize all the inserts to blanks */
    memset(insert,c(' '),(24*29));

    drive_num = c(0);                                               /* AC000 */
    drive_found = FALSE;
    first_display = TRUE;
    insert_offset = 0;

    for (i=u(0); i < u(23); i++)                                    /* AC000 */
       BEGIN

        /* See if entry exists */
        if ( (ext_table[cur_disk][sort[i]].sys_id == uc(DOS12)) ||
             (ext_table[cur_disk][sort[i]].sys_id == uc(DOS16)) ||
             (ext_table[cur_disk][sort[i]].sys_id == uc(DOSNEW)) )  /* AC000  */
           BEGIN

            /* We found one, now get the info */
            drive_found = TRUE;

/*C32*/     /* Get the drive letter - make sure it is Z: or less*/
/*C32*/     /* Put it in the message, and set it up for next time */
/*C32       if (drive_letter > c('Z'))                                     */
/*C32               ext_table[cur_disk][sort[i]].drive_letter = c(' ');    */
/*C32       else ext_table[cur_disk][sort[i]].drive_letter = drive_letter; */

            insert_offset += sprintf(&insert[insert_offset],"%c%c%-11.11s%4.0d%-8.8s%3.0d%%",
                    ext_table[cur_disk][sort[i]].drive_letter,
                    ( ext_table[cur_disk][sort[i]].drive_letter == c(' ') ) ? ' ' : ':',
                    ext_table[cur_disk][sort[i]].vol_label,
                    ext_table[cur_disk][sort[i]].mbytes_used,
                    ext_table[cur_disk][sort[i]].system,
                    ext_table[cur_disk][sort[i]].percent_used );


/*C32       drive_letter++; */
            drive_letter = ext_table[cur_disk][sort[i]].drive_letter;   /*C32*/
            drive_num++;

           END
       END

    /* Display the column of drives */
    if (drive_found)
       BEGIN

        clear_screen(u(2),u(0),u(15),u(79));                    /* AC000 */

        if ( drive_num > 0 )
            BEGIN
            pinsert = &insert[0];
            display(menu_19);
            END

        if ( drive_num > 6 )
            BEGIN
            pinsert = &insert[6*29];
            display(menu_43);
            END

        if ( drive_num > 12 )
            BEGIN
            pinsert = &insert[12*29];
            display(menu_20);
            END

        if ( drive_num > 18 )
            BEGIN
            pinsert = &insert[18*29];
            display(menu_44);
            END
        pinsert = &insert[0];
        END
    else
       BEGIN
        /* Didn't find any */
        if (first_display)
           BEGIN
            /* Wipe out display and put up message */
            clear_screen(u(2),u(0),u(15),u(79));                    /* AC000 */
            display(status_9);
           END
       END
    /* Return the highest drive letter found */
/*C32    drive_letter--; */
    return(drive_letter);

END


/******************************************************************************/
/*Routine name:  CHANGE_CURRENT_DRIVE                                         */
/******************************************************************************/
/*                                                                            */
/*Description:   This routine will change the current drive for FDISK         */
/*               to any valid drive (up to 7).  It will also display a        */
/*               menu with disk information on all disks available for        */
/*               the system be FDISKed.                                       */
/*                                                                            */
/*Called Procedures:    Display                                               */
/*                      Find_part_free_space                                  */
/*                      Sort_Ext_Table                                        */
/*                      Get_Num_Input                                         */
/*                      Clear_Screen                                          */
/*                                                                            */
/*Change History: Created        1/06/89         DRM                          */
/*                                                                            */
/*Input:  None                                                                */
/*                                                                            */
/*Output: None                                                                */
/*                                                                            */
/******************************************************************************/
void change_current_drive()                                         /* BN000 */

BEGIN                                                               /* BN000 */

    unsigned    i;                                                  /* BN000 */
/*C00    unsigned    j;    */                                                      /* BN000 */
    unsigned    k;                                                  /* BN000 */
/*C32    char        drive_letter;         */                            /* BN000 */
/*C32    char        primary_drive_letter; */                            /* BN000 */
    char        drive_num;                                          /* BN000 */
    char        temp;                                               /* BN000 */
/*C11    char        temp1; */                                      /* BN000 */
    unsigned    temp1;                                                  /*C11*/
    unsigned    mbytes_free;                                            /*C11*/
    unsigned    percent_used;                                           /*C11*/
    unsigned    insert_offset;                                      /* BN000 */
    unsigned    input;                                              /* BN000 */
    char        max_input;                                          /* BN000 */
/*C00    char        blanks[9]; */                                                 /* BN000 */

    /* Initialize some variables */
    insert_offset = 0;                                              /* BN000 */
    drive_num = c(0);                                               /* BN000 */
/*C32    primary_drive_letter = c(SEA); */                               /* BN000 */

    /* get the current drive letters */                                 /*C32*/
    get_letters();                                                      /*C32*/

    /* Clear the whole screen for display */
    clear_screen(u(0),u(0),u(24),u(79));                            /* BN000 */

    /* initialize all the inserts to blanks */
/*C19    memset(insert,c(' '),(31*15)); */                                 /* BN000 */
    memset(insert,c(' '),(32*15));                                      /*C19*/

    /* Save current disk */
    temp = cur_disk;                                                /* BN000 */

    /* Display the heading */
    if (status_flag == FALSE)                                       /* BN000 */
        display(menu_47);                                           /* BN000 */
    else                                                            /* BN000 */
        display(menu_54);                                           /* BN000 */

    /* Figure out which drive letter the extended partitions begin with  */
/*C32    drive_letter = (c(SEA) + primary_partition_count); */           /* BN000 */

    /* Get the drive information in ascending order and put into array */
    for (i=u(0); i < u(number_of_drives); i++)                      /* BN000 */

        BEGIN                                                           /* BN000 */

        /* find freespace on current drive and display drive information */
        cur_disk = c(i);                                            /* BN000 */
/*C11   temp1 = find_part_free_space(PRIMARY); */                   /* BN000 */
        temp1 = part_table[cur_disk][0].mbytes_used +                   /*C11*/
                part_table[cur_disk][1].mbytes_used +                   /*C11*/
                part_table[cur_disk][2].mbytes_used +                   /*C11*/
                part_table[cur_disk][3].mbytes_used;                    /*C11*/
        if (temp1 < total_mbytes[cur_disk])                             /*C11*/
            mbytes_free = total_mbytes[cur_disk] - temp1;               /*C11*/
        else                                                            /*C11*/
            mbytes_free = u(0);                                         /*C11*/
        temp1 = part_table[cur_disk][0].percent_used +                  /*C11*/
                part_table[cur_disk][1].percent_used +                  /*C11*/
                part_table[cur_disk][2].percent_used +                  /*C11*/
                part_table[cur_disk][3].percent_used;                   /*C11*/
        if (temp1 < u(100))                                             /*C11*/
            percent_used = temp1;                                       /*C11*/
        else                                                            /*C11*/
            percent_used = u(100);                                      /*C11*/
        insert_offset += sprintf(&insert[insert_offset],"%1.0d%c%c%4.0d%4.0d%3.0d%%",
            (cur_disk + 1),
            ' ',
            ' ',
            total_mbytes[cur_disk],
/*C11       free_space[temp1].mbytes_unused,               */
/*C11       (u(100) - free_space[temp1].percent_unused));  */       /* BN000 */
            mbytes_free,                                                /*C11*/
            percent_used);                                              /*C11*/
        drive_num++;                                                /* BN000 */

        /* Sort the partitions */
        sort_part_table(c(4));                                      /* BN000 */
        /* Now look at the partitions in order */
        for (k=u(0); k < u(4); k++)                                 /* BN000 */
            BEGIN                                                       /* BN000 */
            /* see if there is a primary partition */
            if( (part_table[cur_disk][sort[k]].sys_id == DOSNEW) ||
                (part_table[cur_disk][sort[k]].sys_id == DOS16) ||
                (part_table[cur_disk][sort[k]].sys_id == DOS12) )
                BEGIN                                                   /* BN000 */
                insert_offset += sprintf(&insert[insert_offset],"%c%c%c%4.0d%8c",
                    ' ',
/*C32               c(primary_drive_letter),               */
/*C32               (drive_letter == c(' ') ) ? ' ' : ':', */
                    part_table[cur_disk][sort[k]].drive_letter,         /*C32*/
                    (part_table[cur_disk][sort[k]].drive_letter == c(' ') ) ? ' ' : ':', /*C32*/     /*C32*/
                    part_table[cur_disk][sort[k]].mbytes_used,
                    ' ');                                           /* BN000 */
                    drive_num++;                                    /* BN000 */
/*C32               primary_drive_letter++; */                      /* BN000 */
                END                                                     /* BN000 */
            END                                                         /* BN000 */

        /* loop thru the partitions, only print stuff if it is there */

        /* Get the drives in order by location on disk */
        sort_ext_table(c(23));                                      /* BN000 */

        for (k=u(0); k < u(23); k++)                                /* BN000 */
            BEGIN                                                       /* BN000 */
            /* if last drive letter was Z, print spaces */
/*C32       if ((drive_letter > c('Z')) ) */
/*C32            drive_letter = c(' ');   */                        /* BN000 */
            /* If it has a size, print it */
            if ( (ext_table[cur_disk][sort[k]].mbytes_used != u(0)) ||
                 (ext_table[cur_disk][sort[k]].percent_used != u(0)) ) /* BN000 */

                BEGIN
                insert_offset += sprintf(&insert[insert_offset],"%c%c%c%4.0d%8c",
                    ' ',
/*C32               drive_letter, */
/*C32               (drive_letter == c(' ') ) ? ' ' : ':', */
                    ext_table[cur_disk][sort[k]].drive_letter,          /*C32*/
                    (ext_table[cur_disk][sort[k]].drive_letter == c(' ') ) ? ' ' : ':', /*C32*/
                    ext_table[cur_disk][sort[k]].mbytes_used,
                    ' ');                                           /* BN000 */
                    drive_num++;                                    /* BN000 */
/*C32               drive_letter++; */                              /* BN000 */
                END                                                     /* BN000 */
            END                                                         /* BN000 */
        END                                                             /* BN000 */

    /* Display the column of drives */
    BEGIN                                                           /* BN000 */

    if ( drive_num > 0 )                                            /* BN000 */
        BEGIN                                                       /* BN000 */
        pinsert = &insert[0];                                       /* BN000 */
        display(menu_48);                                           /* BN000 */
        END                                                         /* BN000 */

    if ( drive_num > 8 )                                            /* BN000 */
        BEGIN                                                       /* BN000 */
        pinsert = &insert[8*15];                                    /* BN000 */
        display(menu_49);                                           /* BN000 */
        END                                                         /* BN000 */

    if ( drive_num > 16 )                                           /* BN000 */
        BEGIN                                                       /* BN000 */
        pinsert = &insert[16*15];                                   /* BN000 */
        display(menu_50);                                           /* BN000 */
        END                                                         /* BN000 */

    if ( drive_num > 24 )                                           /* BN000 */
        BEGIN                                                       /* BN000 */
        pinsert = &insert[24*15];                                   /* BN000 */
        display(menu_51);                                           /* BN000 */
        END                                                         /* BN000 */
    pinsert = &insert[0];                                           /* BN000 */
    END                                                             /* BN000 */

    /* display Mbytes = line */
    display(menu_52);                                               /* BN000 */

    /* reset current disk back to original */
    cur_disk = temp;                                                /* BN000 */

    if (status_flag == FALSE)                                       /* BN000 */
        BEGIN
        /* Display prompt of which disk */
        sprintf(insert,"%1.0d%1.0d",number_of_drives,(cur_disk+1)); /* BN000 */
        display(menu_53);                                           /* BN000 */

        /* ESC line at bootom of screen */
        display(menu_11);                                           /* BN000 */

        max_input = c(number_of_drives);                            /* BN000 */

        valid_input = (FLAG)FALSE;                                  /* BN000 */

        /* Get the input from screen */
        while (!valid_input)                                        /* BN000 */
            BEGIN                                                   /* BN000 */
            input = get_num_input(c(cur_disk+1),max_input,input_row,input_col);  /* BN000 */
            clear_screen(u(21),u(0),u(23),u(79));                   /* BN000 */

            /* set current disk to zero base */
            if (input != u(ESC)) cur_disk = c(input-'1');           /* BN000 */
            END                                                     /* BN000 */

        /* clear the screen before going back to main menu */
        clear_screen(u(0),u(0),u(24),u(79));                        /* BN000 */
        END                                                         /* BN000 */
    return;                                                         /* BN000 */
END                                                                     /* BN000 */

