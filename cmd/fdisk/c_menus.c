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
*  03/07/90  EGH  C02  Problem fixed - Cannot create logical volumes, reports
*                      no free space.  Fix allows creation, reports correct
*                      free space.
*  03/07/90  EGH  C06  Problem fixed - Allows creation of partitions when
*                      100% of disk is used.  Fix does not allow partition
*                      less than 64 sectors.
*  03/07/90  EGH  C07  Problem fixed - 'Internal error' received when creating
*                      24 logical volumes & no primary exists.  Fix prevents
*                      more than 23 logical volumes from being created.
*  03/20/90  EGH  C00  Cleaned up build by removing unused variables, declaring
*                      functions properly, changing long JMPs to short JMPs,
*                      etc.
*  03/20/90  EGH  C13  This message is now displayed in the wait_for_ESC()
*                      routine.
*  10/19/90  EGH  C32  Added code to handle multiple primary DOS partitions.
*
******************************************************************************/

#include "dos.h"                                                        /* AN000 */
#include "fdisk.h"                                                      /* AN000 */
#include "extern.h"                                                     /* AN000 */
#include "subtype.h"                                                    /* AN000 */
#include "fdiskmsg.h"                                                   /* AN000 */
#include "stdio.h"

/*  */
/******************* START OF SPECIFICATIONS *******************/
/*                                                             */
/* SUBROUTINE NAME: CREATE_PARTITION                           */
/*                                                             */
/* DESCRIPTIVE NAME: Create DOS related partition(s)           */
/*                                                             */
/* FUNCTION:                                                   */
/*      This routine verifies if there are free partitions,    */
/*      posts an status message if there is not, otherwise     */
/*      prints a screen asking what type of partition to       */
/*      be created, and passes control to the requested        */
/*      function.                                              */
/*                                                             */
/* NOTES: This is a screen control module only, no data is     */
/*        modified. Routine also will only allow 1 DOS and     */
/*        1 Ext DOS partitions per disk, if one already exists,*/
/*        then status message is displayed when the create     */
/*        option for that type partition is selected           */
/*                                                             */
/*        The following screen in managed                      */
/*                                                             */
/*       �0000000000111111111122222222223333333333�            */
/*       �0123456789012345678901234567890123456789�            */
/*     ������������������������������������������Ĵ            */
/*     00�                                        �            */
/*     01�                                        �            */
/*     02�                                        �            */
/*     03�                                        �            */
/*     04�Create DOS Partition                    �            */
/*     05�                                        �            */
/*     06�Current Fixed Disk Drive: #             �            */
/*     07�                                        �            */
/*     08�Choose one of the following:            �            */
/*     09�                                        �            */
/*     10�    1.  Create Primary DOS partition    �            */
/*     11�    2.  Create EXTENDED DOS partition   �            */
/*     12�    3.  Create logical DOS drive(s) in  �            */
/*     13�        the EXTENDED DOS partition      �            */
/*     14�                                        �            */
/*     15�                                        �            */
/*     16�                                        �            */
/*     17�                                        �            */
/*     18�Enter choice: [ ]                       �            */
/*     19�                                        �            */
/*     20�                                        �            */
/*     21�                                        �            */
/*     22�                                        �            */
/*     23�Press ESC to return to FDISK Options    �            */
/*     ��������������������������������������������            */
/*                                                             */
/* ENTRY POINTS: create_partition                              */
/*      LINKAGE: create_partition();                           */
/*                                                             */
/* INPUT: None                                                 */
/*                                                             */
/* EXIT-NORMAL: ERROR=FALSE                                    */
/*                                                             */
/* EXIT-ERROR: ERROR=TRUE                                      */
/*             GOTO internal_program_error if case statement   */
/*             failure when branching to requested function    */
/*                                                             */
/* EFFECTS: No data directly modified by this routine, but     */
/*          child routines will modify data.                   */
/*                                                             */
/* INTERNAL REFERENCES:                                        */
/*   ROUTINES:                                                 */
/*      find_free_partition                                    */
/*      dos_create_partition                                   */
/*      ext_create_partition                                   */
/*      volume_create                                          */
/*      internal_program_error                                 */
/*      find_partition_type                                    */
/*      get_num_input                                          */
/*      display                                                */
/*      wait_for_ESC                                           */
/*      clear_screen                                           */
/*                                                             */
/* EXTERNAL REFERENCES:                                        */
/*   ROUTINES:                                                 */
/*                                                             */
/******************** END OF SPECIFICATIONS ********************/

/*  */
void create_partition()

BEGIN

char   input;
char   default_value;
char   max_input;




    input = c(NUL);                                                     /* AC000 */
    clear_screen(u(0),u(0),u(24),u(79));                                /* AC000 */
    /* put up heading and ESC */
    display(menu_8);
    display(menu_11);

    /* Setup current drive msg */
    insert[0]=cur_disk+1+'0';
    display(menu_5);

    /* See if there are free partitions */
    /* SR; 9/26/89; This check should be done only if the user asks to
       create a primary or extended partition and not for logical volumes */
/**SR; 9/26/89;     if (find_free_partition() != ((char)(NOT_FOUND)))                  /* AC000 */
/**SR; 9/26/89;         BEGIN	***/

        /* display menu */
        display(menu_3);                                                /* AN000 */
        display(menu_9);

        /* ############# ADD CODE HERE FOR THIS FUNCTION ############## */
        /* Do something about highlighting the available options and    */
        /* setting up defaults                                          */
        default_value = c(1);                                          /* AC000 */
        /* ############################################################ */
        /* setup default for prompt */
        insert[0] = c('1');                                            /* AC000 */
        display(menu_7);
        display(menu_10);

        max_input = c(3);                                               /* AC000 */

        input = get_num_input(default_value,max_input,input_row,input_col);

        /* SR; 9/26/89; We check here for the extended volume creation case
           and remove it from the switch statement */

        if (input == '3')
           {       /* Create a volume */
            if (find_partition_type(uc(EXTENDED)))
                volume_create();
            else
               {
                display(error_35);
                clear_screen(u(17),u(0),u(17),u(79));
                wait_for_ESC();
               }
           }
        else if (find_free_partition() != ((char)(NOT_FOUND)))
           {

            /* Go branch to the requested function */
            switch(input)
               BEGIN
                case '1':
                    dos_create_partition();
                    break;

            	case '2':
                    if ((cur_disk >= c(1)) || (find_partition_type(uc(DOS12))) || (find_partition_type(uc(DOS16))) ||
                        (find_partition_type(uc(DOSNEW))))                   /* AN000 */                         /* AC000 */
                    	ext_create_partition();
                    else
                       BEGIN                                               /* AN000 */
                    	/* don't have a primary partition yet, can't create an ext */
                    	display(error_19);                                  /* AN000 */
                    	clear_screen(u(17),u(0),u(17),u(79));               /* AN000 */
                    	wait_for_ESC();                                     /* AN000 */
                       END                                                 /* AN000 */
                    break;

/* SR; 9/26/89; This case has been handled before the switch  */
#if 0
                case '3':
                   BEGIN
                    if (find_partition_type(uc(EXTENDED)))                  /* AC000 */
                        volume_create();
                    else                                                    /* AN000 */
                       BEGIN                                               /* AN000 */
                    	display(error_35);                                  /* AN000 */
                    	clear_screen(u(17),u(0),u(17),u(79));               /* AN000 */
                    	wait_for_ESC();                                     /* AN000 */
                       END                                                 /* AN000 */
                    break;
                   END
#endif

                case ESC:
                    break;

                default:
                    internal_program_error();
                    break;

               END
           }
        else
           {
            /* Display prompt telling there is no avail partition */
            display(error_10);
            input = wait_for_ESC();
           }
    /* clear the screen before going back to main menu */
    clear_screen(u(0),u(0),u(24),u(79));                               /* AC000 */
    return;
END


/*  */
/******************* START OF SPECIFICATIONS *******************/
/*                                                             */
/* SUBROUTINE NAME: DOS_CREATE_PARTITION                       */
/*                                                             */
/* DESCRIPTIVE NAME: Create default DOS partition on disk      */
/*                                                             */
/* FUNCTION: User is prompted to see if he wishes to use to    */
/*           set up a DOS partition in the maximum available   */
/*           size (limited to 32mb). If option is selected     */
/*           than partition is created and marked active. The  */
/*           partition is scanned to insure there are enough   */
/*           contiguous good sectors for DOS.                  */
/*                                                             */
/* NOTES: Screen can be exited via the ESC command before      */
/*        partition is created and nothing will change         */
/*                                                             */
/*        The following screen is managed:                     */
/*                                                             */
/*       �0000000000111111111122222222223333333333�            */
/*       �0123456789012345678901234567890123456789�            */
/*     ������������������������������������������Ĵ            */
/*     00�                                        �            */
/*     01�                                        �            */
/*     02�                                        �            */
/*     03�                                        �            */
/*     04�Create DOS Partition                    �            */
/*     05�                                        �            */
/*     06�Current Fixed Disk Drive: #             �            */
/*     07�                                        �            */
/*     08�Do you wish to use the maximum size     �            */
/*     09�for a DOS partition and make the DOS    �            */
/*     10�partition active (Y/N).........? [Y]    �            */
/*     11�                                        �            */
/*     12�                                        �            */
/*     13�                                        �            */
/*     14�                                        �            */
/*     15�                                        �            */
/*     16�                                        �            */
/*     17�                                        �            */
/*     18�                                        �            */
/*     19�                                        �            */
/*     20�                                        �            */
/*     21�                                        �            */
/*     22�                                        �            */
/*     23�Press ESC to return to FDISK Options    �            */
/*     ��������������������������������������������            */
/*                                                             */
/* ENTRY POINTS: dos_create_partition                          */
/*      LINKAGE: dos_create_partition();                       */
/*               NEAR CALL                                     */
/*                                                             */
/* INPUT: None                                                 */
/*                                                             */
/* EXIT-NORMAL: ERROR=FALSE                                    */
/*                                                             */
/* EXIT-ERROR: ERROR=TRUE                                      */
/*             GOTO internal_program_error if case statement   */
/*             failure when branching to requested function    */
/*                                                             */
/* EFFECTS: No data directly modified by this routine, but     */
/*          child routines will modify data.                   */
/*                                                             */
/* INTERNAL REFERENCES:                                        */
/*   ROUTINES:                                                 */
/*      clear_screen                                           */
/*      display                                                */
/*      get_yn_input                                           */
/*      wait_for_ESC                                           */
/*      input_dos_create                                       */
/*      make_partition                                         */
/*      check_bad_tracks                                       */
/*                                                             */
/*                                                             */
/* EXTERNAL REFERENCES:                                        */
/*   ROUTINES:                                                 */
/*                                                             */
/******************** END OF SPECIFICATIONS ********************/

/*  */
void dos_create_partition()


BEGIN

    char   input;
    char   temp;
    char   second_disk_flag;                                            /* AN000 */

    /* SR; 9/26/89; These variables are added for the various fixes */

    unsigned long total_sectors;
    unsigned char  i;
    char  defined_drives;
    char  temp_cur_disk;

    second_disk_flag = (FLAG)FALSE;                                     /* AN000 */
    input = c(NUL);                                                     /* AC000 */
    /* clear off screen */
    clear_screen(u(0),u(0),u(24),u(79));                                /* AC000 */

    /* Put up header */
    display(menu_12);

    /* Set up current disk message */
    insert[0] = cur_disk+1+'0';
    display(menu_5);

    /* Display ESC prompt */
    display(menu_11);

    /* See if already exists */
    if ((!find_partition_type(uc(DOS12))) && (!find_partition_type(uc(DOS16))) && (!find_partition_type(uc(DOSNEW))))  /* AC000 */

       BEGIN

        /* SR; 9/26/89; Ported from Olivetti fix. The min. no. of sectors
           in a partition must be >64 for BIOS to recognize the drive */

        temp = find_part_free_space(c(PRIMARY));
        total_sectors = (long)free_space[temp].space * max_sector[cur_disk] * max_head[cur_disk];
        if (total_sectors < 64)
           {
            display(error_10);      /**** No room on disk */
            input = wait_for_ESC();
            return;  /*C06*/
           }
        /* SR; 9/26/89; We do not allow creation of a drive on the second
           hard disk if there are already 23 drives on the 1st hard disk */
/*C07   else if (cur_disk == c(1))  */
        else                   /*C07*/
           {
            defined_drives = c(0);
            temp_cur_disk = cur_disk;

            /* Search both drives for defined drives */
            for (i = 0; i < number_of_drives; i++)
               {
                cur_disk = (c(i));

                /* See if there is a primary drive letter */
                if ((find_partition_type(uc(DOS12))) ||
                    (find_partition_type(uc(DOS16))) ||
                    (find_partition_type(uc(DOSNEW))))
                    defined_drives++;

                /* See if extended partition on disk */
                if (find_partition_type(uc(EXTENDED)))
                    /* Get number of logical drives */
                    defined_drives += get_num_logical_dos_drives();
               }
            /* Restore cur_disk  to original */
            cur_disk = temp_cur_disk;

            /* See if 26 or less drives total */
            if (defined_drives > c(23))
               /* Reached the maximum */
               {
                clear_screen(u(16),u(0),u(23),u(79));
                display(error_27);
                input = wait_for_ESC();
                return;
               }
           }

        /* Display prompt, depending on what disk */
        if (cur_disk == c(0))                                           /* AC000 */
            /* Put up make active partition message */
            display(menu_13);
        else
            BEGIN
            /* Second disk, so don;t put up prompt mentioning active partition */
            second_disk_flag = (FLAG)TRUE;                              /* AN000 */
            display(menu_45);                                           /* AC000 */
            END
        /* Get Y/N input */
        input = get_yn_input(c(Yes),input_row,input_col);               /* AC000 AC011 */

        /* Go handle input */
        switch(input)
            BEGIN
            case 1:                                                     /* AC000 */
                if ( second_disk_flag == (FLAG)FALSE)
                    BEGIN
                    /* Go get the biggest area left */
                    temp = find_part_free_space(c(PRIMARY));            /* AC000 */
                    make_partition(free_space[temp].space,temp,uc(ACTIVE),c(PRIMARY)); /* AC000 */
                    reboot_flag = (FLAG)TRUE;                           /* AC000 */
                    if (number_of_drives == uc(1))                      /* AN000 */
                        BEGIN                                           /* AN000 */
                        write_info_to_disk();
                        reboot_system();                                /* AC000 */
                        END                                             /* AN000 */
                    clear_screen(u(16),u(0),u(23),u(79));               /* AN000 */

                    /* made another partition, increment partition count */
/*C32               primary_partition_count++; */

                    display(status_12);                                 /* AN000 */
                    wait_for_ESC();
                    break;
                    END
                else
                    BEGIN                                               /* AN000 */
                    /* Go get the biggest area left */                  /* AN000 */
                    temp = find_part_free_space(c(PRIMARY));            /* AN000 */
                    make_partition(free_space[temp].space,temp,uc(NUL),c(PRIMARY)); /* AN000 */
                    reboot_flag = (FLAG)TRUE;                           /* AN000 */
                    clear_screen(u(16),u(0),u(23),u(79));               /* AN000 */

                    /* made another partition, increment partition count */
/*C32               primary_partition_count++; */

                    display(status_12);                                 /* AN000 */
                    wait_for_ESC();
                    break;
                    END

            case  0:
                input_dos_create();                               /* AC000 */
                break;

            case ESC:
                break;   /* take no action */

            default:
                internal_program_error();
            END
        END
    else
        BEGIN
        /* Display partition table-it will return if no partitions there */
        table_display();

        /* Primary partition already exists message */
        display(error_8);
        wait_for_ESC();
        END
    return;
END


/*  */
/******************* START OF SPECIFICATIONS *******************/
/*                                                             */
/* SUBROUTINE NAME: INPUT_DOS_CREATE                           */
/*                                                             */
/* DESCRIPTIVE NAME: Create DOS partition on disk              */
/*                                                             */
/* FUNCTION: Gets user specified size for partition (maximum   */
/*           is 32mb or largest contiguous freespace, which-   */
/*           ever is smaller). Default is largest avail free   */
/*           space. Partition is created to default size,unless*/
/*           user enters different size, but is not marked     */
/*           active. User specified size must be smaller or    */
/*           equal to the default size                         */
/*                                                             */
/* NOTES: Screen can be exited via the ESC command before      */
/*        partition is created and nothing will change         */
/*                                                             */
/*        The following screen is managed                      */
/*                                                             */
/*       �0000000000111111111122222222223333333333�            */
/*       �0123456789012345678901234567890123456789�            */
/*     ������������������������������������������Ĵ            */
/*     00�                                        �            */
/*     01�                                        �            */
/*     02�                                        �            */
/*     03�                                        �            */
/*     04�Create DOS partition                    �            */
/*     05�                                        �            */
/*     06�Current Fixed Disk Drive: #             �            */
/*     07�                                        �            */
/*     08�Partition Status   Type  Start  End Size�            */
/*     09�                                        �            */
/*     10�                                        �            */
/*     11�                                        �            */
/*     12�                                        �            */
/*     13�                                        �            */
/*     14�Total disk space is #### cylinders.     �            */
/*     15�Maximum space available for partition   �            */
/*     16�is #### cylinders.                      �            */
/*     17�                                        �            */
/*     18�Enter partition size............: [####]�            */
/*     19�                                        �            */
/*     20�                                        �            */
/*     21�                                        �            */
/*     22�                                        �            */
/*     23�Press ESC to return to FDISK Options    �            */
/*     ��������������������������������������������            */
/*                                                             */
/* ENTRY POINTS: input_dos_create                              */
/*      LINKAGE: input_dos_create();                           */
/*               NEAR CALL                                     */
/*                                                             */
/* INPUT: None                                                 */
/*                                                             */
/* EXIT-NORMAL: ERROR=FALSE                                    */
/*                                                             */
/* EXIT-ERROR: ERROR=TRUE                                      */
/*             GOTO internal_program_error if case statement   */
/*             failure when branching to requested function    */
/*                                                             */
/* EFFECTS: No data directly modified by this routine, but     */
/*          child routines will modify data.                   */
/*                                                             */
/* INTERNAL REFERENCES:                                        */
/*   ROUTINES:                                                 */
/*      clear_screen                                           */
/*      table_display                                          */
/*      get_num_input                                          */
/*      display                                                */
/*      wait_for_ESC                                           */
/*      make_partition                                         */
/*      check_bad_tracks                                       */
/*                                                             */
/* EXTERNAL REFERENCES:                                        */
/*   ROUTINES:                                                 */
/*                                                             */
/******************** END OF SPECIFICATIONS ********************/

/*  */
void input_dos_create()

BEGIN

    unsigned  input;
    unsigned  default_entry;
    char      temp;
/*C00    char      location;  */

    input = u(NUL);                                                     /* AC000 */
    /* clear off screen */
    clear_screen(u(0),u(0),u(24),u(79));                               /* AC000 */

    /* Put up heading */
    display(menu_12);

    /* Setup and print current disk */
    insert[0] = cur_disk+1+'0';
    display(menu_5);

    /* Print ESC prompt */
    display(menu_11);

    /* Display partition table-it will return if no partitions there */
    table_display();

    sprintf(insert,"%4.0d",total_mbytes[cur_disk]);
    display(menu_15);

    /* Get the free space */
    temp = find_part_free_space(c(PRIMARY));                           /* AC000 */

    /* Is there any ?*/
    if (free_space[temp].mbytes_unused != u(0))                        /* AC000 */

        BEGIN
        /* Display disk space */
        sprintf(insert,"%4.0d",total_mbytes[cur_disk]);
        display(menu_15);

        /* Setup and print max partition size */

        sprintf(insert,"%4.0d%3.0d%%",
                free_space[temp].mbytes_unused,
                free_space[temp].percent_unused);
        display(menu_16);

        /* Force repeats on the input until something valid (Non-Zero return) */
        default_entry = (unsigned)free_space[temp].mbytes_unused;      /* AC000 */
        valid_input = (FLAG)FALSE;                                     /* AC000 */

        while (!valid_input)

            BEGIN
            /* Display prompt */
            sprintf(insert,"%4.0d",default_entry);
            display(menu_39);

            input = get_large_num_input(default_entry,free_space[temp].mbytes_unused,free_space[temp].percent_unused,menu_39,u(0),error_13);       /* AC000 */

            /* Update default in case of error, so it gets displayed and used */
            /* if user presses CR only */

            default_entry = input;
            clear_screen(u(19),u(0),u(23),u(79));                      /* AC000 */
            END

        if (input != ((unsigned)(ESC_FLAG)))                               /* AC000 */

            BEGIN
            /* Change input to cylinders */
            /* check to see if input was in percent or mbytes */

            if (PercentFlag)                                          /* AN000 */
                BEGIN                                                 /* AN000 */
                if (input == free_space[temp].percent_unused)
                    input = free_space[temp].space;                   /* AN000 */
                else                                                  /* AN000 */
                    input = percent_to_cylinders(input,total_disk[cur_disk]);
                END                                                   /* AN000 */
            else                                                      /* AN000 */
                BEGIN                                                 /* AN000 */
                if (input == free_space[temp].mbytes_unused)
                    input = free_space[temp].space;                   /* AN000 */
                else                                                  /* AN000 */
                    input = (unsigned)mbytes_to_cylinders(input,
                                                          cur_disk);  /* AN004 */
                END                                                   /* AN000 */

            /* Initialize PecentFlag back to FALSE */
            PercentFlag = (FLAG)FALSE;                                  /* AN000 */

            /* SR; 9/26/89; Display error message if input value = 0 */

            if (input == 0)
               {
                display(error_28);
                wait_for_ESC();
               }
            else
               {
            	/* Go create the partition */
            	make_partition(input,temp,uc(NUL),c(PRIMARY));            /* AC000 */

            	/* clear off the old prompt */
            	clear_screen(u(13),u(0),u(19),u(79));                     /* AC000 */

                /* made another partition, increment partition count */
/*C32           primary_partition_count++; */

            	/* Reissue the partition info */
            	table_display();

            	/* display the "okay, we did it" msg */
            	if (number_of_drives == uc(1))                              /* AN000 */
                	display(status_5);
            	else
                    BEGIN                                                   /* AN000 */
                    clear_screen(u(16),u(0),u(23),u(79));                   /* AN000 */
                    display(status_12);                                     /* AN000 */
                    END                                                     /* AN000 */

            	wait_for_ESC();

            	reboot_flag = TRUE;
               }

            END
        END
    /* SR; 9/26/89; Display error if insufficient space for partition */
    else
        {
        display(error_10);
        wait_for_ESC();
        }
    return;
END


/*  */
/******************* START OF SPECIFICATIONS *******************/
/*                                                             */
/* SUBROUTINE NAME: EXT_CREATE_PARTITION                       */
/*                                                             */
/* DESCRIPTIVE NAME: Create EXTENDED DOS partition             */
/*                                                             */
/* FUNCTION: Gets user specified size for EXTENDED partition   */
/*           (Maximum is largest contiguous freespace). The    */
/*           default is the largest available freespace.       */
/*           space. Partition is created to default size,      */
/*           unless user enters different size, but is not     */
/*           marked as active. User specified size must be     */
/*           smaller or equal to default size                  */
/*                                                             */
/* NOTES: Screen can be exited via the ESC command before      */
/*        partition is created and nothing will change         */
/*                                                             */
/*        The following screen is managed                      */
/*                                                             */
/*    �0000000000111111111122222222223333333333�               */
/*    �0123456789012345678901234567890123456789�               */
/*  ������������������������������������������Ĵ               */
/*  00�                                        �               */
/*  01�                                        �               */
/*  02�                                        �               */
/*  03�                                        �               */
/*  04�Create EXTENDED DOS partition           �               */
/*  05�                                        �               */
/*  06�Current Fixed Disk Drive: #             �               */
/*  07�                                        �               */
/*  08�Partition Status   Type  Start  End Size�               */
/*  09�                                        �               */
/*  10�                                        �               */
/*  11�                                        �               */
/*  12�                                        �               */
/*  13�                                        �               */
/*  14�Total disk space is  #### cylinders.    �               */
/*  15�Maximum space available for partition   �               */
/*  16�is #### cylinders.                      �               */
/*  17�                                        �               */
/*  18�Enter partition size............: [####]�               */
/*  19�                                        �               */
/*  20�                                        �               */
/*  21�                                        �               */
/*  22�                                        �               */
/*  23�Press ESC to return to FDISK Options    �               */
/*  ��������������������������������������������               */
/*                                                             */
/* ENTRY POINTS: EXTENDED_create_partition                     */
/*      LINKAGE: EXTENDED_create_partition();                  */
/*               NEAR CALL                                     */
/*                                                             */
/* INPUT: None                                                 */
/*                                                             */
/* EXIT-NORMAL: ERROR=FALSE                                    */
/*                                                             */
/* EXIT-ERROR: ERROR=TRUE                                      */
/*             GOTO internal_program_error if case statement   */
/*             failure when branching to requested function    */
/*                                                             */
/* EFFECTS: No data directly modified by this routine, but     */
/*          child routines will modify data.                   */
/*                                                             */
/* INTERNAL REFERENCES:                                        */
/*   ROUTINES:                                                 */
/*      clear_screen                                           */
/*      table_display                                          */
/*      get_num_input                                          */
/*      display                                                */
/*      find_partition_type                                    */
/*      wait_for_ESC                                           */
/*      make_partition                                         */
/*                                                             */
/* EXTERNAL REFERENCES:                                        */
/*   ROUTINES:                                                 */
/*                                                             */
/******************** END OF SPECIFICATIONS ********************/

/*  */
void ext_create_partition()


BEGIN

    unsigned  input;
    unsigned  default_entry;
    char      temp;


    input = u(NUL);                                                    /* AC000 */
    /* clear off screen */
    clear_screen(u(0),u(0),u(24),u(79));                               /* AC000 */

    /* Put up heading */
    display(menu_17);

    /* Setup and print current disk */
    insert[0] = cur_disk+1+'0';
    display(menu_5);

    /* print ESC prompt */
    display(menu_11);

    /* Display partition table-it will return if no partitions there */
        table_display();

    /* Go see if primary already exists and ext doesn't */
    if ((cur_disk >= c(1)) || (find_partition_type(uc(DOS12))) || (find_partition_type(uc(DOS16))) ||
        (find_partition_type(uc(DOSNEW))))                                            /* AC000 */
        BEGIN
        if (!find_partition_type(uc(EXTENDED)))                         /* AC000 */
            /* We can go create one now */
            BEGIN

            /* Get the free space */
            temp = find_part_free_space(c(EXTENDED));                    /* AC000 */

            /* Is there any ?*/
            /* SR; 9/26/89; Instead of checking only the percentage which
               could be off because of rounding errors, we also check the
               mbytes available */

            if (free_space[temp].space != u(0) &&                  /* AC000 */
                (free_space[temp].percent_unused != u(0) ||
                 free_space[temp].mbytes_unused != u(0)))
                BEGIN

                /* Display disk space */
                sprintf(insert,"%4.0d",total_mbytes[cur_disk]);
                display(menu_15);

                /* Setup and print max partition size */

                sprintf(insert,"%4.0d%3.0d%%",
                        free_space[temp].mbytes_unused,
                        free_space[temp].percent_unused);
                display(menu_16);

                /* Force repeats on the input until something valid (Non-Zero return) */
                /* Display MBytes unless MBytes == 0, then display percent */
                if (free_space[temp].mbytes_unused == u(0))             /* AN000 */
                    BEGIN                                               /* AN000 */
                    default_entry = (unsigned)free_space[temp].percent_unused; /* AC000 */
                    PercentFlag = (FLAG)TRUE;                           /* AN000 */
                    END                                                 /* AN000 */
                else                                                    /* AN000 */
                    BEGIN
                    default_entry = (unsigned)free_space[temp].mbytes_unused; /* AC000 */
                    PercentFlag = (FLAG)FALSE;                          /* AN000 */
                    END

                valid_input = (FLAG)FALSE;                              /* AC000 */

                while (!valid_input)
                    BEGIN
                    /* Display prompt */
                    if (!PercentFlag)                                   /* AN000 */
                        sprintf(insert,"%4.0d",default_entry);
                    else                                                /* AN000 */
                        sprintf(insert,"%3.0d%%",default_entry);        /* AN000 */
                    display(menu_42);                                   /* AC000 */

                    input = get_large_num_input(default_entry,free_space[temp].mbytes_unused,free_space[temp].percent_unused,menu_42,u(0),error_13);   /*  AC000 */

                    /* Update default in case of error, so it gets displayed and used */
                    /* if user presses CR only */

                    default_entry = input;
                    clear_screen(u(19),u(0),u(23),u(79));                /* AC000 */
                    END

                if (input != ((unsigned)(ESC_FLAG)))                          /* AC000 */
                    BEGIN

                    /* Change input to cylinders */
                    if (PercentFlag)                                          /* AN000 */
                        BEGIN                                                 /* AN000 */
                        if (input == free_space[temp].percent_unused)
                            input = free_space[temp].space;                   /* AN000 */
                        else                                                  /* AN000 */
                            input = percent_to_cylinders(input,total_disk[cur_disk]);
                        END                                                   /* AN000 */
                    else                                                      /* AN000 */
                        BEGIN                                                 /* AN000 */
                        if (input == free_space[temp].mbytes_unused)
                            input = free_space[temp].space;                   /* AN000 */
                        else                                                  /* AN000 */
                            input = (unsigned)mbytes_to_cylinders(input,
                                                                  cur_disk);  /* AN004 */
                        END                                                   /* AN000 */


                    /* Initialize PecentFlag back to FALSE */
                    PercentFlag = (FLAG)FALSE;                                  /* AN000 */

                    /* SR; 9/26/89; Display error message if input value = 0 */

                    if (input == 0)
                       {
                        display(error_28);
                        wait_for_ESC();
                       }
                    else
                       {

                    	/* Go create the partition */
                    	make_partition(input,temp,uc(NUL),c(EXTENDED));     /* AC000 */

                    	/* clear off the old prompt */
                    	clear_screen(u(13),u(0),u(19),u(79));               /* AC000 */

                    	/* Display the updated partition information */
                    	table_display();

                    	/* Hit esc to continue line */
/*C13                   clear_screen(u(24),u(0),u(24),u(79));  */           /* AN000 */
/*C13                   display(menu_46);                      */           /* AN000 */

                    	/* Tell user we created it */
                    	display(status_6);
                    	wait_for_ESC();

                    	reboot_flag = (FLAG)TRUE;                           /* AC000 */

                    	/* Go allow him to create disk volumes */
                    	volume_create();
                       }
                    END
                END
            else
                BEGIN
                /* No room */
                display(error_10);
                wait_for_ESC();
                END
            END
        else
            BEGIN
            /* Already have ext partition, tell user and bow out */
            display(error_9);
            wait_for_ESC();
            END
        END
    else
        BEGIN
        /* don't have a primary partition yet, can't create an ext */
        display(error_19);
        wait_for_ESC();
        END

    return;
END


/*  */
/******************* START OF SPECIFICATIONS *******************/
/*                                                             */
/* SUBROUTINE NAME: VOLUME_CREATE                              */
/*                                                             */
/* DESCRIPTIVE NAME: Create DOS disk volumes                   */
/*                                                             */
/* FUNCTION: Create the boot record/partition table structure  */
/*           needed to support the DOS disk volume arch in     */
/*           the EXTENDED partition. Volume is created to the  */
/*           the default size (largest contiguous freespace or */
/*           32mb, whichever smaller) or to the user specified */
/*           size (must be smaller or equal to default size).  */
/*           The volume boot record is created, and the appro- */
/*           priate pointers in other volume partition tables  */
/*           are generated.                                    */
/*                                                             */
/*                                                             */
/* NOTES: Screen can be exited via the ESC command before      */
/*        partition is created and nothing will change         */
/*                                                             */
/*        The following screen is managed                      */
/*                                                             */
/*     �0000000000111111111122222222223333333333�              */
/*     �0123456789012345678901234567890123456789�              */
/*   ������������������������������������������Ĵ              */
/*   00�Create DOS Disk Volume                  �              */
/*   01�                                        �              */
/*   02�Vol Start End  Size                     �              */
/*   03� #  ####  #### ####                     �              */
/*   04�                                        �              */
/*   05�                                        �              */
/*   06�                                        �              */
/*   07�                                        �              */
/*   08�                                        �              */
/*   09�                                        �              */
/*   10�                                        �              */
/*   11�                                        �              */
/*   12�                                        �              */
/*   13�                                        �              */
/*   14�                                        �              */
/*   15�                                        �              */
/*   16�Total partition size is #### cylinders. �              */
/*   17�Maximum space available for disk        �              */
/*   18�volume is #### cylinders.               �              */
/*   19�                                        �              */
/*   20�Enter disk volume size..........: [####]�              */
/*   21�                                        �              */
/*   22�                                        �              */
/*   23�Press ESC to return to FDISK Options    �              */
/*   ��������������������������������������������              */
/*                                                             */
/* ENTRY POINTS: Volume_Create                                 */
/*      LINKAGE: Volume_Create ()                              */
/*           NEAR CALL                                         */
/*                                                             */
/* INPUT: None                                                 */
/*                                                             */
/* EXIT-NORMAL: ERROR=FALSE                                    */
/*                                                             */
/* EXIT-ERROR: ERROR=TRUE                                      */
/*             GOTO internal_program_error if case statement   */
/*             failure when branching to requested function    */
/*                                                             */
/* EFFECTS: No data directly modified by this routine, but     */
/*          child routines will modify data.                   */
/*                                                             */
/* INTERNAL REFERENCES:                                        */
/*   ROUTINES:                                                 */
/*      clear_screen                                           */
/*      display                                                */
/*      volume_display                                         */
/*      get_num_input                                          */
/*      wait_for_ESC                                           */
/*      make_partition                                         */
/*                                                             */
/* EXTERNAL REFERENCES:                                        */
/*   ROUTINES:                                                 */
/*                                                             */
/******************** END OF SPECIFICATIONS ********************/

/*  */
void volume_create()

BEGIN

    unsigned  input;
    unsigned  default_entry;
/*C00    char  temp;              */
    char  drive_letter;
/*C00    char  default_value;     */
    char  location;
/*C00    char  previous_location; */
    char  ext_location;
    unsigned char  i;
    char  defined_drives;
    char  temp_cur_disk;
    unsigned ext_part_percent_unused;                                   /* AN000 */
    unsigned ext_part_num;                                              /* AN000 */

    input = u(NUL);                                                     /* AC000 */

    /* clear off screen */
    clear_screen(u(0),u(0),u(24),u(79));                                /* AC000 */

    /* Display header */
    display (menu_18);

    /* print ESC prompt */
    display(menu_11);

    /* Display volume info */
    drive_letter = volume_display();

    /* Loop until done */
    input = u(NUL);                                                     /* AC000 */
    while (input != ((unsigned)(ESC_FLAG)))                                  /* AC000 */

        BEGIN
        /* See if we have hit the max number of drives */
        defined_drives = c(0);                                          /* AC000 */
        temp_cur_disk = cur_disk;

        /* Search both drives for defined drives */
        for (i = uc(0); i < number_of_drives; i++)                      /* AC000 */

            BEGIN
            cur_disk = ((char)(i));

            /* See if there is a primary drive letter */
            if ((find_partition_type(uc(DOS12))) || (find_partition_type(uc(DOS16))) || (find_partition_type(uc(DOSNEW)))) /*AC000*/
                defined_drives++;

            /* See if extended partition on disk */
            if (find_partition_type(uc(EXTENDED)))                      /* AC000 */
                BEGIN
                /* Get number of logical drives */
                defined_drives = defined_drives + get_num_logical_dos_drives();
                END
            END
        /* Restore cur_disk  to original */
        cur_disk = temp_cur_disk;

        /* See if 26 or less drives total */
/*C07   if (defined_drives < c(24))                      */             /* AC000 */
        if ((defined_drives < c(24)) &&             /*C07*/
            (get_num_logical_dos_drives() < c(23))) /*C07*/
            BEGIN
            location = find_ext_free_space();

            /* find the number of the extended partiton to figure out percent */
            ext_part_num = find_partition_location(uc(EXTENDED));                   /* AN000 */

            /* Set the percent used */
            ext_part_percent_unused =
                cylinders_to_percent(free_space[location].space,
                ((part_table[cur_disk][ext_part_num].end_cyl-part_table[cur_disk][ext_part_num].start_cyl)+1));                         /* AN00

            /* Is there any ?*/

            /* SR; 9/26/89; Instead of checking only the percentage which
               could be off because of rounding errors, we also check the
               mbytes available */

/*C02       if (free_space[temp].space != u(0) &&
                (free_space[temp].percent_unused != u(0) ||
                 free_space[temp].mbytes_unused != u(0)))             */
            if (free_space[location].space != u(0) &&
                (free_space[location].percent_unused != u(0) ||
                 free_space[location].mbytes_unused != u(0)))    /*C02*/
/*SR; 9/26/89;  if (ext_part_percent_unused != u(0))             /* AC000 */
                BEGIN

                /* Display disk space */
                sprintf(insert,"%4.0d",get_partition_size(uc(EXTENDED)) );
                display(menu_21);

                /* Setup and print max partition size */

                sprintf(insert,"%4.0d%3.0d%%",
                        free_space[location].mbytes_unused,
                        ext_part_percent_unused);
                display(menu_22);

                /* Force repeats on the input until something valid (Non-Zero return) */
                /* If MBytes unused  is equel to zero, display percent unused */
                if (free_space[location].mbytes_unused == u(0))         /* AN000 */
                    BEGIN                                               /* AN000 */
                    default_entry = (unsigned)ext_part_percent_unused;     /* AN000 */
                    PercentFlag = (FLAG)TRUE;                           /* AN000 */
                    END                                                 /* AN000 */
                else                                                    /* AN000 */
                    BEGIN                                               /* AN000 */
                    default_entry = (unsigned)free_space[location].mbytes_unused;     /* AC000 */
                    PercentFlag = (FLAG)FALSE;                          /* AN000 */
                    END                                                 /* AN000 */

                valid_input = (FLAG)FALSE;                              /* AC000 */

                while (!valid_input)
                    BEGIN
                    /* Display prompt */
                    if (!PercentFlag)                                   /* AN000 */
                        sprintf(insert,"%4.0d",default_entry);
                    else                                                /* AN000 */
                        sprintf(insert,"%3.0d%%",default_entry);        /* AN000 */

                    display(menu_40);

                    input = get_large_num_input(default_entry,free_space[location].mbytes_unused,ext_part_percent_unused,menu_40,u(0),error_12); /* AC000*/

                    /* Update default in case of error, so it gets displayed and used */
                    /* if user presses CR only */

                    default_entry = input;
                    clear_screen(u(19),u(0),u(23),u(79));               /* AC000 */
                    END

                if (input != ((unsigned)(ESC_FLAG)))                    /* AC000 */
                    BEGIN

                    /* Change input to cylinders */
                    if (PercentFlag)                                          /* AN000 */
                        BEGIN                                                 /* AN000 */
                        if (input == ext_part_percent_unused)
                            input = free_space[location].space;                   /* AN000 */
                        else                                                  /* AN000 */
                            input = percent_to_cylinders(input,((part_table[cur_disk][ext_part_num].end_cyl-part_table[cur_disk][ext_part_num].start_cyl)+1));
                        END                                                   /* AN000 */
                    else                                                      /* AN000 */
                        BEGIN                                                 /* AN000 */
                        if (input == free_space[location].mbytes_unused)
                            input = free_space[location].space;                   /* AN000 */
                        else                                                  /* AN000 */
                            input = (unsigned)mbytes_to_cylinders(input,
                                                                  cur_disk);  /* AN004 */
                        END                                                   /* AN000 */

                    /* Initialize PecentFlag back to FALSE */
                    PercentFlag = (FLAG)FALSE;                                  /* AN000 */

                    /* SR; 9/26/89; Display error message if input value = 0 */

                    if (input == 0)
                       {
                        display(error_28);
                        wait_for_ESC();
                       }
                    else
                       {
                    	/* go create the entry and find out where it put it */
                    	ext_location = make_volume(input,location);

                    	/* clear off the old prompt */
                    	clear_screen(u(15),u(0),u(19),u(79));               /* AC000 */

                    	reboot_flag = (FLAG)TRUE;                           /* AC000 */

                    	/* Display the updated partition information */
                    	drive_letter = volume_display();

                    	/* Tell user we created it */
                    	display(status_7);
                       }
                    END
                END
            else
                BEGIN
                /* No space left or already max'd on the devices */
                /* Get rid of the size prompts */
                clear_screen(u(17),u(0),u(21),u(79));                   /* AC000 */
                display(error_20);
                volume_display();
                wait_for_ESC();                         /* KWC, 11-01-87 */
                input = u(ESC_FLAG);                    /* KWC, 11-01-87 */
                END
            END
        else
            BEGIN
            /* Reached the maximum */
            /* Get rid of the size prompts */
            clear_screen(u(17),u(0),u(21),u(79));                       /* AC000 */
            display(error_27);
            /* Force an exit with ESC */
            wait_for_ESC();                         /* KWC, 11-01-87 */
            input = u(ESC_FLAG);                    /* KWC, 11-01-87 */
            END
        END
    clear_screen(u(0),u(0),u(24),u(79));                               /* AC000 */
    return;
END
