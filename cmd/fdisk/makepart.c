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
*  05/30/90  EGH  C22  Commented out the routine clear_directory and all
*                      references to it.  It wasn't working and it may have
*                      been causing problems.
*  07/03/90  EGH  C23  Fixed assignment of system ID on lolgical volumes.
*                      Bug #1474
*  09/13/90  EGH  C21  We need to set the volume label and system for primary
*                      partitions now that we are using this information.
* 
******************************************************************************/

#include "dos.h"                                                        /* AN000 */
#include "fdisk.h"                                                      /* AN000 */
#include "extern.h"                                                     /* AN000 */
#include "subtype.h"                                                    /* AN000 */
#include "string.h"                                                     /* AN000 */

#if 0                                                                   /*C22*/

/***************************************************************************/
/****  SR; 9/30/89; Calculates the first sector of the directory for    ****/
/****  the newly created partition and writes zeros to the first        ****/
/****  directory sector to eliminate garbage in the volume label.       ****/
/****                                                                   ****/
/***************************************************************************/

void clear_directory (unsigned long total_sectors, unsigned start_cylinder)
{

    char           retry;
    unsigned long  j;
    unsigned long  sectors_per_fat;
    char far       *buffer_pointer = boot_record;

    for (j = 0; j < BYTES_PER_SECTOR; j++)
        boot_record[j] = 0;                     /* Zero sector buffer  */

    retry = 0;
    do
    {
        retry++;
        if (total_sectors > ul(FAT16_SIZE))
        {                                       /* 16 bit FAT */
            j = 2 + (BYTES_PER_SECTOR * 2);
            sectors_per_fat = ((total_sectors - 33) + j - 1) / j;
        }
        else
        {                                       /* 12 bit FAT */
            j = 2 + ((BYTES_PER_SECTOR * 16) + 2) / 3;
            sectors_per_fat = ((total_sectors - 33) + j - 1) / j;
        }

        regs.x.bx = FP_OFF(buffer_pointer);
        segregs.es = FP_SEG(buffer_pointer);
        regs.h.dl = uc(cur_disk) + uc(0x80);
        j = 1 + (2 * sectors_per_fat);          /* sectors before directory */
        regs.h.cl = uc(1 + (j % max_sector[cur_disk]));
        regs.x.ax = u((j / max_sector[cur_disk]) + 1);
        regs.h.dh = uc(regs.x.ax % max_head[cur_disk]);
        regs.x.ax = u(regs.x.ax / max_head[cur_disk]);
        regs.x.ax = regs.x.ax + start_cylinder;
        regs.h.ch = regs.h.al;
        regs.h.cl = regs.h.cl | (regs.h.ah << 6);
        regs.h.ah = uc(WRITE_DISK);
        regs.h.al = 1;                          /* write one sector */
        DiskIo(&regs,&regs,&segregs);
    }
    while (regs.x.cflag && (retry < 5));
}

#endif                                                                  /*C22*/

/*  */

void make_partition(size,free_pointer,bootable,type)

unsigned        size;
char            free_pointer;
unsigned char   bootable;
char            type;

BEGIN

    char table_pointer;
    unsigned i;
    unsigned char   temp;
    unsigned long   total_sectors;

    /* Find a free spot to put it in */
    table_pointer = find_free_partition();

    if (table_pointer != ((char)(NOT_FOUND)))
        BEGIN
        /* found a free partition, now lets go fill it up */

        /* Do we need to make it active? */
        if (bootable == ((unsigned char)(ACTIVE)))
            BEGIN

            /* Go clear out a previously active one */
            for (i=u(0); i <u(4); i++)                                  /* AC000 */
                BEGIN
                if (part_table[cur_disk][i].boot_ind == uc(0x80))       /* AC000 */
                    BEGIN
                    part_table[cur_disk][i].changed = TRUE;
                    part_table[cur_disk][i].boot_ind = uc(0);           /* AC000 */
                    END
                END

                /* Now mark the new one active */
                part_table[cur_disk][table_pointer].boot_ind = uc(0x80); /* AC000 */
            END
        else
            BEGIN
            /* Mark it as not active, leaving the others alone */
            part_table[cur_disk][table_pointer].boot_ind = uc(0);       /* AC000 */
            END

            /* Go get the start cylinder */
            part_table[cur_disk][table_pointer].start_cyl = free_space[free_pointer].start;

            /* Setup end cylinder */
            part_table[cur_disk][table_pointer].end_cyl = part_table[cur_disk][table_pointer].start_cyl + size - 1;

            /* Start sector is always 1 */
            part_table[cur_disk][table_pointer].start_sector = uc(1);   /* AC000 */

            /* End sector is always the last sector */
            part_table[cur_disk][table_pointer].end_sector = max_sector[cur_disk];

            /* End head is always the last head */
            part_table[cur_disk][table_pointer].end_head = uc(max_head[cur_disk] -1);      /* AC004 */

            /* Start head is always 0 unless this is track 0 - then it is 1 */
            temp = uc(0);                                               /* AC000 */
            if (part_table[cur_disk][table_pointer].start_cyl == u(0))  /* AC000 */
                BEGIN
                temp = uc(1);                                           /* AC000 */
                END
            part_table[cur_disk][table_pointer].start_head = temp;

            /* Figure out the total number of sectors */
            /* Total sectors in partition =                    */
            /* [(end_cyl - start_cyl)*(max_sector)*(max_head)] */
            /* - [start_head * max_sector]                     */
            /* Note: This is assuming a track or cylinder aligned partition */

            /* First - get the total size in Cylinders assuming head 0 start*/
            total_sectors = ((unsigned long)(part_table[cur_disk][table_pointer].end_cyl -
                              part_table[cur_disk][table_pointer].start_cyl+1));

            /* Now multiply it by the number of sectors and heads per track */
            total_sectors = total_sectors * max_sector[cur_disk] * max_head[cur_disk];

            /* This will give us the total of sectors if it is cyl aligned */
            /* Now, if it isn't aligned on head 0, we need to subtract off */
            /* the skipped tracks in the first cylinder  */

            /* Because the head is zero based, we can get the total number of */
            /* skipped sectors by multipling the head number by sectors per track */
            total_sectors = total_sectors - ((unsigned long)part_table[cur_disk][table_pointer].start_head) *
                              max_sector[cur_disk];
            part_table[cur_disk][table_pointer].num_sec = total_sectors;

            /* Get the relative sector */
            /* Figure out the total number of sectors */
            /* Total sectors before partition =                */
            /* (start_cyl)*(max_sector)*(max_head)]            */
            /* + [start_head * max_sector]                     */
            /* Note: This is assuming a track or cylinder aligned partition */

            /* Start cyl will work because it is zero based */
            total_sectors = ((unsigned long)part_table[cur_disk][table_pointer].start_cyl);

            /* Get sectors up to head 0 of the partition */
            total_sectors = total_sectors * max_sector[cur_disk] * max_head[cur_disk];

            /* Because the head is zero based, we can get the total number of */
            /* skipped sectors by multipling the head number by sectors per track */
            total_sectors = total_sectors + ((unsigned long)part_table[cur_disk][table_pointer].start_head) *
                              max_sector[cur_disk];

            /* Save it! */
            part_table[cur_disk][table_pointer].rel_sec = total_sectors;

            /* Setup the system id byte */
            if (type == ((char)(EXTENDED)))
                BEGIN
                temp = uc(EXTENDED);                                    /* AC000 */
                END
            else
                BEGIN
                if (type == ((char)(PRIMARY)))
                    BEGIN
                    /* Always set to 06h - let format worry about setting to correct value */
                    /* SR; 9/30/89; We fix up the size ourselves here. The
                       calculation is simple. If total_sectors > 65536 then
                       type = 06 else if total_sectors > 32680 then type = 04
                       else type = 01 */

                    /* SR; 9/30/89; Calculate total_sectors from start
                       including the hidden sectors */

                    total_sectors += part_table[cur_disk][table_pointer].num_sec;

                    if ( (total_sectors >> 16) > 0 )
                        temp = uc(DOSNEW); /* Partition extends beyond the 32M boundary */
                    else if (part_table[cur_disk][table_pointer].num_sec > ul( FAT16_SIZE ))
                        temp = uc(DOS16);
                    else
                        temp = uc(DOS12);                                  /* AC000 */                            /*  AN000  */
                    END
                else
                    BEGIN
                    internal_program_error();
                    END
                END

            /* We got the sys id, now put it in */
            part_table[cur_disk][table_pointer].sys_id = temp;

            /* Set the changed flag */
            part_table[cur_disk][table_pointer].changed = TRUE;

            /* Set the mbytes used */
            part_table[cur_disk][table_pointer].mbytes_used =
                cylinders_to_mbytes(size,cur_disk);                     /* AN004 */

            /* Set the percent used */
            part_table[cur_disk][table_pointer].percent_used =
                cylinders_to_percent(((part_table[cur_disk][table_pointer].end_cyl-part_table[cur_disk][table_pointer].start_cyl)+1),
                total_disk[cur_disk]);                                  /* AN000 */

            /* set the system to unknown and volume label to blanks */  /*C21*/
            strcpy(part_table[cur_disk][table_pointer].system,NOFORMAT); /*C21*/
            strcpy(part_table[cur_disk][table_pointer].vol_label,NOVOLUME); /*C21*/

            /* SR; 9/30/89; Fill the first directory sector with zeros to
               eliminate garbage in the volume label */

/*C22       clear_directory (part_table[cur_disk][table_pointer].num_sec,    */
/*C22                        part_table[cur_disk][table_pointer].start_cyl); */

        END
    else

        BEGIN
        /* This should not have happened */
        internal_program_error();
        return;
        END

    return;
END


/*  */
char make_volume(size,free_pointer)

unsigned    size;
char   free_pointer;

BEGIN

    char table_pointer;
/*C00    unsigned i;        */
    unsigned ext_part_num;                                                  /* AN000 */
    unsigned char   temp;
    unsigned long   total_sectors;

    /* Find a free spot to put it in */
    table_pointer = find_free_ext();

    if (table_pointer != ((char)(NOT_FOUND)))
       BEGIN
        /* found a free partition, now lets go fill it up */


        /* This can never be marked active */
        ext_table[cur_disk][table_pointer].boot_ind = uc(0);        /* AC000 */


        /* Go get the start cylinder */
        ext_table[cur_disk][table_pointer].start_cyl = free_space[free_pointer].start;

        /* Setup end cylinder */
        ext_table[cur_disk][table_pointer].end_cyl = ext_table[cur_disk][table_pointer].start_cyl + size - 1;

        /* Start sector is always 1 */
        ext_table[cur_disk][table_pointer].start_sector = uc(1);    /* AC000 */

        /* End sector is always the last sector */
        ext_table[cur_disk][table_pointer].end_sector = max_sector[cur_disk];

        /* End head is always the last head */
        ext_table[cur_disk][table_pointer].end_head = uc(max_head[cur_disk]-1);  /* AC004 */

        /* Start head is always 1 - NOTE: This is a shortcut for PC-DOS */
        /* If this is being modified for IFS drivers this may not be the */
        /* the case - use caution */
        ext_table[cur_disk][table_pointer].start_head = uc(1);    /* AC000 */

        /* Figure out the total number of sectors */
        /* Total sectors in partition =                    */
        /* [(end_cyl - start_cyl)*(max_sector)*(max_head)] */
        /* - [start_head * max_sector]                     */
        /* Note: This is assuming a track or cylinder aligned partition */

        /* First - get the total size in Cylinders assuming head 0 start*/
        total_sectors = ((unsigned long)(ext_table[cur_disk][table_pointer].end_cyl -
           ext_table[cur_disk][table_pointer].start_cyl+1));

        /* Now multiply it by the number of sectors and heads per track */
        total_sectors = total_sectors * max_sector[cur_disk] * max_head[cur_disk];

        /* This will give us the total of sectors if it is cyl aligned */
        /* Now, if it isn't aligned on head 0, we need to subtract off */
        /* the skipped tracks in the first cylinder  */

        /* Because the head is zero based, we can get the total number of */
        /* skipped sectors by multipling the head number by sectors per track */
        total_sectors = total_sectors - ((unsigned long)(ext_table[cur_disk][table_pointer].start_head *
                           max_sector[cur_disk]));

        ext_table[cur_disk][table_pointer].num_sec = total_sectors;

        /* Get the relative sector */
        /* Figure out the total number of sectors */
        /* Total sectors before partition = max_sector     */
        /* NOTE: Again, this is a PC-DOS 3.30 shortcut - by definition */
        /* a logical drive always starts on head 1, so there is always */
        /* one tracks worth of sectors before it. Hence, max_sector */

        /* Save it! */
        ext_table[cur_disk][table_pointer].rel_sec = ((unsigned long)(max_sector[cur_disk]));

        /* Setup the system id byte */
        /* Set to 06h - format will fix later on */
        /* EGH; 7/03/90; We fix up the size ourselves here. The
           calculation is simple. If total_sectors > 65536 then
           type = 06 else if total_sectors > 32680 then type = 04
           else type = 01 */

        if (ext_table[cur_disk][table_pointer].num_sec > ul(65536))     /*C23*/
            temp = uc(DOSNEW);                                         /* AC000 */                                    /*  AN000  */
        else if (ext_table[cur_disk][table_pointer].num_sec > ul(FAT16_SIZE)) /*C23*/
            temp = uc(DOS16);                                           /*C23*/
        else                                                            /*C23*/
            temp = uc(DOS12);                                           /*C23*/

        /* We got the sys id, now put it in */
        ext_table[cur_disk][table_pointer].sys_id = temp;

        /* Set the changed flag */
        ext_table[cur_disk][table_pointer].changed = TRUE;

        /* Set the mbytes used */
        ext_table[cur_disk][table_pointer].mbytes_used =
            cylinders_to_mbytes(size,cur_disk);                   /* AN004 */

        /* find the number of the extended partiton to figure out percent */
        ext_part_num = find_partition_location(uc(EXTENDED));              /* AN000 */

        /* Set the percent used */
        ext_table[cur_disk][table_pointer].percent_used =
            cylinders_to_percent(((ext_table[cur_disk][table_pointer].end_cyl-ext_table[cur_disk][table_pointer].start_cyl)+1),
            ((part_table[cur_disk][ext_part_num].end_cyl-part_table[cur_disk][ext_part_num].start_cyl)+1));      /* AN000 */

        /* set the system to unknown and volume label to blanks */
        strcpy(ext_table[cur_disk][table_pointer].system,NOFORMAT);     /* AN000 */
        strcpy(ext_table[cur_disk][table_pointer].vol_label,NOVOLUME);  /* AN000 */

        /* SR; 9/30/89; Fill the first directory sector with zeros to
           eliminate garbage in the volume label */

/*C22   clear_directory (part_table[cur_disk][table_pointer].num_sec,    */
/*C22                    part_table[cur_disk][table_pointer].start_cyl); */

       END
    else

       BEGIN
        /* This should not have happened */
        internal_program_error();
       END

    return(table_pointer);
END

