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
******************************************************************************/
#include "dos.h"                                                        /* AN000 */
#include "fdisk.h"                                                      /* AN000 */
#include "extern.h"                                                     /* AN000 */
#include "subtype.h"                                                    /* AN000 */
#include "fdiskmsg.h"                                                   /* AN000 */

#include "string.h"
#include "stdio.h"
#include "stdlib.h"

/* int     printf(char *, ...); */

/*  */
char get_disk_info()

BEGIN

    unsigned char   i;

    /* Initialize values */
    number_of_drives = uc(0);                                       /* AC000 */
    for (i=uc(0); i < uc(MAX_HDISK); i++)                           /* AC000 */
       BEGIN
        total_disk[i] = u(0);                                       /* AC000 */
        total_mbytes[i] = f(0);                                     /* AC000 */
        max_sector[i] = uc(0);                                      /* AC000 */
        max_head[i] = u(0);                                         /* AC004 */
       END

    /* See how many drives there are */
    if (get_drive_parameters(uc(0x80)))                             /* AC000 */

       BEGIN
        /* Get the drive parameters for all drives */
        for (i = uc(0); i < number_of_drives;i++)                   /* AC000 */

           BEGIN
            if (get_drive_parameters(uc(0x80)+i))                   /* AC000 */

               BEGIN
                /* Save drive parameters */
                max_sector[i] = ((unsigned char)(regs.h.cl & 0x3F));
                max_head[i] = ((unsigned)(regs.h.dh +1));          /* AC004 */
                total_disk[i] = ((((unsigned)(regs.h.cl & 0xc0)) << 2) | regs.h.ch) + 1;
                total_mbytes[i] = cylinders_to_mbytes(total_disk[i],i);  /* AN004 */
               END
            else
               BEGIN
                good_disk[i] = FALSE;
                return(FALSE);
               END
           END
        return(TRUE);
       END
    else
        /* No drives present */
        BEGIN
         no_fatal_error = FALSE;
         return(FALSE);
        END
END



/*  */
char write_boot_record(cylinder,which_disk)

unsigned    cylinder;
unsigned char   which_disk;

BEGIN

/*C00    char i;  */
/*C00    char j;  */
    char far *buffer_pointer = boot_record;

    /* Setup read, always on a cylinder boundary */
    regs.h.ah = uc(WRITE_DISK);                                     /* AC000 */
    regs.h.al = uc(1);                                              /* AC000 */
    regs.h.dh = uc(0);                                              /* AC000 */
    regs.h.cl = uc(0x01);                                           /* AC000 */

    /* Specify the disk */
    regs.h.dl = which_disk + 0x80;

    /* Need to scramble CX so that sectors and cyl's are in INT 13 format */

    if (cylinder > u(255))                                          /* AC000 */
       BEGIN
        regs.h.cl = regs.h.cl | ((unsigned char)((cylinder /256) << 6));
       END
    regs.h.ch = (unsigned char)(cylinder & 0xFF);

    /* Point at the place to read the boot record */
    regs.x.bx = FP_OFF(buffer_pointer);
    segregs.es = FP_SEG(buffer_pointer);

    /* write the boot record */
    DiskIo(&regs,&regs,&segregs);                         /* AC000 */

    /* Check for error reading it */
    if ((regs.x.cflag & 1) != u(1))                                 /* AC000 */
       BEGIN
        return(TRUE);
       END
    else
       BEGIN
        /* Tell user there was an error */
        clear_screen(u(0),u(0),u(24),u(79));                        /* AC000 */
        display(error_2);
        no_fatal_error = FALSE;
        return(FALSE);
       END
END

/*  */
unsigned verify_tracks(pointer,type)

char pointer;
char type;

BEGIN
    unsigned        i;

    /* SR; 9/29/89; Temp store */
    unsigned                 j;
/*C00    unsigned        location;     */
    unsigned        sectors_per_fat;
    unsigned        cur_cyl;
    unsigned        verify_cyl;
    unsigned        num_tracks;
    unsigned                        retry_flg;
    unsigned long   total_sectors;
    char   golden_tracks;
    char   retry;
    unsigned        char   cur_head;                                        /* AC004 */

    char far *buffer_pointer = boot_record;
/*C00    char head;                    */



    for (i=u(0); i< u(BYTES_PER_SECTOR);i++)                        /* AC000 */
       BEGIN
        /* Put something other then 0's so that unformatted FAT looks full */

        boot_record[i] = uc(0xF6);                                  /* AC000 */
       END

    /* Get the start cylinder for the sweep */
    cur_cyl = free_space[pointer].start;

    /* Also keep track of what it is */
    verify_cyl = cur_cyl;

    /* Initialize the start head -assume 0*/
    cur_head = uc(0);                                               /* AC004 */

    /* SR; 9/29/89; cur_head should be 1 for extended partitions too */
    if (((type == c(PRIMARY)) && (cur_cyl == u(0))) || type == c(EXTENDED))                  /* AC000 */
      BEGIN

       /* It's head 1 - NOTE: This is convience for PC-DOS because it is */
       /* always this way - This may have to be beefed up for IFS  */
       cur_head = uc(1);                                            /* AC004 */
      END

    /* Now go figure out the number of golden sectors needed. Use the */
    /* allocation equation in the fixed disk section of DOS Tech Ref. */
    /*                                                                */
    /*  TS = Free cyl's * sector/track * track/cyl                    */
    /*  RS = 1                                                        */
    /*  D  = 512                                                      */
    /*  BPD = 32                                                      */
    /*  BPS = BYTES_PER_SECTOR                                        */
    /*  CF = 2                                                        */
    /*  SPF = Solve                                                   */
    /*  SPC = 4 or 8                                                  */
    /*  BPC = 1.5 or 2                                                */
    /*                                                                */
    /*  Golden Sectors = RS + 2(SPF) + BPD(D)  + (DOSFILES)           */
    /*                                 ÄÄÄÄÄÄ   if bootable           */
    /*                                  BPS                           */

    total_sectors = ((unsigned long)free_space[pointer].space) * max_head[cur_disk] * max_sector[cur_disk];

    /* Chop off one track if it starts on head 1 */
    if (cur_head == uc(1))                                          /* AC004 */
       BEGIN
        total_sectors = total_sectors - max_sector[cur_disk];
       END

    /* See if 12 or 16 bit fat */
    if (total_sectors > (unsigned long)FAT16_SIZE)
       BEGIN
        /* SR; 9/30/89; We have to round up the sectors_per_fat.
           Calculate the denominator first */

        j = 2 + (BYTES_PER_SECTOR * 2);
        /* 16 bit */
        /* SR; 9/30/89; To round up add j-1 and divide by j */
/*C00   sectors_per_fat = ((total_sectors - 33) + j - 1) / j;     */
        sectors_per_fat = u(((total_sectors - 33) + j - 1) / j);        /*C00*/
       END
    else
       BEGIN
        /* SR; 9/30/89; We have to round up the sectors_per_fat.
        Calculate the denominator first. We have to round up the
        denominator too. The multiplying factor 16 = SPC * 2 */

        j = 2 + (( BYTES_PER_SECTOR * 16 ) + 2 ) / 3;
        /* 12 bit */
/*C00   sectors_per_fat = ((total_sectors - 33) + j - 1) / j;    */
        sectors_per_fat = u(((total_sectors - 33) + j - 1) / j);        /*C00*/
       END

    /* Round up one just to handle any rounding errors */
    /* SR; 9/30/89; We have taken care of the rounding */
    /***SR; 9/30/89;         sectors_per_fat++;     ***/

    /* Now see how many tracks */
    num_tracks = (sectors_per_fat*2) + 33;

    /* If primary and drive 0, add in enough for system files */
    if ((type == c(PRIMARY)) && (cur_disk == c(0)))                 /* AC000 */
       BEGIN
        num_tracks = num_tracks + SYSTEM_FILE_SECTORS;
       END

     /* Handle upward rounding */                                                                            /* The problem with the IBM code is      */
     if (num_tracks%max_sector[cur_disk] != u(0))                                    /* that if num_tracks is < max_sector[cur_disk] */
         BEGIN                                                                                                /* the num_tracks becomes 0 due to the integer  */
         num_tracks = num_tracks + max_sector[cur_disk];    /* division and num_tracks will not be inc'd .  */
         END                                                                                                  /* This section of code overcomes that.     */

     /* Now convert to tracks */
     num_tracks = num_tracks/max_sector[cur_disk];

     /* SR; 9/30/89; The number of tracks above does not take into
        account the wasted track 0 which contains the partition info.
        If cur_head = 0, then the above calculation would cause the
        last track in the partition to be not verified */

     if ( cur_head == 0 )
             num_tracks++;

     golden_tracks = FALSE;

#if 0	/************* Commented out ********************/

     while (!golden_tracks)
        BEGIN

         for (i = u(0);i < num_tracks; i++)                         /* AC000 */
            BEGIN
             retry = c(0);                                          /* AC000 */
             do
                BEGIN
                 retry++;
                 /* Specify the operation */
                 regs.h.ah = uc(WRITE_DISK);                        /* AC000 */

                 /* Specify number of sectors */
                 regs.h.al = ((unsigned char)max_sector[cur_disk]);

                 /* Specify the start sectors */
                 regs.h.cl = uc(1);                                 /* AC000 */

                 /* Need to scramble CX so that sectors and cyl's are in INT 13 format */
                 if (cur_cyl > u(255))                              /* AC000 */
                    BEGIN
                     regs.h.cl = regs.h.cl | ((unsigned char)((cur_cyl/256) << 6));
                    END
                 regs.h.ch = ((unsigned char)cur_cyl) & 0xFF;

                 /* Specify the disk */
                 regs.h.dl = ((unsigned char)cur_disk) + 0x80;

                 /* Specify the head */
                 regs.h.dh = cur_head;                              /* AC004 */

                 /* Point at the place to write */
                 regs.x.bx = FP_OFF(buffer_pointer);
                 segregs.es = FP_SEG(buffer_pointer);

                 /* write the track */
                 DiskIo(&regs,&regs,&segregs);            /* AC000 */

                END
             while (((regs.x.cflag & 1) == u(1)) && (retry != c(5))); /* AC000 */

#endif   /***************Commented out *****************************/

     while (!golden_tracks)
         {                               /* this allows resetting the starting cyl of the golden tracks */
         for (i = u(0);i < num_tracks; i++)
             {               /* this for loop is to write the number of golden tracks  AC000 */
             retry = c(0);                           /* AC000 */
             do
                 {                                       /* this do loop is for retries */
                 retry++;
                 regs.h.ah = uc(VERIFY_DISK);                          /* BC001 */ /* Specify the operation */
                 regs.h.al = (unsigned char)max_sector[cur_disk];              /* Specify number of sectors */
                 regs.h.cl = 1;  /* Specify the start sectors */ /* AC000 BC001 Scramble CX so that sectors and cyl's are in INT 13 format*/
                 if (cur_cyl > u(255))                                 /* AC000 */
                     regs.h.cl = regs.h.cl | ((unsigned char)((cur_cyl/256) << 6));
                 regs.h.ch = ((unsigned char)cur_cyl) & 0xFF;
                 regs.h.dl = ((unsigned char)cur_disk) + 0x80;         /* Specify the disk */
                 regs.h.dh = cur_head;                                 /* Specify the head */   /* AC004 */
                 int86((int)DISK, &regs, &regs);                       /* BC001 */ /* verify the track */
                     retry_flg = regs.x.cflag & 1;                         /* BN001 */
                 }
             while ((retry_flg) && (retry != c(3)));               /* BN001 */ /* retry loop */

             /************************************************************/
             /* Check to see if cylinder was verified w/o errors.        */
             /************************************************************/

             if (!retry_flg)
                 {                                               /* BN001 */
             /*********************************************************/
             /* Cylinder verified correctly. So write boot sector     */
             /* and bump cylinder count, then go verify that cylinder.*/
             /* Do not pass the end of the partition free space.      */
             /*********************************************************/
                 retry = 0;
                 do
                     {
                     retry++;
                     regs.h.ah = uc(WRITE_DISK);                             /* BN001 */ /* Specify the operation */
                     regs.h.al = 1;                                          /* BN001 */ /* Write one sector only */
                     regs.h.cl = 1;                                          /* BN001 */ /* Specify the start sectors */ /* AC000 BC001 Need to scramble CX for INT 13 format */
                     if (cur_cyl > u(255))                                   /* BN001 */
                             regs.h.cl = regs.h.cl | ((unsigned char)((cur_cyl/256) << 6)); /* BN001 */
                     regs.h.ch = ((unsigned char)cur_cyl) & 0xFF;            /* BN001 */
                     regs.h.dl = ((unsigned char)cur_disk) + 0x80;           /* BN001 */ /* Specify the disk */
                     regs.h.dh = cur_head;                                   /* BN001 */ /* Specify the head */   /* AC004 */
                     regs.x.bx = FP_OFF(buffer_pointer);                     /* BN001 */ /* Point at the place to write */
                     segregs.es = FP_SEG(buffer_pointer);                    /* BN001 */
                     DiskIo(&regs,&regs,&segregs);                           /* BN001 */ /* write the track */
                     retry_flg = regs.x.cflag & 1;                         /* BN001 */
                     }
                 while ((retry_flg) && (retry != c(3)));                 /* BN001 */ /* retry loop */
                 }

             /* See if we had a good read */
             if (!retry_flg)                        /* AC000 */
                 BEGIN
                 golden_tracks = TRUE;
                 /* Get the next head */
                 cur_head++;

                 /* SR; 9/29/89; Head no. is pre-incremented and cannot
                    subtract 1 from max_head[cur_disk] */
                 if (cur_head == (uc(max_head[cur_disk] )))       /* AC004 */
                     BEGIN
                     /* Up to the next cylinder */
                     cur_head = uc(0);                              /* AC004 */
                     cur_cyl++;

                     /* Check to see if we've reached the end of the free_space*/
                     if (cur_cyl > free_space[pointer].end)
                         BEGIN
                         /* It is, so return with the cyl offset equal to the freespace */
                         return(free_space[pointer].space);
                         END
                     END
                 END
             else
                 BEGIN
                 /* Get out of the for loop, with a false flag */
                 golden_tracks = FALSE;

                 /* Bump up to the next cylinder boundary */
                 cur_cyl++;

                 /* SR; 9/29/89; To get out of the loop we have to
                    check if there have been enough golden tracks or
                    not. The setting of the flag as above will not
                    get us out of the loop. We return with the cyl
                    offset equal to the freespace to indicate that
                    this freespace cannot be used */

                 if ( cur_cyl > free_space[pointer].end )
                     return( free_space[pointer].space );

                 cur_head = uc(0);                                  /* AC004 */

                 /* Save the new verify start point */
                 verify_cyl = cur_cyl;

                 break;
                 END
             END  /* for num_tracks */
         } /*while !golden_tracks */

    /* All done, return the offset from original cyl to the new one */
    return(verify_cyl - free_space[pointer].start);
END


/*  */
char get_drive_parameters(drive)

unsigned char   drive;

BEGIN
    /* See how many drives there are */
    regs.h.ah = uc(DISK_INFO);                                      /* AC000 */
    regs.h.dl = drive;
    DiskIo(&regs,&regs,&segregs);                                   /* AC000 */

    /* See if any drives exist */
    if ((regs.h.dl == uc(0)) || ((regs.x.cflag & 1) == u(1)))       /* AC000 */
       BEGIN
        display(error_1);
        return(FALSE);
       END
    else
       BEGIN
        /* Save the number of drives */
        number_of_drives = regs.h.dl;
        if (number_of_drives > MAX_HDISK)
            number_of_drives = MAX_HDISK;
        return(TRUE);
       END
END

/*  */
char read_boot_record(cylinder,which_disk,which_head,which_sector)      /* AC000 */

unsigned        cylinder;
unsigned char   which_disk;
unsigned char   which_head;                                             /* AN000 */
unsigned char   which_sector;                                           /* AN000 */

BEGIN

    char far *buffer_pointer = boot_record;

    /* Setup read, always on a cylinder boundary */
    regs.h.ah = uc(READ_DISK);                                      /* AC000 */
    regs.h.al = uc(1);                                              /* AC000 */
    regs.h.dh = which_head;                                         /* AC000 */
    regs.h.cl = which_sector;                                       /* AC000 */

    /* Specify the disk */
    regs.h.dl = which_disk + 0x80;

    /* Need to scramble CX so that sectors and cyl's are in INT 13 format */

    if (cylinder > u(255))                                          /* AC000 */
       BEGIN
        regs.h.cl = regs.h.cl | ((char)((cylinder /256) << 6));
       END
    regs.h.ch = (unsigned char)(cylinder & 0xFF);

    /* Point at the place to write the boot record */
    regs.x.bx = FP_OFF(buffer_pointer);
    segregs.es = FP_SEG(buffer_pointer);

    /* read in the boot record */
    DiskIo(&regs,&regs,&segregs);                         /* AC000 */
    /* Check for error reading it */
    if ((regs.x.cflag & 1) != u(1))                                 /* AC000 */
       BEGIN
        return(TRUE);
       END
    else
       BEGIN
        /* Tell user there was an error */
        good_disk[which_disk] = FALSE;
        clear_screen(u(0),u(0),u(24),u(79));                        /* AC000 */
        return(FALSE);
       END
END

/*  */
void DiskIo(InRegs,OutRegs,SegRegs)
union   REGS    *InRegs;
union   REGS    *OutRegs;
struct  SREGS   *SegRegs;

BEGIN

/*C00    char    *WritePtr;  */

#ifdef DEBUG

    char    *WritePtr;  /*C00*/

    switch(InRegs->h.ah)
        {
        case 0:
        case 1:
        case 2:
        case 4:
        case 8:
        case 15:
        case 16:
            int86x((int)DISK,InRegs,OutRegs,SegRegs);                         /* AC000 */
            break;

        default:
            WritePtr = getenv("WRITE");
            if (strcmpi(WritePtr,"ON") != 0)
                BEGIN
                printf("\nDisallowing Disk I/O Request\n");
                printf("AX:%04X BX:%04X CX:%04X DX:%04X ES:%04X\n",
                        InRegs->x.ax,InRegs->x.bx,InRegs->x.cx,InRegs->x.dx,SegRegs->es);

                OutRegs->h.ah = (unsigned char) 0;
                OutRegs->x.cflag = (unsigned) 0;
                END
            else int86x((int)DISK,InRegs,OutRegs,SegRegs);

            break;

        }

#else

    int86x((int)DISK,InRegs,OutRegs,SegRegs);                         /* AC000 */

#endif

    return;

END
