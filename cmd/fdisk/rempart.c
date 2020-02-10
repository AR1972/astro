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
*  09/18/90  EGH  C26  This module was created to contain the routines
*                      remove_partition() and remove_volume().  These
*                      routines eliminate duplicate code.
*
******************************************************************************/

#include "dos.h"
#include "fdisk.h"
#include "extern.h"
#include "subtype.h"
#include "string.h"

/*  */

void remove_partition(drive,entry)

char        drive;
unsigned    entry;

BEGIN

    /* Set Partition entry to zero */
    part_table[drive][entry].boot_ind = uc(0);
    part_table[drive][entry].start_head = uc(0);
    part_table[drive][entry].start_sector = uc(0);
    part_table[drive][entry].start_cyl = u(0);
    part_table[drive][entry].sys_id = uc(0);
    part_table[drive][entry].end_head = uc(0);
    part_table[drive][entry].end_sector = uc(0);
    part_table[drive][entry].end_cyl = u(0);
    part_table[drive][entry].rel_sec = ul(0);
    part_table[drive][entry].num_sec = ul(0);
    part_table[drive][entry].changed = (FLAG)TRUE;
    part_table[drive][entry].mbytes_used = f(0);
    part_table[drive][entry].percent_used = u(0);

    strcpy(part_table[drive][entry].system,c(NUL));
    strcpy(part_table[drive][entry].vol_label,c(NUL));

    return;
END


/*  */
void remove_volume(drive,entry)

char        drive;
unsigned    entry;

BEGIN

    /* Set volume entry to zero */
    ext_table[drive][entry].boot_ind = uc(0);
    ext_table[drive][entry].start_head = uc(0);
    ext_table[drive][entry].start_sector = uc(0);
    ext_table[drive][entry].start_cyl = u(0);
    ext_table[drive][entry].sys_id = uc(0);
    ext_table[drive][entry].end_head = uc(0);
    ext_table[drive][entry].end_sector = uc(0);
    ext_table[drive][entry].end_cyl = u(0);
    ext_table[drive][entry].rel_sec = ul(0);
    ext_table[drive][entry].num_sec = ul(0);
    ext_table[drive][entry].mbytes_used = f(0);
    ext_table[drive][entry].percent_used = u(0);
    ext_table[drive][entry].changed = TRUE;
    ext_table[drive][entry].drive_letter = NUL;

    strcpy(ext_table[drive][entry].system,c(NUL));
    strcpy(ext_table[drive][entry].vol_label,c(NUL));

    return;
END

