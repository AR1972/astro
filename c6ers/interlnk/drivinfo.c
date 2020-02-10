/***
* $Workfile:   drivinfo.c  $
* $Revision:   1.8  $
*   $Author:   Dave Sewell  $
*     $Date:   22 Oct 1990 14:58:10  $
***/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "fastlynx.h"
#include "dc.h"
/* BEGIN IPG -  define added to allow translation of "Mb" and "Kb"*/
#include "IPG.h"
/* END IPG -	  for MBSTRING and KBSTRING	*/
void prepare_drive_size(int device_index, char *size_buff)
{
    if (devices[device_index].data_size == 0L ||
        devices[device_index].is_floppy       ||
        (_osmajor >= 3 && devices[device_index].non_removable == FALSE) )
        size_buff[0] = '\0';
    else if (devices[device_index].data_size < 2000000L)
	sprintf(size_buff, KBSTRING,
            (int) (devices[device_index].data_size / 1000L) );
    else
	sprintf(size_buff, MBSTRING,
            (int) (devices[device_index].data_size / (1000L * 1000L) ) );
}

int drive_info_handler(word count)
{
    static char handler[] = "drive_info";
    struct drive_info_a far *drive_info_a = (struct drive_info_a far *) buffer;
    int i;
    char size_buff[15];

    count;

    i = packet.drive_info_r.server_drive_num;
    prepare_drive_size(i, size_buff);
    fstrcpy(drive_info_a->size, size_buff);
    fstrcpy(drive_info_a->volume_label, devices[i].volume_label);
    if ( !FxSend(drive_info_a, sizeof(struct drive_info_a)) ) {
        return FALSE;
    }
    return TRUE;
}
