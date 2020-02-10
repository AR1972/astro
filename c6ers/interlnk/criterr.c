/***
* $Workfile:   criterr.c  $
* $Revision:   1.2  $
*   $Author:   Dave Sewell  $
*     $Date:   23 Aug 1990 12:38:46  $
***/

/*  criterr.c : Alan Butt : Febuary 24, 1989 : Expansion Box Project

    This module contains the C-code portion of the critical error handler.

*/

#include <bios.h>
#include <string.h>
#include <ctype.h>

#include "dc.h"

int pascal criterr(int ax, int err_code, struct device_header far *driver)
{
#define NUM_ERRS    16

    static char *errormsg[NUM_ERRS] = {
        write_protect_error,
        unknown_unit_error,
        drive_not_ready_error,
        unknown_command_error,
        data_error,
        bad_request_struct,
        seek_error,
        unknown_media_type,
        sector_not_found,
        printer_out_of_paper,
        write_fault,
        read_fault,
        general_failure,
        unknown_error,
        unknown_error,
        invalid_disk_change
    };

    /***
    * NOTE: Critical error handlers do not use the processes stack.  Instead
    * they use MS-DOS's AuxStack.  This stack is not very big so we need to
    * be careful to not overflow it.  Hence, the buffers below are static
    * rather than on the stack
    ***/

    static char save_buff[2 * 4 * 80];
    static char device_name[9];
    static char msg[70];

    int c;
    char *err, *operation;
    int msg_len;
    int box_start;
    byte save_row, save_column;

    err = err_code >= NUM_ERRS ? unknown_error : errormsg[err_code];

    if (ax & 0x8000) {                  // if char device or bad FAT
        if (driver->attribute & ATT_CHARACTER) {
            strtcpyf(device_name, driver->name_num, sizeof(device_name));
            operation = "";
        }
        else {
            err = bad_image_of_FAT;
            goto block_dev_err;
        }
    }
    else {
block_dev_err:
        operation = ax & 0x0100 ? writing_to_drive : reading_from_drive;
        device_name[0] = (char) ((ax & 0x00ff) + 'A');
        device_name[1] = '\0';
    }

    strcpy(msg, err);
    strcat(msg, operation);
    strcat(msg, " ");
    strcat(msg, device_name);

    push_attribute(LIST_REVERSE);
    save_row = cursor_row;
    save_column = cursor_column;

    msg_len = pstrlen(msg);
    if (msg_len < (int)pstrlen(abort_or_retry)) msg_len = pstrlen(abort_or_retry);
    box_start = ((80 - msg_len) >> 1);
    save_zone(coord(11, 0), boxsize(4, 80), save_buff);
    draw_box(coord(11, box_start),  boxsize(4, msg_len + 4),
             DOUBLE, LIST_REVERSE);
    justify_str(coord(12, box_start + 2), msg_len, LIST_REVERSE, msg, 0);
    justify_str(coord(13, box_start + 2), msg_len, LIST_REVERSE, abort_or_retry, 0);
    locate(coord(13, (box_start + 2 + (msg_len - pstrlen(abort_or_retry)) / 2 + pstrlen(abort_or_retry))) );
    cursor_on();

    for (;;) {
        c = paragon_bios_keybrd(_KEYBRD_READ) & 0xFF;
                                        // Can't call read_raw_key() because
                                        // read_raw_key calls the background
                                        // function which may call DOS (the
                                        // clock() routine does!).

        if (c == 'a' || c == 'A') {     // Abort case
            quit();                     // perform any cleanup routines
            restore_screen();           // and restore the screen
            return 2;
        }

        if ( toupper(c) == 'R' || toupper(c) == 'I') { // Retry or ignore case
            restore_zone(coord(11, 0), boxsize(4, 80), save_buff);
            cursor_off();
            locate(coord(save_row, save_column));
            pop_attribute();
            return toupper(c) == 'R' ? 1 : 0;
        }

        Bell();                         // Any other character typed
    }
}
