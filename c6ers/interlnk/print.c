/***
* $Workfile:   bioshndl.c  $
* $Revision:   1.1  $
*   $Author:   Dave Sewell  $
*     $Date:   27 Jun 1989 14:54:46  $
***/

/*  bioshndl.c : Alan Butt : May 18, 1989 : Expansion Box Project

    This routine contains contains the handlers for BIOS printing packets.
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <bios.h>
#include "fastlynx.h"
#include "dc.h"

#define TIMEOUT         0x01            // Timeout bit
#define ERROR           0x08            // Error bit
#define SELECTED        0x10            // Selected bit
#define OUT_OF_PAPER    0x20            // Out of paper bit
#define ACKNOWLEDGE     0x40            // Acknowledge bit
#define NOT_BUSY        0x80            // Not busy bit

int prn_write_handler(word count)
{
    byte lpt_id;
    word bufsiz;
    char far *buf;

    if (packet.lpt_o_r.print_count > 1) {
        bufsiz = FxReceive(buffer, (word) sizeof(buffer));
        if (!bufsiz) {
            return FALSE;
        }
        buf = buffer;
    }
    else {
        bufsiz = packet.lpt_o_r.print_count;
        buf = (char far *) &packet.lpt_o_r.print_data;
    }
    lpt_id = packet.lpt_o_r.lpt_id;
    if (lpt_id > 2 || !lpt_drivers[lpt_id]) {
        rhp.io_ans.s.rhp_status = STATUS_ERROR | STATUS_DONE | ERR_UNK_UNIT;
        rhp.io_ans.io_transfered = 0;
    }
    else if (bufsiz != packet.lpt_o_r.print_count) {
        rhp.io_ans.s.rhp_status = STATUS_ERROR | STATUS_DONE | ERR_WRITE_FAULT;
        rhp.io_ans.io_transfered = 0;
    }
    else {
        rhp.io_req.s.rhp_length = sizeof(struct io_req);
        rhp.io_req.s.rhp_command = WRITE;
        rhp.io_req.io_data = buf;
        rhp.io_req.io_requested = packet.lpt_o_r.print_count;
        call_driver((void far *) &rhp, lpt_drivers[lpt_id]);
    }
    packet.lpt_o_a.lpt_status = rhp.io_ans.s.rhp_status;
    packet.lpt_o_a.lpt_transferred = rhp.io_ans.io_transfered;
    return FxSend(&packet, sizeof(struct lpt_o_a));
}

int prn_cmd_handler(word count)
{
    byte lpt_id;
    byte command;
    static char lpt_name[] = "LPT?";
    static int lpt_handles[3] = { -1, -1, -1 };

    if (packet.packet_type == OUTPUT_STATUS_REQ)
        command = OUTPUT_STATUS;
    else if (packet.packet_type == FLUSH_OUTPUT_REQ)
        command = FLUSH_OUTPUT;
    else if (packet.packet_type == AUX_DEV_OPEN_REQ)
        command = DEVICE_OPEN;
    else
        command = DEVICE_CLOSE;
    lpt_id = packet.lpt_cmd_r.lpt_id;
    if (lpt_id > 2 || !lpt_drivers[lpt_id]) {
        rhp.s.rhp_status = STATUS_ERROR | STATUS_DONE | ERR_UNK_UNIT;
    }
    else {
        rhp.s.rhp_length = sizeof(struct static_rhp);
        rhp.s.rhp_command = command;
        if ( (lpt_drivers[lpt_id]->attribute & ATT_OCRM) ||
             (command == OUTPUT_STATUS) || (command == FLUSH_OUTPUT) )
            call_driver((void far *) &rhp, lpt_drivers[lpt_id]);
        else {      // Device open or device close not supported - use INT 21H
            if (command == DEVICE_OPEN) {
                if (lpt_handles[lpt_id] < 0) {
                    lpt_name[3] = (char) lpt_id + (char) '1';
                    if ( _dos_open(lpt_name, 1, &lpt_handles[lpt_id]) != 0 )
                        lpt_handles[lpt_id] = -1;
                }
            }
            else if (command == DEVICE_CLOSE) {
                if (lpt_handles[lpt_id] >= 0) {
                    _dos_close(lpt_handles[lpt_id]);
                    lpt_handles[lpt_id] = -1;
                }
            }
            rhp.s.rhp_status = STATUS_DONE;     // Fake it.
        }
    }
    packet.lpt_cmd_a.lca_status = rhp.s.rhp_status;
    return FxSend(&packet, sizeof(struct lpt_cmd_a));
}

int bios_handler(word count)
{
    int command;
    word status, port;

    port = packet.bios_r.bsr_bios_port;
    command = packet.packet_type - BIOS_PRINT_REQ;
    if (port < (word) num_lpt && printers[port].enabled) {
        status = _bios_printer(command, port, packet.bios_r.bsr_char);
    }
    else
        status = SELECTED | ERROR | TIMEOUT;
    packet.bios_a.bsa_status = (byte) status;

    // send response packet

    if ( !FxSend(&packet, sizeof(struct bios_a)) ) {
        return FALSE;
    }
    return TRUE;
}

