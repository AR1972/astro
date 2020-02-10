/***
* $Workfile:   handler.c  $
* $Revision:   1.5  $
*   $Author:   Dave Sewell  $
*     $Date:   04 May 1990  9:12:48  $
***/

/*  handler.c : Alan Butt : February 1, 1989 : Expansion Box Project

    This routine contains the main slave loop.

*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <conio.h>
#include <ctype.h>
#include "fastlynx.h"
#include "server.h"
#include "dc.h"

byte dos5_switcher = FALSE;                 // Set in switcher.asm

static long server_id;
static long last_client_id;

void make_server_id(void)                   // Called once at system init time
{
    long id_value;                          // Start with random stack value
    long _far *bios_data = (long _far *) 0x00400000L;
    int i;

    for (i = 0; i < 64; i++) {
        id_value ^= *bios_data++;
    }
    *((unsigned *)&id_value) ^= fread_ticks();
    *((unsigned *)&id_value + 1) ^= fread_ticks();
    if (id_value == 0L)
        id_value++;
    server_id = id_value;
}

int server_info_handler(word count)
{
    struct server_info_req request;
    struct server_info_ans answer;

    if ( !FxReceive(&request, sizeof(request)) )
        return FALSE;
    memset(&answer, '\0', sizeof(answer));      // Unused fields must be zero
    answer.os_type   = OS_MSDOS;
    answer.developer = DEV_SEWELL;
    answer.product   = PRODUCT_DOSLINK;
    answer.version   = PRODUCT_VERSION;
    answer.device_server = TRUE;                // Device server supported
    answer.agreed_caps.checksum = FALSE;        // Can't support checksum
    answer.agreed_caps.crc      = TRUE;         // Must use CRC
    fx_max_serial_block = min(request.client_caps.max_serial_block, FxSettings.max_serial_block);
    answer.agreed_caps.max_serial_block = fx_max_serial_block;
    answer.last_client_id = last_client_id;
    answer.server_id = server_id;
    // If new master ID code, then master system must have rebooted.
    // Save the new ID code and clear all auxiliary driver information

    if (request.client_id != last_client_id ||
        request.last_server_id != server_id) {
        last_client_id = request.client_id;
        reset_disk();
        setup_block_devices();
    }
    return FxSend(&answer, sizeof(answer) -
        (request.want_packets_supported ? 0 : sizeof(answer.packets_supported)) );
}

int unknown_handler(word count)
{
    return FALSE;
}

/*  An init packet request sends information about the block devices on this
    system.
*/
int init_handler(word count)
{
    register int i;
    byte sp;
    byte mp;
    byte desired_prn_map[3];
    byte desired_mapping[MAX_DEVICES];
    byte avail_lpt[3];
    extern byte win386_enh_mode;

    config.master_dos_version  = packet.init_packet_r.ipr_dos_version;
    config.master_max_devices  = packet.init_packet_r.ipr_max_devices;
    config.master_first_unit   = packet.init_packet_r.ipr_first_unit;
    memcpy(desired_prn_map, packet.init_packet_r.ipr_prn_map, 3);
    memcpy(desired_mapping, packet.init_packet_r.ipr_mapping, MAX_DEVICES);



    // Build initial response packet

    packet.init_packet_a.ipa_devices = (byte) slave_block_devices;
    packet.init_packet_a.ipa_major_version = MAJOR_VERSION;
    packet.init_packet_a.ipa_minor_version = MINOR_VERSION;
    packet.init_packet_a.ipa_multitasker = win386_enh_mode | desqview | dos5_switcher;
    packet.init_packet_a.ipa_dos_version = slave_dos_version;
    make_printer_map();
    memset(actual_prn_map, UNASSIGNED, 3);
    memset(avail_lpt, FALSE, 3);
    for (sp = 0; sp < (byte) num_lpt; sp++) {
        if (printers[sp].enabled)
            avail_lpt[sp] = TRUE;
    }
    for (mp = 0; mp < 3; mp++) {      // First do hard-coded assignments
        sp = desired_prn_map[mp];
        if (sp < 3 && avail_lpt[sp]) {
            actual_prn_map[mp] = sp;
            avail_lpt[sp] = FALSE;
        }
    }
    for (mp = 0; mp < 3; mp++) {        // Then pick up the DON'T CAREs
        if (desired_prn_map[mp] == DONT_CARE) {
            for (sp = 0; sp < (byte) num_lpt; sp++) {
                if (avail_lpt[sp]) {
                    actual_prn_map[mp] = sp;
                    avail_lpt[sp] = FALSE;
                    break;
                }
            }
        }
    }
    memcpy(packet.init_packet_a.ipa_prn_map, actual_prn_map, 3);
    generate_drive_mappings(packet.init_packet_a.ipa_mapping,
                            config.master_max_devices, desired_mapping);

    for (i = 0; i < slave_block_devices; i++) {
        packet.init_packet_a.ipa_attributes[i] = devices[i].attribute;
    }

    if ( !FxSend(&packet, sizeof(struct init_packet_a)) ) {
        return FALSE;
    }

    show_drive_mappings(packet.init_packet_a.ipa_mapping, TRUE);
    return TRUE;
}

/*  Media Check Handler.  This routine handles a media check for a given
    drive.

    If this is DOS 3.0+, the driver has the OCRM bit set in its attribute word,
    and the media has changed, then the driver will (should) return the volume
    label.  This volume label will be send on to the master.
*/
int media_check_handler(word count)
{
    int device;
    int volume_copied = FALSE;

    memset(&rhp, '\0', sizeof(rhp));                // Start with clean RHP
    device = (int) packet.media_check_r.mcr_unit;
    if (device >= slave_block_devices) {
        rhp.media_check_ans.media_changed = MEDIA_DONT_KNOW;
        rhp.s.rhp_status = STATUS_ERROR | STATUS_DONE | ERR_UNK_UNIT;
    }
    else {
        rhp.media_check_req.s.rhp_length = 15;
        rhp.media_check_req.s.rhp_unit = devices[device].unit;
        rhp.media_check_req.s.rhp_command = MEDIA_CHECK;
        rhp.media_check_req.media_id = packet.media_check_r.mcr_media_id;
        call_driver((void far *) &rhp, devices[device].header);
        if ((rhp.media_check_ans.media_changed == MEDIA_CHANGED) && _osmajor >= 3) {
            volume_copied = TRUE;
            memcpyf(packet.media_check_a.mca_volume, rhp.media_check_ans.media_label, MAX_VOLUME);
        }
    }

    // Build response packet
    if (!volume_copied) {
        memcpyf(packet.media_check_a.mca_volume,
            devices[device].volume_label, MAX_VOLUME + 4);
    }
    packet.media_check_a.mca_status  = rhp.s.rhp_status;
    packet.media_check_a.mca_changed = rhp.media_check_ans.media_changed;

    // Send response packet

    return FxSend(&packet, sizeof(struct media_check_a));
}

static void server_change_fail(void)
{
    rhp.s.rhp_status = STATUS_ERROR | STATUS_DONE | ERR_FAILURE;
}


static int read_first_fat_sector(int device, byte media_id)
{
    word sector;

    if (device >= slave_block_devices) {
        rhp.s.rhp_status = STATUS_ERROR | STATUS_DONE | ERR_UNK_UNIT;
        rhp.io_ans.io_transfered = 0;
        return FALSE;
    }

    rhp.io_req.s.rhp_length = sizeof(struct io_req);
    rhp.io_req.s.rhp_unit = devices[device].unit;
    rhp.io_req.s.rhp_command = READ;
    rhp.io_req.media_id = media_id;
    rhp.io_req.io_data = buffer;
    rhp.io_req.io_requested = 1;        // Read one sector
    sector = 1;                         // Read sector one
    if (slave_dos_version < DOS_VERSION(3, 31) ||
            (devices[device].attribute & ATT_HUGE) == 0)
        rhp.io_req.io_start = sector;
    else if (slave_dos_version == DOS_VERSION(3, 31)) {
        rhp.io_req.s.rhp_length =
            (byte) ((byte *) &rhp.io_req.reserved - (byte *) &rhp);
        *((dword *) &rhp.io_req.io_start) = (dword) sector;
    }
    else {                              // Else huge and > DOS 3.31
        rhp.io_req.io_start = 0xFFFF;
        rhp.io_req.io_huge_start = (dword) sector;
    }

    call_driver((void far *) &rhp, devices[device].header);
    return rhp.s.rhp_status & STATUS_ERROR ? FALSE : TRUE;
}

/*  Build BPB hander.  This routine handles a build bpb request for a given
    driver and drive.

    Note: We need to save the sector size of the device.
*/
int build_bpb_handler(word count)
{
    word length;
    int device;

    count;

    device = (int) packet.build_bpb_r.bbr_unit;

    if (device >= slave_block_devices) {
        rhp.s.rhp_status = STATUS_ERROR | STATUS_DONE | ERR_UNK_UNIT;
    }
    else {
        if ( (devices[device].attribute & ATT_NON_IBM) ||
             read_first_fat_sector(device, packet.build_bpb_r.bbr_media_id) )
        {                  // not ibm format or no error reading fat
            // Note: Length needs to be 22 (length of answer).
            rhp.build_bpb_req.s.rhp_length = sizeof(struct build_bpb_ans);
            rhp.build_bpb_req.s.rhp_unit = devices[device].unit;
            rhp.build_bpb_req.s.rhp_command = BUILD_BPB;
            rhp.build_bpb_req.media_id = packet.build_bpb_r.bbr_media_id;
            rhp.build_bpb_req.bpb_fat = buffer;

            call_driver((void far *) &rhp, devices[device].header);
        }
    }

    // Build response packet
    packet.build_bpb_a.bba_status = rhp.s.rhp_status;
    if ( !(rhp.s.rhp_status & STATUS_ERROR) ) {     // No error building BPB
        packet.build_bpb_a.bba_bpb = *rhp.build_bpb_ans.bpb_bpb;
        devices[device].bytes_per_sector =
            rhp.build_bpb_ans.bpb_bpb->bytes_per_sector;
        length = sizeof(struct build_bpb_a);
        // If media is removeable, read the volume label
        if (devices[device].non_removable == FALSE) {
            reset_disk();
            read_volume_label(device);
        }
    }
    else {                              // Else error getting BPB
        length = (char *) &packet.build_bpb_a.bba_bpb  - (char *) &packet;
    }

    // Send Response Packet

    if ( !FxSend(&packet, length) ) {
        return FALSE;
    }

    devices[device].invalid = FALSE;    // Clear invalid flag after we've
                                        // successfully build a BPB
    return TRUE;
}


/*  Read request Handler.  This routine handles a read request for a given
    device driver and drive.

    If this is DOS 3.0+ and error ERR_DISK_CHANGE (0FH) is returned, the driver
    will (should) return the volume label.
*/
int read_handler(word count)
{
    word volume_length;
    int device;

    count;

    device = (int) packet.io_r.ior_unit;
    if (device >= slave_block_devices) {
        rhp.s.rhp_status = STATUS_ERROR | STATUS_DONE | ERR_UNK_UNIT;
    }
    else {
        rhp.io_req.s.rhp_length = 22;
        rhp.io_req.s.rhp_unit = devices[device].unit;
        rhp.io_req.s.rhp_command = READ;
        rhp.io_req.media_id = packet.io_r.ior_media_id;
        rhp.io_req.io_data = buffer;
        rhp.io_req.io_requested = packet.io_r.ior_requested;
        if (slave_dos_version != DOS_VERSION(3, 31)) {
            rhp.io_req.io_start = packet.io_r.ior_start;
            rhp.io_req.io_huge_start = packet.io_r.ior_huge_start;
        }
        else {  // else MS-DOS 3.31
            rhp.io_req.s.rhp_length =
                (byte) ((byte *) &rhp.io_req.reserved - (byte *) &rhp);
            *((dword *) &rhp.io_req.io_start) = packet.io_r.ior_huge_start;
        }

        call_driver((void far *) &rhp, devices[device].header);
    }

    // build response packet

    packet.io_a.ioa_status = rhp.s.rhp_status;
    if (rhp.s.rhp_status & STATUS_ERROR)
        packet.io_a.ioa_transfered = 0;
    else
        packet.io_a.ioa_transfered = rhp.io_ans.io_transfered;
    packet.io_a.ioa_volume[0] = '\0';
    packet.io_a.ioa_serial_number = 0;
    volume_length = 0;
    if ((rhp.s.rhp_status & 0xff) == ERR_DISK_CHANGE && _osmajor >= 3) {
        memcpyf(packet.io_a.ioa_volume, rhp.io_ans.io_label, MAX_VOLUME);
        volume_length = MAX_VOLUME + 4;
    }

    // send response packet

    if ( !FxSend(&packet, (packet.io_a.ioa_volume
        - (char *) &packet) + volume_length) ) {
        return FALSE;
    }

    // send data

    if (rhp.io_ans.io_transfered) {
        if ( !FxSend(buffer, rhp.io_ans.io_transfered *
            devices[device].bytes_per_sector) ) {
            return FALSE;
        }
    }

    return TRUE;
}


/*  Write request Handler.  This routine handles a write request for a given
    device driver and drive.

    If this is DOS 3.0+ and error ERR_DISK_CHANGE (0FH) is returned, the driver
    will (should) return the volume label.
*/
int write_handler(word count)
{
    int device;
    word bufsiz;
    word volume_length;
    byte command;

    count;

    if (packet.packet_type == WRITE_REQ) {
        command = WRITE;
    }
    else {
        command = WRITE_VERIFY;
    }


    // get data to be written

    bufsiz = FxReceive(buffer, (word) sizeof(buffer));
    if (fx_errno) { // NOTE: have to check fx_errno because bufsize can be zero
        return FALSE;
    }

    device = (int) packet.io_r.ior_unit;

    if (device >= slave_block_devices) {
        rhp.s.rhp_status = STATUS_ERROR | STATUS_DONE | ERR_UNK_UNIT;
        rhp.io_ans.io_transfered = 0;
    }
    else if (bufsiz != packet.io_r.ior_requested *
        devices[device].bytes_per_sector) {
        rhp.s.rhp_status = STATUS_ERROR | STATUS_DONE | ERR_WRITE_FAULT;
        rhp.io_ans.io_transfered = 0;
    }
    else {
        rhp.io_req.s.rhp_length = 22;
        rhp.io_req.s.rhp_unit = devices[device].unit;
        rhp.io_req.s.rhp_command = command;
        rhp.io_req.media_id = packet.io_r.ior_media_id;
        rhp.io_req.io_data = buffer;
        rhp.io_req.io_requested = packet.io_r.ior_requested;
        rhp.io_req.io_start = packet.io_r.ior_start;
        rhp.io_req.io_huge_start = packet.io_r.ior_huge_start;
        if (slave_dos_version == DOS_VERSION(3, 31)) {
            rhp.io_req.s.rhp_length =
                (byte) ((byte *) &rhp.io_req.reserved - (byte *) &rhp);
            *((dword *) &rhp.io_req.io_start) = packet.io_r.ior_huge_start;
        }

        call_driver((void far *) &rhp, devices[device].header);
    }

    // build response packet 

    packet.io_a.ioa_status = rhp.s.rhp_status;
    packet.io_a.ioa_transfered = rhp.io_ans.io_transfered;
    packet.io_a.ioa_volume[0] = '\0';
    packet.io_a.ioa_serial_number = 0;
    volume_length = 0;
    if ((rhp.s.rhp_status & 0xff) == ERR_DISK_CHANGE && _osmajor >= 3) {
        memcpyf(packet.io_a.ioa_volume, rhp.io_ans.io_label, MAX_VOLUME);
        volume_length = MAX_VOLUME + 4;
    }

    return FxSend(&packet, (packet.io_a.ioa_volume
                 - (char *) &packet) + volume_length);
}


/*  Error Handler.  No longer used. */
int error_handler(word count)
{
    return TRUE;
}

int ocrm_handler(word count)
{
    int device;
    byte command;

    count;

    if (packet.packet_type == DEV_OPEN_REQ) {
        command = DEVICE_OPEN;
    }
    else {
        command = DEVICE_CLOSE;
    }

    device = (int) packet.ocrm_r.ocr_unit;

    if (device >= slave_block_devices) {
        rhp.s.rhp_status = STATUS_ERROR | STATUS_DONE | ERR_UNK_UNIT;
    }
    else {
        rhp.s.rhp_length  = sizeof(struct static_rhp);
        rhp.s.rhp_unit    = (byte) device;
        rhp.s.rhp_command = command;

        call_driver((void far *) &rhp, devices[device].header);
    }

    // build response packet

    packet.ocrm_a.oca_status = rhp.s.rhp_status;

    // 4) send response packet

    return FxSend(&packet, sizeof(struct ocrm_a));
}

int gen_ioctl_handler(word count)
{
    int device;
    struct media_id_buffer media_id_buffer;
    byte function = 0;

    device = (int) packet.gen_ioctl_r.gir_unit;
    if (device >= slave_block_devices) {
        rhp.s.rhp_status = STATUS_ERROR | STATUS_DONE | ERR_UNK_UNIT;
    }
    else {
        function = packet.gen_ioctl_r.gir_function;
        if (function == 0x46) {
            if ( !FxReceive(&media_id_buffer, sizeof(media_id_buffer)) ) {
                return FALSE;
            }
        }
        rhp.s.rhp_length  = sizeof(struct gen_ioctl_req);
        rhp.s.rhp_unit    = (byte) device;
        rhp.s.rhp_command = GENERIC_IOCTL;
        rhp.gen_ioctl_req.gen_category = packet.gen_ioctl_r.gir_category;
        rhp.gen_ioctl_req.gen_function = packet.gen_ioctl_r.gir_function;
        rhp.gen_ioctl_req.gen_data = &media_id_buffer;

        call_driver((void far *) &rhp, devices[device].header);
    }

    // build response packet

    packet.gen_ioctl_a.gia_status = rhp.s.rhp_status;

    // 4) send response packet

    if ( !FxSend(&packet, sizeof(struct gen_ioctl_a)) ) {
        return FALSE;
    }
    if ( (rhp.s.rhp_status & STATUS_ERROR) == 0 ) {
        if (function == 0x66)
            return FxSend(&media_id_buffer, sizeof(media_id_buffer));
    }
    return TRUE;
}

