/***
* $Workfile:   devinfo.c  $
* $Revision:   1.4  $
*   $Author:   Dave Sewell  $
*     $Date:   04 May 1990  9:12:38  $
***/

/*  devinfo.c : Alan Butt : December 21, 1988 : Expansion Box Project

    This module is responsible for getting all needed information about
    device drivers the individual logical devices.
*/

#include <stdlib.h>
#include <bios.h>
#include <string.h>
#include <stdio.h>
#include <conio.h>

#include "dc.h"

static struct device_header far *device_driver_chain = 0;
static union dpb far *disk_block_chain = 0;

extern int drdos;


/*  get_dos_info, gets the pointer to the first disk parameter block and
    the pointer to the first device driver.  This information is retrieved
    from undocumented DOS call INT 21H Subfunction 52H.
*/
static void get_dos_info(void)
{
    union REGS regs;
    struct SREGS sregs;
    union dpb far * far * pdpb;
    word *pnt;

    regs.h.ah = 0x52;
    intdosx(&regs, &regs, &sregs);

    pnt = (word *) &device_driver_chain;   // ES:BX + 22H (or 17H) == Start of NUL driver
    *pnt++ = regs.x.bx + (_osmajor > 2 ? 0x22 : 0x17);
    *pnt = sregs.es;

    pnt = (word *) &pdpb;                   // *(ES:BX) is first disk block
    *pnt++ = regs.x.bx;
    *pnt = sregs.es;

    disk_block_chain = *pdpb;
}

/*  get_device_chain() returns the pointer to the first device driver
    on the system.
*/
static struct device_header far * near get_device_chain(void)
{
    if (!device_driver_chain) get_dos_info();
    return device_driver_chain;
}

/*  get_first_dpb() returns pointer to the first disk parameter block.
    This information is retrieved from the undocumented DOS call 52H.

    This routine also sets the global variable device_driver_chain!
*/
static union dpb far * near get_first_dpb(void)
{
    if (!disk_block_chain) get_dos_info();
    return disk_block_chain;
}


void _pascal read_volume_label(int dev)
{
    char path[10];
    struct find_t find;
    struct media_id_buffer mib;
    int i, j;
    struct gen_ioctl_req gir;

    devices[dev].volume_label[0] = '\0';
    devices[dev].serial_number   = 0L;
    if (_osmajor >= 3) {
        sprintf(path, "%c:\\*.*", dev + 'A');
        if ( _dos_findfirst(path, _A_VOLID, &find) == 0 ) {
            for (i = 0, j = 0; i < 12 && find.name[i]; i++) {
                if (find.name[i] == '.') {
                    while (j < 8)
                        devices[dev].volume_label[j++] = ' ';
                }
                else
                    devices[dev].volume_label[j++] = find.name[i];
            }
            devices[dev].volume_label[j] = '\0';
        }
        if (_osmajor >= 4 && (devices[dev].header->attribute & ATT_GEN_IOCTL)) {
            gir.s.rhp_length  = sizeof(struct gen_ioctl_req);
            gir.s.rhp_unit    = devices[dev].unit;
            gir.s.rhp_command = GENERIC_IOCTL;
            gir.gen_category  = 8;
            gir.gen_function  = 0x66;
            gir.gen_data      = &mib;
            call_driver((void far *) &gir, devices[dev].header);
            if ( (gir.s.rhp_status & STATUS_ERROR) == 0) {
                devices[dev].serial_number = mib.serial_number;
            }
        }
    }
}

/*  get_device_information gets all the information about the device drivers
    and the associated logical device information.
*/
void load_device_information()
{
    #define FLOPPY(x)   ( (x == 0 || x == 1) && num_server_floppies >= 1 ) 

   char pathx [] = "X:\\*.*";
   struct find_t xfind;
    union dpb far *dpb;
    byte media_descriptor;
    unsigned num_clusters;
    unsigned sectors_per_cluster;
    unsigned bytes_per_sector;
    byte number_of_fats;
    union _bios_equipment_list {
        word w;
        struct {
            unsigned diskette_installed : 1;
            unsigned math_coprocessor   : 1;
            unsigned pointing_device    : 1;
            unsigned                    : 1;
            unsigned video_mode         : 2;
            unsigned diskette_drives    : 2;
            unsigned                    : 1;
            unsigned num_rs232          : 3;
            unsigned game_adapter       : 1;
            unsigned internal_modem     : 1;
            unsigned num_printers       : 2;
        } b;
    } equipment_list;


    lpt_drivers[0] = find_char_device("LPT1    ");
    lpt_drivers[1] = find_char_device("LPT2    ");
    lpt_drivers[2] = find_char_device("LPT3    ");
    equipment_list.w = _bios_equiplist();
    num_server_floppies = (equipment_list.w & 1) ?
                          ( (equipment_list.w & 0xC0) >> 6) + 1 :
                          0;

    dpb = get_first_dpb();
    slave_block_devices = 0;
    while (FP_OFF(dpb) != 0xffff && slave_block_devices < MAX_SLAVE_DEVICES) {
        // Loop through all DPBs getting device information
        devices[slave_block_devices].dpb = (void far *) dpb;

   // Now, if we're running on DOS 3 or later, we'll determine whether
   //  this drive is non-removable, and if so, touch it to make sure the
   //  DPB variables are current.  A side effect of this code is to
   //  set/reset the .non_removable flag as appropriate.

        devices[slave_block_devices].non_removable   = FALSE;

        if (_osmajor >= 3 && !is_il_drive(slave_block_devices) ) {
            union REGS regs;

            regs.h.ah = 0x44;
            regs.h.al = 0x08;
            regs.h.bl = (byte) slave_block_devices + (byte) 1;
            intdos(&regs, &regs);
            if ( !regs.x.cflag && regs.x.ax == 1 ) {    // Not removable!
                devices[slave_block_devices].non_removable = TRUE;

//   Touch the drive with a find first on the volume label.  This code
//   was removed from get_volume_label.  Unfortunately, it can't be called
//   from there because that function requires some of the other variables
//   which we haven't initialized yet.


            pathx [0] = (char)(slave_block_devices + (int)'A');
            _dos_findfirst(pathx, _A_VOLID, &xfind);
            }
        }

                // DRDOS bug:  puts a default value into the floppy
                // media_id byte, which may result in an incorrect
                // floppy size if the floppy has not been read yet.
                // To get around this, put a 0 in media_id, just
                // like MSDOS does for floppies that have not
                // been accessed yet.

        devices[slave_block_devices].is_floppy = (byte) FLOPPY(slave_block_devices);
        media_descriptor = 0;
        if (_osmajor == 2) {
            devices[slave_block_devices].header           = dpb->dos2.device_driver;
            devices[slave_block_devices].attribute        = dpb->dos2.device_driver->attribute;
            devices[slave_block_devices].unit             = dpb->dos2.unit;
            devices[slave_block_devices].drive            = dpb->dos2.drive;
            devices[slave_block_devices].bytes_per_sector = dpb->dos2.bytes_per_sector;
            if (!drdos || !FLOPPY(slave_block_devices))
                media_descriptor = dpb->dos2.media_id;
            num_clusters        = dpb->dos2.max_number - 1;
            sectors_per_cluster = dpb->dos2.max_cluster + 1;
            bytes_per_sector    = dpb->dos2.bytes_per_sector;
            number_of_fats      = dpb->dos2.number_of_fats;
        }
        else if (_osmajor == 3) {
            devices[slave_block_devices].header           = dpb->dos3.device_driver;
            devices[slave_block_devices].attribute        = dpb->dos3.device_driver->attribute;
            devices[slave_block_devices].unit             = dpb->dos3.unit;
            devices[slave_block_devices].drive            = dpb->dos3.drive;
            devices[slave_block_devices].bytes_per_sector = dpb->dos3.bytes_per_sector;
            if (!drdos || !FLOPPY(slave_block_devices))
                media_descriptor = dpb->dos3.media_id;
            num_clusters        = dpb->dos3.max_number - 1;
            sectors_per_cluster = dpb->dos3.max_cluster + 1;
            bytes_per_sector    = dpb->dos3.bytes_per_sector;
            number_of_fats      = dpb->dos3.number_of_fats;
        }
        else {
            devices[slave_block_devices].header           = dpb->dos4.device_driver;
            devices[slave_block_devices].attribute        = dpb->dos4.device_driver->attribute;
            devices[slave_block_devices].unit             = dpb->dos4.unit;
            devices[slave_block_devices].drive            = dpb->dos4.drive;
            devices[slave_block_devices].bytes_per_sector = dpb->dos4.bytes_per_sector;
            if (!drdos || !FLOPPY(slave_block_devices))
                media_descriptor = dpb->dos4.media_id;
            num_clusters        = dpb->dos4.max_number - 1;
            sectors_per_cluster = dpb->dos4.max_cluster + 1;
            bytes_per_sector    = dpb->dos4.bytes_per_sector;
            number_of_fats      = dpb->dos4.number_of_fats;
        }
        devices[slave_block_devices].sectors_per_cluster = (byte) sectors_per_cluster;
        devices[slave_block_devices].number_of_fats = number_of_fats;
        devices[slave_block_devices].media_descriptor = media_descriptor;
        if (media_descriptor) {
            devices[slave_block_devices].data_size =
                    (unsigned long) num_clusters *
                    (unsigned long) sectors_per_cluster *
                    (unsigned long) bytes_per_sector;
        }


        devices[slave_block_devices].volume_label[0] = '\0';
        devices[slave_block_devices].serial_number   = 0L;

        // If DOS 3.0 and non-removable media, get volume label.
        if (devices[slave_block_devices].non_removable == TRUE)
          read_volume_label(slave_block_devices);
        slave_block_devices++;
        dpb = _osmajor == 2 ? (union dpb far *) dpb->dos2.next_block :
              _osmajor == 3 ? (union dpb far *) dpb->dos3.next_block :
                              (union dpb far *) dpb->dos4.next_block;
    }
}


/*  find_char_device() looks for the first character device with the passed in
    name.  If found, a pointer to the device header is returned.  If not found,
    a NULL pointer is returned.
*/
struct device_header far *find_char_device(char *name)
{
    struct device_header far *header = get_device_chain();

    while (FP_OFF(header) != 0xffff) {
        if (!fmemcmpf(header->name_num, (char far *) name, DEVICE_NAME_SIZE))
            return header;
        header = header->next_header;
    }
    return (struct device_header far *) 0;
}




/* Below is code specific to finding open files -- This is all DOS version
    dependant code.
*/


struct open_file_entry_dos2 {
    byte    open_flag;              // non-zero if file is open
    word    reserved;
    byte    unit_number;            // Unit number (0=char dev,1=A,2=B,...)
    char    file_name[11];          // File name
    byte    reserved2[25];
};

struct open_file_entry_dos3 {
    word    handle_count;           // # of handles that refer to this entry
    byte    reserved[5];
    void far *driver_dpb;           // Pointer to device driver or DPB
    byte    reserved2[21];
    char    file_name[11];          // File name
    byte    reserved3[10];
};

struct open_file_entry_dos4 {
    word    handle_count;           // # of handles that refer to this entry
    byte    reserved[5];
    void far *driver_dpb;           // Pointer to device driver or DPB
    byte    reserved2[21];
    char    file_name[11];          // File name
    byte    reserved3[16];
};

struct open_file_list {
    struct  open_file_list far *next;   // Pointer to next open file list
    word    count;                      // # of open file entries following
    char    table[1];                   // Beginning of open file entry table
};

              
static struct open_file_list far *find_open_file_chain(void)
{
    union REGS regs;
    struct SREGS sregs;

    struct open_file_list far *pnt;
    word far *src;
    word *dst;

    regs.h.ah = 0x52;
    intdosx(&regs, &regs, &sregs);

    dst = (word *) &src;
    *dst++ = regs.x.bx + 4;
    *dst = sregs.es;

    dst = (word *) &pnt;
    *dst++ = *src++;
    *dst++ = *src++;

    return pnt;
}

/*  open_file -- Returns the name of the first open file on a given device

    Inputs:
        dev         Unit number to check (0 = 'A', 1 = B, ...).
        devices[]   Must have the dpb field set (load_device_information() must
                    have been executed).
        filename    Buffer for open file name

    Outputs:
        retval      TRUE if an open file was found on the device,
                    otherwise FALSE
        filename    Name of the file that was open (If one exists).  The
                    name will be in directory entry format without a
                    terminating '\0'.
*/

int open_file(int dev, char *filename)
{
    struct open_file_list far *pnt = find_open_file_chain();
    struct open_file_entry_dos2 far *dos2;
    struct open_file_entry_dos3 far *dos3;
    struct open_file_entry_dos4 far *dos4;
    unsigned int i;

    while (FP_OFF(pnt) != 0xffff) {
        if (_osmajor == 3) {
            dos3 = (struct open_file_entry_dos3 far *) pnt->table;
            for (i = 0; i < pnt->count; i++) {
                if ((dos3 + i)->handle_count &&
                    devices[dev].dpb == (dos3 + i)->driver_dpb) {
                    memcpyf(filename, (dos3 + i)->file_name, 11);
                    return TRUE;
                }
            }
        }
        else if (_osmajor == 4) {
            dos4 = (struct open_file_entry_dos4 far *) pnt->table;
            for (i = 0; i < pnt->count; i++) {
                if ((dos4 + i)->handle_count &&
                    devices[dev].dpb == (dos4 + i)->driver_dpb) {
                    memcpyf(filename, (dos4 + i)->file_name, 11);
                    return TRUE;
                }
            }
        }
        else {  // DOS 2.0
            dos2 = (struct open_file_entry_dos2 far *) pnt->table;
            for (i = 0; i < pnt->count; i++) {
                if ((dos2 + i)->unit_number == (byte) (dev + 1) &&
                    (dos2 + i)->open_flag) {
                    memcpyf(filename, (dos2 + i)->file_name, 11);
                    return TRUE;
                }
            }
        }
        pnt = pnt->next;
    }
    return FALSE;
}
