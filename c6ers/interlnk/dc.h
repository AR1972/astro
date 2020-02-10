/***
* $Workfile:   dc.h  $
* $Revision:   1.8  $
*   $Author:   Dave Sewell  $
*     $Date:   22 Oct 1990 14:59:36  $
***/

/*  ebox.h : Alan Butt : December 19, 1988 : Expansion Box Project
 
    This file contains macros/constants/definitions for the slave program.
*/

#include "osdep.h"                      // Include Paragon Library files
#include "umfunc.h"

#include "drivers.h"                    // Include ebox files
#include "packets.h"
#include "boxfunc.h"

#include "cwuser.h"                     // Include C-Worthy Library files
#include "userform.h"
#include "str.h"

/*  DOS_VERSION() takes two arguments.  The first is placed into the high byte
    of an integer and the second into the low byte.  It is used to build
    numbers to be compared with master_dos_version and slave_dos_version.
*/
#define DOS_VERSION(x, y) (((x) << 8) | (y))


#define MAX_SLAVE_DEVICES   26          // Max number of logical block devices
                                        // on the slave system

#define SCROLL_COLOR    (color(WHITE,BLACK) | MONO_NORMAL)
#define WARNING_COLOR	(color(WHITE, MAGENTA) | INTENSE | MONO_REVERSE)
#define ERROR_COLOR	   (color(WHITE, RED) | INTENSE | MONO_REVERSE)


typedef void (pascal *PVF)(void);       // Pointer to Void Function type.
typedef int  (*PIF)(void);              // Pointer to Integer Function type.
typedef int  (*PIFW)(word);             // Pointer to Int Function/word argument

struct device {
    struct device_header far *header;   // Pointer to device driver header
    void far *dpb;                      // Pointer to units DPB
    word attribute;                     // Device driver attribute word
    word bytes_per_sector;              // Bytes per sector on device
    byte drive;                         // Drive # (0 = A, 1 = B, ...)
    byte unit;                          // Unit number within driver
    byte invalid;                       // Media invalid flag
    byte media_descriptor;              // Media descriptor byte (0 if unknown)
    unsigned long data_size;            // Size of data area in bytes.
    byte number_of_fats;                // Number of FAT table copies.
    byte sectors_per_cluster;           // Number of sectors per cluster.
    byte non_removable;                 // TRUE if known to be non-removable
    char volume_label[12];              // Volume label.
    unsigned long serial_number;        // Volume serial number (0 if unknown)
    byte is_floppy;                     // TRUE if A: or B: floppy disk
};

struct char_device {                    // Auxiliary device driver names
    char master_name[DEVICE_NAME_SIZE]; // Name of device on master system
    char slave_name[DEVICE_NAME_SIZE];  // Name of device on slave system
};                                      // *master_name[] <= ' ' means invalid

/*** The following bit is OR'ed into the baud rate if 7-wire mode is OK ***/

#define PORT_LIST_SIZE  30

struct config {
    enum    baud baud;                  // Baud rate for serial communication
    char    port_list[PORT_LIST_SIZE];  // Serial/Parallel ports to use

    // Block driver init inforation

    word    master_dos_version;         // DOS version of master (WORD format)
    word    master_code;                // ID code of master driver
    byte    master_max_devices;         // Max number of ebox devices on master
    byte    master_first_unit;          // First drive number in master driver

};




enum connection_state { No_connection, Serial_connection, Parallel_connection,
                        No_connection_repaint };

extern char cdecl version[];            // Version number string

/*  If EXTERN is not defined, the following declarations will be defined
    as external.  If EXTERN is defined, they will be the real declarations.
    EXTERN should only be defined in one module that includes this file.
*/

#ifdef EXTERN
    #undef EXTERN
    #define EXTERN                      /* Insure EXTERN is "" */
    #define __INITIALIZERS
#else
    #define EXTERN extern
#endif

#ifdef __INITIALIZERS
    byte actual_prn_map[3] = { UNASSIGNED, UNASSIGNED, UNASSIGNED };
#else
    EXTERN byte actual_prn_map[3];
#endif


EXTERN int num_server_floppies;         // Number of floppy drives on server.

EXTERN int slave_block_devices;         // Count of block devices on slave
EXTERN struct device devices[MAX_SLAVE_DEVICES];
EXTERN struct device_header far *lpt_drivers[3];   // Points to LPT1, LPT2, LPT3

EXTERN word slave_dos_version;          // DOS version of slave (WORD format)
                                        // The bytes in _osversion are backwords

EXTERN byte drive_priority[26];
EXTERN int num_drives;

/* Buffer for packet information */

EXTERN union {
    byte                    packet_type;
    struct init_packet_r    init_packet_r;
    struct init_packet_a    init_packet_a;
    struct media_check_r    media_check_r;
    struct media_check_a    media_check_a;
    struct build_bpb_r      build_bpb_r;
    struct build_bpb_a      build_bpb_a;
    struct io_r             io_r;
    struct io_a             io_a;
    struct error_r          error_r;
    struct ocrm_r           ocrm_r;
    struct ocrm_a           ocrm_a;
    struct bios_r           bios_r;
    struct bios_a           bios_a;
    struct drive_info_r     drive_info_r;
    struct gen_ioctl_r      gen_ioctl_r;
    struct gen_ioctl_a      gen_ioctl_a;
    struct lpt_o_r          lpt_o_r;
    struct lpt_o_a          lpt_o_a;
    struct lpt_cmd_r        lpt_cmd_r;
    struct lpt_cmd_a        lpt_cmd_a;
    // Note: drive_info_a packet might be too big to put in here.
} packet;

/* Device driver request header packets */

EXTERN union {
    struct static_rhp       s;
    struct init_req         init_req;
    struct init_ans         init_ans;
    struct media_check_req  media_check_req;
    struct media_check_ans  media_check_ans;
    struct build_bpb_req    build_bpb_req;
    struct build_bpb_ans    build_bpb_ans;
    struct io_req           io_req;
    struct io_ans           io_ans;
    struct nd_read_ans      nd_read_ans;
    struct gen_ioctl_req    gen_ioctl_req;
} rhp;


/* Configuration Structure */

#ifdef __INITIALIZERS
    struct config config = {
        BAUD_38400,                         // Default starting baud rate
        "LPT*,COM*\0bcdefghijklmnopqrst",

        // Below is block device configuration information

        DOS_VERSION(1, 0),                  // master DOS version
        0,                                  // Master ID code
        MAX_DEVICES,                        // Masters Max # of block devices
        3                                   // First drive #
    };
#else
    EXTERN struct config config;
#endif

#ifdef __INITIALIZERS
    byte max_baud_server = 0xFF;
#else
    EXTERN byte max_baud_server;
#endif

/* Global Strings */

#ifdef __INITIALIZERS
char block_fingerprint[] = "INTERLNK";
#else
extern char block_fingerprint[];
#endif

EXTERN char far buffer[65536];          // A large buffer for I/O

struct printer {
    word address;
    byte exported;
    byte enabled;
};

EXTERN struct printer printers[3];
EXTERN int num_lpt;

#ifdef __INITIALIZERS
    #undef __INITIALIZERS
#endif


#define S_SCANNING   0
#define S_WAITING    1
#define S_SENDING    2
#define S_RECEIVING  3

#define SDP_INACTIVE    0
#define SDP_READING     1
#define SDP_WRITING     2
#define SDP_PRINTING    3

#define TITLE_COLOR    ( color(WHITE,BLACK)| INTENSE | MONO_BOLD)
#define STATUS_COLOR   ( color(WHITE,CYAN) | INTENSE | MONO_REVERSE)

#define ALT_F4      0x16b

