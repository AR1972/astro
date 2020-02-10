/***
* $Workfile:   packets.h  $
* $Revision:   1.2  $
*   $Author:   Dave Sewell  $
*     $Date:   04 May 1990  9:18:30  $
***/

/*  packets.h : January 4, 1989 : Alan Butt

    This file contains the structure definitions for the packets used
    in the expansion box device drivers
*/

#define TEST_PACKET_REQ     0           // Data transfer test request
#define INIT_PACKET_REQ     1           // Initialization Information Request
#define MEDIA_CHECK_REQ     2           // Media Check Request
#define BUILD_BPB_REQ       3           // Build BPB Request
#define READ_REQ            4           // Read request
#define WRITE_REQ           5           // Write Request
#define WRITE_VER_REQ       6           // Write-Verify Request
#define ERROR_REQ           7           // Error packet type
#define AUX_INIT_REQ        8
#define AUX_READ_REQ        9           // Auxiliary driver read request
#define AUX_WRITE_REQ       10          // Auxiliary driver write request
#define AUX_WRITE_VER_REQ   11          // Auxiliary driver write verify request
#define ND_READ_REQ         12          // Non-Destructive read request
#define INPUT_STATUS_REQ    13          // Input status request
#define OUTPUT_STATUS_REQ   14          // Output status request
#define FLUSH_INPUT_REQ     15          // Flush Input request
#define FLUSH_OUTPUT_REQ    16          // Flush Output request
#define AUX_DEV_OPEN_REQ    17          // Auxiliary device open request
#define AUX_DEV_CLOSE_REQ   18          // Auxiliary device close request
#define AUX_IOCTL_READ_REQ  19          // Auxiliary device IOCTL READ request
#define AUX_IOCTL_WRITE_REQ 20          // Auxiliary device IOCTL WRITE request
#define OUTPUT_TIL_BUSY_REQ 21          // Output until busy request
#define DEV_OPEN_REQ        22          // Device Open request
#define DEV_CLOSE_REQ       23          // Device Close requset
#define REMOVABLE_MEDIA_REQ 24          // Removable media request

// NOTE: The BIOS print command code should be in the same numerical
// order as the INT 17H BIOS codes for easy conversion.

#define BIOS_PRINT_REQ      25          // Auxiliary BIOS print character
#define BIOS_INIT_REQ       26          // Auxiliary BIOS init printer
#define BIOS_STATUS_REQ     27          // Auxiliary BIOS get printer status
#define DRIVE_INFO_REQ      28          // Get drive info.
#define GEN_IOCTL_REQ       29          // Generic IOCTL
#define PACKET_TYPES        30          // Number of packet types

#define UNASSIGNED          0xFF
#define DONT_CARE           0xFE


struct init_packet_r {                  // Init packet request
    byte    packet_type;                // Packet type
    byte    ipr_major_version;          // Client program major version number
    byte    ipr_minor_version;          // Client program minor version number
    word    ipr_dos_version;            // Client DOS version number
    byte    ipr_max_devices;            // Maximum devices master can accept
    byte    ipr_first_unit;             // First drive letter on master
    // IMPORTANT NOTE: ipr_prn_map must immediately precede ipr_mapping because
    // a block move is used to load up the values.
    byte    ipr_prn_map[3];             // Requested printer mapping
    byte    ipr_mapping[MAX_DEVICES];   // Master's desired mappings
};

struct init_packet_a {                  // Init packet answer
    byte    ipa_major_version;          // Server program version
    byte    ipa_minor_version;          // Server program version
    word    ipa_dos_version;            // Server DOS version
    byte    ipa_devices;                // Number of logical devices on system
    // IMPORTANT NOTE: ipa_prn_map must immediately precede ipa_mapping because
    // a block move is used to copy the values.
    byte    ipa_prn_map[3];             // Actual mapping of printer ports
    byte    ipa_mapping[MAX_DEVICES];   // Mapping for driver mapping table
    word    ipa_attributes[MAX_DEVICES];// Attributes for block device drivers
    byte    ipa_multitasker;
};



struct media_check_r {                  // Media check request
    byte    packet_type;                // Packet type
    byte    mcr_unit;                   // Unit number with device driver
    byte    mcr_media_id;               // MS-DOSs notion of media id byte
};

struct media_check_a {                  // Media check answer
    word    mca_status;                 // Driver status word
    byte    mca_changed;                // Media Changed code
    byte    mca_volume[MAX_VOLUME];
    unsigned long mca_serial_number;
};



struct build_bpb_r {                    // Build BPB request
    byte    packet_type;                // Packet type
    byte    bbr_unit;                   // Unit Number within device driver;
    byte    bbr_media_id;               // MS-DOSs notion of the media id byte
};

struct build_bpb_a {                    // Build BPB answer
    word    bba_status;                 // Device driver status word
    struct  bios_parameter_block bba_bpb;
};


struct io_r {       // Read/Write/Write-Verify/IOCTL-Read/IOCTL-Write request
    byte    packet_type;                // Packet type
    byte    ior_unit;                   // Unit number within device driver
    byte    ior_media_id;               // MS-DOSs notion of the media ID Byte
    word    ior_requested;              // Bytes/Sectors requested
    word    ior_start;                  // Starting sector number
    dword   ior_huge_start;             // 32-bit starting sector number
};

struct io_a {       // Read/Write/Write-Verify/IOCTL-Read/IOCTL-Write answer
    word    ioa_status;                 // Device driver status word
    word    ioa_transfered;             // Bytes/Sectors transferred
    byte    ioa_volume[MAX_VOLUME];
    unsigned long ioa_serial_number;
};

struct error_r {                        // Error packet
    byte    packet_type;                // Packet type
    byte    err_block_dvr;              // True if block driver
    byte    err_unit;                   // Unit number or Id code
    byte    err_code;                   // Error code
    byte    err_data;                   // Additional information byte
};
                                  
struct ocrm_r {                         // Open/Close/Removable media requests
    byte    packet_type;                // Packet type
    byte    ocr_unit;                   // Unit number
};

struct ocrm_a {                         // Open/Close/removable media answers
    word    oca_status;                 // Device driver status word
};

struct gen_ioctl_r {
    byte    packet_type;
    byte    gir_unit;
    byte    gir_category;
    byte    gir_function;
};

struct gen_ioctl_a {
    word    gia_status;
};



struct bios_r {                         // Auxiliary BIOS request (INT 17H) requests
    byte    packet_type;                // Packet type
    byte    bsr_bios_port;              // Bios Port num + (first_printer << 4)
    byte    bsr_char;                   // Character to print
};

struct bios_a {                         // Auxiliary BIOS request (INT 17H) answers
    byte    bsa_status;                 // Return status
};

struct drive_info_r {
    byte    packet_type;                // Packet type
    byte    server_drive_num;           // 0 = A, 1 = B, etc.
};


struct drive_info_a {
    char    size[10];
    char    volume_label[12];
    byte    write_protected;
};

struct lpt_o_r {  // LPT driver Write/Write-Verify/IOCTL-Write request
    byte    packet_type;                // Packet type
    byte    lpt_id;                     // 0 = LPT1, 1 = LPT2, 2 = LPT3
    word    print_count;                // Number of bytes to print
    byte    print_data;                 // Data byte if print_count == 1
};

struct lpt_o_a {  // LPT driver Write/Write-Verify/IOCTL-Write answer
    word    lpt_status;                 // Device driver status word
    word    lpt_transferred;            // Number of bytes transferred
};

struct lpt_cmd_r {    // Output status/Output flush requests
    byte    packet_type;                // Packet type
    byte    lpt_id;                     // 0 = LPT1, 1 = LPT2, 2 = LPT3
};

struct lpt_cmd_a {    // Output status/Output flush answer
    word    lca_status;                 // Device driver status word
};

// Below are the definitions of the error codes for ERROR_REQ

#define ER_BAD_MEDIA    0               // Unusable media error
