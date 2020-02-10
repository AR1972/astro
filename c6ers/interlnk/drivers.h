/***
* $Workfile:   drivers.h  $
* $Revision:   1.3  $
*   $Author:   Dave Sewell  $
*     $Date:   08 Aug 1989 16:43:52  $
***/

/*  drivers.h : Alan Butt : December 19, 1988 : Expansion Box Project
 
    This file contains definitions for device driver structures.
*/

#pragma pack(1)

#define INIT                 0
#define MEDIA_CHECK          1
#define BUILD_BPB            2
#define IOCTL_READ           3
#define READ                 4
#define ND_READ              5
#define INPUT_STATUS         6
#define FLUSH_INPUT          7
#define WRITE                8
#define WRITE_VERIFY         9
#define OUTPUT_STATUS       10
#define FLUSH_OUTPUT        11
#define IOCTL_WRITE         12
#define DEVICE_OPEN         13
#define DEVICE_CLOSE        14
#define REMOVABLE_MEDIA     15
#define OUTPUT_UNTIL_BUSY   16
#define GENERIC_IOCTL       19
#define GET_LOGICAL_DEVICE  23
#define SET_LOGICAL_DEVICE  24

#define MAX_VOLUME          12          // Maximum volume label length

/*  For details on the bpb structure see MS-DOS Encyclopedia page 1349, and
    the section on device drivers.
*/

struct bios_parameter_block {
    word    bytes_per_sector;   // Bytes per sector
    byte    sectors_per_unit;   // Sectors per allocation unit (power of 2)
    word    reserved_sectors;   // # reserved sectors (starting at sector 0)
    byte    number_of_fats;     // Number of file allocation tables
    word    root_dir_entries;   // Maximum number of root directory entries
    word    total_sectors;      // Total number of sectors in medium
    byte    media_id_byte;      // Media ID byte
    word    sectors_per_fat;    // # of sectors occupied by a single FAT
    word    sectors_per_track;  // Sectors per track (MS-DOS 3.0+)
    word    number_of_heads;    // Number of heads (MS-DOS 3.0+)
    word    hidden_sectors;     // Number of hidden sectors (MS-DOS 3.0+)
    word    high_hidden;        // MSW hidden sectors (MS-DOS 3.2+)
    dword   long_total_sectors; // If total_sectors == 0, Total number of
                                // sectors in medium (MS-DOS 3.2+)
    byte    reserved[6];
};

#define DEVICE_NAME_SIZE 8

struct device_header {                      // Device header structure
    struct  device_header far *next_header; // Link to next driver
    word    attribute;                      // Device driver attribute word
    word    strategy;                       // Offset to strategy entry point
    word    inter;                          // Offset to interrupt entry point
    byte    name_num[DEVICE_NAME_SIZE];     // Device name or number of units
};


/*  The disk paramenter structure is used by undocumented DOS calls 0x32, 0x52,
    and 0x53.  This structure is defined for MS-DOS 2.0 - MS-DOS 4.0.
*/

struct dpb_dos2 {
    byte drive;                     // Drive # (0 = A, 1 = B, ...)
    byte unit;                      // Unit number within device driver
    word bytes_per_sector;          // Number of bytes per sector
    byte max_cluster;               // Largest sector number in cluster
                                    //  add one for number of sectors/cluster
    byte log2_cluster;              // Log base two of the cluster size
    word reserved_sectors;          // Number of reserved (boot) sectors
    byte number_of_fats;            // Number of copies of the FAT
    word root_dir_entries;          // Number of root directory entries
    word first_data;                // First sector of data on medium
    word max_number;                // Largest possible cluster number
                                    //  subtract one for number of clusters
    byte sectors_per_fat;           // Number of sectors in one FAT copy
    word first_root;                // First sector of root directory
    struct device_header far *device_driver;
                                    // Corresponding device driver address
    byte media_id;                  // Media descriptor byte
    byte valid;                     // 0FF indicates block must be rebuild
    struct dpb_dos2 far *next_block;
                                    // address of next device block in list
    word dir_start;                 // Starting cluster of current directory
                                    //  zero indicates the root directory
    char path_name[64];             // ASCIIZ current directory path string
};

struct dpb_dos3 {
    byte drive;                     // Drive # (0 = A, 1 = B, ...)
    byte unit;                      // Unit number within device driver
    word bytes_per_sector;          // Number of bytes per sector
    byte max_cluster;               // Largest sector number in cluster
                                    //  add one for number of sectors/cluster
    byte log2_cluster;              // Log base two of the cluster size
    word reserved_sectors;          // Number of reserved (boot) sectors
    byte number_of_fats;            // Number of copies of the FAT
    word root_dir_entries;          // Number of root directory entries
    word first_data;                // First sector of data on medium
    word max_number;                // Largest possible cluster number
                                    //  subtract one for number of clusters
    byte sectors_per_fat;           // Number of sectors in one FAT copy
    word first_root;                // First sector of root directory
    struct device_header far *device_driver;
                                    // Corresponding device driver address
    byte media_id;                  // Media descriptor byte
    byte valid;                     // 0FF indicates block must be rebuild
    struct dpb_dos3 far *next_block;
                                    // address of next device block in list
    word unknown_zero;              // Unknown value (usually zero?)
    word unknown_ffff;              // Unknown value (usually ffff?)
};


struct dpb_dos4 {
    byte drive;                     // Drive # (0 = A, 1 = B, ...)
    byte unit;                      // Unit number within device driver
    word bytes_per_sector;          // Number of bytes per sector
    byte max_cluster;               // Largest sector number in cluster
                                    //  add one for number of sectors/cluster
    byte log2_cluster;              // Log base two of the cluster size
    word reserved_sectors;          // Number of reserved (boot) sectors
    byte number_of_fats;            // Number of copies of the FAT
    word root_dir_entries;          // Number of root directory entries
    word first_data;                // First sector of data on medium
    word max_number;                // Largest possible cluster number
                                    //  subtract one for number of clusters
    byte sectors_per_fat;           // Number of sectors in one FAT copy
    byte unknown_byte;              // Unknown byte
    word first_root;                // First sector of root directory
    struct device_header far *device_driver;
                                    // Corresponding device driver address
    byte media_id;                  // Media descriptor byte
    byte valid;                     // 0FF indicates block must be rebuild
    struct dpb_dos4 far *next_block;
                                    // address of next device block in list
    word unknown_zero;              // Unknown value (usually zero?)
    word unknown_ffff;              // Unknown value (usually ffff?)
};

union dpb {
    struct dpb_dos2 dos2;           // Disk parameter block for DOS 2.0+
    struct dpb_dos3 dos3;           // Disk parameter block for DOS 3.0+
    struct dpb_dos4 dos4;           // Disk parameter block for DOS 4.0+
};

/*  Device attribute codes:  ATT_* define the various bits to be used in the
    device driver attribute word of the device header.
*/

#define ATT_CHARACTER   0x8000          // Character device
#define ATT_BLOCK       0x0000          // Block device
#define ATT_IOCTL       0x4000          // IOCTL read and write supported
#define ATT_NON_IBM     0x2000          // Non-IBM format (block)
#define ATT_OUTBUSY     0x2000          // Output Until Busy supported (Char)
#define ATT_OCRM        0x0800          // Open/Close/Removeable media supported*
#define ATT_GEN_IOCTL   0x0040          // Generic IOCTL & Get/Set log dev**
#define ATT_INT29       0x0010          // accepts special interupt 29h
#define ATT_CLOCK       0x0008          // this is the CLOCK device
#define ATT_NUL         0x0004          // Current NUL device
#define ATT_HUGE        0x0002          // 32-bit sector addressing (block)
#define ATT_STDOUT      0x0002          // Current standard output
#define ATT_STDIN       0x0001          // Current standard input
                                        // *  MS-DOS 3.0+
                                        // ** MS-DOS 3.2+

#define STATUS_ERROR    0x8000          // error status
#define STATUS_BUSY     0x0200          // busy status
#define STATUS_DONE     0x0100          // Done

#define ERR_WRITE_PROT  0x00            // write-protect error
#define ERR_UNK_UNIT    0x01            // Unknown unit
#define ERR_NOT_READY   0x02            // Drive not ready
#define ERR_UNK_COMMAND 0x03            // Unknown command
#define ERR_CRC         0x04            // CRC error
#define ERR_BAD_LENGTH  0x05            // Bad drive request structure length
#define ERR_SEEK        0x06            // Seek error
#define ERR_UNK_MEDIA   0x07            // Unknown media
#define ERR_NOT_FOUND   0x08            // Sector not found
#define ERR_PAPER_OUT   0x09            // Printer out of paper
#define ERR_WRITE_FAULT 0x0a            // Write fault
#define ERR_READ_FAULT  0x0b            // Read fault
#define ERR_FAILURE     0x0c            // General Failure
#define ERR_DISK_CHANGE 0x0f            // Invalid disk change


#define MEDIA_CHANGED      -1           // Media changed code
#define MEDIA_DONT_KNOW     0           // Don't know if media has changed
#define MEDIA_NOT_CHANGED   1           // Media has not changed code

#define MEDIA_5_25_DS_15    0xF9        // 5.25" double sided, 15 sector
#define MEDIA_5_25_SS_9     0xFC        // 5.25" single sided, 9 sector
#define MEDIA_5_25_DS_9     0xFD        // 5.25" double sided, 9 sector
#define MEDIA_5_25_SS_8     0xFE        // 5.25" single sided, 8 sector
#define MEDIA_5_25_DS_8     0xFF        // 5.25" double sided, 8 sector
#define MEDIA_3_5_DS_9      0xF9        // 3.5"  double sided, 9 sector
#define MEDIA_FIXED_DISK    0xF8        // Fixed disk
#define MEDIA_3_5_DS_18     0xF0        // 3.5"  double sided, 18 sector

struct static_rhp {                     // Static RHP definition
    byte    rhp_length;                 // Request header length
    byte    rhp_unit;                   // Block-device unit number
    byte    rhp_command;                // Command code (driver subfunction)
    word    rhp_status;                 // Driver return status
    byte    reserved[0x0d - 0x05];
};


struct init_req {                       // initialize request
    struct  static_rhp s;
    byte    reserved[5];
    char far *init_cmd;                 // Segment:Offset of line loading driver
    byte    init_first;                 // First unit number
};


struct  init_ans {                      // Initialization answer
    struct  static_rhp s;
    byte    init_units;                 // Units supported
    void far *init_end;                 // Segment:offset free mem above driver
    void far *init_bpb;                 // Segment:Offset BPB pointer array
};



struct media_check_req {                // Media Check request
    struct  static_rhp s;
    byte    media_id;                   // Media ID byte
};

struct media_check_ans {                // Media check answer
    struct  static_rhp s;
    byte    reserved;
    byte    media_changed;              // Media changed code
    char far *media_label;              // Segment:Offset of volume label
};



struct build_bpb_req {                  // Build BPB request
    struct  static_rhp s;
    byte    media_id;                   // Media ID byte
    char far *bpb_fat;                  // Segment:Offset of FAT buffer
};

struct build_bpb_ans {                  // Build BPB answer
    struct  static_rhp s;
    byte    reserved[5];
    struct bios_parameter_block far *bpb_bpb;
                                        // Segment:Offset of BPB
};



struct io_req {     // Read/Write/Write-Verify/IOCTL-Read/IOCTL-Write request
    struct  static_rhp s;
    byte    media_id;                   // Media ID byte
    char far *io_data;                  // Segment:Offset of data
    word    io_requested;               // Bytes/Sectors Requested
    word    io_start;                   // Starting sector number (LSW)
    word    io_start_high;              // Starting sector number (MSW) (MS-DOS 3.31)
    word    reserved;
    dword   io_huge_start;              // 32-bit sector number (MS-DOS 4.0+)
};

struct io_ans {     // Read/Write/Write-Verify/IOCTL-Read/IOCTL-Write answer
    struct  static_rhp s;
    byte    reserved[5];
    word    io_transfered;              // Bytes/Sectors transfered
    word    reserved2;
    char far *io_label;                 // Segment:Offset of volume label
};


struct nd_read_ans {                    // Non-Destructive read answer
    struct  static_rhp s;
    byte    nd_read_char;               // Character read
};


struct gen_ioctl_req {                  // Generic IOCTL request
    struct  static_rhp s;
    byte    gen_category;               // Category (major) code
    byte    gen_function;               // Function (minor) code
    word    gen_si;                     // SI register contents
    word    gen_di;                     // DI register contents
    void far *gen_data;                 // Segment:Offset of data package
};

struct media_id_buffer {
    word  info_level;
    dword serial_number;
    byte  volume_label[11];
    byte  file_sys_type[8];
};

struct device_params {
   byte   dpSpecFunc;                   // special functions
   byte   dpDevType;                    // device type
   word   dpDevAttr;                    // device attributes
   word   dpCylinders;                  // number of cylinders
   byte   dpMediaType;                  // media type
   struct bios_parameter_block bpb;     // BPB
};

#define MAX_DEVICES         26          // Maximum number of units for the block
                                        // device drivers.

#define MAX_CHAR_DEVICES    20          // Max # of character devices
#define MAX_RHP             32          // Maximum size of an RHP
#define PRODUCT_DOSLINK     0           // Product code
#define PRODUCT_VERSION     (((MAJOR_VERSION << 8) | MINOR_VERSION))
#define STACK_SIZE          256         // Device driver stack size (in bytes)
#define MAX_PRINTERS        3           // Max printers that can be redirected

