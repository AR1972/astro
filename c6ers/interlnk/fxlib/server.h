#define MAX_DOS_PATH    67

/*** Packet types. ***/

/*** 
* Packet types 0 - 127 are reserved for packets which may be supported
* by multiple developers.  These types must be assigned by Sewell Development.
* Packet types from 0 to MAX_REQUIRED_PACKET_TYPE must be
* supported by all servers.  Packet types from MAX_REQUIRED_PACKET_TYPE + 1 to
* Each developer will be assigned a range of 128 packet types for their
* private use.  Developers should not use private packet types outside of
* their assigned range.
*
* Packet types 128 - 255 are reserved for Sewell Development.
* Packet types 256 - 383 are reserved for Symantec.
***/

#define SERVER_INFO_REQ         0
#define FILE_XFER_REQ           1
#define CHDDIR_REQ              2
#define DDIR_INFO_REQ           3
#define DISK_FREE_REQ           4
#define DIRLIST_REQ             5
#define FILE_RECV_REQ           6
#define FCB_DELETE_REQ          7
#define FILE_DELETE_REQ         8
#define FILE_ATTRIB_REQ         9
#define RMDIR_REQ               10
#define RENAME_REQ              11
#define DISK_INFO_REQ           12
#define FILE_INFO_REQ           13
#define ECHO_REQ                14
#define MKDIR_REQ               15
#define MAX_REQUIRED_PACKET_TYPE    15
#define FILE_OPEN_REQ       122
#define FILE_READ_REQ       123
#define FILE_CLOSE_REQ      124
#define SERVER_EXIT_REQ     125
#define SET_TIMEOUT_REQ     126
#define SWITCH_REQ          127

#define MAX_COMMON_PACKET_TYPE  127

/*** Packet structures. ***/

/*** NOTE: os_type values must be assigned by Sewell Development. ***/
enum os_type {                  // Operating system type.
    OS_MSDOS   = 0,
    OS_OS2     = 1,
    OS_NETWARE = 2
};

/*** NOTE: developer values must be assigned by Sewell Development. ***/
enum developer {
    DEV_SEWELL   = 0,
    DEV_SYMANTEC = 1
};

struct capabilities {
    byte checksum;                  // TRUE if checksum supported/desired
    byte crc;                       // TRUE if CRC desired
    word max_serial_block;          // Maximum serial block size
    byte reserved[12];              // Reserved for expansion, should be zero.
};

/*** 
* NOTE: the product and version numbers in the structures below are for the
* private use of each developer.  In general, these numbers should be ignored
* unless the client and server program are by the same developer.
***/

struct server_info_req {
    enum os_type os_type;
    enum developer developer;
    word product;                       // Developer assigned product ID code.
    word version;                       // Developer assigned version number.
    byte device_driver;                 // TRUE if client is a device driver.
    byte want_packets_supported;        // TRUE if packets_supported info desired
    struct capabilities client_caps;    // Client capabilities
    long client_id;                     // 32-bit client ID
    long last_server_id;                // Last server ID received by client
};

struct server_info_ans {
    enum os_type os_type;
    enum developer developer;
    word product;                       // Developer assigned product ID code.
    word version;                       // Developer assigned version number.
    byte device_server;                 // TRUE if device server functions supported
    struct capabilities agreed_caps;    // Agreed upon capabilities
    long last_client_id;                // Last client ID received by server
    long server_id;                     // 32-bit server ID
    // NOTE: the packets_supported field should not be sent if the client
    // want_packets_supported is FALSE.  Also, if a server does not support
    // these functions (for example a minimal device driver server), it does
    // not need to return this field, even if it has been requested.
    // Therefore, the client should look at the packet length to see if this
    // field is present.
    byte packets_supported[MAX_COMMON_PACKET_TYPE + 1];
};

struct switch_ans {
    byte wants_client;      // TRUE if client/server switch desired by server
};                          // Server would normally return FALSE

#define XFER_CREATE_FILE  1
#define XFER_OPEN_FILE    2
#define XFER_CHDIR        3
#define XFER_EXIT         4

/***
* file_xfer_req packet is structured as a series of commands prefixed by
* a one byte command code:
*
*   XFER_CREATE_FILE - Create a file which may or may not exist on server.
*                    Followed by a dir_entry structure.
*                    Followed by dir_entry.size bytes of data for the file
*                    which may be spread across multiple FILE_SEND_REQ packets.
*   XFER_OPEN_FILE   - Open a file which is known to exist on the server.
*                    Followed by same info as FX_CREATE_FILE.
*   XFER_CHDIR       - Change to indicated directory.
*                    Followed by a null terminated string containing directory
*                    name.
*   XFER_EXIT      - Return from file transfer.
*   (NOTE)         - A FILE_SEND_REQ packets may begin with data still expected
*                    to fulfill a previous FX_CREATE_FILE or FX_OPEN_FILE 
*                    command.
***/

enum file_xfer_error {
    XFER_OK           = 0,
    XFER_CREATE_ERROR = 1,
    XFER_OPEN_ERROR   = 2,
    XFER_WRITE_ERROR  = 3,
    XFER_FULL_ERROR   = 4,
    XFER_CHDIR_ERROR  = 5
};

/***
* NOTE: If there were no errors in processing the FILE_XFER_REQ packet, the
* server should send back a word size FILE_XFER_ANS packet with file_send_error 
* set to XFER_OK.  Otherwise, the file or directory name (null-terminated) 
* should be sent back along with the appropriate file_send_error code and DOS 
* error code.
***/

struct file_xfer_ans {
    enum file_send_error error;
    short errno;
    char  pathname[MAX_DOS_PATH];
};

struct chddir_req {
    byte create;            // TRUE = create if directory does not exist.
    char pathname[MAX_DOS_PATH];
};

struct chddir_ans {
    short errno;
};

struct ddir_info_ans {
    char pathname[MAX_DOS_PATH];    // Current drive and directory of server
};

typedef unsigned long FILESIZE;
typedef unsigned long FILEDATE;

struct disk_free_ans {                      // Used in DISK_FREE_ANS packet
    unsigned short bytes_per_cluster;
    FILESIZE bytes_free;
};

/***
* A FILE_INFO_REQ can only be used to obtain the dir_entry structure for a file
* in the server's current directory.  The pathname is a null terminated string.
***/

struct file_info_req {
    char pathname[MAX_DOS_PATH];
};

/***
* The FILE_INFO_ANS will have exists set to TRUE if the file exists in the
* server's current directory.  Otherwise, exists will be FALSE and dir_entry
* will be undefined.
***/

struct file_info_ans {
    int exists;
    struct dir_entry dir_entry;
};


struct mkdir_req {
    char pathname[MAX_DOS_PATH];
};

struct mkdir_ans {
    short errno;
};

#define MIN_FILEDATE    0LU
#define MAX_FILEDATE    0xFFFFFFFFLU

#define MAX_FILE_LIST_SIZE   512

struct dirlist_req {
    byte attrib;                                // Attributes to search for.
    FILEDATE min_date;
    FILEDATE max_date;
    char file_list[MAX_FILE_LIST_SIZE];
};

/***
* A DIRLIST_REQ can only be used to request a directory list in the current
* directory.  The filespec_list is a list of filespecs to include in the
* directory search, followed by a list of filespecs to exclude.  Each 
* filespec is null-terminated.  The list of include filespecs is terminated
* by a null filespec as is the list of exclude filespecs.  In other words,
* there are two null bytes to mark the end of each list.  All alphabetic
* characters in either list must be in upper-case.  Filespecs must either
* be pure root names (possibly including wildcards), or must be full path
* names (without drive letter prefixes).  If a full path name is specified,
* the '\' character must be used as the path separator (instead of '/').
* All root name specs are checked during the lookup.  Full path name specs
* are only check if the directory prefix of the path name matches the
* current working directory.
*
* Example filespec list: 
*   "*.bak", 0 "*.old", 0 "myfile.doc", 0 "\wp\*.doc", 0 "\wp\learn", 0, 0
*
* In the example above, all files matching the specification "*.bak",
* "*.old", or "myfile.doc" will be included. Files matching the specification 
* "*.doc" will only if the current directory happens to "\wp".  A file or
* directory named "learn" will be included only if the directory being 
* searched is the "\wp" directory.
***/

struct dirlist_ans {
    word not_included_files;    // Files skipped because they were not included
    word excluded_files;        // Files excluded by exclude list.
    word excluded_dirs;         // Directories excluded by exclude list.
    word min_cutoff_files;      // Files excluded by minimum cutoff time.
    word max_cutoff_files;      // Files excluded by maximum cutoff time.
};
/***
* NOTE: dirlist_ans packet consists of the above structure followed by an
* array of dir_entry structures.
***/

/***
* file_recv_req packet is structured as a series of commands prefixed by a
* one byte command code:
*       
*   XFER_CREATE_FILE - Create a file which may or may not exist on client.
*                      Followed by a dir_entry structure.
*   XFER_OPEN_FILE   - Open a file which is known to exist on the client.
*                      Followed by a dir_entry structure.
*   XFER_CHDIR       - Change to indicated directory.
*                      Followed by a null terminated string containing directory
*                      name.
*   XFER_EXIT        - Will be returned in sequence to terminate the exchange.
*
* NOTE: after receiving this packet the server should build a series of
* FILE_XFER_REQ packets and send them to the client.  The client responds to
* each with a FILE_XFER_ANS packet.
***/

struct server_exit_req {
    byte dos_exit_code;
};


struct fcb_delete_req {
    byte attrib;        // Allowable attributes for files to be deleted
    char filespec[13];  // File spec (allows wildcards)
};

struct fcb_delete_ans {
    short result;       // 0 = OK, -1 = no files or some read-onlys not deleted
};

struct file_delete_req {
    char filespec[MAX_DOS_PATH];       // Does DOS function 41H delete
};

struct file_delete_ans {
    short errno;
};

struct file_attrib_req {
    byte set_attrib;            // 1 = set attributes, 0 = get attributes
    word attrib;                // attribute if setting
    char filespec[MAX_DOS_PATH];
};

struct file_attrib_ans {
    short errno;
    word  attrib;
};

struct rmdir_req {
    char pathname[MAX_DOS_PATH];
};

struct rmdir_ans {
    short errno;
};

struct rename_req {
    char current_name[MAX_DOS_PATH];
    char new_name[MAX_DOS_PATH];
};

struct rename_ans {
    short errno;
};

struct disk_info_req {                       // Used for DISK_INFO_REQ
    struct disk_free_ans diskfree;
    char   cwd[MAX_DOS_PATH];
};

/***
* NOTE: SERVER_WAIT_REQ is a command packet which a client that normally
* transmits IDLE_REQ's in order to maintain the connection may send to inform
* the server that it will be temporarily unable to transmit IDLE_REQ's.  The
* server should then disable it's timeouts until it again receives an IDLE_REQ.
***/

struct file_open_req {
    byte access_mode;
    char filespec[MAX_DOS_PATH];
};

struct file_open_ans {
    short errno;
    word  handle;
};

struct file_read_req {
    word handle;
    long offset;
    word count;
};

struct file_read_ans {
    short errno;
    word  count;            // Number of bytes actually read.
    byte  data[1];          // Data from read
};

struct file_close_req {
    word handle;
};

struct file_close_ans {
    short errno;
};

struct set_timeout_req {
    word timeout;
};
