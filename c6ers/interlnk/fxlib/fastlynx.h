#ifndef TRUE
#define TRUE    1
#endif
#ifndef FALSE
#define FALSE   0
#endif

#define MAX_PORTS 8

#pragma pack(1)

typedef unsigned char byte;
typedef unsigned int  word;

#define SERIAL_PORT         0
#define PARALLEL_PORT       1

#define  PARALLEL_NORMAL   0
#define  PARALLEL_TURBO    1

const int _near fx_port;            // -1 if not connected, else current port index.
const byte _near fx_baud;           // Current serial baud rate (index to baud_table)
const byte _near fx_serial_7_wire;
const byte _near fx_send_variable;
const byte _near fx_recv_variable;
const byte _near fx_parallel_speed; // Current parallel operating speed
const byte fx_max_serial_baud;
word fx_max_serial_block;
byte fx_error_checking_mode;     // Set to ECM_ALLOW_CHECKSUM or ECM_FORCE_CRC
byte fx_force_variable;

#ifdef _PAR18
#define  PARALLEL_11_WIRE        0
#define  PARALLEL_18_WIRE        1
#define  PARALLEL_BIDIRECTIONAL  2

const byte _near fx_parallel_mode;  // Indicates current parallel operating mode.
byte _near fx_disable_par18;        // Set to TRUE to disable parallel 18 wire
#endif

enum fx_errno {
    FX_ERR_TIMEOUT = -1,         // Timeout during initial packet negotiation
    FX_ERR_CRC     = -2,         // Packet completed, but got a CRC / Checksum error
    FX_ERR_FAIL    = -3          // Packet failed to complete
};

const enum fx_errno _near fx_errno;   // Set only on failure of FxSend or FxReceive

#define BAUD_1200       0
#define BAUD_2400       1
#define BAUD_4800       2
#define BAUD_9600       3
#define BAUD_19200      4
#define BAUD_38400      5
#define BAUD_57600      6
#define BAUD_115200     7

#ifdef FX_MAIN
char *baud_table[] = {
    "1200  ",
    "2400  ",
    "4800  ",
    "9600  ",
    "19200 ",
    "38400 ",
    "57600 ",
    "115200"
};
#else
extern char *baud_table[];
#endif

int _near fx_num_ports;               // Number of ports in FxPortInfo array.

struct FxBiosInfo {
    byte num_serial;
    byte num_parallel;
    word serial_address[4];
    word parallel_address[3];
};

#define PF_BAD_PORT     0x80

struct FxPortInfo {
    byte type;
    byte biosnum;
    word address;
    const byte flags;   // Set to PF_BAD_PORT by FastLynxInit if hardware error.
};

struct FxPortInfo _near FxPortInfo[MAX_PORTS];

#define ECM_FORCE_CRC         0
#define ECM_ALLOW_CHECKSUM    8 // Note: must correpond to internal flag bit.

#define MAX_SERIAL_BLOCK    8 * 1024

struct FxSettings {
    word time_out;  // Timeout value in ticks for FxConnect() and FxListen().
    byte error_checking_mode;   // ECM_CRC or ECM_CHECKSUM
    word max_serial_block;
    byte max_serial_baud;
    word allow_7_wire;          // TRUE to enable 7 wire
};

extern struct FxSettings _near FxSettings;

typedef unsigned long FILESIZE;
typedef unsigned long FILEDATE;

struct dir_entry {
    byte attrib;        /* Attribute of matching file        */
    FILEDATE date;      /* Time stamp of file                */
    FILESIZE size;      /* Size of file in bytes             */
    char name[13];      /* Name of matching file             */
};

/*** Bootstrap loader command codes. ***/

#define CLONE_CREATE_CMD  1
#define CLONE_WRITE_CMD   2
#define CLONE_CLOSE_CMD   3
#define CLONE_EXIT_CMD    4
#define CLONE_UNKNOWN_CMD 5

/*** Structure for packets sent to clone bootstrap loader. ***/

#define MAX_CLONE_DATA      2048

struct clone_cmd {
    byte command;
    char ok_msg[80];
    char error_msg[80];
    struct dir_entry dir;
    unsigned cnt;           // Number of bytes of data to write.
    byte   data[MAX_CLONE_DATA];
};

/*** Function prototypes. ***/

void _far _fastcall FxQueryBios(struct FxBiosInfo _near *bios_info);

void _far _pascal FxInit(void); 
// Call once during program init.

void _far _pascal FxExit(void);       // MUST be called before exiting!

int  _far _pascal FxConnect(void);
// FxConnect returns either:
//  Success (1) -  Connected
//  Failure (-1) - Timed out - can retry on next call
//  Continue (0) - Will continue on next call

int  _far _pascal FxListen(void);

// FxStartIdle should be called to restart the idler after a call to FxSend or
// FxReceive, both of which stop the idler.
void _far _pascal FxStartIdle(void);

void _far _pascal FxDisconnect(void);

#define CHECK_ABORT_WAITING     0
#define CHECK_ABORT_DONE        1

void _far _fastcall FxSetCheckAbort(int (_far _fastcall *CheckAbort)(word elapsed_ticks, byte status));

#define SYNC_NO_TIMEOUT         0xFFFF
#define SYNC_ONE_SECOND             18

void _far _fastcall FxSyncTimeout(word timeout);

int _far _fastcall FxSendWord(word command);
int _far _fastcall FxSend(void const _far * buf, word length);
word _far _fastcall FxReceive(void _far *buf, word max_len);
void _far _pascal FxShowBaud (void (_far _pascal *ShowBaud)(void));

int _far _fastcall FxCloneInit(int port_index, int remote_port, char const _near *loading_bootstrap_msg, int (_far _cdecl *CheckAbort)(void));
int _far _fastcall FxBootstrapInit(void);
int _far _fastcall FxSendBootstrap(int num_bytes);
int _far _pascal FxBootstrapVerify(void);
int _far _pascal FxStartBootstrap(void);
int _far _fastcall FxSendSerialBlock(void const _far * buf, word length);
int _far _pascal FxCloneExit(void);
int _far _fastcall FxSetBaud(byte BaudIndex, word PortAddress);
#pragma pack()
