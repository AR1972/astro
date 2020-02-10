/***
* $Workfile:   main.c  $
* $Revision:   1.13  $
*   $Author:   Dave Sewell  $
*     $Date:   22 Oct 1990 14:57:46  $
***/

/*  main.c : Alan Butt : February 1, 1988 : Expansion Box Project

    This module contains main() and the packet dispatcher

*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <bios.h>
#define  FX_MAIN    1
#include "fastlynx.h"
#include <conio.h>

#define EXTERN                          // Make declarations real!
#include "dc.h"

#define DIVIDER_ROW 10
#define BOX_TOP     4
#define MSG_LINES   3

#define REPAINT_KEY -2

extern byte dos5_switcher;

char *clone(char *argp);

PIFW packet_handler[] = {
    server_info_handler,                // Server Info Request handler
    init_handler,                       // Initialize Request
    media_check_handler,                // Media Check Request
    build_bpb_handler,                  // Build BPB Request
    read_handler,                       // Read Request
    write_handler,                      // Write Request
    write_handler,                      // Write-Verify Request
    error_handler,                      // Error packet
    unknown_handler,                    // Aux device Init Request
    unknown_handler,                    // Aux device Read Request
    prn_write_handler,                  // Aux device write Request
    prn_write_handler,                  // Aux device Write-Verify Request
    unknown_handler,                    // Non-destructive read request
    unknown_handler,                    // Input status request
    prn_cmd_handler,                    // Output status request
    unknown_handler,                    // Flush Input request
    prn_cmd_handler,                    // Flush Output request
    prn_cmd_handler,                    // Auxiliary Device Open request
    prn_cmd_handler,                    // Auxiliary Device Close request
    unknown_handler,                    // Auxiliary IOCTL Read request
    prn_write_handler,                  // Auxiliary IOCTL Write request
    prn_write_handler,                  // Output until busy request
    ocrm_handler,                       // Device Open request
    ocrm_handler,                       // Device Close request
    ocrm_handler,                       // Removable Media request
    bios_handler,                       // BIOS INT 17H Print handler
    bios_handler,                       // BIOS INT 17H Init handler
    bios_handler,                       // BIOS INT 17H Status handler
    drive_info_handler,                 // Drive info request handler.
    gen_ioctl_handler,                  // Generic IOCTL handler
};

// WARNING: The array below MUST correspond to packet_handler[] above.

static byte packet_types[] = {
    SDP_INACTIVE,                   // Unknown Packet type handler
    SDP_INACTIVE,                   // Initialize Request
    SDP_READING,                    // Media Check Request
    SDP_READING,                    // Build BPB Request
    SDP_READING,                    // Read Request
    SDP_WRITING,                    // Write Request
    SDP_WRITING,                    // Write-Verify Request
    SDP_INACTIVE,                   // Error packet
    SDP_INACTIVE,                   // Aux device Init Request
    SDP_INACTIVE,                   // Aux device Read Request
    SDP_PRINTING,                   // Aux device write Request
    SDP_PRINTING,                   // Aux device Write-Verify Request
    SDP_INACTIVE,                   // Non-destructive read request
    SDP_INACTIVE,                   // Input status request
    SDP_INACTIVE,                   // Output status request
    SDP_INACTIVE,                   // Flush Input request
    SDP_INACTIVE,                   // Flush Output request
    SDP_INACTIVE,                   // Auxiliary Device Open request
    SDP_INACTIVE,                   // Auxiliary Device Close request
    SDP_INACTIVE,                   // Auxiliary IOCTL Read request
    SDP_INACTIVE,                   // Auxiliary IOCTL Write request
    SDP_PRINTING,                   // Output until busy request
    SDP_READING,                    // Device Open request
    SDP_READING,                    // Device Close request
    SDP_READING,                    // Removable Media request
    SDP_PRINTING,                   // BIOS INT 17H Print handler
    SDP_PRINTING,                   // BIOS INT 17H Init handler
    SDP_PRINTING,                   // BIOS INT 17H Status handler
    SDP_INACTIVE,                   // Install info request handler.
    SDP_INACTIVE                    // Generic IOCTL handler
};

struct FxBiosInfo Bios;

char *final_msg = 0;

int drdos = FALSE;

byte win386_enh_mode = FALSE;       // Used in communication routines also
static byte int_2f_ok = FALSE;

int status_width;

byte check_abort_flag = FALSE;

#define LOC_WIDTH   12
#define EQ_WIDTH     7
#define REM_WIDTH   14

static int pascal auto_serial(void);

static byte included_drives[26];
static byte excluded_drives[26];
static int num_includes;

static int last_port = -1;              // Keeps track of last used port
static int current_serial_port;
static byte excluding = FALSE;

#define LEFT_MARGIN     4

static byte display_left_col = 20 + LEFT_MARGIN;      // Default to non-split-screen value
static byte display_right_col = 40 + LEFT_MARGIN;
static byte display_rows;
static byte split_screen = FALSE;
static int  top_row;

static byte port_column;
static byte status_column;
static byte speed_column;
static int  speed_size;

static int current_drive = -1;
static int current_printer = -1;

static char reading_arrow[] = "อออ";
static char writing_arrow[] = "อออ";
static char spaces[]        = "    ";

static byte screen_initialized = FALSE;

#if 0
void _far _cdecl dprintf(char *fmt, ...)
{
    va_list arg_ptr;
    char msg_buff[512];
    unsigned n;
    static char press_any[] = "\r\nPress any key\r\n";

    va_start(arg_ptr, fmt);
    vsprintf(msg_buff, fmt, arg_ptr);
    _dos_write(1, (void _far *) msg_buff, strlen(msg_buff), &n);
    _dos_write(1, (void _far *) press_any, strlen(press_any), &n);
    read_key();
}
#endif

void cdecl warn_printf(char *fmt, ...)
{
    va_list arg_ptr;
    char msg_buff[512];
    int c;

    do_init_screen();
    va_start(arg_ptr, fmt);
    vsprintf(msg_buff, fmt, arg_ptr);
    strcat(msg_buff, press_enter_to_continue);
    draw_screen(msg_buff, 0, WARNING_COLOR);
    for (;;) {
        c = read_key();
        if (c == F3 || c == ALT_F4 || c == CTRL_C)
            exit(2);
        else if (c == '\r') {
            fill_attr(coord(1, 0), boxsize(scr_rows - 2, 80), ' ', SCROLL_COLOR);
            return;
        }
    }
}

char *port_name(int port_index)
{
    static char port_buff[11];

    if (FxPortInfo[port_index].type == PARALLEL_PORT) {
        if (FxPortInfo[port_index].biosnum)
            sprintf(port_buff, "LPT%d", FxPortInfo[port_index].biosnum);
        else
            sprintf(port_buff, "LPT(%X)", FxPortInfo[port_index].address);
    }
    else {
        if (FxPortInfo[port_index].biosnum)
            sprintf(port_buff, "COM%d", FxPortInfo[port_index].biosnum);
        else
            sprintf(port_buff, "COM(%X)", FxPortInfo[port_index].address);
    }
    return port_buff;
}

static void near check_drdos(void)
{
    char *env;

    if ((env = getenv("OS")) && strcmpi(env, "DRDOS") == 0)
        drdos = TRUE;   // must be running DRDOS
    else
        drdos = FALSE;
}

static int port_coord(void)
{
    return ( coord(scr_rows - 1, port_column) );
}

static int status_coord(void)
{
    return ( coord(scr_rows - 1, status_column) );
}

static int speed_coord(void)
{
    return ( coord(scr_rows - 1, speed_column) );
}

static int drive_coord(int drive_num, int is_client)
{
    int col = display_left_col;
    int row = drive_num;

    if ((byte) drive_num >= display_rows) {
        col = display_right_col;
        row -= display_rows;
    }
    return ( coord(top_row + 4 + row, col) + (is_client ? 20 : 0) );
}

static int printer_coord(int printer_num, int is_client)
{
    return drive_coord(num_drives + printer_num, is_client);
}

static void _far _pascal show_speed(void)
{
    if (fx_port == -1) {
        fill_attr(speed_coord(), boxsize(1, speed_size), ' ', STATUS_COLOR);
    }
    else {
        if (FxPortInfo[fx_port].type == SERIAL_PORT) {
            justify_str( speed_coord(), speed_size, STATUS_COLOR,
                (fx_send_variable || fx_recv_variable) ?
                variable_serial_speed : baud_table[fx_baud], -1);
        }
        else {
            justify_str( speed_coord(), speed_size, STATUS_COLOR,
                fx_parallel_speed == PARALLEL_NORMAL ?
                parallel_normal : parallel_turbo, -1);
        }
    }
}

static void show_port(void)
{
    char buff[40];

    if (fx_port != last_port) {
        sprintf(buff, fx_port == -1 ? "" : "%s:", port_name(fx_port) );
        justify_str( port_coord(), 10, STATUS_COLOR, buff, -1);
        last_port = fx_port;
    }
    show_speed();
}

static void show_drive_state(int status)
{
    if (current_drive != -1) {
        set_attribute(LIST_NORMAL);
        dispchar(drive_coord(current_drive, FALSE) - 2,
            (status == SDP_READING || status == SDP_WRITING) ? '*' : ' ');
    }
}

static void _fastcall show_prn_state(int status)
{
    if (current_printer != -1) {
        set_attribute(LIST_NORMAL);
        dispchar(printer_coord(current_printer, FALSE) - 2,
            status == SDP_PRINTING  ? '*' : ' ');
    }
}

static int _fastcall drive_slot(byte slave_device)
{
    byte *ptr = memchr(drive_priority, slave_device, num_drives);

    if (ptr)
        return ptr - drive_priority;
    else
        return -1;          // Error!
}

void _fastcall show_dp_status(int status)
{
    static int last_paint = SDP_INACTIVE;
    static char *msgs[] = { idle, reading, writing, printing };

    show_drive_state(status);
    show_prn_state(status);
    if (status == last_paint)              // no need to re-paint
        return;
    justify_str(status_coord(), status_width, STATUS_COLOR, msgs[status], -1);
    last_paint = status;
}

#pragma optimize("sleazrg", off)

static void desqview_api(void)
{
    _asm {
        push di 
		mov	ax, 101AH
		int	15H
		mov	ax, bx
		int	15H
		mov	ax, 1025H
		int	15H
        pop di
    }
}

static void enter_critical(void)
{
    _asm {
        mov     ax, 352FH
        int     21H
        mov     ax, es
        or      ax, bx
        jz      enter_done

        inc     int_2f_ok       ; OK to use INT 2FH since vector is not zero
        mov     ax, 1600H
        int     2FH
        test    al, 7FH
        jz      enter_done

        cmp     al, 1
        je      enter_done

        cmp     al, -1
        je      enter_done

        inc     win386_enh_mode
        xor     di, di
        mov     es, di
        mov     bx, 0015H       ; Device ID of DOSMGR device
        mov     ax, 1684H       ; Get API entry point
        int     2FH
        mov     ax, es
        or      ax, di
        jz      crit

        push    cs              ; Push return segment
        mov     ax, OFFSET crit
        push    ax              ; Push return offset
        push    es
        push    di              ; API far call address
        mov     ax, 1           ; SetFocus function number
        retf

crit:   mov     ax, 1681H       ; Enter critical section
        int     2FH
    }
enter_done:;
}

static void _cdecl leave_critical(void)
{
    if (win386_enh_mode) {
        _asm {
                mov     ax, 1682H
                int     2FH
        }
    }
}

static void release_time_slice(void)
{
    if (int_2f_ok) {
        _asm {
                mov     ax, 1680H
                int     2FH
        }
    }
}

static void stop_desqview_multitasking(void)
{
    _asm {
        push di
        mov  bx, 101BH
        call desqview_api           ; Begin critical section
        mov  bx, 1C0DH              ; <Enter>
        mov  ax, 1110H
        int  15H
        mov  bx, 314EH              ; 'N'    
        mov  ax, 1110H
        int  15H
        mov  bx, 1454H              ; 'T'    
        mov  ax, 1110H
        int  15H
        mov  bx, 1352H              ; 'R'    
        mov  ax, 1110H
        int  15H
        mov  bx, 0FC00H             ; Special "Alt" code
        mov  ax, 1110H
        int  15H
        mov  bx, 101CH
        call desqview_api           ; End critical section
        pop  di
    }
}

#pragma optimize("sleazrg", on)

static void attempt_transaction(void)
{
    word count;
    int ptype;
    int request_type;
    int result;
    static byte connected = FALSE;

    if (!connected) {
        if ( FxListen() == 1 ) {
            connected = TRUE;
            show_port();
            FxShowBaud(show_speed);
        }
        else {
            release_time_slice();
            return;
        }
    }
#if 0
    // Special debug code to test midnight wrap around.
    // If time is later than 12:01 am, reset to 11:59 pm.
    {
        union REGS regs;

        regs.h.ah = 0x2C;
        intdos(&regs, &regs);
        if (regs.h.ch != 23 || regs.h.cl < 59) {
            regs.h.ah = 0x2D;
            regs.h.ch = 23;
            regs.h.cl = 59;
            regs.h.dh = 0;
            regs.h.dl = 0;
            intdos(&regs,&regs);
        }
    }
#endif
    count = FxReceive(&packet, sizeof(packet));
    if (count == 0) {
        connected = FALSE;
        show_port();
        return;
    }

    memset(&rhp, '\0', sizeof(rhp));    // Insure RHP all clear

    ptype =  packet.packet_type;
    current_drive = -1;
    current_printer = -1;
    if (ptype >= 0 && ptype < PACKET_TYPES) {
        request_type = packet_types[ptype];
        if (request_type == SDP_READING || request_type == SDP_WRITING)
            current_drive = drive_slot(packet.io_r.ior_unit);
        else if (request_type == SDP_PRINTING)
            current_printer = (int) packet.lpt_o_r.lpt_id;
        show_dp_status(request_type);
        result = (*packet_handler[ptype])(count);
    }
    else
        result = FALSE;
    if (!result) {
        connected = FALSE;
        show_port();
    }
    show_dp_status(SDP_INACTIVE);
}

void generate_drive_mappings(byte *map_table, int num_master_units, byte *desired_map)
{
    int master_unit;
    int i;
    byte slave_devices_used[MAX_DEVICES];

    memcpy(map_table, desired_map, MAX_DEVICES);
    memset(slave_devices_used, '\0', MAX_DEVICES);
    for (i = 0; i < MAX_DEVICES; i++) {
        // First check validity of all fixed assignments.
        if (map_table[i] < MAX_DEVICES) {
            if (map_table[i] >= (byte) slave_block_devices ||
                !memchr(drive_priority, map_table[i], num_drives) )
                map_table[i] = UNASSIGNED;
            else
                slave_devices_used[map_table[i]] = 1;
        }
    }
    master_unit = 0;
    for (i = 0; i < num_drives; i++) {
        while (master_unit < num_master_units &&
               map_table[master_unit] != DONT_CARE) {
            if ( !memchr(drive_priority, map_table[master_unit], num_drives) )
                map_table[master_unit] = UNASSIGNED;
            master_unit++;
        }
        if (master_unit >= num_master_units)
            break;
        while (i < num_drives && slave_devices_used[drive_priority[i]])
            i++;
        if (i >= num_drives)
            break;
        map_table[master_unit] = drive_priority[i];
    }
}

void make_printer_map(void)
{
    int i;

    for (i = 0; i < num_lpt; i++) {
        if ( (fx_port == -1) || FxPortInfo[fx_port].address != printers[i].address) {
            printers[i].enabled = TRUE;
        }
        else
            printers[i].enabled = FALSE;
    }
}

void show_drive_mappings(byte *map_table, int show_master)
{
    int i = 0;
    char buff[20];
#define STATUS_LEFT     41
    byte slave_mapping[MAX_DEVICES];
    int parallel_connection;
    byte di;
    byte *mptr;
    char size_buff[10];

    memset(slave_mapping, 0xFF, sizeof(slave_mapping));
    for (i = 0; i < MAX_DEVICES; i++) {
        if (map_table[i] < MAX_DEVICES)
            slave_mapping[map_table[i]] = (byte) i;
    }

    for (i = 0; i < num_drives; i++) {
        di = drive_priority[i];
        if (slave_mapping[di] != 0xFF)
            set_attribute(LIST_NORMAL);
        else
            set_attribute(LIST_DIM);
        sprintf(buff, "%c:", devices[di].drive + 'A');
        justify_str(drive_coord(i, FALSE), LOC_WIDTH, LIST_NORMAL, buff, -1);
        if (show_master) {
            if (config.master_first_unit + slave_mapping[di] < MAX_DEVICES) {
                sprintf(buff, "%c:",
                    config.master_first_unit + slave_mapping[di] + 'A');
            }
            else
                strcpy(buff, not_connected);
        }
        else {
            buff[0] = '\0';
        }
        justify_str(drive_coord(i, TRUE), REM_WIDTH, LIST_NORMAL, buff, -1);
        if (buff[0])
            justify_str(drive_coord(i, FALSE) + LOC_WIDTH, EQ_WIDTH, LIST_NORMAL,
                    equals_msg, 0);
        prepare_drive_size(di, size_buff);
        justify_str(drive_coord(i, FALSE) + 3, 7, LIST_NORMAL, size_buff, -1);
    }

    parallel_connection = -1;
    make_printer_map();
    for (i = 0; i < num_lpt; i++) {
        sprintf(buff, "LPT%d:", i + 1);
        justify_str(printer_coord(i, FALSE), LOC_WIDTH,
            printers[i].enabled ? LIST_NORMAL : LIST_DIM, buff, -1);
        if (printers[i].enabled) {
            mptr = memchr(actual_prn_map, i, 3);
            if (show_master) {
                if (mptr) {
                    sprintf(buff, "LPT%d:", mptr + 1 - actual_prn_map);
                    justify_str(printer_coord(i, TRUE),
                        REM_WIDTH, LIST_NORMAL, buff, -1);
                }
                else {
                    justify_str(printer_coord(i, TRUE),
                        REM_WIDTH, LIST_NORMAL, not_connected, -1);
                }
                justify_str(printer_coord(i, FALSE) + LOC_WIDTH, EQ_WIDTH,
                            LIST_NORMAL, equals_msg, 0);
            }
        }
        else 
            justify_str(printer_coord(i, TRUE), REM_WIDTH, LIST_NORMAL, "", -1);
    }
}

static void draw_status_box(byte left_col)
{
    byte width   = 40;

    draw_box(coord(top_row, left_col),  boxsize(display_rows + 5, width), SINGLE, LIST_NORMAL);
    justify_str(coord(top_row + 1, left_col + 1), width - 2, LIST_NORMAL, box_header1, 0);
    justify_str(coord(top_row + 2, left_col + 1), width - 2, LIST_NORMAL, box_header2, 0);
    dispchar(coord(top_row + 3, left_col), F_RT1);
    fill(coord(top_row + 3, left_col + 1), boxsize(1, width - 2), F_H1);
    dispchar(coord(top_row + 3, left_col + width - 1), F_LT1);
}

static void near paint_main_screen(void)
{
    char *speed_equal;
    char *speed_ptr;

    if (split_screen) {
        draw_status_box(0);
        draw_status_box(40);
    }
    else {
        draw_status_box(20);
    }
    justify_str(coord(scr_rows - 1, 0), 80, STATUS_COLOR, status_line, -1);
    port_column   = (byte)((strchr(status_line, '=') - status_line) + 1);
    status_column = (byte)((strchr(status_line, ':') - status_line) + 2);
    status_width  = (int)(strchr(status_line, 'ณ') - strchr(status_line, ':')) - 2;
    speed_equal   = strchr(status_line+port_column, '=');
    speed_column  = (byte)(speed_equal + 1 - status_line);
    speed_ptr     = speed_equal + 1;
    while (*(speed_ptr) != 'ณ')
        speed_ptr++;
    speed_size    = speed_ptr - (speed_equal + 1);
}

static void _near show_devices(void)
{
    byte dummy_mapping[MAX_DEVICES];
    byte dummy_desired[MAX_DEVICES];

    memset(dummy_desired, DONT_CARE, MAX_DEVICES);
    generate_drive_mappings(dummy_mapping, MAX_DEVICES, dummy_desired);
    show_drive_mappings(dummy_mapping, FALSE);
}

/*  setup_block_devices() sets the invalid flag for all block devices.  This
    forces media check to send back a media changed state the first time
    through--thus forcing the master systems DOS to ignore any cached data
*/
void setup_block_devices(void)
{
    register int i;

    for (i = 0; i < MAX_SLAVE_DEVICES; i++)
        devices[i].invalid = TRUE;
}

static void pascal reset_cntl_break(void)      // resetcbrk() with correct calling
{                                       // conventions
    resetcbrk();
}

static void get_slave_dos_version(void)
{
    slave_dos_version = DOS_VERSION(_osmajor, _osminor);

    if (slave_dos_version == DOS_VERSION(3, 30)) { // Check for Zenith DOS 3.30+
        union REGS regs;

        regs.x.ax = 0x3000;
        intdos(&regs, &regs);
        if (regs.h.bh == 5) slave_dos_version = DOS_VERSION(3, 31);
                                        // Pretend Zenith DOS 3.30+ is DOS 3.31
                                        // Zeniths OEM number is 5
    }
}

static void _cdecl do_restore_screen(void)
{
    static byte screen_restored = FALSE;

    if (screen_initialized && !screen_restored) {
        screen_restored = TRUE;
        restore_screen();
        if (final_msg)
            printf("\n%s", final_msg);
    }
}

void _cdecl do_init_screen(void)
{
    int len;
    char name_buff[70];

    if (!screen_initialized) {
        screen_initialized = TRUE;
        init_screen();
        cursor_off();
        fill_attr(coord(0, 0), boxsize(scr_rows, 80), ' ', SCROLL_COLOR);
        len = sprintf(name_buff, "%s %s", program_name, version);
        dispstr_attr(coord(0, (80 - len) / 2), name_buff, TITLE_COLOR);
        atexit(do_restore_screen);
    }
}

void add_serial(word address, byte bios_num)
{
    char port_name[20];
    int i;

    if (address < 0x200 || address >= 0x8000 || (address & 7)) {
        if (bios_num)
            sprintf(port_name, "(COM%d)", bios_num);
        else
            port_name[0] = '\0';
        warn_printf(invalid_serial_address, port_name, address);
        return;

    }
    for (i = 0; i < fx_num_ports; i++) {
        if (FxPortInfo[i].address == address)
            return;                         // This port already in table
    }
    if (fx_num_ports < MAX_PORTS) {
        FxPortInfo[fx_num_ports].type    = SERIAL_PORT;
        FxPortInfo[fx_num_ports].biosnum = bios_num;
        FxPortInfo[fx_num_ports].address = address;
        fx_num_ports++;
    }
}

void add_parallel(word address, byte bios_num)
{
    char port_name[20];
    int i;

    if (address < 0x200 || address >= 0x8000 || (address & 3)) {
        if (bios_num)
            sprintf(port_name, "(LPT%d)", bios_num);
        else
            port_name[0] = '\0';
        if (address > 3)            // Don't grouse about LAN redirector
            warn_printf(invalid_parallel_address, port_name, address);
        return;

    }
    for (i = 0; i < fx_num_ports; i++) {
        if (FxPortInfo[i].address == address)
            return;                         // This port already in table
    }
    if (fx_num_ports < MAX_PORTS) {
        FxPortInfo[fx_num_ports].type    = PARALLEL_PORT;
        FxPortInfo[fx_num_ports].biosnum = bios_num;
        FxPortInfo[fx_num_ports].address = address;
        fx_num_ports++;
    }
}

void add_bios_parallel(byte bios_num)
{
    word address = Bios.parallel_address[bios_num - 1];

    if (address) {
        add_parallel(address, bios_num);
    }
}

void add_bios_serial(byte bios_num)
{
    word address = Bios.serial_address[bios_num - 1];

    if (address) {
        if (address == 0x3F8)
            bios_num = 1;
        else if (address == 0x2F8)
            bios_num = 2;
/***
        else if (address == 0x3E8)
            bios_num = 3;
        else if (address == 0x2E8)
            bios_num = 4;
**/
        add_serial(address, bios_num);
    }
}

void add_all_bios_serial(void)
{
    byte port;

    for (port = 0; port < Bios.num_serial; port++) {
        add_bios_serial( (byte) (port + 1) );
    }
}

void add_all_bios_parallel(void)
{
    byte port;

    for (port = 0; port < Bios.num_parallel; port++) {
        add_bios_parallel( (byte) (port + 1) );
    }
}

void cdecl fatal_printf(char *fmt, ...)
{
    va_list arg_ptr;
    static char msg_buff[256];

    va_start(arg_ptr, fmt);
    if (screen_initialized) {
        vsprintf(msg_buff, fmt, arg_ptr);
        final_msg = msg_buff;
    }
    else
        vprintf(fmt, arg_ptr);
    exit(1);
}

static void remove_server_drive(byte drive_index)
{
    byte *ptr;

    if (included_drives[drive_index]) {
        ptr = memchr(drive_priority, drive_index, num_drives);
        if (!ptr)
            fatal_printf("Fatal error #9\n");
        memmove(ptr, ptr + 1, drive_priority + sizeof(drive_priority) - 1 - ptr);
        num_drives--;
    }
}

static char *set_exclude(char *argp)
{
    excluding = TRUE;
    return argp;
}

static char *set_com(char *argp)
{
    int port_address;

    if (*argp == ':') argp++;
    if (*argp == '*') argp++;
    port_address = htoi(argp);
    if (port_address >= 1 && port_address <= 4)
        add_bios_serial( (byte) port_address);
    else if (*argp <= ' ')
        add_all_bios_serial();
    else
        add_serial(port_address, 0);
    while ( isxdigit(*argp) ) argp++;
    return argp;
}

static char *set_lpt(char *argp)
{
    int port_address;

    if (*argp == ':') argp++;
    if (*argp == '*') argp++;
    port_address = htoi(argp);
    if (port_address >= 1 && port_address <= 4)
        add_bios_parallel( (byte) port_address);
    else if (*argp <= ' ')
        add_all_bios_parallel();
    else
        add_parallel(port_address, 0);
    while ( isxdigit(*argp) ) argp++;
    return argp;
}

static char *set_baud(char *argp)
{
    long baud_value;

    if (*argp == ':') argp++;
    baud_value = atol(argp);
    if (baud_value == 115200L)
        FxSettings.max_serial_baud = BAUD_115200;
    else if (baud_value <= 57600L) {
        switch ( (unsigned) baud_value ) {
        case 57600:
            FxSettings.max_serial_baud = BAUD_57600;
            break;
        case 38400:
            FxSettings.max_serial_baud = BAUD_38400;
            break;
        case 19200:
            FxSettings.max_serial_baud = BAUD_19200;
            break;
        case 9600:
            FxSettings.max_serial_baud = BAUD_9600;
            break;
        default:
            fatal_printf(invalid_baud, baud_value);
            break;
        }
    }
    else
        fatal_printf(invalid_baud, baud_value);
    while ( isdigit(*argp) ) argp++;
    return argp;
}

static char *set_maxblock(char *argp)
{
    long maxblock_value;

    if (*argp == ':') argp++;
    maxblock_value = atol(argp);
    if (maxblock_value < 1L || maxblock_value > 0x8000L)
        fatal_printf(invalid_maxblock, maxblock_value);
    FxSettings.max_serial_block = (word) maxblock_value;
    fx_max_serial_block = (word) maxblock_value;
    while ( isdigit(*argp) ) argp++;
    return argp;
}

static char *set_mono(char *argp)
{
    force_mono = TRUE;
    return argp;
}

static char *set_variable(char *argp)
{
    fx_force_variable = TRUE;
    return argp;
}

void process_args(char *argp)
{
    byte drive_index;
    static struct {
        char *param;
        char * (*func)(char *);
    } arg_table[] = {
        { "X",          set_exclude },
        { "COM",        set_com     },
        { "LPT",        set_lpt     },
        { "BAUD",       set_baud    },
        { "RCOPY",      clone       },
        { "MAXBLOCK",   set_maxblock},
        { "B",          set_mono    },
        { "V",          set_variable},
        { 0,            0           }
    };
    int i;
    int len;
    char buff[256];
    char value;

    if ( strchr(argp, '?') ) {
        printf("%s", usage_str);
        exit(1);
    }
    while (*argp) {
        while (*argp && *argp != '/') {
            value = (byte) toupper(*argp);
            if ( value >= 'A' && value <= 'Z') {
                drive_index = (byte) (value - 'A');
                if (excluding) {
                    remove_server_drive(drive_index);
                    excluded_drives[drive_index] = 1;
                    included_drives[drive_index] = 0;
                }
                else {
                    if (!included_drives[drive_index]) {
                        drive_priority[num_drives] = drive_index;
                        num_drives++;
                        num_includes++;
                    }
                    excluded_drives[drive_index] = 0;
                    included_drives[drive_index] = 1;
                }
            }
            else if (value > ' ' && value != ':' && (value != '=' || !excluding) )
                fatal_printf(invalid_server_drive, value);
            argp++;
        }
        if (*argp == '\0')
            break;
        argp++;
        excluding = FALSE;
        for (i = 0; arg_table[i].param; i++) {
            len = pstrlen(arg_table[i].param);
            if ( (memicmp(argp, arg_table[i].param, len) == 0) &&
                  !isalpha(*(argp + len)) ) {
                argp = arg_table[i].func(argp + len);
                break;
            }
        }
        if (!arg_table[i].param) {
            len = sprintf(buff, unrecognized_option);
            while ( isalpha(*argp) ) {
                len += sprintf(buff + len, "%c", *argp);
                argp++;
            }
            strcpy(buff + len, "\n");
            fatal_printf("%s", buff);
        }
    }
}

void cdecl quit(void)
{
    /***
    * IMPORTANT NOTE:  This routine can be called from within the critical
    * error handler if the user requests an abort.  Therefore any routines
    * called here should limit themselves to the appropriate DOS calls
    * and not use very much stack space.
    ***/
    unhook_int2f();
    reset_cntl_break();
    FxExit();
}

int default_drive_ok(int drv)
{

    if ( is_il_drive(drv) )
        return FALSE;
    if ( slave_dos_version >= DOS_VERSION(3, 20) ) {
        union REGS regs;

        // Check for logical (phantom) drive assignments.
        regs.x.ax = 0x440E;                     // Get logical drive map
        regs.h.bl = (byte) drv + (byte) 1;      // Check this drive
        intdos(&regs, &regs);
        if (!regs.x.cflag) {
            if (regs.h.al != 0) {               // It has aliases
                // Show only the last driver letter used on the aliased drive
                return (byte) (drv + 1) == regs.h.al ? TRUE : FALSE;
            }
        }
    }
    else {
        if ( drv == 0 && num_server_floppies < 1 ||         // missing floppy
            drv == 1 && num_server_floppies < 2 ) {
            switch (devices[drv].media_descriptor) {
            case 0xF8:                      // Fixed Disk
            case 0xF0:                      // 3.5"  double sided, 18 sector
            case 0xF9:                      // 5.25" double sided, 15 sector
                                            // 3.5"  double sided   9 sector
            case 0xFC:                      // 5.25" single sided,  9 sector
            case 0xFD:                      // 5.25" double sided,  9 sector
            case 0xFE:                      // 5.25" single sided,  8 sector
            case 0xFF:                      // 5.25" double sided,  8 sector
                                            // RAM disk or floppy disk
                break;
            default:
                return FALSE;               // Missing floppy
            }
        }
    }
    return TRUE;
}

void _cdecl reset_disk(void)
{
    union REGS regs;

    regs.h.ah = 0x0d;                   // reset disk
    intdos(&regs, &regs);               // Write all DOS internal buffers.
}

static int _far _fastcall check_abort(word elapsed_ticks, byte status)
{
    int key;

    while ( check_key() ) {
        key = read_key();
        if (key == ALT_F4) {
            exit(0);
        }
    }
    return 0;
}

int _far _fastcall CheckAbort(word elapsed_ticks, byte status)
{
    int key;

    while (check_key()) {
        key = read_key();
        if (key == F3 || key == ALT_F4) {
            check_abort_flag = TRUE;
            return -1;
        }
    }
    return 0;
}

void cdecl main(int argc, char *argv[])
{
    int main_option = 1;
    int i;
    char *ptr;
    char arg_buff[256];
    int num_good_ports;
    extern byte scroll_messages;

    enter_critical();                   // Enter Windows 386 Enh Mode critical section
    atexit(leave_critical);
    make_server_id();
    FxQueryBios(&Bios);
    num_lpt  = Bios.num_parallel;
    for (i = 0; i < num_lpt; i++) {
        printers[i].address = Bios.parallel_address[i];
        printers[i].exported = TRUE;
        printers[i].enabled  = TRUE;
    }
    config.master_code = 0;             // Always force this to zero on startup
    if (ptr = getenv("INTERSVR"))
        strtcpy(arg_buff, ptr, sizeof(arg_buff));
    else
        arg_buff[0] = '\0';
    for (i = 1; i < argc; i++) {
        strcat(arg_buff, " ");
        strcat(arg_buff, argv[i]);
    }
                // will not return if invoked with ?

    check_drdos();
    get_slave_dos_version();            // Get dos version in WORD format

    FxSetCheckAbort(CheckAbort);
    process_args(arg_buff);
    if (fx_num_ports == 0) {
        add_all_bios_serial();
        add_all_bios_parallel();
        if (fx_num_ports == 0) {
            fatal_printf(no_ports_available_error);
        }
    }
    load_device_information();          // Get all the information about
                                        // the devices on this system
    (void) flushall();                  // Flush all I/O streams
    reset_disk();
    setup_block_devices();
    for (i = 0; i < num_drives; i++) {
        if (drive_priority[i] >= (byte) slave_block_devices)
            remove_server_drive(drive_priority[i]);
    }
    for (i = 0; i < 26; i++) {
        if ( i >= slave_block_devices &&
             (included_drives[i] || excluded_drives[i]) ) {
                    fatal_printf(invalid_server_drive, i + 'A');
        }
    }
    if (num_includes == 0) {    // No included drives, so use default & exclude
        for (i = 0; i < slave_block_devices; i++) {
            if ( !excluded_drives[i] && default_drive_ok(i) ) {
                drive_priority[num_drives] = (byte) i;
                num_drives++;
            }
        }
    }
    set_critical_error();               // Send critical errors to criterr()
    setcbrk();                          // Ignore Cntl-C/Cntl-Break
    FxInit();
    do_init_screen();                   //##################################
    hook_int2f();
    atexit(quit);
    atexit(reset_disk);
    num_good_ports = 0;
    for (i = 0; i < fx_num_ports; i++) {
        if (FxPortInfo[i].type == SERIAL_PORT && (FxPortInfo[i].flags & PF_BAD_PORT)) {
            warn_printf(port_init_error, port_name(i), FxPortInfo[i].address);
        }
        else
            num_good_ports++;
    }
    if (num_good_ports == 0)
        fatal_printf(no_ports_available_error);
    do_init_screen();
    if ( num_drives + num_lpt > scr_rows - 7) {
        display_left_col  = LEFT_MARGIN;
        display_rows = (byte) ((num_drives + num_lpt + 1) / 2);
        split_screen = TRUE;
    }
    else {
        display_rows = (byte)num_drives + (byte) num_lpt;
    }
    top_row = (scr_rows - (display_rows + 6)) / 2 + 1;
    if (win386_enh_mode || dos5_switcher || desqview)
        warn_printf(desqview ? desqview_msg : multitasking_msg);
    paint_main_screen();
    show_devices();
    FxSyncTimeout(2 * SYNC_ONE_SECOND);
    FxSetCheckAbort(check_abort);
    if (desqview)
        stop_desqview_multitasking();
    for (;;) {
        check_abort(0, 0);
        attempt_transaction();
    }
    exit(0);
}
