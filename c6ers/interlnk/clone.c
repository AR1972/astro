
#include <bios.h>
#include <dos.h>
#include <string.h>
#include <direct.h>
#include <stdio.h>
#include <io.h>
#include <stdlib.h>
#include <fcntl.h>
#include <memory.h>
#include <string.h>
#include <conio.h>
#include <stdarg.h>
#include <errno.h>
#include "fastlynx.h"
#include "server.h"
#include "dc.h"

#define CTRL_C	    0x003
#define ESC	        0x01B

int _fastcall percent(long current, long total);
void _fastcall getargs(char near *cmdline);
void _fastcall getprog(char near *progname);
void near pascal set_critical_error(void);
void _fastcall long_to_ascii(long value, char near *buff);
unsigned int _fastcall pstrlen(char near *str);
int _fastcall isdigit(byte value);
unsigned pascal paragon_bios_keybrd(unsigned service);
void add_all_bios_serial(void);
void add_all_bios_parallel(void);

#include "str.h"

#pragma intrinsic(memset, _enable, _disable, memcmp)

extern byte check_abort_flag;

static char dl_name[]  = "interlnk.exe";

static int top_row;
static int left_col;
static int box_height;
static int box_width;
static int remote_port;
static int screen_height;
static int screen_width;
static int status_col;
static int percent_col;
static int scan_port_col;
static int left_margin;

char *server_name = "intersvr.exe";

extern struct FxBiosInfo Bios;
extern char *final_msg;

char double_percent_s[] = "%s%s";

static byte exit_called = FALSE;
static byte remote_found = FALSE;       // Set TRUE when remote computer found

void draw_screen(char *string, int extra_lines, int color)
{
    char *ptr = string;
    int width;
    char *lines[20];
    int  widths[20];
    int  i;

    fill_attr(coord(1, 0), boxsize(scr_rows - 2, 80), ' ', TITLE_COLOR);
    screen_height = 0;
    screen_width = 0;
    while (*ptr) {
        lines[screen_height] = ptr;
        width = 0;
        while (*ptr && *ptr != '\n') {
            width++;
            ptr++;
        }
        if (width > screen_width)
            screen_width = width;
        widths[screen_height] = width;
        screen_height++;
        if (*ptr == '\n') ptr++;
    }
    box_height = (screen_height + 4 + extra_lines);
    box_width  = screen_width + 6;
    top_row = (scr_rows - 2 - box_height) / 2 + 1;
    left_col = (80 - (screen_width + 6)) / 2;
    draw_box(coord(top_row, left_col),  boxsize(box_height, box_width),
            SINGLE, color);
    for (i = 0; i < screen_height; i++) {
        if (widths[i])
            dispmem_attr(coord(top_row + 2 + i, left_col + 3),
                lines[i], widths[i], color);
    }
}

static void call_clone_exit(void)
{
    static byte clone_exit_cmd = CLONE_EXIT_CMD;

    if (remote_found && !exit_called) {
        exit_called = TRUE;
        if (fx_baud == BAUD_38400)
            FxSendSerialBlock(&clone_exit_cmd, sizeof(clone_exit_cmd));
        FxCloneExit();
    }
}

static void cdecl error_printf(char *fmt, ...)
{
    va_list arg_ptr;
    char msg_buff[512];

    va_start(arg_ptr, fmt);
    vsprintf(msg_buff, fmt, arg_ptr);
    strcat(msg_buff, press_enter_to_return);
    call_clone_exit();
    draw_screen(msg_buff, 0, ERROR_COLOR);
    for (;;) {
        if ( read_key() == '\r' )
            break;
    }
}

void near _fastcall update_progress(int percentage)
{
    char buff[5];

    sprintf(buff, "%3d%%", percentage);
    dispstr_attr(coord(scr_rows - 1, percent_col), buff, STATUS_COLOR);
}

static int near pascal get_upload_options()
{
    int key;
    word start_ticks = bios_ticks();
    int option_row;
    int last_remote_port;
    char screen2[1024];

    justify_str(coord(scr_rows - 1, 0), 80, STATUS_COLOR, install_status_line1, -1);
    draw_screen(install_signon, 5, LIST_NORMAL);
    option_row = top_row + 4 + screen_height;
    draw_box(coord(option_row - 1, 35), boxsize(4, 10), SINGLE, LIST_NORMAL);
    last_remote_port = -1;
    remote_port = 0;
    do
    {
        if (remote_port != last_remote_port) {
            last_remote_port = remote_port;
            justify_str(coord(option_row, 36), 8,
                        remote_port == 0 ? LIST_REVERSE : LIST_NORMAL,
                        "COM1", 0);
            justify_str(coord(option_row + 1, 36), 8,
                        remote_port == 0 ? LIST_NORMAL : LIST_REVERSE,
                        "COM2", 0);
        }
        if ( check_key() ) {
            key = read_key();
            switch (key) {
            case CTRL_C:
            case ALT_F4:
                key = F3;
                break;
            case '1':
                remote_port = 0;
                break;
            case '2':
                remote_port = 1;
                break;
            case UP:
                if (remote_port)
                    remote_port = 0;
                break;
            case DOWN:
                if (remote_port == 0)
                    remote_port++;
                break;
            }
        }
    } while (key != '1' && key != '2' && key != F3 && key != '\r');
    if (key == F3) return FALSE;

    strtcpy(screen2, upload_remote_prompt, sizeof(screen2));
    strrepc(screen2, '#', remote_port + '1');
    draw_screen(screen2, 0, LIST_NORMAL);
    justify_str(coord(scr_rows - 1, 0), 80, STATUS_COLOR, install_status_line2, -1);
    status_col = strchr(install_status_line2, '³') - install_status_line2;
    while (install_status_line2[status_col + 1] == ' ')
        status_col++;
    scan_port_col = strchr(install_status_line2, '#') - install_status_line2;
    return TRUE;
}

static int _near _fastcall send_file(byte *file_name)
{
    struct find_t find_buff;
    int fd;
    FILESIZE bytes_sent;
    struct clone_cmd clone;
    char buff[80];

    if ( _dos_findfirst(file_name, _A_NORMAL, &find_buff) != 0 ) {  /* File or dir */
        error_printf("%s: not found.\n", file_name);
        return TRUE;
    }
    clone.command = CLONE_CREATE_CMD;
    clone.dir     = *((struct dir_entry *)&find_buff.attrib);
    sprintf(buff, "%s %s (%ld) ", sending_prefix, file_name, clone.dir.size);
    justify_str(coord(scr_rows - 1, status_col), 80 - status_col,
                STATUS_COLOR, buff, -1);
    percent_col = status_col + pstrlen(buff);
    if ( _dos_open(file_name, O_RDONLY, &fd) != 0 )
        fd = -1;
    if (fd < 0) {
        error_printf(open_error_msg, file_name);
        goto err_return;
    }
    sprintf(clone.ok_msg, "%s %s (%ld)   0%%",
            receiving_prefix, clone.dir.name, clone.dir.size);
    sprintf(clone.error_msg, clone_create_error, clone.dir.name);
    if ( !FxSendSerialBlock(&clone, (byte *)&clone.cnt - (byte *)&clone) )
        goto err_return;
    bytes_sent = 0;
    update_progress(0);
    while (fd >= 0) {
        if ( _dos_read(fd, clone.data, MAX_CLONE_DATA, &clone.cnt) != 0 ) {
            error_printf(read_error_msg, file_name);
            goto err_return;
        }
        bytes_sent += clone.cnt;
        update_progress( percent(bytes_sent, clone.dir.size) );
        sprintf(clone.error_msg, clone_write_error, clone.dir.name);
        if (clone.cnt) {
            clone.command = CLONE_WRITE_CMD;
            sprintf(clone.ok_msg, "\b\b\b\b%3d%%",
                percent(bytes_sent, clone.dir.size));
            if ( !FxSendSerialBlock(&clone, (clone.data - (byte *)&clone) + clone.cnt) )
                goto err_return;
        }
        if (clone.cnt != MAX_CLONE_DATA) {          /* Assume end of file */
            if (bytes_sent != clone.dir.size) {
                error_printf(allocation_error, file_name);
                goto err_return;
            }
            _dos_close(fd);
            fd = -1;
            clone.command = CLONE_CLOSE_CMD;
            strcpy(clone.ok_msg, "\n");
            // Leave the write error message in error_msg
            if ( !FxSendSerialBlock(&clone, ((byte *)&clone.cnt - (byte *)&clone)) )
                goto err_return;
        }
    }
    return TRUE;

err_return:
    if (fd != -1) {
        _dos_close(fd);
    }
    if (!exit_called) {
        if (check_abort_flag)
            error_printf(upload_cancelled);
        else
            error_printf(upload_transfer_error);
    }
    return FALSE;
}

static int _far _cdecl CloneCheckAbort(void)
{
    int key;

    while (check_key()) {
        key = read_key();
        if (key == F3 || key == ALT_F4 || key == CTRL_C) {
            check_abort_flag = TRUE;
            return -1;
        }
    }
    return 0;
}

static void do_clone(void)
{
#define MAX_ATTEMPTS    3
    char interlink_path[MAX_DOS_PATH];
    char path_prefix[MAX_DOS_PATH];
    char *path_tail;
    int port_index = 0;
    int request;
    int result;
    int bootstrap_size;
    int bootstrap_sent;
    int no_send_error;
    char copying_msg[512];

    path_tail = strrchr(server_name, '\\');
    if (path_tail) {
        memcpy(path_prefix, server_name, path_tail + 1 - server_name);
        path_prefix[path_tail + 1 - server_name] = '\0';
    }
    else
        path_prefix[0] = '\0';
    sprintf(interlink_path, double_percent_s, path_prefix, dl_name);
    if ( !get_upload_options() ) {
        return;
    }
    port_index = 0;
    for (;;) {
        if (port_index >= (int) Bios.num_serial) port_index = 0;
        set_attribute(STATUS_COLOR);
        dispchar(coord(scr_rows - 1, scan_port_col), port_index + '1');
        result = FxCloneInit(port_index, remote_port, loading_bootstrap_msg, CloneCheckAbort);
        if (result == 0) {
            break;
        }
        if (result == -1 || result > 3) {
            goto report_error;
        }
        port_index++;
    }
    remote_found = TRUE;
    left_margin = left_col + 3;
    strlwr(server_name);
    strlwr(interlink_path);
    sprintf(copying_msg, upload_copying_msg, server_name, interlink_path);
    draw_screen(copying_msg, 0, LIST_NORMAL);
    justify_str(coord(scr_rows - 1, status_col), 80 - status_col,
                STATUS_COLOR, sending_bootstrap, -1);
    percent_col = status_col + pstrlen(sending_bootstrap) + 1;
    update_progress(0);
    bootstrap_size = FxBootstrapInit();
    bootstrap_sent = 0;
    while (bootstrap_sent < bootstrap_size) {
        request = min(128, bootstrap_size - bootstrap_sent);
        result = FxSendBootstrap(request);
        if (result != 0)
            goto report_error;
        bootstrap_sent += request;
        update_progress( percent( (long)bootstrap_sent, (long)bootstrap_size) );
    }
    result = FxBootstrapVerify();
    if (result != 0) {
        goto report_error;
    }
    result = FxStartBootstrap();
    if (result != 0) {
        error_printf(connection_lost, result);
    }
    else {
        no_send_error = send_file(server_name);
        if (no_send_error) {
            no_send_error = send_file(interlink_path);
        }
    }
    call_clone_exit();
    return;

report_error:
    if (check_abort_flag)
        error_printf(upload_cancelled);
    else if (result == 88)
        error_printf(upload_share_problem);
    else if (result != -1)
        error_printf(upload_unable_to_communicate, result);
}

char *clone(char *argp)
{
    char prog_name[80];

    if (fx_num_ports == 0) {
        add_all_bios_serial();
        if (fx_num_ports == 0) {
            fatal_printf(no_serial_ports_available_error);
        }
    }
    do_init_screen();
    set_critical_error();               // Send critical errors to criterr()
    setcbrk();                          // Ignore Cntl-C/Cntl-Break
    FxInit();
    check_abort_flag = FALSE;
    do_init_screen();
    hook_int2f();
    atexit(quit);

    if (_osmajor >= 3)
    {
        getprog(prog_name);
        server_name = prog_name;
    }
    fill_attr(coord(0, 0), boxsize(scr_rows, 80), ' ', SCROLL_COLOR);
    dispstr_attr(coord(0, (80 - strlen(clone_title)) / 2),
                clone_title, TITLE_COLOR);
    do_clone();
    exit(0);
    return 0;                           // Doesn't really get executed
}
