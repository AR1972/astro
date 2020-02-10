#include "fastlynx.h"
#include <string.h>

#define CTRL_C      3

#define ONE_SECOND  18

#define PROMPT_WAIT (5 * ONE_SECOND + 1)

#define MAX_SYNC_TRIES  3

static int (_far _cdecl *CloneCheckAbort)(void);
static byte _far *loader_ptr;
static byte copying_bootstrap;
static int port_index;

unsigned _near _pascal _fxc_bios_ticks(void);
int _near _pascal _fxc_read_serial(void);
void _near _pascal _fxc_drain(void);
void _near _pascal _fxc_wait_copy(void);
void _near _fastcall _fxc_output(byte value);
void _near _fastcall _fxc_init_port(int port_index);
void _near _pascal _fxc_reset_port(void);
void _near _fastcall _fxc_send_byte(byte value);
int _near _fastcall _fxc_recv_byte(unsigned tick_value);

static int near aborted(void)
{
    static int aborted_flag = FALSE;

    if (aborted_flag || (*CloneCheckAbort)() != 0) {
        aborted_flag = TRUE;
    }
    return aborted_flag;
}

static int near wait(byte *waitlist, unsigned tick_count)
{
    int c;
    byte *ptr;
    unsigned old_ticks, new_ticks;

    old_ticks = _fxc_bios_ticks();
    do {
        c = _fxc_read_serial();
        if (c >= 0) {
            ptr = waitlist;
            while (*ptr) {
                if (*ptr == (byte) c)
                    return (byte) c;
                ptr++;
            }
        }
        new_ticks = _fxc_bios_ticks();
    } while (new_ticks - old_ticks < tick_count);
    return 0;
}

static void _near delay(unsigned tick_count)
{
    wait("", tick_count);
}

static int _near outstr(char const _near *str)
{
    byte list[2];

    list[1] = '\0';
    while (*str) {
//        delay(1);  
        _fxc_output(*str);
        list[0] = *str;
        if ( !wait(list, ONE_SECOND) )      
            return FALSE;
        if (*str == '\r') {
            if ( !wait("\n", ONE_SECOND) )
                return FALSE; 
        }
        str++;
    }
    return TRUE;
}

int _far _fastcall FxCloneInit(int clone_port_index, int remote_port, char const _near *loading_bootstrap_msg, int (_far _cdecl *CheckAbort)(void))
{
    int error_code;
    int got;
    byte prompt_reset;
    static char copy_bootstrap_str[] = "COPY COM#: FXB.COM\r";
    int tried_aux = FALSE;
    extern unsigned _far loader_bios_port;

    CloneCheckAbort = CheckAbort;
    port_index = clone_port_index;
    loader_bios_port = remote_port;
    prompt_reset = FALSE;
    copying_bootstrap = FALSE;
    error_code = 0;
    _fxc_init_port(port_index);
    FxSyncTimeout(2 * SYNC_ONE_SECOND);
begin:
    _fxc_drain();
    _fxc_output(CTRL_C);
    if ( !(got = wait("C", 9)) ) error_code = 1;
    else if ( !(got = wait(">", PROMPT_WAIT)) ) {
        if (!prompt_reset) {
            prompt_reset = TRUE;
            if (outstr("prompt=\r")) goto begin;
            error_code = 8;
        }
        else error_code = 2;
    }
    if (!error_code) {
        delay(ONE_SECOND);
        if ( !outstr("echo ") || !outstr(loading_bootstrap_msg) ||
             !outstr(" > CON\r") ) error_code = 9;
        else {
            if ( !wait(">", PROMPT_WAIT) ) error_code = 10;
            else {
                memcpy(&copy_bootstrap_str[5], "COM", 3);
                copy_bootstrap_str[8] = (char) '1' + (char) remote_port;
start_copy:     delay(ONE_SECOND);
                if ( !outstr(copy_bootstrap_str) )
                    error_code = 4;
            }
        }
    }
    if (error_code) {
        _fxc_reset_port();
        return aborted() ? -1 : error_code;
    }
    if ( wait(">", 3 * ONE_SECOND) ) {  // If prompt came back, SHARE loaded
        if ( (remote_port == 0) && !tried_aux ) {
            tried_aux = TRUE;
            memcpy(&copy_bootstrap_str[5], " AUX", 4);
            goto start_copy;
        }
        else {
            FxCloneExit();
            return aborted() ? -1 : 88;
        }
    }
    return 0;
}

int _far _fastcall FxBootstrapInit(void)
{
    extern byte _far loader_start;
    extern byte _far loader_end;

    copying_bootstrap = TRUE;
    loader_ptr = &loader_start;
    return &loader_end - &loader_start;
}

static int _near terminate_copy(void)
{
    int i;
    int got;

    if (copying_bootstrap) {
        copying_bootstrap = FALSE;
        _fxc_output(0x1a);
        for (i = 0; i < 4; i++) {
            if ( (got = wait(">^", ONE_SECOND / 2)) ) {
                return got;
            }
            _fxc_output(0x1a);
        }
    }
    return 0;
}

int _far _fastcall FxSendBootstrap(int num_bytes)
{
    if ( aborted() ) {
        return -1;
    }
    while (num_bytes--) {
        while ( _fxc_read_serial() >= 0 )
            ;
        _fxc_output(*loader_ptr++);
    }
    return 0;
}

int _far _pascal FxBootstrapVerify(void)
{
    int error_code = 0;
    int got;

    if ( aborted() ) {
        return -1;
    }
    got = terminate_copy();
    if (got == '>')
        return 0;
    else if (got == '^')
        goto got_hat;
    /* Sometimes after copy there is a delay before getting prompt back.    */
    /* NOTE: if the disk is write protected we will get this error.
       Maybe we should check for that condition.
    */
    if ( !(got = wait(">^", 10 * ONE_SECOND)) )
        error_code = 5; 
    else if (got == '^') {
got_hat:
        if ( !outstr("\r") ) error_code = 6;
        else if ( !wait(">", 10 * ONE_SECOND) ) error_code = 7;
    }
    if (error_code)
        _fxc_reset_port();      // Reset current port.
    else
        delay(ONE_SECOND);
    return aborted() ? -1 : error_code;
}

int _far _pascal FxStartBootstrap(void)
{
    int error_code = 0;
    int i;
    static byte check_alive[2] = { CLONE_UNKNOWN_CMD, 0 };
    /***
    * NOTE: The two bytes above correspond to the clone_cmd structure.
    * The first byte is the command code, which is set to unknown so that
    * it will be ignored.  The next byte MUST be a null so that the
    * ok_msg field will be a null string.
    ***/

    _fxc_output(CTRL_C);
    if ( !wait(">", 3 * ONE_SECOND) )
        error_code = 21;
    else if ( !outstr("fxb\r") )
        error_code = 22;
    if (!error_code) {
        delay(ONE_SECOND / 2);
        _fxc_reset_port();
        delay(ONE_SECOND / 2);
        for (i = 0; i < MAX_SYNC_TRIES; i++) {
            if ( FxSendSerialBlock(&check_alive, sizeof(check_alive)) ) {
                // Lengthen timeout for sending files.
                FxSyncTimeout(10 * SYNC_ONE_SECOND);
                break;          // successful
            }
        }
        if (i == MAX_SYNC_TRIES) {           // Couldn't find bootstrap
            error_code = 99;
        }
    }
    return aborted() ? -1 : error_code;
}

int _far _pascal FxCloneExit(void)
{
#define MAX_ATTEMPTS    5
    int attempt;
    int error_code = 11;

#if 0
            if ( got_reply(space_str) )
                error_code = -1;                  // Disk full
            else
#endif
    terminate_copy();                           // Insure copying complete.
    _fxc_init_port(port_index);
    for (attempt = 0; attempt < MAX_ATTEMPTS; attempt++) {
        if ( !wait(">", ONE_SECOND) ) {
            _fxc_output(CTRL_C);
        }
        else {
            delay(3);
            if ( aborted() ) {
                outstr("del fxb.com\r");
                wait(">", ONE_SECOND);
            }
            if ( outstr("ctty con\r") ) {
                error_code = 0;
                break;
            }
        }
    }
    delay(3);
    _fxc_reset_port();
    return aborted() ? -1 : error_code;
}
