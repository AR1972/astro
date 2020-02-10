/***
* $Workfile:   bioskybd.c  $
* $Revision:   1.1  $
*   $Author:   Dave Sewell  $
*     $Date:   24 Apr 1990  7:45:36  $
*
* Keyboard service routines which use BIOS calls.
*
* All keyboard return values are integers with the following meaning:
*     0       = no key available
*     1 - 255 = ASCII character
*   256 - 511 = function key, value = scan code + 256
***/

#include "osdep.h"
#include <bios.h>
#include <memory.h>
#include "umfunc.h"
#include "bioskybd.h"

#define MAX_INTERRUPT_KEYS  10
#define MAX_KEY_SAVE        4

int cdecl interrupt_key_level = 0;
int (* background_proc)(void) = 0;

static unsigned key_stack = 0;

static struct keydef {
    int value;
    int (*proc)(void);
} keys[MAX_INTERRUPT_KEYS];

static struct keydef key_save[MAX_INTERRUPT_KEYS * MAX_KEY_SAVE];
static int keyi = 0;        /* Index into key save structures.  */

static int near _fastcall map_key(unsigned service);

static int near _fastcall map_key(service)
unsigned service;
{
    unsigned value;

    value = paragon_bios_keybrd(service);
    if (value == 0) return 0;
    else if ( (byte) value ) return value & 0xFF;
    else return ((value >> 8) & 0xFF) + 256;
}

/***
* Check to see if a keystroke is available.
* Return 0 if no keystroke available.
* Otherwise return the keystroke and leave it in the buffer.
***/
int pascal check_key()
{
    if (key_stack) return key_stack;
    else return map_key(_KEYBRD_READY);
}

int pascal read_raw_key()       /* No check for interrupt functions.    */
{
    unsigned key;

    if (key_stack) {
        key = key_stack;
        key_stack = 0;
        return key;
    }
    if (background_proc) {
        while ( check_key() == 0 ) {   /* No keys yet. */
            if (background_proc) {
                key = (*background_proc)();
                if (key) return key;
            }
        }
    }
    return map_key(_KEYBRD_READ);
}

int pascal grab_key()       /* No check for interrupt functions.    */
{
    return map_key(_KEYBRD_READ);
}

int pascal read_key()
{
    register int c = 0;
    register int i;

    while (c == 0) {
        c = read_raw_key();
        for (i = 0; i < MAX_INTERRUPT_KEYS; i++) {
            if (keys[i].value == c && keys[i].proc) {
                interrupt_key_level++;
                c = (*keys[i].proc)();
                interrupt_key_level--;
                break;
            }
        }
    }
    return c;
}

/***
* Push a keystroke back onto the input buffer.
* Note:  only one keystroke can be pushed back.  Any previously pushed back
* keystrokes will be thrown away.
***/
void _fastcall unread_key(c)
int c;
{
    key_stack = c;
}

void pascal push_interrupt_keys()
{
    if (keyi < MAX_KEY_SAVE) {
        memcpy(&key_save[keyi * MAX_INTERRUPT_KEYS], keys, sizeof(keys));
        keyi++;
    }
}

void pascal pop_interrupt_keys()
{
    if (keyi) {
        keyi--;
        memcpy(keys, &key_save[keyi * MAX_INTERRUPT_KEYS], sizeof(keys));
    }
}

void _fastcall enable_interrupt_key(int key, int (*proc)(void) )
{
    register int i;

    for (i = 0; i < MAX_INTERRUPT_KEYS; i++) {
        if (keys[i].value == key || keys[i].value == 0) {
            keys[i].value = key;
            keys[i].proc = proc;
            break;
        }
    }
}

void _fastcall disable_interrupt_key(key)
int key;
{
    register int i;

    for (i = 0; i < MAX_INTERRUPT_KEYS; i++) {
        if (keys[i].value == key) {
            keys[i].proc = 0;
            break;
        }
    }
}

