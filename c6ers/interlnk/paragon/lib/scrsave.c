#include "osdep.h"
#include "umfunc.h"
#include "screen.h"

static int screen_mode_switched;
static unsigned original_cursor_value;
static unsigned original_cursor_location;
static byte _far screen_buffer[2 * 80 * 50];

void _far _pascal init_screen(void)
{
    screen_mode_switched = init_scr();
    if (!screen_mode_switched)
        save_zone(coord(0, 0), boxsize(scr_rows, scr_cols), screen_buffer);
    original_cursor_value = cursor_value;
    original_cursor_location = cursor_location;
}

void _far _pascal restore_screen(void)
{
    restore_scr(screen_mode_switched);   // this is a far call.
    if (!screen_mode_switched) {
        restore_zone(coord(0, 0), boxsize(scr_rows, scr_cols), screen_buffer);
        locate(original_cursor_location);
        restore_cursor(original_cursor_value);
    }
}
