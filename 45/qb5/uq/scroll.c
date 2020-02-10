#define CC
#define cwExtraWnd	5

#include <cw\cwindows.h>

WND wndScrollH =
    wndScrollBar (1, FALSE, TRUE, 0, 23, 80, 1, NULL, NULL, 2);

WND wndScrollV =
    wndScrollBar (1, TRUE, TRUE, 80, 0, 1, 23, NULL, NULL, 2);
