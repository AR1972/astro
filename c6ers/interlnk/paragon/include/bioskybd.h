/***
* $Workfile:   bioskybd.h  $
* $Revision:   1.2  $
*   $Author:   Dave Sewell  $
*     $Date:   24 Apr 1990  7:24:24  $
***/

#define CTRL_C	    0x003
#define ESC	    0x01B
#define F1	    0x13B
#define F2	    0x13C
#define F3	    0x13D
#define F4	    0x13E
#define F5	    0x13F
#define F6	    0x140
#define F7	    0x141
#define F8	    0x142
#define F9	    0x143
#define F10	    0x144
#define LEFT	    0x14B
#define RIGHT	    0x14D
#define UP	    0x148
#define DOWN	    0x150
#define HOME	    0x147
#define END	    0x14F
#define PGUP	    0x149
#define PGDN	    0x151
#define C_LEFT	    0x173
#define C_RIGHT     0x174
#define C_HOME	    0x177
#define C_END	    0x175
#define C_PGUP	    0x184
#define C_PGDN	    0x176
#define INS	    0x152
#define DEL	    0x153
#define SHIFT_F5    0x158
#define SHIFT_F7    0x15A
#define ALT_C	    0x12E
#define ALT_D	    0x120
#define ALT_F	    0x121
#define ALT_H       0x123
#define ALT_L       0x126
#define ALT_M	    0x132
#define ALT_N       0x131
#define ALT_P       0x119
#define ALT_R	    0x113
#define ALT_S	    0x11F
#define ALT_T       0x114
#define ALT_U	    0x116
#define ALT_V       0x12F
#define ALT_W       0x111
#define ALT_X       0x12D

extern int (* background_proc)(void);
extern int cdecl interrupt_key_level;
