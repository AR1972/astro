
KeyData STRUC
    char_code	DB  ?
    scan_code	DB  ?
    status	DB  ?
    nls_shift	DB  ?
    shift_state DW  ?
    time	DB  4 DUP(?)
KeyData ENDS

ModeData STRUC
    MDlength	DW  12
    VAtype	DB  ?
    color	DB  ?
    col 	DW  ?
    row 	DW  ?
    hres	DW  ?
    vres	DW  ?
ModeData ENDS

ConfigData STRUC
    CDlength	    DW	10
    adapter_type    DW	?
    display_type    DW	?
    memory_size     DD	?
ConfigData ENDS

PhysBufData STRUC
    buf_start	    DD	?
    buf_length	    DD	?
    selectors	    DW	?, ?
PhysBufData ENDS

CursorData STRUC
    cur_start	    DW	?
    cur_end	    DW	?
    cur_width	    DW	?
    cur_attribute   DW	?
CursorData ENDS

EXTRN  KBDPEEK:FAR
EXTRN  KBDCHARIN:FAR
EXTRN  KBDOPEN:FAR
EXTRN  KBDCLOSE:FAR
EXTRN  KBDFREEFOCUS:FAR
EXTRN  KBDGETFOCUS:FAR
EXTRN  VIOGETMODE:FAR
EXTRN  VIOGETCONFIG:FAR
EXTRN  VIOSETMODE:FAR
EXTRN  VIOWRTCHARSTRATT:FAR
EXTRN  VIOWRTNCELL:FAR
EXTRN  VIOSETCURPOS:FAR
EXTRN  VIOSCROLLUP:FAR
EXTRN  VIOSCROLLDN:FAR
EXTRN  VIOGETBUF:FAR
EXTRN  VIOSETCURTYPE:FAR
EXTRN  VIOSHOWBUF:FAR
EXTRN  VIOWRTCELLSTR:FAR
EXTRN  VIOGETPHYSBUF:FAR
EXTRN  VIOSCRLOCK:FAR
EXTRN  VIOSCRUNLOCK:FAR
EXTRN  VIOGETCURTYPE:FAR
EXTRN  VIOGETCURPOS:FAR
EXTRN  VIOREADCELLSTR:FAR
