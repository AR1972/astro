PUBLIC INIT_IO_ERR
PUBLIC SECT_ADJ
PUBLIC HEADERMES
PUBLIC BADVERMES
PUBLIC ERRXMM
PUBLIC STATMES1
PUBLIC STATMES2
PUBLIC STATMES3
PUBLIC STATMES4
PUBLIC STATMES5
PUBLIC XMMCHAIN
PUBLIC NO_MEM
PUBLIC NO_ABOVE
PUBLIC ERRMSG1
PUBLIC NOXMM
PUBLIC ERRMSG2
PUBLIC PATCH2X
PUBLIC DOS_DRV
PUBLIC BAD_ABOVE

_msg segment public
NO_ABOVE      db "RAMDrive: Expanded Memory Manager not present",13,10,'$'
BAD_ABOVE     db "RAMDrive: Expanded Memory Status shows error",13,10,'$'
NO_MEM        db "RAMDrive: No extended memory available",13,10,'$'
NOXMM         db "RAMDrive: Extended Memory Manager not present",13,10,'$'
XMMCHAIN      db "RAMDrive: Bad Extended Memory Manager control chain",13,10,'$'
ERRXMM        db "RAMDrive: Error in extended memory allocation",13,10,'$'
ERRMSG1       db "RAMDrive: Invalid parameter",13,10,'$'
ERRMSG2       db "RAMDrive: Insufficient memory",13,10,'$'
INIT_IO_ERR   db "RAMDrive: I/O error accessing drive memory",13,10,'$'
BADVERMES     db "RAMDrive: Incorrect DOS version",13,10,'$'
SECT_ADJ      db "RAMDrive: Incompatible parameters: sector size adjusted",13,10,'$'
HEADERMES     db 13,10,"Microsoft RAMDrive version 3.07"
PATCH2X       db " virtual disk "
DOS_DRV       db "A:",13,10,'$'
STATMES1      db "    Disk size: $"
STATMES2      db "k",13,10,"    Sector size: $"
STATMES3      db " bytes",13,10,"    Allocation unit: $"
STATMES4      db " sectors",13,10,"    Directory entries: $"
STATMES5      db 13,10,'$'
_msg ends
end
