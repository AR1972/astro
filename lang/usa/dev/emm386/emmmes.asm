
PUBLIC ActiveMess
PUBLIC Adj_Size
PUBLIC Already_Inst
PUBLIC Any_key
PUBLIC AutoMess
PUBLIC AvailHanMess
PUBLIC AvailPagesMess
PUBLIC bad_mc_state
PUBLIC BadXMM
PUBLIC DMAMODE_Len
PUBLIC DMAMODE_Mess
PUBLIC DMAMODE_Num
PUBLIC DMASIZE_Len
PUBLIC DMASIZE_Mess
PUBLIC DMASIZE_Num
PUBLIC DriverVersion
PUBLIC EXCPE_Len
PUBLIC EXCPE_Mess
PUBLIC EXCPE_Num
PUBLIC HMAonMsg
PUBLIC InactiveMess
PUBLIC Incorrect_DOS
PUBLIC Incorrect_PRT
PUBLIC InsfMem
PUBLIC InstallMess
PUBLIC InvMRA
PUBLIC InvParm
PUBLIC InvPFBA
PUBLIC LIMVerMess
PUBLIC MajVer
PUBLIC MemSizeMess
PUBLIC MinVer1
PUBLIC MinVer2
PUBLIC NoEMSmess
PUBLIC No_PF_Avail
PUBLIC NoWeitek
PUBLIC NoXMM
PUBLIC OF_aerr
PUBLIC OF_amode
PUBLIC OFFMess
PUBLIC OF_inaccess
PUBLIC OF_invparm
PUBLIC OF_not_there
PUBLIC OF_parmerr
PUBLIC OF_proterr
PUBLIC OF_rerr
PUBLIC OF_rmode
PUBLIC OF_verror
PUBLIC OF_vmode
PUBLIC OF_w_inaccess
PUBLIC OF_w_not_inst
PUBLIC OF_woff_err
PUBLIC OF_woff_mode
PUBLIC OF_won_mode
PUBLIC OtherEMM
PUBLIC OverlapWarn
PUBLIC PEN_Len
PUBLIC PEN_Mess
PUBLIC PEN_Num
PUBLIC PFBAMess
PUBLIC PF_not_3_2
PUBLIC PFWarning
PUBLIC POE_Len
PUBLIC POE_Mess
PUBLIC POE_Num
PUBLIC StatusMess
PUBLIC TotalHanMess
PUBLIC TotalPagesMess
PUBLIC UMBavail
PUBLIC UMBlargest
PUBLIC UMBmemMsg
PUBLIC UMBstart
PUBLIC UMBstatusMess
PUBLIC UserAbortMsg
PUBLIC WeitekNAMess
PUBLIC WeitekOFFMess
PUBLIC WeitekONMess
PUBLIC WinBackfillMess
PUBLIC WINEMM_Mess
PUBLIC WinInvPathMess
PUBLIC Inv_DOS_msg 
PUBLIC InitMessage
PUBLIC ISizeMess
PUBLIC OF_won_err

_LAST segment public 'LAST'
OverlapWarn      db 'WARNING: User specified ranges overlap.',13,10,'$'
Incorrect_DOS    db 'EMM386 not installed - incorrect DOS version.',13,10,13,10,'$'
InsfMem          db 'EMM386 not installed - insufficient memory.',13,10,'$'
Already_Inst     db 'EMM386 already installed.',13,10,'$'
No_PF_Avail      db 'WARNING: Unable to set page frame base address--EMS unavailable.',13,10,'$'
Adj_Size         db 'Size of expanded memory pool adjusted.',13,10,'$'
InvMRA           db '[HMAon] is an invalid parameter on this machine.',13,10,'$'
InvParm          db 'Invalid parameter specified.',13,10,'$'
InvPFBA          db 'WARNING: Unable to set page frame base address--EMS unavailable.',13,10,'$'
PFWarning        db 'PFWarning$'
NoWeitek         db 'Weitek Coprocessor not installed.',13,10,'$'
NoXMM            db 'NoXMM$'
BadXMM           db 'BadXMM$'
PF_not_3_2       db 'WARNING: EMM386 installed without a LIM 3.2 compatible Page Frame',13,10,'$'
OtherEMM         db 'OtherEMM$'
HMAonMsg         db 'HMAonMsg$'
UMBmemMsg        db 'UMBmemMsg$'
UserAbortMsg     db 'EMM386 not installed - User requested abort.'13,10,'$'
Any_key          db 'Press any key when ready...',13,13,'$'
ActiveMess       db 'EMM386 Active.',13,10,'$'
AutoMess         db 'EMM386 is in Auto mode.',13,10,'$'
AvailHanMess     db 'AvailHanMess$'
AvailPagesMess   db 'AvailPagesMess$'
bad_mc_state     db 'bad_mc_state$'
InactiveMess     db '$EMM386 Inactive.',13,10,'$'
Incorrect_PRT    db 'Incorrect_PRT$'
InstallMess      db '$EMM386 successfully installed.',13,10,'$'
LIMVerMess       db '  LIM/EMS version . . . . . . . . . . . . .   0.0',13,10,'$'
MajVer           db 'MajVer$'
MemSizeMess      db '  Available expanded memory . . . . . . . .       KB',13,10,'$'
MinVer1          db 'MinVer1$'
MinVer2          db 'MinVer2$'
NoEMSmess        db 'Expanded memory services unavailable.',13,10,'$'
OFFMess          db 'EMM386 Inactive.',13,10,'$'
OF_aerr          db 'OF_aerr$'
OF_amode         db 'OF_amode$'
OF_inaccess      db 'OF_inaccess$'
OF_invparm       db 'OF_invparm$'
OF_not_there     db 'OF_not_there$'
OF_parmerr       db 'OF_parmerr$'
OF_proterr       db 'OF_proterr$'
OF_rerr          db 'OF_rerr$'
OF_rmode         db 'OF_rmode$'
OF_verror        db 'OF_verror$'
OF_vmode         db 'OF_vmode$'
OF_w_inaccess    db 'OF_w_inaccess$'
OF_w_not_inst    db 'OF_w_not_inst$'
OF_woff_err      db 'OF_woff_err$'
OF_woff_mode     db 'OF_woff_mode$'
OF_won_mode      db 'OF_won_mode$'
OF_won_err       db 'OF_won_err$'
PFBAMess         db 'Page Frame Base Address adjusted.',13,10,'$'
StatusMess       db 'StatusMess$'
TotalHanMess     db 'TotalHanMess$'
TotalPagesMess   db 'TotalPagesMess$'
UMBavail         db 'UMBavail$'
UMBlargest       db 'UMBlargest$'
UMBstart         db 'UMBstart$'
UMBstatusMess    db 'UMBstatusMess$'
WeitekNAMess     db 'Weitek Coprocessor is inaccessible until EMM386 is re-activated.',13,10,'$'
WeitekOFFMess    db 'Weitek Coprocessor support is disabled.',13,10,'$'
WeitekONMess     db 'Weitek Coprocessor support is enabled.',13,10,'$'
Inv_DOS_msg      db 'Inv_DOS_msg$'
InitMessage      db 'InitMessage$'
ISizeMess        db 'ISizeMess$'
_LAST ends

_R1_CODE segment public 'R1_CODE'
PEN_Mess         db 13,10,'EMM386 Privileged operation error #'
PEN_Num          db 'xx -',13,10,'Deactivate EMM386 and Continue (C) or reBoot (B) (C or B) ? $'
PEN_Len          equ 0 + ($ - PEN_Mess)
POE_Mess         db 13,10,'EMM386: Unrecoverable privileged operation error #'
POE_Num          db 'xx - press ENTER to reboot$'
POE_Len          equ 0 + ($ - POE_Mess)
EXCPE_Mess       db 13,10,'EMM386 Exception error #'
EXCPE_Num        db 'xx - press ENTER to reboot$'
EXCPE_Len        equ 0 + ($ - EXCPE_Mess)
DMASIZE_Mess     db 13,10,'EMM386 DMA buffer is too small.  Add D='
DMASIZE_Num      db 'nn  parameter and reboot.$'
DMASIZE_Len      equ 0 + ($ - DMASIZE_Mess)
DMAMODE_Mess     db 13,10,'EMM386: DMA mode not supported.  Press ENTER to reboot.$'
DMAMODE_Num      db 'xxx'
DMAMODE_Len      equ 0 + ($ - DMAMODE_Mess)
WinBackfillMess  db 13,10,'EMM386: Unable to start Enhanced Mode Windows due to base memory back fill.',13,10,'$'
WinInvPathMess   db 13,10,'EMM386: Unable to start Enhanced Mode Windows due to invalid path '
                 db 13,10,'        specification for EMM386.',13,10,'$'
WINEMM_Mess      db ' '
DriverVersion    equ 400h+45h
_R1_CODE ends

end