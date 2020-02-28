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

_DATA segment
.MODEL small

OverlapWarn      db 'OverlapWarn'
Incorrect_DOS    db 'Incorrect_DOS'
InsfMem          db 'InsfMem'
Already_Inst     db 'Already_Inst'
No_PF_Avail      db 'No_PF_Avail'
Adj_Size         db 'Adj_Size'
InvMRA           db 'InvMRA'
InvParm          db 'InvParm'
InvPFBA          db 'InvPFBA'
PFWarning        db 'PFWarning'
NoWeitek         db 'NoWeitek'
NoXMM            db 'NoXMM'
BadXMM           db 'BadXMM'
OF_won_err       db 'OF_won_err'
PF_not_3_2       db 'PF_not_3_2'
OtherEMM         db 'OtherEMM'
HMAonMsg         db 'HMAonMsg'
UMBmemMsg        db 'UMBmemMsg'
UserAbortMsg     db 'UserAbortMsg'
Any_key          db 'Any_Key'
ActiveMess       db 'ActiveMess'
AutoMess         db 'AutoMess'
AvailHanMess     db 'AvailHanMess'
AvailPagesMess   db 'AvailPagesMess'
bad_mc_state     db 'bad_mc_state'
DMAMODE_Len      db 'DMAMODE_Len'
DMAMODE_Mess     db 'DMAMODE_Mess'
DMAMODE_Num      db 'DMAMODE_Num'
DMASIZE_Len      db 'DMASIZE_Len'
DMASIZE_Mess     db 'DMASIZE_Mess'
DMASIZE_Num      db 'DMASIZE_Num'
DriverVersion    db 'DriverVersion'
EXCPE_Len        db 'EXCPE_Len'
EXCPE_Mess       db 'EXCPE_Mess'
EXCPE_Num        db 'EXCPE_Num'
InactiveMess     db 'InactiveMess'
Incorrect_PRT    db 'Incorrect_PRT'
InstallMess      db 'InstallMess'
LIMVerMess       db 'LIMVerMess'
MajVer           db 'MajVer'
MemSizeMess      db 'MemSizeMess'
MinVer1          db 'MinVer1'
MinVer2          db 'MinVer2'
NoEMSmess        db 'NoEMSmess'
OF_aerr          db 'OF_aerr'
OF_amode         db 'OF_amode'
OFFMess          db 'OFFMess'
OF_inaccess      db 'OF_inaccess'
OF_invparm       db 'OF_invparm'
OF_not_there     db 'OF_not_there'
OF_parmerr       db 'OF_parmerr'
OF_proterr       db 'OF_proterr'
OF_rerr          db 'OF_rerr'
OF_rmode         db 'OF_rmode'
OF_verror        db 'OF_verror'
OF_vmode         db 'OF_vmode'
OF_w_inaccess    db 'OF_w_inaccess'
OF_w_not_inst    db 'OF_w_not_inst'
OF_woff_err      db 'OF_woff_err'
OF_woff_mode     db 'OF_woff_mode'
OF_won_mode      db 'OF_won_mode'
PEN_Len          db 'PEN_Len'
PEN_Mess         db 'PEN_Mess'
PEN_Num          db 'PEN_Num'
PFBAMess         db 'PFBAMess'
POE_Len          db 'POE_Len'
POE_Mess         db 'POE_Mess'
POE_Num          db 'POE_Num'
StatusMess       db 'StatusMess'
TotalHanMess     db 'TotalHanMess'
TotalPagesMess   db 'TotalPagesMess'
UMBavail         db 'UMBavail'
UMBlargest       db 'UMBlargest'
UMBstart         db 'UMBstart'
UMBstatusMess    db 'UMBstatusMess'
WeitekNAMess     db 'WeitekNAMess'
WeitekOFFMess    db 'WeitekOFFMess'
WeitekONMess     db 'WeitekONMess'
WinBackfillMess  db 'WinBackfillMess'
WINEMM_Mess      db 'WINEMM_Mess'
WinInvPathMess   db 'WinInvPathMess'
Inv_DOS_msg      db 'Inv_DOS_msg'
InitMessage      db 'InitMessage'
ISizeMess        db 'ISizeMess'

_DATA ends
end