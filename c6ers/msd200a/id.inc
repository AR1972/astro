;/* define IDs for machines and displays.

;------------------------------------------------------------------------------
;
;                 Machine Data values used by MachineTest
;
;------------------------------------------------------------------------------

; These equates match the #defines because these will be the values returned
; when the MachineTest function is called.

UNKNOWN_MACHINE                equ       0
IBMPC_XT_AT                    equ       1
IBMPS2_25_30                   equ       2
IBMPS2_50_60_70_80             equ       3
ATT_PC                         equ       4
HP_VECTRA                      equ       5
ZENITH_PC                      equ       6
IBMPS2_70P                     equ       7

;   STANDARD ROM BIOS MACHINE TYPES used in ROM_BIOS_Machine_ID

IBM_PCAT                       equ       0FCh
IBM_PCAT_SUB1                  equ       000h
IBM_PCAT_SUB2                  equ       001h
IBM_PS2_50_SUB                 equ       004h
IBM_PS2_60_SUB                 equ       005h
IBM_PS2_30                     equ       0FAh
IBM_PS2_80                     equ       0F8h
IBM_PS2_30_SUB                 equ       000h
IBM_PS2_80_SUB                 equ       000h
IBM_PS2_80_SUB2                equ       001h
IBM_PS2_25                     equ       0FAh
IBM_PS2_25_SUB                 equ       001h
IBM_PS2_70                     equ       0f8h
IBM_PS2_70_SUB                 equ       004h
IBM_PS2_70_SUB2                equ       009h
IBM_PS2_70_SUBP                equ       00bh

; These table entries determine what type of test will take place. ie test
; machine ID, do a string compare at an absolute location or end the test.

EndTest                        equ       0
MachineID                      equ       1
StringCompare                  equ       2

; Ok, here are the various display ID's needed and returned from the
; hardtest.asm detection module

; Display ID's. These are returned from the GetDisplayID() func

; Mouse ID's returned by GetMouseType().
;
; n.b., values 1 through 5 should not be changed or reassigned.  They match
; the return values from the mouse driver INT 33h's function 36.

UNKNOWN_MOUSE     equ         0     ; Don't know whether or not a mouse is
                                    ; installed...

BUS_MOUSE         equ         1     ; Type of mouse installed.
SERIAL_MOUSE      equ         2
INPORT_MOUSE      equ         3
IBMPS2_MOUSE      equ         4
HP_MOUSE          equ         5

NO_MOUSE          equ         6     ; No mouse installed.
LT_MOUSE          equ         7
LT_PS2MOUSE       equ         8
MSPS2_MOUSE       equ         9
MSBPOINT_MOUSE    equ        10     ; Ballpoint mouse
