                        PAGE    66, 132

INCLUDE     macros.inc

TRUE                    EQU     1
FALSE                   EQU     0

MAX_PORTS               EQU     8

MAX_BIOS_SERIAL         EQU     4
MAX_BIOS_PARALLEL       EQU     3

SERIAL_PORT             EQU     0
PARALLEL_PORT           EQU     1

PARALLEL_NORMAL         EQU     0
PARALLEL_TURBO          EQU     1

IFDEF _PAR18
PARALLEL_11_WIRE        EQU     0
PARALLEL_18_WIRE        EQU     1
PARALLEL_BIDIRECTIONAL  EQU     2
ENDIF

BAUD_1200               EQU     0
BAUD_2400               EQU     1
BAUD_4800               EQU     2
BAUD_9600               EQU     3
BAUD_19200              EQU     4
BAUD_38400              EQU     5
BAUD_57600              EQU     6
BAUD_115200             EQU     7

SEND_OPERATION          EQU     0
RECV_OPERATION          EQU     1

MINIMUM_TICKS           EQU     4

FX_ERR_TIMEOUT          EQU     -1
FX_ERR_CRC              EQU     -2
FX_ERR_FAIL             EQU     -3
FX_ERR_BAD_COUNT        EQU     -4
FX_ERR_BAD_SEQUENCE     EQU     -5

FxBiosInfo              STRUC
    num_serial          db  ?       ;NOTE: num_serial must immediately precede
    num_parallel        db  ?       ;num_parallel because a word store is used
    serial_address      dw  MAX_BIOS_SERIAL DUP(?)
    parallel_address    dw  MAX_BIOS_PARALLEL DUP(?)
FxBiosInfo              ENDS
                
FxPortInfoDef           STRUC
    pi_type             db  ?
    pi_biosnum          db  ?
    pi_address          dw  ?
    pi_flags            db  ?
FxPortInfoDef           ENDS

MAX_SERIAL_BLOCK        EQU     8 * 1024

;--- Definition of bits in the sequence number/flags byte

BYTE_LENGTH             EQU     10000000B
CONTINUED_PACKET        EQU     01000000B
COMPRESSED              EQU     00100000B
SEVEN_WIRE              EQU     00010000B
USE_CHECKSUM            EQU     00001000B
ECHOPLEX                EQU     00000100B
SEQUENCE_BITS           EQU     00000011B
IDLE_BITS               EQU     11111100B

FXS_FORCE_CRC           EQU     0
FXS_ALLOW_CHECKSUM      EQU     USE_CHECKSUM

FxSettingsDef               STRUC
    fxs_time_out            dw  2 * 18          ; Default timeout of 2 seconds
    fxs_error_checking_mode db  FXS_FORCE_CRC   ; Default to using CRC only
    fxs_max_serial_block    dw  MAX_SERIAL_BLOCK
    fxs_max_serial_baud     db  BAUD_115200
    fxs_allow_7_wire        dw  1               ; Default to enabled
FxSettingsDef               ENDS

CHECK_ABORT_WAITING     EQU     0
CHECK_ABORT_DONE        EQU     1

ONE_SECOND              EQU     18

SEWELL_OEM_ID  EQU  298 SHL 5
FX_ID          EQU    0         ; 0 - 7 are the only ones available under this
                                ; OEM ID.

VFXD_Device_ID EQU  SEWELL_OEM_ID OR FX_ID

                        .DATA

                EXTRN   C fx_port:WORD
                EXTRN   C fx_baud:BYTE
                EXTRN   C fx_parallel_speed:BYTE
		        EXTRN	C fx_num_ports:WORD
                EXTRN   C fx_serial_7_wire:BYTE
                EXTRN   C fx_send_variable:BYTE
                EXTRN   C fx_recv_variable:BYTE
                EXTRN   C fx_error_checking_mode:BYTE
                EXTRN   C FxPortInfo:BYTE
                EXTRN   C fx_errno:WORD
                EXTRN   C fx_max_serial_block:WORD
                EXTRN   C fx_max_serial_baud:BYTE
                EXTRN   C fx_error_checking_mode:BYTE
                EXTRN   C fx_force_variable:BYTE
        IFDEF   _PAR18
                EXTRN   C fx_parallel_mode:BYTE
                EXTRN   C fx_disable_par18:BYTE
        ENDIF

        IFNDEF  FX_MAIN
                EXTRN   fx_port_save_area:BYTE
                EXTRN   fx_connect_vector:WORD
                EXTRN   fx_listen_vector:WORD
                EXTRN   fx_tail:WORD
                EXTRN   fx_index:WORD
                EXTRN   seq_num:BYTE
                EXTRN   C FxSettings:BYTE
        ENDIF
